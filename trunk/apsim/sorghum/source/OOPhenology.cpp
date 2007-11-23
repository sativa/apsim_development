//------------------------------------------------------------------------------------------------
#pragma hdrstop

#include <vector>

#include "OOPlant.h"
#include "OOPlantComponents.h"
#include "OOPhenology.h"

using namespace std;

//------------------------------------------------------------------------------------------------

#pragma package(smart_init)

//------------------------------------------------------------------------------------------------
//------ Phenology constructor
//------------------------------------------------------------------------------------------------
Phenology::Phenology(ScienceAPI &api, OOPlant *p) : PlantProcess(api)
   {
   plant = p;

   scienceAPI.read("stage_names","", false, stageNames);
   stageNames.insert(stageNames.begin(),string("nocrop"));  // for compatibility with fortran
   initialize();
   doRegistrations();
   }
//------------------------------------------------------------------------------------------------
//------ Phenology destructor
//------------------------------------------------------------------------------------------------
Phenology::~Phenology()
   {
   }
//------------------------------------------------------------------------------------------------
//------- Initialize variables
//------------------------------------------------------------------------------------------------
void Phenology::initialize(void)
   {
   dltTT = 0.0;
   dltTTFM = 0.0;
   dltPhase = 0.0;
   dltStage = 0.0;

   ttTotal.clear();
   ttTotalFM.clear();
   daysTotal.clear();
   ttTarget.clear();

   for(int i=0;i < nStages;i++)
      {
      ttTotal.push_back(0.0);
      ttTotalFM.push_back(0.0);
      daysTotal.push_back(0.0);
      ttTarget.push_back(0.0);
      }
   setStage(0.0);

   floweringDAS = 0;
   floweringDOY = 0;
   maturityDAS = 0;
   maturityDOY = 0;

   }
//------------------------------------------------------------------------------------------------
//------ get the Phenology parameters
//------------------------------------------------------------------------------------------------
void Phenology::readParams (string cultivar)
   {
   float shootLag,shootRate;
   scienceAPI.read("shoot_lag", "", false, shootLag);
   scienceAPI.read("shoot_rate","", false, shootRate);
   ttTarget[germination] = shootLag + plant->getSowingDepth() * shootRate;

   scienceAPI.read("tt_emerg_to_endjuv", "", false, ttTarget[emergence]);
   scienceAPI.read("tt_endjuv_to_init", "", false, ttEndJuvInit);
   ttTarget[endJuv] = ttEndJuvInit;
   ttTarget[fi] = 50;
   scienceAPI.read("tt_flag_to_flower", "", false, ttTarget[flag] );
   scienceAPI.read("tt_flower_to_start_grain","", false, ttTarget[flowering]);
   scienceAPI.read("tt_flower_to_maturity", "", false, ttFlowerMaturity);
   ttTarget[endGrainFill] = 0.05 * ttFlowerMaturity;
   ttTarget[startGrainFill] = ttFlowerMaturity - ttTarget[flowering]
                                       - ttTarget[endGrainFill];
   scienceAPI.read("tt_maturity_to_ripe", "", false, ttTarget[maturity]);


   // photoperiod function
   float photoperiod_crit1, photoperiod_crit2;
   scienceAPI.read("photoperiod_crit1", "", 0, photoperiod_crit1);
   scienceAPI.read("photoperiod_crit2", "", 0, photoperiod_crit2);
   vector<float> xVec;
   xVec.push_back(photoperiod_crit1);
   xVec.push_back(photoperiod_crit2);

   float slope;
   scienceAPI.read("photoperiod_slope", "", 0, slope);

   vector<float> yVec;
   yVec.push_back(ttEndJuvInit);
   yVec.push_back(ttEndJuvInit + (xVec[1]-xVec[0]) * slope);
   photoParams.load(xVec,yVec);

   // thermal time
   ttParams.read(scienceAPI, "x_temp","y_tt");
   ttFmParams.read(scienceAPI, "x_temp_fm","y_tt_fm");

   // germination
   scienceAPI.read("pesw_germ", "", 0, peswGerm);

 
   scienceAPI.get("latitude", "", 0, latitude, -90.0f, 90.0f);
   scienceAPI.read("twilight","", 0, twilight);

   // report
   char msg[120];
   sprintf(msg, "    tt_emerg_to_endjuv       =  %6.2f\n",ttEndJuvInit);   scienceAPI.write(msg);
   sprintf(msg, "    tt_flower_to_maturity    =  %6.2f\n",ttFlowerMaturity);   scienceAPI.write(msg);
   sprintf(msg, "    tt_flag_to_flower        =  %6.2f\n",ttTarget[flag]);   scienceAPI.write(msg);
   sprintf(msg, "    tt_flower_to_start_grain =  %6.2f\n",ttTarget[flowering]);   scienceAPI.write(msg);
   sprintf(msg, "    tt_maturity_to_ripe      =  %6.2f\n",ttTarget[maturity]);   scienceAPI.write(msg);
   sprintf(msg, "    -------------------------------------------------------\n");   scienceAPI.write(msg);



   }
//------------------------------------------------------------------------------------------------
//------- update stage number and name
//------------------------------------------------------------------------------------------------
void Phenology::setStage(float stageNow)
   {
   stage = stageNow;
   stageName = stageNames[(int)stageNow];
   }
//------------------------------------------------------------------------------------------------
// Register variables for other modules
//------------------------------------------------------------------------------------------------
void Phenology::doRegistrations(void)
   {

   scienceAPI.expose("stage",          "",    "Phenological stage of development",false,                    stage);
   scienceAPI.expose("dlt_tt",         "oCd", "Daily thermal time",false,                                   dltTT);
   scienceAPI.expose("dlt_tt_fm",      "oCd", "Daily thermal time between flowering and maturity",false,    dltTTFM);
   scienceAPI.expose("dlt_stage",      "",    "Change in stage number",false,                               dltStage);
   scienceAPI.expose("tt_sum",         "oCd", "The sum of growing degree days for the current stage",false, ttCurrStage);
   scienceAPI.expose("flowering_date", "doy", "Flowering day number",false,                                 floweringDOY);
   scienceAPI.expose("maturity_date",  "doy", "Maturity day number",false,                                  maturityDOY);
   scienceAPI.expose("flowering_das",  "das", "Days to flowering",false,                                    floweringDAS);
   scienceAPI.expose("maturity_das",   "das", "Days to maturity",false,                                     maturityDAS);
   scienceAPI.expose("stage_code",     "",    "Code of the developmental stages",false,                     stageCode);

   scienceAPI.exposeFunction("tt_tot",  "oCd", "The sum of growing degree days",
                    FloatArrayFunction(&Phenology::getTTTot));
   scienceAPI.exposeFunction("phase_tt", "oCd", "Cumulative growing degree days required for each stage",
                    FloatArrayFunction(&Phenology::getPhaseTT));
   scienceAPI.exposeFunction("stage_name", "", "Full names of stage for reporting",
                    StringFunction(&Phenology::getStageName));



  }
//------------------------------------------------------------------------------------------------
//--------  Do the daily phenology development  - called from plant->process
//-----------   this is two stage thermal time development used in sorghum
//------------------------------------------------------------------------------------------------
void Phenology::development(void)
   {
   float previousStage = stage;
   // update thermal time targets between germination and end Juvenile
   if(stage >= germination && stage <= endJuv)
      {
      checkTargets();
      }
   // calculate daily thermal times
   calcThermalTimes(&plant->today);


   // allow stresses to affect flowering
   calcStressesTT();

   // calculate phenological development (fraction phase elapsed)
   calcPhaseDevelopment();

   // calculate new delta and the new stage
   calcDevelopment();

   // accumulate thermal times
   accumulate (dltTT, ttTotal, previousStage, dltStage);
   accumulate (dltTTFM, ttTotalFM, previousStage, dltStage);
   accumulate (1.0, daysTotal, previousStage, dltStage);
   stageName = stageNames[(int)stage];


   ttCurrStage = ttTotal[(int)stage];

   if(!isEqual((int)previousStage,(int)stage))        // new stage
      {
      char msg[120];
      sprintf(msg, " stage %.1lf %s\n",stage,stageName.c_str());
      scienceAPI.write(msg);

      // send message to plant parts
      for(unsigned i=0;i < plant->PlantParts.size();i++)
         {
         plant->PlantParts[i]->phenologyEvent(stage);
         }
      plant->phenologyEvent(stage);

      //update report vars
      if((int)stage == flowering)
         {
         floweringDAS = plant->das;
         floweringDOY = plant->today.doy;
         }
      if((int)stage == maturity)
         {
         maturityDAS = plant->das;
         maturityDOY = plant->today.doy;
         }
      }

   }
//------------------------------------------------------------------------------------------------
void Phenology::updateVars(void)
   {
   stageCode = float(int(stage));
   }
//------------------------------------------------------------------------------------------------
float Phenology::sumTTtarget(int from, int to)
   {
   return sumVector(ttTarget,from,to);
   }
//------------------------------------------------------------------------------------------------
float Phenology::sumTTtotal(int from, int to)
   {
   return sumVector(ttTotal,from,to);
   }
//------------------------------------------------------------------------------------------------
float Phenology::sumTTtotalFM(int from, int to)
   {
   return sumVector(ttTotalFM,from,to);
   }
//------------------------------------------------------------------------------------------------
float Phenology::sumDaysTotal(int from, int to)
   {
   return sumVector(daysTotal,from,to);
   }
//------------------------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------------------------
void Phenology::checkTargets(void)
   {
 //   if(plant->today.doy >= 313 && plant->today.doy <= 323)
   if(stage >= emergence && stage <= endJuv)
      {
      float photoPeriod = plant->today.getPhotoPeriod(latitude,twilight);
      ttTarget[endJuv] = photoParams.value(photoPeriod);

      float ttEmergFlag = plant->leaf->calcEmergFlagTT();
      ttTarget[fi] = ttEmergFlag - ttTarget[emergence] - ttTarget[endJuv];
      }
   }
//------------------------------------------------------------------------------------------------
//-------- Calculate thermal time
//-----------------------------------------------------------------------------------------------
void Phenology::calcThermalTimes(Today *today)
   {
   dltTT = calcDailyTT(today);
   dltTTFM = calcDailyTTFM(today);
   }
//-----------------------------------------------------------------------------------------------
//   Growing degree day (thermal time) is calculated.
//   Eight interpolations of the air temperature are calculated using a three-hour correction factor.
//   For each air three-hour air temperature, a value of growing degree day is calculated.
//   The eight three-hour estimates are then averaged to obtain the daily value of growing degree days.
//
//-----------------------------------------------------------------------------------------------
float Phenology::calcDailyTT(Today *today)
   {
   float tot = 0.0;
   for(int period = 1; period <= 8; period++)
      {
      // get a three-hour air temperature
      float tMean3Hour = temp3Hr (today->maxT,today->minT, period);
      tot += ttParams.value(tMean3Hour);
      }
   return (tot / 8.0);
   }
//-----------------------------------------------------------------------------------------------
/*    Growing degree day (thermal time) is calculated.
      This function used between flowering and maturity
      G_dlt_tt = 0                  { av_temp <= tt_base oC
               = av_temp - tt_base  { tt_base < av_temp < tt_opt
               = tt_opt             { av_temp >= tt_opt

      default values for tt_base = 5.7 and tt_opt = 24.3                      */
//-----------------------------------------------------------------------------------------------
//
//-----------------------------------------------------------------------------------------------
float Phenology::calcDailyTTFM(Today *today)
   {
   // uses average temperature
   return ttFmParams.value(today->avgT);
   }
//-----------------------------------------------------------------------------------------------
//      a 3 hourly estimate of air temperature
//-----------------------------------------------------------------------------------------------
float Phenology::temp3Hr (float tMax, float tMin, float period)
   {
   float tRangeFract = 0.92105 + 0.1140 * period - 0.0703 * pow(period,2) +
                                                         0.0053 * pow(period,3);
   float diurnalRange = tMax - tMin;
   float deviation = tRangeFract * diurnalRange;
   return  (tMin + deviation);

   }
//-----------------------------------------------------------------------------------------------
//
//-----------------------------------------------------------------------------------------------
float Phenology::calcStressesTT(void)
   {
   float Stress;
   if(stage >= sowing && stage < flowering)
      {
      if(stage < endJuv)
         {
         Stress = plant->water->phenologyStress();
         }
      else
         {
//         float f = Max(0.5, plant->nitrogen->getPhenoStress());
         Stress = Min(plant->water->phenologyStress(),Max(0.5, plant->nitrogen->getPhenoStress()));
         }
      //Update dltTT
      dltTT *= Stress;
      }
   return Stress;
   }
//-----------------------------------------------------------------------------------------------
//--------  Determine fraction of phase that has elapsed
//------------------------------------------------------------------------------------------------
void Phenology::calcPhaseDevelopment(void)
   {
   if(stage >= sowing && stage < germination)
      {
      dltPhase = germinationPhase ();
      }
   // if the stage is < flowering, use ttTotal else use ttTotalFM
   else if(stage >= flowering && stage < maturity)
      {
      dltPhase = phaseFraction(stage, ttTotalFM, dltTTFM, ttTarget);
      }
   else
      {
      dltPhase = phaseFraction(stage, ttTotal, dltTT, ttTarget);
      }
   }

//-----------------------------------------------------------------------------------------------
//
//-----------------------------------------------------------------------------------------------
float Phenology::germinationPhase (void)
   {
   float peswSeed = plant->water->calcPeswSeed();
   if (peswSeed < peswGerm)
      {
      return 0.0;
      }
   if (isEqual(stage,sowing))
      {
      return 0.999;
      }
   return 1.0 + fmod (stage, 1.0);      /* TODO : what is this returning? */
   }

//-----------------------------------------------------------------------------------------------
// Return fraction of thermal time we are through the current phenological phase (0-1)
//-----------------------------------------------------------------------------------------------
float Phenology::phaseFraction(float stage, vector<float> ttTotal,float dltTT,vector<float> stageTT)
   {
   int phase = int (stage);

   float phaseTT = divide (ttTotal[phase] + dltTT, stageTT[phase], 1.0);
   //return phaseTT;
   return bound (phaseTT, 0.0, 1.999999);
   }
//-----------------------------------------------------------------------------------------------
// Determine the current stage of development. calculate the new delta and the new stage
//-----------------------------------------------------------------------------------------------
void Phenology::calcDevelopment(void)
   {
   float newStage = floor(stage) + dltPhase;
   dltStage = newStage - stage;

   if (dltPhase >= 1.0) stage = floor((stage) + 1.0);
   else stage = newStage;

   stage = Min(stage,(float)harvest);
   }
//------------------------------------------------------------------------------------------------
void Phenology::getTTTot(vector<float> &result)
   {
   result = ttTotal;
   }
//------------------------------------------------------------------------------------------------
void Phenology::getPhaseTT(vector<float> &result)
   {
   result = ttTarget;
   }
//------------------------------------------------------------------------------------------------
void Phenology::getStageName(string &result)
   {
   result = stageName;
   }
//------------------------------------------------------------------------------------------------
void Phenology::Summary(void)
   {
   char msg[120];
   sprintf(msg,"flowering (DAS)       = %.0d \t maturity (DAS)          = %.0d\n",
                               floweringDAS, maturityDAS);
   scienceAPI.write(msg);
   sprintf(msg,"flowering day         = %.0d \t maturity day            = %.0d\n",
                              floweringDOY, maturityDOY);
   scienceAPI.write(msg);
   }
//------------------------------------------------------------------------------------------------

