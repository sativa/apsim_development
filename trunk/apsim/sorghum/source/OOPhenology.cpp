//------------------------------------------------------------------------------------------------
#pragma hdrstop

#include "OOPhenology.h"
#include "TypeKind.h"
#include "Utilities.h"
#include "OOPlant.h"

using namespace std;

//------------------------------------------------------------------------------------------------

#pragma package(smart_init)

//------------------------------------------------------------------------------------------------
//------ Phenology constructor
//------------------------------------------------------------------------------------------------
Phenology::Phenology(OOPlant *p)
   {
   plant = p;
   plantInterface = p->plantInterface;

   readArray(plantInterface, "constants","stage_names","",stageNames,false);
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
   vector<string> sections;                  // sections to look for parameters
   sections.push_back("constants");
   sections.push_back(cultivar);

   float shootLag,shootRate;
   readVar(plantInterface,sections,"shoot_lag",shootLag);
   readVar(plantInterface,sections,"shoot_rate",shootRate);
   ttTarget[germination] = shootLag + plant->getSowingDepth() * shootRate;

   ttTarget[emergence] = readVar(plantInterface,sections,"tt_emerg_to_endjuv");
   ttEndJuvInit = readVar(plantInterface,sections,"tt_endjuv_to_init");
   ttTarget[endJuv] = ttEndJuvInit;
   ttTarget[fi] = 50;
   ttTarget[flag] = readVar(plantInterface,sections,"tt_flag_to_flower");
   ttTarget[flowering] = readVar(plantInterface,sections,"tt_flower_to_start_grain");
   ttFlowerMaturity = readVar(plantInterface,sections,"tt_flower_to_maturity");
   ttTarget[endGrainFill] = 0.05 * ttFlowerMaturity;
   ttTarget[startGrainFill] = ttFlowerMaturity - ttTarget[flowering]
                                       - ttTarget[endGrainFill];
   ttTarget[maturity] = readVar(plantInterface,sections,"tt_maturity_to_ripe");


   // photoperiod function
   vector<float> xVec;
   vector<float> yVec;
   xVec.push_back(readVar(plantInterface,sections,"photoperiod_crit1"));
   xVec.push_back(readVar(plantInterface,sections,"photoperiod_crit2"));
   float slope = readVar(plantInterface,sections,"photoperiod_slope");
   yVec.push_back(ttEndJuvInit);
   yVec.push_back(ttEndJuvInit + (xVec[1]-xVec[0]) * slope);
   photoParams.load(xVec,yVec);

   // thermal time
   ttParams.read(plantInterface,sections,"x_temp","y_tt");
   ttFmParams.read(plantInterface,sections,"x_temp_fm","y_tt_fm");

   // germination
   peswGerm = readVar(plantInterface,sections,"pesw_germ");

   int latitudeID    = plantInterface->addRegistration(RegistrationType::get,
                                                            "latitude", floatType,"", "");
  
   plantInterface->getVariable(latitudeID, latitude, -90.0, 90.0, true);
   readVar (plantInterface,sections, "twilight",twilight);


   // report
   plantInterface->writeString ("    -------------------------------------------------------");
   char msg[100];
   sprintf(msg, "    tt_emerg_to_endjuv       =  %6.2f",ttEndJuvInit);plantInterface->writeString (msg);
   sprintf(msg, "    tt_flower_to_maturity    =  %6.2f",ttFlowerMaturity);  plantInterface->writeString (msg);
   sprintf(msg, "    tt_flag_to_flower        =  %6.2f",ttTarget[flag]);  plantInterface->writeString (msg);
   sprintf(msg, "    tt_flower_to_start_grain =  %6.2f",ttTarget[flowering]);  plantInterface->writeString (msg);
   sprintf(msg, "    tt_maturity_to_ripe      =  %6.2f",ttTarget[maturity]);  plantInterface->writeString (msg);

   plantInterface->writeString ("    -------------------------------------------------------");


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
#define setupGetVar plantInterface->addGettableVar
   setupGetVar("stage", stage, "", "Phenological stage of development");
//   setupGetVar("stage_name", stageName, "", "Full names of stage for reporting");
   setupGetVar("dlt_tt", dltTT, "oCd", "Daily thermal time");
   setupGetVar("dlt_tt_fm", dltTTFM, "oCd", "Daily thermal time between flowering and maturity");
   setupGetVar("dlt_stage", dltStage, "", "Change in stage number");
   setupGetVar("tt_sum", ttCurrStage, "oCd", "The sum of growing degree days for the current stage");
   setupGetVar("flowering_date", floweringDOY, "doy", "Flowering day number");
   setupGetVar("maturity_date", maturityDOY, "doy", "Maturity day number");
   setupGetVar("flowering_das", floweringDAS, "das", "Days to flowering");
   setupGetVar("maturity_das", maturityDAS, "das", "Days to maturity");
   setupGetVar("stage_code", stageCode, "", "Code of the developmental stages");
#undef setupGetVar

   setupGetFunction(plantInterface,"tt_tot", protocol::DTsingle, true,
                    &Phenology::getTTTot, "oCd", "The sum of growing degree days");
   setupGetFunction(plantInterface,"phase_tt", protocol::DTsingle, true,
                    &Phenology::getPhaseTT, "oCd", "Cumulative growing degree days required for each stage");
   setupGetFunction(plantInterface,"stage_name", protocol::DTstring, false,
                    &Phenology::getStageName, "", "Full names of stage for reporting");



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
      char msg[80];
      sprintf(msg, " stage %.1lf %s",stage,stageName.c_str());
      plantInterface->writeString(msg);

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
         floweringDOY = plant->today.todayDate.doy;
         }
      if((int)stage == maturity)
         {
         maturityDAS = plant->das;
         maturityDOY = plant->today.todayDate.doy;
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
 //   if(plant->today.todayDate.doy >= 313 && plant->today.todayDate.doy <= 323)
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
void Phenology::getTTTot(protocol::Component *system, protocol::QueryValueData &qd)
   {
   system->sendVariable(qd, protocol::vector<float>(&ttTotal[0], &ttTotal[0] + ttTotal.size()));
   }
//------------------------------------------------------------------------------------------------
void Phenology::getPhaseTT(protocol::Component *system, protocol::QueryValueData &qd)
   {
   system->sendVariable(qd, protocol::vector<float>(&ttTarget[0], &ttTarget[0] + ttTarget.size()));
   }
//------------------------------------------------------------------------------------------------
void Phenology::getStageName(protocol::Component *system, protocol::QueryValueData &qd)
   {
   system->sendVariable(qd, FString(stageName.c_str()));
   }
//------------------------------------------------------------------------------------------------
void Phenology::Summary(void)
   {
   summaryLine(plantInterface,"flowering (DAS)       = %.0f \t maturity (DAS)          = %.0f",
                               floweringDAS, maturityDAS);
   summaryLine(plantInterface,"flowering day         = %.0f \t maturity day            = %.0f",
                              floweringDOY, maturityDOY);
   }
//------------------------------------------------------------------------------------------------

