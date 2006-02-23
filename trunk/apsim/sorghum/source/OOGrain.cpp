//------------------------------------------------------------------------------------------------

#pragma hdrstop

#include "OOGrain.h"
#include "OOPlant.h"

//---------------------------------------------------------------------------

//#pragma package(smart_init)

//------------------------------------------------------------------------------------------------
//------ Grain Constructor
//------------------------------------------------------------------------------------------------
Grain::Grain(OOPlant *p)
   {
   plant = p;
   plantInterface = p->plantInterface;

   doRegistrations();
   initialize();
   }
//------------------------------------------------------------------------------------------------
//------ Grain Destructor
//------------------------------------------------------------------------------------------------
Grain::~Grain()
   {

   }
//--------------------------------------------------------------------------------------------------
// Register variables for other modules
//--------------------------------------------------------------------------------------------------
void Grain::doRegistrations(void)
   {
#define setupGetVar plantInterface->addGettableVar
   setupGetVar("grain_wt", dmGreen, "g/m2", "Live grain dry weight");
   setupGetVar("grain_no", grainNo, "grains/m2", "Grain number");
   setupGetVar("grain_size", grainSize, "g/grain", "Individual grain weight");
   setupGetVar("grain_n", nGreen, "g/m2", "N in grain");
//   setupGetVar("dlt_n_retrans", dltNRetranslocate, "g/m2", "Nitrogen retranslocated out from parts to grain");
   setupGetVar("n_conc_grain", nConc, "%", "N concentration in grain");
   setupGetVar("grain_nd", nDemand, "g/m2", "Today's N demand from grain");

#undef setupGetVar     
   }
//------------------------------------------------------------------------------------------------
//------- Initialize variables
//------------------------------------------------------------------------------------------------
void Grain::initialize(void)
   {
   grainNo = 0.0;
   finalGrainNo = 0.0;
   grainSize = 0.0;
   addGrainWeight = -1;
   dltDMGrainDemand = 0.0;

   partNo = 4;
   PlantPart::initialize();
   }
//------------------------------------------------------------------------------------------------
//------ read Grain parameters
//------------------------------------------------------------------------------------------------
void Grain::readParams (string cultivar)
   {
   vector<string> sections;                  // sections to look for parameters
   sections.push_back("constants");
   sections.push_back(cultivar);
   dmPerSeed = readVar(plantInterface,sections,"dm_per_seed");
   waterContent = readVar(plantInterface,sections,"grn_water_cont");
   // nitrogen
   grainFillRate = readVar(plantInterface,sections,"grainFillRate");
   targetNConc = readVar(plantInterface,sections,"targetGrainNConc");

   // phosphorus
   pMaxTable.read(plantInterface,sections,"x_p_stage_code","y_p_conc_max_grain");
   pMinTable.read(plantInterface,sections,"x_p_stage_code","y_p_conc_min_grain");
   pSenTable.read(plantInterface,sections,"x_p_stage_code","y_p_conc_sen_grain");
   initialPConc = readVar(plantInterface,sections,"p_conc_init_grain");
   }

//------------------------------------------------------------------------------------------------
//------ update variables
//------------------------------------------------------------------------------------------------
void Grain::updateVars(void)
   {
   // initialise P - must be better way
   if(dmGreen < 1e-5 && dltDmGreen > 0)
      pGreen = initialPConc * dltDmGreen;


   dmGreen += dltDmGreen;
   dmGreen += dmRetranslocate;
   nGreen += dltNGreen  + dltNRetranslocate;
   nConc = divide(nGreen,dmGreen,0);

   grainSize = divide (dmGreen + dmDead, grainNo, 0.0);
   stage = plant->phenology->currentStage();

   // Ramp grain number from 0 at StartGrainFill to finalGrainNo at SGF + 100dd
   float gfTTNow = plant->phenology->sumTTtotalFM(startGrainFill,maturity);
   grainNo = Min((gfTTNow/100.0 *  finalGrainNo),finalGrainNo);


 //  dltDmGreen = 0.0;
 //  dmRetranslocate = 0.0;
 //  dltDMGrainDemand = 0.0;


   }
//------------------------------------------------------------------------------------------------
//------- react to a phenology event
//------------------------------------------------------------------------------------------------
void Grain::phenologyEvent(int iStage)
   {
   switch (iStage)
      {
      case emergence : 
         break;
      case fi :
         totDMGreenFI = plant->biomass->getTotalBiomass();                  // for grain number
         break;
      case startGrainFill :
         // Reset translocation limits
         /* TODO : This seems sus - check with graeme */
//         plant->stem->translocFrac = 0.5;
//         plant->leaf->translocFrac = 0.3;

         finalGrainNo = calcGrainNumber();
         break;
      }
   }
//------------------------------------------------------------------------------------------------
void Grain::calcDemandStress(void)
   {
   // for HI approach ?
   /* TODO : See if this needs updating here Should not happen*/
//   plant->water->photosynthesisStress = divide(plant->water->totalSupply,
//                           plant->water->swDemand,1.0);

   dltDMStressMax = yieldPartDemandStress();
   }
//------------------------------------------------------------------------------------------------
void Grain::calcBiomassDemand(void)
   {
   // source sink (grain number approach)
   dltDMGrainDemand = calcDMGrainSourceSink();
   }
//------------------------------------------------------------------------------------------------
//------- calc N demand
//------------------------------------------------------------------------------------------------
//     GRAIN demand to keep grain N filling rate at 0.001mg/grain/dd up to halfway
//       between sgf and maturity where dd is degree days from start_grain_fill
//       then target [N] (1.75%)
float Grain::calcNDemand(void)
   {
   nDemand = 0.0;
   if(stage < startGrainFill)return nDemand;


   float gfFract = plant->phenology->sumTTtotal(startGrainFill, maturity)/
                        plant->phenology->sumTTtarget(startGrainFill, maturity);

   float nRequired = 0.0;
   if(gfFract < 0.5)
      nRequired = grainNo * plant->phenology->getDltTT() * grainFillRate / 1000.0;
   else
//      nRequired = ((dmGreen + dltDmGreen) * targetNConc) - nGreen;
      nRequired = dltDmGreen * targetNConc;

   nDemand = Max(nRequired,0.0);
   return nDemand;
   }
//------------------------------------------------------------------------------------------------
float Grain::calcGrainNumber(void)
   {
   // increase in plant biomass between fi and start grain fill
   float dltDMPlant = plant->biomass->getTotalBiomass()  - totDMGreenFI;

   // growth rate per day
   float nDays = plant->phenology->sumDaysTotal(fi,startGrainFill);
   float growthRate = divide(dltDMPlant,nDays);
   return divide(growthRate, dmPerSeed);
   }
//------------------------------------------------------------------------------------------------
// Calculate the stress factor for diminishing potential harvest index
float Grain::yieldPartDemandStress(void)
   {
   float rueReduction = Min(plant->getTempStress(),plant->nitrogen->getPhotoStress());
   return plant->water->photosynthesisStress() * rueReduction;
   }
//------------------------------------------------------------------------------------------------
// calculate daily grain dm demand using source / sink approach
float Grain::calcDMGrainSourceSink(void)
   {
   // proportion of grain filling stage
   float fracGF = stage - startGrainFill;
   float ttFlowerMaturity = plant->phenology->sumTTtarget(flowering,maturity);
   float lag = divide((140 - plant->phenology->getTTtarget(flowering)),
                     (ttFlowerMaturity - plant->phenology->getTTtarget(flowering)));

   if(fracGF <= lag)return plant->biomass->getDltDM() * 0.25;

   // fraction 0.78 used because end_grain_fill is 95% of total flowering to maturity
   // Ronnie started levelling off at 515 GDD which is 0.78 of 95% of 695
   if(fracGF < 0.78)
      {
      float totDMCaryopsis = divide(plant->biomass->getDltDM() , grainNo);
      totDMCaryopsis = divide(totDMCaryopsis, plant->phenology->getDltTTFM());
      return (0.0000319 + 0.4026 * totDMCaryopsis) * plant->phenology->getDltTTFM() * grainNo;
      }

   // ony occurs first time fracGF >= 0.78
   if(addGrainWeight <= 0.0)
      {
      float grainWt = divide((dmGreen + dmDead),grainNo);
      float grainWtMax = divide(grainWt,(0.85 + 0.6* (fracGF-0.75)));
      addGrainWeight = divide((grainWtMax - grainWt),
                  (ttFlowerMaturity - (ttFlowerMaturity * fracGF)));
      }
   return addGrainWeight * plant->phenology->getDltTTFM() * grainNo;
   }

//------------------------------------------------------------------------------------------------
void Grain::RetranslocateN(float N)
   {
   dltNRetranslocate += N;
   }
//------------------------------------------------------------------------------------------------
float Grain::partitionDM(float dltDM)
   {
   dltDmGreen = Min(dltDMGrainDemand, dltDM);
   return Max(dltDmGreen,0.0);
   }
//------------------------------------------------------------------------------------------------
float Grain::grainDMDifferential(void)
   {
   return dltDMGrainDemand - dltDmGreen;
   }
//------------------------------------------------------------------------------------------------
float Grain::calcPDemand(void)
   {
   // Grain P demand   (demand from soil)
   pDemand = 0.0;
   return pDemand;
   }
//------------------------------------------------------------------------------------------------
float Grain::calcPRetransDemand(void)
   {
   // Grain P demand
   float maxP = pConcMax() * dmGreen;
   return Max(maxP - pGreen,0.0);
   }
//------------------------------------------------------------------------------------------------
void Grain::Summary(void)
   {
   summaryLine(plantInterface,"stover (kg/ha)        = %.1f \t grain yield (kg/ha)     = %.1f",
               plant->biomass->getAboveGroundBiomass() - dmGreen * 10.0, dmGreen * 10.0);
   summaryLine(plantInterface,"grain %% water content = %.1f \t grain yield wet (kg/ha) = %.1f",
               waterContent*100,dmGreen * 10.0 * 100 / (100 - waterContent*100));
   summaryLine(plantInterface,"grain wt (g)          = %.3f \t grains/m^2              = %.1f",
               grainSize,grainNo);
   summaryLine(plantInterface,"grains/head           = %.1f",grainNo / plant->getPlantDensity(),NULL);
  }


