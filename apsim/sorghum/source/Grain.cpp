//------------------------------------------------------------------------------------------------
#include "Grain.h"
#include "Plant.h"

//---------------------------------------------------------------------------
//------ Grain Constructor
//------------------------------------------------------------------------------------------------
Grain::Grain(ScienceAPI &api, Plant *p) : PlantPart(api)
   {
   plant = p;
   name = "Grain";
   partNo = 4;

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
   scienceAPI.expose("GrainGreenWt",    "g/m^2",      "Live grain dry weight",    false, dmGreen);
   scienceAPI.expose("GrainNo",         "grains/m^2", "Grain number",             false, grainNo);
   scienceAPI.expose("GrainSize",       "g/1000grain","1000 grain weight",        false, grainSize);
   scienceAPI.expose("GrainGreenN",     "g/m^2",      "N in grain",               false, nGreen);
   scienceAPI.expose("GrainGreenNConc", "%",          "N concentration in grain", false, nConc);
   scienceAPI.expose("Yield",           "kg/ha",      "Grain yield",              false, yield);
   scienceAPI.expose("GrainGreenP",     "g/m^2",      "P in live Grain",          false, pGreen);
   }
//------------------------------------------------------------------------------------------------
//------- Initialize variables
//------------------------------------------------------------------------------------------------
void Grain::initialize(void)
   {
   grainNo = 0.0;
   finalGrainNo = 0.0;
   grainSize = 0.0;
   dltDMGrainDemand = 0.0;
   yield = 0.0;

   PlantPart::initialize();
   }
//------------------------------------------------------------------------------------------------
//------ read Grain parameters
//------------------------------------------------------------------------------------------------
void Grain::readParams (void)
   {
   scienceAPI.read("dm_per_seed", "", 0, dmPerSeed);
   scienceAPI.read("grn_water_cont", "", 0, waterContent);
   // nitrogen
   scienceAPI.read("grainFillRate","", 0, grainFillRate);
   scienceAPI.read("targetGrainNConc", "", 0, targetNConc);

   // phosphorus
   pMaxTable.read(scienceAPI, "x_p_stage_code","y_p_conc_max_grain");
   pMinTable.read(scienceAPI, "x_p_stage_code","y_p_conc_min_grain");
   pSenTable.read(scienceAPI, "x_p_stage_code","y_p_conc_sen_grain");
   scienceAPI.read("p_conc_init_grain", "", 0, initialPConc);   /* TODO : Remove this */
   }

//------------------------------------------------------------------------------------------------
//------ update variables
//------------------------------------------------------------------------------------------------
void Grain::updateVars(void)
   {
   // initialise P - must be better way
//   if(dmGreen < 1e-5 && dltDmGreen > 0)
//      pGreen = initialPConc * dltDmGreen;


   dmGreen += dltDmGreen;
   dmGreen += dmRetranslocate;
   nGreen += dltNGreen  + dltNRetranslocate;
   nConc = divide(nGreen,dmGreen,0) * 100.0;

   grainSize = divide (dmGreen, grainNo, 0.0) * 1000.0;
   stage = plant->phenology->currentStage();

   // Ramp grain number from 0 at StartGrainFill to finalGrainNo at SGF + 100dd
   float gfTTNow = plant->phenology->sumTTtotalFM(startGrainFill,maturity);
   grainNo = Min((gfTTNow/100.0 *  finalGrainNo),finalGrainNo);

   yield = dmGreen * 10.0;                   // yield in kg/ha for reporting


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
   // if not in grain fill, no demand
   if(stage < startGrainFill)return nDemand;


   // for the first half of grainfilling, the demand is calculated on a grain
   // filling rate per grain per oCd
   // rest on target N concentration

   float gfFract = divide(plant->phenology->sumTTtotal(startGrainFill, maturity),
                        plant->phenology->sumTTtarget(startGrainFill, maturity));

   if(gfFract < 0.5)
      nDemand = grainNo * plant->phenology->getDltTTFM() * grainFillRate / 1000.0;
   else
      nDemand = dltDmGreen * targetNConc;

   nDemand = Max(nDemand,0.0);
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

   float totDMCaryopsis = divide(plant->biomass->getDltDM() , grainNo);
   totDMCaryopsis = divide(totDMCaryopsis, plant->phenology->getDltTTFM());
   return (0.0000319 + 0.4026 * totDMCaryopsis) * plant->phenology->getDltTTFM() * grainNo;
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
   char msg[120];
   sprintf(msg,"Stover (kg/ha)        = %.1f \t Grain yield (kg/ha)     = %.1f\n",
               plant->biomass->getAboveGroundBiomass() - dmGreen * 10.0, dmGreen * 10.0);
   scienceAPI.write(msg);
   sprintf(msg,"Grain %% water content = %.1f \t\t Grain yield wet (kg/ha) = %.1f\n",
               waterContent*100,dmGreen * 10.0 * 100 / (100 - waterContent*100));
   scienceAPI.write(msg);
   sprintf(msg,"Weight 1000 grains(g) = %.1f \t\t Grains/m^2              = %.1f\n",
         grainSize, grainNo);scienceAPI.write(msg);
   sprintf(msg,"Grains/head           = %.1f\n",grainNo / plant->getPlantDensity());
   scienceAPI.write(msg);
  }
//------------------------------------------------------------------------------------------------
void  Grain::Harvest(void)
   {
   // send crop_chopped
   if(dmGreen > 0)
      {
      // Build surface residues by part
      vector<string> part_name;
      vector<float> fraction_to_residue;           // fraction sent to residue (0-1)
      vector<float> dlt_dm_crop;                   // change in dry matter of crop (kg/ha)
      vector<float> dlt_dm_n;                      // N content of changed dry matter (kg/ha)
      vector<float> dlt_dm_p;                      // P content of changed dry matter (kg/ha)

      float fracts[] = {0.0, 0.0, 0.0, 0.0, 0.0};  // No root or grain to residue.

      for (unsigned part = 0; part < plant->PlantParts.size(); part++)
         {
         part_name.push_back(plant->PlantParts[part]->getName());
         if(part < 4)
            {
            dlt_dm_crop.push_back(0.0);
            dlt_dm_n.push_back(0.0);
            dlt_dm_p.push_back(0.0);
            }
         else
            {
            dlt_dm_crop.push_back((plant->PlantParts[part]->getDmGreen() +
                  plant->PlantParts[part]->getDmSenesced()) * gm2kg/sm2ha);
            dlt_dm_n.push_back((plant->PlantParts[part]->getNGreen() +
                  plant->PlantParts[part]->getNSenesced()) * gm2kg/sm2ha);
            dlt_dm_p.push_back((plant->PlantParts[part]->getPGreen() +
                  plant->PlantParts[part]->getPSenesced()) * gm2kg/sm2ha);
            }

         fraction_to_residue.push_back(fracts[part]);
         }

      Variant chopped;
      pack(chopped, "crop_type",   plant->getCropType());
      pack(chopped, "dm_type",     part_name);
      pack(chopped, "dlt_crop_dm", dlt_dm_crop);
      pack(chopped, "dlt_dm_n",    dlt_dm_n);
      pack(chopped, "dlt_dm_p",    dlt_dm_p);
      pack(chopped, "fraction_to_residue", fraction_to_residue);


      scienceAPI.publish ("crop_chopped", chopped); 
      }
   initialize();
   }

