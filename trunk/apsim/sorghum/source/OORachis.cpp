#pragma hdrstop

#include "OOPlant.h"
#include "OOPlantComponents.h"
#include "OORachis.h"
//---------------------------------------------------------------------------

#pragma package(smart_init)

//------------------------------------------------------------------------------------------------
//------ Rachis Constructor
//------------------------------------------------------------------------------------------------
Rachis::Rachis(ScienceAPI &api, OOPlant *p) : PlantPart(api)
   {
   plant = p;
   name = "Rachis";

   doRegistrations();
   initialize();
   }
//------------------------------------------------------------------------------------------------
//------ Rachis Destructor
//------------------------------------------------------------------------------------------------
Rachis::~Rachis()
   {
   }
//--------------------------------------------------------------------------------------------------
// Register variables for other modules
//--------------------------------------------------------------------------------------------------
void Rachis::doRegistrations(void)
   {
   scienceAPI.expose("flower_wt",     "g/m2", "Live flower dry weight", 0,      dmGreen);
   scienceAPI.expose("flower_n",      "g/m2", "N in flower", 0,                 nGreen);
   scienceAPI.expose("n_conc_flower", "",     "Flower N concentration", 0,      nConc);
   scienceAPI.expose("flower_nd",     "g/m2", "Today's N demand from flower", 0,nDemand);
   }
//------------------------------------------------------------------------------------------------
//------- Initialize variables
//------------------------------------------------------------------------------------------------
void Rachis::initialize(void)
   {
   partNo = 3;
   PlantPart::initialize();
   }
//------------------------------------------------------------------------------------------------
//------ read Rachis parameters
//------------------------------------------------------------------------------------------------
void Rachis::readParams (string cultivar)
   {
   // nitrogen
   scienceAPI.read("initialRachisNConc","", 0, initialNConc      );
   scienceAPI.read("targetRachisNConc" ,"", 0, targetNConc       );
   scienceAPI.read("structRachisNConc" ,"", 0, structRachisNConc );
   // phosphorus
   pMaxTable.read(scienceAPI, "x_p_stage_code","y_p_conc_max_flower");
   pMinTable.read(scienceAPI, "x_p_stage_code","y_p_conc_min_flower");
   pSenTable.read(scienceAPI, "x_p_stage_code","y_p_conc_sen_flower");
   scienceAPI.read("p_conc_init_flower", "", 0, initialPConc);
   }

//------------------------------------------------------------------------------------------------
//------ read Rachis parameters
//------------------------------------------------------------------------------------------------
void Rachis::updateVars(void)
   {
   dmGreen += dltDmGreen;
   nGreen += (dltNGreen + dltNRetranslocate);
   nConc = divide(nGreen,dmGreen,0);
   stage = plant->phenology->currentStage();
   }
//------------------------------------------------------------------------------------------------
//------- react to a phenology event
//------------------------------------------------------------------------------------------------
void Rachis::phenologyEvent(int iStage)
   {
   switch (iStage)
      {
      case emergence :
         dmGreen = 0.0;
         nGreen = initialNConc * dmGreen;
         pGreen = initialPConc * dmGreen;
         break;
      }
   }
//------------------------------------------------------------------------------------------------
//------- nitrogen
//------------------------------------------------------------------------------------------------
float Rachis::calcNDemand(void)
   {
   nDemand = 0.0;
   if(stage >= startGrainFill)return nDemand;

   // RACHIS N demand (g/m2) to keep rachis [N] at targetRachisNConc
   float nRequired = (dmGreen + dltDmGreen) * targetNConc;
   nDemand = Max(nRequired - nGreen,0.0);
   return nDemand;
   }
//------------------------------------------------------------------------------------------------
float Rachis::calcStructNDemand(void)
   {
   // calculate N required to maintain structural [N]
   float structNDemand = 0.0;
   if(stage >= startGrainFill)return structNDemand;

   // RACHIS demand to keep rachis [N] at levels of structRachisNConc
   float nRequired = (dmGreen + dltDmGreen) * structRachisNConc;
   structNDemand = Max(nRequired - nGreen,0.0);
   return structNDemand;
   }
//------------------------------------------------------------------------------------------------
float Rachis::provideN(float requiredN)
   {
   // calculate the N available for translocation to other plant parts
   // N could be required for structural Stem/Rachis N, new leaf N or grain N
   // Rachis N is available at a rate which is a function of rachis [N]
   // dltRachisNconc per day (17dd) = 0.076*rachisNconcPct - 0.0199

   
   float rachisNConcPct = divide((nGreen + dltNGreen),(dmGreen + dltDmGreen)) * 100;
   if(rachisNConcPct < structRachisNConc * 100)return 0;
   float NConcPctAvailable = divide(17,plant->phenology->getDltTT())
                                          * (0.076 * rachisNConcPct - 0.0199);
   float availableN = NConcPctAvailable / 100 * (dmGreen + dltDmGreen);
   availableN = Max(availableN,0.0);
   
   float nProvided = Min(availableN,requiredN);
   dltNRetranslocate -= nProvided;
   return nProvided;
   }
//------------------------------------------------------------------------------------------------
float Rachis::partitionDM(float dltDM)
   {
   dltDmGreen = dltDM;
   return dltDmGreen;
   }
//------------------------------------------------------------------------------------------------
float Rachis::calcPDemand(void)
   {
   // Leaf P demand

   float rel_growth_rate = divide(plant->biomass->getDltDMPotRUE(),
         plant->biomass->getAboveGroundBiomass(),0.0);

   float deficit = pConcMax() * dmGreen * (1.0 + rel_growth_rate) - pGreen;

   pDemand = Max(deficit,0.0);
   return pDemand;
   }
//------------------------------------------------------------------------------------------------

