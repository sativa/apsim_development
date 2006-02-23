

#pragma hdrstop

#include "OORachis.h"
#include "OOPlant.h"
//---------------------------------------------------------------------------

#pragma package(smart_init)

//------------------------------------------------------------------------------------------------
//------ Rachis Constructor
//------------------------------------------------------------------------------------------------
Rachis::Rachis(OOPlant *p)
   {
   plant = p;
   plantInterface = p->plantInterface;

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
#define setupGetVar plantInterface->addGettableVar
   setupGetVar("flower_wt", dmGreen, "g/m2", "Live flower dry weight");
   setupGetVar("flower_n", nGreen, "g/m2", "N in flower");
   setupGetVar("n_conc_flower", nConc, "", "Flower N concentration");
   setupGetVar("flower_nd", nDemand, "g/m2", "Today's N demand from flower");

#undef setupGetVar   
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
   vector<string> sections;                  // sections to look for parameters
   sections.push_back("constants");
   sections.push_back(cultivar);
   // nitrogen
   initialNConc = readVar(plantInterface,sections,"initialRachisNConc");
   targetNConc = readVar(plantInterface,sections,"targetRachisNConc");
   structRachisNConc = readVar(plantInterface,sections,"structRachisNConc");
   // phosphorus
   pMaxTable.read(plantInterface,sections,"x_p_stage_code","y_p_conc_max_flower");
   pMinTable.read(plantInterface,sections,"x_p_stage_code","y_p_conc_min_flower");
   pSenTable.read(plantInterface,sections,"x_p_stage_code","y_p_conc_sen_flower");
   initialPConc = readVar(plantInterface,sections,"p_conc_init_flower");

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

   // zero deltas
   dltDmGreen = 0.0;
   dltNRetranslocate = 0.0;
   dltNGreen = 0.0;
//   dltDmGreen = 0.0;

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

