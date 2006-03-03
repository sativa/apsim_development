//------------------------------------------------------------------------------------------------

#pragma hdrstop

#include "OOStem.h"
#include "OOPlant.h"

//---------------------------------------------------------------------------

#pragma package(smart_init)

//------------------------------------------------------------------------------------------------
//------ Stem Constructor
//------------------------------------------------------------------------------------------------
Stem::Stem(OOPlant *p)
   {
   plant = p;
   plantInterface = p->plantInterface;
   name = "Stem";

   doRegistrations();
   initialize();
   }
//--------------------------------------------------------------------------------------------------
// Register variables for other modules
//--------------------------------------------------------------------------------------------------
void Stem::doRegistrations(void)
   {
#define setupGetVar plantInterface->addGettableVar
//   setupGetVar("height", canopyHeight, "mm", "Height from ground to the top of canopy");
   setupGetVar("stem_wt", dmGreen, "g/m2", "Stem dry weight");
   setupGetVar("stem_n", nGreen, "g/m2", "N in stem");
   setupGetVar("dlt_n_retrans_stem", dltNRetranslocate, "g/m2", "Nitrogen retranslocated from stem to grain");
   setupGetVar("n_conc_stem", nConc, "%", "Stem N concentration");
   setupGetVar("stem_nd", nDemand, "g/m2", "Today's N demand from the stem");
   setupGetVar("dlt_n_green_stem", dltNGreen, "g/m2", "Today's N increase in stem");
   setupGetVar("n_conc_stem", nConc, "%", "Live stem N concentration");

#undef setupGetVar
   }
//------------------------------------------------------------------------------------------------
//------- Initialize variables
//------------------------------------------------------------------------------------------------
void Stem::initialize(void)
   {
   canopyHeight = 0.0;
   dltCanopyHeight = 0.0;
   dmGreenStem = 0.0;

   partNo = 2;
   PlantPart::initialize();
   }
//------------------------------------------------------------------------------------------------
//------ read Stem parameters
//------------------------------------------------------------------------------------------------
void Stem::readParams (string cultivar)
   {
   vector<string> sections;                  // sections to look for parameters
   sections.push_back("constants");
   sections.push_back(cultivar);
   heightFn.read(plantInterface,sections,"x_stem_wt","y_height");
   initialDM = readVar(plantInterface,sections,"dm_stem_init");
   translocFrac = readVar(plantInterface,sections,"stem_trans_frac");
   // nitrogen
   initialNConc = readVar(plantInterface,sections,"initialStemNConc");
   targetNFn.read(plantInterface,sections,"x_stem_n","targetStemNConc");
//   targetNConc  = readVar(plantInterface,sections,"targetStemNConc");
   structNFn.read(plantInterface,sections,"x_stem_n","structStemNConc");

   // phosphorus
   pMaxTable.read(plantInterface,sections,"x_p_stage_code","y_p_conc_max_stem");
   pMinTable.read(plantInterface,sections,"x_p_stage_code","y_p_conc_min_stem");
   pSenTable.read(plantInterface,sections,"x_p_stage_code","y_p_conc_sen_stem");
   initialPConc = readVar(plantInterface,sections,"p_conc_init_stem");

   density = plant->getPlantDensity();
   }

//------------------------------------------------------------------------------------------------
void Stem::process(void)
   {
   calcCanopyHeight();
   }

//------------------------------------------------------------------------------------------------
//------ update Stem variables
//------------------------------------------------------------------------------------------------
void Stem::updateVars(void)
   {
   float dayNConc = divide(nGreen,dmGreen,0);
   dmGreen += dltDmGreen;
   dmGreen += dmRetranslocate;

   dmGreenStem = dmGreen / density;

   canopyHeight += dltCanopyHeight;
   nGreen += (dltNGreen +  dltNRetranslocate);
   nConc = divide(nGreen,dmGreen,0);
   dltNConc = dayNConc - nConc;

   stage = plant->phenology->currentStage();

   dltDmGreen = 0.0;
   dmRetranslocate = 0.0;
   dltNRetranslocate = 0.0;
   dltNGreen = 0.0;
   }
//------------------------------------------------------------------------------------------------
//------- react to a phenology event
//------------------------------------------------------------------------------------------------
void Stem::phenologyEvent(int iStage)
   {
   switch (iStage)
      {
      case emergence :
         dmGreen = initialDM * density;
         nGreen = initialNConc * dmGreen;
         pGreen = initialPConc * dmGreen;
         break;
      case flowering :
         //set the minimum weight of stem; used for translocation to grain and stem
         float dmPlantStem = divide (dmGreen, density);
         dmPlantMin = dmPlantStem * (1.0 - translocFrac);
         break;
      }
   }
//------------------------------------------------------------------------------------------------
void Stem::calcCanopyHeight(void)
   {
   float newHeight = heightFn.value(dmGreenStem);
   dltCanopyHeight = Max(0.0,newHeight - canopyHeight);
   }
//------------------------------------------------------------------------------------------------
float Stem::calcNDemand(void)
   {
   nDemand = 0.0;
//   if(stage <= startGrainFill)
//      {
      // STEM demand (g/m2) to keep stem [N] at levels from  targetStemNConc
      float nRequired = (dmGreen + dltDmGreen) * targetNFn.value(stage);
      nDemand = Max(nRequired - nGreen,0.0);
//      }
   return nDemand;
   }
//------------------------------------------------------------------------------------------------
float Stem::calcStructNDemand(void)
   {
   // calculate N required to maintain structural [N]
   float structNDemand = 0.0;
   if(stage >= startGrainFill)return structNDemand;

   // STEM demand to keep stem [N] at levels of structStemNConc
   float nRequired = (dmGreen + dltDmGreen) * structNFn.value(stage);
   structNDemand = Max(nRequired - nGreen,0.0);
//   float stemNConcPct = divide((nGreen + dltNGreen+structNDemand),(dmGreen + dltDmGreen)) * 100;
   return structNDemand;
   }
//------------------------------------------------------------------------------------------------
float Stem::provideN(float requiredN)
   {
   // calculate the N available for translocation to other plant parts
   // N could be required for structural Stem/Rachis N, new leaf N or grain N
   // Stem N is availavle at a rate which is a function of stem [N]
   // dltStemNconc per day (17dd) = 0.076*stemNconcPct - 0.0199
   // cannot take below Structural stem [N]% 0.5

   float stemNConcPct = divide((nGreen + dltNGreen),(dmGreen + dltDmGreen)) * 100;
   if(stemNConcPct < structNFn.value(stage) * 100)return 0;
   
   float dltStemNconc = divide(17,plant->phenology->getDltTT())
                                          * (0.076 * (stemNConcPct) - 0.0199);
   float availableN = (dltStemNconc) / 100 * (dmGreen + dltDmGreen);
   // cannot take below structural N
   float structN =  (dmGreen + dltDmGreen) * structNFn.value(stage);
   availableN = Min(availableN,(nGreen + dltNGreen) - structN);
  
   availableN = Max(availableN,0.0);
   
   float nProvided = Min(availableN,requiredN);
   dltNRetranslocate -= nProvided;
   return nProvided;
   }
//------------------------------------------------------------------------------------------------
float Stem::dmRetransAvailable(void)
   {
   // calculate dry matter available for translocation to grain
   float stemWt = dmGreen + dltDmGreen;
   float stemWtAvail = stemWt - dmPlantMin * density;
   return Max(stemWtAvail,0.0);
   }
//------------------------------------------------------------------------------------------------
float Stem::calcPDemand(void)
   {
   // Leaf P demand

   float rel_growth_rate = divide(plant->biomass->getDltDMPotRUE(),
         plant->biomass->getAboveGroundBiomass(),0.0);

   float deficit = pConcMax() * dmGreen * (1.0 + rel_growth_rate) - pGreen;

   pDemand = Max(deficit,0.0);
   return pDemand;
   }
//------------------------------------------------------------------------------------------------

