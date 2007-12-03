#pragma hdrstop
#include <vector>

#include "OOPlant.h"
#include "OOPlantComponents.h"
#include "OOPhosphorus.h"

using namespace std;
//------------------------------------------------------------------------------------------------
//------ Phosphorus Constructor
//------------------------------------------------------------------------------------------------
Phosphorus::Phosphorus(ScienceAPI &api, OOPlant *p) : PlantProcess(api)
   {
   plant = p;

   StressParts.push_back(plant->leaf);
   StressParts.push_back(plant->stem);
   StressParts.push_back(plant->grain);

   initialize();
   doRegistrations();

   }
//------------------------------------------------------------------------------------------------
//------ Phosphorus Destructor
//------------------------------------------------------------------------------------------------
Phosphorus::~Phosphorus()
   {
   }
//--------------------------------------------------------------------------------------------------
// Register variables for other modules
//--------------------------------------------------------------------------------------------------
void Phosphorus::doRegistrations(void)
   {
   scienceAPI.expose("pfact_pheno", "", "Phosphorus stress factor for phenology", 0, phenoStress);
   scienceAPI.expose("pfact_expansion", "", "Phosphorus stress factor for leaf expansion", 0, expansionStress);
   scienceAPI.expose("pfact_photo", "", "Phosphorus stress factor for photosynthesis", 0, photoStress);
   scienceAPI.expose("pfact_grain", "", "Phosphorus stress factor for grain", 0, grainStress);
   scienceAPI.expose("p_total_uptake", "g/m2", "Today's P uptake", 0, pUptakeTotal);

   scienceAPI.exposeFunction("GreenP", "g/m2", "P content of live plant parts", 
                     FloatFunction(&Phosphorus::getPGreen));
   scienceAPI.exposeFunction("SenescedP","g/m2", "P content of senesced plant parts",
                     FloatFunction(&Phosphorus::getPSenesced));
   scienceAPI.exposeFunction("p_sen",  "g/m2", "P content of senesced plant parts",
                     FloatFunction(&Phosphorus::getPSenesced));
   scienceAPI.exposeFunction("p_dead", "g/m2", "P content of dead plant parts",    
                     FloatFunction(&Phosphorus::getPDead));

   scienceAPI.exposeFunction("p_demand", "kg/ha", "P demand of plant parts",
                     FloatArrayFunction(&Phosphorus::getPDemand));

   scienceAPI.exposeFunction("dlt_p_green", "g/m2", "Daily P increase in live plant parts",
                     FloatArrayFunction(&Phosphorus::getDltPGreen));
   scienceAPI.exposeFunction("dlt_p_retrans", "g/m2", "P retranslocated from plant parts to grain",
                     FloatArrayFunction(&Phosphorus::getDltPRetrans));
   scienceAPI.exposeFunction("dlt_p_detached", "g/m2", "Actual P loss with detached plant",
                     FloatArrayFunction(&Phosphorus::getDltPDetached));
   scienceAPI.exposeFunction("dlt_p_dead", "g/m2", "Actual P loss with dead plant",
                     FloatArrayFunction(&Phosphorus::getDltPDead));
   scienceAPI.exposeFunction("dlt_n_dead_detached", "g/m2", "Actual N loss with detached dead plant",
                     FloatArrayFunction(&Phosphorus::getDltPDeadDetached));



   }
//------------------------------------------------------------------------------------------------
//------- Initialize variables
//------------------------------------------------------------------------------------------------
void Phosphorus::initialize(void)
   {
   active = false;

   phenoStress     = 1.0;
   expansionStress = 1.0;
   photoStress     = 1.0;
   grainStress     = 1.0;

   totalDemand = 0.0;
   
   pBiomass = 0.0;
   pStover = 0.0;
   pGreenBiomass = 0.0;
   pUptakeTotal = 0.0;
   pPlant = 0.0;

   currentLayer = 0.0;
   //Set up reporting vectors
   pGreen.clear();
   dltPGreen.clear();
   dltPRetrans.clear();
   pSenesced.clear();
   pDead.clear();
   dltPDetached.clear();
   dltPDead.clear();
   dltPDetachedDead.clear();
   pDemand.clear();

//   dltP.clear();

   for(unsigned i = 0; i < plant->PlantParts.size(); i++)
      {
      pGreen.push_back(0.0);
      dltPGreen.push_back(0.0);
      dltPRetrans.push_back(0.0);
      pSenesced.push_back(0.0);
      pDead.push_back(0.0);
      dltPDetached.push_back(0.0);
      dltPDead.push_back(0.0);
      dltPDetachedDead.push_back(0.0);
      pDemand.push_back(0.0);
      }

   }
//------------------------------------------------------------------------------------------------
//----------- read Phosphorus parameters
//------------------------------------------------------------------------------------------------
void Phosphorus::readParams (string cultivar)
   {
   std::vector<float> values;
   if (scienceAPI.get("labile_p", "", 1, values, 0.0, 10000.0))
      {
      active = true;
      }
   else return;

   scienceAPI.read("pfact_pheno_slope"    ,"", 0, phenoSlope);
   scienceAPI.read("pfact_photo_slope"    ,"", 0, photoSlope);
   scienceAPI.read("pfact_expansion_slope","", 0, expansionSlope);
   scienceAPI.read("pfact_grain_slope"    ,"", 0, grainSlope);

   }
//------------------------------------------------------------------------------------------------
//-------- Get Phosphorus variables from other modules
//------------------------------------------------------------------------------------------------
void Phosphorus::getOtherVariables (void)
   {
   stage = plant->phenology->currentStage();
   }
//------------------------------------------------------------------------------------------------
//-------- Set Phosphorus variables in other modules
//------------------------------------------------------------------------------------------------
void Phosphorus::setOtherVariables (void)
   {
/*
   std::vector<float> dltPValues;
   for(int i=0;i < nLayers;i++)dltPValues.push_back(0.0);

   for(unsigned i=0;i < dltP.size();i++)
      {
      dltNo3Values[i] = dltNo3[i] * gm2kg /sm2ha;
      }
   plantInterface->setVariable(dltNo3ID, dltNo3Values);
        */
   }
//------------------------------------------------------------------------------------------------
//----------- perform daily phosphorus dynamics  ---------------------------------------------------
void Phosphorus::prepare(void)
   {
   getOtherVariables ();
   demand();
   calcStress();
   }
//------------------------------------------------------------------------------------------------
void Phosphorus::process(void)
   {
//   supply();
   uptake();
   partition();
   senescence();
   detachment();
   retranslocate();
   updateP();
   }
//------------------------------------------------------------------------------------------------
void Phosphorus::calcStress(void)
   {
   float pfact = pStress();          // generic p stress factor

   phenoStress = bound(pfact * phenoSlope,0.0,1.0);
   photoStress = bound(pfact * photoSlope,0.0,1.0);
   expansionStress = bound(pfact * expansionSlope,0.0,1.0);
   grainStress = bound(pfact * grainSlope,0.0,1.0);
   }
//------------------------------------------------------------------------------------------------
float Phosphorus::pStress(void)
   {
   // calculate a generic p stress factor by getting the max and min concentrations
   // and calculating where the actual concentration is
   float wt = 0.0, max = 0.0, min = 0.0, act = 0.0;
   float stress;
   for(unsigned i=0;i < StressParts.size();i++)
      {
      float partWt = StressParts[i]->getDmGreen();
      wt += partWt;
      max += StressParts[i]->pConcMax() * partWt;
      min += StressParts[i]->pConcMin() * partWt;
      act += StressParts[i]->getPGreen();
      }

   float actConc = divide(act, wt, 1.0);
   float maxConc = divide(max, wt, 1.0);
   float minConc = divide(min, wt, 1.0);

   if ((wt < 1.0e-5) || (act < 1.0e-5)) stress = 1.0;
   else stress = divide(actConc - minConc, maxConc - minConc, 1.0);

   return bound(stress,0.0,1.0);
   }
//------------------------------------------------------------------------------------------------
//----------- update phosphorus state variables at the end of the day
//------------------------------------------------------------------------------------------------
void Phosphorus::updateVars(void)
   {
   if(!active)return;
   for(unsigned i=0;i < plant->PlantParts.size();i++)
      {
      pGreen[i] = plant->PlantParts[i]->getPGreen();
      pDemand[i] = plant->PlantParts[i]->getPDemand() * 10;
      pSenesced[i] = plant->PlantParts[i]->getPSenesced();
      dltPGreen[i] = plant->PlantParts[i]->getDltPGreen();
      dltPRetrans[i] = plant->PlantParts[i]->getDltPRetrans();
      pDead[i] = plant->PlantParts[i]->getPDead();
//      dltPDead[i] = plant->PlantParts[i]->getDltPDead();
//      dltPDetached[i] = plant->PlantParts[i]->getDltPDetached();
//      dltPDetachedDead[i] = plant->PlantParts[i]->getDltPDetachedDead();

      }


   pPlant = sumVector(pGreen) + sumVector(pSenesced);
   pGreenBiomass = sumVector(pGreen) - plant->roots->getPGreen();
   pBiomass = pGreenBiomass + sumVector(pSenesced) - plant->roots->getPSenesced();
   pStover = pBiomass - plant->grain->getPGreen() - plant->grain->getPSenesced();

   }
//------------------------------------------------------------------------------------------------
//------- calculate phosphorus supply potential from mass flow diffusion and fixation
//------------------------------------------------------------------------------------------------
void Phosphorus::supply(void)
   {
   }
//------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------
//------- calculate phosphorus demand in each plant part
//------------------------------------------------------------------------------------------------
void Phosphorus::demand(void)
   {
   totalDemand = 0;
   for(unsigned i=0;i < plant->PlantParts.size();i++)
      {
      pDemand[i] = plant->PlantParts[i]->calcPDemand() * 10;
      }
   totalDemand = sumVector(pDemand);
   }
//------------------------------------------------------------------------------------------------
//------- calculate phosphorus uptake
//------------------------------------------------------------------------------------------------
//     Get total P uptake
void Phosphorus::uptake(void)
   {
   vector<float> layeredUptake;

   if (!scienceAPI.get("uptake_p_sorghum", "", 1, layeredUptake, 0.0f, 10000.0f))
      {
      // we have no P uptake - set to demand
      pUptakeTotal = totalDemand * kg2gm/ha2sm;
      }
   else
      pUptakeTotal = sumVector(layeredUptake);
   }
//------------------------------------------------------------------------------------------------
//------- partition Phosphorus
//------------------------------------------------------------------------------------------------
//     allocate N to each plant part
void Phosphorus::partition(void)
   {
   for(unsigned i=0;i < plant->PlantParts.size();i++)
      {
      plant->PlantParts[i]->partitionP(pUptakeTotal * divide(plant->PlantParts[i]->getPDemand(),
               totalDemand,0.0));
      }
   }
//------------------------------------------------------------------------------------------------
void Phosphorus::senescence(void)
   {
   for(unsigned i=0;i < plant->PlantParts.size();i++)
      {
      plant->PlantParts[i]->calcDltPSenesced();
      }
   }
//------------------------------------------------------------------------------------------------
void Phosphorus::detachment(void)
   {
   for(unsigned i=0;i < plant->PlantParts.size();i++)
      {
      plant->PlantParts[i]->calcDltPDetached();
      }
   }
//------------------------------------------------------------------------------------------------
void Phosphorus::updateP(void)
   {
   for(unsigned i=0;i < plant->PlantParts.size();i++)
      {
      plant->PlantParts[i]->updateP();
      }
   }
//------------------------------------------------------------------------------------------------
//------- partition Phosphorus
//------------------------------------------------------------------------------------------------
void Phosphorus::retranslocate(void)
   {
   // dummy code to get this done - will change
   string plantParts = "ssssd";
   vector<float> supply;
   vector<float> demand;

   for( unsigned i=0;i < plantParts.size();i++)
      {
      if(plantParts[i] == 's')
         {
         float minP = plant->PlantParts[i]->pConcMin() * plant->PlantParts[i]->getDmGreen();
         supply.push_back(Max(plant->PlantParts[i]->getPGreen() - minP,0.0));
         demand.push_back(0.0);
         }
      else
         {
         supply.push_back(0.0);
         float maxP = plant->PlantParts[i]->pConcMax() * plant->PlantParts[i]->getDmGreen();
         demand.push_back(Max(maxP - plant->PlantParts[i]->getPGreen(),0.0));
         }
      }
   // retranslocate

   for(unsigned i=0;i < plant->PlantParts.size();i++)
      if(plantParts[i] == 's')
         {
         float fraction = bound(divide(sumVector(demand),sumVector(supply),0.0),0.0,1.0);
         plant->PlantParts[i]->setPRetrans(-supply[i]*fraction);
         }
      else
         {
         float fraction = bound(divide(sumVector(supply),sumVector(demand),0.0),0.0,1.0);
         plant->PlantParts[i]->setPRetrans(demand[i]*fraction);
         }
   }

//------------------------------------------------------------------------------------------------
//------- Calculate plant Phosphorus detachment from senesced and dead pools
//------------------------------------------------------------------------------------------------
void Phosphorus::detachment(vector<float> senDetachFrac, vector<float> deadDetachFrac)
   {
   for(unsigned i = 0; i < plant->PlantParts.size(); i++)
      {
//      plant->PlantParts[i]->NDetachment(senDetachFrac, deadDetachFrac);
      }
   }
//------------------------------------------------------------------------------------------------
float Phosphorus::layerProportion(void)
   {
   // calculates the proportion of the current root layer that is populated by roots
   float layerTop    = sumVector(dLayer, currentLayer);
   float layerBottom = sumVector(dLayer, currentLayer+1);

   return Min(divide(rootDepth - layerTop,layerBottom - layerTop),1.0);
   }
//------------------------------------------------------------------------------------------------
void Phosphorus::getPGreen(float &result)
   {
   result = sumVector(pGreen);
   }
//------------------------------------------------------------------------------------------------
void Phosphorus::getPSenesced(float &result)
   {
   result = sumVector(pSenesced);
   }
//------------------------------------------------------------------------------------------------
void Phosphorus::getPDemand(vector<float> &result)
   {
   result = pDemand;
   }
//------------------------------------------------------------------------------------------------
void Phosphorus::getDltPGreen(vector<float> &result)
   {
   result = dltPGreen;
   }
//------------------------------------------------------------------------------------------------
void Phosphorus::getDltPRetrans(vector<float> &result)
   {
   result = dltPRetrans;
   }
//------------------------------------------------------------------------------------------------
void Phosphorus::getDltPDetached(vector<float> &result)
   {
   result = dltPDetached;
   }
//------------------------------------------------------------------------------------------------
void Phosphorus::getDltPDead(vector<float> &result)
   {
   result= dltPDead;
   }
//------------------------------------------------------------------------------------------------
void Phosphorus::getDltPDeadDetached(vector<float> &result)
   {
   result = dltPDetachedDead;
   }
//------------------------------------------------------------------------------------------------
void Phosphorus::getPDead(float &result)
   {
   result = sumVector(pDead);
   }
//------------------------------------------------------------------------------------------------
void Phosphorus::Summary(void)
   {
   char msg[120];
   sprintf(msg, "grain P percent            =  %8.3f \t grain P uptake     (kg/ha) = %8.3f\n",
            plant->grain->getPConc() * 100,plant->grain->getPGreen() * 10.0);
   scienceAPI.write(msg);
   sprintf(msg, "total P content    (kg/ha) =  %8.3f \t senesced P content (kg/ha) = %8.3f\n",
            pBiomass * 10.0,sumVector(pSenesced) * 10.0);
   scienceAPI.write(msg);
   sprintf(msg, "green P content    (kg/ha) =  %8.3f \t dead P content     (kg/ha) = %8.3f\n",
            sumVector(pGreen) * 10.0 - plant->grain->getPGreen() * 10.0, sumVector(pDead) * 10.0);
   scienceAPI.write(msg);
   }
//------------------------------------------------------------------------------------------------
//------- React to a newProfile message
//------------------------------------------------------------------------------------------------
void Phosphorus::onNewProfile(NewProfileType &p /* message */)
   {
   /*
   protocol::ApsimVariant av(plantInterface);
   av.aliasTo(v.getMessageData());

   protocol::vector<float> temp;
   av.get("dlayer",   protocol::DTsingle, true, temp);
   convertVector(temp,dLayer);

   // dlayer may be changed from its last setting due to erosion
   profileDepth = sumVector(dLayer);      // depth of soil profile (mm)
   nLayers = dLayer.size();
*/
   /* TODO : Insert new root profile and llDep code for change in profile due to erosion */
   /* TODO : Check validity of ll,dul etc as in crop_check_sw */
   /* TODO : why does this not include no3 */
   }
//------------------------------------------------------------------------------------------------

