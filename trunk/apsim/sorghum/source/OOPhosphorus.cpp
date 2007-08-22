//------------------------------------------------------------------------------------------------
#pragma hdrstop

#include "TypeKind.h"
#include "OOPlant.h"
#include "OOPhosphorus.h"
#include <vector>
#include <ComponentInterface/datatypes.h>
//------------------------------------------------------------------------------------------------

#pragma package(smart_init)
//------------------------------------------------------------------------------------------------
//------ Phosphorus Constructor
//------------------------------------------------------------------------------------------------
Phosphorus::Phosphorus(OOPlant *p)
   {
   plant = p;
   plantInterface = plant->plantInterface;

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
#define setupGetVar plantInterface->addGettableVar
//   setupGetVar("p_demand", totalDemand, "g/m2", "Today's total crop P demand");

   setupGetVar("pfact_pheno", phenoStress, "", "Phosphorus stress factor for phenology");
//   setupGetVar("pfact_expan", expansionStress, "", "Phosphorus stress factor for leaf expansion");
   setupGetVar("pfact_expansion", expansionStress, "", "Phosphorus stress factor for leaf expansion");
   setupGetVar("pfact_photo", photoStress, "", "Phosphorus stress factor for photosynthesis");
   setupGetVar("pfact_grain", grainStress, "", "Phosphorus stress factor for grain");
   setupGetVar("p_total_uptake", pUptakeTotal, "g/m2", "Today's P uptake");

/*   setupGetVar("n_sd_ratio", supplyDemandRatio, "", "Phosphorus supply/demand ratio");
   setupGetVar("n_supply_soil", nSupply, "g/m2", "Today's total N supply from soil profile");
   setupGetVar("n_massflow_uptake", actualMassFlow, "g/m2", "Today's N uptake by massflow from soil profile");
   setupGetVar("n_diffusion_uptake", actualDiffusion, "g/m2", "Today's N uptake by diffusion from soil profile");
   setupGetVar("diffusion_supply_tot", sumDiffSupply, "g/m2", "Accumulative total of crop N supply by diffusion");
   setupGetVar("biomass_n", nBiomass, "g/m2", "N above ground biomass including grain");
   setupGetVar("stover_n", nStover, "g/m2", "N above ground biomass excluding grain");
   setupGetVar("green_biomass_n", nGreenBiomass, "g/m2", "N in live above ground biomass including grain");
   setupGetVar("n_cum_uptake", nUptakeTotal, "g/m2", "Phosphorus stress factor for photosynthesis");
   setupGetVar("n_Plant", nPlant, "g/m2", "Total Phosphorus in the plant including roots");      */

#undef setupGetVar

   setupGetFunction(plantInterface,"p_green", protocol::DTsingle, false,
                    &Phosphorus::getPGreen, "g/m2", "P content of live plant parts");
   setupGetFunction(plantInterface,"p_senesced", protocol::DTsingle, false,
                    &Phosphorus::getPSenesced, "g/m2", "P content of senesced plant parts");
   setupGetFunction(plantInterface,"p_sen", protocol::DTsingle, false,
                    &Phosphorus::getPSenesced, "g/m2", "P content of senesced plant parts");
   setupGetFunction(plantInterface,"p_dead", protocol::DTsingle, false,
                    &Phosphorus::getPDead, "g/m2", "P content of dead plant parts");

   setupGetFunction(plantInterface,"p_demand", protocol::DTsingle, true,
                    &Phosphorus::getPDemand, "kg/ha", "P demand of plant parts");

   setupGetFunction(plantInterface, "dlt_p_green", protocol::DTsingle, true,
                    &Phosphorus::getDltPGreen, "g/m2", "Daily P increase in live plant parts");
   setupGetFunction(plantInterface, "dlt_p_retrans", protocol::DTsingle, true,
                    &Phosphorus::getDltPRetrans, "g/m2", "P retranslocated from plant parts to grain");
   setupGetFunction(plantInterface, "dlt_p_detached", protocol::DTsingle, true,
                    &Phosphorus::getDltPDetached, "g/m2", "Actual P loss with detached plant");
   setupGetFunction(plantInterface, "dlt_p_dead", protocol::DTsingle, true,
                    &Phosphorus::getDltPDead, "g/m2", "Actual P loss with dead plant");
   setupGetFunction(plantInterface, "dlt_n_dead_detached", protocol::DTsingle, true,
                    &Phosphorus::getDltPDeadDetached, "g/m2", "Actual N loss with detached dead plant");



   labileID    = plantInterface->addRegistration(RegistrationType::get,"labile_p", floatArrayType,"", "");
   uptakeID = plantInterface->addRegistration(RegistrationType::get,"uptake_p_sorghum", floatArrayType,"", "");
/*
   no3MinID = plantInterface->addRegistration(RegistrationType::get,"no3_min", floatArrayType,"", "");
   dltNo3ID = plantInterface->addRegistration(RegistrationType::set,"dlt_no3", floatArrayType,"", "");  */
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
   if (plantInterface->getVariable(labileID, values, 0.0, 10000.0, true))
      {
      active = true;
      }
   else return;
   std::vector<string> sections;                  // sections to look for parameters
   sections.push_back("constants");
   sections.push_back(cultivar);

   phenoSlope = readVar(plantInterface,sections,"pfact_pheno_slope");
   photoSlope = readVar(plantInterface,sections,"pfact_photo_slope");
   expansionSlope = readVar(plantInterface,sections,"pfact_expansion_slope");
   grainSlope = readVar(plantInterface,sections,"pfact_grain_slope");

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

   float dumP = sumVector(pDemand);
   
   if (!plantInterface->getVariable(uptakeID, layeredUptake, 0.0, 10000.0, true))
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
void Phosphorus::getPGreen(protocol::Component *system, protocol::QueryValueData &qd)
   {
   system->sendVariable(qd, sumVector(pGreen));
   }
//------------------------------------------------------------------------------------------------
void Phosphorus::getPSenesced(protocol::Component *system, protocol::QueryValueData &qd)
   {
   system->sendVariable(qd, sumVector(pSenesced));
   }
//------------------------------------------------------------------------------------------------
void Phosphorus::getPDemand(protocol::Component *system, protocol::QueryValueData &qd)
   {
   system->sendVariable(qd, protocol::vector<float>(&pDemand[0], &pDemand[0] + pDemand.size()));
   }
//------------------------------------------------------------------------------------------------
void Phosphorus::getDltPGreen(protocol::Component *system, protocol::QueryValueData &qd)
   {
   system->sendVariable(qd, protocol::vector<float>(&dltPGreen[0], &dltPGreen[0] + dltPGreen.size()));
   }
//------------------------------------------------------------------------------------------------
void Phosphorus::getDltPRetrans(protocol::Component *system, protocol::QueryValueData &qd)
   {
   system->sendVariable(qd, protocol::vector<float>(&dltPRetrans[0], &dltPRetrans[0] + dltPRetrans.size()));
   }
//------------------------------------------------------------------------------------------------
void Phosphorus::getDltPDetached(protocol::Component *system, protocol::QueryValueData &qd)
   {
   system->sendVariable(qd, protocol::vector<float>(&dltPDetached[0], &dltPDetached[0] + dltPDetached.size()));
   }
//------------------------------------------------------------------------------------------------
void Phosphorus::getDltPDead(protocol::Component *system, protocol::QueryValueData &qd)
   {
   system->sendVariable(qd, protocol::vector<float>(&dltPDead[0], &dltPDead[0] + dltPDead.size()));
   }
//------------------------------------------------------------------------------------------------
void Phosphorus::getDltPDeadDetached(protocol::Component *system, protocol::QueryValueData &qd)
   {
   system->sendVariable(qd, protocol::vector<float>(&dltPDetachedDead[0], &dltPDetachedDead[0] + dltPDetachedDead.size()));
   }
//------------------------------------------------------------------------------------------------
void Phosphorus::getPDead(protocol::Component *system, protocol::QueryValueData &qd)
   {
   system->sendVariable(qd, sumVector(pDead));
   }
//------------------------------------------------------------------------------------------------
void Phosphorus::Summary(void)
   {
   float f = sumVector(pGreen);
   summaryLine(plantInterface,"grain P percent            =  %8.3f \t grain P uptake     (kg/ha) = %8.3f",
            plant->grain->getPConc() * 100,plant->grain->getPGreen() * 10.0);
   summaryLine(plantInterface,"total P content    (kg/ha) =  %8.3f \t senesced P content (kg/ha) = %8.3f",
            pBiomass * 10.0,sumVector(pSenesced) * 10.0);
   summaryLine(plantInterface,"green P content    (kg/ha) =  %8.3f \t dead P content     (kg/ha) = %8.3f",
            sumVector(pGreen) * 10.0 - plant->grain->getPGreen() * 10.0, sumVector(pDead) * 10.0);
   }
//------------------------------------------------------------------------------------------------
//------- React to a newProfile message
//------------------------------------------------------------------------------------------------
void Phosphorus::doNewProfile(protocol::Variant &v /* message */)
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

