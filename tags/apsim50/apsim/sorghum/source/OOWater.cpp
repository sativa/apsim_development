//------------------------------------------------------------------------------------------------
#pragma hdrstop

#include "OOPlant.h"
#include "TypeKind.h"
#include "OOWater.h"
#include <vector>
#include <ComponentInterface/dataTypes.h>
//------------------------------------------------------------------------------------------------

#pragma package(smart_init)

//------------------------------------------------------------------------------------------------
//------ Water Constructor
//------------------------------------------------------------------------------------------------
Water::Water(OOPlant *p)
   {
   plant = p;
   plantInterface = p->plantInterface;

   //Init Accumulation Vars
    for(int i=0;i < nStages;i++)
      {
      phenoStressTotal.push_back(0.0);
      photoStressTotal.push_back(0.0);
       }
   doRegistrations();
   initialize();
   }
//------------------------------------------------------------------------------------------------
//------ Water Destructor
//------------------------------------------------------------------------------------------------
Water::~Water()
   {
   }
//--------------------------------------------------------------------------------------------------
// Register variables for other modules
//--------------------------------------------------------------------------------------------------
void Water::doRegistrations(void)
   {
#define setupGetVar plantInterface->addGettableVar
   setupGetVar("sw_supply_demand_ratio", sdRatio, "", "Water Supply/demand ratio");
   setupGetVar("sw_supply_sum", AccTotalSupply, "mm", "Accumulative soil water supply over the profile");
   setupGetVar("sw_supply", totalSupply, "mm", "Daily soil water supply over the profile");
   setupGetVar("sw_demand", swDemand, "mm", "Total crop demand for water");
   setupGetVar("transpiration", dltUptake, "mm", "Daily water uptake from all rooted soil layers");
   setupGetVar("transpiration_tot", totalUptake, "mm", "Accumulative water uptake from the whole profile");
   setupGetVar("cep", totalUptake, "mm", "Accumulative water uptake from the whole profile");
   setupGetVar("swdef_photo", photoStress, "", "Water stress factor for photosynthesis");
   setupGetVar("swdef_pheno", phenoStress, "", "Water stress factor for phenology");
   setupGetVar("swdef_expan", expansionStress, "", "Water stress factor for leaf expansion growth");
   setupGetVar("esw_profile", eswTot, "", "Plant extractable water over the whole profile");
   setupGetVar("ep", ep, "", "Water uptake from the whole profile");

#undef setupGetVar

   swDepID       = plantInterface->addRegistration(RegistrationType::get,"sw_dep", floatArrayType,"", "");
   dltSwDepID    = plantInterface->addRegistration(RegistrationType::set,
                                                           "dlt_sw_dep", floatArrayType,"", "");
   setupGetFunction(plantInterface,"esw_layer", protocol::DTsingle, true,
                    &Water::getEswLayers, "mm", "Plant extractable soil water in each layer");
   setupGetFunction(plantInterface,"sw_deficit", protocol::DTsingle, true,
                    &Water::getSwDefLayers, "mm", "Soil water deficit below dul (dul - sw)");
   setupGetFunction(plantInterface,"sw_uptake", protocol::DTsingle, true,
                    &Water::getSwUptakeLayers, "mm", "Daily water uptake in each different rooted layers");



   }
//------------------------------------------------------------------------------------------------
//------- Initialize variables
//------------------------------------------------------------------------------------------------
void Water::initialize(void)
   {
   photoStress = 1;phenoStress = 1;expansionStress = 1;
   currentLayer = 0;
   lastLayerPropn = 0;
   sdRatio = 1;
   rootDepth = 0;
   totalUptake = 0.0;
   AccTotalSupply = 0.0;
   ep = 0.0;
   swDemand = 0.0;
   }
//------------------------------------------------------------------------------------------------
//------ read Water parameters
//------------------------------------------------------------------------------------------------
void Water::readParams (string cultivar)
   {
   vector<string> sections;                  // sections to look for parameters
   sections.push_back("constants");
   sections.push_back("parameters");
   swPhenoTable.read(plantInterface,sections,"x_sw_avail_ratio","y_swdef_pheno");
   swExpansionTable.read(plantInterface,sections,"x_sw_demand_ratio","y_swdef_leaf");

   readArray (plantInterface,sections,"ll",ll);
   llDep.clear();
   eswCap.clear();
   for(int layer = 0; layer < nLayers; layer++)
      {
      llDep.push_back(ll[layer]*dLayer[layer]);
      eswCap.push_back(dulDep[layer] - llDep[layer]);
      }

   readArray (plantInterface,sections, "kl", kl);
   readArray (plantInterface,sections, "xf", xf);

    // report
   plantInterface->writeString ("");
   plantInterface->writeString ("");
   plantInterface->writeString ("                       Root Profile");
   plantInterface->writeString ("    ---------------------------------------------------");
   plantInterface->writeString ("         Layer       Kl           Lower    Exploration");
   plantInterface->writeString ("         Depth     Factor         Limit      Factor  ");
   plantInterface->writeString ("         (mm)         ()        (mm/mm)       (0-1)");
   plantInterface->writeString ("    ---------------------------------------------------");

   char msg[100];
   for (int layer = 0; layer < nLayers; layer++)
      {
      sprintf (msg, "    %9.1f%10.3f%15.3f%12.3f", dLayer[layer],kl[layer],
                                              ll[layer],xf[layer]);
      plantInterface->writeString (msg);
      }
   plantInterface->writeString ("    ---------------------------------------------------\n");

   }
//------------------------------------------------------------------------------------------------
//------ update Water parameters
//------------------------------------------------------------------------------------------------
void Water::updateVars(void)
   {
   sdRatio = Min(divide(totalSupply, swDemand),float(1.0));
   rootDepth = plant->roots->getRootDepth();
   currentLayer = findIndex(rootDepth, dLayer);
   setOtherVariables ();
   //swDef.clear();
   ep = -sumVector(dltSwDep);
   for(int i = 0; i < nLayers; i++)
      {
      swUptake[i] = -dltSwDep[i];
      swDef[i] = dulDep[i] - swDep[i];
      dltSwDep[i] = 0.0;
      }
   eswTot = sumVector(esw);

   }
//------------------------------------------------------------------------------------------------
//-------- Get Water variables from other modules
//------------------------------------------------------------------------------------------------
void Water::getOtherVariables (void)
   {
  // get sw from Soilwat2
   std::vector<float> temp;
   plantInterface->getVariable(swDepID, temp, 0.0, 1000.0);
//   convertVector(temp,swDep);
   fillVector(temp,swDep);

//   esw.clear();
   for (int i = 0; i < nLayers; i++)
      {
      esw[i] = swDep[i] - llDep[i];
      }      
   }
//------------------------------------------------------------------------------------------------
//-------- Set Water variables in other modules
//------------------------------------------------------------------------------------------------
void Water::setOtherVariables (void)
   {
   std::vector<float> dltSWDepValues;
   for(unsigned i=0;i < dltSwDep.size();i++)
      {
      dltSWDepValues.push_back(dltSwDep[i]);
      }
   plantInterface->setVariable(dltSwDepID, dltSWDepValues);
   }
//------------------------------------------------------------------------------------------------
//------- React to a newProfile message
//------------------------------------------------------------------------------------------------
void Water::doNewProfile(protocol::Variant &v /* message */)
   {
   protocol::ApsimVariant av(plantInterface);
   av.aliasTo(v.getMessageData());

   protocol::vector<float> temp;
/* TODO : Problem here summing the protocol::vector - do we need it? */
   av.get("dlayer",   protocol::DTsingle, true, temp);
   convertVector(temp,dLayer);
   av.get("ll15_dep", protocol::DTsingle, true, temp);
   convertVector(temp,ll15Dep);
   av.get("dul_dep",  protocol::DTsingle, true, temp);
   convertVector(temp,dulDep);
   av.get("sat_dep",  protocol::DTsingle, true, temp);
   convertVector(temp,satDep);
   av.get("sw_dep",   protocol::DTsingle, true, temp);
   convertVector(temp,swDep);
   av.get("bd",       protocol::DTsingle, true, temp);
   convertVector(temp,bd);


   // dlayer may be changed from its last setting due to erosion
   profileDepth = sumVector(dLayer);      // depth of soil profile (mm)
   nLayers = dLayer.size();

   //Reset the working vectors
   available.clear();
   availablePot.clear();
   supply.clear();
   swUptake.clear();
   swDep.clear();
   esw.clear();
   swDef.clear();
   dltSwDep.clear();
   for(int i = 0; i < nLayers; i++)
      {
      available.push_back(0.0);
      availablePot.push_back(0.0);
      supply.push_back(0.0);
      swUptake.push_back(0.0);
      swDep.push_back(0.0);
      esw.push_back(0.0);
      swDef.push_back(0.0);
      dltSwDep.push_back(0.0);
      }

   /* TODO : Insert new root profile and llDep code for change in profile due to erosion */
   /* TODO : Check validity of ll,dul etc as in crop_check_sw */
   /* TODO : why does this not include no3 */
   }


//------------------------------------------------------------------------------------------------
void Water::process(void)
   {
   getOtherVariables ();
   calcDailySupply();
   calcStresses();
   calcUptake();
   }
//------------------------------------------------------------------------------------------------
float Water::swAvailRatio(int currentLayer)
   {
   return  divide (esw[currentLayer],eswCap[currentLayer], 10.0);
   }
//------------------------------------------------------------------------------------------------
//--------------- Plant transpiration and soil water extraction
//-----------     Calculate daily water demand - called from plant->prepare
//------------------------------------------------------------------------------------------------
float Water::calcDemand(void)
   {
   swDemand = divide (plant->biomass->getDltDMPotRUE(),plant->getTranspEff());
   return swDemand;
   }
//------------------------------------------------------------------------------------------------
//-----------     Calculate daily water supply - called from plant->process
//------------------------------------------------------------------------------------------------
void Water::calcDailySupply(void)
   {
   calcAvailable();
   calcAvailablePot();
   calcSupply();
   }
//------------------------------------------------------------------------------------------------
//-----------  calculate daily water stresses - called from plant->process
//------------------------------------------------------------------------------------------------
void Water::calcStresses(void)
   {
   photoStress = calcSwDefPhoto();
   phenoStress = calcSwDefPheno();
   expansionStress = calcSwDefExpansion();
   accumulate(photoStress, photoStressTotal, plant->phenology->currentStage(), plant->phenology->getDltStage());
   accumulate(phenoStress, phenoStressTotal, plant->phenology->currentStage(), plant->phenology->getDltStage());

   }
//------------------------------------------------------------------------------------------------
void Water::calcAvailable(void)
   {
   for(int layer = 0;layer <= currentLayer;layer++)
      {
      available[layer] = Max(swDep[layer] - llDep[layer],0.0);
      }
   available[currentLayer] *= layerProportion();
   totalAvail = sumVector(available);
   }
//------------------------------------------------------------------------------------------------
float Water::layerProportion(void)
   {
   // calculates the proportion of the current root layer that is populated by roots
   float layerTop    = sumVector(dLayer, currentLayer);
   float layerBottom = sumVector(dLayer, currentLayer+1);

   return Min(divide(rootDepth - layerTop,layerBottom - layerTop),1.0);
   }
//------------------------------------------------------------------------------------------------
void Water::calcAvailablePot(void)
   {
   for(int layer = 0;layer <= currentLayer;layer++)
      {
      availablePot[layer] = Max(dulDep[layer] - llDep[layer],0.0);
      }
   availablePot[currentLayer] *= layerProportion();
   totalAvailPot = sumVector(availablePot);
   }
//------------------------------------------------------------------------------------------------
void Water::calcSupply(void)
   {
   /*       Return potential water uptake from each layer of the soil profile
          by the crop (mm water). Row Spacing and configuration (skip) are used
           to calculate semicircular root front to give proportion of the
           layer occupied by the roots. This fraction is applied to the supply */

   for(int layer = 0;layer <= currentLayer;layer++)
      {
      float prop = plant->roots->RootProportionInLayer(layer);
      supply[layer] = Max(available[layer] * kl[layer] *  prop,0.0);
      }
   totalSupply = sumVector(supply);
   AccTotalSupply += totalSupply;
   }
//------------------------------------------------------------------------------------------------
//-------- Calculate the daily crop water stresses
//------------------------------------------------------------------------------------------------
float Water::calcSwDefPhoto(void)
   {
   return bound(divide(totalSupply,swDemand,1.0),0.0,1.0);
   }
//------------------------------------------------------------------------------------------------
float Water::calcSwDefPheno(void)
   {
   float swAvailRatio = divide(totalAvail,totalAvailPot,1.0);
   swAvailRatio = bound(swAvailRatio,0.0,1.0);
   return swPhenoTable.value(swAvailRatio);
   }
//------------------------------------------------------------------------------------------------
float Water::calcSwDefExpansion(void)
  {
  float sdRatio = divide(totalSupply,swDemand,10.0);
  return swExpansionTable.value(sdRatio);
  }
//------------------------------------------------------------------------------------------------
//-------- Calculate the daily uptake
//------------------------------------------------------------------------------------------------
void Water::calcUptake(void)
   {
   //we have no uptake if there is no demand or potential
   if(totalSupply <= 0.0 || swDemand <= 0.0)
      {
      return;
      }
   // if demand is less than roots could take up. water is non-limiting.
   // distribute demand proportionately in all layers.
  if(swDemand < totalSupply)
      {
      for(int layer = 0; layer <= currentLayer; layer++)
         {
         dltSwDep[layer] = -1 * divide (supply[layer],totalSupply, 0.0) * swDemand;
         }
      }
   // water is limiting - not enough to meet demand so take what is available
   else
      {
      for(int layer = 0; layer <= currentLayer; layer++)
         {
         dltSwDep[layer] = -1 * supply[layer];
         }
      }
   totalUptake += sumVector(dltSwDep) * -1;
   }
//------------------------------------------------------------------------------------------------
float Water::calcPeswSeed(void)
   {
   return divide(esw[currentLayer],dLayer[currentLayer],0.0);
   }
//------------------------------------------------------------------------------------------------
float Water::swAvailFracLayer(int layer)
   {
   return divide(available[layer],availablePot[layer]);
   }
//------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------
//Get functions for registration
//------------------------------------------------------------------------------------------------
void Water::getEswLayers(protocol::Component *system, protocol::QueryValueData &qd)
   {
   system->sendVariable(qd, protocol::vector<float>(&esw[0], &esw[0] + esw.size()));
   }
//------------------------------------------------------------------------------------------------
void Water::getSwDefLayers(protocol::Component *system, protocol::QueryValueData &qd)
   {
   system->sendVariable(qd, protocol::vector<float>(&swDef[0], &swDef[0] + swDef.size()));
   }
//------------------------------------------------------------------------------------------------
void Water::getSwUptakeLayers(protocol::Component *system, protocol::QueryValueData &qd)
   {
   system->sendVariable(qd, protocol::vector<float>(&swUptake[0], &swUptake[0] + swUptake.size()));
   }
//------------------------------------------------------------------------------------------------

