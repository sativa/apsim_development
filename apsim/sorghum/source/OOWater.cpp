//------------------------------------------------------------------------------------------------
#pragma hdrstop

#include <vector>

#include "OOPlant.h"
#include "OOPlantComponents.h"
#include "OOWater.h"
//------------------------------------------------------------------------------------------------

#pragma package(smart_init)

//------------------------------------------------------------------------------------------------
//------ Water Constructor
//------------------------------------------------------------------------------------------------
Water::Water(ScienceAPI &api, OOPlant *p) : PlantProcess(api)
   {
   plant = p;

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
   scienceAPI.expose("sw_supply_demand_ratio", "",   "Water Supply/demand ratio",false,                       sdRatio);
   scienceAPI.expose("sw_supply_sum",          "mm", "Accumulative soil water supply over the profile",false, AccTotalSupply);
   scienceAPI.expose("sw_supply",              "mm", "Daily soil water supply over the profile",false,        totalSupply);
   scienceAPI.expose("sw_demand",              "mm", "Total crop demand for water",false,                     swDemand);
   scienceAPI.expose("transpiration",          "mm", "Daily water uptake from all rooted soil layers",false,  dltUptake);
   scienceAPI.expose("transpiration_tot",      "mm", "Accumulative water uptake from the whole profile",false,totalUptake);
   scienceAPI.expose("cep",                    "mm", "Accumulative water uptake from the whole profile",false,totalUptake);
   scienceAPI.expose("swdef_photo",            "",   "Water stress factor for photosynthesis",false,          photoStress);
   scienceAPI.expose("swdef_pheno",            "",   "Water stress factor for phenology",false,               phenoStress);
   scienceAPI.expose("swdef_expan",            "",   "Water stress factor for leaf expansion growth",false,   expansionStress);
   scienceAPI.expose("esw_profile",            "",   "Plant extractable water over the whole profile",false,  eswTot);
   scienceAPI.expose("ep",                     "",   "Water uptake from the whole profile",false,             ep);

   scienceAPI.exposeFunction("esw_layer", "mm", "Plant extractable soil water in each layer",
                    FloatArrayFunction(&Water::getEswLayers));
   scienceAPI.exposeFunction("sw_deficit", "mm", "Soil water deficit below dul (dul - sw)",
                    FloatArrayFunction(&Water::getSwDefLayers));
   scienceAPI.exposeFunction("sw_uptake", "mm", "Daily water uptake in each different rooted layers",
                    FloatArrayFunction(&Water::getSwUptakeLayers));
   scienceAPI.exposeFunction("ll_dep", "mm", "Crop lower limit",
                    FloatArrayFunction(&Water::getllDep));

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
void Water::readLL(void) 
{
   scienceAPI.read("ll","mm/mm", 0, ll);

   if (ll.size() != (unsigned int)nLayers) 
      {
      string msg = "Number of soil layers (";
      msg += itoa(nLayers) ;
      msg += ") doesn't match ll parameter (";
      msg += itoa(ll.size());
      msg += ").";
      throw std::runtime_error(msg);
      }
   llDep.clear();
   eswCap.clear();
   for(int layer = 0; layer < nLayers; layer++)
      {
      llDep.push_back(ll[layer]*dLayer[layer]);
      eswCap.push_back(dulDep[layer] - llDep[layer]);
      }

}
void Water::readParams (string cultivar)
   {
   swPhenoTable.read(scienceAPI,"x_sw_avail_ratio","y_swdef_pheno");
   swExpansionTable.read(scienceAPI,"x_sw_demand_ratio","y_swdef_leaf");

   readLL();
   scienceAPI.read("kl", "", 0, kl);
   scienceAPI.read("xf", "", 0, xf);

    // report
   char msg[100];
   sprintf(msg,"\n");   scienceAPI.write(msg);
   sprintf(msg,"\n");   scienceAPI.write(msg);
   sprintf(msg,"                       Root Profile\n");   scienceAPI.write(msg);
   sprintf(msg,"    ---------------------------------------------------\n");   scienceAPI.write(msg);
   sprintf(msg,"         Layer       Kl           Lower    Exploration\n");   scienceAPI.write(msg);
   sprintf(msg,"         Depth     Factor         Limit      Factor\n");   scienceAPI.write(msg);
   sprintf(msg,"         (mm)         ()        (mm/mm)       (0-1)\n");   scienceAPI.write(msg);
   sprintf(msg,"    ---------------------------------------------------\n");   scienceAPI.write(msg);

   for (int layer = 0; layer < nLayers; layer++)
      {
      sprintf (msg, "    %9.1f%10.3f%15.3f%12.3f\n", dLayer[layer],kl[layer],
                                              ll[layer],xf[layer]);
      scienceAPI.write(msg);
      }
   scienceAPI.write("    ---------------------------------------------------\n");
   scienceAPI.write("\n");

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
   scienceAPI.get("sw_dep", "mm", false, swDep, 0.0, 1000.0);

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
   scienceAPI.set("dlt_sw_dep", "mm", dltSwDep);
   }
//------------------------------------------------------------------------------------------------
//------- React to a newProfile message
//------------------------------------------------------------------------------------------------
void Water::onNewProfile(NewProfileType &v /* message */)
   {
   dLayer = v.dlayer_value;
   ll15Dep = v.ll15_dep_value;
   dulDep = v.dul_dep_value;
   satDep = v.sat_dep_value;
   swDep = v.sw_dep_value;
   bd = v.bd_value;

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
void Water::getEswLayers(vector<float> &result)
   {
   result = esw;
   }
//------------------------------------------------------------------------------------------------
void Water::getSwDefLayers(vector<float> &result)
   {
   result = swDef;
   }
//------------------------------------------------------------------------------------------------
void Water::getSwUptakeLayers(vector<float> &result)
   {
   result = swUptake;
   }
//------------------------------------------------------------------------------------------------
void Water::getllDep(vector<float> &result)
   {
   if (llDep.size() == 0)
      {
      readLL();
      }
   result = llDep;
   }
