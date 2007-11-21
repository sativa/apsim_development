//------------------------------------------------------------------------------------------------
#pragma hdrstop

#include <ComponentInterface2/Variant.h>

#include "OOPlant.h"
#include "OOPlantComponents.h"
#include "OORoots.h"

#pragma package(smart_init)

//------------------------------------------------------------------------------------------------
//------ Roots Constructor
//------------------------------------------------------------------------------------------------
Roots::Roots(ScienceAPI& api, OOPlant *p) : PlantPart(api)
   {
   plant = p;
   name = "Roots";

   initialize();
   doRegistrations();
   }
//------------------------------------------------------------------------------------------------
//------ Roots Destructor
//------------------------------------------------------------------------------------------------
Roots::~Roots()
   {
   }
//------------------------------------------------------------------------------------------------
// Register variables for other modules
//--------------------------------------------------------------------------------------------------
void Roots::doRegistrations(void)
   {

   scienceAPI.expose("root_length", "mm/mm2", "Root length", false, rootLength);
   scienceAPI.expose("rlv", "mm/mm3", "Root length density in layers", false, rlvFactor);
   scienceAPI.exposeFunction("root_proportion", "0-1", "Root proportion in layers",
                    FloatArrayFunction(&Roots::getRP));

   scienceAPI.expose("root_depth", "mm", "Depth of roots",false,               rootDepth);
   scienceAPI.expose("root_front", "mm", "Depth of roots",false,               rootFront);
   scienceAPI.expose("root_wt",    "g/m2", "Live root dry weight",false,       dmGreen);
   scienceAPI.expose("groot_n",    "g/m2", "N in live root",false,             nGreen);
   scienceAPI.expose("troot_n",    "g/m2", "N in live and dead roots",false,   nTotal);
   scienceAPI.expose("RootGreenNConc", "%", "Live root N concentration", false, nConc);
   scienceAPI.expose("droot_wt",   "g/m2", "Dead root dry weight",false,       dmSenesced);
   scienceAPI.expose("troot_wt",   "g/m2", "Total root dry weight",false,      rootDMTot);
   scienceAPI.expose("root_nd",    "g/m2", "Today's N demand from roots",false,nDemand);
   }

//------------------------------------------------------------------------------------------------
//------- React to a newProfile message
//------------------------------------------------------------------------------------------------
void Roots::onNewProfile(NewProfileType &v /* message */)
   {
   dLayer = v.dlayer_value;

   // dlayer may be changed from its last setting due to erosion
   profileDepth = sumVector(dLayer);      // depth of soil profile (mm)
   nLayers = dLayer.size();
   /* TODO : Insert new root profile and llDep code for change in profile due to erosion */
   rootLength.clear();
   rlvFactor.clear();
   dltRootLength.clear();
   dltScenescedRootLength.clear();

   for(int i=0;i < nLayers;i++)
      {
      rootLength.push_back(0.0);
      rlvFactor.push_back(0.0);
      dltRootLength.push_back(0.0);
      dltScenescedRootLength.push_back(0.0);
      }

   /* TODO : Check validity of ll,dul etc as in crop_check_sw */
   /* TODO : why does this not include no3 */
   }
//------------------------------------------------------------------------------------------------
//------- Initialize variables
//------------------------------------------------------------------------------------------------
void Roots::initialize(void)
   {
   rootDepth = 0.0;
   rootFront = 0.0;
   currentLayer = 0;
   leftDist = 0.0;
   rightDist = 0.0;


   partNo = 0;

   PlantPart::initialize();

   }
//------------------------------------------------------------------------------------------------
//------- Read root parameters
//------------------------------------------------------------------------------------------------
void Roots::readParams (string cultivar)
   {
   scienceAPI.read("xf", "", 0, xf);

   initialRootDepth   = plant->getSowingDepth();
   scienceAPI.read("dm_root_init", "", 0, initialDM);
   scienceAPI.read("specific_root_length", "", 0, specificRootLength);
   scienceAPI.read("dm_root_sen_frac", "", 0, dmRootSenFrac);
   scienceAPI.read("root_depth_rate","", 0, rootDepthRate);
   rootDepthRate.insert(rootDepthRate.begin(),0);  // for compatibility with fortran

   swRoot.read(scienceAPI, "x_sw_ratio","y_sw_fac_root");
   rldFn.read(scienceAPI, "x_plant_rld","y_rel_root_rate");

   // nitrogen
   scienceAPI.read("initialRootNConc", "", 0, initialNConc);
   scienceAPI.read("targetRootNConc", "", 0, targetNConc);

   // phosphorus
   pMaxTable.read(scienceAPI, "x_p_stage_code","y_p_conc_max_root");
   pMinTable.read(scienceAPI, "x_p_stage_code","y_p_conc_min_root");
   pSenTable.read(scienceAPI, "x_p_stage_code","y_p_conc_sen_root");
   scienceAPI.read("p_conc_init_root", "", 0, initialPConc);

   }

//------------------------------------------------------------------------------------------------
void Roots::process(void)
   {
   // root length
   if(stage > germination)
      {
      calcRootDistribution();
      calcSenLength();
      }
   }

//------------------------------------------------------------------------------------------------
//------- react to a phenology event
//------------------------------------------------------------------------------------------------
void Roots::phenologyEvent(int stage)
   {
   switch (stage)
      {
      case germination :
//         rootDepth = initialRootDepth;
         calcInitialLength();
         leftDist = plant->getRowSpacing() * 1000 * (plant->getSkipRow() - 0.5);
         rightDist = plant->getRowSpacing() * 1000 * 0.5;

         break;
      case emergence :
         dmGreen = initialDM * plant->getPlantDensity();
         nGreen = initialNConc * dmGreen;
         pGreen = initialPConc * dmGreen;
         break;
      }
   }
//------------------------------------------------------------------------------------------------
//------- at the end of the day, update state variables   -  called from plant->process
//------------------------------------------------------------------------------------------------
void Roots::updateVars(void)
   {
   stage = plant->phenology->currentStage();
   // update root variables by daily deltas
   dltRootDepth = calcDltRootDepth(plant->phenology->currentStage());
   rootDepth += dltRootDepth;
   dltRootFront = calcDltRootFront(plant->phenology->currentStage());
   rootFront += dltRootFront;
   // calculate current root layer
   currentLayer = findIndex(rootDepth, dLayer);
   // calculate proportion of this layer occupied
   lastLayerPropn = layerProportion();

   dmGreen += dltDmGreen - dltDmSenesced;
   dmSenesced += dltDmSenesced;
   nGreen  += dltNGreen  - dltNSenesced;
   nSenesced += dltNSenesced;
   nConc = divide(nGreen,dmGreen,0);


   rootDMTot = dmGreen + dmSenesced;

   nTotal = nGreen + nSenesced;
   }
//------------------------------------------------------------------------------------------------
//-------
//------------------------------------------------------------------------------------------------
float Roots::calcNDemand(void)
   {
   // ROOT demand to keep root [N] at targetRootNConc
   float nRequired = (dmGreen + dltDmGreen) * targetNConc;   // g/m2
   nDemand = Max(nRequired - nGreen,0.0);
   return nDemand;
   }
//------------------------------------------------------------------------------------------------
float Roots::layerProportion(void)
   {
   // calculates the proportion of the current root layer that is populated by roots
   float layerTop    = sumVector(dLayer, currentLayer);
   float layerBottom = sumVector(dLayer, currentLayer+1);

   return divide(rootDepth - layerTop,layerBottom - layerTop);
   }
//------------------------------------------------------------------------------------------------
void Roots::calcInitialLength(void)
   {


    //In the fortran it is only done by sowing depth
    dltRootDepth = initialRootDepth;
    dltRootFront = initialRootDepth;

/*
   float initialLength = dmGreen / sm2smm * specificRootLength;
   float rld = divide (initialLength,rootDepth);

   for(int layer=0; layer <= currentLayer;layer++)
      {
      rootLength[layer] = rld * dLayer[layer];
      }
   rootLength[currentLayer] = rld * dLayer[currentLayer] * lastLayerPropn; */
   }
//------------------------------------------------------------------------------------------------
void Roots::calcSenLength(void)
   {
   float senescedLength = dltDmSenesced / sm2smm * specificRootLength;
   float rootLengthSum = sumVector(rootLength);
   for(int i=0;i <= currentLayer;i++)
      {
      dltScenescedRootLength[i] = senescedLength *
                                 divide(rootLength[i],rootLengthSum);
      }
   }
//------------------------------------------------------------------------------------------------
//-------- Calculate the increase in rooting depth.
//------------------------------------------------------------------------------------------------
// Calculate the increase in root length density in each rooted layer based upon soil hospitality,
// moisture and fraction of layer explored by roots.
//------------------------------------------------------------------------------------------------
void Roots::calcRootDistribution(void)
   {
   float rlvFactorTotal = 0.0;
   for(int layer = 0; layer <= currentLayer; layer++)
      {
      float rld = divide (rootLength[layer],dLayer[layer]);
      float plantRld = divide (rld, plant->getPlantDensity());
      float branchingFactor = rldFn.value(plantRld);
      rlvFactor[layer] = (swAvailFactor(layer) * branchingFactor * xf[layer] *
                  divide(dLayer[layer],rootDepth * 10));
      rlvFactor[layer] = Max(rlvFactor[layer],1e-6);
      rlvFactorTotal += rlvFactor[layer];
      }
   float dltLengthTot = dltDmGreen / sm2smm * specificRootLength;
   for(int layer = 0; layer <= currentLayer; layer++)
      {
      dltRootLength[layer] = dltLengthTot * divide(rlvFactor[layer],rlvFactorTotal);
      /* ADTODO : is this right???*/
      rootLength[layer] += dltRootLength[layer];
      }
   }
//------------------------------------------------------------------------------------------------
float Roots::calcDltRootDepth(float stage)
   {
   // sw available factor of root layer
   float swFactor = swAvailFactor(currentLayer);
   dltRootDepth  = rootDepthRate[int (stage)] * swFactor * xf[currentLayer];
   //constrain it by the maximum depth that roots are allowed to grow
   dltRootDepth = Min(dltRootDepth,profileDepth - rootDepth);

   return dltRootDepth;
   }
//------------------------------------------------------------------------------------------------
float Roots::calcDltRootFront(float stage)
   {
   // calculate the root front
   float swFactor = swAvailFactor(currentLayer);
   dltRootFront  = rootDepthRate[int (stage)] * swFactor * xf[currentLayer];

   double maxFront = sqrt(pow(rootDepth,2) + pow(leftDist,2));
   dltRootFront = Min(dltRootFront, maxFront - rootFront);
   return dltRootFront;
   }
//------------------------------------------------------------------------------------------------
float Roots::swAvailFactor(int layer)
   {
   // get the soil water availability fraction of the current layer
   float swAvailRatio = plant->water->swAvailRatio(layer);
   // use this value in the swRoot table function
   return swRoot.value(swAvailRatio);
   }
//------------------------------------------------------------------------------------------------
void Roots::partitionDM(float dltDM)
   {
   dltDmGreen = dltDM;
   }
//------------------------------------------------------------------------------------------------
void Roots::calcSenescence(void)
   {
   dltDmSenesced = dmGreen * dmRootSenFrac;
   float senNConc = divide(nGreen,dmGreen);
   dltNSenesced  = dltDmSenesced * senNConc;
   }
//------------------------------------------------------------------------------------------------
//Get functions for registration
//------------------------------------------------------------------------------------------------
void Roots::getRP(vector<float> &result)
   {
   for (int layer = 0; layer < nLayers; layer++)
      if (layer <= currentLayer)
        result.push_back(RootProportionInLayer(layer));
      else 
        result.push_back(0.0);  
   }
//------------------------------------------------------------------------------------------------
float Roots::calcPDemand(void)
   {
   // ROOT P demand

   float rel_growth_rate = divide(plant->biomass->getDltDMPotRUE(),
         plant->biomass->getAboveGroundBiomass(),0.0);

   float deficit = pConcMax() * dmGreen * (1.0 + rel_growth_rate) - pGreen;

   pDemand = Max(deficit,0.0);
   return pDemand;
   }
//------------------------------------------------------------------------------------------------
void Roots::incorporateResidue(void)
   {
   //Root residue incorporation    called from plantActions doEndCrop

   if(!totalBiomass() > 0.0)return;
   
   vector <float> dmIncorp;
   vector <float> nIncorp;
   vector <float> pIncorp;
   float rootLengthSum = sumVector(rootLength);

   float carbon = totalBiomass() * gm2kg /sm2ha;
   float n = totalN() * gm2kg /sm2ha;
   float p = totalP() * gm2kg /sm2ha;
   for (unsigned layer = 0; layer < dLayer.size(); layer++)
      {
      dmIncorp.push_back(carbon * divide(rootLength[layer],rootLengthSum,0.0));
      nIncorp.push_back(n * divide(rootLength[layer],rootLengthSum,0.0));
      pIncorp.push_back(p * divide(rootLength[layer],rootLengthSum,0.0));
      }

#if 0
   IncorpFomType incorpFom;
   incorpFom.dlt_fom_type_name =plant->getCropType();

   incorpFom.dlt_fom_wt_name =plant->getCropType(); //????
   incorpFom.dlt_fom_wt_value = dmIncorp;
   incorpFom.dlt_fom_n_value = nIncorp;
   incorpFom.dlt_fom_p_value = pIncorp;
#else
   Variant incorpFom;
   pack(incorpFom, "dlt_fom_type", plant->getCropType());
   pack(incorpFom, "dlt_fom_wt", dmIncorp);
   pack(incorpFom, "dlt_fom_n", nIncorp);
   pack(incorpFom, "dlt_fom_p", pIncorp);
#endif

   scienceAPI.publish ("incorp_fom", incorpFom);
   }
//------------------------------------------------------------------------------------------------
float Roots::RootProportionInLayer(int layer)
   {
   /* Row Spacing and configuration (skip) are used to calculate semicircular root front to give
   proportion of the layer occupied by the roots. */
   float top;
   if(layer == 0)top = 0;
   else top = sumVector(dLayer,layer);
   float bottom = top + dLayer[layer];

   float rootArea = getRootArea(top, bottom, rootFront, rightDist);    // Right side
   rootArea += getRootArea(top, bottom, rootFront, leftDist);          // Left Side
   float soilArea = (rightDist + leftDist) * (bottom - top);

   return divide(rootArea, soilArea);
   }
//------------------------------------------------------------------------------------------------
float Roots::getRootArea(float top, float bottom, float rootLength, float hDist)
   {
   // get the area occupied by roots in a semi-circular section between top and bottom
   float topArea = 0.0, bottomArea = 0;
   float SDepth, Theta, rootArea;

   // intersection of roots and Section
   if(rootLength <= hDist) SDepth = 0.0;
   else SDepth = sqrt(pow(rootLength,2) - pow(hDist,2));

   // Rectangle - SDepth past bottom of this area
   if(SDepth >= bottom) rootArea = (bottom - top) * hDist;
   else               // roots Past top
      {
      Theta = 2 * acos(divide(Max(top,SDepth),rootLength));
      topArea = (pow(rootLength,2) / 2.0 * (Theta - sin(Theta))) / 2.0;

      // bottom down
      if(rootLength > bottom)
         {
         Theta = 2 * acos(bottom/rootLength);
         bottomArea = (pow(rootLength,2) / 2.0 * (Theta - sin(Theta))) / 2.0;
         }
      // rectangle
      if(SDepth > top) topArea = topArea + (SDepth - top) * hDist;
      rootArea = topArea - bottomArea;
      }
   return rootArea;
   }
//------------------------------------------------------------------------------------------------


