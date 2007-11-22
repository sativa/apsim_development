#pragma hdrstop

#include <ComponentInterface2/Variant.h>

#include "OOPlant.h"
#include "OOPlantComponents.h"
#include "OOBiomass.h"

//---------------------------------------------------------------------------

#pragma package(smart_init)

//------------------------------------------------------------------------------------------------
//------ Biomass Constructor
//------------------------------------------------------------------------------------------------
Biomass::Biomass(ScienceAPI &api, OOPlant *p) : PlantProcess(api)
   {
   plant = p;

   initialize();
   doRegistrations();

   }
//------------------------------------------------------------------------------------------------
//------ Biomass Destructor
//------------------------------------------------------------------------------------------------
Biomass::~Biomass()
   {

   }
//--------------------------------------------------------------------------------------------------
// Register variables for other modules
//--------------------------------------------------------------------------------------------------
void Biomass::doRegistrations(void)
   {
   scienceAPI.expose("dlt_dm",           "g/m^2", "Daily biomass production",false,                     dltDM);
   scienceAPI.expose("dlt_dm_water",     "g/m^2", "Potential daily biomass production due to TE",false, dltDMPotTE);
   scienceAPI.expose("dlt_dm_light",     "g/m^2", "Potential daily biomass production due to RUE",false,dltDMPotRUE);
   scienceAPI.expose("stem+flower_wt",   "g/m^2", "Live plant stem and flower dry weight",false,        stemRachisBiomass);
   scienceAPI.expose("biomass",          "kg/ha", "Total above-ground biomass",false,                   aboveGroundBiomass);
   scienceAPI.expose("green_biomass_wt", "g/m^2", "Total live above-ground biomass",false,              aboveGroundGreenBiomass);
   scienceAPI.expose("hi",               "",  "Harvest index",false,                                    hi);
   scienceAPI.expose("stover_wt",        "g/m^2", "Stover biomass weight",false,                        dmStover);
   scienceAPI.expose("yield",            "kg/ha", "Grain yield",false,                                  yield);
   scienceAPI.expose("green_biomass",    "kg/ha", "Total above ground live biomass",false,              biomGreen);
   scienceAPI.expose("stover",           "kg/ha", "Stover biomass",false,                               biomStover);

   scienceAPI.exposeFunction("dm_green", "g/m^2", "Live plant dry weight",
                    FloatFunction(&Biomass::getDMGreen));
   scienceAPI.exposeFunction("dm_senesced", "g/m^2", "Senesced plant dry weight",
                    FloatFunction(&Biomass::getDMSenesced));
   scienceAPI.exposeFunction("dm_dead", "g/m^2", "Dry weight of dead plants",
                    FloatFunction(&Biomass::getDMDead));
   scienceAPI.exposeFunction("dlt_dm_green", "g/m^2", "Plant biomass growth in each part",
                    FloatFunction(&Biomass::getDltDMGreen));
   scienceAPI.exposeFunction("dlt_dm_detached", "g/m^2", "Plant biomass detached from each part",
                    FloatArrayFunction(&Biomass::getDltDMDetached));
   scienceAPI.exposeFunction("dlt_dm_dead_detached", "g/m^2", "Plant biomass detached from dead plant parts",
                    FloatArrayFunction(&Biomass::getDltDMDeadDetached));
   scienceAPI.exposeFunction("dlt_dm_green_retrans", "g/m^2", "Plant biomass retranslocated from each part",
                    FloatArrayFunction(&Biomass::getDltDMGreenRetrans));

   scienceAPI.exposeFunction("biomass_wt", "g/m2", "Total above-ground biomass",
                    FloatFunction(&Biomass::getBiomass));

   }                  
//------------------------------------------------------------------------------------------------
//------- Initialize variables
//------------------------------------------------------------------------------------------------
void Biomass::initialize(void)
   {
   effectiveRue = 0.0;
   stem2FlowerFrac = 0.0;
   aboveGroundBiomass = 0.0;
   aboveGroundGreenBiomass = 0.0;
   totalBiomass = 0.0;
   stemRachisBiomass = 0.0;
   hi = 0.0;
   yield = 0.0;
   dltDMPotTE = 0.0;
   dltDMPotRUE = 0.0;
   dltDM = 0.0;
   stage = 0.0;
   dmStover = 0.0;
   biomGreen = 0.0;
   biomStover = 0.0;

   //Setup report vectors
   greenDM.clear();
   senescedDM.clear();
   deadDM.clear();
   dltDMGreen.clear();
   dltDMDetachedSen.clear();
   dltDMDetachedDead.clear();
   dltDMRetranslocate.clear();
   for(unsigned i = 0; i < plant->PlantParts.size(); i++)
      {
      greenDM.push_back(0.0);
      senescedDM.push_back(0.0);
      deadDM.push_back(0.0);
      dltDMGreen.push_back(0.0);
      dltDMDetachedSen.push_back(0.0);
      dltDMDetachedDead.push_back(0.0);
      dltDMRetranslocate.push_back(0.0);
      }
   }
//------------------------------------------------------------------------------------------------
//------ read Biomass parameters
//------------------------------------------------------------------------------------------------
void Biomass::readParams (string cultivar)
   {
   scienceAPI.read("ratio_root_shoot","", 0, ratioRootShoot);
   ratioRootShoot.insert(ratioRootShoot.begin(),0);  // for compatibility with fortran
   
   scienceAPI.read("frac_stem2flower", "", 0, stem2FlowerFrac);
   }


//------------------------------------------------------------------------------------------------
void Biomass::process(void)
   {
   calcBiomassTE();
   calcDltBiomass();

   // calculate grain biomass demand
   if(stage >= startGrainFill && stage <= endGrainFill)
      {
      plant->grain->calcDemandStress();
      plant->grain->calcBiomassDemand();
      }

   // biomass partitioning
   calcPartitioning();
   // biomass retranslocation
   if(stage >= startGrainFill && stage <= endGrainFill)
      calcRetranslocation();

   dmScenescence();
   }
//------------------------------------------------------------------------------------------------
//------ read Biomass parameters
//------------------------------------------------------------------------------------------------
void Biomass::updateVars(void)
   {
   for(unsigned i = 0; i < plant->PlantParts.size(); i++)
      {
      greenDM[i] = plant->PlantParts[i]->getDmGreen();
      senescedDM[i] = plant->PlantParts[i]->getDmSenesced();
      deadDM[i] = plant->PlantParts[i]->getDmDead();
      dltDMGreen[i] = plant->PlantParts[i]->getDltDmGreen();
      dltDMDetachedSen[i] = plant->PlantParts[i]->getDltDetDmSenesced();
      dltDMDetachedDead[i] = plant->PlantParts[i]->getDltDetDmDead();
      dltDMRetranslocate[i] = plant->PlantParts[i]->getDltDmRetranslocate();
      }
   float greenBiomass = sumVector(greenDM);
   totalBiomass = greenBiomass + sumVector(senescedDM) + sumVector(deadDM);

   aboveGroundGreenBiomass = greenBiomass - plant->roots->getDmGreen() -
                                                      plant->roots->getDmDead();

   aboveGroundBiomass = aboveGroundGreenBiomass + plant->leaf->getDmSenesced();

   stage = plant->phenology->currentStage();

   stemRachisBiomass = plant->rachis->getDmGreen() + plant->stem->getDmGreen();

   yield = plant->grain->getDmGreen() * 10;
   dmStover = aboveGroundBiomass - plant->grain->getDmGreen() - plant->grain->getDmSenesced();
   biomStover = dmStover * 10;
   biomGreen = aboveGroundGreenBiomass * 10;
   aboveGroundBiomass *= 10;
   //Calculate harvest index
   if(aboveGroundBiomass > 0.0)
      {
      hi = yield / aboveGroundBiomass;
      }
   }
//------------------------------------------------------------------------------------------------
//------------------- calculate biomass production due to water (transpiration)
//------------------------------------------------------------------------------------------------
void Biomass::calcBiomassTE(void)
   {
   dltDMPotTE = calcDltDMPotTE();
   }
//------------------------------------------------------------------------------------------------
//------------------- calculate biomass production due to light (limited by water and n)
//------------------------------------------------------------------------------------------------
void Biomass::calcBiomassRUE(float rue, float radnIntercepted)
   {
   effectiveRue = rue * Min(plant->getTempStress(),plant->nitrogen->getPhotoStress());
   dltDMPotRUE =  effectiveRue * radnIntercepted;
   }
//------------------------------------------------------------------------------------------------
//-------------------
//------------------------------------------------------------------------------------------------
void Biomass::calcDltBiomass(void)
   {
   dltDM = Min(dltDMPotRUE, dltDMPotTE);
   }
//------------------------------------------------------------------------------------------------
//-------------------  Partitioning
//------------------------------------------------------------------------------------------------
void Biomass::calcPartitioning(void)
   {
   calcBiomassPartitioning();
   }
//------------------------------------------------------------------------------------------------
//-------------------  Partitioning
//------------------------------------------------------------------------------------------------
void Biomass::calcRetranslocation(void)
   {
   calcBiomassRetranslocation();
   }
//------------------------------------------------------------------------------------------------
//-------------------  Scenescence
//------------------------------------------------------------------------------------------------
void Biomass::dmScenescence(void)
   {
   plant->roots->calcSenescence();
   plant->leaf->calcSenescence();
   }

//------------------------------------------------------------------------------------------------
float Biomass::calcDltDMPotTE(void)
   {
   return plant->water->getTotalSupply() * plant->getTranspEff();
   }
//------------------------------------------------------------------------------------------------
void Biomass::calcBiomassPartitioning(void)
   {
   // Roots
   // Root must be satisfied. The roots don't take any of the carbohydrate produced
   //  - that is for tops only.  Here we assume that enough extra was produced to meet demand.
   // Thus the root growth is not removed from the carbo produced by the model.
   float biomPool = dltDM;

   int currentPhase = (int) stage;
   plant->roots->partitionDM(ratioRootShoot[currentPhase] * biomPool);

   //  leaf and stem to fi then rachis as well to flag
   if(stage >= emergence && stage < flag)
      {
      // leaf first
      biomPool -= plant->leaf->partitionDM(biomPool);

      //if stage > fi give some to rachis
      if(stage >= fi)
         biomPool -= plant->rachis->partitionDM(biomPool * stem2FlowerFrac);

      // rest to stem
      plant->stem->partitionDM(biomPool);

      }
   else if(stage >= flag && stage < flowering)
      {
      // we only have rachis and stem growth here
      biomPool -= plant->rachis->partitionDM(biomPool * stem2FlowerFrac);
      plant->stem->partitionDM(biomPool);
      }
   else if(stage >= flowering && stage < maturity)
      {
      //grain filling starts - stem continues when it can
      biomPool -= plant->grain->partitionDM(biomPool);
      plant->stem->partitionDM(biomPool);
      }
   else
      {
      plant->stem->partitionDM(biomPool);
      }
   }
//------------------------------------------------------------------------------------------------
// Calculate plant dry matter delta's due to retranslocation to grain (g/m^2)
void Biomass::calcBiomassRetranslocation(void)
   {
   float grainDifferential = plant->grain->grainDMDifferential();
   if(grainDifferential > 0)
      {
      // we can translocate stem and leaf carbohydrate to grain if needed

      float stemWtAvail = plant->stem->dmRetransAvailable();
      float stemRetrans = Min(grainDifferential,stemWtAvail);
      grainDifferential -= stemRetrans;
      plant->stem->dmRetrans(-1 * stemRetrans);

      float leafWtAvail = plant->leaf->dmRetransAvailable();
      float leafRetrans = Min(grainDifferential,leafWtAvail);
      grainDifferential -= leafRetrans;

      plant->leaf->dmRetrans(-1 * leafRetrans);
      plant->grain->dmRetrans(stemRetrans + leafRetrans);
      }
   }
//------------------------------------------------------------------------------------------------
//------- Calculate Dry Matter detachment
//------------------------------------------------------------------------------------------------
void Biomass::detachment(vector<float> senDetachFrac, vector<float> deadDetachFrac)
   {
   for(unsigned i = 0; i < plant->PlantParts.size(); i++)
      {
      plant->PlantParts[i]->dmDetachment(senDetachFrac, deadDetachFrac);
      }
   }
//------------------------------------------------------------------------------------------------
void Biomass::getDMGreen(float &result)
   {
   result = sumVector(greenDM);
   }
//------------------------------------------------------------------------------------------------
void Biomass::getDMSenesced(float &result)
   {
   result = sumVector(senescedDM);
   }
//------------------------------------------------------------------------------------------------
void Biomass::getDMDead(float &result)
   {
   result = sumVector(deadDM);
   }
//------------------------------------------------------------------------------------------------
void Biomass::getDltDMGreen(float &result)
   {
   result = sumVector(dltDMGreen);
   }
//------------------------------------------------------------------------------------------------
void Biomass::getDltDMDetached(vector<float> &result)
   {
   result = dltDMDetachedSen;
   }
//------------------------------------------------------------------------------------------------
void Biomass::getDltDMDeadDetached(vector<float> &result)
   {
   result = dltDMDetachedDead;
   }
//------------------------------------------------------------------------------------------------
void Biomass::getDltDMGreenRetrans(vector<float> &result)
   {
   result = dltDMRetranslocate;
   }
//------------------------------------------------------------------------------------------------
void Biomass::getBiomass(float &result)
   {
   result = aboveGroundBiomass / 10;
   }
//------------------------------------------------------------------------------------------------
void Biomass::Summary(void)
   {
   char msg[80];
   sprintf(msg,"total above ground biomass    (kg/ha) = %.1f\n",aboveGroundBiomass); scienceAPI.write(msg);
   sprintf(msg,"live above ground biomass     (kg/ha) = %.1f\n",aboveGroundBiomass); scienceAPI.write(msg);
   sprintf(msg,"green above ground biomass    (kg/ha) = %.1f\n",aboveGroundGreenBiomass*10); scienceAPI.write(msg);
   sprintf(msg,"senesced above ground biomass (kg/ha) = %.1f\n",aboveGroundBiomass - aboveGroundGreenBiomass*10); scienceAPI.write(msg);
   sprintf(msg,"dead above ground biomass     (kg/ha) = %.1f\n",aboveGroundBiomass - aboveGroundBiomass); scienceAPI.write(msg);
   }
//------------------------------------------------------------------------------------------------
void Biomass::incorporateResidue(void)
   {
   //Stover + remaining grain into surface residue     called from plantActions doEndCrop
   float remaining = totalBiomass - plant->roots->getDmGreen() -  
                                 plant->roots->getDmSenesced() -   
                                 plant->roots->getDmDead();
   if (remaining > 0.0)
      {
      // Build surface residues by part
      float fracts[] = {0.0, 1.0, 1.0, 1.0, 1.0};  // No root to surface residue.

#if 0
      CropChoppedType chopped;
      chopped.crop_type = plant->getCropType();
      for (unsigned part = 0; part < plant->PlantParts.size(); part++)
         {
         chopped.dm_type.push_back(plant->PlantParts[part]->getName());
         chopped.dlt_crop_dm.push_back((plant->PlantParts[part]->getDmGreen() +
                                        plant->PlantParts[part]->getDmSenesced() + 
                                        plant->PlantParts[part]->getDmDead()) *
                                         gm2kg/sm2ha);
         chopped.dlt_dm_n.push_back((plant->PlantParts[part]->getNGreen() +
                                     plant->PlantParts[part]->getNSenesced() + 
                                     plant->PlantParts[part]->getNDead()) *
                                      gm2kg/sm2ha);

/// where did this get to???
///         chopped.dlt_dm_p.push_back((plant->PlantParts[part]->getPGreen() +
///                                     plant->PlantParts[part]->getPSenesced() + 
///                                     plant->PlantParts[part]->getPDead()) *
///                                      gm2kg/sm2ha);

         chopped.fraction_to_residue.push_back(fracts[part]);
         }
#else
      Variant chopped;
      vector<string> part_name;
      vector<float> fraction_to_residue;           // fraction sent to residue (0-1)
      vector<float> dlt_dm_crop;                   // change in dry matter of crop (kg/ha)
      vector<float> dlt_dm_n;                      // N content of changed dry matter (kg/ha)
      vector<float> dlt_dm_p;                      // N content of changed dry matter (kg/ha)

      for (unsigned part = 0; part < plant->PlantParts.size(); part++)
         {
         part_name.push_back(plant->PlantParts[part]->getName());
         dlt_dm_crop.push_back((plant->PlantParts[part]->getDmGreen() +
                                plant->PlantParts[part]->getDmSenesced() + 
                                plant->PlantParts[part]->getDmDead()) *
                                                       gm2kg/sm2ha);
         dlt_dm_n.push_back((plant->PlantParts[part]->getNGreen() +
                             plant->PlantParts[part]->getNSenesced() + 
                             plant->PlantParts[part]->getNDead()) *
                                                       gm2kg/sm2ha);
         dlt_dm_p.push_back((plant->PlantParts[part]->getPGreen() +
                             plant->PlantParts[part]->getPSenesced() + 
                             plant->PlantParts[part]->getPDead()) *
                                                       gm2kg/sm2ha);
         fraction_to_residue.push_back(fracts[part]);
         }


      pack(chopped, "crop_type",   plant->getCropType());
      pack(chopped, "dm_type",     part_name);
      pack(chopped, "dlt_crop_dm", dlt_dm_crop);
      pack(chopped, "dlt_dm_n",    dlt_dm_n);
      pack(chopped, "dlt_dm_p",    dlt_dm_p);
      pack(chopped, "fraction_to_residue", fraction_to_residue);
      
#endif
      scienceAPI.publish ("crop_chopped", chopped); 
      }
   }
//------------------------------------------------------------------------------------------------
