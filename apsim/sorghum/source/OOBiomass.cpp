
#pragma hdrstop

#include "OOPlant.h"
#include "OOBiomass.h"

#include <ComponentInterface/dataTypes.h>
//---------------------------------------------------------------------------

#pragma package(smart_init)

//------------------------------------------------------------------------------------------------
//------ Biomass Constructor
//------------------------------------------------------------------------------------------------
Biomass::Biomass(OOPlant *p)
   {
   plant = p;
   plantInterface = p->plantInterface;

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
#define setupGetVar plantInterface->addGettableVar
   setupGetVar("dlt_dm", dltDM, "g/m^2", "Daily biomass production");
   setupGetVar("dlt_dm_water", dltDMPotTE, "g/m^2", "Potential daily biomass production due to TE");
   setupGetVar("dlt_dm_light", dltDMPotRUE, "g/m^2", "Potential daily biomass production due to RUE");
   setupGetVar("stem+flower_wt", stemRachisBiomass, "g/m^2", "Live plant stem and flower dry weight");
   setupGetVar("biomass", aboveGroundBiomass, "kg/ha", "Total above-ground biomass");
   setupGetVar("green_biomass_wt", aboveGroundGreenBiomass, "g/m^2", "Total live above-ground biomass");
   setupGetVar("hi", hi, "", "Harvest index");
   setupGetVar("stover_wt", dmStover, "g/m^2", "Stover biomass weight");
   setupGetVar("yield", yield, "kg/ha", "Grain yield");
   setupGetVar("green_biomass", biomGreen, "kg/ha", "Total above ground live biomass");
   setupGetVar("stover", biomStover, "kg/ha", "Stover biomass");
#undef setupGetVar

   setupGetFunction(plantInterface,"dm_green", protocol::DTsingle, true,
                    &Biomass::getDMGreen, "g/m^2", "Live plant dry weight");
   setupGetFunction(plantInterface,"dm_senesced", protocol::DTsingle, true,
                    &Biomass::getDMSenesced, "g/m^2", "Senesced plant dry weight");
   setupGetFunction(plantInterface,"dm_dead", protocol::DTsingle, true,
                    &Biomass::getDMDead, "g/m^2", "Dry weight of dead plants");
   setupGetFunction(plantInterface,"dlt_dm_green", protocol::DTsingle, true,
                    &Biomass::getDltDMGreen, "g/m^2", "Plant biomass growth in each part");
   setupGetFunction(plantInterface,"dlt_dm_detached", protocol::DTsingle, true,
                    &Biomass::getDltDMDetached, "g/m^2", "Plant biomass detached from each part");
   setupGetFunction(plantInterface,"dlt_dm_dead_detached", protocol::DTsingle, true,
                    &Biomass::getDltDMDeadDetached, "g/m^2", "Plant biomass detached from dead plant parts");
   setupGetFunction(plantInterface,"dlt_dm_green_retrans", protocol::DTsingle, true,
                    &Biomass::getDltDMGreenRetrans, "g/m^2", "Plant biomass retranslocated from each part");

   setupGetFunction(plantInterface,"biomass_wt", protocol::DTsingle, false,
                    &Biomass::getBiomass, "g/m2", "Total above-ground biomass");





   }                  
//------------------------------------------------------------------------------------------------
//------- Initialize variables
//------------------------------------------------------------------------------------------------
void Biomass::initialize(void)
   {
   aboveGroundBiomass = 0.0;
   hi = 0.0;
   yield = 0.0;
   dltDM = 0.0;
   dltDMPotTE = 0.0;
   dltDMPotRUE = 0.0;
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
   vector<string> sections;                  // sections to look for parameters
   sections.push_back("constants");
   sections.push_back(cultivar);
//   initialDM = readVar(plantInterface,sections,"dm_Biomass_init");

   readArray(plantInterface,sections,"ratio_root_shoot",ratioRootShoot);
   ratioRootShoot.insert(ratioRootShoot.begin(),0);  // for compatibility with fortran
   
   stem2FlowerFrac = readVar(plantInterface,sections,"frac_stem2flower");
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
void Biomass::getDMGreen(protocol::Component *system, protocol::QueryValueData &qd)
   {
   system->sendVariable(qd, protocol::vector<float>(&greenDM[0], &greenDM[0] + greenDM.size()));
   }
//------------------------------------------------------------------------------------------------
void Biomass::getDMSenesced(protocol::Component *system, protocol::QueryValueData &qd)
   {
   system->sendVariable(qd, protocol::vector<float>(&senescedDM[0], &senescedDM[0] + senescedDM.size()));
   }
//------------------------------------------------------------------------------------------------
void Biomass::getDMDead(protocol::Component *system, protocol::QueryValueData &qd)
   {
   system->sendVariable(qd, protocol::vector<float>(&deadDM[0], &deadDM[0] + deadDM.size()));
   }
//------------------------------------------------------------------------------------------------
void Biomass::getDltDMGreen(protocol::Component *system, protocol::QueryValueData &qd)
   {
   system->sendVariable(qd, protocol::vector<float>(&dltDMGreen[0], &dltDMGreen[0] + dltDMGreen.size()));
   }
//------------------------------------------------------------------------------------------------
void Biomass::getDltDMDetached(protocol::Component *system, protocol::QueryValueData &qd)
   {
   system->sendVariable(qd, protocol::vector<float>(&dltDMDetachedSen[0], &dltDMDetachedSen[0] + dltDMDetachedSen.size()));
   }
//------------------------------------------------------------------------------------------------
void Biomass::getDltDMDeadDetached(protocol::Component *system, protocol::QueryValueData &qd)
   {
   system->sendVariable(qd, protocol::vector<float>(&dltDMDetachedDead[0], &dltDMDetachedDead[0] + dltDMDetachedDead.size()));
   }
//------------------------------------------------------------------------------------------------
void Biomass::getDltDMGreenRetrans(protocol::Component *system, protocol::QueryValueData &qd)
   {
   system->sendVariable(qd, protocol::vector<float>(&dltDMRetranslocate[0], &dltDMRetranslocate[0] + dltDMRetranslocate.size()));
   }
//------------------------------------------------------------------------------------------------
void Biomass::getBiomass(protocol::Component *system, protocol::QueryValueData &qd)
   {
   float biom = aboveGroundBiomass / 10;
   system->sendVariable(qd, biom);
   }
//------------------------------------------------------------------------------------------------
void Biomass::Summary(void)
   {
   summaryLine(plantInterface,"total above ground biomass    (kg/ha) = %.1f",aboveGroundBiomass,NULL);
   summaryLine(plantInterface,"live above ground biomass     (kg/ha) = %.1f",aboveGroundBiomass,NULL);
   summaryLine(plantInterface,"green above ground biomass    (kg/ha) = %.1f",aboveGroundGreenBiomass*10,NULL);
   summaryLine(plantInterface,"senesced above ground biomass (kg/ha) = %.1f",
            aboveGroundBiomass - aboveGroundGreenBiomass*10,NULL);
   summaryLine(plantInterface,"dead above ground biomass     (kg/ha) = %.1f",
            aboveGroundBiomass - aboveGroundBiomass,NULL);
   }
//------------------------------------------------------------------------------------------------
void Biomass::incorporateResidue(void)
   {
   //Stover + remaining grain into surface residue     called from plantActions doEndCrop
   float carbon = totalBiomass - plant->roots->getDmGreen() -  plant->roots->getDmSenesced()
                                                   -   plant->roots->getDmDead();
   float n = plant->nitrogen->getNStover();
   float p = plant->phosphorus->getPStover();

   if(carbon > 0.0)
      {
      // Build surface residues by part
      vector<string> part_name;
      vector<float> fraction_to_residue;           // fraction sent to residue (0-1)
      vector<float> dlt_dm_crop;                   // change in dry matter of crop (kg/ha)
      vector<float> dlt_dm_n;                      // N content of changed dry matter (kg/ha)
      vector<float> dlt_dm_p;                      // P content of changed dry matter (kg/ha)

      float fracts[] = {0.0, 1.0, 1.0, 1.0, 1.0};

      for (unsigned part = 0; part < plant->PlantParts.size(); part++)
         {
         part_name.push_back(plant->PlantParts[part]->getName());
         dlt_dm_crop.push_back((plant->PlantParts[part]->getDmGreen() +
               plant->PlantParts[part]->getDmSenesced() + plant->PlantParts[part]->getDmDead()) *
                                                       gm2kg/sm2ha);
         dlt_dm_n.push_back((plant->PlantParts[part]->getNGreen() +
               plant->PlantParts[part]->getNSenesced() + plant->PlantParts[part]->getNDead()) *
                                                       gm2kg/sm2ha);
         dlt_dm_p.push_back((plant->PlantParts[part]->getPGreen() +
               plant->PlantParts[part]->getPSenesced() + plant->PlantParts[part]->getPDead()) *
                                                       gm2kg/sm2ha);


         fraction_to_residue.push_back(fracts[part]);
         }

      unsigned int id = plantInterface->addRegistration(RegistrationType::event,"crop_chopped", "", "", "");
/*      protocol::crop_choppedType chopped;
      chopped.crop_type = plant->getCropType();

      chopped. dm_type = part_name;
      chopped.dlt_crop_dm = dlt_dm_crop;
      chopped.dlt_dm_n = dlt_dm_n;
      chopped.fraction_to_residue = fraction_to_residue;


      plantInterface->publish (id, chopped);    */

    protocol::ApsimVariant outgoingApsimVariant(plantInterface);
    outgoingApsimVariant.store("crop_type", protocol::DTstring, false, FString(plant->getCropType().c_str()));

    // Make an FStrings string array and store it..
    unsigned int maxlen = 0;
    for (unsigned int i=0; i <  part_name.size();i++)
        {
        maxlen = max(maxlen, part_name[i].size());
        }
    char *buf = new char [maxlen*part_name.size()];
    memset(buf, 0,maxlen*part_name.size());
    for (unsigned int i=0; i <  part_name.size();i++)
        {
        strncpy(buf+i*maxlen, part_name[i].c_str(), maxlen);
        }
    outgoingApsimVariant.store("dm_type", protocol::DTstring, true,
              FStrings(buf, maxlen, part_name.size(), part_name.size()));
    delete [] buf;

    outgoingApsimVariant.store("dlt_crop_dm", protocol::DTsingle, true, dlt_dm_crop);
    outgoingApsimVariant.store("dlt_dm_n", protocol::DTsingle, true, dlt_dm_n);
    outgoingApsimVariant.store("dlt_dm_p", protocol::DTsingle, true, dlt_dm_p);
    outgoingApsimVariant.store("fraction_to_residue", protocol::DTsingle, true, fraction_to_residue);
    plantInterface->publish (id, outgoingApsimVariant);


      }
   }
//------------------------------------------------------------------------------------------------



