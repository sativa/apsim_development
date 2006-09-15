#include <sstream>
#include <ComponentInterface/Component.h>
#include <ComponentInterface/datatypes.h>
#include <ComponentInterface/ApsimVariant.h>
#include <ComponentInterface/MessageDataExt.h>
#include "PlantComponent.h"
#include "PlantLibrary.h"
#include "PlantInterface.h"
#include "PlantPhenology.h"
#include "CropPhenology.h"

using namespace std;

void CropPhenology::zeroDeltas(void)
//=======================================================================================
   {
   dlt_tt = dlt_tt_phenol  = 0.0;
   }

void CropPhenology::zeroAllGlobals(void)
//=======================================================================================
   {
   PlantPhenology::zeroAllGlobals();
   das = 0;
   }

void CropPhenology::readConstants (protocol::Component *s, const string &section)
//=======================================================================================
   {
   PlantPhenology::readConstants(s, section);
   zeroAllGlobals();
   }

void CropPhenology::doRegistrations (protocol::Component *s)
//=======================================================================================
   {
   PlantPhenology::doRegistrations(s);

   setupEvent(s, "sow", RegistrationType::respondToEvent, &CropPhenology::onSow, "<type/>");
   setupEvent(s, "end_crop", RegistrationType::respondToEvent, &CropPhenology::onEndCrop, "<type/>");

   s->addGettableVar("das", das, "d", "Days after Sowing");
   s->addGettableVar("dlt_tt_phenol", dlt_tt_phenol,"dd", "Todays thermal time (incl. stress factors)");
   s->addGettableVar("dlt_tt", dlt_tt, "dd", "Todays thermal time (no stress factors)");

   }

void CropPhenology::onSow(unsigned &, unsigned &, protocol::Variant &v)
//=======================================================================================
   {
   protocol::ApsimVariant incomingApsimVariant(plant->getComponent());
   incomingApsimVariant.aliasTo(v.getMessageData());
   if (incomingApsimVariant.get("sowing_depth", protocol::DTsingle, false, sowing_depth) == false)
      throw std::invalid_argument("sowing_depth not specified");
   bound_check_real_var(plant, sowing_depth, 0.0, 100.0, "sowing_depth");
   currentStage = 1.0;
   das = 0;
   setupTTTargets();
   }

void CropPhenology::onEndCrop(unsigned &, unsigned &, protocol::Variant &)
//=======================================================================================
   {
   zeroAllGlobals();
   }

void CropPhenology::onHarvest(unsigned &, unsigned &, protocol::Variant &)
//=======================================================================================
   {
   previousStage = currentStage;
   currentStage = stage_reduction_harvest[currentStage];
   for (unsigned int stage = (int) currentStage; stage != phases.size(); stage++)
      phases[stage]->reset();
   setupTTTargets();
   }

void CropPhenology::onKillStem(unsigned &, unsigned &, protocol::Variant &)
//=======================================================================================
   {
   previousStage = currentStage;
   currentStage = stage_reduction_kill_stem[currentStage];
   for (unsigned int stage = (int)currentStage; stage != phases.size(); stage++)
      phases[stage]->reset();
   setupTTTargets();
   }

void CropPhenology::readSpeciesParameters(protocol::Component *s, vector<string> &sections)
//=======================================================================================
   {
   PlantPhenology::readSpeciesParameters (s, sections);
   iniSectionList = sections;
   initialOnBiomassRemove = true;

   stage_reduction_harvest.search(s, sections,
                                "stage_code_list" , "()", 1.0, 100.0,
                                "stage_stem_reduction_harvest" , "()", 1.0, 100.0);

   stage_reduction_kill_stem.search(s, sections,
                                  "stage_code_list" , "()", 1.0, 100.0,
                                  "stage_stem_reduction_kill_stem" , "()", 1.0, 100.0);

   y_tt.search(s, sections,
               "x_temp", "oC", 0.0, 100.0,
               "y_tt", "oC days", 0.0, 100.0);

   s->readParameter (sections, "shoot_lag", shoot_lag, 0.0, 100.0);

   s->readParameter (sections, "shoot_rate", shoot_rate, 0.0, 100.0);

   s->readParameter (sections, "pesw_germ", pesw_germ, 0.0, 1.0);

   rel_emerg_rate.search(s, sections,
                         "fasw_emerg", "()", 0.0, 1.0,
                         "rel_emerg_rate",  "()", 0.0, 1.0);
   }

void CropPhenology::update(void)
//=======================================================================================
   {
   PlantPhenology::update();
   }

bool CropPhenology::plant_germination(float pesw_germ,         // plant extractable soil water required for germination
                                      float sowing_depth,      // depth of seed (mm)
                                      float pesw_seed) // soil water structure
//=======================================================================================
//    Determine whether seed germinates based on soil water availability
   {
   // Soil water content of the seeded layer must be > the
   // lower limit to be adequate for germination.
   if (pesw_seed > pesw_germ)
      {
      // we have germination
      return true;
      }
   // no germination yet.
   return false;
   }
