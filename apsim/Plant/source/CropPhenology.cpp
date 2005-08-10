#include "CropPhenology.h"
#include "PlantComponent.h"

using namespace std;

void CropPhenology::zeroDeltas(void)
   {
   dlt_tt = dlt_tt_phenol = dlt_cumvd = 0.0;
   }
void CropPhenology::zeroAllGlobals(void)
   {
   PlantPhenology::zeroAllGlobals();
   das = cumvd = 0.0;
   }
void CropPhenology::readConstants (protocol::Component *s, const string &section)
   {
   PlantPhenology::readConstants(s, section);
   zeroAllGlobals();
   }
void CropPhenology::doRegistrations (protocol::Component *s)
   {
   PlantPhenology::doRegistrations(s);


   setupEvent(parentPlant, "sow", RegistrationType::respondToEvent, &CropPhenology::onSow);
   setupEvent(parentPlant, "end_crop", RegistrationType::respondToEvent, &CropPhenology::onEndCrop);

   parentPlant->addGettableVar("das", das,               "d", "Days after Sowing");
   parentPlant->addGettableVar("dlt_tt_phenol", dlt_tt_phenol,"dd", "Todays thermal time (incl. stress factors)");
   parentPlant->addGettableVar("dlt_tt", dlt_tt,         "dd", "Todays thermal time (no stress factors)");
   parentPlant->addGettableVar("dlt_cumvd", dlt_cumvd,   "", "Todays vd");
   parentPlant->addGettableVar("flowering_das", flowering_das, "days", "Days from sowing to flowering");
   parentPlant->addGettableVar("maturity_das", maturity_das, "days", "Days from sowing to maturity");

   }

void CropPhenology::onSow(unsigned &, unsigned &, protocol::Variant &v)
   {
   protocol::ApsimVariant incomingApsimVariant(parentPlant);
   incomingApsimVariant.aliasTo(v.getMessageData());
   if (incomingApsimVariant.get("sowing_depth", protocol::DTsingle, false, sowing_depth) == false)
      {
      throw std::invalid_argument("sowing_depth not specified");
      }
   bound_check_real_var(parentPlant, sowing_depth, 0.0, 100.0, "sowing_depth");
   currentStage = 1.0;
   das = 0;
   setupTTTargets();
   }


void CropPhenology::onEndCrop(unsigned &, unsigned &, protocol::Variant &)
   {
   zeroAllGlobals();
   }

void CropPhenology::onHarvest(unsigned &, unsigned &, protocol::Variant &)
   {
   previousStage = currentStage;
   currentStage = stage_reduction_harvest[currentStage];
   for (unsigned int stage = currentStage; stage != phases.size(); stage++)
      phases[stage].reset();
   }

void CropPhenology::onKillStem(unsigned &, unsigned &, protocol::Variant &)
   {
   previousStage = currentStage;
   currentStage = stage_reduction_kill_stem[currentStage];
   for (unsigned int stage = (int)currentStage; stage != phases.size(); stage++)
      phases[stage].reset();
   }

void CropPhenology::readSpeciesParameters(protocol::Component *s, vector<string> &sections)
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

   s->readParameter (sections,
                       "shoot_lag"//, "(oc)"
                      , shoot_lag
                      , 0.0, 100.0);

   s->readParameter (sections,
                       "shoot_rate"//, "(oc/mm)"
                      , shoot_rate
                      , 0.0, 100.0);

   s->readParameter (sections,
                       "pesw_germ"//, "(mm/mm)"
                      , pesw_germ
                      , 0.0, 1.0);

   rel_emerg_rate.search(s, sections,
                         "fasw_emerg", "()", 0.0, 1.0,
                         "rel_emerg_rate",  "()", 0.0, 1.0);

   }

void CropPhenology::update(void)
   {
   PlantPhenology::update();
   if (on_day_of ("flowering"))
        {
        flowering_das = das;
        }
   if (on_day_of ("maturity"))
        {
        maturity_das = das;
        }
   }

//  Purpose
//    Determine whether seed germinates based on soil water availability
bool CropPhenology::plant_germination(float pesw_germ,         //(INPUT)  plant extractable soil water required for germination
                       float sowing_depth,      //(INPUT)  depth of seed (mm)
                       const environment_t &sw) //(INPUT)  soil water structure

   {
   float pesw_seed;              // plant extractable soil water in
                                 // seedling layer available for
                                 // germination ( mm/mm)

   // determine if soil water content is sufficient to allow germination.
   int layer_no_seed = sw.find_layer_no (sowing_depth);
   pesw_seed = divide (sw.sw_dep[layer_no_seed] - sw.ll_dep[layer_no_seed],
                       sw.dlayer[layer_no_seed], 0.0);

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