#include "TTTphenology.h"
#include <ComponentInterface/Component.h>
#include <ComponentInterface/dataTypes.h>
#include <ComponentInterface/ApsimVariant.h>
#include <ComponentInterface/MessageDataExt.h>
#include "PlantComponent.h"
#include "PlantLibrary.h"
#include "PlantPhenology.h"
#include "Environment.h"

void TTTPhenology::zeroDeltas(void)
   {
   CropPhenology::zeroDeltas();
   dlt_cumvd = 0.0;
   }
void TTTPhenology::zeroAllGlobals(void)
   {
   CropPhenology::zeroAllGlobals();
   est_days_emerg_to_init=cumvd =0;
   }


void TTTPhenology::readConstants (protocol::Component *s, const string &section)
   {
   CropPhenology::readConstants(s, section);
   s->writeString("phenology model: TTT");
   }





// static TT targets (called at sowing)
void TTTPhenology::setupTTTargets(void)
   {
   pPhase *germ_to_emerg = getStage("germination");
   germ_to_emerg->setTarget(shoot_lag + sowing_depth * shoot_rate);

   pPhase *end_grain_to_maturity = getStage("end_grain_fill");
   end_grain_to_maturity->setTarget(tt_end_grain_to_maturity);

   pPhase *maturity_to_ripe = getStage("maturity");
   maturity_to_ripe->setTarget(tt_maturity_to_ripe);
   }

// dynamic TT targets
void TTTPhenology::updateTTTargets(const environment_t &e)
   {
   dlt_cumvd = vernal_days.value((e.maxt + e.mint)*0.5);

   if (inPhase("germination"))
      {
      pPhase *emerg_to_endjuv = getStage("emergence");
      emerg_to_endjuv->setTarget(tt_emerg_to_endjuv[cumvd + dlt_cumvd]);
      }
   else if (inPhase("emergence"))
      {
      if (on_day_of("emergence"))
         {
         int est_day_of_floral_init = e.day_of_year + est_days_emerg_to_init % 366;
         float est_photoperiod = e.daylength (est_day_of_floral_init, twilight);

         pPhase *endjuv_to_init = getStage("end_of_juvenile");
         endjuv_to_init->setTarget(tt_endjuv_to_init[est_photoperiod]);
         }

      pPhase *emerg_to_endjuv = getStage("emergence");
      emerg_to_endjuv->setTarget(tt_emerg_to_endjuv[cumvd + dlt_cumvd]);

      pPhase *endjuv_to_init = getStage("end_of_juvenile");
      endjuv_to_init->setTarget(tt_endjuv_to_init[photoperiod]);
      }
   else if (inPhase("end_of_juvenile"))
      {
      pPhase *endjuv_to_init = getStage("end_of_juvenile");
      endjuv_to_init->setTarget(tt_endjuv_to_init[photoperiod]);

      pPhase *init_to_flower = getStage("floral_initiation");
      init_to_flower->setTarget(tt_init_to_flower[photoperiod]);
      }
   else if (inPhase("floral_initiation"))
      {
      pPhase *init_to_flower = getStage("floral_initiation");
      init_to_flower->setTarget(tt_init_to_flower[photoperiod]);

      pPhase *flower_to_start_grain = getStage("flowering");
      flower_to_start_grain->setTarget(tt_flower_to_start_grain[photoperiod]);
      }
   else if (inPhase("flowering"))
      {
      pPhase *flower_to_start_grain = getStage("flowering");
      flower_to_start_grain->setTarget(tt_flower_to_start_grain[photoperiod]);

      pPhase *start_to_end_grain = getStage("start_grain_fill");
      start_to_end_grain->setTarget(tt_start_to_end_grain[photoperiod]);
      }
   else if (inPhase("start_grain_fill"))
      {
      pPhase *start_to_end_grain = getStage("start_grain_fill");
      start_to_end_grain->setTarget(tt_start_to_end_grain[photoperiod]);
      }
   else
      {
      //??
      }
   }





void TTTPhenology::readCultivarParameters(protocol::Component *s, const string & cultivar)
   {
   CropPhenology::readCultivarParameters(s, cultivar);

   s->readParameter (cultivar
                     , "est_days_emerg_to_init"//, "()"
                     , est_days_emerg_to_init
                     , 0, 100);

   tt_emerg_to_endjuv.read(s, cultivar
                            , "cum_vernal_days", "vd", 0.0, 100.0
                            , "tt_emerg_to_endjuv", "dd", 0.0, tt_emerg_to_endjuv_ub);

   tt_endjuv_to_init.read(s, cultivar
                          , "x_pp_endjuv_to_init", "h", 0.0, 24.0
                          , "y_tt_endjuv_to_init", "dd", 0.0, 1e6);

   tt_init_to_flower.read(s, cultivar
                          , "x_pp_init_to_flower", "h", 0.0, 24.0
                          , "y_tt_init_to_flower", "dd", 0.0, 1e6);

   tt_flower_to_start_grain.read(s, cultivar
                          , "x_pp_flower_to_start_grain", "h", 0.0, 24.0
                          , "y_tt_flower_to_start_grain", "dd", 0.0, 1e6);

   tt_start_to_end_grain.read(s, cultivar
                          , "x_pp_start_to_end_grain", "h", 0.0, 24.0
                          , "y_tt_start_to_end_grain", "dd", 0.0, 1e6);

   s->readParameter (cultivar
                    , "tt_end_grain_to_maturity"//, "()"
                    , tt_end_grain_to_maturity
                    , 0.0, 1e6);

   s->readParameter (cultivar
                    , "tt_maturity_to_ripe"//, "()"
                    , tt_maturity_to_ripe
                    , 0.0, tt_maturity_to_ripe_ub);
   }

void TTTPhenology::readSpeciesParameters (protocol::Component *s, vector<string> &sections)
   {
   CropPhenology::readSpeciesParameters (s, sections);

   vernal_days.search(s, sections,
                       "x_vernal_temp", "(oc)", -10., 60.0,
                       "y_vernal_days", "(days)", 0.0, 1.0);

   s->readParameter (sections
                   ,"tt_emerg_to_endjuv_ub"//, "()"
                   , tt_emerg_to_endjuv_ub
                   , 0.0, 1.e6);

   s->readParameter (sections
                   ,"tt_maturity_to_ripe_ub"//, "()"
                   , tt_maturity_to_ripe_ub
                   , 0.0, 1.e6);

}





void TTTPhenology::writeCultivarInfo (PlantComponent *systemInterface)
   {
   string s;
   s =  "   est_days_emerg_to_init     = " + itoa(est_days_emerg_to_init) + " (days)\n";
   s += tt_emerg_to_endjuv.description();
   s += tt_endjuv_to_init.description();
   s += tt_init_to_flower.description();
   s += tt_flower_to_start_grain.description();
   s += tt_start_to_end_grain.description();
   s += "   tt_end_grain_to_maturity   = " + ftoa(tt_end_grain_to_maturity, "10.0") + " (dd)\n";
   s += "   tt_maturity_to_ripe        = " + ftoa(tt_maturity_to_ripe, "10.0") + " (dd)";
   systemInterface->writeString (s.c_str());
   }

float TTTPhenology::TT(const environment_t &e)
   {
        return linint_3hrly_temp (e.maxt, e.mint, &y_tt);
   }

//+  Purpose
//     Use temperature, photoperiod and genetic characteristics
//     to determine when the crop begins a new growth phase.
//     The initial daily thermal time and height are also set.

void TTTPhenology::process (const environment_t &e, const pheno_stress_t &ps)
   {
   float phase_devel, new_stage;

   dlt_tt = TT(e);

   if (inPhase("sowing"))
      {
      dlt_tt_phenol = dlt_tt;
      // can't germinate on same day as sowing, because we would miss out on
      // day of sowing elsewhere.
      phase_devel = 0.999;
      if (das == 0)
         {
         phase_devel = 0.999;
         }
      else if ( plant_germination(pesw_germ, sowing_depth, e) )
         {
      	phase_devel =  1.999;
         }
      new_stage = floor(currentStage) + phase_devel;
      }


   else if (inPhase("germination"))
      {
      int layer_no_seed = e.find_layer_no (sowing_depth);
      float fasw_seed = divide (e.sw_dep[layer_no_seed] - e.ll_dep[layer_no_seed],
                                e.dul_dep[layer_no_seed] - e.ll_dep[layer_no_seed], 0.0);
      fasw_seed = bound (fasw_seed, 0.0, 1.0);

      dlt_tt_phenol = dlt_tt * rel_emerg_rate[fasw_seed];
      const pPhase *current = phases[currentStage];
      float a =  current->getTT() + dlt_tt_phenol;
      float b =  current->getTTTarget();
      phase_devel = divide(a, b, 1.0);
      new_stage = floor(currentStage) + phase_devel;
      }
   else if (inPhase("emergence2floral_initiation"))
      {
      dlt_tt_phenol = dlt_tt * min(ps.swdef, ps.nfact);
      const pPhase *current = phases[currentStage];
      float a =  current->getTT() + dlt_tt_phenol;
      float b =  current->getTTTarget();
      phase_devel = divide(a, b, 1.0);
      new_stage = floor(currentStage) + phase_devel;
      }
    else if (inPhase("flowering"))
      {
      dlt_tt_phenol = dlt_tt *  ps.swdef_flower;          //no nstress
      const pPhase *current = phases[currentStage];
      float a =  current->getTT() + dlt_tt_phenol;
      float b =  current->getTTTarget();
      phase_devel = divide(a, b, 1.0);
      new_stage = floor(currentStage) + phase_devel;
      }
    else if (inPhase("start_grain_fill2harvest_ripe"))
      {
      dlt_tt_phenol = dlt_tt *  ps.swdef_grainfill;       //no nstress
      const pPhase *current = phases[currentStage];
      float a =  current->getTT() + dlt_tt_phenol;
      float b =  current->getTTTarget();
      phase_devel = divide(a, b, 1.0);
      new_stage = floor(currentStage) + phase_devel;
      }
   else
      {
      // ??Hmmm. should probably stop dead here??
      dlt_tt_phenol = dlt_tt;
      phase_devel = 0.0;
      new_stage = floor(currentStage) + phase_devel;
      }

   dltStage = new_stage - currentStage;

   /// accumulate() to objects
   float value = dlt_tt_phenol;             //  (INPUT) value to add to array
   float p_index = currentStage;           //  (INPUT) current p_index no
   float dlt_index = dltStage;       //  (INPUT) increment in p_index no

   {
   int current_index;           // current index number ()
   float fract_in_old;           // fraction of value in last index
   float index_devel;            // fraction_of of current index elapsed ()
   int new_index;                // number of index just starting ()
   float portion_in_new;         // portion of value in next index
   float portion_in_old;         // portion of value in last index

   current_index = int(p_index);

   // make sure the index is something we can work with
   index_devel = p_index - floor(p_index) + dlt_index;
   if (index_devel >= 1.0)
      {
      // now we need to divvy
      new_index = (int) (p_index + min (1.0, dlt_index));
      if (reals_are_equal(fmod(p_index,1.0),0.0))
         {
         fract_in_old = 1.0 - divide(index_devel - 1.0, dlt_index, 0.0);
         portion_in_old = fract_in_old * (value + phases[current_index]->getTT())-
                              phases[current_index]->getTT();
         }
      else
         {
         fract_in_old = 1.0 - divide(index_devel - 1.0, dlt_index, 0.0);
         portion_in_old = fract_in_old * value;
         }
      portion_in_new = value - portion_in_old;
      phases[current_index]->add(fract_in_old, portion_in_old);
      phases[new_index]->add(1.0-fract_in_old, portion_in_new);
      }
   else
      {
      phases[current_index]->add(1.0, value);
      }
   }
   if (phase_devel >= 1.0)
      currentStage = floor(currentStage + 1.0);
   else
      currentStage = new_stage;

   if ((unsigned int)currentStage >= phases.size() || currentStage < 0.0)
     throw std::runtime_error("stage has gone wild in TTTPhenology::process()..");

   if ((int)currentStage != (int)previousStage) plant->doPlantEvent(phases[(int)currentStage]->name());
   cumvd += dlt_cumvd;
   das++;
   }

void TTTPhenology::onRemoveBiomass(float removeBiomPheno)
{
   if (initialOnBiomassRemove == true)
   {
      initialOnBiomassRemove = false;
      y_removeFractPheno.search(parentPlant, iniSectionList,
               "x_removeBiomPheno", "()", 0.0, 1.0,
               "y_removeFractPheno", "()", 0.0, 1.0);
   }
   else
   {     // parameters already read - do nothing
   }


//   float ttCritical = max(0.0, ttInPhase("above_ground") - ttInPhase("emergence"));
   float ttCritical = ttInPhase("above_ground");
   float removeFractPheno = y_removeFractPheno[removeBiomPheno];
   float removeTTPheno = ttCritical * removeFractPheno;

   ostrstream msg;
   msg << "Phenology change:-" << endl;
   msg << "    Fraction DM removed  = " << removeBiomPheno << endl;
   msg << "    Fraction TT removed  = " << removeFractPheno << endl;
   msg << "    Critical TT          = " << ttCritical << endl;
   msg << "    Remove TT            = " << removeTTPheno << endl;

   float ttRemaining = removeTTPheno;
   vector <pPhase*>::reverse_iterator rphase;
   for (rphase = phases.rbegin(); rphase !=  phases.rend(); rphase++)
   {
      pPhase* phase = *rphase;
      if (!phase->isEmpty())
      {
         float ttCurrentPhase = phase->getTT();
         if (ttRemaining > ttCurrentPhase)
         {
            phase->reset();
            ttRemaining -= ttCurrentPhase;
            currentStage -= 1.0;
         }
         else
         {
            phase->add(0.0, -ttRemaining);
            currentStage = (phase_fraction(0.0) + floor(currentStage));
            //ttRemaining = 0.0; /* not used */
            break;
         }
      }
      else
      { // phase is empty - not interested in it
      }
   }
   msg << "New Above ground TT = " << ttInPhase("above_ground") << endl << ends;
   if (plant->removeBiomassReport())
      parentPlant->writeString (msg.str());

}

void TTTPhenology::prepare (const environment_t &e)
   {
   CropPhenology::prepare(e);
   photoperiod = e.daylength (twilight);

   updateTTTargets(e);
   }

void TTTPhenology::doRegistrations (protocol::Component *s)
   {
   CropPhenology::doRegistrations(s);
   parentPlant->addGettableVar("dlt_cumvd", dlt_cumvd,   "", "Todays vd");
   }