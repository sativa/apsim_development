#include <ComponentInterface/Component.h>
#include <ComponentInterface/datatypes.h>
#include <ComponentInterface/ApsimVariant.h>
#include <ComponentInterface/MessageDataExt.h>
#include "PlantComponent.h"
#include "PlantLibrary.h"
#include "PlantInterface.h"
#include "PlantPhenology.h"
#include "GenericPhenology.h"
#include "Environment.h"
#include <iostream.h>
#include <sstream>

void GenericPhenology::zeroAllGlobals(void)
//=======================================================================================
   {
   CropPhenology::zeroAllGlobals();
   }


void GenericPhenology::readConstants (protocol::Component *s, const string &section)
//=======================================================================================
   {
   CropPhenology::readConstants(s, section);
   s->writeString("phenology model: Generic");
   }

void GenericPhenology::setupTTTargets(void)
//=======================================================================================
// static TT targets (called at sowing)
   {
   for(unsigned i=0; i!= phases.size();i++)
      {
      phases[i]->setupTTTarget();
      }
   }

void GenericPhenology::readCultivarParameters(protocol::Component *s, const string & cultivar)
//=======================================================================================
   {
   CropPhenology::readCultivarParameters(s, cultivar);
   for(unsigned i=0; i!= phases.size();i++)
      {
      phases[i]->readCultivarParameters(s, cultivar);
      }
   }

void GenericPhenology::onSow(unsigned &, unsigned &, protocol::Variant &v)
//=======================================================================================
   {
   protocol::ApsimVariant incomingApsimVariant(plant->getComponent());
   incomingApsimVariant.aliasTo(v.getMessageData());

   for(unsigned i=0; i!= phases.size();i++)
      {
      phases[i]->onSow(incomingApsimVariant);
      }
   setupTTTargets();
   currentStage = 1.0;
   das = 0;
   }
void GenericPhenology::readSpeciesParameters (protocol::Component *s, vector<string> &sections)
//=======================================================================================
   {
   CropPhenology::readSpeciesParameters (s, sections);
   for(unsigned i=0; i!= phases.size();i++)
      {
      phases[i]->readSpeciesParameters(s, sections);
      }
   }

void GenericPhenology::writeCultivarInfo (PlantComponent *systemInterface)
//=======================================================================================
   {
   string s;

   for(unsigned i=0;i!=phases.size();i++)
      {
      //s += "   tt_"+phases[i]->name()+" = "+ftoa(phases[i]->getTTTarget(), "10.0") + " (dd)\n";
      s += phases[i]->name() +"\n";
      s += phases[i]->description()+ " \n";
      }
   systemInterface->writeString (s.c_str());
   }

float GenericPhenology::TT(const environment_t &e)
   {
   return linint_3hrly_temp (e.maxt, e.mint, &y_tt);
   }

void GenericPhenology::process (const environment_t &e, const pheno_stress_t &ps,float fasw_seed, float pesw_seed)
//=======================================================================================
//     Use temperature, photoperiod and genetic characteristics
//     to determine when the crop begins a new growth phase.
//     The initial daily thermal time and height are also set.

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
      else if ( plant_germination(pesw_germ, sowing_depth, pesw_seed) )
         {
      	phase_devel =  1.999;
         }
      new_stage = floor(currentStage) + phase_devel;
      }
   else if (inPhase("germination"))
      {
      dlt_tt_phenol = dlt_tt * rel_emerg_rate[fasw_seed];
      const pPhase *current = phases[(int)currentStage];
      float a =  current->getTT() + dlt_tt_phenol;
      float b =  current->getTTTarget();
      phase_devel = divide(a, b, 1.0);
      new_stage = floor(currentStage) + phase_devel;
      }
   else
      {
      // ??Hmmm. should probably stop dead here??
      //dlt_tt_phenol = dlt_tt;
      //phase_devel = 0.0;
      //new_stage = floor(currentStage) + phase_devel;

      dlt_tt_phenol = dlt_tt;
      const pPhase *current = phases[(int)currentStage];
      float a =  current->getTT() + dlt_tt_phenol;
      float b =  current->getTTTarget();
      phase_devel = divide(a, b, 1.0);
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
     throw std::runtime_error("stage has gone wild in GenericPhenology::process()..");

   if ((int)currentStage != (int)previousStage) plant->doPlantEvent(phases[(int)currentStage]->name());

   das++;
   }

void GenericPhenology::onRemoveBiomass(float removeBiomPheno)
//=======================================================================================
   {
   if (initialOnBiomassRemove == true)
      {
      initialOnBiomassRemove = false;
      y_removeFractPheno.search(plant->getComponent(), iniSectionList,
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

   ostringstream msg;
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
      if (phase->isEmpty())
         {
         // Do nothing
         }
      else
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
      }
   msg << "New Above ground TT = " << ttInPhase("above_ground") << endl << ends;
   if (plant->removeBiomassReport())
      plant->getComponent()->writeString (msg.str().c_str());

   }

void GenericPhenology::prepare (const environment_t &e)
//=======================================================================================
   {
   CropPhenology::prepare(e);
   photoperiod = e.daylength (twilight);

   for(unsigned i=0; i!= phases.size();i++)
      phases[i]->updateTTTargets(*this,e);

   }


