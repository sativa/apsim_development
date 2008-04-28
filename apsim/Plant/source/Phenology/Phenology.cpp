#include "StdPlant.h"

#include "Phenology.h"
#include "Environment.h"
#include "FixedPhase.h"
#include "VernalPhase.h"
#include "InductivePhase.h"
#include "PhotoPhase.h"
#include "EmergentPhase.h"
#include "LeafAppPhase.h"
#include "SowingPhase.h"
#include "CWEmergentPhase.h"
#include "CWInductivePhase.h"
#include "CWFixedPhase.h"
#include "CWSowingPhase.h"

#include "Utility/OutputVariable.h"

Phenology::Phenology(ScienceAPI& api, plantInterface& p)
   : plantThing(api, "Phenology"), plant(p)
   {
   // --------------------------------------------------------------------------
   // Constructor.
   // --------------------------------------------------------------------------
   initialise();
   }
Phenology::~Phenology()
   {
   // --------------------------------------------------------------------------
   // Destructor.
   // --------------------------------------------------------------------------
   clear();
   }
void Phenology::clear()
   {
   // --------------------------------------------------------------------------
   // Clear out all phases.
   // --------------------------------------------------------------------------
   for (unsigned i = 0; i != phases.size(); i++)
      delete phases[i];
   phases.clear();
   }
void Phenology::initialise()
   {
   // --------------------------------------------------------------------------
   // This will remove all existing phases and recreate them. It
   // will also register our variables.
   // --------------------------------------------------------------------------
   scienceAPI.expose("stage", "", "Plant stage", currentStage);
   scienceAPI.expose("dlt_stage", "", "Change in plant stage", dltStage);
   scienceAPI.exposeFunction("stage_name", "", "Plant stage name", StringFunction(&Phenology::stageName));
   scienceAPI.expose("das", "day", "Days after sowing", das);
   scienceAPI.expose("dlt_tt_phenol", "deg. day", "Todays thermal time (incl. stress factors)", dlt_tt_phenol);
   scienceAPI.exposeFunction("dlt_tt", "dd", "Todays thermal time (no stress factors)", FloatFunction(&Phenology::get_dlt_tt));

   scienceAPI.subscribe("harvest", NullFunction(&Phenology::onHarvest));
   scienceAPI.subscribe("end_crop", NullFunction(&Phenology::onEndCrop));
   scienceAPI.subscribe("kill_stem", NullFunction(&Phenology::onKillStem));

   clear();
   phases.push_back(new pPhase(scienceAPI, plant, "out"));
   currentStage = 0.0;

   // Read the sequential list of stage names
   vector<string> stage_names;
   vector<string> phase_types;
   vector<string> phase_names;
   scienceAPI.read("stage_names", stage_names);
   scienceAPI.read("phase_type", phase_types);
   scienceAPI.read("phase_names", phase_names);

   for (unsigned i=0;i!=phase_names.size();i++)
      {
      pPhase* newPhase;
      if(phase_types[i]=="sowing")
         newPhase = new SowingPhase(scienceAPI, plant, phase_names[i]);
      else if(phase_types[i]=="generic")
         newPhase = new pPhase(scienceAPI, plant, phase_names[i]);
      else if(phase_types[i]=="vernal")
         newPhase = new VernalPhase(scienceAPI, plant, phase_names[i]);
      else if(phase_types[i]=="photo")
         newPhase = new PhotoPhase(scienceAPI, plant, phase_names[i]);
      else if(phase_types[i]=="emergent")
         newPhase = new EmergentPhase(scienceAPI, plant, phase_names[i]);
      else if(phase_types[i]=="inductive")
         newPhase = new InductivePhase(scienceAPI, plant, phase_names[i]);
      else if(phase_types[i]=="leafapp")
         newPhase = new LeafAppPhase(scienceAPI, plant, phase_names[i]);
      else if(phase_types[i]=="fixed")
         newPhase = new FixedPhase(scienceAPI, plant, phase_names[i]);
      else if(phase_types[i]=="cwemergent")
         newPhase = new CWEmergentPhase(scienceAPI, plant, phase_names[i]);
      else if(phase_types[i]=="cwinductive")
         newPhase = new CWInductivePhase(scienceAPI, plant, phase_names[i]);
      else if(phase_types[i]=="cwfixed")
         newPhase = new CWFixedPhase(scienceAPI, plant, phase_names[i]);
      else if(phase_types[i]=="cwsowing")
         newPhase = new CWSowingPhase(scienceAPI, plant, phase_names[i]);
      else
         throw runtime_error("Invalid phase type: " + phase_types[i]);

      phases.push_back(newPhase);
      }
   zeroAll();

   //XX composites need to be defined as "start stage, end stage" pairs.
   // find composite phases that we care about
   vector<string> composite_names;
   scienceAPI.read("composite_phases", composite_names);
   for (vector<string>::iterator name = composite_names.begin();
        name !=  composite_names.end();
        name++)
      {
      compositePhase composite;
      vector<string> composite_names;
      scienceAPI.read(*name, composite_names);
      for (vector<string>::iterator phase = composite_names.begin();
           phase !=  composite_names.end();
           phase++)
         {
         pPhase *p = find(*phase);
         if (p != NULL)
           composite.add(p);
         else
           throw std::invalid_argument("Unknown phase name '" + (*phase) + "'");
         }
      composites.insert(string2composite::value_type(*name,composite));
      }
   }

void Phenology::read()
   {
   // --------------------------------------------------------------------------
   // Re-read all parameters but don't reinitialise the phases.
   // --------------------------------------------------------------------------
   for(unsigned i=0; i!= phases.size();i++)
      phases[i]->read();
   }

pPhase* Phenology::find(const string& PhaseName) const
   {
   // --------------------------------------------------------------------------
   // Find the specified phase and return a pointer to it. Returns NULL if
   // not found.
   // --------------------------------------------------------------------------
   for(unsigned i=0; i!=phases.size();i++)
      {
      if(phases[i]->name()==PhaseName)
         return phases[i];
      }
      return NULL;
   }

void Phenology::update(void)
   {
   phases[(int)currentStage]->update();
   }

void Phenology::setupTTTargets(void)
//=======================================================================================
// static TT targets (called at sowing)
   {
   for(unsigned i=0; i!= phases.size();i++)
      {
      phases[i]->setupTTTarget();
      }
   }

void Phenology::write()
//=======================================================================================
   {
   string s;
   scienceAPI.write("   Phases:");
   for(unsigned i=0;i!=phases.size();i++)
      {
      s += "      " + phases[i]->name() +"\n";
      string desc = phases[i]->description();
      if (desc != "")
         s += desc;
      }
   scienceAPI.write(s);
   }

float Phenology::get_dlt_tt(void)
   {
   if (das > 0)
      return phases[(int)currentStage]->TT();
   else
      return 0;
   }


// Is it the first day of a stage?
bool Phenology::on_day_of(const string &stageName)
   {
   const pPhase *trial = getStage(stageName);
   if (trial == NULL) return false;

   const pPhase *current = phases[(int)currentStage];
   if (*current == *trial)
      return (trial->isFirstDay());
   return false;
   }

// Are we currently in a certain phase?
bool Phenology::inPhase(const string &phase_name) const
   {
   // See if it's a composite
   compositePhase phase = composites[phase_name];
   if (!phase.isEmpty())
   	 return phase.contains(*phases[(int)currentStage]);

   // No, see if the stage is known at all to us
   pPhase *test = find(phase_name);
   if (test == NULL)
      return false;
   else
      {
      const pPhase *current = phases[(int)currentStage];
      return(*current == *test);
      }
   }

pPhase *Phenology::getStage(const string &name)
   {
	pPhase *pos = find(name);
   // Do not check in with this commented out - NIH
	//if (pos == NULL) throw std::runtime_error(string("Can't find stage ") + name);
   return pos;
   }

int Phenology::daysInCurrentPhase(void)
   {
   const pPhase *current = phases[(int)currentStage];
   return ((int) current->getDays());
   }

float Phenology::ttInPhase(const string &phaseName) const
   {
      // See if it's a composite
      compositePhase phaseGroup = composites[phaseName];
      if (!phaseGroup.isEmpty())
      {
         return phaseGroup.getTT();
      }
      else
      {
         // No, see if the stage is known at all to us
         pPhase *phase = find(phaseName);
         if (phase == NULL)
         {
            throw std::runtime_error("unknown phase name " + phaseName);
         }
         else
         {
   	      return phase->getTT();
   	   }
   	}
   }
float Phenology::TTTargetInPhase(const string &phaseName) const
   {
      // See if it's a composite
      compositePhase phaseGroup = composites[phaseName];
      if (!phaseGroup.isEmpty())
      {
         return phaseGroup.getTTTarget();
      }
      else
      {
         // No, see if the stage is known at all to us
         pPhase *phase = find(phaseName);
         if (phase == NULL)
         {
            throw std::runtime_error("unknown phase name " + phaseName);
         }
         else
         {
   	      return phase->getTTTarget();
   	   }
   	}
   }
float Phenology::ttInCurrentPhase(void)
   {
	const pPhase *current = phases[(int)currentStage];
	return ((int)current->getTT());
   }

int Phenology::daysInPhase(const string &phaseName)
   {
      // See if it's a composite
      compositePhase phaseGroup = composites[phaseName];
      if (!phaseGroup.isEmpty())
      {
         return ((int)phaseGroup.getDays());
      }
      else
      {
         // No, see if the stage is known at all to us
         pPhase *phase = find(phaseName);
         if (phase == NULL)
         {
            throw std::runtime_error("unknown phase name3 " + phaseName);
         }
         else
         {
   	      return ((int)phase->getDays());
   	   }
   	}
   }

string Phenology::stageName(void)
   {
   unsigned int stage_no = (unsigned int) currentStage;
   return string(phases[stage_no]->name());
   }
string Phenology::previousStageName(void)
   {
   unsigned int stage_no = (unsigned int) previousStage;
   return string(phases[stage_no]->name());
   }

//  Purpose
//       Return fraction of thermal time we are through the current
//       phenological phase (0-1)
float Phenology::phase_fraction(float dlt_tt) //(INPUT)  daily thermal time (growing degree days)
   {
   const pPhase *current = phases[(int) currentStage];

   float dividend = current->getTT() + dlt_tt;
   float divisor = current->getTTTarget();
   float result = divide (dividend, divisor, 0.0);
   result = bound(result, 0.0, 1.0);
   return result;
   }
void Phenology::zeroAll(void)
   {
   previousStage = currentStage = 0.0;
   for (unsigned int i=0; i < phases.size(); i++) phases[i]->reset();
   day_of_year = 0;
   zeroDeltas();
   das = 0;
   }

//+  Purpose
//       Return an interpolated stage code from a table of stage_codes
//       and a nominated stage number. Returns the first or last table value if the stage number is not
//       found. Interpolation is done on thermal time.
// Hmmmmmm. This is very dodgy.
float Phenology::stageCode (void)
    {
    if (currentStage < 3.0) return 3.0;
    if (phases[(int)currentStage]->isFirstDay())
        {
        float tt_tot = phases[(int)currentStage]->getTT();
        float phase_tt = phases[(int)currentStage]->getTTTarget();
        float fraction_of = divide (tt_tot, phase_tt, 0.0);
        fraction_of = bound(fraction_of, 0.0, 0.999);
        return((int)currentStage +  fraction_of);
        }
    return currentStage;
    }

void Phenology::onSetPhase(float resetPhase)
{

   ostringstream msg;
   msg << "Phenology change:-" << endl;
   msg << "    Reset Phase  = " << resetPhase << endl;
   msg << "    Old Phase    = " << currentStage << endl;

   vector <pPhase*>::reverse_iterator rphase;
   for (rphase = phases.rbegin(); rphase !=  phases.rend(); rphase++)
   {
      pPhase* phase = *rphase;
      if (!phase->isEmpty())
      {
         if (floor(currentStage) > floor(resetPhase))
         {
            phase->reset();
            currentStage -= 1.0;
//            previousStage = currentStage;
            if (currentStage < 4.0)  //FIXME - hack to stop onEmergence being fired which initialises biomass parts
            {
               currentStage = 4.0;
//               previousStage = currentStage;
               break;
            }
         }
         else
         {
            if (floor(currentStage) == floor(resetPhase))
            {
               currentStage = resetPhase;
//               previousStage = currentStage;
               float ttPhase = phase->getTTTarget() * (currentStage - floor(currentStage));
               phase->setTT(ttPhase);
               break;
            }
            else
            {
               msg << "    Unable set to a higher phase" << endl;
               plant.getComponent()->writeString (msg.str().c_str());
               break;
            } // Trying to set to a higher phase so do nothing
         }
      }
      else
      { // phase is empty - not interested in it
      }
   }
   msg << "    New Phase  = " << currentStage << endl << ends;
//   if (plant->removeBiomassReport())
//      plant->getComponent()->writeString (msg.str().c_str());

}

void Phenology::onHarvest()
//=======================================================================================
   {
   lookupFunction stage_reduction_harvest;
   stage_reduction_harvest.read(scienceAPI,
                                "stage_code_list" , "()", 1.0, 100.0,
                                "stage_stem_reduction_harvest" , "()", 1.0, 100.0);

   previousStage = currentStage;
   currentStage = stage_reduction_harvest[currentStage];
   for (unsigned int stage = (int) currentStage; stage != phases.size(); stage++)
      phases[stage]->reset();
   setupTTTargets();
   }

void Phenology::onKillStem()
//=======================================================================================
   {
   lookupFunction stage_reduction_kill_stem;
   stage_reduction_kill_stem.read(scienceAPI,
                                  "stage_code_list" , "()", 1.0, 100.0,
                                  "stage_stem_reduction_kill_stem" , "()", 1.0, 100.0);

   previousStage = currentStage;
   currentStage = stage_reduction_kill_stem[currentStage];
   for (unsigned int stage = (int)currentStage; stage != phases.size(); stage++)
      phases[stage]->reset();
   setupTTTargets();
   }

void Phenology::onSow(protocol::Variant &v)
//=======================================================================================
   {
   protocol::ApsimVariant incomingApsimVariant(plant.getComponent());
   incomingApsimVariant.aliasTo(v.getMessageData());

   float sowing_depth;
   if (incomingApsimVariant.get("sowing_depth", protocol::DTsingle, false, sowing_depth) == false)
      throw std::invalid_argument("sowing_depth not specified");
   bound_check_real_var(scienceAPI, sowing_depth, 0.0, 100.0, "sowing_depth");
   currentStage = 1.0;
   das = 0;
   for(unsigned i=0; i!= phases.size();i++)
      phases[i]->OnSow(sowing_depth);
   setupTTTargets();
   }

void Phenology::onEndCrop()
   {
   zeroAll();
   }

void Phenology::process()
   {
   dltStage = 0;
   dlt_tt = 0.0;
   dlt_tt_phenol  = 0.0;
   previousStage = currentStage;
   for(unsigned i=0; i!= phases.size();i++)
      phases[i]->updateTTTargets(*this);

   float phase_devel, new_stage;
   for(unsigned i=0; i!= phases.size();i++)
      phases[i]->process();

   phases[(int)currentStage]->calcPhaseDevelopment(das,
                                                   dlt_tt_phenol, phase_devel);

   new_stage = floor(currentStage) + phase_devel;

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

   // (implicit) assert(dlt_index <= 1.0);
   current_index = int(p_index);

   // make sure the index is something we can work with
   if(current_index >= 0)
      {
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
   }

   if (phase_devel >= 1.0)
      currentStage = floor(currentStage + 1.0);
   else
      currentStage = new_stage;

   if ((unsigned int)currentStage >= phases.size() || currentStage < 0.0)
     throw std::runtime_error("stage has gone wild in Phenology::process()..");

   if ((int)currentStage != (int)previousStage)
      plant.doPlantEvent(phases[(int)currentStage]->name());
   das++;
   }


void Phenology::onRemoveBiomass(float removeBiomPheno)
   {
   interpolationFunction y_removeFractPheno;
   y_removeFractPheno.read(scienceAPI,
               "x_removeBiomPheno", "()", 0.0, 1.0,
               "y_removeFractPheno", "()", 0.0, 1.0);

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
      if (!phase->isEmpty())
      {
         float ttCurrentPhase = phase->getTT();
         if (ttRemaining > ttCurrentPhase)
         {
            phase->reset();
            ttRemaining -= ttCurrentPhase;
            currentStage -= 1.0;
            if (currentStage < 4.0)  //FIXME - hack to stop onEmergence being fired which initialises biomass parts
            {
               currentStage = 4.0;
               break;
            }
         }
         else
         {
            phase->add(0.0, -ttRemaining);
            currentStage = (phase_fraction(0.0) + floor(currentStage));
            break;
         }
      }
      else
      { // phase is empty - not interested in it
      }
   }
   msg << "New Above ground TT = " << ttInPhase("above_ground") << endl << ends;
   if (plant.removeBiomassReport())
      scienceAPI.warning(msg.str());
   }

