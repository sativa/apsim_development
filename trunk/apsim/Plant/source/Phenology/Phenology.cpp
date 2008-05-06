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
#include "WaitingPhase.h"
#include "CWEmergentPhase.h"
#include "CWInductivePhase.h"
#include "CWFixedPhase.h"
#include "CWSowingPhase.h"

#include "Utility/OutputVariable.h"
#include <general/string_functions.h>

Phenology::Phenology(ScienceAPI& api, plantInterface& p)
   : plantThing(api, "Phenology"), plant(p)
   {
   // --------------------------------------------------------------------------
   // Constructor.
   // --------------------------------------------------------------------------
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
void Phenology::onInit1(protocol::Component *)
   {
   // --------------------------------------------------------------------------
   // This will remove all existing phases and recreate them. It
   // will also register our variables.
   // --------------------------------------------------------------------------
   scienceAPI.expose("Stage", "", "Plant stage", currentStage);
   scienceAPI.exposeWritable("Phase", "", "Plant stage", FloatSetter(&Phenology::onSetStage));
   scienceAPI.expose("DeltaStage", "", "Change in plant stage", dltStage);
   scienceAPI.exposeFunction("StageName", "", "Plant stage name", StringFunction(&Phenology::stageName));
   scienceAPI.exposeFunction("TT", "deg. day", "Todays thermal time", FloatFunction(&Phenology::get_dlt_tt));

   scienceAPI.subscribe("harvest", NullFunction(&Phenology::onHarvest));
   scienceAPI.subscribe("end_crop", NullFunction(&Phenology::onEndCrop));
   scienceAPI.subscribe("kill_stem", NullFunction(&Phenology::onKillStem));
   currentStage = 0.0;
   initialise();
   }

void Phenology::initialise()
   {
   clear();
   phases.push_back(new pPhase(scienceAPI, plant, "out"));

   // Read the sequential list of stage names
   vector<string> phase_types;
   vector<string> phase_names;
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
      else if(phase_types[i]=="waiting")
         newPhase = new WaitingPhase(scienceAPI, plant, phase_names[i]);
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
      std::vector<string> compositeNames;
      scienceAPI.read(*name, compositeNames);
      float startFraction = 0.0;
      float endFraction = 1.0;
      for (unsigned p = 0; p != compositeNames.size(); p++)
         {
         string bracketedValue = splitOffBracketedValue(compositeNames[p], '(', ')');
         if (bracketedValue != "")
            {
            if (p == 0)
               startFraction = atof(bracketedValue.c_str());
            else if (p == compositeNames.size()-1)
               endFraction = atof(bracketedValue.c_str());
            else
               throw runtime_error("A composite phase can only have a fraction on the first or last phase");
            }

         pPhase *phase = find(compositeNames[p]);
         if (phase != NULL)
           composite.add(phase);
         else
           throw std::invalid_argument("Unknown phase name '" + compositeNames[p] + "'");
         }
      composite.setStartEndFractions(startFraction, endFraction);
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
      if(Str_i_Eq(phases[i]->name(), PhaseName))
         return phases[i];
      }
      return NULL;
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

string Phenology::description()
   {
   return " stage " + ftoa(floor(currentStage), 1) + " " + stageName();
   }
void Phenology::writeSummary()
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
   	 return phase.contains(*phases[(int)currentStage], fractionInCurrentPhase());

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
string Phenology::stageName(void)
   {
   unsigned int stage_no = (unsigned int) currentStage;
   return string(phases[stage_no]->name());
   }

float Phenology::fractionInCurrentPhase() const
   {
   return currentStage - int(currentStage);
   }
void Phenology::zeroAll(void)
   {
   previousStage = currentStage = 0.0;
   for (unsigned int i=0; i < phases.size(); i++) phases[i]->reset();
   day_of_year = 0;
   zeroDeltas();
   das = 0;
   }
void Phenology::onSetStage(float resetPhase)
   {
   bound_check_real_var(scienceAPI, resetPhase, 1.0, 11.0, "stage");
   if (!inPhase("out"))
      {
      ostringstream msg;
      msg << "Phenology change:-" << endl;
      msg << "    Reset Phase  = " << resetPhase << endl;
      msg << "    Old Phase    = " << currentStage << endl;

      vector <pPhase*>::reverse_iterator rphase;
      for (rphase = phases.rbegin(); rphase !=  phases.rend(); rphase++)
         {
         pPhase* phase = *rphase;
         if (phase->daysInPhase() > 0)
            {
            if (floor(currentStage) > floor(resetPhase))
               {
               phase->reset();
               currentStage -= 1.0;
               if (currentStage < 4.0)  //FIXME - hack to stop onEmergence being fired which initialises biomass parts
                  {
                  currentStage = 4.0;
                  break;
                  }
               }
            else
               {
               if (floor(currentStage) == floor(resetPhase))
                  {
                  currentStage = resetPhase;
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
      }
   }

void Phenology::onHarvest()
//=======================================================================================
   {
   string existingStage = stageName();
   lookupFunction stage_reduction_harvest;
   stage_reduction_harvest.read(scienceAPI,
                                "stage_code_list" , "()", 1.0, 100.0,
                                "stage_stem_reduction_harvest" , "()", 1.0, 100.0);

   previousStage = currentStage;
   currentStage = stage_reduction_harvest[currentStage];
   
   for (unsigned int stage = (int) currentStage; stage != phases.size(); stage++)
      phases[stage]->reset();
   setupTTTargets();
   if (existingStage != stageName())
      plant.doPlantEvent(existingStage, phases[(int)currentStage]->name(), true);
   }

void Phenology::onKillStem()
//=======================================================================================
   {
   string existingStage = stageName();
   lookupFunction stage_reduction_kill_stem;
   stage_reduction_kill_stem.read(scienceAPI,
                                  "stage_code_list" , "()", 1.0, 100.0,
                                  "stage_stem_reduction_kill_stem" , "()", 1.0, 100.0);

   previousStage = currentStage;
   currentStage = stage_reduction_kill_stem[currentStage];
   for (unsigned int stage = (int)currentStage; stage != phases.size(); stage++)
      phases[stage]->reset();
   setupTTTargets();
   if (existingStage != stageName())
      plant.doPlantEvent(existingStage, phases[(int)currentStage]->name(), true);
   }

void Phenology::onSow(protocol::Variant &v)
//=======================================================================================
   {
   //initialise();
   
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
   plant.doPlantEvent(phases[0]->name(), phases[(int)currentStage]->name(), false);
   }

void Phenology::onEndCrop()
   {
   zeroAll();
   }

void Phenology::process()
   {
   string existingStage = stageName();
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
   // Add a new day to all phases up to but not including the current phase.
   for (int i = 1; i <= (int)currentStage; i++)
      phases[i]->addToAfter(1, dlt_tt_phenol);

   if (phase_devel >= 1.0)
      currentStage = floor(currentStage + 1.0);
   else
      currentStage = new_stage;

   if ((unsigned int)currentStage >= phases.size() || currentStage < 0.0)
     throw std::runtime_error("stage has gone wild in Phenology::process()..");

   if (existingStage != stageName())
      plant.doPlantEvent(existingStage, phases[(int)currentStage]->name(), false);

   das++;
   }


void Phenology::onRemoveBiomass(float removeBiomPheno)
   {
   string existingStage = stageName();
   
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
      if (phase->daysInPhase() > 0)
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
            // Return fraction of thermal time we are through the current
            // phenological phase (0-1)
            const pPhase *current = phases[(int) currentStage];
            float frac = divide(current->getTT(), current->getTTTarget(), 0.0);

            currentStage = frac + floor(currentStage);
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
   if (existingStage != stageName())
      plant.doPlantEvent(existingStage, phases[(int)currentStage]->name(), true);
   }

float Phenology::doInterpolation(externalFunction& f)
   {
   return f.value(currentStage);
   }

float Phenology::doLookup(const std::vector<float>& f)
   {
   return f[int(currentStage)-1];
   }

