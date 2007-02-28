#include <stdio.h>
#include <math.h>
#include <vector>
#include <string>
#include <stdexcept>
#include <ComponentInterface/MessageDataExt.h>
#include <ComponentInterface/Component.h>
#include <ComponentInterface/datatypes.h>
#include <ComponentInterface/ApsimVariant.h>

#include "PlantComponent.h"
#include "PlantLibrary.h"
#include "PlantInterface.h"
#include "PlantPhenology.h"
#include "Environment.h"
#include "FixedPhase.h"
#include "VernalPhase.h"
#include "PhotoPhase.h"
#include "EmergentPhase.h"
#include "LeafAppPhase.h"
#include "TTTPhenology.h"
#include "TTTRatePhenology.h"
#include "GenericPhenology.h"
#include "WheatPhenology.h"
#include "BroccoliPhenology.h"

#include "Utility/OutputVariable.h"
#include "iostream.h"

PlantPhenology * constructPhenology(plantInterface *plant, const string &name)
    {
    class PlantPhenology *phenology = NULL;
    if (name == "")
       throw std::invalid_argument("The parameter 'phenology_model'\nisn't in your ini file.\n\nGet one.\n");
    else if (name == "legume")
       phenology = new TTTPhenology(plant);
    else if (name == "tttrate")
       phenology = new TTTRatePhenology(plant);
    else if (name == "generic")
       phenology = new GenericPhenology(plant);
    else if (name == "wheat")
       phenology = new WheatPhenology(plant);
    else if (name == "broccoli")
       phenology = new BroccoliPhenology(plant);
    else
       throw std::invalid_argument("Unknown phenology model '" + name + "'");
    return phenology;
    }

PlantPhenology::PlantPhenology(plantInterface *p)
   {
   plant = p;          // "Plant" interface
   }

void PlantPhenology::readConstants (protocol::Component *s, const string &section)
{

   phases.push_back(new pPhase("out"));
   currentStage = 0.0;
   initialOnBiomassRemove = true;

   // Read the sequential list of stage names
   string scratch = s->readParameter(section, "stage_names");
   string ptypes = s->readParameter(section, "phase_type");
   string pnames = s->readParameter(section, "phase_names");

   vector<string> stage_names;
   vector<string> phase_types;
   vector<string> phase_names;
   Split_string(scratch, " ", stage_names);
   Split_string(ptypes, " ", phase_types);
   Split_string(pnames, " ", phase_names);

   if (stage_names.size() == 0) throw std::runtime_error("No stage names found");
   for (unsigned i=0;i!=phase_names.size();i++)
      {
      if(phase_types[i]=="generic")
         {
         phases.push_back(new pPhase(phase_names[i]));
         }
      else if(phase_types[i]=="vernal")
         {
         VernalPhase* vernal = new VernalPhase(phase_names[i]);
         phases.push_back(vernal);
         }
      else if(phase_types[i]=="photo")
         {
         PhotoPhase* photo = new PhotoPhase(phase_names[i]);
         phases.push_back(photo);
         }
      else if(phase_types[i]=="emergent")
         {
         EmergentPhase* emerg = new EmergentPhase(phase_names[i]);
         phases.push_back(emerg);
         }
      else if(phase_types[i]=="leafapp")
         {
         LeafAppPhase* leafapp = new LeafAppPhase(phase_names[i]);
         phases.push_back(leafapp);
         }
      else
         {
         pPhase* newPhase = new FixedPhase(phase_names[i]);
         phases.push_back(newPhase);
         }
      }

   //XX composites need to be defined as "start stage, end stage" pairs.
   // find composite phases that we care about
   scratch = s->readParameter(section, "composite_phases");
   vector<string> composite_names;
   Split_string(scratch, " ", composite_names);
   for (vector<string>::iterator name = composite_names.begin();
        name !=  composite_names.end();
        name++)
      {
      compositePhase composite;
      scratch = s->readParameter(section, *name);
      vector<string> composite_names;
      Split_string(scratch, " ", composite_names);
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


   for (unsigned i = 0; i != phases.size(); i++)
      {
   // Register stage names as events (eg. flowering)
      s->addRegistration(RegistrationType::event, phases[i]->name().c_str(),
                         nullTypeDDML, "", "");

      std::vector <Output*> Outputs;
      phases[i]->GetOutputs(Outputs);
      for (unsigned o=0; o!=Outputs.size();o++)
         {
         OutputVariable *Variable = dynamic_cast<OutputVariable*> (Outputs[o]);
         cout << Variable ;
         s->addGettableVar(Outputs[o]->Name.c_str(), *((float*)Variable->Variable), Outputs[o]->Units.c_str(), Outputs[o]->Description.c_str());
         delete Outputs[o];
         }

      }
};

pPhase* PlantPhenology::find(const string& PhaseName)
   {
   for(unsigned i=0; i!=phases.size();i++)
      {
      if(phases[i]->name()==PhaseName)
         return phases[i];
      }
      return NULL;
   }

void PlantPhenology::doRegistrations (protocol::Component *s)
{

   s->addGettableVar("stage", currentStage, "", "Plant stage");
   s->addGettableVar("dlt_stage", dltStage, "", "Change in plant stage");
   setupGetFunction(s, "stage_name", protocol::DTstring, false,
                    &PlantPhenology::get_stage_name, "", "Plant stage name");
   setupGetFunction(s, "stage_code", protocol::DTint4, false,
                    &PlantPhenology::get_stage_code, "", "Plant stage code");
   setupGetFunction(s, "phase_tt", protocol::DTsingle, true,
                    &PlantPhenology::get_phase_tt, "dd", "Thermal time target for each crop phase");
   setupGetFunction(s, "tt_tot", protocol::DTsingle, true,
                    &PlantPhenology::get_tt_tot, "dd", "Thermal time spent in each crop stage");
   setupGetFunction(s, "days_tot",protocol::DTsingle, true,
                    &PlantPhenology::get_days_tot, "days", "Days spent in each crop stage");

}



void PlantPhenology::readSpeciesParameters (protocol::Component *s, vector<string> &sections)
   {
   s->readParameter (sections
                      , "twilight"//, "(o)"
                      , twilight
                      , -90.0, 90.0);
   }

void PlantPhenology::zeroDeltas(void)
   {
   dltStage = 0;
   }

void PlantPhenology::prepare(const environment_t &/* sw*/)
   {
   previousStage = currentStage;
   }

void PlantPhenology::update(void)
   {
   phases[(int)currentStage]->update();
   }


// Is it the first day of a stage?
bool PlantPhenology::on_day_of(const string &stageName)
   {
   const pPhase *trial = getStage(stageName);
   if (trial == NULL) return false;

   const pPhase *current = phases[(int)currentStage];
   if (*current == *trial)
      return (trial->isFirstDay());
   return false;
   }

// Are we currently in a certain phase?
bool PlantPhenology::inPhase(const string &phase_name)
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

pPhase *PlantPhenology::getStage(const string &name)
   {
	pPhase *pos = find(name);
   // Do not check in with this commented out - NIH
	//if (pos == NULL) throw std::runtime_error(string("Can't find stage ") + name);
   return pos;
   }

int PlantPhenology::daysInCurrentPhase(void)
   {
   const pPhase *current = phases[(int)currentStage];
   return ((int) current->getDays());
   }

float PlantPhenology::ttInPhase(const string &phaseName)
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
float PlantPhenology::TTTargetInPhase(const string &phaseName)
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
float PlantPhenology::ttInCurrentPhase(void)
   {
	const pPhase *current = phases[(int)currentStage];
	return ((int)current->getTT());
   }

int PlantPhenology::daysInPhase(const string &phaseName)
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

string PlantPhenology::stageName(void)
   {
   unsigned int stage_no = (unsigned int) currentStage;
   return string(phases[stage_no]->name());
   }
string PlantPhenology::stageName(int n)
   {
   return phases[n]->name();
   }
string PlantPhenology::previousStageName(void)
   {
   unsigned int stage_no = (unsigned int) previousStage;
   return string(phases[stage_no]->name());
   }

//  Purpose
//       Return fraction of thermal time we are through the current
//       phenological phase (0-1)
float PlantPhenology::phase_fraction(float dlt_tt) //(INPUT)  daily thermal time (growing degree days)
   {
   const pPhase *current = phases[(int) currentStage];

   float dividend = current->getTT() + dlt_tt;
   float divisor = current->getTTTarget();
   float result = divide (dividend, divisor, 0.0);
   result = bound(result, 0.0, 1.0);
   return result;
   }
void PlantPhenology::zeroAllGlobals(void)
   {
   previousStage = currentStage = 0.0;
   for (unsigned int i=0; i < phases.size(); i++) phases[i]->reset();
   day_of_year = 0;
   }

//+  Purpose
//       Return an interpolated stage code from a table of stage_codes
//       and a nominated stage number. Returns the first or last table value if the stage number is not
//       found. Interpolation is done on thermal time.
// Hmmmmmm. This is very dodgy.
float PlantPhenology::stageCode (void)
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


//////////////////////GetVariable etc////////////////////////////
void PlantPhenology::get_stage_name(protocol::Component *s, protocol::QueryValueData &qd)
   {
   unsigned int stage_no = (unsigned int) currentStage;
   s->sendVariable(qd, phases[stage_no]->name());
   }
void PlantPhenology::get_stage_code(protocol::Component *s, protocol::QueryValueData &qd)
   {
   int stage_no = (int) currentStage;
   s->sendVariable(qd, stage_no);
   }

// NB. the 0'th element in these arrays is the "out" stage.
// For backward compatibility, don't report it.
void PlantPhenology::get_phase_tt(protocol::Component *s, protocol::QueryValueData &qd)
   {
   vector<float> t;
   for(unsigned int i=1; i < phases.size(); i++) t.push_back(phases[i]->getTTTarget());
   s->sendVariable(qd, t);
   }
void PlantPhenology::get_tt_tot(protocol::Component *s, protocol::QueryValueData &qd)
   {
   vector<float> t;
   for(unsigned int i=1; i < phases.size(); i++) t.push_back(phases[i]->getTT());
   s->sendVariable(qd, t);
   }
void PlantPhenology::get_days_tot(protocol::Component *s, protocol::QueryValueData &qd)
   {
   vector<float> t;
   for(unsigned int i=1; i < phases.size(); i++) t.push_back(phases[i]->getDays());
   s->sendVariable(qd, t);
   }

