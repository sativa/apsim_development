#include <stdio.h>
#include <math.h>
#include <vector>
#include <string>
#include <stdexcept>
#include <ComponentInterface/Component.h>
#include <ComponentInterface/dataTypes.h>
#include <ComponentInterface/ApsimVariant.h>
#include <ComponentInterface/MessageDataExt.h>

#include "PlantComponent.h"
#include "PlantLibrary.h"
#include "PlantPhenology.h"
#include "Environment.h"

PlantPhenology::PlantPhenology(PlantComponent *s, plantInterface *p)
   {
   plant = p;          // "Plant" interface
   parentPlant = s;    // "System" interface
   }

void PlantPhenology::readConstants (protocol::Component *s, const string &section)
{
   phases.push_back(pPhase("out"));
   currentStage = 0.0;
   initialOnBiomassRemove = true;
   removeBiomassReport = "off";

   // Read the sequential list of stage names
   string scratch = s->readParameter(section, "stage_names");
   vector<string> stage_names;
   Split_string(scratch, " ", stage_names);
   if (stage_names.size() == 0) throw std::runtime_error("No stage names found");
   for (vector<string>::iterator sn = stage_names.begin();
        sn !=  stage_names.end();
        sn++)
      {
      phases.push_back(*sn);
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
         pPhase *p = find(phases.begin(), phases.end(), *phase);
         if (p != phases.end())
           composite.add(p);
         else
           throw std::invalid_argument("Unknown phase name '" + (*phase) + "'");
         }
      composites.insert(string2composite::value_type(*name,composite));
      }

   // Register stage names as events (eg. flowering)
   for (unsigned i = 0; i != phases.size(); i++)
      {
      s->addRegistration(RegistrationType::event, phases[i].name().c_str(),
                         "", "", "");
      }
};

void PlantPhenology::doRegistrations (protocol::Component *s)
{

   parentPlant->addGettableVar("stage", currentStage, "", "Plant stage");
   parentPlant->addGettableVar("dlt_stage", dltStage, "", "Change in plant stage");
   setupGetFunction(parentPlant, "stage_name", protocol::DTstring, false,
                    &PlantPhenology::get_stage_name, "", "Plant stage name");
   setupGetFunction(parentPlant, "stage_code", protocol::DTint4, false,
                    &PlantPhenology::get_stage_code, "", "Plant stage code");
   setupGetFunction(parentPlant, "phase_tt", protocol::DTsingle, true,
                    &PlantPhenology::get_phase_tt, "dd", "Thermal time target for each crop phase");
   setupGetFunction(parentPlant, "tt_tot", protocol::DTsingle, true,
                    &PlantPhenology::get_tt_tot, "dd", "Thermal time spent in each crop stage");
   setupGetFunction(parentPlant, "days_tot",protocol::DTsingle, true,
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

void PlantPhenology::prepare(const environment_t &sw)
   {
   previousStage = currentStage;
   }

void PlantPhenology::update(void)
   {
   phases[(int)currentStage].update();
   }


// Is it the first day of a stage?
bool PlantPhenology::on_day_of(const string &stageName)
   {
   const pPhase *trial = getStage(stageName);
   const pPhase &current = phases[(int)currentStage];
   if (current == *trial)
      return (trial->isFirstDay());
   return false;
   }

// Are we currently in a certain phase?
bool PlantPhenology::inPhase(const string &phase_name)
   {
   // See if it's a composite
   compositePhase phase = composites[phase_name];
   if (!phase.isEmpty())
   	 return phase.contains(phases[(int)currentStage]);

   // No, see if the stage is known at all to us
   pPhase *test = find(phases.begin(), phases.end(), pPhase(phase_name));
   if (test == phases.end()) { throw std::runtime_error("unknown phase name " + phase_name);}
   const pPhase &current = phases[(int)currentStage];
   return(current == *test);
   }

pPhase *PlantPhenology::getStage(const string &name)
   {
   pPhase test(name);
   for (vector<pPhase>::iterator s = phases.begin();
        s != phases.end();
        s++)
       {
       if (*s == test) return s;
       }
   throw std::runtime_error(string("Can't find a stage called ") + name);
// Alternate:
//	pStage *pos = find(phases.begin(), phases.end(), pPhase(name));
//	if (pos == phases.end()) throw std::runtime_error(string("Can't find stage ") + stage);
//   return pos;
   }

int PlantPhenology::daysInCurrentPhase(void)
   {
   const pPhase &current = phases[currentStage];
   return current.getDays();
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
         pPhase *phase = find(phases.begin(), phases.end(), pPhase(phaseName));
         if (phase == phases.end())
         {
            throw std::runtime_error("unknown phase name " + phaseName);
         }
         else
         {
   	      return phase->getTT();
   	   }
   	}
   }

float PlantPhenology::ttInCurrentPhase(void)
   {
	const pPhase &current = phases[currentStage];
	return current.getTT();
   }

int PlantPhenology::daysInPhase(const string &phaseName)
   {
      // See if it's a composite
      compositePhase phaseGroup = composites[phaseName];
      if (!phaseGroup.isEmpty())
      {
         return phaseGroup.getDays();
      }
      else
      {
         // No, see if the stage is known at all to us
         pPhase *phase = find(phases.begin(), phases.end(), pPhase(phaseName));
         if (phase == phases.end())
         {
            throw std::runtime_error("unknown phase name " + phaseName);
         }
         else
         {
   	      return phase->getDays();
   	   }
   	}
   }

string PlantPhenology::stageName(void)
   {
   unsigned int stage_no = (unsigned int) currentStage;
   return string(phases[stage_no].name());
   }
string PlantPhenology::stageName(int n)
   {
   return phases[n].name();
   }
string PlantPhenology::previousStageName(void)
   {
   unsigned int stage_no = (unsigned int) previousStage;
   return string(phases[stage_no].name());
   }

//  Purpose
//       Return fraction of thermal time we are through the current
//       phenological phase (0-1)
float PlantPhenology::phase_fraction(float dlt_tt) //(INPUT)  daily thermal time (growing degree days)
   {
   const pPhase &current = phases[(int) currentStage];

   float dividend = current.getTT() + dlt_tt;
   float divisor = current.getTTTarget();
   float result = divide (dividend, divisor, 0.0);
   result = bound(result, 0.0, 1.0);
   return result;
   }
void PlantPhenology::zeroAllGlobals(void)
   {
   previousStage = currentStage = 0.0;
   for (unsigned int i=0; i < phases.size(); i++) phases[i].reset();
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
    if (phases[currentStage].isFirstDay())
        {
        float tt_tot = phases[currentStage].getTT();
        float phase_tt = phases[currentStage].getTTTarget();
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
   s->sendVariable(qd, phases[stage_no].name());
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
   for(unsigned int i=1; i < phases.size(); i++) t.push_back(phases[i].getTTTarget());
   s->sendVariable(qd, t);
   }
void PlantPhenology::get_tt_tot(protocol::Component *s, protocol::QueryValueData &qd)
   {
   vector<float> t;
   for(unsigned int i=1; i < phases.size(); i++) t.push_back(phases[i].getTT());
   s->sendVariable(qd, t);
   }
void PlantPhenology::get_days_tot(protocol::Component *s, protocol::QueryValueData &qd)
   {
   vector<float> t;
   for(unsigned int i=1; i < phases.size(); i++) t.push_back(phases[i].getDays());
   s->sendVariable(qd, t);
   }

