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

// Return the index of the layer that contains "depth"
int environment_t::find_layer_no(float depth) const
   {
   unsigned int indx;
   float progressive_sum = 0.0;

   for(indx = 0; indx < dlayer.size(); indx++)
      {
      progressive_sum = progressive_sum + dlayer[indx];
      if(progressive_sum >= depth)
         break;
      }
   if (indx != 0 && indx==dlayer.size()) return (indx - 1); // last element in array
   return indx;                                            // index of
   }

float environment_t::daylength(float sun_angle) const
{
	return daylength(day_of_year, sun_angle);
}
float environment_t::daylength(int dyoyr, float sun_angle) const
{
   return ::day_length(dyoyr, latitude, sun_angle);
} 

bool operator == (const pPhase &a, const pPhase &b) {
   return (a.name() == b.name());
};

// Add tt and days to the accumulator. Return a balance if too much.
// A target of 0.0 or less indicates it's a stage that doesn't use
// thermal time accumulation. So avoid taking any tt if you can avoid it..
void pPhase::add(float dlt_days, float dlt_tt, float *balance_days, float *balance_tt)
   {
   if (target > 0.0)
      {
      if ((tt + dlt_tt) > target)
         {
         // only take a enough to fill, and don't take any out..
         float tt_in_old = max(0.0, target - tt);
         float days_in_old = dlt_days * divide(tt_in_old, dlt_tt, 0.0);

         tt += tt_in_old;
         days += days_in_old;
         *balance_tt = dlt_tt - tt_in_old;
         *balance_days = dlt_days - days_in_old;
         }
      else
         {
         // take it all
         tt += dlt_tt;
         days += dlt_days;
         *balance_tt = 0.0; *balance_days = 0.0;
         }
      }
   else
      {
      // no target - don't take any.
      *balance_tt = dlt_tt;
      *balance_days = dlt_days;
      }
   }
bool compositePhase::contains(const pPhase &p) const
   {
   for (vector<pPhase *>::const_iterator s = phases.begin(); s !=  phases.end(); s++)
      {
      if ((*s)->name() == p.name()) return true;
      }
   return false;
//   return (find(phases.begin(), phases.end(), p) != phases.end());
   }

float compositePhase::getTT(void) const
{
   float tt = 0.0;
   for (vector<pPhase *>::const_iterator phase = phases.begin(); phase !=  phases.end(); phase++)
   {
      tt += (*phase)->getTT();
   }
   return tt;
}

float compositePhase::getDays(void) const
{
   float days = 0.0;
   for (vector<pPhase *>::const_iterator phase = phases.begin(); phase !=  phases.end(); phase++)
   {
      days += (*phase)->getDays();
   }
   return days;
}

void PlantPhenology::initialise (PlantComponent *s, const string &section)
{
   parentPlant = s;
   phases.push_back(pPhase("out"));
   currentStage = 0.0;
   initialOnBiomassRemove = true;

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
      //s->writeString((string("registered ") + phases[i].name()).c_str());
      }
};

void PlantPhenology::doRegistrations (protocol::Component *s)
{

#define setupGetVar s->addGettableVar
#define setupGetFunction(name,type,length,address,units,desc) {\
   boost::function2<void, protocol::Component *, protocol::QueryValueData &> fn;\
   fn = boost::bind(address, this, _1, _2); \
   s->addGettableVar(name, type, length, fn, units, desc);\
   }

   setupGetVar("stage", currentStage, "", "Plant stage");
   setupGetVar("dlt_stage", dltStage, "", "Change in plant stage");
   setupGetFunction("stage_name", protocol::DTstring, false,
                    &PlantPhenology::get_stage_name, "", "Plant stage name");
   setupGetFunction("stage_code", protocol::DTint4, false,
                    &PlantPhenology::get_stage_code, "", "Plant stage code");
   setupGetFunction("phase_tt", protocol::DTsingle, true,
                    &PlantPhenology::get_phase_tt, "dd", "Thermal time target for each crop phase");
   setupGetFunction("tt_tot", protocol::DTsingle, true,
                    &PlantPhenology::get_tt_tot, "dd", "Thermal time spent in each crop stage");
   setupGetFunction("days_tot",protocol::DTsingle, true,
                    &PlantPhenology::get_days_tot, "days", "Days spent in each crop stage");


#undef setupGetVar
#undef setupGetFunction
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

//bool PlantPhenology::stage_is_between(const pStage &a, const pStage &b)
//   {
//   pStage *apos = find(phases.begin(), phases.end(), a);
//   if (apos == phases.end()) { throw std::runtime_error("unknown stage name " + a.name());}
//   pStage *bpos = find(phases.begin(), phases.end(), b);
//   if (bpos == phases.end()) { throw std::runtime_error("unknown stage name " + b.name());}
//   pStage *cpos = &phases[currentStage];
//   return (cpos >= apos && cpos < bpos);
//   }

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
   flowering_das = maturity_das = 0;
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
///////////////////////////WHEAT///////////////////////////////////
void WheatPhenology::zeroAllGlobals(void)
   {
   PlantPhenology::zeroAllGlobals();
   das = cumvd = vern_eff = photop_eff = 0.0;
   }

void WheatPhenology::zeroDeltas(void)
   {
   dlt_tt = dlt_tt_phenol = dlt_cumvd = 0.0;
   }

void WheatPhenology::initialise (PlantComponent *s, const string &section)
   {
   s->writeString("phenology model: Wheat");
   PlantPhenology::initialise(s, section);
   zeroAllGlobals();
};

void WheatPhenology::doRegistrations (protocol::Component *s)
   {
   PlantPhenology::doRegistrations(s);

#define setupEvent(name,type,address) {\
   boost::function3<void, unsigned &, unsigned &, protocol::Variant &> fn;\
   fn = boost::bind(address, this, _1, _2, _3); \
   s->addEvent(name, type, fn);\
   }

   setupEvent("sow", RegistrationType::respondToEvent, &WheatPhenology::onSow);
   setupEvent("end_crop", RegistrationType::respondToEvent, &WheatPhenology::onEndCrop);
   //XX not yet setupEvent("harvest", RegistrationType::respondToEvent, &WheatPhenology::onHarvest);
   //XXnot yet setupEvent("kill_stem", RegistrationType::respondToEvent, &WheatPhenology::onKillStem);

#define setupGetVar s->addGettableVar
#define setupGetFunction(name,type,length,address,units,desc) {\
   boost::function2<void, protocol::Component *, protocol::QueryValueData &> fn;\
   fn = boost::bind(address, this, _1, _2); \
   s->addGettableVar(name, type, length, fn, units, desc);\
   }

   setupGetVar("das", das,               "d", "Days after Sowing");
   setupGetVar("cum_vernal_days", cumvd, "vd", "Cumulative vernalisation");
   setupGetVar("vern_eff", vern_eff,     "", "Vernalisation effect");
   setupGetVar("photop_eff", photop_eff, "", "Photoperiod effect");
   setupGetVar("dlt_cumvd", dlt_cumvd,   "", "Todays vd");
   setupGetVar("dlt_tt_phenol", dlt_tt_phenol,"dd", "Todays thermal time (incl. stress factors)");
   setupGetVar("dlt_tt", dlt_tt,         "dd", "Todays thermal time (no stress factors)");
   setupGetFunction("zadok_stage", protocol::DTsingle, false,
                    &WheatPhenology::get_zadok_stage,
                    "0-100", "Zadok's growth developmental stage");
   setupGetVar("flowering_das", flowering_das, "days", "Days from sowing to flowering");
   setupGetVar("maturity_das", maturity_das, "days", "Days from sowing to maturity");

#undef setupGetVar
#undef setupGetFunction
#undef setupEvent
   }

void WheatPhenology::readSpeciesParameters(protocol::Component *s, vector<string> &sections)
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

void WheatPhenology::readCultivarParameters(protocol::Component *s, const string & cultivar)
   {
   PlantPhenology::readCultivarParameters(s, cultivar);
   s->readParameter (cultivar
                   , "phyllochron"//, "()"
                   , phyllochron
                   , 0.0, 300.);

   s->readParameter (cultivar
                   , "startgf_to_mat"//, "()"
                   , startgf_to_mat
                   , 0.0, 3000.);

   s->readParameter (cultivar
                   , "vern_sens"//, "()"
                   , vern_sens
                   , 0.0, 10.0);

   s->readParameter (cultivar
                   , "photop_sens"//, "()"
                   , photop_sens
                   , 0.0, 10.0);
   }

void WheatPhenology::onSow(unsigned &, unsigned &, protocol::Variant &v)
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

void WheatPhenology::setupTTTargets(void)
   {
   pPhase *sow_to_germ = getStage("sowing");
   sow_to_germ->setTarget(0.0);

   // On the germination day, calculate the tt for emergence
   pPhase *germ_to_emerg = getStage("germination");
   germ_to_emerg->setTarget(shoot_lag + sowing_depth * shoot_rate);

   //This is to avoid a warning in leaf number final
   pPhase *emerg_to_endjuv = getStage("emergence");
   emerg_to_endjuv->setTarget(1.0);

   pPhase *endjuv_to_init = getStage("end_of_juvenile");
   endjuv_to_init->setTarget(400.0);

   pPhase *init_to_flower = getStage("floral_initiation");
   init_to_flower->setTarget(5.0 * phyllochron + 80.0);

   pPhase *flower_to_start_grain = getStage("flowering");
   flower_to_start_grain->setTarget( 200.0 - 80.0);

   pPhase *end_grain_to_maturity = getStage("end_grain_fill");
   end_grain_to_maturity->setTarget( 0.05 *
           (flower_to_start_grain->getTTTarget() + startgf_to_mat));

   pPhase *start_to_end_grain = getStage("start_grain_fill");
   start_to_end_grain->setTarget( startgf_to_mat - end_grain_to_maturity->getTTTarget());

   pPhase *maturity_to_ripe = getStage("maturity");
   maturity_to_ripe->setTarget(1.0);

   pPhase *ripe_to_harvest = getStage("harvest_ripe");
   ripe_to_harvest->setTarget(1000.0) ;       // keep it from dying????
   }

void WheatPhenology::onEndCrop(unsigned &, unsigned &, protocol::Variant &)
   {
   zeroAllGlobals();
   }

void WheatPhenology::onHarvest(unsigned &, unsigned &, protocol::Variant &)
   {
   previousStage = currentStage;
   currentStage = stage_reduction_harvest[currentStage];
   for (unsigned int stage = currentStage; stage != phases.size(); stage++)
      phases[stage].reset();
// XX one day, we'll need to be able to restart part way through a stage like this:
//   float fract = fmod(currentStage, 1.0);
//   phases[currentStage].reset(fract);
   }
void WheatPhenology::onKillStem(unsigned &, unsigned &, protocol::Variant &)
   {
   previousStage = currentStage;
   currentStage = stage_reduction_kill_stem[currentStage];
   for (unsigned int stage = (int)currentStage; stage != phases.size(); stage++)
      phases[stage].reset();
   }


void WheatPhenology::prepare(const environment_t &e)
   {
   PlantPhenology::prepare(e);
   }

// vernalisation & photoperiod stresses
void WheatPhenology::vernalisation (const environment_t &t)
   {
   float tempcr = crown_temp_nwheat (t.maxt, t.mint, 0.0);

   dlt_cumvd = wheat_vernaliz_days(t.maxt ,t.mint ,tempcr, 0.0 , cumvd);

   //maximum vernalisation requirement is 50 days
   vern_eff = wheat_vernaliz_effect(vern_sens, cumvd, dlt_cumvd, 50.0);

   float photoperiod = t.daylength(twilight);
   photop_eff = wheat_photoperiod_effect(photoperiod, photop_sens);

   // Thermal time is calculated from crown temperature
   dlt_tt = y_tt[tempcr];
   }

// Crown temperature from nwheat
float WheatPhenology::crown_temp_nwheat (float maxt, float mint, float snow)
   {
   // Calculate max crown temperature
   float cx;
   if (maxt < 0.)
        cx = 2.0 + maxt * (0.4 + 0.0018 * pow(snow - 15., 2));
   else
        cx = maxt;

   // Calculate min crown temperature
   float cn;
   if (mint < 0.)
        cn = 2.0 + mint * (0.4 + 0.0018 * pow(snow - 15., 2));
   else
        cn = mint;

   return ((cn+cx)/2.0);
   }

// Soil water stresses
//+  Purpose
//     Use temperature, photoperiod and genetic characteristics
//     to determine when the crop begins a new growth phase.
//     The initial daily thermal time and height are also set.
// NB. There are 2 ways to advance from one stage to the next:
// via the dltStage calculation in stage_devel(), or
// via thermal time accumulation
#ifdef PHENOLOGY_PETE
void WheatPhenology::process (const environment_t &sw, const pheno_stress_t &ps)
   {
   vernalisation(sw);

   if (inPhase("sowing"))
      {
      dlt_tt_phenol = dlt_tt;
      // can't germinate on same day as sowing, because we would miss out on
      // day of sowing elsewhere.
      if (das == 0)
         {
         dltStage = 0.0;
         }
      else if ( plant_germination(pesw_germ, sowing_depth, sw) )
         {
      	dltStage = 1.0;
         }
      }
   else if (inPhase("germination"))
      {
      int layer_no_seed = sw.find_layer_no (sowing_depth);
      float fasw_seed = divide (sw.sw_dep[layer_no_seed] - sw.ll_dep[layer_no_seed],
                                sw.dul_dep[layer_no_seed] - sw.ll_dep[layer_no_seed], 0.0);
      fasw_seed = bound (fasw_seed, 0.0, 1.0);

      dlt_tt_phenol = dlt_tt * min(vern_eff, photop_eff) * rel_emerg_rate[fasw_seed];
      dltStage = (phase_fraction(dlt_tt_phenol) + floor(currentStage))- currentStage;
      }
   else if (inPhase("above_ground"))
      {
      float fstress = min (ps.swdef, ps.nfact);  //eme2fi: (g.swdef_pheno, g.nfact_pheno);
      if (inPhase("flowering"))
         fstress = ps.swdef_flower;              //g.swdef_pheno_flower;
      else if (inPhase("start_grain_fill"))
         fstress = ps.swdef_grainfill;           //g.swdef_pheno_grainfill;
      else if (inPhase("end_grain_fill") || inPhase("maturity"))
         fstress = 1.0;                          //no stress - not really a stage..
      else if (inPhase("harvest_ripe"))
         fstress = 0.0;                          //stop development
      dlt_tt_phenol = dlt_tt * fstress * min(vern_eff, photop_eff);
      dltStage = (phase_fraction(dlt_tt_phenol) + floor(currentStage)) - currentStage;
      }
   else
      {
      // ??Hmmm. should probably stop dead here??
      dlt_tt_phenol = dlt_tt;
      dltStage = 0.0;
      }

   // update thermal time states and day count
   float balance_tt = 0.0;
   float balance_days = 0.0;
   phases[(int)currentStage].add(1.0, dlt_tt_phenol, &balance_days, &balance_tt);

   // (new) stage calculation
   currentStage += dltStage;

   if ((unsigned int)currentStage >= phases.size() || currentStage < 0.0)
     throw std::runtime_error("stage has gone wild in WheatPhenology::process()..");

   // get rid of any remaining tt into the next stage
   if (balance_tt > 0.0)
      phases[(int)currentStage].add(balance_days, balance_tt);
   if ((int)currentStage != (int)previousStage) parent->onPlantEvent(phases[(int)currentStage].name());
   cumvd += dlt_cumvd;
   das++;
   }
#else
///////// This uses the "old" methodology///////////////
void WheatPhenology::process (const environment_t &sw, const pheno_stress_t &ps)
   {
   float phase_devel, new_stage;

   vernalisation(sw);

   if (inPhase("sowing"))
      {
      dlt_tt_phenol = dlt_tt;
      // can't germinate on same day as sowing, because we would miss out on
      // day of sowing elsewhere.
      if (das == 0)
         {
         phase_devel = 0.999;
         }
      else if ( plant_germination(pesw_germ, sowing_depth, sw) )
         {
         phase_devel = 1.999;
         }
      else
         {
         phase_devel = 0.0;
         }
      new_stage = floor(currentStage) + phase_devel;
      }
   else if (inPhase("germination"))
      {
      int layer_no_seed = sw.find_layer_no (sowing_depth);
      float fasw_seed = divide (sw.sw_dep[layer_no_seed] - sw.ll_dep[layer_no_seed],
                                sw.dul_dep[layer_no_seed] - sw.ll_dep[layer_no_seed], 0.0);
      fasw_seed = bound (fasw_seed, 0.0, 1.0);

      dlt_tt_phenol = dlt_tt *
                       min(vern_eff, photop_eff) *
                       rel_emerg_rate[fasw_seed];

      const pPhase &current = phases[currentStage];
      phase_devel = divide(current.getTT() + dlt_tt_phenol, current.getTTTarget(), 1.0);
      new_stage = floor(currentStage) + phase_devel;
      }
   else if (inPhase("above_ground"))
      {
      float fstress = min (ps.swdef, ps.nfact);  //eme2fi: (g.swdef_pheno, g.nfact_pheno);
      if (inPhase("flowering"))
         fstress = ps.swdef_flower;              //g.swdef_pheno_flower;
      else if (inPhase("start_grain_fill"))
         fstress = ps.swdef_grainfill;           //g.swdef_pheno_grainfill;
      else if (inPhase("end_grain_fill") || inPhase("maturity"))
         fstress = 1.0;                          //no stress - not really a stage..
      else if (inPhase("harvest_ripe"))
         fstress = 0.0;                          //stop development
      dlt_tt_phenol = dlt_tt *
                      fstress *
                      min(vern_eff, photop_eff);

      const pPhase &current = phases[currentStage];
      phase_devel = divide(current.getTT() + dlt_tt_phenol, current.getTTTarget(), 1.0);
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
            portion_in_old = fract_in_old * (value + phases[current_index].getTT())-
                                 phases[current_index].getTT();
            }
         else
            {
            fract_in_old = 1.0 - divide(index_devel - 1.0, dlt_index, 0.0);
            portion_in_old = fract_in_old * value;
            }
         portion_in_new = value - portion_in_old;
         phases[current_index].add(fract_in_old, portion_in_old);
         phases[new_index].add(1.0-fract_in_old, portion_in_new);
         }
      else
         {
         phases[current_index].add(1.0, value);
         }
      }
   }

   if (phase_devel >= 1.0)
      currentStage = floor(currentStage + 1.0);
   else
      currentStage = new_stage;

   if ((unsigned int)currentStage >= phases.size() || currentStage < 0.0)
     throw std::runtime_error("stage has gone wild in WheatPhenology::process()..");

   if ((int)currentStage != (int)previousStage) plant->doPlantEvent(phases[(int)currentStage].name());
   cumvd += dlt_cumvd;
   das++;
   }
#endif

void WheatPhenology::update(void) 
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
//+  Mission Statement
//     Photoperiod factor
float WheatPhenology::wheat_photoperiod_effect(float photoperiod, float p_photop_sen)
    {
    float photop_eff = 1.0;

    if (inPhase("eme2ej"))
        {
        float  photop_sen_factor = p_photop_sen * 0.002;
        photop_eff = 1. - photop_sen_factor * pow(20. - photoperiod, 2);
        photop_eff = bound (photop_eff, 0.0, 1.0);
        }
    return photop_eff;
    }


//+  Purpose
//     Calculate daily vernalisation and accumulate to g_cumvd

//+  Mission Statement
//     Calculate todays vernalization (used to affect phenology)

//+  Notes
//   Nwheat originally had the following if logic for determining whether
//   vernalisation is calculated for today
//     if     (          cumvd .lt. reqvd
//     :                        .and.
//     :       (istage .eq.emerg .or. istage .eq. germ)   )
//     :then
//
//   In order to remove the explicit value 'reqvd' and make the stages
//   more flexibile this logic was replaced. - NIH 14/07/98
float WheatPhenology::wheat_vernaliz_days(float g_maxt    //Daily maximum Temperature
                                         ,float g_mint    //Daily minimum temperature
                                         ,float tempcr    //Crown temperature
                                         ,float g_snow    //Snow depth of the day (mm)
                                         ,float g_cumvd)  //cumulative vernalisation days till yesterday
   {
   float dlt_cumvd = 0.0;

   if (inPhase("vernalisation"))
      {
      if (g_mint < 15.0 && g_maxt > 0.0)
          {
          // Cold
          float vd,vd1,vd2;
          vd1 = 1.4 - 0.0778 * tempcr;
          vd2 = 0.5 + 13.44 / pow(g_maxt-g_mint + 3., 2) * tempcr;
          vd = min (vd1, vd2);
          dlt_cumvd = l_bound (vd, 0.0);
          }
      if (g_maxt > 30. && g_cumvd + dlt_cumvd < 10.)
          {
          // high temperature will reduce vernalization
          dlt_cumvd = - 0.5*(g_maxt - 30.);
          dlt_cumvd = - min(-(dlt_cumvd), g_cumvd);
          }
      }
   return dlt_cumvd;
   }

//+  Purpose
//     Vernalisation factor
float WheatPhenology::wheat_vernaliz_effect(float p_vern_sens
                                           ,float cumvd
                                           ,float dlt_cumvd
                                           ,float reqvd) {
    float vfac;                // vernalization factor
    float vern_sens_fac;
    float vern_effect = 1.0;

    if (inPhase("eme2ej"))
        {
        if (reqvd < 0.0) { reqvd = 50.0; }
        vern_sens_fac =  p_vern_sens* 0.0054545 + 0.0003;
        vfac = 1. - vern_sens_fac * (reqvd - (cumvd+dlt_cumvd));
        vern_effect = bound (vfac, 0.0, 1.0);
        }
    return vern_effect;
    }


//  Purpose
//    Determine whether seed germinates based on soil water availability
bool plant_germination(float pesw_germ,         //(INPUT)  plant extractable soil water required for germination
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

// XX This is heavily tied to current ini file. !!Watch Out!!
// NB. "5.2" is half way between FI and flag leaf in wheat
void WheatPhenology::get_zadok_stage(protocol::Component *system, protocol::QueryValueData &qd)
{
    float sowing = 1.0, emerg = 3.0;
    float zadok_stage = 0.0;
    if (currentStage >= sowing &&
        currentStage <= emerg)
       {
       zadok_stage = 5.0 * (currentStage - sowing);
       }
    else if (currentStage > emerg &&
             currentStage <= 4.9)
       {
       float leaf_no_now = max(0.0, plant->getLeafNo() - 2.0);

       static float tillerno_y[] =   // tiller no.
           {0, 0, 5};
       static float tillerno_x[] =   // lf. no.
           {0, 5, 8};
       float tiller_no_now = linear_interp_real (leaf_no_now
                                               , tillerno_x
                                               , tillerno_y
                                               , sizeof(tillerno_x)/sizeof(float));
       if (tiller_no_now <= 0.0)
           {
           zadok_stage = 10.0 + leaf_no_now;
           }
        else
           {
           zadok_stage = 20.0 + tiller_no_now;
           }
       }
    else if (currentStage > 4.9 &&
             currentStage < 11 )
       {
// from senthold's archive:
//1    2    3         4.5     5       6
//eme  ej   eveg(fl)  anth    sgf   mat
//10   30   43 	     65      70     9

// from CropMod
//                 sow ger eme  juv    fi   flag    fl st_gf end_gf mat hv_rpe end_crop
//stage_code       = 1   2   3    4      5     6     7    8    9    10   11   12
//Zadok_stage      = 0   5  10   10     15    40    60   71   87    90   93  100

// Plant:
//                 sow    ger   eme  juv    fi       fl   st_gf end_gf  mat hv_rpe  end
//stage_code      = 1      2    3     4     5   4.9 5.4   6     7      8     9    10     11  ()     ! numeric code for phenological stages
//                  na     na   na    na    na   30  43   65    71     87    90    100

       static float zadok_code_y[] =
           {30,   40, 65, 71, 87, 90, 100};
       static float zadok_code_x[] =
           {4.9, 5.4,  6,  7,  8,  9,  10};

      zadok_stage = linear_interp_real (currentStage
                                       , zadok_code_x
                                       , zadok_code_y
                                       , sizeof(zadok_code_x)/sizeof(float));
       }
    system->sendVariable(qd, zadok_stage);
}

void WheatPhenology::writeCultivarInfo (PlantComponent *systemInterface)
   {
   string s;
   s += "   pesw germination           = ";
   s += ftoa(pesw_germ, "10.2") + " (0-1)\n";

   s += "   vernalisation sensitivity  = ";
   s += ftoa(vern_sens, "10.2") + " ()\n";

   s += "   photoperiod sensitivity    = ";
   s += ftoa(photop_sens, "10.2") + " ()\n";

   s += "   phyllochron                = ";
   s += ftoa(phyllochron, "10.0") + " ()\n";

   s += "   tt start gf to maturity    = ";
   s += ftoa(startgf_to_mat, "10.0") + " (dd)";

   systemInterface->writeString (s.c_str());
   }

void LegumePhenology::onSow(unsigned &, unsigned &, protocol::Variant &v)
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
void LegumePhenology::onEndCrop(unsigned &, unsigned &, protocol::Variant &)
   {
   zeroAllGlobals();
   }
void LegumePhenology::onHarvest(unsigned &, unsigned &, protocol::Variant &)
   {
   previousStage = currentStage;
   currentStage = stage_reduction_harvest[currentStage];
   for (unsigned int stage = currentStage; stage != phases.size(); stage++)
      phases[stage].reset();
   }
void LegumePhenology::onKillStem(unsigned &, unsigned &, protocol::Variant &)
   {
   previousStage = currentStage;
   currentStage = stage_reduction_kill_stem[currentStage];
   for (unsigned int stage = currentStage; stage != phases.size(); stage++)
      phases[stage].reset();
   }

void LegumePhenology::onRemoveBiomass(float removeBiomPheno)
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


   float ttCritical = max(0.0, ttInPhase("above_ground") - ttInPhase("emergence"));
   float removeFractPheno = y_removeFractPheno[removeBiomPheno];
   float removeTTPheno = ttCritical * removeFractPheno;

   ostrstream msg;
   msg << "Phenology change:-" << endl;
   msg << "    Fraction DM removed  = " << removeBiomPheno << endl;
   msg << "    Fraction TT removed  = " << removeFractPheno << endl;
   msg << "    Critical TT          = " << ttCritical << endl;
   msg << "    Remove TT            = " << removeTTPheno << endl;

   float ttRemaining = removeTTPheno;
   vector <pPhase>::reverse_iterator phase;
   for (phase = phases.rbegin(); phase !=  phases.rend(); phase++)
   {
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
   parentPlant->writeString (msg.str());

}

void LegumePhenology::prepare (const environment_t &e)
   {
   PlantPhenology::prepare(e);
   photoperiod = e.daylength (twilight);

   updateTTTargets(e);
   }


// static TT targets (called at sowing)
void LegumePhenology::setupTTTargets(void)
   {
   pPhase *germ_to_emerg = getStage("germination");
   germ_to_emerg->setTarget(shoot_lag + sowing_depth * shoot_rate);

   pPhase *end_grain_to_maturity = getStage("end_grain_fill");
   end_grain_to_maturity->setTarget(tt_end_grain_to_maturity);

   pPhase *maturity_to_ripe = getStage("maturity");
   maturity_to_ripe->setTarget(tt_maturity_to_ripe);
   }

// dynamic TT targets
void LegumePhenology::updateTTTargets(const environment_t &e)
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

void LegumePhenology::initialise (PlantComponent *s, const string &section)
   {
   s->writeString("phenology model: Legume");
   PlantPhenology::initialise(s, section);
   zeroAllGlobals();
   }

void LegumePhenology::doRegistrations (protocol::Component *s)
   {
   PlantPhenology::doRegistrations(s);
#define setupGetVar s->addGettableVar
#define setupGetFunction(name,type,length,address,units,desc) {\
   boost::function2<void, protocol::Component *, protocol::QueryValueData &> fn;\
   fn = boost::bind(address, this, _1, _2); \
   s->addGettableVar(name, type, length, fn, units, desc);\
   }
#define setupEvent(name,type,address) {\
   boost::function3<void, unsigned &, unsigned &, protocol::Variant &> fn;\
   fn = boost::bind(address, this, _1, _2, _3); \
   s->addEvent(name, type, fn);\
   }
   setupEvent("sow", RegistrationType::respondToEvent, &LegumePhenology::onSow);
   setupEvent("end_crop", RegistrationType::respondToEvent, &LegumePhenology::onEndCrop);
   //XX not yet setupEvent("harvest", RegistrationType::respondToEvent, &LegumePhenology::onHarvest);
   //XX not yet setupEvent("kill_stem", RegistrationType::respondToEvent, &LegumePhenology::onKillStem);

   setupGetVar("das", das,               "d", "Days after Sowing");
   setupGetVar("dlt_tt_phenol", dlt_tt_phenol,"dd", "Todays thermal time (incl. stress factors)");
   setupGetVar("dlt_tt", dlt_tt,         "dd", "Todays thermal time (no stress factors)");
   setupGetVar("dlt_cumvd", dlt_cumvd,   "", "Todays vd");
   setupGetVar("flowering_das", flowering_das, "days", "Days from sowing to flowering");
   setupGetVar("maturity_das", maturity_das, "days", "Days from sowing to maturity");

#undef setupGetVar
#undef setupGetFunction
#undef setupEvent
   }


void LegumePhenology::readCultivarParameters(protocol::Component *s, const string & cultivar)
   {
   PlantPhenology::readCultivarParameters(s, cultivar);

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

void LegumePhenology::readSpeciesParameters (protocol::Component *s, vector<string> &sections)
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

   vernal_days.search(s, sections,
                       "x_vernal_temp", "(oc)", -10., 60.0,
                       "y_vernal_days", "(days)", 0.0, 1.0);

   s->readParameter (sections,
                       "shoot_lag"//, "(oc)"
                      , shoot_lag
                      , 0.0, 100.0);

   s->readParameter (sections,
                       "shoot_rate"//, "(oc/mm)"
                      , shoot_rate
                      , 0.0, 100.0);

   y_tt.search(s, sections,
               "x_temp", "oC", 0.0, 100.0,
               "y_tt", "oC days", 0.0, 100.0);

   s->readParameter (sections
                   ,"tt_emerg_to_endjuv_ub"//, "()"
                   , tt_emerg_to_endjuv_ub
                   , 0.0, 1.e6);

   s->readParameter (sections
                   ,"tt_maturity_to_ripe_ub"//, "()"
                   , tt_maturity_to_ripe_ub
                   , 0.0, 1.e6);

   s->readParameter (sections,
                       "pesw_germ"//, "(mm/mm)"
                      , pesw_germ
                      , 0.0, 1.0);

   rel_emerg_rate.search(s, sections,
                         "fasw_emerg", "()", 0.0, 1.0,
                         "rel_emerg_rate",  "()", 0.0, 1.0);

}

void LegumePhenology::zeroDeltas(void)
   {
   dlt_tt = dlt_tt_phenol = dlt_cumvd = 0.0;
   }

void LegumePhenology::zeroAllGlobals(void)
   {
   PlantPhenology::zeroAllGlobals();
   das = cumvd = 0.0;
   est_days_emerg_to_init=0;
   }

void LegumePhenology::writeCultivarInfo (PlantComponent *systemInterface)
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



//+  Purpose
//     Use temperature, photoperiod and genetic characteristics
//     to determine when the crop begins a new growth phase.
//     The initial daily thermal time and height are also set.
#ifdef PHENOLOGY_PETE
void LegumePhenology::process(const environment_t &e, const pheno_stress_t &ps)
   {
   dlt_tt = linint_3hrly_temp (e.maxt, e.mint, &y_tt);
   if (inPhase("sowing"))
      {
      dlt_tt_phenol = dlt_tt;
      // can't germinate on same day as sowing, because we would miss out on
      // day of sowing elsewhere.
      if (das == 0)
         {
         dltStage = 0.0;
         }
      else if ( plant_germination(pesw_germ, sowing_depth, e) )
         {
      	dltStage = 1.0;
         }
      }
   else if (inPhase("germination"))
      {
      int layer_no_seed = e.find_layer_no (sowing_depth);
      float fasw_seed = divide (e.sw_dep[layer_no_seed] - e.ll_dep[layer_no_seed],
                                e.dul_dep[layer_no_seed] - e.ll_dep[layer_no_seed], 0.0);
      fasw_seed = bound (fasw_seed, 0.0, 1.0);

      dlt_tt_phenol = dlt_tt * rel_emerg_rate[fasw_seed];
      dltStage = (phase_fraction(dlt_tt_phenol) + floor(currentStage))- currentStage;
      }
   else if (inPhase("emergence2floral_initiation"))
       {
       dlt_tt_phenol = dlt_tt *  min(ps.swdef, ps.nfact);
       dltStage = (phase_fraction(dlt_tt_phenol) + floor(currentStage))- currentStage;
       }
    else if (inPhase("flowering"))
       {
       dlt_tt_phenol = dlt_tt *  ps.swdef_flower;          //no nstress
       dltStage = (phase_fraction(dlt_tt_phenol) + floor(currentStage))- currentStage;
       }
    else if (inPhase("start_grain_fill2harvest_ripe"))
       {
       dlt_tt_phenol = dlt_tt *  ps.swdef_grainfill;       //no nstress
       dltStage = (phase_fraction(dlt_tt_phenol) + floor(currentStage))- currentStage;
       }
    else
       {
       dlt_tt_phenol = dlt_tt;
       dltStage = (phase_fraction(dlt_tt_phenol) + floor(currentStage))- currentStage;
       }


   // update thermal time states and day count
   float balance_tt = 0.0, balance_days = 0.0;
   phases[(int)currentStage].add(1.0, dlt_tt_phenol, &balance_days, &balance_tt);

   // (new) stage calculation
   currentStage += dltStage;

   if ((unsigned int)currentStage >= phases.size() || currentStage < 0.0)
     throw std::runtime_error("stage has gone wild in LegumePhenology::process()..");

   // get rid of any remaining tt into the next stage
   if (balance_tt > 0.0)
      phases[(int)currentStage].add(balance_days, balance_tt);

   if ((int)currentStage != (int)previousStage) parent->onPlantEvent(phases[(int)currentStage].name());
   das++;
   cumvd += dlt_cumvd;
   }
#else
void LegumePhenology::process (const environment_t &e, const pheno_stress_t &ps)
   {
   float phase_devel, new_stage;

   dlt_tt = linint_3hrly_temp (e.maxt, e.mint, &y_tt);

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
      const pPhase &current = phases[currentStage];
      float a =  current.getTT() + dlt_tt_phenol;
      float b =  current.getTTTarget();
      phase_devel = divide(a, b, 1.0);
      new_stage = floor(currentStage) + phase_devel;
      }
   else if (inPhase("emergence2floral_initiation"))
      {
      dlt_tt_phenol = dlt_tt * min(ps.swdef, ps.nfact);
      const pPhase &current = phases[currentStage];
      float a =  current.getTT() + dlt_tt_phenol;
      float b =  current.getTTTarget();
      phase_devel = divide(a, b, 1.0);
      new_stage = floor(currentStage) + phase_devel;
      }
    else if (inPhase("flowering"))
      {
      dlt_tt_phenol = dlt_tt *  ps.swdef_flower;          //no nstress
      const pPhase &current = phases[currentStage];
      float a =  current.getTT() + dlt_tt_phenol;
      float b =  current.getTTTarget();
      phase_devel = divide(a, b, 1.0);
      new_stage = floor(currentStage) + phase_devel;
      }
    else if (inPhase("start_grain_fill2harvest_ripe"))
      {
      dlt_tt_phenol = dlt_tt *  ps.swdef_grainfill;       //no nstress
      const pPhase &current = phases[currentStage];
      float a =  current.getTT() + dlt_tt_phenol;
      float b =  current.getTTTarget();
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
         portion_in_old = fract_in_old * (value + phases[current_index].getTT())-
                              phases[current_index].getTT();
         }
      else
         {
         fract_in_old = 1.0 - divide(index_devel - 1.0, dlt_index, 0.0);
         portion_in_old = fract_in_old * value;
         }
      portion_in_new = value - portion_in_old;
      phases[current_index].add(fract_in_old, portion_in_old);
      phases[new_index].add(1.0-fract_in_old, portion_in_new);
      }
   else
      {
      phases[current_index].add(1.0, value);
      }
   }
   if (phase_devel >= 1.0)
      currentStage = floor(currentStage + 1.0);
   else
      currentStage = new_stage;

   if ((unsigned int)currentStage >= phases.size() || currentStage < 0.0)
     throw std::runtime_error("stage has gone wild in LegumePhenology::process()..");

   if ((int)currentStage != (int)previousStage) plant->doPlantEvent(phases[(int)currentStage].name());
   cumvd += dlt_cumvd;
   das++;
   }
#endif

void LegumePhenology::update(void) 
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

#if 0
//+  Purpose
//     Calculate min and max crown temperatures.
void crop_crown_temp_nwheat( float tempmx        //Daily maximum temperature of the air (C)
                            ,float tempmn        //Daily minimum temperature of the air (C)
                            ,float snow          //Snow depth on the current day (mm)
                            ,float *tempcx       //Daily maximum of crown temperature (C)     - OUTPUT
                            ,float *tempcn)      //Daily minimum of crown temperature (C)     - OUTPUT
   {
   // Calculate max crown temperature
   if (tempmx < 0.)
        {
        *tempcx = 2.0 + tempmx * (0.4 + 0.0018 * pow(snow - 15., 2));
        }
   else
        {
        *tempcx = tempmx;
        }

   // Calculate min crown temperature
   if (tempmn < 0.)
        {
        *tempcn = 2.0 + tempmn * (0.4 + 0.0018 * pow(snow - 15., 2));
        }
   else
        {
        *tempcn = tempmn;
        }
   }

#endif
/* Purpose
*     returns the temperature for a 3 hour period.
*      a 3 hourly estimate of air temperature
*/
float temp_3hr (float tmax, float tmin, int period)
   {
   // Local Variables
   float period_no;              // period number
   float diurnal_range;          // diurnal temperature range for the
                                 //   day (oC)
   float t_deviation;            // deviation from day's minimum for this
                                 //    3 hr period
   float t_range_fract;          // fraction_of of day's range_of for this
                                 //   3 hr period

   // Implementation Section ----------------------------------
   if (period < 1)
      throw std::invalid_argument("3 hr. period number is below 1");
   else if (period > 8)
      throw std::invalid_argument("3 hr. period number is above 8");

   period_no = float(period);
   t_range_fract = 0.92105
                   + 0.1140  * period_no
                   - 0.0703  * pow(period_no,2)
                   + 0.0053  * pow(period_no,3);

   diurnal_range = tmax - tmin;
   t_deviation = t_range_fract * diurnal_range;
   return  (tmin + t_deviation);
   }

/* Purpose
*     Eight interpolations of the air temperature are
*     calculated using a three-hour correction factor.
*     For each air three-hour air temperature, a value
*     is calculated.  The eight three-hour estimates
*     are then averaged to obtain the daily value.
*/
float linint_3hrly_temp (float tmax,          //(INPUT) maximum temperature (oC)
                         float tmin,          //(INPUT) maximum temperature (oC)
                         externalFunction *ttFn)
   {
   //Constants
   const int num3hr = 24/3;     // number of 3 hourly temperatures

   // Local Variables
   int period;                  // three hourly period number
   float tot;                   // sum_of of 3 hr interpolations
   float y_3hour;               // 3 hr interpolated value

   // Implementation Section ----------------------------------
   tot = 0.0;

   for(period = 1; period <= num3hr; period++)
      {
      // get mean temperature for 3 hr period (oC)
      float tmean_3hour = temp_3hr (tmax, tmin, period);
      tot = tot + ttFn->value(tmean_3hour);
      }
   return (tot / float(num3hr));
   }

