#include <stdio.h>
#include <math.h>
#include <vector>
#include <string>
#include <stdexcept>
#include <ComponentInterface/Component.h>
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
 // (INPUT) angle to measure time between such as twilight (deg).
 // angular distance between 90 deg and end of twilight - altitude
 // of sun. +ve up, -ve down.

   //+ Purpose
   //      return the time elasped in hours between the specified sun angle
   //      from 90 deg in am and pm. +ve above the horizon, -ve below the horizon.

   //+ Notes
   //                    there is a small err in cos (90), thus a special
   //                    case is made for this.

   //+ Changes
   //       020392 jngh specified and programmed
   //       130592 jngh limited altitude for twilight to increase range_of
   //                   of latitudes. - cr324
   //                   limited cos of the hourangle between -1 and 1 - cr324
   //       190592 jngh renamed day_length routine - cr323
   //       290592 jngh set cos hourangle to +/-1 when latitude is +/- 90 - cr350
   //                   corrected descriptions - cr352
   //       230792 jngh corrected coshra to take account of latitude sign - cr401
   //       200893 jngh corrected problem with precision which occurred when
   //                   latitude is very close to tropic line and declination
   //                   is also very close, abs(slsd-clcd) may go slightly above
   //                   1.0, which asin doesn't like.
   //       071293 jngh added sun (twilight) angle to arguments
   //       270295 jngh put in function to test for equal reals.

   //+ Constant Values
   const double  aeqnox = 82.25 ;//  average day number of autumnal equinox

   const double  pi = 3.14159265359 ;

   const double  dg2rdn =  (2.0*pi) /360.0 ; // convert degrees to radians

   const double  decsol = 23.45116 * dg2rdn ;// amplitude of declination of sun
                                             //   - declination of sun at solstices.
                                             // cm says here that the maximum
                                             // declination is 23.45116 or 23 degrees
                                             // 27 minutes.
                                             // I have seen else_where that it should
                                             // be 23 degrees 26 minutes 30 seconds -
                                             // 23.44167
   const double  dy2rdn =  (2.0*pi) /365.25 ; // convert days to radians
   const double  rdn2hr = 24.0/(2.0*pi)  ;    // convert radians to hours

   //+ Local Variables
   double alt;    // twilight altitude limited to max/min
                  //   sun altitudes end of twilight
                  //   - altitude of sun. (radians)
   double altmn;  // altitude of sun at midnight
   double altmx;  // altitude of sun at midday
   double clcd;   // cos of latitude * cos of declination
   double coshra; // cos of hour angle - angle between the
                  //   sun and the meridian.
   double dec;    // declination of sun in radians - this
                  //   is the angular distance at solar
                  //   noon between the sun and the equator.
   double hrangl; // hour angle - angle between the sun
                  //   and the meridian (radians).
   double hrlt;   // day_length in hours
   double latrn;  // latitude in radians
   double slsd;   // sin of latitude * sin of declination
   double sun_alt;// angular distance between
                  // sunset and end of twilight - altitude
                  // of sun. (radians)
                  // Twilight is defined as the interval
                  // between sunrise or sunset and the
                  // time when the true centre of the sun
                  // is 6 degrees below the horizon.
                  // Sunrise or sunset is defined as when
                  // the true centre of the sun is 50'
                  // below the horizon.

   sun_alt = sun_angle * dg2rdn;

   // calculate daylangth in hours by getting the
   // solar declination (radians) from the day of year, then using
   // the sin and cos of the latitude.

   // declination ranges from -.41 to .41 (summer and winter solstices)

   dec = decsol*sin (dy2rdn* ((double)dyoyr - aeqnox));

   // get the max and min altitude of sun for today and limit
   // the twilight altitude between these.

   if (reals_are_equal(fabs(latitude), 90.0)) {
     //coshra = sign (1.0, -dec) * sign (1.0, lat); XXXXXsign???
   } else {
     latrn = latitude*dg2rdn;
     slsd = sin(latrn)*sin(dec);
     clcd = cos(latrn)*cos(dec);

     altmn = asin(min(max(slsd - clcd, -1.0), 1.0));
     altmx = asin(min(max(slsd + clcd, -1.0), 1.0));
     alt = min(max(sun_alt, altmn), altmx);

     // get cos of the hour angle
     coshra = (sin (alt) - slsd) /clcd;
     coshra = min(max(coshra, -1.0), 1.0);
   }

   // now get the hour angle and the hours of light
   hrangl = acos (coshra);
   hrlt = hrangl*rdn2hr*2.0;
   return hrlt;

}

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
//   for (vector<pStage>::const_iterator s = phases.begin();
//        s !=  phases.end();
//        s++)
//      {
//      if (*s == p) return true;
//      }
//   return false;
   return (find(phases.begin(), phases.end(), p) != phases.end());
   }

void PlantPhenology::initialise (PlantComponent *s, const string &section)
{
   phases.push_back(pPhase("out"));
   currentStage = 0.0;
   
   // Read the sequential list of stage names
   string scratch = s->readParameter(section, "stage_names");
   vector<string> stage_names;
   Split_string(scratch, " ", stage_names);
   if (stage_names.size() == 0) throw std::runtime_error("No stage code names found");
   for (vector<string>::iterator sn = stage_names.begin();
        sn !=  stage_names.end();
        sn++)
      {
      phases.push_back(*sn);
      }

   // find composite phases that we care about
   scratch = s->readParameter(section, "composite_phases");
   vector<string> composite_names;
   Split_string(scratch, " ", composite_names);
   for (vector<string>::iterator name = composite_names.begin();
        name !=  composite_names.end();
        name++)
      {
      compositePhase composite(*name);
      scratch = s->readParameter(section, *name);
      vector<string> composite_names;
      Split_string(scratch, " ", composite_names);
      for (vector<string>::iterator phase = composite_names.begin();
           phase !=  composite_names.end();
           phase++)
         {
         pPhase *p = find(phases.begin(), phases.end(), *phase);
         if (p != phases.end())
           composite.add(*p);
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
   setupGetFunction("phase_tt", protocol::DTsingle, true,
                    &PlantPhenology::get_phase_tt, "dd", "Thermal time target for each crop phase");
   setupGetFunction("tt_tot", protocol::DTsingle, true,
                    &PlantPhenology::get_tt_tot, "dd", "Thermal time spent in each crop stage");
   setupGetFunction("days_tot",protocol::DTsingle, true,
                    &PlantPhenology::get_days_tot, "days", "Days spent in each crop stage");
   
#undef setupGetVar
#undef setupGetFunction
}
void PlantPhenology::readSpeciesParameters (PlantComponent *s, vector<string> &sections)
   {
   s->searchParameter (sections
                      , "twilight"//, "(o)"
                      , twilight
                      , -90.0, 90.0);
   }

void PlantPhenology::prepare(const environment_t &sw)
   {
   dltStage = 0;
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

//xxxxxxxxxxxxxxxx
void PlantPhenology::setStage(const pPhase &stage)
   {
   // See if the stage is known at all to us
   pPhase *newpos = find(phases.begin(), phases.end(), stage);
   if (newpos == phases.end()) throw std::runtime_error("Can't set stage to " + stage.name());

   int newStageNumber = newpos - phases.begin();
   currentStage = newStageNumber;
   if ((unsigned int)currentStage >= phases.size() || currentStage < 0.0)
     throw std::runtime_error("stage has gone wild in PlantPhenology::setStage..");
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

int PlantPhenology::daysInCurrentStage(void)
   {
	const pPhase &current = phases[currentStage];
	return current.getDays();
   }

float PlantPhenology::ttInCurrentStage(void)
   {
	const pPhase &current = phases[currentStage];
	return current.getTT();
   }
int PlantPhenology::daysInStage(const pPhase &stage)
   {
	pPhase *mystage = find(phases.begin(), phases.end(), stage);
	if (mystage == phases.end()) throw std::runtime_error("Can't find stage " + stage.name());
	return mystage->getDays();
   }
float PlantPhenology::ttInStage(const pPhase &stage)
   {
	pPhase *mystage = find(phases.begin(), phases.end(), stage);
	if (mystage == phases.end()) throw std::runtime_error("Can't find stage " + stage.name());
	return mystage->getTT();
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
void PlantPhenology::zeroStateVariables(void)
   {
   previousStage = currentStage = dltStage = 0.0;
   for(unsigned int i=0; i < phases.size(); i++)
      phases[i].reset();
   }

/////+  Purpose
/////       Return an interpolated stage code from a table of stage_codes
/////       and a nominated stage number. Returns the first or last table value if the stage number is not
/////       found. Interpolation is done on thermal time.
///// Hmmmmmm. This is very dodgy.
///float PlantPhenology::stageCode (void) 
///    {
///if (currentStage < 3.0) return 3.0;
///if (phases[currentStage].isFirstDay()) {
///    float tt_tot = phases[currentStage].getTT();
///    float phase_tt = phases[currentStage].getTTTarget();
///    float fraction_of = divide (tt_tot, phase_tt, 0.0);
///    fraction_of = bound(fraction_of, 0.0, 0.999);
///    return((int)currentStage +  fraction_of);
///}
///return currentStage;
///}


//////////////////////GetVariable etc////////////////////////////
void PlantPhenology::get_stage_name(protocol::Component *s, protocol::QueryValueData &qd)
   {
   unsigned int stage_no = (unsigned int) currentStage;
   s->sendVariable(qd, phases[stage_no].name());
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
void WheatPhenology::zeroStateVariables(void)
   {
   PlantPhenology::zeroStateVariables();
   das = dlt_cumvd = cumvd = vern_eff = photop_eff = dlt_tt = dlt_tt_phenol = 0.0;
   }

void WheatPhenology::initialise (PlantComponent *s, const string &section)
   {
   s->writeString("phenology model: Wheat");
   PlantPhenology::initialise(s, section);
   zeroStateVariables();
};

void WheatPhenology::doRegistrations (protocol::Component *s)
   {
   PlantPhenology::doRegistrations(s);
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
   }

void WheatPhenology::readSpeciesParameters (PlantComponent *s, vector<string> &sections)
   {
   PlantPhenology::readSpeciesParameters (s, sections);

   stage_reduction_harvest.search(s, sections,
                                "stage_code_list" , "()", 1.0, 100.0,
                                "stage_stem_reduction_harvest" , "()", 1.0, 100.0);

   stage_reduction_kill_stem.search(s, sections,
                                  "stage_code_list" , "()", 1.0, 100.0,
                                  "stage_stem_reduction_kill_stem" , "()", 1.0, 100.0);

   y_tt.search(s, sections,
               "x_temp", "oC", 0.0, 100.0,
               "y_tt", "oC days", 0.0, 100.0);

   s->searchParameter (sections,
                       "shoot_lag"//, "(oc)"
                      , shoot_lag
                      , 0.0, 100.0);

   s->searchParameter (sections,
                       "shoot_rate"//, "(oc/mm)"
                      , shoot_rate
                      , 0.0, 100.0);

   s->searchParameter (sections,
                       "pesw_germ"//, "(mm/mm)"
                      , pesw_germ
                      , 0.0, 1.0);
   
   rel_emerg_rate.search(s, sections,
                         "fasw_emerg", "()", 0.0, 1.0,
                         "rel_emerg_rate",  "()", 0.0, 1.0);                   

   }

void WheatPhenology::readCultivarParameters(PlantComponent *s, const string & cultivar)
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

void WheatPhenology::onSow(float depth)
   {
   currentStage = 1.0;
   sowing_depth = depth;
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

void WheatPhenology::onEndCrop()
   {
   zeroStateVariables();
   }

void WheatPhenology::onHarvest()
   {
   previousStage = currentStage;
   currentStage = stage_reduction_harvest[currentStage];
   for (unsigned int stage = currentStage; stage != phases.size(); stage++)
      phases[stage].reset();
// XX one day, we'll need to be able to restart part way through a stage like this:
//   float fract = fmod(currentStage, 1.0);
//   phases[currentStage].reset(fract);
   }
void WheatPhenology::onKillStem()
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

      dlt_tt_phenol = dlt_tt *
                       min(vern_eff, photop_eff) *
                       rel_emerg_rate[fasw_seed];
      dltStage = (phase_fraction(dlt_tt_phenol) + floor(currentStage))- currentStage;
      }
   else if (inPhase("above_ground"))
      {
      float fstress = min (ps.swdef, ps.nfact);  //eme2fi: (g.swdef_pheno, g.nfact_pheno);
      if (inPhase("flowering"))
         fstress = ps.swdef_flower;              //g.swdef_pheno_flower;
      else if (inPhase("grainfill"))
         fstress = ps.swdef_grainfill;           //g.swdef_pheno_grainfill;
      else if (inPhase("harvest_ripe"))
         fstress = 0.0;                          //stop development
      dlt_tt_phenol = dlt_tt *
                      fstress *
                      min(vern_eff, photop_eff);
      dltStage = (phase_fraction(dlt_tt_phenol) + floor(currentStage))- currentStage;
      }
   else
      {
      // ??Hmmm. should probably stop dead here??
      dlt_tt_phenol = dlt_tt;
      dltStage = 0.0;
      }

   // update thermal time states and day count
   float balance_tt = 0.0, balance_days = 0.0;
   phases[(int)currentStage].add(1.0, dlt_tt_phenol, &balance_days, &balance_tt);

   // (new) stage calculation
   currentStage += dltStage;

   if ((unsigned int)currentStage >= phases.size() || currentStage < 0.0)
     throw std::runtime_error("stage has gone wild in WheatPhenology::process()..");

   // get rid of any remaining tt into the next stage
   if (balance_tt > 0.0)
      phases[(int)currentStage].add(balance_days, balance_tt);
   //if ((int)currentStage != (int)previousStage) parent->onPhenologyEvent(phases[(int)currentStage]);
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
         dltStage = 0.0;
         }
      else if ( plant_germination(pesw_germ, sowing_depth, sw) )
         {
      	dltStage = 1.0;
         }
      phase_devel = 1.999;
      new_stage = currentStage + dltStage;
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
      dltStage = new_stage - currentStage;
      }
   else if (inPhase("above_ground"))
      {
      float fstress = min (ps.swdef, ps.nfact);  //eme2fi: (g.swdef_pheno, g.nfact_pheno);
      if (inPhase("flowering"))
         fstress = ps.swdef_flower;              //g.swdef_pheno_flower;
      else if (inPhase("grainfill"))
         fstress = ps.swdef_grainfill;           //g.swdef_pheno_grainfill;
      else if (inPhase("harvest_ripe"))
         fstress = 0.0;                          //stop development
      dlt_tt_phenol = dlt_tt *
                      fstress *
                      min(vern_eff, photop_eff);

      const pPhase &current = phases[currentStage];
      phase_devel = divide(current.getTT() + dlt_tt_phenol, current.getTTTarget(), 1.0);
      new_stage = floor(currentStage) + phase_devel;
      dltStage = new_stage - currentStage;
      }
   else
      {
      // ??Hmmm. should probably stop dead here??
      dlt_tt_phenol = dlt_tt;
      dltStage = 0.0;
      }

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

   cumvd += dlt_cumvd;
   das++;
   }
#endif

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

// NB. "5.2" is half way between FI and flag leaf in wheat
void WheatPhenology::get_zadok_stage(protocol::Component *system, protocol::QueryValueData &qd)
{
    float zadok_stage = 0.0;
#if 0
    if (g.current_stage >= sowing &&
        g.current_stage <= emerg)
       {
       zadok_stage = 5.0 * (g.current_stage - sowing);
       }
    else if (g.current_stage > emerg &&
             g.current_stage <= 4.9)
       {
       float leaf_no_now = sum_between (emerg-1, now-1, g.leaf_no);

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
    else if (g.current_stage > 4.9 &&
             g.current_stage < plant_end )
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

      zadok_stage = linear_interp_real (g.current_stage
                                       , zadok_code_x
                                       , zadok_code_y
                                       , sizeof(zadok_code_x)/sizeof(float));
       }
#endif
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

void LegumePhenology::onSow(float depth)
   {
   currentStage = 1.0;
   sowing_depth = depth;
   das = 0;
   setupTTTargets();
   }
void LegumePhenology::onEndCrop()
   {
   zeroStateVariables();
   }
void LegumePhenology::onHarvest()
   {
   previousStage = currentStage;
   currentStage = stage_reduction_harvest[currentStage];
   for (unsigned int stage = currentStage; stage != phases.size(); stage++)
      phases[stage].reset();
   }
void LegumePhenology::onKillStem()
   {
   previousStage = currentStage;
   currentStage = stage_reduction_kill_stem[currentStage];
   for (unsigned int stage = currentStage; stage != phases.size(); stage++)
      phases[stage].reset();
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
   zeroStateVariables();
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

   setupGetVar("das", das,               "d", "Days after Sowing");
   setupGetVar("dlt_tt_phenol", dlt_tt_phenol,"dd", "Todays thermal time (incl. stress factors)");
   setupGetVar("dlt_tt", dlt_tt,         "dd", "Todays thermal time (no stress factors)");
   setupGetVar("dlt_cumvd", dlt_cumvd,   "", "Todays vd");
   }


void LegumePhenology::readCultivarParameters(PlantComponent *s, const string & cultivar)
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

void LegumePhenology::readSpeciesParameters (PlantComponent *s, vector<string> &sections)
   {
   PlantPhenology::readSpeciesParameters (s, sections);
   stage_reduction_harvest.search(s, sections,
                                 "stage_code_list" , "()", 1.0, 100.0,
                                 "stage_stem_reduction_harvest" , "()", 1.0, 100.0);

   stage_reduction_kill_stem.search(s, sections,
                                   "stage_code_list" , "()", 1.0, 100.0,
                                   "stage_stem_reduction_kill_stem" , "()", 1.0, 100.0);

   vernal_days.search(s, sections, 
                       "x_vernal_temp", "(oc)", -10., 60.0,
                       "y_vernal_days", "(days)", 0.0, 1.0);

   s->searchParameter (sections,
                       "shoot_lag"//, "(oc)"
                      , shoot_lag
                      , 0.0, 100.0);

   s->searchParameter (sections,
                       "shoot_rate"//, "(oc/mm)"
                      , shoot_rate
                      , 0.0, 100.0);

   y_tt.search(s, sections,
               "x_temp", "oC", 0.0, 100.0,
               "y_tt", "oC days", 0.0, 100.0);

   s->searchParameter (sections
                   ,"tt_emerg_to_endjuv_ub"//, "()"
                   , tt_emerg_to_endjuv_ub
                   , 0.0, 1.e6);

   s->searchParameter (sections
                   ,"tt_maturity_to_ripe_ub"//, "()"
                   , tt_maturity_to_ripe_ub
                   , 0.0, 1.e6);

   s->searchParameter (sections,
                       "pesw_germ"//, "(mm/mm)"
                      , pesw_germ
                      , 0.0, 1.0);

   rel_emerg_rate.search(s, sections,
                         "fasw_emerg", "()", 0.0, 1.0,
                         "rel_emerg_rate",  "()", 0.0, 1.0);

   }

void LegumePhenology::zeroStateVariables(void)
   {
   PlantPhenology::zeroStateVariables();
   das = dlt_cumvd = cumvd = dlt_tt = dlt_tt_phenol = 0.0;
   }

void LegumePhenology::zeroEverything(void)
   {
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

   //if ((int)currentStage != (int)previousStage) parent->sendPhenologyEvent(stage);
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
      const pPhase &current = phases[currentStage];
      float a =  current.getTT() + dlt_tt_phenol;
      float b =  current.getTTTarget();
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

   cumvd += dlt_cumvd;
   das++;
   }
#endif

#if 0
void LegumeCohortPhenology::init (void)
   {
   // initialise phenology phase targets
   legnew_phenology_init(c.shoot_lag
                          , c.shoot_rate
                          , g.maxt
                          , g.mint
                          , c.x_vernal_temp
                          , c.y_vernal_days
                          , c.num_vernal_temp
                          , &g.cum_vernal_days
                          , p.cum_vernal_days
                          , p.tt_emerg_to_endjuv
                          , p.num_cum_vernal_days
                          , c.twilight
                          , phenology->stageNumber()
                          , g.days_tot
                          , g.day_of_year
                          , g.year
                          , g.latitude
                          , g.sowing_depth
                          , p.x_pp_endjuv_to_init
                          , p.y_tt_endjuv_to_init
                          , p.num_pp_endjuv_to_init
                          , p.x_pp_init_to_flower
                          , p.y_tt_init_to_flower
                          , p.num_pp_init_to_flower
                          , p.x_pp_flower_to_start_grain
                          , p.y_tt_flower_to_start_grain
                          , p.num_pp_flower_to_start_grain
                          , p.x_pp_start_to_end_grain
                          , p.y_tt_start_to_end_grain
                          , p.num_pp_start_to_end_grain
                          , p.tt_end_grain_to_maturity
                          , p.tt_maturity_to_ripe
                          , p.est_days_emerg_to_init
                          , g.phase_tt);

    plant_fruit_phenology_init(c.twilight
                           , g.current_fruit_stage
                           , g.fruit_days_tot
                           , max_fruit_stage
                           , max_fruit_cohorts
                           , g.num_fruit_cohorts
                           , g.day_of_year
                           , g.latitude
                           , p.x_pp_flower_to_start_grain
                           , p.y_tt_flower_to_start_grain
                           , p.num_pp_flower_to_start_grain
                           , p.x_pp_fruit_start_to_end_grain
                           , p.y_tt_fruit_start_to_end_grain
                           , p.num_pp_fruit_start_to_end_grain
                           , p.tt_end_grain_to_maturity
                           , p.tt_maturity_to_ripe
                           , g.fruit_phase_tt);
    plant_fruit_cohort_init (
                     initial_fruit_stage
                   , gXXcurrent_stageXX
                   , g.days_tot
                   , g.current_fruit_stage
                   , &g.num_fruit_cohorts );

   }
//+  Purpose
//     Use temperature, photoperiod and genetic characteristics
//     to determine when the crop begins a new growth phase.
//     The initial daily thermal time and height are also set.
void LegumeCohortPhenology::process (void)
   {
         plant_phenology3 (&g.previous_stage
                          ,&gXXcurrent_stageXX
                          ,sowing
                          ,germ
                          ,start_grain_fill
                          ,harvest_ripe
                          ,emerg
                          ,flowering
                          ,max_stage
                          ,c.num_temp
                          ,c.x_temp
                          ,c.y_tt
                          ,g.maxt
                          ,g.mint
                          ,min(g.nfact_pheno, phosphorus->fact_pheno())
                          ,g.swdef_pheno
                          ,g.swdef_pheno_flower
                          ,g.swdef_pheno_grainfill
                          ,c.pesw_germ
                          ,c.fasw_emerg
                          ,c.rel_emerg_rate
                          ,c.num_fasw_emerg
                          ,g.dlayer
                          ,max_layer
                          ,g.sowing_depth
                          ,g.sw_dep
                          ,g.dul_dep
                          ,p.ll_dep
                          ,&g.dlt_tt
                          ,g.phase_tt
                          ,&g.phase_devel
                          ,&g.dlt_stage
                          ,g.tt_tot
                          ,g.days_tot);

         plant_fruit_cohort_init (phenology->on_day_of("initial_fruit_stage"))
                    , g.days_tot
                    , g.current_fruit_stage
                    , &g.num_fruit_cohorts );

         plant_fruit_phenology (g.previous_fruit_stage
                               ,g.current_fruit_stage
                               ,initial_fruit_stage
                               ,harvest_ripe
                               ,start_grain_fill
                               ,end_grain_fill
                               ,max_fruit_stage
                               ,max_fruit_cohorts
                               ,g.num_fruit_cohorts
                               ,g.dm_fruit_green
                               ,p.dm_fruit_max
                               ,g.fruit_no
                               ,c.num_temp
                               ,c.x_temp
                               ,c.y_tt
                               ,g.maxt
                               ,g.mint
                               ,g.swdef_pheno_flower
                               ,g.swdef_pheno_grainfill
                               ,g.fruit_phase_tt
                               ,g.fruit_phase_devel
                               ,g.dlt_fruit_tt
                               ,g.dlt_fruit_stage
                               ,g.fruit_tt_tot
                               ,g.fruit_days_tot);

         plant_fruit_phenology_update (g.previous_stage
                                      ,&gXXcurrent_stageXX
                                      ,g.current_fruit_stage
                                      ,max_fruit_cohorts
                                      ,g.num_fruit_cohorts
                                      ,c.fruit_phen_end
                                      ,g.fruit_no
                                      ,&g.dlt_stage);

         // update thermal time states and day count
         accumulate (g.dlt_tt, g.tt_tot, g.previous_stage-1.0, g.dlt_stage);
         accumulate (1.0, g.days_tot, g.previous_stage-1.0, g.dlt_stage);
   }
void LegumeCohortPhenology::plant_fruit_phenology_update (float g_previous_stage
                                         ,float *g_current_stage             // output
                                         ,float *g_current_fruit_stage
                                         ,int   max_fruit_cohorts
                                         ,int   g_num_fruit_cohorts
                                         ,float c_fruit_phen_end
                                         ,float *g_fruit_no
                                         ,float *g_dlt_stage)                // output
{
   float fruit_no_cum ;
   float fruit_no_crit ;
   int  cohort;

   const char *my_name = "plant_fruit_phenology_update";
   push_routine (my_name);

   if (phenology->inPhase ("fruit_filling"))
      {
      fruit_no_crit = 0.0 ;
      if (phenology->inPhase ("grainfill"))
         {
         cohort = 0;
         do {
            if (g_current_fruit_stage[cohort] >= initial_stage)
               {
               fruit_no_crit = fruit_no_crit + g_fruit_no[cohort];
               }
            cohort++;
            } while (cohort < g_num_fruit_cohorts);
         // C1
         // fprintf(stdout, "%d %d %f\n", g.day_of_year, g_num_fruit_cohorts, fruit_no_crit);
         fruit_no_crit = fruit_no_crit * c_fruit_phen_end;
         if (fruit_no_crit > 0.0)
            {
            fruit_no_cum = 0.0;
            cohort = 0;
            do {
               fruit_no_cum = fruit_no_cum + g_fruit_no[cohort];
               if (fruit_no_cum >= fruit_no_crit)
                  {
                  if (floor(*g_current_stage) <
                      floor(g_current_fruit_stage[cohort]))
                     {
                     *g_current_stage = floor(*g_current_stage + 1.0);
                     }
                  else
                     {
                     *g_current_stage = max(*g_current_stage,
                                            g_current_fruit_stage[cohort]);
                     }
                  break;
                  }
               cohort++;
               } while (cohort < g_num_fruit_cohorts);
            }
         else
            {
            // no fruit
            *g_current_stage = g_current_fruit_stage[0];
            }
         }
      else  // sgf->ed
         {
         // we haven't reached grain fill yet
         *g_current_stage = g_current_fruit_stage[0];
         }
      if (((int)*g_current_stage) != ((int)g_previous_stage))
         {
         *g_current_stage = (int)(*g_current_stage);
         }
      *g_dlt_stage = *g_current_stage - g_previous_stage;
      } // initial-> end devel
   else
      {
      // dlt_stage is calculated for us in phenology3 - >implies
      //*g_dlt_stage = *g_dlt_stage;
      }
   if (*g_dlt_stage < 0.0)
      {
      throw std::runtime_error ("negative dlt_stage in plant_fruit_phenology_update");
      }
   // C2
   //fprintf(stdout, "%d %f %f %d %d\n", g.day_of_year, *g_dlt_stage, *g_current_stage,g_num_fruit_cohorts, cohort);

   pop_routine (my_name);
}
////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////
// XX note *3* versions of this routine here...
float legume_stage_code(
                     float *c_stage_code_list //(INPUT)  list of stage numbers
                   , float *g_phase_tt        //(INPUT)  Cumulative growing degree days required for each stage (deg days)
                   , float *g_tt_tot          //(INPUT)  Cumulative growing degree days required for each stage (deg days)
                   , float stage_no           //(INPUT) stage number to convert
                   , float *stage_table       //(INPUT) table of stage codes
                   , int numvals, int max_stage)  //(INPUT) size_of of table(s)
  {
//*+  Purpose
//*       Return an interpolated stage code from a table of stage_codes
//*       and a nominated stage number. Returns 0 if the stage number is not
//*       found. Interpolation is done on thermal time.

//*+  Mission Statement
//*     Get the stage code from a table of stage codes

   float   phase_tt;              // required thermal time between stages
                                 //      ! (oC)
   float   fraction_of;           //!
   int     next_stage;            //! next stage number to use
   float   tt_tot;                //! elapsed thermal time between stages
                                 ///      ! (oC)
   int     this_stage;            //! this stage to use
   float   x_stage_code;          //! interpolated stage code


      if (numvals >= 2)
         {
         // we have a valid table
         this_stage = stage_no_of (stage_table[0]
                                      , c_stage_code_list
                                      , max_stage);

         for (int i = 1; i < numvals; i++)
            {
            next_stage = stage_no_of (stage_table[i]
                                         , c_stage_code_list
                                         , max_stage);

            if (stage_is_between (this_stage, next_stage, stage_no))
               {
               // we have found its place
               tt_tot = sum_between (this_stage-1, next_stage-1, g_tt_tot);
               phase_tt = sum_between (this_stage-1, next_stage-1, g_phase_tt);
               fraction_of = divide (tt_tot, phase_tt, 0.0);
               fraction_of = bound(fraction_of, 0.0, 0.999);
               x_stage_code = stage_table[i-1]
                           + (stage_table[i] - stage_table[i-1])
                           * fraction_of;
//Cx
//fprintf(stdout, "%d,%d,%.9f,%.9f,%.9f,%.9f\n", g.day_of_year,
//this_stage, tt_tot,phase_tt,fraction_of,x_stage_code);
               break;
               }
            else
               {
               x_stage_code = 0.0;
               this_stage = next_stage;
               }
            }
         }
      else
         {
         // we have no valid table
         x_stage_code = 0.0;

         throw std::runtime_error("Bad stage lookup table in legume_stage_code()");
         }

   return x_stage_code;
   }



//===========================================================================
float crop_stage_code (float *c_stage_code_list,
                       float *g_tt_tot,
                       float *g_phase_tt,
                       float stage_no,              // (INPUT) stage number to convert
                       float *stage_table,           // (INPUT) table of stage codes
                       int   numvals,                 // (INPUT) size_of of table
                       int   max_stage)               // (INPUT) max stage number
//===========================================================================

/*  Purpose
*       Return an interpolated stage code from a table of stage_codes
*       and a nominated stage number. Returns 0 if the stage number is not
*       found. Interpolation is done on thermal time.
*
*  Mission Statement
*   the crop stage code for %4
*
*  Changes
*       080994 jngh specified and programmed
*       19/5/2009 ad convert to BC++
*/
   {
   //  Local Variables
   float phase_tt;               // required thermal time between stages
                                 // (oC)
   float fraction_of;            //
   int i;                        // array index - counter
   int next_stage;               // next stage number to use
   float tt_tot;                 // elapsed thermal time between stages
                                 // (oC)
   int this_stage;               // this stage to use
   float x_stage_code;           // interpolated stage code
   // Implementation Section ----------------------------------

   if (numvals > 1)
      {
      // we have a valid table
      this_stage = stage_no_of (stage_table[0], c_stage_code_list, max_stage);

      for(i = 1; i < numvals; i++)
         {
         next_stage = stage_no_of (stage_table[i], c_stage_code_list, max_stage);

         if (stage_is_between (this_stage, next_stage, stage_no))
            {
            //we have found its place
            tt_tot = sum_between (this_stage-1, next_stage-1, g_tt_tot);
            phase_tt = sum_between (this_stage-1, next_stage-1, g_phase_tt);
            fraction_of = divide (tt_tot, phase_tt, 0.0);
            x_stage_code = stage_table[i-1]+ (stage_table[i] - stage_table[i-1]) * fraction_of;
            break;
            }
         else
            {
            x_stage_code = 0.0;
            }
         this_stage = next_stage;
         }
      }
   else
      {
      //we have no valid table
      char  error_mess[80];
      sprintf(error_mess, "Invalid lookup table in crop_stage_code(): number of values = %d", numvals);
      throw std::runtime_error(error_mess);
      }
   return x_stage_code;
   }

//==========================================================================
void crop_thermal_time (int    C_num_temp,          //(INPUT)  size_of table
                        float *C_x_temp,            //(INPUT)  temperature table for photosyn
                        float *C_y_tt,              //(INPUT)  degree days
                        float  G_current_stage,     //(INPUT)  current phenological stage
                        float  G_maxt,              //(INPUT)  maximum air temperature (oC)
                        float  G_mint,              //(INPUT)  minimum air temperature (oC)
                        int    start_stress_stage,  //(INPUT)
                        int    end_stress_stage,    //(INPUT)
                        float  G_nfact_pheno,       //(INPUT)
                        float  G_swdef_pheno,       //(INPUT)
                        float *G_dlt_tt)            //(OUTPUT) daily thermal time (oC)
//===========================================================================

/*  Purpose
*     Growing degree day (thermal time) is calculated. Daily thermal time is reduced
*     if water or nitrogen stresses occur.
*
*  Mission Statement
*   Calculate today's thermal time, %11.
*
*  Notes
*     Eight interpolations of the air temperature are
*     calculated using a three-hour correction factor.
*     For each air three-hour air temperature, a value of growing
*     degree day is calculated.  The eight three-hour estimates
*     are then averaged to obtain the daily value of growing degree
*     days.

*  Changes
*        240498 nih specified and programmed
*        19/5/2003 ad converted to BC++
*/

   {
   // thermal time for the day (deg day)
   float dly_therm_time = linint_3hrly_temp (G_maxt, G_mint, C_x_temp, C_y_tt, C_num_temp);

   if (stage_is_between (start_stress_stage, end_stress_stage,G_current_stage))
      {
      *G_dlt_tt = dly_therm_time *  min(G_swdef_pheno, G_nfact_pheno);
      }
   else
      {
      *G_dlt_tt = dly_therm_time;
      }
   }


//===========================================================================
float crop_phase_tt(float G_dlt_tt,          //(INPUT)  daily thermal time (growing de
                    float *G_phase_tt,       //(INPUT)  Cumulative growing degree days
                    float *G_tt_tot,         //(INPUT)  the sum of growing degree days
                    float current_stage)          //(INPUT) stage number
//===========================================================================

/*  Purpose
*       Return fraction of thermal time we are through the current
*       phenological phase (0-1)
*
*  Mission statement
*   the fractional progress through growth stage %4
*
*  Changes
*     010994 jngh specified and programmed
*     19/5/2003 ad converted to BC++
*/

   {
   //  Local Variables
   int stage_no;
   float dividend;
   float divisor;
   //Implementation Section ----------------------------------
   stage_no = int (current_stage);

   //assert(stage_no >= 0 && stage_no < max_stage);
   dividend = G_tt_tot[stage_no-1] + G_dlt_tt;
   divisor = G_phase_tt[stage_no-1];
   return (divide (dividend, divisor, 1.0));
   }

//===========================================================================
void crop_devel( float g_phase_devel,        //(INPUT)  development of current phase (
                 float *g_dlt_stage,           //(OUTPUT) change in growth stage
                 float *g_current_stage)       //(INPUT/OUTPUT) new stage no.
//===========================================================================

/*  Purpose
*     Determine the curent stage of development.
*
*  Mission statement
*   Determine the current stage of crop development.
*
*  Changes
*     21/5/2003 ad converted to BC++
*     010994 jngh specified and programmed
*  Return New Stage
*/

   {
   //mechanical operation - not to be changed

   // calculate the new delta and the new stage
   float new_stage = floor(*g_current_stage) + g_phase_devel;
   *g_dlt_stage = new_stage - *g_current_stage;

   if (g_phase_devel >= 1.0)
      *g_current_stage = floor((*g_current_stage) + 1.0);
   else
      *g_current_stage = new_stage;
   }
#endif

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

#if 0
//==========================================================================
void cproc_phenology1 (float  *G_previous_stage,         //   OUTPUT
                       float  *G_current_stage,          //   OUTPUT
                       int    sowing_stage,              //   IN
                       int    germ_stage,                //   IN
                       int    end_development_stage,     //   IN
                       int    start_stress_stage,        //   IN
                       int    end_stress_stage,          //   IN
                       int    max_stage,                 //   IN
                       int    C_num_temp,                //   IN
                       float *C_x_temp,                  //   IN
                       float *C_y_tt,                   //    IN
                       float  G_maxt,                    //   IN
                       float  G_mint,                    //   IN
                       float  G_nfact_pheno,             //   IN
                       float  G_swdef_pheno,             //   IN
                       float  C_pesw_germ,               //   IN
                       float *C_fasw_emerg,             //    (INPUT)
                       float *C_rel_emerg_rate,         //    (INPUT)
                       int    C_num_fasw_emerg,          //   (INPUT)
                       float *G_dlayer,                 //    IN
                       int    max_layer,                 //   IN
                       float  G_sowing_depth,            //   IN
                       float *G_sw_dep,                 //    IN
                       float *G_dul_dep,                //    IN
                       float *P_ll_dep,                 //    IN
                       float *G_dlt_tt,                 //    OUT
                       float *G_phase_tt,               //    OUT
                       float *G_phase_devel,             //   OUT
                       float *G_dlt_stage,              //    OUT
                       float *G_tt_tot,                 //    OUT
                       float *G_days_tot)               //    OUT
//===========================================================================

/*  Purpose
*     Use temperature, photoperiod and genetic characteristics
*     to determine when the crop begins a new growth phase.
*     The initial daily thermal time and height are also set.
*
*  Mission Statement
*   Calculate crop phenological development using thermal time targets.
*
*  Changes
*     240498 nih specified and programmed
*     19/5/2003 ad converted to BC++
*
*/
   {
   // Implementation Section ----------------------------------
   *G_previous_stage = *G_current_stage;

   // get thermal times
   crop_thermal_time(C_num_temp, C_x_temp, C_y_tt, *G_current_stage, G_maxt,
         G_mint, start_stress_stage, end_stress_stage, G_nfact_pheno, G_swdef_pheno,
         G_dlt_tt);

   crop_phase_devel(sowing_stage, germ_stage, end_development_stage, C_pesw_germ,
         C_fasw_emerg, C_rel_emerg_rate, C_num_fasw_emerg, *G_current_stage,
         G_days_tot, G_dlayer, max_layer, G_sowing_depth, G_sw_dep, G_dul_dep,
         P_ll_dep, *G_dlt_tt, G_phase_tt, G_tt_tot, G_phase_devel);

   crop_devel(max_stage, *G_phase_devel, G_dlt_stage, G_current_stage);

   // update thermal time states and day count
   accumulate (*G_dlt_tt, G_tt_tot, *G_previous_stage - 1.0, *G_dlt_stage);
   accumulate (1.0,     G_days_tot, *G_previous_stage - 1.0, *G_dlt_stage);
   }


//=======================================================================
void crop_germ_dlt_tt(float *C_fasw_emerg,        //(INPUT)  plant extractable soil water i
                      float *C_rel_emerg_rate,    //(INPUT)
                      int    C_num_fasw_emerg,    //(INPUT)
                      float  G_current_stage,     //(INPUT)  current phenological stage
                      int    germ_phase,          //(INPUT)
                      float *G_dlayer,            //(INPUT)  thickness of soil layer I (mm)
                      int    max_layer,           //(INPUT)
                      float  G_sowing_depth,      //(INPUT)  sowing depth (mm)
                      float *G_sw_dep,            //(INPUT)  soil water content of layer L
                      float *P_ll_dep,            //(INPUT)  lower limit of plant-extractab
                      float *G_dul_dep,           //(INPUT)  drained upper limit(mm)
                      float *G_dlt_tt)            //(IN/OUTPUT)
//===========================================================================

/*  Purpose
*      Calculate daily thermal time for germination to emergence
*      limited by soil water availability in the seed layer.
*
*  Mission statement
*   Calculate emergence adjusted thermal time (based on moisture status)
*
*  Changes
*     030498 igh  changed c_num_fasw_emerg to integer
*     19/5/2003 ad converted to BC++
*/
   {
   //  Local Variables
   int layer_no_seed;            // seedling layer number
   float fasw_seed;
   float rel_emerg_rate;         // relative emergence rate (0-1)
   int current_phase;

   current_phase = (int)G_current_stage;

   if (current_phase == germ_phase)
      {
      layer_no_seed = find_layer_no (G_sowing_depth, G_dlayer, max_layer);
      fasw_seed = divide (G_sw_dep[layer_no_seed] - P_ll_dep[layer_no_seed],
                          G_dul_dep[layer_no_seed] - P_ll_dep[layer_no_seed], 0.0);
      fasw_seed = bound (fasw_seed, 0.0, 1.0);

      rel_emerg_rate = linear_interp_real (fasw_seed, C_fasw_emerg,
                                           C_rel_emerg_rate, C_num_fasw_emerg);

      *G_dlt_tt = *G_dlt_tt * rel_emerg_rate;
      }
   else
      {
      //*G_dlt_tt = *G_dlt_tt;
      }
   }


//+  Purpose
//       Calculate number of vernal days from daily temperature

//+  Mission Statement
//     Calculate number of vernal days from daily temperature

//+  Changes
//       101299 nih specified and programmed
float legnew_vernal_days(float  g_maxt
                               ,float  g_mint
                               ,float  *c_x_vernal_temp
                               ,float  *c_y_vernal_days
                               ,int    c_num_vernal_temp) {


    //+  Local Variables
    float av_temp;
    float result;
    //- Implementation Section ----------------------------------


    av_temp = (g_maxt + g_mint)/2.0;

    result = linear_interp_real(av_temp
                                ,c_x_vernal_temp
                                ,c_y_vernal_days
                                ,c_num_vernal_temp);

    return result;
    }
// ==================================================================
// Nwheat Phenology model taken from CROPMOD module
// ==================================================================

//+  Purpose
//     Use temperature, photoperiod and genetic characteristics
//     to determine when the crop begins a new growth phase.
//     The initial daily thermal time and height are also set.

//+  Mission Statement
//   Calculate crop phenological development using thermal time targets.

//+  Changes
//     240498 nih specified and programmed
//     240599 ew reprogrammed to take out the stress in thermal time
void cproc_phenology_nw (
     float *g_previous_stage
    ,float *g_current_stage
    ,int   sowing_stage
    ,int   germ_stage
    ,int   end_development_stage
    ,int   start_stress_stage
    ,int   end_stress_stage
    ,int   end_flower_stage
    ,int   max_stage
    ,int   c_num_temp
    ,float *c_x_temp
    ,float *c_y_tt
    ,float g_maxt
    ,float g_mint
    ,float g_nfact_pheno
    ,float g_swdef_pheno
    ,float g_swdef_pheno_flower
    ,float g_swdef_pheno_grainfill
    ,float g_vern_eff
    ,float g_photop_eff
    ,float c_pesw_germ
    ,float *c_fasw_emerg            // (INPUT)
    ,float *c_rel_emerg_rate        // (INPUT)
    ,int   c_num_fasw_emerg         // (INPUT)
    ,float *g_dlayer
    ,int   max_layer
    ,float g_sowing_depth
    ,float *g_sw_dep
    ,float *g_dul_dep
    ,float *p_ll_dep
    ,float *g_dlt_tt
    ,float *g_phase_tt
    ,float *g_phase_devel
    ,float *g_dlt_stage
    ,float *g_tt_tot
    ,float *g_days_tot
    ) {

//+  Local variables
    float fstress;
    float g_dlt_tt_phenol;

    float tempcx;                                 //maximum crown temp
    float tempcn;                                 //minimum crown temp

//- Implementation Section ----------------------------------

    *g_previous_stage = *g_current_stage;

// get thermal times
    //c==============================================================================;
    //c        call crop_thermal_time_nw (;
    //c     :                             g_maxt,;
    //c     :                             g_mint,;
    //c     :                             0.0,;
    //c     :                             26.0,;
    //c     :                             34.0,;
    //c     :                             g_dlt_tt);

    //c==============================================================================;
//USE CROWN TEMPERATURE AND THREE HOURS THERMAL TIME

    crop_crown_temp_nwheat (g_maxt,g_mint,0.0,&tempcx,&tempcn);

    *g_dlt_tt = linear_interp_real((tempcx+tempcn)/2.0
                                  ,c_x_temp
                                  ,c_y_tt
                                  ,c_num_temp);

    //c         call crop_thermal_time;
    //c     :               (;
    //c     :                c_num_temp;
    //c     :              , c_x_temp;
    //c     :              , c_y_tt;
    //c     :              , g_current_stage;
    //c     :              , tempcx           ;     //G_maxt
    //c     :              , tempcn           ;     //G_mint
    //c     :              , start_stress_stage;
    //c     :              , end_stress_stage;
    //c     :              , 1.0              ;     //G_nfact_pheno
    //c     :              , 1.0              ;     //G_swdef_pheno
    //c     :              , g_dlt_tt;
    //c     :               );

    //c==============================================================================;

    if (stage_is_between (start_stress_stage,end_stress_stage, *g_current_stage))
        fstress = min (g_swdef_pheno, g_nfact_pheno);
    else if (stage_is_between (end_stress_stage,end_flower_stage, *g_current_stage))
        fstress = g_swdef_pheno_flower;
    else if (stage_is_between (end_flower_stage,end_development_stage, *g_current_stage))
        fstress = g_swdef_pheno_grainfill;
    else
        fstress = 1.0;


    //g_dlt_tt        = g_dlt_tt ;                      //*fstress Enli deleted the stress

    g_dlt_tt_phenol = (*g_dlt_tt) * fstress * min(g_vern_eff, g_photop_eff);

    crop_phase_devel (sowing_stage
                     , germ_stage
                     , end_development_stage
                     , c_pesw_germ
                     , c_fasw_emerg
                     , c_rel_emerg_rate
                     , c_num_fasw_emerg
                     , *g_current_stage
                     , g_days_tot
                     , g_dlayer
                     , max_layer
                     , g_sowing_depth
                     , g_sw_dep
                     , g_dul_dep
                     , p_ll_dep
                     , g_dlt_tt_phenol
                     , g_phase_tt
                     , g_tt_tot
                     , g_phase_devel);

    crop_devel(max_stage
               , *g_phase_devel
               , g_dlt_stage
               , g_current_stage);

    // update thermal time states and day count
    accumulate (g_dlt_tt_phenol, g_tt_tot, *g_previous_stage-1.0, *g_dlt_stage);
    accumulate (1.0, g_days_tot, *g_previous_stage-1.0, *g_dlt_stage);
    }



void plant_phenology3 (float *g_previous_stage
                             ,float *g_current_stage
                             ,int   sowing_stage
                             ,int   germ_stage
                             ,int   end_flowering_stage
                             ,int   end_development_stage
                             ,int   start_stress_stage
                             ,int   end_stress_stage
                             ,int   max_stage
                             ,int   c_num_temp
                             ,float *c_x_temp
                             ,float *c_y_tt
                             ,float g_maxt
                             ,float g_mint
                             ,float g_nfact_pheno
                             ,float g_swdef_pheno
                             ,float g_swdef_pheno_flower
                             ,float g_swdef_pheno_grainfill
                             ,float c_pesw_germ
                             ,float *c_fasw_emerg
                             ,float *c_rel_emerg_rate
                             ,int   c_num_fasw_emerg
                             ,float *g_dlayer
                             ,int   max_layer
                             ,float g_sowing_depth
                             ,float *g_sw_dep
                             ,float *g_dul_dep
                             ,float *p_ll_dep
                             ,float *g_dlt_tt
                             ,float *g_phase_tt
                             ,float *g_phase_devel // OUT
                             ,float *g_dlt_stage
                             ,float *g_tt_tot
                             ,float *g_days_tot)
  {
    //- Implementation Section ----------------------------------
    *g_previous_stage = *g_current_stage;

    if (stage_is_between(sowing_stage, end_stress_stage, *g_current_stage))
       {
       // get thermal times
       crop_thermal_time(c_num_temp
                    , c_x_temp
                    , c_y_tt
                    , *g_current_stage
                    , g_maxt
                    , g_mint
                    , start_stress_stage
                    , end_stress_stage
                    , g_nfact_pheno
                    , g_swdef_pheno
                    , g_dlt_tt );
       }
    else if (stage_is_between(end_stress_stage, end_flowering_stage, *g_current_stage))
       {
       crop_thermal_time(c_num_temp
                    , c_x_temp
                    , c_y_tt
                    , *g_current_stage
                    , g_maxt
                    , g_mint
                    , start_stress_stage
                    , end_flowering_stage
                    , g_swdef_pheno_flower  //no nstress, so just repeat swdef stress here
                    , g_swdef_pheno_flower
                    , g_dlt_tt );
       }
    else if (stage_is_between(end_flowering_stage, end_development_stage, *g_current_stage))
       {
       crop_thermal_time(c_num_temp
                    , c_x_temp
                    , c_y_tt
                    , *g_current_stage
                    , g_maxt
                    , g_mint
                    , end_flowering_stage
                    , end_development_stage
                    , g_swdef_pheno_grainfill  //no nstress, so just repeat swdef stress here
                    , g_swdef_pheno_grainfill
                    , g_dlt_tt );
       }
    else
       {
       crop_thermal_time(c_num_temp
                    , c_x_temp
                    , c_y_tt
                    , *g_current_stage
                    , g_maxt
                    , g_mint
                    , start_stress_stage
                    , end_stress_stage
                    , g_nfact_pheno
                    , g_swdef_pheno
                    , g_dlt_tt );
       }

    crop_phase_devel(sowing_stage
                       , germ_stage
                       , end_development_stage
                       , c_pesw_germ
                       , c_fasw_emerg
                       , c_rel_emerg_rate
                       , c_num_fasw_emerg
                       , *g_current_stage
                       , g_days_tot
                       , g_dlayer
                       , max_layer
                       , g_sowing_depth
                       , g_sw_dep
                       , g_dul_dep
                       , p_ll_dep
                       , *g_dlt_tt
                       , g_phase_tt
                       , g_tt_tot
                       , g_phase_devel);
    crop_devel(max_stage
             , *g_phase_devel
             , g_dlt_stage
             , g_current_stage);
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

