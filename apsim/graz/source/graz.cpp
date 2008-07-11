#pragma hdrstop
#include <sstream>
#include <math.h>

#include <ApsimShared/ApsimVersion.h>
#include <ComponentInterface2/ScienceAPI.h>
#include <ComponentInterface2/DataTypes.h>
#include <general/math_functions.h>
#include <general/stl_functions.h>
#include <general/string_functions.h>
#include <general/date_class.h>
#include <general/StringTokenizer.h>

#include "graz.h"
using namespace std;

const float gm2kg = 1.0/1000.0;           // constant to convert g to kg
const float kg2gm = 1000.0;               // conversion of kilograms to grams
const float ha2sm = 10000.0;              // conversion of hectares to sq metres
const float sm2ha = 1.0/10000.0;          // constant to convert m^2 to hectares

#define min(a, b)  (((a) < (b)) ? (a) : (b))

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// Create an instance of the GRAZ module
// ------------------------------------------------------------------
extern "C" grazComponent* STDCALL EXPORT createComponent(ScienceAPI& scienceAPI)
   {
   return new grazComponent(scienceAPI);
   }
extern "C" void STDCALL EXPORT deleteComponent(grazComponent* component)
   {
   delete component;
   }
// ------------------------------------------------------------------
// initialise the GRAZ component.
// ------------------------------------------------------------------
grazComponent::grazComponent(ScienceAPI& scienceapi)
   : scienceAPI(scienceapi)
   {
   scienceAPI.subscribe("init2", nullFunction(&grazComponent::onInit2));
   acc_eaten = 0.0;
   acc_growth = 0.0;
   alw = 0.0;
   stocking_rate = 0.0;
   pasture_source = "";
   dlt_lwg = 0.0;
   acc_eaten = 0.0;
   acc_growth = 0.0;
   intake_restr = 0.0;
   }

// ------------------------------------------------------------------
// initialise the GRAZ component - STAGE 2.
// ------------------------------------------------------------------
void grazComponent::onInit2(void)
   {
   scienceAPI.subscribe("process", nullFunction(&grazComponent::onProcess));

   //expose..  (rw)
   scienceAPI.expose("stocking_rate", "beasts/ha", "Stocking rate",   1, stocking_rate);
   scienceAPI.expose("alw",           "kg",        "Weight of beast", 1, alw);
   scienceAPI.expose("pasture_source", "",         "Crop or pasture module to eat", 1, pasture_source);

   //expose..  (r)
   scienceAPI.expose("lwg",          "kg",     "Live weight gain", 0,        dlt_lwg);
   scienceAPI.expose("acc_eaten",    "kg/ha",  "Live weight gain", 0,        acc_eaten);
   scienceAPI.expose("acc_growth",   "kg/ha",  "Cumulative grass growth", 0, acc_growth);
   scienceAPI.expose("intake_restr", "",       "Intake of feed",   0,        intake_restr);
   
   // read ...
   scienceAPI.read("intake_util_intercept", "", 0, intake_util_intercept,(float)  0.0, (float)100.0);
   scienceAPI.read("intake_util_slope",     "", 0, intake_util_slope,    (float)-100.0,(float)100.0);
   scienceAPI.read("yld_eat_restr",         "", 0, yld_eat_restr,        (float)  0.0, (float)300.0);
   scienceAPI.read("summer_lwg",            "", 0, summer_lwg,           (float)  0.0, (float)100.0);
   scienceAPI.read("autumn_lwg",            "", 0, autumn_lwg,           (float)  0.0, (float)100.0);
   scienceAPI.read("winter_lwg",            "", 0, winter_lwg,           (float)  0.0, (float)100.0);
   scienceAPI.read("spring_lwg",            "", 0, spring_lwg,           (float)  0.0, (float)100.0);
   scienceAPI.read("leaf_diet",             "", 0, leaf_diet,            (float)  0.0, (float)100.0);
   scienceAPI.read("std_alw",               "", 0, std_alw,              (float)  0.0, (float)1000.0);
   scienceAPI.read("metabol_expon",         "", 0, metabol_expon,        (float)  0.0, (float)100.0);
   scienceAPI.read("prop_can_eat",          "", 0, prop_can_eat,         (float)  0.0, (float)100.0);
   scienceAPI.read("acc_eaten_reset",       "", 0, acc_eaten_reset,      (int)  0, (int)366);
   scienceAPI.read("allow_supplements",     "", 0, allow_supplements);

   // (optional) initial values
   scienceAPI.read("stocking_rate", "beasts/ha", 1, stocking_rate,(float)  0.0, (float)100.0);
   scienceAPI.read("alw",           "kg",        1, alw,          (float)  0.0, (float)1000.0);
   scienceAPI.read("pasture_source","",          1, pasture_source);

   cout << endl;
   cout << "------- " << scienceAPI.name() << " Initialisation ";
   cout.width(79-24-scienceAPI.name().length());
   cout.fill('-');
   cout << '-' << endl;
   cout.fill(' ');
   cout << "  Initial Stocking rate: " << stocking_rate << endl;
   cout << "  Initial Live Weight: " << alw << endl;
   cout << endl;
   }


void grazComponent::onPrepare(void)
   {
   // Gather other variables
   scienceAPI.get("day", "", 0, jday, 1, 366);
   scienceAPI.get("month", "", 0, month, 1, 12);
   scienceAPI.get("year", "", 0, year, 1800, 2100);

   std::string prefix;
   if (pasture_source == "") 
      prefix = "";
   else
      prefix = pasture_source + ".";

   scienceAPI.get(prefix + "LeafGreenWt",    "g/m^2", false, green_leaf,   (float)0.0, (float)10000.0);
   scienceAPI.get(prefix + "StemGreenWt",    "g/m^2", false, green_stem,   (float)0.0, (float)10000.0);
   // NB. This ignores dm_dead_xx - dm attached to dead plants. FIXME!!
   scienceAPI.get(prefix + "LeafSenescedWt", "g/m^2", false, dead_leaf,    (float)0.0, (float)10000.0);
   scienceAPI.get(prefix + "StemSenescedWt", "g/m^2", false, dead_stem,    (float)0.0, (float)10000.0);
   scienceAPI.get("growth",           "kg/ha", false, grass_growth, (float)0.0, (float)10000.0);
   
   // Set deltas
   green_leaf_eaten = 0.0;
   green_stem_eaten = 0.0;
   dead_leaf_eaten = 0.0;
   dead_stem_eaten = 0.0;
   dlt_lwg = 0.0;
   dead_leaf_tramp = 0.0;
   dead_stem_tramp = 0.0;
   }

void grazComponent::onProcess(void)
   {
   onPrepare();

   if (stocking_rate > 0.0) 
      eat();
      
   event ();
   update ();
   }
   
void grazComponent::eat(void)
   {
   float green_pool, dead_pool;
   float tsdm;
   float green_prop;           // Proportion of green in tsdm (kg/ha)
   float green_prop_leaf;      // Proportion of green leaf in tsdm (kg/ha)
   float dead_prop_leaf;       // Proportion of dead leaf in tsdm (kg/ha)
   float mod_green_prop;       // adjusted green biomass ratio (0.1-1)
   float green_diet;           // proportion of green in diet
   float prop_consumed;        // (biomass eaten : biomass grown) since
                               // start of season
   float intake_restr_growth;  // restriction of intake by proportion of
                               // growth over season (0-1)
   float intake_restr_tsdm;    // restriction of intake by low level of tsdm
   float anim_intake;          // intake of biomass (kg/beast)
   float tsdm_eaten;           // biomass eaten     (kg/ha)
   float green_eaten, dead_eaten; // pool eaten     (kg/ha)
   float curve_factor;         // competition curve
   float trampled;             // trampled dead leaf + stem
   float trampled_stem;        // trampled dead stem
   float trampled_leaf;        // trampled dead leaf
   float stock_equiv;          // stock equivalent

//   const float MIN_ALW = 10.0;
   green_pool = green_leaf + green_stem;
   dead_pool = dead_leaf + dead_stem;
   tsdm = green_pool + dead_pool;

   green_prop = divide (green_pool, tsdm, 0.0);
   green_prop_leaf = divide (green_leaf, green_pool, 0.0);
   dead_prop_leaf = divide (dead_leaf, dead_pool, 0.0);

   mod_green_prop = (green_prop - 0.10) / 0.90;
   mod_green_prop = bound (mod_green_prop, 0.0, 1.0);

   // If green is less than 10%, no active selection for green by stock
   if (mod_green_prop > 0.0) {
      green_diet = graz_comp_curve(mod_green_prop, 19.0);
   } else {
      green_diet = green_prop;
   }

   // Calculate utilization and its effects on intake
   if (acc_growth > 0.01) {
      prop_consumed = divide(acc_eaten, acc_growth, 0.0);
      prop_consumed = min(1.0, prop_consumed);
   } else {
      prop_consumed = 0.0;
   }

   // restriction by proportion of growth:
   intake_restr_growth = intake_util_intercept + intake_util_slope * prop_consumed;

   // restriction by low tsdm:
   intake_restr_tsdm = divide (tsdm, yld_eat_restr, 0.0);

   intake_restr = min (intake_restr_tsdm, intake_restr_growth) ;
   intake_restr = bound (intake_restr, 0.0, 1.0);

   // This is the animal lwg model.
   anim_intake = intake_restr * divide (graz_pot_lwg () + 1.058, 0.304, 0.0);

   // Restrict intake such that the herd cannot eat more than is
   //     in sward, then adjust individual animal intake accordingly
   stock_equiv = graz_stock_equiv ();
   tsdm_eaten = stock_equiv * anim_intake;
   tsdm_eaten = bound (tsdm_eaten, 0.0, tsdm);

   if (stock_equiv > 0) {
      anim_intake = divide(tsdm_eaten, stock_equiv, 0.0);
   } else {
      anim_intake = 0.0;
   }

   //  Lwg calculation.
   //    if supplementing, there is no restriction effect on LWG
   if (allow_supplements) {
      dlt_lwg = graz_pot_lwg();
   } else {
      dlt_lwg = anim_intake * 0.304 - 1.058;
   }

   //  Restrict the lwg, so that alw never goes below minimum
// not in spaghetti !!!
//      dlt_lwg = max (MIN_ALW - alw, dlt_lwg)

   curve_factor = divide (0.5 * leaf_diet - leaf_diet,
                          0.5 * leaf_diet - 0.5, 0.0);

   green_eaten = green_diet * tsdm_eaten;
   green_eaten = min(green_pool, green_eaten);
   dead_eaten = (1.0 - green_diet) * tsdm_eaten;
   dead_eaten = min(dead_pool, dead_eaten);

   green_leaf_eaten = green_eaten *
          graz_comp_curve(green_prop_leaf, curve_factor);
   green_leaf_eaten = max(green_leaf, green_leaf_eaten);

   dead_leaf_eaten = dead_eaten *
          graz_comp_curve(dead_prop_leaf, curve_factor);
   dead_leaf_eaten = max(dead_leaf, - dead_leaf_eaten);

   green_stem_eaten = green_eaten - green_leaf_eaten;
   green_stem_eaten = max(green_stem, green_stem_eaten);

   dead_stem_eaten = dead_eaten - dead_leaf_eaten ;
   dead_stem_eaten = max(dead_stem, dead_stem_eaten);

   //  Trampling
   trampled = tsdm_eaten * ( divide(1.0, prop_can_eat, 0.0) - 1.0);

   //  Apportion to leaf and stem
   trampled_leaf = trampled * graz_comp_curve(dead_prop_leaf, curve_factor);
   trampled_stem = trampled - trampled_leaf;

   // Limit the trampling so that we don't trample more than is
   //  actually there.
   dead_leaf_tramp = max(dead_leaf - dead_leaf_eaten, trampled_leaf);
   dead_stem_tramp = max(dead_stem - dead_stem_eaten, trampled_stem);
   }   

// Calculate stock equivalent
float grazComponent::graz_stock_equiv (void)
   {
   return (stocking_rate * (pow(divide (alw, std_alw, 0.0), metabol_expon)));
   }

// Calculate potential lwg.
float grazComponent::graz_pot_lwg (void) 
   {
   enum { SUMMER, AUTUMN, WINTER, SPRING};
   const float DAYS_PER_SEASON = 91.25;
   const int season[] = {-1,SUMMER,SUMMER,
                            AUTUMN,AUTUMN,AUTUMN,
                            WINTER,WINTER,WINTER,
                            SPRING,SPRING,SPRING,SUMMER};
   if (month < 1 || month > 12)
      throw ("Unknown month??");

   if (season[month] == SUMMER) {
       return(divide (summer_lwg, DAYS_PER_SEASON, 0.0));
   } else if (season[month] == AUTUMN) {
       return(divide (autumn_lwg, DAYS_PER_SEASON, 0.0));
   } else if (season[month] == WINTER) {
       return( divide (winter_lwg, DAYS_PER_SEASON, 0.0));
   } else if (season[month] == SPRING) {
       return(divide (spring_lwg, DAYS_PER_SEASON, 0.0));
   }
   throw ("Unknown season??");
}

float grazComponent::graz_comp_curve(float ndx, float a)
{
//*     Standard competition curve (or at least so McKeon calls it) This
//*     function is used by McKeon in several places to transform an index
//*     in the range [0-1] to another index in the same range, but
//*     weighted in a different way. The weighting is controlled by the a
//*     parameter. An a value of 1 leaves the index untransformed.
   return( divide(a * ndx, (ndx * (a - 1) + 1), 0.0));
}

void grazComponent::event (void)
   {
   if (jday == acc_eaten_reset) 
      {
      acc_eaten = 0.0;
      acc_growth = green_leaf + green_stem;
      }
   }

void grazComponent::update (void)
   {
   acc_eaten += 
      green_leaf_eaten +
      green_stem_eaten +
      dead_leaf_eaten +
      dead_stem_eaten;

   acc_growth += grass_growth;

   alw += dlt_lwg;

   if (dead_leaf_eaten + green_leaf_eaten +
       dead_stem_eaten + green_stem_eaten > 0.0 ) 
      { 
      RemoveCropDmType dmEaten;
      dmType greenEaten;

      greenEaten.pool = "green";
      greenEaten.part.push_back("leaf");
      greenEaten.dlt.push_back(green_leaf_eaten * kg2gm / ha2sm);
      greenEaten.part.push_back("stem");
      greenEaten.dlt.push_back(green_stem_eaten * kg2gm / ha2sm);
      dmEaten.dm.push_back(greenEaten);

      // NB. our "dead pool" is the plant "senesced pool".. Ideally, we should treat
      //  senesced and dead pools equally.
      dmType deadEaten;
      deadEaten.pool = "senesced";                                
      deadEaten.part.push_back("leaf");
      deadEaten.dlt.push_back(
           (dead_leaf_eaten - dead_leaf_tramp) * kg2gm / ha2sm);   // send in g/sm
      deadEaten.part.push_back("stem");
      deadEaten.dlt.push_back(
           (dead_stem_eaten - dead_stem_tramp) * kg2gm / ha2sm);
      dmEaten.dm.push_back(deadEaten);

      string s;
      if (pasture_source == "") 
         s = "remove_crop_biomass";
      else
         s = pasture_source + ".remove_crop_biomass";

      scienceAPI.publish(s, dmEaten);  
      }

   if (dead_leaf_tramp + dead_stem_tramp > 0.0 ) 
      {
      RemoveCropDmType dmTrampled;
         
      dmType deadTrampled;
      deadTrampled.pool = "senesced";
      deadTrampled.part.push_back("leaf");
      deadTrampled.dlt.push_back(dead_leaf_tramp * kg2gm / ha2sm);   // send in g/sm
      deadTrampled.part.push_back("stem");
      deadTrampled.dlt.push_back(dead_stem_tramp * kg2gm / ha2sm);
      dmTrampled.dm.push_back(deadTrampled);
      
      string s;
      if (pasture_source == "") 
         s = "detach_crop_biomass_rate";
      else
         s = pasture_source + ".detach_crop_biomass_rate";
         
      scienceAPI.publish(s, dmTrampled);
      }
   }


double divide (double dividend, double divisor, double default_value)
//===========================================================================

/*Definition
 *   Returns (dividend / divisor) if the division can be done
 *   without overflow or underflow.  If divisor is zero or
 *   overflow would have occurred, a specified default is returned.
 *   If underflow would have occurred, zero is returned.
 *Assumptions
 *   largest/smallest real number is 1.0e+/-30
 *Parameters
 *   dividend:     dividend
 *   divisor:      divisor
 *   defaultValue: default value to return if overflow
 *Calls
 *   reals_are_equal
 */

   {
   //Constant Values
   double LARGEST = 1.0e30;    //largest acceptable no. for quotient
   double SMALLEST = 1.0e-30;  //smallest acceptable no. for quotient
   double nought = 0.0;

   //Local Varialbes
   double quotient;

   //Implementation
   if(isEqual(divisor, 0.0))  //dividing by 0
      {
      quotient = default_value;
      }
   else if(isEqual(dividend, 0.0))      //multiplying by 0
      {
      quotient = 0.0;
      }

   else if(fabs(divisor) < 1.0)            //possible overflow
      {
      if(fabs(dividend) > fabs(LARGEST * divisor)) //overflow
         {
         quotient = default_value;
         }
      else
         {
         quotient = dividend / divisor;          //ok
         }
      }
   else if(fabs(divisor) > 1.0)             //possible underflow
      {
      if(fabs(dividend) < fabs(SMALLEST * divisor))    //underflow
         {
         quotient = nought;
         }
      else
         {
         quotient = dividend / divisor;                //ok
         }
      }
   else
      {
      quotient = dividend / divisor;                   //ok
      }
   return quotient;
   }


float bound(float var, float lower, float upper)
//===========================================================================

/*Definition
 *   Returns "lower", if "var" is less than "lower".  Returns "upper"
 *   if "var" is greater than "upper".  Otherwise returns "var".  A
 *   warning error is flagged if "lower" is greater than "upper".
 *Parameters
 *   var:   variable to be constrained
 *   lower: lower limit of variable
 *   upper: upper limit of variable
 *Calls
 *   l_bound
 *   u_bound
 *   warningError
 */

   {
   //Local variables
   float result;           // result for return
   float high;             //temporary variable constrained
                           //to upper limit of variable

   numeric_limits<float> mathInfo;
   float epsilon =  mathInfo.epsilon();

   //check that lower & upper bounds are valid
//   if (lower > upper)
   if ((lower - epsilon) > (upper + epsilon))
      {
      // bounds are invalid
      char msg[80];
      sprintf(msg,
            "Lower bound %f is > upper bound %f\n",
            lower, upper);
      throw std::invalid_argument(msg);
      }
   // constrain variable
   high = u_bound (var, upper);
   result = l_bound (high, lower);
   return result;
   }


//===========================================================================
float l_bound (float var, float lower)
//===========================================================================

/*Definition
 *   Returns "var" providing that it is greater than or equal to
 *   the lower bound, "lower".  Otherwise returns "lower".
 *Parameters
 *   var:   variable to be constrained
 *   lower: lower limit of variable
 *Calls
 *
 */

   {
   if (var < lower) return lower;
   return var;
   }

//===========================================================================
float u_bound (float var, float upper)
//===========================================================================

/*Definition
 *   Returns "var" providing that it is less than or equal to the
 *   upper bound, "upper".  Otherwise returns "upper".
 *Parameters
 *   var:   variable to be constrained
 *   upper: upper limit of variable
 *Calls
 *
 */

   {
   if (var > upper) return upper;
   return var;
   }

