
// Modification log
// 6 Aug 97 J. Hargreaves  Implementation

#define YES 1
#define NO 0
#define TEST_PlantFruit NO					// build unit test?
#include <vcl.h>
#include <math.h>

#include <stdio.h>
#include <math.h>
#include <map>
#include <string>
#include <algorithm>
#include <stdexcept>
#include <cstring.h>
#include <iostream.h>
#include <boost/function.hpp>
#include <boost/bind.hpp>

using namespace std;

#include <ComponentInterface/Type.h>
#include <ComponentInterface/ApsimVariant.h>
#include <ComponentInterface/Component.h>
#include <ComponentInterface/dataTypes.h>
#include <ComponentInterface/Messages.h>
#include <ComponentInterface/MessageDataExt.h>
#include <ApsimShared/ApsimComponentData.h>
#include <ApsimShared/FStringExt.h>
#include <general/string_functions.h>
#include "PlantLibrary.h"
#include "PlantComponent.h"
#include "PlantPhenology.h"
#include "Plant.h"
#ifndef PLANTFRUIT_H
#include "PlantFruit.h"
#endif

      inline bool floatsAreEqual(float A, float B, float C) {return(fabs(A-B)<C);}

// default constructor
// 	initialise data members.
PlantFruit::PlantFruit(Plant *P)  			 // member initialisation
{
   plant = P;
   zeroVariables();
}

//PlantFruit::PlantFruit(float greenShell, float greenMeal, float senescedShell, float senescedMeal, float deadShell, float deadMeal)
//{
//
//	green.shell = greenShell;
//	green.meal = greenMeal;
//	senesced.shell = senescedShell;
//	senesced.meal = senescedMeal;
//	dead.shell = deadShell;
//	dead.meal = deadMeal;
//}

// destructor
PlantFruit::~PlantFruit()
{
}

ostream &operator<<(ostream &output, const PlantFruit &pool)
{
	output << "PlantFruit:" << endl;
	output << "   Green cover:    " << pool.cover.green << endl;
	output << "   Senesced cover: " << pool.cover.sen << endl;
	output << "   Dead cover:     " << pool.cover.dead << endl;
	output << endl;
//	output << "   Green shell:    " << pool.green.shell << endl;
//	output << "   Green meal:    " << pool.green.meal << endl;
//	output << "   Senesced shell: " << pool.senesced.shell << endl;
//	output << "   Senesced meal: " << pool.senesced.meal << endl;
//	output << "   Dead shell:     " << pool.dead.shell << endl;
//	output << "   Dead meal:     " << pool.dead.meal << endl << endl;
	output << endl;
      return output;
}

// copy constructor
//	copy data members of object
//===========================================================================
PlantFruit::PlantFruit(const PlantFruit &PlantFruit)
//===========================================================================
{
	throw std::invalid_argument("Copy constructor NI for plantFruit");
}


// Assigment operator
//	assign data members of object
//===========================================================================
const PlantFruit &PlantFruit::operator=(const PlantFruit &other)
//===========================================================================
{
	throw std::invalid_argument("Assignment operator NI for plantFruit");
}


// ====================================================================
void PlantFruit::doInit (PlantComponent *systemInterface, PlantPhenology *plantPhenology)
// ====================================================================
{
   parentPlant = systemInterface;
   phenology = plantPhenology;
}


// Command
//===========================================================================
float PlantFruit::divide (float dividend, float divisor, float default_value) const
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
   const float LARGEST = 1.0e30;    //largest acceptable no. for quotient
   const float SMALLEST = 1.0e-30;  //smallest acceptable no. for quotient
   const float nought = 0.0;
   const float one = 1.0;
   const float granularity = 1.0e-6;

   //Local Varialbes
   float quotient;

   //Implementation
   if(floatsAreEqual(dividend, nought, granularity))      //multiplying by 0
      {
      quotient = nought;
      }
   else if(floatsAreEqual(divisor, nought, granularity))  //dividing by 0
      {
      quotient = default_value;
      }
   else if(fabs(divisor) < one)            //possible overflow
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
   else if(fabs(divisor) > one)             //possible underflow
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


//===========================================================================
//void PlantFruit::setValue(float greenShell, float greenMeal, float senescedShell, float senescedMeal, float deadShell, float deadMeal)
//===========================================================================
//{
//
//	green.shell = greenShell;
//	green.meal = greenMeal;
//	senesced.shell = senescedShell;
//	senesced.meal = senescedMeal;
//	dead.shell = deadShell;
//	dead.meal = deadMeal;
//}



// Query
float PlantFruit::coverTotal() const
{
   return 1.0 - (1.0 - cover.green) * (1.0 - cover.sen) * (1.0 - cover.dead);
}

float PlantFruit::coverGreen() const
{
   return cover.green;
}

float PlantFruit::coverDead() const
{
   return cover.dead;
}

float PlantFruit::coverSen() const
{
   return cover.sen;
}

//float PlantFruit::total() const
//{
//
//	return green.shell + green.meal + senesced.shell + senesced.meal + dead.shell + dead.meal;
//}

void PlantFruit::display(ostream &os) const
{
	os << "PlantFruit:" << endl;
	os << "Green cover:    " << cover.green << endl;
	os << "Senesced cover: " << cover.sen << endl;
	os << "Dead cover:     " << cover.dead << endl;
//	os << "Green shell: " << green.shell << endl;
//	os << "Green meal: " << green.meal << endl;
//	os << "Senesced shell: " << senesced.shell << endl;
//	os << "Senesced meal: " << senesced.meal << endl;
//	os << "Dead shell: " << dead.shell << endl;
//	os << "Dead meal: " << dead.meal << endl << endl;
	os << endl;
}


void PlantFruit::zeroVariables()
{
   cover.green = 0.0;
   cover.sen   = 0.0;
   cover.dead  = 0.0;
   g.pai = 0.0;
   c.extinctionCoeff = 0.0;

//   green.shell = 0.0;
//   green.meal = 0.0;
//   green.oil = 0.0;
//   senesced.shell = 0.0;
//   senesced.meal = 0.0;
//   senesced.oil = 0.0;
//   dead.shell = 0.0;
//   dead.meal = 0.0;
//   dead.oil = 0.0;
}

//===========================================================================
void PlantFruit::read_constants (Plant *systemInterface)
//===========================================================================
{
//+  Purpose
//       Read all fruit constants.

//+  Constant Values
    const char*  section_name = "constants" ;

//+  Local Variables
    int    numvals;                  // number of values read
    int    num_sections;             // number of sections to search
    int    part;
    float  temp[max_table];
    string scratch;
//- Implementation Section ----------------------------------


//    systemInterface->readParameter (section_name, "cExtinctionCoeff",    /*"()",   */ c.pfact_photo_slope    , 0.0, 100.0);
}

//===========================================================================
float PlantFruit::calcCover (
                        float cExtinctionCoeff    // extinction coefficient of pod
                      , float pai               // pod area index
                        )
//===========================================================================
{

//+  Purpose
//     Calculate pod cover

//+  Changes
//     02 Feb 2005 JNGH - Programmed and Specified

//+  Local Variables
    float cover;                // pod cover in canopy

//- Implementation Section ----------------------------------
    if (pai > 0.0)
        cover = 1.0 - exp(-cExtinctionCoeff * pai);
    else
        cover = 0.0;

    return cover;
}

//===========================================================================
float PlantFruit::interceptRadiation (
                                      float radiation    // incident radiation on pods
                                     )
//===========================================================================
{

//+  Purpose
//     Calculate pod total radiation interception and return transmitted radiation

//+  Changes
//     02 Feb 2005 JNGH - Programmed and Specified


//- Implementation Section ----------------------------------

   float radiationIntercepted = coverTotal() * radiation;
   return radiation - radiationIntercepted;
}


//===========================================================================
void PlantFruit::legnew_bio_grain_oil (
                                   float  c_grain_oil_conc          // (INPUT) fractional oil content of grain (0-1)
                                  ,float  c_carbo_oil_conv_ratio    // (INPUT) Carbohydrate:oil conversion ratio (>= 1.0)
                                  ,float  *grain_energy              // (OUTPUT) multiplier of grain weight to account
                                 )                                // for seed energy content (>= 1.0)
//===========================================================================
{
   //+  Purpose
//       Calculate grain oil factors

//+  Changes
//      141100 jngh specified and programmed

//- Implementation Section ----------------------------------

    *grain_energy = 1.0 + c_grain_oil_conc * (c_carbo_oil_conv_ratio - 1.0);
    bound_check_real_var (parentPlant, *grain_energy, 1.0, 2.0, "grain_energy");
    }

//===========================================================================
void PlantFruit::legnew_bio_yieldpart_demand1(
                                         float c_twilight                          // (INPUT)  twilight in angular distance b
                                        ,int   g_day_of_year                       // (INPUT)  day of year
                                        ,float g_latitude                          // (INPUT)  latitude (degrees, negative fo
                                        ,int  *yield_parts                         // (INPUT)
                                        ,int   num_yield_parts                     // (INPUT)
                                        ,int   root_part                           // (INPUT)
                                        ,int   max_part                            // (INPUT)
                                        ,float g_dlt_dm                            // (INPUT)  the daily biomass production (
                                        ,float *g_dm_green                         // (INPUT)  live plant dry weight (biomass
                                        ,float *g_dm_senesced                      // (INPUT)  senesced plant dry wt (g/m^2)
                                        ,float g_dm_stress_average
                                        ,float *p_x_pp_hi_incr                     // (INPUT)
                                        ,float *p_y_hi_incr                        // (INPUT)  harvest index increment per da
                                        ,int   p_num_pp_hi_incr                    // (INPUT)
                                        ,float *p_x_hi_max_pot_stress              // (INPUT) Potential Max HI Stress dete
                                        ,float *p_y_hi_max_pot                     // (INPUT) Potential Max HI
                                        ,int   p_num_hi_max_pot                    // (INPUT) Number of lookup pairs
                                        ,float g_grain_energy                      // (INPUT)
                                        ,float *dlt_dm_yieldpart_demand             // (OUTPUT) grain dry matter potential (g/m^2)
                                        )
//===========================================================================
{
//+  Purpose
//        Find grain demand for carbohydrate using harvest index (g/m^2)

//+  Mission Statement
//   Calculate yield component biomass demand using harvest index increments

//+  Changes
//     010994 jngh specified and programmed

//+  Local Variables
//    float ave_stress;                             // average dm_stress from flowering to gra
    float dlt_dm_yield;                           // grain demand for carbohydrate (g/m^2)
    float dlt_dm_yield_unadj;                     // grain demand for carbohydrate, unadjusted
// for energy conversion (g/m^2)
    float dm_tops;                                // drymatter of tops (g/m^2)
    float harvest_index;                          // last harvest index (g grain/g biomass)
    float hi_max_pot;                             // max potential HI due to stress
    float dm_tops_new;                            // new drymatter  tops (g/m^2)
    float harvest_index_new;                      // next harvest index (g grain/g biomass)
    float dm_grain_new;                           // new drymatter grain (g/m^2)
    float dm_green_yield_parts;                   // dry matter of yield parts (g/m^2)
    float energy_adjust;                          // adjustment for energy used in oil conversion.
    int   indx;                                   // loop index
    float hi_incr;                                // harvest index increment per day
    float photoperiod;                            // hours of photosynthetic light (hours)

//- Implementation Section ----------------------------------

    if (phenology->inPhase("grainfill"))
        {

//        ave_stress = divide ((*g_dm_stress_max).getSum(),
//                             (*g_dm_stress_max).getN(),
//                             1.0);
        hi_max_pot = linear_interp_real(g_dm_stress_average
                                        ,p_x_hi_max_pot_stress
                                        ,p_y_hi_max_pot
                                        ,p_num_hi_max_pot);

        photoperiod = day_length (g_day_of_year, g_latitude, c_twilight);

        hi_incr = linear_interp_real(photoperiod
                                    ,p_x_pp_hi_incr
                                    ,p_y_hi_incr
                                    ,p_num_pp_hi_incr);

    // effective grain filling period

        dm_tops = sum_real_array (g_dm_green, max_part)
                         - g_dm_green[root_part]
                         + sum_real_array (g_dm_senesced, max_part)
                         - g_dm_senesced[root_part];
        dm_green_yield_parts = 0.0;

        for (indx = 0; indx < num_yield_parts; indx++)
            {
            dm_green_yield_parts = dm_green_yield_parts
                         + g_dm_green[yield_parts[indx]];
            }
        harvest_index = divide (dm_green_yield_parts, dm_tops, 0.0);
        dm_tops_new = dm_tops + g_dlt_dm;

        harvest_index_new = u_bound (harvest_index + hi_incr, hi_max_pot);

        dm_grain_new = dm_tops_new * harvest_index_new;
        dlt_dm_yield_unadj = dm_grain_new - dm_green_yield_parts;

    // adjust for grain energy

        dlt_dm_yield_unadj = bound (dlt_dm_yield_unadj, 0.0, dm_grain_new);

        energy_adjust = divide (g_grain_energy
                                , 1.0 + harvest_index_new*(g_grain_energy - 1.0)
                                , 0.0);

        dlt_dm_yield = dlt_dm_yield_unadj * energy_adjust;
    //jh         dlt_dm_yield = dlt_dm_yield_unadj
        }
    else
        {
        // we are out of grain fill period
        dlt_dm_yield = 0.0;
        }


    *dlt_dm_yieldpart_demand = dlt_dm_yield;

    return;
    }

//===========================================================================
void PlantFruit::plant_grain_n_demand1(float c_sfac_slope            //   (INPUT)  soil water stress factor slope
                                      , float c_sw_fac_max            //   (INPUT)  soil water stress factor maxim
                                      , float c_temp_fac_min          //   (INPUT)  temperature stress factor mini
                                      , float c_tfac_slope            //   (INPUT)  temperature stress factor slop
                                      , float g_maxt                  //   (INPUT)  maximum air temperature (oC)
                                      , float g_mint                  //   (INPUT)  minimum air temperature (oC)
                                      , float g_nfact_grain_conc      //   (INPUT)
                                      , float *g_n_conc_crit          //   (INPUT)  critical N concentration (g N/
                                      , float g_swdef_expansion       //   (INPUT)
                                      , float *g_n_conc_min           //   (INPUT)  minimum N concentration (g N/g
                                      , float *g_dlt_dm_green         //   (INPUT)  plant biomass growth (g/m^2)
                                      , float *g_dlt_dm_green_retrans //   (INPUT)  plant biomass growth (g/m^2)
                                      , float *g_dm_green             //   (INPUT)  live plant dry weight (biomass
                                      , float *g_n_conc_max           //   (INPUT)  maximum N concentration (g N/g
                                      , float *g_n_green              //   (INPUT)  plant nitrogen content (g N/m^
                                      , float *grain_n_demand)        //   grain N demand (g/m^2)
//===========================================================================
  {
//  Purpose
//    Calculate plant n demand

      float   n_avail[max_part];     // N available for transfer to grain
                                     // (g/m^2)
      float   n_avail_stover;        // total N available in stover
                                     // (g/m^2)
      float   n_potential;           // maximum grain N demand (g/m^2)
      int     part;                  // plant part number

//- Implementation Section ----------------------------------

      *grain_n_demand = (g_dlt_dm_green[meal] + g_dlt_dm_green_retrans[meal])
             * crop_n_dlt_grain_conc(meal,
                            c_sfac_slope
                          , c_sw_fac_max
                          , c_temp_fac_min
                          , c_tfac_slope
                          , g_maxt
                          , g_mint
                          , g_nfact_grain_conc
                          , g_n_conc_crit
                          , g_n_conc_min
                          , g_swdef_expansion);


      n_potential  = (g_dm_green[meal]
                     + g_dlt_dm_green[meal]
                     + g_dlt_dm_green_retrans[meal])
               * g_n_conc_max[meal];


      *grain_n_demand = u_bound (*grain_n_demand
                                , n_potential - g_n_green[meal]);

   }

//===========================================================================
float PlantFruit::dm_yield_demand ( float  c_frac_pod                    // (INPUT)  fraction of remaining dm allocated to pod
                                  , float  g_grain_energy                // multiplier of grain weight to account f
                                  , float  g_dlt_dm_veg                  // (INPUT)  the daily vegetative biomass production (g/m^2)
                                  , double g_dlt_dm                      // (INPUT)  the daily biomass production of pod (g/m^2)
                                  , float  g_dlt_dm_grain_demand         // (INPUT)  grain dm demand (g/m^2)
                                  )
//===========================================================================
{

//+  Purpose
//       Calculate grain dm yield demand (g/m^2)
//       (OUTPUT) assimilate demand for reproductive part (g/m^2)
//+  Changes
//      170703 jngh specified and programmed

//+  Local Variables
      float      dm_grain_demand;       // assimilate demand for grain (g/m^2)
      float      dm_pod_demand;         // assimilate demand for pod (g/m^2)
      float      dm_yield_demand;       // (OUTPUT) assimilate demand for reproductive part (g/m^2)

//- Implementation Section ----------------------------------

         // calculate demands of reproductive parts

      dm_grain_demand = divide (g_dlt_dm_grain_demand, g_grain_energy, 0.0);

      if (dm_grain_demand > 0.0)
      {
         dm_pod_demand = dm_grain_demand * c_frac_pod;
      }
      else
      {
         dm_pod_demand = g_dlt_dm_veg * c_frac_pod;        // fix
      }

      dm_yield_demand = dm_pod_demand
                      + g_dlt_dm_grain_demand
                      - g_dlt_dm;

      return dm_yield_demand;
}

//     ===========================================================
void PlantFruit::dm_partition1 (
                                float  c_frac_pod                    // (INPUT)  fraction of remaining dm allocated to pod
                               ,float  g_grain_energy                // multiplier of grain weight to account f
                               ,float  c_grain_oil_conc              // multiplier of grain weight to account f
                               ,double g_dlt_dm                      // (INPUT)  the daily biomass production (
                               ,float  g_dlt_dm_grain_demand         // (INPUT)  grain dm demand (g/m^2)
                               ,float  *dlt_dm_oil_conv               // (OUTPUT) actual biomass used in conversion to oil (g/m2)
                               ,float  *dlt_dm_green                  // (OUTPUT) actual biomass partitioned to plant parts (g/m^2)
                               )
//     ===========================================================
{

//+  Purpose
//       Partitions new dm (assimilate) between plant components (g/m^2)

//+  Changes
//      170703 jngh specified and programmed

//+  Local Variables
    double dlt_dm_green_tot;                       // total of partitioned dm (g/m^2)
    double dm_remaining;                           // interim dm pool for partitioning
    double yield_demand;                           // sum of grain, energy & pod
    double dm_grain_demand;                        // assimilate demand for grain (g/m^2)
    double dm_meal_demand;                         // assimilate demand for meal (g/m^2)
    double dm_oil_demand;                          // assimilate demand for oil (g/m^2)
    double dm_oil_conv_demand;                     // assimilate demand for conversion to oil (g/m^2)
    double dm_pod_demand;                          // assimilate demand for pod (g/m^2)

//- Implementation Section ----------------------------------

         // first we zero all plant component deltas
    fill_real_array (dlt_dm_green, 0.0, max_part);

    // calculate demands of reproductive parts
    dm_grain_demand = divide (g_dlt_dm_grain_demand, g_grain_energy, 0.0);

    dm_meal_demand = dm_grain_demand * (1.0 - c_grain_oil_conc);
    dm_oil_demand = dm_grain_demand - dm_meal_demand;
    dm_oil_conv_demand = g_dlt_dm_grain_demand - dm_grain_demand;

    if (dm_grain_demand > 0.0)
    {
        dm_pod_demand = dm_grain_demand * c_frac_pod;
    }
    else
    {
        dm_pod_demand = g_dlt_dm;
    }
    yield_demand = dm_pod_demand
                 + dm_meal_demand
                 + dm_oil_demand
                 + dm_oil_conv_demand;

         // now distribute the assimilate to plant parts
    if (yield_demand >= g_dlt_dm)
            // reproductive demand exceeds supply - distribute assimilate to those parts only
    {
            // reproductive demand exceeds supply - distribute assimilate to those parts only
        dlt_dm_green[meal] = g_dlt_dm * divide (dm_meal_demand, yield_demand, 0.0);
        dlt_dm_green[oil]  = g_dlt_dm * divide (dm_oil_demand, yield_demand, 0.0);
        *dlt_dm_oil_conv   = g_dlt_dm * divide (dm_oil_conv_demand, yield_demand, 0.0);
        dlt_dm_green[pod] = g_dlt_dm
                          - dlt_dm_green[meal]
                          - dlt_dm_green[oil]
                          - (*dlt_dm_oil_conv);
    }
    else
    {
        // more assimilate than needed for reproductive parts
        // distribute to all parts

        // satisfy reproductive demands
        dlt_dm_green[meal]   = dm_meal_demand;
        dlt_dm_green[oil]    = dm_oil_demand;
        *dlt_dm_oil_conv     = dm_oil_conv_demand;
        dlt_dm_green[pod]    = dm_pod_demand;

//        // distribute remainder to vegetative parts
//        dm_remaining = g_dlt_dm - yield_demand;
    }

    // do mass balance check - roots are not included
    dlt_dm_green_tot = sum_real_array (dlt_dm_green, max_part)
                     + *dlt_dm_oil_conv;

    if (!reals_are_equal(dlt_dm_green_tot, g_dlt_dm, 1.0E-4))  // XX this is probably too much slop - try doubles XX
    {
         string msg = "Fruit dlt_dm_green_tot mass balance is off: "
                    + ftoa(dlt_dm_green_tot, ".6")
                    + " vs "
                    + ftoa(g_dlt_dm, ".6");
         parentPlant->warningError(msg.c_str());
    }

      // check that deltas are in legal range
    bound_check_real_array (parentPlant, dlt_dm_green, max_part, 0.0, g_dlt_dm, "Fruit dlt_dm_green");

}



#if TEST_PlantFruit							// build unit test?


// PlantFruit class test harness

// Tests default constructor, copy constructor, assignment operator and
// each of the get and set functions.  Does not test the destructor.

// Modification log
// 6 Aug 97  J. Hargreaves    Initial implementation


#ifndef PlantFruit_H
#include "PlantFruit.h"
#endif

int main()
{
	cout << "PlantFruit test started" << endl;

	PlantFruit p, *aPtr = &p;

//	cout << endl << "Test set and get functions:" << endl;
//	p.setValue(10.0, 2.0, 20.0, 3.0, 30.0, 4.0);
//	if (p.total() != 69.0)
//		cout << "setValue(10.0, 2.0, 20.0, 3.0, 30.0, 4.0) / Total() test FAILED"
//		<< endl;
//
//	cout << endl << "Test default constructor:" << endl;
//	PlantFruit q;                           						// run default constructor
//	if (q.total() != 0.0)
//		cout << "default constructor test FAILED" << endl;
//
//	cout << endl << "Test constructor:" << endl;
//	PlantFruit a(1.0, 2.0, 3.0, 4.0, 5.0, 6.0);                           						// run default constructor
//	if (a.total() != 21.0)
//		cout << "constructor test FAILED" << endl;
//
//	cout << endl << "Test copy constructor:" << endl;
//	PlantFruit s = p;                       // run copy constructor
//	if (s.total() != p.total())
//      cout << "copy constructor test FAILED" << endl;
//
//	cout << endl << "Test assignment operator:" << endl;
//	s.setValue(50.0, 5.0, 60.0, 6.0, 70.0, 7.0); // change object
//
//	if (s.total() != p.total())
//	{
//		s = p;                          // run operator=
//		if (s.total() != p.total())
//			cout << "assignment operator test FAILED" << endl;
//	}
//	else
//		cout << "assignment operator test FAILED DIFFERENCE TEST" << endl;
//
//	cout << endl << "Test multiply operator:" << endl;
//	s.setValue(50.0, 5.0, 60.0, 6.0, 70.0, 7.0); // change object
//	PlantFruit k = p * s;
//	if (k.total() != 3856.0)
//		cout << "multiply operator test FAILED" << endl;
//
//	cout << endl << "Test simple multiply operator:" << endl;
//	s.setValue(50.0, 5.0, 60.0, 6.0, 70.0, 7.0); // change object
//	 k = s * 2.0;
//	if (k.total() != 396.0)
//		cout << "simple multiply operator test FAILED" << endl;
//
//	cout << endl << "Test divide operator:" << endl;
//	s.setValue(50.0, 5.0, 60.0, 6.0, 70.0, 7.0); // change object
//	 k = s/p;
//	if (k.total() < 16.58332 || k.total() > 16.58334)
//		cout << "divide operator test FAILED" << endl;
//
//	cout << endl << "Test simple divide operator:" << endl;
//	s.setValue(50.0, 5.0, 60.0, 6.0, 70.0, 7.0); // change object
//	 k = s / 2.0;
//	if (k.total() != 99.0)
//		cout << "simple divide operator test FAILED" << endl;
//
//	PlantFruit t;
//	t.setValue(50.0, 5.0, 60.0, 6.0, 70.0, 7.0); // change object
//	cout << endl << "Display PlantFruit t" << endl;
//	t.display();
//
//	PlantFruit x;
//	x.setValue(50.0, 5.0, 60.0, 6.0, 70.0, 7.0); // change object
//
//	cout << endl << "Display PlantFruit x - static binding" << endl;
//	x.display();
//
//	cout << endl << "Display PlantFruit x - dynamic binding" << endl;
//	PlantFruit *PlantFruitPtr = &x;
//	PlantFruitPtr->display();

	cout << endl << "PlantFruit test finished" << endl;
	return 0;
}

#endif

