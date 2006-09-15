#include <stdio.h>
#include <math.h>
#include <vector>
#include <string>
#include "PlantLibrary.h"
#include "PlantInterface.h"

//---------------------------------------------------------------------------
//===========================================================================
float crop_failure_germination(plantInterface *iface,
                              int days_germ_limit,    // (INPUT)  maximum days allowed after sowing for germination to take place (days) 
                              int daysInStage,        // (INPUT) number of days we have spent in this stage
                              float plants)           // (INPUT)  Plant density (plants/m^2)

//===========================================================================
/*  Purpose
*      Crop failure from lack of germination within a specific maximum number of days.
*
*  Mission Statement
*   Determine crop failure due to failed germination
*
*  Changes
*/
   {
   if (daysInStage >= days_germ_limit)
      {
      char  output[1024];
      sprintf(output, 
              " crop failure because of lack of\n         germination within %d days of sowing",
              days_germ_limit);
      iface->writeString (output);
      return -1.0 * plants;
      }
   return 0.0;
   }


//=============================================================================
float crop_failure_emergence(plantInterface *iface, 
                            float tt_emerg_limit,    // (INPUT)  maximum degree days allowed for emergence to take place (deg day) 
                            float ttInStage,         // (INPUT)  the sum of growing degree days for this phenological stage (oC d)
                            float plants)            // (INPUT)  Plant density (plants/m^2)                                        

//=============================================================================

/*  Purpose
*      Crop failure from lack of emergence within a specific maximum
*      thermal time sum from germination.
*
*  Mission Statement
*   Determine crop failure due to failed emergence
*
*/
   {
   //  Local Variables
   if (ttInStage > tt_emerg_limit)
      {
      iface->writeString (" failed emergence due to deep planting");
      return -1.0 * plants;
      }
   return 0.0;
   }

//+  Mission Statement
//     Determine plant population death from leaf area senescing
float crop_failure_leaf_sen (plantInterface *iface
                             ,float g_lai              // (INPUT)  live plant green lai
                             ,float g_plants)           // (INPUT)  Plant density (plants/m^2)
   {
   float leaf_area = divide (g_lai, g_plants, 0.0); // leaf area per plant

   if (reals_are_equal (leaf_area, 0.0, 1.0e-6))
        {
        iface->writeString ("Crop failure because of total leaf senescence.");
        return -1.0 * g_plants;
        }
   return 0.0;
   }

//+  Purpose
//      Determine plant death from prolonged phenology delay.

//+  Mission Statement
//     Determine plant death from prolonged phenology delay

//+  Changes
//       290994 jngh specified and programmed
//       110695 psc  added plant death from high soil temp
//       100795 jngh moved plant_kill crop to end of routine
float crop_failure_phen_delay (plantInterface *iface
                              ,float c_swdf_pheno_limit          // (INPUT)  critical cumulative phenology water stress above which the crop fails (unitless)
                              ,float cswd_pheno                 // (INPUT)  cumulative water stress type 3
                              ,float g_plants)                    // (INPUT)  Plant density (plants/m^2)
  {
  if (cswd_pheno >= c_swdf_pheno_limit)
        {
        iface->writeString ("Crop failure because of prolonged");
        iface->writeString ("phenology delay through water stress.");
        return -1.0 * g_plants;
        }
  return 0.0;
  }


//===========================================================================
void crop_death_drought (plantInterface *iface, 
                         int  emerg,                // (INPUT) emergence stage                                                                                
                         int  flag_leaf,            // (INPUT) flag leaf stage                                                                                
                         int  plant_end,            // (INPUT) maximum plant stage                                                                            
                         float *g_cswd_photo,       // (INPUT) cumulative water stress photosynthesis                                                         
                         float *g_leaf_no,          // (INPUT) leaf no in different phases                                                                    
                         float  c_leaf_no_crit,     // (INPUT) critical leaf no below which drought death may occur                                           
                         float  c_swdf_photo_limit, // (INPUT) critical cumulative photosynthesis water stress, above which the crop partly fails (unitless)  
                         float  g_swdef_photo,      // (INPUT) daily water stress for photosynthesis                                                          
                         float  c_swdf_photo_rate,  // (INPUT) rate of plant reduction with photosynthesis water stress,above which the crop fails (unitless) 
                         float  g_plants,           // (INPUT) plant density (plants/m2)                                                                      
                         float *dlt_plants)         // (OUTPUT)daily plant death (plants/m2)                                                                  
//==========================================================================
/*  Purpose
*      Determine percentage plant failure due to water stress
*
*  Mission statement
*       Determine plant death from drought
*
*  Changes
*       21/5/2003 ad converted to BC++
*       290994 jngh specified and programmed
*       240801 ew   generalised and put in the library
*/

   {
   //  Local Variables
   float cswd_photo;             // cumulative water stress for photoperiod
   float leaf_no;                // number of leaves
   float killfr;                 // fraction of crop population to kill
   char output[80];

   // Implementation Section ----------------------------------
   cswd_photo = sum_between (emerg-1, flag_leaf-1, g_cswd_photo);
   leaf_no = sum_between (emerg-1, plant_end-1, g_leaf_no);

   if ((leaf_no < c_leaf_no_crit) && 
       (cswd_photo > c_swdf_photo_limit) &&
       (g_swdef_photo < 1.0))
      {
      killfr = c_swdf_photo_rate * (cswd_photo - c_swdf_photo_limit);
      killfr = bound (killfr, 0.0, 1.0);
      *dlt_plants = -1 * g_plants * killfr;

      sprintf(output, "plant_kill. %d % failure because of water stress.",
              int((killfr * 100.0) + 0.5));

      iface->writeString (output);
      }
   else
      {
      *dlt_plants = 0.0;
      }
   }

//==========================================================================
void crop_death_seedling_hightemp (plantInterface *iface, 
                                   int days_after_emerg,           // (INPUT) days after emergence                   
                                   int g_year,                     // (INPUT) year                                   
                                   int g_day_of_year,              // (INPUT) day of year                            
                                   float *g_soil_temp,              // (INPUT) soil surface temperature (C)           
                                   float *c_x_weighted_temp,        // (INPUT) soil temperature (C) in lookup table   
                                   float *c_y_plant_death,          // (INPUT) fraction of plants killed              
                                   int   c_num_weighted_temp,        // (INPUT) no of table elements                   
                                   float g_plants,                 // (INPUT) plant density (plants/m2)              
                                   float *dlt_plants)               // (OUPUT) daily plant death (plants/m2)          
//=========================================================================

/*  Purpose
*      Determine plant seedling death.
*
*  Mission Statement
*    Determine plant seeding death
*
*  Changes
*       21/5/2003 ad converted to BC++
*       290994 jngh specified and programmed
*       240801 ew   generalised and put in the library
*
*  Sub-Program Arguments
*      int *days_after_emerg           !
*      int *g_year                     !
*      int *g_day_of_year              !
*      float *g_soil_temp(*)           !
*      float *c_x_weighted_temp(*)     !
*      float *c_y_plant_death(*)       !
*      int *c_num_weighted_temp        !
*      float *g_plants                 !
*      float *dlt_plants               !
*/
   {
   //  Local Variables
   float killfr;                 //fraction of crop population to kill
   char output[80];                // output string

   // Implementation Section ----------------------------------

   //cpsc  add code to kill plants for high soil surface temperatures
   if (days_after_emerg == 1)
      {
      soil_temp_weighted_3days (g_year, g_day_of_year, g_soil_temp,
                                c_x_weighted_temp, c_y_plant_death, c_num_weighted_temp, &killfr);
      *dlt_plants = -1 * g_plants * killfr;

       if (killfr > 0.0)
         {
         sprintf(output,
            "plant_kill. %d % failure because of high soil surface temperatures.",
            int((killfr * 100.0) + 0.5));

         iface->writeString (output);
         }
       else
         {
         //do nothing
         }
      }
   else
      {
      *dlt_plants = 0.0;
      }
      return;
   }

//==========================================================================
void soil_temp_weighted_3days (int  g_year,                  //   (INPUT) year                                   
                               int  g_day_of_year,           //   (INPUT) day of year                            
                               float *g_soil_temp,            //   (INPUT) soil surface temperature (C)           
                               float *c_x_weighted_temp,     //   (INPUT) soil temperature (C) in lookup table   
                               float *c_y_plant_death,       //   (INPUT) fraction of plants killed              
                               int   c_num_weighted_temp,    //   (INPUT) no of table elements                   
                               float *killfr)                //    (OUTPUT) fraction of plants killed            
//==========================================================================
/*  Purpose
*        Calculate fraction of plants killed by high temperature during
*        emergence (0-1).
*
*  Mission Statement
*     Calculate fraction of plants killed by high temperature during emergence
*
*  Changes
*       21/5/2003 ad converted to BC++
*     230695 jngh specified and programmed
*       240801 ew   generalised and put in the library
*
*/
   {
   //  Local Variables
   int day_before;               // day of year number of day before
                                 // yesterday ()
   float weighted_temp;          // 3 day weighted soil temperature (oC)
   int yesterday;                // day of year number of yesterday

   // Implementation Section ----------------------------------
   yesterday = offset_day_of_year (g_year, g_day_of_year, -1);
   day_before = offset_day_of_year (g_year, g_day_of_year, -2);

   weighted_temp = 0.25 * g_soil_temp[day_before] +
         0.50 * g_soil_temp[yesterday] +
         0.25 * g_soil_temp[g_day_of_year];

   *killfr = linear_interp_real (weighted_temp, c_x_weighted_temp,
                                  c_y_plant_death, c_num_weighted_temp);

   }

//===========================================================================
void crop_death_actual (float g_dlt_plants_failure_germ,
                        float g_dlt_plants_failure_emergence, 
                        float g_dlt_plants_failure_leaf_sen,
                        float g_dlt_plants_failure_phen_delay, 
                        float g_dlt_plants_death_seedling,
                        float g_dlt_plants_death_drought, 
                        float g_dlt_plants_death_barrenness,
                        float *dlt_plants)
//==========================================================================
/*  Purpose
*      Determine actual plant death.
*
*  Mission Statement
*     Determine actual plant death
*
* Changes
*       21/5/2003 ad converted to BC++
*       290994 jngh specified and programmed
*
*  Constant Values
*      character  my_name*(*)           ! name of procedure
*      parameter (my_name = 'crop_death_actual')
*
*/
   {
   *dlt_plants = g_dlt_plants_failure_germ;
   *dlt_plants = min (*dlt_plants, g_dlt_plants_failure_emergence);
   *dlt_plants = min (*dlt_plants, g_dlt_plants_failure_leaf_sen);
   *dlt_plants = min (*dlt_plants, g_dlt_plants_failure_phen_delay);
   *dlt_plants = min (*dlt_plants, g_dlt_plants_death_seedling);
   *dlt_plants = min (*dlt_plants, g_dlt_plants_death_drought);
   *dlt_plants = min (*dlt_plants, g_dlt_plants_death_barrenness);
   }
