#include <general/pch.h>
#include <vcl.h>
#include <stdio.h>
#include <math.h>
#include <vector>
#include <string>
#include "Plantlibrary.h"


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

//         write (error_message,'(a, i10)')
//     :               'Invalid lookup table - number of values ='
//     :              , numvals
         warning_error (&err_user, "Bad stage lookup table");
         }

   return x_stage_code;
   }

//+  Purpose
//       Return an interpolated stage code from a table of stage_codes
//       and a nominated stage number. Returns 0 if the stage number is not
//       found. Interpolation is done on thermal time.

//+  Mission Statement
//     Get the stage code from a table of stage codes

//+  Changes
//       080994 jngh specified and programmed
float plant_stage_code (float  *c_stage_code_list  // (INPUT)  list of stage numbers
                              ,float  *g_phase_tt         // (INPUT)  Cumulative growing degree days required for each stage (deg days)
                              ,float  *g_tt_tot           // (INPUT)  the sum of growing degree days for a phenological stage (oC d)
                              ,float  stage_no            // (INPUT) stage number to convert
                              ,float  *stage_table        // (INPUT) table of stage codes
                              ,int    numvals             // (INPUT) size_of of table
                              ,int    max_stage) {        // IN
     //+  Local Variables
     float phase_tt;                                   // required thermal time between stages (oC)
     float fraction_of;                                //
     int   i;                                          // array index - counter
     int   next_stage;                                 // next stage number to use
     float tt_tot;                                     // elapsed thermal time between stages (oC)
     int   this_stage;                                 // this stage to use
     float x_stage_code = 0.0;                         // interpolated stage code

     //- Implementation Section ----------------------------------

     if (numvals>=2)
         {
         // we have a valid table
         this_stage = stage_no_of (stage_table[0] , c_stage_code_list, max_stage);

         for (i = 1; i < numvals; i++)
           {
           next_stage = stage_no_of (stage_table[i], c_stage_code_list, max_stage);
           if (stage_is_between (this_stage, next_stage, stage_no))
              {
              // we have found its place
              tt_tot = sum_between (this_stage-1, next_stage-1, g_tt_tot);
              phase_tt = sum_between (this_stage-1, next_stage-1, g_phase_tt);
              fraction_of = divide (tt_tot, phase_tt, 0.0);
              fraction_of = bound(fraction_of, 0.0, 0.999);
              x_stage_code = stage_table[i-1] + (stage_table[i] - stage_table[i-1])
                                  * fraction_of;
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

        char msg[80];
        sprintf(msg, "invalid stage code lookup table - number of values = %d", numvals);
        warning_error (&err_user, msg);
        }
    return (x_stage_code);
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
      sprintf(error_mess, "Invalid lookup table - number of values = %d", numvals);
      warning_error (&err_user, error_mess);
      x_stage_code = 0.0;
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
void crop_devel(int   max_stage,            //(INPUT)
                 float g_phase_devel,        //(INPUT)  development of current phase (
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
      {
      *g_current_stage = floor((*g_current_stage) + 1.0);
      if (int(*g_current_stage) == max_stage)
         {
         *g_current_stage = 1.0;   //Eh? shouldn't this throw an error??
         }
      }
   else
      {
      *g_current_stage = new_stage;
      }
   }



//===========================================================================
float crop_germination(int   sowing_stage,             //(INPUT)
                       int   germ_stage,               //(INPUT)
                       float C_pesw_germ,              //(INPUT)  plant extractable soil water i
                       float G_current_stage,          //(INPUT)  current phenological stage
                       float *G_days_tot,              //(INPUT)  duration of each phase (days)
                       float *G_dlayer,                //(INPUT)  thickness of soil layer I (mm)
                       int   max_layer,                //(INPUT)
                       float G_sowing_depth,           //(INPUT)  sowing depth (mm)
                       float *G_sw_dep,                //(INPUT)  soil water content of layer L
                       float *P_ll_dep)                //(INPUT)  lower limit of plant-extractab

//===========================================================================

/*  Purpose
*      Determine germination based on soil water availability
*
*  Mission statement
*   Determine germination based upon soil moisture status.
*
*  Changes
*     010994 jngh specified and programmed
*     19/5/2003 ad converted to BC++
*/

   {
   //  Local Variables
   int layer_no_seed;            // seedling layer number
   float pesw_seed;              // plant extractable soil water in
                                 //seedling layer available for
                                 // germination ( mm/mm)
   float dividend;
   float divisor;

         // determine if soil water content is sufficient to allow germination.
         // Soil water content of the seeded layer must be > the
         // lower limit to be adequate for germination.

   if (stage_is_between (sowing_stage, germ_stage, G_current_stage))
      {
      layer_no_seed = find_layer_no (G_sowing_depth, G_dlayer, max_layer);
      pesw_seed = divide (G_sw_dep[layer_no_seed] - P_ll_dep[layer_no_seed],
                          G_dlayer[layer_no_seed], 0.0);

      // can't germinate on same day as sowing, because miss out on
      // day of sowing else_where
      if ((pesw_seed > C_pesw_germ) &&
          !on_day_of (sowing_stage, G_current_stage))
         {
         // we have germination
         // set the current stage so it is on the point of germination
         return (1.0 + fmod(G_current_stage, 1.0));        //XX why not "return germ_stage"??
         }
      else
         {
         // no germination yet but indicate that we are on the way.
         return 0.999;
         }
      }
   else
      {
      //no sowing yet
      }
   return 0.0;
   }


//===========================================================================
void crop_phase_devel(int   sowing_stage,              //(INPUT)
                      int   germ_stage,                //(INPUT)
                      int   end_development_stage,     //(INPUT)
                      float C_pesw_germ,               //(INPUT)  plant extractable soil water i
                      float *C_fasw_emerg,             // (INPUT)
                      float *C_rel_emerg_rate,         // (INPUT)
                      int   C_num_fasw_emerg,          //(INPUT)
                      float G_current_stage,           //(INPUT)  current phenological stage
                      float *G_days_tot,               // (INPUT)  duration of each phase (days)
                      float *G_dlayer,                 // (INPUT)  thickness of soil layer I (mm)
                      int   max_layer,                 //(INPUT)
                      float G_sowing_depth,            //(INPUT)  sowing depth (mm)
                      float *G_sw_dep,                 // (INPUT)  soil water content of layer L
                      float *G_dul_dep,                // (INPUT)
                      float *P_ll_dep,                 // (INPUT)  lower limit of plant-extractab
                      float G_dlt_tt,                  // (INPUT)  Cumulative growing degree days
                      float *G_phase_tt,               //
                      float *G_tt_tot,                 // (INPUT)  the sum of growing degree days
                      float *phase_devel)              // (OUTPUT) fraction of current phase
                                                       //          elapsed ()
//===========================================================================

/*  Purpose
*     Determine the fraction of current phase elapsed ().
*
*  Mission statement
*   Determine the progress through the current growth stage.
*
*  Changes
*     010994 jngh specified and programmed
*     19/5/2003 ad converted to BC++
*/
   {
   //Implementation Section ----------------------------------
   if (stage_is_between (sowing_stage, germ_stage ,G_current_stage))
      {
      *phase_devel = crop_germination(sowing_stage, germ_stage, C_pesw_germ,
                                      G_current_stage, G_days_tot, G_dlayer, max_layer,
                                      G_sowing_depth, G_sw_dep, P_ll_dep);
      }
   else if (stage_is_between (germ_stage, end_development_stage, G_current_stage))
      {
      crop_germ_dlt_tt(C_fasw_emerg, C_rel_emerg_rate, C_num_fasw_emerg, G_current_stage,
                       germ_stage, G_dlayer, max_layer, G_sowing_depth, G_sw_dep, P_ll_dep,
                       G_dul_dep, &G_dlt_tt);
      *phase_devel =  crop_phase_tt(G_dlt_tt, G_phase_tt, G_tt_tot, G_current_stage);
      }
   else
      {
      *phase_devel = fmod(G_current_stage, 1.0);
      }
   }

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


//+  Purpose
//     Calculate min and max crown temperatures.

//+  Changes
//       280394 nih - programmed and specified
//       030399 ew  - regprogrammed
void crop_crown_temp_nwheat
   ( float tempmx        //Daily maximum temperature of the air (C)
    ,float tempmn        //Daily minimum temperature of the air (C)
    ,float snow          //Snow depth on the current day (mm)
    ,float *tempcx        //Daily maximum of crown temperature (C)     - OUTPUT
    ,float *tempcn        //Daily minimum of crown temperature (C)     - OUTPUT
    ) {
//- Implementation Section ----------------------------------


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

    //+  Purpose
//     Use temperature, photoperiod and genetic characteristics
//     to determine when the crop begins a new growth phase.
//     The initial daily thermal time and height are also set.

//+  Mission Statement
//   Calculate crop phenological development using thermal time targets.
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
//+  Purpose
//     <insert here>

//+  Mission Statement
//     Photoperiod factor

//+  Changes
//     <insert here>
void wheat_photoperiod_effect(
     float current_stage
    ,int   start_stage
    ,int   end_stage
    ,float photoperiod
    ,float p_photop_sen
    ,float *photop_eff
    ) {
//+  Local Variables
    float photop_sen_factor;

//- Implementation Section ----------------------------------
    if (stage_is_between(start_stage,end_stage,current_stage))
        {

        photop_sen_factor = p_photop_sen * 0.002;

        *photop_eff = 1. - photop_sen_factor * pow(20. - photoperiod, 2);
        *photop_eff = bound (*photop_eff, 0.0, 1.0);

        }
    else
        {
        *photop_eff = 1.0;
        }

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

//+  Changes
//     14/07/98 nih taken from Nwheat
void wheat_vernaliz_days_nwheat
   ( float g_current_stage     //The current development stage
    ,int   start_stage         //Stage vernalisation begins
    ,int   end_stage           //Stage vernalisation ends
    ,float g_maxt              //Daily maximum Temperature
    ,float g_mint              //Daily minimum temperature
    ,float g_snow              //Snow depth of the day (mm)
    ,float *g_dlt_cumvd         //vernalisation day today                      OUTPUT
    ,float g_cumvd             //cumulative vernalisation days till yesterday
   )  {
//+  Local Variables
    float tempcn;
    float tempcx;
    float tempcr;
    float vd,vd1,vd2;

//- Implementation Section ----------------------------------
    if (stage_is_between(start_stage,end_stage
        ,g_current_stage))
        {
       // The cumulative vernalization has not reached the required level of vernalization

       crop_crown_temp_nwheat (g_maxt,g_mint,g_snow,&tempcx,&tempcn);

       tempcr = (tempcn+tempcx)/2.0;

       if (g_mint < 15.0 && g_maxt > 0.0)
           {
           vd1 = 1.4 - 0.0778 * tempcr;
           vd2 = 0.5 + 13.44 / pow(g_maxt-g_mint + 3., 2) * tempcr;
           vd = min (vd1, vd2);
           vd = l_bound (vd, 0.0);
           *g_dlt_cumvd = vd;
           }
       else
           {
           // too cold or too warm - no vernalization
           }
       if (g_maxt > 30. && g_cumvd+(*g_dlt_cumvd) < 10.)
           {
           // high temperature will reduce vernalization
           *g_dlt_cumvd = - 0.5*(g_maxt - 30.);
           *g_dlt_cumvd = - min(-(*g_dlt_cumvd), g_cumvd);
           }
       else
           {
           }
       }
   else
       {
       *g_dlt_cumvd = 0.0;
       }
   }

//+  Purpose
//     <insert here>

//+  Mission Statement
//     Vernalisation factor

//+  Changes
//     <insert here>
void wheat_vernaliz_effect_nwheat
   (
     float current_stage
    ,int   start_stage
    ,int   end_stage
    ,float p_vern_sens
    ,float cumvd
    ,float dlt_cumvd
    ,float reqvd
    ,float *vern_effect        //OUTPUT
    ) {
//+  Local Variables
    float vfac;                                   // vernalization factor
    float vern_sens_fac;

//- Implementation Section ----------------------------------
    if (stage_is_between(start_stage,end_stage,current_stage))
        {

        if (reqvd < 0.0) { reqvd = 50.0; }
        vern_sens_fac =  p_vern_sens* 0.0054545 + 0.0003;
        vfac = 1. - vern_sens_fac * (reqvd - (cumvd+dlt_cumvd));
        *vern_effect = bound (vfac, 0.0, 1.0);
        }
    else
        {
        *vern_effect = 1.0;
        }
    }


