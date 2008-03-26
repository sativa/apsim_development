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
#include "Plant.h"


//  Purpose
//      Initialise plant weights and plant weight minimums
//      at required instances.
//
//  Mission Statement
//    Initialise plant weights and plant weight minimums at required instances.
void Plant::plant_fruit_dm_init(
                    float c_dm_leaf_init            // (INPUT)  leaf growth before emergence (
                   ,float c_dm_root_init            // (INPUT)  root growth before emergence (
                   ,float c_pod_trans_frac          // (INPUT)  fraction of pod used in trans
                   ,int   max_part                  // (INPUT)
                   ,int   max_fruit_cohorts         // (INPUT)
                   ,float*g_current_fruit_stage     // (INPUT)  current cohort phenological stage
                   ,int   g_num_fruit_cohorts       // (INPUT)
                   ,float*g_fruit_no                // (INPUT)  Fruit density (Fruit/m^2)
                   ,float g_plants                  // (INPUT/OUTPUT) plant part weights (g/m^2)
                   ,float**g_fruit_sdr_daily         //
                   ,float**dm_fruit_green           // (INPUT)  Plant density (plants/m^2)
                   ,float*dm_fruit_pod_min)         // (OUTPUT) minimum weight of each plant part (g/plant)
{
#if 0
   const char *my_name = "plant_fruit_dm_init";

   //  Local Variables
   float      dm_fruit_pod;          // dry matter in pods (g/pod)

   //*- Implementation Section ----------------------------------

   push_routine (my_name);

   // initialise plant weight
   // initialisations - set up dry matter for pod etc
   if (phenology->on_day_of("emergence"))
      {
      // seedling has just emerged.
      // initialise fruit.
    	for (int cohort = 0; cohort < max_fruit_cohorts; cohort++)
    	   {
         dm_fruit_pod_min[cohort] = 0.0;
         for (int part=0; part< max_part; part++)
            {
            dm_fruit_green[cohort][part] = 0.0;
            }
         for (int day=0; day< 366; day++)
            {
            g_fruit_sdr_daily[cohort][day] = 0.0;
            }
         }
      dm_fruit_green[0][root] = c_dm_root_init * g_plants;
      dm_fruit_green[0][leaf] = c_dm_leaf_init * g_plants;
      }
   else if (phenology->inPhase("grainfill"))
      {
      for (int cohort = 0; cohort < g_num_fruit_cohorts; cohort++)
         {
         if (0/*on_day_of (start_grain_fill, g_current_fruit_stage[cohort])*/)
             {
             // we are at first day of grainfill for this cohort.
             dm_fruit_pod = divide (dm_fruit_green[cohort][pod], g_fruit_no[cohort], 0.0);
             dm_fruit_pod_min[cohort] = dm_fruit_pod * (1.0 - c_pod_trans_frac);
            }
         }
      }
   pop_routine (my_name);
#endif
}

//  Purpose
//    Use temperature, photoperiod and genetic characteristics
//    to determine when the crop begins a new growth phase.
//    The initial daily thermal time and height are also set.
//
//  Mission Statement
//  Calculate crop phenological development using thermal time targets.
void Plant::plant_fruit_phenology_update (float g_previous_stage
                                         ,float *g_current_stage             // output
                                         ,float *g_current_fruit_stage
                                         ,int   initial_stage
                                         ,int   end_development_stage
                                         ,int   start_grain_fill
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

   if (stage_is_between (initial_stage, end_development_stage, *g_current_stage))
      {
      fruit_no_crit = 0.0 ;
      if (stage_is_between (start_grain_fill, end_development_stage, *g_current_stage))
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

void Plant::plant_fruit_abort (int option)
    {
    const char*  my_name = "plant_fruit_abort" ;

    push_routine (my_name);

    if (option == 1)
        {
        // not active
        }
    else if (option == 2)
        {
        plant_fruit_no_abort(0//initial_fruit_stage
                           , 0//start_grain_fill
                           , g.current_fruit_stage
                           , max_fruit_stage
                           , max_part
                           , max_fruit_cohorts
                           , c.days_assimilate_ave
                           , g.day_of_year
                           , g.year
                           , g.fruit_no
                           , g.num_fruit_cohorts
                           , p.x_stage_sdr_min
                           , p.y_sdr_min
                           , p.num_sdr_min
                           , p.dm_fruit_max
                           , c.fract_dm_fruit_abort_crit
                           , g.fruit_days_tot
                           , g.dlt_dm_fruit_demand
                           , g.dm_fruit_green
                           , g.dlt_dm_fruit_green
                           , g.dlt_dm_fruit_green_retrans
                           , g.fruit_sdr_daily
                           , g.fruit_sdr
                           , g.dlt_fruit_no_abort);

        plant_fruit_dm_abort(max_part
                           , c.dm_abort_fract
                           , max_fruit_cohorts
                           , g.num_fruit_cohorts
                           , g.fruit_no
                           , g.dm_fruit_green
                           , g.dlt_dm_fruit_green
                           , g.dlt_dm_fruit_green_retrans
                           , g.dlt_fruit_no_abort
                           , g.dlt_dm_fruit_abort);
        for (int part = 0; part < max_part; part++)
           {
           g.dlt_dm_green_retrans[part] = 0.0;
           g.dlt_dm_green_abort[part] = 0.0;
           g.dlt_dm_senesced[part] = g.dlt_dm_senesced[part]; // explicit..
           for (int cohort=0; cohort<max_fruit_cohorts; cohort++)
              {
              g.dlt_dm_green_retrans[part] += g.dlt_dm_fruit_green_retrans[cohort][part];
              g.dlt_dm_green_abort[part] += g.dlt_dm_fruit_abort[cohort][part];
              g.dlt_dm_senesced[part] += g.dlt_dm_green_abort[part];
              }
           }
         plant_fruit_n_retranslocate( max_part
                                  , c.dm_abort_fract
                                  , max_fruit_cohorts
                                  , g.num_fruit_cohorts
                                  , g.dm_fruit_green
                                  , g.n_green
                                  , g.dlt_dm_fruit_abort
                                  , g.dlt_n_retrans );
        }
    else
        {
        throw std::invalid_argument ("invalid template option");
        }

    pop_routine (my_name);
    }


void Plant::plant_fruit_cleanup (int option)
    {
    const char*  my_name = "plant_fruit_cleanup" ;

    push_routine (my_name);

    if (option == 1)
        {
        // not active
        }
    else if (option == 2)
        {
        plant_fruit_update(g.plants
                         , g.dlt_plants
                         , g.dlt_fruit_flower_no
                         , g.dlt_fruit_site_no
                         , g.dlt_fruit_no_abort
                         , g.dlt_dm_fruit_senesced
                         , g.dlt_dm_fruit_green
                         , g.dlt_dm_fruit_green_retrans
                         , g.dlt_dm_fruit_abort
                         , g.dlt_dm_parasite
                         , g.dm_fruit_green
                         , g.dm_fruit_dead
                         , g.dm_fruit_senesced
                         , g.num_fruit_cohorts
                         , g.fruit_no
                         , &g.fruit_site_no
                         , g.dlt_dm
                         , g.fruit_sdr
                         , g.day_of_year
                         , g.year
                         , g.dlt_dm_daily
                         , g.fruit_sdr_daily
                         , &g.setting_fruit);
        }
    else
        {
        throw std::invalid_argument ("invalid template option");
        }

    pop_routine (my_name);
    }


void Plant::plant_fruit_cohort_init( bool  do_init
                                    ,float *g_days_tot
                                    ,float *g_current_fruit_stage // stage of each fruit cohort
                                    ,int   *g_num_fruit_cohorts)  // current count of fruit cohorts
    {
    const char*  my_name = "plant_fruit_cohort_init" ;
    float init_stage_cohort = -1.0;  //XX
    push_routine (my_name);

    if (do_init)
       {
       *g_num_fruit_cohorts = 1;
       g_current_fruit_stage[*g_num_fruit_cohorts-1] = (float) init_stage_cohort;
       }
    else if (*g_num_fruit_cohorts > 0)
       {
       *g_num_fruit_cohorts = *g_num_fruit_cohorts + 1;
       if (*g_num_fruit_cohorts <= max_fruit_cohorts)
           {
           g_current_fruit_stage[*g_num_fruit_cohorts-1] = (float) init_stage_cohort;
           }
       else
           {
           throw std::runtime_error ("number of fruit cohorts exceeded maximum allowed");
           }
       }
    pop_routine (my_name);
    }

//+  Purpose
//       Get change in plant fruit number

//+  Mission Statement
//       Get change in plant fruit number
void Plant::crop_fruit_site_number(float g_current_stage
                             ,int   initial_fruit_stage
                             , int final_fruit_stage
                             , float **g_fruit_tt_tot
                             , float **g_fruit_phase_tt
                             , float p_cutout_fract
                             ,float g_plants
                             , int max_fruit_cohorts
                             , int max_stage
                             ,float *p_x_node_no_fruit_sites
                             ,float *p_y_fruit_sites_per_node
                             ,int   p_num_node_no_fruit_sites
                             ,float *g_node_no_first_flower      //IN & OUT
                             ,int   start_node_app               // (INPUT)  stage of start of fruit appeara
                             ,int   end_node_app                 // (INPUT)  stage of end of fruit appearanc
                             ,float g_node_no                   // (INPUT)  number of fully expanded nodes
                             ,float g_maxt
                             ,float g_mint
                             ,float *c_x_temp_fruit_site
                             ,float *c_y_rel_fruit_site
                             ,int   c_num_temp_fruit_site
                             ,float g_dlt_node_no
                             ,float *dlt_fruit_site_no )         // OUT
{
    const char*  my_name = "crop_fruit_site_number" ;

    //+  Local Variables
    float fruit_sites_per_node;
    float dlt_fruit_site_no_pot;
    float node_no_now;
    float metabolic_fact;
    float fruit_tt_target,
          fruit_tt_cum,
          temp_fac,
          tav,
          dlt_node_no;

    //- Implementation Section ----------------------------------
    push_routine (my_name);

    node_no_now =  g_node_no;

    if (phenology->on_day_of ("initial_fruit_stage"))
        {
        *g_node_no_first_flower = node_no_now;
        dlt_node_no = g_dlt_node_no;
        }
    else if (*g_node_no_first_flower > 0.0)
        {
        dlt_node_no = g_dlt_node_no;
        }
    else
        {
        dlt_node_no = 0.0;
        }

    // C3
    //fprintf(stdout, "%d %f\n", g_day_of_year, dlt_dm_average);
    fruit_sites_per_node = linear_interp_real(node_no_now
                                             ,p_x_node_no_fruit_sites
                                             ,p_y_fruit_sites_per_node
                                             ,p_num_node_no_fruit_sites);

    dlt_fruit_site_no_pot = dlt_node_no
                  * g_plants
                  * fruit_sites_per_node;

    fruit_tt_target = sum_between(initial_fruit_stage-1,
                                  final_fruit_stage-1,
                                  g_fruit_phase_tt [0])
                  * p_cutout_fract;
    fruit_tt_cum = sum_between(initial_fruit_stage-1,
                               final_fruit_stage-1,
                               g_fruit_tt_tot[0]);

    metabolic_fact = divide (fruit_tt_cum, fruit_tt_target, 0.0);
    metabolic_fact = bound (metabolic_fact, 0.0, 1.0);

    tav = (g_maxt + g_mint) / 2.0;

    temp_fac = linear_interp_real(tav
                                  ,c_x_temp_fruit_site
                                  ,c_y_rel_fruit_site
                                  ,c_num_temp_fruit_site);

    *dlt_fruit_site_no = dlt_fruit_site_no_pot
                          * (1.0 - metabolic_fact)
                          * temp_fac;
 }

// Purpose
//     Get change in plant flower number
//
// Mission Statement
//     Get change in plant flower number
//
// Changes
//     101003 jngh specified and programmed
void Plant::crop_fruit_flower_number (
      float p_dm_fruit_set_crit
     ,float p_dm_fruit_set_min
     ,float g_dlt_dm
     ,float *g_dlt_dm_daily
     ,int   c_days_assimilate_ave
     ,int   g_day_of_year
     ,int   g_year
     ,float *g_fruit_flower_no
     ,int   max_fruit_cohorts
     ,float g_dlt_fruit_site_no
     ,bool  g_setting_fruit
     ,float *dlt_flower_no  // OUTPUT
      ) {
    //  Local Variables
    float dlt_dm_average;
    float flower_no_potential;
    float dlt_flower_no_potential;
    int start_day;
    int start_year;

    start_day = offset_day_of_year(g_year, g_day_of_year, -1);
    if (g_day_of_year == 1)
         start_year = g_year - 1;
    else
         start_year = g_year;


    dlt_dm_average = crop_running_ave(start_day
                                     , start_year
                                     , g_dlt_dm_daily
                                     , c_days_assimilate_ave);

    if (g_setting_fruit)
       {
       flower_no_potential = divide (dlt_dm_average, p_dm_fruit_set_min, 0.0);
       }
    else
       {
       flower_no_potential = divide (dlt_dm_average, p_dm_fruit_set_crit, 0.0);
       }
    dlt_flower_no_potential = flower_no_potential -
                 sum_real_array(g_fruit_flower_no, max_fruit_cohorts);
    dlt_flower_no_potential = l_bound(dlt_flower_no_potential, 0.0);

    *dlt_flower_no = dlt_flower_no_potential;
    *dlt_flower_no = u_bound (*dlt_flower_no, g_dlt_fruit_site_no);

    }

// Purpose
//     Get change in plant fruit number
//
// Mission Statement
//     Get change in plant fruit number
//
// Changes
//     101003 jngh specified and programmed
void Plant::crop_fruit_number (int flowering
                             , int max_stage
                             , int max_fruit_cohorts
                             , int g_num_fruit_cohorts
                             , float c_tt_flower_to_start_pod
                             , float **g_tt_tot
                             , float *g_flower_no
                             , float *dlt_fruit_no)
    {
    //  Local Variables
    int   cohort = 0;
    do {
       if (g_flower_no[cohort] > 0.0)
            {
            if (g_tt_tot[cohort][flowering-1] >= c_tt_flower_to_start_pod)
                {
                // flowers become fruit
                dlt_fruit_no[cohort] = g_flower_no[cohort];
                }
            }
       cohort++;
       } while (cohort < g_num_fruit_cohorts);
  }
//+  Purpose
//       Get plant fruit number shed

//+  Mission Statement
//       Get plant fruit number shed
void Plant::plant_fruit_no_abort(
     int   initial_stage
    ,int   start_grain_fill
    ,float*g_current_fruit_stage
    ,int   max_fruit_stage
    ,int   max_part
    ,int   max_fruit_cohorts
    ,int   c_days_assimilate_ave
    ,int   g_day_of_year
    ,int   g_year
    ,float*g_fruit_no
    ,int   g_num_fruit_cohorts
    ,float *p_x_stage_sdr_min
    ,float *p_y_sdr_min
    ,int   p_num_sdr_min
    ,float p_dm_fruit_max
    ,float c_fract_dm_fruit_abort_crit
    ,float **g_fruit_days_tot
    ,float *g_dm_fruit_demand
    ,float **g_dm_fruit_green
    ,float **g_dlt_dm_fruit
    ,float **g_dlt_dm_fruit_retrans
    ,float **g_fruit_sdr_daily
    ,float *g_fruit_sdr
    ,float *dlt_fruit_no_abort
    )
{
    const char*  my_name = "plant_fruit_no_abort" ;


    float supply_demand_ratio;
    float survive_fract;
    float dlt_fruit_no_survive;
    int   cohort;
    int   current_phase;
    float dlt_dm_fruit;
    float dm_fruit;
    float dm_fruit_crit;
    float dlt_dm_fruit_retrans;
    float fruit_sdr_average;
    float dm_supply;
    float sdr_days_tot;
    int   days_assimilate_ave;
    int   start_day;
    int   start_year;
    float sdr_min;

    //- Implementation Section ----------------------------------
    push_routine (my_name);

    cohort = 0;
    do {
       if (g_fruit_no[cohort]> 0.0)
          {
          //slight positional difference form john for loop ordering
          dm_fruit_crit = p_dm_fruit_max
                         * c_fract_dm_fruit_abort_crit
                         * g_fruit_no[cohort];

          dm_fruit = 0.0;
          dlt_dm_fruit = 0.0;
          dlt_dm_fruit_retrans = 0.0;
          for (int part=pod; part< max_part;  part++)
             {
             dm_fruit += g_dm_fruit_green[cohort][part];
             dlt_dm_fruit += g_dlt_dm_fruit[cohort][part];
             dlt_dm_fruit_retrans += g_dlt_dm_fruit_retrans[cohort][part];
             }
          // can only abort fruit that is not in grain filling phase
          dm_supply = dlt_dm_fruit + dlt_dm_fruit_retrans;
          supply_demand_ratio = divide(dm_supply
                                     , g_dm_fruit_demand[cohort]
                                     , 1.0);

          g_fruit_sdr[cohort] = supply_demand_ratio;

          current_phase = (int)g_current_fruit_stage[cohort];

          sdr_days_tot = sum_between(initial_stage-1, current_phase-1, g_fruit_days_tot[cohort]);
          days_assimilate_ave = min(c_days_assimilate_ave, sdr_days_tot);

          start_day = offset_day_of_year(g_year, g_day_of_year, -1);

          if (g_day_of_year == 1)
               start_year = g_year - 1;
          else
               start_year = g_year;

          fruit_sdr_average = crop_running_ave(start_day
                                             , start_year
                                             , g_fruit_sdr_daily[cohort]
                                             , days_assimilate_ave-1);

//          fprintf(stdout, "%3d %3d %10.4f\n", g.day_of_year, days_assimilate_ave,
//                  fruit_sdr_average);

          fruit_sdr_average = divide(
                      fruit_sdr_average * (float)(days_assimilate_ave-1)
                      + g_fruit_sdr[cohort]
                     , days_assimilate_ave, 0.0);

          if (dm_fruit <= dm_fruit_crit)
              {
              sdr_min = linear_interp_real (g_current_fruit_stage[cohort]
                                          , p_x_stage_sdr_min
                                          , p_y_sdr_min
                                          , p_num_sdr_min);


              survive_fract = divide(fruit_sdr_average, sdr_min, 0.0);
              survive_fract = u_bound (survive_fract, 1.0);

              dlt_fruit_no_survive = g_fruit_no[cohort] * survive_fract;
              if (dlt_fruit_no_survive > tolerance_fruit_no)
                 dlt_fruit_no_abort[cohort] = g_fruit_no[cohort] - dlt_fruit_no_survive;
              else
                 dlt_fruit_no_abort[cohort] = g_fruit_no[cohort];
              }
          else
              {
              dlt_fruit_no_abort[cohort] = 0.0;
              }
          }
      else
          {
          //no fruit in this cohort
          dlt_fruit_no_abort[cohort] = 0.0;
          }
      cohort++;
      } while (cohort < g_num_fruit_cohorts);
   pop_routine (my_name);
   }


//+  Purpose
//       Get plant fruit number shed

//+  Mission Statement
//       Get plant fruit number shed
void Plant::plant_fruit_dm_abort(int   max_part
                               ,float c_dm_abort_fract
                               ,int   max_fruit_cohorts
                               ,int   g_num_fruit_cohorts
                               ,float *g_fruit_no
                               ,float **g_dm_fruit_green
                               ,float **g_dlt_dm_fruit
                               ,float **g_dm_fruit_retranslocate
                               ,float *g_dlt_fruit_no_abort
                               ,float **dlt_dm_fruit_abort)
    {
    const char*  my_name = "plant_fruit_dm_abort" ;

    //+  Local Variables
    float abort_fract;
    int   cohort;
    int   part;
    float dm_fruit_abort_retrans;
    float dm_tot_fruit;
    float dm_tot_fruit_abort;

    //- Implementation Section ----------------------------------
    push_routine (my_name);

    for (cohort=0; cohort < max_fruit_cohorts; cohort++)
       for (part = pod; part < max_part; part++)
           dlt_dm_fruit_abort[cohort][part] = 0.0;

    cohort = 0;
    do {
       if (g_fruit_no[cohort] > 0.0)
           {
           abort_fract = divide (g_dlt_fruit_no_abort[cohort]
                                , g_fruit_no[cohort], 0.0)
                                * c_dm_abort_fract;

           for (part = pod; part < max_part; part++)
              {
              dm_tot_fruit =
                     ( g_dm_fruit_green[cohort][part]
                     + g_dlt_dm_fruit[cohort][part]
                     + g_dm_fruit_retranslocate[cohort][part]);

              if (dm_tot_fruit > tolerance_dm)
                 dm_tot_fruit_abort = dm_tot_fruit * abort_fract;
              else
                 dm_tot_fruit_abort = dm_tot_fruit;

              dlt_dm_fruit_abort[cohort][part] = dm_tot_fruit_abort
                                                 * c_dm_abort_fract;

              dm_fruit_abort_retrans = dm_tot_fruit_abort
                                     - dlt_dm_fruit_abort[cohort][part];

              g_dm_fruit_retranslocate[cohort][part] =
                           g_dm_fruit_retranslocate[cohort][part]
                           - dm_fruit_abort_retrans;

              //retranslocate fractiion of aborted fruit dm to stem
              g_dm_fruit_retranslocate[0][stem] =
                           g_dm_fruit_retranslocate[0][stem]
                           + dm_fruit_abort_retrans;
              }
           }
       else
           {
           //no fruit in this cohort
           for (part = pod; part < max_part; part++)
              {
              dlt_dm_fruit_abort[cohort][part] = 0.0;
              }
           }
       cohort++;
    } while (cohort < g_num_fruit_cohorts);

    pop_routine (my_name);
    }

void Plant::plant_fruit_n_retranslocate (
           int   max_part
          ,float c_dm_abort_fract
          ,int   max_fruit_cohorts
          ,int   g_num_fruit_cohorts
          ,float **g_dm_fruit_green
          ,float *g_n_green
          ,float **g_dlt_dm_fruit_abort
          ,float *g_n_fruit_retranslocate )
    {
    int part, cohort;
    float dlt_dm_abort;
    float dm_fruit_abort_retrans;
    float dm_tot_fruit_abort;
    float dm_green;
    float green_n_conc;
    float n_fruit_abort_retrans;


    for(part = pod; part < max_part; part++)
       {
       dm_green = 0.0;
       for (cohort = 0; cohort < g_num_fruit_cohorts; cohort++)
       	 dm_green += g_dm_fruit_green[cohort][part];
       if (dm_green > tolerance_dm)
          {
          green_n_conc = divide (g_n_green[part]
                                 , dm_green
                                 , 0.0);
          dlt_dm_abort = 0.0;
          for (cohort = 0; cohort < g_num_fruit_cohorts; cohort++)
              dlt_dm_abort += g_dlt_dm_fruit_abort[cohort][part];

          dm_tot_fruit_abort = divide (dlt_dm_abort
                                      , c_dm_abort_fract
                                      , 0.0);
          dm_fruit_abort_retrans = dm_tot_fruit_abort - dlt_dm_abort;

          n_fruit_abort_retrans = dm_fruit_abort_retrans * green_n_conc;
          }
       else
          {
          n_fruit_abort_retrans = g_n_green[part];
          }
       g_n_fruit_retranslocate[part] = g_n_fruit_retranslocate[part]
                                      - n_fruit_abort_retrans;

       // retranslocate fractiion of aborted fruit N to stem
       g_n_fruit_retranslocate[stem] = g_n_fruit_retranslocate[stem]
                                      + n_fruit_abort_retrans;
       }
    }

//+  Purpose
//       Update plant fruit

//+  Mission Statement
//       Update plant fruit
void Plant::plant_fruit_update(
     float  g_plants
    ,float  g_dlt_plants
    ,float  g_dlt_fruit_flower_no
    ,float  g_dlt_fruit_site_no
    ,float  *g_dlt_fruit_no_abort
    ,float  **g_dlt_dm_fruit_senesced
    ,float  **g_dlt_dm_fruit_green
    ,float  **g_dlt_dm_fruit_retrans
    ,float  **g_dlt_dm_fruit_abort
    ,float  g_dlt_dm_parasite
    ,float  **g_dm_fruit_green
    ,float  **g_dm_fruit_dead
    ,float  **g_dm_fruit_senesced
    ,int    g_num_fruit_cohorts
    ,float  *g_fruit_no
    ,float  *g_fruit_site_no      // IN/OUT
    ,float  g_dlt_dm
    ,float  *g_fruit_sdr
    ,int    g_day_of_year
    ,int    g_year
    ,float  *g_dlt_dm_daily
    ,float  **g_fruit_sdr_daily
    ,bool   *g_setting_fruit)     //IN/OUT
{
    //+  Constant Values
    const char*  my_name = "plant_fruit_update" ;

    //+  Local Variables
    int   part;
    int   cohort;
    float dying_fract_plants;
    float dlt_dm_green_dead;
    float dlt_dm_senesced_dead;
    float dlt_fruit_no_lost;
    float dlt_dm_veg;

//- Implementation Section ----------------------------------

    push_routine (my_name);

    *g_fruit_site_no  = *g_fruit_site_no  + g_dlt_fruit_site_no;
    //fprintf(stdout, "%3d %10f\n", g.day_of_year, *g_fruit_site_no);
    if (g_dlt_fruit_flower_no <= 0.0 && g_setting_fruit)
       {
       *g_setting_fruit = false;
       }
    else if (g_dlt_fruit_flower_no > 0.0 && !g_setting_fruit)
       {
       *g_setting_fruit = true;
       }
    else
       {
       // carry on with current fruit setting status
       }

    dlt_dm_veg = 0.0;
    cohort = 0;
    do {
       for (part = leaf; part <= stem; part++)
          dlt_dm_veg += g_dlt_dm_fruit_green[cohort][part];
       cohort++;
       } while (cohort < g_num_fruit_cohorts);
    crop_store_value(g_day_of_year, g_year, g_dlt_dm_daily,
                     dlt_dm_veg + g_dlt_dm_parasite);

    // C4
    //fprintf(stdout, "%d %f\n", g_day_of_year,   g_dlt_dm);
    dying_fract_plants = divide (-g_dlt_plants, g_plants, 0.0);
    dying_fract_plants = bound (dying_fract_plants, 0.0, 1.0);

    if (g_num_fruit_cohorts > 0)
        {
        cohort = 0;
        do {
           if (g_fruit_no[cohort] > 0.0)
              crop_store_value(g_day_of_year, g_year
                          , g_fruit_sdr_daily[cohort]
                          , g_fruit_sdr[cohort]);

           // transfer plant grain no.
           g_fruit_no[cohort] = g_fruit_no[cohort] - g_dlt_fruit_no_abort[cohort];
           dlt_fruit_no_lost  = g_fruit_no[cohort] * dying_fract_plants;
           g_fruit_no[cohort] = g_fruit_no[cohort] - dlt_fruit_no_lost;

           for (part = 0; part < max_part; part++)
              {
              g_dm_fruit_green[cohort][part] =
                  g_dm_fruit_green[cohort][part]
                  + g_dlt_dm_fruit_green[cohort][part]
                  + g_dlt_dm_fruit_retrans[cohort][part]
                  - g_dlt_dm_fruit_senesced[cohort][part]
                  - g_dlt_dm_fruit_abort[cohort][part];

              dlt_dm_green_dead = g_dm_fruit_green[cohort][part] * dying_fract_plants;
              g_dm_fruit_green[cohort][part] = g_dm_fruit_green[cohort][part] - dlt_dm_green_dead;
              g_dm_fruit_dead[cohort][part] = g_dm_fruit_dead[cohort][part] + dlt_dm_green_dead;

              dlt_dm_senesced_dead = g_dm_fruit_senesced[cohort][part] * dying_fract_plants;
              g_dm_fruit_senesced[cohort][part] = g_dm_fruit_senesced[cohort][part] - dlt_dm_senesced_dead;
              g_dm_fruit_dead[cohort][part] = g_dm_fruit_dead[cohort][part] + dlt_dm_senesced_dead;
              }
            cohort++;
            } while (cohort < g_num_fruit_cohorts);

        }
    else
        {
        for (part = 0; part < max_part; part++)
           {
           g_dm_fruit_green[0][part] =
                 g_dm_fruit_green[0][part]
                 + g_dlt_dm_fruit_green[0][part]
                 + g_dlt_dm_fruit_retrans[0][part]
                 - g_dlt_dm_fruit_senesced[0][part];

           dlt_dm_green_dead = g_dm_fruit_green[0][part] * dying_fract_plants;
           g_dm_fruit_green[0][part] =
                g_dm_fruit_green[0][part]
                - dlt_dm_green_dead;
           g_dm_fruit_dead[0][part] =
                g_dm_fruit_dead[0][part]
                + dlt_dm_green_dead;

           dlt_dm_senesced_dead = g_dm_fruit_senesced[0][part] * dying_fract_plants;
           g_dm_fruit_senesced[0][part] =
                g_dm_fruit_senesced[0][part]
                - dlt_dm_senesced_dead;
           g_dm_fruit_dead[0][part] =
                g_dm_fruit_dead[0][part]
                + dlt_dm_senesced_dead;
           }
       }
    pop_routine (my_name);
    }


//+  Purpose
//       Returns cumulative thermal time targets required for the
//       individual growth stages.

//+  Mission Statement
//     Get the cumulative thermal time targets for growth phases
void Plant::plant_fruit_phenology_init(
     float c_twilight                          // (INPUT)  twilight in angular distance b
    ,float *g_current_stage                    // (INPUT)  current phenological stage
    ,float **g_days_tot                        // (INPUT)  duration of each phase (days)
    ,int   max_fruit_stage
    ,int   max_fruit_cohorts
    ,float g_num_fruit_cohorts
    ,float g_day_of_year                       // (INPUT)  day of year
    ,float g_latitude                          // (INPUT)  latitude (degrees, negative fo
    ,float *p_x_pp_flower_to_start_grain       // (INPUT)
    ,float *p_y_tt_flower_to_start_grain       // (INPUT)
    ,float p_num_pp_flower_to_start_grain      // (INPUT)
    ,float *p_x_pp_start_to_end_grain          // (INPUT)
    ,float *p_y_tt_start_to_end_grain          // (INPUT)
    ,float p_num_pp_start_to_end_grain         // (INPUT)
    ,float p_tt_end_grain_to_maturity          // (INPUT)
    ,float p_tt_maturity_to_ripe               // (INPUT)  growing deg day required to fo
    ,float **phase_tt)                         // (INPUT/OUTPUT) cumulative growing
                                               // degree days required for
                                               // each stage (deg days)
{
#if 0
    const char*  my_name = "plant_fruit_phenology_init" ;

    float photoperiod;                            // hours of photosynthetic light (hours)
    int   cohort;

    //- Implementation Section ----------------------------------

    push_routine (my_name);

    photoperiod = day_length (g_day_of_year
                            , g_latitude
                            , c_twilight);

    phase_tt[g.num_fruit_cohorts][0/*flower_to_start_grain-1*/] =
          linear_interp_real(photoperiod
                            ,p_x_pp_flower_to_start_grain
                            ,p_y_tt_flower_to_start_grain
                            ,p_num_pp_flower_to_start_grain);

    cohort = 0;
    do {
       if (0/*stage_is_between (flowering, start_grain_fill, g_current_stage[cohort])*/)
           {
           if (0/*on_day_of(flowering, g_current_stage[cohort])*/)
               {
               phase_tt[cohort][end_grain_to_maturity-1] = p_tt_end_grain_to_maturity;
               phase_tt[cohort][maturity_to_ripe-1] = p_tt_maturity_to_ripe;
               }
           phase_tt[cohort][flower_to_start_grain-1] = linear_interp_real
                                                       (photoperiod
                                                       ,p_x_pp_flower_to_start_grain
                                                       ,p_y_tt_flower_to_start_grain
                                                       ,p_num_pp_flower_to_start_grain);
           phase_tt[cohort][start_to_end_grain-1] = linear_interp_real
                                                   (photoperiod
                                                   ,p_x_pp_start_to_end_grain
                                                   ,p_y_tt_start_to_end_grain
                                                   ,p_num_pp_start_to_end_grain);
           }
      else if (stage_is_between (start_grain_fill, end_grain_fill, g_current_stage[cohort]))
           {
           phase_tt[cohort][start_to_end_grain-1] = linear_interp_real
                                                    (photoperiod
                                                    ,p_x_pp_start_to_end_grain
                                                    ,p_y_tt_start_to_end_grain
                                                    ,p_num_pp_start_to_end_grain);
           }
      cohort++;
      } while (cohort < g_num_fruit_cohorts);
   pop_routine (my_name);
#endif
   }


//+  Purpose
//     Use temperature, photoperiod and genetic characteristics
//     to determine when the crop begins a new growth phase.
//     The initial daily thermal time and height are also set.

//+  Mission Statement
//   Calculate crop phenological development using thermal time targets.
void Plant::plant_fruit_phenology (
     float *g_previous_stage
    ,float *g_current_stage
    ,int   initial_stage
    ,int   end_development_stage
    ,int   start_grain_fill
    ,int   end_grain_fill
    ,int   max_stage
    ,int   max_fruit_cohorts
    ,int   g_num_fruit_cohorts
    ,float **g_dm_fruit_green
    ,int   p_dm_fruit_max
    ,float *g_fruit_no
    , int   c_num_temp
    , float *c_x_temp
    , float *c_y_tt
    , float g_maxt
    , float g_mint
    , float g_swdef_pheno_flower
    , float g_swdef_pheno_grainfill
    ,float **g_phase_tt
    ,float *g_phase_devel
    ,float *g_dlt_tt
    ,float *g_dlt_stage
    ,float **g_tt_tot
    ,float **g_days_tot)
{
#if 0
    int   cohort;
    const char*  my_name = "plant_fruit_phenology" ;

    //- Implementation Section ----------------------------------

    push_routine (my_name);

    cohort = 0;
    do {
       g_previous_stage[cohort] = g_current_stage[cohort];
       if (stage_is_between(initial_stage
                           , start_grain_fill
                           ,g_current_stage[cohort]))
          {
          crop_thermal_time(c_num_temp
                          , c_x_temp
                          , c_y_tt
                          , g_current_stage[cohort]
                          , g_maxt
                          , g_mint
                          , initial_stage
                          , start_grain_fill
                          , g_swdef_pheno_flower    // no nstress, so just repeat swdef stress here
                          , g_swdef_pheno_flower
                          , &g_dlt_tt[cohort]);
          }
       else if (stage_is_between(start_grain_fill
                                , end_development_stage
                                , g_current_stage[cohort]))
          {
          crop_thermal_time (c_num_temp
                           , c_x_temp
                           , c_y_tt
                           , g_current_stage[cohort]
                           , g_maxt
                           , g_mint
                           , start_grain_fill
                           , end_development_stage
                           , g_swdef_pheno_grainfill    // no nstress, so just repeat swdef stress here
                           , g_swdef_pheno_grainfill
                           , &g_dlt_tt[cohort]);
          }

       plant_fruit_phase_devel(initial_stage
                               , end_development_stage
                               , start_grain_fill
                               , end_grain_fill
                               , g_current_stage[cohort]
                               , g_dm_fruit_green[cohort]
                               , p_dm_fruit_max
                               , g_fruit_no[cohort]
                               , g_dlt_tt[cohort]
                               , g_phase_tt[cohort]
                               , g_tt_tot[cohort]
                               , &g_phase_devel[cohort]);

       crop_devel(g_phase_devel[cohort]
                   , &g_dlt_stage[cohort]
                   , &g_current_stage[cohort]);

       // update thermal time states and day count
       accumulate (g_dlt_tt[cohort], g_tt_tot[cohort]
                    , g_previous_stage[cohort]-1.0, g_dlt_stage[cohort]);

       accumulate (1.0, g_days_tot[cohort]
                    , g_previous_stage[cohort]-1.0, g_dlt_stage[cohort]);
       cohort++;
    } while (cohort < g_num_fruit_cohorts);

    pop_routine (my_name);
#endif
  }

//+  Purpose
//     Determine the fraction of current phase elapsed ().

//+  Mission statement
//   Determine the progress through the current growth stage.

void Plant::plant_fruit_phase_devel( int    initial_stage                  // (INPUT)
                              ,int    end_development_stage          // (INPUT)
                              ,int    start_grain_fill               // (INPUT)
                              ,int    end_grain_fill                 // (INPUT)
                              ,float  g_current_stage                // (INPUT)  current phenological stage
                              ,float  *g_dm_fruit_green              // (INPUT)
                              ,float  p_dm_fruit_max                 // (INPUT)
                              ,float  g_fruit_no                     // (INPUT)
                              ,float  g_dlt_tt                       // (INPUT)  daily thermal time (growing de
                              ,float  *g_phase_tt                    // (INPUT)  Cumulative growing degree days
                              ,float  *g_tt_tot                      // (INPUT)  the sum of growing degree days
                              ,float  *phase_devel)                   // (OUTPUT) fraction of current phase elapsed
    {
    //+  Local variables
    float phase_devel_grain;
    const char*  my_name = "fruit_phase_devel" ;

    //- Implementation Section ----------------------------------
    push_routine (my_name);

    if (phenology->inPhase("fruit_filling"))
        {
        *phase_devel =  crop_phase_tt( g_dlt_tt
                                    , g_phase_tt
                                    , g_tt_tot
                                    , g_current_stage);
        }
    else
        {
        *phase_devel = fmod(g_current_stage, 1.0);
        }


   if (phenology->inPhase ("grainfill"))
       {
       phase_devel_grain = divide (sum_real_array(&g_dm_fruit_green[pod]
                                                  ,oil-pod)
                                  ,p_dm_fruit_max*g_fruit_no, 0.0);
       }
   else
       {
       phase_devel_grain = 0.0;
       }


   *phase_devel = max(*phase_devel, phase_devel_grain);
   *phase_devel = bound(*phase_devel, 0.0, 1.0);

   pop_routine (my_name);
   }


//+  Purpose
//       Zero crop variables & arrays

//+  Mission Statement
//     Zero all the global variables and arrays
void Plant::fruit_zero_all_globals ()
    {
    const char*  my_name = "fruit_zero_all_globals" ;

    //- Implementation Section ----------------------------------

    push_routine (my_name);

    // Fruit Globals
    for (int i=0; i < max_fruit_cohorts; i++) {
      g.current_fruit_stage[i]    = 0.0;
    	for (int j = 0; j < max_fruit_stage; j++) {
        g.fruit_phase_tt[i][j]         = 0.0;
        g.fruit_days_tot[i][j]         = 0.0;
      }
    }
    pop_routine (my_name);
    }



//+  Purpose
//       Perform grain filling calculations
void Plant::legnew_bio_yieldpart_demand3(
        int   max_fruit_cohorts,                           // (INPUT)
        int   max_part,                                    // (INPUT)
        int   max_fruit_stage,                             // (INPUT)
        int   g_num_fruit_cohorts,                         // (INPUT)
        float *g_current_stage,                            // (INPUT)
        int   flowering,                                   // (INPUT)
        int   start_grain_fill,                            // (INPUT)
        int   end_grain_fill,                              // (INPUT)
        float *y_tt_flower_to_start_grain,                 // (INPUT)
        float *y_tt_fruit_start_to_end_grain,              // (INPUT)
        float *g_dlt_tt,                                    // (INPUT)
        float **g_tt_tot,                                  // (INPUT)
        float *g_fruit_no,                                 // (INPUT)
        float p_potential_fruit_filling_rate,              // (INPUT)
        float p_dm_fruit_max,                              // (INPUT)
        float **g_dm_fruit_green,                          // (INPUT)
        float g_maxt,                                      // (INPUT)
        float g_mint,                                      // (INPUT)
        float *c_x_temp_grainfill,                         // (INPUT)
        float *c_y_rel_grainfill,                          // (INPUT)
        int   c_num_temp_grainfill,                        // (INPUT)
        float *c_x_stage_no_partition,                     // (INPUT)
        float *c_y_frac_pod,                               // (INPUT)
        int   c_num_stage_no_partition,
        float c_tt_flower_to_start_pod,
        float **g_fruit_phase_tt,                          // INPUT
        float g_grain_energy,                              // (INPUT)
        float *dlt_dm_fruit_demand,                         //(OUTPUT)
        float *dlt_dm_grain_demand)                         //(OUTPUT)
{
    const char*  my_name = "legnew_bio_yieldpart_demand3" ;

    //+  Local Variables
    float tav;
    float dlt_dm_yield_unadj;                     // grain demand for carbohydrate, unadjusted
                                                  // for energy conversion (g/m^2)
    float potential_grain_filling_rate;
    float temp_fac;
    float dlt_dm_pod_demand;
    float dm_max;
    float dm_pod_max;
    float tt_fruit_age, tt_fruit_age_max, m1, m2;
    float dlt_fruit_age;
//    int   current_phase;
    int   cohort;
    float dummy[1];
    float dlt_dm_fruit_max;
    float pod_age_fract;
    float frac_pod;
    //- Implementation Section ----------------------------------
    push_routine (my_name);

    m1 = y_tt_flower_to_start_grain[0];
    for (int i = 1; i < max_table; i++)
       m1 = max(y_tt_flower_to_start_grain[i], m1);

    m2 =y_tt_fruit_start_to_end_grain[0];
    for (int i = 1; i < max_table; i++)
       m2 = max(y_tt_fruit_start_to_end_grain[i], m2);

    tt_fruit_age_max = m1 + m2;

    tav = (g_maxt + g_mint) / 2.0;
    temp_fac = linear_interp_real(tav
                                  ,c_x_temp_grainfill
                                  ,c_y_rel_grainfill
                                  ,c_num_temp_grainfill);

    cohort = 0;
    do {
       if (g_fruit_no[cohort] > 0.0)
           {
           if (stage_is_between(flowering, start_grain_fill, g_current_stage[cohort]))
               {
               // we are in flowering stage
               dlt_fruit_age = divide(g_dlt_tt[cohort],
                                      tt_fruit_age_max, 0.0);
               frac_pod = linear_interp_real(g_current_stage[cohort]
                                            ,c_x_stage_no_partition
                                            ,c_y_frac_pod
                                            ,c_num_stage_no_partition);
               dm_max = p_dm_fruit_max
                      * temp_fac
                      * g_fruit_no[cohort]
                      * frac_pod;

               dlt_dm_pod_demand = dm_max * dlt_fruit_age;

               dlt_dm_fruit_demand[cohort] = dlt_dm_pod_demand;
               dlt_dm_grain_demand[cohort] = 0.0;
               }
           else if (stage_is_between(start_grain_fill,end_grain_fill,g_current_stage[cohort]))
               {
               // we are in grain filling stage
               frac_pod = linear_interp_real(g_current_stage[cohort]
                                            ,c_x_stage_no_partition
                                            ,c_y_frac_pod
                                            ,c_num_stage_no_partition);

               potential_grain_filling_rate = divide(p_potential_fruit_filling_rate
                                                     , 1.0 + frac_pod
                                                     , 0.0);

               dlt_dm_yield_unadj = g_fruit_no[cohort]
                                       * potential_grain_filling_rate
                                       * temp_fac
                                       * g_dlt_tt[cohort];

              // adjust for grain energy
              dlt_dm_grain_demand[cohort] = dlt_dm_yield_unadj * g_grain_energy;
              dlt_dm_pod_demand = dlt_dm_yield_unadj * frac_pod;
              dlt_dm_fruit_demand[cohort] = dlt_dm_pod_demand + dlt_dm_grain_demand[cohort];
              dlt_dm_fruit_max = g_fruit_no[cohort] * p_dm_fruit_max
                      - sum_real_array(&g_dm_fruit_green[cohort][pod], max_part-pod);
              dlt_dm_fruit_max = l_bound (dlt_dm_fruit_max, 0.0);

              // adjust the demands
              if (dlt_dm_fruit_demand[cohort] > dlt_dm_fruit_max)
                  {
                  dlt_dm_grain_demand[cohort] = dlt_dm_grain_demand[cohort]
                                                 * divide (dlt_dm_fruit_max
                                                           , dlt_dm_fruit_demand[cohort]
                                                           , 0.0);
                  dlt_dm_fruit_demand[cohort] = dlt_dm_fruit_max;
                  }
              }
           else
              {
              // no changes
              dlt_dm_grain_demand[cohort] = 0.0;
              dlt_dm_fruit_demand[cohort] = 0.0;
              }
           }
       else
           {
           // no fruit
           dlt_dm_grain_demand[cohort] = 0.0;
           dlt_dm_fruit_demand[cohort] = 0.0;
           }
       cohort++;
       } while (cohort < g_num_fruit_cohorts);
       pop_routine (my_name);
   }


//  Purpose
//      Calculate Fruit Site Number
//
//  Mission Statement
//      Calculate Fruit Numer
void Plant::plant_fruit_site_number (int option)
   {
   const char *my_name = "plant_fruit_site_number";

   push_routine (my_name);

   if (option == 1)
      {
      // do not use fruit number
      g.fruit_site_no = 0;
      g.dlt_fruit_site_no = 0.0;
      }
   else if (option == 2)
      {
      crop_fruit_site_number (phenology->stageNumber()
                       , 0//initial_fruit_stage
                       , 0//end_grain_fill
                       , g.fruit_tt_tot
                       , g.fruit_phase_tt
                       , p.cutout_fract
                       , g.plants
                       , max_fruit_cohorts
                       , max_table
                       , p.x_node_no_fruit_sites
                       , p.y_fruit_sites_per_node
                       , p.num_node_no_fruit_sites
                       , &g.node_no_first_flower
                       , 0//emerg
                       , 0//maturity
                       , g.node_no
                       , g.maxt
                       , g.mint
                       , c.x_temp_fruit_site
                       , c.y_rel_fruit_site
                       , c.num_temp_fruit_site
                       , g.dlt_node_no
                       , &g.dlt_fruit_site_no );
      }
   else
      throw std::invalid_argument ("Invalid template option for plant fruit number");

   pop_routine (my_name);
   }

//  Purpose
//      Calculate Fruit Site Number
//
//  Mission Statement
//      Calculate Fruit Numer
void Plant::plant_fruit_number (int option)
   {
#if 0
   const char *my_name = "plant_fruit_number";

   push_routine (my_name);

   if (option == 1)
      {
      // do not use fruit number
      fill_real_array(g.fruit_no,0, max_fruit_cohorts);
      fill_real_array(g.dlt_fruit_no,0, max_fruit_cohorts);
      fill_real_array(g.fruit_flower_no,0, max_fruit_cohorts);
      }
   else if (option == 2)
      {
      crop_fruit_flower_number (p.dm_fruit_set_crit
                       , p.dm_fruit_set_min
                       , g.dlt_dm
                       , g.dlt_dm_daily
                       , c.days_assimilate_ave
                       , g.day_of_year
                       , g.year
                       , g.fruit_flower_no
                       , max_fruit_cohorts
                       , g.dlt_fruit_site_no
                       , g.setting_fruit
                       , &g.dlt_fruit_flower_no);
      if (g.num_fruit_cohorts > 0)
         {
         g.fruit_flower_no[g.num_fruit_cohorts-1] = g.dlt_fruit_flower_no;
         }
      crop_fruit_number(flowering
                      , max_table
                      , max_fruit_cohorts
                      , g.num_fruit_cohorts
                      , c.tt_flower_to_start_pod
                      , g.fruit_tt_tot
                      , g.fruit_flower_no
                      , g.dlt_fruit_no);

       // update fruit and flower numbers now
      for (int cohort = 0; cohort < g.num_fruit_cohorts; cohort++)
         {
         g.fruit_no[cohort] +=  g.dlt_fruit_no[cohort];
         g.fruit_flower_no[cohort] -= g.dlt_fruit_no[cohort];
         }
      }
   else
      throw std::invalid_argument ("Invalid template option for plant fruit number");

   pop_routine (my_name);
#endif
   }


