//---------------------------------------------------------------------------

#include <string>
#include <math.h>
#include "PlantLibrary.h"
//---------------------------------------------------------------------------

using namespace std;

//===========================================================================
void crop_n_conc_ratio(const int leaf,         //IN
                       const int stem,         //IN
                       float *dm_green,         //(INPUT)  live plant dry weight (biomass)
                       float *N_conc_crit,      //(INPUT)  critical N concentration (g N/g 
                       float *N_conc_min,       //(INPUT)  minimum N concentration (g N/g b
                       float *N_green,          //(INPUT)  plant nitrogen content (g N/m^2)
                       float *N_conc_ratio)     //(OUTPUT) available N as fraction of N c
//===========================================================================

/*  Purpose
*   Calculate the fractional position of leaf and stem nitrogen concentration
*   between the minimum and critical concentrations.
*
*  Mission Statement
*   Calculate N concentration ratio for leaf and stem
*
*  Changes
*     010994 jngh specified and programmed
*     090695 psc  added N_fact for phenology & externalise multipliers for ndef
*     970314 slw extracted this common code from nitrogen stress routines
*/
   {
   //  Local Variables
   float N_conc_stover;          // tops (stover) actual N concentration
                                 // (0-1)
   float dm_stover;              // tops (stover) plant weight (g/m^2)
   float N_conc_stover_crit;     // tops (stover) critical N concentration
                                 // (0-1)
   float N_conc_stover_min;      // tops (stover) minimum N concentration
                                 // (0-1)
   float N_leaf_crit;            // critical leaf nitrogen (g/m^2)
   float N_leaf_min;             // minimum leaf nitrogen (g/m^2)
   float N_stem_crit;            // critical stem nitrogen (g/m^2)
   float N_stem_min;             // minimum stem nitrogen (g/m^2)
   float N_stover;               // tops (stover) plant nitrogen (g/m^2)
   float N_stover_crit;          // critical top nitrogen (g/m^2)
   float N_stover_min;           // minimum top nitrogen (g/m^2)
   float dividend;
   float divisor;
   // Implementation Section ----------------------------------

   // calculate actual N concentrations

   dm_stover = dm_green[leaf] + dm_green[stem];
   N_stover = N_green[leaf] + N_green[stem];

   N_conc_stover = divide (N_stover, dm_stover, 0.0);

   // calculate critical N concentrations
   N_leaf_crit = N_conc_crit[leaf] * dm_green[leaf];
   N_stem_crit = N_conc_crit[stem] * dm_green[stem];
   N_stover_crit = N_leaf_crit + N_stem_crit;

   N_conc_stover_crit = divide (N_stover_crit, dm_stover, 0.0);

   // calculate minimum N concentrations

   N_leaf_min = N_conc_min[leaf] * dm_green[leaf];
   N_stem_min = N_conc_min[stem] * dm_green[stem];
   N_stover_min = N_leaf_min + N_stem_min;

   N_conc_stover_min = divide (N_stover_min, dm_stover, 0.0);

   //calculate shortfall in N concentrations
   dividend =  N_conc_stover - N_conc_stover_min;
   divisor =   N_conc_stover_crit - N_conc_stover_min;
   *N_conc_ratio = divide (dividend, divisor, 0.0);
   }


//=========================================================================
void crop_nfact_photo(const int leaf, 
                      const int stem,
                      float *dm_green,       //(INPUT)  live plant dry weight (biomass)
                      float *n_conc_crit,    //(INPUT)  critical N concentration (g N/g
                      float *n_conc_min,     //(INPUT)  minimum N concentration (g N/g b
                      float *n_green,        //(INPUT)  plant nitrogen content (g N/m^2)
                      float n_fact_photo,   //(INPUT)  multipler for N deficit effect o 
                      float *nfact)          //(OUTPUT) N stress factor
//=========================================================================

/*  Purpose
*     The concentration of Nitrogen in leaves and stem is used to derive a
*     Nitrogen stress index for photosynthesis.  The stress index for
*     photosynthesis is calculated from today's relative nutritional status
*     between a critical and minimum leaf Nitrogen concentration.
*
*  Mission Statement
*   Calculate Nitrogen stress factor for photosynthesis
*
*  Changes
*     060495 nih taken from template
*     970215 slw split from mungb_nfact
*
*/
   {
   float N_conc_ratio;          // available N as fraction of N capacity
   float N_def;                 // N factor (0-1)

   crop_n_conc_ratio(leaf, stem, dm_green, n_conc_crit, n_conc_min, n_green, &N_conc_ratio);
   N_def = n_fact_photo * N_conc_ratio;
   *nfact = bound (N_def, 0.0, 1.0);
   }


//========================================================================
void crop_nfact_pheno(const int leaf,        //IN
                      const int stem,        //IN
                      float *dm_green,       //(INPUT)  live plant dry weight (biomass)
                      float *n_conc_crit,    //(INPUT)  critical N concentration (g N/g
                      float *n_conc_min,     //(INPUT)  minimum N concentration (g N/g b
                      float *n_green,        //(INPUT)  plant nitrogen content (g N/m^2)
                      float n_fact_pheno,    //(INPUT)  multipler for N deficit effect on phenology 
                      float *nfact)          //(OUTPUT) N stress factor
//========================================================================
/*  Purpose
*     The concentration of Nitrogen in leaves and stem is used to derive a
*     Nitrogen stress index for phenological development. The stress index for
*     phenology is calculated from today's relative nutritional status between
*     a critical and minimum leaf Nitrogen concentration.
*
*  Mission Statement
*   Calculate Nitrogen stress factor for phenological development
*
*  Changes
*     060495 nih taken from template
*     970215 slw split from mungb_nfact
*
*/
   {
   float N_conc_ratio;          // available N as fraction of N capacity
   float N_def;                 // N factor (0-1)

   crop_n_conc_ratio(leaf, stem, dm_green, n_conc_crit, n_conc_min,n_green, &N_conc_ratio);
   N_def = n_fact_pheno * N_conc_ratio;
   *nfact = bound (N_def, 0.0, 1.0);
   }

//===========================================================================
void crop_nfact_grain_conc(const int leaf, 
                           const int stem,
                           float *dm_green,         //(INPUT)  live plant dry weight (biomass)              
                           float *n_conc_crit,      //(INPUT)  critical N concentration (g N/g              
                           float *n_conc_min,       //(INPUT)  minimum N concentration (g N/g b             
                           float *n_green,          //(INPUT)  plant nitrogen content (g N/m^2)             
                           float *nfact)            //(OUTPUT) N stress factor
//===========================================================================

/*  Purpose
*     The concentration of Nitrogen in leaves and stem is used to derive a
*     Nitrogen stress index for grain N accumulation. The stress index for
*     grain N accumulation is calculated from today's relative nutritional
*     status between a critical and minimum leaf Nitrogen concentration.
*
*  Mission Statement
*   Calculate Nitrogen stress factor for grain Nitrogen content
*
*  Changes
*     060495 nih taken from template
*     970215 slw split from mungb_nfact
*/
   {
   float N_conc_ratio;          // available N as fraction of N capacity  (0-1)
   crop_n_conc_ratio(leaf, stem, dm_green, n_conc_crit, n_conc_min, n_green, &N_conc_ratio);
   *nfact = bound (N_conc_ratio, 0.0, 1.0);
   }


//==========================================================================
void crop_nfact_expansion(const int leaf, 
                          float *dm_green,         //(INPUT) live plant dry weight (biomass)  (g/m^2)
                          float *N_conc_crit,      //(INPUT)  critical N concentration (g N/ biomass)                               
                          float *N_conc_min,       // (INPUT) minimum N concentration (g N/g biomass) 
                          float *N_green,          // (INPUT) plant nitrogen content (g N/m^2 
                          float N_fact_expansion, //(INPUT) multipler for N deficit effect leaf expansion 
                          float *nfact)            //(OUTPUT) stress factor 
//==========================================================================

/*  Purpose
*   The concentration of Nitrogen in leaves is used to derive a Nitrogen stress index
*   for leaf expansion. The stress index for leaf expansion is calculated from today's
*   relative nutitional status between a critical and minimum leaf Nitrogen concentration.
*
*  Mission Statement
*   Calculate Nitrogen stress factor for expansion
*
*  Changes
*     010994 jngh specified and programmed
*     090695 psc  added N_fact for phenology & externalise multipliers for ndef
*     970318 slw split from sorg_nfact
*                                 ! 
*/
   {
   //  Local Variables
   float N_conc_leaf;             // leaf actual N concentration (0-1)
   float N_def;                   // N factor (0-1)
   float N_conc_leaf_crit;        // leaf critical N concentration (0-1)
   float N_conc_leaf_min;         // leaf minimum N concentration (0-1)
   float N_leaf_crit;             // critical leaf nitrogen (g/m^2)
   float N_leaf_min;              // minimum leaf nitrogen (g/m^2) (0-1)
   float N_conc_ratio_leaf;       // available N as fraction of N capacity
                                 // (0-1) for leaf only
   float dividend;
   float divisor;
   // Implementation Section ----------------------------------

   // calculate actual N concentrations
   N_conc_leaf = divide (N_green[leaf], dm_green[leaf], 0.0);

   // calculate critical N concentrations
   N_leaf_crit = N_conc_crit[leaf] * dm_green[leaf];
   N_conc_leaf_crit = divide (N_leaf_crit, dm_green[leaf], 0.0);

   // calculate minimum N concentrations
   N_leaf_min = N_conc_min[leaf] * dm_green[leaf];
   N_conc_leaf_min = divide (N_leaf_min, dm_green[leaf], 0.0);

   // calculate shortfall in N concentrations
   dividend = N_conc_leaf - N_conc_leaf_min;
   divisor = N_conc_leaf_crit - N_conc_leaf_min;

   N_conc_ratio_leaf = divide (dividend, divisor, 0.0);

   // calculate 0-1 N deficiency factors

//scc Thought I'd better put this in (it's in sugar module)
//scc This does not work properly, because the ratio takes no account of
//the fact that we might be trying to grow a huge amount of leaf today
//Run this on leaf instead of stover (like Sugar model)

   N_def = N_fact_expansion * N_conc_ratio_leaf;
   *nfact = bound (N_def, 0.0, 1.0);
   }

//============================================================================
void crop_n_detachment(const int num_part, 
                       const int root,
                       const int leaf, 
                       float dm_leaf_detach_frac,  // (INPUT)  fraction of senesced leaf dry matter detaching from live plant each day (0-1)
                       float *dlt_N_senesced,      //  (INPUT)  actual N loss with senesced plant (g/m^2)
                       float *dlt_N_detached)      // (OUTPUT) actual nitrogen senesced  from plant parts (g/m^2)
//============================================================================

/*  Purpose
*   Calculate today's Nitrogen lost from senesced pools due to detachment
*
*  Mission Statement
*   Calculate today's Nitrogen lost from senesced pools due to detachment
*
*  Changes
*       091294 jngh specified and programmed
*       970317 slw extracted from Mungbean
*
*/
   {
   // first we zero all plant component deltas
   fill_real_array (dlt_N_detached, 0.0, num_part);

   dlt_N_detached[leaf] = dlt_N_senesced[leaf] * dm_leaf_detach_frac;
   dlt_N_detached[root] = dlt_N_senesced[root];
   }


//==========================================================================
void crop_n_dead_detachment(int num_part,
                            float *dead_detach_frac, //(INPUT)  fraction of dead plant parts detaching each day (0-1)
                            float *n_dead,           //(INPUT)  plant N content of dead plants (g N/m^2)
                            float *dlt_N_dead_detached)  //(OUTPUT) change in dm of dead plants (g/m^2)
//==========================================================================

/*  Purpose
*      Plant Nitrogen loss from dead plants
*
*  Mission Statement
*   Calculate today's Nitrogen lost from dead pools due to detachment
*
*  Changes
*       091294 jngh specified and programmed
*       970317 slw extracted from Mungbean
*
*  Constant Values
*      character  my_name*(*)           ! name of procedure
*      parameter (my_name = 'crop_N_dead_detachment')
*
*  Sub-Program Arguments
*      int *num_part
*      float *dead_detach_frac(*) ! 
*      float *n_dead(*)           ! 
*      float *dlt_N_dead_detached(*)   ! 
*/
   {
   for(int part = 0; part < num_part; part++)
      {
      dlt_N_dead_detached[part] = n_dead[part]* dead_detach_frac[part];
      }
   }

//==========================================================================
float crop_n_dlt_grain_conc(const int grain,
                            float sfac_slope,      //(INPUT)  soil water stress factor slope
                            float sw_fac_max,      //(INPUT)  soil water stress factor maximum
                            float temp_fac_min,    //(INPUT)  temperature stress factor minimum optimum temp
                            float tfac_slope,      //(INPUT)  temperature stress factor slope
                            float maxt,            //(INPUT)  maximum air temperature (oC)
                            float mint,            //(INPUT)  maximum air temperature (oC)
                            float nfact_grain_conc,// (INPUT)
                            float *n_conc_crit,     //(INPUT)  critical N concentration (g N/g biomass)
                            float *n_conc_min,      //(INPUT)  minimum N concentration (g N/g biomass)
                            float swdef_expansion) // (INPUT)
//==========================================================================

/*  Purpose
*     Calculate the nitrogen concentration required to meet the increase
*     from daily grain growth (0-1) as affected by temperature and water stress.
*
*  Mission Statement
*   Calculate the nitrogen concentration required for grain growth.
*
*  Notes
*     First, two factors are calculated and used to estimate the
*     effects of mean temperature and drought stress on the N
*     concentration in grain growth for the day.  High temperature
*     or drought stress can cause the factors to exceed 1.
*     N deficiency can cause nfac < 1.  The net effect of these
*     equations is to allow grain nitrogen concentration to range
*     from less than .01 when N deficiency is severe to about .018
*     stress limit grain growth.
*     Here, optimum N concentration = 1.7%
*
*/
   {
   //  Local Variables
   float N_conc_pot;                   // potential grain N concentration
                                       // (0-1) (g N/g part)
   float N_grain_sw_fac;               // soil water stress factor for N
                                       // uptake
   float N_grain_temp_fac;             // temperature stress factor for N
                                       // uptake
   float ave_temp;                     // mean temperature (oC)

   ave_temp = (maxt + mint) / 2.0;

   //!!!!!!!!!! return to orig cm
   N_grain_temp_fac = temp_fac_min + tfac_slope * ave_temp;
   N_grain_sw_fac = sw_fac_max - sfac_slope * swdef_expansion ;

   // N stress reduces grain N concentration below critical
   N_conc_pot = n_conc_min[grain] + (n_conc_crit[grain] -
                  n_conc_min[grain]) * nfact_grain_conc;

            // Temperature and water stresses can decrease/increase grain
            // N concentration

            // when there is no N stress, the following can be a higher N conc than
            // the crit and thus the N conc of the grain can exceed N critical.

   return  (N_conc_pot * max (N_grain_temp_fac, N_grain_sw_fac));
   }


//===========================================================================
void crop_n_retrans_avail(const int num_part, 
                          const int root,
                          const int grain, 
                          float *g_N_conc_min, 
                          float *g_dm_green, 
                          float *g_N_green,
                          float *N_avail)
//===========================================================================

/*  Purpose
*     Calculate N available for transfer to grain (g/m^2)
*     from each plant part.  By definition, available grain N
*     is set to 0.
*
*  Mission Statement
*   Calculate the Nitrogen available for retranslocation to grain
*
*  Notes
*     N available for translocation to the grain is the sum of
*     N available in the stover.
*     N available in stover is the difference of its N content
*     and the minimum it's allowed to fall to.
*     NB. No translocation from roots.
*
*  Changes
*       080994 jngh specified and programmed
*       970318 slw extracted from Sorg
*
*/
   {
   float N_min;                  // nitrogen minimum level (g/m^2)
   int part;                     // plant part number

   // get grain N potential (supply) -----------
   // now find the available N of each part.
   for(part = 0; part < num_part; part++)
      {
      N_min = g_N_conc_min[part] * g_dm_green[part];
      N_avail[part] = l_bound (g_N_green[part] - N_min, 0.0);
      }
   N_avail[grain]  = 0.0;
   N_avail[root] = 0.0;
   }


//=========================================================================
void cproc_n_senescence1 (const int num_part,         //(INPUT) number of plant part
                          float *c_n_sen_conc,         //(INPUT)  N concentration of senesced materia (g/m^2)
                          float *g_dlt_dm_senesced,   //(INPUT)  plant biomass senescence (g/m^2)
                          float *g_n_green,           //(INPUT) nitrogen in plant material (g/m^2)
                          float *g_dm_green,          //(INPUT) plant material (g/m^2)
                          float *dlt_N_senesced)      // (OUTPUT) actual nitrogen senesced from plant parts (g/m^2)
//=========================================================================

/*  Purpose
*       Derives seneseced plant nitrogen (g N/m^2)
*
*  Mission Statement
*   Calculate change in senesced plant Nitrogen
*
*  Changes
*       121297 nih specified and programmed
*
*/
   {
   //  Local Variables
   int part;            // plant part counter variable
   float green_n_conc;  // N conc of green material (g/g)
   float sen_n_conc;    // N conc of senescing material (g/g)

    // first we zero all plant component deltas
   for(part = 0; part < num_part; part++)
      {
      green_n_conc = divide (g_n_green[part],g_dm_green[part],0.0);
      sen_n_conc = min (c_n_sen_conc[part], green_n_conc);
      dlt_N_senesced[part] = g_dlt_dm_senesced[part] * sen_n_conc;
      dlt_N_senesced[part] = u_bound (dlt_N_senesced[part],g_n_green[part]);
      }
   }

//===========================================================================
void cproc_n_uptake1(float C_no3_diffn_const,   //(INPUT)  time constant for uptake by di
                     float *G_dlayer,            //(INPUT)  thickness of soil layer I (mm)//
                     const int max_layer,        // INPUT)  max number of soil layers 
                     float *G_no3gsm_diffn_pot,  //(INPUT)  potential NO3 (supply) from so
                     float *G_no3gsm_mflow_avail,// (INPUT)  potential NO3 (supply) from
                     float G_n_fix_pot,         //(INPUT) potential N fixation (g/m2) 
                     const char *C_n_supply_preference, //(INPUT) 
                     float *G_n_demand,                //(INPUT)  critical plant nitrogen demand
                     float *G_n_max,                   //(INPUT)  maximum plant nitrogen demand
                     const int max_part,              //(INPUT)  number of plant parts
                     float G_root_depth,              // (INPUT)  depth of roots (mm)
                     float *dlt_no3gsm)                // (OUTPUT) actual plant N uptake from NO3 in each layer (g/m^2
//===========================================================================

/*  Purpose
*       Return actual plant nitrogen uptake from each soil layer.
*       N uptake is from nitrate only and comes via mass flow and active (diffusion) uptake.
*
*  Mission Statement
*   Calculate crop Nitrogen Uptake
*
*  Changes
*       160498 nih specified and programmed
*
*/
   {

   //  Local Variables
   int deepest_layer;                  // deepest layer in which the roots are
                                       // growing
   float NO3gsm_diffn;                 // actual N available (supply) for
                                       // plant (g/m^2) by diffusion
   float NO3gsm_mflow;                 // actual N available (supply) for
                                       // plant (g/m^2) by mass flow
   float *NO3gsm_diffn_avail = new float[crop_max_layer];   // potential NO3 (supply)
                                       // from soil (g/m^2), by diffusion
   float NO3gsm_diffn_supply;          // total potential N uptake (supply)
                                       // for plant (g/m^2) by diffusion
   float NO3gsm_mflow_supply;          // total potential N uptake (supply)
                                       // for plant (g/m^2) by mass flow
   float diffn_fract;                  // fraction of nitrogen to use (0-1)
                                       // for diffusion
   float mflow_fract;                  // fraction of nitrogen to use (0-1)
                                       // for mass flow
   int layer;                          // soil layer number of profile
   float N_demand;                     // total nitrogen demand (g/m^2)
   float NO3gsm_uptake;                // plant NO3 uptake from layer (g/m^2)
   float N_max;                        // potential N uptake per plant (g/m^2)
   float temp1;                  //Vars to pass composite terms to functions
   float temp2;                  //expecting a pointer
   // Implementation Section ----------------------------------

            // get potential N uptake (supply) from the root profile.
            // get totals for diffusion and mass flow.

   deepest_layer = find_layer_no (G_root_depth, G_dlayer, max_layer);
   for(layer = 0; layer <= deepest_layer; layer++)
      {
      NO3gsm_diffn_avail[layer] = G_no3gsm_diffn_pot[layer]- G_no3gsm_mflow_avail[layer];
      NO3gsm_diffn_avail[layer] = l_bound (NO3gsm_diffn_avail[layer], 0.0);
      }
   NO3gsm_mflow_supply = sum_real_array (G_no3gsm_mflow_avail , deepest_layer+1);
   NO3gsm_diffn_supply = sum_real_array (NO3gsm_diffn_avail, deepest_layer+1);

            // get actual total nitrogen uptake for diffusion and mass flow.
            // If demand is not satisfied by mass flow, then use diffusion.
            // N uptake above N critical can only happen via mass flow.

   N_demand = sum_real_array (G_n_demand, max_part);
   N_max    = sum_real_array (G_n_max, max_part);

   if (NO3gsm_mflow_supply >= N_demand)
      {
      NO3gsm_mflow = NO3gsm_mflow_supply;
      NO3gsm_mflow = u_bound (NO3gsm_mflow, N_max);
      NO3gsm_diffn = 0.0;
      }
   else
      {
      NO3gsm_mflow = NO3gsm_mflow_supply;

      if (strcmp(C_n_supply_preference,"active") == 0)
         {
         temp1 = N_demand - NO3gsm_mflow;
         NO3gsm_diffn = bound (temp1, 0.0, NO3gsm_diffn_supply);
         }
      else if (strcmp(C_n_supply_preference,"fixation") == 0)
         {
         temp1 = N_demand - NO3gsm_mflow - G_n_fix_pot;
         NO3gsm_diffn = bound (temp1, 0.0, NO3gsm_diffn_supply);
         }
      else
         {
         fatal_error (&err_user, "bad n supply preference");
         }

      NO3gsm_diffn = divide (NO3gsm_diffn, C_no3_diffn_const, 0.0);

      }

   // get actual change in N contents
   fill_real_array (dlt_no3gsm, 0.0, max_layer);
   for(layer = 0; layer <= deepest_layer; layer++)
      {
               // allocate nitrate
               // Find proportion of nitrate uptake to be taken from layer
               // by diffusion and mass flow

      mflow_fract = divide(G_no3gsm_mflow_avail[layer],NO3gsm_mflow_supply, 0.0);

      diffn_fract = divide(NO3gsm_diffn_avail[layer], NO3gsm_diffn_supply, 0.0);

               // now find how much nitrate the plant removes from
               // the layer by both processes

      NO3gsm_uptake = NO3gsm_mflow * mflow_fract + NO3gsm_diffn * diffn_fract;
      dlt_no3gsm[layer] = -1 * NO3gsm_uptake;
      }
   }


// ====================================================================
void cproc_n_supply1 (float *G_dlayer,                   // (INPUT)
                      int    max_layer,               // (INPUT)
                      float *G_dlt_sw_dep,               // (INPUT)
                      float *G_NO3gsm,                   // (INPUT)
                      float *G_NO3gsm_min,               // (INPUT)
                      float G_root_depth,                // (INPUT)
                      float *G_sw_dep,                   // (INPUT)
                      float *G_NO3gsm_mflow_avail,       // (OUTPUT)
                      float *G_sw_avail,                 // (INPUT) 
                      float *G_NO3gsm_diffn_pot,         // (OUTPUT)
                      float G_current_stage,             // (INPUT) 
                      float *C_n_fix_rate,               // (INPUT) 
                      float fixation_determinant,       // (INPUT) 
                      float G_swdef_fixation,           // (INPUT) 
                      float *G_N_fix_pot)                // (INPUT) 
//=========================================================================
/*  Purpose
*      Calculate nitrogen supplies from soil and fixation
*
*  Mission Statement
*   Calculate crop Nitrogen supplies (soil + fixation)
*
*  Changes
*     21-04-1998 - neilh - Programmed and Specified
*/
   {
   crop_n_mass_flow1(max_layer, G_dlayer, G_dlt_sw_dep, G_NO3gsm, G_NO3gsm_min,
            G_root_depth, G_sw_dep, G_NO3gsm_mflow_avail);

   crop_n_diffusion1(max_layer, G_dlayer, G_NO3gsm, G_NO3gsm_min, G_root_depth,
            G_sw_avail, G_sw_dep, G_NO3gsm_diffn_pot);

   // determine N from fixation
   crop_n_fixation_pot1(G_current_stage, C_n_fix_rate, fixation_determinant,
            G_swdef_fixation, G_N_fix_pot);
   }

//=============================================================================
void crop_n_mass_flow1(const int num_layer,          // (INPUT)  number of layers in profile           
                       float *dlayer,                // (INPUT)  thickness of soil layer I (mm)        
                       float *dlt_sw_dep,            // (INPUT)  water uptake in each layer (mm water) 
                       float *no3gsm,                // (INPUT)  nitrate nitrogen in layer L (g N/m^2) 
                       float *no3gsm_min,            // (INPUT)  minimum allowable NO3 in soil (g/m^2) 
                       float root_depth,             // (INPUT)  depth of roots (mm)                   
                       float *sw_dep,                // (INPUT)  soil water content of layer L (mm)    
                       float *NO3gsm_mflow_pot)      // (OUTPUT) potential plant NO3 uptake (supply) g/m^2 by mass flow                 
//=============================================================================

/*  Purpose
*       Return potential nitrogen uptake (supply) by mass flow (water
*       uptake) (g/m^2)
*
*  Mission Statement
*   Calculate crop nitrogen supply from mass flow, %8.
*
*  Changes
*      090994 jngh specified and programmed
*      970216 slw generalised to avoid common blocks , added num_layer parameter
*
*/
   {
   //  Local Variables
   int deepest_layer;            // deepest layer in which the roots are growing
   int layer;                    // layer number of soil
   float NO3_conc;               // nitrogen concentration (g/m^2/mm)
   float NO3gsm_mflow;           // potential nitrogen uptake (g/m^2)

   float temp1;                  //Vars to pass composite terms to functions
   float temp2;                  //expecting a pointer

   fill_real_array (NO3gsm_mflow_pot, 0.0, num_layer);

   // only take the layers in which roots occur
   deepest_layer = find_layer_no(root_depth, dlayer, num_layer);
   for(layer = 0; layer <= deepest_layer; layer++)
      {
      // get  NO3 concentration
      NO3_conc = divide(no3gsm[layer], sw_dep[layer], 0.0);
      // get potential uptake by mass flow
      NO3gsm_mflow = NO3_conc * (-1 * dlt_sw_dep[layer]);
      temp1 = no3gsm[layer] - no3gsm_min[layer];
      NO3gsm_mflow_pot[layer] = u_bound (NO3gsm_mflow, temp1);
      }
   }


//=============================================================================
void crop_n_diffusion1 (const int num_layer,      // (INPUT)  number of layers in profile           
                        float *dlayer,            // (INPUT)  thickness of soil layer I (mm)        
                        float *no3gsm,            // (INPUT)  nitrate nitrogen in layer L (g N/m^   
                        float *no3gsm_min,        // (INPUT)  minimum allowable NO3 in soil (g/m^   
                        float root_depth,         // (INPUT)  depth of roots (mm)                   
                        float *sw_avail,          // (INPUT)  actual extractable soil water (mm)    
                        float *sw_avail_pot,      // (INPUT)  potential extractable soil water (m   
                        float *NO3gsm_diffn_pot)  // (OUTPUT) potential plant NO3 by diffusion      
//=============================================================================

/*  Purpose
*       Return potential nitrogen uptake (supply) by diffusion
*       for a plant (g/m^2)
*
*  Mission Statement
*   Calculate crop nitrogen supply from active uptake, %8.
*
*  Changes
*      060495 nih taken from template
*      160297 slw generalised to avoid common blocks , added num_layer parameter
*
*/
   {
   //  Local Variables
   int deepest_layer;            // deepest layer in which the roots are growing
   int layer;                    // layer number of soil
   float NO3gsm_diffn;           // potential nitrogen uptake (g/m^2)
   float sw_avail_fract;         // fraction of extractable soil water ()

   float temp1;                  //Vars to pass composite terms to functions
   float temp2;                  //expecting a pointer

   //- Implementation Section ----------------------------------
   // only take the layers in which roots occur
   fill_real_array(NO3gsm_diffn_pot, 0.0, num_layer);

   deepest_layer = find_layer_no(root_depth, dlayer, num_layer);
   for(layer = 0; layer <= deepest_layer; layer++)
      {
      sw_avail_fract = divide(sw_avail[layer], sw_avail_pot[layer], 0.0);
      sw_avail_fract = bound(sw_avail_fract, 0.0, 1.0);

      // get extractable NO3
      // restricts NO3 available for diffusion to NO3 in plant
      // available water range
      NO3gsm_diffn = sw_avail_fract * no3gsm[layer];
      temp2 = no3gsm[layer] - no3gsm_min[layer];
      NO3gsm_diffn_pot[layer] = u_bound(NO3gsm_diffn, temp2);
      }
   }

//===========================================================================
void crop_n_fixation_pot1( float G_current_stage,              // (INPUT) Current stage                     
                           float *C_n_fix_rate,                // (INPUT)  potential rate of N fixation (   
                           float fixation_determinant,         // (INPUT)                                   
                           float G_swdef_fixation,             // (INPUT)                                   
                           float *N_fix_pot)                   // (OUTPUT) N fixation potential (g/         
//===========================================================================

/*  Purpose
*          Calculate the quantity of atmospheric nitrogen fixed
*          per unit standing crop biomass (fixation_determinant) and
*          limited by the soil water deficit factor for fixation.
*
*  Mission Statement
*   Calculate crop nitrogen supply from fixation, %5.
*
*/
   {
   //  Local Variables
   int current_phase;                     // guess

   //- Implementation Section ----------------------------------
   current_phase = int(G_current_stage);

   *N_fix_pot = C_n_fix_rate[current_phase-1] * fixation_determinant * G_swdef_fixation;
   return;
   }

//============================================================================
void cproc_n_demand1(const int max_part,          // (INPUT)
                     int   *demand_parts,         // (INPUT)
                     const int num_demand_parts,  // (INPUT)
                     float G_dlt_dm,              // (INPUT)  the daily biomass production (
                     float *G_dlt_dm_green,        // (INPUT)  plant biomass growth (g/m^2)
                     float G_dlt_dm_pot_rue,      // (INPUT)  potential dry matter productio
                     float *G_dlt_n_retrans,       // (INPUT)  nitrogen retranslocated out fr
                     float *G_dm_green,            // (INPUT)  live plant dry weight (biomass
                     float *G_n_conc_crit,         // (INPUT)  critical N concentration (g N/
                     float *G_n_conc_max,          // (INPUT)  maximum N concentration (g N/g    
                     float *G_n_green,             // (INPUT)  plant nitrogen content (g N/m^    
                     float *N_demand,             // (OUTPUT) critical plant nitrogen demand g/m^2)     
                     float *N_max)                // (OUTPUT) max plant nitrogen demand  (g/m^2)       
//============================================================================
                                                                                      
/*  Purpose
*       Return plant nitrogen demand for each plant component
*
*  Mission Statement
*   Calculate the Nitrogen demand and maximum uptake for each plant pool
*
*  Notes
*           Nitrogen required for grain growth has already been removed
*           from the stover.  Thus the total N demand is the sum of the
*           demands of the stover and roots.  Stover N demand consists of
*           two components:
*           Firstly, the demand for nitrogen by the potential new growth.
*           Secondly, the demand due to the difference between
*           the actual N concentration and the critical N concentration
*           of the tops (stover), which can be positive or negative
*
*           NOTE that this routine will not work if the root:shoot ratio
*           is broken. - NIH
*
*  Changes
*     010994 jngh specified and programmed
*     210498 nih  adapted to crop template specifications
*
*/
   {
   //  Local Variables
   int counter;
   float N_crit;                 // critical N amount (g/m^2)
   float N_demand_new ;          // demand for N by new growth
                                 // (g/m^2)
   float N_demand_old;           // demand for N by old biomass
                                 // (g/m^2)
   float N_potential;            // maximum N uptake potential (g/m^2)
   float N_max_new;              // N required by new growth to reach
                                 // N_conc_max  (g/m^2)
   float N_max_old;              // N required by old biomass to reach
                                 // N_conc_max  (g/m^2)
   int part;                     // plant part
   float dlt_dm_pot;             // potential dry weight increase
                                 // (g/m^2)
   float part_fract;             // plant part fraction of dm  (0-1)

   //- Implementation Section ----------------------------------
   fill_real_array (N_demand, 0.0, max_part);
   fill_real_array (N_max, 0.0, max_part);
   for(counter = 0; counter < num_demand_parts; counter++)
      {
      part = demand_parts[counter];

            // need to calculate dm using potential rue not affected by
            // N and temperature

      part_fract = divide (G_dlt_dm_green[part], G_dlt_dm, 0.0);
      dlt_dm_pot = G_dlt_dm_pot_rue * part_fract;
      dlt_dm_pot = bound (dlt_dm_pot, 0.0, G_dlt_dm_pot_rue);

      if (G_dm_green[part] > 0.0)
         {
         // get N demands due to difference between actual N concentrations
         // and critical N concentrations of tops (stover) and roots.
         N_crit       = G_dm_green[part] * G_n_conc_crit[part];
         N_potential  = G_dm_green[part] * G_n_conc_max[part];

         // retranslocation is -ve for outflows
         N_demand_old = N_crit -
                  (G_n_green[part] + G_dlt_n_retrans[part]);
         N_max_old    = N_potential -
                  (G_n_green[part] + G_dlt_n_retrans[part]);

         // get potential N demand (critical N) of potential growth
         N_demand_new = dlt_dm_pot * G_n_conc_crit[part];
         N_max_new    = dlt_dm_pot * G_n_conc_max[part];

         N_demand[part] = N_demand_old + N_demand_new;
         N_max[part]    = N_max_old    + N_max_new ;

         N_demand[part] = l_bound (N_demand[part], 0.0);
         N_max[part]    = l_bound (N_max[part], 0.0);
         }
      else
         {
         N_demand[part] = 0.0;
         N_max[part]    = 0.0;
         }
      }
      // this routine does not allow excess N in one component to move
      // to another component deficient in N
   }


//==========================================================================
void cproc_n_init1(float *C_n_init_conc,  // (INPUT)  initial N concentration (
                   int    max_part, 
                   int    init_stage, 
                   float G_current_stage, // (INPUT)  current phenological stage    
                   float *G_days_tot,     // (INPUT)  duration of each phase (days) 
                   float *G_dm_green,     // (INPUT)  live plant dry weight (biomass
                   float *N_green)        // plant nitrogen (g/m^2)                 
//===========================================================================

/*  Purpose
*   Initialise plant Nitrogen pools
*
*  Mission Statement
*   Initialise plant Nitrogen pools (on first day of %3)
*
*  Changes
*     210498 nih specified and programmed
*
*/
   {
   //  Local Variables
   int part;

   //- Implementation Section ----------------------------------
   if (on_day_of (init_stage, G_current_stage))
      {
      for(part = 0; part < max_part; part++)
         {
         N_green[part] = C_n_init_conc[part] * G_dm_green[part];
         }
      }
   }


//==========================================================================
void cproc_n_detachment1(const int max_part,
                         float *c_sen_detach_frac, 
                         float *g_n_senesced, 
                         float *g_dlt_n_detached,
                         float *c_dead_detach_frac, 
                         float *g_n_dead, 
                         float *g_dlt_n_dead_detached)
//==========================================================================

/*  Purpose
*       Simulate plant Nitrogen detachment.
*
*  Mission Statement
*       Calculate plant Nitrogen detachment from senesced and dead pools
*
*  Changes
*      220498 nih specified and programmed
*/
   {
   //- Implementation Section ----------------------------------
   crop_pool_fraction_delta(max_part, c_sen_detach_frac ,g_n_senesced,
            g_dlt_n_detached);

   crop_pool_fraction_delta(max_part, c_dead_detach_frac, g_n_dead,
            g_dlt_n_dead_detached);
   }


//=========================================================================
void cproc_n_supply2 (float *g_dlayer,                // (INPUT) 
                      const int max_layer,            // (INPUT) 
                      float *g_dlt_sw_dep,            // (INPUT) 
                      float *g_NO3gsm,                // (INPUT) 
                      float *g_NO3gsm_min,            // (INPUT) 
                      float g_root_depth,             // (INPUT) 
                      float *g_sw_dep,                // (INPUT) 
                      float *g_NO3gsm_mflow_avail,    // (OUTPUT)
                      float *g_sw_avail,              // (INPUT) 
                      float *g_sw_avail_pot,          // (INPUT) 
                      float *g_NO3gsm_diffn_pot,      // (OUTPUT)
                      float g_current_stage,          // (INPUT) 
                      float *c_n_fix_rate,             // (INPUT) 
                      float fixation_determinant,     // (INPUT) 
                      float g_swdef_fixation,        // (INPUT) 
                      float *g_N_fix_pot)             // (INPUT) 
//=========================================================================

/*  Purpose
*      Calculate nitrogen supplys from soil and fixation
*
*  Mission Statement
*      Calculate nitrogen supplys from soil and fixation
*
*  Changes
*     21-04-1998 - neilh - Programmed and Specified
*
*/
   {
   //- Implementation Section ----------------------------------
   crop_n_mass_flow1  (max_layer, g_dlayer, g_dlt_sw_dep, g_NO3gsm, g_NO3gsm_min,
            g_root_depth, g_sw_dep, g_NO3gsm_mflow_avail);

   crop_n_diffusion1 (max_layer, g_dlayer, g_NO3gsm, g_NO3gsm_min, g_root_depth,
            g_sw_avail, g_sw_avail_pot, g_NO3gsm_diffn_pot);

   // determine N from fixation
   crop_n_fixation_pot1(g_current_stage, c_n_fix_rate, fixation_determinant,
         g_swdef_fixation, g_N_fix_pot);
   }

#ifdef NOTYET
//=========================================================================
extern "C" void _stdcall _export cproc_n_demand2(const int *max_part, int *demand_parts,
      const int *num_demand_parts, float *G_dlt_dm_green, float *G_dlt_n_retrans,
      float *G_dm_green, float *G_n_conc_crit, float *G_n_conc_max, float *G_n_green,
      float *N_demand, float *N_max)
//=========================================================================

/*  Purpose
*       Return plant nitrogen demand for each plant component
*
*  Mission Statement
*       Calculate nitrogen demand and maximum uptake for each plant pool
*
*  Notes
*           Nitrogen required for grain growth has already been removed
*           from the stover.  Thus the total N demand is the sum of the
*           demands of the stover and roots.  Stover N demand consists of
*           two components:
*           Firstly, the demand for nitrogen by the new growth.
*           Secondly, the demand due to the difference between
*           the actual N concentration and the critical N concentration
*           of the tops (stover), which can be positive or negative
*
*           NOTE that this routine will not work if the root:shoot ratio
*           is broken. - NIH
*
*  Changes
*     010994 jngh specified and programmed
*     210498 nih  adapted to crop template specifications
*
*  Sub-Program Arguments
*      int *max_part                   // (INPUT)
*      int *demand_parts (*)           // (INPUT)
*      int *num_demand_parts           // (INPUT)
*      float *G_dlt_dm_green(*)        // (INPUT)  plant biomass growth (g/m^2)
*      float *G_dlt_n_retrans(*)       // (INPUT)  nitrogen retranslocated out fr
*      float *G_dm_green(*)            // (INPUT)  live plant dry weight (biomass
*      float *G_n_conc_crit(*)         // (INPUT)  critical N concentration (g N/
*      float *G_n_conc_max(*)          // (INPUT)  maximum N concentration (g N/g
*      float *G_n_green(*)             // (INPUT)  plant nitrogen content (g N/m^
*      float *N_demand (*)             // (OUTPUT) critical plant nitrogen demand
*                                      // (g/m^2)
*      float *N_max (*)                // (OUTPUT) max plant nitrogen demand
*                                      // (g/m^2)
*/
   {
   // Local Variables
   int counter;
   float N_crit;                 // critical N amount (g/m^2)
   float N_demand_new;           // demand for N by new growth
                                 // (g/m^2)
   float N_demand_old;           // demand for N by old biomass
                                 // (g/m^2)
   float N_potential;            // maximum N uptake potential (g/m^2)
   float N_max_new;              // N required by new growth to reach
                                 // N_conc_max  (g/m^2)
   float N_max_old;              // N required by old biomass to reach
                                 // N_conc_max  (g/m^2)
   int part;                     // plant part
   float part_fract;             // plant part fraction of dm  (0-1)
   float zero = 0.0;

   //- Implementation Section ----------------------------------
   fill_real_array (N_demand, &zero, max_part);
   fill_real_array (N_max, &zero, max_part);

   for(counter = 0; counter < *num_demand_parts; counter ++)
      {
      part = demand_parts[counter];

            // need to calculate dm using potential rue not affected by
            // N and temperature

      if (G_dm_green[part - 1] > 0.0)
         {
               // get N demands due to difference between actual N concentrations
               // and critical N concentrations of tops (stover) and roots.

         N_crit       = G_dm_green[part - 1] * G_n_conc_crit[part - 1];
         N_potential  = G_dm_green[part - 1] * G_n_conc_max[part - 1];

               // retranslocation is -ve for outflows

         N_demand_old = N_crit -
                     (G_n_green[part - 1] + G_dlt_n_retrans[part - 1]);
         N_max_old    = N_potential -
                     (G_n_green[part - 1] + G_dlt_n_retrans[part - 1]);


               // get potential N demand (critical N) of potential growth

         N_demand_new = G_dlt_dm_green[part - 1] * G_n_conc_crit[part - 1];
         N_max_new    = G_dlt_dm_green[part - 1] * G_n_conc_max[part - 1];

         N_demand[part - 1] = N_demand_old + N_demand_new;
         N_max[part - 1]    = N_max_old    + N_max_new;

         N_demand[part - 1] = l_bound (N_demand[part - 1], 0.0);
         N_max[part - 1]    = l_bound (N_max[part - 1], 0.0);
         }
      else
         {
         N_demand[part - 1] = 0.0;
         N_max[part - 1]    = 0.0;
         }
      }
         // this routine does not allow excess N in one component to move
         // to another component deficient in N
   return;
   }
#endif
#ifdef NOTYET
//=========================================================================
extern "C" void _stdcall _export cproc_n_uptake2(float *C_no3_diffn_const, float *G_dlayer,
      const int *max_layer, float *G_no3gsm_diffn_pot, float *G_no3gsm_mflow_avail,
      float *G_N_fix_pot, char *C_n_supply_preference, float *G_n_demand,
      float *G_n_max, int *max_part, float *G_root_depth, float *dlt_NO3gsm,
      float *dlt_NO3gsm_massflow, float *dlt_NO3gsm_diffusion)
//=========================================================================

/*  Purpose
*       Return actual plant nitrogen uptake from
*       each soil layer.
*
*  Mission Statement
*   Calculate crop Nitrogen Uptake
*
*  Changes
*       160498 nih specified and programmed
*       281100 ew  added the arguments for mass flow and diffusion uptake
*
*
*  Constant Values
*      character  my_name*(*)           // name of procedure
*      parameter (my_name = 'cproc_N_uptake2')
*
*
*  Sub-Program Arguments
*      float *C_no3_diffn_const        // (INPUT)  time constant for uptake by di
*      float *G_dlayer(*)              // (INPUT)  thickness of soil layer I (mm)
*      int *max_layer                  // (INPUT)  max number of soil layers
*      float *G_no3gsm_diffn_pot(*)    // (INPUT)  potential NO3 (supply) from so
*      float *G_no3gsm_mflow_avail(*)  // (INPUT)  potential NO3 (supply) from
*      float *G_N_Fix_Pot              // (INPUT) potential N fixation (g/m2)
*      char *C_n_supply_preference*(*) //(INPUT)
*      float *G_n_demand(*)            // (INPUT)  critical plant nitrogen demand
*      int *max_part                   // (INPUT)  number of plant parts
*      float *G_n_max(*)               // (INPUT)  maximum plant nitrogen demand
*      float *G_root_depth             // (INPUT)  depth of roots (mm)
*      float *dlt_NO3gsm(*)            // (OUTPUT) actual plant N uptake
*                                      // from NO3 in each layer (g/m^2)
*      float *dlt_NO3gsm_massflow(*)   //(OUTPUT) actual plant N uptake from mass flow (g/m2)
*      float *dlt_NO3gsm_diffusion(*)  //(OUTPUT) actual plant N uptake from diffusion (g/m2)
*/

   {

   // Local Variables
   int deepest_layer;            // deepest layer in which the roots are
                                 // growing
   float NO3gsm_diffn;           // actual N available (supply) for
                                 // plant (g/m^2) by diffusion
   float NO3gsm_mflow;           // actual N available (supply) for
                                 // plant (g/m^2) by mass flow
   float *NO3gsm_diffn_avail = new float[crop_max_layer]; // potential NO3 (supply)
                                 // from soil (g/m^2), by diffusion
   float NO3gsm_diffn_supply;    // total potential N uptake (supply)
                                 // for plant (g/m^2) by diffusion
   float NO3gsm_mflow_supply;    // total potential N uptake (supply)
                                 // for plant (g/m^2) by mass flow
   float diffn_fract;            // fraction of nitrogen to use (0-1)
                                 // for diffusion
   float mflow_fract;            // fraction of nitrogen to use (0-1)
                                 // for mass flow
   int layer;                    // soil layer number of profile
   float N_demand;               // total nitrogen demand (g/m^2)
   float NO3gsm_uptake;          // plant NO3 uptake from layer (g/m^2)
   float N_max;                  // potential N uptake per plant (g/m^2)
   float zero = 0.0;
   float temp1;                  //Vars to pass composite terms to functions
   float temp2;                  //expecting a pointer
   //- Implementation Section ----------------------------------
            // get potential N uptake (supply) from the root profile.
            // get totals for diffusion and mass flow.

   deepest_layer = find_layer_no (G_root_depth, G_dlayer, max_layer);
   for(layer = 0; layer <= deepest_layer; layer++)
      {
      NO3gsm_diffn_avail[layer] = G_no3gsm_diffn_pot[layer] -
                                    G_no3gsm_mflow_avail[layer];
      NO3gsm_diffn_avail[layer] = l_bound (&NO3gsm_diffn_avail[layer], &zero);
      }

   NO3gsm_mflow_supply = sum_real_array (G_no3gsm_mflow_avail, deepest_layer+1);
   NO3gsm_diffn_supply = sum_real_array (NO3gsm_diffn_avail, deepest_layer+1);

            // get actual total nitrogen uptake for diffusion and mass flow.
            // If demand is not satisfied by mass flow, then use diffusion.
            // N uptake above N critical can only happen via mass flow.

   N_demand = sum_real_array (G_n_demand, max_part);
   N_max    = sum_real_array (G_n_max, max_part);

   if (NO3gsm_mflow_supply >= N_demand)
      {
      NO3gsm_mflow = NO3gsm_mflow_supply;
      NO3gsm_mflow = u_bound (&NO3gsm_mflow, &N_max);
      NO3gsm_diffn = 0.0;
      }
   else
      {
      NO3gsm_mflow = NO3gsm_mflow_supply;

      if (strcmp(C_n_supply_preference,"active") == 0)
         {
         temp1 = N_demand - NO3gsm_mflow;
         NO3gsm_diffn = bound (&temp1, &zero, &NO3gsm_diffn_supply);
         }
      else if (strcmp(C_n_supply_preference,"fixation") == 0)
         {
         temp1 =  N_demand - NO3gsm_mflow - *G_N_fix_pot;
         NO3gsm_diffn = bound (&temp1, &zero, &NO3gsm_diffn_supply);
         }
      else
         {
         fatal_error (&err_user, "bad n supply preference");
         }

      NO3gsm_diffn = divide (NO3gsm_diffn, *C_no3_diffn_const, 0.0);
      }

            // get actual change in N contents

   fill_real_array (dlt_NO3gsm, &zero, max_layer);

   for(layer = 0 ; layer < deepest_layer; layer++)
      {
               // allocate nitrate
               // Find proportion of nitrate uptake to be taken from layer
               // by diffusion and mass flow

      mflow_fract = divide (&G_no3gsm_mflow_avail[layer], &NO3gsm_mflow_supply, &zero);

      diffn_fract = divide (&NO3gsm_diffn_avail[layer], &NO3gsm_diffn_supply, &zero);

               // now find how much nitrate the plant removes from
               // the layer by both processes

      NO3gsm_uptake = NO3gsm_mflow * mflow_fract + NO3gsm_diffn * diffn_fract;
      dlt_NO3gsm[layer] = -1 * NO3gsm_uptake;


      dlt_NO3gsm_massflow [layer] = - 1 * NO3gsm_mflow * mflow_fract;
      dlt_NO3gsm_diffusion[layer] = - 1 * NO3gsm_diffn * diffn_fract;
      }
   return;
   }
#endif
