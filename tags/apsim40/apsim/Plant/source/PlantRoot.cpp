#include <stdio.h>
#include <math.h>
#include <vector>
#include <string>
#include <stdexcept>
#include "PlantComponent.h"
#include "PlantLibrary.h"
#include "Plant.h"

#define dim(A,B) (min(0,(A)-(B)))

//===========================================================================
float root_proportion (int    layer,              // (INPUT) layer to look at         
                       float *dlayr,              // (INPUT) array of layer depths    
                       float  root_depth)         // (INPUT) depth of roots           
//===========================================================================

 /* Purpose
 *       returns the proportion of layer that has roots in it (0-1).
 *
 *  Definition
 *     Each element of "dlayr" holds the height of  the
 *     corresponding soil layer.  The height of the top layer is
 *     held in "dlayr"(1), and the rest follow in sequence down
 *     into the soil profile.  Given a root depth of "root_depth",
 *     this function will return the proportion of "dlayr"("layer")
 *     which has roots in it  (a value in the range 0..1).
 *
 *  Mission Statement
 *      proportion of layer %1 explored by roots
 *
 * Changes
 *     21/5/2003 ad converted to BC++
 *     010994 jngh specified and programmed
 *     230698 jngh corrected to allow for layers below root zone
 *
 */
   {
   // Local Variables
   float depth_to_layer_bottom;   // depth to bottom of layer (mm)
   float depth_to_layer_top;      // depth to top of layer (mm)
   float depth_to_root;           // depth to root in layer (mm)
   float depth_of_root_in_layer;  // depth of root within layer (mm)
   // Implementation Section ----------------------------------
   depth_to_layer_bottom = sum_real_array (dlayr, layer+1);
   depth_to_layer_top = depth_to_layer_bottom - dlayr[layer];
   depth_to_root  = min(depth_to_layer_bottom, root_depth);
   depth_of_root_in_layer = max(0.0, depth_to_root-depth_to_layer_top);

   return (divide (depth_of_root_in_layer, dlayr[layer], 0.0));
   }

//==========================================================================
void  cproc_root_depth_init1(float initial_root_depth,         // (INPUT)  initial depth of roots (mm)       
                             float current_stage,              // (INPUT)  current phenological stage
                             int   initialisation_stage,       // (INPUT)  stage at which to initialise
                             float *days_tot,                   // (INPUT)  duration of each phase (days)
                             float *root_depth)                 // (OUTPUT) initial root depth (mm)
//==========================================================================

/*  Purpose
*       Return the initial root depth (mm)
*
*  Mission Statement
*   Initialise rooting depth (on the first day of %3)
*
*  Changes
*      160498 nih specified and programmed
*
*/
   {
   // Implementation Section ----------------------------------
   if (on_day_of (initialisation_stage, current_stage))
      {
      // initialise root depth
      *root_depth = initial_root_depth;
      }
   else
      {
      // we have no initial root depth today
      }
   }

//============================================================
void cproc_root_depth2 (float G_current_stage,             //  (INPUT) current growth stage
                        float G_maxt,                     //
                        float G_mint,                     //
                        float G_swdef_photo,              //
                        float G_root_depth,               //  (INPUT) root depth (mm)        
                        int   C_num_temp_root,              //                                              
                        float *C_x_temp_root,              //                                              
                        float *C_y_temp_root_fac,          //                                              
                        int   C_num_ws_root,                //                                              
                        float *C_x_ws_root,                //                                              
                        float *C_y_ws_root_fac,            //                                              
                        int   C_num_sw_ratio,               //! (INPUT) number of sw lookup pairs           
                        float *C_x_sw_ratio,               //  (INPUT) sw factor lookup x                  
                        float *C_y_sw_fac_root,            //  (INPUT) sw factor lookup y                  
                        float *G_dlayer,                   //  (INPUT)  layer thicknesses (mm)             
                        float *G_dul_dep,                  //  (INPUT) DUL (mm)                            
                        float *G_sw_dep,                   //  (INPUT) SW (mm)                             
                        float *P_ll_dep,                   //  (INPUT) LL (mm)                             
                        float *C_root_depth_rate,          //  (INPUT) root front velocity (mm)            
                        float *P_xf,                       //  (INPUT) exploration factor                  
                        float *G_dlt_root_depth)           //  (OUTPUT) increase in rooting depth (mm    
//==========================================================================

/*  Purpose
*       Calculate plant rooting depth through time
*
*  Mission Statement
*   Calculate today's rooting depth
*
*  Changes
*     20001128 ew  specified and programmed
*
*/
   {
   //  Local Variables
   int deepest_layer;                  // deepest layer in which the roots are
                                       // growing
   float temp;
   float temp_factor;
   float ws_factor;
   float sw_avail_factor;

   // Implementation Section ----------------------------------
   //Temperature factor
   temp  = 0.5 *(G_maxt + G_mint);

   temp_factor = linear_interp_real (temp, C_x_temp_root, C_y_temp_root_fac,
                                     C_num_temp_root);

   //Water stress factor
   ws_factor = linear_interp_real (G_swdef_photo, C_x_ws_root, C_y_ws_root_fac,
                                   C_num_ws_root);

   //Soil water availability factor
   crop_root_sw_avail_factor(C_num_sw_ratio, C_x_sw_ratio, C_y_sw_fac_root,
                             G_dlayer, G_dul_dep, G_sw_dep, P_ll_dep, G_root_depth, &sw_avail_factor);

   crop_root_depth_increase2(G_current_stage, C_root_depth_rate, G_dlayer,
                             G_root_depth, temp_factor, ws_factor, sw_avail_factor, P_xf,
                             G_dlt_root_depth);
   }
//=========================================================================
void crop_root_sw_avail_factor(int  num_sw_ratio,                 // (INPUT)                                                                    
                               float *x_sw_ratio,                 // (INPUT)                                                                    
                               float *y_sw_fac_root,              // (INPUT)                                                                    
                               float *dlayer,                     // (INPUT) layer depth (mm)
                               float *dul_dep,                    // (INPUT) drained upper limit for layer L (mm water)                         
                               float *sw_dep,                     // (INPUT) soil water content of layer L (mm)                                 
                               float *ll_dep,                     // (INPUT) lower limit of plant-extractable soil water for soil layer L (mm)  
                               float  root_depth,                 //                                                                            
                               float *sw_avail_factor)            // (OUTPUT) sw availability factor for root depth growth                      
//=========================================================================

/*  Purpose
*      Get the soil water availability factor for root depth growth. A weighted factor is used.
*      it is 1.0 unless the plant-extractable soil water declines below a fraction of
*      plant-extractable soil water capacity for the two adjacent layers
*
*  Mission Statement
*   Determine root hospitality factor for moisture (for layer %7)
*
*/
   {
   //  Local Variables
   int deepest_layer;
   int layer;
   int next_layer;
   float cum_depth;
   float rootdepth_in_layer;
   float weighting_factor;
   float fasw1;
   float fasw2;
   float fasw;

   // Implementation Section ----------------------------------

   //Total soil layers
   deepest_layer = count_of_real_vals (dlayer, max_layer);

   //the layer with root front
   layer = find_layer_no (root_depth, dlayer, max_layer);

   cum_depth = sum_real_array (dlayer, layer+1);

   rootdepth_in_layer = dlayer[layer] - (cum_depth - root_depth);

   rootdepth_in_layer = bound (rootdepth_in_layer, 0.0, dlayer[layer]);

   weighting_factor = divide (rootdepth_in_layer, dlayer[layer], 0.0);

   next_layer = min(layer+1, deepest_layer);

   fasw1 = divide (sw_dep [layer] - ll_dep[layer], 
                   dul_dep[layer] - ll_dep[layer], 0.0);

   fasw2 = divide (sw_dep [next_layer] - ll_dep[next_layer],
                   dul_dep[next_layer] - ll_dep[next_layer], 0.0);

   fasw1 = min(1.0,max(0.0, fasw1));
   fasw2 = min(1.0,max(0.0, fasw2));

   fasw = weighting_factor * fasw2 + (1.0 - weighting_factor) * fasw1;

   *sw_avail_factor = linear_interp_real (fasw, x_sw_ratio, y_sw_fac_root, num_sw_ratio);
   }

//============================================================================
void crop_root_depth_increase2(float current_stage,             //(INPUT)  current phenological stage
                               float *c_root_depth_rate,         //(INPUT)  root growth rate potential (mm        
                               float *dlayer,                    //(INPUT)  thickness of soil layer I (mm)        
                               float root_depth,                //(INPUT)  depth of roots (mm)                   
                               float temp_factor,               //(INPUT)  depth of roots (mm)                   
                               float ws_factor,                 //(INPUT)  depth of roots (mm)                   
                               float sw_avail_factor,           //(INPUT)                                        
                               float *p_xf,                      //(INPUT) eXploration Factor (0-1)               
                               float *dlt_root_depth)            //(OUTPUT) increase in root depth (mm)           
//============================================================================

/*  Purpose
*       Return the increase in root depth (mm)
*
*  Mission Statement
*   Calculate the increase in rooting depth.
*
*
*  Changes
*     20001128 ew  specified and programmed
*/

   {
   //Local Variables
   int stage_no;                 //current phenological stage
   float root_depth_max;         //maximum depth to which roots can
   int current_layer;            //layer of root front
   int deepest_layer;            //deepest layer for rooting

   current_layer = find_layer_no(root_depth,dlayer, max_layer);

   stage_no = int (current_stage);   //used as index

   // this equation allows soil water in the deepest
   // layer in which roots are growing
   // to affect the daily increase in rooting depth.

   *dlt_root_depth  = c_root_depth_rate[stage_no-1]   // XX should be lookup against stage_no (OK for now)
                        * temp_factor
                        * min(ws_factor, sw_avail_factor)
                        * p_xf[current_layer];

   // constrain it by the maximum
   // depth that roots are allowed to grow.
   deepest_layer = count_of_real_vals (p_xf, max_layer);

   root_depth_max = sum_real_array (dlayer, deepest_layer+1);

   *dlt_root_depth = u_bound(*dlt_root_depth, root_depth_max - root_depth);
   }
//============================================================================
void cproc_root_length_growth1(float  C_specific_root_length,      //   (INPUT) length of root per unit wt (mm                    
                               float *G_dlayer,                    //   (INPUT)  thickness of soil layer I (mm)                   
                               float  G_dlt_root_wt,               //   (INPUT)  plant root biomass growth (g/m                   
                               float *G_dlt_root_length,           //   (OUTPUT) increase in root length (mm/mm                   
                               float  G_dlt_root_depth,            //   (INPUT)  increase in root depth (mm)                      
                               float  G_root_depth,                //   (INPUT)  depth of roots (mm)                              
                               float *G_root_length,               //   (INPUT)                                                   
                               float  G_plants,                    //   (INPUT)                                                   
                               float *P_xf,                        //   (INPUT)  eXtension rate Factor (0-1)                      
                               int    C_num_sw_ratio,              //   (INPUT)                                                   
                               float *C_x_sw_ratio,                //   (INPUT)                                                   
                               float *C_y_sw_fac_root,             //   (INPUT)                                                   
                               float *C_x_plant_rld,               //   (INPUT)                                                   
                               float *C_y_rel_root_rate,           //   (INPUT)                                                   
                               int    C_num_plant_rld,             //   (INPUT)                                                   
                               float *G_dul_dep,                   //   (INPUT)  drained upper limit soil water                   
                               float *G_sw_dep,                    //   (INPUT)  soil water content of layer L                    
                               float *P_ll_dep,                    //   (INPUT)  lower limit of plant-extractab                   
                               int    max_layer)                   //   (INPUT)  maximum number of soil laye                         
//============================================================================

/*  Purpose
*   Calculate the increase in root length density in each rooted
*   layer based upon soil hospitality, moisture and fraction of
*   layer explored by roots.
*
*  Mission Statement
*   Calculate the root length growth for each layer
*
*  Changes
*   neilh - 13-06-1995 - Programmed and Specified
*   neilh - 28-02-1997 - Made root factor constraint
*
*  Calls
*
*/
   {
   //  Local Variables
   int deepest_layer;         // deepest rooted later
   float dlt_length_tot;      // total root length increase (mm/m^2)
   int layer;                 // simple layer counter variable
   float rlv_factor_tot;      // total rooting factors across profile
   float branching_factor;    //
   float plant_rld;
   float rld;
   float temp;

   // Implementation Section ----------------------------------
   if (max_layer <= 0) { throw std::invalid_argument("max_layer is 0 in crop_root_length_growth1"); }
   
   float *rlv_factor = new float[max_layer];  // relative rooting factor for a layer

   fill_real_array (G_dlt_root_length, 0.0, max_layer);

   temp = G_root_depth + G_dlt_root_depth;
   deepest_layer = find_layer_no (temp, G_dlayer, max_layer);
   rlv_factor_tot = 0.0;
   for(layer = 0; layer <= deepest_layer; layer++)
      {
      rld = divide (G_root_length[layer], G_dlayer[layer], 0.0);

      plant_rld = divide (rld, G_plants, 0.0);

      branching_factor = linear_interp_real(plant_rld, C_x_plant_rld,
                                            C_y_rel_root_rate, C_num_plant_rld);

      rlv_factor[layer] = crop_sw_avail_fac(C_num_sw_ratio, C_x_sw_ratio,
                                            C_y_sw_fac_root, G_dul_dep, 
                                            G_sw_dep, P_ll_dep, layer) 
               * branching_factor             // branching factor
               * P_xf [layer]                 // growth factor
               * divide(G_dlayer[layer]       // space weighting
                      ,G_root_depth           //       factor
                      ,0.0);

      rlv_factor[layer] = l_bound(rlv_factor[layer], 1e-6);
      rlv_factor_tot = rlv_factor_tot + rlv_factor[layer];
      }
   dlt_length_tot = G_dlt_root_wt/sm2smm * C_specific_root_length;
   for(layer = 0; layer <= deepest_layer; layer++)
      {
      G_dlt_root_length [layer] = dlt_length_tot *
                  divide (rlv_factor[layer], rlv_factor_tot, 0.0);
      }

   //Cleanup
   delete [] rlv_factor;
}

float crop_sw_avail_fac(int   num_sw_ratio,             //(INPUT)                                                  
                        float *x_sw_ratio,              //(INPUT)                                                  
                        float *y_sw_fac_root,           //(INPUT)                                                  
                        float *dul_dep,                 //(INPUT) drained upper limit for layer L (mm water)       
                        float *sw_dep,                  //(INPUT) soil water content of layer L (mm)               
                        float *ll_dep,                  //(INPUT) lower limit of plant-extractable soil            
                        int   layer)                    //(INPUT) soil profile layer number                        
//===========================================================================

/*  Purpose
*      Get the soil water availability factor in a layer.  For a layer,
*      it is 1.0 unless the plant-extractable soil water declines
*      below a fraction of plant-extractable soil water capacity for
*      that layer.
*
*  Mission Statement
*   Determine root hospitality factor for moisture (for layer %7)
*
*  Changes
*     010994 jngh specified and programmed
*     970216 slw generalised to avoid common blocks
*     19/5/2003 ad converted to BC++
*
*/

   {
   //  Local Variables
   float pesw;                // plant extractable soil-water (mm/mm)
   float pesw_capacity;       // plant extractable soil-water capacity (mm/mm)
   float sw_avail_ratio;      // soil water availability ratio (0-1)

   // Implementation Section ----------------------------------
   pesw = sw_dep[layer] - ll_dep[layer];
   pesw_capacity = dul_dep[layer] - ll_dep[layer];
   sw_avail_ratio = divide (pesw, pesw_capacity, 10.0);
   float sw_avail = linear_interp_real (sw_avail_ratio, x_sw_ratio,
                                         y_sw_fac_root, num_sw_ratio);
   return sw_avail;
   }

//===========================================================================
void cproc_root_length_init1 (float root_wt,
                              float c_specific_root_length, 
                              float g_root_depth, 
                              float *g_dlayer,
                              float *g_root_length, 
                              int   max_layer)
//============================================================================

/*  Purpose
*     Initialise crop root length at emergence based on root weight
*     at emergence and specific root length.
*
*  Mission Statement
*   Initialise crop root length (on first day of %1)
*
*  Changes
*     02-05-1997 - huth - Programmed and Specified
*     02-11-1998 - wang - corrected the first argument of root_proportion
*
*/
   {
   //  Local Variables
   float initial_root_length;       // initial root length (mm/mm^2)
   float rld;                       // initial root length density (mm/mm^3)
   int deepest_layer;             // number of layers with roots
   int layer;                       // simple layer counter variable

      initial_root_length = root_wt / sm2smm * c_specific_root_length;
      rld = divide (initial_root_length, g_root_depth, 0.0);
      deepest_layer = find_layer_no (g_root_depth, g_dlayer, max_layer);

      fill_real_array (g_root_length, 0.0, max_layer);

      for(layer = 0; layer <= deepest_layer; layer++)
         {
         g_root_length[layer] = rld * g_dlayer[layer] *
                  root_proportion (layer, g_dlayer, g_root_depth);
         }
      }
   


//========================================================================
void crop_root_dist(float *G_dlayer,          //(INPUT)  thickness of soil layer I (mm)
                    float *G_root_length,     //(INPUT)                                
                    float G_root_depth,       //(INPUT)  depth of roots (mm)           
                    float *root_array,        //(OUTPUT) array to contain distributed material             
                    float root_sum,           //(INPUT) Material to be distributed                   
                    int   max_layer)          // (INPUT) max number of soil layers
//=========================================================================
/*  Purpose
*       Distribute root material over profile based upon root
*       length distribution.
*
*  Mission Statement
*   Distribute %5 over the profile according to root distribution
*
*  Changes
*     <insert here>
*/
   {
   //  Local Variables
   int layer;                    // layer number ()
   int deepest_layer;            // deepest layer in which the
                                    // roots are growing
   float root_length_sum;        // sum of root distribution array
   // Implementation Section ----------------------------------

   // distribute roots over profile to root_depth

   fill_real_array (root_array, 0.0, max_layer);

   deepest_layer = find_layer_no (G_root_depth, G_dlayer, max_layer);

   root_length_sum = sum_real_array (G_root_length, deepest_layer+1);

   for (layer = 0; layer <= deepest_layer; layer++)
      {
      root_array[layer] = root_sum * divide (G_root_length[layer],
                                             root_length_sum, 0.0);
      }
   }
//=============================================================================
void cproc_root_length_senescence1(float  C_specific_root_length,    //(INPUT)  length of root per unit wt (m         
                                   float *G_dlayer,                  //(INPUT)  thickness of soil layer I (mm)        
                                   float  G_dlt_root_dm_senesced,    //(INPUT)  plant biomass senescence  (g/m^2)
                                   float *G_root_length,             //(INPUT)                                                           
                                   float  G_root_depth,              //(INPUT)  depth of roots (mm)
                                   float *G_dlt_root_length_senesced,//(OUTPUT) root length lost from each layer (mm/mm^2)   
                                    int   max_layer)                 // (INPUT)  maximum layer number 
//=============================================================================

/*  Purpose
*     Calculate root length senescence based upon changes in senesced root
*     biomass and the specific root length.
*
*  Mission Statement
*     Calculate root length senescence
*
*  Notes
*   nih - I know there is a simpler way of doing this but if we make the
*         calculation of senescence rate more complex this aproach will
*         automatically handle it.
*
*  Changes
*   neilh - 14-06-1995 - Programmed and Specified
*
*/
   {
   //  Local Variables
   float senesced_length;           // length of root to senesce (mm/m2)
   // Implementation Section ----------------------------------
   fill_real_array (G_dlt_root_length_senesced, 0.0, max_layer);

   senesced_length = G_dlt_root_dm_senesced / sm2smm * C_specific_root_length;

   crop_root_dist(G_dlayer,G_root_length, G_root_depth, G_dlt_root_length_senesced,
                  senesced_length, max_layer);
   }

//==========================================================================
void crop_root_redistribute (float *root_length,       //  root length (mm/mm^2)                 
                             float  root_depth_old,    //  old root depth (mm)
                             float *dlayer_old,        //  old soil profile layers (mm)          
                             int    nlayr_old,         //  number of old soil profile layers     
                             float root_depth_new,     //  new root depth (mm)                   
                             float *dlayer_new,        //  new soil profile layers (mm)          
                             int   nlayr_new)          //  number of new soil profile layers     
//==========================================================================

/*  Purpose
*      Map root length density distribution into a new layer structure
*      after reduction is profile depth due to erosion.
*
*  Mission Statement
*   Redistribute root profile according to changes in layer structure.
*
*  Assumptions
*      That the new profile is shallower and the roots are at the
*      bottom of the old profile.
*
*  Notes
*      Remapping is achieved by first constructing a map of
*      cumulative root length vs depth that is 'squashed'
*      to the new profile depth.
*      The new values of root length per layer can be linearly
*      interpolated back from this shape taking into account
*      the rescaling of the profile.
*
*  Changes
*     01-05-1997 - huth - Programmed and Specified
*
*/
   {
   //  Local Variables
   int layer;                   // simple layer counter
   int nlayr_rt_old;            // No. of layers in old root profile
   int nlayr_rt_new;            // No. of layers in old root profile
   float pro_red_fr;                 // profile reduction fraction
   float cum_root_bottom;            // cum root at bottom of layer (mm/mm2)
   float cum_root_top;               // cum root at top of layer (mm/mm2)
   float layer_top;                  // depth to top of layer (mm)
   float layer_bottom;               // depth to bottom of layer (mm)

   // Implementation Section ----------------------------------
   int max_layer = max(nlayr_old,nlayr_new); 
   if (max_layer <= 0) {throw std::invalid_argument("max_layer 0 in crop_root_length_growth1");}
   float *cum_root_length = new float[max_layer];  //Cum Root length with depth (mm/mm2)
   float *cum_root_depth = new float[max_layer];   //Cum Root depth (mm)

   // Identify key layers
   // -------------------
   nlayr_rt_old = find_layer_no(root_depth_old,dlayer_old,nlayr_old);
   nlayr_rt_new = find_layer_no(root_depth_new,dlayer_new,nlayr_new);

      // calculate the fractional reduction in total profile depth
      // ---------------------------------------------------------
   pro_red_fr = divide (root_depth_new, root_depth_old, 0.0);

      // build interpolation pairs based on 'squashed' original root profile
      // -------------------------------------------------------------------
   cum_root_depth[0] = 0.0;
   cum_root_length[0] = 0.0;

   for(layer = 0; layer <= nlayr_rt_old; layer++)
      {
      if (layer == nlayr_rt_old)
         {
         cum_root_depth[layer + 1] = root_depth_old * pro_red_fr;
         }
      else
         {
         cum_root_depth[layer + 1] = cum_root_depth[layer] + dlayer_old[layer] * pro_red_fr;
         }
      cum_root_length[layer + 1] = cum_root_length[layer] + root_length[layer];
      }

   fill_real_array (root_length, 0.0, nlayr_rt_old);

   // look up new root length from interpolation pairs
   // ------------------------------------------------
   for(layer = 0; layer <= nlayr_rt_new; layer++)
      {
      layer_bottom = sum_real_array (dlayer_new, layer+1);
      layer_top = layer_bottom - dlayer_new[layer];
      cum_root_top = linear_interp_real (layer_top, cum_root_depth,
                                         cum_root_length, nlayr_rt_old + 1);
      cum_root_bottom = linear_interp_real (layer_bottom, cum_root_depth,
                                         cum_root_length, nlayr_rt_old + 1);
      root_length[layer] = cum_root_bottom - cum_root_top;
      }

   //delete dynamic memoy
   delete []cum_root_depth;
   delete []cum_root_length;
   }

//+  Purpose
//   Calculate the increase in root length density in each rooted
//   layer based upon soil hospitality, moisture and fraction of
//   layer explored by roots.

//+  Mission Statement
//   Calculate the root length growth for each layer
void cproc_root_length_growth_new (
     float  c_specific_root_length                 // (INPUT) length of root per unit wt (mm
    ,float  p_root_distribution_pattern            // (INPUT) patter with depth
    ,float *g_dlayer                               // (INPUT)  thickness of soil layer I (mm)
    ,float  g_dlt_root_wt                          // (INPUT)  plant root biomass growth (g/m
    ,float *g_dlt_root_length                      // (OUTPUT) increase in root length (mm/mm
    ,float  g_dlt_root_depth                       // (INPUT)  increase in root depth (mm)
    ,float  g_root_depth                           // (INPUT)  depth of roots (mm)
    ,float *g_root_length                          // (INPUT)
    ,float  g_plants                               // (INPUT)
    ,float *p_xf                                   // (INPUT)  eXtension rate Factor (0-1)
    ,int    c_num_sw_ratio                         // (INPUT)
    ,float *c_x_sw_ratio                           // (INPUT)
    ,float *c_y_sw_fac_root                        // (INPUT)
    ,float *c_x_plant_rld                          // (INPUT)
    ,float *c_y_rel_root_rate                      // (INPUT)
    ,int    c_num_plant_rld                        // (INPUT)
    ,float *g_dul_dep                              // (INPUT)  drained upper limit soil water
    ,float *g_sw_dep                               // (INPUT)  soil water content of layer L
    ,float *p_ll_dep                               // (INPUT)  lower limit of plant-extractab
    ,int    max_layer)                             // (INPUT)  maximum number of soil laye
    {
     //+  Local Variables
    int   deepest_layer;                          // deepest rooted later
    float dlt_length_tot;                         // total root length increase (mm/m^2)
    int   layer;                                  // simple layer counter variable
    float *rlv_factor;                           // relative rooting factor for a layer
    float rlv_factor_tot;                         // total rooting factors across profile
    float branching_factor;                       //
    float plant_rld;
    float rld;
    float root_length_tot;
    float rwf;
    float cum_layer_depth;
    float cum_depth;
    float *root_length_new;

//- Implementation Section ----------------------------------
    rlv_factor = new float [max_layer];
    root_length_new = new float [max_layer];

        fill_real_array (g_dlt_root_length, 0.0, max_layer);
        fill_real_array (root_length_new, 0.0, max_layer);

        deepest_layer = find_layer_no (g_root_depth+g_dlt_root_depth
                                      , g_dlayer
                                      , max_layer);

        cum_layer_depth = sum_real_array(g_dlayer, deepest_layer);

        rlv_factor_tot = 0.0;
        cum_depth      = 0.0;

        for (layer = 0; layer <= deepest_layer; layer++) 
           {
           cum_depth = cum_depth + 0.5 * g_dlayer[layer];
           rwf       = divide (cum_depth, cum_layer_depth, 0.0) ;
           rwf       = pow((1.0 - rwf),p_root_distribution_pattern);

           rld       = divide (g_root_length[layer], g_dlayer[layer], 0.0);
   
           plant_rld = divide (rld, g_plants ,0.0);
   
           branching_factor = linear_interp_real(plant_rld
                                                ,c_x_plant_rld
                                                ,c_y_rel_root_rate
                                                ,c_num_plant_rld);
   
           rlv_factor[layer] =
                  crop_sw_avail_fac(c_num_sw_ratio
                                    , c_x_sw_ratio
                                    , c_y_sw_fac_root
                                    , g_dul_dep
                                    , g_sw_dep
                                    , p_ll_dep
                                    , layer)
                * branching_factor
                * p_xf [layer]
                * divide(g_dlayer[layer], g_root_depth, 0.0);
   
           rlv_factor[layer] = rlv_factor[layer] * rwf;

           rlv_factor[layer] = l_bound(rlv_factor[layer],1e-6);
           rlv_factor_tot = rlv_factor_tot + rlv_factor[layer];
        }

        dlt_length_tot = g_dlt_root_wt/sm2smm * c_specific_root_length;

        root_length_tot= sum_real_array(g_root_length, max_layer);
        root_length_tot= root_length_tot + dlt_length_tot;

        for (layer = 0; layer <= deepest_layer; layer++) 
            {
            root_length_new[layer] = root_length_tot
                   * divide (rlv_factor[layer]
                   ,rlv_factor_tot
                   ,0.0);

            g_dlt_root_length [layer] = root_length_new[layer] - g_root_length [layer];

            g_dlt_root_length [layer] = dlt_length_tot
                   * divide (rlv_factor[layer]
                   ,rlv_factor_tot
                   ,0.0);

            }
    delete [] rlv_factor;
    delete [] root_length_new;
    }
//+  Purpose
//       Return the increase in root depth (mm)

//+  Mission Statement
//   Calculate the increase in rooting depth.

//+  Notes
//         there is a discrepency when the root crosses into another
//         layer. - cr380

//+  Changes
//      031097 nih specified and programmed
void legopt_root_depth_increase
    (
     float  c_root_depth_rate[]                 // (INPUT)  root growth rate potential (mm
    ,float  g_current_stage                    // (INPUT)  current phenological stage
    ,float  *g_dlayer                          // (INPUT)  thickness of soil layer I (mm)
    ,float  g_root_depth                       // (INPUT)  depth of roots (mm)
    ,float  g_sw_avail_fac_deepest_layer       // (INPUT)
    ,float  *p_xf                              // (INPUT) eXploration Factor (0-1)
    ,float  temp_factor
    ,float  *dlt_root_depth                     // (OUTPUT) increase in root depth (mm)
    ) {
//+  Local Variables
    int   current_phase;                          // current phase number
    float root_depth_max;                         // maximum depth to which roots can go (mm)
    int   current_layer;                          // layer of root front
    int   deepest_layer;                          // deepest layer for rooting

//- Implementation Section ----------------------------------

    current_layer = find_layer_no(g_root_depth, g_dlayer, max_layer);
    current_phase = (int) g_current_stage;

// this equation allows soil water in the deepest
// layer in which roots are growing
// to affect the daily increase in rooting depth.

    *dlt_root_depth  = c_root_depth_rate[current_phase-1]
                              * g_sw_avail_fac_deepest_layer
                              * p_xf[current_layer]
                              * temp_factor;

// constrain it by the maximum
// depth that roots are allowed to grow.

    deepest_layer = count_of_real_vals (p_xf, max_layer);
    root_depth_max = sum_real_array (g_dlayer, deepest_layer+1);
    *dlt_root_depth = u_bound (*dlt_root_depth, root_depth_max - g_root_depth);
    }

//+  Purpose
//       Calculate plant rooting depth through time

//+  Mission Statement
//   Calculate today's rooting depth

//+  Changes
//     170498 nih specified and programmed
void legopt_root_depth1
    (
     float *g_dlayer                     // (INPUT)  layer thicknesses (mm)
    ,int   c_num_sw_ratio                // (INPUT) number of sw lookup pairs
    ,float *c_x_sw_ratio                 // (INPUT) sw factor lookup x
    ,float *c_y_sw_fac_root              // (INPUT) sw factor lookup y
    ,float *g_dul_dep                    // (INPUT) DUL (mm)
    ,float *g_sw_dep                     // (INPUT) SW (mm)
    ,float *p_ll_dep                     // (INPUT) LL (mm)
    ,float *c_root_depth_rate            // (INPUT) root front velocity (mm)
    ,float g_current_stage               // (INPUT) current growth stage
    ,float g_mint
    ,float g_maxt
    ,float *c_x_temp_root_advance
    ,float *c_y_rel_root_advance
    ,int   c_num_temp_root_advance
    ,float *p_xf                         // (INPUT) exploration factor
    ,float g_root_depth                  // (input) root depth (mm)
    ,float *g_dlt_root_depth              // (OUTPUT) increase in rooting depth (mm)
    ) {
    //+  Local Variables
    int   deepest_layer;                          // deepest layer in which the roots are growing
    float sw_avail_fac_deepest_layer;             //
    float temp_factor;
    float av_temp;

    //- Implementation Section ----------------------------------

    deepest_layer = find_layer_no (g_root_depth, g_dlayer, max_layer);

    sw_avail_fac_deepest_layer = crop_sw_avail_fac ( c_num_sw_ratio
                                                   ,c_x_sw_ratio
                                                   ,c_y_sw_fac_root
                                                   ,g_dul_dep
                                                   ,g_sw_dep
                                                   ,p_ll_dep
                                                   ,deepest_layer );

    av_temp = (g_mint + g_maxt)/2.0;

    temp_factor = linear_interp_real(av_temp
                                     ,c_x_temp_root_advance
                                     ,c_y_rel_root_advance
                                     ,c_num_temp_root_advance);

    legopt_root_depth_increase(c_root_depth_rate
                               , g_current_stage
                               , g_dlayer
                               , g_root_depth
                               , sw_avail_fac_deepest_layer
                               , p_xf
                               , temp_factor
                               , g_dlt_root_depth);

    }
