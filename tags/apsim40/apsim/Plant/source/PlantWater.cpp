#include <stdio.h>
#include <math.h>
#include <vector>
#include <string>
#include "Plantlibrary.h"
#include "PlantInterface.h"


//===========================================================================
void crop_sw_avail(int   num_layer,     // (INPUT)  number of layers in profile
                   float *dlayer,            // (INPUT)  thickness of soil layer I (mm)
                   float root_depth,        // (INPUT)  depth of roots (mm)
                   float *sw_dep,            // (INPUT)  soil water content of layer L (mm)
                   float *ll_dep,            // (INPUT)  lower limit of plant-extractable  soil water for soil layer L (mm)
                   float *sw_avail)          // (OUTPUT) crop water potential uptake  for each full layer (mm)
//===========================================================================

/*  Purpose
*       Return actual water available for extraction from each layer in the
*       soil profile by the crop (mm water)
*
*  Mission Statement
*   Calculate the available soil water
*
*  Notes
*       see cr474 for limitations and potential problems.
*
*  Changes
*       21/5/2003 ad converted to BC++
*       010994 jngh specified and programmed
*       970216 slw generalised to avoid common blocks, added num_layer
*/
   {
   //  Local Variables
   int deepest_layer;    // index of deepest layer in which the roots are growing
   int layer;            // soil profile layer number
   // Implementation Section ----------------------------------

   // get potential uptake

   fill_real_array (sw_avail, 0.0, num_layer);

   deepest_layer = find_layer_no (root_depth, dlayer, num_layer);
   for(int layer = 0; layer <= deepest_layer; layer++)
      {
      sw_avail[layer] = sw_dep[layer] - ll_dep[layer];
      sw_avail[layer] = l_bound (sw_avail[layer], 0.0);
      }
   // correct bottom layer for actual root penetration
   sw_avail[deepest_layer] = sw_avail[deepest_layer] *
             root_proportion(deepest_layer, dlayer, root_depth);
   }



//=========================================================================
void crop_sw_supply(int   num_layer,        // (INPUT)  number of layers in profile
                    float *dlayer,          // (INPUT)  thickness of soil layer I (mm)
                    float root_depth,       // (INPUT)  depth of roots (mm)
                    float *sw_dep,          // (INPUT)  soil water content of layer L (mm)
                    float *kl,              // (INPUT)  root length density factor for water
                    float *ll_dep,          // (INPUT)  lower limit of plant-extractable soi
                    float *sw_supply)       // (OUTPUT) potential crop water uptake from each layer (mm) (supply to roots)
//=========================================================================

/*  Purpose
*       Return potential water uptake from each layer of the soil profile
*       by the crop (mm water). This represents the maximum amount in each
*       layer regardless of lateral root distribution but takes account of
*       root depth in bottom layer.
*
*  Mission Statement
*   Calculate today's soil water supply
*
*  Notes
*      This code still allows water above dul to be taken - cnh
*
*  Changes
*       21/5/2003 ad converted to BC++
*       010994 jngh specified and programmed - adapted from barley
*       970216 slw generalised to avoid common blocks, added num_layer
*/
   {
   //  Local Variables
   int deepest_layer;      // index of deepest layer in which the roots are growing
   float sw_avail;         // water available (mm)
   // Implementation Section ----------------------------------

   // get potential uptake

   fill_real_array (sw_supply, 0.0, num_layer);

   deepest_layer = find_layer_no (root_depth, dlayer, num_layer);
   for(int i = 0; i <= deepest_layer; i++)
      {
      sw_avail = (sw_dep[i] - ll_dep[i]);
      sw_supply[i] = sw_avail * kl[i];
      sw_supply[i] = l_bound (sw_supply[i], 0.0);
      }
   //now adjust bottom layer for depth of root
   sw_supply[deepest_layer] = sw_supply[deepest_layer]*
               root_proportion(deepest_layer, dlayer, root_depth);
   }


//========================================================================
void crop_sw_uptake0(int   num_layer,      // (INPUT)  number of layers in profile
                     float *dlayer,             // (INPUT)  thickness of soil layer I (mm)
                     float root_depth,         // (INPUT)  depth of roots (mm)
                     float sw_demand,          // (INPUT)  total crop demand for water (mm)
                     float *sw_supply,          // (INPUT)  potential water to take up (supply) from current soil water (mm)
                     float *dlt_sw_dep)         // (OUTPUT) root water uptake (mm)
//========================================================================

/*  Purpose
*       Returns actual water uptake from each layer of the soil
*       profile by the crop (mm).
*
*  Mission Statement
*   Calculate the uptake of soil water by the crop
*
*  Changes
*       21/5/2003 ad converted to BC++
*       010994 jngh specified and programmed
*       970216 slw generalised to avoid common blocks , added num_layer parameter
*/
   {
   //  Local Variables
   int deepest_layer;       // deepest layer in which the roots are growing
   int layer;               // layer number of profile ()
   float sw_supply_sum;     // total potential over profile (mm)
   // Implementation Section ----------------------------------
   // find total root water potential uptake as sum of all layers

   deepest_layer = find_layer_no (root_depth, dlayer, num_layer);
   sw_supply_sum = sum_real_array (sw_supply, deepest_layer+1);
   if ((sw_supply_sum < 0.0) || (sw_demand < 0.0))
      {
      // we have no uptake - there is no demand or potential
      fill_real_array (dlt_sw_dep, 0.0, num_layer);
      }
   else
      {
      // get actual uptake

      fill_real_array (dlt_sw_dep, 0.0, num_layer);
      if (sw_demand  < sw_supply_sum)
         {
               // demand is less than what roots could take up.
               // water is non-limiting.
               // distribute demand proportionately in all layers.
         for(int layer = 0; layer <= deepest_layer; layer++)
            {
            dlt_sw_dep[layer] = -1 * divide (sw_supply[layer], sw_supply_sum, 0.0) * sw_demand;
            }
         }
      else
         {
                // water is limiting - not enough to meet demand so take
                // what is available (potential)
         for(int layer = 0; layer <= deepest_layer; layer++)
            {
            dlt_sw_dep[layer] = -1 * sw_supply[layer];
            }
         }
      }
   }



//==========================================================================
void crop_swdef_expansion(int   num_sw_demand_ratio,   //  (INPUT)
                          float *x_sw_demand_ratio,    //  (INPUT)
                          float *y_swdef_leaf,         //  (INPUT)
                          int   num_layer,             //  (INPUT)  number of layers in profile
                          float *dlayer,               //  (INPUT)  thickness of soil layer I (mm)
                          float root_depth,            //  (INPUT)  depth of roots (mm)
                          float sw_demand,            //  (INPUT)  total crop demand for water (mm)
                          float *sw_supply,            //  (INPUT)  potential water to take up (supply) from current soil water (mm)
                          float *swdef)                //  (OUTPUT) sw stress factor (0-1)
//==========================================================================


/*  Purpose
*       Get the soil water to demand ratio and calculate the 0-1 stress factor for leaf expansion.
*       1 is no stress, 0 is full stress.
*
*  Mission Statement
*   Calculate the soil water stress factor for leaf expansion
*
*  Changes
*       21/5/2003 ad converted to BC++
*       010994 jngh specified and programmed
*       970216 slw generalised to avoid common blocks , added num_layer parameter
*/
{
   //  Local Variables
   int deepest_layer;            // deepest layer in which the roots are growing
   float sw_demand_ratio;        // water supply:demand ratio
   float sw_supply_sum;          // total supply over profile (mm)

   // Implementation Section ----------------------------------
   if (sw_demand > 0.0)
   {
      deepest_layer = find_layer_no(root_depth, dlayer, num_layer);

      // get potential water that can be taken up when profile is full

      sw_supply_sum = sum_real_array (sw_supply, deepest_layer+1);
      sw_demand_ratio = divide (sw_supply_sum, sw_demand, 10.0);
      *swdef = linear_interp_real (sw_demand_ratio, x_sw_demand_ratio,
                                   y_swdef_leaf, num_sw_demand_ratio);
   }
   else
   {
      *swdef = 1.0;
   }
}

//========================================================================
void crop_swdef_photo(int   num_layer,    //(INPUT)  number of layers in profile
                      float *dlayer,      //(INPUT)  thickness of soil layer I (mm)
                      float root_depth,   //(INPUT)  depth of roots (mm)
                      float sw_demand,   //(INPUT)  total crop demand for water (mm)
                      float *dlt_sw_dep,   //(INPUT)  daily soil water uptake (mm)
                      float *swdef)       //(OUTPUT) sw stress factor (0-1)
//========================================================================
/*  Purpose
*       Calculate the soil water supply to demand ratio and therefore the 0-1 stress factor
*       for photosysnthesis. 1 is no stress, 0 is full stress.
*
*  Mission Statement
*   Calculate the soil water stress factor for photosynthesis
*
*  Changes
*       21/5/2003 ad converted to BC++
*       010994 jngh specified and programmed
*       970216 slw generalised to avoid common blocks , added num_layer parameter
*/
{
   //  Local Variables
   int deepest_layer;      // deepest layer in which the roots are growing
   float sw_demand_ratio;  // water supply:demand ratio
   float sw_uptake_sum;    // total uptake over profile (mm)
   // Implementation Section ----------------------------------
   if (root_depth > 0.0)
   {
      deepest_layer = find_layer_no (root_depth, dlayer, num_layer);

      //get potential water that can be taken up when profile is full
      sw_uptake_sum = -sum_real_array (dlt_sw_dep, deepest_layer+1);
      sw_demand_ratio = divide (sw_uptake_sum, sw_demand, 1.0);
      *swdef = bound (sw_demand_ratio , 0.0, 1.0);
   }
   else
   {
      *swdef = 1.0;
   }
}


//=========================================================================
void crop_swdef_pheno(int    num_sw_avail_ratio,        // (INPUT)
                      float *x_sw_avail_ratio,          // (INPUT)
                      float *y_swdef_pheno,             // (INPUT)
                      int    num_layer,                 // (INPUT)  number of layers in profile
                      float *dlayer,                    // (INPUT)  thickness of soil layer I (mm)
                      float  root_depth,                // (INPUT)  depth of roots (mm)
                      float *sw_avail,                  // (INPUT)  actual extractable soil water (mm)
                      float *sw_avail_pot,              // (INPUT)  potential extractable soil water (mm)
                      float *swdef)                     // (OUTPUT) sw stress factor (0-1)
//=========================================================================

/*  Purpose
*       Get the soil water availability ratio in the root zone
*       and calculate the 0 - 1 stress factor for phenology.
*       1 is no stress, 0 is full stress.
*
*  Mission Statement
*   Calculate the soil water stress factor for phenological development
*
*  Changes
*       21/5/2003 ad converted to BC++
*       010994 jngh specified and programmed
*       970216 slw generalised to avoid common blocks , added num_layer parameter
*/
{
   //  Local Variables
   int deepest_layer;         // deepest layer in which the roots are growing
   float sw_avail_ratio;      // water availability ratio
   float sw_avail_pot_sum;    // potential extractable soil water (mm)
   float sw_avail_sum;        // actual extractable soil water (mm)

   if (root_depth > 0.0)
   {
      deepest_layer = find_layer_no (root_depth, dlayer, num_layer);
      sw_avail_pot_sum = sum_real_array (sw_avail_pot, deepest_layer+1);
      sw_avail_sum = sum_real_array (sw_avail, deepest_layer+1);
      sw_avail_ratio = divide (sw_avail_sum, sw_avail_pot_sum, 1.0);
      sw_avail_ratio = bound (sw_avail_ratio , 0.0, 1.0);
      *swdef = linear_interp_real(sw_avail_ratio, x_sw_avail_ratio,
                                  y_swdef_pheno, num_sw_avail_ratio);
   }
   else
   {
      *swdef = 1.0;
   }
}

//==========================================================================
void crop_swdef_fixation(int  num_sw_avail_fix,      // (INPUT)
                         float *x_sw_avail_fix,      // (INPUT)
                         float *y_swdef_fix,         // (INPUT)
                         int   num_layer,       // (INPUT)  number of layers in profile
                         float *dlayer,              // (INPUT)  thickness of soil layer I (mm)
                         float  root_depth,          // (INPUT)  depth of roots (mm)
                         float *sw_avail,            // (INPUT)  actual extractable soil water (mm)
                         float *sw_avail_pot,        // (INPUT)  potential extractable soil water (mm)
                         float *swdef)               // (OUTPUT) sw stress factor (0-1)
//==========================================================================

/*  Purpose
*       Get the soil water availability ratio in the root zone and
*       calculate the 0 - 1 stress factor for fixation.
*       1 is no stress, 0 is full stress.
*
*  Mission Statement
*   Calculate the soil water stress factor for the fixation of nitrogen
*
*  Changes
*       21/5/2003 ad converted to BC++
*       010994 jngh specified and programmed
*       970216 slw generalised to avoid common blocks , added num_layer parameter
*/
{
   //  Local Variables
   int deepest_layer;        // deepest layer in which the roots are growing
   float sw_avail_ratio;     // water availability ratio
   float sw_avail_pot_sum;   // potential extractable soil water (mm)
   float sw_avail_sum;       // actual extractable soil water (mm)

   // Implementation Section ----------------------------------
   if (root_depth > 0.0)
   {
      deepest_layer = find_layer_no(root_depth, dlayer, num_layer);

      // get potential water that can be taken up when profile is full
      sw_avail_pot_sum = sum_real_array(sw_avail_pot, deepest_layer+1);
      sw_avail_sum = sum_real_array(sw_avail, deepest_layer+1);
      sw_avail_ratio = divide(sw_avail_sum, sw_avail_pot_sum, 1.0);
      sw_avail_ratio = bound(sw_avail_ratio , 0.0, 1.0);
      *swdef = linear_interp_real(sw_avail_ratio, x_sw_avail_fix, y_swdef_fix,
                                  num_sw_avail_fix);
   }
   else
   {
      *swdef = 1.0;
   }
}

//===========================================================================
void crop_oxdef_photo1(int   C_num_oxdef_photo,    //  (INPUT)
                       float *C_oxdef_photo,      //  (INPUT)
                       float *C_oxdef_photo_rtfr, //  (INPUT)
                       float *G_ll15_dep,         //  (INPUT)
                       float *G_sat_dep,          //  (INPUT)
                       float *G_sw_dep,           //  (INPUT)  soil water content of layer L
                       float *G_dlayer,           //  (INPUT)  thickness of soil layer I (mm)
                       float *G_root_length,      //  (INPUT)
                       float G_root_depth,        //  (INPUT)  depth of roots (mm)
                       int   max_layer,           //  (INPUT)
                       float *oxdef_photo)        //  (OUTPUT)
//===========================================================================

/*  Purpose
*   Calculate 0-1 factor for water logging effect on growth
*
*  Mission Statement
*   Calculate the oxygen deficit factor for photosynthesis
*
*  Changes
*       21/5/2003 ad converted to BC++
*       neilh - 18-11-1997 - adapted from sugar model
*
*/
{
   if (G_root_depth > 0.0)
   {

   //  Local Variables
   int layer;
   int num_root_layers;
   float wet_root_fr;
   float wfps;
   float tot_root_fr;
   float *root_fr = new float[max_layer];

   // Implementation Section ----------------------------------
   crop_root_dist(G_dlayer, G_root_length, G_root_depth, root_fr, 1.0,  max_layer);

   num_root_layers = count_of_real_vals (root_fr, max_layer);

   wet_root_fr = 0.0;
   for(layer = 0; layer <= num_root_layers; layer++)
      {
      wfps = divide (G_sw_dep[layer] - G_ll15_dep[layer],
                     G_sat_dep[layer] - G_ll15_dep[layer], 0.0);
      wfps = bound (wfps, 0.0, 1.0);

      wet_root_fr = wet_root_fr + wfps * root_fr[layer];
      }
   *oxdef_photo = linear_interp_real(wet_root_fr,C_oxdef_photo_rtfr,C_oxdef_photo,
                                     C_num_oxdef_photo);
   delete [] root_fr;
   }
   else
   {
      *oxdef_photo = 1.0;
   }
}


//============================================================================
void cproc_sw_supply1 (commsInterface *iface,
                       float C_sw_lb,              //(INPUT)
                       float *G_dlayer,            //(INPUT)
                       float *P_ll_dep,            //(INPUT)
                       float *G_dul_dep,           //(INPUT)
                       float *G_sw_dep,            //(INPUT)
                       int   max_layer,            //(INPUT)
                       float G_root_depth,        //(INPUT)
                       float *P_kl,                //(INPUT)
                       float *G_sw_avail,          //(OUTPUT)
                       float *G_sw_avail_pot,      //(OUTPUT)
                       float *G_sw_supply)         //(OUTPUT)
//============================================================================

/*  Purpose
*     Calculate the crop water supply based on the KL approach.
*
*  Mission Statement
*   Calculate today's soil water supply
*
*  Changes
*     21/5/2003 ad converted to BC++
*     17-04-1998 - neilh - Programmed and Specified
*
*/
   {
   // Implementation Section ----------------------------------
   crop_check_sw(iface, C_sw_lb, G_dlayer, G_dul_dep, max_layer,
                 G_sw_dep, P_ll_dep);

   crop_sw_avail_pot(max_layer, G_dlayer, G_dul_dep,
                     G_root_depth, P_ll_dep, G_sw_avail_pot);   // potential extractable sw

   crop_sw_avail(max_layer, G_dlayer, G_root_depth, G_sw_dep,
                 P_ll_dep, G_sw_avail);                     // actual extractable sw (sw-ll)

   crop_sw_supply(max_layer,G_dlayer,G_root_depth,G_sw_dep,
                  P_kl, P_ll_dep, G_sw_supply);
   }


//===========================================================================
void crop_sw_avail_pot(int   num_layer,       //   (INPUT)  number of layers in profile
                       float *dlayer,         //   (INPUT)  thickness of soil layer I (mm)
                       float *dul_dep,        //   (INPUT)  drained upper limit soil water content for soil layer L (mm water)
                       float root_depth,      //   (INPUT)  depth of roots (mm)
                       float *ll_dep,         //   (INPUT)  lower limit of plant-extractable soil water for soil layer L (mm)
                       float *sw_avail_pot)   //   (OUTPUT) crop water potential uptake for each full layer (mm)
//===========================================================================
/*  Purpose
*       Return potential available soil water from each layer in the root zone.
*
*  Mission Statement
*   Calculate the potentially (or maximum) available soil water
*
*  Notes
*       see cr474 for limitations and potential problems.
*
*  Changes
*       21/5/2003 ad converted to BC++
*       010994 jngh specified and programmed
*       970216 slw generalised to avoid common blocks , added num_layer parameter
*/
   {
   //  Local Variables
   int deepest_layer;    // index of deepest layer in which the roots are growing
   int layer;            // soil profile layer number
   // Implementation Section ----------------------------------

   // get potential uptake

   fill_real_array (sw_avail_pot, 0.0, num_layer);

   deepest_layer = find_layer_no (root_depth, dlayer, num_layer);
   for(layer = 0; layer <= deepest_layer; layer++)
      {
      sw_avail_pot[layer] = dul_dep[layer] - ll_dep[layer];
      }
   // correct bottom layer for actual root penetration
   sw_avail_pot[deepest_layer] = sw_avail_pot[deepest_layer] *
                 root_proportion(deepest_layer, dlayer, root_depth);
   }


//==========================================================================
void cproc_sw_uptake1(int   num_layer,        //  (INPUT)  number of layers in profile
                      float *dlayer,          //  (INPUT)  thickness of soil layer I (mm)
                      float root_depth,       //  (INPUT)  depth of roots (mm)
                      float sw_demand,       //  (INPUT)  total crop demand for water (mm)
                      float *sw_supply,       //  (INPUT)  potential water to take up (supply)
                      float *dlt_sw_dep)      //  (OUTPUT) root water uptake (mm)
//==========================================================================
/*  Purpose
*       Returns actual water uptake from each layer of the soil
*       profile by the crop (mm).
*
*  Mission Statement
*   Calculate the crop uptake of soil water
*
*  Changes
*       21/5/2003 ad converted to BC++
*       200498 nih created from crop_sw_uptake0
*/
   {
   //  Local Variables
   int deepest_layer;      // deepest layer in which the roots are growing
   int layer;              // layer number of profile ()
   float sw_supply_sum;    // total potential over profile (mm)
   // Implementation Section ----------------------------------

   //find total root water potential uptake as sum of all layers

   deepest_layer = find_layer_no (root_depth, dlayer, num_layer);
   sw_supply_sum = sum_real_array (sw_supply, deepest_layer+1);

   if ((sw_supply_sum < 0.0) || (sw_demand  < 0.0))
      {
      //we have no uptake - there is no demand or potential
      fill_real_array (dlt_sw_dep, 0.0, num_layer);
      }
   else
      {
      // get actual uptake
      fill_real_array (dlt_sw_dep, 0.0, num_layer);
      if (sw_demand < sw_supply_sum)
         {
         // demand is less than what roots could take up.
         // water is non-limiting.
         // distribute demand proportionately in all layers.
         for(layer = 0; layer <= deepest_layer; layer++)
            {
            dlt_sw_dep[layer] = -1 * divide (sw_supply[layer], sw_supply_sum, 0.0) * sw_demand;
            }
         }
      else
         {
         // water is limiting - not enough to meet demand so take
         // what is available (potential)
         for(layer = 0; layer <= deepest_layer; layer++)
            {
            dlt_sw_dep[layer] = -1 * sw_supply[layer];
            }
         }
      }
   }

//===========================================================================
void cproc_sw_demand1(float dlt_dm_pot_rue,      //(INPUT)  potential dry matter production with opt
                      float transp_eff,          //(INPUT)  transpiration efficiency (g dm/m^2/mm wa
                      float *sw_demand)          //(OUTPUT) crop water demand (mm)
//===========================================================================
/*  Purpose
*       Return crop water demand from soil by the crop (mm) calculated by
*       dividing biomass production limited by radiation by transpiration efficiency.
*
*  Mission Statement
*   Calculate the crop demand for soil water (based upon transpiration efficiency)
*
*  Changes
*       21/5/2003 ad converted to BC++
*       010994 jngh specified and programmed
*       970216 slw generalised
*
*/
   {
   // get potential transpiration from potential
   // carbohydrate production and transpiration efficiency
   *sw_demand = divide (dlt_dm_pot_rue, transp_eff, 0.0);
   }
//==========================================================================
void cproc_sw_demand_bound (float sw_demand_unbounded,  //(INPUT)  Unbounded sw demand (mm)
                            float eo_crop_factor,       //(INPUT) crop factor for eo   (-)
                            float eo,                   //(INPUT) eo                  (mm)
                            float cover_green,          //(INPUT) green crop cover    (-)
                            float *sw_demand_bounded)    //(OUTPUT) bounded sw demand (mm)
//===========================================================================

/*  Purpose
*       Calculated a bounded value of crop sw demand by constraining sw demand
*       to some fraction/multiple of atmospheric potential (Eo).
*
*  Mission Statement
*   Constrain sw demand according to atmospheric potential.
*
*  Changes
*       21/5/2003 ad converted to BC++
*       311002 nih/jngh specified and programmed
*/
   {
   //  Local Variables
   float sw_demand_max;    // maximum sw demand as constrained
                           // by atmospheric potential (mm)

   // Implementation Section ----------------------------------
   sw_demand_max = eo_crop_factor * eo * cover_green ;

   *sw_demand_bounded = u_bound(sw_demand_unbounded, sw_demand_max);
   }

//==========================================================================
float svp(float temp) //(INPUT)  fraction of distance between svp at mi
//==========================================================================
/*  Purpose
*
*  Mission Statement
*    function to get saturation vapour pressure for a given temperature in oC (kpa)
*
*  Changes
*       21/5/2003 ad converted to BC++
*
*/
   {
   float val = 6.1078 *
            exp(17.269 * temp / (237.3 + temp)) *
            mb2kpa;
   return val;
   }

//==========================================================================
void cproc_transp_eff1(float svp_fract,         ///  (INPUT)  fraction of distance between svp at mi
                       float transp_eff_cf,    ///   (INPUT)  transpiration efficiency coefficien
                       float maxt,              ///  (INPUT)  maximum air temperature (oC)
                       float mint,              ///  (INPUT)  minimum air temperature (oC)
                       float *transp_eff)       ///   (OUTPUT)
//==========================================================================
/*  Purpose
*       Calculate today's transpiration efficiency from the transpiration
*       efficiency coefficient and vapour pressure deficit, which is calculated
*       from min and max temperatures.
*
*  Mission Statement
*   Calculate today's transpiration efficiency from VPD
*
*  Assumptions
*       the temperatures are > -237.3 oC for the svp function.
*
*  Notes
*       Average saturation vapour pressure for ambient temperature
*       during transpiration is calculated as part-way between that
*       for minimum temperature and that for the maximum temperature.
*       Tanner & Sinclair (1983) used .75 and .67 of the distance as
*       representative of the positive net radiation (rn).  Daily SVP
*       should be integrated from about 0900 hours to evening when Radn
*       becomes negative.
*
*  Changes
*       21/5/2003 ad converted to BC++
*       140198 nih developed from crop_transp_eff1
*       070199 igh added l_bound to vpd to stop vpd = 0
*
*/
   {
   //  Local Variables
   float temp_arg;      // dummy temperature for function (oC)
   float vpd;           // vapour pressure deficit (kpa)
   int current_phase;
   // Implementation Section ----------------------------------

   // get vapour pressure deficit when net radiation is positive.
   vpd = l_bound (svp_fract * ( svp(maxt) - svp(mint)), 0.01);

   *transp_eff = divide (transp_eff_cf, vpd, 0.0) / g2mm;
   }

//===========================================================================
void cproc_bio_water1(int   num_layer,      //(INPUT)  number of layers in profile
                      float *dlayer,        //(INPUT)  thickness of soil layer I (mm)
                      float root_depth,    //(INPUT)  depth of roots (mm)
                      float *dlt_sw_dep,     //(INPUT)  potential water to take up (supply)
                      float transp_eff,    //(INPUT)  transpiration efficiency (g dm/m^2/m
                      float *dlt_dm_pot_te) //(OUTPUT) potential dry matter production
                                            //         by transpiration (g/m^2)
//===========================================================================
/*  Purpose
*   Calculate the potential biomass production based upon today's water supply.
*
*  Mission Statement
*   Calculate the potential biomass production based upon today's water supply.
*
*  Changes
*       21/5/2003 ad converted to BC++
*       090994 jngh specified and programmed
*       160297 slw generalised to avoid common blocks , added num_layer paramete
*/
   {
   //  Local Variables
   int deepest_layer;     // deepest layer in which the roots are growing
   float sw_uptake_sum;   // Water available to roots (mm)
   // Implementation Section ----------------------------------

   // potential (supply) by transpiration

   deepest_layer = find_layer_no (root_depth, dlayer, num_layer);
   sw_uptake_sum = -sum_real_array (dlt_sw_dep, deepest_layer+1);
//   static int ctr = 0;
//fprintf(stdout, "%d, %f %f\n", ctr, sw_supply_sum, transp_eff);
//ctr = ctr + 1;
   *dlt_dm_pot_te = sw_uptake_sum * transp_eff;
   }

//=========================================================================
void crop_check_sw(commsInterface *iface,
                   float minsw,    // (INPUT)  lowest acceptable value for ll
                   float *dlayer,   // (INPUT)  thickness of soil layer I (mm)
                   float *dul_dep,  // (INPUT)  drained upper limit soil water content for soil layer L (mm water)
                   int   max_layer,// (INPUT)  number of layers in profile ()
                   float *sw_dep,   // (INPUT)  soil water content of layer L (mm)
                   float *ll_dep)   // (INPUT)  lower limit of plant-extractable soil water
                                    //          for soil layer L (mm)
//=========================================================================
/*  Purpose
*       Check validity of soil water parameters for all soil profile layers.
*
*  Mission Statement
*       Check validity of soil water parameters for all soil profile layers.
*
*  Notes
*           Reports an error if
*           - ll_dep and dul_dep are not in ascending order
*           - ll is below c_minsw
*           - sw < c_minsw
*
*  Changes
*     21/5/2003 ad converted to BC++
*     010994 jngh specified and programmed
*     970216 slw generalised to avoid common blocks
*/
   {
   //  Local Variables
   float dul;              // drained upper limit water content of layer (mm water/mm soil)
   char err_msg[80];          // error message
   int layer;              // layer number
   float ll;               // lower limit water content of layer (mm water/mm soil)
   float sw;               // soil water content of layer l (mm water/mm soil)
   int num_layers;
   // Implementation Section ----------------------------------

   num_layers = count_of_real_vals (dlayer, max_layer);  //XX index_of_real_vals
   for(layer = 0; layer <= num_layers; layer++)
      {
      sw = divide (sw_dep[layer], dlayer[layer], 0.0);
      dul = divide (dul_dep[layer], dlayer[layer], 0.0);
      ll = divide (ll_dep[layer], dlayer[layer], 0.0);

      if (ll < minsw)
         {
         sprintf(err_msg,
            " lower limit of %8.2f in layer %d\n         is below acceptable value of %8.2f",
            ll, layer, minsw);
         iface->warningError (err_msg);
         }
      else
         {
         }

      if (dul < ll)
         {

         sprintf(err_msg,
            " Drained upper limit of %8.2f in layer %d\n         is at or below lower limit of %8.2f",
            dul,layer, ll);
         iface->warningError (err_msg);
         }
      else
         {
         }
      if (sw < minsw)
         {
         sprintf(err_msg,
            " Soil water of %8.2f in layer %d\n         is below acceptable value of %8.2f",
            sw, layer, minsw);
         iface->warningError (err_msg);
         }
      else
         {
         }
      }
   return;
   }


//==========================================================================
void cproc_transp_eff_co2(float svp_fract,        // (INPUT)  fraction of distance between svp at mi
                          float transp_eff_cf,    // (INPUT)  transpiration efficiency coefficien
                          float maxt,             // (INPUT)  maximum air temperature (oC)
                          float mint,             // (INPUT)  minimum air temperature (oC)
                          float co2level,         // (INPUT)  current co2 level (ppm)
                          float *co2_level_te,     // (INPUT)  co2 levels (ppm)
                          float *te_co2_modifier,  // (INPUT)  te modifiers of co2 levels (0-1)
                          int   num_co2_level_te,   // (INPUT)  number of table elements in co2-te modifier table
                          float *transp_eff)       // (OUTPUT) transpiration coefficient
//=========================================================================
/*  Purpose
*       Calculate today's transpiration efficiency from min,max temperatures and co2 level
*       and converting mm water to g dry matter (g dm/m^2/mm water)
*
*  Mission Statement
*       Calculate today's transpiration efficiency from VPD and CO2 level
*
*  Assumptions
*       the temperatures are > -237.3 oC for the svp function.
*       if co2_level=0.0 then co2_level=350ppm
*
*  Notes
*       Average saturation vapour pressure for ambient temperature
*       during transpiration is calculated as part-way between that
*       for minimum temperature and that for the maximum temperature.
*       Tanner & Sinclair (1983) used .75 and .67 of the distance as
*       representative of the positive net radiation (rn).  Daily SVP
*       should be integrated from about 0900 hours to evening when Radn
*       becomes negative.
*
*  Changes
*       21/5/2003 ad converted to BC++
*       20000721 ew developed from crop_transp_eff1 and added co2 effect on transp_eff
*
*  Sub-Program Arguments
*/
   {
   //  Local Variables
   float temp_arg;      // dummy temperature for function (oC)
   float vpd;           // vapour pressure deficit (kpa)
   float co2_modifier;
   float tolerance;

   // Implementation Section ----------------------------------
   //get vapour pressure deficit when net radiation is positive.

   vpd = svp_fract * (svp(maxt) - svp(mint));
   vpd = l_bound (vpd, 0.01);

   *transp_eff = divide (transp_eff_cf, vpd, 0.0) / g2mm;

   co2_modifier = linear_interp_real(co2level, co2_level_te, te_co2_modifier,
                                     num_co2_level_te);

   *transp_eff = *transp_eff * co2_modifier;
   }

