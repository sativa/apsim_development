#include <stdio.h>
#include <math.h>
#include <vector>
#include <string>
#include <ComponentInterface/Component.h>
#include "PlantComponent.h"
#include "PlantLibrary.h"
#include "Plant.h"






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
   float vpd;           // vapour pressure deficit (kpa)
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

//===========================================================================
void plant_bio_water1(float sw_supply        //(INPUT)  potential water to take up (supply)
                    , float transp_eff       //(INPUT)  transpiration efficiency (g dm/m^2/m
                    , float *dlt_dm_pot_te)  //(OUTPUT) potential dry matter production
                                       //         by transpiration (g/m^2)
//===========================================================================
//  Purpose
//   Calculate the potential biomass production based upon today's water supply.

//  Mission Statement
//   Calculate the potential biomass production based upon today's water supply.

//  Changes
//       090994 jngh specified and programmed

{
   // Implementation Section ----------------------------------

   // potential (supply) by transpiration

   *dlt_dm_pot_te = sw_supply * transp_eff;
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
   float vpd;           // vapour pressure deficit (kpa)
   float co2_modifier;

   // Implementation Section ----------------------------------
   //get vapour pressure deficit when net radiation is positive.

   vpd = svp_fract * (svp(maxt) - svp(mint));
   vpd = l_bound (vpd, 0.01);

   *transp_eff = divide (transp_eff_cf, vpd, 0.0) / g2mm;

   co2_modifier = linear_interp_real(co2level, co2_level_te, te_co2_modifier,
                                     num_co2_level_te);

   *transp_eff = *transp_eff * co2_modifier;
   }

