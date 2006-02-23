
#include <stdio.h>
#include <math.h>
#include <map>
#include <string>
#include <stdexcept>

#include <boost/function.hpp>
#include <boost/bind.hpp>

#include <ComponentInterface/Type.h>
#include <ComponentInterface/ApsimVariant.h>
#include <ComponentInterface/Component.h>
#include <ComponentInterface/dataTypes.h>
#include <ComponentInterface/Messages.h>
#include <ComponentInterface/MessageDataExt.h>
#include <ApsimShared/ApsimComponentData.h>
#include <ApsimShared/FStringExt.h>
#include <general/string_functions.h>

#include "PlantInterface.h"
#include "PlantLibrary.h"
#include "Plant.h"
#include "OilPart.h"
using namespace std;

void fruitOilPart::doInit (PlantComponent *systemInterface, PlantPhenology *plantPhenology)
   // ====================================================================
{
   parentPlant = systemInterface;
   phenology = plantPhenology;
}

void fruitOilPart::update(void)
{
   plantPart::update();
   plantPart::updateDm();
   plantPart::updateN();
   plantPart::updateP();
}

void fruitOilPart::onHarvest(float /* cutting_height */, float remove_fr,
                             vector<string> &dm_type,
                             vector<float> &dlt_crop_dm,
                             vector<float> &dlt_dm_n,
                             vector<float> &dlt_dm_p,
                             vector<float> &fraction_to_residue)
{
   float fractToResidue = 0.0;

   // biomass is removed
   float retain_fr_green = 0.0;
   float retain_fr_sen   = 0.0;
   float retain_fr_dead  = 0.0;

   float chop_fr_green = 1.0 - retain_fr_green;
   float chop_fr_dead  = 1.0 - retain_fr_dead;
   float chop_fr_sen   = 1.0 - retain_fr_sen;

   float dlt_dm_harvest = DMDead     * chop_fr_dead
                        + DMGreen    * chop_fr_green
                        + DMSenesced * chop_fr_sen;

   float dlt_n_harvest = NDead     * chop_fr_dead
                        + NGreen    * chop_fr_green
                        + NSenesced * chop_fr_sen;

   float dlt_p_harvest = PDead  * chop_fr_dead
                        + PGreen * chop_fr_green
                        + PSen   * chop_fr_sen;

   DMDead     *= retain_fr_dead;
   DMSenesced *= retain_fr_sen;
   DMGreen    *= retain_fr_green;


   NDead     *= retain_fr_dead;
   NSenesced *= retain_fr_sen;
   NGreen    *= retain_fr_green;

   PDead  *= retain_fr_dead;
   PSen   *= retain_fr_sen;
   PGreen *= retain_fr_green;


   dm_type.push_back(c.name);
   fraction_to_residue.push_back(fractToResidue);
   dlt_crop_dm.push_back (dlt_dm_harvest * gm2kg/sm2ha);
   dlt_dm_n.push_back    (dlt_n_harvest  * gm2kg/sm2ha);
   dlt_dm_p.push_back    (dlt_p_harvest  * gm2kg/sm2ha);

}

void fruitOilPart::onKillStem(void)
{
   DMDead += DMGreen + DMSenesced;
   DMGreen = 0.0;
   DMSenesced = 0.0;

   NDead += NGreen + NSenesced;
   NGreen = 0.0;
   NSenesced = 0.0;

   PDead += PGreen + PSen;
   PGreen = 0.0;
   PSen = 0.0;
}

void fruitOilPart::onFlowering(void)
{  // do nothing
}

// set the minimum weight of part; used for retranslocation to grain
void fruitOilPart::onStartGrainFill(void)
{  // do nothing
}

void fruitOilPart::dm_retranslocate1 (float g_dlt_dm)                    //FIXME
//     ===========================================================
{
}

void fruitOilPart::dm_partition1 (double g_dlt_dm)                    //FIXME
//     ===========================================================
{

   ////+  Purpose
   ////       Partitions new dm (assimilate) between plant components (g/m^2)
   //
   ////+  Changes
   ////      170703 jngh specified and programmed
   //
   ////+  Local Variables
   //    double dlt_dm_green_tot;                       // total of partitioned dm (g/m^2)
   //    double yield_demand;                           // sum of grain, energy & pod
   //    double dm_grain_demand;                        // assimilate demand for grain (g/m^2)
   //    double dm_oil_demand;                          // assimilate demand for oil (g/m^2)
   //    double dm_oil_conv_demand;                     // assimilate demand for conversion to oil (g/m^2)
   //
   ////- Implementation Section ----------------------------------
   //
   //     dlt.dm_green = 0.0;
   //     gDlt_dm_oil_conv = 0.0;
   //
   //    // calculate demands of reproductive parts
   //    dm_grain_demand = divide (gDlt_dm_grain_demand, gGrain_energy, 0.0);
   //
   //    dm_oil_demand = dm_grain_demand - dm_meal_demand;
   //    dm_oil_conv_demand = gDlt_dm_grain_demand - dm_grain_demand;
   //
   //    yield_demand = dm_oil_demand
   //                 + dm_oil_conv_demand;
   //
   //         // now distribute the assimilate to plant parts
   //    if (yield_demand >= g_dlt_dm)
   //            // reproductive demand exceeds supply - distribute assimilate to those parts only
   //    {
   //            // reproductive demand exceeds supply - distribute assimilate to those parts only
   //        oilPart->dlt.dm_green  = g_dlt_dm * divide (dm_oil_demand     , yield_demand, 0.0);
   //        gDlt_dm_oil_conv       = g_dlt_dm * divide (dm_oil_conv_demand, yield_demand, 0.0);
   //
   //    }
   //    else
   //    {
   //        // more assimilate than needed for reproductive parts
   //        // distribute to all parts
   //
   //        // satisfy reproductive demands
   //        oilPart->dlt.dm_green    = dm_oil_demand;
   //        gDlt_dm_oil_conv         = dm_oil_conv_demand;
   //
   //    }
   //
   //     dltDmGreen();      // update fruit dlt.dm_green
   //
   //    // do mass balance check
   //    dlt_dm_green_tot = dlt.dm_green
   //                     + gDlt_dm_oil_conv;
   //
   //    if (!reals_are_equal(dlt_dm_green_tot, g_dlt_dm, 1.0E-4))  // XX this is probably too much slop - try doubles XX
   //    {
   //         string msg = "Grain dlt_dm_green_tot mass balance is off: "
   //                    + ftoa(dlt_dm_green_tot, ".6")
   //                    + " vs "
   //                    + ftoa(g_dlt_dm, ".6");
   //         parentPlant->warningError(msg.c_str());
   //    }
   //
   //      // check that deltas are in legal range       //FIXME need to do something about this when array is removed
   ////    bound_check_real_array (parentPlant, dlt_dm_green, max_part, 0.0, g_dlt_dm, "Fruit dlt.dm_green");

}

