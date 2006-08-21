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
#include "MealPart.h"
using namespace std;

void fruitMealPart::doDMDemand (float dm_demand)                    //remove
//     ===========================================================  //remove
{                                                                   //remove
    DMGreenDemand = dm_demand;                                      //remove
}                                                                   //remove

void fruitMealPart::doNDemand (float n_demand)                      //remove
//     ===========================================================  //remove
{                                                                   //remove
    NDemand = n_demand;                                             //remove
}                                                                   //remove

void fruitMealPart::doDMDemandGrain (float dm_demand)
//     ===========================================================
{
    DMGreenDemand = dm_demand;
}

float fruitMealPart::nDemand2(void)
   //===========================================================================
{
   return l_bound(NDemand - dlt.n_senesced_retrans - dlt.n_green, 0.0);
}

void fruitMealPart::doNRetranslocate(float dltN, float /*grain_n_demand*/)
//     ===========================================================
{
    dlt.n_retrans = dltN;
}

void fruitMealPart::onHarvest(float /* cutting_height */, float /*remove_fr*/,
                              vector<string> &dm_type,
                              vector<float> &dlt_crop_dm,
                              vector<float> &dlt_dm_n,
                              vector<float> &dlt_dm_p,
                              vector<float> &fraction_to_residue)
//=======================================================================================
// biomass is removed, nothing is sent to surface residues..
{
     dm_type.push_back (c.name);
     fraction_to_residue.push_back (0.0);
     dlt_crop_dm.push_back ((DMDead+DMGreen+DMSenesced) * gm2kg/sm2ha);
     dlt_dm_n.push_back    ((NDead+NGreen+NSenesced)  * gm2kg/sm2ha);
     dlt_dm_p.push_back    ((PDead+PGreen+PSen)  * gm2kg/sm2ha);

     DMDead     = 0.0;
     DMSenesced = 0.0;
     DMGreen    = 0.0;

     NDead     = 0.0;
     NSenesced = 0.0;
     NGreen    = 0.0;

     PDead     = 0.0;
     PSen      = 0.0;
     PGreen    = 0.0;
}

void fruitMealPart::onKillStem(void)
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

void fruitMealPart::onFlowering(void)
{  // do nothing
}

// set the minimum weight of part; used for retranslocation to grain
void fruitMealPart::onStartGrainFill(void)
{  // do nothing
}

void fruitMealPart::doNConcGrainLimits(float n_min_grain, float n_crit_grain, float n_max_grain)
{
   float dm_meal;                                // meal mass (g/m2)
   dm_meal = dmGreenNew();
   g.n_conc_crit = divide (n_crit_grain, dm_meal, 0.0);
   g.n_conc_max = divide (n_max_grain, dm_meal, 0.0);
   g.n_conc_min = divide (n_min_grain, dm_meal, 0.0);
   
}

float fruitMealPart::nCapacity2(void)
{
   float n_potential  = dmGreenNew() * g.n_conc_max; 
   return (n_potential - nGreen());
}

float fruitMealPart::N_conc_pot(float nfact_grain_conc) 
{
   return (g.n_conc_min + (g.n_conc_crit - g.n_conc_min) * nfact_grain_conc);
}
