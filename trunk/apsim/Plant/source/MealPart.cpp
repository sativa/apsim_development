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

void fruitMealPart::doInit (PlantComponent *systemInterface, PlantPhenology *plantPhenology)
// ====================================================================
{
   parentPlant = systemInterface;
   phenology = plantPhenology;
}

void fruitMealPart::dm_partition1 (double g_dlt_dm)
//     ===========================================================
{
    dlt.dm_green = g_dlt_dm;
}

void fruitMealPart::dm_retranslocate1 (float g_dlt_dm)
//     ===========================================================
{
    dlt.dm_green_retrans = g_dlt_dm;
}

void fruitMealPart::n_partition1 (double dltN)
//     ===========================================================
{
    dlt.n_green = dltN;
}

void fruitMealPart::n_retranslocate1 (float dltN)
//     ===========================================================
{
    dlt.n_retrans = dltN;
}

void fruitMealPart::update(void)
{
   plantPart::update();
   plantPart::updateDm();
   plantPart::updateN();
   plantPart::updateP();
}

void fruitMealPart::onHarvest(float /* cutting_height */, float remove_fr,
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


     dm_type.push_back (c.name);
     fraction_to_residue.push_back (fractToResidue);
     dlt_crop_dm.push_back (dlt_dm_harvest * gm2kg/sm2ha);
     dlt_dm_n.push_back    (dlt_n_harvest  * gm2kg/sm2ha);
     dlt_dm_p.push_back    (dlt_p_harvest  * gm2kg/sm2ha);
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
