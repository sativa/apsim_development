
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

     float dlt_dm_harvest = g.dm_dead     * chop_fr_dead
                          + g.dm_green    * chop_fr_green
                          + g.dm_senesced * chop_fr_sen;

     float dlt_n_harvest = g.n_dead     * chop_fr_dead
                         + g.n_green    * chop_fr_green
                         + g.n_senesced * chop_fr_sen;

     float dlt_p_harvest = g.p_dead  * chop_fr_dead
                         + g.p_green * chop_fr_green
                         + g.p_sen   * chop_fr_sen;

     g.dm_dead     *= retain_fr_dead;
     g.dm_senesced *= retain_fr_sen;
     g.dm_green    *= retain_fr_green;


     g.n_dead     *= retain_fr_dead;
     g.n_senesced *= retain_fr_sen;
     g.n_green    *= retain_fr_green;

     g.p_dead  *= retain_fr_dead;
     g.p_sen   *= retain_fr_sen;
     g.p_green *= retain_fr_green;


     dm_type.push_back(c.name);
     fraction_to_residue.push_back(fractToResidue);
     dlt_crop_dm.push_back (dlt_dm_harvest * gm2kg/sm2ha);
     dlt_dm_n.push_back    (dlt_n_harvest  * gm2kg/sm2ha);
     dlt_dm_p.push_back    (dlt_p_harvest  * gm2kg/sm2ha);

}

void fruitOilPart::onKillStem(void)
{
       g.dm_dead += g.dm_green + g.dm_senesced;
       g.dm_green = 0.0;
       g.dm_senesced = 0.0;

       g.n_dead += g.n_green + g.n_senesced;
       g.n_green = 0.0;
       g.n_senesced = 0.0;

       g.p_dead += g.p_green + g.p_sen;
       g.p_green = 0.0;
       g.p_sen = 0.0;
}

void fruitOilPart::onFlowering(void)
{  // do nothing
}

// set the minimum weight of part; used for retranslocation to grain
void fruitOilPart::onStartGrainFill(void)
{  // do nothing
}

