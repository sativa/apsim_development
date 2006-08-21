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
#include "StemPart.h"
using namespace std;

void plantStemPart::onHarvest(float cutting_height, float remove_fr,
                              vector<string> &dm_type,
                              vector<float> &dlt_crop_dm,
                              vector<float> &dlt_dm_n,
                              vector<float> &dlt_dm_p,
                              vector<float> &fraction_to_residue)
//=======================================================================================
// Quite stem specific...
   {
   float fractToResidue = 1.0 - remove_fr;

   // Some biomass is removed according to harvest height
   float fr_height = divide (cutting_height,Height, 0.0);

   float retain_fr_green, retain_fr_sen, retain_fr_dead;
   if (c.fr_remain.isInitialised())
      retain_fr_green = c.fr_remain.value(fr_height);
   else
      retain_fr_green = 0.0;

   retain_fr_sen  = retain_fr_green;
   retain_fr_dead = retain_fr_green;

   float chop_fr_green = (1.0 - retain_fr_green);
   float chop_fr_dead = (1.0 - retain_fr_dead);
   float chop_fr_sen = (1.0 - retain_fr_sen);

   float dlt_dm_harvest = dmDead() * chop_fr_dead
                        + dmGreen() * chop_fr_green
                        + dmSenesced() * chop_fr_sen;

   float dlt_n_harvest = nDead() * chop_fr_dead
                       + nGreen() * chop_fr_green
                       + nSenesced() * chop_fr_sen;

   float dlt_p_harvest = pDead() * chop_fr_dead
                       + pGreen() * chop_fr_green
                       + pSenesced() * chop_fr_sen;

   DMDead *= retain_fr_dead;
   DMSenesced *= retain_fr_sen;
   DMGreen *= retain_fr_green;

   NDead *= retain_fr_dead;
   NSenesced *= retain_fr_sen;
   NGreen *= retain_fr_green;

   PDead *= retain_fr_dead;
   PSen *= retain_fr_sen;
   PGreen *= retain_fr_green;

   Height = l_bound(cutting_height, 1.0);

   dm_type.push_back(c.name);
   fraction_to_residue.push_back(fractToResidue);
   dlt_crop_dm.push_back(dlt_dm_harvest * gm2kg/sm2ha);
   dlt_dm_n.push_back(dlt_n_harvest * gm2kg/sm2ha);
   dlt_dm_p.push_back(dlt_p_harvest * gm2kg/sm2ha);
   }

void plantStemPart::update(void)
{
    plantPart::update();
}

void plantStemPart::removeBiomass2(float )
{
    Height *= (1.0 - divide(dlt.dm_green, DMGreen, 0.0));
    Width *= (1.0 - divide(dlt.dm_green, DMGreen, 0.0));
}

