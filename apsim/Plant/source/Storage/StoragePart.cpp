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
#include <ComponentInterface/datatypes.h>
#include <ComponentInterface/Messages.h>
#include <ComponentInterface/MessageDataExt.h>
#include <ApsimShared/ApsimComponentData.h>
#include <ApsimShared/FStringExt.h>
#include <general/string_functions.h>
#include <ComponentInterface/ScienceAPI.h>

#include "PlantInterface.h"
#include "PlantLibrary.h"
#include "Plant.h"
#include "StoragePart.h"
using namespace std;


StoragePart* StoragePart::construct(ScienceAPI& scienceAPI, plantInterface *p, const string &name)
//=======================================================================================
// Setup correct storage part model for user-defined type
   {
   string type;
   scienceAPI.readOptional("storage_part", type);
   if (type == "generic")
      return new StoragePart(scienceAPI, p, name);
   else
      return NULL;
   }

void StoragePart::onHarvest(float cutting_height, float remove_fr,
                              vector<string> &dm_type,
                              vector<float> &dlt_crop_dm,
                              vector<float> &dlt_dm_n,
                              vector<float> &dlt_dm_p,
                              vector<float> &fraction_to_residue)
//=======================================================================================
// Quite stem specific...
   {
   float fractToResidue = 0.0;


   float dlt_dm_harvest = dmDead()
                        + dmGreen()
                        + dmSenesced();

   float dlt_n_harvest = nDead()
                       + nGreen()
                       + nSenesced();

   float dlt_p_harvest = pDead()
                       + pGreen()
                       + pSenesced();

   DMDead = 0.0;
   DMSenesced = 0.0;
   DMGreen = 0.0;

   NDead = 0.0;
   NSenesced = 0.0;
   NGreen = 0.0;

   PDead = 0.0;
   PSen = 0.0;
   PGreen = 0.0;

   dm_type.push_back(c.name);
   fraction_to_residue.push_back(fractToResidue);
   dlt_crop_dm.push_back(dlt_dm_harvest * gm2kg/sm2ha);
   dlt_dm_n.push_back(dlt_n_harvest * gm2kg/sm2ha);
   dlt_dm_p.push_back(dlt_p_harvest * gm2kg/sm2ha);
   }

void StoragePart::update(void)
//=======================================================================================
   {
   plantPart::update();
   }

void StoragePart::removeBiomass2(float )
//=======================================================================================
   {

   }
