#include <stdio.h>
#include <math.h>
#include <stdexcept>
#include <string>
#include "PlantPart.h"

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


   float dlt_dm_harvest = Green.DM()
                        + Senesced.DM();

   float dlt_n_harvest = Green.N()
                       + Senesced.N();

   float dlt_p_harvest = Green.P()
                       + Senesced.P();

   Green.Clear();
   Senesced.Clear();

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

