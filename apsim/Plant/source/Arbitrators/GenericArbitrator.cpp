#include <stdio.h>
#include <math.h>
#include <stdexcept>
#include <string>
#include "PlantPart.h"

#include "Leaf/LeafPart.h"
#include "arbitrator.h"
#include "GenericArbitrator.h"

void genericArbitrator::zeroAllGlobals(void)
//=======================================================================================
   {
   fill_real_array (frac_leaf,0.0,max_table);
   fill_real_array (ratio_root_shoot, 0.0, max_table);
   }

void genericArbitrator::readSpeciesParameters(protocol::Component *system, vector<string> &sections)
//=======================================================================================
   {
   int numvals;
   scienceAPI.read("frac_leaf", frac_leaf, numvals, 0.0f, 1.0f);
   scienceAPI.read("ratio_root_shoot", ratio_root_shoot, numvals, 0.0f, 1000.0f);
   scienceAPI.read("partitionparts", PartitionParts);
   scienceAPI.read("partitionrules", PartitionRules);

   for (unsigned i = 0; i != PartitionRules.size(); i++)
      {
      vector <float> empty;
      if (PartitionRules[i] == "frac")
         {
         vector <float> frac;
         scienceAPI.read("frac_"+PartitionParts[i], frac, 0.0, 1.0);
         Fracs.push_back(frac);
         }
      else
         Fracs.push_back(empty);
      }
   }

void genericArbitrator::partitionDM(float dlt_dm,vector <plantPart *>& Parts, string FruitName)
//=======================================================================================
   //  Partitions new dm (assimilate) between plant components (g/m^2)
   // Root must be satisfied. The roots don't take any of the
   // carbohydrate produced - that is for tops only.  Here we assume
   // that enough extra was produced to meet demand. Thus the root
   // growth is not removed from the carbo produced by the model.
   {
   for (vector<plantPart *>::const_iterator part = Parts.begin(); part != Parts.end(); part++)
      (*part)->zeroDltDmGreen();

   float dm_remaining = dlt_dm;
   float dlt_dm_green_tot = 0.0;

   for (unsigned i = 0; i != PartitionParts.size(); i++)
      {

      plantPart *Part = FindPart(Parts, PartitionParts[i]);
      if (Part == NULL)
         throw runtime_error("Unknown Part "+PartitionParts[i]);

      Part->zeroDltDmGreen();
      if (PartitionRules[i] == "magic")
         {
         // root:shoot ratio of new dm
         float  c_ratio_root_shoot = ratio_root_shoot[(int)plant->getStageNumber()-1];
         Part->giveDmGreen(c_ratio_root_shoot * dlt_dm);
         }
      else
         {
         float uptake;
         if (PartitionRules[i] == "demand")
            uptake = min(Part->dmGreenDemand(), dm_remaining);
         else if (PartitionRules[i] == "frac")
            {
            // fraction of remaining dm allocated to this part
            float frac = Fracs[i][(int)plant->getStageNumber()-1];
            uptake = min(frac * dm_remaining,Part->dmGreenDemand());
            }
         else if (PartitionRules[i] == "remainder")
            uptake = dm_remaining;
         else
            throw runtime_error("Unknown Partition Rule "+PartitionRules[i]);

         Part->giveDmGreen(uptake);
         dm_remaining = dm_remaining - uptake;
         dlt_dm_green_tot = dlt_dm_green_tot + Part->dltDmGreen();
         }
      }


   if (!reals_are_equal(dlt_dm_green_tot, dlt_dm, 1.0E-4))
       {
       string msg = "dlt_dm_green_tot mass balance is off: "
                    + ftoa(dlt_dm_green_tot, ".6")
                    + " vs "
                    + ftoa(dlt_dm, ".6");
       plant->warningError(msg.c_str());
      }
   }

float genericArbitrator::dltDMWhole(float dlt_dm)
//=======================================================================================
   {
   return ((1.0 + ratio_root_shoot[(int)plant->getStageNumber()-1]) * dlt_dm);
   }

////////////////End generic parts
