#include "StdPlant.h"

#include "Leaf/Leaf.h"
#include "arbitrator.h"
#include "GenericArbitratorXY.h"

void genericArbitratorXY::zeroAllGlobals(void)
//=======================================================================================
   {
   //fill_real_array (ratio_root_shoot, 0.0, max_table);
   }

void genericArbitratorXY::readSpeciesParameters(protocol::Component *, vector<string> &)
//=======================================================================================
   {
   ratio_root_shoot.read (scienceAPI
                         ,"x_stage_no_partition", "()", 0.0, 100.0
                         ,"y_ratio_root_shoot","()",0.0,100.0);

   //scienceAPI.read("ratio_root_shoot", ratio_root_shoot, numvals, 0.0f, 1000.0f);
   scienceAPI.read("partitionparts", PartitionParts);
   scienceAPI.read("partitionrules", PartitionRules);

   for (unsigned i = 0; i != PartitionRules.size(); i++)
      {
      interpolationFunction empty;
      if (PartitionRules[i] == "frac")
         {
         interpolationFunction frac;
         frac.read(scienceAPI
                  ,"x_stage_no_partition", "()", 0.0, 100.0
                  ,"y_frac_"+PartitionParts[i],"()",0.0,1.0);
         Fracs.push_back(frac);
         }
      else
         Fracs.push_back(empty);
      }
   }

void genericArbitratorXY::partitionDM(float dlt_dm,vector <plantPart *>& Parts, string FruitName)
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
         float  c_ratio_root_shoot = ratio_root_shoot.value(plant->getStageNumber());
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
            float frac = Fracs[i].value(plant->getStageNumber());
            uptake = min(frac * dm_remaining,Part->dmGreenDemand());
            }
         else if (PartitionRules[i] == "remainder")
            uptake = dm_remaining;
         else
            throw runtime_error("Unknown Partition Rule "+PartitionRules[i]);

         Part->giveDmGreen(uptake);
         dm_remaining = dm_remaining - uptake;
         dlt_dm_green_tot = dlt_dm_green_tot + Part->dltDmUptake();
         }
      }


   if (!reals_are_equal(dlt_dm_green_tot, dlt_dm, 1.0E-4))
       {
       string msg = "dlt_dm_green_tot mass balance is off: "
                    + ftoa(dlt_dm_green_tot, ".6")
                    + " vs "
                    + ftoa(dlt_dm, ".6");
       scienceAPI.warning(msg);
      }
   }

float genericArbitratorXY::dltDMWhole(float dlt_dm)
//=======================================================================================
   {
   return ((1.0 + ratio_root_shoot[(int)plant->getStageNumber()-1]) * dlt_dm);
   }

////////////////End generic parts
