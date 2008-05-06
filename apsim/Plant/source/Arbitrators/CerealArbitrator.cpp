#include "StdPlant.h"

#include "Leaf/Leaf.h"
#include "arbitrator.h"
#include "CerealArbitrator.h"
#include "Phenology/Phenology.h"

void cerealArbitrator::zeroAllGlobals(void)
//=======================================================================================
   {
   num_stage_no_partition = 0;
   }

void cerealArbitrator::readSpeciesParameters(protocol::Component *, vector<string> &)
//=======================================================================================
   {
   y_frac_leaf.read(scienceAPI, "x_stage_no_partition", "", 0.0f, 20.0f,
                                "y_frac_leaf", "", 0.0f, 1.0f);
   y_ratio_root_shoot.read(scienceAPI, "x_stage_no_partition", "", 0.0f, 20.0f,
                                       "y_ratio_root_shoot", "", 0.0f, 1000.0f);
   }

void cerealArbitrator::partitionDM(float dlt_dm,   vector <plantPart *>& Parts, string FruitName)
//=======================================================================================
   // Parcel out dlt DM to all parts
   // Root must be satisfied. The roots don't take any of the
   // carbohydrate produced - that is for tops only.  Here we assume
   // that enough extra was produced to meet demand. Thus the root
   // growth is not removed from the carbo produced by the model.
   {

   plantPart *rootPart = FindPart(Parts, "root");
   plantPart *leafPart = FindPart(Parts, "leaf");
   plantPart *stemPart = FindPart(Parts, "stem");
   plantPart *fruitPart = FindPart(Parts, FruitName);

   // first we zero all plant component deltas
   rootPart->zeroDltDmGreen();
   leafPart->zeroDltDmGreen();
   stemPart->zeroDltDmGreen();
   fruitPart->zeroDltDmGreen();

   // now we get the root delta for all stages - partition scheme
   // specified in coeff file
   float ratio_root_shoot = plant->phenology().doInterpolation(y_ratio_root_shoot);

   rootPart->giveDmGreen(ratio_root_shoot * dlt_dm);

   // now distribute the assimilate to plant parts
   if (fruitPart->dmGreenDemand () >= dlt_dm)
        {
        // reproductive demand exceeds supply - distribute assimilate to those parts only
        fruitPart->giveDmGreen(dlt_dm);
        }
    else
        {
        // more assimilate than needed for reproductive parts
        // distribute to all parts

        // satisfy reproductive demands
        float fruitDm = fruitPart->dmGreenDemand();
        float uptake = fruitPart->giveDmGreen(fruitDm);
        float dm_remaining = dlt_dm - uptake;

        // distribute remainder to vegetative parts
        // fraction of remaining dm allocated to leaf
        // Interpolate leaf and pod fractions

        float frac_leaf = plant->phenology().doInterpolation(y_frac_leaf);

        // limit the delta leaf area to maximum
        float dLeaf = u_bound (frac_leaf * dm_remaining,
                               leafPart->dmGreenDemand());

        uptake = leafPart->giveDmGreen(dLeaf);

        // everything else to stem
        dm_remaining -= uptake;
        stemPart->giveDmGreen(dm_remaining);
        }

   // do mass balance check - roots are not included
   float dlt_dm_green_tot = /*rootPart->dltDmGreen() +*/
                              leafPart->dltDmGreen() +
                              stemPart->dltDmGreen() +
                              fruitPart->dltDmUptake();

   if (!reals_are_equal(dlt_dm_green_tot, dlt_dm, 1.0E-4))
       {
       string msg = "dlt_dm_green_tot mass balance is off: "
                    + ftoa(dlt_dm_green_tot, ".6")
                    + " vs "
                    + ftoa(dlt_dm, ".6")
                    + "\nrootPart="  + ftoa(rootPart->dltDmGreen(), ".6")
                    + "\nleafPart="  + ftoa(leafPart->dltDmGreen(), ".6")
                    + "\nstemPart="  + ftoa(stemPart->dltDmGreen(), ".6")
                    + "\nfruitPart=" + ftoa(fruitPart->dltDmGreen(), ".6")
                    ;
       scienceAPI.warning(msg);
      }
}

float cerealArbitrator::dltDMWhole(float dlt_dm)
//=======================================================================================
   {
   return ((1.0 + plant->phenology().doInterpolation(y_ratio_root_shoot)) * dlt_dm);
   }
