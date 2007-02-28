#include <stdio.h>
#include <string>
#include <stdexcept>
#include <ComponentInterface/Component.h>
#include <ComponentInterface/ApsimVariant.h>

#include "PlantComponent.h"
#include "PlantLibrary.h"
#include "Plant.h"
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
   system->readParameter (sections
                         ,"frac_leaf"//,  "()"
                         , frac_leaf, numvals
                         , 0.0, 1.0);

   system->readParameter (sections
                         ,"ratio_root_shoot"//, "()"
                         , ratio_root_shoot, numvals
                         , 0.0, 1000.0);
   }

void genericArbitrator::partitionDM(float dlt_dm,
                                    plantPart *rootPart,
                                    plantLeafPart *leafPart,
                                    plantPart *stemPart,
                                    plantPart *fruitPart)
//=======================================================================================
   //  Partitions new dm (assimilate) between plant components (g/m^2)
   // Root must be satisfied. The roots don't take any of the
   // carbohydrate produced - that is for tops only.  Here we assume
   // that enough extra was produced to meet demand. Thus the root
   // growth is not removed from the carbo produced by the model.
   {
   // first we zero all plant component deltas
   rootPart->zeroDltDmGreen();
   leafPart->zeroDltDmGreen();
   stemPart->zeroDltDmGreen();
   fruitPart->zeroDltDmGreen();


    // root:shoot ratio of new dm
   float  c_ratio_root_shoot = ratio_root_shoot[(int)plant->getStageNumber()-1];
   rootPart->giveDmGreen(c_ratio_root_shoot * dlt_dm);

   // now distribute the assimilate to plant parts
   if (fruitPart->dmGreenDemand () >= dlt_dm)
        {
        // reproductive demand exceeds supply - distribute assimilate to those parts only
        fruitPart->giveDmGreen( dlt_dm );
        }
    else
        {
        // more assimilate than needed for reproductive parts
        // distribute to all parts

        // satisfy reproductive demands
        float uptake = fruitPart->giveDmGreen( fruitPart->dmGreenDemand() );
        float dm_remaining = dlt_dm - uptake;

        // distribute remainder to vegetative parts
        // fraction of remaining dm allocated to leaf
        float c_frac_leaf = frac_leaf[(int)plant->getStageNumber()-1];

        // limit the delta leaf area to maximum
        float dLeaf = u_bound(c_frac_leaf * dm_remaining,
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
                              fruitPart->dltDmGreen();

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
       plant->warningError(msg.c_str());
      }
   }

float genericArbitrator::dltDMWhole(float dlt_dm)
//=======================================================================================
   {
   return ((1.0 + ratio_root_shoot[(int)plant->getStageNumber()-1]) * dlt_dm);
   }

////////////////End generic parts
