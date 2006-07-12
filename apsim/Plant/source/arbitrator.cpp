#include <stdio.h>
#include <string>
#include <stdexcept>
#include <ComponentInterface/Component.h>
#include <ComponentInterface/ApsimVariant.h>

#include "PlantComponent.h"
#include "PlantLibrary.h"
#include "Plant.h"
#include "PlantParts.h"
#include "Arbitrator.h"

Arbitrator* constructArbitrator(plantInterface *p, const string &type)
   {
   Arbitrator *object;
   if (type == "")
     object = new nullArbitrator(p);
   else if (type == "1")
     object = new genericArbitrator(p);
   else if (type == "2")
     object = new cerealArbitrator(p);
   else
     throw std::invalid_argument("Unknown arbitrator object '" + type + "'");

   return (object);
   }   
   
void genericArbitrator::zeroAllGlobals(void)
   {
   fill_real_array (frac_leaf,0.0,max_table);
   fill_real_array (ratio_root_shoot, 0.0, max_table);
   }
   
void genericArbitrator::readSpeciesParameters(protocol::Component *system, vector<string> &sections)
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
                                    plantPart *leafPart,
                                    plantPart *stemPart,
                                    plantPart *fruitPart) 
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
   rootPart->dlt.dm_green = c_ratio_root_shoot * dlt_dm;

   // now distribute the assimilate to plant parts
   if (fruitPart->dmGreenDemand () >= dlt_dm)
        {
        // reproductive demand exceeds supply - distribute assimilate to those parts only
        fruitPart->dlt.dm_green = dlt_dm;
        }
    else
        {
        // more assimilate than needed for reproductive parts
        // distribute to all parts

        // satisfy reproductive demands
        fruitPart->dlt.dm_green = fruitPart->dmGreenDemand();
        float dm_remaining = dlt_dm - fruitPart->dmGreenDemand();

        // distribute remainder to vegetative parts
        // fraction of remaining dm allocated to leaf
        float c_frac_leaf = frac_leaf[(int)plant->getStageNumber()-1];
        leafPart->dlt.dm_green = c_frac_leaf * dm_remaining;

        // limit the delta leaf area to maximum
        double dlt_dm_leaf_max = leafPart->dmGreenDemand();
        leafPart->dlt.dm_green = u_bound (leafPart->dlt.dm_green, dlt_dm_leaf_max);

        // everything else to stem
        dm_remaining -= leafPart->dlt.dm_green;
        stemPart->dlt.dm_green = dm_remaining;
        }

   // do mass balance check - roots are not included
   float dlt_dm_green_tot = /*rootPart->dlt.dm_green +*/
                              leafPart->dlt.dm_green +
                              stemPart->dlt.dm_green +
                              fruitPart->dlt.dm_green;

   if (!reals_are_equal(dlt_dm_green_tot, dlt_dm, 1.0E-4)) 
       {
       string msg = "dlt_dm_green_tot mass balance is off: "
                    + ftoa(dlt_dm_green_tot, ".6")
                    + " vs "
                    + ftoa(dlt_dm, ".6");
       plant->warningError(msg.c_str());
      }
   }

////////////////End generic parts
void cerealArbitrator::zeroAllGlobals(void)
   {
   fill_real_array (x_stage_no_partition, 0.0, max_table);
   fill_real_array (y_frac_leaf, 0.0, max_table);
   fill_real_array (y_ratio_root_shoot, 0.0, max_table);
   num_stage_no_partition = 0;
   }

void cerealArbitrator::readSpeciesParameters(protocol::Component *system, vector<string> &sections)
   {
   int numvals;
   system->readParameter (sections
                         ,"x_stage_no_partition"//, "()"
                         , x_stage_no_partition
                         , num_stage_no_partition
                         , 0.0, 20.0);

   system->readParameter (sections
                         ,"y_frac_leaf"//,  "()"
                         , y_frac_leaf, numvals
                         , 0.0, 1.0);
   system->readParameter (sections
                         ,"y_ratio_root_shoot"//, "()"
                         , y_ratio_root_shoot, numvals
                         , 0.0, 1000.0);
   }
   
void cerealArbitrator::partitionDM(float dlt_dm,
                                    plantPart *rootPart,
                                    plantPart *leafPart,
                                    plantPart *stemPart,
                                    plantPart *fruitPart) 
   // Parcel out dlt DM to all parts
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

   // now we get the root delta for all stages - partition scheme
   // specified in coeff file
   float ratio_root_shoot = linear_interp_real(plant->getStageNumber()
                                          ,x_stage_no_partition
                                          ,y_ratio_root_shoot
                                          ,num_stage_no_partition);

   rootPart->dlt.dm_green = ratio_root_shoot * dlt_dm;

   // now distribute the assimilate to plant parts
   if (fruitPart->dmGreenDemand () >= dlt_dm)
        {
        // reproductive demand exceeds supply - distribute assimilate to those parts only
        fruitPart->dlt.dm_green = dlt_dm;
        }
    else
        {
        // more assimilate than needed for reproductive parts
        // distribute to all parts

        // satisfy reproductive demands
        fruitPart->dlt.dm_green = fruitPart->dmGreenDemand();
        float dm_remaining = dlt_dm - fruitPart->dmGreenDemand();

        // distribute remainder to vegetative parts
        // fraction of remaining dm allocated to leaf
        // Interpolate leaf and pod fractions
        float frac_leaf = linear_interp_real(plant->getStageNumber()
                                   ,x_stage_no_partition
                                   ,y_frac_leaf
                                   ,num_stage_no_partition);

        leafPart->dlt.dm_green = frac_leaf * dm_remaining;

        // limit the delta leaf area to maximum
        double dlt_dm_leaf_max = leafPart->dmGreenDemand();
        leafPart->dlt.dm_green = u_bound (leafPart->dlt.dm_green, dlt_dm_leaf_max);

        // everything else to stem
        dm_remaining -= leafPart->dlt.dm_green;
        stemPart->dlt.dm_green = dm_remaining;
        }

   // do mass balance check - roots are not included
   float dlt_dm_green_tot = /*rootPart->dlt.dm_green +*/
                              leafPart->dlt.dm_green +
                              stemPart->dlt.dm_green +
                              fruitPart->dlt.dm_green;

   if (!reals_are_equal(dlt_dm_green_tot, dlt_dm, 1.0E-4)) 
       {
       string msg = "dlt_dm_green_tot mass balance is off: "
                    + ftoa(dlt_dm_green_tot, ".6")
                    + " vs "
                    + ftoa(dlt_dm, ".6");
       plant->warningError(msg.c_str());
      }
}

