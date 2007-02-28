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
#include "NullArbitrator.h"
#include "GenericArbitrator.h"

Arbitrator* constructArbitrator(plantInterface *p, const string &type)
//=======================================================================================
   {
   Arbitrator *object;
   if (type == "")
     object = new nullArbitrator(p);
   else if (type == "1")
     object = new genericArbitrator(p);
   else if (type == "2")
     object = new cerealArbitrator(p);
   else if (type == "allometric")
     object = new allometricArbitrator(p);
   else
     throw std::invalid_argument("Unknown arbitrator object '" + type + "'");

   return (object);
   }

void cerealArbitrator::zeroAllGlobals(void)
//=======================================================================================
   {
   fill_real_array (x_stage_no_partition, 0.0, max_table);
   fill_real_array (y_frac_leaf, 0.0, max_table);
   fill_real_array (y_ratio_root_shoot, 0.0, max_table);
   num_stage_no_partition = 0;
   }

void cerealArbitrator::readSpeciesParameters(protocol::Component *system, vector<string> &sections)
//=======================================================================================
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
                                    plantLeafPart *leafPart,
                                    plantPart *stemPart,
                                    plantPart *fruitPart)
//=======================================================================================
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
        float frac_leaf = linear_interp_real(plant->getStageNumber()
                                   ,x_stage_no_partition
                                   ,y_frac_leaf
                                   ,num_stage_no_partition);

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

float cerealArbitrator::dltDMWhole(float dlt_dm)
//=======================================================================================
   {
   return ((1.0 + linear_interp_real(plant->getStageNumber()
                                          ,x_stage_no_partition
                                          ,y_ratio_root_shoot
                                          ,num_stage_no_partition)) * dlt_dm);
   }

//////////allometricArbitrator
void allometricArbitrator::zeroAllGlobals(void)
//=======================================================================================
   {
   SLAmin  = 0.0;
   }

void allometricArbitrator::doRegistrations(protocol::Component *system)
//=======================================================================================
   {
   Arbitrator::doRegistrations(system);
   system->addGettableVar("SLAcalc", SLAcalc, "mm^2/g", "SLA of new leaf dm");
   }
void allometricArbitrator::undoRegistrations(protocol::Component *system)
//=======================================================================================
   {
   system->removeGettableVar("SLAcalc");
   }
void allometricArbitrator::readSpeciesParameters(protocol::Component *system, vector<string> &sections)
//=======================================================================================
   {
   ratio_stem_leaf.search(system, sections,
                "x_frac_leaf_stage", "(oCd)", 0.0, 5000.0,
                "y_stem_leaf_ratio", "()", 0.0, 1.0);

   ratio_root_shoot.search(system, sections,
                           "x_stage_no_partition", "()", 0.0, 20.0,
                           "y_ratio_root_shoot", "()", 0.0, 1000.0);

   system->readParameter (sections,
                          "sla_min",//, "(mm^2/g)",
                          SLAmin,
                          0.0, 100000.0);
   SLAmaxFn.search(system, sections,
                   "x_lai", "(mm2/mm2)", 0.0, 50.0,
                   "y_sla_max", "()", 0.0, 100000.0);
   }

void allometricArbitrator::partitionDM(float dlt_dm,
                                    plantPart *rootPart,
                                    plantLeafPart *leafPart,
                                    plantPart *stemPart,
                                    plantPart *fruitPart)
//=======================================================================================
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
   SLAcalc = 0.0;
   float SLAmax = SLAmaxFn[leafPart->getLAI()];

   // now we get the root delta for all stages - partition scheme
   // specified in coeff file
   rootPart->giveDmGreen(ratio_root_shoot[plant->getStageNumber()] * dlt_dm);

   // now distribute the assimilate to plant parts
   if (fruitPart->dmGreenDemand () >= dlt_dm)
        {
        // reproductive demand exceeds supply - distribute assimilate to those parts only
        fruitPart->giveDmGreen(dlt_dm);
        }
    else
        {
        // more assimilate than needed for reproductive parts: find remainder
        float uptake = fruitPart->giveDmGreen(fruitPart->dmGreenDemand());
        float dm_remaining = dlt_dm - uptake;                           // g/m^2

        // Distribute remainder to vegetative parts

        // Find potentials
        float dltLeafAreaPot = leafPart->dltLeafAreaPot();                               // Sink size of leaves (mm^2/plant)
        float dltStemPot =  dltLeafAreaPot *
                              ratio_stem_leaf[plant->getStageNumber()] *
                                  plant->getPlants();                                    // g/m^2
        float dltLeafPot = l_bound(dm_remaining - dltStemPot, 0.0);                      // g/m^2

        if (dltLeafPot > 0.0)
           // Stem:Leaf ratio is valid
           SLAcalc = divide(dltLeafAreaPot, dltLeafPot / plant->getPlants(), 0.0);       // mm^2/g
        else
           SLAcalc = SLAmax + 1.0;

        //fprintf(stdout,"ratio = %f, dltLeafAreaPot=%f, dlt_dm=%f, dltStemPot=%f dltLeafPot=%f SLAcalc=%f\n",ratio_stem_leaf[plant->getStageNumber()], dltLeafAreaPot,dlt_dm, dltStemPot, dltLeafPot, SLAcalc);

        // Determine SD state
        if (SLAcalc <= SLAmin)
           {
           // Supply > demand
           uptake = leafPart->giveDmGreen(min(dm_remaining, divide(dltLeafAreaPot, SLAmin, 0.0) * plant->getPlants() ));
           dm_remaining -= uptake;
           uptake = stemPart->giveDmGreen(min(dm_remaining, dltStemPot));
           dm_remaining -= uptake;
           /*extra*/stemPart->giveDmGreen(dm_remaining);
           dm_remaining = 0.0;
           //fprintf(stdout,"S>D:dLeaf=%f,dStem=%f\n", leafPart->dlt.dm_green, stemPart->dlt.dm_green);
           }
        else if (SLAcalc <= SLAmax)
           {
           // Supply ~ demand
           uptake = leafPart->giveDmGreen(min(dm_remaining, divide(dltLeafAreaPot, SLAcalc, 0.0) * plant->getPlants() ));
           dm_remaining -= uptake;
           uptake = stemPart->giveDmGreen(min(dm_remaining, dltStemPot));
           dm_remaining -= uptake;
           //fprintf(stdout,"S=D:dLeaf=%f,dStem=%f\n", leafPart->dlt.dm_green, stemPart->dlt.dm_green);
           }
        else //(SLAcalc > SLAmax)
           {
           // Supply < demand
           float lf = divide(dltStemPot,
                             divide(dltLeafAreaPot, SLAmax, 0.0) * plant->getPlants()  + dltStemPot,
                             0.0);
           dm_remaining -= (leafPart->giveDmGreen(dm_remaining * (1.0 - lf)) +
                            stemPart->giveDmGreen(dm_remaining * lf) );
           //fprintf(stdout,"S<D:lf=%f,dLeaf=%f,dStem=%f\n", lf, leafPart->dlt.dm_green, stemPart->dlt.dm_green);
           }
       if (dm_remaining > 1.0E-4) {throw std::runtime_error("Unallocated DM left in allometricArbitrator::partitionDM");}
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
                    + ftoa(dlt_dm, ".6");
      plant->warningError(msg.c_str());
      }
}
float allometricArbitrator::dltDMWhole(float dlt_dm)
//=======================================================================================
   {
   return ((1.0 + ratio_root_shoot[plant->getStageNumber()]) * dlt_dm);
   }


