#include <stdio.h>
#include <string>
#include <stdexcept>
#include <ComponentInterface/Component.h>
#include <ComponentInterface/ApsimVariant.h>
#include <ComponentInterface/ScienceAPI.h>

#include "PlantComponent.h"
#include "PlantLibrary.h"
#include "Plant.h"
#include "PlantPart.h"
#include "Leaf/LeafPart.h"
#include "arbitrator.h"
#include "NullArbitrator.h"
#include "GenericArbitrator.h"

plantPart* Arbitrator::FindPart(vector <plantPart *>& Parts, string name)
   {
   for (vector<plantPart *>::const_iterator part = Parts.begin(); part != Parts.end(); part++)
      {
      if (Str_i_Eq((*part)->name(), name))
         return *part;
      }
   return NULL;
   }

Arbitrator* constructArbitrator(ScienceAPI& scienceAPI, plantInterface *p, const string &type)
//=======================================================================================
   {
   Arbitrator *object;
   if (type == "")
     object = new nullArbitrator(scienceAPI, p);
   else if (type == "1")
     object = new genericArbitrator(scienceAPI, p);
   else if (type == "2")
     object = new cerealArbitrator(scienceAPI, p);
   else if (type == "allometric")
     object = new allometricArbitrator(scienceAPI, p);
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
   scienceAPI.read("x_stage_no_partition", x_stage_no_partition, num_stage_no_partition, 0.0f, 20.0f);
   scienceAPI.read("y_frac_leaf", y_frac_leaf, numvals, 0.0f, 1.0f);
   scienceAPI.read("y_ratio_root_shoot", y_ratio_root_shoot, numvals, 0.0f, 1000.0f);
   }

void cerealArbitrator::partitionDM(float dlt_dm,   vector <plantPart *>& Parts, string FruitName)
//=======================================================================================
   // Parcel out dlt DM to all parts
   // Root must be satisfied. The roots don't take any of the
   // carbohydrate produced - that is for tops only.  Here we assume
   // that enough extra was produced to meet demand. Thus the root
   // growth is not removed from the carbo produced by the model.
   {
   cout << "here now";
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

void allometricArbitrator::onInit1(protocol::Component *system)
//=======================================================================================
   {
   Arbitrator::onInit1(system);
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
   ratio_stem_leaf.read(scienceAPI,
                "x_frac_leaf_stage", "(oCd)", 0.0, 5000.0,
                "y_stem_leaf_ratio", "()", 0.0, 1.0);

   ratio_root_shoot.read(scienceAPI,
                           "x_stage_no_partition", "()", 0.0, 20.0,
                           "y_ratio_root_shoot", "()", 0.0, 1000.0);

   scienceAPI.read("sla_min", SLAmin, 0.0f, 100000.0f);
   SLAmaxFn.read(scienceAPI,
                   "x_lai", "(mm2/mm2)", 0.0, 50.0,
                   "y_sla_max", "()", 0.0, 100000.0);
   }

void allometricArbitrator::partitionDM(float dlt_dm,vector <plantPart *>& Parts, string FruitName)
//=======================================================================================
   // Parcel out dlt DM to all parts
   // Root must be satisfied. The roots don't take any of the
   // carbohydrate produced - that is for tops only.  Here we assume
   // that enough extra was produced to meet demand. Thus the root
   // growth is not removed from the carbo produced by the model.
   {

   plantPart *rootPart = FindPart(Parts, "root");
   plantLeafPart *leafPart = dynamic_cast<plantLeafPart*> (FindPart(Parts, "leaf"));
   plantPart *stemPart= FindPart(Parts, "stem");
   plantPart *fruitPart= FindPart(Parts, FruitName);

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


   