#include <stdio.h>
#include <math.h>
#include <map>
#include <string>
#include <stdexcept>

#include <boost/function.hpp>
#include <boost/bind.hpp>

#include <ComponentInterface/MessageDataExt.h>
#include <ComponentInterface/Type.h>
#include <ComponentInterface/ApsimVariant.h>
#include <ComponentInterface/Component.h>
#include <ComponentInterface/datatypes.h>
#include <ComponentInterface/ScienceAPI.h>
#include <ApsimShared/ApsimComponentData.h>
#include <ApsimShared/FStringExt.h>
#include <general/string_functions.h>

#include "PlantInterface.h"
#include "PlantLibrary.h"
#include "Plant.h"
#include "PlantPart.h"
#include "RootPart.h"
#include "Utility/PlantUtility.h"
#include "RootGrowthOption1.h"
#include "RootGrowthOption2.h"

using namespace std;

static const char* IncorpFOMType =    "<type name = \"IncorpFOM\">" \
                                      "   <field name=\"dlt_fom_type_name\" kind=\"string\"/>" \
                                      "   <field name=\"dlt_fom_type_numbytes\" kind=\"integer4\"/>" \
                                      "   <field name=\"dlt_fom_type_code\" kind=\"integer4\"/>" \
                                      "   <field name=\"dlt_fom_type_isarray\" kind=\"boolean\"/>" \
                                      "   <field name=\"dlt_fom_type_value\" kind=\"string\"/>" \

                                      "   <field name=\"dlt_fom_wt_name\" kind=\"string\"/>" \
                                      "   <field name=\"dlt_fom_wt_numbytes\" kind=\"integer4\"/>" \
                                      "   <field name=\"dlt_fom_wt_code\" kind=\"integer4\"/>" \
                                      "   <field name=\"dlt_fom_wt_isarray\" kind=\"boolean\"/>" \
                                      "   <field name=\"dlt_fom_wt_value\" kind=\"single\" array=\"T\"/>" \

                                      "   <field name=\"dlt_fom_n_name\" kind=\"string\"/>" \
                                      "   <field name=\"dlt_fom_n_numbytes\" kind=\"integer4\"/>" \
                                      "   <field name=\"dlt_fom_n_code\" kind=\"integer4\"/>" \
                                      "   <field name=\"dlt_fom_n_isarray\" kind=\"boolean\"/>" \
                                      "   <field name=\"dlt_fom_n_value\" kind=\"single\" array=\"T\"/>" \

                                      "   <field name=\"dlt_fom_p_name\" kind=\"string\"/>" \
                                      "   <field name=\"dlt_fom_p_numbytes\" kind=\"integer4\"/>" \
                                      "   <field name=\"dlt_fom_p_code\" kind=\"integer4\"/>" \
                                      "   <field name=\"dlt_fom_p_isarray\" kind=\"boolean\"/>" \
                                      "   <field name=\"dlt_fom_p_value\" kind=\"single\" array=\"T\"/>" \
                                      "</type>";

static const char* floatArrayType =   "<type kind=\"single\" array=\"T\"/>";

plantRootPart::plantRootPart(ScienceAPI& scienceAPI, plantInterface *p, const string &name)
   : plantPart(scienceAPI, p, name)
//=======================================================================================
// Constructor
   {
   sw_dep_id = 0;
   dlt_sw_dep_id = 0;
   incorp_fom_ID = 0;

   zeroSoil();
   zeroAllGlobals();
   }

plantRootPart* constructRootPart(ScienceAPI& scienceAPI, plantInterface *p, const string &type, const string &name)
//=======================================================================================
// Setup correct root model for user-defined type
   {
   if (type == "Jones+RitchieGrowthPattern")
     return new rootGrowthOption2(scienceAPI, p, name);
   // default:
   return new rootGrowthOption1(scienceAPI, p, name);
   }


void plantRootPart::zeroAllGlobals(void)
//=======================================================================================
// Zero all global values
   {
   plantPart::zeroAllGlobals();
   root_depth            = 0.0;
   initialRootDepth = 0.0;
   rootDieBackFraction = 0.0;
   specificRootLength = 0.0;

   fill_real_array (root_length , 0.0, max_layer);
   fill_real_array (root_length_dead, 0.0, max_layer);
   n_conc_min = n_conc_crit = n_conc_max = 0.0;

   num_sw_avail_ratio = 0;
   fill_real_array (x_sw_avail_ratio , 0.0, max_table);
   fill_real_array (y_swdef_pheno , 0.0, max_table);

      fill_real_array (x_sw_avail_ratio_flower, 0.0, max_table);
      fill_real_array (y_swdef_pheno_flower, 0.0, max_table);
      num_sw_avail_ratio_flower = 0;

      fill_real_array (x_sw_avail_ratio_grainfill, 0.0, max_table);
      fill_real_array (y_swdef_pheno_grainfill, 0.0, max_table);
      num_sw_avail_ratio_grainfill = 0;

      num_sw_demand_ratio = 0;
      fill_real_array (x_sw_demand_ratio , 0.0, max_table);
      fill_real_array (y_swdef_leaf , 0.0, max_table);

      num_sw_avail_fix = 0;
      fill_real_array (x_sw_avail_fix , 0.0, max_table);
      fill_real_array (y_swdef_fix , 0.0, max_table);

      fill_real_array (oxdef_photo , 0.0, max_table);
      fill_real_array (oxdef_photo_rtfr, 0.0, max_table);
      num_oxdef_photo = 0;

      fill_real_array (kl, 0.0, max_layer);
      kl_ub = 0.0;


      fill_real_array (sw_avail_pot, 0.0, max_layer);
      fill_real_array (sw_avail, 0.0, max_layer);
      fill_real_array (sw_supply , 0.0, max_layer);
      fill_real_array (dlt_sw_dep, 0.0, max_layer);
      fill_real_array (no3gsm , 0.0, max_layer);
      fill_real_array (nh4gsm , 0.0, max_layer);
      fill_real_array (dlt_no3gsm, 0.0, max_layer);
      fill_real_array (dlt_nh4gsm, 0.0, max_layer);

      uptake_source = "";


   }

void plantRootPart::zeroSoil(void)
//=======================================================================================
// Zero all Soil variables
   {
      fill_real_array (dlayer , 0.0, max_layer);
      fill_real_array (ll15_dep , 0.0, max_layer);
      fill_real_array (dul_dep , 0.0, max_layer);
      fill_real_array (sat_dep , 0.0, max_layer);
      fill_real_array (bd , 0.0, max_layer);
      fill_real_array (sw_dep , 0.0, max_layer);
      fill_real_array (ll_dep, 0.0, max_layer);
      num_layers = 0;
      sw_dep_ub = 0.0;
      sw_dep_lb = 0.0;

      sw_lb = 0.0;
      sw_ub = 0.0;
      xf.clear();
      //for (unsigned int layer = 1; layer != max_layer; layer++)
      //xf.push_back(0.0);

   }

void plantRootPart::zeroDeltas(void)
//=======================================================================================
// Zero all daily deltas
   {
   plantPart::zeroDeltas();
   dltRootDepth = 0.0;
   setTo(dltRootLength, (float)0.0);
   setTo(dltRootLengthSenesced, (float)0.0);
   setTo(dltRootLengthDead, (float)0.0);

   fill_real_array (sw_avail , 0.0, max_layer);
   fill_real_array (sw_avail_pot , 0.0, max_layer);
   fill_real_array (sw_supply , 0.0, max_layer);
   fill_real_array (dlt_sw_dep , 0.0, max_layer);
   // fill_real_array (dlt_no3gsm , 0.0, max_layer);  required for output!!!
   // fill_real_array (dlt_nh4gsm , 0.0, max_layer);

   }

void plantRootPart::doRegistrations(protocol::Component *system)
//=======================================================================================
// Register data with the communications protocol
   {
   plantPart::doRegistrations(system);
   system->addGettableVar("root_depth",
               root_depth, "mm", "depth of roots");

   setupGetFunction(system, "root_length", protocol::DTsingle, true,
                    &plantRootPart::get_root_length,
                    "mm/mm^2", "Root length");

   setupGetFunction(system, "root_length_dead", protocol::DTsingle, true,
                    &plantRootPart::get_root_length_dead, "mm/mm^2", "Dead root length");

   setupGetFunction(system, "rlv", protocol::DTsingle, true,
                    &plantRootPart::get_rlv, "mm/mm^3", "Root length density");

   setupGetFunction(system, "rld", protocol::DTsingle, true,
                    &plantRootPart::get_rlv, "mm/mm^3", "Root length density");

   setupGetFunction(system, "kl", protocol::DTsingle, true,
                    &plantRootPart::get_kl, "", "Root Water Uptake Parameter");

   setupGetFunction(system, "xf", protocol::DTsingle, true,
                    &plantRootPart::get_xf, "", "Root Exploration Factor");

   incorp_fom_ID = plant->getComponent()->addRegistration(RegistrationType::event,
                                                          "incorp_fom", IncorpFOMType,
                                                          "", "");
   }

void plantRootPart::readConstants (protocol::Component *system, const string &section)
//=======================================================================================
// Read Constants
   {
   plantPart::readConstants(system, section);

   scienceAPI.read("sw_ub", sw_ub, 0.0f, 1.0f);
   scienceAPI.read("sw_lb", sw_lb, 0.0f, 1.0f);
   scienceAPI.read("sw_dep_ub", sw_dep_ub, 0.0f, 10000.0f);
   scienceAPI.read("sw_dep_lb", sw_dep_lb, 0.0f, 10000.0f);
}

void plantRootPart::readSpeciesParameters(protocol::Component *system, vector<string> &sections)
//=======================================================================================
// Read species-specific parameters
   {
   plantPart::readSpeciesParameters(system, sections);

   scienceAPI.read("kl_ub",kl_ub, 0.0f, 1.0f);
   scienceAPI.read("specific_root_length", specificRootLength,0.0f, 1000000.0f);
   scienceAPI.read("n_conc_crit_root", n_conc_crit, 0.0f, 100.0f);
   scienceAPI.read("n_conc_max_root", n_conc_max, 0.0f, 100.0f);

   scienceAPI.read("n_conc_min_root", n_conc_min, 0.0f, 100.0f);
   scienceAPI.read("initial_root_depth", initialRootDepth, 0.0f, 1000.0f);
   scienceAPI.read("specific_root_length", specificRootLength, 0.0f, 1000000.0f);
   scienceAPI.read("root_die_back_fr", rootDieBackFraction, 0.0f, 0.99f);

   rel_root_rate.read(scienceAPI,
                         "x_plant_rld", "()", 0.0, 0.1,
                         "y_rel_root_rate", "()", 0.001, 1.0);

   sw_fac_root.read(scienceAPI,
                         "x_sw_ratio", "()", 0.0, 100.,
                         "y_sw_fac_root", "()", 0.0, 100.0);

   rel_root_advance.read(scienceAPI,
                         "x_temp_root_advance", "(oc)", -10.0, 60.,
                         "y_rel_root_advance", "()", 0.0, 1.0);

   root_depth_rate.read(scienceAPI,
                         "stage_code_list", "()", 0.0, 100.0,
                         "root_depth_rate", "(mm/day)", 0.0, 1000.0);

   ws_root_fac.read(scienceAPI,
                         "x_ws_root", "()", 0.0, 1.0,
                         "y_ws_root_fac", "()", 0.0, 1.0);

   scienceAPI.read("x_sw_avail_ratio", x_sw_avail_ratio, num_sw_avail_ratio, 0.0f, 100.0f);
   scienceAPI.read("y_swdef_pheno", y_swdef_pheno, num_sw_avail_ratio, 0.0f, 100.0f);
   scienceAPI.read("x_sw_avail_ratio_flower", x_sw_avail_ratio_flower, num_sw_avail_ratio_flower, 0.0f, 1.0f);
   scienceAPI.read("y_swdef_pheno_flower", y_swdef_pheno_flower, num_sw_avail_ratio_flower, 0.0f, 5.0f);
   scienceAPI.read("x_sw_avail_ratio_grainfill", x_sw_avail_ratio_grainfill, num_sw_avail_ratio_grainfill, 0.0f, 1.0f);
   scienceAPI.read("y_swdef_pheno_grainfill", y_swdef_pheno_grainfill, num_sw_avail_ratio_grainfill, 0.0f, 5.0f);
   scienceAPI.read("x_sw_demand_ratio", x_sw_demand_ratio, num_sw_demand_ratio, 0.0f, 100.0f);
   scienceAPI.read("y_swdef_leaf", y_swdef_leaf, num_sw_demand_ratio, 0.0f, 100.0f);
   scienceAPI.read("x_sw_avail_fix", x_sw_avail_fix, num_sw_avail_fix, 0.0f, 100.0f);
   scienceAPI.read("y_swdef_fix", y_swdef_fix, num_sw_avail_fix, 0.0f, 100.0f);
   scienceAPI.read("oxdef_photo_rtfr", oxdef_photo_rtfr, num_oxdef_photo, 0.0f, 1.0f);
   scienceAPI.read("oxdef_photo", oxdef_photo, num_oxdef_photo, 0.0f, 1.0f);
   }


void plantRootPart::readRootParameters(protocol::Component *system, const char *section_name)
//=======================================================================================
// Read Rooting parameters
   {
    vector<float> ll ;   // lower limit of plant-extractable
                         // soil water for soil layer l
                         // (mm water/mm soil)
    float dep_tot, esw_tot;                      // total depth of soil & ll
    char  msg[200];

    system->writeString (" - reading root profile parameters");

    if (scienceAPI.readOptional("ll", ll, 0.0, sw_ub))
       {
       for (unsigned int layer = 0; layer != ll.size(); layer++)
          ll_dep[layer] = ll[layer]*dlayer[layer];

       if ((int)ll.size() != num_layers)
          throw std::runtime_error ("Size of LL array doesn't match soil profile.");
       }
    else
       {
       unsigned int id = system->addRegistration(RegistrationType::get,
                                                 "ll15", floatArrayType,
                                                 "", "");
       system->getVariable(id, ll, 0.0, sw_ub, true);
       if (ll.size() == 0)
          throw std::runtime_error("No Crop Lower Limit found");

       for (unsigned int i=0; i< ll.size(); i++) ll_dep[i] = ll[i]*dlayer[i];
       system->writeString ("   Using externally supplied Lower Limit (ll15)");
       }

   scienceAPI.read("xf", xf, 0.0f, 1.0f);
   if (xf.size() != (unsigned) num_layers)
       throw std::runtime_error ("Size of XF array doesn't match soil profile.");

   int num_kls = 0;
   scienceAPI.read("kl", kl, num_kls, 0.0f, kl_ub);
   if (num_kls != num_layers)
      throw std::runtime_error  ("Size of KL array doesn't match soil profile.");


   scienceAPI.readOptional("uptake_source", uptake_source);
   if (uptake_source == "")uptake_source = "calc";


    // report
    system->writeString ("                   Root Profile");
    system->writeString ("    -----------------------------------------------");
    system->writeString ("     Layer       Kl           Lower    Exploration");
    system->writeString ("     Depth     Factor         Limit      Factor  ");
    system->writeString ("     (mm)         ()        (mm/mm)       (0-1)");
    system->writeString ("    -----------------------------------------------");

    dep_tot = esw_tot = 0.0;
    for (int layer = 0; layer < num_layers; layer++)
       {
       sprintf (msg, "%9.1f%10.3f%15.3f%12.3f"
          , dlayer[layer]
          , kl[layer]
          , divide(ll_dep[layer],dlayer[layer],0.0)
          , xf[layer]);
       system->writeString (msg);
       dep_tot += dlayer[layer];
       esw_tot += dul_dep[layer] - ll_dep[layer];
       }
    system->writeString ("    -----------------------------------------------");
    sprintf (msg
          , "    Extractable SW: %5.0fmm in %5.0fmm total depth (%3.0f%%)."
          , esw_tot
          , dep_tot
          , fract2pcnt * divide(esw_tot, dep_tot, 0.0));
    system->writeString (msg);


   }

void plantRootPart::onSowing(void)
//=======================================================================================
// Sowing Event Handler
   {
   int n = num_layers;
   dltRootLength.clear(); dltRootLength.resize(n);
   dltRootLengthDead.clear(); dltRootLengthDead.resize(n);
   dltRootLengthSenesced.clear(); dltRootLengthSenesced.resize(n);
   }

void plantRootPart::onGermination(void)
//=======================================================================================
// Germination Event Handler
   {
   plantPart::onGermination();
   root_depth = initialRootDepth;
   DMPlantMin = 0.0;
   }

void plantRootPart::onEmergence(void)
//=======================================================================================
//     Initialise crop root length at emergence based on root weight
//     at emergence and specific root length.
   {
   plantPart::onEmergence();
   DMPlantMin = 0.0;

   // initial root length (mm/mm^2)
   float initial_root_length = dmGreen() / sm2smm * specificRootLength;

   // initial root length density (mm/mm^3)
   float rld = divide (initial_root_length, root_depth, 0.0);

   int deepest_layer = find_layer_no (root_depth);

   for (int layer = 0; layer <= deepest_layer; layer++)
      {
      root_length[layer] = rld *dlayer[layer] *
                   root_proportion (layer);
      }
   }

void plantRootPart::onFlowering(void)
//=======================================================================================
// Flowering Event Handler
   {
   DMPlantMin = 0.0; //override default implementation
   }

void plantRootPart::onStartGrainFill(void)
//=======================================================================================
// Start of Grain Filling Event Handler
   {
   DMPlantMin = 0.0;
   }

void plantRootPart::onHarvest(float /*cutting_height*/, float /* remove_fr*/,
                              vector<string> &dm_type,
                              vector<float> &dlt_crop_dm,
                              vector<float> &dlt_dm_n,
                              vector<float> &dlt_dm_p,
                              vector<float> &fraction_to_residue)
//=======================================================================================
// Harvesting Event Handler
   {
   // Push dead fraction into senesced pool
   float dlt_dm_die = DMGreen * rootDieBackFraction;
   DMGreen -= dlt_dm_die;
   DMSenesced += dlt_dm_die;

   float dlt_n_die = dlt_dm_die * c.n_sen_conc;
   NGreen -= dlt_n_die;
   NSenesced += dlt_n_die;

   float dlt_p_die = PGreen * rootDieBackFraction;
   PGreen -= dlt_p_die;
   PSen += dlt_p_die;

   // Unlike above ground parts, no roots go to surface residue module.
   dm_type.push_back(c.name);
   fraction_to_residue.push_back(0.0);
   dlt_crop_dm.push_back(0.0);
   dlt_dm_n.push_back(0.0);
   dlt_dm_p.push_back(0.0);
   }


void plantRootPart::onKillStem(void)
//=======================================================================================
// Kill Stem Event Handler
   {
   // Calculate Root Die Back
   float dlt_dm_sen = DMGreen * rootDieBackFraction;
   DMSenesced += dlt_dm_sen;
   DMGreen -= dlt_dm_sen;

   float dlt_n_sen =  DMGreen * rootDieBackFraction * c.n_sen_conc;
   NSenesced += dlt_n_sen;
   NGreen -= dlt_n_sen;

   float dlt_p_sen =  PGreen * rootDieBackFraction;
   PSen += dlt_p_sen;
   PGreen -= dlt_p_sen;

   plantPart::onKillStem();
   }

void plantRootPart::plant_root_depth (void)
//=======================================================================================
//  Calculate change in plant rooting depth
   {
   const environment_t *e = plant->getEnvironment();
   //Temperature factor
   float avg_temp = (e->mint + e->maxt)/2.0;
   float temp_factor = rel_root_advance.value(avg_temp);

   //Water stress factor
   float ws_factor = ws_root_fac.value (plant->getSwdefPhoto());

   //Soil water availability factor
   int deepest_layer = num_layers-1;

   //  the layer with root front
   int layer = find_layer_no(root_depth);

   float cum_depth = sum_real_array(dlayer, layer+1);
   float rootdepth_in_layer = dlayer[layer] - (cum_depth - root_depth);

   rootdepth_in_layer = bound (rootdepth_in_layer, 0.0, dlayer[layer]);

   float weighting_factor = divide (rootdepth_in_layer, dlayer[layer], 0.0);

   int next_layer = min(layer+1, deepest_layer);

   float fasw1 = divide (sw_dep [layer] - ll_dep[layer],
                         dul_dep[layer] - ll_dep[layer], 0.0);

   float fasw2 = divide (sw_dep [next_layer] - ll_dep[next_layer],
                         dul_dep[next_layer] - ll_dep[next_layer], 0.0);

   fasw1 = min(1.0,max(0.0, fasw1));
   fasw2 = min(1.0,max(0.0, fasw2));

   float fasw = weighting_factor * fasw2 + (1.0 - weighting_factor) * fasw1;

   float sw_avail_factor = sw_fac_root.value(fasw);

   // this equation allows soil water in the deepest
   // layer in which roots are growing
   // to affect the daily increase in rooting depth.
   int stage = (int)plant->getStageNumber();
   dltRootDepth  = root_depth_rate.value(stage) *
                     temp_factor *
                       min(ws_factor, sw_avail_factor) *
                         xf[layer];

   // prevent roots partially entering layers where xf == 0
   for (deepest_layer = xf.size()-1;
        deepest_layer >= 0 && xf[deepest_layer] <= 0.0;
        deepest_layer--)
      ; /* nothing */

   float root_depth_max = sum_real_array (dlayer, deepest_layer+1);
   dltRootDepth = u_bound ( dltRootDepth, root_depth_max - root_depth);

   if (dltRootDepth < 0.0) throw std::runtime_error("negative root growth??") ;
   }

void plantRootPart::update(void)
//=======================================================================================
// Update Daily State
   {
   plantPart::update();
   root_depth += dltRootDepth;

   for (int layer = 0; layer < num_layers; layer++)
      root_length[layer] += dltRootLength[layer];

   for (int layer = 0; layer < num_layers; layer++)
      root_length[layer] -= dltRootLengthSenesced[layer];

    // Note that movement and detachment of C is already done, just
    // need to maintain relationship between length and mass
    // Note that this is not entirely accurate.  It links live root
    // weight with root length and so thereafter dead(and detaching)
    // root is assumed to have the same distribution as live roots.
    float dying_fract_plants = plant->getDyingFractionPlants();
    for (int layer = 0; layer < num_layers; layer++)
        {
        dltRootLengthDead[layer] = root_length[layer] * dying_fract_plants;
        root_length[layer] -= dltRootLengthDead[layer];
        root_length_dead[layer] += dltRootLengthDead[layer];
        }
   }

void plantRootPart::sen_length(void)
//=======================================================================================
//     Calculate root length senescence based upon changes in senesced root
//     biomass and the specific root length.
   {
   setTo (dltRootLengthSenesced, (float) 0.0);
   float senesced_length = dlt.dm_senesced / sm2smm * specificRootLength;
   root_dist(senesced_length, dltRootLengthSenesced);
   }

void plantRootPart::root_dist(float root_sum, vector<float> &root_array)           //(INPUT) Material to be distributed
//=========================================================================
//       Distribute root material over profile based upon root
//       length distribution.
   {
   // distribute roots over profile to root_depth
   int deepest_layer = find_layer_no (root_depth);
   float root_length_sum = sum_real_array (root_length, deepest_layer+1);
   for (int layer = 0; layer <= deepest_layer; layer++)
      root_array[layer] = root_sum *
                           divide (root_length[layer], root_length_sum, 0.0);
   }

void plantRootPart::root_dist_dead(float root_sum, vector<float> &root_array)      //(INPUT) Material to be distributed
//=========================================================================
//       Distribute root material over profile based upon dead root
//       length distribution.
   {
   // distribute roots over profile to root_depth
   int deepest_layer = find_layer_no (root_depth);
   float root_length_sum = sum_real_array (root_length_dead, deepest_layer+1);
   for (int layer = 0; layer <= deepest_layer; layer++)
      root_array[layer] = root_sum *
                           divide (root_length_dead[layer], root_length_sum, 0.0);
   }

void plantRootPart::collectDetachedForResidue(vector<string> &//part_name
                              , vector<float> &//dm_residue
                              , vector<float> &//dm_n
                              , vector<float> &//dm_p
                              , vector<float> &//fract_to_residue
                              )
//=======================================================================================
// Unlike above ground parts, no roots go to surface residue module.
   {
   }

void plantRootPart::collectDeadDetachedForResidue(vector<string> &//part_name
                              , vector<float> &//dm_residue
                              , vector<float> &//dm_n
                              , vector<float> &//dm_p
                              , vector<float> &//fract_to_residue
                              )
//=======================================================================================
// Unlike above ground parts, no roots go to surface residue module.
   {
   }

void plantRootPart::onEndCrop(vector<string> &/*dm_type*/,
                          vector<float> &/*dlt_crop_dm*/,
                          vector<float> &/*dlt_dm_n*/,
                          vector<float> &/*dlt_dm_p*/,
                          vector<float> &/*fraction_to_residue*/)
//=======================================================================================
// Unlike above ground parts, no roots go to surface residue module. Send our DM to FOM pool.
   {
   root_incorp (dmGreen() + dmSenesced(), nGreen() + nSenesced(), pGreen() + pSenesced());
   root_incorp_dead (dmDead(), nDead(), pDead());

   DMDead     = 0.0;
   DMSenesced = 0.0;
   DMGreen    = 0.0;

   NDead     = 0.0;
   NSenesced = 0.0;
   NGreen    = 0.0;

   PDead  = 0.0;
   PSen   = 0.0;
   PGreen  = 0.0;

   }

void plantRootPart::updateOthers(void)
//=======================================================================================
// dispose of detached material from dead & senesced roots into FOM pool
   {
   root_incorp (dlt.dm_detached,
                dlt.n_detached,
                dlt.p_det);

   root_incorp_dead (dlt.dm_dead_detached,
                     dlt.n_dead_detached,
                     dlt.p_dead_det);
   }


void plantRootPart::root_incorp (float  dlt_dm_root,                  // (INPUT) root residue dm (g/m^2)
                                 float  dlt_N_root,                   // (INPUT) root residue N (g/m^2)
                                 float  dlt_P_root)                   // (INPUT) root residue P (g/m^2)
   //=======================================================================================
   //       Add root DM, N & P to FOM pool
   {
   if (dlt_dm_root>0.0)
      {
      vector<float> dlt_dm_incorp(num_layers); // root residue (kg/ha)
      vector<float> dlt_N_incorp(num_layers);  // root residue N (kg/ha)
      vector<float> dlt_P_incorp(num_layers);  // root residue P (kg/ha)

      // DM
      root_dist(dlt_dm_root * gm2kg /sm2ha, dlt_dm_incorp);

      // Nitrogen
      root_dist(dlt_N_root * gm2kg /sm2ha, dlt_N_incorp);

      // Phosporous
      root_dist(dlt_P_root * gm2kg /sm2ha, dlt_P_incorp);

      protocol::ApsimVariant outgoingApsimVariant(plant->getComponent());
      outgoingApsimVariant.store("dlt_fom_type", protocol::DTstring, false, FString(plant->getCropType().c_str()));
      outgoingApsimVariant.store("dlt_fom_wt", protocol::DTsingle, true, dlt_dm_incorp);
      outgoingApsimVariant.store("dlt_fom_n", protocol::DTsingle, true, dlt_N_incorp);
      outgoingApsimVariant.store("dlt_fom_p", protocol::DTsingle, true, dlt_P_incorp);
      plant->getComponent()->publish (incorp_fom_ID, outgoingApsimVariant);
      }
   else
      {
      // no roots to incorporate
      }
   }

void plantRootPart::root_incorp_dead (float  dlt_dm_root,                  // (INPUT) root residue dm (g/m^2)
                                      float  dlt_N_root,                   // (INPUT) root residue N (g/m^2)
                                      float  dlt_P_root)                   // (INPUT) root residue P (g/m^2)
   //=======================================================================================
   //       Add root DM, N & P to FOM pool
   {
   if (dlt_dm_root>0.0)
      {
      vector<float> dlt_dm_incorp(num_layers); // root residue (kg/ha)
      vector<float> dlt_N_incorp(num_layers);  // root residue N (kg/ha)
      vector<float> dlt_P_incorp(num_layers);  // root residue P (kg/ha)

      // DM
      root_dist_dead(dlt_dm_root * gm2kg /sm2ha, dlt_dm_incorp);

      // Nitrogen
      root_dist_dead(dlt_N_root * gm2kg /sm2ha, dlt_N_incorp);

      // Phosporous
      root_dist_dead(dlt_P_root * gm2kg /sm2ha, dlt_P_incorp);

      protocol::ApsimVariant outgoingApsimVariant(plant->getComponent());
      outgoingApsimVariant.store("dlt_fom_type", protocol::DTstring, false, FString(plant->getCropType().c_str()));
      outgoingApsimVariant.store("dlt_fom_wt", protocol::DTsingle, true, dlt_dm_incorp);
      outgoingApsimVariant.store("dlt_fom_n", protocol::DTsingle, true, dlt_N_incorp);
      outgoingApsimVariant.store("dlt_fom_p", protocol::DTsingle, true, dlt_P_incorp);
      plant->getComponent()->publish (incorp_fom_ID, outgoingApsimVariant);
      }
   else
      {
      // no roots to incorporate
      }
   }


float plantRootPart::root_proportion (int layer)
//===========================================================================
//  Returns the proportion of layer that has roots in it (0-1).
//  Definition
//     Each element of "dlayr" holds the height of  the
//     corresponding soil layer.  The height of the top layer is
//     held in "dlayr"(1), and the rest follow in sequence down
//     into the soil profile.  Given a root depth of "root_depth",
//     this function will return the proportion of "dlayr"("layer")
//     which has roots in it  (a value in the range 0..1).
//
   {
   // Local Variables
   float depth_to_layer_bottom;   // depth to bottom of layer (mm)
   float depth_to_layer_top;      // depth to top of layer (mm)
   float depth_to_root;           // depth to root in layer (mm)
   float depth_of_root_in_layer;  // depth of root within layer (mm)
   // Implementation Section ----------------------------------
   depth_to_layer_bottom = sum_real_array(dlayer, layer+1);
   depth_to_layer_top = depth_to_layer_bottom - dlayer[layer];
   depth_to_root  = min(depth_to_layer_bottom, root_depth);
   depth_of_root_in_layer = max(0.0, depth_to_root-depth_to_layer_top);

   return (divide (depth_of_root_in_layer, dlayer[layer], 0.0));
   }

void plantRootPart::doNConccentrationLimits(float)
//==========================================================================
// N targets are static - override plantPart's implementation
   {
   plantPart::g.n_conc_crit = plantRootPart::n_conc_crit;
   plantPart::g.n_conc_min =  plantRootPart::n_conc_min;
   plantPart::g.n_conc_max =  plantRootPart::n_conc_max;
   }

void plantRootPart::redistribute(const vector<float> &dlayer_old,        //  old soil profile layers (mm)
                                 const vector<float> &dlayer_new,        //  new soil profile layers (mm)
                                 float root_depth_new)
//==========================================================================
/*  Purpose
*      Map root length density distribution into a new layer structure
*      after reduction is profile depth due to erosion.
*
*  Assumptions
*      That the new profile is shallower and the roots are at the
*      bottom of the old profile.
*
*  Notes
*      Remapping is achieved by first constructing a map of
*      cumulative root length vs depth that is 'squashed'
*      to the new profile depth.
*      The new values of root length per layer can be linearly
*      interpolated back from this shape taking into account
*      the rescaling of the profile.
*
*/
   {
   //  Local Variables
   int layer;                   // simple layer counter
   int nlayr_rt_old;            // No. of layers in old root profile
   int nlayr_rt_new;            // No. of layers in old root profile
   float pro_red_fr;                 // profile reduction fraction
   float cum_root_bottom;            // cum root at bottom of layer (mm/mm2)
   float cum_root_top;               // cum root at top of layer (mm/mm2)
   float layer_top;                  // depth to top of layer (mm)
   float layer_bottom;               // depth to bottom of layer (mm)

   // Implementation Section ----------------------------------
   int maxLayer = max(dlayer_old.size(),dlayer_new.size());
   if (maxLayer <= 0) {throw std::invalid_argument("max_layer <= 0 in crop_root_length_growth1");}
   float *cum_root_length = new float[maxLayer];  //Cum Root length with depth (mm/mm2)
   float *cum_root_depth = new float[maxLayer];   //Cum Root depth (mm)

   // Identify key layers
   // -------------------
   nlayr_rt_old = find_layer_no(root_depth,dlayer_old);
   nlayr_rt_new = find_layer_no(root_depth_new,dlayer_new);

      // calculate the fractional reduction in total profile depth
      // ---------------------------------------------------------
   pro_red_fr = divide (root_depth_new, root_depth, 0.0);

      // build interpolation pairs based on 'squashed' original root profile
      // -------------------------------------------------------------------
   cum_root_depth[0] = 0.0;
   cum_root_length[0] = 0.0;

   for(layer = 0; layer <= nlayr_rt_old; layer++)
      {
      if (layer == nlayr_rt_old)
         {
         cum_root_depth[layer + 1] = root_depth * pro_red_fr;
         }
      else
         {
         cum_root_depth[layer + 1] = cum_root_depth[layer] + dlayer_old[layer] * pro_red_fr;
         }
      cum_root_length[layer + 1] = cum_root_length[layer] + root_length[layer];
      }

   fill_real_array (root_length, 0.0, nlayr_rt_old);

   // look up new root length from interpolation pairs
   // ------------------------------------------------
   for(layer = 0; layer <= nlayr_rt_new; layer++)
      {
      layer_bottom = sum(dlayer_new, layer+1);
      layer_top = layer_bottom - dlayer_new[layer];
      cum_root_top = linear_interp_real (layer_top, cum_root_depth,
                                         cum_root_length, nlayr_rt_old + 1);
      cum_root_bottom = linear_interp_real (layer_bottom, cum_root_depth,
                                         cum_root_length, nlayr_rt_old + 1);
      root_length[layer] = cum_root_bottom - cum_root_top;
      }
   root_depth = root_depth_new;

   //delete dynamic memoy
   delete []cum_root_depth;
   delete []cum_root_length;
   }

void plantRootPart::get_root_length(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
// Getter Function for root length
{
    system->sendVariable(qd, protocol::vector<float>(root_length,root_length+num_layers));
}

void plantRootPart::get_rlv(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
// Getter Function for Root Length Volume
{
    float rlv[max_layer];
    for (int layer = 0; layer < num_layers; layer++)
       {
       rlv[layer] = divide (root_length[layer], dlayer[layer], 0.0);
       }
    system->sendVariable(qd, protocol::vector<float>(rlv,rlv+num_layers));
}

void plantRootPart::get_root_length_dead(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
// Getter Function for dead plant root length
{
    system->sendVariable(qd, protocol::vector<float>(root_length_dead, root_length_dead+num_layers));
}

void plantRootPart::get_kl(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
// Getter Function for KL
{
    system->sendVariable(qd, protocol::vector<float>(kl,kl+num_layers));
}

void plantRootPart::get_xf(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
// Getter Function of XF
{
    system->sendVariable(qd, xf);
}

void plantRootPart::checkBounds(void)
//=======================================================================================
// Check for data outside of realistic bounds
   {
   if (root_depth < 0)
     throw std::runtime_error(c.name + " depth is negative! (" + ftoa(root_depth,".4") +")");
   for (int layer = 0; layer < num_layers; layer++)
      {
      if (root_length[layer] < 0)
         throw std::runtime_error(c.name + " length in layer " + itoa(layer+1) + " is negative! (" + ftoa(root_length[layer],".4") +")");
      if (root_length_dead[layer] < 0)
         throw std::runtime_error(c.name + " length dead in layer " + itoa(layer+1) + " is negative! (" + ftoa(root_length[layer],".4") +")");
      }
   }

void plantRootPart::getOtherVariables (protocol::Component *system)
//=======================================================================================
// Get data from other modules as required
   {
    std::vector<float> values;               // Scratch area
    system->getVariable(sw_dep_id, values, sw_dep_lb, sw_dep_ub);
    for (unsigned int i=0; i< values.size(); i++)
        	sw_dep[i] = values[i];

   }
void plantRootPart::CalcWaterSupply ()
//=======================================================================================
// Calculate today's daily water supply from this root system
   {
              cproc_sw_supply1 (plant
                          ,sw_lb
                          ,dlayer
                          ,ll_dep
                          ,dul_dep
                          ,sw_dep
                          ,root_depth
                          ,kl
                          ,sw_avail
                          ,sw_avail_pot
                          ,sw_supply);

   }

float plantRootPart::waterUptake (void)
//=======================================================================================
// Return the total daily water uptake from this root system
   {
   return (- sum_real_array(dlt_sw_dep, max_layer));;
   }

void plantRootPart::doWaterUptake (float sw_demand)
//=======================================================================================
// Calculate todays daily water uptake by this root system
   {
   cproc_sw_uptake1(max_layer, dlayer, root_depth, sw_demand, sw_supply, dlt_sw_dep);
   }

void plantRootPart::plant_water_stress (
                                       float sw_demand,
                                       float& swdef_photo,
                                       float& swdef_pheno,
                                       float& swdef_pheno_flower,
                                       float& swdef_pheno_grainfill,
                                       float& swdef_expansion,
                                       float& swdef_fixation )
//     ===========================================================
//         Get current water stress factors (0-1)
    {
        crop_swdef_photo(max_layer, dlayer, root_depth,
                             sw_demand, dlt_sw_dep, &swdef_photo);

        crop_swdef_pheno(num_sw_avail_ratio,
                         x_sw_avail_ratio, y_swdef_pheno, max_layer,dlayer,
                         root_depth, sw_avail, sw_avail_pot, &swdef_pheno);

        crop_swdef_pheno(num_sw_avail_ratio_flower,
                         x_sw_avail_ratio_flower, y_swdef_pheno_flower, max_layer,dlayer,
                         root_depth, sw_avail, sw_avail_pot,
                         &swdef_pheno_flower);

        crop_swdef_pheno(num_sw_avail_ratio_grainfill,
                         x_sw_avail_ratio_grainfill, y_swdef_pheno_grainfill, max_layer, dlayer,
                         root_depth, sw_avail, sw_avail_pot,
                         &swdef_pheno_grainfill);

        crop_swdef_expansion(num_sw_demand_ratio,
                             x_sw_demand_ratio, y_swdef_leaf, max_layer, dlayer,
                             root_depth, sw_demand, sw_supply, &swdef_expansion);

        crop_swdef_fixation(num_sw_avail_fix,
                            x_sw_avail_fix, y_swdef_fix, max_layer, dlayer,
                            root_depth, sw_avail, sw_avail_pot,
                            &swdef_fixation);

    }

float plantRootPart::oxdef_stress ()
//=======================================================================================
// Calculate today's oxygen deficit (i.e. water logging) stress factor
    {
        float stress;

        crop_oxdef_photo1(  num_oxdef_photo
                          , oxdef_photo
                          , oxdef_photo_rtfr
                          , ll15_dep
                          , sat_dep
                          , sw_dep
                          , dlayer
                          , root_length
                          , root_depth
                          , &stress);
        return stress;
    }

void plantRootPart::removeBiomass2(float chop_fr)
//=======================================================================================
// Remove biomass from the root system due to senescence or plant death
   {
   float dlt_dm_die = DMGreen * rootDieBackFraction * chop_fr;
   DMSenesced += dlt_dm_die;
   DMGreen -= dlt_dm_die;

   float dlt_n_die = dlt_dm_die * c.n_sen_conc;
   NSenesced += dlt_n_die;
   NGreen -= dlt_n_die;

      // do root_length
   vector<float> dltRootLengthDie;
   dltRootLengthDie.clear(); dltRootLengthDie.resize(num_layers);
   setTo (dltRootLengthDie, (float) 0.0);
   float Die_length = dlt_dm_die / sm2smm * specificRootLength;
   root_dist(Die_length, dltRootLengthDie);
   for (int layer = 0; layer < num_layers; layer++)
      root_length[layer] -= dltRootLengthDie[layer];

   }

void plantRootPart::onNewProfile(protocol::Variant &v)
//=======================================================================================
// Handler for OnNewProfile event
    {

    float profile_depth;                          // depth of soil profile (mm)

    protocol::ApsimVariant av(plant->getComponent());
    av.aliasTo(v.getMessageData());

    protocol::vector<float> previousLayers;
    for (unsigned layer = 0; layer != max_layer; layer++)
       previousLayers.push_back(dlayer[layer]);

    protocol::vector<float> scratch;
    av.get("dlayer", protocol::DTsingle, true, scratch);
    num_layers = scratch.size();


    for (unsigned layer = 0; layer != scratch.size(); layer++)
       dlayer[layer] = scratch[layer];

    if (xf.size()==0)
       for (unsigned layer = 0; layer != scratch.size(); layer++)
          xf.push_back(0.0);


    av.get("ll15_dep", protocol::DTsingle, true, scratch);
    for (unsigned layer = 0; layer != scratch.size(); layer++) { ll15_dep[layer] = scratch[layer]; }
    av.get("dul_dep", protocol::DTsingle, true, scratch);
    for (unsigned layer = 0; layer != scratch.size(); layer++) { dul_dep[layer] = scratch[layer]; }
    av.get("sat_dep", protocol::DTsingle, true, scratch);
    for (unsigned layer = 0; layer != scratch.size(); layer++) { sat_dep[layer] = scratch[layer]; }
    av.get("sw_dep", protocol::DTsingle, true, scratch);
    for (unsigned layer = 0; layer != scratch.size(); layer++) { sw_dep[layer] = scratch[layer]; }
    av.get("bd", protocol::DTsingle, true, scratch);
    for (unsigned layer = 0; layer != scratch.size(); layer++) { bd[layer] = scratch[layer]; }

    // dlayer may be changed from its last setting due to erosion
    profile_depth = sum_real_array(dlayer,max_layer);

    if (root_depth > profile_depth)
        {
        vector<float> vdlayer;
        for (unsigned layer = 0; layer != max_layer; layer++)
          vdlayer.push_back(dlayer[layer]);
        vector<float> vprevdlayer;
        for (unsigned layer = 0; layer != previousLayers.size(); layer++)
          vprevdlayer.push_back(previousLayers[layer]);

        redistribute( vprevdlayer,  vdlayer, profile_depth);

        }
     for (unsigned layer = 0; layer < (unsigned) num_layers; layer++)
        {
        ll_dep[layer] = divide (ll_dep[layer], previousLayers[layer], 0.0)
                              * dlayer[layer];
        }
    }

int plantRootPart::find_layer_no(float depth) const
//=======================================================================================
// Return the index of the layer corresponding to the given depth
   {
   unsigned int indx;
   float progressive_sum = 0.0;

   for(indx = 0; indx < (unsigned) num_layers; indx++)
      {
      progressive_sum = progressive_sum + dlayer[indx];
      if(progressive_sum >= depth)
         break;
      }
   if (indx != 0 && indx==(unsigned)num_layers) return (indx - 1); // last element in array
   return indx;                                            // index of
   }

float plantRootPart::sw_avail_ratio(int layer) const //(INPUT) soil profile layer number
//===========================================================================
//     Get the soil water availability factor in a layer.  For a layer,
//     it is 1.0 unless the plant-extractable soil water declines
//     below a fraction of plant-extractable soil water capacity for
//     that layer.
   {
   //  Local Variables
   float pesw;                // plant extractable soil-water (mm/mm)
   float pesw_capacity;       // plant extractable soil-water capacity (mm/mm)
   float sw_avail_ratio;      // soil water availability ratio (0-1)

   pesw = sw_dep[layer] - ll_dep[layer];
   pesw_capacity = dul_dep[layer] - ll_dep[layer];
   sw_avail_ratio = divide (pesw, pesw_capacity, 10.0);
   return sw_avail_ratio;
   }


int plantRootPart::find_layer_no(float depth,      // depth in profile
                  float *dlayr,     // layer depth array
                  int num_layers)   // lowest layer
//===========================================================================

/*Purpose
 *   returns layer number of depth in profile dlayr
 *Definition
 *   Each of the "num_layers" elements of "dlayr" holds the
 *   height of the corresponding soil layer.  The height of the
 *   top layer is held in "dlayr"(0), and the rest follow in
 *   sequence down into the soil profile.  This function
 *   returns the index of the first element of "dlayr" which
 *   has its lower surface deeper than or equal to "depth".  If
 *   "depth" is deeper than the lower surface of the layer
 *   corresponding to "dlayr"("num_layers"), then "num_layers"
 *   is returned.
 */

   {
   return get_cumulative_index_real(depth, dlayr, num_layers);
   }

int plantRootPart::find_layer_no(float depth, const vector<float> &dlayer )
//===========================================================================
   {
   float progressive_sum = 0.0; //cumulative sum_of
   unsigned int indx;                    //index count_of_real_vals

   for(indx = 0; indx < dlayer.size(); indx++)
      {
      progressive_sum +=  dlayer[indx];
      if(progressive_sum >= depth)
         {
         break;
         }
      }
   if (indx==dlayer.size()) return (indx - 1); // last element in array
   return indx;                                // index of
   }

void plantRootPart::DoIDs(protocol::Component *system)
//=======================================================================================
// Register outputs and store variable ID's
   {
   // Get
   sw_dep_id= system->addRegistration(RegistrationType::get,
                                   "sw_dep", addUnitsToDDML(floatArrayType, "mm").c_str(),
                                   "", "");
   // Respond to Get
   setupGetFunction(system, "sw_uptake", protocol::DTsingle, true,
                    &plantRootPart::get_sw_uptake, "mm", "Plant water uptake per layer");
   setupGetFunction(system, "sw_supply", protocol::DTsingle, false,
                    &plantRootPart::get_sw_supply, "mm", "Soil water supply");
   setupGetFunction(system, "sw_supply_layr", protocol::DTsingle, true,
                    &plantRootPart::get_sw_supply_layr, "mm", "Soil water supply");
   setupGetFunction(system, "ep", protocol::DTsingle, false,
                    &plantRootPart::get_ep, "mm", "Plant water uptake");
   setupGetFunction(system, "esw_layr", protocol::DTsingle, true,
                    &plantRootPart::get_esw_layr, "mm", "Extractable soil water");
   setupGetFunction(system, "no3_uptake", protocol::DTsingle, true,
                    &plantRootPart::get_no3_uptake,"kg/ha","NO3 uptake");
   setupGetFunction(system, "nh4_uptake", protocol::DTsingle, true,
                    &plantRootPart::get_nh4_uptake,"kg/ha","NH4 uptake");
   setupGetFunction(system, "no3_tot", protocol::DTsingle, false,
                    &plantRootPart::get_no3_tot,"g/m^2", "NO3 available to plants");
   setupGetFunction(system, "ll_dep", protocol::DTsingle, true,
                    &plantRootPart::get_ll_dep,"mm","Crop lower limit");
   setupGetFunction(system, "ll", protocol::DTsingle, true,
                    &plantRootPart::get_ll,"%vol","Crop lower limit");

   //Sets
   dlt_sw_dep_id = system->addRegistration(RegistrationType::set,
                                   "dlt_sw_dep", addUnitsToDDML(floatArrayType, "mm").c_str(),
                                   "", "");

   }

void plantRootPart::UpdateOtherVariables(protocol::Component *system)
//=======================================================================================
// Update data owned by other modules that has changed due to calculations by this root system
   {
    float scratch[max_layer];                     // soil NO3 change (kg/ha)
    int   layer;                                  // soil layer no.

   for (layer = 0; layer< num_layers;layer++) {scratch[layer] = dlt_sw_dep[layer];}
   protocol::vector<float> dlt_sw_dep_values(scratch, scratch+num_layers);
   system->setVariable(dlt_sw_dep_id, dlt_sw_dep_values);
   }

void plantRootPart::get_sw_uptake(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
// Getter Function for Soil Water uptake
{
    float rwu[max_layer];
    for (int layer = 0; layer < num_layers; layer++)
        {
        rwu[layer] = fabs(dlt_sw_dep[layer]);
        }
    system->sendVariable(qd, protocol::vector<float>(rwu, rwu+num_layers));
}


void plantRootPart::get_sw_supply(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
// Getter Function for Total Profile Soil Water Supply
{
    int deepest_layer = find_layer_no (root_depth);
    float sw_supply_sum = sum_real_array (sw_supply, deepest_layer+1);
    system->sendVariable(qd, sw_supply_sum);
}

void plantRootPart::get_sw_supply_layr(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
// Getter function for soil water supply from each layer
{
    system->sendVariable(qd, protocol::vector<float>(sw_supply, sw_supply+num_layers));
}

void plantRootPart::get_ep(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
// Getter Function for plant EP
{
    float sum = 0.0;
    for (int layer = 0; layer < num_layers; layer++)
        {
        sum = sum + fabs(dlt_sw_dep[layer]);
        }
    system->sendVariable(qd, sum);
}

void plantRootPart::get_esw_layr(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
// Getter function for extractable soil water content of each layer
{
    float esw_layr[max_layer];
    for (int layer = 0; layer < num_layers; layer++)
       {
       esw_layr[layer] = l_bound (sw_dep[layer] - ll_dep[layer], 0.0);
       }
    system->sendVariable(qd, protocol::vector<float>(esw_layr,esw_layr+num_layers));
}
void plantRootPart::get_no3_uptake(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
// Getter function for no3 uptake from each soil layer
{
    float no3_uptake[max_layer];
    fill_real_array(no3_uptake,0.0, max_layer);
    for (int layer = 0; layer < num_layers; layer++) {
       no3_uptake[layer] =  dlt_no3gsm[layer] * gm2kg/sm2ha;
    }
    system->sendVariable(qd, protocol::vector<float>(no3_uptake, no3_uptake+num_layers));
}

void plantRootPart::get_nh4_uptake(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
// Getter function for nh4 uptake from each soil layer
{
    float nh4_uptake[max_layer];
    fill_real_array(nh4_uptake,0.0, max_layer);
    for (int layer = 0; layer <= num_layers; layer++) {
       nh4_uptake[layer] =  dlt_nh4gsm[layer] * gm2kg/sm2ha;
    }
    system->sendVariable(qd, protocol::vector<float>(nh4_uptake, nh4_uptake+num_layers));
}

void plantRootPart::get_no3_tot(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
// Getter Function for total profile no3 uptake
{
    int deepest_layer = find_layer_no (root_depth);
    float no3gsm_tot = sum_real_array (no3gsm, deepest_layer+1);
    system->sendVariable(qd, no3gsm_tot);
}
void plantRootPart::get_ll_dep(protocol::Component *systemInterface, protocol::QueryValueData &qd)
//=======================================================================================
// Getter function for crop lower limit (mm)
   {
   vector<float> lldep;
   for(int layer = 0; layer < num_layers; layer++)
      lldep.push_back(ll_dep[layer]);
   systemInterface->sendVariable(qd, lldep);
}

void plantRootPart::get_ll(protocol::Component *systemInterface, protocol::QueryValueData &qd)
//=======================================================================================
// Getter function for crop lower limit (volumetric)
   {
   vector<float> ll;
   for(int layer = 0; layer < num_layers; layer++)
      ll.push_back(ll_dep[layer] / dlayer[layer]);
   systemInterface->sendVariable(qd, ll);
   }

