#include <stdio.h>
#include <math.h>
#include <stdexcept>
#include <string>
#include "PlantPart.h"

#include "RootPart.h"
#include "RootGrowthOption1.h"
#include "RootGrowthOption2.h"
#include "Environment.h"
#include <numeric>
using namespace std;

string IncorpFOMType = protocol::DDML(protocol::IncorpFomType());
string floatArrayType = protocol::DDML(vector<float>());

RootPart::RootPart(ScienceAPI& scienceAPI, plantInterface *p, const string &name)
   : RootBase(scienceAPI, p, name)
//=======================================================================================
// Constructor
   {
   incorp_fom_ID = 0;

   zeroSoil();
   zeroAllGlobals();
   }


void RootPart::zeroAllGlobals(void)
//=======================================================================================
// Zero all global values
   {
   plantPart::zeroAllGlobals();
   root_depth            = 0.0;
   initialRootDepth = 0.0;
   rootDieBackFraction = 0.0;
   specificRootLength = 0.0;

   fill_real_array (root_length , 0.0, max_layer);
   fill_real_array (root_length_senesced, 0.0, max_layer);
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

      fill_real_array (no3gsm_uptake_pot, 0.0, max_layer);
      fill_real_array (nh4gsm_uptake_pot, 0.0, max_layer);
      no3_diffn_const = 0.0;
      no3_uptake_max = 0.0;
      no3_conc_half_max = 0.0;
   }

void RootPart::zeroSoil(void)
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

void RootPart::zeroDeltas(void)
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
   fill_real_array (dlt_no3gsm , 0.0, max_layer);
   fill_real_array (dlt_nh4gsm , 0.0, max_layer);

   fill_real_array (no3gsm_uptake_pot, 0.0, max_layer);
   fill_real_array (nh4gsm_uptake_pot, 0.0, max_layer);
   }

void RootPart::onInit1(protocol::Component *system)
//=======================================================================================
// Perform all component initialisation.
   {
   plantPart::onInit1(system);
   system->addGettableVar("root_depth",
               root_depth, "mm", "depth of roots");

   setupGetFunction(system, "root_length", protocol::DTsingle, true,
                    &RootPart::get_root_length,
                    "mm/mm^2", "Root length");

   setupGetFunction(system, "root_length_senesced", protocol::DTsingle, true,
                    &RootPart::get_root_length_senesced, "mm/mm^2", "Senesced root length");

   setupGetFunction(system, "rlv", protocol::DTsingle, true,
                    &RootPart::get_rlv, "mm/mm^3", "Root length density");

   setupGetFunction(system, "rld", protocol::DTsingle, true,
                    &RootPart::get_rlv, "mm/mm^3", "Root length density");

   setupGetFunction(system, "kl", protocol::DTsingle, true,
                    &RootPart::get_kl, "", "Root Water Uptake Parameter");

   setupGetFunction(system, "xf", protocol::DTsingle, true,
                    &RootPart::get_xf, "", "Root Exploration Factor");

   setupGetFunction(system, "no3gsm_uptake_pot", protocol::DTsingle, true,
                    &RootPart::get_no3gsm_uptake_pot,
                    "g/m2", "Pot NO3 uptake");

   setupGetFunction(system, "nh4gsm_uptake_pot", protocol::DTsingle, true,
                    &RootPart::get_nh4gsm_uptake_pot,
                    "g/m2", "Pot NH4 uptake");

   incorp_fom_ID = plant->getComponent()->addRegistration(RegistrationType::event,
                                                          "incorp_fom", IncorpFOMType.c_str(),
                                                          "", "");
   // Respond to Get
   setupGetFunction(system, "sw_uptake", protocol::DTsingle, true,
                    &RootPart::get_sw_uptake, "mm", "Plant water uptake per layer");
   setupGetFunction(system, "sw_supply", protocol::DTsingle, false,
                    &RootPart::get_sw_supply, "mm", "Soil water supply");
   setupGetFunction(system, "sw_supply_layr", protocol::DTsingle, true,
                    &RootPart::get_sw_supply_layr, "mm", "Soil water supply");
   setupGetFunction(system, "ep", protocol::DTsingle, false,
                    &RootPart::get_ep, "mm", "Plant water uptake");
   setupGetFunction(system, "esw_layr", protocol::DTsingle, true,
                    &RootPart::get_esw_layr, "mm", "Extractable soil water");
   setupGetFunction(system, "no3_uptake", protocol::DTsingle, true,
                    &RootPart::get_no3_uptake,"kg/ha","NO3 uptake");
   setupGetFunction(system, "nh4_uptake", protocol::DTsingle, true,
                    &RootPart::get_nh4_uptake,"kg/ha","NH4 uptake");
   setupGetFunction(system, "no3_tot", protocol::DTsingle, false,
                    &RootPart::get_no3_tot,"g/m^2", "NO3 available to plants");
   setupGetFunction(system, "ll_dep", protocol::DTsingle, true,
                    &RootPart::get_ll_dep,"mm","Crop lower limit");
   setupGetFunction(system, "ll", protocol::DTsingle, true,
                    &RootPart::get_ll,"%vol","Crop lower limit");
   setupGetFunction(system, "n_supply_soil", protocol::DTsingle, false,
                    &RootPart::get_n_supply_soil,
                    "g/m^2", "N supply");
   setupGetFunction(system, "no3_swfac", protocol::DTsingle, true,
                    &RootPart::get_no3_swfac,
                    "???", "Work this out...>>");
   }

void RootPart::read()
//=======================================================================================
// Read all parameters
   {
   //plantPart::readConstants(NULL, "");
   //plantPart::readSpeciesParameters(NULL, vector<string>());
   //plantPart::readCultivarParameters(NULL, "");

   scienceAPI.readOptional("crop_type", crop_type);
   scienceAPI.read("n_supply_preference", n_supply_preference);

   scienceAPI.read("sw_ub", sw_ub, 0.0f, 1.0f);
   scienceAPI.read("sw_lb", sw_lb, 0.0f, 1.0f);
   scienceAPI.read("sw_dep_ub", sw_dep_ub, 0.0f, 10000.0f);
   scienceAPI.read("sw_dep_lb", sw_dep_lb, 0.0f, 10000.0f);

   // Read species-specific parameters

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

   // Read Rooting parameters
    vector<float> ll ;   // lower limit of plant-extractable
                         // soil water for soil layer l
                         // (mm water/mm soil)

    if (scienceAPI.readOptional("ll", ll, 0.0, sw_ub))
       {
       for (unsigned int layer = 0; layer != ll.size(); layer++)
          ll_dep[layer] = ll[layer]*dlayer[layer];

       if ((int)ll.size() != num_layers)
          throw std::runtime_error ("Size of LL array doesn't match soil profile.");
       }
    else
       {
       scienceAPI.getOptional("ll15", "", ll, 0.0, sw_ub);
       if (ll.size() == 0)
          throw std::runtime_error("No Crop Lower Limit found");

       for (unsigned int i=0; i< ll.size(); i++) ll_dep[i] = ll[i]*dlayer[i];
       cout << "        Using externally supplied Lower Limit (ll15)\n";
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

   int numvals;
   scienceAPI.read("n_fix_rate", n_fix_rate, numvals, 0.0f, 1.0f);
   scienceAPI.read("N_stress_start_stage", n_stress_start_stage, 0.0f, 100.0f);

   scienceAPI.read("n_uptake_option", n_uptake_option, 1, 3);
   if (n_uptake_option==1)
      scienceAPI.read("no3_diffn_const", no3_diffn_const, 0.0f, 100.0f);
   else if (n_uptake_option==2)
      {
      scienceAPI.read("no3_uptake_max", no3_uptake_max, 0.0f, 1.0f);
      scienceAPI.read("no3_conc_half_max", no3_conc_half_max, 0.0f, 100.0f);
      scienceAPI.read("total_n_uptake_max", total_n_uptake_max, 0.0f, 100.0f);
      }
   else if (n_uptake_option==3)
      {
      scienceAPI.read("kno3", kno3, 0.0f, 1.0f);
      scienceAPI.read("no3ppm_min", no3ppm_min, 0.0f, 10.0f);
      scienceAPI.read("knh4", knh4, 0.0f, 1.0f);
      scienceAPI.read("nh4ppm_min", nh4ppm_min, 0.0f, 10.0f);
      scienceAPI.read("total_n_uptake_max", total_n_uptake_max, 0.0f, 100.0f);
      }

   scienceAPI.read("no3_ub", no3_ub, 0.0f, 100000.0f);
   scienceAPI.read("no3_lb", no3_lb, 0.0f, 100000.0f);
   scienceAPI.read("nh4_ub", nh4_ub, 0.0f, 100000.0f);
   scienceAPI.read("nh4_lb", nh4_lb, 0.0f, 100000.0f);
   }

void RootPart::write()
//=======================================================================================
// Write all parameters as a summary to stdout.
   {
   cout << "                        Root Profile" << endl;
   cout << "         -----------------------------------------------" << endl;
   cout << "          Layer       Kl           Lower    Exploration" << endl;
   cout << "          Depth     Factor         Limit      Factor" << endl;
   cout << "          (mm)         ()        (mm/mm)       (0-1)" << endl;
   cout << "         -----------------------------------------------" << endl;

    float dep_tot, esw_tot;                      // total depth of soil & ll
    char  msg[200];

    dep_tot = esw_tot = 0.0;
    for (int layer = 0; layer < num_layers; layer++)
       {
       sprintf (msg, "     %9.1f%10.3f%15.3f%12.3f"
          , dlayer[layer]
          , kl[layer]
          , divide(ll_dep[layer],dlayer[layer],0.0)
          , xf[layer]);
       cout << msg << endl;
       dep_tot += dlayer[layer];
       esw_tot += dul_dep[layer] - ll_dep[layer];
       }
    cout << "         -----------------------------------------------" << endl;
    sprintf (msg
          , "         Extractable SW: %5.0fmm in %5.0fmm total depth (%3.0f%%)."
          , esw_tot
          , dep_tot
          , fract2pcnt * divide(esw_tot, dep_tot, 0.0));
    cout << msg << endl;
   }

void RootPart::onTransplanting(void)
   {
   onSowing();
   onGermination();
   onEmergence();
   }
void RootPart::onSowing(void)
//=======================================================================================
// Sowing Event Handler
   {
   int n = num_layers;
   dltRootLength.clear(); dltRootLength.resize(n);
   dltRootLengthDead.clear(); dltRootLengthDead.resize(n);
   dltRootLengthSenesced.clear(); dltRootLengthSenesced.resize(n);
   }

void RootPart::onGermination(void)
//=======================================================================================
// Germination Event Handler
   {
   plantPart::onGermination();
   root_depth = initialRootDepth;
   DMPlantMin = 0.0;
   }

void RootPart::onEmergence(void)
//=======================================================================================
//     Initialise crop root length at emergence based on root weight
//     at emergence and specific root length.
   {
   plantPart::onEmergence();
   DMPlantMin = 0.0;

   // initial root length (mm/mm^2)
   float initial_root_length = Green().DM / sm2smm * specificRootLength;

   // initial root length density (mm/mm^3)
   float rld = divide (initial_root_length, root_depth, 0.0);

   int deepest_layer = find_layer_no (root_depth);

   for (int layer = 0; layer <= deepest_layer; layer++)
      root_length[layer] = rld *dlayer[layer] * root_proportion (layer);
   }

void RootPart::onFlowering(void)
//=======================================================================================
// Flowering Event Handler
   {
   DMPlantMin = 0.0; //override default implementation
   }

void RootPart::onStartGrainFill(void)
//=======================================================================================
// Start of Grain Filling Event Handler
   {
   DMPlantMin = 0.0;
   }

void RootPart::onHarvest(float /*cutting_height*/, float /* remove_fr*/,
                              vector<string> &dm_type,
                              vector<float> &dlt_crop_dm,
                              vector<float> &dlt_dm_n,
                              vector<float> &dlt_dm_p,
                              vector<float> &fraction_to_residue)
//=======================================================================================
// Harvesting Event Handler
   {
   // Push dead fraction into senesced pool
   float dlt_dm_die = Green().DM * rootDieBackFraction;
   Green().DM -= dlt_dm_die;
   Senesced().DM += dlt_dm_die;

   float dlt_n_die = dlt_dm_die * c.n_sen_conc;
   Green().N -= dlt_n_die;
   Senesced().N += dlt_n_die;

   float dlt_p_die = Green().P * rootDieBackFraction;
   Green().P -= dlt_p_die;
   Senesced().P += dlt_p_die;

   // Unlike above ground parts, no roots go to surface residue module.
   dm_type.push_back(c.name);
   fraction_to_residue.push_back(0.0);
   dlt_crop_dm.push_back(0.0);
   dlt_dm_n.push_back(0.0);
   dlt_dm_p.push_back(0.0);
   }


void RootPart::onKillStem(void)
//=======================================================================================
// Kill Stem Event Handler
   {
   // Calculate Root Die Back
   float dlt_dm_sen = Green().DM * rootDieBackFraction;
   Senesced().DM += dlt_dm_sen;
   Green().DM -= dlt_dm_sen;

   float dlt_n_sen =  Green().DM * rootDieBackFraction * c.n_sen_conc;
   Senesced().N += dlt_n_sen;
   Green().N -= dlt_n_sen;

   float dlt_p_sen =  Green().P * rootDieBackFraction;
   Senesced().P += dlt_p_sen;
   Green().P -= dlt_p_sen;

   plantPart::onKillStem();
   }

void RootPart::plant_root_depth (void)
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

void RootPart::update(void)
//=======================================================================================
// Update Daily State
   {
   plantPart::update();
   root_depth += dltRootDepth;

   for (int layer = 0; layer < num_layers; layer++)
      root_length[layer] += dltRootLength[layer];

   for (int layer = 0; layer < num_layers; layer++)
      {
      root_length[layer] -= dltRootLengthSenesced[layer];
      root_length_senesced[layer] += dltRootLengthSenesced[layer];
      }
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
        root_length_senesced[layer] += dltRootLengthDead[layer];
        }

   bound_check_real_var(plant, root_depth, 0.0
                        , sum_real_array (dlayer, max_layer)
                        , "root_depth");
   }

void RootPart::sen_length(void)
//=======================================================================================
//     Calculate root length senescence based upon changes in senesced root
//     biomass and the specific root length.
   {
   setTo (dltRootLengthSenesced, (float) 0.0);
   float senesced_length = Senescing.DM / sm2smm * specificRootLength;
   root_dist(senesced_length, dltRootLengthSenesced);
   }

void RootPart::root_dist(float root_sum, vector<float> &root_array)           //(INPUT) Material to be distributed
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

void RootPart::root_dist_dead(float root_sum, vector<float> &root_array)      //(INPUT) Material to be distributed
//=========================================================================
//       Distribute root material over profile based upon dead root
//       length distribution.
   {
   // distribute roots over profile to root_depth
   int deepest_layer = find_layer_no (root_depth);
   float root_length_sum = sum_real_array (root_length_senesced, deepest_layer+1);
   for (int layer = 0; layer <= deepest_layer; layer++)
      root_array[layer] = root_sum *
                           divide (root_length_senesced[layer], root_length_sum, 0.0);
   }

void RootPart::collectDetachedForResidue(vector<string> &//part_name
                              , vector<float> &//dm_residue
                              , vector<float> &//dm_n
                              , vector<float> &//dm_p
                              , vector<float> &//fract_to_residue
                              )
//=======================================================================================
// Unlike above ground parts, no roots go to surface residue module.
   {
   }

void RootPart::onEndCrop(vector<string> &/*dm_type*/,
                          vector<float> &/*dlt_crop_dm*/,
                          vector<float> &/*dlt_dm_n*/,
                          vector<float> &/*dlt_dm_p*/,
                          vector<float> &/*fraction_to_residue*/)
//=======================================================================================
// Unlike above ground parts, no roots go to surface residue module. Send our DM to FOM pool.
   {
   root_incorp (Green().DM , Green().N, Green().P);
   root_incorp_dead (Senesced().DM, Senesced().N, Senesced().P);
   //root_incorp_dead (dmDead(), nDead(), pDead());

   Senesced().Clear();
   Green().Clear();

   }

void RootPart::updateOthers(void)
//=======================================================================================
// dispose of detached material from dead & senesced roots into FOM pool
   {
   root_incorp (Detaching.DM,
                Detaching.N,
                Detaching.P);
   }


void RootPart::root_incorp (float  dlt_dm_root,                  // (INPUT) root residue dm (g/m^2)
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

void RootPart::root_incorp_dead (float  dlt_dm_root,                  // (INPUT) root residue dm (g/m^2)
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


float RootPart::root_proportion (int layer)
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

void RootPart::doNConccentrationLimits(float)
//==========================================================================
// N targets are static - override plantPart's implementation
   {
   plantPart::g.n_conc_crit = RootPart::n_conc_crit;
   plantPart::g.n_conc_min =  RootPart::n_conc_min;
   plantPart::g.n_conc_max =  RootPart::n_conc_max;
   }

void RootPart::redistribute(const vector<float> &dlayer_old,        //  old soil profile layers (mm)
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

void RootPart::get_root_length(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
// Getter Function for root length
{
    system->sendVariable(qd, protocol::vector<float>(root_length,root_length+num_layers));
}

void RootPart::get_rlv(protocol::Component *system, protocol::QueryValueData &qd)
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

void RootPart::get_root_length_senesced(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
// Getter Function for dead plant root length
{
    system->sendVariable(qd, protocol::vector<float>(root_length_senesced, root_length_senesced+num_layers));
}

void RootPart::get_kl(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
// Getter Function for KL
{
    system->sendVariable(qd, protocol::vector<float>(kl,kl+num_layers));
}

void RootPart::get_xf(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
// Getter Function of XF
{
    system->sendVariable(qd, xf);
}

void RootPart::get_no3gsm_uptake_pot(protocol::Component *system, protocol::QueryValueData &qd)                  //FIXME - belongs in rootPart
   {
   system->sendVariable(qd, protocol::vector<float>(no3gsm_uptake_pot, no3gsm_uptake_pot+num_layers));
   }

void RootPart::get_nh4gsm_uptake_pot(protocol::Component *system, protocol::QueryValueData &qd)                  //FIXME - belongs in rootPart
   {
   system->sendVariable(qd, protocol::vector<float>(nh4gsm_uptake_pot, nh4gsm_uptake_pot+num_layers));
   }

void RootPart::checkBounds(void)
//=======================================================================================
// Check for data outside of realistic bounds
   {
   if (root_depth < 0)
     throw std::runtime_error(c.name + " depth is negative! (" + ftoa(root_depth,".4") +")");
   for (int layer = 0; layer < num_layers; layer++)
      {
      if (root_length[layer] < 0)
         throw std::runtime_error(c.name + " length in layer " + itoa(layer+1) + " is negative! (" + ftoa(root_length[layer],".4") +")");
      if (root_length_senesced[layer] < 0)
         throw std::runtime_error(c.name + " length dead in layer " + itoa(layer+1) + " is negative! (" + ftoa(root_length[layer],".4") +")");
      }
   }

void RootPart::getOtherVariables()
//=======================================================================================
// Get data from other modules as required
   {
   std::vector<float> values;               // Scratch area
   scienceAPI.get("sw_dep", "", values, sw_dep_lb, sw_dep_ub);
   for (unsigned int i=0; i< values.size(); i++)
      sw_dep[i] = values[i];

   values.clear();
   if (!scienceAPI.getOptional("no3", "", values, no3_lb, no3_ub))
      {
      // we have no N supply - make non-limiting.
      for (int i = 0; i < num_layers; i++)
         values.push_back(10000.0);
      }
   for (int i = 0; i < num_layers; i++)
      no3gsm[i] = values[i] * kg2gm /ha2sm;

    values.clear();
    if (!scienceAPI.getOptional("nh4", "", values, nh4_lb, nh4_ub))
        {
        // we have no N supply - make non-limiting.
        for (int i = 0; i < num_layers; i++)
           values.push_back(10000.0);
        }
    for (int i = 0; i < num_layers; i++)
       nh4gsm[i] = values[i] * kg2gm /ha2sm;

   }

void RootPart::waterSupply ()
//=======================================================================================
// Calculate today's daily water supply from this root system
// based on the KL approach
   {
   crop_check_sw(plant, sw_lb, dlayer, dul_dep, sw_dep, ll_dep);

   // potential extractable sw
   potentialExtractableSW();

   // actual extractable sw (sw-ll)
   SWAvailable();

   SWSupply();
   }

float RootPart::waterUptake (void)
//=======================================================================================
// Return the total daily water uptake from this root system
   {
   return (- sum_real_array(dlt_sw_dep, max_layer));;
   }

void RootPart::doWaterUptake (float sw_demand)
//=======================================================================================
// Calculate todays daily water uptake by this root system
   {
   int deepest_layer = find_layer_no (root_depth, dlayer, max_layer);
   float sw_supply_sum = sum_real_array (sw_supply, deepest_layer+1);

   if ((sw_supply_sum < 0.0) || (sw_demand  < 0.0))
      {
      //we have no uptake - there is no demand or potential
      fill_real_array (dlt_sw_dep, 0.0, max_layer);
      }
   else
      {
      // get actual uptake
      fill_real_array (dlt_sw_dep, 0.0, max_layer);
      if (sw_demand < sw_supply_sum)
         {
         // demand is less than what roots could take up.
         // water is non-limiting.
         // distribute demand proportionately in all layers.
         for(int layer = 0; layer <= deepest_layer; layer++)
            {
            dlt_sw_dep[layer] = -1 * divide (sw_supply[layer], sw_supply_sum, 0.0) * sw_demand;
            }
         }
      else
         {
         // water is limiting - not enough to meet demand so take
         // what is available (potential)
         for(int layer = 0; layer <= deepest_layer; layer++)
            {
            dlt_sw_dep[layer] = -1 * sw_supply[layer];
            }
         }
      }
   }

void RootPart::plant_water_stress (
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
   swdef_photo = SWDefPhoto(sw_demand);
   swdef_pheno = SWDefPheno(num_sw_avail_ratio,
                            x_sw_avail_ratio,
                            y_swdef_pheno);
   swdef_pheno_flower = SWDefPheno(num_sw_avail_ratio_flower,
                                   x_sw_avail_ratio_flower,
                                   y_swdef_pheno_flower);
   swdef_pheno_grainfill = SWDefPheno(num_sw_avail_ratio_grainfill,
                                      x_sw_avail_ratio_grainfill,
                                      y_swdef_pheno_grainfill);
   swdef_expansion = SWDefExpansion(sw_demand);
   swdef_fixation = SWDefFixation();
   }

float RootPart::oxdef_stress ()
//=======================================================================================
// Calculate today's oxygen deficit (i.e. water logging) stress factor
   {
   if (root_depth > 0.0)
      {
      float wfps;
      vector<float> root_fr;
      rootDist(1.0, root_fr);

      float wet_root_fr = 0.0;
      for (unsigned layer = 0; layer <= root_fr.size(); layer++)
         {
         wfps = divide(sw_dep[layer] - ll15_dep[layer],
                       sat_dep[layer] - ll15_dep[layer], 0.0);
         wfps = bound (wfps, 0.0, 1.0);

         wet_root_fr = wet_root_fr + wfps * root_fr[layer];
         }
      return linear_interp_real(wet_root_fr, oxdef_photo_rtfr, oxdef_photo,
                                num_oxdef_photo);
      }
   else
      return 1.0;
   }

void RootPart::removeBiomass2(float chop_fr)
//=======================================================================================
// Remove biomass from the root system due to senescence or plant death
   {
   float dlt_dm_die = Green().DM * rootDieBackFraction * chop_fr;
   Senesced().DM += dlt_dm_die;
   Green().DM -= dlt_dm_die;

   float dlt_n_die = dlt_dm_die * c.n_sen_conc;
   Senesced().N += dlt_n_die;
   Green().N -= dlt_n_die;

   /// WHY NO P?????????

      // do root_length
   vector<float> dltRootLengthDie;
   dltRootLengthDie.clear(); dltRootLengthDie.resize(num_layers);
   setTo (dltRootLengthDie, (float) 0.0);
   float Die_length = dlt_dm_die / sm2smm * specificRootLength;
   root_dist(Die_length, dltRootLengthDie);
   for (int layer = 0; layer < num_layers; layer++)
      root_length[layer] -= dltRootLengthDie[layer];

   }

void RootPart::onNewProfile(protocol::Variant &v)
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

int RootPart::find_layer_no(float depth)
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

float RootPart::sw_avail_ratio(int layer)  //(INPUT) soil profile layer number
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


int RootPart::find_layer_no(float depth,      // depth in profile
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

int RootPart::find_layer_no(float depth, const vector<float> &dlayer )
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

void RootPart::UpdateOtherVariables()
//=======================================================================================
// Update data owned by other modules that has changed due to calculations by this root system
   {
   if (Str_i_Eq(uptake_source, "calc"))
      {
      vector<float> dltNO3KgHa, dltNH4KgHa, dltSwDep;
      for (int layer = 0; layer< num_layers;layer++)
         {
         dltNO3KgHa.push_back(dlt_no3gsm[layer] * gm2kg /sm2ha);
         dltNH4KgHa.push_back(dlt_nh4gsm[layer] * gm2kg /sm2ha);
         dltSwDep.push_back(dlt_sw_dep[layer]);
         }
      scienceAPI.set("dlt_no3", "kg/ha", dltNO3KgHa);
      scienceAPI.set("dlt_nh4", "kg/ha", dltNH4KgHa);
      scienceAPI.set("dlt_sw_dep", "mm", dltSwDep);
      }
   else
      {
      // no need to send updates
      }
   }

void RootPart::get_sw_uptake(protocol::Component *system, protocol::QueryValueData &qd)
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


void RootPart::get_sw_supply(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
// Getter Function for Total Profile Soil Water Supply
{
    int deepest_layer = find_layer_no (root_depth);
    float sw_supply_sum = sum_real_array (sw_supply, deepest_layer+1);
    system->sendVariable(qd, sw_supply_sum);
}

void RootPart::get_sw_supply_layr(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
// Getter function for soil water supply from each layer
{
    system->sendVariable(qd, protocol::vector<float>(sw_supply, sw_supply+num_layers));
}

void RootPart::get_ep(protocol::Component *system, protocol::QueryValueData &qd)
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

void RootPart::get_esw_layr(protocol::Component *system, protocol::QueryValueData &qd)
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
void RootPart::get_no3_uptake(protocol::Component *system, protocol::QueryValueData &qd)
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

void RootPart::get_nh4_uptake(protocol::Component *system, protocol::QueryValueData &qd)
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

void RootPart::get_no3_tot(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
// Getter Function for total profile no3 uptake
{
    int deepest_layer = find_layer_no (root_depth);
    float no3gsm_tot = sum_real_array (no3gsm, deepest_layer+1);
    system->sendVariable(qd, no3gsm_tot);
}
void RootPart::get_ll_dep(protocol::Component *systemInterface, protocol::QueryValueData &qd)
//=======================================================================================
// Getter function for crop lower limit (mm)
   {
   vector<float> lldep;
   for(int layer = 0; layer < num_layers; layer++)
      lldep.push_back(ll_dep[layer]);
   systemInterface->sendVariable(qd, lldep);
}

void RootPart::get_ll(protocol::Component *systemInterface, protocol::QueryValueData &qd)
//=======================================================================================
// Getter function for crop lower limit (volumetric)
   {
   vector<float> ll;
   for(int layer = 0; layer < num_layers; layer++)
      ll.push_back(ll_dep[layer] / dlayer[layer]);
   systemInterface->sendVariable(qd, ll);
   }

void RootPart::get_n_supply_soil(protocol::Component *systemInterface, protocol::QueryValueData &qd)    //FIXME - belongs in rootPart
//=======================================================================================
// Getter function for soil n supply (g/m^2)
   {
   int deepest_layer = find_layer_no(root_depth);
   float n_uptake_sum = sum_real_array(dlt_no3gsm, deepest_layer+1)
                     +  sum_real_array(dlt_nh4gsm, deepest_layer+1);
   if (n_uptake_sum > 0)
      n_uptake_sum = - n_uptake_sum;
   else if (n_uptake_sum < 0)
      n_uptake_sum = - n_uptake_sum ;
   else
      n_uptake_sum = 0.0;
   systemInterface->sendVariable(qd, n_uptake_sum);
   }

void RootPart::get_no3_swfac(protocol::Component *systemInterface, protocol::QueryValueData &qd)                          //FIXME - belongs in rootPart
//=======================================================================================
// Getter function for soil n supply (g/m^2)
   {
   float swfac[max_layer];
   for (int layer=0; layer < num_layers; layer++)
      {
      swfac[layer] = pow(divide(sw_avail[layer], sw_avail_pot[layer],0.0), 2);
      swfac[layer] = bound(swfac[layer],0.0,1.0);
      }

   systemInterface->sendVariable(qd, protocol::vector<float>(swfac, swfac+num_layers));
   }


float RootPart::plant_nit_supply(float biomass, float stageNumber, float swdef_fixation)
//=======================================================================================
// Calculate Plant Nitrogen Supply
    {
//+  Local Variables
    float no3gsm_min[max_layer];   // minimum allowable NO3 in soil (g/m^2)
    fill_real_array (no3gsm_min, 0.0, max_layer);

    if (n_uptake_option == 1)
        cproc_n_supply1 (dlayer
                         , dlt_sw_dep
                         , no3gsm
                         , no3gsm_min
                         , root_depth
                         , sw_dep
                         , no3gsm_mflow_avail
                         , sw_avail
                         , no3gsm_diffn_pot
                         , stageNumber
                         , n_fix_rate
                         , biomass
                         , swdef_fixation
                         , &n_fix_pot);

    else if (n_uptake_option == 2)
        cproc_n_supply3 (dlayer
                         , no3gsm
                         , no3gsm_min
                         , no3gsm_uptake_pot
                         , root_depth
                         , root_length
                         , bd
                         , n_stress_start_stage
                         , total_n_uptake_max
                         , no3_uptake_max
                         , no3_conc_half_max
                         , sw_avail_pot
                         , sw_avail
                         , stageNumber
                         , n_fix_rate
                         , biomass
                         , swdef_fixation
                         , &n_fix_pot);

     else if (n_uptake_option == 3)
        {
        float nh4gsm_min[max_layer];   // minimum allowable NH4 in soil (g/m^2)
        fill_real_array (nh4gsm_min, 0.0, max_layer);

        cproc_n_supply4 (dlayer
                             , bd
                             , no3gsm
                             , no3gsm_min
                             , no3gsm_uptake_pot
                             , nh4gsm
                             , nh4gsm_min
                             , nh4gsm_uptake_pot
                             , root_depth
                             , n_stress_start_stage
                             , kno3
                             , no3ppm_min
                             , knh4
                             , nh4ppm_min
                             , total_n_uptake_max
                             , sw_avail_pot
                             , sw_avail
                             , stageNumber
                             , n_fix_rate
                             , biomass
                             , swdef_fixation
                             , &n_fix_pot);
        }
    else
        {
        throw std::invalid_argument ("invalid template N uptake option");
        }
   return n_fix_pot;
   }

void RootPart::plant_nit_uptake(float sumNMax, float sumSoilNDemand, float nDemand)
//=======================================================================================
//       Find nitrogen uptake.
    {
    if (Str_i_Eq(uptake_source, "apsim"))
        {
        // NIH - note that I use a -ve conversion
        // factor FOR NOW to make it a delta.
        plant_get_ext_uptakes(uptake_source.c_str()
                             ,crop_type.c_str()
                             ,"no3"
                             ,-kg2gm/ha2sm
                             ,0.0
                             ,100.0
                             ,dlt_no3gsm);


        }
    else if (n_uptake_option == 1)
        {
        cproc_n_uptake1(no3_diffn_const
                       , dlayer
                       , no3gsm_diffn_pot
                       , no3gsm_mflow_avail
                       , n_fix_pot
                       , n_supply_preference.c_str()
                       , nDemand
                       , sumNMax
                       , root_depth
                       , dlt_no3gsm);
        }
    else if ((n_uptake_option == 2) || (n_uptake_option == 3))
        {
        cproc_n_uptake3(dlayer
                        , no3gsm_uptake_pot
                        , nh4gsm_uptake_pot
                        , n_fix_pot
                        , n_supply_preference.c_str()
                        , sumSoilNDemand
                        , sumNMax
                        , root_depth
                        , dlt_no3gsm
                        , dlt_nh4gsm);
        }
    else
        {
        throw std::invalid_argument ("invalid template option");
        }
    }

//+  Purpose
//       Plant transpiration and soil water extraction
void RootPart::plant_water_uptake (int option, float SWDemand)
    {
    int   layer;                                  // layer number of profile ()
    float ext_sw_supply[max_layer];

    if (Str_i_Eq(uptake_source,"apsim"))
        {
        plant_get_ext_uptakes(uptake_source.c_str()
                             ,crop_type.c_str()
                             ,"water"
                             ,1.0
                             ,0.0
                             ,100.0
                             ,ext_sw_supply);

        for (layer = 0; layer < num_layers; layer++)
           {
           dlt_sw_dep[layer] = -ext_sw_supply[layer];
           }
        }
    else if (option == 1)
        {
        doWaterUptake(SWDemand);
        }
    else
        {
        throw std::invalid_argument ("invalid template option");
        }

    }

// SWIM
void RootPart::plant_get_ext_uptakes (const char *uptake_source,        //(INPUT) uptake flag
                           const char *crop_type,            //(INPUT) crop type name
                           const char *uptake_type,          //(INPUT) uptake name
                           float unit_conversion_factor,     //(INPUT) unit conversion factor
                           float uptake_lbound,              //(INPUT) uptake lower limit
                           float uptake_ubound,              //(INPUT) uptake upper limit
                           float *uptake_array)              //(OUTPUT) crop uptake array

/*  Purpose
*     Ask swim for uptakes of water or solute
*
*  Mission Statement
*   Get the soil uptake for %3 from another module
*
*  Notes
*      Bounds should probably be passed in when crops decide what
*      these should be (ie when ini files have limits for uptake
*      in them)
*
*  Changes
*     08-05-1997 - huth - Programmed and Specified
*     20/5/2003 ad converted to BC++
*/
   {
   char uptake_name[80];             // Uptake variable name
   unsigned layer;
   std::vector<float> values;   // Scratch area

   if (strcmp(uptake_source, "apsim") == 0 && *crop_type != '\0')
      {
      // NB - if crop type is blank then swim will know nothing
      // about this crop (eg if not initialised yet)

      sprintf(uptake_name, "uptake_%s_%s", uptake_type, crop_type);

      scienceAPI.get(uptake_name, "", values, uptake_lbound, uptake_ubound);
      for (layer=0; layer< values.size(); layer++)
         uptake_array[layer] = values[layer] * unit_conversion_factor;
      }
   }



float RootPart::peswTotal()
//=======================================================================================
// Calculate total plant extractable soil water.
   {
   vector<float> pesw;
   int deepest_layer = find_layer_no(root_depth, dlayer, max_layer);
   for (int layer = 0; layer <= deepest_layer; layer++)
      {
      pesw.push_back(sw_dep[layer] - ll_dep[layer]);
      pesw[layer] = l_bound (pesw[layer], 0.0);
      }
   return std::accumulate(pesw.begin(), pesw.end(), 0.0);
   }

float RootPart::pesw(int depth)
//=======================================================================================
// Calculate plant extractable soil water at the given depth.
   {
   int layerNo = find_layer_no(depth);
   return divide (sw_dep[layerNo] - ll_dep[layerNo], dlayer[layerNo], 0.0);
   }

float RootPart::dltSwDep()
//=======================================================================================
// Calculate total plant extractable soil water.
   {
   int deepest_layer = find_layer_no (root_depth, dlayer, max_layer);
   return -sum_real_array (dlt_sw_dep, deepest_layer+1);
   }

float RootPart::nUptake()
//=======================================================================================
// find the proportion of uptake to be distributed
   {
   int deepest_layer = find_layer_no (root_depth, dlayer, max_layer);
   float nUptakeSum = - sum_real_array (dlt_no3gsm, deepest_layer+1)
                      - sum_real_array (dlt_nh4gsm, deepest_layer+1);
   return nUptakeSum;
   }

float RootPart::fasw(int depth)
//=======================================================================================
// calculate the fraction of available soil water at the given depth (mm)
   {
   int layerNo = find_layer_no(depth);
   float fasw = divide (sw_dep[layerNo] - ll_dep[layerNo],
                        dul_dep[layerNo] - ll_dep[layerNo], 0.0);
   return bound (fasw, 0.0, 1.0);
   }

float RootPart::SWDefExpansion(float sw_demand)
//==========================================================================
// Get the soil water to demand ratio and calculate the 0-1 stress factor for leaf expansion.
// 1 is no stress, 0 is full stress.
   {
   if (sw_demand > 0.0)
      {
      int deepest_layer = find_layer_no(root_depth, dlayer, max_layer);

      // get potential water that can be taken up when profile is full
      float sw_supply_sum = sum_real_array (sw_supply, deepest_layer+1);
      float sw_demand_ratio = divide (sw_supply_sum, sw_demand, 10.0);
      return linear_interp_real (sw_demand_ratio, x_sw_demand_ratio,
                                 y_swdef_leaf, num_sw_demand_ratio);
      }
   else
      return 1.0;
   }

float RootPart::SWDefPhoto(float sw_demand)
//========================================================================
// Calculate the soil water supply to demand ratio and therefore the 0-1 stress factor
//       for photosysnthesis. 1 is no stress, 0 is full stress.
   {
   if (root_depth > 0.0)
      {
      int deepest_layer = find_layer_no (root_depth, dlayer, max_layer);

      //get potential water that can be taken up when profile is full
      float sw_uptake_sum = -sum_real_array (dlt_sw_dep, deepest_layer+1);
      float sw_demand_ratio = divide (sw_uptake_sum, sw_demand, 1.0);
      return bound (sw_demand_ratio , 0.0, 1.0);
      }
   else
      return 1.0;
   }


float RootPart::SWDefPheno(int num_sw_avail_ratio,
                           float x_sw_avail_ratio[],
                           float y_swdef_pheno[])
//=========================================================================
// Get the soil water availability ratio in the root zone
// and calculate the 0 - 1 stress factor for phenology.
// 1 is no stress, 0 is full stress.
   {
   if (root_depth > 0.0)
      {
      int deepest_layer = find_layer_no (root_depth, dlayer, max_layer);
      float sw_avail_pot_sum = sum_real_array (sw_avail_pot, deepest_layer+1);
      float sw_avail_sum = sum_real_array (sw_avail, deepest_layer+1);
      float sw_avail_ratio = divide (sw_avail_sum, sw_avail_pot_sum, 1.0);
      sw_avail_ratio = bound (sw_avail_ratio , 0.0, 1.0);
      return linear_interp_real(sw_avail_ratio, x_sw_avail_ratio,
                                y_swdef_pheno, num_sw_avail_ratio);
      }
   else
      return 1.0;
   }

float RootPart::SWDefFixation()
//==========================================================================
// Get the soil water availability ratio in the root zone and
// calculate the 0 - 1 stress factor for fixation.
// 1 is no stress, 0 is full stress.
   {
   if (root_depth > 0.0)
      {
      int deepest_layer = find_layer_no(root_depth, dlayer, max_layer);

      // get potential water that can be taken up when profile is full
      float sw_avail_pot_sum = sum_real_array(sw_avail_pot, deepest_layer+1);
      float sw_avail_sum = sum_real_array(sw_avail, deepest_layer+1);
      float sw_avail_ratio = divide(sw_avail_sum, sw_avail_pot_sum, 1.0);
      sw_avail_ratio = bound(sw_avail_ratio , 0.0, 1.0);
      return linear_interp_real(sw_avail_ratio, x_sw_avail_fix, y_swdef_fix,
                                num_sw_avail_fix);
      }
   else
      return 1.0;
   }



//=========================================================================
void crop_check_sw(plantInterface *iface,
                   float minsw,    // (INPUT)  lowest acceptable value for ll
                   float *dlayer,   // (INPUT)  thickness of soil layer I (mm)
                   float *dul_dep,  // (INPUT)  drained upper limit soil water content for soil layer L (mm water)
                   float *sw_dep,   // (INPUT)  soil water content of layer L (mm)
                   float *ll_dep)   // (INPUT)  lower limit of plant-extractable soil water
                                    //          for soil layer L (mm)
//=========================================================================
/*  Purpose
*       Check validity of soil water parameters for all soil profile layers.
*
*  Mission Statement
*       Check validity of soil water parameters for all soil profile layers.
*
*  Notes
*           Reports an error if
*           - ll_dep and dul_dep are not in ascending order
*           - ll is below c_minsw
*           - sw < c_minsw
*
*  Changes
*     21/5/2003 ad converted to BC++
*     010994 jngh specified and programmed
*     970216 slw generalised to avoid common blocks
*/
   {
   //  Local Variables
   float dul;              // drained upper limit water content of layer (mm water/mm soil)
   char err_msg[80];          // error message
   int layer;              // layer number
   float ll;               // lower limit water content of layer (mm water/mm soil)
   float sw;               // soil water content of layer l (mm water/mm soil)
   int num_layers;
   // Implementation Section ----------------------------------

   num_layers = count_of_real_vals (dlayer, max_layer);  //XX index_of_real_vals
   for(layer = 0; layer <= num_layers; layer++)
      {
      sw = divide (sw_dep[layer], dlayer[layer], 0.0);
      dul = divide (dul_dep[layer], dlayer[layer], 0.0);
      ll = divide (ll_dep[layer], dlayer[layer], 0.0);

      if (ll < minsw)
         {
         sprintf(err_msg,
            " lower limit of %8.2f in layer %d\n         is below acceptable value of %8.2f",
            ll, layer, minsw);
         iface->warningError (err_msg);
         }
      else
         {
         }

      if (dul < ll)
         {

         sprintf(err_msg,
            " Drained upper limit of %8.2f in layer %d\n         is at or below lower limit of %8.2f",
            dul,layer, ll);
         iface->warningError (err_msg);
         }
      else
         {
         }
      if (sw < minsw)
         {
         sprintf(err_msg,
            " Soil water of %8.2f in layer %d\n         is below acceptable value of %8.2f",
            sw, layer, minsw);
         iface->warningError (err_msg);
         }
      else
         {
         }
      }

   }



void RootPart::potentialExtractableSW()
//===========================================================================
// Return potential available soil water from each layer in the root zone.
   {
   fill_real_array (sw_avail_pot, 0.0, max_layer);

   int deepest_layer = find_layer_no (root_depth, dlayer, max_layer);
   for (int layer = 0; layer <= deepest_layer; layer++)
      sw_avail_pot[layer] = dul_dep[layer] - ll_dep[layer];

   // correct bottom layer for actual root penetration
   sw_avail_pot[deepest_layer] = sw_avail_pot[deepest_layer] * root_proportion(deepest_layer);
   }

void RootPart::SWAvailable()
//===========================================================================
// Return actual water available for extraction from each layer in the
// soil profile by the crop (mm water)
   {
   fill_real_array (sw_avail, 0.0, max_layer);

   int deepest_layer = find_layer_no (root_depth, dlayer, max_layer);
   for(int layer = 0; layer <= deepest_layer; layer++)
      {
      sw_avail[layer] = sw_dep[layer] - ll_dep[layer];
      sw_avail[layer] = l_bound (sw_avail[layer], 0.0);
      }
   // correct bottom layer for actual root penetration
   sw_avail[deepest_layer] = sw_avail[deepest_layer] * root_proportion(deepest_layer);
   }


void RootPart::SWSupply()
//=========================================================================
// Return potential water uptake from each layer of the soil profile
// by the crop (mm water). This represents the maximum amount in each
// layer regardless of lateral root distribution but takes account of
// root depth in bottom layer.
   {
   fill_real_array (sw_supply, 0.0, max_layer);

   int deepest_layer = find_layer_no (root_depth, dlayer, max_layer);
   float sw_avail;
   for(int i = 0; i <= deepest_layer; i++)
      {
      sw_avail = (sw_dep[i] - ll_dep[i]);
      sw_supply[i] = sw_avail * kl[i];
      sw_supply[i] = l_bound (sw_supply[i], 0.0);
      }
   //now adjust bottom layer for depth of root
   sw_supply[deepest_layer] = sw_supply[deepest_layer] * root_proportion(deepest_layer);
   }

void RootPart::rootDist(float root_sum, vector<float>& rootArray)
//=========================================================================
// Distribute root material over profile based upon root
// length distribution.
   {
   int deepest_layer = find_layer_no (root_depth, dlayer, max_layer);

   float root_length_sum = sum_real_array (root_length, deepest_layer+1);

   for (int layer = 0; layer <= deepest_layer; layer++)
      rootArray.push_back(root_sum * divide (root_length[layer], root_length_sum, 0.0));
   }


