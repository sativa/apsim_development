#include "StdPlant.h"

#include "RootPart.h"
#include "RootGrowthOption1.h"
#include "RootGrowthOption2.h"
#include "Environment.h"
#include "Soil.h"
#include <numeric>
using namespace std;

string IncorpFOMType = protocol::DDML(protocol::IncorpFomType());
string floatArrayType = protocol::DDML(vector<float>());


RootPart::RootPart(ScienceAPI& scienceAPI, plantInterface *p, const string &name)
   : RootBase(scienceAPI, p, name), soil(scienceAPI)
//=======================================================================================
// Constructor
   {
   incorp_fom_ID = 0;


   zeroAllGlobals();
   }


void RootPart::zeroAllGlobals(void)
//=======================================================================================
// Zero all global values
   {
   SimplePart::zeroAllGlobals();
   root_depth            = 0.0;
   initialRootDepth = 0.0;
   rootDieBackFraction = 0.0;
   specificRootLength = 0.0;

   fill_real_array (root_length , 0.0, max_layer);
   fill_real_array (root_length_senesced, 0.0, max_layer);
   n_conc_min = n_conc_crit = n_conc_max = 0.0;




      uptake_source = "";

      no3_diffn_const = 0.0;
      no3_uptake_max = 0.0;
      no3_conc_half_max = 0.0;
   }

void RootPart::zeroDeltas(void)
//=======================================================================================
// Zero all daily deltas
   {
   SimplePart::zeroDeltas();
   soil.ZeroDeltas();

   dltRootDepth = 0.0;
   setTo(dltRootLength, (float)0.0);
   setTo(dltRootLengthSenesced, (float)0.0);
   setTo(dltRootLengthDead, (float)0.0);

   }

void RootPart::onInit1(protocol::Component *system)
//=======================================================================================
// Perform all component initialisation.
   {
   SimplePart::onInit1(system);
   soil.onInit1 (system);

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
   setupGetFunction(system, "ll", protocol::DTsingle, true,
                    &RootPart::get_ll,"%vol","Crop lower limit");
   setupGetFunction(system, "n_supply_soil", protocol::DTsingle, false,
                    &RootPart::get_n_supply_soil,
                    "g/m^2", "N supply");

   }

void RootPart::read()
//=======================================================================================
// Read all parameters
   {
   //SimplePart::readConstants(NULL, "");
   //SimplePart::readSpeciesParameters(NULL, vector<string>());
   //SimplePart::readCultivarParameters(NULL, "");

   scienceAPI.readOptional("crop_type", crop_type);
   scienceAPI.read("n_supply_preference", n_supply_preference);

   // Read species-specific parameters

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


   soil.Read();
   }

void RootPart::write()
//=======================================================================================
// Write all parameters as a summary to stdout.
   {
   soil.write();
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
   int n = soil.num_layers;
   dltRootLength.clear(); dltRootLength.resize(n);
   dltRootLengthDead.clear(); dltRootLengthDead.resize(n);
   dltRootLengthSenesced.clear(); dltRootLengthSenesced.resize(n);
   }

void RootPart::onGermination(void)
//=======================================================================================
// Germination Event Handler
   {
   SimplePart::onGermination();
   root_depth = initialRootDepth;
   DMPlantMin = 0.0;
   }

void RootPart::onEmergence(void)
//=======================================================================================
//     Initialise crop root length at emergence based on root weight
//     at emergence and specific root length.
   {
   SimplePart::onEmergence();
   DMPlantMin = 0.0;

   // initial root length (mm/mm^2)
   float initial_root_length = Green.DM() / sm2smm * specificRootLength;

   // initial root length density (mm/mm^3)
   float rld = divide (initial_root_length, root_depth, 0.0);

   int deepest_layer = soil.find_layer_no (root_depth);

   for (int layer = 0; layer <= deepest_layer; layer++)
      root_length[layer] = rld *soil.dlayer[layer] * soil.root_proportion (layer, root_depth);
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
   float dlt_dm_die = Green.DM() * rootDieBackFraction;
   float dlt_n_die = dlt_dm_die * c.n_sen_conc;
   float dlt_p_die = Green.P() * rootDieBackFraction;

   Biomass Dead(dlt_dm_die, dlt_n_die, dlt_p_die);

   Green = Green - Dead;
   Senesced = Senesced + Dead;

   // Unlike above ground parts, no roots go to surface residue module.
   dm_type.push_back(myName);
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
   float dlt_dm_sen = Green.DM() * rootDieBackFraction;
   float dlt_n_sen =  (Green.DM() - dlt_dm_sen) * rootDieBackFraction * c.n_sen_conc;
   float dlt_p_sen =  Green.P() * rootDieBackFraction;

   Biomass Dead(dlt_dm_sen, dlt_n_sen, dlt_p_sen);
   Green = Green - Dead;
   Senesced = Senesced + Dead;

   SimplePart::onKillStem();
   }

void RootPart::plant_root_depth (void)
//=======================================================================================
//  Calculate change in plant rooting depth
   {
   const Environment *e = &plant->environment();
   //Temperature factor
   float avg_temp = (e->mint() + e->maxt())/2.0;
   float temp_factor = rel_root_advance.value(avg_temp);

   //Water stress factor
   float ws_factor = ws_root_fac.value (plant->getSwdefPhoto());

   //Soil water availability factor
   int deepest_layer = soil.num_layers-1;

   //  the layer with root front
   int layer = soil.find_layer_no(root_depth);

   float cum_depth = sum_real_array(soil.dlayer, layer+1);
   float rootdepth_in_layer = soil.dlayer[layer] - (cum_depth - root_depth);

   rootdepth_in_layer = bound (rootdepth_in_layer, 0.0, soil.dlayer[layer]);

   float weighting_factor = divide (rootdepth_in_layer, soil.dlayer[layer], 0.0);

   int next_layer = min(layer+1, deepest_layer);

   float fasw1 = soil.layer_fasw(layer);

   float fasw2 = soil.layer_fasw(next_layer);

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
                         soil.xf[layer];

   // prevent roots partially entering layers where xf == 0
   for (deepest_layer = soil.xf.size()-1;
        deepest_layer >= 0 && soil.xf[deepest_layer] <= 0.0;
        deepest_layer--)
      ; /* nothing */

   float root_depth_max = sum_real_array (soil.dlayer, deepest_layer+1);
   dltRootDepth = u_bound ( dltRootDepth, root_depth_max - root_depth);

   if (dltRootDepth < 0.0) throw std::runtime_error("negative root growth??") ;
   }

void RootPart::update(void)
//=======================================================================================
// Update Daily State
   {
   SimplePart::update();
   root_depth += dltRootDepth;

   for (int layer = 0; layer < soil.num_layers; layer++)
      root_length[layer] += dltRootLength[layer];

   for (int layer = 0; layer < soil.num_layers; layer++)
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
    for (int layer = 0; layer < soil.num_layers; layer++)
        {
        dltRootLengthDead[layer] = root_length[layer] * dying_fract_plants;
        root_length[layer] -= dltRootLengthDead[layer];
        root_length_senesced[layer] += dltRootLengthDead[layer];
        }

   bound_check_real_var(scienceAPI, root_depth, 0.0
                        , sum_real_array (soil.dlayer, max_layer)
                        , "root_depth");
   }

void RootPart::sen_length(void)
//=======================================================================================
//     Calculate root length senescence based upon changes in senesced root
//     biomass and the specific root length.
   {
   setTo (dltRootLengthSenesced, (float) 0.0);
   float senesced_length = Senescing.DM() / sm2smm * specificRootLength;
   root_dist(senesced_length, dltRootLengthSenesced);
   }

void RootPart::root_dist(float root_sum, vector<float> &root_array)           //(INPUT) Material to be distributed
//=========================================================================
//       Distribute root material over profile based upon root
//       length distribution.
   {
   // distribute roots over profile to root_depth
   int deepest_layer = soil.find_layer_no (root_depth);
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
   int deepest_layer = soil.find_layer_no (root_depth);
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
   root_incorp (Green.DM() , Green.N(), Green.P());
   root_incorp_dead (Senesced.DM(), Senesced.N(), Senesced.P());
   //root_incorp_dead (dmDead(), nDead(), pDead());

   Senesced.Clear();
   Green.Clear();

   }

void RootPart::updateOthers(void)
//=======================================================================================
// dispose of detached material from dead & senesced roots into FOM pool
   {
   root_incorp (Detaching.DM(),
                Detaching.N(),
                Detaching.P());
   }


void RootPart::root_incorp (float  dlt_dm_root,                  // (INPUT) root residue dm (g/m^2)
                                 float  dlt_N_root,                   // (INPUT) root residue N (g/m^2)
                                 float  dlt_P_root)                   // (INPUT) root residue P (g/m^2)
   //=======================================================================================
   //       Add root DM, N & P to FOM pool
   {
   if (dlt_dm_root>0.0)
      {
      vector<float> dlt_dm_incorp(soil.num_layers); // root residue (kg/ha)
      vector<float> dlt_N_incorp(soil.num_layers);  // root residue N (kg/ha)
      vector<float> dlt_P_incorp(soil.num_layers);  // root residue P (kg/ha)

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
      vector<float> dlt_dm_incorp(soil.num_layers); // root residue (kg/ha)
      vector<float> dlt_N_incorp(soil.num_layers);  // root residue N (kg/ha)
      vector<float> dlt_P_incorp(soil.num_layers);  // root residue P (kg/ha)

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




void RootPart::doNConccentrationLimits(float)
//==========================================================================
// N targets are static - override SimplePart's implementation
   {
   SimplePart::g.n_conc_crit = RootPart::n_conc_crit;
   SimplePart::g.n_conc_min =  RootPart::n_conc_min;
   SimplePart::g.n_conc_max =  RootPart::n_conc_max;
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
    system->sendVariable(qd, protocol::vector<float>(root_length,root_length+soil.num_layers));
}

void RootPart::get_rlv(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
// Getter Function for Root Length Volume
{
    float rlv[max_layer];
    for (int layer = 0; layer < soil.num_layers; layer++)
       {
       rlv[layer] = divide (root_length[layer], soil.dlayer[layer], 0.0);
       }
    system->sendVariable(qd, protocol::vector<float>(rlv,rlv+soil.num_layers));
}

void RootPart::get_root_length_senesced(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
// Getter Function for dead plant root length
{
    system->sendVariable(qd, protocol::vector<float>(root_length_senesced, root_length_senesced+soil.num_layers));
}

void RootPart::get_no3gsm_uptake_pot(protocol::Component *system, protocol::QueryValueData &qd)                  //FIXME - belongs in rootPart
   {
   system->sendVariable(qd, protocol::vector<float>(soil.no3gsm_uptake_pot, soil.no3gsm_uptake_pot+soil.num_layers));
   }

void RootPart::get_nh4gsm_uptake_pot(protocol::Component *system, protocol::QueryValueData &qd)                  //FIXME - belongs in rootPart
   {
   system->sendVariable(qd, protocol::vector<float>(soil.nh4gsm_uptake_pot, soil.nh4gsm_uptake_pot+soil.num_layers));
   }

void RootPart::checkBounds(void)
//=======================================================================================
// Check for data outside of realistic bounds
   {
   if (root_depth < 0)
     throw std::runtime_error(myName + " depth is negative! (" + ftoa(root_depth,".4") +")");
   for (int layer = 0; layer < soil.num_layers; layer++)
      {
      if (root_length[layer] < 0)
         throw std::runtime_error(myName + " length in layer " + itoa(layer+1) + " is negative! (" + ftoa(root_length[layer],".4") +")");
      if (root_length_senesced[layer] < 0)
         throw std::runtime_error(myName + " length dead in layer " + itoa(layer+1) + " is negative! (" + ftoa(root_length[layer],".4") +")");
      }
   }

void RootPart::getOtherVariables()
//=======================================================================================
// Get data from other modules as required
   {
   soil.getOtherVariables();
   }


float RootPart::waterUptake (void)
//=======================================================================================
// Return the total daily water uptake from this root system
   {
   return soil.waterUptake();
   }


void RootPart::doPlantWaterStress (float sw_demand, SWStress *swStress)
//     ===========================================================
//         Get current water stress factors (0-1)
   {
    swStress->doPlantWaterStress (sw_demand);
   }



float RootPart::wet_root_fr (void)
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
         wet_root_fr = wet_root_fr + soil.WFPS(layer) * root_fr[layer];
         }
      return wet_root_fr;
      }
   else
      return 0.0;
   }

void RootPart::removeBiomass2(float chop_fr)
//=======================================================================================
// Remove biomass from the root system due to senescence or plant death
   {
   float dlt_dm_die = Green.DM() * rootDieBackFraction * chop_fr;
   float dlt_n_die = dlt_dm_die * c.n_sen_conc;

   Biomass Dead(dlt_dm_die, dlt_n_die, 0);
   Green = Green - Dead;
   Senesced = Senesced + Dead;

   /// WHY NO P?????????

      // do root_length
   vector<float> dltRootLengthDie;
   dltRootLengthDie.clear(); dltRootLengthDie.resize(soil.num_layers);
   setTo (dltRootLengthDie, (float) 0.0);
   float Die_length = dlt_dm_die / sm2smm * specificRootLength;
   root_dist(Die_length, dltRootLengthDie);
   for (int layer = 0; layer < soil.num_layers; layer++)
      root_length[layer] -= dltRootLengthDie[layer];

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

   pesw = soil.sw_dep[layer] - soil.ll_dep[layer];
   pesw_capacity = soil.dul_dep[layer] - soil.ll_dep[layer];
   sw_avail_ratio = divide (pesw, pesw_capacity, 10.0);
   return sw_avail_ratio;
   }



void RootPart::UpdateOtherVariables()
//=======================================================================================
// Update data owned by other modules that has changed due to calculations by this root system
   {
   soil.UpdateOtherVariables(uptake_source);
   }

void RootPart::get_sw_uptake(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
// Getter Function for Soil Water uptake
{
    float rwu[max_layer];
    for (int layer = 0; layer < soil.num_layers; layer++)
        {
        rwu[layer] = fabs(soil.dlt_sw_dep[layer]);
        }
    system->sendVariable(qd, protocol::vector<float>(rwu, rwu+soil.num_layers));
}


void RootPart::get_sw_supply(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
// Getter Function for Total Profile Soil Water Supply
{
    int deepest_layer = soil.find_layer_no (root_depth);
    float sw_supply_sum = sum_real_array (soil.sw_supply, deepest_layer+1);
    system->sendVariable(qd, sw_supply_sum);
}

void RootPart::get_sw_supply_layr(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
// Getter function for soil water supply from each layer
{
    system->sendVariable(qd, protocol::vector<float>(soil.sw_supply, soil.sw_supply+soil.num_layers));
}

void RootPart::get_ep(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
// Getter Function for plant EP
{
    float sum = 0.0;
    for (int layer = 0; layer < soil.num_layers; layer++)
        {
        sum = sum + fabs(soil.dlt_sw_dep[layer]);
        }
    system->sendVariable(qd, sum);
}

void RootPart::get_esw_layr(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
// Getter function for extractable soil water content of each layer
{
    float esw_layr[max_layer];
    for (int layer = 0; layer < soil.num_layers; layer++)
       {
       esw_layr[layer] = l_bound (soil.sw_dep[layer] - soil.ll_dep[layer], 0.0);
       }
    system->sendVariable(qd, protocol::vector<float>(esw_layr,esw_layr+soil.num_layers));
}
void RootPart::get_no3_uptake(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
// Getter function for no3 uptake from each soil layer
{
    float no3_uptake[max_layer];
    fill_real_array(no3_uptake,0.0, max_layer);
    for (int layer = 0; layer < soil.num_layers; layer++) {
       no3_uptake[layer] =  soil.dlt_no3gsm[layer] * gm2kg/sm2ha;
    }
    system->sendVariable(qd, protocol::vector<float>(no3_uptake, no3_uptake+soil.num_layers));
}

void RootPart::get_nh4_uptake(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
// Getter function for nh4 uptake from each soil layer
{
    float nh4_uptake[max_layer];
    fill_real_array(nh4_uptake,0.0, max_layer);
    for (int layer = 0; layer <= soil.num_layers; layer++) {
       nh4_uptake[layer] =  soil.dlt_nh4gsm[layer] * gm2kg/sm2ha;
    }
    system->sendVariable(qd, protocol::vector<float>(nh4_uptake, nh4_uptake+soil.num_layers));
}

void RootPart::get_no3_tot(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
// Getter Function for total profile no3 uptake
{
    int deepest_layer = soil.find_layer_no (root_depth);
    float no3gsm_tot = sum_real_array (soil.no3gsm, deepest_layer+1);
    system->sendVariable(qd, no3gsm_tot);
}

void RootPart::get_ll(protocol::Component *systemInterface, protocol::QueryValueData &qd)
//=======================================================================================
// Getter function for crop lower limit (volumetric)
   {
   vector<float> ll;
   for(int layer = 0; layer < soil.num_layers; layer++)
      ll.push_back(soil.ll_dep[layer] / soil.dlayer[layer]);
   systemInterface->sendVariable(qd, ll);
   }

void RootPart::get_n_supply_soil(protocol::Component *systemInterface, protocol::QueryValueData &qd)    //FIXME - belongs in rootPart
//=======================================================================================
// Getter function for soil n supply (g/m^2)
   {
   int deepest_layer = soil.find_layer_no(root_depth);
   float n_uptake_sum = sum_real_array(soil.dlt_no3gsm, deepest_layer+1)
                     +  sum_real_array(soil.dlt_nh4gsm, deepest_layer+1);
   if (n_uptake_sum > 0)
      n_uptake_sum = - n_uptake_sum;
   else if (n_uptake_sum < 0)
      n_uptake_sum = - n_uptake_sum ;
   else
      n_uptake_sum = 0.0;
   systemInterface->sendVariable(qd, n_uptake_sum);
   }

float RootPart::plant_nit_supply(float biomass, float stageNumber, float swdef_fixation)
//=======================================================================================
// Calculate Plant Nitrogen Supply
    {
//+  Local Variables
    float no3gsm_min[max_layer];   // minimum allowable NO3 in soil (g/m^2)
    fill_real_array (no3gsm_min, 0.0, max_layer);

    if (n_uptake_option == 1)
        cproc_n_supply1 (soil.dlayer
                         , soil.dlt_sw_dep
                         , soil.no3gsm
                         , no3gsm_min
                         , root_depth
                         , soil.sw_dep
                         , soil.no3gsm_mflow_avail
                         , soil.sw_avail
                         , soil.no3gsm_diffn_pot
                         , stageNumber
                         , n_fix_rate
                         , biomass
                         , swdef_fixation
                         , &n_fix_pot);

    else if (n_uptake_option == 2)
        cproc_n_supply3 (soil.dlayer
                         , soil.no3gsm
                         , no3gsm_min
                         , soil.no3gsm_uptake_pot
                         , root_depth
                         , root_length
                         , soil.bd
                         , n_stress_start_stage
                         , total_n_uptake_max
                         , no3_uptake_max
                         , no3_conc_half_max
                         , soil.sw_avail_pot
                         , soil.sw_avail
                         , stageNumber
                         , n_fix_rate
                         , biomass
                         , swdef_fixation
                         , &n_fix_pot);

     else if (n_uptake_option == 3)
        {
        float nh4gsm_min[max_layer];   // minimum allowable NH4 in soil (g/m^2)
        fill_real_array (nh4gsm_min, 0.0, max_layer);

        cproc_n_supply4 (soil.dlayer
                             , soil.bd
                             , soil.no3gsm
                             , no3gsm_min
                             , soil.no3gsm_uptake_pot
                             , soil.nh4gsm
                             , nh4gsm_min
                             , soil.nh4gsm_uptake_pot
                             , root_depth
                             , n_stress_start_stage
                             , kno3
                             , no3ppm_min
                             , knh4
                             , nh4ppm_min
                             , total_n_uptake_max
                             , soil.sw_avail_pot
                             , soil.sw_avail
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

void RootPart::doNUptake(float sumNMax, float sumSoilNDemand, float nDemand)
//=======================================================================================
//       Find nitrogen uptake.
    {
    if (Str_i_Eq(uptake_source, "apsim"))
        {
        // NIH - note that I use a -ve conversion
        // factor FOR NOW to make it a delta.
        soil.plant_get_ext_uptakes(uptake_source.c_str()
                             ,crop_type.c_str()
                             ,"no3"
                             ,-kg2gm/ha2sm
                             ,0.0
                             ,100.0
                             ,soil.dlt_no3gsm);


        }
    else if (n_uptake_option == 1)
        {
        cproc_n_uptake1(no3_diffn_const
                       , soil.dlayer
                       , soil.no3gsm_diffn_pot
                       , soil.no3gsm_mflow_avail
                       , n_fix_pot
                       , n_supply_preference.c_str()
                       , nDemand
                       , sumNMax
                       , root_depth
                       , soil.dlt_no3gsm);
        }
    else if ((n_uptake_option == 2) || (n_uptake_option == 3))
        {
        cproc_n_uptake3(soil.dlayer
                        , soil.no3gsm_uptake_pot
                        , soil.nh4gsm_uptake_pot
                        , n_fix_pot
                        , n_supply_preference.c_str()
                        , sumSoilNDemand
                        , sumNMax
                        , root_depth
                        , soil.dlt_no3gsm
                        , soil.dlt_nh4gsm);
        }
    else
        {
        throw std::invalid_argument ("invalid template option");
        }
    }

//+  Purpose
//       Plant transpiration and soil water extraction
void RootPart::doWaterUptake (int option, float SWDemand)
    {
    soil.doWaterSupply(root_depth);
    if (Str_i_Eq(uptake_source,"apsim"))
        {
        soil.doWaterUptakeExternal(uptake_source, crop_type);
        }
    else if (option == 1)
        {
        soil.doWaterUptakeInternal(SWDemand, root_depth);
        }
    else
        {
        throw std::invalid_argument ("invalid template option");
        }
    }

// SWIM


float RootPart::peswTotal()
//=======================================================================================
// Calculate total plant extractable soil water.
   {
   return soil.peswTotal(root_depth);
   }

float RootPart::swSupply()
//=======================================================================================
// Calculate total plant extractable soil water.
   {
   return soil.swSupply(root_depth);
   }

float RootPart::swAvailablePotential()
//=======================================================================================
// Calculate total plant potential extractable soil water.
   {
   int deepest_layer = find_layer_no (root_depth, soil.dlayer, max_layer);
   return sum_real_array (soil.sw_avail_pot, deepest_layer+1);
   }


float RootPart::nUptake()
//=======================================================================================
//
   {
   int deepest_layer = find_layer_no (root_depth, soil.dlayer, max_layer);
   float nUptakeSum = - sum_real_array (soil.dlt_no3gsm, deepest_layer+1)
                      - sum_real_array (soil.dlt_nh4gsm, deepest_layer+1);
   return nUptakeSum;
   }



void RootPart::rootDist(float root_sum, vector<float>& rootArray)
//=========================================================================
// Distribute root material over profile based upon root
// length distribution.
   {
   int deepest_layer = find_layer_no (root_depth, soil.dlayer, max_layer);

   float root_length_sum = sum_real_array (root_length, deepest_layer+1);

   for (int layer = 0; layer <= deepest_layer; layer++)
      rootArray.push_back(root_sum * divide (root_length[layer], root_length_sum, 0.0));
   }


float RootPart::pesw(int depth)
//=======================================================================================
// Calculate plant extractable soil water at the given depth.
   {
   return soil.pesw(depth);
   }

float RootPart::fasw(int depth)
   {
   return soil.fasw(depth);
   }
float RootPart::swAvailable()
//=======================================================================================
// Calculate total plant extractable soil water.
   {
   return soil.swAvailable(root_depth);
   }   