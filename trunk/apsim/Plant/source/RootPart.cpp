#include <stdio.h>
#include <math.h>
#include <map>
#include <string>
#include <stdexcept>

#include <boost/function.hpp>
#include <boost/bind.hpp>

#include <ComponentInterface/Type.h>
#include <ComponentInterface/ApsimVariant.h>
#include <ComponentInterface/Component.h>
#include <ComponentInterface/dataTypes.h>
#include <ComponentInterface/Messages.h>
#include <ComponentInterface/MessageDataExt.h>
#include <ApsimShared/ApsimComponentData.h>
#include <ApsimShared/FStringExt.h>
#include <general/string_functions.h>

#include "PlantInterface.h"
#include "PlantLibrary.h"
#include "Plant.h"
#include "PlantParts.h"
#include "RootPart.h"
using namespace std;

plantRootPart* constructRootPart(plantInterface *p, const string &type, const string &name) 
   {
   if (type == "Jones+RitchieGrowthPattern")
     return new rootGrowthOption2(p, name);
   // default:
   return new rootGrowthOption1(p, name);
   }

void plantRootPart::zeroAllGlobals(void)
//=======================================================================================
   {
   plantPart::zeroAllGlobals();
   root_depth            = 0.0;
   fill_real_array (root_length , 0.0, max_layer);
   fill_real_array (root_length_dead, 0.0, max_layer);
   n_conc_min = n_conc_crit = n_conc_max = 0.0;
   }

void plantRootPart::zeroDeltas(void)
//=======================================================================================
   {
   plantPart::zeroDeltas();
   dltRootDepth = 0.0;
   setTo(dltRootLength, (float)0.0);
   setTo(dltRootLengthSenesced, (float)0.0);
   setTo(dltRootLengthDead, (float)0.0);
   }

void plantRootPart::doRegistrations(protocol::Component *system)
//=======================================================================================
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

   }

// Read Constants
void plantRootPart::readConstants (protocol::Component *system, const string &section)
{
    plantPart::readConstants(system, section);
    // Nothing to do here..
}

void plantRootPart::readSpeciesParameters(protocol::Component *system, vector<string> &sections)
//=======================================================================================
    {
    plantPart::readSpeciesParameters(system, sections);
    system->readParameter (sections,
                           "specific_root_length",//, "(mm/g)"
                            specificRootLength,
                            0.0, 1.0e6);
    system->readParameter (sections
                          , "n_conc_crit_root"//, "()"
                          , n_conc_crit
                          , 0.0, 100.0);

    system->readParameter (sections
                          , "n_conc_max_root"//, "()"
                          , n_conc_max
                          , 0.0, 100.0);

    system->readParameter (sections
                          , "n_conc_min_root"//, "()"
                          , n_conc_min
                          , 0.0, 100.0);

    system->readParameter (sections
                   ,"initial_root_depth"//, "(mm)"
                   , initialRootDepth
                   , 0.0, 1000.0);

    system->readParameter (sections
                   ,"specific_root_length"//, "(mm/g)"
                   , specificRootLength
                   , 0.0, 1.0e6);

    system->readParameter (sections
                   ,"root_die_back_fr"//, "(0-1)"
                   , rootDieBackFraction
                   , 0.0, 0.99);

    rel_root_rate.search(system, sections,
                         "x_plant_rld", "()", 0.0, 0.1,
                         "y_rel_root_rate", "()", 0.001, 1.0);

    sw_fac_root.search(system, sections,
                         "x_sw_ratio", "()", 0.0, 100.,
                         "y_sw_fac_root", "()", 0.0, 100.0);

    rel_root_advance.search(system, sections,
                         "x_temp_root_advance", "(oc)", -10.0, 60.,
                         "y_rel_root_advance", "()", 0.0, 1.0);

    root_depth_rate.search(system, sections,
                         "stage_code_list", "()", 0.0, 100.0,
                         "root_depth_rate", "(mm/day)", 0.0, 1000.0);

   }


void plantRootPart::readRootParameters(protocol::Component *system, const char *section_name)
   {
   system->readParameter (section_name, "xf", xf, 0.0, 1.0);
   if (xf.size() != (unsigned) plant->getEnvironment()->num_layers)
      system->writeString ("Warning!!\nXF parameter doesn't match soil profile?");
   }

void plantRootPart::onSowing(void)
   {
   int n = plant->getEnvironment()->num_layers;
   dltRootLength.clear(); dltRootLength.resize(n);
   dltRootLengthDead.clear(); dltRootLengthDead.resize(n);
   dltRootLengthSenesced.clear(); dltRootLengthSenesced.resize(n);
   }

void plantRootPart::onGermination(void)
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
   float initial_root_length = DMGreen / sm2smm * specificRootLength;

   // initial root length density (mm/mm^3)
   float rld = divide (initial_root_length, root_depth, 0.0);

   int deepest_layer = plant->getEnvironment()->find_layer_no (root_depth);

   for (int layer = 0; layer <= deepest_layer; layer++)
      {
      root_length[layer] = rld *
              plant->getEnvironment()->dlayer[layer] *
                   root_proportion (layer);
      }
   }
void plantRootPart::onFlowering(void)
   {
   DMPlantMin = 0.0; //explicit
   }
void plantRootPart::onStartGrainFill(void)
   {
   DMPlantMin = 0.0;
   }

void plantRootPart::onHarvest(float /*cutting_height*/, float remove_fr,
                              vector<string> &dm_type,
                              vector<float> &dlt_crop_dm,
                              vector<float> &dlt_dm_n,
                              vector<float> &dlt_dm_p,
                              vector<float> &fraction_to_residue)
//=======================================================================================
   {

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

   // XXX?????? NO!!!!!!
   root_depth            = 0.0;
   fill_real_array (root_length , 0.0, max_layer);
   fill_real_array (root_length_dead, 0.0, max_layer);
   }

//+  Purpose
//       Plant root distribution in the soil
void rootGrowthOption1::plant_root_depth (void)
   {
   const environment_t *e = plant->getEnvironment();
   int deepest_layer =e->find_layer_no (root_depth);
   float sw_avail_fac = sw_fac_root.value(e->sw_avail_ratio(deepest_layer));

   float avg_temp = (e->mint + e->maxt)/2.0;

   float temp_factor = rel_root_advance.value(avg_temp);

   // this equation allows soil water in the deepest
   // layer in which roots are growing
   // to affect the daily increase in rooting depth.
   int stage = (int)plant->getStageNumber();
   dltRootDepth  = root_depth_rate.value(stage) *
                       sw_avail_fac *
                         xf[deepest_layer] *
                           temp_factor;

   // XXX is this cap redundant?
   for (deepest_layer = xf.size();
        deepest_layer >= 0 && xf[deepest_layer] <= 0.0;
        deepest_layer--)
      ; /* nothing */

   float root_depth_max = sum (e->dlayer, deepest_layer+1);
   dltRootDepth = u_bound ( dltRootDepth, root_depth_max - root_depth); 

   if (dltRootDepth < 0.0) throw std::runtime_error("negative root growth??") ;
   }

void rootGrowthOption2::plant_root_depth ()
   {
//        cproc_root_depth2 ( phenology->stageNumber()
//                           ,Environment.maxt
//                           ,Environment.mint
//                           ,g.swdef_photo
//                           ,rootPart->root_depth
//                           ,c.num_temp_root_advance
//                           ,c.x_temp_root_advance
//                           ,c.y_rel_root_advance
//                           ,c.num_ws_root
//                           ,c.x_ws_root
//                           ,c.y_ws_root_fac
//                           ,c.num_sw_ratio
//                           ,c.x_sw_ratio
//                           ,c.y_sw_fac_root
//                           ,g.dlayer
//                           ,g.dul_dep
//                           ,g.sw_dep
//                           ,p.ll_dep
//                           ,c.root_depth_rate
//                           ,p.xf
//                           ,&g.dlt_root_depth);
   throw std::runtime_error("root growth option 2 NYI - see pdev");
   }

void plantRootPart::update(void)
   {
   plantPart::update();
   root_depth += dltRootDepth;

   //add_real_array (dltRootLength, root_length);
   for (int layer = 0; layer < plant->getEnvironment()->num_layers; layer++)
      {
      root_length[layer] += dltRootLength[layer];
      if (root_length[layer] < 0)
        throw std::runtime_error("root dltRL " + itoa(layer) + " = " + ftoa(root_length[layer], ".2"));
      }
   //subtract_real_array (dltRootLengthSenesced, root_length);
   for (int layer = 0; layer < plant->getEnvironment()->num_layers; layer++)
      {
      root_length[layer] -= dltRootLengthSenesced[layer];
      if (root_length[layer] < 0)
        throw std::runtime_error("root dltRLS " + itoa(layer) + " = " + ftoa(root_length[layer], ".2"));
      }
   }

void plantRootPart::update2(float dying_fract_plants)
   {
    // Note that movement and detachment of C is already done, just
    // need to maintain relationship between length and mass
    // Note that this is not entirely accurate.  It links live root
    // weight with root length and so thereafter dead(and detaching)
    // root is assumed to have the same distribution as live roots.
    plantPart::update2(dying_fract_plants);

    for (int layer = 0; layer < plant->getEnvironment()->num_layers; layer++)
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
//  Purpose
//       Distribute root material over profile based upon root
//       length distribution.
//
   {
   // distribute roots over profile to root_depth
   int deepest_layer = plant->getEnvironment()->find_layer_no (root_depth);
   float root_length_sum = sum_real_array (root_length, deepest_layer+1);
   for (int layer = 0; layer <= deepest_layer; layer++)
      root_array[layer] = root_sum *
                           divide (root_length[layer], root_length_sum, 0.0);
   }

void plantRootPart::collectDetachedForResidue(vector<string> &part_name
                              , vector<float> &dm_residue
                              , vector<float> &dm_n
                              , vector<float> &dm_p
                              , vector<float> &fraction_to_residue)
//=======================================================================================
// Unlike above ground parts, no roots go to surface residue module.
   {
   plantPart::collectDetachedForResidue(part_name, dm_residue, dm_n, dm_p, fraction_to_residue);
   int end = fraction_to_residue.size()-1;
   fraction_to_residue[end] = 0.0;
   }
   
void plantRootPart::collectDeadDetachedForResidue(vector<string> &part_name
                              , vector<float> &dm_residue
                              , vector<float> &dm_n
                              , vector<float> &dm_p
                              , vector<float> &fraction_to_residue)
//=======================================================================================
// Unlike above ground parts, no roots go to surface residue module.
   {
   plantPart::collectDeadDetachedForResidue(part_name, dm_residue, dm_n, dm_p, fraction_to_residue);
   int end = fraction_to_residue.size()-1; 
   fraction_to_residue[end] = 0.0;
   }
   
void plantRootPart::onEndCrop(vector<string> &dm_type,
                          vector<float> &dlt_crop_dm,
                          vector<float> &dlt_dm_n,
                          vector<float> &dlt_dm_p,
                          vector<float> &fraction_to_residue)
//=======================================================================================
// Unlike above ground parts, no roots go to surface residue module.
   {
   plantPart::onEndCrop(dm_type, dlt_crop_dm, dlt_dm_n, dlt_dm_p, fraction_to_residue);
   int end = fraction_to_residue.size()-1; 
   fraction_to_residue[end] = 0.0;
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
   depth_to_layer_bottom = sum(plant->getEnvironment()->dlayer, layer+1);
   depth_to_layer_top = depth_to_layer_bottom - plant->getEnvironment()->dlayer[layer];
   depth_to_root  = min(depth_to_layer_bottom, root_depth);
   depth_of_root_in_layer = max(0.0, depth_to_root-depth_to_layer_top);

   return (divide (depth_of_root_in_layer, plant->getEnvironment()->dlayer[layer], 0.0));
   }

void plantRootPart::doNConccentrationLimits(void)
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
{
    int num_layers = plant->getEnvironment()->num_layers;
    system->sendVariable(qd, protocol::vector<float>(root_length,root_length+num_layers));
}

void plantRootPart::get_rlv(protocol::Component *system, protocol::QueryValueData &qd)
{
    float rlv[max_layer];
    int num_layers = plant->getEnvironment()->num_layers;
    for (int layer = 0; layer < num_layers; layer++)
       {
       rlv[layer] = divide (root_length[layer], plant->getEnvironment()->dlayer[layer], 0.0);
       }
    system->sendVariable(qd, protocol::vector<float>(rlv,rlv+num_layers));
}

void plantRootPart::get_root_length_dead(protocol::Component *system, protocol::QueryValueData &qd)
{
    int num_layers = plant->getEnvironment()->num_layers;
    system->sendVariable(qd, protocol::vector<float>(root_length_dead, root_length_dead+num_layers));
}

void rootGrowthOption1::root_length_growth (void)
//============================================================================
//  (was cproc_root_length_growth1)
//  Calculate the increase in root length density in each rooted
//  layer based upon soil hospitality, moisture and fraction of
//  layer explored by roots.
   {
   float dlt_length_tot;      // total root length increase (mm/m^2)
   float rlv_factor_tot;      // total rooting factors across profile
   float branching_factor;    //
   float plant_rld;
   float rld;

   setTo(dltRootLength, (float)0.0);

   float depth_today = root_depth + dltRootDepth;
   const environment_t *env = plant->getEnvironment();
   int deepest_layer = env->find_layer_no (depth_today);

   vector<float> rlv_factor(env->num_layers);  // relative rooting factor for all layers
   rlv_factor_tot = 0.0;
   for(int layer = 0; layer <= deepest_layer; layer++)
      {
      rld = divide (root_length[layer], env->dlayer[layer], 0.0);

      plant_rld = divide (rld, plant->getPlants(), 0.0);

      branching_factor = rel_root_rate.value(plant_rld);

      rlv_factor[layer] = sw_fac_root.value(env->sw_avail_ratio(layer)) *
                 branching_factor *                                      // branching factor
                   xf [layer]  *                                       // growth factor
                     divide(env->dlayer[layer],      // space weighting
                            root_depth, 0.0);                            //       factor

      rlv_factor[layer] = l_bound(rlv_factor[layer], 1e-6);
      rlv_factor_tot = rlv_factor_tot + rlv_factor[layer];
      }

   dlt_length_tot = dlt.dm_green/sm2smm * specificRootLength;
   //if (dlt_length_tot < 0.0) throw std::runtime_error("negative root length_tot growth??") ;

   for(int layer = 0; layer <= deepest_layer; layer++)
      {
      dltRootLength[layer] = dlt_length_tot *
                              divide (rlv_factor[layer], rlv_factor_tot, 0.0);
      //if (dltRootLength[layer] < 0.0) throw std::runtime_error("negative root length_layer growth??") ;
      }
                              
}

void rootGrowthOption2::root_length_growth (void)
//=======================================================================================
//   (was cproc_root_length_growth_new)
//   Calculate the increase in root length density in each rooted
//   layer based upon soil hospitality, moisture and fraction of
//   layer explored by roots.
    {
    float dlt_length_tot;                         // total root length increase (mm/m^2)
    int   layer;                                  // simple layer counter variable
    float rlv_factor_tot;                         // total rooting factors across profile
    float branching_factor;                       //
    float plant_rld;
    float rld;
    float root_length_tot;
    float rwf;
    float cum_depth;

   vector<float> rlv_factor(plant->getEnvironment()->num_layers);
   vector<float> root_length_new(plant->getEnvironment()->num_layers);

   setTo(dltRootLength, (float)0.0);

   float depth_today = root_depth + dltRootDepth;
   int deepest_layer = plant->getEnvironment()->find_layer_no (depth_today);

   float cum_layer_depth = sum(plant->getEnvironment()->dlayer, deepest_layer);

   rlv_factor_tot = 0.0;
   cum_depth      = 0.0;

   for (int layer = 0; layer <= deepest_layer; layer++)
      {
      cum_depth +=  0.5 * plant->getEnvironment()->dlayer[layer];
      rwf       = divide (cum_depth, cum_layer_depth, 0.0) ;
      rwf       = pow((1.0 - rwf), rootDistributionPattern);

      rld       = divide (root_length[layer], plant->getEnvironment()->dlayer[layer], 0.0);

      plant_rld = divide (rld, plant->getPlants() ,0.0);

      branching_factor = rel_root_rate.value(plant_rld);

      rlv_factor[layer] = sw_fac_root.value(plant->getEnvironment()->sw_avail_ratio(layer)) *
                            branching_factor *
                             xf [layer] *
                             divide(plant->getEnvironment()->dlayer[layer], root_depth, 0.0) *
                              rwf;

      rlv_factor[layer] = l_bound(rlv_factor[layer], 1e-6);
      rlv_factor_tot += rlv_factor[layer];
      }

   dlt_length_tot = dlt.dm_green/sm2smm * specificRootLength;

   root_length_tot = sum_real_array(root_length, max_layer) + dlt_length_tot;

   for (int layer = 0; layer <= deepest_layer; layer++)
      {
      root_length_new[layer] = root_length_tot *
                                divide (rlv_factor[layer],
                                        rlv_factor_tot, 0.0);

      dltRootLength [layer] = root_length_new[layer] - root_length[layer];

      dltRootLength [layer] = dlt_length_tot *
                                divide (rlv_factor[layer], rlv_factor_tot, 0.0);

      }
   }


void rootGrowthOption2::readSpeciesParameters(protocol::Component *system, vector<string> &sections)
   {
   plantRootPart::readSpeciesParameters(system, sections);
   system->readParameter (sections, "root_distribution_pattern", 
                          rootDistributionPattern, 0.0, 100.0);
   }
void plantRootPart::checkBounds(void)
   {
   if (root_depth < 0) throw std::runtime_error(c.name + " depth is negative! (" + ftoa(root_depth,".4") +")");
   for (int layer = 0; layer < plant->getEnvironment()->num_layers; layer++) 
      {
      if (root_length[layer] < 0) throw std::runtime_error(c.name + " length is negative! (" + ftoa(root_length[layer],".4") +")");
      if (root_length_dead[layer] < 0) throw std::runtime_error(c.name + " length dead is negative! (" + ftoa(root_length[layer],".4") +")");
      }
   }