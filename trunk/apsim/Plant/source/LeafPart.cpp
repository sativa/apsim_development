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
using namespace std;


void plantLeafPart::onHarvest(float /* cutting_height */, float remove_fr,
                              vector<string> &dm_type,
                              vector<float> &dlt_crop_dm,
                              vector<float> &dlt_dm_n,
                              vector<float> &dlt_dm_p,
                              vector<float> &fraction_to_residue)
{
    float retain_fr_green, retain_fr_sen, retain_fr_dead;

    float dm_init = u_bound(plantPart::c.dm_init * plant->getPlants(), plantPart::g.dm_green);
    float n_init = u_bound(dm_init * plantPart::c.n_init_conc, plantPart::g.n_green);
    float p_init = u_bound(dm_init * plantPart::c.p_init_conc, plantPart::g.p_green);

    retain_fr_green = divide(dm_init, g.dm_green, 0.0);
    retain_fr_sen  = 0.0;
    retain_fr_dead = 0.0;

    float dlt_dm_harvest = g.dm_dead + g.dm_green + g.dm_senesced - dm_init;
    float dlt_n_harvest = g.n_dead + g.n_green + g.n_senesced - n_init;
    float dlt_p_harvest = g.p_dead + g.p_green + g.p_sen - p_init;

    g.dm_dead *= retain_fr_dead;
    g.dm_senesced *= retain_fr_sen;
    g.dm_green *= retain_fr_green;

    g.n_dead *= retain_fr_dead;
    g.n_senesced *= retain_fr_sen;
    g.n_green = n_init;

    g.p_dead *= retain_fr_dead;
    g.p_sen *= retain_fr_sen;
    g.p_green = p_init;

    dm_type.push_back(c.name);
    fraction_to_residue.push_back(1.0 - remove_fr);
    dlt_crop_dm.push_back(dlt_dm_harvest * gm2kg/sm2ha);
    dlt_dm_n.push_back(dlt_n_harvest * gm2kg/sm2ha);
    dlt_dm_p.push_back(dlt_p_harvest * gm2kg/sm2ha);
}

#if 0   
void plantLeafPart::readSpeciesParameters (protocol::Component *system, vector<string> &search_order)
{
    system->readParameter (search_order
                           ,"cLeafNumberAtEmerg"//, "()"
                           , cLeafNumberAtEmerg
                           , 0.0, 100.0);

}
#endif


void plantLeafPart::doRegistrations(protocol::Component *system)
{
   plantPart::doRegistrations(system);
//   setupGetFunction(system, "leaf_no", protocol::DTsingle, false,
//                    &Plant::get_leaf_no, "leaves/plant", "number of leaves per plant");
//
//   system->addGettableVar("node_no",
//               g.node_no, "nodes/plant", "number of mainstem nodes per plant");
//
//   system->addGettableVar("dlt_leaf_no",
//               g.dlt_leaf_no, "leaves/m2", "Change in number of leaves");
//
//   system->addGettableVar("dlt_node_no",
//               g.dlt_node_no, "nodes/m2", "Change in number of nodes");
//
//   setupGetFunction(system, "leaf_no_dead", protocol::DTsingle, false,
//                     &Plant::get_leaf_no_dead, "leaves/m2", "number of dead leaves per square meter");
//
//   setupGetFunction(system, "leaf_area", protocol::DTsingle, true,
//                    &Plant::get_leaf_area, "mm^2", "Leaf area for each node");
//
   setupGetFunction(system, "lai_sum", protocol::DTsingle, false,
                    &plantLeafPart::get_lai_sum, "m^2/m^2", "LAI of all leaf parts");

   setupGetFunction(system, "tlai", protocol::DTsingle, false,
                    &plantLeafPart::get_tlai, "m^2/m^2", "tlai");

   system->addGettableVar("slai", gSLAI, "m^2/m^2", "Senesced lai");

   system->addGettableVar("lai", gLAI, "m^2/m^2", "Leaf area index");

   system->addGettableVar("dlt_lai_pot", dltLAI_pot, "m^2/m^2", "Potential change in live plant lai");

   system->addGettableVar("dlt_lai_stressed", dltLAI_stressed, "m^2/m^2", "Potential change in lai allowing for stress");

//   parent->addGettableVar("dlt_leaf_no_pot",
//               g.dlt_leaf_no_pot, "m^2/m^2", "Leaf no");
//
   system->addGettableVar("tlai_dead", gTLAI_dead, "m^2/m^2", "tlai dead");

//   parent->addGettableVar("dlt_slai_age",
//               g.dlt_slai_age, "m^2/m^2", "Change in lai via age");
//
//   parent->addGettableVar("dlt_slai_light",
//               g.dlt_slai_light, "m^2/m^2", "Change in lai via light");
//
//   parent->addGettableVar("dlt_slai_water",
//               g.dlt_slai_water, "m^2/m^2", "Change in lai via water stress");
//
//   parent->addGettableVar("dlt_slai_frost",
//               g.dlt_slai_frost, "m^2/m^2", "Change in lai via low temperature");
//
//   parent->addGettableVar("leaves_per_node",
//               g.leaves_per_node, "","");
//
//   setupGetFunction(parent, "leaf_area_tot", protocol::DTsingle, false,
//                    &Plant::get_leaf_area_tot,
//                    "m^2", "Total plant leaf area");
//
}

void plantLeafPart::get_tlai(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, gLAI + gSLAI);
}

void plantLeafPart::get_lai_sum(protocol::Component *system, protocol::QueryValueData &qd)
{
    float lai_sum = gLAI + gSLAI + gTLAI_dead;
    system->sendVariable(qd, lai_sum);
}

void plantLeafPart::zeroDeltas(void)
{
   dltLAI = 0.0;
   dltSLAI = 0.0;
   dltLAI_pot = 0.0;
   dltLAI_stressed = 0.0;
   dltTLAI_dead = 0.0;
   dltTLAI_dead_detached = 0.0;
}

void plantLeafPart::zeroAllGlobals(void)
{
   gSLAI = 0.0;
   gLAI = 0.0;
   gTLAI_dead = 0.0;
}

//+  Purpose
//       Leaf number initialisation
void plantLeafPart::onEmergence(void)
   {
   plantPart::onEmergence();

   }

void plantLeafPart::checkBounds(void)
{
   if (gLAI < 0.0) std::runtime_error(c.name + " LAI is negative! " + ftoa(gLAI,".6"));
   if (gSLAI < 0.0) std::runtime_error(c.name + " SLAI is negative! " + ftoa(gSLAI,".6"));
   if (gTLAI_dead < 0.0) std::runtime_error(c.name + " SLAI is negative! " + ftoa(gSLAI,".6"));
}

#if 0
void plantLeafPart::detachment(void)
{

}

//+  Purpose
//       Simulate potential crop leaf area development - may be limited by
//       DM production in subsequent routine
v//+  Purpose
//       Simulate potential stressed crop leaf area development - may
//       be limited by DM production in subsequent routine

void plantLeafPart::leaf_area(void)
{
	// Potential
        cproc_leaf_area_pot1 (c.x_node_no
                              , c.y_leaf_size
                              , c.num_node_no
                              , g.node_no
                              , c.node_no_correction
                              , g.dlt_leaf_no_pot
                              , g.plants
                              , &g.dlt_lai_pot);
        // Stressed 
        cproc_leaf_area_stressed1 (g.dlt_lai_pot
                                   ,g.swdef_expansion
                                   ,min(g.nfact_expansion, g.pfact_expansion)
                                   ,&g.dlt_lai_stressed);

}

//+  Purpose
//       Leaf number development
void plantLeafPart::leaf_number(void)
{
    // plant node/leaf approach
    if (c.leaf_no_pot_option == 1)
        {
        cproc_leaf_no_pot1(c.x_node_no_app
                           , c.y_node_app_rate
                           , c.num_node_no_app
                           , c.x_node_no_leaf
                           , c.y_leaves_per_node
                           ,  c.num_node_no_leaf
                           , phenology->inPhase("node_formation")
                           , phenology->on_day_of("emergence")
                           , g.node_no
                           , phenology->get_dlt_tt()
                           , &g.dlt_leaf_no_pot
                           , &g.dlt_node_no_pot);
        }
    else if (c.leaf_no_pot_option == 2)
        {
        //wheat
        float tiller_no_now =  g.node_no;
        cproc_leaf_no_pot3  (c.x_node_no_app
                             , c.y_node_app_rate
                             , c.num_node_no_app
                             , c.x_node_no_leaf
                             , c.y_leaves_per_node
                             , c.num_node_no_leaf
                             , phenology->inPhase("tiller_formation")
                             , phenology->on_day_of("emergence")
                             , tiller_no_now
                             , phenology->get_dlt_tt()
                             , min(g.nfact_expansion, g.pfact_expansion)
                             , g.swdef_expansion
                             , &g.leaves_per_node
                             , &g.dlt_leaf_no_pot
                             , &g.dlt_node_no_pot);
        }
    else
        {
        throw std::invalid_argument ("invalid template option");
        }
}

//+  Purpose
//       Simulate actual crop leaf area development - checks that leaf area
//       development matches DM production.
void plantLeafPart::leaf_area_actual(void)
{
        // limit the delta leaf area by carbon supply
        cproc_leaf_area_actual1 (c.x_lai
                                 , c.y_sla_max
                                 , c.num_lai
                                 , leafPart->dlt.dm_green
                                 , &g.dlt_lai
                                 , g.dlt_lai_stressed
                                 , g.lai);
}

//+  Purpose
//       Simulate actual crop leaf area development - checks that leaf area
//       development matches DM production.
void plantLeafPart::leaf_no_actual(void)
{
        cproc_leaf_no_actual1(c.num_lai_ratio
                             , c.x_lai_ratio
                             , c.y_leaf_no_frac
                             , g.dlt_lai
                             , g.dlt_lai_stressed
                             , &g.dlt_leaf_no
                             , g.dlt_leaf_no_pot
                             , &g.dlt_node_no
                             , g.dlt_node_no_pot);
}
#endif
