#include <stdio.h>
#include <math.h>
#include <map>
#include <string>
#include <stdexcept>
#include <sstream>

#include <boost/function.hpp>
#include <boost/bind.hpp>

#include <ComponentInterface/Type.h>
#include <ComponentInterface/ApsimVariant.h>
#include <ComponentInterface/Component.h>
#include <ComponentInterface/datatypes.h>
#include <ComponentInterface/MessageDataExt.h>
#include <ApsimShared/ApsimComponentData.h>
#include <ApsimShared/FStringExt.h>
#include <general/string_functions.h>

#include "PlantInterface.h"
#include "PlantLibrary.h"
#include "Plant.h"
#include "PlantPart.h"
#include "LeafPart.h"
#include "genericLeafPart.h"
using namespace std;

const float  tolerance_lai = 1.0e-4 ;


// Read Constants
void genericLeafPart::readConstants (protocol::Component *system, const string &section)
{
    plantPart::readConstants(system, section);
   // Nothing to do here..
}

// Read species specific parameters
void genericLeafPart::readSpeciesParameters (protocol::Component *system, vector<string> &search_order)
{
    plantPart::readSpeciesParameters(system, search_order);
    system->readParameter (search_order
                           ,"leaf_no_at_emerg"//, "()"
                           , cLeafNumberAtEmerg
                           , 0.0, 100.0);

    system->readParameter (search_order
                   ,"initial_tpla"//, "(mm^2)"
                   , cInitialTPLA
                   , 0.0, 100000.0);

    system->readParameter (search_order
                   ,"min_tpla"//, "(mm^2)"
                   , cMinTPLA
                   , 0.0, 100000.0);

    system->readParameter (search_order
                   ,"sla_min"//, "(mm^2/g)"
                   , cSLAMin
                   , 0.0, 100000.0);

    system->readParameter (search_order
                     ,"sen_start_stage"//, "()"
                     , cSenStartStage
                     , 0.0, 100.0);

    system->readParameter (search_order
                    ,"fr_lf_sen_rate"//, "(/degday)"
                    , cFrLeafSenRate
                    , 0.0, 1.0);

    system->readParameter (search_order
                    ,"node_sen_rate"//, "(degday)"
                    , cNodeSenRate
                    , 0.0, 1000.0);

    system->readParameter (search_order
                   , "n_fact_lf_sen_rate"//, "(/degday)"
                   , cNFactLeafSenRate
                   , 0.0, 5.0);

    system->readParameter (search_order
                   , "node_no_correction"//, "()"
                   , cNodeNoCorrection
                   , 0.0, 10.0);

    cSLAMax.search(system, search_order
                   , "x_lai",  "(mm2/mm2)", 0.0, 15.0
                   , "y_sla_max", "(mm2/g)", 0.0, 2.e5);

    cLeafNoFrac.search(system, search_order
                     ,"x_lai_ratio", "()", 0.0, 1.0
                     ,"y_leaf_no_frac", "()", 0.0, 1.0);

    system->readParameter (search_order
                   , "lai_sen_light"//, "(m^2/m^2)"
                   , cLAISenLight
                   , 3.0, 20.0);

    system->readParameter (search_order
                   , "sen_light_slope"//, "()"
                   , cSenLightSlope
                   , 0.0, 100.0);

    system->readParameter (search_order
                   , "sen_rate_water"//, "()"
                   , cSenRateWater
                   , 0.0, 100.0);

    cSenescenceFac.search (system, search_order
                     , "x_temp_senescence", "(oc)", -20.0, 20.0
                     , "y_senescence_fac", "()", 0.0, 1.0);

    cLeafSize.search(system, search_order
                        , "x_node_no",  "()", 0.0, 100.0
                        , "y_leaf_size", "(mm2)", 0.0, 60000.0);

    cNodeAppRate.search(system, search_order
                        , "x_node_no_app",  "()", 0.0, 200.0
                        , "y_node_app_rate", "()", 0.0, 400.0);

    cLeavesPerNode.search(system, search_order
                        , "x_node_no_leaf",  "()", 0.0, 200.0
                        , "y_leaves_per_node", "()", 0.0, 50.0);

    system->readParameter (search_order
                     ,"x_row_spacing"//,  "(mm)"
                     , cXRowSpacing, cNumRowSpacing
                     , 0.0, 2000.);

    system->readParameter (search_order
                     ,"y_extinct_coef"//, "()"
                     , cYExtinctCoef, cNumRowSpacing
                     , 0.0, 1.0);

    system->readParameter (search_order
                     ,"y_extinct_coef_dead"//, "()"
                     , cYExtinctCoefDead, cNumRowSpacing
                     , 0.0, 1.0);

    cRue.search(system, search_order,
                 "x_stage_rue", "()", 0.0, 1000.0,
                 "y_rue", "(g dm/mj)", 0.0, 1000.0);

   int   numvals;                                // number of values returned
   system->readParameter (search_order,
                          "transp_eff_cf"//, "(kpa)"
                          , c.transpEffCf, numvals
                          , 0.0, 1.0);

}

// Connect our bits to the system
void genericLeafPart::doRegistrations(protocol::Component *system)
{
   plantPart::doRegistrations(system);
   setupGetFunction(system, "leaf_no", protocol::DTsingle, false,
                    &genericLeafPart::get_leaf_no, "leaves/plant", "Number of leaves per plant");

   system->addGettableVar("node_no", gNodeNo, "nodes/plant", "Number of mainstem nodes per plant");

   setupGetFunction(system, "leaf_no_sen", protocol::DTsingle, false,
                     &genericLeafPart::get_leaf_no_sen, "leaves/m2", "Number of senesced leaves per square meter");

   setupGetFunction(system, "leaf_area", protocol::DTsingle, true,
                    &genericLeafPart::get_leaf_area, "mm^2", "Leaf area for each node");

   setupGetFunction(system, "leaf_area_tot", protocol::DTsingle, false,
                    &genericLeafPart::get_leaf_area_tot, "m^2", "Total plant leaf area");

   setupGetFunction(system, "lai_sum", protocol::DTsingle, false,
                    &genericLeafPart::get_lai_sum, "m^2/m^2", "LAI of all leaf components");

   setupGetFunction(system, "tlai", protocol::DTsingle, false,
                    &genericLeafPart::get_tlai, "m^2/m^2", "Total lai");

   system->addGettableVar("slai", gSLAI, "m^2/m^2", "Senesced lai");

   system->addGettableVar("lai", gLAI, "m^2/m^2", "Leaf area index");

   system->addGettableVar("dlt_lai_pot", dltLAI_pot, "m^2/m^2", "Potential change in live plant lai");

   system->addGettableVar("dlt_lai_stressed", dltLAI_stressed, "m^2/m^2", "Potential change in lai allowing for stress");

   system->addGettableVar("dlt_leaf_no", dltLeafNo, "leaves/m2", "Change in number of leaves");

   system->addGettableVar("dlt_node_no", dltNodeNo, "nodes/m2", "Change in number of nodes");

   system->addGettableVar("dlt_leaf_no_pot", dltLeafNoPot, "m^2/m^2", "Potential Leaf no");

   system->addGettableVar("tlai_dead", gTLAI_dead, "m^2/m^2", "tlai dead");

   system->addGettableVar("dlt_slai_age", dltSLAI_age, "m^2/m^2", "Change in lai via age");

   system->addGettableVar("dlt_slai_light", dltSLAI_light, "m^2/m^2", "Change in lai via light");

   system->addGettableVar("dlt_slai_water", dltSLAI_water, "m^2/m^2", "Change in lai via water stress");

   system->addGettableVar("dlt_slai_frost", dltSLAI_frost, "m^2/m^2", "Change in lai via low temperature");

   system->addGettableVar("leaves_per_node", gLeavesPerNode, "","");

}

void genericLeafPart::get_tlai(protocol::Component *system, protocol::QueryValueData &qd)
{
    float tlai = gLAI + gSLAI;
    system->sendVariable(qd, tlai);
}

void genericLeafPart::get_lai_sum(protocol::Component *system, protocol::QueryValueData &qd)
{
    float lai_sum = gLAI + gSLAI + gTLAI_dead;
    system->sendVariable(qd, lai_sum);
}
void genericLeafPart::get_leaf_no(protocol::Component *system, protocol::QueryValueData &qd)
{
   system->sendVariable(qd, this->getLeafNo());
}
float genericLeafPart::getLeafNo(void) const
{
   float sum = 0.0;
   for (int i = 0; i < max_node; i++) sum += gLeafNo[i];
   return sum;
}

void genericLeafPart::get_leaf_area_tot(protocol::Component *system, protocol::QueryValueData &qd)
{
   float sum = 0.0;
   for (int i = 0; i < max_node; i++) sum += gLeafArea[i];
   system->sendVariable(qd, sum);
}

void genericLeafPart::get_leaf_no_sen(protocol::Component *system, protocol::QueryValueData &qd)
{
   float sum = 0.0;
   for (int i = 0; i < max_node; i++) sum += gLeafNoSen[i];
   system->sendVariable(qd, sum);
}

void genericLeafPart::get_leaf_area(protocol::Component *system, protocol::QueryValueData &qd)
{
   system->sendVariable(qd, protocol::vector<float>(gLeafArea, gLeafArea+20/*max_node*/)); // XX system can't handle big arrays..
}

// Clean out yesterday's rate calculations
void genericLeafPart::zeroDeltas(void)
{
   plantPart::zeroDeltas();
   dltLAI = 0.0;
   dltSLAI = 0.0;
   dltLAI_pot = 0.0;
   dltLAI_stressed = 0.0;
   dltTLAI_dead = 0.0;
   dltTLAI_dead_detached = 0.0;
   dltSLAI_detached = 0.0;
   dltSLAI_age = 0.0;
   dltSLAI_light = 0.0;
   dltSLAI_water = 0.0;
   dltSLAI_frost = 0.0;
   dltLeafNo              = 0.0;
//    g.dlt_node_no              = 0.0; JNGH - need to carry this through for site no next day.
   dltLeafNoPot = 0.0;
   dltNodeNoPot = 0.0;
}

// Initialise all constants & parameters
void genericLeafPart::zeroAllGlobals(void)
{
   plantPart::zeroAllGlobals();
   cLeafNumberAtEmerg = 0.0;
   cSLAMin = 0.0;
   cInitialTPLA = 0.0;
   cNodeNoCorrection = 0.0;
   cLAISenLight = 0.0;
   cSenLightSlope = 0.0;

   gSLAI = 0.0;
   gLAI = 0.0;
   gTLAI_dead = 0.0;
   fill_real_array (gLeafNo , 0.0, max_node);
   fill_real_array (gLeafNoSen , 0.0, max_node);
   fill_real_array (gLeafArea , 0.0, max_node);
   gNodeNo = 0.0;
   gLeavesPerNode = 0.0;
   dltNodeNo = 0.0;

   coverLeaf.green = 0.0;
   coverLeaf.sen   = 0.0;
   coverLeaf.dead  = 0.0;

   fill_real_array (cXRowSpacing, 0.0, max_table);
   fill_real_array (cYExtinctCoef, 0.0, max_table);
   fill_real_array (cYExtinctCoefDead, 0.0, max_table);
   cNumRowSpacing = 0;
}

// Leaf, Node number and area initialisation
void genericLeafPart::onEmergence(void)
   {
   plantPart::onEmergence();
   initialiseAreas();
   }

void genericLeafPart::onKillStem(void)
   // transfer plant leaf area
   {
   plantPart::onKillStem();
   float deadLAI = gTLAI_dead + gLAI;
   onEmergence();
   gTLAI_dead = deadLAI;
   }

// Initialise leaf areas to a newly emerged state.
void genericLeafPart::initialiseAreas(void)
   {
   gNodeNo = cLeafNumberAtEmerg;

   fill_real_array (gLeafNo, 0.0, max_node);
   fill_real_array (gLeafNoSen, 0.0, max_node);

   int   leaf_no_emerged = (int) cLeafNumberAtEmerg;
   float leaf_emerging_fract = fmod(cLeafNumberAtEmerg, 1.0);
   for (int leaf = 0; leaf < leaf_no_emerged; leaf++)
      {
      gLeafNo[leaf] = 1.0;
      }
   gLeafNo[leaf_no_emerged] = leaf_emerging_fract;

   fill_real_array (gLeafArea, 0.0, max_node);
   float avg_leaf_area = divide (cInitialTPLA, cLeafNumberAtEmerg, 0.0);
   for (int leaf = 0; leaf < leaf_no_emerged; leaf++)
      {
      gLeafArea[leaf] = avg_leaf_area;
      }
   gLeafArea[leaf_no_emerged] = leaf_emerging_fract * avg_leaf_area;

   gLAI = cInitialTPLA * smm2sm * plant->getPlants();
   gSLAI = 0.0;
   gTLAI_dead = 0.0;
   }

// Harvest event
void genericLeafPart::onHarvest(float /* cutting_height */, float remove_fr,
                              vector<string> &dm_type,
                              vector<float> &dlt_crop_dm,
                              vector<float> &dlt_dm_n,
                              vector<float> &dlt_dm_p,
                              vector<float> &fraction_to_residue)
{
   onHarvest_GenericAboveGroundPart(remove_fr, dm_type, dlt_crop_dm, dlt_dm_n, dlt_dm_p, fraction_to_residue);
   initialiseAreas();
}

// Sanity checks
void genericLeafPart::checkBounds(void)
{
   plantPart::checkBounds();
   if (gLAI < 0.0) throw std::runtime_error(c.name + " LAI is negative! (" + ftoa(gLAI,".6") + ")");
   if (gSLAI < 0.0) throw std::runtime_error(c.name + " SLAI is negative! (" + ftoa(gSLAI,".6") + ")");
   if (gTLAI_dead < 0.0) throw std::runtime_error(c.name + " gTLAI_dead is negative! (" + ftoa(gTLAI_dead,".6") + ")");
   if (gNodeNo < 0) throw std::runtime_error(c.name + " node number is negative! (" + ftoa(gNodeNo,".6") + ")");
   if (gNodeNo >= max_node) throw std::runtime_error(c.name + " node number exceeds array size! (" + ftoa(gNodeNo,".6") + ")");

   //     Check that leaf records agree
   float leaf_area_tot = sum_real_array (gLeafArea, max_node) * plant->getPlants() * smm2sm;

   if (! reals_are_equal (leaf_area_tot, gLAI + gSLAI, tolerance_lai))
     {
     ostringstream msg;
     msg << "Total leaf area doesn't match LAI. LAI = ";
     msg <<  leaf_area_tot << ". Lai total = " <<  (gLAI + gSLAI) << ends;
     throw std::runtime_error (msg.str());
     }

//    leaf_area_tot = 0.0;
//    for (int node = 0; node < max_node; node++)
//      {
//      leaf_area_tot +=
//                  divide (gLeafNoDead[node], gLeafNo[node], 0.0)
//                     * gLeafArea[node]
//                     * plant->getPlants() * smm2sm;
//      }
//
//    if (! reals_are_equal (leaf_area_tot, gLAI + gSLAI + gTLAI_dead, tolerance_lai))
//      {
//      ostrstream msg;
//      msg << "total leaf area doesn't match TPLA. LAI = ";
//      msg <<  leaf_area_tot << ". Lai total = " <<  (gLAI + gSLAI + gTLAI_dead) << ends;
//      throw std::runtime_error (msg.str());
//      }

   if (sum_real_array(gLeafNoSen, max_node) >
       sum_real_array(gLeafNo, max_node))
       {
       throw std::runtime_error ("Too much senesced leaf number - exceeds live leaves");
       }
}

// Calculate deltas from potential and stresses
void genericLeafPart::actual(void)
   {
   this->leaf_area_actual ();
   this->leaf_no_actual ();
   }

//Purpose
//   Simulate actual crop leaf area development - checks that leaf area
//   development matches D_m production via a maximum specific leaf area
//   for the daily increase in LAI. SLA_max changes as a function of LAI.
//
void genericLeafPart::leaf_area_actual(void)
{
   float sla_max = cSLAMax.value(gLAI);                    //calculated daily max spec leaf area

   float dltLAI_carbon = dlt.dm_green * sla_max * smm2sm;  //maximum daily increase in leaf area
                                                           //index from carbon supply
   dltLAI = min(dltLAI_carbon, dltLAI_stressed);
}

//Purpose
//   Simulate actual leaf number increase as limited by dry matter production.
void genericLeafPart::leaf_no_actual (void)
   {
   //ratio of actual to potential lai
   float lai_ratio = divide (dltLAI, dltLAI_stressed, 0.0);

   //ratio of actual to potential leaf appearance
   float leaf_no_frac= cLeafNoFrac.value(lai_ratio);

   dltLeafNo = dltLeafNoPot * leaf_no_frac;

   if (dltLeafNo < dltNodeNoPot)
      {
      dltNodeNo = dltLeafNo;
      }
   else
      {
      dltNodeNo = dltNodeNoPot;
      }
   }


//+  Purpose
//     Calculate the fractional death of oldest green leaf.
void genericLeafPart::leaf_death (float  g_nfact_expansion, float  g_dlt_tt)
   {
   float leaf_no_now;                            // total number of leaves yesterday
   float leaf_no_sen_now;                       // total number of dead leaves yesterday
   float leaf_death_rate;                        // thermal time for senescence of another leaf (oCd)
   float leaf_per_node;                          // no. of leaves senescing per node
   float tpla_now;                               //
   float max_sleaf_no_now;                       // max number of senesced leaves allowable
   float max_sen_area;                           // max area that can be senesced
   float node_sen_rate;


   leaf_no_now = sum_real_array (gLeafNo, max_node);

   leaf_per_node = leaf_no_now * cFrLeafSenRate;

   node_sen_rate = divide( cNodeSenRate
                          , 1.0 + cNFactLeafSenRate * (1.0 - g_nfact_expansion)
                          , 0.0);

   leaf_death_rate = divide (node_sen_rate, leaf_per_node, 0.0);

   if (plant->inPhase("harvest_ripe"))
       {
       // Constrain leaf death to remaining leaves
       //cnh do we really want to do this?;  XXXX
       leaf_no_sen_now = sum_real_array (gLeafNoSen,max_node);
       dltLeafNoSen = l_bound (leaf_no_now - leaf_no_sen_now, 0.0);
       }
   else if (plant->getStageNumber() > cSenStartStage
       /*XXXX should be phenology->inPhase("leaf_senescence") !!!!!*/)
       {
       dltLeafNoSen = divide (g_dlt_tt, leaf_death_rate, 0.0);

       // Ensure minimum leaf area remains
       tpla_now = sum_real_array (gLeafArea, max_node);
       max_sen_area = l_bound (tpla_now - cMinTPLA, 0.0);
       max_sleaf_no_now = legnew_leaf_no_from_area (gLeafArea
                                                    , gLeafNo
                                                    , max_node
                                                    , max_sen_area);

       // Constrain leaf death to remaining leaves
       leaf_no_sen_now = sum_real_array (gLeafNoSen, max_node);
       dltLeafNoSen = u_bound (dltLeafNoSen, max_sleaf_no_now - leaf_no_sen_now);
       }
   else
       {
       dltLeafNoSen = 0.0;
       }
   }

// Public interface to calculate potentials
void genericLeafPart::potential (int leaf_no_pot_option /* (INPUT) option number*/
                              , float stressFactor    /* (INPUT) stress factor */
                              , float dlt_tt)         /* (INPUT) Thermal Time */
   {
   this->leaf_no_pot (leaf_no_pot_option, stressFactor, dlt_tt);
   this->leaf_area_potential ();
   }

//+  Purpose
//     Calculate leaf number development
void genericLeafPart::leaf_no_pot (int option, float stressFactor, float dlt_tt)
    {
    if (option == 1)
        {
        cproc_leaf_no_pot1(cNodeAppRate
                           , cLeavesPerNode
                           , plant->inPhase("node_formation")
                           , plant->on_day_of("emergence")
                           , gNodeNo
                           , dlt_tt
                           , &dltLeafNoPot
                           , &dltNodeNoPot);
        }
    else if (option == 2)
        {
        //wheat
        float tiller_no_now =  gNodeNo;
        cproc_leaf_no_pot3  (cNodeAppRate
                             , cLeavesPerNode
                             , plant->inPhase("tiller_formation")
                             , plant->on_day_of("emergence")
                             , tiller_no_now
                             , dlt_tt
                             , stressFactor
                             , &gLeavesPerNode
                             , &dltLeafNoPot
                             , &dltNodeNoPot);
        }
    else
        {
        throw std::invalid_argument ("invalid template option in leaf_no_pot");
        }
    }

//+  Purpose
//  Calculate the potential increase in leaf area development (mm^2)
//  on an individual leaf basis, with account taken of the area of
//  currently expanding leaves (node_no_correction).
void genericLeafPart::leaf_area_potential ()
   {
   float node_no_now = gNodeNo + cNodeNoCorrection;

   float leaf_size = cLeafSize.value (node_no_now);

   dltLAI_pot =  dltLeafNoPot * leaf_size * smm2sm * plant->getPlants();
   }


//+  Purpose
//   Calculate the biomass non-limiting leaf area development from the
//   potential daily increase in lai and stress factors (water &
//   nitrogen)
void genericLeafPart::leaf_area_stressed (float stressFactor)
    {
    dltLAI_stressed = dltLAI_pot * stressFactor;
    }

void genericLeafPart::detachment (void)
   {
        cproc_lai_detachment1 (c.sen_detach_frac
                               , gSLAI
                               , &dltSLAI_detached
                               , c.dead_detach_frac
                               , gTLAI_dead
                               , &dltTLAI_dead_detached);


        plant_leaf_detachment (gLeafArea
                               , dltSLAI_detached
                               , plant->getPlants()
                               , max_node);

   }

//   Calculate todays leaf area senescence
void genericLeafPart::leaf_area_sen(float swdef_photo , float mint)
{
    float plants = plant->getPlants();

    dltSLAI_age = legopt_leaf_area_sen_age1( gLeafNo
                              , gLeafNoSen
                              , dltLeafNoSen
                              , max_node
                              , gLAI
                              , gSLAI
                              , cMinTPLA
                              , gLeafArea
                              , plants);

    dltSLAI_light = crop_leaf_area_sen_light1 (cLAISenLight, cSenLightSlope, gLAI, plants, cMinTPLA);


    dltSLAI_water = crop_leaf_area_sen_water1 (cSenRateWater,
                               gLAI,
                               swdef_photo,
                               plants,
                               cMinTPLA);

    dltSLAI_frost = crop_leaf_area_sen_frost1(cSenescenceFac,
                              gLAI,
                              mint,
                              plants,
                              cMinTPLA);

    dltSLAI = max(max(max(dltSLAI_age, dltSLAI_light), dltSLAI_water), dltSLAI_frost);
}

// Update state variables
void genericLeafPart::update(void)
{
    plantPart::update();
    // need to account for truncation of partially developed leaf (add 1)
    float node_no = 1.0 + gNodeNo;

    float dlt_leaf_area = divide (dltLAI, plant->getPlants(), 0.0) * sm2smm;
    accumulate (dlt_leaf_area, gLeafArea, node_no-1.0, dltNodeNo);

    // Area senescence is calculated apart from plant number death
    // so any decrease in plant number will mean an increase in average
    // plant size as far as the leaf size record is concerned.
    if ((plant->getPlants() /*+ g_dlt_plants*/)<=0.0)   //XXXX FIXME!!
        {
        fill_real_array(gLeafArea, 0.0, max_node);
        }

    accumulate (dltLeafNo, gLeafNo, node_no-1.0, dltNodeNo);

    float leaf_no_sen_tot = sum_real_array(gLeafNoSen, max_node) + dltLeafNoSen;

    for (int node = 0; node < max_node; node++)
        {
        if (leaf_no_sen_tot > gLeafNo[node])
            {
            leaf_no_sen_tot -=  gLeafNo[node];
            gLeafNoSen[node] = gLeafNo[node];
            }
        else
            {
            gLeafNoSen[node] = leaf_no_sen_tot;
            leaf_no_sen_tot = 0.0;
            }
        }
    gNodeNo += dltNodeNo;

    // transfer plant leaf area
    gLAI +=  dltLAI - dltSLAI;
    gSLAI += dltSLAI - dltSLAI_detached;

    // Transfer dead leaf areas
    float dying_fract_plants = plant->getDyingFractionPlants();
    float dlt_lai_dead  = gLAI  * dying_fract_plants;
    float dlt_slai_dead = gSLAI * dying_fract_plants;
    gLAI -=  dlt_lai_dead;
    gSLAI -=  dlt_slai_dead;
    gTLAI_dead +=  dlt_lai_dead + dlt_slai_dead - dltTLAI_dead_detached;
}

// Remove detachment from leaf area record
void genericLeafPart::remove_detachment (float dlt_slai_detached, float dlt_lai_removed )
    {
    // Remove detachment from leaf area record from bottom upwards
    float area_detached = dlt_slai_detached / plant->getPlants() * sm2smm;  // (mm2/plant)

    for (int node = 0; node < max_node; node++)
      {
      if(area_detached > gLeafArea[node])
        {
        area_detached = area_detached - gLeafArea[node];
        gLeafArea[node] = 0.0;
        }
      else
        {
        gLeafArea[node] = gLeafArea[node] - area_detached;
        break;
        }
      }

    // Remove detachment from leaf area record from top downwards
    float area_removed = dlt_lai_removed / plant->getPlants() * sm2smm;  // (mm2/plant)

    for (int node = (int)gNodeNo; node >= 0 ; node--)
    {
      if(area_removed > gLeafArea[node])
      {
        area_removed = area_removed - gLeafArea[node];
        gLeafArea[node] = 0.0;
      }
      else
      {
        gLeafArea[node] = gLeafArea[node] - area_removed;
        break;
      }
   }

   // calc new node number
   for (int node = max_node - 1; node >= 0; node--)
      {
      if (!reals_are_equal(gLeafArea[node], 0.0, 1.0E-4))    // Slop?
         {
         gNodeNo = (float)node;  //FIXME - need adjustment for leafs remaining in for this node
         break;
         }
      }

   // calc new leaf number
   int newNodeNo = (int) (1.0 + gNodeNo);
   for (int node = newNodeNo - 1; node < max_node; node++)
      {
      gLeafNo[node] = 0.0;
      gLeafNoSen[node] = 0.0;
      }
}

void genericLeafPart::removeBiomass(void)
// (Re)-Initialise plant leaf area from deltas
    {
    float chop_fr_green = divide(dlt.dm_green_removed, DMGreen, 0.0);
    float chop_fr_sen   = divide(dlt.dm_senesced_removed, DMSenesced, 0.0);
    float chop_fr_dead  = divide(dlt.dm_dead_removed, DMDead, 0.0);

    float dlt_lai = gLAI * chop_fr_green;
    float dlt_slai = gSLAI * chop_fr_sen;
    float dlt_tlai_dead = gTLAI_dead * chop_fr_dead;

    // keep leaf area above a minimum
    float lai_init = cInitialTPLA * smm2sm * plant->getPlants();
    float dlt_lai_max = gLAI - lai_init;
    dlt_lai = u_bound (dlt_lai, dlt_lai_max);

    gLAI -= dlt_lai;
    gSLAI -= dlt_slai;
    gTLAI_dead -= dlt_tlai_dead;
    remove_detachment (dlt_slai, dlt_lai);

     plantPart::removeBiomass();

   // keep dm above a minimum
    float dm_init = c.dm_init * plant->getPlants();
    DMGreen = l_bound (DMGreen, dm_init);

    float n_init = dm_init * c.n_init_conc;
    NGreen = l_bound (NGreen, n_init);
}

float genericLeafPart::coverTotal(void)
//=======================================================================================
{
   return 1.0 - (1.0 - coverLeaf.green) * (1.0 - coverLeaf.sen) * (1.0 - coverLeaf.dead);
}

float genericLeafPart::coverGreen(void)
//=======================================================================================
{
   return coverLeaf.green;
}

float genericLeafPart::coverDead(void)
//=======================================================================================
{
   return coverLeaf.dead;
}

float genericLeafPart::coverSen(void)
//=======================================================================================
{
   return coverLeaf.sen;
}

float genericLeafPart::doCover (float canopy_fac, float g_row_spacing)
   //===========================================================================
{

   //+  Purpose
   //     Calculate leaf cover

   //+  Changes
   //     19 Jan 2006 JNGH - Programmed and Specified

   //- Implementation Section ----------------------------------

    legnew_cover(g_row_spacing
                 , cXRowSpacing
                 , cYExtinctCoef
                 , cNumRowSpacing
                 , canopy_fac
                 , getLAI()
                 , &coverLeaf.green);


    legnew_cover (g_row_spacing
                 , cXRowSpacing
                 , cYExtinctCoefDead
                 , cNumRowSpacing
                 , canopy_fac
                 , getSLAI()
                 , &coverLeaf.sen);

    legnew_cover (g_row_spacing
                 , cXRowSpacing
                 , cYExtinctCoefDead
                 , cNumRowSpacing
                 , canopy_fac
                 , getTLAI_dead()
                 , &coverLeaf.dead);

   return coverLeaf.green;
}

float genericLeafPart::interceptRadiationGreen (float radiation)    // incident radiation on leafs
    //===========================================================================
{
   //     Calculate leaf total radiation interception and return transmitted radiation

   radiationInterceptedGreen = coverGreen() * radiation;
   return radiationInterceptedGreen;
}

float genericLeafPart::interceptRadiationTotal (float radiation)    // incident radiation on leafs
    //===========================================================================
{
   //     Calculate leaf total radiation interception and return transmitted radiation

   radiationInterceptedTotal = coverTotal() * radiation;
   return radiationInterceptedTotal;
}

void genericLeafPart::doDmPotRUE (void )                    // (OUTPUT) potential dry matter (carbohydrate) production (g/m^2)
   //===========================================================================
{
   //       Potential biomass (carbohydrate) production from
   //       photosynthesis (g/m^2).  The effect of factors such
   //       temperature and nutritional status of the plant are
   //       taken into account in the radiation use efficiency.

   double stress_factor = min(min(min(plant->getTempStressPhoto(), plant->getNfactPhoto())
                                  , plant->getOxdefPhoto()), plant->getPfactPhoto());

   dlt.dm_pot_rue = (radiationInterceptedGreen * cRue.value(plant->getStageNumber())) * stress_factor * plant->getCo2ModifierRue();
}

void genericLeafPart::doTECO2()          // (OUTPUT) transpiration coefficient
   //==========================================================================
{
   cproc_transp_eff_co2_1(plant->getVpd()
                          , c.transpEffCf[(int)plant->getStageNumber()-1]
                          , plant->getCo2ModifierTe()
                          , &transpEff);
}

float genericLeafPart::SWDemand(void)         //(OUTPUT) crop water demand (mm)
   //===========================================================================
   /*  Purpose
   *       Return crop water demand from soil by the crop (mm) calculated by
   *       dividing biomass production limited by radiation by transpiration efficiency.
   */
{
   // get potential transpiration from potential
   // carbohydrate production and transpiration efficiency
   float sw_demand = 0.0;
   cproc_sw_demand1 (dlt.dm_pot_rue
                     , transpEff
                     , &sw_demand);
   return sw_demand;
}

void genericLeafPart::doDmPotTE (void)  //(OUTPUT) potential dry matter production by transpiration (g/m^2)
   //===========================================================================
   //   Calculate the potential biomass production based upon today's water supply.

{
   // potential (supply) by transpiration

   dlt.dm_pot_rue = plant->getWaterSupplyLeaf() * transpEff;

   // Capping of sw demand will create an effective TE- recalculate it here       //FIXME
   // In an ideal world this should NOT be changed here - NIH
   //       g.transp_eff = g.transp_eff * divide(g.sw_demand_te,g.sw_demand, 1.0);
   //       g.swDemandTEFruit = g.swDemandTEFruit * divide(g.sw_demand,g.sw_demand_te, 1.0);          // Hack to correct TE for fruit
}

