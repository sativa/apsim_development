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
#include "LeafPart.h"
#include "cohortingLeafPart.h"
using namespace std;


void cohortingLeafPart::readConstants (protocol::Component *system, const string &section)
//=======================================================================================
// Read Constants
   {
   plantPart::readConstants(system, section);
   // Nothing to do here..
   }

void cohortingLeafPart::readSpeciesParameters (protocol::Component *system, vector<string> &search_order)
//=======================================================================================
// Read species specific parameters
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


   cGrowthPeriod.search(system, search_order,
                        "x_leaf_cohort", "(cohort)", 0, 50.0,
                        "y_leaf_growth_period", "(oC)", 0.0, 2000.0);

   cLagPeriod.search(system, search_order,
                        "x_leaf_cohort", "(cohort)", 0, 50.0,
                        "y_leaf_lag_period", "(oC)", 0.0, 2000.0);

   cSenescingPeriod.search(system, search_order,
                        "x_leaf_cohort", "(cohort)", 0, 50.0,
                        "y_leaf_sen_period", "(oC)", 0.0, 2000.0);


   cAreaPot.search(system, search_order,
                        "x_leaf_cohort", "(cohort)", 0, 50.0,
                        "y_leaf_area_pot", "(mm^2)", 0.0, 2000000.0);

   }


void cohortingLeafPart::doRegistrations(protocol::Component *system)
//=======================================================================================
// Connect our bits to the system
   {
   plantPart::doRegistrations(system);
   setupGetFunction(system, "node_no", protocol::DTsingle, false,
                    &cohortingLeafPart::get_node_no, "/plant", "Number of main stem nodes");

   setupGetFunction(system, "leaf_no", protocol::DTsingle, true,
                    &cohortingLeafPart::get_leaf_no, "mm^2/plant", "Number of leaves in each cohort");

   setupGetFunction(system, "leaf_area", protocol::DTsingle, true,
                    &cohortingLeafPart::get_leaf_area, "mm^2/plant", "Leaf area for each leaf cohort");

   setupGetFunction(system, "leaf_area_tot", protocol::DTsingle, false,
                    &cohortingLeafPart::get_leaf_area_tot, "mm^2/plant", "Total plant leaf area");

   setupGetFunction(system, "leaf_age", protocol::DTsingle, true,
                    &cohortingLeafPart::get_leaf_age, "oCd", "Age of each leaf cohort");

   setupGetFunction(system, "lai_sum", protocol::DTsingle, false,
                    &cohortingLeafPart::get_lai_sum, "m^2/m^2", "LAI of all leaf components");

   setupGetFunction(system, "tlai", protocol::DTsingle, false,
                    &cohortingLeafPart::get_tlai, "m^2/m^2", "Total lai");

   setupGetFunction(system, "slai", protocol::DTsingle, false,
                    &cohortingLeafPart::get_sen_leaf_area_index, "m^2/m^2", "Senesced leaf area index");

   setupGetFunction(system, "lai", protocol::DTsingle, false,
                    &cohortingLeafPart::get_leaf_area_index, "m^2/m^2", "Leaf area index");

   system->addGettableVar("dlt_lai_pot", dltLAI_pot, "m^2/m^2", "Potential change in live plant lai");

   system->addGettableVar("dlt_lai_stressed", dltLAI_stressed, "m^2/m^2", "Potential change in lai allowing for stress");

   system->addGettableVar("dlt_leaf_no", dltLeafNo, "leaves/m2", "Change in number of leaves");

   system->addGettableVar("dlt_leaf_no_pot", dltLeafNoPot, "m^2/m^2", "Potential Leaf no");

   system->addGettableVar("dlt_tiller_no", dltNodeNo, "tillers/m2", "Change in number of tillers");

   system->addGettableVar("dlt_node_no", dltNodeNo, "/m2", "Change in number of main stem nodes");

   system->addGettableVar("tlai_dead", gTLAI_dead, "m^2/m^2", "tlai dead");

   setupGetFunction(system, "dlt_slai", protocol::DTsingle, false,
                    &cohortingLeafPart::get_dlt_slai, "m^2/m^2", "Change in lai");

   setupGetFunction(system, "dlt_slai_age", protocol::DTsingle, false,
                    &cohortingLeafPart::get_dlt_slai_age, "m^2/m^2", "Change in lai via age");

   system->addGettableVar("dlt_slai_light", dltSLAI_light, "m^2/m^2", "Change in lai via light");

   system->addGettableVar("dlt_slai_water", dltSLAI_water, "m^2/m^2", "Change in lai via water stress");

   system->addGettableVar("dlt_slai_frost", dltSLAI_frost, "m^2/m^2", "Change in lai via low temperature");
   }

void cohortingLeafPart::get_tlai(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
{
    float tlai = getLAI() + getSLAI();
    system->sendVariable(qd, tlai);
}

void cohortingLeafPart::get_lai_sum(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
{
    float lai_sum = getLAI() + getSLAI() + gTLAI_dead;
    system->sendVariable(qd, lai_sum);
}

void cohortingLeafPart::get_leaf_no(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
{
   system->sendVariable(qd, gLeafNo);
}

void cohortingLeafPart::get_node_no(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
{
   if (gNodeNo > 0)
      system->sendVariable(qd, (float)(gNodeNo));
   else
      system->sendVariable(qd, (float)(0.0));
}

void cohortingLeafPart::get_dlt_slai_age(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
{
   system->sendVariable(qd, sum(dltSLA_age) * plant->getPlants() * smm2sm);
}

float cohortingLeafPart::getLeafNo(void) const
//=======================================================================================
{
   return (sum(gLeafNo));
}

void cohortingLeafPart::get_leaf_area_tot(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
{
   system->sendVariable(qd, sum(gLeafArea));
}

void cohortingLeafPart::get_leaf_area(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
{
   system->sendVariable(qd, gLeafArea);
}

void cohortingLeafPart::get_leaf_age(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
{
   system->sendVariable(qd, gLeafAge);
}

void cohortingLeafPart::get_leaf_area_index(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
{
   system->sendVariable(qd, getLAI());
}

void cohortingLeafPart::get_sen_leaf_area_index(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
{
   system->sendVariable(qd, getSLAI());
}

void cohortingLeafPart::get_dlt_slai(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
{
   float dltSLAI = max(max(max(sum(dltSLA_age) * plant->getPlants() * smm2sm,
                               dltSLAI_light),
                               dltSLAI_water),
                               dltSLAI_frost);
   system->sendVariable(qd, dltSLAI);
}

void cohortingLeafPart::zeroDeltas(void)
//=======================================================================================
// Clean out yesterday's rate calculations
{
   plantPart::zeroDeltas();
   dltLAI = 0.0;
   dltLAI_pot = 0.0;
   dltLAI_stressed = 0.0;
   dltTLAI_dead = 0.0;
   dltTLAI_dead_detached = 0.0;
   dltSLAI_detached = 0.0;
   dltSLAI_light = 0.0;
   dltSLAI_water = 0.0;
   dltSLAI_frost = 0.0;
   dltLeafNo              = 0.0;
//    g.dlt_node_no              = 0.0; JNGH - need to carry this through for site no next day.
   dltNodeNo = 0.0;
   setTo(dltSLA_age, (float) 0.0);
}

void cohortingLeafPart::zeroAllGlobals(void)
//=======================================================================================
// Initialise all constants & parameters
{
   plantPart::zeroAllGlobals();
   cLeafNumberAtEmerg = 0.0;
   cSLAMin = 0.0;
   cInitialTPLA = 0.0;
   cLAISenLight = 0.0;
   cSenLightSlope = 0.0;
   //cGrowthPeriod.clear();
   //cAreaPot.clear();

   gTLAI_dead = 0.0;
   gLeafAge.clear();
   gLeafArea.clear();
   gLeafAreaSen.clear();
   dltSLA_age.clear();
   gLeafNo.clear();
   gLeavesPerNode = 0.0;

   gNodeNo = 0.0;
   dltNodeNo = 0.0;
}

void cohortingLeafPart::onEmergence(void)
//=======================================================================================
// Leaf, Node number and area initialisation
   {
   plantPart::onEmergence();
   initialiseAreas();
   }

void cohortingLeafPart::onKillStem(void)
//=======================================================================================
// transfer plant leaf area to dead pool
   {
   plantPart::onKillStem();
   float deadLAI = gTLAI_dead + getLAI();
   onEmergence();
   gTLAI_dead = deadLAI;
   }

void cohortingLeafPart::initialiseAreas(void)
//=======================================================================================
// Initialise leaf areas to a newly emerged state.
   {
   gNodeNo = 0.0;

   gLeafArea.clear();
   gLeafAreaSen.clear();
   gLeafAge.clear();
   gLeafNo.clear();
   dltSLA_age.clear();

   float tpla = cInitialTPLA;

   // Fill cohorts until no more area available
   for (unsigned int cohort = 0; tpla > 0.0; cohort++)
      {
      gLeafArea.push_back(min(tpla, cAreaPot[cohort]));
      gLeafAreaSen.push_back(0.0);
      gLeafAge.push_back(0.0);
      gLeafNo.push_back(cLeafNumberAtEmerg);
      dltSLA_age.push_back(0.0);

      if (tpla > cAreaPot[cohort])
         gNodeNo += 1.0;
      else
         gNodeNo += divide(tpla, cAreaPot[cohort], 0.0);

      tpla -= cAreaPot[cohort];
      }
   gTLAI_dead = 0.0;
   }

void cohortingLeafPart::onHarvest(float /* cutting_height */, float remove_fr,
                              vector<string> &dm_type,
                              vector<float> &dlt_crop_dm,
                              vector<float> &dlt_dm_n,
                              vector<float> &dlt_dm_p,
                              vector<float> &fraction_to_residue)
//=======================================================================================
// Harvest event
{
   onHarvest_GenericAboveGroundPart(remove_fr, dm_type, dlt_crop_dm, dlt_dm_n, dlt_dm_p, fraction_to_residue);
   initialiseAreas();
}

void cohortingLeafPart::checkBounds(void)
//=======================================================================================
// Sanity checks
{
   plantPart::checkBounds();
   if (gTLAI_dead < 0.0) throw std::runtime_error(c.name + " gTLAI_dead is negative! (" + ftoa(gTLAI_dead,".6") + ")");
   if (gNodeNo < 0) throw std::runtime_error(c.name + " node number is negative! (" + ftoa(gNodeNo,".6") + ")");

   for (unsigned int cohort = 0; cohort != gLeafArea.size(); cohort++)
      {
      if (gLeafArea[cohort] < 0.0)
         throw std::runtime_error(c.name + " LA cohort is negative! (" + ftoa(gLeafArea[cohort],".6") + ")");
      if (gLeafAreaSen[cohort] < 0.0)
         throw std::runtime_error(c.name + " LA Senesced cohort is negative! (" + ftoa(gLeafAreaSen[cohort],".6") + ")");
      if (gLeafNo[cohort] < 0)
         throw std::runtime_error(c.name + " leaf number is negative! (" + ftoa(gLeafNo[cohort],".6") + ")");
      }
}

void cohortingLeafPart::actual(void)
//=======================================================================================
// Plant is telling us to calculate deltas from potential and stresses
   {
   this->leaf_area_actual ();
   this->leaf_no_actual ();
   }

void cohortingLeafPart::leaf_area_actual(void)
//=======================================================================================
//   Simulate actual crop leaf area development - checks that leaf area
//   development matches D_m production via a maximum specific leaf area
//   for the daily increase in LAI. SLA_max changes as a function of LAI.
{
   float sla_max = cSLAMax[getLAI()];                      //calculated daily max spec leaf area

   float dltLAI_carbon = dlt.dm_green * sla_max * smm2sm;  //maximum daily increase in leaf area
                                                           //index from carbon supply
   dltLAI = min(dltLAI_carbon, dltLAI_stressed);
}

void cohortingLeafPart::leaf_no_actual (void)
//=======================================================================================
//   Simulate actual leaf & tiller number increase as limited by dry matter production.
   {
   //ratio of actual to potential lai
   float lai_ratio = divide (dltLAI, dltLAI_stressed, 0.0);

   //ratio of actual to potential leaf appearance
   float leaf_no_frac= cLeafNoFrac[lai_ratio];

   dltLeafNo = dltLeafNoPot * leaf_no_frac;
   }


void cohortingLeafPart::leaf_death (float  g_nfact_expansion, float  g_dlt_tt)
//=======================================================================================
//     Calculate the fractional death of oldest green leaf.
   {
#if 0
// XXX broken
   float leaf_no_now;                            // total number of leaves yesterday
   float leaf_no_sen_now;                       // total number of dead leaves yesterday
   float leaf_death_rate;                        // thermal time for senescence of another leaf (oCd)
   float leaf_per_node;                          // no. of leaves senescing per node
   float tpla_now;                               //
   float max_sleaf_no_now;                       // max number of senesced leaves allowable
   float max_sen_area;                           // max area that can be senesced
   float node_sen_rate;

   leaf_no_now = gLeafNo;

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
       tpla_now = sum(gLeafArea);
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
#endif
   }

void cohortingLeafPart::potential (int leaf_no_pot_option, /* (INPUT) option number*/
                                   float stressFactor,     /* (INPUT) stress factor */
                                   float dlt_tt)           /* (INPUT) Thermal Time */
//=======================================================================================
// Plant is telling us to calculate potentials
   {
   dltTT = dlt_tt; //Yuck..  XXXXXXXXXXX
   if (leaf_no_pot_option != 2) throw std::invalid_argument("cohorting not implemented for indeterminates");

   this->leaf_no_pot (stressFactor, dlt_tt);
   this->leaf_area_potential (dlt_tt);
   }

void cohortingLeafPart::leaf_no_pot (float stressFactor, float dlt_tt)
//=======================================================================================
//     Calculate leaf number development
    {
    bool tillering = plant->inPhase("tiller_formation");
    if (tillering)
       {
       float node_app_rate = cNodeAppRate[gNodeNo];
       dltNodeNo = divide (dlt_tt, node_app_rate, 0.0);
       }
    else
       dltNodeNo = 0.0;

    dltLeafNoPot = 0.0;
    if (tillering)
        {
        float leaves_per_node_now = cLeavesPerNode[gNodeNo];
        gLeavesPerNode = min(gLeavesPerNode, leaves_per_node_now);
        float dlt_leaves_per_node = cLeavesPerNode[gNodeNo + dltNodeNo] - leaves_per_node_now;

        gLeavesPerNode +=  dlt_leaves_per_node * stressFactor;

        dltLeafNoPot = dltNodeNo * gLeavesPerNode;
        }
    }

void cohortingLeafPart::leaf_area_potential (float tt)
//=======================================================================================
//  Calculate the potential increase in leaf area development (mm^2)
   {
   float areaPot = 0.0;
   for (unsigned int cohort = 0; cohort != gLeafArea.size(); cohort++)
      {
      if (cGrowthPeriod[cohort] - gLeafAge[cohort] > 0.0)
         areaPot += cAreaPot[cohort] * u_bound(divide(tt, cGrowthPeriod[cohort], 0.0), 1.0);
      }
   dltLAI_pot =  areaPot * smm2sm * plant->getPlants();
   }

void cohortingLeafPart::leaf_area_stressed (float stressFactor)
//=======================================================================================
//   Calculate the biomass non-limiting leaf area development from the
//   potential daily increase in lai and stress factors (water & nitrogen)
   {
   dltLAI_stressed = dltLAI_pot * stressFactor;
   }

void cohortingLeafPart::detachment (void)
//=======================================================================================
   {
   cproc_lai_detachment1 (c.sen_detach_frac
                               , getSLAI()
                               , &dltSLAI_detached
                               , c.dead_detach_frac
                               , gTLAI_dead
                               , &dltTLAI_dead_detached);

   float area_detached = divide(dltSLAI_detached,  plant->getPlants(), 0.0) * sm2smm;

   for (unsigned int cohort = 0; cohort != gLeafArea.size(); cohort++) 
      {
      if(area_detached > gLeafArea[cohort])
        {
        area_detached -= gLeafArea[cohort];
        gLeafArea[cohort] = 0.0;
        }
      else
        {
        gLeafArea[cohort] -= area_detached;
        break;
        }
      }

  }

void cohortingLeafPart::leaf_area_sen(float swdef_photo , float mint)
//=======================================================================================
//   Calculate todays leaf area senescence
{
    float plants = plant->getPlants();

    // Age senescence for each cohort
    for (unsigned int cohort = 0; cohort != gLeafArea.size(); cohort++)
       if (gLeafAge[cohort] > (cGrowthPeriod[cohort] + cLagPeriod[cohort]) &&
           gLeafAge[cohort] < (cGrowthPeriod[cohort] + cLagPeriod[cohort] + cSenescingPeriod[cohort]))
          {
          float qq = (cAreaPot[cohort]*dltTT)/cSenescingPeriod[cohort];
          if (qq > cAreaPot[cohort])
             dltSLA_age[cohort] = cAreaPot[cohort];
          else
             dltSLA_age[cohort] = qq;
          }

    dltSLAI_light = crop_leaf_area_sen_light1 (cLAISenLight, cSenLightSlope, getLAI(), plants, cMinTPLA);


    dltSLAI_water = crop_leaf_area_sen_water1 (cSenRateWater,
                               getLAI(),
                               swdef_photo,
                               plants,
                               cMinTPLA);

    dltSLAI_frost = crop_leaf_area_sen_frost1(cSenescenceFac,
                              getLAI(),
                              mint,
                              plants,
                              cMinTPLA);
}

// Update state variables
void cohortingLeafPart::update(void)
//=======================================================================================
{
   unsigned int cohort;
   plantPart::update();

   if (((int)gNodeNo) != ((int)(gNodeNo + dltNodeNo))) {
      // Initiate a new cohort
      gLeafArea.push_back(0.0);
      gLeafAreaSen.push_back(0.0);
      gLeafAge.push_back(0.0);
      gLeafNo.push_back(0.0);
      dltSLA_age.push_back(0.0);
   }
   if (gLeafArea.size() > 0)
     gLeafNo[gLeafArea.size()-1] += dltLeafNo;  // Add leaves to currently expanding cohort.
   gNodeNo += dltNodeNo;

   for (unsigned int cohort = 0; cohort != gLeafArea.size(); cohort++)
      gLeafAge[cohort] += dltTT;

   float dltLeafArea = divide (dltLAI, plant->getPlants(), 0.0) * sm2smm;

    // Partition new LAI to cohorts
    if (dltLeafArea > 0.0)
       {
       float areaPot = 0.0;
       for (cohort = 0; cohort != gLeafArea.size(); cohort++)
          {
          if (gLeafAge[cohort] < cGrowthPeriod[cohort])
             areaPot += cAreaPot[cohort] * u_bound(divide(dltTT, cGrowthPeriod[cohort], 0.0), 1.0);
          }
       for (cohort = 0; cohort != gLeafArea.size(); cohort++)
          {
          float fract = 0.0;
          if (gLeafAge[cohort] < cGrowthPeriod[cohort])
             {
             float dA = cAreaPot[cohort] * u_bound(divide(dltTT, cGrowthPeriod[cohort], 0.0), 1.0);
             fract = divide(dA, areaPot, 0.0);
             }
          gLeafArea[cohort] += fract * dltLeafArea;
          }
       }

    // Transfer SLAI processes within cohorts
    // XX We need to re-think all of these
    if (sum(dltSLA_age) > 0.0)
       {
       // Age senescence per cohort
       for (cohort = 0; cohort != gLeafArea.size(); cohort++)
          {
          gLeafArea[cohort] = l_bound(gLeafArea[cohort] - dltSLA_age[cohort], 0.0);
          gLeafAreaSen[cohort] += dltSLA_age[cohort];
          }
       }
    if (dltSLAI_light > 0.0)
       {
       // bottom up (shading)
       float dltLeafArea = divide (dltSLAI_light, plant->getPlants(), 0.0) * sm2smm;
       for (cohort = 0; cohort != gLeafArea.size() && dltLeafArea > 0.0; cohort++)
          {
          float dlt = (dltLeafArea > gLeafArea[cohort]) ? gLeafArea[cohort] : dltLeafArea;
          gLeafArea[cohort] -= dlt;
          gLeafAreaSen[cohort] += dlt;
          dltLeafArea -= dlt;
          }
       }
    if (dltSLAI_water > 0.0)
       {
       // bottom up
       float dltLeafArea = divide (dltSLAI_water, plant->getPlants(), 0.0) * sm2smm;
       for (cohort = 0; cohort != gLeafArea.size(); cohort++)
          {
          float dlt = (dltLeafArea > gLeafArea[cohort]) ? gLeafArea[cohort] : dltLeafArea;
          gLeafArea[cohort] -= dlt;
          gLeafAreaSen[cohort] += dlt;
          dltLeafArea -= dlt;
          }
       }
    if (dltSLAI_frost > 0.0)
       {
       // top down
       float dltLeafArea = divide (dltSLAI_frost, plant->getPlants(), 0.0) * sm2smm;
       for (int cohort = (int)gLeafArea.size()-1; cohort >= 0 && dltLeafArea > 0.0; cohort--)
          {
          float dlt = (dltLeafArea > gLeafArea[cohort]) ? gLeafArea[cohort] : dltLeafArea;
          gLeafArea[cohort] -= dlt;
          gLeafAreaSen[cohort] += dlt;
          dltLeafArea -= dlt;
          }
       }

    // Plant death
    float dying_fract_plants = plant->getDyingFractionPlants();
    float dltLAI_dead = 0.0, dltSLAI_dead = 0.0;

    //XX I'm not sure any of this is needed???????
    if (dying_fract_plants > 0.0)
       {
       // uniform effect
       float dltLeafArea = sum(gLeafArea) * dying_fract_plants;
       float areaTot = sum(gLeafArea);
       float dltLeafAreaSen = sum(gLeafAreaSen) * dying_fract_plants;
       float areaTotSen = sum(gLeafAreaSen);
       for (cohort = 0; cohort != gLeafArea.size(); cohort++)
          {
          float area = dltLeafArea * divide(gLeafArea[cohort], areaTot, 0.0);
          gLeafArea[cohort] = u_bound(gLeafArea[cohort] - area, 0.0);

          area = dltLeafAreaSen * divide(gLeafAreaSen[cohort], areaTotSen, 0.0);
          gLeafAreaSen[cohort] = u_bound(gLeafAreaSen[cohort] - area, 0.0);
          }
       dltLAI_dead = dltLeafArea * plant->getPlants() * smm2sm;
       dltSLAI_dead = dltLeafAreaSen * plant->getPlants() * smm2sm;
       }

       // Transfer dead leaf areas
       gTLAI_dead +=  dltLAI_dead + dltSLAI_dead - dltTLAI_dead_detached;
}

// Remove detachment from leaf area record
void cohortingLeafPart::remove_detachment (float dlt_slai_detached, float dlt_lai_removed )
//=======================================================================================
    {
#if 0
// XXX broken
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
#endif
}

void cohortingLeafPart::remove_biomass_update(void)
//=======================================================================================
// Initialise plant leaf area from deltas
    {
#if 0
// XXX broken
    float chop_fr_green = divide(dlt.dm_green, DMGreen, 0.0);
    float chop_fr_sen   = divide(dlt.dm_senesced, DMSenesced, 0.0);
    float chop_fr_dead  = divide(dlt.dm_dead, DMDead, 0.0);

    float dlt_lai = getLAI() * chop_fr_green;
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

    // keep dm above a minimum
    float dm_init = c.dm_init * plant->getPlants();
    DMGreen = l_bound (DMGreen, dm_init);

    float n_init = dm_init * c.n_init_conc;
    NGreen = l_bound (NGreen, n_init);
#endif
}

float cohortingLeafPart::senFract (void) const
//=======================================================================================
   {
   float dltSLAI = max(max(max(sum(dltSLA_age) * plant->getPlants() * smm2sm,
                               dltSLAI_light),
                               dltSLAI_water),
                               dltSLAI_frost);

   return(divide (dltSLAI, getLAI() + dltLAI, 0.0));             // fraction of canopy senescing
   }
