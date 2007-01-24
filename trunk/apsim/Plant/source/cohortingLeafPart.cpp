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
#include <ApsimShared/ApsimComponentData.h>
#include <ApsimShared/FStringExt.h>
#include <general/string_functions.h>

#include "PlantInterface.h"
#include "PlantLibrary.h"
#include "Plant.h"
#include "PlantPart.h"
#include "LeafPart.h"
#include "cohortingLeafPart.h"
#include "iostream.h"
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


void cohortingLeafPart::doRegistrations(protocol::Component *system)
//=======================================================================================
// Connect our bits to the system
   {
   plantPart::doRegistrations(system);
   setupGetFunction(system, "node_no", protocol::DTsingle, false,
                    &cohortingLeafPart::get_node_no, "/plant", "Number of main stem nodes");

   setupGetFunction(system, "node_no_sen", protocol::DTsingle, false,
                    &cohortingLeafPart::get_node_no_sen, "/plant", "Number of main stem nodes senesced");

   setupGetFunction(system, "node_no_fx", protocol::DTsingle, false,
                    &cohortingLeafPart::get_node_no_fx, "/plant", "Number of main stem nodes senesced");

   setupGetFunction(system, "leaf_no", protocol::DTsingle, true,
                    &cohortingLeafPart::get_leaf_no, "mm^2/plant", "Number of leaves in each cohort");

   setupGetFunction(system, "leaf_area", protocol::DTsingle, true,
                    &cohortingLeafPart::get_leaf_area, "mm^2/plant", "Leaf area for each leaf cohort");

   setupGetFunction(system, "leaf_area_max", protocol::DTsingle, true,
                    &cohortingLeafPart::get_leaf_area_max, "mm^2/plant", "Maximum Leaf area for each leaf cohort");

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

   system->addGettableVar("dlt_lai", dltLAI, "m^2/m^2", "Actual change in live plant lai");

   system->addGettableVar("dlt_lai_pot", dltLAI_pot, "m^2/m^2", "Potential change in live plant lai");

   system->addGettableVar("dlt_lai_stressed", dltLAI_stressed, "m^2/m^2", "Potential change in lai allowing for stress");

   system->addGettableVar("dlt_lai_carbon", dltLAI_carbon, "m^2/m^2", "Potential change in lai allowing for growth");

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
      system->sendVariable(qd, (float)((int)gNodeNo));
   else
      system->sendVariable(qd, (float)(0.0));
}

void cohortingLeafPart::get_node_no_sen(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
{
   float node_no_sen = 0.0;

   if (gNodeNo == 0)
      node_no_sen = 0.0;

   else
      for (unsigned int cohort = 1; cohort != gLeafArea.size(); cohort++)
         {
         if (reals_are_equal(gLeafArea[cohort-1], 0.0, 1.0E-4)&&gLeafArea[cohort]>0.0)
            {
            // This is the senescing node
            node_no_sen = cohort-1+1+divide(gLeafAreaSen[cohort],gLeafAreaMax[cohort],0.0);
            break;
            }
         }

   system->sendVariable(qd, node_no_sen);
}

void cohortingLeafPart::get_node_no_fx(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
{
   float node_no_fx = gNodeNo;

   if (gNodeNo == 0)
      node_no_fx = 0.0;

   else
      for (unsigned int cohort = 0; cohort != gLeafArea.size()-1; cohort++)
         {
         if (gLeafAge[cohort]>cGrowthPeriod[cohort+1] && gLeafAge[cohort+1]<=cGrowthPeriod[cohort+1+1])
            {
            // This is the expanded node
            node_no_fx = cohort+1; //+divide(gLeafAge[cohort+1],cGrowthPeriod[cohort+1+1],0.0);
            break;
            }
         }

   system->sendVariable(qd, node_no_fx);
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

void cohortingLeafPart::get_leaf_area_max(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
{
   system->sendVariable(qd, gLeafAreaMax);
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
   dltLAI_carbon = 0.0;
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
   gLeafAreaMax.clear();
   gLeafAreaSen.clear();
   dltSLA_age.clear();
   gDltLeafAreaPot.clear();
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
   gDltLeafAreaPot.clear();
   gLeafAreaMax.clear();
   gLeafAreaSen.clear();
   gLeafAge.clear();
   gLeafNo.clear();
   dltSLA_age.clear();

   float tpla = cInitialTPLA;

   // Fill cohorts until no more area available
   for (unsigned int cohort = 0; tpla > 0.0; cohort++)
      {
      gLeafArea.push_back(min(tpla, cAreaPot[cohort+1]));
      gLeafAreaMax.push_back(min(tpla, cAreaPot[cohort+1]));
      gLeafAreaSen.push_back(0.0);
      gLeafAge.push_back(0.0);
      gDltLeafAreaPot.push_back(0.0);
      gLeafNo.push_back(cLeafNumberAtEmerg);
      dltSLA_age.push_back(0.0);

      if (tpla > cAreaPot[cohort+1])
         gNodeNo += 1.0;
      else
         gNodeNo += divide(tpla, cAreaPot[cohort+1], 0.0);

      tpla -= cAreaPot[cohort+1];
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

   dltLAI_carbon = dlt.dm_green * sla_max * smm2sm;  //maximum daily increase in leaf area
                                                           //index from carbon supply
   //cout <<  dltLAI_carbon, dltLAI_stressed;
//    char  msg[200];                               // message
//       sprintf (msg, "%10.6f%10.6f%10.6f",dltLAI_carbon, dltLAI_stressed,dltLAI_pot);
//    plant->writeString (msg);

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


void cohortingLeafPart::leaf_death (float  /* g_nfact_expansion*/, float  /* g_dlt_tt*/)
//=======================================================================================
//     Calculate the fractional death of oldest green leaf.
   {
//XXX Fix me
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
       float node_app_rate = cNodeAppRate[(int)gNodeNo+1];
       dltNodeNo = divide (dlt_tt, node_app_rate, 0.0);
       }
    else
       dltNodeNo = 0.0;

    dltLeafNoPot = 0.0;
    if (tillering)
        {
        float leaves_per_node_now = cLeavesPerNode[(int)gNodeNo+1];
        gLeavesPerNode = min(gLeavesPerNode, leaves_per_node_now);
        float dlt_leaves_per_node = cLeavesPerNode[(int)(gNodeNo + dltNodeNo)+1] - leaves_per_node_now;

        gLeavesPerNode +=  dlt_leaves_per_node * stressFactor;

        dltLeafNoPot = dltNodeNo * gLeavesPerNode;
        }
    }

void cohortingLeafPart::leaf_area_potential (float tt)
//=======================================================================================
//  Calculate the potential increase in leaf area development (mm^2)
   {
   for (unsigned int cohort = 0; cohort != gLeafArea.size(); cohort++)
      {
      if (cGrowthPeriod[cohort+1] - gLeafAge[cohort] > 0.0)
         gDltLeafAreaPot[cohort] = cAreaPot[cohort+1] * u_bound(divide(tt, cGrowthPeriod[cohort+1], 0.0), 1.0);
      else
         gDltLeafAreaPot[cohort] = 0.0;
      }

   dltLAI_pot =  sum(gDltLeafAreaPot) * smm2sm * plant->getPlants();
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
      if(area_detached > gLeafAreaSen[cohort])
        {
        area_detached -= gLeafAreaSen[cohort];
        gLeafAreaSen[cohort] = 0.0;
        }
      else
        {
        gLeafAreaSen[cohort] -= area_detached;
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
       if (gLeafAge[cohort] > (cGrowthPeriod[cohort+1] + cLagPeriod[cohort+1]) &&
           gLeafAge[cohort] < (cGrowthPeriod[cohort+1] + cLagPeriod[cohort+1] + cSenescingPeriod[cohort+1]))
          {
//          float qq = (gLeafAreaMax[cohort]*dltTT)/cSenescingPeriod[cohort];
//          if (qq > gLeafAreaMax[cohort])
//             dltSLA_age[cohort] = gLeafAreaMax[cohort];
//          else
//             dltSLA_age[cohort] = qq;
          float tt_remaining = max(0.0,cGrowthPeriod[cohort+1] + cLagPeriod[cohort+1] + cSenescingPeriod[cohort+1] - gLeafAge[cohort]);
          float senfr = min(1.0,divide(dltTT,tt_remaining,0.0));
          dltSLA_age[cohort] = gLeafArea[cohort]*senfr;
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
      gDltLeafAreaPot.push_back(0.0);
      gLeafAreaMax.push_back(0.0);
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
       for (cohort = 0; cohort != gLeafArea.size(); cohort++)
          {
          float fract = divide(gDltLeafAreaPot[cohort], sum(gDltLeafAreaPot), 0.0);
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
          gLeafArea[cohort] = l_bound(gLeafArea[cohort] - area, 0.0);

          area = dltLeafAreaSen * divide(gLeafAreaSen[cohort], areaTotSen, 0.0);
          gLeafAreaSen[cohort] = l_bound(gLeafAreaSen[cohort] - area, 0.0);
          }
       dltLAI_dead = dltLeafArea * plant->getPlants() * smm2sm;
       dltSLAI_dead = dltLeafAreaSen * plant->getPlants() * smm2sm;
       }

       // Transfer dead leaf areas
       gTLAI_dead +=  dltLAI_dead + dltSLAI_dead - dltTLAI_dead_detached;

       // Keep track of maximum size of each cohort
       for (cohort = 0; cohort != gLeafArea.size(); cohort++)
          {
          if(gLeafArea[cohort] > gLeafAreaMax[cohort])
             {
             gLeafAreaMax[cohort] = gLeafArea[cohort];
             }
          }
}

// Remove detachment from leaf area record
void cohortingLeafPart::remove_detachment (float /* dlt_slai_detached*/, float /* dlt_lai_removed*/ )
//=======================================================================================
   {

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

float cohortingLeafPart::coverTotal(void)
//=======================================================================================
{
   return 1.0 - (1.0 - coverLeaf.green) * (1.0 - coverLeaf.sen) * (1.0 - coverLeaf.dead);
}

float cohortingLeafPart::coverGreen(void)
//=======================================================================================
{
   return coverLeaf.green;
}

float cohortingLeafPart::coverDead(void)
//=======================================================================================
{
   return coverLeaf.dead;
}

float cohortingLeafPart::coverSen(void)
//=======================================================================================
{
   return coverLeaf.sen;
}

void cohortingLeafPart::doCover (PlantSpatial &spatial)
   //===========================================================================
{

   //+  Purpose
   //     Calculate leaf cover

   //+  Changes
   //     19 Jan 2006 JNGH - Programmed and Specified

   //- Implementation Section ----------------------------------

    legnew_cover(spatial.rowSpacing()
                 , cXRowSpacing
                 , cYExtinctCoef
                 , cNumRowSpacing
                 , spatial.canopyFac()
                 , getLAI()
                 , &coverLeaf.green);


    legnew_cover (spatial.rowSpacing()
                 , cXRowSpacing
                 , cYExtinctCoefDead
                 , cNumRowSpacing
                 , spatial.canopyFac()
                 , getSLAI()
                 , &coverLeaf.sen);

    legnew_cover (spatial.rowSpacing()
                 , cXRowSpacing
                 , cYExtinctCoefDead
                 , cNumRowSpacing
                 , spatial.canopyFac()
                 , getTLAI_dead()
                 , &coverLeaf.dead);
}

float cohortingLeafPart::interceptRadiationGreen (float radiation)    // incident radiation on leafs
    //===========================================================================
{
   //     Calculate leaf total radiation interception and return transmitted radiation

   radiationInterceptedGreen = coverGreen() * radiation;
   return radiationInterceptedGreen;
}

float cohortingLeafPart::interceptRadiationTotal (float radiation)    // incident radiation on leafs
    //===========================================================================
{
   //     Calculate leaf total radiation interception and return transmitted radiation

   radiationInterceptedTotal = coverTotal() * radiation;
   return radiationInterceptedTotal;
}

void cohortingLeafPart::doDmPotRUE (void )                    // (OUTPUT) potential dry matter (carbohydrate) production (g/m^2)
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

void cohortingLeafPart::doTECO2()          // (OUTPUT) transpiration coefficient
   //==========================================================================
{
   cproc_transp_eff_co2_1(plant->getVpd()
                          , c.transpEffCf[(int)plant->getStageNumber()-1]
                          , plant->getCo2ModifierTe()
                          , &transpEff);
}

void cohortingLeafPart::doSWDemand(float SWDemandMaxFactor)         //(OUTPUT) crop water demand (mm)
   //===========================================================================
   /*  Purpose
   *       Return crop water demand from soil by the crop (mm) calculated by
   *       dividing biomass production limited by radiation by transpiration efficiency.
   */
{
   // get potential transpiration from potential
   // carbohydrate production and transpiration efficiency

   cproc_sw_demand1 (dlt.dm_pot_rue
                     , transpEff
                     , &sw_demand_te);

       // Capping of sw demand will create an effective TE- recalculate it here
       // In an ideal world this should NOT be changed here - NIH

   float SWDemandMax = SWDemandMaxFactor * coverGreen() ;
   sw_demand = u_bound(sw_demand_te, SWDemandMax);
   transpEff = transpEff * divide(sw_demand_te, sw_demand, 1.0);
}

void cohortingLeafPart::doDmPotTE (float swSupply)  //(OUTPUT) potential dry matter production by transpiration (g/m^2)
   //===========================================================================
   //   Calculate the potential biomass production based upon today's water supply.

{
   // potential (supply) by transpiration

   dlt.dm_pot_te = swSupply * transpEff;
}

void cohortingLeafPart::doBioActual (void)
   //===========================================================================
{
   //       Takes the minimum of biomass production limited by radiation and
   //       biomass production limited by water.

   // use whichever is limiting
   dlt.dm = min (dlt.dm_pot_rue, dlt.dm_pot_te);
}

