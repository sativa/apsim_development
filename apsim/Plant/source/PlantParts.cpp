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
#include "PlantFruit.h"
using namespace std;


//////////---------------------------
// Hacks to set up and delete part arrays
// The "hacks" will not have usable parameter/constants, but most state variables will be OK.
void Plant::setupHacks(vector<plantPart *> &parts)
   {
   plantPart *x = new plantPartHack(this, root, "root");
   parts.push_back(x);

   parts.push_back(leafPart);

   parts.push_back(stemPart);

   x=new plantPartHack(this, pod,  "pod");
   parts.push_back(x);

   x=new plantPartHack(this, meal, "meal");
   parts.push_back(x);

   x=new plantPartHack(this, oil,  "oil");
   parts.push_back(x);
//   parts.push_back(fruitPart);
   }
void Plant::deleteHacks(vector<plantPart *> &parts)
   {
   vector<plantPart *>::iterator part;
   for (part = parts.begin(); part != parts.end(); part++)
      if ((*part)->c.name != "stem" && (*part)->c.name != "leaf" && (*part)->c.name != "fruit")
         delete *part;
   }


void Plant::setupHacks1(vector<plantPart *> &parts)
   {
   plantPart *x = new plantPartHack(this, root, "root");

   parts.push_back(x);
   parts.push_back(leafPart);
   parts.push_back(stemPart);
   parts.push_back(fruitPart);

   }
//////////---------------------------
void plantPart::doRegistrations(protocol::Component *system)
{
   string varName1, varName2, varName3, varName4, varName5, varName6;
   string varName7, varName8, varName9;
   string desc1, desc2, desc3, desc4, desc5, desc6, desc7, desc8, desc9;


   varName1 = "dm_green_" + c.name;
   desc1 = "Weight of " + c.name;
   system->addGettableVar(varName1.c_str(), g.dm_green, "g/m^2", desc1.c_str());

   varName2 = "n_green_" + c.name;
   desc2 = "N in " + c.name;
   system->addGettableVar(varName2.c_str(),  g.n_green, "g/m^2", desc2.c_str());

   varName3 = "p_green_" + c.name;
   desc3 = "P in " + c.name;
   system->addGettableVar(varName3.c_str(),  g.p_green, "g/m^2", desc3.c_str());

   varName1 = c.name + "_wt";
   desc1 = "Weight of " + c.name;
   system->addGettableVar(varName1.c_str(), g.dm_green, "g/m^2", desc1.c_str());

   varName2 = c.name + "_n";
   desc2 = "N in " + c.name;
   system->addGettableVar(varName2.c_str(),  g.n_green, "g/m^2", desc2.c_str());

   varName3 = c.name + "_p";
   desc3 = "P in " + c.name;
   system->addGettableVar(varName3.c_str(),  g.p_green, "g/m^2", desc3.c_str());

   varName1 = "dm_dead_" + c.name;
   desc1 = "Weight of dead " + c.name;
   system->addGettableVar(varName1.c_str(), g.dm_dead, "g/m^2", desc1.c_str());

   varName2 = "n_dead_" + c.name;
   desc2 = "N in dead " + c.name;
   system->addGettableVar(varName2.c_str(),  g.n_dead, "g/m^2", desc2.c_str());

   varName3 = "p_dead_" + c.name;
   desc3 = "P in dead " + c.name;
   system->addGettableVar(varName3.c_str(),  g.p_dead, "g/m^2", desc3.c_str());

   varName1 = "dead" + c.name + "_wt";
   desc1 = "Weight of dead " + c.name;
   system->addGettableVar(varName1.c_str(), g.dm_dead, "g/m^2", desc1.c_str());

   varName2 = "dead" + c.name + "_n";
   desc2 = "N in dead " + c.name;
   system->addGettableVar(varName2.c_str(),  g.n_dead, "g/m^2", desc2.c_str());

   varName3 = "dead" + c.name + "_p";
   desc3 = "P in dead " + c.name;
   system->addGettableVar(varName3.c_str(),  g.p_dead, "g/m^2", desc3.c_str());

   varName1 = "dm_senesced_" + c.name;
   desc1 = "Weight of senesced " + c.name;
   system->addGettableVar(varName1.c_str(), g.dm_senesced, "g/m^2", desc1.c_str());

   varName2 = "n_senesced_" + c.name;
   desc2 = "N in senesced " + c.name;
   system->addGettableVar(varName2.c_str(),  g.n_senesced, "g/m^2", desc2.c_str());

   varName3 = "p_senesced_" + c.name;
   desc3 = "P in senesced " + c.name;
   system->addGettableVar(varName3.c_str(),  g.p_sen, "g/m^2", desc3.c_str());

   varName1 = "dlt_dm_green_" + c.name;
   desc1 = "Delta Weight of " + c.name;
   system->addGettableVar(varName1.c_str(), dlt.dm_green, "g/m^2", desc1.c_str());

   varName2 = "dlt_n_green_" + c.name;
   desc2 = "Delta N in " + c.name;
   system->addGettableVar(varName2.c_str(),  dlt.n_green, "g/m^2", desc2.c_str());

   varName3 = "dlt_p_green_" + c.name;
   desc3 = "Delta P in " + c.name;
   system->addGettableVar(varName3.c_str(),  dlt.p_green, "g/m^2", desc3.c_str());

   varName1 = "dlt_dm_dead_" + c.name;
   desc1 = "Delta Weight of dead " + c.name;
   system->addGettableVar(varName1.c_str(), dlt.dm_dead, "g/m^2", desc1.c_str());

   varName2 = "dlt_n_dead_" + c.name;
   desc2 = "Delta N in dead " + c.name;
   system->addGettableVar(varName2.c_str(),  dlt.n_dead, "g/m^2", desc2.c_str());

   varName3 = "dlt_p_dead_" + c.name;
   desc3 = "Delta P in dead " + c.name;
   system->addGettableVar(varName3.c_str(),  dlt.p_dead, "g/m^2", desc3.c_str());

   varName1 = "dlt_dm_senesced_" + c.name;
   desc1 = "Delta Weight of senesced " + c.name;
   system->addGettableVar(varName1.c_str(), dlt.dm_senesced, "g/m^2", desc1.c_str());

   varName2 = "dlt_n_senesced_" + c.name;
   desc2 = "Delta N in senesced " + c.name;
   system->addGettableVar(varName2.c_str(),  dlt.n_senesced, "g/m^2", desc2.c_str());

   varName3 = "dlt_p_senesced_" + c.name;
   desc3 = "Delta P in senesced " + c.name;
   system->addGettableVar(varName3.c_str(),  dlt.p_sen, "g/m^2", desc3.c_str());

   varName1 = "dlt_dm_detached_" + c.name;
   desc1 = "Delta Weight of detached " + c.name;
   system->addGettableVar(varName1.c_str(), dlt.dm_detached, "g/m^2", desc1.c_str());

   varName2 = "dlt_n_detached_" + c.name;
   desc2 = "Delta N in detached " + c.name;
   system->addGettableVar(varName2.c_str(),  dlt.n_detached, "g/m^2", desc2.c_str());

   varName3 = "dlt_p_detached_" + c.name;
   desc3 = "Delta P in detached " + c.name;
   system->addGettableVar(varName3.c_str(),  dlt.p_det, "g/m^2", desc3.c_str());

   varName4 =  "n_conc_" + c.name;
   desc4 = "N concentration in " + c.name;
   setupGetFunction(system, varName4.c_str(), protocol::DTsingle, false,
                    &plantPart::get_n_conc, "%", desc4.c_str());

   varName5 = "p_conc_" + c.name;
   desc5 = "P concentration in " + c.name;
   setupGetFunction(system, varName5.c_str(), protocol::DTsingle, false,
                    &plantPart::get_p_conc, "%", desc5.c_str());

   varName6 = "n_conc_crit_" + c.name;
   desc6 = "critical N content in " + c.name;
   setupGetFunction(system, varName6.c_str(), protocol::DTsingle, false,
                    &plantPart::get_n_conc_crit,
                    "%", desc6.c_str());

   varName7 = "n_conc_min_" + c.name;
   desc7 = "minimum N content in " + c.name;
   setupGetFunction(system, varName7.c_str(), protocol::DTsingle, false,
                    &plantPart::get_n_conc_min,
                    "%", desc7.c_str());

   varName8 = "n_demand_" + c.name;
   desc8 = "N demand of " + c.name;
   system->addGettableVar(varName8.c_str(),
               v.n_demand, "g/m^2", desc8.c_str());

   varName9 = "dlt_n_retrans_" + c.name;
   desc9 = "N retranslocated to/from " + c.name;
   system->addGettableVar(varName9.c_str(),
               dlt.n_retrans, "g/m^2", desc9.c_str());

   varName9 = "dlt_n_senesced_retrans_" + c.name;
   desc9 = "N retranslocated to/from senesced " + c.name;
   system->addGettableVar(varName9.c_str(),
               dlt.n_senesced_retrans, "g/m^2", desc9.c_str());

   varName9 = "dlt_n_senesced_trans_" + c.name;
   desc9 = "N translocated to/from senesced " + c.name;
   system->addGettableVar(varName9.c_str(),
               dlt.n_senesced_trans, "g/m^2", desc9.c_str());

   varName9 = "dlt_dm_retrans_" + c.name;
   desc9 = "DM retranslocated to/from " + c.name;
   system->addGettableVar(varName9.c_str(),
               dlt.dm_green_retrans, "g/m^2", desc9.c_str());
}

void plantPart::get_n_conc(protocol::Component *system, protocol::QueryValueData &qd)
{
    float n_conc = divide (g.n_green, g.dm_green, 0.0) * 100.0;
    system->sendVariable(qd, n_conc);
}
void plantPart::get_n_conc_crit(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, (float) (g.n_conc_crit * 100.0));
}
void plantPart::get_n_conc_min(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, (float) (g.n_conc_min * 100.0));
}
void plantPart::get_p_conc(protocol::Component *system, protocol::QueryValueData &qd)
{
    float p_conc = divide (g.p_green, g.dm_green, 0.0) * 100.0;
    system->sendVariable(qd, p_conc);
}

void plantPart::zeroAllGlobals(void)
{
   g.dm_dead=0.0;
   g.dm_green=0.0;
   g.dm_senesced=0.0;
   g.n_dead=0.0;
   g.n_green=0.0;
   g.n_senesced=0.0;
   g.height=0.0;
   g.width=0.0;
   g.n_conc_crit=0.0;
   g.n_conc_max=0.0;
   g.n_conc_min=0.0;
   g.p_conc_sen=0.0;
   g.p_conc_max=0.0;
   g.p_conc_min=0.0;
   g.dm_plant_min=0.0;

   g.p_green=0.0;
   g.p_sen=0.0;
   g.p_dead=0.0;

   zeroDeltas();
}

void plantPart::zeroDeltas(void)
{
   dlt.dm_green = 0.0;
   dlt.dm_senesced = 0.0;
   dlt.dm_detached = 0.0;
   dlt.dm_dead = 0.0;
   dlt.dm_dead_detached = 0.0;
   dlt.dm_green_retrans = 0.0;
   dlt.n_green = 0.0;
   dlt.n_senesced = 0.0;
   dlt.n_senesced_retrans = 0.0;
   dlt.n_senesced_trans = 0.0;
   dlt.n_detached = 0.0;
   dlt.n_dead = 0.0;
   dlt.n_dead_detached = 0.0;
   dlt.n_retrans = 0.0;

   dlt.p_green = 0.0;
   dlt.p_sen = 0.0;
   dlt.p_det = 0.0;
   dlt.p_dead_det = 0.0;
   dlt.p_retrans = 0.0;
   dlt.p_dead = 0.0;

   dlt.height = 0.0;
   dlt.width = 0.0;

   v.dm_green_demand = 0.0;
   v.n_capacity = 0.0;
   v.n_demand = 0.0 ;
   v.soil_n_demand = 0.0;
   v.n_max = 0.0 ;
   v.p_demand = 0.0;
}
void plantPart::checkBounds(void)
{
   // Use a small comparison tolerance here.
   const float ctz = -0.00001;
   if (g.dm_green < ctz) throw std::runtime_error(c.name + " dm_green pool is negative! " + ftoa(g.dm_green,".6"));
   if (g.n_green < ctz) throw std::runtime_error(c.name + " n_green pool is negative!" + ftoa(g.n_green,".6"));
   if (g.p_green < ctz) throw std::runtime_error(c.name + " p_green pool is negative!" + ftoa(g.p_green,".6"));
   if (g.dm_dead < ctz) throw std::runtime_error(c.name + " dm_dead pool is negative!" + ftoa(g.dm_dead,".6"));
   if (g.n_dead < ctz) throw std::runtime_error(c.name + " n_dead pool is negative!" + ftoa(g.n_dead,".6"));
   if (g.p_dead < ctz) throw std::runtime_error(c.name + " p_dead pool is negative!" + ftoa(g.p_dead,".6"));
   if (g.dm_senesced < ctz) throw std::runtime_error(c.name + " dm_sen pool is negative!" + ftoa(g.dm_senesced,".6"));
   if (g.n_senesced < ctz) throw std::runtime_error(c.name + " n_sen pool is negative!" + ftoa(g.n_senesced,".6"));
   if (g.p_sen < ctz) throw std::runtime_error(c.name + " p_sen pool is negative!" + ftoa(g.p_sen,".6"));
}

void plantPart::readConstants(protocol::Component *system, const string &section)
    {
    if (plant->phosphorusAware())
       {
#if 0
       c.p_conc_min.read(system, section
                        , "x_p_stage_code",  "()", 0.0, 100.0
                        , ("y_p_conc_min_" + c.name).c_str(), "(g/g)", 0.0, 1.0);

       c.p_conc_max.read(system, section
                        , "x_p_stage_code",  "()", 0.0, 100.0
                        , ("y_p_conc_max_" + c.name).c_str(), "(g/g)", 0.0, 1.0);

       c.p_conc_sen.read(system, section
                        , "x_p_stage_code",  "()", 0.0, 100.0
                        , ("y_p_conc_sen_" + c.name).c_str(), "(g/g)", 0.0, 1.0);

       system->readParameter (section
                               , c.name + "_p_conc_init"
                               //, "(g/g)"
                               , c.p_init_conc
                               , 0.0, 1.0);
#else
       system->readParameter (section, "x_p_stage_code", /*"()",*/ c.x_p_stage_code, c.num_x_p_stage_code, 0.0, 12.0);

       system->readParameter (section, ("y_p_conc_max_" + c.name).c_str(), /*"(g/g)",*/
                              c.y_p_conc_max, c.num_x_p_stage_code, 0.0, 1.0);
       system->readParameter (section, ("y_p_conc_sen_" + c.name).c_str(), /*"(g/g)", */
                              c.y_p_conc_sen, c.num_x_p_stage_code, 0.0, 1.0);
       system->readParameter (section, ("y_p_conc_min_" + c.name).c_str(), /*"(g/g)",*/
                              c.y_p_conc_min, c.num_x_p_stage_code, 0.0, 1.0);
       system->readParameter (section, c.name + "_p_conc_init", /*"(g/g)",*/
                              c.p_init_conc, 0.0, 1.0);
#endif

       vector<string> parts;
       Split_string(system->readParameter (section, "stress_determinants"), " ", parts);
       if (find(parts.begin(), parts.end(), c.name) != parts.end())
          c.p_stress_determinant = true;
       else
          c.p_stress_determinant = false;

       Split_string(system->readParameter (section, "yield_parts"), " ", parts);
       if (find(parts.begin(),parts.end(), c.name) != parts.end())
          c.p_yield_part = true;
       else
          c.p_yield_part = false;

       Split_string(system->readParameter (section, "retrans_parts"), " ", parts);
       if (find(parts.begin(),parts.end(), c.name) != parts.end())
          c.p_retrans_part = true;
       else
          c.p_retrans_part = false;
       }
    }
void plantPart::readSpeciesParameters(protocol::Component *system, vector<string> &sections)
    {
    system->readParameter (sections
                            , c.name + "_trans_frac"
                            //, "()"
                            , c.trans_frac
                            , 0.0, 1.0);

    system->readParameter (sections
                            , c.name + "_sen_detach_frac"
                            //, "()"
                            , c.sen_detach_frac
                            , 0.0, 1.0);

    system->readParameter (sections
                            , c.name + "_dead_detach_frac"
                            //, "()"
                            , c.dead_detach_frac
                            , 0.0, 1.0);

    system->readParameter (sections
                            , c.name + "_dm_init"
                            //,  "(g/plant)"
                            , c.dm_init
                            , 0.0, 1.0);

    system->readParameter (sections
                            , c.name + "_n_init_conc"
                            //,  "(g/g)"
                            , c.n_init_conc
                            , 0.0, 1.0);

    c.n_conc_crit.search(system, sections
                        , "x_stage_code" , "()", 1.0, 100.0
                        , ("y_n_conc_crit_" + c.name).c_str(), "()", 0.0, 100.0);

    c.n_conc_min.search(system, sections
                        , "x_stage_code" , "()", 1.0, 100.0
                        , ("y_n_conc_min_" + c.name).c_str(), "()", 0.0, 100.0);

    c.n_conc_max.search(system, sections
                        , "x_stage_code" , "()", 1.0, 100.0
                        , ("y_n_conc_max_" + c.name).c_str(), "()", 0.0, 100.0);

    c.dm_sen_frac.search(system, sections
                        , ("x_dm_sen_frac_" + c.name).c_str(), "()", 0.0, 100.0
                        , ("y_dm_sen_frac_" + c.name).c_str(), "()", 0.0, 1.0);

    system->readParameter (sections
                        , (c.name + "_n_sen_conc").c_str() //, "()"
                        , c.n_sen_conc
                        , 0.0, 1.0);

    c.fr_remain.search(system, sections
                     , "fr_height_cut",  "(0-1)", 0.0, 1.0
                     , ("fr_"+c.name+"_remain").c_str(), "(0-1)", 0.0, 1.0);

    if (system->readParameter (sections
                             , (c.name + "_n_retrans_fraction").c_str() //, "()"
                             , c.n_retrans_fraction
                             , 0.0, 1.0, true) == false)
        c.n_retrans_fraction = 1.0;

    if (system->readParameter (sections
                            , "n_deficit_uptake_fraction"//, "()"
                            , c.n_deficit_uptake_fraction
                            , 0.0, 1.0, true) == false)
        c.n_deficit_uptake_fraction = 0.0;

    }

void plantPart::readCultivarParameters (protocol::Component *system, const string &cultivar)
{
    c.height.read(system, cultivar
                , ("x_" + c.name + "_wt").c_str() , "(g/plant)", 0.0, 1000.0
                , "y_height", "(mm)", 0.0, 5000.0);
    c.width.read(system, cultivar
                , ("x_" + c.name + "_wt").c_str() , "(g/plant)", 0.0, 1000.0
                , "y_width", "(mm)", 0.0, 5000.0);
}

void plantPart::onEmergence()
{
   g.dm_green = c.dm_init * plant->getPlants();
   g.n_green = c.n_init_conc * g.dm_green;
   g.p_green = c.p_init_conc * g.dm_green;
}

void plantPart::onFlowering(void)
{
   float dm_plant = divide (g.dm_green, plant->getPlants(), 0.0);
   g.dm_plant_min = dm_plant;
}

// set the minimum weight of part; used for retranslocation to grain
void plantPart::onStartGrainFill(void)
{
   float dm_plant = divide (g.dm_green, plant->getPlants(), 0.0);
   g.dm_plant_min = dm_plant * (1.0 - c.trans_frac);
}

void plantPart::onKillStem(void)
{
       float dm_init = u_bound(plantPart::c.dm_init * plant->getPlants(), plantPart::g.dm_green);
       float n_init = u_bound(dm_init * plantPart::c.n_init_conc, plantPart::g.n_green);
       float p_init = u_bound(dm_init * plantPart::c.p_init_conc, plantPart::g.p_green);
//        float dm_init = c.dm_init * plant->getPlants();           //FIXME ????
//        float n_init = dm_init * c.n_init_conc;
//        float p_init = dm_init * c.p_init_conc;

        g.dm_dead += g.dm_green + g.dm_senesced - dm_init;
        g.dm_dead = l_bound (g.dm_dead, 0.0);
        g.dm_green = dm_init;
        g.dm_senesced = 0.0;

        g.n_dead += g.n_green + g.n_senesced - n_init;
        g.n_dead = l_bound (g.n_dead, 0.0);
        g.n_green = n_init;
        g.n_senesced = 0.0;

        g.p_dead += g.p_green + g.p_sen - p_init;
        g.p_dead = l_bound (g.p_dead, 0.0);
        g.p_green = p_init;
        g.p_sen = 0.0;

}

void plantPart::n_conc_limits(void)
{
   g.n_conc_crit = c.n_conc_crit.value(plant->getStageCode());
   g.n_conc_min = c.n_conc_min.value(plant->getStageCode());
   g.n_conc_max = c.n_conc_max.value(plant->getStageCode());
}

void plantPart::morphology(void)
{
   float dm_plant;               // dry matter of part (g/plant)
   dm_plant = divide (g.dm_green, plant->getPlants(), 0.0);

   if (c.height.isInitialised())
      {
      float new_height = c.height.value(dm_plant);       // new plant height (mm)
      dlt.height = l_bound(new_height - g.height, 0.0);
      }
   else
     {
     dlt.height = 0.0;
     }

   if (c.width.isInitialised())
     {
     float new_width = c.width.value(dm_plant);
     dlt.width = l_bound(new_width - g.width, 0.0);
     }
   else
     {
     dlt.width = 0.0;
     }
}

void plantPart::prepare(void)
{
   zeroDeltas();
}

void plantPart::update(void)
{
   g.height += dlt.height;
   g.width += dlt.width;
}

void plantPart::updateN(float dying_fract_plants)
{
// Update N
       dying_fract_plants = plant->getDyingFractionPlants();
    // transfer N
       g.n_dead -= dlt.n_dead_detached;
       g.n_green += dlt.n_green;
       g.n_green += dlt.n_retrans;
       g.n_green -= dlt.n_senesced;
       g.n_senesced += dlt.n_senesced;
       g.n_green += dlt.n_senesced_retrans;
       g.n_senesced -= dlt.n_detached;
       g.n_green = max(0.0, g.n_green);   // Can occur at total leaf senescence. FIXME! XXXX
       dlt.n_green_dead = g.n_green * dying_fract_plants;
       g.n_green -= dlt.n_green_dead;
       g.n_dead += dlt.n_green_dead;

       dlt.n_senesced_dead = g.n_senesced * dying_fract_plants;
       g.n_senesced -= dlt.n_senesced_dead;
       g.n_dead += dlt.n_senesced_dead;

}

void plantPart::updateDm(float dying_fract_plants)
{

       dying_fract_plants = plant->getDyingFractionPlants();
// Update DM
       g.dm_dead -= dlt.dm_dead_detached;

       g.dm_green += dlt.dm_green;
       g.dm_green += dlt.dm_green_retrans;
       g.dm_green -= dlt.dm_senesced;

       g.dm_senesced += dlt.dm_senesced;
       g.dm_senesced -= dlt.dm_detached;

       dlt.dm_green_dead = g.dm_green * dying_fract_plants;
       g.dm_green -=  dlt.dm_green_dead;
       g.dm_dead += dlt.dm_green_dead;

       dlt.dm_senesced_dead = g.dm_senesced * dying_fract_plants;
       g.dm_senesced -= dlt.dm_senesced_dead;
       g.dm_dead += dlt.dm_senesced_dead;

}
void plantPart::updateP(float dying_fract_plants)
{
       dying_fract_plants = plant->getDyingFractionPlants();

           g.p_green += dlt.p_green;
           g.p_green += dlt.p_retrans;
           g.p_green -= dlt.p_sen;
           g.p_green = max(0.0, g.p_green);  // Can occur at total leaf senescence. FIXME! XXXX

           float dlt_p_green_dead = g.p_green * dying_fract_plants;
           g.p_green -= dlt_p_green_dead;
           g.p_dead += dlt_p_green_dead;

           float dlt_p_senesced_dead = g.p_sen * dying_fract_plants;
           g.p_sen  -= dlt_p_senesced_dead;
           g.p_dead += dlt_p_senesced_dead;
}

/* Purpose
*     Return plant nitrogen demand for each plant component
*
*  Mission Statement
*     Calculate the Nitrogen demand and maximum uptake for each plant pool
*
*/
void plantPart::doNDemand1(float dlt_dm,             // (INPUT)  Whole plant the daily biomass production (g/m^2)
                          float dlt_dm_pot_rue)     // (INPUT)  Whole plant potential dry matter production (g/m^2)
{
    float part_fract = divide (dlt.dm_green, dlt_dm, 0.0);
    float dlt_dm_pot = dlt_dm_pot_rue * part_fract;         // potential dry weight increase (g/m^2)
    dlt_dm_pot = bound(dlt_dm_pot, 0.0, dlt_dm_pot_rue);

    if (g.dm_green > 0.0)
        {
        // get N demands due to difference between
        // actual N concentrations and critical N concentrations
        float N_crit       = g.dm_green * g.n_conc_crit;    // critical N amount (g/m^2)
        float N_potential  = g.dm_green * g.n_conc_max;     // maximum N uptake potential (g/m^2)

        // retranslocation is -ve for outflows
        float N_demand_old = N_crit -                       // demand for N by old biomass (g/m^2)
                 (g.n_green + dlt.n_retrans);
        float N_max_old    = N_potential -                  // N required by old biomass to reach
                 (g.n_green + dlt.n_retrans);               // N_conc_max  (g/m^2)


        // get potential N demand (critical N) of potential growth
        float N_demand_new = dlt_dm_pot * g.n_conc_crit;     // demand for N by new growth
                                                             // (g/m^2)
        float N_max_new    = dlt_dm_pot * g.n_conc_max;      // N required by new growth to reach
                                                             // N_conc_max  (g/m^2)
        v.n_demand = N_demand_old + N_demand_new;
        v.n_max    = N_max_old    + N_max_new ;

        v.n_demand = l_bound (v.n_demand, 0.0);
        v.n_max    = l_bound (v.n_max, 0.0);
        }
     else
        {
        v.n_demand = v.n_max = 0.0;
        }
}

//N demand as calculated by plant_n_demand
//+  Purpose
//       Return plant nitrogen demand for each plant component
//
//+  Mission Statement
//   Calculate the Nitrogen demand and maximum uptake for each plant pool
//
//+  Notes
//           Nitrogen required for grain growth has already been removed
//           from the stover.  Thus the total N demand is the sum of the
//           demands of the stover and roots.  Stover N demand consists of
//           two components:
//           Firstly, the demand for nitrogen by the potential new growth.
//           Secondly, the demand due to the difference between
//           the actual N concentration and the critical N concentration
//           of the tops (stover), which can be positive or negative
//
//           NOTE that this routine will not work if the root:shoot ratio
//           is broken. - NIH
//
//+  Changes
//     27-6-2003 nih taken from cproc_n_demand1
void plantPart::doNDemand2(float dlt_dm,             // (INPUT)  Whole plant the daily biomass production (g/m^2)
                          float dlt_dm_pot_rue)     // (INPUT)  Whole plant potential dry matter production (g/m^2)
{
    float part_fract = divide (dlt.dm_green, dlt_dm, 0.0);
    float dlt_dm_pot = dlt_dm_pot_rue * part_fract;         // potential dry weight increase (g/m^2)
    dlt_dm_pot = bound(dlt_dm_pot, 0.0, dlt_dm_pot_rue);

    if (g.dm_green > 0.0)
        {
        // get N demands due to difference between
        // actual N concentrations and critical N concentrations
        float N_crit       = g.dm_green * g.n_conc_crit;    // critical N amount (g/m^2)
        float N_potential  = g.dm_green * g.n_conc_max;     // maximum N uptake potential (g/m^2)

        // retranslocation is -ve for outflows
        float N_demand_old = N_crit - g.n_green;            // demand for N by old biomass (g/m^2)
        if (N_demand_old > 0.0)                             // Don't allow demand to satisfy all deficit
           N_demand_old *= c.n_deficit_uptake_fraction;

        float N_max_old    = N_potential - g.n_green;       // N required by old biomass to reach
                                                            // N_conc_max  (g/m^2)
       if (N_max_old>0.0)
           N_max_old *= c.n_deficit_uptake_fraction;        // Don't allow demand to satisfy all deficit


        // get potential N demand (critical N) of potential growth
        float N_demand_new = dlt_dm_pot * g.n_conc_crit;     // demand for N by new growth
                                                             // (g/m^2)
        float N_max_new    = dlt_dm_pot * g.n_conc_max;      // N required by new growth to reach
                                                             // N_conc_max  (g/m^2)
        v.n_demand = N_demand_old + N_demand_new;
        v.n_max    = N_max_old    + N_max_new ;

        v.n_demand = l_bound (v.n_demand, 0.0);
        v.n_max    = l_bound (v.n_max, 0.0);
        }
     else
        {
        v.n_demand = 0.0;
        v.n_max = 0.0;
        }
}

void plantPart::doPDemand(void)
{
      vector<plantPart *>::iterator part;
      float    deficit;
      float    p_conc_max;
      float    rel_growth_rate;

      v.p_demand = 0.0;
      rel_growth_rate = plant->getRelativeGrowthRate();

      if (c.p_yield_part)
         {
         // A yield part - does not contribute to soil demand
         v.p_demand = 0.0;
         }
      else
         {
         // Not a yield part - therefore it contributes to demand
         p_conc_max = linear_interp_real (plant->getStageCode()
                                        , c.x_p_stage_code
                                        , c.y_p_conc_max
                                        , c.num_x_p_stage_code);

         // scale up to include potential new growth
         // assuming partitioning today similar to current
         // plant form - a rough approximation
         deficit = p_conc_max * g.dm_green * (1.0 + rel_growth_rate) - g.p_green;

         v.p_demand = l_bound(deficit, 0.0);
         }
}
void plantPart::doSoilNDemand(void)
{
   v.soil_n_demand = v.n_demand - dlt.n_senesced_retrans;
   v.soil_n_demand = l_bound(v.soil_n_demand,0.0);
}

void plantPart::doSenescence1(float sen_fr)
{
   float fraction_senescing = c.dm_sen_frac.value(sen_fr);

   fraction_senescing = bound (fraction_senescing, 0.0, 1.0);
   dlt.dm_senesced = (g.dm_green + dlt.dm_green + dlt.dm_green_retrans)
                   * fraction_senescing;
}

void plantPart::doSenescence2(float sen_fr)
{
   float fraction_senescing = c.dm_sen_frac.value(sen_fr);

   fraction_senescing = bound (fraction_senescing, 0.0, 1.0);
   dlt.dm_senesced = g.dm_green * fraction_senescing;
}

void plantPart::doDmMin(void)
{   // do nothing - set on events
}

void plantPart::doNSenescence(void)
{
   float green_n_conc = divide (g.n_green, g.dm_green, 0.0);

   float dlt_n_in_senescing_part = dlt.dm_senesced * green_n_conc;

   float sen_n_conc = min (c.n_sen_conc, green_n_conc);

   dlt.n_senesced = dlt.dm_senesced * sen_n_conc;
   dlt.n_senesced = u_bound (dlt.n_senesced, g.n_green);

   dlt.n_senesced_trans = dlt_n_in_senescing_part - dlt.n_senesced;
   dlt.n_senesced_trans = l_bound(dlt.n_senesced_trans, 0.0);
}

void plantPart::doNSenescedRetrans(float navail, float n_demand_tot)
{
      dlt.n_senesced_retrans = navail * divide (v.n_demand, n_demand_tot, 0.0);
}


void plantPart::dm_detachment1(void)
   {
   dlt.dm_detached = g.dm_senesced * c.sen_detach_frac;
   dlt.dm_dead_detached = g.dm_dead * c.dead_detach_frac;
   }

void plantPart::n_detachment1(void)
   {
   dlt.n_detached = g.n_senesced * c.sen_detach_frac;
   dlt.n_dead_detached = g.n_dead * c.dead_detach_frac;
   }

void plantPart::doPSenescence(void)
{
   float green_p_conc = divide (g.p_green, g.dm_green, 0.0);

   float sen_p_conc = linear_interp_real (plant->getStageCode()
                                        , c.x_p_stage_code
                                        , c.y_p_conc_sen
                                        , c.num_x_p_stage_code);

   dlt.p_sen = min(green_p_conc, sen_p_conc) * dlt.dm_senesced;
   dlt.p_sen = u_bound (dlt.p_sen, g.p_green);
}

void plantPart::p_detachment1(void)
   {
//   dlt.p_det = g.p_senesced * c.sen_detach_frac;      //FIXME should be able to use these later
//   dlt.p_dead_det = g.p_dead * c.dead_detach_frac;

      float sen_detach_frac = divide(dlt.dm_detached
                                    , g.dm_senesced
                                    , 0.0);

      dlt.p_det = g.p_sen * sen_detach_frac;

      float dead_detach_frac = divide(dlt.dm_dead_detached
                                    , g.dm_dead
                                    , 0.0);

      dlt.p_dead_det = g.p_dead * dead_detach_frac;
   }

void plantPart::updatePDet(void)
   {
            g.p_sen +=  dlt.p_sen;
            g.p_sen -=  dlt.p_det;
            g.p_dead -= dlt.p_dead_det;
   }
/*  Purpose
*   The concentration of Nitrogen in plant parts is used to derive a Nitrogen stress index
*   for many processes. This stress index is calculated from today's relative nutitional
*   status between a critical and minimum Nitrogen concentration.
*
*  Mission Statement
*   Calculate Nitrogen stress factor from a bunch of parts
*/
float critNFactor(vector<const plantPart *> &parts, float multiplier)
{
   vector<const plantPart *>::iterator part;

   float dm = 0.0, N = 0.0;
   for (part = parts.begin(); part != parts.end(); part++)
      {
      dm += (*part)->g.dm_green;
      N += (*part)->g.n_green;
      }

   if (dm > 0.0)
      {
      float N_conc = divide (N, dm, 0.0);

      // calculate critical N concentrations
      float N_crit = 0.0;
      for (part = parts.begin(); part != parts.end(); part++)
        N_crit += (*part)->g.n_conc_crit * (*part)->g.dm_green;

      float N_conc_crit = divide (N_crit, dm, 0.0);

      // calculate minimum N concentrations
      float N_min = 0.0;
      for (part = parts.begin(); part != parts.end(); part++)
        N_min += (*part)->g.n_conc_min * (*part)->g.dm_green;

      float N_conc_min = divide (N_min, dm, 0.0);

      //calculate shortfall in N concentrations
      float dividend =  N_conc - N_conc_min;
      float divisor =   N_conc_crit - N_conc_min;
      float result = multiplier * divide (dividend, divisor, 1.0);
      result = bound (result, 0.0, 1.0);

      return (result);
   }
   return (1.0);
}

// Quite stem specific...
void plantStemPart::onHarvest(float cutting_height, float remove_fr,
                              vector<string> &dm_type,
                              vector<float> &dlt_crop_dm,
                              vector<float> &dlt_dm_n,
                              vector<float> &dlt_dm_p,
                              vector<float> &fraction_to_residue)
{
    float fractToResidue = 1.0 - remove_fr;

    // Some biomass is removed according to harvest height
    float fr_height = divide (cutting_height,g.height, 0.0);

    float retain_fr_green, retain_fr_sen, retain_fr_dead;
    if (c.fr_remain.isInitialised())
       retain_fr_green = c.fr_remain.value(fr_height);
    else
       retain_fr_green = 0.0;

    retain_fr_sen  = retain_fr_green;
    retain_fr_dead = retain_fr_green;

    float chop_fr_green = (1.0 - retain_fr_green);
    float chop_fr_dead = (1.0 - retain_fr_dead);
    float chop_fr_sen = (1.0 - retain_fr_sen);

    float dlt_dm_harvest = g.dm_dead * chop_fr_dead
                         + g.dm_green * chop_fr_green
                         + g.dm_senesced * chop_fr_sen;

    float dlt_n_harvest = g.n_dead * chop_fr_dead
                        + g.n_green * chop_fr_green
                        + g.n_senesced * chop_fr_sen;

    float dlt_p_harvest = g.p_dead * chop_fr_dead
                        + g.p_green * chop_fr_green
                        + g.p_sen * chop_fr_sen;


    g.dm_dead *= retain_fr_dead;
    g.dm_senesced *= retain_fr_sen;
    g.dm_green *= retain_fr_green;

    g.n_dead *= retain_fr_dead;
    g.n_senesced *= retain_fr_sen;
    g.n_green *= retain_fr_green;

    g.p_dead *= retain_fr_dead;
    g.p_sen *= retain_fr_sen;
    g.p_green *= retain_fr_green;

    g.height = max(1.0, cutting_height);

    dm_type.push_back(c.name);
    fraction_to_residue.push_back(fractToResidue);
    dlt_crop_dm.push_back(dlt_dm_harvest * gm2kg/sm2ha);
    dlt_dm_n.push_back(dlt_n_harvest * gm2kg/sm2ha);
    dlt_dm_p.push_back(dlt_p_harvest * gm2kg/sm2ha);
}


void fruitPodPart::onHarvest(float /* cutting_height */, float remove_fr,
                              vector<string> &dm_type,
                              vector<float> &dlt_crop_dm,
                              vector<float> &dlt_dm_n,
                              vector<float> &dlt_dm_p,
                              vector<float> &fraction_to_residue)
{
    float fractToResidue = 1.0 - remove_fr;

    float dm_init = u_bound (plantPart::c.dm_init * plant->getPlants(), plantPart::g.dm_green);
    float n_init  = u_bound (             dm_init * plantPart::c.n_init_conc, plantPart::g.n_green);
    float p_init  = u_bound (             dm_init * plantPart::c.p_init_conc, plantPart::g.p_green);

     float retain_fr_green = divide(dm_init, g.dm_green, 0.0);
     float retain_fr_dead  = 0.0;
     float retain_fr_sen   = 0.0;

    float dlt_dm_harvest = g.dm_dead + g.dm_green + g.dm_senesced - dm_init;
    float dlt_n_harvest  = g.n_dead  + g.n_green  + g.n_senesced  - n_init;
    float dlt_p_harvest  = g.p_dead  + g.p_green  + g.p_sen       - p_init;

    g.dm_dead     *= retain_fr_dead;
    g.dm_senesced *= retain_fr_sen;
    g.dm_green    *= retain_fr_green;

    g.n_dead     *= retain_fr_dead;
    g.n_senesced *= retain_fr_sen;
    g.n_green    = n_init;

    g.p_dead  *= retain_fr_dead;
    g.p_sen   *= retain_fr_sen;
    g.p_green  = p_init;

    dm_type.push_back(c.name);
    fraction_to_residue.push_back(fractToResidue);
    dlt_crop_dm.push_back (dlt_dm_harvest * gm2kg/sm2ha);
    dlt_dm_n.push_back    (dlt_n_harvest  * gm2kg/sm2ha);
    dlt_dm_p.push_back    (dlt_p_harvest  * gm2kg/sm2ha);
}

void fruitPodPart::onKillStem(void)
{
       g.dm_dead += g.dm_green + g.dm_senesced;
       g.dm_green = 0.0;
       g.dm_senesced = 0.0;

       g.n_dead += g.n_green + g.n_senesced;
       g.n_green = 0.0;
       g.n_senesced = 0.0;

       g.p_dead += g.p_green + g.p_sen;
       g.p_green = 0.0;
       g.p_sen = 0.0;
}

void fruitPodPart::onFlowering(void)
{
     g.dm_plant_min = 0.0;
}

// set the minimum weight of part; used for retranslocation to grain
void fruitPodPart::onStartGrainFill(void)
{
     g.dm_plant_min = 0.0;
}

void fruitPodPart::doDmMin(float c_pod_trans_frac)
{
   float dm_plant = divide (g.dm_green, plant->getPlants(), 0.0);
   g.dm_plant_min = max (dm_plant * (1.0 - c_pod_trans_frac), g.dm_plant_min);
}


void fruitOilPart::onHarvest(float /* cutting_height */, float remove_fr,
                              vector<string> &dm_type,
                              vector<float> &dlt_crop_dm,
                              vector<float> &dlt_dm_n,
                              vector<float> &dlt_dm_p,
                              vector<float> &fraction_to_residue)
{
    float fractToResidue = 0.0;

     // biomass is removed
     float retain_fr_green = 0.0;
     float retain_fr_sen   = 0.0;
     float retain_fr_dead  = 0.0;

     float chop_fr_green = 1.0 - retain_fr_green;
     float chop_fr_dead  = 1.0 - retain_fr_dead;
     float chop_fr_sen   = 1.0 - retain_fr_sen;

     float dlt_dm_harvest = g.dm_dead     * chop_fr_dead
                          + g.dm_green    * chop_fr_green
                          + g.dm_senesced * chop_fr_sen;

     float dlt_n_harvest = g.n_dead     * chop_fr_dead
                         + g.n_green    * chop_fr_green
                         + g.n_senesced * chop_fr_sen;

     float dlt_p_harvest = g.p_dead  * chop_fr_dead
                         + g.p_green * chop_fr_green
                         + g.p_sen   * chop_fr_sen;

     g.dm_dead     *= retain_fr_dead;
     g.dm_senesced *= retain_fr_sen;
     g.dm_green    *= retain_fr_green;


     g.n_dead     *= retain_fr_dead;
     g.n_senesced *= retain_fr_sen;
     g.n_green    *= retain_fr_green;

     g.p_dead  *= retain_fr_dead;
     g.p_sen   *= retain_fr_sen;
     g.p_green *= retain_fr_green;


     dm_type.push_back(c.name);
     fraction_to_residue.push_back(fractToResidue);
     dlt_crop_dm.push_back (dlt_dm_harvest * gm2kg/sm2ha);
     dlt_dm_n.push_back    (dlt_n_harvest  * gm2kg/sm2ha);
     dlt_dm_p.push_back    (dlt_p_harvest  * gm2kg/sm2ha);

}

void fruitOilPart::onKillStem(void)
{
       g.dm_dead += g.dm_green + g.dm_senesced;
       g.dm_green = 0.0;
       g.dm_senesced = 0.0;

       g.n_dead += g.n_green + g.n_senesced;
       g.n_green = 0.0;
       g.n_senesced = 0.0;

       g.p_dead += g.p_green + g.p_sen;
       g.p_green = 0.0;
       g.p_sen = 0.0;
}

void fruitOilPart::onFlowering(void)
{  // do nothing
}

// set the minimum weight of part; used for retranslocation to grain
void fruitOilPart::onStartGrainFill(void)
{  // do nothing
}

void fruitMealPart::onHarvest(float /* cutting_height */, float remove_fr,
                              vector<string> &dm_type,
                              vector<float> &dlt_crop_dm,
                              vector<float> &dlt_dm_n,
                              vector<float> &dlt_dm_p,
                              vector<float> &fraction_to_residue)
{
    float fractToResidue = 0.0;

     // biomass is removed
     float retain_fr_green = 0.0;
     float retain_fr_sen   = 0.0;
     float retain_fr_dead  = 0.0;

     float chop_fr_green = 1.0 - retain_fr_green;
     float chop_fr_dead  = 1.0 - retain_fr_dead;
     float chop_fr_sen   = 1.0 - retain_fr_sen;

     float dlt_dm_harvest = g.dm_dead     * chop_fr_dead
                          + g.dm_green    * chop_fr_green
                          + g.dm_senesced * chop_fr_sen;

     float dlt_n_harvest = g.n_dead     * chop_fr_dead
                         + g.n_green    * chop_fr_green
                         + g.n_senesced * chop_fr_sen;

     float dlt_p_harvest = g.p_dead  * chop_fr_dead
                         + g.p_green * chop_fr_green
                         + g.p_sen   * chop_fr_sen;

     g.dm_dead     *= retain_fr_dead;
     g.dm_senesced *= retain_fr_sen;
     g.dm_green    *= retain_fr_green;


     g.n_dead     *= retain_fr_dead;
     g.n_senesced *= retain_fr_sen;
     g.n_green    *= retain_fr_green;

     g.p_dead  *= retain_fr_dead;
     g.p_sen   *= retain_fr_sen;
     g.p_green *= retain_fr_green;


     dm_type.push_back (c.name);
     fraction_to_residue.push_back (fractToResidue);
     dlt_crop_dm.push_back (dlt_dm_harvest * gm2kg/sm2ha);
     dlt_dm_n.push_back    (dlt_n_harvest  * gm2kg/sm2ha);
     dlt_dm_p.push_back    (dlt_p_harvest  * gm2kg/sm2ha);
}

void fruitMealPart::onKillStem(void)
{
       g.dm_dead += g.dm_green + g.dm_senesced;
       g.dm_green = 0.0;
       g.dm_senesced = 0.0;

       g.n_dead += g.n_green + g.n_senesced;
       g.n_green = 0.0;
       g.n_senesced = 0.0;

       g.p_dead += g.p_green + g.p_sen;
       g.p_green = 0.0;
       g.p_sen = 0.0;
}

void fruitMealPart::onFlowering(void)
{  // do nothing
}

// set the minimum weight of part; used for retranslocation to grain
void fruitMealPart::onStartGrainFill(void)
{  // do nothing
}



void plantPart::onEndCrop(vector<string> &dm_type,
                          vector<float> &dlt_crop_dm,
                          vector<float> &dlt_dm_n,
                          vector<float> &dlt_dm_p,
                          vector<float> &fraction_to_residue)
{
    dm_type.push_back(c.name);
    dlt_crop_dm.push_back ((g.dm_dead + g.dm_green + g.dm_senesced) * gm2kg/sm2ha);
    dlt_dm_n.push_back    ((g.n_dead  + g.n_green  + g.n_senesced)  * gm2kg/sm2ha);
    dlt_dm_p.push_back    ((g.p_dead  + g.p_green  + g.p_sen)       * gm2kg/sm2ha);
    fraction_to_residue.push_back(1.0);
}

// Add detached material to the parts of a message
//void plantPart::live_detached(vector<string> &dm_type,
//                              vector<float> &fraction_to_residue,
//                              vector<float> &dm,
//                              vector<float> &dm_n,
//                              vector<float> &dm_p)
//{
//    dm_type.push_back(c.name);
//    fraction_to_residue.push_back(1.0);
//    dm.push_back(dlt.dm_detached * gm2kg/sm2ha);
//    dm_n.push_back(dlt.n_detached * gm2kg/sm2ha);
//    dm_p.push_back(dlt.p_det * gm2kg/sm2ha);
//}

/*  Purpose
*     Calculate N available for transfer to grain (g/m^2)
*     from each plant part.
*
*  Mission Statement
*   Calculate the Nitrogen available for retranslocation to grain
*/
float plantPart::availableRetranslocateN(void)
   {
   float N_min = g.n_conc_min * g.dm_green;
   float N_avail = l_bound (g.n_green - N_min, 0.0);
   return (N_avail * c.n_retrans_fraction);
   }

float plantPart::dmTotal(void) {return (dmGreen() + dmSenesced() + dmDead());}
float plantPart::dmGreen(void) const {return (g.dm_green);}
float plantPart::dmSenesced(void) const {return (g.dm_senesced);}
float plantPart::dmDead(void) const {return (g.dm_dead);}

float plantPart::dmGreenStressDeterminant(void)
{
    if (c.p_stress_determinant)
       return g.dm_green;
    else
       return 0.0;
}

float plantPart::pGreenStressDeterminant(void)
{
    if (c.p_stress_determinant)
       return g.p_green;
    else
       return 0.0;
}

float plantPart::pMaxPotStressDeterminant(void)
{
    if (c.p_stress_determinant)
       return pMaxPot();
    else
       return 0.0;
}

float plantPart::pMinPotStressDeterminant(void)
{
    if (c.p_stress_determinant)
       return pMinPot();
    else
       return 0.0;
}
float plantPart::nDemand(void) const {return (v.n_demand);}
float plantPart::nMax(void) const{return (v.n_max);}

float plantPart::pDemand(void) {return (v.p_demand);}
float plantPart::nTotal(void) {return (nGreen() + nSenesced() + nDead());}
float plantPart::nGreen(void) const {return (g.n_green);}
float plantPart::nSenesced(void) const {return (g.n_senesced);}
float plantPart::nDead(void) const {return (g.n_dead);}
float plantPart::nConc(void) const
{
    float n_conc = divide (g.n_green, g.dm_green, 0.0) * 100.0;     //FIXME ?? 100.0
    return n_conc;
}

float plantPart::nMaxPot(void)
{
//       float n_conc_max = linear_interp_real (plant->getStageCode()
//                                           , c.x_stage_code
//                                           , c.y_conc_max
//                                           , c.num_x_stage_code);
   float n_conc_max = c.n_conc_max.value(plant->getStageCode());
    return n_conc_max * g.dm_green;
}

float plantPart::nMinPot(void)
{
//       float n_conc_min = linear_interp_real (plant->getStageCode()
//                                           , c.x_stage_code
//                                           , c.y_conc_min
//                                           , c.num_x_stage_code);
   float n_conc_min = c.n_conc_min.value(plant->getStageCode());
    return n_conc_min * g.dm_green;
}
float plantPart::pTotal(void) {return (pGreen() + pSenesced() + pDead());}
float plantPart::pGreen(void) const {return (g.p_green);}
float plantPart::pSenesced(void) const {return (g.p_sen);}
float plantPart::pDead(void) const {return (g.p_dead);}
float plantPart::pConc(void) const
{
    float p_conc = divide (g.p_green, g.dm_green, 0.0) * 100.0;        //FIXME ?? 100.0
    return p_conc;
}

float plantPart::pRetransSupply(void)
{
    if (c.p_retrans_part)
       return max(g.p_green - pMinPot(), 0.0);
    else
       return 0.0;
}

float plantPart::pRetransDemand(void)
{
    if (c.p_yield_part)
       return max(pMaxPot() - g.p_green, 0.0);
    else
       return 0.0;
}


void plantPart::distributeDltPGreen(float p_uptake, float total_p_demand)
{
    dlt.p_green = p_uptake * divide(v.p_demand
                                     , total_p_demand
                                     , 0.0);
}

void plantPart::distributeDltPRetrans(float total_p_supply, float total_p_demand)
{
   float p_supply = pRetransSupply();
   float p_demand = pRetransDemand();
   if (p_supply > 0.0)
      {
      float fraction = divide(total_p_demand, total_p_supply, 0.0);
      fraction = bound(fraction, 0.0, 1.0);
      dlt.p_retrans = - p_supply * fraction;
      }
   else if (p_demand > 0.0)
      {
      float fraction = divide(total_p_supply, total_p_demand, 0.0);
      fraction = bound(fraction, 0.0, 1.0);
      dlt.p_retrans = p_demand * fraction;
      }
   else
      {
      dlt.p_retrans = 0.0;// this part is not involved
      }
}

void plantPart::pInit()
{
   g.p_green = c.p_init_conc * g.dm_green;
}

float plantPart::pMaxPot(void)
{
       float p_conc_max = linear_interp_real (plant->getStageCode()
                                           , c.x_p_stage_code
                                           , c.y_p_conc_max
                                           , c.num_x_p_stage_code);
//   float p_conc_max = c.p_conc_max.value(plant->getStageCode());
    return p_conc_max * g.dm_green;
}

float plantPart::pMinPot(void)
{
       float p_conc_min = linear_interp_real (plant->getStageCode()
                                           , c.x_p_stage_code
                                           , c.y_p_conc_min
                                           , c.num_x_p_stage_code);
//   float p_conc_min = c.p_conc_min.value(plant->getStageCode());
    return p_conc_min * g.dm_green;
}
void plantPart::onPlantEvent(const string &event)
   {
   if (event == "emergence") onEmergence();
   else if (event == "flowering") onFlowering();
   else if (event == "start_grain_fill") onStartGrainFill();
   }



void plantPart::get_p_demand(vector<float> &p_demand)
{
   p_demand.push_back(v.p_demand);
}

void plantPart::get_dlt_p_green(vector<float> &dlt_p_green)
{
   dlt_p_green.push_back(dlt.p_green);
}

void plantPart::get_p_green(vector<float> &p_green)
{
   p_green.push_back(g.p_green);
}

void plantPart::get_dlt_p_retrans(vector<float> &dlt_p_retrans)
{
   dlt_p_retrans.push_back(dlt.p_retrans);
}

//-------------------Hacks-------------------------------
void plantPartHack::get(void) {
      v.dm_green_demand     = myplant->g.dm_green_demand[part];
      g.dm_dead             = myplant->g.dm_dead[part];
      g.dm_green            = myplant->g.dm_green[part];
      g.dm_senesced         = myplant->g.dm_senesced[part];
      v.n_demand            = myplant->g.n_demand[part];
      v.soil_n_demand       = myplant->g.soil_n_demand[part];
      v.n_max               = myplant->g.n_max[part];
      g.n_dead              = myplant->g.n_dead[part];
      g.n_green             = myplant->g.n_green[part];
      g.n_senesced          = myplant->g.n_senesced[part];
      g.width               = myplant->g.canopy_width;
      g.n_conc_crit         = myplant->g.n_conc_crit[part];
      g.n_conc_max          = myplant->g.n_conc_max[part];
      g.n_conc_min          = myplant->g.n_conc_min[part];
      g.dm_plant_min        = myplant->g.dm_plant_min[part];
      dlt.dm_green          = myplant->g.dlt_dm_green[part];
      dlt.dm_senesced       = myplant->g.dlt_dm_senesced[part];
      dlt.dm_detached       = myplant->g.dlt_dm_detached[part];
      dlt.dm_dead_detached  = myplant->g.dlt_dm_dead_detached[part];
      dlt.dm_green_retrans  = myplant->g.dlt_dm_green_retrans[part];
      dlt.n_green           = myplant->g.dlt_n_green[part];
      dlt.n_senesced        = myplant->g.dlt_n_senesced[part];
      dlt.n_senesced_retrans= myplant->g.dlt_n_senesced_retrans[part];
      dlt.n_senesced_trans  = myplant->g.dlt_n_senesced_trans[part];
      dlt.n_detached        = myplant->g.dlt_n_detached[part];
      dlt.n_dead            = myplant->g.dlt_n_dead[part];
      dlt.n_dead_detached   = myplant->g.dlt_n_dead_detached[part];
      dlt.n_retrans         = myplant->g.dlt_n_retrans[part];
      dlt.height            = myplant->g.dlt_canopy_height;
      dlt.width             = myplant->g.dlt_canopy_width;
      g.p_dead              = myplant->g.p_dead[part];
      g.p_green             = myplant->g.p_green[part];
      g.p_sen               = myplant->g.p_sen[part];
      dlt.p_green           = myplant->g.dlt_p_green[part];
      dlt.p_sen             = myplant->g.dlt_p_sen[part];
      dlt.p_det             = myplant->g.dlt_p_det[part];
      dlt.p_dead_det        = myplant->g.dlt_p_dead_det[part];
      dlt.p_retrans         = myplant->g.dlt_p_retrans[part];
      dlt.p_dead            = myplant->g.dlt_p_dead[part];
      v.p_demand            = myplant->g.p_demand[part];

      c.p_stress_determinant =myplant->c.p_stress_determinants[part];
      c.p_yield_part    = myplant->c.p_yield_parts[part];
      c.p_retrans_part  = myplant->c.p_retrans_parts[part];

      c.p_init_conc         = myplant->c.p_conc_init[part];
      c.num_x_p_stage_code  = myplant->c.num_x_p_stage_code;
      c.num_x_p_stage_code  = myplant->c.num_x_p_stage_code;
      for (int i = 0; i< myplant->c.num_x_p_stage_code; i++) {
      	  c.x_p_stage_code[i] = myplant->c.x_p_stage_code[i];
          c.y_p_conc_max [i]  = myplant->c.y_p_conc_max  [part][i];
          c.y_p_conc_min [i]  = myplant->c.y_p_conc_min  [part][i];
          c.y_p_conc_sen [i]  = myplant->c.y_p_conc_sen  [part][i];
      }
}

void plantPartHack::put(void) {
      myplant->g.dm_green_demand[part]  =            v.dm_green_demand       ;
      myplant->g.dm_dead[part]=                      g.dm_dead               ;
      myplant->g.dm_green[part]=                     g.dm_green              ;
      myplant->g.dm_senesced[part]=                  g.dm_senesced           ;
      myplant->g.n_demand[part]=                     v.n_demand              ;
      myplant->g.soil_n_demand[part]=                v.soil_n_demand         ;
      myplant->g.n_max[part]=                        v.n_max                 ;
      myplant->g.n_dead[part]=                       g.n_dead                ;
      myplant->g.n_green[part]=                      g.n_green               ;
      myplant->g.n_senesced[part]=                   g.n_senesced            ;
      myplant->g.canopy_width=                        g.width                ;
      myplant->g.n_conc_crit[part]=                  g.n_conc_crit           ;
      myplant->g.n_conc_max[part]=                   g.n_conc_max            ;
      myplant->g.n_conc_min[part]=                   g.n_conc_min            ;
      myplant->g.dm_plant_min[part]=                 g.dm_plant_min          ;
      myplant->g.dlt_dm_green[part]=                 dlt.dm_green            ;
      myplant->g.dlt_dm_senesced[part]=              dlt.dm_senesced         ;
      myplant->g.dlt_dm_detached[part]=              dlt.dm_detached         ;
      myplant->g.dlt_dm_dead_detached[part]=         dlt.dm_dead_detached    ;
      myplant->g.dlt_dm_green_retrans[part]=         dlt.dm_green_retrans    ;
      myplant->g.dlt_n_green[part]=                  dlt.n_green             ;
      myplant->g.dlt_n_senesced[part]=               dlt.n_senesced          ;
      myplant->g.dlt_n_senesced_retrans[part]=       dlt.n_senesced_retrans  ;
      myplant->g.dlt_n_senesced_trans[part]=         dlt.n_senesced_trans    ;
      myplant->g.dlt_n_detached[part]=               dlt.n_detached          ;
      myplant->g.dlt_n_dead[part]=                   dlt.n_dead              ;
      myplant->g.dlt_n_dead_detached[part]=          dlt.n_dead_detached     ;
      myplant->g.dlt_n_retrans[part]=                dlt.n_retrans           ;

      myplant->g.p_dead[part]=                       g.p_dead                ;
      myplant->g.p_green[part]=                      g.p_green               ;
      myplant->g.p_sen[part]=                        g.p_sen            ;

      myplant->g.dlt_p_green[part]                     =dlt.p_green;
      myplant->g.dlt_p_sen[part]                       =dlt.p_sen;
      myplant->g.dlt_p_det[part]                       =dlt.p_det;
      myplant->g.dlt_p_dead_det[part]                  =dlt.p_dead_det;
      myplant->g.dlt_p_retrans[part]                   =dlt.p_retrans;
      myplant->g.dlt_p_dead[part]                      =dlt.p_dead;
      myplant->g.p_demand[part]                        =v.p_demand;

      //myplant->g.dlt_canopy_height=                   dlt.height              ;xxstem only??
      //myplant->g.dlt_canopy_width=                    dlt.width               ;
};
