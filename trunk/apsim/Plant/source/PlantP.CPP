/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *                                                  *
*       Author:  John Hargreaves                                               *
*     Date written: 25 Feb 2004                                                *
* Acknowledgements: Neil Huth, CSIRO, Sustainable Ecosystems.                  *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
//
//			PlantP.cpp
//             	PlantP class definition (Orthodox Canonical Form)
//
//             	Defines default constructor, copy constructor and
//                assignment operator.
//
// Modification log
//  25 Feb 04  J. Hargreaves  implementation

#include <stdio.h>
#include <math.h>
#include <map>
#include <string>
#include <algorithm>
#include <stdexcept>
#include <cstring.h>
#include <iostream.h>
#include <boost/function.hpp>
#include <boost/bind.hpp>

using namespace std;

#include <ComponentInterface/Type.h>
#include <ComponentInterface/ApsimVariant.h>
#include <ComponentInterface/Component.h>
#include <ComponentInterface/dataTypes.h>
#include <ComponentInterface/Messages.h>
#include <ComponentInterface/MessageDataExt.h>
#include <ApsimShared/ApsimComponentData.h>
#include <ApsimShared/FStringExt.h>
#include <general/string_functions.h>

#include "PlantLibrary.h"
#include "PlantComponent.h"
#include "Plant.h"
#include "PlantP.h"


static const char* floatType =        "<type kind=\"single\"/>";
static const char* floatArrayType =   "<type kind=\"single\" array=\"T\"/>";

/////////////These might be redundancies??//////////
void push_routine (const char *) {};
void pop_routine (const char *) {};


// default constructor
// 	initialise data members.
PlantP::PlantP(Plant *P)  			           // member initialisation
{
    plant = P;
    zero_variables();
 //   fprintf (stdout, "constructor\n");
}


// copy constructor
//	copy data members of object
PlantP::PlantP(const PlantP &PlantP)       // member initialisation
{
	throw std::invalid_argument("Copy constructor NI for plantp");
}


// Assigment operator
//	assign data members of object
const PlantP &PlantP::operator=(const PlantP &other)
{
 	throw std::invalid_argument("Assignment operator NI for plantp");
}

// destructor
PlantP::~PlantP()
{
}

// Register Methods & Events
void PlantP::doIDs(PlantComponent *systemInterface)
{
    // we want to send these events out
    ids.add_residue_p = systemInterface->addRegistration(RegistrationType::event,
                                    "add_residue_p", "",
                                    "", "");
    ids.incorp_fom_p = systemInterface->addRegistration(RegistrationType::event,
                                    "incorp_fom_p", "",
                                    "", "");
    // we will want this variable
    // ids.layered_p_uptake = systemInterface->addRegistration(RegistrationType::get,
    //                                "layered_p_uptake", floatType,
    //                                "", "");
}

// Register Gets & Sets
void PlantP::doRegistrations(protocol::Component *systemInterface)
   {
#define setupGetVar systemInterface->addGettableVar
#define setupGetFunction(name,type,length,address,units,desc) {\
   boost::function2<void, protocol::Component *, protocol::QueryValueData &> fn;\
   fn = boost::bind(address, this, _1, _2); \
   systemInterface->addGettableVar(name, type, length, fn, units, desc);\
   }

   setupGetVar("p_green", max_part,
               g.part_p_green,
               "g/m^2", "P content of plant parts");

   setupGetVar("p_sen", max_part,
               g.part_p_sen,
               "g/m^2", "P content of plant parts");

   setupGetFunction("p_demand", protocol::DTsingle, false,
                    &PlantP::get_p_demand,
                    "g/m^2","");

   setupGetVar("pfact_photo", 
               g.pfact_photo,
               "", "P factor in photosynthesis");

   setupGetVar("pfact_pheno", 
               g.pfact_pheno,
               "", "P factor in phenology");

   setupGetVar("pfact_expansion", 
               g.pfact_expansion,
               "", "P factor in leaf expansion");

   setupGetVar("pfact_grain",  
               g.pfact_grain,
               "", "P factor in grain");

   setupGetFunction("p_stress_photo", protocol::DTsingle, false,
                    &PlantP::get_pstress_photo,
                    "", "P stress in photosynthesis");

   setupGetFunction("p_stress_pheno", protocol::DTsingle, false,
                    &PlantP::get_pstress_pheno,
                    "", "P stress in phenology");

   setupGetFunction("p_stress_expansion", protocol::DTsingle, false,
                    &PlantP::get_pstress_expansion,
                    "", "P stress in leaf expansion");

   setupGetFunction("p_stress_expan", protocol::DTsingle, false,
                    &PlantP::get_pstress_expansion,
                    "", "P stress in leaf expansion");

   setupGetFunction("p_stress_grain", protocol::DTsingle, false,
                    &PlantP::get_pstress_grain,
                    "", "P stress in grain");

   setupGetFunction("biomass_p", protocol::DTsingle, false,
                    &PlantP::get_biomass_p,
                    "g/m^2","P in biomass");

   setupGetFunction("p_uptake", protocol::DTsingle, false,
                    &PlantP::get_biomass_p,
                    "g/m^2","P  uptake");

   setupGetFunction("green_biomass_p", protocol::DTsingle, false,
                    &PlantP::get_green_biomass_p,
                    "g/m^2","P in green biomass");

   setupGetFunction("grain_p", protocol::DTsingle, false,
                    &PlantP::get_grain_p,
                    "g/m^2","P in grain");

   setupGetVar("leaf_p", 
               g.part_p_green[leaf],
               "g/m^2","P in leaf");

   setupGetVar("stem_p",  
               g.part_p_green[stem],
               "g/m^2","P in stem");

   setupGetVar("root_p",  
               g.part_p_green[root],
               "g/m^2","P in roots");

   setupGetVar("deadleaf_p", 
               g.part_p_sen[leaf],
               "g/m^2","P in dead leaf");

   setupGetFunction("head_p", protocol::DTsingle, false,
                    &PlantP::get_head_p,
                    "g/m^2","P in head");

   setupGetFunction("pod_p", protocol::DTsingle, false,
                    &PlantP::get_pod_p,
                    "g/m^2","P in pod shell");

   setupGetVar("p_senesced", max_part,
               g.part_p_sen,
               "g/m^2","P in senesced parts");

   setupGetVar("p_dead", max_part,
               g.part_p_dead,
               "g/m^2","P in dead parts");

   setupGetVar("dlt_p_green", max_part,
               g.dlt_part_p_green,
               "g/m^2","dlt P parts");

   setupGetVar("dlt_p_retrans", max_part,
               g.dlt_part_p_retrans,
               "g/m^2","dlt P parts");

// john: this was missing?
   setupGetVar("dlt_p_det", max_part,
               g.dlt_part_p_det,
               "g/m^2","dlt P detached");

   setupGetVar("dlt_p_detached",  max_part,
               g.dlt_part_p_det,
               "g/m^2","dlt P detached");

   setupGetVar("dlt_p_dead", max_part,
               g.dlt_part_p_dead,
               "g/m^2","dlt P in dead");

   setupGetVar("dlt_p_sen", max_part,
               g.dlt_part_p_sen,
               "g/m^2","dlt P in senesced");

   setupGetFunction("p_conc_stover", protocol::DTsingle, false,
                    &PlantP::get_p_conc_stover,
                    "%","P in stover");

   setupGetFunction("p_conc_leaf", protocol::DTsingle, false,
                    &PlantP::get_p_conc_leaf,
                    "%","P in leaf");

   setupGetFunction("p_conc_stem", protocol::DTsingle, false,
                    &PlantP::get_p_conc_stem,
                    "%","P in stem");

   setupGetFunction("p_conc_grain", protocol::DTsingle, false,
                    &PlantP::get_p_conc_grain,
                    "%","P in grain");

   setupGetFunction("p_grain_pcnt", protocol::DTsingle, false,
                    &PlantP::get_p_conc_grain,
                    "%","P in grain");

   setupGetFunction("p_uptake_stover", protocol::DTsingle, false,
                    &PlantP::get_p_uptake_stover,
                    "%","P in stover");

   setupGetVar("grain_p_demand", 
               g.part_demand[meal],
               "g/m^2","P demand of grain");

#undef setupGetVar
#undef setupGetFunction
   }

// =======================================
void PlantP::zero_variables ()
{
//+  Purpose
//     Set all variables in this module to zero.

//+  Changes
//     <insert here>

//+  Constant Values
   const char*  my_name = "PlantP::zero_variables";
//- Implementation Section ----------------------------------
      push_routine (my_name);

      // Parameters
      // ==========

      // Globals
      // =======
     g.growth_stage = 0.0;      // Reals
      fill_real_array (g.part_p_green, 0.0, max_part);
      fill_real_array (g.dlt_part_p_green, 0.0, max_part);
      fill_real_array (g.part_p_sen, 0.0, max_part);
      fill_real_array (g.part_p_dead, 0.0, max_part);
      fill_real_array (g.dlt_part_p_dead, 0.0, max_part);
      fill_real_array (g.dlt_part_p_sen, 0.0, max_part);
      fill_real_array (g.dlt_part_p_det, 0.0, max_part);
      fill_real_array (g.dlt_part_p_dead_det, 0.0, max_part);
      fill_real_array (g.dlt_part_p_retrans, 0.0, max_part);
      fill_real_array (g.part_demand, 0.0, max_part);

      g.part_names.clear();

      g.num_parts = 0;           // Integers

      // Constants
      // =========
      c.stress_determinants.clear();
      fill_real_array (c.x_p_stage_code, 0.0, max_part);  // Reals

      for (int part = 0; part < max_part; part++)
      {
         fill_real_array (c.y_p_conc_max[part], 0.0, max_table);
      }
      for (int part = 0; part < max_part; part++)
      {
         fill_real_array (c.y_p_conc_min[part], 0.0, max_table);
      }
      for (int part = 0; part < max_part; part++)
      {
         fill_real_array (c.y_p_conc_sen[part], 0.0, max_table);
      }
      c.pfact_photo_slope = 0.0;
      c.pfact_expansion_slope = 0.0;
      c.pfact_pheno_slope = 0.0;
      c.num_x_p_stage_code = 0;
      g.phosphorus_aware = false;

      pop_routine (my_name);
 }


// ====================================================================
//     PlantP_Send_my_variable (const char *Variable_name)


//+  Purpose
//      Return the value of one of our variables to caller
//      Called by function pointer registered through addGettableVar()
void PlantP::get_p_demand(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    float p_demand = sum_real_array(g.part_demand, max_part);
    systemInterface->sendVariable(qd, p_demand);   //(g/m^2
}

void PlantP::get_pfact_grain(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    systemInterface->sendVariable(qd, g.pfact_grain);  //()
}

void PlantP::get_pstress_photo(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    float pstress_photo;
    if (g.pfact_photo > 0.0)
       pstress_photo = 1.0 - g.pfact_photo;
    else
       pstress_photo = 0.0;
    systemInterface->sendVariable(qd, pstress_photo);  //()
}

void PlantP::get_pstress_pheno(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    float pstress_pheno;
    if (g.pfact_pheno > 0.0)
       pstress_pheno = 1.0 - g.pfact_pheno;
    else
       pstress_pheno = 0.0;
    systemInterface->sendVariable(qd, pstress_pheno);  //()
}

void PlantP::get_pstress_expansion(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    float pstress_expansion;
    if (g.pfact_expansion > 0.0)
       pstress_expansion = 1.0 - g.pfact_expansion;
    else
       pstress_expansion = 0.0;
    systemInterface->sendVariable(qd, pstress_expansion);  //()
}

void PlantP::get_pstress_grain(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    float pstress_grain;
    if (g.pfact_grain > 0.0)
       pstress_grain = 1.0 - g.pfact_grain;
    else
       pstress_grain = 0.0;
    systemInterface->sendVariable(qd, pstress_grain);  //()
}

void PlantP::get_biomass_p(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    float biomass_p = sum_real_array (g.part_p_green, max_part) - g.part_p_green[root]
                    + sum_real_array (g.part_p_sen, max_part)   - g.part_p_sen[root]
                    + sum_real_array (g.part_p_dead, max_part)  - g.part_p_dead[root];
    systemInterface->sendVariable(qd, biomass_p);  //()
}

void PlantP::get_green_biomass_p(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    float biomass_p = sum_real_array (g.part_p_green, max_part) - g.part_p_green[root];
    systemInterface->sendVariable(qd, biomass_p);  //()
}

void PlantP::get_grain_p(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    float grain_p = g.part_p_green[meal] + g.part_p_green[oil];
    systemInterface->sendVariable(qd, grain_p);  //()
}

void PlantP::get_pod_p(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    systemInterface->sendVariable(qd, g.part_p_green[pod]);  //()
}

void PlantP::get_head_p(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    systemInterface->sendVariable(qd, g.part_p_green[pod] + g.part_p_green[meal]);  //()
}

void PlantP::get_p_conc_stover(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    float p_conc_stover = divide (g.part_p_green[leaf] + g.part_p_green[stem] + g.part_p_green[pod]
                                , plant->g.dm_green[leaf] + plant->g.dm_green[stem] + plant->g.dm_green[pod]
                                , 0.0) * 100.0 ;
    systemInterface->sendVariable(qd, p_conc_stover);  //()
}

void PlantP::get_p_conc_leaf(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    float p_conc_leaf = divide (g.part_p_green[leaf]
                              , plant->g.dm_green[leaf]
                              , 0.0) * 100.0 ;
    systemInterface->sendVariable(qd, p_conc_leaf);  //()
}

void PlantP::get_p_conc_stem(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    float p_conc_stem = divide (g.part_p_green[stem]
                              , plant->g.dm_green[stem]
                              , 0.0) * 100.0 ;
    systemInterface->sendVariable(qd, p_conc_stem);  //()
}

void PlantP::get_p_conc_grain(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    float p_conc_grain = divide (g.part_p_green[meal] + g.part_p_green[oil]
                               , plant->g.dm_green[meal] + plant->g.dm_green[oil]
                               , 0.0) * 100.0;
    systemInterface->sendVariable(qd, p_conc_grain);  //()
}

void PlantP::get_p_uptake_stover(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    float p_uptake_stover = g.part_p_green[leaf]+g.part_p_green[stem]+g.part_p_green[pod];
    systemInterface->sendVariable(qd, p_uptake_stover);  //()
}


//     ===========================================================
void PlantP::PlantP_read_param ()
{
   //+  Purpose
   //       Read all module parameters.

   //+  Changes
   //     <insert here>

   //+  Constant Values
   const char*  my_name = "PlantP_read_param";
   //- Implementation Section ----------------------------------
   push_routine (my_name);

   pop_routine  (my_name);
}


//     ===========================================================
void PlantP::read_constants (PlantComponent *systemInterface)
{
//+  Purpose
//       Read all module constants.

//+  Constant Values
    const char*  my_name = "PlantP_read_constants";
    const char*  section_name = "constants" ;

//+  Local Variables
    int    numvals;                  // number of values read
    int    num_sections;             // number of sections to search
    int    part;
    float  temp[max_table];
    string scratch;
//- Implementation Section ----------------------------------

    push_routine (my_name);

    scratch = systemInterface->readParameter (section_name, "stress_determinants");
    Split_string(scratch, " ", c.stress_determinants);
    scratch = systemInterface->readParameter (section_name, "yield_parts");
    Split_string(scratch, " ", c.yield_parts);
    scratch = systemInterface->readParameter (section_name, "retrans_parts");
    Split_string(scratch, " ", c.retrans_parts);
    systemInterface->readParameter (section_name, "pfact_photo_slope",    /*"()",   */ c.pfact_photo_slope    , 0.0, 100.0);
    systemInterface->readParameter (section_name, "pfact_expansion_slope",/*"()",   */ c.pfact_expansion_slope, 0.0, 100.0);
    systemInterface->readParameter (section_name, "pfact_pheno_slope",    /*"()",   */ c.pfact_pheno_slope    , 0.0, 100.0);
    systemInterface->readParameter (section_name, "pfact_grain_slope",    /*"()",   */ c.pfact_grain_slope    , 0.0, 100.0);
    systemInterface->readParameter (section_name, "x_p_stage_code",       /*"(g/g)",*/ c.x_p_stage_code, c.num_x_p_stage_code, 0.0, 100.0);

    for (part = 0; part < g.num_parts; part++)
    {
       string keyword ="y_p_conc_max_" + g.part_names[part];
       systemInterface->readParameter (section_name, keyword, /*"(g/g)",*/ temp, c.num_x_p_stage_code, 0.0, 1.0);

       for (int i=0; i < c.num_x_p_stage_code; i++)
       {
          c.y_p_conc_max[part][i] = temp[i];
       }

       keyword = "y_p_conc_sen_" + g.part_names[part];
       systemInterface->readParameter (section_name, keyword, /*"(g/g)", */temp, c.num_x_p_stage_code, 0.0, 1.0);

       for (int i=0; i < c.num_x_p_stage_code; i++)
       {
          c.y_p_conc_sen[part][i] = temp[i];
       }

       keyword = "y_p_conc_min_" + g.part_names[part];
       systemInterface->readParameter (section_name, keyword, /*"(g/g)",*/ temp, c.num_x_p_stage_code, 0.0, 1.0);

       for (int i=0; i < c.num_x_p_stage_code; i++)
       {
          c.y_p_conc_min[part][i] = temp[i];
       }

       keyword = "p_conc_init_" + g.part_names[part];
       systemInterface->readParameter (section_name, keyword, /*"(g/g)",*/ c.p_conc_init[part], 0.0, 1.0);
   }

   pop_routine  (my_name);
}


// ================================================
void PlantP::prepare (float growth_stage
                        , float part_wts[]
                        , float dlt_dm_pot)
{
//+  Purpose

//+  Changes
//

//+  Constant Values
      const char*  my_name = "PlantP_prepare";

//- Implementation Section ----------------------------------
      push_routine (my_name);

      if (g.phosphorus_aware == true)
      {
         PlantP_demand(growth_stage, part_wts, dlt_dm_pot);
         PlantP_Stress(growth_stage, part_wts);
      }

      pop_routine (my_name);
}

// ====================================================================
void PlantP::process(float growth_stage
                           , float dm_green[]
                           , float dlt_dm_senesced[])
{
//+  Purpose
//     <insert here>

//+  Changes
//

//+  Constant Values
      const char*  my_name = "PlantP_process";

//+  Local Variables
      int   part;

//- Implementation Section ----------------------------------
      push_routine (my_name);

      if (g.phosphorus_aware == true)
      {
         PlantP_init_pools(dm_green);

         PlantP_partition ();
         PlantP_senescence (growth_stage
                          , dm_green
                          , dlt_dm_senesced);

         for (part = 0; part < max_part; part++)
         {
            g.part_p_green[part] = g.part_p_green[part]
                                 + g.dlt_part_p_green[part]
                                 - g.dlt_part_p_sen[part];
         }

         // Now do any retranslocation to try and keep pools
         // at target concentrations.

         PlantP_retrans (growth_stage, dm_green);

         for (part = 0; part < max_part; part++)
         {
            g.part_p_green[part] = g.part_p_green[part]
                                 + g.dlt_part_p_retrans[part];
         }
         fprintf(stdout,"end process\n");
      }
      pop_routine (my_name);
}

// ====================================================================
void PlantP::detachment(float dm_senesced[]
                      , float dm_dead[]
                      , float dlt_dm_detached[]
                      , float dlt_dm_dead_detached[])
{
//+  Purpose
//     <insert here>

//+  Changes
//

//+  Constant Values
      const char*  my_name = "PlantP_detachment";

//+  Local Variables
      int   part;


//- Implementation Section ----------------------------------
      push_routine (my_name);

      if (g.phosphorus_aware == true)
      {
         PlantP_detachment (dm_senesced, dm_dead, dlt_dm_detached, dlt_dm_dead_detached);

         for (part = 0; part < max_part; part++)
         {
            g.part_p_sen[part] = g.part_p_sen[part]
                               + g.dlt_part_p_sen[part]
                               - g.dlt_part_p_det[part];

            g.part_p_dead[part] -= g.dlt_part_p_dead_det[part];
         }
      }
      pop_routine (my_name);
}

// ====================================================================
void PlantP::death(float g_plants, float g_dlt_plants)
{
//+  Purpose
//     <insert here>

//+  Changes
//

//+  Constant Values
      const char*  my_name = "PlantP_death";

//+  Local Variables
      int   part;


//- Implementation Section ----------------------------------
      push_routine (my_name);

      if (g.phosphorus_aware == true)
      {
          float dying_fract_plants = divide (-g_dlt_plants, g_plants, 0.0);
          dying_fract_plants = bound (dying_fract_plants, 0.0, 1.0);

         for (part = 0; part < max_part; part++)
         {
            float dlt_p_green_dead = g.part_p_green[part] * dying_fract_plants;
            g.part_p_green[part] = g.part_p_green[part] - dlt_p_green_dead;
            g.part_p_dead[part]  = g.part_p_dead[part]  + dlt_p_green_dead;

            float dlt_p_senesced_dead = g.part_p_sen[part] * dying_fract_plants;
            g.part_p_sen[part]  = g.part_p_sen[part]  - dlt_p_senesced_dead;
            g.part_p_dead[part] = g.part_p_dead[part] + dlt_p_senesced_dead;
         }
      }
      pop_routine (my_name);
}

// ===============================
void PlantP::PlantP_partition ()
{
//+  Purpose
//     <insert here>

//+  Changes
//

//+  Constant Values
      const char*  my_name = "PlantP_partition";

//+  Local Variables
      vector<float> values;               // Scratch area
      int   numvals;
      int   part;
      float p_uptake;
      float total_demand;


//- Implementation Section ----------------------------------
      push_routine (my_name);

      if (ids.layered_p_uptake != 0)
      {
          plant->parent->getVariable(ids.layered_p_uptake, values, 0.0, 100.0);  //XXX ick..
          float sumValue = 0.0;
          for (unsigned int i = 0; i < values.size(); i++)
          {
          	sumValue += values[i];
          }
          p_uptake = sumValue * kg2gm/ha2sm;
      }
      else
      {
          p_uptake = sum_real_array (g.part_demand, g.num_parts);
      }

      total_demand = sum_real_array (g.part_demand, g.num_parts);

      for (part = 0; part < g.num_parts; part++)
      {
         g.dlt_part_p_green[part] = p_uptake
                                  * divide(g.part_demand[part]
                                           ,total_demand
                                           ,0.0);
      }
      pop_routine (my_name);
}

// ====================================================================
void PlantP::PlantP_senescence (float growth_stage
                              , float part_wts[]
                              , float dlt_part_sen[])
{
//+  Purpose
//     <insert here>

//+  Changes
//

//+  Constant Values
      const char*  my_name = "PlantP_senescence";

//+  Local Variables
      int   part;
      float p_conc_green;
      float p_conc_sen;

//- Implementation Section ----------------------------------
      push_routine (my_name);

      fill_real_array (g.dlt_part_p_sen, 0.0, max_part);

      for (part = 0; part < g.num_parts; part++)
      {
         if (dlt_part_sen[part] < part_wts[part])
         {
            p_conc_green = divide (g.part_p_green[part]
                                  , part_wts[part]
                                  , 0.0);

            p_conc_sen = linear_interp_real (growth_stage
                                           , c.x_p_stage_code
                                           , c.y_p_conc_sen[part]
                                           , c.num_x_p_stage_code);

            g.dlt_part_p_sen[part] = min(p_conc_green, p_conc_sen) * dlt_part_sen[part];
         }
         else
         {
            g.dlt_part_p_sen[part] = g.part_p_green[part];
         }
      }

      pop_routine (my_name);
}


// ====================================================================
void PlantP::PlantP_detachment (float dm_senesced[]
                              , float dm_dead[]
                              , float dlt_dm_detached[]
                              , float dlt_dm_dead_detached[])
{
//+  Purpose

//+  Changes

//+  Constant Values
      const char*  my_name = "PlantP_detachment";

//+  Local Variables


//- Implementation Section ----------------------------------
      push_routine (my_name);

      fill_real_array (g.dlt_part_p_det, 0.0, max_part);
      fill_real_array (g.dlt_part_p_dead_det, 0.0, max_part);

      for (int part = 0; part < g.num_parts; part++)
      {
         float sen_detach_frac = divide(dlt_dm_detached[part]
                                       , dm_senesced[part]
                                       , 0.0);

         g.dlt_part_p_det[part] = g.part_p_sen[part] * sen_detach_frac;

         float dead_detach_frac = divide(dlt_dm_dead_detached[part]
                                       , dm_dead[part]
                                       , 0.0);

         g.dlt_part_p_dead_det[part] = g.part_p_dead[part] * dead_detach_frac;
      }

      pop_routine (my_name);
}


// ====================================================================
void PlantP::zero_daily_variables ()
{

//+  Purpose
//     <insert here>

//+  Changes
//

//+  Constant Values
      const char*  my_name = "PlantP::zero_daily_variables";

//- Implementation Section ----------------------------------
      push_routine (my_name);

      fill_real_array (g.dlt_part_p_green, 0.0, max_part);
      fill_real_array (g.dlt_part_p_dead, 0.0, max_part);
      fill_real_array (g.dlt_part_p_sen, 0.0, max_part);
      fill_real_array (g.dlt_part_p_det, 0.0, max_part);
      fill_real_array (g.dlt_part_p_dead_det, 0.0, max_part);
      fill_real_array (g.dlt_part_p_retrans, 0.0, max_part);
      fill_real_array (g.part_demand, 0.0, max_part);
      g.pfact_photo        = 0.0;
      g.pfact_expansion    = 0.0;
      g.pfact_pheno        = 0.0;
      g.pfact_grain        = 0.0;

      pop_routine (my_name);
}
// ====================================================================
void PlantP::doInit (PlantComponent *systemInterface
                   , const string   &crop_type
                   , vector<string> &part_names)
{
      g.crop_type = crop_type;
      PlantP_set_phosphorus_aware (systemInterface);
      if (g.phosphorus_aware == true)
      {
         PlantP_Set_Up_Parts (part_names);
         PlantP_read_param ();
         read_constants (systemInterface);

         string keyword = "uptake_p_" + crop_type;
//    ids.layered_p_uptake = systemInterface->addRegistration(RegistrationType::get,
//                                    "layered_p_uptake", floatType,
//                                    "", "");
         ids.layered_p_uptake = systemInterface->addRegistration(RegistrationType::get,
                                                               keyword.c_str(), floatArrayType,
                                                               "", "");
      }
}

// ====================================================================
void PlantP::PlantP_Set_Up_Parts (vector<string> &part_names)
{

//+  Purpose
//      Initialise PlantP parts arrays

//+  Changes
//     <insert here>

//+  Constant Values
      const char*  my_name = "PlantP_set_up_parts";

//+  Local Variables

//- Implementation Section ----------------------------------
      push_routine (my_name);

      g.num_parts = part_names.size();
      g.part_names = part_names;
      // Put further setup stuff here.....
      pop_routine (my_name);
}

// ====================================================================
void PlantP::PlantP_set_phosphorus_aware (PlantComponent *systemInterface)
{
//+  Purpose
//      Check that soil phosphorus is in system                              

//+  Mission statement
//     Check the phosphorus awareness of the system

//+  Changes

//+  Constant Values
const char*  my_name = "PlantP_set_phosphorus_aware";

//+  Local Variables
      vector<float> values;               // Scratch area
      bool soilpPresent;

//- Implementation Section ----------------------------------

      push_routine (my_name);

      unsigned int idSoilpVar = systemInterface->addRegistration(RegistrationType::get,
                                                               "labile_p", floatArrayType,
                                                               "", "");
      systemInterface->getVariable(idSoilpVar, values, 0.0, 1000000.0, true);
      soilpPresent = (values.size() > 0);

      if(soilpPresent == true)
      {
           //module is p aware
         g.phosphorus_aware = true;
         plant->parent->writeString ("   - Module is set phosphorus aware");
         plant->parent->writeString (" ");
      }
      else
      {
         g.phosphorus_aware = false;
      }

   pop_routine (my_name);
}



// ====================================================================
void PlantP::PlantP_demand (float growth_stage
                           , float parts_wt[]
                           , float dlt_dm_pot)
{

//+  Purpose
//      Calculate plant P demands

//+  Changes
//     <insert here>

//+  Constant Values
      const char*  my_name = "PlantP_demand";

//+  Local Variables
      float    deficit;
      float    p_conc_max;
      float    rel_growth_rate;

//- Implementation Section ----------------------------------
      push_routine (my_name);

      fill_real_array (g.part_demand, 0.0, max_part);
      g.growth_stage = growth_stage;

      rel_growth_rate = divide(dlt_dm_pot
                              , sum_real_array(parts_wt, g.num_parts)
                              , 0.0);

      for (int part = 0; part < g.num_parts; part++)
         {
         if (binary_search(c.yield_parts.begin(),
                           c.yield_parts.end(),
                           g.part_names[part]))
            {
            // A yield part - does not contribute to soil demand
            g.part_demand[part] = 0.0;
            }
         else
            {
            // Not a yield part - therefore it contributes to demand
            p_conc_max = linear_interp_real (g.growth_stage
                                           , c.x_p_stage_code
                                           , c.y_p_conc_max[part]
                                           , c.num_x_p_stage_code);

            // scale up to include potential new growth
            // assuming partitioning today similar to current
            // plant form - a rough approximation

            deficit = p_conc_max * parts_wt[part] * (1.0 + rel_growth_rate)
                    - g.part_p_green[part];

            g.part_demand[part] = l_bound(deficit, 0.0);
            }
         }
      pop_routine (my_name);
}


// ====================================================================
float PlantP::fact_photo () const
{
//+  Purpose
//      Provide value of P factor for photosynthesis
      if (g.phosphorus_aware == true)
         return g.pfact_photo;
      else
         return 1.0;
}


// ====================================================================
float PlantP::fact_grain () const
{
//+  Purpose
//      Provide value of P factor for grain filling

      if (g.phosphorus_aware == true)
         return g.pfact_grain;
      else
         return 1.0;
}

// ====================================================================
float PlantP::fact_expansion () const
{
//+  Purpose
//      Provide value of P factor for cell expansion
      if (g.phosphorus_aware == true)
         return g.pfact_expansion;
      else
         return 1.0;
}

// ====================================================================
float PlantP::fact_pheno ()  const
{
//+  Purpose
//      Provide value of P factor for phenology
      if (g.phosphorus_aware == true)
         return g.pfact_pheno;
      else
         return 1.0;
}


//// ====================================================================
//void PlantP::add_residue (float chop_fr_green[]
//                        , float chop_fr_sen[]
//                        , float chop_fr_dead[]
//                        , float fraction_to_residue[])
//{
////+  Purpose
////      Send P to soil or residues when plants are 'chopped'
//
////+  Changes
////     <insert here>
//
////+  Constant Values
//      const char*  my_name = "PlantP_add_residue";
//
////+  Local Variables
//      float    dlt_residue_p;
//
////- Implementation Section ----------------------------------
//      push_routine (my_name);
//
//      dlt_residue_p = 0.0;
//      for (int part = 0; part < g.num_parts; part++)
//      {
//         dlt_residue_p += (chop_fr_green[part] * g.part_p_green[part] * fraction_to_residue[part]
//                           + chop_fr_sen[part] * g.part_p_sen[part]   * fraction_to_residue[part]
//                           + chop_fr_dead[part] * g.part_p_dead[part]  * fraction_to_residue[part])
//                         * gm2kg/sm2ha;
//
//         g.part_p_green[part] = g.part_p_green[part] * (1.0 - chop_fr_green[part]);
//         g.part_p_sen[part]   = g.part_p_sen[part]   * (1.0 - chop_fr_sen[part]);
//         g.part_p_dead[part]  = g.part_p_dead[part]  * (1.0 - chop_fr_dead[part]);
//      }
//
//      protocol::ApsimVariant outgoingApsimVariant(plant->parent);
//      outgoingApsimVariant.store("dlt_residue_p", protocol::DTsingle, false,
//                                 dlt_residue_p);
//
//      plant->parent->publish (ids.add_residue_p, outgoingApsimVariant);
//
//      pop_routine (my_name);
//}
// ====================================================================
void PlantP::crop_chopped (float chop_fr_green[]        //IN
                            , float chop_fr_sen[]          //IN
                            , float chop_fr_dead[]         //IN
                            , float *dlt_residue_p         //OUT
                            , float dlt_dm_p[]             //OUT
                              )
{

//+  Purpose
//      Calculate P to soil or residues when plants are 'chopped'

//+  Changes
//     <insert here>

//+  Calls

//+  Constant Values
      const char*  my_name = "residue_chopped";

//+  Local Variables

//- Implementation Section ----------------------------------
      push_routine (my_name);

      if (g.phosphorus_aware == true)
      {
         *dlt_residue_p = 0.0;
         for (int part = 0; part < g.num_parts; part++)
         {
            dlt_dm_p[part]  = (chop_fr_green[part] * g.part_p_green[part]
                              + chop_fr_sen[part]  * g.part_p_sen[part]
                              + chop_fr_dead[part] * g.part_p_dead[part])
                            * gm2kg/sm2ha;

            *dlt_residue_p += dlt_dm_p[part];

            g.part_p_green[part] = g.part_p_green[part] * (1.0 - chop_fr_green[part]);
            g.part_p_sen[part]   = g.part_p_sen[part]   * (1.0 - chop_fr_sen[part]);
            g.part_p_dead[part]  = g.part_p_dead[part]  * (1.0 - chop_fr_dead[part]);
         }
      }
      else
      {
         for (int part = 0; part <max_part; part++)
         {
            dlt_dm_p[part] = 0.0;
            *dlt_residue_p = 0.0;
         }
      }

      pop_routine (my_name);
}

// ====================================================================
void PlantP::end_crop (float chop_fr[]        //IN
                     , float *dlt_residue_p         //OUT
                     , float dlt_dm_p[]             //OUT
                      )
{

//+  Purpose
//      Calculate P to soil or residues when plants are 'chopped'

//+  Changes
//     <insert here>

//+  Calls

//+  Constant Values
      const char*  my_name = "end_crop";

//+  Local Variables

//- Implementation Section ----------------------------------
      push_routine (my_name);

      if (g.phosphorus_aware == true)
      {
         *dlt_residue_p = 0.0;
         for (int part = 0; part < g.num_parts; part++)
         {
            dlt_dm_p[part]  = (g.part_p_green[part] + g.part_p_sen[part] + g.part_p_dead[part])
                            * chop_fr[part] * gm2kg/sm2ha;

            *dlt_residue_p += dlt_dm_p[part];

            g.part_p_green[part] = 0.0;
            g.part_p_sen[part]   = 0.0;
            g.part_p_dead[part]  = 0.0;
         }
      }
      else
      {
         for (int part = 0; part < max_part; part++)
         {
            dlt_dm_p[part] = 0.0;
            *dlt_residue_p = 0.0;
         }
      }

      pop_routine (my_name);
}

// ====================================================================
void PlantP::detached (float chop_fr[]
                     , float *dlt_residue_p         //OUT
                     , float dlt_dm_p[]             //OUT
                      )
{

//+  Purpose
//      Calculate P to soil or residues when plants are 'chopped'

//+  Changes
//     <insert here>

//+  Calls

//+  Constant Values
      const char*  my_name = "PlantP_detached";

//+  Local Variables

//- Implementation Section ----------------------------------
      push_routine (my_name);

      if (g.phosphorus_aware == true)
      {
         *dlt_residue_p = 0.0;
         for (int part = 0; part < g.num_parts; part++)
         {
            dlt_dm_p[part]  = g.dlt_part_p_det[part] * chop_fr[part]
                            * gm2kg/sm2ha;

            *dlt_residue_p += dlt_dm_p[part];
         }
      }
      else
      {
         for (int part = 0; part < max_part; part++)
         {
            dlt_dm_p[part] = 0.0;
            *dlt_residue_p = 0.0;
         }
      }

      pop_routine (my_name);
}

// ====================================================================
void PlantP::dead_detached (float chop_fr[]
                          , float *dlt_residue_p         //OUT
                          , float dlt_dm_p[]             //OUT
                           )
{

//+  Purpose
//      Calculate P to soil or residues when plants are 'chopped'

//+  Changes
//     <insert here>

//+  Calls

//+  Constant Values
      const char*  my_name = "PlantP_dead_detached";

//+  Local Variables

//- Implementation Section ----------------------------------
      push_routine (my_name);

      if (g.phosphorus_aware == true)
      {
         *dlt_residue_p = 0.0;
         for (int part = 0; part < g.num_parts; part++)
         {
            dlt_dm_p[part]  = g.dlt_part_p_dead_det[part] * chop_fr[part]
                            * gm2kg/sm2ha;

            *dlt_residue_p += dlt_dm_p[part];
         }
      }
      else
      {
         for (int part = 0; part < max_part; part++)
         {
            dlt_dm_p[part] = 0.0;
            *dlt_residue_p = 0.0;
         }
      }

      pop_routine (my_name);
}

// ====================================================================
float PlantP::incorp_fom (float incorp_fr_green[]      //IN
                       , float incorp_fr_sen[]        //IN
                       , float dlayer[]               //IN
                       , float root_length[]          //IN
                       , float root_depth)             //IN
{
//+  Purpose
//      Send P to soil or residues when plants are 'chopped'

//+  Changes
//     <insert here>

//+  Constant Values
      const char*  my_name = "PlantP_incorp_fom";

//+  Local Variables
      float    dlt_p_tot = 0.0;
      float    dlt_fom_p[max_layer];
      int      num_layers;

//- Implementation Section ----------------------------------
      push_routine (my_name);

   if (g.phosphorus_aware == true)
   {
      for (int part = 0; part < g.num_parts; part++)
      {
         dlt_p_tot += incorp_fr_green[part] * g.part_p_green[part]
                    + incorp_fr_sen[part]   * g.part_p_sen[part]
                    + incorp_fr_sen[part]   * g.dlt_part_p_det[part];

         g.part_p_green[part] = g.part_p_green[part] * (1.0 - incorp_fr_green[part]);
         g.part_p_sen[part]   = g.part_p_sen[part]   * (1.0 - incorp_fr_sen[part]);
      }

      if (dlt_p_tot > 0.0)
      {
         crop_root_dist( dlayer
                       , root_length
                       , root_depth
                       , dlt_fom_p
                       , dlt_p_tot * gm2kg/sm2ha
                       , max_layer);

         num_layers = 1 + count_of_real_vals(root_length, max_layer);
         if (num_layers > 0)
         {
            protocol::ApsimVariant outgoingApsimVariant(plant->parent);
            outgoingApsimVariant.store("dlt_fom_p", protocol::DTsingle, true,
                                       protocol::vector<float>(dlt_fom_p, dlt_fom_p+num_layers));
            plant->parent->publish (ids.incorp_fom_p, outgoingApsimVariant);
         }
      }
      else
      {} // nothing to incorporate
   }
   else
     {
     // not P aware, state variables (green & sen) unchanged.
     }
   pop_routine (my_name);
   return(dlt_p_tot);
}

// ====================================================================
float PlantP::incorp_fom_dead (float incorp_fr_dead[]       //IN
                             , float dlayer[]               //IN
                             , float root_length_dead[]     //IN
                             , float root_depth)            // IN
{
//+  Purpose
//      Send P to soil or residues when plants are 'chopped'

//+  Changes
//     <insert here>

//+  Constant Values
      const char*  my_name = "PlantP_incorp_fom_dead";

//+  Local Variables
      float    dlt_p_tot = 0.0;
      float    dlt_fom_p[max_layer];
      int      num_layers;

//- Implementation Section ----------------------------------
      push_routine (my_name);

   if (g.phosphorus_aware == true)
   {
      for (int part = 0; part < g.num_parts; part++)
      {
         dlt_p_tot += incorp_fr_dead[part]  * g.part_p_dead[part]
                    + incorp_fr_dead[part]  * g.dlt_part_p_dead_det[part];

         g.part_p_dead[part]  = g.part_p_dead[part]  * (1.0 - incorp_fr_dead[part]);
      }

      if (dlt_p_tot > 0.0)
      {
         crop_root_dist( dlayer
                       , root_length_dead
                       , root_depth
                       , dlt_fom_p
                       , dlt_p_tot * gm2kg/sm2ha
                       , max_layer);

         num_layers = 1 + count_of_real_vals(root_length_dead, max_layer);
         if (num_layers > 0)
         {
            protocol::ApsimVariant outgoingApsimVariant(plant->parent);
            outgoingApsimVariant.store("dlt_fom_p", protocol::DTsingle, true,
                                       protocol::vector<float>(dlt_fom_p, dlt_fom_p+num_layers));
            plant->parent->publish (ids.incorp_fom_p, outgoingApsimVariant);
         }
      }
      else
      {} // nothing to incorporate
   }
   else
   {} // not P aware
   pop_routine (my_name);
   return(dlt_p_tot);
}

// ====================================================================
float PlantP::incorp_fom_detached (float incorp_fr[]      //IN
                                , float dlayer[]               //IN
                                , float root_length[]          //IN
                                , float root_depth)             //IN
{
//+  Purpose
//      Send P to soil or residues when plants are 'chopped'

//+  Changes
//     <insert here>

//+  Constant Values
      const char*  my_name = "PlantP_incorp_fom_detached";

//+  Local Variables
      float    dlt_p_tot = 0.0;
      float    dlt_fom_p[max_layer];
      int      num_layers;

//- Implementation Section ----------------------------------
      push_routine (my_name);

   if (g.phosphorus_aware == true)
   {
      for (int part = 0; part < g.num_parts; part++)
      {
         dlt_p_tot += g.dlt_part_p_det[part] * incorp_fr[part];
      }
      if (dlt_p_tot > 0.0)
      {
          crop_root_dist( dlayer
                        , root_length
                        , root_depth
                        , dlt_fom_p
                        , dlt_p_tot * gm2kg/sm2ha
                        , max_layer);

          num_layers = 1 + count_of_real_vals(root_length, max_layer);
          if (num_layers > 0)
          {
             protocol::ApsimVariant outgoingApsimVariant(plant->parent);
             outgoingApsimVariant.store("dlt_fom_p", protocol::DTsingle, true,
                                        protocol::vector<float>(dlt_fom_p, dlt_fom_p+num_layers));
             plant->parent->publish (ids.incorp_fom_p, outgoingApsimVariant);
          }
      }
   }
   else
   {}
   pop_routine (my_name);
   return(dlt_p_tot);
}

// ====================================================================
float PlantP::incorp_fom_dead_detached (float incorp_fr[]      //IN
                                      , float dlayer[]               //IN
                                      , float root_length[]          //IN
                                      , float root_depth)             //IN
{
//+  Purpose
//      Send P to soil or residues when plants are 'chopped'

//+  Changes
//     <insert here>

//+  Constant Values
      const char*  my_name = "incorp_fom_dead_detached";

//+  Local Variables
      float    dlt_p_tot = 0.0;
      float    dlt_fom_p[max_layer];
      int      num_layers;

//- Implementation Section ----------------------------------
      push_routine (my_name);

   if (g.phosphorus_aware == true)
   {
      for (int part = 0; part < g.num_parts; part++)
      {
         dlt_p_tot += g.dlt_part_p_dead_det[part] * incorp_fr[part];
      }
      if (dlt_p_tot > 0.0)
      {
         crop_root_dist( dlayer
                       , root_length
                       , root_depth
                       , dlt_fom_p
                       , dlt_p_tot * gm2kg/sm2ha
                       , max_layer);

         num_layers = 1 + count_of_real_vals(root_length, max_layer);
         if (num_layers > 0)
         {
            protocol::ApsimVariant outgoingApsimVariant(plant->parent);
            outgoingApsimVariant.store("dlt_fom_p", protocol::DTsingle, true,
                                       protocol::vector<float>(dlt_fom_p, dlt_fom_p+num_layers));
            plant->parent->publish (ids.incorp_fom_p, outgoingApsimVariant);
         }
      }
   }
   else
   {}
   pop_routine (my_name);
   return(dlt_p_tot);
}

// ====================================================================
float PlantP::PlantP_Pfact (float growth_stage
                           , float dm_green[])
{

//+  Purpose
//      Provide value of generic P factor

//+  Changes
//     <insert here>

//+  Constant Values
      const char*  my_name = "PlantP_Pfact";

//+  Local Variables
      float    p_conc_max;
      float    p_conc_min;
      float    max_p;
      float    min_p;
      float    act_p;
      float    max_p_conc;
      float    min_p_conc;
      float    act_p_conc;
      float    determinants_wt;
      float    pfact;

//- Implementation Section ----------------------------------
      push_routine (my_name);

   if (g.phosphorus_aware == true)
   {
      act_p = 0.0;
      min_p = 0.0;
      max_p = 0.0;
      determinants_wt = 0.0;

      for (unsigned int determinant = 0; determinant < c.stress_determinants.size(); determinant++)
         {
         vector<string>::iterator pos = find(g.part_names.begin(),
                                          g.part_names.end(),
                                          c.stress_determinants[determinant]);
         if (pos != g.part_names.end())
            {
            int part = pos - g.part_names.begin();
            act_p = act_p + g.part_p_green[part];

            p_conc_max = linear_interp_real (growth_stage
                                           , c.x_p_stage_code
                                           , c.y_p_conc_max[part]      // (part,1:c.num_x_p_stage_code)
                                           , c.num_x_p_stage_code);
            max_p = max_p + p_conc_max * dm_green[part];

            p_conc_min = linear_interp_real (growth_stage
                                           , c.x_p_stage_code
                                           , c.y_p_conc_min[part]      // (part,1:c.num_x_p_stage_code)
                                           , c.num_x_p_stage_code);
            min_p = min_p + p_conc_min * dm_green[part];
            determinants_wt = determinants_wt + dm_green[part];
            }
         }

      act_p_conc = divide(act_p, determinants_wt, 0.0);
      max_p_conc = divide(max_p, determinants_wt, 0.0);
      min_p_conc = divide(min_p, determinants_wt, 0.0);

      if ((determinants_wt <= 0.0) || (act_p <= 0.0))
      {
         // appears that things are not yet initialised
         pfact = 1.0;
      }
      else
      {
         pfact = divide(act_p_conc - min_p_conc
                       , max_p_conc - min_p_conc
                       , 1.0);
      }

      pfact = bound(pfact, 0.0, 1.0);
   }
   else
   {
      pfact = 1.0;
   }

      pop_routine (my_name);
      return pfact;
}














// ====================================================================
void PlantP::PlantP_Stress (float growth_stage
                           , float dm_green[])
{

//+  Purpose
//      Provide value of  P stress factors

//+  Changes
//     <insert here>

//+  Constant Values
      const char*  my_name = "PlantP_Stress";

//+  Local Variables
      float    pfact;

//- Implementation Section ----------------------------------
      push_routine (my_name);

      pfact = PlantP_Pfact(growth_stage,dm_green);

      g.pfact_photo = pfact * c.pfact_photo_slope;
      g.pfact_photo = bound(g.pfact_photo, 0.0, 1.0);

      g.pfact_expansion = pfact * c.pfact_expansion_slope;
      g.pfact_expansion = bound(g.pfact_expansion, 0.0, 1.0);

      g.pfact_pheno = pfact * c.pfact_pheno_slope;
      g.pfact_pheno = bound(g.pfact_pheno, 0.0, 1.0);

      g.pfact_grain = pfact * c.pfact_grain_slope;
      g.pfact_grain = bound(g.pfact_grain, 0.0, 1.0);

      pop_routine (my_name);
}
// ====================================================================
void PlantP::PlantP_init_pools (float dm_green[])
{

//+  Purpose
//      Initialise Plant P Pools

//+  Changes
//     <insert here>

//+  Constant Values
      const char*  my_name = "PlantP_init_pools";

//+  Local Variables


//- Implementation Section ----------------------------------
      push_routine (my_name);

      if (sum_real_array(dm_green, g.num_parts) > 0.0
            &&
          sum_real_array(g.part_p_green, g.num_parts) <= 0.0)
         {
         // biomass has been initialised but the p pools have not
         for (int part = 0; part < g.num_parts; part++)
            {
            g.part_p_green[part] = dm_green[part] * c.p_conc_init[part];
            }
         }
      pop_routine (my_name);
}
// ====================================================================
void PlantP::PlantP_retrans (float growth_stage
                           , float dm_green[])
{

//+  Purpose
//      Calculate retranslocation between pools

//+  Changes
//     <insert here>

//+  Constant Values
      const char*  my_name = "PlantP_retrans";

//+  Local Variables
      float    supply[max_part];
      float    demand[max_part];

      int      part;
      float    p_conc_min;
      float    min_p;
      float    p_conc_max;
      float    max_p;
      float    fraction;

//- Implementation Section ----------------------------------
      push_routine (my_name);

      fill_real_array (g.dlt_part_p_retrans, 0.0, max_part);
      fill_real_array (supply, 0.0, max_part);
      fill_real_array (demand, 0.0, max_part);

      for (vector<string>::iterator spart = c.retrans_parts.begin();
           spart != c.retrans_parts.end();
           spart++)
         {
         vector<string>::iterator pos = find(g.part_names.begin(),
                                              g.part_names.end(),
                                              *spart);
         if (pos != g.part_names.end())
            {
            int part = pos - g.part_names.begin();

            p_conc_min = linear_interp_real (growth_stage
                                        , c.x_p_stage_code
                                        , c.y_p_conc_min[part]
                                        , c.num_x_p_stage_code);
            min_p = p_conc_min * dm_green[part];
            supply[part] = max(g.part_p_green[part] - min_p, 0.0);
            }
         }

      for (vector<string>::iterator ypart = c.yield_parts.begin();
           ypart != c.yield_parts.end();
           ypart++)
         {
         vector<string>::iterator pos = find(g.part_names.begin(),
                                              g.part_names.end(),
                                              *ypart);
         if (pos != g.part_names.end())
            {
            int part = pos - g.part_names.begin();

            p_conc_max = linear_interp_real (growth_stage
                                        , c.x_p_stage_code
                                        , c.y_p_conc_max[part]     // (part,1:c.num_x_p_stage_code)
                                        , c.num_x_p_stage_code);
            max_p = p_conc_max * dm_green[part];
            demand[part] = max(max_p - g.part_p_green[part], 0.0);
            }
      }

      for (int part = 0; part < g.num_parts; part++)
         {
         if (supply[part] > 0.0)
            {
            fraction = divide(sum_real_array(demand, max_part)
                            , sum_real_array(supply, max_part)
                            , 0.0);
            fraction = bound(fraction, 0.0, 1.0);
            g.dlt_part_p_retrans[part] = - supply[part] * fraction;
            }
         else if (demand[part] > 0.0)
            {
            fraction = divide(sum_real_array(supply, max_part)
                            , sum_real_array(demand, max_part)
                            , 0.0);
            fraction = bound(fraction, 0.0, 1.0);
            g.dlt_part_p_retrans[part] = demand[part] * fraction;
            }
         else
            {
            // this part is not involved
            }
         }
      pop_routine (my_name);
}

// ====================================================================
void PlantP::summary (float dm_green[], float dm_dead[])
{
//+  Purpose
//      Summary

//+  Changes
//     <insert here>


//+  Constant Values
    const char*  my_name = "PlantP_summary" ;

//+  Local Variables
    char  msg[400];
      float       P_grain;               // total grain P uptake (kg/ha)
      float       P_dead;                // above ground dead plant P (kg/ha)
      float       P_green;               // above ground green plant P (kg/ha)
      float       P_senesced;            // above ground senesced plant P (kg/ha)
      float       P_stover;              // nitrogen content of stover (kg\ha)
      float       P_total;               // total gross nitrogen content (kg/ha)
      float       P_grain_conc_percent;  // grain nitrogen .

//- Implementation Section ----------------------------------          g.part_p_green(1:g.num_parts)
      push_routine (my_name);

   if (g.phosphorus_aware == true)
   {
       P_grain_conc_percent = divide (
           g.part_p_green[meal] + g.part_p_dead[meal] + g.part_p_green[oil] + g.part_p_dead[oil]
         , dm_green[meal] + dm_dead[meal] + dm_green[oil] + dm_dead[oil]
         , 0.0) * fract2pcnt;

       P_grain = (g.part_p_green[meal] + g.part_p_dead[meal]
               +  g.part_p_green[oil]  + g.part_p_dead[oil]) * gm2kg/sm2ha;

       P_green = (sum_real_array (g.part_p_green, max_part)
               -  g.part_p_green[root] - g.part_p_green[meal] - g.part_p_green[oil]) * gm2kg / sm2ha;

       P_senesced = (sum_real_array (g.part_p_sen, max_part)
                  -  g.part_p_sen[root] - g.part_p_sen[meal]- g.part_p_sen[oil]) * gm2kg / sm2ha;

       P_dead = (sum_real_array (g.part_p_dead, max_part)
           - g.part_p_dead[root] - g.part_p_dead[meal] - g.part_p_dead[oil]) * gm2kg / sm2ha;

        P_stover = P_green + P_senesced + P_dead;
        P_total = P_grain + P_stover;



       sprintf (msg, "%s%10.2f%20s%s%10.2f"
                , " grain P percent        = ", P_grain_conc_percent, " "
                , " total P content (kg/ha)= ", P_total);
       plant->parent->writeString (msg);

       sprintf (msg, "%s%10.2f%20s%s%8.2f"
                , " grain P uptake (kg/ha) = ", P_grain, " "
                , " senesced P content (kg/ha)=", P_senesced);
       plant->parent->writeString (msg);

       sprintf (msg, "%s%10.2f%20s%s%10.2f"
                , " green P content (kg/ha)= ", P_green, " "
                , " dead P content (kg/ha) = ", P_dead);
       plant->parent->writeString (msg);
   }
   pop_routine (my_name);
}




////
//// PlantP class test harness
////
//// Tests default constructor, copy constructor, assignment operator and
//// each of the get and set functions.  Does not test the destructor.
////
//// Modification log
//// 27/7/97 J Hargreaves    Initial implementation
////
//
//int main()
//{
//	cout << "PlantP test started" << endl;
//
//	PlantP p;
//
//	cout << endl << "Test set and get functions:" << endl;
//	string name = "Bill";
//	p.setName(name);
//	if (p.getName() == name)
//		cout << "setName(name) / getName() test OK" << endl;
//	else
//		cout << "setName(name) / getName() test FAILED" << endl;
//	string phone = "1234-5678";
//	p.setPhone(phone);
//	if (p.getPhone() == phone)
//		cout << "setPhone(phone) / getPhone() test OK" << endl;
//	else
//		cout << "setPhone(phone) / getPhone() test FAILED" << endl;
//
//	cout << endl << "Test default constructor:" << endl;
//	PlantP q;                           // run default constructor
//	if (q.getName() == "" && q.getPhone() == "")
//		cout << "default constructor test OK" << endl;
//	else
//		cout << "default constructor test FAILED" << endl;
//
//	cout << endl << "Test constructor parameters:" << endl;
//	PlantP r(name, phone);              // run constructor with parameters
//	if (r.getName() == name && r.getPhone() == phone)
//		cout << "constructor parameters test OK" << endl;
//	else
//		cout << "constructor parameters test FAILED" << endl;
//
//	cout << endl << "Test copy constructor:" << endl;
//	PlantP s = p;                       // run copy constructor
//	if (s.getName() == p.getName() && s.getPhone() == p.getPhone())
//		cout << "copy constructor test OK" << endl;
//	else
//		cout << "copy constructor test FAILED" << endl;
//
//	cout << endl << "Test assignment operator:" << endl;
//	s.setName("Another name"); s.setPhone("Another number"); // change object
//	if (s.getName() != p.getName() && s.getPhone() != p.getPhone())
//	{
//		s = p;                          // run operator=
//		if (s.getName() == p.getName() && s.getPhone() == p.getPhone())
//			cout << "assignment operator test OK" << endl;
//		else
//			cout << "assignment operator test FAILED" << endl;
//	}
//	else
//		cout << "assignment operator test FAILED DIFFERENCE TEST" << endl;
//
//	cout << endl << "PlantP test finished" << endl;
//	return 0;
//}



