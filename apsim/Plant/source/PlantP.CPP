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

#include <general/pch.h>
#include <vcl.h>
#include <stdio.h>
#include <math.h>
#include <map>
#include <string>
#include <algorithm>
#include <cstring.h>
#include <iostream.h>
#include <boost/function.hpp>
#include <boost/bind.hpp>

#include <ComponentInterface/Type.h>
#include <ComponentInterface/ApsimVariant.h>
#include <ComponentInterface/Component.h>
#include <ComponentInterface/dataTypes.h>
#include <ComponentInterface/Messages.h>
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
	throw "Copy constructor NI for plantp";
}


// Assigment operator
//	assign data members of object
const PlantP &PlantP::operator=(const PlantP &other)
{
 	throw "Assignment operator NI for plantp";
}

// destructor
PlantP::~PlantP()
{
}

// Register Methods & Events
void PlantP::doIDs(PlantComponent *systemInterface)
{
    // we want to send these events out
    ids.add_residue_p = systemInterface->addRegistration(protocol::eventReg,
                                    "add_residue_p", "",
                                    "", "");
    ids.incorp_fom_p = systemInterface->addRegistration(protocol::eventReg,
                                    "incorp_fom_p", "",
                                    "", "");
    // we will want this variable
    // ids.layered_p_uptake = systemInterface->addRegistration(protocol::getVariableReg,
    //                                "layered_p_uptake", floatType,
    //                                "", "");
}

// Register Gets & Sets
void PlantP::doRegistrations(PlantComponent *systemInterface)
   {
   // Function pointer
   boost::function2<void, protocol::Component *, protocol::QueryValueData &> myFn;

   systemInterface->addGettableVar("p_green", protocol::DTsingle, max_part,
                                   &g.part_p_green,
                                   "g/m^2", "P content of plant parts");

   systemInterface->addGettableVar("p_sen", protocol::DTsingle, max_part,
                                   &g.part_p_sen,
                                   "g/m^2", "P content of plant parts");

   myFn = boost::bind(&PlantP::get_p_demand, this, _1, _2);
   systemInterface->addGettableVar("p_demand", protocol::DTsingle, 1,
                                   &myFn,
                                   "kg/ha","");

   systemInterface->addGettableVar("pfact_photo", protocol::DTsingle, 1,
                                   &g.pfact_photo,
                                   "", "P factor in photosynthesis");

   systemInterface->addGettableVar("pfact_pheno", protocol::DTsingle, 1,
                                   &g.pfact_pheno,
                                   "", "P factor in phenology");

   systemInterface->addGettableVar("pfact_expansion", protocol::DTsingle, 1,
                                   &g.pfact_expansion,
                                   "", "P factor in leaf expansion");

   systemInterface->addGettableVar("pfact_grain", protocol::DTsingle, 1,
                                   &g.pfact_grain,
                                   "", "P factor in grain");

   myFn = boost::bind(&PlantP::get_p_demand, this, _1, _2);
   systemInterface->addGettableVar("biomass_p", protocol::DTsingle, 1,
                                   myFn,
                                   "g/m^2","P in biomass");

   myFn = boost::bind(&PlantP::get_biomass_p, this, _1, _2);
   systemInterface->addGettableVar("p_uptake", protocol::DTsingle, 1,
                                   myFn,
                                   "g/m^2","P  uptake");

   myFn = boost::bind(&PlantP::get_green_biomass_p, this, _1, _2);
   systemInterface->addGettableVar("green_biomass_p", protocol::DTsingle, 1,
                                   myFn,
                                   "g/m^2","P in green biomass");

   myFn = boost::bind(&PlantP::get_grain_p, this, _1, _2);
   systemInterface->addGettableVar("grain_p", protocol::DTsingle, 1,
                                   myFn,
                                   "g/m^2","P in grain");

   systemInterface->addGettableVar("leaf_p", protocol::DTsingle, 1,
                                   &g.part_p_green[leaf],
                                   "g/m^2","P in leaf");

   systemInterface->addGettableVar("stem_p", protocol::DTsingle, 1,
                                   &g.part_p_green[stem],
                                   "g/m^2","P in stem");

   systemInterface->addGettableVar("root_p", protocol::DTsingle, 1,
                                   &g.part_p_green[root],
                                   "g/m^2","P in roots");

   systemInterface->addGettableVar("deadleaf_p", protocol::DTsingle, 1,
                                   &g.part_p_sen[leaf],
                                   "g/m^2","P in dead leaf");

   myFn = boost::bind(&PlantP::get_head_p, this, _1, _2);
   systemInterface->addGettableVar("head_p", protocol::DTsingle, 1,
                                   myFn,
                                   "g/m^2","P in head");

   systemInterface->addGettableVar("p_senesced", protocol::DTsingle, max_part,
                                   &g.part_p_sen,
                                   "g/m^2","P in senesced parts");

   systemInterface->addGettableVar("p_dead", protocol::DTsingle, max_part,
                                   &g.part_p_dead,
                                   "g/m^2","P in dead parts");

   systemInterface->addGettableVar("dlt_p_green", protocol::DTsingle, max_part,
                                   &g.dlt_part_p_green,
                                   "g/m^2","dlt P parts");

   systemInterface->addGettableVar("dlt_p_retrans", protocol::DTsingle,max_part,
                                   &g.dlt_part_p_retrans,
                                   "g/m^2","dlt P parts");
// john: this was missing?
//   systemInterface->addGettableVar("dlt_p_detached", protocol::DTsingle, max_part,
//                                   &g.dlt_part_p_detached,
//                                   "g/m^2","dlt P detached");

   systemInterface->addGettableVar("dlt_p_dead", protocol::DTsingle, max_part,
                                   &g.dlt_part_p_dead,
                                   "g/m^2","dlt P in dead");

   systemInterface->addGettableVar("dlt_p_sen", protocol::DTsingle, max_part,
                                   &g.dlt_part_p_sen,
                                   "g/m^2","dlt P in senesced");

   myFn = boost::bind(&PlantP::get_p_conc_stover, this, _1, _2);
   systemInterface->addGettableVar("p_conc_stover", protocol::DTsingle, 1,
                                   myFn,
                                   "%","P in stover");

   myFn = boost::bind(&PlantP::get_p_conc_leaf, this, _1, _2);
   systemInterface->addGettableVar("p_conc_leaf", protocol::DTsingle, 1,
                                   myFn,
                                   "%","P in leaf");

   myFn = boost::bind(&PlantP::get_p_conc_stem, this, _1, _2);
   systemInterface->addGettableVar("p_conc_stem", protocol::DTsingle, 1,
                                   myFn,
                                   "%","P in stem");

   myFn = boost::bind(&PlantP::get_p_conc_grain, this, _1, _2);
   systemInterface->addGettableVar("p_conc_grain", protocol::DTsingle, 1,
                                   myFn,
                                   "%","P in grain");

   myFn = boost::bind(&PlantP::get_p_conc_grain, this, _1, _2);
   systemInterface->addGettableVar("p_grain_pcnt", protocol::DTsingle, 1,
                                   myFn,
                                   "%","P in grain");

   myFn = boost::bind(&PlantP::get_p_uptake_stover, this, _1, _2);
   systemInterface->addGettableVar("p_uptake_stover", protocol::DTsingle, 1,
                                   myFn,
                                   "%","P in stover");
// john: this was missing?
//   systemInterface->addGettableVar("grain_p_demand", protocol::DTsingle, 1,
//                                   &g.part_demand[grain],
//                                   "g/m^2","P demand of grain");
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
    float p_demand = sum_real_array(g.part_demand, max_part) * gm2kg / sm2ha;
    systemInterface->sendVariable(qd, p_demand);   //(kg/ha)
}

void PlantP::get_pfact_grain(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    systemInterface->sendVariable(qd, g.pfact_grain);  //()
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
void PlantP::read_constants (void)
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


//- Implementation Section ----------------------------------

    push_routine (my_name);

    plant->read_array (section_name, "stress_determinants",  "()",    c.stress_determinants);
    plant->read_array (section_name, "yield_parts",          "()",    c.yield_parts);
    plant->read_array (section_name, "retrans_parts",        "()",    c.retrans_parts);
    plant->read_var   (section_name, "pfact_photo_slope",     "()",   c.pfact_photo_slope    , 0.0, 100.0);
    plant->read_var   (section_name, "pfact_expansion_slope", "()",   c.pfact_expansion_slope, 0.0, 100.0);
    plant->read_var   (section_name, "pfact_pheno_slope",     "()",   c.pfact_pheno_slope    , 0.0, 100.0);
    plant->read_var   (section_name, "pfact_grain_slope",    "()",    c.pfact_grain_slope    , 0.0, 100.0);
    plant->read_array (section_name, "x_p_stage_code",       "(g/g)", c.x_p_stage_code, c.num_x_p_stage_code, 0.0, 100.0);

    for (part = 0; part < g.num_parts; part++)
    {
       string keyword ="y_p_conc_max_" + g.part_names[part];
       plant->read_array (section_name, keyword.c_str(), "(g/g)", temp, c.num_x_p_stage_code, 0.0, 1.0);

       for (int i=0; i < c.num_x_p_stage_code; i++)
       {
          c.y_p_conc_max[part][i] = temp[i];
       }

       keyword = "y_p_conc_sen_" + g.part_names[part];
       plant->read_array (section_name, keyword.c_str(), "(g/g)", temp, c.num_x_p_stage_code, 0.0, 1.0);

       for (int i=0; i < c.num_x_p_stage_code; i++)
       {
          c.y_p_conc_sen[part][i] = temp[i];
       }

       keyword = "y_p_conc_min_" + g.part_names[part];
       plant->read_array (section_name, keyword.c_str(), "(g/g)", temp, c.num_x_p_stage_code, 0.0, 1.0);

       for (int i=0; i < c.num_x_p_stage_code; i++)
       {
          c.y_p_conc_min[part][i] = temp[i];
       }

       keyword = "p_conc_init_" + g.part_names[part];
       plant->read_var (section_name, keyword.c_str(), "(g/g)", c.p_conc_init[part], 0.0, 1.0);
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
         zero_daily_variables();

         PlantP_demand(growth_stage, part_wts, dlt_dm_pot);
         PlantP_Stress(growth_stage, part_wts);
      }

      pop_routine (my_name);
}

// ====================================================================
void PlantP::process(float growth_stage
                           , float dm_green[]
                           , float dm_senesced[]
                           , float dlt_dm_senesced[]
                           , float dlt_dm_detached[])
{
//+  Purpose
//     <insert here>

//+  Changes
//

//+  Constant Values
      const char*  my_name = "PlantP_process";

//+  Local Variables
      float layered_p_uptake[max_layer];
      int   numvals;
      int   part;
      float p_uptake;
      float total_demand;


//- Implementation Section ----------------------------------
      push_routine (my_name);

      if (g.phosphorus_aware == true)
      {
         PlantP_init_pools(dm_green);

         PlantP_partition ();
         PlantP_senescence (growth_stage
                          , dm_green
                          , dlt_dm_senesced);
         PlantP_detachment (dm_senesced, dlt_dm_detached);

         for (part = 0; part < max_part; part++)
         {
            g.part_p_green[part] = g.part_p_green[part]
                                 + g.dlt_part_p_green[part]
                                 - g.dlt_part_p_sen[part];

            g.part_p_sen[part] = g.part_p_sen[part]
                               + g.dlt_part_p_sen[part]
                               - g.dlt_part_p_det[part];
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
      protocol::vector<float> values;               // Scratch area
      int   numvals;
      int   part;
      float p_uptake;
      float total_demand;


//- Implementation Section ----------------------------------
      push_routine (my_name);

      if (ids.layered_p_uptake != 0)
      {
          values.empty();
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

      for (part = 0; part < g.num_parts; part++);
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

      for (part = 0; part < g.num_parts; part++);
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

      pop_routine (my_name);
}


// ====================================================================
void PlantP::PlantP_detachment (float dm_senesced[]
                              , float dlt_dm_detached[])
{
//+  Purpose

//+  Changes

//+  Constant Values
      const char*  my_name = "PlantP_detachment";

//+  Local Variables


//- Implementation Section ----------------------------------
      push_routine (my_name);

      fill_real_array (g.dlt_part_p_det, 0.0, max_part);

      for (int part = 0; part < g.num_parts; part++)
      {
         float sen_detach_frac = divide(dlt_dm_detached[part]
                                       , dm_senesced[part]
                                       , 0.0);

         g.dlt_part_p_det[part] = g.part_p_sen[part] * sen_detach_frac;
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
      fill_real_array (g.dlt_part_p_green, 0.0, max_part);
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
         read_constants ();

         string keyword = "uptake_p_" + crop_type;
//    ids.layered_p_uptake = systemInterface->addRegistration(protocol::getVariableReg,
//                                    "layered_p_uptake", floatType,
//                                    "", "");
         ids.layered_p_uptake = systemInterface->addRegistration(protocol::getVariableReg,
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
      protocol::vector<float> values;               // Scratch area
      bool soilpPresent;

//- Implementation Section ----------------------------------

      push_routine (my_name);

      unsigned int idSoilpVar = systemInterface->addRegistration(protocol::getVariableReg,
                                                               "labile_p", floatArrayType,
                                                               "", "");
      values.empty();
      systemInterface->getVariable(idSoilpVar, values, 0.0, 1000000.0, true);
      soilpPresent = (values.size() > 0);

      if(soilpPresent == true)
      {
           //module is p aware
         g.phosphorus_aware = true;
         plant->parent->writeString ("   - Module is set Phosphorous aware");
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
void PlantP::residue_chopped (float chop_fr_green[]
                            , float chop_fr_sen[]
                            , float chop_fr_dead[]
                            , float fraction_to_residue[]
                            , float dlt_residue_p
                            , float dlt_dm_p[]
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
         dlt_residue_p = 0.0;
         for (int part = 0; part < g.num_parts; part++)
         {
            dlt_dm_p[part]  = (chop_fr_green[part] * g.part_p_green[part] * fraction_to_residue[part]
                              + chop_fr_sen[part] * g.part_p_sen[part]   * fraction_to_residue[part]
                              + chop_fr_dead[part] * g.part_p_dead[part]  * fraction_to_residue[part])
                            * gm2kg/sm2ha;

            dlt_residue_p += dlt_dm_p[part];

            g.part_p_green[part] = g.part_p_green[part] * (1.0 - chop_fr_green[part]);
            g.part_p_sen[part]   = g.part_p_sen[part]   * (1.0 - chop_fr_sen[part]);
            g.part_p_dead[part]  = g.part_p_dead[part]  * (1.0 - chop_fr_dead[part]);
         }
      }
      else
      {
         for (int part = 0; part < g.num_parts; part++)
         {
            dlt_dm_p[part] = 0.0;
            dlt_residue_p = 0.0;
         }
      }

      pop_routine (my_name);
}
// ====================================================================
void PlantP::incorp_fom (float incorp_fr_green[]
                       , float incorp_fr_sen[]
                       , float incorp_fr_dead[]
                       , float dlayer[]
                       , float root_length[]
                       , float root_depth
                       , float p_incorporated)
{
//+  Purpose
//      Send P to soil or residues when plants are 'chopped'

//+  Changes
//     <insert here>

//+  Constant Values
      const char*  my_name = "PlantP_incorp_fom";

//+  Local Variables
      float    dlt_fom_p[max_layer];
      int      num_layers;

//- Implementation Section ----------------------------------
      push_routine (my_name);

      p_incorporated = 0.0;
      for (int part = 0; part < g.num_parts; part++)
      {
         p_incorporated +=(incorp_fr_green[part] * g.part_p_green[part]
                           + incorp_fr_sen[part]   * g.part_p_sen[part]
                           + incorp_fr_dead[part]  * g.part_p_dead[part])
                         * gm2kg/sm2ha;

         g.part_p_green[part] = g.part_p_green[part] * (1.0 - incorp_fr_green[part]);
         g.part_p_sen[part]   = g.part_p_sen[part]   * (1.0 - incorp_fr_sen[part]);
         g.part_p_dead[part]  = g.part_p_dead[part]  * (1.0 - incorp_fr_dead[part]);
      }

      crop_root_dist( dlayer
                    , root_length
                    , root_depth
                    , dlt_fom_p
                    , p_incorporated
                    , max_layer);

      num_layers = 1 + count_of_real_vals(root_length, max_layer);
      if (num_layers > 0)
      {
         protocol::ApsimVariant outgoingApsimVariant(plant->parent);
         outgoingApsimVariant.store("dlt_fom_p", protocol::DTsingle, true,
                                    protocol::vector<float>(dlt_fom_p, dlt_fom_p+num_layers));
         plant->parent->publish (ids.incorp_fom_p, outgoingApsimVariant);
      }
      pop_routine (my_name);
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



