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
#include "PlantParts.h"
#include "PlantPhenology.h"


static const char* floatType =        "<type kind=\"single\"/>";
static const char* floatArrayType =   "<type kind=\"single\" array=\"T\"/>";

/////////////These might be redundancies??//////////
void push_routine (const char *) {};
void pop_routine (const char *) {};


// =======================================
void Plant::zero_p_variables ()
{
//+  Purpose
//     Set all variables in this module to zero.

//+  Changes
//     <insert here>

//+  Constant Values
   const char*  my_name = "Plant::zero_p_variables";
//- Implementation Section ----------------------------------
      push_routine (my_name);
      // Parameters
      // ==========

      // Globals
      // =======
      fill_real_array (g.p_green, 0.0, max_part);
      fill_real_array (g.p_sen, 0.0, max_part);
      fill_real_array (g.p_dead, 0.0, max_part);
      fill_real_array (g.dlt_p_green, 0.0, max_part);
      fill_real_array (g.dlt_p_dead, 0.0, max_part);
      fill_real_array (g.dlt_p_sen, 0.0, max_part);
      fill_real_array (g.dlt_p_det, 0.0, max_part);
      fill_real_array (g.dlt_p_dead_det, 0.0, max_part);
      fill_real_array (g.dlt_p_retrans, 0.0, max_part);
      fill_real_array (g.p_demand, 0.0, max_part);


      // Constants
      // =========
      fill_real_array (c.x_p_stage_code, 0.0, max_part);

      for (int part = 0; part < max_part; part++)
      {
         c.p_stress_determinants[part] = false;
         c.p_yield_parts[part] = false;
         c.p_retrans_parts[part] = false;
         fill_real_array (c.y_p_conc_max[part], 0.0, max_table);
         fill_real_array (c.y_p_conc_min[part], 0.0, max_table);
         fill_real_array (c.y_p_conc_sen[part], 0.0, max_table);
      }
      g.pfact_photo        = 1.0;
      g.pfact_expansion    = 1.0;
      g.pfact_pheno        = 1.0;
      g.pfact_grain        = 1.0;

      c.pfact_photo_slope = 0.0;
      c.pfact_expansion_slope = 0.0;
      c.pfact_pheno_slope = 0.0;
      c.num_x_p_stage_code = 0;
      g.phosphorus_aware = false;

      pop_routine (my_name);
 }





//     ===========================================================
void Plant::read_p_constants (PlantComponent *systemInterface)
{
//+  Purpose
//       Read all module constants.

//+  Constant Values
    const char*  my_name = "PlantP_read_constants";
    const char*  section_name = "constants" ;

//+  Local Variables
    int    numvals;                  // number of values read
    int    num_sections;             // number of sections to search
    float  temp[max_table];
    string scratch;
    vector<string> parts;
    vector<string>::iterator part;
//- Implementation Section ----------------------------------

    push_routine (my_name);

    Split_string(systemInterface->readParameter (section_name, "stress_determinants"), " ", parts);
    // Find each part and mark it..
    for (part = parts.begin(); part != parts.end(); part++) 
       {
       vector<string>::iterator cpart = find(c.part_names.begin(),
                                             c.part_names.end(), *part);
       if (cpart != c.part_names.end()) 
          {
          int ipart = cpart - c.part_names.begin();
          c.p_stress_determinants[ipart] = true;
          }
       }

    Split_string(systemInterface->readParameter (section_name, "yield_parts"), " ", parts);
    for (part = parts.begin(); part != parts.end(); part++) 
       {
       vector<string>::iterator cpart = find(c.part_names.begin(),
                                             c.part_names.end(), *part);
       if (cpart != c.part_names.end()) 
          {
          int ipart = cpart - c.part_names.begin();
          c.p_yield_parts[ipart] = true;
          }
       }

    Split_string(systemInterface->readParameter (section_name, "retrans_parts"), " ", parts);
    for (part = parts.begin(); part != parts.end(); part++) 
       {
       vector<string>::iterator cpart = find(c.part_names.begin(),
                                             c.part_names.end(), *part);
       if (cpart != c.part_names.end()) 
          {
          int ipart = cpart - c.part_names.begin();
          c.p_retrans_parts[ipart] = true;
          }
       }

    systemInterface->readParameter (section_name, "pfact_photo_slope",    /*"()",   */ c.pfact_photo_slope    , 0.0, 100.0);
    systemInterface->readParameter (section_name, "pfact_expansion_slope",/*"()",   */ c.pfact_expansion_slope, 0.0, 100.0);
    systemInterface->readParameter (section_name, "pfact_pheno_slope",    /*"()",   */ c.pfact_pheno_slope    , 0.0, 100.0);
    systemInterface->readParameter (section_name, "pfact_grain_slope",    /*"()",   */ c.pfact_grain_slope    , 0.0, 100.0);

    systemInterface->readParameter (section_name, "x_p_stage_code", /*"()",*/ c.x_p_stage_code, c.num_x_p_stage_code, 0.0, 12.0);
    for (int ipart = 0; ipart < max_part; ipart++)
       {
       string keyword ="y_p_conc_max_" + c.part_names[ipart];
       systemInterface->readParameter (section_name, keyword, /*"(g/g)",*/ temp, c.num_x_p_stage_code, 0.0, 1.0);

       for (int i=0; i < c.num_x_p_stage_code; i++)
       {
          c.y_p_conc_max[ipart][i] = temp[i];
       }

       keyword = "y_p_conc_sen_" + c.part_names[ipart];
       systemInterface->readParameter (section_name, keyword, /*"(g/g)", */temp, c.num_x_p_stage_code, 0.0, 1.0);

       for (int i=0; i < c.num_x_p_stage_code; i++)
       {
          c.y_p_conc_sen[ipart][i] = temp[i];
       }

       keyword = "y_p_conc_min_" + c.part_names[ipart];
       systemInterface->readParameter (section_name, keyword, /*"(g/g)",*/ temp, c.num_x_p_stage_code, 0.0, 1.0);

       for (int i=0; i < c.num_x_p_stage_code; i++)
       {
          c.y_p_conc_min[ipart][i] = temp[i];
       }

       keyword = "p_conc_init_" + c.part_names[ipart];
       systemInterface->readParameter (section_name, keyword, /*"(g/g)",*/ c.p_conc_init[ipart], 0.0, 1.0);
   }

   pop_routine  (my_name);
}

void Plant::prepare_p(void)
{
   if (g.phosphorus_aware == true)
      {
      vector<plantPart*> allParts;
      setupHacks(allParts);
      PlantP_demand(allParts, g.dlt_dm_pot_rue);
      PlantP_Stress(allParts);
      deleteHacks(allParts);
      }
}

// ====================================================================
void Plant::process_p(void)
{
//+  Purpose
//     <insert here>

//+  Changes
//

//+  Constant Values
      const char*  my_name = "PlantP_process";

//+  Local Variables
      vector<plantPart *>::iterator part;

//- Implementation Section ----------------------------------
      push_routine (my_name);

      if (g.phosphorus_aware == true)
      {
         vector<plantPart*> allParts;
         setupHacks(allParts);

         PlantP_init_pools(allParts);

         PlantP_partition(allParts);
         PlantP_senescence (allParts);

         for (part = allParts.begin(); part != allParts.end(); part++)
         {
            (*part)->g.p_green += (*part)->dlt.p_green;
            (*part)->g.p_green -= (*part)->dlt.p_sen;
         }

         // Now do any retranslocation to try and keep pools
         // at target concentrations.
         PlantP_retrans (allParts);

         for (part = allParts.begin(); part != allParts.end(); part++)
         {
            (*part)->g.p_green += (*part)->dlt.p_retrans;
         }
         deleteHacks(allParts);
      }
      pop_routine (my_name);
}

// ====================================================================
void Plant::detachment_p(void)
{
//+  Purpose
//     <insert here>

//+  Changes
//

//+  Constant Values
      const char*  my_name = "detachment_p";

//+  Local Variables


//- Implementation Section ----------------------------------
      push_routine (my_name);

      if (g.phosphorus_aware == true)
      {
         vector<plantPart*> allParts;
         vector<plantPart*>::iterator part;
         setupHacks(allParts);

         PlantP_detachment (allParts);

         for (part = allParts.begin(); part != allParts.end(); part++)
         {
            (*part)->g.p_sen +=  (*part)->dlt.p_sen;
            (*part)->g.p_sen -= (*part)->dlt.p_det;
            (*part)->g.p_dead -= (*part)->dlt.p_dead_det;
         }
         deleteHacks(allParts);
      }
      pop_routine (my_name);
}

// ====================================================================
void Plant::death_p(void)
{
//+  Purpose
//     <insert here>

//+  Changes
//

//+  Constant Values
      const char*  my_name = "PlantP_death";

//+  Local Variables

//- Implementation Section ----------------------------------
      push_routine (my_name);

      if (g.phosphorus_aware == true)
      {
         float dying_fract_plants = divide (-g.dlt_plants, g.plants, 0.0);
         dying_fract_plants = bound (dying_fract_plants, 0.0, 1.0);
         if (dying_fract_plants > 0.0) 
            {
            vector<plantPart*> parts;
            vector<plantPart*>::iterator part;
            setupHacks(parts);
            for (part = parts.begin(); part != parts.end(); part++)
                {
                float dlt_p_green_dead = (*part)->g.p_green * dying_fract_plants;
                (*part)->g.p_green -= dlt_p_green_dead;
                (*part)->g.p_dead += dlt_p_green_dead;
                
                float dlt_p_senesced_dead = (*part)->g.p_sen * dying_fract_plants;
                (*part)->g.p_sen  -= dlt_p_senesced_dead;
                (*part)->g.p_dead += dlt_p_senesced_dead;
                }
            deleteHacks(parts);
            }
      }
      pop_routine (my_name);
}

// ===============================
void Plant::PlantP_partition (vector<plantPart*>&parts)
{
//+  Purpose
//     <insert here>

//+  Changes
//

//+  Constant Values
      const char*  my_name = "PlantP_partition";

//+  Local Variables
      vector<float> values;               // Scratch area
      vector<plantPart*>::iterator part;
      float p_uptake;
      float total_demand;


//- Implementation Section ----------------------------------
      push_routine (my_name);

      if (id.layered_p_uptake != 0)
      {
          parent->getVariable(id.layered_p_uptake, values, 0.0, 100.0);
          float sumValue = 0.0;
          for (unsigned int i = 0; i < values.size(); i++)
          {
          	sumValue += values[i];
          }
          p_uptake = sumValue * kg2gm/ha2sm;
      }
      else
      {
          p_uptake = 0.0;
          for (part = parts.begin(); part != parts.end(); part++)
             p_uptake += (*part)->v.p_demand;
      }

      total_demand = 0.0;
      for (part = parts.begin(); part != parts.end(); part++)
          total_demand += (*part)->v.p_demand;;

      for (part = parts.begin(); part != parts.end(); part++)
      {
         (*part)->dlt.p_green = p_uptake
                                  * divide((*part)->v.p_demand
                                           ,total_demand
                                           ,0.0);
      }
      pop_routine (my_name);
}

// ====================================================================
void Plant::PlantP_senescence (vector<plantPart*>&parts)
{
//+  Purpose
//     <insert here>

//+  Changes
//

//+  Constant Values
      const char*  my_name = "PlantP_senescence";

//+  Local Variables
      vector<plantPart*>::iterator part;
      float p_conc_green;
      float p_conc_sen;

//- Implementation Section ----------------------------------
      push_routine (my_name);

      for (part = parts.begin(); part != parts.end(); part++)
         (*part)->dlt.p_sen = 0.0;
 
      for (part = parts.begin(); part != parts.end(); part++)
      {
         if ((*part)->dlt.dm_senesced < (*part)->g.dm_green)
         {
            p_conc_green = divide ((*part)->g.p_green
                                  ,(*part)->g.dm_green
                                  , 0.0);

            p_conc_sen = linear_interp_real (phenology->stageNumber()
                                           , (*part)->c.x_p_stage_code
                                           , (*part)->c.y_p_conc_sen
                                           , (*part)->c.num_x_p_stage_code);

            (*part)->dlt.p_sen = min(p_conc_green, p_conc_sen) * (*part)->dlt.dm_senesced;
         }
         else
         {
            (*part)->dlt.p_sen = (*part)->g.p_green;
         }
      }

      pop_routine (my_name);
}


// ====================================================================
void Plant::PlantP_detachment (vector<plantPart*>&parts)
{
//+  Purpose

//+  Changes

//+  Constant Values
      const char*  my_name = "PlantP_detachment";

//+  Local Variables
      vector<plantPart*>::iterator part;

//- Implementation Section ----------------------------------
      push_routine (my_name);

      for (part = parts.begin(); part != parts.end(); part++)
      {
         float sen_detach_frac = divide((*part)->dlt.dm_detached
                                       , (*part)->g.dm_senesced
                                       , 0.0);

         (*part)->dlt.p_det = (*part)->g.p_sen * sen_detach_frac;

         float dead_detach_frac = divide((*part)->dlt.dm_dead_detached
                                       , (*part)->g.dm_dead
                                       , 0.0);

         (*part)->dlt.p_dead_det = (*part)->g.p_dead * dead_detach_frac;
      }

      pop_routine (my_name);
}


// ====================================================================
void Plant::zero_daily_p_variables ()
{

//+  Purpose
//     <insert here>

//+  Changes
//

//+  Constant Values
      const char*  my_name = "PlantP::zero_daily_p_variables";

//- Implementation Section ----------------------------------
      push_routine (my_name);

      fill_real_array (g.dlt_p_green, 0.0, max_part);
      fill_real_array (g.dlt_p_dead, 0.0, max_part);
      fill_real_array (g.dlt_p_sen, 0.0, max_part);
      fill_real_array (g.dlt_p_det, 0.0, max_part);
      fill_real_array (g.dlt_p_dead_det, 0.0, max_part);
      fill_real_array (g.dlt_p_retrans, 0.0, max_part);
      fill_real_array (g.p_demand, 0.0, max_part);

      pop_routine (my_name);
}
// ====================================================================
void Plant::doPInit (PlantComponent *systemInterface)
{
      if (g.phosphorus_aware == true)
      {
         read_p_constants (systemInterface);

         string keyword = "uptake_p_" + c.crop_type;
         id.layered_p_uptake = systemInterface->addRegistration(RegistrationType::get,
                                                               keyword.c_str(), floatArrayType,
                                                               "", "");
      }
}


// ====================================================================
void Plant::PlantP_set_phosphorus_aware (PlantComponent *systemInterface)
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
         parent->writeString ("   - Module is set phosphorus aware");
         parent->writeString (" ");
      }
      else
      {
         g.phosphorus_aware = false;
      }

   pop_routine (my_name);
}



// ====================================================================
void Plant::PlantP_demand (vector<plantPart *> &allParts, float dlt_dm_pot)
{

//+  Purpose
//      Calculate plant P demands

//+  Changes
//     <insert here>

//+  Constant Values
      const char*  my_name = "PlantP_demand";

//+  Local Variables
      vector<plantPart *>::iterator part;
      float    deficit;
      float    p_conc_max;
      float    rel_growth_rate;
      float    dm_green_sum;

//- Implementation Section ----------------------------------
      push_routine (my_name);

      dm_green_sum = 0.0;
      for (part = allParts.begin(); part != allParts.end(); part++) 
         {
         (*part)->v.p_demand = 0.0;
         dm_green_sum += (*part)->g.dm_green;
         }
      rel_growth_rate = divide(dlt_dm_pot, dm_green_sum, 0.0);

      for (part = allParts.begin(); part != allParts.end(); part++) 
         {
         if ((*part)->c.p_yield_part)
            {
            // A yield part - does not contribute to soil demand
            (*part)->v.p_demand = 0.0;
            }
         else
            {
            // Not a yield part - therefore it contributes to demand
            p_conc_max = linear_interp_real (phenology->stageNumber()
                                           , (*part)->c.x_p_stage_code
                                           , (*part)->c.y_p_conc_max
                                           , (*part)->c.num_x_p_stage_code);

            // scale up to include potential new growth
            // assuming partitioning today similar to current
            // plant form - a rough approximation
            deficit = p_conc_max * (*part)->g.dm_green * (1.0 + rel_growth_rate) - (*part)->g.p_green;

            (*part)->v.p_demand = l_bound(deficit, 0.0);
            }
         }
      pop_routine (my_name);
}


// ====================================================================
float Plant::PlantP_Pfact (vector<plantPart *> &allParts)
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
      vector<plantPart*>::iterator part;

//- Implementation Section ----------------------------------
      push_routine (my_name);

   if (g.phosphorus_aware == true)
   {
      act_p = 0.0;
      min_p = 0.0;
      max_p = 0.0;
      determinants_wt = 0.0;

      for (part = allParts.begin(); part != allParts.end(); part++)
         {
         if ((*part)->c.p_stress_determinant)
            {
            act_p += (*part)->g.p_green;

            p_conc_max = linear_interp_real (phenology->stageNumber()
                                           , (*part)->c.x_p_stage_code
                                           , (*part)->c.y_p_conc_max
                                           , (*part)->c.num_x_p_stage_code);
            max_p += p_conc_max * (*part)->g.dm_green;

            p_conc_min = linear_interp_real (phenology->stageNumber()
                                           , (*part)->c.x_p_stage_code
                                           , (*part)->c.y_p_conc_min
                                           , (*part)->c.num_x_p_stage_code);
            min_p += p_conc_min * (*part)->g.dm_green;
            determinants_wt += (*part)->g.dm_green;
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
void Plant::PlantP_Stress (vector<plantPart *> &allParts)
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

      pfact = PlantP_Pfact(allParts);

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
void Plant::PlantP_init_pools (vector<plantPart*>&parts)
{

//+  Purpose
//      Initialise Plant P Pools

//+  Changes
//     <insert here>

//+  Constant Values
      const char*  my_name = "PlantP_init_pools";

//+  Local Variables
   vector<plantPart *>::iterator part;
   float dmSum = 0.0, pSum = 0.0; 

//- Implementation Section ----------------------------------
      push_routine (my_name);

   // This is wrong. need to initialise these on an event. XXXX
   for (part = parts.begin(); part != parts.end(); part++) 
      { 
      dmSum += (*part)->g.dm_green;
      pSum += (*part)->g.p_green;
      }
   if (dmSum > 0.0 && pSum <= 0.0)
         {
         // biomass has been initialised but the p pools have not
         for (part = parts.begin(); part != parts.end(); part++) 
            {
            (*part)->g.p_green = (*part)->g.dm_green * (*part)->c.p_init_conc;
            }
         }
   pop_routine (my_name);
}
// ====================================================================
void Plant::PlantP_retrans (vector<plantPart*>&parts)
{

//+  Purpose
//      Calculate retranslocation between pools

//+  Changes
//     <insert here>

//+  Constant Values
      const char*  my_name = "PlantP_retrans";

//+  Local Variables
      vector<float>    supply(parts.size());
      vector<float>    demand(parts.size());

      unsigned int ipart;
      float    p_conc_min;
      float    min_p;
      float    p_conc_max;
      float    max_p;
      float    fraction;
      float    totSupply, totDemand;

//- Implementation Section ----------------------------------
      push_routine (my_name);

      for (ipart =0; ipart != parts.size(); ipart++)
         {
         if (parts[ipart]->c.p_retrans_part)
            {
            p_conc_min = linear_interp_real (phenology->stageNumber()
                                        , parts[ipart]->c.x_p_stage_code
                                        , parts[ipart]->c.y_p_conc_min
                                        , parts[ipart]->c.num_x_p_stage_code);
            min_p = p_conc_min * parts[ipart]->g.dm_green;
            supply[ipart] = max(parts[ipart]->g.p_green - min_p, 0.0);
            }
         else 
            {
            supply[ipart] = 0.0;
            }   
         }

      for (ipart =0; ipart != parts.size(); ipart++)
         {
         if (parts[ipart]->c.p_yield_part)
            {
            p_conc_max = linear_interp_real (phenology->stageNumber()
                                        , parts[ipart]->c.x_p_stage_code
                                        , parts[ipart]->c.y_p_conc_max
                                        , parts[ipart]->c.num_x_p_stage_code);
            max_p = p_conc_max * parts[ipart]->g.dm_green;
            demand[ipart] = max(max_p - parts[ipart]->g.p_green, 0.0);
            }
         else
            {
            demand[ipart] = 0.0;
            }
         }

      totSupply = totDemand = 0.0;
      for (ipart =0; ipart != parts.size(); ipart++)
         {
         totSupply += supply[ipart];
         totDemand += demand[ipart];
         }

      for (ipart =0; ipart != parts.size(); ipart++)
         {
         if (supply[ipart] > 0.0)
            {
            fraction = divide(totDemand, totSupply, 0.0);
            fraction = bound(fraction, 0.0, 1.0);
            parts[ipart]->dlt.p_retrans = - supply[ipart] * fraction;
            }
         else if (demand[ipart] > 0.0)
            {
            fraction = divide(totSupply, totDemand, 0.0);
            fraction = bound(fraction, 0.0, 1.0);
            parts[ipart]->dlt.p_retrans = demand[ipart] * fraction;
            }
         else
            {
            parts[ipart]->dlt.p_retrans = 0.0;// this part is not involved
            }
         }
      pop_routine (my_name);
}

// ====================================================================
void Plant::summary_p (void)
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

//- Implementation Section ----------------------------------          g.p_green(1:g.num_parts)
      push_routine (my_name);

   if (g.phosphorus_aware == true)
   {
       P_grain_conc_percent = divide (
           g.p_green[meal] + g.p_dead[meal] + g.p_green[oil] + g.p_dead[oil]
         , g.dm_green[meal] + g.dm_dead[meal] + g.dm_green[oil] + g.dm_dead[oil]
         , 0.0) * fract2pcnt;

       P_grain = (g.p_green[meal] + g.p_dead[meal]
               +  g.p_green[oil]  + g.p_dead[oil]) * gm2kg/sm2ha;

       P_green = (sum_real_array (g.p_green, max_part) + leafPart->g.p_green + stemPart->g.p_green 
               -  g.p_green[root] - g.p_green[meal] - g.p_green[oil]) * gm2kg / sm2ha;

       P_senesced = (sum_real_array (g.p_sen, max_part) + leafPart->g.p_sen + stemPart->g.p_sen
                  -  g.p_sen[root] - g.p_sen[meal]- g.p_sen[oil]) * gm2kg / sm2ha;

       P_dead = (sum_real_array (g.p_dead, max_part) + leafPart->g.p_dead + stemPart->g.p_dead
           - g.p_dead[root] - g.p_dead[meal] - g.p_dead[oil]) * gm2kg / sm2ha;

       P_stover = P_green + P_senesced + P_dead;
       P_total = P_grain + P_stover;

       sprintf (msg, "%s%10.2f%20s%s%10.2f"
                , " grain P percent        = ", P_grain_conc_percent, " "
                , " total P content (kg/ha)= ", P_total);
       parent->writeString (msg);

       sprintf (msg, "%s%10.2f%20s%s%8.2f"
                , " grain P uptake (kg/ha) = ", P_grain, " "
                , " senesced P content (kg/ha)=", P_senesced);
       parent->writeString (msg);

       sprintf (msg, "%s%10.2f%20s%s%10.2f"
                , " green P content (kg/ha)= ", P_green, " "
                , " dead P content (kg/ha) = ", P_dead);
       parent->writeString (msg);
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



