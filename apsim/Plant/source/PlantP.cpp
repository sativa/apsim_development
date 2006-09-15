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
#include <iostream.h>
#include <boost/function.hpp>
#include <boost/bind.hpp>

using namespace std;

#include <ComponentInterface/Type.h>
#include <ComponentInterface/ApsimVariant.h>
#include <ComponentInterface/Component.h>
#include <ComponentInterface/datatypes.h>
#include <ComponentInterface/Messages.h>
#include <ComponentInterface/MessageDataExt.h>
#include <ApsimShared/ApsimComponentData.h>
#include <ApsimShared/FStringExt.h>
#include <general/string_functions.h>

#include "PlantLibrary.h"
#include "PlantComponent.h"
#include "Plant.h"
#include "PlantPart.h"
#include "CompositePart.h"
#include "PlantFruit.h"
#include "Phenology/PlantPhenology.h"


static const char* floatArrayType =   "<type kind=\"single\" array=\"T\"/>";

void Plant::zero_p_variables ()
// =======================================
//     Set all variables in this module to zero.
{
      g.pfact_photo        = 1.0;
      g.pfact_expansion    = 1.0;
      g.pfact_pheno        = 1.0;
      g.pfact_grain        = 1.0;

      c.pfact_photo_slope = 0.0;
      c.pfact_expansion_slope = 0.0;
      c.pfact_pheno_slope = 0.0;
      c.num_x_p_stage_code = 0;
      g.phosphorus_aware = false;

 }





//     ===========================================================
void Plant::read_p_constants (PlantComponent *systemInterface)
{
//+  Purpose
//       Read all module constants.

//+  Constant Values
    const char*  section_name = "constants" ;

//+  Local Variables
    systemInterface->readParameter (section_name, "pfact_photo_slope",    /*"()",   */ c.pfact_photo_slope    , 0.0, 100.0);
    systemInterface->readParameter (section_name, "pfact_expansion_slope",/*"()",   */ c.pfact_expansion_slope, 0.0, 100.0);
    systemInterface->readParameter (section_name, "pfact_pheno_slope",    /*"()",   */ c.pfact_pheno_slope    , 0.0, 100.0);
    systemInterface->readParameter (section_name, "pfact_grain_slope",    /*"()",   */ c.pfact_grain_slope    , 0.0, 100.0);
}

void Plant::prepare_p(void)
{
   if (g.phosphorus_aware == true)
      {
      PlantP_demand(myParts);
      PlantP_Stress(myParts);
      }
}


// ===============================
void Plant::PlantP_partition (vector<plantPart*>&parts)
{
      vector<float> values;               // Scratch area
      vector<plantPart*>::iterator part;
      float p_uptake;

      float total_p_demand = 0.0;
      for (part = parts.begin(); part != parts.end(); part++)
          total_p_demand += (*part)->pDemand();

      if (id.layered_p_uptake != 0)
      {
          parent->getVariable(id.layered_p_uptake, values, 0.0, 100.0);
          float sumValue = 0.0;
          for (unsigned int i = 0; i < values.size(); i++)
          	sumValue += values[i];

          p_uptake = sumValue * kg2gm/ha2sm;
      }
      else
          p_uptake = total_p_demand;

      for (part = parts.begin(); part != parts.end(); part++)
         (*part)->doPPartition(p_uptake, total_p_demand);

}


// ====================================================================
void Plant::PlantP_senescence (vector<plantPart*>&parts)
{
        for (vector<plantPart *>::iterator t = parts.begin();
             t != parts.end();
             t++)
           (*t)->doPSenescence();
}


// ====================================================================
void Plant::zero_daily_p_variables ()
{

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


void Plant::PlantP_set_phosphorus_aware (PlantComponent *systemInterface)
// ====================================================================
//      Check that soil phosphorus is in system
{
//+  Local Variables
      vector<float> values;               // Scratch area
      bool soilpPresent;

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

}


// ====================================================================
void Plant::PlantP_demand (vector<plantPart *> &allParts)
{

//+  Purpose
//      Calculate plant P demands

        for (vector<plantPart *>::iterator t = allParts.begin();
             t != allParts.end();
             t++)
           (*t)->doPDemand();
}


float Plant::PlantP_Pfact (vector<plantPart *> &allParts)
// ====================================================================
//      Provide value of generic P factor
{
      float    max_p;
      float    min_p;
      float    act_p;
      float    max_p_conc;
      float    min_p_conc;
      float    act_p_conc;
      float    determinants_wt;
      float    pfact;
      vector<plantPart*>::iterator part;

   if (g.phosphorus_aware == true)
   {
      act_p = 0.0;
      min_p = 0.0;
      max_p = 0.0;
      determinants_wt = 0.0;

      for (part = allParts.begin(); part != allParts.end(); part++)
         {
            act_p += (*part)->pGreenStressDeterminant();
            max_p += (*part)->pMaxPotStressDeterminant();
            min_p += (*part)->pMinPotStressDeterminant();
            determinants_wt += (*part)->dmGreenStressDeterminant();

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

   return pfact;
}

void Plant::PlantP_Stress (vector<plantPart *> &allParts)
// ====================================================================
//      Provide value of  P stress factors
{
      float    pfact = PlantP_Pfact(allParts);

      g.pfact_photo = pfact * c.pfact_photo_slope;
      g.pfact_photo = bound(g.pfact_photo, 0.0, 1.0);

      g.pfact_expansion = pfact * c.pfact_expansion_slope;
      g.pfact_expansion = bound(g.pfact_expansion, 0.0, 1.0);

      g.pfact_pheno = pfact * c.pfact_pheno_slope;
      g.pfact_pheno = bound(g.pfact_pheno, 0.0, 1.0);

      g.pfact_grain = pfact * c.pfact_grain_slope;
      g.pfact_grain = bound(g.pfact_grain, 0.0, 1.0);

}
void Plant::PlantP_init_pools (vector<plantPart*>&parts)  //FIXME - this is not referenced anywhere!!!
// ====================================================================
//      Initialise Plant P Pools
{
//+  Local Variables
   vector<plantPart *>::iterator part;
   float dmSum = 0.0, pSum = 0.0;

//- Implementation Section ----------------------------------

   // This is wrong. need to initialise these on an event. XXXX
   for (part = parts.begin(); part != parts.end(); part++)
      {
      dmSum += (*part)->dmGreen();
      pSum += (*part)->pGreen();
      }



   if (dmSum > 0.0 && pSum <= 0.0)
         {
         // biomass has been initialised but the p pools have not
         for (part = parts.begin(); part != parts.end(); part++)
            {
            (*part)->doPInit();
            }
         }
}

void Plant::plant_p_retrans(void)
{
   if (g.phosphorus_aware == true)
      {
      PlantP_retrans (myParts);
      }
}

void Plant::PlantP_retrans (vector<plantPart*>&parts)
//      Calculate retranslocation between pools
{
      vector<float>    supply(parts.size());
      vector<float>    demand(parts.size());

      unsigned int ipart;
      float    totSupply, totDemand;

//- Implementation Section ----------------------------------

      totSupply = 0.0;
      totDemand = 0.0;
      for (ipart =0; ipart != parts.size(); ipart++)
         {
         totSupply += parts[ipart]->pRetransSupply();
         totDemand += parts[ipart]->pRetransDemand();
         }

      for (ipart =0; ipart != parts.size(); ipart++)
         parts[ipart]->doPRetranslocate(totSupply, totDemand);

}

// ====================================================================
void Plant::summary_p (void)
{
   char  msg[400];
      float       P_grain;               // total grain P uptake (kg/ha)
      float       P_dead;                // above ground dead plant P (kg/ha)
      float       P_green;               // above ground green plant P (kg/ha)
      float       P_senesced;            // above ground senesced plant P (kg/ha)
      float       P_stover;              // nitrogen content of stover (kg\ha)
      float       P_total;               // total gross nitrogen content (kg/ha)
      float       P_grain_conc_percent;  // grain nitrogen .

//- Implementation Section ----------------------------------          g.p_green(1:g.num_parts)

   if (g.phosphorus_aware == true)
   {
       P_grain_conc_percent = grainPConcTot();

       P_grain = grainPTot() * gm2kg/sm2ha;

       P_green = stoverPGreen() * gm2kg / sm2ha;

       P_senesced = stoverPSenesced() * gm2kg / sm2ha;

       P_dead = stoverPDead() * gm2kg / sm2ha;

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
}








