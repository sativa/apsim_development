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
#include "CompositePart.h"
#include "PlantFruit.h"
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

      // Constants
      // =========
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
         vector<plantPart*>::iterator part;

         PlantP_detachment (myParts);

         for (part = myParts.begin(); part != myParts.end(); part++)
         {
            (*part)->updatePDet();
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
      float total_p_demand;


//- Implementation Section ----------------------------------
      push_routine (my_name);

      total_p_demand = 0.0;
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

      pop_routine (my_name);
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
         (*part)->doPDetachment();

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
void Plant::PlantP_demand (vector<plantPart *> &allParts)
{

//+  Purpose
//      Calculate plant P demands

        for (vector<plantPart *>::iterator t = allParts.begin();
             t != allParts.end();
             t++)
           (*t)->doPDemand();
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
void Plant::PlantP_init_pools (vector<plantPart*>&parts)  //FIXME - this is not referenced anywhere!!!
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
   pop_routine (my_name);
}

void Plant::plant_p_retrans(void)
{
   if (g.phosphorus_aware == true)
      {
      PlantP_retrans (myParts);
      }
}

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
      float    totSupply, totDemand;

//- Implementation Section ----------------------------------
      push_routine (my_name);

      totSupply = 0.0;
      totDemand = 0.0;
      for (ipart =0; ipart != parts.size(); ipart++)
         {
         totSupply += parts[ipart]->pRetransSupply();
         totDemand += parts[ipart]->pRetransDemand();
         }

      for (ipart =0; ipart != parts.size(); ipart++)
         parts[ipart]->doPRetranslocate(totSupply, totDemand);

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




