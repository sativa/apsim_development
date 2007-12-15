/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *                                                  *
*       Author:  John Hargreaves                                               *
*     Date written: 25 Feb 2004                                                *
* Acknowledgements: Neil Huth, CSIRO, Sustainable Ecosystems.                  *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
//
//          PlantP.cpp
//              PlantP class definition (Orthodox Canonical Form)
//
//              Defines default constructor, copy constructor and
//                assignment operator.
//
// Modification log
//  25 Feb 04  J. Hargreaves  implementation

#include <stdio.h>
#include <math.h>
#include <stdexcept>
#include <string>
#include "PlantPart.h"

using namespace std;

#include "Plant.h"
#include "Reproductive/PlantFruit.h"
#include "Phenology/PlantPhenology.h"


static const char* floatArrayType =   "<type kind=\"single\" array=\"T\"/>";

void Plant::zero_p_variables ()
// =======================================
//     Set all variables in this module to zero.
{
      pFact = 1.0;

      c.pFactSlope = 0.0;
      c.num_x_p_stage_code = 0;

 }





//     ===========================================================
void Plant::read_p_constants (PlantComponent *systemInterface)
{
//+  Purpose
//       Read all module constants.

//+  Constant Values
    const char*  section_name = "constants" ;

//+  Local Variables
    scienceAPI.read("pfact_photo_slope", c.pFactSlope.photo, 0.0f, 100.0f);
    scienceAPI.read("pfact_expansion_slope", c.pFactSlope.expansion, 0.0f, 100.0f);
    scienceAPI.read("pfact_pheno_slope", c.pFactSlope.pheno, 0.0f, 100.0f);
    scienceAPI.read("pfact_grain_slope", c.pFactSlope.grain, 0.0f, 100.0f);
}
// ===============================

void Plant::prepare_p(void)
{
   if (phosphorus->isPresent())
      {
      plant.doPDemand();
      PlantP_Stress(myParts);
      phosphorus->PlantP_Stress(myParts);
      }
}


// ===============================
void Plant::doPPartition (vector<plantPart*>&parts)
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
void Plant::doPInit (PlantComponent *systemInterface)
{
      if (phosphorus->isPresent())
      {
         read_p_constants (systemInterface);
         phosphorus->read_p_constants();

         string keyword = "uptake_p_" + c.crop_type;
         id.layered_p_uptake = systemInterface->addRegistration(RegistrationType::get,
                                                               keyword.c_str(), floatArrayType,
                                                               "", "");
      }

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

   if (phosphorus->isPresent())
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

      pFact.photo = pfact * c.pFactSlope.photo;
      pFact.photo = bound(pFact.photo, 0.0, 1.0);

      pFact.expansion = pfact * c.pFactSlope.expansion;
      pFact.expansion = bound(pFact.expansion, 0.0, 1.0);

      pFact.pheno = pfact * c.pFactSlope.pheno;
      pFact.pheno = bound(pFact.pheno, 0.0, 1.0);

      pFact.grain = pfact * c.pFactSlope.grain;
      pFact.grain = bound(pFact.grain, 0.0, 1.0);

}

void Plant::doPRetranslocate (void)
//      Calculate retranslocation between pools
{
      vector<float>    supply(myParts.size());
      vector<float>    demand(myParts.size());

      unsigned int ipart;
      float    totSupply, totDemand;

//- Implementation Section ----------------------------------

      totSupply = 0.0;
      totDemand = 0.0;
      for (ipart =0; ipart != myParts.size(); ipart++)
         {
         totSupply += myParts[ipart]->pRetransSupply();
         totDemand += myParts[ipart]->pRetransDemand();
         }

      for (ipart =0; ipart != myParts.size(); ipart++)
         myParts[ipart]->doPRetranslocate(totSupply, totDemand);

}

// ====================================================================
void Plant::summary_p (void)
{
   char  msg[400];
      float       P_grain;               // total grain P uptake (kg/ha)
      float       P_dead;                // above ground dead plant P (kg/ha)
      float       P_stover;              // nitrogen content of stover (kg\ha)
      float       P_total;               // total gross nitrogen content (kg/ha)
      float       P_grain_conc_percent;  // grain nitrogen .

//- Implementation Section ----------------------------------          g.p_green(1:g.num_parts)

   if (phosphorus->isPresent())
   {
       P_grain_conc_percent = fruitPart->GrainTotal.PconcPercent();

       P_grain = tops.GrainTotal.P() * gm2kg/sm2ha;  // why not graintotal??


       P_stover = tops.VegetativeTotal.P()* gm2kg/sm2ha;
       P_total = P_grain + P_stover;
       P_dead = 0.0; //note we no longer have dead pools

       sprintf (msg, "%s%10.2f%20s%s%10.2f"
                , " grain P percent        = ", P_grain_conc_percent, " "
                , " total P content (kg/ha)= ", P_total);
       parent->writeString (msg);

       sprintf (msg, "%s%10.2f%20s%s%8.2f"
                , " grain P uptake (kg/ha) = ", P_grain, " "
                , " senesced P content (kg/ha)=", (tops.VegetativeTotal.P() - tops.Vegetative.P())* gm2kg/sm2ha);
       parent->writeString (msg);

       sprintf (msg, "%s%10.2f%20s%s%10.2f"
                , " green P content (kg/ha)= ", tops.Vegetative.P() * gm2kg/sm2ha, " "
                , " dead P content (kg/ha) = ", P_dead);
       parent->writeString (msg);
   }
}


//#####################################################################################
// Phosphorus implementation
//######################################################################################

Phosphorus::Phosphorus(ScienceAPI& scienceAPI, PlantComponent *p)
   : scienceAPI(scienceAPI)
   , parent(p)
{
      pFact = 1.0;
      c.pFactSlope = 0.0;
   PlantP_set_phosphorus_aware ();

   parent->addGettableVar("pfact_photo",
               pFact.photo,
               "", "P factor in photosynthesis");

   parent->addGettableVar("pfact_pheno",
               pFact.pheno,
               "", "P factor in phenology");

   parent->addGettableVar("pfact_expansion",
               pFact.expansion,
               "", "P factor in leaf expansion");

   parent->addGettableVar("pfact_grain",
               pFact.grain,
               "", "P factor in grain");

   setupGetFunction(parent, "p_stress_photo", protocol::DTsingle, false,
                    &Phosphorus::get_pstress_photo,
                    "", "P stress in photosynthesis");

   setupGetFunction(parent, "p_stress_pheno", protocol::DTsingle, false,
                    &Phosphorus::get_pstress_pheno,
                    "", "P stress in phenology");

   setupGetFunction(parent, "p_stress_expansion", protocol::DTsingle, false,
                    &Phosphorus::get_pstress_expansion,
                    "", "P stress in leaf expansion");

   setupGetFunction(parent, "p_stress_expan", protocol::DTsingle, false,
                    &Phosphorus::get_pstress_expansion,
                    "", "P stress in leaf expansion");

   setupGetFunction(parent, "p_stress_grain", protocol::DTsingle, false,
                    &Phosphorus::get_pstress_grain,
                    "", "P stress in grain");

   }

// destructor
Phosphorus::~Phosphorus()
{
}

bool Phosphorus::isPresent(void)
{
   return phosphorus_aware;
};

void Phosphorus::PlantP_set_phosphorus_aware ()
// ====================================================================
//      Check that soil phosphorus is in system
{
//+  Local Variables
      vector<float> values;               // Scratch area
      bool soilpPresent;

      unsigned int idSoilpVar = parent->addRegistration(RegistrationType::get,
                                                               "labile_p", floatArrayType,
                                                               "", "");
      parent->getVariable(idSoilpVar, values, 0.0, 1000000.0, true);
      soilpPresent = (values.size() > 0);

      if(soilpPresent == true)
      {
           //module is p aware
         phosphorus_aware = true;
         parent->writeString ("   - Module is set phosphorus aware");
         parent->writeString (" ");
      }
      else
      {
         phosphorus_aware = false;
      }
}

//     ===========================================================
void Phosphorus::read_p_constants (void)
{
//+  Purpose
//       Read all module constants.

//+  Constant Values
    const char*  section_name = "constants" ;

//+  Local Variables
    scienceAPI.read("pfact_photo_slope", c.pFactSlope.photo, 0.0f, 100.0f);
    scienceAPI.read("pfact_expansion_slope", c.pFactSlope.expansion, 0.0f, 100.0f);
    scienceAPI.read("pfact_pheno_slope", c.pFactSlope.pheno, 0.0f, 100.0f);
    scienceAPI.read("pfact_grain_slope", c.pFactSlope.grain, 0.0f, 100.0f);
}
float Phosphorus::PlantP_Pfact (vector<plantPart *> &allParts)
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

   if (isPresent())
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

void Phosphorus::PlantP_Stress (vector<plantPart *> &allParts)
// ====================================================================
//      Provide value of  P stress factors
{
      float    pfact = PlantP_Pfact(allParts);

      pFact.photo = pfact * c.pFactSlope.photo;
      pFact.photo = bound(pFact.photo, 0.0, 1.0);

      pFact.expansion = pfact * c.pFactSlope.expansion;
      pFact.expansion = bound(pFact.expansion, 0.0, 1.0);

      pFact.pheno = pfact * c.pFactSlope.pheno;
      pFact.pheno = bound(pFact.pheno, 0.0, 1.0);

      pFact.grain = pfact * c.pFactSlope.grain;
      pFact.grain = bound(pFact.grain, 0.0, 1.0);

}

void Phosphorus::get_pfact_grain(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    systemInterface->sendVariable(qd, pFact.grain);  //()
}

void Phosphorus::get_pstress_photo(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    float pstress_photo;
    if (pFact.photo > 0.0)
       pstress_photo = 1.0 - pFact.photo;
    else
       pstress_photo = 0.0;
    systemInterface->sendVariable(qd, pstress_photo);  //()
}

void Phosphorus::get_pstress_pheno(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    float pstress_pheno;
    if (pFact.pheno > 0.0)
       pstress_pheno = 1.0 - pFact.pheno;
    else
       pstress_pheno = 0.0;
    systemInterface->sendVariable(qd, pstress_pheno);  //()
}

void Phosphorus::get_pstress_expansion(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    float pstress_expansion;
    if (pFact.expansion > 0.0)
       pstress_expansion = 1.0 - pFact.expansion;
    else
       pstress_expansion = 0.0;
    systemInterface->sendVariable(qd, pstress_expansion);  //()
}

void Phosphorus::get_pstress_grain(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    float pstress_grain;
    if (pFact.grain > 0.0)
       pstress_grain = 1.0 - pFact.grain;
    else
       pstress_grain = 0.0;
    systemInterface->sendVariable(qd, pstress_grain);  //()
}

//#####################################################################################
// Nitrogen implementation
//######################################################################################

Nitrogen::Nitrogen(ScienceAPI& scienceAPI, PlantComponent *p)
   : scienceAPI(scienceAPI)
   , parent(p)
{
}

// destructor
Nitrogen::~Nitrogen()
{
}

void Nitrogen::init(PlantComponent *p)
   {
      nFact = 1.0;
      c.nFact = 0.0;
   parent->addGettableVar("nfact_photo",
               nFact.photo, "", "N factor for photosynthesis");

   parent->addGettableVar("nfact_pheno",
               nFact.pheno, "", "N factor for phenology");

   parent->addGettableVar("nfact_expan",
               nFact.expansion, "", "N factor for leaf expansion");

   parent->addGettableVar("nfact_grain",
               nFact.grain, "", "N factor for ??");

   setupGetFunction(parent, "n_stress_photo", protocol::DTsingle, false,
                    &Nitrogen::get_nstress_photo,
                    "","N stress for photosyntesis");

   setupGetFunction(parent, "n_stress_pheno", protocol::DTsingle, false,
                    &Nitrogen::get_nstress_pheno,
                    "","N stress for phenology");

   setupGetFunction(parent, "n_stress_expan", protocol::DTsingle, false,
                    &Nitrogen::get_nstress_expan,
                    "","N stress for leaf expansion");

   setupGetFunction(parent, "n_stress_grain", protocol::DTsingle, false,
                    &Nitrogen::get_nstress_grain,
                    "","N stress for grain filling");


   }

//     ===========================================================
void Nitrogen::read_n_constants (void)
{
//+  Purpose
//       Read all module constants.

//+  Constant Values
    const char*  section_name = "constants" ;

//+  Local Variables
    //    plant_nfact
    scienceAPI.read("n_stress_option", c.n_stress_option, 1, 2);

//    scienceAPI.read("N_stress_start_stage", c.n_stress_start_stage, 0.0f, 100.0f);
    scienceAPI.read("n_fact_photo", c.nFact.photo, 0.0f, 100.0f);
    scienceAPI.read("n_fact_pheno", c.nFact.pheno, 0.0f, 100.0f);
    scienceAPI.read("n_fact_expansion", c.nFact.expansion, 0.0f, 100.0f);
    c.nFact.grain = 1.0;
}

void Nitrogen::plant_nit_stress (plantPart* leafPart, plantPart* stemPart)
//=======================================================================================
// Calculate Plant Nitrogen Stress Factors
    {
    if (c.n_stress_option == 1)
        {
        vector<plantPart *> parts;

        // Expansion uses leaves only
        parts.push_back(leafPart);
        nFact.expansion = critNFactor(parts, c.nFact.expansion);

        // Rest have leaf & stem
        parts.push_back(stemPart);
        nFact.pheno = critNFactor(parts, c.nFact.pheno);
        nFact.photo = critNFactor(parts, c.nFact.photo);
        nFact.grain = critNFactor(parts, c.nFact.grain);
        }
    else if (c.n_stress_option == 2)
        {
        vector< plantPart *> parts;

        // Expansion & photosynthesis from leaves only
        parts.push_back(leafPart);
        nFact.expansion = critNFactor(parts, c.nFact.expansion);
        nFact.photo = critNFactor(parts, c.nFact.photo);

        // leaf & stem
        parts.push_back(stemPart);
        nFact.pheno = critNFactor(parts, c.nFact.pheno);
        nFact.grain = critNFactor(parts, 1.0);
        }
    else
        {
        throw std::invalid_argument ("invalid template option in plant_nit_stress");
        }

    }


float Nitrogen::critNFactor(vector< plantPart *> &parts, float multiplier)
//=======================================================================================
//   Calculate Nitrogen stress factor from a bunch of parts
/*  Purpose
*   The concentration of Nitrogen in plant parts is used to derive a Nitrogen stress index
*   for many processes. This stress index is calculated from today's relative nutitional
*   status between a critical and minimum Nitrogen concentration.
*/
   {
   vector< plantPart *>::iterator part;

   float dm = 0.0, N = 0.0;
   for (part = parts.begin(); part != parts.end(); part++)
      {
      dm += (*part)->Green.DM();
      N += (*part)->Green.N();
      }

   if (dm > 0.0)
      {
      float N_conc = divide (N, dm, 0.0);

      // calculate critical N concentrations
      float N_crit = 0.0;
      for (part = parts.begin(); part != parts.end(); part++)
          N_crit += (*part)->nCrit();

      float N_conc_crit = divide (N_crit, dm, 0.0);

      // calculate minimum N concentrations
      float N_min = 0.0;
      for (part = parts.begin(); part != parts.end(); part++)
         N_min += (*part)->nMin();

      float N_conc_min = divide (N_min, dm, 0.0);

      //calculate shortfall in N concentrations
      float dividend =  N_conc - N_conc_min;
      float divisor =   N_conc_crit - N_conc_min;
      float result = multiplier * divide (dividend, divisor, 1.0);
      result = bound (result, 0.0, 1.0);
      return (result);
      }
   else
      return (1.0);
   }

void Nitrogen::get_nstress_pheno(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    float nstress_pheno;
    if (nFact.pheno > 0.0)
       nstress_pheno = 1.0 - nFact.pheno;
    else
       nstress_pheno = 0.0;
    systemInterface->sendVariable(qd, nstress_pheno);  //()
}

void Nitrogen::get_nstress_photo(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    float nstress_photo;
    if (nFact.photo > 0.0)
       nstress_photo = 1.0 - nFact.photo;
    else
       nstress_photo = 0.0;
    systemInterface->sendVariable(qd, nstress_photo);  //()
}

void Nitrogen::get_nstress_expan(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    float nstress_expan;
    if (nFact.expansion > 0.0)
       nstress_expan = 1.0 - nFact.expansion;
    else
       nstress_expan = 0.0;
    systemInterface->sendVariable(qd, nstress_expan);  //()
}

void Nitrogen::get_nstress_grain(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    float nstress_grain;
    if (nFact.grain > 0.0)
       nstress_grain = 1.0 - nFact.grain;
    else
       nstress_grain = 0.0;
    systemInterface->sendVariable(qd, nstress_grain);  //()
}


