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





// ===============================

void Plant::prepare_p(void)
{
   if (pStress->isPhosphorusAware())
      {
      plant.doPDemand();
      pStress->PlantP_Stress(myParts);
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
      if (pStress->isPhosphorusAware())
      {
         pStress->read_p_constants();

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

   if (pStress->isPhosphorusAware())
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

   if (pStress->isPhosphorusAware())
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

