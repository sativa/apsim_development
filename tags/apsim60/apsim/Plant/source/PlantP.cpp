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
      pStress->doPlantPStress(myParts);
      }
}


// ===============================
void Plant::doPPartition (void)
{
  if (pStress->isPhosphorusAware())
  {
      vector<float> values;               // Scratch area
      float p_uptake;

      if (id.layered_p_uptake != 0)
      {
          parent->getVariable(id.layered_p_uptake, values, 0.0, 100.0);
          float sumValue = 0.0;
          for (unsigned int i = 0; i < values.size(); i++)
            sumValue += values[i];

          p_uptake = sumValue * kg2gm/ha2sm;
      }
      else
          p_uptake = plant.pDemand();

      plant.doPPartition(p_uptake, plant.pDemand());
  }
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

void Plant::doPRetranslocate (void)
//      Calculate retranslocation between pools
{
  if (pStress->isPhosphorusAware())
      plant.doPRetranslocate(plant.pRetransSupply(), plant.pRetransDemand());

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
