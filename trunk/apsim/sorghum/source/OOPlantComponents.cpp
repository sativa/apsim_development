//---------------------------------------------------------------------------

#pragma hdrstop

#include "OOPlantComponents.h"
#include "Utilities.h"

//---------------------------------------------------------------------------

#pragma package(smart_init)
//---------------------------------------------------------------------------
PlantPart::PlantPart(void)
   {
   initialize();
   }
//---------------------------------------------------------------------------

void PlantPart::initialize(void)
   {
   //Init Variables
   stage = 0.0;
   partNo = 0;


   // biomass
   dmGreen = 0.0;
   dltDmGreen = 0.0;
   dmSenesced = 0.0;
   dltDmSenesced = 0.0;
   dmDead = 0.0;
   dmRetranslocate = 0.0;
   dltDetDmSenesced = 0.0;
   dltDetDmDead = 0.0;


   // nitrogen
   nGreen = 0.0;
   dltNGreen = 0.0;
   dltNRetranslocate = 0.0;
   nSenesced = 0.0;
   nDead = 0.0;
   dltDetNSenesced = 0.0;
   dltDetNDead = 0.0;
   nConc = 0.0;
   nDemand = 0.0;
   dltNSenesced = 0.0;
   dltDetNSenesced = 0.0;

   // phosphorus
   pGreen = 0.0;
   pDemand = 0.0;
   pDead = 0.0;
   pSenesced = 0.0;
   dltPGreen = 0.0;
   dltPSenesced = 0.0;
   dltPDetached = 0.0;
   dltPRetranslocate = 0.0;
   }

//---------------------------------------------------------------------------
void PlantPart::dmDetachment(vector<float> senDetachFrac,
      vector<float> deadDetachFrac)
   {
   calcPartFractionDelta (partNo, senDetachFrac, dmSenesced, dltDetDmSenesced);
   calcPartFractionDelta (partNo, deadDetachFrac, dmDead, dltDetDmDead);
   }
//---------------------------------------------------------------------------
void PlantPart::NDetachment(vector<float> senDetachFrac,
      vector<float> deadDetachFrac)
   {
   calcPartFractionDelta (partNo, senDetachFrac, nSenesced, dltDetNSenesced);
   calcPartFractionDelta (partNo, deadDetachFrac, nDead, dltDetNDead);
   }
//---------------------------------------------------------------------------
void PlantPart::resetDailyVars(void)
   {
   dltDmGreen = 0.0;
   dltDmSenesced = 0.0;
   dmRetranslocate = 0.0;
   dltDetDmSenesced = 0.0;
   dltDetDmDead = 0.0;
   dltNGreen = 0.0;
   dltNSenesced = 0.0;
   dltNRetranslocate = 0.0;
   dltDetNSenesced = 0.0;
   dltDetNDead = 0.0;

   dltPSenesced = 0.0;
   dltPDetached = 0.0;
   dltPGreen = 0.0;
   dltPRetranslocate = 0.0;
   }
//---------------------------------------------------------------------------
void PlantPart::calcDltPSenesced(void)
   {
   float p_conc_green = divide (pGreen,dmGreen,0.0);
   float p_conc_sen = pSenTable.value(stage);
   dltPSenesced = Min(p_conc_green,p_conc_sen) * dltDmSenesced;
   }
//---------------------------------------------------------------------------
void PlantPart::calcDltPDetached(void)
   {
   float sen_detach_frac = divide (dltDetDmSenesced,dmSenesced,0.0);
   dltPDetached = dltPSenesced * sen_detach_frac;
   }
//---------------------------------------------------------------------------
void PlantPart::updateP(void)
   {
   pGreen = pGreen + dltPGreen - dltPSenesced + dltPRetranslocate;
   pSenesced = pSenesced + dltPSenesced - dltPDetached;
   pConc = divide(pGreen,dmGreen,0);

   }
//---------------------------------------------------------------------------


