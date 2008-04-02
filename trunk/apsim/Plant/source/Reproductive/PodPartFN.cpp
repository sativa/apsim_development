#include "StdPlant.h"

#include "GrainPart.h"
#include "Co2Modifier.h"
#include "FruitCohortFN.h"
#include "PodPartFN.h"
using namespace std;

fruitPodPartFN::fruitPodPartFN(ScienceAPI& scienceAPI, plantInterface *p, FruitCohort *g, const string &name)
   : fruitPodPart(scienceAPI, p, g, name)
   {
   }

void fruitPodPartFN::onInit1(protocol::Component *system)
//=======================================================================================
{
   fruitPodPart::onInit1(system);


}

void fruitPodPartFN::prepare(void)
//=======================================================================================
   {
   fruitPodPart::prepare();

   }

void fruitPodPartFN::update(void)
//=======================================================================================
{
   fruitPodPart::update();
}

void fruitPodPartFN::onFlowering(void)
//=======================================================================================
{
   fruitPodPart::onFlowering();
}

// set the minimum weight of part; used for retranslocation to grain
void fruitPodPartFN::onStartGrainFill(void)
//=======================================================================================
{
   fruitPodPart::onStartGrainFill();
}

void fruitPodPartFN::doDmMin(void)
//=======================================================================================
{
   float dm_plant = divide (Green.DM(), plant->getPlants(), 0.0);
   DMPlantMin = max (dm_plant * (1.0 - c.trans_frac), DMPlantMin);
}


void fruitPodPartFN::doDmDemand(float dlt_dm_supply)
//=======================================================================================
{
   float dlt_dm_supply_by_pod = 0.0;  // FIXME
   dlt_dm_supply += dlt_dm_supply_by_pod;

   float dm_grain_demand = myParent->dltDmPotentialGrain();

   if (dm_grain_demand <= 0.0)
   {
      // we are in flowering phase
      DMGreenDemand = dlt_dm_supply * fracPod->value(myParent->getStageNumber())  - dlt_dm_supply_by_pod;
   }
   else
   {
      DMGreenDemand = dm_grain_demand * fracPod->value(myParent->getStageNumber())  - dlt_dm_supply_by_pod;

//      float dltDmPodMax = myParent->dltDmGrainMax() * fracPod->value(myParent->getStageNumber();
//      DMGreenDemand = bound (DMGreenDemand, 0.0, dltDmPodMax);
   }
}

void fruitPodPartFN::doDmRetranslocate(float DMAvail, float DMDemandDifferentialTotal)
//=======================================================================================
   {
   Retranslocation = Retranslocation + Biomass(DMAvail * divide (dmDemandDifferential(), DMDemandDifferentialTotal, 0.0),
                                               Retranslocation.N(), Retranslocation.P());
   }

float fruitPodPartFN::dltDmRetranslocateSupply(float DemandDifferential)
//=======================================================================================
   {
   float DMPartPot = Green.DM() + Retranslocation.DM();
   float DMPartAvail = DMPartPot - DMPlantMin * plant->getPlants();
   DMPartAvail = l_bound (DMPartAvail, 0.0);
   float DltDmRetransPart = min (DemandDifferential, DMPartAvail);
   Retranslocation = Retranslocation - Biomass(DltDmRetransPart, 0, 0);   //XXXX this is a bad thing..
   return DltDmRetransPart;
   }

void fruitPodPartFN::zeroAllGlobals(void)
//=======================================================================================
{
   fruitPodPart::zeroAllGlobals();
}

void fruitPodPartFN::zeroDeltas(void)
//=======================================================================================
{
   fruitPodPart::zeroDeltas();
}


void fruitPodPartFN::readSpeciesParameters(protocol::Component *system, vector<string> &sections)
//=======================================================================================
{
   fruitPodPart::readSpeciesParameters(system, sections);

}

void fruitPodPartFN::doProcessBioDemand(void)
   //===========================================================================
{
}

void fruitPodPartFN::doTECO2()          // (OUTPUT) transpiration coefficient
   //==========================================================================
{
   cproc_transp_eff_co2_1(plant->getVpd()
                          , TECoeff.value(myParent->getStageNumber())
                          , co2Modifier->te()
                          , &transpEff);
}

