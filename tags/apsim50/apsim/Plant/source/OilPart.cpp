
#include <stdio.h>
#include <math.h>
#include <map>
#include <string>
#include <stdexcept>

#include <boost/function.hpp>
#include <boost/bind.hpp>

#include <ComponentInterface/Type.h>
#include <ComponentInterface/ApsimVariant.h>
#include <ComponentInterface/Component.h>
#include <ComponentInterface/dataTypes.h>
#include <ComponentInterface/Messages.h>
#include <ComponentInterface/MessageDataExt.h>
#include <ApsimShared/ApsimComponentData.h>
#include <ApsimShared/FStringExt.h>
#include <general/string_functions.h>

#include "PlantInterface.h"
#include "OilPart.h"
using namespace std;

void fruitOilPart::doInit (PlantComponent *systemInterface, PlantPhenology *plantPhenology)
   // ====================================================================
{
   parentPlant = systemInterface;
   phenology = plantPhenology;
}

void fruitOilPart::doRegistrations(protocol::Component *system)
   //===========================================================================
{
   plantPart::doRegistrations(system);
   system->addGettableVar("dlt_dm_oil_conv",gDlt_dm_oil_conv,"g/m^2", "change in oil via ??");
   system->addGettableVar("dlt_dm_oil_conv_retrans", dmOil_conv_retranslocate, "g/m^2", "change in oil via retranslocation");
   system->addGettableVar("grain_oil_conc", cGrain_oil_conc, "%", "??");
}

float fruitOilPart::grainEnergy(void) const {return gGrain_energy;}

void fruitOilPart::zeroAllGlobals(void)
{
   plantPart::zeroAllGlobals();
   cCarbo_oil_conv_ratio  = 0.0;
   cGrain_oil_conc  = 0.0;
   gGrain_energy = 0.0;
   dmOil_conv_retranslocate = 0.0;
}

void fruitOilPart::zeroDeltas(void)
{
   plantPart::zeroDeltas();
   gDlt_dm_oil_conv = 0.0;
}


void fruitOilPart::update(void)
{
   plantPart::update();
   plantPart::updateDm();
   plantPart::updateN();
   plantPart::updateP();
}

void fruitOilPart::onHarvest(float /* cutting_height */, float remove_fr,
                             vector<string> &dm_type,
                             vector<float> &dlt_crop_dm,
                             vector<float> &dlt_dm_n,
                             vector<float> &dlt_dm_p,
                             vector<float> &fraction_to_residue)
{
   float fractToResidue = 0.0;

   // biomass is removed
   float retain_fr_green = 0.0;
   float retain_fr_sen   = 0.0;
   float retain_fr_dead  = 0.0;

   float chop_fr_green = 1.0 - retain_fr_green;
   float chop_fr_dead  = 1.0 - retain_fr_dead;
   float chop_fr_sen   = 1.0 - retain_fr_sen;

   float dlt_dm_harvest = DMDead     * chop_fr_dead
                        + DMGreen    * chop_fr_green
                        + DMSenesced * chop_fr_sen;

   float dlt_n_harvest = NDead     * chop_fr_dead
                        + NGreen    * chop_fr_green
                        + NSenesced * chop_fr_sen;

   float dlt_p_harvest = PDead  * chop_fr_dead
                        + PGreen * chop_fr_green
                        + PSen   * chop_fr_sen;

   DMDead     *= retain_fr_dead;
   DMSenesced *= retain_fr_sen;
   DMGreen    *= retain_fr_green;


   NDead     *= retain_fr_dead;
   NSenesced *= retain_fr_sen;
   NGreen    *= retain_fr_green;

   PDead  *= retain_fr_dead;
   PSen   *= retain_fr_sen;
   PGreen *= retain_fr_green;


   dm_type.push_back(c.name);
   fraction_to_residue.push_back(fractToResidue);
   dlt_crop_dm.push_back (dlt_dm_harvest * gm2kg/sm2ha);
   dlt_dm_n.push_back    (dlt_n_harvest  * gm2kg/sm2ha);
   dlt_dm_p.push_back    (dlt_p_harvest  * gm2kg/sm2ha);

}

void fruitOilPart::onKillStem(void)
{
   DMDead += DMGreen + DMSenesced;
   DMGreen = 0.0;
   DMSenesced = 0.0;

   NDead += NGreen + NSenesced;
   NGreen = 0.0;
   NSenesced = 0.0;

   PDead += PGreen + PSen;
   PGreen = 0.0;
   PSen = 0.0;
}

void fruitOilPart::onFlowering(void)
{  // do nothing
}

// set the minimum weight of part; used for retranslocation to grain
void fruitOilPart::onStartGrainFill(void)
{  // do nothing
}

void fruitOilPart::doBioGrainOil (void)    // for seed energy content (>= 1.0)
   //===========================================================================
{
   //       Calculate grain oil factors

   gGrain_energy = 1.0 + cGrain_oil_conc * (cCarbo_oil_conv_ratio - 1.0);
   bound_check_real_var (parentPlant, gGrain_energy, 1.0, 2.0, "grain_energy");
}

float fruitOilPart::energyAdjustHI (float harvestIndex)
   //===========================================================================
{
   return divide (1.0
                 , 1.0 + harvestIndex*(gGrain_energy - 1.0)
                 , 0.0);
}

float fruitOilPart::energyAdjustDM (float DM)
   //===========================================================================
{
   return DM * gGrain_energy;
}

float fruitOilPart::doDmDemand (float dmDemand)
   //===========================================================================
{
   return divide (dmDemand, gGrain_energy, 0.0);
}

float fruitOilPart::dltDmGreenUptake(void)
//=======================================================================================
   {
   return (dlt.dm_green + gDlt_dm_oil_conv);
   }

float fruitOilPart::dltDmGreenRetransUptake(void)
//=======================================================================================
   {
   return (dlt.dm_green_retrans + dmOil_conv_retranslocate);
   }

void fruitOilPart::doDMDemand (float dlt_dm_grain_demand)
//     ===========================================================
{
    float dltDmOil = divide (dlt_dm_grain_demand, gGrain_energy, 0.0) * cGrain_oil_conc;
    float dltDmOilConversion =  divide (dlt_dm_grain_demand, gGrain_energy, 0.0) * (gGrain_energy - 1.0);
    DMGreenDemand = dltDmOil + dltDmOilConversion;
}

void fruitOilPart::doDmPartition(float DMAvail, float DMDemandTotal)
//=======================================================================================
   {
   float dltDM = DMAvail * divide (DMGreenDemand, DMDemandTotal, 0.0);
   dlt.dm_green = divide (dltDM, cCarbo_oil_conv_ratio, 0.0);
   gDlt_dm_oil_conv = dltDM - dlt.dm_green;
   }

void fruitOilPart::doDmRetranslocate(float DMAvail, float DMDemandDifferentialTotal)
//=======================================================================================
   {
   float dltDM = DMAvail * divide (dmDemandDifferential(), DMDemandDifferentialTotal, 0.0);
   dlt.dm_green_retrans = divide (dltDM, cCarbo_oil_conv_ratio, 0.0);
   dmOil_conv_retranslocate = dltDM - dlt.dm_green_retrans;
   }

float fruitOilPart::dmDemandDifferential(void)
//=======================================================================================
   {
   return dmGreenDemand() - dltDmGreenUptake();
   }

void fruitOilPart::readSpeciesParameters(protocol::Component *system, vector<string> &sections)
   //===========================================================================
{
   plantPart::readSpeciesParameters(system, sections);

   system->readParameter (sections
                          ,"carbo_oil_conv_ratio"//, "()"
                          , cCarbo_oil_conv_ratio
                          , 0.0, 20.0);

   system->readParameter (sections
                          ,"grain_oil_conc"//, "()"
                          , cGrain_oil_conc
                          , 0.0, 1.0);

}