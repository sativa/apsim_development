
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
#include <ComponentInterface/datatypes.h>
#include <ComponentInterface/Messages.h>
#include <ComponentInterface/MessageDataExt.h>
#include <ApsimShared/ApsimComponentData.h>
#include <ApsimShared/FStringExt.h>
#include <general/string_functions.h>

#include "PlantInterface.h"
#include "OilPart.h"
using namespace std;

void fruitOilPart::doRegistrations(protocol::Component *system)
//===========================================================================
   {
   plantPart::doRegistrations(system);
   system->addGettableVar("dlt_dm_oil_conv",gDlt_dm_oil_conv,"g/m^2", "change in oil via ??");
   system->addGettableVar("dlt_dm_oil_conv_retrans", dmOil_conv_retranslocate, "g/m^2", "change in oil via retranslocation");
   system->addGettableVar("grain_oil_conc", cGrain_oil_conc, "%", "??");
   }

float fruitOilPart::grainEnergy(void) const {return gGrain_energy;}
//=======================================================================================

void fruitOilPart::zeroAllGlobals(void)
//=======================================================================================
// Zero all data
   {
   plantPart::zeroAllGlobals();
   cCarbo_oil_conv_ratio  = 0.0;
   cGrain_oil_conc  = 0.0;
   gGrain_energy = 0.0;
   dmOil_conv_retranslocate = 0.0;
   }

void fruitOilPart::zeroDeltas(void)
//=======================================================================================
// Zero daily deltas
   {
   plantPart::zeroDeltas();
   gDlt_dm_oil_conv = 0.0;
   }


void fruitOilPart::onHarvest(float /* cutting_height */, float /*remove_fr*/,
                             vector<string> &dm_type,
                             vector<float> &dlt_crop_dm,
                             vector<float> &dlt_dm_n,
                             vector<float> &dlt_dm_p,
                             vector<float> &fraction_to_residue)
//=======================================================================================
// Event Handler for harvesting event
   {
   // biomass is removed, nothing is sent to surface residues..
   dm_type.push_back (c.name);
   fraction_to_residue.push_back (0.0);
   dlt_crop_dm.push_back ((DMDead+DMGreen+DMSenesced) * gm2kg/sm2ha);
   dlt_dm_n.push_back    ((NDead+NGreen+NSenesced)  * gm2kg/sm2ha);
   dlt_dm_p.push_back    ((PDead+PGreen+PSen)  * gm2kg/sm2ha);

   DMDead     = 0.0;
   DMSenesced = 0.0;
   DMGreen    = 0.0;

   NDead     = 0.0;
   NSenesced = 0.0;
   NGreen    = 0.0;

   PDead     = 0.0;
   PSen      = 0.0;
   PGreen    = 0.0;
   }


void fruitOilPart::onKillStem(void)
//=======================================================================================
// Event Handler for KillStem event
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
//=======================================================================================
// Event Handler for Flowering event
   {  // do nothing
   }

void fruitOilPart::onStartGrainFill(void)
//=======================================================================================
// Event Handler for the start of grain filling event
   {  // do nothing
   }

void fruitOilPart::doBioGrainOil (void)    // for seed energy content (>= 1.0)
//=======================================================================================
//  Calculate grain oil factors
   {
   gGrain_energy = 1.0 + cGrain_oil_conc * (cCarbo_oil_conv_ratio - 1.0);
   bound_check_real_var (plant, gGrain_energy, 1.0, 2.0, "grain_energy");
   }

float fruitOilPart::energyAdjustHI (float harvestIndex)
//=======================================================================================
// Returns an adjustment factor for converting biomass to account for oil (high c) content
   {
   return divide (1.0
                 , 1.0 + harvestIndex*(gGrain_energy - 1.0)
                 , 0.0);
   }

float fruitOilPart::energyAdjustDM (float DM)
//=======================================================================================
   {
   return DM * gGrain_energy;
   }

float fruitOilPart::calcDmDemand (float dmDemand)
//=======================================================================================
   {
   return divide (dmDemand, gGrain_energy, 0.0);
   }

float fruitOilPart::dltDmGreen(void) const
//=======================================================================================
   {
   return (dlt.dm_green + gDlt_dm_oil_conv);
   }

float fruitOilPart::dltDmGreenRetransUptake(void) const
//=======================================================================================
   {
   return (dlt.dm_green_retrans + dmOil_conv_retranslocate);
   }

void fruitOilPart::doDMDemand (float dlt_dm_grain_demand)                                                    //remove
//=======================================================================================
   {                                                                                                           //remove
   float dltDmOil = divide (dlt_dm_grain_demand, gGrain_energy, 0.0) * cGrain_oil_conc;                    //remove
   float dltDmOilConversion =  divide (dlt_dm_grain_demand, gGrain_energy, 0.0) * (gGrain_energy - 1.0);   //remove
   DMGreenDemand = dltDmOil + dltDmOilConversion;                                                          //remove
   }                                                                                                           //remove

void fruitOilPart::doDMDemandGrain (float dlt_dm_grain_demand)
//=======================================================================================
   {
   float dltDmOil = divide (dlt_dm_grain_demand, gGrain_energy, 0.0) * cGrain_oil_conc;
   float dltDmOilConversion =  divide (dlt_dm_grain_demand, gGrain_energy, 0.0) * (gGrain_energy - 1.0);
   DMGreenDemand = dltDmOil + dltDmOilConversion;
   }

float fruitOilPart::giveDmGreen(float delta)
//=======================================================================================
   {
   float d = divide (delta, cCarbo_oil_conv_ratio, 0.0);
   dlt.dm_green += d;
   gDlt_dm_oil_conv = delta - d;
   return delta;
   }

void fruitOilPart::doDmRetranslocate(float DMAvail, float DMDemandDifferentialTotal)
//=======================================================================================
   {
   float dltDM = DMAvail * divide (dmDemandDifferential(), DMDemandDifferentialTotal, 0.0);
   dlt.dm_green_retrans = divide (dltDM, cCarbo_oil_conv_ratio, 0.0);
   dmOil_conv_retranslocate = dltDM - dlt.dm_green_retrans;
   }

float fruitOilPart::dmDemandDifferential(void) const
//=======================================================================================
   {
   return dmGreenDemand() - dltDmGreen();
   }

void fruitOilPart::readSpeciesParameters(protocol::Component *system, vector<string> &sections)
//=======================================================================================
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


