#include "CompositePart.h"
#include "FloretPart.h"
#include <ComponentInterface/ScienceAPI.h>
using namespace std;

FloretPart::FloretPart(ScienceAPI& scienceAPI, plantInterface *p, const string &name)
   : plantPart(scienceAPI, p, name)
   {
   fill_real_array (cX_co2_te_modifier, 0.0, max_table);
   fill_real_array (cY_co2_te_modifier, 0.0, max_table);
   cPartition_option = 0;
   cNum_co2_te_modifier = 0;
   }

void FloretPart::onInit1(protocol::Component *system)
//=======================================================================================
{
   plantPart::onInit1(system);

   system->addGettableVar("pai", gPai, "m^2/m^2", "Floret area index");
   system->addGettableVar("dlt_pai", gDlt_pai, "m^2/m^2", "Delta Floret area index");
   system->addGettableVar("dlt_dm_pot_rue_Floret", dlt.dm_pot_rue, "g/m^2", "Potential dry matter production via photosynthesis");
   system->addGettableVar("dlt_dm_pot_te_Floret", dlt.dm_pot_rue, "g/m^2", "Potential dry matter production via transpiration");

}

void FloretPart::update(void)
//=======================================================================================
{
   plantPart::update();
   gPai += gDlt_pai;
}

void FloretPart::onHarvest(float /* cutting_height */, float remove_fr,
                             vector<string> &dm_type,
                             vector<float> &dlt_crop_dm,
                             vector<float> &dlt_dm_n,
                             vector<float> &dlt_dm_p,
                             vector<float> &fraction_to_residue)
//=======================================================================================
{
   onHarvest_GenericAboveGroundPart(remove_fr, dm_type, dlt_crop_dm, dlt_dm_n, dlt_dm_p, fraction_to_residue);
}

void FloretPart::onFlowering(void)
//=======================================================================================
{
   DMPlantMin = 0.0;
}

// set the minimum weight of part; used for retranslocation to grain
void FloretPart::onStartGrainFill(void)
//=======================================================================================
{
   DMPlantMin = 0.0;
}

void FloretPart::doDmMin(void)
//=======================================================================================
{
   float dm_plant = divide (Green.DM, plant->getPlants(), 0.0);
   DMPlantMin = max (dm_plant * (1.0 - c.trans_frac), DMPlantMin);
}


void FloretPart::doDmDemand(float dlt_dm_supply)
//=======================================================================================
{
   if (cPartition_option == 1)
      doDmDemand1(dlt_dm_supply);
   else if (cPartition_option == 2)
      doDmDemand2(dlt_dm_supply);
   else
      throw std::invalid_argument("invalid template option in FloretPart::doDmDemand");
}

void FloretPart::doDmDemand1(float dlt_dm_supply)
//=======================================================================================
{
   float dlt_dm_supply_by_Floret = 0.0;  // FIXME
   dlt_dm_supply += dlt_dm_supply_by_Floret;
   DMGreenDemand = dlt_dm_supply * fracFloret1() - dlt_dm_supply_by_Floret;
}

void FloretPart::doDmDemand2(float dlt_dm_supply)
//=======================================================================================
{
   float dlt_dm_supply_by_Floret = 0.0;  // FIXME
   dlt_dm_supply += dlt_dm_supply_by_Floret;
   DMGreenDemand = dlt_dm_supply * fracFloret() - dlt_dm_supply_by_Floret;
}

void FloretPart::doDmRetranslocate(float DMAvail, float DMDemandDifferentialTotal)
//=======================================================================================
   {
   Retranslocation.DM += DMAvail * divide (dmDemandDifferential(), DMDemandDifferentialTotal, 0.0);
   }

float FloretPart::dltDmRetranslocateSupply(float DemandDifferential)
//=======================================================================================
   {
   float DMPartPot = Green.DM + Retranslocation.DM;
   float DMPartAvail = DMPartPot - DMPlantMin * plant->getPlants();
   DMPartAvail = l_bound (DMPartAvail, 0.0);
   float DltDmRetransPart = min (DemandDifferential, DMPartAvail);
   Retranslocation.DM = - DltDmRetransPart;          //XXXX this is a bad thing..
   return DltDmRetransPart;
   }

void FloretPart::zeroAllGlobals(void)
//=======================================================================================
{
   plantPart::zeroAllGlobals();
   coverFloret.green = 0.0;
   coverFloret.sen   = 0.0;
   gPai = 0.0;
}

void FloretPart::zeroDeltas(void)
//=======================================================================================
{
   plantPart::zeroDeltas();

   gDlt_pai = 0.0;
}


void FloretPart::readConstants (protocol::Component *system, const string &section)
   {
   plantPart::readConstants(system, section);
   }

void FloretPart::readSpeciesParameters(protocol::Component *system, vector<string> &sections)
//=======================================================================================
{
   plantPart::readSpeciesParameters(system, sections);

   int   numvals;                                // number of values returned

   scienceAPI.read("transp_eff_cf", c.transpEffCf, numvals, 0.0f, 1.0f);
   scienceAPI.read("x_co2_te_modifier", cX_co2_te_modifier, cNum_co2_te_modifier, 0.0f, 1000.0f);
   scienceAPI.read("y_co2_te_modifier", cY_co2_te_modifier, cNum_co2_te_modifier, 0.0, 10.0f);
   scienceAPI.read("extinct_coef_Floret", cExtinctionCoeffFloret, 0.0f, 1.0f);
   scienceAPI.read("spec_Floret_area", cSpec_Floret_area, 0.0f, 100000.0f);
   scienceAPI.read("rue_Floret", cRue_Floret, 0.0f, 3.0f);

   //    plant_transp_eff

   scienceAPI.read("svp_fract", cSvp_fract, 0.0f, 1.0f);
   string partition_option;
   scienceAPI.read("partition_option", partition_option);

   if (partition_option == "1")
      {
      cPartition_option=1;
      scienceAPI.read("frac_Floret", cFrac_Floret, numvals, 0.0, 2.0);
      }
   else if (partition_option == "2" || partition_option == "allometric" || partition_option == "genericxy")
      {
      // NIH - why the hell do we do this??
      cPartition_option=2;
      scienceAPI.read("x_stage_no_partition", cX_stage_no_partition, cNum_stage_no_partition, 0.0f, 20.0f);
      scienceAPI.read("y_frac_Floret", cY_frac_Floret, numvals, 0.0f, 2.0f);
      }
   else
      throw std::invalid_argument("invalid template option in FloretPart::readSpeciesParameters");
}

// Query
float FloretPart::coverTotal(void)
//=======================================================================================
{
   return 1.0 - (1.0 - coverFloret.green) * (1.0 - coverFloret.sen);
}

float FloretPart::coverGreen(void)
//=======================================================================================
{
   return coverFloret.green;
}

float FloretPart::coverSen(void)
//=======================================================================================
{
   return coverFloret.sen;
}

void FloretPart::doCover (PlantSpatial &spatial)

   //===========================================================================
{

   //+  Purpose
   //     Calculate Floret cover

   //+  Changes
   //     02 Feb 2005 JNGH - Programmed and Specified

   //+  Local Variables
   float cover;                // Floret cover in canopy
   float coverA;

   //- Implementation Section ----------------------------------

   if (gPai > 0.0)
      {
      coverA = 1.0 - exp(-cExtinctionCoeffFloret * gPai*spatial.canopyFac());
      cover = divide (coverA, spatial.canopyFac(), 0.0);
      }
   else
      cover = 0.0;

   coverFloret.green = cover;
}

float FloretPart::fracFloret (void)
   //===========================================================================
{

   float g_current_stage = plant->getStageNumber();
   float fracFloret = linear_interp_real(g_current_stage
                                      ,cX_stage_no_partition
                                      ,cY_frac_Floret
                                      ,cNum_stage_no_partition);
   return fracFloret;
}

float FloretPart::fracFloret1 (void)
   //===========================================================================
{
   return cFrac_Floret[(int)plant->getStageNumber()-1];
}

void FloretPart::doProcessBioDemand(void)
   //===========================================================================
{
}

void FloretPart::doBioActual (void)
   //===========================================================================
{
   //       Takes the minimum of biomass production limited by radiation and
   //       biomass production limited by water.

   // use whichever is limiting
   dlt.dm = min (dlt.dm_pot_rue, dlt.dm_pot_te);
}

void FloretPart::calcDlt_Floret_area (void)
   //===========================================================================
{
   gDlt_pai = dltDmGreen() * cSpec_Floret_area * smm2sm;
}

float FloretPart::interceptRadiationGreen (float radiation)    // incident radiation on Florets
    //===========================================================================
{
   //     Calculate Floret total radiation interception and return transmitted radiation

   radiationInterceptedGreen = coverGreen() * radiation;
   return radiationInterceptedGreen;
}

float FloretPart::interceptRadiationTotal (float radiation)    // incident radiation on Florets
    //===========================================================================
{
   //     Calculate Floret total radiation interception and return transmitted radiation

   radiationInterceptedTotal = coverTotal() * radiation;
   return radiationInterceptedTotal;
}

void FloretPart::doDmPotRUE (void )                    // (OUTPUT) potential dry matter (carbohydrate) production (g/m^2)
   //===========================================================================
{
   //       Potential biomass (carbohydrate) production from
   //       photosynthesis (g/m^2).  The effect of factors such
   //       temperature and nutritional status of the plant are
   //       taken into account in the radiation use efficiency.

   double stress_factor = min(min(min(plant->getTempStressPhoto(), plant->getNfactPhoto())
                                  , plant->getOxdefPhoto()), plant->getPfactPhoto());

   dlt.dm_pot_rue = (radiationInterceptedGreen * cRue_Floret) * stress_factor * plant->getCo2ModifierRue();
}


void FloretPart::doTECO2()          // (OUTPUT) transpiration coefficient
   //==========================================================================
{
   cproc_transp_eff_co2_1(plant->getVpd()
                          , c.transpEffCf[(int)plant->getStageNumber()-1]
                          , plant->getCo2ModifierTe()
                          , &transpEff);
}

void FloretPart::doSWDemand(float SWDemandMaxFactor)         //(OUTPUT) crop water demand (mm)
   //===========================================================================
   /*  Purpose
   *       Return crop water demand from soil by the crop (mm) calculated by
   *       dividing biomass production limited by radiation by transpiration efficiency.
   */
{
   // get potential transpiration from potential
   // carbohydrate production and transpiration efficiency

   cproc_sw_demand1 (dlt.dm_pot_rue
                     , transpEff
                     , &sw_demand_te);

       // Capping of sw demand will create an effective TE- recalculate it here
       // In an ideal world this should NOT be changed here - NIH

   float SWDemandMax = SWDemandMaxFactor * coverGreen() ;
   sw_demand = u_bound(sw_demand_te, SWDemandMax);
   transpEff = transpEff * divide(sw_demand_te, sw_demand, 1.0);
}

void FloretPart::doDmPotTE (float swSupply)  //(OUTPUT) potential dry matter production by transpiration (g/m^2)
   //===========================================================================
   //   Calculate the potential biomass production based upon today's water supply.

{
   // potential (supply) by transpiration

   dlt.dm_pot_te = swSupply * transpEff;
}

