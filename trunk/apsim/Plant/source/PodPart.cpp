#include "PodPart.h"
using namespace std;

void fruitPodPart::doRegistrations(protocol::Component *system)
//=======================================================================================
{
   plantPart::doRegistrations(system);

   system->addGettableVar("pai", gPai, "m^2/m^2", "Pod area index");
   system->addGettableVar("dlt_pai", gDlt_pai, "m^2/m^2", "Delta Pod area index");
   system->addGettableVar("dlt_dm_pot_rue_pod", gDlt_dm_pot_rue, "g/m^2", "Potential dry matter production via photosynthesis");
   system->addGettableVar("dlt_dm_pot_te_pod", gDlt_dm_pot_te, "g/m^2", "Potential dry matter production via transpiration");

}

void fruitPodPart::update(void)
//=======================================================================================
{
   plantPart::update();
   plantPart::updateDm();
   plantPart::updateN();
   plantPart::updateP();
   gPai += gDlt_pai;
}

void fruitPodPart::onHarvest(float /* cutting_height */, float remove_fr,
                             vector<string> &dm_type,
                             vector<float> &dlt_crop_dm,
                             vector<float> &dlt_dm_n,
                             vector<float> &dlt_dm_p,
                             vector<float> &fraction_to_residue)
//=======================================================================================
{
   float fractToResidue = 1.0 - remove_fr;

   float dm_init = u_bound (plantPart::c.dm_init * plant->getPlants(), plantPart::DMGreen);
   float n_init  = u_bound (             dm_init * plantPart::c.n_init_conc, plantPart::NGreen);
   float p_init  = u_bound (             dm_init * plantPart::c.p_init_conc, plantPart::PGreen);

   float retain_fr_green = divide(dm_init, DMGreen, 0.0);
   float retain_fr_dead  = 0.0;
   float retain_fr_sen   = 0.0;

   float dlt_dm_harvest = DMDead + DMGreen + DMSenesced - dm_init;
   float dlt_n_harvest  = NDead  + NGreen  + NSenesced  - n_init;
   float dlt_p_harvest  = PDead  + PGreen  + PSen       - p_init;

   DMDead     *= retain_fr_dead;
   DMSenesced *= retain_fr_sen;
   DMGreen    *= retain_fr_green;

   NDead     *= retain_fr_dead;
   NSenesced *= retain_fr_sen;
   NGreen    = n_init;

   PDead  *= retain_fr_dead;
   PSen   *= retain_fr_sen;
   PGreen  = p_init;

   dm_type.push_back(c.name);
   fraction_to_residue.push_back(fractToResidue);
   dlt_crop_dm.push_back (dlt_dm_harvest * gm2kg/sm2ha);
   dlt_dm_n.push_back    (dlt_n_harvest  * gm2kg/sm2ha);
   dlt_dm_p.push_back    (dlt_p_harvest  * gm2kg/sm2ha);
}

void fruitPodPart::onKillStem(void)
//=======================================================================================
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

void fruitPodPart::onFlowering(void)
//=======================================================================================
{
   DMPlantMin = 0.0;
}

// set the minimum weight of part; used for retranslocation to grain
void fruitPodPart::onStartGrainFill(void)
//=======================================================================================
{
   DMPlantMin = 0.0;
}

void fruitPodPart::doDmMin(void)
//=======================================================================================
{
   float dm_plant = divide (DMGreen, plant->getPlants(), 0.0);
   DMPlantMin = max (dm_plant * (1.0 - c.trans_frac), DMPlantMin);
}

void fruitPodPart::doDmDemand(float dm_grain_demand, float dlt_dm_supply)
//=======================================================================================
{
   if (cPartition_option == 1)
      doDmDemand1(dm_grain_demand, dlt_dm_supply);
   else if (cPartition_option == 2)
      doDmDemand2(dm_grain_demand, dlt_dm_supply);
   else
      throw std::invalid_argument("invalid template option in fruitPodPart::doDmDemand");
}

void fruitPodPart::doDmDemand1(float dm_grain_demand, float dlt_dm_supply)
//=======================================================================================
{
   float dlt_dm_supply_by_pod = 0.0;  // FIXME
   dlt_dm_supply += dlt_dm_supply_by_pod;

   if (dm_grain_demand > 0.0)
      DMGreenDemand = dm_grain_demand * fracPod1() - dlt_dm_supply_by_pod;
   else
      DMGreenDemand = dlt_dm_supply * fracPod1() - dlt_dm_supply_by_pod;
}

void fruitPodPart::doDmDemand2(float dm_grain_demand, float dlt_dm_supply)
//=======================================================================================
{
   float dlt_dm_supply_by_pod = 0.0;  // FIXME
   dlt_dm_supply += dlt_dm_supply_by_pod;

   if (dm_grain_demand > 0.0)
      DMGreenDemand = dm_grain_demand * fracPod() - dlt_dm_supply_by_pod;
   else
      DMGreenDemand = dlt_dm_supply * fracPod() - dlt_dm_supply_by_pod;
}

void fruitPodPart::doDmRetranslocate(float DMAvail, float DMDemandDifferentialTotal)
//=======================================================================================
   {
   dlt.dm_green_retrans += DMAvail * divide (dmDemandDifferential(), DMDemandDifferentialTotal, 0.0);
   }

float fruitPodPart::dltDmRetranslocateSupply(float DemandDifferential)
//=======================================================================================
   {
   float DMPartPot = DMGreen + dlt.dm_green_retrans;
   float DMPartAvail = DMPartPot - DMPlantMin * plant->getPlants();
   DMPartAvail = l_bound (DMPartAvail, 0.0);
   float DltDmRetransPart = min (DemandDifferential, DMPartAvail);
   dlt.dm_green_retrans = - DltDmRetransPart;
   return DltDmRetransPart;
   }

void fruitPodPart::zeroAllGlobals(void)
//=======================================================================================
{
   plantPart::zeroAllGlobals();

   coverPod.green = 0.0;
   coverPod.sen   = 0.0;
   coverPod.dead  = 0.0;
   gTranspEff = 0.0;
   gPai = 0.0;
   fill_real_array (cTransp_eff_cf, 0.0, max_table);
   fill_real_array (cX_co2_te_modifier, 0.0, max_table);
   fill_real_array (cY_co2_te_modifier, 0.0, max_table);
   cNum_co2_te_modifier = 0;
}

void fruitPodPart::zeroDeltas(void)
//=======================================================================================
{
   plantPart::zeroDeltas();

   gDlt_pai = 0.0;
   gDlt_dm = 0.0;
   gDlt_dm_pot_rue = 0.0;
   gDlt_dm_pot_te = 0.0;
}


void fruitPodPart::readSpeciesParameters(protocol::Component *system, vector<string> &sections)
//=======================================================================================
{
   plantPart::readSpeciesParameters(system, sections);

   int   numvals;                                // number of values returned

    system->readParameter (sections,
                       "partition_option"//, "()"
                      , cPartition_option
                      , 1, 3);

   system->readParameter (sections,
                          "transp_eff_cf"//, "(kpa)"
                          , cTransp_eff_cf, numvals
                          , 0.0, 1.0);

   system->readParameter (sections
                          , "x_co2_te_modifier"//, "()"
                          , cX_co2_te_modifier, cNum_co2_te_modifier
                          , 0.0, 1000.0);

   system->readParameter (sections
                          , "y_co2_te_modifier"//, "()"
                          , cY_co2_te_modifier, cNum_co2_te_modifier
                          , 0.0, 10.0);

   system->readParameter (sections
                          ,"extinct_coef_pod"//, "()"
                          , cExtinctionCoeffPod
                          , 0.0, 1.0);

   system->readParameter (sections
                          ,"spec_pod_area"//, "()"
                          , cSpec_pod_area
                          , 0.0, 100000.0);

   system->readParameter (sections
                          ,"rue_pod"//, "()"
                          , cRue_pod
                          , 0.0, 3.0);

   //    plant_transp_eff

   system->readParameter (sections
                          ,"svp_fract"//, "()"
                          , cSvp_fract
                          , 0.0, 1.0);

   int cPartition_option = 0;
   system->readParameter (sections,
                          "partition_option"//, "()"
                          , cPartition_option
                          , 1, 3);

   if (cPartition_option==1 )
      system->readParameter (sections
                             ,"frac_pod"//, "()"
                             , cFrac_pod, numvals
                             , 0.0, 2.0);

   else if (cPartition_option==2)
      {
      system->readParameter (sections
                             ,"x_stage_no_partition"//, "()"
                             , cX_stage_no_partition
                             , cNum_stage_no_partition
                             , 0.0, 20.0);

      system->readParameter (sections
                             ,"y_frac_pod"//, "()"
                             , cY_frac_pod, numvals
                             , 0.0, 2.0);
      }
}

// Query
float fruitPodPart::coverTotal(void)
//=======================================================================================
{
   return 1.0 - (1.0 - coverPod.green) * (1.0 - coverPod.sen) * (1.0 - coverPod.dead);
}

float fruitPodPart::coverGreen(void)
//=======================================================================================
{
   return coverPod.green;
}

float fruitPodPart::coverDead(void)
//=======================================================================================
{
   return coverPod.dead;
}

float fruitPodPart::coverSen(void)
//=======================================================================================
{
   return coverPod.sen;
}

float fruitPodPart::calcCover (float canopy_fac)
   //===========================================================================
{

   //+  Purpose
   //     Calculate pod cover

   //+  Changes
   //     02 Feb 2005 JNGH - Programmed and Specified

   //+  Local Variables
   float cover;                // pod cover in canopy
   float coverA;

   //- Implementation Section ----------------------------------
   if (gPai > 0.0)
      {
      coverA = 1.0 - exp(-cExtinctionCoeffPod * gPai*canopy_fac);
      cover = divide (coverA, canopy_fac, 0.0);
      }
   else
      cover = 0.0;

   coverPod.green = cover;
   return cover;
}

float fruitPodPart::dltDmPotTe(void) {return gDlt_dm_pot_te;}
float fruitPodPart::dltDmPotRue(void) {return gDlt_dm_pot_rue;}

float fruitPodPart::fracPod (void)
   //===========================================================================
{

   float g_current_stage = plant->getStageNumber();
   float fracPod = linear_interp_real(g_current_stage
                                      ,cX_stage_no_partition
                                      ,cY_frac_pod
                                      ,cNum_stage_no_partition);
   return fracPod;
}

float fruitPodPart::fracPod1 (void)
   //===========================================================================
{
   return cFrac_pod[(int)plant->getStageNumber()-1];
}

void fruitPodPart::doProcessBioDemand(void)
   //===========================================================================
{
   doDmPotTE ();
   return;
}

void fruitPodPart::doBioActual (void)                                             //FIXME
   //===========================================================================
{
   //       Takes the minimum of biomass production limited by radiation and
   //       biomass production limited by water.

   // use whichever is limiting
   gDlt_dm = min (gDlt_dm_pot_rue, gDlt_dm_pot_te);
}

void fruitPodPart::calcDlt_pod_area (void)
   //===========================================================================
{
   gDlt_pai = dltDmGreen() * cSpec_pod_area * smm2sm;
}

float fruitPodPart::interceptRadiation (float radiation)    // incident radiation on pods
    //===========================================================================
{
   //     Calculate pod total radiation interception and return transmitted radiation

   float radiationIntercepted = coverTotal() * radiation;
   return radiation - radiationIntercepted;
}

void fruitPodPart::doDmPotRUE (double  radn_int_pod)                    // (OUTPUT) potential dry matter (carbohydrate) production (g/m^2)
   //===========================================================================
{
   //       Potential biomass (carbohydrate) production from
   //       photosynthesis (g/m^2).  The effect of factors such
   //       temperature and nutritional status of the plant are
   //       taken into account in the radiation use efficiency.

   double stress_factor = min(min(min(plant->getTempStressPhoto(), plant->getNfactPhoto())
                                  , plant->getOxdefPhoto()), plant->getPfactPhoto());

   gDlt_dm_pot_rue = (radn_int_pod * cRue_pod) * stress_factor * plant->getCo2ModifierRue();
}


void fruitPodPart::doTECO2()          // (OUTPUT) transpiration coefficient
   //==========================================================================
{
   cproc_transp_eff_co2_1(plant->getVpd()
                          , cTransp_eff_cf[(int)plant->getStageNumber()-1]
                          , plant->getCo2ModifierTe()
                          , &gTranspEff);
}

float fruitPodPart::SWDemand(void)         //(OUTPUT) crop water demand (mm)
   //===========================================================================
   /*  Purpose
   *       Return crop water demand from soil by the crop (mm) calculated by
   *       dividing biomass production limited by radiation by transpiration efficiency.
   */
{
   // get potential transpiration from potential
   // carbohydrate production and transpiration efficiency
   float sw_demand = 0.0;
   cproc_sw_demand1 (gDlt_dm_pot_rue
                     , gTranspEff
                     , &sw_demand);
   return sw_demand;
}

void fruitPodPart::doDmPotTE (void)  //(OUTPUT) potential dry matter production by transpiration (g/m^2)
   //===========================================================================
   //   Calculate the potential biomass production based upon today's water supply.

{
   // potential (supply) by transpiration

   gDlt_dm_pot_te = plant->getWaterSupplyPod() * gTranspEff;

   // Capping of sw demand will create an effective TE- recalculate it here       //FIXME
   // In an ideal world this should NOT be changed here - NIH
   //       g.transp_eff = g.transp_eff * divide(g.sw_demand_te,g.sw_demand, 1.0);
   //       g.swDemandTEFruit = g.swDemandTEFruit * divide(g.sw_demand,g.sw_demand_te, 1.0);          // Hack to correct TE for fruit
}

