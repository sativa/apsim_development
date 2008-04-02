#include "StdPlant.h"

#include "GrainPart.h"
#include "PodPart.h"
#include "Co2Modifier.h"
#include "FruitCohort.h"
using namespace std;
// ================================ AREA =================================================
// This class only has functionality for pods. It needs extra methods and data members to be used for leaf and other organs.

PlantPartArea::PlantPartArea(ScienceAPI& api, plantInterface *p, const string &name)
   : plant(p)
   , myName(name)
   , scienceAPI(api)
   {
   }

void PlantPartArea::onInit1(protocol::Component *system)
//=======================================================================================
{
   system->addGettableVar((myName.substr(0, 1) + "ai").c_str(), partAI, "m^2/m^2", (myName + " area index").c_str());
   system->addGettableVar(("dlt_" + myName.substr(0, 1) +"ai").c_str(), dlt_partAI, "m^2/m^2", ("Delta " + myName + " area index").c_str());
}

void PlantPartArea::update(void)
//=======================================================================================
{
   partAI += dlt_partAI;
}

void PlantPartArea::onHarvest(float /* cutting_height */, float /*remove_fr*/,
                             vector<string> &/*dm_type*/,
                             vector<float> &/*dlt_crop_dm*/,
                             vector<float> &/*dlt_dm_n*/,
                             vector<float> &/*dlt_dm_p*/,
                             vector<float> &/*fraction_to_residue*/)
//=======================================================================================
{
//   onHarvest_GenericAboveGroundPart(remove_fr, dm_type, dlt_crop_dm, dlt_dm_n, dlt_dm_p, fraction_to_residue);
}
void PlantPartArea::zeroAllGlobals(void)
//=======================================================================================
{
   cover.green = 0.0;
   cover.sen   = 0.0;
   partAI = 0.0;
}

void PlantPartArea::zeroDeltas(void)
//=======================================================================================
{
   dlt_partAI = 0.0;
}


void PlantPartArea::readSpeciesParameters(protocol::Component *, vector<string> &)
//=======================================================================================
{

   scienceAPI.read("extinct_coef_" + myName, cExtinctionCoeff, 0.0f, 1.0f);
   scienceAPI.read("spec_" + myName + "_area", cSpec_area, 0.0f, 100000.0f);
   scienceAPI.read("rue_" + myName, cRue, 0.0f, 3.0f);
}

// Query
float PlantPartArea::coverTotal(void)
//=======================================================================================
{
   return 1.0 - (1.0 - cover.green) * (1.0 - cover.sen);
}

float PlantPartArea::coverGreen(void)
//=======================================================================================
{
   return cover.green;
}

float PlantPartArea::coverSen(void)
//=======================================================================================
{
   return cover.sen;
}

void PlantPartArea::doCover (PlantSpatial &spatial)
   //===========================================================================
{

   //+  Purpose
   //     Calculate  cover

   //+  Changes
   //     02 Feb 2005 JNGH - Programmed and Specified

   //+  Local Variables
   float coverA;

   //- Implementation Section ----------------------------------

   if (partAI > 0.0)
      {
      coverA = 1.0 - exp(-cExtinctionCoeff * partAI*spatial.canopyFac());
      cover.green = divide (coverA, spatial.canopyFac(), 0.0);
      }
   else
      cover.green = 0.0;
}

void PlantPartArea::calcDlt_area (float dltDm)
   //===========================================================================
{
   dlt_partAI = dltDm * cSpec_area * smm2sm;
}

float PlantPartArea::interceptRadiationGreen (float radiation)    // incident radiation
    //===========================================================================
{
   //     Calculate total radiation interception and return transmitted radiation

   return coverGreen() * radiation;
}

float PlantPartArea::interceptRadiationTotal (float radiation)    // incident radiation
    //===========================================================================
{
   //     Calculate total radiation interception and return transmitted radiation

   return coverTotal() * radiation;
}

fruitPodPart::fruitPodPart(ScienceAPI& scienceAPI, plantInterface *p, FruitCohort *g, const string &name)
   : SimplePart(scienceAPI, p, name)
   , pod(scienceAPI, p, name)
   , myParent(g)
   {
    co2Modifier = new Co2Modifier(scienceAPI, plant->getComponent());
   }

void fruitPodPart::onInit1(protocol::Component *system)
//=======================================================================================
{
   SimplePart::onInit1(system);

   system->addGettableVar("dlt_dm_pot_rue_pod", dlt.dm_pot_rue, "g/m^2", "Potential dry matter production via photosynthesis");
   pod.onInit1(system);
}

void fruitPodPart::prepare(void)
//=======================================================================================
   {
   co2Modifier->doPlant_Co2Modifier (*(plant->getEnvironment()));

}

void fruitPodPart::update(void)
//=======================================================================================
{
   SimplePart::update();
   pod.update();
}

void fruitPodPart::onHarvest(float /* cutting_height */, float remove_fr,
                             vector<string> &dm_type,
                             vector<float> &dlt_crop_dm,
                             vector<float> &dlt_dm_n,
                             vector<float> &dlt_dm_p,
                             vector<float> &fraction_to_residue)
//=======================================================================================
{
   onHarvest_GenericAboveGroundPart(remove_fr, dm_type, dlt_crop_dm, dlt_dm_n, dlt_dm_p, fraction_to_residue);
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
   float dm_plant = divide (Green.DM(), plant->getPlants(), 0.0);
   DMPlantMin = max (dm_plant * (1.0 - c.trans_frac), DMPlantMin);
}


void fruitPodPart::doDmDemand(float dlt_dm_supply)
//=======================================================================================
{
   float dlt_dm_supply_by_pod = 0.0;  // FIXME
   dlt_dm_supply += dlt_dm_supply_by_pod;

   float dm_grain_demand = myParent->dltDmPotentialGrain();

   if (dm_grain_demand > 0.0)
      DMGreenDemand = dm_grain_demand * fracPod->value(myParent->getStageNumber()) - dlt_dm_supply_by_pod;
   else
      DMGreenDemand = dlt_dm_supply * fracPod->value(myParent->getStageNumber())  - dlt_dm_supply_by_pod;
}

void fruitPodPart::doDmRetranslocate(float DMAvail, float DMDemandDifferentialTotal)
//=======================================================================================
   {
   Retranslocation = Retranslocation + Biomass(DMAvail * divide (dmDemandDifferential(), DMDemandDifferentialTotal, 0.0),
                                               Retranslocation.N(), Retranslocation.P());
   }

float fruitPodPart::dltDmRetranslocateSupply(float DemandDifferential)
//=======================================================================================
   {
   float DMPartPot = Green.DM() + Retranslocation.DM();
   float DMPartAvail = DMPartPot - DMPlantMin * plant->getPlants();
   DMPartAvail = l_bound (DMPartAvail, 0.0);
   float DltDmRetransPart = min (DemandDifferential, DMPartAvail);
   Retranslocation = Retranslocation - Biomass(DltDmRetransPart, 0, 0);   //XXXX this is a bad thing..
   return DltDmRetransPart;
   }

void fruitPodPart::zeroAllGlobals(void)
//=======================================================================================
{
   SimplePart::zeroAllGlobals();
   pod.zeroAllGlobals();
}

void fruitPodPart::zeroDeltas(void)
//=======================================================================================
{
   SimplePart::zeroDeltas();

   pod.zeroDeltas();
}


void fruitPodPart::readSpeciesParameters(protocol::Component *system, vector<string> &sections)
//=======================================================================================
{
   SimplePart::readSpeciesParameters(system, sections);

   co2Modifier->init();
   co2Modifier->read_co2_constants ();

   int   numvals;                                // number of values returned

   scienceAPI.read("transp_eff_cf", c.transpEffCf, numvals, 0.0f, 1.0f);
   scienceAPI.read("rue_pod", cRue_pod, 0.0f, 3.0f);

   //    plant_transp_eff

   TECoeff.read(scienceAPI, "stage_code", "-", 0.0, 100.0
                          , "transp_eff_cf", "-", 0.0, 1.0);
   string partition_option;
   scienceAPI.read("partition_option", partition_option);

   if (partition_option == "1")
      {
      fracPod = new lookupFunction();
      fracPod->read(scienceAPI, "stage_code", "-", 0.0, 100.0
                              , "frac_pod", "-", 0.0, 2.0);

      }
   else if (partition_option == "2" || partition_option == "allometric")
      {
      fracPod = new interpolationFunction();
      fracPod->read(scienceAPI, "x_stage_no_partition", "-", 0.0, 20.0
                             , "y_frac_pod", "-", 0.0, 2.0);

      }
   else
      throw std::invalid_argument("invalid template option in fruitPodPart::readSpeciesParameters");
   pod.readSpeciesParameters(system, sections);
}

// Query
float fruitPodPart::coverTotal(void)
//=======================================================================================
{
   return pod.coverTotal();
}

float fruitPodPart::coverGreen(void)
//=======================================================================================
{
   return pod.coverGreen();
}

float fruitPodPart::coverSen(void)
//=======================================================================================
{
   return pod.coverSen();
}

void fruitPodPart::doCover (PlantSpatial &spatial)

   //===========================================================================
{
   pod.doCover (spatial);
}

void fruitPodPart::doProcessBioDemand(void)
   //===========================================================================
{
}

void fruitPodPart::doBioActual (void)
   //===========================================================================
{
   //       Takes biomass production limited by radiation and discounted by water supply.
   if (plant->Tops().SWDemand() > 0.0)
      dlt.dm = dlt.dm_pot_rue * plant->getSwdefPhoto();
   else
      dlt.dm = 0.0;
}

void fruitPodPart::calcDlt_pod_area (void)
   //===========================================================================
{
   pod.calcDlt_area(dltDmGreen());
//   gDlt_pai = dltDmGreen() * cSpec_pod_area * smm2sm;
}

float fruitPodPart::interceptRadiationGreen (float radiation)    // incident radiation on pods
    //===========================================================================
{
   //     Calculate pod total radiation interception and return transmitted radiation

   radiationInterceptedGreen = pod.interceptRadiationGreen(radiation);
   return radiationInterceptedGreen;
}

float fruitPodPart::interceptRadiationTotal (float radiation)    // incident radiation on pods
    //===========================================================================
{
   //     Calculate pod total radiation interception and return transmitted radiation

   radiationInterceptedTotal = pod.interceptRadiationTotal(radiation);
   return radiationInterceptedTotal;
}

void fruitPodPart::doDmPotRUE (void )                    // (OUTPUT) potential dry matter (carbohydrate) production (g/m^2)
   //===========================================================================
{
   //       Potential biomass (carbohydrate) production from
   //       photosynthesis (g/m^2).  The effect of factors such
   //       temperature and nutritional status of the plant are
   //       taken into account in the radiation use efficiency.

   double stress_factor = min(min(min(plant->getTempStressPhoto(), plant->getNfactPhoto())
                                  , plant->getOxdefPhoto()), plant->getPfactPhoto());

   dlt.dm_pot_rue = (radiationInterceptedGreen * cRue_pod) * stress_factor * co2Modifier->rue();
}


void fruitPodPart::doSWDemand(float SWDemandMaxFactor)         //(OUTPUT) crop water demand (mm)
   //===========================================================================
   /*  Purpose
   *       Return crop water demand from soil by the crop (mm) calculated by
   *       dividing biomass production limited by radiation by transpiration efficiency.
   */
{
   // get potential transpiration from potential
   // carbohydrate production and transpiration efficiency

   cproc_transp_eff_co2_1(plant->getVpd()
                          , c.transpEffCf[(int)plant->getStageNumber()-1]
                          , co2Modifier->te()
                          , &transpEff);

   cproc_sw_demand1 (dlt.dm_pot_rue
                     , transpEff
                     , &sw_demand_te);

       // Capping of sw demand will create an effective TE- recalculate it here
       // In an ideal world this should NOT be changed here - NIH

   float SWDemandMax = SWDemandMaxFactor * coverGreen() ;
   sw_demand = u_bound(sw_demand_te, SWDemandMax);
   transpEff = transpEff * divide(sw_demand_te, sw_demand, 1.0);
}

float fruitPodPart::removePodFraction (float DM)
    //===========================================================================
{
   float frac_pod = fracPod->value(myParent->getStageNumber());
   return divide(DM, 1.0 + frac_pod, 0.0);
}

