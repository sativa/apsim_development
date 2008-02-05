#include <stdio.h>
#include <math.h>
#include <stdexcept>
#include <string>
#include "SimplePart.h"
#include "CompositePart.h"
#include "Leaf.h"
#include "GenericLeaf.h"
#include "CohortingLeaf.h"
using namespace std;

// Return one of the leaf objects we know about.
Leaf* constructLeafPart (ScienceAPI& scienceAPI, plantInterface *p, const string &type, const string &name)
  {
  Leaf *object;
  if (type == "generic_leaf")
    object = new GenericLeaf(scienceAPI, p, name);
  else if (type == "cohorting")
    object = new CohortingLeaf(scienceAPI, p, name);
  else
    throw std::invalid_argument("Unknown leaf_object '" + type + "'");

  return (object);
  }


void Leaf::doNConccentrationLimits(float modifier)
{
   SimplePart::doNConccentrationLimits(modifier);
   g.n_conc_crit *= modifier;
   if (g.n_conc_crit <= g.n_conc_min)
      throw std::runtime_error("Aiieeee!!\nnconc_crit < nconc_min!\nWhat's happened to CO2??");
}


float Leaf::dmRetransSupply(void)
  {
  float dm_part_avail = Green.DM() - DMPlantMin * plant->getPlants();
  return (l_bound (dm_part_avail, 0.0));
  }

void Leaf::doSWDemand(float SWDemandMaxFactor)         //(OUTPUT) crop water demand (mm)
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
                          , plant->getCo2ModifierTe()
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


void Leaf::doBioActual (void)
   //===========================================================================
{
   //       Takes biomass production limited by radiation and discounted by water supply.
   if (plant->Tops().SWDemand() > 0.0)
      dlt.dm = dlt.dm_pot_rue * plant->getSwdefPhoto();
   else
      dlt.dm = 0.0;
}

float Leaf::coverTotal(void)
//=======================================================================================
{
   return 1.0 - (1.0 - coverLeaf.green) * (1.0 - coverLeaf.sen);
}

float Leaf::coverGreen(void)
//=======================================================================================
{
   return coverLeaf.green;
}

float Leaf::coverSen(void)
//=======================================================================================
{
   return coverLeaf.sen;
}

void Leaf::doCover (PlantSpatial &spatial)
   //===========================================================================
{

   //+  Purpose
   //     Calculate leaf cover

   //+  Changes
   //     19 Jan 2006 JNGH - Programmed and Specified

   //- Implementation Section ----------------------------------

    legnew_cover(spatial.rowSpacing()
                 , cXRowSpacing
                 , cYExtinctCoef
                 , cNumRowSpacing
                 , spatial.canopyFac()
                 , getLAI()
                 , &coverLeaf.green);


    legnew_cover (spatial.rowSpacing()
                 , cXRowSpacing
                 , cYExtinctCoefDead
                 , cNumRowSpacing
                 , spatial.canopyFac()
                 , getSLAI()
                 , &coverLeaf.sen);

}

float Leaf::interceptRadiationGreen (float radiation)    // incident radiation on leafs
    //===========================================================================
{
   //     Calculate leaf total radiation interception and return transmitted radiation

   radiationInterceptedGreen = coverGreen() * radiation;
   return radiationInterceptedGreen;
}

float Leaf::interceptRadiationTotal (float radiation)    // incident radiation on leafs
    //===========================================================================
{
   //     Calculate leaf total radiation interception and return transmitted radiation

   radiationInterceptedTotal = coverTotal() * radiation;
   return radiationInterceptedTotal;
}

void Leaf::doDmPotRUE (void )                    // (OUTPUT) potential dry matter (carbohydrate) production (g/m^2)
   //===========================================================================
{
   //       Potential biomass (carbohydrate) production from
   //       photosynthesis (g/m^2).  The effect of factors such
   //       temperature and nutritional status of the plant are
   //       taken into account in the radiation use efficiency.

   double stress_factor = min(min(min(plant->getTempStressPhoto(), plant->getNfactPhoto())
                                  , plant->getOxdefPhoto()), plant->getPfactPhoto());

   dlt.dm_pot_rue = (radiationInterceptedGreen * cRue.value(plant->getStageNumber())) * stress_factor * plant->getCo2ModifierRue();
}


