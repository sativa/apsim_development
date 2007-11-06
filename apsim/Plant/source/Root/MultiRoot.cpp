#include <stdio.h>
#include <math.h>
#include <stdexcept>
#include <string>
#include "PlantPart.h"

#include "MultiRoot.h"
#include "RootGrowthOption1.h"
#include "RootGrowthOption2.h"

using namespace std;

MultiRoot::MultiRoot(ScienceAPI& scienceAPI, plantInterface *p, const string &name)
   : RootBase(scienceAPI, p, name)
//=======================================================================================
// Constructor
   {
   }

void MultiRoot::read()
   {
   scienceAPI.read("n_uptake_option", n_uptake_option, 1, 3);
   }
void MultiRoot::write()
//=======================================================================================
// Write all parameters as a summary to stdout.
   {
   cout << "         No Roots At All - Potential uptakes assumed." << endl;
   }


float MultiRoot::waterUptake (void)
//=======================================================================================
// Return the total daily water uptake from this root system
   {
   return SWDemand;
   }

void MultiRoot::doWaterUptake (float sw_demand_in)
//=======================================================================================
// Calculate todays daily water uptake by this root system
   {
   SWDemand = sw_demand_in;
   }

void MultiRoot::plant_water_stress (
                                       float sw_demand,
                                       float& swdef_photo,
                                       float& swdef_pheno,
                                       float& swdef_pheno_flower,
                                       float& swdef_pheno_grainfill,
                                       float& swdef_expansion,
                                       float& swdef_fixation )
//     ===========================================================
//         Get current water stress factors (0-1)
   {
   swdef_photo = 1.0;
   swdef_pheno = 1.0;
   swdef_pheno_flower = 1.0;
   swdef_pheno_grainfill = 1.0;
   swdef_expansion = 1.0;
   swdef_fixation = 1.0;
   }

float MultiRoot::oxdef_stress ()
//=======================================================================================
// Calculate today's oxygen deficit (i.e. water logging) stress factor
   {
      return 1.0;
   }


//int MultiRoot::find_layer_no(float depth)
////=======================================================================================
//// Return the index of the layer corresponding to the given depth
//   {
//   return 1;
//   }

float MultiRoot::sw_avail_ratio(int layer)  //(INPUT) soil profile layer number
//===========================================================================
//     Get the soil water availability factor in a layer.  For a layer,
//     it is 1.0 unless the plant-extractable soil water declines
//     below a fraction of plant-extractable soil water capacity for
//     that layer.
   {
   return 1.0;
   }


//int MultiRoot::find_layer_no(float depth,      // depth in profile
//                  float *dlayr,     // layer depth array
//                  int num_layers)   // lowest layer
////===========================================================================
//   {
//   return 1;
//   }
//
//int MultiRoot::find_layer_no(float depth, const vector<float> &dlayer )
////===========================================================================
//   {
//   return 1;                                // index of
//   }
//


float MultiRoot::plant_nit_supply(float biomass, float stageNumber, float swdef_fixation)
//=======================================================================================
// Calculate Plant Nitrogen Supply
    {
    return 0.0;
   }


float MultiRoot::peswTotal()
//=======================================================================================
// Calculate total plant extractable soil water.
   {
   return 0.0;
   }

float MultiRoot::pesw(int depth)
//=======================================================================================
// Calculate plant extractable soil water at the given depth.
   {
   return 1.0;
   }

float MultiRoot::dltSwDep()
//=======================================================================================
// Calculate total plant extractable soil water.
   {
   return SWDemand;
   }

float MultiRoot::nUptake()
//=======================================================================================
// find the proportion of uptake to be distributed
   {
   return NDemand;
   }

float MultiRoot::fasw(int depth)
//=======================================================================================
// calculate the fraction of available soil water at the given depth (mm)
   {
   return 1.0;
   }


void MultiRoot::plant_nit_uptake(float sumNMax, float sumSoilNDemand, float nDemand)
//=======================================================================================
//       Find nitrogen uptake.
    {
    if (n_uptake_option == 1)
       NDemand = nDemand ;
    else
       NDemand = sumSoilNDemand ;
    }

void MultiRoot::onInit1(protocol::Component *system)
//=======================================================================================
// Perform all component initialisation.
   {
   plantPart::onInit1(system);
   system->addGettableVar("ep",
               SWDemand, "mm", "ep");

   }

//+  Purpose
//       Plant transpiration and soil water extraction
void MultiRoot::plant_water_uptake (int option, float swDemand)
    {
    SWDemand = swDemand;
    }   