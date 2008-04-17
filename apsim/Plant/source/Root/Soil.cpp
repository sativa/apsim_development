#include "StdPlant.h"
#include "Soil.h"
#include <numeric>
using namespace std;


Soil::Soil(ScienceAPI& s)
   : scienceAPI(s)
//=======================================================================================
// Constructor
   {
   //scienceAPI = s;
   zero();
   }

void Soil::onInit1(protocol::Component *system)
//=======================================================================================
// Perform all component initialisation.
   {

   scienceAPI.subscribe("new_profile", NewProfileFunction(&Soil::onNewProfile));


   }

void Soil::onNewProfile(protocol::NewProfileType &v)
//=======================================================================================
// Handler for OnNewProfile event
   {
    float profile_depth;                          // depth of soil profile (mm)

    vector<float> previousLayers;
    for (unsigned layer = 0; layer != max_layer; layer++)
       previousLayers.push_back(dlayer[layer]);

    vector<float> scratch = v.dlayer;
    num_layers = scratch.size();

    for (unsigned i = 0; i < scratch.size(); i++)
      dlayer[i] = scratch[i];

    scratch = v.ll15_dep;
    for (unsigned i = 0; i < scratch.size(); i++)
      ll15_dep[i] = scratch[i];

    scratch = v.dul_dep;
    for (unsigned i = 0; i < scratch.size(); i++)
      dul_dep[i] = scratch[i];

    scratch = v.sat_dep;
    for (unsigned i = 0; i < scratch.size(); i++)
      sat_dep[i] = scratch[i];

//    scratch = v.sw_dep;
//    for (unsigned i = 0; i < scratch.size(); i++)
//      sw_dep[i] = scratch[i];

    scratch = v.bd;
    for (unsigned i = 0; i < scratch.size(); i++)
      bd[i] = scratch[i];

//    if (xf.size()==0)
//       for (int layer = 0; layer != num_layers; layer++)
//          xf.push_back(0.0);


    // dlayer may be changed from its last setting due to erosion
//    profile_depth = sum_real_array(dlayer,max_layer);

//    if (root_depth > profile_depth)
//        {
//        vector<float> vdlayer;
//        for (unsigned layer = 0; layer != max_layer; layer++)
//          vdlayer.push_back(dlayer[layer]);
//        vector<float> vprevdlayer;
//        for (unsigned layer = 0; layer != previousLayers.size(); layer++)
//          vprevdlayer.push_back(previousLayers[layer]);
//
//        redistribute( vprevdlayer,  vdlayer, profile_depth);
//
//        }
//     for (unsigned layer = 0; layer < (unsigned) num_layers; layer++)
//        {
//        ll_dep[layer] = divide (ll_dep[layer], previousLayers[layer], 0.0)
//                              * dlayer[layer];
//        }


   }

void Soil::zero(void)
//=======================================================================================
// Zero everything
   {
      num_layers = 0;
      fill_real_array (dlayer , 0.0, max_layer);
      fill_real_array (ll15_dep , 0.0, max_layer);
      fill_real_array (dul_dep , 0.0, max_layer);
      fill_real_array (sat_dep , 0.0, max_layer);
      fill_real_array (bd , 0.0, max_layer);
      fill_real_array (no3gsm , 0.0, max_layer);
      fill_real_array (nh4gsm , 0.0, max_layer);
      fill_real_array (sw_dep , 0.0, max_layer);
   }

int Soil::find_layer_no(float depth)
//=======================================================================================
// Return the index of the layer corresponding to the given depth
   {
   unsigned int indx;
   float progressive_sum = 0.0;

   for(indx = 0; indx < (unsigned) num_layers; indx++)
      {
      progressive_sum = progressive_sum + dlayer[indx];
      if(progressive_sum >= depth)
         break;
      }
   if (indx != 0 && indx==(unsigned)num_layers) return (indx - 1); // last element in array
   return indx;                                            // index of
   }

int Soil::find_layer_no(float depth,      // depth in profile
                  float *dlayr,     // layer depth array
                  int num_layers)   // lowest layer
//===========================================================================

/*Purpose
 *   returns layer number of depth in profile dlayr
 *Definition
 *   Each of the "num_layers" elements of "dlayr" holds the
 *   height of the corresponding soil layer.  The height of the
 *   top layer is held in "dlayr"(0), and the rest follow in
 *   sequence down into the soil profile.  This function
 *   returns the index of the first element of "dlayr" which
 *   has its lower surface deeper than or equal to "depth".  If
 *   "depth" is deeper than the lower surface of the layer
 *   corresponding to "dlayr"("num_layers"), then "num_layers"
 *   is returned.
 */

   {
   return get_cumulative_index_real(depth, dlayr, num_layers);
   }

int Soil::find_layer_no(float depth, const vector<float> &dlayer )
//===========================================================================
   {
   float progressive_sum = 0.0; //cumulative sum_of
   unsigned int indx;                    //index count_of_real_vals

   for(indx = 0; indx < dlayer.size(); indx++)
      {
      progressive_sum +=  dlayer[indx];
      if(progressive_sum >= depth)
         {
         break;
         }
      }
   if (indx==dlayer.size()) return (indx - 1); // last element in array
   return indx;                                // index of
   }
