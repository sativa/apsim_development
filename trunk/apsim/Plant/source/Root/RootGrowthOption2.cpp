#include <stdio.h>
#include <math.h>
#include <map>
#include <string>
#include <stdexcept>

#include <boost/function.hpp>
#include <boost/bind.hpp>

#include <ComponentInterface/MessageDataExt.h>
#include <ComponentInterface/Type.h>
#include <ComponentInterface/ApsimVariant.h>
#include <ComponentInterface/Component.h>
#include <ComponentInterface/datatypes.h>
#include <ApsimShared/ApsimComponentData.h>
#include <ApsimShared/FStringExt.h>
#include <general/string_functions.h>

#include "PlantInterface.h"
#include "PlantLibrary.h"
#include "Plant.h"
#include "PlantPart.h"
#include "RootPart.h"
#include "RootGrowthOption2.h"
#include "Utility/PlantUtility.h"

using namespace std;


void rootGrowthOption2::root_length_growth (void)
//=======================================================================================
//   (was cproc_root_length_growth_new)
//   Calculate the increase in root length density in each rooted
//   layer based upon soil hospitality, moisture and fraction of
//   layer explored by roots.
    {
    float dlt_length_tot;                         // total root length increase (mm/m^2)
    float rlv_factor_tot;                         // total rooting factors across profile
    float branching_factor;                       //
    float plant_rld;
    float rld;
    float root_length_tot;
    float rwf;
    float cum_depth;

   vector<float> rlv_factor(num_layers);
   vector<float> root_length_new(num_layers);

   setTo(dltRootLength, (float)0.0);

   float depth_today = root_depth + dltRootDepth;
   int deepest_layer = find_layer_no (depth_today);

   float cum_layer_depth = sum_real_array(dlayer, deepest_layer);

   rlv_factor_tot = 0.0;
   cum_depth      = 0.0;

   for (int layer = 0; layer <= deepest_layer; layer++)
      {
      cum_depth +=  0.5 * dlayer[layer];
      rwf       = divide (cum_depth, cum_layer_depth, 0.0) ;
      rwf       = pow((1.0 - rwf), rootDistributionPattern);

      rld       = divide (root_length[layer], dlayer[layer], 0.0);

      plant_rld = divide (rld, plant->getPlants() ,0.0);

      branching_factor = rel_root_rate.value(plant_rld);

      rlv_factor[layer] = sw_fac_root.value(sw_avail_ratio(layer)) *
                            branching_factor *
                             xf [layer] *
                             divide(dlayer[layer], root_depth, 0.0) *
                              rwf;

      rlv_factor[layer] = l_bound(rlv_factor[layer], 1e-6);
      rlv_factor_tot += rlv_factor[layer];
      }

   dlt_length_tot = dlt.dm_green/sm2smm * specificRootLength;

   root_length_tot = sum_real_array(root_length, max_layer) + dlt_length_tot;

   for (int layer = 0; layer <= deepest_layer; layer++)
      {
      root_length_new[layer] = root_length_tot *
                                divide (rlv_factor[layer],
                                        rlv_factor_tot, 0.0);

      dltRootLength [layer] = root_length_new[layer] - root_length[layer];

      dltRootLength [layer] = dlt_length_tot *
                                divide (rlv_factor[layer], rlv_factor_tot, 0.0);

      }
   }


void rootGrowthOption2::readSpeciesParameters(protocol::Component *system, vector<string> &sections)
//=======================================================================================
// Read Species-specific Parameters
   {
   plantRootPart::readSpeciesParameters(system, sections);
   system->readParameter (sections, "root_distribution_pattern",
                          rootDistributionPattern, 0.0, 100.0);


   }
