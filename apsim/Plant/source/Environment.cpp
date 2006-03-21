#include "environment.h"
#include "plantlibrary.h"

// Return the index of the layer that contains "depth"
int environment_t::find_layer_no(float depth) const
   {
   unsigned int indx;
   float progressive_sum = 0.0;

   for(indx = 0; indx < dlayer.size(); indx++)
      {
      progressive_sum = progressive_sum + dlayer[indx];
      if(progressive_sum >= depth)
         break;
      }
   if (indx != 0 && indx==dlayer.size()) return (indx - 1); // last element in array
   return indx;                                            // index of
   }

float environment_t::sw_avail_ratio(int layer) const //(INPUT) soil profile layer number
//===========================================================================

/*  Purpose
*      Get the soil water availability factor in a layer.  For a layer,
*      it is 1.0 unless the plant-extractable soil water declines
*      below a fraction of plant-extractable soil water capacity for
*      that layer.
*/

   {
   //  Local Variables
   float pesw;                // plant extractable soil-water (mm/mm)
   float pesw_capacity;       // plant extractable soil-water capacity (mm/mm)
   float sw_avail_ratio;      // soil water availability ratio (0-1)

   pesw = sw_dep[layer] - ll_dep[layer];
   pesw_capacity = dul_dep[layer] - ll_dep[layer];
   sw_avail_ratio = divide (pesw, pesw_capacity, 10.0);
   return sw_avail_ratio;
   }

float environment_t::daylength(float sun_angle) const
{
	return daylength(day_of_year, sun_angle);
}
float environment_t::daylength(int dyoyr, float sun_angle) const
{
   return ::day_length(dyoyr, latitude, sun_angle);
}
