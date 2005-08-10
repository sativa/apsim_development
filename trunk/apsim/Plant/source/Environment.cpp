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

float environment_t::daylength(float sun_angle) const
{
	return daylength(day_of_year, sun_angle);
}
float environment_t::daylength(int dyoyr, float sun_angle) const
{
   return ::day_length(dyoyr, latitude, sun_angle);
}
