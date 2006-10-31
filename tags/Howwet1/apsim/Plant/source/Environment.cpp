#include "Environment.h"
#include "PlantLibrary.h"

float environment_t::daylength(float sun_angle) const
{
	return daylength(day_of_year, sun_angle);
}

float environment_t::daylength(int dyoyr, float sun_angle) const
{
   return ::day_length(dyoyr, latitude, sun_angle);
}
