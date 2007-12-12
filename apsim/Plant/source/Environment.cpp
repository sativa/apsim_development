#include <stdio.h>
#include <math.h>
#include <stdexcept>
#include <string>
#include "plantPart.h"

#include "Environment.h"

static const char* floatType =        "<type kind=\"single\"/>";

environment_t::environment_t(ScienceAPI& sAPI)
//===========================================================================
   : scienceAPI(sAPI)
   {
   hasreadconstants = false;
   year = 0;
   day_of_year = 0;
   latitude = 0.0;

   mint = 0.0;
   maxt = 0.0;
   radn = 0.0;
   }

environment_t::~environment_t(void)
//===========================================================================
   {
   }

void environment_t::onInit1(protocol::Component *system)
   //===========================================================================
   {
   scienceAPI.subscribe ("tick", TimeFunction(&environment_t::OnTick));
   }

void environment_t::OnNewMet(protocol::NewMetType &newmet)
//===========================================================================
// Field a NewMet event
  {
  radn = newmet.radn;
  maxt = newmet.maxt;
  mint = newmet.mint;
  }

void environment_t::OnTick(protocol::TimeType &Tick)
//=======================================================================================
// Event Handler for the Tick Event
   {
   double sd = (double)Tick.startday;
   jday_to_day_of_year(&sd, &day_of_year, &year);
    }
void environment_t::getOtherVariables(protocol::Component *system)
//===========================================================================
   {
    if (!scienceAPI.getOptional("co2", "mg/kg",co2, 0.0, 1500.0))
       {
       co2 = co2_default;
       }
    scienceAPI.get("latitude", "deg", latitude, -90., 90.);
   }

void environment_t::read()
//===========================================================================
   {
   scienceAPI.read("svp_fract", svp_fract, 0.0f, 1.0f);
   scienceAPI.read("co2_default", co2_default, 0.0f, 1000.0f);

   scienceAPI.subscribe ("newmet", NewMetFunction(&environment_t::OnNewMet));

   }

void environment_t::zeroAllGlobals(void)
//===========================================================================
   {
   svp_fract = 0.0;
   }

float environment_t::vpd(float svp_fract, float maxt, float mint)
//==========================================================================
   {
   float vpd = svp_fract * (svp(maxt) - svp(mint));
   return vpd;
   }


float environment_t::svp(float temp)
//==========================================================================
// function to get saturation vapour pressure for a given temperature in oC (kpa)
   {
   float val = 6.1078 *
            exp(17.269 * temp / (237.3 + temp)) *
            mb2kpa;
   return val;
   }

float environment_t::vpdEstimate (void)
//===========================================================================
   {
   return (vpd(svp_fract, maxt, mint));
   }

float environment_t::daylength(float sun_angle) const
//===========================================================================
   {
	return daylength(day_of_year, sun_angle);
   }

float environment_t::daylength(int dyoyr, float sun_angle) const
//===========================================================================
   {
   return ::day_length(dyoyr, latitude, sun_angle);
   }
