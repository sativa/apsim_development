#include "Environment.h"
#include <ComponentInterface/ScienceAPI.h>
static const char* floatType =        "<type kind=\"single\"/>";

environment_t::environment_t(void)
//===========================================================================
   {
   }

environment_t::~environment_t(void)
//===========================================================================
   {
   }

environment_t::environment_t(const environment_t &/* environment_t*/)
//===========================================================================
// copy constructor
//	copy data members of object
   {
	throw std::invalid_argument("Copy constructor NI for environment_t");
   }


const environment_t &environment_t::operator=(const environment_t &/*other*/)
//===========================================================================
// Assigment operator
//	assign data members of object
   {
   throw std::invalid_argument("Assignment operator NI for environment_t");
   }

void environment_t::onInit1(protocol::Component *system)
   //===========================================================================
   {
   co2ID = system->addRegistration(RegistrationType::get,
                                    "co2", addUnitsToDDML(floatType, "ppm").c_str(),
                                    "", "");
   }

void environment_t::doNewMet(protocol::NewMetType &newmet)
//===========================================================================
// Field a NewMet event
  {
  radn = newmet.radn;
  maxt = newmet.maxt;
  mint = newmet.mint;
  }

void environment_t::getOtherVariables(protocol::Component *system)
//===========================================================================
   {
    if (!system->getVariable(co2ID, co2, 0.0, 1500.0, true))
       {
       co2 = co2_default;
       }
   }

void environment_t::read(ScienceAPI& scienceAPI)
//===========================================================================
   {
   scienceAPI.read("svp_fract", svp_fract, 0.0f, 1.0f);
   scienceAPI.read("co2_default", co2_default, 0.0f, 1000.0f);
   }

void environment_t::zeroAllGlobals(void)
//===========================================================================
   {
   svp_fract = 0.0;
   }

float environment_t::vpd(float svp_fract, float maxt, float mint) const
//==========================================================================
   {
   float vpd = svp_fract * (svp(maxt) - svp(mint));
   return vpd;
   }


float environment_t::svp(float temp) const
//==========================================================================
// function to get saturation vapour pressure for a given temperature in oC (kpa)
   {
   float val = 6.1078 *
            exp(17.269 * temp / (237.3 + temp)) *
            mb2kpa;
   return val;
   }
   
float environment_t::vpdEstimate (void) const
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
