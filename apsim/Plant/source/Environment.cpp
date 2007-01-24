#include "Environment.h"

static const char* floatType =        "<type kind=\"single\"/>";

environment_t::environment_t(void)
   {
   }

environment_t::~environment_t(void)
   {
   }

// copy constructor
//	copy data members of object
environment_t::environment_t(const environment_t &/* environment_t*/)
//===========================================================================
{
	throw std::invalid_argument("Copy constructor NI for environment_t");
}


// Assigment operator
//	assign data members of object
const environment_t &environment_t::operator=(const environment_t &/*other*/)
   //===========================================================================
{
   throw std::invalid_argument("Assignment operator NI for environment_t");
}

void environment_t::doIDs(protocol::Component *system)
   //===========================================================================
   {
   co2ID = system->addRegistration(RegistrationType::get,
                                    "co2", addUnitsToDDML(floatType, "ppm").c_str(),
                                    "", "");
   }

void environment_t::getOtherVariables(protocol::Component *system)
   //===========================================================================
   {
    if (!system->getVariable(co2ID, co2, 0.0, 1500.0, true))
       {
       co2 = co2_default;
       }
   }

void environment_t::readSpeciesParameters(protocol::Component *system, vector<string> &sections)
   //===========================================================================
{
    system->readParameter (sections
                   ,"svp_fract"//, "()"
                   , svp_fract
                   , 0.0, 1.0);

    system->readParameter (sections
                     , "co2_default"//, "()"
                     , co2_default
                     , 0.0, 1000.0);
}

void environment_t::zeroAllGlobals(void)
   //===========================================================================
{
   svp_fract = 0.0;
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
