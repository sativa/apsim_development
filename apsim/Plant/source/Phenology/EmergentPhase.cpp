#include "StdPlant.h"

#include "Phase.h"
#include "EmergentPhase.h"

void EmergentPhase::onSow(protocol::ApsimVariant incomingApsimVariant)
//=======================================================================================
   {
   if (incomingApsimVariant.get("sowing_depth", protocol::DTsingle, false, sowing_depth) == false)
      throw std::invalid_argument("sowing_depth not specified");
   //bound_check_real_var(scienceAPI, sowing_depth, 0.0, 100.0, "sowing_depth");
   }
void EmergentPhase::readCultivarParameters(protocol::Component *s, const string & cultivar)
//=======================================================================================
   {
   pPhase::readCultivarParameters(s, cultivar);

   }

void EmergentPhase::readSpeciesParameters (protocol::Component *s, vector<string> &sections)
//=======================================================================================
   {
   pPhase::readSpeciesParameters (s, sections);
   scienceAPI.read("shoot_lag", shoot_lag, 0.0f, 1000.0f);
   scienceAPI.read("shoot_rate", shoot_rate, 0.0f, 1000.0f);
   }


void EmergentPhase::setupTTTarget()
//=======================================================================================
   {
   target = shoot_lag+sowing_depth*shoot_rate;
   }

string EmergentPhase::description()
//=======================================================================================
   {
   string s;
   s = "shoot_lag = "+ftoa(shoot_lag, "10.0")+ " (dd)\n";
   s += "shoot_rate = "+ftoa(shoot_rate, "10.0")+ " (dd/mm)\n";

   return s;
   } 

