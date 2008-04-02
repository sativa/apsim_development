#include "StdPlant.h"

#include "Phase.h"
#include "FixedPhase.h"

void FixedPhase::readCultivarParameters(protocol::Component *s, const string & cultivar)
//=======================================================================================
   {
   pPhase::readCultivarParameters(s, cultivar);
   string key = "tt_"+name();
   scienceAPI.read(key, target, 0.0f, 1000000.0f);
   }

string FixedPhase::description()
//=======================================================================================
   {
   return "   tt_"+pPhase::name()+" = "+ftoa(target, "10.0")+ " (dd)";
   }

