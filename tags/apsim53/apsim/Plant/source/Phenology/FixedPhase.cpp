#include <stdio.h>
#include <math.h>
#include <vector>
#include <string>
#include <stdexcept>
#include <ComponentInterface/Component.h>
#include <ComponentInterface/datatypes.h>
#include <ComponentInterface/ApsimVariant.h>
#include <ComponentInterface/MessageDataExt.h>
#include <ComponentInterface/ScienceAPI.h>

#include "PlantComponent.h"
#include "PlantLibrary.h"
#include "PlantInterface.h"
#include "Phase.h"
#include "FixedPhase.h"

void FixedPhase::readCultivarParameters(protocol::Component *s, const string & cultivar)
//=======================================================================================
   {
   pPhase::readCultivarParameters(s, cultivar);
   string key = "tt_"+name();
   scienceAPI.read(key, target, 0.0f, 1000000.0f);
   }

string FixedPhase::description() const
//=======================================================================================
   {
   return "   tt_"+pPhase::name()+" = "+ftoa(target, "10.0")+ " (dd)";
   }

