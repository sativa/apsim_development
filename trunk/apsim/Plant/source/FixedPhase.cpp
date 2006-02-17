#include <stdio.h>
#include <math.h>
#include <vector>
#include <string>
#include <stdexcept>
#include <ComponentInterface/Component.h>
#include <ComponentInterface/dataTypes.h>
#include <ComponentInterface/ApsimVariant.h>
#include <ComponentInterface/MessageDataExt.h>

#include "PlantComponent.h"
#include "PlantLibrary.h"
#include "FixedPhase.h"

void FixedPhase::readCultivarParameters(protocol::Component *s, const string & cultivar)
//=======================================================================================
   {
   pPhase::readCultivarParameters(s, cultivar);
   string key = "tt_"+name();
   s->readParameter (cultivar, key, target, 0, 1e6);
   }

string FixedPhase::description() const
//=======================================================================================
   {
   return "   tt_"+pPhase::name()+" = "+ftoa(target, "10.0")+ " (dd)";
   }