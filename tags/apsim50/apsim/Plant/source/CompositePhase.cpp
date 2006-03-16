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
#include "Phase.h"
#include "CompositePhase.h"

bool compositePhase::contains(const pPhase &PhaseToLookFor) const
//=======================================================================================
   {
   for (vector<pPhase *>::const_iterator phase = phases.begin(); phase !=  phases.end(); phase++)
      if ((*phase)->name() == PhaseToLookFor.name()) return true;

   return false;
   }

float compositePhase::getTT(void) const
//=======================================================================================
   {
   float tt = 0.0;
   for (vector<pPhase *>::const_iterator phase = phases.begin(); phase !=  phases.end(); phase++)
      tt += (*phase)->getTT();

   return tt;
   }

float compositePhase::getDays(void) const
//=======================================================================================
   {
   float days = 0.0;
   for (vector<pPhase *>::const_iterator phase = phases.begin(); phase !=  phases.end(); phase++)
      days += (*phase)->getDays();

   return days;
   }
