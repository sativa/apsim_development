#include "StdPlant.h"


#include "Phase.h"
#include "CompositePhase.h"

bool compositePhase::contains(const pPhase &PhaseToLookFor)
//=======================================================================================
   {
   for (vector<pPhase *>::const_iterator phase = phases.begin(); phase !=  phases.end(); phase++)
      if ((*phase)->name() == PhaseToLookFor.name()) return true;

   return false;
   }

float compositePhase::getTT(void)
//=======================================================================================
   {
   float tt = 0.0;
   for (vector<pPhase *>::const_iterator phase = phases.begin(); phase !=  phases.end(); phase++)
      tt += (*phase)->getTT();

   return tt;
   }

float compositePhase::getTTTarget(void)
//=======================================================================================
   {
   float tttarget = 0.0;
   for (vector<pPhase *>::const_iterator phase = phases.begin(); phase !=  phases.end(); phase++)
      tttarget += (*phase)->getTTTarget();

   return tttarget;
   }


float compositePhase::getDays(void)
//=======================================================================================
   {
   float days = 0.0;
   for (vector<pPhase *>::const_iterator phase = phases.begin(); phase !=  phases.end(); phase++)
      days += (*phase)->getDays();

   return days;
   }
