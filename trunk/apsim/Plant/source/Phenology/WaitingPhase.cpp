#include "StdPlant.h"

#include "WaitingPhase.h"

WaitingPhase::WaitingPhase(ScienceAPI& scienceAPI, plantInterface& p, const string& stage_name)
   : pPhase (scienceAPI, p, stage_name)
   {
   target = 10000000;
   }
