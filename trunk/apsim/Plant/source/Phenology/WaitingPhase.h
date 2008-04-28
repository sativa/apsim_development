#ifndef WaitingPhaseH
#define WaitingPhaseH

#include "Phase.h"
class WaitingPhase : public pPhase
   {
   public:
      WaitingPhase(ScienceAPI& scienceAPI, plantInterface& p, const string& stage_name);

   };


#endif

