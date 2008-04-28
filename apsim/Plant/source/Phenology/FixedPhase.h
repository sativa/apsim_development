#ifndef FixedPhaseH
#define FixedPhaseH

#include "Phase.h"
// A fixed duration phenological phase.
class FixedPhase : public pPhase
   {
   protected:
      interpolationFunction stressFunction;

      virtual float stress();
   public:
      void read();
      FixedPhase(ScienceAPI& scienceAPI, plantInterface& p, const string& stage_name)
         : pPhase (scienceAPI, p, stage_name){};
      virtual string description();
   };


#endif

