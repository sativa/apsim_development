#ifndef InductivePhaseH
#define InductivePhaseH

#include "VernalPhase.h"
class InductivePhase : public VernalPhase
   {
   protected:
      virtual float stress();

      virtual void updateTTTargets(Phenology &/* parent*/);
   public:
      InductivePhase(ScienceAPI& scienceAPI, plantInterface& p, const string& stage_name)
         : VernalPhase (scienceAPI, p, stage_name){};

   };

#endif
