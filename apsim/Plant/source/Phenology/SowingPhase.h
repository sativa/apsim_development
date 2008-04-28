#ifndef SowingPhaseH
#define SowingPhaseH

#include "phase.h"
class SowingPhase : public pPhase
   {
   public:
      SowingPhase(ScienceAPI& scienceAPI, plantInterface& p, const string& stage_name)
         : pPhase (scienceAPI, p, stage_name){};

      void calcPhaseDevelopment(int das,
                                float& dlt_tt_phenol, float& phase_devel);
   private:
      bool germinating();

   };


#endif

