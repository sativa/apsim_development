#ifndef FixationH
#define FixationH
#include <vector>

class Fixation : public plantThing
   {
   public:
      Fixation(ScienceAPI& scienceAPI, const std::string& name);
      float Potential(float biomass, float stageNumber, float swdef_fixation);
      float NFixPot(){return n_fix_pot;};
   private:

       float n_fix_rate[20];                      // potential rate of N fixation (g N fixed
       float n_fix_pot;
    };

#endif
