#include "StdPlant.h"

#include "Fixation.h"

Fixation::Fixation(ScienceAPI& api, const std::string& name)
//===========================================================================
   : plantThing(api, name)
   {
   int numvals;
   scienceAPI.read("n_fix_rate", n_fix_rate, numvals, 0.0f, 1.0f);
   scienceAPI.expose("dlt_n_fixed_pot", "g/m^2", "potential N fixation", n_fix_pot);
   }

float Fixation::Potential(float biomass, float stageNumber, float swdef_fixation)
//===========================================================================
   {
   int current_phase = int(stageNumber);
   n_fix_pot = n_fix_rate[current_phase-1] * biomass * swdef_fixation;
   return n_fix_pot;
   }
