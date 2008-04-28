#include "StdPlant.h"

#include "Phase.h"
#include "Environment.h"


pPhase::pPhase(ScienceAPI& api, plantInterface& p, const std::string& name)
   : scienceAPI(api), plant(p)
   {
   myName = name;
   tt = target = days = 0.0;
   empty = true;
   scienceAPI.expose(name + "TTTarget", "deg. days", name + " thermal time target", target);
   scienceAPI.expose(name + "TT", "deg. days", name + " - thermal time spent in this phase", tt);
   scienceAPI.expose(name + "Days", "day", name + " - days spent in this phase", days);
   }

bool operator == (const pPhase &a, const pPhase &b)
// ===================================================================================
   {
   return (a.name() == b.name());
   };


void pPhase::read()
   {
   y_tt.read(scienceAPI,
               "x_temp", "oC", 0.0, 100.0,
               "y_tt", "oC days", 0.0, 100.0);
   }

float pPhase::TT()
   {
   return linint_3hrly_temp (plant.environment().maxt(), plant.environment().mint(), &y_tt);
   }

void pPhase::calcPhaseDevelopment(int /*das*/, 
                                      float& dlt_tt_phenol, float& phase_devel)
   {
   dlt_tt_phenol = TT() * stress();

   phase_devel = divide(getTT() + dlt_tt_phenol, getTTTarget(), 1.0);
   }

