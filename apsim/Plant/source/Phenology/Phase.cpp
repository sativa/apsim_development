#include "StdPlant.h"

#include "Phase.h"
#include "Environment.h"


pPhase::pPhase(ScienceAPI& api, plantInterface& p, const std::string& name)
   : scienceAPI(api), plant(p)
   {
   myName = name;
   tt = target = days = 0.0;
   tt_after = 0.0;
   days_after = 0.0;
   scienceAPI.expose(name + "TTTarget", "deg. days", name + " thermal time target", target);
   scienceAPI.expose("TTAfter" + name, "deg. days", name + " - thermal time spent in this phase", tt_after);
   scienceAPI.exposeFunction("DaysAfter" + name, "day", name + " - days spent in this phase", IntFunction(&pPhase::getDaysAfter));
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

