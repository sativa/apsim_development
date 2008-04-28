#include "StdPlant.h"

#include "Environment.h"
#include "Phase.h"
#include "VernalPhase.h"
#include "Utility/Output.h"
#include "Utility/OutputVariable.h"

void VernalPhase::reset()
//=======================================================================================
   {
   pPhase::reset();
   cumvd = 0.0;
   dlt_cumvd = 0.0;
   }

void VernalPhase::GetOutputs(std::vector <Output*> &Outputs)
//=======================================================================================
   {
   pPhase::GetOutputs(Outputs);
   OutputVariable *CumVDVariable = new OutputVariable("cumvd","","Cumulative Vernal Days",cumvd);
   Outputs.push_back(CumVDVariable);


   }
void VernalPhase::read()
//=======================================================================================
   {
   pPhase::read();

   string key1 = "cumvd_"+name();
   string key2 = "tt_"+name();

   vernal_tt.read(scienceAPI,
                  key1.c_str(), "vd", 0.0, 100.0,
                  key2.c_str(), "dd", 0.0, 1e6);
   vernal_days.read(scienceAPI,
                      "x_vernal_temp", "(oc)", -10., 60.0,
                      "y_vernal_days", "(days)", 0.0, 1.0);
   }


void VernalPhase::updateTTTargets(Phenology &/* parent*/)
//=======================================================================================
   {
   dlt_cumvd = vernal_days.value(plant.environment().meant());
   //dlt_cumvd = linint_3hrly_temp (plant->environment().maxt(), plant->environment().mint(), &vernal_days);
   cumvd = cumvd + dlt_cumvd;
   target = vernal_tt.value(cumvd);
   }

string VernalPhase::description()
//=======================================================================================
   {
   return vernal_tt.description();
   }


