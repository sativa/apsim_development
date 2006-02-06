#include <stdio.h>
#include <math.h>
#include <vector>
#include <string>
#include <stdexcept>
#include <ComponentInterface/Component.h>
#include <ComponentInterface/dataTypes.h>
#include <ComponentInterface/ApsimVariant.h>
#include <ComponentInterface/MessageDataExt.h>

#include "PlantComponent.h"
#include "PlantLibrary.h"
#include "VernalPhase.h"


void VernalPhase::readCultivarParameters(protocol::Component *s, const string & cultivar)
   {
   pPhase::readCultivarParameters(s, cultivar);

   string key1 = "cumvd_"+name();
   string key2 = "tt_"+name();

   vernal_tt.read(s, cultivar
                   , key1.c_str(), "vd", 0.0, 100.0
                   , key2.c_str(), "dd", 0.0, 1e6);
   //::MessageBox(NULL,vernal_tt.description().c_str(),"GGG",0);
   }
void VernalPhase::readSpeciesParameters (protocol::Component *s, vector<string> &sections)
   {
   pPhase::readSpeciesParameters (s, sections);

   vernal_days.search(s, sections,
                       "x_vernal_temp", "(oc)", -10., 60.0,
                       "y_vernal_days", "(days)", 0.0, 1.0);
   }

// dynamic TT targets
void VernalPhase::updateTTTargets(const environment_t &e)
   {
   dlt_cumvd = vernal_days.value((e.maxt + e.mint)*0.5);
   cumvd = cumvd + dlt_cumvd;
   target = vernal_tt.value(cumvd);

   }

string VernalPhase::description() const
   {
      return vernal_tt.description();
   } 