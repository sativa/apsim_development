#include "StdPlant.h"

#include "Phase.h"
#include "PhotoPhase.h"
#include "Environment.h"

void PhotoPhase::readCultivarParameters(protocol::Component *s, const string & cultivar)
//=======================================================================================
   {
   pPhase::readCultivarParameters(s, cultivar);

   string key1 = "x_pp_"+name();
   string key2 = "y_tt_"+name();

   photo_tt.read(scienceAPI,
                  key1.c_str(), "h", 0.0, 100.0,
                  key2.c_str(), "dd", 0.0, 1e6);
   }

void PhotoPhase::readSpeciesParameters (protocol::Component *s, vector<string> &sections)
//=======================================================================================
   {
   pPhase::readSpeciesParameters (s, sections);
   scienceAPI.read("twilight", twilight, -90.0f, 90.0f);
   string key = name()+"_pp_inductive_cutoff";
   scienceAPI.read(key, cutoff);
   }


void PhotoPhase::updateTTTargets(PlantPhenology &/* parent*/)
//=======================================================================================
   {
   if (cutoff == "start")
      {
      if (tt == 0.0)
         {
         photoperiod = plant->environment().dayLength(twilight);
         target = photo_tt.value(photoperiod);
         }
      }
   else if (cutoff == "end")
      {
      photoperiod = plant->environment().dayLength(twilight);
      target = photo_tt.value(photoperiod);

      }
   else
      throw std::invalid_argument("Invalid cutoff for photoperiod inductive phase");
   }

string PhotoPhase::description()
//=======================================================================================
   {
   return photo_tt.description();
   } 

