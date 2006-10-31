#include <stdio.h>
#include <math.h>
#include <vector>
#include <string>
#include <stdexcept>
#include <ComponentInterface/Component.h>
#include <ComponentInterface/datatypes.h>
#include <ComponentInterface/ApsimVariant.h>
#include <ComponentInterface/MessageDataExt.h>

#include "PlantComponent.h"
#include "PlantLibrary.h"
#include "PlantInterface.h"
#include "Phase.h"
#include "PhotoPhase.h"
#include "Environment.h"

#include <iostream.h>

void PhotoPhase::readCultivarParameters(protocol::Component *s, const string & cultivar)
//=======================================================================================
   {
   pPhase::readCultivarParameters(s, cultivar);

   string key1 = "x_pp_"+name();
   string key2 = "y_tt_"+name();

   photo_tt.read(s, cultivar,
                  key1.c_str(), "h", 0.0, 100.0,
                  key2.c_str(), "dd", 0.0, 1e6);
   }

void PhotoPhase::readSpeciesParameters (protocol::Component *s, vector<string> &sections)
//=======================================================================================
   {
   pPhase::readSpeciesParameters (s, sections);
      s->readParameter (sections
                      , "twilight"//, "(o)"
                      , twilight
                      , -90.0, 90.0);
      string key = name()+"_pp_inductive_cutoff";
      cutoff = s->readParameter (sections, key);
   }


void PhotoPhase::updateTTTargets(PlantPhenology &parent,const environment_t &e)
//=======================================================================================
   {
   if (cutoff == "start")
      {
      if (tt == 0.0)
         {
         photoperiod = e.daylength (twilight);
         target = photo_tt.value(photoperiod);
         }
      }
   else if (cutoff == "end")
      {
      photoperiod = e.daylength (twilight);
      target = photo_tt.value(photoperiod);

      }
   else
      throw std::invalid_argument("Invalid cutoff for photoperiod inductive phase");
   }

string PhotoPhase::description() const
//=======================================================================================
   {
   return photo_tt.description();
   } 

