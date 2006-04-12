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
#include "EmergentPhase.h"

#include <iostream.h>

void EmergentPhase::onSow(protocol::ApsimVariant incomingApsimVariant)
//=======================================================================================
   {
   if (incomingApsimVariant.get("sowing_depth", protocol::DTsingle, false, sowing_depth) == false)
      throw std::invalid_argument("sowing_depth not specified");
   //bound_check_real_var(parentPlant, sowing_depth, 0.0, 100.0, "sowing_depth");
   }
void EmergentPhase::readCultivarParameters(protocol::Component *s, const string & cultivar)
//=======================================================================================
   {
   pPhase::readCultivarParameters(s, cultivar);

   }

void EmergentPhase::readSpeciesParameters (protocol::Component *s, vector<string> &sections)
//=======================================================================================
   {
   pPhase::readSpeciesParameters (s, sections);
      s->readParameter (sections
                      , "shoot_lag"
                      , shoot_lag
                      , 0, 1000.0);

      s->readParameter (sections
                      , "shoot_rate"
                      , shoot_rate
                      , 0, 1000.0);
   }


void EmergentPhase::setupTTTarget()
//=======================================================================================
   {
   target = shoot_lag+sowing_depth*shoot_rate;
   }

string EmergentPhase::description() const
//=======================================================================================
   {
   string s;
   s = "shoot_lag = "+ftoa(shoot_lag, "10.0")+ " (dd)\n";
   s += "shoot_rate = "+ftoa(shoot_rate, "10.0")+ " (dd/mm)\n";

   return s;
   } 