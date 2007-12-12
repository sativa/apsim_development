#include <stdio.h>
#include <math.h>
#include <stdexcept>
#include <string>
#include "plantPart.h"

#include "StageBasedInterpolationFunction.h"
using namespace std;

StageBasedInterpolationFunction::StageBasedInterpolationFunction(plantInterface& p, ScienceAPI& API, const std::string& Name, const std::string& Units, const std::string& Description)
   : scienceAPI(API), plant(p)
   {
   this->Name = Name;
   f.read(scienceAPI, "X"+Name+"StageCode" , "()", 1.0, 12.0
                    , "Y"+Name, "()", 0.0, 1e6);
   if (f.isInitialised())
      scienceAPI.exposeFunction(Name, Units, Description, FloatFunction(&StageBasedInterpolationFunction::value));
   }


float StageBasedInterpolationFunction::value(void)
   {
   return f.value(plant.getStageCode());
   }
