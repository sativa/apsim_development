#include <stdio.h>
#include <math.h>
#include <stdexcept>
#include <string>
#include "PlantPart.h"

#include "RootBase.h"
#include "RootGrowthOption1.h"
#include "RootGrowthOption2.h"
#include "NoRoot.h"
#include "MultiRoot.h"

using namespace std;


RootBase* RootBase::construct(ScienceAPI& scienceAPI, plantInterface *p, const string &type, const string &name)
//=======================================================================================
// Setup correct root model for user-defined type
   {
   if (type == "Jones+RitchieGrowthPattern")
      return new rootGrowthOption2(scienceAPI, p, name);
   else if (type == "NoRoot")
      return new NoRoot(scienceAPI, p, name);
   else if (type == "MultiRoot")
      return new MultiRoot(scienceAPI, p, name);

   else
      // default:
      return new rootGrowthOption1(scienceAPI, p, name);
   }

RootBase::RootBase(ScienceAPI& scienceAPI, plantInterface *p, const string &name)
   : plantPart(scienceAPI,p,name)
   {
   }   
