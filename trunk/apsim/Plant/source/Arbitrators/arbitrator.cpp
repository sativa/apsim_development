#include <stdio.h>
#include <string>
#include <stdexcept>
#include <ComponentInterface/Component.h>
#include <ComponentInterface/ApsimVariant.h>
#include <ComponentInterface/ScienceAPI.h>

#include "PlantComponent.h"
#include "PlantLibrary.h"
#include "Plant.h"
#include "PlantPart.h"
#include "Leaf/LeafPart.h"
#include "arbitrator.h"
#include "NullArbitrator.h"
#include "CerealArbitrator.h"
#include "GenericArbitrator.h"
#include "GenericArbitratorXY.h"
#include "AllometricArbitrator.h"


plantPart* Arbitrator::FindPart(vector <plantPart *>& Parts, string name)
   {
   for (vector<plantPart *>::const_iterator part = Parts.begin(); part != Parts.end(); part++)
      {
      if (Str_i_Eq((*part)->name(), name))
         return *part;
      }
   return NULL;
   }

Arbitrator* constructArbitrator(ScienceAPI& scienceAPI, plantInterface *p, const string &type)
//=======================================================================================
   {
   Arbitrator *object;
   if (type == "")
     object = new nullArbitrator(scienceAPI, p);
   else if (type == "1")
     object = new genericArbitrator(scienceAPI, p);
   else if (type == "2")
     object = new cerealArbitrator(scienceAPI, p);
   else if (type == "genericxy")
     object = new genericArbitratorXY(scienceAPI, p);
   else if (type == "allometric")
     object = new allometricArbitrator(scienceAPI, p);
   else
     throw std::invalid_argument("Unknown arbitrator object '" + type + "'");

   return (object);
   }



   