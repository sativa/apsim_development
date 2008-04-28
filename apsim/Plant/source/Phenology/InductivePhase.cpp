#include "StdPlant.h"

#include "InductivePhase.h"

float InductivePhase::stress()
   {
   return min(plant.getSwDefPheno(), min(plant.getNFactPheno(), plant.getPFactPheno()));
   }



