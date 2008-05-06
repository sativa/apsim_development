#include "StdPlant.h"

#include "RUEModel.h"
#include "Co2Modifier.h"
#include "Phenology/Phenology.h"
using namespace std;

RUEModel::RUEModel(ScienceAPI& scienceAPI, plantInterface& p)
   : PhotosynthesisModel(scienceAPI, p)
   {
//    RUE.read(scienceAPI,
//              "x_stage_rue", "()", 0.0, 1000.0,
//              "y_rue", "(g dm/mj)", 0.0, 1000.0);
   };

float RUEModel::Potential (float radiationInterceptedGreen)
   {
   double stress_factor = min(min(min(plant.getTempStressPhoto(), plant.getNfactPhoto())
                               , plant.getOxdefPhoto()), plant.getPfactPhoto());

   return radiationInterceptedGreen  * plant.phenology().doInterpolation(RUE) * stress_factor * plant.getCo2Modifier()->rue();
   }

void RUEModel::Read(void)
   {
    RUE.read(scienceAPI,
              "x_stage_rue", "()", 0.0, 1000.0,
              "y_rue", "(g dm/mj)", 0.0, 1000.0);
   }