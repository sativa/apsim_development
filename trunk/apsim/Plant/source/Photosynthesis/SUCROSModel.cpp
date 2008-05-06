#include "StdPlant.h"
#include "SUCROSModel.h"
#include "Co2Modifier.h"
#include "Phenology/Phenology.h"
using namespace std;

SUCROSModel::SUCROSModel(ScienceAPI& scienceAPI, plantInterface& p)
   : PhotosynthesisModel(scienceAPI, p)
   {
//    RUE.read(scienceAPI,
//              "x_stage_rue", "()", 0.0, 1000.0,
//              "y_rue", "(g dm/mj)", 0.0, 1000.0);
   };

float SUCROSModel::Potential (float radiationInterceptedGreen)
   {
   double stress_factor = min(min(min(plant.getTempStressPhoto(), plant.getNfactPhoto())
                               , plant.getOxdefPhoto()), plant.getPfactPhoto());

   return radiationInterceptedGreen  * plant.phenology().doInterpolation(RUE) * stress_factor * plant.getCo2Modifier()->rue();
   }

void SUCROSModel::Read(void)
   {
//    PZPHP data structure needs to be filled here

    RUE.read(scienceAPI,
              "x_stage_rue", "()", 0.0, 1000.0,
              "y_rue", "(g dm/mj)", 0.0, 1000.0);
   }