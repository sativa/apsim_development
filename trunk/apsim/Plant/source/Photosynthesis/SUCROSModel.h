#ifndef SUCROSModelH
#define SUCROSModelH
#include <stdio.h>
#include <math.h>
#include <map>
#include <string>
#include <stdexcept>
#include <iomanip>

#include <ComponentInterface/datatypes.h>
#include <general/string_functions.h>
#include <ComponentInterface/ScienceAPI.h>
#include "Utility/InterpolationFunction.h"

#include "PlantInterface.h"
#include "Utility/PlantUtility.h"
#include "PhotosynthesisModel.h"

class SUCROSModel : public PhotosynthesisModel {

  public:
  SUCROSModel(ScienceAPI& scienceAPI, plantInterface& p);
  float Potential (float radiationInterceptedGreen);
  void  Read (void);
  private:
  interpolationFunction RUE;                        // radiation use efficiency as f(stage number) (g dm/mj)
};

#endif

