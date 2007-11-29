#include <stdio.h>
#include <math.h>
#include <stdexcept>
#include <string>
#include "PlantPart.h"
#include "ReproductivePart.h"

using namespace std;

ReproductivePart::ReproductivePart(ScienceAPI& api, plantInterface *p, const string &name)
//=======================================================================================
     : plantPart(api, p, name)
     {
     }

