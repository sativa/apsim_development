#include <stdio.h>
#include <math.h>
#include <values.h>
#include <limits>
#include <vector>
#include <string>
#include <stdexcept>
#include <sstream>
#include <iomanip>
#include <ComponentInterface/Component.h>
#include <ComponentInterface/ScienceAPI.h>
#include "PlantComponent.h"
#include "PlantInterface.h"
#include "PlantLibrary.h"
#include "ExternalFunction.h"
using namespace std;

externalFunction::externalFunction() {};
externalFunction::~externalFunction() {};

void externalFunction::read(ScienceAPI& /*scienceAPI*/,
                       const string& xname, const string& xunits, float /* x0*/, float /* x1*/,
                       const string& yname, const string& yunits, float /* y0*/, float /* y1*/)
      {
      xName = string(xname); yName = string(yname);
      xUnits = string(xunits); yUnits = string(yunits);
      }

std::string externalFunction::description(void) const
   {
   return string("");
   }
