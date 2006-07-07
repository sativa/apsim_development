#include <stdio.h>
#include <string>
#include <stdexcept>

#include <ComponentInterface/Component.h>
#include <general/string_functions.h>

#include "PlantInterface.h"
#include "PlantLibrary.h"
#include "Plant.h"
#include "PlantParts.h"
#include "LeafPart.h"
#include "genericLeafPart.h"
#include "cohortingLeafPart.h"
using namespace std;

// Return one of the leaf objects we know about.
plantLeafPart* constructLeafPart (plantInterface *p, const string &type, const string &name)
  {
  plantLeafPart *object;
  if (type == "generic_leaf")
    object = new genericLeafPart(p, name);
  else if (type == "cohorting")
    object = new cohortingLeafPart(p, name);
  else
    throw std::invalid_argument("Unknown leaf_object '" + type + "'");

  return (object);
  }
