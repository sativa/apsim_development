#include <stdio.h>
#include <math.h>
#include <stdexcept>
#include <string>
#include "SimplePart.h"

#include "Leaf.h"
#include "GenericLeaf.h"
#include "CohortingLeaf.h"
using namespace std;

// Return one of the leaf objects we know about.
Leaf* constructLeafPart (ScienceAPI& scienceAPI, plantInterface *p, const string &type, const string &name)
  {
  Leaf *object;
  if (type == "generic_leaf")
    object = new GenericLeaf(scienceAPI, p, name);
  else if (type == "cohorting")
    object = new CohortingLeaf(scienceAPI, p, name);
  else
    throw std::invalid_argument("Unknown leaf_object '" + type + "'");

  return (object);
  }


void Leaf::doNConccentrationLimits(float modifier)
{
   SimplePart::doNConccentrationLimits(modifier);
   g.n_conc_crit *= modifier;
   if (g.n_conc_crit <= g.n_conc_min)
      throw std::runtime_error("Aiieeee!!\nnconc_crit < nconc_min!\nWhat's happened to CO2??");
}


float Leaf::dmRetransSupply(void)
  {
  float dm_part_avail = Green.DM() - DMPlantMin * plant->getPlants();
  return (l_bound (dm_part_avail, 0.0));
  }

