#include "StdPlant.h"

#include "ThingFactory.h"
#include "Environment.h"
#include "Phenology/Zadok.h"
#include "Phenology/Phenology.h"
#include "Fixation.h"
#include "Root/RootGrowthOption1.h"
#include "Root/RootGrowthOption2.h"
#include "Root/NoRoot.h"
#include "Root/MultiRoot.h"

//---------------------------------------------------------------------------
// This function creates parts.
//---------------------------------------------------------------------------
plantThing* createThing(ScienceAPI& api, plantInterface& plant, const std::string& type, std::string& name)
   {
   string typeLower = type;
   To_lower(typeLower);

   if (typeLower == "environment")
      return new Environment(api, name);
   else if (typeLower == "phenology")
      return new Phenology(api, plant);
   else if (typeLower == "fixation")
      return new Fixation(api, plant, name);
   else if (typeLower == "zadok")
      return new Zadok(api, plant);
   else if (typeLower == "root")
      return new rootGrowthOption1(api, &plant, name);
   else if (typeLower == "jones+ritchiegrowthpattern")
      return new rootGrowthOption2(api, &plant, name);
   else if (typeLower == "noroot")
      return new NoRoot(api, &plant, name);
   else if (typeLower == "multiroot")
      return new MultiRoot(api, &plant, name);

   throw runtime_error("Cannot create a thing of type: " + type);
   }

