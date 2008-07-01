#include "StdPlant.h"

#include "ThingFactory.h"
#include "Environment.h"
#include "Population.h"
#include "Phenology/Zadok.h"
#include "Phenology/Phenology.h"
#include "Fixation.h"
#include "Root/RootGrowthOption1.h"
#include "Root/RootGrowthOption2.h"
#include "Root/NoRoot.h"
#include "Root/MultiRoot.h"
#include "Arbitrators/GenericArbitrator.h"
#include "Arbitrators/GenericArbitratorXY.h"
#include "Leaf/GenericLeaf.h"
#include "Leaf/CohortingLeaf.h"
#include "Stem.h"
#include "Reproductive/FloretPart.h"
#include "Reproductive/PlantFruitCohorting.h"
#include "Reproductive/PlantFruit.h"
#include "Storage/StoragePart.h"
//---------------------------------------------------------------------------
// This function creates parts.
//---------------------------------------------------------------------------
plantThing* createThing(ScienceAPI& api, plantInterface& plant, const std::string& type, std::string& name)
   {
   string typeLower = type;
   To_lower(typeLower);

   if (typeLower == "environment")
      return new Environment(api, name);
   else if (typeLower == "population")
      return new Population(api, plant);
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
   else if (typeLower == "genericarbitrator")
      return new GenericArbitrator(api, plant);
   else if (typeLower == "genericarbitratorxy")
      return new GenericArbitratorXY(api, plant);
   else if (typeLower == "genericleaf")
      return new GenericLeaf(api, &plant, "leaf");
   else if (typeLower == "cohortingleaf")
      return new CohortingLeaf(api, &plant, "leaf");
   else if (typeLower == "stem")
      return new Stem(api, &plant, "Stem");
   else if (typeLower == "floret")
      return new FloretPart(api, &plant, "Floret");
   else if (typeLower == "cohortingfruit")
      return new PlantFruitCohorting(api, &plant, "Fruit");
   else if (typeLower == "fruit")
      return new PlantFruit(api, &plant, "fruit");
   else if (typeLower == "storage")
      return new StoragePart(api, &plant, name);
   throw runtime_error("Cannot create a thing of type: " + type);
   }

