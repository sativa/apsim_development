#include "StdPlant.h"

#include "ThingFactory.h"
#include "Environment.h"
#include "TTTPhenology.h"
#include "GenericPhenology.h"
#include "WheatPhenology.h"
//---------------------------------------------------------------------------
// This function creates parts.
//---------------------------------------------------------------------------
plantThing* createThing(ScienceAPI& api, plantInterface& plant, const std::string& name, std::string& type)
   {
   string nameLower = name;
   To_lower(nameLower);

   if (nameLower == "environment")
      return new Environment(api, name);
   else if (nameLower == "tttphenology")
      return new TTTPhenology(api, &plant);
   else if (nameLower == "genericphenology")
      return new GenericPhenology(api, &plant);
   else if (nameLower == "wheatphenology")
      return new WheatPhenology(api, &plant);

   throw runtime_error("Cannot create a thing of type: " + type);
   }

