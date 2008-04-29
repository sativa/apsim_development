#include "StdPlant.h"

#include "ThingFactory.h"
#include "Environment.h"
#include "Phenology/Zadok.h"
#include "Phenology/Phenology.h"
#include "Fixation.h"
//---------------------------------------------------------------------------
// This function creates parts.
//---------------------------------------------------------------------------
plantThing* createThing(ScienceAPI& api, plantInterface& plant, const std::string& name, std::string& type)
   {
   string nameLower = name;
   To_lower(nameLower);

   if (nameLower == "environment")
      return new Environment(api, name);
   else if (nameLower == "phenology")
      return new Phenology(api, plant);
   else if (nameLower == "fixation")
      return new Fixation(api, plant, name);
   else if (nameLower == "zadok")
      return new Zadok(api, plant);

   throw runtime_error("Cannot create a thing of type: " + type);
   }

