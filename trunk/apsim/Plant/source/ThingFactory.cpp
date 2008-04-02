#include "StdPlant.h"

#include "ThingFactory.h"
#include "Environment.h"
//---------------------------------------------------------------------------
// This function creates parts.
//---------------------------------------------------------------------------
plantThing* createThing(ScienceAPI& api, std::string& name, std::string& type)
   {
   string nameLower = name;
   To_lower(nameLower);

   if (nameLower == "environment")
      return new Environment(api, name);

   throw runtime_error("Cannot create a thing of type: " + type);
   }
   
