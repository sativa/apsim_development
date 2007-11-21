#ifndef DeltaH
#define DeltaH

#include <string>
#include "Biomass.h"
class ScienceAPI;
class Pool;
class Delta : public Biomass
   {
   public:
      Delta(ScienceAPI& scienceAPI, const std::string& Name, const std::string& PartName);
      void Move (Pool& From, Pool& To);
      Delta operator = (const Biomass& Pool2);
   };

#endif