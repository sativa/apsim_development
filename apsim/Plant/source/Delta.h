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

      Biomass& operator = (const Biomass& Pool2);

   protected:
      virtual void CheckBounds() { }; // Delta's can be negative. (Do we want this?)
   };

#endif
