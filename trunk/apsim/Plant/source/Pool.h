#ifndef PoolH
#define PoolH
#include <string>
#include "Biomass.h"
class ScienceAPI;
class Delta;
class Pool : public Biomass
   {
   public:
      Pool(ScienceAPI& scienceAPI, const std::string& Name, const std::string& PartName);
      void Init (float Plants);

      interpolationFunction DigestibilityMax;
      interpolationFunction DigestibilityAvg;
      interpolationFunction DigestibilityMin;

      Pool operator = (const Biomass& Pool2);

   private:
      std::string PartName;
      std::string Name;
      ScienceAPI& scienceAPI;

   };

#endif