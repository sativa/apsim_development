#ifndef EnvironmentH
#define EnvironmentH
#include <vector>

#include <ComponentInterface/MessageDataExt.h>
#include <ComponentInterface/Component.h>
#include <ComponentInterface/Type.h>

#include "Utility/PlantUtility.h"
#include "PlantLibrary.h"

class environment_t {
  public:
  environment_t(ScienceAPI& scienceAPI);
  ~environment_t(void);

   void read();
   void zeroAllGlobals(void);
   void onInit1(protocol::Component *system);
   void getOtherVariables(protocol::Component *system);

   float radn;                                       // solar radiation (Mj/m^2/day)
   float maxt;                                       // minimum air temperature (oC)
   float mint;                                       // maximum air temperature (oC)
   float avet;                                       // maximum air temperature (oC)
   float latitude;                                   // latitude (degrees, negative for southern hemisphere)
   int day_of_year;                                  // day of year
   int year;                                         // year

      float svp_fract;                                  // fraction of distance between svp at
                                                        // min temp and svp at max temp where
                                                        // average svp during transpiration
                                                        // lies. (0-1)
      float     co2_default;
      float     co2;

   float vpdEstimate (void);

   float daylength(float) const;
   float daylength(int, float) const;
   private:
      float svp(float temp);
      float vpd(float svp_fract, float maxt, float mint);
      ScienceAPI& scienceAPI;
      bool hasreadconstants;
      void OnNewMet(protocol::NewMetType &newmet) ;
      void OnTick(protocol::TimeType &Tick);
};

#endif
