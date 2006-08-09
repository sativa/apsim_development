#ifndef ENVIRONMENT_H
#define ENVIRONMENT_H
#include <vector>

class environment_t {
  public:

   float radn;                                       // solar radiation (Mj/m^2/day)
   float maxt;                                       // minimum air temperature (oC)
   float mint;                                       // maximum air temperature (oC)
   float latitude;                                   // latitude (degrees, negative for southern hemisphere)
   int day_of_year;                                  // day of year
   int year;                                         // year



   float daylength(float) const;
   float daylength(int, float) const;
};

#endif