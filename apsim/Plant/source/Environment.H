#ifndef ENVIRONMENT_H
#define ENVIRONMENT_H
#include <vector>

class environment_t {
  public:
   std::vector<float> dlayer;                        // thickness of soil layer I (mm)
   int num_layers;                                   // number of soil layers 
   
	std::vector<float> sw_dep;
	std::vector<float> ll_dep;
	std::vector<float> dul_dep;
   float radn;                                       // solar radiation (Mj/m^2/day)
   float maxt;                                       // minimum air temperature (oC)
   float mint;                                       // maximum air temperature (oC)
   float latitude;                                   // latitude (degrees, negative for southern hemisphere)
   int day_of_year;                                  // day of year
   int year;                                         // year

   int find_layer_no(float) const;
   float sw_avail_ratio(int layer) const;
   float daylength(float) const;
   float daylength(int, float) const;
};

#endif