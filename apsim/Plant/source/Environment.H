#ifndef ENVIRONMENT_H
#define ENVIRONMENT_H
#include <vector>

class environment_t {
  public:
	std::vector<float> dlayer;
	std::vector<float> sw_dep;
	std::vector<float> ll_dep;
	std::vector<float> dul_dep;
   float maxt, mint, latitude;
   int day_of_year;
   int find_layer_no(float) const;
   float daylength(float) const;
   float daylength(int, float) const;
};

#endif