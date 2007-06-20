#ifndef InterpolationFunctionH
#define InterpolationFunctionH

#include <list>
#include <math.h>
#include <string>
#include <algorithm>

using namespace std;
class plantInterface;
class ScienceAPI;

// Implement stick (linear interpolation) functions
class interpolationFunction : public externalFunction
{
 private:
   vector<float> x;
   vector<float> y;
 public:
   float integral(float v1, float v2);
   void read(ScienceAPI& scienceAPI,
             const string& xName, const string&  xunits, float x0, float x1,
             const string& yName, const string& yunits, float y0, float y1);
   float value(float v) const;
   vector<float> xVal() const {
   	return(x);
   };
   vector<float> yVal() const {
   	return(y);
   };
   std::string description(void) const;
   bool isInitialised(void)
      {
      if (x.size() == 0) return false;
      if (y.size() == 0) return false;
      return true;
      };
   float minYval(){return *min_element(y.begin(),y.end());}
};


#endif
