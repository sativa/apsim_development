#ifndef RootGrowthOption2H
#define RootGrowthOption2H
#include "RootPart.h"

class rootGrowthOption2 : public plantRootPart
//=======================================================================================
//
   {
 private:
   float rootDistributionPattern;
 public:
   rootGrowthOption2(plantInterface *p, const string &name) : plantRootPart(p, name) {};
   void readSpeciesParameters(protocol::Component *system, vector<string> &sections);
   void root_length_growth (void);
   };

#endif /* RootGrowthOption2 */
