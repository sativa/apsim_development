#ifndef RootGrowthOption1H
#define RootGrowthOption1H
#include "RootPart.h"

class rootGrowthOption1 : public plantRootPart
//=======================================================================================
//
   {
 public:
   rootGrowthOption1(ScienceAPI& scienceAPI, plantInterface *p, const string &name)
      : plantRootPart(scienceAPI, p, name) {};
   void root_length_growth (void);
   };
#endif /* RootGrowthOption1 */
