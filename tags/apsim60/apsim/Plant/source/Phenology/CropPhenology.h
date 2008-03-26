#ifndef CropPhenologyH
#define CropPhenologyH

#include "PlantPhenology.h"

class CropPhenology : public PlantPhenology
   {
   protected:
       CropPhenology(ScienceAPI& scienceAPI, plantInterface *p)
       : PlantPhenology(scienceAPI, p) {};



    private:
    public:
    };
#endif

