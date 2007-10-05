#ifndef StemPartH
#define StemPartH
#include "PlantPart.h"

class plantStemPart : public plantPart
   {
   public:
      plantStemPart(ScienceAPI& scienceAPI, plantInterface *p, const string &name)
         : plantPart(scienceAPI, p, name) {};
      ~plantStemPart() {};
      void onHarvest(float height, float remove_fr,
                     vector<string> &dm_type,
                     vector<float> &dlt_crop_dm,
                     vector<float> &dlt_dm_n,
                     vector<float> &dlt_dm_p,
                     vector<float> &fraction_to_residue);
      void  removeBiomass2(float);
   };

#endif /* StemPartH */
