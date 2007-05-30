#ifndef StoragePartH
#define StoragePartH
#include "PlantPart.h"

class StoragePart : public plantPart
   {
   public:

      static StoragePart* construct(ScienceAPI& scienceAPI, plantInterface *p, const string &name) ;
      StoragePart(ScienceAPI& scienceAPI, plantInterface *p, const string &name)
         : plantPart(scienceAPI, p, name) {};
      ~StoragePart() {};
      void onHarvest(float height, float remove_fr,
                     vector<string> &dm_type,
                     vector<float> &dlt_crop_dm,
                     vector<float> &dlt_dm_n,
                     vector<float> &dlt_dm_p,
                     vector<float> &fraction_to_residue);
      void  update(void);
      void  removeBiomass2(float);
   };

#endif /* StoragePartH */
