#ifndef OilPartH
#define OilPartH
#include "PlantParts.h"

class fruitOilPart : public plantPart {
  public:
   fruitOilPart(plantInterface *p, const string &name) : plantPart(p, name) {};
   ~fruitOilPart() {};
   void onHarvest(float height, float remove_fr,
                  vector<string> &dm_type,
                  vector<float> &dlt_crop_dm,
                  vector<float> &dlt_dm_n,
                  vector<float> &dlt_dm_p,
                  vector<float> &fraction_to_residue);

   void onKillStem(void);
   void onFlowering(void);
   void onStartGrainFill(void);

};

#endif /* OilPartH */

