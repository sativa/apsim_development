#ifndef ReproStructH
#define ReproStructH
class ReproStruct : public plantPart
   {
   public:
// Constants
      float extinct_coef_pod;
      float spec_pod_area;
      float rue_pod;

      ReproStruct(plantInterface *p, const string &name)
         : plantPart(p,name)
         {
         }

   void onHarvest(float height, float remove_fr,
                  vector<string> &dm_type,
                  vector<float> &dlt_crop_dm,
                  vector<float> &dlt_dm_n,
                  vector<float> &dlt_dm_p,
                  vector<float> &fraction_to_residue) {};
   };
#endif