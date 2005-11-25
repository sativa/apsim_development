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

   };
#endif