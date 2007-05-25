#ifndef NoRootH
#define NoRootH
#include "RootBase.h"

class NoRoot : public RootBase
   {
   public:
      NoRoot(ScienceAPI& scienceAPI, plantInterface *p, const string &name);
      ~NoRoot(){};

      void read();
      void onInit1(protocol::Component *system);
      void plant_water_uptake (int option, float swDemand);
      void plant_nit_uptake(float sumNMax, float sumSoilNDemand, float NDemand);

      void zeroAllGlobals(void){};
      void zeroDeltas(void){};

      float sw_avail_ratio(int layer) const;

      void plant_water_stress (
                                       float sw_demand,
                                       float& swdef_photo,
                                       float& swdef_pheno,
                                       float& swdef_pheno_flower,
                                       float& swdef_pheno_grainfill,
                                       float& swdef_expansion,
                                       float& swdef_fixation );

      void doWaterUptake(float sw_demand);
      float waterUptake(void);

      float oxdef_stress ();
      virtual void write();

      float plant_nit_supply(float biomass, float stageNumber, float swdef_fixation);

      float peswTotal();
      float pesw(int depth);
      float dltSwDep();
      float nUptake();
      float fasw(int depth);


   private:
      float SWDemand;
      float NDemand;
      int   n_uptake_option;

   };


#endif /* NoRootH */
