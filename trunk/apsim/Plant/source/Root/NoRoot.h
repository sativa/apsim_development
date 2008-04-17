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
      void doWaterUptake (int option, float swDemand);
      void doNUptake(float sumNMax, float sumSoilNDemand, float NDemand);

      void zeroAllGlobals(void){};
      void zeroDeltas(void){};

      float sw_avail_ratio(int layer);

      void doPlantWaterStress (float sw_demand, SWStress *swStress);

      void doWaterUptakeInternal(float sw_demand);
      float waterUptake(void);

      virtual void write();

      float plant_nit_supply(float biomass, float stageNumber, float swdef_fixation);

      float peswTotal();
      float pesw(int depth);
      float nUptake();
      float fasw(int depth);


   private:
      float SWDemand;
      float NDemand;
      int   n_uptake_option;

   };


#endif /* NoRootH */
