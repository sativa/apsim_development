#ifndef OilPartH
#define OilPartH
#include "PlantParts.h"


class fruitOilPart : public plantPart {
  public:
   fruitOilPart(plantInterface *p, const string &name) : plantPart(p, name) {};
   ~fruitOilPart() {};
   void doInit(PlantComponent *systemInterface, PlantPhenology *plantPhenology);
   void doRegistrations(protocol::Component *);
   void update(void);
   void onHarvest(float height, float remove_fr,
                  vector<string> &dm_type,
                  vector<float> &dlt_crop_dm,
                  vector<float> &dlt_dm_n,
                  vector<float> &dlt_dm_p,
                  vector<float> &fraction_to_residue);

   void onKillStem(void);
   void onFlowering(void);
   void onStartGrainFill(void);
   void zeroAllGlobals(void);
   void zeroDeltas(void);
   void readSpeciesParameters (protocol::Component *, vector<string> &);
   float grainEnergy(void) const;
   float energyAdjustHI(float harvestIndex) ;
   float energyAdjustDM(float DM) ;
   float dm_yield_demand(float dmDemand) ;
   void doDMDemand(float dlt_dm_grain_demand);
   void doDmPartition(float DMAvail, float DMDemandTotal);
   void doDmRetranslocate(float DMAvail, float DMDemandDifferentialTotal);
   float dltDmGreenUptake(void);
   float dltDmGreenRetransUptake(void);
   float dmDemandDifferential(void);

   void bio_grain_oil (void);

  private:


   float cGrain_oil_conc;                            // fractional oil content of grain (0-1)
   float gDlt_dm_oil_conv;
   float dmOil_conv_retranslocate;
   float cCarbo_oil_conv_ratio;
   float gGrain_energy;                 // multiplier of grain weight to account for seed energy content

      PlantComponent *parentPlant;
      PlantPhenology *phenology;


};

#endif /* OilPartH */

