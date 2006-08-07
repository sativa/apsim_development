#ifndef OilPartH
#define OilPartH
#include "PlantPart.h"


class fruitOilPart : public plantPart {
  public:
   fruitOilPart(plantInterface *p, const string &name) : plantPart(p, name) {};
   ~fruitOilPart() {};
   void doRegistrations(protocol::Component *);
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
   float calcDmDemand(float dmDemand) ;
   void doDMDemand(float dlt_dm_grain_demand) ;       //remove
   void doDMDemandGrain(float dlt_dm_grain_demand);
   void doDmPartition(float DMAvail, float DMDemandTotal);
   void doDmRetranslocate(float DMAvail, float DMDemandDifferentialTotal);
   float dltDmGreenUptake(void) const;
   float dltDmGreenRetransUptake(void) const;
   float dmDemandDifferential(void) const;

   void doBioGrainOil (void);

  private:


   float cGrain_oil_conc;                            // fractional oil content of grain (0-1)
   float gDlt_dm_oil_conv;
   float dmOil_conv_retranslocate;
   float cCarbo_oil_conv_ratio;
   float gGrain_energy;                 // multiplier of grain weight to account for seed energy content

};

#endif /* OilPartH */

