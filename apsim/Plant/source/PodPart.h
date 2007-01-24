#ifndef PodPartH
#define PodPartH
#include "PlantPart.h"

class fruitGrainPart;
class fruitPodPart : public plantPart {
  public:
   fruitPodPart(plantInterface *p, fruitGrainPart *g, const string &name) ;
   ~fruitPodPart() {};
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
   void doDmMin(void);
   void doDmDemand(float  dlt_dm_supply);
   void doProcessBioDemand(void);
   void readConstants (protocol::Component *, const string &);
   void readSpeciesParameters (protocol::Component *, vector<string> &);
   void doDmRetranslocate(float DMAvail, float DMDemandDifferentialTotal);
   float dltDmRetranslocateSupply(float DemandDifferential) ;

   void zeroAllGlobals(void);
   void zeroDeltas(void);
   float interceptRadiationGreen(float radiation);
   float interceptRadiationTotal(float radiation);
   float coverTotal(void)  ;
   float coverGreen(void)  ;
   float coverSen(void)  ;
   float coverDead(void)  ;
   void doCover (PlantSpatial &spatial);
   void calcDlt_pod_area (void);
   void doDmPotRUE (void );

   void doTECO2();
   void doSWDemand(float SWDemandMaxFactor);
   void doDmPotTE(float swSupply);
   void doBioActual (void);

   private:
      float fracPod(void);
      float fracPod1(void);
      void doDmDemand1(float  dlt_dm_supply);
      void doDmDemand2(float  dlt_dm_supply);

      float cExtinctionCoeffPod;
      float cSpec_pod_area;
      float cRue_pod;

      float gPai;
      float gDlt_pai;
      float cSvp_fract;
      float cFrac_pod[max_table];                        // fraction of dm or grain weight allocated to pod
      float cX_stage_no_partition[max_table];
      float cY_frac_pod[max_table];                      // fraction of dm or grain weight allocated to pod
      int   cNum_stage_no_partition;
      float cPod_trans_frac;                            // fraction of pod used in translocat
      float cX_co2_te_modifier[max_table];
      float cY_co2_te_modifier[max_table];
      int   cNum_co2_te_modifier;

      int cPartition_option;
      struct Cover
      {
         float green;
         float sen;
         float dead;
      };

      Cover coverPod;
      fruitGrainPart *myGrain;                           // The grain we encapsulate
};

#endif /* PodPartH */
