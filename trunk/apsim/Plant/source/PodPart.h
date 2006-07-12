#ifndef PodPartH
#define PodPartH
#include "PlantParts.h"

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
   float dltDmRetranslocateSupply(float DemandDifferential) const;

   void zeroAllGlobals(void);
   void zeroDeltas(void);
   float interceptRadiation(float radiation);
   float coverTotal(void)  ;
   float coverGreen(void)  ;
   float coverSen(void)  ;
   float coverDead(void)  ;
   float calcCover (float canopy_fac);
   float dltDmPotTe(void);
   float dltDmPotRue(void);
   void calcDlt_pod_area (void);
   void doDmPotRUE (double  radn_int_pod);

   void doTECO2();
   float SWDemand(void);
   void doDmPotTE(void);
   void doBioActual (void);

   private:
      float fracPod(void);
      float fracPod1(void);
      void doDmDemand1(float  dlt_dm_supply);
      void doDmDemand2(float  dlt_dm_supply);

      float gDlt_dm;
      float gDlt_dm_pot_rue;
      float gDlt_dm_pot_te;
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
      float cTransp_eff_cf[max_table];                  // transpiration efficiency coefficient
                                                        // to convert vpd to
                                                        // transpiration efficiency (kpa)
                                                        // although this is expressed as a
                                                        // pressure it is really in the form
                                                        // kpa*g carbo per m^2 / g water per m^2
                                                        // and this can be converted to
                                                        // kpa*g carbo per m^2 / mm water
                                                        // because 1g water = 1 cm^3 water
      float cX_co2_te_modifier[max_table];
      float cY_co2_te_modifier[max_table];
      int   cNum_co2_te_modifier;

      int cPartition_option;
      float gTranspEff;
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
