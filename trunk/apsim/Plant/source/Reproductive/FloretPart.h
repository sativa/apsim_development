#ifndef FloretPartH
#define FloretPartH
#include "PlantPart.h"

class FloretPart : public plantPart {
  public:
   FloretPart(ScienceAPI& scienceAPI, plantInterface *p, const string &name) ;
   ~FloretPart() {};
   void onInit1(protocol::Component *);
   void update(void);
   void onHarvest(float height, float remove_fr,
                  vector<string> &dm_type,
                  vector<float> &dlt_crop_dm,
                  vector<float> &dlt_dm_n,
                  vector<float> &dlt_dm_p,
                  vector<float> &fraction_to_residue);


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
   void calcDlt_Floret_area (void);
   void doDmPotRUE (void );

   void doTECO2();
   void doSWDemand(float SWDemandMaxFactor);
   void doDmPotTE(float swSupply);
   void doBioActual (void);

   virtual float dmGreenVeg(void) const {return 0;}
   virtual float dmSenescedVeg(void) const {return 0;}
   virtual float nGreenVeg(void)const {return 0;}
   virtual float nSenescedVeg(void)const {return 0;}
   virtual float nDeadVeg(void)const {return 0;}
   virtual float pGreenVeg(void) const {return 0;}
   virtual float pSenescedVeg(void) const {return 0;}
   virtual float pDeadVeg(void) const {return 0;}





   private:
      float fracFloret(void);
      float fracFloret1(void);
      void doDmDemand1(float  dlt_dm_supply);
      void doDmDemand2(float  dlt_dm_supply);

      float cExtinctionCoeffFloret;
      float cSpec_Floret_area;
      float cRue_Floret;

      float gPai;
      float gDlt_pai;
      float cSvp_fract;
      float cFrac_Floret[max_table];                        // fraction of dm allocated to Floret
      float cX_stage_no_partition[max_table];
      float cY_frac_Floret[max_table];                      // fraction of dm allocated to Floret
      int   cNum_stage_no_partition;
      float cFloret_trans_frac;                            // fraction of Floret used in translocat
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

      Cover coverFloret;

};

#endif /* FloretPartH */
