#ifndef PlantPartsH
#define PlantPartsH

// Next iteration will provide this as a generic plant part. For now, it's a stem.
class plantPart : public plantThing {
      friend class Plant;
   public: // for now
   // (global) state variables
   struct {
      float dm_green;                     // live plant dry weight (biomass) (g/m^2)
      float dm_senesced;                  // senesced plant dry wt (g/m^2)
      float dm_dead;                      // dry wt of dead plants (g/m^2)

      float n_green;                      // plant nitrogen content (g N/m^2)
      float n_dead;                       // plant N content of dead plants (g N/m^2)
      float n_senesced;                   // plant N content of senesced plant (g N/m^2)

      float p_green;
      float p_sen;
      float p_dead;

      float height;                       // The height of this part (mm)
      float width;                        // The width of this part (mm)
      float n_conc_crit;                  // critical N concentration (g N/g biomass)
      float n_conc_max;                   // maximum N concentration (g N/g biomass)
      float n_conc_min;                   // minimum N concentration (g N/g biomass)
      float p_conc_sen;                  // critical P concentration (g N/g biomass)
      float p_conc_max;                   // maximum P concentration (g N/g biomass)
      float p_conc_min;                   // minimum P concentration (g N/g biomass)
      float dm_plant_min;                 // minimum weight of each plant part (g/plant)
   } g;

   // deltas
   struct {
      float dm_green;                     // biomass growth (g/m^2)
      float dm_senesced;                  // biomass senescence (g/m^2)
      float dm_dead;                      // biomass dead (g/m^2)

//      float dm_green_detached;            // biomass detached from green part (g/m^2)
      float dm_detached;                  // biomass detached from senesced part (g/m^2)
      float dm_dead_detached;             // biomass detached from dead plant (g/m^2)

      float dm_green_retrans;             // biomass retranslocated to/from (+/-) green part to/from <<somewhere else??>> (g/m^2)

      float dm_green_dead;                // plant biomass from green part to dead population(g/m^2)
      float dm_senesced_dead;             // plant biomass from green part to dead population(g/m^2)

      float n_green;                      // actual N uptake into plant (g/m^2)
      float n_senesced;                   // actual N loss with senesced plant (g/m^2)
      float n_dead;                       // actual N loss with dead plant (g/m^2)

//      float n_green_detached;             // actual N loss with detached green part (g/m^2)
      float n_detached;                   // actual N loss with detached senesced part (g/m^2)
      float n_dead_detached;              // actual N loss with detached dead plant (g/m^2)

      float n_retrans;                    // nitrogen retranslocated to/from (+/-) green part to/from <<somewhere else??>> (g/m^2)

      float n_green_dead;                 // plant N from green part to dead population(g/m^2)
      float n_senesced_dead;              // plant N from senesced part to dead population(g/m^2)

      float n_senesced_retrans;           // plant N retranslocated to/from (+/-) senesced part to/from <<somewhere else??>> (g/m^2)
      float n_senesced_trans;

      float p_green;
      float p_sen;
      float p_dead;
      float p_det;

      float p_dead_det;
      float p_retrans;

      float height;                       // growth upwards (mm)
      float width;                        // growth outwards (mm)
   } dlt;

   // "Variables"
   struct {
      float dm_green_demand;              // biomass demand (g/m^2)
      float n_demand ;                    // critical plant nitrogen demand (g/m^2)
      float p_demand;

      float soil_n_demand;
      float n_capacity;                   // amount of nitrogen this part can take(g/m^2)
      float n_max ;                       // maximum plant nitrogen demand (g/m^2)
   } v;

   // "Constants"
   struct {
      float dm_init;                      // Initial value
      float n_init_conc;                  // Initial N value
      float p_init_conc;                  // Initial P value
      float n_sen_conc;                   // N concentration of senesced material (gN/gdm)

      float trans_frac;                   // fraction of part used in translocation to grain
      float n_retrans_fraction;           // fraction of N in paret availale for retranslocation

      float dead_detach_frac;             // fraction of dead plant parts detaching each day (0-1)
      float sen_detach_frac;              // fraction of dead plant parts detaching each day (0-1)

      bool  p_stress_determinant;         // is a P stress_determinant
      bool  p_yield_part;                 // is a P yield_part
      bool  p_retrans_part;               // is a P retrans_part

      bool  stress_determinant;           // is a stress_determinant
      bool  yield_part;                   // is a yield_part
      bool  retrans_part;                // is a retrans_part

      float n_deficit_uptake_fraction;    // xxxxxxxxxx

      interpolationFunction n_conc_min;
      interpolationFunction n_conc_crit;
      interpolationFunction n_conc_max;

      interpolationFunction p_conc_min;
      interpolationFunction p_conc_sen;
      interpolationFunction p_conc_max;
      int   num_x_p_stage_code;
      float x_p_stage_code [max_table];
      float y_p_conc_min[max_table];
      float y_p_conc_sen[max_table];
      float y_p_conc_max[max_table];

      interpolationFunction height;
      interpolationFunction width;

      interpolationFunction dm_sen_frac;
      interpolationFunction fr_remain;
      string name;                        // What we call ourselves
   } c;
protected:
   plantInterface *plant;                 // The plant we are attached to

   virtual void onEmergence(void);
   virtual void onFlowering(void);
   virtual void onStartGrainFill(void);

private:
   void get_n_conc(protocol::Component *, protocol::QueryValueData &);
   void get_n_conc_crit(protocol::Component *, protocol::QueryValueData &);
   void get_n_conc_min(protocol::Component *, protocol::QueryValueData &);
   void get_p_conc(protocol::Component *, protocol::QueryValueData &);

  public: // (for now)
   plantPart(plantInterface *p, const string &name)
     {
     zeroAllGlobals();
     plant = p;
     c.name = name;
     c.trans_frac = 0.0;
     c.n_retrans_fraction = 1.0;
     c.p_stress_determinant = false;
     c.p_yield_part = false;
     c.p_retrans_part = false;
     };
   virtual ~plantPart() {};

   virtual void zeroAllGlobals(void);
   virtual void zeroDeltas(void);
   virtual void zeroDltNSenescedTrans(void);
   virtual void checkBounds(void);

   virtual void doRegistrations(protocol::Component *);
   virtual void readConstants (protocol::Component *, const string &);
   virtual void readSpeciesParameters (protocol::Component *, vector<string> &);
   virtual void readCultivarParameters (protocol::Component *, const string &);

   virtual void onPlantEvent(const string &);

   virtual void prepare(void);
   void process(void);
   virtual void update(void);
   virtual void updateDm(void);
   virtual void updateN(void);
   virtual void updateP(void);

   virtual void morphology(void);
   virtual void n_conc_limits(void);

   virtual void doDmMin(void);
   virtual void doNDemand1(float, float);
   virtual void doNDemand2(float, float);
   virtual void doSoilNDemand(void);
   virtual void doSenescence1(float);
   virtual void doSenescence2(float);
   virtual void doNSenescence(void);
   virtual void doNSenescedRetrans(float navail, float n_demand_tot);
   virtual void doNRetranslocate( float N_supply, float g_grain_n_demand);
   virtual void dm_detachment1(void);
   virtual void n_detachment1(void);

   virtual void p_detachment1(void);
   virtual void doPDemand(void);
   virtual void doPSenescence(void);
   virtual float dmTotal(void);
   virtual float dmGreen(void) const;
   virtual float dmSenesced(void) const;
   virtual float dmDead(void) const;
   virtual float dmRetransSupply(void);
   virtual float dmRetransDemand(void);

   virtual float dmGreenStressDeterminant(void);
   virtual float pGreenStressDeterminant(void);
   virtual float pMaxPotStressDeterminant(void);
   virtual float pMinPotStressDeterminant(void);
   virtual float nTotal(void);
   virtual float nGreen(void) const;
   virtual float nSenesced(void) const;
   virtual float nDead(void) const;
   virtual float nConc(void) const;
   virtual float dltNRetransOut(void);
   virtual float dltNGreen(void);

   virtual float nMaxPot(void) ;
   virtual float nMinPot(void) ;
   virtual float pTotal(void);
   virtual float pGreen(void) const;
   virtual float pSenesced(void) const;
   virtual float pDead(void) const;
   virtual float pConc(void) const;

   virtual float pMaxPot(void) ;
   virtual float pMinPot(void) ;
   virtual void updatePDet(void) ;

   virtual float nDemand(void);
   virtual float nMax(void) const;
   virtual float nCapacity(void);
   virtual void  nPartition(float nSupply);
   virtual void  nFix(float nSupply);
   virtual float nRetransSupply(void);
   virtual float nRetransDemand(void);

   virtual float pDemand(void);
   virtual float pRetransSupply(void);
   virtual float pRetransDemand(void);

   virtual void distributeDltPGreen(float p_uptake, float total_p_demand);
   virtual void distributeDltPRetrans(float total_p_supply, float total_p_demand);
   virtual void pInit(void);

   virtual void onHarvest(float height, float remove_fr,
                          vector<string> &dm_type,
                          vector<float> &dlt_crop_dm,
                          vector<float> &dlt_dm_n,
                          vector<float> &dlt_dm_p,
                          vector<float> &fraction_to_residue) = 0;

   virtual void onEndCrop(vector<string> &dm_type,
                  vector<float> &dlt_crop_dm,
                  vector<float> &dlt_dm_n,
                  vector<float> &dlt_dm_p,
                  vector<float> &fraction_to_residue);

   virtual void onKillStem(void);
   virtual void onDayOf(const string &);

   virtual float availableRetranslocateN(void);
   const string &name(void) {return c.name;};

   virtual void get_p_demand(vector<float> &p_demand);
   virtual void get_dlt_p_green(vector<float> &dlt_p_green);
   virtual void get_dlt_p_retrans(vector<float> &dlt_p_retrans);
   virtual void get_p_green(vector<float> &p_green);
};

class plantStemPart : public plantPart {
  public:
   plantStemPart(plantInterface *p, const string &name) : plantPart(p, name) {};
   ~plantStemPart() {};
   void onHarvest(float height, float remove_fr,
                  vector<string> &dm_type,
                  vector<float> &dlt_crop_dm,
                  vector<float> &dlt_dm_n,
                  vector<float> &dlt_dm_p,
                  vector<float> &fraction_to_residue);
};

class plantLeafPart : public plantPart {
  public:
   plantLeafPart(plantInterface *p, const string &name) : plantPart(p, name) {};
   ~plantLeafPart() {};
   void onHarvest(float height, float remove_fr,
                  vector<string> &dm_type,
                  vector<float> &dlt_crop_dm,
                  vector<float> &dlt_dm_n,
                  vector<float> &dlt_dm_p,
                  vector<float> &fraction_to_residue);

   void zeroAllGlobals(void);
   void zeroDeltas(void);
   void checkBounds(void);
   void doRegistrations(protocol::Component *);
   void onEmergence(void);

   void get_tlai(protocol::Component *system, protocol::QueryValueData &qd);
   void get_lai_sum(protocol::Component *system, protocol::QueryValueData &qd);

   float gLAI;                                        // area of leaf
   float gSLAI;                                       // area of leaf senesced from plant
   float gTLAI_dead;                                  // total lai of dead plants
   float dltLAI;                                      // area of leaf
   float dltSLAI;                                     // area of leaf that senesces from plant
   float dltLAI_pot;                                  // potential change in live plant lai
   float dltLAI_stressed;                             // potential change in lai allowing for stress
   float dltTLAI_dead;                                // plant lai change in dead plant
   float dltTLAI_dead_detached;                       // plant lai detached from dead plant

};

class fruitPodPart : public plantPart {
  public:
   fruitPodPart(plantInterface *p, const string &name) : plantPart(p, name) {};
   ~fruitPodPart() {};
   void onHarvest(float height, float remove_fr,
                  vector<string> &dm_type,
                  vector<float> &dlt_crop_dm,
                  vector<float> &dlt_dm_n,
                  vector<float> &dlt_dm_p,
                  vector<float> &fraction_to_residue);

   void onKillStem(void);

   void onFlowering(void);
   void onStartGrainFill(void);
   void doDmMin(float c_pod_trans_frac);
};

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

class fruitMealPart : public plantPart {
  public:
   fruitMealPart(plantInterface *p, const string &name) : plantPart(p, name) {};
   ~fruitMealPart() {};
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



float critNFactor(vector<const plantPart *> &, float );

class plantPartHack : public plantPart {
  private:
   Plant *myplant;
   int   part;
   void get(void);
   void put(void);
 public:
   plantPartHack(Plant *p, int ipart, const string &name) : plantPart((plantInterface *)p, name) {
      part = ipart;
      myplant = p;
      get();
   };
   ~plantPartHack(){put();};
   void onHarvest(float height, float remove_fr,
                  vector<string> &dm_type,
                  vector<float> &dlt_crop_dm,
                  vector<float> &dlt_dm_n,
                  vector<float> &dlt_dm_p,
                  vector<float> &fraction_to_residue) {};
};

#endif /* PlantPartsH */