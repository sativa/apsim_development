#ifndef PlantPartsH
#define PlantPartsH

// Next iteration will provide this as a generic plant part. For now, it's a stem.
class plantPart : public plantThing {
      friend class Plant;
   public: // for now
   // (global) state variables
   struct {
      float dm_dead;                      // dry wt of dead plants (g/m^2)
      float dm_green;                     // live plant dry weight (biomass) (g/m^2)
      float dm_senesced;                  // senesced plant dry wt (g/m^2)
//      float dlt_dm_green_dead;                // plant biomass to dead population(g/m^2)
      float n_dead;                       // plant N content of dead plants (g N/m^2)
      float n_green;                      // plant nitrogen content (g N/m^2)
      float n_senesced;                   // plant N content of senesced plant (g N/m^2)
//      float dlt_n_green_dead;                // plant biomass to dead population(g/m^2)
      float height;                       // The height of this part (mm)
      float width;                        // The width of this part (mm)
      float n_conc_crit;                  // critical N concentration (g N/g biomass)
      float n_conc_max;                   // maximum N concentration (g N/g biomass)
      float n_conc_min;                   // minimum N concentration (g N/g biomass)
      float dm_plant_min;                 // minimum weight of each plant part (g/plant)

      float p_green;
      float p_sen;
      float p_dead;
   } g;

   // deltas
   struct {
      float dm_green;                     // biomass growth (g/m^2)
      float dm_senesced;                  // biomass senescence (g/m^2)
      float dm_detached;                  // biomass detached (g/m^2)
      float dm_dead;                      // biomass dead (g/m^2)
      float dm_dead_detached;             // biomass detached from dead plant (g/m^2)
      float dm_green_dead;                // plant biomass to dead population(g/m^2)
      float dm_senesced_dead;             // plant biomass to dead population(g/m^2)
      float dm_green_retrans;             // biomass retranslocated (g/m^2)
      float n_green;                      // actual N uptake into plant (g/m^2)
      float n_senesced;                   // actual N loss with senesced plant (g/m^2)
      float n_senesced_retrans;
      float n_senesced_trans;
      float n_detached;                   // actual N loss with detached plant (g/m^2)
      float n_dead;                       // actual N loss with dead plant (g/m^2)
      float n_dead_detached;              // actual N loss with detached dead plant (g/m^2)
      float n_green_dead;                 // plant N to dead population(g/m^2)
      float n_senesced_dead;              // plant N to dead population(g/m^2)
      float n_retrans;                    // nitrogen retranslocated out from parts to <<somewhere else??>> (g/m^2)

      float p_green;
      float p_sen;
      float p_det;
      float p_dead_det;
      float p_retrans;
      float p_dead;

      float height;                       // growth upwards (mm)
      float width;                        // growth outwards (mm)
   } dlt;

   // "Variables"
   struct {
      float dm_green_demand;              // biomass demand (g/m^2)
      float n_capacity;                   // amount of nitrogen this part can take(g/m^2)
      float n_demand ;                    // critical plant nitrogen demand (g/m^2)
      float soil_n_demand;
      float n_max ;                       // maximum plant nitrogen demand (g/m^2)
      float p_demand;
   } v;

   // "Constants"
   struct {
      float dm_init;                      // Initial value
      float n_init_conc;                  // Initial N value
      float p_init_conc;                  // Initial P value
      float n_sen_conc;                   // N concentration of senesced material (gN/gdm)
      float trans_frac;                   // fraction of part used in translocation to grain
      float n_retrans_fraction;           // fraction of N in paret availale for retranslocation

      float dead_detach_frac;             // fraction of dead plant parts
                                          // detaching each day (0-1)

      float sen_detach_frac;              // fraction of dead plant parts
                                          // detaching each day (0-1)

      bool  p_stress_determinant;         // is a P stress_determinant
      bool  p_yield_part;                 // is a P yield_part
      bool  p_retrans_part;               // is a P retrans_part

      interpolationFunction n_conc_min;
      interpolationFunction n_conc_crit;
      interpolationFunction n_conc_max;

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
private:
   plantInterface *plant;                 // The plant we are attached to

   void onEmergence(void);
   void onFlowering(void);
   void onStartGrainFill(void);

   void get_n_conc(protocol::Component *, protocol::QueryValueData &);
   void get_n_conc_crit(protocol::Component *, protocol::QueryValueData &);
   void get_n_conc_min(protocol::Component *, protocol::QueryValueData &);
   void get_p_conc(protocol::Component *, protocol::QueryValueData &);

  public: // (for now)
   void zeroAllGlobals(void);
   void zeroDeltas(void);

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

   void doRegistrations(protocol::Component *);
   void readConstants (protocol::Component *, const string &);
   void readSpeciesParameters (protocol::Component *, vector<string> &);
   void readCultivarParameters (protocol::Component *, const string &);

   void onPlantEvent(const string &);

   void prepare(void);
   void process(void);
   void update(void);

   void morphology(void);
   void n_conc_limits(void);

   void doNDemand1(float, float);
   void doNDemand2(float, float);
   void doSoilNDemand(void);
   void doSenescence1(float);
   void doSenescence2(float);
   void doNSenescence(void);
   void dm_detachment1(void);
   void n_detachment1(void);

   void onHarvest(float height, float remove_fr,
                  vector<string> &dm_type,
                  vector<float> &dlt_crop_dm,
                  vector<float> &dlt_dm_n,
                  vector<float> &dlt_dm_p,
                  vector<float> &fraction_to_residue);

   void onEndCrop(vector<string> &dm_type,
                  vector<float> &dlt_crop_dm,
                  vector<float> &dlt_dm_n,
                  vector<float> &dlt_dm_p,
                  vector<float> &fraction_to_residue);

   float availableRetranslocateN(void);
   const string &name(void) {return c.name;};
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
};

#endif /* PlantPartsH */