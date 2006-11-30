#ifndef LeafPartH
#define LeafPartH

// Abstract class for leaf objects
class plantLeafPart : public plantPart {
  public:
   plantLeafPart(plantInterface *p, const string &name) : plantPart(p, name) {};
   virtual ~plantLeafPart() {};

   virtual void onHarvest(float height, float remove_fr,
                  vector<string> &dm_type,
                  vector<float> &dlt_crop_dm,
                  vector<float> &dlt_dm_n,
                  vector<float> &dlt_dm_p,
                  vector<float> &fraction_to_residue) =0;

   virtual void zeroAllGlobals(void) =0;
   virtual void zeroDeltas(void) =0;
   virtual void checkBounds(void) =0;
   virtual void doRegistrations(protocol::Component *) =0;
   virtual void readConstants (protocol::Component *, const string &) =0;
   virtual void readSpeciesParameters(protocol::Component *system, vector<string> &search_order) =0;
   virtual void onEmergence(void) =0;
   virtual float getLeafNo(void) const = 0;                          // The number of leaves 
   virtual float senFract (void) const = 0;                          // Fraction of canopy senescing today
   virtual float dltDmCapacity(void) const =0;                       // Maximum DM this part can take today

   virtual void potential (int option, float, float) =0;             // Calculate potentials
   virtual void leaf_area_stressed(float) =0;                        // the leaf area development from the
                                                                     //   potential daily increase in lai and stress factors
   virtual void actual(void) =0;                                     // Calculate actual leaf development from potential & stressed
   virtual void leaf_death (float nfact_expansion, float  dlt_tt) =0;// 
   virtual void leaf_area_sen (float, float) =0;
   virtual void detachment (void) =0;
   virtual void update (void) = 0;
   virtual void update2(float dying_fract_plants) =0;

   virtual void remove_detachment (float dlt_slai_detached, float dlt_lai_removed ) =0;

   float gLAI;                                        // area of leaf
   float gSLAI;                                       // area of leaf senesced from plant
   float gTLAI_dead;                                  // total lai of dead plants

   float cInitialTPLA;                                // initial plant leaf area (mm^2)  XX needed by Plant::remove_biomass
};

#define max_node 1000                         // maximum number of plant nodes
class genericLeafPart : public plantLeafPart {
  private:
   void leaf_no_pot (int option, float, float);
   void leaf_area_potential(void);
   void leaf_area_actual(void);
   void leaf_no_actual (void);
   void initialiseAreas(void);

  public: 
   genericLeafPart(plantInterface *p, const string &name) : plantLeafPart(p, name) {};
   ~genericLeafPart() {};
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
   void readConstants (protocol::Component *, const string &);
   void readSpeciesParameters(protocol::Component *system, vector<string> &search_order);
   void onEmergence(void);

  private:
   void get_tlai(protocol::Component *system, protocol::QueryValueData &qd);
   void get_lai_sum(protocol::Component *system, protocol::QueryValueData &qd);
   void get_leaf_no(protocol::Component *system, protocol::QueryValueData &qd);
   void get_leaf_no_sen(protocol::Component *system, protocol::QueryValueData &qd);
   void get_leaf_area(protocol::Component *system, protocol::QueryValueData &qd);
   void get_leaf_area_tot(protocol::Component *system, protocol::QueryValueData &qd);

  public:   
   float getLeafNo(void) const;
   float senFract (void) const
     { return(divide (dltSLAI, gLAI + dltLAI, 0.0)); };             // fraction of canopy senescing
   float dltDmCapacity(void) const 
     { return(divide (dltLAI_stressed, cSLAMin * smm2sm, 0.0));};   // Maximum DM this part can take today

   void potential (int option, float, float);
   void leaf_area_stressed(float);
   void actual(void);   

   void leaf_death (float nfact_expansion, float  dlt_tt);
   void leaf_area_sen (float, float);
   void detachment (void);
   void update (void) ;
   void update2(float dying_fract_plants);

   void remove_detachment (float dlt_slai_detached, float dlt_lai_removed );
   //void onRemoveBiomass(float );

 
  private:
   float gNodeNo;                                     // number of fully expanded nodes ()
   float gLeafNo[max_node];                           // number of fully expanded leaves ()
   float gLeafNoSen[max_node];                        // no of senesced leaves ()
   float gLeafArea[max_node];                         // leaf area of each leaf (mm^2)
   float gLeavesPerNode;

   float cLeafNumberAtEmerg;                          // leaf number at emergence ()
   float cFrLeafSenRate;                              // rate of leaf senescence
   float cSenStartStage;                              // Phenological stage when leaves start senescing
   float cNFactLeafSenRate;
   float cNodeSenRate;
   float cSLAMin;                                     // minimum specific leaf area for
                                                      // new leaf area (mm^2/g)
   float cMinTPLA;                                    // minimum plant leaf area(mm2/plant)
   float cNodeNoCorrection;                           //
   float cLAISenLight;                                // critical lai above which light
   float cSenLightSlope;                              // slope of linear relationship
                                                      // between lai and
                                                      // light competition factor for
                                                      // determining leaf senesence rate.
   float cSenRateWater;                               // slope in linear eqn
                                                      // relating soil water
                                                      // stress during photosynthesis
                                                      // to leaf senesense rate

   interpolationFunction cNodeAppRate;
   interpolationFunction cLeavesPerNode;
   interpolationFunction cLeafSize;
   interpolationFunction cSLAMax;
   interpolationFunction cLeafNoFrac;
   interpolationFunction cSenescenceFac;              // temperature senescence table (oC) 

   float dltLAI;                                      // area of leaf 
   float dltSLAI;                                     // area of leaf that senesces from plant
   float dltLAI_pot;                                  // potential change in live plant lai
   float dltLAI_stressed;                             // potential change in lai allowing for stress
   float dltTLAI_dead;                                // plant lai change in dead plant
   float dltTLAI_dead_detached;                       // plant lai detached from dead plant
   float dltSLAI_detached;                            // senesced lai detached
   float dltSLAI_age;                                 // senesced lai from age
   float dltSLAI_light;                               // senesced lai from light
   float dltSLAI_water;                               // senesced lai from water
   float dltSLAI_frost;                               // senesced lai from frost

   float dltLeafNoSen;                               // fraction of oldest green leaf senesced ()
   float dltLeafNo;                                // actual fraction of oldest leaf expanding ()
   float dltNodeNo;                                // actual fraction of oldest node expanding ()
   float dltLeafNoPot;                            // potential fraction of oldest leaf expanding ()
   float dltNodeNoPot;                            // potential fraction of oldest leaf expanding ()
};

class ahmadsLeafPart : public plantLeafPart {
  public:
   ahmadsLeafPart(plantInterface *p, const string &name) : plantLeafPart(p, name) {};
   ~ahmadsLeafPart() {};

   void onHarvest(float height, float remove_fr,
                  vector<string> &dm_type,
                  vector<float> &dlt_crop_dm,
                  vector<float> &dlt_dm_n,
                  vector<float> &dlt_dm_p,
                  vector<float> &fraction_to_residue) {};

   void zeroAllGlobals(void) {};
   void zeroDeltas(void) {};
   void checkBounds(void) {};
   void doRegistrations(protocol::Component *) {};
   void readConstants (protocol::Component *, const string &) {};
   void readSpeciesParameters(protocol::Component *system, vector<string> &search_order) {};
   void onEmergence(void) {};
   float getLeafNo(void) const {};
   float senFract (void) const {};                           // fraction of canopy senescing
   float dltDmCapacity(void) const {};                       // Maximum DM this part can take today

   void potential (int option, float, float) {};
   void leaf_area_stressed(float) {};
   void actual(void) {};
   void leaf_death (float nfact_expansion, float  dlt_tt) {};
   void leaf_area_sen (float, float) {};
   void detachment (void) {};
   void update (void) {};
   void update2(float dying_fract_plants) {};

   void remove_detachment (float dlt_slai_detached, float dlt_lai_removed ) {};

   float gLAI;                                        // area of leaf
   float gSLAI;                                       // area of leaf senesced from plant
   float gTLAI_dead;                                  // total lai of dead plants

   float cInitialTPLA;                                // initial plant leaf area (mm^2)  XX needed by Plant::remove_biomass
};


plantLeafPart* constructLeafPart(plantInterface *p, const string &type, const string &name);
#endif