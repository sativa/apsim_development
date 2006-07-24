#ifndef cohortingLeafPartH
#define cohortingLeafPartH

class cohortingLeafPart : public plantLeafPart {
  public:
   cohortingLeafPart(plantInterface *p, const string &name) : plantLeafPart(p, name) {};
   ~cohortingLeafPart() {};
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
   void onKillStem(void);

   float getLAI(void) const
     { return sum(gLeafArea) * plant->getPlants() * smm2sm; };
   float getSLAI(void) const
     { return sum(gLeafAreaSen) * plant->getPlants() * smm2sm; };
   float getTLAI_dead(void) const {return gTLAI_dead;};
   float getLeafNo(void) const;
   float senFract (void) const;
   float dmGreenDemand(void) const
     { return(divide (dltLAI_stressed, cSLAMin * smm2sm, 0.0));};   // Maximum DM this part can take today

   float dltLeafAreaPot(void) {return (divide(dltLAI_stressed, plant->getPlants(), 0.0) * sm2smm);};
   void potential (int option, float, float);
   void leaf_area_stressed(float);
   void actual(void);

   void leaf_death (float nfact_expansion, float  dlt_tt);
   void leaf_area_sen (float, float);
   void detachment (void);
   void update (void) ;

   void remove_biomass_update(void);
   void remove_detachment (float dlt_slai_detached, float dlt_lai_removed );

  private:
   void get_tlai(protocol::Component *system, protocol::QueryValueData &qd);
   void get_lai_sum(protocol::Component *system, protocol::QueryValueData &qd);
   void get_node_no(protocol::Component *system, protocol::QueryValueData &qd);
   void get_node_no_sen(protocol::Component *system, protocol::QueryValueData &qd);
   void get_node_no_fx(protocol::Component *system, protocol::QueryValueData &qd);
   void get_leaf_no(protocol::Component *system, protocol::QueryValueData &qd);
   void get_leaf_area_index(protocol::Component *system, protocol::QueryValueData &qd);
   void get_sen_leaf_area_index(protocol::Component *system, protocol::QueryValueData &qd);
   void get_leaf_area(protocol::Component *system, protocol::QueryValueData &qd);
   void get_leaf_area_max(protocol::Component *system, protocol::QueryValueData &qd);
   void get_leaf_age(protocol::Component *system, protocol::QueryValueData &qd);
   void get_leaf_area_tot(protocol::Component *system, protocol::QueryValueData &qd);
   void get_dlt_slai(protocol::Component *system, protocol::QueryValueData &qd);
   void get_dlt_slai_age(protocol::Component *system, protocol::QueryValueData &qd);

   void leaf_no_pot (float, float);
   void leaf_area_potential(float);
   void leaf_area_actual(void);
   void leaf_no_actual (void);
   void initialiseAreas(void);

   float gTLAI_dead;                                  // total lai of dead plants
   float gNodeNo;                                     // number of nodes ()
   vector<float> gLeafNo;                             // number of leaves in each cohort
   vector<float> gLeafArea;                           // leaf area of each cohort (mm^2)
   vector<float> gLeafAreaMax;                        // Maxiumum leaf area of each cohort (mm^2)
   vector<float> gLeafAreaSen;                        // senesced leaf area of each cohort (mm^2)
   vector<float> gLeafAge;                            // age (TT) of each cohort (oC^d)
   vector<float> gDltLeafAreaPot;                     // Pot daily increase in leaf area
   float gLeavesPerNode;

   float cInitialTPLA;                                // initial plant leaf area (mm^2)
   float cLeafNumberAtEmerg;                          // leaf number at emergence ()
   float cFrLeafSenRate;                              // rate of leaf senescence
   float cSenStartStage;                              // Phenological stage when leaves start senescing
   float cNFactLeafSenRate;
   float cNodeSenRate;
   float cSLAMin;                                     // minimum specific leaf area for
                                                      // new leaf area (mm^2/g)
   float cMinTPLA;                                    // minimum plant leaf area(mm2/plant)
   float cLAISenLight;                                // critical lai above which light
   float cSenLightSlope;                              // slope of linear relationship
                                                      // between lai and
                                                      // light competition factor for
                                                      // determining leaf senesence rate.
   float cSenRateWater;                               // slope in linear eqn
                                                      // relating soil water
                                                      // stress during photosynthesis
                                                      // to leaf senesense rate

//   lookupFunction cGrowthPeriod;                      // Growth period of each leaf cohort (oC)
//   lookupFunction cLagPeriod;                         // Stable period of each leaf cohort (oC)
//   lookupFunction cSenescingPeriod;                   // Senescence period of each leaf cohort (oC)
//   lookupFunction cAreaPot;                            // max leaf area of each leaf cohort (mm^2)

   interpolationFunction cGrowthPeriod;                      // Growth period of each leaf cohort (oC)
   interpolationFunction cLagPeriod;                         // Stable period of each leaf cohort (oC)
   interpolationFunction cSenescingPeriod;                   // Senescence period of each leaf cohort (oC)
   interpolationFunction cAreaPot;                            // max leaf area of each leaf cohort (mm^2)

   interpolationFunction cNodeAppRate;
   interpolationFunction cLeavesPerNode;
   interpolationFunction cLeafSize;
   interpolationFunction cSLAMax;
   interpolationFunction cLeafNoFrac;
   interpolationFunction cSenescenceFac;              // temperature senescence table (oC)

   float dltLAI;                                      // area of leaf
   float dltLAI_pot;                                  // potential change in live plant lai
   float dltLAI_stressed;                             // potential change in lai allowing for stress
   float dltLAI_carbon;                               // potential change in lai allowing for growth
   float dltTLAI_dead;                                // plant lai change in dead plant
   float dltTLAI_dead_detached;                       // plant lai detached from dead plant
   float dltSLAI_detached;                            // senesced lai detached
   vector<float> dltSLA_age;                          // area that senesces from each leaf via age (mm^2)
   float dltSLAI_light;                               // senesced lai from light
   float dltSLAI_water;                               // senesced lai from water
   float dltSLAI_frost;                               // senesced lai from frost
   float dltTT;

   float dltLeafNoSen;                               // fraction of oldest green leaf senesced ()
   float dltLeafNo;                                // actual fraction of oldest leaf expanding ()
   float dltNodeNo;                                // actual fraction of oldest node expanding ()
   float dltLeafNoPot;                            // potential fraction of oldest leaf expanding ()
};


#endif
