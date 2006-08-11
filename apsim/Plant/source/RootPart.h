#ifndef RootPartH
#define RootPartH
#include "PlantPart.h"

class plantRootPart : public plantPart
   {
   public:
      float dlayer[max_layer];                         // thickness of soil layer I (mm)
      float ll15_dep[max_layer];
      float dul_dep[max_layer];                        // drained upper limit soil water content for soil layer L (mm water)
      float sat_dep[max_layer];
      float bd[max_layer];
      float sw_dep[max_layer];                         // soil water content of layer L (mm)
      float no3gsm [max_layer];                         // nitrate nitrogen in layer L (g N/m^2)
      float nh4gsm [max_layer];                         // nitrate nitrogen in layer L (g N/m^2)
      float dlt_nh4gsm[max_layer];                      // actual NH4 uptake from soil (g/m^2)
      float dlt_no3gsm[max_layer];                      // actual NO3 uptake from soil (g/m^2)

      float sw_lb;                                      // lower limit of soilwater  (mm/mm)
      float sw_ub;                                      // upper limit of soilwater  (mm/mm)

      float ll_dep[max_layer];                          // lower limit of plant-extractable
                                                        // soil water for soil layer L (mm)
      float dlt_sw_dep[max_layer];                      // water uptake in each layer (mm water)
      float kl[max_layer];                              // root length density factor for water
      float kl_ub;                                      // upper limit of water uptake factor
      int   num_layers;

      string  uptake_source;                            // source of uptake information
      float sw_avail_pot[max_layer];                    // potential extractable soil water (mm)
      float sw_avail[max_layer];                        // actual extractable soil water (mm)
      float sw_supply [max_layer];                      // potential water to take up (supply)
                                                        // from current soil water (mm)

      float root_depth;                                 // depth of roots (mm)
      float root_length[max_layer];                     // root length (mm/mm^2)
      float root_length_dead[max_layer];                // root length of dead population (mm/mm^2)

      float dltRootDepth;                               // increase in root depth (mm)
      vector<float> dltRootLength;
      vector<float> dltRootLengthDead;
      vector<float> dltRootLengthSenesced;
      vector<float> xf;                                 // root exploration factor (0-1)

      plantRootPart(plantInterface *p, const string &name);
      ~plantRootPart(){};

      void zeroAllGlobals(void);
      void zeroSoil(void);
      void zeroDeltas(void);
      void doRegistrations(protocol::Component *system);
      void readConstants (protocol::Component *system, const string &section);
      void readSpeciesParameters(protocol::Component *system, vector<string> &sections);
      void readRootParameters(protocol::Component *system, const char *section_name);
      void onSowing(void);
      void onGermination(void);
      void onEmergence(void);
      void onFlowering(void);
      void onStartGrainFill(void);
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
      void onKillStem(void);
      void update();
      void updateOthers();
      void checkBounds(void);
      void sen_length(void);
      virtual void root_length_growth (void) = 0;
      virtual void plant_root_depth (void);

      void collectDetachedForResidue(vector<string> &part_name
                                          , vector<float> &dm_residue
                                          , vector<float> &dm_n
                                          , vector<float> &dm_p
                                          , vector<float> &fract);
      void collectDeadDetachedForResidue(vector<string> &part_name
                                          , vector<float> &dm_dead_detached
                                          , vector<float> &n_dead_detached
                                          , vector<float> &p_dead_detached
                                          , vector<float> &fract);
      void doNConccentrationLimits(void);
      void redistribute(const vector<float> &, const vector<float> &, float);
      int find_layer_no(float) const;
      int find_layer_no(float,const vector<float>&);
      int find_layer_no(float, float *, int);
      float sw_avail_ratio(int layer) const;

void plantRootPart::plant_water_stress (
                                       float sw_demand,
                                       float& swdef_photo,
                                       float& swdef_pheno,
                                       float& swdef_pheno_flower,
                                       float& swdef_pheno_grainfill,
                                       float& swdef_expansion,
                                       float& swdef_fixation );

void CalcWaterSupply();
void doWaterUptake(float sw_demand);
void getOtherVariables(protocol::Component *system);
void UpdateOtherVariables(protocol::Component *system);
void DoIDs(protocol::Component *system);

float plantRootPart::oxdef_stress ();
void plantRootPart::doNewProfile(protocol::Variant &v);

   private:

// IDs for gets sets etc
     unsigned int sw_dep_id;
     unsigned int dlt_sw_dep_id;


// External Getters and Setters etc
void plantRootPart::get_sw_uptake(protocol::Component *system, protocol::QueryValueData &qd);
void plantRootPart::get_sw_supply(protocol::Component *system, protocol::QueryValueData &qd);
void plantRootPart::get_sw_supply_layr(protocol::Component *system, protocol::QueryValueData &qd);
void plantRootPart::get_ep(protocol::Component *system, protocol::QueryValueData &qd);
void plantRootPart::get_esw_layr(protocol::Component *system, protocol::QueryValueData &qd);
void plantRootPart::get_no3_uptake(protocol::Component *system, protocol::QueryValueData &qd);
void plantRootPart::get_nh4_uptake(protocol::Component *system, protocol::QueryValueData &qd);
void plantRootPart::get_no3_tot(protocol::Component *system, protocol::QueryValueData &qd);
void plantRootPart::get_ll_dep(protocol::Component *systemInterface, protocol::QueryValueData &qd);
void plantRootPart::get_ll(protocol::Component *systemInterface, protocol::QueryValueData &qd);

      float sw_dep_ub;                                  // upper limit of soilwater depth (mm)
      float sw_dep_lb;                                  // lower limit of soilwater depth (mm)


      void get_rlv(protocol::Component *system, protocol::QueryValueData &qd);
      void get_root_length(protocol::Component *system, protocol::QueryValueData &qd);
      void get_root_length_dead(protocol::Component *system, protocol::QueryValueData &qd);

      float root_proportion(int layer);
      void root_dist(float root_sum, vector<float> &root_array);
      void root_dist_dead(float root_sum, vector<float> &root_array);
      void root_incorp (float  dlt_dm_root, float dlt_n_root, float dlt_p_root);
      void root_incorp_dead (float  dlt_dm_root, float dlt_n_root, float dlt_p_root);

      float initialRootDepth;                         // initial depth of roots (mm)
      float n_conc_min, n_conc_crit, n_conc_max;
      float rootDieBackFraction;                      // fraction of roots dying at harvest or kill_stem

      unsigned int incorp_fom_ID;

      int   num_sw_avail_ratio;
      float x_sw_avail_ratio [max_table];
      float y_swdef_pheno [max_table];

      int        num_sw_avail_ratio_flower;
      float      x_sw_avail_ratio_flower[max_table];
      float      y_swdef_pheno_flower [max_table];

      int        num_sw_avail_ratio_grainfill;
      float      x_sw_avail_ratio_grainfill [max_table];
      float      y_swdef_pheno_grainfill [max_table];

      int   num_sw_demand_ratio;
      float x_sw_demand_ratio [max_table];
      float y_swdef_leaf [max_table];

      int   num_sw_avail_fix;
      float x_sw_avail_fix [max_table];
      float y_swdef_fix [max_table];

      float oxdef_photo [max_table];
      float oxdef_photo_rtfr[max_table];
      int   num_oxdef_photo;

   protected:

      float specificRootLength;
      interpolationFunction rel_root_rate;
      interpolationFunction sw_fac_root;
      interpolationFunction rel_root_advance;
      interpolationFunction ws_root_fac;
      lookupFunction root_depth_rate;
   };

class rootGrowthOption1 : public plantRootPart
   {
 public:
   rootGrowthOption1(plantInterface *p, const string &name) : plantRootPart(p, name) {};
   void root_length_growth (void);
   };

class rootGrowthOption2 : public plantRootPart
   {
 private:
   float rootDistributionPattern;
 public:
   rootGrowthOption2(plantInterface *p, const string &name) : plantRootPart(p, name) {};
   void readSpeciesParameters(protocol::Component *system, vector<string> &sections);
   void root_length_growth (void);
   };

plantRootPart* constructRootPart(plantInterface *p, const string &type, const string &name);
#endif /* RootPartH */
