#ifndef RootPartH
#define RootPartH
#include "RootBase.h"
#include "Soil.h"

class RootPart : public RootBase
   {
   public:
      ~RootPart(){};
      Soil soil;
      void zeroAllGlobals(void);
      void zeroDeltas(void);
      void onInit1(protocol::Component *system);
      virtual void read();
      void onSowing(void);
      void onGermination(void);
      void onEmergence(void);
      void onTransplanting(void);
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
      void removeBiomass2(float chop_fr);
      void sen_length(void);
      virtual void root_length_growth (void) = 0;
      virtual void plant_root_depth (void);

      void collectDetachedForResidue(vector<string> &part_name
                                          , vector<float> &dm_residue
                                          , vector<float> &dm_n
                                          , vector<float> &dm_p
                                          , vector<float> &fract);
      void doNConccentrationLimits(float);
      void redistribute(const vector<float> &, const vector<float> &, float);

      float sw_avail_ratio(int layer);

      void doPlantWaterStress (float sw_demand, SWStress *swStress);

      void doWaterSupply();
      float waterUptake(void);
      void getOtherVariables();
      void UpdateOtherVariables();

      float wet_root_fr (void);
      virtual void write();

      float plant_nit_supply(float biomass, float stageNumber, float swdef_fixation);
      void doNUptake(float sumNMax, float sumSoilNDemand, float NDemand);
      void doWaterUptake (int option, float SWDemand);
      float peswTotal();
      float pesw(int depth);
      float swSupply();
      float swAvailable();
      float swAvailablePotential();
      float nUptake();
      float fasw(int depth);


   protected:
      RootPart(ScienceAPI& scienceAPI, plantInterface *p, const string &name);


      vector<float> dltRootLength;
      float root_length[max_layer];                     // root length (mm/mm^2)
      float dltRootDepth;                               // increase in root depth (mm)
      float root_depth;                                 // depth of roots (mm)
      float specificRootLength;
      interpolationFunction rel_root_rate;
      interpolationFunction sw_fac_root;

   private:

      interpolationFunction rel_root_advance;
      interpolationFunction ws_root_fac;
      lookupFunction root_depth_rate;

      string uptake_source;                            // source of uptake information
      float root_length_senesced[max_layer];                // root length of senesced material (mm/mm^2)
      vector<float> dltRootLengthDead;
      vector<float> dltRootLengthSenesced;
      float initialRootDepth;                         // initial depth of roots (mm)
      float n_conc_min, n_conc_crit, n_conc_max;
      float rootDieBackFraction;                      // fraction of roots dying at harvest or kill_stem

      unsigned int incorp_fom_ID;

      int   n_uptake_option;
      float n_fix_rate[max_table];                      // potential rate of N fixation (g N fixed
      float n_stress_start_stage;
      float total_n_uptake_max;
      float no3_diffn_const;                            // time constant for uptake by
                                                        // diffusion (days). H van Keulen &
                                                        // NG Seligman. Purdoe 1987. This is the
                                                        // time it would take to take up by
                                                        // diffusion the current amount of N if
                                                        // it wasn't depleted between time steps

      float no3_uptake_max;
      float no3_conc_half_max;
      float kno3;
      float no3ppm_min;
      float knh4;
      float nh4ppm_min;
      string crop_type;
      float n_fix_pot;                                  // N fixation potential (g/m^2)
      string n_supply_preference;                        // preference of n supply



      // External Getters and Setters etc
      void get_sw_uptake(protocol::Component *system, protocol::QueryValueData &qd);
      void get_sw_supply(protocol::Component *system, protocol::QueryValueData &qd);
      void get_sw_supply_layr(protocol::Component *system, protocol::QueryValueData &qd);
      void get_ep(protocol::Component *system, protocol::QueryValueData &qd);
      void get_esw_layr(protocol::Component *system, protocol::QueryValueData &qd);
      void get_no3_uptake(protocol::Component *system, protocol::QueryValueData &qd);
      void get_nh4_uptake(protocol::Component *system, protocol::QueryValueData &qd);
      void get_no3_tot(protocol::Component *system, protocol::QueryValueData &qd);
      void get_ll(protocol::Component *systemInterface, protocol::QueryValueData &qd);
      void get_no3gsm_uptake_pot(protocol::Component *, protocol::QueryValueData &);
      void get_nh4gsm_uptake_pot(protocol::Component *, protocol::QueryValueData &);
      void get_n_supply_soil(protocol::Component *system, protocol::QueryValueData &qd);
      void get_rlv(protocol::Component *system, protocol::QueryValueData &qd);
      void get_root_length(protocol::Component *system, protocol::QueryValueData &qd);
      void get_root_length_senesced(protocol::Component *system, protocol::QueryValueData &qd);



      void root_dist(float root_sum, vector<float> &root_array);
      void root_dist_dead(float root_sum, vector<float> &root_array);
      void root_incorp (float  dlt_dm_root, float dlt_n_root, float dlt_p_root);
      void root_incorp_dead (float  dlt_dm_root, float dlt_n_root, float dlt_p_root);


      void rootDist(float root_sum, std::vector<float>& rootArray);

   };


#endif /* RootPartH */
