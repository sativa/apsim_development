#ifndef RootPartH
#define RootPartH
#include "PlantParts.h"

class plantRootPart : public plantPart
   {
   public:
      float root_depth;                                 // depth of roots (mm)
      float root_length[max_layer];                     // root length (mm/mm^2)
      float root_length_dead[max_layer];                // root length of dead population (mm/mm^2)      

      float dltRootDepth;                               // increase in root depth (mm)
      vector<float> dltRootLength;
      vector<float> dltRootLengthDead;
      vector<float> dltRootLengthSenesced;
      vector<float> xf;                                 // root exploration factor (0-1)
            
      plantRootPart(plantInterface *p, const string &name) : plantPart(p, name) {};
      ~plantRootPart() {};

      void zeroAllGlobals(void);
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
      void update2(float);
      void checkBounds(void);
      void sen_length(void);
      virtual void root_length_growth (void) = 0;
      virtual void plant_root_depth (void);

      void collectDetachedForResidue(vector<string> &part_name
                                          , vector<float> &dm_residue
                                          , vector<float> &dm_n
                                          , vector<float> &dm_p
                                          , vector<float> &fraction_to_residue);
      void collectDeadDetachedForResidue(vector<string> &part_name
                                          , vector<float> &dm_dead_detached
                                          , vector<float> &n_dead_detached
                                          , vector<float> &p_dead_detached
                                          , vector<float> &fraction_to_residue);
      void doNConccentrationLimits(void);
      void redistribute(const vector<float> &, const vector<float> &, float);
   private:
      void get_rlv(protocol::Component *system, protocol::QueryValueData &qd);
      void get_root_length(protocol::Component *system, protocol::QueryValueData &qd);
      void get_root_length_dead(protocol::Component *system, protocol::QueryValueData &qd);

      float root_proportion(int layer);                                    
      void root_dist(float root_sum, vector<float> &root_array);

      float initialRootDepth;                         // initial depth of roots (mm)
      float n_conc_min, n_conc_crit, n_conc_max;
      float rootDieBackFraction;                      // fraction of roots dying at harvest or kill_stem

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
