#ifndef RootBaseH
#define RootBaseH
#include "../PlantPart.h"

class RootBase : public plantPart
   {
   public:
      RootBase(ScienceAPI& scienceAPI, plantInterface *p, const string &name);
      static RootBase* construct(ScienceAPI& scienceAPI, plantInterface *p,
                                 const std::string &type, const std::string &name);
      ~RootBase(){};

      virtual void zeroAllGlobals(void){};
      virtual void zeroDeltas(void){};
      virtual void onInit1(protocol::Component *system){};
      virtual void read(){};
      virtual void onSowing(void){};
      virtual void onGermination(void){};
      virtual void onEmergence(void){};
      virtual void onFlowering(void){};
      virtual void onStartGrainFill(void){};
      virtual void onHarvest(float height, float remove_fr,
                     vector<string> &dm_type,
                     vector<float> &dlt_crop_dm,
                     vector<float> &dlt_dm_n,
                     vector<float> &dlt_dm_p,
                     vector<float> &fraction_to_residue){};
      virtual void onEndCrop(vector<string> &dm_type,
                     vector<float> &dlt_crop_dm,
                     vector<float> &dlt_dm_n,
                     vector<float> &dlt_dm_p,
                     vector<float> &fraction_to_residue){};
      virtual void onKillStem(void){};
      virtual void update(){};
      virtual void updateOthers(){};
      virtual void checkBounds(void){};
      virtual void removeBiomass2(float chop_fr){};
      virtual void sen_length(void){};
      virtual void root_length_growth (void){};
      virtual void plant_root_depth (void){};

      virtual void collectDetachedForResidue(vector<string> &part_name
                                          , vector<float> &dm_residue
                                          , vector<float> &dm_n
                                          , vector<float> &dm_p
                                          , vector<float> &fract){};
      virtual void collectDeadDetachedForResidue(vector<string> &part_name
                                          , vector<float> &dm_dead_detached
                                          , vector<float> &n_dead_detached
                                          , vector<float> &p_dead_detached
                                          , vector<float> &fract){};
      virtual void doNConccentrationLimits(float){};
      virtual void redistribute(const vector<float> &, const vector<float> &, float){};
//      virtual int find_layer_no(float) const{};
//      virtual int find_layer_no(float,const vector<float>&){};
//      virtual int find_layer_no(float, float *, int){};
      virtual float sw_avail_ratio(int layer) const{return 0;}

      virtual void plant_water_stress (
                                       float sw_demand,
                                       float& swdef_photo,
                                       float& swdef_pheno,
                                       float& swdef_pheno_flower,
                                       float& swdef_pheno_grainfill,
                                       float& swdef_expansion,
                                       float& swdef_fixation ){};

      virtual void waterSupply(){};
      virtual void doWaterUptake(float sw_demand){};
      virtual float waterUptake(void){return 0;}
      virtual void getOtherVariables(){};
      virtual void UpdateOtherVariables(){};

      virtual float oxdef_stress (){return 0;}
      virtual void onNewProfile(protocol::Variant &v){};
      virtual void write(){};

      virtual float plant_nit_supply(float biomass, float stageNumber, float swdef_fixation){return 0;}
      virtual void plant_nit_uptake(float sumNMax, float sumSoilNDemand, float NDemand){};
      virtual void plant_water_uptake (int option, float SWDemand){};
      virtual float peswTotal(){return 0;}
      virtual float pesw(int depth){return 0;}
      virtual float dltSwDep(){return 0;}
      virtual float nUptake(){return 0;}
      virtual float fasw(int depth){return 0;}


   };


#endif /* RootBaseH */
