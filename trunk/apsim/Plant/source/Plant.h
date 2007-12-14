#ifndef PlantH
#define PlantH

class ApsimVariant;
class PlantComponent;
class PlantPhenology;
class plantPart;
class SimplePart;
class plantStemPart;
class plantLeafPart;
class RootBase;
class plantThing;
class eventObserver;
class Plant;
class Arbitrator;

#include "PlantInterface.h"
#include "Environment.h"
#include "PlantSpatial.h"
#include "CompositePart.h"
#include "Population.h"
typedef bool (Plant::*ptr2setFn) (protocol::QuerySetValueData&);

typedef std::map<unsigned, ptr2setFn>   UInt2SetFnMap;
typedef std::map<unsigned, string>      UInt2StringMap;




typedef enum {photosynthetic_pathway_UNDEF, photosynthetic_pathway_C3, photosynthetic_pathway_C4} photosynthetic_pathway_t;

//      Process names used for stress
// photosynthesis flag
const int  photo = 0 ;
// cell expansion flag
const int  expansion = 1 ;
// phenological flag
const int  pheno = 2 ;
// grain concentration flag
const int  grain_conc = 3 ;
// N fixation flag
const int  fixation = 4 ;


//   This class performs crop crop growth
//     simulates root, leaf, head, stem and grain development. Water and
//     nitrogen uptake, photosynhesis, and leaf and root senescense.
class Plant : public plantInterface, public IPlant {
private:
   PlantComponent *parent;                   // for interface calls to system
   ScienceAPI& scienceAPI;
   stageSubject   stageObservers;            // A collection of state variable observers, reset at each new stage
   stageSubject   otherObservers;            // Another collection of state variable observers

   PlantSpatial plantSpatial;

   CompositePart plant;
   CompositePart tops;

   vector <plantThing *> myThings;
   vector <plantPart *> myParts;
   vector <plantPart *> myTopsParts;

   plantStemPart  *stemPart;
   plantLeafPart  *leafPart;
   RootBase  *rootPart;
   PlantPhenology *phenology;
   plantPart     *fruitPart;
   Population population;

   eventObserver *sowingEventObserver;     // Bookkeeper for Sowing events
   eventObserver *emergenceEventObserver;  // Bookkeeper for Emergence events
   eventObserver *FIEventObserver;         // Bookkeeper for Floral Initiation events
   eventObserver *floweringEventObserver;  // Bookkeeper for flowering events
   eventObserver *maturityEventObserver;   // Bookkeeper for maturity events
   Arbitrator    *arbitrator;

   float grainGreen(void);
   float grainSenesced(void);
   float grainDead(void);
   float grainTot(void);

   float grainNGreen(void);
   float grainNSenesced(void);
   float grainNDead(void);
   float grainNTot(void);

   float grainPDead(void);
   float grainPConc(void);
   float SWDemandTE(void) ;
   float SWDemand(void) ;
   void read(void);

public:
   Plant(PlantComponent *P, ScienceAPI& api);
   ~Plant(void);

   void onInit1(void);
   void onInit2(void);
   bool respondToSet(unsigned int &id, protocol::QuerySetValueData& qd) ;

   void onPrepare(unsigned &, unsigned &, protocol::Variant &) ;
   void onProcess(unsigned &, unsigned &, protocol::Variant &) ;
   void onSow(unsigned &, unsigned &, protocol::Variant &v) ;
   void onHarvest(unsigned &, unsigned &, protocol::Variant &v) ;
   void onEndCrop(unsigned &, unsigned &, protocol::Variant &v) ;
   void onKillStem(unsigned &, unsigned &, protocol::Variant &v) ;
   void onRemoveCropBiomass(unsigned &, unsigned &, protocol::Variant &v) ;
   void onDetachCropBiomass(unsigned &, unsigned &, protocol::Variant &v) ;
   void onEndRun(unsigned &, unsigned &, protocol::Variant &v) ;
   void doAutoClassChange(unsigned &, unsigned &, protocol::Variant &v) ;
   void onTick(unsigned &, unsigned &, protocol::Variant &v) ;

   void registerClassActions(void);
   void sendStageMessage(const char *what);
   void doPlantEvent(const string &);

   void writeString (const char *line);
   void warningError (const char *msg);

   const std::string & getCropType(void) ;
   protocol::Component *getComponent(void) ;
   std::string Name(void) {return g.module_name;}

   void doDmRetranslocate (void);
   void plant_temp_stress (void);
   void plant_oxdef_stress (void);
   void doDmPotTE (void);
   void plant_retrans_init (int option);
   void plant_leaf_area_potential (int option /* (INPUT) option number */);
   void plant_leaf_area_stressed (int option  /* (INPUT) option number*/);
   void plant_leaf_area_init (int option);
   void plant_leaf_area_actual (int option /* (INPUT) option number*/);
   void plant_pod_area (int option         /* (INPUT) option number*/);
   void plant_leaf_no_actual (int option   /* (INPUT) option number*/);
   void plant_nit_init (int option         /* (INPUT) option number*/);
   void doNRetranslocate (int option      /* (INPUT) option number*/);
   void doNDemand (int option       /* (INPUT) option number*/);
   void doNUptake (void);
   void doNPartition (void);
   void plant_nit_stress (int option       /* (INPUT) option number*/);
   void doSoilNDemand (void);

   void doNDemandEstimate (int option);
   void plant_height (int   option/*(INPUT) option number*/);
   void plant_width (int   option /*(INPUT) option number*/);
   void doNSenescence (int   option/*(INPUT) option number*/);
   void plant_leaf_death (int   option/*(INPUT) option number*/);
   void plant_leaf_area_sen (int   option/*(INPUT) option number*/);
   void plant_cleanup (void);
   void plant_update(void) ;
   void plant_check_bounds( float  g_cover_green
                           ,float  g_cover_sen) ;

   void plant_totals(float *g_lai_max
                     ,float  *g_n_fix_uptake
                     ,float  *g_n_fixed_tops
                    )  ;
   void plant_event(void);

   void plant_root_depth (int option /* (INPUT) option number*/);
   void plant_water_distribute (int option /*(INPUT) option number*/);
   void plant_light_supply_partition (int option /*(INPUT) option number*/);
   void plant_root_length_growth (int option /*(INPUT) option number*/);
   void plant_water_supply_partition(float sw_demand
                                     , float swDemandVeg
                                     , float swSupply
                                     , float *swSupplyVeg
                                     , float *swSupplyFruit);

   void plant_rue_co2_modifier(float co2,                 //!CO2 level (ppm)
                               float maxt,                //!daily max temp (C)
                               float mint,                //!daily min temp (C)
                               float *modifier);          //!modifier (-)

   void doNPartition(float g_n_fix_pot, float &n_fix_uptake, std::vector<plantPart *> &);

   void legnew_dm_partition1 (float c_frac_leaf
                              , float c_ratio_root_shoot
                              , double g_dlt_dm
                              , float dm_yield_demand_fruit
                              , double *dlt_dm_fruit);

   void legnew_dm_partition2 (float  g_current_stage
                              , float  *c_x_stage_no_partition
                              , float  *c_y_frac_leaf
                              , int    c_num_stage_no_partition
                              , float *c_y_ratio_root_shoot
                              , double g_dlt_dm
                              , float dm_yield_demand_fruit
                              , double *dlt_dm_fruit);

   void legnew_dm_retranslocate (vector<plantPart *> &allParts        // (INPUT) all parts of plant
                                 ,vector<plantPart *> &supply_pools   // (INPUT)
                                 ,float  g_dm_demand_differential     // (INPUT)  grain dm demand (g/m^2)
                                 ,float  g_plants                     // (INPUT)  Plant density (plants/m^2)
                                 ,float  *dlt_dm_retrans_to_fruit);   // (OUTPUT) dm retranslocated to fruit (g/m^2)

   void doNRetranslocate(float g_grain_n_demand);

   void doNSenescedRetrans (void);
   void plant_process ( void );

   bool onSetPhase (protocol::QuerySetValueData &v/*(INPUT) message variant*/);
   void plant_harvest (protocol::Variant &v/*(INPUT) message variant*/);
   void plant_detach_crop_biomass (protocol::Variant &v/*(INPUT) incoming message variant*/);
   void plant_dormancy (protocol::Variant &v/*(INPUT) incoming message variant*/);
   void plant_harvest_update (protocol::Variant &v/*(INPUT)message arguments*/);
   void plant_kill_stem_update (protocol::Variant &v/*(INPUT) message arguments*/);
   void plant_remove_biomass_update (protocol::RemoveCropDmType dmRemoved);
   void plant_zero_all_globals (void);
   void plant_zero_variables (void);
   void plant_zero_daily_variables (void);
   void plant_start_crop (protocol::Variant &v/*(INPUT) message arguments*/);
   void plant_end_crop (void);
   void plant_get_other_variables (void);
   void plant_update_other_variables (void);
   void plant_read_constants ( void );
   void plant_prepare (void);
   void plant_read_species_const (void);
   void plant_harvest_report (void);
   bool  plant_auto_class_change (const char *action);
   void plant_send_crop_chopped_event (const string&  crop_type               // (INPUT) crop type
                                       ,vector<string> &dm_type               // (INPUT) residue type
                                       ,vector<float>  &dlt_crop_dm           // (INPUT) residue weight (kg/ha)
                                       ,vector<float>  &dlt_dm_n              // (INPUT) residue N weight (kg/ha)
                                       ,vector<float>  &dlt_dm_p              // (INPUT) residue P weight (kg/ha)
                                       ,vector<float>  &fraction_to_residue);


   void plant_get_ext_uptakes (const char *uptake_source,        //(INPUT) uptake flag
                               const char *crop_type,            //(INPUT) crop type name
                               const char *uptake_type,          //(INPUT) uptake name
                               float unit_conversion_factor,     //(INPUT) unit conversion factor
                               float uptake_lbound,              //(INPUT) uptake lower limit
                               float uptake_ubound,              //(INPUT) uptake upper limit
                               float *uptake_array);             //(OUTPUT) crop uptake array

   bool set_plant_crop_class(protocol::QuerySetValueData&v);
   bool set_plant_grain_oil_conc(protocol::QuerySetValueData&v);

   void get_plant_status(protocol::Component *, protocol::QueryValueData &);
   float getStageCode(void);
   float getStageNumber(void);
   int daysInCurrentPhase(void);
   float ttInCurrentPhase(void);
   status_t Status(void) {return g.plant_status;}
   void SetStatus(status_t NewStatus) {g.plant_status = NewStatus;}
   CompositePart& Tops(void) {return tops;}

   void get_crop_type(protocol::Component *, protocol::QueryValueData &);
   void get_crop_class(protocol::Component *, protocol::QueryValueData &);
   void get_leaf_no(protocol::Component *, protocol::QueryValueData &);
   float getLeafNo(void);
   float getLAI(void);
   float getCumSwdefPheno(void);
   float getCumSwdefPhoto(void);
   float getDyingFractionPlants(void);
   void get_dlt_leaf_no(protocol::Component *, protocol::QueryValueData &);
   void get_dlt_node_no(protocol::Component *, protocol::QueryValueData &);
   void get_leaf_no_dead(protocol::Component *, protocol::QueryValueData &);
   void get_leaf_area(protocol::Component *, protocol::QueryValueData &);
   void get_height(protocol::Component *, protocol::QueryValueData &);
   void get_width(protocol::Component *, protocol::QueryValueData &);
   void get_root_depth(protocol::Component *, protocol::QueryValueData &);
   void get_plants(protocol::Component *, protocol::QueryValueData &);
   float getPlants(void);
   float getCo2(void);
   //  float getRadnInterceptedPod(void);
   float getDltDMPotRueVeg(void);
   //  float getDltDmVeg(void);
   float getDmTops(void);
   float getDltDmGreen(void);
   float getDltDm(void);
   float getDmVeg(void);
   float getDmGreenStem(void);
   float getDmGreenTot(void);

   float GreenDM(void) ;
   float GreenN(void) ;
   float GreenP(void) ;
   float SenescedDM(void) ;
   float SenescedN(void) ;
   float SenescedP(void);

// FIXME - remove next line when corrections for P demand activated
   float getRelativeGrowthRate(void);
   float getTotalPotentialGrowthRate(void);
   float getCo2ModifierRue(void);
   float getCo2ModifierTe(void);
   float getCo2ModifierNConc(void);
   float getVpd(void);

   float getTempStressPhoto(void);
   float getNfactPhoto(void);
   float getNfactGrainConc(void);
   float getOxdefPhoto(void);
   float getPfactPhoto(void);
   float getSwdefPhoto(void);

   void get_cover_tot(protocol::Component *, protocol::QueryValueData &);
   void get_lai_canopy_green(protocol::Component *, protocol::QueryValueData &);
   void get_pai(protocol::Component *, protocol::QueryValueData &);
   void get_root_wt(protocol::Component *, protocol::QueryValueData &);
   void get_leaf_wt(protocol::Component *, protocol::QueryValueData &);
   void get_stem_wt(protocol::Component *, protocol::QueryValueData &);
   void get_biomass(protocol::Component *, protocol::QueryValueData &);
   void get_green_biomass(protocol::Component *, protocol::QueryValueData &);
   void get_biomass_wt(protocol::Component *, protocol::QueryValueData &);
   void get_green_biomass_wt(protocol::Component *, protocol::QueryValueData &);
   void get_stover_biomass_wt(protocol::Component *, protocol::QueryValueData &);
   void get_dm_plant_min(protocol::Component *, protocol::QueryValueData &);
   void get_dlt_dm(protocol::Component *, protocol::QueryValueData &);
   void get_dlt_dm_pot_rue(protocol::Component *, protocol::QueryValueData &);
   void get_dlt_dm_pot_te(protocol::Component *, protocol::QueryValueData &);
   void get_dlt_dm_green_retrans(protocol::Component *, protocol::QueryValueData &);
   void get_biomass_n(protocol::Component *, protocol::QueryValueData &);
   void get_n_uptake(protocol::Component *, protocol::QueryValueData &);
   void get_green_biomass_n(protocol::Component *, protocol::QueryValueData &);
   void get_leaf_n(protocol::Component *, protocol::QueryValueData &);
   void get_stem_n(protocol::Component *, protocol::QueryValueData &);
   void get_root_n(protocol::Component *, protocol::QueryValueData &);
   void get_deadleaf_n(protocol::Component *, protocol::QueryValueData &);
   void get_temp_stress_photo(protocol::Component *, protocol::QueryValueData &);
   void get_swdef_pheno(protocol::Component *, protocol::QueryValueData &);
   void get_swdef_photo(protocol::Component *, protocol::QueryValueData &);
   void get_swdef_expan(protocol::Component *, protocol::QueryValueData &);
   void get_swdef_fixation(protocol::Component *, protocol::QueryValueData &);
   void get_swstress_pheno(protocol::Component *, protocol::QueryValueData &);
   void get_swstress_photo(protocol::Component *, protocol::QueryValueData &);
   void get_swstress_expan(protocol::Component *, protocol::QueryValueData &);
   void get_swstress_fixation(protocol::Component *, protocol::QueryValueData &);
   void get_oxdef_photo(protocol::Component *, protocol::QueryValueData &);
   void get_transp_eff(protocol::Component *, protocol::QueryValueData &);
   void get_ep(protocol::Component *, protocol::QueryValueData &);
   void get_sw_uptake(protocol::Component *, protocol::QueryValueData &);
   void get_sw_supply(protocol::Component *, protocol::QueryValueData &);
   void get_esw_layr(protocol::Component *, protocol::QueryValueData &);
   void get_n_conc_stover(protocol::Component *, protocol::QueryValueData &);
   void get_n_conc_root(protocol::Component *, protocol::QueryValueData &);
   void get_n_conc_crit(protocol::Component *, protocol::QueryValueData &);
   void get_n_conc_min(protocol::Component *, protocol::QueryValueData &);

   void get_n_uptake_stover(protocol::Component *, protocol::QueryValueData &);
   void get_no3_tot(protocol::Component *, protocol::QueryValueData &);
   void get_n_demanded(protocol::Component *, protocol::QueryValueData &);
   void get_nfact_pheno(protocol::Component *, protocol::QueryValueData &);
   void get_dlt_n_fixed_pot(protocol::Component *, protocol::QueryValueData &);
   void get_dlt_n_fixed(protocol::Component *, protocol::QueryValueData &);
   void get_nfact_photo(protocol::Component *, protocol::QueryValueData &);
   void get_nfact_expan(protocol::Component *, protocol::QueryValueData &);
   void get_nfact_grain(protocol::Component *, protocol::QueryValueData &);
   void get_nfact_grain_tot(protocol::Component *, protocol::QueryValueData &);
   void get_nstress_photo(protocol::Component *, protocol::QueryValueData &);
   void get_nstress_pheno(protocol::Component *, protocol::QueryValueData &);
   void get_nstress_expan(protocol::Component *, protocol::QueryValueData &);
   void get_nstress_grain(protocol::Component *, protocol::QueryValueData &);
   void get_rlv(protocol::Component *, protocol::QueryValueData &);
   void get_no3_demand(protocol::Component *, protocol::QueryValueData &);
   void get_sw_demand(protocol::Component *, protocol::QueryValueData &);
   void get_sw_demand_te(protocol::Component *, protocol::QueryValueData &);
   void get_root_length(protocol::Component *, protocol::QueryValueData &);
   void get_root_length_dead(protocol::Component *, protocol::QueryValueData &);

   void get_parasite_c_gain(protocol::Component *, protocol::QueryValueData &);
   void get_leaf_area_tot(protocol::Component *, protocol::QueryValueData &);
   void get_dm_parasite_retranslocate(protocol::Component *, protocol::QueryValueData &);
   void get_sw_supply_layr(protocol::Component *, protocol::QueryValueData &);
   void get_no3_uptake(protocol::Component *, protocol::QueryValueData &);
   void get_nh4_uptake(protocol::Component *, protocol::QueryValueData &);
   void get_zadok_stage(protocol::Component *, protocol::QueryValueData &);


   void get_p_demand(protocol::Component *, protocol::QueryValueData &qd);
   void get_p_demand_parts(protocol::Component *, protocol::QueryValueData &qd);
   void get_pfact_photo(protocol::Component *, protocol::QueryValueData &qd);
   void get_pfact_pheno(protocol::Component *, protocol::QueryValueData &qd);
   void get_pfact_expansion(protocol::Component *, protocol::QueryValueData &qd);
   void get_pfact_expan(protocol::Component *, protocol::QueryValueData &qd);
   void get_pfact_grain(protocol::Component *, protocol::QueryValueData &qd);
   void get_pstress_photo(protocol::Component *, protocol::QueryValueData &qd);
   void get_pstress_pheno(protocol::Component *, protocol::QueryValueData &qd);
   void get_pstress_expansion(protocol::Component *, protocol::QueryValueData &qd);
   void get_pstress_grain(protocol::Component *, protocol::QueryValueData &qd);
   void get_biomass_p(protocol::Component *, protocol::QueryValueData &qd);
   void get_p_uptake(protocol::Component *, protocol::QueryValueData &qd);
   void get_green_biomass_p(protocol::Component *, protocol::QueryValueData &qd);
   void get_leaf_p(protocol::Component *, protocol::QueryValueData &qd);
   void get_stem_p(protocol::Component *, protocol::QueryValueData &qd);
   void get_root_p(protocol::Component *, protocol::QueryValueData &qd);
   void get_deadleaf_p(protocol::Component *, protocol::QueryValueData &qd);
   void get_dlt_p_retrans(protocol::Component *, protocol::QueryValueData &qd);
   void get_p_conc_stover(protocol::Component *, protocol::QueryValueData &qd);
   void get_p_conc_leaf(protocol::Component *, protocol::QueryValueData &qd);
   void get_p_uptake_stover(protocol::Component *, protocol::QueryValueData &qd);
   void get_ll_dep(protocol::Component *systemInterface, protocol::QueryValueData &qd);
   void get_ll(protocol::Component *systemInterface, protocol::QueryValueData &qd);

   bool on_day_of(const string &what) ;
   bool inPhase(const string &what) ;
   int  getDayOfYear(void) {return (Environment.day_of_year);};

   //Phosporousy things:
   void zero_p_variables (void);
   void read_p_constants (PlantComponent *systemInterface);
   void doPInit(PlantComponent *systemInterface);
   void PlantP_set_phosphorus_aware (PlantComponent *systemInterface);

   bool phosphorusAware(void)  {return g.phosphorus_aware;};
   bool removeBiomassReport(void)  {return c.remove_biomass_report == "on";};
   void prepare_p(void);
   void summary_p (void);

   void  PlantP_Stress (vector<plantPart *>&);
   void  doPPartition (vector<plantPart*>&);
   void  doPRetranslocate (void);

   float PlantP_Pfact (vector<plantPart *>&);

   const environment_t *getEnvironment(void) {return &Environment;};

private:
   /* system interface: */
   UInt2SetFnMap   IDtoSetFn;    /* setVariable */
   UInt2StringMap  IDtoAction;   /* class actions*/

   //     ================================================================
   //     Plant
   //     ================================================================
   struct IDS {
      // gets
      unsigned int eo;
      unsigned int fr_intc_radn;

      unsigned int parasite_c_demand;
      unsigned int parasite_sw_demand;
      unsigned int add_residue_p;
      unsigned int layered_p_uptake;

      // events.
      unsigned int crop_chopped;
   } id;
   struct crop_chopped {
      string crop_type;
      vector<string> dm_type;
      vector<float> dlt_crop_dm;
      vector<float> dlt_dm_n;
      vector<float> fraction_to_residue;
   };
   struct new_profile {
      vector<float> dlayer;
      vector<float> ll15_dep;
      vector<float> dul_dep;
      vector<float> sat_dep;
      vector<float> sw_dep;
      vector<float> bd;
   };
   environment_t Environment;

   //     ================================================================
   //       plant Globals
   //     ================================================================
   struct {
      bool  hasreadconstants;
      string   module_name;                             // module name
      string   crop_class;                              // crop type
      status_t plant_status;                            // status of crop
      bool  plant_status_out_today;
      string   cultivar;                                // name of cultivar
      string   pre_dormancy_crop_class;
      string   averageStressMessage;                    // Message of average stresses for each phase
      float swdef_expansion;
      float swdef_photo;
      float swdef_pheno;
      float swdef_fixation;
      float nfact_expansion;
      float nfact_photo;
      float nfact_grain_conc;
      float nfact_pheno;
      float remove_biom_pheno;
      float temp_stress_photo;
      float oxdef_photo;
      float fr_intc_radn;                               // fraction of radiation intercepted by canopy
      float eo;                                         // potential evapotranspiration (mm)
      factorObserver cnd_photo;                         // cumulative nitrogen stress type 1
      factorObserver cnd_grain_conc ;                   // cumulative nitrogen stress type 2
      factorObserver cswd_pheno;                        // cumulative water stress type 1
      factorObserver cswd_photo;                        // cumulative water stress type 1
      factorObserver cswd_expansion ;                   // cumulative water stress type 2
      float dlt_canopy_height;                          // change in canopy height (mm)
      float dlt_canopy_width;                           // change in canopy height (mm)
      float canopy_width;                               // canopy height (mm)

//      float lai_canopy_green;                           // green lai of canopy

      float n_fix_pot;                                  // N fixation potential (g/m^2)
      float n_fix_uptake;                               // N fixation actual (g/m^2)
      float n_fixed_tops;                               // cum. fixed N in tops

      float lai_max;                                    // maximum lai - occurs at flowering
      float ext_n_demand;

      float swdef_pheno_flower;
      float swdef_pheno_grainfill;

      // parasite
      float       dlt_dm_parasite_demand;  // parasite dm demand [g/m^2]
      float       dlt_sw_parasite_demand;  // parasite dm demand [g/m^2]
      float       dm_parasite_retranslocate; // plant biomass retranslocated to parasite [g/m^2]
      float       dlt_dm_parasite;         // parasite biomass growth [g/m^2]

      // Phosphorous
      float pfact_photo;
      float pfact_expansion;
      float pfact_pheno;
      float pfact_grain;
      bool  phosphorus_aware;

      float     co2_modifier_rue;
      float     co2_modifier_te;
      float     co2_modifier_n_conc;
      float     vpd;
   } g;   // Globals


   //     ================================================================
   //       plant Parameters
   //     ================================================================
   struct {
      float eo_crop_factor;                             // Crop factor for sw demand applied to Eo
   } p; // Parameters


   //     ================================================================
   //       plant Constants
   //     ================================================================
   struct {
      int   leaf_no_pot_option;
      int   n_retrans_option;
      int   n_stress_option;
      int   n_senescence_option;

      float n_stress_start_stage;

      string crop_type;                                  // crop type
      string default_crop_class;                         // crop class
      vector<string> part_names;                         // names of plant parts
      string n_supply_preference;                        // preference of n supply
      float n_fact_photo;                               // multipler for N deficit effect on photosynthesis
      float n_fact_pheno;                               // multipler for N deficit effect on phenology
      float n_fact_expansion;

      float svp_fract;                                  // fraction of distance between svp at
                                                        // min temp and svp at max temp where
                                                        // average svp during transpiration
                                                        // lies. (0-1)
      float transp_eff_cf[max_table];                   // transpiration efficiency coefficient
                                                        // to convert vpd to
                                                        // transpiration efficiency (kpa)
                                                        // although this is expressed as a
                                                        // pressure it is really in the form
                                                        // kpa*g carbo per m^2 / g water per m^2
                                                        // and this can be converted to
                                                        // kpa*g carbo per m^2 / mm water
                                                        // because 1g water = 1 cm^3 water
      float n_fix_rate[max_table];                      // potential rate of N fixation (g N fixed
                                                        // per g above ground biomass
      float x_ave_temp[max_table];                      // critical temperatures for
                                                        // photosynthesis (oC)
      float y_stress_photo[max_table];                  // Factors for critical temperatures
                                                        // (0-1)
      float x_temp[max_table];                          // temperature table for photosynthesis
                                                        // degree days
      float y_tt[max_table];                            // degree days
      int   num_ave_temp;                               // size_of critical temperature table
      int   num_factors;                                // size_of table


      vector<string> class_action;
      vector<string> class_change;

      float eo_crop_factor_default;                     // Default Crop factor for sw demand applied to Eo

      float      x_co2_te_modifier[max_table], y_co2_te_modifier[max_table];
      int        num_co2_te_modifier;
      float      x_co2_nconc_modifier[max_table], y_co2_nconc_modifier[max_table];
      int        num_co2_nconc_modifier;
      photosynthetic_pathway_t photosynthetic_pathway;
      string     remove_biomass_report;

      float x_p_stage_code [max_table];
      int   num_x_p_stage_code;
      float pfact_photo_slope;
      float pfact_expansion_slope;
      float pfact_pheno_slope;
      float pfact_grain_slope;

   }  c;   // Constants

};  // Plant

#endif //PLANT_H_
