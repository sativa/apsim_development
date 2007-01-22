#ifndef PLANT_H_
#define PLANT_H_

class ApsimVariant;
class PlantComponent;
class PlantPhenology;
class CompositePart;
class plantPart;
class plantStemPart;
class plantLeafPart;
class plantRootPart;
class PlantFruit;
class plantThing;
class eventObserver;
class Plant;
class ReproStruct;
class Arbitrator;

#include "PlantInterface.h"
#include "Environment.h"

typedef bool (Plant::*ptr2setFn) (protocol::QuerySetValueData&);

typedef std::map<unsigned, ptr2setFn>   UInt2SetFnMap;
typedef std::map<unsigned, string>      UInt2StringMap;

////////////////////////
// array size settings
// Maximum number of layers in soil
#define max_layer 100

// Maximum size_of of tables
#define max_table 30

//      crop status names
typedef enum {out, dead, alive} status_t;

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
   friend class plantPartHack;
   stageSubject   stageObservers;            // A collection of state variable observers, reset at each new stage
   stageSubject   otherObservers;            // Another collection of state variable observers

   vector <plantThing *> myThings;
   vector <plantPart *> myParts;
   vector <plantPart *> myStoverParts;
   plantStemPart  *stemPart;
   plantLeafPart  *leafPart;
   plantRootPart  *rootPart;
   ReproStruct    *reproStruct;
   PlantPhenology *phenology;
   PlantFruit     *fruitPart;

   eventObserver *sowingEventObserver;     // Bookkeeper for Sowing events
   eventObserver *emergenceEventObserver;  // Bookkeeper for Emergence events
   eventObserver *FIEventObserver;         // Bookkeeper for Floral Initiation events
   eventObserver *floweringEventObserver;  // Bookkeeper for flowering events
   eventObserver *maturityEventObserver;   // Bookkeeper for maturity events
   Arbitrator    *arbitrator;

   float plantGreen(void) const;
   float plantSenesced(void) const;
   float plantDead(void) const;
   float plantTot(void) const;
   float plantDltDmGreen(void) const;

   float plantNGreen(void);
   float plantNSenesced(void);
   float plantNDead(void);
   float plantNTot(void);
   float plantDltNRetrans(void);
   float plantDltNGreen(void) ;

   float plantPGreen(void) const;
   float plantPSenesced(void) const;
   float plantPDead(void) const;
   float plantPTot(void) const;

   float topsGreen(void) const;
   float topsSenesced(void) const;
   float topsDead(void) const;
   float topsTot(void) const;
   float topsDltDmGreen(void) const;

   float topsNGreen(void) const;
   float topsNSenesced(void) const;
   float topsNDead(void) const;
   float topsNTot(void) const;

   float topsPGreen(void) const;
   float topsPSenesced(void) const;
   float topsPDead(void) const;
   float topsPTot(void) const;

   float stoverGreen(void) const;
   float stoverSenesced(void) const;
   float stoverDead(void) const;
   float stoverTot(void) const;

   float stoverNGreen(void) const;
   float stoverNSenesced(void) const;
   float stoverNDead(void) const;
   float stoverNTot(void) const;

   float stoverPGreen(void) const;
   float stoverPSenesced(void) const;
   float stoverPDead(void) const;
   float stoverPTot(void) const;

   float grainGreen(void) const;
   float grainSenesced(void) const;
   float grainDead(void) const;
   float grainTot(void) const;

   float grainNGreen(void) const;
   float grainNSenesced(void) const;
   float grainNDead(void) const;
   float grainNTot(void) const;

   float grainPGreen(void) const;
   float grainPSenesced(void) const;
   float grainPDead(void) const;
   float grainPTot(void) const;
   float grainPConc(void) const;
   float grainPConcTot(void) const;
   float sumNMax(void) ;
   float sumSoilNDemand(void) ;
   float nDemand(void) ;
   float nCapacity(void) ;
   void doRegistrations(protocol::Component *) ;
   void doIDs(void) ;

public:
   Plant(PlantComponent *P);
   ~Plant();

   void doInit1(protocol::Component *);
   void doInit2(protocol::Component *);
   bool respondToSet(unsigned int &id, protocol::QuerySetValueData& qd) ;

   void doPrepare(unsigned &, unsigned &, protocol::Variant &) ;
   void doProcess(unsigned &, unsigned &, protocol::Variant &) ;
   void doSow(unsigned &, unsigned &, protocol::Variant &v) ;
   void doHarvest(unsigned &, unsigned &, protocol::Variant &v) ;
   void doEndCrop(unsigned &, unsigned &, protocol::Variant &v) ;
   void doKillCrop(unsigned &, unsigned &, protocol::Variant &v) ;
   void doKillStem(unsigned &, unsigned &, protocol::Variant &v) ;
   void doRemoveCropBiomass(unsigned &, unsigned &, protocol::Variant &v) ;
   void doDetachCropBiomass(unsigned &, unsigned &, protocol::Variant &v) ;
   void doEndRun(unsigned &, unsigned &, protocol::Variant &v) ;
   void doAutoClassChange(unsigned &, unsigned &, protocol::Variant &v) ;
   void doTick(unsigned &, unsigned &, protocol::Variant &v) ;
   void doNewMet(unsigned &, unsigned &, protocol::Variant &v) ;
   void doNewProfile(unsigned &, unsigned &, protocol::Variant &v) ;

   void registerClassActions(void);
   void sendStageMessage(const char *what);
   void doPlantEvent(const string &);

   void writeString (const char *line);
   void warningError (const char *msg);

   const std::string & getCropType(void) ;
   protocol::Component *getComponent(void) ;

   void plant_co2_modifier_rue(void);
   void plant_co2_modifier_te(void);
   void plant_co2_modifier_n_conc(void);
   void plant_vpd (float c_svp_fract, float g_maxt, float g_min);

   void plant_bio_actual (int option /* (INPUT) option number*/);
   void plant_bio_retrans (void);
   void plant_water_stress (void);
   void plant_temp_stress (void);
   void plant_oxdef_stress ();
   void plant_bio_water (void);
   void plant_retrans_init (int option);
   void plant_detachment (void);
   void plant_plant_death (int option /* (INPUT) option number*/);
   float plant_death_seedling    (
                                  int    c_num_weighted_temp
                                  ,float  *c_x_weighted_temp
                                  ,float  *c_y_plant_death
                                  ,int    g_day_of_year
                                  ,float  *g_soil_temp
                                  ,int    g_year
                                  ,float  g_plants) ;
   float plant_death_drought(float  c_leaf_no_crit
                             ,float  c_swdf_photo_limit
                             ,float  c_swdf_photo_rate
                             ,float  g_cswd_photo
                             ,float  g_plants
                             ,float  g_swdef_photo) ;
   void plant_death_external_action(protocol::Variant &v
                                    ,float g_plants
                                    ,float *dlt_plants
                                   ) ;
   void plant_death_crop_killed(float g_plants
                                , status_t g_plant_status
                                , float *dlt_plants
                               ) ;
   void plant_death_actual(float g_dlt_plants_death_drought
                           ,float *g_dlt_plants_death_external
                           ,float g_dlt_plants_death_seedling
                           ,float g_dlt_plants_failure_emergence
                           ,float g_dlt_plants_failure_germ
                           ,float g_dlt_plants_failure_leaf_sen
                           ,float g_dlt_plants_failure_phen_delay
                           ,float *dlt_plants
                          ) ;
   void plant_plants_temp (int    c_num_weighted_temp
                           ,float  *c_x_weighted_temp
                           ,float  *c_y_plant_death
                           ,int    g_day_of_year
                           ,float  *g_soil_temp
                           ,int    g_year
                           ,float  *killfr
                          ) ;
   void plant_kill_crop (status_t *g_plant_status) ;
   void plant_leaf_area_potential (int option /* (INPUT) option number */);
   void plant_leaf_area_stressed (int option  /* (INPUT) option number*/);
   void plant_leaf_area_init (int option);
   void plant_leaf_area_actual (int option /* (INPUT) option number*/);
   void plant_pod_area (int option         /* (INPUT) option number*/);
   void plant_leaf_no_actual (int option   /* (INPUT) option number*/);
   void plant_nit_init (int option         /* (INPUT) option number*/);
   void plant_nit_supply (int option       /* (INPUT) option number*/);
   void plant_nit_retrans (int option      /* (INPUT) option number*/);
   void plant_nit_demand (int option       /* (INPUT) option number*/);
   void plant_nit_uptake (int option       /* (INPUT) option number*/);
   void plant_nit_partition ();
   void plant_nit_stress (int option       /* (INPUT) option number*/);
   void plant_soil_nit_demand ();

   void plant_nit_demand_est (int option);
   void plant_height (int   option/*(INPUT) option number*/);
   void plant_width (int   option /*(INPUT) option number*/);
   void plant_sen_bio (int option);
   void plant_sen_nit (int   option/*(INPUT) option number*/);
   void plant_leaf_death (int   option/*(INPUT) option number*/);
   void plant_leaf_area_sen (int   option/*(INPUT) option number*/);
   void plant_cleanup ();
   void plant_update(float  g_row_spacing
                     ,float  g_skip_row_fac
                     ,float  g_skip_plant_fac
                     ,float  *g_canopy_width
                     ,float  *g_cover_dead
                     ,float  *g_cover_green
                     ,float  *g_cover_sen
                     ,float  g_dlt_plants
                     ,float *g_plants) ;
   void plant_check_bounds(float  g_cover_dead
                           ,float  g_cover_green
                           ,float  g_cover_sen
                           ,float *g_dlayer
                           ,float  g_plants
                           ,float  g_root_depth
                          ) ;

   void plant_totals(float *g_dlayer
                     ,float *g_dlt_sw_dep
                     ,float *g_lai_max
                     ,float  *g_n_conc_act_stover_tot
                     ,float  *g_n_conc_crit_stover_tot
                     ,float  *g_n_demand_tot
                     ,float  *g_n_uptake_stover_tot
                     ,float  *g_n_uptake_tot
                     ,float  *g_n_fix_uptake
                     ,float  *g_n_fixed_tops
                     ,float  *g_root_depth
                     ,float  *g_transpiration_tot
                    )  ;
   void plant_event();

   void plant_dm_init (void);

   void plant_root_depth (int option /* (INPUT) option number*/);
   void plant_water_supply (int option /* (INPUT) option number*/);
   void plant_water_demand (int option /* (INPUT) option number*/);
   void plant_water_distribute (int option /*(INPUT) option number*/);
   void plant_water_uptake (int option /*(INPUT) option number*/);
   void plant_light_supply_partition (int option /*(INPUT) option number*/);
   void plant_bio_rue (int option    /*(INPUT) option number*/);
   void plant_transpiration_eff (int option /*(INPUT) option number*/);
   void plant_root_length_growth (int option /*(INPUT) option number*/);
   void plant_water_supply_partition(float sw_demand
                                     , float swDemandVeg
                                     , float swSupply
                                     , float *swSupplyVeg
                                     , float *swSupplyFruit);

   void plant_dm_pot_rue_veg (externalFunction *c_rue
                              , double  radn_int
                              , double  stress_factor
                              , float  *dlt_dm_pot);

   void plant_rue_co2_modifier(float co2,                 //!CO2 level (ppm)
                               float maxt,                //!daily max temp (C)
                               float mint,                //!daily min temp (C)
                               float *modifier);          //!modifier (-)

   void plant_n_conc_limits (float  g_co2_modifier_n_conc) ;

   void legnew_n_partition
      (float  *g_dlayer
       ,float  *g_dlt_no3gsm
       ,float  *g_dlt_nh4gsm
       ,float  g_n_fix_pot
       ,float  g_root_depth
       ,float  *n_fix_uptake
       ,vector<plantPart *> &);        // (INPUT) vector of plant parts

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

   void legnew_n_retranslocate(float g_grain_n_demand);

   void plant_N_senescence (void);
   void plant_process ( void );
   void plant_dead (void);
   void plant_harvest (protocol::Variant &v/*(INPUT) message variant*/);
   void plant_kill_stem (protocol::Variant &v/*(INPUT) incoming message variant*/);
   void plant_remove_crop_biomass (protocol::Variant &v/*(INPUT) incoming message variant*/);
   void plant_detach_crop_biomass (protocol::Variant &v/*(INPUT) incoming message variant*/);
   void plant_dormancy (protocol::Variant &v/*(INPUT) incoming message variant*/);
   void plant_harvest_update (protocol::Variant &v/*(INPUT)message arguments*/);
   void plant_kill_stem_update (protocol::Variant &v/*(INPUT) message arguments*/);
   void plant_remove_biomass_update (protocol::RemoveCropDmType dmRemoved);
   void plant_zero_all_globals (void);
   void plant_zero_variables (void);
   void plant_zero_daily_variables ();
   void plant_init (void);
   void plant_start_crop (protocol::Variant &v/*(INPUT) message arguments*/);
   void plant_read_cultivar_params ();
   void plant_read_root_params ();
   void plant_end_crop ();
   void plant_kill_crop_action (protocol::Variant &mVar);
   void plant_store_value (
                           int    g_day_of_year
                           ,int    g_year
                           ,float  array[]
                           ,float  value
                          ) ;
   void plant_get_other_variables ();
   void plant_set_other_variables ();
   void plant_update_other_variables (void);
   void plant_read_constants ( void );
   void plant_prepare (void);
   void plant_read_species_const ();
   void plant_harvest_report ();
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

   void plant_get_site_characteristics ();
   bool set_plant_crop_class(protocol::QuerySetValueData&v);
   bool set_plant_grain_oil_conc(protocol::QuerySetValueData&v);

   void get_plant_status(protocol::Component *, protocol::QueryValueData &) const;
   float getStageCode(void) const ;
   float getStageNumber(void) const ;

   void get_crop_type(protocol::Component *, protocol::QueryValueData &);
   void get_crop_class(protocol::Component *, protocol::QueryValueData &);
   void get_leaf_no(protocol::Component *, protocol::QueryValueData &);
   float getLeafNo(void) const;
   void get_dlt_leaf_no(protocol::Component *, protocol::QueryValueData &);
   void get_dlt_node_no(protocol::Component *, protocol::QueryValueData &);
   void get_leaf_no_dead(protocol::Component *, protocol::QueryValueData &);
   void get_leaf_area(protocol::Component *, protocol::QueryValueData &);
   void get_height(protocol::Component *, protocol::QueryValueData &);
   void get_width(protocol::Component *, protocol::QueryValueData &);
   void get_root_depth(protocol::Component *, protocol::QueryValueData &);
   void get_plants(protocol::Component *, protocol::QueryValueData &);
   float getPlants(void) const;
   float getCo2(void) const;
   //  float getRadnInterceptedPod(void) const;
   float getDltDMPotRueVeg(void) const;
   float getDmGreenVeg(void) const;
   //  float getDltDmVeg(void) const;
   float getWaterSupplyPod(void) const;
   float getWaterSupplyLeaf(void) const;
   float getDmTops(void) const;
   float getDltDmGreen(void) const;
   float getDltDm(void) const;
   float getDmVeg(void) const;
   float getDmGreenStem(void) const;
   float getDmGreenTot(void) const;
// FIXME - remove next line when corrections for P demand activated
   float getRelativeGrowthRate(void);
   float getTotalPotentialGrowthRate(void);
   float getDyingFractionPlants(void);
   float getCo2ModifierRue(void) const;
   float getCo2ModifierTe(void) const;
   float getCo2ModifierNConc(void) const;
   float getVpd(void) const;

   float getTempStressPhoto(void) const;
   float getNfactPhoto(void) const;
   float getNfactGrainConc(void) const;
   float getOxdefPhoto(void) const;
   float getPfactPhoto(void) const;
   float getSwdefPhoto(void) const;

   void get_cover_green(protocol::Component *, protocol::QueryValueData &);
   void get_cover_tot(protocol::Component *, protocol::QueryValueData &);
   void get_lai_canopy_green(protocol::Component *, protocol::QueryValueData &);
   void get_pai(protocol::Component *, protocol::QueryValueData &);
   void get_root_wt(protocol::Component *, protocol::QueryValueData &);
   void get_leaf_wt(protocol::Component *, protocol::QueryValueData &);
   void get_stem_wt(protocol::Component *, protocol::QueryValueData &);
   void get_dm_green(protocol::Component *, protocol::QueryValueData &);
   void get_dm_senesced(protocol::Component *, protocol::QueryValueData &);
   void get_dm_dead(protocol::Component *, protocol::QueryValueData &);
   void get_biomass(protocol::Component *, protocol::QueryValueData &);
   void get_green_biomass(protocol::Component *, protocol::QueryValueData &);
   void get_biomass_wt(protocol::Component *, protocol::QueryValueData &);
   void get_green_biomass_wt(protocol::Component *, protocol::QueryValueData &);
   void get_stover_biomass_wt(protocol::Component *, protocol::QueryValueData &);
   void get_dm_plant_min(protocol::Component *, protocol::QueryValueData &);
   void get_dlt_dm(protocol::Component *, protocol::QueryValueData &);
   void get_dlt_dm_pot_rue(protocol::Component *, protocol::QueryValueData &);
   void get_dlt_dm_pot_te(protocol::Component *, protocol::QueryValueData &);
   void get_dlt_dm_green(protocol::Component *, protocol::QueryValueData &);
   void get_dlt_dm_green_dead(protocol::Component *, protocol::QueryValueData &);
   void get_dlt_dm_green_retrans(protocol::Component *, protocol::QueryValueData &);
   void get_dlt_dm_detached(protocol::Component *, protocol::QueryValueData &);
   void get_dlt_dm_senesced(protocol::Component *, protocol::QueryValueData &);
   void get_dlt_dm_senesced_dead(protocol::Component *, protocol::QueryValueData &);
   void get_dlt_dm_dead_detached(protocol::Component *, protocol::QueryValueData &);
   void get_biomass_n(protocol::Component *, protocol::QueryValueData &);
   void get_n_uptake(protocol::Component *, protocol::QueryValueData &);
   void get_green_biomass_n(protocol::Component *, protocol::QueryValueData &);
   void get_leaf_n(protocol::Component *, protocol::QueryValueData &);
   void get_stem_n(protocol::Component *, protocol::QueryValueData &);
   void get_root_n(protocol::Component *, protocol::QueryValueData &);
   void get_deadleaf_n(protocol::Component *, protocol::QueryValueData &);
   void get_n_senesced(protocol::Component *, protocol::QueryValueData &);
   void get_n_dead(protocol::Component *, protocol::QueryValueData &);
   void get_dlt_n_green(protocol::Component *, protocol::QueryValueData &);
   void get_dlt_n_retrans(protocol::Component *, protocol::QueryValueData &);
   void get_dlt_n_senesced_trans(protocol::Component *, protocol::QueryValueData &);
   void get_dlt_n_senesced_retrans(protocol::Component *, protocol::QueryValueData &);
   void get_dlt_n_senesced_dead(protocol::Component *, protocol::QueryValueData &);
   void get_dlt_n_detached(protocol::Component *, protocol::QueryValueData &);
   void get_dlt_n_dead_detached(protocol::Component *, protocol::QueryValueData &);
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
   void get_cep(protocol::Component *, protocol::QueryValueData &);
   void get_sw_supply(protocol::Component *, protocol::QueryValueData &);
   void get_esw_layr(protocol::Component *, protocol::QueryValueData &);
   void get_n_green(protocol::Component *, protocol::QueryValueData &);
   void get_dlt_n_dead(protocol::Component *, protocol::QueryValueData &);
   void get_dlt_n_senesced(protocol::Component *, protocol::QueryValueData &);
   void get_n_conc_stover(protocol::Component *, protocol::QueryValueData &);
   void get_n_conc_root(protocol::Component *, protocol::QueryValueData &);
   void get_n_conc_crit(protocol::Component *, protocol::QueryValueData &);
   void get_n_conc_min(protocol::Component *, protocol::QueryValueData &);

   void get_n_uptake_stover(protocol::Component *, protocol::QueryValueData &);
   void get_no3_tot(protocol::Component *, protocol::QueryValueData &);
   void get_n_demand(protocol::Component *, protocol::QueryValueData &);
   void get_n_demanded(protocol::Component *, protocol::QueryValueData &);
   void get_n_supply_soil(protocol::Component *, protocol::QueryValueData &);
   void get_nfact_pheno(protocol::Component *, protocol::QueryValueData &);
   void get_dlt_n_fixed_pot(protocol::Component *, protocol::QueryValueData &);
   void get_dlt_n_fixed(protocol::Component *, protocol::QueryValueData &);
   void get_n_fixed_tops(protocol::Component *, protocol::QueryValueData &);
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
   void get_no3gsm_uptake_pot(protocol::Component *, protocol::QueryValueData &);
   void get_nh4gsm_uptake_pot(protocol::Component *, protocol::QueryValueData &);
   void get_no3_swfac(protocol::Component *, protocol::QueryValueData &);

   void get_parasite_c_gain(protocol::Component *, protocol::QueryValueData &);
   void get_leaf_area_tot(protocol::Component *, protocol::QueryValueData &);
   void get_dm_parasite_retranslocate(protocol::Component *, protocol::QueryValueData &);
   void get_sw_supply_layr(protocol::Component *, protocol::QueryValueData &);
   void get_no3_uptake(protocol::Component *, protocol::QueryValueData &);
   void get_nh4_uptake(protocol::Component *, protocol::QueryValueData &);
   void get_zadok_stage(protocol::Component *, protocol::QueryValueData &);


   void get_p_green(protocol::Component *, protocol::QueryValueData &qd);
   void get_p_sen(protocol::Component *, protocol::QueryValueData &qd);
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
   void get_p_senesced(protocol::Component *, protocol::QueryValueData &qd);
   void get_p_dead(protocol::Component *, protocol::QueryValueData &qd);
   void get_dlt_p_green(protocol::Component *, protocol::QueryValueData &qd);
   void get_dlt_p_retrans(protocol::Component *, protocol::QueryValueData &qd);
   void get_dlt_p_detached(protocol::Component *, protocol::QueryValueData &qd);
   void get_dlt_p_dead(protocol::Component *, protocol::QueryValueData &qd);
   void get_dlt_p_sen(protocol::Component *, protocol::QueryValueData &qd);
   void get_p_conc_stover(protocol::Component *, protocol::QueryValueData &qd);
   void get_p_conc_leaf(protocol::Component *, protocol::QueryValueData &qd);
   void get_p_uptake_stover(protocol::Component *, protocol::QueryValueData &qd);
   void get_ll_dep(protocol::Component *systemInterface, protocol::QueryValueData &qd);
   void get_ll(protocol::Component *systemInterface, protocol::QueryValueData &qd);

   bool on_day_of(const string &what) ;
   bool inPhase(const string &what) ;
   int  getDayOfYear(void) {return (Environment.day_of_year);};

   //Phosporousy things:
   void zero_p_variables ();
   void zero_daily_p_variables ();
   void read_p_constants (PlantComponent *systemInterface);
   void doPInit(PlantComponent *systemInterface);
   void PlantP_set_phosphorus_aware (PlantComponent *systemInterface);

   bool phosphorusAware(void) const {return g.phosphorus_aware;};
   bool removeBiomassReport(void) const {return c.remove_biomass_report == "on";};
   void prepare_p(void);
   void plant_p_retrans (void);
   void summary_p (void);

   void  PlantP_demand (vector<plantPart *>&);
   void  PlantP_Stress (vector<plantPart *>&);
   void  PlantP_init_pools (vector<plantPart*>&);
   void  PlantP_partition (vector<plantPart*>&);
   void  PlantP_senescence (vector<plantPart*>&);
   void  PlantP_retrans (vector<plantPart*>&);

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

      unsigned int no3;
      unsigned int nh4;

      unsigned int latitude;
      unsigned int parasite_c_demand;
      unsigned int parasite_sw_demand;
      unsigned int maxt_soil_surface;
      unsigned int co2;

      unsigned int add_residue_p;
      unsigned int layered_p_uptake;

      // sets
      unsigned int dlt_no3;
      unsigned int dlt_nh4;


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
      float sowing_depth;
      float row_spacing;                                // row spacing (m) [optional]
      float skip_row;                                   // skip row (0, 1, 2)
      float skip_plant;                                 // skip plant (0, 1, 2)
      float skip_row_fac;                               // skip row factor
      float skip_plant_fac;                             // skip plant factor
      float fr_intc_radn;                               // fraction of radiation intercepted by canopy
      float soil_temp[366+1];                           // soil surface temperature (oC)
      float eo;                                         // potential evapotranspiration (mm)
      factorObserver cnd_photo;                         // cumulative nitrogen stress type 1
      factorObserver cnd_grain_conc ;                   // cumulative nitrogen stress type 2
      factorObserver cswd_pheno;                        // cumulative water stress type 1
      factorObserver cswd_photo;                        // cumulative water stress type 1
      factorObserver cswd_expansion ;                   // cumulative water stress type 2
      float dlt_canopy_height;                          // change in canopy height (mm)
      float dlt_canopy_width;                           // change in canopy height (mm)
      float canopy_width;                               // canopy height (mm)
      float plants;                                     // Plant density (plants/m^2)
      float dlt_plants;                                 // change in Plant density (plants/m^2)
      float cover_green;                                // fraction of radiation reaching the
                                                        // canopy that is intercepted by the
                                                        // green leaves of the canopy (0-1)
      float cover_sen;                                  // fraction of radiation reaching the
                                                        // canopy that is intercepted by the
                                                        // senesced leaves of the canopy (0-1)
      float cover_dead;                                 // fraction of radiation reaching the
                                                        // canopy that is intercepted by the
                                                        // dead leaves of the dead canopy (0-1)
      float dlt_plants_death_seedling;
      float dlt_plants_death_drought;
      float dlt_plants_failure_phen_delay;
      float dlt_plants_failure_leaf_sen;
      float dlt_plants_failure_emergence;
      float dlt_plants_failure_germ;
      float dlt_plants_death_external;
      float dlt_dm;                                     // the daily biomass production (g/m^2)
      float dlt_dm_pot_rue;                             // potential dry matter production with
                                                        // optimum water and nitrogen and
                                                        // temperature stress conditions (g/m^2)
      float dlt_dm_pot_te;                              // the potential daily biomass production from te (g/m^2)
      float radn_int;                                   // radn intercepted by leaves (mj/m^2)
      float radnIntGreenFruit;                          // radn intercepted by fruit (mj/m^2)
      float transp_eff;                                 // transpiration efficiency (g dm/m^2/mm water)
      float transpEffFruit;                             // transpiration efficiency of fruit (g dm/m^2/mm water)

      float lai_canopy_green;                           // green lai of canopy

      float no3gsm_diffn_pot[max_layer];                // potential NO3 (supply) from soil (g/m^2), by diffusion
      float no3gsm_mflow_avail[max_layer];              // potential NO3 (supply) from soil (g/m^2) by mass flow

      float nh4gsm_diffn_pot[max_layer];                // potential NH4 (supply) from soil (g/m^2), by diffusion
      float nh4gsm_mflow_avail[max_layer];              // potential NH4 (supply) from soil (g/m^2) by mass flow
      float nh4gsm_uptake_pot[max_layer];
      float n_fix_pot;                                  // N fixation potential (g/m^2)
      float no3gsm_uptake_pot[max_layer];
      float n_fix_uptake;                               // N fixation actual (g/m^2)
      float n_fixed_tops;                               // cum. fixed N in tops

      float swSupplyFruit;                              // crop water water supply to fruit (mm)
      float swSupplyVeg;                                // crop water water supply to vegetative parts (mm)
      float sw_demand;                                  // total crop demand for water (mm)
      float swDemandTEFruit;                            // crop fruit demand for water calculated from transpiration efficiency (mm)
      float transpiration_tot;                          // cumulative transpiration (mm)
      float n_uptake_tot;                               // cumulative total N uptake (g/m^2)
      float n_demand_tot;                               // sum of N demand since last output (g/m^2)
      float n_conc_act_stover_tot;                      // sum of tops actual N concentration (g N/g biomass)
      float n_conc_crit_stover_tot;                     // sum of tops critical N concentration (g N/g biomass)
      float n_uptake_stover_tot;                        // sum of tops N uptake (g N/m^2)
      float lai_max;                                    // maximum lai - occurs at flowering
      float ext_n_demand;
      float grain_energy;                               // multiplier of grain weight to account
                                                        // for seed energy content

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

      float     co2;
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
      int   n_uptake_option;
      int   leaf_no_pot_option;
      int   n_retrans_option;
      int   n_stress_option;
      int   n_senescence_option;
      int   dm_senescence_option;

      float n_stress_start_stage;

      float no3_uptake_max;
      float no3_conc_half_max;
      float kno3;
      float no3ppm_min;
      float knh4;
      float nh4ppm_min;

      string crop_type;                                  // crop type
      string default_crop_class;                         // crop class
      vector<string> part_names;                         // names of plant parts
      string n_supply_preference;                        // preference of n supply
      float n_fact_photo;                               // multipler for N deficit effect on photosynthesis
      float n_fact_pheno;                               // multipler for N deficit effect on phenology
      float n_fact_expansion;
      interpolationFunction rue;                        // radiation use efficiency as f(stage number) (g dm/mj)

      float leaf_no_crit;                               // critical number of leaves below
                                                        // which portion of the crop may
                                                        // die due to water stress
      float tt_emerg_limit;                             // maximum degree days allowed for
                                                        // emergence to take place (deg day)
      float days_germ_limit;                            // maximum days allowed after sowing
                                                        // for germination to take place (days)
      float swdf_pheno_limit;                           // critical cumulative phenology
                                                        // water stress above which the crop
                                                        // fails (unitless)
      float swdf_photo_limit;                           // critical cumulative photosynthesis
                                                        // water stress above which the crop
                                                        // partly fails (unitless)
      float swdf_photo_rate;                            // rate of plant reduction with
                                                        // photosynthesis water stress
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
      float no3_diffn_const;                            // time constant for uptake by
                                                        // diffusion (days). H van Keulen &
                                                        // NG Seligman. Purdoe 1987. This is the
                                                        // time it would take to take up by
                                                        // diffusion the current amount of N if
                                                        // it wasn't depleted between time steps
      float n_fix_rate[max_table];                      // potential rate of N fixation (g N fixed
                                                        // per g above ground biomass
      float grn_water_cont;                             // water content of grain g/g
      float x_ave_temp[max_table];                      // critical temperatures for
                                                        // photosynthesis (oC)
      float y_stress_photo[max_table];                  // Factors for critical temperatures
                                                        // (0-1)
      float x_temp[max_table];                          // temperature table for photosynthesis
                                                        // degree days
      float y_tt[max_table];                            // degree days
      float x_weighted_temp[max_table];                 // temperature table for poor
                                                        // establishment
      float y_plant_death[max_table];                   // index of plant death
                                                        // rates for critical temperatures
                                                        // (0-1)
      int   num_ave_temp;                               // size_of critical temperature table
      int   num_factors;                                // size_of table
      int   num_weighted_temp;                          // size of table


      float no3_ub;                                     // upper limit of soil NO3 (kg/ha)
      float no3_lb;                                     // lower limit of soil NO3 (kg/ha)
      float nh4_ub;                                     // upper limit of soil NH4 (kg/ha)
      float nh4_lb;                                     // lower limit of soil NH4 (kg/ha)
      float latitude_ub;                                // upper limit of latitude for model (oL)
      float latitude_lb;                                // lower limit of latitude for model(oL)
      float row_spacing_default;
      float skip_row_default;                           //Default skip row ()
      float skip_plant_default;                         //Default skip plant ()
      vector<string> class_action;
      vector<string> class_change;

      float eo_crop_factor_default;                     // Default Crop factor for sw demand applied to Eo
      float total_n_uptake_max;

      int        root_growth_option;


      float      co2_default;
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
