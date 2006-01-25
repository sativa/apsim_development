#ifndef PLANT_H_
#define PLANT_H_

class PlantComponent;
class PlantPhenology;
class ApsimVariant;
class plantPart;
class plantStemPart;
class plantLeafPart;
class PlantFruit;
class plantThing;
class eventObserver;
class Plant;
class ReproStruct;

typedef bool (Plant::*ptr2setFn) (protocol::QuerySetValueData&);

typedef std::map<unsigned, ptr2setFn>   UInt2SetFnMap;
typedef std::map<unsigned, string>      UInt2StringMap;

////////////////////////
// array size settings
// maximum number of plant nodes
#define max_node 1000

// Maximum number of layers in soil
#define max_layer 100

// Maximum size_of of tables
#define max_table 30

//      crop status names
typedef enum {out, dead, alive} status_t;

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

//   Short description:
//      indices of plant part names
const int  root = 0 ;
//const int  leaf = 1 ;
//const int  stem = 2 ;
const int  pod  = 1 ;
const int  meal = 2 ; // excludes oil component
const int  oil  = 3 ; // seed oil
// number of plant parts
const int  max_part = 4 ; // NB. implies for (i=0; i < max_part; max_part++) usage
//const int  max_part = 1 ; // NB. implies for (i=0; i < max_part; max_part++) usage
const int  max_part1 = 1 ; // NB. implies for (i=0; i < max_part; max_part++) usage

//typedef enum {pw_C3, pw_C4, pw_UNDEF} photosynthetic_pathway_t;


//   This class performs crop crop growth
//     simulates root, leaf, head, stem and grain development. Water and
//     nitrogen uptake, photosynhesis, and leaf and root senescense.
class Plant : public plantInterface {
 private:
  PlantComponent *parent;                                // for interface calls to system
  friend class plantPartHack;
  stageSubject   stageObservers;            // A collection of state variable observers, reset at each new stage
  stageSubject   otherObservers;            // Another collection of state variable observers

  vector <plantThing *> myThings;
  vector <plantPart *> myParts;
  plantStemPart  *stemPart;
  plantLeafPart  *leafPart;
  ReproStruct    *reproStruct;
  PlantPhenology *phenology;
  PlantFruit     *fruitPart;

  eventObserver *floweringEventObserver;     // Bookkeeper for flowering events
  eventObserver *maturityEventObserver;      // Bookkeeper for maturity events

  float plantGreen(void) const;
  float plantSenesced(void) const;
  float plantDead(void) const;
  float plantTot(void) const;
  float plantDltDmGreen(void) const;

  float plantNGreen(void) const;
  float plantNSenesced(void) const;
  float plantNDead(void) const;
  float plantNTot(void) const;
  float plantDltNRetrans(void) const;
  float plantDltNGreen(void) const;

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
  float sumNDemand(void) ;

 public:
  Plant(PlantComponent *P);
  ~Plant();
  void initialise(void) ;
  void doIDs(void) ;
  void doInit1(protocol::Component *);
  void doRegistrations(protocol::Component *) ;
  bool setVariable(unsigned id, protocol::QuerySetValueData& qd) ;
  void doPrepare(unsigned &, unsigned &, protocol::Variant &) ;
  void doProcess(unsigned &, unsigned &, protocol::Variant &) ;
  void doSow(unsigned &, unsigned &, protocol::Variant &v) ;
  void doHarvest(unsigned &, unsigned &, protocol::Variant &v) ;
  void doEndCrop(unsigned &, unsigned &, protocol::Variant &v) ;
  void doKillCrop(unsigned &, unsigned &, protocol::Variant &v) ;
  void doKillStem(unsigned &, unsigned &, protocol::Variant &v) ;
  void doRemoveCropBiomass(unsigned &, unsigned &, protocol::Variant &v) ;
  void doEndRun(unsigned &, unsigned &, protocol::Variant &v) ;
  void doAutoClassChange(unsigned &, unsigned &, protocol::Variant &v) ;
  void doTick(unsigned &, unsigned &, protocol::Variant &v) ;
  void doNewMet(unsigned &, unsigned &, protocol::Variant &v) ;
  void doNewProfile(unsigned &, unsigned &, protocol::Variant &v) ;
  void registerClassActions(void);
  void onApsimGetQuery(protocol::ApsimGetQueryData&);
  void sendStageMessage(const char *what);
  void doPlantEvent(const string &);

  void plant_bio_actual (int option /* (INPUT) option number*/);
  void plant_bio_partition (int option /* (INPUT) option number */);
  void plant_bio_retrans (int option /* (INPUT) option number */);
  void plant_water_stress (int option /* (INPUT) option number */);
  void plant_temp_stress (int option/* (INPUT) option number*/);
  void plant_oxdef_stress (int option /* (INPUT) option number */);
  void plant_bio_water (int option /* (INPUT) option number */);
  void plant_bio_init (int option);
  void plant_bio_grain_demand_stress (int option /* (INPUT) option number */);
  void plant_retrans_init (int option);
  void plant_detachment (int option /* (INPUT) option number */);
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
         			   ,float  *g_leaf_no
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
  void plant_kill_crop (float  *g_dm_dead
      			,float  *g_dm_green
      			,float  *g_dm_senesced
      			,status_t *g_plant_status
      			) ;
  void plant_leaf_area_potential (int option /* (INPUT) option number */);
  void plant_leaf_area_stressed (int option /* (INPUT) option number*/);
  void plant_leaf_area_init (int option);
  void plant_leaf_no_init (int option);
  void plant_leaf_area_actual (int option /* (INPUT) option number*/);
  void plant_pod_area (int option /* (INPUT) option number*/);
  void plant_leaf_no_actual (int option /* (INPUT) option number*/);
  void plant_leaf_no_pot (int option /* (INPUT) option number*/);
  void plant_nit_init (int option /* (INPUT) option number*/);
  void plant_nit_supply (int option /* (INPUT) option number*/);
  void plant_nit_retrans (int option/* (INPUT) option number*/);
  void plant_nit_demand (int option /* (INPUT) option number*/);
  void plant_nit_uptake (int option/* (INPUT) option number*/);
  void plant_nit_partition (int option /* (INPUT) option number*/);
  void plant_nit_stress (int option /* (INPUT) option number*/);
  void plant_nit_grain_demand (int Option);
  void plant_soil_nit_demand (int Option);

  void plant_nit_demand_est (int option);
  void plant_height (int   option/*(INPUT) option number*/);
  void plant_width (int   option/*(INPUT) option number*/);
  void plant_sen_bio (int option);
  void plant_sen_nit (int   option/*(INPUT) option number*/);
  void plant_leaf_death (int   option/*(INPUT) option number*/);
  void plant_leaf_area_sen (int   option/*(INPUT) option number*/);
  void plant_cleanup ();
  void plant_check_leaf_record ();
  void plant_update
                   (float  c_n_conc_crit_root
                   ,float  c_n_conc_max_root
                   ,float  c_n_conc_min_root
                   ,float *c_x_stage_code
                   ,float *c_x_co2_nconc_modifier
                   ,float *c_y_co2_nconc_modifier
                   ,int   c_num_co2_nconc_modifier
                   ,float  g_co2
                   ,float  g_row_spacing
                   ,float  g_skip_row_fac
                   ,float  g_skip_plant_fac
                   ,float *c_x_row_spacing
                   ,float *c_y_extinct_coef
                   ,float *c_y_extinct_coef_dead
                   ,int    c_num_row_spacing
                   ,float  *g_canopy_width
                   ,float  *g_cover_dead
                   ,float  *g_cover_green
                   ,float  *g_cover_sen
                   ,float  g_dlt_dm
                   ,float *g_dlt_dm_dead_detached
                   ,float *g_dlt_dm_detached
                   ,float *g_dlt_dm_green
                   ,float *g_dlt_dm_green_retrans
                   ,float *g_dlt_dm_senesced
                   ,float *g_dlt_dm_green_dead
                   ,float *g_dlt_dm_senesced_dead
                   ,float  g_dlt_leaf_no
                   ,float  g_dlt_node_no
                   ,float  g_dlt_leaf_no_dead
                   ,float *g_dlt_n_dead_detached
                   ,float *g_dlt_n_detached
                   ,float *g_dlt_n_green
                   ,float *g_dlt_n_retrans
                   ,float *g_dlt_n_senesced
                   ,float *g_dlt_n_senesced_trans
                   ,float *g_dlt_n_senesced_retrans
                   ,float *g_dlt_n_green_dead
                   ,float *g_dlt_n_senesced_dead
                   ,float  g_dlt_plants
                   ,float  g_dlt_root_depth
                   ,float  g_dlt_slai_detached
                   ,float *g_dm_dead
                   ,float *g_dm_green
                   ,float *g_dm_senesced
                   ,float *g_lai_canopy_green
                   ,float *g_leaf_area
                   ,float *g_leaf_no
                   ,float *g_node_no
                   ,float *g_leaf_no_dead
                   ,float *g_n_conc_crit
                   ,float *g_n_conc_max
                   ,float *g_n_conc_min
                   ,float *g_n_dead
                   ,float *g_n_green
                   ,float *g_n_senesced
                   ,float *g_plants
                   ,float *g_root_depth
                   ,float g_swdef_pheno
                   ,float *g_dlt_root_length_dead
                   ,float *g_root_length_dead
                   ,float *g_root_length
                   ,float *g_dlt_root_length
                   ,float *g_dlt_root_length_senesced) ;
  void plant_check_bounds
                         (float  g_cover_dead
                         ,float  g_cover_green
                         ,float  g_cover_sen
                         ,float *g_dlayer
                         ,float *g_dm_dead
                         ,float *g_dm_green
                         ,float *g_dm_senesced
                         ,float *g_leaf_area
                         ,float *g_leaf_no
                         ,float *g_leaf_no_dead
                         ,float *g_n_conc_crit
                         ,float *g_n_conc_max
                         ,float *g_n_conc_min
                         ,float *g_n_dead
                         ,float *g_n_green
                         ,float *g_n_senesced
                         ,float  g_plants
                         ,float  g_root_depth
                         ) ;
  void plant_totals(int   g_day_of_year
                   ,float *g_dlayer
                   ,float *g_dlt_n_retrans
                   ,float *g_dlt_sw_dep
                   ,float *g_dm_green
                   ,float *g_lai_max
                   ,float  *g_n_conc_act_stover_tot
                   ,float  *g_n_conc_crit
                   ,float  *g_n_conc_crit_stover_tot
                   ,float  *g_n_dead
                   ,float  *g_n_demand
                   ,float  *g_n_demand_tot
                   ,float  *g_n_green
                   ,float  *g_n_senesced
                   ,float  *g_n_uptake_stover_tot
                   ,float  *g_n_uptake_tot
                   ,float  *g_dlt_n_green
                   ,float  *g_n_fix_uptake
                   ,float  *g_n_fixed_tops
                   ,float  *g_root_depth
                   ,float  *g_transpiration_tot
                   )  ;
  void plant_event(float *g_dlayer
                   ,float *g_dm_dead
                   ,float *g_dm_green
                   ,float *g_dm_senesced
                   ,float *g_n_green
                   ,float  g_root_depth
                   ,float *g_sw_dep
                   ,float *p_ll_dep);

  void plant_root_incorp (
                          float  dlt_dm_root
                         ,float  dlt_n_root
                         ,float  dlt_p_root
                         ,float  *root_length)               ;
  void plant_dm_init (float  c_dm_root_init
////		      ,float  c_pod_trans_frac
		      ,float  g_plants
		      ,float  *dm_green
		      ,float  *dm_plant_min);

  void plant_root_depth (int option /* (INPUT) option number*/);
  void plant_water_supply (int option /* (INPUT) option number*/);
  void plant_water_demand (int option /* (INPUT) option number*/);
  void plant_water_distribute (int option /*(INPUT) option number*/);
  void plant_water_uptake (int option /*(INPUT) option number*/);
  void plant_light_supply_partition (int option /*(INPUT) option number*/);
  void plant_bio_rue (int option /*(INPUT) option number*/);
  void plant_transpiration_eff (int option /*(INPUT) option number*/);
  void plant_sen_root_length (int option /*(INPUT) option number*/);
  void plant_root_depth_init (int option /*(INPUT) option number*/);
  void plant_root_length_growth (int option /*(INPUT) option number*/);
  void plant_root_length_init (int option /*(INPUT) option number*/);
  void plant_water_supply_partition(float sw_demand
                                  , float swDemandVeg
                                  , float swSupply
                                  , float *swSupplyVeg
                                  , float *swSupplyFruit);

 void plant_dm_pot_rue (externalFunction *c_rue
                         ,float  rue_pod
                         ,float  cover_green
                         ,float  cover_pod
                         ,double  radn_int
                         ,double  stress_factor
                         ,float g_co2, float g_maxt, float g_mint
                         ,photosynthetic_pathway_t photosynthetic_pathway
                         ,float  *dlt_dm_pot);

 void plant_dm_pot_rue_veg (externalFunction *c_rue
                          , double  radn_int
                          , double  stress_factor
                          , float g_co2
                          , float g_maxt
                          , float g_mint
                          , photosynthetic_pathway_t photosynthetic_pathway
                          , float  *dlt_dm_pot);

  void plant_rue_co2_modifier(photosynthetic_pathway_t,  //!please use 'C3' or 'C4' for crop_type
                              float co2,                 //!CO2 level (ppm)
                              float maxt,                //!daily max temp (C)
                              float mint,                //!daily min temp (C)
                              float *modifier);           //!modifier (-)

  void plant_n_conc_limits (float  c_n_conc_crit_root
      			     ,float  c_n_conc_max_root
      			     ,float  c_n_conc_min_root
      			     ,float  *c_x_stage_code
      	                 ,float  *c_x_co2_nconc_modifier
                             ,float  *c_y_co2_nconc_modifier
                             ,int    c_num_co2_nconc_modifier
                             ,float  g_co2
      			     ,float  g_current_stage
      			     ,float  *n_conc_crit
      			     ,float  *n_conc_max
      			     ,float  *n_conc_min) ;

  void legnew_n_partition
    (float  *g_dlayer
    ,float  *g_dlt_no3gsm
    ,float  *g_dlt_nh4gsm
    ,float  *g_n_demand
    ,float  g_n_fix_pot
    ,float  *g_n_max
    ,float  g_root_depth
    ,float  *dlt_n_green
    ,float  *n_fix_uptake
    ,vector<plantPart *> &);        // (INPUT) vector of plant parts
////    ,plantPart * oilPart
////    ,plantPart * mealPart) ;

  void legnew_dm_partition1 (float c_frac_leaf
                              , float c_ratio_root_shoot
                              , float c_sla_min
                              , double g_dlt_dm
                              , float dm_yield_demand_fruit
                              , double *dlt_dm_fruit
                              , float *g_dlt_dm_green);

  void legnew_dm_partition2 (float  g_current_stage
                               , float  *c_x_stage_no_partition
                               , float  *c_y_frac_leaf
                               , int    c_num_stage_no_partition
                               , float *c_y_ratio_root_shoot
                               , float c_sla_min
                               , double g_dlt_dm
                               , float dm_yield_demand_fruit
                               , double *dlt_dm_fruit
                               , float *dlt_dm_green);

  void legnew_dm_partition1
    (
     float  c_frac_leaf
    ,float  c_frac_pod
    ,float  g_grain_energy
    ,float  c_grain_oil_conc
    ,float  c_ratio_root_shoot
    ,float  c_sla_min
    ,double  g_dlt_dm
    ,float  g_dlt_dm_grain_demand
    ,float  dlt_dm_oil_conv
    ,float  *dlt_dm_green
    ) ;
  void legnew_dm_partition2
    (
     float  g_current_stage
    ,float  *c_x_stage_no_partition
    ,float  *c_y_frac_leaf
    ,float  *c_y_frac_pod
    ,int    c_num_stage_no_partition
    ,float  g_grain_energy
    ,float  c_grain_oil_conc
    ,float  *c_y_ratio_root_shoot
    ,float  c_sla_min
    ,double  g_dlt_dm
    ,float  g_dlt_dm_grain_demand
    ,float  dlt_dm_oil_conv
    ,float  *dlt_dm_green
    ) ;

void Plant::legnew_dm_retranslocate
    (vector<plantPart *> &allParts        // (INPUT) all parts of plant
    ,vector<plantPart *> &supply_pools    // (INPUT)
    ,float  g_dm_demand_differential      // (INPUT)  grain dm demand (g/m^2)
    ,float  g_plants                      // (INPUT)  Plant density (plants/m^2)
    ,float  *dlt_dm_retrans_to_fruit);    // (OUTPUT) dm retranslocated to fruit (g/m^2)

  void legnew_n_retranslocate(float g_grain_n_demand);
//  void legnew_n_retranslocate_test( int    *supply_pools
//                                , int    num_supply_pools
//                                , float  *g_n_conc_min               // (INPUT)  minimum N concentration (g N/g
//                                , float  *g_dm_green                 // (INPUT)  live plant dry weight (biomass
//                                , float  *g_n_green                  // (INPUT)  plant nitrogen content (g N/m^
//                                , float  g_grain_n_demand            //  INPUT
//                                , float  dlt_n_retrans_supply        // (OUTPUT) plant N supply to fruit (g N/m^2)
//                                , float  *dlt_n_retrans              // (OUTPUT) plant N taken out from plant parts (g N/m^2)
//                                );


  void legnew_leaf_death_leg
    (
     float  c_sen_start_stage
    ,float  c_fr_lf_sen_rate
    ,float  c_node_sen_rate
    ,float  g_nfact_expansion
    ,float  c_n_fact_lf_sen_rate
    ,float  g_dlt_tt
    ,float  *g_leaf_no
    ,float  *g_leaf_no_dead
    ,float  *g_leaf_area
    ,float  c_min_tpla
    ,float  *dlt_leaf_no_dead
    ) ;
  void plant_N_senescence (int num_part                  //(INPUT) number of plant part
                        ,float *c_n_sen_conc           //(INPUT)  N concentration of senesced materia  (g/m^2)
                        ,float *g_n_conc_max           //(INPUT) critical N conc
                        ,float* g_dlt_dm_senesced      // (INPUT)  plant biomass senescence (g/m^2)
                        ,float* g_n_green              //(INPUT) nitrogen in plant material (g/m^2)
                        ,float* g_dm_green             // (INPUT) plant material (g/m^2)
                        ,float* g_n_demand             //
                        ,float* dlt_n_senesced_trans   // (OUTPUT)  plant N senescence (g/m^2)
                        ,float* dlt_n_senesced_retrans //
                        ,float* dlt_n_senesced);        //  (OUTPUT) actual nitrogen senesced
                                                       //    from plant parts (g/m^2)

   void plant_root_incorp (float dlt_dm_root,
      float dlt_n_root, float dlt_p_root, float *g_dlayer, float *g_root_length, float g_root_depth,
      const char *c_crop_type);
  void plant_soil_n_demand1 (float *);
  void plant_process ( void );
  void plant_dead (void);
  void plant_harvest (protocol::Variant &v/*(INPUT) message variant*/);
  void plant_kill_stem (protocol::Variant &v/*(INPUT) incoming message variant*/);
  void plant_remove_crop_biomass (protocol::Variant &v/*(INPUT) incoming message variant*/);
  void plant_dormancy (protocol::Variant &v/*(INPUT) incoming message variant*/);
  void plant_harvest_update (protocol::Variant &v/*(INPUT)message arguments*/);
  void plant_kill_stem_update (protocol::Variant &v/*(INPUT) message arguments*/);
  void plant_remove_biomass_update (protocol::Variant &v/*(INPUT) message arguments*/);
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
                                      ,vector<float>  &fraction_to_residue); // (INPUT) residue fraction to residue (0-1)
  void plant_n_demand( int max_part                       // (INPUT)
       , int *demand_parts                  // (INPUT)
       , int num_demand_parts               // (INPUT)
       , int grain_part_no                  //
       , float g_dlt_dm                     // (INPUT)  the daily biomass production (
       , float *g_dlt_dm_green              // (INPUT)  plant biomass growth (g/m^2)
       , float g_dlt_dm_pot_rue             // (INPUT)  potential dry matter productio
       , float *g_dm_green                  // (INPUT)  live plant dry weight (biomass
       , float *g_n_conc_crit               // (INPUT)  critical N concentration (g N/
       , float *g_n_conc_max                // (INPUT)  maximum N concentration (g N/g
       , float *g_n_green                   // (INPUT)  plant nitrogen content (g N/m^
       , float g_grain_n_demand             //
       , float c_n_deficit_uptake_fraction  //
       , float *n_demand                    // (OUTPUT) critical plant nitrogen demand  (g/m^2)
       , float *n_max);                      // (OUTPUT) max plant nitrogen demand  (g/m^2)


  void plant_get_ext_uptakes (const char *uptake_source,        //(INPUT) uptake flag
                           const char *crop_type,            //(INPUT) crop type name
                           const char *uptake_type,          //(INPUT) uptake name
                           float unit_conversion_factor,     //(INPUT) unit conversion factor
                           float uptake_lbound,              //(INPUT) uptake lower limit
                           float uptake_ubound,              //(INPUT) uptake upper limit
                           float *uptake_array);              //(OUTPUT) crop uptake array



void legnew_dm_part_demands(float c_frac_pod              // (INPUT)  fraction of remaining dm allocated to pod
                          , float g_grain_energy          // multiplier of grain weight to account f
                          , float c_grain_oil_conc        // (INPUT)  grain dm demand (g/m^2)
                          , float g_dlt_dm_grain_demand   // multiplier of grain weight to account f
                          , float *dm_oil_conv_demand      // assimilate demand for reproductive parts (g/m^2)
                          , float *dlt_dm_demand_meal      // assimilate demand for reproductive parts (g/m^2)
                          , float *dlt_dm_demand_oil       // assimilate demand for reproductive parts (g/m^2)
                          , float *dlt_dm_demand_pod );     // assimilate demand for conversion to oil (g/m^2)

//JNGH implement? void legnew_dm_distribute(int max_part
//JNGH implement?                         , float *dm_remaining          // interim dm pool for partitioning
//JNGH implement?                         , float dlt_dm_demand_meal    // assimilate demand for reproductive parts (g/m^2)
//JNGH implement?                         , float dlt_dm_demand_oil     // assimilate demand for reproductive parts (g/m^2)
//JNGH implement?                         , float dlt_dm_demand_pod     // assimilate demand for reproductive parts (g/m^2)
//JNGH implement?                         , float dm_oil_conv_demand    // assimilate demand for conversion to oil (g/m^2)
//JNGH implement?                         , float dlt_dm_oil_conv       // (OUTPUT) actual biomass used in conversion to oil (g/m2)
//JNGH implement?                         , float *dlt_dm_green);       // (OUTPUT) actual biomass partitioned

  void plant_get_site_characteristics ();
  bool set_plant_crop_class(protocol::QuerySetValueData&v);
  bool set_plant_grain_oil_conc(protocol::QuerySetValueData&v);

  void get_plant_status(protocol::Component *, protocol::QueryValueData &) const;
  float getStageCode(void) const ;

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
  photosynthetic_pathway_t getPhotosynthetic_pathway(void) const;
//  float getRadnInterceptedPod(void) const;
  float getDltDMPotRueVeg(void) const;
  float getDmGreenVeg(void) const;
//  float getDltDmVeg(void) const;
  float getWaterSupplyPod(void) const;
  float getDmTops(void) const;
  float getDltDm(void) const;
  float getDmVeg(void) const;
  float getDmGreenStem(void) const;
  float getDmGreenTot(void) const;
  float getRelativeGrowthRate(void);
  float getDyingFractionPlants(void);
////  PlantComponent *system(void);

  float getTempStressPhoto(void) const;
  float getNfactPhoto(void) const;
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
  void get_dlt_dm_oil_conv(protocol::Component *, protocol::QueryValueData &);
  void get_dlt_dm_oil_conv_retrans(protocol::Component *, protocol::QueryValueData &);
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
  void get_leaves_per_node(protocol::Component *, protocol::QueryValueData &);
  void get_dlt_slai_age(protocol::Component *, protocol::QueryValueData &);
  void get_dlt_slai_light(protocol::Component *, protocol::QueryValueData &);
  void get_dlt_slai_water(protocol::Component *, protocol::QueryValueData &);
  void get_dlt_slai_frost(protocol::Component *, protocol::QueryValueData &);

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
  void get_ll(protocol::Component *systemInterface, protocol::QueryValueData &qd);

  int  getDayOfYear(void) {return (g.day_of_year);};

  // To transfer to Fruit class
  void plant_bio_distribute (int option /* (INPUT) option number */);

  //Phosporousy things:
  void zero_p_variables ();
  void zero_daily_p_variables ();
  void read_p_constants (PlantComponent *systemInterface);
  void doPInit(PlantComponent *systemInterface);
  void PlantP_set_phosphorus_aware (PlantComponent *systemInterface);

  bool phosphorusAware(void) const {return g.phosphorus_aware;};
  void prepare_p(void);
  void plant_p_retrans (void);
  void detachment_p(void);
  void summary_p (void);

  void  PlantP_demand (vector<plantPart *>&, float dlt_dm_pot);
  void  PlantP_Stress (vector<plantPart *>&);
  void  PlantP_init_pools (vector<plantPart*>&);
  void  PlantP_partition (vector<plantPart*>&);
  void  PlantP_senescence (vector<plantPart*>&);
  void  PlantP_retrans (vector<plantPart*>&);
  void  PlantP_detachment (vector<plantPart*>&);
  float PlantP_Pfact (vector<plantPart *>&);

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
       unsigned int sw_dep;
       unsigned int no3;
       unsigned int no3_min;
       unsigned int nh4;
       unsigned int nh4_min;

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
       unsigned int dlt_sw_dep;

       // events.
       unsigned int crop_chopped;
       unsigned int incorp_fom;
       unsigned int incorp_fom_p;
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
  //     ================================================================
  //       plant Globals
  //     ================================================================
  struct {
      bool  hasreadconstants;
      string   module_name;                             // module name
      string   crop_class;                                // crop type
      status_t plant_status;                              // status of crop
      bool  plant_status_out_today;
      string   cultivar;                                // name of cultivar
      string   pre_dormancy_crop_class;
      string   averageStressMessage;                    // Message of average stresses for each phase
      float swdef_expansion;
      float swdef_photo;
      float swdef_pheno;
      float swdef_fixation;
      float sw_avail_fac_deepest_layer;
      float nfact_expansion;
      float nfact_photo;
      float nfact_grain_conc;
      float nfact_pheno;
      float remove_biom_pheno;
      float temp_stress_photo;
      float oxdef_photo;
      float row_spacing;                                // row spacing (m) [optional]
      float skip_row;                                   // skip row (0, 1, 2)
      float skip_plant;                                 // skip plant (0, 1, 2)
      float skip_row_fac;                               // skip row factor
      float skip_plant_fac;                             // skip plant factor
      int   year;                                       // year
      int   day_of_year;                                // day of year
      float fr_intc_radn;                               // fraction of radiation intercepted by canopy
      float latitude;                                   // latitude (degrees, negative for southern hemisphere)
      float radn;                                       // solar radiation (Mj/m^2/day)
      float mint;                                       // minimum air temperature (oC)
      float maxt;                                       // maximum air temperature (oC)
      float soil_temp[366+1];                             // soil surface temperature (oC)
      float eo;                                         // potential evapotranspiration (mm)
      factorObserver cnd_photo;                      // cumulative nitrogen stress type 1
      factorObserver cnd_grain_conc ;                // cumulative nitrogen stress type 2
      factorObserver cswd_pheno;                     // cumulative water stress type 1
      factorObserver cswd_photo;                     // cumulative water stress type 1
      factorObserver cswd_expansion ;                // cumulative water stress type 2
      float dlt_canopy_height;                          // change in canopy height (mm)
      float dlt_canopy_width;                           // change in canopy height (mm)
      float canopy_width;                              // canopy height (mm)
      float plants;                                    // Plant density (plants/m^2)
      float dlt_plants;                                 // change in Plant density (plants/m^2)
      float dlt_root_depth;                             // increase in root depth (mm)
      float root_depth;                                 // depth of roots (mm)
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
      float dltDmPotRueFruit;                           // potential dry matter production of fruit with
                                                        // optimum water and nitrogen and
                                                        // temperature stress conditions (g/m^2)
      float dltDmPotTeFruit;                            // the potential daily biomass production of fruit from te (g/m^2)
      float dlt_dm_oil_conv;                            // plant biomass used in conversion to oil (g/m^2)
      double dlt_dm_supply_to_fruit;                     // dry matter supplied to fruit from assimilate (g/m^2)
      float dlt_dm_yield_demand_fruit;                  // dry matter demand by fruit (g/m^2)
      float dlt_dm_retrans_to_fruit;                    // dry matter retranslocated to fruit (g/m^2)
      float dlt_dm_green[max_part];                     // plant biomass growth (g/m^2)
      float dlt_dm_senesced[max_part];                  // plant biomass senescence (g/m^2)
      float dlt_dm_detached[max_part];                  // plant biomass detached (g/m^2)
      float dlt_dm_green_dead[max_part];                // plant biomass to dead population(g/m^2)
      float dlt_dm_senesced_dead[max_part];             // plant biomass to dead population(g/m^2)
      float dlt_dm_dead_detached[max_part];             // plant biomass detached from dead plant (g/m^2)
      float dlt_dm_oil_conv_retranslocate;              // retranslocated plant biomass used in conversion to oil for (g/m^2)
      float dlt_dm_green_retrans[max_part];             // plant biomass retranslocated (g/m^2)
//      stateObserver dm_stress_max;                      // sum of maximum daily stress on dm production per phase
//      float dlt_dm_stress_max;                          // maximum daily stress on dm production (0-1)
      float dlt_dm_grain_demand;                        // grain dm demand (g/m^2)
      float dm_green_demand[max_part];                  // biomass demand of the plant parts (g/m^2)
      float dm_dead[max_part];                          // dry wt of dead plants (g/m^2)
      float dm_green[max_part];                         // live plant dry weight (biomass) (g/m^2)
      float dm_senesced[max_part];                      // senesced plant dry wt (g/m^2)
      float radn_int;                                   // radn intercepted by leaves (mj/m^2)
      float radnIntGreenFruit;                          // radn intercepted by fruit (mj/m^2)
      float transp_eff;                                 // transpiration efficiency (g dm/m^2/mm water)
      float transpEffFruit;                             // transpiration efficiency of fruit (g dm/m^2/mm water)
//      float slai;                                       // area of leaf that senesces from plant
//      float dlt_slai;                                   // area of leaf that senesces from plant
//      float dlt_lai;                                    // actual change in live plant lai
//      float dlt_lai_pot;                                // potential change in live plant lai
//      float dlt_lai_stressed;                           // potential change in lai  allowing for stress
//      float lai;                                        // live plant green lai
      float lai_canopy_green;                           // green lai of canopy
//      float tlai_dead;                                  // total lai of dead plants
//      float dlt_tlai_dead;                              // plant lai change in dead plant
//      float dlt_tlai_dead_detached;                     // plant lai detached from dead plant
      float dlt_slai_detached;                          // plant senesced lai detached
      float dlt_slai_age;                               // senesced lai from age
      float dlt_slai_light;                             // senesced lai from light
      float dlt_slai_water;                             // senesced lai from water
      float dlt_slai_frost;                             // senesced lai from frost
      float pai;
      float dlt_pai;
      float leaf_no[max_node];                          // number of fully expanded leaves ()
      float node_no;                                    // number of fully expanded nodes ()
      float leaf_no_dead[max_node];                     // no of dead leaves ()
      float dlt_leaf_no;                                // actual fraction of oldest leaf expanding ()
      float dlt_node_no;                                // actual fraction of oldest node expanding ()
      float dlt_leaf_no_pot;                            // potential fraction of oldest leaf expanding ()
      float dlt_node_no_pot;                            // potential fraction of oldest leaf expanding ()
      float dlt_leaf_no_dead;                           // fraction of oldest green leaf senesced ()
      float leaf_no_final;                              // total number of leaves the plant produces
      float leaf_area[max_node];                        // leaf area of each leaf (mm^2)
      float lai_equilib_light[366+1];                     // lai threshold for light senescence
      float lai_equilib_water[366+1];                     // lai threshold for water senescence
      float n_demand [max_part];                        // critical plant nitrogen demand (g/m^2)
      float soil_n_demand[max_part];
      float grain_n_demand;                             // grain n demand from soil OR retrans
      float grain_n_supply;                             // grain n supply from soil OR retrans
      float n_max [max_part];                           // maximum plant nitrogen demand (g/m^2)
      float dlt_n_green[max_part];                      // actual N uptake into plant (g/m^2)
      float dlt_n_senesced[max_part];                   // actual N loss with senesced plant (g/m^2)
      float dlt_n_senesced_retrans[max_part];
      float dlt_n_senesced_trans[max_part];
      float dlt_n_detached[max_part];                   // actual N loss with detached plant (g/m^2)
      float dlt_n_dead[max_part];                       // actual N loss with dead plant (g/m^2)
      float dlt_n_green_dead[max_part];                 // plant N to dead population(g/m^2)
      float dlt_n_senesced_dead[max_part];              // plant N to dead population(g/m^2)
      float dlt_n_dead_detached[max_part];              // actual N loss with detached dead plant (g/m^2)
      float n_dead[max_part];                           // plant N content of dead plants (g N/m^2)
      float n_green[max_part];                          // plant nitrogen content (g N/m^2)
      float n_senesced[max_part];                       // plant N content of senesced plant (g N/m^2)
      float dlt_n_retrans[max_part];                    // nitrogen retranslocated out from parts to grain (g/m^2)
      float dlt_no3gsm[max_layer];                      // actual NO3 uptake from soil (g/m^2)
      float no3gsm [max_layer];                         // nitrate nitrogen in layer L (g N/m^2)
      float no3gsm_min[max_layer];                      // minimum allowable NO3 in soil (g/m^2)
      float no3gsm_diffn_pot[max_layer];                // potential NO3 (supply) from soil (g/m^2), by diffusion
      float no3gsm_mflow_avail[max_layer];              // potential NO3 (supply) from soil (g/m^2) by mass flow
      float dlt_nh4gsm[max_layer];                      // actual NH4 uptake from soil (g/m^2)
      float nh4gsm [max_layer];                         // nitrate nitrogen in layer L (g N/m^2)
      float nh4gsm_min[max_layer];                      // minimum allowable NH4 in soil (g/m^2)
      float nh4gsm_diffn_pot[max_layer];                // potential NH4 (supply) from soil (g/m^2), by diffusion
      float nh4gsm_mflow_avail[max_layer];              // potential NH4 (supply) from soil (g/m^2) by mass flow
      float nh4gsm_uptake_pot[max_layer];
      float n_fix_pot;                                  // N fixation potential (g/m^2)
      float no3gsm_uptake_pot[max_layer];
      float n_fix_uptake;                               // N fixation actual (g/m^2)
      float n_fixed_tops;                               // cum. fixed N in tops
      float n_conc_crit[max_part];                      // critical N concentration (g N/g biomass)
      float n_conc_max[max_part];                       // maximum N concentration (g N/g biomass)
      float n_conc_min[max_part];                       // minimum N concentration (g N/g biomass)
      float dm_plant_min[max_part];                     // minimum weight of each plant part (g/plant)
////      float cover_pod;
      float dlayer [max_layer];                         // thickness of soil layer I (mm)
      float dlt_sw_dep[max_layer];                      // water uptake in each layer (mm water)
      float ll15_dep[max_layer];
      float dul_dep [max_layer];                        // drained upper limit soil water content for soil layer L (mm water)
      float sat_dep[max_layer];
      float bd[max_layer];
      float sw_dep [max_layer];                         // soil water content of layer L (mm)
      float swSupplyFruit;                              // crop water water supply to fruit (mm)
      float swSupplyVeg;                                // crop water water supply to vegetative parts (mm)
      float sw_demand;                                  // total crop demand for water (mm)
      float sw_demand_te;                               // crop demand for water calculated from transpiration efficiency (mm)
      float swDemandTEFruit;                            // crop fruit demand for water calculated from transpiration efficiency (mm)
      float sw_avail_pot[max_layer];                    // potential extractable soil water (mm)
      float sw_avail[max_layer];                        // actual extractable soil water (mm)
      float sw_supply [max_layer];                      // potential water to take up (supply)
      // from current soil water (mm)
      int   num_layers;                                 // number of layers in profile ()
      float transpiration_tot;                          // cumulative transpiration (mm)
      float n_uptake_tot;                               // cumulative total N uptake (g/m^2)
      float n_demand_tot;                               // sum of N demand since last output (g/m^2)
      float n_conc_act_stover_tot;                      // sum of tops actual N concentration (g N/g biomass)
      float n_conc_crit_stover_tot;                     // sum of tops critical N concentration (g N/g biomass)
      float n_uptake_stover_tot;                        // sum of tops N uptake (g N/m^2)
      float lai_max;                                    // maximum lai - occurs at flowering
      float dlt_root_length_dead[max_layer];                     // root length (mm/mm^2)
      float root_length[max_layer];                     // root length (mm/mm^2)
      float root_length_dead[max_layer];                // root length of dead population (mm/mm^2)
      float dlt_root_length[max_layer];                 // root length growth (mm/mm^2)
      float dlt_root_length_senesced[max_layer];        // root length senescence (mm/mm^2)
      float ext_n_demand;
      float ext_sw_demand;                              // Note: currently unused - use sw_demand
      float grain_energy;                               // multiplier of grain weight to account
      // for seed energy content
      float leaves_per_node;

      float swdef_pheno_flower;
      float swdef_pheno_grainfill;

         // parasite
      float       dlt_dm_parasite_demand;  // parasite dm demand [g/m^2]
      float       dlt_sw_parasite_demand;  // parasite dm demand [g/m^2]
      float       dm_parasite_retranslocate;    // plant biomass retranslocated to parasite [g/m^2]
      float       dlt_dm_parasite;      // parasite biomass growth [g/m^2]

      // Phosphorous
      float p_green[max_part];
      float p_sen[max_part];
      float p_dead[max_part];
      float dlt_p_green[max_part];
      float dlt_p_sen[max_part];
      float dlt_p_det[max_part];
      float dlt_p_dead_det[max_part];
      float dlt_p_retrans[max_part];
      float dlt_p_dead[max_part];
      float p_demand[max_part];
      float pfact_photo;
      float pfact_expansion;
      float pfact_pheno;
      float pfact_grain;
      bool  phosphorus_aware;

      float     co2;
    } g;   // Globals


//     ================================================================
//       plant Parameters
//     ================================================================
    struct {
//      float grains_per_gram_stem;
//      float potential_grain_filling_rate;

      float x_pp_hi_incr[max_table];
      float y_hi_incr[max_table];                       // harvest index increment per day ()
      int   num_pp_hi_incr;
      int   num_hi_max_pot;
      float x_hi_max_pot_stress[max_table];             // maximum harvest index (g grain/g biomass)
      float y_hi_max_pot[max_table];                    // maximum harvest index (g grain/g biomass)
      float kl[max_layer];                              // root length density factor for water
      float ll_dep[max_layer];                          // lower limit of plant-extractable
                                                        // soil water for soil layer L (mm)
      float xf[max_layer];                              // root exploration factor (0-1)
      string  uptake_source;                            // source of uptake information
      float eo_crop_factor;                             // Crop factor for sw demand applied to Eo

      float     root_distribution_pattern;    // root dist patt for root_growth_option == 2
      float minTempGrnFill;
      int   daysDelayGrnFill;
    } p; // Parameters


   //     ================================================================
   //       plant Constants
   //     ================================================================
   struct {
      int   grain_fill_option;
      int   grain_n_option;
      int   n_uptake_option;
      int   leaf_no_pot_option;
      int   partition_option;
      int   n_retrans_option;
      int   n_stress_option;
      int   n_senescence_option;
      int   dm_senescence_option;

      float sen_start_stage;
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
      float x_sw_ratio [max_table];
      float y_sw_fac_root [max_table];
      float x_ws_root [max_table];
      float y_ws_root_fac [max_table];
      float x_sw_demand_ratio [max_table];
      float y_swdef_leaf [max_table];
      float x_sw_avail_ratio [max_table];
      float y_swdef_pheno [max_table];
      float x_sw_avail_fix [max_table];
      float y_swdef_fix [max_table];
      float oxdef_photo [max_table];
      float oxdef_photo_rtfr[max_table];
      int   num_oxdef_photo;
      int   num_sw_ratio;
      int   num_ws_root;
      int   num_sw_demand_ratio;
      int   num_sw_avail_ratio;
      int   num_sw_avail_fix;
      float twilight;                                   // twilight in angular distance between
                                                        // sunset and end of twilight - altitude
                                                        // of sun. (deg)
      float x_lai_ratio[max_table];                     // ratio table for critical leaf size
                                                        // below which leaf number is reduced ()
      float y_leaf_no_frac[max_table];                  // reduction in leaf appearance ()
      int   num_lai_ratio;                              // number of ratios in table ()
////      float n_conc_crit_grain;                          // critical N concentration of grain (g N/g biomass)
////      float n_conc_max_grain;                           // maximum N concentration of grain (g N/g biomass)
////      float n_conc_min_grain;                           // minimum N concentration of grain (g N/g biomass)
      float n_conc_crit_root;                           // critical N concentration of root (g N/g biomass)
      float n_conc_max_root;                            // maximum N concentration of root (g N/g biomass)
      float n_conc_min_root;                            // minimum N concentration of root (g N/g biomass)
      float x_stage_code[max_table];                    // stage table for N concentrations (g N/g biomass)
      float y_n_conc_crit_leaf[max_table];              // critical N concentration of leaf (g N/g biomass)
      float y_n_conc_max_leaf[max_table];               // maximum N concentration of leaf (g N/g biomass)
      float y_n_conc_min_leaf[max_table];               // minimum N concentration of leaf (g N/g biomass)
////      float y_n_conc_crit_pod[max_table];               // critical N concentration of pod(g N/g biomass)
////      float y_n_conc_max_pod[max_table];                // maximum N concentration of pod (g N/g biomass)
////      float y_n_conc_min_pod[max_table];                // minimum N concentration of pod (g N/g biomass)
      float n_fact_photo;                               // multipler for N deficit effect on photosynthesis
      float n_fact_pheno;                               // multipler for N deficit effect on phenology
      float n_fact_expansion;
      float n_retrans_fraction;
      float n_init_conc[max_part];                      // initial N concentration (gN/gdm)
      float n_sen_conc[max_part];                       // N concentration of senescedmaterial (gN/gdm)
      int   num_n_conc_stage;                           // no of values in stage table
      float x_row_spacing[max_table];
      float y_extinct_coef[max_table];
      float y_extinct_coef_dead[max_table];
      interpolationFunction rue;                        // radiation use efficiency as f(stage number) (g dm/mj)
      float root_depth_rate[max_table];                 // root growth rate potential (mm depth/day)

      int   num_row_spacing;
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
      float initial_root_depth;                         // initial depth of roots (mm)
      float x_lai [max_table];                          // lookup for sla max
      float y_sla_max[max_table];                       // lookup for sla max
      float sla_min;                                    // minimum specific leaf area for
                                                        // new leaf area (mm^2/g)
      float initial_tpla;                               // initial plant leaf area (mm^2)
      float min_tpla;                                   // minimum plant leaf area(mm2/plant)
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
      int   num_lai;
      float grain_n_conc_min;                           // minimum nitrogen concentration of grain

      float seed_wt_min;                                // minimum grain weight (g/kernel)
      float leaf_no_at_emerg;                           // leaf number at emergence ()
      float no3_diffn_const;                            // time constant for uptake by
                                                        // diffusion (days). H van Keulen &
                                                        // NG Seligman. Purdoe 1987. This is the
                                                        // time it would take to take up by
                                                        // diffusion the current amount of N if
                                                        // it wasn't depleted between time steps
      float n_fix_rate[max_table];                      // potential rate of N fixation (g N fixed
                                                        // per g above ground biomass
      float x_node_no_app[max_table];
      float y_node_app_rate[max_table];
      float x_node_no_leaf[max_table];
      float y_leaves_per_node[max_table];
      float dm_init [max_part];                         // initial dm (g/plant)
      float leaf_init_rate;                             // growing degree days to initiate each le
                                                        // primordium until fl_initling (deg day)
      float leaf_no_seed;                               // number of leaf primordia present in
                                                        // seed
      float **x_dm_sen_frac;
      float **y_dm_sen_frac;
      int   *num_dm_sen_frac;

      float dead_detach_frac[max_part];                 // fraction of dead plant parts
                                                        // detaching each day (0-1)
      float sen_detach_frac[max_part];                  // fraction of dead plant parts
                                                        // detaching each day (0-1)

      int   num_node_no_app;
      int   num_node_no_leaf;
      float swdf_grain_min;                             // minimum of water stress factor
      float hi_min;                                     // minimum harvest index (g grain/
                                                        // g biomass)
      float sfac_slope;                                 // soil water stress factor slope
      float tfac_slope;                                 // temperature stress factor slope
      float lai_sen_light;                              // critical lai above which light
      float sw_fac_max;                                 // soil water stress factor maximum
      float x_temp_senescence[max_table];               // temperature senescence
                                                        // table (oC)
      float y_senescence_fac[max_table];                // temperature factor
                                                        // senescence table (0-1)
      float temp_fac_min;                               // temperature stress factor minimum
                                                        // optimum temp
      float spla_slope;                                 // regression slope for calculating
                                                        // inflection point for leaf senescence
      float sen_threshold;                              // supply:demand ratio for onset of
                                                        // water senescence
      float sen_rate_water;                             // slope in linear eqn
                                                        // relating soil water
                                                        // stress during photosynthesis
                                                        // to leaf senesense rate
      float sen_light_slope;                            // slope of linear relationship
                                                        // between lai and
                                                        // light competition factor for
                                                        // determining leaf senesence rate.
      int   num_temp_senescence;                        // number of temperatures in
                                                        // senescence table
      float grn_water_cont;                             // water content of grain g/g
      float partition_rate_leaf;                        // rate coefficient of sigmoidal
                                                        // function between leaf partition
                                                        // fraction and internode no**2 (0-1)

      float frac_leaf[max_table];                       // fraction of remaining dm allocated to leaves
      float frac_pod[max_table];                        // fraction of dm or grain weight allocated to pod
      float ratio_root_shoot[max_table];                // root:shoot ratio of new dm ()

      float x_stage_no_partition[max_table];
      float y_frac_leaf[max_table];                     // fraction of remaining dm allocated to leaves
      float y_frac_pod[max_table];                      // fraction of dm or grain weight allocated to pod
      float y_ratio_root_shoot[max_table];              // root:shoot ratio of new dm ()

      int   num_stage_no_partition;
      float leaf_trans_frac;                            // fraction of leaf used in translocat
                                                        // to grain
                                                        // to grain
      float htstress_coeff;                             // coeff for conversion of heat stress
                                                        // during flowering to
                                                        // heat stress factor on grain number
                                                        // development.
      float temp_grain_crit_stress;                     // temperature above which heat stress
                                                        // occurs
      float node_sen_rate;
      float fr_lf_sen_rate;
      float n_fact_lf_sen_rate;
      float carbo_oil_conv_ratio;                       // Carbohydrate:oil conversion ratio (>= 1.0)
      float grain_oil_conc;                             // fractional oil content of grain (0-1)
      float node_no_correction;
      float x_node_no[max_table];                       // lookup for leaf size
      float y_leaf_size[max_table];                     // lookup for leaf size
      int   num_node_no;
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
      int   num_temp;                                   // size_of table
      int   num_ave_temp;                               // size_of critical temperature table
      int   num_temp_grain;                             // size_of table
      int   num_factors;                                // size_of table
      int   num_temp_other;                             //
      int   num_weighted_temp;                          // size of table
      float kl_ub;                                      // upper limit of water uptake factor
      float sw_dep_ub;                                  // upper limit of soilwater depth (mm)
      float sw_dep_lb;                                  // lower limit of soilwater depth (mm)
      float sw_ub;                                      // upper limit of soilwater depth (mm/mm)
      float sw_lb;                                      // lower limit of soilwater depth (mm/mm)
      float no3_ub;                                     // upper limit of soil NO3 (kg/ha)
      float no3_lb;                                     // lower limit of soil NO3 (kg/ha)
      float no3_min_ub;                                 // upper limit of minimum soil NO3 (kg/ha)
      float no3_min_lb;                                 // lower limit of minimum soil NO3 (kg/ha)
      float nh4_ub;                                     // upper limit of soil NH4 (kg/ha)
      float nh4_lb;                                     // lower limit of soil NH4 (kg/ha)
      float nh4_min_ub;                                 // upper limit of minimum soil NH4 (kg/ha)
      float nh4_min_lb;                                 // lower limit of minimum soil NH4 (kg/ha)
      float leaf_no_min;                                // lower limit of leaf number ()
      float leaf_no_max;                                // upper limit of leaf number ()
      float latitude_ub;                                // upper limit of latitude for model (oL)
      float latitude_lb;                                // lower limit of latitude for model(oL)
      float maxt_ub;                                    // upper limit of maximum temperature (oC)
      float maxt_lb;                                    // lower limit of maximum temperature (oC)
      float mint_ub;                                    // upper limit of minimum temperature (oC)
      float mint_lb;                                    // lower limit of minimum temperature (oC)
      float radn_ub;                                    // upper limit of solar radiation (Mj/m^2)
      float radn_lb;                                    // lower limit of solar radiation (Mj/M^2)
      float dlayer_ub;                                  // upper limit of layer depth (mm)
      float dlayer_lb;                                  // lower limit of layer depth (mm)
      float row_spacing_default;
      float skip_row_default;                           //Default skip row ()
      float skip_plant_default;                         //Default skip plant ()
      float specific_root_length;                       // as name suggests (mm/g)
      float root_die_back_fr;                           // fraction of roots dying at harvest
      float x_plant_rld [max_table];
      float y_rel_root_rate [max_table];
      int   num_plant_rld;
      vector<string> class_action;
      vector<string> class_change;

      float x_temp_root_advance[max_table];
      float y_rel_root_advance[max_table];
      int   num_temp_root_advance;

      float eo_crop_factor_default;                     // Default Crop factor for sw demand applied to Eo
      float n_deficit_uptake_fraction;
      float total_n_uptake_max;

      int        root_growth_option;

      float      x_sw_avail_ratio_flower[max_table];
      float      y_swdef_pheno_flower [max_table];
      float      x_sw_avail_ratio_grainfill [max_table];
      float      y_swdef_pheno_grainfill [max_table];
      int        num_sw_avail_ratio_flower;
      int        num_sw_avail_ratio_grainfill;

      float      co2_default;
      float      x_co2_te_modifier[max_table], y_co2_te_modifier[max_table];
      int        num_co2_te_modifier;
      float      x_co2_nconc_modifier[max_table], y_co2_nconc_modifier[max_table];
      int        num_co2_nconc_modifier;
      photosynthetic_pathway_t photosynthetic_pathway;
      string     remove_biomass_report;

      bool    p_stress_determinants[max_part];           //         character stress_determinants(max_part)*32
      bool    p_yield_parts[max_part];                   //         character yield_parts(max_part)*32
      bool    p_retrans_parts[max_part];                 //         character retrans_parts(max_part)*32

      float x_p_stage_code [max_table];
      int   num_x_p_stage_code;
      float y_p_conc_max [max_part][max_table];
      float y_p_conc_min [max_part][max_table];
      float y_p_conc_sen [max_part][max_table];
      float p_conc_init [max_part];
      float pfact_photo_slope;
      float pfact_expansion_slope;
      float pfact_pheno_slope;
      float pfact_grain_slope;

      }  c;   // Constants

   void setupHacks(vector<plantPart *> &parts);
   void setupHacks1(vector<plantPart *> &parts);
   void deleteHacks(vector<plantPart *> &parts);
};  // Plant

#endif //PLANT_H_
