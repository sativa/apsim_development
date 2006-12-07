#ifndef PLANT_H_
#define PLANT_H_

class PlantComponent;
class ApsimVariant;
class Plant;

typedef void (Plant::*ptr2getFn) (protocol::QueryValueData&);
typedef bool (Plant::*ptr2setFn) (protocol::QuerySetValueData&);
typedef void (Plant::*ptr2EventFn) (unsigned int &, protocol::Variant&);

typedef std::map<unsigned, ptr2getFn>   UInt2GetFnMap;
typedef std::map<unsigned, ptr2setFn>   UInt2SetFnMap;
typedef std::map<unsigned, ptr2EventFn> UInt2EventFnMap;

////////////////////////

//   Short description:
//      plant_tolerances
const float  tolerance_lai = 1.0e-4 ;

//   Short description:
//      array size_of settings
const int  max_node = 1000 ;                      // maximum number of plant leaves
const int  max_layer = 100 ;                      // Maximum number of layers in soil
const int  max_table = 20 ;                       // Maximum size_of of tables

//   Short description:
//      crop status names
extern const char*  status_alive;
extern const char*  status_dead;
extern const char*  status_out;
extern const char*  crop_inactive;

//   Short description:
//      Process names used for stress
// photosynthesis flag
const int  photo = 1 ;
// cell expansion flag
const int  expansion = 2 ;
// phenological flag
const int  pheno = 3 ;
// grain concentration flag
const int  grain_conc = 4 ;
// N fixation flag
const int  fixation = 5 ;

//   Short description:
//      plant part names
// root
const int  root = 1 ;
// leaf
const int  leaf = 2 ;
// stem
const int  stem = 3 ;
// pod
const int  pod = 4 ;
// meal - excludes oil component
const int  meal = 5 ;
// seed oil
const int  oil = 6 ;
// number of plant parts
const int  max_part = 6 ;

//   Short description:
//      Define crop phenological stage and phase names

// number of growth stages
const int  max_stage = 12 ;

// at this point in time ()
const int  now = max_stage+1 ;

// mechanical operations

// plant_end stage
const int  plant_end = 12 ;
// fallow phase
const int  fallow = plant_end ;
// Sowing stage
const int  sowing = 1 ;
// seed sow_to_germ phase
const int  sow_to_germ = sowing ;
// Germination stage
const int  germ = 2 ;
// germ_to_emerg elongation phase
const int  germ_to_emerg = germ ;
// Emergence stage
const int  emerg = 3 ;
// basic vegetative phase
const int  emerg_to_endjuv = emerg ;
// End of emerg_to_endjuv stage
const int  endjuv = 4 ;
// Photoperiod sensitive phase
const int  endjuv_to_init = endjuv ;
// Floral (Tassel) initiation stage
const int  floral_init = 5 ;
// flower development phase
const int  init_to_flower = floral_init ;
// flowering (Silking) stage
const int  flowering = 6 ;
// grain development phase
const int  flower_to_start_grain = flowering ;
// start of linear grain filling stage
const int  start_grain_fill = 7 ;
// linear grain filling phase
const int  start_to_end_grain = start_grain_fill ;
// End of linear (effective) grain filling
// stage
const int  end_grain_fill = 8 ;
// End of effective grain filling
const int  end_grain_to_maturity = end_grain_fill ;
// physiological maturity (black layer)
// stage
const int  maturity = 9 ;
// grain dry down phase
const int  maturity_to_ripe = maturity ;
// harvest ripe stage
const int  harvest_ripe = 10 ;
// harvest ready phase (waiting for
// harvest
const int  ripe_to_harvest = harvest_ripe ;       // by manager)

extern vector<string> part_name;


//   This class performs crop crop growth
//     simulates root, leaf, head, stem and grain development. Water and
//     nitrogen uptake, photosynhesis, and leaf and root senescense.
class Plant {
 private:
  PlantComponent *parent;                                // for interface calls to system
  
 public:
  Plant(PlantComponent *P)
    {
      part_name.push_back("root");
      part_name.push_back("leaf");
      part_name.push_back("stem");
      part_name.push_back("pod");
      part_name.push_back("meal");
      part_name.push_back("oil");
      plant_zero_all_globals();
      parent = P;
    };
  ~Plant() {};
	 
  void doInit(void) ;
  void doRegistrations(void) ;
  void doEvent(unsigned int &id, protocol::Variant &v);
  void getVariable(unsigned id, protocol::QueryValueData& qd) ;
  bool setVariable(unsigned id, protocol::QuerySetValueData& qd) ;
  void doPrepare(unsigned &, protocol::Variant &) ;
  void doProcess(unsigned &, protocol::Variant &) ;
  void doSow(unsigned &, protocol::Variant &v) ;
  void doHarvest(unsigned &, protocol::Variant &v) ;
  void doEndCrop(unsigned &, protocol::Variant &v) ;
  void doKillCrop(unsigned &, protocol::Variant &v) ;
  void doKillStem(unsigned &, protocol::Variant &v) ;
  void doEndRun(unsigned &, protocol::Variant &v) ;
  void doAutoClassChange(unsigned &, protocol::Variant &v) ;
  void doTick(unsigned &id, protocol::Variant &v) ;
  void doNewMet(unsigned &, protocol::Variant &v) ;
  void doNewProfile(unsigned &, protocol::Variant &v) ;
  void plant_bio_actual (int option /* (INPUT) option number*/);
  void plant_bio_grain_demand (int option /* (INPUT) option number */);
  void plant_bio_grain_oil (int option /* (INPUT) option number */);
  void plant_bio_partition (int option /* (INPUT) option number */);
  void plant_bio_retrans (int option /* (INPUT) option number */);
  void plant_water_stress (int option /* (INPUT) option number */);
  void plant_temp_stress (int option/* (INPUT) option number*/);
  void plant_dm_init (float  c_dm_leaf_init                            
		      ,float  c_dm_root_init                            
		      ,float  c_dm_stem_init                            
		      ,float  c_leaf_trans_frac                         
		      ,float  c_stem_trans_frac                         
		      ,float  c_pod_trans_frac                          
		      ,float  g_current_stage                           
		      ,float  *g_days_tot                               
		      ,float  g_plants                                  
		      ,float  *dm_green                                  
		      ,float  *dm_plant_min                              
		      );
  void plant_oxdef_stress (int option /* (INPUT) option number */);
  void plant_bio_water (int option /* (INPUT) option number */);
  void plant_bio_init (int option);
  void plant_bio_grain_demand_stress (int option /* (INPUT) option number */);
  void plant_retrans_init (int option);
  void plant_detachment (int option /* (INPUT) option number */);
  void plant_leaf_detachment (float *leaf_area
			      ,float dlt_slai_detached
			      ,float plants);
  void plant_plant_death (int option /* (INPUT) option number*/);
  void plant_failure_leaf_sen (float g_current_stage                              
			       ,float g_lai                                        
			       ,float g_plants                                     
			       ,float *dlt_plants                                   
			       ) ;
  void plant_failure_phen_delay (float c_swdf_pheno_limit          
				 ,float *g_cswd_pheno               
				 ,float g_current_stage             
				 ,float g_plants                    
				 ,float *dlt_plants                  
				 ) ;
  void plant_death_seedling    (
				int    c_num_weighted_temp      
				,float  *c_x_weighted_temp        
				,float  *c_y_plant_death          
				,int    g_day_of_year            
				,float  *g_soil_temp              
				,int    g_year                   
				,float  *g_days_tot               
				,float  g_plants                 
				,float  *dlt_plants               
				) ;
  void plant_death_drought(float  c_leaf_no_crit              
			   ,float  c_swdf_photo_limit          
			   ,float  c_swdf_photo_rate           
			   ,float  *g_cswd_photo               
			   ,float  *g_leaf_no                  
			   ,float  g_plants                    
			   ,float  g_swdef_photo               
			   ,float  *dlt_plants                  
			   ) ;
  void plant_death_external_action(protocol::Variant &v         
				   ,float g_plants              
				   ,float *dlt_plants           
				   ) ;
  void plant_death_crop_killed(float g_plants                            
			       , char *g_plant_status                     
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
			,string &g_plant_status                             
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
  void plant_n_conc_limits ( float  *c_stage_code_list                  
			     ,float  *g_phase_tt                         
			     ,float  *g_tt_tot                           
			     ,float  c_n_conc_crit_meal                 
			     ,float  c_n_conc_crit_root                 
			     ,float  c_n_conc_max_meal                  
			     ,float  c_n_conc_max_root                  
			     ,float  c_n_conc_min_meal                  
			     ,float  c_n_conc_min_root                  
			     ,float  *c_x_stage_code                     
			     ,float  *c_y_n_conc_crit_leaf               
			     ,float  *c_y_n_conc_crit_pod                
			     ,float  *c_y_n_conc_crit_stem               
			     ,float  *c_y_n_conc_max_leaf                
			     ,float  *c_y_n_conc_max_pod                 
			     ,float  *c_y_n_conc_max_stem                
			     ,float  *c_y_n_conc_min_leaf                
			     ,float  *c_y_n_conc_min_pod                 
			     ,float  *c_y_n_conc_min_stem                
			     ,float  g_current_stage                    
			     ,float  *n_conc_crit                        
			     ,float  *n_conc_max                         
			     ,float  *n_conc_min                         
			     ) ;
  void plant_n_conc_grain_limits(float  c_n_conc_crit_grain             
				 ,float  c_n_conc_max_grain              
				 ,float  c_n_conc_min_grain              
				 ,float  g_current_stage                 
				 ,float  *g_dlt_dm_green_retrans         
				 ,float  *g_dlt_dm_green                 
				 ,float  *g_dm_green                     
				 ,float  *n_conc_crit                    
				 ,float  *n_conc_max                     
				 ,float  *n_conc_min                     
				 ) ;
  void plant_n_init (float  c_n_leaf_init_conc                  
		     ,float  c_n_root_init_conc                  
		     ,float  c_n_stem_init_conc                  
		     ,float  g_current_stage                     
		     ,float  *g_days_tot                         
		     ,float  *g_dm_green                         
		     ,float  *n_green                            
		     ) ;
  void plant_nit_demand_est (int option);
  void plant_height (int   option/*(INPUT) option number*/);
  void plant_width (int   option/*(INPUT) option number*/);
  void plant_phenology_init (int   option/*(INPUT) option number*/);
  void plant_phenology (int   option/*(INPUT) option number*/);
  void plant_sen_bio (int   option/*(INPUT) option number*/);
  void plant_sen_nit (int   option/*(INPUT) option number*/);
  void plant_leaf_death (int   option/*(INPUT) option number*/);
  void plant_leaf_area_sen (int   option/*(INPUT) option number*/);
  void plant_cleanup ();
  void plant_check_leaf_record ();
  void plant_update
    (
     float *c_stage_code_list                      
    ,float *g_phase_tt                             
    ,float *g_tt_tot                               
    ,float  c_n_conc_crit_meal                     
    ,float  c_n_conc_crit_root                     
    ,float  c_n_conc_max_meal                      
    ,float  c_n_conc_max_root                      
    ,float  c_n_conc_min_meal                      
    ,float  c_n_conc_min_root                      
    ,float *c_x_stage_code                         
    ,float *c_y_n_conc_crit_leaf                   
    ,float *c_y_n_conc_crit_pod                    
    ,float *c_y_n_conc_crit_stem                   
    ,float *c_y_n_conc_max_leaf                    
    ,float *c_y_n_conc_max_pod                     
    ,float *c_y_n_conc_max_stem                    
    ,float *c_y_n_conc_min_leaf                    
    ,float *c_y_n_conc_min_pod                     
    ,float *c_y_n_conc_min_stem                    
    ,float  g_row_spacing                          
    ,float  g_skip_row_fac                         
    ,float  g_skip_plant_fac                       
    ,float *c_x_row_spacing
    ,float *c_y_extinct_coef
    ,float *c_y_extinct_coef_dead
    ,int    c_num_row_spacing
    ,float  g_canopy_height                              
    ,float  g_canopy_width                               
    ,float *g_cnd_grain_conc                             
    ,float *g_cnd_photo                                  
    ,float  g_cover_dead                                 
    ,float  g_cover_green                                
    ,float  g_cover_sen                                  
    ,float *g_cswd_expansion                             
    ,float *g_cswd_pheno                                 
    ,float *g_cswd_photo                                 
    ,float  g_current_stage                              
    ,float  g_dlt_canopy_height                                
    ,float  g_dlt_canopy_width                                 
    ,float  g_dlt_dm                                           
    ,float *g_dlt_dm_dead_detached                             
    ,float *g_dlt_dm_detached                                  
    ,float *g_dlt_dm_green                                     
    ,float *g_dlt_dm_green_retrans                             
    ,float *g_dlt_dm_senesced                                  
    ,float  g_dlt_dm_stress_max                                
    ,float  g_dlt_heat_stress_tt                               
    ,float  g_dlt_lai                                          
    ,float  g_dlt_leaf_no                                      
    ,float  g_dlt_node_no                                      
    ,float  g_dlt_leaf_no_dead                                 
    ,float *g_dlt_n_dead_detached                              
    ,float *g_dlt_n_detached                                   
    ,float *g_dlt_n_green                                      
    ,float *g_dlt_n_retrans                                    
    ,float *g_dlt_n_senesced_trans                             
    ,float *g_dlt_n_senesced                                   
    ,float  g_dlt_plants                                       
    ,float  g_dlt_root_depth                                   
    ,float  g_dlt_slai                                         
    ,float  g_dlt_slai_detached                                
    ,float  g_dlt_stage                                        
    ,float  g_dlt_tlai_dead_detached                           
    ,float *g_dm_dead                                          
    ,float *g_dm_green                                         
    ,float *g_dm_plant_top_tot                                 
    ,float *g_dm_senesced                                      
    ,float *g_dm_stress_max                                    
    ,float  g_grain_no                                         
    ,float *g_heat_stress_tt                                   
    ,float  g_lai                                              
    ,float  g_lai_canopy_green                                 
    ,float *g_leaf_area                                        
    ,float *g_leaf_no                                          
    ,float *g_node_no                                          
    ,float *g_leaf_no_dead                                     
    ,float  g_nfact_grain_conc                                 
    ,float  g_nfact_photo                                      
    ,float *g_n_conc_crit                                      
    ,float *g_n_conc_max                                       
    ,float *g_n_conc_min                                       
    ,float *g_n_dead                                           
    ,float *g_n_green                                          
    ,float *g_n_senesced                                       
    ,float *g_plants                                           
    ,float g_previous_stage                                     
    ,float g_root_depth                                         
    ,float g_slai                                               
    ,float g_swdef_expansion                                    
    ,float g_swdef_pheno                                        
    ,float g_swdef_photo                                        
    ,float g_tlai_dead                                          
    ,float *g_root_length_dead                                  
    ,float *g_root_length                                       
    ,float *g_dlt_root_length                                   
    ,float *g_dlt_root_length_senesced
    ,float g_pai
    ,float g_dlt_pai
    ,float c_extinct_coef_pod
    ,float g_cover_pod
    ,int   p_num_canopy_widths
    ) ;
  void plant_check_bounds
    (
     float  g_canopy_height                     
    ,float  g_cover_dead                        
    ,float  g_cover_green                       
    ,float  g_cover_sen                         
    ,float  g_current_stage                     
    ,float *g_days_tot                          
    ,float *g_dlayer                            
    ,float *g_dm_dead                           
    ,float *g_dm_green                          
    ,float *g_dm_senesced                       
    ,float *g_dm_stress_max                     
    ,float  g_grain_no                          
    ,float *g_heat_stress_tt                    
    ,float  g_lai                               
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
    ,float *g_phase_tt                          
    ,float  g_plants                            
    ,float  g_root_depth                        
    ,float  g_slai                              
    ,float  g_tlai_dead                         
    ,float *g_tt_tot                            
    ) ;
  void plant_totals
    (
     float g_current_stage              
    ,float *g_days_tot                   
    ,int   g_day_of_year                
    ,float *g_dlayer                     
    ,float *g_dlt_n_retrans              
    ,float *g_dlt_sw_dep                 
    ,float *g_dm_green                   
    ,int   *g_flowering_date             
    ,int   *g_flowering_das              
    ,float *g_lai                        
    ,float *g_lai_max                    
    ,int   *g_maturity_date              
    ,int   *g_maturity_das               
    ,float  *g_n_conc_act_stover_tot           
    ,float  *g_n_conc_crit                     
    ,float  *g_n_conc_crit_stover_tot          
    ,float  *g_n_dead                          
    ,float  *g_n_demand                        
    ,float  *g_n_demand_tot                    
    ,float  *g_n_green                         
    ,float  *g_n_senesced                      
    ,float  *g_n_uptake_grain_tot              
    ,float  *g_n_uptake_stover_tot             
    ,float  *g_n_uptake_tot                    
    ,float  *g_dlt_n_green                     
    ,float  *g_n_fix_uptake                    
    ,float  *g_n_fixed_tops                    
    ,float  *g_root_depth                      
    ,float  *g_transpiration_tot               
    )  ;
  void plant_event
    (float  g_current_stage               
    ,float *g_days_tot                    
    ,float *g_dlayer                      
    ,float *g_dm_dead                     
    ,float *g_dm_green                    
    ,float *g_dm_senesced                 
    ,float  g_lai                         
    ,float *g_n_green                     
    ,float  g_root_depth                  
    ,float *g_sw_dep                      
    ,float *p_ll_dep                      
    ) ;
  void plant_root_incorp (
     float  dlt_dm_root                  
    ,float  dlt_n_root                   
    ,float  *root_length)               ;
  float plant_stage_code (float  *c_stage_code_list  
			  ,float  *g_phase_tt         
			  ,float  *g_tt_tot           
			  ,float  stage_no            
			  ,float  *stage_table        
			  ,int    numvals)           ;
  void plant_root_depth (int option /* (INPUT) option number*/);
  void plant_water_supply (int option /* (INPUT) option number*/);
  void plant_water_demand (int option /* (INPUT) option number*/);
  void plant_water_uptake (int option /*(INPUT) option number*/);
  void plant_light_supply (int option /*(INPUT) option number*/);
  void plant_bio_rue (int option /*(INPUT) option number*/);
  void plant_transpiration_eff (int option /*(INPUT) option number*/);
  void plant_sen_root_length (int option /*(INPUT) option number*/);
  void plant_root_depth_init (int option /*(INPUT) option number*/);
  void plant_root_length_growth (int option /*(INPUT) option number*/);
  void plant_root_length_init (int option /*(INPUT) option number*/);
  void legnew_extinct_coef 
    (
     float g_row_spacing
    ,float *c_x_row_spacing
    ,float *c_y_extinct_coef
    ,int   c_num_row_spacing
    ,float *extinct_coef      
    ) ;
  void legnew_cover (
    float g_row_spacing
    ,float *c_x_row_spacing
    ,float *c_y_extinct_coef
    ,int   c_num_row_spacing
    ,float canopy_fac                        
    ,float g_lai
    ,float *g_cover_green                    
    ) ;
  void legnew_cover_leaf_pod (
     float g_row_spacing
    ,float *c_x_row_spacing
    ,float *c_y_extinct_coef
    ,int   c_num_row_spacing
    ,float c_extinct_coef_pod
    ,float  canopy_fac
    ,float g_lai
    ,float g_pai
    ,float lai_canopy                             
    ,float *g_cover_green  
    ,float *g_cover_pod    
    ) ;
  void legnew_n_partition
    (float  *g_dlayer            
    ,float  *g_dlt_no3gsm        
    ,float  *g_n_demand          
    ,float  g_n_fix_pot         
    ,float  *g_n_max             
    ,float  g_root_depth        
    ,float  *dlt_n_green         
    ,float  *n_fix_uptake        
    ) ;
  void legnew_bio_grain_oil (
     float  c_grain_oil_conc          
    ,float  c_carbo_oil_conv_ratio    
    ,float  grain_energy              
    );
  void legnew_bio_yieldpart_demand1
    (float g_current_stage                     
    ,float c_twilight                          
    ,int   g_day_of_year                       
    ,float g_latitude                          
    ,int   start_stress_stage                  
    ,int   start_grainfill_stage               
    ,int   end_grainfill_stage                 
    ,int  *yield_parts                         
    ,int   num_yield_parts                     
    ,int   root_part                           
    ,int   max_part                            
    ,float g_dlt_dm                            
    ,float *g_dm_green                         
    ,float *g_dm_senesced                      
    ,float *g_days_tot                         
    ,float *g_dm_stress_max                    
    ,float *p_x_pp_hi_incr                     
    ,float *p_y_hi_incr                        
    ,int   p_num_pp_hi_incr                    
    ,float *p_x_hi_max_pot_stress              
    ,float *p_y_hi_max_pot                     
    ,int   p_num_hi_max_pot                    
    ,float g_grain_energy                      
    ,float dlt_dm_yieldpart_demand             
    ) ;
  void legnew_dm_partition1
    (
     float  c_frac_leaf                   
    ,float  c_frac_pod                    
    ,float  g_grain_energy                
    ,float  c_grain_oil_conc              
    ,float  c_ratio_root_shoot            
    ,float  c_sla_min                     
    ,float  g_dlt_dm                      
    ,float  g_dlt_dm_grain_demand         
    ,float  g_dlt_lai_stressed            
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
    ,float  g_dlt_dm                            
    ,float  g_dlt_dm_grain_demand               
    ,float  g_dlt_lai_stressed                  
    ,float  dlt_dm_oil_conv                     
    ,float  *dlt_dm_green                       
    ) ;
  void legnew_dm_retranslocate1
    (
     float  c_frac_pod                    
    ,float  g_grain_energy                
    ,float  c_grain_oil_conc              
    ,int    pod                           
    ,int    meal                          
    ,int    oil                           
    ,int    max_part                      
    ,int    *supply_pools                 
    ,int    num_supply_pools              
    ,float  g_dlt_dm_grain_demand         
    ,float  g_dlt_dm_oil_conv             
    ,float  *g_dlt_dm_green               
    ,float  *g_dm_green                   
    ,float  *g_dm_plant_min               
    ,float  g_plants                      
    ,float  dm_oil_conv_retranslocate     
    ,float  *dm_retranslocate             
    ) ;
  void legnew_dm_retranslocate2
    (
     float  g_current_stage
    ,float  *c_x_stage_no_partition
    ,float  *c_y_frac_pod
    ,int    c_num_stage_no_partition
    ,float  g_grain_energy               
    ,float  c_grain_oil_conc             
    ,int    pod                          
    ,int    meal                         
    ,int    oil                          
    ,int    max_part                     
    ,int    *supply_pools                
    ,int    num_supply_pools             
    ,float  g_dlt_dm_grain_demand        
    ,float  g_dlt_dm_oil_conv            
    ,float  *g_dlt_dm_green              
    ,float  *g_dm_green                  
    ,float  *g_dm_plant_min              
    ,float  g_plants                     
    ,float  dm_oil_conv_retranslocate    
    ,float  *dm_retranslocate            
    ) ;
  void legnew_phenology_init(
     float  c_shoot_lag                  
    ,float  c_shoot_rate                 
    ,float  g_maxt
    ,float  g_mint
    ,float  *c_x_vernal_temp
    ,float  *c_y_vernal_days
    ,int    c_num_vernal_temp
    ,float  g_cum_vernal_days
    ,float  *p_cum_vernal_days
    ,float  *p_tt_emerg_to_endjuv               
    ,int    p_num_cum_vernal_days               
    ,float  c_twilight                     
    ,float  g_current_stage                
    ,float  *g_days_tot                    
    ,int    g_day_of_year                  
    ,int    g_year                         
    ,float  g_latitude                     
    ,float  g_sowing_depth                 
    ,float  *p_x_pp_endjuv_to_init         
    ,float  *p_y_tt_endjuv_to_init         
    ,int    p_num_pp_endjuv_to_init        
    ,float  *p_x_pp_init_to_flower         
    ,float  *p_y_tt_init_to_flower         
    ,int    p_num_pp_init_to_flower        
    ,float  *p_x_pp_flower_to_start_grain  
    ,float  *p_y_tt_flower_to_start_grain  
    ,int    p_num_pp_flower_to_start_grain 
    ,float  *p_x_pp_start_to_end_grain     
    ,float  *p_y_tt_start_to_end_grain     
    ,int    p_num_pp_start_to_end_grain    
    ,float  p_tt_end_grain_to_maturity    
    ,float  p_tt_maturity_to_ripe         
    ,int    p_est_days_emerg_to_init       
    ,float  *phase_tt                      
    ) ;
  void legnew_n_retranslocate
    (
     float  c_sfac_slope                
    ,float  c_sw_fac_max                
    ,float  c_temp_fac_min              
    ,float  c_tfac_slope                
    ,float  g_maxt                      
    ,float  g_mint                      
    ,float  g_nfact_grain_conc          
    ,float  *g_n_conc_crit              
    ,float  g_swdef_expansion           
    ,float  *g_n_conc_min               
    ,float  *g_dlt_dm_green             
    ,float  *g_dlt_dm_green_retrans     
    ,float  *g_dm_green                 
    ,float  *g_n_conc_max               
    ,float  *g_n_green                  
    ,float  *dlt_n_retrans              
    ) ;
  void legnew_leaf_death_leg
    (
     float  c_sen_start_stage          
    ,float  c_fr_lf_sen_rate           
    ,float  c_node_sen_rate            
    ,float  g_nfact_expansion                                                                                  
    ,float  g_current_stage            
    ,float  g_dlt_tt                   
    ,float  *g_leaf_no                 
    ,float  *g_leaf_no_dead            
    ,float  *g_leaf_area               
    ,float  c_min_tpla                 
    ,float  *dlt_leaf_no_dead           
    ) ;
  void legnew_dm_pot_rue (
     float  current_stage
    ,float  *g_tt_tot
    ,float  *c_stage_code_list
    ,float  *c_x_stage_code_list_rue
    ,float  *g_phase_tt
    ,float  *c_y_rue
    ,float  rue_pod
    ,float  cover_green
    ,float  cover_pod
    ,float  radn_int
    ,float  stress_factor
    ,float  *dlt_dm_pot                    
    ) ;
  float legnew_node_no_from_area
    (
     float *g_leaf_area         
    ,int   num_nodes            
    ,float pla                  
    ) ;
  float legnew_leaf_no_from_area (
     float  *g_leaf_area                
    ,float  *g_leaf_no                                                              
    ,int    num_nodes                   
    ,float  pla                         
    ) ;
  void legnew_retrans_init
    (
     float c_leaf_trans_frac                      
    ,float c_stem_trans_frac                      
    ,float  c_pod_trans_frac                      
    ,float  g_current_stage                       
    ,float *g_days_tot                            
    ,float  g_plants                              
    ,float *dm_green                              
    ,float *dm_plant_min                          
    ) ;
  float legnew_vernal_days(float  g_maxt
                               ,float  g_mint
                               ,float  *c_x_vernal_temp
                               ,float  *c_y_vernal_days
                               ,int    c_num_vernal_temp) ;
  void legnew_n_senescence1 
    (
     int    num_part               
    ,float  *c_n_sen_conc          
    ,float  *g_dlt_dm_senesced     
    ,float  *g_n_green             
    ,float  *g_dm_green            
    ,float  *dlt_n_senesced_trans  
    ,float  *dlt_n_senesced        
    ) ;
  void legnew_canopy_fac (
     float  g_row_spacing
    ,float  g_plants
    ,float  g_skip_row_fac              
    ,float  g_skip_plant_fac            
    ,float  g_canopy_width
    ,float  *g_canopy_fac
    ) ;
  void plant_canopy_width
    (
     float  g_canopy_width                   
    ,float  *p_x_stem_wt                                                                 
    ,float  *p_y_width                                                                   
    ,int    p_num_stem_wt                                                                
    ,float  *g_dm_green                      
    ,float  g_plants                         
    ,int    stem                             
    ,float  *dlt_canopy_width                 
    ) ;
  void cproc_leaf_no_pot3
    (
     float  *c_x_node_no_app                  
    ,float  *c_y_node_app_rate                
    ,int    c_num_node_no_app                 
    ,float  *c_x_node_no_leaf                 
    ,float  *c_y_leaves_per_node              
    ,int    c_num_node_no_leaf                
    ,float  g_current_stage                   
    ,int    start_node_app                    
    ,int    end_node_app                      
    ,int    emerg                             
    ,float  *g_days_tot                       
    ,float  g_dlt_tt                          
    ,float  *g_node_no                        
    ,float  g_nfact_expansion                                                                               
    ,float  g_swdef_expansion                                                                               
    ,float  *g_leaves_per_node                 
    ,float  *dlt_leaf_no_pot                   
    ,float  *dlt_node_no_pot                   
    ) ;
  void legopt_leaf_area_sen1
    (
     int    emergence                        
    ,int    this_stage                       
    ,float  g_dlt_lai_stressed               
    ,float  g_dlt_leaf_no                    
    ,float  g_dlt_leaf_no_dead               
    ,float  g_lai                            
    ,float  *g_leaf_area                     
    ,float  *g_leaf_no                                                                            
    ,float  *g_leaf_no_dead                  
    ,int    max_node                                                                              
    ,float  g_plants                         
    ,float  g_slai                           
    ,float  c_min_tpla                       
    ,float  *g_dlt_slai_age                   
    ,float  c_lai_sen_light                  
    ,float  c_sen_light_slope                
    ,float  *g_dlt_slai_light                 
    ,float  c_sen_rate_water                 
    ,float  g_swdef_photo                    
    ,float  *g_dlt_slai_water                 
    ,float  *c_x_temp_senescence             
    ,float  *c_y_senescence_fac              
    ,int    c_num_temp_senescence            
    ,float  g_mint                           
    ,float  *g_dlt_slai_frost                 
    ,float  *g_dlt_slai                       
    ) ;
  void legopt_leaf_area_init1
    (
     float  c_initial_tpla              
    ,float  c_leaf_no_at_emerg          
    ,int    init_stage                  
    ,float  g_current_stage             
    ,float  *g_days_tot                 
    ,float  g_plants                    
    ,float  *lai                        
    ,float  *leaf_area                  
    ) ;
  void legopt_leaf_no_init1
    (
     float  c_leaf_no_at_emerg          
    ,float  g_current_stage             
    ,int    emerg                       
    ,float  *g_days_tot                 
    ,float  *leaf_no                    
    ,float  *node_no                    
    ) ;
  void legopt_leaf_area_sen_age1
    (
     int    emergence                           
    ,int    this_stage                          
    ,float  *g_leaf_no
    ,float  *g_leaf_no_dead                     
    ,float  g_dlt_leaf_no_dead                  
    ,int    max_node
    ,float  g_lai                               
    ,float  g_slai                              
    ,float  c_min_tpla                          
    ,float  *g_leaf_area                        
    ,float  g_plants                            
    ,float  *dlt_slai_age                        
    );
  void legopt_root_depth1 
    (
     float *g_dlayer                     
    ,int   c_num_sw_ratio                
    ,float *c_x_sw_ratio                 
    ,float *c_y_sw_fac_root              
    ,float *g_dul_dep                    
    ,float *g_sw_dep                     
    ,float *p_ll_dep                     
    ,float *c_root_depth_rate            
    ,float g_current_stage               
    ,float g_mint                                                                       
    ,float g_maxt                                                                       
    ,float *c_x_temp_root_advance                                                       
    ,float *c_y_rel_root_advance                                                        
    ,int   c_num_temp_root_advance                                                      
    ,float *p_xf                         
    ,float g_dlt_root_depth              
    ,float g_root_depth                  
    ) ;
  void legopt_root_depth_increase
    (
     float  c_root_depth_rate[]                 
    ,float  g_current_stage                    
    ,float  *g_dlayer                          
    ,float  g_root_depth                       
    ,float  g_sw_avail_fac_deepest_layer       
    ,float  *p_xf                              
    ,float  temp_factor
    ,float  *dlt_root_depth                     
    ) ;
  void cproc_n_uptake3
    (
     float  *g_dlayer                            
    ,int    max_layer                            
    ,float  *g_no3gsm_uptake_pot                 
    ,float  g_n_fix_pot                          
    ,const char   *c_n_supply_preference               
    ,float  *g_n_demand                          
    ,float  *g_n_max                             
    ,int    max_part                             
    ,float  g_root_depth                         
    ,float  *dlt_no3gsm                          
    ) ;
  void cproc_n_supply3 (
     float  g_dlayer[]                        
    ,int    max_layer                        
    ,float  g_no3gsm[]                        
    ,float  g_no3gsm_min[]                    
    ,float  *g_no3gsm_uptake_pot
    ,float  g_root_depth                     
    ,float  *g_root_length
    ,float  g_bd[]
    ,float  c_no3_uptake_max
    ,float  c_no3_conc_half_max
    ,float  *g_sw_avail_pot                  
    ,float  *g_sw_avail                      
    ,float  g_current_stage                  
    ,float  *c_n_fix_rate                    
    ,float  fixation_determinant             
    ,float  g_swdef_fixation                 
    ,float  g_n_fix_pot                      
    ) ;
  void plant_grain_number (int option /*(INPUT) option number*/);
  void crop_grain_number (
     float g_current_stage
    ,float *g_days_tot
    ,int   emerg
    ,int   start_grain_fill
    ,float *dm_green
    ,int   stem
    ,float p_grains_per_gram_stem
    ,float *g_grain_no    
    ) ;
  void legnew_bio_yieldpart_demand2(
     float g_current_stage
    ,int   start_grain_fill
    ,int   end_grain_fill
    ,float g_grain_no
    ,float p_potential_grain_filling_rate
    ,float g_maxt
    ,float g_mint
    ,float *c_x_temp_grainfill
    ,float *c_y_rel_grainfill
    ,int   c_num_temp_grainfill
    ,float *g_dlt_dm_grain_demand
    ) ;
  void cproc_phenology_nw (
     float g_previous_stage
    ,float g_current_stage
    ,int   sowing_stage
    ,int   germ_stage
    ,int   end_development_stage
    ,int   start_stress_stage
    ,int   end_stress_stage
    ,int   max_stage
    ,int   c_num_temp
    ,float *c_x_temp
    ,float *c_y_tt
    ,float g_maxt
    ,float g_mint
    ,float g_nfact_pheno
    ,float g_swdef_pheno
    ,float g_vern_eff
    ,float g_photop_eff
    ,float c_pesw_germ
    ,float *c_fasw_emerg            
    ,float *c_rel_emerg_rate        
    ,int   c_num_fasw_emerg         
    ,float *g_dlayer
    ,int   max_layer
    ,float g_sowing_depth
    ,float *g_sw_dep
    ,float *g_dul_dep
    ,float *p_ll_dep
    ,float g_dlt_tt
    ,float *g_phase_tt
    ,float g_phase_devel
    ,float g_dlt_stage
    ,float *g_tt_tot
    ,float *g_days_tot
    ) ;
  void crop_crown_temp_nwheat
   ( float tempmx        
    ,float tempmn        
    ,float snow          
    ,float *tempcx        
    ,float *tempcn        
    ) ;
  void wheat_vernaliz_days_nwheat
   ( float g_current_stage     
    ,int   start_stage         
    ,int   end_stage           
    ,float g_maxt              
    ,float g_mint              
    ,float g_snow              
    ,float *g_dlt_cumvd         
    ,float g_cumvd             
   )                           ;
  void wheat_vernaliz_effect_nwheat
   (
     float current_stage
    ,int   start_stage
    ,int   end_stage
    ,float p_vern_sens
    ,float cumvd
    ,float dlt_cumvd
    ,float reqvd
    ,float *vern_effect        
    ) ;
  void wheat_phenology_init_nwheat
    (
     float  c_shoot_lag           
    ,float  c_shoot_rate          
    ,float  g_current_stage       
    ,float  *g_days_tot           
    ,float  g_sowing_depth        
    ,float  *phase_tt             
    ,float  p_startgf_to_mat
    ,float  p_phyllochron 
    ) ;
  void wheat_photoperiod_effect(
     float current_stage
    ,int   start_stage
    ,int   end_stage
    ,float photoperiod
    ,float p_photop_sen
    ,float *photop_eff
    ) ;
  void plant_process ( void );
  void plant_dead (void);
  void plant_harvest (protocol::Variant &v/*(INPUT) message variant*/);
  void plant_kill_stem (protocol::Variant &v/*(INPUT) incoming message variant*/);
  void plant_dormancy (protocol::Variant &v/*(INPUT) incoming message variant*/);
  void plant_harvest_update (protocol::Variant &v/*(INPUT)message arguments*/);
  void plant_kill_stem_update (protocol::Variant &v/*(INPUT) message arguments*/);
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
  bool  plant_my_type (protocol::Variant &v /* (INPUT) incoming message variant*/) ;
  void plant_read_constants ( void );
  void plant_prepare (void);
  void plant_read_species_const ();
  void plant_harvest_report ();
  bool  plant_auto_class_change (char *action);
  void plant_send_crop_chopped_event (
    const string&  crop_type                      
    ,vector<string>& dm_type                        
    ,float * dlt_crop_dm                    
    ,float * dlt_dm_n                       
    ,float * fraction_to_residue            
    ,int     max_part                       
    ) ;
  void plant_onnew_profile ( protocol::Variant &v /* (INPUT) message arguments*/);
  void plant_get_site_characteristics ();
  bool set_plant_crop_class(protocol::QuerySetValueData&v);
  bool set_plant_grain_oil_conc(protocol::QuerySetValueData&v);
  void get_plant_status(protocol::QueryValueData &qd);
  void get_dlt_stage(protocol::QueryValueData &qd);
  void get_stage(protocol::QueryValueData &qd);
  void get_stage_code(protocol::QueryValueData &qd);
  void get_stage_name(protocol::QueryValueData &qd);
  void get_crop_type(protocol::QueryValueData &qd);
  void get_crop_class(protocol::QueryValueData &qd);
  void get_dlt_tt(protocol::QueryValueData &qd);
  void get_phase_tt(protocol::QueryValueData &qd);
  void get_tt_tot(protocol::QueryValueData &qd);
  void get_days_tot(protocol::QueryValueData &qd);
  void get_das(protocol::QueryValueData &qd);
  void get_cum_vernal_days(protocol::QueryValueData &qd);
  void get_flowering_date(protocol::QueryValueData &qd);
  void get_maturity_date(protocol::QueryValueData &qd);
  void get_flowering_das(protocol::QueryValueData &qd);
  void get_maturity_das(protocol::QueryValueData &qd);
  void get_leaf_no(protocol::QueryValueData &qd);
  void get_node_no(protocol::QueryValueData &qd);
  void get_dlt_leaf_no(protocol::QueryValueData &qd);
  void get_dlt_node_no(protocol::QueryValueData &qd);
  void get_leaf_no_dead(protocol::QueryValueData &qd);
  void get_leaf_area(protocol::QueryValueData &qd);
  void get_height(protocol::QueryValueData &qd);
  void get_width(protocol::QueryValueData &qd);
  void get_root_depth(protocol::QueryValueData &qd);
  void get_plants(protocol::QueryValueData &qd);
  void get_cover_green(protocol::QueryValueData &qd);
  void get_cover_tot(protocol::QueryValueData &qd);
  void get_lai_sum(protocol::QueryValueData &qd);
  void get_tlai(protocol::QueryValueData &qd);
  void get_slai(protocol::QueryValueData &qd);
  void get_lai(protocol::QueryValueData &qd);
  void get_lai_canopy_green(protocol::QueryValueData &qd);
  void get_tlai_dead(protocol::QueryValueData &qd);
  void get_pai(protocol::QueryValueData &qd);
  void get_grain_no(protocol::QueryValueData &qd);
  void get_root_wt(protocol::QueryValueData &qd);
  void get_leaf_wt(protocol::QueryValueData &qd);
  void get_stem_wt(protocol::QueryValueData &qd);
  void get_pod_wt(protocol::QueryValueData &qd);
  void get_grain_wt(protocol::QueryValueData &qd);
  void get_meal_wt(protocol::QueryValueData &qd);
  void get_oil_wt(protocol::QueryValueData &qd);
  void get_dm_green(protocol::QueryValueData &qd);
  void get_dm_senesced(protocol::QueryValueData &qd);
  void get_dm_dead(protocol::QueryValueData &qd);
  void get_yield(protocol::QueryValueData &qd);
  void get_biomass(protocol::QueryValueData &qd);
  void get_green_biomass(protocol::QueryValueData &qd);
  void get_biomass_wt(protocol::QueryValueData &qd);
  void get_green_biomass_wt(protocol::QueryValueData &qd);
  void get_dlt_dm(protocol::QueryValueData &qd);
  void get_dlt_dm_pot_rue(protocol::QueryValueData &qd);
  void get_dlt_dm_pot_te(protocol::QueryValueData &qd);
  void get_dlt_dm_grain_demand(protocol::QueryValueData &qd);
  void get_dlt_dm_green(protocol::QueryValueData &qd);
  void get_dlt_dm_green_retrans(protocol::QueryValueData &qd);
  void get_dlt_dm_detached(protocol::QueryValueData &qd);
  void get_dlt_dm_senesced(protocol::QueryValueData &qd);
  void get_dlt_dm_dead_detached(protocol::QueryValueData &qd);
  void get_grain_oil_conc(protocol::QueryValueData &qd);
  void get_dlt_dm_oil_conv(protocol::QueryValueData &qd);
  void get_dlt_dm_oil_conv_retrans(protocol::QueryValueData &qd);
  void get_biomass_n(protocol::QueryValueData &qd);
  void get_n_uptake(protocol::QueryValueData &qd);
  void get_green_biomass_n(protocol::QueryValueData &qd);
  void get_grain_n(protocol::QueryValueData &qd);
  void get_leaf_n(protocol::QueryValueData &qd);
  void get_stem_n(protocol::QueryValueData &qd);
  void get_root_n(protocol::QueryValueData &qd);
  void get_pod_n(protocol::QueryValueData &qd);
  void get_n_senesced(protocol::QueryValueData &qd);
  void get_n_dead(protocol::QueryValueData &qd);
  void get_dlt_n_green(protocol::QueryValueData &qd);
  void get_dlt_n_retrans(protocol::QueryValueData &qd);
  void get_dlt_n_detached(protocol::QueryValueData &qd);
  void get_dlt_n_dead_detached(protocol::QueryValueData &qd);
  void get_temp_stress_photo(protocol::QueryValueData &qd);
  void get_swdef_pheno(protocol::QueryValueData &qd);
  void get_swdef_photo(protocol::QueryValueData &qd);
  void get_swdef_expan(protocol::QueryValueData &qd);
  void get_swdef_fixation(protocol::QueryValueData &qd);
  void get_oxdef_photo(protocol::QueryValueData &qd);
  void get_transp_eff(protocol::QueryValueData &qd);
  void get_ep(protocol::QueryValueData &qd);
  void get_cep(protocol::QueryValueData &qd);
  void get_sw_supply(protocol::QueryValueData &qd);
  void get_esw_layr(protocol::QueryValueData &qd);
  void get_n_conc_stover(protocol::QueryValueData &qd);
  void get_n_conc_leaf(protocol::QueryValueData &qd);
  void get_n_conc_stem(protocol::QueryValueData &qd);
  void get_n_conc_grain(protocol::QueryValueData &qd);
  void get_n_conc_meal(protocol::QueryValueData &qd);
  void get_n_conc_crit(protocol::QueryValueData &qd);
  void get_n_conc_min(protocol::QueryValueData &qd);
  void get_n_uptake_stover(protocol::QueryValueData &qd);
  void get_no3_tot(protocol::QueryValueData &qd);
  void get_n_demand(protocol::QueryValueData &qd);
  void get_n_supply_soil(protocol::QueryValueData &qd);
  void get_dlt_n_fixed_pot(protocol::QueryValueData &qd);
  void get_dlt_n_fixed(protocol::QueryValueData &qd);
  void get_n_fixed_tops(protocol::QueryValueData &qd);
  void get_nfact_photo(protocol::QueryValueData &qd);
  void get_nfact_expan(protocol::QueryValueData &qd);
  void get_nfact_grain(protocol::QueryValueData &qd);
  void get_nfact_grain_tot(protocol::QueryValueData &qd);
  void get_rlv(protocol::QueryValueData &qd);
  void get_no3_demand(protocol::QueryValueData &qd);
  void get_sw_demand(protocol::QueryValueData &qd);
  void get_sw_demand_te(protocol::QueryValueData &qd);
  void get_root_length(protocol::QueryValueData &qd);
  void get_root_length_dead(protocol::QueryValueData &qd);
  void get_no3gsm_uptake_pot(protocol::QueryValueData &qd);
  void get_no3_swfac(protocol::QueryValueData &qd);
  void get_leaves_per_node(protocol::QueryValueData &qd);


 private:
  /* system interface: */
  UInt2EventFnMap IDtoEventFn;  /* events */
  UInt2GetFnMap   IDtoGetFn;    /* getVariable */
  UInt2SetFnMap   IDtoSetFn;    /* setVariable */

  //     ================================================================
  //     Plant
  //     ================================================================
  struct IDS {
       // gets
       int eo;
       int fr_intc_radn;
       int sw_dep;
       int no3;
       int no3_min;
       int latitude;

       // sets
       int dlt_no3;
       int dlt_sw_dep;

       // events.
       int crop_chopped;
       
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
      string  module_name;                             // module name
      string  crop_class;                                // crop type
      string  plant_status;                              // status of crop
      string  cultivar;                                // name of cultivar
      string  pre_dormancy_crop_class;
      float swdef_expansion;
      float swdef_photo;
      float swdef_pheno;
      float swdef_fixation;
      float sw_avail_fac_deepest_layer;
      float nfact_expansion;
      float nfact_photo;
      float nfact_grain_conc;
      float nfact_pheno;
      float temp_stress_photo;
      float oxdef_photo;
      float row_spacing;                                // row spacing (m) [optional]
      float skip_row;                                   // skip row (0, 1, 2)
      float skip_plant;                                 // skip plant (0, 1, 2)
      float skip_row_fac;                               // skip row factor
      float skip_plant_fac;                             // skip plant factor
      float sowing_depth;                               // sowing depth (mm)
      int   year;                                       // year
      int   day_of_year;                                // day of year
      float fr_intc_radn;                               // fraction of radiation intercepted by canopy
      float latitude;                                   // latitude (degrees, negative for southern hemisphere)
      float radn;                                       // solar radiation (Mj/m^2/day)
      float mint;                                       // minimum air temperature (oC)
      float maxt;                                       // maximum air temperature (oC)
      float soil_temp[366];                             // soil surface temperature (oC)
      float eo;                                         // potential evapotranspiration (mm)
      float cnd_photo [max_stage];                      // cumulative nitrogen stress type 1
      float cnd_grain_conc [max_stage];                 // cumulative nitrogen stress type 2
      float cswd_photo [max_stage];                     // cumulative water stress type 1
      float cswd_expansion [max_stage];                 // cumulative water stress type 2
      float cswd_pheno [max_stage];                     // cumulative water stress type 3
      float cum_vernal_days;                            // Accumulated Vernal Days
      float dlt_tt;                                     // daily thermal time (growing deg day)
      float tt_tot[max_stage];                          // the sum of growing degree days for a phenological stage (oC d)
      float phase_tt[max_stage];                        // Cumulative growing degree days required for each stage (deg days)
      float dlt_tt_curv;                                // daily thermal time (growing deg day)
      float tt_curv_tot[max_stage];                     // the sum of growing degree days for a phenological stage (oC d)
      float phase_tt_curv[max_stage];                   // Cumulative growing degree days required for each stage (deg days)
      float dlt_tt_other;                               // daily thermal time (growing deg day)
      float tt_other_tot[max_stage];                    // the sum of growing degree days for a phenological stage (oC d)
      float phase_tt_other[max_stage];                  // Cumulative growing degree days required for each stage (deg days)
      float heat_stress_tt[max_stage];                  // heat stress cumulation in each phase
      float dlt_heat_stress_tt;                         // change in heat stress accumulation
      float dlt_stage;                                  // change in stage number
      float current_stage;                              // current phenological stage
      float previous_stage;                             // previous phenological stage
      float days_tot [max_stage];                       // duration of each phase (days)
      float dlt_canopy_height;                          // change in canopy height (mm)
      float canopy_height;                              // canopy height (mm)
      float dlt_canopy_width;                           // change in canopy height (mm)
      float canopy_width;                              // canopy height (mm)
      float phase_devel;                               // development of current phase ()
      float plants;                                    // Plant density (plants/m^2)
      float dlt_plants;                                 // change in Plant density (plants/m^2)
      float grain_no;                                   // grain number (grains/plant)
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
      float dlt_dm_oil_conv;                            // plant biomass used in conversion to oil (g/m^2)
      float dlt_dm_green[max_part];                     // plant biomass growth (g/m^2)
      float dlt_dm_senesced[max_part];                  // plant biomass senescence (g/m^2)
      float dlt_dm_detached[max_part];                  // plant biomass detached (g/m^2)
      float dlt_dm_dead[max_part];                      // plant biomass dead (g/m^2)
      float dlt_dm_dead_detached[max_part];             // plant biomass detached from dead plant (g/m^2)
      float dlt_dm_oil_conv_retranslocate;              // retranslocated plant biomass used in conversion to oil for (g/m^2)
      float dlt_dm_green_retrans[max_part];             // plant biomass retranslocated (g/m^2)
      float dm_stress_max[max_stage];                   // sum of maximum daily stress on dm production per phase
      float dlt_dm_stress_max;                          // maximum daily stress on dm production (0-1)
      float dlt_dm_grain_demand;                        // grain dm demand (g/m^2)
      float dm_green_demand[max_part];                  // biomass demand of the plant parts (g/m^2)
      float dm_dead[max_part];                          // dry wt of dead plants (g/m^2)
      float dm_green[max_part];                         // live plant dry weight (biomass) (g/m^2)
      float dm_senesced[max_part];                      // senesced plant dry wt (g/m^2)
      float dm_plant_top_tot[max_stage];                // total carbohydrate production in tops per stage (g/plant)
      float radn_int;                                   // radn intercepted by leaves (mj/m^2)
      float transp_eff;                                 // transpiration efficiency (g dm/m^2/mm water)
      float slai;                                       // area of leaf that senesces from plant
      float dlt_slai;                                   // area of leaf that senesces from plant
      float dlt_lai;                                    // actual change in live plant lai
      float dlt_lai_pot;                                // potential change in live plant lai
      float dlt_lai_stressed;                           // potential change in lai  allowing for stress
      float lai;                                        // live plant green lai
      float lai_canopy_green;                           // green lai of canopy
      float tlai_dead;                                  // total lai of dead plants
      float dlt_slai_detached;                          // plant senesced lai detached
      float dlt_tlai_dead;                              // plant lai change in dead plant
      float dlt_tlai_dead_detached;                     // plant lai detached from dead plant
      float dlt_slai_age;                               // senesced lai from age
      float dlt_slai_light;                             // senesced lai from light
      float dlt_slai_water;                             // senesced lai from water
      float dlt_slai_frost;                             // senesced lai from frost
      float pai;
      float dlt_pai;
      float leaf_no[max_node];                          // number of fully expanded leaves ()
      float node_no[max_stage];                         // number of fully expanded nodes ()
      float leaf_no_dead[max_node];                     // no of dead leaves ()
      float dlt_leaf_no;                                // actual fraction of oldest leaf expanding ()
      float dlt_node_no;                                // actual fraction of oldest node expanding ()
      float dlt_leaf_no_pot;                            // potential fraction of oldest leaf expanding ()
      float dlt_node_no_pot;                            // potential fraction of oldest leaf expanding ()
      float dlt_leaf_no_dead;                           // fraction of oldest green leaf senesced ()
      float leaf_no_final;                              // total number of leaves the plant produces
      float leaf_area[max_node];                        // leaf area of each leaf (mm^2)
      float lai_equilib_light[366];                     // lai threshold for light senescence
      float lai_equilib_water[366];                     // lai threshold for water senescence
      float n_demand [max_part];                        // critical plant nitrogen demand (g/m^2)
      float n_max [max_part];                           // maximum plant nitrogen demand (g/m^2)
      float dlt_n_green[max_part];                      // actual N uptake into plant (g/m^2)
      float dlt_n_senesced[max_part];                   // actual N loss with senesced plant (g/m^2)
      float dlt_n_senesced_trans[max_part];
      float dlt_n_detached[max_part];                   // actual N loss with detached plant (g/m^2)
      float dlt_n_dead[max_part];                       // actual N loss with dead plant (g/m^2)
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
      float n_fix_pot;                                  // N fixation potential (g/m^2)
      float no3gsm_uptake_pot[max_layer];
      float n_fix_uptake;                               // N fixation actual (g/m^2)
      float n_fixed_tops;                               // cum. fixed N in tops
      float n_conc_crit[max_part];                      // critical N concentration (g N/g biomass)
      float n_conc_max[max_part];                       // maximum N concentration (g N/g biomass)
      float n_conc_min[max_part];                       // minimum N concentration (g N/g biomass)
      float dm_plant_min[max_part];                     // minimum weight of each plant part (g/plant)
      float cover_pod;
      float dlayer [max_layer];                         // thickness of soil layer I (mm)
      float dlt_sw_dep[max_layer];                      // water uptake in each layer (mm water)
      float ll15_dep[max_layer];
      float dul_dep [max_layer];                        // drained upper limit soil water content for soil layer L (mm water)
      float sat_dep[max_layer];
      float bd[max_layer];
      float sw_dep [max_layer];                         // soil water content of layer L (mm)
      float sw_demand;                                  // total crop demand for water (mm)
      float sw_demand_te;                               // crop demand for water calculated from transpiration efficiency (mm)
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
      float n_uptake_grain_tot;                         // sum of grain N uptake (g N/m^2)
      float n_uptake_stover_tot;                        // sum of tops N uptake (g N/m^2)
      float lai_max;                                    // maximum lai - occurs at flowering
      int   flowering_date;                             // flowering day number
      int   maturity_date;                              // maturity day number
      int   flowering_das;                              // days to flowering
      int   maturity_das;                               // days to maturity
      float root_length[max_layer];                     // root length (mm/mm^2)
      float root_length_dead[max_layer];                // root length of dead population (mm/mm^2)
      float dlt_root_length[max_layer];                 // root length growth (mm/mm^2)
      float dlt_root_length_senesced[max_layer];        // root length senescence (mm/mm^2)
      float ext_n_demand;
      float ext_sw_demand;                              // Note: currently unused - use sw_demand
      float grain_energy;                               // multiplier of grain weight to account
      // for seed energy content
      float dlt_cumvd;
      float cumvd;
      float vern_eff;
      float photop_eff;
      float leaves_per_node;
    } g;   // Globals 


//     ================================================================
//       plant Parameters
//     ================================================================
    struct {
      float grains_per_gram_stem;
      float potential_grain_filling_rate;
      float tt_maturity_to_ripe;                        // growing deg day required to for grain dry down (deg day)
      float tt_end_grain_to_maturity;
      float cum_vernal_days[max_table];
      float tt_emerg_to_endjuv[max_table];              // Growing degree days to complete
                                                        // emerg_to_endjuv stage (emergence to
                                                        // end of emerg_to_endjuv) (deg day)
      int   num_cum_vernal_days;
      
      float tt_flower_to_maturity;                      // Growing degree days to complete
                                                        // grainfill
                                                        // (silking to maturity) (deg day)
      float x_pp_endjuv_to_init[max_table];
      float y_tt_endjuv_to_init[max_table];
      float x_pp_init_to_flower[max_table];
      float y_tt_init_to_flower[max_table];
      float x_pp_flower_to_start_grain[max_table];
      float y_tt_flower_to_start_grain[max_table];
      float x_pp_start_to_end_grain[max_table];
      float y_tt_start_to_end_grain[max_table];
      
      int   num_pp_endjuv_to_init;
      int   num_pp_init_to_flower;
      int   num_pp_flower_to_start_grain;
      int   num_pp_start_to_end_grain;
      
      int   est_days_emerg_to_init;                     // estimated days from emergence to floral initiation
      float x_pp_hi_incr[max_table];
      float y_hi_incr[max_table];                       // harvest index increment per day ()
      int   num_pp_hi_incr;
      int   num_hi_max_pot;
      float x_hi_max_pot_stress[max_table];             // maximum harvest index (g grain/g biomass)
      float y_hi_max_pot[max_table];                    // maximum harvest index (g grain/g biomass)
      float kl[max_layer];                              // root length density factor for water
      float ll_dep[max_layer];                          // lower limit of plant-extractable
                                                        // soil water for soil layer L (mm)
      float x_stem_wt[max_table];
      float y_height [max_table];
      float y_width [max_table];
      int   num_stem_wt;
      int   num_canopy_widths;
      float xf[max_layer];                              // root exploration factor (0-1)
      string  uptake_source;                            // source of uptake information//XXcharacter uptake_source*10
      float eo_crop_factor;                             // Crop factor for sw demand applied to Eo
      
      float startgf_to_mat;
      float phyllochron;
      float vern_sens;
      float photop_sens;
    } p; // Parameters


   //     ================================================================
   //       plant Constants
   //     ================================================================
   struct {
	   int   grain_fill_option;
      int   n_uptake_option;
      int   phenology_option;
      int   leaf_no_pot_option;
      int   partition_option;
      int   grain_no_option;
      
      float sen_start_stage;
      float x_temp_grainfill[max_table];
      float y_rel_grainfill[max_table];
      int   num_temp_grainfill;
      
      float no3_uptake_max;
      float no3_conc_half_max;
      
      string crop_type;                                  // crop type
      string default_crop_class;                      // crop type
      vector<string> stage_names;                        // full names of stages for reporting
      vector<string> part_names;                         // names of plant parts
      string n_supply_preference;                     // preference of n supply
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
      float stage_code_list[max_stage];                 // list of stage numbers
      float twilight;                                   // twilight in angular distance between
                                                        // sunset and end of twilight - altitude
                                                        // of sun. (deg)
      float x_lai_ratio[max_table];                     // ratio table for critical leaf size
                                                        // below which leaf number is reduced ()
      float y_leaf_no_frac[max_table];                  // reduction in leaf appearance ()
      int   num_lai_ratio;                              // number of ratios in table ()
      float n_conc_crit_grain;                          // critical N concentration of grain (g N/g biomass)
      float n_conc_max_grain;                           // maximum N concentration of grain (g N/g biomass)
      float n_conc_min_grain;                           // minimum N concentration of grain (g N/g biomass)
      float n_conc_crit_root;                           // critical N concentration of root (g N/g biomass)
      float n_conc_max_root;                            // maximum N concentration of root (g N/g biomass)
      float n_conc_min_root;                            // minimum N concentration of root (g N/g biomass)
      float x_stage_code[max_stage];                    // stage table for N concentrations (g N/g biomass)
      float y_n_conc_crit_leaf[max_stage];              // critical N concentration of leaf (g N/g biomass)
      float y_n_conc_max_leaf[max_stage];               // maximum N concentration of leaf (g N/g biomass)
      float y_n_conc_min_leaf[max_stage];               // minimum N concentration of leaf (g N/g biomass)
      float y_n_conc_crit_stem[max_stage];              // critical N concentration of stem (g N/g biomass)
      float y_n_conc_max_stem[max_stage];               // maximum N concentration of stem (g N/g biomass)
      float y_n_conc_min_stem[max_stage];               // minimum N concentration of stem (g N/g biomass)
      float y_n_conc_crit_pod[max_stage];               // critical N concentration of pod(g N/g biomass)
      float y_n_conc_max_pod[max_stage];                // maximum N concentration of pod (g N/g biomass)
      float y_n_conc_min_pod[max_stage];                // minimum N concentration of pod (g N/g biomass)
      float n_fact_photo;                               // multipler for N deficit effect on photosynthesis
      float n_fact_pheno;                               // multipler for N deficit effect on phenology
      float n_fact_expansion;
      float n_init_conc[max_part];                      // initial N concentration (gN/gdm)
      float n_sen_conc[max_part];                       // N concentration of senescedmaterial (gN/gdm)
      int   num_n_conc_stage;                           // no of values in stage table
      float x_row_spacing[max_table];
      float y_extinct_coef[max_table];
      float y_extinct_coef_dead[max_table];
      float x_stage_code_list_rue[max_stage];           // list of stage numbers
      float y_rue[max_stage];                           // radiation use efficiency (g dm/mj)
      float stage_stem_reduction_harvest[max_stage];    // phenological stage for stem reduction at harvest
      float stage_stem_reduction_kill_stem[max_stage];  // phenological stage for stem reduction at kill all stem
      float root_depth_rate[max_stage];                 // root growth rate potential (mm depth/day)
      
      float extinct_coef_pod;
      float spec_pod_area;
      float rue_pod;
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
      float transp_eff_cf[max_stage];                   // transpiration efficiency coefficient
                                                        // to convert vpd to
                                                        // transpiration efficiency (kpa)
                                                        // although this is expressed as a
                                                        // pressure it is really in the form
                                                        // kpa*g carbo per m^2 / g water per m^2
                                                        // and this can be converted to
                                                        // kpa*g carbo per m^2 / mm water
                                                        // because 1g water = 1 cm^3 water
      int   num_lai;
      float pesw_germ;                                  // plant extractable soil water in
                                                        // seedling layer inadequate for
                                                        // germination (mm/mm)
      float grain_n_conc_min;                           // minimum nitrogen concentration of grain
      
      float seed_wt_min;                                // minimum grain weight (g/kernel)
      float leaf_no_at_emerg;                           // leaf number at emergence ()
      float fasw_emerg[max_table];
      float rel_emerg_rate[max_table];
      int   num_fasw_emerg;
      float no3_diffn_const;                            // time constant for uptake by
                                                        // diffusion (days). H van Keulen &
                                                        // NG Seligman. Purdoe 1987. This is the
                                                        // time it would take to take up by
                                                        // diffusion the current amount of N if
                                                        // it wasn't depleted between time steps
      float n_fix_rate[max_stage];                      // potential rate of N fixation (g N fixed
                                                        // per g above ground biomass
      float shoot_lag;                                  // minimum growing degree days for
                                                        // germination (deg days)
      float shoot_rate;                                 // growing deg day increase with depth
                                                        // for germination (deg day/mm depth)
      float x_node_no_app[max_table];
      float y_node_app_rate[max_table];
      float x_node_no_leaf[max_table];
      float y_leaves_per_node[max_table];
      float dm_init [max_part];                         // initial dm (g/plant)
      float leaf_init_rate;                             // growing degree days to initiate each le
                                                        // primordium until fl_initling (deg day)
      float leaf_no_seed;                               // number of leaf primordia present in
                                                        // seed
      float x_dm_sen_frac[max_table][max_part];
      float y_dm_sen_frac[max_table][max_part];
      float dead_detach_frac[max_part];                 // fraction of dead plant parts
                                                        // detaching each day (0-1)
      float sen_detach_frac[max_part];                  // fraction of dead plant parts
                                                        // detaching each day (0-1)

      int   num_dm_sen_frac [max_part];
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

      float frac_leaf[max_stage];                       // fraction of remaining dm allocated to leaves
      float frac_pod[max_stage];                        // fraction of dm or grain weight allocated to pod
      float ratio_root_shoot[max_table];                // root:shoot ratio of new dm ()

      float x_stage_no_partition[max_table];
      float y_frac_leaf[max_table];                     // fraction of remaining dm allocated to leaves
      float y_frac_pod[max_table];                      // fraction of dm or grain weight allocated to pod
      float y_ratio_root_shoot[max_table];              // root:shoot ratio of new dm ()

      int   num_stage_no_partition;
      float stem_trans_frac;                            // fraction of stem used in translocat
                                                        // to grain
      float leaf_trans_frac;                            // fraction of leaf used in translocat
                                                        // to grain
      float pod_trans_frac;                             // fraction of pod used in translocat
                                                        // to grain
      float htstress_coeff;                             // coeff for conversion of heat stress
                                                        // during flowering to
                                                        // heat stress factor on grain number
                                                        // development.
      float temp_grain_crit_stress;                     // temperature above which heat stress
                                                        // occurs
      float node_sen_rate;
      float fr_lf_sen_rate;
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
      float y_grain_rate[max_table];                    // Relative grain fill
                                                        // rates for critical temperatures
                                                        // (0-1)
      int   num_temp;                                   // size_of table
      int   num_ave_temp;                               // size_of critical temperature table
      int   num_temp_grain;                             // size_of table
      int   num_factors;                                // size_of table
      int   num_temp_other;                             //
      int   num_weighted_temp;                          // size of table
      float tt_emerg_to_endjuv_ub;                      // upper limit
      float tt_maturity_to_ripe_ub;                     // upper limit
      float kl_ub;                                      // upper limit of water uptake factor
      float sw_dep_ub;                                  // upper limit of soilwater depth (mm)
      float sw_dep_lb;                                  // lower limit of soilwater depth (mm)
      float sw_ub;                                      // upper limit of soilwater depth (mm/mm)
      float sw_lb;                                      // lower limit of soilwater depth (mm/mm)
      float no3_ub;                                     // upper limit of soil NO3 (kg/ha)
      float no3_lb;                                     // lower limit of soil NO3 (kg/ha)
      float no3_min_ub;                                 // upper limit of minimum soil NO3 (kg/ha)
      float no3_min_lb;                                 // lower limit of minimum soil NO3 (kg/ha)
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
      float fr_height_cut [max_table];
      float fr_stem_remain[max_table];
      int   num_fr_height_cut;
      float specific_root_length;                       // as name suggests (mm/g)
      float root_die_back_fr;                           // fraction of roots dying at harvest
      float x_plant_rld [max_table];
      float y_rel_root_rate [max_table];
      int   num_plant_rld;
      vector<string> class_action;
      vector<string> class_change;

      float x_vernal_temp[max_table];
      float y_vernal_days[max_table];
      int   num_vernal_temp;

      float x_temp_root_advance[max_table];
      float y_rel_root_advance[max_table];
      int   num_temp_root_advance;
      
      float eo_crop_factor_default;                     // Default Crop factor for sw demand applied to Eo
    }  c;   // Constants
};  // Plant

#endif //PLANT_H_