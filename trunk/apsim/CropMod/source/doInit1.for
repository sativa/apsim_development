* ====================================================================
      subroutine doInit1()
* ====================================================================
      use CropModData
      Use infrastructure
      implicit none
      ml_external doInit1
      integer dummy

      id%incorp_fom = add_registration(eventReg, 'incorp_fom', 
     :                                 nullTypeDDML, '', '')
      id%add_residue_p = add_registration(eventReg, 'add_residue_p', 
     :                                    nullTypeDDML, '', '')
      id%crop_chopped = add_registration(eventReg, 'crop_chopped', 
     :                                   crop_choppedTypeDDML, '', '')
      id%sowing = add_registration(eventReg, 'sowing', 
     :                             nullTypeDDML, '', '')
      id%harvesting = add_registration(eventReg, 'harvesting', 
     :                                 nullTypeDDML, '', '')
      id%create = add_registration(respondToEventReg, 'create', 
     :                             nullTypeDDML, '', '')
      id%sysinit = add_registration(respondToEventReg, 'sysinit', 
     :                              nullTypeDDML, '', '')
      id%sow = add_registration(respondToEventReg, 'sow', 
     :                          nullTypeDDML, '', '')
      id%harvest = add_registration(respondToEventReg, 'harvest', 
     :                              nullTypeDDML, '', '')
      id%kill_crop = add_registration(respondToEventReg, 'kill_crop', 
     :                                nullTypeDDML, '', '')
      id%end_crop = add_registration(respondToEventReg, 'end_crop', 
     :                               nullTypeDDML, '', '')
      id%prepare = add_registration(respondToEventReg, 'prepare', 
     :                              nullTypeDDML, '', '')
      id%process = add_registration(respondToEventReg, 'process', 
     :                              nullTypeDDML, '', '')

      dummy = add_registration_with_units(respondToGetReg, 'crop_type', 
     :                                    stringTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                          'extinct_coeff', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 'radn_int', 
     :                                    singleTypeDDML, 'MJ/m^2/d')
      dummy = add_registration_with_units(respondToGetReg, 'rue_day', 
     :                                    singleTypeDDML, 'g/MJ')
      dummy = add_registration_with_units(respondToGetReg, 'das', 
     :                                    integer4TypeDDML, 'days')
      dummy = add_registration_with_units(respondToGetReg
     :                         , 'plant_status', stringTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 'stage_name', 
     :                                    stringTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 'stage_code', 
     :                                    singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 'stage', 
     :                                    singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                            'zadok_stage', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 'dlt_stage', 
     :                                    singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 'dlt_tt', 
     :                                    singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 'tt_tot', 
     :                                    singleArrayTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 'tt_sum', 
     :                                    singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 'days_tot', 
     :                                    singleArrayTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 'phase_tt', 
     :                                    singleArrayTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 'dlt_tt_fm', 
     :                                    singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 'tt_tot_fm', 
     :                                    singleArrayTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'flowering_date',  integer4TypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,  
     :                        'maturity_date', integer4TypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'flowering_das', integer4TypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'maturity_das', integer4TypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'leaf_primodia', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'leaf_no_final', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'leaf_no', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'dlt_leaf_no', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'leaf_no_dead', singleArrayTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'leaf_area', singleArrayTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'cover_green', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'cover_tot', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'lai', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'lai_max', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'lai_sum', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'tlai', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'slai', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'tlai_dead', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'sla', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'dlt_lai', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'dlt_lai_pot', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'dlt_lai_stressed', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'tiller_tt_tot', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'dlt_slai', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'dlt_slai_age', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'dlt_slai_light', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'dlt_slai_water', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'dlt_slai_nitrogen', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'plants', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'height', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'plants', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'tiller_no', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'tiller_no_fertile', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'grain_no', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'grain_size', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'root_depth', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'root_length', singleArrayTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'rlv', singleArrayTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'leaf_part', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'stem_part', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'grain_part', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'root_part', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'leaf_wt', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'stem_wt', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'flower_wt', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'stem+flower_wt', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'grain_wt', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'root_wt', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'droot_wt', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'troot_wt', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'biomass_wt', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'green_biomass_wt', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'stover_wt', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'dm_green', singleArrayTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'dm_senesced', singleArrayTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'dm_dead', singleArrayTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'yield', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'biomass', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'stover', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'green_biomass', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'hi', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'dlt_dm_water', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'dlt_dm_light', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'dlt_dm', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'dlt_dm_green', singleArrayTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :             'dlt_dm_green_retrans', singleArrayTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :            'dlt_dm_senesced', singleArrayTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :             'dlt_dm_detached', singleArrayTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :            'dlt_dm_dead_detached', singleArrayTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'swdef_pheno', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'swdef_photo', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'swdef_expan', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'swdef_tiller', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'sw_stress_photo', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'sw_stress_pheno', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'sw_stress_expan', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'sw_stress_tiller', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'ep', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'sw_uptake', singleArrayTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'transpiration', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'transpiration_tot', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'cep', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'esw_layer', singleArrayTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'esw_profile', singleArrayTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'sw_deficit', singleArrayTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'vpd', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'transp_eff', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'sw_demand', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'sw_demand_te', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'sw_supply', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'sw_supply_sum', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'sw_supply_demand_ratio', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'll', singleArrayTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'll_dep', singleArrayTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'kl', singleArrayTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :                        'xf', singleArrayTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'leaf_nd', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'stem_nd', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'flower_nd', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'grain_nd', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'root_nd', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'n_demand', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'no3_demand', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'n_supply_soil', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'n_massflow_uptake', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'no3_massflow_uptake', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'nh4_massflow_uptake', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'no3_diffusion_uptake', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'nh4_diffusion_uptake', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'n_total_uptake', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'no3_total_uptake', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'nh4_total_uptake', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'n_cum_uptake', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'no3_uptake', singleArrayTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'nh4_uptake', singleArrayTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'no3_tot', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'nh4_tot', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'hi_n', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'biomass_n', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'green_biomass_n', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'stover_n', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'grain_n', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'gleaf_n', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'dleaf_n', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'tleaf_n', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'stem_n', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'flower_n', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'groot_n', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'droot_n', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'troot_n', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'n_green', singleArrayTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'n_senesced', singleArrayTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'n_dead', singleArrayTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'dlt_n_green', singleArrayTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'dlt_n_retrans', singleArrayTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'dlt_n_detached', singleArrayTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'dlt_n_dead_detached', singleArrayTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'sln', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'n_conc_stover', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'n_conc_leaf', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'n_conc_stem', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'n_conc_root', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'n_conc_grain', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'n_conc_leaf_crit', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'n_conc_stem_crit', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'n_conc_flower_crit', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'n_conc_root_crit', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'n_conc_stover_crit', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'n_conc_leaf_max', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'n_conc_stem_max', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'n_conc_flower_max', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'n_conc_root_max', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'n_conc_stover_max', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'n_conc_leaf_min', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'n_conc_stem_min', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'n_conc_flower_min', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'n_conc_root_min', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'n_conc_stover_min', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'n_grain_pcnt', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'grain_protein', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'nfact_photo', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'nfact_pheno', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'nfact_expan', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'nfact_tiller', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'nfact_grain', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'nfact_grain_tot', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'n_stress_photo', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'n_stress_pheno', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'n_stress_expan', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'n_stress_tiller', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'n_stress_grain', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'p_green', singleArrayTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'p_sen', singleArrayTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'p_demand', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'pfact_photo', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'pfact_expansion', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'pfact_expan', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'pfact_expansion', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'pfact_grain', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'p_stress_photo', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'p_stress_fact_pheno', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'p_stress_fact_expan', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'p_stress_fact_grain', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'p_stress_fact_grain', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'biomass_p', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'p_uptake', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'green_biomass_p', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'grain_p', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'leaf_p', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'stem_p', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'root_p', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'deadleaf_p', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'flower_p', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'head_p', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'p_senesced', singleArrayTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'p_dead', singleArrayTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'dlt_p_green', singleArrayTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'dlt_p_retrans', singleArrayTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'dlt_p_detached', singleArrayTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'dlt_p_dead', singleArrayTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'dltp_sen', singleArrayTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'p_conc_stover', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'p_conc_leaf', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'p_conc_stem', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'p_conc_grain', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'p_grain_pcnt', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'p_uptake_stover', singleTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 
     :              'grain_p_demand', singleTypeDDML, '')
   

      dummy = add_registration_with_units(respondToGetReg, '', 
     :                                    singleTypeDDML, '')
      end subroutine doInit1
