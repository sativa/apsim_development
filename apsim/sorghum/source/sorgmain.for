C     Last change:  E     6 Aug 2001    3:00 pm

*     ===========================================================
      subroutine crop_process ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*       Simulate crop processes.  These include biomass production,
*       phenological stages, plant component development,
*       water uptake and nitrogen uptake, and plant senescense.

*+  Changes
*      250894 sc   specified and programmed

*+  Calls

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sorg_process')
      character  string*200            ! output string

*- Implementation Section ----------------------------------
      call push_routine (my_name)

!CROP WATER SUPPLY
!     crop template settings: Sorg_root_depth(1),Sorg_water_supply(1)
!                 call sorg_light_supply(400)
!     Skip Row settings: Sorg_root_depth(3),Sorg_water_supply(2)
!                 call sorg_light_supply(2)


      if (g%plant_status.eq.status_alive) then


c      call Sorg_root_depth(1)          !CT = crop template
      call Sorg_root_depth(3)          !skip row GMC
      call Sorg_root_depth_init(2)     !CT - called later as root_depth sets delta
                                        ! option 1 initial root depth = c%...
                                        ! option 2 initial root depth = sowing depth
c      call Sorg_water_supply(1)        !CT
      call Sorg_water_supply(2)        !skip row GMC
      call Sorg_water_stress(1)        !CT


c      if (g%current_stage .lt. 3.0) then
c        g%swdef_pheno     = 1.0
c        g%swdef_photo     = 1.0
c        g%swdef_expansion = 1.0
c        g%swdef_tiller    = 1.0
c      end if


!CROP WATER DEMAND (following are in PREPARE section for APSWIM version)
!cf Sorg light_supply is based on direct calculation of k from rowspacing etc
!rather than looking for cover calculation (that is only done at end of day)
!Problem with other modules is that they use cover_green which is calculated elsewhere!
!      call sorg_nit_stress(400)          !CT
      call sorg_nit_stress(401)          !GMC set to 1 for tests    SORGHUM
      call Sorg_temp_stress(1)         !CT
c      call sorg_light_supply(400)
      call sorg_light_supply(2)        ! skip row GMC
      call Sorg_bio_RUE(1)             !CT
      call Sorg_transpiration_eff(1)   !CT No bounding on VPD or TE
      call Sorg_water_demand(1)        !CT!movable!

      if (g%phosphorus_aware) then
         call PlantP_prepare(g%current_stage
     :                      ,g%dm_green
     :                      ,g%dlt_dm_light)
      else
      endif


cew    if (g%plant_status.eq.status_alive) then




!PHENOLOGY
         call sorg_phenology_init(1)     !improved, but similar to maize
         call sorg_phenology(400)        !different TT for grainfill
!below causes slight variation as cannot control N,water stress separately
!         call sorg_phenology(403)        !different TT for grainfill

!WATER UPTAKE
         call Sorg_water_uptake(2)      !CT

!CANOPY HEIGHT
         call Sorg_height(1)            !CT

!NODE NUMBER and APPEARANCE
         call Sorg_leaf_no_init(1)      !CT
         call sorg_leaf_no_pot(400)      !Revised w. start/end stages

!LEAF AREA - Potential and stressed
         call sorg_leaf_area_pot(2)      !TPLA (works using new template)
         call Sorg_leaf_area_stressed(1)!CT

!BIOMASS Water_limited, light_N_temp limited, actual
         call Sorg_bio_TE(1)            !CT delta_bio_water
         call Sorg_bio_RUE(1)           !CT delta_bio_light
         call sorg_bio_init(1)           !these 2 routines are together
         call sorg_bio_actual(1)         !same as maize_bio_actual

!ECONOMIC YIELD - demand
         call Sorg_bio_grain_demand_stress(1)!CT
!         call Sorg_bio_grain_demand(1) !CT HI approach
         call Sorg_bio_grain_demand(2)  !local source sink RLV  SORGHUM

!BIOMASS PARTITIONING and RETRANSLOCATION
         call sorg_bio_partition(400)    !CODE is SAME as maize option(1)
         call Sorg_bio_retrans(1)       !CT

!ROOT BIOMASS PARTITIONING AND ROOT LENGTH DENSITY
         call Sorg_root_length_init(1)  !CT
         call Sorg_root_dist(1)         !CT

!LEAF AREA - Actual (biomass limited)
         call sorg_leaf_area_actual(400) !Limits g%dlt_lai by C

!         call sorg_leaf_death(400)       !AGE kill of leaves - not req. for SPLA approach

!SENESCENCE - leaf area, biomass, roots, N

         call sorg_leaf_area_sen(400)    !Alternate approach to leaf senesc.
         call Sorg_sen_bio(1)           !CT
         call Sorg_sen_root_length(1)   !CT
!         call Sorg_sen_nit(1)           !CT  original
         call Sorg_sen_nit(400)           ! new nitrogen approach   SORGHUM

!NITROGEN SUPPLY - soil, N_fix, other parts
!         call Sorg_nit_supply(1)        !CT
!         call Sorg_nit_init(1)          !CT
!         call sorg_N_retranslocate(400)  !CODE is SAME as maize

!NITROGEN DEMAND/UPTAKE/PARTITION
!         call Sorg_nit_demand(2)        !CT
!         call Sorg_nit_uptake(1)        !CT
!         call sorg_N_partition(400)      !CODE is SAME as maize
!---------------------    SORGHUM  ---------------------------------
         call Sorg_nit_init(2)          !GMC New N 2

         call Sorg_nit_supply(1)        !CT

         call Sorg_nit_demand(3)        !GMC New N 3

         call Sorg_nit_uptake(2)        !GMC New N 2 (remove N from soil)

         call sorg_N_partition(401)     !GMC New 401 (calc dltN to plant and from soil)

         call sorg_N_retranslocate(401) !GMC New 401 (retranslocate N where needed)

                                 !SECTION 9: PLANT P RELATIONS
         if (g%phosphorus_aware) then
            call PlantP_Process(g%current_stage
     :                      ,g%dm_green
     :                      ,g%dm_senesced
     :                      ,g%dlt_dm_senesced
     :                      ,g%dlt_dm_detached)
         else
         endif

!---------------------    SORGHUM  ---------------------------------

!DEATH of PLANTS (cf. plant part pools)
         call sorg_plant_death(1)

!Check to see if plant death should terminate crop

         if(reals_are_equal (g%dlt_plants_dead + g%plants, 0.0))then
            call kill_crop (
     .          g%plant_status,
     .          g%dm_green,
     .          g%dm_senesced,
     .          g%dm_dead)
         endif

      else

!        plant_status not alive

      endif

!     cleanup after sorg process

!modify detachment so that it is generalised
!in cropother.for
!      call crop_detachment(1)   !Calls 4 routines in option 1
      call Crop_detachment(1)
      call Crop_cleanup()        !CODE is almost same as maize

      call pop_routine (my_name)

      return
      end subroutine

*     ===========================================================
      subroutine crop_read_cultivar_params (cultivar)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      character  cultivar*(*)          ! (INPUT) keyname of cultivar in crop
                                       ! parameter file

*+  Purpose
*       Get cultivar parameters for named cultivar, from crop parameter file.

*+  Changes
*       090994 sc   specified and programmed
*       10/6/98 dph fixed invalid format specification.

*+  Calls
                                       ! lu_src_sum

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sorg_read_cultivar_params')

*+  Local Variables
      character  string*200            ! output string
      integer    numvals               ! number of values read
      integer    i

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call write_string (
     :                 new_line//'   - Reading Cultivar Parameters')

         ! TEMPLATE OPTION
         !   sorg_leaf_area_devel_plant

!scc This coeff. moved from sorg.ini to sorg.par file
      call read_real_var (cultivar
     :                    , 'main_stem_coef', '()'
     :                    , p%main_stem_coef, numvals
     :                    , 0.0, 10.0)

       call read_real_var (cultivar
     :                    , 'tpla_prod_coef', '(????)'
     :                    , p%tpla_prod_coef, numvals
     :                    , 0.0, 10.0)

cSCC change upper limit from 10 to 1000
       call read_real_var (cultivar
     :                    , 'tpla_inflection', '(????)'
     :                    , p%tpla_inflection, numvals
     :                    , 0.0, 1000.0)

cSCC Moved to be read in w. sowing info
!       call read_real_var (cultivar
!     :                    , 'tiller_no_fertile', '(????)'
!     :                    , p%tiller_no_fertile, numvals
!     :                    , 0.0, 100.0)

         ! TEMPLATE OPTION
         !   sorg_leaf_area_sen_age1

       call read_real_var (cultivar
     :                    , 'spla_prod_coef', '(????)'
     :                    , p%spla_prod_coef, numvals
     :                    , 0.0, 100.0)

cSCC changed lower limit from 0 to -1000
       call read_real_var (cultivar
     :                    , 'spla_intercept', '(????)'
     :                    , p%spla_intercept, numvals
     :                    , -1000.0, 100.0)

         ! TEMPLATE OPTION
         !       legume_dm_grain_hi

      call read_real_var (cultivar
     :                    , 'hi_incr', '()'
     :                    , p%hi_incr, numvals
     :                    , 0.0, 1.0)

      call read_real_array (cultivar
     :                   , 'x_hi_max_pot_stress', max_table, '(0-1)'
     :                   , p%x_hi_max_pot_stress, p%num_hi_max_pot
     :                   , 0.0, 1.0)

      call read_real_array (cultivar
     :                   , 'y_hi_max_pot', max_table, '(0-1)'
     :                   , p%y_hi_max_pot, p%num_hi_max_pot
     :                   , 0.0, 1.0)


         ! TEMPLATE OPTION
         !   sorg_check_grain_no  sorg_grain_no

      call read_real_var (cultivar
     :                    , 'head_grain_no_max', '()'
     :                    , p%head_grain_no_max, numvals
     :                    , 0.0, c%head_grain_no_max_ub)

         ! TEMPLATE OPTION
         !   sorg_dm_grain

      call read_real_var (cultivar
     :                    , 'grain_gth_rate', '()'
     :                    , p%grain_gth_rate, numvals
     :                    , 0.0, c%grain_gth_rate_ub)

         !   sorg_phenology_init

      call read_real_var (cultivar
     :                    , 'tt_emerg_to_endjuv', '()'
     :                    , p%tt_emerg_to_endjuv, numvals
     :                    , 0.0, c%tt_emerg_to_endjuv_ub)

      call read_real_var (cultivar
     :                    , 'tt_flower_to_maturity', '()'
     :                    , p%tt_flower_to_maturity, numvals
     :                    , 0.0, c%tt_flower_to_maturity_ub)

      call read_integer_var (cultivar
     :                    , 'est_days_endjuv_to_init', '()'
     :                    , p%est_days_endjuv_to_init, numvals
     :                    , 0, 100)

      call read_real_var (cultivar
     :                    , 'pp_endjuv_to_init', '()'
     :                    , p%pp_endjuv_to_init, numvals
     :                    , 0.0, c%pp_endjuv_to_init_ub)

      call read_real_var (cultivar
     :                    , 'tt_endjuv_to_init', '()'
     :                    , p%tt_endjuv_to_init, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (cultivar
     :                    , 'photoperiod_crit1', '()'
     :                    , p%photoperiod_crit1, numvals
     :                    , 0.0, 24.0)

      call read_real_var (cultivar
     :                    , 'photoperiod_crit2', '()'
     :                    , p%photoperiod_crit2, numvals
     :                    , 0.0, 24.0)

      call read_real_var (cultivar
     :                    , 'photoperiod_slope', '()'
     :                    , p%photoperiod_slope, numvals
     :                    , 0.0, 200.0)

      call read_real_var (cultivar
     :                    , 'tt_flag_to_flower', '()'
     :                    , p%tt_flag_to_flower, numvals
     :                    , 0.0, c%tt_flag_to_flower_ub)

      call read_real_var (cultivar
     :                    , 'tt_flower_to_start_grain', '()'
     :                    , p%tt_flower_to_start_grain, numvals
     :                    , 0.0, c%tt_flower_to_start_grain_ub)


      call read_real_var (cultivar
     :                    , 'tt_maturity_to_ripe', '()'
     :                    , p%tt_maturity_to_ripe, numvals
     :                    , 0.0, c%tt_maturity_to_ripe_ub)

      call read_real_var (cultivar
     :                    , 'dm_per_seed', '()'
     :                    , p%dm_per_seed, numvals
     :                    , 0.0, 1.0)

      call read_real_array (cultivar
     :                     , 'x_stem_wt', max_table, '()'
     :                     , p%x_stem_wt, p%num_stem_wt
     :                     , 0.0, 1000.0)

      call read_real_array (cultivar
     :                     , 'y_height', max_table, '()'
     :                     , p%y_height, p%num_stem_wt
     :                     , 0.0, 5000.0)

             ! report

      string = '    ------------------------------------------------'
      call write_string (string)

      write (string, '(4x,2a)')
     :                'Cultivar                 = ', cultivar
      call write_string (string)

      write (string, '(4x, a, i4)')
     :                'est_days_endjuv_to_init  = '
     :               , p%est_days_endjuv_to_init
      call write_string (string)

      write (string, '(4x, a, f7.2)')
     :                'tt_emerg_to_endjuv       = '
     :               , p%tt_emerg_to_endjuv
      call write_string (string)

      write (string, '(4x, a, f7.2)')
     :                'tt_flower_to_maturity       = '
     :               , p%tt_flower_to_maturity
      call write_string (string)

      write (string, '(4x, a, f7.2)')
     :                'pp_endjuv_to_initp       = '
     :               , p%pp_endjuv_to_init
      call write_string (string)


      write (string, '(4x, a, f7.2)')
     :                'head_grain_no_max        = '
     :               , p%head_grain_no_max
      call write_string (string)

      write (string, '(4x, a, f7.2)')
     :                'grain_gth_rate           = '
     :               , p%grain_gth_rate
      call write_string (string)

      write (string, '(4x, a, f7.2)')
     :                'tt_flag_to_flower        = '
     :               , p%tt_flag_to_flower
      call write_string (string)

      write (string, '(4x, a, f7.2)')
     :                'tt_flower_to_start_grain = '
     :               , p%tt_flower_to_start_grain
      call write_string (string)

      write (string, '(4x, a, f7.2)')
     :                'tt_maturity_to_ripe      = '
     :               , p%tt_maturity_to_ripe
      call write_string (string)

         ! TEMPLATE OPTION
       write (string, '(4x, a, f7.3)')
     :                'hi_incr                  = '
     :               , p%hi_incr
       call write_string (string)

         ! TEMPLATE OPTION

      write (string, '(4x, a, 10f7.2)')
     :                'x_hi_max_pot_stress = '
     :               , (p%x_hi_max_pot_stress(i), i=1,p%num_hi_max_pot)
      call write_string (string)

      write (string, '(4x, a, 10f7.2)')
     :                'y_hi_max_pot        = '
     :               , (p%y_hi_max_pot(i), i=1,p%num_hi_max_pot)
      call write_string (string)

         ! TEMPLATE OPTION
       write (string, '(4x, a, f7.3)')
     :                'tpla_prod_coef           = '
     :               , p%tpla_prod_coef
       call write_string (string)

         ! TEMPLATE OPTION
       write (string, '(4x, a, f7.3)')
     :                'tpla_inflection          = '
     :               , p%tpla_inflection
       call write_string (string)

         ! TEMPLATE OPTION
!       write (string, '(4x, a, f7.3)')
!     :                'tiller_no_fertile        = '
!     :               , p%tiller_no_fertile
!       call write_string (string)

         ! TEMPLATE OPTION
       write (string, '(4x, a, f7.3)')
     :                'spla_prod_coef           = '
     :               , p%spla_prod_coef
       call write_string (string)

         ! TEMPLATE OPTION
       write (string, '(4x, a, f7.3)')
     :                'spla_intercept           = '
     :               , p%spla_intercept
       call write_string (string)

      write (string, '(4x, a, 10f7.1)')
     :                'x_stem_wt      = '
     :               , (p%x_stem_wt(i), i=1,p%num_stem_wt)
      call write_string (string)

      write (string, '(4x, a, 10f7.1)')
     :                'y_height      = '
     :               , (p%y_height(i), i=1,p%num_stem_wt)
      call write_string (string)

      string = '    ------------------------------------------------'
      call write_string (string)

      call write_string (new_line//new_line)

      call pop_routine (my_name)
      return
      end subroutine
*     ===========================================================
      subroutine crop_read_constants ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*       Crop initialisation - reads constants from constants file

*+  Changes
*     010994 sc   specified and programmed
*     070495 psc added extra constants (leaf_app etc.)
*     110695 psc added soil temp effects on plant establishment
*     270995 scc added leaf area options
*     020998 sb removed c%year_lb and c%year_ub.

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sorg_read_constants')
      character  section_name*(*)
      parameter (section_name = 'constants')

*+  Local Variables
      integer    numvals               ! number of values returned

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call write_string (
     :                  new_line//'    - Reading constants')

      call read_char_var (section_name
     :                     , 'crop_type', '()'
     :                     , c%crop_type, numvals)

      call read_char_array (section_name
     :                     , 'stage_names', max_stage, '()'
     :                     , c%stage_names, numvals)

      call read_real_array (section_name
     :                     , 'stage_code', max_stage, '()'
     :                     , c%stage_code_list, numvals
     :                     , 0.0, 1000.0)

      call read_real_array (section_name
     :                     , 'rue', max_stage, '(g dm/mj)'
     :                     , c%rue, numvals
     :                     , 0.0, 1000.0)

      call read_real_array (section_name
     :                     , 'root_depth_rate', max_stage, '(mm)'
     :                     , c%root_depth_rate, numvals
     :                     , 0.0, 1000.0)

      call read_real_array (section_name
     :                     , 'ratio_root_shoot', max_stage, '()'
     :                     , c%ratio_root_shoot, numvals
     :                     , 0.0, 1000.0)

c      call read_real_var (section_name
c     :                    , 'extinction_coef', '()'
c     :                    , c%extinction_coef, numvals
c     :                    , 0.0, 10.0)

c      call read_real_var (section_name
c     :                    , 'extinction_coef_dead', '()'
c     :                    , c%extinction_coef_dead, numvals
c     :                    , 0.0, 10.0)
!cpsc
c      call read_real_var (section_name
c     :                    , 'extinction_coef_change', '()'
c     :                    , c%extinction_coef_change, numvals
c     :                    , 0.0, 10.0)

!! gmc row_spacing_default     = 0.75        (m)
! Default rowing spacing used to  calculate k
      call read_real_var (section_name
     :                    , 'row_spacing_default', '()'
     :                    , c%row_spacing_default, numvals
     :                    , 0.0, 2.0)

      call read_real_array (section_name
     :                    , 'x_row_spacing', max_table, '(m)'
     :                    , c%x_row_spacing, c%num_row_spacing
     :                    , 0.0, 2000.)

      call read_real_array (section_name
     :                    , 'y_extinct_coef', max_table, '()'
     :                    , c%y_extinct_coef, c%num_row_spacing
     :                    , 0.0, 1.0)

      call read_real_array (section_name
     :                    , 'y_extinct_coef_dead', max_table, '()'
     :                    , c%y_extinct_coef_dead, c%num_row_spacing
     :                    , 0.0, 1.0)
          ! legume_root_distrib

!      call read_real_var (section_name
!     :                    , 'root_extinction', '()'
!     :                    , c%root_extinction, numvals
!     :                    , 0.0, 10.0)

         ! crop failure

      call read_real_var (section_name
     :                    , 'leaf_no_crit', '()'
     :                    , c%leaf_no_crit, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'tt_emerg_limit', '(oC)'
     :                    , c%tt_emerg_limit, numvals
     :                    , 0.0, 365.0)

      call read_real_var (section_name
     :                    , 'days_germ_limit', '(days)'
     :                    , c%days_germ_limit, numvals
     :                    , 0.0, 365.0)

      call read_real_var (section_name
     :                    , 'swdf_pheno_limit', '()'
     :                    , c%swdf_pheno_limit, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'swdf_photo_limit', '()'
     :                    , c%swdf_photo_limit, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'swdf_photo_rate', '()'
     :                    , c%swdf_photo_rate, numvals
     :                    , 0.0, 1.0)


         !    sorg_root_depth

         !    Maize_root_depth

!      call read_real_var (section_name
!     :                    , 'initial_root_depth', '(mm)'
!     :                    , c%initial_root_depth, numvals
!     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'specific_root_length', '(mm/g)'
     :                    , c%specific_root_length, numvals
     :                    , 0.0, 1.e6)

      call read_real_array (section_name
     :                     , 'x_plant_rld', max_table, '(mm)'
     :                     , c%x_plant_rld, c%num_plant_rld
     :                     , 0.0, 0.1)
      call read_real_array (section_name
     :                     , 'y_rel_root_rate', max_table, '()'
     :                     , c%y_rel_root_rate, c%num_plant_rld
     :                     , 0.0, 1.0)

         !    sorg_leaf_area_init

      call read_real_var (section_name
     :                    , 'initial_tpla', '(mm^2)'
     :                    , c%initial_tpla, numvals
     :                    , 0.0, 100000.0)

         ! TEMPLATE OPTION
         !    sorg_leaf_area_devel

      call read_real_var (section_name
     :                    , 'sla_max', '(mm^2/g)'
     :                    , c%sla_max, numvals
     :                    , 0.0, 100000.0)

         ! TEMPLATE OPTION
         !    sorg_leaf_area_devel_plant

      call read_real_var (section_name
     :                    , 'tiller_coef', '()'
     :                    , c%tiller_coef , numvals
     :                    , 0.0, 10.0)

      call read_real_var (section_name
     :                    , 'tpla_inflection_ratio', '()'
     :                    , c%tpla_inflection_ratio , numvals
     :                    , 0.0, 1.0)

! scc This parameter moved from sorg.ini to sorg.par file
!      call read_real_var (section_name
!     :                    , 'main_stem_coef', '()'
!     :                    , c%main_stem_coef, numvals
!     :                    , 0.0, 10.0)

         !    sorg_height

      call read_real_var (section_name
     :                    , 'height_max', '(mm)'
     :                    , c%height_max, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'height_stem_slope', '(mm/g/stem)'
     :                    , c%height_stem_slope, numvals
     :                    , 0.0, 1000.0)

         !    sorg_get_cultivar_params

      call read_real_var (section_name
     :                    , 'head_grain_no_max_ub', '()'
     :                    , c%head_grain_no_max_ub, numvals
     :                    , 0.0, 100000.0)

      call read_real_var (section_name
     :                    , 'grain_gth_rate_ub', '()'
     :                    , c%grain_gth_rate_ub, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'tt_emerg_to_endjuv_ub', '()'
     :                    , c%tt_emerg_to_endjuv_ub, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'pp_endjuv_to_init_ub', '()'
     :                    , c%pp_endjuv_to_init_ub, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'tt_flower_to_maturity_ub', '()'
     :                    , c%tt_flower_to_maturity_ub, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'tt_maturity_to_ripe_ub', '()'
     :                    , c%tt_maturity_to_ripe_ub, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'tt_flower_to_start_grain_ub', '()'
     :                    , c%tt_flower_to_start_grain_ub, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'tt_flag_to_flower_ub', '()'
     :                    , c%tt_flag_to_flower_ub, numvals
     :                    , 0.0, 1000.0)

         !    Maize_transp_eff

      call read_real_var (section_name
     :                    , 'svp_fract', '()'
     :                    , c%svp_fract, numvals
     :                    , 0.0, 1.0)

      call read_real_array (section_name
     :                     , 'transp_eff_cf', max_stage, '(kpa)'
     :                     , c%transp_eff_cf, numvals
     :                     , 0.0, 1.0)

      call read_real_array (section_name
     :                     , 'n_fix_rate', max_stage, '()'
     :                     , c%n_fix_rate, numvals
     :                     , 0.0, 1.0)

         ! TEMPLATE OPTION
         !    sorg_grain_no

      call read_real_var (section_name
     :                    , 'head_grain_no_crit', '()'
     :                    , c%head_grain_no_crit, numvals
     :                    , 0.0, 1000.0)

         !    sorg_plants_barren

      call read_real_var (section_name
     :                    , 'barren_crit', '()'
     :                    , c%barren_crit, numvals
     :                    , 0.0, 1.0)

         !    sorg_germination

      call read_real_var (section_name
     :                    , 'pesw_germ', '(mm/mm)'
     :                    , c%pesw_germ, numvals
     :                    , 0.0, 1.0)

         ! TEMPLATE OPTION
         !    sorg_grain_no

      call read_real_var (section_name
     :                    , 'grain_n_conc_min', '()'
     :                    , c%grain_N_conc_min, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'seed_wt_min', '(g/seed)'
     :                    , c%seed_wt_min, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'growth_rate_min', '(g/plant)'
     :                    , c%growth_rate_min, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'growth_rate_crit', '(g/plant)'
     :                    , c%growth_rate_crit, numvals
     :                    , 0.0, 1000.0)

         !    sorg_leaf_appearance

      call read_real_var (section_name
     :                    , 'leaf_no_at_emerg', '()'
     :                    , c%leaf_no_at_emerg, numvals
     :                    , 0.0, 100.0)

         !    sorg_N_uptake

      call read_real_var (section_name
     :                    , 'no3_diffn_const', '(days)'
     :                    , c%NO3_diffn_const, numvals
     :                    , 0.0, 100.0)

      call read_char_var (section_name
     :                     , 'n_supply_preference', '()'
     :                     , c%n_supply_preference, numvals)

         !    sorg_phenology_init


      call read_real_var (section_name
     :                    , 'shoot_lag', '(oC)'
     :                    , c%shoot_lag, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'shoot_rate', '(oC/mm)'
     :                    , c%shoot_rate, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'photoperiod_base', '(hr)'
     :                    , c%photoperiod_base, numvals
     :                    , 0.0, 24.0)

      call read_real_var (section_name
     :                    , 'leaf_app_rate', '(oC)'
     :                    , c%leaf_app_rate, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'leaf_app_rate1', '(oC)'
     :                    , c%leaf_app_rate1, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'leaf_app_rate2', '(oC)'
     :                    , c%leaf_app_rate2, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'leaf_no_rate_change', '()'
     :                    , c%leaf_no_rate_change, numvals
     :                    , 0.0, 30.0)

         !    sorg_dm_init

      call read_real_var (section_name
     :                    , 'dm_leaf_init', '(g/plant)'
     :                    , c%dm_leaf_init, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'dm_root_init', '(g/plant)'
     :                    , c%dm_root_init, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'dm_stem_init', '(g/plant)'
     :                    , c%dm_stem_init, numvals
     :                    , 0.0, 1000.0)

         !    sorg_get_root_params

      call read_real_var (section_name
     :                    , 'll_ub', '()'
     :                    , c%ll_ub, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'kl_ub', '()'
     :                    , c%kl_ub, numvals
     :                    , 0.0, 1000.0)

         !    sorg_leaf_no_final

      call read_real_var (section_name
     :                    , 'leaf_init_rate', '(oC)'
     :                    , c%leaf_init_rate, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'leaf_no_seed', '(leaves)'
     :                    , c%leaf_no_seed, numvals
     :                    , 0.0, 100.0)

c      call read_real_var (section_name
c     :                    , 'floral_init_error', '(oc)'
c     :                    , c%floral_init_error, numvals
c     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'leaf_no_min', '()'
     :                   , c%leaf_no_min, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'leaf_no_max', '()'
     :                   , c%leaf_no_max, numvals
     :                   , 0.0, 100.0)

         !    sorg_retranslocate

      call read_real_var (section_name
     :                    , 'stem_trans_frac', '()'
     :                    , c%stem_trans_frac, numvals
     :                    , 0.0, 1.0)

      call read_real_var (section_name
     :                    , 'leaf_trans_frac', '()'
     :                    , c%leaf_trans_frac, numvals
     :                    , 0.0, 1.0)

         !    sorg_watck

      call read_real_var (section_name
     :                    , 'minsw', '()'
     :                    , c%minsw, numvals
     :                    , 0.0, 1000.0)

         ! TEMPLATE OPTION
         !    sorg_dm_grain

      call read_real_var (section_name
     :                    , 'swdf_grain_min', '()'
     :                    , c%swdf_grain_min, numvals
     :                    , 0.0, 100.0)

         ! TEMPLATE OPTION
         !    sorg_dm_grain_hi

      call read_real_var (section_name
     :                    , 'hi_min', '()'
     :                    , c%hi_min, numvals
     :                    , 0.0, 100.0)

         !    sorg_N_dlt_grain_conc

      call read_real_var (section_name
     :                    , 'sw_fac_max', '()'
     :                    , c%sw_fac_max, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'temp_fac_min', '()'
     :                    , c%temp_fac_min, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'sfac_slope', '()'
     :                    , c%sfac_slope, numvals
     :                    , -10.0, 0.0)

      call read_real_var (section_name
     :                    , 'tfac_slope', '()'
     :                    , c%tfac_slope, numvals
     :                    , 0.0, 100.0)

         !    sorg_leaf_death

cSCC changed lower limit from 10.0 to 0.0
      call read_real_var (section_name
     :                    , 'leaf_no_dead_const', '()'
     :                    , c%leaf_no_dead_const, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'leaf_no_dead_slope', '()'
     :                    , c%leaf_no_dead_slope, numvals
     :                    , 0.0, 100.0)

         !    sorg_get_other_variables

         ! checking the bounds of the bounds..
      call read_real_var (section_name
     :                    , 'latitude_ub', '(oL)'
     :                    , c%latitude_ub, numvals
     :                    , -90.0, 90.0)

      call read_real_var (section_name
     :                    , 'latitude_lb', '(oL)'
     :                    , c%latitude_lb, numvals
     :                    , -90.0, 90.0)

      call read_real_var (section_name
     :                    , 'maxt_ub', '(oC)'
     :                    , c%maxt_ub, numvals
     :                    , 0.0, 60.0)

      call read_real_var (section_name
     :                    , 'maxt_lb', '(oC)'
     :                    , c%maxt_lb, numvals
     :                    , 0.0, 60.0)

      call read_real_var (section_name
     :                    , 'mint_ub', '(oC)'
     :                    , c%mint_ub, numvals
     :                    , 0.0, 40.0)

      call read_real_var (section_name
     :                    , 'mint_lb', '(oC)'
     :                    , c%mint_lb, numvals
     :                    , -100.0, 100.0)

      call read_real_var (section_name
     :                    , 'radn_ub', '(MJ/m^2)'
     :                    , c%radn_ub, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'radn_lb', '(MJ/m^2)'
     :                    , c%radn_lb, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'dlayer_ub', '(mm)'
     :                    , c%dlayer_ub, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'dlayer_lb', '(mm)'
     :                    , c%dlayer_lb, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'dul_dep_ub', '(mm)'
     :                    , c%dul_dep_ub, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'dul_dep_lb', '(mm)'
     :                    , c%dul_dep_lb, numvals
     :                    , 0.0, 10000.0)

                                ! 8th block
      call read_real_var (section_name
     :                    , 'sw_dep_ub', '(mm)'
     :                    , c%sw_dep_ub, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'sw_dep_lb', '(mm)'
     :                    , c%sw_dep_lb, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'no3_ub', '(kg/ha)'
     :                    , c%NO3_ub, numvals
     :                    , 0.0, 100000.0)

      call read_real_var (section_name
     :                    , 'no3_lb', '(kg/ha)'
     :                    , c%NO3_lb, numvals
     :                    , 0.0, 100000.0)

      call read_real_var (section_name
     :                    , 'no3_min_ub', '(kg/ha)'
     :                    , c%NO3_min_ub, numvals
     :                    , 0.0, 100000.0)

      call read_real_var (section_name
     :                    , 'no3_min_lb', '(kg/ha)'
     :                    , c%NO3_min_lb, numvals
     :                    , 0.0, 100000.0)

         !    sorg_event

      call read_real_var (section_name
     :                    , 'grn_water_cont', '(g/g)'
     :                    , c%grn_water_cont, numvals
     :                    , 0.0, 1.0)

         !    sorg_dm_partition

      call read_real_var (section_name
     :                    , 'sla_min', '(mm^2/g)'
     :                    , c%sla_min, numvals
     :                    , 0.0, 100000.0)

      call read_real_var (section_name
     :                    , 'partition_rate_leaf', '()'
     :                    , c%partition_rate_leaf, numvals
     :                    , 0.0, 1.0)

      call read_real_var (section_name
     :                    , 'frac_stem2flower', '()'
     :                    , c%frac_stem2flower, numvals
     :                    , 0.0, 1.0)

         ! TEMPLATE OPTION
         !    sorg_grain_no

      call read_real_var (section_name
     :                    , 'htstress_coeff', '()'
     :                    , c%htstress_coeff, numvals
     :                    , 0.0, 1.0)

         !    sorg_dm_senescence

      call read_real_var (section_name
     :                    , 'dm_root_sen_frac', '()'
     :                    , c%dm_root_sen_frac, numvals
     :                    , 0.0, 1.0)

      call read_real_var (section_name
     :                    , 'dm_leaf_sen_frac', '()'
     :                    , c%dm_leaf_sen_frac, numvals
     :                    , 0.0, 1.0)

         !    sorg_dm_dead_detachment

      call read_real_array (section_name
     :                    , 'dead_detach_frac', max_part, '()'
     :                    , c%dead_detach_frac, numvals
     :                    , 0.0, 1.0)

      call read_real_var (section_name
     :                    , 'dm_leaf_detach_frac', '()'
     :                    , c%dm_leaf_detach_frac, numvals
     :                    , 0.0, 1.0)

         ! TEMPLATE OPTION
         !    sorg_leaf_area_devel

c      call read_real_var (section_name
c     :                    , 'leaf_no_correction', '()'
c     :                    , c%leaf_no_correction, numvals
c     :                    , 0.0, 100.0)

         ! TEMPLATE OPTION
         !    sorg_leaf_size

         ! TEMPLATE OPTION
         !    sorg_leaf_area_sen_light

      call read_real_var (section_name
     :                   , 'lai_sen_light', '(m^2/m^2)'
     :                   , c%lai_sen_light, numvals
     :                   , 3.0, 20.0)

      call read_real_var (section_name
     :                    , 'sen_light_slope', '()'
     :                    , c%sen_light_slope, numvals
     :                    , 0.0, 100.0)


         ! TEMPLATE OPTION
         !    sorg_leaf_area_sen_frost

      call read_real_array (section_name
     :                   , 'x_temp_senescence', max_table, '(oC)'
     :                   , c%x_temp_senescence, c%num_temp_senescence
     :                   , -20.0, 20.0)

      call read_real_array (section_name
     :                   , 'y_senescence_fac', max_table, '()'
     :                   , c%y_senescence_fac, c%num_temp_senescence
     :                   , 0.0, 1.0)

         ! TEMPLATE OPTION
         !    sorg_leaf_area_sen_water

      call read_real_var (section_name
     :                    , 'sen_rate_water', '()'
     :                    , c%sen_rate_water, numvals
     :                    , 0.0, 100.0)

         ! TEMPLATE OPTION
         !    sorg_leaf_area_sen_light1

      call read_real_var (section_name
     :                    , 'sen_light_time_const', '(days)'
     :                    , c%sen_light_time_const, numvals
     :                    , 0.0, 50.0)

      call read_real_var (section_name
     :                    , 'sen_radn_crit', '(Mj/m^2)'
     :                    , c%sen_radn_crit, numvals
     :                    , 0.0, 10.0)

         ! TEMPLATE OPTION
         !    sorg_leaf_area_sen_frost1

      call read_real_var (section_name
     :                    , 'frost_kill', '(oC)'
     :                    , c%frost_kill, numvals
     :                    , -6.0, 6.0)

        ! TEMPLATE OPTION
         !    sorg_leaf_area_sen_water1

      call read_real_var (section_name
     :                    , 'sen_water_time_const', '(days)'
     :                    , c%sen_water_time_const, numvals
     :                    , 0.0, 50.0)

      call read_real_var (section_name
     :                    , 'sen_threshold', '()'
     :                    , c%sen_threshold, numvals
     :                    , 0.0, 10.0)

         ! TEMPLATE OPTION
         !    sorg_leaf_area_sen_age1

      call read_real_var (section_name
     :                    , 'spla_slope', '(oC/leaf)'
     :                    , c%spla_slope, numvals
     :                    , 0.0, 1000.0)

         !    sorg_phenology_init

      call read_real_var (section_name
     :                   , 'twilight', '(o)'
     :                   , c%twilight, numvals
     :                   , -90.0, 90.0)

         ! TEMPLATE OPTION
         !    sorg_heat_stress

      call read_real_var (section_name
     :                   , 'temp_grain_crit_stress', '(oC)'
     :                   , c%temp_grain_crit_stress, numvals
     :                   , 20.0, 50.0)

         !    sorg_N_conc_limits

      call read_real_array (section_name
     :                     , 'x_stage_code', max_stage, '()'
     :                     , c%x_stage_code, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_crit_leaf', max_stage, '()'
     :                     , c%y_N_conc_crit_leaf, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_max_leaf', max_stage, '()'
     :                     , c%y_N_conc_max_leaf, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_min_leaf', max_stage, '()'
     :                     , c%y_N_conc_min_leaf, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_crit_stem', max_stage, '()'
     :                     , c%y_N_conc_crit_stem, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_max_stem', max_stage, '()'
     :                     , c%y_N_conc_max_stem, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_min_stem', max_stage, '()'
     :                     , c%y_N_conc_min_stem, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_crit_flower', max_stage, '()'
     :                     , c%y_N_conc_crit_flower, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_max_flower', max_stage, '()'
     :                     , c%y_N_conc_max_flower, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_min_flower', max_stage, '()'
     :                     , c%y_N_conc_min_flower, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_crit_root', max_stage, '()'
     :                     , c%y_N_conc_crit_flower, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_max_root', max_stage, '()'
     :                     , c%y_N_conc_max_flower, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_min_root', max_stage, '()'
     :                     , c%y_N_conc_min_flower, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'n_conc_crit_grain', '()'
     :                   , c%N_conc_crit_grain, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'n_conc_max_grain', '()'
     :                   , c%N_conc_max_grain, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'n_conc_min_grain', '()'
     :                   , c%N_conc_min_grain, numvals
     :                   , 0.0, 100.0)

         !    Maize_N_init

      call read_real_array (section_name
     :                     , 'n_init_conc', max_part, '()'
     :                     , c%n_init_conc, numvals
     :                     , 0.0, 100.0)
!     SORGHUM
      call read_real_array (section_name
     :                     , 'n_target_conc', max_part, '()'
     :                     , c%n_target_conc, numvals
     :                     , 0.0, 100.0)
         !    Maize_N_senescence

      call read_real_array (section_name
     :                     , 'n_sen_conc', max_part, '()'
     :                     , c%n_sen_conc, numvals
     :                     , 0.0, 1.0)

         !    nfact

      call read_real_var (section_name
     :                   , 'N_fact_photo', '()'
     :                   , c%N_fact_photo, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'N_fact_pheno', '()'
     :                   , c%N_fact_pheno, numvals
     :                   , 0.0, 100.0)

!scc put this in for consistence w. sugar

      call read_real_var (section_name
     :                   , 'N_fact_expansion', '()'
     :                   , c%N_fact_expansion, numvals
     :                   , 0.0, 100.0)

         !    sorg_rue_reduction

      call read_real_array (section_name
     :                     , 'x_ave_temp', max_table, '(oC)'
     :                     , c%x_ave_temp, c%num_ave_temp
     :                     , 0.0, 100.0)

!cscc added the following to do 3-hour effect on RUE

      call read_real_array (section_name
     :                     , 'x_temp_photo', max_table, '(oC)'
     :                     , c%x_temp_photo, c%num_temp_photo
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_stress_photo', max_table, '()'
     :                     , c%y_stress_photo, c%num_factors
     :                     , 0.0, 1.0)

         ! TEMPLATE OPTION
         !    sorg_dm_grain

      call read_real_array (section_name
     :                     , 'x_temp_grain', max_table, '(oC)'
     :                     , c%x_temp_grain, c%num_temp_grain
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_grain_rate', max_table, '()'
     :                     , c%y_grain_rate, c%num_temp_grain
     :                     , 0.0, 1.0)

         !    sorg_tt

      call read_real_array (section_name
     :                     , 'x_temp', max_table, '(oC)'
     :                     , c%x_temp, c%num_temp
     :                     , -10.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_tt', max_table, '(oC)'
     :                     , c%y_tt, c%num_temp
     :                     , -10.0, 100.0)

      call read_real_var (section_name
     :                   , 'tt_base', '()'
     :                   , c%tt_base, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'tt_opt', '()'
     :                   , c%tt_opt, numvals
     :                   , 0.0, 100.0)

!cpsc
      call read_real_array (section_name
     :                     , 'x_weighted_temp', max_table, '(oC)'
     :                     , c%x_weighted_temp, c%num_weighted_temp
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_plant_death', max_table, '(oC)'
     :                     , c%y_plant_death, c%num_weighted_temp
     :                     , 0.0, 100.0)

         ! TEMPLATE OPTION
         !    sorg_tt_other

      ! call read_real_array (section_name
      !:                     , 'x_temp_other', max_table, '(oC)'
      !:                     , c%x_temp_other, c%num_temp_other
      !:                     , 0.0, 100.0)

      ! call read_real_array (section_name
      !:                     , 'y_tt_other', max_table, '(oC)'
      !:                     , c%y_tt_other, c%num_temp_other
      !:                     , 0.0, 100.0)

         ! TEMPLATE OPTION
         !    sorg_tt_curv

      ! call read_real_var (section_name
      !:                    , 'imin', '()'
      !:                    , c%imin, numvals
      !:                    , 0.0, 100.0)

      ! call read_real_var (section_name
      !:                    , 'iopt', '()'
      !:                    , c%iopt, numvals
      !:                    , 0.0, 100.0)

      ! call read_real_var (section_name
      !:                    , 'imax', '()'
      !:                    , c%imax, numvals
      !:                    , 0.0, 100.0)

      ! call read_real_var (section_name
      !:                    , 'ioptr', '()'
      !:                    , c%ioptr, numvals
      !:                    , 0.0, 100.0)

      ! call read_real_var (section_name
      !:                    , 'amin', '()'
      !:                    , c%amin, numvals
      !:                    , 0.0, 100.0)

      ! call read_real_var (section_name
      !:                    , 'aopt', '()'
      !:                    , c%aopt, numvals
      !:                    , 0.0, 100.0)

      ! call read_real_var (section_name
      !:                    , 'amax', '()'
      !:                    , c%amax, numvals
      !:                    , 0.0, 100.0)

      ! call read_real_var (section_name
      !:                    , 'aoptr', '()'
      !:                    , c%aoptr, numvals
      !:                    , 0.0, 100.0)

         !    swdef

      call read_real_array (section_name
     :                     , 'x_sw_demand_ratio', max_table, '()'
     :                     , c%x_sw_demand_ratio, c%num_sw_demand_ratio
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_swdef_leaf', max_table, '()'
     :                     , c%y_swdef_leaf, c%num_sw_demand_ratio
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'x_sw_avail_ratio', max_table, '()'
     :                     , c%x_sw_avail_ratio, c%num_sw_avail_ratio
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_swdef_pheno', max_table, '()'
     :                     , c%y_swdef_pheno, c%num_sw_avail_ratio
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'x_sw_ratio', max_table, '()'
     :                     , c%x_sw_ratio, c%num_sw_ratio
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_sw_fac_root', max_table, '()'
     :                     , c%y_sw_fac_root, c%num_sw_ratio
     :                     , 0.0, 100.0)
      call read_real_var (section_name
     :                    , 'nh4_ub', '(kg/ha)'
     :                    , c%Nh4_ub, numvals
     :                    , 0.0, 100000.0)

      call read_real_var (section_name
     :                    , 'nh4_lb', '(kg/ha)'
     :                    , c%Nh4_lb, numvals
     :                    , 0.0, 100000.0)

      call read_real_var (section_name
     :                    , 'nh4_min_ub', '(kg/ha)'
     :                    , c%Nh4_min_ub, numvals
     :                    , 0.0, 100000.0)

      call read_real_var (section_name
     :                    , 'nh4_min_lb', '(kg/ha)'
     :                    , c%Nh4_min_lb, numvals
     :                    , 0.0, 100000.0)


      call pop_routine (my_name)
      return
      end subroutine


* ====================================================================
       subroutine simulation_prepare ()
* ====================================================================
      Use infrastructure
      implicit none

*+  Purpose
*     <insert here>

*+  Changes
*     12-05-1997 - huth - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'sorg_prepare')

*- Implementation Section ----------------------------------
      call push_routine (myname)

!      call Maize_nit_stress(1)
!      call Maize_temp_stress(1)

!      call Maize_light_supply(1)
!      call Maize_bio_RUE(1)
!      call Maize_transpiration_eff(1)
!      call Maize_water_demand(1)
!      call Maize_Nit_demand_est(1)

      call pop_routine (myname)
      return
      end subroutine



