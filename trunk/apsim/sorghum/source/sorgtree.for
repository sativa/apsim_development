C     Last change:  E     6 Aug 2001   12:28 pm
*     ===========================================================
      subroutine Sorg_root_depth (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     root distribution

*+  Changes
*     5/9/96 dph
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Sorg_root_depth')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then
         call cproc_root_depth1 (
     :                              g%dlayer
     :                             ,c%num_sw_ratio
     :                             ,c%x_sw_ratio
     :                             ,c%y_sw_fac_root
     :                             ,g%dul_dep
     :                             ,g%sw_dep
     :                             ,p%ll_dep
     :                             ,c%root_depth_rate
     :                             ,g%current_stage
     :                             ,p%xf
     :                             ,g%dlt_root_depth
     :                             ,g%root_depth
     :                             )

      elseif (Option .eq. 3) then    ! skip
         call cproc_root_depth3 (
     :                              g%dlayer
     :                             ,c%num_sw_ratio
     :                             ,c%x_sw_ratio
     :                             ,c%y_sw_fac_root
     :                             ,g%dul_dep
     :                             ,g%sw_dep
     :                             ,p%ll_dep
     :                             ,c%root_depth_rate
     :                             ,g%current_stage
     :                             ,p%xf
     :                             ,g%row_spacing
     :                             ,g%skip_row_fac
     :                             ,g%dlt_root_front
     :                             ,g%dlt_root_depth
     :                             ,g%root_front
     :                             ,g%root_depth
     :                             )
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Sorg_root_depth_init (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     root distribution

*+  Changes
*     5/9/96 dph
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Sorg_root_depth_init')
      character  string*200            ! output string

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call cproc_root_depth_init1
     :               (
     :                c%initial_root_depth
     :              , g%current_stage
     :              , germ
     :              , g%days_tot
     :              , g%dlt_root_depth
     :               )

      elseif (Option .eq. 2) then

         call cproc_root_depth_init1
     :               (
     :                g%sowing_depth
     :              , g%current_stage
     :              , germ
     :              , g%days_tot
     :              , g%dlt_root_depth
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Sorg_water_supply (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     Soil water supply

*+  Changes
*     5/9/96 dph
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Sorg_water_supply')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Option .eq. 1) then

         call cproc_sw_supply1 (
     :                 c%minsw
     :                ,g%dlayer
     :                ,p%ll_dep
     :                ,g%dul_dep
     :                ,g%sw_dep
     :                ,g%num_layers
     :                ,g%root_depth
     :                ,p%kl
     :                ,g%sw_avail
     :                ,g%sw_avail_pot
     :                ,g%sw_supply
     :                )
      elseif (Option .eq. 2) then      ! skip

         call cproc_sw_supply2 (
     :                 c%minsw
     :                ,g%dlayer
     :                ,p%ll_dep
     :                ,g%dul_dep
     :                ,g%sw_dep
     :                ,g%num_layers
     :                ,g%root_depth
     :                ,g%root_front
     :                ,p%kl
     :                ,g%skip_row_fac
     :                ,g%row_spacing
     :                ,g%sw_avail
     :                ,g%sw_avail_pot
     :                ,g%sw_supply
     :                )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Sorg_water_stress(Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option       ! (INPUT) option number

*+  Purpose
*     Get current water stress factors (0-1)

*+  Changes
*     010994 jngh specified and programmed
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*) ! name of procedure
      parameter (my_name = 'crop_water_stress')
      character  string*200            ! output string

*+  Local Variables
      real ext_sw_supply (max_layer) ! external sw supply (mm)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Option .eq. 1) then
         if (p%uptake_source .eq. 'apsim') then
            ! this would have been avoided if we have
            ! each stress factor in its own routine! - NIH
            ! photo requires (really) actually water uptake
            ! but expansion requires pot water uptake.
            ! we only have one supply variable.

            call crop_get_ext_uptakes(
     :                 p%uptake_source   ! uptake flag
     :                ,c%crop_type       ! crop type
     :                ,'water'           ! uptake name
     :                ,1.0               ! unit conversion factor
     :                ,0.0               ! uptake lbound
     :                ,100.0             ! uptake ubound
     :                ,ext_sw_supply     ! uptake array
     :                ,max_layer         ! array dim
     :                )
            call crop_swdef_photo(max_layer, g%dlayer, g%root_depth,
     :           g%sw_demand, ext_sw_supply, g%swdef_photo)
         else
            call crop_swdef_photo(max_layer, g%dlayer, g%root_depth,
     :           g%sw_demand, g%sw_supply, g%swdef_photo)
         endif

         call crop_swdef_expansion(c%num_sw_demand_ratio,
     :        c%x_sw_demand_ratio, c%y_swdef_leaf, max_layer, g%dlayer,
     :        g%root_depth,g%sw_demand, g%sw_supply, g%swdef_expansion)
         call crop_swdef_pheno(c%num_sw_avail_ratio,
     :        c%x_sw_avail_ratio, c%y_swdef_pheno, max_layer, g%dlayer,
     :        g%root_depth, g%sw_avail, g%sw_avail_pot, g%swdef_pheno)
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sorg_nit_stress(Option)
*     ===========================================================
      Use infrastructure
      implicit none


*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*     Get current Nitrogen stress factors (0-1)

*+  Changes
*     010994 jngh specified and programmed
*     970225 slw modified to split stress factors

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Sorg_nit_stress')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Option .eq. 1) then

         call crop_nfact_pheno(leaf, stem, g%dm_green,
     .                         g%N_conc_crit,
     .                         g%N_conc_min,
     .                         g%N_green,
     .                         c%N_fact_pheno, g%nfact_pheno)
         call crop_nfact_photo(leaf, stem,
     .                     g%dm_green,
     .                     g%N_conc_crit,
     .                     g%N_conc_min,
     .                     g%N_green,
     .                     c%N_fact_photo, g%nfact_photo)
         call crop_nfact_grain_conc(leaf, stem,
     .                     g%dm_green,
     .                     g%N_conc_crit,
     .                     g%N_conc_min,
     .                     g%N_green, g%nfact_grain_conc)
         call crop_nfact_expansion(leaf,
     .                     g%dm_green,
     .                     g%N_conc_crit,
     .                     g%N_conc_min,
     .                     g%N_green,
     .                     c%N_fact_expansion,
     .                     g%nfact_expansion)

      else if (Option .eq. 400) then

         call crop_nfact_pheno(leaf, stem, g%dm_green,
     .                         g%N_conc_crit,
     .                         g%N_conc_min,
     .                         g%N_green,
     .                         c%N_fact_pheno, g%nfact_pheno)

         call crop_nfact_photo(leaf, stem,
     .                     g%dm_green,
     .                     g%N_conc_crit,
     .                     g%N_conc_min,
     .                     g%N_green,
     .                     c%N_fact_photo, g%nfact_photo)
         call crop_nfact_grain_conc(leaf, stem,
     .                     g%dm_green,
     .                     g%N_conc_crit,
     .                     g%N_conc_min,
     .                     g%N_green, g%nfact_grain_conc)
         call crop_nfact_expansion(leaf,
     .                     g%dm_green,
     .                     g%N_conc_crit,
     .                     g%N_conc_min,
     .                     g%N_green,
     .                     c%N_fact_expansion,
     .                     g%nfact_expansion)

      else if (Option .eq. 401) then
        g%nfact_pheno = g%nfact_expansion
        g%nfact_grain_conc = 1

         call sorg_nfact_photo(leaf,
     .                     g%lai,
     .                     g%N_green,
     .                     g%nfact_photo)
          g%nfact_expansion = g%nfact_photo
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Sorg_temp_stress(Option)
*     ===========================================================
      Use infrastructure
      implicit none
!      include 'stress.inc'

*+  Sub-Program Arguments
      integer Option         ! (INPUT) option number

*+  Purpose
*     Get current temperature stress factors (0-1)

*+  Changes
*     010994 jngh specified and programmed
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*) ! name of procedure
      parameter (my_name = 'Sorg_temp_stress')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Option .eq. 1) then
          call crop_temperature_stress_photo
     :               (c%num_ave_temp, c%x_ave_temp, c%y_stress_photo,
     :                g%maxt, g%mint,  g%temp_stress_photo)
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sorg_light_supply (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     light supply

*+  Changes
*     5/9/96 dph
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sorg_light_supply')

*+  Local Variables
      real extinct_coef

*- Implementation Section ----------------------------------
      call push_routine (my_name)

       extinct_coef = linear_interp_real (g%row_spacing
     :                                  ,c%x_row_spacing
     :                                  ,c%y_extinct_coef
     :                                  ,c%num_row_spacing)

      if (Option .eq. 400) then

         g%cover_green = 1.0 - exp (-extinct_coef*g%lai)

         call crop_radn_int0(g%cover_green,
     :                     g%fr_intc_radn, g%radn, g%radn_int)

      elseif (Option .eq. 2) then       !  Skip row gmc
         g%cover_green =
     :    divide(1.0 - exp(-extinct_coef*g%lai*g%skip_row_fac),
     :                           g%skip_row_fac,0.0)
         call crop_radn_int0(g%cover_green,
     :                     g%fr_intc_radn, g%radn, g%radn_int)
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Sorg_bio_RUE (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     biomass light

*+  Changes
*     5/9/96 dph
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Sorg_bio_RUE')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then
         ! potential by photosynthesis

         call crop_dm_pot_rue(
     .          g%current_stage,
     .          c%rue,
     .          g%radn_int,
     .          g%temp_stress_photo,
     .          min(g%nfact_photo, PlantP_pfact_photo()),
     .          g%dlt_dm_light)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Sorg_transpiration_eff (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*     Calculate today's transpiration efficiency from min and max
*     temperatures and converting mm water to g dry matter
*     (g dm/m^2/mm water)

*+  Changes
*     5/9/96 dph
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Sorg_transpiration_eff')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call cproc_transp_eff1(
     :               c%svp_fract
     :             , c%transp_eff_cf
     :             , g%current_stage
     :             , g%maxt
     :             , g%mint
     :             , g%transp_eff
     :             )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Sorg_water_demand (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     Soil water demand

*+  Changes
*     5/9/96 dph
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Sorg_water_demand')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call cproc_sw_demand1(
     :           g%dlt_dm_light
     :         , g%transp_eff
     :         , g%sw_demand
     :         )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sorg_phenology_init (option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer option

*+  Purpose
*     Initialise crop growth stage targets

*+  Changes
*     240498 igh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sorg_phenology_init')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (option .eq. 1)  then

      call sorg_phen_init (
!     .          germ_stage,
!     .          emerg_stage,
!     .          begin_PP_sensitive_stage, !end_juv
!     .          germ_stage,

     .          g%current_stage,
     .          g%days_tot,
     .          c%shoot_lag,
     .          g%sowing_depth,
     .          c%shoot_rate,
     .          p%tt_emerg_to_endjuv,
     .          p%tt_endjuv_to_init,
     .          g%day_of_year,
     .          g%latitude,
     .          c%twilight,
     .          p%photoperiod_crit1,
     .          p%photoperiod_crit2,
     .          p%photoperiod_slope,
     .          g%leaf_no_final,
     .          c%leaf_no_rate_change,
     .          c%leaf_no_at_emerg,
     .          c%leaf_app_rate1,
     .          c%leaf_app_rate2,
     .          p%tt_flag_to_flower,
     .          p%tt_flower_to_start_grain,
     .          p%tt_flower_to_maturity,
     .          p%tt_maturity_to_ripe,
     .          g%phase_tt)
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



* ====================================================================
       subroutine sorg_phenology (Option)
* ====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*     <insert here>

*+  Changes
*     <insert here>

      real stress_pheno

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'sorg_phenology')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      If (Option .eq. 1) then

c      call sproc_phenology1 (
c     .       g%previous_stage,
c     .       g%current_stage,
c
c     .       g%maxt, g%mint,
c     .       c%x_temp, c%y_tt,
c     .       c%num_temp, g%dlt_tt,
c
c     :       c%num_sw_avail_ratio,
c     :       c%x_sw_avail_ratio, c%y_swdef_pheno, max_layer,g%dlayer,
c     :       g%root_depth, g%sw_avail, g%sw_avail_pot, g%swdef_pheno,
c
c     .       leaf, stem, g%dm_green,
c     .       g%N_conc_crit, g%N_conc_min, g%N_green,
c     .       c%N_fact_pheno, g%nfact_pheno,
c
c     .          g%current_stage,
c     .          g%days_tot,
c     .          c%shoot_lag,
c     .          g%sowing_depth,
c     .          c%shoot_rate,
c     .          p%tt_emerg_to_endjuv,
c     .          p%tt_endjuv_to_init,
c     .          g%day_of_year,
c     .          g%latitude,
c     .          c%twilight,
c     .          p%photoperiod_crit1,
c     .          p%photoperiod_crit2,
c     .          p%photoperiod_slope,
c     .          g%leaf_no_final,
c     .          c%leaf_no_rate_change,
c     .          c%leaf_no_at_emerg,
c     .          c%leaf_app_rate1,
c     .          c%leaf_app_rate2,
c     .          g%tt_tot,
c     .          p%tt_flag_to_flower,
c     .          p%tt_flower_to_start_grain,
c     .          p%tt_flower_to_maturity,
c     .          p%tt_maturity_to_ripe,
c     .          g%phase_tt,
c
c     .          g%dlayer,
c     .          g%sw_dep,
c     .          p%ll_dep,
c     .          c%pesw_germ,
c     .          g%days_tot,
c     .          g%tt_tot,
c
c     .          g%dlt_stage,
c     .          max_stage,
c
c     :          g%days_tot)

      elseif (Option .eq. 400) then
         stress_pheno = min(g%nfact_pheno, PlantP_pfact_pheno())

!version with 2 thermal times (different for GF)
      call sorg_phenology2 (
     .       g%previous_stage,
     .       g%current_stage,

     .       g%maxt, g%mint,
     .       c%x_temp, c%y_tt,
     .       c%num_temp, g%dlt_tt,

     :       c%num_sw_avail_ratio,
     :       c%x_sw_avail_ratio, c%y_swdef_pheno, g%dlayer,
     :       g%root_depth, g%sw_avail, g%sw_avail_pot, g%swdef_pheno,

     .       g%dm_green,
     .       g%N_conc_crit, g%N_conc_min, g%N_green,
     .       c%N_fact_pheno, stress_pheno,

     .          g%days_tot,
     .          g%sowing_depth,
     .          g%tt_tot,
     .          g%phase_tt,

     .          g%sw_dep,
     .          p%ll_dep,
     .          c%pesw_germ,

     .          g%dlt_stage,

     .          c%tt_base,
     .          c%tt_opt,
     .          g%tt_tot_fm,
     .          g%dlt_tt_fm,
     .          g%sw_supply_demand_ratio)

      elseif (Option .eq. 403) then

!version with 2 thermal times (different for GF)
      call sorg_phenology3 (
     .       g%previous_stage,
     .       g%current_stage,

     .       g%maxt, g%mint,
     .       c%x_temp, c%y_tt,
     .       c%num_temp, g%dlt_tt,

     :       c%num_sw_avail_ratio,
     :       c%x_sw_avail_ratio, c%y_swdef_pheno, g%dlayer,
     :       g%root_depth, g%sw_avail, g%sw_avail_pot, g%swdef_pheno,

     .       g%dm_green,
     .       g%N_conc_crit, g%N_conc_min, g%N_green,
     .       c%N_fact_pheno, g%nfact_pheno,

     .          g%days_tot,
     .          c%shoot_lag,
     .          g%sowing_depth,
     .          c%shoot_rate,
     .          p%tt_emerg_to_endjuv,
     .          p%tt_endjuv_to_init,
     .          g%day_of_year,
     .          g%latitude,
     .          c%twilight,
     .          p%photoperiod_crit1,
     .          p%photoperiod_crit2,
     .          p%photoperiod_slope,
     .          g%leaf_no_final,
     .          c%leaf_no_rate_change,
     .          c%leaf_no_at_emerg,
     .          c%leaf_app_rate1,
     .          c%leaf_app_rate2,
     .          g%tt_tot,
     .          p%tt_flag_to_flower,
     .          p%tt_flower_to_start_grain,
     .          p%tt_flower_to_maturity,
     .          p%tt_maturity_to_ripe,
     .          g%phase_tt,

     .          g%sw_dep,
     .          p%ll_dep,
     .          c%pesw_germ,

     .          g%dlt_stage,

     .          c%tt_base,
     .          c%tt_opt,
     .          g%tt_tot_fm,
     .          g%dlt_tt_fm,
     .          g%sw_supply_demand_ratio)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (myname)
      return
      end subroutine



*     ===========================================================
      subroutine Sorg_water_uptake (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     Soil water uptake

*+  Changes
*     5/9/96 dph
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Sorg_water_uptake')

*+  Local Variables
      integer    deepest_layer
      integer    layer                 ! layer number of profile ()
      real       ext_sw_supply(max_layer)

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (p%uptake_source .eq. 'apsim') then
         call crop_get_ext_uptakes(
     :                 p%uptake_source   ! uptake flag
     :                ,c%crop_type       ! crop type
     :                ,'water'           ! uptake name
     :                ,1.0               ! unit conversion factor
     :                ,0.0               ! uptake lbound
     :                ,100.0             ! uptake ubound
     :                ,ext_sw_supply     ! uptake array
     :                ,max_layer         ! array dim
     :                )

         do 100 layer = 1, g%num_layers
            g%dlt_sw_dep(layer) = -ext_sw_supply(layer)
  100    continue


      elseif (Option .eq. 1) then

         call crop_sw_uptake0(max_layer, g%dlayer, g%root_depth,
     :              g%sw_demand, g%sw_supply, g%dlt_sw_dep)

      elseif (Option .eq. 2) then

         deepest_layer = find_layer_no
     :                   (g%root_depth, g%dlayer, max_layer)
         g%sw_supply_sum = sum_real_array (g%sw_supply, deepest_layer)
         g%sw_supply_demand_ratio = divide(g%sw_supply_sum
     :                                           , g%sw_demand,0.0)


         call cproc_sw_uptake1(
     :            max_layer,
     :            g%dlayer,
     :            g%root_depth,
     :            g%sw_demand,
     :            g%sw_supply,
     :            g%dlt_sw_dep)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Sorg_nit_supply (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*      n supply

*+  Changes
*     5/9/96 dph

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Sorg_nit_supply')

*+  Local Variables
      real    fixation_determinant

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (p%uptake_source .eq. 'apsim') then
         ! do nothing here for now
         ! I assume that the retrans routine does not need the
         ! call below as it is called on its own from process routine.
         ! -NIH

      elseif (Option .eq. 1) then

         fixation_determinant = sum_real_array(g%dm_green, max_part)
     :                        - g%dm_green(root)

         call cproc_n_supply1 (
     :            g%dlayer
     :          , max_layer
     :          , g%dlt_sw_dep
     :          , g%NO3gsm
     :          , g%NO3gsm_min
     :          , g%root_depth
     :          , g%sw_dep
     :          , g%NO3gsm_mflow_avail
     :          , g%sw_avail
     :          , g%NO3gsm_diffn_pot
     :          , g%current_stage
     :          , c%n_fix_rate
     :          , fixation_determinant
     :          , g%swdef_fixation
     :          , g%N_fix_pot
     :          )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Sorg_nit_init (Option)
*     ===========================================================
      Use infrastructure
      implicit none


*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*      Initialise plant nitrogen.

*+  Changes
*     250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Sorg_nit_init')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call cproc_N_init1
     :               (
     :                c%n_init_conc
     :              , max_part
     :              , emerg
     :              , g%current_stage
     :              , g%days_tot
     :              , g%dm_green
     :              , g%N_green
     :               )

      elseif (Option .eq. 2) then

         call sorg_N_init1
     :               (
     :                C%n_init_conc
     :              , max_part
     :              , G%dm_green
     :              , g%lai
     :              , g%plants
     :              , g%N_green
     :               )
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Sorg_height (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*       canopy height

*+  Changes
*      5/9/96 dph

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Sorg_height')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call cproc_canopy_height
     :               (
     :                g%canopy_height
     :              , p%x_stem_wt
     :              , p%y_height
     :              , p%num_stem_wt
     :              , g%dm_green
     :              , g%plants
     :              , stem
     :              , g%dlt_canopy_height
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Sorg_leaf_no_init (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*

*+  Changes
*      28/4/98 igh

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Sorg_leaf_no_init')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call cproc_leaf_no_init1
     :               (
     :                c%leaf_no_at_emerg
     :              , g%current_stage
     :              , emerg
     :              , g%days_tot
     :              , g%leaf_no
     :              , g%node_no
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



* ====================================================================
       subroutine sorg_leaf_no_pot (Option)
* ====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*     <insert here>

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'sorg_leaf_number')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      If (Option .eq. 400) then

      call sorg_leaf_number1(
     .          g%current_stage,
     .          g%days_tot,
     .          g%phase_tt,

     .          c%leaf_init_rate,
     .          c%leaf_no_seed,
     .          c%leaf_no_min,
     .          c%leaf_no_max,
     .          g%leaf_no_final,
     .          g%leaf_no,
     .          c%leaf_no_rate_change,
     .          c%leaf_app_rate2,
     .          c%leaf_app_rate1,
     .          g%dlt_tt,
     .          g%dlt_leaf_no,
     .          g%node_no)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine sorg_leaf_area_pot (Option)
* ====================================================================
      Use infrastructure
      implicit none


*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*     <insert here>

*+  Changes
*     <insert here>

*+  Local variables
      real tpla_max

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'sorg_leaf_area_pot')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      If (Option .eq. 1) then

      call sproc_leaf_area_pot2 (
     .          g%days_tot,
     .          g%current_stage,
     .          g%leaf_no_final,
     .          c%initial_tpla,
     .          c%leaf_no_rate_change,
     .          c%leaf_no_at_emerg,
     .          c%leaf_app_rate1,
     .          c%leaf_app_rate2,
     .          p%tt_flag_to_flower,
     .          g%tiller_no_fertile,
     .          c%tiller_coef,
     .          p%main_stem_coef,
     .          g%tt_tot,
     .          c%tpla_inflection_ratio,
     .          g%tpla_today,
     .          g%tpla_yesterday,
     .          p%tpla_prod_coef,
     .          g%plants,
     .          g%lai,
     .          g%dlt_lai_pot)


      elseif (Option .eq. 2) then

!converted to new routine

      call cproc_tpla_max (
     .          g%leaf_no_final,
     .          g%tiller_no_fertile,
     .          c%tiller_coef,
     .          p%main_stem_coef,
     .          tpla_max)

      call cproc_leaf_area_pot_tpla (
     .          emerg,
     .          flag_leaf,
     .          now,
     .          g%phase_tt,
     .          g%tt_tot,
     .          g%days_tot,
     .          g%current_stage,
     .          c%initial_tpla,
     .          tpla_max,
     .          c%tpla_inflection_ratio,
     .          g%tpla_today,
     .          g%tpla_yesterday,
     .          p%tpla_prod_coef,
     .          g%plants,
     .          g%lai,
     .          g%dlt_lai_pot)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (myname)
      return
      end subroutine



*     ===========================================================
      subroutine Sorg_leaf_area_stressed (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) option number

*+  Purpose
*     Calculate the stressed dlt_lai from the potential dlt_lai.

*+  Changes
*     26/2/96  sb made it up.

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sorg_leaf_area_stressed')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

!stress factors should really be called again here!!!

         call cproc_leaf_area_stressed1 (
     :              g%dlt_lai_pot
     :             ,g%swdef_expansion
     :             ,min(g%nfact_expansion, PlantP_pfact_expansion())
     :             ,g%dlt_lai_stressed
     :             )

      else

         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Sorg_bio_TE (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*      bio water

*+  Changes
*     5/9/96 dph
*     970317 slw new template form

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Sorg_bio_TE')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Option .eq. 1) then

         call cproc_bio_water1(
     .           max_layer
     .         , g%dlayer
     .         , g%root_depth
     .         , g%sw_supply
     .         , g%transp_eff
     .         , g%dlt_dm_water
     .         )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sorg_bio_init (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     bio actual

*+  Changes
*     5/9/96 dph
*     970317 slw new template form

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Sorg_bio_actual')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then
         ! use whichever is limiting

         !scc need to feed this back to N/leaf area interaction
         !Note that dlt_dm_light is w. RUE as limited by temperature and Nfac
         call sorg_dm_init (g%current_stage,
     .          g%days_tot,
     .          c%dm_root_init,
     .          g%plants,
     .          c%dm_stem_init,
     .          c%dm_leaf_init,
     .          c%stem_trans_frac,
     .          c%leaf_trans_frac,
     .          g%dm_green, g%dm_plant_min)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sorg_bio_actual (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     bio actual

*+  Changes
*     5/9/96 dph
*     970317 slw new template form

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Sorg_bio_actual')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then
         ! use whichever is limiting

         !scc need to feed this back to N/leaf area interaction
         !Note that dlt_dm_light is w. RUE as limited by temperature and Nfac

         g%dlt_dm = min (g%dlt_dm_light, g%dlt_dm_water)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Sorg_bio_grain_demand_stress (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Simulate crop grain biomass demand stress factor

*+  Changes
*      280598 nih specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Sorg_bio_grain_demand_Stress')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

        g%swdef_photo = divide(g%sw_supply_sum,g%sw_demand,1.0)

        if(g%swdef_photo .ge.1.0) then
           g%swdef_photo = 1.0
        endif

         call cproc_yieldpart_demand_stress1
     :               (
     :                g%nfact_photo
     :              , g%swdef_photo
     :              , g%temp_stress_photo
     :              , g%dlt_dm_stress_max
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Sorg_bio_grain_demand (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Simulate crop grain biomass demand.

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Sorg_bio_grain_demand')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then
         call cproc_bio_yieldpart_demand1
     :               (
     :                g%current_stage
     :              , flag_leaf ! Start Stress_stage
     :              , start_grain_fill
     :              , maturity
     :              , grain
     :              , root
     :              , max_part
     :              , g%dlt_dm
     :              , g%dm_green
     :              , g%dm_senesced
     :              , g%days_tot
     :              , g%dm_stress_max
     :              , p%hi_incr
     :              , p%x_hi_max_pot_stress
     :              , p%y_hi_max_pot
     :              , p%num_hi_max_pot
     :              , g%dlt_dm_grain_demand
     :               )
      else if (Option .eq. 2) then
           call sorg_dm_grain_source_sink (
     .          c%stem_trans_frac,
     .          c%leaf_trans_frac,
     .          g%current_stage,
     .          g%days_tot,
     .          g%dlt_dm,
     .          g%dlt_dm_grain_demand,
     .          g%grain_no,
     .          g%dlt_tt_fm,
     .          p%tt_flower_to_start_grain,
     .          p%tt_flower_to_maturity,
     .          g%dm_green,
     .          g%dm_dead,
     .          p%dm_per_seed,
     .          g%dm_green_tot_fi)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



* ====================================================================
       subroutine sorg_bio_partition (Option)
* ====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*     <insert here>

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'sorg_bio_partition')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      If (Option .eq. 400) then

      call sproc_bio_partition1 (
     .          g%current_stage,
     .          c%ratio_root_shoot,
     .          g%dlt_dm,
     .          g%leaf_no,
     .          c%partition_rate_leaf,
     .          g%dlt_lai_stressed,
     .          c%sla_min,
     .          c%frac_stem2flower,
     .          g%dlt_dm_grain_demand,
     .          g%dlt_dm_green)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (myname)
      return
      end subroutine



*     ===========================================================
      subroutine Sorg_bio_retrans (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     bio retrans

*+  Changes
*     5/9/96 dph
*     970317 slw new template form

*+  Constant Values
      integer num_supply_pools
      parameter (num_supply_pools = 2)
*
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Sorg_bio_retrans')

*+  Local Variables
      integer supply_pools(num_supply_pools)
      data supply_pools /stem,leaf/
      save /supply_pools/

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call cproc_dm_retranslocate1
     :               (
     :                g%current_stage
     :              , start_grain_fill
     :              , maturity
     :              , grain
     :              , max_part
     :              , supply_pools
     :              , num_supply_pools
     :              , g%dlt_dm_grain_demand
     :              , g%dlt_dm_green
     :              , g%dm_green
     :              , g%dm_plant_min
     :              , g%plants
     :              , g%dlt_dm_green_retrans
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



* ====================================================================
       subroutine sorg_leaf_area_actual (Option)
* ====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*     <insert here>

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'sorg_leaf_area_actual')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      If (Option .eq. 400) then

!old version
      call sproc_leaf_area_actual1 (
     .          g%current_stage,
     .          g%dlt_lai,
     .          g%dlt_lai_stressed,
     .          g%dlt_dm_green,
     .          c%sla_max
     .          )

      elseif (Option .eq. 3) then

!this is the one that we would probably use....
!         leaf_no_now = sum_between(sowing, now, g%leaf_no)
!         interp_sla_max = linear_interp_real(leaf_no_now
!     .                                      ,c%x_leaf_no
!     .                                      ,c%leaf_no_sla_max
!     .                                      ,c%num_x_leaf_no)

!         call maize_leaf_area1 (
!     .          g%current_stage,
!     .          g%dlt_lai,
!     .          g%dlt_lai_stressed,
!     .          g%dlt_dm_green,
!     .          interp_sla_max)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (myname)
      return
      end subroutine



*     ===========================================================
      subroutine Sorg_root_length_init (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     root length initialisation

*+  Changes
*     5/9/96 dph
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Sorg_root_length_init')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call cproc_root_length_init1 (
     :                emerg
     :               ,g%current_stage
     :               ,g%days_tot
     :               ,g%dm_green(root)
     :               ,c%specific_root_length
     :               ,g%root_depth
     :               ,g%dlayer
     :               ,g%root_length
     :               ,max_layer)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Sorg_root_dist (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Plant root distribution calculations

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Sorg_root_dist')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Option .eq. 1) then

         call cproc_root_length_growth1
     :               (
     :                c%specific_root_length
     :              , g%dlayer
     :              , g%dlt_dm_green(root)
     :              , g%dlt_root_length
     :              , g%dlt_root_depth
     :              , g%root_depth
     :              , g%root_length
     :              , g%plants
     :              , p%xf
     :              , c%num_sw_ratio
     :              , c%x_sw_ratio
     :              , c%y_sw_fac_root
     :              , c%x_plant_rld
     :              , c%y_rel_root_rate
     :              , c%num_plant_rld
     :              , g%dul_dep
     :              , g%sw_dep
     :              , p%ll_dep
     :              , max_layer
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Sorg_sen_bio (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     biomass senescence

*+  Changes
*      5/9/96 dph
*     970318 slw new template version

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Sorg_sen_bio')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Option .eq. 1) then
         call crop_dm_senescence0(max_part, root, leaf, stem,
     .          c%dm_leaf_sen_frac,
     .          c%dm_root_sen_frac,
     .          g%dlt_dm_green,
     .          g%dlt_dm_green_retrans,
     .          g%dlt_lai,
     .          g%dlt_slai,
     .          g%dm_green,
     .          g%lai,
     .          g%dlt_dm_senesced,
     .          g%dlt_dm_sen_retrans)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Sorg_sen_root_length (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Simulate plant nitrogen senescence.

*+  Changes
*      091294 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Sorg_sen_root_length')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call cproc_root_length_senescence1
     :               (
     :               c%specific_root_length
     :              , g%dlayer
     :              , g%dlt_dm_senesced (root)
     :              , g%root_length
     :              , g%root_depth
     :              , g%dlt_root_length_senesced
     :              , max_layer
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Sorg_sen_nit (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     biomass senescence
      real LAI, SLN

*+  Changes
*     5/9/96 dph
*     970318 slw new template version

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Sorg_sen_nit')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Option .eq. 1) then

         call cproc_N_senescence1 (max_part
     :                              , c%n_sen_conc
     :                              , g%dlt_dm_senesced
     :                              , g%n_green
     :                              , g%dm_green
     :                              , g%dlt_N_senesced)

      else if (Option .eq. 400) then
         LAI = g%lai + g%dlt_lai - g%dlt_slai
         if (LAI .gt. 0.0) then
            SLN = divide(g%n_green(leaf), LAI, 0.0)
         else
            SLN = 0.0
         endif
         call sorg_N_senescence1 (max_part, LAI
     :                              , c%n_sen_conc
     :                              , g%dlt_dm_senesced
     :                              , g%n_green
     :                              , g%dm_green
     :                              , g%swdef_expansion
     :                              , g%dlt_N_senesced)


      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



* ====================================================================
       subroutine sorg_N_retranslocate (Option)
* ====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*     <insert here>

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'sorg_N_retranslocate')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      If (Option .eq. 400) then

      call sproc_N_retranslocate1 (
     .          g%dlt_dm_green,
     .          g%maxt,
     .          g%mint,
     .          c%temp_fac_min,
     .          c%tfac_slope,
     .          c%sw_fac_max,
     .          c%sfac_slope,
     .          g%N_conc_min,
     .          g%N_conc_crit,
     .          g%dm_green,
     .          g%N_green,
     .          g%N_conc_max,
     .          g%swdef_expansion,
     .          g%nfact_grain_conc,
     .          g%dlt_N_retrans)
      else If (Option .eq. 401) then    !SORGHUM

      call sorg_N_retranslocate1 (
     .          g%N_demand,g%dlt_N_senesced,
     .          g%NFract,
     .          g%lai, g%dlt_lai, g%dlt_slai,
     .          G%n_green, g%dlt_N_green,
     .          G%phase_tt,g%tt_tot_fm,g%dlt_tt_fm,
     .          g%nfact_expansion,
     :          G%dlt_dm_green,
     :          G%dm_green,
     .          g%current_stage,
     .          g%dlt_N_retrans)


      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (myname)
      return
      end subroutine



*     ===========================================================
      subroutine Sorg_nit_demand (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     n demand

*+  Changes
*     5/9/96 dph

*+  Constant Values
      integer num_demand_parts
      parameter (num_demand_parts = 4)
*
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Sorg_nit_demand')

*+  Local Variables
      integer demand_parts(num_demand_parts)
      data demand_parts /root,leaf,stem,flower/
      save /demand_parts/

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         ! maize version that works on dm_light
         ! calculate potential new shoot and root growth
         ! need to calculate dm using potential rue not affected by
         ! N and temperature

      elseif (Option .eq. 2) then

       call cproc_N_demand2
     :               (
     :                max_part
     :              , demand_parts
     :              , num_demand_parts
     :              , g%dlt_dm_green
     :              , g%dlt_n_retrans
     :              , g%dm_green
     :              , g%n_conc_crit
     :              , g%n_conc_max
     :              , g%n_green
     :              , g%N_demand, g%N_max
     :               )
       elseif (Option .eq. 3) then
        ! new n routine
       call sorg_N_demand3
     :               (
     :                max_part,
     :                G%dlt_dm_green,
     :                G%dm_green,
     :                G%n_green,
     :                g%lai,
     :                g%dlt_lai,
     :                g%dlt_slai,
     :                G%current_stage,
     :                g%grain_no,
     :                g%plants,
     :                g%tt_tot_fm,
     :                g%dlt_tt_fm,
     :                g%dlt_tt,
     :                g%phase_tt,
     .                c%x_stage_code,
     .                c%y_N_conc_crit_stem,
     .                c%n_target_conc,
     :                g%N_demand
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Sorg_nit_uptake (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     n uptake

*+  Changes
*     5/9/96 dph

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Sorg_nit_uptake')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (p%uptake_source .eq. 'apsim') then
         ! NIH - note that I use a -ve conversion
         ! factor FOR NOW to make it a delta.
         call crop_get_ext_uptakes(
     :                 p%uptake_source   ! uptake flag
     :                ,c%crop_type       ! crop type
     :                ,'no3'             ! uptake name
     :                ,-kg2gm/ha2sm      ! unit conversion factor
     :                ,0.0               ! uptake lbound
     :                ,100.0             ! uptake ubound
     :                ,g%dlt_no3gsm      ! uptake array
     :                ,max_layer         ! array dim
     :                )

      elseif (Option .eq. 1) then

         call cproc_N_uptake1
     :               (
     :                c%no3_diffn_const
     :              , g%dlayer
     :              , max_layer
     :              , g%no3gsm_diffn_pot
     :              , g%no3gsm_mflow_avail
     :              , g%N_fix_pot
     :              , c%n_supply_preference
     :              , g%n_demand
     :              , g%n_max
     :              , max_part
     :              , g%root_depth
     :              , g%dlt_NO3gsm
     :               )
      elseif (Option .eq. 2) then
        ! new N routine
         call sorg_N_uptake2
     :               (
     :                C%no3_diffn_const
     :              , G%dlayer
     :              , G%no3gsm_diffn_pot
     :              , G%no3gsm_mflow_avail
     :              , G%N_fix_pot
     :              , c%n_supply_preference
     :              , G%n_demand
     :              , G%root_depth
     :              , g%NFract
     :              , g%current_stage
     :              , g%dlt_NO3gsm
     :               )


      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



* ====================================================================
       subroutine sorg_N_partition (Option)
* ====================================================================
      Use infrastructure
      implicit none


*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*     <insert here>

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'sorg_N_partition')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      If (Option .eq. 400) then

      call sproc_N_partition1(
     .          g%root_depth,
     .          g%dlayer,
     .          g%N_demand,
     .          g%N_max,
     .          g%dlt_NO3gsm,
     .          g%dlt_N_green
     .                     )
      else If (Option .eq. 401) then       ! SORGHUM

      call sorg_N_partition1(
     .          g%N_demand,
     .          G%NFract,
     .          G%dlt_N_green
     .                     )


      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine sorg_plant_death (Option)
* ====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*     <insert here>

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'sorg_plant_death')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      If (Option .eq. 1) then

      call sproc_plant_death1 (
     .          c%tt_emerg_limit,
     .          g%current_stage,
     .          g%plants,
     .          g%tt_tot,
     .          g%dlt_plants_failure_emergence,

     .          g%lai,
     .          g%dlt_slai,

     .          g%cswd_photo,
     .          g%leaf_no,
     .          c%leaf_no_crit,
     .          c%swdf_photo_limit,
     .          g%swdef_photo,
     .          c%swdf_photo_rate,
     .          g%dlt_plants_death_drought,
     .          g%dlt_plants_dead)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (myname)
      return
      end subroutine



*     ===========================================================
      subroutine Sorg_detachment(option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer option

*+  Purpose
*       Simulate plant detachment.
cscc Detachment is also a function of the environment. We've
c noticed large diff. in detachment between wet and dry environments
c in maize

*+  Changes
*      091294 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Sorg_detachment')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call cproc_dm_detachment1( max_part
     :                              , c%sen_detach_frac
     :                              , g%dm_senesced
     :                              , g%dlt_dm_detached
     :                              , c%dead_detach_frac
     :                              , g%dm_dead
     :                              , g%dlt_dm_dead_detached)

         call cproc_n_detachment1( max_part
     :                              , c%sen_detach_frac
     :                              , g%n_senesced
     :                              , g%dlt_n_detached
     :                              , c%dead_detach_frac
     :                              , g%n_dead
     :                              , g%dlt_n_dead_detached)

         call cproc_lai_detachment1 (leaf
     :                             , c%sen_detach_frac
     :                             , g%slai
     :                             , g%dlt_slai_detached
     :                             , c%dead_detach_frac
     :                             , g%tlai_dead
     :                             , g%dlt_tlai_dead_detached)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sorg_leaf_area_sen (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) template option number

*+  Purpose
*     Estimates leaf area senesence determined by age, frost, light, water
*
*   Called by _process
*
*   Number of options: 2
*
*   Option 1:
*     CERES
*   Calls srop_leaf_area_sen_age1, srop_leaf_area_sen_light1,
*         srop_leaf_area_sen_water1, srop_leaf_area_sen_frost1 in crop.for
*
*   Option 2:
*     Mechanistic versions
*   Calls srop_leaf_area_sen_age2
*         srop_leaf_area_sen_light2, srop_lai_equilib_water
*         srop_lai_equilib_light , srop_leaf_area_sen_water2,
*         srop_leaf_area_sen_frost2 in crop.for

*+  Changes
*     5/9/96 dph
*     970318 slw new template version

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sorg_leaf_area_sen')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

         ! Standard routines derived from Ceres - simpler ?
         !TEMPLATE OPTION alternatives developed by GLH - mechanistic

      if (Option .eq. 1) then
*Following routines are already in CROP.FOR

            call srop_leaf_area_sen_age1 (
     :          emerg, now,
     .          g%leaf_no_dead,
     .          g%dlt_leaf_no_dead,
     .          g%leaf_area,
     .          g%plants,
     .          g%slai,
     .          g%lai,
     .          g%dlt_slai_age)
            call srop_leaf_area_sen_light1 (
     .          c%lai_sen_light,
     .          c%sen_light_slope,
     .          g%lai,
     .          g%dlt_slai_light)
            call srop_leaf_area_sen_water1 (
     .          c%sen_rate_water,
     .          g%lai,
     .          g%swdef_photo,
     .          g%dlt_slai_water)
            call srop_leaf_area_sen_frost1(
     .          c%y_senescence_fac,
     .          c%x_temp_senescence,
     .          c%num_temp_senescence,
     .          g%lai,
     .          g%mint,
     .          g%dlt_slai_frost)

         ! now take largest of deltas
         g%dlt_slai = max (g%dlt_slai_age
     :                 , g%dlt_slai_light
     :                 , g%dlt_slai_water
     :                 , g%dlt_slai_frost)

      else if (Option .eq. 400) then

*check against cropsid.for
*set the lai_max_possible for input to SPLA routines

           call srop_leaf_area_lai_max_possible (
     .          floral_init,
     .          flag_leaf,
     .          harvest_ripe,
     .          g%current_stage,
     .          g%swdef_lai_loss,
     .          g%lai_max_possible,
     .          g%lai,
     .          g%slai,
     .          g%dlt_lai_pot,
     .          g%dlt_lai_stressed)

            call srop_leaf_area_sen_age2 (
     .          g%current_stage,
     .          g%tt_tot,
     .          p%spla_intercept,
     .          c%spla_slope,
     .          g%leaf_no_final,
     .          g%lai_max_possible,
     .          p%spla_prod_coef,
     .          g%slai,
     .          g%days_tot,
     .          g%lai,
     .          g%dlt_lai,
     .          g%dlt_slai_age)

            call srop_lai_equilib_light (
     .          g%radn_int,
     .          g%cover_green,
     .          c%sen_radn_crit,
     .          c%extinction_coef,
     .          g%lai,
     .          g%day_of_year,
     .          g%year,
     .          g%lai_equilib_light)

            call srop_leaf_area_sen_light2 (
     .          g%radn_int,
     .          g%radn,
     .          c%sen_radn_crit,
     .          g%year,
     .          g%day_of_year,
     .          g%lai_equilib_light,
     .          g%lai,
     .          c%sen_light_time_const,
     .          g%dlt_slai_light)

*Water limiting routines... in CROP.FOR
            call srop_lai_equilib_water(
     .          g%day_of_year,
     .          g%year,
     .          c%rue,
     .          g%cover_green,
     .          g%current_stage,
     .          g%lai,
     .          g%nfact_photo,
     .          g%radn,
     .          g%radn_int,
     .          g%sw_supply_sum,
     .          g%temp_stress_photo,
     .          g%transp_eff,
     .          g%lai_equilib_water)

            call srop_leaf_area_sen_water2 (
     .          g%day_of_year,
     .          g%year,
     .          c%sen_threshold,
     .          c%sen_water_time_const,
     :          max_layer,
     .          g%dlayer,
     .          g%lai,
     .          g%lai_equilib_water,
     .          g%root_depth,
     .          g%sw_demand,
     .          g%sw_supply,
     .          g%dlt_slai_water)

*Frost effects in CROP.FOR
            call srop_leaf_area_sen_frost2(
     .          c%frost_kill,
     .          g%lai,
     .          g%mint,
     .          g%dlt_slai_frost)

         ! now take largest of deltas
         g%dlt_slai = max (g%dlt_slai_age
     :                 , g%dlt_slai_light
     :                 , g%dlt_slai_water
     :                 , g%dlt_slai_frost)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine
