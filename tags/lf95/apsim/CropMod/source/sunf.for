C     Last change:  E    24 May 2000   12:25 pm

!       INCLUDE 'CropMod.inc'


*=======================================================================
      subroutine sunf_process ()
*=======================================================================
*+  Purpose
*       Simulate crop processes.  These include biomass production,
*       phenological stages, plant component development,
*       water uptake and nitrogen uptake, and plant senescense.

*+  Changes
*      250894 sc   specified and programmed
*=======================================================================
      use CropModModule
      implicit none
      include   'const.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Calls

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sunf_process')


c      INTEGER num_layers

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
!CROP WATER SUPPLY
c     if (g%plant_status.eq.status_alive) then


      call sunf_root_depth(1)          !CT = crop template
      
      call sunf_root_depth_init(2)     !CT - called later as root_depth sets delta
                                        ! option 1 initial root depth = c%...
                                        ! option 2 initial root depth = sowing depth
      call sunf_water_supply(1)        !CT
      
      call sunf_water_stress(1)        !CT

!CROP WATER DEMAND (following are in PREPARE section for APSWIM version)
!cf sunf light_supply is based on direct calculation of k from rowspacing etc
!rather than looking for cover calculation (that is only done at end of day)
!Problem with other modules is that they use cover_green which is calculated elsewhere!
      
      call sunf_nit_stress(400)        !CT
      
      call sunf_temp_stress(1)         !CT
      
!sunf - testing new version of the k calculation our new test = 500
!      call sunf_light_supply(400)
      
      call sunf_light_supply(500)      !EW already checked
      
      call sunf_bio_RUE(1)             !CT
      
      call sunf_transpiration_eff(1)   !CT No bounding on VPD or TE
      
      call sunf_water_demand(1)        !CT!movable!

*   if (g%plant_status.eq.status_alive) then

!PHENOLOGY
         call sunf_phenology_init(2)     !1 = works with two leaf appearance rates, 2= works with lar lookup table - Enli
         
         call sunf_phenology(400)        !different TT for grainfill, EW not checked yet

!below causes slight variation as cannot control N,water stress separately
!         call sunf_phenology(403)        !different TT for grainfill

!WATER UPTAKE
         call sunf_water_uptake(2)      !CT

c         num_layers = count_of_real_vals (g%dlayer, max_layer)

c        call fill_real_array(g%dlt_sw_dep, -0.2, num_layers)


!CANOPY HEIGHT
         call sunf_height(1)            !CT

!NODE NUMBER and APPEARANCE
         call sunf_leaf_no_init(1)      !CT
         call sunf_leaf_no_pot(500)     !400 - two leaf appearance rates, 500 = lar lookup table - Enli

!LEAF AREA - Potential and stressed
!         call sunf_leaf_area_pot(2)      !TPLA (works using new template)
        
         call sunf_leaf_area_pot(500)      !TPLA (works using new template)
          
         call sunf_leaf_area_stressed(1)!CT

!BIOMASS Water_limited, light_N_temp limited, actual
         call sunf_bio_TE(1)            !CT delta_bio_water
         call sunf_bio_RUE(1)           !CT delta_bio_light
         call sunf_bio_init(1)           !these 2 routines are together
         call sunf_bio_actual(1)         !same as maize_bio_actual

!ECONOMIC YIELD - demand
         call sunf_bio_grain_demand_stress(1)!CT
        
         call sunf_bio_grain_demand(500) !CT HI approach EW ???? neet to be changed for  critical_temp = 15.0 ????

!         call crop_bio_grain_demand(3)  !local source sink RLV
 
!BIOMASS PARTITIONING and RETRANSLOCATION
!NEW VERSION - based on leaf and head distribution ratios
!as a function of porportional thermal time to anthesis
!         call sunf_bio_partition(400)    !CODE is SAME as maize option(1)
        
         call sunf_bio_partition(500)   !EW we changed this subroutine for sunflower biom partition

         call sunf_bio_retrans(1)       !CT

!ROOT BIOMASS PARTITIONING AND ROOT LENGTH DENSITY
         call sunf_root_length_init(1)  !CT
         call sunf_root_dist(1)         !CT

!LEAF AREA - Actual (biomass limited)
         call sunf_leaf_area_actual(400) !Limits g%dlt_lai by C

!         call sunf_leaf_death(400)       !AGE kill of leaves - not req. for SPLA approach

!SENESCENCE - leaf area, biomass, roots, N
 
         call sunf_leaf_area_sen(400)    !Alternate approach to leaf senesc.
         
!        print *, "g%dlt_leaf_no_dead", g%dlt_leaf_no_dead
         
         call sunf_sen_bio(1)           !CT
         call sunf_sen_root_length(1)   !CT
         call sunf_sen_nit(1)           !CT

!NITROGEN SUPPLY - soil, N_fix, other parts
         call sunf_nit_supply(1)        !CT
         call sunf_nit_init(1)          !CT
         
         call sunf_N_retranslocate(400)  !EW ???? is grain N demand calculation OK
         
         
!NITROGEN DEMAND/UPTAKE/PARTITION
         call sunf_nit_demand(2)        !CT
         call sunf_nit_uptake(1)        !CT
         
         call sunf_N_partition(400)      !CODE is SAME as maize


      call pop_routine (my_name)

      return
      end


*     ===========================================================
      subroutine sunf_root_depth (Option)
*     ===========================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'crp_root.pub'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     root distribution

*+  Changes
*     5/9/96 dph
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sunf_root_depth')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 1) then

c      PRINT *, option

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
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')

c      PRINT *, "else=", option

      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sunf_root_depth_init (Option)
*     ===========================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'crp_root.pub'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     root distribution

*+  Changes
*     5/9/96 dph
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sunf_root_depth_init')

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
      end



*     ===========================================================
      subroutine sunf_water_supply (Option)
*     ===========================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'crp_watr.pub'
      include 'error.pub'                         
      include 'data.pub'
      include 'science.pub'

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     Soil water supply

*+  Changes
*     5/9/96 dph
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sunf_water_supply')

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

 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sunf_water_stress(Option)
*     ===========================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'crp_watr.pub'
      include 'crp_comm.pub'                      
      include 'error.pub'                         
      include 'data.pub'
      include 'science.pub'

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

*+  Local Variables
      real ext_sw_supply (max_layer) ! external sw supply (mm)

c      INTEGER    deepest_layer
c      REAL       sw_supply_sum
c      REAL       ratio
c      CHARACTER  string*100


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




c         deepest_layer = find_layer_no (g%root_depth, g%dlayer
c     :                                , max_layer)
c         sw_supply_sum = sum_real_array (g%sw_supply, deepest_layer)
c
c         ratio = divide(sw_supply_sum, g%sw_demand,0.0)

c        write (string, '(3x, i4, 4f7.1)')
c     :                   deepest_layer, sw_supply_sum
c     :                 , g%sw_demand, ratio, g%swdef_expansion
c
c        call write_string(string)



         call crop_swdef_pheno(c%num_sw_avail_ratio,
     :        c%x_sw_avail_ratio, c%y_swdef_pheno, max_layer, g%dlayer,
     :        g%root_depth, g%sw_avail, g%sw_avail_pot, g%swdef_pheno)
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sunf_nit_stress(Option)
*     ===========================================================
      use CropModModule
      implicit none
      include   'const.inc'
      include 'crp_nitn.pub'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*     Get current Nitrogen stress factors (0-1)

*+  Changes
*     010994 jngh specified and programmed
*     970225 slw modified to split stress factors

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sunf_nit_stress')

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
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sunf_temp_stress(Option)
*     ===========================================================
      use CropModModule
      implicit none
      include 'const.inc'
c     include 'stress.inc'
      include 'crp_temp.pub'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option         ! (INPUT) option number

*+  Purpose
*     Get current temperature stress factors (0-1)

*+  Changes
*     010994 jngh specified and programmed
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*) ! name of procedure
      parameter (my_name = 'sunf_temp_stress')

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
      end



*     ===========================================================
      subroutine sunf_light_supply (Option)
*     ===========================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'crp_util.pub'
      include 'error.pub'                         
      include 'science.pub'
      include 'data.pub'

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     light supply

*+  Changes
*     5/9/96 dph
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sunf_light_supply')

*+  Local Variables
      real extinct_coef
      real extinct_coef_at_flowerng
      save extinct_coef_at_flowerng

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 400) then
      
        extinct_coef = linear_interp_real (g%row_spacing
     :                                   ,c%x_row_spacing
     :                                   ,c%y_extinct_coef
     :                                   ,c%num_row_spacing)

        g%cover_green = 1.0 - exp (-extinct_coef*g%lai)
 
        call crop_radn_int0(g%cover_green,
     :                     g%fr_intc_radn, g%radn, g%radn_int)
         
         
      !NEW ADDED PART FOR SUNFLOWER    
      else if (Option .eq. 500) then


      !EW extinction coefficient needs value when lai is small 
      !   extinction coefficient should not increase after anthesis

        if (g%lai.gt.0.0) then
            extinct_coef = 3.76 * g%lai ** (-0.81)   !!!MCW
        else
            extinct_coef = 0.5
        endif
        
        extinct_coef = bound(extinct_coef, 0.4, 1.0)

        if (on_day_of (flowering, g%current_stage, g%days_tot)) 
     :     extinct_coef_at_flowerng = extinct_coef
        
        
        if (stage_is_between (flowering, maturity, g%current_stage))
     :     extinct_coef = extinct_coef_at_flowerng 
      
        
       g%cover_green = 1.0 - exp (-extinct_coef*g%lai)
 
       call crop_radn_int0(g%cover_green,
     :                     g%fr_intc_radn, g%radn, g%radn_int)
 


      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sunf_bio_RUE (Option)
*     ===========================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'crp_biom.pub'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     biomass light

*+  Changes
*     5/9/96 dph
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sunf_bio_RUE')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 1) then
         ! potential by photosynthesis
 
         call crop_dm_pot_rue(
     .          g%current_stage,
     .          c%rue,
     .          g%radn_int,
     .          g%temp_stress_photo,
     .          g%nfact_photo,
     .          g%dlt_dm_light)
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sunf_transpiration_eff (Option)
*     ===========================================================
      use CropModModule
      implicit none
      include     'const.inc'
      include 'crp_watr.pub'
      include 'error.pub'                         

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
      parameter (my_name = 'sunf_transpiration_eff')

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
      end



*     ===========================================================
      subroutine sunf_water_demand (Option)
*     ===========================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'crp_watr.pub'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     Soil water demand

*+  Changes
*     5/9/96 dph
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sunf_water_demand')

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
      end



*     ===========================================================
      subroutine sunf_phenology_init (option)
*     ===========================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'error.pub'

*+  Sub-Program Arguments
      integer option

*+  Purpose
*     Initialise crop growth stage targets

*+  Changes
*     240498 igh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sunf_phenology_init')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (option .eq. 1)  then
 
      
      else if (option .eq. 2)  then 
      
       call sunf_phen_init_new (
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

     .          p%determinate_crop,
     .          p%x_node_num_lar,
     .          p%y_node_lar,

     .          p%tt_fi_to_flag,
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
      end



* ====================================================================
       subroutine sunf_phenology (Option)
* ====================================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'convert.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*     <insert here>

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'sunf_phenology')

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      If (Option .eq. 1) then

      elseif (Option .eq. 400) then
 
!version with 2 thermal times (different for GF)
      call sunf_phenology2 (
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
     .          g%sw_supply_demand_ratio,
     .          p%tt_switch_stage)                                   !<------------- Enli added the switch


      elseif (Option .eq. 403) then
 
!version with 2 thermal times (different for GF)
      call sunf_phenology3 (
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
      end



*     ===========================================================
      subroutine sunf_water_uptake (Option)
*     ===========================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'science.pub'
      include 'data.pub'                          
      include 'crp_watr.pub'                      
      include 'crp_comm.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     Soil water uptake

*+  Changes
*     5/9/96 dph
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sunf_water_uptake')

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
      end



*     ===========================================================
      subroutine sunf_nit_supply (Option)
*     ===========================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'convert.inc'
      include 'data.pub'
      include 'crp_nitn.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*      n supply

*+  Changes
*     5/9/96 dph

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sunf_nit_supply')

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
      end



*     ===========================================================
      subroutine sunf_nit_init (Option)
*     ===========================================================
      use CropModModule
      implicit none
      include   'const.inc'
      include 'crp_nitn.pub'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*      Initialise plant nitrogen.

*+  Changes
*     250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sunf_nit_init')

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
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sunf_height (Option)
*     ===========================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'crp_cnpy.pub'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*       canopy height

*+  Changes
*      5/9/96 dph

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sunf_height')

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
      end



*     ===========================================================
      subroutine sunf_leaf_no_init (Option)
*     ===========================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'crp_cnpy.pub'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*

*+  Changes
*      28/4/98 igh

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sunf_leaf_no_init')

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
      end



* ====================================================================
       subroutine sunf_leaf_no_pot (Option)
* ====================================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'convert.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*     <insert here>

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'sunf_leaf_no_pot')

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      If (Option .eq. 400) then
 
      else if (Option .eq. 500) then
      
      call sunf_leaf_number_new(
     .          emerg,
     .          flag_leaf,

     .          g%current_stage,
     .          g%days_tot,
     .          g%phase_tt,
 
     .          p%determinate_crop,
     .          p%x_node_num_lar,
     .          p%y_node_lar,
     .          p%num_node_lar,

     .          c%leaf_init_rate,
     .          p%rel_leaf_init_rate,
     .          c%leaf_no_seed,
     .          c%leaf_no_min,
     .          c%leaf_no_max,
     .          g%leaf_no_final,
     .          g%leaf_no,

     .          g%dlt_tt,
     .          g%dlt_leaf_no,
     .          g%node_no)
      
      
      else
      
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (myname)
      return
      end



* ====================================================================
       subroutine sunf_leaf_area_pot (Option)
* ====================================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'convert.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*     <insert here>

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'sunf_leaf_area_pot')

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      If (Option .eq. 500) then
!converted to new routine
! For sunflower have to allow leaf growth to continue until anthesis, not flag leaf
! See Rawson and Turner AJAR 1982
 
       call sunf_leaf_area_pot_new(
     .          emerg,
     .          flowering,
     .          now,
     .          g%phase_tt,
 
     .          g%days_tot,
     .          g%current_stage,
     .          g%leaf_no_final,
     .          c%initial_tpla,
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
 
 
 
      elseif (Option .eq. 1) then
 
 
      elseif (Option .eq. 2) then
 

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (myname)
      return
      end



*     ===========================================================
      subroutine sunf_leaf_area_stressed (Option)
*     ===========================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'crp_cnpy.pub'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) option number

*+  Purpose
*     Calculate the stressed dlt_lai from the potential dlt_lai.

*+  Changes
*     26/2/96  sb made it up.

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sunf_leaf_area_stressed')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
!stress factors should really be called again here!!!
 
         call cproc_leaf_area_stressed1 (
     :                       g%dlt_lai_pot
     :                      ,g%swdef_expansion
     :                      ,g%nfact_expansion
     :                      ,g%dlt_lai_stressed
     :                      )
 
      else
 
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sunf_bio_TE (Option)
*     ===========================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'crp_watr.pub'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*      bio water

*+  Changes
*     5/9/96 dph
*     970317 slw new template form

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sunf_bio_TE')

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
      end



*     ===========================================================
      subroutine sunf_bio_init (Option)
*     ===========================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'error.pub'

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     bio actual

*+  Changes
*     5/9/96 dph
*     970317 slw new template form

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sunf_bio_actual')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 1) then
         ! use whichever is limiting
 
         !scc need to feed this back to N/leaf area interaction
         !Note that dlt_dm_light is w. RUE as limited by temperature and Nfac
         call sunf_dm_init (g%current_stage,
     .          g%days_tot,
     .          c%dm_root_init,
     .          g%plants,
     .          c%dm_stem_init,
     .          c%dm_leaf_init,
     :          c%flower_trans_frac,   !added for sunflower
     .          c%stem_trans_frac,
     .          c%leaf_trans_frac,
     .          g%dm_green, g%dm_plant_min)
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sunf_bio_actual (Option)
*     ===========================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'error.pub'

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     bio actual

*+  Changes
*     5/9/96 dph
*     970317 slw new template form

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sunf_bio_actual')

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
      end



*     ===========================================================
      subroutine sunf_bio_grain_demand_stress (Option)
*     ===========================================================
      use CropModModule
      implicit none
      include   'const.inc'
      include 'crp_biom.pub'
      include 'error.pub'                         
      include 'data.pub'

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Simulate crop grain biomass demand stress factor

*+  Changes
*      280598 nih specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sunf_bio_grain_demand_Stress')

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
      end



*     ===========================================================
      subroutine sunf_bio_grain_demand (Option)
*     ===========================================================
      use CropModModule
      implicit none
      include   'const.inc'
      include 'crp_biom.pub'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Simulate crop grain biomass demand.

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sunf_bio_grain_demand')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 500) then
         call sunf_bio_yieldpart_demand1
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
     :              , g%mint
     :              , g%dlt_dm_grain_demand
     :              ,p%x_hi_incr_min_temp              !Enli added the following three variables (lookup tab)
     :              ,p%y_hi_incr_reduct_fac
     :              ,p%mum_hi_incr_min_temp 
     :               )
 
      elseif (Option .eq. 1) then
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
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



* ====================================================================
       subroutine sunf_bio_partition (Option)
* ====================================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'convert.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*     <insert here>

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'sunf_bio_partition')

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
!Should be the old sorghum one.....
      If (Option .eq. 400) then
 
        elseif (Option .eq. 500) then

      call sproc_bio_partition2 (
     .          g%current_stage,
     .          c%ratio_root_shoot,
     .          g%dlt_dm,
     .          g%leaf_no,
     .          c%partition_rate_leaf,
     .          g%dlt_lai_stressed,
     .          c%sla_min,
     .          c%frac_stem2flower,
     :          c%frac_pod2grain,
     :          c%grain_energy,
     .          g%dlt_dm_grain_demand,
     :          g%phase_tt,
     :          g%tt_tot,
     .          g%dlt_dm_green)
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (myname)
      return
      end



*     ===========================================================
      subroutine sunf_bio_retrans (Option)
*     ===========================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'crp_biom.pub'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     bio retrans

*+  Changes
*     5/9/96 dph
*     970317 slw new template form

*+  Constant Values
      integer num_supply_pools
      parameter (num_supply_pools = 3)
*
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sunf_bio_retrans')

*+  Local Variables
      integer supply_pools(num_supply_pools)
      data supply_pools /flower,stem,leaf/
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
      end



* ====================================================================
       subroutine sunf_leaf_area_actual (Option)
* ====================================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'convert.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*     <insert here>

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'sunf_leaf_area_actual')

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
      end



*     ===========================================================
      subroutine sunf_root_length_init (Option)
*     ===========================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'crp_root.pub'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     root length initialisation

*+  Changes
*     5/9/96 dph
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sunf_root_length_init')

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
      end



*     ===========================================================
      subroutine sunf_root_dist (Option)
*     ===========================================================
      use CropModModule
      implicit none
      include   'const.inc'
      include 'crp_root.pub'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Plant root distribution calculations

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sunf_root_dist')

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
      end



*     ===========================================================
      subroutine sunf_sen_bio (Option)
*     ===========================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'crp_biom.pub'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     biomass senescence

*+  Changes
*      5/9/96 dph
*     970318 slw new template version

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sunf_sen_bio')

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
      end



*     ===========================================================
      subroutine sunf_sen_root_length (Option)
*     ===========================================================
      use CropModModule
      implicit none
      include   'const.inc'
      include 'crp_root.pub'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Simulate plant nitrogen senescence.

*+  Changes
*      091294 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sunf_sen_root_length')

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
      end



*     ===========================================================
      subroutine sunf_sen_nit (Option)
*     ===========================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'crp_nitn.pub'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     biomass senescence

*+  Changes
*     5/9/96 dph
*     970318 slw new template version

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sunf_sen_nit')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
         call cproc_N_senescence1 (max_part
     :                              , c%n_sen_conc
     :                              , g%dlt_dm_senesced
     :                              , g%n_green
     :                              , g%dm_green
     :                              , g%dlt_N_senesced)
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



* ====================================================================
       subroutine sunf_N_retranslocate (Option)
* ====================================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'convert.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*     <insert here>

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'sunf_N_retranslocate')

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
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (myname)
      return
      end



*     ===========================================================
      subroutine sunf_nit_demand (Option)
*     ===========================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'crp_nitn.pub'
      include 'error.pub'                         

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
      parameter (my_name = 'sunf_nit_demand')

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
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sunf_nit_uptake (Option)
*     ===========================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'convert.inc'
      include 'crp_nitn.pub'
      include 'crp_comm.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     n uptake

*+  Changes
*     5/9/96 dph

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sunf_nit_uptake')

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
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



* ====================================================================
       subroutine sunf_N_partition (Option)
* ====================================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'convert.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*     <insert here>

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'sunf_N_partition')

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
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (myname)
      return
      end



* ====================================================================
       subroutine sunf_plant_death (Option)
* ====================================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'convert.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*     <insert here>

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'sunf_plant_death')

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      If (Option .eq. 1) then
 
      call sproc_plant_death1 (
     .          c%tt_emerg_limit,
     .          g%current_stage,
     .          g%plants,
     .          g%tt_tot,
     .          g%dlt_plants_all,
 
     .          g%lai,
     .          g%dlt_slai,
 
     .          g%cswd_photo,
     .          g%leaf_no,
     .          c%leaf_no_crit,
     .          c%swdf_photo_limit,
     .          g%swdef_photo,
     .          c%swdf_photo_rate,
     .          g%dlt_plants_water,
     .          g%dlt_plants_dead)
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (myname)
      return
      end



*     ===========================================================
      subroutine sunf_detachment(option)
*     ===========================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'crp_cnpy.pub'
      include 'crp_nitn.pub'                      
      include 'crp_biom.pub'                      
      include 'error.pub'                         

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
      parameter (my_name = 'sunf_detachment')

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
      end



*     ===========================================================
      subroutine sunf_leaf_area_sen (Option)
*     ===========================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'error.pub'

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
      parameter (my_name = 'sunf_leaf_area_sen')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
         ! Standard routines derived from Ceres - simpler ?
         !TEMPLATE OPTION alternatives developed by GLH - mechanistic
 
      if (Option .eq. 1) then
 
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
 
!sunf - modifying the next one

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
     .          g%plants,      
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
      end


C********************************************************************************
C sunflower specific processes
C********************************************************************************


      !NEED TO GENERALISE TEMPLATE VERSION TO DO THIS
!Include Bange thesis effect of minimum temperature reducing p_hi_incr
*     ===========================================================
      subroutine sunf_bio_yieldpart_demand1
     :               (
     :                G_current_stage
     :              , start_stress_stage
     :              , start_grainfill_stage
     :              , end_grainfill_stage
     :              , yield_part
     :              , root_part
     :              , max_part
     :              , G_dlt_dm
     :              , G_dm_green
     :              , G_dm_senesced
     :              , G_days_tot
     :              , G_dm_stress_max
     :              , P_hi_incr
     :              , P_x_hi_max_pot_stress
     :              , P_y_hi_max_pot
     :              , P_num_hi_max_pot
     :              , g_mint
     :              , dlt_dm_yieldpart_demand
     :              ,p_x_hi_incr_min_temp
     :              ,p_y_hi_incr_reduct_fac
     :              ,p_mum_hi_incr_min_temp 
     :               )
*     ===========================================================
      implicit none
c     dll_export cproc_bio_yieldpart_demand1
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      INTEGER    Start_Stress_Stage    ! (INPUT)
      INTEGER    Start_Grainfill_stage ! (INPUT)
      INTEGER    End_Grainfill_Stage   ! (INPUT)
      INTEGER    Yield_part            ! (INPUT)
      INTEGER    Root_part             ! (INPUT)
      INTEGER    max_part              ! (INPUT)
      REAL       G_dlt_dm              ! (INPUT)  the daily biomass production (
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
      REAL       G_dm_senesced(*)      ! (INPUT)  senesced plant dry wt (g/m^2)
      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
      REAL       G_dm_stress_max(*)    ! (INPUT)  sum of maximum daily stress on
      REAL       P_hi_incr             ! (INPUT)  harvest index increment per da
      REAL       P_x_hi_max_pot_stress(*) ! (INPUT) Potential Max HI Stress dete
      REAL       P_y_hi_max_pot(*)     ! (INPUT) Potential Max HI
      INTEGER    P_num_hi_max_pot      ! (INPUT) Number of lookup pairs
      real       dlt_dm_yieldpart_demand ! (OUTPUT) grain dry matter
                                       ! potential (g/m^2)
                                       
      real       g_mint                                       
      real       p_x_hi_incr_min_temp(*)   !(INPUT) minimum temp affecting hi_incr
      real       p_y_hi_incr_reduct_fac(*) !(INPUT) recduction factor of minimum temp on hi_incr
      integer    p_mum_hi_incr_min_temp    !(INPUT) Number of lookup pairs

*+  Purpose
*        Find grain demand for carbohydrate using harvest index (g/m^2)

*+  Mission Statement
*   Calculate yield component biomass demand using harvest index increments

*+  Changes
*     010994 jngh specified and programmed
*     250299 ew   modified and added the lookup table for minT effect

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_bio_yieldpart_demand1')

*+  Local Variables
      real       ave_stress            ! average dm_stress from flowering to gra
      real       stress_sum            ! total    "          "     "      "    "
      real       days_sum              ! total    days       "     "      "    "
      real       dlt_dm_yield          ! grain demand for carbohydrate
                                       ! (g/m^2)
      real       dm_tops               ! drymatter of tops (g/m^2)
      real       harvest_index         ! last harvest index (g grain/g biomass)
      real       hi_max_pot            ! max potential HI due to stress
      real       dm_tops_new           ! new drymatter  tops (g/m^2)
      real       harvest_index_new     ! next harvest index (g grain/g biomass)
      real       dm_grain_new          ! new drymatter grain (g/m^2)

*NEW temperature variables

      real       min_temp_fact_hi_incr
!      real       critical_temp

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (stage_is_between (start_grainfill_stage
     :                    , end_grainfill_Stage
     :                    , g_current_stage)) then
 
         stress_sum = sum_between (start_stress_stage
     :                            ,start_grainfill_stage
     :                            ,g_dm_stress_max)
         days_sum = sum_between (start_stress_stage
     :                          ,start_grainfill_stage
     :                          ,g_days_tot)
         ave_stress = divide (stress_sum, days_sum, 1.0)
         hi_max_pot = linear_interp_Real(ave_stress
     :                                  ,p_x_hi_max_pot_stress
     :                                  ,p_y_hi_max_pot
     :                                  ,p_num_hi_max_pot)
 
            ! effective grain filling period
        
         dm_tops = sum_real_array (g_dm_green, max_part)
     :           - g_dm_green(root_part)
     :           + sum_real_array (g_dm_senesced, max_part)
     :           - g_dm_senesced(root_part)

         harvest_index = divide (g_dm_green(yield_part), dm_tops, 0.0)

         dm_tops_new = dm_tops + g_dlt_dm
 
!================================================================================
!This part is new for sunflower - ENLI
!Include Bange thesis effect of minimum temperature reducing p_hi_incr
 
!NEW BIT - min. temp modifier on p_hi_incr

       min_temp_fact_hi_incr = linear_interp_Real(g_mint
     :                                  ,p_x_hi_incr_min_temp
     :                                  ,p_y_hi_incr_reduct_fac
     :                                  ,p_mum_hi_incr_min_temp)
                
c      print *, "min_temp_fact_hi_incr", min_temp_fact_hi_incr



       harvest_index_new = u_bound (harvest_index + 
     :           p_hi_incr * min_temp_fact_hi_incr, hi_max_pot)
 
!================================================================================

         dm_grain_new = dm_tops_new * harvest_index_new

         dlt_dm_yield = dm_grain_new - g_dm_green(yield_part)

         dlt_dm_yield = bound (dlt_dm_yield, 0.0, dm_grain_new)

 
      else
            ! we are out of grain fill period
 
         dlt_dm_yield = 0.0
      endif
 
      dlt_dm_yieldpart_demand = dlt_dm_yield
 
      call pop_routine (my_name)
      return
      end



C     Last change:  D     9 Sep 1998    2:22 pm
*     ===========================================================
      subroutine sunf_dm_init (
     .          g_current_stage,
     .          g_days_tot,
     .          c_dm_root_init,
     .          g_plants,
     .          c_dm_stem_init,
     .          c_dm_leaf_init,
     :          c_flower_trans_frac,
     .          c_stem_trans_frac,
     .          c_leaf_trans_frac,
     .           dm_green, dm_plant_min)
*     ===========================================================
      implicit none
      include   'CropDefCons.inc'
      include 'science.pub'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
       real g_current_stage
       real g_days_tot(*)
       real c_dm_root_init
       real g_plants
       real c_dm_stem_init
       real c_dm_leaf_init
       real c_flower_trans_frac
       real c_stem_trans_frac
       real c_leaf_trans_frac
       real dm_green(*)           ! (INPUT/OUTPUT) plant part weights
                                       ! (g/m^2)
      real  dm_plant_min(*)       ! (OUTPUT) minimum weight of each
                                       ! plant part (g/plant)

*+  Purpose
*       Initialise plant weights and plant weight minimums
*       at required instances.

*+  Changes
*     010994 jngh specified and programmed
*     970317 slw new template form

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sunf_dm_init')

*+  Local Variables
      real       dm_plant_leaf         ! dry matter in leaves (g/plant)
      real       dm_plant_stem         ! dry matter in stems (g/plant)
      real       dm_plant_flower       ! dry matter in flowers (g/plant) !!added for sunflower
       
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
         ! initialise plant weight
         ! initialisations - set up dry matter for leaf, stem, flower, grain
         ! and root
 
      if (on_day_of (emerg, g_current_stage, g_days_tot)) then
             ! seedling has just emerged.
 
             ! initialise root, stem and leaf.
 
         dm_green(root) = c_dm_root_init * g_plants
         dm_green(stem) = c_dm_stem_init * g_plants
         dm_green(leaf) = c_dm_leaf_init * g_plants
         dm_green(grain) = 0.0
         dm_green(flower) = 0.0
 
!changed from start_grain_fill
 
      elseif (on_day_of (flowering
     :                 , g_current_stage, g_days_tot)) then
 
             ! we are at first day of grainfill.
             ! set the minimum weight of leaf; used for translocation to grain
             ! and stem!
 
         dm_plant_leaf = divide (dm_green(leaf), g_plants, 0.0)
         dm_plant_min(leaf) = dm_plant_leaf * (1.0 - c_leaf_trans_frac)
         dm_plant_stem = divide (dm_green(stem), g_plants, 0.0)
         dm_plant_min(stem) = dm_plant_stem * (1.0 - c_stem_trans_frac)
!For sunflower had to make the head (flower) available for retrans to grain
         dm_plant_flower = divide (dm_green(flower), g_plants, 0.0)
         dm_plant_min(flower) = dm_plant_flower 
     :                       * (1.0 - c_flower_trans_frac)
 
      else   ! no changes
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine srop_leaf_area_lai_max_possible (
     .          start_stage_SPLA,
     .          end_stage_TPLA,
     .          end_stage_SPLA,
     .          g_current_stage,
     .          g_swdef_lai_loss,
     .          g_lai_max_possible,
     .          g_lai,
     .          g_slai,
     .          g_dlt_lai_pot,
     .          g_dlt_lai_stressed)
 
*     ===========================================================
      implicit none
      include 'error.pub'                         
      include 'science.pub'                         

*+  Sub-Program Arguments
      integer    start_stage_SPLA
      integer    end_stage_TPLA
      integer    end_stage_SPLA
      real       g_current_stage
      real       g_swdef_lai_loss
      real       g_lai_max_possible
      real       g_lai
      real       g_slai
      real       g_dlt_lai_pot           !lai potential from TT
      real       g_dlt_lai_stressed      !g_dlt_lai_pot limited by stress

*+  Purpose
*     Calculate the stressed dlt_lai from the potential dlt_lai.
*     Returns both as well as an upper limit on potential lai
*     due to irretrivable stress losses
*
*   Called by srop_leaf_area_potential(2) in croptree.for

*+  Changes
*     26/2/96  sb made it up.
*     27/5/97 scc tried to fix it.

*+  Calls
*      include   'CropDefCons.inc'
*      include 'const.inc'

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_leaf_area_lai_max_possible')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
cscc/glh Need a proper target for SPLA. Although I think that we should
c not have a target at all, but rather a 'longevity' approach. This makes
c it easier to implement N stress also
 
c (Trying to..) calculate LAI that is (irretreivably) lost due to stress
 
      if (stage_is_between (start_stage_SPLA, end_stage_SPLA
     :                     , g_current_stage)) then
 
!replace following g_dlt_lai_stressed w. g_dlt_lai_actual
         g_swdef_lai_loss = g_swdef_lai_loss
     :      + g_dlt_lai_pot - g_dlt_lai_stressed
 
c Calculate max. LAI possible, accounting for losses due to stress
c Unfortunately, this sometimes goes negative! Need to have another
c look at its calculation. Not used at present.
 
         g_lai_max_possible = g_lai + g_slai - g_swdef_lai_loss
 
!scc after flag leaf, tplamax for senescence is set to the tpla reached
 
         if (stage_is_between (end_stage_TPLA, end_stage_SPLA
     :                     , g_current_stage)) then
            g_lai_max_possible = g_lai + g_slai
         else
            ! do nothing
         endif


!This is the cheap version

            g_lai_max_possible = g_lai + g_slai
 
      else
      ! Before floral initiation
 
         g_lai_max_possible = 0.0
 
      endif
 
      call pop_routine (my_name)
      return
      end



* ====================================================================
       subroutine crop_cover1 (
     .          g_row_spacing,
     .          c_x_row_spacing,
     .          c_y_extinct_coef,
     .          c_num_row_spacing,
     .          g_lai,
     .          g_cover_green)
* ====================================================================
      implicit none
      include 'science.pub'                       
      include 'error.pub'                         

*+  Sub-Program Arguments
      real g_row_spacing
      real c_x_row_spacing(*)
      real c_y_extinct_coef(*)
      real c_num_row_spacing
      real g_lai
      real g_cover_green

*+  Purpose
*scc Does crop cover calculation for green, senesced or dead LAI
!scc This is an excellent general routine

*+  Changes
*     15-08-1997 - huth - Programmed and Specified
*
*   Called by srop_water_demand(1) in crop

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'crop_cover1')

*+  Local Variables
      real extinct_coef

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      extinct_coef = linear_interp_real (g_row_spacing
     :                                  ,c_x_row_spacing
     :                                  ,c_y_extinct_coef
     :                                  ,int(c_num_row_spacing))
 
      g_cover_green = (1.0 -exp (-extinct_coef*g_lai))
 
      call pop_routine (myname)
      return
      end


*     ===========================================================
      subroutine sunf_leaf_number_new(
     .          start_leaf_stage,
     .          end_leaf_stage,

     .          g_current_stage,
     .          g_days_tot,
     .          g_phase_tt,
 
     .          p_determinate_crop,
     .          p_x_node_num_lar,
     .          p_y_node_lar,
     .          p_num_node_lar,

     .          c_leaf_init_rate,
     .          p_rel_leaf_init_rate,
     .          c_leaf_no_seed,
     .          c_leaf_no_min,
     .          c_leaf_no_max,
     .          g_leaf_no_final,
     .          g_leaf_no,

     .          g_dlt_tt,
     .          g_dlt_leaf_no,
     .          g_node_no)
*     ===========================================================
      implicit none
      include 'CropDefCons.inc' !!!THESE SHOULD BE READ IN as part of initialisation
      include 'error.pub'

*+  Sub-Program Arguments
      integer    start_leaf_stage
      integer    end_leaf_stage

      real       g_current_stage
      real       g_days_tot(*)
      real       g_phase_tt(*)


      integer    p_determinate_crop
      real       p_x_node_num_lar(*)
      real       p_y_node_lar(*)
      integer    p_num_node_lar
      

      real       c_leaf_init_rate
      real       p_rel_leaf_init_rate

*
      real       c_leaf_no_seed
      real       c_leaf_no_min
      real       c_leaf_no_max
      real       g_leaf_no_final         ! (OUTPUT) maximum total leaf number
*
      real       g_leaf_no(*)

      real       g_dlt_tt
      real       g_dlt_leaf_no           ! (OUTPUT) new fraction of oldest
      real       g_node_no                                       ! expanding leaf

*+  Purpose
*     Initialises final leaf number and controls leaf appearance
*
*   Called by _process
*
*   Number of options: 1
*   Option 1:
*     Applies to cereals
*   Calls srop_leaf_number_final1, srop_leaf_appearance1 in crop.for

*+  Changes
*      5/9/96 dph

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sunf_leaf_number_new')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
         call sunf_leaf_number_final1 (
     .          emerg,
     .          floral_init,
     .          plant_end,
 
     .          g_current_stage,
     .          g_days_tot,
     .          g_phase_tt,
     .          c_leaf_init_rate,
     .          p_rel_leaf_init_rate,
     .          c_leaf_no_seed,
     .          c_leaf_no_min,
     .          c_leaf_no_max,
     .          g_leaf_no_final)
 
      call sunf_leaf_appearance_new(
     .          g_leaf_no,
     .          g_leaf_no_final,

     .          p_determinate_crop,
     .          p_x_node_num_lar,
     .          p_y_node_lar,
     .          p_num_node_lar,
     
     .          start_leaf_stage,
     .          end_leaf_stage,
     
     .          g_current_stage,
     .          g_days_tot,
     .          g_dlt_tt,
     .          g_dlt_leaf_no)
 
      call pop_routine (my_name)
      return
      end


*     ===========================================================
      subroutine sunf_leaf_number_final1 (
     .          start_leaf_init,
     .          end_leaf_init,
     .          reset_stage,
 
     .          g_current_stage,
     .          g_days_tot,
     .          g_phase_tt,
     .          c_leaf_init_rate,
     .          p_rel_leaf_init_rate,
     .          c_leaf_no_seed,
     .          c_leaf_no_min,
     .          c_leaf_no_max,
     .          g_leaf_no_final)
*     ===========================================================
      implicit none
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    start_leaf_init !stage to begin (e.g. emerg) est. final leaf no.
      integer    end_leaf_init !stage to end (e.g. floral_init) est. final leaf no.
      integer    reset_stage   !stage to reset final leaf no.
*
      real       g_current_stage
      real       g_days_tot(*)
      real       g_phase_tt(*)
      real       c_leaf_init_rate
      REAL       p_rel_leaf_init_rate
      real       c_leaf_no_seed
      real       c_leaf_no_min
      real       c_leaf_no_max
      real       g_leaf_no_final         ! (OUTPUT) maximum total leaf number

*+  Purpose
*       Calculate total leaf number.  This is set at floral_initialisation and
*       is set at an approximated number at germination to allow
*       other calculations to proceed until the correct number is known.
*
*   Called by srop_leaf_number(1) in croptree.for

*+  Changes
*     010994 jngh specified and programmed
*     070495 psc  changed from emerg to germ
*     0596   glh  fixed it up
!     201099 scc  Added parameter to modify leaf init rate for sunflower
!                 expressed relative to Sunfola which has a base rate of
!                 24 deg days per leaf


*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sunf_leaf_number_final1')

*+  Local Variables
      real       tt_floral_init        ! cumulative dtt from sowing
                                       ! to true floral initiation (deg day)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
         ! set total leaf number
 
      if (stage_is_between(start_leaf_init, end_leaf_init
     .     , g_current_stage)
     .      .or.
     .      on_day_of (end_leaf_init, g_current_stage, g_days_tot))
     .      then
 
          ! estimate the final leaf no from an approximated thermal
          ! time for the period from emergence to floral initiation.
 
        tt_floral_init = sum_between(start_leaf_init, end_leaf_init
     .     ,g_phase_tt)

! For sunflower leaf init rate is expressed relative to Sunfola
! which has a rate of 24 deg days per leaf

        g_leaf_no_final = divide (tt_floral_init
     :                , c_leaf_init_rate * p_rel_leaf_init_rate, 0.0)
     :                + c_leaf_no_seed
 
         call bound_check_real_var (g_leaf_no_final
     :                            , c_leaf_no_min, c_leaf_no_max
     :                            , 'g_leaf_no_final')
 
      elseif (on_day_of (reset_stage, g_current_stage, g_days_tot))
     . then
         g_leaf_no_final = 0.0
 
      endif
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sunf_leaf_appearance_new(
     .          g_leaf_no,
     .          g_leaf_no_final,

     .          p_determinate_crop,
     .          p_x_node_num_lar,
     .          p_y_node_lar,
     .          p_num_node_lar,
     
     .          start_leaf_stage,
     .          end_leaf_stage,
     
     .          g_current_stage,
     .          g_days_tot,
     .          g_dlt_tt,
     .          g_dlt_leaf_no)
*     ===========================================================
      implicit none
      include   'CropDefCons.inc'
      include 'science.pub'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       g_leaf_no(*)
      real       g_leaf_no_final
      
      integer    p_determinate_crop
      real       p_x_node_num_lar(*)
      real       p_y_node_lar(*)
      integer    p_num_node_lar
      
      integer    start_leaf_stage
      integer    end_leaf_stage

      real       g_current_stage
      real       g_days_tot(*)
      real       g_dlt_tt
      real       g_dlt_leaf_no         ! (OUTPUT) new fraction of oldest
                                       ! expanding leaf

*+  Purpose
*       Return the fractional increase in emergence of the oldest
*       expanding leaf.
*       Note ! this does not take account of the other younger leaves
*       that are currently expanding
*
*   Called by srop_leaf_number(1) in croptree.for

*+  Changes
*       25021999 ew specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sunf_leaf_appearance_new')

*+  Local Variables
      real       leaf_no_change        ! number of leaves where the last lar change occurs
      real       leaf_no_remain        ! number of leaves to go 
      real       leaf_no_now           ! number of fully expanded leaves
      real       leaf_app_rate         ! rate of leaf appearance (oCd/leaf)
      integer    stage                 ! rate of leaf appearance (oCd/leaf)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 

      leaf_no_now = sum_between (emerg, now, g_leaf_no)


      if (p_determinate_crop .eq. 1) then
          leaf_no_change = g_leaf_no_final - p_x_node_num_lar(3)
      else
          leaf_no_change = p_x_node_num_lar(3)
      endif 


      !Get the lar from the lookup table
      if (leaf_no_now.lt.p_x_node_num_lar(2)) then
          leaf_app_rate = p_y_node_lar(1)
          
      else if (leaf_no_now .ge. p_x_node_num_lar(2) .and.   
     :         leaf_no_now .lt. leaf_no_change) then
          leaf_app_rate = p_y_node_lar(2)
      
      else
          leaf_app_rate = p_y_node_lar(3)
      endif 
          
 
 
      stage = int(g_current_stage)
      
      
      if (stage.ge.start_leaf_stage.and.stage.le.end_leaf_stage) then
         ! if leaves are still growing, the cumulative number of
         ! phyllochrons or fully expanded leaves is calculated from
         ! daily thermal time for the day.
 
         g_dlt_leaf_no = divide (g_dlt_tt, leaf_app_rate, 0.0)

        if (p_determinate_crop .eq. 1) then 
            leaf_no_remain=MAX(0.0, g_leaf_no_final - leaf_no_now)
            g_dlt_leaf_no =bound(g_dlt_leaf_no, 0.0, leaf_no_remain)
        endif 
        
      else
         g_dlt_leaf_no = 0.0
 
      endif
 
      call pop_routine (my_name)
      return
      end





*     ===========================================================
      subroutine sunf_leaf_appearance1 (
     .          g_leaf_no,
     .          g_leaf_no_final,
     .          c_leaf_no_rate_change,
     .          c_leaf_app_rate2,
     .          c_leaf_app_rate1,
     .          g_current_stage,
     .          g_days_tot,
     .          g_dlt_tt,
     .          g_dlt_leaf_no)
*     ===========================================================
      implicit none
      include   'CropDefCons.inc'
      include 'science.pub'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       g_leaf_no(*)
      real       g_leaf_no_final
      real       c_leaf_no_rate_change
      real       c_leaf_app_rate2
      real       c_leaf_app_rate1
      real       g_current_stage
      real       g_days_tot(*)
      real       g_dlt_tt
      real       g_dlt_leaf_no           ! (OUTPUT) new fraction of oldest
                                       ! expanding leaf

*+  Purpose
*       Return the fractional increase in emergence of the oldest
*       expanding leaf.
*       Note ! this does not take account of the other younger leaves
*       that are currently expanding
*
*   Called by srop_leaf_number(1) in croptree.for

*+  Changes
*       031194 jngh specified and programmed
*       070495 psc  added 2nd leaf appearance rate
*       260596 glh  corrected error in leaf no calcn

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sunf_leaf_appearance1')

*+  Local Variables
      real       leaf_no_remaining     ! number of leaves to go before all
                                       ! are fully expanded
      real       leaf_no_now           ! number of fully expanded leaves
      real       leaf_app_rate         ! rate of leaf appearance (oCd/leaf)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
cglh uses sowing, not emerg to calc leaf no.
 
      leaf_no_now = sum_between (emerg, now, g_leaf_no)
      leaf_no_remaining = g_leaf_no_final - leaf_no_now
 
!scc Peter's 2 stage version used here, modified to apply
! to last few leaves before flag
!i.e. c_leaf_no_rate_change is leaf number from the top down (e.g. 4)
 
      if (leaf_no_remaining .le. c_leaf_no_rate_change) then
 
         leaf_app_rate = c_leaf_app_rate2
 
      else
 
         leaf_app_rate = c_leaf_app_rate1
 
      endif
 
 
      if (on_day_of (emerg, g_current_stage, g_days_tot)) then
 
             ! initialisation done elsewhere.
         g_dlt_leaf_no = 0.0
 
      elseif (leaf_no_remaining.gt.0.0) then
 
             ! if leaves are still growing, the cumulative number of
             ! phyllochrons or fully expanded leaves is calculated from
             ! daily thermal time for the day.
 
         g_dlt_leaf_no = divide (g_dlt_tt, leaf_app_rate, 0.0)
         g_dlt_leaf_no = bound (g_dlt_leaf_no, 0.0, leaf_no_remaining)
 
      else
             ! we have full number of leaves.
 
         g_dlt_leaf_no = 0.0
 
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sunf_phenology2 (
     .       g_previous_stage,
     .       g_current_stage,
 
     .       g_maxt, g_mint,
     .       c_x_temp, c_y_tt,
     .       c_num_temp, g_dlt_tt,
 
     :       C_num_sw_avail_ratio,
     :       C_x_sw_avail_ratio, C_y_swdef_pheno, G_dlayer,
     :       g_root_depth, g_sw_avail, g_sw_avail_pot, g_swdef_pheno,
 
     .       g_dm_green,
     .       g_N_conc_crit, g_N_conc_min, g_N_green,
     .       c_N_fact_pheno, g_nfact_pheno,
 
     .          g_days_tot,
     .          g_sowing_depth,
     .          g_tt_tot,
     .          g_phase_tt,
 
     .          g_sw_dep,
     .          p_ll_dep,
     .          c_pesw_germ,
 
     .          g_dlt_stage,
 
     .          c_tt_base,
     .          c_tt_opt,
     .          g_tt_tot_fm,
     .          g_dlt_tt_fm,
     .          g_sw_supply_demand_ratio,
     .          p_tt_switch_stage)                                   !<------------- Enli added the switch
 
 
*     ===========================================================
      implicit none
      include 'CropDefCons.inc'
      include 'const.inc'
c     include 'stress.inc' !to set value of photo - not sure if correct way
      include 'science.pub'                       
      include 'error.pub'                         

*+  Purpose
*     Use temperature, photoperiod and genetic characteristics
*     to determine when the crop begins a new growth phase.
*     The initial daily thermal time and height are also set.
*     Has a different thermal time during grain filling....
*
*   Called by _process
*
*   Number of options: 1
*   Option 1:
*     Designed for cereals...???
*   Calls srop_pheno_swdef_fact1, srop_pheno_n_fact1
*         srop_thermal_time1,
*         srop_phenology_init1, srop_phase_devel1, srop_devel1 in crop.for

*+  Notes
cscc 030997 HAVE to generalise this routine. Could do so by being able to
c specify init routine and stages to apply water and N stress
cscc Needs to incorporate water stress and low N effects on phenology
c usually by slowing down leaf appearance in vegetative phase
c and often hastening leaf senescence in grainfilling phase
c Water stress effect during grainfilling is partly because the canopy heats up
c more than it would if it were irrigated. Really need to predict canopy temp.
c somehow ...
c But if slow down leaf appearance etc. need to relate that to the leaf area mod
c (how do we do this w. TPLA approach?)

*+  Changes
*     010994 jngh specified and programmed

*+  Calls
      real       g_maxt
      real       g_mint
      real       c_x_temp(*)
      real       c_y_tt(*)
      integer    c_num_temp
      real       g_dlt_tt                ! (OUTPUT) daily thermal time (oC)
*
      INTEGER c_num_sw_avail_ratio  ! (INPUT)
      REAL    c_x_sw_avail_ratio(*) ! (INPUT)
      REAL    c_y_swdef_pheno(*)    ! (INPUT)
      REAL    g_dlayer(max_layer)           ! (INPUT)  thickness of soil layer I (mm)
      REAL    g_root_depth          ! (INPUT)  depth of roots (mm)
      REAL    g_sw_avail(max_layer)         ! (INPUT)  actual extractable soil water (mm
      REAL    g_sw_avail_pot(max_layer)     ! (INPUT)  potential extractable soil water
      REAL    G_swdef_pheno         ! (OUTPUT) sw stress factor (0-1)
      REAL    G_nfact_pheno         ! (OUTPUT) sw stress factor (0-1)
*
      REAL       g_dm_green(max_part)         ! (INPUT)  live plant dry weight (biomass
      REAL       g_n_conc_crit(max_part)      ! (INPUT)  critical N concentration (g N/
      REAL       g_n_conc_min(max_part)       ! (INPUT)  minimum N concentration (g N/g
      REAL       g_n_green(max_part)          ! (INPUT)  plant nitrogen content (g N/m^
      REAL       c_n_fact_pheno        ! (INPUT)  multipler for N deficit effect
*
      real      g_days_tot(*)
!      real      c_shoot_lag
      real      g_sowing_depth
!      real      c_shoot_rate
!      real      p_tt_emerg_to_endjuv
!      real      p_tt_endjuv_to_init
!      integer   g_day_of_year
!      real      g_latitude
!      real      c_twilight
!      real      p_photoperiod_crit1
!      real      p_photoperiod_crit2
!      real      p_photoperiod_slope
!      real      g_leaf_no_final
!      real      c_leaf_no_rate_change
!      real      c_leaf_no_at_emerg
!      real      c_leaf_app_rate1
!      real      c_leaf_app_rate2
      real      g_tt_tot(*)
!      real      p_tt_flag_to_flower
!      real      p_tt_flower_to_start_grain
!      real      p_tt_flower_to_maturity
!      real      p_tt_maturity_to_ripe
      real      G_phase_tt (*) ! (INPUT/OUTPUT) cumulative growing
                               ! degree days required for stage (deg days)
*
      real       g_sw_dep(max_layer)
      real       p_ll_dep(max_layer)
      real       c_pesw_germ
      real       G_phase_dvl           ! (OUTPUT) fraction of current phase elap
*
      real       g_dlt_stage             ! (OUTPUT) change in growth stage
      real       g_current_stage         ! (OUTPUT) new stage no.
      real       g_previous_stage         ! (OUTPUT) new stage no.
*
      real       c_tt_base      !variables used in thermal time for GF
      real       c_tt_opt       !variables used in thermal time for GF
      real       g_tt_tot_fm    !variables used in thermal time for GF
      real       g_dlt_tt_fm    !variables used in thermal time for GF
      real       g_sw_supply_demand_ratio
      integer    p_tt_switch_stage                                    !<------------- Enli added the switch
      
      

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sproc_phenology2')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
!alternative option for thermal time during grain_fill
!has 2 extra variables -
!     .          g_tt_tot_fm,
!     .          g_dlt_tt_fm,
 
         g_previous_stage = g_current_stage
 
         call srop_thermal_time1 (
     .          g_maxt, g_mint,
     .          c_x_temp, c_y_tt,
     .          c_num_temp, g_dlt_tt)
 
         call srop_thermal_time2 (
     .          g_maxt, g_mint,
     .          c_tt_base, c_tt_opt,
     .          g_dlt_tt_fm)
 
            !Modify g_dlt_tt by stress factors
 
       if (stage_is_between(sowing, flowering, g_current_stage)) then
 
!       g_swdef_pheno left alone
 
         if (stage_is_between(sowing, endjuv, g_current_stage))
     :          g_nfact_pheno = 1.0
         if (g_nfact_pheno .lt. 0.5) g_nfact_pheno = 0.5
 
          g_dlt_tt = g_dlt_tt *
     :             min (g_swdef_pheno, g_nfact_pheno)
 
         else
 
            g_dlt_tt = g_dlt_tt
 
         endif
 
         ! initialise phenology phase targets
 
!Determine fraction of phase that has elapsed
!    If stage is between flowering and maturity, use this function to calc
!       daily thermal time for phenology decisions only
 
 
        ! if (stage_is_between(flowering, maturity,    
         if (stage_is_between(p_tt_switch_stage, maturity, !<------------- Enli added the switch
     .                g_current_stage)) then
            call srop_phase_devel1(
     .          g_current_stage,
     .          g_sowing_depth,
     .          g_dlayer,
     .          g_sw_dep,
     .          p_ll_dep,
     .          c_pesw_germ,
     .          g_days_tot,
     .          g_tt_tot_fm,
     .          g_dlt_tt_fm,
     .          g_phase_tt,
     .          g_phase_dvl)
        else
           call srop_phase_devel1(
     .          g_current_stage,
     .          g_sowing_depth,
     .          g_dlayer,
     .          g_sw_dep,
     .          p_ll_dep,
     .          c_pesw_germ,
     .          g_days_tot,
     .          g_tt_tot,
     .          g_dlt_tt,
     .          g_phase_tt,
     .          g_phase_dvl)
        endif
 
! calculate new delta and the new stage
 
         call srop_devel1 (
     .          g_dlt_stage,
     .          g_current_stage,
     .          g_phase_dvl,
     .          max_stage)
 
         call accumulate (g_dlt_tt, g_tt_tot
     :               , g_previous_stage, g_dlt_stage)
 
         call accumulate (1.0, g_days_tot
     :               , g_previous_stage, g_dlt_stage)
 
         call accumulate (g_dlt_tt_fm, g_tt_tot_fm
     :               , g_previous_stage, g_dlt_stage)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sunf_phen_init_new (
     .          g_current_stage,
     .          g_days_tot,
     .          c_shoot_lag,
     .          g_sowing_depth,
     .          c_shoot_rate,
     .          p_tt_emerg_to_endjuv,
     .          p_tt_endjuv_to_init,
     .          g_day_of_year,
     .          g_latitude,
     .          c_twilight,
     .          p_photoperiod_crit1,
     .          p_photoperiod_crit2,
     .          p_photoperiod_slope,
     .          g_leaf_no_final,
     .          c_leaf_no_rate_change,
     .          c_leaf_no_at_emerg,

     .          p_determinate_crop,
     .          p_x_node_num_lar,
     .          p_y_node_lar,

     .          p_tt_fi_to_flag,
     .          p_tt_flag_to_flower,
     .          p_tt_flower_to_start_grain,
     .          p_tt_flower_to_maturity,
     .          p_tt_maturity_to_ripe,
     .          g_phase_tt)
*     ===========================================================
      implicit none
      include   'CropDefCons.inc'
      include 'science.pub'
      include 'error.pub'                         

*+  Sub-Program Arguments
      real      g_current_stage
      real      g_days_tot(*)
      real      c_shoot_lag
      real      g_sowing_depth
      real      c_shoot_rate
      real      p_tt_emerg_to_endjuv
      real      p_tt_endjuv_to_init
      integer   g_day_of_year
      real      g_latitude
      real      c_twilight
      real      p_photoperiod_crit1
      real      p_photoperiod_crit2
      real      p_photoperiod_slope
      real      g_leaf_no_final
      real      c_leaf_no_rate_change
      real      c_leaf_no_at_emerg

      integer   p_determinate_crop
      real      p_x_node_num_lar(*)
      real      p_y_node_lar(*)

      REAL      p_tt_fi_to_flag
      real      p_tt_flag_to_flower
      real      p_tt_flower_to_start_grain
      real      p_tt_flower_to_maturity
      real      p_tt_maturity_to_ripe
      real      g_phase_tt (*)           ! (INPUT/OUTPUT) cumulative growing
                                       ! degree days required for
                                       ! each stage (deg days)

*+  Purpose
*       Returns cumulative thermal time targets required for the
*       individual developmental stages.
*
*   Called by srop_phenology(1) in croptree.for

*+  Changes
*     010994 jngh specified and programmed
*     070495 psc added 2nd leaf appearance rate
*     090695 psc l_bound added (otherwise won't progress if g_phase_tt=0)
*     120995 glh restructured routine

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sunf_phen_init_new')

*+  Local Variables
c      real       tt_emerg_to_flag_leaf ! thermal time to develop
c                                       ! and fully expand all leaves (oC)
      real       photoperiod           ! daylength (hours)
c     real       leaf_no_change
      
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      ! set estimates of phase thermal time targets at germination
 
      if (on_day_of (germ, g_current_stage, g_days_tot)) then
         g_phase_tt(germ_to_emerg) = c_shoot_lag
     :                             + g_sowing_depth*c_shoot_rate
 
         g_phase_tt(emerg_to_endjuv) = p_tt_emerg_to_endjuv
         g_phase_tt(endjuv_to_init)  = p_tt_endjuv_to_init
 
      
      ! revise thermal time target for floral initialisation at emergence
 
      elseif (on_day_of (emerg, g_current_stage, g_days_tot) .or.
     :        stage_is_between (emerg, endjuv, g_current_stage) .or.
     :        on_day_of (endjuv, g_current_stage, g_days_tot)) then
 
         photoperiod = day_length (g_day_of_year, g_latitude,c_twilight)
 
         if (photoperiod.le.p_photoperiod_crit1) then
            g_phase_tt(endjuv_to_init) = p_tt_endjuv_to_init
 
         elseif (photoperiod.lt.p_photoperiod_crit2) then
 
            g_phase_tt(endjuv_to_init) = p_tt_endjuv_to_init +
     :      p_photoperiod_slope*(photoperiod - p_photoperiod_crit1)
 
         elseif (photoperiod.ge.p_photoperiod_crit2) then
            g_phase_tt(endjuv_to_init) = p_tt_endjuv_to_init +
     :      p_photoperiod_slope*(p_photoperiod_crit2
     :                         - p_photoperiod_crit1)
 
         else
         endif
 

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

c
c         leaf_no_change = max(g_leaf_no_final - p_x_node_num_lar(3),
c     :                        c_leaf_no_at_emerg)
c
c         leaf_no_change = min (leaf_no_change, g_leaf_no_final)
c
c
c      if (p_determinate_crop.eq.0) leaf_no_change=p_x_node_num_lar(3)
c
c
c      if (leaf_no_change .gt. p_x_node_num_lar(2)) then
c         tt_emerg_to_flag_leaf =
c    :     (p_x_node_num_lar(2)-c_leaf_no_at_emerg )*p_y_node_lar(1)
c     :    +(leaf_no_change     -p_x_node_num_lar(2))*p_y_node_lar(2)
c     :    +                     p_x_node_num_lar(3) *p_y_node_lar(3)
c      else
c         tt_emerg_to_flag_leaf =
c     :     (leaf_no_change - c_leaf_no_at_emerg) *p_y_node_lar(1)
c     :    +                  p_x_node_num_lar(3) *p_y_node_lar(3)
c      endif

c
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     
c         g_phase_tt(init_to_flag) = tt_emerg_to_flag_leaf
c     :              - g_phase_tt(emerg_to_endjuv)
c     :              - g_phase_tt(endjuv_to_init)
 
         g_phase_tt(init_to_flag) = p_tt_fi_to_flag

 
         g_phase_tt(flag_to_flower) = p_tt_flag_to_flower
 
         g_phase_tt(flower_to_start_grain) =
     :                    p_tt_flower_to_start_grain
 
         g_phase_tt(end_grain_to_maturity) =
     :                  0.05*p_tt_flower_to_maturity
 
         g_phase_tt(start_to_end_grain) = p_tt_flower_to_maturity
     :                  - g_phase_tt(flower_to_start_grain)
     :                  - g_phase_tt(end_grain_to_maturity)
       
         g_phase_tt(maturity_to_ripe) = p_tt_maturity_to_ripe
      else
          ! do nothing
      endif
 
      call pop_routine (my_name)
      return
      end





*     ===========================================================
      subroutine sunf_phenology3 (
     .       g_previous_stage,
     .       g_current_stage,
 
     .       g_maxt, g_mint,
     .       c_x_temp, c_y_tt,
     .       c_num_temp, g_dlt_tt,
 
     :       C_num_sw_avail_ratio,
     :       C_x_sw_avail_ratio, C_y_swdef_pheno, G_dlayer,
     :       g_root_depth, g_sw_avail, g_sw_avail_pot, g_swdef_pheno,
 
     .       g_dm_green,
     .       g_N_conc_crit, g_N_conc_min, g_N_green,
     .       c_N_fact_pheno, g_nfact_pheno,
 
     .          g_days_tot,
     .          c_shoot_lag,
     .          g_sowing_depth,
     .          c_shoot_rate,
     .          p_tt_emerg_to_endjuv,
     .          p_tt_endjuv_to_init,
     .          g_day_of_year,
     .          g_latitude,
     .          c_twilight,
     .          p_photoperiod_crit1,
     .          p_photoperiod_crit2,
     .          p_photoperiod_slope,
     .          g_leaf_no_final,
     .          c_leaf_no_rate_change,
     .          c_leaf_no_at_emerg,
     .          c_leaf_app_rate1,
     .          c_leaf_app_rate2,
     .          g_tt_tot,
     .          p_tt_flag_to_flower,
     .          p_tt_flower_to_start_grain,
     .          p_tt_flower_to_maturity,
     .          p_tt_maturity_to_ripe,
     .          g_phase_tt,
 
     .          g_sw_dep,
     .          p_ll_dep,
     .          c_pesw_germ,
 
     .          g_dlt_stage,
 
     .          c_tt_base,
     .          c_tt_opt,
     .          g_tt_tot_fm,
     .          g_dlt_tt_fm,
     .          g_sw_supply_demand_ratio)
 
 
 
*     ===========================================================
      implicit none
      include 'CropDefCons.inc'
      include 'const.inc'
c     include 'stress.inc' !to set value of photo - not sure if correct way
      include 'science.pub'                       
      include 'crp_phen.pub'                      
      include 'error.pub'                         

*+  Purpose
*     Use temperature, photoperiod and genetic characteristics
*     to determine when the crop begins a new growth phase.
*     The initial daily thermal time and height are also set.
*     Has a different thermal time during grain filling....
*
*   Called by _process
*
*   Number of options: 1
*   Option 1:
*     Designed for cereals...???
*   Calls srop_pheno_swdef_fact1, srop_pheno_n_fact1
*         srop_thermal_time1,
*         srop_phenology_init1, srop_phase_devel1, srop_devel1 in crop.for

*+  Notes
cscc 030997 HAVE to generalise this routine. Could do so by being able to
c specify init routine and stages to apply water and N stress
cscc Needs to incorporate water stress and low N effects on phenology
c usually by slowing down leaf appearance in vegetative phase
c and often hastening leaf senescence in grainfilling phase
c Water stress effect during grainfilling is partly because the canopy heats up
c more than it would if it were irrigated. Really need to predict canopy temp.
c somehow ...
c But if slow down leaf appearance etc. need to relate that to the leaf area mod
c (how do we do this w. TPLA approach?)

*+  Changes
*     010994 jngh specified and programmed

*+  Calls
      real       g_maxt
      real       g_mint
      real       c_x_temp(*)
      real       c_y_tt(*)
      integer    c_num_temp
      real       g_dlt_tt                ! (OUTPUT) daily thermal time (oC)
*
      INTEGER c_num_sw_avail_ratio  ! (INPUT)
      REAL    c_x_sw_avail_ratio(*) ! (INPUT)
      REAL    c_y_swdef_pheno(*)    ! (INPUT)
      REAL    g_dlayer(max_layer)           ! (INPUT)  thickness of soil layer I (mm)
      REAL    g_root_depth          ! (INPUT)  depth of roots (mm)
      REAL    g_sw_avail(max_layer)         ! (INPUT)  actual extractable soil water (mm
      REAL    g_sw_avail_pot(max_layer)     ! (INPUT)  potential extractable soil water
      REAL    G_swdef_pheno         ! (OUTPUT) sw stress factor (0-1)
      REAL    G_nfact_pheno         ! (OUTPUT) sw stress factor (0-1)
*
      REAL       g_dm_green(max_part)         ! (INPUT)  live plant dry weight (biomass
      REAL       g_n_conc_crit(max_part)      ! (INPUT)  critical N concentration (g N/
      REAL       g_n_conc_min(max_part)       ! (INPUT)  minimum N concentration (g N/g
      REAL       g_n_green(max_part)          ! (INPUT)  plant nitrogen content (g N/m^
      REAL       c_n_fact_pheno        ! (INPUT)  multipler for N deficit effect
*
      real      g_days_tot(*)
      real      c_shoot_lag
      real      g_sowing_depth
      real      c_shoot_rate
      real      p_tt_emerg_to_endjuv
      real      p_tt_endjuv_to_init
      integer   g_day_of_year
      real      g_latitude
      real      c_twilight
      real      p_photoperiod_crit1
      real      p_photoperiod_crit2
      real      p_photoperiod_slope
      real      g_leaf_no_final
      real      c_leaf_no_rate_change
      real      c_leaf_no_at_emerg
      real      c_leaf_app_rate1
      real      c_leaf_app_rate2
      real      g_tt_tot(*)
      real      p_tt_flag_to_flower
      real      p_tt_flower_to_start_grain
      real      p_tt_flower_to_maturity
      real      p_tt_maturity_to_ripe
      real      G_phase_tt (*) ! (INPUT/OUTPUT) cumulative growing
                               ! degree days required for stage (deg days)
*
      real       g_sw_dep(max_layer)
      real       p_ll_dep(max_layer)
      real       c_pesw_germ
      real       G_phase_dvl           ! (OUTPUT) fraction of current phase elap
*
      real       g_dlt_stage             ! (OUTPUT) change in growth stage
      real       g_current_stage         ! (OUTPUT) new stage no.
      real       g_previous_stage         ! (OUTPUT) new stage no.
*
      real       c_tt_base      !variables used in thermal time for GF
      real       c_tt_opt       !variables used in thermal time for GF
      real       g_tt_tot_fm    !variables used in thermal time for GF
      real       g_dlt_tt_fm    !variables used in thermal time for GF
      real       g_sw_supply_demand_ratio

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sunf_phenology3')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
!alternative option for thermal time during grain_fill
!has 2 extra variables -
!     .          g_tt_tot_fm,
!     .          g_dlt_tt_fm,
 
         g_previous_stage = g_current_stage
 
         call crop_thermal_time
     :               (
     :                C_num_temp
     :              , C_x_temp
     :              , C_y_tt
     :              , G_current_stage
     :              , G_maxt
     :              , G_mint
     :              , emerg             !start_stress_stage
     :              , flowering         !end_stress_stage
     :              , G_nfact_pheno
     :              , G_swdef_pheno
     :              , g_dlt_tt
     :               )
 
!         call srop_thermal_time1 (
!     .          g_maxt, g_mint,
!     .          c_x_temp, c_y_tt,
!     .          c_num_temp, g_dlt_tt)
 
!should generalise this....
         call srop_thermal_time2 (
     .          g_maxt, g_mint,
     .          c_tt_base, c_tt_opt,
     .          g_dlt_tt_fm)
 
         if (stage_is_between(emerg, flowering, g_current_stage))then
 
            !Modify g_dlt_tt by stress factors
 
          if(stage_is_between(emerg,floral_init,g_current_stage))then
 
             call srop_pheno_swdef_fact1(C_num_sw_avail_ratio,
     :        C_x_sw_avail_ratio, C_y_swdef_pheno, max_layer,G_dlayer,
     :        G_root_depth, G_sw_avail, G_sw_avail_pot, g_swdef_pheno)
 
          else
 
             call srop_pheno_swdef_fact2 (
     .        g_sw_supply_demand_ratio,
     .        g_swdef_pheno)
 
          endif
!         prevent N stress on phenology before end juvenile  GMC
          if (stage_is_between(sowing, endjuv, g_current_stage))then
             g_nfact_pheno = 1.0
          else
             call srop_pheno_N_fact1(leaf, stem, g_dm_green,
     .      g_N_conc_crit, g_N_conc_min, g_N_green,
     .      c_N_fact_pheno, g_nfact_pheno)
          endif
 
!         g_nfact_pheno = 1.0 !Needed for SLN model at present...
 
          g_dlt_tt = g_dlt_tt *
     :             min (g_swdef_pheno, g_nfact_pheno)
 
         else
 
            g_dlt_tt = g_dlt_tt
 
         endif
 
         ! initialise phenology phase targets
 
!Determine fraction of phase that has elapsed
!    If stage is between flowering and maturity, use this function to calc
!       daily thermal time for phenology decisions only
 
         if (stage_is_between(flowering, maturity,
     .                g_current_stage)) then
            call srop_phase_devel1(
     .          g_current_stage,
     .          g_sowing_depth,
     .          g_dlayer,
     .          g_sw_dep,
     .          p_ll_dep,
     .          c_pesw_germ,
     .          g_days_tot,
     .          g_tt_tot_fm,
     .          g_dlt_tt_fm,
     .          g_phase_tt,
     .          g_phase_dvl)
        else
           call srop_phase_devel1(
     .          g_current_stage,
     .          g_sowing_depth,
     .          g_dlayer,
     .          g_sw_dep,
     .          p_ll_dep,
     .          c_pesw_germ,
     .          g_days_tot,
     .          g_tt_tot,
     .          g_dlt_tt,
     .          g_phase_tt,
     .          g_phase_dvl)
        endif
 
! calculate new delta and the new stage
 
!         call srop_devel1 (
!     .          g_dlt_stage,
!     .          g_current_stage,
!     .          g_phase_dvl,
!     .          max_stage)
 
         call crop_devel
     :               (
     :                G_current_stage
     :              , max_stage
     :              , G_phase_dvl
     :              , g_dlt_stage, g_current_stage
     :               )
 
         call accumulate (g_dlt_tt, g_tt_tot
     :               , g_previous_stage, g_dlt_stage)
 
         call accumulate (1.0, g_days_tot
     :               , g_previous_stage, g_dlt_stage)
 
         call accumulate (g_dlt_tt_fm, g_tt_tot_fm
     :               , g_previous_stage, g_dlt_stage)
 
      call pop_routine (my_name)
      return
      end


*     ===========================================================
*     Routines from CropLib and CropProc ========================
*     CropLib Routines ==========================================
*     ===========================================================
      subroutine srop_phase_devel1 (
     .          g_current_stage,
     .          g_sowing_depth,
     .          g_dlayer,
     .          g_sw_dep,
     .          p_ll_dep,
     .          c_pesw_germ,
     .          g_days_tot,
     .          g_tt_tot,
     .          g_dlt_tt,
     .          g_phase_tt,
     .          phase_dvl)
*     ===========================================================
      implicit none
      include   'CropDefCons.inc'
      include 'science.pub'
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       g_current_stage
      real       g_sowing_depth
      real       g_dlayer(*)
      real       g_sw_dep(*)
      real       p_ll_dep(*)
      real       c_pesw_germ
      real       g_days_tot(*)
      real       g_tt_tot(*)
      real       g_dlt_tt
      real       g_phase_tt(*)
      real       phase_dvl           ! (OUTPUT) fraction of current phase elap

*+  Purpose
*     Determine the fraction of current phase elapsed ().
*
*   Called by srop_phenology(1) in croptree.for
*   Calls srop_germination and srop_phase_tt in crop.for

*+  Changes
*     010994 jngh specified and programmed

*+  Calls
      real       srop_germination     ! function
      real       srop_phase_tt        ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_phase_devel1')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (stage_is_between (sowing, germ, g_current_stage)) then
         phase_dvl = srop_germination (
     .          g_sowing_depth,
     .          g_dlayer,
     .          g_sw_dep,
     .          p_ll_dep,
     .          c_pesw_germ,
     .          g_current_stage,
     .          g_days_tot)
 
      elseif (stage_is_between (germ, harvest_ripe
     :                        , g_current_stage)) then
 
         phase_dvl =  srop_phase_tt (
     .          g_current_stage,
     .          g_tt_tot,
     .          g_dlt_tt,
     .          g_phase_tt)
 
      else
         phase_dvl = mod(g_current_stage, 1.0)
 
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine srop_pheno_N_fact1(
     .          leaf, stem,
     .          g_dm_green,
     .          g_n_conc_crit,
     .          g_n_conc_min,
     .          g_n_green,
     .          c_n_fact_pheno,
     .          g_nfact)
*     ===========================================================
      implicit none
      include   'const.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer leaf
      integer stem
      REAL       g_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
      REAL       g_n_conc_crit(*)      ! (INPUT)  critical N concentration (g N/
      REAL       g_n_conc_min(*)       ! (INPUT)  minimum N concentration (g N/g
      REAL       g_n_green(*)          ! (INPUT)  plant nitrogen content (g N/m^
      REAL       c_n_fact_pheno        ! (INPUT)  multipler for N deficit effect
      real       g_nfact                 ! (OUTPUT) N stress factor

*+  Purpose
*     The concentration of Nitrogen in leaves is used to derive a
*     series of Nitrogen stress indices.  The stress indices for
*     photosynthesis and cell expansion are calculated from today's
*     relative nutritional status between a critical and minimum
*     leaf Nitrogen concentration
*
*   Called by srop_phenology(1) in croptree.for
*   Calls srop_n_conc_ratio

*+  Changes
*     060495 nih taken from template
*     970215 slw split from mungb_nfact

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_pheno_N_fact1')

*+  Local Variables
      real       N_conc_ratio          ! available N as fraction of N capacity
      real       N_def                 ! N factor (0-1)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      call srop_N_conc_ratio(leaf, stem, g_dm_green,
     :                       g_n_conc_crit, g_n_conc_min,
     :                       g_n_green, N_conc_ratio)
 
      N_def = c_n_fact_pheno * N_conc_ratio
      g_nfact = bound (N_def, 0.0, 1.0)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine srop_pheno_swdef_fact1(
     .          c_num_sw_avail_ratio,
     .          c_x_sw_avail_ratio,
     .          c_y_swdef_pheno,
     .          num_layer,
     .          g_dlayer,
     .          g_root_depth,
     .          g_sw_avail,
     .          g_sw_avail_pot,
     .          g_swdef)
*     ===========================================================
      implicit none
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      INTEGER c_num_sw_avail_ratio  ! (INPUT)
      REAL    c_x_sw_avail_ratio(*) ! (INPUT)
      REAL    c_y_swdef_pheno(*)    ! (INPUT)
      INTEGER num_layer           ! (INPUT)  number of layers in profile
      REAL    g_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL    g_root_depth          ! (INPUT)  depth of roots (mm)
      REAL    g_sw_avail(*)         ! (INPUT)  actual extractable soil water (mm
      REAL    g_sw_avail_pot(*)     ! (INPUT)  potential extractable soil water
      REAL    g_swdef               ! (OUTPUT) sw stress factor (0-1)

*+  Purpose
*       Get the soil water availability factor (0-1), commonly
*       called soil water deficit factor. 1 is no stress, 0 is full stress.
*
*   Called by srop_phenology(1) in croptree.for

*+  Changes
*       010994 jngh specified and programmed
*       970216 slw generalised to avoid common blocks , added num_layer paramete

*+  Constant Values
      character  my_name*(*)      ! name of procedure
      parameter (my_name = 'srop_pheno_swdef_fact1')

*+  Local Variables
      integer deepest_layer       ! deepest layer in which the roots are growing
      real    sw_avail_ratio      ! water availability ratio
      real    sw_avail_pot_sum    ! potential extractable soil water (mm)
      real    sw_avail_sum        ! actual extractable soil water (mm)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      deepest_layer = find_layer_no (g_root_depth, g_dlayer, num_layer)
      sw_avail_pot_sum = sum_real_array (g_sw_avail_pot, deepest_layer)
      sw_avail_sum = sum_real_array (g_sw_avail, deepest_layer)
      sw_avail_ratio = divide (sw_avail_sum, sw_avail_pot_sum, 1.0) !???
      sw_avail_ratio = bound (sw_avail_ratio , 0.0, 1.0)
      g_swdef = linear_interp_real(sw_avail_ratio, c_x_sw_avail_ratio,
     :                           c_y_swdef_pheno, c_num_sw_avail_ratio)
 
      call pop_routine (my_name)
      return
      end



* ====================================================================
       subroutine srop_pheno_swdef_fact2 (
     .    g_sw_supply_demand_ratio,
     .    g_swdef_pheno)
 
* ====================================================================
      implicit none
      include 'error.pub'                         

*+  Sub-Program Arguments
      real g_sw_supply_demand_ratio
      real g_swdef_pheno

*+  Purpose
*     <insert here>

*+  Changes
*     10-10-1997 - chapman - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'srop_pheno_swdef_fact2')

*+  Local Variables
      real c_critical_sd_ratio
      real c_sd_slope

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
!cscc 8/11/95 Change in stress approach to one that effectively
! extends targets...
!rlv 9/10/97 changed to affect dlt_tt not extend target
!
! Donatelli et al. Crop Sci. 1992. trans. ratio effect on phenology
!3/11/97 SCC - GMC to change this to a table function
 
        c_critical_sd_ratio= 0.55
        c_sd_slope= 0.61
 
        if(g_sw_supply_demand_ratio.ge.c_critical_sd_ratio) then
            g_swdef_pheno = 1.0
        else
            g_swdef_pheno = 1.0/(1.0 + c_sd_slope
     :      * (c_critical_sd_ratio - g_sw_supply_demand_ratio))
        endif
 
      call pop_routine (myname)
      return
      end



*     ===========================================================
      subroutine srop_N_Conc_Ratio(
     .          leaf,stem,
     .          g_dm_green,
     .          g_n_conc_crit,
     .          g_n_conc_min,
     .          g_n_green,
     .          g_N_conc_ratio)
*     ===========================================================
      implicit none
      include   'const.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer leaf
      integer stem
      REAL       g_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
      REAL       g_n_conc_crit(*)      ! (INPUT)  critical N concentration (g N/
      REAL       g_n_conc_min(*)       ! (INPUT)  minimum N concentration (g N/g
      REAL       g_n_green(*)          ! (INPUT)  plant nitrogen content (g N/m^
      real       g_N_conc_ratio          ! (OUTPUT) available N as fraction of N

*+  Purpose
*         Use shoot nitrogen concentrations to calculate 0-1 N availability
*         factors.  Since all plant processes are not equally susceptible
*         to N stress, N deficiency factors are calculated from a 0-1 N
*         factor to affect different processes.
*           0 affects grain N potential
*           1 affects photosynthesis
*           2 affects leaf senescence, grain N concentration & cell expansion
*           3 affects grain number
*
*           nfac range is 0.001 to 0.95 or 1.0 for optimum conditions.
*           N_def - 1 range is 0.2012 to .98 or 1 for optimum conditions.
*           N_def - 2 range is .00095 to .9025 or .95 for optimum conditions.
*           N_def - 3 range is .201 to 1 for optimum conditions.
*
*         ???? check that returns 1 & 0 for optimum and zero conditions.
*
*    Called by srop_RUE_N_fact1, srop_pheno_n_fact, srop_grainconc_N_fact1
*      in crop.for

*+  Changes
*     010994 jngh specified and programmed
*     090695 psc  added N_fact for phenology & externalise multipliers for ndef
*     970314 slw extracted this common code from nitrogen stress routines

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_N_conc_ratio')

*+  Local Variables
      real       N_conc_stover         ! tops (stover) actual N concentration
                                       ! (0-1)
      real       dm_stover             ! tops (stover) plant weight (g/m^2)
      real       N_conc_stover_crit    ! tops (stover) critical N concentration
                                       ! (0-1)
      real       N_conc_stover_min     ! tops (stover) minimum N concentration
                                       ! (0-1)
      real       N_leaf_crit           ! critical leaf nitrogen (g/m^2)
      real       N_leaf_min            ! minimum leaf nitrogen (g/m^2)
      real       N_stem_crit           ! critical stem nitrogen (g/m^2)
      real       N_stem_min            ! minimum stem nitrogen (g/m^2)
      real       N_stover              ! tops (stover) plant nitrogen (g/m^2)
      real       N_stover_crit         ! critical top nitrogen (g/m^2)
      real       N_stover_min          ! minimum top nitrogen (g/m^2)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
         ! calculate actual N concentrations
 
      dm_stover = g_dm_green(leaf) + g_dm_green(stem)
      N_stover = g_n_green(leaf) + g_n_green(stem)
 
      N_conc_stover = divide (N_stover, dm_stover, 0.0)
 
         ! calculate critical N concentrations
 
      N_leaf_crit = g_n_conc_crit(leaf) * g_dm_green(leaf)
      N_stem_crit = g_n_conc_crit(stem) * g_dm_green(stem)
      N_stover_crit = N_leaf_crit + N_stem_crit
 
      N_conc_stover_crit = divide (N_stover_crit, dm_stover, 0.0)
 
         ! calculate minimum N concentrations
 
      N_leaf_min = g_n_conc_min(leaf) * g_dm_green(leaf)
      N_stem_min = g_n_conc_min(stem) * g_dm_green(stem)
      N_stover_min = N_leaf_min + N_stem_min
 
      N_conc_stover_min = divide (N_stover_min, dm_stover, 0.0)
 
         ! calculate shortfall in N concentrations
 
      g_N_conc_ratio = divide ((N_conc_stover - N_conc_stover_min)
     :              , (N_conc_stover_crit - N_conc_stover_min), 0.0)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      real function srop_phase_tt (
     .          g_current_stage,
     .          g_tt_tot,
     .          g_dlt_tt,
     .          g_phase_tt)
*     ===========================================================
      implicit none
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       g_current_stage              ! (INPUT) stage number
      real       g_tt_tot(*)
      real       g_dlt_tt
      real       g_phase_tt(*)

*+  Purpose
*       Return fraction of thermal time we are through the current
*       phenological phase (0-1)
*
*   Called by srop_phase_devel1 in cropopt.for

*+  Changes
*     010994 jngh specified and programmed
*     970518 scc modified, according to JNGH phen bug fix

*+  Calls
!      real       bound                 ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_phase_tt')

*+  Local Variables
      integer    phase                 ! phase number containing stage

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      phase = int (g_current_stage)
cjh  changed 0.0 to 1.0
      srop_phase_tt = divide (g_tt_tot(phase) + g_dlt_tt
     :                       , g_phase_tt(phase), 1.0)
!      srop_phase_tt = bound (srop_phase_tt, 0.0, 1.999999)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      real function srop_germination (
     .          g_sowing_depth,
     .          g_dlayer,
     .          g_sw_dep,
     .          p_ll_dep,
     .          c_pesw_germ,
     .          g_current_stage,
     .          g_days_tot)
*     ===========================================================
      implicit none
      include   'CropDefCons.inc'
      include 'data.pub'
      include 'science.pub'                       
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       g_sowing_depth
      real       g_dlayer(*)
      real       g_sw_dep(*)
      real       p_ll_dep(*)
      real       c_pesw_germ
      real       g_current_stage
      real       g_days_tot(*)

*+  Purpose
*      Determine germination based on soil water availability
*
*   Called by srop_phase_devel1 in cropopt.for
*
*   Returns srop_germination as a fraction of current phase elapsed

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_germination')

*+  Local Variables
      integer    layer_no_seed         ! seedling layer number
      real       pesw_seed             ! plant extractable soil water in
                                       ! seedling layer available for
                                       ! germination ( mm/mm)

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
         ! determine if soil water content is sufficient to allow germination.
         ! Soil water content of the seeded layer must be > the
         ! lower limit to be adequate for germination.
 
      if (stage_is_between (sowing, germ, g_current_stage)) then
 
         layer_no_seed = find_layer_no (g_sowing_depth, g_dlayer
     :                                             , max_layer)
         pesw_seed = divide (g_sw_dep(layer_no_seed)
     :                     - p_ll_dep(layer_no_seed)
     :                     , g_dlayer(layer_no_seed), 0.0)
 
            ! can't germinate on same day as sowing, because miss out on
            ! day of sowing else_where
 
         if (pesw_seed.gt.c_pesw_germ
     :   .and.
     :   .not. on_day_of (sowing, g_current_stage, g_days_tot)) then
               ! we have germination
               ! set the fraction of the current phase
               ! so it is on the point of germination
            srop_germination = 1.0 + mod (g_current_stage, 1.0)
         else
                ! no germination yet but indicate that we are on the way.
            srop_germination = 0.999
         endif
      else
             ! no sowing yet
         srop_germination = 0.0
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine srop_thermal_time1 (
     .          g_maxt,
     .          g_mint,
     .          c_x_temp,
     .          c_y_tt,
     .          c_num_temp,
     .          g_dlt_tt)
*     ===========================================================
      implicit none
      include    'CropDefCons.inc'
      include 'science.pub'
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       g_maxt
      real       g_mint
      real       c_x_temp(*)
      real       c_y_tt(*)
      integer    c_num_temp
      real       g_dlt_tt                ! (OUTPUT) daily thermal time (oC)

*+  Purpose
*     Growing degree day (thermal time) is calculated.
*
*   Called by srop_phenology(1) in croptree.for

*+  Notes
*     Eight interpolations of the air temperature are
*     calculated using a three-hour correction factor.
*     For each air three-hour air temperature, a value of growing
*     degree day is calculated.  The eight three-hour estimates
*     are then averaged to obtain the daily value of growing degree
*     days.

*+  Changes
*     140994 jngh specified and programmed
*     090695 psc  added N_fact for phenology stress
*     030997 scc  removed all stress factors - they belong in CROPTREE!

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_thermal_time1')

*+  Local Variables
      real       dly_therm_time        ! thermal time for the day (deg day)

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      dly_therm_time = linint_3hrly_temp (g_maxt, g_mint
     :                 , c_x_temp, c_y_tt
     :                 , c_num_temp)
 
      g_dlt_tt = dly_therm_time
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine srop_thermal_time2 (
     .          g_maxt,
     .          g_mint,
     .          c_tt_base,
     .          c_tt_opt,
     .          g_dlt_tt_fm)
*     ===========================================================
      implicit none
      include    'CropDefCons.inc'
      include 'error.pub'

*+  Sub-Program Arguments
      real       g_maxt
      real       g_mint
      real       c_tt_base
      real       c_tt_opt
      real       g_dlt_tt_fm                ! (OUTPUT) daily thermal time (oC)

*+  Purpose
*     Growing degree day (thermal time) is calculated.
*
*     This function used between flowering and maturity
*
*   Called by srop_phenology(1) in croptree.for

*+  Notes
*   G_dlt_tt = 0                  { av_temp <= tt_base oC
*            = av_temp - tt_base  { tt_base < av_temp < tt_opt
*            = tt_opt             { av_temp >= tt_opt
*
*   default values for tt_base = 5.7 and tt_opt = 23.5
*

*+  Changes
*     190997 gmc programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_thermal_time2')

*+  Local Variables
      real       av_temp               ! average daily temp
      real       dly_therm_time        ! thermal time for the day (deg day)

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      av_temp = (g_maxt + g_mint) / 2.0
 
      if(av_temp .le. c_tt_base) then
         dly_therm_time = 0.0
      else if(av_temp .le. c_tt_opt) then
         dly_therm_time = av_temp - c_tt_base
      else
         dly_therm_time = c_tt_opt - c_tt_base
      endif
 
      g_dlt_tt_fm = dly_therm_time
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine srop_devel1 (
     .          g_dlt_stage,
     .          g_current_stage,
     .          phase_devel,
     .          max_stage)
*     ===========================================================
      implicit none
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       g_dlt_stage             ! (OUTPUT) change in growth stage
      real       g_current_stage         ! (OUTPUT) new stage no.
      real       phase_devel

*+  Purpose
*     Determine the current stage of development.
*
*   Called by srop_phenology(1) in croptree.for

*+  Changes
*     010994 jngh specified and programmed
*     070495 psc add l_bound to dlt-stage
*     970518 scc Fixed Phen bug (JNH notes)

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_devel1')

*+  Local Variables
      real       new_stage             ! new stage number
      integer    max_stage              !New code

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      ! calculate the new delta and the new stage
 
      new_stage = aint (g_current_stage) + phase_devel
      g_dlt_stage = new_stage - g_current_stage
 
 
      if (phase_devel.ge.1.0) then
         g_current_stage = aint (g_current_stage + 1.0)
         if (int(g_current_stage).eq.max_stage) then
            g_current_stage = 1.0
         endif
      else
         g_current_stage = new_stage
 
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine srop_leaf_area_sen_frost1(
     .          c_frost_fraction,
     .          c_frost_temp,
     .          c_num_frost_temp,
     .          g_lai,
     .          g_mint,
     .          g_dlt_slai_frost)
*     ===========================================================
      implicit none
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      REAL    c_frost_fraction(*) ! (INPUT)
      REAL    c_frost_temp(*)     ! (INPUT)
      INTEGER c_num_frost_temp    ! (INPUT)
      REAL    g_lai               ! (INPUT)  live plant green lai
      REAL    g_mint              ! (INPUT)  minimum air temperature (oC)
      real    g_dlt_slai_frost    ! (OUTPUT) lai frosted today

*+  Purpose
*       Return the lai that would senesce on the
*       current day from low temperatures
*
*   Called from srop_leaf_area_sen(1) in croptree.for

*+  Changes
*     070495 nih taken from template
*     970317 slw templated

*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'srop_leaf_area_sen_frost1')

*+  Local Variables
      real dlt_slai_low_temp    ! lai senesced from low temps
      real sen_fac_temp         ! low temperature factor (0-1)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
          ! low temperature factor
      sen_fac_temp = linear_interp_real(g_mint,c_frost_temp,
     :                      c_frost_fraction,c_num_frost_temp)
 
      dlt_slai_low_temp = sen_fac_temp * g_lai
      g_dlt_slai_frost = bound (dlt_slai_low_temp, 0.0, g_lai)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine srop_leaf_area_sen_frost2(
     .          c_frost_kill,
     .          g_lai,
     .          g_mint,
     .          g_dlt_slai_frost)
*     ===========================================================
      implicit none
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      REAL c_frost_kill        ! (INPUT)  temperature threshold for leaf death (oC
      REAL g_lai               ! (INPUT)  live plant green lai
      REAL g_mint              ! (INPUT)  minimum air temperature (oC)
      real g_dlt_slai_frost    ! (OUTPUT) lai frosted today

*+  Purpose
*+      Return the lai that would senesce on the
*       current day from frosting
*
*   Called from srop_leaf_area_sen(2) in croptree.for

*+  Changes
*     010994 jngh specified and programmed
*     970317 slw templated

*+  Constant Values
      character  my_name*(*) ! name of procedure
      parameter (my_name = 'srop_leaf_area_sen_frost2')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
          ! calculate senecence due to frost
      if (g_mint.le.c_frost_kill) then
         g_dlt_slai_frost = g_lai
      else
         g_dlt_slai_frost = 0.0
      endif
      g_dlt_slai_frost = bound (g_dlt_slai_frost, 0.0, g_lai)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine srop_leaf_area_sen_age2 (
     .          g_current_stage,
     .          g_tt_tot,
     .          p_spla_intercept,
     .          c_spla_slope,
     .          g_leaf_no_final,
     .          g_lai_max_possible,
     .          p_spla_prod_coef,
     .          g_slai,
     .          g_days_tot,
     .          g_plants,
     .          g_lai,
     .          g_dlt_lai,
     .          g_dlt_slai_age)
*     ===========================================================
      implicit none
      include   'CropDefCons.inc'
      include 'science.pub'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       g_current_stage
      real       g_tt_tot(*)
      real       p_spla_intercept
      real       c_spla_slope
      real       g_leaf_no_final
      real       g_lai_max_possible
      real       p_spla_prod_coef
      real       g_slai
      real       g_days_tot(*)
      real       g_plants
      real       g_lai
      real       g_dlt_lai
      real       g_dlt_slai_age     ! (OUTPUT)

*+  Purpose
*     Return the lai that would senesce  on the
*     current day from natural ageing
*
*   Called by srop_leaf_area_sen(2) in croptree.for

*+  Notes
cscc This function needs to be the rate of sen that occurs under
c non-limiting conditions of water and N (pref. from experiments w. N
c applied at flowering also)

*+  Changes
*     010994 jngh specified and programmed
*     970318 slw new template version

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_leaf_area_sen_age2')

*+  Local Variables
      real       spla_inflection       ! inflection point of leaf area
                                       ! senescence function (oC)
      real       slai_today            ! total senescence up to today
      real       tt_since_emerg        ! thermal time since emergence (oC)

      real       tplamax               ! new
      
      
      
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
         ! calculate senescence due to ageing
      if (stage_is_between (floral_init, harvest_ripe
     :                     , g_current_stage)) then
 
cscc This aging should really be linked better to phenology. The spla_inflection
c could be a function of pred. time from floral_init and to harvest_ripe or at
c least be top-limited by the actual tplamax cf. intitial pred. of tplamax. This
c would be similar to the change made to TPLA prediction. Obviously though there
c still need to feedback to actual production etc.
 
         tt_since_emerg = sum_between (emerg, now, g_tt_tot)
!        spla_inflection = p_spla_intercept
!    :                   + c_spla_slope * g_leaf_no_final

        spla_inflection = sum_between(emerg,floral_init,g_tt_tot)
  
cscc The Senescence paper on sunfhum says that the numerator below is supposed
c to be tplamax. I guess that after flag leaf, the below will be tplamax, but be
c the slai_today equation is not really doing what it should be, and is prob.
c underestimating senescence.
c Up to flag leaf, need to adjust the numerator daily, depending on stresses.
c The g_lai_max_possible is calculated in leaf (leaf_Area_Devel_plant)
!scc May 96. This not doing anything at present as g_lai_max_possible has been s
!to lai+g+g_slai. Need to fix code in leaf_Area_Devel_plant.
 
!         slai_today = divide ((g_lai + g_slai)

!         slai_today = divide ((g_lai_max_possible)
!    :              , (1.0 + exp(-p_spla_prod_coef
!    :                        * (tt_since_emerg - spla_inflection)))
!    :              , 0.0)

         tplamax = g_lai_max_possible*10000/g_plants     !and change to cm2
          
         slai_today = tplamax*p_spla_intercept
     :       *exp(p_spla_prod_coef * (tt_since_emerg - spla_inflection))
     
         slai_today = slai_today/10000*g_plants          !and change to cm2 
    

         g_dlt_slai_age = l_bound (slai_today - g_slai, 0.0)
 
         ! all leaves senesce at harvest ripe
 
cscc Does this make sense? I know we are supposed to harvest at PM, but leaves
c of sunfhum don't instantly senescence when you harvest.
c What if you harvest the crop and leave it to rattoon?
 
      elseif (on_day_of (harvest_ripe
     :                 , g_current_stage, g_days_tot)) then
          g_dlt_slai_age = g_lai + g_dlt_lai
 
      else
         g_dlt_slai_age = 0.0
      endif
 
      g_dlt_slai_age = bound (g_dlt_slai_age, 0.0, g_lai)
 
       
!      write(*,900)
!900   format(" spla_inflection, tplamax, g_slai, slai_today"
!c     :   , " g_lai_max_possible, g_dlt_slai_age")
!      write(*,1000)spla_inflection, tplamax, g_slai, slai_today
!     :   , g_lai_max_possible, g_dlt_slai_age
 
1000  format(6f10.3)
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine srop_leaf_area_sen_age1 (
     :          first_stage, last_stage,
     .          g_leaf_no_dead,
     .          g_dlt_leaf_no_dead,
     .          g_leaf_area,
     .          g_plants,
     .          g_slai,
     .          g_lai,
     .          g_dlt_slai_age)
*     ===========================================================
      implicit none
      include   'convert.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
       integer first_stage
       integer last_stage
       real g_leaf_no_dead(*)
       real g_dlt_leaf_no_dead
       real g_leaf_area(*)
       real g_plants
       real g_slai
       real g_lai
       real g_dlt_slai_age          ! (OUTPUT) new senesced lai from
                                       ! phasic devel.

*+  Purpose
*       Return the lai that would senesce on the
*       current day due to ageing
*
*   Called from srop_leaf_area_sen(1) in croptree.for

*+  Changes
*     010994 jngh specified and programmed
*     970317 slw templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_leaf_area_sen_age1')

*+  Local Variables
      real       area_sen_dying_leaf   ! senesced leaf area from
                                       ! current leaf dying (mm^2)
      integer    dying_leaf            ! current leaf number dying ()
      real       leaf_no_dead_today    ! today's number of dead leaves ()
      real       slai_age              ! lai senesced by natural ageing

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
         ! now calculate the leaf senescence
         ! due to normal phenological (phasic) development
 
         ! get highest leaf no. senescing today
      leaf_no_dead_today = sum_between (first_stage, last_stage,
     :                                  g_leaf_no_dead)
     :             + g_dlt_leaf_no_dead
      dying_leaf = int (1.0 + leaf_no_dead_today)
         ! get area senesced from highest leaf no.
 
      area_sen_dying_leaf = mod (leaf_no_dead_today, 1.0)
     :                    * g_leaf_area(dying_leaf)
 
      slai_age = (sum_real_array (g_leaf_area, dying_leaf - 1)
     :         + area_sen_dying_leaf)
     :         * smm2sm * g_plants
 
      g_dlt_slai_age = bound (slai_age - g_slai, 0.0, g_lai)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine srop_leaf_area_sen_light1 (
     .          c_lai_sen_light,
     .          c_sen_light_slope,
     .          g_lai,
     .          g_dlt_slai_light)
*     ===========================================================
      implicit none
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
       real c_lai_sen_light
       real c_sen_light_slope
       real g_lai
       real g_dlt_slai_light        ! (OUTPUT) lai senesced by low light

*+  Purpose
*       Return the lai that would senesce on the
*       current day due to light competition
*
*   Called from srop_leaf_area_sen(1) in croptree.for

*+  Changes
*     010994 jngh specified and programmed
*     970317 slw templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_leaf_area_sen_light1')

*+  Local Variables
      real       slai_light_fac        ! light competition factor (0-1)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
         ! calculate 0-1 factor for leaf senescence due to
         ! competition for light.
 
c+!!!!!!!! this doesnt account for other growing crops
c+!!!!!!!! should be based on reduction of intercepted light and k*lai
         ! competition for light factor
 
      if (g_lai.gt.c_lai_sen_light) then
         slai_light_fac = c_sen_light_slope * (g_lai - c_lai_sen_light)
      else
         slai_light_fac = 0.0
      endif
 
      g_dlt_slai_light = g_lai * slai_light_fac
      g_dlt_slai_light = bound (g_dlt_slai_light, 0.0, g_lai)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine srop_leaf_area_sen_water1(
     .          c_sen_rate_water,
     .          g_lai,
     .          g_swdef_photo,
     .          g_dlt_slai_water)
*     ===========================================================
      implicit none
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      REAL c_sen_rate_water       ! (INPUT)  slope in linear eqn relating soil wat
      REAL g_lai                  ! (INPUT)  live plant green lai
      REAL g_swdef_photo          ! (INPUT)
      REAL g_dlt_slai_water       ! (OUTPUT) water stress senescense

*+  Purpose
*       Return the lai that would senesce on the
*       current day due to water stress
*
*   Called from srop_leaf_area_sen(1) in croptree.for

*+  Changes
*     010994 jngh specified and programmed
*     970216 slw generalised to avoid common blocks

*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'srop_leaf_area_sen_water1')

*+  Local Variables
      real       slai_water_fac ! drought stress factor (0-1)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
        ! drought stress factor
      slai_water_fac = c_sen_rate_water* (1.0 - g_swdef_photo)
      g_dlt_slai_water = g_lai * slai_water_fac
      g_dlt_slai_water = bound (g_dlt_slai_water, 0.0, g_lai)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine srop_leaf_area_sen_water2(
     .          g_day_of_year,
     .          g_year,
     .          c_sen_threshold,
     .          c_sen_water_time_const,
     .          num_layer,
     .          g_dlayer,
     .          g_lai,
     .          g_lai_equilib_water,
     .          g_root_depth,
     .          g_sw_demand,
     .          g_sw_supply,
     .          g_dlt_slai_water)
*     ===========================================================
      implicit none
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      INTEGER g_day_of_year              ! (INPUT)  day of year
      INTEGER g_year                     ! (INPUT)  year
      REAL    c_sen_threshold            ! (INPUT)  supply:demand ratio for onset
      REAL    c_sen_water_time_const     ! (INPUT)  delay factor for water senesce
      INTEGER num_layer                ! (INPUT)  number of layers in profile
      REAL    g_dlayer(*)                ! (INPUT)  thickness of soil layer I (mm)
      REAL    g_lai                      ! (INPUT)  live plant green lai
      REAL    g_lai_equilib_water(*)     ! (INPUT)  lai threshold for water senesc
      REAL    g_root_depth               ! (INPUT)  depth of roots (mm)
      REAL    g_sw_demand                ! (INPUT)  total crop demand for water (m
      REAL    g_sw_supply(*)             ! (INPUT)  potential water to take up (su
      REAL    g_dlt_slai_water           ! (OUTPUT) water stress senescense

*+  Purpose
*       Return the lai that would senesce on the
*       current day from water stress
*
*   Called from srop_leaf_area_sen(2) in croptree.for
*   Calls srop_running_ave in  crop.for

*+  Changes
*     010994 jngh specified and programmed
*     970216 slw generalised to avoid common blocks , added num_layer parameter

*+  Calls
      real    srop_running_ave        ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_leaf_area_sen_water2')

*+  Local Variables
      real    ave_lai_equilib_water    ! running mean lai threshold for water se
      integer deepest_layer            ! deepest layer in which the roots are gr
      real    sw_demand_ratio          ! water supply:demand ratio
      real    sw_supply_sum            ! total supply over profile (mm)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
         ! calculate senescense from water stress
         ! NOTE needs rework for multiple crops
 
      deepest_layer = find_layer_no (g_root_depth, g_dlayer, num_layer)
      sw_supply_sum = sum_real_array (g_sw_supply, deepest_layer)
      sw_demand_ratio = divide (sw_supply_sum, g_sw_demand, 1.0)
 
      if (sw_demand_ratio.lt.c_sen_threshold) then
         ave_lai_equilib_water = srop_running_ave(g_day_of_year,
     :                            g_year, g_lai_equilib_water, 10)
 
         g_dlt_slai_water = (g_lai - ave_lai_equilib_water)
     :                  / c_sen_water_time_const
 
         g_dlt_slai_water = l_bound (g_dlt_slai_water, 0.0)
 
      else
         g_dlt_slai_water = 0.0
 
      endif
      g_dlt_slai_water = bound (g_dlt_slai_water, 0.0, g_lai)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine srop_leaf_area_sen_light2 (
     .          g_radn_int,
     .          g_radn,
     .          c_sen_radn_crit,
     .          g_year,
     .          g_day_of_year,
     .          g_lai_equilib_light,
     .          g_lai,
     .          c_sen_light_time_const,
     .          g_dlt_slai_light)
*     ===========================================================
      implicit none
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
       real g_radn_int
       real g_radn
       real c_sen_radn_crit
       integer g_year
       integer g_day_of_year
       real g_lai_equilib_light(*)
       real g_lai
       real c_sen_light_time_const
       real g_dlt_slai_light        ! (OUTPUT) lai senesced by low light

*+  Purpose
*       Return the lai that would senesce on the
*       current day from low light
*
*   Called from srop_leaf_area_sen(2) in croptree.for
*   Calls srop_running_ave in  crop.for

*+  Changes
*     010994 jngh specified and programmed
*     970317 slw templated

*+  Calls
      real       srop_running_ave     ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_leaf_area_sen_light2')

*+  Local Variables
      real       ave_lai_equilib_light ! running mean lai threshold for light
                                       ! senescence ()
      real       radn_transmitted      ! radn transmitted through canopy
                                       ! (mj/m^2)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
         ! calculate senescense from water stress
 
c+!!!!!!!! this doesnt account for other growing crops
c+!!!!!!!! should be based on reduction of intercepted light and k*lai
c+!!!!!!!!
             ! calculate senescence due to low light
cglh - This works out se. based on when light drops below ps compensation point
c the leaf can't sustain itself.
 
      radn_transmitted = g_radn - g_radn_int
 
      if (radn_transmitted.lt.c_sen_radn_crit) then
 
         ave_lai_equilib_light = srop_running_ave
     .         (g_day_of_year, g_year, g_lai_equilib_light, 10)
         g_dlt_slai_light = divide (g_lai - ave_lai_equilib_light
     :                          , c_sen_light_time_const , 0.0)
         g_dlt_slai_light = l_bound (g_dlt_slai_light, 0.0)
      else
         g_dlt_slai_light = 0.0
      endif
 
      g_dlt_slai_light = bound (g_dlt_slai_light, 0.0, g_lai)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine srop_failure_germination1(
     .          sowing, germ, now,
     .          c_days_germ_limit,
     .          g_current_stage,
     .          g_days_tot,
     .          g_plants,
     .          g_dlt_plants)
*     ===========================================================
      implicit none
      include   'const.inc'            ! new_line
      include 'data.pub'                          
      include 'science.pub'                       
      include 'error.pub'

*+  Sub-Program Arguments
      integer sowing
      integer germ
      integer now
      REAL       c_days_germ_limit     ! (INPUT)  maximum days allowed after sowin
      REAL       g_current_stage       ! (INPUT)  current phenological stage
      REAL       g_days_tot(*)         ! (INPUT)  duration of each phase (days)
      REAL       g_plants              ! (INPUT)  Plant density (plants/m^2)
      real       g_dlt_plants            ! (OUTPUT) change in plant number

*+  Purpose
*      Crop failure from lack of germination.

*+  Changes
*       290994 jngh specified and programmed
*       970317 slw extracted from Mungbean

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_failure_germination1')

*+  Local Variables
      character  string*200            ! output string

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (stage_is_between (sowing, germ, g_current_stage)
     :   .and. sum_between (sowing, now, g_days_tot)
     :         .ge.c_days_germ_limit) then
 
         g_dlt_plants = - g_plants
 
         write (string, '(3a, i4, a)')
     :                 ' crop failure because of lack of'
     :                  ,new_line
     :                  ,'         germination within'
     :                  , c_days_germ_limit
     :                  , ' days of sowing'
         call write_string ( string)
 
      else
         g_dlt_plants = 0.0
 
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine srop_failure_emergence1(
     .          germ, emerg, now,
     .          c_tt_emerg_limit,
     .          g_current_stage,
     .          g_plants,
     .          g_tt_tot,
     .          g_dlt_plants)
*     ===========================================================
      implicit none
      include   'const.inc'            ! new_line
      include 'data.pub'                          
      include 'science.pub'                       
      include 'error.pub'

*+  Sub-Program Arguments
      integer germ
      integer emerg
      integer now
      REAL       c_tt_emerg_limit      ! (INPUT)  maximum degree days allowed fo
      REAL       g_current_stage       ! (INPUT)  current phenological stage
      REAL       g_plants              ! (INPUT)  Plant density (plants/m^2)
      REAL       g_tt_tot(*)           ! (INPUT)  the sum of growing degree days
      real       g_dlt_plants            ! (OUTPUT) change in plant number

*+  Purpose
*      Crop failure from lack of emergence.

*+  Changes
*       290994 jngh specified and programmed
*       970317 slw extracted from Mungbean
*
*   Called by srop_plant_death(1) in crop

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_failure_emergence1')

*+  Local Variables
      character  string*200            ! output string

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (stage_is_between (germ, emerg, g_current_stage)
     :       .and. sum_between (germ, now, g_tt_tot)
     :       .gt. c_tt_emerg_limit) then
 
         g_dlt_plants = - g_plants
 
         write (string, '(a)')
     :                 ' failed emergence due to deep planting'
         call write_string (string)
 
      else
         g_dlt_plants = 0.0
 
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      real function srop_running_ave(
     .          g_day_of_year,
     .          g_year,
     .          array,
     .          number_of_days)
*     ===========================================================
      implicit none
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      INTEGER g_day_of_year        ! (INPUT)  day of year
      INTEGER g_year               ! (INPUT)  year
      real    array(*)           ! (INPUT) array to use for average
      integer number_of_days     ! (INPUT) number of days to average over

*+  Purpose
*       return the running average of an array over the last specified
*       number of days.
*
*   Called by srop_leaf_area_sen_light2, srop_leaf_area_sen_water2 in crop.for

*+  Changes
*     010994 jngh specified and programmed
*     970317 slw templated

*+  Constant Values
      character  my_name*(*)     ! name of procedure
      parameter (my_name = 'srop_running_ave')

*+  Local Variables
      integer start_day          ! day of year to start running

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      start_day = offset_day_of_year(g_year,
     :                              g_day_of_year, - number_of_days)
 
      srop_running_ave = divide(sum_part_of_real(array, start_day,
     :                                           g_day_of_year, 366)
     :                          , real (abs (number_of_days)), 0.0)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine srop_lai_equilib_water(
     .           g_day_of_year,
     .           g_year,
     .           c_rue,
     .           g_cover_green,
     .           g_current_stage,
     .           g_lai,
     .           g_nfact_photo,
     .           g_radn,
     .           g_radn_int,
     .           g_sw_supply_sum,
     .           g_temp_stress_photo,
     .           g_transp_eff,
     .           g_lai_equilib_water)
*     ===========================================================
      implicit none
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      INTEGER g_day_of_year          ! (INPUT)  day of year
      INTEGER g_year                 ! (INPUT)  year
      REAL    c_extinction_coef      ! (INPUT)  radiation extinction coefficient
      REAL    c_rue(*)               ! (INPUT)  radiation use efficiency (g dm/m
      REAL    g_cover_green          ! (INPUT)  fraction of radiation reaching t
      REAL    g_current_stage        ! (INPUT)  current phenological stage
      REAL    g_lai                  ! (INPUT)  live plant green lai
      REAL    g_nfact_photo          ! (INPUT)
      REAL    g_radn                 ! (INPUT)  solar radiation (Mj/m^2/day)
      REAL    g_radn_int             ! (INPUT)  g_radn intercepted by leaves (mj
      REAL    g_sw_supply_sum        ! (INPUT)  potential water to take up (supp
      REAL    g_temp_stress_photo    ! (INPUT)
      REAL    g_transp_eff           ! (INPUT)  transpiration efficiency (g dm/m
      real    g_lai_equilib_water(*) ! (INPUT/OUTPUT) lai threshold for water se

*+  Purpose
*       Return the lai equilibrium water.
*
*   Called from srop_leaf_area_sen(2) in croptree.for
*   Calls srop_store_value in crop.for

*+  Changes
*     010994 jngh specified and programmed
*     070795 jngh corrected for case of rue = 0
*     040895 jngh corrected for intercropping
*     970216 slw generalised to avoid common blocks , added num_layer parameter

*+  Constant Values
      character  my_name*(*)       ! name of procedure
      parameter (my_name = 'srop_lai_equilib_water')

*+  Local Variables
      real       dlt_dm_transp     ! potential dry matter production
                                   ! by transpiration (g/m^2)
      real       lai_equilib_water_today ! lai threshold for water senescence
      real       lrue              ! radiation use efficiency (g dm/mj)
      real       rue_reduction     ! Effect of non-optimal N and Temp
                                   ! conditions on RUE (0-1)
      integer    stage_no          ! current stage no.
      real       intc_crit         ! critical interception (0-1)
      real       radn_canopy       ! radiation reaching canopy mj/m^2)
      real       sen_radn_crit     ! critical radiation (mj/m^2)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      c_extinction_coef = 0.4
 
      stage_no = int (g_current_stage)
 
      dlt_dm_transp = g_sw_supply_sum * g_transp_eff
      rue_reduction = min (g_temp_stress_photo, g_nfact_photo)
      lrue = c_rue(stage_no) * rue_reduction
 
      call bound_check_real_var (lrue, 0.0, c_rue(stage_no), 'c_rue')
 
      radn_canopy = divide (g_radn_int, g_cover_green, g_radn)
      sen_radn_crit = divide (dlt_dm_transp, lrue, radn_canopy)
      intc_crit = divide (sen_radn_crit, radn_canopy, 1.0)
 
      if (intc_crit.lt.1.0) then
            ! needs rework for row spacing
         lai_equilib_water_today = -log (1.0 - intc_crit)
     :                           / c_extinction_coef
 
      else
         lai_equilib_water_today =  g_lai
      endif
 
      call srop_store_value(g_day_of_year, g_year,
     :          g_lai_equilib_water, lai_equilib_water_today)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine srop_store_value(
     .          g_day_of_year,
     .          g_year,
     .          array,
     .          value)
*     ===========================================================
      implicit none
      include 'date.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      INTEGER g_day_of_year    ! (INPUT)  day of year
      INTEGER g_year           ! (INPUT)  year
      REAL    array(*)       ! (OUTPUT) storage array
      REAL    value          ! (INPUT) value to be stored

*+  Purpose
*       Stores a value in an annual circular array
*
*   Called by srop_lai_equlib_light, srop_lai_equilib_water in crop.for

*+  Changes
*     230695 jngh specified and programmed
*     970317 slw templated

*+  Constant Values
      character  my_name*(*) ! name of procedure
      parameter (my_name = 'srop_store_value')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      array(g_day_of_year) = value
 
      if (g_day_of_year.eq.365
     :   .and. leap_year (g_year - 1)) then
         array(366) = 0.0
      else
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine srop_lai_equilib_light (
     .          g_radn_int,
     .          g_cover_green,
     .          c_sen_radn_crit,
     .          c_extinction_coef,
     .          g_lai,
     .          g_day_of_year,
     .          g_year,
     .          g_lai_eqlb_light)
*     ===========================================================
      implicit none
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
       real g_radn_int
       real g_cover_green
       real c_sen_radn_crit
       real c_extinction_coef
       real g_lai
       integer g_day_of_year
       integer g_year
       real g_lai_eqlb_light(*)  ! (IN/OUT) lai threshold for light senescence

*+  Purpose
*       Return the lai equilibrium light
*
*   Called from srop_leaf_area_sen(2) in croptree.for
*   Calls srop_store_value

*+  Changes
*     010994 jngh specified and programmed
*     040895 jngh corrected for intercropping
*     970317 slw templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_lai_equilib_light')

*+  Local Variables
      real       lai_eqlb_light_today ! lai threshold for light senescence
      real       radn_canopy           ! radiation reaching canopy mj/m^2)
      real       trans_crit            ! critical transmission (0-1)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      radn_canopy = divide (g_radn_int, g_cover_green, 0.0)
      trans_crit = divide (c_sen_radn_crit, radn_canopy, 0.0)
 
      c_extinction_coef = 0.4
 
      if (trans_crit.gt.0.0) then
            ! needs rework for row spacing
         lai_eqlb_light_today = -log (trans_crit)/c_extinction_coef
      else
         lai_eqlb_light_today = g_lai
      endif
 
      call srop_store_value (
     .          g_day_of_year,
     .          g_year,
     .          g_lai_eqlb_light,
     .          lai_eqlb_light_today)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine srop_phenology_init1 (
     .          g_current_stage,
     .          g_days_tot,
     .          c_shoot_lag,
     .          g_sowing_depth,
     .          c_shoot_rate,
     .          p_tt_emerg_to_endjuv,
     .          p_tt_endjuv_to_init,
     .          g_day_of_year,
     .          g_latitude,
     .          c_twilight,
     .          p_photoperiod_crit1,
     .          p_photoperiod_crit2,
     .          p_photoperiod_slope,
     .          g_leaf_no_final,
     .          c_leaf_no_rate_change,
     .          c_leaf_no_at_emerg,
     .          c_leaf_app_rate1,
     .          c_leaf_app_rate2,
     .          g_tt_tot,
     .          p_tt_flag_to_flower,
     .          p_tt_flower_to_start_grain,
     .          p_tt_flower_to_maturity,
     .          p_tt_maturity_to_ripe,
     .          g_phase_tt)
*     ===========================================================
      implicit none
      include   'CropDefCons.inc'
      include 'data.pub'
      include 'science.pub'                       
      include 'error.pub'                         

*+  Sub-Program Arguments
      real      g_current_stage
      real      g_days_tot(*)
      real      c_shoot_lag
      real      g_sowing_depth
      real      c_shoot_rate
      real      p_tt_emerg_to_endjuv
      real      p_tt_endjuv_to_init
      integer   g_day_of_year
      real      g_latitude
      real      c_twilight
      real      p_photoperiod_crit1
      real      p_photoperiod_crit2
      real      p_photoperiod_slope
      real      g_leaf_no_final
      real      c_leaf_no_rate_change
      real      c_leaf_no_at_emerg
      real      c_leaf_app_rate1
      real      c_leaf_app_rate2
      real      g_tt_tot(*)
      real      p_tt_flag_to_flower
      real      p_tt_flower_to_start_grain
      real      p_tt_flower_to_maturity
      real      p_tt_maturity_to_ripe
      real      g_phase_tt (*)           ! (INPUT/OUTPUT) cumulative growing
                                       ! degree days required for
                                       ! each stage (deg days)

*+  Purpose
*       Returns cumulative thermal time targets required for the
*       individual developmental stages.
*
*   Called by srop_phenology(1) in croptree.for

*+  Changes
*     010994 jngh specified and programmed
*     070495 psc added 2nd leaf appearance rate
*     090695 psc l_bound added (otherwise won't progress if g_phase_tt=0)
*     120995 glh restructured routine

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_phenology_init1')

*+  Local Variables
      real       tt_emerg_to_flag_leaf ! thermal time to develop
                                       ! and fully expand all leaves (oC)
      real       photoperiod           ! daylength (hours)
*
      real       leaf_no               ! leaf no. above which app. rate changes

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
         ! set estimates of phase thermal time targets at germination
 
      if (on_day_of (germ, g_current_stage, g_days_tot)) then
         g_phase_tt(germ_to_emerg) = c_shoot_lag
     :                             + g_sowing_depth*c_shoot_rate
         g_phase_tt(emerg_to_endjuv) = p_tt_emerg_to_endjuv
         g_phase_tt(endjuv_to_init) = p_tt_endjuv_to_init
 
         ! revise thermal time target for floral initialisation at emergence
 
      elseif (on_day_of (emerg, g_current_stage, g_days_tot)) then
 
         photoperiod = day_length (g_day_of_year, g_latitude,c_twilight)
         if (photoperiod.le.p_photoperiod_crit1) then
            g_phase_tt(endjuv_to_init) = p_tt_endjuv_to_init
         elseif (photoperiod.lt.p_photoperiod_crit2) then
            g_phase_tt(endjuv_to_init) = p_tt_endjuv_to_init +
     :      p_photoperiod_slope*(photoperiod - p_photoperiod_crit1)
         elseif (photoperiod.ge.p_photoperiod_crit2) then
            g_phase_tt(endjuv_to_init) = p_tt_endjuv_to_init +
     :      p_photoperiod_slope*(p_photoperiod_crit2
     :                              - p_photoperiod_crit1)
         else
         endif
 
! revise thermal time target for floral initialisation up to initialisation
! glh revise thermal time target for floral initialisation up to endjuv
!     pp at start of period more influential than pp at end of period
 
      elseif (stage_is_between (emerg, floral_init
     :                        , g_current_stage)) then
 
         if (stage_is_between (emerg, endjuv
     :                        , g_current_stage)) then
         photoperiod = day_length (g_day_of_year, g_latitude
     :                           , c_twilight)
          if (photoperiod.le.p_photoperiod_crit1) then
            g_phase_tt(endjuv_to_init) = p_tt_endjuv_to_init
          elseif (photoperiod.lt.p_photoperiod_crit2) then
            g_phase_tt(endjuv_to_init) = p_tt_endjuv_to_init +
     :      p_photoperiod_slope*(photoperiod - p_photoperiod_crit1)
          elseif (photoperiod.ge.p_photoperiod_crit2) then
            g_phase_tt(endjuv_to_init) = p_tt_endjuv_to_init +
     :      p_photoperiod_slope*(p_photoperiod_crit2
     :                                 - p_photoperiod_crit1)
          else
          endif
         endif
 
! set estimates of phase thermal time targets at initiation
 
cscc/glh should this be endjuv too
c      elseif (on_day_of (end_juv, g_current_stage
c     :                 , g_days_tot)) then
 
 
!      elseif (on_day_of (floral_init, g_current_stage
!     :                 , g_days_tot)) then
 
 
c scc/glh changed this to speed up last few leaves before
c flag leaf (as opposed to psc 'slow down the first leaves' approach)
cpsc
 
         leaf_no = max (g_leaf_no_final - c_leaf_no_rate_change,
     :                 c_leaf_no_at_emerg)
         leaf_no = min (leaf_no, g_leaf_no_final)
         tt_emerg_to_flag_leaf = (leaf_no - c_leaf_no_at_emerg)
     :                         * c_leaf_app_rate1
     :                         + (g_leaf_no_final - leaf_no)
     :                         * c_leaf_app_rate2
 
!         tt_emerg_to_flag_leaf = (g_leaf_no_final - c_leaf_no_at_emerg)
!     :                         * c_leaf_app_rate
 
         g_phase_tt(init_to_flag) = tt_emerg_to_flag_leaf
     :              - sum_between (emerg, floral_init, g_tt_tot)
 
         g_phase_tt(flag_to_flower) = p_tt_flag_to_flower
 
         g_phase_tt(flower_to_start_grain) =
     :                    p_tt_flower_to_start_grain
 
         g_phase_tt(end_grain_to_maturity) =
     :                  0.05*p_tt_flower_to_maturity
 
         g_phase_tt(start_to_end_grain) = p_tt_flower_to_maturity
     :                  - g_phase_tt(flower_to_start_grain)
     :                  - g_phase_tt(end_grain_to_maturity)
         g_phase_tt(maturity_to_ripe) = p_tt_maturity_to_ripe
 
      else
          ! do nothing
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      real function srop_N_dlt_grain_conc(
     .              grain,
     .              c_sfac_slope,
     .              c_sw_fac_max,
     .              c_temp_fac_min,
     .              c_tfac_slope,
     .              g_maxt,
     .              g_mint,
     .              g_nfact_grain_conc,
     .              g_n_conc_crit,
     .              g_n_conc_min,
     .              g_swdef_expansion)
*     ===========================================================
      implicit none
      include 'error.pub'

*+  Sub-Program Arguments
      integer    grain
      REAL       c_sfac_slope          ! (INPUT)  soil water stress factor slope
      REAL       c_sw_fac_max          ! (INPUT)  soil water stress factor maximum
      REAL       c_temp_fac_min        ! (INPUT)  temperature stress factor minimu
      REAL       c_tfac_slope          ! (INPUT)  temperature stress factor slope
      REAL       g_maxt                ! (INPUT)  maximum air temperature (oC)
      REAL       g_mint                ! (INPUT)  minimum air temperature (oC)
      REAL       g_nfact_grain_conc    ! (INPUT)
      REAL       g_n_conc_crit(*)      ! (INPUT)  critical N concentration (g N/g
      REAL       g_n_conc_min(*)       ! (INPUT)  minimum N concentration (g N/g b
      REAL       g_swdef_expansion     ! (INPUT)

*+  Purpose
*     Calculate the nitrogen concentration required to meet the increase
*     from daily grain growth (0-1)

*+  Notes
*     First, two factors are calculated and used to estimate the
*     effects of mean temperature and drought stress on the N
*     concentration in grain growth for the day.  High temperature
*     or drought stress can cause the factors to exceed 1.
*     N deficiency can cause nfac < 1.  The net effect of these
*     equations is to allow grain nitrogen concentration to range
*     from less than .01 when N deficiency is severe to about .018
*     when adequate N is available but high temperature or drought
*     stress limit grain growth.
*     Here, optimum N concentration = 1.7%
*
*       called by srop_N_retranslocate1

*+  Changes
*       090994 jngh specified and programmed
*       970317 slw extracted from Mungbean

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_N_dlt_grain_conc')

*+  Local Variables
      real       N_conc_pot            ! potential grain N concentration
                                       ! (0-1) (g N/g part)
      real       N_grain_sw_fac        ! soil water stress factor for N
                                       ! uptake
      real       N_grain_temp_fac      ! temperature stress factor for N
                                       ! uptake
      real       ave_temp              ! mean temperature (oC)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      ave_temp = (g_maxt + g_mint) /2.0
 
c+!!!!!!!!!! return to orig cm
      N_grain_temp_fac = c_temp_fac_min + c_tfac_slope* ave_temp
      N_grain_sw_fac = c_sw_fac_max - c_sfac_slope * g_swdef_expansion
 
            ! N stress reduces grain N concentration below critical
 
      N_conc_pot = g_n_conc_min(grain)
     :           + (g_n_conc_crit(grain) - g_n_conc_min(grain))
     :           * g_nfact_grain_conc
 
            ! Temperature and water stresses can decrease/increase grain
            ! N concentration
 
            ! when there is no N stress, the following can be a higher N conc th
            ! the crit and thus the N conc of the grain can exceed N critical.
 
      srop_N_dlt_grain_conc = N_conc_pot
     :                       * max (N_grain_temp_fac, N_grain_sw_fac)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine srop_N_retrans_avail(
     .          num_part, root, grain,
     .          g_N_conc_min,
     .          g_dm_green,
     .          g_N_green,
     .          N_avail)
*     ===========================================================
      implicit none
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
       integer num_part
       integer root
       integer grain
       real g_N_conc_min(*)
       real g_dm_green(*)
       real g_N_green(*)
       real N_avail(*)

*+  Purpose
*     Calculate N available for transfer to grain (g/m^2)
*     from each plant part.  By definition, available grain N
*     is set to 0.

*+  Notes
*     N available for translocation to the grain is the sum of
*     N available in the stover.
*     N available in stover is the difference of its N content
*     and the minimum it's allowed to fall to.
*     NB. No translocation from roots.

*+  Changes
*       080994 jngh specified and programmed
*       970318 slw extracted from sunf
*
*       Called by srop_N_retranslocate1

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_N_retrans_avail')

*+  Local Variables
      real       N_min                 ! nitrogen minimum level (g/m^2)
      integer    part                  ! plant part number

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
         ! get grain N potential (supply) -----------
         ! now find the available N of each part.
 
!scc Propose a change to this section to stop all N being
!available in 1 day and limited it to a 5 day process
 
      do 1000 part = 1, num_part
         N_min = g_N_conc_min(part) * g_dm_green(part)
         N_avail(part) = l_bound (g_N_green(part) - N_min, 0.0)
!scc
!         N_avail(part) = N_avail(part) * 0.2
1000  continue
 
      N_avail(grain) = 0.0
      N_avail(root) = 0.0
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine srop_death_drought1 (
     .           g_cswd_photo,
     .           g_leaf_no,
     .           c_leaf_no_crit,
     .           c_swdf_photo_limit,
     .           g_swdef_photo,
     .           c_swdf_photo_rate,
     .           g_plants,
     .           g_dlt_plants_water)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'CropDefCons.inc'
      include 'data.pub'
      include 'error.pub'

*+  Sub-Program Arguments
       real g_cswd_photo(*)
       real g_leaf_no(*)
       real c_leaf_no_crit
       real c_swdf_photo_limit
       real g_swdef_photo
       real c_swdf_photo_rate
       real g_plants
*
       real g_dlt_plants_water

*+  Purpose
*      Determine percentage plant failure due to water stress

*+  Changes
*       290994 jngh specified and programmed
*
*   Called by srop_plant_death(1) in crop

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_death_drought1')

*+  Local Variables
      real       cswd_photo            ! cumulative water stress for photoperiod
      real       leaf_no               ! number of leaves
      real       killfr                ! fraction of crop population to kill
      character  string*200            ! output string

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      cswd_photo = sum_between (emerg, flag_leaf, g_cswd_photo)
      leaf_no = sum_between (emerg, now, g_leaf_no)
 
      if (leaf_no.lt.c_leaf_no_crit
     :       .and. cswd_photo.gt.c_swdf_photo_limit
     :       .and. g_swdef_photo .lt.1.0) then
 
         killfr = c_swdf_photo_rate* (cswd_photo - c_swdf_photo_limit)
         killfr = bound (killfr, 0.0, 1.0)
         g_dlt_plants_water = - g_plants*killfr
 
         write (string, '(a, i4, a)')
     :          'plant_kill.'
     :         , nint (killfr*100.0)
     :         , '% failure because of water stress.'
 
         call write_string (string)
 
      else
         g_dlt_plants_water = 0.0
 
      endif
 
      call pop_routine (my_name)
      return
      end

*     Routines from CropLib and CropProc ========================
*     CropProc Routines =========================================

*NEED NEW NAME ETC. FOR THIS DIFFERENT VERSION
*should generalise so that sorghum and sunflower can still use it...

*     ===========================================================
      subroutine sproc_bio_partition2 (
     .          g_current_stage,
     .          c_ratio_root_shoot,
     .          g_dlt_dm,
     .          g_leaf_no,
     .          c_partition_rate_leaf,
     .          g_dlt_lai_stressed,
     .          c_sla_min,
     .          c_frac_stem2flower,
     :          c_frac_pod2grain,
     :          c_grain_energy,
     .          g_dlt_dm_grain_demand,   
     :          g_phase_tt,
     :          g_tt_tot,
     .          g_dlt_dm_green)
*     ===========================================================
      implicit none
      include   'convert.inc'
      include   'CropDefCons.inc'
      include 'science.pub'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real g_current_stage
      real c_ratio_root_shoot(*)
      real g_dlt_dm
      real g_leaf_no(*)
      real c_partition_rate_leaf
      real g_dlt_lai_stressed
      real c_sla_min
      real c_frac_stem2flower

      real c_frac_pod2grain
      real c_grain_energy
      real g_dlt_dm_grain_demand
      real g_phase_tt (*)            ! Added 24.09.98
      real g_tt_tot (*)              ! Added 24.09.98

      real g_dlt_dm_green (*)      ! (OUTPUT) actual biomass partitioned
                                       ! to plant parts (g/m^2)

*+  Purpose
*     Partitions new dm (assimilate) between plant components (g/m^2)

*+  Changes
*     010994 jngh specified and programmed
*     250495 psc  modified dlt_dm_green(grain) to account for barren heads
*     970317 slw new template form

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sproc_bio_partition1')

*+  Local Variables
      integer    current_phase         ! current phase no.
      real       dlt_dm_green_tot      ! total of partitioned dm (g/m^2)
      real       dlt_dm_leaf_max       ! max increase in leaf dm (g/m^2)
!      real       internode_no          ! internode no of stem (leaves emerged
                                       ! since emergence)
      real       partition_coef_leaf   ! partitioning coefficient of dm to
                                       ! leaf (0-1)

      real       partition_grain
      real       yield_demand
      
      real       tt_emerg_to_flower
      real       tt_since_emerg
      
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
         ! Root must be satisfied. The roots don't take any of the
         ! carbohydrate produced - that is for tops only.  Here we assume
         ! that enough extra was produced to meet demand. Thus the root
         ! growth is not removed from the carbo produced by the model.
 
         ! first we zero all plant component deltas
 
      call fill_real_array (g_dlt_dm_green, 0.0, max_part)
 
         ! now we get the root delta for all stages - partition scheme
         ! specified in coeff file
 
      current_phase = int (g_current_stage)
      g_dlt_dm_green(root) =c_ratio_root_shoot(current_phase)*g_dlt_dm
 
 
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Enli  Begin
* ENLI has to find the change
 
      if (stage_is_between (emerg, flag_leaf, g_current_stage)) then
            ! we have leaf development only

        !==============================================================
        ! part.ratio = -0.7*(TT/TTEFA) + 1

         tt_emerg_to_flower = sum_between(emerg,
     :                          flowering,g_phase_tt)

         tt_since_emerg = sum_between (emerg, now, g_tt_tot)

         partition_coef_leaf = 1.0 
     :     -0.7*divide(tt_since_emerg,tt_emerg_to_flower, 0.0)


         g_dlt_dm_green(leaf) = partition_coef_leaf * g_dlt_dm
        
        ! limit the delta leaf area to maximum
         dlt_dm_leaf_max = divide (g_dlt_lai_stressed
     :                           , c_sla_min * smm2sm, 0.0)
         g_dlt_dm_green(leaf) = u_bound (g_dlt_dm_green(leaf)
     :                               , dlt_dm_leaf_max)
 
        !Beginning at halfway between BV and FA,
        !part ratio of stem to head = 0.56*(TT/TTEFA) - 0.42
        !equals 0 if ratio tt/ttefa<0.75
 
         c_frac_stem2flower = -0.42 
     :     +0.56*divide(tt_since_emerg,tt_emerg_to_flower, 0.0)

         if (c_frac_stem2flower.lt.0.0) c_frac_stem2flower=0.0
         
         g_dlt_dm_green(flower) = (g_dlt_dm - g_dlt_dm_green(leaf))
     :                            * c_frac_stem2flower

         g_dlt_dm_green(stem) = g_dlt_dm
     :             - (g_dlt_dm_green(flower) + g_dlt_dm_green(leaf))
 
 
        elseif (stage_is_between (flag_leaf, start_grain_fill
     :                        , g_current_stage)) then
 
        !==============================================================
        !repeat here
        ! we only have flower and stem growth here

         tt_emerg_to_flower = sum_between(emerg,
     :                          flowering,g_phase_tt)

         tt_since_emerg = sum_between (emerg, now, g_tt_tot)

         c_frac_stem2flower = -0.42 
     :     +0.56*divide(tt_since_emerg,tt_emerg_to_flower, 0.0)

         if (c_frac_stem2flower.lt.0.0) c_frac_stem2flower=0.0

         g_dlt_dm_green(flower) = g_dlt_dm*c_frac_stem2flower
         g_dlt_dm_green(stem) = g_dlt_dm - g_dlt_dm_green(flower)
 
     
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Enli End

       elseif (stage_is_between (start_grain_fill, maturity
     :                        , g_current_stage)) then
 
        !==============================================================
        !In sunflower, c_frac_pod2grain =0 so some of the code are redundant
        partition_grain = divide (1.0
     :                     , c_frac_pod2grain + c_grain_energy, 0.0)
  
         g_dlt_dm_green(grain) = divide (g_dlt_dm_grain_demand
     :                              , c_grain_energy, 0.0)
         g_dlt_dm_green(grain) = bound (g_dlt_dm_green(grain), 0.0,
     :                              g_dlt_dm * partition_grain)
        !epsc
         g_dlt_dm_green(energy) = g_dlt_dm_green(grain)
     :                        * (c_grain_energy - 1.0)
         g_dlt_dm_green(flower) = g_dlt_dm_green(grain)
     :                        * c_frac_pod2grain
         yield_demand = g_dlt_dm_green(flower) + g_dlt_dm_green(grain)
     :                    + g_dlt_dm_green(energy)
 
        if (yield_demand .ge. g_dlt_dm) then
            g_dlt_dm_green(grain) = g_dlt_dm
     :            * divide (g_dlt_dm_green(grain), yield_demand, 0.0)
            g_dlt_dm_green(energy) = g_dlt_dm
     :            * divide (g_dlt_dm_green(energy), yield_demand, 0.0)
            g_dlt_dm_green(flower) = g_dlt_dm
     :            - (g_dlt_dm_green(grain) + g_dlt_dm_green(energy))
            g_dlt_dm_green(stem) = 0.0
            g_dlt_dm_green(leaf) = 0.0
         else
            g_dlt_dm_green(stem) = g_dlt_dm - yield_demand
            g_dlt_dm_green(leaf) = 0.0

         endif
     
       elseif (stage_is_between (maturity, plant_end
     :                        , g_current_stage)) then
 
            ! put into stem
         g_dlt_dm_green(stem) = g_dlt_dm
 
      else
            ! no partitioning
      endif
 
         ! do mass balance check - roots are not included
      dlt_dm_green_tot = sum_real_array (g_dlt_dm_green, max_part)
     :                 - g_dlt_dm_green(root)
      call bound_check_real_var (dlt_dm_green_tot, g_dlt_dm, g_dlt_dm
     :                        , 'dlt_dm_green_tot mass balance')
 
         ! check that deltas are in legal range
 
      call bound_check_real_array (g_dlt_dm_green, 0.0, g_dlt_dm
     :                          , 'dlt_dm_green', max_part)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sproc_N_retranslocate1 (
     .          g_dlt_dm_green,
     .          g_maxt,
     .          g_mint,
     .          c_temp_fac_min,
     .          c_tfac_slope,
     .          c_sw_fac_max,
     .          c_sfac_slope,
     .          g_N_conc_min,
     .          g_N_conc_crit,
     .          g_dm_green,
     .          g_N_green,
     .          g_N_conc_max,
     .          g_swdef_expansion,
     .          g_nfact_grain_conc,
     .          o_dlt_N_retrans)
*     ===========================================================
      implicit none
      include   'CropDefCons.inc'
      include 'data.pub'
      include 'error.pub'                         

*+  Sub-Program Arguments
       real g_dlt_dm_green(*)
       real g_maxt
       real g_mint
       real c_temp_fac_min
       real c_tfac_slope
       real c_sw_fac_max
       real c_sfac_slope
       real g_N_conc_min(*)
       real g_N_conc_crit(*)
       real g_dm_green(*)
       real g_N_green(*)
       real g_N_conc_max(*)
       real g_swdef_expansion
       real g_nfact_grain_conc
       real o_dlt_N_retrans (*)     ! (OUTPUT) plant N taken out from
                                       ! plant parts (g N/m^2)

*+  Purpose
*     Calculate the nitrogen retranslocation from the various plant parts
*     to the grain.
*
*       Called by srop_nit_retrans(1) in croptree
*       Calls srop_N_dlt_grain_conc,  srop_N_retrans_avail   in crop

*+  Changes
*     080994 jngh specified and programmed

*+  Calls
      real       srop_N_dlt_grain_conc ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sproc_N_retranslocate1')

*+  Local Variables
      real       grain_N_demand        ! grain N demand (g/m^2)
      real       N_avail(max_part)     ! N available for transfer to grain
                                       ! (g/m^2)
      real       N_avail_stover        ! total N available in stover
                                       ! (g/m^2)
      real       N_potential           ! maximum grain N demand (g/m^2)
      integer    part                  ! plant part number

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      grain_N_demand = g_dlt_dm_green(grain) * srop_N_dlt_grain_conc(
     :          grain,
     .          c_sfac_slope,
     .          c_sw_fac_max,
     .          c_temp_fac_min,
     .          c_tfac_slope,
     .          g_maxt,
     .          g_mint,
     .          g_nfact_grain_conc,
     .          g_N_conc_crit,
     .          g_N_conc_min,
     .          g_swdef_expansion)
 
      N_potential  = (g_dm_green(grain) + g_dlt_dm_green(grain))
     :             * g_N_conc_max(grain)
 
      grain_N_demand = u_bound (grain_N_demand
     :                        , N_potential - g_N_green(grain))
 
      call srop_N_retrans_avail (max_part, root, grain,
     .          g_N_conc_min,
     .          g_dm_green,
     .          g_N_green,N_avail)  ! grain N potential (supply)
 
            ! available N does not include roots or grain
cjh  this should not presume roots and grain are 0.
csc  true....
 
      N_avail_stover  =  sum_real_array (N_avail, max_part)
 
          ! get actual grain N uptake
 
          ! limit retranslocation to total available N
 
      call fill_real_array (o_dlt_N_retrans, 0.0, max_part)
 
      if (grain_N_demand.ge.N_avail_stover) then
 
             ! demand greater than or equal to supply
             ! retranslocate all available N
 
         o_dlt_N_retrans(leaf) = - N_avail(leaf)
         o_dlt_N_retrans(stem) = - N_avail(stem)
         o_dlt_N_retrans(flower) = - N_avail(flower)
         o_dlt_N_retrans(grain) = N_avail_stover
 
      else
             ! supply greater than demand.
             ! Retranslocate what is needed
 
         o_dlt_N_retrans(leaf) = - grain_N_demand
     :                         * divide (N_avail(leaf)
     :                                 , N_avail_stover, 0.0)
 
         o_dlt_N_retrans(flower) = - grain_N_demand
     :                         * divide (N_avail(flower)
     :                                 , N_avail_stover, 0.0)
 
         o_dlt_N_retrans(stem) = - grain_N_demand
     :                         - o_dlt_N_retrans(leaf)   ! note - these are
     :                         - o_dlt_N_retrans(flower) ! -ve values.
 
         o_dlt_N_retrans(grain) = grain_N_demand
 
      endif
             ! just check that we got the maths right.
 
      do 1000 part = root, flower
         call bound_check_real_var (abs (o_dlt_N_retrans(part))
     :                            , 0.0, N_avail(part)
     :                            , 'o_dlt_N_retrans(part)')
1000  continue
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sproc_N_partition1(
     .          g_root_depth,
     .          g_dlayer,
     .          g_N_demand,
     .          g_N_max,
     .          dlt_NO3gsm,
     .          dlt_N_green
     .                     )
*     ===========================================================
      implicit none
      include   'CropDefCons.inc'
      include 'data.pub'
      include 'error.pub'                         
      include 'science.pub'                         

*+  Sub-Program Arguments
       real g_root_depth
       real g_dlayer(*)
       real g_N_demand(*)
       real g_N_max(*)
      real       dlt_N_green(max_part) ! (OUTPUT) actual plant N uptake
                                       ! into each plant part (g/m^2)
      real       dlt_NO3gsm(max_layer) ! (OUTPUT) actual plant N uptake
                                       ! from NO3 in each layer (g/m^2)

*+  Purpose
*     Return actual plant nitrogen uptake to each plant part and from
*     each soil layer.

*+  Changes
*      080994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sproc_N_partition1')

*+  Local Variables
      real       plant_part_fract      ! fraction of nitrogen to use (0-1)
                                       ! for plant part
      real       N_uptake_sum          ! total plant N uptake (g/m^2)
      real       N_excess              ! N uptake above N crit (g/m^2)
      real       N_capacity(max_part)  ! amount of N that can be stored in
                                       ! plant part above Ncrit (g/m^2)
      real       N_capacity_sum
      real       N_demand              ! total nitrogen demand (g/m^2)
      integer    part                  ! plant part number
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
               ! find proportion of uptake to be
               ! distributed to to each plant part and distribute it.
 
      deepest_layer = find_layer_no (g_root_depth, g_dlayer, max_layer)
      N_uptake_sum = - sum_real_array (dlt_NO3gsm, deepest_layer)
      N_demand = sum_real_array (g_N_demand, max_part)
 
      N_excess = N_uptake_sum - N_demand
      N_excess = l_bound (N_excess, 0.0)
 
      if (N_excess.gt.0.0) then
         do 1200 part = 1, max_part
            N_capacity(part) = g_N_max(part) - g_N_demand(part)
1200     continue
         N_capacity(grain) = 0.0
      else
         call fill_real_array (N_capacity, 0.0, max_part)
      endif
 
      N_capacity_sum = sum_real_array (N_capacity, max_part)
 
!scc RCM found that this partitioning was biased toward leaf...
!60:40 vs stem. Can achieve same effect via concentration I guess.
 
!scc Should this happen - could probably put excess into preferentially
!stem, leaf, flower, root (reverse order of usage)
 
 
      do 1300 part = 1, max_part
         if (N_excess.gt.0.0) then
            plant_part_fract = divide (N_capacity(part)
     :                               , N_capacity_sum, 0.0)
            dlt_N_green(part) = g_N_demand(part)
     :                        + N_excess * plant_part_fract
          else
            plant_part_fract = divide (g_N_demand(part)
     :                            , N_demand, 0.0)
            dlt_N_green(part) = N_uptake_sum * plant_part_fract
          endif
1300  continue
 
      dlt_N_green(grain) = 0.0
 
      call bound_check_real_var (
     :             sum_real_array (dlt_N_green, max_part)
     :           , N_uptake_sum, N_uptake_sum
     :           , 'dlt_N_green mass balance')
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sproc_plant_death1 (
     .          c_tt_emerg_limit,
     .          g_current_stage,
     .          g_plants,
     .          g_tt_tot,
     .          g_dlt_plants_all,
 
     .          g_lai,
     .          g_dlt_slai,
 
     .          g_cswd_photo,
     .          g_leaf_no,
     .          c_leaf_no_crit,
     .          c_swdf_photo_limit,
     .          g_swdef_photo,
     .          c_swdf_photo_rate,
     .          g_dlt_plants_water,
     .          g_dlt_plants_dead)
 
*     ===========================================================
      implicit none
      include 'const.inc'
      include 'CropDefCons.inc'
      include 'error.pub'
      include 'science.pub'                         
      include 'data.pub'                         

*+  Sub-Program Arguments
      REAL c_tt_emerg_limit      ! (INPUT)  maximum degree days allowed fo
      REAL g_current_stage       ! (INPUT)  current phenological stage
      REAL g_plants              ! (INPUT)  Plant density (plants/m^2)
      REAL g_tt_tot(*)           ! (INPUT)  the sum of growing degree days
      real g_dlt_plants_all      ! (OUTPUT) change in plant number
*
      real g_lai
      real g_dlt_slai
*
      real g_cswd_photo(*)
      real g_leaf_no(*)
      real c_leaf_no_crit
      real c_swdf_photo_limit
      real g_swdef_photo
      real c_swdf_photo_rate
      real g_dlt_plants_water
      real g_dlt_plants_dead

*+  Purpose
*       crop death
*       works out how many plants to kill on an area basis

*+  Changes
*      5/9/96 dph
*      970912 scc - simplified the thing!
*
*   Called by _process in _main
*   Calls: srop_failure_emergence1,srop_death_drought1 in crop

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sproc_plant_death1')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
         call srop_failure_emergence1 (sowing, emerg, now,
     .          c_tt_emerg_limit,
     .          g_current_stage,
     .          g_plants,
     .          g_tt_tot,
     .          g_dlt_plants_all)
          call srop_death_drought1 (
     .          g_cswd_photo,
     .          g_leaf_no,
     .          c_leaf_no_crit,
     .          c_swdf_photo_limit,
     .          g_swdef_photo,
     .          c_swdf_photo_rate,
     .          g_plants,
     .          g_dlt_plants_water)
 
!scc Don't really need a call to calculate a minimum!!!!
 
        g_dlt_plants_dead = min (g_dlt_plants_all
     :          ,g_dlt_plants_water)
 
!         call srop_death_actual1 (
!     .          g_dlt_plants_all,
!     .          g_dlt_plants_water,
!     .          dlt_plants
!     .            )
 
!        if leaves are killed from frost, g_dlt_slai is set to g_lai
!        need to kill plant if lai = 0
!        gmc & rlv
!
         if (stage_is_between(flag_leaf,maturity,
     .      g_current_stage)) then
            if (reals_are_equal(g_dlt_slai, g_lai)) then
               g_dlt_plants_dead = -g_plants
            endif
         endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sproc_leaf_area_actual1 (
     .          g_current_stage,
     .          g_dlt_lai,
     .          g_dlt_lai_stressed,
     .          g_dlt_dm_green,
     .          c_sla_max
     .          )
*     ===========================================================
      implicit none
      include   'convert.inc'
      include   'CropDefCons.inc'
      include 'science.pub'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       g_current_stage
      real       g_dlt_lai
      real       g_dlt_lai_stressed
      real       g_dlt_dm_green(*)
      real       c_sla_max

*+  Purpose
*       Simulate actual crop leaf area development - checks that leaf area
*       development matches DM production and calculates tiller production.
*       Requires g_dlt_lai_stressed from srop_leaf_area_stressed1 after
*       dlt_lai_pot has been calculated by srop_leaf_area_devel_plant1

*+  Changes
*      250894 jngh specified and programmed
*      240596 glh  added tillering

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sproc_leaf_area_actual1')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
         ! limit the delta leaf area by carbon supply
!glh/scc but don't do when crop small - results in many errors
 
      if (stage_is_between (emerg, endjuv
     :                        , g_current_stage))  then
 
           g_dlt_lai = g_dlt_lai_stressed
 
      elseif (stage_is_between (endjuv, maturity
     :      , g_current_stage))  then
 
            g_dlt_lai = u_bound (g_dlt_lai_stressed
     :                   , g_dlt_dm_green(leaf)*c_sla_max * smm2sm)
 
      else
 
           g_dlt_lai = g_dlt_lai_stressed
 
      endif
 
      call pop_routine (my_name)
      return
      end




*     ===========================================================
      subroutine sunf_leaf_area_pot_new (
     .          begin_stage,
     .          end_stage_TPLA_plateau,
     .          current,
     .          g_phase_tt,
     .          g_days_tot,
     .          g_current_stage,
     .          g_leaf_no_final,
     .          c_initial_tpla,
     .          g_tiller_no_fertile,
     .          c_tiller_coef,
     .          p_main_stem_coef,
     .          g_tt_tot,
     .          c_tpla_inflection_ratio,
     .          g_tpla_today,
     .          g_tpla_yesterday,
     .          p_tpla_prod_coef,
     .          g_plants,
     .          g_lai,
     .          g_dlt_lai_pot)
*     ===========================================================
      implicit none
      include 'convert.inc'
      include 'CropDefCons.inc'
      include 'science.pub'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    begin_stage                !stage number of start
      integer    end_stage_TPLA_plateau     !stage number to stop TPLA growth
      integer    current                    !stage number now
      real       g_phase_tt(*)
      real       g_days_tot(*)
      real       g_current_stage
      real       g_leaf_no_final
      real       c_initial_tpla
      real       g_tiller_no_fertile
      real       c_tiller_coef
      real       p_main_stem_coef
      real       g_tt_tot(*)
      real       c_tpla_inflection_ratio
      real       g_tpla_today
      real       g_tpla_yesterday
      real       p_tpla_prod_coef
      real       g_plants
      real       g_lai
      real       g_dlt_lai_pot           ! (OUTPUT) change in leaf area

*+  Purpose
*   Return the potential increase in leaf area development (mm^2)
*   calculated on a whole plant basis as determined by thermal time
*
*   Called by srop_leaf_area_potential(2) in croptree.for

*+  Changes
*     12-12-1998   EW modified based on the sorghum version

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_leaf_area_pot2')

*+  Local Variables
      real       tpla_max              ! maximum total plant leaf area (mm^2)
      real       tt_since_begin        ! deg days since begin TPLA Period
      real       tpla_inflection       ! inflection adjusted for leaf no.
      real       tt_begin_to_end_TPLA  ! thermal time for TPLA period
c     real       tt_emerg_to_init
      real       density_factor

*  calls
      real      sunflower_dens_fact
   
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      !Once leaf no is calculated maximum plant leaf area is determined
 
      if (on_day_of (begin_stage, g_current_stage, g_days_tot)) then
         g_lai = c_initial_tpla * smm2sm * g_plants
      endif
 
      if (stage_is_between (begin_stage, end_stage_TPLA_plateau,
     .                      g_current_stage) .and.
     .    g_phase_tt(end_stage_TPLA_plateau) .gt.0.0) then
 
        tt_begin_to_end_TPLA = sum_between(begin_stage,
     :                          end_stage_TPLA_plateau,g_phase_tt)

* the original sorghum equation 
*         tpla_max = (((g_tiller_no_fertile + 1.0) ** c_tiller_coef)
*     :            * g_leaf_no_final ** p_main_stem_coef) * scm2smm
     
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  Enli Begin
*THE GENERIC WAY TO SIMULATE TPLA_MAX WOULD BE:

*         tpla_max = a * (((g_tiller_no_fertile + 1.0) ** c_tiller_coef)
*     :            * g_leaf_no_final ** p_main_stem_coef) * scm2smm

* for sorghum    a = 1    <------ a is introduced to generalise the tpla_max equation
* for sunflower  a = 16.005, g_tiller_no_fertile = 0, c_tiller_coef = 1.8117

* 02/03/1999 Scott gave me the power equation for sunflower as y = 16.005*x^1.8117   
*                                                              r2= 0.9334 

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  Enli End    

*  scc introduced new function for tpla_max  on 25/09/98
*  This works out tpla_max as function of final leaf number
*==============================================================================

       tpla_max = (445.97 * g_leaf_no_final - 5828.86) * scm2smm 
         
       density_factor = sunflower_dens_fact(g_plants) 

         !density_factor = exp(0.2 - 0.16 * g_plants)
         !density_factor = density_factor/0.549
         
       tpla_max = tpla_max * density_factor  !In the sorghum version no density factor is used
 
 
cscc 10/95 fixing the beta inflection coefficient as halfway to thermal
c time of flag_leaf expanded. Code needs work as the halfway point jumps
c around a bit as we progress (espec. when final_leaf_no is reset at floral in

* scc fixed tpla_inflection  on 25/09/98
* For sunflower, this inflection point is set to 0.65 in sunf.ini. This is not the
* same as it was in the old model where the tpla_inflection is taken from the
* estimate of tt_emerg_to_budvis
* With tbase of 8 (original model), the fitted inflection was 517 deg days (over 5 genos)
* With tbase of 4, this averaged should be 532 deg days (from SCC calculations in fit tpla.ssc)

*the p_tpla_prod_coef is allowed to vary for cultivars in the sunf.par file

         tt_since_begin = sum_between (begin_stage, current, g_tt_tot)

         tpla_inflection = tt_begin_to_end_TPLA *
     :           c_tpla_inflection_ratio

c scc end of changes for tpla (more below)
  
!========================================================================
!The following remains the same for sorghum and sunflower

         g_tpla_today = divide (Tpla_max
     :              , (1.0 + exp(-p_tpla_prod_coef
     :                        * (tt_since_begin - tpla_inflection)))
     :              , 0.0)
 
         if (g_tpla_today .lt. g_tpla_yesterday)then
            g_tpla_today = g_tpla_yesterday
         endif
 
 
         g_dlt_lai_pot = (g_tpla_today - g_tpla_yesterday)
     .                  *smm2sm * g_plants
 
         g_tpla_yesterday = g_tpla_today
 
      else
      
!Beyond TPLA growth stage
         g_dlt_lai_pot = 0.0
 
      endif
 
 
      call pop_routine (my_name)
      return
      end



* ====================================================================
       real function sunflower_dens_fact(plants)
* ====================================================================
      implicit none
      include 'data.pub'
      include 'error.pub'    
      
      real     plants                     

*+  Purpose
*   this function returns the scaling factor for total leaf area as
*   affected by planting density.

*+  Notes
*   this factor is mostly based on the work of sadras and hall (1988)
*   (field crops research 18, 185-196).  there are some changes however.
*   firstly the abovementioned worked on relative leaf area normalised
*   against a planting density of 1 plant/m^2.  this function uses their
*   density function but normalises for a density of 5 plants/m^2.
*   also, there appears to be an error in the sadras and hall paper.
*   the plotted function and the equation stated do not match.  the equation
*   is stated to be
*                    ln(ra) = -0.20 - 0.16 * density
*   the equation that agrees with their plot versus observed data is
*                    ln(ra) =  0.20 - 0.16 * density
*   we shall use the latter equation.

*+  Changes
*   2-12-93 nih specified and programmed

*+  Constant Values
      real dint                           ! intercept for density function
      parameter (dint = 0.2)
*
      real dslope                         ! slope for density function
      parameter (dslope = -0.16)
*
      character*(*) myname                ! name of this function
      parameter (myname = 'sunflower_dens_fact')

*+  Local Variables
      real density                        ! effective planting density
                                          ! (plants/m^2)
      real dens_fact                      ! sadras and hall's density
                                          ! factor (0-1)

*- Implementation Section ----------------------------------
      call Push_routine(myname)
 
      ! planting densities above 10 plants/m^2 have no additional effect
      !                   on total leaf area.
      density = u_bound (plants, 10.0)
 
      dens_fact = exp (dint + dslope * density)
      dens_fact = bound (dens_fact, 0.0, 1.0)
      ! adjust the normalised density factor for a planting density of
      !                        5 plants/m^2
      dens_fact = dens_fact / 0.549
 
      sunflower_dens_fact = dens_fact
 
      call Pop_routine (myname)
      return
      end




*     ===========================================================
      subroutine Read_Constants_Sunf ()
*     ===========================================================
      use CropModModule
      implicit none
      include   'const.inc'
      include 'read.pub'
      include 'error.pub'
      include 'datastr.pub'

*+  Purpose
*       Crop initialisation - reads constants from constants file

*+  Changes
*     010994 sc   specified and programmed
*     070495 psc added extra constants (leaf_app etc.)
*     110695 psc added soil temp effects on plant establishment
*     270995 scc added leaf area options

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'Read_Constants_Sunf')
*
      character  section_name*(*)
      parameter (section_name = 'constants')


*+  Local Variables
      integer    numvals               !number of values returned


*- Implementation Section ----------------------------------
 
      call push_routine (my_name)


      call write_string (
     :                  new_line//'    - Reading constants')
 
      call read_char_var (section_name
     :                     , 'crop_type', '()'
     :                     , c%crop_type, numvals)



c      PRINT *, "c%crop_type = ", c%crop_type


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
 
 
         !    sunf_root_depth

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
     :                     , 0.0, 0.3)

      call read_real_array (section_name
     :                     , 'y_rel_root_rate', max_table, '()'
     :                     , c%y_rel_root_rate, c%num_plant_rld
     :                     , 0.0, 1.0)
 
         !    sunf_leaf_area_init

      call read_real_var (section_name
     :                    , 'initial_tpla', '(mm^2)'
     :                    , c%initial_tpla, numvals
     :                    , 0.0, 100000.0)
 
         ! TEMPLATE OPTION
         !    sunf_leaf_area_devel

      call read_real_var (section_name
     :                    , 'sla_max', '(mm^2/g)'
     :                    , c%sla_max, numvals
     :                    , 0.0, 100000.0)
 
         ! TEMPLATE OPTION
         !    sunf_leaf_area_devel_plant

      call read_real_var (section_name
     :                    , 'tiller_coef', '()'
     :                    , c%tiller_coef , numvals
     :                    , 0.0, 10.0)
 
      call read_real_var (section_name
     :                    , 'tpla_inflection_ratio', '()'
     :                    , c%tpla_inflection_ratio , numvals
     :                    , 0.0, 1.0)
 
! scc This parameter moved from sunf.ini to sunf.par file
!      call read_real_var (section_name
!     :                    , 'main_stem_coef', '()'
!     :                    , c%main_stem_coef, numvals
!     :                    , 0.0, 10.0)
 
         !    sunf_height

!      call read_real_var (section_name
!     :                    , 'height_max', '(mm)'
!     :                    , c%height_max, numvals
!     :                    , 0.0, 10000.0)
 
!      call read_real_var (section_name
!     :                    , 'height_stem_slope', '(mm/g/stem)'
!     :                    , c%height_stem_slope, numvals
!     :                    , 0.0, 1000.0)
 
         !    sunf_get_cultivar_params

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
         !    sunf_grain_no

      call read_real_var (section_name
     :                    , 'head_grain_no_crit', '()'
     :                    , c%head_grain_no_crit, numvals
     :                    , 0.0, 1000.0)
 
         !    sunf_plants_barren

      call read_real_var (section_name
     :                    , 'barren_crit', '()'
     :                    , c%barren_crit, numvals
     :                    , 0.0, 1.0)
 
         !    sunf_germination

      call read_real_var (section_name
     :                    , 'pesw_germ', '(mm/mm)'
     :                    , c%pesw_germ, numvals
     :                    , 0.0, 1.0)
 
         ! TEMPLATE OPTION
         !    sunf_grain_no

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
 
         !    sunf_leaf_appearance

      call read_real_var (section_name
     :                    , 'leaf_no_at_emerg', '()'
     :                    , c%leaf_no_at_emerg, numvals
     :                    , 0.0, 100.0)
 
         !    sunf_N_uptake

      call read_real_var (section_name
     :                    , 'no3_diffn_const', '(days)'
     :                    , c%NO3_diffn_const, numvals
     :                    , 0.0, 100.0)
 
      call read_char_var (section_name
     :                     , 'n_supply_preference', '()'
     :                     , c%n_supply_preference, numvals)
 
         !    sunf_phenology_init

 
      call read_real_var (section_name
     :                    , 'shoot_lag', '(oC)'
     :                    , c%shoot_lag, numvals
     :                    , 0.0, 100.0)
 
      call read_real_var (section_name
     :                    , 'shoot_rate', '(oC/mm)'
     :                    , c%shoot_rate, numvals
     :                    , 0.0, 100.0)
 
 !     call read_real_var (section_name
 !    :                    , 'photoperiod_base', '(hr)'
 !    :                    , c%photoperiod_base, numvals
 !    :                    , 0.0, 24.0)
 
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
 
         !    sunf_dm_init

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
 
         !    sunf_get_root_params

!      call read_real_var (section_name
!     :                    , 'll_ub', '()'
!     :                    , c%ll_ub, numvals
!     :                    , 0.0, 1000.0)
 
      call read_real_var (section_name
     :                    , 'kl_ub', '()'
     :                    , c%kl_ub, numvals
     :                    , 0.0, 1000.0)
 
         !    sunf_leaf_no_final

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
 
         !    sunf_retranslocate

      call read_real_var (section_name
     :                    , 'stem_trans_frac', '()'
     :                    , c%stem_trans_frac, numvals
     :                    , 0.0, 1.0)
 
      call read_real_var (section_name
     :                    , 'leaf_trans_frac', '()'
     :                    , c%leaf_trans_frac, numvals
     :                    , 0.0, 1.0)
 
      call read_real_var (section_name
     :                    , 'flower_trans_frac', '()'
     :                    , c%flower_trans_frac, numvals
     :                    , 0.0, 1.0)
 
         !    sunf_watck

      call read_real_var (section_name
     :                    , 'minsw', '()'
     :                    , c%minsw, numvals
     :                    , 0.0, 1000.0)
 
         ! TEMPLATE OPTION
         !    sunf_dm_grain

      call read_real_var (section_name
     :                    , 'swdf_grain_min', '()'
     :                    , c%swdf_grain_min, numvals
     :                    , 0.0, 100.0)
 
         ! TEMPLATE OPTION
         !    sunf_dm_grain_hi

      call read_real_var (section_name
     :                    , 'hi_min', '()'
     :                    , c%hi_min, numvals
     :                    , 0.0, 100.0)
 
         !    sunf_N_dlt_grain_conc

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
 
         !    sunf_leaf_death

cSCC changed lower limit from 10.0 to 0.0
      call read_real_var (section_name
     :                    , 'leaf_no_dead_const', '()'
     :                    , c%leaf_no_dead_const, numvals
     :                    , 0.0, 100.0)
 
      call read_real_var (section_name
     :                    , 'leaf_no_dead_slope', '()'
     :                    , c%leaf_no_dead_slope, numvals
     :                    , 0.0, 100.0)
 
         !    sunf_get_other_variables

         ! checking the bounds of the bounds..
      call read_integer_var (section_name
     :                    , 'year_ub', '()'
     :                    , c%year_ub, numvals
     :                    , 1800, 2100)
 
      call read_integer_var (section_name
     :                    , 'year_lb', '()'
     :                    , c%year_lb, numvals
     :                    , 1800, 2100)

c      PRINT *, "c%year_lb = ", c%year_lb
c      PRINT *, "c%year_ub = ", c%year_ub

 
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
     :                    , -30.0, 60.0)
 
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
 
         !    sunf_event

      call read_real_var (section_name
     :                    , 'grn_water_cont', '(g/g)'
     :                    , c%grn_water_cont, numvals
     :                    , 0.0, 1.0)
 
       call read_real_var (section_name
     :                    , 'grain_energy', '()'
     :                    , c%grain_energy, numvals
     :                    , 0.0, 2.0)

       call read_real_var (section_name
     :                    , 'frac_pod2grain', '()'
     :                    , c%frac_pod2grain, numvals
     :                    , 0.0, 1.0)

         !    sunf_dm_partition

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
         !    sunf_grain_no

      call read_real_var (section_name
     :                    , 'htstress_coeff', '()'
     :                    , c%htstress_coeff, numvals
     :                    , 0.0, 1.0)
 
         !    sunf_dm_senescence

      call read_real_var (section_name
     :                    , 'dm_root_sen_frac', '()'
     :                    , c%dm_root_sen_frac, numvals
     :                    , 0.0, 1.0)
 
      call read_real_var (section_name
     :                    , 'dm_leaf_sen_frac', '()'
     :                    , c%dm_leaf_sen_frac, numvals
     :                    , 0.0, 1.0)
 
         !    sunf_dm_dead_detachment

      call read_real_array (section_name
     :                    , 'dead_detach_frac', max_part, '()'
     :                    , c%dead_detach_frac, numvals
     :                    , 0.0, 1.0)
 
      call read_real_var (section_name
     :                    , 'dm_leaf_detach_frac', '()'
     :                    , c%dm_leaf_detach_frac, numvals
     :                    , 0.0, 1.0)
 
         ! TEMPLATE OPTION
         !    sunf_leaf_area_devel

c      call read_real_var (section_name
c     :                    , 'leaf_no_correction', '()'
c     :                    , c%leaf_no_correction, numvals
c     :                    , 0.0, 100.0)
 
         ! TEMPLATE OPTION
         !    sunf_leaf_size

         ! TEMPLATE OPTION
         !    sunf_leaf_area_sen_light

      call read_real_var (section_name
     :                   , 'lai_sen_light', '(m^2/m^2)'
     :                   , c%lai_sen_light, numvals
     :                   , 3.0, 20.0)
 
      call read_real_var (section_name
     :                    , 'sen_light_slope', '()'
     :                    , c%sen_light_slope, numvals
     :                    , 0.0, 100.0)
 
 
         ! TEMPLATE OPTION
         !    sunf_leaf_area_sen_frost

      call read_real_array (section_name
     :                   , 'x_temp_senescence', max_table, '(oC)'
     :                   , c%x_temp_senescence, c%num_temp_senescence
     :                   , -20.0, 20.0)
 
      call read_real_array (section_name
     :                   , 'y_senescence_fac', max_table, '()'
     :                   , c%y_senescence_fac, c%num_temp_senescence
     :                   , 0.0, 1.0)
 
         ! TEMPLATE OPTION
         !    sunf_leaf_area_sen_water

      call read_real_var (section_name
     :                    , 'sen_rate_water', '()'
     :                    , c%sen_rate_water, numvals
     :                    , 0.0, 100.0)
 
         ! TEMPLATE OPTION
         !    sunf_leaf_area_sen_light1

      call read_real_var (section_name
     :                    , 'sen_light_time_const', '(days)'
     :                    , c%sen_light_time_const, numvals
     :                    , 0.0, 50.0)
 
      call read_real_var (section_name
     :                    , 'sen_radn_crit', '(Mj/m^2)'
     :                    , c%sen_radn_crit, numvals
     :                    , 0.0, 10.0)
 
         ! TEMPLATE OPTION
         !    sunf_leaf_area_sen_frost1

      call read_real_var (section_name
     :                    , 'frost_kill', '(oC)'
     :                    , c%frost_kill, numvals
     :                    , -6.0, 6.0)
 
        ! TEMPLATE OPTION
         !    sunf_leaf_area_sen_water1

      call read_real_var (section_name
     :                    , 'sen_water_time_const', '(days)'
     :                    , c%sen_water_time_const, numvals
     :                    , 0.0, 50.0)
 
      call read_real_var (section_name
     :                    , 'sen_threshold', '()'
     :                    , c%sen_threshold, numvals
     :                    , 0.0, 10.0)
 
         ! TEMPLATE OPTION
         !    sunf_leaf_area_sen_age1

      call read_real_var (section_name
     :                    , 'spla_slope', '(oC/leaf)'
     :                    , c%spla_slope, numvals
     :                    , 0.0, 1000.0)
 
         !    sunf_phenology_init

      call read_real_var (section_name
     :                   , 'twilight', '(o)'
     :                   , c%twilight, numvals
     :                   , -90.0, 90.0)
 
         ! TEMPLATE OPTION
         !    sunf_heat_stress

      call read_real_var (section_name
     :                   , 'temp_grain_crit_stress', '(oC)'
     :                   , c%temp_grain_crit_stress, numvals
     :                   , 20.0, 50.0)
 
         !    sunf_N_conc_limits

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
 
      call read_real_var (section_name
     :                   , 'n_conc_crit_root', '()'
     :                   , c%N_conc_crit_root, numvals
     :                   , 0.0, 100.0)
 
      call read_real_var (section_name
     :                   , 'n_conc_max_root', '()'
     :                   , c%N_conc_max_root, numvals
     :                   , 0.0, 100.0)
 
      call read_real_var (section_name
     :                   , 'n_conc_min_root', '()'
     :                   , c%N_conc_min_root, numvals
     :                   , 0.0, 100.0)
 
         !    Maize_N_init
 
      call read_real_array (section_name
     :                     , 'n_init_conc', max_part, '()'
     :                     , c%n_init_conc, numvals
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
 
         !    sunf_rue_reduction

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
         !    sunf_dm_grain

      call read_real_array (section_name
     :                     , 'x_temp_grain', max_table, '(oC)'
     :                     , c%x_temp_grain, c%num_temp_grain
     :                     , 0.0, 100.0)
 
      call read_real_array (section_name
     :                     , 'y_grain_rate', max_table, '()'
     :                     , c%y_grain_rate, c%num_temp_grain
     :                     , 0.0, 1.0)
 
         !    sunf_tt

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
         !    sunf_tt_other

      ! call read_real_array (section_name
      !:                     , 'x_temp_other', max_table, '(oC)'
      !:                     , c%x_temp_other, c%num_temp_other
      !:                     , 0.0, 100.0)
 
      ! call read_real_array (section_name
      !:                     , 'y_tt_other', max_table, '(oC)'
      !:                     , c%y_tt_other, c%num_temp_other
      !:                     , 0.0, 100.0)
 
         ! TEMPLATE OPTION
         !    sunf_tt_curv

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
 
 
 
 
!===================================================================================== 
!Effect of minimum temperature on harvest index increase, based on Mike Bange's thesis - ENLI
      call read_real_array (section_name
     :        , 'x_hi_incr_min_temp', max_table, '(0-1)'
     :        , p%x_hi_incr_min_temp, p%mum_hi_incr_min_temp
     :        , 0.0, 50.0)
 
      call read_real_array (section_name
     :        , 'y_hi_incr_reduct_fac', max_table, '(0-1)'
     :        , p%y_hi_incr_reduct_fac, p%mum_hi_incr_min_temp
     :        , 0.0, 1.0)
     


       call read_integer_var(section_name
     :                   , 'tt_switch_stage', '()'
     :                   , p%tt_switch_stage, numvals
     :                   , 1, 10)
      
     

      call read_real_array (section_name
     :        , 'x_node_num_lar', max_table, '(0-1)'
     :        , p%x_node_num_lar, p%num_node_lar
     :        , 0.0, 50.0)

      call read_real_array (section_name
     :        , 'y_node_lar', max_table, '(0-1)'
     :        , p%y_node_lar, p%num_node_lar
     :        , 0.0, 100.0)

        call read_integer_var(section_name
     :                   , 'determinate_crop', '()'
     :                   , p%determinate_crop, numvals
     :                   , 0, 2)
     

cew - added this section

C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C          VALUE LIMITS - MAX AND MINS
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      !-----------------------------------------------------------------
      !Canopy height
      call read_real_var (section_name
     :                    , 'canopy_height_max', '()'
     :                    , c%height_max, numvals
     :                    , 0.0, 5000.0)

      !-----------------------------------------------------------------
      !ROOT PARAMETERS
      call read_real_var (section_name
     :                    , 'll_ub', '()'
     :                    , c%ll_ub, numvals
     :                    , 0.0, 1000.0)
 
      call read_real_var (section_name
     :                    , 'kl_ub', '()'
     :                    , c%kl_ub, numvals
     :                    , 0.0, 1000.0)



      call read_real_var (section_name
     :                    , 'nh4_ub', '(kg/ha)'
     :                    , c%NH4_ub, numvals
     :                    , 0.0, 100000.0)
 
      call read_real_var (section_name
     :                    , 'nh4_lb', '(kg/ha)'
     :                    , c%NH4_lb, numvals
     :                    , 0.0, 100000.0)
 
      call read_real_var (section_name
     :                    , 'nh4_min_ub', '(kg/ha)'
     :                    , c%NH4_min_ub, numvals
     :                    , 0.0, 100000.0)
 
      call read_real_var (section_name
     :                    , 'nh4_min_lb', '(kg/ha)'
     :                    , c%NH4_min_lb, numvals
     :                    , 0.0, 100000.0)


      call pop_routine (my_name)
      return
      end


*     ===========================================================
      subroutine Read_Cultivar_Params_Sunf (cultivar)
*     ===========================================================
      use CropModModule
      implicit none
      include   'const.inc'            ! new_line,  blank
      include 'read.pub'
      include 'error.pub'                         

*+  Sub-Program Arguments
      character  cultivar*(*)          ! (INPUT) keyname of cultivar in crop
                                       ! parameter file

*+  Purpose
*       Get cultivar parameters for named cultivar, from crop parameter file.

*+  Changes
*       090994 sc   specified and programmed
*       10/6/98 dph fixed invalid format specification.

*+  Calls

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Read_Cultivar_Params_Sunf')

*+  Local Variables
      character  string*200            ! output string
      integer    numvals               ! number of values read
      integer    i

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      call write_string (
     :                 new_line//'   - Reading Cultivar Parameters')
 
         ! TEMPLATE OPTION
         !   sunf_leaf_area_devel_plant

!scc This coeff. moved from sunf.ini to sunf.par file
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
         !   sunf_leaf_area_sen_age1

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
         !   sunf_check_grain_no  sunf_grain_no

      call read_real_var (cultivar
     :                    , 'head_grain_no_max', '()'
     :                    , p%head_grain_no_max, numvals
     :                    , 0.0, c%head_grain_no_max_ub)
 
         ! TEMPLATE OPTION
         !   sunf_dm_grain

      call read_real_var (cultivar
     :                    , 'grain_gth_rate', '()'
     :                    , p%grain_gth_rate, numvals
     :                    , 0.0, c%grain_gth_rate_ub)
 
         !   sunf_phenology_init

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
     :                    , 'rel_leaf_init_rate', '()'
     :                    , p%rel_leaf_init_rate, numvals
     :                    , 0.0, 1.0)
 
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
     :                    , -1000.0, 200.0)
 
      call read_real_var (cultivar
     :                    , 'tt_fi_to_flag', '()'
     :                    , p%tt_fi_to_flag, numvals
     :                    , 0.0, 1000.0)

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
 
      write (string, '(4x, a, f7.2)')
     :                'rel_leaf_init_rate       = '
     :               , p%rel_leaf_init_rate
      call write_string (string)


      write (string, '(4x, a, f7.2)')
     :                'photoperiod_crit1        = '
     :               , p%photoperiod_crit1
      call write_string (string)

      write (string, '(4x, a, f7.2)')
     :                'photoperiod_crit2        = '
     :               , p%photoperiod_crit2
      call write_string (string)

      write (string, '(4x, a, f7.2)')
     :                'photoperiod_slope        = '
     :               , p%photoperiod_slope
      call write_string (string)

      write (string, '(4x, a, f7.2)')
     :                'tt_emerg_to_endjuv       = '
     :               , p%tt_emerg_to_endjuv
      call write_string (string)


      write (string, '(4x, a, f7.2)')
     :                'tt_endjuv_to_init        = '
     :               , p%tt_endjuv_to_init
      call write_string (string)


      write (string, '(4x, a, f7.2)')
     :                'tt_fi_to_flag            = '
     :               , p%tt_fi_to_flag
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
     :                'tt_flower_to_maturity    = '
     :               , p%tt_flower_to_maturity
      call write_string (string)

      write (string, '(4x, a, f7.2)')
     :                'tt_maturity_to_ripe      = '
     :               , p%tt_maturity_to_ripe
      call write_string (string)
 

c      write (string, '(4x, a, i4)')
c     :                'est_days_endjuv_to_init  = '
c     :               , p%est_days_endjuv_to_init

c      call write_string (string)
c      write (string, '(4x, a, f7.2)')
c     :                'pp_endjuv_to_init        = '
c     :               , p%pp_endjuv_to_init
c      call write_string (string)

      write (string, '(4x, a, f7.2)')
     :                'head_grain_no_max        = '
     :               , p%head_grain_no_max
      call write_string (string)
 
      write (string, '(4x, a, f7.2)')
     :                'grain_gth_rate           = '
     :               , p%grain_gth_rate
      call write_string (string)


         ! TEMPLATE OPTION
       write (string, '(4x, a, f7.3)')
     :                'hi_incr                  = '
     :               , p%hi_incr
       call write_string (string)
 
         ! TEMPLATE OPTION
 
      write (string, '(4x, a, 10f7.2)')
     :                'x_hi_max_pot_stress      = '
     :               , (p%x_hi_max_pot_stress(i), i=1,p%num_hi_max_pot)
      call write_string (string)
 
      write (string, '(4x, a, 10f7.2)')
     :                'y_hi_max_pot             = '
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
     :                'x_stem_wt                = '
     :               , (p%x_stem_wt(i), i=1,p%num_stem_wt)
      call write_string (string)
 
      write (string, '(4x, a, 10f7.1)')
     :                'y_height                 = '
     :               , (p%y_height(i), i=1,p%num_stem_wt)
      call write_string (string)
 
      string = '    ------------------------------------------------'
      call write_string (string)
 
      call write_string (new_line//new_line)
 
      call pop_routine (my_name)

      return
      end


