C     Last change:  E    12 May 2000   10:23 am

!      INCLUDE 'CropMod.inc'

*================================================================
      subroutine Crop_Process ()
*=================================================================
*+  Purpose
*       Simulate crop processes.  These include biomass production,
*       phenological stages, plant component development,
*       water uptake and nitrogen uptake, and plant senescense.

*+  Changes
*      250894 sc   specified and programmed

*+  Include section

      use CropModModule
      implicit none

      include 'const.inc'
      include 'science.pub'                          
      include 'data.pub'                          
      include 'error.pub'                         


*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Crop_Process')


*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (g%plant_status.eq.status_alive) then


         if (c%crop_type .eq. 'wheat') then

           call Dynamic_Processes ()

         elseif (c%crop_type .eq. 'sunflower') then

           call sunf_process ()

         else

         end if

         !------------------------------------------------
         !CROP DEATH of PLANTS (cf. plant part pools)
         !call Crop_Death(1)
 
         !Check to see if plant death should terminate crop
         if(reals_are_equal (g%dlt_plants_dead + g%plants, 0.0))then
            call Kill_Crop (
     .          g%plant_status,
     .          g%dm_green,
     .          g%dm_senesced,
     .          g%dm_dead)
         endif

      else
         ! plant_status not alive
      endif
 

      !Detachment and Cleanup after dynamic process
      !modify detachment so that it is generalised in cropother.for

      call Crop_Detachment(1)
      call Crop_Cleanup()        !CODE is almost same as maize


      call pop_routine (my_name)
 
      return
      end



*================================================================
      subroutine Dynamic_Processes ()
*=================================================================

      use CropModModule
      implicit none

      include 'const.inc'
      include 'science.pub'                          
      include 'data.pub'
      include 'error.pub'

*+  Calls

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Dynamic_Processes')

      INTEGER GetSwitchCode


*- Implementation Section ----------------------------------
      call push_routine (my_name)

      !iwheat uses a accumulative radiation value
      call iw_rad_accum_10d(g%radn,g%current_stage,g%days_tot,
     :                      g%rad_accum,g%accum_rad_10d)


      !SECTION 8 - nitrogen stress factor is used everywhere
      call nitrogen_stress           (GetSwitchCode(c%nit_switch,7))


      !SECTION 1:  PLANT WATER REALTION
      call water_supply              (GetSwitchCode(c%wat_switch,1))
      call water_demand              (GetSwitchCode(c%wat_switch,2))
      call water_uptake              (GetSwitchCode(c%wat_switch,3))
      call water_stress              (GetSwitchCode(c%wat_switch,4))


      !SECTION 2:  PHENOLOGICAL DEVELOPMENT
      call phenology_initalisation   (GetSwitchCode(c%phen_switch,1))
*     call thermal_time              (GetSwitchCode(c%phen_switch,2))   !thermal time will be separated from phenology subroutine
      call vernalization             (GetSwitchCode(c%phen_switch,3))
      call photoperiodism            (GetSwitchCode(c%phen_switch,4))
      call phenology                 (GetSwitchCode(c%phen_switch,5))

      !SECTION 3: CARBOHYDRATE/BIOMASS PRODUCTION
      call biomass_potential         (GetSwitchCode(c%carb_switch,1))
      call biomass_actual            (GetSwitchCode(c%carb_switch,2))


      !SECTION 4: CARBOHYDRATE/BIOMASS PARTITIONING AND ORGAN BIOMASS GROWTH
      call biomass_initialisation    (GetSwitchCode(c%part_switch,1))
      call biomass_yieldpart_demand  (GetSwitchCode(c%part_switch,2))
      call biomass_partition         (GetSwitchCode(c%part_switch,3))
      call biomass_retranslocation   (GetSwitchCode(c%part_switch,4))


      !SECTION 5: CANOPY FORMATION - ABOVE GROUND PART
      call leaf_number_initialisation(GetSwitchCode(c%can_switch,1))
      call leaf_initiation           (GetSwitchCode(c%can_switch,2))
      call leaf_appearance           (GetSwitchCode(c%can_switch,3))
      call tillering_initialisation  (GetSwitchCode(c%can_switch,4))
      call tillering                 (GetSwitchCode(c%can_switch,5))

*     call leaf_area_initialisation  (GetSwitchCode(c%area_switch,1))
      call leaf_area_potential       (GetSwitchCode(c%can_switch,6))
      call leaf_area_actual          (GetSwitchCode(c%can_switch,7))
      call crop_height               (GetSwitchCode(c%can_switch,8))

      !SECTION 6: ROOT SYSTEM FORMATION - UNDER GROUND PART
      call root_depth_initialisation (GetSwitchCode(c%root_switch,1))
      call root_depth                (GetSwitchCode(c%root_switch,2))
      call root_length_initialisation(GetSwitchCode(c%root_switch,3))
      call root_length               (GetSwitchCode(c%root_switch,4))

      !SECTION 7: PLANT SENESCENCE AND NITROGEN IN SENENSCED PARTS
      call senescence_leaf_area      (GetSwitchCode(c%sen_switch,1))
      call senescence_biomass        (GetSwitchCode(c%sen_switch,2))
      call senescence_root_length    (GetSwitchCode(c%sen_switch,3))
      call senescence_nitrogen       (GetSwitchCode(c%sen_switch,4))


      !SECTION 8: PLANT NITROGEN RELATION (DEMAND, UPTAKE AND STRESS)
      call nitrogen_initialisation   (GetSwitchCode(c%nit_switch,1))
      call nitrogen_supply           (GetSwitchCode(c%nit_switch,2))
      call nitrogen_demand           (GetSwitchCode(c%nit_switch,3))
      call nitrogen_uptake           (GetSwitchCode(c%nit_switch,4))
      call nitrogen_partition        (GetSwitchCode(c%nit_switch,5))
      call nitrogen_retranslocation  (GetSwitchCode(c%nit_switch,6))


      call pop_routine (my_name)
 
      return
      end




*     ===========================================================
      subroutine water_supply (Option)
*     ===========================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'science.pub'
      include 'data.pub'
      include 'crp_watr.pub'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option   !(INPUT) template option number

*+  Purpose
*     Soil water supply or potential crop water extraction

*+  Changes
*     990405 ew - specified and programmed

*+  Constant Values
      character  my_name*(*) ! name of procedure
      parameter (my_name = 'water_supply')

*+  Local variables
       INTEGER deepest_layer


*- Implementation Section ----------------------------------
 
      call push_routine (my_name)


      if ((Option.eq.1).or.(Option.eq.2)) then
 
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

      else if (Option .eq. 3) then

        call potential_water_extraction_nw (
     :                  g%current_stage,
     :                  emerg,
     :                  maturity,
     :                  g%lai,
     :                  g%leaf_no,
     :                  max_layer,
     :                  g%root_depth,
     :                  g%root_length,
     :                  g%dlayer,
     :                  p%ll_dep,
     :                  g%dul_dep,
     :                  g%sw_dep,
     :                  c%minsw,
     :                  g%sw_avail,
     :                  g%sw_avail_pot,
     :                  g%sw_supply)
    
      else if (Option.eq.0) then

         !This module is excluded from the model
      else

         call Fatal_error (ERR_internal, 'Invalid template option')
      endif



        deepest_layer = find_layer_no
     :                   (g%root_depth, 
     :                    g%dlayer, 
     :                    max_layer)
     
        g%sw_supply_sum = sum_real_array(g%sw_supply, deepest_layer)

 
      call pop_routine (my_name)
      return
      end


*     ===========================================================
      subroutine water_demand (Option)
*     ===========================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'crp_watr.pub'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option ! (INPUT) template option number

*+  Purpose
*     calculation of daily crop water demand

*+  Changes
*     990405 ew - templated and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'water_demand')

*+  Local variables


*- Implementation Section ----------------------------------


      call push_routine (my_name)


      if ((Option.eq.1).or.(Option.eq.2)) then
 

         call biomass_potential (Option)

         call cproc_transp_eff1(
     :                     c%svp_fract
     :                   , c%transp_eff_cf
     :                   , g%current_stage
     :                   , g%maxt
     :                   , g%mint
     :                   , g%transp_eff
     :                      )

        call cproc_sw_demand1(
     :                     g%dlt_dm_light
     :                   , g%transp_eff
     :                   , g%sw_demand
     :                     )
 

      elseif (Option.eq.3) then
 

         call biomass_potential (Option)

         call cproc_transp_eff_nw(
     :                     c%svp_fract
     :                   , c%transp_eff_cf    !fixed internally for nwheat transp_eff_cf=0.006   ?????????
     :                   , g%current_stage
     :                   , g%maxt
     :                   , g%mint
     :                   , g%transp_eff
     :                      )

        call cproc_sw_demand1(
     :                     g%dlt_dm_light
     :                   , g%transp_eff
     :                   , g%sw_demand
     :                     )

      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine water_uptake (Option)
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
      integer Option      ! (INPUT) template option number

*+  Purpose
*     Soil water uptake

*+  Changes
*     990506  ew - programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'water_uptake')

*+  Local Variables
c     integer    deepest_layer
      integer    layer                 ! layer number of profile ()
      real       ext_sw_supply(max_layer)

*- Implementation Section ----------------------------------

      call push_routine (my_name)
 
      if (p%uptake_source .eq. 'apsim') then
         call crop_get_ext_uptakes(
     :                    p%uptake_source   ! uptake flag
     :                   ,c%crop_type       ! crop type
     :                   ,'water'           ! uptake name
     :                   ,1.0               ! unit conversion factor
     :                   ,0.0               ! uptake lbound
     :                   ,100.0             ! uptake ubound
     :                   ,ext_sw_supply     ! uptake array
     :                   ,max_layer         ! array dim
     :                   )
 
         do 100 layer = 1, g%num_layers
            g%dlt_sw_dep(layer) = -ext_sw_supply(layer)
  100    continue
 
 
      elseif ((Option.eq.1).OR.(Option.eq.2).OR.(Option.eq.3)) then
 
         call crop_sw_uptake0(max_layer, 
     :                    g%dlayer,
     :                    g%root_depth,
     :                    g%sw_demand,
     :                    g%sw_supply,
     :                    g%dlt_sw_dep)
     
      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine water_stress(Option)
*     ===========================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'crp_watr.pub'
      include 'crp_comm.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option       ! (INPUT) option number

*+  Purpose
*     Get current water stress factors (0-1)

*+  Changes
*     990506  ew - programmed

*+  Constant Values
      character  my_name*(*) ! name of procedure
      parameter (my_name = 'water_stress')

*+  Local Variables
      real ext_sw_supply (max_layer) ! external sw supply (mm)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 

      if (Option .eq. 1) then

         call crop_swdef_photo(max_layer, g%dlayer, g%root_depth,
     :           g%sw_demand, g%sw_supply, g%swdef_photo)

         call crop_swdef_expansion(c%num_sw_demand_ratio,
     :        c%x_sw_demand_ratio, c%y_swdef_leaf, max_layer, g%dlayer,
     :        g%root_depth,g%sw_demand, g%sw_supply, g%swdef_expansion)
     
         call crop_swdef_pheno(c%num_sw_avail_ratio,
     :        c%x_sw_avail_ratio, c%y_swdef_pheno, max_layer, g%dlayer,
     :        g%root_depth, g%sw_avail, g%sw_avail_pot, g%swdef_pheno)


         call crop_swdef_tiller(c%num_sw_avail_ratio_tiller,
     :        c%x_sw_avail_ratio_tiller, c%y_swdef_tiller,
     :        max_layer, g%dlayer,
     :        g%root_depth, g%sw_avail, g%sw_avail_pot, g%swdef_tiller)


      elseif (Option .eq. 2) then
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

      g%swdef_pheno = 1.0


      else if (Option .eq. 3) then

        call water_stress_nw (
     :                  emerg,
     :                  maturity,
     :                  g%current_stage,
     :                  p%uptake_source, 
     :                  g%sw_demand,
     :                  max_layer,
     :                  g%root_depth,
     :                  g%dlayer,
     :                  g%dul_dep,
     :                  p%ll_dep,
     :                  g%sw_dep,
     :                  g%dlt_sw_dep,
     :                  g%swdef_photo,
     :                  g%swdef_expansion,
     :                  g%swdef_pheno,
     :                  g%swdef_tiller)

      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
 
      ! print *, "===================================================="
      ! print *, "g%sw_demand =",         g%sw_demand
      ! print *, "g%swdef_photo =",       g%swdef_photo
      ! print *, "g%swdef_expansion =",   g%swdef_expansion
      ! print *, "g%swdef_pheno =",       g%swdef_pheno
      
      ! print *, "===================================================="
 
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine phenology_initalisation (option)
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
*     990506  ew - programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'phenology_initalisation')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if ((option.eq.1).OR.(option.eq.2).OR.(option.eq.3))  then

           call wheat_phenology_init_ew
     :               (
     :                c%shoot_lag
     :              , c%shoot_rate
     :              , g%current_stage
     :              , g%days_tot
     :              , g%sowing_depth
     :              , g%tt_tot
     :              , g%phase_tt
     :              , p%startgf_to_mat
     :              , c%leaf_app_rate1
     :              , g%vern_eff
     :              , g%photop_eff
     :               )


      elseif (option .eq. 5) then       !NEW OPTION
         call wheat_phenology_init
     :               (
     :                c%shoot_lag
     :              , c%shoot_rate
     :              , c%twilight
     :              , g%current_stage
     :              , g%days_tot
     :              , g%day_of_year
     :              , g%year
     :              , g%latitude
     :              , g%maxt
     :              , g%mint
     :              , g%sowing_depth
     :              , g%tt_tot
     :              , g%leaf_no_final
     :              , c%leaf_no_at_emerg
     :              , c%leaf_app_rate1
     :              , c%leaf_app_rate2
     :              , c%leaf_no_rate_change
     :              , p%photoperiod
     :              , p%phase_tt_init
     :              , p%num_photoperiod
     :              , g%cumvd
     :              , p%x_vfac_cumvd
     :              , p%y_vfac
     :              , p%num_x_vfac_cumvd
     :              , p%tt_emerg_to_endjuv
     :              , p%tt_flower_to_maturity
     :              , p%tt_flower_to_start_grain
     :              , p%tt_flag_to_flower
     :              , p%tt_maturity_to_ripe
     :              , p%est_days_emerg_to_init
     :              , g%phase_tt
     :               )


      elseif (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine vernalization (option)
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
*     990506  ew - programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'vernalization')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if ((option.eq.1).OR.(option.eq.2).OR.(option.eq.3))  then

         call wheat_vernaliz_days (g%current_Stage
     :                            ,germ
     :                            ,floral_init
     :                            ,g%maxt
     :                            ,g%mint
     :                            ,g%cumvd)

         call wheat_vernaliz_effect(g%current_stage
     :                            ,emerg
     :                            ,floral_init
     :                            ,p%vern_sen_internal
     :                            ,g%cumvd
     :                            ,-1.0           !reqvd = 50
     :                            ,g%vern_eff)

      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end


*     ===========================================================
      subroutine photoperiodism (option)
*     ===========================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'science.pub'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer option

      REAL    photoperiod

*+  Purpose
*     Initialise crop growth stage targets

*+  Changes
*     990506  ew - programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'photoperiodism')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if ((option.eq.1).OR.(option.eq.2).OR.(option.eq.3))  then

         photoperiod = day_length (g%day_of_year,
     :                             g%latitude,
     :                             -6.0)          !c%twilight)
 
         call wheat_photoperiod_effect(g%current_stage,
     :                                 emerg,
     :                                 floral_init,
     :                                 photoperiod,
     :                                 p%photop_sen_internal,
     :                                 g%photop_eff)

      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end


* ====================================================================
       subroutine phenology (Option)
* ====================================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'convert.inc'
      include 'error.pub'                         
      include 'crp_phen.pub'                      

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*     <insert here>

*+  Changes
*     990506  ew - programmed

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'phenology')

*- Implementation Section ----------------------------------
      call push_routine (myname)
 

      If ((Option.eq.1).or.(Option.eq.2).or.(Option.eq.3)) then

          call cproc_phenology_nw (
     :                             g%previous_stage
     :                            ,g%current_stage
     :                            ,sowing
     :                            ,germ
     :                            ,harvest_ripe
     :                            ,emerg
     :                            ,flowering
     :                            ,max_stage
     :                            ,c%num_temp
     :                            ,c%x_temp
     :                            ,c%y_tt
     :                            ,g%maxt
     :                            ,g%mint
     :                            ,g%nfact_pheno
     :                            ,g%swdef_pheno
     :                            ,g%vern_eff
     :                            ,g%photop_eff
     :                            ,c%pesw_germ
     :                            ,c%fasw_emerg
     :                            ,c%rel_emerg_rate
     :                            ,c%num_fasw_emerg
     :                            ,g%dlayer
     :                            ,max_layer
     :                            ,g%sowing_depth
     :                            ,g%sw_dep
     :                            ,g%dul_dep
     :                            ,p%ll_dep
     :                            ,g%dlt_tt
     :                            ,g%phase_tt
     :                            ,g%phase_devel
     :                            ,g%dlt_stage
     :                            ,g%tt_tot
     :                            ,g%days_tot
     :                            )

      elseif (Option .eq. 500) then
         call cproc_phenology1 (
     :                             g%previous_stage
     :                            ,g%current_stage
     :                            ,sowing
     :                            ,germ
     :                            ,harvest_ripe
     :                            ,emerg
     :                            ,flowering
     :                            ,max_stage
     :                            ,c%num_temp
     :                            ,c%x_temp
     :                            ,c%y_tt
     :                            ,g%maxt
     :                            ,g%mint
     :                            ,g%nfact_pheno
     :                            ,g%swdef_pheno
     :                            ,c%pesw_germ
     :                            ,c%fasw_emerg
     :                            ,c%rel_emerg_rate
     :                            ,c%num_fasw_emerg
     :                            ,g%dlayer
     :                            ,max_layer
     :                            ,g%sowing_depth
     :                            ,g%sw_dep
     :                            ,g%dul_dep
     :                            ,p%ll_dep
     :                            ,g%dlt_tt
     :                            ,g%phase_tt
     :                            ,g%phase_devel
     :                            ,g%dlt_stage
     :                            ,g%tt_tot
     :                            ,g%days_tot
     :                            )
      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (myname)
      return
      end





*     ===========================================================
      subroutine biomass_potential (Option)
*     ===========================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'crp_biom.pub'
      include 'error.pub'
      include 'crp_util.pub'
      include 'crp_temp.pub'

      include 'science.pub'
      include 'data.pub'
      

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     biomass light

*+  Changes
*     990506  ew - programmed

      REAL iw_kvalue
      REAL nwheat_kvalue

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'biomass_potential')

*+  Local variables
      real  dlt_dm_pot
      REAL  extinct_coef

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 1) then

        extinct_coef = linear_interp_real(g%lai
     :                                   ,c%x_extinct_coeff_lai
     :                                   ,c%y_extinct_coeff_lai
     :                                   ,c%num_extinct_coeff_lai)


         if (g%current_stage.lt.7.0) then
             g%extinction_coeff = extinct_coef
         else
             g%extinction_coeff = 0.42
         end if


         g%cover_green = 1.0 - exp (-g%extinction_coeff*g%lai)


         call crop_radn_int0(
     :                     g%cover_green,
     :                     g%fr_intc_radn,   !just to make sure it is zero then cover_green is used -ew
     :                     g%radn, 
     :                     g%radn_int)

         call crop_temperature_stress_photo(
     :                     c%num_ave_temp,
     :                     c%x_ave_temp,
     :                     c%y_stress_photo,
     :                     g%maxt,
     :                     g%mint,
     :                     g%temp_stress_photo)


c       call crop_dm_potential (
        call crop_dm_pot_rue (
     .                     g%current_stage,
     .                     c%rue,
     .                     g%radn_int,
     .                     g%temp_stress_photo,
     .                     g%nfact_photo,
     .                     g%dlt_dm_light)


      elseif (Option .eq. 2) then
         ! potential by photosynthesis
 
         extinct_coef  = iw_kvalue (g%lai, g%current_stage)
         g%cover_green = 1.0 - exp (-extinct_coef*g%lai)

         g%extinction_coeff = extinct_coef


         call crop_radn_int0(
     :                     g%cover_green,
     :                     g%fr_intc_radn,  !just to make sure it is zero then cover_green is used -ew
     :                     g%radn, 
     :                     g%radn_int)

         call crop_temperature_stress_photo(
     :                     c%num_ave_temp,
     :                     c%x_ave_temp,
     :                     c%y_stress_photo,
     :                     g%maxt,
     :                     g%mint,
     :                     g%temp_stress_photo)

        g%temp_stress_photo =1.0  ! in the original i_wheat model, no temperature reduction factor for rue -ew

        call crop_dm_pot_rue(
     .                     g%current_stage,
     .                     c%rue,
     .                     g%radn_int,
     .                     g%temp_stress_photo,
     .                     g%nfact_photo,
     .                     g%dlt_dm_light)
 


      else if (Option .eq. 3) then

         extinct_coef = nwheat_kvalue (g%lai,g%current_stage,
     :                     start_grain_fill)
         g%cover_green = 1.0 - exp (-extinct_coef*g%lai)

         g%extinction_coeff = extinct_coef

         call crop_radn_int0(
     :                     g%cover_green,
     :                     g%fr_intc_radn,  !just to make sure it is zero then cover_green is used -ew
     :                     g%radn, 
     :                     g%radn_int)

        ! potential by photosynthesis with optimal temperature,nitrogen and water supply
         call potential_biom_nw (
     :                     g%radn,
     :                     g%radn_int,
     :                     g%current_stage,
     :                     dlt_dm_pot)

         !Actual biom by photosynthesis with temperature,nitrogen and water stress
         call temperature_stress_nw(g%maxt,g%mint,g%temp_stress_photo)

         call actual_biom_nw (
     :                     dlt_dm_pot,
     :                     g%temp_stress_photo,
     :                     g%swdef_photo,
     :                     g%nfact_photo,
     :                     g%current_stage,
     :                     start_grain_fill,
     :                     g%dm_plant_min(stem)*g%plants,
     :                     g%dm_green(stem),
     :                     g%tt_tot(start_grain_fill),
     :                     g%phase_tt(start_grain_fill),
     :                     g%dlt_dm)
         
         !Potential biom by photosynthesis with temperature,nitrogen stress and without water stress
         g%dlt_dm_light = g%dlt_dm*
     :                     divide(g%nfact_photo,
     :                     min(g%swdef_photo,g%nfact_photo),1.0)


      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end


*     ===========================================================
      subroutine biomass_actual (Option)
*     ===========================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'crp_watr.pub'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     bio actual

*+  Changes
*     990506  ew - programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'biomass_actual')

*+  Local variables
      real  dlt_dm_pot

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 1) then
        ! use whichever is limiting
 
        !g%dlt_dm = g%dlt_dm_light * min(g%swdef_photo, g%nfact_photo)

         call cproc_transp_eff1(
     :              c%svp_fract
     :            , c%transp_eff_cf
     :            , g%current_stage
     :            , g%maxt
     :            , g%mint
     :            , g%transp_eff
     :             )

          call cproc_bio_water1(
     .             max_layer
     .           , g%dlayer
     .           , g%root_depth
     .           , g%sw_supply
     .           , g%transp_eff
     .           , g%dlt_dm_water
     .             )


        g%dlt_dm = min (g%dlt_dm_light, g%dlt_dm_water)


      elseif (Option .eq. 2) then
         ! use whichever is limiting
 
         !scc need to feed this back to N/leaf area interaction
         !Note that dlt_dm_light is w. RUE as limited by temperature and Nfac

         call cproc_transp_eff1(
     :              c%svp_fract
     :            , c%transp_eff_cf
     :            , g%current_stage
     :            , g%maxt
     :            , g%mint
     :            , g%transp_eff
     :             )


          call cproc_bio_water1(
     .             max_layer
     .           , g%dlayer
     .           , g%root_depth
     .           , g%sw_supply
     .           , g%transp_eff
     .           , g%dlt_dm_water
     .             )


        g%dlt_dm = min (g%dlt_dm_light, g%dlt_dm_water)

      else if (Option.eq.3) then
  
        ! potential by photosynthesis with optimal temperature,nitrogen and water supply
         call potential_biom_nw (
     :                     g%radn,
     :                     g%radn_int,
     :                     g%current_stage,
     :                     dlt_dm_pot)

         call temperature_stress_nw(g%maxt,g%mint,g%temp_stress_photo)

         call actual_biom_nw (
     :                          dlt_dm_pot,
     :                          g%temp_stress_photo,
     :                          g%swdef_photo,
     :                          g%nfact_photo,
     :                          g%current_stage,
     :                          start_grain_fill,
     :                          g%dm_plant_min(stem)*g%plants,
     :                          g%dm_green(stem),
     :                          g%tt_tot(start_grain_fill),
     :                          g%phase_tt(start_grain_fill),
     :                          g%dlt_dm)

  
      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end




*     ===========================================================
      subroutine biomass_initialisation (Option)
*     ===========================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'error.pub'

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose

*+  Changes
*     990506  ew - programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'biomass_initialisation')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option.eq.1) then

         call wheat_dm_init (g%current_stage,
     .          g%days_tot,
     .          c%dm_root_init,
     .          g%plants,
     .          c%dm_stem_init,
     .          c%dm_leaf_init,
     .          c%stem_trans_frac,
     .          c%leaf_trans_frac,
     .          c%initial_tpla,
     .          c%sla_min,
     .          c%sla_max,
     .          g%dm_green, 
     .          g%dm_plant_min,
     ,          p%head_grain_no_max,
     .          g%dm_seed_reserve,
     .          g%lai,
     .          g%grain_no)

      elseif (Option.eq.2) then

         call wht_dm_init (g%current_stage,
     .          g%days_tot,
     .          c%dm_root_init,
     .          g%plants,
     .          c%dm_stem_init,
     .          c%dm_leaf_init,
     .          c%stem_trans_frac,
     .          c%leaf_trans_frac,
     .          c%initial_tpla,
     .          g%dm_green, 
     .          g%dm_plant_min,
     .          g%dm_seed_reserve,
     .          g%lai)

      elseif (Option.eq.3) then

         call wht_dm_init_nw (g%current_stage,
     .          g%days_tot,
     .          c%dm_root_init,
     .          g%plants,
     .          c%dm_stem_init,
     .          c%dm_leaf_init,
     .          c%stem_trans_frac,
     .          c%leaf_trans_frac,
     .          c%initial_tpla,
     .          g%dm_green, 
     .          g%dm_plant_min,
     ,          p%head_grain_no_max,
     .          g%dm_seed_reserve,
     .          g%lai,
     .          g%grain_no)

      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end


*     ===========================================================
      subroutine biomass_yieldpart_demand (Option)
*     ===========================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'crp_biom.pub'
      include 'science.pub'
      include 'data.pub'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Simulate crop grain biomass demand.

*+  Changes
*      19990405   ew programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'biomass_yieldpart_demand')

*+  Local variables

*- Implementation Section ----------------------------------

      call push_routine (my_name)
 

      if (Option .eq. 1) then !use the grain # approach

         call cproc_bio_yieldpart_demand_nw
     :               (
     :                g%current_stage
     :              , start_grain_fill
     :              , end_grain_fill
     :              , g%dm_green
     :              , g%days_tot
     :              , g%maxt
     :              , g%mint
     :              , g%dlt_tt
     :              , p%head_grain_no_max
     :              , p%grain_gth_rate
     :              , g%nfact_expansion
     :              , g%N_conc_min
     :              , g%N_green 
     :              , g%grain_no
     :              , g%dlt_dm_grain_demand
     :               )


      else if (Option .eq. 2) then

          call biomass_grain_demand_stress (1)

          call cproc_bio_yieldpart_demand_iw
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
     :              , g%dlt_tt
     :              , g%lai
     :              , g%tt_tot
     :              , g%phase_tt
     :              , c%N_conc_crit_grain  !g%n_conc_crit
     :              , g%n_green
     :              , g%dlt_dm_grain_demand
     :               )


      else if (Option .eq. 3) then


         call cproc_bio_yieldpart_demand_nw
     :               (
     :                g%current_stage
     :              , start_grain_fill
     :              , end_grain_fill
     :              , g%dm_green
     :              , g%days_tot
     :              , g%maxt
     :              , g%mint
     :              , g%dlt_tt
     :              , p%head_grain_no_max
     :              , p%grain_gth_rate
     :              , g%nfact_expansion
     :              , g%N_conc_min
     :              , g%N_green 
     :              , g%grain_no
     :              , g%dlt_dm_grain_demand
     :               )


      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end


*     ===========================================================
      subroutine biomass_grain_demand_stress (Option)
*     ===========================================================
      use CropModModule
      implicit none
      include   'const.inc'
      include 'crp_biom.pub'
      include 'science.pub'                      
      include 'error.pub'                         
      include 'data.pub'

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Simulate crop grain biomass demand stress factor

*+  Changes

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'biomass_grain_demand_stress')

      integer   deepest_layer

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 

        deepest_layer = find_layer_no
     :                   (g%root_depth, 
     :                    g%dlayer, 
     :                    max_layer)
     
        g%sw_supply_sum = sum_real_array(g%sw_supply, deepest_layer)
       
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


* ====================================================================
       subroutine biomass_partition (Option)
* ====================================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'convert.inc'
      include 'error.pub'                         
      include 'crp_cnpy.pub'

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*     <insert here>

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'biomass_partition')
      
      
c      REAL nw_sla


*- Implementation Section ----------------------------------
      call push_routine (myname)
 

      if (Option.eq.1) then

         call leaf_area_potential(option)

         call cproc_leaf_area_stressed1 (
     :                       g%dlt_lai_pot
     :                      ,g%swdef_expansion
     :                      ,g%nfact_expansion
     :                      ,g%dlt_lai_stressed
     :                      )




      if(g%accum_rad_10d.lt.100.0) then
         c%sla_max = g%accum_rad_10d * (- 1.5) + 400.0
         c%sla_min = g%accum_rad_10d * (- 1.5) + 300.0
        else
         c%sla_max = 300.0
         c%sla_min = 150.0
      endif

         c%sla_max = c%sla_max * 100.0
         c%sla_min = c%sla_min * 100.0



         call cproc_bio_partition_wheat (
     :                  g%current_stage,
     : 	 		c%ratio_root_shoot,
     :                  c%sla_min,
     :                  g%dlt_lai_stressed,
     :                  g%dlt_dm,
     :                  g%phase_tt,
     :                  g%tt_tot,
     :                  g%swdef_photo,
     :                  g%nfact_photo,
     :                  g%swdef_expansion,
     :                  g%nfact_expansion,
     :                  g%dlt_dm_grain_demand,
     :                  g%dlt_dm_green)
     


      elseif (Option.eq.2) then

         call cproc_bio_partition_iwheat (
     : 				g%current_stage,
     : 				g%dlt_dm,
     :                          g%dm_seed_reserve,
     :                          g%lai,
     : 				g%accum_rad_10d,
     : 				g%plants,
     : 				g%dlt_tt,
     : 				g%phase_tt,
     : 				g%tt_tot,
     .                          p%tiller_curve,
     .                          p%tiller_tt_infl,
     .                          g%tiller_tt_tot,
     .                          g%tiller_area_pot,
     : 				c%ratio_root_shoot,
     : 				g%tiller_area_max,
     :                          g%dm_green,
     : 				c%leaf_app_rate1,
     : 				g%swdef_photo,
     : 				g%nfact_photo,
     : 				g%swdef_expansion,
     : 				g%nfact_expansion,
     : 				g%dlt_dm_grain_demand,
     : 				g%dlt_dm_green)


      elseif (Option.eq.3) then

c          nw_sla = 22500.0  ! mm2/g - nwheat value

          call cproc_bio_partition_nw (
     :                  g%current_stage,
     :                  g%maxt,
     :                  g%mint,
     :                  g%dlt_dm,
     :                  g%dlt_tt,
     :                  g%phase_tt,
     :                  g%tt_tot,
     :                  c%leaf_app_rate1, !p%phint,
     :                  c%sla_min,
     :                  c%ratio_root_shoot,
     :                  g%leaf_no,
     :                  g%tiller_no_fertile,
     :                  g%swdef_expansion,
     :                  g%nfact_expansion,
     :                  g%dlt_dm_grain_demand,
     :                  g%plants,
     :                  g%dm_green,
     :                  g%dm_plant_min,
     :                  g%dlt_dm_green,
     :                  g%dlt_dm_leaf_pot)

      elseif (Option.eq.4) then

          call cproc_bio_partition_nw_ew (
     :                  g%current_stage,
     :                  g%maxt,
     :                  g%mint,
     :                  g%dlt_dm,
     :                  g%dlt_tt,
     :                  g%phase_tt,
     :                  g%tt_tot,
     :                  c%leaf_app_rate1, !p%phint,
     :                  c%sla_min,
     :                  c%ratio_root_shoot,
     :                  g%leaf_no,
     :                  g%tiller_no_fertile,
     :                  g%swdef_expansion,
     :                  g%nfact_expansion,
     :                  g%dlt_dm_grain_demand,
     :                  g%plants,
     :                  g%dm_green,
     :                  g%dm_plant_min,
     :                  g%dlt_dm_green,
     :                  g%dlt_dm_leaf_pot)

      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (myname)
      return
      end


*     ===========================================================
      subroutine biomass_retranslocation (Option)
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

*+  Constant Values
      integer   num_supply_pools
      parameter (num_supply_pools = 2)
*
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'biomass_retranslocation')

*+  Local Variables
      integer supply_pools(num_supply_pools)
      data    supply_pools /stem,leaf/      !in nwheat no carbon from leaves to grain  ??????????????
      save    /supply_pools/

*- Implementation Section ----------------------------------
      call push_routine (my_name)


      if (Option.eq.1) then
 
         call cproc_dm_retranslocate1
     :               (
     :                g%current_stage
     :              , start_grain_fill
     :              , end_grain_fill
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

      elseif (Option.eq.2) then
 
         call cproc_dm_retranslocate1
     :               (
     :                g%current_stage
     :              , start_grain_fill
     :              , end_grain_fill
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

      elseif (Option.eq.3) then
 
         call dm_retranslocate_nw
     :               (
     :                g%current_stage
     :              , leaf
     :              , emerg
     :              , floral_init
     :              , g%dlt_dm_leaf_pot
     :              , g%dm_seed_reserve
     :              , start_grain_fill
     :              , maturity
     :              , grain
     :              , max_part
     :              , supply_pools
     :              , 1
     :              , g%dlt_dm_grain_demand
     :              , g%dlt_dm_green
     :              , g%dm_green
     :              , g%dm_plant_min
     :              , g%plants
     :              , g%dlt_dm_green_retrans
     :               )
 



      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end





*     ===========================================================
      subroutine leaf_number_initialisation (Option)
*     ===========================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'crp_cnpy.pub'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose

*+  Changes

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'leaf_number_initialisation')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if ((Option.eq.1).or.(Option.eq.2).or.(Option.eq.3)) then
 
         call cproc_leaf_no_init1
     :               (
     :                c%leaf_no_at_emerg
     :              , g%current_stage
     :              , emerg
     :              , g%days_tot
     :              , g%leaf_no
     :              , g%node_no
     :               )

      else if (Option.eq.0) then

         !This module is excluded from the model

 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end


* ====================================================================
       subroutine leaf_initiation (Option)
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
      parameter (myname = 'leaf_initiation')

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      if ((Option.eq.1).or.(Option.eq.2).or.(Option.eq.3)) then
 
            CALL wht_leaf_number_final2 (
     .          emerg,
     .          floral_init,
     .          plant_end,
     .          g%dlt_tt,
     .          g%current_stage,
     .          g%days_tot,
     .          g%phase_tt,
     .          c%leaf_init_rate,
     .          c%leaf_no_seed,
     .          c%leaf_no_min,
     .          c%leaf_no_max,
     .          g%leaf_no_final,
     .          g%leaf_primodia)


      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (myname)
      return
      end


* ====================================================================
       subroutine leaf_appearance (Option)
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
      parameter (myname = 'leaf_appearance')

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      if ((Option.eq.1).or.(Option.eq.2).or.(Option.eq.3)) then
 
              call wht_leaf_appearance2 (
     .          g%leaf_no,
     .          g%leaf_no_final,
     .          c%leaf_no_rate_change,
     .          c%leaf_app_rate2,
     .          c%leaf_app_rate1,
     .          g%current_stage,
     .          g%days_tot,
     .          g%dlt_tt,
     .          g%dlt_leaf_no) ! fraction of leaf emerged

      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (myname)
      return
      end


*     ===========================================================
      subroutine tillering_initialisation (Option)
*     ===========================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'science.pub'
      include 'data.pub'                         
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number
                  
*+  Purpose
*     root distribution

*+  Changes

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'tillering_initialisation')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if ((Option.eq.1).or.(Option.eq.2).or.(Option.eq.3)) then
        
        if (on_day_of (emerg, g%current_stage, g%days_tot)) then
 
          g%tiller_no_fertile = 1.0
          g%dm_tiller_pot     = 0.0
          g%tiller_no_sen     = 0.0
                 
          g%swdef_tiller = 1.0
          g%nfact_tiller = 1.0
                 
        endif

        g%dlt_tiller_no      =0.0
        g%dlt_stiller_no     =0.0
         
      else if (Option.eq.0) then

         !This module is excluded from the model
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine tillering (Option)
*     ===========================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'science.pub'
      include 'data.pub'                         
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number
                  
*+  Purpose
*     root distribution

*+  Changes

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'tillering')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 1) then

        call tillering_nw (
     :                  g%current_stage,
     :                  emerg,
     :                  floral_init,
     :                  flag_leaf,
     :                  g%dm_green(stem),
     :                  g%dlt_dm_green(stem),
     :                  g%tt_tot,
     :                  g%phase_tt,
     :                  g%dlt_tt,
     :                  c%leaf_app_rate1,
     :                  g%leaf_no,
     :                  g%dlt_leaf_no,
     :                  g%tiller_no_fertile,
     :                  g%plants,
     :                  g%swdef_tiller,
     :                  g%swdef_photo,
     :                  g%nfact_tiller,
     :                  g%nfact_expansion,
     :                  g%dm_tiller_pot,
     :                  p%dm_tiller_max,
     :                  g%dlt_tiller_no,
     :                  g%dlt_stiller_no)


      elseif (Option.eq.2) then

        CALL  iw_tillering(
     .                          g%current_stage,
     .                          g%tiller_area_max,
     .                          g%tiller_area_act,
     .                          g%tiller_no_fertile)

      elseif (Option .eq. 3) then

        call tillering_nw (
     :                  g%current_stage,
     :                  emerg,
     :                  floral_init,
     :                  flag_leaf,
     :                  g%dm_green(stem),
     :                  g%dlt_dm_green(stem),
     :                  g%tt_tot,
     :                  g%phase_tt,
     :                  g%dlt_tt,
     :                  c%leaf_app_rate1,
     :                  g%leaf_no,
     :                  g%dlt_leaf_no,
     :                  g%tiller_no_fertile,
     :                  g%plants,
     :                  g%swdef_tiller,
     :                  g%swdef_photo,
     :                  g%nfact_tiller,
     :                  g%nfact_expansion,
     :                  g%dm_tiller_pot,
     :                  p%dm_tiller_max,
     :                  g%dlt_tiller_no,
     :                  g%dlt_stiller_no)

      else if (Option.eq.0) then

         !This module is excluded from the model
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end


* ====================================================================
       subroutine leaf_area_potential (Option)
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
      parameter (myname = 'leaf_area_potential')

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      if (Option.eq.1) then


          call cproc_leaf_area_pot_iw (
     .          g%plants,
     .          g%current_stage,
     .          c%leaf_app_rate1,
     .          g%dlt_tt, !*MIN(g%nfact_expansion,g%swdef_expansion),
     .          g%tiller_area_max,
     .          p%tiller_curve,
     .          p%tiller_tt_infl,
     .          g%tiller_tt_tot,
     .          g%tiller_area_pot,
     .          g%dlt_tiller_area_pot,
     .          g%dlt_lai_pot)


      else if (Option.eq.2) then


          call cproc_leaf_area_pot_iw (
     .          g%plants,
     .          g%current_stage,
     .          c%leaf_app_rate1,
     .          g%dlt_tt,
     .          g%tiller_area_max,
     .          p%tiller_curve,
     .          p%tiller_tt_infl,
     .          g%tiller_tt_tot,
     .          g%tiller_area_pot,
     .          g%dlt_tiller_area_pot,
     .          g%dlt_lai_pot)



      elseif (Option .eq. 4) then


c        call wheat_leaf_area_potential (
c     :                             15.0
c     :                            ,g%current_stage
c     :                            ,g%dlt_tt
c     :                            ,G%phase_tt
c     :                            ,g%tt_tot
c     :                            ,g%dlt_lai_pot
c     :                            )

      elseif ((Option .eq. 0).or.(Option .eq. 3)) then

         !This module is excluded from the model
      else

       call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (myname)
      return
      end



*     ===========================================================
      subroutine leaf_area_actual (Option)
*     ===========================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'science.pub'
      include 'data.pub'
      include 'crp_cnpy.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) option number

*+  Purpose
*     Calculate the stressed dlt_lai from the potential dlt_lai.

*+  Changes

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'leaf_area_actual')
      
c     real swdef_exp
      !real nfact_exp
c      REAL sla_est

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if ((Option.eq.1).or.(Option.eq.2)) then
 
         call cproc_leaf_area_stressed1 (
     :                       g%dlt_lai_pot
     :                      ,g%swdef_expansion
     :                      ,g%nfact_expansion
     :                      ,g%dlt_lai_stressed
     :                      )

          g%dlt_lai = g%dlt_lai_stressed

          g%dlt_lai = MIN(g%dlt_lai,
     ;                    g%dlt_dm_green(leaf)*c%sla_max*1E-6)


c        call iw_sla_est(
c     .          g%current_stage,
c     .          g%accum_rad_10d,
c     .          g%tt_tot,
c     .          g%phase_tt,
c     .          sla_est)

c      if (stage_is_between (emerg, floral_init, g%current_stage)) then
c         g%dlt_lai = MIN(g%dlt_lai, g%dlt_dm_green(leaf)*sla_est*1E-4)
c      endif


      else if (Option .eq. 3) then
        
        call leaf_area_nw (
     :                  g%current_stage,
     :                  emerg,
     :                  floral_init,
     :                  g%maxt,
     :                  g%mint,
     :                  g%dlt_tt,
     :                  g%dlt_dm_green(leaf),
     :                  c%sla_min,
     :                  g%phase_tt,
     :                  g%tt_tot,
     :                  c%leaf_app_rate1,
     :                  g%leaf_no,
     :                  g%tiller_no_fertile,
     :                  g%swdef_expansion,
     :                  g%nfact_expansion,
     :                  g%plants,
     :                  g%dlt_lai_stressed)

        g%dlt_lai = g%dlt_lai_stressed

      elseif (Option .eq. 0) then

      else
      
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end


*     ===========================================================
      subroutine crop_height (Option)
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

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'crop_height')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if ((Option.eq.1).or.(Option.eq.2).or.(Option.eq.3)) then
 
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

      elseif (Option .eq. 0) then

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine root_depth_initialisation (Option)
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

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'root_depth_initialisation')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if ((Option.eq.1).or.(Option.eq.2).or.(Option.eq.3)) then

         call cproc_root_depth_init1
     :               (
     :                g%sowing_depth
     :              , g%current_stage
     :              , germ
     :              , g%days_tot
     :              , g%root_depth
     :               )

      else if (Option.eq.0) then

         !This module is excluded from the model
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end


*     ===========================================================
      subroutine root_depth (Option)
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

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'root_depth')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if ((Option.eq.1).or.(Option.eq.2).or.(Option.eq.3)) then

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
      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end


*     ===========================================================
      subroutine root_length_initialisation (Option)
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

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'root_length_initialisation')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if ((Option.eq.1).or.(Option.eq.2).or.(Option.eq.3)) then

         call cproc_root_length_init_ew ( ! the crop template func cproc_root_length_init1 has a problem
     :                emerg
     :               ,g%current_stage
     :               ,g%days_tot
     :               ,g%dm_green(root)
     :               ,c%specific_root_length
     :               ,g%root_depth
     :               ,g%dlayer
     :               ,g%root_length
     :               ,max_layer)

      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine root_length (Option)
*     ===========================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'crp_root.pub'
      include 'science.pub'
      include 'data.pub'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number
      REAL       dlt_dm_root

*+  Purpose
*       Plant root distribution calculations

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'root_length')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if ((Option.eq.1).or.(Option.eq.2).or.(Option.eq.3)) then
 
!        if (on_day_of (emerg, g%current_stage, g%days_tot)) then
!            dlt_dm_root = 0.06*g%plants
!        else
            dlt_dm_root = g%dlt_dm_green(root)
!        end if


         call cproc_root_length_growth1
     :               (
     :                c%specific_root_length
     :              , g%dlayer
     :              , dlt_dm_root
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





      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end




*     ===========================================================
      subroutine senescence_leaf_area (Option)
*     ===========================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'science.pub'
      include 'data.pub'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) template option number

*+  Purpose
*     Estimates leaf area senesence determined by age, frost, light, water
*

*+  Changes

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'senescence_leaf_area')
      
*+  Local variables
      real     sla_est      
      real     rue_red_fac
      REAL     dlt_slai_age
      REAL     dlt_slai_shade

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 


      if (Option.eq.2) then

            call iw_tiller_area_sen_light(
     .                               g%plants,
     .                               g%lai,
     .                               g%current_stage,
     .                               g%days_tot,
     .                               g%tiller_area_act,
     .                               g%tiller_area_max,
     .                               g%accum_rad_10d,
     .                               g%dlt_tiller_sen_area_light)


         call iw_tiller_area_sen_age (
     .           g%current_stage,
     .           g%dlt_tt,
     .           g%tt_tot,
     .           g%phase_tt,
     .           g%days_tot,
     .           g%tiller_area_act,
     .           g%tiller_area_max,
     .           g%tiller_area_act_stage,
     .           g%tiller_area_sen,
     .           g%dlt_tiller_sen_area_age)

        call iw_sla_est(
     .          g%current_stage,  
     .          g%accum_rad_10d,
     .          g%tt_tot,
     .          g%phase_tt,
     .          sla_est)

       call iw_rue_red_fac(
     .          0.00011,         !p%sln_critical= sln_cr = 0.00011
     .          sla_est,  
     .          g%dm_green,
     .          g%N_conc_min,
     .          g%N_green,
     .          g%tt_tot,
     .          rue_red_fac)

         !rue_red_fac = max(0.25, g%nfact_expansion)  !ew - added this line for check
         !rue_red_fac = max(0.25, g%nfact_photo)  !ew - added this line for check
         

c        deepest_layer = find_layer_no
c     :                   (g%root_depth,
c     :                    g%dlayer,
c     :                    max_layer)

c
c       g%sw_supply_sum = sum_real_array(g%sw_supply, deepest_layer)


         call iw_tiller_area_sen_water(
     .           g%radn,  
     .           g%plants,
     .           g%current_stage,
     .           g%lai,
     .           c%rue,
     .           rue_red_fac,
     .           g%dlt_dm, !g%dlt_dm_light,
     .           g%sw_demand,
     .           g%sw_supply_sum,
     .           g%tiller_area_act,
     .           g%tiller_area_max,
     .           g%dlt_tiller_sen_area_water)
                                                                           

          call iw_tiller_area_sen_nitrogen (
     .           g%current_stage,
     .           g%lai,
     .           rue_red_fac,
     .           g%tiller_count,
     .           g%tiller_kill_day,
     .           g%tiller_area_max,
     .           g%tiller_area_act,
     .           g%dlt_tiller_sen_area_nitrogen)


          call leaf_area_from_tillers()



      else if (Option.eq.3) then

      !NEIL, I CAN NOT UNDERSTAND THE NWHEAT LEAF SENESCENCE PROCESS SUBROUTINE. SO THE SUBROUTINES NOW USED (_nw_ew) ARE
      !      MODIFIED VERSION WITHOUT AGING BEFORE FLAG LEAF.
      !      YOU CAN USE THE OTHER TWO (ending with _nw). They are coded according the original ones

        call leaf_senescence_age_nw(
     :                   g%current_stage,
     :                   g%phase_tt,
     :                   g%tt_tot,
     :                   g%days_tot,
     :                   g%dlt_tt,
     :                   g%dlt_lai,
     :                   g%lai,
     :                   g%lai_stage,
     :                   g%slai,
     :                   g%leaf_no,
     :                   c%leaf_app_rate1,
     :                   g%plsc,
     :                   dlt_slai_age)

          call leaf_senescence_stressed_nw(
     :                g%current_stage,
     :                g%lai,
     :                dlt_slai_age,
     :                g%leaf_no,
     :                g%maxt,
     :                g%swdef_photo,
     :                g%nfact_tiller,
     :                g%plsc,
     :                g%dlt_slai )

      else if ((Option.eq.1).or.(Option.eq.4)) then

c        call leaf_senescence_age_nw(
       call leaf_senescence_age_nw_ew(
     :                   g%current_stage,
     :                   g%phase_tt,
     :                   g%tt_tot,
     :                   g%days_tot,
     :                   g%dlt_tt,
     :                   g%dlt_lai,
     :                   g%lai,
     :                   g%lai_stage,
     :                   g%slai,
     :                   g%leaf_no,
     :                   c%leaf_app_rate1,
     :                   g%plsc,
     :                g%swdef_expansion,
     :                g%nfact_expansion,
     :                g%swdef_photo,
     :                g%nfact_photo,
     :                   dlt_slai_age)

c          call leaf_senescence_stressed_nw(
         call leaf_senescence_stressed_nw_ew(
     :                g%current_stage,
     :                g%lai,
     :                dlt_slai_age,
     :                g%leaf_no,
     :                g%maxt,
     :                g%swdef_expansion,
     :                g%nfact_expansion,
     :                g%swdef_photo,
     :                g%nfact_photo,
     :                g%plsc,
     :                g%dlt_slai )


        call  Kill_tillers()


      elseif (Option.eq.5) then

       call leaf_senescence_age_wheat(
     :                   g%current_stage,
     :                   g%phase_tt,
     :                   g%tt_tot,
     :                   g%days_tot,
     :                   g%dlt_tt,
     :                   g%dlt_lai,
     :                   g%lai,
     :                   g%lai_stage,
     :                   g%slai,
     :                   g%leaf_no,
     :                   c%leaf_app_rate1,
     :                   g%plsc,
     :                   dlt_slai_age)

       call leaf_senescence_shade_wheat(
     :                   g%current_stage,
     :                   g%phase_tt,
     :                   g%tt_tot,
     :                   g%days_tot,
     :                   g%dlt_tt,
     :                   g%dlt_lai,
     :                   g%lai,
     :                   g%lai_stage,
     :                   g%slai,
     :                   g%leaf_no,
     :                   c%leaf_app_rate1,
     :                   g%plsc,
     :                   dlt_slai_shade)


         call leaf_senescence_stressed_wheat(
     :                g%current_stage,
     :                g%lai,
     :                MAX(dlt_slai_age,dlt_slai_shade),
     :                g%leaf_no,
     :                g%maxt,
     :                g%swdef_photo,
     :                g%nfact_photo,
     :                g%plsc,
     :                g%dlt_slai )





      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end


* ====================================================================
       subroutine leaf_area_from_tillers()
* ====================================================================
*+  Purpose
*     calculate the leaf area and tiller area growth rate (alive and senscenced) 

*+  Changes
*     EW reprogrammed Jan. 1999

*+  Include section
      use CropModModule
      implicit none
      include 'science.pub'
      include 'data.pub'
      include 'convert.inc'
      include 'error.pub'                         

*+  Constant Values
      character*(*) myname             ! name of this procedure
      parameter (myname = 'leaf_area_from_tillers')

*+  Local Values
      integer n

      
*- Implementation Section ----------------------------------
      call push_routine (myname)


       !g%dlt_lai           = 0.0
       
       g%dlt_slai          = 0.0
       g%dlt_slai_age      = 0.0
       g%dlt_slai_light    = 0.0
       g%dlt_slai_water    = 0.0
       g%dlt_slai_nitrogen = 0.0
       
       do n  = 1, max_leaf
       
        !Tiller area senescenc
        g%dlt_tiller_sen_area(n) = max(
     .                             g%dlt_tiller_sen_area_age(n),
     .                             g%dlt_tiller_sen_area_light(n),
     .                             g%dlt_tiller_sen_area_water(n),
     .                             g%dlt_tiller_sen_area_nitrogen(n))
                                         

        g%dlt_tiller_area_act(n)= g%dlt_tiller_area_pot(n)
     .                        * divide(g%dlt_lai, g%dlt_lai_pot,0.0)

        if (g%tiller_area_max(n).eq.0.0) then
            g%dlt_tiller_sen_area(n) = g%tiller_area_act(n)
     .                               + g%dlt_tiller_area_act(n)
        endif

         !Leaf area senescence
         g%dlt_slai       =  g%dlt_slai 
     .                     + g%dlt_tiller_sen_area(n)
         g%dlt_slai_age   =  g%dlt_slai_age
     .                     + g%dlt_tiller_sen_area_age(n)
         g%dlt_slai_light =  g%dlt_slai_light 
     .                     + g%dlt_tiller_sen_area_light(n)
         g%dlt_slai_water =  g%dlt_slai_water 
     .                     + g%dlt_tiller_sen_area_water(n)
         g%dlt_slai_nitrogen =  g%dlt_slai_nitrogen 
     .                     + g%dlt_tiller_sen_area_nitrogen(n)

      end do


         !Change the unit to m2/m2

         !g%dlt_lai         = g%dlt_lai          *smm2sm*g%plants*100
         g%dlt_slai         = g%dlt_slai         *smm2sm*g%plants*100.0
         g%dlt_slai_age     = g%dlt_slai_age     *smm2sm*g%plants*100.0
         g%dlt_slai_light   = g%dlt_slai_light   *smm2sm*g%plants*100.0
         g%dlt_slai_water   = g%dlt_slai_water   *smm2sm*g%plants*100.0
         g%dlt_slai_nitrogen= g%dlt_slai_nitrogen*smm2sm*g%plants*100.0


        !ew added this line
         g%dlt_slai = min(g%dlt_slai, g%lai + g%dlt_lai)
                

      call pop_routine (myname)
      return
      end


* ====================================================================
       subroutine Kill_tillers()
* ====================================================================
*+  Purpose
*     calculate the leaf area and tiller area growth rate (alive and senscenced) 

*+  Changes
*     EW reprogrammed Jan. 1999

*+  Include section
      use CropModModule
      implicit none
      include 'science.pub'
      include 'data.pub'
      include 'convert.inc'
      include 'error.pub'                         

*+  Constant Values
      character*(*) myname             ! name of this procedure
      parameter (myname = 'Kill_tillers')

*+  Local Values
      integer n,m
      INTEGER NumOfTillers
      REAL    dlt_lai_sen
      REAL    tiller_area_act


      
*- Implementation Section ----------------------------------
      call push_routine (myname)

      NumOfTillers = 0
      do n = 1, max_leaf
         if (g%dlt_tiller_area_pot(n).gt.0.0) then
             NumOfTillers = n
         end if
      end do

      NumOfTillers = MAX(2, NumOfTillers)

       !Change from m2/m2 to per tiller area
       dlt_lai_sen = g%dlt_slai /(smm2sm*g%plants*100.0)

       m = max_leaf

       do n  =  NumOfTillers, 2, -1

        g%dlt_tiller_area_act(n)= g%dlt_tiller_area_pot(n)
     .                        * divide(g%dlt_lai, g%dlt_lai_pot,0.0)

        tiller_area_act = g%tiller_area_act(n)+g%dlt_tiller_area_act(n)

        if (tiller_area_act .lt. dlt_lai_sen) then      !A tiller is killed in this case
            dlt_lai_sen = dlt_lai_sen - tiller_area_act
            g%dlt_tiller_sen_area(n) = tiller_area_act
            g%tiller_area_max(n)     = 0.0

c           PRINT *, "stress kill tiller", n
            m=n
        else
            g%dlt_tiller_sen_area(n) = dlt_lai_sen
            dlt_lai_sen = 0.0
c           g%tiller_area_max(n) = g%tiller_area_max(n)-dlt_lai_sen

        endif

      enddo


c       do n = m, max_leaf
c            g%tiller_area_max(n)=0.0
c       end do

      call pop_routine (myname)
      return
      end


*     ===========================================================
      subroutine senescence_biomass (Option)
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

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'senescence_biomass')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 

      if (Option.eq.1) then

         CALL crop_dm_senescence1(   max_part,
     :                               root,
     :                               leaf,
     :                               stem,
     :                               c%dm_leaf_sen_frac,
     :                               c%dm_root_sen_frac,
     :                               g%dm_green,
     :                               g%dlt_dm_green,
     :                               g%dlt_dm_green_retrans,
     :                               g%lai,
     :                               g%dlt_lai,
     :                               g%dlt_slai,
     :                               g%dlt_dm_senesced,
     :                               g%dlt_dm_sen_retrans)


      elseif (Option.eq.2) then

         CALL crop_dm_senescence_iw( max_part,
     :                               root,
     :                               leaf,
     :                               stem,
     :                               c%dm_leaf_sen_frac,
     :                               c%dm_root_sen_frac,
     :                               g%dm_green,
     :                               g%dlt_dm_green,
     :                               g%dlt_dm_green_retrans,
     :                               g%dm_senesced,
     :                               g%lai,
     :                               g%dlt_lai,
     :                               g%dlt_slai,
     :                               g%dlt_dm_senesced,
     :                               g%dlt_dm_sen_retrans)



      elseif (Option.eq.3) then

         CALL crop_dm_senescence1(   max_part,
     :                               root,
     :                               leaf,
     :                               stem,
     :                               c%dm_leaf_sen_frac,
     :                               c%dm_root_sen_frac,
     :                               g%dm_green,
     :                               g%dlt_dm_green,
     :                               g%dlt_dm_green_retrans,
     :                               g%lai,
     :                               g%dlt_lai,
     :                               g%dlt_slai,
     :                               g%dlt_dm_senesced,
     :                               g%dlt_dm_sen_retrans)



      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine senescence_root_length (Option)
*     ===========================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'crp_root.pub'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Simulate plant nitrogen senescence.

*+  Changes

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'senescence_root_length')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if ((Option.eq.1).or.(Option.eq.2).or.(Option.eq.3)) then
 
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
 
      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine senescence_nitrogen (Option)
*     ===========================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'crp_nitn.pub'
      include 'science.pub'
      include 'data.pub'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     biomass senescence

*+  Changes

*+  Locals
      real leaf_nc
      real leaf_nd
      real stem_nc
      REAL stem_nd
      REAL stem_nt
      REAL dlt_n_sen_supply(max_part)
      REAL daily_N_sen

      REAL sla_est
      REAL sln_cr
      REAL nconc_leaf

      REAL dlt_n_sen_root

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'senescence_nitrogen')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)

 
      if (Option.eq.2) then


         !dlt_n_senesced calculated later in N retranslocation
         call cproc_N_senescence1 (max_part
     :                              , c%n_sen_conc
     :                              , g%dlt_dm_senesced
     :                              , g%n_green
     :                              , g%dm_green
     :                              , g%dlt_N_senesced)

         dlt_n_sen_root = g%dlt_N_senesced(root)

         call fill_real_array (g%dlt_n_senesced, 0.0, max_part)

         g%dlt_N_senesced(root) =  dlt_n_sen_root





c-----------------------------------------------------------------
       !i_wheat modifies the n_conc_crit(leaf)
        call iw_sla_est(
     .          g%current_stage,
     .          g%accum_rad_10d,
     .          g%tt_tot,
     .          g%phase_tt,
     .          sla_est)

        sln_cr = 0.00011

        nconc_leaf = sln_cr * sla_est *(1.0 + 0.2)

        g%n_conc_crit(leaf) = MAX(g%n_conc_crit(leaf), nconc_leaf)

c-----------------------------------------------------------------







      elseif (Option.eq.200) then



c"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
c       Instantiation V5.0
c
c         g%n_green(leaf) = g%n_green(leaf)
c     :          + sum_real_array (dlt_n_sen_supply, max_part)
c
c        leaf_nn = g%n_green(leaf)! +g%dlt_n_green(stem)
c        leaf_dm = g%dm_green(leaf)+g%dlt_dm_green(leaf)
c     :                            +g%dlt_dm_green_retrans(leaf)
c        leaf_nc = divide(leaf_nn, leaf_dm, 0.0)
c
c
c
c        call iw_sla_est(
c     .          g%current_stage,
c     .          g%accum_rad_10d,
c     .          g%tt_tot,
c     .          g%phase_tt,
c     .          sla_est)
c
c        sln_cr = 0.00011
c
c        nconc_leaf = sln_cr * sla_est *(1.0 + 0.2)
c
c        g%n_conc_crit(leaf) = MAX(g%n_conc_crit(leaf), nconc_leaf)
c
c
c        if (leaf_nc .gt. 1.5*g%n_conc_crit(leaf)) then
c            leaf_nn         = 1.5*leaf_dm*g%n_conc_crit(leaf)
c            g%n_green(stem) = g%n_green(stem)+ g%n_green(leaf)-leaf_nn
c            g%n_green(leaf) = leaf_nn
c        end if
c
c"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
c       Instantiation V8.0

         call cproc_N_senescence1 (max_part
     :                              , c%n_sen_conc
     :                              , g%dlt_dm_senesced
     :                              , g%n_green
     :                              , g%dm_green
     :                              , g%dlt_N_senesced)
 

         call fill_real_array (dlt_n_sen_supply, 0.0, max_part)

         call cproc_N_sen_supply    (max_part
     :                              , c%n_sen_conc
     :                              , g%dlt_dm_senesced
     :                              , g%n_green
     :                              , g%dm_green
     :                              , dlt_N_sen_supply)

         call subtract_real_array(dlt_n_sen_supply,g%n_green,max_part)


       !i_wheat modifies the n_conc_crit(leaf)
        call iw_sla_est(
     .          g%current_stage,
     .          g%accum_rad_10d,
     .          g%tt_tot,
     .          g%phase_tt,
     .          sla_est)

        sln_cr = 0.00011

        nconc_leaf = sln_cr * sla_est *(1.0 + 0.2)

        g%n_conc_crit(leaf) = MAX(g%n_conc_crit(leaf), nconc_leaf)


        daily_N_sen = sum_real_array (dlt_n_sen_supply, max_part)


        !luxury consuption occurs, stem N loss 5%
        if (stage_is_between(emerg, flowering,
     :                       g%current_stage).and.
     :                       (g%lai.gt.0.05)) then

            if (g%n_green(stem).gt.
     :                  g%dm_green(stem) * g%n_conc_crit(stem)) then
                g%n_green(stem) = g%n_green(stem)*(1.0 - 0.05)
            end if

        endif

        if (stage_is_between(emerg, start_grain_fill,
     :                       g%current_stage).and.
     :                       (g%lai.gt.0.05)) then

            !Before grain filling, stem get some N from leaf if leaf Nc higher than critical
            stem_nc = divide(g%n_green(stem),g%dm_green(stem),0.0)
            leaf_nc = divide(g%n_green(leaf),g%dm_green(leaf),0.0)

            if ( stem_nc.lt.g%n_conc_crit(stem).AND.
     :           leaf_nc.gt.g%n_conc_crit(leaf)) then
               stem_nd = g%dm_green(stem)* (g%n_conc_crit(stem)-stem_nc)
               g%n_green(stem) = g%n_green(stem)+stem_nd
               g%n_green(leaf) = g%n_green(leaf)-stem_nd
            end if

            stem_nt=g%n_green(stem)-g%dm_green(stem)*g%n_conc_min(stem)


            !If N available in dead leaf, stem get it first to met demand
            stem_nc = divide(g%n_green(stem),g%dm_green(stem),0.0)
            if ( stem_nc.lt.g%n_conc_crit(stem)) then
               stem_nd = g%dm_green(stem)* (g%n_conc_crit(stem)-stem_nc)
               stem_nd = MIN(stem_nd, daily_N_sen)

               g%n_green(stem) = g%n_green(stem)+stem_nd
               daily_N_sen     = daily_N_sen    -stem_nd
            end if

            !If N available in dead leaf, stem get it first to met demand
            leaf_nc = divide(g%n_green(leaf), g%dm_green(leaf), 0.0)
            if (leaf_nc.lt.g%n_conc_crit(leaf)) then
               leaf_nd = g%dm_green(leaf)* (g%n_conc_crit(leaf)-leaf_nc)

               if (leaf_nd.gt.stem_nt+daily_n_sen) then
                   g%n_green(leaf)=g%n_green(leaf)+(stem_nt+daily_n_sen)
                   g%n_green(stem)=g%n_green(stem)- stem_nt
                   daily_n_sen    = 0.0
               else
                   if (leaf_nd.lt.daily_n_sen) then
                     g%n_green(leaf)=g%n_green(leaf)+leaf_nd
                     daily_n_sen    =daily_n_sen    -leaf_nd
                   else
                     g%n_green(leaf)=g%n_green(leaf)+leaf_nd
                     daily_n_sen    =0.0
                     g%n_green(stem)=g%n_green(stem)
     :                              -(leaf_nd-daily_n_sen)
                   end if
               end if
            end if
        endif

        g%n_green(stem) =  g%n_green(stem) + daily_N_sen

c"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


      else if ((Option.eq.1).or.(Option.eq.3)) then

         call cproc_N_senescence1 (max_part
     :                              , c%n_sen_conc
     :                              , g%dlt_dm_senesced
     :                              , g%n_green
     :                              , g%dm_green
     :                              , g%dlt_N_senesced)
 

         call fill_real_array (dlt_n_sen_supply, 0.0, max_part)

         call cproc_N_sen_supply    (max_part
     :                              , c%n_sen_conc
     :                              , g%dlt_dm_senesced
     :                              , g%n_green
     :                              , g%dm_green
     :                              , dlt_N_sen_supply)

         call subtract_real_array(dlt_n_sen_supply,g%n_green,max_part)

c         g%n_green(leaf) = g%n_green(leaf)
c     :          - sum_real_array (g%dlt_n_sen_supply, max_part)

         g%n_green(stem) = g%n_green(stem)
     :          + sum_real_array (dlt_n_sen_supply, max_part)


      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif


      call pop_routine (my_name)
      return
      end




*     ===========================================================
      subroutine nitrogen_initialisation (Option)
*     ===========================================================
      use CropModModule
      implicit none
      include   'const.inc'
      include 'crp_nitn.pub'
      include 'error.pub'                         
      include 'science.pub'
      include 'data.pub'

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*      Initialise plant nitrogen.

*+  Changes

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'nitrogen_initialisation')

      REAL n_avail(max_part)
      REAL n_avail_sum
      REAL dlt_leaf_n
      REAL dlt_stem_n
      REAL dlt_root_n


*- Implementation Section ----------------------------------
      call push_routine (my_name)
 

      if ((Option.eq.1).or.(Option.eq.2).or.(Option.eq.3)) then
 
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



         IF (((Option.eq.3).or.(Option.eq.1)).and.on_day_of (
     :            start_grain_fill, g%current_stage, g%days_tot)) then

          call crop_n_retrans_avail_nw(  max_part,
     :                                   root, 
     :                                   grain,
     :                                   g%nfact_expansion,
     :                                   g%N_conc_min,
     :                                   g%dm_green,
     :                                   g%N_green, 
     :                                   N_avail)

         n_avail_sum = sum_real_array(n_avail,max_part)

         g%n_green(grain) = 0.03 * g%dm_green(grain)
         g%n_green(grain) = min (g%n_green(grain),n_avail_sum)

         dlt_stem_n      = min(n_avail(stem), g%n_green(grain))
         g%n_green(stem) = g%n_green(stem) - dlt_stem_n

         dlt_leaf_n =  min(n_avail(leaf), g%n_green(grain)-dlt_stem_n)
         g%n_green(leaf) = g%n_green(leaf) - dlt_leaf_n

         dlt_root_n = g%n_green(grain) - dlt_stem_n - dlt_leaf_n
         g%n_green(root) = g%n_green(root) - dlt_root_n

         ENDIF

       
      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end


*     ===========================================================
      subroutine nitrogen_supply (Option)
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

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'nitrogen_supply')

*+  Local Variables
      real    fixation_determinant

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (p%uptake_source .eq. 'apsim') then
         ! do nothing here for now
         ! I assume that the retrans routine does not need the
         ! call below as it is called on its own from process routine.
         ! -NIH
 
      elseif (Option.eq.1) then
 
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



      elseif (Option.eq. 2) then
 
         fixation_determinant = sum_real_array(g%dm_green, max_part)
     :                        - g%dm_green(root)
 

             call cproc_n_supply_iw (
     :            g%dlayer
     :          , max_layer
     :          , g%dlt_sw_dep
     :          , g%NO3gsm
     :          , g%NO3gsm_min
     :          , g%NH4gsm
     :          , g%NH4gsm_min
     :          , g%root_depth
     :          , g%sw_dep
     :          , p%ll_dep
     :          , g%NO3gsm_mflow_avail
     :          , g%NH4gsm_mflow_avail
     :          , g%sw_avail
     :          , g%sw_avail_pot
     :          , g%NO3gsm_diffn_pot
     :          , g%NH4gsm_diffn_pot
     :          , G%current_stage
     :          , C%n_fix_rate
     :          , fixation_determinant
     :          , G%swdef_fixation
     :          , g%N_fix_pot
     :          )



 
      elseif (Option .eq. 3) then
 
 
       call Potential_N_extraction_nw(
     :                  max_layer,
     :                  g%root_length,
     :                  g%NO3gsm,
     :                  g%NO3gsm_min,
     :                  g%NO3ppm,
     :                  g%NH4gsm,
     :                  g%NH4gsm_min,
     :                  g%NH4ppm,
     :                  g%sw_dep,
     :                  p%ll_dep,
     :                  g%dul_dep,
     :                  g%sat_dep,
     :                  g%pot_extract_NO3gsm,
     :                  g%pot_extract_NH4gsm)

 
      else if (Option.eq.0) then

         !This module is excluded from the model
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine nitrogen_demand (Option)
*     ===========================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'crp_nitn.pub'
      include 'science.pub'
      include 'data.pub'
      include 'error.pub'

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     n demand

*+  Changes

*+  Constant Values
      integer num_demand_parts
      parameter (num_demand_parts = 4)
*
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'nitrogen_demand')

*+  Local Variables
      integer demand_parts(num_demand_parts)
      data demand_parts /root,leaf,stem,flower/
      save /demand_parts/

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option.eq.2) then

       call fill_real_array (g%dlt_N_retrans, 0.0, max_part)

       call cproc_N_demand_iw   !nwheat uses potential biomass growth rate to determine N demand ????????????????
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
     :              , g%N_demand
     :              , g%N_max
     :               )

           g%n_demand(flower) =0.0


      elseif ((Option.eq.1).or.(Option.eq.3)) then

c      call fill_real_array (g%dlt_N_retrans, 0.0, max_part)

       call cproc_N_demand2   !nwheat uses potential biomass growth rate to determine N demand ????????????????
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
     :              , g%N_demand
     :              , g%N_max
     :               )

           g%n_demand(flower) =0.0

      else if (Option.eq.0) then

         !This module is excluded from the model
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end


*     ===========================================================
      subroutine nitrogen_uptake (Option)
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

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'nitrogen_uptake')

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
 
      elseif ((Option.eq.1).OR.(Option.eq.4)) then
 
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

              call cproc_N_uptake_iw
     :               (
     :                C%no3_diffn_const
     :              , G%dlayer
     :              , max_layer
     :              , G%no3gsm_diffn_pot
     :              , G%no3gsm_mflow_avail
     :              , G%nh4gsm_mflow_avail
     :              , G%N_fix_pot
     :              , c%n_supply_preference
     :              , G%n_demand
     :              , G%n_max
     :              , max_part
     :              , G%root_depth
     :              , g%dlt_NO3gsm
     :              , g%dlt_NH4gsm
     :               )



      elseif (Option .eq. 3) then
 
       call  cproc_N_uptake_nw
     :               (
     :                c%n_supply_preference
     :              , g%n_demand
     :              , g%pot_extract_NO3gsm
     :              , g%pot_extract_NH4gsm
     :              , g%N_fix_pot
     :              , g%dlayer
     :              , max_layer
     :              , g%root_depth
     :              , g%n_max
     :              , max_part
     :              , g%dlt_NO3gsm
     :              , g%dlt_NH4gsm
     :               )
 
      else if (Option.eq.0) then

         !This module is excluded from the model
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end


* ====================================================================
       subroutine Nitrogen_partition (Option)
* ====================================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'convert.inc'
      include 'science.pub'
      include 'data.pub'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*     <insert here>

*+  Changes
*     <insert here>

*+  Locals
       REAL    dlt_N_uptake_sum
       integer deepest_layer


*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'Nitrogen_partition')

*- Implementation Section ----------------------------------
      call push_routine (myname)


       deepest_layer = find_layer_no (g%root_depth
     :                              ,g%dlayer
     :                              ,max_layer)

       dlt_N_uptake_sum = - sum_real_array(g%dlt_NO3gsm, deepest_layer)
     :                    - sum_real_array(g%dlt_NH4gsm, deepest_layer)

 

      if (Option.eq.1) then
 
        call cproc_N_partition_ew(
     .          g%N_demand,
     .          g%N_max,
     .          dlt_n_uptake_sum,
     .          g%dlt_N_green)


      else if (Option .eq. 2) then


        call cproc_N_partition_ew(
     .          g%N_demand,
     .          g%N_max,
     .          dlt_n_uptake_sum,
     .          g%dlt_N_green)



      else if (Option .eq. 3) then
 
        call sproc_N_partition_ew(
     .          g%root_depth,
     .          g%dlayer,
     .          g%N_demand,
     .          g%N_max,
     .          g%dlt_NO3gsm,
     .          g%dlt_NH4gsm,
     .          g%dlt_N_green
     .                     )


      elseif (Option.eq.4) then
 
        call sproc_N_partition1(
     .          g%root_depth,
     .          g%dlayer,
     .          g%N_demand,
     .          g%N_max,
     .          g%dlt_NO3gsm,
     .          g%dlt_N_green
     .                     )


      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (myname)
      return
      end


* ====================================================================
       subroutine nitrogen_retranslocation (Option)
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
      parameter (myname = 'nitrogen_retranslocation')

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      if (Option .eq. 1) then
   
         call cproc_N_retranslocate_nw (  !for nwheat
     .          g%current_stage, 
     .          g%dlt_dm_green,
     .          g%N_conc_min,
     .          g%N_conc_crit,
     .          g%N_conc_max,
     .          c%N_conc_max_grain,     
     :          g%nfact_expansion,
     :          g%maxt,
     :          g%mint,
     :          g%dlt_tt,
     :          g%grain_no,
     .          g%dm_green,
     .          g%N_green,
     .          g%N_senesced,
     .          g%N_dead,
     .          g%dlt_N_retrans)
   
      elseif (Option.eq.2) then

         call cproc_N_retranslocate_iw (  !for i_wheat
     .          g%current_stage,
     .          g%dm_green,
     .          g%dlt_dm_green,
     .          g%N_conc_min,
     .          g%N_conc_crit,
     .          g%N_conc_max,
     .          c%N_conc_max_grain,
     .          g%N_green,
     .          g%dlt_n_green,
     .          g%N_senesced,
     .          g%N_dead,
     .          g%phase_tt,
     .          g%tt_tot,
     .          g%accum_rad_10d,
     .          g%lai,

     .          g%dm_senesced,
     .          g%dlt_dm_senesced,
     .          g%dlt_dm_green_retrans,
     .          g%dlt_dm_sen_retrans,

     .          g%dlt_n_senesced,
     .          g%dlt_N_retrans,
     .          g%dlt_N_sen_retrans)


      else if (Option .eq. 3) then
   
         call cproc_N_retranslocate_nw (  !for nwheat
     .          g%current_stage, 
     .          g%dlt_dm_green,
     .          g%N_conc_min,
     .          g%N_conc_crit,
     .          g%N_conc_max,
     .          c%N_conc_max_grain,     
     :          g%nfact_expansion,
     :          g%maxt,
     :          g%mint,
     :          g%dlt_tt,
     :          g%grain_no,
     .          g%dm_green,
     .          g%N_green,
     .          g%N_senesced,
     .          g%N_dead,
     .          g%dlt_N_retrans)

      elseif (Option.eq.200) then

          call  cproc_N_retranslocate2 (  !for i_wheat
     .          g%current_stage, 
     .          g%dlt_dm_green,
     .          g%N_conc_min,
     .          g%N_conc_crit,
     .          g%N_conc_max,
     .          c%N_conc_max_grain,     
     .          g%dm_green,
     .          g%N_green,
     .          g%N_senesced,
     .          g%N_dead,
     .          g%dlt_N_retrans)
      
      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (myname)
      return
      end



*     ===========================================================
      subroutine nitrogen_stress(Option)
*     ===========================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'crp_nitn.pub'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*     Get current Nitrogen stress factors (0-1)

*+  Local variables
       REAL rue_red_fac
       REAL sla_est

*+  Changes

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'nitrogen_stress')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (Option.eq.1) then
 
c        call nitrogen_stress_wang (
c     :                          leaf,
c     :                          stem,
c     :                          emerg,
c     :                          g%current_stage,
c     :                          g%dm_green,
c     :                          g%n_conc_crit,
c     :                          g%n_conc_min,
c     :                          g%n_green,
c     :                          g%nfact_photo,
c     :                          g%nfact_expansion,
c     :                          g%nfact_pheno,
c     :                          g%nfact_tiller)

        call nitrogen_stress_nw (
     :                          leaf, 
     :                          stem, 
     :                          emerg,
     :                          g%current_stage,
     :                          g%dm_green,
     :                          g%n_conc_crit, 
     :                          g%n_conc_min,
     :                          g%n_green, 
     :                          g%nfact_photo,
     :                          g%nfact_expansion,
     :                          g%nfact_pheno,
     :                          g%nfact_tiller)


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

c         g%nfact_tiller = g%nfact_expansion

      elseif (Option.eq.2) then
 
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



             call iw_sla_est(
     .          g%current_stage,  
     .          g%accum_rad_10d,
     .          g%tt_tot,
     .          g%phase_tt,
     .          sla_est)


            call iw_rue_red_fac(
     .          0.00011,         !p%sln_critical= sln_cr = 0.00011
     .          sla_est,  
     .          g%dm_green,
     .          g%N_conc_min,
     .          g%N_green,
     .          g%tt_tot,
     .          rue_red_fac)


         g%nfact_photo     = rue_red_fac
         g%nfact_expansion = 1.0
         g%nfact_pheno    = 1.0



      else if (Option .eq. 3) then  !the nwheat appraoch
   
        call nitrogen_stress_nw (
     :                          leaf, 
     :                          stem, 
     :                          emerg,
     :                          g%current_stage,
     :                          g%dm_green,
     :                          g%n_conc_crit, 
     :                          g%n_conc_min,
     :                          g%n_green, 
     :                          g%nfact_photo,
     :                          g%nfact_expansion,
     :                          g%nfact_pheno,
     :                          g%nfact_tiller)
        
      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif


      if (INT(g%current_stage).le.3) then

         g%nfact_photo     = 1.0
         g%nfact_expansion = 1.0
         g%nfact_pheno     = 1.0
         g%nfact_tiller    = 1.0
      end if


      call pop_routine (my_name)
      return
      end








* ====================================================================
       subroutine Crop_death (Option)
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
      parameter (myname = 'Crop_death')

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
      subroutine Crop_Detachment(option)
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
      parameter (my_name = 'Crop_Detachment')

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




