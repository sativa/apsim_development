C     Last change:  E    18 Nov 1999    2:23 pm
      include 'sorghum.inc'
!     ===========================================================
      subroutine AllocInstance (InstanceName, InstanceNo)
!     ===========================================================
      use SorgModule
      implicit none
 
!+  Sub-Program Arguments
      character InstanceName*(*)       ! (INPUT) name of instance
      integer   InstanceNo             ! (INPUT) instance number to allocate
 
!+  Purpose
!      Module instantiation routine.
 
!- Implementation Section ----------------------------------
               
      allocate (Instances(InstanceNo)%gptr)
      allocate (Instances(InstanceNo)%pptr)
      allocate (Instances(InstanceNo)%cptr)
      Instances(InstanceNo)%Name = InstanceName
 
      return
      end

!     ===========================================================
      subroutine FreeInstance (anInstanceNo)
!     ===========================================================
      use SorgModule
      implicit none
 
!+  Sub-Program Arguments
      integer anInstanceNo             ! (INPUT) instance number to allocate
 
!+  Purpose
!      Module de-instantiation routine.
 
!- Implementation Section ----------------------------------
               
      deallocate (Instances(anInstanceNo)%gptr)
      deallocate (Instances(anInstanceNo)%pptr)
      deallocate (Instances(anInstanceNo)%cptr)
 
      return
      end
     
!     ===========================================================
      subroutine SwapInstance (anInstanceNo)
!     ===========================================================
      use SorgModule
      implicit none
 
!+  Sub-Program Arguments
      integer anInstanceNo             ! (INPUT) instance number to allocate
 
!+  Purpose
!      Swap an instance into the global 'g' pointer
 
!- Implementation Section ----------------------------------
               
      g => Instances(anInstanceNo)%gptr
      p => Instances(anInstanceNo)%pptr
      c => Instances(anInstanceNo)%cptr
 
      return
      end
*     ================================================================
      subroutine Main (action, data_string)
*     ================================================================
      use SorgModule
      implicit none
      include   'const.inc'            ! ACTION_presence, ACTION_init, ACTION_process
      include 'action.inc'
      include 'crp_comm.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      character  action*(*)            ! (INPUT) Message action to perform
      character  data_string*(*)       ! (INPUT) Message data

*+  Purpose
*      This module performs crop growth
*       simulates root, leaf, head, stem and grain development. Water and
*       nitrogen uptake, photosynhesis, and leaf and root senescense.

*+  Changes
*      250894 sc   specified and programmed
*      011195 jngh  added call to message_unused
*      060599 sdb removed version reference and presence action
*      261099 dph added ACTION_Create handler

*+  Constant Values
      character  my_name*(*)           ! name of this procedure
      parameter (my_name='sorg')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
         ! initialise error flags

      if (action.eq.ACTION_Create) then
         call sorg_zero_all_global_variables()
 
      elseif (action.eq.ACTION_init) then
            ! zero pools
         call sorg_zero_variables ()
            ! Get constants
         call sorg_init ()
            ! request and receive variables from owner-modules
         call sorg_get_other_variables ()
 
      elseif (action.eq.ACTION_set_variable) then
            ! respond to request to reset variable values - from modules
         call sorg_set_my_variable (data_string)
 
      elseif (action.eq.ACTION_get_variable) then
            ! respond to request for variable values - from modules
         call sorg_send_my_variable (Data_string)
 
      elseif (action.eq.ACTION_prepare) then
         if (g%plant_status.ne.status_out) then
            call sorg_zero_daily_variables ()
               ! request and receive variables from owner-modules
            call sorg_get_other_variables ()
               ! do crop processes
            call sorg_prepare ()
         else
            ! crop not in
            call sorg_zero_variables ()
         endif
 
 
      elseif (action.eq.ACTION_sow) then
         if (crop_my_type (c%crop_type)) then
               ! request and receive variables from owner-modules
            call sorg_get_other_variables ()
               ! start crop and do  more initialisations
            call sorg_start_crop ()
         else
            ! not my type!
            call Message_unused ()
         endif
 
      elseif (action.eq.ACTION_process) then
         if (g%plant_status.ne.status_out) then
c            call sorg_zero_daily_variables ()
               ! request and receive variables from owner-modules
            call sorg_get_other_variables ()
               ! do crop processes
            call sorg_process ()
               ! send changes to owner-modules
            call sorg_set_other_variables ()
         else
            ! crop not in
c            call sorg_zero_variables ()
         endif
      elseif (action.eq.ACTION_harvest) then
         if (Crop_my_type (c%crop_type)) then
               ! harvest crop - turn into residue
               call sorg_harvest (
     .          g%dm_green,
     .          g%dm_dead,
     .          c%grn_water_cont,
     .          g%grain_no,
     .          g%plants,
     .          g%dm_senesced,
     .          g%leaf_no,
     .          g%N_green,
     .          g%N_dead,
     .          g%N_senesced,
     .          g%flowering_date,
     .          g%maturity_date,
     .          g%flowering_das,
     .          g%maturity_das,
     .          g%lai_max,
     .          g%cswd_photo,
     .          g%days_tot,
     .          g%cswd_expansion,
     .          g%cnd_photo,
     .          g%cnd_grain_conc,
     .          c%stage_names)
        else
            ! not my type!
            call Message_unused ()
         endif
 
      elseif (action.eq.ACTION_end_crop) then
         if (crop_my_type (c%crop_type)) then
               ! end crop - turn into residue
            call sorg_end_crop ()
         else
            ! not my type!
            call Message_unused ()
         endif
 
      elseif (action.eq.ACTION_kill_crop) then
         if (crop_my_type (c%crop_type)) then
               ! kill crop - die
            call sorg_kill_crop (
     .          g%plant_status,
     .          g%dm_green,
     .          g%dm_senesced,
     .          g%dm_dead)
         else
            ! not my type!
            call Message_unused ()
         endif
      else
               ! don't use message
         call Message_unused ()
 
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sorg_process ()
*     ===========================================================
      use SorgModule
      implicit none
      include   'const.inc'
      include 'data.pub'                          
      include 'error.pub'                         

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

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
!CROP WATER SUPPLY
 
      call Sorg_root_depth(1)          !CT = crop template
      call Sorg_root_depth_init(2)     !CT - called later as root_depth sets delta
                                        ! option 1 initial root depth = c%...
                                        ! option 2 initial root depth = sowing depth
      call Sorg_water_supply(1)        !CT
      call Sorg_water_stress(1)        !CT
 
!CROP WATER DEMAND (following are in PREPARE section for APSWIM version)
!cf Sorg light_supply is based on direct calculation of k from rowspacing etc
!rather than looking for cover calculation (that is only done at end of day)
!Problem with other modules is that they use cover_green which is calculated elsewhere!
!      call sorg_nit_stress(400)          !CT
      call sorg_nit_stress(401)          !GMC set to 1 for tests    SORGHUM
      call Sorg_temp_stress(1)         !CT
      call sorg_light_supply(400)
      call Sorg_bio_RUE(1)             !CT
      call Sorg_transpiration_eff(1)   !CT No bounding on VPD or TE
      call Sorg_water_demand(1)        !CT!movable!
 
      if (g%plant_status.eq.status_alive) then
 
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

!---------------------    SORGHUM  ---------------------------------
 
!DEATH of PLANTS (cf. plant part pools)
         call sorg_plant_death(1)
 
!Check to see if plant death should terminate crop
 
         if(reals_are_equal (g%dlt_plants_dead + g%plants, 0.0))then
            call sorg_kill_crop (
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
      call Sorg_detachment(1)
      call sorg_cleanup()        !CODE is almost same as maize
 
      call pop_routine (my_name)
 
      return
      end



*     ===========================================================
      subroutine sorg_cleanup ()
*     ===========================================================
      use SorgModule
      implicit none
      include 'error.pub'                         

*+  Purpose
*       clean

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sorg_cleanup')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      call sorg_update ()
 
      call sorg_check_bounds (
     .          g%leaf_no,
     .          g%leaf_no_dead,
     .          g%root_depth,
     .          g%dlayer,
     .          g%grain_no,
     .          p%head_grain_no_max,
     .          g%plants,
     .          g%current_stage,
     .          g%phase_tt,
     .          g%days_tot,
     .          g%tt_tot,
     .          g%canopy_height,
     .          c%height_max,
     .          g%lai,
     .          g%slai,
     .          g%tlai_dead,
     .          g%cover_green,
     .          g%cover_sen,
     .          g%cover_dead,
     .          g%leaf_area,
     .          g%heat_stress_tt,
     .          g%dm_stress_max,
     .          g%N_conc_crit,
     .          g%N_conc_min,
     .          g%N_conc_max,
     .          g%N_dead,
     .          g%N_green,
     .          g%N_senesced,
     .          g%dm_dead,
     .          g%dm_green,
     .          g%dm_senesced)
 
      call sorg_totals (
     .          g%N_green,
     .          g%dm_green,
     .          g%dlt_N_retrans,
     .          g%N_conc_crit,
     .          g%N_demand,
     .          g%root_depth,
     .          g%dlayer,
     .          g%current_stage,
     .          g%days_tot,
     .          g%N_uptake_tot,
     .          g%transpiration_tot,
     .          g%dlt_sw_dep,
     .          g%N_conc_act_stover_tot,
     .          g%N_conc_crit_stover_tot,
     .          g%N_demand_tot,
     .          g%N_uptake_stover_tot,
     .          g%N_uptake_grain_tot,
     .          g%lai_max,
     .          g%lai,
     .          g%flowering_date,
     .          g%maturity_date,
     .          g%flowering_das,
     .          g%maturity_das,
     .          g%N_dead,
     .          g%N_senesced,
     .          g%day_of_year)
      call sorg_event (
     .          g%current_stage,
     .          g%days_tot,
     .          c%stage_code_list,
     .          c%stage_names,
     .          g%dm_green,
     .          g%dm_senesced,
     .          g%dm_dead,
     .          g%N_green,
     .          g%root_depth,
     .          g%dlayer,
     .          g%sw_dep,
     .          p%ll_dep,
     .          g%lai)
 
      call pop_routine (my_name)
 
      return
      end



*     ===========================================================
      subroutine sorg_harvest (
     .          g_dm_green,
     .          g_dm_dead,
     .          c_grn_water_cont,
     .          g_grain_no,
     .          g_plants,
     .          g_dm_senesced,
     .          g_leaf_no,
     .          g_N_green,
     .          g_N_dead,
     .          g_N_senesced,
     .          g_flowering_date,
     .          g_maturity_date,
     .          g_flowering_das,
     .          g_maturity_das,
     .          g_lai_max,
     .          g_cswd_photo,
     .          g_days_tot,
     .          g_cswd_expansion,
     .          g_cnd_photo,
     .          g_cnd_grain_conc,
     .          c_stage_names)
*     ===========================================================
      implicit none
      include   'const.inc'            ! new_line, lu_scr_sum, blank,
      include   'convert.inc'          ! gm2kg, sm2ha, sm2smm
      include   'sorgcons.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
       real g_dm_green(*)
       real g_dm_dead(*)
       real c_grn_water_cont
       real g_grain_no
       real g_plants
       real g_dm_senesced(*)
       real g_leaf_no(*)
       real g_N_green(*)
       real g_N_dead(*)
       real g_N_senesced(*)
       integer g_flowering_date
       integer g_maturity_date
       integer g_flowering_das
       integer g_maturity_das
       real g_lai_max
       real g_cswd_photo(*)
       real g_days_tot(*)
       real g_cswd_expansion(*)
       real g_cnd_photo(*)
       real g_cnd_grain_conc(*)
       character c_stage_names(*)*(*)

*+  Purpose
*       Report occurence of harvest and the current status of specific
*       variables.

*+  Changes
*     010994 sc   specified and programmed

*+  Calls
                                       ! lu_scr_sum

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'harvest')

*+  Local Variables
      real       biomass_dead          ! above ground dead plant wt (kg/ha)
      real       biomass_green         ! above ground green plant wt (kg/ha)
      real       biomass_senesced      ! above ground senesced plant wt (kg/ha)
      real       dm                    ! above ground total dry matter (kg/ha)
      real       grain_wt              ! grain dry weight (g/kernel)
      real       head_grain_no         ! final grains /head
      real       leaf_no               ! total leaf number
      real       N_grain               ! total grain N uptake (kg/ha)
      real       N_dead                ! above ground dead plant N (kg/ha)
      real       N_green               ! above ground green plant N (kg/ha)
      real       N_senesced            ! above ground senesced plant N (kg/ha)
      real       N_stover              ! nitrogen content of stover (kg\ha)
      real       N_total               ! total gross nitrogen content (kg/ha)
      real       N_grain_conc_percent  ! grain nitrogen %
      integer    phase                 ! phenological phase number
      real       si1                   ! mean water stress type 1
      real       si2                   ! mean water stress type 2
      real       si4                   ! mean nitrogen stress type 1
      real       si5                   ! mean nitrogen stress type 2
      real       stover                ! above ground dry weight less grain
                                       ! (kg/ha)
      character  string*200            ! message
      real       yield                 ! grain yield dry wt (kg/ha)
      real       yield_wet             ! grain yield including moisture
                                       ! (kg/ha)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
          ! crop harvested. Report status
 
      yield = (g_dm_green(grain) + g_dm_dead(grain))
     :      * gm2kg / sm2ha
 
          ! include the grain water content
      yield_wet = yield / (1.0 - c_grn_water_cont)
 
      grain_wt = divide (g_dm_green(grain) + g_dm_dead(grain)
     :                 , g_grain_no, 0.0)
!cpsc
      head_grain_no = divide (g_grain_no, g_plants, 0.0)
 
      biomass_green = (sum_real_array (g_dm_green, max_part)
     :              - g_dm_green(root))
     :              * gm2kg / sm2ha
 
      biomass_senesced = (sum_real_array (g_dm_senesced, max_part)
     :                 - g_dm_senesced(root))
     :                 * gm2kg / sm2ha
 
      biomass_dead = (sum_real_array (g_dm_dead, max_part)
     :             - g_dm_dead(root))
     :             * gm2kg / sm2ha
 
      dm = (biomass_green + biomass_senesced + biomass_dead)
 
      stover = dm - yield
 
      leaf_no = sum_between (emerg, harvest_ripe, g_leaf_no)
      N_grain_conc_percent = divide (g_N_green(grain) + g_N_dead(grain)
     :                            , g_dm_green(grain) + g_dm_dead(grain)
     :                            , 0.0)
     :                     * fract2pcnt
 
      N_grain = (g_N_green(grain) + g_N_dead(grain))
     :        * gm2kg/sm2ha
 
      N_green = (sum_real_array (g_N_green, max_part)
     :        - g_N_green(root) - g_N_green(grain))
     :        * gm2kg / sm2ha
 
      N_senesced = (sum_real_array (g_N_senesced, max_part)
     :           - g_N_senesced(root) - g_N_senesced(grain))
     :           * gm2kg / sm2ha
 
      N_dead = (sum_real_array (g_N_dead, max_part)
     :       - g_N_dead(root) - g_N_dead(grain))
     :       * gm2kg / sm2ha
 
      N_stover = N_green + N_senesced + N_dead
      N_total = N_grain + N_stover
 
 
      call write_string (new_line//new_line)
 
      write (string, '(a,i4,t40,a,i4)')
     :            'flowering (DAS) =', g_flowering_das
     :            ,'maturity (DAS)  =', g_maturity_das
      call write_string (string)
 
      write (string, '(a,i4,t40,a,f10.1)')
     :            ' flowering day  = ',g_flowering_date
     :          , ' stover (kg/ha) =',stover
      call write_string (string)
 
      write (string, '(a,i4,t40,a,f10.1)')
     :            ' maturity day        = ', g_maturity_date
     :          , ' grain yield (kg/ha) =', yield
      call write_string (string)
 
      write (string, '(a,f6.1,t40,a,f10.1)')
     :            ' grain % water content   = ', c_grn_water_cont
     :                                         * fract2pcnt
     :          , ' grain yield wet (kg/ha) =', yield_wet
      call write_string (string)
 
      write (string, '(a,f10.3,t40,a,f10.3)')
     :            ' grain wt (g) =', grain_wt
     :          , ' grains/m^2   =', g_grain_no
      call write_string (string)
 
      write (string, '(a,f6.1,t40,a,f6.3)')
     :            ' grains/head =', head_grain_no
     :          , ' maximum lai =', g_lai_max
      call write_string (string)
 
      write (string, '(a,f10.1)')
     :            ' total above ground biomass (kg/ha) =', dm
      call write_string (string)
 
      write (string, '(a,f10.1)')
     :         ' live above ground biomass (kg/ha) =', biomass_green
     :                                               + biomass_senesced
      call write_string (string)
 
      write (string, '(a,f10.1)')
     :            ' green above ground biomass (kg/ha) =', biomass_green
      call write_string (string)
 
      write (string, '(a,f10.1)')
     :      ' senesced above ground biomass (kg/ha) =', biomass_senesced
      call write_string (string)
 
      write (string, '(a,f10.1)')
     :            ' dead above ground biomass (kg/ha) =', biomass_dead
      call write_string (string)
 
      write (string, '(a,f6.1)')
     :            ' number of leaves =', leaf_no
      call write_string (string)
 
      write (string, '(a,f10.2,t40,a,f10.2)')
     :            ' grain N percent =', N_grain_conc_percent
     :          , ' total N content (kg/ha) =', N_total
      call write_string (string)
 
      write (string, '(a,f10.2,t40,a,f10.2)')
     :            ' grain N uptake (kg/ha) =', N_grain
     :          , ' senesced N content (kg/ha) =', N_senesced
 
      call write_string (string)
 
      write (string, '(a,f10.2,t40,a,f10.2)')
     :            ' green N content (kg/ha) =', N_green
     :          , ' dead N content (kg/ha) =', N_dead
      call write_string (string)
 
      do 2000 phase = emerg_to_endjuv, start_to_end_grain
         si1 = divide (g_cswd_photo(phase)
     :               , g_days_tot(phase), 0.0)
         si2 = divide (g_cswd_expansion(phase)
     :               , g_days_tot(phase), 0.0)
         si4 = divide (g_cnd_photo(phase)
     :               , g_days_tot(phase), 0.0)
         si5 = divide (g_cnd_grain_conc(phase)
     :               , g_days_tot(phase), 0.0)
 
         call write_string (new_line//new_line)
 
         write (string,'(2a)')
     :         ' stress indices for ', c_stage_names(phase)
         call write_string (string)
 
         write (string,'(2(a, f16.7))')
     :         ' water stress 1 =', si1
     :         , '   nitrogen stress 1 =', si4
         call write_string (string)
 
         write (string,'(2(a, f16.7))')
     :         ' water stress 2 =', si2
     :         , '   nitrogen stress 2 =', si5
         call write_string (string)
2000  continue
 
      g_dm_green(grain) = 0.0
      g_N_green(grain) = 0.0
 
      g_dm_dead(grain) = 0.0
      g_N_dead(grain) = 0.0
 
      call pop_routine (my_name)
      return
      end

*     ===========================================================
      subroutine sorg_zero_all_global_variables ()
*     ===========================================================
      use SorgModule
      implicit none
      include 'error.pub'

*+  Purpose
*       Zero all global variables

*+  Changes
*     26/10/99 dph 

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sorg_all_global_variables')

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      
      g%plant_status = ' '
      g%row_spacing = 0.0        
      g%sowing_depth = 0.0
      g%year = 0              
      g%day_of_year = 0       
      g%swdef_expansion  = 0.0
      g%swdef_photo = 0.0
      g%swdef_pheno = 0.0
      g%nfact_expansion = 0.0
      g%nfact_photo = 0.0
      g%nfact_grain_conc = 0.0
      g%nfact_pheno = 0.0
      g%temp_stress_photo = 0.0
      g%swdef_fixation = 0.0
      g%fr_intc_radn = 0.0
      g%latitude  = 0.0
      g%radn = 0.0                
      g%mint = 0.0                
      g%maxt = 0.0                
      g%soil_temp(:) = 0.0      
      g%vpd = 0.0
      g%eo = 0.0
      g%cnd_photo(:) = 0.0
      g%cnd_grain_conc (:) = 0.0
      g%cswd_photo(:) = 0.0     
      g%cswd_expansion(:) = 0.0 
      g%cswd_pheno(:) = 0.0     
      g%dlt_tt = 0.0              
      g%tt_tot(:) = 0.0   
      g%phase_tt(:) = 0.0 
      g%dlt_tt_curv = 0.0         
      g%tt_curv_tot(:) = 0.0  
      g%phase_tt_curv(:) = 0.0
      g%dlt_tt_other = 0.0        
      g%tt_other_tot(:) = 0.0 
      g%phase_tt_other(:) = 0.0    
      g%heat_stress_tt(:) = 0.0 
      g%dlt_heat_stress_tt = 0.0 
      g%dlt_stage = 0.0          
      g%current_stage = 0.0      
      g%previous_stage = 0.0     
      g%days_tot(:) = 0.0
      g%dlt_canopy_height = 0.0  
      g%canopy_height = 0.0      
      g%plants = 0.0             
      g%dlt_plants = 0.0         
      g%grain_no = 0.0           
      g%dlt_root_depth = 0.0     
      g%root_depth = 0.0         
      g%cover_green = 0.0        
      g%cover_sen = 0.0          
      g%cover_dead = 0.0         
      g%dlt_plants_all = 0.0
      g%dlt_plants_temp = 0.0
      g%dlt_plants_water = 0.0
      g%dlt_plants_barren = 0.0
      g%dlt_plants_dead = 0.0     
      g%dlt_dm = 0.0              
      g%dlt_dm_green(:) = 0.0 
      g%dlt_dm_senesced(:) = 0.0
      g%dlt_dm_detached(:) = 0.0 
      g%dlt_dm_dead_detached(:) = 0.0
      g%dlt_dm_green_retrans(:) = 0.0
      g%dlt_dm_sen_retrans(:) = 0.0
      g%dm_stress_max(:) = 0.0
      g%dlt_dm_stress_max = 0.0
      g%dlt_dm_grain_demand = 0.0
      g%dm_green_demand(:) = 0.0
      g%dm_dead(:) = 0.0   
      g%dm_green(:) = 0.0  
      g%dm_senesced(:) = 0.0
      g%dm_plant_top_tot(:) = 0.0
      g%dlt_dm_light = 0.0         
      g%dlt_dm_water = 0.0
      g%dlt_dm_N = 0.0             
      g%radn_int = 0.0             
      g%transp_eff = 0.0  
      g%slai = 0.0        
      g%dlt_slai = 0.0    
      g%dlt_lai = 0.0     
      g%dlt_lai_pot = 0.0         
      g%dlt_lai_stressed = 0.0    
      g%tpla_today = 0.0
      g%tpla_yesterday = 0.0
      g%lai = 0.0                 
      g%tlai_dead = 0.0           
      g%dlt_slai_detached = 0.0   
      g%dlt_tlai_dead_detached = 0.0
      g%dlt_slai_age = 0.0
      g%dlt_slai_light = 0.0
      g%dlt_slai_water = 0.0
      g%dlt_slai_frost = 0.0
      g%leaf_no(:) = 0.0
      g%node_no(:) = 0.0
      g%dlt_leaf_no_pot = 0.0
      g%dlt_node_no_pot = 0.0
      g%leaf_no_dead(:) = 0.0
      g%dlt_leaf_no = 0.0        
      g%dlt_leaf_no_dead = 0.0   
      g%leaf_no_final = 0.0      
      g%leaf_area(:) = 0.0
      g%lai_equilib_light(:) = 0.0
      g%lai_equilib_water(:) = 0.0
      g%tiller_no_fertile = 0.0  
      g%dlt_tiller_no = 0.0      
      g%dlt_stiller_no = 0.0   
      g%swdef_lai_loss = 0.0 
      g%lai_max_possible = 0.0  
      g%N_demand(:) = 0.0
      g%N_max(:) = 0.0
      g%dlt_N_green(:) = 0.0 
      g%dlt_N_senesced(:) = 0.0 
      g%dlt_N_detached(:) = 0.0 
      g%dlt_N_dead_detached(:)  = 0.0 
      g%N_dead(:) = 0.0      
      g%N_green(:) = 0.0     
      g%N_senesced(:) = 0.0  
      g%dlt_N_retrans(:) = 0.0                                            
      g%dlt_NO3gsm(:) = 0.0  
      g%NO3gsm(:) = 0.0   
      g%NO3gsm_min(:) = 0.0  
      g%NO3gsm_diffn_pot(:) = 0.0  
      g%NO3gsm_mflow_avail(:) = 0.0  
      g%n_fix_pot = 0.0 
      g%N_conc_crit(:) = 0.0 
      g%N_conc_max(:) = 0.0  
      g%N_conc_min(:) = 0.0  
      g%dm_plant_min(:) = 0.0 
      g%dlayer(:) = 0.0     
      g%dlt_sw_dep(:) = 0.0 
      g%dul_dep(:) = 0.0    
      g%sw_dep(:) = 0.0     
      g%sw_demand = 0.0             
      g%sw_avail_pot(:) = 0.0  
      g%sw_avail(:) = 0.0   
      g%sw_supply(:) = 0.0  
      g%sw_supply_sum = 0.0         
      g%sw_supply_demand_ratio = 0.0 
      g%root_length(:) = 0.0 
      g%dlt_root_length(:) = 0.0  
      g%dlt_root_length_senesced(:) = 0.0 
      g%num_layers = 0          
      g%transpiration_tot = 0.0  
      g%N_uptake_tot = 0.0       
      g%N_demand_tot = 0.0       
      g%N_conc_act_stover_tot = 0.0   
      g%N_conc_crit_stover_tot = 0.0  
      g%N_uptake_grain_tot = 0.0      
      g%N_uptake_stover_tot = 0.0     
      g%lai_max = 0.0                 
      g%flowering_date = 0          
      g%maturity_date = 0           
      g%flowering_das = 0           
      g%maturity_das = 0           
      g%canopy_SLN = 0.0
      g%dlt_canopy_SLN = 0.0
      g%dlt_tt_fm = 0.0           
      g%tt_tot_fm(:) = 0.0
      g%dm_green_tot_fi = 0.0  
      g%uptake_water(:) = 0.0
      g%uptake_no3  (:) = 0.0
      g%num_uptake_water = 0
      g%num_uptake_no3 = 0   
      p%tt_maturity_to_ripe = 0.0   
      p%tt_flag_to_flower = 0.0     
      p%tt_flower_to_start_grain = 0.0 
      p%tt_emerg_to_endjuv = 0.0 
      p%tt_endjuv_to_init = 0.0  
      p%tt_flower_to_maturity = 0.0
      p%pp_endjuv_to_init = 0.0  
      p%photoperiod_crit1 = 0.0  
      p%photoperiod_crit2 = 0.0  
      p%photoperiod_slope = 0.0  
      p%est_days_endjuv_to_init = 0 
      p%hi_incr = 0.0                 
      p%x_hi_max_pot_stress(:) = 0.0
      p%y_hi_max_pot(:) = 0.0 
      p%num_hi_max_pot = 0
      p%main_stem_coef = 0.0  
      p%tpla_prod_coef = 0.0  
      p%tpla_inflection = 0.0 
      p%spla_prod_coef = 0.0  
      p%spla_intercept = 0.0  
      p%head_grain_no_max = 0.0   
      p%grain_gth_rate = 0.0      
      p%x_stem_wt(:) = 0.0
      p%y_height(:) = 0.0 
      p%num_stem_wt = 0         
      p%kl(:) = 0.0       
      p%ll_dep(:) = 0.0   
      p%xf(:) = 0.0       
      p%dm_per_seed = 0.0      
      p%uptake_source = ' ' 
      c%stage_names(:) = ' ' 
      c%crop_type = ' '
      c%x_sw_ratio(:) = 0.0        
      c%y_sw_fac_root(:) = 0.0     
      c%x_sw_demand_ratio(:) = 0.0 
      c%y_swdef_leaf(:) = 0.0      
      c%x_sw_avail_ratio(:) = 0.0  
      c%y_swdef_pheno(:) = 0.0     
      c%num_sw_ratio = 0                 
      c%num_sw_demand_ratio = 0          
      c%num_sw_avail_ratio = 0            
      c%stage_code_list(:) = 0.0 
      c%twilight = 0.0           
      c%N_conc_crit_grain = 0.0  
      c%N_conc_max_grain = 0.0   
      c%N_conc_min_grain = 0.0   
      c%N_conc_crit_root = 0.0   
      c%N_conc_max_root = 0.0    
      c%N_conc_min_root = 0.0    
      c%x_stage_code(:) = 0.0 
      c%y_n_conc_crit_leaf(:) = 0.0
      c%y_n_conc_max_leaf(:) = 0.0 
      c%y_n_conc_min_leaf(:) = 0.0 
      c%y_n_conc_crit_stem(:) = 0.0
      c%y_n_conc_max_stem(:) = 0.0 
      c%y_n_conc_min_stem(:) = 0.0 
      c%y_n_conc_crit_flower(:) = 0.0
      c%y_n_conc_max_flower(:) = 0.0 
      c%y_n_conc_min_flower(:) = 0.0 
      c%N_fact_photo = 0.0        
      c%N_fact_pheno = 0.0        
      c%N_fact_pheno_lb = 0.0     
      c%N_fact_expansion = 0.0       
      c%N_init_conc (:) = 0.0
      c%N_sen_conc (:) = 0.0
      c%num_N_conc_stage = 0    
      c%extinction_coef = 0.0     
      c%extinction_coef_dead = 0.0 
      c%extinction_coef_change = 0.0 
      c%x_row_spacing(:) = 0.0
      c%y_extinct_coef(:) = 0.0
      c%y_extinct_coef_dead(:) = 0.0
      c%root_extinction = 0.0        
      c%rue(:) = 0.0   
      c%root_depth_rate(:) = 0.0 
      c%ratio_root_shoot(max_stage) = 0.0
      c%num_Row_spacing = 0     
      c%leaf_no_crit = 0.0        
      c%tt_emerg_limit = 0.0      
      c%days_germ_limit = 0.0     
      c%swdf_pheno_limit = 0.0    
      c%swdf_photo_limit = 0.0    
      c%swdf_photo_rate = 0.0     
      c%initial_root_depth = 0.0  
      c%specific_root_length = 0.0
      c%sla_max = 0.0             
      c%sla_min = 0.0             
      c%tiller_coef = 0.0         
      c%tpla_inflection_ratio = 0.0
      c%initial_tpla = 0.0       
      c%height_max = 0.0         
      c%height_stem_slope = 0.0  
      c%svp_fract = 0.0          
      c%transp_eff_cf(:) = 0.0  
      c%n_fix_rate(:) = 0.0
      c%head_grain_no_crit = 0.0  
      c%barren_crit = 0.0         
      c%pesw_germ = 0.0           
      c%grain_N_conc_min = 0.0    
      c%seed_wt_min = 0.0         
      c%growth_rate_min = 0.0     
      c%growth_rate_crit = 0.0    
      c%leaf_no_at_emerg = 0.0    
      c%photoperiod_base = 0.0    
      c%NO3_diffn_const = 0.0     
      c%shoot_lag = 0.0           
      c%shoot_rate = 0.0          
      c%x_node_no_app(:) = 0.0
      c%y_node_app_rate(:) = 0.0
      c%y_leaves_per_node(:) = 0.0
      c%leaf_app_rate = 0.0       
      c%leaf_app_rate1 = 0.0      
      c%leaf_app_rate2 = 0.0      
      c%leaf_no_rate_change = 0.0 
      c%dm_leaf_init = 0.0        
      c%dm_root_init = 0.0        
      c%dm_stem_init = 0.0        
      c%leaf_init_rate = 0.0      
      c%leaf_no_seed = 0.0        
      c%dm_root_sen_frac = 0.0    
      c%dm_leaf_sen_frac = 0.0    
      c%dead_detach_frac(:) = 0.0
      c%sen_detach_frac(:) = 0.0 
      c%dm_leaf_detach_frac = 0.0
      c%minsw = 0.0               
      c%swdf_grain_min = 0.0     
      c%hi_min = 0.0              
      c%sfac_slope = 0.0          
      c%tfac_slope = 0.0          
      c%lai_sen_light = 0.0       
      c%sw_fac_max = 0.0          
      c%x_temp_senescence(:) = 0.0 
      c%y_senescence_fac(:) = 0.0  
      c%temp_fac_min = 0.0        
      c%frost_kill = 0.0          
      c%spla_slope = 0.0          
      c%sen_light_time_const = 0.0
      c%sen_water_time_const = 0.0
      c%sen_threshold = 0.0       
      c%sen_radn_crit = 0.0       
      c%sen_rate_water = 0.0      
      c%sen_light_slope  = 0.0    
      c%num_temp_senescence  = 0
      c%grn_water_cont = 0.0            
      c%frac_stem2flower = 0.0          
      c%partition_rate_leaf = 0.0       
      c%stem_trans_frac = 0.0           
      c%leaf_trans_frac = 0.0           
      c%htstress_coeff = 0.0            
      c%temp_grain_crit_stress = 0.0    
      c%leaf_no_dead_const = 0.0        
      c%leaf_no_dead_slope = 0.0        
      c%leaf_no_correction = 0.0  
      c%x0_const = 0.0            
      c%x0_slope = 0.0            
      c%y0_const = 0.0            
      c%y0_slope = 0.0            
      c%a_const = 0.0             
      c%a_slope1 = 0.0            
      c%a_slope2 = 0.0            
      c%b_const  = 0.0            
      c%b_slope1 = 0.0            
      c%b_slope2 = 0.0            
      c%x_ave_temp(:) = 0.0  
      c%y_stress_photo(:) = 0.0
      c%x_temp(:) = 0.0     
      c%y_tt(:) = 0.0       
      c%x_weighted_temp(:) = 0.0
      c%y_plant_death(:) = 0.0  
      c%x_temp_grain(:) = 0.0 
      c%y_grain_rate(:) = 0.0 
      c%x_temp_other(:) = 0.0 
      c%y_tt_other(:) = 0.0   
      c%imin = 0.0                    
      c%iopt = 0.0                    
      c%imax = 0.0                    
      c%ioptr = 0.0                   
      c%amin = 0.0                    
      c%aopt = 0.0                    
      c%amax = 0.0                    
      c%aoptr = 0.0                   
      c%x_temp_photo(:) = 0.0 
      c%num_temp = 0             
      c%num_ave_temp = 0         
      c%num_temp_grain  = 0      
      c%num_factors = 0          
      c%num_temp_other = 0       
      c%num_weighted_temp = 0    
      c%num_temp_photo = 0       
      c%num_x_leaf_no = 0          
      c%num_x_lai    = 0           
      c%num_kvalue_rowspace  = 0   
      c%num_plant_rld    = 0
      c%num_node_no_app = 0
      c%num_fasw_emerg   = 0
      c%head_grain_no_max_ub  = 0.0     
      c%grain_gth_rate_ub = 0.0         
      c%tt_emerg_to_endjuv_ub = 0.0     
      c%pp_endjuv_to_init_ub = 0.0     
      c%tt_flower_to_maturity_ub  = 0.0 
      c%tt_maturity_to_ripe_ub = 0.0   
      c%tt_flower_to_start_grain_ub = 0.0  
      c%tt_flag_to_flower_ub = 0.0         
      c%ll_ub  = 0.0              
      c%kl_ub = 0.0               
      c%sw_dep_ub = 0.0           
      c%sw_dep_lb = 0.0           
      c%NO3_ub  = 0.0             
      c%NO3_lb  = 0.0             
      c%NO3_min_ub = 0.0          
      c%NO3_min_lb = 0.0          
      c%leaf_no_min = 0.0         
      c%leaf_no_max = 0.0         
      c%latitude_ub = 0.0         
      c%latitude_lb = 0.0         
      c%maxt_ub = 0.0             
      c%maxt_lb = 0.0             
      c%mint_ub = 0.0             
      c%mint_lb  = 0.0            
      c%radn_ub  = 0.0            
      c%radn_lb  = 0.0            
      c%dlayer_ub = 0.0           
      c%dlayer_lb = 0.0           
      c%dul_dep_ub  = 0.0         
      c%dul_dep_lb  = 0.0         
      c%grno_grate(:) = 0.0 
      c%grno_fract(:) = 0.0 
      c%x_leaf_no (:) = 0.0
      c%x_lai (:) = 0.0
      c%leaf_no_sla_max (:)  = 0.0
      c%leaf_no_sla_min (:) = 0.0
      c%y_lai_sla_max (:) = 0.0
      c%lai_sla_min (:)  = 0.0
      c%kvalue_rowspace (:) = 0.0
      c%kvalue_adjustment (:) = 0.0
      c%x_plant_rld (:) = 0.0
      c%y_rel_root_rate (:) = 0.0
      c%x_SLN_photo(:) = 0.0
      c%y_SLN_photo(:) = 0.0
      c%newleaf_SLN  = 0.0
      c%num_SLN_photo = 0
      c%tt_base = 0.0                
      c%tt_opt  = 0.0                
      c%n_supply_preference = ' '
      
      call pop_routine (my_name)
      return
      end
      
*     ===========================================================
      subroutine sorg_zero_variables ()
*     ===========================================================
      use SorgModule
      implicit none
      include 'data.pub'                          
      include 'error.pub'                         

*+  Purpose
*       Zero crop variables & arrays

*+  Changes
*     010994 sc   specified and programmed
*     090695 psc  add row spacing = 0

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sorg_zero_variables')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
          ! zero pools etc.
 
      call sorg_zero_daily_variables ()
 
      call fill_real_array (g%cnd_grain_conc, 0.0, max_stage)
      call fill_real_array (g%cnd_photo, 0.0, max_stage)
      call fill_real_array (g%cswd_expansion, 0.0, max_stage)
      call fill_real_array (g%cswd_pheno, 0.0, max_stage)
      call fill_real_array (g%cswd_photo, 0.0, max_stage)
      call fill_real_array (g%days_tot, 0.0, max_stage)
      call fill_real_array (g%dm_dead, 0.0, max_part)
      call fill_real_array (g%dm_green, 0.0, max_part)
      call fill_real_array (g%dm_plant_min, 0.0, max_part)
      call fill_real_array (g%dm_plant_top_tot, 0.0, max_stage)
      call fill_real_array (g%heat_stress_tt, 0.0, max_stage)
      call fill_real_array (g%leaf_area, 0.0, max_leaf)
      call fill_real_array (g%leaf_no, 0.0, max_stage)
      call fill_real_array (g%leaf_no_dead, 0.0, max_stage)
      call fill_real_array (p%ll_dep, 0.0, max_layer)
      call fill_real_array (g%N_conc_crit, 0.0, max_part)
      call fill_real_array (g%N_conc_min, 0.0, max_part)
      call fill_real_array (g%N_green, 0.0, max_part)
      call fill_real_array (g%phase_tt, 0.0, max_stage)
      call fill_real_array (g%tt_tot, 0.0, max_stage)
      call fill_real_array (g%tt_tot_fm, 0.0, max_stage)
      call fill_real_array (g%phase_tt_curv, 0.0, max_stage)
      call fill_real_array (g%tt_curv_tot, 0.0, max_stage)
      call fill_real_array (g%phase_tt_other, 0.0, max_stage)
      call fill_real_array (g%tt_other_tot, 0.0, max_stage)
      call fill_real_array (g%lai_equilib_light, 0.0, 366)
      call fill_real_array (g%lai_equilib_water, 0.0, 366)
      call fill_real_array (g%soil_temp, 0.0, 366)
 
      call fill_real_array (g%dm_senesced, 0.0, max_part)
      call fill_real_array (g%dm_stress_max, 0.0, max_stage)
      call fill_real_array (g%N_dead, 0.0, max_part)
      call fill_real_array (g%N_senesced, 0.0, max_part)
 
 
      g%num_layers = 0
      g%canopy_height = 0.0
      g%grain_no = 0.0
      g%flowering_date = 0
      g%maturity_date = 0
      g%flowering_das = 0
      g%maturity_das = 0
      g%leaf_no_final = 0.0
      g%lai_max = 0.0
cscc need to allow this to be read in if running GLH tiller model
c      g%tiller_no_fertile = 0.0
      g%tpla_today = 0.0
      g%tpla_yesterday = 0.0
      g%N_conc_act_stover_tot = 0.0
      g%N_conc_crit_stover_tot = 0.0
      g%N_demand_tot = 0.0
      g%N_uptake_grain_tot = 0.0
      g%N_uptake_stover_tot = 0.0
      g%N_uptake_tot = 0.0
      g%plants = 0.0
      g%root_depth = 0.0
      g%sowing_depth = 0.0
!cpsc
      g%row_spacing = 0.0
cjh
      g%cover_green = 0.0
      g%cover_sen   = 0.0
      g%cover_dead  = 0.0
!cpsc
      g%slai = 0.0
      g%lai = 0.0
      g%tlai_dead = 0.0
      g%transpiration_tot = 0.0
      g%previous_stage = 0.0
 
!slw zero stress factors
!scc These should be set to 1.0 (no stress, otherwise averaging is confusing!!!)
      g%swdef_expansion = 1.0
      g%swdef_photo = 1.0
      g%swdef_pheno = 1.0
      g%nfact_expansion = 1.0
      g%nfact_photo = 1.0
      g%temp_stress_photo = 1.0
      g%nfact_grain_conc = 1.0
      g%nfact_pheno = 1.0
 
! zero root length variables
      call fill_real_array (g%root_length, 0.0, max_layer)
      call fill_real_array (p%xf, 0.0, max_layer)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sorg_zero_daily_variables ()
*     ===========================================================
      use SorgModule
      implicit none
      include 'data.pub'                          
      include 'error.pub'                         

*+  Purpose
*       Zero crop daily variables & arrays

*+  Changes
*     010994 sc   specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sorg_zero_daily_variables')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
          ! zero pools etc.
 
      call fill_real_array (g%dlt_dm_green, 0.0, max_part)
      call fill_real_array (g%dlt_dm_green_retrans, 0.0, max_part)
      call fill_real_array (g%dlt_dm_sen_retrans, 0.0, max_part)
      call fill_real_array (g%dlt_N_green, 0.0, max_part)
      call fill_real_array (g%dlt_N_retrans, 0.0, max_part)
      call fill_real_array (g%dlt_NO3gsm, 0.0, max_layer)
      call fill_real_array (g%dlt_sw_dep, 0.0, max_layer)
      call fill_real_array (g%dm_green_demand, 0.0, max_part)
cnh      call fill_real_array (g%N_demand, 0.0, max_part)
 
      call fill_real_array (g%dlt_dm_dead_detached, 0.0, max_part)
      call fill_real_array (g%dlt_dm_detached, 0.0, max_part)
      call fill_real_array (g%dlt_dm_senesced, 0.0, max_part)
      call fill_real_array (g%dlt_N_dead_detached, 0.0, max_part)
      call fill_real_array (g%dlt_N_detached, 0.0, max_part)
      call fill_real_array (g%dlt_N_senesced, 0.0, max_part)
      call fill_real_array (g%sw_avail, 0.0, max_layer)
      call fill_real_array (g%sw_avail_pot, 0.0, max_layer)
      call fill_real_array (g%sw_supply, 0.0, max_layer)
 
      call fill_real_array (g%uptake_water, 0.0, max_layer)
      call fill_real_array (g%uptake_no3, 0.0, max_layer)
      g%num_uptake_water = 0
      g%num_uptake_no3 = 0
 
      g%dlt_tlai_dead_detached = 0.0
      g%dlt_slai_detached = 0.0
      g%dlt_canopy_height = 0.0
      g%dlt_dm = 0.0
      g%dlt_dm_grain_demand = 0.0
      g%dlt_dm_stress_max = 0.0
      g%dlt_heat_stress_tt = 0.0
      g%dlt_leaf_no = 0.0
      g%dlt_leaf_no_dead = 0.0
c scc following are part of glh tiller routines
c      g%dlt_tiller_no =0.0
c      g%dlt_stiller_no = 0.0
      g%dlt_plants = 0.0
      g%dlt_root_depth = 0.0
      g%dlt_slai = 0.0
      g%dlt_stage = 0.0
      g%dlt_lai = 0.0
      g%dlt_tt = 0.0
      g%dlt_tt_fm = 0.0
      g%dlt_tt_curv = 0.0
      g%dlt_tt_other = 0.0

c      g%sw_demand = 0.0
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sorg_init ()
*     ===========================================================
      use SorgModule
      implicit none
      include 'error.pub'                         

*+  Purpose
*       Crop initialisation

*+  Changes
*     010994 sc   specified and programmed
*      060599 sdb removed version reference

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sorg_init')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      call Write_string (' Initialising')
 
           ! initialize crop variables
 
      call sorg_read_constants ()
 
      g%current_stage = real (plant_end)
      g%plant_status = status_out
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sorg_start_crop ()
*     ===========================================================
      use SorgModule
      implicit none
      include   'const.inc'            ! lu_scr_sum, blank
      include 'intrface.pub'                      
      include 'error.pub'                         

*+  Purpose
*       Start crop using parameters specified in passed record

*+  Changes
*     010994 sc   specified and programmed
*     090695 psc  add row spacing read
*     220896 jngh changed extract to collect
*                 removed data_record from argument

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sorg_start_crop')

*+  Local Variables
      character  cultivar*20           ! name of cultivar
      integer    numvals               ! number of values found in array
      character  string*200            ! output string

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      call Write_string ( 'Sowing initiate')
 
cjh      if (data_record.ne.blank) then
 
         call collect_real_var ('plants', '()'
     :                        , g%plants, numvals, 0.0, 100.0)
!cpsc
         call collect_real_var_optional (
     :                          'row_spacing', '(m)'
     :                        , g%row_spacing, numvals
     :                        , 0.0, 2.0)
 
         call collect_real_var (
     :                          'sowing_depth', '(mm)'
     :                        , g%sowing_depth, numvals
     :                        , 0.0, 100.0)
 
         call collect_char_var ('cultivar', '()'
     :                        , cultivar, numvals)
 
!scc added FTN 11/10/95
         call collect_real_var (
     :                      'tiller_no_fertile', '()'
     :                    , g%tiller_no_fertile, numvals
     :                    , 0.0, 10.0)
 
         if (numvals.eq.0) then
            g%tiller_no_fertile = 0.0
         else
         endif
             ! report
 
         call write_string (new_line//new_line)
 
         string = '                 Crop Sowing Data'
         call write_string (string)
 
         string = '    ------------------------------------------------'
         call write_string (string)
!cpsc
         call write_string (
     :           '    Sowing  Depth Plants Spacing Cultivar    FTN')
!cpsc
         call write_string (
     :           '    Day no   mm     m^2     m     Name       no')
 
         string = '    ------------------------------------------------'
         call write_string (string)
!cpsc /scc
         write (string, '(3x, i7, 3f7.1, 1x, a10,1x,f7.2)')
     :                   g%day_of_year, g%sowing_depth
     :                 , g%plants, g%row_spacing, cultivar
     :                 , g%tiller_no_fertile
         call write_string (string)
 
         string = '    ------------------------------------------------'
         call write_string (string)
 
                 ! get cultivar parameters
 
         call sorg_read_cultivar_params (cultivar)
 
                 ! get root profile parameters
 
         call sorg_read_root_params ()
 
         g%current_stage = real (sowing)
         g%plant_status = status_alive
 
cjh      else
            ! report empty sowing record
cjh         call fatal_error (err_user, 'No sowing criteria supplied')
cjh      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sorg_read_cultivar_params (cultivar)
*     ===========================================================
      use SorgModule
      implicit none
      include   'const.inc'            ! new_line, lu_scr_sum, blank
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
      end



*     ===========================================================
      subroutine sorg_read_root_params ()
*     ===========================================================
      use SorgModule
      implicit none
      include   'const.inc'            ! new_line, lu_scr_sum, blank,
      include 'data.pub'                          
      include 'read.pub'                          
      include 'error.pub'                         

*+  Purpose
*       Get root profile parameters

*+  Changes
*       090994 sc   specified and programmed
*     210395 jngh changed from Maize_section to a parameters section

*+  Calls
                                       ! lu_scr_sum

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sorg_read_root_params')
*
      character  section_name*(*)
      parameter (section_name = 'parameters')

*+  Local Variables
      integer    layer                 ! layer number
      real       ll (max_layer)        ! lower limit of plant-extractable
                                       ! soil water for soil layer l
                                       ! (mm water/mm soil)
      integer    num_layers            ! number of layers in profile
      integer    numvals
      character  string*200            ! output string

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      call write_string (
     :                  new_line
     :                  //'   - Reading root profile parameters')
 
         !       Maize_sw_supply
 
      call read_char_var_optional (section_name
     :                     , 'uptake_source', '()'
     :                     , p%uptake_source, numvals)
      if (numvals.eq.0) then
         p%uptake_source = 'calc'
      else
      endif
 
      call read_real_array (section_name
     :                     , 'll', max_layer, '()'
     :                     , ll, num_layers
     :                     , 0.0, c%ll_ub)
 
      call fill_real_array (p%ll_dep, 0.0, max_layer)
      do 1000 layer = 1, num_layers
         p%ll_dep(layer) = ll(layer)*g%dlayer(layer)
1000  continue
 
      call read_real_array (section_name
     :                     , 'kl', max_layer, '()'
     :                     , p%kl, num_layers
     :                     , 0.0, c%kl_ub)
 
      call read_real_array (section_name
     :                     , 'xf', max_layer, '()'
     :                     , p%xf, num_layers
     :                     , 0.0, 1.0)
 
      if (p%uptake_source.eq.'apsim') then
         string = 'Uptake of NO3 and water calculated by'
     :            //' another APSIM module'
 
      elseif (p%uptake_source.eq.'calc') then
         string = 'Uptake of NO3 and water calculated by '
     :            //c%crop_type
 
      else
         string = blank
 
      endif
 
      call write_string (string)
      call write_string (blank)
 
          ! report
      call write_string (new_line//new_line)
 
      write (string,'(4x, a)') '                Root Profile'
      call write_string (string)
 
      string = '    ------------------------------------------------'
      call write_string (string)
 
      string = '      Layer      Kl      Lower Exploration'
      call write_string (string)
      string = '      Depth              limit   Factor'
      call write_string (string)
 
      string = '      (mm)       ()     (mm/mm)    ()'
      call write_string (string)
 
      string = '    ------------------------------------------------'
      call write_string (string)
 
      do 2000 layer = 1, num_layers
         write (string,'(3x, 4f9.3)')
     :            g%dlayer(layer)
     :          , p%kl(layer)
     :          , ll(layer)
     :          , p%xf(layer)
         call write_string (string)
2000  continue
 
      string = '     ------------------------------------------------'
      call write_string (string)
 
      call write_string (new_line//new_line)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sorg_end_crop ()
*     ===========================================================
      use SorgModule
      implicit none
      include   'const.inc'            ! new_line, lu_scr_sum
      include   'convert.inc'          ! gm2kg, sm2ha, mm2cm, cmm2cc
      include 'data.pub'                          
      include 'error.pub'                         

*+  Purpose
*       End crop

*+  Changes
*       290994 sc   specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sorg_end_crop')

*+  Local Variables
      real       dm_residue            ! dry matter added to residue (g/m^2)
      real       N_residue             ! nitrogen added to residue (g/m^2)
      real       dm_root               ! dry matter added to soil (g/m^2)
      real       N_root                ! nitrogen added to soil (g/m^2)
      character  string*400            ! output string
      real       yield                 ! grain wt (kg/ha)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (g%plant_status.ne.status_out) then
         g%plant_status = status_out
         g%current_stage = real (plant_end)
 
cnh inserted options here
         call sorg_harvest_root_incorp(2) ! 1 uses exp function for rtdp
                                           ! 2 uses calc root length dist
         call sorg_harvest_top_residue(1)
 
                ! report
 
         yield = (g%dm_green(grain) + g%dm_dead(grain)) *gm2kg /sm2ha
         write (string, '(3x, a, f7.1)')
     :                  ' ended. Yield (dw) = ', yield
         call Write_string (string)
 
             ! now do post harvest processes
 
         dm_root = g%dm_green(root)
     :           + g%dm_dead(root)
     :           + g%dm_senesced(root)
 
         N_root  = g%N_green(root)
     :           + g%N_dead(root)
     :           + g%N_senesced(root)
 
 
             ! put stover into surface residue
 
         dm_residue = (sum_real_array (g%dm_green, max_part)
     :              - g%dm_green(root) - g%dm_green(grain))
 
     :              + (sum_real_array (g%dm_senesced, max_part)
     :              - g%dm_senesced(root) - g%dm_senesced(grain))
 
     :              + (sum_real_array (g%dm_dead, max_part)
     :              - g%dm_dead(root) - g%dm_dead(grain))
 
         N_residue = (sum_real_array (g%N_green, max_part)
     :             - g%N_green(root) - g%N_green(grain))
 
     :             + (sum_real_array (g%N_senesced, max_part)
     :             - g%N_senesced(root) - g%N_senesced(grain))
 
     :             + (sum_real_array (g%N_dead, max_part)
     :             - g%N_dead(root) - g%N_dead(grain))
 
         write (string, '(40x, a, f7.1, a, 3(a, 40x, a, f6.1, a))')
     :                  '  straw residue ='
     :                  , dm_residue * gm2kg /sm2ha, ' kg/ha'
     :                  , new_line
     :                  , '  straw N = '
     :                  , N_residue * gm2kg /sm2ha, ' kg/ha'
 
     :                  , new_line
     :                  , '  root residue = '
     :                  , dm_root * gm2kg /sm2ha, ' kg/ha'
     :                  , new_line
     :                  , '  root N = '
     :                  , N_root * gm2kg /sm2ha, ' kg/ha'
 
         call write_string (string)
 
      else
      endif
 
      call pop_routine (my_name)
      return
      end



*     ================================================================
      subroutine sorg_get_other_variables ()
*     ================================================================
      use SorgModule
      implicit none
      include   'const.inc'
      include   'convert.inc'
      include 'data.pub'                          
      include 'crp_root.pub'                      
      include 'crp_util.pub'                      
      include 'intrface.pub'                      
      include 'error.pub'                         

*+  Purpose
*      Get the values of variables/arrays from other modules.

*+  Assumptions
*      assumes variable has the following format
*         <variable_name> = <variable_value/s> (<units>)

*+  Changes
*     010994 sc   specified and programmed
*     140896 jngh modified fr_intc_radn name to inclued a suffix of module name
*     020998 sb used min_year and max_year intstead of c%year_lb and c%year_ub.

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sorg_get_other_variables')

*+  Local Variables
      integer    layer                 ! layer number
      integer    numvals               ! number of values put into array
      real       dlayer(max_layer)     ! soil layer depths (mm)
      real       NO3(max_layer)        ! soil NO3 content (kg/ha)
      real       NO3_min(max_layer)    ! soil NO3 minimum (kg/ha)
      character  module_name*(Max_module_name_size) ! module name
      real       soil_temp             ! soil surface temperature (oC)
      real       profile_depth         ! depth of soil profile (mm)
      real       root_depth_new        ! new root depth (mm)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
            ! date
 
      call get_integer_var (unknown_module, 'day', '()'
     :                                    , g%day_of_year, numvals
     :                                    , 1, 366)
 
      call get_integer_var (unknown_module, 'year', '()'
     :                                    , g%year, numvals
     :                                    , min_year, max_year)
 
                                ! climate
      call get_real_var (unknown_module, 'latitude', '(oL)'
     :                                  , g%latitude, numvals
     :                                  , c%latitude_lb, c%latitude_ub)
 
      call get_real_var (unknown_module, 'maxt', '(oC)'
     :                                  , g%maxt, numvals
     :                                  , c%maxt_lb, c%maxt_ub)
 
      call get_real_var (unknown_module, 'mint', '(oC)'
     :                                  , g%mint, numvals
     :                                  , c%mint_lb, c%mint_ub)
 
      call get_real_var (unknown_module, 'radn', '(Mj/m^2)'
     :                                  , g%radn, numvals
     :                                  , c%radn_lb, c%radn_ub)
!mjr eo read in to limit demand in crop routines
 
      call get_real_var_optional (unknown_module, 'eo', '(mm)'
     :                                  , g%eo, numvals
     :                                  , 0.0, 20.0)
 
!cpsc
      call get_real_var_optional (unknown_module, 'soil_temp', '(oC)'
     :                                  , soil_temp, numvals
     :                                  , 0.0, 80.0)
 
      if (numvals.eq.0) then
         ! soil temp not supplied
      else
         call crop_store_value (
     .          g%day_of_year,
     .          g%year,
     .          g%soil_temp, soil_temp)
      endif
 
                               ! canopy
      call get_current_module (module_name)
      call get_real_var_optional (unknown_module
     :                           , 'fr_intc_radn_'//module_name
     :                           , '()'
     :                           , g%fr_intc_radn
     :                           , numvals
     :                           , 0.0
     :                           , 1.0)
 
c+!!!!!!!! what to do if no waterbalance variables found
            ! soil profile and soil water
 
 
 
      call get_real_array (unknown_module, 'dlayer', max_layer
     :                                    , '(mm)'
     :                                    , dlayer, numvals
     :                                    , c%dlayer_lb, c%dlayer_ub)
 
 
 
 
      if (g%num_layers.eq.0) then
            ! we assume dlayer hasn't been initialised yet.
cnh         call add_real_array (dlayer, g%dlayer, numvals)
         do 100 layer = 1, numvals
            g%dlayer(layer) = dlayer(layer)
  100    continue
         g%num_layers = numvals
 
 
 
      else
            ! dlayer may be changed from its last setting
 
        profile_depth = sum_real_array (dlayer, numvals)
 
        if (g%root_depth.gt.profile_depth) then
           root_depth_new = profile_depth
           call Crop_root_redistribute
     :                        ( g%root_length
     :                        , g%root_depth
     :                        , g%dlayer
     :                        , g%num_layers
     :                        , root_depth_new
     :                        , dlayer
     :                        , numvals)
 
           g%root_depth = root_depth_new
        else
           ! roots are ok.
      endif
 
         do 1000 layer = 1, numvals
            p%ll_dep(layer) = divide (p%ll_dep(layer)
     :                              , g%dlayer(layer), 0.0)
     :                      * dlayer(layer)
 
            g%dlayer(layer) = dlayer(layer)
1000     continue
         g%num_layers = numvals
      endif
 
      call get_real_array (unknown_module, 'dul_dep', max_layer
     :                                    , '(mm)'
     :                                    , g%dul_dep, numvals
     :                                    , c%dul_dep_lb, c%dul_dep_ub)
 
      call get_real_array (unknown_module, 'sw_dep', max_layer
     :                                    , '(mm)'
     :                                    , g%sw_dep, numvals
     :                                    , c%sw_dep_lb, c%sw_dep_ub)
 
                                ! soil nitrogen pools
      call get_real_array_optional (unknown_module, 'no3', max_layer
     :                                  ,  '(kg/ha)'
     :                                  , NO3, numvals
     :                                  , c%NO3_lb, c%NO3_ub)
      if (numvals.eq.0) then
            ! we have no N supply - make non-limiting.
         call fill_real_array (NO3, 10000.0, g%num_layers)
      else
      endif
      do 2000 layer = 1, g%num_layers
         g%NO3gsm(layer) = NO3(layer) * kg2gm /ha2sm
2000  continue
 
 
      call get_real_array_optional (unknown_module, 'no3_min',max_layer
     :                                  ,  '(kg/ha)'
     :                                  , NO3_min, numvals
     :                                  , c%NO3_min_lb, c%NO3_min_ub)
      do 3000 layer = 1, g%num_layers
         g%NO3gsm_min(layer) = NO3_min(layer) * kg2gm /ha2sm
3000  continue
 
      call pop_routine (my_name)
      return
      end



*     ================================================================
      subroutine sorg_set_other_variables ()
*     ================================================================
      use SorgModule
      implicit none
      include   'const.inc'
      include   'convert.inc'
      include 'action.inc'
      include 'data.pub'                          
      include 'intrface.pub'                      
      include 'error.pub'     
      include 'postbox.pub'                    

*+  Purpose
*      Set the value of a variable or array in other module/s.

*+  Notes
*      a flag is set if any of the totals is requested.  The totals are
*      reset during the next process phase when this happens.

*+  Changes
*     010994 sc   specified and programmed
*     220896 jngh changed set_ to post_ construct

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Sorg_set_other_variables')

*+  Local Variables
      real       dlt_NO3(max_layer)    ! soil NO3 change (kg/ha)
      integer    layer                 ! soil layer no.
      integer    num_layers            ! number of layers

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      call sorg_update_other_variables()
 
      if (p%uptake_source.eq.'calc') then
c+!!!! perhaps we should get number of layers at init and keep it
         num_layers = count_of_real_vals (g%dlayer, max_layer)
 
         do 1000 layer = 1, num_layers
            dlt_NO3(layer) = g%dlt_NO3gsm(layer) * gm2kg /sm2ha
1000     continue
 
         call new_postbox()
         call post_real_array ('dlt_no3', '(kg/ha)'
     :                       , dlt_NO3, num_layers)
 
         call post_real_array ('dlt_sw_dep', '(mm)'
     :                       , g%dlt_sw_dep, num_layers)
 
         call Action_send (unknown_module
     :                               ,ACTION_set_variable
     :                               ,'dlt_no3')
         call Action_send (unknown_module
     :                               ,ACTION_set_variable
     :                               ,'dlt_sw_dep')
         call delete_postbox()
 
      else
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sorg_update_other_variables()
*     ===========================================================
      use SorgModule
      implicit none
      include   'convert.inc'
      include 'error.pub'                         

*+  Purpose
*       Update other module states

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sorg_update_other_variables')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      call sorg_daily_top_residue (1)
      call sorg_daily_root_incorp(2)
 
      call pop_routine (my_name)
      return
      end



*     ===============================================================
      subroutine sorg_set_my_variable (Variable_name)
*     ===============================================================
      implicit none
      include 'error.pub'

*+  Sub-Program Arguments
      character  Variable_name*(*)     ! (INPUT) Variable name to search for

*+  Purpose
*      Set a variable in this module as requested by another.

*+  Changes
*      290393 sc
*      220896 jngh  added call to message_unused
*                   changed respond2set to collect

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sorg_set_my_variable')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
             ! **** Repeat for each variable
 
*      if (variable_name .eq. '????') then
*         call collect_real_array (variable_name, '()', max_layer
*     :                               , ????, numvals
*     :                               , 0.0, 1.0)
 
*      else
            ! Don't know this variable name
         call Message_unused ()
*      endif
 
 
      call pop_routine (my_name)
      return
      end



*     ================================================================
      subroutine sorg_send_my_variable (variable_name)
*     ================================================================
      use SorgModule
      implicit none
      include   'convert.inc'          ! gm2kg, sm2ha, mm2cm, cmm2cc, sm2smm
      include 'science.pub'                       
      include 'data.pub'                          
      include 'intrface.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      character variable_name*(*)      ! (INPUT) variable name to search for

*+  Purpose
*      Return the value of a variable requested by other modules.

*+  Changes
*      string_concat
*      090495 psc  added nfact to output list
*      170495 psc  added grain_size, yield, biomass to output list
*      220896 jngh  added call to message_unused

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sorg_send_my_variable')

*+  Local Variables
      real       apt_N_up              ! N uptake by stover (kg/ha)
      real       cover_tot             ! total crop cover fraction (0-1)
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      real       grain_N_pcnt          ! grain N concentration percent (%)
      real       lai_sum               ! leaf area index of all leaf material
                                       ! live + dead
      integer    num_layers            ! number of layers in profile
      integer    stage_no              ! current stage no.
      real       NO3gsm_tot            ! total NO3 in the root profile (g/m^2)
      real       N_demand              ! sum N demand for plant parts (g/m^2)
      real       N_uptake_sum          ! N supply from soil
      real       grain_size            ! individual grain wt (g/grain)
      real       yield                 ! grain yield (kg/ha)
      real       biomass               ! total above-ground biomass (kg/ha)
      real       biomass_n             ! total above-ground biomass N (kg/ha)
      real       sw_supply_sum         ! total supply over profile (mm)
      integer    layer                 ! soil layer
      real       esw_layr(max_layer)   ! plant extractable soil water
      real       rlv(max_layer)
      real       n_conc
      real       hi                    ! harvest index (yield/biomass)
      real       sw_deficit(max_layer) ! Soil water deficit below dul_dep (mm)
      integer    i
      real       stover

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
 
            ! management
 
cjh
      if (variable_name .eq. 'plant_status') then
         call respond2get_char_var (variable_name
     :                             , '()'
     :                             , g%plant_status)
 
      elseif (variable_name .eq. 'dlt_stage') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%dlt_stage)
 
      elseif (variable_name .eq. 'stage') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%current_stage)
 
      elseif (variable_name .eq. 'stage_code') then
         if (g%plant_status.ne.status_out) then
            stage_no = int (g%current_stage)
            call respond2get_real_var (variable_name
     :                             , '()'
     :                             , c%stage_code_list(stage_no))
         else
            call respond2get_real_var (variable_name
     :                             , '()'
     :                             , 0.0)
         endif
      elseif (variable_name .eq. 'stage_name') then
         if (g%plant_status.ne.status_out) then
            stage_no = int (g%current_stage)
            call respond2get_char_var (variable_name
     :                             , '()'
     :                             , c%stage_names(stage_no))
         else
            call respond2get_char_var (variable_name
     :                             , '()'
     :                             , status_out)
         endif
      elseif (variable_name .eq. 'crop_type') then
         call respond2get_char_var (variable_name
     :                             , '()'
     :                             , c%crop_type)
 
      elseif (variable_name .eq. 'dlt_tt') then
         call respond2get_real_var (variable_name
     :                             , '(oCd)'
     :                             , g%dlt_tt)
 
      elseif (variable_name .eq. 'dlt_tt_fm') then
         call respond2get_real_var (variable_name
     :                             , '(oCd)'
     :                             , g%dlt_tt_fm)
 
      elseif (variable_name .eq. 'phase_tt') then
         call respond2get_real_array (variable_name
     :                             , '(oC)'
     :                             , g%phase_tt
     :                             , max_stage)
 
      elseif (variable_name .eq. 'tt_tot') then
         call respond2get_real_array (variable_name
     :                             , '(oC)'
     :                             , g%tt_tot
     :                             , max_stage)
 
      elseif (variable_name .eq. 'tt_tot_fm') then
         call respond2get_real_array (variable_name
     :                             , '(oC)'
     :                             , g%tt_tot_fm
     :                             , max_stage)
 
      elseif (variable_name .eq. 'days_tot') then
         call respond2get_real_array (variable_name
     :                             , '()'
     :                             , g%days_tot
     :                             , max_stage)
 
      elseif (variable_name .eq. 'flowering_date') then
         call respond2get_integer_var (variable_name
     :                             , '(doy)'
     :                             , g%flowering_date)
 
      elseif (variable_name .eq. 'maturity_date') then
         call respond2get_integer_var (variable_name
     :                             , '(doy)'
     :                             , g%maturity_date)
 
      elseif (variable_name .eq. 'flowering_das') then
         call respond2get_integer_var (variable_name
     :                             , '(days)'
     :                             , g%flowering_das)
 
      elseif (variable_name .eq. 'maturity_das') then
         call respond2get_integer_var (variable_name
     :                             , '(days)'
     :                             , g%maturity_das)
 
 
      elseif (variable_name .eq. 'leaf_no') then
         call respond2get_real_array (variable_name
     :                              , '()'
     :                              , g%leaf_no
     :                              , max_stage)
 
      elseif (variable_name .eq. 'dlt_leaf_no') then
         call respond2get_real_var (variable_name
     :                              , '()'
     :                              , g%dlt_leaf_no
     :                              )
 
      elseif (variable_name .eq. 'leaf_no_dead') then
         call respond2get_real_array (variable_name
     :                              , '()'
     :                              , g%leaf_no_dead
     :                              , max_stage)
 
      elseif (variable_name .eq. 'leaf_area') then
         call respond2get_real_array (variable_name
     :                              , '()'
     :                              , g%leaf_area
     :                              , max_leaf)
 
      elseif (variable_name .eq. 'height') then
         call respond2get_real_var (variable_name
     :                             , '(mm)'
     :                             , g%canopy_height)
 
      elseif (variable_name .eq. 'root_depth') then
         call respond2get_real_var (variable_name
     :                             , '(mm)'
     :                             , g%root_depth)
 
      elseif (variable_name .eq. 'plants') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%plants)
 
      elseif (variable_name .eq. 'grain_no') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%grain_no)
 
      elseif (variable_name .eq. 'grain_size') then
         grain_size = divide (g%dm_green(grain) + g%dm_dead(grain)
     :                 , g%grain_no, 0.0)
         call respond2get_real_var (variable_name
     :                             , '(g)'
     :                             , grain_size)
 
      elseif (variable_name .eq. 'cover_green') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%cover_green)
 
      elseif (variable_name .eq. 'cover_tot') then
         cover_tot = 1.0
     :             - (1.0 - g%cover_green)
     :             * (1.0 - g%cover_sen)
     :             * (1.0 - g%cover_dead)
 
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , cover_tot)
 
      elseif (variable_name .eq. 'lai_sum') then
         lai_sum = g%lai + g%slai + g%tlai_dead
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , lai_sum)
 
      elseif (variable_name .eq. 'tlai') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%lai + g%slai)
 
      elseif (variable_name .eq. 'slai') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%slai)
 
      elseif (variable_name .eq. 'lai') then
         call respond2get_real_var (variable_name
     :                             , '(m^2/m^2)'
     :                             , g%lai)
 
      elseif (variable_name .eq. 'tlai_dead') then
         call respond2get_real_var (variable_name
     :                             , '(m^2/m^2)'
     :                             , g%tlai_dead)
 
      elseif (variable_name .eq. 'tiller_no_fertile') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%tiller_no_fertile)
 
      elseif (variable_name .eq. 'sla') then
         call respond2get_real_var (variable_name
     :                             , '(mm2/g)'
     :                             , divide(g%lai*sm2smm
     :                             , g%dm_green(leaf), 0.0))
 
            ! plant biomass
 
      elseif (variable_name .eq. 'root_wt') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%dm_green(root))
 
      elseif (variable_name .eq. 'leaf_wt') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%dm_green(leaf))
 
      elseif (variable_name .eq. 'stem_wt') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%dm_green(stem))
 
      elseif (variable_name .eq. 'flower_wt') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%dm_green(flower))
 
      elseif (variable_name .eq. 'stem+flower_wt') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%dm_green(stem)
     :                               +g%dm_green(flower))
 
      elseif (variable_name .eq. 'grain_wt') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%dm_green(grain))
 
      elseif (variable_name .eq. 'dm_green') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g%dm_green
     :                             , max_part)
 
      elseif (variable_name .eq. 'dm_senesced') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g%dm_senesced
     :                             , max_part)
 
      elseif (variable_name .eq. 'dm_dead') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g%dm_dead
     :                             , max_part)
 
      elseif (variable_name .eq. 'yield') then
         yield = (g%dm_green(grain) + g%dm_dead(grain))
     :           * gm2kg / sm2ha
         call respond2get_real_var (variable_name
     :                             , '(kg/ha)'
     :                             , yield)
 
!NEED to DECIDE if biomass is to be g/m2 or kg/ha!!!!
      elseif (variable_name .eq. 'biomass') then
         biomass = (sum_real_array (g%dm_green, max_part)
     :           - g%dm_green(root)
     :           + sum_real_array (g%dm_senesced, max_part)
     :           - g%dm_senesced(root)
     :           + sum_real_array (g%dm_dead, max_part)
     :           - g%dm_dead(root))
     :           * gm2kg / sm2ha
 
         call respond2get_real_var (variable_name
     :                             , '(kg/ha)'
     :                             , biomass)
 
!scc 10/95 output harvest index
      elseif (variable_name .eq. 'hi') then
         yield = (g%dm_green(grain) + g%dm_dead(grain))
     :           * gm2kg / sm2ha
         biomass = (sum_real_array (g%dm_green, max_part)
     :           - g%dm_green(root)
     :           + sum_real_array (g%dm_senesced, max_part)
     :           - g%dm_senesced(root)
     :           + sum_real_array (g%dm_dead, max_part)
     :           - g%dm_dead(root))
     :           * gm2kg / sm2ha
         hi = divide(yield, biomass, 0.0)
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , hi)
 
!mjr 05/97 output stover (kg/ha)
      elseif (variable_name .eq. 'stover') then
         yield = (g%dm_green(grain) + g%dm_dead(grain))
     :           * gm2kg / sm2ha
         biomass = (sum_real_array (g%dm_green, max_part)
     :           - g%dm_green(root)
     :           + sum_real_array (g%dm_senesced, max_part)
     :           - g%dm_senesced(root)
     :           + sum_real_array (g%dm_dead, max_part)
     :           - g%dm_dead(root))
     :           * gm2kg / sm2ha
         stover = biomass - yield
         call respond2get_real_var (variable_name
     :                             , '(kg/ha)'
     :                             , stover)
 
      elseif (variable_name .eq. 'green_biomass') then
         biomass = (sum_real_array (g%dm_green, max_part)
     :                 - g%dm_green(root))
     :           * gm2kg / sm2ha
 
         call respond2get_real_var (variable_name
     :                             , '(kg/ha)'
     :                             , biomass)
 
 
      elseif (variable_name .eq. 'biomass_wt') then
         biomass = (sum_real_array (g%dm_green, max_part)
     :           - g%dm_green(root)
     :           + sum_real_array (g%dm_senesced, max_part)
     :           - g%dm_senesced(root)
     :           + sum_real_array (g%dm_dead, max_part)
     :           - g%dm_dead(root))
 
         call respond2get_real_var (variable_name
     :                             , '(g/m2)'
     :                             , biomass)
 
      elseif (variable_name .eq. 'green_biomass_wt') then
         biomass = sum_real_array (g%dm_green, max_part)
     :           - g%dm_green(root)
 
         call respond2get_real_var (variable_name
     :                             , '(g/m2)'
     :                             , biomass)
 
      elseif (variable_name .eq. 'stover_wt') then
         yield = (g%dm_green(grain) + g%dm_dead(grain))
         biomass = (sum_real_array (g%dm_green, max_part)
     :           - g%dm_green(root)
     :           + sum_real_array (g%dm_senesced, max_part)
     :           - g%dm_senesced(root)
     :           + sum_real_array (g%dm_dead, max_part)
     :           - g%dm_dead(root))
         stover = biomass - yield
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , stover)
 
      elseif (variable_name .eq. 'dlt_dm') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%dlt_dm)
 
      elseif (variable_name .eq. 'dlt_dm_green') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g%dlt_dm_green
     :                             , max_part)
 
      elseif (variable_name .eq. 'dlt_dm_green_retrans') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g%dlt_dm_green_retrans
     :                             , max_part)
 
      elseif (variable_name .eq. 'dlt_dm_detached') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g%dlt_dm_detached
     :                             , max_part)
 
      elseif (variable_name .eq. 'dlt_dm_dead_detached') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g%dlt_dm_dead_detached
     :                             , max_part)
 
      elseif ((variable_name .eq. 'biomass_n')
     :                       .or.
     :       (variable_name .eq. 'n_uptake')) then
         biomass_n = (sum_real_array (g%n_green, max_part)
     :             - g%n_green(root)
     :             + sum_real_array (g%n_senesced, max_part)
     :             - g%n_senesced(root)
     :             + sum_real_array (g%n_dead, max_part)
     :             - g%n_dead(root))
 
         call respond2get_real_var (variable_name
     :                             , '(g/m2)'
     :                             , biomass_n)
 
      elseif (variable_name .eq. 'green_biomass_n') then
         biomass_n = (sum_real_array (g%n_green, max_part)
     :                 - g%n_green(root))
 
         call respond2get_real_var (variable_name
     :                             , '(g/m2)'
     :                             , biomass_n)
 
      elseif (variable_name .eq. 'n_green') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g%N_green
     :                             , max_part)
 
      elseif (variable_name .eq. 'n_senesced') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g%N_senesced
     :                             , max_part)
 
      elseif (variable_name .eq. 'n_dead') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g%N_dead
     :                             , max_part)
 
      elseif (variable_name .eq. 'dlt_n_green') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g%dlt_N_green
     :                             , max_part)
 
      elseif (variable_name .eq. 'dlt_n_retrans') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g%dlt_N_retrans
     :                             , max_part)
 
      elseif (variable_name .eq. 'dlt_n_detached') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g%dlt_N_detached
     :                             , max_part)
 
      elseif (variable_name .eq. 'dlt_n_dead_detached') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g%dlt_N_dead_detached
     :                             , max_part)
 
      elseif (variable_name .eq. 'swdef_pheno') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%swdef_pheno)
 
      elseif (variable_name .eq. 'swdef_photo') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%swdef_photo)
 
      elseif (variable_name .eq. 'swdef_expan') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%swdef_expansion)
 
      elseif (variable_name .eq. 'ep') then
         num_layers = count_of_real_vals (g%dlayer, max_layer)
         call respond2get_real_array (variable_name
     :                               , '(mm)'
     :                               , g%dlt_sw_dep
     :                               , num_layers)
 
      elseif (variable_name .eq. 'cep') then
         call respond2get_real_var (variable_name
     :                             , '(mm)'
     :                             , - g%transpiration_tot)
 
      elseif (variable_name .eq. 'sw_supply') then
         deepest_layer = find_layer_no (g%root_depth, g%dlayer
     :                                , max_layer)
         sw_supply_sum = sum_real_array (g%sw_supply, deepest_layer)
         call respond2get_real_var (variable_name
     :                             , '(mm)'
     :                             , sw_supply_sum)
 
      elseif (variable_name .eq. 'esw_layr') then
 
         num_layers = count_of_real_vals (g%dlayer, max_layer)
         do 1000 layer = 1, num_layers
            esw_layr(layer) = l_bound (g%sw_dep(layer) - p%ll_dep(layer)
     :                        , 0.0)
1000     continue
         call respond2get_real_array (variable_name
     :                               , '(mm)'
     :                               , esw_layr
     :                               , num_layers)
 
      elseif (variable_name .eq. 'das') then
         call respond2get_integer_var (variable_name
     :                             , '(days)'
     :                             , nint(sum_between (sowing, now
     :                                          , g%days_tot)))
 
cscc/glh soil water deficit below dul
      elseif (variable_name .eq. 'sw_deficit') then
         deepest_layer = find_layer_no (g%root_depth, g%dlayer
     :       , max_layer)
         num_layers = count_of_real_vals (g%dlayer, max_layer)
         do 10 i=1,num_layers
            sw_deficit(i) = l_bound(g%dul_dep(i) - g%sw_dep(i),0.0)
10       continue
 
         call respond2get_real_array (variable_name
     :                               , '(mm)'
     :                               , sw_deficit
     :                               , num_layers)
 
      elseif (variable_name .eq. 'sw_demand') then
         call respond2get_real_var (variable_name
     :                             , '(mm)'
     :                             , g%sw_demand)
 
      elseif (variable_name .eq. 'sw_supply_sum') then
         call respond2get_real_var (variable_name
     :                             , '(mm)'
     :                             , g%sw_supply_sum)
 
      elseif (variable_name .eq. 'sw_supply_demand_ratio') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , divide(g%sw_supply_sum,
     :           g%sw_demand,0.0))
 
      elseif (variable_name .eq. 'vpd') then
         call respond2get_real_var (variable_name
     :                             , '(kpa)'
     :                             , g%vpd)
 
            ! plant nitrogen
 
      elseif (variable_name .eq. 'sln') then
         call respond2get_real_var (variable_name
     :                             , '(gN/m2leaf)'
     :                             , divide(
     :                    g%N_green(leaf), g%lai, 0.0))
 
      elseif (variable_name .eq. 'n_conc_stover') then
         N_conc = divide ((g%N_green(leaf)
     :                    + g%N_green(stem)
     :                    + g%N_green(flower))
     :                  , (g%dm_green(leaf)
     :                    + g%dm_green(stem)
     :                    + g%dm_green(flower))
     :                  , 0.0) * 100.
 
         call respond2get_real_var (variable_name
     :                             , '(%)'
     :                             , N_conc)
 
      elseif (variable_name .eq. 'n_conc_leaf') then
         N_conc = divide (g%N_green(leaf)
     :                  , g%dm_green(leaf)
     :                  , 0.0) * 100.
 
         call respond2get_real_var (variable_name
     :                             , '(%)'
     :                             , N_conc)
 
      elseif (variable_name .eq. 'n_conc_stem') then
         N_conc = divide (g%N_green(stem)
     :                  , g%dm_green(stem)
     :                  , 0.0) * 100.
 
         call respond2get_real_var (variable_name
     :                             , '(%)'
     :                             , N_conc)
 
      elseif (variable_name .eq. 'n_conc_grain') then
         N_conc = divide (g%N_green(grain)
     :                  , g%dm_green(grain)
     :                  , 0.0) * 100.
 
         call respond2get_real_var (variable_name
     :                             , '(%)'
     :                             , N_conc)
 
      elseif (variable_name .eq. 'n_conc_crit') then
         N_conc = divide ((g%N_conc_crit(leaf)*g%dm_green(leaf)
     :                    + g%N_conc_crit(stem)*g%dm_green(stem))
     :                  , (g%dm_green(leaf)
     :                    + g%dm_green(stem))
     :                  , 0.0) * 100.
 
         call respond2get_real_var (variable_name
     :                             , '(%)'
     :                             , N_conc)
 
      elseif (variable_name .eq. 'n_conc_min') then
         N_conc = divide ((g%N_conc_min(leaf)*g%dm_green(leaf)
     :                    + g%N_conc_min(stem)*g%dm_green(stem))
     :                  , (g%dm_green(leaf)
     :                    + g%dm_green(stem))
     :                  , 0.0) * 100.
 
         call respond2get_real_var (variable_name
     :                             , '(%)'
     :                             , N_conc)
 
      elseif (variable_name .eq. 'n_grain_pcnt') then
         grain_N_pcnt = divide (g%N_green(grain)
     :                        , g%dm_green(grain), 0.0)
     :                        * fract2pcnt
         call respond2get_real_var (variable_name
     :                             , '(%)'
     :                             , grain_N_pcnt)
 
 
      elseif (variable_name .eq. 'grain_n_uptake') then
         call respond2get_real_var (variable_name
     :                             , '(g/m2)'
     :                             , g%N_green(grain))
 
      elseif (variable_name .eq. 'stover_n_uptake') then
         apt_N_up = (g%N_green(leaf)+g%n_green(stem)+g%n_green(flower))
         call respond2get_real_var (variable_name
     :                             , '(g/m2)'
     :                             , apt_N_up)
 
      elseif (variable_name .eq. 'no3_tot') then
         deepest_layer = find_layer_no (g%root_depth, g%dlayer
     :                                , max_layer)
         NO3gsm_tot = sum_real_array (g%NO3gsm, deepest_layer)
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , NO3gsm_tot)
 
      elseif (variable_name .eq. 'n_demand') then
         N_demand = sum_real_array (g%N_demand, max_part)
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , N_demand)
 
      elseif (variable_name .eq. 'n_supply_soil') then
         deepest_layer = find_layer_no (g%root_depth,g%dlayer,max_layer)
         N_uptake_sum = - sum_real_array (g%dlt_NO3gsm, deepest_layer)
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , N_uptake_sum)
 
      elseif (variable_name .eq. 'nfact_photo') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%nfact_photo)
 
      elseif (variable_name .eq. 'nfact_pheno') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%nfact_pheno)
 
      elseif (variable_name .eq. 'nfact_expan') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%nfact_expansion)
 
      elseif (variable_name .eq. 'nfact_grain') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%nfact_grain_conc)
 
      elseif (variable_name .eq. 'nfact_grain_tot') then
         call respond2get_real_array (variable_name
     :                             , '()'
     :                             , g%cnd_grain_conc
     :                             , max_stage)
 
      ! SWIM COMMS STUFF
      elseif (variable_name .eq. 'rlv') then
         num_layers = count_of_real_vals (g%dlayer, max_layer)
         do 2000 layer = 1, num_layers
            rlv(layer) = divide (g%root_length(layer)
     :                          ,g%dlayer(layer)
     :                          ,0.0)
 2000    continue
         call respond2get_real_array (variable_name
     :                               , '(mm/mm3)'
     :                               , rlv
     :                               , num_layers)
 
      elseif (variable_name .eq. 'no3_demand') then
         N_demand = sum_real_array (g%N_demand, max_part)
     :            * gm2kg/sm2ha
         call respond2get_real_var (variable_name
     :                             , '(kg/ha)'
     :                             , N_demand)
 
      elseif (variable_name .eq. 'sw_demand') then
         call respond2get_real_var (variable_name
     :                             , '(mm)'
     :                             , g%sw_demand)
 
 
      elseif (variable_name .eq. 'root_length') then
         num_layers = count_of_real_vals (g%dlayer, max_layer)
         call respond2get_real_array (variable_name
     :                               , '(mm/mm2)'
     :                               , g%root_length
     :                               , num_layers)
 
      else
         ! not my variable
         call Message_unused ()
 
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      logical function sorg_my_type ()
*     ===========================================================
      use SorgModule
      implicit none
      include 'intrface.pub'                      
      include 'error.pub'                         

*+  Purpose
*       Returns true if 'type' is equal to the crop type or is absent.

*+  Assumptions
*       If type is not specified, it is assumed the message was addressed
*        directly to the module.

*+  Changes
*      211294 sc   specified and programmed
*      220896 jngh changed extract to collect
*                  removed datastring from argument list

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sorg_my_type')

*+  Local Variables
      character  crop_type*50          ! crop type in data string
      integer    numvals               ! number of values returned

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      call collect_char_var_optional ('type', '()'
     :                              , crop_type, numvals)
 
      if (crop_type.eq.c%crop_type .or. numvals.eq.0) then
         sorg_my_type = .true.
      else
         sorg_my_type = .false.
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sorg_read_constants ()
*     ===========================================================
      use SorgModule
      implicit none
      include   'const.inc'
      include 'read.pub'                          
      include 'error.pub'                         

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
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sorg_update ()
*     ===========================================================
      use SorgModule
      implicit none
      include   'convert.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Purpose
*       Update states
*
*   Called by sorg_cleanup in sorgmain.for
*   Calls sorg_cover in sorgsum.for, sorg_N_conc_limits in sorgnit.for

*+  Changes
*      250894 jngh specified and programmed

*+  Calls
!      include   'sorgcons.inc'

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sorg_update')

*+  Local Variables
      real       dlt_dm_plant          ! dry matter increase (g/plant)
      real       dlt_leaf_area         ! leaf area increase (mm^2/plant)
      real       dlt_dm_green_dead     ! dry matter of green plant part dying
                                       ! (g/m^2)
      real       dlt_dm_senesced_dead  ! dry matter of senesced plant part
                                       ! dying (g/m^2)
      real       dlt_N_green_dead      ! N content of green plant part dying
                                       ! (g/m^2)
      real       dlt_N_senesced_dead   ! N content of senesced plant part
                                       ! dying (g/m^2)
      real       dlt_grain_no_lost     ! grain no lost from barrenness
                                       ! (grains/m^2)
      real       dlt_lai_dead          ! lai of green leaf of plants dying ()
      real       dlt_slai_dead         ! lai of senesced leaf of plant dying ()
      real       dying_fract           ! fraction op population dying (0-1)
      real       leaf_no               ! currently expanding leaf no.
      integer    part                  ! plant part index

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
         ! Note.
         ! Accumulate is used to add a value into a specified array element.
         ! If a specified increment of the element indicates a new element
         ! is started, the value is distributed proportionately between the
         ! two elements of the array
 
         ! Add is used to add the elements of one array into the corresponding
         ! elements of another array.
 
         ! now update with deltas
 
         ! The following table describes the transfer of material that should
         ! take place
         !                        POOLS
         !                 green senesced  dead
         ! dlt_green         +                     (incoming only)
         ! dlt_retrans       +-
         ! dlt_senesced      -      +
         ! dlt_dead          -      -       +
         ! dlt_detached             -       -      (outgoing only)
 
         ! transfer N
 
      call subtract_real_array (g%dlt_N_dead_detached, g%N_dead
     :                        , max_part)
 
      call add_real_array (g%dlt_N_green, g%N_green, max_part)
      call add_real_array (g%dlt_N_retrans, g%N_green, max_part)
      call subtract_real_array (g%dlt_N_senesced, g%N_green
     :                        , max_part)
 
      call add_real_array (g%dlt_N_senesced, g%N_senesced
     :                   , max_part)
      call subtract_real_array (g%dlt_N_detached, g%N_senesced
     :                        , max_part)
 
      dying_fract = divide (-g%dlt_plants, g%plants, 0.0)
      dying_fract = bound (dying_fract, 0.0, 1.0)
 
      do 1000 part = 1, max_part
         dlt_N_green_dead = g%N_green(part) * dying_fract
         g%N_green(part) = g%N_green(part) - dlt_N_green_dead
         g%N_dead(part) = g%N_dead(part) + dlt_N_green_dead
 
         dlt_N_senesced_dead = g%N_senesced(part) * dying_fract
         g%N_senesced(part) = g%N_senesced(part) - dlt_N_senesced_dead
         g%N_dead(part) = g%N_dead(part) + dlt_N_senesced_dead
1000  continue
 
 
         ! Transfer plant dry matter
 
      dlt_dm_plant = divide (g%dlt_dm, g%plants, 0.0)
 
      call accumulate (dlt_dm_plant, g%dm_plant_top_tot
     :               , g%previous_stage, g%dlt_stage)
 
      call subtract_real_array (g%dlt_dm_dead_detached, g%dm_dead
     :                        , max_part)
 
      call add_real_array (g%dlt_dm_green, g%dm_green, max_part)
      call add_real_array (g%dlt_dm_green_retrans, g%dm_green, max_part)
      call subtract_real_array (g%dlt_dm_senesced, g%dm_green
     :                        , max_part)
 
      call add_real_array (g%dlt_dm_senesced, g%dm_senesced
     :                   , max_part)
      call subtract_real_array (g%dlt_dm_sen_retrans, g%dm_senesced
     :                        , max_part)
      call subtract_real_array (g%dlt_dm_detached, g%dm_senesced
     :                        , max_part)
 
      do 2000 part = 1, max_part
         dlt_dm_green_dead = g%dm_green(part) * dying_fract
         g%dm_green(part) = g%dm_green(part) - dlt_dm_green_dead
         g%dm_dead(part) = g%dm_dead(part) + dlt_dm_green_dead
 
         dlt_dm_senesced_dead = g%dm_senesced(part) * dying_fract
         g%dm_senesced(part) = g%dm_senesced(part)
     :                       - dlt_dm_senesced_dead
         g%dm_dead(part) = g%dm_dead(part) + dlt_dm_senesced_dead
2000  continue
 
cjh
         ! transfer plant grain no.
      dlt_grain_no_lost  = g%grain_no * dying_fract
      g%grain_no = g%grain_no - dlt_grain_no_lost
cglh
         ! update fertile no (pluses and minuses are the best I can do!)
 
      g%tiller_no_fertile = g%tiller_no_fertile + g%dlt_tiller_no
     :                                         - g%dlt_stiller_no
 
         ! transfer plant leaf area
 
      g%lai = g%lai + g%dlt_lai - g%dlt_slai
      g%slai = g%slai + g%dlt_slai - g%dlt_slai_detached
 
      dlt_lai_dead  = g%lai  * dying_fract
      dlt_slai_dead = g%slai * dying_fract
 
      g%lai = g%lai - dlt_lai_dead
      g%slai = g%slai - dlt_slai_dead
      g%tlai_dead = g%tlai_dead + dlt_lai_dead + dlt_slai_dead
     :            - g%dlt_tlai_dead_detached
 
        ! now update new canopy covers for erosion etc?
 
      call crop_cover1
     :               (g%row_spacing
     :                ,c%x_row_spacing,c%y_extinct_coef
     :                ,c%num_row_spacing
     :                ,g%lai,g%cover_green)
      call crop_cover1
     :               (g%row_spacing
     :                ,c%x_row_spacing,c%y_extinct_coef_dead
     :                ,c%num_row_spacing
     :                ,g%slai,g%cover_sen)
      call crop_cover1
     :               (g%row_spacing
     :                ,c%x_row_spacing,c%y_extinct_coef_dead
     :                ,c%num_row_spacing
     :                ,g%tlai_dead,g%cover_dead)
 
 
         ! plant leaf development
         ! need to account for truncation of partially developed leaf (add 1)
      leaf_no = 1.0 + sum_between (emerg, now, g%leaf_no)
      dlt_leaf_area = divide (g%dlt_lai, g%plants, 0.0) * sm2smm
      call accumulate (dlt_leaf_area, g%leaf_area
     :               , leaf_no, g%dlt_leaf_no)
 
      call accumulate (g%dlt_leaf_no, g%leaf_no
     :               , g%previous_stage, g%dlt_stage)
 
      call accumulate (g%dlt_leaf_no_dead, g%leaf_no_dead
     :               , g%previous_stage, g%dlt_stage)
 
         ! plant stress
 
      call accumulate (g%dlt_heat_stress_tt, g%heat_stress_tt
     :               , g%previous_stage, g%dlt_stage)
 
      call accumulate (g%dlt_dm_stress_max, g%dm_stress_max
     :               , g%current_stage, g%dlt_stage)
 
      call accumulate (1.0 - g%swdef_photo
     .               , g%cswd_photo
     :               , g%previous_stage, g%dlt_stage)
      call accumulate (1.0 - g%swdef_expansion
     .               , g%cswd_expansion
     :               , g%previous_stage, g%dlt_stage)
      call accumulate (1.0 - g%swdef_pheno
     .               , g%cswd_pheno
     :               , g%previous_stage, g%dlt_stage)
 
      call accumulate (1.0 - g%nfact_photo
     .               , g%cnd_photo
     :               , g%previous_stage, g%dlt_stage)
      call accumulate (1.0 - g%nfact_grain_conc
     .               , g%cnd_grain_conc
     :               , g%previous_stage, g%dlt_stage)
 
         ! other plant states
 
      g%canopy_height = g%canopy_height + g%dlt_canopy_height
      g%plants = g%plants + g%dlt_plants
      g%root_depth = g%root_depth + g%dlt_root_depth
      call add_real_array      (g%dlt_root_length
     :                         ,g%root_length
     :                         ,max_layer)
      call subtract_real_array (g%dlt_root_length_senesced
     :                         ,g%root_length
     :                         ,max_layer)
 
 
      call sorg_N_conc_limits (
     .          g%current_stage,
     .          c%N_conc_crit_grain,
     .          c%N_conc_max_grain,
     .          c%N_conc_min_grain,
     .          c%N_conc_crit_root,
     .          c%N_conc_max_root,
     .          c%N_conc_min_root,
     .          c%x_stage_code,
     .          c%stage_code_list,
     .          g%tt_tot,
     .          g%phase_tt,
     .          c%y_N_conc_crit_stem,
     .          c%y_N_conc_crit_leaf,
     .          c%y_N_conc_crit_flower,
     .          c%y_N_conc_min_stem,
     .          c%y_N_conc_min_leaf,
     .          c%y_N_conc_min_flower,
     .          c%y_N_conc_max_stem,
     .          c%y_N_conc_max_leaf,
     .          c%y_N_conc_max_flower,
     .          g%N_conc_crit,
     .          g%N_conc_max,
     .          g%N_conc_min)  ! plant N concentr
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sorg_check_bounds (
     .          g_leaf_no,
     .          g_leaf_no_dead,
     .          g_root_depth,
     .          g_dlayer,
     .          g_grain_no,
     .          p_head_grain_no_max,
     .          g_plants,
     .          g_current_stage,
     .          g_phase_tt,
     .          g_days_tot,
     .          g_tt_tot,
     .          g_canopy_height,
     .          c_height_max,
     .          g_lai,
     .          g_slai,
     .          g_tlai_dead,
     .          g_cover_green,
     .          g_cover_sen,
     .          g_cover_dead,
     .          g_leaf_area,
     .          g_heat_stress_tt,
     .          g_dm_stress_max,
     .          g_N_conc_crit,
     .          g_N_conc_min,
     .          g_N_conc_max,
     .          g_N_dead,
     .          g_N_green,
     .          g_N_senesced,
     .          g_dm_dead,
     .          g_dm_green,
     .          g_dm_senesced)
*     ===========================================================
      implicit none
      include   'sorgcons.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
       real g_leaf_no(*)
       real g_leaf_no_dead(*)
       real g_root_depth
       real g_dlayer(*)
       real g_grain_no
       real p_head_grain_no_max
       real g_plants
       real g_current_stage
       real g_phase_tt(*)
       real g_days_tot(*)
       real g_tt_tot(*)
       real g_canopy_height
       real c_height_max
       real g_lai
       real g_slai
       real g_tlai_dead
       real g_cover_green
       real g_cover_sen
       real g_cover_dead
       real g_leaf_area(*)
       real g_heat_stress_tt(*)
       real g_dm_stress_max(*)
       real g_N_conc_crit(*)
       real g_N_conc_min(*)
       real g_N_conc_max(*)
       real g_N_dead(*)
       real g_N_green(*)
       real g_N_senesced(*)
       real g_dm_dead(*)
       real g_dm_green(*)
       real g_dm_senesced(*)

*+  Purpose
*         Check bounds of internal pools
*
*   Called by sorg_cleanup in sorgmain.for

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sorg_check_bounds')

*+  Local Variables
                                       ! top (g/m^2)

*- Implementation Section ----------------------------------
 
 
      call push_routine (my_name)
 
      call bound_check_real_var
     :           (sum_real_array (g_leaf_no, max_stage)
     :          , 0.0
     :          , real (max_leaf)
     :          , 'leaf_no')
 
      call bound_check_real_var
     :           (sum_real_array (g_leaf_no_dead, max_stage)
     :          , 0.0
     :          , real (max_leaf)
     :          , 'leaf_no_dead')
 
      call bound_check_real_var
     :           (g_root_depth
     :          , 0.0
     :          , sum_real_array (g_dlayer, max_layer)
     :          , 'root_depth')
 
      call bound_check_real_var
     :           (g_grain_no
     :          , 0.0
     :          , p_head_grain_no_max * g_plants
     :          , 'grain_no')
 
      call bound_check_real_var
     :           (g_current_stage
     :          , 0.0
     :          , real (max_stage)
     :          , 'current_stage')
 
      call bound_check_real_var
     :           (sum_real_array (g_phase_tt, max_stage)
     :          , 0.0
     :          , 1000000.0
     :          , 'phase_tt')
 
      call bound_check_real_var
     :           (sum_real_array (g_days_tot, max_stage)
     :          , 0.0
     :          , 40000.0
     :          , 'days_tot')
 
      call bound_check_real_var
     :           (sum_real_array (g_tt_tot, max_stage)
     :          , 0.0
     :          , 40000.0
     :          , 'tt_tot')
 
      call bound_check_real_var
     :           (g_plants
     :          , 0.0
     :          , 10000.0
     :          , 'plants')
 
      call bound_check_real_var
     :           (g_canopy_height
     :          , 0.0
     :          , c_height_max
     :          , 'canopy_height')
 
      call bound_check_real_var
     :           (g_lai
     :          , 0.0
     :          , 30.0 - g_slai - g_tlai_dead
     :          , 'lai')
 
      call bound_check_real_var
     :           (g_slai
     :          , 0.0
     :          , 30.0 - g_lai - g_tlai_dead
     :          , 'slai')
 
      call bound_check_real_var
     :           (g_tlai_dead
     :          , 0.0
     :          , 30.0 - g_slai - g_lai
     :          , 'tlai_dead')
 
      call bound_check_real_var
     :           (g_cover_green
     :          , 0.0
     :          , 1.0
     :          , 'cover_green')
 
      call bound_check_real_var
     :           (g_cover_sen
     :          , 0.0
     :          , 1.0
     :          , 'cover_sen')
 
      call bound_check_real_var
     :           (g_cover_dead
     :          , 0.0
     :          , 1.0
     :          , 'cover_dead')
 
      call bound_check_real_var
     :           (sum_real_array (g_leaf_area, max_leaf)
     :          , 0.0
     :          , 10000000.0
     :          , 'leaf_area')
 
      call bound_check_real_var
     :           (sum_real_array (g_heat_stress_tt, max_stage)
     :          , 0.0
     :          , 1000000.0
     :          , 'heat_stress_tt')
      call bound_check_real_var
     :           (sum_real_array (g_dm_stress_max, max_stage)
     :          , 0.0
     :          , 1000000.0
     :          , 'dm_stress_max')
 
      call bound_check_real_var
     :           (sum_real_array (g_N_conc_crit, max_part)
     :          , sum_real_array (g_N_conc_min, max_part)
     :          , sum_real_array (g_N_conc_max, max_part)
     :          , 'N_conc_crit')
 
      call bound_check_real_var
     :           (sum_real_array (g_N_conc_max, max_part)
     :          , sum_real_array (g_N_conc_crit, max_part)
     :          , 1.0
     :          , 'N_conc_max')
 
      call bound_check_real_var
     :           (sum_real_array (g_N_conc_min, max_part)
     :          , 0.0
     :          , sum_real_array (g_N_conc_crit, max_part)
     :          , 'N_conc_min')
 
      call bound_check_real_var
     :           (sum_real_array (g_N_dead, max_part)
     :          , 0.0
     :          , 10000.0 - sum_real_array (g_N_green, max_part)
     :                    - sum_real_array (g_N_senesced, max_part)
     :          , 'N_dead')
 
      call bound_check_real_var
     :           (sum_real_array (g_N_green, max_part)
     :          , 0.0
     :          , 10000.0 - sum_real_array (g_N_dead, max_part)
     :                    - sum_real_array (g_N_senesced, max_part)
     :          , 'N_green')
 
      call bound_check_real_var
     :           (sum_real_array (g_N_senesced, max_part)
     :          , 0.0
     :          , 10000.0 - sum_real_array (g_N_green, max_part)
     :                    - sum_real_array (g_N_dead, max_part)
     :          , 'N_senesced')
 
      call bound_check_real_var
     :           (sum_real_array (g_dm_dead, max_part)
     :          , 0.0
     :          , 10000.0 - sum_real_array (g_dm_green, max_part)
     :                    - sum_real_array (g_dm_senesced, max_part)
     :          , 'dm_dead')
 
      call bound_check_real_var
     :           (sum_real_array (g_dm_green, max_part)
     :          , 0.0
     :          , 10000.0 - sum_real_array (g_dm_dead, max_part)
     :                    - sum_real_array (g_dm_senesced, max_part)
     :          , 'dm_green')
 
      call bound_check_real_var
     :           (sum_real_array (g_dm_senesced, max_part)
     :          , 0.0
     :          , 10000.0 - sum_real_array (g_dm_green, max_part)
     :                    - sum_real_array (g_dm_dead, max_part)
     :          , 'dm_senesced')
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sorg_totals (
     .          g_N_green,
     .          g_dm_green,
     .          g_dlt_N_retrans,
     .          g_N_conc_crit,
     .          g_N_demand,
     .          g_root_depth,
     .          g_dlayer,
     .          g_current_stage,
     .          g_days_tot,
     .          g_N_uptake_tot,
     .          g_transpiration_tot,
     .          g_dlt_sw_dep,
     .          g_N_conc_act_stover_tot,
     .          g_N_conc_crit_stover_tot,
     .          g_N_demand_tot,
     .          g_N_uptake_stover_tot,
     .          g_N_uptake_grain_tot,
     .          g_lai_max,
     .          g_lai,
     .          g_flowering_date,
     .          g_maturity_date,
     .          g_flowering_das,
     .          g_maturity_das,
     .          g_N_dead,
     .          g_N_senesced,
     .          g_day_of_year)
*     ===========================================================
      implicit none
      include   'sorgcons.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
       real g_N_green(*)
       real g_dm_green(*)
       real g_dlt_N_retrans(*)
       real g_N_conc_crit(*)
       real g_N_demand(*)
       real g_root_depth
       real g_dlayer(*)
       real g_current_stage
       real g_days_tot(*)
       real g_N_uptake_tot
       real g_transpiration_tot
       real g_dlt_sw_dep(*)
       real g_N_conc_act_stover_tot
       real g_N_conc_crit_stover_tot
       real g_N_demand_tot
       real g_N_uptake_stover_tot
       real g_N_uptake_grain_tot
       real g_lai_max
       real g_lai
       integer g_flowering_date
       integer g_maturity_date
       integer g_flowering_das
       integer g_maturity_das
       real g_N_dead(*)
       real g_N_senesced(*)
       integer g_day_of_year

*+  Purpose
*         Collect totals of crop variables for output
*
*   Called by sorg_cleanup in sorgmain.for

*+  Changes
*     010994 jngh specified and programmed

*+  Calls
cpsc  add below
cjh      include   'convert.inc'          ! gm2kg, sm2ha, sm2smm

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sorg_totals')

*+  Local Variables
      real       N_conc_stover         ! tops actual N concentration
                                       ! (g N/g part)
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      real       N_conc_stover_crit    ! tops critical N concentration
                                       ! (g N/g part)
      real       N_green_demand        ! plant N demand (g/m^2)
      real       N_uptake              ! nitrogen uptake from soil (g/m^2)
      real       N_uptake_stover       ! nitrogen uptake from soil by veg.
                                       ! top (g/m^2)
cpsc add below
      real       N_grain               ! total grain N uptake
      real       N_dead                ! above ground dead plant N
      real       N_green               ! above ground green plant N
      real       N_senesced            ! above ground senesced plant N
      real       N_stover              ! nitrogen content of stover

*- Implementation Section ----------------------------------
 
 
      call push_routine (my_name)
 
             ! get totals
      N_conc_stover = divide ((g_N_green(leaf)
     :                       + g_N_green(stem)
     :                       + g_N_green(flower))
 
     :                      , (g_dm_green(leaf)
     :                       + g_dm_green(stem)
     :                       + g_dm_green(flower))
     :                       , 0.0)
 
      N_uptake = sum_real_array (g_dlt_N_retrans, max_part)
      N_uptake_stover =  g_dlt_N_retrans(leaf) + g_dlt_N_retrans(stem)
 
          ! note - g_N_conc_crit should be done before the stages change
 
      N_conc_stover_crit = (g_N_conc_crit(leaf) + g_N_conc_crit(stem))
     :                   * 0.5
      N_green_demand = sum_real_array (g_N_demand, max_part)
 
      deepest_layer = find_layer_no (g_root_depth, g_dlayer, max_layer)
 
      if (on_day_of (sowing, g_current_stage, g_days_tot)) then
         g_N_uptake_tot = N_uptake
         g_transpiration_tot =
     :           - sum_real_array (g_dlt_sw_dep, deepest_layer)
         g_N_conc_act_stover_tot = N_conc_stover
         g_N_conc_crit_stover_tot = N_conc_stover_crit
         g_N_demand_tot = N_green_demand
         g_N_uptake_stover_tot = N_uptake_stover
         g_N_uptake_grain_tot = sum_real_array (g_dlt_N_retrans
     :                                        , max_part)
 
      else
         g_N_uptake_tot = g_N_uptake_tot + N_uptake
         g_transpiration_tot = g_transpiration_tot
     :                       + (-sum_real_array (g_dlt_sw_dep
     :                                         , deepest_layer))
         g_N_conc_act_stover_tot = N_conc_stover
         g_N_conc_crit_stover_tot = N_conc_stover_crit
         g_N_demand_tot = g_N_demand_tot + N_green_demand
         g_N_uptake_stover_tot = g_N_uptake_stover_tot
     :                         + N_uptake_stover
         g_N_uptake_grain_tot = g_N_uptake_grain_tot
     :                        + sum_real_array (g_dlt_N_retrans
     :                                        , max_part)
 
      endif
 
      g_lai_max = max (g_lai_max, g_lai)
      if (on_day_of (flowering, g_current_stage, g_days_tot)) then
         g_flowering_date = g_day_of_year
         g_flowering_das  = sum_between (sowing, now, g_days_tot)
      else if (on_day_of (maturity, g_current_stage, g_days_tot)) then
         g_maturity_date = g_day_of_year
         g_maturity_das  = sum_between (sowing, now, g_days_tot)
      else
      endif
 
cpsc add below 07/04/95
 
      N_grain = (g_N_green(grain) + g_N_dead(grain))
 
      N_green = (sum_real_array (g_N_green, max_part)
     :        - g_N_green(root) - g_N_green(grain))
 
      N_senesced = (sum_real_array (g_N_senesced, max_part)
     :           - g_N_senesced(root) - g_N_senesced(grain))
 
      N_dead = (sum_real_array (g_N_dead, max_part)
     :       - g_N_dead(root) - g_N_dead(grain))
 
      N_stover = N_green + N_senesced + N_dead
 
      g_N_uptake_grain_tot = N_grain
      g_N_uptake_stover_tot = N_stover
      g_N_uptake_tot = N_grain + N_stover
 
cpsc  add above
 
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sorg_event (
     .          g_current_stage,
     .          g_days_tot,
     .          c_stage_code_list,
     .          c_stage_names,
     .          g_dm_green,
     .          g_dm_senesced,
     .          g_dm_dead,
     .          g_N_green,
     .          g_root_depth,
     .          g_dlayer,
     .          g_sw_dep,
     .          p_ll_dep,
     .          g_lai)
 
*     ===========================================================
      implicit none
      include   'const.inc'            ! new_line, lu_scr_sum, blank,
      include   'convert.inc'
      include   'sorgcons.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
       real g_current_stage
       real g_days_tot(*)
       real c_stage_code_list(*)
       character c_stage_names(*)*(*)
       real g_dm_green(*)
       real g_dm_senesced(*)
       real g_dm_dead(*)
       real g_N_green(*)
       real g_root_depth
       real g_dlayer(*)
       real g_sw_dep(*)
       real p_ll_dep(*)
       real g_lai

*+  Purpose
*       Report occurence of event and the current status of specific
*       variables.
*
*   Called by sorg_cleanup in sorgmain.for

*+  Changes
*     010994 jngh specified and programmed

*+  Calls
                                       ! lu_scr_sum

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sorg_event')

*+  Local Variables
      real       biomass               ! total above ground plant wt (g/m^2)
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      integer    layer                 ! profile layer number
      real       pesw_tot              ! total plant extractable sw (mm)
      real       pesw(max_layer)       ! plant extractable soil water (mm)
      real       N_green               ! plant nitrogen of tops (g/m^2)
                                       ! less flower
      real       dm_green              ! plant wt of tops (g/m^2) less flower
      integer    stage_no              ! stage number at beginning of phase
      character  string*200            ! message
      real       N_green_conc_percent  ! n% of tops less flower (incl grain)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      stage_no = int (g_current_stage)
 
      if (on_day_of (stage_no, g_current_stage, g_days_tot)) then
             ! new phase has begun.
         write (string, '(a, f6.1, 1x, a)')
     :                   ' stage '
     :                  , c_stage_code_list(stage_no)
     :                  , c_stage_names(stage_no)
         call Write_string (string)
 
         biomass = sum_real_array (g_dm_green, max_part)
     :           - g_dm_green(root)
 
     :           + sum_real_array (g_dm_senesced, max_part)
     :           - g_dm_senesced(root)
 
     :           + sum_real_array (g_dm_dead, max_part)
     :           - g_dm_dead(root)
 
         dm_green = sum_real_array (g_dm_green, max_part)
     :            - g_dm_green(root)
         N_green = sum_real_array (g_N_green, max_part)
     :           - g_N_green(root)
 
         N_green_conc_percent = divide (N_green, dm_green, 0.0)
     :                        * fract2pcnt
 
         deepest_layer = find_layer_no (g_root_depth, g_dlayer
     :                                , max_layer)
         do 1000 layer = 1, deepest_layer
            pesw(layer) = g_sw_dep(layer) - p_ll_dep(layer)
            pesw(layer) = l_bound (pesw(layer), 0.0)
1000     continue
         pesw_tot = sum_real_array (pesw, deepest_layer)
 
         if (stage_is_between (emerg, plant_end, g_current_stage)) then
            write (string, '(2(a, g16.7e2), a, 2(a, g16.7e2))')
     :              '                     biomass =       '
     :            , biomass
     :            , '   lai = '
     :            , g_lai
     :            , new_line
     :            ,'                     stover N conc ='
     :            , N_green_conc_percent
     :            , '   extractable sw ='
     :            , pesw_tot
            call write_string (string)
         else
         endif
 
      else
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sorg_root_distrib (
     .          g_root_depth,
     .          g_dlayer,
     .          root_array, root_sum)
*     ===========================================================
      implicit none
      include   'sorgcons.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
       real g_root_depth
       real g_dlayer(*)
      real       root_array(*)         ! (OUTPUT) array to contain
                                       ! distributed material
      real       root_sum              ! (INPUT) Material to be distributed

*+  Purpose
*       Distribute root material over profile
*
*   Called by sorg_root_incorp in sorgsum.for

*+  Changes
*       290994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sorg_root_distrib')

*+  Local Variables
      real       cum_depth             ! cumulative depth (mm)
      integer    layer                 ! layer number ()
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      real       root_distrb(max_layer) ! root distribution ()
      real       root_distrb_sum       ! sum of root distribution array

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
             ! distribute roots over profile to root_depth
 
      call fill_real_array (root_array, 0.0, max_layer)
      call fill_real_array (root_distrb, 0.0, max_layer)
 
      deepest_layer = find_layer_no (g_root_depth, g_dlayer
     :                                , max_layer)
      cum_depth = 0.0
      do 1000 layer = 1, deepest_layer
         cum_depth = cum_depth + g_dlayer(layer)
         cum_depth = u_bound (cum_depth, g_root_depth)
         root_distrb(layer) = exp (-3.0 * divide (cum_depth
     :                                          , g_root_depth, 0.0))
1000  continue
 
      root_distrb_sum = sum_real_array (root_distrb, deepest_layer)
      do 2000 layer = 1, deepest_layer
         root_array(layer) = root_sum * divide (root_distrb(layer)
     :                                        , root_distrb_sum, 0.0)
 
2000  continue
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sorg_top_residue (
     .          c_crop_type,
     .          dlt_residue_weight, dlt_residue_N)
*     ===========================================================
      implicit none
      include   'const.inc'            ! all_active_modules
      include   'convert.inc'
      include 'intrface.pub'                      
      include 'error.pub'     
      include 'postbox.pub'                    

*+  Sub-Program Arguments
       character c_crop_type*(*)
      real       dlt_residue_weight    ! (INPUT) new surface residue (g/m^2)
      real       dlt_residue_N         ! (INPUT) new surface residue N (g/m^2)

*+  Purpose
*       Add residue to residue pool
*
*   Called by sorg_end_crop in sorgmain.for

*+  Changes
*       220794 jngh specified and programmed
*       170895 jngh changed message send to message pass to module
*       220896 jngh changed to post_ construct

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sorg_top_residue')

*+  Local Variables
cjh      character  string*200            ! output string

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (dlt_residue_weight.gt.0.0) then
            ! send out surface residue
 
         call New_postbox ()
 
         call post_char_var('dlt_residue_type','()',c_crop_type)
 
         call post_real_var ('dlt_residue_wt'
     :                        ,'(kg/ha)'
     :                        ,dlt_residue_weight * gm2kg /sm2ha)
 
         call post_real_var ('dlt_residue_n'
     :                        ,'(kg/ha)'
     :                        ,dlt_residue_N * gm2kg /sm2ha)
 
         call Action_send (
     :                              unknown_module
     :                            , 'add_residue'
     :                            , Blank
     :                            )
 
         call Delete_postbox ()
 
      else
         ! no surface residue
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sorg_root_incorp (
     .          g_root_depth,
     .          g_dlayer,
     .          c_crop_type,
     .          dlt_dm_root, dlt_N_root)
*     ===========================================================
      implicit none
      include   'const.inc'            ! all_active_modules
      include   'convert.inc'
      include 'action.inc'
      include   'sorgcons.inc'
      include 'science.pub'                       
      include 'intrface.pub'                      
      include 'error.pub'     
      include 'postbox.pub'                    

*+  Sub-Program Arguments
       real g_root_depth
       real g_dlayer(*)
       character c_crop_type*(*)
      real       dlt_dm_root           ! (INPUT) new root residue dm (g/m^2)
      real       dlt_N_root            ! (INPUT) new root residue N (g/m^2)

*+  Purpose
*       Add root residue to root residue pool
*
*   Called by sorg_end_crop in sorgmain.for
*   Calls sorg_root_distrib in sorgsum.for

*+  Changes
*       220794 jngh specified and programmed
*       170895 jngh changed message send to message pass to module
*       220896 jngh changed to post_ construct

*+  Calls
cjh      integer    lastnb                ! function
cjh      character  string_concat*(ACTION_data_size) ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sorg_root_incorp')

*+  Local Variables
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      real       dlt_dm_incorp(max_layer) ! root residue (kg/ha)
      real       dlt_N_incorp(max_layer)  ! root residue N (kg/ha)
*
cjh      integer    layer                 ! layer number
cjh      character  string*(ACTION_data_size) ! output string

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (dlt_dm_root.gt.0.0) then
 
            ! send out root residue
 
         call sorg_root_distrib (
     .          g_root_depth,
     .          g_dlayer,
     .          dlt_dm_incorp
     :         , dlt_dm_root * gm2kg /sm2ha)
         call sorg_root_distrib (
     .          g_root_depth,
     .          g_dlayer,
     .          dlt_N_incorp
     :        , dlt_N_root * gm2kg /sm2ha)
 
         deepest_layer = find_layer_no (g_root_depth, g_dlayer
     :                                , max_layer)
 
         call New_postbox ()
 
         call post_char_var('dlt_fom_type=','()',c_crop_type)
 
         call post_real_array ('dlt_fom_wt'
     :                        ,'(kg/ha)'
     :                        ,dlt_dm_incorp
     :                        ,deepest_layer)
 
         call post_real_array ('dlt_fom_n'
     :                        ,'(kg/ha)'
     :                        ,dlt_n_incorp
     :                        ,deepest_layer)
 
         call Action_send (
     :                              unknown_module
     :                            , 'incorp_fom'
     :                            , Blank
     :                            )
 
         call Delete_postbox ()
 
      else
         ! no roots to incorporate
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      real function sorg_stage_code (
     .          c_stage_code_list,
     .          g_tt_tot,
     .          g_phase_tt,
     .          stage_no, stage_table, numvals)
*     ===========================================================
      implicit none
      include   'const.inc'            ! err_user
      include   'sorgcons.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
       real c_stage_code_list(*)
       real g_tt_tot(*)
       real g_phase_tt(*)
*
      real       stage_no              ! (INPUT) stage number to convert
      real       stage_table(*)        ! (INPUT) table of stage codes
      integer    numvals               ! (INPUT) size_of of table

*+  Purpose
*       Return an interpolated stage code from a table of stage_codes
*       and a nominated stage number. Returns 0 if the stage number is not
*       found. Interpolation is done on thermal time.
*
*   Called from sorg_N_conc_limits in sorgnit.for

*+  Changes
*       080994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sorg_stage_code')

*+  Local Variables
      real       phase_tt              ! required thermal time between stages
                                       ! (oC)
      character  error_message*100     ! error message
      real       fraction_of           !
      integer    i                     ! array index - counter
      integer    next_stage            ! next stage number to use
      real       tt_tot                ! elapsed thermal time between stages
                                       ! (oC)
      integer    this_stage            ! this stage to use
      real       x_stage_code          ! interpolated stage code

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (numvals.ge.2) then
            ! we have a valid table
         this_stage = stage_no_of (stage_table(1)
     :                           , c_stage_code_list, max_stage)
 
         do 1000 i = 2, numvals
            next_stage = stage_no_of (stage_table(i)
     :                              , c_stage_code_list, max_stage)
 
            if (stage_is_between (this_stage, next_stage, stage_no))
     :         then
                  ! we have found its place
               tt_tot = sum_between (this_stage, next_stage, g_tt_tot)
               phase_tt = sum_between (this_stage, next_stage
     :                               , g_phase_tt)
               fraction_of = divide (tt_tot, phase_tt, 0.0)
               x_stage_code = stage_table(i-1)
     :                      + (stage_table(i) - stage_table(i-1))
     :                      * fraction_of
               goto 2000
 
            else
               x_stage_code = 0.0
               this_stage = next_stage
 
            endif
1000     continue
2000     continue
      else
            ! we have no valid table
 
         x_stage_code = 0.0
 
         write (error_message,'(a, i10)')
     :               'Invalid lookup table - number of values ='
     :              , numvals
         call warning_error (err_user, error_message)
 
      endif
      sorg_stage_code = x_stage_code
 
      call pop_routine (my_name)
 
      return
      end



*     ===========================================================
      subroutine sorg_N_conc_limits (
     .          g_current_stage,
     .          c_N_conc_crit_grain,
     .          c_N_conc_max_grain,
     .          c_N_conc_min_grain,
     .          c_N_conc_crit_root,
     .          c_N_conc_max_root,
     .          c_N_conc_min_root,
     .          c_x_stage_code,
     .          c_stage_code_list,
     .          g_tt_tot,
     .          g_phase_tt,
     .          c_y_N_conc_crit_stem,
     .          c_y_N_conc_crit_leaf,
     .          c_y_N_conc_crit_flower,
     .          c_y_N_conc_min_stem,
     .          c_y_N_conc_min_leaf,
     .          c_y_N_conc_min_flower,
     .          c_y_N_conc_max_stem,
     .          c_y_N_conc_max_leaf,
     .          c_y_N_conc_max_flower,
     .          N_conc_crit,
     .          N_conc_max,
     .          N_conc_min)
*     ===========================================================
      implicit none
      include   'sorgcons.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
       real g_current_stage
       real c_N_conc_crit_grain
       real c_N_conc_max_grain
       real c_N_conc_min_grain
       real c_N_conc_crit_root
       real c_N_conc_max_root
       real c_N_conc_min_root
       real c_x_stage_code(*)
       real c_stage_code_list(*)
       real g_tt_tot(*)
       real g_phase_tt(*)
       real c_y_n_conc_crit_stem(*)
       real c_y_n_conc_crit_leaf(*)
       real c_y_n_conc_crit_flower(*)
       real c_y_n_conc_min_stem(*)
       real c_y_n_conc_min_leaf(*)
       real c_y_n_conc_min_flower(*)
       real c_y_n_conc_max_stem(*)
       real c_y_n_conc_max_leaf(*)
       real c_y_n_conc_max_flower(*)
      real       N_conc_crit(*)        ! (OUTPUT) critical N concentration
                                       ! (g N/g part)
      real       N_conc_max(*)         ! (OUTPUT) maximum N concentration
                                       ! (g N/g part)
      real       N_conc_min(*)         ! (OUTPUT) minimum N concentration
                                       ! (g N/g part)

*+  Purpose
*       Calculate the critical N concentration below which plant growth
*       is affected.  Also minimum and maximum N concentrations below
*       and above which it is not allowed to fall or rise.
*       These are analogous to the water concentrations
*       of sat, dul and ll.

*+  Changes
*     080994 jngh specified and programmed

*+  Calls
      real       sorg_stage_code      ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sorg_N_conc_limits')

*+  Local Variables
      integer    numvals               ! number of values in stage code table
      real       current_stage_code            ! interpolated current stage code

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      call fill_real_array (N_conc_crit, 0.0, max_part)
      call fill_real_array (N_conc_min, 0.0, max_part)
 
      if (stage_is_between (emerg, maturity, g_current_stage)) then
         N_conc_crit(grain) = c_N_conc_crit_grain
         N_conc_max(grain) = c_N_conc_max_grain
         N_conc_min(grain) = c_N_conc_min_grain
 
         N_conc_crit(root) = c_N_conc_crit_root
         N_conc_max(root) = c_N_conc_max_root
         N_conc_min(root) = c_N_conc_min_root
 
             ! the tops critical N percentage concentration is the stover
             ! (non-grain shoot) concentration below which N concentration
             ! begins to affect plant growth.
 
         numvals = count_of_real_vals (c_x_stage_code, max_stage)
         current_stage_code = sorg_stage_code (
     .          c_stage_code_list,
     .          g_tt_tot,
     .          g_phase_tt,
     .          g_current_stage,
     .          c_x_stage_code,
     .          numvals)
         N_conc_crit(stem) = linear_interp_real (current_stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_crit_stem
     :                             , numvals)
         N_conc_crit(leaf) = linear_interp_real (current_stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_crit_leaf
     :                             , numvals)
         N_conc_crit(flower) = linear_interp_real (current_stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_crit_flower
     :                             , numvals)
 
             ! the  minimum N concentration is the N concentration
             ! below which N does not fall.
 
         N_conc_min(stem) = linear_interp_real (current_stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_min_stem
     :                             , numvals)
 
         N_conc_min(leaf) = linear_interp_real (current_stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_min_leaf
     :                             , numvals)
 
         N_conc_min(flower) = linear_interp_real (current_stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_min_flower
     :                             , numvals)
 
             ! the  maximum N concentration is the N concentration
             ! above which N does not rise.
 
         N_conc_max(stem) = linear_interp_real (current_stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_max_stem
     :                             , numvals)
 
         N_conc_max(leaf) = linear_interp_real (current_stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_max_leaf
     :                             , numvals)
 
         N_conc_max(flower) = linear_interp_real (current_stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_max_flower
     :                             , numvals)
 
      else
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sorg_kill_crop (
     .          g_plant_status,
     .          g_dm_green,
     .          g_dm_senesced,
     .          g_dm_dead)
*     ===========================================================
      implicit none
      include   'convert.inc'          ! gm2kg, sm2ha, mm2cm, cmm2cc
      include   'sorgcons.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
       character g_plant_status*(*)
       real g_dm_green(*)
       real g_dm_senesced(*)
       real g_dm_dead(*)

*+  Purpose
*       Kill crop

*+  Changes
*       290994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sorg_kill_crop')

*+  Local Variables
      real       biomass               ! above ground dm (kg/ha)
      character  string*200            ! output string

*- Implementation Section ----------------------------------
 
c+!!!!!! fix problem with deltas in update when change from alive to dead ?zero
      call push_routine (my_name)
 
      if (g_plant_status.eq.status_alive) then
         g_plant_status = status_dead
 
         biomass = (sum_real_array (g_dm_green, max_part)
     :           - g_dm_green(root)) * gm2kg /sm2ha
 
     :           + (sum_real_array (g_dm_senesced, max_part)
     :           - g_dm_senesced(root)) * gm2kg /sm2ha
 
     :           + (sum_real_array (g_dm_dead, max_part)
     :           - g_dm_dead(root)) * gm2kg /sm2ha
 
 
                ! report
 
         write (string, '(3x, a, f7.1, a)')
     :                  ' kill. Standing above-ground dm = '
     :                  , biomass, ' (kg/ha)'
         call Write_string (string)
 
      else
      endif
 
      call pop_routine (my_name)
      return
      end



* ====================================================================
       subroutine sorg_harvest_root_incorp (option)
* ====================================================================
      use SorgModule
      implicit none
      include 'const.inc'
      include 'crp_comm.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
       integer option

*+  Purpose
*     <insert here>

*+  Changes
*     02-05-1997 - huth - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'sorg_harvest_root_incorp')

*+  Local Variables
      real       dm_root               ! dry matter added to soil (g/m^2)
      real       N_root                ! nitrogen added to soil (g/m^2)

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      if (Option .eq. 1) then
         dm_root = g%dm_green(root)
     :           + g%dm_dead(root)
     :           + g%dm_senesced(root)
 
         N_root  = g%N_green(root)
     :           + g%N_dead(root)
     :           + g%N_senesced(root)
 
         call sorg_root_incorp (
     .          g%root_depth,
     .          g%dlayer,
     .          c%crop_type,
     .          dm_root, N_root)
 
      elseif (Option .eq. 2) then
         dm_root = g%dm_green(root)
     :           + g%dm_dead(root)
     :           + g%dm_senesced(root)
 
         N_root  = g%N_green(root)
     :           + g%N_dead(root)
     :           + g%N_senesced(root)
 
         call crop_root_incorp (
     :          dm_root
     :         ,N_root
     :         ,g%dlayer
     :         ,g%root_length
     :         ,g%root_depth
     :         ,c%crop_type
     :         ,max_layer)
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (myname)
      return
      end



* ====================================================================
       subroutine sorg_harvest_top_residue (option)
* ====================================================================
      use SorgModule
      implicit none
      include 'const.inc'
      include 'data.pub'                          
      include 'crp_comm.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
       integer option

*+  Purpose
*     <insert here>

*+  Changes
*     02-05-1997 - huth - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'sorg_harvest_top_residue')

*+  Local Variables
      real       dm_residue            ! dry matter added to residue (g/m^2)
      real       N_residue             ! nitrogen added to residue (g/m^2)

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      if (Option .eq. 1) then
         dm_residue = (sum_real_array (g%dm_green, max_part)
     :              - g%dm_green(root) - g%dm_green(grain))
 
     :              + (sum_real_array (g%dm_senesced, max_part)
     :              - g%dm_senesced(root) - g%dm_senesced(grain))
 
     :              + (sum_real_array (g%dm_dead, max_part)
     :              - g%dm_dead(root) - g%dm_dead(grain))
 
         N_residue = (sum_real_array (g%N_green, max_part)
     :             - g%N_green(root) - g%N_green(grain))
 
     :             + (sum_real_array (g%N_senesced, max_part)
     :             - g%N_senesced(root) - g%N_senesced(grain))
 
     :             + (sum_real_array (g%N_dead, max_part)
     :             - g%N_dead(root) - g%N_dead(grain))
 
         call crop_top_residue (
     .                          c%crop_type
     .                         ,dm_residue
     .                         ,N_residue)
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (myname)
      return
      end



* ====================================================================
       subroutine sorg_daily_root_incorp (option)
* ====================================================================
      use SorgModule
      implicit none
      include 'const.inc'
      include 'crp_comm.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
       integer option

*+  Purpose
*     <insert here>

*+  Changes
*     02-05-1997 - huth - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'sorg_daily_root_incorp')

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      if (Option .eq. 1) then
          call sorg_root_incorp (
     .          g%root_depth,
     .          g%dlayer,
     .          c%crop_type,
     .          g%dlt_dm_detached(root),
     :          g%dlt_N_detached(root))
 
      elseif (Option .eq. 2) then
 
         call crop_root_incorp (
     .          g%dlt_dm_detached(root)
     :         ,g%dlt_N_detached(root)
     :         ,g%dlayer
     :         ,g%root_length
     :         ,g%root_depth
     :         ,c%crop_type
     :         ,max_layer)
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (myname)
      return
      end



* ====================================================================
       subroutine sorg_daily_top_residue (option)
* ====================================================================
      use SorgModule
      implicit none
      include 'const.inc'
      include 'data.pub'                          
      include 'crp_comm.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
       integer option

*+  Purpose
*     <insert here>

*+  Changes
*     02-05-1997 - huth - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'sorg_daily_top_residue')

*+  Local Variables
      real       dm_residue            ! dry matter added to residue (g/m^2)
      real       N_residue             ! nitrogen added to residue (g/m^2)

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      if (Option .eq. 1) then
         dm_residue = (sum_real_array (g%dlt_dm_detached, max_part)
     :              - g%dlt_dm_detached(root))
         N_residue = (sum_real_array (g%dlt_N_detached, max_part)
     :             - g%dlt_N_detached(root))
 
         call crop_top_residue (
     .                          c%crop_type
     .                         ,dm_residue
     .                         ,N_residue)
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (myname)
      return
      end



* ====================================================================
       subroutine sorg_prepare ()
* ====================================================================
      implicit none
      include 'error.pub'                         

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
      end



