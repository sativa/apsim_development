*     ===========================================================
      character*(*) function millet_version ()
*     ===========================================================

*   Short description:
*       Return version number of crop module

*   Assumptions:
*       none

*   Notes:
*   $Log$
*   Revision 1.1  1996/11/04 00:10:24  SidWright
*   Initial revision
*r  $
*      
*         Rev 1.7   19 Jul 1995 18:21:16   PVCSUSER
*      Added version keywords and lengthened version name

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*       011092 jngh specified and programmed
*       220696 jngh removed PVCS revision numbering

*   Calls:
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
*       none

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_version')

      character  version_number*(*)    ! version number of module
      parameter (version_number = 'V1.11 230696')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      millet_version = version_number 
cjh     :  '$Revision$$Date$'

      call pop_routine (my_name)
      return
      end
*     ================================================================
      subroutine APSIM_millet (action, data_string)
*     ================================================================

*   Short description:
*      This module performs crop crop growth
*       simulates root, leaf, head, stem and grain development. Water and
*       nitrogen uptake, photosynhesis, and leaf and root senescense.

*   Assumptions:
*      none

*   Notes:
*      none

*   Procedure attributes:
*      Version:         any hardware/fortran77
*      Extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       implicit none

*   Changes:
*      250894 jngh specified and programmed
*      220696 jngh added message_unused to else

*   Calls:
*     get_current_module
*     lastnb
*     set_fatal_off
*     set_fatal_on
*     millet_version
*     millet_get_other_variables
*     millet_init
*     millet_kill_crop
*     millet_my_type
*     millet_process
*     millet_send_my_variable
*     millet_set_my_variable
*     millet_set_other_variables
*     millet_start_crop
*     millet_zero_daily_variables
*     millet_zero_variables
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      character  action*(*)            ! (INPUT) Message action to perform
      character  data_string*(*)       ! (INPUT) Message data

*   Global variables
      include   'millet.inc'            !
      include   'const.inc'            ! mes_presence, mes_init, mes_process
                                       ! mes_report

      integer    lastnb                ! function
      logical    millet_my_type         ! function
      character  millet_version*52     ! function

*   Internal variables
      character  module_name*8         ! module name

*   Constant values
      character  my_name*(*)           ! name of this procedure
      parameter (my_name='millet')

*   Initial data values
*     none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

         ! initialise error flags
      call set_warning_off ()

      if (action.eq.mes_presence) then      ! report presence
         call get_current_module (module_name)
         write(*, *) 'module_name = '
     :              , module_name(:lastnb(module_name))
     :              // blank
     :              // millet_version ()

      elseif (action.eq.mes_init) then
            ! zero pools
         call millet_zero_variables ()
            ! Get constants
         call millet_init ()
            ! request and receive variables from owner-modules
         call millet_get_other_variables ()

      elseif (action.eq.mes_set_variable) then
            ! respond to request to reset variable values - from modules
         call millet_set_my_variable (data_string)

      elseif (action.eq.mes_get_variable) then
            ! respond to request for variable values - from modules
         call millet_send_my_variable (Data_string)

      elseif (action.eq.mes_prepare) then
            ! do nothing
      elseif (action.eq.mes_sow) then
         if (millet_my_type ()) then
               ! request and receive variables from owner-modules
            call millet_get_other_variables ()
               ! start crop and do  more initialisations
            call millet_start_crop ()
         else
            ! not my type!
            call message_unused ()
         endif

      elseif (action.eq.mes_initiate_crop) then
         if (millet_my_type ()) then
               ! request and receive variables from owner-modules
            call millet_get_other_variables ()
               ! start crop and do  more initialisations
            call millet_initiate ()
         else
            ! not my type!
            call message_unused ()
         endif

      elseif (action.eq.mes_process) then
         if (g_plant_status.ne.status_out) then
            call millet_zero_daily_variables ()
               ! request and receive variables from owner-modules
            call millet_get_other_variables ()
               ! do crop processes
            call millet_process ()
               ! send changes to owner-modules
            call millet_set_other_variables ()
         else
            ! crop not in
            call millet_zero_variables ()
         endif
      elseif (action.eq.mes_harvest) then
         if (millet_my_type ()) then
               ! harvest crop - turn into residue
              call millet_harvest ()
         else
            ! not my type!
            call message_unused ()
         endif

      elseif (action.eq.mes_end_crop) then
         if (millet_my_type ()) then
               ! end crop - turn into residue
            call millet_end_crop ()
         else
            ! not my type!
            call message_unused ()
         endif

      elseif (action.eq.mes_kill_crop) then
         if (millet_my_type ()) then
               ! kill crop - die
            call millet_kill_crop ()
         else
            ! not my type!
            call message_unused ()
         endif
      else
               ! don't use message
         call message_unused ()
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine millet_process ()
*     ===========================================================

*   Short description:
*       Simulate crop processes.  These include biomass production,
*       phenological stages, plant component development,
*       water uptake and nitrogen uptake, and plant senescense.

*   Assumptions:
*       none

*   Notes:
*       none

*   Procedure attributes:
*      Version:         any hardware/fortran77
*      Extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       implicit none

*   Changes:
*      250894 jngh specified and programmed

*   Calls:
*     millet_biomass
*     millet_check_bounds
*     millet_totals
*     millet_event
*     millet_leaf_area
*     millet_leaf_area_potential
*     millet_Nitrogen
*     millet_phenology
*     millet_plant_death
*     millet_senescence
*     millet_transpiration
*     millet_update
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'millet.inc'

*   Internal variables

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_process')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
c+!!!!!!!!! check order dependency of deltas
      call push_routine (my_name)

      call millet_transpiration ()
      call millet_phenology ()

      if (g_plant_status.eq.status_alive) then

         call millet_leaf_area_potential ()
         call millet_biomass ()
         call millet_leaf_area ()
         call millet_tillering ()
         call millet_senescence ()
         call millet_Nitrogen ()
         call millet_plant_death ()

      else
         ! crop is dead
      endif

      if (g_plant_status.eq.status_dead) then
            ! crop is dead
!cjngh         call millet_zero_globals ()
         call millet_dead ()

      else
            ! crop is alive
      endif
      
      call millet_detachment ()
      call millet_update ()
      call millet_check_bounds ()
      call millet_totals ()
      call millet_event ()

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine millet_dead ()
*     ===========================================================

*   Short description:
*       Set up states for dead crop

*   Assumptions:
*       none

*   Notes:
*       none

*   Procedure attributes:
*      Version:         any hardware/fortran77
*      Extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       implicit none

*   Changes:
*      091095 jngh specified and programmed

*   Calls:
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'millet.inc'

*   Internal variables

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_dead')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      g_current_stage   = real (plant_end)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine millet_harvest ()
*     ===========================================================

*   Short description:
*       Report occurence of harvest and the current status of specific
*       variables.

*   Assumptions:
*       none

*   Notes:
*       none

*   Procedure attributes:
*      Version:         any hardware/fortran77
*      Extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       implicit none

*   Changes:
*     010994 jngh specified and programmed

*   Calls:
*     divide
*     find_layer_no
*     l_bound
*     on_day_of
*     pop_routine
*     push_routine
*     report_event
*     millet_kill_crop
*     stage_is_between
*     sum_between
*     sum_real_array
*     write_string

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'const.inc'            ! new_line, lu_scr_sum, blank,
                                       ! lu_scr_sum
      include   'convert.inc'          ! gm2kg, sm2ha, sm2smm
      include   'millet.inc'

      real       divide                ! function
      real       sum_between           ! function
      real       sum_real_array        ! function

*   Internal variables
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

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_harvest')

*   Initial data values
*       none
* --------------------- Executable code section ----------------------

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


      call write_string (lu_scr_sum, new_line//new_line)

      write (string, '(a,i4,t40,a,f10.1)')
     :            ' flowering day  = ',g_isdate
     :          , ' stover (kg/ha) =',stover
      call write_string (lu_scr_sum, string)

      write (string, '(a,i4,t40,a,f10.1)')
     :            ' maturity day        = ', g_mdate
     :          , ' grain yield (kg/ha) =', yield
      call write_string (lu_scr_sum, string)

      write (string, '(a,f6.1,t40,a,f10.1)')
     :            ' grain % water content   = ', c_grn_water_cont
     :                                         * fract2pcnt
     :          , ' grain yield wet (kg/ha) =', yield_wet
      call write_string (lu_scr_sum, string)

      write (string, '(a,f10.3,t40,a,f10.3)')
     :            ' grain wt (g) =', grain_wt
     :          , ' grains/m^2   =', g_grain_no
      call write_string (lu_scr_sum, string)

      write (string, '(a,f6.1,t40,a,f6.3)')
     :            ' grains/head =', head_grain_no
     :          , ' maximum lai =', g_lai_max
      call write_string (lu_scr_sum, string)

      write (string, '(a,f10.1)')
     :            ' total above ground biomass (kg/ha) =', dm
      call write_string (lu_scr_sum, string)

      write (string, '(a,f10.1)')
     :         ' live above ground biomass (kg/ha) =', biomass_green
     :                                               + biomass_senesced
      call write_string (lu_scr_sum, string)

      write (string, '(a,f10.1)')
     :            ' green above ground biomass (kg/ha) =', biomass_green
      call write_string (lu_scr_sum, string)

      write (string, '(a,f10.1)')
     :      ' senesced above ground biomass (kg/ha) =', biomass_senesced
      call write_string (lu_scr_sum, string)

      write (string, '(a,f10.1)')
     :            ' dead above ground biomass (kg/ha) =', biomass_dead
      call write_string (lu_scr_sum, string)

      write (string, '(a,f6.1)')
     :            ' number of leaves =', leaf_no
      call write_string (lu_scr_sum, string)

      write (string, '(a,f10.2,t40,a,f10.2)')
     :            ' grain N percent =', N_grain_conc_percent
     :          , ' total N content (kg/ha) =', N_total
      call write_string (lu_scr_sum, string)

      write (string, '(a,f10.2,t40,a,f10.2)')
     :            ' grain N uptake (kg/ha) =', N_grain
     :          , ' senesced N content (kg/ha) =', N_senesced

      call write_string (lu_scr_sum, string)

      write (string, '(a,f10.2,t40,a,f10.2)')
     :            ' green N content (kg/ha) =', N_green
     :          , ' dead N content (kg/ha) =', N_dead
      call write_string (lu_scr_sum, string)

      do 2000 phase = emerg_to_endjuv, start_to_end_grain
         si1 = divide (g_cswd_photo(phase)
     :               , g_days_tot(phase), 0.0)
         si2 = divide (g_cswd_expansion(phase)
     :               , g_days_tot(phase), 0.0)
         si4 = divide (g_cnd_photo(phase)
     :               , g_days_tot(phase), 0.0)
         si5 = divide (g_cnd_grain_conc(phase)
     :               , g_days_tot(phase), 0.0)

         call write_string (lu_scr_sum, new_line//new_line)

         write (string,'(2a)')
     :         ' stress indices for ', c_stage_names(phase)
         call write_string (lu_scr_sum, string)

         write (string,'(2(a, f16.7))')
     :         ' water stress 1 =', si1
     :         , '   nitrogen stress 1 =', si4
         call write_string (lu_scr_sum, string)

         write (string,'(2(a, f16.7))')
     :         ' water stress 2 =', si2
     :         , '   nitrogen stress 2 =', si5
         call write_string (lu_scr_sum, string)
2000  continue

      g_dm_green(grain) = 0.0
      g_N_green(grain) = 0.0

      g_dm_dead(grain) = 0.0
      g_N_dead(grain) = 0.0

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine millet_zero_variables ()
*     ===========================================================

*   Short description:
*       Zero crop variables & arrays

*   Assumptions:
*       none

*   Notes:
*       none

*   Procedure attributes:
*      Version:         any hardware/fortran77
*      Extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       implicit none

*   Changes:
*     010994 jngh specified and programmed
*     090695 psc  add row spacing = 0

*   Calls:
*     fill_real_array
*     pop_routine
*     push_routine
*     millet_zero_daily_variables

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'millet.inc'

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'millet_zero_variables')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

          ! zero pools etc.

      call millet_zero_daily_variables ()

      call fill_real_array (g_cnd_grain_conc, 0.0, max_stage)
      call fill_real_array (g_cnd_photo, 0.0, max_stage)
      call fill_real_array (g_cswd_expansion, 0.0, max_stage)
      call fill_real_array (g_cswd_pheno, 0.0, max_stage)
      call fill_real_array (g_cswd_photo, 0.0, max_stage)
      call fill_real_array (g_days_tot, 0.0, max_stage)
      call fill_real_array (g_dm_dead, 0.0, max_part)
      call fill_real_array (g_dm_green, 0.0, max_part)
      call fill_real_array (g_dm_plant_min, 0.0, max_part)
      call fill_real_array (g_dm_plant_top_tot, 0.0, max_stage)
      call fill_real_array (g_heat_stress_tt, 0.0, max_stage)
      call fill_real_array (g_leaf_area, 0.0, max_leaf)
      call fill_real_array (g_leaf_area, 0.0, max_leaf)
      call fill_real_array (g_leaf_no, 0.0, max_stage)
      call fill_real_array (g_tiller_no, 0.0, max_stage)
      call fill_real_array (g_leaf_no_dead, 0.0, max_stage)
      call fill_real_array (p_ll_dep, 0.0, max_layer)
      call fill_real_array (g_N_conc_crit, 0.0, max_part)
      call fill_real_array (g_N_conc_min, 0.0, max_part)
      call fill_real_array (g_N_green, 0.0, max_part)
      call fill_real_array (g_phase_tt, 0.0, max_stage)
      call fill_real_array (g_tt_tot, 0.0, max_stage)
      call fill_real_array (g_phase_tt_curv, 0.0, max_stage)
      call fill_real_array (g_tt_curv_tot, 0.0, max_stage)
      call fill_real_array (g_phase_tt_other, 0.0, max_stage)
      call fill_real_array (g_tt_other_tot, 0.0, max_stage)
      call fill_real_array (g_lai_equilib_light, 0.0, 366)
      call fill_real_array (g_lai_equilib_water, 0.0, 366)
      call fill_real_array (g_soil_temp, 0.0, 366)

      call fill_real_array (g_dm_senesced, 0.0, max_part)
      call fill_real_array (g_dm_stress_max, 0.0, max_stage)
      call fill_real_array (g_N_dead, 0.0, max_part)
      call fill_real_array (g_N_senesced, 0.0, max_part)


      g_num_layers = 0
      g_canopy_height = 0.0
      g_grain_no = 0.0
      g_isdate = 0
      g_mdate = 0
      g_leaf_no_final = 0.0
      g_lai_max = 0.0
      g_N_conc_act_stover_tot = 0.0
      g_N_conc_crit_stover_tot = 0.0
      g_N_demand_tot = 0.0
      g_N_uptake_grain_tot = 0.0
      g_N_uptake_stover_tot = 0.0
      g_N_uptake_tot = 0.0
      g_plants = 0.0
      g_root_depth = 0.0
      g_sowing_depth = 0.0
!cpsc      
      g_row_spacing = 0.0
cjh
      g_cover_green = 0.0
      g_cover_sen   = 0.0
      g_cover_dead  = 0.0
!cpsc      
      g_slai = 0.0
      g_lai = 0.0
      g_tlai_dead = 0.0
      g_transpiration_tot = 0.0
      g_previous_stage = 0.0

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine millet_zero_daily_variables ()
*     ===========================================================

*   Short description:
*       Zero crop daily variables & arrays

*   Assumptions:
*       none

*   Notes:
*       none

*   Procedure attributes:
*      Version:         any hardware/fortran77
*      Extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       implicit none

*   Changes:
*     010994 jngh specified and programmed

*   Calls:
*     fill_real_array
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'millet.inc'

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'millet_zero_daily_variables')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

          ! zero pools etc.

      call fill_real_array (g_dlt_dm_green, 0.0, max_part)
      call fill_real_array (g_dlt_dm_green_retrans, 0.0, max_part)
      call fill_real_array (g_dlt_N_green, 0.0, max_part)
      call fill_real_array (g_dlt_N_retrans, 0.0, max_part)
      call fill_real_array (g_dlt_NO3gsm, 0.0, max_layer)
      call fill_real_array (g_dlt_sw_dep, 0.0, max_layer)
      call fill_real_array (g_dm_green_demand, 0.0, max_part)
      call fill_real_array (g_N_demand, 0.0, max_part)

      call fill_real_array (g_dlt_dm_dead_detached, 0.0, max_part)
      call fill_real_array (g_dlt_dm_detached, 0.0, max_part)
      call fill_real_array (g_dlt_dm_senesced, 0.0, max_part)
      call fill_real_array (g_dlt_N_dead_detached, 0.0, max_part)
      call fill_real_array (g_dlt_N_detached, 0.0, max_part)
      call fill_real_array (g_dlt_N_senesced, 0.0, max_part)
      call fill_real_array (g_sw_avail, 0.0, max_layer)
      call fill_real_array (g_sw_avail_pot, 0.0, max_layer)
      call fill_real_array (g_sw_supply, 0.0, max_layer)
      
      g_dlt_tiller_no = 0.0
      g_dm_tiller_independence = 0.0 
      g_N_tiller_independence = 0.0
      g_tiller_independence = 0

      g_dlt_tlai_dead_detached = 0.0
      g_dlt_slai_detached = 0.0
      g_dlt_canopy_height = 0.0
      g_dlt_dm = 0.0
      g_dlt_dm_grain_demand = 0.0
      g_dlt_dm_stress_max = 0.0
      g_dlt_heat_stress_tt = 0.0
      g_dlt_leaf_no = 0.0
      g_dlt_leaf_no_pot = 0.0
      g_dlt_leaf_no_dead = 0.0
      g_dlt_plants = 0.0
      g_dlt_root_depth = 0.0
      g_dlt_slai = 0.0
      g_dlt_stage = 0.0
      g_dlt_lai = 0.0
      g_dlt_tt = 0.0
      g_dlt_tt_curv = 0.0
      g_dlt_tt_other = 0.0
      g_sw_demand = 0.0

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine millet_init ()
*     ===========================================================

*   Short description:
*       Crop initialisation

*   Assumptions:
*       none

*   Notes:
*       none

*   Procedure attributes:
*      Version:         any hardware/fortran77
*      Extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       implicit none

*   Changes:
*     010994 jngh specified and programmed

*   Calls:
*     pop_routine
*     push_routine
*     report_event
*     millet_read_constants
*     millet_version

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'millet.inc'

      character  millet_version*52      ! function

*   Internal variables
*       none

*   Constant values

      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'millet_init')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      call report_event (' Initialising, '
     :                  // millet_version ())

           ! initialize crop variables

      call millet_read_constants ()

      g_current_stage = real (plant_end)
      g_plant_status = status_out

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine millet_start_crop ()
*     ===========================================================

*   Short description:
*       Start crop using parameters specified in passed record

*   Assumptions:
*       none

*   Notes:
*       none

*   Procedure attributes:
*      Version:         any hardware/fortran77
*      Extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       implicit none

*   Changes:
*     010994 jngh specified and programmed
*     090695 psc  add row spacing read
*     220696 jngh changed extract to collect

*   Calls:
*     collect_char_var
*     collect_real_var
*     collect_real_var_optional
*     fatal_error
*     pop_routine
*     push_routine
*     millet_read_cultivar_params
*     millet_read_root_params
*     write_string

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'const.inc'            ! lu_scr_sum, blank
      include   'millet.inc'

*   Internal variables
      integer    numvals               ! number of values found in array
      character  string*200            ! output string

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'millet_start_crop')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      call report_event ( 'Sow')

cjh      if (data_record.ne.blank) then

         call collect_real_var ('plants', '()'
     :                        , g_plants, numvals, 0.0, 100.0)
!cpsc
         call collect_real_var_optional (
     :                          'row_spacing', '(m)'
     :                        , g_row_spacing, numvals
     :                        , 0.0, 2.0)

         call collect_real_var (
     :                          'sowing_depth', '(mm)'
     :                        , g_sowing_depth, numvals
     :                        , 0.0, 100.0)

         call collect_char_var ('cultivar', '()'
     :                        , g_cultivar, numvals)

             ! report

         call write_string (lu_scr_sum, new_line//new_line)

         string = '                 Crop Sowing Data'
         call write_string (lu_scr_sum, string)

         string = '    ------------------------------------------------'
         call write_string (lu_scr_sum, string)
!cpsc
         call write_string (lu_scr_sum
     :                    , '    Sowing  Depth Plants Spacing Cultivar')
!cpsc
         call write_string (lu_scr_sum
     :                    , '    Day no   mm     m^2     m     Name   ')

         string = '    ------------------------------------------------'
         call write_string (lu_scr_sum, string)
!cpsc
         write (string, '(3x, i7, 3f7.1, 1x, a10)')
     :                   g_day_of_year, g_sowing_depth
     :                 , g_plants, g_row_spacing, g_cultivar
         call write_string (lu_scr_sum, string)

         string = '    ------------------------------------------------'
         call write_string (lu_scr_sum, string)

                 ! get cultivar parameters

         call millet_read_cultivar_params ()

                 ! get root profile parameters

         call millet_read_root_params ()

         g_current_stage = real (sowing)
         g_plant_status = status_alive

cjh      else
            ! report empty sowing record
cjh         call fatal_error (err_user, 'No sowing criteria supplied')
cjh      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine millet_initiate ()
*     ===========================================================

*   Short description:
*       Initiate crop, tillers or other axes using parameters 
*       specified in passed record.

*   Assumptions:
*       none

*   Notes:
*       none

*   Procedure attributes:
*      Version:         any hardware/fortran77
*      Extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       implicit none

*   Changes:
*     091095 jngh specified and programmed
*     220696 jngh changed extract to collect

*   Calls:
*     collect_char_var
*     collect_real_var
*     collect_real_var_optional
*     fatal_error
*     pop_routine
*     push_routine
*     millet_read_cultivar_params
*     millet_read_root_params
*     write_string

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'const.inc'            ! lu_scr_sum, blank
      include   'millet.inc'

      real       divide                ! function
      
*   Internal variables
      integer    numvals               ! number of values found in array
      character  string*200            ! output string
      real       N_tiller_plant        ! N content of tiller (g/plant)

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'millet_initiate')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      call report_event ( 'Initiate')

cjh      if (data_record.ne.blank) then

         call collect_real_var ('plants', '()'
     :                        , g_plants, numvals, 0.0, 100.0)

         call collect_real_var_optional (
     :                          'dm_tiller_plant', '(g/plant)'
     :                        , c_dm_leaf_init, numvals
     :                        , 0.0, 100.0)

         call collect_real_var_optional (
     :                          'n_tiller_plant', '(g/plant)'
     :                        , N_tiller_plant, numvals
     :                        , 0.0, 10.0)

         call collect_real_var_optional (
     :                          'row_spacing', '(m)'
     :                        , g_row_spacing, numvals
     :                        , 0.0, 2.0)

         call collect_char_var ('cultivar', '()'
     :                        , g_cultivar, numvals)

             ! report

         call write_string (lu_scr_sum, new_line//new_line)

         string = '                 Crop Initiate Data'
         call write_string (lu_scr_sum, string)

         string = '    ------------------------------------------------'
         call write_string (lu_scr_sum, string)
         call write_string (lu_scr_sum
     :            , '   Initiate   Axes   DM      N   Spacing Cultivar')
         call write_string (lu_scr_sum
     :            , '    Day no    m^2 g/plant g/plant   m      Name  ')

         string = '    ------------------------------------------------'
         call write_string (lu_scr_sum, string)
         write (string, '(3x, i7, 4f7.1, 1x, a10)')
     :                   g_day_of_year
     :                 , g_plants, c_dm_leaf_init, N_tiller_plant
     :                 , g_row_spacing, g_cultivar
         call write_string (lu_scr_sum, string)

         string = '    ------------------------------------------------'
         call write_string (lu_scr_sum, string)

                 ! get cultivar parameters

         call millet_read_cultivar_params ()

                 ! get root profile parameters

         call millet_read_root_params ()
         
         c_N_leaf_init_conc = divide (N_tiller_plant
     :                              , c_dm_leaf_init, 0.0)
         c_dm_root_init = c_dm_leaf_init

cjh            this need to be changed back to emergence again. A temporary
cjh            fix to overcome problems of variables being calculated between
cjh            germ and emerg (leaf_no_final and phase_tt(germ_to emerg &
cjh            emerg_to_endjuv).
cjh         g_current_stage = real (emerg)

         g_current_stage = real (germ)
         g_plant_status = status_alive

cjh      else
            ! report empty sowing record
cjh         call fatal_error (err_user, 'No initiate criteria supplied')
cjh      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine millet_read_cultivar_params ()
*     ===========================================================

*   Short description:
*       Get cultivar parameters for named cultivar, from crop parameter file.

*   Assumptions:
*       none

*   Notes:
*       none

*   Procedure attributes:
*      Version:         any hardware/fortran77
*      Extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       implicit none

*   Changes:
*       090994 jngh specified and programmed

*   Calls:
*     pop_routine
*     push_routine
*     read_integer_var
*     read_real_var
*     write_string

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'const.inc'            ! new_line, lu_scr_sum, blank
                                       ! lu_src_sum
      include   'millet.inc'

*   Internal variables
      character  string*200            ! output string
      integer    numvals               ! number of values read

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_read_cultivar_params')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      call write_string (lu_scr_sum
     :                 ,new_line//'   - Reading Cultivar Parameters')

         ! TEMPLATE OPTION
         !   millet_leaf_area_devel_plant

      ! call read_real_var (g_cultivar
      !:                    , 'tpla_prod_coef', '(????)'
      !:                    , p_tpla_prod_coef, numvals
      !:                    , 0.0, 10.0)

      ! call read_real_var (g_cultivar
      !:                    , 'tpla_inflection', '(????)'
      !:                    , p_tpla_inflection, numvals
      !:                    , 0.0, 10.0)

      ! call read_real_var (g_cultivar
      !:                    , 'tiller_no_fertile', '(????)'
      !:                    , p_tiller_no_fertile, numvals
      !:                    , 0.0, 100.0)

         ! TEMPLATE OPTION
         !   millet_leaf_area_sen_age1

      ! call read_real_var (g_cultivar
      !:                    , 'spla_prod_coef', '(????)'
      !:                    , p_spla_prod_coef, numvals
      !:                    , 0.0, 100.0)

      ! call read_real_var (g_cultivar
      !:                    , 'spla_intercept', '(????)'
      !:                    , p_spla_intercept, numvals
      !:                    , 0.0, 100.0)

         ! TEMPLATE OPTION
         !       millet_dm_grain_hi

      ! call read_real_var (g_cultivar
      !:                    , 'hi_incr', '()'
      !:                    , p_hi_incr, numvals
      !:                    , 0.0, 1.0)

      call read_real_var (g_cultivar
     :                    , 'hi_max_pot', '()'
     :                    , p_hi_max_pot, numvals
     :                    , 0.0, 1.0)

         ! TEMPLATE OPTION
         !   millet_check_grain_no  millet_grain_no

      call read_real_var (g_cultivar
     :                    , 'head_grain_no_max', '()'
     :                    , p_head_grain_no_max, numvals
     :                    , 0.0, c_head_grain_no_max_ub)

         ! TEMPLATE OPTION
         !   millet_dm_grain

      call read_real_var (g_cultivar
     :                    , 'grain_gth_rate', '()'
     :                    , p_grain_gth_rate, numvals
     :                    , 0.0, c_grain_gth_rate_ub)

         !   millet_phenology_init

      call read_real_var (g_cultivar
     :                    , 'tt_emerg_to_endjuv', '()'
     :                    , p_tt_emerg_to_endjuv, numvals
     :                    , 0.0, c_tt_emerg_to_endjuv_ub)

      call read_integer_var (g_cultivar
     :                    , 'est_days_emerg_to_init', '()'
     :                    , p_est_days_emerg_to_init, numvals
     :                    , 0, 100)

      call read_real_var (g_cultivar
     :                    , 'pp_endjuv_to_init', '()'
     :                    , p_pp_endjuv_to_init, numvals
     :                    , 0.0, c_pp_endjuv_to_init_ub)

      call read_real_var (g_cultivar
     :                    , 'tt_flower_to_maturity', '()'
     :                    , p_tt_flower_to_maturity, numvals
     :                    , 0.0, c_tt_flower_to_maturity_ub)

      call read_real_var (g_cultivar
     :                    , 'tt_flag_to_flower', '()'
     :                    , p_tt_flag_to_flower, numvals
     :                    , 0.0, c_tt_flag_to_flower_ub)

      call read_real_var (g_cultivar
     :                    , 'tt_flower_to_start_grain', '()'
     :                    , p_tt_flower_to_start_grain, numvals
     :                    , 0.0, c_tt_flower_to_start_grain_ub)


      call read_real_var (g_cultivar
     :                    , 'tt_maturity_to_ripe', '()'
     :                    , p_tt_maturity_to_ripe, numvals
     :                    , 0.0, c_tt_maturity_to_ripe_ub)


             ! report

      string = '    ------------------------------------------------'
      call write_string (lu_scr_sum, string)

      write (string, '(4x,2a)')
     :                'Cultivar                 = ', g_cultivar
      call write_string (lu_scr_sum, string)

      write (string, '(4x, a, f7.1)')
     :                'est_days_emerg_to_init  = '
     :               , p_est_days_emerg_to_init
      call write_string (lu_scr_sum, string)

      write (string, '(4x, a, f7.1)')
     :                'tt_emerg_to_endjuv       = '
     :               , p_tt_emerg_to_endjuv
      call write_string (lu_scr_sum, string)

      write (string, '(4x, a, f7.1)')
     :                'pp_endjuv_to_initp       = '
     :               , p_pp_endjuv_to_init
      call write_string (lu_scr_sum, string)

      write (string, '(4x, a, f7.1)')
     :                'tt_flower_to_maturity    = '
     :               , p_tt_flower_to_maturity
      call write_string (lu_scr_sum, string)

      write (string, '(4x, a, f7.1)')
     :                'head_grain_no_max        = '
     :               , p_head_grain_no_max
      call write_string (lu_scr_sum, string)

      write (string, '(4x, a, f7.1)')
     :                'grain_gth_rate           = '
     :               , p_grain_gth_rate
      call write_string (lu_scr_sum, string)

      write (string, '(4x, a, f7.1)')
     :                'tt_flag_to_flower        = '
     :               , p_tt_flag_to_flower
      call write_string (lu_scr_sum, string)

      write (string, '(4x, a, f7.1)')
     :                'tt_flower_to_start_grain = '
     :               , p_tt_flower_to_start_grain
      call write_string (lu_scr_sum, string)

      write (string, '(4x, a, f7.1)')
     :                'tt_maturity_to_ripe      = '
     :               , p_tt_maturity_to_ripe
      call write_string (lu_scr_sum, string)

         ! TEMPLATE OPTION
      ! write (string, '(4x, a, f7.1)')
      !:                'hi_incr                  = '
      !:               , p_hi_incr
      ! call write_string (lu_scr_sum, string)

         ! TEMPLATE OPTION
      ! write (string, '(4x, a, f7.1)')
      !:                'hi_max_pot                   = '
      !:               , p_hi_max_pot
      ! call write_string (lu_scr_sum, string)

         ! TEMPLATE OPTION
      ! write (string, '(4x, a, f7.1)')
      !:                'tpla_prod_coef           = '
      !:               , p_tpla_prod_coef
      ! call write_string (lu_scr_sum, string)

         ! TEMPLATE OPTION
      ! write (string, '(4x, a, f7.1)')
      !:                'tpla_inflection          = '
      !:               , p_tpla_inflection
      ! call write_string (lu_scr_sum, string)

         ! TEMPLATE OPTION
      ! write (string, '(4x, a, f7.1)')
      !:                'tiller_no_fertile        = '
      !:               , p_tiller_no_fertile
      ! call write_string (lu_scr_sum, string)

         ! TEMPLATE OPTION
      ! write (string, '(4x, a, f7.1)')
      !:                'spla_prod_coef           = '
      !:               , p_spla_prod_coef
      ! call write_string (lu_scr_sum, string)

         ! TEMPLATE OPTION
      ! write (string, '(4x, a, f7.1)')
      !:                'spla_intercept           = '
      !:               , p_spla_intercept
      ! call write_string (lu_scr_sum, string)

      string = '    ------------------------------------------------'
      call write_string (lu_scr_sum, string)

      call write_string (lu_scr_sum, new_line//new_line)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine millet_read_root_params ()
*     ===========================================================

*   Short description:
*       Get root profile parameters

*   Assumptions:
*       none

*   Notes:
*       none

*   Procedure attributes:
*      Version:         any hardware/fortran77
*      Extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       implicit none

*   Changes:
*       090994 jngh specified and programmed
*     210395 jngh changed from millet_section to a parameters section

*   Calls:
*     fill_real_array
*     pop_routine
*     push_routine
*     read_real_array
*     write_string

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'const.inc'            ! new_line, lu_scr_sum, blank,
                                       ! lu_scr_sum
      include   'millet.inc'            ! dlayer(max_layer)

*   Internal variables
      integer    layer                 ! layer number
      real       ll (max_layer)        ! lower limit of plant-extractable
                                       ! soil water for soil layer l
                                       ! (mm water/mm soil)
      integer    num_layers            ! number of layers in profile
      character  string*200            ! output string

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_read_root_params')

      character  section_name*(*)
      parameter (section_name = 'parameters')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      call write_string (lu_scr_sum
     :                  ,new_line
     :                  //'   - Reading root profile parameters')

         !       millet_sw_supply

      call read_real_array (section_name
     :                     , 'll', max_layer, '()'
     :                     , ll, num_layers
     :                     , 0.0, c_ll_ub)

      call fill_real_array (p_ll_dep, 0.0, max_layer)
      do 1000 layer = 1, num_layers
         p_ll_dep(layer) = ll(layer)*g_dlayer(layer)
1000  continue

      call read_real_array (section_name
     :                     , 'kl', max_layer, '()'
     :                     , p_kl, num_layers
     :                     , 0.0, c_kl_ub)


          ! report
      call write_string (lu_scr_sum, new_line//new_line)

      write (string,'(4x, a)') '                Root Profile'
      call write_string (lu_scr_sum, string)

      string = '    ------------------------------------------------'
      call write_string (lu_scr_sum, string)

      string = '      Layer depth  Kl factor   Lower limit'
      call write_string (lu_scr_sum, string)

      string = '         (mm)         ()        (mm/mm)'
      call write_string (lu_scr_sum, string)

      string = '    ------------------------------------------------'
      call write_string (lu_scr_sum, string)

      do 2000 layer = 1, num_layers
         write (string,'(3x, 3f12.3)')
     :            g_dlayer(layer)
     :          , p_kl(layer)
     :          , ll(layer)
         call write_string (lu_scr_sum, string)
2000  continue

      string = '     ------------------------------------------------'
      call write_string (lu_scr_sum, string)

      call write_string (lu_scr_sum, new_line//new_line)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine millet_end_crop ()
*     ===========================================================

*   Short description:
*       End crop

*   Assumptions:
*       none

*   Notes:
*       none

*   Procedure attributes:
*      Version:         any hardware/fortran77
*      Extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       implicit none

*   Changes:
*       290994 jngh specified and programmed

*   Calls:
*     pop_routine
*     push_routine
*     report_event
*     millet_top_residue
*     millet_root_incorp
*     millet_root_distrib
*     sum_real_array
*     write_string

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'const.inc'            ! new_line, lu_scr_sum
      include   'convert.inc'          ! gm2kg, sm2ha, mm2cm, cmm2cc
      include   'millet.inc'

      real       sum_real_array        ! function

*   Internal variables
      real       dm_residue            ! dry matter added to residue (g/m^2)
      real       N_residue             ! nitrogen added to residue (g/m^2)
      real       dm_root               ! dry matter added to soil (g/m^2)
      real       N_root                ! nitrogen added to soil (g/m^2)
      character  string*400            ! output string
      real       yield                 ! grain wt (kg/ha)

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'millet_end_crop')

*   Initial data values
*     none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      if (g_plant_status.ne.status_out) then
         g_plant_status = status_out
         g_current_stage = real (plant_end)

                ! report

         yield = (g_dm_green(grain) + g_dm_dead(grain)) *gm2kg /sm2ha
         write (string, '(3x, a, f7.1)')
     :                  ' ended. Yield (dw) = ', yield
         call report_event (string)

             ! now do post harvest processes

         dm_root = g_dm_green(root) 
     :           + g_dm_dead(root) 
     :           + g_dm_senesced(root)

         N_root  = g_N_green(root) 
     :           + g_N_dead(root) 
     :           + g_N_senesced(root)

         call millet_root_incorp (dm_root, N_root)

             ! put stover into surface residue

         dm_residue = (sum_real_array (g_dm_green, max_part)
     :              - g_dm_green(root) - g_dm_green(grain))

     :              + (sum_real_array (g_dm_senesced, max_part)
     :              - g_dm_senesced(root) - g_dm_senesced(grain))

     :              + (sum_real_array (g_dm_dead, max_part)
     :              - g_dm_dead(root) - g_dm_dead(grain))

         N_residue = (sum_real_array (g_N_green, max_part)
     :             - g_N_green(root) - g_N_green(grain))

     :             + (sum_real_array (g_N_senesced, max_part)
     :             - g_N_senesced(root) - g_N_senesced(grain))

     :             + (sum_real_array (g_N_dead, max_part)
     :             - g_N_dead(root) - g_N_dead(grain))

         call millet_top_residue (dm_residue, N_residue)

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

         call write_string (lu_scr_sum, string)

      else
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine millet_store_value (array, value)
*     ===========================================================
*   Short description:
*       Stores a value in an annual circular array

*   Assumptions:
*       none

*   Notes:
*       none

*   Procedure attributes:
*      Version:         any hardware/fortran77
*      Extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       implicit none

*   Changes:
*     230695 jngh specified and programmed

*   Calls:
*     divide
*     leap_year
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       array(*)              ! (OUTPUT) storage array
      real       value                 ! (INPUT) value to be stored
      
*   Global variables
      include   'millet.inc'

      logical    leap_year             ! function

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_store_value')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

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
*     ================================================================
      subroutine millet_get_other_variables ()
*     ================================================================

*   Short description:
*      Get the values of variables/arrays from other modules.

*   Assumptions:
*      assumes variable has the following format
*         <variable_name> = <variable_value/s> (<units>)

*   Notes:
*      none

*   Procedure attributes:
*      Version:         any hardware/fortran77
*      Extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       implicit none

*   Changes:
*     010994 jngh specified and programmed
*     220696 jngh optimised order of gets

*   Calls:
*     add_real_array
*     divide
*     fill_real_array
*     get_integer_var
*     get_real_array
*     get_real_array_optional
*     get_real_var
*     get_real_var_optional
*     millet_store_value
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
*      none

*   Global variables
      include   'const.inc'
      include   'convert.inc'
      include   'millet.inc'

      real       divide                ! function

*   Internal variables
      integer    layer                 ! layer number
      integer    numvals               ! number of values put into array
      real       dlayer(max_layer)     ! soil layer depths (mm)
      real       NO3(max_layer)        ! soil NO3 content (kg/ha)
      real       NO3_min(max_layer)    ! soil NO3 minimum (kg/ha)
      real       soil_temp             ! soil surface temperature (oC)
      
*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_get_other_variables')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

            ! date

      call get_integer_var (unknown_module, 'day', '()'
     :                                    , g_day_of_year, numvals
     :                                    , 1, 366)

      call get_integer_var (unknown_module, 'year', '()'
     :                                    , g_year, numvals
     :                                    , c_year_lb, c_year_ub)

                               ! canopy
      call get_real_var_optional (unknown_module, 'fr_intc_radn', '()'
     :                                  , g_fr_intc_radn, numvals
     :                                  , 0.0, 1.0)

                                ! climate
      call get_real_var (unknown_module, 'latitude', '(oL)'
     :                                  , g_latitude, numvals
     :                                  , c_latitude_lb, c_latitude_ub)

      call get_real_var (unknown_module, 'maxt', '(oC)'
     :                                  , g_maxt, numvals
     :                                  , c_maxt_lb, c_maxt_ub)

      call get_real_var (unknown_module, 'mint', '(oC)'
     :                                  , g_mint, numvals
     :                                  , c_mint_lb, c_mint_ub)

      call get_real_var (unknown_module, 'radn', '(Mj/m^2)'
     :                                  , g_radn, numvals
     :                                  , c_radn_lb, c_radn_ub)
!cpsc
      call get_real_var_optional (unknown_module, 'soil_temp', '(oC)'
     :                                  , soil_temp, numvals
     :                                  , 0.0, 80.0)

      if (numvals.eq.0) then
         ! soil temp not supplied
      else
         call millet_store_value (g_soil_temp, soil_temp)
      endif

                               ! canopy
      call get_real_var_optional (unknown_module, 'fr_intc_radn', '()'
     :                                  , g_fr_intc_radn, numvals
     :                                  , 0.0, 1.0)

c+!!!!!!!! what to do if no waterbalance variables found
            ! soil profile and soil water

      call get_real_array (unknown_module, 'dlayer', max_layer
     :                                    , '(mm)'
     :                                    , dlayer, numvals
     :                                    , c_dlayer_lb, c_dlayer_ub)

      if (g_num_layers.eq.0) then
            ! we assume dlayer hasn't been initialised yet.
         call add_real_array (dlayer, g_dlayer, numvals)
         g_num_layers = numvals

      else
            ! dlayer may be changed from its last setting
         do 1000 layer = 1, numvals
            p_ll_dep(layer) = divide (p_ll_dep(layer)
     :                              , g_dlayer(layer), 0.0)
     :                      * dlayer(layer)

            g_dlayer(layer) = dlayer(layer)
1000     continue
         g_num_layers = numvals
      endif

      call get_real_array (unknown_module, 'dul_dep', max_layer
     :                                    , '(mm)'
     :                                    , g_dul_dep, numvals
     :                                    , c_dul_dep_lb, c_dul_dep_ub)

      call get_real_array (unknown_module, 'sw_dep', max_layer
     :                                    , '(mm)'
     :                                    , g_sw_dep, numvals
     :                                    , c_sw_dep_lb, c_sw_dep_ub)

                                ! soil nitrogen pools
      call get_real_array_optional (unknown_module, 'no3', max_layer
     :                                  ,  '(kg/ha)'
     :                                  , NO3, numvals
     :                                  , c_NO3_lb, c_NO3_ub)
      if (numvals.eq.0) then
            ! we have no N supply - make non-limiting.
         call fill_real_array (NO3, 10000.0, g_num_layers)
      else
      endif
      do 2000 layer = 1, g_num_layers
         g_NO3gsm(layer) = NO3(layer) * kg2gm /ha2sm
2000  continue      


      call get_real_array_optional (unknown_module, 'no3_min',max_layer
     :                                  ,  '(kg/ha)'
     :                                  , NO3_min, numvals
     :                                  , c_NO3_min_lb, c_NO3_min_ub)
      do 3000 layer = 1, g_num_layers
         g_NO3gsm_min(layer) = NO3_min(layer) * kg2gm /ha2sm
3000  continue      

      call pop_routine (my_name)
      return
      end
*     ================================================================
      subroutine millet_set_other_variables ()
*     ================================================================

*   Short description:
*      Set the value of a variable or array in other module/s.

*   Assumptions:
*      none

*   Notes:
*      a flag is set if any of the totals is requested.  The totals are
*      reset during the next process phase when this happens.

*   Procedure attributes:
*      Version:         any hardware/fortran77
*      Extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       implicit none

*   Changes:
*     010994 jngh specified and programmed
*     240696 jngh changed set_ to post_ construct

*   Calls:
*     count_of_real_vals
*     pop_routine
*     push_routine
*     set_real_array

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*      none

*   Global variables
      include   'const.inc'
      include   'convert.inc'
      include   'millet.inc'

      integer    count_of_real_vals    ! function

*   Internal variables
      real       dlt_NO3(max_layer)    ! soil NO3 change (kg/ha)
      integer    layer                 ! soil layer no.
      integer    num_layers            ! number of layers

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_set_other_variables')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

c+!!!! perhaps we should get number of layers at init and keep it
      num_layers = count_of_real_vals (g_dlayer, max_layer)

      do 1000 layer = 1, num_layers
         dlt_NO3(layer) = g_dlt_NO3gsm(layer) * gm2kg /sm2ha
1000  continue      

      call new_postbox()
      call post_real_array ('dlt_no3', '(kg/ha)'
     :                    , dlt_NO3, num_layers)

      call post_real_array ('dlt_sw_dep', '(mm)'
     :                    , g_dlt_sw_dep, num_layers)

      call message_send_immediate (unknown_module
     :                               ,Mes_set_variable
     :                               ,'dlt_no3')
      call message_send_immediate (unknown_module
     :                               ,Mes_set_variable
     :                               ,'dlt_sw_dep')
      call delete_postbox()

      call pop_routine (my_name)
      return
      end
*     ===============================================================
      subroutine millet_set_my_variable (Variable_name)
*     ===============================================================

*   Short description:
*      Set a variable in this module as requested by another.

*   Assumptions:
*      none

*   Notes:
*      none

*   Procedure attributes:
*      Version:         any hardware/fortran77
*      Extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       implicit none

*   Changes:
*      290393 jngh
*      220696 jngh added message_unused to else
*                  changed respond2set to collect

*   Calls:
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      character  Variable_name*(*)     ! (INPUT) Variable name to search for

*   Global variables
*      none

*   Internal variables
*      none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_set_my_variable')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

             ! **** Repeat for each variable

*      if (variable_name .eq. '????') then
*         call collect_real_array (variable_name, '()', max_layer
*     :                               , ????, numvals
*     :                               , 0.0, 1.0)

*      else
            ! Don't know this variable name
         call message_unused ()
*      endif

      call pop_routine (my_name)
      return
      end
*     ================================================================
      subroutine millet_send_my_variable (variable_name)
*     ================================================================

*   Short description:
*      Return the value of a variable requested by other modules.

*   Assumptions:
*      none

*   Notes:
*      none

*   Procedure attributes:
*      Version:         any hardware/fortran77
*      Extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       implicit none

*   Changes:
*      string_concat
*      090495 psc  added nfact to output list
*      170495 psc  added grain_size, yield, biomass to output list
*      220696 jngh added message_unused to else

*   Calls:
*     count_of_real_vals
*     divide
*     find_layer_no
*     l_bound
*     pop_routine
*     push_routine
*     respond2get_char_var
*     respond2get_integer_var
*     respond2get_real_array
*     respond2get_real_var
*     millet_swdef
*     sum_between
*     sum_real_array
*     millet_nfact

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      character variable_name*(*)      ! (INPUT) variable name to search for

*   Global variables
      include   'convert.inc'          ! gm2kg, sm2ha, mm2cm, cmm2cc
      include   'millet.inc'

      integer    count_of_real_vals    ! function
      real       day_length            ! function
      real       divide                ! function
      integer    find_layer_no         ! function
      real       l_bound               ! function
      real       millet_swdef          ! function
      real       sum_real_array        ! function
      real       sum_between           ! function
      real       millet_nfact          ! function
      real       millet_N_fixation     ! function

*   Internal variables
      real       act_N_up              ! cumulative total N uptake by plant
                                       ! (kg/ha)
      real       ag_N_up               ! N uptake by grain (kg/ha)
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
      real       N_supply              ! N supply for grain (g/m^2)
      real       N_uptake_sum          ! N supply from soil
      real       grain_size            ! individual grain wt (g/grain)
      real       yield                 ! grain yield (kg/ha)
      real       biomass               ! total above-ground biomass (kg/ha)
      real       sw_supply_sum         ! total supply over profile (mm)
      integer    layer                 ! soil layer
      real       esw_layr(max_layer)   ! plant extractable soil water            !

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_send_my_variable')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)


            ! management

cjh
      if (variable_name .eq. 'plant_status') then
         call respond2get_char_var (variable_name
     :                             , '()'
     :                             , g_plant_status)

      elseif (variable_name .eq. 'stage') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g_current_stage)

      elseif (variable_name .eq. 'stage_code') then
         stage_no = int (g_current_stage)
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , c_stage_code_list(stage_no))

      elseif (variable_name .eq. 'stage_name') then
         stage_no = int (g_current_stage)
         call respond2get_char_var (variable_name
     :                             , '()'
     :                             , c_stage_names(stage_no))

      elseif (variable_name .eq. 'crop_type') then
         call respond2get_char_var (variable_name
     :                             , '()'
     :                             , c_crop_type)

      elseif (variable_name .eq. 'dlt_tt') then
         call respond2get_real_var (variable_name
     :                             , '(oCd)'
     :                             , g_dlt_tt)

      elseif (variable_name .eq. 'phase_tt') then
         call respond2get_real_array (variable_name
     :                             , '(oC)'
     :                             , g_phase_tt
     :                             , max_stage)

      elseif (variable_name .eq. 'tt_tot') then
         call respond2get_real_array (variable_name
     :                             , '(oC)'
     :                             , g_tt_tot
     :                             , max_stage)

      elseif (variable_name .eq. 'dlt_tt_curv') then
         call respond2get_real_var (variable_name
     :                             , '(oCd)'
     :                             , g_dlt_tt_curv)

      elseif (variable_name .eq. 'phase_tt_curv') then
         call respond2get_real_array (variable_name
     :                             , '(oC)'
     :                             , g_phase_tt_curv
     :                             , max_stage)

      elseif (variable_name .eq. 'tt_curv_tot') then
         call respond2get_real_array (variable_name
     :                             , '(oC)'
     :                             , g_tt_curv_tot
     :                             , max_stage)

      elseif (variable_name .eq. 'dlt_tt_other') then
         call respond2get_real_var (variable_name
     :                             , '(oCd)'
     :                             , g_dlt_tt_other)

      elseif (variable_name .eq. 'phase_tt_other') then
         call respond2get_real_array (variable_name
     :                             , '(oC)'
     :                             , g_phase_tt_other
     :                             , max_stage)

      elseif (variable_name .eq. 'tt_other_tot') then
         call respond2get_real_array (variable_name
     :                             , '(oC)'
     :                             , g_tt_other_tot
     :                             , max_stage)

      elseif (variable_name .eq. 'days_tot') then
         call respond2get_real_array (variable_name
     :                             , '()'
     :                             , g_days_tot
     :                             , max_stage)

      elseif (variable_name .eq. 'leaf_no') then
         call respond2get_real_array (variable_name
     :                              , '()'
     :                              , g_leaf_no
     :                              , max_stage)

      elseif (variable_name .eq. 'leaf_no_dead') then
         call respond2get_real_array (variable_name
     :                              , '()'
     :                              , g_leaf_no_dead
     :                              , max_stage)

      elseif (variable_name .eq. 'leaf_area') then
         call respond2get_real_array (variable_name
     :                              , '()'
     :                              , g_leaf_area
     :                              , max_leaf)

      elseif (variable_name .eq. 'height') then
         call respond2get_real_var (variable_name
     :                             , '(mm)'
     :                             , g_canopy_height)

      elseif (variable_name .eq. 'root_depth') then
         call respond2get_real_var (variable_name
     :                             , '(mm)'
     :                             , g_root_depth)

      elseif (variable_name .eq. 'plants') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g_plants)

      elseif (variable_name .eq. 'grain_no') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g_grain_no)

      elseif (variable_name .eq. 'grain_size') then
         grain_size = divide (g_dm_green(grain) + g_dm_dead(grain)
     :                      , g_grain_no, 0.0)
         call respond2get_real_var (variable_name
     :                             , '(g)'
     :                             , grain_size)


      elseif (variable_name .eq. 'cover_green') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g_cover_green)

      elseif (variable_name .eq. 'cover_tot') then
         cover_tot = 1.0
     :             - (1.0 - g_cover_green)
     :             * (1.0 - g_cover_sen)
     :             * (1.0 - g_cover_dead)

         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , cover_tot)

      elseif (variable_name .eq. 'lai_sum') then
         lai_sum = g_lai + g_slai + g_tlai_dead
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , lai_sum)

      elseif (variable_name .eq. 'tlai') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g_lai + g_slai)

      elseif (variable_name .eq. 'slai') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g_slai)

      elseif (variable_name .eq. 'lai') then
         call respond2get_real_var (variable_name
     :                             , '(m^2/m^2)'
     :                             , g_lai)

      elseif (variable_name .eq. 'tlai_dead') then
         call respond2get_real_var (variable_name
     :                             , '(m^2/m^2)'
     :                             , g_tlai_dead)

            ! plant biomass

      elseif (variable_name .eq. 'root_wt') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g_dm_green(root))

      elseif (variable_name .eq. 'leaf_wt') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g_dm_green(leaf))

      elseif (variable_name .eq. 'stem_wt') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g_dm_green(stem))

      elseif (variable_name .eq. 'flower_wt') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g_dm_green(flower))

      elseif (variable_name .eq. 'grain_wt') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g_dm_green(grain))

      elseif (variable_name .eq. 'dm_green') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g_dm_green
     :                             , max_part)

      elseif (variable_name .eq. 'dm_senesced') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g_dm_senesced
     :                             , max_part)

      elseif (variable_name .eq. 'dm_dead') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g_dm_dead
     :                             , max_part)

      elseif (variable_name .eq. 'yield') then
         yield = (g_dm_green(grain) + g_dm_dead(grain))
     :           * gm2kg / sm2ha
         call respond2get_real_var (variable_name
     :                             , '(kg/ha)'
     :                             , yield)

      elseif (variable_name .eq. 'biomass') then
         biomass = (sum_real_array (g_dm_green, max_part)
     :           - g_dm_green(root)
     :           + sum_real_array (g_dm_senesced, max_part)
     :           - g_dm_senesced(root)
     :           + sum_real_array (g_dm_dead, max_part)
     :           - g_dm_dead(root))
     :           * gm2kg / sm2ha
         call respond2get_real_var (variable_name
     :                             , '(kg/ha)'
     :                             , biomass)

      elseif (variable_name .eq. 'dlt_dm') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g_dlt_dm)

      elseif (variable_name .eq. 'dlt_dm_green') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g_dlt_dm_green
     :                             , max_part)

      elseif (variable_name .eq. 'dlt_dm_green_retrans') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g_dlt_dm_green_retrans
     :                             , max_part)

      elseif (variable_name .eq. 'dlt_dm_detached') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g_dlt_dm_detached
     :                             , max_part)

      elseif (variable_name .eq. 'dlt_dm_dead_detached') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g_dlt_dm_dead_detached
     :                             , max_part)

      elseif (variable_name .eq. 'n_green') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g_N_green
     :                             , max_part)

      elseif (variable_name .eq. 'n_senesced') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g_N_senesced
     :                             , max_part)

      elseif (variable_name .eq. 'n_dead') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g_N_dead
     :                             , max_part)

      elseif (variable_name .eq. 'dlt_n_green') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g_dlt_N_green
     :                             , max_part)

      elseif (variable_name .eq. 'dlt_n_retrans') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g_dlt_N_retrans
     :                             , max_part)

      elseif (variable_name .eq. 'dlt_n_detached') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g_dlt_N_detached
     :                             , max_part)

      elseif (variable_name .eq. 'dlt_n_dead_detached') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g_dlt_N_dead_detached
     :                             , max_part)

      elseif (variable_name .eq. 'swdef_pheno') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , millet_swdef (pheno))

      elseif (variable_name .eq. 'swdef_photo') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , millet_swdef (photo))

      elseif (variable_name .eq. 'swdef_expan') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , millet_swdef (expansion))

      elseif (variable_name .eq. 'ep') then
         num_layers = count_of_real_vals (g_dlayer, max_layer)
         call respond2get_real_array (variable_name
     :                               , '(mm)'
     :                               , g_dlt_sw_dep
     :                               , num_layers)

      elseif (variable_name .eq. 'cep') then
         call respond2get_real_var (variable_name
     :                             , '(mm)'
     :                             , - g_transpiration_tot)

      elseif (variable_name .eq. 'sw_demand') then
         call respond2get_real_var (variable_name
     :                             , '(mm)'
     :                             , g_sw_demand)

      elseif (variable_name .eq. 'sw_supply') then
         deepest_layer = find_layer_no (g_root_depth, g_dlayer
     :                                , max_layer)
         sw_supply_sum = sum_real_array (g_sw_supply, deepest_layer)
         call respond2get_real_var (variable_name
     :                             , '(mm)'
     :                             , sw_supply_sum)

      elseif (variable_name .eq. 'esw_layr') then

         num_layers = count_of_real_vals (g_dlayer, max_layer)
         do 1000 layer = 1, num_layers
            esw_layr(layer) = l_bound (g_sw_dep(layer) - p_ll_dep(layer)
     :                        , 0.0)
1000     continue
         call respond2get_real_array (variable_name
     :                               , '(mm)'
     :                               , esw_layr
     :                               , num_layers)

      elseif (variable_name .eq. 'das') then
         call respond2get_real_var (variable_name
     :                             , '(days)'
     :                             , sum_between (sowing, now
     :                                          , g_days_tot))

            ! plant nitrogen

      elseif (variable_name .eq. 'n_conc_stover') then
         call respond2get_real_var (variable_name
     :                             , '(%)'
     :                             , g_N_conc_act_stover_tot)

      elseif (variable_name .eq. 'n_conc_crit') then
         call respond2get_real_var (variable_name
     :                             , '(%)'
     :                             , g_N_conc_crit_stover_tot)

      elseif (variable_name .eq. 'n_grain_pcnt') then
         grain_N_pcnt = divide (g_N_green(grain)
     :                        , g_dm_green(grain), 0.0)
     :                        * fract2pcnt
         call respond2get_real_var (variable_name
     :                             , '(%)'
     :                             , grain_N_pcnt)


      elseif (variable_name .eq. 'n_uptake_grain') then
         ag_N_up = g_N_uptake_grain_tot*gm2kg /sm2ha
         call respond2get_real_var (variable_name
     :                             , '(kg/ha)'
     :                             , ag_N_up)


      elseif (variable_name .eq. 'n_uptake') then
         act_N_up = g_N_uptake_tot*gm2kg /sm2ha
         call respond2get_real_var (variable_name
     :                             , '(kg/ha)'
     :                             , act_N_up)


      elseif (variable_name .eq. 'n_uptake_stover') then
         apt_N_up = g_N_uptake_stover_tot*gm2kg /sm2ha
         call respond2get_real_var (variable_name
     :                             , '(kg/ha)'
     :                             , apt_N_up)

      elseif (variable_name .eq. 'no3_tot') then
         deepest_layer = find_layer_no (g_root_depth, g_dlayer
     :                                , max_layer)
         NO3gsm_tot = sum_real_array (g_NO3gsm, deepest_layer)
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , NO3gsm_tot)

      elseif (variable_name .eq. 'n_demand') then
         N_demand = sum_real_array (g_N_demand, max_part)
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , N_demand)

      elseif (variable_name .eq. 'n_supply') then
         N_supply = sum_real_array ( g_dlt_N_green, max_part)
     :            - g_dlt_N_green(grain)
     :            - g_dlt_N_green(flower)
     :            - g_dlt_N_green(root)
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , N_supply)

      elseif (variable_name .eq. 'n_supply_soil') then
         deepest_layer = find_layer_no (g_root_depth,g_dlayer,max_layer)
         N_uptake_sum = - sum_real_array (g_dlt_NO3gsm, deepest_layer)
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , N_uptake_sum)

      elseif (variable_name .eq. 'n_fix_pot') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , millet_N_fixation ())
      elseif (variable_name .eq. 'nfact_photo') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , millet_nfact (photo))

      elseif (variable_name .eq. 'nfact_grain') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , millet_nfact (grain_conc))

      elseif (variable_name .eq. 'photoperiod') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , day_length (g_day_of_year
     :                             , g_latitude, c_twilight))
     
      else
         ! not my variable
         call message_unused ()

      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      logical function millet_my_type ()
*     ===========================================================

*   Short description:
*       Returns true if 'type' is equal to the crop type or is absent.

*   Assumptions:
*       If type is not specified, it is assumed the message was addressed
*        directly to the module.

*   Notes:
*       none

*   Procedure attributes:
*      Version:         any hardware/fortran77
*      Extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       implicit none

*   Changes:
*      211294 jngh specified and programmed
*     220696 jngh changed extract to collect

*   Calls:
*     collect_char_var_optional
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'millet.inc'

*   Internal variables
      character  crop_type*50          ! crop type in data string
      integer    numvals               ! number of values returned

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_my_type')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      call collect_char_var_optional ('type', '()'
     :                              , crop_type, numvals)

      if (crop_type.eq.c_crop_type .or. numvals.eq.0) then
         millet_my_type = .true.
      else
         millet_my_type = .false.
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine millet_read_constants ()
*     ===========================================================

*   Short description:
*       Crop initialisation - reads constants from constants file

*   Assumptions:
*       none

*   Notes:
*       none

*   Procedure attributes:
*      Version:         any hardware/fortran77
*      Extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       implicit none

*   Changes:
*     010994 jngh specified and programmed
*     070495 psc added extra constants (leaf_app etc.)
*     110695 psc added soil temp effects on plant establishment

*   Calls:
*     pop_routine
*     push_routine
*     read_integer_var
*     read_real_var
*     read_real_array
*     write_string

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'const.inc'
      include   'millet.inc'

*   Internal variables
      integer    numvals               ! number of values returned

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'millet_read_constants')

      character  section_name*(*)
      parameter (section_name = 'constants')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      call write_string (lu_scr_sum
     :                  ,new_line//'    - Reading constants')

      call read_char_var (section_name
     :                     , 'crop_type', '()'
     :                     , c_crop_type, numvals)

      call read_char_array (section_name
     :                     , 'stage_names', max_stage, '()'
     :                     , c_stage_names, numvals)

      call read_real_array (section_name
     :                     , 'stage_code', max_stage, '()'
     :                     , c_stage_code_list, numvals
     :                     , 0.0, 1000.0)

      call read_real_array (section_name
     :                     , 'rue', max_stage, '(g dm/mj)'
     :                     , c_rue, numvals
     :                     , 0.0, 1000.0)

      call read_real_array (section_name
     :                     , 'root_depth_rate', max_stage, '(mm)'
     :                     , c_root_depth_rate, numvals
     :                     , 0.0, 1000.0)

      call read_real_array (section_name
     :                     , 'ratio_root_shoot', max_stage, '()'
     :                     , c_ratio_root_shoot, numvals
     :                     , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'extinction_coef', '()'
     :                    , c_extinction_coef, numvals
     :                    , 0.0, 10.0)

      call read_real_var (section_name
     :                    , 'extinction_coef_dead', '()'
     :                    , c_extinction_coef_dead, numvals
     :                    , 0.0, 10.0)
!cpsc
      call read_real_var (section_name
     :                    , 'extinction_coef_change', '()'
     :                    , c_extinction_coef_change, numvals
     :                    , 0.0, 10.0)

          ! millet_root_distrib

      call read_real_var (section_name
     :                    , 'root_extinction', '()'
     :                    , c_root_extinction, numvals
     :                    , 0.0, 10.0)

         ! crop failure

      call read_real_var (section_name
     :                    , 'leaf_no_crit', '()'
     :                    , c_leaf_no_crit, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'tt_emerg_limit', '(oC)'
     :                    , c_tt_emerg_limit, numvals
     :                    , 0.0, 365.0)

      call read_real_var (section_name
     :                    , 'days_germ_limit', '(days)'
     :                    , c_days_germ_limit, numvals
     :                    , 0.0, 365.0)

      call read_real_var (section_name
     :                    , 'swdf_pheno_limit', '()'
     :                    , c_swdf_pheno_limit, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'swdf_photo_limit', '()'
     :                    , c_swdf_photo_limit, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'swdf_photo_rate', '()'
     :                    , c_swdf_photo_rate, numvals
     :                    , 0.0, 1.0)


         !    millet_root_depth

      call read_real_var (section_name
     :                    , 'initial_root_depth', '(mm)'
     :                    , c_initial_root_depth, numvals
     :                    , 0.0, 1000.0)

cglh      call read_real_var (section_name
cglh     :                    , 'root_depth_lag_start', '(days)'
cglh     :                    , c_root_depth_lag_start, numvals
cglh     :                    , 0.0, 365.0)

cglh      call read_real_var (section_name
cglh     :                    , 'root_depth_lag_end', '(days)'
cglh     :                    , c_root_depth_lag_end, numvals
cglh     :                    , 0.0, 365.0)

         !    millet_leaf_area_init

      call read_real_var (section_name
     :                    , 'initial_tpla', '(mm^2)'
     :                    , c_initial_tpla, numvals
     :                    , 0.0, 100000.0)

         ! TEMPLATE OPTION
         !    millet_leaf_area

      call read_real_var (section_name
     :                    , 'sla_max', '(mm^2/g)'
     :                    , c_sla_max, numvals
     :                    , 0.0, 100000.0)

      call read_real_array (section_name
     :                    , 'x_lai_ratio', max_table, '()'
     :                    , c_x_lai_ratio, c_num_lai_ratio
     :                    , 0.0, 1.0)

      call read_real_array (section_name
     :                    , 'y_leaf_no_frac', max_table, '()'
     :                    , c_y_leaf_no_frac, c_num_lai_ratio
     :                    , 0.0, 1.0)

         ! TEMPLATE OPTION
         !    millet_leaf_area_devel_plant

      ! call read_real_var (section_name
      !:                    , 'tiller_coef', '()'
      !:                    , c_tiller_coef , numvals
      !:                    , 0.0, 10.0)

      ! call read_real_var (section_name
      !:                    , 'main_stem_coef', '()'
      !:                    , c_main_stem_coef, numvals
      !:                    , 0.0, 10.0)

         !    millet_height

      call read_real_var (section_name
     :                    , 'height_max', '(mm)'
     :                    , c_height_max, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'height_stem_slope', '(mm/g/stem)'
     :                    , c_height_stem_slope, numvals
     :                    , 0.0, 1000.0)

         !    millet_get_cultivar_params

      call read_real_var (section_name
     :                    , 'head_grain_no_max_ub', '()'
     :                    , c_head_grain_no_max_ub, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'grain_gth_rate_ub', '()'
     :                    , c_grain_gth_rate_ub, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'tt_emerg_to_endjuv_ub', '()'
     :                    , c_tt_emerg_to_endjuv_ub, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'pp_endjuv_to_init_ub', '()'
     :                    , c_pp_endjuv_to_init_ub, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'tt_flower_to_maturity_ub', '()'
     :                    , c_tt_flower_to_maturity_ub, numvals
     :                    , 0.0, 2000.0)

      call read_real_var (section_name
     :                    , 'tt_maturity_to_ripe_ub', '()'
     :                    , c_tt_maturity_to_ripe_ub, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'tt_flower_to_start_grain_ub', '()'
     :                    , c_tt_flower_to_start_grain_ub, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'tt_flag_to_flower_ub', '()'
     :                    , c_tt_flag_to_flower_ub, numvals
     :                    , 0.0, 1000.0)

         !    millet_transp_eff

      call read_real_var (section_name
     :                    , 'svp_fract', '()'
     :                    , c_svp_fract, numvals
     :                    , 0.0, 1.0)

      call read_real_var (section_name
     :                    , 'transp_eff_cf', '(kpa)'
     :                    , c_transp_eff_cf, numvals
     :                    , 0.0, 1.0)

         ! TEMPLATE OPTION
         !    millet_grain_no

      call read_real_var (section_name
     :                    , 'head_grain_no_crit', '()'
     :                    , c_head_grain_no_crit, numvals
     :                    , 0.0, 1000.0)

         !    millet_plants_barren

      call read_real_var (section_name
     :                    , 'barren_crit', '()'
     :                    , c_barren_crit, numvals
     :                    , 0.0, 1.0)

         !    millet_germination

      call read_real_var (section_name
     :                    , 'pesw_germ', '(mm/mm)'
     :                    , c_pesw_germ, numvals
     :                    , 0.0, 1.0)

         ! TEMPLATE OPTION
         !    millet_grain_no

      call read_real_var (section_name
     :                    , 'grain_n_conc_min', '()'
     :                    , c_grain_N_conc_min, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'seed_wt_min', '(g/seed)'
     :                    , c_seed_wt_min, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'growth_rate_min', '(g/plant)'
     :                    , c_growth_rate_min, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'growth_rate_crit', '(g/plant)'
     :                    , c_growth_rate_crit, numvals
     :                    , 0.0, 1000.0)

         !    millet_leaf_appearance

      call read_real_var (section_name
     :                    , 'leaf_no_at_emerg', '()'
     :                    , c_leaf_no_at_emerg, numvals
     :                    , 0.0, 100.0)

         !    millet_N_uptake

      call read_real_var (section_name
     :                    , 'no3_diffn_const', '(days)'
     :                    , c_NO3_diffn_const, numvals
     :                    , 0.0, 100.0)

         !    millet_N_fixation

      call read_real_var (section_name
     :                    , 'N_fix_rate', '(g N/g plant)'
     :                    , c_N_fix_rate, numvals
     :                    , 0.0, 1.0)

         !    millet_phenology_init

      call read_real_var (section_name
     :                    , 'shoot_lag', '(oC)'
     :                    , c_shoot_lag, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'shoot_rate', '(oC/mm)'
     :                    , c_shoot_rate, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'photoperiod_base', '(hr)'
     :                    , c_photoperiod_base, numvals
     :                    , 0.0, 24.0)

      call read_real_var (section_name
     :                    , 'photoperiod_crit', '(hr)'
     :                    , c_photoperiod_crit, numvals
     :                    , 0.0, 24.0)

      call read_real_var (section_name
     :                    , 'leaf_app_rate1', '(oC)'
     :                    , c_leaf_app_rate1, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'leaf_app_rate2', '(oC)'
     :                    , c_leaf_app_rate2, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'leaf_no_rate_change', '()'
     :                    , c_leaf_no_rate_change, numvals
     :                    , 0.0, 30.0)

         !    millet_dm_init

      call read_real_var (section_name
     :                    , 'dm_leaf_init', '(g/plant)'
     :                    , c_dm_leaf_init, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'dm_root_init', '(g/plant)'
     :                    , c_dm_root_init, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'dm_stem_init', '(g/plant)'
     :                    , c_dm_stem_init, numvals
     :                    , 0.0, 1000.0)

         !    millet_get_root_params

      call read_real_var (section_name
     :                    , 'll_ub', '()'
     :                    , c_ll_ub, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'kl_ub', '()'
     :                    , c_kl_ub, numvals
     :                    , 0.0, 1000.0)

         !    millet_leaf_no_final

      call read_real_var (section_name
     :                    , 'leaf_init_rate', '(oC)'
     :                    , c_leaf_init_rate, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'leaf_no_seed', '(leaves)'
     :                    , c_leaf_no_seed, numvals
     :                    , 0.0, 100.0)

cglh      call read_real_var (section_name
cglh     :                    , 'floral_init_error', '(oc)'
cglh     :                    , c_floral_init_error, numvals
cglh     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'leaf_no_min', '()'
     :                   , c_leaf_no_min, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'leaf_no_max', '()'
     :                   , c_leaf_no_max, numvals
     :                   , 0.0, 100.0)

         !    millet_retranslocate

      call read_real_var (section_name
     :                    , 'stem_trans_frac', '()'
     :                    , c_stem_trans_frac, numvals
     :                    , 0.0, 1.0)

      call read_real_var (section_name
     :                    , 'leaf_trans_frac', '()'
     :                    , c_leaf_trans_frac, numvals
     :                    , 0.0, 1.0)

         !    millet_watck

      call read_real_var (section_name
     :                    , 'minsw', '()'
     :                    , c_minsw, numvals
     :                    , 0.0, 1000.0)

         ! TEMPLATE OPTION
         !    millet_dm_grain

      call read_real_var (section_name
     :                    , 'swdf_grain_min', '()'
     :                    , c_swdf_grain_min, numvals
     :                    , 0.0, 100.0)

         ! TEMPLATE OPTION
         !    millet_dm_grain_hi

      ! call read_real_var (section_name
      !:                    , 'hi_min', '()'
      !:                    , c_hi_min, numvals
      !:                    , 0.0, 100.0)

         !    millet_N_dlt_grain_conc

      call read_real_var (section_name
     :                    , 'sw_fac_max', '()'
     :                    , c_sw_fac_max, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'temp_fac_min', '()'
     :                    , c_temp_fac_min, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'sfac_slope', '()'
     :                    , c_sfac_slope, numvals
     :                    , -10.0, 0.0)

      call read_real_var (section_name
     :                    , 'tfac_slope', '()'
     :                    , c_tfac_slope, numvals
     :                    , 0.0, 100.0)

         !    millet_leaf_death

      call read_real_var (section_name
     :                    , 'leaf_no_dead_const', '()'
     :                    , c_leaf_no_dead_const, numvals
     :                    , -1.0, 100.0)

      call read_real_var (section_name
     :                    , 'leaf_no_dead_slope', '()'
     :                    , c_leaf_no_dead_slope, numvals
     :                    , 0.0, 100.0)

         !    millet_get_other_variables

         ! checking the bounds of the bounds..
      call read_integer_var (section_name
     :                    , 'year_ub', '()'
     :                    , c_year_ub, numvals
     :                    , 1800, 2000)

      call read_integer_var (section_name
     :                    , 'year_lb', '()'
     :                    , c_year_lb, numvals
     :                    , 1800, 2000)

      call read_real_var (section_name
     :                    , 'latitude_ub', '(oL)'
     :                    , c_latitude_ub, numvals
     :                    , -90.0, 90.0)

      call read_real_var (section_name
     :                    , 'latitude_lb', '(oL)'
     :                    , c_latitude_lb, numvals
     :                    , -90.0, 90.0)

      call read_real_var (section_name
     :                    , 'maxt_ub', '(oC)'
     :                    , c_maxt_ub, numvals
     :                    , 0.0, 60.0)

      call read_real_var (section_name
     :                    , 'maxt_lb', '(oC)'
     :                    , c_maxt_lb, numvals
     :                    , 0.0, 60.0)

      call read_real_var (section_name
     :                    , 'mint_ub', '(oC)'
     :                    , c_mint_ub, numvals
     :                    , 0.0, 40.0)

      call read_real_var (section_name
     :                    , 'mint_lb', '(oC)'
     :                    , c_mint_lb, numvals
     :                    , -100.0, 100.0)

      call read_real_var (section_name
     :                    , 'radn_ub', '(MJ/m^2)'
     :                    , c_radn_ub, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'radn_lb', '(MJ/m^2)'
     :                    , c_radn_lb, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'dlayer_ub', '(mm)'
     :                    , c_dlayer_ub, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'dlayer_lb', '(mm)'
     :                    , c_dlayer_lb, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'dul_dep_ub', '(mm)'
     :                    , c_dul_dep_ub, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'dul_dep_lb', '(mm)'
     :                    , c_dul_dep_lb, numvals
     :                    , 0.0, 10000.0)

                                ! 8th block
      call read_real_var (section_name
     :                    , 'sw_dep_ub', '(mm)'
     :                    , c_sw_dep_ub, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'sw_dep_lb', '(mm)'
     :                    , c_sw_dep_lb, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'no3_ub', '(kg/ha)'
     :                    , c_NO3_ub, numvals
     :                    , 0.0, 100000.0)

      call read_real_var (section_name
     :                    , 'no3_lb', '(kg/ha)'
     :                    , c_NO3_lb, numvals
     :                    , 0.0, 100000.0)

      call read_real_var (section_name
     :                    , 'no3_min_ub', '(kg/ha)'
     :                    , c_NO3_min_ub, numvals
     :                    , 0.0, 100000.0)

      call read_real_var (section_name
     :                    , 'no3_min_lb', '(kg/ha)'
     :                    , c_NO3_min_lb, numvals
     :                    , 0.0, 100000.0)

         !    millet_event

      call read_real_var (section_name
     :                    , 'grn_water_cont', '(g/g)'
     :                    , c_grn_water_cont, numvals
     :                    , 0.0, 1.0)

         !    millet_dm_partition

      call read_real_var (section_name
     :                    , 'sla_min', '(mm^2/g)'
     :                    , c_sla_min, numvals
     :                    , 0.0, 100000.0)

      call read_real_var (section_name
     :                    , 'partition_rate_leaf', '()'
     :                    , c_partition_rate_leaf, numvals
     :                    , 0.0, 1.0)

      call read_real_var (section_name
     :                    , 'frac_leaf_pre_flower', '()'
     :                    , c_frac_leaf_pre_flower, numvals
     :                    , 0.0, 1.0)

      call read_real_var (section_name
     :                    , 'frac_leaf_post_flower', '()'
     :                    , c_frac_leaf_post_flower, numvals
     :                    , 0.0, 1.0)
     
      call read_real_var (section_name
     :                    , 'frac_stem2flower', '()'
     :                    , c_frac_stem2flower, numvals
     :                    , 0.0, 1.0)

      call read_real_var (section_name
     :                    , 'frac_flower2grain', '()'
     :                    , c_frac_flower2grain, numvals
     :                    , 0.0, 1.0)

         ! TEMPLATE OPTION
         !    millet_grain_no

      call read_real_var (section_name
     :                    , 'htstress_coeff', '()'
     :                    , c_htstress_coeff, numvals
     :                    , 0.0, 1.0)

         !    millet_dm_senescence

      call read_real_var (section_name
     :                    , 'dm_root_sen_frac', '()'
     :                    , c_dm_root_sen_frac, numvals
     :                    , 0.0, 1.0)

      call read_real_var (section_name
     :                    , 'dm_leaf_sen_frac', '()'
     :                    , c_dm_leaf_sen_frac, numvals
     :                    , 0.0, 1.0)

         !    millet_dm_dead_detachment

      call read_real_array (section_name
     :                    , 'dead_detach_frac', max_part, '()'
     :                    , c_dead_detach_frac, numvals
     :                    , 0.0, 1.0)

      call read_real_var (section_name
     :                    , 'dm_leaf_detach_frac', '()'
     :                    , c_dm_leaf_detach_frac, numvals
     :                    , 0.0, 1.0)

         ! TEMPLATE OPTION
         !    millet_leaf_area_devel

      call read_real_var (section_name
     :                    , 'leaf_no_correction', '()'
     :                    , c_leaf_no_correction, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'leaf_size_average', '()'
     :                    , c_leaf_size_average, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'leaf_size_endjuv', '()'
     :                    , c_leaf_size_endjuv, numvals
     :                    , 0.0, 10000.0)

         ! TEMPLATE OPTION
         !    millet_leaf_size

      call read_real_var (section_name
     :                    , 'x0_const', '()'
     :                    , c_x0_const, numvals
     :                    , -10.0, 100.0)

      call read_real_var (section_name
     :                    ,'x0_slope', '()'
     :                    , c_x0_slope, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'y0_const', '()'
     :                    , c_y0_const, numvals
     :                    , -40000.0, 100000.0)

      call read_real_var (section_name
     :                    , 'y0_slope', '()'
     :                    , c_y0_slope, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'a_const', '()'
     :                    , c_a_const, numvals
     :                    , -100.0, 0.0)

      call read_real_var (section_name
     :                    , 'a_slope1', '()'
     :                    , c_a_slope1, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'a_slope2', '()'
     :                    , c_a_slope2, numvals
     :                    , -100.0, 0.0)

      call read_real_var (section_name
     :                    , 'b_const', '()'
     :                    , c_b_const, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'b_slope1', '()'
     :                    , c_b_slope1, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'b_slope2', '()'
     :                    , c_b_slope2, numvals
     :                    , -100.0, 0.0)
     
         !    millet_tiller
     
      call read_integer_var (section_name
     :                   , 'tiller_no_pot', '()'
     :                   , c_tiller_no_pot, numvals
     :                   , 0.0, 10)

      call read_char_var (section_name
     :                     , 'tiller_appearance', '()'
     :                     , c_tiller_appearance, numvals)

      call read_real_array (section_name
     :                     , 'x_tiller_no_next', max_table, '()'
     :                     , c_x_tiller_no_next, c_num_tiller_no_next
     :                     , 0.0, 10.0)

      call read_real_array (section_name
     :                     , 'y_tiller_tt', max_table, '(oCd)'
     :                     , c_y_tiller_tt, c_num_tiller_no_next
     :                     , 0.0, 2000.0)

      call read_real_var (section_name
     :                   , 'dm_tiller_crit', '(g/plant)'
     :                   , c_dm_tiller_crit, numvals
     :                   , 0.0, 100.0)

         ! TEMPLATE OPTION
         !    millet_leaf_area_sen_light

      call read_real_var (section_name
     :                   , 'lai_sen_light', '(m^2/m^2)'
     :                   , c_lai_sen_light, numvals
     :                   , 3.0, 20.0)

      call read_real_var (section_name
     :                    , 'sen_light_slope', '()'
     :                    , c_sen_light_slope, numvals
     :                    , 0.0, 100.0)


         ! TEMPLATE OPTION
         !    millet_leaf_area_sen_frost

      call read_real_array (section_name
     :                   , 'x_temp_senescence', max_table, '(oC)'
     :                   , c_x_temp_senescence, c_num_temp_senescence
     :                   , -20.0, 20.0)

      call read_real_array (section_name
     :                   , 'y_senescence_fac', max_table, '()'
     :                   , c_y_senescence_fac, c_num_temp_senescence
     :                   , 0.0, 1.0)

         ! TEMPLATE OPTION
         !    millet_leaf_area_sen_water

      call read_real_var (section_name
     :                    , 'sen_rate_water', '()'
     :                    , c_sen_rate_water, numvals
     :                    , 0.0, 100.0)

         ! TEMPLATE OPTION
         !    millet_leaf_area_sen_light1

      ! call read_real_var (section_name
      !:                    , 'sen_light_time_const', '(days)'
      !:                    , c_sen_light_time_const, numvals
      !:                    , 0.0, 50.0)

      ! call read_real_var (section_name
      !:                    , 'sen_radn_crit', '(Mj/m^2)'
      !:                    , c_sen_radn_crit, numvals
      !:                    , 0.0, 10.0)

         ! TEMPLATE OPTION
         !    millet_leaf_area_sen_frost1

      ! call read_real_var (section_name
      !:                    , 'frost_kill', '(oC)'
      !:                    , c_frost_kill, numvals
      !:                    , -6.0, 6.0)

         ! TEMPLATE OPTION
         !    millet_leaf_area_sen_water1

      ! call read_real_var (section_name
      !:                    , 'sen_water_time_const', '(days)'
      !:                    , c_sen_water_time_const, numvals
      !:                    , 0.0, 50.0)

      ! call read_real_var (section_name
      !:                    , 'sen_threshold', '()'
      !:                    , c_sen_threshold, numvals
      !:                    , 0.0, 10.0)

         ! TEMPLATE OPTION
         !    millet_leaf_area_sen_age1

      ! call read_real_var (section_name
      !:                    , 'spla_slope', '(oC/leaf)'
      !:                    , c_spla_slope, numvals
      !:                    , 0.0, 6.0)

         !    millet_phenology_init

      call read_real_var (section_name
     :                   , 'twilight', '(o)'
     :                   , c_twilight, numvals
     :                   , -90.0, 90.0)

         ! TEMPLATE OPTION
         !    millet_heat_stress

      call read_real_var (section_name
     :                   , 'temp_grain_crit_stress', '(oC)'
     :                   , c_temp_grain_crit_stress, numvals
     :                   , 20.0, 50.0)

         !    millet_N_conc_limits

      call read_real_array (section_name
     :                     , 'x_stage_code', max_stage, '()'
     :                     , c_x_stage_code, c_num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_crit_leaf', max_stage, '()'
     :                     , c_y_N_conc_crit_leaf, c_num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_max_leaf', max_stage, '()'
     :                     , c_y_N_conc_max_leaf, c_num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_min_leaf', max_stage, '()'
     :                     , c_y_N_conc_min_leaf, c_num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_crit_stem', max_stage, '()'
     :                     , c_y_N_conc_crit_stem, c_num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_max_stem', max_stage, '()'
     :                     , c_y_N_conc_max_stem, c_num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_min_stem', max_stage, '()'
     :                     , c_y_N_conc_min_stem, c_num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_crit_flower', max_stage, '()'
     :                     , c_y_N_conc_crit_flower, c_num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_max_flower', max_stage, '()'
     :                     , c_y_N_conc_max_flower, c_num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_min_flower', max_stage, '()'
     :                     , c_y_N_conc_min_flower, c_num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'n_conc_crit_grain', '()'
     :                   , c_N_conc_crit_grain, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'n_conc_max_grain', '()'
     :                   , c_N_conc_max_grain, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'n_conc_min_grain', '()'
     :                   , c_N_conc_min_grain, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'n_conc_crit_root', '()'
     :                   , c_N_conc_crit_root, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'n_conc_max_root', '()'
     :                   , c_N_conc_max_root, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'n_conc_min_root', '()'
     :                   , c_N_conc_min_root, numvals
     :                   , 0.0, 100.0)

         !    millet_N_init

      call read_real_var (section_name
     :                   , 'n_leaf_init_conc', '()'
     :                   , c_N_leaf_init_conc, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'n_root_init_conc', '()'
     :                   , c_N_root_init_conc, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'n_stem_init_conc', '()'
     :                   , c_N_stem_init_conc, numvals
     :                   , 0.0, 100.0)

         !    millet_N_senescence

      call read_real_var (section_name
     :                   , 'n_leaf_sen_conc', '()'
     :                   , c_N_leaf_sen_conc, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'n_root_sen_conc', '()'
     :                   , c_N_root_sen_conc, numvals
     :                   , 0.0, 100.0)

         !    millet_nfact

      call read_real_var (section_name
     :                   , 'N_fact_photo', '()'
     :                   , c_N_fact_photo, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'N_fact_pheno', '()'
     :                   , c_N_fact_pheno, numvals
     :                   , 0.0, 100.0)

         !    millet_rue_reduction

      call read_real_array (section_name
     :                     , 'x_ave_temp', max_table, '(oC)'
     :                     , c_x_ave_temp, c_num_ave_temp
     :                     , 0.0, 100.0)


      call read_real_array (section_name
     :                     , 'y_stress_photo', max_table, '()'
     :                     , c_y_stress_photo, c_num_factors
     :                     , 0.0, 1.0)

         ! TEMPLATE OPTION
         !    millet_dm_grain

      call read_real_array (section_name
     :                     , 'x_temp_grain', max_table, '(oC)'
     :                     , c_x_temp_grain, c_num_temp_grain
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_grain_rate', max_table, '()'
     :                     , c_y_grain_rate, c_num_temp_grain
     :                     , 0.0, 1.0)

         !    millet_tt

      call read_real_array (section_name
     :                     , 'x_temp', max_table, '(oC)'
     :                     , c_x_temp, c_num_temp
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_tt', max_table, '(oC)'
     :                     , c_y_tt, c_num_temp
     :                     , 0.0, 100.0)
!cpsc
      call read_real_array (section_name
     :                     , 'x_weighted_temp', max_table, '(oC)'
     :                     , c_x_weighted_temp, c_num_weighted_temp
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_plant_death', max_table, '(oC)'
     :                     , c_y_plant_death, c_num_weighted_temp
     :                     , 0.0, 100.0)

         ! TEMPLATE OPTION
         !    millet_tt_other

      ! call read_real_array (section_name
      !:                     , 'x_temp_other', max_table, '(oC)'
      !:                     , c_x_temp_other, c_num_temp_other
      !:                     , 0.0, 100.0)

      ! call read_real_array (section_name
      !:                     , 'y_tt_other', max_table, '(oC)'
      !:                     , c_y_tt_other, c_num_temp_other
      !:                     , 0.0, 100.0)

         ! TEMPLATE OPTION
         !    millet_tt_curv

      ! call read_real_var (section_name
      !:                    , 'imin', '()'
      !:                    , c_imin, numvals
      !:                    , 0.0, 100.0)

      ! call read_real_var (section_name
      !:                    , 'iopt', '()'
      !:                    , c_iopt, numvals
      !:                    , 0.0, 100.0)

      ! call read_real_var (section_name
      !:                    , 'imax', '()'
      !:                    , c_imax, numvals
      !:                    , 0.0, 100.0)

      ! call read_real_var (section_name
      !:                    , 'ioptr', '()'
      !:                    , c_ioptr, numvals
      !:                    , 0.0, 100.0)

      ! call read_real_var (section_name
      !:                    , 'amin', '()'
      !:                    , c_amin, numvals
      !:                    , 0.0, 100.0)

      ! call read_real_var (section_name
      !:                    , 'aopt', '()'
      !:                    , c_aopt, numvals
      !:                    , 0.0, 100.0)

      ! call read_real_var (section_name
      !:                    , 'amax', '()'
      !:                    , c_amax, numvals
      !:                    , 0.0, 100.0)

      ! call read_real_var (section_name
      !:                    , 'aoptr', '()'
      !:                    , c_aoptr, numvals
      !:                    , 0.0, 100.0)

         !    millet_swdef

      call read_real_array (section_name
     :                     , 'x_sw_demand_ratio', max_table, '()'
     :                     , c_x_sw_demand_ratio, c_num_sw_demand_ratio
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_swdef_leaf', max_table, '()'
     :                     , c_y_swdef_leaf, c_num_sw_demand_ratio
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'x_sw_avail_ratio', max_table, '()'
     :                     , c_x_sw_avail_ratio, c_num_sw_avail_ratio
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_swdef_pheno', max_table, '()'
     :                     , c_y_swdef_pheno, c_num_sw_avail_ratio
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'x_sw_ratio', max_table, '()'
     :                     , c_x_sw_ratio, c_num_sw_ratio
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_sw_fac_root', max_table, '()'
     :                     , c_y_sw_fac_root, c_num_sw_ratio
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'x_sw_avail_fix', max_table, '()'
     :                     , c_x_sw_avail_fix, c_num_sw_avail_fix
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_swdef_fix', max_table, '()'
     :                     , c_y_swdef_fix, c_num_sw_avail_fix
     :                     , 0.0, 100.0)

      call pop_routine (my_name)
      return
      end
