*     ===========================================================
      character*(*) function millet_version ()
*     ===========================================================
      implicit none
      include 'error.pub'                         

*+  Purpose
*       Returns version number of crop module

*+  Notes
*
****    Rev 2.00   26 Oct 1997
*

*+  Changes
*       011092 jngh specified and programmed
*       220696 jngh removed PVCS revision numbering
!*       280497 ejvo made breadth, skewness linear function of leafnumber
*       140797 gd made changes to use with new util
*       261097 gol corrected tiller appearance & update to version 2.00

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_version')
*
      character  version_number*(*)    ! version number of module
*
      parameter (version_number = 'V2.00 261097')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      millet_version = version_number
*
****     Rev 2.00   26 Oct 1997
*
      call pop_routine (my_name)
      return
      end



*     ================================================================
      subroutine APSIM_millet (action, data_string)
*     ================================================================
      implicit none
      dll_export apsim_millet
      include   'millet.inc'            !
      include   'const.inc'            ! mes_presence, mes_init, mes_process
      include 'string.pub'                        
      include 'engine.pub'                        
      include 'error.pub'                         

*+  Sub-Program Arguments
      character  action*(*)            ! (INPUT) Message action to perform
      character  data_string*(*)       ! (INPUT) Message data

*+  Purpose
*      This module performs crop crop growth
*       simulates root, leaf, head, stem and grain development. Water and
*       nitrogen uptake, photosynhesis, and leaf and root senescense.

*+  Changes
*      250894 jngh specified and programmed
*      220696 jngh added message_unused to else

*+  Calls
                                       ! mes_report
*
      logical    millet_my_type         ! function
      character  millet_version*52     ! function

*+  Constant Values
      character  my_name*(*)           ! name of this procedure
      parameter (my_name='millet')

*+  Local Variables
      character  module_name*8         ! module name

*- Implementation Section ----------------------------------
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
 
            if (g_stem_class .eq. class_tiller) then
               call fatal_error (err_user,
     :                      'Cannot sow initiated tiller!')
            else
               call millet_set_my_class (class_main)
 
               ! request and receive variables from owner-modules
               call millet_get_other_variables ()
               ! start crop and do  more initialisations
               call millet_start_crop ()
            endif
         else
            ! not my type!
            call message_unused ()
         endif
 
      elseif (action.eq.mes_initiate_crop) then
 
         if (millet_my_type ()) then
 
            if (g_stem_class .eq. class_main) then
               call fatal_error (err_user,
     :                      'Cannot initiate main tiller!')
            else
               call millet_set_my_class (class_tiller)
               ! request and receive variables from owner-modules
               call millet_get_other_variables ()
               ! start crop and do  more initialisations
               call millet_initiate ()
            endif
 
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
      implicit none
      include   'millet.inc'
      include 'error.pub'                         

*+  Purpose
*       Simulate crop processes.  These include biomass production,
*       phenological stages, plant component development,
*       water uptake and nitrogen uptake, and plant senescense.

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_process')

*- Implementation Section ----------------------------------
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
      implicit none
      include   'millet.inc'
      include 'error.pub'                         

*+  Purpose
*       Set up states for dead crop

*+  Changes
*      091095 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_dead')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      g_current_stage   = real (plant_end)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_harvest ()
*     ===========================================================
      implicit none
      include   'const.inc'            ! new_line, lu_scr_sum, blank,
      include   'convert.inc'          ! gm2kg, sm2ha, sm2smm
      include   'millet.inc'
      include 'data.pub'                          
      include 'write.pub'                         
      include 'error.pub'                         

*+  Purpose
*       Report occurence of harvest and the current status of specific
*       variables.

*+  Changes
*     010994 jngh specified and programmed

*+  Calls
                                       ! lu_scr_sum

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_harvest')

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
 
      g_leaf_no_total = sum_between (emerg, harvest_ripe, g_leaf_no)
      leaf_no = sum_between (emerg, harvest_ripe, g_leaf_no)
cejvo      leaf_no = sum_between (germ, harvest_ripe, g_leaf_no)
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
      implicit none
      include   'millet.inc'
      include   'const.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Purpose
*       Zero crop variables & arrays

*+  Changes
*     010994 jngh specified and programmed
*     090695 psc  add row spacing = 0

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'millet_zero_variables')

*- Implementation Section ----------------------------------
 
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
cejvo
      call fill_real_array (g_y_tiller_tt_adj, 0.0, max_table)
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
!cejvo
      g_leaf_no_effective = 0.0
      g_leaf_no_ref = 0.0
      g_leaf_no_dead_const2 = 0.0
      g_lf_no_dead_at_flaglf = 0.0
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
 
      g_stem_class = blank
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_zero_daily_variables ()
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Purpose
*       Zero crop daily variables & arrays

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'millet_zero_daily_variables')

*- Implementation Section ----------------------------------
 
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
      implicit none
      include   'millet.inc'
      include 'write.pub'                         
      include 'error.pub'                         

*+  Purpose
*       Crop initialisation

*+  Changes
*     010994 jngh specified and programmed

*+  Calls
      character  millet_version*52      ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'millet_init')

*- Implementation Section ----------------------------------
 
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
      implicit none
      include   'const.inc'            ! lu_scr_sum, blank
      include   'millet.inc'
      include 'intrface.pub'                      
      include 'write.pub'                         
      include 'error.pub'                         

*+  Purpose
*       Start crop using parameters specified in passed record

*+  Changes
*     010994 jngh specified and programmed
*     090695 psc  add row spacing read
*     220696 jngh changed extract to collect

*+  Calls
!

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'millet_start_crop')

*+  Local Variables
      integer    numvals               ! number of values found in array
      character  string*200            ! output string
 
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
!gd
!         call get_current_module (module_name)
!         write(*,*) 'lastnb', lastnb(module_name)
 
      call report_event ( 'Sow')
 
cjh      if (data_record.ne.blank) then
 
         call collect_real_var ('plants', '()'
     :                        , g_plants, numvals, 0.0, 100.0)
!cpsc
         call collect_real_var_optional (
     :                          'row_spacing', '(m)'
     :                        , g_row_spacing, numvals
     :                        , 0.0, 2.0)
 
         if (g_row_spacing .eq. 0.0) then
           g_row_spacing = c_row_spacing_default
         endif
 
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
      implicit none
      include   'const.inc'            ! lu_scr_sum, blank
      include   'millet.inc'
      include 'data.pub'                          
      include 'intrface.pub'                      
      include 'write.pub'                         
      include 'error.pub'                         

*+  Purpose
*       Initiate crop, tillers or other axes using parameters
*       specified in passed record.

*+  Changes
*     091095 jngh specified and programmed
*     220696 jngh changed extract to collect

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'millet_initiate')

*+  Local Variables
      integer    numvals               ! number of values found in array
      character  string*200            ! output string
      real       N_tiller_plant        ! N content of tiller (g/plant)

*- Implementation Section ----------------------------------
 
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
 
         if (g_row_spacing .eq. 0.0) then
           g_row_spacing = c_row_spacing_default
         endif
 
 
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
      implicit none
      include   'const.inc'            ! new_line, lu_scr_sum, blank
      include   'millet.inc'
      include 'read.pub'                          
      include 'write.pub'                         
      include 'error.pub'                         

*+  Purpose
*       Get cultivar parameters for named cultivar, from crop parameter file.

*+  Changes
*       090994 jngh specified and programmed

*+  Calls
                                       ! lu_src_sum

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_read_cultivar_params')

*+  Local Variables
      character  string*200            ! output string
      integer    numvals               ! number of values read

*- Implementation Section ----------------------------------
 
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
 
cgd   Eriks modifications for Leaf Area
      call read_real_var (g_cultivar
     :                    , 'y0_const', '()'
     :                    , p_y0_const, numvals
     :                    , -40000.0, 100000.0)
 
      call read_real_var (g_cultivar
     :                    , 'y0_slope', '()'
     :                    , p_y0_slope, numvals
     :                    , 0.0, 10000.0)
             ! report
 
      string = '    ------------------------------------------------'
      call write_string (lu_scr_sum, string)
 
      write (string, '(4x,2a)')
     :                'Cultivar                 = ', g_cultivar
      call write_string (lu_scr_sum, string)
 
      write (string, '(4x, a, i7)')
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
      implicit none
      include   'const.inc'            ! new_line, lu_scr_sum, blank,
      include   'millet.inc'            ! dlayer(max_layer)
      include 'data.pub'                          
      include 'read.pub'                          
      include 'write.pub'                         
      include 'error.pub'                         

*+  Purpose
*       Get root profile parameters

*+  Changes
*       090994 jngh specified and programmed
*     210395 jngh changed from millet_section to a parameters section

*+  Calls
                                       ! lu_scr_sum

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_read_root_params')
*
      character  section_name*(*)
      parameter (section_name = 'parameters')

*+  Local Variables
!      integer    numvals
      integer    layer                 ! layer number
      real       ll (max_layer)        ! lower limit of plant-extractable
                                       ! soil water for soil layer l
                                       ! (mm water/mm soil)
      integer    num_layers            ! number of layers in profile
      character  string*200            ! output string

*- Implementation Section ----------------------------------
 
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
      implicit none
      include   'const.inc'            ! new_line, lu_scr_sum
      include   'convert.inc'          ! gm2kg, sm2ha, mm2cm, cmm2cc
      include   'millet.inc'
      include 'data.pub'                          
      include 'write.pub'                         
      include 'error.pub'                         

*+  Purpose
*       End crop

*+  Changes
*       290994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'millet_end_crop')

*+  Local Variables
      real       dm_residue            ! dry matter added to residue (g/m^2)
      real       N_residue             ! nitrogen added to residue (g/m^2)
      real       dm_root               ! dry matter added to soil (g/m^2)
      real       N_root                ! nitrogen added to soil (g/m^2)
      character  string*400            ! output string
      real       yield                 ! grain wt (kg/ha)

*- Implementation Section ----------------------------------
 
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
      implicit none
      include   'millet.inc'
      include 'date.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       array(*)              ! (OUTPUT) storage array
      real       value                 ! (INPUT) value to be stored

*+  Purpose
*       Stores a value in an annual circular array

*+  Changes
*     230695 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_store_value')

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



*     ================================================================
      subroutine millet_get_other_variables ()
*     ================================================================
      implicit none
      include   'const.inc'
      include   'convert.inc'
      include   'millet.inc'
      include 'data.pub'                          
      include 'engine.pub'                        
      include 'intrface.pub'                      
      include 'error.pub'                         

*+  Purpose
*      Get the values of variables/arrays from other modules.

*+  Assumptions
*      assumes variable has the following format
*         <variable_name> = <variable_value/s> (<units>)

*+  Changes
*     010994 jngh specified and programmed
*     220696 jngh optimised order of gets
*     140896 jngh modified fr_intc_radn name to inclued a suffix of module name

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_get_other_variables')

*+  Local Variables
      integer    layer                 ! layer number
      integer    numvals               ! number of values put into array
      real       dlayer(max_layer)     ! soil layer depths (mm)
      real       NO3(max_layer)        ! soil NO3 content (kg/ha)
      real       NO3_min(max_layer)    ! soil NO3 minimum (kg/ha)
      character  module_name*(Max_module_name_size) ! module name
      real       soil_temp             ! soil surface temperature (oC)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
            ! date
 
      call get_integer_var (unknown_module, 'day', '()'
     :                                    , g_day_of_year, numvals
     :                                    , 1, 366)
 
      call get_integer_var (unknown_module, 'year', '()'
     :                                    , g_year, numvals
     :                                    , c_year_lb, c_year_ub)
 
                               ! canopy
      call get_current_module (module_name)
      call get_real_var_optional (unknown_module
     :                           , 'fr_intc_radn_'//module_name
     :                           , '()'
     :                           , g_fr_intc_radn
     :                           , numvals
     :                           , 0.0
     :                           , 1.0)
 
 
      if (g_stem_class .eq. class_tiller) then
 
cgd leaf_no_ref is the final leaf no for the main shoot needed by tillers
        call get_real_var  (unknown_module, 'leaf_no_ref', '()'
     :                             , g_leaf_no_ref,numvals
     :                             , 0.0, 100.0)
 
      else
         ! main tiller owns leaf_no_ref
      endif
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
cpsc      call get_real_var_optional (unknown_module, 'fr_intc_radn', '()'
cpsc     :                                  , g_fr_intc_radn, numvals
cpsc     :                                  , 0.0, 1.0)
 
      call get_real_var_optional (unknown_module, 'cover_green_sum','()'
     :                                  , g_cover_green_sum, numvals
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
      implicit none
      include   'const.inc'
      include   'convert.inc'
      include   'millet.inc'
      include 'data.pub'                          
      include 'engine.pub'                        
      include 'intrface.pub'                      
      include 'error.pub'                         

*+  Purpose
*      Set the value of a variable or array in other module/s.

*+  Notes
*      a flag is set if any of the totals is requested.  The totals are
*      reset during the next process phase when this happens.

*+  Changes
*     010994 jngh specified and programmed
*     240696 jngh changed set_ to post_ construct

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_set_other_variables')

*+  Local Variables
      real       dlt_NO3(max_layer)    ! soil NO3 change (kg/ha)
      integer    layer                 ! soil layer no.
      integer    num_layers            ! number of layers

*- Implementation Section ----------------------------------
 
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
      implicit none
      include   'millet.inc'            !
      include 'engine.pub'                        
      include 'intrface.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      character  Variable_name*(*)     ! (INPUT) Variable name to search for

*+  Purpose
*      Set a variable in this module as requested by another.

*+  Changes
*      290393 jngh
*      220696 jngh added message_unused to else
*                  changed respond2set to collect

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_set_my_variable')

*+  Local Variables
      integer    numvals               ! number of values found in array

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
             ! **** Repeat for each variable
 
*      if (variable_name .eq. '????') then
*         call collect_real_array (variable_name, '()', max_layer
*     :                               , ????, numvals
*     :                               , 0.0, 1.0)
cejvo used for setting from the manager
cused for validation of leaf area model only
 
      if (variable_name .eq. 'leaf_no_final') then
         call collect_real_var (variable_name
     :                              , '()'
     :                              , g_leaf_no_final, numvals
     :                              , 0.0, 40.0)
 
      elseif (variable_name .eq. 'dlt_leaf_no_pot') then
         call collect_real_var (variable_name
     :                              , '()'
     :                              , g_dlt_leaf_no_pot, numvals
     :                              , 0.0, 40.0)
 
      else
            ! Don't know this variable name
         call message_unused ()
      endif
 
      call pop_routine (my_name)
      return
      end



*     ================================================================
      subroutine millet_send_my_variable (variable_name)
*     ================================================================
      implicit none
      include   'convert.inc'          ! gm2kg, sm2ha, mm2cm, cmm2cc
      include   'millet.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'engine.pub'                        
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
*      220696 jngh added message_unused to else

*+  Calls
      real       millet_swdef          ! function
      real       millet_nfact          ! function
      real       millet_N_fixation     ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_send_my_variable')

*+  Local Variables
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

*- Implementation Section ----------------------------------
 
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
cgd
      elseif (variable_name .eq. 'dlt_tiller_no') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g_dlt_tiller_no)
 
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
cgd
      elseif (variable_name .eq. 'tiller_no') then
         call respond2get_real_array (variable_name
     :                              , '()'
     :                              , g_tiller_no
     :                              , max_stage)
 
      elseif (variable_name .eq. 'leaf_no') then
         call respond2get_real_array (variable_name
     :                              , '()'
     :                              , g_leaf_no
     :                              , max_stage)
 
cgd
      elseif (variable_name .eq. 'leaf_no_final') then
         call respond2get_real_var (variable_name
     :                              , '()'
     :                              , g_leaf_no_final)
cgd
      elseif (variable_name .eq. 'leaf_no_total') then
         call respond2get_real_var (variable_name
     :                              , '()'
     :                              , g_leaf_no_total)
cejvo
      elseif (variable_name .eq. 'leaf_no_effective') then
         call respond2get_real_var (variable_name
     :                              , '()'
     :                              , g_leaf_no_effective)
 
      elseif (variable_name .eq. 'leaf_no_dead_const2') then
         call respond2get_real_var (variable_name
     :                              , '()'
     :                              , g_leaf_no_dead_const2)
 
      elseif (variable_name .eq. 'lf_no_dead_at_flaglf') then
         call respond2get_real_var (variable_name
     :                              , '()'
     :                              , g_lf_no_dead_at_flaglf)
 
      elseif (variable_name .eq. 'leaf_no_dead') then
         call respond2get_real_array (variable_name
     :                              , '()'
     :                              , g_leaf_no_dead
     :                              , max_stage)
 
      elseif ((variable_name .eq. 'leaf_no_ref') .and.
     :   (g_stem_class .eq. class_main)) then
 
         ! only the main stem can respond
 
         call respond2get_real_var (variable_name
     :                              , '()'
     :                              , g_leaf_no_ref)
 
      elseif (variable_name .eq. 'leaf_area') then
         call respond2get_real_array (variable_name
     :                              , '()'
     :                              , g_leaf_area
     :                              , max_leaf)
cejvo
      elseif (variable_name .eq. 'y_tiller_tt_adj') then
         call respond2get_real_array (variable_name
     :                              , '()'
     :                              , g_y_tiller_tt_adj
     :                              , max_table)
 
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
      implicit none
      include   'millet.inc'
      include 'intrface.pub'                      
      include 'error.pub'                         

*+  Purpose
*       Returns true if 'type' is equal to the crop type or is absent.

*+  Assumptions
*       If type is not specified, it is assumed the message was addressed
*        directly to the module.

*+  Changes
*      211294 jngh specified and programmed
*     220696 jngh changed extract to collect

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_my_type')

*+  Local Variables
      character  crop_type*50          ! crop type in data string
      integer    numvals               ! number of values returned

*- Implementation Section ----------------------------------
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
      implicit none
      include   'const.inc'
      include   'millet.inc'
      include 'read.pub'                          
      include 'write.pub'                         
      include 'error.pub'                         

*+  Purpose
*       Crop initialisation - reads constants from constants file

*+  Changes
*     010994 jngh specified and programmed
*     070495 psc added extra constants (leaf_app etc.)
*     110695 psc added soil temp effects on plant establishment
*     261097 gol added constant 'c_photo_tiller_crit'

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'millet_read_constants')
*
      character  section_name*(*)
      parameter (section_name = 'constants')

*+  Local Variables
      integer    numvals               ! number of values returned

*- Implementation Section ----------------------------------
 
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
 
!     call read_real_var (section_name
!    :                    , 'extinction_coef', '()'
!    :                    , c_extinction_coef, numvals
!    :                    , 0.0, 10.0)
 
!     call read_real_var (section_name
!    :                    , 'extinction_coef_dead', '()'
!    :                    , c_extinction_coef_dead, numvals
!    :                    , 0.0, 10.0)
!cpsc
!     call read_real_var (section_name
!    :                    , 'extinction_coef_change', '()'
!    :                    , c_extinction_coef_change, numvals
!    :                    , 0.0, 10.0)
 
cejvo
      call read_real_var (section_name
     :                    , 'row_spacing_default', '()'
     :                    , c_row_spacing_default, numvals
     :                    , 0.0, 2.0)
 
      call read_real_array (section_name
     :                    , 'x_row_spacing', max_table, '(m)'
     :                    , c_x_row_spacing, c_num_row_spacing
     :                    , 0.0, 2.0)
 
      call read_real_array (section_name
     :                    , 'y_extinct_coef', max_table, '()'
     :                    , c_y_extinct_coef, c_num_row_spacing
     :                    , 0.0, 1.0)
 
      call read_real_array (section_name
     :                    , 'y_extinct_coef_dead', max_table, '()'
     :                    , c_y_extinct_coef_dead, c_num_row_spacing
     :                    , 0.0, 1.0)
 
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
 
cejvo             linear interpolate SLA_max
 
      call read_real_array (section_name
     :                    , 'x_lai', max_table, '(mm2/mm2)'
     :                    , c_x_lai, c_num_lai
     :                    , 0.0, 15.0)
 
      call read_real_array (section_name
     :                    , 'y_sla_max', max_table, '(mm2/g)'
     :                    , c_y_sla_max, c_num_lai
     :                    , 0.0, 1000000.0)
 
!      call read_real_var (section_name
!     :                    , 'sla_max', '(mm^2/g)'
!     :                    , c_sla_max, numvals
!     :                    , 0.0, 100000.0)
 
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
 
cgol added new constant "c_photo_tiller_crit" to read from ini file
      call read_real_var (section_name
     :                    , 'photo_tiller_crit', '(hr)'
     :                    , c_photo_tiller_crit, numvals
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
 
cgd
      call read_real_var (section_name
     :                   , 'leaf_no_diff', '()'
     :                   , c_leaf_no_diff, numvals
     :                   , 0.0, 10.0)
 
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
     :                    , -10.0, 10.0)
 
      call read_real_var (section_name
     :                    , 'tfac_slope', '()'
     :                    , c_tfac_slope, numvals
     :                    , 0.0, 100.0)
 
         !    millet_leaf_death
 
      call read_real_var (section_name
     :                    , 'leaf_no_dead_const', '()'
     :                    , c_leaf_no_dead_const, numvals
     :                    , -9.0, 100.0)
cejvo
       call read_real_var (section_name
     :                    , 'leaf_no_dead_slope', '()'
     :                    , c_leaf_no_dead_slope, numvals
     :                    , 0.0, 100.0)
 
      call read_real_var (section_name
     :                    , 'leaf_no_dead_slope1', '()'
     :                    , c_leaf_no_dead_slope1, numvals
     :                    , 0.0, 100.0)
 
      call read_real_var (section_name
     :                    , 'leaf_no_dead_slope2', '()'
     :                    , c_leaf_no_dead_slope2, numvals
     :                    , 0.0, 100.0)
 
         !    millet_get_other_variables
 
         ! checking the bounds of the bounds..
      call read_integer_var (section_name
     :                    , 'year_ub', '()'
     :                    , c_year_ub, numvals
     :                    , 1, 5000)
 
      call read_integer_var (section_name
     :                    , 'year_lb', '()'
     :                    , c_year_lb, numvals
     :                    , 1, 5000)
 
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
 
cgol lower and upper bounds amended to accept ejvo's leaf area parameters
      call read_real_var (section_name
     :                    , 'a_const', '()'
     :                    , c_a_const, numvals
     :                    , -1000.0, 1000.0)
 
      call read_real_var (section_name
     :                    , 'a_slope1', '()'
     :                    , c_a_slope1, numvals
     :                    , -1000.0, 1000.0)
 
      call read_real_var (section_name
     :                    , 'a_slope2', '()'
     :                    , c_a_slope2, numvals
     :                    , -1000.0, 1000.0)
 
      call read_real_var (section_name
     :                    , 'b_const', '()'
     :                    , c_b_const, numvals
     :                    , -1000.0, 1000.0)
 
      call read_real_var (section_name
     :                    , 'b_slope1', '()'
     :                    , c_b_slope1, numvals
     :                    , -1000.0, 1000.0)
 
      call read_real_var (section_name
     :                    , 'b_slope2', '()'
     :                    , c_b_slope2, numvals
     :                    , -1000.0, 1000.0)
 
         !    millet_tiller
 
cgol set maximum tiller number to 5
 
      call read_integer_var (section_name
     :                       , 'tiller_no_pot', '()'
     :                       , c_tiller_no_pot, numvals
     :                       , 0, 5)
 
      call read_char_var (section_name
     :                     , 'tiller_appearance', '()'
     :                     , c_tiller_appearance, numvals)
 
cgol set maximum next tiller number to 6.0
 
      call read_real_array (section_name
     :                     , 'x_tiller_no_next', max_table, '()'
     :                     , c_x_tiller_no_next, c_num_tiller_no_next
     :                     , 0.0, 6.0)
 
      call read_real_array (section_name
     :                     , 'y_tiller_tt', max_table, '(oCd)'
     :                     , c_y_tiller_tt, c_num_tiller_no_next
     :                     , 0.0, 2000.0)
 
      call read_real_var (section_name
     :                     , 'tiller_appearance_slope', '()'
     :                     , c_tiller_appearance_slope, numvals
     :                     , 0.0, 5.0)
 
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
cpsc
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



*     ===========================================================
      real function millet_swdef (type)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'millet.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    type                  ! (INPUT) factor type

*+  Purpose
*       Get the soil water availability factor (0-1), commonly
*       called soil water deficit factor. 1 is no stress, 0 is full stress.

*+  Changes
*       010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_swdef')

*+  Local Variables
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      character  err_messg*100         ! error message
      real       sw_avail_ratio        ! water availability ratio
      real       sw_demand_ratio       ! water supply:demand ratio
      real       sw_avail_pot_sum      ! potential extractable soil water (mm)
      real       sw_avail_sum          ! actual extractable soil water (mm)
      real       sw_supply_sum         ! total supply over profile (mm)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
      deepest_layer = find_layer_no (g_root_depth, g_dlayer, max_layer)
 
      if (type.eq.pheno) then
         sw_avail_pot_sum = sum_real_array (g_sw_avail_pot
     :                                    , deepest_layer)
         sw_avail_sum = sum_real_array (g_sw_avail, deepest_layer)
 
         sw_avail_ratio = divide (sw_avail_sum
     :                          , sw_avail_pot_sum, 1.0) !???
         sw_avail_ratio = bound (sw_avail_ratio , 0.0, 1.0)
 
         millet_swdef = linear_interp_real (sw_avail_ratio
     :                       , c_x_sw_avail_ratio, c_y_swdef_pheno
     :                       , c_num_sw_avail_ratio)
 
      elseif (type.eq.photo) then
            ! get potential water that can be taken up when profile is full
 
         sw_supply_sum = sum_real_array (g_sw_supply, deepest_layer)
         sw_demand_ratio = divide (sw_supply_sum, g_sw_demand, 1.0)
         millet_swdef = bound (sw_demand_ratio , 0.0, 1.0)
 
      elseif (type.eq.expansion) then
            ! get potential water that can be taken up when profile is full
 
         sw_supply_sum = sum_real_array (g_sw_supply, deepest_layer)
         sw_demand_ratio = divide (sw_supply_sum, g_sw_demand, 10.0)
 
         millet_swdef = linear_interp_real (sw_demand_ratio
     :                       , c_x_sw_demand_ratio, c_y_swdef_leaf
     :                       , c_num_sw_demand_ratio)
 
      elseif (type.eq.fixation) then
            ! get potential water that can be taken up when profile is full
 
         sw_avail_pot_sum = sum_real_array (g_sw_avail_pot
     :                                    , deepest_layer)
         sw_avail_sum = sum_real_array (g_sw_avail, deepest_layer)
 
         sw_avail_ratio = divide (sw_avail_sum
     :                          , sw_avail_pot_sum, 1.0) !???
         sw_avail_ratio = bound (sw_avail_ratio , 0.0, 1.0)
 
         millet_swdef = linear_interp_real (sw_avail_ratio
     :                       , c_x_sw_avail_fix, c_y_swdef_fix
     :                       , c_num_sw_avail_fix)
 
      else
         ! we have an unknown type
         write (err_messg, '(a, i5)')
     :                   ' Unknown type of sw deficit. Type ='
     :                   , type
         call fatal_error (err_internal, err_messg)
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_biomass ()
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'error.pub'                         

*+  Purpose
*       Simulate crop biomass processes.

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_biomass')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
      call millet_dm_init (g_dm_green, g_dm_plant_min)
 
            ! drymatter production
      call millet_dm_production (g_dlt_dm)
 
         ! NOTE comment out for hi approach
            ! NOTE comment out the unwanted dm_grain call
            ! note: The dm partition and retranslocate subroutines
            ! implicitly account for both grain no and harvest index
            ! approaches in calculating delta grain.
 
         ! TEMPLATE OPTION
         ! Standard routines (4) simulate grain no approach (Ceres)
         ! Alternative routines (3) simulate harvest index approach (GLH)
      call millet_heat_stress (g_dlt_heat_stress_tt) ! high temperature stress
      call millet_grain_no (g_grain_no)              ! set grain number
      call millet_dm_grain (g_dlt_dm_grain_demand)
 
         ! TEMPLATE OPTION or
      ! call millet_dm_stress_max (g_dlt_dm_stress_max)
      ! call millet_dm_grain_hi (g_dlt_dm_grain_demand)
 
      call millet_dm_partition (g_dlt_dm_green)
      ! call millet_dm_partition_leg (g_dlt_dm_green)
      call millet_dm_retranslocate (g_dlt_dm_green_retrans)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_dm_init (dm_green, dm_plant_min)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dm_green(*)           ! (INPUT/OUTPUT) plant part weights
                                       ! (g/m^2)
      real       dm_plant_min(*)       ! (OUTPUT) minimum weight of each
                                       ! plant part (g/plant)

*+  Purpose
*       Initialise plant weights and plant weight minimums
*       at required instances.

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_dm_init')

*+  Local Variables
      real       dm_plant_leaf         ! dry matter in leaves (g/plant)
      real       dm_plant_stem         ! dry matter in stems (g/plant)

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
         dm_green(tiller) = 0.0
         dm_green(grain) = 0.0
         dm_green(flower) = 0.0
 
      elseif (on_day_of (flowering, g_current_stage, g_days_tot)) then
             ! we are at first day of flowering
             ! set the minimum weight of stem; used for retranslocation to grain
 
         dm_plant_stem = divide (dm_green(stem), g_plants, 0.0)
         dm_plant_min(stem) = dm_plant_stem * (1.0 - c_stem_trans_frac)
 
      elseif (on_day_of (start_grain_fill
     :                 , g_current_stage, g_days_tot)) then
 
             ! we are at first day of grainfill.
             ! set the minimum weight of leaf; used for translocation to grain
 
         dm_plant_leaf = divide (dm_green(leaf), g_plants, 0.0)
         dm_plant_min(leaf) = dm_plant_leaf * (1.0 - c_leaf_trans_frac)
 
      else   ! no changes
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_grain_no (grain_no)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       grain_no              ! (OUTPUT) grains per m^2

*+  Purpose
*     Calculate the grains per m^2 and heads per m^2

*+  Changes
*       111094 jngh specified and programmed
*       250495 psc added head no to output

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'millet_grain_no')

*+  Local Variables
      real       dm_plant              ! dm per plant (g/plant)
      real       head_grain_no         ! grains per plant
      real       head_grain_no_max     ! maximum grains per plant
      real       temp_fac              ! high temperature stress factor (0-1)
      real       N_avail_plant_sum     ! total N available for transfer to
                                       ! grain (g/plant)
      real       N_avail(max_part)     ! nitrogen available for grain
                                       ! (g/m^2)
      real       head_grain_no_optimum ! grains per plant in optimum conditions
      real       grain_no_fract        ! fraction of potential grains/
                                       ! plant
      real       growth_rate           ! average rate of
                                       ! photosynthesis during flowering
                                       ! (g/plant).

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
         ! ------------- find actual grain uptake ---------------
 
      if (on_day_of (start_grain_fill
     :             , g_current_stage, g_days_tot)) then
 
            ! calculate number of grains/plant
            ! Grains/plant is calculated from a genotype-specific
            ! coefficient for potential kernel number and the average
            ! rate of photosynthesis during this stage.  The function
            ! used to predict grains/plant is derived from Edmeades and
            ! Daynard (1979).
 
         dm_plant = sum_between (flag_leaf, now, g_dm_plant_top_tot)
         growth_rate = divide (
     :                   dm_plant
     :                 , sum_between (flag_leaf, now, g_days_tot)
     :                 , 0.0)
 
            ! note - this function will never reach 1. Thus head_grain_no_max
            ! will never be achieved.
 
         grain_no_fract = divide ((growth_rate - c_growth_rate_min)
     :                          , (c_growth_rate_crit
     :                             + (growth_rate - c_growth_rate_min))
     :                          , 0.0)
 
         grain_no_fract = bound (grain_no_fract, 0.0, 1.0)
 
         head_grain_no_optimum = p_head_grain_no_max * grain_no_fract
 
 
            ! grain numbers are reduced by heat stress during flowering.
 
         temp_fac = 1.0
     :            - sum_between (flag_leaf, now, g_heat_stress_tt)
     :            * c_htstress_coeff
 
         temp_fac = bound (temp_fac, 0.0, 1.0)
 
            ! Grain numbers are sensitive to N deficiency occurring
            ! after flowering.  These may be reduced by post-flowering N
            ! deficiency. Calculate the maximum number of grains that can
            ! be produced from the plant's currently available nitrogen
            ! pool - assuming minimum grain weights and grain nitrogen
            ! concentrations to be achieved.
 
            ! In millet_N_conc_limits, the min grain N conc is 0.007
 
         call millet_N_retrans_avail (N_avail)
         N_avail_plant_sum  = divide (sum_real_array (N_avail, max_part)
     :                              , g_plants, 0.0)
         head_grain_no_max = divide (N_avail_plant_sum
     :                      , (c_seed_wt_min * c_grain_N_conc_min), 0.0)
 
         head_grain_no = head_grain_no_optimum * temp_fac
 
         grain_no  =  u_bound (head_grain_no
     :                       , head_grain_no_max)
     :             * g_plants
 
!      print *,'2', grain_no,head_grain_no,head_grain_no_max,
!     :           head_grain_no_optimum,temp_fac,N_avail_plant_sum
!     :          ,c_seed_wt_min,c_grain_N_conc_min
 
      else
            ! do nothing
 
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_heat_stress (dlt_tt_heat_stress)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_tt_heat_stress    ! (OUTPUT) heat stress (oC)

*+  Purpose
*       Calculate heat stress on grain number for the current day.

*+  Changes
*       250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_heat_stress')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
            ! high temperature stress reduces grain no via 'htsf'
 
      if (g_maxt.gt.c_temp_grain_crit_stress) then
         dlt_tt_heat_stress = g_maxt - c_temp_grain_crit_stress
      else
         dlt_tt_heat_stress = 0.0
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_dm_production (dlt_dm)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'data.pub'                          
      include 'science.pub'                       
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_dm                ! (OUTPUT) actual dry matter
                                       ! production (g/m^2)

*+  Purpose
*       Actual dm production (g/m^2)

*+  Changes
*       090994 jngh specified and programmed

*+  Calls
      real       millet_transp_eff      ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_dm_production')

*+  Local Variables
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      real       dlt_dm_transp         ! potential dry matter production
                                       ! by transpiration (g/m^2)
      real       dlt_dm_pot            ! potential dry matter production with
                                       ! optimum water and nitrogen and
                                       ! temperature stress conditions (g/m^2)
      real       sw_supply_sum         ! Water available to roots (mm)

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
         ! potential (supply) by transpiration
 
      deepest_layer = find_layer_no (g_root_depth, g_dlayer, max_layer)
      sw_supply_sum = sum_real_array (g_sw_supply, deepest_layer)
      dlt_dm_transp = sw_supply_sum*millet_transp_eff ()
 
         ! potential by photosynthesis
 
      call millet_dm_potential (dlt_dm_pot)
 
         ! use whichever is limiting
      dlt_dm = min (dlt_dm_pot, dlt_dm_transp)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_dm_partition (dlt_dm_green)
*     ===========================================================
      implicit none
      include   'convert.inc'
      include   'millet.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_dm_green (*)      ! (OUTPUT) actual biomass partitioned
                                       ! to plant parts (g/m^2)

*+  Purpose
*       Partitions new dm (assimilate) between plant components (g/m^2)

*+  Changes
*       010994 jngh specified and programmed
*       250495 psc  modified dlt_dm_green(grain) to account for barren heads

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'millet_dm_partition')

*+  Local Variables
      integer    current_phase         ! current phase no.
      real       dlt_dm_axis           ! dry weight partitioned to
                                       ! axes (excluding tillers)(g/m^2)
      real       dlt_dm_green_tot      ! total of partitioned dm (g/m^2)
      real       dlt_dm_leaf_max       ! max increase in leaf dm (g/m^2)
!cpsc
      real       dlt_dm_grain_max      ! limit to dlt grain wt (g/m^2)
      real       dm_grain_max          ! limit of grain wt based on max. HI (g/m^2)
      real       dm_tops               ! drymatter of tops (g/m^2)
      real       internode_no          ! internode no of stem (leaves emerged
                                       ! since emergence)
      real       partition_coef_leaf   ! partitioning coefficient of dm to
                                       ! leaf (0-1)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
         ! Root must be satisfied. The roots don't take any of the
         ! carbohydrate produced - that is for tops only.  Here we assume
         ! that enough extra was produced to meet demand. Thus the root
         ! growth is not removed from the carbo produced by the model.
 
         ! first we zero all plant component deltas
 
      call fill_real_array (dlt_dm_green, 0.0, max_part)
 
         ! now we get the root delta for all stages - partition scheme
         ! specified in coeff file
 
      current_phase = int (g_current_stage)
      dlt_dm_green(root) = c_ratio_root_shoot(current_phase)*g_dlt_dm
 
      if (stage_is_between (emerg, floral_init, g_current_stage)) then
            ! we have leaf development only
         dlt_dm_green(leaf) = g_dlt_dm
 
         if (c_tiller_no_pot.gt.0) then
 
            dlt_dm_leaf_max = divide (g_dlt_lai_pot
     :                              , c_sla_min * smm2sm, 0.0)
            dlt_dm_green(leaf) = u_bound (dlt_dm_green(leaf)
     :                                  , dlt_dm_leaf_max)
            dlt_dm_green(tiller) = g_dlt_dm - dlt_dm_green(leaf)
 
            dlt_dm_green(tiller) = l_bound (dlt_dm_green(tiller), 0.0)
 
         else
               ! no tillering simulated
            dlt_dm_green(tiller) = 0.0
         endif
 
      elseif (stage_is_between (floral_init, flag_leaf
     :                        , g_current_stage)) then
 
            ! stem elongation and flower development start
            ! Each new leaf demands an increasing proportion of dry matter
            ! partitioned to stem and flower
 
         internode_no = sum_between (floral_init, now, g_leaf_no)
         partition_coef_leaf = 1.0
     :            /(1.0 + c_partition_rate_leaf * internode_no**2)
 
         dlt_dm_green(leaf) = partition_coef_leaf * g_dlt_dm
             ! limit the delta leaf area to maximum
         dlt_dm_leaf_max = divide (g_dlt_lai_pot
     :                           , c_sla_min * smm2sm, 0.0)
         dlt_dm_green(leaf) = u_bound (dlt_dm_green(leaf)
     :                               , dlt_dm_leaf_max)
 
         if (c_tiller_no_pot.gt.0) then
 
            dlt_dm_axis = divide (dlt_dm_green(leaf)
     :                          , partition_coef_leaf, g_dlt_dm)
 
            dlt_dm_green(tiller) = g_dlt_dm - dlt_dm_axis
 
            dlt_dm_green(tiller) = l_bound (dlt_dm_green(tiller), 0.0)
 
         else
            dlt_dm_axis = g_dlt_dm
            dlt_dm_green(tiller) = 0.0
 
         endif
 
         dlt_dm_green(flower) = (dlt_dm_axis - dlt_dm_green(leaf))
     :                        * c_frac_stem2flower
 
         dlt_dm_green(stem) = dlt_dm_axis
     :                      - (dlt_dm_green(flower)
     :                      + dlt_dm_green(leaf))
 
         dlt_dm_green(flower) = l_bound (dlt_dm_green(flower), 0.0)
         dlt_dm_green(stem) = l_bound (dlt_dm_green(stem), 0.0)
 
 
      elseif (stage_is_between (flag_leaf, start_grain_fill
     :                        , g_current_stage)) then
 
            ! we only have flower and stem growth here
         dlt_dm_green(flower) = g_dlt_dm*c_frac_stem2flower
         dlt_dm_green(stem) = g_dlt_dm - dlt_dm_green(flower)
         dlt_dm_green(stem) = l_bound (dlt_dm_green(stem), 0.0)
 
      elseif (stage_is_between (start_grain_fill, maturity
     :                        , g_current_stage)) then
 
            ! grain filling starts - stem continues when it can
 
!cpsc  bound HI to a maximum value
         dm_tops = sum_real_array (g_dm_green, max_part)
     :           - g_dm_green(root)
     :           + sum_real_array (g_dm_senesced, max_part)
     :           - g_dm_senesced(root)
 
         dm_grain_max = (dm_tops + g_dlt_dm) * p_hi_max_pot
         dlt_dm_grain_max = bound (dm_grain_max - g_dm_green(grain)
     :                           , 0.0, g_dlt_dm)
 
         dlt_dm_green(grain) = bound (g_dlt_dm_grain_demand
     :                              , 0.0, dlt_dm_grain_max)
 
!cpsc
!         dlt_dm_green(grain) = bound (g_dlt_dm_grain_demand
!     :                              , 0.0, g_dlt_dm)
 
         dlt_dm_green(stem) = g_dlt_dm - dlt_dm_green(grain)
 
         dlt_dm_green(stem) = l_bound (dlt_dm_green(stem), 0.0)
 
 
      elseif (stage_is_between (maturity, plant_end
     :                        , g_current_stage)) then
 
            ! put into stem
         dlt_dm_green(stem) = g_dlt_dm
 
      else
            ! no partitioning
      endif
 
         ! do mass balance check - roots are not included
      dlt_dm_green_tot = sum_real_array (dlt_dm_green, max_part)
     :                 - dlt_dm_green(root)
      call bound_check_real_var (dlt_dm_green_tot, g_dlt_dm, g_dlt_dm
     :                        , 'dlt_dm_green_tot mass balance')
 
         ! check that deltas are in legal range
 
      call bound_check_real_array (dlt_dm_green, 0.0, g_dlt_dm
     :                          , 'dlt_dm_green', max_part)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_dm_partition_leg (dlt_dm_green)
*     ===========================================================
      implicit none
      include   'convert.inc'
      include   'millet.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_dm_green (*)      ! (OUTPUT) actual biomass partitioned
                                       ! to plant parts (g/m^2)

*+  Purpose
*       Partitions new dm (assimilate) between plant components (g/m^2)

*+  Changes
*       010994 jngh specified and programmed
*       250495 psc  modified dlt_dm_green(grain) to account for barren heads

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'millet_dm_partition_leg')

*+  Local Variables
      integer    current_phase         ! current phase no.
      real       dlt_dm_green_tot      ! total of partitioned dm (g/m^2)
      real       dlt_dm_leaf_max       ! max increase in leaf dm (g/m^2)
      real       partition_grain       ! fraction of dm partitioned to grain
                                       ! versus flower
      real       dm_remaining

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
         ! Root must be satisfied. The roots don't take any of the
         ! carbohydrate produced - that is for tops only.  Here we assume
         ! that enough extra was produced to meet demand. Thus the root
         ! growth is not removed from the carbo produced by the model.
 
         ! first we zero all plant component deltas
 
      call fill_real_array (dlt_dm_green, 0.0, max_part)
 
         ! now we get the root delta for all stages - partition scheme
         ! specified in coeff file
 
      current_phase = int (g_current_stage)
      dlt_dm_green(root) = c_ratio_root_shoot(current_phase)*g_dlt_dm
 
      if (stage_is_between (emerg, flowering, g_current_stage)) then
 
         dlt_dm_green(leaf) = c_frac_leaf_pre_flower * g_dlt_dm
 
             ! limit the delta leaf area to maximum
         dlt_dm_leaf_max = divide (g_dlt_lai_pot
     :                           , c_sla_min * smm2sm, 0.0)
         dlt_dm_green(leaf) = u_bound (dlt_dm_green(leaf)
     :                               , dlt_dm_leaf_max)
 
         dlt_dm_green(stem) = g_dlt_dm - dlt_dm_green(leaf)
 
      elseif (stage_is_between (flowering, start_grain_fill
     :                        , g_current_stage)) then
 
            ! we now have leaf growth during this period as well as
            ! flower and stem growth. jet.
 
         dlt_dm_green(leaf) = c_frac_leaf_post_flower * g_dlt_dm
 
             ! limit the delta leaf area to maximum
         dlt_dm_leaf_max = divide (g_dlt_lai_pot
     :                           , c_sla_min * smm2sm, 0.0)
         dlt_dm_green(leaf) = u_bound (dlt_dm_green(leaf)
     :                               , dlt_dm_leaf_max)
 
         dm_remaining = g_dlt_dm - dlt_dm_green(leaf)
         dlt_dm_green(flower) = dm_remaining * c_frac_stem2flower
         dlt_dm_green(stem) = g_dlt_dm - (dlt_dm_green(flower)
     :                                   + dlt_dm_green(leaf))
 
      elseif (stage_is_between (start_grain_fill, maturity
     :                        , g_current_stage)) then
 
            ! grain filling starts - stem continues when it can
 
         partition_grain = 1.0 - divide (c_frac_flower2grain,
     :                                  c_frac_flower2grain + 1.0, 0.0)
         dlt_dm_green(grain) = bound (g_dlt_dm_grain_demand, 0.0,
     :                              g_dlt_dm * partition_grain)
         dlt_dm_green(flower) = dlt_dm_green(grain)
     :                        * c_frac_flower2grain
 
         if (dlt_dm_green(flower)+dlt_dm_green(grain).ge.g_dlt_dm) then
            dlt_dm_green(flower) = g_dlt_dm - dlt_dm_green(grain)
            dlt_dm_green(stem) = 0.0
         else
            dlt_dm_green(stem) = g_dlt_dm
     :                    - (dlt_dm_green(grain)+dlt_dm_green(flower))
         endif
 
      elseif (stage_is_between (maturity, plant_end
     :                        , g_current_stage)) then
 
            ! put into stem
         dlt_dm_green(stem) = g_dlt_dm
 
      else
            ! no partitioning
      endif
 
         ! do mass balance check - roots are not included
      dlt_dm_green_tot = sum_real_array (dlt_dm_green, max_part)
     :                 - dlt_dm_green(root)
 
      call bound_check_real_var (dlt_dm_green_tot, g_dlt_dm, g_dlt_dm
     :                        , 'dlt_dm_green_tot mass balance')
 
         ! check that deltas are in legal range
 
      call bound_check_real_array (dlt_dm_green, 0.0, g_dlt_dm
     :                          , 'dlt_dm_green', max_part)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_dm_grain (dlt_dm_grain_demand)
*     ===========================================================
      implicit none
      include   'convert.inc'          ! mg2gm
      include   'millet.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_dm_grain_demand   ! (OUTPUT) grain dry matter potential
                                       ! (g/m^2)

*+  Purpose
*        Find grain demand for carbohydrate (g/m^2)

*+  Changes
*     010994 jngh specified and programmed

*+  Calls
      real       millet_dm_grain_max    ! function
      real       millet_swdef           ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_dm_grain')

*+  Local Variables
      real       dlt_dm_grain          ! grain demand for carbohydrate
                                       ! (g/m^2)
      real       fract_of_optimum      ! fraction of optimum conditions (0-1)
      real       dlt_dm_grain_optm     ! potential grain growth (g/m^2)
      real       rgfill                ! relative rate of grain fill for the
                                       ! day (0-1) due to temperature
                                       ! response (average of periods)
      real       sw_def_fac            ! water stress factor (0-1)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (stage_is_between (start_grain_fill, end_grain_fill
     :                    , g_current_stage)) then
 
            ! effective grain filling period
 
            ! calculate the relative rate of grain fill (0-1) eight times
            ! for the day, using interpolated 3 hourly mean temperatures.
            ! this is a temperature stress factor.
 
 
            ! The old cm function had the optimum temperature at 26
            ! with the range being 6-46 and was a quadratic.
            ! This was changed with the optimum at 30 with the range
            ! being 17.57-42.43, still a quadratic.
            ! It now has a range 3.68 - 56.32 and is stepwise linear
 
         rgfill = linint_3hrly_temp (g_maxt, g_mint
     :                             , c_x_temp_grain, c_y_grain_rate
     :                             , c_num_temp_grain)
 
 
            ! get water stress factor
 
         sw_def_fac = (c_swdf_grain_min
     :              + (1.0 - c_swdf_grain_min) * millet_swdef(photo))
         fract_of_optimum = rgfill * sw_def_fac
 
            ! now calculate the grain growth demand for the day in g/m^2
 
         dlt_dm_grain_optm = g_grain_no * (p_grain_gth_rate * mg2gm)
         dlt_dm_grain = bound (dlt_dm_grain_optm * fract_of_optimum
     :                       , 0.0, millet_dm_grain_max ())
 
      else
            ! we are out of grain fill period
 
         dlt_dm_grain = 0.0
      endif
 
      dlt_dm_grain_demand = dlt_dm_grain
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_dm_grain_hi (dlt_dm_grain_demand)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_dm_grain_demand   ! (OUTPUT) grain dry matter potential
                                       ! (g/m^2)

*+  Purpose
*        Find grain demand for carbohydrate using harvest index (g/m^2)

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_dm_grain_hi')

*+  Local Variables
      real       dlt_dm_grain          ! grain demand for carbohydrate
                                       ! (g/m^2)
      real       dm_tops               ! drymatter of tops (g/m^2)
      real       harvest_index         ! last harvest index (g grain/g biomass)
      real       dm_tops_new           ! new drymatter  tops (g/m^2)
      real       harvest_index_new     ! next harvest index (g grain/g biomass)
      real       dm_grain_new          ! new drymatter grain (g/m^2)
      real       stress_sum            !
      real       ave_stress            !
      real       days_sum              !
      real       hi_max                !

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (stage_is_between (start_grain_fill, maturity
     :                    , g_current_stage)) then
 
            ! effective grain filling period
 
         stress_sum = sum_between (flag_leaf, start_grain_fill
     :                            , g_dm_stress_max)
         days_sum = sum_between (flag_leaf, start_grain_fill
     :                         , g_days_tot)
         ave_stress = divide (stress_sum, days_sum, 1.0)
         hi_max = c_hi_min + (p_hi_max_pot - c_hi_min) * ave_stress
 
         dm_tops = sum_real_array (g_dm_green, max_part)
     :           - g_dm_green(root)
     :           + sum_real_array (g_dm_senesced, max_part)
     :           - g_dm_senesced(root)
         harvest_index = divide (g_dm_green(grain), dm_tops, 0.0)
         dm_tops_new = dm_tops + g_dlt_dm
 
         harvest_index_new = u_bound (harvest_index + p_hi_incr, hi_max)
 
         dm_grain_new = dm_tops_new * harvest_index_new
         dlt_dm_grain = dm_grain_new - g_dm_green(grain)
         dlt_dm_grain = bound (dlt_dm_grain, 0.0, dm_grain_new)
 
      else
            ! we are out of grain fill period
 
         dlt_dm_grain = 0.0
      endif
 
      dlt_dm_grain_demand = dlt_dm_grain
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_dm_stress_max (dlt_dm_stress_max)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_dm_stress_max     ! (OUTPUT) max daily stress (0-1)

*+  Purpose
*        Find maximum stress on daily dm production (0-1)

*+  Assumptions
*       Here we assume that the soil water stress factor has included stress
*       factors that reduce RUE. The stress returned from here is the
*       maximum stress experienced relative to all factors non limiting.

*+  Changes
*     010994 jngh specified and programmed

*+  Calls
      real       millet_rue_reduction   ! function
      real       millet_swdef           ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_dm_stress_max')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      dlt_dm_stress_max = millet_swdef (photo)
     :                    * millet_rue_reduction ()
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      real function millet_dm_grain_max ()
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Purpose
*       Maximum grain growth for available nitrogen (g/m^2)

*+  Changes
*       141093 jngh specified and programmed

*+  Calls
      real       millet_N_dlt_grain_conc ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_dm_grain_max')

*+  Local Variables
      real       N_avail(max_part)     ! nitrogen available for grain uptake
                                       ! from each part (g/m^2)
      real       N_avail_sum           ! total nitrogen available for grain
                                       ! uptake (g/m^2)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      call millet_N_retrans_avail (N_avail)
      N_avail_sum = sum_real_array (N_avail, max_part)
 
      millet_dm_grain_max = divide (N_avail_sum
     :                           , millet_N_dlt_grain_conc ()
     :                           , 0.0)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_dm_retranslocate (dm_retranslocate)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dm_retranslocate(*)   ! (INPUT) actual change in plant part
                                       ! weights due to translocation (g/m^2)

*+  Purpose
*     Calculate plant dry matter delta's due to retranslocation (g/m^2)

*+  Changes
*       010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'millet_dm_retranslocate')

*+  Local Variables
      real       dlt_dm_retrans_leaf   ! carbohydrate removed from leaf
                                       ! (g/m^2)
      real       dlt_dm_retrans_stem   ! carbohydrate removed from stem in
                                       ! (g/m^2)
      real       dm_grain_differential ! demand in excess of available supply
                                       ! (g/m^2)
      real       dm_leaf_avail         ! carbohydrate avail from leaf(g/m^2)
      real       dm_stem_avail         ! carb available from stem (g/m^2)
      real       dm_leaf_pot           ! potential leaf weight (g/m^2)
      real       dm_stem_pot           ! potential stem weight (g/m^2)
      real       mass_balance          ! sum of translocated carbo (g/m^2)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
         ! now translocate carbohydrate between plant components
         ! this is different for each stage
 
      call fill_real_array (dm_retranslocate, 0.0, max_part)
 
      if (stage_is_between (start_grain_fill, maturity
     :                    , g_current_stage)) then
 
         if (g_dlt_dm_grain_demand .gt. g_dlt_dm_green(grain)) then
               ! we can translocate stem and leaf carbohydrate
               ! to grain if needed
 
            dm_grain_differential = g_dlt_dm_grain_demand
     :                            - g_dlt_dm_green(grain)
 
               ! get available carbohydrate in stem and leaf for transfer
 
            dm_stem_pot = g_dm_green(stem) + g_dlt_dm_green(stem)
            dm_stem_avail = dm_stem_pot - g_dm_plant_min(stem)*g_plants
            dm_stem_avail = l_bound (dm_stem_avail, 0.0)
 
            dm_leaf_pot = g_dm_green(leaf) + g_dlt_dm_green(leaf)
            dm_leaf_avail = dm_leaf_pot - g_dm_plant_min(leaf)*g_plants
            dm_leaf_avail = l_bound (dm_leaf_avail, 0.0)
 
               ! now find out how much to translocate.
               ! first chop at stem
 
            dlt_dm_retrans_stem = min (dm_grain_differential
     :                               , dm_stem_avail)
 
               ! second chop at leaf
 
            dm_grain_differential = dm_grain_differential
     :                            - dlt_dm_retrans_stem
            dlt_dm_retrans_leaf = min (dm_grain_differential
     :                               , dm_leaf_avail)
            dlt_dm_retrans_leaf = bound (dlt_dm_retrans_leaf
     :                                 , 0.0, dm_leaf_avail)
 
               ! get actual growth/withdrawal
 
            call fill_real_array (dm_retranslocate, 0.0, max_part)
            dm_retranslocate(stem) = - dlt_dm_retrans_stem
            dm_retranslocate(leaf) = - dlt_dm_retrans_leaf
            dm_retranslocate(grain) = - sum_real_array (dm_retranslocate
     :                                                , max_part)
 
               ! ??? check that stem and leaf are >= min wts
         else
               ! we have no retranslocation
            call fill_real_array (dm_retranslocate, 0.0, max_part)
         endif
 
      else
 
            ! we have no retranslocation
         call fill_real_array (dm_retranslocate, 0.0, max_part)
 
      endif
 
         ! now check that we have mass balance
 
      mass_balance = sum_real_array (dm_retranslocate, max_part)
      call bound_check_real_var (mass_balance, 0.0, 0.0
     :                         , 'dm_retranslocate mass balance')
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_detachment ()
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'error.pub'                         

*+  Purpose
*       Simulate plant detachment.

*+  Changes
*      091294 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_detachment')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      call millet_dm_detachment (g_dlt_dm_detached)
      call millet_slai_detachment (g_dlt_slai_detached)
      call millet_N_detachment (g_dlt_N_detached)
 
      call millet_dm_dead_detachment (g_dlt_dm_dead_detached)
      call millet_tlai_dead_detachment (g_dlt_tlai_dead_detached)
      call millet_N_dead_detachment (g_dlt_N_dead_detached)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_dm_detachment (dlt_dm_detached)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_dm_detached(*)    ! (OUTPUT) actual biomass detached
                                       ! from senesced plant parts (g/m^2)

*+  Purpose
*       Derives detachment of seneseced plant dry matter (g/m^2)

*+  Changes
*       091294 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'millet_dm_detachment')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
         ! first we zero all plant component deltas
 
      call fill_real_array (dlt_dm_detached, 0.0, max_part)
 
      dlt_dm_detached(leaf) = g_dm_senesced(leaf)
     :                      * c_dm_leaf_detach_frac
      dlt_dm_detached(root) = g_dlt_dm_senesced(root)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_slai_detachment (dlt_slai_detached)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_slai_detached     ! (OUTPUT) lai detached from senesced
                                       ! plant leaf

*+  Purpose
*       Derives detachment of seneseced plant slai (g/m^2)

*+  Changes
*       091294 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'millet_slai_detachment')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      dlt_slai_detached = g_slai * c_dm_leaf_detach_frac
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_N_detachment (dlt_N_detached)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_N_detached(*)     ! (OUTPUT) actual nitrogen senesced
                                       ! from plant parts (g/m^2)

*+  Purpose
*       Derives seneseced plant nitrogen (g N/m^2)

*+  Changes
*       091294 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'millet_N_detachment')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
         ! first we zero all plant component deltas
 
      call fill_real_array (dlt_N_detached, 0.0, max_part)
 
      dlt_N_detached(leaf) = g_dlt_N_senesced(leaf)
     :                     * c_dm_leaf_detach_frac
      dlt_N_detached(root) = g_dlt_N_senesced(root)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_plant_death ()
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'error.pub'                         

*+  Purpose
*      Determine plant death in crop

*+  Changes
*       290994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_plant_death')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      call millet_plants (g_dlt_plants)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_plants (dlt_plants)
*     ===========================================================
      implicit none
      include   'const.inc'            ! new_line
      include   'millet.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'write.pub'                         
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_plants            ! (OUTPUT) change in plant number

*+  Purpose
*      Determine plant death.

*+  Changes
*       290994 jngh specified and programmed
*       110695 psc  added plant death from high soil temp
*       100795 jngh moved millet_kill crop to end of routine

*+  Calls
      real       millet_swdef           ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_plants')

*+  Local Variables
      real       cswd_pheno            ! cumulative water stress for phenology
      real       cswd_photo            ! cumulative water stress for photoperiod
      integer    days_after_emerg      ! days after emergence (days)
      real       dlt_plants_all        ! death - whole crop (plants/m^2)
      real       dlt_plants_temp       ! death from high temperature (plants/m^2)
      real       dlt_plants_water      ! death from water stress (plants/m^2)
      real       dlt_plants_barren     ! death from barrenness (plants/m^2)
      real       leaf_no               ! number of leaves
      real       killfr                ! fraction of crop population to kill
      character  string*200            ! output string

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      cswd_pheno = sum_between (emerg, flag_leaf, g_cswd_pheno)
      cswd_photo = sum_between (emerg, flag_leaf, g_cswd_photo)
      leaf_no = sum_between (emerg, now, g_leaf_no)
 
      if (stage_is_between (sowing, germ, g_current_stage)
     :   .and. sum_between (sowing, now, g_days_tot)
     :         .ge.c_days_germ_limit) then
 
         dlt_plants_all = - g_plants
 
         write (string, '(3a, i4, a)')
     :                 ' crop failure because of lack of'
     :                  ,new_line
     :                  ,'         germination within'
     :                  , c_days_germ_limit
     :                  , ' days of sowing'
         call write_string (lu_scr_sum, string)
 
      elseif (stage_is_between (germ, emerg, g_current_stage)
     :       .and. sum_between (germ, now, g_tt_tot)
     :       .gt.c_tt_emerg_limit) then
 
         dlt_plants_all = - g_plants
 
         write (string, '(a)')
     :                 ' failed emergence due to deep planting'
         call write_string (lu_scr_sum, string)
 
      elseif (reals_are_equal (g_lai, 0.0)
     :       .and. stage_is_between (floral_init, plant_end
     :                             , g_current_stage)) then
 
         dlt_plants_all = - g_plants
 
         write (string, '(3a)')
     :                ' crop failure because of total leaf senescence.'
         call write_string (lu_scr_sum, string)
 
      elseif (stage_is_between (emerg, flag_leaf, g_current_stage)
     :       .and. cswd_pheno.ge.c_swdf_pheno_limit) then
 
         dlt_plants_all = - g_plants
 
         write (string, '(3a)')
     :                 '         crop failure because of prolonged'
     :                ,new_line
     :                ,'         phenology delay through water stress.'
         call write_string (lu_scr_sum, string)
 
      else
         dlt_plants_all = 0.0
 
      endif
 
!cpsc  add code to kill plants for high soil surface temperatures
cjh
      days_after_emerg = int (sum_between (emerg, now, g_days_tot)) - 1
      if (days_after_emerg .eq. 1) then
 
         call millet_plants_temp (killfr)
         dlt_plants_temp = - g_plants*killfr
 
         if (killfr .gt. 0.0) then
            write (string, '(a, i4, a)')
     :            'plant_kill.'
     :          , nint (killfr*100.0)
     :          , '% failure because of high soil surface temperatures.'
 
            call report_event (string)
 
         else
                  ! do nothing
         endif
 
      else
         dlt_plants_temp = 0.0
 
      endif
 
      if (leaf_no.lt.c_leaf_no_crit
     :       .and. cswd_photo.gt.c_swdf_photo_limit
     :       .and. millet_swdef(photo).lt.1.0) then
 
         killfr = c_swdf_photo_rate* (cswd_photo - c_swdf_photo_limit)
         killfr = bound (killfr, 0.0, 1.0)
         dlt_plants_water = - g_plants*killfr
 
         write (string, '(a, i4, a)')
     :          'plant_kill.'
     :         , nint (killfr*100.0)
     :         , '% failure because of water stress.'
 
         call report_event (string)
 
      else
         dlt_plants_water = 0.0
 
      endif
 
      if (on_day_of (start_grain_fill
     :             , g_current_stage, g_days_tot)) then
         call millet_plants_barren (killfr)
         dlt_plants_barren = - g_plants*killfr
 
         if (killfr .gt. 0.0) then
            write (string, '(a, i4, a)')
     :             'plant_kill.'
     :            , nint (killfr*100.0)
     :            , '% failure because of barreness.'
 
         call report_event (string)
 
         else
                  ! do nothing
         endif
 
 
      elseif (g_plant_status .eq. status_dead) then
 
         dlt_plants = - g_plants
 
         write (string, '(3a)')
     :                ' crop killed because of external action.'
         call write_string (lu_scr_sum, string)
 
      else
         dlt_plants_barren = 0.0
 
      endif
 
      dlt_plants = min (dlt_plants_all
     :                , dlt_plants_temp
     :                , dlt_plants_water
     :                , dlt_plants_barren)
 
      if (reals_are_equal (dlt_plants + g_plants, 0.0)) then
         call millet_kill_crop ()
      else
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_plants_temp (killfr)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'science.pub'                       
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       killfr                ! (OUTPUT) fraction of plants killed
                                       ! (plants/m^2)

*+  Purpose
*        Calculate fraction of plants killed by high temperature during
*        emergence (0-1).

*+  Changes
*     230695 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_plants_temp')

*+  Local Variables
      integer    day_before            ! day of year number of day before
                                       ! yesterday ()
      real       weighted_temp         ! 3 day weighted soil temperature (oC)
      integer    yesterday             ! day of year number of yesterday

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      yesterday = offset_day_of_year (g_year, g_day_of_year, - 1)
      day_before = offset_day_of_year (g_year, g_day_of_year, - 2)
 
      weighted_temp = 0.25 * g_soil_temp(day_before)
     :              + 0.50 * g_soil_temp(yesterday)
     :              + 0.25 * g_soil_temp(g_day_of_year)
 
      killfr = linear_interp_real (weighted_temp
     :                           , c_x_weighted_temp
     :                           , c_y_plant_death
     :                           , c_num_weighted_temp)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_plants_barren (killfr)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       killfr                ! (OUTPUT) fraction of plants killed
                                       ! (plants/m^2)

*+  Purpose
*        Calculate fraction of barren heads (0-1).
*        Allows no more than 1 head per plant.

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_plants_barren')

*+  Local Variables
      real       fract_of_optimum      ! fraction of optimum no. of heads due
                                       ! to barrenness below which some
                                       ! heads become barren. (0-1)
      real       head_grain_no         ! (grains/head)

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      call millet_check_grain_no ()
 
         ! determine barrenness
 
      head_grain_no = divide (g_grain_no, g_plants, 0.0)
 
      if (head_grain_no.le.c_head_grain_no_crit) then
            ! all heads barren
         fract_of_optimum = 0.0
 
      elseif (head_grain_no.lt.p_head_grain_no_max * c_barren_crit) then
            ! we have some barren heads
         fract_of_optimum =
     :              (divide (head_grain_no - c_head_grain_no_crit
     :                     , p_head_grain_no_max - c_head_grain_no_crit
     :                     , 0.0))
     :              **0.33
 
      else
            ! we have no barren heads
         fract_of_optimum = 1.0
      endif
 
      fract_of_optimum = bound (fract_of_optimum, 0.0, 1.0)
      killfr = 1.0 - fract_of_optimum
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_check_grain_no ()
*     ===========================================================
      implicit none
      include   'const.inc'            ! err_user
      include   'millet.inc'
      include 'error.pub'                         

*+  Purpose
*        Check validity of grain no. parameters

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_check_grain_no')

*+  Local Variables
      character  err_messg*200         ! error message

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (c_head_grain_no_crit.gt.p_head_grain_no_max*c_barren_crit
     :   .and. p_head_grain_no_max.gt.0.0) then
         write (err_messg,'(a, g16.7e2, a, g16.7e2, 3a, g16.7e2, a)')
     :               'critical grain no. ('
     :              , c_head_grain_no_crit
     :              ,') exceeds  ('
     :              , p_head_grain_no_max*c_barren_crit
     :              ,')'
     :              ,new_line
     :              ,'        which is '
     :              , c_barren_crit
     :              ,' of potential.'
         call warning_error (err_user, err_messg)
 
      else
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_dm_dead_detachment (dlt_dm_dead_detached)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_dm_dead_detached(*)   ! (OUTPUT) change in dm of dead
                                           ! plants (g/m^2)

*+  Purpose
*      Plant dry matter loss from dead plants

*+  Changes
*       091294 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_dm_dead_detachment')

*+  Local Variables
      integer    part                  ! part index

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      do 1000 part = 1, max_part
         dlt_dm_dead_detached(part) = g_dm_dead(part)
     :                              * c_dead_detach_frac(part)
1000  continue
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_tlai_dead_detachment (dlt_tlai_dead_detached)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_tlai_dead_detached   ! (OUTPUT) change in lai of dead
                                          ! plants

*+  Purpose
*      Plant leaf area loss from dead plants

*+  Changes
*       091294 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_tlai_dead_detachment')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      dlt_tlai_dead_detached = g_tlai_dead * c_dead_detach_frac(leaf)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_N_dead_detachment (dlt_N_dead_detached)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_N_dead_detached(*)   ! (OUTPUT) change in dm of dead
                                          ! plants (g/m^2)

*+  Purpose
*      Plant Nitrogen loss from dead plants

*+  Changes
*       091294 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_N_dead_detachment')

*+  Local Variables
      integer    part                  ! part index

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      do 1000 part = 1, max_part
         dlt_N_dead_detached(part) = g_N_dead(part)
     :                             * c_dead_detach_frac(part)
1000  continue
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_kill_crop ()
*     ===========================================================
      implicit none
      include   'convert.inc'          ! gm2kg, sm2ha, mm2cm, cmm2cc
      include   'millet.inc'
      include 'data.pub'                          
      include 'write.pub'                         
      include 'error.pub'                         

*+  Purpose
*       Kill crop

*+  Changes
*       290994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'millet_kill_crop')

*+  Local Variables
      real       biomass               ! above ground dm (kg/ha)
      character  string*200            ! output string

*- Implementation Section ----------------------------------
 
c+!!!!!! fix problem with deltas in update when change from alive to dead ?zero deltas
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
     :                  ' millet_kill. Standing above-ground dm = '
     :                  , biomass, ' (kg/ha)'
         call report_event (string)
 
      else
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_leaf_area_potential ()
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'error.pub'                         

*+  Purpose
*       Simulate potential crop leaf area development - may be limited by
*       DM production in subsequent routine

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_leaf_area_potential')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
            ! Plant leaf development
            ! initialise total leaf number
      call millet_leaf_area_init (g_lai)
      call millet_leaf_no_final (g_leaf_no_final)
      call millet_leaf_no_init(g_leaf_no)
      call millet_leaf_appearance (g_dlt_leaf_no_pot) ! fraction of leaf emerged
         ! TEMPLATE OPTION
         ! Two alternative leaf area routines
      call millet_leaf_area_devel (g_dlt_lai_pot) ! individual leaf approach
         ! TEMPLATE OPTION or
      ! call millet_leaf_area_devel_leg (g_dlt_lai_pot) ! individual leaf approach
      ! call millet_leaf_area_devel_plant (g_dlt_lai_pot) ! whole plant approach
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_leaf_area_init (lai)
*     ===========================================================
      implicit none
      include   'convert.inc'
      include   'millet.inc'
      include 'science.pub'                       
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       lai                   ! (OUTPUT) total plant leaf area

*+  Purpose
*       Initialise leaf area.

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_leaf_area_init')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (on_day_of (emerg, g_current_stage, g_days_tot)) then
         lai = c_initial_tpla * smm2sm * g_plants
      else
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_leaf_no_final (leaf_no_final)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       leaf_no_final         ! (OUTPUT) maximum total leaf number

*+  Purpose
*       Calculate total leaf number.  This is set at floral_initialising and
*       is set at an approximated number at germination to allow
*       other calculations to proceed until the correct number is known.

*+  Changes
*     010994 jngh specified and programmed
*     070495 psc  changed from emerg to germ

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_leaf_no_final')

*+  Local Variables
cglh      real       tt_cum                ! cumulative dtt from sowing (deg day)
      real       tt_floral_init        ! cumulative dtt from sowing
                                       ! to true floral initiation (deg day)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
          ! set total leaf number
 
      if (on_day_of (emerg, g_current_stage, g_days_tot)) then
 
               ! estimate the final leaf no from an approximated thermal
               ! time for the period from emergence to floral initiation.
 
         tt_floral_init = sum_between (germ, floral_init, g_phase_tt)
cglh         tt_cum = sum_between (emerg, floral_init, g_phase_tt)
cglh         tt_floral_init = tt_cum - c_floral_init_error
 
            ! just check that maths is ok.
cglh         call bound_check_real_var (tt_floral_init, 0.0, tt_cum
cglh     :                             , 'tt_floral_init')
         leaf_no_final = divide (tt_floral_init
     :                         , c_leaf_init_rate, 0.0)
     :                 + c_leaf_no_seed
 
         call bound_check_real_var (leaf_no_final
     :                            , c_leaf_no_min, c_leaf_no_max
     :                            , 'leaf_no_final')
 
      elseif (on_day_of (floral_init, g_current_stage, g_days_tot)) then
 
               ! now we know the thermal time, get the actual final leaf no.
 
         tt_floral_init = sum_between (germ, floral_init, g_tt_tot)
 
cglh         tt_cum = sum_between (emerg, floral_init, g_tt_tot)
cglh         tt_floral_init = tt_cum - c_floral_init_error
 
            ! just check that maths is ok.
cglh         call bound_check_real_var (tt_floral_init, 0.0, tt_cum
cglh     :                             , 'tt_floral_init')
 
         if (g_stem_class .eq. class_main) then
 
            ! calculate main stem final leaf no.
 
            leaf_no_final = divide (tt_floral_init
     :                         , c_leaf_init_rate, 0.0)
     :                 + c_leaf_no_seed
 
cgd
            g_leaf_no_ref = leaf_no_final
 
         else
            ! tillers use main stem leaf no final
         endif
 
 
         leaf_no_final = g_leaf_no_ref - c_leaf_no_diff
 
!         write (*,*) leaf_no_final,g_leaf_no_ref,c_leaf_no_diff
!         write (*,*) 'fln',leaf_no_final
!
!         write (*,*) g_leaf_no_ref,leaf_no_final
!
!         write (*,*) g_current_stage
!         write (*,*) leaf_no_final,tt_floral_init,c_leaf_init_rate
         call bound_check_real_var (leaf_no_final
     :                            , c_leaf_no_min, c_leaf_no_max
     :                            , 'leaf_no_final')
 
      elseif (on_day_of (plant_end, g_current_stage, g_days_tot)) then
         leaf_no_final = 0.0
 
      else
         ! leave leaf_no_final as is.
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_leaf_appearance (dlt_leaf_no_pot)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_leaf_no_pot       ! (OUTPUT) new fraction of oldest
                                       ! expanding leaf

*+  Purpose
*       Return the fractional increase in emergence of the oldest
*       expanding leaf.
*       Note ! this does not take account of the other younger leaves
*       that are currently expanding

*+  Changes
*       031194 jngh specified and programmed
*       070495 psc  added 2nd leaf appearance rate

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_leaf_appearance')

*+  Local Variables
      real       leaf_no_remaining     ! number of leaves to go before all
                                       ! are fully expanded
      real       leaf_no_now           ! number of fully expanded leaves
      real       leaf_app_rate         ! rate of leaf appearance (oCd/leaf)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      leaf_no_now = sum_between (emerg, now, g_leaf_no)
      leaf_no_remaining = g_leaf_no_final - leaf_no_now
 
      if (leaf_no_now .le. c_leaf_no_rate_change) then
 
         leaf_app_rate = c_leaf_app_rate1
 
      else
 
         leaf_app_rate = c_leaf_app_rate2
 
      endif
 
      if (on_day_of (emerg, g_current_stage, g_days_tot)) then
 
             ! no leaf growth on first day because initialised elsewhere ???
 
         dlt_leaf_no_pot = 0.0
 
      elseif (leaf_no_remaining.gt.0.0) then
 
             ! we  haven't reached full number of leaves yet
 
             ! if leaves are still growing, the cumulative number of
             ! phyllochrons or fully expanded leaves is calculated from
             ! daily thermal time for the day.
 
         dlt_leaf_no_pot = divide (g_dlt_tt, leaf_app_rate, 0.0)
         dlt_leaf_no_pot = bound (dlt_leaf_no_pot, 0.0
     :                          , leaf_no_remaining)
 
      else
             ! we have full number of leaves.
 
         dlt_leaf_no_pot = 0.0
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_leaf_area_devel (dlt_lai_pot)
*     ===========================================================
      implicit none
      include   'convert.inc'
      include   'millet.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_lai_pot           ! (OUTPUT) change in leaf area

*+  Purpose
*       Return the potential increase in leaf area development (mm^2)
*       calculated on an individual leaf basis.

*+  Changes
*     010994 jngh specified and programmed

*+  Calls
      real       millet_leaf_size       ! function
      real       millet_swdef           ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_leaf_area_devel')

*+  Local Variables
      real       area                  ! potential maximum area of oldest
                                       ! expanding leaf (mm^2) in today's
                                       ! conditions
!      real       leaf_no_effective     ! effective leaf no - includes
                                       ! younger leaves that have emerged
                                       ! after the current one

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
           ! once leaf no is calculated leaf area of largest expanding leaf
           ! is determined
 
      g_leaf_no_effective = sum_between (emerg, now, g_leaf_no)
     :                  + c_leaf_no_correction
      area = millet_leaf_size (g_leaf_no_effective)
 
      dlt_lai_pot = g_dlt_leaf_no_pot * area * smm2sm * g_plants
     :            * millet_swdef (expansion)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_leaf_area_devel_leg (dlt_lai_pot)
*     ===========================================================
      implicit none
      include   'convert.inc'
      include   'millet.inc'
      include 'science.pub'                       
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_lai_pot           ! (OUTPUT) change in leaf area

*+  Purpose
*       Return the potential increase in leaf area development (mm^2)
*       calculated on an individual leaf basis.

*+  Changes
*     010994 jngh specified and programmed

*+  Calls
!      real       millet_leaf_size       ! function
      real       millet_swdef           ! function
!      real       sum_between           ! function
!      real       leaf_no_effective     ! effective leaf no - includes
                                       ! younger leaves that have emerged
                                       ! after the current one

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_leaf_area_devel_leg')

*+  Local Variables
!      real       area                  ! potential maximum area of oldest
                                       ! expanding leaf (mm^2) in today's
                                       ! conditions

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (stage_is_between (emerg, endjuv, g_current_stage)) then
 
        dlt_lai_pot = g_dlt_leaf_no * c_leaf_size_endjuv * smm2sm
     :            * g_plants * millet_swdef (expansion)
 
      else
 
        dlt_lai_pot = g_dlt_leaf_no * c_leaf_size_average * smm2sm
     :            * g_plants * millet_swdef (expansion)
 
      endif
 
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      real function millet_leaf_size (leaf_no)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       leaf_no               ! (INPUT) nominated leaf number

*+  Purpose
*       Return the leaf area (mm^2) of a specified leaf no.

*+  Changes
*       290894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_leaf_size')

*+  Local Variables
      real       area                  ! potential area of nominated leaf
                                       ! no (mm^2)
      real       area_max              ! potential area of largest leaf (mm^2)
      real       breadth               ! breadth coef of leaf
      real       largest_leaf          ! leaf no of largeat leaf
      real       skewness              ! skewness coef of leaf

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
           ! Once leaf no is calculated leaf area of largest expanding leaf
           ! is determined with quadratic relationship. Coefficients for this
           ! curve are functions of total leaf no.
 
      largest_leaf = c_x0_const + (c_x0_slope * g_leaf_no_final)
      area_max     = p_y0_const + (p_y0_slope * g_leaf_no_final)
 
!cejvo made breadth and skewness linear functions of leafnumber
 
!      breadth  = c_a_const + (c_a_slope1 * g_leaf_no_final)
!      skewness = c_b_const + (c_b_slope1 * g_leaf_no_final)
 
      breadth  = c_a_const
     :         + divide (c_a_slope1
     :                , 1.0 + c_a_slope2 * g_leaf_no_final
     :                , 0.0)
 
      skewness = c_b_const
     :         + divide (c_b_slope1
     :                , 1.0 + c_b_slope2 * g_leaf_no_final
     :                , 0.0)
 
      area = area_max * exp (breadth * (leaf_no - largest_leaf)**2
     :                      + skewness * (leaf_no - largest_leaf)**3)
      millet_leaf_size = area
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_leaf_area_devel_plant (dlt_lai_pot)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include   'convert.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_lai_pot           ! (OUTPUT) change in leaf area

*+  Purpose
*       Return the potential increase in leaf area development (mm^2)
*       calculated on a whole plant basis.

*+  Changes
*     010994 jngh specified and programmed

*+  Calls
      real       millet_swdef           ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_leaf_area_devel_plant')

*+  Local Variables
      real       tpla_max              ! maximum total plant leaf area (mm^2)
      real       tpla_today            ! total plant leaf area today (mm^2)
      real       tlai_today            ! total lai today
      real       tt_since_emerg        ! deg days since emergence (oC)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
           ! once leaf no is calculated maximum plant leaf area
           ! is determined
      if (stage_is_between (emerg, flowering, g_current_stage)) then
 
         tpla_max = (((p_tiller_no_fertile + 1.0) ** c_tiller_coef)
     :            * g_leaf_no_final ** c_main_stem_coef) * scm2smm
 
         tt_since_emerg = sum_between (emerg, now, g_tt_tot)
         tpla_today = divide (tpla_max
     :              , (1.0 + exp(-p_tpla_prod_coef
     :                        * (tt_since_Emerg - p_tpla_inflection)))
     :              , 0.0)
 
         tlai_today = Tpla_today * smm2sm * g_plants
 
            ! limit delta leaf area by water stress
         dlt_lai_pot = (tlai_today - (g_lai + g_slai))
     :               * millet_swdef (expansion)
 
      else
         dlt_lai_pot = 0.0
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_leaf_area ()
*     ===========================================================
      implicit none
      include   'convert.inc'
      include   'millet.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Purpose
*       Simulate actual crop leaf area development - checks that leaf area
*       development matches DM production.

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_leaf_area')

*+  Local Variables
cejvo
      real       extinct_coef          ! extinction coeficient used to back
                                       !  calculate LAI from intercropped canopies
      real       lai_sum               ! total LAI of all crops (in units of this
                                       !  crop's LAI
      real       lai_ratio             ! ratio of actual to potential lai ()
      real       leaf_no_frac          ! ratio of actual to potential leaf
                                       ! appearance ()
      real       sla                   ! maximum SLA allowed as a function of LAI

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
         ! limit the delta leaf area by carbon supply
cejvo
      if (g_fr_intc_radn .gt. 0.0) then
 
         extinct_coef = linear_interp_real (g_row_spacing
     :                        , c_x_row_spacing, c_y_extinct_coef
     :                        , c_num_row_spacing)
 
!         lai_sum = g_lai +
!     :             divide (-alog(1.0 - g_fr_intc_radn),extinct_coef,0.0)
 
         lai_sum =  divide (-alog(1.0 - g_cover_green_sum)
     :                           ,extinct_coef,0.0)
 
!      write (*,*) g_lai,lai_sum,g_cover_green_sum,extinct_coef
!     :                        , g_fr_intc_radn
 
 
      else
 
         lai_sum = g_lai
 
      endif
 
      sla = linear_interp_real (lai_sum
     :                        , c_x_lai, c_y_sla_max
     :                        , c_num_lai)
 
      g_dlt_lai = u_bound (g_dlt_lai_pot
     :                   , g_dlt_dm_green(leaf) * sla * smm2sm)
 
!      g_dlt_lai = u_bound (g_dlt_lai_pot
!     :                   , g_dlt_dm_green(leaf)*c_sla_max * smm2sm)
 
      lai_ratio = divide (g_dlt_lai, g_dlt_lai_pot, 0.0)
 
      leaf_no_frac= linear_interp_real (lai_ratio
     :                        , c_x_lai_ratio, c_y_leaf_no_frac
     :                        , c_num_lai_ratio)
!cgd
!      write (*,*)lai_ratio, c_x_lai_ratio, c_y_leaf_no_frac,
!     :c_num_lai_ratio,leaf_no_frac
      g_dlt_leaf_no = g_dlt_leaf_no_pot * leaf_no_frac
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_leaf_no_init (leaf_no)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'science.pub'                       
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       leaf_no(*)               ! (OUTPUT) initial leaf number

*+  Purpose
*       Return the the initial number of leaves.

*+  Changes
*       250996 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_leaf_no_init')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
 
      if (on_day_of (emerg, g_current_stage, g_days_tot)) then
 
             ! initialise first leaves
 
         leaf_no(emerg) = c_leaf_no_at_emerg
      else
         ! no inital leaf no
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_nitrogen ()
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'error.pub'                         

*+  Purpose
*       Simulate crop nitrogen processes.

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_nitrogen')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      call millet_N_init (g_N_green)
      call millet_N_retranslocate (g_dlt_N_retrans)
      call millet_N_demand (g_N_demand, g_N_max)
      call millet_N_uptake (g_dlt_NO3gsm, g_dlt_N_green)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_N_retranslocate (dlt_N_retrans)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_N_retrans (*)     ! (OUTPUT) plant N taken out from
                                       ! plant parts (g N/m^2)

*+  Purpose
*     Calculate the nitrogen retranslocation from the various plant parts
*     to the grain.

*+  Changes
*       080994 jngh specified and programmed

*+  Calls
      real       millet_N_dlt_grain_conc ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_N_retranslocate')

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
 
      grain_N_demand = g_dlt_dm_green(grain)
     :                 * millet_N_dlt_grain_conc ()
 
      N_potential  = (g_dm_green(grain) + g_dlt_dm_green(grain))
     :             * g_N_conc_max(grain)
 
      grain_N_demand = u_bound (grain_N_demand
     :                        , N_potential - g_N_green(grain))
 
      call millet_N_retrans_avail (N_avail)  ! grain N potential (supply)
 
            ! available N does not include roots or grain
cjh  this should not presume roots and grain are 0.
      N_avail_stover  =  sum_real_array (N_avail, max_part)
 
          ! get actual grain N uptake
 
          ! limit retranslocation to total available N
 
      call fill_real_array (dlt_N_retrans, 0.0, max_part)
 
      if (grain_N_demand.ge.N_avail_stover) then
 
             ! demand greater than or equal to supply
             ! retranslocate all available N
 
         dlt_N_retrans(leaf) = - N_avail(leaf)
         dlt_N_retrans(stem) = - N_avail(stem)
         dlt_N_retrans(flower) = - N_avail(flower)
         dlt_N_retrans(grain) = N_avail_stover
 
      else
 
             ! supply greater than demand.
             ! Retranslocate what is needed
 
         dlt_N_retrans(leaf) = - grain_N_demand
     :                         * divide (N_avail(leaf)
     :                                 , N_avail_stover, 0.0)
 
         dlt_N_retrans(flower) = - grain_N_demand
     :                         * divide (N_avail(flower)
     :                                 , N_avail_stover, 0.0)
 
cgol correct N retranslocation rounding error
          dlt_N_retrans(stem) = - grain_N_demand
     :                         * divide (N_avail(stem)
     :                                 , N_avail_stover, 0.0)
 
!          dlt_N_retrans(stem) = - grain_N_demand
!     :                         - dlt_N_retrans(leaf)   ! note - these are
!     :                         - dlt_N_retrans(flower) ! -ve values.
 
         dlt_N_retrans(grain) = grain_N_demand
 
      endif
 
             ! just check that we got the maths right.
 
      do 1000 part = root, flower
         call bound_check_real_var (abs (dlt_N_retrans(part))
     :                            , 0.0, N_avail(part)
     :                            , 'dlt_N_retrans(part)')
1000  continue
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_N_retrans_avail (N_avail)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       N_avail (*)           ! (OUTPUT) total N available for
                                       ! transfer to grain (g/m^2)

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

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_N_retrans_avail')

*+  Local Variables
      real       N_min                 ! nitrogen minimum level (g/m^2)
      integer    part                  ! plant part number

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
         ! get grain N potential (supply) -----------
         ! now find the available N of each part.
 
      do 1000 part = 1, max_part
         N_min = g_N_conc_min(part) * g_dm_green(part)
         N_avail(part) = l_bound (g_N_green(part) - N_min, 0.0)
1000  continue
 
      N_avail(grain) = 0.0
      N_avail(root) = 0.0
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      real function millet_N_dlt_grain_conc ()
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'error.pub'                         

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

*+  Changes
*       090994 jngh specified and programmed

*+  Calls
      real       millet_nfact           ! function
      real       millet_swdef           ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_N_dlt_grain_conc')

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
      N_grain_sw_fac = c_sw_fac_max
     :               - c_sfac_slope * millet_swdef(expansion)
 
            ! N stress reduces grain N concentration below critical
 
      N_conc_pot = g_N_conc_min(grain)
     :           + (g_N_conc_crit(grain) - g_N_conc_min(grain))
     :           * millet_nfact(grain_conc)
 
            ! Temperature and water stresses can decrease/increase grain
            ! N concentration
 
cjh  when there is no N stress, the following can be a higher N conc than
cjh   the crit and thus the N conc of the grain can exceed N critical.
 
      millet_N_dlt_grain_conc = N_conc_pot
     :                       * max (N_grain_temp_fac, N_grain_sw_fac)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_N_demand (N_demand, N_max)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       N_demand (*)          ! (OUTPUT) critical plant nitrogen demand
                                       ! (g/m^2)
      real       N_max (*)             ! (OUTPUT) max plant nitrogen demand
                                       ! (g/m^2)

*+  Purpose
*       Return plant nitrogen demand for each plant component

*+  Notes
*           Nitrogen required for grain growth has already been removed
*           from the stover.  Thus the total N demand is the sum of the
*           demands of the stover and roots.  Stover N demand consists of
*           two components:
*           Firstly, the demand for nitrogen by the potential new growth.
*           Secondly, the demand due to the difference between
*           the actual N concentration and the critical N concentration
*           of the tops (stover), which can be positive or negative

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_N_demand')

*+  Local Variables
      integer    current_phase         ! current phase number
      real       N_crit                ! critical N amount (g/m^2)
      real       N_demand_new          ! demand for N by new growth
                                       ! (g/m^2)
      real       N_demand_old          ! demand for N by old biomass
                                       ! (g/m^2)
      real       N_potential       ! maximum N uptake potential (g/m^2)
      real       N_max_new             ! N required by new growth to reach
                                       ! N_conc_max  (g/m^2)
      real       N_max_old             ! N required by old biomass to reach
                                       ! N_conc_max  (g/m^2)
      integer    part                  ! plant part
      real       dlt_dm_pot_radn       ! potential dry matter production with
                                       ! optimum water and nitrogen and
                                       ! temperature stress conditions (g/m^2)
      real       dlt_dm_pot (max_part) ! potential dry weight increase
                                       ! (g/m^2)
      real       part_fract            ! plant part fraction of dm  (0-1)
      real       radn_int              ! radn intercepted by leaves (mj/m^2)

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
         ! calculate potential new shoot and root growth
 
      current_phase = int (g_current_stage)
 
            ! need to calculate dm using potential rue not affected by
            ! N and temperature
 
      call millet_radn_int (radn_int)
      dlt_dm_pot_radn = c_rue(current_phase)*radn_int
 
      do 500 part = 1, max_part
 
         part_fract = divide (g_dlt_dm_green(part), g_dlt_dm, 0.0)
         dlt_dm_pot(part) = dlt_dm_pot_radn * part_fract
         dlt_dm_pot(part) = bound (dlt_dm_pot(part)
     :                           , 0.0, dlt_dm_pot_radn)
cjhtest         dlt_dm_pot(part) = g_dlt_dm_green(part)
500   continue
 
            ! recalculate roots because today's drymatter production
            ! does not include roots
 
      dlt_dm_pot(root) = dlt_dm_pot_radn
     :                 * c_ratio_root_shoot(current_phase)
 
 
         ! g_dlt_dm_pot is above ground biomass only so leave roots
         ! out of comparison
 
      call bound_check_real_var (
     :             sum_real_array (dlt_dm_pot, max_part)
     :           - dlt_dm_pot(root)
     :           , 0.0, dlt_dm_pot_radn
     :           , 'dlt_dm_pot - dlt_dm_pot(root)')
 
 
      do 1000 part = 1, max_part
         if (g_dm_green(part).gt.0.0) then
 
               ! get N demands due to difference between actual N concentrations
               ! and critical N concentrations of tops (stover) and roots.
 
            N_crit       = g_dm_green(part) * g_N_conc_crit(part)
            N_potential  = g_dm_green(part) * g_N_conc_max(part)
 
               ! retranslocation is -ve for outflows
 
            N_demand_old = N_crit
     :                   - (g_N_green(part) + g_dlt_N_retrans(part))
            N_max_old    = N_potential
     :                   - (g_N_green(part) + g_dlt_N_retrans(part))
 
 
               ! get potential N demand (critical N) of potential growth
 
            N_demand_new = dlt_dm_pot(part) * g_N_conc_crit(part)
            N_max_new    = dlt_dm_pot(part) * g_N_conc_max(part)
 
            N_demand(part) = N_demand_old + N_demand_new
            N_max(part)    = N_max_old    + N_max_new
 
            N_demand(part) = l_bound (N_demand(part), 0.0)
            N_max(part)    = l_bound (N_max(part), 0.0)
 
         else
            N_demand(part) = 0.0
            N_max(part)    = 0.0
 
         endif
 
1000  continue
 
            ! grain doesn't take N from the soil (retranslocation only)
 
         N_demand(grain) = 0.0
         N_max(grain) = 0.0
 
         ! this routine does not allow excess N in one component to move
         ! to another component deficient in N
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_N_uptake (dlt_NO3gsm, dlt_N_green)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_N_green(max_part) ! (OUTPUT) actual plant N uptake
                                       ! into each plant part (g/m^2)
*
      real       dlt_NO3gsm(max_layer) ! (OUTPUT) actual plant N uptake
                                       ! from NO3 in each layer (g/m^2)

*+  Purpose
*       Return actual plant nitrogen uptake to each plant part and from
*       each soil layer.

*+  Changes
*       080994 jngh specified and programmed
*       150995 psc  milletpea + fixation

*+  Calls
cjh      include   'convert.inc'          ! gm2kg, sm2ha
*
      real       millet_N_fixation      ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_N_uptake')

*+  Local Variables
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      real       NO3gsm_diffn          ! actual N available (supply) for
                                       ! plant (g/m^2) by diffusion
      real       NO3gsm_mflow          ! actual N available (supply) for
                                       ! plant (g/m^2) by mass flow
      real       NO3gsm_diffn_supply   ! total potential N uptake (supply)
                                       ! for plant (g/m^2) by diffusion
      real       NO3gsm_diffn_avail(max_layer) ! potential NO3 (supply) from
                                       ! soil (g/m^2), by diffusion
      real       NO3gsm_mflow_supply   ! total potential N uptake (supply)
                                       ! for plant (g/m^2) by mass flow
      real       NO3gsm_mflow_avail(max_layer) ! potential NO3 (supply) from
                                       ! soil (g/m^2) by mass flow
      real       diffn_fract           ! fraction of nitrogen to use (0-1)
                                       ! for diffusion
      real       mflow_fract           ! fraction of nitrogen to use (0-1)
                                       ! for mass flow
      real       plant_part_fract      ! fraction of nitrogen to use (0-1)
                                       ! for plant part
      integer    layer                 ! soil layer number of profile
      real       N_demand              ! total nitrogen demand (g/m^2)
      integer    part                  ! plant part number
      real       N_uptake_sum          ! total plant N uptake (g/m^2)
      real       NO3gsm_uptake         ! plant NO3 uptake from layer (g/m^2)
      real       N_max                 ! potential N uptake per plant (g/m^2)
      real       N_excess              ! N uptake above N crit (g/m^2)
      real       N_capacity(max_part)  ! amount of N that can be stored in
                                       ! plant part above Ncrit (g/m^2)
      real       N_capacity_sum        ! total excess N storage (g/m^2)
      real       N_fix                 ! N fixation potential (g/m^2)
      real       N_fix_demand_tot      ! total demand for N fixation (g/m^2)
      real       N_fix_uptake          ! actual N fixation (g/m^2)
      real       fix_demand            ! demand for fixed N per plant part (g/m^2)
      real       fix_part_fract        ! fraction of fixed N per plant part (g/m^2)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
            ! find potential N uptake (supply, available N)
            ! Get it for nitrate by diffusion and mass flow
            ! Note: the N available by diffusion is really the total N
            ! available to the roots by mass flow and diffusion.
 
      call millet_N_mass_flow (NO3gsm_mflow_avail)
      call millet_N_diffusion (NO3gsm_diffn_avail)
 
      deepest_layer = find_layer_no (g_root_depth, g_dlayer, max_layer)
      do 1000 layer = 1, deepest_layer
         NO3gsm_diffn_avail(layer) = NO3gsm_diffn_avail(layer)
     :                             - NO3gsm_mflow_avail(layer)
         NO3gsm_diffn_avail(layer) = l_bound (NO3gsm_diffn_avail(layer)
     :                                       , 0.0)
1000  continue
 
            ! get potential N uptake (supply) from the root profile.
            ! get totals for diffusion and mass flow.
 
      NO3gsm_mflow_supply = sum_real_array (NO3gsm_mflow_avail
     :                                     , deepest_layer)
      NO3gsm_diffn_supply = sum_real_array (NO3gsm_diffn_avail
     :                                     , deepest_layer)
 
            ! get actual total nitrogen uptake for diffusion and mass flow.
            ! If demand is not satisfied by mass flow, then use diffusion.
            ! N uptake above N critical can only happen via mass flow.
 
      N_demand = sum_real_array (g_N_demand, max_part)
      N_max    = sum_real_array (g_N_max, max_part)
 
      if (NO3gsm_mflow_supply.ge.N_demand) then
         NO3gsm_mflow = NO3gsm_mflow_supply
         NO3gsm_mflow = u_bound (NO3gsm_mflow, N_max)
         NO3gsm_diffn = 0.0
 
      else
         NO3gsm_mflow = NO3gsm_mflow_supply
         NO3gsm_diffn = bound (N_demand - NO3gsm_mflow, 0.0
     :                        , NO3gsm_diffn_supply)
         NO3gsm_diffn = divide (NO3gsm_diffn, c_NO3_diffn_const, 0.0)
 
      endif
 
            ! get actual change in N contents
 
      call fill_real_array (dlt_NO3gsm, 0.0, max_layer)
 
      do 1100 layer = 1,deepest_layer
 
               ! allocate nitrate
               ! Find proportion of nitrate uptake to be taken from layer
               ! by diffusion and mass flow
 
         mflow_fract = divide (NO3gsm_mflow_avail(layer)
     :                       , NO3gsm_mflow_supply, 0.0)
 
         diffn_fract = divide (NO3gsm_diffn_avail(layer)
     :                       , NO3gsm_diffn_supply, 0.0)
 
               ! now find how much nitrate the plant removes from
               ! the layer by both processes
 
         NO3gsm_uptake = NO3gsm_mflow * mflow_fract
     :                 + NO3gsm_diffn * diffn_fract
         dlt_NO3gsm(layer) = - NO3gsm_uptake
 
 
1100  continue
 
               ! find proportion of uptake to be
               ! distributed to to each plant part and distribute it.
 
      N_uptake_sum = - sum_real_array (dlt_NO3gsm, deepest_layer)
 
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
 
 
               ! determine N from fixation
 
      N_fix = millet_N_fixation ()
 
      N_fix_demand_tot = l_bound (N_demand - N_uptake_sum, 0.0)
 
      N_fix_uptake = bound (N_fix, 0.0, N_fix_demand_tot)
 
      do  1400 part = 1, max_part
         fix_demand = l_bound (g_N_demand(part) - dlt_N_green(part)
     :                        , 0.0)
         fix_part_fract = divide (fix_demand, N_fix_demand_tot, 0.0)
         dlt_N_green(part) = dlt_N_green(part)
     :                     + fix_part_fract * N_fix_uptake
1400  Continue
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_N_mass_flow (NO3gsm_mflow_pot)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       NO3gsm_mflow_pot (max_layer) ! (OUTPUT) potential plant NO3
                                              ! uptake (supply) g/m^2,
                                              ! by mass flow

*+  Purpose
*       Return potential nitrogen uptake (supply) by mass flow (water
*       uptake) (g/m^2)

*+  Changes
*       090994 jngh specified and programmed

*+  Calls
cjh      include   'convert.inc'          ! ha2sm, kg2gm

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_N_mass_flow')

*+  Local Variables
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      integer    layer                 ! layer number of soil
      real       NO3_conc              ! nitrogen concentration (g/m^2/mm)
      real       NO3gsm_mflow          ! potential nitrogen uptake (g/m^2)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      call fill_real_array (NO3gsm_mflow_pot, 0.0, max_layer)
 
         ! only take the layers in which roots occur
 
      deepest_layer = find_layer_no (g_root_depth, g_dlayer, max_layer)
      do 1000 layer = 1, deepest_layer
 
            ! get  NO3 concentration
 
         NO3_conc = divide (g_NO3gsm(layer)
     :                    , g_sw_dep(layer), 0.0)
 
            ! get potential uptake by mass flow
 
         NO3gsm_mflow = NO3_conc * (-g_dlt_sw_dep(layer))
         NO3gsm_mflow_pot(layer) = u_bound (NO3gsm_mflow,
     :                            g_NO3gsm(layer) - g_NO3gsm_min(layer))
 
1000  continue
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_N_diffusion (NO3gsm_diffn_pot)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       NO3gsm_diffn_pot (max_layer) ! (OUTPUT) potential plant NO3
                                              ! uptake (supply) g/m^2,
                                              !  by diffusion

*+  Purpose
*       Return potential nitrogen uptake (supply) by diffusion
*       for a plant (g/m^2)

*+  Changes
*       090994 jngh specified and programmed

*+  Calls
cjh      include   'convert.inc'          ! ha2sm, kg2gm

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_N_diffusion')

*+  Local Variables
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      integer    layer                 ! layer number of soil
      real       NO3gsm_diffn          ! potential nitrogen uptake (g/m^2)
      real       sw_avail_fract        ! fraction of extractable soil water ()

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
         ! only take the layers in which roots occur
 
      call fill_real_array (NO3gsm_diffn_pot, 0.0, max_layer)
 
      deepest_layer = find_layer_no (g_root_depth, g_dlayer, max_layer)
      do 1000 layer = 1, deepest_layer
 
         sw_avail_fract = divide (g_sw_avail(layer)
     :                  , g_sw_dep(layer), 0.0)
         sw_avail_fract = bound (sw_avail_fract, 0.0, 1.0)
 
            ! get extractable NO3
            ! restricts NO3 available for diffusion to NO3 in plant
            ! available water range
 
         NO3gsm_diffn = sw_avail_fract * g_NO3gsm(layer)
         NO3gsm_diffn_pot(layer) = u_bound (NO3gsm_diffn,
     :                           g_NO3gsm(layer) - g_NO3gsm_min(layer))
 
1000  continue
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      real function millet_N_fixation ()
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Purpose
*           calculate the quantity of atmospheric nitrogen fixed
*          per unit carbohydrate per day (mgN_fixed/g plant)

*+  Changes
*       240595  psc   specified

*+  Calls
      real       millet_swdef                ! function

*+  Constant Values
      character  my_name*(*)                 ! name of subroutine
      parameter (my_name = 'millet_N_fixation')

*+  Local Variables
      real    biomass                       ! cummulative biomass (g/plant)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      biomass = (sum_real_array (g_dm_green, max_part)
     :           - g_dm_green(root))
     :           + g_dlt_dm
 
      millet_N_fixation = c_N_fix_rate * biomass
     :                  * millet_swdef(fixation)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_N_conc_limits (N_conc_crit
     :                              , N_conc_max
     :                              , N_conc_min)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
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
*       080994 jngh specified and programmed

*+  Calls
      real       millet_stage_code      ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_N_conc_limits')

*+  Local Variables
      integer    numvals               ! number of values in stage code table
      real       stage_code            ! interpolated current stage code

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
         stage_code = millet_stage_code (g_current_stage
     :                                , c_x_stage_code, numvals)
         N_conc_crit(stem) = linear_interp_real (stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_crit_stem
     :                             , numvals)
         N_conc_crit(leaf) = linear_interp_real (stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_crit_leaf
     :                             , numvals)
         N_conc_crit(flower) = linear_interp_real (stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_crit_flower
     :                             , numvals)
 
             ! the  minimum N concentration is the N concentration
             ! below which N does not fall.
 
         N_conc_min(stem) = linear_interp_real (stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_min_stem
     :                             , numvals)
 
         N_conc_min(leaf) = linear_interp_real (stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_min_leaf
     :                             , numvals)
 
         N_conc_min(flower) = linear_interp_real (stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_min_flower
     :                             , numvals)
 
             ! the  maximum N concentration is the N concentration
             ! above which N does not rise.
 
         N_conc_max(stem) = linear_interp_real (stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_max_stem
     :                             , numvals)
 
         N_conc_max(leaf) = linear_interp_real (stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_max_leaf
     :                             , numvals)
 
         N_conc_max(flower) = linear_interp_real (stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_max_flower
     :                             , numvals)
 
      else
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      real function millet_nfact (type)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'millet.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    type                  ! (INPUT) factors type

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

*+  Changes
*     010994 jngh specified and programmed
*     090695 psc  added N_fact for phenology & externalise multipliers for ndef

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_nfact')

*+  Local Variables
      real       N_conc_stover         ! tops (stover) actual N concentration
                                       ! (0-1)
      real       dm_stover             ! tops (stover) plant weight (g/m^2)
      character  err_messg*100           ! error message
      real       N_def                 ! N factor (0-1)
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
      real       N_conc_ratio          ! available N as fraction of N capacity
                                       ! (0-1)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
         ! calculate actual N concentrations
 
      dm_stover = g_dm_green(leaf) + g_dm_green(stem)
      N_stover = g_N_green(leaf) + g_N_green(stem)
 
      N_conc_stover = divide (N_stover, dm_stover, 0.0)
 
         ! calculate critical N concentrations
 
      N_leaf_crit = g_N_conc_crit(leaf) * g_dm_green(leaf)
      N_stem_crit = g_N_conc_crit(stem) * g_dm_green(stem)
      N_stover_crit = N_leaf_crit + N_stem_crit
 
      N_conc_stover_crit = divide (N_stover_crit, dm_stover, 0.0)
 
         ! calculate minimum N concentrations
 
      N_leaf_min = g_N_conc_min(leaf) * g_dm_green(leaf)
      N_stem_min = g_N_conc_min(stem) * g_dm_green(stem)
      N_stover_min = N_leaf_min + N_stem_min
 
      N_conc_stover_min = divide (N_stover_min, dm_stover, 0.0)
 
         ! calculate shortfall in N concentrations
 
      N_conc_ratio = divide ((N_conc_stover - N_conc_stover_min)
     :              , (N_conc_stover_crit - N_conc_stover_min), 0.0)
 
         ! calculate 0-1 N deficiency factors
 
      if (type.eq.photo) then
         N_def = c_N_fact_photo * N_conc_ratio
         millet_nfact = bound (N_def, 0.0, 1.0)
 
      elseif (type.eq.grain_conc) then
         N_def = N_conc_ratio
         millet_nfact = bound (N_def, 0.0, 1.0)
 
      elseif (type.eq.pheno) then
         N_def = c_N_fact_pheno * N_conc_ratio
         millet_nfact = bound (N_def, 0.0, 1.0)
 
      else
         ! we have an unknown type
         write (err_messg, '(a, i5)')
     :                   ' Unknown type of N factor. Type ='
     :                   , type
         call fatal_error (err_internal, err_messg)
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_N_init (N_green)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'science.pub'                       
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       N_green(*)            ! plant nitrogen (g/m^2)

*+  Purpose
*       Set plant nitrogen

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_N_init')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (on_day_of (emerg, g_current_stage, g_days_tot)) then
         N_green(root) = c_N_root_init_conc*g_dm_green(root)
         N_green(stem) = c_N_stem_init_conc*g_dm_green(stem)
         N_green(leaf) = c_N_leaf_init_conc*g_dm_green(leaf)
         N_green(flower) = 0.0
         N_green(grain) = 0.0
 
      else
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_phenology ()
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'science.pub'                       
      include 'error.pub'                         

*+  Purpose
*     Use temperature, photoperiod and genetic characteristics
*     to determine when the crop begins a new growth phase.
*     The initial daily thermal time and height are also set.

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_phenology')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      g_previous_stage = g_current_stage
 
         ! get thermal times
 
         ! TEMPLATE OPTION
         ! Three options for calc thermal time provided. Must have
         ! at least one returning g_dlt_tt as argument.
 
      call millet_tt (g_dlt_tt)
         ! TEMPLATE OPTION and/or
      ! call millet_tt_curv (g_dlt_tt_curv)
         ! TEMPLATE OPTION and/or
      ! call millet_tt_other (g_dlt_tt_other)
 
         ! initialise phenology phase targets
 
      call millet_phenology_init (g_phase_tt)
      call millet_devel (g_dlt_stage, g_current_stage)
 
         ! update thermal time states and day count
 
      call accumulate (g_dlt_tt, g_tt_tot
     :               , g_previous_stage, g_dlt_stage)
 
         ! TEMPLATE OPTION and/or
      ! call accumulate (g_dlt_tt_curv, g_tt_curv_tot
      !:               , g_previous_stage, g_dlt_stage)
         ! TEMPLATE OPTION and/or
      ! call accumulate (g_dlt_tt_other, g_tt_other_tot
      !:               , g_previous_stage, g_dlt_stage)
 
      call accumulate (1.0, g_days_tot
     :               , g_previous_stage, g_dlt_stage)
 
          ! canopy height
 
      call millet_canopy_height (g_dlt_canopy_height)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_tt (dlt_tt)
*     ===========================================================
      implicit none
      include    'millet.inc'
      include 'science.pub'                       
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_tt                ! (OUTPUT) daily thermal time (oC)

*+  Purpose
*     Growing degree day (thermal time) is calculated.

*+  Notes
*     Eight interpolations of the air temperature are
*     calculated using a three-hour correction factor.
*     For each air three-hour air temperature, a value of growing
*     degree day is calculated.  The eight three-hour estimates
*     are then averaged to obtain the daily value of growing degree
*     days.

*+  Changes
*       140994 jngh specified and programmed
*       090695 psc  added N_fact for phenology stress

*+  Calls
      real       millet_swdef           ! function
      real       millet_nfact           ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_tt')

*+  Local Variables
      real       dly_therm_time        ! thermal time for the day (deg day)

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      dly_therm_time = linint_3hrly_temp (g_maxt, g_mint
     :                 , c_x_temp, c_y_tt
     :                 , c_num_temp)
 
      if (stage_is_between (emerg, flag_leaf, g_current_stage)) then
 
!cpsc
         dlt_tt = dly_therm_time *
     :             min (millet_swdef (pheno), millet_nfact(pheno))
 
      else
 
         dlt_tt = dly_therm_time
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_tt_curv (dlt_tt_curv)
*     ===========================================================
      implicit none
      include    'millet.inc'
      include 'data.pub'                          
      include 'science.pub'                       
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_tt_curv           ! (OUTPUT) daily thermal time (oC)

*+  Purpose
*     Growing degree day accumulation is calculated using a curvilinear
*     function.

*+  Notes
*      Uses Landsberg curvilinear function on average daily temp.

*+  Changes
*       150994 glh specified and programmed

*+  Calls
      real       millet_swdef           ! function

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'millet_tt_curv')

*+  Local Variables
      real       ave_temp              ! average daily temp (oC)

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      ave_temp = (g_maxt + g_mint) * 0.5
 
      if (stage_is_between (emerg, floral_init
     :                     , g_current_stage)) then
         dlt_tt_curv = Curvilinear (c_imin, c_iopt, c_imax
     :                            , c_ioptr, Ave_temp)
     :               * millet_swdef (pheno)
 
      elseif (stage_is_between (floral_init, flowering
     :                         , g_current_stage)) then
         dlt_tt_curv = Curvilinear (c_amin, c_aopt, c_amax
     :                            , c_aoptr, Ave_temp)
     :               * millet_swdef (pheno)
 
      elseif (stage_is_between (flowering, maturity
     :                         , g_current_stage)) then
         dlt_tt_curv = l_bound((u_bound(ave_temp, 23.5) - 5.7), 0.0)
 
      else
         dlt_tt_curv = 0.0
 
      endif
 
      call pop_routine (myname)
      return
      end



*     ===========================================================
      subroutine millet_tt_other (dlt_tt_other)
*     ===========================================================
      implicit none
      include    'millet.inc'
      include 'science.pub'                       
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_tt_other          ! (OUTPUT) daily thermal time (oC)

*+  Purpose
*     Growing degree day accumulation is calculated.

*+  Changes
*       140994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_tt_other')

*+  Local Variables
      real       ave_temp              ! mean temperature (oC)
      real       dly_therm_time        ! thermal time for the day (deg day)

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      ave_temp = (g_maxt + g_mint) * 0.5
      dly_therm_time = linear_interp_real (ave_temp
     :                        , c_x_temp_other, c_y_tt_other
     :                        , c_num_temp_other)
!cgd
!      write (*,*) 'gd',ave_temp,c_x_temp_other,c_y_tt_other,
!     :c_num_temp_other,dly_therm_time
      dlt_tt_other = dly_therm_time
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_phenology_init (phase_tt)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       phase_tt (*)          ! (INPUT/OUTPUT) cumulative growing
                                       ! degree days required for
                                       ! each stage (deg days)

*+  Purpose
*       Returns cumulative thermal time targets required for the
*       individual growth stages.

*+  Changes
*     010994 jngh specified and programmed
*     070495 psc added 2nd leaf appearance rate
*     090695 psc l_bound added (otherwise won't progress if phase_tt=0)

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_phenology_init')

*+  Local Variables
      integer    est_day_of_floral_init ! estimated day of year of floral
                                        ! initiation
      real       tt_emerg_to_flag_leaf ! growing degree days to develop
                                       ! and fully expand all leaves
                                       ! (deg day).  This is the gdd
                                       ! required from emergence to end of
                                       ! leaf growth
      real       photoperiod           ! hours of photosynthetic light (hours)
      real       photoperiod_active    ! hours of light in excess of threshold
                                       ! (hours)
      real       leaf_no               ! leaf no. above which app. rate changes

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (on_day_of (germ, g_current_stage, g_days_tot)) then
         phase_tt(germ_to_emerg) = c_shoot_lag
     :                           + g_sowing_depth*c_shoot_rate
         phase_tt(emerg_to_endjuv) = p_tt_emerg_to_endjuv
 
      elseif (on_day_of (emerg, g_current_stage, g_days_tot)) then
 
         est_day_of_floral_init = offset_day_of_year (g_year
     :                                  , g_day_of_year
     :                                  , p_est_days_emerg_to_init)
     :
         photoperiod = day_length (est_day_of_floral_init
     :                           , g_latitude, c_twilight)
 
         photoperiod_active = photoperiod - c_photoperiod_base
         photoperiod_active = bound (photoperiod_active, 0.0, 24.0)
!cpsc                          need below to exert stage when phase_tt = 0
cjh         phase_tt(endjuv_to_init) = l_bound (p_pp_endjuv_to_init
cjh     :                            * photoperiod_active, 0.1)
         phase_tt(endjuv_to_init) = p_pp_endjuv_to_init
     :                            * photoperiod_active
 
      elseif (stage_is_between (endjuv, floral_init
     :                        , g_current_stage)) then
         photoperiod = day_length (g_day_of_year, g_latitude
     :                           , c_twilight)
         photoperiod_active = photoperiod - c_photoperiod_base
         photoperiod_active = bound (photoperiod_active, 0.0, 24.0)
!cpsc                          need below to exert stage when phase_tt = 0
cjh         phase_tt(endjuv_to_init) = l_bound (p_pp_endjuv_to_init
cjh     :                            * photoperiod_active, 0.1)
         phase_tt(endjuv_to_init) = p_pp_endjuv_to_init
     :                            * photoperiod_active
 
      elseif (on_day_of (floral_init, g_current_stage
     :                 , g_days_tot)) then
cpsc
         leaf_no = max (c_leaf_no_rate_change, c_leaf_no_at_emerg)
cjh
         leaf_no = min (leaf_no, g_leaf_no_final)
         tt_emerg_to_flag_leaf = (leaf_no - c_leaf_no_at_emerg)
     :                         * c_leaf_app_rate1
     :                         + (g_leaf_no_final - leaf_no)
     :                         * c_leaf_app_rate2
         phase_tt(init_to_flag) = tt_emerg_to_flag_leaf
     :                          - sum_between (emerg, floral_init
     :                                       , g_tt_tot)
 
         phase_tt(flag_to_flower) = p_tt_flag_to_flower
 
         phase_tt(flower_to_start_grain) = p_tt_flower_to_start_grain
 
         phase_tt(end_grain_to_maturity) = 0.05*p_tt_flower_to_maturity
 
         phase_tt(start_to_end_grain) = p_tt_flower_to_maturity
     :                                - phase_tt(flower_to_start_grain)
     :                                - phase_tt(end_grain_to_maturity)
         phase_tt(maturity_to_ripe) = p_tt_maturity_to_ripe
 
      else
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_devel (dlt_stage, current_stage)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_stage             ! (OUTPUT) change in growth stage
      real       current_stage         ! (OUTPUT) new stage no.

*+  Purpose
*     Determine the curent stage of development.

*+  Changes
*     010994 jngh specified and programmed
*     070495 psc add l_bound to dlt-stage

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_devel')

*+  Local Variables
      real       new_stage             ! new stage number
      real       phase_devel           ! fraction of current phase elapsed ()

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
         ! mechanical operation - not to be changed
 
         ! now calculate the new delta and the new stage
 
      call millet_phase_devel (phase_devel)
      new_stage = aint (g_current_stage) + phase_devel
cjh      dlt_stage = l_bound(new_stage - g_current_stage,0.0)
      dlt_stage = new_stage - g_current_stage
 
      if (phase_devel.ge.1.0) then
         current_stage = aint (current_stage + 1.0)
         if (int(current_stage).eq.max_stage) then
            current_stage = 1.0
         else
         endif
 
      else
         current_stage = new_stage
 
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_phase_devel (phase_devel)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'science.pub'                       
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       phase_devel           ! (OUTPUT) fraction of current phase
                                       ! elapsed ()

*+  Purpose
*     Determine the fraction of current phase elapsed ().

*+  Changes
*     010994 jngh specified and programmed

*+  Calls
      real       millet_germination     ! function
      real       millet_phase_tt        ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_phase_devel')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (stage_is_between (sowing, germ, g_current_stage)) then
         phase_devel = millet_germination (g_current_stage)
 
      elseif (stage_is_between (germ, harvest_ripe
     :                        , g_current_stage)) then
 
         phase_devel =  millet_phase_tt (g_current_stage)
 
      else
cjh         phase_devel = 0.0
         phase_devel = mod(g_current_stage, 1.0)
 
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      real function millet_germination (current_stage)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'data.pub'                          
      include 'science.pub'                       
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       current_stage         ! (OUTPUT) phenological stage number

*+  Purpose
*      Determine germination based on soil water availability

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_germination')

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
 
      if (stage_is_between (sowing, germ, current_stage)) then
 
         layer_no_seed = find_layer_no (g_sowing_depth, g_dlayer
     :                                , max_layer)
         pesw_seed = divide (g_sw_dep(layer_no_seed)
     :                     - p_ll_dep(layer_no_seed)
     :                     , g_dlayer(layer_no_seed), 0.0)
 
            ! can't germinate on same day as sowing, because miss out on
            ! day of sowing else_where
 
         if (pesw_seed.gt.c_pesw_germ
     :   .and.
     :   .not. on_day_of (sowing, g_current_stage, g_days_tot)) then
               ! we have germination
               ! set the current stage so it is on the point of germination
            millet_germination = 1.0 + mod (g_current_stage, 1.0)
 
         else
                ! no germination yet but indicate that we are on the way.
            millet_germination = 0.999
         endif
      else
             ! no sowing yet
         millet_germination = 0.0
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      real function millet_phase_tt (stage_no)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       stage_no              ! (INPUT) stage number

*+  Purpose
*       Return fraction of thermal time we are through the current
*       phenological phase (0-1)

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_phase_tt')

*+  Local Variables
      integer    phase                 ! phase number containing stage

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      phase = int (stage_no)
cjh  changed 0.0 to 1.0
      millet_phase_tt = divide (g_tt_tot(phase) + g_dlt_tt
     :                       , g_phase_tt(phase), 1.0)
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      real function millet_phase_tt_curv (stage_no)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       stage_no              ! (INPUT) stage number

*+  Purpose
*       returns fraction of thermal time we are through the current
*       phenological phase (0-1) based on curvilinear tt.

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_phase_tt_curv')

*+  Local Variables
      integer    phase                 ! phase number containing stage

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      phase = int (stage_no)
      millet_phase_tt_curv = divide (g_tt_curv_tot(phase)
     :                             , g_phase_tt_curv(phase), 0.0)
      millet_phase_tt_curv = bound (millet_phase_tt_curv, 0.0, 1.999)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      real function millet_phase_tt_other (stage_no)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       stage_no              ! (INPUT) stage number

*+  Purpose
*       Return fraction of thermal time we are through the current
*       phenological phase (0-1) based on other function.

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_phase_tt_other')

*+  Local Variables
      integer    phase                 ! phase number containing stage

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      phase = int (stage_no)
      millet_phase_tt_other = divide (g_tt_other_tot(phase)
     :                             , g_phase_tt_other(phase), 0.0)
      millet_phase_tt_other = bound (millet_phase_tt_other, 0.0, 1.99)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_canopy_height (dlt_canopy_height)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_canopy_height     ! (INPUT) canopy height change (mm)

*+  Purpose
*       Get change in plant canopy height

*+  Changes
*       231093 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'millet_canopy_height')

*+  Local Variables
      real       dm_stem_plant         ! dry matter of stem (g/plant)
      real       pot_height            ! potential height (mm)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (stage_is_between (emerg, flag_leaf
     :                    , g_current_stage)) then
 
         dm_stem_plant = divide (g_dm_green(stem), g_plants, 0.0)
         pot_height = c_height_stem_slope * dm_stem_plant
         pot_height = bound (pot_height, 0.0, c_height_max)
         dlt_canopy_height = pot_height - g_canopy_height
 
      else
         dlt_canopy_height = 0.0
 
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_senescence ()
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'error.pub'                         

*+  Purpose
*       Simulate plant senescence.

*+  Changes
*      091294 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_senescence')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      call millet_leaf_death (g_dlt_leaf_no_dead)
      ! call millet_leaf_death_leg (g_dlt_leaf_no_dead)
 
         ! TEMPLATE OPTION
         ! Two routines return lai equilibria for alternative senescence
         ! routines for leaf area (millet_leaf_area_sen_water1 and millet_light1)
      ! call millet_lai_equilib_water (g_lai_equilib_water)
      ! call millet_lai_equilib_light (g_lai_equilib_light)
 
      call millet_leaf_area_sen (g_dlt_slai)
 
      call millet_dm_senescence (g_dlt_dm_senesced)
      call millet_N_senescence (g_dlt_N_senesced)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_leaf_death (dlt_leaf_no_dead)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_leaf_no_dead      ! (OUTPUT) new fraction of oldest
                                       ! green leaf

*+  Purpose
*       Return the fractional death of oldest green leaf.

*+  Changes
*     010994 jngh specified and programmed

*+  Calls
!      real       bound                 ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_leaf_death')

*+  Local Variables
      real       leaf_no_dead_today    ! total number of dead leaves today
      real       leaf_no_dead_yesterday ! total number of dead leaves
                                        ! yesterday
      real       leaf_no_dead_now      !
*
cejvo
      real       temp                  ! temporary variable to calculate
                                       ! slope 2
      real       ttsum                 ! temperature sum

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
cpsc need to develop leaf senescence functions for crop
 
      leaf_no_dead_yesterday = sum_between (emerg, now, g_leaf_no_dead)
 
cejvo made it absolute leafnumber and added second slope
 
      if ((stage_is_between (emerg, harvest_ripe, g_current_stage))
     : .and. (g_leaf_no_effective .lt. g_leaf_no_final)) then
 
cgol added upper and lower bounds
!         leaf_no_dead_today = amax1(leaf_no_dead_yesterday,
!     :    amin1((c_leaf_no_dead_const + c_leaf_no_dead_slope1
!     :          * sum_between (emerg, now, g_tt_tot))
!     :          , g_leaf_no_final))
cgd
      ttsum= sum_between (emerg, now, g_tt_tot)
      leaf_no_dead_now = c_leaf_no_dead_const + c_leaf_no_dead_slope1
     :                   * ttsum
      leaf_no_dead_now = u_bound (leaf_no_dead_now,g_leaf_no_final)
      leaf_no_dead_today = l_bound (leaf_no_dead_yesterday
     :                             , leaf_no_dead_now)
 
      g_lf_no_dead_at_flaglf = leaf_no_dead_today
!      ttsum= sum_between (emerg, now, g_tt_tot)
 
      elseif ((stage_is_between (emerg, harvest_ripe, g_current_stage))
     :   .and. (g_leaf_no_effective .ge. g_leaf_no_final) .and.
     :   (g_leaf_no_dead_const2 .eq. 0.0)) then
 
!         leaf_no_dead_today = amax1(leaf_no_dead_yesterday,
!     :    amin1((c_leaf_no_dead_const + c_leaf_no_dead_slope1
!     :          * sum_between (emerg, now, g_tt_tot))
!     :          , g_leaf_no_final))
 
      ttsum= sum_between (emerg, now, g_tt_tot)
      leaf_no_dead_now = c_leaf_no_dead_const + c_leaf_no_dead_slope1
     :                   * ttsum
      leaf_no_dead_now = u_bound (leaf_no_dead_now,g_leaf_no_final)
      leaf_no_dead_today = l_bound (leaf_no_dead_yesterday
     :                             , leaf_no_dead_now)
 
      temp = g_lf_no_dead_at_flaglf - (c_leaf_no_dead_slope2
     :         * sum_between (emerg, now, g_tt_tot))
      g_leaf_no_dead_const2 = temp
!      ttsum= sum_between (emerg, now, g_tt_tot)
 
      elseif((stage_is_between (emerg, harvest_ripe, g_current_stage))
     :   .and. (g_leaf_no_effective .ge. g_leaf_no_final) .and.
     :   (g_leaf_no_dead_const2 .ne. 0.0)) then
 
!         leaf_no_dead_today = amax1(leaf_no_dead_yesterday,
!     :    amin1((g_leaf_no_dead_const2 + c_leaf_no_dead_slope2
!     :          * sum_between (emerg, now, g_tt_tot))
!     :          , g_leaf_no_final))
 
      ttsum= sum_between (emerg, now, g_tt_tot)
      leaf_no_dead_now = g_leaf_no_dead_const2 + c_leaf_no_dead_slope2
     :                   * ttsum
      leaf_no_dead_now = u_bound (leaf_no_dead_now,g_leaf_no_final)
      leaf_no_dead_today = l_bound (leaf_no_dead_yesterday
     :                             , leaf_no_dead_now)
 
      elseif (on_day_of (harvest_ripe
     :                 , g_current_stage, g_days_tot)) then
         leaf_no_dead_today = g_leaf_no_final
 
      else
      leaf_no_dead_today = 0.0
 
      endif
 
cgol removed bound check
!      leaf_no_dead_today = bound (leaf_no_dead_today
!     :                           , leaf_no_dead_yesterday
!     :                           , g_leaf_no_final)
!
cgol added lower bound of zero
!      dlt_leaf_no_dead = amax1(0.0, leaf_no_dead_today -
!     : leaf_no_dead_yesterday)
      dlt_leaf_no_dead = l_bound (0.0, leaf_no_dead_today -
     : leaf_no_dead_yesterday)
 
cccc
!     write (*,*) 'fln',g_leaf_no_final,'slno',leaf_no_dead_today
!     write (*,*) 'efln',g_leaf_no_effective
!     write (*,*) 'slnfl',g_lf_no_dead_at_flaglf
!     write (*,*) 'temp',temp,'ttsum',ttsum
!     write (*,*) 'int2',g_leaf_no_dead_const2
cccc
 
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_leaf_death_leg (dlt_leaf_no_dead)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_leaf_no_dead      ! (OUTPUT) new fraction of oldest
                                       ! green leaf

*+  Purpose
*       Return the fractional death of oldest green leaf.

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_leaf_death_leg')

*+  Local Variables
      real       leaf_no_now            ! total number of leaves yesterday
      real       leaf_no_dead_now       ! total number of dead leaves
                                        ! yesterday
      real       leaf_death_rate        ! thermal time for senescence of
                                        ! another leaf (oCd)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
        leaf_death_rate = divide (c_leaf_no_dead_slope
     :                         , c_leaf_no_dead_const, 0.0)
 
      if (stage_is_between (flowering, harvest_ripe
     :                           , g_current_stage)) then
 
         dlt_leaf_no_dead = divide (g_dlt_tt, leaf_death_rate, 0.0)
 
      elseif (on_day_of (harvest_ripe
     :                 , g_current_stage, g_days_tot)) then
 
         leaf_no_now = sum_between (emerg, now, g_leaf_no)
         leaf_no_dead_now = sum_between (emerg, now, g_leaf_no_dead)
         dlt_leaf_no_dead = l_bound (leaf_no_now - leaf_no_dead_now,0.0)
 
      else
         dlt_leaf_no_dead = 0.0
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_lai_equilib_water (lai_equilib_water)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       lai_equilib_water(*)  ! (INPUT/OUTPUT) lai threshold for water
                                       ! senescence

*+  Purpose
*       Return the lai equilibrium water.

*+  Changes
*     010994 jngh specified and programmed
*     070795 jngh corrected for case of rue = 0
*     040895 jngh corrected for intercropping

*+  Calls
      real       millet_rue_reduction   ! function
      real       millet_transp_eff      ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_lai_equilib_water')

*+  Local Variables
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      real       dlt_dm_transp         ! potential dry matter production
                                       ! by transpiration (g/m^2)
      real       lai_equilib_water_today ! lai threshold for water senescence
      real       rue                   ! radiation use efficiency (g dm/mj)
      integer    stage_no              ! current stage no.
      real       sw_supply_sum         ! total supply over profile (mm)
      real       intc_crit             ! critical interception (0-1)
      real       radn_canopy           ! radiation reaching canopy mj/m^2)
      real       radn_int              ! radiation intercepted by canopy
                                       ! (mj/m^2)
      real       sen_radn_crit         ! critical radiation (mj/m^2)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      stage_no = int (g_current_stage)
 
      deepest_layer = find_layer_no (g_root_depth, g_dlayer
     :                             , max_layer)
      sw_supply_sum = sum_real_array (g_sw_supply, deepest_layer)
 
      dlt_dm_transp = sw_supply_sum * millet_transp_eff ()
      rue = c_rue(stage_no) * millet_rue_reduction ()
 
      call bound_check_real_var (rue, 0.0, c_rue(stage_no)
     :                           , 'rue')
 
      call millet_radn_int (radn_int)
      radn_canopy = divide (radn_int, g_cover_green, g_radn)
      sen_radn_crit = divide (dlt_dm_transp, rue, radn_canopy)
      intc_crit = divide (sen_radn_crit, radn_canopy, 1.0)
 
      if (intc_crit.lt.1.0) then
            ! needs rework for row spacing
         lai_equilib_water_today = -log (1.0 - intc_crit)
     :                           / c_extinction_coef
 
      else
         lai_equilib_water_today =  g_lai
      endif
 
      call millet_store_value (lai_equilib_water
     :                      , lai_equilib_water_today)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_lai_equilib_light (lai_equilib_light)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       lai_equilib_light(*)  ! (INPUT/OUTPUT) lai threshold for light
                                       ! senescence

*+  Purpose
*       Return the lai equilibrium light

*+  Changes
*     010994 jngh specified and programmed
*     040895 jngh corrected for intercropping

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_lai_equilib_light')

*+  Local Variables
      real       lai_equilib_light_today ! lai threshold for light senescence
      real       radn_canopy           ! radiation reaching canopy mj/m^2)
      real       radn_int              ! radiation intercepted by canopy
                                       ! (mj/m^2)
      real       trans_crit            ! critical transmission (0-1)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      call millet_radn_int (radn_int)
      radn_canopy = divide (radn_int, g_cover_green, 0.0)
      trans_crit = divide (c_sen_radn_crit, radn_canopy, 0.0)
 
      if (trans_crit.gt.0.0) then
            ! needs rework for row spacing
         lai_equilib_light_today = -log (trans_crit)/c_extinction_coef
 
      else
         lai_equilib_light_today = g_lai
      endif
 
      call millet_store_value (lai_equilib_light
     :                      , lai_equilib_light_today)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_leaf_area_sen (dlt_slai)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_slai              ! (OUTPUT) lai senesced

*+  Purpose
*       Return the lai that senesces on the current day

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_leaf_area_sen')

*+  Local Variables
      real       dlt_slai_age     !
      real       dlt_slai_light   !
      real       dlt_slai_water   !
      real       dlt_slai_frost   !

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
         ! get senescense from age, light competition, temperature
         ! and water stresses.
 
         ! TEMPLATE OPTION
         ! Standard routine derived from Ceres - simpler ?
         ! alternative routine (1) developed by GLH - mechanistic
      call millet_leaf_area_sen_age (dlt_slai_age)
         ! TEMPLATE OPTION or
      ! call millet_leaf_area_sen_age_leg (dlt_slai_age)
      ! call millet_leaf_area_sen_age1 (dlt_slai_age)
 
         ! TEMPLATE OPTION
      call millet_leaf_area_sen_light (dlt_slai_light)
         ! TEMPLATE OPTION or
      ! call millet_leaf_area_sen_light1 (dlt_slai_light)
 
         ! TEMPLATE OPTION
      call millet_leaf_area_sen_water (dlt_slai_water)
         ! TEMPLATE OPTION or
      ! call millet_leaf_area_sen_water1 (dlt_slai_water)
 
         ! TEMPLATE OPTION
      call millet_leaf_area_sen_frost (dlt_slai_frost)
         ! TEMPLATE OPTION or
      ! call millet_leaf_area_sen_frost1 (dlt_slai_frost)
 
         ! now take largest of deltas
      dlt_slai = max (dlt_slai_age
     :              , dlt_slai_light
     :              , dlt_slai_water
     :              , dlt_slai_frost)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_leaf_area_sen_age (dlt_slai_age)
*     ===========================================================
      implicit none
      include   'convert.inc'
      include   'millet.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_slai_age          ! (OUTPUT) new senesced lai from
                                       ! phasic devel.

*+  Purpose
*       Return the lai that would senesce on the
*       current day due to ageing

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_leaf_area_sen_age')

*+  Local Variables
      real       area_sen_dying_leaf   ! senesced leaf area from
                                       ! current leaf dying (mm^2)
      integer    dying_leaf            ! current leaf number dying ()
      real       leaf_no_dead          ! today's number of dead leaves ()
      real       slai_age              ! lai senesced by natural ageing

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
         ! now calculate the leaf senescence
         ! due to normal phenological (phasic) development
 
         ! get highest leaf no. senescing today
 
 
      leaf_no_dead = sum_between (emerg, now, g_leaf_no_dead)
     :             + g_dlt_leaf_no_dead
 
      dying_leaf = int (1.0 + leaf_no_dead)
 
         ! get area senesced from highest leaf no.
 
      area_sen_dying_leaf = mod (leaf_no_dead, 1.0)
     :                    * g_leaf_area(dying_leaf)
 
      slai_age = (sum_real_array (g_leaf_area, dying_leaf - 1)
     :         + area_sen_dying_leaf)
     :         * smm2sm * g_plants
 
      dlt_slai_age = bound (slai_age - g_slai, 0.0, g_lai)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_leaf_area_sen_age_leg (dlt_slai_age)
*     ===========================================================
      implicit none
      include   'convert.inc'
      include   'millet.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_slai_age          ! (OUTPUT) new senesced lai from
                                       ! phasic devel.

*+  Purpose
*       Return the lai that would senesce on the
*       current day due to ageing

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_leaf_area_sen_age_leg')

*+  Local Variables
      real       area_sen_dying_leaf   ! senesced leaf area from
                                       ! current leaf dying (mm^2)
      integer    dying_leaf            ! current leaf number dying ()
      real       leaf_no_dead          ! today's number of dead leaves ()
      real       slai_age              ! lai senesced by natural ageing

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
         ! now calculate the leaf senescence
         ! due to normal phenological (phasic) development
 
         ! get highest leaf no. senescing today
 
      leaf_no_dead = sum_between (emerg, now, g_leaf_no_dead)
     :             + g_dlt_leaf_no_dead
 
      dying_leaf = int (1.0 + leaf_no_dead)
 
         ! get area senesced from highest leaf no.
 
      area_sen_dying_leaf = mod (leaf_no_dead, 1.0)
     :            * c_leaf_size_average
 
      slai_age = (sum_real_array (g_leaf_area, dying_leaf - 1)
     :         + area_sen_dying_leaf)
     :         * smm2sm * g_plants
 
      dlt_slai_age = bound (slai_age - g_slai, 0.0, g_lai)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_leaf_area_sen_age1 (dlt_slai_age)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_slai_age     ! (OUTPUT)

*+  Purpose
*       Return the lai that would senesce  on the
*       current day from natural ageing

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_leaf_area_sen_age1')

*+  Local Variables
      real       spla_inflection       ! inflection point of leaf area
                                       ! senescence function (oC)
      real       slai_today            ! total senescence up to today
      real       tt_since_emerg        ! thermal time since emergence (oC)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
         ! calculate senescence due to ageing
 
      if (stage_is_between (floral_init, harvest_ripe
     :                     , g_current_stage)) then
 
         tt_since_emerg = sum_between (emerg, now, g_tt_tot)
         spla_inflection = p_spla_intercept
     :                   + c_spla_slope * g_leaf_no_final
         slai_today = divide ((g_lai + g_slai)
     :              , (1.0 + exp(-p_spla_prod_coef
     :                        * (tt_since_emerg - spla_inflection)))
     :              , 0.0)
 
         dlt_slai_age = l_bound (slai_today - g_slai, 0.0)
 
         ! all leaves senesce at harvest ripe
      elseif (on_day_of (harvest_ripe
     :                 , g_current_stage, g_days_tot)) then
          dlt_slai_age = g_lai + g_dlt_lai
 
      else
         dlt_slai_age = 0.0
      endif
 
      dlt_slai_age = bound (dlt_slai_age, 0.0, g_lai)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_leaf_area_sen_water (dlt_slai_water)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_slai_water        ! (OUTPUT) water stress senescense

*+  Purpose
*       Return the lai that would senesce on the
*       current day due to water stress

*+  Changes
*     010994 jngh specified and programmed

*+  Calls
      real       millet_swdef           ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_leaf_area_sen_water')

*+  Local Variables
      real       slai_water_fac        ! drought stress factor (0-1)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
         ! get senescense from stresses.
 
         ! calculate 0-1 factors for leaf senescence due to drought
         ! stress
 
         ! drought stress factor
 
      slai_water_fac = c_sen_rate_water* (1.0 - millet_swdef (photo))
 
      dlt_slai_water = g_lai * slai_water_fac
      dlt_slai_water = bound (dlt_slai_water, 0.0, g_lai)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_leaf_area_sen_water1 (dlt_slai_water)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_slai_water        ! (OUTPUT) water stress senescense

*+  Purpose
*       Return the lai that would senesce on the
*       current day from water stress

*+  Changes
*     010994 jngh specified and programmed

*+  Calls
      real       millet_running_ave     ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_leaf_area_sen_water1')

*+  Local Variables
      real       ave_lai_equilib_water ! running mean lai threshold for water
                                       ! senescence ()
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      real       sw_demand_ratio       ! water supply:demand ratio
      real       sw_supply_sum         ! total supply over profile (mm)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
         ! calculate senescense from water stress
 
         ! NOTE needs rework for multiple crops
 
      deepest_layer = find_layer_no (g_root_depth, g_dlayer, max_layer)
      sw_supply_sum = sum_real_array (g_sw_supply, deepest_layer)
      sw_demand_ratio = divide (sw_supply_sum, g_sw_demand, 1.0)
 
      if (sw_demand_ratio.lt.c_sen_threshold) then
 
         ave_lai_equilib_water = millet_running_ave
     :                           (g_lai_equilib_water, 10)
 
         dlt_slai_water = (g_lai - ave_lai_equilib_water)
     :                  / c_sen_water_time_const
 
         dlt_slai_water = l_bound (dlt_slai_water, 0.0)
 
      else
         dlt_slai_water = 0.0
 
      endif
 
      dlt_slai_water = bound (dlt_slai_water, 0.0, g_lai)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_leaf_area_sen_light (dlt_slai_light)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_slai_light        ! (OUTPUT) lai senesced by low light

*+  Purpose
*       Return the lai that would senesce on the
*       current day due to light competition

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_leaf_area_sen_light')

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
 
      dlt_slai_light = g_lai * slai_light_fac
      dlt_slai_light = bound (dlt_slai_light, 0.0, g_lai)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_leaf_area_sen_light1 (dlt_slai_light)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_slai_light        ! (OUTPUT) lai senesced by low light

*+  Purpose
*       Return the lai that would senesce on the
*       current day from low light

*+  Changes
*     010994 jngh specified and programmed

*+  Calls
      real       millet_running_ave     ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_leaf_area_sen_light1')

*+  Local Variables
      real       ave_lai_equilib_light ! running mean lai threshold for light
                                       ! senescence ()
      real       radn_int              ! radn intercepted by leaves (mj/m^2)
      real       radn_transmitted      ! radn transmitted through canopy
                                       ! (mj/m^2)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
         ! calculate senescense from water stress
 
c+!!!!!!!! this doesnt account for other growing crops
c+!!!!!!!! should be based on reduction of intercepted light and k*lai
c+!!!!!!!!
             ! calculate senescence due to low light
 
      call millet_radn_int (radn_int)
      radn_transmitted = g_radn - radn_int
 
      if (radn_transmitted.lt.c_sen_radn_crit) then
 
         ave_lai_equilib_light = millet_running_ave
     :                           (g_lai_equilib_light, 10)
 
         dlt_slai_light = divide (g_lai - ave_lai_equilib_light
     :                          , c_sen_light_time_const , 0.0)
 
         dlt_slai_light = l_bound (dlt_slai_light, 0.0)
 
      else
         dlt_slai_light = 0.0
 
      endif
 
      dlt_slai_light = bound (dlt_slai_light, 0.0, g_lai)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_leaf_area_sen_frost (dlt_slai_frost)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_slai_frost        ! (OUTPUT) lai frosted today

*+  Purpose
*       Return the lai that would senesce on the
*       current day from low temperatures

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_leaf_area_sen_frost')

*+  Local Variables
      real       sen_fac_temp          ! low temperature factor (0-1)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
         ! get senescense from stresses.
 
         ! calculate 0-1 factors for leaf senescence due to  low temperature.
 
      sen_fac_temp = linear_interp_real (g_mint
     :                                 , c_x_temp_senescence
     :                                 , c_y_senescence_fac
     :                                 , c_num_temp_senescence)
 
      dlt_slai_frost = sen_fac_temp * g_lai
      dlt_slai_frost = bound (dlt_slai_frost, 0.0, g_lai)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_leaf_area_sen_frost1 (dlt_slai_frost)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_slai_frost        ! (OUTPUT) lai frosted today

*+  Purpose
*       Return the lai that would senesce on the
*       current day from frosting

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_leaf_area_sen_frost1')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
          ! calculate senecence due to frost
 
      if (g_mint.le.c_frost_kill) then
         dlt_slai_frost = g_lai
 
      else
         dlt_slai_frost = 0.0
      endif
 
      dlt_slai_frost = bound (dlt_slai_frost, 0.0, g_lai)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      real function millet_running_ave (array, number_of_days)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       array(*)              ! (INPUT) array to use for average
      integer    number_of_days        ! (INPUT) number of days to average
                                       ! over

*+  Purpose
*       return the running average of an array over the last specified
*       number of days.

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_running_ave')

*+  Local Variables
      integer    start_day             ! day of year to start running
                                       ! average

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      start_day = offset_day_of_year (g_year
     :                              , g_day_of_year, - number_of_days)
 
      millet_running_ave = divide (sum_part_of_real (array
     :                                            , start_day
     :                                            , g_day_of_year, 366)
     :                          , real (abs (number_of_days)), 0.0)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_dm_senescence (dlt_dm_senesced)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_dm_senesced(*)    ! (OUTPUT) actual biomass senesced
                                       ! from plant parts (g/m^2)

*+  Purpose
*       Derives seneseced plant dry matter (g/m^2)

*+  Changes
*       091294 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'millet_dm_senescence')

*+  Local Variables
      real       dlt_dm_senescing      ! dm of leaves senesced (g/m^2)
      real       dm_green_leaf_today   ! today's green leaf dry matter (g/m^2)
      real       lai_today             ! today's green lai
      real       sla_today             ! today's specific leaf area (m^2/g)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
         ! first we zero all plant component deltas
 
      call fill_real_array (dlt_dm_senesced, 0.0, max_part)
 
      lai_today = g_lai + g_dlt_lai
 
      if (g_dlt_slai .lt. lai_today) then
         dm_green_leaf_today = g_dm_green(leaf)
     :                       + g_dlt_dm_green(leaf)
     :                       + g_dlt_dm_green_retrans(leaf) ! -ve outflow
         sla_today = divide (lai_today, dm_green_leaf_today, 0.0)
 
         dlt_dm_senescing = divide (g_dlt_slai, sla_today, 0.0)
 
      else
         dlt_dm_senescing = g_dm_green(leaf)
     :                   + g_dlt_dm_green(leaf)
      endif
 
         ! a proportion of senesced leaf dry matter may be retranslocated to
         ! the stem
 
      dlt_dm_senesced(leaf) = dlt_dm_senescing * c_dm_leaf_sen_frac
      dlt_dm_senesced(stem) = - (dlt_dm_senescing
     :                         - dlt_dm_senesced(leaf))
 
      dlt_dm_senesced(root) = g_dm_green(root) * c_dm_root_sen_frac
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_N_senescence (dlt_N_senesced)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_N_senesced(*)     ! (OUTPUT) actual nitrogen senesced
                                       ! from plant parts (g/m^2)

*+  Purpose
*       Derives seneseced plant nitrogen (g N/m^2)

*+  Changes
*       091294 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'millet_N_senescence')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
         ! first we zero all plant component deltas
 
      call fill_real_array (dlt_N_senesced, 0.0, max_part)
 
      dlt_N_senesced(leaf) = g_dlt_dm_senesced(leaf)
     :                     * c_N_leaf_sen_conc
      dlt_N_senesced(root) = g_dlt_dm_senesced(root)
     :                     * c_N_root_sen_conc
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_update ()
*     ===========================================================
      implicit none
      include   'convert.inc'
      include   'millet.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Purpose
*       Update states

*+  Changes
*      250894 jngh specified and programmed

*+  Calls
      real       millet_nfact           ! function
      real       millet_swdef           ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_update')

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
      real       dm_residue            ! dry matter added to residue (g/m^2)
      real       dying_fract           ! fraction op population dying (0-1)
      real       extinct_coef          ! extinction coef of green leaves
      real       extinct_coef_dead     ! extinction coef of dead leaves
      real       N_residue             ! nitrogen added to residue (g/m^2)
      real       leaf_no               ! currently expanding leaf no.
      integer    part                  ! plant part index
*
! gd
!      character  module_name*8         ! module name

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
 
      call subtract_real_array (g_dlt_N_dead_detached, g_N_dead
     :                        , max_part)
 
      call add_real_array (g_dlt_N_green, g_N_green, max_part)
      call add_real_array (g_dlt_N_retrans, g_N_green, max_part)
      call subtract_real_array (g_dlt_N_senesced, g_N_green
     :                        , max_part)
      g_N_green(tiller) = g_N_green(tiller) - g_N_tiller_independence
 
      call add_real_array (g_dlt_N_senesced, g_N_senesced
     :                   , max_part)
      call subtract_real_array (g_dlt_N_detached, g_N_senesced
     :                        , max_part)
 
      dying_fract = divide (-g_dlt_plants, g_plants, 0.0)
      dying_fract = bound (dying_fract, 0.0, 1.0)
 
      do 1000 part = 1, max_part
         dlt_N_green_dead = g_N_green(part) * dying_fract
         g_N_green(part) = g_N_green(part) - dlt_N_green_dead
         g_N_dead(part) = g_N_dead(part) + dlt_N_green_dead
 
         dlt_N_senesced_dead = g_N_senesced(part) * dying_fract
         g_N_senesced(part) = g_N_senesced(part) - dlt_N_senesced_dead
         g_N_dead(part) = g_N_dead(part) + dlt_N_senesced_dead
1000  continue
 
         ! Transfer plant dry matter
 
      dlt_dm_plant = divide (g_dlt_dm, g_plants, 0.0)
 
      call accumulate (dlt_dm_plant, g_dm_plant_top_tot
     :               , g_previous_stage, g_dlt_stage)
 
      call subtract_real_array (g_dlt_dm_dead_detached, g_dm_dead
     :                        , max_part)
 
      call add_real_array (g_dlt_dm_green, g_dm_green, max_part)
      call add_real_array (g_dlt_dm_green_retrans, g_dm_green, max_part)
      call subtract_real_array (g_dlt_dm_senesced, g_dm_green
     :                        , max_part)
      g_dm_green(tiller) = g_dm_green(tiller) - g_dm_tiller_independence
 
      call add_real_array (g_dlt_dm_senesced, g_dm_senesced
     :                   , max_part)
      call subtract_real_array (g_dlt_dm_detached, g_dm_senesced
     :                        , max_part)
 
      do 2000 part = 1, max_part
         dlt_dm_green_dead = g_dm_green(part) * dying_fract
         g_dm_green(part) = g_dm_green(part) - dlt_dm_green_dead
         g_dm_dead(part) = g_dm_dead(part) + dlt_dm_green_dead
 
         dlt_dm_senesced_dead = g_dm_senesced(part) * dying_fract
         g_dm_senesced(part) = g_dm_senesced(part)
     :                       - dlt_dm_senesced_dead
         g_dm_dead(part) = g_dm_dead(part) + dlt_dm_senesced_dead
2000  continue
 
         ! initiate new tiller and transfer any DM and N content.
 
      call millet_tiller_initiate (g_dm_tiller_independence
     :                          , g_N_tiller_independence)
 
         ! dispose of detached material from senesced parts in
         ! live population
 
      dm_residue = (sum_real_array (g_dlt_dm_detached, max_part)
     :           - g_dlt_dm_detached(root))
      N_residue = (sum_real_array (g_dlt_N_detached, max_part)
     :          - g_dlt_N_detached(root))
 
      call millet_top_residue (dm_residue, N_residue)
 
         ! put roots into root residue
 
      call millet_root_incorp (g_dlt_dm_detached(root)
     :                    , g_dlt_N_detached(root))
 
         ! now dispose of dead population detachments
 
      dm_residue = (sum_real_array (g_dlt_dm_dead_detached, max_part)
     :           - g_dlt_dm_dead_detached(root))
      N_residue = (sum_real_array (g_dlt_N_dead_detached, max_part)
     :          - g_dlt_N_dead_detached(root))
 
      call millet_top_residue (dm_residue, N_residue)
 
         ! put roots into root residue
 
      call millet_root_incorp (g_dlt_dm_dead_detached(root)
     :                      , g_dlt_N_dead_detached(root))
 
         ! tiller development
      call accumulate (g_dlt_tiller_no, g_tiller_no
     :               , g_previous_stage, g_dlt_stage)
 
!         call get_current_module (module_name)
!
! gd
!      write (*,*) 'module name', module_name
!      write (*,*) 'dlt_tiller_no = ',g_dlt_tiller_no
!      write (*,*) 'tiller_no = ', g_tiller_no
!      write (*,*) 'previous_stage = ', g_previous_stage
!      write (*,*) 'dlt_stage = ', g_dlt_stage
cjh
         ! transfer plant grain no.
      dlt_grain_no_lost  = g_grain_no * dying_fract
      g_grain_no = g_grain_no - dlt_grain_no_lost
 
         ! transfer plant leaf area
 
      g_lai = g_lai + g_dlt_lai - g_dlt_slai
      g_slai = g_slai + g_dlt_slai - g_dlt_slai_detached
 
      dlt_lai_dead  = g_lai  * dying_fract
      dlt_slai_dead = g_slai * dying_fract
 
      g_lai = g_lai - dlt_lai_dead
      g_slai = g_slai - dlt_slai_dead
      g_tlai_dead = g_tlai_dead + dlt_lai_dead + dlt_slai_dead
     :            - g_dlt_tlai_dead_detached
 
         ! now update new canopy covers
! ejvo
      extinct_coef = linear_interp_real (g_row_spacing
     :                        , c_x_row_spacing, c_y_extinct_coef
     :                        , c_num_row_spacing)
 
      extinct_coef_dead = linear_interp_real (g_row_spacing
     :                        , c_x_row_spacing, c_y_extinct_coef_dead
     :                        , c_num_row_spacing)
 
      call millet_cover (g_cover_green, extinct_coef, g_lai)
      call millet_cover (g_cover_sen, extinct_coef_dead, g_slai)
      call millet_cover (g_cover_dead, extinct_coef_dead
     :                 , g_tlai_dead)
 
 
         ! plant leaf development
         ! need to account for truncation of partially developed leaf (add 1)
      leaf_no = 1.0 + sum_between (emerg, now, g_leaf_no)
      dlt_leaf_area = divide (g_dlt_lai, g_plants, 0.0) * sm2smm
      call accumulate (dlt_leaf_area, g_leaf_area
     :               , leaf_no, g_dlt_leaf_no)
 
      call accumulate (g_dlt_leaf_no, g_leaf_no
     :               , g_previous_stage, g_dlt_stage)
 
      call accumulate (g_dlt_leaf_no_dead, g_leaf_no_dead
     :               , g_previous_stage, g_dlt_stage)
 
         ! plant stress
 
      call accumulate (g_dlt_heat_stress_tt, g_heat_stress_tt
     :               , g_previous_stage, g_dlt_stage)
 
      call accumulate (g_dlt_dm_stress_max, g_dm_stress_max
     :               , g_current_stage, g_dlt_stage)
 
      call accumulate (1.0 - millet_swdef(photo), g_cswd_photo
     :               , g_previous_stage, g_dlt_stage)
      call accumulate (1.0 - millet_swdef(expansion), g_cswd_expansion
     :               , g_previous_stage, g_dlt_stage)
      call accumulate (1.0 - millet_swdef(pheno), g_cswd_pheno
     :               , g_previous_stage, g_dlt_stage)
 
      call accumulate (1.0 - millet_nfact(photo), g_cnd_photo
     :               , g_previous_stage, g_dlt_stage)
      call accumulate (1.0 - millet_nfact(grain_conc), g_cnd_grain_conc
     :               , g_previous_stage, g_dlt_stage)
 
         ! other plant states
 
      g_canopy_height = g_canopy_height + g_dlt_canopy_height
      g_plants = g_plants + g_dlt_plants
      g_root_depth = g_root_depth + g_dlt_root_depth
 
      call millet_N_conc_limits (g_N_conc_crit
     :                        , g_N_conc_max
     :                        , g_N_conc_min)  ! plant N concentr
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_check_bounds ()
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Purpose
*         Check bounds of internal pools

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_check_bounds')

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
      subroutine millet_totals ()
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Purpose
*         Collect totals of crop variables for output

*+  Changes
*     010994 jngh specified and programmed

*+  Calls
cpsc  add below
cjh      include   'convert.inc'          ! gm2kg, sm2ha, sm2smm

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_totals')

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
         g_isdate = g_day_of_year
      else if (on_day_of (maturity, g_current_stage, g_days_tot)) then
         g_mdate = g_day_of_year
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
      subroutine millet_event ()
*     ===========================================================
      implicit none
      include   'const.inc'            ! new_line, lu_scr_sum, blank,
      include   'convert.inc'
      include   'millet.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'write.pub'                         
      include 'error.pub'                         

*+  Purpose
*       Report occurence of event and the current status of specific
*       variables.

*+  Changes
*     010994 jngh specified and programmed

*+  Calls
                                       ! lu_scr_sum

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_event')

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
         call report_event (string)
 
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
            call write_string (lu_scr_sum, string)
         else
         endif
 
      else
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_cover (cover, extinction_coef, lai)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       cover                 ! (OUTPUT) fraction of radn that is
                                       !  intercepted by leaves (0-1)
      real       extinction_coef       ! (INPUT) extinction coefficient ()
      real       lai                   ! (INPUT) leaf area index ()

*+  Purpose
*       'Cover' by leaves (0-1) . Fraction of radiation reaching the
*       canopy, intercepted by the leaves of the canopy.

*+  Changes
*     040895 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_cover')

*+  Local Variables
!      real       k_coef                ! extinction coefficient

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
         ! this equation implies that leaf interception of
         ! solar radiation obeys Beer's law
 
!     if (g_row_spacing .gt. 0.0) then
!        k_coef = exp (-c_extinction_coef_change
!    :                * extinction_coef
!    :                * g_row_spacing)
!     else
!        k_coef = extinction_coef
!     endif
 
      cover = 1.0 - exp (-extinction_coef * lai)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_root_distrib (root_array, root_sum)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       root_array(*)         ! (OUTPUT) array to contain
                                       ! distributed material
      real       root_sum              ! (INPUT) Material to be distributed

*+  Purpose
*       Distribute root material over profile

*+  Changes
*       290994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'millet_root_distrib')

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
         root_distrb(layer) = exp (-c_root_extinction
     :                      * divide (cum_depth, g_root_depth, 0.0))
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
      subroutine millet_top_residue (dlt_residue_weight, dlt_residue_N)
*     ===========================================================
      implicit none
      include   'const.inc'            ! all_active_modules
      include   'convert.inc'
      include   'millet.inc'
      include 'engine.pub'                        
      include 'intrface.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_residue_weight    ! (INPUT) new surface residue (g/m^2)
      real       dlt_residue_N         ! (INPUT) new surface residue N (g/m^2)

*+  Purpose
*       Add residue to residue pool

*+  Changes
*       220794 jngh specified and programmed
*       170895 jngh changed message send to message pass to module
*       220696 jngh changed to post_ construct

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'millet_top_residue')

*+  Local Variables
cjh      character  string*200            ! output string

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (dlt_residue_weight.gt.0.0) then
            ! send out surface residue
 
cjh         write(string, '(2a, 2(a, g16.7e3, a))' )
cjh     :           'dlt_residue_type = ', c_crop_type
cjh     :         , ', dlt_residue_wt = '
 
cjh     :         , dlt_residue_weight * gm2kg /sm2ha, '(kg/ha)'
cjh     :         , ', dlt_residue_n = '
cjh     :         , dlt_residue_N * gm2kg /sm2ha, '(kg/ha)'
 
cjh         call message_pass_to_module (all_active_modules
cjh     :                               , 'add_residue'
cjh     :                               , string)
 
         call New_postbox ()
 
         call post_char_var('dlt_residue_type','()',c_crop_type)
 
         call post_real_var ('dlt_residue_wt'
     :                        ,'(kg/ha)'
     :                        ,dlt_residue_weight * gm2kg /sm2ha)
 
         call post_real_var ('dlt_residue_n'
     :                        ,'(kg/ha)'
     :                        ,dlt_residue_N * gm2kg /sm2ha)
 
         call message_send_immediate (
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
      subroutine millet_root_incorp (dlt_dm_root, dlt_N_root)
*     ===========================================================
      implicit none
      include   'const.inc'            ! all_active_modules
      include   'convert.inc'
      include   'millet.inc'
      include 'science.pub'                       
      include 'engine.pub'                        
      include 'intrface.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_dm_root           ! (INPUT) new root residue dm (g/m^2)
      real       dlt_N_root            ! (INPUT) new root residue N (g/m^2)

*+  Purpose
*       Add root residue to root residue pool

*+  Changes
*       220794 jngh specified and programmed
*       170895 jngh changed message send to message pass to module
*       220696 jngh changed to post_ construct

*+  Calls
cjh      integer    lastnb                ! function
cjh      character  string_concat*(mes_data_size) ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'millet_root_incorp')

*+  Local Variables
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      real       dlt_dm_incorp(max_layer) ! root residue (kg/ha)
      real       dlt_N_incorp(max_layer)  ! root residue N (kg/ha)
*
cjh      integer    layer                 ! layer number
cjh      character  string*(mes_data_size) ! output string

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (dlt_dm_root.gt.0.0) then
 
            ! send out root residue
 
         call millet_root_distrib (dlt_dm_incorp
     :                          , dlt_dm_root * gm2kg /sm2ha)
         call millet_root_distrib (dlt_N_incorp
     :                          , dlt_N_root * gm2kg /sm2ha)
 
         deepest_layer = find_layer_no (g_root_depth, g_dlayer
     :                                , max_layer)
 
cjh         string = 'dlt_fom_type='// c_crop_type
 
cjh         write (string(lastnb(string)+1:), '(a, 20g16.7e3)' )
cjh     :              ', dlt_fom_wt = '
cjh     :               , (dlt_dm_incorp(layer), layer = 1, deepest_layer)
cjh         string =  string_concat (string, '(kg/ha)')
 
cjh         write (string(lastnb(string)+1:), '(a, 20g16.7e3)')
cjh     :              ', dlt_fom_n = '
cjh     :               , (dlt_N_incorp(layer), layer = 1, deepest_layer)
cjh         string = string_concat (string, '(kg/ha)')
 
cjh         call message_pass_to_module (all_active_modules
cjh     :                               , 'incorp_fom'
cjh     :                               , string)
 
         call New_postbox ()
 
         call post_char_var ('dlt_fom_type=','()',c_crop_type)
 
         call post_real_array ('dlt_fom_wt'
     :                        ,'(kg/ha)'
     :                        ,dlt_dm_incorp
     :                        ,deepest_layer)
 
         call post_real_array ('dlt_fom_n'
     :                        ,'(kg/ha)'
     :                        ,dlt_n_incorp
     :                        ,deepest_layer)
 
         call message_send_immediate (
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
      real function millet_stage_code (stage_no, stage_table, numvals)
*     ===========================================================
      implicit none
      include   'const.inc'            ! err_user
      include   'millet.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       stage_no              ! (INPUT) stage number to convert
      real       stage_table(*)        ! (INPUT) table of stage codes
      integer    numvals               ! (INPUT) size_of of table

*+  Purpose
*       Return an interpolated stage code from a table of stage_codes
*       and a nominated stage number. Returns 0 if the stage number is not
*       found. Interpolation is done on thermal time.

*+  Changes
*       080994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_stage_code')

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
      millet_stage_code = x_stage_code
 
      call pop_routine (my_name)
 
      return
      end



*     ===========================================================
      subroutine millet_tillering ()
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'millet.inc'
      include 'error.pub'                         

*+  Purpose
*       Tiller routine

*+  Changes
*       091095 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'millet_tillering')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (c_tiller_no_pot.gt.0) then
 
         if (c_tiller_appearance.eq.'tt') then
            call millet_tiller_appearance_tt (g_dlt_tiller_no)
 
         elseif (c_tiller_appearance.eq.'dm') then
            call millet_tiller_appearance_dm (g_dlt_tiller_no)
 
         else
            ! no tillers simulated
            call fatal_error (err_user
     :                     , 'No tiller appearance method supplied')
         endif
         call millet_tiller_independence (g_tiller_independence
     :                                 , g_dm_tiller_independence
     :                                 , g_N_tiller_independence)
 
      else
         ! no tillers simulated
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_tiller_appearance_tt (dlt_tiller_no)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_tiller_no         ! (OUTPUT) new fraction of next
                                       ! tiller to emerge

*+  Purpose
*       Uses thermal time to return the fractional increase in
*       appearance of the next tiller to emerge.

*+  Changes
*       091095 jngh specified and programmed
*       261097 gol added global variable 'g_daylength_at_emerg' and global
*                  constant 'c_photo_tiller_crit' and new section to amend
*                  tiller initiation thermal time

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_tiller_appearance_tt')

*+  Local Variables
c gol added 'g_daylength_at_emerg (h)' and 'c_photo_tiller_crit (h)' to constants
c (millet.inc). 'c_photo_tiller_crit' is the critical photoperiod above which
c the thermal time for first tiller initiation (global constant 'c_y_tiller_tt')
c is increased in a one-step process
*
      real       tiller_no_remaining   ! number of tillers to go before
                                       ! potential no. is reached  ()
      real       tiller_no_now         ! number of tillers ()
      real       tiller_no_next        ! next tiller number ()
      real       tiller_app_rate       ! rate of tiller appearance (oCd/tiller)
!      real       ttsum                 ! temperature sum

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
cejvo made leaf appearance function of plant density
      g_y_tiller_tt_adj(2) = c_y_tiller_tt(2)
     :                       + c_tiller_appearance_slope
     :                       * g_plants
 
cgol added tiller initiation adjustment for daylength
 
      if (on_day_of (emerg, g_current_stage, g_days_tot)) then
 
         g_daylength_at_emerg = day_length (g_day_of_year, g_latitude,
     :                                      c_twilight)
 
         if (g_daylength_at_emerg.gt.c_photo_tiller_crit) then
 
            g_y_tiller_tt_adj(1) = c_y_tiller_tt(1)
     :                             + g_y_tiller_tt_adj(2)
 
         else
 
             g_y_tiller_tt_adj(1) = c_y_tiller_tt(1)
 
         endif
 
      endif
 
cgol check calculations (now turned off)
!      write (*,*) g_day_of_year, g_latitude, c_twilight,
!     : g_daylength_at_emerg, c_photo_tiller_crit, g_y_tiller_tt_adj(1),
!     : c_y_tiller_tt(2)
!     : g_y_tiller_tt_adj(2)
 
      if (stage_is_between (emerg, flag_leaf, g_current_stage)) then
 
cgol bounds added to tiller number determination
!     for tiller_no_now and tiller_no_remaining to stop model initating
!     tillers beyond the potential tiller number (set previously to 5)
 
!         tiller_no_now = amin1(amax1(0.0, sum_between (emerg, now,
!     :                         g_tiller_no)), c_tiller_no_pot)
!         tiller_no_remaining = amax1(0.0, (real (c_tiller_no_pot) -
!     :                               tiller_no_now))
 
         tiller_no_now = sum_between (emerg, now, g_tiller_no)
 
         tiller_no_now = l_bound (0.0, tiller_no_now)
 
         tiller_no_now = u_bound (tiller_no_now, real(c_tiller_no_pot))
 
         tiller_no_remaining = l_bound(0.0, real(c_tiller_no_pot
     :                                       - tiller_no_now))
 
         tiller_no_next = aint (tiller_no_now) + 1.0
 
!gd
!      write (*,*) g_current_stage,
!     : tiller_no_now,tiller_no_next,tiller_no_remaining
!       write (*,*) 'ttsum',ttsum, 'yt',g_y_tiller_tt_adj
!     : c_num_tiller_no_next
!      write (*,*) c_x_tiller_no_next, c_y_tiller_tt,c_num_tiller_no_next
 
         tiller_app_rate = linear_interp_real (tiller_no_next
     :                       , c_x_tiller_no_next, g_y_tiller_tt_adj
     :                       , c_num_tiller_no_next)
!
!      write (*,*) tiller_app_rate
!
         dlt_tiller_no = divide (g_dlt_tt, tiller_app_rate, 0.0)
         dlt_tiller_no = bound (dlt_tiller_no, 0.0, tiller_no_remaining)
 
      else
             ! we have full number of leaves.
 
         dlt_tiller_no = 0.0
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_tiller_appearance_dm (dlt_tiller_no)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_tiller_no         ! (OUTPUT) new fraction of next
                                       ! tiller to emerge

*+  Purpose
*       Uses assimilate flux to determine the fractional increase in
*       appearance of the next tiller to emerge.

*+  Changes
*       091095 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_tiller_appearance_dm')

*+  Local Variables
      real       tiller_no_remaining   ! number of tillers to go before
                                       ! potential no. is reached
      real       tiller_no_now         ! number of tillers

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (stage_is_between (emerg, flag_leaf, g_current_stage)) then
 
         tiller_no_now = sum_between (emerg, now, g_tiller_no)
         tiller_no_remaining = real (c_tiller_no_pot) - tiller_no_now
         dlt_tiller_no = divide (g_dm_green(tiller)
     :                         , c_dm_tiller_crit*g_plants, 0.0)
 
         dlt_tiller_no = bound (dlt_tiller_no, 0.0, tiller_no_remaining)
 
      else
             ! we have full number of leaves.
 
         dlt_tiller_no = 0.0
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_tiller_independence (tiller_independence
     :                                    , dm_tiller_independence
     :                                    , N_tiller_independence)
*     ===========================================================
      implicit none
      include   'const.inc'            ! all_active_modules
      include   'convert.inc'
      include   'millet.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    tiller_independence    ! (OUTPUT) new tiller ready for
                                        ! independence ()
      real       dm_tiller_independence ! (OUTPUT) new tiller DM (g/m^2)
      real       N_tiller_independence  ! (OUTPUT) new tiller N (g/m^2)

*+  Purpose
*       Initiate millet tiller module

*+  Changes
*       101095 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'millet_tiller_independence')

*+  Local Variables
      real       dm_tiller_fract       ! fraction of DM to this tiller ()
      real       tiller_no             ! number of tillers developed ()

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      tiller_no = sum_between (emerg, now, g_tiller_no)
      if (aint (tiller_no + g_dlt_tiller_no) .gt. tiller_no) then
            ! tiller is independent
 
         tiller_independence = 1
         if (c_tiller_appearance.eq.'dm') then
            dm_tiller_independence = c_dm_tiller_crit * g_plants
            dm_tiller_fract = divide (dm_tiller_independence
     :                              , g_dm_green(tiller), 0.0)
            N_tiller_independence = g_N_green(tiller) * dm_tiller_fract
 
         else
               ! tiller not collecting DM or N
            dm_tiller_independence = 0.0
            N_tiller_independence = 0.0
 
         endif
 
      else
            ! tiller not independent yet
         tiller_independence = 0
         dm_tiller_independence = 0.0
         N_tiller_independence = 0.0
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_tiller_initiate (dm_tiller_independence
     :                                , N_tiller_independence)
*     ===========================================================
      implicit none
      include   'const.inc'            ! all_active_modules
      include   'millet.inc'
      include 'string.pub'                        
      include 'data.pub'                          
      include 'intrface.pub'                      
      include 'engine.pub'                        
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dm_tiller_independence ! (INPUT) new tiller DM (g/m^2)
      real       N_tiller_independence  ! (INPUT) new tiller N (g/m^2)

*+  Purpose
*       Initiate millet tiller module

*+  Changes
*       101095 jngh specified and programmed
*       220696 jngh changed to post_ construct

*+  Calls
      character  no_spaces*8           ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'millet_tiller_initiate')

*+  Local Variables
cjh      character  string*200            ! output string
      character  tiller_module*20      ! tiller module name
      character  module_name*8         ! this module name
      real       dm_tiller_plant       ! dry matter of tiller (g/plant)
      real       N_tiller_plant        ! N content of tiller (g/plant)
      integer    tiller_no             ! number of tillers developed ()

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (g_tiller_independence.gt.0) then
            ! send out new tiller
 
         tiller_no = int (sum_between (emerg, now, g_tiller_no)) + 1
 
!         write (*,*) 'tiller_no(int(sum(emerg,now))) = ', tiller_no
 
         call get_current_module (module_name)
         write (tiller_module, '(a, i2)') module_name, tiller_no
 
!         write (*,*) 'tiller_module', tiller_module
 
         tiller_module = no_spaces (tiller_module)
 
!         write (*,*) 'no_spaces(tiller_module)', tiller_module
 
         if (lastnb (tiller_module).le.8) then
 
            dm_tiller_plant = divide (dm_tiller_independence
     :                              , g_plants, 0.0)
            N_tiller_plant = divide (N_tiller_independence
     :                             , g_plants, 0.0)
 
cjh            write(string, '(4(a, g16.7e3, a), 2a)' )
cjh     :           'plants = '       , g_plants        , '(plants/m2)'
cjh     :         , ',tiller_wt = '   , dm_tiller_plant , '(g/plant)'
cjh     :         , ',tiller_N = '    , N_tiller_plant  , '(g/plant)'
cjh     :         , ', row_spacing = ', g_row_spacing   , '(m)'
cjh     :         , ', cultivar = '   , g_cultivar
 
cjh            call message_pass_to_module (tiller_module
cjh     :                                  , Mes_initiate_crop
cjh     :                                  , string)
 
            call New_postbox ()
 
            call post_real_var ('plants'
     :                         ,'(plants/m2)'
     :                         ,g_plants)
 
            call post_real_var ('tiller_wt'
     :                        ,'(g/plant)'
     :                        ,dm_tiller_plant)
 
            call post_real_var ('tiller_N'
     :                        ,'(g/plant)'
     :                        ,N_tiller_plant)
 
            call post_real_var ('row_spacing'
     :                        ,'(m)'
     :                        ,g_row_spacing)
 
            call post_char_var ('cultivar'
     :                        ,'()'
     :                        ,g_cultivar)
 
            call message_send_immediate (
     :                              tiller_module
     :                            , Mes_initiate_crop
     :                            , Blank
     :                            )
 
            call Delete_postbox ()
 
         else
            call Fatal_Error (err_internal,
     :                       ' Tiller module name too long - '
     :                       // Tiller_module)
 
         endif
 
      else
         ! no tiller ready for independence
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_transpiration ()
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'error.pub'                         

*+  Purpose
*       Plant transpiration and soil water extraction

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_transpiration')

*- Implementation Section ----------------------------------
c+!!!!!!!!! check order dependency of deltas
      call push_routine (my_name)
 
      call millet_root_depth (g_dlt_root_depth)   ! increase in root depth
      call millet_root_depth_init (g_root_depth)  ! initial root depth
 
            ! WATER UPTAKE
      call millet_check_sw ()
      call millet_sw_avail_pot (g_sw_avail_pot) ! potential extractable sw (dul-l
      call millet_sw_avail (g_sw_avail)       ! actual extractable sw (sw-ll)
      call millet_sw_demand (g_sw_demand)
      call millet_sw_supply (g_sw_supply)
      call millet_sw_uptake (g_dlt_sw_dep)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_root_depth (dlt_root_depth)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_root_depth        ! (OUTPUT) increase in root depth (mm)

*+  Purpose
*       Return the increase in root depth (mm)

*+  Notes
*         there is a discrepency when the root crosses into another
*         layer. - cr380

*+  Changes
*      250894 jngh specified and programmed

*+  Calls
      real       millet_sw_avail_fac    ! function
cglh      real       sum_between           ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_root_depth')

*+  Local Variables
      integer    current_phase         ! current phase number
cglh      real       days_after_sowing     ! elapsed time since sowing (days)
cglh      real       days_after_flowering  ! elapsed time since flowering (days)
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      real       root_depth_max        ! maximum depth to which roots can
                                       ! go (mm)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
cglh      days_after_sowing = sum_between (sowing, now, g_days_tot)
cglh      days_after_flowering = sum_between (flowering, now, g_days_tot)
 
      if (on_day_of (germ, g_current_stage, g_days_tot)) then
 
             ! initialise root depth has been done elsewhere,
             ! so no root growth today ????
 
         dlt_root_depth = 0.0
 
      elseif (stage_is_between (emerg, start_grain_fill
     :                        , g_current_stage)) then
cglh      elseif (days_after_sowing.gt.c_root_depth_lag_start
cglh     :  .and. days_after_flowering.lt.c_root_depth_lag_end) then
            ! we have root growth (in a vegetative phase)
 
            ! this equation allows soil water in the deepest
            ! layer in which roots are growing
            ! to affect the daily increase in rooting depth.
 
         deepest_layer = find_layer_no (g_root_depth, g_dlayer
     :                                , max_layer)
         current_phase = int (g_current_stage)
         dlt_root_depth  = c_root_depth_rate(current_phase)
     :                   * millet_sw_avail_fac (deepest_layer)
 
            ! constrain it by the maximum
            ! depth that roots are allowed to grow.
 
         root_depth_max = sum_real_array (g_dlayer, g_num_layers)
         dlt_root_depth = u_bound (dlt_root_depth
     :                           , root_depth_max - g_root_depth)
 
      else  ! we have no root growth
 
         dlt_root_depth = 0.0
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      real function millet_sw_avail_fac (layer)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    layer                 ! (INPUT) soil profile layer number

*+  Purpose
*      Get the soil water availability factor in a layer.  For a layer,
*      it is 1.0 unless the plant-extractable soil water declines
*      below a fraction of plant-extractable soil water capacity for
*      that layer.

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_sw_avail_fac')

*+  Local Variables
      real       pesw                  ! plant extractable soil-water (mm/mm)
      real       pesw_capacity         ! plant extractable soil-water capacity
                                       ! (mm/mm)
      real       sw_avail_ratio        ! soil water availability ratio (0-1)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      pesw = g_sw_dep(layer) - p_ll_dep(layer)
      pesw_capacity = g_dul_dep(layer) - p_ll_dep(layer)
 
      sw_avail_ratio = divide (pesw, pesw_capacity, 10.0)
      millet_sw_avail_fac = linear_interp_real (sw_avail_ratio
     :                           , c_x_sw_ratio, c_y_sw_fac_root
     :                           , c_num_sw_ratio)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_sw_avail_pot (sw_avail_pot)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       sw_avail_pot(*)       ! (OUTPUT) crop water potential uptake
                                       ! for each full layer (mm)

*+  Purpose
*       Return potential water uptake from each layer in the soil profile
*       by the crop (mm water) from a fully wet profile

*+  Notes
*       see cr474 for limitations and potential problems.

*+  Changes
*       010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_sw_avail_pot')

*+  Local Variables
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      integer    layer                 ! soil profile layer number

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
            ! get potential uptake
 
      call fill_real_array (sw_avail_pot, 0.0, max_layer)
 
      deepest_layer = find_layer_no (g_root_depth, g_dlayer, max_layer)
      do 1000 layer = 1, deepest_layer
         sw_avail_pot(layer) = g_dul_dep(layer) - p_ll_dep(layer)
1000  continue
 
            ! correct bottom layer for actual root penetration
      sw_avail_pot(deepest_layer) = sw_avail_pot(deepest_layer)
     :                            * root_proportion
     :                            (deepest_layer
     :                           , g_dlayer
     :                           , g_root_depth)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_sw_avail (sw_avail)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       sw_avail(*)           ! (OUTPUT) crop water potential uptake
                                       ! for each full layer (mm)

*+  Purpose
*       Return actual water available for extraction from each layer in the
*       soil profile by the crop (mm water)

*+  Notes
*       see cr474 for limitations and potential problems.

*+  Changes
*       010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_sw_avail')

*+  Local Variables
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      integer    layer                 ! soil profile layer number

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
            ! get potential uptake
 
      call fill_real_array (sw_avail, 0.0, max_layer)
 
      deepest_layer = find_layer_no (g_root_depth, g_dlayer, max_layer)
      do 1000 layer = 1, deepest_layer
         sw_avail(layer) = g_sw_dep(layer) - p_ll_dep(layer)
         sw_avail(layer) = l_bound (sw_avail(layer), 0.0)
1000  continue
 
            ! correct bottom layer for actual root penetration
      sw_avail(deepest_layer) = sw_avail(deepest_layer)
     :                        * root_proportion
     :                         (deepest_layer, g_dlayer, g_root_depth)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_sw_demand (sw_demand)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       sw_demand             ! (OUTPUT) crop water demand (mm)

*+  Purpose
*       Return crop water demand from soil by the crop (mm)

*+  Changes
*       010994 jngh specified and programmed

*+  Calls
      real       millet_transp_eff      ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_sw_demand')

*+  Local Variables
      real       dlt_dm_pot            ! potential dry matter production with
                                       ! optimum water and nitrogen and
                                       ! temperature stress conditions (g/m^2)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
            ! get potential transpiration from potential
            ! carbohydrate production and transpiration efficiency
 
      call millet_dm_potential (dlt_dm_pot)
 
      sw_demand = divide (dlt_dm_pot, millet_transp_eff (), 0.0)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_dm_potential (dlt_dm_pot)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_dm_pot            ! (OUTPUT) potential dry matter
                                       ! (carbohydrate) production (g/m^2)

*+  Purpose
*       Potential biomass (carbohydrate) production from
*       photosynthesis (g/m^2)

*+  Changes
*       090994 jngh specified and programmed

*+  Calls
      real       millet_rue_reduction   ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_dm_potential')

*+  Local Variables
      integer    current_phase         ! current phase number
      real       rue                   ! radiation use efficiency under
                                       ! no stress (g biomass/mj)
      real       radn_int              ! radn intercepted by leaves (mj/m^2)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      current_phase = int (g_current_stage)
      rue = c_rue(current_phase) * millet_rue_reduction ()
 
         ! potential dry matter production with temperature
         ! and N content stresses is calculated.
         ! This is g of dry biomass produced per MJ of intercepted
         ! radiation under stressed conditions.
 
      call millet_radn_int (radn_int)
      dlt_dm_pot = rue * radn_int
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      real function millet_rue_reduction ()
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Purpose
*       Effect of non-optimal N and Temp conditions on RUE

*+  Changes
*       090994 jngh specified and programmed

*+  Calls
      real       millet_nfact           ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_rue_reduction')

*+  Local Variables
      real       temp_stress_photo     ! photosynthetic reduction factor for
                                       ! temperature stress (0-1)
      real       ave_temp              ! mean temperature for the day (oC)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
         ! now get the temperature stress factor that reduces
         ! photosynthesis (0-1)
 
      ave_temp = (g_maxt + g_mint) /2.0
 
      temp_stress_photo = linear_interp_real (ave_temp
     :                          , c_x_ave_temp, c_y_stress_photo
     :                          , c_num_ave_temp)
      temp_stress_photo = bound (temp_stress_photo, 0.0, 1.0)
 
      millet_rue_reduction = min (temp_stress_photo
     :                      , millet_nfact (photo))
 
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_radn_int (radn_int)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       radn_int              ! (OUTPUT) radiation intercepted
                                       ! by leaves (mj/m^2)

*+  Purpose
*       Radiation intercepted by leaves (mj/m^2)

*+  Changes
*     010994 jngh specified and programmed
*     090695 psc  change extinction coef with row spacing

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_radn_int')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (reals_are_equal (g_fr_intc_radn, 0.0)) then
            ! we need to calculate our own interception
 
         radn_int = g_cover_green * g_radn
 
      else
            ! interception has already been calculated for us
         radn_int = g_fr_intc_radn * g_radn
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      real function millet_transp_eff ()
*     ===========================================================
      implicit none
      include   'convert.inc'          ! g2mm, mb2kpa
      include   'millet.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Purpose
*       Calculate today's transpiration efficiency from min and max
*       temperatures and converting mm water to g dry matter
*       (g dm/m^2/mm water)

*+  Assumptions
*       the temperatures are > -237.3 oC for the svp function.

*+  Notes
*       Average saturation vapour pressure for ambient temperature
*       during transpiration is calculated as part-way between that
*       for minimum temperature and that for the maximum temperature.
*       Tanner & Sinclair (1983) used .75 and .67 of the distance as
*       representative of the positive net radiation (rn).  Daily SVP
*       should be integrated from about 0900 hours to evening when Radn
*       becomes negetive.

*+  Changes
*       240894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_transp_eff')

*+  Local Variables
      real       svp                   ! function to get saturation vapour
                                       ! pressure for a given temperature
                                       ! in oC (kpa)
      real       temp_arg              ! dummy temperature for function (oC)
      real       vpd                   ! vapour pressure deficit (kpa)

*+  Initial Data Values
      ! set up saturation vapour pressure function
*
      svp(temp_arg) = 6.1078
     :              * exp (17.269*temp_arg/ (237.3 + temp_arg))
     :              * mb2kpa

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
            ! get vapour pressure deficit when net radiation is positive.
 
      vpd = c_svp_fract* (svp (g_maxt) - svp (g_mint))
      millet_transp_eff = divide (c_transp_eff_cf, vpd, 0.0) /g2mm
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_sw_supply (sw_supply)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       sw_supply(*)          ! (OUTPUT) potential crop water uptake
                                       ! from each layer (mm) (supply to roots)

*+  Purpose
*       Return potential water uptake from each layer of the soil profile
*       by the crop (mm water). This represents the maximum amount in each
*       layer regardless of lateral root distribution but takes account of
*       root depth in bottom layer.

*+  Notes
*      This code still allows water above dul to be taken - cnh

*+  Changes
*     010994 jngh specified and programmed - adapted from barley

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_sw_supply')

*+  Local Variables
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      integer    layer                 ! soil profile layer number
      real       sw_avail              ! water available (mm)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
            ! get potential uptake
 
      call fill_real_array (sw_supply, 0.0, max_layer)
 
      deepest_layer = find_layer_no (g_root_depth, g_dlayer, max_layer)
      do 1000 layer = 1, deepest_layer
         sw_avail = (g_sw_dep(layer) - p_ll_dep(layer))
         sw_supply(layer) = sw_avail * p_kl(layer)
         sw_supply(layer) = l_bound (sw_supply(layer), 0.0)
 
1000  continue
 
            ! now adjust bottom layer for depth of root
      sw_supply(deepest_layer) = sw_supply(deepest_layer)
     :                         * root_proportion
     :                          (deepest_layer, g_dlayer, g_root_depth)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_sw_uptake (dlt_sw_dep)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_sw_dep (*)        ! (OUTPUT) root water uptake (mm)

*+  Purpose
*       Returns actual water uptake from each layer of the soil
*       profile by the crop (mm).

*+  Changes
*       010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_sw_uptake')

*+  Local Variables
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      integer    layer                 ! layer number of profile ()
      real       sw_supply_sum         ! total potential over profile (mm)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
 
            ! find total root water potential uptake as sum of all layers
 
      deepest_layer = find_layer_no (g_root_depth, g_dlayer, max_layer)
      sw_supply_sum = sum_real_array (g_sw_supply, deepest_layer)
 
      if (sw_supply_sum.le.0.0 .or. g_sw_demand.le.0.0) then
            ! we have no uptake - there is no demand or potential
 
         call fill_real_array (dlt_sw_dep, 0.0, max_layer)
 
      else
               ! get actual uptake
 
         call fill_real_array (dlt_sw_dep, 0.0, max_layer)
 
         if (g_sw_demand.lt.sw_supply_sum) then
 
               ! demand is less than what roots could take up.
               ! water is non-limiting.
               ! distribute demand proportionately in all layers.
 
            do 1000 layer = 1, deepest_layer
               dlt_sw_dep(layer) = - divide (g_sw_supply(layer)
     :                                    , sw_supply_sum, 0.0)
     :                            * g_sw_demand
 
1000        continue
 
         else
                ! water is limiting - not enough to meet demand so take
                ! what is available (potential)
 
            do 1100 layer = 1, deepest_layer
               dlt_sw_dep(layer) = - g_sw_supply(layer)
 
1100        continue
 
         endif
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_check_sw ()
*     ===========================================================
      implicit none
      include   'const.inc'            ! err_internal
      include   'millet.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Purpose
*       Check validity of soil water parameters for all soil profile layers.

*+  Notes
*           Reports an error if
*           - ll_dep and dul_dep are not in ascending order
*           - ll is below c_minsw
*           - sw < c_minsw

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_check_sw')

*+  Local Variables
      real       dul                   ! drained upper limit water content
                                       ! of layer (mm water/mm soil)
      character  err_messg*200         ! error message
      integer    layer                 ! layer number
      real       ll                    ! lower limit water content
                                       ! of layer (mm water/mm soil)
      real       sw                    ! soil water content of layer l
                                       ! (mm water/mm soil)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      do 2000 layer = 1, g_num_layers
 
         sw = divide (g_sw_dep(layer), g_dlayer(layer), 0.0)
         dul = divide (g_dul_dep(layer), g_dlayer(layer), 0.0)
         ll = divide (p_ll_dep(layer), g_dlayer(layer), 0.0)
 
         if (ll.lt.c_minsw) then
            write (err_messg, '(a,f8.2,a,i3,2a,f8.2)')
     :           ' lower limit of ', ll
     :          ,' in layer ', layer
     :          , new_line
     :          ,'         is below acceptable value of ', c_minsw
            call warning_error (err_internal, err_messg)
         else
         endif
 
         if (dul.le.ll) then
            write (err_messg, '(a,f8.2,a,i3,2a,f8.2)')
     :            ' Drained upper limit of ',dul
     :           ,' in layer ', layer
     :           ,new_line
     :           ,'         is at or below lower limit of ', ll
            call warning_error (err_internal, err_messg)
         else
         endif
 
         if (sw.lt.c_minsw) then
            write (err_messg, '(a,f8.2,a,i3,2a,f8.2)')
     :            ' Soil water of ', sw
     :           ,' in layer ', layer
     :           ,new_line
     :           ,'         is below acceptable value of ', c_minsw
            call warning_error (err_internal, err_messg)
 
         else
         endif
2000  continue
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_root_depth_init (root_depth)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'science.pub'                       
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       root_depth            ! (OUTPUT) initial root depth (mm)

*+  Purpose
*       Return the initial root depth (mm)

*+  Changes
*      250996 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_root_depth_init')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (on_day_of (germ, g_current_stage, g_days_tot)) then
 
             ! initialise root depth
             ! this version (cmsat) does not take account of sowing depth.
 
         root_depth = c_initial_root_depth
 
      else  ! we have no initial root
 
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_set_my_class (My_Class)
*     ===========================================================
      implicit none
      include   'millet.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      character my_class*(*)

*+  Purpose
*       Set class type for this module.

*+  Changes
*     280598 igh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_set_my_class')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      g_stem_class = my_class
 
      call pop_routine (my_name)
      return
      end



