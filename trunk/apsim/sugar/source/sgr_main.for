*     ===========================================================
      character*(*) function sugar_version ()
*     ===========================================================
      implicit none
      include 'error.pub'                         

*+  Purpose
*       Return version number of sugar module

*+  Changes
*       060495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_version')
*
      character  version_number*(*)    ! version number of module
      parameter (version_number = 'V0.3 040497')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      sugar_version = version_number
 
      call pop_routine (my_name)
      return
      end



*     ================================================================
      subroutine APSIM_sugar (action, data_string)
*     ================================================================
      implicit none
      dll_export apsim_sugar
      include   'sugar.inc'            !
      include   'const.inc'            ! mes_presence, mes_init, mes_process
      include 'crp_comm.pub'                      
      include 'string.pub'                        
      include 'engine.pub'                        
      include 'error.pub'                         

*+  Sub-Program Arguments
      character  action*(*)            ! (INPUT) Message action to perform
      character  data_string*(*)       ! (INPUT) Message data

*+  Purpose
*     This is the highest level routine for the APSIM sugar module.
*     This routines handles all the input and output communications to
*     other modules in APSIM and calls model process routines for each
*     timestep when required.

*+  Changes
*      250894 jngh specified and programmed
*      050996 nih  added graze action
*

*+  Calls
      character  sugar_version*15      ! function

*+  Constant Values
      character  my_name*(*)           ! name of this procedure
      parameter (my_name='sugar')

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
     :              // sugar_version ()
 
      elseif (action.eq.mes_init) then
            ! Get constants
         call sugar_init ()
 
      elseif (action.eq.mes_set_variable) then
            ! respond to request to reset variable values - from modules
         call sugar_set_my_variable (data_string)
 
      elseif (action.eq.mes_get_variable) then
            ! respond to request for variable values - from modules
         call sugar_send_my_variable (Data_string)
 
      elseif (action.eq.mes_prepare) then
         call sugar_prepare ()
 
      elseif (action.eq.mes_sow) then
         if (crop_my_type (c_crop_type)) then
               ! start crop and do  more initialisations
            call sugar_start_crop ()
         else
            ! not my type!
            call message_unused ()
         endif
 
      elseif (action.eq.mes_process) then
         if (g_crop_status.ne.crop_out) then
               ! do crop processes
            call sugar_process ()
         else
            ! crop not in
            call sugar_zero_variables ()
         endif
      elseif (action.eq.mes_harvest) then
         if (crop_my_type (c_crop_type)) then
               ! harvest crop - turn into residue
            call sugar_harvest ()
         else
            ! not my type!
            call message_unused ()
         endif
 
      elseif (action.eq.mes_end_crop) then
         if (crop_my_type (c_crop_type)) then
               ! end crop - turn into residue
            call sugar_end_crop ()
         else
            ! not my type!
            call message_unused ()
         endif
 
      elseif (action.eq.mes_kill_crop) then
         if (crop_my_type (c_crop_type)) then
               ! kill crop - die
            call sugar_kill_crop
     :               (
     :                G_crop_status
     :              , G_day_of_year
     :              , G_dm_dead
     :              , G_dm_green
     :              , G_dm_senesced
     :              , G_year
     :               )
         else
            ! not my type!
            call message_unused ()
         endif
 
      elseif (action.eq.MES_Inter_timestep) then
         call sugar_zero_daily_variables ()
 
      elseif (action.eq.'graze') then
         call sugar_graze ()
      elseif (action.eq.'hill_up') then
         call sugar_hill_up ()
      elseif (action.eq.'lodge') then
         call sugar_lodge ()
 
      else
               ! don't use message
         call message_unused ()
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_process ()
*     ===========================================================
      implicit none
      include   'sugar.inc'
      include 'error.pub'                         

*+  Purpose
*       Simulate crop processes.  These include biomass production,
*       phenological stages, plant component development,
*       water uptake and nitrogen uptake, plant senescense and so on.
*       This routine is called once per day during the process stage.

*+  Changes
*      060495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_process')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      ! request and receive variables from owner-modules
      call sugar_get_other_variables ()
 
      call sugar_root_depth(1)
      call sugar_root_Depth_init(1) ! after because it sets the delta
      call sugar_water_supply(1)
      call sugar_water_uptake (1)
      call sugar_water_stress_expansion (1)
      call sugar_water_stress_stalk (1)
      call sugar_water_stress_pheno (1)
 
      if (g_crop_status.eq.crop_alive) then
         call sugar_min_sstem_sucrose(1)
         call sugar_phenology_init (1)
         call sugar_phenology (1)
         call sugar_height (1)
 
         call sugar_leaf_no_init (1)
         call sugar_leaf_no_pot (1)
         call sugar_leaf_area_init (1)
         call sugar_leaf_area_potential (1)
 
         call sugar_bio_water (1)
         call sugar_water_log (1)
         call sugar_bio_RUE(1)
         call sugar_bio_actual (1)
         call sugar_leaf_area_stressed (1)
         call sugar_bio_partition (1)
         call sugar_bio_retrans (1)
         call sugar_leaf_actual (1)
         call sugar_root_dist (1)
 
         !!!! LEAF AREA SEN !!!!
 
         call sugar_leaf_death (1)
         call sugar_leaf_area_sen(1)
 
         call sugar_sen_bio (1)
         call sugar_sen_nit (1)
         call sugar_sen_root_length (1)
 
         call sugar_nit_retrans (1)
         call sugar_nit_Demand(1)
         call sugar_nit_supply (1)
         call sugar_nit_init (1)
         call sugar_nit_uptake (1)
         call sugar_nit_partition (1)
 
 
         call sugar_water_content_cane (1)
c         call sugar_water_content()
c         call sugar_plant_death ()
         call sugar_plant_death (1)
         call sugar_realloc (1)
      else
      endif
 
c      call sugar_detachment ()
      call sugar_detachment (1)
 
      call sugar_cleanup()
c      call sugar_update ()
c      call sugar_crop_totals ()
c      call sugar_event ()
 
cjh      call sugar_water_stress (1)
cjh      call sugar_nit_stress (1)
      call sugar_water_stress_expansion (1)
      call sugar_water_stress_photo (1)
      call sugar_water_stress_pheno (1)
 
      call sugar_nit_stress_photo (1)
      call sugar_nit_stress_expansion (1)
      call sugar_nit_stress_stalk (1)
 
      ! send changes to owner-modules
      call sugar_set_other_variables ()
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_harvest ()
*     ===========================================================
      implicit none
      include   'const.inc'            ! new_line, lu_scr_sum, blank,
      include   'convert.inc'          ! gm2kg, sm2ha, sm2smm
      include   'sugar.inc'
      include 'string.pub'                        
      include 'data.pub'                          
      include 'crp_comm.pub'                      
      include 'write.pub'                         
      include 'error.pub'                         

*+  Purpose
*       Report occurence of harvest and the current status of specific
*       variables.

*+  Changes
*     070495 nih taken from template

*+  Calls
                                       ! lu_scr_sum
*
      character  string_concat*50      ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_harvest')

*+  Local Variables
      real       biomass_dead          ! above ground dead plant wt (kg/ha)
      real       biomass_green         ! above ground green plant wt (kg/ha)
      real       biomass_senesced      ! above ground senesced plant wt (kg/ha)
      character  cultivar_ratoon*30    !
      real       dm                    ! above ground total dry matter (kg/ha)
      real       leaf_no               ! total leaf number
      real       N_dead                ! above ground dead plant N (kg/ha)
      real       N_green               ! above ground green plant N (kg/ha)
      real       N_senesced            ! above ground senesced plant N (kg/ha)
      real       N_total               ! total gross nitrogen content (kg/ha)
      integer    phase                 ! phenological phase number
      real       si1                   ! mean water stress type 1
      real       si2                   ! mean water stress type 2
      real       si4                   ! mean nitrogen stress type 1
      character  string*400            ! message
*
      real       dm_root
      real       N_root
      real       dm_residue
      real       N_residue
*
      integer layer
      real hold_ratoon_no
      real hold_dm_root
      real hold_n_root
      real hold_num_layers
      real hold_root_depth
      real hold_root_length(max_layer)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
          ! crop harvested. Report status
 
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
 
      leaf_no = sum_between (emerg, now, g_leaf_no)
 
      N_green = (sum_real_array (g_N_green, max_part)
     :        - g_N_green(root))
     :        * gm2kg / sm2ha
 
      N_senesced = (sum_real_array (g_N_senesced, max_part)
     :           - g_N_senesced(root))
     :           * gm2kg / sm2ha
 
      N_dead = (sum_real_array (g_N_dead, max_part)
     :       - g_N_dead(root))
     :       * gm2kg / sm2ha
 
      N_total = N_green + N_senesced + N_dead
 
      call write_string (lu_scr_sum, new_line//new_line)
 
      write (string, '(a,i4)')
     :            ' flowering day  = ',g_isdate
      call write_string (lu_scr_sum, string)
 
      write (string, '(a,f6.3)')
     :            ' maximum lai =', g_lai_max
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
     :            ' total N content (kg/ha) =', N_total
     :          , ' senesced N content (kg/ha) =', N_senesced
 
      call write_string (lu_scr_sum, string)
 
      write (string, '(a,f10.2,t40,a,f10.2)')
     :            ' green N content (kg/ha) =', N_green
     :          , ' dead N content (kg/ha) =', N_dead
      call write_string (lu_scr_sum, string)
 
      do 2000 phase = emerg_to_begcane, flowering_to_crop_end
         si1 = divide (g_cswd_photo(phase)
     :               , g_days_tot(phase), 0.0)
         si2 = divide (g_cswd_expansion(phase)
     :               , g_days_tot(phase), 0.0)
         si4 = divide (g_cnd_photo(phase)
     :               , g_days_tot(phase), 0.0)
 
         call write_string (lu_scr_sum, new_line//new_line)
 
         write (string,'(2a)')
     :         ' stress indices for ', c_stage_names(phase)
         call write_string (lu_scr_sum, string)
 
         write (string,'(2(a, g16.7e2))')
     :         ' water stress 1 =', si1
     :         , '   nitrogen stress 1 =', si4
         call write_string (lu_scr_sum, string)
 
         write (string,'(a, g16.7e2)')
     :         ' water stress 2 =', si2
         call write_string (lu_scr_sum, string)
2000  continue
 
      ! the following is a copy/adaption of sugar_end_crop
 
      if (g_crop_status.ne.crop_out) then
 
                ! report
 
             ! now do post harvest processes
 
         dm_root = g_dm_green(root)* c_root_die_back_fr
     :           + g_dm_dead(root)
     :           + g_dm_senesced(root)
 
         N_root  = g_N_green(root)* c_root_die_back_fr
     :           + g_N_dead(root)
     :           + g_N_senesced(root)
 
 
             ! put stover into surface residue
 
         dm_residue = (sum_real_array (g_dm_green, max_part)
     :              - g_dm_green(root) - g_dm_green(sstem)
     :              - g_dm_green(sucrose))
 
     :              + (sum_real_array (g_dm_senesced, max_part)
     :              - g_dm_senesced(root))
 
     :              + (sum_real_array (g_dm_dead, max_part)
     :              - g_dm_dead(root))
 
         N_residue = (sum_real_array (g_N_green, max_part)
     :             - g_N_green(root) - g_N_green (sstem)
     :             - g_N_green(sucrose))
 
     :             + (sum_real_array (g_N_senesced, max_part)
     :             - g_N_senesced(root))
 
     :             + (sum_real_array (g_N_dead, max_part)
     :             - g_N_dead(root))
 
 
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
 
      call crop_root_incorp (dm_root
     :                      ,N_root
     :                      ,g_dlayer
     :                      ,g_root_length
     :                      ,g_root_depth
     :                      ,c_crop_type
     :                      ,max_layer
     :                      )
 
 
         call crop_top_residue (c_crop_type, dm_residue, N_residue)
 
         hold_ratoon_no = g_ratoon_no
         hold_dm_root   = g_dm_green (root)*(1.0 - c_root_die_back_fr)
         hold_n_root    = g_N_green (root)*(1.0 - c_root_die_back_fr)
         hold_num_layers= g_num_layers
         hold_root_depth= g_root_depth
         do 101 layer=1,max_layer
            hold_root_length(layer) = g_root_length(layer)
     :                                *(1.0 - c_root_die_back_fr)
  101    continue
 
 
         call sugar_zero_globals ()
         call sugar_zero_daily_variables ()
 
         g_current_stage   = real (sprouting)
         g_ratoon_no       = hold_ratoon_no +1
         g_dm_green (root) = hold_dm_root
         g_N_green (root)  = hold_n_root
         g_num_layers      = hold_num_layers
         g_root_depth      = hold_root_depth
         g_plants          = g_initial_plant_density
 
         do 102 layer=1,max_layer
            g_root_length(layer) = hold_root_length(layer)
  102    continue
 
         ! now update constants if need be
         If (g_ratoon_no .eq. 1) then
 
            call sugar_read_crop_constants ('ratoon_crop')
 
            cultivar_ratoon = string_concat(g_crop_cultivar
     :                                     ,'_ratoon')
            call sugar_read_cultivar_params (cultivar_ratoon)
 
         else
            ! only need to update constants when we move from a plant
            ! crop to a ratoon crop.
         endif
      else
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_zero_variables ()
*     ===========================================================
      implicit none
      include   'sugar.inc'
      include 'error.pub'                         

*+  Purpose
*       Zero crop variables & arrays

*+  Changes
*     060495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_zero_variables')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
          ! zero pools etc.
      call sugar_zero_globals ()
      call sugar_zero_daily_variables ()
      call sugar_zero_parameters ()
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_zero_daily_variables ()
*     ===========================================================
      implicit none
      include   'sugar.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Purpose
*       Zero crop daily variables & arrays

*+  Changes
*     060495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_zero_daily_variables')

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
 
 
      g_dlt_tlai_dead_detached = 0.0
      g_dlt_slai_detached = 0.0
      g_dlt_canopy_height = 0.0
      g_dlt_dm = 0.0
      g_partition_xs = 0.0
      g_dlt_leaf_no = 0.0
      g_dlt_node_no = 0.0
      g_dlt_node_no_dead = 0.0
      g_dlt_plants = 0.0
      g_dlt_root_depth = 0.0
      g_dlt_slai = 0.0
      g_dlt_stage = 0.0
      g_dlt_lai = 0.0
      g_dlt_tt = 0.0
 
      g_sw_demand = 0.0
      g_dm_graze = 0.0
      g_n_graze = 0.0
 
      g_dlt_min_sstem_sucrose = 0.0
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_init ()
*     ===========================================================
      implicit none
      include   'sugar.inc'
      include 'write.pub'                         
      include 'error.pub'                         

*+  Purpose
*       Crop initialisation

*+  Changes
*     060495 nih taken from template

*+  Calls
      character  sugar_version*20      ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_init')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      call sugar_zero_variables ()
cnh      call report_date_and_event (g_day_of_year,g_year,
cnh     :                 ' Initialising, Version : '
cnh     :                  // sugar_version ())
      call report_event (' Initialising, Version : '
     :                     // sugar_version ())
 
           ! initialize crop variables
 
      call sugar_read_constants ()
 
      g_current_stage = real (crop_end)
      g_crop_status = crop_out
 
      call sugar_get_other_variables ()
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_start_crop ()
*     ===========================================================
      implicit none
      include   'const.inc'            ! lu_scr_sum, blank
      include   'sugar.inc'
      include 'string.pub'                        
      include 'intrface.pub'                      
      include 'write.pub'                         
      include 'error.pub'                         

*+  Purpose
*       Start crop using parameters specified in passed record

*+  Changes
*     060495 nih taken from template
*     041095 nih changed start of ratton crop from emergence to sprouting
*     060696 nih changed extract routines to collect routine calls
*                removed datastring from argument list

*+  Calls
      character string_concat*50       ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_start_crop')

*+  Local Variables
      character  cultivar*20           ! name of cultivar
      integer    numvals               ! number of values found in array
      character  string*200            ! output string
      character  cultivar_ratoon*30    ! name of cultivar ratoon section

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      ! request and receive variables from owner-modules
      call sugar_get_other_variables ()
 
 
cnh      call report_event ( 'Sowing initiate')
         call report_date_and_event
     :           (g_day_of_year,g_year,'Sowing initiate')
 
 
         call collect_real_var ('plants', '()'
     :                        , g_plants, numvals, 0.0, 100.0)
         g_initial_plant_density = g_plants
 
         call collect_integer_var_optional ('ratoon', '()'
     :                        , g_ratoon_no, numvals, 0, 10)
         if (numvals.eq.0) then
            g_ratoon_no = 0
         else
         endif
 
         call collect_real_var ('sowing_depth', '(mm)'
     :                        , g_sowing_depth, numvals
     :                        , 0.0, 500.0)
 
         call collect_char_var ('cultivar', '()'
     :                        , cultivar, numvals)
 
             ! report
 
         call write_string (lu_scr_sum, new_line//new_line)
 
         string = '                 Crop Sowing Data'
         call write_string (lu_scr_sum, string)
 
         string = '    ------------------------------------------------'
         call write_string (lu_scr_sum, string)
 
         call write_string (lu_scr_sum
     :                    , '    Sowing  Depth Plants Cultivar')
 
         call write_string (lu_scr_sum
     :                    , '    Day no   mm     m^2    Name   ')
 
         string = '    ------------------------------------------------'
         call write_string (lu_scr_sum, string)
 
         write (string, '(3x, i7, 2f7.1, 1x, a10)')
     :                   g_day_of_year, g_sowing_depth
     :                 , g_plants, cultivar
         call write_string (lu_scr_sum, string)
 
         string = '    ------------------------------------------------'
         call write_string (lu_scr_sum, string)
 
                 ! get cultivar parameters
 
         if (g_ratoon_no .eq. 0) then
            call sugar_read_crop_constants ('plant_crop')
            call sugar_read_cultivar_params (cultivar)
         else
            call sugar_read_crop_constants ('ratoon_crop')
            cultivar_ratoon = string_concat(cultivar,'_ratoon')
            call sugar_read_cultivar_params (cultivar_ratoon)
         endif
 
                 ! get root profile parameters
 
         call sugar_read_root_params ()
 
         if (g_ratoon_no.eq.0) then
            g_current_stage = real (sowing)
         else
            g_current_stage = real (sowing)
         endif
 
         g_crop_status = crop_alive
         g_crop_cultivar = cultivar
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_read_cultivar_params (section_name)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'sugar.inc'
      include 'read.pub'                          
      include 'write.pub'                         
      include 'error.pub'                         

*+  Sub-Program Arguments
      character section_name*(*)

*+  Purpose
*       Crop initialisation - reads constants from constants file

*+  Changes
*       25-07-96 - NIH/MJR added sucrose/water stress partitioning factor

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_read_cultivar_params')

*+  Local Variables
      integer    numvals               ! number of values returned

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      call write_string (lu_scr_sum
     :   ,new_line//'    - Reading constants from '//section_name)
 
         !    sugar_leaf_size
 
      call read_real_array (section_name
     :                     , 'leaf_size', max_table, '()'
     :                     , c_leaf_size, c_num_leaf_size
     :                     , 1000.0, 100000.0)
 
      call read_real_array (section_name
     :                     , 'leaf_size_no', max_table, '()'
     :                     , c_leaf_size_no, c_num_leaf_size
     :                     , 0.0, real(max_leaf))
 
      call read_real_var (section_name
     :                    , 'cane_fraction', '()'
     :                    , c_cane_fraction, numvals
     :                    , 0.0, 1.0)
 
      call read_real_array (section_name
     :                    , 'sucrose_fraction_stalk', max_table,'()'
     :                    , c_sucrose_fraction_stalk, numvals
     :                    , 0.0, 1.0)
 
      call read_real_array (section_name
     :                    , 'stress_factor_stalk', max_table,'()'
     :                    , c_stress_factor_stalk
     :                    , c_num_stress_Factor_stalk
     :                    , 0.0, 1.0)
 
      call read_real_var (section_name
     :                    , 'sucrose_delay', '()'
     :                    , c_sucrose_delay, numvals
     :                    , 0.0, 2000.)
 
      call read_real_var (section_name
     :                    , 'min_sstem_sucrose', '(g/m2)'
     :                    , c_min_sstem_sucrose, numvals
     :                    , 0.0, 5000.)
 
      call read_real_var (section_name
     :                    , 'min_sstem_sucrose_redn', '(g/m2)'
     :                    , c_min_sstem_sucrose_redn, numvals
     :                    , 0.0, 5000.)
 
      call read_real_var (section_name
     :                    , 'tt_emerg_to_begcane', '()'
     :                    , p_tt_emerg_to_begcane, numvals
     :                    , 0.0, c_tt_emerg_to_begcane_ub)
 
      call read_real_var (section_name
     :                    , 'tt_begcane_to_flowering', '()'
     :                    , p_tt_begcane_to_flowering, numvals
     :                    , 0.0, c_tt_begcane_to_flowering_ub)
 
      call read_real_var (section_name
     :                    , 'tt_flowering_to_crop_end', '()'
     :                    , p_tt_flowering_to_crop_end, numvals
     :                    , 0.0, c_tt_flowering_to_crop_end_ub)
 
         !    sugar_leaf_death
 
 
      call read_real_var  (section_name
     :                    , 'green_leaf_no', '()'
     :                    , c_green_leaf_no, numvals
     :                    , 0.0, real(max_leaf))
 
         !    sugar_leaf_size
 
      call read_real_array (section_name
     :                     , 'tillerf_leaf_size', max_table, '()'
     :                     , c_tillerf_leaf_size,c_num_tillerf_leaf_size
     :                     , 0.0, 100.0)
 
      call read_real_array (section_name
     :                  , 'tillerf_leaf_size_no', max_table, '()'
     :                  , c_tillerf_leaf_size_no,c_num_tillerf_leaf_size
     :                  , 0.0, real(max_leaf))
 
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_read_root_params ()
*     ===========================================================
      implicit none
      include   'const.inc'            ! new_line, lu_scr_sum, blank,
      include   'sugar.inc'            ! dlayer(max_layer)
      include 'data.pub'                          
      include 'read.pub'                          
      include 'write.pub'                         
      include 'error.pub'                         

*+  Purpose
*       Get root profile parameters

*+  Changes
*       060495 nih taken from template

*+  Calls
                                       ! lu_scr_sum, Err_User

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_read_root_params')
*
      character  section_name*(*)
      parameter (section_name = 'parameters')

*+  Local Variables
      integer    layer                 ! layer number
      real       ll (max_layer)        ! lower limit of plant-extractable
                                       ! soil water for soil layer l
                                       ! (mm water/mm soil)
      integer    num_layers            ! number of layers in profile
      integer    numvals               !
      character  string*200            ! output string
      real       rlv (max_layer)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      call write_string (lu_scr_sum
     :                  ,new_line
     :                  //'   - Reading root profile parameters')
 
 
      call read_char_var   (section_name
     :                     , 'uptake_source'
     :                     , '()'
     :                     , g_uptake_source
     :                     , numvals)
 
      call read_real_array (section_name
     :                     , 'xf', max_layer, '()'
     :                     , p_xf, num_layers
     :                     , 0.0, 1.0)
 
         !       sugar_sw_supply
 
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
 
      call read_real_array (section_name
     :                     , 'rlv', max_layer, '()'
     :                     , rlv, num_layers
     :                     , 0.0, 20.0)
      call fill_real_array (g_root_depth, 0.0, max_layer)
      do 1001 layer = 1, num_layers
         g_root_length(layer) = rlv(layer)*g_dlayer(layer)
1001  continue
 
      if (g_uptake_source.eq.'calc') then
 
          ! report
         call write_string (lu_scr_sum, new_line//new_line)
         call write_string (lu_scr_sum,
     :      'Sugar module is calculating its own soil uptakes')
         call write_string (lu_scr_sum, new_line//new_line)
 
      else if (g_uptake_source .eq. 'apsim') then
 
          ! report
         call write_string (lu_scr_sum, new_line//new_line)
         call write_string (lu_scr_sum,
     :   'Sugar module is using uptakes provided from another module')
         call write_string (lu_scr_sum, new_line//new_line)
 
      else
         ! the user has not specified 'calc' or 'apsim'
         ! so give out an error message
         call fatal_error (ERR_USER, 'Bad value for uptake_source')
      endif
 
         write (string,'(4x, a)') '                Root Profile'
         call write_string (lu_scr_sum, string)
 
         string = '  --------------------------------------------------'
         call write_string (lu_scr_sum, string)
 
         string = '    Layer depth  Kl factor   Lower limit Root Factor'
         call write_string (lu_scr_sum, string)
 
         string = '         (mm)         ()        (mm/mm)     (0-1)'
         call write_string (lu_scr_sum, string)
 
         string = '  --------------------------------------------------'
         call write_string (lu_scr_sum, string)
 
         do 2000 layer = 1, num_layers
            write (string,'(1x, 4f12.3)')
     :            g_dlayer(layer)
     :          , p_kl(layer)
     :          , ll(layer)
     :          , p_xf(layer)
            call write_string (lu_scr_sum, string)
2000     continue
 
         string = '   -------------------------------------------------'
         call write_string (lu_scr_sum, string)
 
         call write_string (lu_scr_sum, new_line//new_line)
 
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_kill_crop
     :               (
     :                G_crop_status
     :              , G_day_of_year
     :              , G_dm_dead
     :              , G_dm_green
     :              , G_dm_senesced
     :              , G_year
     :               )
*     ===========================================================
      implicit none
      include   'convert.inc'          ! gm2kg, sm2ha, mm2cm, cmm2cc
      include   'sugconst.inc'
      include 'data.pub'                          
      include 'write.pub'                         
      include 'error.pub'                         

*+  Sub-Program Arguments
      CHARACTER  G_crop_status   *(*)  ! (INPUT)
      INTEGER    G_day_of_year         ! (INPUT)  day of year
      REAL       G_dm_dead(*)          ! (INPUT)  dry wt of dead plants (g/m^2)
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass) (g/m^2)
      REAL       G_dm_senesced(*)      ! (INPUT)  senesced plant dry wt (g/m^2)
      INTEGER    G_year                ! (INPUT)  year

*+  Purpose
*       Kill crop

*+  Changes
*       070495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_kill_crop')

*+  Local Variables
      real       biomass               ! above ground dm (kg/ha)
      character  string*200            ! output string

*- Implementation Section ----------------------------------
 
c+!!!!!! fix problem with deltas in update when change from alive to dead ?zero
      call push_routine (my_name)
 
      if (g_crop_status.eq.crop_alive) then
         g_crop_status = crop_dead
 
         biomass = (sum_real_array (g_dm_green, max_part)
     :           - g_dm_green(root)) * gm2kg /sm2ha
 
     :           + (sum_real_array (g_dm_senesced, max_part)
     :           - g_dm_senesced(root)) * gm2kg /sm2ha
 
     :           + (sum_real_array (g_dm_dead, max_part)
     :           - g_dm_dead(root)) * gm2kg /sm2ha
 
 
                ! report
 
         write (string, '(3x, a, f7.1, a)')
     :                  ' crop_kill. Standing above-ground dm = '
     :                  , biomass, ' (kg/ha)'
cnh         call report_event (string)
         call report_date_and_event (g_day_of_year,g_year,string)
 
      else
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_end_crop ()
*     ===========================================================
      implicit none
      include   'const.inc'            ! new_line, lu_scr_sum
      include   'convert.inc'          ! gm2kg, sm2ha, mm2cm, cmm2cc
      include   'sugar.inc'
      include 'data.pub'                          
      include 'write.pub'                         
      include 'crp_comm.pub'                      
      include 'error.pub'                         

*+  Purpose
*       End crop

*+  Changes
*       070495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_end_crop')

*+  Local Variables
      real       dm_residue            ! dry matter added to residue (g/m^2)
      real       N_residue             ! nitrogen added to residue (g/m^2)
      real       dm_root               ! dry matter added to soil (g/m^2)
      real       N_root                ! nitrogen added to soil (g/m^2)
      character  string*400            ! output string

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (g_crop_status.ne.crop_out) then
         g_crop_status = crop_out
         g_current_stage = real (crop_end)
 
                ! report
 
             ! now do post harvest processes
 
         dm_root = g_dm_green(root)
     :           + g_dm_dead(root)
     :           + g_dm_senesced(root)
 
         N_root  = g_N_green(root)
     :           + g_N_dead(root)
     :           + g_N_senesced(root)
 
      call crop_root_incorp (dm_root
     :                      ,N_root
     :                      ,g_dlayer
     :                      ,g_root_length
     :                      ,g_root_depth
     :                      ,c_crop_type
     :                      ,max_layer
     :                      )
 
 
             ! put stover into surface residue
 
         dm_residue = (sum_real_array (g_dm_green, max_part)
     :              - g_dm_green(root))
 
     :              + (sum_real_array (g_dm_senesced, max_part)
     :              - g_dm_senesced(root))
 
     :              + (sum_real_array (g_dm_dead, max_part)
     :              - g_dm_dead(root))
 
         N_residue = (sum_real_array (g_N_green, max_part)
     :             - g_N_green(root))
 
     :             + (sum_real_array (g_N_senesced, max_part)
     :             - g_N_senesced(root))
 
     :             + (sum_real_array (g_N_dead, max_part)
     :             - g_N_dead(root))
 
         call crop_top_residue (c_crop_type, dm_residue, N_residue)
 
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



*     ================================================================
      subroutine sugar_get_other_variables ()
*     ================================================================
      implicit none
      include   'const.inc'
      include   'convert.inc'
      include   'sugar.inc'
      include 'data.pub'                          
      include 'engine.pub'                        
      include 'intrface.pub'                      
      include 'error.pub'                         

*+  Purpose
*      Get the values of variables/arrays from other modules.

*+  Changes
*     060495 nih taken from template
*     210896 nih added module name as suffice to intercepted radiation
*     020998 sb Relaced c_year_lb and c_year_ub with min_year and max_year.

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_get_other_variables')

*+  Local Variables
      integer    layer                 ! layer number
      integer    numvals               ! number of values put into array
      character  mod_name*12           ! module name
      real       dlayer(max_layer)     ! soil layer depths (mm)
      real       NO3(max_layer)        ! soil NO3 content (kg/ha)
      real       NO3_min(max_layer)    ! soil NO3 minimum (kg/ha)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
 
      ! INPUT module
      ! ------------
      call get_integer_var (unknown_module, 'day', '()'
     :                                    , g_day_of_year, numvals
     :                                    , 1, 366)
      call get_integer_var (unknown_module, 'year', '()'
     :                                    , g_year, numvals
     :                                    , min_year, max_year)
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
 
      ! Canopy Module
      ! -------------
      call get_current_module (mod_name)
      call get_real_var_optional (unknown_module
     :                           , 'fr_intc_radn_'//mod_name
     :                           , '()'
     :                           , g_fr_intc_radn
     :                           , numvals
     :                           , 0.0
     :                           , 1.0)
 
      ! Soil Water module
      ! -----------------
      call get_real_array (unknown_module, 'dlayer', max_layer
     :                                    , '(mm)'
     :                                    , dlayer, numvals
     :                                    , c_dlayer_lb, c_dlayer_ub)
 
      if (g_num_layers.eq.0) then
            ! we assume dlayer hasn't been initialised yet.
cnh why do we keep incrementing dlayer??????????
cnh         call add_real_array (dlayer, g_dlayer, numvals)
         do 999 layer = 1,numvals
            g_dlayer(layer) = dlayer(layer)
  999    continue
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
      call get_real_array (unknown_module, 'sat_dep', max_layer
     :                                    , '(mm)'
     :                                    , g_sat_dep, numvals
     :                                    , c_sw_dep_lb, c_sw_dep_ub)
      call get_real_array (unknown_module, 'll15_dep', max_layer
     :                                    , '(mm)'
     :                                    , g_ll15_dep, numvals
     :                                    , c_sw_dep_lb, c_sw_dep_ub)
 
      ! soil nitrogen module
      ! --------------------
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
 
c      call get_real_array (unknown_module, 'st', max_layer
c     :                                    , '(oC)'
c     :                                    , g_st, numvals
c     :                                    , -10., 80.)
 
      call pop_routine (my_name)
      return
      end



*     ================================================================
      subroutine sugar_set_other_variables ()
*     ================================================================
      implicit none
      include   'const.inc'
      include   'convert.inc'
      include   'sugar.inc'
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
*     070495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_set_other_variables')

*+  Local Variables
      real       dlt_NO3(max_layer)    ! soil NO3 change (kg/ha)
      integer    layer                 ! soil layer no.
      integer    num_layers            ! number of layers

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
c      call sugar_update_other_variables ()
 
      if (g_uptake_source.eq.'calc') then
         num_layers = count_of_real_vals (g_dlayer, max_layer)
 
         do 1000 layer = 1, num_layers
            dlt_NO3(layer) = g_dlt_NO3gsm(layer) * gm2kg /sm2ha
1000     continue
 
         call new_postbox()
         call post_real_array ('dlt_no3', '(kg/ha)'
     :                    , dlt_NO3, num_layers)
         call message_send_immediate (unknown_module
     :                               ,MES_set_variable
     :                               ,'dlt_no3')
         call delete_postbox()
 
         call new_postbox()
         call post_real_array ('dlt_sw_dep', '(mm)'
     :                    , g_dlt_sw_dep, num_layers)
         call message_send_immediate (unknown_module
     :                               ,MES_set_variable
     :                               ,'dlt_sw_dep')
         call delete_postbox()
 
      else
         ! assume that the module that calculated uptake has also
         ! updated these pools.
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===============================================================
      subroutine sugar_set_my_variable (Variable_name)
*     ===============================================================
      implicit none
      include 'const.inc'
      include 'sugar.inc'
      include 'engine.pub'                        
      include 'intrface.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      character  Variable_name*(*)     ! (INPUT) Variable name to search for

*+  Purpose
*      Set a variable in this module as requested by another.

*+  Changes
*      060495 nih - taken from template
*      060696 nih - changed respond2set routines to collect routines

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_set_my_variable')

*+  Local Variables
      integer numvals

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (variable_name .eq. 'plants') then
 
         call collect_real_var (variable_name
     :                         ,'(m-2)'
     :                         ,g_plants
     :                         ,numvals
     :                         ,0.0
     :                         ,1000.)
 
        if (g_current_stage.gt.emerg) then
           call warning_error (ERR_User,
     :            'You have updated plant number after emergence')
        else
        endif
 
      else
            ! Don't know this variable name
            call message_unused ()
      endif
 
      call pop_routine (my_name)
      return
      end



*     ================================================================
      subroutine sugar_send_my_variable (variable_name)
*     ================================================================
      implicit none
      include   'convert.inc'          ! gm2kg, sm2ha, mm2cm, cmm2cc
      include   'sugar.inc'
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
*      060495 nih - taken from template

*+  Calls
      real       sugar_profile_fasw    ! function
cbak

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_send_my_variable')

*+  Local Variables
      real       act_N_up              ! cumulative total N uptake by plant
                                       ! (kg/ha)
      real       biomass               ! above ground biomass (alive+dead)
      real       biomass_n             ! N in above ground biomass (alive+dead)
      real       cane_dmf              !
      real       cane_wt               ! cane weight (sstem + sucrose)
      real       ccs                   ! commercial cane sugar(g/g)
      real       cover                 ! crop cover fraction (0-1)
      real       das                   ! days after sowing
      real       radn_int              ! daily radn intercepted (MJ)
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      real       fasw
      real       lai_sum               ! leaf area index of all leaf material
                                       ! live + dead
      real       lai_dead              ! dead leaf area index
                                       ! (m^2 leaf/m^2 soil)
      integer    num_layers            ! number of layers in profile
      integer    stage_no              ! current stage no.
      real       NO3gsm_tot            ! total NO3 in the root profile (g/m^2)
      real       N_demand              ! sum N demand for plant parts (g/m^2)
      real       N_supply              ! N supply for grain (g/m^2)
cbak
      real       conc_n_leaf           ! Current N concentration in leaves
      real       conc_n_cab            ! Current N concentration in cabbage
      real       conc_n_cane           ! Current N concentration in cane
*
*
      real       n_leaf_crit           ! Weight of N in leaves at the critical c
      real       n_leaf_min            ! Weight of N in leaves at the min concen
      real       green_biomass_n       ! Weight of N in green tops (g/m^2)
cmjr
      real       canefw                ! Weight of fresh cane at 30% dry matter
      real       scmstf                ! sucrose conc in fresh millable stalk
      real       scmst                 ! sucrose conc in dry millable stalk
      real       temp
      real       tla
      integer    layer
      real       rwu(max_layer)        ! root water uptake (mm)
      real       rlv(max_layer)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (variable_name .eq. 'crop_status') then
         call respond2get_char_var (variable_name
     :                             , '()'
     :                             , g_crop_status)
 
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
cnh         if (c_crop_type.ne.' ') then
            call respond2get_char_var (variable_name
     :                             , '()'
     :                             , c_crop_type)
cnh         else
cnh             call message_unused ()
cnh         endif
 
      elseif (variable_name .eq. 'plants') then
         call respond2get_real_var (variable_name
     :                             , '(/m2)'
     :                             , g_plants)
 
      elseif (variable_name .eq. 'ratoon_no') then
         call respond2get_integer_var (variable_name
     :                             , '()'
     :                             , g_ratoon_no)
 
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
 
c      elseif (variable_name .eq. 'days_tot') then
c I removed this NIH
 
      elseif (variable_name .eq. 'leaf_no') then
         call respond2get_real_array (variable_name
     :                              , '()'
     :                              , g_leaf_no
     :                              , max_stage)
 
      elseif (variable_name .eq. 'node_no_dead') then
         call respond2get_real_array (variable_name
     :                              , '()'
     :                              , g_node_no_dead
     :                              , max_stage)
 
      elseif (variable_name .eq. 'leaf_area') then
         call respond2get_real_array (variable_name
     :                              , '()'
     :                              , g_leaf_area
     :                              , max_leaf)
 
      elseif (variable_name .eq. 'leaf_dm') then
         call respond2get_real_array (variable_name
     :                              , '()'
     :                              , g_leaf_dm
     :                              , max_leaf)
 
      elseif (variable_name .eq. 'height') then
         call respond2get_real_var (variable_name
     :                             , '(mm)'
     :                             , g_canopy_height)
 
      elseif (variable_name .eq. 'root_depth') then
         call respond2get_real_var (variable_name
     :                             , '(mm)'
     :                             , g_root_depth)
 
      elseif (variable_name .eq. 'cover_green') then
         cover = 1.0 - exp (-c_extinction_coef * g_lai)
 
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , cover)
 
      elseif (variable_name .eq. 'radn_int') then
         cover = 1.0 - exp (-c_extinction_coef * g_lai)
         radn_int = cover * g_radn
         call respond2get_real_var (variable_name
     :                             , '(mj/m2)'
     :                             , radn_int)
 
      elseif (variable_name .eq. 'cover_tot') then
         lai_dead = g_slai + g_tlai_dead
         cover = 1.0
     :         - exp (-c_extinction_coef * g_lai
     :                -c_extinction_coef_dead * lai_dead)
 
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , cover)
 
      elseif (variable_name .eq. 'lai_sum') then
         lai_sum = g_lai + g_slai + g_tlai_dead
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , lai_sum)
 
      elseif (variable_name .eq. 'tlai') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g_lai + g_slai)
 
      elseif (variable_name .eq. 'tla') then
         tla = sum_real_array (g_leaf_area, max_leaf)
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , tla)
 
      elseif (variable_name .eq. 'slai') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g_slai)
 
      elseif (variable_name .eq. 'lai') then
         call respond2get_real_var (variable_name
     :                             , '(m^2/m^2)'
     :                             , g_lai)
 
            ! plant biomass
 
      elseif (variable_name .eq. 'root_wt') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g_dm_green(root))
 
      elseif (variable_name .eq. 'leaf_wt') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g_dm_green(leaf))
 
      elseif (variable_name .eq. 'sstem_wt') then
         ! Add dead pool for lodged crops
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g_dm_green(sstem)
     :                              +g_dm_green(sstem))
 
      elseif (variable_name .eq. 'canefw') then
         ! Add dead pool for lodged crops
         canefw = (g_dm_green(sstem) + g_dm_green(sucrose)
     :          +  g_dm_dead(sstem) + g_dm_dead(sucrose)
     :          + g_plant_wc(sstem))*g2t/sm2ha
         call respond2get_real_var (variable_name
     :                             , '(t/ha)'
     :                             , canefw)
 
      elseif (variable_name .eq. 'ccs') then
         canefw = (g_dm_green(sstem) + g_dm_green(sucrose)
     :          + g_plant_wc(sstem))
         scmstf = divide(g_dm_green(sucrose),canefw,0.0)
         ccs = 1.23*scmstf - 0.029
         ccs = l_bound(ccs,0.0)
         ccs = ccs * 100.          ! convert to %
         call respond2get_real_var (variable_name
     :                             , '(%)'
     :                             , ccs)
 
      elseif (variable_name .eq. 'scmstf') then
         canefw = (g_dm_green(sstem) + g_dm_green(sucrose)
     :          + g_plant_wc(sstem))
         scmstf = divide(g_dm_green(sucrose),canefw,0.0)
         call respond2get_real_var (variable_name
     :                             , '(g/g)'
     :                             , scmstf)
 
      elseif (variable_name .eq. 'scmst') then
         cane_wt = g_dm_green(sstem) + g_dm_green(sucrose)
         scmst = divide(g_dm_green(sucrose),cane_wt,0.0)
         call respond2get_real_var (variable_name
     :                             , '(g/g)'
     :                             , scmst)
 
      elseif (variable_name .eq. 'sucrose_wt') then
         ! Add dead pool to allow for lodged stalks
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g_dm_green(sucrose)
     :                              +g_dm_dead(sucrose))
 
      elseif (variable_name .eq. 'cabbage_wt') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g_dm_green(cabbage))
 
      elseif (variable_name .eq. 'cane_wt') then
         ! Add dead pool for lodged crops
         cane_wt = g_dm_green(sstem)+g_dm_green(sucrose)
     :           + g_dm_dead(sstem)+g_dm_dead(sucrose)
 
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , cane_wt)
 
      elseif (variable_name .eq. 'biomass') then
 
         biomass =
     :        sum_Real_array(g_dm_green,max_part)-g_dm_green(root)
     :      + sum_real_array(g_dm_senesced,max_part)-g_dm_senesced(root)
     :      + sum_real_array(g_dm_dead,max_part)-g_dm_dead(root)
 
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , biomass)
 
      elseif (variable_name .eq. 'green_biomass') then
         ! Add dead pool for lodged crops
         biomass =
     :        sum_Real_array(g_dm_green,max_part)-g_dm_green(root)
     :           + g_dm_dead(sstem)+g_dm_dead(sucrose)
 
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , biomass)
 
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
 
      elseif (variable_name .eq. 'dlt_dm') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g_dlt_dm)
 
      elseif (variable_name .eq. 'partition_xs') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g_partition_xs)
 
      elseif (variable_name .eq. 'dlt_dm_green') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g_dlt_dm_green
     :                             , max_part)
 
      elseif (variable_name .eq. 'dlt_dm_detached') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g_dlt_dm_detached
     :                             , max_part)
 
cbak Reporting of N concentrations
 
      elseif (variable_name .eq. 'n_critical') then
         call respond2get_real_array (variable_name
     :                             , '(g/g)'
     :                             , g_N_conc_crit
     :                             , max_part)
 
      elseif (variable_name .eq. 'n_minimum') then
         call respond2get_real_array (variable_name
     :                             , '(g/g)'
     :                             , g_N_conc_min
     :                             , max_part)
 
cbak
      elseif (variable_name .eq. 'n_conc_leaf') then
        Conc_N_leaf = divide (g_N_green(leaf) ,g_dm_green(leaf), 0.0)
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , Conc_N_leaf)
 
      elseif (variable_name .eq. 'n_conc_cab') then
      Conc_N_cab = divide (g_N_green(cabbage), g_dm_green(cabbage), 0.0)
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , Conc_N_cab)
 
      elseif (variable_name .eq. 'n_conc_cane') then
        Conc_N_cane = divide (g_N_green(sstem)+g_N_green(sucrose),
     :                 g_dm_green(sstem)+g_dm_green(sucrose), 0.0)
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , Conc_N_cane)
 
cbak Weights of N in plant
 
      elseif (variable_name .eq. 'n_leaf_crit') then
       N_leaf_crit = g_N_conc_crit(leaf) * g_dm_green(leaf)
 
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , N_leaf_crit)
 
      elseif (variable_name .eq. 'n_leaf_min') then
       N_leaf_min = g_N_conc_min(leaf) * g_dm_green(leaf)
 
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , N_leaf_min)
 
 
      elseif (variable_name .eq. 'biomass_n') then
         biomass_n =
     :        sum_Real_array(g_n_green,max_part)-g_n_green(root)
     :      + sum_real_array(g_n_senesced,max_part)-g_n_senesced(root)
     :      + sum_real_array(g_n_dead,max_part)-g_n_dead(root)
cbak
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , biomass_n)
 
      elseif (variable_name .eq. 'green_biomass_n') then
         green_biomass_n =
     :        sum_Real_array(g_n_green,max_part)-g_n_green(root)
cbak
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , green_biomass_n)
 
 
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
 
cbak   Delta N in plant tops
 
      elseif (variable_name .eq. 'dlt_n_green') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g_dlt_N_green
     :                             , max_part)
 
      elseif (variable_name .eq. 'swdef_pheno') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g_swdef_pheno)
 
      elseif (variable_name .eq. 'swdef_photo') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g_swdef_photo)
 
      elseif (variable_name .eq. 'swdef_expan') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g_swdef_expansion)
 
      elseif (variable_name .eq. 'swdef_stalk') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g_swdef_stalk)
 
      elseif (variable_name .eq. 'nfact_photo') then
cjhtemp
c      call sugar_nit_stress_photo (1)
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g_nfact_photo)
 
cbak
      elseif (variable_name .eq. 'nfact_expan') then
cjhtemp
c      call sugar_nit_stress_expansion (1)
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g_nfact_expansion)
 
      elseif (variable_name .eq. 'ep') then
         num_layers = count_of_real_vals (g_dlayer, max_layer)
         do 10 layer = 1, num_layers
            rwu(layer) = - g_dlt_sw_dep(layer)
   10    continue
         call respond2get_real_array (variable_name
     :                               , '(mm)'
     :                               , rwu
     :                               , num_layers)
 
      elseif (variable_name .eq. 'cep') then
         call respond2get_real_var (variable_name
     :                             , '(mm)'
     :                             , - g_transpiration_tot)
 
      elseif (variable_name .eq. 'sw_demand') then
         call respond2get_real_var (variable_name
     :                             , '(mm)'
     :                             , g_sw_demand)
 
      elseif (variable_name .eq. 'fasw') then
         fasw = sugar_profile_fasw ()
         call respond2get_real_var (variable_name
     :                             , '(0-1)'
     :                             , fasw)
 
      elseif (variable_name .eq. 'cane_dmf') then
         cane_dmf = divide (
     :                      g_dm_green(sstem)+g_dm_green(sucrose)
     :                     ,g_dm_green(sstem)+g_dm_green(sucrose)
     :                                       +g_plant_wc(sstem)
     :                     ,0.0)
         call respond2get_real_var (variable_name
     :                             , '(0-1)'
     :                             , cane_dmf)
 
      elseif (variable_name .eq. 'oxdef_photo') then
         call respond2get_real_var (variable_name
     :                             , '(0-1)'
     :                             , g_oxdef_photo)
 
      elseif (variable_name .eq. 'das') then
         das = sum_between (sowing, now, g_days_tot)
         das = nint(das)
         call respond2get_real_var (variable_name
     :                             , '(days)'
     :                             , das)
 
            ! plant nitrogen
 
      elseif (variable_name .eq. 'n_uptake') then
         act_N_up = g_N_uptake_tot*gm2kg /sm2ha
         call respond2get_real_var (variable_name
     :                             , '(kg/ha)'
     :                             , act_N_up)
 
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
      elseif (variable_name .eq. 'no3_demand') then
         N_demand = sum_real_array (g_N_demand, max_part)*10.
 
         call respond2get_real_var (variable_name
     :                             , '(kg/ha)'
     :                             , N_demand)
 
      elseif (variable_name .eq. 'n_supply') then
         N_supply = sum_real_array ( g_dlt_N_green, max_part)
     :            - g_dlt_N_green(root)
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , N_supply)
 
 
      elseif (variable_name .eq. 'rlv') then
         num_layers = count_of_real_vals (g_dlayer, max_layer)
         do 2000 layer = 1, num_layers
            rlv(layer) = divide (g_root_length(layer)
     :                          ,g_dlayer(layer)
     :                          ,0.0)
 2000    continue
         call respond2get_real_array (variable_name
     :                               , '(mm/mm3)'
     :                               , rlv
     :                               , num_layers)
 
      elseif (variable_name .eq. 'dm_graze') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g_dm_graze)
 
      elseif (variable_name .eq. 'n_graze') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g_N_graze)
      elseif (variable_name .eq. 'lai2') then
         temp = sum_real_array(g_leaf_area,max_leaf)
         temp = temp * g_plants / 1000000.
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , temp)
      elseif (variable_name .eq. 'leaf_wt2') then
         temp = sum_real_array(g_leaf_dm,max_leaf)
         temp = temp * g_plants
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , temp)
 
      else
         ! not my variable
         call message_unused ()
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      real function sugar_stage_code
     :               (
     :                C_stage_code_list
     :              , G_phase_tt
     :              , G_tt_tot
     :              , stage_no, stage_table, numvals
     :               )
*     ===========================================================
      implicit none
      include   'const.inc'            ! err_user
      include   'sugconst.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      REAL       C_stage_code_list(*)  ! (INPUT)  list of stage numbers
      REAL       G_phase_tt(*)         ! (INPUT)  Cumulative growing degree days required for each stage (deg days)
      REAL       G_tt_tot(*)           ! (INPUT)  the sum of growing degree days for a phenological stage (oC d)
      real       stage_no              ! (INPUT) stage number to convert
      real       stage_table(*)        ! (INPUT) table of stage codes
      integer    numvals               ! (INPUT) size_of of table

*+  Purpose
*       Return an interpolated stage code from a table of stage_codes
*       and a nominated stage number. Returns 0 if the stage number is not
*       found. Interpolation is done on thermal time.

*+  Changes
*       070495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_stage_code')

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
      sugar_stage_code = x_stage_code
 
      call pop_routine (my_name)
 
      return
      end



*     ===========================================================
      subroutine sugar_read_constants ()
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'sugar.inc'
      include 'read.pub'                          
      include 'write.pub'                         
      include 'error.pub'                         

*+  Purpose
*       Crop initialisation - reads constants from constants file

*+  Changes
*     060495 nih taken from template
*     020998 sb deleted c_year_lb and c_year_ub.

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_read_constants')
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
 
      call read_real_var (section_name
     :                    , 'tt_emerg_to_begcane_ub', '()'
     :                    , c_tt_emerg_to_begcane_ub, numvals
     :                    , 0.0, 10000.0)
 
      call read_real_var (section_name
     :                    , 'tt_begcane_to_flowering_ub', '()'
     :                    , c_tt_begcane_to_flowering_ub, numvals
     :                    , 0.0, 10000.0)
 
      call read_real_var (section_name
     :                    , 'tt_flowering_to_crop_end_ub', '()'
     :                    , c_tt_flowering_to_crop_end_ub, numvals
     :                    , 0.0, 10000.0)
 
         !    sugar_N_uptake
 
      call read_real_var (section_name
     :                    , 'no3_diffn_const', '(days)'
     :                    , c_NO3_diffn_const, numvals
     :                    , 0.0, 100.0)
 
      call read_char_var (section_name
     :                   , 'n_supply_preference', '()'
     :                   , c_n_supply_preference, numvals)
 
         !    sugar_get_root_params
 
      call read_real_var (section_name
     :                    , 'll_ub', '()'
     :                    , c_ll_ub, numvals
     :                    , 0.0, 1000.0)
 
      call read_real_var (section_name
     :                    , 'kl_ub', '()'
     :                    , c_kl_ub, numvals
     :                    , 0.0, 1000.0)
 
         !    sugar_watck
 
      call read_real_var (section_name
     :                    , 'minsw', '()'
     :                    , c_minsw, numvals
     :                    , 0.0, 1000.0)
 
 
         !    sugar_get_other_variables
 
         ! checking the bounds of the bounds..
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
 
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_zero_globals ()
*     ===========================================================
      implicit none
      include   'sugar.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Purpose
*       Zero global variables and arrays

*+  Changes
*     150595 nih created from sugar_zero_variables

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_zero_globals')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
          ! zero pools etc.
 
      call fill_real_array (g_cnd_photo, 0.0, max_stage)
      call fill_real_array (g_cswd_expansion, 0.0, max_stage)
      call fill_real_array (g_cswd_pheno, 0.0, max_stage)
      call fill_real_array (g_cswd_photo, 0.0, max_stage)
      call fill_real_array (g_days_tot, 0.0, max_stage)
      call fill_real_array (g_dm_dead, 0.0, max_part)
      call fill_real_array (g_dm_green, 0.0, max_part)
      call fill_real_array (g_dm_plant_min, 0.0, max_part)
      call fill_real_array (g_plant_wc, 0.0, max_part)
      call fill_real_array (g_dm_plant_top_tot, 0.0, max_stage)
      call fill_real_array (g_leaf_area, 0.0, max_leaf)
      call fill_real_array (g_leaf_area, 0.0, max_leaf)
      call fill_real_array (g_leaf_dm, 0.0, max_leaf)
      call fill_real_array (g_leaf_no, 0.0, max_stage)
      call fill_real_array (g_node_no, 0.0, max_stage)
      call fill_real_array (g_node_no_dead, 0.0, max_stage)
      call fill_real_array (g_N_conc_crit, 0.0, max_part)
      call fill_real_array (g_N_conc_min, 0.0, max_part)
      call fill_real_array (g_N_green, 0.0, max_part)
      call fill_real_array (g_phase_tt, 0.0, max_stage)
      call fill_real_array (g_tt_tot, 0.0, max_stage)
      call fill_real_array (g_dm_senesced, 0.0, max_part)
      call fill_real_array (g_N_dead, 0.0, max_part)
      call fill_real_array (g_N_senesced, 0.0, max_part)
      call fill_real_array (g_root_length, 0.0, max_layer)
      call fill_real_array (g_plant_wc, 0.0, max_part)
      call fill_real_array (g_dlt_plant_wc, 0.0, max_part)
 
      g_num_layers = 0
      g_canopy_height = 0.0
      g_isdate = 0
      g_mdate = 0
      g_leaf_no_final = 0.0
      g_lai_max = 0.0
      g_N_conc_act_stover_tot = 0.0
      g_N_conc_crit_stover_tot = 0.0
      g_N_demand_tot = 0.0
      g_N_uptake_stover_tot = 0.0
      g_N_uptake_tot = 0.0
      g_plants = 0.0
cnh      g_initial_plant_density = 0.0
      g_root_depth = 0.0
      g_sowing_depth = 0.0
      g_slai = 0.0
      g_lai = 0.0
      g_transpiration_tot = 0.0
      g_previous_stage = 0.0
      g_ratoon_no = 0
      g_node_no_detached = 0.0
      g_lodge_flag = .false.
      g_min_sstem_sucrose = 0.0
 
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_zero_parameters ()
*     ===========================================================
      implicit none
      include   'sugar.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Purpose
*       Zero parameter variables and arrays

*+  Changes
*     150595 nih created from sugar_zero_variables

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_zero_parameters')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
          ! zero pools etc.
 
      call fill_real_array (p_ll_dep, 0.0, max_layer)
      g_uptake_source = ' '
cnh      c_crop_type = ' '
 
      call pop_routine (my_name)
      return
      end



* ====================================================================
       subroutine sugar_prepare ()
* ====================================================================
      implicit none
      include 'sugar.inc'
      include 'error.pub'                         

*+  Purpose
*     APSim allows modules to perform calculations in preparation for
*     the standard APSim timestep.  This model uses this opportunity
*     to calculate potential growth variables for the coming day
*     and phenological development.

*+  Changes
*   neilh - 05-07-1995 - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'sugar_prepare')

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
 
      if (g_crop_status.eq.crop_alive) then
         call sugar_get_other_variables ()
 
         call sugar_nit_stress_photo (1)
         call sugar_nit_stress_expansion (1)
         call sugar_nit_stress_pheno (1)
         call sugar_nit_stress_stalk (1)
 
         call sugar_temp_stress_photo(1)
         call sugar_temp_stress_stalk(1)
 
         call sugar_light_supply(1)
         call sugar_water_log (1)
         call sugar_bio_RUE(1)
         call sugar_transpiration_eff(1)
         call sugar_water_demand (1)
         call sugar_nit_demand_est (1)
 
      else
      endif
 
      call pop_routine (myname)
      return
      end



*     ===========================================================
      subroutine sugar_read_crop_constants (section_name)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'sugar.inc'
      include 'read.pub'                          
      include 'write.pub'                         
      include 'error.pub'                         

*+  Sub-Program Arguments
      character section_name*(*)

*+  Purpose
*       Crop initialisation - reads constants from constants file

*+  Changes
*     <insert here>

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_read_crop_constants')

*+  Local Variables
      integer    numvals               ! number of values returned

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      call write_string (lu_scr_sum
     :    ,new_line//'    - Reading constants from '//section_name)
 
         !    sugar_get_cultivar_params
 
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
 
      call read_real_array (section_name
     :                     , 'transp_eff_cf', max_stage, '()'
     :                     , c_transp_eff_cf, numvals
     :                     , 0.0, 1.0)
 
      call read_real_array (section_name
     :                     , 'n_fix_rate', max_stage, '()'
     :                     , c_n_fix_rate, numvals
     :                     , 0.0, 1.0)
 
      call read_real_var (section_name
     :                    , 'extinction_coef', '()'
     :                    , c_extinction_coef, numvals
     :                    , 0.0, 10.0)
 
      call read_real_var (section_name
     :                    , 'extinction_coef_dead', '()'
     :                    , c_extinction_coef_dead, numvals
     :                    , 0.0, 10.0)
 
         ! crop failure
 
      call read_real_var (section_name
     :                    , 'leaf_no_crit', '()'
     :                    , c_leaf_no_crit, numvals
     :                    , 0.0, 100.0)
 
      call read_real_var (section_name
     :                    , 'tt_emerg_limit', '(oC)'
     :                    , c_tt_emerg_limit, numvals
     :                    , 0.0, 1000.0)
 
      call read_real_var (section_name
     :                    , 'days_germ_limit', '(days)'
     :                    , c_days_germ_limit, numvals
     :                    , 0.0, 365.0)
 
      call read_real_var (section_name
     :                    , 'swdf_pheno_limit', '()'
     :                    , c_swdf_pheno_limit, numvals
     :                    , 0.0, 1000.0)
 
      call read_real_var (section_name
     :                    , 'swdf_photo_limit', '()'
     :                    , c_swdf_photo_limit, numvals
     :                    , 0.0, 1000.0)
 
      call read_real_var (section_name
     :                    , 'swdf_photo_rate', '()'
     :                    , c_swdf_photo_rate, numvals
     :                    , 0.0, 1.0)
 
 
         !    sugar_root_depth
 
      call read_real_var (section_name
     :                    , 'initial_root_depth', '(mm)'
     :                    , c_initial_root_depth, numvals
     :                    , 0.0, 1000.0)
 
      call read_real_var (section_name
     :                    , 'specific_root_length', '(mm)'
     :                    , c_specific_root_length, numvals
     :                    , 0.0, 50000.0)
 
      call read_real_array (section_name
     :                     , 'x_plant_rld', max_table, '(mm/mm3/plant)'
     :                     , c_x_plant_rld, c_num_plant_rld
     :                     , 0.0, 0.1)
 
      call read_real_array (section_name
     :                     , 'y_rel_root_rate', max_table, '(0-1)'
     :                     , c_y_rel_root_rate, c_num_plant_rld
     :                     , 0.0, 1.0)
 
      call read_real_var (section_name
     :                    , 'root_die_back_fr', '(0-1)'
     :                    , c_root_die_back_fr, numvals
     :                    , 0.0, 1.0)
 
         !    sugar_leaf_area_init
 
      call read_real_var (section_name
     :                    , 'initial_tpla', '(mm^2)'
     :                    , c_initial_tpla, numvals
     :                    , 0.0, 100000.0)
 
         !    sugar_leaf_area_devel
      call read_real_array (section_name
     :                    , 'sla_lfno',max_table,'()'
     :                    , c_sla_lfno, c_num_sla_lfno
     :                    , 0.0, 100.0)
 
      call read_real_array (section_name
     :                    , 'sla_max',max_table,'(mm^2/g)'
     :                    , c_sla_max, numvals
     :                    , 0.0, 50000.0)
 
      call read_real_array (section_name
     :                    , 'sla_min',max_table,'(mm^2/g)'
     :                    , c_sla_min, numvals
     :                    , 0.0, 50000.0)
 
         !    sugar_height
 
      call read_real_array (section_name
     :                    , 'x_stem_wt',max_table,'(g/plant)'
     :                    , c_x_stem_wt, c_num_stem_wt
     :                    , 0.0, 10000.0)
 
      call read_real_array (section_name
     :                    , 'y_height',max_table,'(mm)'
     :                    , c_y_height, c_num_stem_wt
     :                    , 0.0, 10000.0)
 
         !    sugar_transp_eff
 
      call read_real_var (section_name
     :                    , 'svp_fract', '()'
     :                    , c_svp_fract, numvals
     :                    , 0.0, 1.0)
 
 
         !    sugar_germination
 
      call read_real_var (section_name
     :                    , 'pesw_germ', '(mm/mm)'
     :                    , c_pesw_germ, numvals
     :                    , 0.0, 1.0)
 
      call read_real_array (section_name
     :                    , 'fasw_emerg', max_table,'(0-1)'
     :                    , c_fasw_emerg
     :                    , c_num_fasw_emerg
     :                    , 0.0, 1.0)
      call read_real_array (section_name
     :                    , 'rel_emerg_rate', max_table,'(0-1)'
     :                    , c_rel_emerg_rate
     :                    , c_num_fasw_emerg
     :                    , 0.0, 1.0)
 
         !    sugar_leaf_appearance
 
      call read_real_var (section_name
     :                    , 'leaf_no_at_emerg', '()'
     :                    , c_leaf_no_at_emerg, numvals
     :                    , 0.0, 100.0)
 
         !    sugar_phenology_init
 
      call read_real_var (section_name
     :                    , 'shoot_lag', '(oC)'
     :                    , c_shoot_lag, numvals
     :                    , 0.0, 1000.0)
 
      call read_real_var (section_name
     :                    , 'shoot_rate', '(oC/mm)'
     :                    , c_shoot_rate, numvals
     :                    , 0.0, 100.0)
 
      call read_real_array (section_name
     :                    , 'x_node_no_app', max_table,'(oC)'
     :                    , c_x_node_no_app
     :                    , c_num_node_no_app
     :                    , 0.0, 1000.0)
 
      call read_real_array (section_name
     :                    , 'y_node_app_rate', max_table,'(oC)'
     :                    , c_y_node_app_rate
     :                    , c_num_node_no_app
     :                    , 0.0, 1000.0)
 
      call read_real_array (section_name
     :                    , 'x_node_no_leaf', max_table,'(oC)'
     :                    , c_x_node_no_leaf
     :                    , c_num_node_no_leaf
     :                    , 0.0, 1000.0)
 
      call read_real_array (section_name
     :                    , 'y_leaves_per_node', max_table,'(oC)'
     :                    , c_y_leaves_per_node
     :                    , c_num_node_no_leaf
     :                    , 0.0, 1000.0)
 
         !    sugar_dm_init
 
      call read_real_var (section_name
     :                    , 'dm_leaf_init', '(g/plant)'
     :                    , c_dm_leaf_init, numvals
     :                    , 0.0, 1000.0)
 
      call read_real_var (section_name
     :                    , 'dm_root_init', '(g/plant)'
     :                    , c_dm_root_init, numvals
     :                    , 0.0, 1000.0)
 
      call read_real_var (section_name
     :                    , 'dm_sstem_init', '(g/plant)'
     :                    , c_dm_sstem_init, numvals
     :                    , 0.0, 1000.0)
 
      call read_real_var (section_name
     :                    , 'dm_cabbage_init', '(g/plant)'
     :                    , c_dm_cabbage_init, numvals
     :                    , 0.0, 1000.0)
 
      call read_real_var (section_name
     :                    , 'dm_sucrose_init', '(g/plant)'
     :                    , c_dm_sucrose_init, numvals
     :                    , 0.0, 1000.0)
 
      call read_real_var (section_name
     :                    , 'leaf_cabbage_ratio', '()'
     :                    , c_leaf_cabbage_ratio, numvals
     :                    , 0.0, 10.0)
 
      call read_real_var (section_name
     :                    , 'cabbage_sheath_fr', '()'
     :                    , c_cabbage_sheath_fr, numvals
     :                    , 0.0, 1.0)
 
         !    sugar_dm_senescence
 
      call read_real_var (section_name
     :                    , 'dm_root_sen_frac', '()'
     :                    , c_dm_root_sen_frac, numvals
     :                    , 0.0, 1.0)
 
         !    sugar_dm_dead_detachment
 
      call read_real_array (section_name
     :                    , 'dead_detach_frac', max_part, '()'
     :                    , c_dead_detach_frac, numvals
     :                    , 0.0, 1.0)
 
      call read_real_array (section_name
     :                    , 'sen_detach_frac', max_part, '()'
     :                    , c_sen_detach_frac, numvals
     :                    , 0.0, 1.0)
 
         !    sugar_leaf_area_devel
 
      call read_real_var (section_name
     :                    , 'leaf_no_correction', '()'
     :                    , c_leaf_no_correction, numvals
     :                    , 0.0, 100.0)
 
         !    sugar_leaf_area_sen_light
 
      call read_real_var (section_name
     :                   , 'lai_sen_light', '(m^2/m^2)'
     :                   , c_lai_sen_light, numvals
     :                   , 0.0, 10.0)
 
      call read_real_var (section_name
     :                    , 'sen_light_slope', '()'
     :                    , c_sen_light_slope, numvals
     :                    , 0.0, 100.0)
 
 
         !    sugar_leaf_area_sen_frost
 
      call read_real_array (section_name
     :                   , 'frost_temp', max_table, '(oC)'
     :                   , c_frost_temp, c_num_frost_temp
     :                   , -20.0, 100.0)
 
      call read_real_array (section_name
     :                   , 'frost_fraction', max_table, '(oC)'
     :                   , c_frost_fraction, numvals
     :                   , 0.0, 1.0)
 
         !    sugar_leaf_area_sen_water
 
      call read_real_var (section_name
     :                    , 'sen_rate_water', '()'
     :                    , c_sen_rate_water, numvals
     :                    , 0.0, 100.0)
 
         !    sugar_phenology_init
 
      call read_real_var (section_name
     :                   , 'twilight', '(o)'
     :                   , c_twilight, numvals
     :                   , -90.0, 90.0)
 
         !    sugar_N_conc_limits
 
      call read_real_array (section_name
     :                     , 'x_stage_code', max_stage, '()'
     :                     , c_x_stage_code, c_num_N_conc_stage
     :                     , 0.0, 100.0)
 
      call read_real_array (section_name
     :                     , 'y_n_conc_crit_leaf', max_stage, '()'
     :                     , c_y_N_conc_crit_leaf, c_num_N_conc_stage
     :                     , 0.0, 100.0)
 
      call read_real_array (section_name
     :                     , 'y_n_conc_min_leaf', max_stage, '()'
     :                     , c_y_N_conc_min_leaf, c_num_N_conc_stage
     :                     , 0.0, 100.0)
 
      call read_real_array (section_name
     :                     , 'y_n_conc_crit_cane', max_stage, '()'
     :                     , c_y_N_conc_crit_cane, c_num_N_conc_stage
     :                     , 0.0, 100.0)
 
      call read_real_array (section_name
     :                     , 'y_n_conc_min_cane', max_stage, '()'
     :                     , c_y_N_conc_min_cane, c_num_N_conc_stage
     :                     , 0.0, 100.0)
 
      call read_real_array (section_name
     :                     , 'y_n_conc_crit_cabbage', max_stage, '()'
     :                     , c_y_N_conc_crit_cabbage, c_num_N_conc_stage
     :                     , 0.0, 100.0)
 
      call read_real_array (section_name
     :                     , 'y_n_conc_min_cabbage', max_stage, '()'
     :                     , c_y_N_conc_min_cabbage, c_num_N_conc_stage
     :                     , 0.0, 100.0)
 
      call read_real_var (section_name
     :                   , 'n_conc_crit_root', '()'
     :                   , c_N_conc_crit_root, numvals
     :                   , 0.0, 100.0)
 
      call read_real_var (section_name
     :                   , 'n_conc_min_root', '()'
     :                   , c_N_conc_min_root, numvals
     :                   , 0.0, 100.0)
 
         !    sugar_N_init
 
      call read_real_var (section_name
     :                   , 'n_leaf_init_conc', '()'
     :                   , c_N_leaf_init_conc, numvals
     :                   , 0.0, 100.0)
 
      call read_real_var (section_name
     :                   , 'n_root_init_conc', '()'
     :                   , c_N_root_init_conc, numvals
     :                   , 0.0, 100.0)
 
      call read_real_var (section_name
     :                   , 'n_sstem_init_conc', '()'
     :                   , c_N_sstem_init_conc, numvals
     :                   , 0.0, 100.0)
 
      call read_real_var (section_name
     :                   , 'n_cabbage_init_conc', '()'
     :                   , c_N_cabbage_init_conc, numvals
     :                   , 0.0, 100.0)
 
         !    sugar_N_senescence
 
      call read_real_var (section_name
     :                   , 'n_leaf_sen_conc', '()'
     :                   , c_N_leaf_sen_conc, numvals
     :                   , 0.0, 100.0)
 
      call read_real_var (section_name
     :                   , 'n_root_sen_conc', '()'
     :                   , c_N_root_sen_conc, numvals
     :                   , 0.0, 100.0)
 
      call read_real_var (section_name
     :                   , 'n_cabbage_sen_conc', '()'
     :                   , c_N_cabbage_sen_conc, numvals
     :                   , 0.0, 100.0)
 
         !    sugar_rue_reduction
 
      call read_real_array (section_name
     :                     , 'x_ave_temp', max_table, '(oC)'
     :                     , c_x_ave_temp, c_num_ave_temp
     :                     , 0.0, 100.0)
 
 
      call read_real_array (section_name
     :                     , 'y_stress_photo', max_table, '()'
     :                     , c_y_stress_photo, numvals
     :                     , 0.0, 1.0)
 
      call read_real_array (section_name
     :                     , 'x_ave_temp_stalk', max_table, '(oC)'
     :                     , c_x_ave_temp_stalk, c_num_ave_temp_stalk
     :                     , 0.0, 100.0)
 
 
      call read_real_array (section_name
     :                     , 'y_stress_stalk', max_table, '()'
     :                     , c_y_stress_stalk, numvals
     :                     , 0.0, 1.0)
 
         !    sugar_tt
 
      call read_real_array (section_name
     :                     , 'x_temp', max_table, '(oC)'
     :                     , c_x_temp, c_num_temp
     :                     , 0.0, 100.0)
 
      call read_real_array (section_name
     :                     , 'y_tt', max_table, '(oC)'
     :                     , c_y_tt, c_num_temp
     :                     , 0.0, 100.0)
 
         !    sugar_swdef
 
      call read_real_array (section_name
     :                     , 'x_sw_demand_ratio', max_table, '()'
     :                     , c_x_sw_demand_ratio, c_num_sw_demand_ratio
     :                     , 0.0, 100.0)
 
      call read_real_array (section_name
     :                     , 'y_swdef_leaf', max_table, '()'
     :                     , c_y_swdef_leaf, c_num_sw_demand_ratio
     :                     , 0.0, 100.0)
 
      call read_real_array (section_name
     :                     , 'x_demand_ratio_stalk', max_table, '()'
     :                     , c_x_demand_ratio_stalk
     :                     , c_num_demand_ratio_stalk
     :                     , 0.0, 100.0)
 
      call read_real_array (section_name
     :                     , 'y_swdef_stalk', max_table, '()'
     :                     , c_y_swdef_stalk, c_num_demand_ratio_stalk
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
 
      ! Nitrogen Stress Factors
      ! -----------------------
      call read_real_var (section_name
     :                   , 'k_nfact_photo', '()'
     :                   , c_k_nfact_photo, numvals
     :                   , 0.0, 100.0)
 
      call read_real_var (section_name
     :                   , 'k_nfact_expansion', '()'
     :                   , c_k_nfact_expansion, numvals
     :                   , 0.0, 100.0)
 
      call read_real_var (section_name
     :                   , 'k_nfact_stalk', '()'
     :                   , c_k_nfact_stalk, numvals
     :                   , 0.0, 100.0)
 
      call read_real_var (section_name
     :                   , 'k_nfact_pheno', '()'
     :                   , c_k_nfact_pheno, numvals
     :                   , 0.0, 100.0)
 
      ! Water logging function
      ! ----------------------
      call read_real_array (section_name
     :                     , 'oxdef_photo_rtfr', max_table, '()'
     :                     , c_oxdef_photo_rtfr, c_num_oxdef_photo
     :                     , 0.0, 1.0)
      call read_real_array (section_name
     :                     , 'oxdef_photo', max_table, '()'
     :                     , c_oxdef_photo, c_num_oxdef_photo
     :                     , 0.0, 1.0)
 
      ! Plant Water Content function
      ! ----------------------------
      call read_real_array (section_name
     :                     , 'cane_dmf_max', max_table, '()'
     :                     , c_cane_dmf_max, c_num_cane_dmf
     :                     , 0.0, 1.0)
      call read_real_array (section_name
     :                     , 'cane_dmf_min', max_table, '()'
     :                     , c_cane_dmf_min, c_num_cane_dmf
     :                     , 0.0, 1.0)
      call read_real_array (section_name
     :                     , 'cane_dmf_tt', max_table, '()'
     :                     , c_cane_dmf_tt, c_num_cane_dmf
     :                     , 0.0, 10000.)
      call read_real_var (section_name
     :                   , 'cane_dmf_rate', '()'
     :                   , c_cane_dmf_rate, numvals
     :                   , 0.0, 100.0)
 
 
      ! Death by Lodging Constants
      ! --------------------------
      call read_real_array (section_name
     :                     , 'stress_lodge', max_table, '(0-1)'
     :                     , c_stress_lodge, c_num_stress_lodge
     :                     , 0.0, 1.0)
      call read_real_array (section_name
     :                     , 'death_fr_lodge', max_table, '(0-1)'
     :                     , c_death_fr_lodge, c_num_stress_lodge
     :                     , 0.0, 1.0)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_update_other_variables ()
*     ===========================================================
      implicit none
      include   'convert.inc'
      include   'sugar.inc'
      include 'data.pub'                          
      include 'crp_comm.pub'                      
      include 'error.pub'                         

*+  Purpose
*       Update other modules states

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_update_other_variables')

*+  Local Variables
      real       dm_residue            ! dry matter added to residue (g/m^2)
      real       N_residue             ! nitrogen added to residue (g/m^2)

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
         ! dispose of detached material from senesced parts in
         ! live population
 
      dm_residue = (sum_real_array (g_dlt_dm_detached, max_part)
     :           - g_dlt_dm_detached(root))
      N_residue = (sum_real_array (g_dlt_N_detached, max_part)
     :          - g_dlt_N_detached(root))
 
      call crop_top_residue (c_crop_type, dm_residue, N_residue)
 
             ! put roots into root residue
 
      call crop_root_incorp (g_dlt_dm_detached(root)
     :                      ,g_dlt_N_detached(root)
     :                      ,g_dlayer
     :                      ,g_root_length
     :                      ,g_root_depth
     :                      ,c_crop_type
     :                      ,max_layer
     :                      )
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_hill_up ()
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'sugar.inc'
      include 'engine.pub'                        
      include 'data.pub'                          
      include 'intrface.pub'                      
      include 'error.pub'                         

*+  Purpose
*       Mound soil around base of crop and bury some plant material

*+  Changes
*     120897 nih

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_hill_up')

*+  Local Variables
      integer    numvals               ! number of values found in array
      real       canefr
      real       topsfr
      real       fom(max_layer)
      real       fon(max_layer)
      integer    leaf_no

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (int(g_current_stage).eq.emerg) then
         call collect_real_var ('topsfr', '()'
     :                      , topsfr, numvals, 0.0, 1.0)
         call collect_real_var ('canefr', '()'
     :                      , canefr, numvals, 0.0, 1.0)
 
         call fill_Real_array (fom,0.0,max_layer)
         call fill_Real_array (fon,0.0,max_layer)
 
         fom(1) = topsfr * (g_dm_green(leaf)
     :                     +g_dm_green(cabbage)
     :                     +g_dm_senesced(leaf)
     :                     +g_dm_senesced(cabbage)
     :                     +g_dm_dead(leaf)
     :                     +g_dm_dead(cabbage))
     :          + canefr * (g_dm_green(sstem)
     :                     +g_dm_green(sucrose)
     :                     +g_dm_senesced(sstem)
     :                     +g_dm_senesced(sucrose)
     :                     +g_dm_dead(sstem)
     :                     +g_dm_dead(sucrose))
 
         fon(1) = topsfr * (g_n_green(leaf)
     :                     +g_n_green(cabbage)
     :                     +g_n_senesced(leaf)
     :                     +g_n_senesced(cabbage)
     :                     +g_n_dead(leaf)
     :                     +g_n_dead(cabbage))
     :          + canefr * (g_n_green(sstem)
     :                     +g_n_green(sucrose)
     :                     +g_n_senesced(sstem)
     :                     +g_n_senesced(sucrose)
     :                     +g_n_dead(sstem)
     :                     +g_n_dead(sucrose))
 
 
            call New_postbox ()
 
            call post_char_var('dlt_fom_type=','()',c_crop_type)
 
            call post_real_array ('dlt_fom_wt'
     :                           ,'(kg/ha)'
     :                           ,fom
     :                           ,1)
 
            call post_real_array ('dlt_fom_n'
     :                           ,'(kg/ha)'
     :                           ,fon
     :                           ,1)
 
            call message_send_immediate (
     :                                 unknown_module
     :                               , 'incorp_fom'
     :                               , Blank
     :                               )
 
            call Delete_postbox ()
 
         g_dm_green(leaf) = g_dm_green(leaf)*(1.-topsfr)
         g_dm_green(cabbage) = g_dm_green(cabbage)*(1.-topsfr)
         g_dm_senesced(leaf) = g_dm_senesced(leaf)*(1.-topsfr)
         g_dm_senesced(cabbage) = g_dm_senesced(cabbage)*(1.-topsfr)
         g_dm_dead(leaf) = g_dm_dead(leaf)*(1.-topsfr)
         g_dm_dead(cabbage) = g_dm_dead(cabbage)*(1.-topsfr)
 
         g_dm_green(sstem) = g_dm_green(sstem)*(1.-canefr)
         g_dm_green(sucrose) = g_dm_green(sucrose)*(1.-canefr)
         g_dm_senesced(sstem) = g_dm_senesced(sstem)*(1.-canefr)
         g_dm_senesced(sucrose) = g_dm_senesced(sucrose)*(1.-canefr)
         g_dm_dead(sstem) = g_dm_dead(sstem)*(1.-canefr)
         g_dm_dead(sucrose) = g_dm_dead(sucrose)*(1.-canefr)
 
         g_n_green(leaf) = g_n_green(leaf)*(1.-topsfr)
         g_n_green(cabbage) = g_n_green(cabbage)*(1.-topsfr)
         g_n_senesced(leaf) = g_n_senesced(leaf)*(1.-topsfr)
         g_n_senesced(cabbage) = g_n_senesced(cabbage)*(1.-topsfr)
         g_n_dead(leaf) = g_n_dead(leaf)*(1.-topsfr)
         g_n_dead(cabbage) = g_n_dead(cabbage)*(1.-topsfr)
 
         g_n_green(sstem) = g_n_green(sstem)*(1.-canefr)
         g_n_green(sucrose) = g_n_green(sucrose)*(1.-canefr)
         g_n_senesced(sstem) = g_n_senesced(sstem)*(1.-canefr)
         g_n_senesced(sucrose) = g_n_senesced(sucrose)*(1.-canefr)
         g_n_dead(sstem) = g_n_dead(sstem)*(1.-canefr)
         g_n_dead(sucrose) = g_n_dead(sucrose)*(1.-canefr)
 
      ! Now we need to update the leaf tracking info
 
      g_lai = g_lai * (1. - topsfr)
      g_slai = g_slai * (1. - topsfr)
 
      do 100 leaf_no = 1, max_leaf
         g_leaf_area(leaf_no) = g_leaf_area(leaf_no)
     :                        *(1.-topsfr)
         g_leaf_dm (leaf_no) = g_leaf_dm (leaf_no)
     :                        *(1.-topsfr)
  100 continue
 
      else
         call fatal_Error(Err_User,
     :      'Can only hill up during emergence phase')
      endif
 
      call pop_routine (my_name)
      return
      end



* ====================================================================
       subroutine sugar_lodge ()
* ====================================================================
      implicit none
      include 'sugar.inc'
      include 'write.pub'                         
      include 'error.pub'                         

*+  Purpose
*     <insert here>

*+  Changes
*     25-08-1997 - unknown - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'sugar_lodge')

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      g_lodge_flag = .true.
      call report_event ('crop lodging')
 
      call pop_routine (myname)
      return
      end



