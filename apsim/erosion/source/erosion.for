*     ===========================================================
      character*(*) function erosion_version ()
*     ===========================================================
 
*   Short description:
*     return version number of erosion module
 
*   Assumptions:
*     none
 
*   Notes:
*     none
 
*   Procedure attributes:
*     Version:         Any hardware/Fortran77
*     Extensions:      Long names <= 20 chars.
*                      Lowercase
*                      Underscore
*                      Inline comments
*                      Include
*                      implicit none
 
*   Changes:
*     DMS & NH 25/02/94
*     DMS 25/02/94 (new template)
 
*   Calls:
*     pop_routine
*     push_routine
 
* ----------------------- Declaration section ------------------------
 
      implicit none
 
*   Subroutine arguments
*     none
 
*   Global variables
 
      dll_import pop_routine                      
      dll_import push_routine                     
 
*   Internal variables
*     none
 
*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'erosion_version')
 
      character  version_number*(*) ! version number of module
      parameter (version_number = 'V1.14 170398')
 
*   Initial data values
*     none
 
* --------------------- Executable code section ----------------------
      call push_routine (my_name)
 
      erosion_version = version_number
 
      call pop_routine (my_name)
      return
      end
* ====================================================================
      subroutine APSIM_erosion (Action, Data_string)
* ====================================================================
 
*   Short description:
*     This routine is the interface between the main system and the
*     erosion module.
 
*   Assumptions:
*     none
 
*   Notes:
*     none
 
*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none
 
*   Changes:
*     DMS 25/02/94 (new template)
*     PdeV 25/08/94
*     011195 jngh  added call to message_unused
 
*   Calls:
*     erosion_version
*     erosion_init
*     erosion_write_summary
*     erosion_reset_dailies
*     erosion_get_dailies
*     erosion_get_other_variables
*     erosion_process
*     erosion_set_other_variables
*     erosion_send_my_variable
*     erosion_end_run
*     message_unused
*     pop_routine
*     push_routine
*     set_warning_off
 
* ----------------------- Declaration section ------------------------
 
      implicit none
      dll_export apsim_erosion
 
*   Subroutine arguments
      character  Action*(*)            ! Message action to perform
      character  Data_string*(*)       ! Message data
 
*   Global variables
      include   'const.inc'            ! Global constant definitions
      include   'erosion.inc'          ! module_name
 
      character  erosion_version*20    ! function
      integer    lastnb                ! function
      dll_import lastnb
 
      dll_import pop_routine                      
      dll_import message_unused                   
      dll_import get_current_module               
      dll_import set_warning_off                  
      dll_import push_routine                     
 
*   Internal variables
      character  module_name*10
 
*   Constant values
      character  my_name*(*)
      parameter (my_name = 'erosion')
 
*   Initial data values
*     None
 
* --------------------- Executable code section ----------------------
 
      call push_routine (my_name)
 
         ! initialise error flags
      call set_warning_off ()
 
      if (action.eq.MES_presence) then      ! report presence
         call get_current_module (module_name)
         write(*, *) 'module_name = '
     :              , module_name(:lastnb (module_name))
     :              // blank
     :              // erosion_version ()
 
      else if (Action.eq.MES_Init) then
            ! initilization once per run
            ! start with a clean slate
         call erosion_zero_variables ()
         call erosion_get_other_variables ()
         call erosion_init ()   ! get parameters & do initial one-off calc's
         call erosion_write_summary () ! tell summary file what we're using
 
      else if (Action.eq.MES_Process) then
         call erosion_zero_daily_variables ()
            ! get todays variables
         call erosion_get_other_variables ()
            ! do daily processes
         call erosion_process ()
            ! send back changed variables.
         call erosion_set_other_variables ()
 
      else if (Action.eq.MES_Get_variable) then
            ! respond to requests from other modules
         call erosion_send_my_variable (Data_string)
 
      else if (Action.eq.MES_Set_variable) then
         call erosion_set_my_variable (data_string)
 
      else if (Action.eq.MES_end_run) then
         call erosion_end_run ()
 
      else
            ! Do nothing..
         call Message_unused ()
      endif
 
      call pop_routine (my_name)
      return
      end
* ====================================================================
      subroutine erosion_init ()
* ====================================================================
 
*   Short description:
*     Initialise erosion module
 
*   Assumptions:
*     None
 
*   Notes:
*     none
 
*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none
 
*   Changes:
*     DMS 25/02/94 (new template)
 
*   Calls:
*     a_erosion_version
*     write_string
*     erosion_zero_variables
*     erosion_read_param
*     pop_routine
*     push_routine
 
*----------------------- Declaration section ------------------------
      implicit none
 
*   Subroutine arguments
*     none
 
*   Global variables
      include   'const.inc'
      include   'convert.inc'          ! pcnt2fract
      include   'erosion.inc'          ! erosion model commons
 
      character  erosion_version*40    ! function
      integer    count_of_real_vals    ! function
      dll_import count_of_real_vals
      real       sum_real_array        ! function
      dll_import sum_real_array
 
      dll_import pop_routine                      
      dll_import fatal_error                      
      dll_import report_event                     
      dll_import push_routine                     
 
*   Internal variables
      real       s                     ! temporary (USLE LS factor) slope (0-1)
      real       a                     ! temporary (USLE LS factor)
 
*   Constant values
      character  my_name*(*)
      parameter (my_name = 'erosion_init')
 
*   Initial data values
*     none
 
* --------------------- Executable code section ----------------------
      call push_routine (my_name)
 
         ! Notify system that we have initialised
      call report_event (' Initialising, Version : '
     :                  // erosion_version ())
 
         ! Get all parameters from parameter file
      call erosion_read_param ()
 
         ! Sanity checking
      if (g_bed_depth .lt. sum_real_array(g_dlayer, max_layer)) then
          call fatal_error(err_user,
     :                  'Depth to bedrock is less than profile depth')
      else
      endif
 
      if (p_model_type .ne. freeb_model .and.
     :    p_model_type .ne. rose_model ) then
         call fatal_error(err_user, 'Unknown model_type.')
      else
      endif
 
         ! Calculate USLE LS factor
      s = p_slope * pcnt2fract
      a = 0.6 * (1.0 - exp (-35.835 * s))
      p_ls_factor = ((p_slope_length / 22.1) ** a)
     :            * (65.41*s*s + 4.56*s + 0.065)
 
      if (p_profile_reduction .eq. on) then
 
            ! find soil profile to calculate
            ! initial layer_merge_mm
         p_layer_merge_mm = g_dlayer(
     :                      count_of_real_vals (g_dlayer, max_layer))
     :                    * p_profile_layer_merge
 
      else
         p_layer_merge_mm = 0.0
      endif
 
 
      call pop_routine (my_name)
      return
      end
* ====================================================================
      subroutine erosion_write_summary ()
* ====================================================================
 
*   Short description:
*     Tell summary file what parameters we're using
 
*   Assumptions:
*     None
 
*   Notes:
*     none
 
*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none
 
*   Changes:
*     DMS 25/02/94 (new template)
*     PdeV 2/10/94
 
*   Calls:
*     write_string
*     pop_routine
*     push_routine
 
* ----------------------- Declaration section ------------------------
      implicit none
 
*   Subroutine arguments
*     none
 
*   Global variables
      include   'const.inc'            ! Constant definitions
      include   'erosion.inc'          ! erosion model common
 
      dll_import pop_routine                      
      dll_import write_string                     
      dll_import push_routine                     
 
*   Internal variables
      character  string*(500)          ! String to output
 
*   Constant values
      character  my_name*(*)
      parameter (my_name = 'erosion_write_summary')
 
*   Initial data values
*     none
 
* --------------------- Executable code section ----------------------
      call push_routine (my_name)
 
      call write_string (lu_scr_sum, new_line//new_line)
 
      string = '                 Erosion Parameters'
      call write_string (lu_scr_sum, string)
 
      string = '     -----------------------------------------------'
      call write_string (lu_scr_sum, string)
 
      if (p_profile_reduction .eq. on) then
         write (string, '(a)')
     :          '      Profile reduction:                     on'
         call write_string (lu_scr_sum, string)
 
         write (string, '(a, f4.3)')
     :          '      Fraction of original layer for merge: '
     :                  , p_profile_layer_merge
         call write_string (lu_scr_sum, string)
 
      else
         write (string, '(a)')
     :          '      Profile reduction:                     off'
         call write_string (lu_scr_sum, string)
      endif
 
      if (p_model_type .eq. freeb_model) then
         write (string, '(2a)')
     :          '      Freebairn cover-sediment concentration model'
     :             , new_line
         call write_string (lu_scr_sum, string)
 
         write (string, '(a, f6.4, a)')
     :          '      LS factor:                             ',
     :          p_ls_factor, new_line
         call write_string (lu_scr_sum, string)
 
                     ! susp load K is 0.0 if not being used
         if (p_k_factor_susp .le. 0.0) then
           write (string, '(a, f6.4, a)')
     :          '      K factor:                              ',
     :          p_k_factor_bed, new_line
           call write_string (lu_scr_sum, string)
         else
           write (string, '(a, f6.4, a)')
     :          '      K factor (bedload):                    ',
     :          p_k_factor_bed, new_line
           call write_string (lu_scr_sum, string)
           write (string, '(a, f6.4, a)')
     :          '      K factor (suspended load):             ',
     :          p_k_factor_susp, new_line
           call write_string (lu_scr_sum, string)
         endif
 
         write (string, '(a, f6.4, a)')
     :          '      P factor:                              ',
     :          p_p_factor, new_line
         call write_string (lu_scr_sum, string)
 
      else if (p_model_type .eq. rose_model) then
 
         write (string, '(2a)')
     :          '      Rose sediment concentration model'
     :                  , new_line
         call write_string (lu_scr_sum, string)
 
         if (p_entrain_eff_susp .le. 0.0) then
           write (string, '(a, f6.4, a)')
     :          '       Efficiency of entrainment:            '
     :               , p_entrain_eff_bed, new_line
           call write_string (lu_scr_sum, string)
         else
           write (string, '(a, f6.4, a)')
     :          '       Efficiency of bed load entrainment:   '
     :               , p_entrain_eff_bed, new_line
           call write_string (lu_scr_sum, string)
           write (string, '(a, f6.4, a)')
     :          '       Efficiency of susp. load entrainment: '
     :               , p_entrain_eff_susp, new_line
           call write_string (lu_scr_sum, string)
         endif
 
         write (string, '(a, f6.2, a)')
     :          '       Slope (%):                            ',
     :          p_slope, new_line
         call write_string (lu_scr_sum, string)
 
      else
            ! whoops - whats going on?
         write (string, '(2a)')
     :          '      ? Unknown model type ?'
     :                  , new_line
         call write_string (lu_scr_sum, string)
 
      endif
 
      string = '     -----------------------------------------------'
      call write_string (lu_scr_sum, string)
 
      call pop_routine (my_name)
      return
      end
* ====================================================================
      subroutine erosion_read_param ()
* ====================================================================
 
*   Short description:
*     Read in all parameters from parameter file.
 
*   Assumptions:
*     None
 
*   Notes:
*     none
 
*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none
 
*   Changes:
*     DMS 25/02/94 (new template)
*     PdeV 25/08/94
*     210395 jngh changed from erosion_section to a parameters section
 
*   Calls:
*     erosion_model_type
*     pop_routine
*     push_routine
 
* ----------------------- Declaration section ------------------------
      implicit none
 
*   Subroutine arguments
*     none
 
*   Global variables
      include   'const.inc'
      include   'erosion.inc'          ! erosion model common block
 
      dll_import pop_routine                      
      dll_import read_real_var_optional           
      dll_import read_real_var                    
      dll_import read_char_var                    
      dll_import write_string                     
      dll_import push_routine                     
 
*   Internal variables
      integer    num_read              ! temporary
      integer    num_read_eteff        ! temporary
      integer    num_read_b2           ! temporary
      character  string*(80)           ! temporary
 
*   Constant values
      character  section_name*(*)
      parameter (section_name = 'parameters')
 
      character  my_name*(*)
      parameter (my_name = 'erosion_read_param')
 
*   Initial data values
*     none
 
* --------------------- Executable code section ----------------------
      call push_routine (my_name)
 
      call write_string (lu_scr_sum
     :                 ,new_line//'   - Reading Parameters')
 
                                ! determine model type
      call read_char_var (
     :     section_name         ! Section header
     :   , 'model'              ! Keyword
     :   , '()'                 ! Units
     :   , string               ! Variable
     :   , num_read)            ! Number of values returned
 
      if (string(1:4) .eq. 'rose') then
         p_model_type = rose_model
 
      else if (string(1:4) .eq. 'free') then
         p_model_type = freeb_model
 
      else
         p_model_type = 0
 
      endif
 
      call read_char_var (
     :     section_name         ! Section header
     :   , 'profile_reduction'  ! Keyword
     :   , '()'                 ! Units
     :   , string               ! Variable
     :   , num_read)            ! Number of values returned
 
      if (string(1:2) .eq. 'on') then
         p_profile_reduction = on
      else
         p_profile_reduction = off
      endif
 
      call read_real_var (
     :     section_name         ! Section header
     :   , 'profile_layer_merge'  ! Keyword
     :   , '()'                 ! Units
     :   , p_profile_layer_merge  ! Variable
     :   , num_read             ! Number of values returned
     :   , 0.0                  ! Lower Limit for bound checking
     :   , 1.0)                 ! Upper Limit for bound checking
 
      call read_real_var (
     :     section_name         ! Section header
     :   , 'minimum_depth'      ! Keyword
     :   , '()'                 ! Units
     :   , p_minimum_depth      ! Variable
     :   , num_read             ! Number of values returned
     :   , 0.0                  ! Lower Limit for bound checking
     :   , 1000.0)              ! Upper Limit for bound checking
 
      call read_real_var (
     :     section_name         ! Section header
     :   , 'slope'              ! Keyword
     :   , '()'                 ! Units
     :   , p_slope              ! Variable
     :   , num_read             ! Number of values returned
     :   , 0.0                  ! Lower Limit for bound checking
     :   , 100.0)               ! Upper Limit for bound checking
 
      call read_real_var (
     :     section_name         ! Section header
     :   , 'slope_length'       ! Keyword
     :   , '()'                 ! Units
     :   , p_slope_length       ! Variable
     :   , num_read             ! Number of values returned
     :   , 0.0                  ! Lower Limit for bound checking
     :   , 100.0)               ! Upper Limit for bound checking
 
      call read_real_var (
     :     section_name         ! Section header
     :   , 'bed_depth'          ! Keyword
     :   , '()'                 ! Units
     :   , g_bed_depth          ! Variable
     :   , num_read             ! Number of values returned
     :   , 0.0                  ! Lower Limit for bound checking
     :   , 5000.0)              ! Upper Limit for bound checking
 
cPdeV - used for fudges
c      call read_real_var (
c     :     section_name         ! Section header
c     :   , 'crop_cover_wtg'     ! Keyword
c     :   , '()'                 ! Units
c     :   , p_crop_cover_wtg     ! Variable
c     :   , num_read             ! Number of values returned
c     :   , 0.0                  ! Lower Limit for bound checking
c     :   , 1.0)                 ! Upper Limit for bound checking
 
      call read_real_var_optional (
     :     section_name         ! Section header
     :   , 'cover_extra'        ! Keyword
     :   , '()'                 ! Units
     :   , g_cover_extra        ! Variable
     :   , num_read             ! Number of values returned
     :   , -1.0                 ! Lower Limit for bound checking
     :   , 1.0)                 ! Upper Limit for bound checking
 
                                ! model specific parameters..
      if (p_model_type .eq. freeb_model) then
 
         call read_real_var_optional (
     :        section_name      ! Section header
     :      , 'k_factor'        ! Keyword
     :      , '()'              ! Units
     :      , p_k_factor_bed    ! Variable
     :      , num_read          ! Number of values returned
     :      , 0.0               ! Lower Limit for bound checking
     :      , 1.0)              ! Upper Limit for bound checking
         if (num_read .le. 0) then
           call read_real_var (
     :          section_name      ! Section header
     :        , 'k_factor_bed'    ! Keyword
     :        , '()'              ! Units
     :        , p_k_factor_bed    ! Variable
     :        , num_read          ! Number of values returned
     :        , 0.0               ! Lower Limit for bound checking
     :        , 1.0)              ! Upper Limit for bound checking
           call read_real_var (
     :          section_name      ! Section header
     :        , 'k_factor_susp'   ! Keyword
     :        , '()'              ! Units
     :        , p_k_factor_susp   ! Variable
     :        , num_read          ! Number of values returned
     :        , 0.0               ! Lower Limit for bound checking
     :        , 1.0)              ! Upper Limit for bound checking
         else
            ! Nothing
         endif
         call read_real_var (
     :        section_name      ! Section header
     :      , 'p_factor'        ! Keyword
     :      , '()'              ! Units
     :      , p_p_factor        ! Variable
     :      , num_read          ! Number of values returned
     :      , 0.0               ! Lower Limit for bound checking
     :      , 1.0)              ! Upper Limit for bound checking
 
      else if (p_model_type .eq. rose_model) then
 
         call read_real_var_optional (
     :        section_name      ! Section header
     :      , 'entrain_eff'     ! Keyword
     :      , '()'              ! Units
     :      , p_entrain_eff_bed ! Variable
     :      , num_read_eteff    ! Number of values returned
     :      , 0.0               ! Lower Limit for bound checking
     :      , 2.0)              ! Upper Limit for bound checking
         call read_real_var_optional (
     :        section_name      ! Section header
     :      , 'eros_rose_b2'    ! Keyword
     :      , '()'              ! Units
     :      , p_eros_rose_b2_bed ! Variable
     :      , num_read_b2       ! Number of values returned
     :      , 0.01              ! Lower Limit for bound checking
     :      , 0.2)              ! Upper Limit for bound checking
 
         if (num_read_eteff .ne. 1 .or. num_read_b2 .ne. 1) then
           call read_real_var (
     :          section_name      ! Section header
     :        , 'entrain_eff_bed'     ! Keyword
     :        , '()'              ! Units
     :        , p_entrain_eff_bed ! Variable
     :        , num_read_eteff    ! Number of values returned
     :        , 0.0               ! Lower Limit for bound checking
     :        , 2.0)              ! Upper Limit for bound checking
           call read_real_var (
     :          section_name      ! Section header
     :        , 'eros_rose_b2_bed' ! Keyword
     :        , '()'              ! Units
     :        , p_eros_rose_b2_bed ! Variable
     :        , num_read_b2       ! Number of values returned
     :        , 0.01              ! Lower Limit for bound checking
     :        , 0.2)              ! Upper Limit for bound checking
           call read_real_var (
     :          section_name      ! Section header
     :        , 'entrain_eff_susp'     ! Keyword
     :        , '()'              ! Units
     :        , p_entrain_eff_susp ! Variable
     :        , num_read_eteff    ! Number of values returned
     :        , 0.0               ! Lower Limit for bound checking
     :        , 2.0)              ! Upper Limit for bound checking
           call read_real_var (
     :          section_name      ! Section header
     :        , 'eros_rose_b2_susp' ! Keyword
     :        , '()'              ! Units
     :        , p_eros_rose_b2_susp ! Variable
     :        , num_read_b2       ! Number of values returned
     :        , 0.01              ! Lower Limit for bound checking
     :        , 0.2)              ! Upper Limit for bound checking
         else
             ! Nothing - we're not splitting soil loss.
         endif
      else
                                ! nothing - unknown model type. A fatal error will be sent.
      endif
 
      call pop_routine (my_name)
      return
      end
* ====================================================================
      subroutine erosion_zero_variables ()
* ====================================================================
 
*   Short description:
*     Set all variables in this module to zero.
 
*   Assumptions:
*     None
 
*   Notes:
*     none
 
*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none
 
*   Changes:
*     DMS 25/02/94 (new template)
 
*   Calls:
*     pop_routine
*     push_routine
 
* ----------------------- Declaration section ------------------------
      implicit none
 
*   Subroutine arguments
*     none
 
*   Global variables
      include 'erosion.inc'     ! erosion common blocks
 
      dll_import pop_routine                      
      dll_import push_routine                     
 
*   Internal variables
*     none
 
*   Constant values
      character my_name*(*)
      parameter (my_name = 'erosion_zero_variables')
 
*   Initial data values
*     none
 
* --------------------- Executable code section ----------------------
      call push_routine (my_name)
 
      call erosion_zero_daily_variables ()
 
c      p_crop_cover_wtg       = 0.0
      p_entrain_eff_bed      = 0.0
      p_eros_rose_b2_bed     = 0.0
      p_entrain_eff_susp     = 0.0
      p_eros_rose_b2_susp    = 0.0
      p_minimum_depth        = 0.0
      p_model_type           = 0
      p_profile_reduction    = 0
      p_profile_layer_merge  = 0.0
 
      p_slope        = 0.0
      p_slope_length = 0.0
      p_ls_factor    = 0.0
      p_k_factor_bed = 0.0
      p_k_factor_susp= 0.0
      p_p_factor     = 0.0
      p_layer_merge_mm = 0.0
 
      g_bed_depth    = 0.0
      g_runoff       = 0.0
      g_soil_loss_bed  = 0.0
      g_soil_loss_susp = 0.0
      g_day_of_year  = 0
      g_erosion_cover= 0.0
      g_year         = 0
      g_cover_extra  = 0.0
c      g_contact_cover= 0.0
c      g_total_cover  = 0.0
 
      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine erosion_zero_daily_variables ()
*     ===========================================================
 
*   Short description:
*       zero erosion daily variables & arrays
 
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
*     210498 pdev added profile resets here due to stale data left in dlayer
*                 after an entire layer was eroded.
*   Calls:
*     pop_routine
*     push_routine
 
* ----------------------- Declaration section ------------------------
 
      implicit none
 
*   Subroutine arguments
*       none
 
*   Global variables
      include   'erosion.inc'
 
      dll_import pop_routine                      
      dll_import fill_real_array                  
      dll_import push_routine                     
 
*   Internal variables
*       none
 
*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'erosion_zero_daily_variables')
 
*   Initial data values
*       none
 
* --------------------- Executable code section ----------------------
 
      call push_routine (my_name)
 
          !  zero pools etc.
      call fill_real_array (g_dlayer, 0.0, max_layer)
      call fill_real_array (g_dlt_dlayer, 0.0, max_layer)
      call fill_real_array (g_bd, 0.0, max_layer)
 
      g_soil_loss_bed = 0.0
      g_soil_loss_susp = 0.0
c      g_crop_cover = 0.0
c      g_basal_cover = 0.0
c      g_resid_cover = 0.0
 
 
      call pop_routine (my_name)
      return
      end
* ====================================================================
      subroutine erosion_get_other_variables ()
* ====================================================================
 
*   Short description:
*     Get the values of variables from other modules
 
*   Assumptions:
*     None
 
*   Notes:
*     none
 
*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none
 
*   Changes:
*     DMS 25/02/94 (new template)
*     PdeV 27/08/94
 
*   Calls:
*     get_integer_var
*     get_real_var
*     get_real_array
*     pop_routine
*     push_routine
 
* ----------------------- Declaration section ------------------------
      implicit none
 
*   Subroutine arguments
*     none
 
*   Global variables
      include   'const.inc'            ! Constant definitions
      include   'erosion.inc'          ! erosion common block
 
      real bound
      dll_import bound
 
      dll_import pop_routine                      
      dll_import get_real_array                   
      dll_import get_real_var                     
      dll_import get_integer_var                  
      dll_import push_routine                     
 
*   Internal variables
c      real       visible_contact_cover
      integer   numvals
      real      total_cover
 
*   Constant values
      character  my_name*(*)
      parameter (my_name = 'erosion_get_other_variables')
 
*   Initial data values
*     none
 
* --------------------- Executable code section ----------------------
      call push_routine (my_name)
 
                                ! Get Year
      call get_integer_var(
     :     unknown_module       ! Module that responds (Not Used)
     :   , 'year'               ! Variable Name
     :   , '()'                 ! Units                (Not Used)
     :   , g_year               ! Variable
     :   , numvals              ! Number of values returned
     :   , 0                    ! Lower Limit for bound checking
     :   , 2000  )              ! Upper Limit for bound checking
 
      call get_integer_var(
     :     unknown_module       ! Module that responds (Not Used)
     :   , 'day'                ! Variable Name
     :   , '()'                 ! Units                (Not Used)
     :   , g_day_of_year        ! Variable
     :   , numvals              ! Number of values returned
     :   , 0                    ! Lower Limit for bound checking
     :   , 366  )               ! Upper Limit for bound checking
 
                                ! Get runoff
      call Get_real_var(
     :     unknown_module       ! Module that responds (Not Used)
     :   , 'runoff'             ! Variable Name
     :   , '(mm)'               ! Units                (Not Used)
     :   , g_runoff             ! Variable
     :   , numvals              ! Number of values returned
     :   , 0.0                  ! Lower Limit for bound checking
     :   , 1000.0)              ! Upper Limit for bound checking
 
      call Get_real_var (
     :     unknown_module       ! Module that responds (Not Used)
     :   , 'total_cover'        ! Variable Name
     :   , '()'                 ! Units                (Not Used)
     :   , total_cover          ! Variable
     :   , numvals              ! Number of values returned
     :   , 0.0                  ! Lower Limit for bound checking
     :   , 1.0 )                ! Upper Limit for bound checking
 
      g_erosion_cover = bound(total_cover + g_cover_extra, 0.0, 1.0)
 
c$$$c      use this for dms' special covers..
c$$$
c$$$      call Get_real_var(
c$$$     :     unknown_module       ! Module that responds (Not Used)
c$$$     :   , 'crop_cover'         ! Variable Name
c$$$     :   , '()'                 ! Units                (Not Used)
c$$$     :   , g_crop_cover         ! Variable
c$$$     :   , numvals              ! Number of values returned
c$$$     :   , 0.0                  ! Lower Limit for bound checking
c$$$     :   , 1.0)                 ! Upper Limit for bound checking
c$$$
c$$$      call Get_real_var(
c$$$     :     unknown_module       ! Module that responds (Not Used)
c$$$     :   , 'resid_cover'        ! Variable Name
c$$$     :   , '()'                 ! Units                (Not Used)
c$$$     :   , g_resid_cover        ! Variable
c$$$     :   , numvals              ! Number of values returned
c$$$     :   , 0.0                  ! Lower Limit for bound checking
c$$$     :   , 1.0)                 ! Upper Limit for bound checking
c$$$
c$$$      call Get_real_var(
c$$$     :     unknown_module       ! Module that responds (Not Used)
c$$$     :   , 'basal_cover'        ! Variable Name
c$$$     :   , '()'                 ! Units                (Not Used)
c$$$     :   , g_basal_cover        ! Variable
c$$$     :   , numvals              ! Number of values returned
c$$$     :   , 0.0                  ! Lower Limit for bound checking
c$$$     :   , 1.0)                 ! Upper Limit for bound checking
c$$$
c$$$      g_contact_cover = g_resid_cover + g_basal_cover +
c$$$     :     g_cover_extra
c$$$
c$$$      visible_contact_cover = g_contact_cover *
c$$$     :     (1.0 - g_crop_cover * p_crop_cover_wtg)
c$$$
c$$$      g_erosion_cover = bound(visible_contact_cover +
c$$$     :     g_crop_cover * p_crop_cover_wtg, 0.0, 1.0)
      call get_real_array(
     :     unknown_module       ! Module that responds (Not Used)
     :   , 'dlayer'             ! Variable Name
     :   , max_layer            ! size of array
     :   , '(mm)'               ! Units                (Not Used)
     :   , g_dlayer             ! Variable
     :   , numvals              ! Number of values returned
     :   , 0.0                  ! Lower Limit for bound checking
     :   , 2000.0)              ! Upper Limit for bound checking
 
      call get_real_array(
     :     unknown_module       ! Module that responds (Not Used)
     :   , 'bd'                 ! Variable Name
     :   , max_layer            ! size of array
     :   , '(kg/m^3)'           ! Units                (Not Used)
     :   , g_bd                 ! Variable
     :   , numvals              ! Number of values returned
     :   , 0.0                  ! Lower Limit for bound checking
     :   , 10.0)               ! Upper Limit for bound checking
 
      call pop_routine (my_name)
      return
      end
* ====================================================================
      subroutine erosion_set_my_variable (variable_name)
* ====================================================================
 
*   Short description:
*     Set the values of my variables from other modules
 
*   Assumptions:
*     None
 
*   Notes:
*     none
 
*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none
 
*   Changes:
*     DMS 25/02/94 (new template)
*     PdeV 27/08/94
*      011195 jngh  added call to message_unused
*      090696 jngh changed respond2set to collect
 
*   Calls:
*     message_unused
*     collect_real_var
*     pop_routine
*     push_routine
 
* ----------------------- Declaration section ------------------------
      implicit none
 
*   Subroutine arguments
      character  variable_name*20      ! (INPUT)
 
*   Global variables
      include   'erosion.inc'          ! erosion common block
 
      dll_import pop_routine                      
      dll_import message_unused                   
      dll_import collect_real_var                 
      dll_import push_routine                     
 
*   Internal variables
      integer    numvals
      character*20 units               ! units of variable received
 
*   Constant values
      character my_name*(*)
      parameter (my_name = 'erosion_set_my_variable')
 
*   Initial data values
*     none
 
* --------------------- Executable code section ----------------------
      call push_routine (my_name)
 
      if (variable_name .eq. 'cover_extra') then
 
         call collect_real_var (
     :       variable_name      ! Name of Variable  (not used)
     :      , units             ! Units of variable (not used)
     :      , g_cover_extra     ! Variable
     :      , numvals
     :      , -1.0
     :      , 1.0)
 
      else
             ! nothing
         call Message_unused ()
      endif
 
      call pop_routine (my_name)
      return
      end
* ====================================================================
      subroutine erosion_set_other_variables ()
* ====================================================================
 
*   Short description:
*     Update variables owned by other modules.
 
*   Assumptions:
*     None
 
*   Notes:
*     none
 
*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none
 
*   Changes:
*     DMS 25/02/94 (New template)
*     300695 jngh changed number of values sent from max_layer to numvals
*     090696 nih  changed set calls to post_var constructs
 
*   Calls:
*     count_of_real_vals
*     post_real_array
*     pop_routine
*     push_routine
 
* ----------------------- Declaration section ------------------------
 
      implicit none
 
*   Subroutine arguments
*     none
 
*   Global variables
      include   'const.inc'
      include   'erosion.inc'          ! erosion common block
 
      integer    count_of_real_vals    ! function
      dll_import count_of_real_vals
 
      dll_import pop_routine                      
      dll_import delete_postbox                   
      dll_import message_send_immediate           
      dll_import post_real_array                  
      dll_import new_postbox                      
      dll_import push_routine                     
 
*   Internal variables
      integer    num_layers
 
*   Constant values
      character  my_name*(*)
      parameter (my_name = 'erosion_set_other_variables')
 
*   Initial data values
*     none
 
* --------------------- Executable code section ----------------------
      call push_routine (my_name)
 
         ! no soil loss -> no profile change
      if ((g_soil_loss_bed + g_soil_loss_susp) .gt. 0.0 .and.
     :     p_profile_reduction .eq. on) then
 
         call new_postbox()
c$$$         call post_real_array (
c$$$     :          'dlayer'
c$$$     :        , '(mm)'
c$$$     :        , g_dlayer
c$$$     :        , max_layers)
 
         num_layers = count_of_real_vals (g_dlayer, max_layer)
 
         call post_real_array (
     :          'dlt_dlayer'
     :        , '(mm)'
     :        , g_dlt_dlayer
     :        , num_layers)      ! trailing 0s
 
         call message_send_immediate (unknown_module
     :                               ,MES_set_variable
     :                               ,'dlt_dlayer')
         call delete_postbox()
 
      else
         ! nothing
      endif
 
      call pop_routine (my_name)
      return
      end
* ====================================================================
      subroutine erosion_send_my_variable (variable_name)
* ====================================================================
 
*   Short description:
*     Return the value of one of our variables to caller
 
*   Assumptions:
*     None
 
*   Notes:
*     none
 
*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none
 
*   Changes:
*     DMS 25/02/94 (new template)
*     PdeV. 27/08/94
*      011195 jngh  added call to message_unused
 
*   Calls:
*     divide
*     count_of_real_vals
*     message_unused
*     sum_real_array
*     pop_routine
*     push_routine
 
* ----------------------- Declaration section ------------------------
      implicit none
 
*   Subroutine arguments
      character  variable_name*(*)     ! (INPUT) variable name to search for
 
*   Global variables
      include   'convert.inc'          ! t2g, ha2sqcm, cm2mm
      include   'erosion.inc'          ! erosion Common block
 
      real       divide                ! function
      dll_import divide
 
      dll_import pop_routine                      
      dll_import message_unused                   
      dll_import respond2get_real_var             
      dll_import push_routine                     
 
*   Internal variables
      real       soil_loss_tha         ! soil loss from surface (t/ha)
      real       soil_loss_mm          ! soil loss from surface (mm)
      real       sed_conc              ! sediment concentration (g/l)
 
*   Constant values
      character my_name*(*)
      parameter (my_name = 'erosion_send_my_variable')
 
*   Initial data values
*     none
 
* --------------------- Executable code section ----------------------
 
      call push_routine (my_name)
 
      soil_loss_tha = g_soil_loss_bed + g_soil_loss_susp
 
      if (Variable_name .eq. 'soil_loss') then
         call respond2get_real_var (variable_name
     :                             , '(t/ha)'
     :                             , soil_loss_tha)
 
      else if (Variable_name .eq. 'soil_loss_bed') then
         call respond2get_real_var (variable_name
     :                             , '(t/ha)'
     :                             , g_soil_loss_bed)
 
      else if (Variable_name .eq. 'soil_loss_susp') then
         call respond2get_real_var (variable_name
     :                             , '(t/ha)'
     :                             , g_soil_loss_susp)
 
      else if (Variable_name .eq. 'soil_loss_mm') then
         soil_loss_mm = divide (soil_loss_tha * t2g/ha2scm
     :                        , g_bd(1), 0.0)
     :                * cm2mm
 
         call respond2get_real_var (variable_name
     :                             , '(mm)'
     :                             , soil_loss_mm)
 
      else if (Variable_name .eq. 'sed_conc') then
         sed_conc = divide (soil_loss_tha * t2g/ha2sm
     :                    , g_runoff * mm2lpsm, 0.0)
         call respond2get_real_var (variable_name
     :                             , '(g/l)'
     :                             , sed_conc)
 
      else if (Variable_name .eq. 'sed_conc_bed') then
         sed_conc = divide (g_soil_loss_bed * t2g/ha2sm
     :                    , g_runoff * mm2lpsm, 0.0)
         call respond2get_real_var (variable_name
     :                             , '(g/l)'
     :                             , sed_conc)
 
      else if (Variable_name .eq. 'sed_conc_susp') then
         sed_conc = divide (g_soil_loss_susp * t2g/ha2sm
     :                    , g_runoff * mm2lpsm, 0.0)
         call respond2get_real_var (variable_name
     :                             , '(g/l)'
     :                             , sed_conc)
 
      else if (Variable_name .eq. 'bed_depth') then
         call respond2get_real_var (variable_name
     :                             , '(mm)'
     :                             , g_bed_depth )
 
      else if (Variable_name .eq. 'erosion_cover') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g_erosion_cover)
 
      else if (Variable_name .eq. 'cover_extra') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g_cover_extra)
 
      else
            ! nothing
         call Message_unused ()
      endif
 
      call pop_routine (my_name)
      return
      end
* ====================================================================
      subroutine erosion_process ( )
* ====================================================================
 
*   Short description:
*     Perform actions for current day.
 
*   Assumptions:
*      None
 
*   Notes:
 
 
*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none
 
*   Changes:
*     DMS 25/02/94 (new template)
*     PdeV. 28/08/94
 
*   Calls:
*     erosion_freeb
*     erosion_rose
*     count_of_real_vals
*     erosion_depth_loss
*     erosion_move_profile
*     pop_routine
*     push_routine
 
* ----------------------- Declaration section ------------------------
      implicit none
 
*   Subroutine arguments
*     none
 
*   Global variables
      include   'const.inc'       ! Constant definitions
      include   'erosion.inc'     ! erosion common block
 
      dll_import pop_routine                      
      dll_import push_routine                     
 
*   Internal variables
*      none
 
*   Constant values
      character  my_name*(*)
      parameter (my_name = 'erosion_process')
 
*   Initial data values
*      none
 
* --------------------- Executable code section ----------------------
      call push_routine (my_name)
 
      g_soil_loss_bed = 0.0
      g_soil_loss_susp = 0.0
 
      if (p_model_type .eq. freeb_model) then
         call erosion_freeb (g_soil_loss_bed, g_soil_loss_susp)
 
      else if (p_model_type .eq. rose_model) then
         call erosion_rose (g_soil_loss_bed, g_soil_loss_susp)
 
      else
      endif
 
      if ((g_soil_loss_bed + g_soil_loss_susp .gt. 0.0) .and.
     :    p_profile_reduction .eq. on) then
            ! move profile
         call erosion_move_profile ()
      else
         ! nothing
      endif
 
      call pop_routine (my_name)
      return
      end
 
*     ===========================================================
      subroutine erosion_freeb (bed_loss, susp_loss)
*     ===========================================================
 
*   Short description:
*     Freebairn cover-sediment concentration model
*     from PERFECT. returns t/ha bed and suspended loss
 
*   Assumptions:
*       none
 
*   Notes:
*       none
 
*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none
 
*   Changes:
*     PdeV. 28/08/94
 
*   Calls:
*     pop_routine
*     push_routine
 
* ----------------------- Declaration section ------------------------
      implicit none
 
*   Subroutine arguments
      real      bed_loss               ! (OUTPUT) soil loss in bed load (t/ha)
      real      susp_loss              ! (OUTPUT) soil loss in suspended load (t/ha)
 
*   Global variables
      include   'convert.inc'          ! fract2pcnt
      include   'erosion.inc'
 
      dll_import pop_routine                      
      dll_import push_routine                     
 
*   Internal variables
      real       erosion_cover_pcnt    ! erosion cover percent
      real       sed_conc              ! sediment concentration (%)
                                       ! ie. g soil/g water *100
 
*   Constant values
      character  my_name*(*)
      parameter (my_name = 'erosion_freeb')
 
*   Initial data values
*     none
 
* --------------------- Executable code section ----------------------
      call push_routine (my_name)
 
      erosion_cover_pcnt = g_erosion_cover * fract2pcnt
      if (g_erosion_cover.lt.0.5)then
         sed_conc = 16.52
     :            - 0.46 * erosion_cover_pcnt
     :            + 0.0031 * erosion_cover_pcnt * erosion_cover_pcnt
 
      else
         sed_conc = 2.54 - 0.0254 * erosion_cover_pcnt
 
      endif
 
      bed_loss = sed_conc * pcnt2fract * g2t/(g2mm * sm2ha)
     :              * p_ls_factor * p_k_factor_bed
     :              * p_p_factor * g_runoff
 
      susp_loss = sed_conc * pcnt2fract * g2t/(g2mm * sm2ha)
     :              * p_ls_factor * p_k_factor_susp
     :              * p_p_factor * g_runoff
 
cjh      erosion_freeb = sed_conc
cjh     :              * p_ls_factor * p_k_factor
cjh     :              * p_p_factor * g_runoff  / 10.0
cjh       (100*g/(1000*1000))/(g*1000/1000000) *mm  -> t/ha
 
      call pop_routine (my_name)
      return
      end
 
*     ===========================================================
      subroutine erosion_rose (bed_loss, susp_loss)
*     ===========================================================
 
*   Short description:
*     Simplified rose model from PERFECT
*     returns t/ha bed and suspended loads
 
*   Assumptions:
*       none
 
*   Notes:
*******************************************************************
*                                                                 *
*  This subroutine calculates soil loss using the simplified Rose *
*  algorithm.                                                     *
*     apsim         perfect   descr                               *
*     ---           ---       ------------                        *
*     total_cover - covm   -  mulch cover     ( 0 - 1)            *
*     entrain_eff - kusle  -  efficiency of entrainment (bare conditions)*
*     runoff      - runf   -  event runoff (mm)                   *
*     (returned)  - sed    -  soil loss (t/ha)                    *
*     slope       - aslope -  slope (%)                           *
*                                                                 *
*******************************************************************
 
*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none
 
*   Changes:
*     PdeV. 28/08/94
 
*   Calls:
*     pop_routine
*     push_routine
 
* ----------------------- Declaration section ------------------------
      implicit none
 
*   Subroutine arguments
      real      bed_loss               ! (OUTPUT) soil loss in bed load (t/ha)
      real      susp_loss              ! (OUTPUT) soil loss in suspended load (t/ha)
 
*   Global variables
      include   'convert.inc'
      include   'erosion.inc'
 
      dll_import pop_routine                      
      dll_import push_routine                     
 
*   Internal variables
      real       lambda_bed                ! efficiency of entrainment ?
      real       lambda_susp                ! efficiency of entrainment ?
 
*   Constant values
      character  my_name*(*)
      parameter (my_name = 'erosion_rose')
 
*   Initial data values
*     none
 
* --------------------- Executable code section ----------------------
      call push_routine (my_name)
 
      lambda_bed = p_entrain_eff_bed
     :       * exp (- p_eros_rose_b2_bed * g_erosion_cover * fract2pcnt)
 
      bed_loss = 2700.0 * (p_slope * pcnt2fract)
     :             * (1.0 - g_erosion_cover)
     :             * lambda_bed * g_runoff / 100.0
 
 
      lambda_susp = p_entrain_eff_susp
     :       * exp (- p_eros_rose_b2_susp *
     :               g_erosion_cover * fract2pcnt)
 
      susp_loss = 2700.0 * (p_slope * pcnt2fract)
     :             * (1.0 - g_erosion_cover)
     :             * lambda_susp * g_runoff / 100.0
 
c      erosion_rose = 2700.0 * (p_slope * pcnt2fract)
c     :             * (1.0 - g_erosion_cover)
c     :             * lambda * g_runoff / 100.0
cjh           what is the unit conversion here???
 
      call pop_routine (my_name)
      return
      end
 
*     ================================================================
      subroutine erosion_move_profile ()
*     ================================================================
 
*   Short description:
*     move things in the profile
 
*   Assumptions:
*     None
 
*   Notes:
*     N (ie kg/ha) variables move from top down.
*     profile is eroded from the bottom up.
 
*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none
 
*   Changes:
*     DMS 25/02/94 (new template)
*     PdeV. 28/08/94
 
*   Calls:
*     erosion_start_day_log
*     erosion_move_conc
*     erosion_move_depth
*     erosion_move_dlayr
*     count_of_real_vals
*     sum_real_array
*     pop_routine
*     push_routine
 
* ----------------------- Declaration section ------------------------
      implicit none
 
*   Subroutine arguments
*     none
 
*   Global variables
      include   'erosion.inc'          ! erosion common blocks
 
      real       sum_real_array        ! function
      dll_import sum_real_array
      integer    count_of_real_vals    ! function
      dll_import count_of_real_vals
 
      dll_import pop_routine                      
      dll_import push_routine                     
 
*   Internal variables
      integer    num_layers
      real       dlt_bed_depth
 
*   Constant values
      character  my_name*(*)
      parameter (my_name = 'erosion_move_profile')
 
*   Initial data values
*      none
 
* --------------------- Executable code section ----------------------
      call push_routine (my_name)
 
      call erosion_move_dlayr (g_dlt_dlayer, dlt_bed_depth)
 
c      write (*,*) 'xxx',g_dlt_dlayer, dlt_bed_depth
 
      num_layers = count_of_real_vals (g_dlayer, max_layer)
 
         ! was that too much?
      if (sum_real_array (g_dlayer, num_layers)
     :     + sum_real_array (g_dlt_dlayer, num_layers)
     :     .lt.  p_minimum_depth) then
 
         call erosion_bomb_run ()
      else
         ! nothing
      endif
         ! update depth to bedrock
      g_bed_depth = g_bed_depth + dlt_bed_depth
 
      call pop_routine (my_name)
      return
      end
 
*     ================================================================
      subroutine erosion_bomb_run ()
*     ================================================================
 
*   Short description:
*      kill the run
 
*   Assumptions:
*     None
 
*   Notes:
 
*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none
 
*   Changes:
*     DMS 25/02/94 (new template)
*     PdeV. 28/08/94
 
*   Calls:
*     fatal_error
*     pop_routine
*     push_routine
 
* ----------------------- Declaration section ------------------------
      implicit none
 
*   Subroutine arguments
*     none
 
*   Global variables
      include   'const.inc'            ! Constant definitions
      include   'erosion.inc'          ! erosion common block
 
      dll_import pop_routine                      
      dll_import fatal_error                      
      dll_import push_routine                     
 
*   Internal variables
*     none
 
*   Constant values
      character  my_name*(*)
      parameter (my_name = 'erosion_bomb_run')
 
*   Initial data values
*      none
 
* --------------------- Executable code section ----------------------
      call push_routine (my_name)
 
      call fatal_error (err_user, 'Out of soil to erode. Giving up.')
 
      call pop_routine (my_name)
      return
      end
 
*     ================================================================
      subroutine erosion_move_dlayr (dlt_dlayer, dlt_bed_depth)
*     ================================================================
 
*   Short description:
*     move dlayr
 
*   Assumptions:
*      None
 
*   Notes:
*     Erodes profile from bottom up.
 
*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none
 
*   Changes:
*     DMS 25/02/94 (new template)
*     PdeV. 28/08/94
 
*   Calls:
*     count_of_real_vals
*     sum_real_array
*     pop_routine
*     push_routine
 
* ----------------------- Declaration section ------------------------
      implicit none
 
*   Subroutine arguments
      real       dlt_dlayer(*)         ! (OUTPUT)
      real       dlt_bed_depth         ! (OUTPUT)
 
*   Global variables
      include   'const.inc'            ! Constant definitions
      include   'convert.inc'
      include   'erosion.inc'          ! erosion common block
 
      integer    count_of_real_vals    ! function
      dll_import count_of_real_vals
      real       sum_real_array        ! function
      dll_import sum_real_array
      real       divide                ! function
      dll_import divide
 
      dll_import pop_routine                      
      dll_import warning_error                    
      dll_import fill_real_array                  
      dll_import push_routine                     
 
*   Internal variables
      real       tot_depth
      real       overrun
      real       new_depth
      real       top                   ! temporary
      integer    num_layers
      integer    i
      real       dlt_depth_mm(max_layer) ! bd based change in depth
      character  string*80               ! message string
 
*   Constant values
      character  my_name*(*)
      parameter (my_name = 'erosion_move_dlayr')
 
*   Initial data values
*      none
 
* --------------------- Executable code section ----------------------
      call push_routine (my_name)
 
      call fill_real_array (dlt_dlayer, 0.0, max_layer)
      dlt_bed_depth = 0.0
 
      num_layers = count_of_real_vals (g_dlayer, max_layer)
 
         ! find density based change in each layer
 
      do 1000 i = 1, num_layers
         top = (g_soil_loss_bed + g_soil_loss_susp) * t2g/ha2scm
         dlt_depth_mm(i) =  divide (top
     :                            , g_bd(i), 0.0) * cm2mm
 
c     What happens when layer completely eroded?
         if (dlt_depth_mm(i) .gt. g_dlayer(i) ) then
            write (string, '(a, i4, a, i5)')
     :            'warning - trying to erode all of layer', i
     :          , ' on day', g_day_of_year
 
            call warning_error (err_user, string)
         else
            ! nothing
         endif
 1000 continue
 
 
         ! Check whether we've moved bedrock
         ! into the profile. If so, we have to change dlayer.
      tot_depth = sum_real_array(g_dlayer, num_layers)
     :          + dlt_depth_mm(num_layers)
 
      if (tot_depth .gt. g_bed_depth ) then
         overrun = tot_depth - g_bed_depth
         dlt_bed_depth = - overrun
 
         do 2000 i = num_layers, 1, -1
            if (overrun .gt. 0.0) then
                  ! yes - eroded into bedrock.
               if (overrun .le. g_dlayer(i)) then
                     ! move portion of layer
                  dlt_dlayer(i) = - overrun
                     ! find if layers merge
                  new_depth = dlt_dlayer(i) + g_dlayer(i)
                  if (new_depth .lt. p_layer_merge_mm) then
                     if (i .le. 1) then
                        call erosion_bomb_run ()
                     else
                        dlt_dlayer(i-1) = new_depth
                        dlt_dlayer(i) = - g_dlayer(i)
                        p_layer_merge_mm = g_dlayer(i - 1)
     :                                   * p_profile_layer_merge
                     endif
                  else
                     ! nothing
                  endif
                  overrun = 0.0
               else
                     ! remove entire layer
                  dlt_dlayer(i) = - g_dlayer(i)
                  overrun = overrun - g_dlayer(i)
               endif
            else
               ! nothing
            endif
 2000    continue
      endif
 
      call pop_routine (my_name)
      return
      end
 
*     ================================================================
      subroutine erosion_end_run ()
*     ================================================================
 
*   Short description:
*      Perform cleanup because the current simulation is about to end.
 
*   Assumptions:
*      None
 
*   Notes:
*      closes log file if necessary
 
*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none
 
*   Changes:
*     DMS 25/02/94 (new template)
*     PdeV. 28/08/94
 
*   Calls:
*     pop_routine
*     push_routine
 
* ----------------------- Declaration section ------------------------
      implicit none
 
*   Subroutine arguments
*      none
 
*   Global variables
      include   'erosion.inc'
 
      dll_import pop_routine                      
      dll_import push_routine                     
 
*   Internal variables
*      none
 
*   Constant values
      character  my_name*(*)
      parameter (my_name = 'erosion_end_run')
 
*   Initial data values
*     none
 
* --------------------- Executable code section ----------------------
 
      call push_routine (my_name)
 
      call pop_routine (my_name)
      return
      end
 
