*     ===========================================================
      character*(*) function irrigate_version ()
*     ===========================================================

*   Short description:
*       return version number of irrigate module

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
*       080794 jngh removed print to screen statements

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
      parameter (my_name = 'irrigate_version')

      character  version_number*(*)    ! version number of module
      parameter (version_number = 'V1.31 120996')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      irrigate_version = version_number

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine APSIM_irrigate (Action, Data_String)
*     ===========================================================

*   Short description:
*      This routine is the interface between the main system and the
*      irrigate module.

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
*     210395 jngh changed from irrigate_section to a parameters section
*      011195 jngh  added call to message_unused
*      060696 jngh removed data string from irrigate_irrigate call
*      110996 nih  changed call to prepare to inter_timestep

*   Calls:
*     irrigate_zero_variables
*     irrigate_Init
*     irrigate_Inter_timestep
*     irrigate_get_other_variables
*     irrigate_Process
*     irrigate_Post
*     irrigate_set_other_variables
*     irrigate_Send_my_variable
*     irrigate_set_my_variable
*     irrigate_end_run
*     irrigate_irrigate
*     message_unused
*     pop_routine
*     push_routine
*     set_warning_off

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      character  Action*(*)            ! Message action to perform
      character  Data_String*(*)       ! Message data

*   Global variables
      include   'const.inc'            ! Global constant definitions
      include   'irrigate.inc'         ! irrigate common block

      character  irrigate_version*20   ! function
      integer    lastnb                ! function

*   Internal variables
      character  Module_name*10        ! name of module

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'irrigate')

*   Initial data values
*      None

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

         ! initialise error flags
      call set_warning_off ()

      if (Action.eq.MES_Presence) then
         Call Get_Current_Module (Module_Name)
         write (*, *) 'module_name = '
     :              , module_name(:lastnb (module_name))
     :              // blank
     :              // irrigate_version ()

      else if (Action.eq.MES_Init) then
         call irrigate_zero_variables ()
         call irrigate_Init ()

      else if (Action.eq.MES_Inter_Timestep) then
         call irrigate_Inter_Timestep()

      else if (Action.eq.MES_Process) then
         call irrigate_get_other_variables ()
         call irrigate_process ()

      else if ((Action.eq.'irrigate').or.(Action.eq.'apply')) then
         call irrigate_get_other_variables ()
         call irrigate_irrigate ()

      else if (Action.eq.MES_Get_variable) then
         call irrigate_Send_my_variable (Data_String)

      else if (Action .eq. MES_Set_variable) then
         call irrigate_set_my_variable (Data_String)

      else
            ! Don't use message
         call Message_unused ()

      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine irrigate_irrigate ()
*     ===========================================================

*   Short description:
*      This routine is determines the irigation parameters and
*      irrigates.

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
*     110395 jngh moved messaging to to apply routine
*     110496 nih  upgraded routine to use the postbox calls
*     060696 jngh changed extract to collect routines
*                 removed data string from argument
*                 implemented postbox method for data transfer
*     110996 nih  added increment for g_irr_applied

*   Calls:
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*      None

*   Global variables
      include   'const.inc'            ! Global constant definitions
      include   'irrigate.inc'         ! irrigate common block

*   Internal variables
      real       amount                ! amount of irrigation to apply
      integer    numvals               ! number of values collected
      integer    numvals_solute(max_solutes) ! number of values collected for
                                       ! each solute
      real       solute(max_solutes)   ! amount of solute in irrigation ()
      integer    solnum                ! solute number counter variable
      character  time*10               !
      real       duration              !

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'irrigate_irrigate')

*   Initial data values
*      None

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      ! Look for all irrigation information
      ! -----------------------------------

      Call collect_real_var (
     :                       'amount'
     :                     , '(mm)'
     :                     , amount
     :                     , numvals
     :                     , 0.0
     :                     , 1000.0)
      if (numvals.gt.0) then
      else
         call fatal_error (err_user
     :                   , 'Irrigation amount not specified correctly')
      endif

      Call collect_real_var_optional (
     :                       'duration'
     :                     , '(min)'
     :                     , duration
     :                     , numvals
     :                     , 0.0
     :                     , 1440.0)

      if (numvals .eq. 0) then
            !set default
         duration = g_irr_duration_def
      else
          ! got a value
      endif

      Call collect_char_var_optional (
     :                       'time'
     :                     , '(hh:mm)'
     :                     , time
     :                     , numvals)

      if (numvals .eq. 0) then
            !set default
         time = g_irr_time_def
      else
          ! got a value
      endif


      ! look for any solute information in the postbox
      ! ----------------------------------------------
      do 100 solnum = 1, g_num_solutes
         Call collect_real_var_optional (
     :                       g_solute_names(solnum)
     :                     , '(kg/ha)'
     :                     , solute(solnum)
     :                     , numvals_solute(solnum)
     :                     , 0.0
     :                     , 1000.0)

  100 continue

      call new_postbox ()

         ! send message regardless of fatal error - will stop anyway

      call post_real_var   ('amount'
     :                        ,'(mm)'
     :                        , amount)

      call post_real_var   ('duration'
     :                        ,'(min)'
     :                        , duration)

      call post_char_var   ('time'
     :                        ,'(hh:mm)'
     :                        , time)

      do 200 solnum = 1, g_num_solutes
         if (numvals_solute(solnum) .ne.0) then
            call post_real_var   (g_solute_names(solnum)
     :                           ,'(kg/ha)'
     :                           , solute(solnum))
         else
         endif
200   continue

      call message_send_immediate (unknown_module, 'add_water', blank)

      call delete_postbox ()

      g_irr_applied = g_irr_applied + amount

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine irrigate_Init ()
*     ===========================================================

*   Short description:
*      Initialise irrigate module

*   Assumptions:
*      None

*   Notes:
*       none

*   Procedure attributes:
*      Version:         Any hardwfare/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:

*   Calls:
*     irrigate_version
*     irrigate_get_other_variables
*     report_event

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*      none

*   Global variables
      include   'const.inc'            ! Constant definitions
      include   'irrigate.inc'         ! irrigate model common

      character  irrigate_version*15   ! function
      integer    count_of_integer_vals ! function
      logical    reals_are_equal

*   Internal variables
      integer    Counter               ! simple counter variable
      character  Event_string*79       ! String to output
      integer    num_irrigs            ! no. of irrigation applications
      character  String*79             ! String to output

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'irrigate_init')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      call irrigate_get_other_variables ()

         ! Notify system that we have initialised

      Event_string = ' Initialising, Version : ' // irrigate_version ()
      call report_event (Event_string)

         ! Get all parameters from parameter file

      call irrigate_read_param ()

      num_irrigs = count_of_integer_vals (g_irr_day, max_irrigs)
      If (num_irrigs .gt. 0) then
         call write_string (lu_scr_sum, new_line//new_line)

         string = '                 Irrigation Schedule'
         call write_string (lu_scr_sum, string)

         string = '     -----------------------------------------------'
         call write_string (lu_scr_sum, string)

         string = '               day     year    amount (mm)'
         call write_String (LU_Scr_sum, string)
         string = '     -----------------------------------------------'
         call write_String (LU_scr_sum, string)

         do 100 counter = 1, num_irrigs
            write (string,'(14x, i5, i6, f8.2)')
     :                                  g_irr_day(counter)
     :                                , g_irr_year(counter)
     :                                , g_irr_amount(counter)
            call write_string (LU_Scr_sum, String)
  100    continue
         string = '     -----------------------------------------------'
         call write_String (LU_scr_sum, string)
      Else
      Endif

      call write_string (lu_scr_sum, new_line//new_line)

      string = '                 Irrigation parameters'
      call write_string (lu_scr_sum, string)

      string = '     -----------------------------------------------'
      call write_string (lu_scr_sum, string)

      if (g_manual_irrig .eq. 'on') then
         call write_String (LU_Summary_file,
     :                     '      Irrigation Schedule (Enabled)')
      else
         call write_String (LU_Summary_file,
     :                     '      Irrigation Schedule (Disabled)')
      endif

      if (g_auto_irrig .eq. 'on') then
         call write_String (LU_Summary_file,
     :        '      Automatic Irrigation Application (Enabled)')
      else
         call write_String (LU_Summary_file,
     :        '      Automatic Irrigation Application (Disabled)')
      endif
      If (reals_are_equal (g_crit_fr_asw, -1.0)) then
         write (string, '(a)')
     :        '      critical fraction of available soil water = '
     :     // ' not intialised'
      else
         write (string, '(a, f5.2)')
     ;        '      critical fraction of available soil water = '
     :       , g_crit_fr_asw
      endif
      call write_String (LU_Summary_file, string)

      if (reals_are_equal (g_asw_depth, -1.0)) then
         write (string, '(a)')
     :        '      depth for calculating available soil water = '
     :     // ' not initialised'
      else
         write (string, '(a, f10.2)')
     ;        '      depth for calculating available soil water = '
     :       , g_asw_depth
      endif
      call write_String (LU_Summary_file, string)

      string = '     -----------------------------------------------'
      call write_string (lu_scr_sum, string)

      call write_string(LU_Scr_Sum,
     : 'The following solutes can be added with irrigation water')
      do 200 counter=1,max_solutes
         if(g_solute_names(counter).ne.' ') then
            call write_string(LU_Scr_Sum,
     :                  '     '//g_solute_names(counter))
         else
         endif
  200 continue

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine irrigate_read_param ()
*     ===========================================================

*   Short description:
*      Read in all parameters from parameter file.

*   Assumptions:
*      None

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

*   Calls:
*      Get_param
*      Open_param_file

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*      none

*   Global variables
      include   'const.inc'
      include   'irrigate.inc'         ! irrigate model common block

      logical    reals_are_equal       ! function

*   Internal variables
      integer    counter
      integer    numvals               ! number of values read from file
      integer    solnum
      real       temp_solute(max_irrigs)! temp solute array (kg/ha)

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'irrigate_read_param')

      character  section_name*(*)
      parameter (section_name = 'parameters')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      call write_string (lu_scr_sum
     :                 ,new_line//'   - Reading Parameters')

      ! Read in the list of solutes that are allowed in irrigation
      ! water.

      call read_char_array_optional (
     :           section_name         ! Section header
     :         , 'solutes'            ! Keyword
     :         , max_solutes          ! array size
     :         , '(nnnn)'             ! Units
     :         , g_solute_names       ! Array
     :         , g_num_solutes)       ! Number of values returned


         ! Read in irrigation schedule from parameter file
         !         -------------------
      call read_integer_array_optional (
     :           section_name         ! Section header
     :         , 'day'                ! Keyword
     :         , max_irrigs           ! array size
     :         , '()'                 ! Units
     :         , g_irr_day            ! Array
     :         , numvals              ! Number of values returned
     :         , 1                    ! Lower Limit for bound checking
     :         , 366)                 ! Upper Limit for bound checking


      call read_integer_array_optional (
     :           section_name         ! Section header
     :         , 'year'               ! Keyword
     :         , max_irrigs           ! array size
     :         , '()'                 ! Units
     :         , g_irr_year           ! Array
     :         , numvals              ! Number of values returned
     :         , 1800                 ! Lower Limit for bound checking
     :         , 2000)                ! Upper Limit for bound checking


      call read_real_array_optional (
     :           section_name         ! Section header
     :         , 'amount'             ! Keyword
     :         , max_irrigs           ! array size
     :         , '(mm)'               ! Units
     :         , g_irr_amount         ! Array
     :         , numvals              ! Number of values returned
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 1000.0)              ! Upper Limit for bound checking

      call read_char_array_optional (
     :           section_name         ! Section header
     :         , 'time'               ! Keyword
     :         , max_irrigs           ! array size
     :         , '(hh:mm)'            ! Units
     :         , g_irr_time           ! Array
     :         , numvals)             ! Number of values returned

      call read_real_array_optional (
     :           section_name         ! Section header
     :         , 'duration'           ! Keyword
     :         , max_irrigs           ! array size
     :         , '(mm)'               ! Units
     :         , g_irr_duration       ! Array
     :         , numvals              ! Number of values returned
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 1000.0)              ! Upper Limit for bound checking

      call read_char_var_optional (
     :           section_name         ! Section header
     :         , 'default_time'       ! Keyword
     :         , '(hh:mm)'            ! Units
     :         , g_irr_time_def       ! Variable
     :         , numvals)             ! Number of values returned
      If (numvals.lt.1) then
         g_irr_time_def = '00:00'
      else
      endif

      call read_real_var_optional (
     :           section_name         ! Section header
     :         , 'default_duration'   ! Keyword
     :         , '(min)'              ! Units
     :         , g_irr_duration_def   ! Variable
     :         , numvals              ! Number of values returned
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 1000.)               ! Upper Limit for bound checking
      If (numvals.lt.1) then
         g_irr_duration_def = 60.*24. !i.e. 24 hours
      else
      endif

      do 100 solnum = 1, g_num_solutes
         call read_real_array_optional (
     :           section_name         ! Section header
     :         , g_solute_names(solnum) ! Keyword
     :         , max_irrigs           ! array size
     :         , '(kg/ha)'            ! Units
     :         , temp_solute          ! Variable
     :         , numvals              ! Number of values returned
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 1000.)               ! Upper Limit for bound checking
            do 50 counter=1, numvals
               g_irr_solutes(solnum,counter) = temp_solute(counter)
   50       continue

  100 continue

         ! Read in automatic irrigation info from parameter file
         !         -------------------------

      call read_real_var_optional (
     :           section_name         ! Section header
     :         , 'crit_fr_asw'        ! Keyword
     :         , '(0-1)'              ! Units
     :         , g_crit_fr_asw        ! Variable
     :         , numvals              ! Number of values returned
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 1.0)                 ! Upper Limit for bound checking


      call read_real_var_optional (
     :           section_name         ! Section header
     :         , 'asw_depth'          ! Keyword
     :         , '(mm)'               ! Units
     :         , g_asw_depth          ! Variable
     :         , numvals              ! Number of values returned
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 10000.0)             ! Upper Limit for bound checking

         ! Read in irrigation flags from parameter file
         !         ----------------

      call read_char_var_optional (
     :           section_name         ! Section header
     :         , 'manual_irrigation'  ! Keyword
     :         , '()'                 ! Units
     :         , g_Manual_irrig       ! Variable
     :         , numvals)             ! Number of values returned


      call read_char_var_optional (
     :           section_name         ! Section header
     :         , 'automatic_irrigation'  ! Keyword
     :         , '()'                 ! Units
     :         , g_auto_irrig         ! Variable
     :         , numvals)             ! Number of values returned

      if (g_auto_irrig .eq. 'on') then
         if (reals_are_equal (g_crit_fr_asw, -1.0)
     :      .or. reals_are_equal (g_asw_depth, -1.0)) then
            call fatal_error (Err_user,
     :         'Cannot initiate auto irrigation until its configuration'
     :         //' parameters are set.')
         else
         endif
      else
      endif

      if (g_manual_irrig .eq. 'on') then
         if (g_irr_day(1) .eq. 0 .or.
     :       g_irr_year(1) .eq. 0 .or.
     :       reals_are_equal (g_irr_amount(1), 0.0)) then

            call fatal_error (Err_user,
     :         'Cannot initiate manual irrigation until its'//
     :         ' configuration parameters are set.')

         else
         endif
      else
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine irrigate_zero_variables ()
*     ===========================================================

*   Short description:
*     Set all variables in this module to zero.

*   Assumptions:
*      None

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

*   Calls:
*       none

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*      none

*   Global variables
      include   'irrigate.inc'         ! irrigate common block

*   Internal variables
      integer counter
      integer solnum

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'irrigate_zero_variables')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      g_year = 0
      g_day = 0
      g_irr_applied = 0.0

      call fill_integer_array (g_irr_day, 0, max_irrigs)
      call fill_integer_array (g_irr_year, 0, max_irrigs)
      call fill_real_array  (g_irr_amount, 0.0, max_irrigs)
      call fill_real_array  (g_irr_duration, 0.0, max_irrigs)
      call fill_char_array (g_irr_time, ' ', max_irrigs)
      call fill_char_array (g_solute_names, ' ', max_solutes)

      do 200 solnum=1,max_solutes
         do 100 counter=1, max_irrigs
            g_irr_solutes(solnum,counter) = 0.0
  100    continue
  200 continue

      g_auto_irrig = 'off'
      g_manual_irrig = 'off'
      g_asw_depth = -1.0
      g_crit_fr_asw = -1.0
      g_irr_time_def = ' '
      g_irr_duration_def = 0.0
      g_irr_pointer = 1
      g_num_solutes = 0

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine irrigate_get_other_variables ()
*     ===========================================================

*   Short description:
*      Get the values of variables from other modules

*   Assumptions:
*      None

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

*   Calls:
*     get_variable_value

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*      none

*   Global variables
      include   'const.inc'            ! Constant definitions
      include   'irrigate.inc'         ! irrigate common block

*   Internal variables
      integer    numvals               ! number of values returned

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'irrigate_get_other_variables')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      call Get_integer_var (
     :      unknown_module  ! Module that responds (Not Used)
     :    , 'year'          ! Variable Name
     :    , '()'            ! Units                (Not Used)
     :    , g_year          ! Variable
     :    , numvals         ! Number of values returned
     :    , 1800            ! Lower Limit for bound checking
     :    , 2000)           ! Upper Limit for bound checking

      call Get_integer_var (
     :      unknown_module  ! Module that responds (Not Used)
     :    , 'day'           ! Variable Name
     :    , '()'            ! Units                (Not Used)
     :    , g_day           ! Variable
     :    , numvals         ! Number of values returned
     :    , 0               ! Lower Limit for bound checking
     :    , 366)            ! Upper Limit for bound checking


      call Get_real_array (
     :      unknown_module  ! Module that responds (Not Used)
     :    , 'sw_dep'        ! Variable Name
     :    , max_layer       ! size of array
     :    , '(mm)'          ! Units                (Not Used)
     :    , g_sw_dep        ! Variable
     :    , numvals         ! Number of values returned
     :    , 0.0             ! Lower Limit for bound checking
     :    , 1000.0)         ! Upper Limit for bound checking

      call Get_real_array (
     :      unknown_module  ! Module that responds (Not Used)
     :    , 'll15_dep'      ! Variable Name
     :    , max_layer       ! size of array
     :    , '(mm)'          ! Units                (Not Used)
     :    , g_ll15_dep      ! Variable
     :    , numvals         ! Number of values returned
     :    , 0.0             ! Lower Limit for bound checking
     :    , 1000.0)         ! Upper Limit for bound checking

      call Get_real_array (
     :      unknown_module  ! Module that responds (Not Used)
     :    , 'dul_dep'       ! Variable Name
     :    , max_layer       ! size of array
     :    , '(mm)'          ! Units                (Not Used)
     :    , g_dul_dep       ! Variable
     :    , numvals         ! Number of values returned
     :    , 0.0             ! Lower Limit for bound checking
     :    , 1000.0)         ! Upper Limit for bound checking

      call Get_real_array (
     :      unknown_module  ! Module that responds (Not Used)
     :    , 'dlayer'        ! Variable Name
     :    , max_layer       ! size of array
     :    , '(mm)'          ! Units                (Not Used)
     :    , g_dlayer        ! Variable
     :    , numvals         ! Number of values returned
     :    , 0.0             ! Lower Limit for bound checking
     :    , 1000.0)         ! Upper Limit for bound checking

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine irrigate_Send_my_variable (Variable_name)
*     ===========================================================

*   Short description:
*      Return the value of one of our variables to caller

*   Assumptions:
*      None

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
*      011195 jngh  added call to message_unused

*   Calls:
*     message_unused

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      character  Variable_name*(*)     ! (INPUT) Variable name to search for

*   Global variables
      include   'irrigate.inc'         ! irrigate Common block

*   Internal variables
*     none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'irrigate_send_my_variable')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      if (Variable_name .eq. 'irrigation') then
         call respond2get_real_var (
     :                              variable_name
     :                            , '(mm)'
     :                            , g_irr_applied)

      else
         call Message_unused ()
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine irrigate_set_my_variable (Variable_name)
*     ===========================================================

*   Short description:
*     Set one of our variables altered by some other module

*   Assumptions:
*      None

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
*      011195 jngh  added call to message_unused
*      060695 jngh changed respond2set to collect routines

*   Calls:
*     message_unused

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      character  Variable_name*(*)     ! (INPUT) Variable name to search for

*   Global variables
      include   'const.inc'
      include   'irrigate.inc'         ! irrigate common block

      logical    reals_are_equal       ! function

*   Internal variables
      integer    numvals               ! number of values returned

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'irrigate_set_my_variable')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      if (Variable_name .eq. 'manual_irrigation') then
         call collect_char_var (
     :                variable_name        ! variable name
     :              , '()'                 ! units
     :              , g_manual_irrig       ! array
     :              , numvals)             ! number of elements returned

         if (g_manual_irrig .eq. 'on') then
            if (g_irr_day(1) .eq. 0 .or.
     :          g_irr_year(1) .eq. 0 .or.
     :          reals_are_equal (g_irr_amount(1), 0.0)) then

               call fatal_error (Err_user,
     :         'Cannot initiate manual irrigation because its'//
     :         ' configuration parameters are not set.')

            else
            endif
         else
         endif

      elseif (Variable_name .eq. 'automatic_irrigation') then
         call collect_char_var (
     :                variable_name           ! variable name
     :              , '()'                    ! units
     :              , g_auto_irrig            ! array
     :              , numvals)                ! number of elements returned

         if (g_auto_irrig .eq. 'on') then
            if (reals_are_equal (g_crit_fr_asw, -1.0)
     :         .or. reals_are_equal (g_asw_depth, -1.0)) then
               call fatal_error (Err_user,
     :         'Cannot initiate auto irrigation until its configuration'
     :         //' parameters are set.')
            else
            endif
         else
         endif

      elseif (Variable_name .eq. 'crit_fr_asw') then
         call collect_real_var (
     :                variable_name     ! array name
     :              , '()'              ! units
     :              , g_crit_fr_asw     ! array
     :              , numvals           ! number of elements returned
     :              , 0.0               ! lower limit for bounds checking
     :              , 1.0)              ! upper limit for bounds checking


      elseif (Variable_name .eq. 'asw_depth') then
         call collect_real_var (
     :                variable_name     ! array name
     :              , '(mm)'            ! units
     :              , g_asw_depth       ! array
     :              , numvals           ! number of elements returned
     :              , 0.0               ! lower limit for bounds checking
     :              , 1000.0)           ! upper limit for bounds checking

      else
            ! Don't know this variable name
         call Message_unused ()
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine irrigate_Process ()
*     ===========================================================

*   Short description:
*      Perform actions for current day.

*   Assumptions:
*      None

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

*   Calls:
*      growth_grow_crop

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*      none

*   Global variables
      include   'irrigate.inc'         ! irrigation common block

*   Internal variables
*      none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'irrigate_process')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      if (g_manual_irrig .eq. 'on') then
         call irrigate_schedule ()

      else
      endif

      if (g_auto_irrig .eq. 'on') then
         call irrigate_automatic ()

      else
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine irrigate_schedule ()
*     ===========================================================

*   Short description:
*       irrigation management. Informs manager that irrigation is
*       required and how much.

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
*     070694 - nih adapted from old irigat module
*     110395 jngh moved messaging to to apply routine
*     060696 jngh implemented postbox method for data transfer
*     110996 nih  added increment for g_irr_applied

*   Calls:
*      count_of_integer_vals
*      pop_routine
*      push_routine
*      report_event

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include    'const.inc'
      include    'irrigate.inc'        ! irrigation common block

      integer    count_of_integer_vals ! function

*   Internal variables
      real       amount                ! amount of irrigation to apply
      integer    irigno                ! loop counter for input
      real       MyDuration
      character  MyTime*10
      integer    num_irrigs            ! number of irrigation applications
      integer    solnum
      integer    start_irrig_no        ! index in schedule to start at

*   Constant values
      character  my_name*(*)           ! name of this procedure
      parameter (my_name = 'irrigate_schedule')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      num_irrigs = count_of_integer_vals (g_irr_day, max_irrigs)

      if (num_irrigs.gt.0) then

             ! we have a schedule.  see if we have an irrigation today.

         start_irrig_no = g_irr_pointer

         do 1000 irigno = start_irrig_no, num_irrigs
            if (g_day.eq.g_irr_day(irigno)
     :                     .and.
     :         g_year.eq.g_irr_year(irigno))
     :      then
               amount = g_irr_amount(irigno) * effirr

               if (g_irr_time(irigno).eq.blank) then
                  MyTime = g_irr_time_def
               else
                  MyTime = g_irr_Time(irigno)
               endif

               if (g_irr_Duration(irigno).eq.0.0) then
                  MyDuration = g_irr_duration_def
               else
                  MyDuration = g_irr_Duration(irigno)
               endif


               call new_postbox ()

               call post_real_var   ('amount'
     :                              ,'(mm)'
     :                              , amount)

               call post_real_var   ('duration'
     :                              ,'(min)'
     :                              , myduration)

               call post_char_var   ('time'
     :                              ,'(hh:mm)'
     :                              , mytime)

               do 200 solnum = 1, g_num_solutes
                     call post_real_var (g_solute_names(solnum)
     :                                  ,'(kg/ha)'
     :                                  , g_irr_solutes(solnum, irigno))
200            continue

               call message_send_immediate (unknown_module
     :                                     , 'add_water'
     :                                     , blank)

               call delete_postbox ()

               g_irr_applied = g_irr_applied + amount
               g_irr_pointer = irigno + 1

            else
            endif
1000     continue
      else
         ! no irrigations
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine irrigate_automatic ()
*     ===========================================================

*   Short description:
*       Automatic irrigation management.

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
*      070694 - nih adapted from jngh's old automatic irrigation module
*      040895 - jngh corrected format statement to match data types.
*      021195 jngh changed message_pass_to_module to message_send_immediate
*      060696 jngh implemented postbox method for data transfer
*      110996 nih  added increment for g_irr_applied

*   Calls:
*      count_of_integer_vals
*      get_cumulative_index_real
*      message_send_immediate
*      pop_routine
*      push_routine
*      real
*      report_event
*      sum_real_array

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'const.inc'
      include   'irrigate.inc'

      real       divide                ! function
      integer    get_cumulative_index_real ! function
      real       sum_real_array        ! function

*   Internal variables
      real       amount                ! amount of irrigation to apply (mm)
      real       avail_fr              ! fraction of avalable water in
                                       !    specified profile
      real       cumdep                ! cumulative depth in loop (mm)
      integer    nlayr                 ! number of layers
      real       swdef                 ! sw deficit (mm)
      real       avail_sw              ! total avail. sw down to specified depth
                                       ! (mm)
      real       pot_avail_sw          ! total potential avail sw down to
                                       ! specified depth (mm)
      real       excess_fr             ! fraction of excess depth below specifie
                                       ! in last layer (mm)

*   Constant values
      character  my_name*(*)           ! name of this module
      parameter (my_name = 'irrigate_automatic')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

               ! get water deficit on the spot

      nlayr = get_cumulative_index_real (g_asw_depth, g_dlayer
     :                                 , max_layer)
      cumdep = sum_real_array (g_dlayer, nlayr)

      excess_fr = divide ((cumdep - g_asw_depth) ,g_dlayer(nlayr), 0.0)

cnh note that results may be strange if swdep < ll15
      avail_sw  = (sum_real_array (g_sw_dep, nlayr)
     :          - excess_fr * g_sw_dep(nlayr))
     :          - (sum_real_array (g_ll15_dep, nlayr)
     :          - excess_fr * g_ll15_dep(nlayr))
     :          + g_irr_applied

      pot_avail_sw = (sum_real_array (g_dul_dep, nlayr)
     :             - excess_fr * g_dul_dep(nlayr))
     :             - (sum_real_array (g_ll15_dep, nlayr)
     :             - excess_fr * g_ll15_dep(nlayr))

      avail_fr = divide (avail_sw, pot_avail_sw, 0.0)
      swdef = (pot_avail_sw - avail_sw)

          ! now get automatic irrigation amount

      if (avail_fr.lt.g_crit_fr_asw) then
         amount = divide (swdef, effirr, 0.0)

         call new_postbox ()

            ! send message regardless of fatal error - will stop anyway

         call post_real_var   ('amount'
     :                        ,'(mm)'
     :                        , amount)

         call post_real_var   ('duration'
     :                        ,'(min)'
     :                        , g_irr_duration_def)

         call post_char_var   ('time'
     :                        ,'(hh:mm)'
     :                        , g_irr_time_Def)

!         No solutes in automatic irrigation just yet - later on

!         do 200 solnum = 1, g_num_solutes
!            if (numvals_solute(solnum) .ne.0) then
!               call post_real_var   (g_solute_names(solnum)
!     :                              ,'(kg/ha)'
!     :                              , solute(solnum))
!            else
!            endif
!200      continue

         call message_send_immediate (unknown_module
     :                               , 'add_water'
     :                               , blank)

         call delete_postbox ()

         g_irr_applied = g_irr_applied + amount

      else
          ! soil not dry enough to require irrigation
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine irrigate_inter_timestep ()
*     ===========================================================

*   Short description:

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

*   Calls:
*   pop_routine
*   push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*      none

*   Global variables
      include   'irrigate.inc'

*   Internal variables
*      none

*   Constant values
      character*(*) my_name            ! name of current procedure
      parameter (my_name = 'irrigate_inter_timestep')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      g_irr_applied = 0.0

      call pop_routine (my_name)
      return
      end

