*     ===========================================================
      character*(*) function irrigate_version ()
*     ===========================================================
      implicit none
      include 'error.pub'
 
*+  Purpose
*       return version number of irrigate module
 
*+  Mission Statement
*     Version number
 
*+  Changes
*       080794 jngh removed print to screen statements
 
*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'irrigate_version')
*
      character  version_number*(*)    ! version number of module
      parameter (version_number = 'V1.31 120996')
 
*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      irrigate_version = version_number
 
      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      subroutine APSIM_irrigate (Action, Data_String)
*     ===========================================================
      implicit none
      dll_export apsim_irrigate
      include   'const.inc'            ! Global constant definitions
      include   'irrigate.inc'         ! irrigate common block
      include 'string.pub'
      include 'engine.pub'
      include 'error.pub'
 
*+  Sub-Program Arguments
      character  Action*(*)            ! Message action to perform
      character  Data_String*(*)       ! Message data
 
*+  Purpose
*      This routine is the interface between the main system and the
*      irrigate module.
 
*+  Mission Statement
*     Apsim Irrigate Module
 
*+  Changes
*     210395 jngh changed from irrigate_section to a parameters section
*      011195 jngh  added call to message_unused
*      060696 jngh removed data string from irrigate_irrigate call
*      110996 nih  changed call to prepare to inter_timestep
 
*+  Calls
      character  irrigate_version*20   ! function
 
*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'irrigate')
 
*+  Local Variables
      character  Module_name*10        ! name of module
 
*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
         ! initialise error flags
      call set_warning_off ()
 
      if (Action.eq.MES_Presence) then
         Call Get_Current_Module (Module_Name)
         write (*, *) 'module_name = '
     :              , trim(module_name)
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
      implicit none
      include   'const.inc'            ! Global constant definitions
      include   'irrigate.inc'         ! irrigate common block
      include 'engine.pub'
      include 'intrface.pub'
      include 'error.pub'
      include 'write.pub'
 
*+  Purpose
*      This routine responds to an irrigate message from another
*      module.  Gets any parameters and irrigates.
 
*+  Mission Statement
*     Respond to Irrigate message
 
*+  Changes
*     110395 jngh moved messaging to to apply routine
*     110496 nih  upgraded routine to use the postbox calls
*     060696 jngh changed extract to collect routines
*                 removed data string from argument
*                 implemented postbox method for data transfer
*     110996 nih  added increment for g_irr_applied
*      160399 nih  added irrigation allocation
 
*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'irrigate_irrigate')
 
*+  Local Variables
      real       amount                ! amount of irrigation to apply
      integer    numvals               ! number of values collected
      integer    numvals_solute(max_solutes) ! number of values collected for
                                       ! each solute
      real       solute(max_solutes)   ! amount of solute in irrigation ()
      integer    solnum                ! solute number counter variable
      character  time*10               !
      real       duration              !
 
*- Implementation Section ----------------------------------
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
 
      call irrigate_check_allocation(amount)

      Call collect_real_var_optional (
     :                       'duration'
     :                     , '(min)'
     :                     , duration
     :                     , numvals
     :                     , 0.0
     :                     , 1440.0)
 
      if (numvals .eq. 0) then
            !set default
         duration = p_default_duration
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
         time = p_default_time
      else
          ! got a value
      endif
 
 
      ! look for any solute information in the postbox
      ! ----------------------------------------------
      do 100 solnum = 1, g_num_solutes
         Call collect_real_var_optional (
     :                       p_solutes(solnum)
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
            call post_real_var   (p_solutes(solnum)
     :                           ,'(kg/ha)'
     :                           , solute(solnum))
         else
         endif
200   continue
 
      call message_send_immediate (unknown_module, 'add_water', blank)
 
      call delete_postbox ()
 
      g_irrigation_applied = g_irrigation_applied + amount
 
      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      subroutine irrigate_Init ()
*     ===========================================================
      implicit none
      include   'const.inc'            ! Constant definitions
      include   'irrigate.inc'         ! irrigate model common
      include 'data.pub'
      include 'write.pub'
      include 'error.pub'
 
*+  Purpose
*      Initialise irrigate module
 
*+  Mission Statement
*     Initialise
 
*+  Changes
*     <insert here>
 
*+  Calls
      character  irrigate_version*15   ! function
 
*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'irrigate_init')
 
*+  Local Variables
      integer    Counter               ! simple counter variable
      character  Event_string*79       ! String to output
      integer    num_irrigs            ! no. of irrigation applications
      character  String*79             ! String to output
 
*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
         ! Notify system that we have initialised
 
      Event_string = ' Initialising, Version : ' // irrigate_version ()
      call report_event (Event_string)
 
         ! Get all parameters from parameter file
 
      call irrigate_read_param ()
 
      num_irrigs = count_of_integer_vals (p_day, max_irrigs)
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
     :                                  p_day(counter)
     :                                , p_year(counter)
     :                                , p_amount(counter)
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
 
      if (p_manual_irrigation .eq. 'on') then
         call write_String (LU_Summary_file,
     :                     '      Irrigation Schedule (Enabled)')
      else
         call write_String (LU_Summary_file,
     :                     '      Irrigation Schedule (Disabled)')
      endif
 
      if (p_automatic_irrigation .eq. 'on') then
         call write_String (LU_Summary_file,
     :        '      Automatic Irrigation Application (Enabled)')
      else
         call write_String (LU_Summary_file,
     :        '      Automatic Irrigation Application (Disabled)')
      endif
      If (reals_are_equal (p_crit_fr_asw, -1.0)) then
         write (string, '(a)')
     :        '      critical fraction of available soil water = '
     :     // ' not intialised'
      else
         write (string, '(a, f5.2)')
     ;        '      critical fraction of available soil water = '
     :       , p_crit_fr_asw
      endif
      call write_String (LU_Summary_file, string)
 
      if (reals_are_equal (p_asw_depth, -1.0)) then
         write (string, '(a)')
     :        '      depth for calculating available soil water = '
     :     // ' not initialised'
      else
         write (string, '(a, f10.2)')
     ;        '      depth for calculating available soil water = '
     :       , p_asw_depth
      endif
      call write_String (LU_Summary_file, string)

      if (p_irrigation_allocation .eq. 'on') then
         call write_String (LU_Summary_file,
     :        '      Irrigation Allocation Budget (Enabled)')
      else
         call write_String (LU_Summary_file,
     :        '      Irrigation Allocation Budget (Disabled)')
      endif
 
      string = '     -----------------------------------------------'
      call write_string (lu_scr_sum, string)
 
      call write_string(LU_Scr_Sum,
     : 'The following solutes can be added with irrigation water')
      do 200 counter=1,max_solutes
         if(p_solutes(counter).ne.' ') then
            call write_string(LU_Scr_Sum,
     :                  '     '//p_solutes(counter))
         else
         endif
  200 continue
 
      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      subroutine irrigate_read_param ()
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'irrigate.inc'         ! irrigate model common block
      include 'data.pub'
      include 'read.pub'
      include 'write.pub'
      include 'error.pub'
 
*+  Purpose
*      Read in all parameters from parameter file.
 
*+  Mission Statement
*     Read parameters from parameter file
 
*+  Changes
*      201097 IGH - added profile depth to bound checking
 
*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'irrigate_read_param')
*
      character  section_name*(*)
      parameter (section_name = 'parameters')
 
*+  Local Variables
      integer    counter
      integer    numvals               ! number of values read from file
      integer    solnum
      real       temp_solute(max_irrigs)! temp solute array (kg/ha)
 
*- Implementation Section ----------------------------------
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
     :         , p_solutes            ! Array
     :         , g_num_solutes)       ! Number of values returned
 
 
         ! Read in irrigation schedule from parameter file
         !         -------------------
      call read_integer_array_optional (
     :           section_name         ! Section header
     :         , 'day'                ! Keyword
     :         , max_irrigs           ! array size
     :         , '()'                 ! Units
     :         , p_day                ! Array
     :         , numvals              ! Number of values returned
     :         , 1                    ! Lower Limit for bound checking
     :         , 366)                 ! Upper Limit for bound checking
 
 
      call read_integer_array_optional (
     :           section_name         ! Section header
     :         , 'year'               ! Keyword
     :         , max_irrigs           ! array size
     :         , '()'                 ! Units
     :         , p_year               ! Array
     :         , numvals              ! Number of values returned
     :         , min_year                 ! Lower Limit for bound checking
     :         , max_year)                ! Upper Limit for bound checking
 
 
      call read_real_array_optional (
     :           section_name         ! Section header
     :         , 'amount'             ! Keyword
     :         , max_irrigs           ! array size
     :         , '(mm)'               ! Units
     :         , p_amount             ! Array
     :         , numvals              ! Number of values returned
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 1000.0)              ! Upper Limit for bound checking
 
      call read_char_array_optional (
     :           section_name         ! Section header
     :         , 'time'               ! Keyword
     :         , max_irrigs           ! array size
     :         , '(hh:mm)'            ! Units
     :         , p_time               ! Array
     :         , numvals)             ! Number of values returned
 
      call read_real_array_optional (
     :           section_name         ! Section header
     :         , 'duration'           ! Keyword
     :         , max_irrigs           ! array size
     :         , '(mm)'               ! Units
     :         , p_duration           ! Array
     :         , numvals              ! Number of values returned
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 1000.0)              ! Upper Limit for bound checking
 
      call read_char_var_optional (
     :           section_name         ! Section header
     :         , 'default_time'       ! Keyword
     :         , '(hh:mm)'            ! Units
     :         , p_default_time       ! Variable
     :         , numvals)             ! Number of values returned
      If (numvals.lt.1) then
         p_default_time = '00:00'
      else
      endif
 
      call read_real_var_optional (
     :           section_name         ! Section header
     :         , 'default_duration'   ! Keyword
     :         , '(min)'              ! Units
     :         , p_default_duration   ! Variable
     :         , numvals              ! Number of values returned
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 1000.)               ! Upper Limit for bound checking
      If (numvals.lt.1) then
         p_default_duration = 60.*24. !i.e. 24 hours
      else
      endif
 
      do 100 solnum = 1, g_num_solutes
         call read_real_array_optional (
     :           section_name         ! Section header
     :         , p_solutes(solnum)    ! Keyword
     :         , max_irrigs           ! array size
     :         , '(kg/ha)'            ! Units
     :         , temp_solute          ! Variable
     :         , numvals              ! Number of values returned
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 1000.)               ! Upper Limit for bound checking
            do 50 counter=1, numvals
               g_irrigation_solutes(solnum,counter) =
     :                                   temp_solute(counter)
   50       continue
 
  100 continue
 
         ! Read in automatic irrigation info from parameter file
         !         -------------------------
 
      call read_real_var_optional (
     :           section_name         ! Section header
     :         , 'crit_fr_asw'        ! Keyword
     :         , '(0-1)'              ! Units
     :         , p_crit_fr_asw        ! Variable
     :         , numvals              ! Number of values returned
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 1.0)                 ! Upper Limit for bound checking
 
      call read_real_var_optional (
     :           section_name         ! Section header
     :         , 'asw_depth'          ! Keyword
     :         , '(mm)'               ! Units
     :         , p_asw_depth          ! Variable
     :         , numvals              ! Number of values returned
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 10000.)              ! Upper Limit for bound checking
 
         ! Read in irrigation flags from parameter file
         !         ----------------
 
      call read_char_var_optional (
     :           section_name         ! Section header
     :         , 'manual_irrigation'  ! Keyword
     :         , '()'                 ! Units
     :         , p_Manual_irrigation  ! Variable
     :         , numvals)             ! Number of values returned
 
 
      call read_char_var_optional (
     :           section_name         ! Section header
     :         , 'automatic_irrigation'  ! Keyword
     :         , '()'                 ! Units
     :         , p_automatic_irrigation  ! Variable
     :         , numvals)             ! Number of values returned
 
      if (p_automatic_irrigation .eq. 'on') then
         if (reals_are_equal (p_crit_fr_asw, -1.0)
     :      .or. reals_are_equal (p_asw_depth, -1.0)) then
            call fatal_error (Err_user,
     :         'Cannot initiate auto irrigation until its configuration'
     :         //' parameters are set.')
         else
         endif
      else
      endif
 
      if (p_manual_irrigation .eq. 'on') then
         if (p_day(1) .eq. 0 .or.
     :       p_year(1) .eq. 0 .or.
     :       reals_are_equal (p_amount(1), 0.0)) then
 
            call fatal_error (Err_user,
     :         'Cannot initiate manual irrigation until its'//
     :         ' configuration parameters are set.')
 
         else
         endif
      else
      endif

      call read_char_var_optional (
     :           section_name            ! Section header
     :         , 'irrigation_allocation' ! Keyword
     :         , '()'                    ! Units
     :         , p_irrigation_allocation ! Variable
     :         , numvals)                ! Number of values returned

      if (p_irrigation_allocation .eq. 'on') then

         call read_real_var (
     :           section_name         ! Section header
     :         , 'allocation'         ! Keyword
     :         , '(mm)'               ! Units
     :         , g_allocation         ! Variable
     :         , numvals              ! Number of values returned
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 10000.)              ! Upper Limit for bound checking

         if (p_manual_irrigation.eq.'on') then
            call fatal_error (Err_user,
     :         ' Cannot have irrigation allocation enabled '//
     :         ' when using manual irrigation')            
         else
         endif

      else
         g_allocation = 0.0

      endif


      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      subroutine irrigate_zero_variables ()
*     ===========================================================
      implicit none
      include   'irrigate.inc'         ! irrigate common block
      include 'data.pub'
      include 'error.pub'
 
*+  Purpose
*     Set all variables in this module to zero.
 
*+  Mission Statement
*     Zero variables
 
*+  Changes
*     <insert here>
 
*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'irrigate_zero_variables')
 
*+  Local Variables
      integer counter
      integer solnum
 
*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      g_year = 0
      g_day = 0
      g_irrigation_applied = 0.0
      g_allocation = 0.0
      g_carry_over = 0.0
 
      call fill_integer_array (p_day, 0, max_irrigs)
      call fill_integer_array (p_year, 0, max_irrigs)
      call fill_real_array  (p_amount, 0.0, max_irrigs)
      call fill_real_array  (p_duration, 0.0, max_irrigs)
      call fill_char_array (p_time, ' ', max_irrigs)
      call fill_char_array (p_solutes, ' ', max_solutes)
 
      do 200 solnum=1,max_solutes
         do 100 counter=1, max_irrigs
            g_irrigation_solutes(solnum,counter) = 0.0
  100    continue
  200 continue
 
      p_automatic_irrigation = 'off'
      p_manual_irrigation = 'off'
      p_irrigation_allocation = 'off'
      p_asw_depth = -1.0
      p_crit_fr_asw = -1.0
      p_default_time = ' '
      p_default_duration = 0.0
      g_irr_pointer = 1
      g_num_solutes = 0
 
      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      subroutine irrigate_get_other_variables ()
*     ===========================================================
      implicit none
      include   'const.inc'            ! Constant definitions
      include   'irrigate.inc'         ! irrigate common block
      include 'intrface.pub'
      include 'error.pub'
 
*+  Purpose
*      Get the values of variables from other modules
 
*+  Mission Statement
*     Get Other Variables
 
*+  Changes
*     <insert here>
 
*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'irrigate_get_other_variables')
 
*+  Local Variables
      integer    numvals               ! number of values returned
 
*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      call Get_integer_var (
     :      unknown_module  ! Module that responds (Not Used)
     :    , 'year'          ! Variable Name
     :    , '()'            ! Units                (Not Used)
     :    , g_year          ! Variable
     :    , numvals         ! Number of values returned
     :    , min_year            ! Lower Limit for bound checking
     :    , max_year)           ! Upper Limit for bound checking
 
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
      implicit none
      include   'irrigate.inc'         ! irrigate Common block
      include 'engine.pub'
      include 'intrface.pub'
      include 'error.pub'
 
*+  Sub-Program Arguments
      character  Variable_name*(*)     ! (INPUT) Variable name to search for
 
*+  Purpose
*      Return the value of one of our variables to caller
 
*+  Mission Statement
*     Send Value of Requested Variable
 
*+  Changes
*      011195 jngh  added call to message_unused
*      230399 nih   added output for irrigation_fasw and irrigation_def
 
*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'irrigate_send_my_variable')

*+  Local Variables
      real fasw
      real swdef

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Variable_name .eq. 'irrigation') then
         call respond2get_real_var (
     :                              variable_name
     :                            , '(mm)'
     :                            , g_irrigation_applied)
 
      elseif (Variable_name .eq. 'automatic_irrigation') then
         call respond2get_char_var (
     :                variable_name           ! variable name
     :              , '()'                    ! units
     :              , p_automatic_irrigation) ! array
 
      elseif (Variable_name .eq. 'manual_irrigation') then
         call respond2get_char_var (
     :                variable_name           ! variable name
     :              , '()'                    ! units
     :              , p_manual_irrigation)    ! array
 
      elseif (Variable_name .eq. 'crit_fr_asw') then
         call respond2get_real_var (
     :                variable_name           ! variable name
     :              , '()'                    ! units
     :              , p_crit_fr_asw)          ! array
 
      elseif (Variable_name .eq. 'asw_depth') then
         call respond2get_real_var (
     :                variable_name           ! variable name
     :              , '(mm)'                  ! units
     :              , p_asw_depth)            ! array

      elseif (Variable_name .eq. 'irr_fasw') then
         call irrigate_fasw (fasw, swdef)

         call respond2get_real_var (
     :                variable_name           ! variable name
     :              , '(0-1)'                 ! units
     :              , fasw)                   ! array

      elseif (Variable_name .eq. 'irr_deficit') then
         call irrigate_fasw (fasw, swdef)

         call respond2get_real_var (
     :                variable_name           ! variable name
     :              , '(mm)'                  ! units
     :              , swdef)                  ! array

      elseif (Variable_name .eq. 'allocation') then
         call respond2get_real_var (
     :                variable_name           ! variable name
     :              , '(mm)'                  ! units
     :              , g_allocation)           ! array

      elseif (Variable_name .eq. 'carry_over') then
         call respond2get_real_var (
     :                variable_name           ! variable name
     :              , '(mm)'                  ! units
     :              , g_carry_over)           ! array
 
      else
         call Message_unused ()
      endif
 
      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      subroutine irrigate_set_my_variable (Variable_name)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'irrigate.inc'         ! irrigate common block
      include 'data.pub'
      include 'engine.pub'
      include 'intrface.pub'
      include 'error.pub'
 
*+  Sub-Program Arguments
      character  Variable_name*(*)     ! (INPUT) Variable name to search for
 
*+  Purpose
*     Set one of our variables altered by some other module
 
*+  Mission Statement
*     Set Variable as Requested
 
*+  Changes
*      011195 jngh  added call to message_unused
*      060695 jngh changed respond2set to collect routines
*      201097 IGH - added profile depth to bound checking
 
*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'irrigate_set_my_variable')
 
*+  Local Variables
      integer    numvals               ! number of values returned
      real       amount
 
*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Variable_name .eq. 'manual_irrigation') then
         call collect_char_var (
     :                variable_name        ! variable name
     :              , '()'                 ! units
     :              , p_manual_irrigation  ! array
     :              , numvals)             ! number of elements returned
 
         if (p_manual_irrigation .eq. 'on') then
            if (p_day(1) .eq. 0 .or.
     :          p_year(1) .eq. 0 .or.
     :          reals_are_equal (p_amount(1), 0.0)) then
 
               call fatal_error (Err_user,
     :         'Cannot initiate manual irrigation because its'//
     :         ' configuration parameters are not set.')
 
            elseif (p_irrigation_allocation .eq. 'on') then
               call fatal_error (Err_user,
     :         'Cannot initiate manual irrigation'//
     :         ' when irrigation allocation is being used.')
            else
            endif
         else
         endif
 
      elseif (Variable_name .eq. 'automatic_irrigation') then
         call collect_char_var (
     :                variable_name           ! variable name
     :              , '()'                    ! units
     :              , p_automatic_irrigation  ! array
     :              , numvals)                ! number of elements returned
 
         if (p_automatic_irrigation .eq. 'on') then
            if (reals_are_equal (p_crit_fr_asw, -1.0)
     :         .or. reals_are_equal (p_asw_depth, -1.0)) then
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
     :              , p_crit_fr_asw     ! array
     :              , numvals           ! number of elements returned
     :              , 0.0               ! lower limit for bounds checking
     :              , 1.0)              ! upper limit for bounds checking
 
 
      elseif (Variable_name .eq. 'asw_depth') then
 
         call collect_real_var (
     :                variable_name     ! array name
     :              , '(mm)'            ! units
     :              , p_asw_depth       ! array
     :              , numvals           ! number of elements returned
     :              , 0.0               ! lower limit for bounds checking
     :              , 10000.)           ! upper limit for bounds checking
 
      elseif (Variable_name .eq. 'amount') then
 
         call collect_real_var (
     :                variable_name     ! array name
     :              , '(mm)'            ! units
     :              , amount            ! array
     :              , numvals           ! number of elements returned
     :              , 0.0               ! lower limit for bounds checking
     :              , 1000.)            ! upper limit for bounds checking

         call irrigate_set_amount(amount)

      elseif (Variable_name .eq. 'allocation') then
 
         if (p_irrigation_allocation .eq. 'on') then

            g_carry_over = g_allocation

            call collect_real_var (
     :                variable_name     ! array name
     :              , '(mm)'            ! units
     :              , g_allocation      ! array
     :              , numvals           ! number of elements returned
     :              , 0.0               ! lower limit for bounds checking
     :              , 10000.)           ! upper limit for bounds checking

         else
               call fatal_error (Err_user,
     :            'Cannot set allocation amount'//
     :            ' when irrigation allocation is not being used.')

         endif

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
      implicit none
      include   'irrigate.inc'         ! irrigation common block
      include 'error.pub'
 
*+  Purpose
*      Perform actions for current day.
 
*+  Mission Statement
*     Perform actions for the current day
 
*+  Changes
*     <insert here>
 
*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'irrigate_process')
 
*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      call irrigate_get_other_variables ()
      call irrigate_check_variables ()
 
      if (p_manual_irrigation .eq. 'on') then
         call irrigate_schedule ()
 
      else
      endif
 
      if (p_automatic_irrigation .eq. 'on') then
         call irrigate_automatic ()
 
      else
      endif
 
      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      subroutine irrigate_schedule ()
*     ===========================================================
      implicit none
      include    'const.inc'
      include    'irrigate.inc'        ! irrigation common block
      include 'data.pub'
      include 'engine.pub'
      include 'intrface.pub'
      include 'error.pub'
 
*+  Purpose
*       irrigation management. Informs manager that irrigation is
*       required and how much.
 
*+  Mission Statement
*     Apply Scheduled Irrigation
 
*+  Changes
*     070694 - nih adapted from old irigat module
*     110395 jngh moved messaging to to apply routine
*     060696 jngh implemented postbox method for data transfer
*     110996 nih  added increment for g_irr_applied
 
*+  Constant Values
      character  my_name*(*)           ! name of this procedure
      parameter (my_name = 'irrigate_schedule')
 
*+  Local Variables
      real       amount                ! amount of irrigation to apply
      integer    irigno                ! loop counter for input
      real       MyDuration
      character  MyTime*10
      integer    num_irrigations       ! number of irrigation applications
      integer    solnum
      integer    start_irrig_no        ! index in schedule to start at
 
*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      num_irrigations = count_of_integer_vals (p_day, max_irrigs)
 
      if (num_irrigations.gt.0) then
 
             ! we have a schedule.  see if we have an irrigation today.
 
         start_irrig_no = g_irr_pointer
 
         do 1000 irigno = start_irrig_no, num_irrigations
            if (g_day.eq.p_day(irigno)
     :                     .and.
     :         g_year.eq.p_year(irigno))
     :      then
               amount = p_amount(irigno) * effirr
 
               if (p_time(irigno).eq.blank) then
                  MyTime = p_default_time
               else
                  MyTime = p_time(irigno)
               endif
 
               if (p_Duration(irigno).eq.0.0) then
                  MyDuration = p_default_duration
               else
                  MyDuration = p_Duration(irigno)
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
                     call post_real_var (p_solutes(solnum)
     :                    ,'(kg/ha)'
     :                    , g_irrigation_solutes(solnum, irigno))
200            continue
 
               call message_send_immediate (unknown_module
     :                                     , 'add_water'
     :                                     , blank)
 
               call delete_postbox ()
 
               g_irrigation_applied = g_irrigation_applied + amount
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
      implicit none
      include   'const.inc'
      include   'irrigate.inc'
      include 'data.pub'
      include 'engine.pub'
      include 'intrface.pub'
      include 'error.pub'
      include 'write.pub'
 
*+  Purpose
*       Automatic irrigation management.
 
*+  Mission Statement
*     Apply Automatic irrigation
 
*+  Changes
*      070694 - nih adapted from jngh's old automatic irrigation module
*      040895 - jngh corrected format statement to match data types.
*      021195 jngh changed message_pass_to_module to message_send_immediate
*      060696 jngh implemented postbox method for data transfer
*      110996 nih  added increment for g_irr_applied
*      160399 nih  added irrigation allocation
 
*+  Constant Values
      character  my_name*(*)           ! name of this module
      parameter (my_name = 'irrigate_automatic')
 
*+  Local Variables
      real       amount                ! amount of irrigation to apply (mm)
      real       avail_fr              ! fraction of avalable water in
                                       !    specified profile
      real       swdef                 ! sw deficit (mm)

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      call irrigate_fasw (avail_fr, swdef)

      if (avail_fr.lt.p_crit_fr_asw) then
         amount = divide (swdef, effirr, 0.0)
 
         call irrigate_check_allocation(amount)

         call new_postbox ()
 
            ! send message regardless of fatal error - will stop anyway
 
         call post_real_var   ('amount'
     :                        ,'(mm)'
     :                        , amount)
 
         call post_real_var   ('duration'
     :                        ,'(min)'
     :                        , p_default_duration)
 
         call post_char_var   ('time'
     :                        ,'(hh:mm)'
     :                        , p_default_time)
 
!         No solutes in automatic irrigation just yet - later on
 
!         do 200 solnum = 1, g_num_solutes
!            if (numvals_solute(solnum) .ne.0) then
!               call post_real_var   (p_solutes(solnum)
!     :                              ,'(kg/ha)'
!     :                              , solute(solnum))
!            else
!            endif
!200      continue
 
         call message_send_immediate (unknown_module
     :                               , 'add_water'
     :                               , blank)
 
         call delete_postbox ()
 
         g_irrigation_applied = g_irrigation_applied + amount
 
      else
          ! soil not dry enough to require irrigation
      endif
 
      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      subroutine irrigate_inter_timestep ()
*     ===========================================================
      implicit none
      include   'irrigate.inc'
      include 'error.pub'
 
*+  Purpose
*     <insert here>
 
*+  Mission Statement
*     Reset total irrigation applied
 
*+  Changes
*     <insert here>
 
*+  Constant Values
      character*(*) my_name            ! name of current procedure
      parameter (my_name = 'irrigate_inter_timestep')
 
*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      g_irrigation_applied = 0.0
      g_carry_over = 0.0
 
      call pop_routine (my_name)
      return
      end
 
 
 
* ====================================================================
       subroutine irrigate_check_variables ()
* ====================================================================
      implicit none
      include 'const.inc'
      include 'irrigate.inc'
      include 'data.pub'
      include 'error.pub'
 
*+  Purpose
*      Check the value of parameters or state variables
*      for validity.
 
*+  Mission Statement
*     Check the value of parameters and state variables
 
*+  Changes
*     10-11-1997 - neil huth - Programmed and Specified
 
*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'irrigate_check_variables')
 
*+  Local Variables
      real       profile_depth           ! total soil profile depth
 
*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      if (p_automatic_irrigation .eq. 'on') then
 
         profile_depth = sum_real_array (g_dlayer, max_layer)
 
         if (p_asw_depth .gt. profile_depth) then
            call fatal_error (Err_User,
     :      'ASW_depth for automatic irrigation must not '//
     :      'exceed profile depth.')
         else
            ! No problems here
         endif
 
         if (p_asw_depth .le. 0.0) then
            call fatal_error (Err_User,
     :      'ASW_depth for automatic irrigation must not '//
     :      'be zero or negetive.')
         else
            ! No problems here
         endif
      else
         ! Do not worry about these parameters as they may not be
         ! set by the user.
      endif
 
      call pop_routine (myname)
      return
      end
 
*     ===========================================================
      subroutine irrigate_set_amount (amount)
*     ===========================================================
      implicit none
      include    'const.inc'
      include    'irrigate.inc'        ! irrigation common block
      include 'data.pub'
      include 'engine.pub'
      include 'intrface.pub'
      include 'error.pub'
      include 'write.pub'

*+  Sub-Program Arguments
      real amount ! (INPUT)
 
*+  Purpose
*       To apply an amount of irrigation as specified by user.
 
*+  Mission Statement
*     Apply Set Amount from Manager
 
*+  Changes
*     091298 nih  created
*     160399 nih  added irrigation allocation
  
*+  Constant Values
      character  my_name*(*)           ! name of this procedure
      parameter (my_name = 'irrigate_set_amount')
 
*+  Local Variables
      real actual_amount
 
*- Implementation Section ----------------------------------
      call push_routine (my_name)
      if (amount.ge.0.) then

         call irrigate_check_allocation(amount)

         actual_amount = amount * effirr

         call new_postbox ()
 
         call post_real_var   ('amount'
     :                        ,'(mm)'
     :                        , actual_amount)
 
         call post_real_var   ('duration'
     :                        ,'(min)'
     :                        , p_default_duration)
 
         call post_char_var   ('time'
     :                        ,'(hh:mm)'
     :                        , p_default_time)
 
 
         call message_send_immediate (unknown_module
     :                               , 'add_water'
     :                               , blank)
 
         call delete_postbox ()
 
         g_irrigation_applied = g_irrigation_applied + amount
      else
         call fatal_error (ERR_User,'negative irrigation amount')
      endif
 
      call pop_routine (my_name)
      return
      end
  
 
*     ===========================================================
      subroutine irrigate_check_allocation (amount)
*     ===========================================================
      implicit none
      include   'irrigate.inc'
      include 'error.pub'
      include 'write.pub'

*+  Sub-Program Arguments
      real amount
 
*+  Purpose
*     Check that an amount of irrigation meets allocation budget
 
*+  Mission Statement
*     Check amount with allocation budget
 
*+  Changes
*     <insert here>

*+  Local Variables
      character ReportString*200   ! simple reporting string
 
*+  Constant Values
      character*(*) my_name            ! name of current procedure
      parameter (my_name = 'irrigate_check_allocation')
 
*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (p_irrigation_allocation.eq.'on') then

         if (amount.gt.g_allocation) then

            write(ReportString,'(1x,A,f6.2,A,f6.2,A)')
     :       ' Irrigation of ',amount
     :       ,' mm reduced to remaining allocation of '
     :       ,g_allocation, ' mm'

            call Report_Event (ReportString)
            amount = g_allocation
         else
         endif

         g_allocation = g_allocation - amount

      else
      endif

      call pop_routine (my_name)
      return
      end

*     ===========================================================
      subroutine irrigate_fasw (fasw, swdef)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'irrigate.inc'
      include 'data.pub'
      include 'error.pub'

*+  Sub-Program Arguments
      real fasw
      real swdef
 
*+  Purpose
*       Calculate Fraction of available soil water and water deficit
 
*+  Mission Statement
*       Calculate Fraction of available soil water and water deficit.
 
*+  Changes
*      230399 nih  based on code from irrigate automatic
 
*+  Constant Values
      character  my_name*(*)           ! name of this module
      parameter (my_name = 'irrigate_fasw')
 
*+  Local Variables
      real       cumdep                ! cumulative depth in loop (mm)
      integer    nlayr                 ! number of layers
      real       avail_sw              ! total avail. sw down to specified depth
                                       ! (mm)
      real       pot_avail_sw          ! total potential avail sw down to
                                       ! specified depth (mm)
      real       excess_fr             ! fraction of excess depth below specifie
                                       ! in last layer (mm)

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
               ! get water deficit on the spot
 
      nlayr = get_cumulative_index_real (p_asw_depth, g_dlayer
     :                                 , max_layer)
      cumdep = sum_real_array (g_dlayer, nlayr)
 
      excess_fr = divide ((cumdep - p_asw_depth) ,g_dlayer(nlayr), 0.0)
 
cnh note that results may be strange if swdep < ll15
      avail_sw  = (sum_real_array (g_sw_dep, nlayr)
     :          - excess_fr * g_sw_dep(nlayr))
     :          - (sum_real_array (g_ll15_dep, nlayr)
     :          - excess_fr * g_ll15_dep(nlayr))
     :          + g_irrigation_applied
 
      pot_avail_sw = (sum_real_array (g_dul_dep, nlayr)
     :             - excess_fr * g_dul_dep(nlayr))
     :             - (sum_real_array (g_ll15_dep, nlayr)
     :             - excess_fr * g_ll15_dep(nlayr))
 
      fasw = divide (avail_sw, pot_avail_sw, 0.0)
      swdef = l_bound(pot_avail_sw - avail_sw, 0.0)

      call pop_routine (my_name)
      return
      end
 
 
