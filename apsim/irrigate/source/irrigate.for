      include   'irrigate.inc'
!     ===========================================================
      subroutine AllocInstance (InstanceName, InstanceNo)
!     ===========================================================
      use IrrigateModule
      implicit none
 
!+  Sub-Program Arguments
      character InstanceName*(*)       ! (INPUT) name of instance
      integer   InstanceNo             ! (INPUT) instance number to allocate
 
!+  Purpose
!      Module instantiation routine.
 
!- Implementation Section ----------------------------------
               
      allocate (Instances(InstanceNo)%gptr)
      allocate (Instances(InstanceNo)%pptr)
      Instances(InstanceNo)%Name = InstanceName
 
      return
      end

!     ===========================================================
      subroutine FreeInstance (anInstanceNo)
!     ===========================================================
      use IrrigateModule
      implicit none
 
!+  Sub-Program Arguments
      integer anInstanceNo             ! (INPUT) instance number to allocate
 
!+  Purpose
!      Module de-instantiation routine.
 
!- Implementation Section ----------------------------------
               
      deallocate (Instances(anInstanceNo)%gptr)
      deallocate (Instances(anInstanceNo)%pptr)
 
      return
      end
     
!     ===========================================================
      subroutine SwapInstance (anInstanceNo)
!     ===========================================================
      use IrrigateModule
      implicit none
 
!+  Sub-Program Arguments
      integer anInstanceNo             ! (INPUT) instance number to allocate
 
!+  Purpose
!      Swap an instance into the global 'g' pointer
 
!- Implementation Section ----------------------------------
               
      g => Instances(anInstanceNo)%gptr
      p => Instances(anInstanceNo)%pptr
 
      return
      end
*     ===========================================================
      subroutine Main (Action, Data_String)
*     ===========================================================
      use IrrigateModule
      implicit none
      include   'const.inc'            ! Global constant definitions
      include   'event.inc'
      include   'action.inc'
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
 
*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'irrigate')
 
*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
         ! initialise error flags
      
      if (Action.eq.ACTION_Get_variable) then
         call irrigate_Send_my_variable (Data_String)
  
      else if (Action.eq.ACTION_Init) then
         call irrigate_zero_variables ()
         call irrigate_Init ()
 
      else if (Action.eq.EVENT_tick) then
         call irrigate_ONtick()
 
      else if (Action.eq.ACTION_Process) then
         call irrigate_get_other_variables ()
         call irrigate_process ()
 
      else if ((Action.eq.'irrigate').or.(Action.eq.'apply')) then
         call irrigate_get_other_variables ()
         call irrigate_irrigate ()
 
      else if (Action .eq. ACTION_Set_variable) then
         call irrigate_set_my_variable (Data_String)

      else if (Action .eq. EVENT_new_solute) then
         call irrigate_on_new_solute ()
 
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
      use IrrigateModule
      implicit none
      include   'const.inc'            ! Global constant definitions
      include 'event.inc'
      include 'postbox.pub'
      include 'intrface.pub'
      include 'error.pub'
 
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
         duration = p%default_duration
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
         time = p%default_time
      else
          ! got a value
      endif
 
 
      ! look for any solute information in the postbox
      ! ----------------------------------------------
      do 100 solnum = 1, g%num_solutes
         Call collect_real_var_optional (
     :                       g%solute_names(solnum)
     :                     , '(kg/ha)'
     :                     , solute(solnum)
     :                     , numvals_solute(solnum)
     :                     , 0.0
     :                     , 1000.0)

  100 continue
 
      call new_postbox ()
 
         ! send message regardless of fatal error - will stop anyway
 
      call post_real_var   (DATA_irrigate_amount
     :                        ,'(mm)'
     :                        , amount*p%irrigation_efficiency)
 
      call post_real_var   (DATA_irrigate_duration
     :                        ,'(min)'
     :                        , duration)
 
      call post_char_var   (DATA_irrigate_time
     :                        ,'(hh:mm)'
     :                        , time)
 
      do 200 solnum = 1, g%num_solutes
         if (numvals_solute(solnum) .ne.0) then
            ! NOTE - solutes NOT lost due to inefficiency!!!!!
            call post_real_var   (g%solute_names(solnum)
     :                           ,'(kg/ha)'
     :                           , solute(solnum))
         else
         endif
200   continue
 
      call event_send (EVENT_irrigated)
 
      call delete_postbox ()
 
      g%irrigation_applied = g%irrigation_applied 
     :                     + amount * p%irrigation_efficiency
      g%irrigation_tot = g%irrigation_tot + amount
      g%irrigation_loss = g%irrigation_loss 
     :                  + amount * (1. - p%irrigation_efficiency)
 
      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      subroutine irrigate_Init ()
*     ===========================================================
      use IrrigateModule
      implicit none
      include   'const.inc'            ! Constant definitions
      include 'data.pub'
      include 'error.pub'
 
*+  Purpose
*      Initialise irrigate module
 
*+  Mission Statement
*     Initialise
 
*+  Changes
*     <insert here>
 
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
 
      Event_string = ' Initialising '
      call Write_string (Event_string)
 
         ! Get all parameters from parameter file
 
      call irrigate_read_param ()
 
      num_irrigs = count_of_integer_vals (p%day, max_irrigs)
      If (num_irrigs .gt. 0) then
         call write_string (new_line//new_line)
 
         string = '                 Irrigation Schedule'
         call write_string (string)
 
         string = '     -----------------------------------------------'
         call write_string (string)
 
         string = '               day     year    amount (mm)'
         call write_String (string)
         string = '     -----------------------------------------------'
         call write_String (string)
 
         do 100 counter = 1, num_irrigs
            write (string,'(14x, i5, i6, f8.2)')
     :                                  p%day(counter)
     :                                , p%year(counter)
     :                                , p%amount(counter)
            call write_string (String)
  100    continue
         string = '     -----------------------------------------------'
         call write_String (string)
      Else
      Endif
 
      call write_string (new_line//new_line)
 
      string = '                 Irrigation parameters'
      call write_string (string)
 
      string = '     -----------------------------------------------'
      call write_string (string)
 
      if (p%manual_irrigation .eq. 'on') then
         call write_String (
     :        '      Irrigation Schedule (Enabled)')
      else
         call write_String (
     :        '      Irrigation Schedule (Disabled)')
      endif
 
      if (p%automatic_irrigation .eq. 'on') then
         call write_String (
     :        '      Automatic Irrigation Application (Enabled)')
      else
         call write_String (
     :        '      Automatic Irrigation Application (Disabled)')
      endif
      If (reals_are_equal (p%crit_fr_asw, -1.0)) then
         write (string, '(a)')
     :        '      critical fraction of available soil water = '
     :     // ' not intialised'
      else
         write (string, '(a, f5.2)')
     ;        '      critical fraction of available soil water = '
     :       , p%crit_fr_asw
      endif
      call write_String (string)
 
      if (reals_are_equal (p%asw_depth, -1.0)) then
         write (string, '(a)')
     :        '      depth for calculating available soil water = '
     :     // ' not initialised'
      else
         write (string, '(a, f10.2)')
     ;        '      depth for calculating available soil water = '
     :       , p%asw_depth
      endif
      call write_String (string)

      if (p%irrigation_allocation .eq. 'on') then
         call write_String (
     :        '      Irrigation Allocation Budget (Enabled)')
      else
         call write_String (
     :        '      Irrigation Allocation Budget (Disabled)')
      endif
 
      string = '     -----------------------------------------------'
      call write_string (string)
 
      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      subroutine irrigate_read_param ()
*     ===========================================================
      use IrrigateModule
      implicit none
      include   'const.inc'
      include 'data.pub'
      include 'read.pub'
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
      integer    numvals               ! number of values read from file
 
*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      call write_string (new_line//'   - Reading Parameters')
 
 
         ! Read in irrigation schedule from parameter file
         !         -------------------
      call read_integer_array_optional (
     :           section_name         ! Section header
     :         , 'day'                ! Keyword
     :         , max_irrigs           ! array size
     :         , '()'                 ! Units
     :         , p%day                ! Array
     :         , numvals              ! Number of values returned
     :         , 1                    ! Lower Limit for bound checking
     :         , 366)                 ! Upper Limit for bound checking
 
 
      call read_integer_array_optional (
     :           section_name         ! Section header
     :         , 'year'               ! Keyword
     :         , max_irrigs           ! array size
     :         , '()'                 ! Units
     :         , p%year               ! Array
     :         , numvals              ! Number of values returned
     :         , min_year                 ! Lower Limit for bound checking
     :         , max_year)                ! Upper Limit for bound checking
 
 
      call read_real_array_optional (
     :           section_name         ! Section header
     :         , 'amount'             ! Keyword
     :         , max_irrigs           ! array size
     :         , '(mm)'               ! Units
     :         , p%amount             ! Array
     :         , numvals              ! Number of values returned
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 1000.0)              ! Upper Limit for bound checking
 
      call read_char_array_optional (
     :           section_name         ! Section header
     :         , 'time'               ! Keyword
     :         , max_irrigs           ! array size
     :         , '(hh:mm)'            ! Units
     :         , p%time               ! Array
     :         , numvals)             ! Number of values returned
 
      call read_real_array_optional (
     :           section_name         ! Section header
     :         , 'duration'           ! Keyword
     :         , max_irrigs           ! array size
     :         , '(mm)'               ! Units
     :         , p%duration           ! Array
     :         , numvals              ! Number of values returned
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 1000.0)              ! Upper Limit for bound checking
 
      call read_char_var_optional (
     :           section_name         ! Section header
     :         , 'default_time'       ! Keyword
     :         , '(hh:mm)'            ! Units
     :         , p%default_time       ! Variable
     :         , numvals)             ! Number of values returned
      If (numvals.lt.1) then
         p%default_time = '00:00'
      else
      endif
 
      call read_real_var_optional (
     :           section_name         ! Section header
     :         , 'default_duration'   ! Keyword
     :         , '(min)'              ! Units
     :         , p%default_duration   ! Variable
     :         , numvals              ! Number of values returned
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 1000.)               ! Upper Limit for bound checking
      If (numvals.lt.1) then
         p%default_duration = 60.*24. !i.e. 24 hours
      else
      endif
 
 
  100 continue
 
         ! Read in automatic irrigation info from parameter file
         !         -------------------------
 
      call read_real_var_optional (
     :           section_name         ! Section header
     :         , 'crit_fr_asw'        ! Keyword
     :         , '(0-1)'              ! Units
     :         , p%crit_fr_asw        ! Variable
     :         , numvals              ! Number of values returned
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 1.0)                 ! Upper Limit for bound checking
 
      call read_real_var_optional (
     :           section_name         ! Section header
     :         , 'asw_depth'          ! Keyword
     :         , '(mm)'               ! Units
     :         , p%asw_depth          ! Variable
     :         , numvals              ! Number of values returned
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 10000.)              ! Upper Limit for bound checking
 
         ! Read in irrigation flags from parameter file
         !         ----------------
 
      call read_char_var_optional (
     :           section_name         ! Section header
     :         , 'manual_irrigation'  ! Keyword
     :         , '()'                 ! Units
     :         , p%Manual_irrigation  ! Variable
     :         , numvals)             ! Number of values returned
 
 
      call read_char_var_optional (
     :           section_name         ! Section header
     :         , 'automatic_irrigation'  ! Keyword
     :         , '()'                 ! Units
     :         , p%automatic_irrigation  ! Variable
     :         , numvals)             ! Number of values returned
 
      if (p%automatic_irrigation .eq. 'on') then
         if (reals_are_equal (p%crit_fr_asw, -1.0)
     :      .or. reals_are_equal (p%asw_depth, -1.0)) then
            call fatal_error (Err_user,
     :         'Cannot initiate auto irrigation until its configuration'
     :         //' parameters are set.')
         else
         endif
      else
      endif
 
      if (p%manual_irrigation .eq. 'on') then
         if (p%day(1) .eq. 0 .or.
     :       p%year(1) .eq. 0 .or.
     :       reals_are_equal (p%amount(1), 0.0)) then
 
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
     :         , p%irrigation_allocation ! Variable
     :         , numvals)                ! Number of values returned

      if (p%irrigation_allocation .eq. 'on') then

         call read_real_var (
     :           section_name         ! Section header
     :         , 'allocation'         ! Keyword
     :         , '(mm)'               ! Units
     :         , g%allocation         ! Variable
     :         , numvals              ! Number of values returned
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 10000.)              ! Upper Limit for bound checking

         if (p%manual_irrigation.eq.'on') then
            call fatal_error (Err_user,
     :         ' Cannot have irrigation allocation enabled '//
     :         ' when using manual irrigation')            
         else
         endif

      else
         g%allocation = 0.0

      endif

      call read_real_var_optional (
     :           section_name          ! Section header
     :         , 'irrigation_efficiency' ! Keyword
     :         , '(0-1)'               ! Units
     :         , p%irrigation_efficiency ! Variable
     :         , numvals              ! Number of values returned
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 1.0)                 ! Upper Limit for bound checking
      if (numvals.eq.0) then
         p%irrigation_efficiency = 1.0
      endif

      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      subroutine irrigate_zero_variables ()
*     ===========================================================
      use IrrigateModule
      implicit none
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
 
      g%year = 0
      g%day = 0
      g%irrigation_applied = 0.0
      g%allocation = 0.0
      g%carry_over = 0.0
 
      call fill_integer_array (p%day, 0, max_irrigs)
      call fill_integer_array (p%year, 0, max_irrigs)
      call fill_real_array  (p%amount, 0.0, max_irrigs)
      call fill_real_array  (p%duration, 0.0, max_irrigs)
      call fill_char_array (p%time, ' ', max_irrigs)

      call fill_char_array (g%solute_names, ' ', max_solutes)
      call fill_char_array (g%solute_owners, ' ', max_solutes)
      g%num_solutes = 0
 
      do 200 solnum=1,max_solutes
         do 100 counter=1, max_irrigs
            g%irrigation_solutes(solnum,counter) = 0.0
  100    continue
  200 continue
 
      p%automatic_irrigation = 'off'
      p%manual_irrigation = 'off'
      p%irrigation_allocation = 'off'
      p%asw_depth = -1.0
      p%crit_fr_asw = -1.0
      p%default_time = ' '
      p%default_duration = 0.0
      g%irr_pointer = 1
      g%num_solutes = 0
 
      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      subroutine irrigate_get_other_variables ()
*     ===========================================================
      use IrrigateModule
      implicit none
      include   'const.inc'            ! Constant definitions
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
 
      call Get_real_array (
     :      unknown_module  ! Module that responds (Not Used)
     :    , 'sw_dep'        ! Variable Name
     :    , max_layer       ! size of array
     :    , '(mm)'          ! Units                (Not Used)
     :    , g%sw_dep        ! Variable
     :    , numvals         ! Number of values returned
     :    , 0.0             ! Lower Limit for bound checking
     :    , 1000.0)         ! Upper Limit for bound checking
 
      call Get_real_array (
     :      unknown_module  ! Module that responds (Not Used)
     :    , 'll15_dep'      ! Variable Name
     :    , max_layer       ! size of array
     :    , '(mm)'          ! Units                (Not Used)
     :    , g%ll15_dep      ! Variable
     :    , numvals         ! Number of values returned
     :    , 0.0             ! Lower Limit for bound checking
     :    , 1000.0)         ! Upper Limit for bound checking
 
      call Get_real_array (
     :      unknown_module  ! Module that responds (Not Used)
     :    , 'dul_dep'       ! Variable Name
     :    , max_layer       ! size of array
     :    , '(mm)'          ! Units                (Not Used)
     :    , g%dul_dep       ! Variable
     :    , numvals         ! Number of values returned
     :    , 0.0             ! Lower Limit for bound checking
     :    , 1000.0)         ! Upper Limit for bound checking
 
      call Get_real_array (
     :      unknown_module  ! Module that responds (Not Used)
     :    , 'dlayer'        ! Variable Name
     :    , max_layer       ! size of array
     :    , '(mm)'          ! Units                (Not Used)
     :    , g%dlayer        ! Variable
     :    , numvals         ! Number of values returned
     :    , 0.0             ! Lower Limit for bound checking
     :    , 1000.0)         ! Upper Limit for bound checking
 
      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      subroutine irrigate_Send_my_variable (Variable_name)
*     ===========================================================
      use IrrigateModule
      implicit none
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
     :                            , g%irrigation_applied)

      elseif (Variable_name .eq. 'irrig_tot') then
         call respond2get_real_var (
     :                variable_name           ! variable name
     :              , '()'                    ! units
     :              , g%irrigation_tot)       ! array

      elseif (Variable_name .eq. 'irrig_loss') then
         call respond2get_real_var (
     :                variable_name           ! variable name
     :              , '()'                    ! units
     :              , g%irrigation_loss)       ! array
 
      elseif (Variable_name .eq. 'automatic_irrigation') then
         call respond2get_char_var (
     :                variable_name           ! variable name
     :              , '()'                    ! units
     :              , p%automatic_irrigation) ! array
 
      elseif (Variable_name .eq. 'manual_irrigation') then
         call respond2get_char_var (
     :                variable_name           ! variable name
     :              , '()'                    ! units
     :              , p%manual_irrigation)    ! array
 
      elseif (Variable_name .eq. 'crit_fr_asw') then
         call respond2get_real_var (
     :                variable_name           ! variable name
     :              , '()'                    ! units
     :              , p%crit_fr_asw)          ! array
 
      elseif (Variable_name .eq. 'asw_depth') then
         call respond2get_real_var (
     :                variable_name           ! variable name
     :              , '(mm)'                  ! units
     :              , p%asw_depth)            ! array

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
     :              , '(Ml)'                  ! units
     :              , (g%allocation/100))           ! array

      elseif (Variable_name .eq. 'carry_over') then
         call respond2get_real_var (
     :                variable_name           ! variable name
     :              , '(Ml)'                  ! units
     :              , (g%carry_over/100))           ! array
 
      else
         call Message_unused ()
      endif
 
      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      subroutine irrigate_set_my_variable (Variable_name)
*     ===========================================================
      use IrrigateModule
      implicit none
      include   'const.inc'
      include 'data.pub'
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
     :              , p%manual_irrigation  ! array
     :              , numvals)             ! number of elements returned
 
         if (p%manual_irrigation .eq. 'on') then
            if (p%day(1) .eq. 0 .or.
     :          p%year(1) .eq. 0 .or.
     :          reals_are_equal (p%amount(1), 0.0)) then
 
               call fatal_error (Err_user,
     :         'Cannot initiate manual irrigation because its'//
     :         ' configuration parameters are not set.')
 
            elseif (p%irrigation_allocation .eq. 'on') then
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
     :              , p%automatic_irrigation  ! array
     :              , numvals)                ! number of elements returned
 
         if (p%automatic_irrigation .eq. 'on') then
            if (reals_are_equal (p%crit_fr_asw, -1.0)
     :         .or. reals_are_equal (p%asw_depth, -1.0)) then
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
     :              , p%crit_fr_asw     ! array
     :              , numvals           ! number of elements returned
     :              , 0.0               ! lower limit for bounds checking
     :              , 1.0)              ! upper limit for bounds checking
 
 
      elseif (Variable_name .eq. 'asw_depth') then
 
         call collect_real_var (
     :                variable_name     ! array name
     :              , '(mm)'            ! units
     :              , p%asw_depth       ! array
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
     
      elseif (Variable_name .eq. 'irrigation_efficiency') then
 
         call collect_real_var (
     :                variable_name            ! array name
     :              , '(mm)'                   ! units
     :              , p%irrigation_efficiency  ! array
     :              , numvals                  ! number of elements returned
     :              , 0.0                      ! lower limit for bounds checking
     :              , 1.)                      ! upper limit for bounds checking

      elseif (Variable_name .eq. 'allocation') then
 
         if (p%irrigation_allocation .eq. 'on') then

            g%carry_over = g%allocation

            call collect_real_var (
     :                variable_name     ! array name
     :              , '(mm)'            ! units
     :              , g%allocation      ! array
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
      use IrrigateModule
      implicit none
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
 
      if (p%manual_irrigation .eq. 'on') then
         call irrigate_schedule ()
 
      else
      endif
 
      if (p%automatic_irrigation .eq. 'on') then
         call irrigate_automatic ()
 
      else
      endif
 
      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      subroutine irrigate_schedule ()
*     ===========================================================
      use IrrigateModule
      implicit none
      include    'const.inc'
      include 'event.inc'
      include 'postbox.pub'
      include 'data.pub'
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
 
      num_irrigations = count_of_integer_vals (p%day, max_irrigs)
 
      if (num_irrigations.gt.0) then
 
             ! we have a schedule.  see if we have an irrigation today.
 
         start_irrig_no = g%irr_pointer
 
         do 1000 irigno = start_irrig_no, num_irrigations
            if (g%day.eq.p%day(irigno)
     :                     .and.
     :         g%year.eq.p%year(irigno))
     :      then
               amount = p%amount(irigno) 
 
               if (p%time(irigno).eq.blank) then
                  MyTime = p%default_time
               else
                  MyTime = p%time(irigno)
               endif
 
               if (p%Duration(irigno).eq.0.0) then
                  MyDuration = p%default_duration
               else
                  MyDuration = p%Duration(irigno)
               endif
 
 
               call new_postbox ()
 
               call post_real_var   (DATA_irrigate_amount
     :                              ,'(mm)'
     :                              , amount*p%irrigation_efficiency)
 
               call post_real_var   (DATA_irrigate_duration
     :                              ,'(min)'
     :                              , myduration)
 
               call post_char_var   (DATA_irrigate_time
     :                              ,'(hh:mm)'
     :                              , mytime)
 
               do 200 solnum = 1, g%num_solutes
                  ! NOTE - solutes NOT lost due to inefficiency!!!!!
                     call post_real_var (g%solute_names(solnum)
     :                    ,'(kg/ha)'
     :                    , g%irrigation_solutes(solnum, irigno))
200            continue
 
               call event_send (EVENT_irrigated)
 
               call delete_postbox ()
 
               g%irrigation_applied = g%irrigation_applied 
     :                              + amount * p%irrigation_efficiency
               g%irrigation_tot = g%irrigation_tot + amount
               g%irrigation_loss = g%irrigation_loss
     :                        + amount * (1. - p%irrigation_efficiency)

               g%irr_pointer = irigno + 1
 
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
      use IrrigateModule
      implicit none
      include   'const.inc'
      include 'event.inc'
      include 'postbox.pub'
      include 'data.pub'
      include 'intrface.pub'
      include 'error.pub'
 
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

      if (avail_fr.lt.p%crit_fr_asw) then
         amount = divide (swdef, p%irrigation_efficiency, 0.0)
 
         call irrigate_check_allocation(amount)

         call new_postbox ()
 
            ! send message regardless of fatal error - will stop anyway
 
         call post_real_var   (DATA_irrigate_amount
     :                        ,'(mm)'
     :                        , amount*p%irrigation_efficiency)
 
         call post_real_var   (DATA_irrigate_duration
     :                        ,'(min)'
     :                        , p%default_duration)
 
         call post_char_var   (DATA_irrigate_time
     :                        ,'(hh:mm)'
     :                        , p%default_time)
 
!         No solutes in automatic irrigation just yet - later on
 
!         do 200 solnum = 1, g%num_solutes
!            if (numvals_solute(solnum) .ne.0) then
!               call post_real_var   (p%solutes(solnum)
!     :                              ,'(kg/ha)'
!     :                              , solute(solnum))
!            else
!            endif
!200      continue
 
         call event_send(EVENT_irrigated) 
         call delete_postbox ()
 
         g%irrigation_applied = g%irrigation_applied 
     :                        + amount * p%irrigation_efficiency
         g%irrigation_tot = g%irrigation_tot + amount
         g%irrigation_loss = g%irrigation_loss 
     :                     + amount * (1. - p%irrigation_efficiency)

 
      else
          ! soil not dry enough to require irrigation
      endif
 
      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      subroutine irrigate_ONtick ()
*     ===========================================================
      use IrrigateModule
      implicit none
      include 'event.pub'
      include 'error.pub'
 
*+  Purpose
*     Update internal time record and reset daily state variables.
 
*+  Mission Statement
*     Update internal time record and reset daily state variables.
 
*+  Changes
*     NIH 250899

*+  Local Variables
      character temp1*5
      integer   temp2
 
*+  Constant Values
      character*(*) my_name            ! name of current procedure
      parameter (my_name = 'irrigate_ONtick')
 
*- Implementation Section ----------------------------------
      call push_routine (my_name)

      ! Note that time and timestep information is not required
      ! and so dummy variables are used in their place.

      call handler_ONtick(g%day, g%year, temp1, temp2)
 
      g%irrigation_applied = 0.0
      g%irrigation_tot = 0.0
      g%irrigation_loss = 0.0

      g%carry_over = 0.0
 
      call pop_routine (my_name)
      return
      end
 
 
 
* ====================================================================
       subroutine irrigate_check_variables ()
* ====================================================================
      use IrrigateModule
      implicit none
      include 'const.inc'
      include 'data.pub'
      include 'error.pub'
 
*+  Purpose
*      Check the value of parameters or state variables
*      for validity.
 
*+  Mission Statement
*     Check the value of parameters and state variables
 
*+  Changes
*     10-11-1997 - neil huth - Programmed and Specified
*     20/10/99 - dph - beefed up the error messages.
 
*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'irrigate_check_variables')
 
*+  Local Variables
      real       profile_depth           ! total soil profile depth
      character  msg*100
 
*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      if (p%automatic_irrigation .eq. 'on') then
 
         profile_depth = sum_real_array (g%dlayer, max_layer)
 
         if (p%asw_depth .gt. profile_depth) then
            write (msg, '(3a,f8.1,2a,f8.1)' )
     :      'ASW_depth for automatic irrigation must not ' //
     :      'exceed profile depth.',
     :      new_line,
     :      'ASW_depth=',
     :      p%asw_depth,
     :      new_line,
     :      'Profile depth=',
     :      profile_depth 
            call fatal_error (Err_User, msg)
         else
            ! No problems here
         endif
 
         if (p%asw_depth .le. 0.0) then
            write (msg, '(3a,f8.1)' )
     :      'ASW_depth for automatic irrigation must not '//
     :      'be zero or negetive.',
     :      new_line,
     :      'ASW_depth=',
     :      p%asw_depth
            call fatal_error (Err_User, msg)
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
      use IrrigateModule
      implicit none
      include    'const.inc'
      include 'event.inc'
      include 'postbox.pub'
      include 'data.pub'
      include 'intrface.pub'
      include 'error.pub'

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
 
*- Implementation Section ----------------------------------
      call push_routine (my_name)
      if (amount.ge.0.) then

         call irrigate_check_allocation(amount)

         call new_postbox ()
 
         call post_real_var   (DATA_irrigate_amount
     :                        ,'(mm)'
     :                        , amount*p%irrigation_efficiency)
 
         call post_real_var   (DATA_irrigate_duration
     :                        ,'(min)'
     :                        , p%default_duration)
 
         call post_char_var   (DATA_irrigate_time
     :                        ,'(hh:mm)'
     :                        , p%default_time)
 
 
         call event_send(EVENT_irrigated) 
         call delete_postbox ()
 
         g%irrigation_applied = g%irrigation_applied 
     :                        + amount * p%irrigation_efficiency
         g%irrigation_tot = g%irrigation_tot + amount
         g%irrigation_loss = g%irrigation_loss 
     :                     + amount * (1. - p%irrigation_efficiency)

      else
         call fatal_error (ERR_User,'negative irrigation amount')
      endif
 
      call pop_routine (my_name)
      return
      end
  
 
*     ===========================================================
      subroutine irrigate_check_allocation (amount)
*     ===========================================================
      use IrrigateModule
      implicit none
      include 'error.pub'

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
 
      if (p%irrigation_allocation.eq.'on') then

         if (amount.gt.g%allocation) then

            write(ReportString,'(1x,A,f6.2,A,f6.2,A)')
     :       ' Irrigation of ',amount
     :       ,' mm reduced to remaining allocation of '
     :       ,g%allocation, ' mm'

            call Write_string (ReportString)
            amount = g%allocation
         else
         endif

         g%allocation = g%allocation - amount

      else
      endif

      call pop_routine (my_name)
      return
      end

*     ===========================================================
      subroutine irrigate_fasw (fasw, swdef)
*     ===========================================================
      use IrrigateModule
      implicit none
      include   'const.inc'
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
 
      nlayr = get_cumulative_index_real (p%asw_depth, g%dlayer
     :                                 , max_layer)
      cumdep = sum_real_array (g%dlayer, nlayr)
 
      excess_fr = divide ((cumdep - p%asw_depth) ,g%dlayer(nlayr), 0.0)
 
cnh note that results may be strange if swdep < ll15
      avail_sw  = (sum_real_array (g%sw_dep, nlayr)
     :          - excess_fr * g%sw_dep(nlayr))
     :          - (sum_real_array (g%ll15_dep, nlayr)
     :          - excess_fr * g%ll15_dep(nlayr))
     :          + g%irrigation_applied
 
      pot_avail_sw = (sum_real_array (g%dul_dep, nlayr)
     :             - excess_fr * g%dul_dep(nlayr))
     :             - (sum_real_array (g%ll15_dep, nlayr)
     :             - excess_fr * g%ll15_dep(nlayr))
 
      fasw = divide (avail_sw, pot_avail_sw, 0.0)
      swdef = l_bound(pot_avail_sw - avail_sw, 0.0)

      call pop_routine (my_name)
      return
      end
 
 
*     ===========================================================
      subroutine irrigate_on_new_solute ()
*     ===========================================================
      use IrrigateModule
      implicit none
      include 'const.inc' 
      include 'event.inc'
      include 'error.pub'
      include 'read.pub'
      include 'intrface.pub'
 
*+  Purpose
*     Add new solute to internal list of system solutes
 
*+  Mission Statement
*      Add new solute information to list of system solutes
 
*+  Changes
*       170599 nih - specified
 
*+  Constant Values
      character  my_name*(*)           ! this subroutine name
      parameter (my_name = 'irrigate_on_new_solute')

      character  section_name*(*)
      parameter (section_name = 'parameters')
 
*+  Local Variables
      integer numvals
      integer num_irrigs
      character names(max_solutes)*32
      character sender*(max_module_name_size)
      integer counter1
      integer counter2
      real       temp_solute(max_irrigs)! temp solute array (kg/ha)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)

      call collect_char_var (DATA_sender
     :                      ,'()'
     :                      ,sender
     :                      ,numvals)

      call collect_char_array (DATA_new_solute_names
     :                        ,max_solutes
     :                        ,'()'
     :                        ,names
     :                        ,numvals)

      if (g%num_solutes+numvals.gt.max_solutes) then
         call fatal_error (ERR_Internal
     :                    ,'Too many solutes for Soilwat2')
      else

         do 100 counter1 = 1, numvals

            g%num_solutes = g%num_solutes + 1
            g%solute_names(g%num_solutes) = names(counter1)
            g%solute_owners(g%num_solutes) = sender

            call read_real_array_optional (
     :              section_name         ! Section header
     :            , g%solute_names(g%num_solutes) ! Keyword
     :            , max_irrigs           ! array size
     :            , '(kg/ha)'            ! Units
     :            , temp_solute          ! Variable
     :            , num_irrigs           ! Number of values returned
     :            , 0.0                  ! Lower Limit for bound checking
     :            , 1000.)               ! Upper Limit for bound checking

               do 50 counter2=1, num_irrigs
                  g%irrigation_solutes(g%num_solutes,counter2) =
     :                                   temp_solute(counter2)

   50          continue


  100    continue
      endif

      call pop_routine (my_name)
      return
      end
 
