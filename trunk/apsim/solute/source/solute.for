*     ===========================================================
      character*(*) function solute_version ()
*     ===========================================================


*   Short description:
*       return version number of solute module

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

*   Calls:
*       none

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
*       none

*   Internal variables
*       none

*   Constant values

      character  version_number*(*)    ! version number of module
      parameter (version_number = 'V1.21 04/04/97')
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'solute_version')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (myname)

      solute_version = version_number

      call pop_routine (myname)
      return
      end
* ====================================================================
       subroutine APSIM_solute (Action, Data_string)
* ====================================================================

*   Short description:
*      This routine is the interface between the main system and the
*      solute module.

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

*   Calls:
*       solute_zero_variables
*       solute_Init
*       solute_get_other_variables
*       solute_Process
*       solute_set_other_variables
*       solute_Send_my_variable
*       Send_variable_value
*       split_line
*       solute_set_my_variable

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
       character Action*(*)            ! Message action to perform
       character Data_string*(*)       ! Message data

*   Global variables
       include 'const.inc'             ! Global constant definitions
*       include 'solute.inc'          ! solute common block
       character*20 solute_version   ! function

*   Internal variables
      character Module_name*32         ! name of this module

*   Constant values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'apsim_solute')


*   Initial data values
*      None

* --------------------- Executable code section ----------------------

      call push_routine (myname)

      if (Action.eq.MES_Presence) then
         call get_current_module (Module_name)
         print*,Module_Name,' ',solute_version()

      else if (Action.eq.MES_Init) then
         call solute_Init ()

      else if (Action.eq.MES_Process) then
         call solute_get_other_variables ()

      else if (Action.eq.MES_Get_variable) then
         call solute_Send_my_variable (Data_string)

      else if (Action.eq.MES_Set_variable) then
         call Solute_Set_my_variable (data_string)

      else
         ! Don't use message
         call Message_Unused ()
      endif

      call pop_routine (myname)
      return
      end
* ====================================================================
       subroutine solute_Init ()
* ====================================================================

*   Short description:
*      Initialise solute module

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
*     solute_version
*     solute_get_other_variables
*     rep_evnt

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
*      none

*   Global variables
       include 'const.inc'             ! Constant definitions
*       include 'solute.inc'            ! solute model common
       character solute_version*15     ! function

*   Internal variables
       character Event_string*40       ! String to output

*   Constant values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'solute_init')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      call push_routine (myname)

      call solute_zero_variables ()

      call solute_get_other_variables ()

      ! Notify system that we have initialised

      Event_string = 'Initialising Version : ' // solute_version()
      call report_event (Event_string)

      ! Get all parameters from parameter file

      call solute_read_constants ()

      call solute_read_param ()


      call pop_routine (myname)
      return
      end
* ====================================================================
       subroutine solute_zero_variables ()
* ====================================================================

*   Short description:
*     Set all variables to initial state.  i.e. zero or blank.

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
       include 'solute.inc'          ! solute common block

*   Internal variables
       integer layer
       integer solnum

*   Constant values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'solute_zero_variables')


*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      call push_routine (myname)

      do 200 solnum = 1, max_solutes
         do 100 layer = 1, max_layer
            g_solute(solnum,layer) = 0.0
  100    continue
         p_solute_names(solnum) = ' '
  200 continue

      call pop_routine (myname)
      return
      end
* ====================================================================
       subroutine solute_get_other_variables ()
* ====================================================================

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
       include 'const.inc'             ! Constant definitions
       include 'solute.inc'            ! solute common block

*   Internal variables
       integer numvals              ! number of values returned

*   Constant values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'solute_get_other_variables')


*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      call push_routine (myname)

      call Get_real_array (
     :      unknown_module, ! Module that responds (Not Used)
     :      'dlayer',       ! Variable Name
     :      max_layer,      ! Array Size
     :      '(mm)',         ! Units                (Not Used)
     :      g_dlayer,       ! Variable
     :      numvals,        ! Number of values returned
     :      0,              ! Lower Limit for bound checking
     :      1000.)          ! Upper Limit for bound checking


      call pop_routine (myname)
      return
      end
* ====================================================================
       subroutine solute_Send_my_variable (Variable_name)
* ====================================================================

*   Short description:
*       Return the value of one of our variables to caller.  The
*       variable is either the solute names or the solute information.
*       Solute information is stored in a two dimensional array
*       so for requested solute, read layer information into a
*       single dimension array and send to the system.

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
*    130596 NIH added check for num_layers=0

*   Calls:
*       none

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
       character Variable_name*(*)     ! (INPUT) Variable name to search for

*   Global variables
       include 'const.inc'             ! constant definitions
       include 'solute.inc'            ! solute Common block
       integer count_of_real_vals

*   Internal variables
       integer layer
       integer num_layers
       integer solnum
       real sol(max_layer)
       logical found

*   Constant values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'solute_send_my_variable')


*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      call push_routine (myname)

      if (variable_name .eq. 'solute_names') then

         call respond2get_char_array (
     :               variable_name,
     :               '()',
     :               p_solute_names,
     :               g_num_solutes)

      else

         found = .false.

         do 200 solnum = 1,g_num_solutes

            if (Variable_name .eq. p_solute_names(solnum)) then

               num_layers = count_of_real_vals(g_dlayer,max_layer)

               if (num_layers.eq.0) then
                  ! water balance is not initialised yet
                  num_layers = 1
               else
               endif

               do 100 layer = 1,max_layer
                  sol(layer) = g_solute(solnum,layer)
  100          continue

               call respond2get_real_array (
     :               p_solute_names(solnum),
     :               '(kg/ha)',
     :               sol,
     :               num_layers)

               found = .true.

            else

            endif
  200    continue

         if (.not. found) then
            ! We have checked all solutes and we did not respond to anything
            call Message_Unused ()
         else
            ! we found the solute so no message unused flag needed
         endif

      endif

      call pop_routine (myname)
      return
      end
*     ===========================================================
      subroutine solute_read_param ()
*     ===========================================================


*   Short description:
*       Read in all parameters from parameter file.  Solute information
*       is stored in a two dimensional array so for each solute, read
*       layer information into a single dimension array and insert
*       into the two dimensional array.

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
*       NIH specified and coded


*   Calls:
*       a_fertil_version
*       bndchk
*       close
*       count
*       counti
*       getary
*       int
*       lastnb
*       openf
*       popsr
*       push_routine
*       return_logical_unit
*       set
*       write_string

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include 'const.inc'              ! new_line, lu_scr_sum, blank
                                       ! lu_summary_file

      include 'solute.inc'

*   Internal variables
      integer    layer
      integer    solnum
      integer    numvals               ! number of values read
      real       sol(max_layer)

*   Constant values

      character  myname*(*)            ! name of this procedure
      parameter (myname = 'solute_read_param')

      character section_name*(*)
      parameter (section_name = 'parameters')

*   Initial data values


* --------------------- Executable code section ----------------------

      call push_routine (myname)

      ! Read in solute name from parameter file
      !         -----------
         call read_char_array (
     :           section_name,        ! Section header
     :           'solute_names',      ! Keyword
     :           max_solutes,         ! array size
     :           '()',                ! Units
     :           p_solute_names,      ! Array
     :           g_num_solutes)       ! Number of values returned


      do 200 solnum = 1, g_num_solutes


         if (p_solute_names(solnum).ne.blank) then

      !     Read in solute in profile from parameter file
      !             -----------------
            call read_real_array (
     :           section_name,          ! Section header
     :           p_solute_names(solnum),! Keyword
     :           max_layer,             ! array size
     :           '()',                  ! Units
     :           sol,                   ! Array
     :           numvals,               ! Number of values returned
     :           c_lb_solute,           ! Lower Limit for bound checking
     :           c_ub_solute)           ! Upper Limit for bound checking

            do 100 layer = 1, numvals
               g_solute(solnum,layer) = sol(layer)
  100       continue
         else
            ! solute is blank so ignore it.
         endif
  200 continue

      call pop_routine (myname)
      return
      end
* ====================================================================
       subroutine solute_set_my_variable (Variable_name)
* ====================================================================

*   Short description:
*       Set one of our variables altered by some other module.
*       Solute information is stored in a two dimensional array
*       so for desired solute, read updated layer information into a
*       single dimension array and update into the two dimensional
*       array.

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
*    070696 nih changed respond2set calls to collect calls

*   Calls:
*       none

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      character Variable_name*(*)      ! (INPUT) Variable name to search for

*   Global variables
      include 'const.inc'
      include 'solute.inc'             ! solute common block

*   Internal variables
      integer layer
      integer solnum
      integer numvals                  ! number of values returned
      real sol(max_layer)
      real dlt_sol(max_layer)
      logical found

*   Constant values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'solute_set_my_variable')


*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      call push_routine (myname)
      if (g_num_solutes .eq. 0) then
         call Message_Unused ()
      else
         found = .false.

         do 200 solnum = 1, g_num_solutes

            if (Variable_name .eq. p_solute_names(solnum)) then

               call collect_real_array (
     :                Variable_name,       ! variable name
     :                max_layer,           ! array size
     :                '(kg/ha)',           ! units
     :                sol,                 ! array
     :                numvals,             ! number of elements returned
     :                c_lb_solute,         ! lower bound
     :                c_ub_solute)         ! upper bound

               do 100 layer = 1, numvals
                  g_solute (solnum,layer) = sol(layer)
  100          continue
               found = .true.

            elseif (Variable_name .eq. 'dlt_'//
     :                p_solute_names(solnum)) then

               call collect_real_array (
     :                Variable_name,        ! variable name
     :                max_layer,
     :                '(kg/ha)',           ! units
     :                dlt_sol,             ! array
     :                numvals,             ! number of elements returned
     :                -c_ub_solute,
     :                c_ub_solute)

               do 150 layer = 1, numvals
                  g_solute (solnum,layer) = g_solute(solnum,layer)
     :                               + dlt_sol(layer)
  150          continue
               found = .true.
            else
               ! Don't know this variable name
            endif

  200    continue

         if (.not. found) then
            call Message_Unused ()
         else
            ! we found the variable so no message unused flag needed
         endif

      endif

      call pop_routine (myname)
      return
      end
* ====================================================================
       subroutine solute_read_constants ()
* ====================================================================

*   Short description:
*      Read in all constants from ini file.

*   Assumptions:
*      None

*   Notes:
*      None

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*     17-03-1997 - huth - Programmed and Specified

*   Calls:
*     Pop_routine
*     Push_routine

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
*      none

*   Global variables
      include 'const.inc'
      include 'solute.inc'             ! solute common block

*   Internal variables
      integer    numvals               ! number of values read from file

*   Constant values
      character*(*) section_name
      parameter (section_name = 'constants')

      character*(*) myname               ! name of current procedure
      parameter (myname = 'solute_read_constants')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------
      call push_routine (myname)

      call write_string (lu_scr_sum
     :                 ,new_line//'   - Reading Constants')


      call read_real_var (
     :           section_name         ! Section header
     :         , 'ub_solute'          ! Keyword
     :         , '()'                 ! Units
     :         , c_ub_solute          ! Variable
     :         , numvals              ! Number of values returned
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 1E10)                ! Upper Limit for bound checking

      call read_real_var (
     :           section_name         ! Section header
     :         , 'lb_solute'          ! Keyword
     :         , '()'                 ! Units
     :         , c_lb_solute          ! Variable
     :         , numvals              ! Number of values returned
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 1E10)                ! Upper Limit for bound checking


      call pop_routine (myname)
      return
      end
