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
      parameter (version_number = 'V1.20 07/06/96')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      solute_version = version_number

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
       include 'solute.inc'          ! solute common block
       character*20 solute_version   ! function

*   Internal variables
      character Module_name*32         ! name of this module

*   Constant values
*      None

*   Initial data values
*      None

* --------------------- Executable code section ----------------------

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
       include 'solute.inc'            ! solute model common
       character solute_version*15     ! function

*   Internal variables
       character Event_string*40       ! String to output

*   Constant values
       character ID_Init*(*)          ! Message indicating initialisation
       parameter (ID_Init='Initialising')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      call solute_zero_variables ()

      call solute_get_other_variables ()

      ! Notify system that we have initialised

      Event_string = 'Initialising Version : ' // solute_version()
      call report_event (Event_string)

      ! Get all parameters from parameter file

      call solute_read_param ()

      return
      end
* ====================================================================
       subroutine solute_zero_variables ()
* ====================================================================

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
       include 'solute.inc'          ! solute common block

*   Internal variables
       integer layer
       integer solnum

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------
      do 200 solnum = 1, max_solutes
         do 100 layer = 1, max_layer
            solute(solnum,layer) = 0.0
  100    continue
         solute_names(solnum) = ' '
  200 continue

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
*     none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------


c      call Get_integer_var(
c     :      unknown_module, ! Module that responds (Not Used)
c     :      'year'  ,       ! Variable Name
c     :      '()',           ! Units                (Not Used)
c     :      g_year,         ! Variable
c     :      numvals,        ! Number of values returned
c     :      1800,           ! Lower Limit for bound checking
c     :      2000)           ! Upper Limit for bound checking
c
c      call Get_integer_var(
c     :      unknown_module, ! Module that responds (Not Used)
c     :      'day',          ! Variable Name
c     :      '()',           ! Units                (Not Used)
c     :      g_day,          ! Variable
c     :      numvals,        ! Number of values returned
c     :      0,              ! Lower Limit for bound checking
c     :      366)            ! Upper Limit for bound checking
c
      call Get_real_array (
     :      unknown_module, ! Module that responds (Not Used)
     :      'dlayer',       ! Variable Name
     :      max_layer,      ! Array Size
     :      '(mm)',         ! Units                (Not Used)
     :      g_dlayer,       ! Variable
     :      numvals,        ! Number of values returned
     :      0.0,            ! Lower Limit for bound checking
     :      1000.)          ! Upper Limit for bound checking


      return
      end
* ====================================================================
       subroutine solute_Send_my_variable (Variable_name)
* ====================================================================

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

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      if (variable_name .eq. 'solute_names') then
         
         call respond2get_char_array (
     :               variable_name,
     :               '()',
     :               solute_names,
     :               num_solutes)

      else

         do 200 solnum = 1,num_solutes

            if (Variable_name .eq. solute_names(solnum)) then

               num_layers = count_of_real_vals(g_dlayer,max_layer)

               if (num_layers.eq.0) then
                  ! water balance is not initialised yet
                  num_layers = 1
               else
               endif

               do 100 layer = 1,max_layer
                  sol(layer) = solute(solnum,layer)
  100          continue

               call respond2get_real_array (
     :               solute_names(solnum),
     :               '(kg/ha)',
     :               sol,
     :               num_layers)

               goto 300
            else

            endif
  200    continue

         ! We have checked all solutes and we did not respond to anything
         call Message_Unused ()

  300    continue

      endif

      return
      end
*     ===========================================================
      subroutine solute_read_param ()
*     ===========================================================


*   Short description:
*       get fertiliser schedule to apply

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
     :           solute_names,        ! Array
     :           num_solutes)         ! Number of values returned


      do 200 solnum = 1, num_solutes


         if (solute_names(solnum).ne.blank) then

      !     Read in solute in profile from parameter file
      !             -----------------
            call read_real_array (
     :           section_name,        ! Section header
     :           solute_names(solnum), ! Keyword
     :           max_layer,           ! array size
     :           '()',                ! Units
     :           sol,                 ! Array
     :           numvals,             ! Number of values returned
     :           0.0,                 ! Lower Limit for bound checking
     :           1000.0)              ! Upper Limit for bound checking

            do 100 layer = 1, numvals
               solute(solnum,layer) = sol(layer)
  100       continue
         else
            goto 300
         endif
  200 continue
  300 continue

      call pop_routine (myname)
      return
      end
* ====================================================================
       subroutine solute_set_my_variable (Variable_name)
* ====================================================================

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
      real    sol(max_layer)

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      do 200 solnum = 1, num_solutes

         if (Variable_name .eq. solute_names(solnum)) then

            call collect_real_array (
     :                Variable_name,       ! variable name
     :                max_layer,
     :                '(kg/ha)',           ! units
     :                sol,                 ! array
     :                numvals,             ! number of elements returned
     :                0.0,
     :                1000.)

            do 100 layer = 1, numvals
               solute (solnum,layer) = sol(layer)
  100       continue
            
            goto 300
         elseif (Variable_name .eq. 'dlt_'//solute_names(solnum)) then

            call collect_real_array (
     :                Variable_name,        ! variable name
     :                max_layer,
     :                '(kg/ha)',           ! units
     :                sol,                 ! array
     :                numvals,             ! number of elements returned
     :               -1000.,
     :                1000.)

            do 150 layer = 1, numvals
               solute (solnum,layer) = solute(solnum,layer)
     :                               + sol(layer)
  150       continue
            
            goto 300

         else
            ! Don't know this variable name
         endif

  200 continue

      ! We have checked all solutes and we did not respond to anything
      call Message_Unused ()

  300 continue

      return
      end
