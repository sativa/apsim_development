*     ===========================================================
      character*(*) function fertiliz_version ()
*     ===========================================================

*   Short description:
*       return version number of fertiliz module

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
      parameter (my_name = 'fertiliz_version')

      character  version_number*(*)    ! version number of module
      parameter (version_number = 'V1.2 060696')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      fertiliz_version = version_number

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine APSIM_fertiliz (Action, Data_string)
*     ===========================================================

*   Short description:
*      This routine is the interface between the main system and the
*      fertiliz module.

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
*      011195 jngh  added call to message_unused
*      060696 nih   removed data_string from call to fertiliz_fertiliz

*   Calls:
*     fertiliz_zero_variables
*     fertiliz_Init
*     fertiliz_get_other_variables
*     fertiliz_Process
*     fertiliz_set_other_variables
*     fertiliz_Send_my_variable
*     message_unused
*     Send_variable_value
*     fertiliz_set_my_variable
*     pop_routine
*     push_routine
*     set_warning_off

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      character  Action*(*)            ! Message action to perform
      character  Data_string*(*)       ! Message data

*   Global variables
      include   'const.inc'            ! Global constant definitions
      include   'fertiliz.inc'         ! fertiliz common block

      character*20 fertiliz_version    ! function
      integer    lastnb                ! function

*   Internal variables
      character  Module_name*10        ! name of this module

*   Constant values
      character  my_name*(*)
      parameter (my_name = 'fertiliz')

*   Initial data values
*      None

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

         ! initialise error flags
      call set_warning_off ()

      if (Action.eq.MES_Presence) then
         call get_current_module (Module_name)
         write(*, *) 'module_name = '
     :              , module_name(:lastnb (module_name))
     :              // blank
     :              // fertiliz_version ()

      else if (Action.eq.MES_Init) then
         call fertiliz_zero_variables ()
         call fertiliz_Init ()

      else if (Action.eq.MES_Prepare) then
         call fertiliz_prepare()

      else if (Action.eq.MES_Process) then
         call fertiliz_get_other_variables ()
         call fertiliz_schedule ()

      else if (Action.eq.'apply' .or. Action.eq.'fertilize') then
         call fertiliz_get_other_variables ()
         call fertiliz_fertilize ()

      else if (Action.eq.MES_Get_variable) then
         call fertiliz_Send_my_variable (Data_string)

      else
            ! Don't use message
         call Message_unused ()

      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine fertiliz_fertilize ()
*     ===========================================================

*   Short description:
*      This routine deciphers the data string and applies the fertiliser.

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
*    060696 nih removed data_string argument
*               replace extract routines with collect routine calls

*   Calls:
*     collect_real_var
*     collect_real_var_optional
*     collect_char_var
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*     none

*   Global variables
      include   'const.inc'            ! Global constant definitions
      include   'fertiliz.inc'         ! fertiliz common block

*   Internal variables
      real       amount                ! amount of fertilizer added (kg/ha)
      real       depth                 ! depth of fertilizer added (mm)
      integer    numvals_amount        ! number of values read from data
                                       ! string
      integer    numvals_depth         ! number of values read from data
                                       ! string
      integer    numvals_type          ! number of values read from data
                                       ! string
      character  type*32               ! type of fertilizer added (flag)

*   Constant values
      character  my_name*(*)
      parameter (my_name = 'fertiliz_fertilize')

*   Initial data values
*      None

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      call collect_real_var (
     :             'amount'
     :           , '(kg/ha)'
     :           , amount
     :           , numvals_amount
     :           , 0.0
     :           , 1000.0)

      call collect_real_var_optional (
     :             'depth'
     :           , '(mm)'
     :           , depth
     :           , numvals_depth
     :           , 0.0
     :           , 1000.0)

      If (numvals_depth.eq.0) then
         depth = 0.0
      else
      endif

      call collect_char_var (
     :             'type'
     :           , '(-)'
     :           , type
     :           , numvals_type)

      if ((numvals_type.eq.0) .or. (numvals_amount.eq.0)) then
            call fatal_error (err_user
     :               ,'Fertilizer application specification error')

      else
            call fertiliz_apply (amount, depth, type)

      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine fertiliz_Init ()
*     ===========================================================

*   Short description:
*      Initialise fertiliz module

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
*     fertiliz_version
*     fertiliz_get_other_variables
*     report_event
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*      none

*   Global variables
      include   'fertiliz.inc'         ! fertiliz model common

      character  fertiliz_version*15   ! function

*   Internal variables
      character  Event_string*40       ! String to output

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'fertiliz_init')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      call fertiliz_get_other_variables ()

         ! Notify system that we have initialised

      Event_string = ' Initialising, Version : ' // fertiliz_version()
      call report_event (Event_string)

         ! Get all parameters from parameter file

      call fertiliz_read_param ()

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine fertiliz_zero_variables ()
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
*     fill_real_array
*     fill_integer_array
*     fill_char_array
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*      none

*   Global variables
      include    'fertiliz.inc'        ! fertiliz common block

*   Internal variables
*      none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'fertiliz_zero_variables')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      g_day = 0
      g_year = 0
      g_fert_applied = 0.0

      call fill_real_array (g_dlayer, 0.0, max_layer)
      call fill_real_array (g_fert_amount, 0.0, max_fert)
      call fill_integer_array (g_fert_day, 0, max_fert)
      call fill_integer_array (g_fert_year, 0, max_fert)
      call fill_char_array (g_fert_type, ' ', max_fert)
      call fill_real_array (g_fert_depth, 0.0, max_fert)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine fertiliz_get_other_variables ()
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
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*      none

*   Global variables
      include   'const.inc'            ! Constant definitions
      include   'fertiliz.inc'         ! fertiliz common block

*   Internal variables
      integer    numvals               ! number of values returned

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'fertiliz_get_other_variables')

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
     :    , 'dlayer'        ! Variable Name
     :    , max_layer       ! Array size_of
     :    , '(mm)'          ! Units                (Not Used)
     :    , g_dlayer        ! Variable
     :    , numvals         ! Number of values returned
     :    , 0.0             ! Lower Limit for bound checking
     :    , 1000.0)         ! Upper Limit for bound checking

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine fertiliz_Send_my_variable (Variable_name)
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
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      character  Variable_name*(*)     ! (INPUT) Variable name to search for

*   Global variables
      include   'fertiliz.inc'         ! fertiliz Common block

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'fertiliz_send_my_variable')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      if (Variable_name .eq. 'fertilizer') then

         call respond2get_real_var (
     :                              variable_name
     :                            , '(kg/ha)'
     :                            , g_fert_applied)

      else
         call Message_unused ()
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine fertiliz_apply (amount, depth, type)
*     ===========================================================

*   Short description:
*       apply fertiliser as directed

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
*       9-6-94 nih adapted from jngh's fertil module
*      27-5-96 nih changed call get_last_module to get_posting_module
*       6-6-96 nih changed set_real_array to use post_Real_array construct

*   Calls:
*      getmyr
*      get_cumulative_index_real
*      offset_day_of_year
*      pop_routine
*      push_routine
*      report_event
*      traceback_routine
*      warning_error
*      write_string

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       amount                !
      real       depth                 !
      character  type*(*)              !

*   Global variables
      include   'const.inc'
      include   'fertiliz.inc'

      integer    get_cumulative_index_real ! function
      integer    lastnb                ! function

*   Constant values
      character  myname*(*)            ! procedure name
      parameter (myname = 'fertiliz_apply')

      integer    mxcomp                ! max no of components allowed in
      parameter (mxcomp = 20)          ! fertilizer spec.

*   Internal variables
      real       array(max_layer)      ! array of soil variable to be
                                       ! updated with a component of the
                                       ! fertilizer being added. (kg/ha)
      integer    array_size            ! no. of elements in array
      character  Components(mxcomp)*32 ! names of components of fertilizer
      integer    Counter               ! simple counter variable
      real       delta_array(max_layer) ! delta values for 'array' (kg/ha)
      character  dlt_name*36           ! name of a compontent's delta
      real       fraction(mxcomp)      ! fractional composition of fertilizer
                                       ! components (0-1)
      character  full_name*50          ! full name of fertilizer added
      integer    layer                 ! layer number of fertiliser placement
      integer    numvals               ! number of values returned
      character  owner_module*32       ! module that owns 'array'
      character  string*200            ! output string


*   Initial data values
*     none


* --------------------- Executable code section ----------------------

      call push_routine (myname)

      if (amount.gt.0.0) then

         call write_string (lu_scr_sum
     :            ,new_line//'   - Reading Fertiliser Type Parameters')

            ! find the layer that the fertilizer is to be added to.
         layer = get_cumulative_index_real (depth, g_dlayer, max_layer)

         call read_char_var (
     :           type                 ! Section header
     :         , 'full_name'          ! Keyword
     :         , '()'                 ! Units
     :         , full_name            ! Array
     :         , numvals)             ! Number of values returned

         call read_char_array (
     :           type                 ! Section header
     :         , 'components'         ! Keyword
     :         , mxcomp               ! array size_of
     :         , '()'                 ! Units
     :         , components           ! Array
     :         , numvals)             ! Number of values returned

         call read_real_array (
     :           type                 ! Section header
     :         , 'fraction'           ! Keyword
     :         , mxcomp               ! array size_of
     :         , '()'                 ! Units
     :         , fraction             ! Array
     :         , numvals              ! Number of values returned
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 1.0)                 ! Upper Limit for bound checking

            ! this assumes that the ini file has same no. of fractions and
            ! components!!!
         do 100 counter = 1, numvals

            call Get_real_array_optional (
     :         unknown_module       ! Module that responds (Not Used)
     :       , components(counter)  ! Variable Name
     :       , max_layer            ! Array size_of
     :       , '(kg/ha)'            ! Units                (Not Used)
     :       , array                ! Variable
     :       , array_size           ! Number of values returned
     :       , 0.0                  ! Lower Limit for bound checking
     :       , 1.0e30)              ! Upper Limit for bound checking

            if (array_size .gt. 0) then
                  ! this variable is being tracked - send the delta to it

               call Get_posting_Module (Owner_module)

               call fill_real_array (delta_array, 0.0, max_layer)
               delta_array(layer) = amount * fraction(counter)

               call new_postbox()
               dlt_name = 'dlt_'//components(counter)
               call post_real_array (
     :                      dlt_name
     :                    , '(kg/ha)'
     :                    , delta_array
     :                    , array_size)
               call message_send_immediate (unknown_module
     :                                     ,MES_set_variable
     :                                     ,dlt_name)
               call delete_postbox()

            else
               ! nobody knows about this component - forget it!
            endif

  100    continue

         g_fert_applied = g_fert_applied + amount

         write (string, '(1x, f7.2, 6a, 41x, a, f7.2, a, i3, a)')
     :             amount,
     :             ' of ',
     :             Full_name(:max (1, lastnb (full_name))),
     :             ' (',
     :             type(:max (1, lastnb (type))),
     :             ')',
     :             new_line,
     :             'added at depth ',
     :             depth,
     :             ' (layer ',
     :             layer,
     :             ')'

        call report_event (string)

      else
            ! we have no fertiliser applied
      endif

      call pop_routine (myname)
      return
      end
*     ===========================================================
      subroutine fertiliz_schedule ()
*     ===========================================================

*   Short description:
*       fertiliser management.

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
*      9/6/94 nih adapted from old fertil module

*   Calls:
*      count_of_integer_vals
*      pop_routine
*      push_routine
*      rep_evnt

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'fertiliz.inc'

      integer    count_of_integer_vals ! function

*   Internal variables
      integer    fertno                ! loop counter for input
      integer    nfert                 ! number of fertiliser applications

*   Constant values
      character  myname*(*)            ! name of this module
      parameter (myname = 'fertiliz_schedule')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (myname)

      nfert = count_of_integer_vals (g_fert_day, max_fert)

      if (nfert.gt.0) then

             ! we have a schedule.  see if we have fertiliser today.

         do 1000 fertno = 1, nfert
            if (g_day .eq. g_fert_day(fertno) .and.
     :          g_year .eq. g_fert_year(fertno)) then

               call fertiliz_apply (
     :                               g_fert_amount(fertno)
     :                             , g_fert_depth(fertno)
     :                             , g_fert_type(fertno)  )

            else
            endif
1000     continue
      else

      endif

      call pop_routine (myname)
      return
      end
*     ===========================================================
      subroutine fertiliz_read_param ()
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
*       14-7-94 NIH - changed key names to match documentation.
*     210395 jngh changed from unknown_section to a defined section

*   Calls:
*       a_fertil_version
*       bndchk
*       close
*       count_of_real_vals
*       count_of_integer_vals
*       getary
*       int
*       lastnb
*       openf
*       pop_routine
*       push_routine
*       return_logical_unit
*       fill_real_array
*       write_string

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include 'const.inc'
      include 'fertiliz.inc'

      integer    count_of_integer_vals ! function

*   Internal variables
      integer    k                     ! loop counter for report
      integer    nfert                 ! number of fertiliser applications
      integer    numdays               ! number of fertilizer days read
      integer    numvals               ! number of values read
      character  string*200            ! output string

*   Constant values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'fertiiliz_read_param')

      character section_name*(*)
      parameter (section_name = 'parameters')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (myname)

      call write_string (lu_scr_sum
     :                 ,new_line//'   - Reading Parameters')


      ! Read in fertiliser schedule from parameter file
      !         -------------------
         call read_integer_array_optional (
     :           section_name         ! Section header
     :         , 'day'                ! Keyword
     :         , max_fert             ! array size_of
     :         , '()'                 ! Units
     :         , g_fert_day           ! Array
     :         , numdays              ! Number of values returned
     :         , 1                    ! Lower Limit for bound checking
     :         , 366)                 ! Upper Limit for bound checking

         call read_integer_array_optional (
     :           section_name         ! Section header
     :         , 'year'               ! Keyword
     :         , max_fert             ! array size_of
     :         , '()'                 ! Units
     :         , g_fert_year          ! Array
     :         , numvals              ! Number of values returned
     :         , 1800                 ! Lower Limit for bound checking
     :         , 2000)                ! Upper Limit for bound checking

         if (numvals.ne.numdays) then
            call fatal_error(err_user,
     :                 'incorrect no. of fertilizer years specified')
         else
         endif

         call read_real_array_optional (
     :           section_name         ! Section header
     :         , 'amount'             ! Keyword
     :         , max_fert             ! array size_of
     :         , '(kg/ha)'            ! Units
     :         , g_fert_amount        ! Array
     :         , numvals              ! Number of values returned
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 1000.0)              ! Upper Limit for bound checking
         if (numvals.ne.numdays) then
            call fatal_error(err_user,
     :               'incorrect no. of fertilizer amounts specified')
         else
         endif

         call read_char_array_optional (
     :           section_name         ! Section header
     :         , 'type'               ! Keyword
     :         , max_fert             ! array size_of
     :         , '()'                 ! Units
     :         , g_fert_type          ! Array
     :         , numvals)             ! Number of values returned
         if (numvals.ne.numdays) then
            call fatal_error(err_user,
     :               'incorrect no. of fertilizer types specified')
         else
         endif

         call read_real_array_optional (
     :           section_name         ! Section header
     :         , 'depth'              ! Keyword
     :         , max_fert             ! array size_of
     :         , '(mm)'               ! Units
     :         , g_fert_depth         ! Array
     :         , numvals              ! Number of values returned
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 1000.0)               ! Upper Limit for bound checking
         if (numvals.ne.numdays) then
            call fatal_error(err_user,
     :               'incorrect no. of fertilizer depths specified')
         else
         endif

             ! report fertiliser settings

      call write_string (lu_scr_sum, new_line//new_line)

      string = '               Fertiliser Schedule (kg/ha)'
      call write_string (lu_scr_sum, string)

      string = '     -----------------------------------------------'
      call write_string (lu_scr_sum, string)


      nfert = count_of_integer_vals (g_fert_day, max_fert)

      if (nfert.eq.0) then
         string = '      No fertiliser schedule is used'
         call write_string (lu_scr_sum, string)

      else

         string = '      day  year  amount    type'
         call write_string (lu_scr_sum, string)

         string = '     -----------------------------------------------'
         call write_string (lu_scr_sum, string)

         do 1000 k = 1, nfert

            write (string, '(5x, i4, 1x, i5, f8.1, 1x, a40)')
     :          g_fert_day(k), g_fert_year(k), g_fert_amount(k),
     :          g_fert_type(k)
            call write_string (lu_scr_sum, string)

1000     continue

      endif

      string = '     -----------------------------------------------'
      call write_string (lu_scr_sum, string)

      call pop_routine (myname)
      return
      end

*     ===========================================================
      subroutine fertiliz_prepare ()
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
      include   'fertiliz.inc'

*   Internal variables
*      none

*   Constant values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'fertiliz_prepare')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------
      call push_routine (myname)

      g_fert_applied = 0.0

      call pop_routine (myname)
      return
      end
