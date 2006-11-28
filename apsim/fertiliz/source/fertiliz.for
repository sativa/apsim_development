      module FertilizModule
      use Registrations
!     ================================================================
!     Fertiliz_array_sizes and constants
!     ================================================================

!   Short description:
!      array size settings and constants

!   Notes:
!      none

!   Attributes:
!      Version:         Any hardware/Fortran77
!      Extensions:      Long names <= 20 chars.
!                       Lowercase
!                       Underscore
!                       Inline comments
!   Changes:
!      2/11/94 - nih
!      2/11/94 - nih
!      2/11/94 - nih
!      080195    jngh
!      300695    jngh changed max_layer form 11 to 100
!      27/09/99  ew changed for instatiation


! ----------------------- Declaration section ------------------------

! ====================================================================
!   Parameter values
      integer    max_layer
      parameter (max_layer = 100)

      integer    max_fert
      parameter (max_fert = 50)

! ====================================================================
      type FertilizGlobals
        sequence
        integer    year                  ! year
        integer    day                   ! day of year

        real       dlayer(max_layer)     ! depth of each profile layer (mm)
        real       fert_applied          ! amount of fertilizer applied today(kg/ha)

      end type FertilizGlobals


      type FertilizParameters
       sequence
       integer    fert_year(max_fert)
       integer    fert_day(max_fert)      ! day of year of fertilizer application
       character  fert_type(max_fert)*32  ! code number for type of fertilizer
       real       fert_depth(max_fert)    ! depth of incorporation of fertilizer (mm)
       real       fert_amount(max_fert)   ! amount of fertilizer nitrogen added fert_day(j)(kg n/ha)

      end type FertilizParameters


! ====================================================================

      ! instance variables.
      common /InstancePointers/ ID,g,p,c
      save InstancePointers
      type (FertilizGlobals),pointer :: g
      type (FertilizParameters),pointer :: p
      type (IDsType),pointer :: id


      contains


*     ===========================================================
      subroutine fertiliz_fertilize ()
*     ===========================================================
      Use infrastructure
      implicit none
c     include   'fertiliz.inc'         ! fertiliz common block

*+  Purpose
*      This routine deciphers the data string and applies the fertiliser.

*+  Mission Statement
*     Apply the specified fertilizer

*+  Changes
*    060696 nih removed data_string argument
*               replace extract routines with collect routine calls

*+  Constant Values
      character  my_name*(*)
      parameter (my_name = 'fertiliz_fertilize')

*+  Local Variables
      real       amount                ! amount of fertilizer added (kg/ha)
      real       depth                 ! depth of fertilizer added (mm)
      integer    numvals_amount        ! number of values read from data
                                       ! string
      integer    numvals_depth         ! number of values read from data
                                       ! string
      integer    numvals_type          ! number of values read from data
                                       ! string
      character  type*32               ! type of fertilizer added (flag)

*- Implementation Section ----------------------------------
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
      end subroutine



*     ===========================================================
      subroutine fertiliz_Init ()
*     ===========================================================
      Use infrastructure
      implicit none
c     include   'fertiliz.inc'         ! fertiliz model common

*+  Purpose
*      Initialise fertiliz module

*+  Mission Statement
*     Initialise the module state

*+  Changes
*     <insert here>
*     dph 18/10/99 removed call to get_other_variables

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'fertiliz_init')

*+  Local Variables
      character  Event_string*40       ! String to output

*- Implementation Section ----------------------------------
      call push_routine (my_name)

         ! Notify system that we have initialised

      Event_string = ' Initialising '
      call Write_string (Event_string)

         ! Get all parameters from parameter file

      call fertiliz_read_param ()

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine fertiliz_zero_variables ()
*     ===========================================================
      Use infrastructure
      implicit none
c     include    'fertiliz.inc'        ! fertiliz common block

*+  Purpose
*     Set all variables in this module to zero.

*+  Mission Statement
*     Initialise module state variables

*+  Changes
*     <insert here>

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'fertiliz_zero_variables')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      g%day = 0
      g%year = 0
      g%fert_applied = 0.0

      call fill_real_array    (g%dlayer, 0.0, max_layer)
      call fill_integer_array (p%fert_day, 0, max_fert)
      call fill_integer_array (p%fert_year, 0, max_fert)
      call fill_char_array    (p%fert_type, ' ', max_fert)
      call fill_real_array    (p%fert_depth, 0.0, max_fert)
      call fill_real_array    (p%fert_amount, 0.0, max_fert)

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine fertiliz_get_other_variables ()
*     ===========================================================
      Use infrastructure
      implicit none
c     include   'fertiliz.inc'         ! fertiliz common block

*+  Purpose
*      Get the values of variables from other modules

*+  Mission Statement
*     Get required state variables from other modules

*+  Changes
*     <insert here>

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'fertiliz_get_other_variables')

*+  Local Variables
      integer    numvals               ! number of values returned

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine fertiliz_Send_my_variable (Variable_name)
*     ===========================================================
      Use infrastructure
      implicit none
c     include   'fertiliz.inc'         ! fertiliz Common block

*+  Sub-Program Arguments
      character  Variable_name*(*)     ! (INPUT) Variable name to search for

*+  Purpose
*      Return the value of one of our variables to caller

*+  Mission Statement
*     Provide information to requesting module

*+  Changes
*      011195 jngh  added call to message_unused

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'fertiliz_send_my_variable')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Variable_name .eq. 'fertilizer') then

         call respond2get_real_var (
     :                              variable_name
     :                            , '(kg/ha)'
     :                            , g%fert_applied)

      else
         call Message_unused ()
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine fertiliz_apply (amount, depth, type)
*     ===========================================================
      Use infrastructure
      implicit none
c     include   'fertiliz.inc'

*+  Sub-Program Arguments
      real       amount                !
      real       depth                 !
      character  type*(*)              !

*+  Purpose
*       apply fertiliser as directed

*+  Mission Statement
*     Pass the fertilizer information to other modules

*+  Changes
*       9-6-94 nih adapted from jngh's fertil module
*      27-5-96 nih changed call get_last_module to get_posting_module
*       6-6-96 nih changed set_real_array to use post_Real_array construct

*+  Constant Values
      character  myname*(*)            ! procedure name
      parameter (myname = 'fertiliz_apply')
*
      integer    mxcomp                ! max no of components allowed in
      parameter (mxcomp = 20)          ! fertilizer spec.

*+  Local Variables
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
      integer    owner_module          ! module that owns 'array'
      character  string*200            ! output string

*- Implementation Section ----------------------------------

      call push_routine (myname)

      if (amount.gt.0.0) then

         call write_string (new_line//
     :   '   - Reading Fertiliser Type Parameters')

            ! find the layer that the fertilizer is to be added to.
         layer = get_cumulative_index_real (depth, g%dlayer, max_layer)

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

               Owner_module = Get_posting_Module ()

               call fill_real_array (delta_array, 0.0, max_layer)
               delta_array(layer) = amount * fraction(counter)

               dlt_name = 'dlt_'//components(counter)
               call set_real_array (
     :                      Owner_module
     :                    , dlt_name
     :                    , '(kg/ha)'
     :                    , delta_array
     :                    , array_size)

            else
               ! nobody knows about this component - forget it!
            endif

  100    continue

         g%fert_applied = g%fert_applied + amount

         write (string, '(1x, f7.2, 6a, 41x, a, f7.2, a, i3, a)')
     :             amount,
     :             ' of ',
     :             trim(full_name),
     :             ' (',
     :             trim(type),
     :             ')',
     :             new_line,
     :             'added at depth ',
     :             depth,
     :             ' (layer ',
     :             layer,
     :             ')'

        call Write_string (string)

      else
            ! we have no fertiliser applied
      endif

      call pop_routine (myname)
      return
      end subroutine



*     ===========================================================
      subroutine fertiliz_schedule ()
*     ===========================================================
      Use infrastructure
      implicit none
c     include   'fertiliz.inc'

*+  Purpose
*       fertiliser management.

*+  Mission Statement
*     Check the internal fertilizer schedule

*+  Changes
*      9/6/94 nih adapted from old fertil module

*+  Constant Values
      character  myname*(*)            ! name of this module
      parameter (myname = 'fertiliz_schedule')

*+  Local Variables
      integer    fertno                ! loop counter for input
      integer    nfert                 ! number of fertiliser applications

*- Implementation Section ----------------------------------

      call push_routine (myname)

      nfert = count_of_integer_vals (p%fert_day, max_fert)

      if (nfert.gt.0) then

             ! we have a schedule.  see if we have fertiliser today.

         do 1000 fertno = 1, nfert
            if (g%day .eq. p%fert_day(fertno) .and.
     :          g%year .eq. p%fert_year(fertno)) then

               call fertiliz_apply (
     :                               p%fert_amount(fertno)
     :                             , p%fert_depth(fertno)
     :                             , p%fert_type(fertno)  )

            else
            endif
1000     continue
      else

      endif

      call pop_routine (myname)
      return
      end subroutine



*     ===========================================================
      subroutine fertiliz_read_param ()
*     ===========================================================
      Use infrastructure
      implicit none
c     include 'fertiliz.inc'

*+  Purpose
*       get fertiliser schedule to apply

*+  Mission Statement
*     Read module parameters

*+  Changes
*       NIH specified and coded
*       14-7-94 NIH - changed key names to match documentation.
*     210395 jngh changed from unknown_section to a defined section

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'fertiiliz_read_param')
*
      character section_name*(*)
      parameter (section_name = 'parameters')

*+  Local Variables
      integer    k                     ! loop counter for report
      integer    nfert                 ! number of fertiliser applications
      integer    numdays               ! number of fertilizer days read
      integer    numvals               ! number of values read
      character  string*200            ! output string

*- Implementation Section ----------------------------------

      call push_routine (myname)

      call write_string (new_line//
     :     '   - Reading Parameters')


      ! Read in fertiliser schedule from parameter file
      !         -------------------
         call read_integer_array_optional (
     :           section_name         ! Section header
     :         , 'day'                ! Keyword
     :         , max_fert             ! array size_of
     :         , '()'                 ! Units
     :         , p%fert_day           ! Array
     :         , numdays              ! Number of values returned
     :         , 1                    ! Lower Limit for bound checking
     :         , 366)                 ! Upper Limit for bound checking

         call read_integer_array_optional (
     :           section_name         ! Section header
     :         , 'year'               ! Keyword
     :         , max_fert             ! array size_of
     :         , '()'                 ! Units
     :         , p%fert_year          ! Array
     :         , numvals              ! Number of values returned
     :         , 1800                 ! Lower Limit for bound checking
     :         , 2100)                ! Upper Limit for bound checking

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
     :         , p%fert_amount        ! Array
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
     :         , p%fert_type          ! Array
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
     :         , p%fert_depth         ! Array
     :         , numvals              ! Number of values returned
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 1000.0)               ! Upper Limit for bound checking
         if (numvals.ne.numdays) then
            call fatal_error(err_user,
     :               'incorrect no. of fertilizer depths specified')
         else
         endif

             ! report fertiliser settings

      call write_string (new_line//new_line)

      string = '               Fertiliser Schedule (kg/ha)'
      call write_string (string)

      string = '     -----------------------------------------------'
      call write_string (string)


      nfert = count_of_integer_vals (p%fert_day, max_fert)

      if (nfert.eq.0) then
         string = '      No fertiliser schedule is used'
         call write_string (string)

      else

         string = '      day  year  amount    type'
         call write_string (string)

         string = '     -----------------------------------------------'
         call write_string (string)

         do 1000 k = 1, nfert

            write (string, '(5x, i4, 1x, i5, f8.1, 1x, a40)')
     :          p%fert_day(k), p%fert_year(k), p%fert_amount(k),
     :          p%fert_type(k)
            call write_string (string)

1000     continue

      endif

      string = '     -----------------------------------------------'
      call write_string (string)

      call pop_routine (myname)
      return
      end subroutine



*     ===========================================================
      subroutine fertiliz_ONtick (variant)
*     ===========================================================
      Use infrastructure
      implicit none

      integer, intent(in) :: variant

c     include   'fertiliz.inc'

*+  Purpose
*     Update internal time record and reset daily state variables.

*+  Mission Statement
*     Update internal time record and reset daily state variables.

*+  Changes
*        150696 nih changed routine from fertiliz_prepare to
*                   fertiliz_inter_timestep.
*        260899 nih changed routine to EVENT_ONtick

*+  Local Variables
      character temp1*5
      integer   temp2
      type(timeType) :: tick

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'fertiliz_ONtick')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call unpack_time(variant, tick)
      call jday_to_day_of_year(dble(tick%startday), g%day, g%year)

      g%fert_applied = 0.0

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
      subroutine fertiliz_set_my_variable (variable_name)
* ====================================================================
      Use infrastructure
      implicit none
c     include   'fertiliz.inc'

*+  Sub-Program Arguments
      character variable_name*(*)         ! (input) variable name to search for

*+  Purpose
*      set a variable in this module as requested by another.

*+  Mission Statement
*      Set an internal variable as requested

*+  Changes
*      020498 nih

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'fertiliz_set_my_variable')

*+  Local Variables
      integer    numvals               ! number of values returned in array
      real       amount                ! amount of fertilizer to apply
      character  type*32               ! type of fertilizer to apply

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (index(variable_name,'fert_') .eq. 1) then
         type = variable_name(len('fert_')+1:)
         call collect_real_var (variable_name, '()'
     :                             , amount, numvals
     :                             , 0.0, 1000.)

         ! apply this type of fertilizer to the top layer
         ! This is a bit of a shortcut - needs improving.
         call fertiliz_apply (amount, 1.0, type)

      else
         call Message_unused ()

      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine fertiliz_ONNew_Profile ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*     Update internal soil layer structure with new data

*+  Mission Statement
*     Update internal soil layer structure with new data

*+  Changes
*        150600 nih

*+  Local Variables
      integer numvals

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'fertiliz_ONNew_Profile')

*- Implementation Section ----------------------------------
      call push_routine (myname)

         call collect_real_array
     :         (DATA_dlayer  ! Name of Variable  (not used)
     :        , max_layer    ! size of array to be set
     :        , '(mm)'       ! Units of variable (not used)
     :        , g%dlayer     ! Variable array
     :        , numvals      ! Number of elements returned
     :        , 0.0          ! Lower Limit for bound checking
     :        , 1000.0)      ! Upper Limit for bound checking

      call pop_routine (myname)
      return
      end subroutine

      end module FertilizModule


!     ===========================================================
      subroutine alloc_dealloc_instance(doAllocate)
!     ===========================================================
      use FertilizModule
      implicit none
      ml_external alloc_dealloc_instance

!+  Sub-Program Arguments
      logical, intent(in) :: doAllocate

!+  Purpose
!      Module instantiation routine.

!- Implementation Section ----------------------------------

      if (doAllocate) then
         allocate(g)
         allocate(p)
         allocate(id)
      else
         deallocate(g)
         deallocate(p)
         deallocate(id)
      end if
      return
      end subroutine




*     ===========================================================
      subroutine Main (Action, Data_string)
*     ===========================================================
      Use infrastructure
      Use FertilizModule
      implicit none
      ml_external Main


*+  Sub-Program Arguments
      character  Action*(*)            ! Message action to perform
      character  Data_string*(*)       ! Message data

*+  Purpose
*      This routine is the interface between the main system and the
*      fertiliz module.

*+  Mission Statement
*     The fertiliz main routine

*+  Changes
*      011195 jngh  added call to message_unused
*      060696 nih   removed data_string from call to fertiliz_fertiliz
*      150696 nih   changed routine call from fertiliz_prepare to
*                   fertiliz_inter_timestep.
*     dph 18/10/99  added call to get_other_variables before set_my_variable

*+  Constant Values
      character  my_name*(*)
      parameter (my_name = 'fertiliz')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

         ! initialise error flags
C      call set_warning_off ()

      if (Action.eq.ACTION_Get_variable) then
         call fertiliz_Send_my_variable (Data_string)

      else if (Action.eq.ACTION_Create) then
         call doRegistrations(id)
         call fertiliz_zero_variables ()

      else if (Action.eq.ACTION_Init) then

         call fertiliz_Init ()

      else if (Action.eq.ACTION_Process) then

         call fertiliz_get_other_variables ()
         call fertiliz_schedule ()

      else if (Action.eq.'apply' .or. Action.eq.'fertilize') then
         call fertiliz_get_other_variables ()
         call fertiliz_fertilize ()

      else if (Action .eq. ACTION_Set_variable) then
         call fertiliz_get_other_variables ()
         call fertiliz_set_my_variable (Data_string)

      else if (Action .eq. EVENT_new_profile) then
         call fertiliz_OnNew_Profile()

      else
            ! Don't use message
         call Message_unused ()

      endif

      call pop_routine (my_name)
      return
      end subroutine


! ====================================================================
! This routine is the event handler for all events
! ====================================================================
      subroutine respondToEvent(fromID, eventID, variant)
      Use infrastructure
      Use FertilizModule
      implicit none
      ml_external respondToEvent

      integer, intent(in) :: fromID
      integer, intent(in) :: eventID
      integer, intent(in) :: variant

      if (eventID .eq. id%tick) then
         call fertiliz_ONtick(variant)
      endif

      return
      end subroutine respondToEvent
