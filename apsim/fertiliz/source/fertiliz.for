C     Last change:  IH   27 Sep 1999    3:34 pm

      include  'Fertiliz.inc'            ! Global constant definitions

!     ===========================================================
      subroutine AllocInstance (InstanceName, InstanceNo)
!     ===========================================================
      use FertilizModule
      implicit none
 
!+  Sub-Program Arguments
      character InstanceName*(*)       ! (INPUT) name of instance
      integer   InstanceNo             ! (INPUT) instance number to allocate
 
!+  Purpose
!      Module instantiation routine.
 
!- Implementation Section ----------------------------------
               
      allocate (Instances(InstanceNo)%gptr)
      allocate (Instances(InstanceNo)%pptr)
c     allocate (Instances(InstanceNo)%cptr)
      Instances(InstanceNo)%Name = InstanceName
 
      return
      end

!     ===========================================================
      subroutine FreeInstance (anInstanceNo)
!     ===========================================================
      use FertilizModule
      implicit none
 
!+  Sub-Program Arguments
      integer anInstanceNo             ! (INPUT) instance number to allocate
 
!+  Purpose
!      Module de-instantiation routine.
 
!- Implementation Section ----------------------------------
               
      deallocate (Instances(anInstanceNo)%gptr)
      deallocate (Instances(anInstanceNo)%pptr)
c     deallocate (Instances(anInstanceNo)%cptr)
 
      return
      end
     
!     ===========================================================
      subroutine SwapInstance (anInstanceNo)
!     ===========================================================
      use FertilizModule
      implicit none
 
!+  Sub-Program Arguments
      integer anInstanceNo             ! (INPUT) instance number to allocate
 
!+  Purpose
!      Swap an instance into the global 'g' pointer
 
!- Implementation Section ----------------------------------
               
      g => Instances(anInstanceNo)%gptr
      p => Instances(anInstanceNo)%pptr
c     c => Instances(anInstanceNo)%cptr
 
      return
      end


*     ===========================================================
      subroutine Main (Action, Data_string)
*     ===========================================================
      use FertilizModule
      implicit none

      include   'action.inc'

      include   'const.inc'            ! Global constant definitions
      include   'event.inc'
c     include   'fertiliz.inc'         ! fertiliz common block
      include 'string.pub'
      include 'error.pub'
 
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
  
      else if (Action.eq.ACTION_Init) then
 
         call fertiliz_zero_variables ()
         call fertiliz_Init ()
 
      else if (Action.eq.EVENT_tick) then
         call fertiliz_ONtick()
 
      else if (Action.eq.ACTION_Process) then
 
         call fertiliz_get_other_variables ()
         call fertiliz_schedule ()
 
      else if (Action.eq.'apply' .or. Action.eq.'fertilize') then
         call fertiliz_get_other_variables ()
         call fertiliz_fertilize ()
 
      else if (Action .eq. ACTION_Set_variable) then
         call fertiliz_get_other_variables ()
         call fertiliz_set_my_variable (Data_string)
 
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
      use FertilizModule
      implicit none
      include   'const.inc'            ! Global constant definitions
c     include   'fertiliz.inc'         ! fertiliz common block
      include 'intrface.pub'
      include 'error.pub'
 
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
      end
 
 
 
*     ===========================================================
      subroutine fertiliz_Init ()
*     ===========================================================
      use FertilizModule
      implicit none
c     include   'fertiliz.inc'         ! fertiliz model common
      include 'error.pub'
 
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
      end
 
 
 
*     ===========================================================
      subroutine fertiliz_zero_variables ()
*     ===========================================================
      use FertilizModule
      implicit none
c     include    'fertiliz.inc'        ! fertiliz common block
      include 'data.pub'
      include 'error.pub'
 
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
      end
 
 
 
*     ===========================================================
      subroutine fertiliz_get_other_variables ()
*     ===========================================================
      use FertilizModule
      implicit none
      include   'const.inc'            ! Constant definitions
c     include   'fertiliz.inc'         ! fertiliz common block
      include 'intrface.pub'
      include 'error.pub'
 
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
 
      call Get_real_array (
     :      unknown_module  ! Module that responds (Not Used)
     :    , 'dlayer'        ! Variable Name
     :    , max_layer       ! Array size_of
     :    , '(mm)'          ! Units                (Not Used)
     :    , g%dlayer        ! Variable
     :    , numvals         ! Number of values returned
     :    , 0.0             ! Lower Limit for bound checking
     :    , 1000.0)         ! Upper Limit for bound checking
 
      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      subroutine fertiliz_Send_my_variable (Variable_name)
*     ===========================================================
      use FertilizModule
      implicit none
c     include   'fertiliz.inc'         ! fertiliz Common block
      include 'intrface.pub'
      include 'error.pub'
 
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
      end
 
 
 
*     ===========================================================
      subroutine fertiliz_apply (amount, depth, type)
*     ===========================================================
      use FertilizModule
      implicit none
      include   'ACTION.inc'
      include   'const.inc'
c     include   'fertiliz.inc'
      include 'string.pub'
      include 'data.pub'
      include 'intrface.pub'
      include 'read.pub'
      include 'error.pub'
      include 'Postbox.pub'
 
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
      character  owner_module*32       ! module that owns 'array'
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
               call Action_send (Owner_module
     :                                     ,ACTION_set_variable
     :                                     ,dlt_name)
               call delete_postbox()
 
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
      end
 
 
 
*     ===========================================================
      subroutine fertiliz_schedule ()
*     ===========================================================
      use FertilizModule
      implicit none
c     include   'fertiliz.inc'
      include 'data.pub'
      include 'error.pub'
 
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
      end
 
 
 
*     ===========================================================
      subroutine fertiliz_read_param ()
*     ===========================================================
      use FertilizModule
      implicit none
      include 'const.inc'
c     include 'fertiliz.inc'
      include 'data.pub'
      include 'read.pub'
      include 'error.pub'
 
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
      end
 
 
 
*     ===========================================================
      subroutine fertiliz_ONtick ()
*     ===========================================================
      use FertilizModule
      implicit none
c     include   'fertiliz.inc'
      include 'error.pub'
      include 'event.pub'
 
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
 
*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'fertiliz_ONtick')
 
*- Implementation Section ----------------------------------
      call push_routine (myname)

      ! Note that time and timestep information is not required
      ! and so dummy variables are used in their place.

      call handler_ONtick(g%day, g%year, temp1, temp2)

      g%fert_applied = 0.0

      call pop_routine (myname)
      return
      end
 
 
 
* ====================================================================
      subroutine fertiliz_set_my_variable (variable_name)
* ====================================================================
      use FertilizModule
      implicit none
c     include   'fertiliz.inc'
      include 'intrface.pub'
      include 'error.pub'
 
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
      end
 
 
 
