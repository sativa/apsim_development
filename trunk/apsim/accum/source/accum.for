      include 'accum.inc'

!     ===========================================================
      subroutine alloc_dealloc_instance(doAllocate)
!     ===========================================================
      use AccumModule
      implicit none
      ml_external alloc_dealloc_instance

!+  Sub-Program Arguments
      logical, intent(in) :: doAllocate

!+  Purpose
!      Module instantiation routine.

!- Implementation Section ----------------------------------

      if (doAllocate) then
         allocate(id)
         allocate(g)

      else
         deallocate(id)
         deallocate(g)

      end if
      return
      end

!     ===========================================================
      subroutine do_init1(sdml)
!     ===========================================================
      use AccumModule
      use DataTypesModule
      implicit none
      ml_external do_init1

!+  Purpose
!      Perform all registrations and zeroing

!+  Sub-Program Arguments
      character (len=*), intent(in) :: sdml

*+  Constant Values
       character ID_Init*(*)          ! Message indicating initialisation
       parameter (ID_Init='Initialising')

*+  Local Variables
       character Event_string*40       ! String to output

*- Implementation Section ----------------------------------

      call do_registrations(id)

      return
      end

!     ===========================================================
      subroutine do_commence()
!     ===========================================================
      implicit none
      ml_external do_commence

!+  Purpose
!      Perform all registrations and zeroing

!- Implementation Section ----------------------------------

      return
      end

* ====================================================================
      subroutine do_init2 ()
* ====================================================================
      use AccumModule
      use ComponentInterfaceModule
      implicit none
      ml_external do_init2

*+  Purpose
*     Initialise the apswim module

*+  Changes

*+  Calls

*+  Constant Values
       character ID_Init*(*)          ! Message indicating initialisation
       parameter (ID_Init='Initialising')

*+  Local Variables
       character Event_string*40       ! String to output

*- Implementation Section ----------------------------------

      ! Notify system that we have initialised

      Event_string = ID_Init
      call Write_string (Event_string)

      ! Get all parameters from parameter file

      call Accum_read_param ()

      return
      end


!     ===========================================================
      subroutine respondToEvent(fromID, eventID, variant)
!     ===========================================================
      use AccumModule
      use ComponentInterfaceModule
      use DataTypesModule
      implicit none
      ml_external respondToEvent

!+  Purpose
!      Event handler for all events coming into module.

!+  Sub-Program Arguments
      integer, intent(in) :: fromID
      integer, intent(in) :: eventID
      integer, intent(in) :: variant

!- Implementation Section ----------------------------------

      if (eventID .eq. id%post) then
         call Accum_get_other_variables()

      else
         call error('bad event ID',.true.)
      endif
      return
      end
!     ===========================================================
      subroutine respondToMethod(fromID, methodID, variant)
!     ===========================================================
      use AccumModule
      use ComponentInterfaceModule
      implicit none
      ml_external respondToMethod

!+  Purpose
!      Method handler for all method calls coming into module.

!+  Sub-Program Arguments
      integer, intent(in) :: fromID
      integer, intent(in) :: methodID
      integer, intent(in) :: variant

!- Implementation Section ----------------------------------

      call error('bad method ID',.true.)

      return
      end

!     ===========================================================
      subroutine notify_termination()
!     ===========================================================
      use AccumModule
      implicit none
      ml_external notify_termination

!+  Purpose
!      Prepare for termination

!- Implementation Section ----------------------------------

      return
      end

* ====================================================================
       subroutine respondToGet (fromID, Variable_info)
* ====================================================================
      use AccumModule
      use ComponentInterfaceModule
      use DataTypesModule
      implicit none
      ml_external respondToGet

*+  Sub-Program Arguments
      integer, intent(in) :: fromID
      type(QueryData), intent(in) :: variable_info

*+  Purpose
*      Return the value of one of our variables to caller

*+  Changes

*+  Calls

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'respondToGet')

*+  Local Variables
       integer variable
       integer day
       real sum

*- Implementation Section ----------------------------------

      call push_routine (myname)

      do variable = 1, g%num_variables
         ! If variable is ours then accumulate the variable for the
         ! specified number of days and return the value to the caller.
         if (Variable_info%id .eq.
     .       g%Variable_respondtogetids(variable)) then
            Sum = 0.0
            do day = 1, g%Variable_sizes(variable)
               Sum = Sum + g%variable_values(variable, day)
            end do

            call return_single(Variable_info, Sum)
         endif
      end do

      call pop_routine(myname)

      return
      end
* ====================================================================
      logical function respondToSet (fromID, VariableID, variant)
* ====================================================================
      use AccumModule
      use ComponentInterfaceModule

      implicit none
      ml_external respondToSet

!+  Sub-Program Arguments
      integer, intent(in) :: fromID
      integer, intent(in)     :: VariableID
      integer, intent(in out) :: variant


*+  Purpose
*       Set one of our variables altered by some other module.

*+  Changes
*      21-06-96 NIH Changed respond2set calls to collect calls

*+  Local Variables

*- Implementation Section ----------------------------------

      respondToSet = .false.
      return
      end

* ====================================================================
       subroutine Accum_read_param ()
* ====================================================================
      use AccumModule
      use ComponentInterfaceModule
      use DataTypesModule
      implicit none

*+  Purpose
*      Read in all parameters from parameter file.

*+  Changes
*     210395 jngh changed from unknown_section to a defined section
*     19/7/95 DPH Changed the .lt. to a .le. when checking to see if the
*                 variable_sizes is greater than Max_days

*+  Constant Values
      character This_routine*(*)       ! Name of this routine
      parameter (This_routine='Accum_read_param')
*
      character  section_name*(*)      ! name of parameter section
      parameter (section_name = 'parameters')
*
      character Err2*(*)               ! Error 2 to display
      parameter (Err2=
     .   'The number of days for accumulation was not specified'
     .   // New_line //
     .   'for a variable.  e.g. rain[5]')
*
      character Err3*(*)               ! Error 3 to display
      parameter (Err3=
     .   'The number of days to accumulate a variable is to big.'
     .   // New_line //
     .   'Maximum number of days is 20')

*+  Local Variables
      integer Indx                     ! Index into array
       integer Numvals                 ! Number of values
       integer Pos                     ! Position in string
       character Size_string*50        ! string version of size
      logical found

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      found = read_parameter(section_name, 'accum_variables',
     .   g%variable_names, g%num_variables)

      ! loop through each variable and pull off size component.

      do 10 indx = 1, g%num_variables
         Pos = index(g%variable_names(indx), '[')
         if (Pos .eq. 0) then
            call error(Err2, .true.)

         else
            ! Register the variable as a respondToGet
            g%Variable_respondtogetids(indx) = add_registration
     .            (respondToGetReg, g%variable_names(indx),
     .             singleddml)


            ! Extract size component.

            Size_string = g%variable_names(indx)(Pos + 1:)
            g%variable_names(indx)(Pos:) = Blank
            Pos = index(Size_string, ']')
            if (Pos .eq. 0) then
               call error(Err2, .true.)

            else
               Size_string(Pos:) = Blank
               call String_to_integer_var(Size_string,
     .              g%variable_sizes(indx), Numvals)

               ! Also register that we are going to get the value of the raw variable
               g%Variable_getids(indx) = add_registration
     .               (getVariableReg, g%variable_names(indx),
     .                singleddml)

               if (g%variable_sizes(indx) .gt. 0 .and.
     .             g%variable_sizes(g%num_variables) .le. Max_days) then
                  goto 10

               else
                  call error(Err3, .true.)
               endif
            endif
         endif
10    continue

      call pop_routine(This_routine)
      return
      end



* ====================================================================
       subroutine Accum_zero_variables ()
* ====================================================================
      use AccumModule
      implicit none

*+  Purpose
*     Set all variables in this module to zero.

*+  Changes
*     <insert here>

*+  Local Variables
      integer Var_index                ! variable index
      integer Day_index                ! day index

*- Implementation Section ----------------------------------

      g%num_variables = 0
      do 10 Var_index = 1, Max_variables
         do 10 Day_index = 1, Max_days
            g%variable_values(Var_index, Day_index) = 0.0
10    continue

      return
      end



* ====================================================================
       subroutine Accum_get_other_variables ()
* ====================================================================
      use AccumModule
      use ComponentInterfaceModule
      use DataTypesModule
      implicit none

*+  Purpose
*      Get the values of variables from other modules

*+  Changes
*     <insert here>

*+  Local Variables
      integer Day_index                ! index into day part of variable_sizes array
      integer Var_index                ! Index into variable arrays
      integer Numvals                  ! Number of values returned
      logical ok

*- Implementation Section ----------------------------------

      ! Move all other variables down one position in the array.

      do 10 Var_index = 1, g%num_variables
         do 10 Day_index =  g%variable_sizes(Var_index), 2, -1
            g%variable_values(Var_index, Day_index) =
     .         g%variable_values(Var_index, Day_index - 1)
10    continue

      ! Get all required variables for today

      do 20 Var_index = 1, g%num_variables
         ok = Get_single(g%variable_getids(Var_index),
     .        g%variable_values(Var_index, 1))
20    continue

      return
      end

