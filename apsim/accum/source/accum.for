       
!     ===========================================================
      subroutine AllocInstance (InstanceName, InstanceNo)
!     ===========================================================
      use AccumModule
      Use infrastructure
      implicit none
 
!+  Sub-Program Arguments
      character InstanceName*(*)       ! (INPUT) name of instance
      integer   InstanceNo             ! (INPUT) instance number to allocate
 
!+  Purpose
!      Module instantiation routine.
 
!- Implementation Section ----------------------------------
               
      allocate (Instances(InstanceNo)%gptr)
      Instances(InstanceNo)%Name = InstanceName
 
      return
      end

!     ===========================================================
      subroutine FreeInstance (anInstanceNo)
!     ===========================================================
      use AccumModule
      Use infrastructure
      implicit none
 
!+  Sub-Program Arguments
      integer anInstanceNo             ! (INPUT) instance number to allocate
 
!+  Purpose
!      Module de-instantiation routine.
 
!- Implementation Section ----------------------------------
               
      deallocate (Instances(anInstanceNo)%gptr)
 
      return
      end
     
!     ===========================================================
      subroutine SwapInstance (anInstanceNo)
!     ===========================================================
      use AccumModule
      Use infrastructure
      implicit none
 
!+  Sub-Program Arguments
      integer anInstanceNo             ! (INPUT) instance number to allocate
 
!+  Purpose
!      Swap an instance into the global 'g' pointer
 
!- Implementation Section ----------------------------------
               
      g => Instances(anInstanceNo)%gptr
 
      return
      end
       
* ====================================================================
       subroutine Main (Action, Data)
* ====================================================================
      use AccumModule
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
       character Action*(*)            ! Message action to perform
       character Data*(*)              ! Message data
*
*+  Purpose
*      This routine is the interface between the main system and the
*      Accum module.

*+  Changes
*     DPH 26/10/95  Added call to message_unused
*     jngh 09/06/96 added version to presence report
*     dph 7/5/99 removed presence if test. c186

*- Implementation Section ----------------------------------
 
      if (Action.eq.ACTION_Init) then
         call Accum_zero_variables ()
         call Accum_Init ()
 
      else if (Action .eq. ACTION_Post) then
         call Accum_get_other_variables()
 
      else if (Action.eq.ACTION_Get_variable) then
         ! respond to request for one of our variable values
 
         call Accum_send_my_variable (Data)
 
      else
         ! Don't use message
 
         call Message_unused ()
      endif
 
      return
      end



* ====================================================================
       subroutine Accum_Init ()
* ====================================================================
      use AccumModule
      Use infrastructure
      implicit none

*+  Purpose
*      Initialise Accum module

*+  Changes
*     ????
*     jngh removed a_ from version function
*     dph 7/5/99 removed version info. c186

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



* ====================================================================
       subroutine Accum_read_param ()
* ====================================================================
      use AccumModule
      Use infrastructure
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

*- Implementation Section ----------------------------------
 
      call push_routine(This_routine)
 
      call read_char_array(section_name, 'accum_variables',
     .   Max_variables, ' ', g%variable_names, g%num_variables)
 
      ! loop through each variable and pull off size component.
 
      do 10 indx = 1, g%num_variables
         Pos = index(g%variable_names(indx), '[')
         if (Pos .eq. 0) then
            call Fatal_error(Err_internal, Err2)
 
         else
            ! Extract size component.
 
            Size_string = g%variable_names(indx)(Pos + 1:)
            g%variable_names(indx)(Pos:) = Blank
            Pos = index(Size_string, ']')
            if (Pos .eq. 0) then
               call Fatal_error(Err_internal, Err2)
 
            else
               Size_string(Pos:) = Blank
               call String_to_integer_var(Size_string,
     .              g%variable_sizes(indx), Numvals)
 
               if (g%variable_sizes(indx) .gt. 0 .and.
     .             g%variable_sizes(g%num_variables) .le. Max_days) then
                  goto 10
 
               else
                  call Fatal_error(Err_internal, Err3)
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
      Use infrastructure
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
      Use infrastructure
      implicit none

*+  Purpose
*      Get the values of variables from other modules

*+  Changes
*     <insert here>

*+  Local Variables
      integer Day_index                ! index into day part of variable_sizes array
      integer Var_index                ! Index into variable arrays
      integer Numvals                  ! Number of values returned

*- Implementation Section ----------------------------------
 
      ! Move all other variables down one position in the array.
 
      do 10 Var_index = 1, g%num_variables
         do 10 Day_index =  g%variable_sizes(Var_index), 2, -1
            g%variable_values(Var_index, Day_index) =
     .         g%variable_values(Var_index, Day_index - 1)
10    continue
 
      ! Get all required variables for today
 
      do 20 Var_index = 1, g%num_variables
         call Get_real_var(Unknown_module, g%variable_names(Var_index),
     .        ' ', g%variable_values(Var_index, 1), Numvals, -10000.0,
     .        10000.0)
20    continue
 
      return
      end



* ====================================================================
       subroutine Accum_Send_my_variable (Variable_name)
* ====================================================================
      use AccumModule
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
       character Variable_name*(*)     ! (INPUT) Variable name to search for

*+  Purpose
*      Return the value of one of our variables to caller

*+  Changes
*     DPH 26/10/95 Added call to message_unused
*     DPH 30/5/96  Moved the first endif past the Find_string_in_array call
*                  so that we only look for our variables that have a '[' char.

*+  Constant Values
      character Routine_name*(*)       ! Name of this routine
      parameter (Routine_name='accum_send_my_variable')

*+  Local Variables
      integer Day_index                ! Index into day part of g%variable_values array
      logical Found                    ! Have we found the variable yet ?
      integer Num_days                 ! Number of days to accumulate.
      integer Numvals                  ! number of values
      integer Pos                      ! Position in string
      character Size_string*100        ! String version of size
      real Sum                         ! Accumulated value
      integer Var_index                ! Index into variable arrays
      character Var_name*50            ! Variable name requested.

*- Implementation Section ----------------------------------
 
      call push_routine(Routine_name)
 
      Pos = index(Variable_name, '[')
 
      if (Pos .eq. 0) then
         ! None found
 
         Found = .false.
 
      else
         Var_name = Variable_name(1:Pos-1)
         Size_string = Variable_name(Pos + 1:)
         Pos = index(Size_string, ']')
         if (Pos .gt. 0) then
            Size_string(Pos:) = Blank
            call String_to_integer_var(Size_string, Num_days, Numvals)
         else
            Numvals = 0
         endif
 
         ! Determine weather the requested variable is one of ours.
 
         Var_index = Find_string_in_array
     .      (Var_name, g%variable_names, g%num_variables)
         Found = (Var_index .gt. 0)
      endif
 
      ! If variable is ours then accumulate the variable for the
      ! specified number of days and return the value to the caller.
 
      if (Found) then
 
         if (Num_days .eq. -1) then
            Num_days = g%variable_sizes(Var_index)
 
         else
            ! Caller has specified the number of days.
         endif
 
         Sum = 0.0
         do 20 Day_index = 1, Num_days
            Sum = Sum + g%variable_values(Var_index, Day_index)
20       continue
 
         call respond2get_real_var(Variable_name, '()', Sum)
 
      else
         ! Not our variable
 
         call Message_unused ()
      endif
 
      call pop_routine(Routine_name)
      return
      end



