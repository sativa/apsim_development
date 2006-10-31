      module ITestModule
      use Registrations
      integer Max_variables            ! Maximum number of variables.
      parameter (Max_variables=50)

      integer Max_days                 ! maximum number of days for a single
      parameter (Max_days = 366)       ! accumulation

      type ITestGlobals
         sequence
         integer Day                      ! Day of year.
         integer Num_gets
         character getVariableNames(Max_variables)*30
         real      lower_get_bound(Max_variables)
         real      upper_get_bound(Max_variables)

         integer Num_reads
         character readVariableNames(Max_variables)*30
         real      lower_read_bound(Max_variables)
         real      upper_read_bound(Max_variables)
      end type ITestGlobals

      ! instance variables.
      common /InstancePointers/ ID,g,p,c
      save InstancePointers
      type (ITestGlobals),pointer :: g
      type (IDsType),pointer :: ID


      contains

* ====================================================================
       subroutine ITest_Init ()
* ====================================================================
      Use infrastructure
      implicit none

*+  Purpose
*      Initialise ITest module

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

      call ITest_read_param ()

      return
      end subroutine



* ====================================================================
       subroutine ITest_read_param ()
* ====================================================================
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
      parameter (This_routine='ITest_read_param')
*
      character  section_name*(*)      ! name of parameter section
      parameter (section_name = 'parameters')
*
*+  Local Variables
      integer Indx                     ! Index into array
      integer Numvals                 ! Number of values
      character bound_name*50        ! string version of size

      integer pos1, pos2
      real dummyReal, dummyRealArray(2)
      double precision dummydouble, dummydoubleArray(2)
      integer dummyinteger, dummyintegerArray(2)
      logical dummylogical, dummylogicalArray(2)      
      character Variable_name*50
      character type_name*50            ! Variable name requested.
      character shape_name*50           ! Variable name requested.
      character message*200             ! Variable name requested.

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      call read_char_array_optional(
     .   section_name, 'get_variables',
     .   Max_variables, ' ', g%getVariableNames, g%num_gets)

      ! loop through each variable and pull off size component.
      do 10 indx = 1, g%num_gets
!         bound_name = g%getVariableNames(indx) // 'lower'
         bound_name =  'lower'
         call read_real_var(section_name, bound_name, '', 
     .                      g%lower_get_bound(indx),
     .                      numvals, -1000.0, 1000.0)

!         bound_name = g%getVariableNames(indx) // 'upper'
         bound_name =  'upper'
         call read_real_var(section_name, bound_name, '', 
     .                      g%upper_get_bound(indx),
     .                      numvals, -1000.0, 1000.0)

10    continue

      call read_char_array_optional(
     .   section_name, 'read_variables',
     .   Max_variables, ' ', g%readVariableNames, g%num_reads)

      do 20 indx = 1, g%num_reads
         !bound_name = g%readVariableNames(indx) // 'lower'
         bound_name =  'lower'
         call read_real_var(section_name, bound_name, '', 
     .                      g%lower_read_bound(indx),
     .                      numvals, -1000.0, 1000.0)

         !bound_name = g%readVariableNames(indx) // 'upper'
         bound_name =  'upper'
         call read_real_var(section_name, bound_name, '', 
     .                      g%upper_read_bound(indx),
     .                      numvals, -1000.0, 1000.0)

20    continue

      do 30 indx = 1, g%num_reads
        Variable_name = g%readVariableNames(indx)
        Pos1 = index(Variable_name, '_')
        if (Pos1 .ne. 0) then
           type_name = Variable_name(1:Pos1-1)
           Pos2 = index(Variable_name(pos1+1:50), '_')
           if (Pos2 .ne. 0) then
             shape_name = Variable_name(Pos1+1:pos2-1)
           else 
             shape_name = Variable_name(Pos1+1:50)
           endif
        endif

        if (type_name .eq. 'real') then
          if (shape_name .eq. 'var') then
             call read_real_var(section_name, 
     .              Variable_name,
     .              ' ', 
     .              dummyreal, 
     .              Numvals,
     .              g%lower_read_bound(indx),
     .              g%upper_read_bound(indx))
             write (message,*) 'got ', numvals,
     .         ' real, value = ', dummyReal
             call write_string(message)
          elseif (shape_name .eq. 'array') then
             call read_real_array(section_name, 
     .              Variable_name,
     .              2,  
     .              ' ',
     .              dummyrealArray, 
     .              Numvals,
     .              g%lower_read_bound(indx),
     .              g%upper_read_bound(indx))
          else
             write (message,*)  'a.dont know about ',
     .           Variable_name        
             call write_string(message)
             write (message,*)  'shape=', shape_name
             call write_string(message)
          endif

        elseif (type_name .eq. 'double') then
          if (shape_name .eq. 'var') then
             call read_double_var(section_name, 
     .              Variable_name,
     .              ' ', 
     .              dummydouble, 
     .              Numvals,
     .              dble (g%lower_read_bound(indx)),
     .              dble (g%upper_read_bound(indx)))
             write (message,*) 'got ', numvals,
     .         ' double, value = ', dummyDouble
             call write_string(message)
          elseif (shape_name .eq. 'array') then
             call read_double_array(section_name, 
     .              Variable_name,
     .              2,  
     .              ' ',
     .              dummydoubleArray, 
     .              Numvals,
     .              dble(g%lower_read_bound(indx)),
     .              dble(g%upper_read_bound(indx)))
          else
             write (message,*)  'c dont know about ',
     .           Variable_name        
             call write_string(message)
             write (message,*)  'shape=', shape_name
             call write_string(message)
          endif

        elseif (type_name .eq. 'integer') then
          if (shape_name .eq. 'var') then
             call read_integer_var(section_name, 
     .              Variable_name,
     .              ' ', 
     .              dummyinteger, 
     .              Numvals,
     .              floor(g%lower_read_bound(indx)),
     .              floor(g%upper_read_bound(indx)))
             write (message,*) 'got ', numvals,
     .         ' integer, value = ', dummyInteger
             call write_string(message)
          elseif (shape_name .eq. 'array') then
             call read_integer_array(section_name, 
     .              Variable_name,
     .              2,  
     .              ' ',
     .              dummyintegerArray, 
     .              Numvals,
     .              floor(g%lower_read_bound(indx)),
     .              floor(g%upper_read_bound(indx)))
          else
             write (message,*)  'e dont know about ',
     .           Variable_name        
             call write_string(message)
             write (message,*)  'shape=', shape_name
             call write_string(message)
          endif

        elseif (type_name .eq. 'logical') then
          if (shape_name .eq. 'var') then
             call read_logical_var(section_name, 
     .              Variable_name,
     .              ' ', 
     .              dummyLogical, 
     .              Numvals)
             write (message,*) 'got ', numvals,
     .         ' logical, value = ', dummyLogical
             call write_string(message)
          elseif (shape_name .eq. 'array') then
             call read_logical_array(section_name, 
     .              Variable_name,
     .              2,  
     .              ' ',
     .              dummylogicalArray, 
     .              Numvals)
          else
             write (message,*)  'g dont know about ',
     .           Variable_name        
             call write_string(message)
             write (message,*)  'shape=', shape_name
             call write_string(message)
          endif

        else
          write (message,*)  'h dont know about ',
     .           Variable_name        
          call write_string(message)
          write (message,*)  'type=', type_name
          call write_string(message)
        endif        
        
        
        
        
        

        
        
        
30    continue

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine ITest_zero_variables ()
* ====================================================================
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

      g%num_gets = 0

      return
      end subroutine



* ====================================================================
       subroutine ITest_get_other_variables ()
* ====================================================================
      Use infrastructure
      implicit none

*+  Purpose
*      Get the values of variables from other modules

*+  Changes
*     <insert here>

*+  Local Variables
      integer indx                ! Index into variable arrays
      integer Numvals                  ! Number of values returned
      integer pos1, pos2, length
      real dummyReal, dummyRealArray(2)
      double precision dummyDouble, dummyDoubleArray(2)
      integer dummyInteger, dummyIntegerArray(2)
      logical dummyLogical, dummyLogicalArray(2)
      character Variable_name*50
      character type_name*50            ! Variable name requested.
      character shape_name*50            ! Variable name requested.
      character message*200             ! Variable name requested.
      
*- Implementation Section ----------------------------------

      ! Get all required variables for today

      do 20 indx = 1, g%num_gets
        Variable_name = g%getVariableNames(indx)
        length = len(Variable_name)
        Pos1 = index(Variable_name, '_')
        if (Pos1 .ne. 0) then
           type_name = Variable_name(1:Pos1-1)
           Pos2 = index(Variable_name(pos1+1:length), '_')
           if (Pos2 .ne. 0) then
             shape_name = Variable_name(Pos1+1:Pos1+pos2-1)
           else 
             shape_name = Variable_name(Pos1+1:length)
           endif
        endif
        if (type_name .eq. 'real') then
          if (shape_name .eq. 'var') then
             call Get_real_var(Unknown_module, 
     .              Variable_name,
     .              ' ', 
     .              dummyreal, 
     .              Numvals,
     .              g%lower_get_bound(indx),
     .              g%upper_get_bound(indx))
             write (message,*) 'got ', numvals,
     .         ' real, value = ', dummyReal
             call write_string(message)
          elseif (shape_name .eq. 'array') then
             call Get_real_array(Unknown_module, 
     .              Variable_name,
     .              2,  
     .              ' ',
     .              dummyrealArray, 
     .              Numvals,
     .              g%lower_get_bound(indx),
     .              g%upper_get_bound(indx))
             write (message,*) 'got ', numvals,
     .         ' real, value = ', dummyRealArray
             call write_string(message)
          else
             write (message,*)  'i dont know about get variable ',
     .           Variable_name        
             call write_string(message)
             write (message,*)  'type=', type_name
             call write_string(message)
             write (message,*)  'shape=', shape_name
             call write_string(message)
          endif
        elseif (type_name .eq. 'integer') then          
          if (shape_name .eq. 'var') then
             call Get_integer_var(Unknown_module, 
     .              Variable_name,
     .              ' ', 
     .              dummyinteger, 
     .              Numvals,
     .              floor(g%lower_get_bound(indx)),
     .              floor(g%upper_get_bound(indx)))
             write (message,*) 'got ', numvals,
     .         ' integer, value = ', dummyInteger
             call write_string(message)
          elseif (shape_name .eq. 'array') then
             call Get_integer_array(Unknown_module, 
     .              Variable_name,
     .              2,  
     .              ' ',
     .              dummyintegerArray, 
     .              Numvals,
     .              floor(g%lower_get_bound(indx)),
     .              floor(g%upper_get_bound(indx)))
             write (message,*) 'got ', numvals,
     .         ' integer, value = ', dummyIntegerArray
             call write_string(message)
          else
             write (message,*)  'i dont know about get variable ',
     .           Variable_name        
             call write_string(message)
             write (message,*)  'type=', type_name
             call write_string(message)
             write (message,*)  'shape=', shape_name
             call write_string(message)
          endif
          
        elseif (type_name .eq. 'double') then          
          if (shape_name .eq. 'var') then
             call Get_double_var(Unknown_module, 
     .              Variable_name,
     .              ' ', 
     .              dummydouble, 
     .              Numvals,
     .              dble(g%lower_get_bound(indx)),
     .              dble(g%upper_get_bound(indx)))
             write (message,*) 'got ', numvals,
     .         ' double, value = ', dummydouble
             call write_string(message)
          elseif (shape_name .eq. 'array') then
             call Get_double_array(Unknown_module, 
     .              Variable_name,
     .              2,  
     .              ' ',
     .              dummydoubleArray, 
     .              Numvals,
     .              dble(g%lower_get_bound(indx)),
     .              dble(g%upper_get_bound(indx)))
             write (message,*) 'got ', numvals,
     .         ' double, value = ', dummydoubleArray
             call write_string(message)
          else
             write (message,*)  'i dont know about get variable ',
     .           Variable_name        
             call write_string(message)
             write (message,*)  'type=', type_name
             call write_string(message)
             write (message,*)  'shape=', shape_name
             call write_string(message)
          endif
          
        elseif (type_name .eq. 'logical') then          
          if (shape_name .eq. 'var') then
             call Get_logical_var(Unknown_module, 
     .              Variable_name,
     .              ' ', 
     .              dummylogical, 
     .              Numvals)
             write (message,*) 'got ', numvals,
     .         ' logical, value = ', dummylogical
             call write_string(message)
          elseif (shape_name .eq. 'array') then
             call Get_logical_array(Unknown_module, 
     .              Variable_name,
     .              2,  
     .              ' ',
     .              dummylogicalArray, 
     .              Numvals)
             write (message,*) 'got ', numvals,
     .         ' logical, value = ', dummylogicalArray
             call write_string(message)
          else
             write (message,*)  'i dont know about get variable ',
     .           Variable_name        
             call write_string(message)
             write (message,*)  'type=', type_name
             call write_string(message)
             write (message,*)  'shape=', shape_name
             call write_string(message)
          endif          
          
          
        else
             write (message,*)  'i dont know about get variable ',
     .           Variable_name        
             call write_string(message)
             write (message,*)  'type=', type_name
             call write_string(message)
         endif
20    continue

      return
      end subroutine



* ====================================================================
       subroutine ITest_Send_my_variable (Variable_name)
* ====================================================================
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
      parameter (Routine_name='ITest_send_my_variable')

*+  Local Variables
      integer Day_index                ! Index into day part of g%variable_values array
      logical Found                    ! Have we found the variable yet ?
      integer length
      integer Numvals                  ! number of values
      integer Pos1, Pos2, pos3               ! Position in string
      character message*100        ! String version of size
      real Sum                         ! ITestulated value
      integer Var_index                ! Index into variable arrays
      character type_name*50            ! Variable name requested.
      character shape_name*50            ! Variable name requested.
      character add_name*50            ! Variable name requested.
      integer intArray(2)
      real realArray(2)
      double precision doubleArray(2)
      integer integerArray(2)
      logical logicalArray(2) 
      data realArray/42.0, 42.0/
      data intArray/42, 42/
      data doubleArray/42.0, 42.0/
      data logicalArray/.true., .true./
      
*- Implementation Section ----------------------------------
      call push_routine(Routine_name)
      length = len(Variable_name)
      
      Pos1 = index(Variable_name, '_')
      if (Pos1 .ne. 0) then
         type_name = Variable_name(1:Pos1-1)
         Pos2 = index(Variable_name(pos1+1:length), '_')
         if (Pos2 .ne. 0) then
           shape_name = Variable_name(Pos1+1:Pos1+pos2-1)
           add_name = Variable_name(Pos1+pos2+1:length)
         else
           add_name = ' '
           shape_name = Variable_name(Pos1+1:length)
         endif
      endif

      if (type_name .eq. 'real') then
         if (shape_name .eq. 'var' .and.
     .        add_name .ne. 'missing') then
            call respond2get_real_var(Variable_name, '()', 42.0)
         elseif (shape_name .eq. 'array'.and.
     .            add_name .ne. 'missing') then
            call respond2get_real_array(Variable_name, '()', 
     .                                   realArray, 2)
         else
            call Message_unused ()
         endif
      else
         call Message_unused ()
      endif    
      
      if (type_name .eq. 'integer') then
         if (shape_name .eq. 'var' .and.
     .        add_name .ne. 'missing') then
            call respond2get_integer_var(Variable_name, '()', 42)
         elseif (shape_name .eq. 'array'.and.
     .            add_name .ne. 'missing') then
            call respond2get_integer_array(Variable_name, '()', 
     .                                   integerArray, 2)
         else
            call Message_unused ()
         endif
      else
         call Message_unused ()
      endif 
      
      if (type_name .eq. 'double') then
         if (shape_name .eq. 'var' .and.
     .        add_name .ne. 'missing') then
            call respond2get_double_var(Variable_name, '()', 
     .      doubleArray(1))
         elseif (shape_name .eq. 'array'.and.
     .            add_name .ne. 'missing') then
            call respond2get_double_array(Variable_name, '()', 
     .                                   doubleArray, 2)
         else
            call Message_unused ()
         endif
      else
         call Message_unused ()
      endif       
      
      
            if (type_name .eq. 'logical') then
         if (shape_name .eq. 'var' .and.
     .        add_name .ne. 'missing') then
            call respond2get_logical_var(Variable_name, '()', 
     .      logicalArray(1))
         elseif (shape_name .eq. 'array'.and.
     .            add_name .ne. 'missing') then
            call respond2get_logical_array(Variable_name, '()', 
     .                                   logicalArray, 2)
         else
            call Message_unused ()
         endif
      else
         call Message_unused ()
      endif  
      
      call pop_routine(Routine_name)
      return
      end subroutine

      end module ITestModule


!     ===========================================================
      subroutine alloc_dealloc_instance(doAllocate)
!     ===========================================================
      use ITestModule
      implicit none
      ml_external alloc_dealloc_instance

!+  Sub-Program Arguments
      logical, intent(in) :: doAllocate

!+  Purpose
!      Module instantiation routine.

!- Implementation Section ----------------------------------

      if (doAllocate) then
         allocate(g)
         allocate(id)
      else
         deallocate(g)
         deallocate(id)
      end if
      return
      end subroutine



* ====================================================================
       subroutine Main (Action, Data)
* ====================================================================
      Use infrastructure
      Use ITestModule
      implicit none
      ml_external Main

*+  Sub-Program Arguments
       character Action*(*)            ! Message action to perform
       character Data*(*)              ! Message data
*
*+  Purpose
*      This routine is the interface between the main system and the
*      ITest module.

*+  Changes
*     DPH 26/10/95  Added call to message_unused
*     jngh 09/06/96 added version to presence report
*     dph 7/5/99 removed presence if test. c186

*- Implementation Section ----------------------------------

      if (Action.eq.ACTION_Init) then
         call ITest_zero_variables ()
         call ITest_Init ()

      else if (Action.eq.ACTION_Create) then
         call doRegistrations(id)

      else if (Action .eq. ACTION_Process) then
         call ITest_get_other_variables()

      else if (Action.eq.ACTION_Get_variable) then
         ! respond to request for one of our variable values

         call ITest_send_my_variable (Data)

      else
         ! Don't use message

         call Message_unused ()
      endif

      return
      end subroutine
      
! ====================================================================
! This routine is the event handler for all events
! ====================================================================
      subroutine respondToEvent(fromID, eventID, variant)
      Use infrastructure
      implicit none
      ml_external respondToEvent
      
      integer, intent(in) :: fromID
      integer, intent(in) :: eventID
      integer, intent(in) :: variant
      
      return
      end subroutine respondToEvent
