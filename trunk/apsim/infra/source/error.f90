module ErrorModule

contains

      subroutine warning_Error (dummy,msg)
      integer dummy
      character*(*) msg
      call error(msg,.false.)
      return
      end subroutine
      subroutine fatal_Error (dummy,msg)
      integer dummy
      character*(*) msg
      call error(msg,.true.)
      return
      end subroutine

!     ===========================================================
      subroutine pop_routine (name)
!     ===========================================================
      implicit none

!+ Sub-Program Arguments
      character  name*(*)              ! (INPUT) name to pop off stack

!+ Purpose
!       pop name off stack.  this is used in conjunction with push_routine
!       to maintain a trace stack for procedure calls

!+  Definition
!     There is a subprogram call stack which is used to show the
!     current nesting of called subprograms in error messages
!     when an error is flagged.  Subprograms called should use
!     push_routine() early in its executable code to put the its
!     name onto the subprogram call stack.  POP_ROUTINE() should
!     be called just before the end of the subprogram in order
!     to remove its name from the subprogram call stack.  "name"
!     is passed  so that this routine may perform checking to
!     ensure that routine names are pushed and popped in
!     strictly ordered pairs.

!+  Mission Statement
!

!+ Changes
!       250892  specified and programmed (jng hargreaves)
!       170294 jngh chanaged list directed writes to formatted.
!   DPH 19/10/94 Removed routine name argument from call to fatal_error
!     jngh 21/2/95 changed write to string to replacement statement.
!       260698 jngh added call to log profiling elapsed time
!       14/7/99 dph changed to call APSIMSYSTEM_CALLSTACK_PUSH in
!               new infrastructure.

!+ Calls
      dll_import apsimsystem_callstack_pop

!+ Local Variables


!- Implementation Section ----------------------------------

      call APSIMSYSTEM_CALLSTACK_POP (name)

      return
      end subroutine



!     ===========================================================
      subroutine push_routine (name)
!     ===========================================================
      implicit none

!+ Sub-Program Arguments
      character  name*(*)              ! (INPUT) name to push onto stack

!+ Purpose
!     Push name onto stack.  This is used in conjunction with pop_routine
!     to maintain a trace stack for procedure calls.

!+  Definition
!     There is a subprogram call stack which is used to show the
!     current nesting of called subprograms in error messages
!     when an error is flagged.  Subprograms called should use
!     PUSH_ROUTINE() early in its executable code to put the
!     name of the subprogram, "name" onto the subprogram call
!     stack.  pop_routine() should be called just before the end
!     of the subprogram in order to remove its name from the
!     subprogram call stack.

!+  Mission Statement
!

!+ Changes
!       250892  specified and programmed (jng hargreaves)
!       170294 jngh chanaged list directed writes to formatted.
!       060894 jngh used assign_string to trap truncations
!              removed old code
!   DPH 19/10/94 Removed routine name argument from call to fatal_error
!       260698 jngh added call to log profiling start time
!       14/7/99 dph changed to call APSIMSYSTEM_CALLSTACK_PUSH in
!               new infrastructure.

!+ Calls
      dll_import apsimsystem_callstack_push

!+ Local Variables

!- Implementation Section ----------------------------------

      call APSIMSYSTEM_CALLSTACK_PUSH (name)

      return
      end subroutine

! ====================================================================
       subroutine error(Error_string, isFatal)
! ====================================================================
      use ConstantsModule             ! Global common block
      use ComponentInterfaceModule
      implicit none

!+ Sub-Program Arguments
       character Error_string*(*)      ! (INPUT) err string to display.
       logical isFatal

!+ Purpose
!      Display an err message, clear message queue, and put an err
!      message on message queue.

!+  Mission Statement
!     Report a fatal error

!+ Changes
!      DPH 28/5/92
!     jngh 5/8/94 added call to set fatal err flag on
!     DPH 19/10/94 removed routine_name from arguments and call
!                  get_current_module s/r and use traceback_routine s/r
!     DPH 7/11/94 Changed Global, Global_all specifiers to new
!                 All_modules, All_active_modules specifiers.
!     DPH 21/02/95 Removed call to Message_send
!     jngh 21/2/95 moved set_fatal_on to end
!     dph 14/7/99 changed to call APSIMSYSTEM_ERROR_FATAL in new infrastructure

!+ Calls
       dll_import apsimsystem_error_fatal
       dll_import ei_getname

!+ Constant Values

!+ Local Variables
       character moduleName*50      ! current module name
       character Message*1000       ! Customised string.

!- Implementation Section ----------------------------------

      ! get the current module name
      call EI_GETNAME(EventInterface, ModuleName)

      Message = Trim(Message) // New_line // New_line // Error_string

      if (isFatal) then
         call APSIMSYSTEM_ERROR_FATAL(Message, ModuleName)
      else
         call APSIMSYSTEM_ERROR_WARNING(Message, moduleName)
      endif
      return
      end subroutine

!     ===========================================================
      subroutine assert(isok, whatchkd)
!     ===========================================================
      use ConstantsModule
      implicit none

!+  Sub-Program Arguments
      character  whatchkd*(*)     ! (IN) What test did pass or fail.
      logical isok         ! (IN) Did the test pass ?

!+  Mission statement
!     Give error message and abort if not %1

!+  Purpose
!     Gives error message on failure of test 'IsOK'.

!+  Changes
!     030998 sb created

!+  Calls
      dll_import push_routine
      dll_import pop_routine
      dll_import fatal_error

!+  Constant Values
      character  my_name*(*)
      parameter (my_name='assert')

!- Implementation Section ----------------------------------
      call push_routine(my_name)

      if (.not. isok) then
         call error('ASSERT FAIL: '//whatchkd, .true.)
      end if

      call pop_routine(my_name)
      return
      end subroutine

end module errorModule

