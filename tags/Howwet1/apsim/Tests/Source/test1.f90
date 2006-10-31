module TestModule
   use ComponentInterfaceModule
   !use DataTypesModule          
                          
   interface
      subroutine test(condition, fileName, lineNumber)
         ml_external test
         logical, intent(in)           :: condition
         character (len=*), intent(in) :: fileName
         integer                       :: lineNumber
      end subroutine test
   end interface
                          
   common /InstancePointers/ ID,g,p,c
   save InstancePointers
   !type (IDsType), pointer :: ID
   
end module TestModule

!===========================================================
! Module instantiation routine.
!===========================================================
subroutine alloc_dealloc_instance(doAllocate)
   use TestModule
   implicit none
   ml_external alloc_dealloc_instance

   logical, intent(in) :: doAllocate
end subroutine alloc_dealloc_instance

!===========================================================
! Simulation is about to commence.
!===========================================================
subroutine do_commence()
   use TestModule
   implicit none
   ml_external do_commence

end subroutine do_commence

!===========================================================
! Simulation is about to terminate.
!===========================================================
subroutine notify_termination()
   use TestModule
   implicit none
   ml_external notify_termination

end subroutine notify_termination

!===========================================================
! Perform all registrations
!===========================================================
subroutine do_init1(sdml)
   use TestModule
   implicit none
   ml_external do_init1

   character (len=*), intent(in) :: sdml 
   logical found             
   real    mwcon(100)
   real    cn_red
   integer numvals
   
   found = read_parameter ('parameters', 'mwcon', mwcon, numvals  &
                           , 0.0, 1000.0, .true.)
   call test(found, "test1.f90", 66);

end subroutine do_init1

!===========================================================
! Do second phase initialisation stuff.
!===========================================================
subroutine do_init2()
   use TestModule
   implicit none
   ml_external do_init2

   logical found             
   integer value
   found = read_parameter ('parameters', 'xxxx', value  &
                           , 0, 1000)
   call test((found .eq. .false.), "test1.f90", 83);

end subroutine do_init2

!===========================================================
! Event handler for all events coming into module.
!===========================================================
subroutine respondToEvent(fromID, eventID, variant)
   use TestModule
   implicit none
   ml_external respondToEvent

   integer, intent(in) :: fromID
   integer, intent(in) :: eventID
   integer, intent(in) :: variant

end subroutine respondToEvent

!===========================================================
! Method handler for all method calls coming into module.
!===========================================================
subroutine respondToMethod(fromID, variant)
   use TestModule
   implicit none
   ml_external respondToMethod

   integer, intent(in) :: fromID
   integer, intent(in) :: variant

end subroutine respondToMethod

!===========================================================
!  Return the value of a variable
!===========================================================
subroutine respondToGet(fromID, variable_info)
   use TestModule
   use DateModule
   implicit none
   ml_external respondToGet

   integer, intent(in)         :: fromID
   type(QueryData), intent(in) :: variable_info

end subroutine respondToGet

!===========================================================
!  Set one of our variables altered by some other module
!===========================================================
function respondToSet(fromID, VariableID, variant)
   use TestModule
   implicit none
   ml_external respondToSet

   integer, intent(in)     :: fromID
   integer, intent(in)     :: VariableID
   integer, intent(in out) :: variant
   logical                 :: respondToSet

   respondToSet = .true.
end function respondToSet
