module ErrorModule

   interface
      subroutine push_routine(name)
      ml_external push_routine
      character(len=*), intent(in) :: name
      end subroutine
      
      subroutine pop_routine(name)
      ml_external pop_routine
      character(len=*), intent(in) :: name
      end subroutine 
   end interface   

   contains

      subroutine warning_Error (dummy,msg)
      use ComponentInterfaceModule
      integer dummy
      character*(*) msg
      call error(msg,.false.)
      return
      end subroutine
      subroutine fatal_Error (dummy,msg)
      use ComponentInterfaceModule
      integer dummy
      character*(*) msg
      call error(msg,.true.)
      return
      end subroutine

end module errorModule

