      module sorghummodule
      use croplibrary
      use cropmoddata

      contains

      include 'cropmodcomms.for'
      include 'sorgmain.for'
      include 'sorgn.for'
      include 'sorgopt.for'
      include 'sorgtree.for'

      end module


!     ===========================================================
      subroutine alloc_dealloc_instance(doAllocate)
!     ===========================================================
      use sorghummodule
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
         allocate(c)
      else
         deallocate(g)
         deallocate(p)
         deallocate(c)
      end if
      return
      end subroutine


      include 'CropModMain.for'
      