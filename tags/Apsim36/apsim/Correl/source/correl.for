      module CorrelModule
      use Registrations
!     ================================================================
!      correl constants
!     ================================================================

!     variables used in correl module

!   assumptions:
!      none

!   notes:
!      none

!   changes:
!      180396  jngh programmed

! ----------------------- declaration section ------------------------

!   constant values

      integer offset
      parameter (offset = 30)

      integer  max_numvars
      parameter (max_numvars = 20)
!     ================================================================

      type CorrelGlobals
         sequence
         character   name_found(max_numvars)*8
         integer     records
         real        values(40000,max_numvars)
         integer     numvars
         logical     first_time
      end type CorrelGlobals
!     ================================================================

      type CorrelParameters
         sequence
         character   names(max_numvars)*8
         character   check_name(max_numvars)*8

         integer     offset
         integer     numvars
         integer     check_vals(max_numvars,max_numvars)
         integer     num_check

      end type CorrelParameters
!     ================================================================


      type CorrelConstants
         sequence
         logical dummyConstant
      end type CorrelConstants
!     ================================================================

      ! instance variables.
      common /InstancePointers/ ID,g,p,c
      save InstancePointers
      type (CorrelGlobals),pointer :: g
      type (CorrelParameters),pointer :: p
      type (CorrelConstants),pointer :: c
      type (IDsType),pointer :: id

      contains



*====================================================================
      subroutine correl_zero_variables ()
*====================================================================
      Use infrastructure
      implicit none

*+  Purpose
*     Set all variables in this module to zero.

*+  Changes
*       210995 jngh programmed

*+  Constant Values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'correl_zero_variables')

*- Implementation Section ----------------------------------
      call push_routine (myname)

         !variables

      g%records = 0

      call correl_zero_daily_variables ()

      call pop_routine (myname)

      return
      end subroutine



*====================================================================
      subroutine correl_zero_daily_variables ()
*====================================================================
      Use infrastructure
      implicit none

*+  Purpose
*     Set all variables in this module to zero.

*+  Changes
*       210995 jngh programmed

*+  Constant Values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'correl_zero_daily_variables')

*- Implementation Section ----------------------------------
      call push_routine (myname)


      call pop_routine (myname)

      return
      end subroutine



*====================================================================
      subroutine correl_init ()
*====================================================================
      Use infrastructure
      implicit none

*+  Purpose
*      Initialise correl module

*+  Changes
*       210995 jngh programmed
*       190599 jngh removed version references

*+  Constant Values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'correl_init')

*- Implementation Section ----------------------------------
      call push_routine (myname)

         ! notify system that we have initialised

      call Write_string ('Initialising:')

         ! get all parameters from parameter file

      call correl_read_param ()

         ! get all constants from constants file

      call correl_read_constants ()

      call pop_routine (myname)

      return
      end subroutine



*===========================================================
      subroutine correl_read_param ()
*===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*       Read all module parameters.

*+  Changes
*       210995 jngh programmed
*       020398 jngh added in reading and reporting for reference height,
*                   disp and Z0 at instrument site.
*                   changed z0soil to mm

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'correl_read_param')
*
      character section_name*(*)
      parameter (section_name = 'parameters')

*+  Local Variables
      integer    numvals               ! number of values read
c      character  line*80               ! output string
      integer    temp(max_numvars)
      integer    numints
      character  check(max_numvars)*8
      integer    numcheck
      integer    i
      integer    k


*- Implementation Section ----------------------------------

      call push_routine (myname)

      call write_string (
     :          new_line//'   - Reading correl Parameters')

         ! offset days
      call read_integer_var (
     :           section_name
     :          ,'offset'
     :          ,'(-)'
     :          ,p%offset
     :          ,numvals
     :          ,0
     :          ,offset)

         ! met parameter names
      call read_char_array (
     :           section_name
     :          ,'names'
     :          ,max_numvars
     :          ,'(-)'
     :          ,p%names
     :          ,p%numvars)

      call read_char_array (
     :           section_name
     :          ,'check'
     :          ,max_numvars
     :          ,'(-)'
     :          ,check
     :          ,numcheck)

      k = 0
      do i = 1, numcheck
         call read_integer_array (
     :           section_name
     :          ,check(i)
     :          ,max_numvars
     :          ,'(-)'
     :          ,temp
     :          ,numints
     :          ,-1
     :          ,1)
         if (numints.eq.0) then
         else
            k = k + 1
            p%check_name(k) = check(i)
            p%check_vals(:,k) = temp
         endif
      end do
      p%num_check = k

      call pop_routine  (myname)
      return
      end subroutine



*===========================================================
      subroutine correl_read_constants ()
*===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*       Read all module constants.

*+  Changes
*       210995 jngh programmed

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'correl_read_constants')
*
      character section_name*(*)
      parameter (section_name = 'constants')

*+  Local Variables
      integer    numvals               ! number of values read

*- Implementation Section ----------------------------------

      call push_routine (myname)

      call pop_routine  (myname)
      return
      end subroutine



*================================================================
      subroutine correl_prepare ()
*================================================================
      Use infrastructure
      implicit none

*+  Purpose
*     perform calculations before the current timestep.

*+  Changes
*       210995 jngh programmed

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'correl_prepare')

*- Implementation Section ----------------------------------

      call push_routine (myname)

      call pop_routine (myname)
      return
      end subroutine



*====================================================================
      subroutine correl_get_other_variables_init ()
*====================================================================
      Use infrastructure
      implicit none

*+  Purpose
*      Get the values of variables from other modules

*+  Changes
*       210995 jngh programmed

*+  Constant Values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'correl_get_other_variables_init')

*+  Local Variables
      integer    numvals               ! number of values returned
      integer    i
      integer    j
      real       value

*- Implementation Section ----------------------------------
      call push_routine (myname)

      g%numvars = 0
      do i=1, p%numvars
      if (p%names(i).ne.blank) then

      call get_real_var_optional (
     :      unknown_module
     :     ,p%names(i)
     :     ,'()'
     :     ,value
     :     ,numvals
     :     ,-1000.0
     :     ,10000.0)

         if (numvals.gt.0) then
            g%numvars = g%numvars + 1
            g%name_found(g%numvars) = p%names(i)
         else
         endif

      else
      endif

      end do

      call pop_routine (myname)
      return
      end subroutine



*====================================================================
      subroutine correl_get_other_variables ()
*====================================================================
      Use infrastructure
      implicit none

*+  Purpose
*      Get the values of variables from other modules

*+  Changes
*       210995 jngh programmed

*+  Constant Values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'correl_get_other_variables')

*+  Local Variables
      integer    numvals               ! number of values returned
      integer    i

*- Implementation Section ----------------------------------
      call push_routine (myname)

      g%records = g%records + 1
      do i=1, g%numvars

         call get_real_var_optional (
     :      unknown_module
     :     ,g%name_found(i)
     :     ,'()'
     :     ,g%values(g%records,i)
     :     ,numvals
     :     ,-1000.0
     :     ,10000.0)

      end do

      call pop_routine (myname)
      return
      end subroutine



*====================================================================
      subroutine correl_send_my_variable (Variable_name)
*====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      character  Variable_name*(*)     ! (INPUT) Variable name to search for

*+  Purpose
*            return the value of one of our variables to       caller

*+  Changes
*       210995 jngh programmed

*+  Constant Values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'correl_send_my_variable')

*- Implementation Section ----------------------------------
      call push_routine (myname)

         call Message_unused ()

      call pop_routine (myname)
      return
      end subroutine



*================================================================
      subroutine correl_process ()
*================================================================
      Use infrastructure
      implicit none

*+  Purpose
*      perform actions for current day

*+  Changes
*       210995 jngh programmed

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'correl_process')

*- Implementation Section ----------------------------------

      call push_routine (myname)

      call pop_routine (myname)
      return
      end subroutine



*================================================================
      subroutine correl_calculate (variable_name)
*================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      character  Variable_name*(*)     ! (INPUT) Variable name to search for

*+  Purpose
*      perform actions for current day

*+  Changes
*       210995 jngh programmed

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'correl_calculate')

*+  Local Variables
      real     cor(max_numvars,max_numvars,offset*2+1)
      integer  i
      integer  j
      integer  k
      integer  n
      integer  nr
      double precision     sum
      double precision     sumsqx(max_numvars)
      double precision     diffx(max_numvars)
      double precision     sumsqy(max_numvars)
      double precision     diffy(max_numvars)
      double precision     mean(max_numvars)
      double precision     sumprod(max_numvars,max_numvars)
      character string*1000
      integer   corloc(max_numvars,max_numvars,1)
      real      temp(offset*2+1)

*- Implementation Section ----------------------------------

      call push_routine (myname)


      do n = 1, p%offset*2+1
         do i = 1, g%numvars
            sum = 0.0
            do nr = p%offset+1, g%records-p%offset
               sum = sum + g%values(nr,i)
            end do

            mean(i) = sum/g%records
            sumsqx(i) = 0.0
            sumsqy(i) = 0.0
            do j=1,g%numvars
               sumprod(i,j) = 0.0
            end do


         end do

            ! correlate on next day

         do nr = p%offset+1, g%records-p%offset
            do i = 1, g%numvars
               diffx(i) = g%values(nr,i) - mean(i)
               diffy(i) = g%values(nr+n-(p%offset+1),i) - mean(i)
               sumsqx(i) = sumsqx(i) + diffx(i)**2
               sumsqy(i) = sumsqy(i) + diffy(i)**2
            end do
            do i=1,g%numvars
               do j=1,g%numvars
               sumprod(i,j) = sumprod(i,j) + diffx(i)*diffy(j)
               end do
            end do
         end do

         do i=1,g%numvars
            if (sumsqx(i).ne.0.0) then
            do j=1,g%numvars
               if (sumsqy(j).ne.0.0) then
               cor(i,j,n) =  sumprod(i,j)
     :                  / sqrt(sumsqx(i)*sumsqy(j) )
               else
                  cor(i,j,n) = 0.0
               endif

            end do
            else
               do j=1,g%numvars
               cor(i,j,n) = 0.0
               end do
            endif
         end do
      end do

      open (unit=50, file='correl.out', status='unknown')
      rewind(50)

      string = blank
      write (string, '(10x, 100a7)') (g%name_found(i), i=1,g%numvars)
      call write_string (string)
      write (50, *) trim(string)
      do i=1,g%numvars
         write (string, '(1x, a7)') g%name_found(i)
         call write_string (string)
         write (50, *) trim(string)
         do k = 1,p%offset*2+1
            write (string, '(2x, a3, sp, i2, ss, 700f7.2)')
     :            'day',k-(p%offset+1), (cor(i,j,k), j=1,g%numvars)
            call write_string (string)
            write (50, *) trim(string)
         end do
      end do

!      write (string, '(10x, 100a7)') (g%name_found(i), i=1,g%numvars)
!      call write_string (string)
!      write (50, *) trim(string)
!      do i=1,g%numvars
!         do j=1,g%numvars
!            temp = cor(i,j,:)
!            corloc(i,j,1) = maxloc(temp)-(p%offset+1)
!         end do
!            write (string, '(1x, a7, 2x, 700f7.2)')
!     :            g%name_found(i), (corloc(i,j), j=1,g%numvars)
!            call write_string (string)
!            write (50, *) trim(string)
!      end do

      close (50)

      call pop_routine (myname)
      return
      end subroutine



      end module CorrelModule


!     ===========================================================
      subroutine alloc_dealloc_instance(doAllocate)
!     ===========================================================
      use CorrelModule
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
         allocate(id)
      else
         deallocate(g)
         deallocate(p)
         deallocate(c)
         deallocate(id)
      end if
      return
      end subroutine




*====================================================================
      subroutine Main (Action, Data_string)
*====================================================================
      Use infrastructure
      Use CorrelModule
      implicit none
      ml_external Main


*+  Sub-Program Arguments
      character  Action*(*)            ! Message action to perform
      character  Data_string*(*)       ! Message data

*+  Purpose
*      This routine is the interface between the main system and the
*      correl module.

*+  Changes
*       210995 jngh programmed
*       090696 jngh changed presence report to standard
*       190599 jngh removed version references and ACTION_presence

*+  Constant Values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'Correl_main')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      if (Action.eq.ACTION_Init) then
         call correl_zero_variables ()
         call correl_init ()
         call correl_get_other_variables_init ()
                      
      else if (Action.eq.ACTION_Create) then
         call doRegistrations(id)
                      
      elseif (Action.eq.ACTION_Prepare) then
         call correl_prepare ()

      elseif (Action.eq.ACTION_Get_variable) then
         call correl_send_my_variable (Data_string)

      elseif (Action.eq.ACTION_Process) then
         call correl_zero_daily_variables ()
         call correl_get_other_variables ()
         call correl_process ()

      elseif (Action.eq.ACTION_End_Run) then
         call correl_calculate (Data_string)
         close(50)

      else
            ! don't use message
         call Message_unused ()

      endif

      call pop_routine (myname)
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
