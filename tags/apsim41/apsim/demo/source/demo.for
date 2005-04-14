*     ========================================
      module demoModule
      use Registrations
!     ========================================
      
!     ========================================
      Type demoGlobals
         sequence
         real    latitude
         real    longitude
         real    tav
         real    amp
         integer year
         integer day
         real    radn
         real    maxt
         real    mint
         real    rain
                 
      end type demoGlobals
!     ========================================

      ! instance variables.
      common /InstancePointers/ ID,g,p,c
      save InstancePointers
      type (demoGlobals),pointer :: g
      type (IDsType), pointer :: ID
               
      contains

* ====================================================================
       subroutine demo_Init ()
* ====================================================================
      Use infrastructure
      implicit none

*+  Purpose
*      Initialise demo module

*+  Mission Statement
*     Initialise all internal state variables

*+  Calls

*+  Changes
*     SDB 07/06/01 Created.

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'demo_init')

*+  Local Variables
       character Event_string*40       ! String to output
       integer iostatus

*- Implementation Section ----------------------------------

      call push_routine (myname)


! this is dodgy but needs to open in init to provide data for
! other init sections such as residue2. Then needs to reopen for
! proper reading at the aligned position in the binary file.

      if (demo_open_binary()) then
         call demo_read_constants ()
         call demo_read_todays_data ()
         call demo_close_binary ()
            if (demo_open_binary()) then
               call demo_read_constants ()
            end if
      else
         call fatal_error(err_user,
     .                  'Cannot open the demo met file')
      end if

      ! Notify system that we have initialised

      Event_string = 'Initialising the Demonstration met data'
      call Write_string (Event_string)

      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       logical function demo_open_binary()
* ====================================================================
      Use infrastructure
      implicit none

!+  Purpose
!     Try and open the binary file.

!+  Changes
!     sdb 07/06/01 created

*+  Calls
      dll_import getApsuiteDirectory


!+  Constant Values
      character this_routine*(*)       ! name of this routine
      parameter (this_routine='demo_open_binary')

!+  Local variables
      integer iostatus
      character apsuite_dir_name*200
      character demo_met_file*200

!- Implementation Section ----------------------------------
      call push_routine(this_routine)
      apsuite_dir_name = ' '
      demo_met_file = ' '
      call getApsuiteDirectory(apsuite_dir_name)
      demo_met_file=string_concat(apsuite_dir_name,
     :                          '\apsim\demo\met.bin')
      open(51, file=demo_met_file,
     :  iostat=iostatus, status='old', access='transparent')

      demo_open_binary = (iostatus .eq. 0)

      call pop_routine(this_routine)
      return
      end function


* ====================================================================
       subroutine demo_Send_my_variable (Variable_name)
* ====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
       character Variable_name*(*)     ! (INPUT) Variable name to search for

*+  Purpose

*+  Mission Statement
*     Supply information to requesting module

*+  Changes
*    sdb  07/06/01 created

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'demo_send_my_variable')

*+  Local Variables


*- Implementation Section ----------------------------------

      call push_routine (myname)
      !print *,variable_name
      !pause
      if (variable_name .eq. 'latitude') then
         !print *,g%latitude
         !pause

         call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'()'            ! variable units
     :              ,g%latitude)      ! variable

      elseif (variable_name .eq. 'longitude') then

         call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'()'            ! variable units
     :              ,g%longitude)      ! variable

      elseif (variable_name .eq. 'tav') then

         call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'(oC)'            ! variable units
     :              ,g%tav)      ! variable

      elseif (variable_name .eq. 'amp') then

         call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'(oC)'            ! variable units
     :              ,g%amp)      ! variable

      elseif (variable_name .eq. 'radn') then

         call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'(MJ/m2)'            ! variable units
     :              ,g%radn)               ! variable

      elseif (variable_name .eq. 'maxt') then

         call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'(oC)'            ! variable units
     :              ,g%maxt)               ! variable


      elseif (variable_name .eq. 'mint') then

         call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'(oC)'            ! variable units
     :              ,g%mint)               ! variable

      elseif (variable_name .eq. 'rain') then

         call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'(mm)'            ! variable units
     :              ,g%rain)               ! variable

      else
         ! not my variable

         call Message_unused ()

      endif



      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       subroutine demo_read_constants ()
* ====================================================================
      Use infrastructure
      implicit none

*+  Purpose
*      Read in all constants

*+  Mission Statement
*     Read constants

*+  Changes
*     sdb  07/06/01 - Programmed and Specified

*+  Constant Values
      character*(*) section_name
      parameter (section_name = 'constants')
*
      character*(*) myname               ! name of current procedure
      parameter (myname = 'demo_read_constants')

*+  Local Variables
      integer    iostatus           ! status of the read
*- Implementation Section ----------------------------------
      call push_routine (myname)

      read(51, iostat=iostatus) g%latitude, g%longitude, g%tav, g%amp
      if (iostatus .ne. 0)  then
            call fatal_error(err_user,
     .                  'Cannot read constants from the met file')

      endif
      call pop_routine (myname)
      return
      end subroutine

!     ===========================================================
      subroutine demo_close_binary()
!     ===========================================================
      Use infrastructure
      implicit none

!+  Sub-Program Arguments


!+  Purpose
!     Closes the Dalby binary file which has been given number of 51

!+  Definition
!     Close the open file.

!+  Changes
!     SDB 06/06/01  Created.

!+  Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'demo_close_binary')

!- Implementation section ----------------------
      call push_routine (my_name)

      close(51)

      call pop_routine (my_name)
      return
      end subroutine

! ====================================================================
      subroutine demo_read_todays_data()
! ====================================================================
      Use infrastructure
      implicit none

!+  Purpose
!     Read in todays demo data.

!+  Changes
!      sdb 07/06/01 created


!+  Constant Values
      character this_routine*(*)       ! name of this routine
      parameter (this_routine='demo_read_todays_data')

!- Implementation Section ----------------------------------
      call push_routine(this_routine)

      call demo_read_line()
      call met_send_newmet_event()

      call pop_routine(this_routine)
      return
      end subroutine

! ====================================================================
      subroutine demo_read_line()
! ====================================================================
      Use infrastructure
      implicit none

!+  Purpose
!     Read todays demo data.

!+  Changes
!      sdb 07/06/01 created


!+  Constant Values
      character this_routine*(*)       ! name of this routine
      parameter (this_routine='demo_read_line')

!+  Local Variables
      double precision todays_date
      double precision files_date
      integer file_date(3), numvals
      integer iostatus

!- Implementation Section ----------------------------------
      call push_routine(this_routine)
      call get_double_var(unknown_module, 'today', '(day)',
     .                     todays_date, numvals, 0.0d0, 3660000000.0d0)

      do
         read(51, iostat=iostatus) g%year,g%day,g%radn,g%maxt,g%mint,
     :                             g%rain
         if (iostatus .eq. 0) then
            ! Convert date.
            call day_of_year_to_date(g%day, g%year, file_date)
            files_date = date_to_jday(file_date(1),
     .                              file_date(2), file_date(3))
            if (Doubles_are_equal (files_date,todays_date)) then
               exit
            end if
         else
            call fatal_error(err_user,
     .            'Dates not within met data bounds')
            exit
         endif
      end do

100   continue
      call pop_routine(this_routine)
      return
      end subroutine

* ====================================================================
      subroutine met_send_newmet_event()
* ====================================================================
      Use infrastructure
      implicit none

*+  Purpose
*      Broadcast the new met data

*+  Changes

*+  Constant Values
      character this_routine*(*)       ! name of this routine
      parameter (this_routine='met_send_newmet_event')

*+  Local Variables
      real       vp
      real       svp           ! function to get saturation vapour
                               ! pressure for a given temperature in oC (kpa)
      real       temp_arg      ! dummy temperature for function (oC)
*
      svp(temp_arg) = 6.1078
     :              * exp (17.269*temp_arg/ (237.3 + temp_arg))


*- Implementation Section ----------------------------------
      call push_routine(this_routine)

      vp = svp(g%mint)

      call new_postbox()

      call post_real_var   (DATA_radn
     :                     , '()'
     :                     , g%radn)
      call post_real_var   (DATA_maxt
     :                     , '()'
     :                     , g%maxt)
      call post_real_var   (DATA_mint
     :                     , '()'
     :                     , g%mint)
      call post_real_var   (DATA_rain
     :                     , '()'
     :                     , g%rain)
      call post_real_var   (DATA_vp
     :                     , '()'
     :                     , vp)

      call event_send (EVENT_newmet)

      call delete_postbox()

      call pop_routine(this_routine)
      return
      end subroutine

      end module demoModule

!     ===========================================================
      subroutine alloc_dealloc_instance(doAllocate)
!     ===========================================================
      use demoModule
      implicit none  
      ml_external alloc_dealloc_instance

!+  Sub-Program Arguments
      logical, intent(in) :: doAllocate

!+  Purpose
!      Module instantiation routine.

!- Implementation Section ----------------------------------

      if (doAllocate) then
         allocate(g)
         allocate(ID)
      else
         deallocate(g)
         deallocate(ID)
      end if
      return
      end subroutine

* ====================================================================
       subroutine Main (Action, Data_string)
* ====================================================================
      use demoModule
      Use infrastructure
      implicit none
      ml_external Main

*+  Sub-Program Arguments
       character Action*(*)            ! Message action to perform
       character Data_string*(*)       ! Message data

*+  Purpose
*      This routine is the interface between the main system and the
*      demo module.

*+  Mission Statement
*     Apsim demo

*+  Changes
*     SDB 7/6/01 Created

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'demo Main')

*- Implementation Section ----------------------------------

      call push_routine (myname)

      if (Action.eq.ACTION_Init) then
         call demo_Init ()

      else if (Action.eq.ACTION_Create) then
         call doRegistrations(id)

      else if (Action.eq.ACTION_Prepare) then
         call demo_read_todays_data()

      else if (Action.eq.ACTION_Get_variable) then
         call demo_Send_my_variable (Data_string)

      else if (action .eq. ACTION_end_run) then
         call demo_close_binary()

      else
         ! Don't use message
         call Message_Unused ()
      endif

      call pop_routine (myname)
      return
      end

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
