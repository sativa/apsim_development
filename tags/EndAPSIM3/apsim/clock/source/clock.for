C     Last change:  E     5 Dec 2000    8:52 am
      module ClockModule
      use ComponentInterfaceModule
      use DataTypesModule

      integer, parameter :: MAX_EVENT_NAME_SIZE = 100
      integer, parameter :: MAX_NUM_EVENTS = 15

      type ClockData
         sequence
         ! Global variables
         integer day                   ! current day for simulation
         integer year                  ! current year for simulation
         integer timestep              ! length of timestep (min)
         double precision start_date   ! start date of simulation
         double precision end_date     ! end date of simulation
         double precision demo_start   ! DEMO start date of simulation
         double precision demo_end     ! DEMO end date of simulation
         double precision current_date ! current date of simulation
         double precision current_time ! current time of simulation (mins)
         logical pause_current_run     ! pause the current run.
         logical end_current_run       ! end the current run.
         integer Percent_complete      ! percentage of simulation completed.
         integer currentTimestepEvent  ! index into event list
         integer, dimension(MAX_NUM_EVENTS) :: timestepEvents
                                       ! list of all events this sequencer is going
                                       ! to publish every timestep.
         integer numTimestepEvents     ! number of timestep events.
      end type ClockData

      ! instance variables.
      common /InstancePointers/ ID,g,p,c
      save InstancePointers
      type (ClockData),pointer :: g
      type (IDsType), pointer :: ID

      ! Constant values
      integer mins_in_day
      parameter (mins_in_day = 1440)

      end module ClockModule

!     ===========================================================
      subroutine alloc_dealloc_instance(doAllocate)
!     ===========================================================
      use ClockModule
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
      end

!     ===========================================================
      subroutine do_init1(sdml)
!     ===========================================================
      use ClockModule
      implicit none
      ml_external do_init1

!+  Purpose
!      Perform all registrations

!+  Sub-Program Arguments
      character (len=*), intent(in) :: sdml

!- Implementation Section ----------------------------------

      call do_registrations(ID)
      call clock_read_timesteps()

      g%end_current_run = .false.
      return
      end

!     ===========================================================
      subroutine respondToEvent(fromID, eventID, variant)
!     ===========================================================
      use ClockModule
      implicit none
      ml_external respondToEvent

!+  Purpose
!      Event handler for all events coming into module.

!+  Sub-Program Arguments
      integer, intent(in) :: fromID
      integer, intent(in) :: eventID
      integer, intent(in) :: variant

!- Implementation Section ----------------------------------


      return
      end
!     ===========================================================
      subroutine respondToMethod(fromID, variant)
!     ===========================================================
      use ClockModule
      implicit none
      ml_external respondToMethod

!+  Purpose
!      Method handler for all method calls coming into module.

!+  Sub-Program Arguments
      integer, intent(in) :: fromID
      integer, intent(in) :: variant

!- Implementation Section ----------------------------------


      return
      end

!     ===========================================================
      subroutine notify_termination()
!     ===========================================================
      use ClockModule
      implicit none
      ml_external notify_termination

!+  Purpose
!      Perform all registrations

!- Implementation Section ----------------------------------

      g%end_current_run = .true.

      return
      end

* ====================================================================
      subroutine do_init2 ()
* ====================================================================
      use ClockModule
      implicit none
      ml_external do_init2

*+  Purpose
*     Initialise the clock module

*+  Changes
*     dph 25/11/96
*     nih 28/04/99 - added simple sub-daily timestep function
*     dph 19/12/00 - removed the advance_clock

*+  Calls
      integer :: registrationNameToID
      logical clock_read_params

*+  Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='clock_init')

*+  Local Variables
      character msg*400                ! message to write to summary file
      integer day, month, year
      logical ok

*- Implementation Section ----------------------------------

      call push_routine (this_routine)

      ! read in all parameters for clock module.
      ok = clock_read_params ()

      if (ok) then
         ! set the clock to start_day.

         g%pause_current_run = .false.
         g%percent_complete = -1

         g%current_date = g%start_date
         g%current_time = -g%timestep
         call jday_to_day_of_year (g%current_date,
     .                             g%day,
     .                             g%year)

         call clock_advance_clock()

         ! write parameters to summary file.
         call jday_to_date (day, month, year, g%start_date)
         write (msg, '(a, i2,a,i2,a,i4)')
     .         'Simulation start date = ',
     .         day, '/', month, '/', year
         if (msg(28:28) .eq. Blank) then
            msg(28:28) = '0'
         endif
         call Write_string (msg)

         call jday_to_date (day, month, year, g%end_date)
         write (msg, '(a, i2,a,i2,a,i4)')
     .         'Simulation end date   = ',
     .         day, '/', month, '/', year
         if (msg(28:28) .eq. Blank) then
            msg(28:28) = '0'
         endif
         call Write_string (msg)

         write (msg, '(a, i4, a)')
     .      'Time step =           = ', g%timestep, ' (mins)'
         call Write_string (msg)
      endif

      call pop_routine (this_routine)
      return
      end subroutine


* ====================================================================
      logical function clock_read_params ()
* ====================================================================
      use ClockModule
      use DateModule
      implicit none

*+  Purpose
*     read in all parameters

*+  Changes
*     sdb 28/03/01 - externalized from the read params section in clock.for for the purposes of demo module

*+  Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='read_params')

*+  Local Variables
      integer day_of_year              ! day of year
      integer year                     ! year
      integer thisdate(3)              ! day, month, year
      integer numvals                  ! used in call to read_integer_var routine
      character date_st*(100)          ! string representation of date.
      logical found
      integer i

*- Implementation Section ----------------------------------

      call push_routine (this_routine)

      ! go get a start date
      found = read_parameter('parameters', 'start_date', date_st)
      if (found) then
         call String_to_jday (date_st, g%start_date, numvals, 0.0d0)

         if (numvals.eq.0) then
            call error ('Cannot convert the date:'
     .                  // TRIM(date_st)
     .                  //' to a valid date (dd/mm/yyyy)', .true.)
         endif

         ! go get an end date
         found = read_parameter('parameters', 'end_date', date_st)
         if (found) then
            call String_to_jday (date_st, g%end_date, numvals, 0.0d0)

            if (numvals.eq.0) then
               call error ('Cannot convert the date:'
     .                     // TRIM(date_st)
     .                     //' to a valid date (dd/mm/yyyy)', .true.)
            endif
         endif
      endif
      clock_read_params = found

      found = read_parameter('parameters', 'timestep', g%timestep, 1,
     .                   mins_in_day, .true.)

      if (.not. found) then
         g%timestep = mins_in_day
      endif

      if (mod(mins_in_day,g%timestep).ne.0) then
         call error (
     :       'Timestep must be factor of 1440 minutes (one day)',
     :       .true.)
      else
      endif

      call pop_routine(this_routine)
      return
      end

* ====================================================================
      subroutine clock_read_timesteps ()
* ====================================================================
      use ClockModule
      use DateModule
      implicit none

*+  Purpose
*     read in all parameters

*+  Changes
*     sdb 28/03/01 - externalized from the read params section in clock.for for the purposes of demo module

*+  Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='clcok_read_timesteps')

*+  Local Variables
      character(len=*), dimension(MAX_NUM_EVENTS)
     .     , parameter :: timestepEvents
     .   = (/'prepare                        ',
     .       'DoMicromet                     ',
     .       'process                        ',
     .       'DoSurfaceWaterBalance          ',
     .       'DoSoilWaterBalance             ',
     .       'DoSoilTemperature              ',
     .       'DoPotentialResidueDecomposition',
     .       'DoCropgrowth                   ',
     .       'DoSoilNitrogenBalance          ',
     .       'DoSoilPhosphorusBalance        ',
     .       'DoCropUpdate                   ',
     .       'DoResidue                      ',
     .       'DoErosion                      ',
     .       'post                           ',
     .       'rep                            '/)
      integer i

*- Implementation Section ----------------------------------

      call push_routine(this_routine)

      ! timestep events are now hardcoded because windows won't let us
      ! read from a clock.ini file.  Under NT platforms this file is
      ! mapped to the registry, so when we read from a clock.ini, Windows
      ! goes looking in the registry!  Thanks Bill!
      g%numTimestepEvents = 15
!      found = read_parameter('constants',
!     .                       'timestep_events',
!     .                       timestepEvents,
!     .                       g%numTimestepEvents)

      ! Register all timestep events.
      do i=1, g%numTimestepEvents
         g%timestepEvents(i) = add_registration
     .         (eventReg, timestepEvents(i), nullddml)
      enddo

      call pop_routine (this_routine)
      return
      end subroutine

* ====================================================================
      subroutine clock_advance_clock ()
* ====================================================================
      use ClockModule
      implicit none

*+  Purpose
*     advance the simulation to the next timestep.

*+  Changes
*     dph 25/11/96
*     nih 28/04/99 - added simple sub-daily timestep function
*     dph 21/7/99 - changed to subroutine.
*     dph 3/8/99  - added code to print percent complete to screen.

*+  Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='clock_next_timestep')

*- Implementation Section ----------------------------------

      call push_routine (This_routine)
      g%current_time = g%current_time + g%timestep

      g%current_date = g%start_date
     :               + int(g%current_time/dble(mins_in_day))

      ! set the event to publish next
      g%currentTimestepEvent = 1

      ! check for end of run conditions.

      if (int(g%current_date) .eq. int(g%end_date + 1)) then
         call Write_string (
     .       'Simulation is terminating due to end ' //
     .       'criteria being met.')
         g%end_current_run = .true.
         call terminate_simulation()
      else
         ! convert julian day to day and year for speed reasons later.

         call jday_to_day_of_year (g%current_date,
     .                             g%day,
     .                             g%year)

         call Clock_DoTick()

      endif

!      call clock_print_percent_complete ()

      call pop_routine (This_routine)
      return
      end subroutine

!* ====================================================================
!       subroutine clock_print_percent_complete ()
!* ====================================================================
!      use ClockModule
!      implicit none
!
!*+  Sub-Program Arguments
!
!*+  Calls
!      dll_import screen_writepercentcomplete
!
!*+  Purpose
!*      print a percentage complete if necessary.
!
!*+  Changes
!*     dph 3/8/99
!*     dph 3/11/99 modified to only call Screen_writepercentcomplete
!*                 if percent is a multiple of 5.
!
!*+  Constant Values
!
!*+  Local Variables
!      integer New_percent_complete         ! percentage of simulation completed.
!
!*- Implementation Section ----------------------------------
!
!      ! print out percent complete to screen if necessary
!      New_percent_complete = (g%current_date - g%start_date)
!     .                   / (g%end_date - g%start_date) * 100.0
!      if (New_percent_complete - g%Percent_complete .ge. 5 .or.
!     .    g%Percent_complete .eq. -1) then
!         call Screen_WritePercentComplete (New_percent_complete)
!         g%Percent_complete = New_percent_complete
!      endif
!      return
!      end subroutine
!
* ====================================================================
       subroutine respondToGet(fromID, variable_info)
* ====================================================================
      use ClockModule
      use DateModule
      implicit none
      ml_external respondToGet

*+  Sub-Program Arguments
      integer, intent(in) :: fromID
      type(QueryData), intent(in) :: variable_info

*+  Calls
      character Clock_time_string*(5)      ! function
      type(TimeType) :: clock_get_time

*+  Purpose
*      Return the value of a variable

*+  Changes
*     dph 25/11/96
*     nih 28/04/99 - added timestep output
*     nih 17/05/99 - added start/end time outputs - C191

*+  Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='clock_send_my_variable')

*+  Local Variables
      integer thisdate(3)              ! day, month, year
      logical logical_to_return        ! logical value to return to calling module
      character time_string*(5)        ! time string
      integer   doy                    ! day of year
      integer   day
      integer   month
      integer   year                   ! year
      character str*100                ! string for date formatting
      type(TimeType) :: time          ! time to send back

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      call day_of_year_to_date (g%day, g%year, thisdate)

      if (variable_info%id .eq. ID%day) then
         call return_day(variable_info, g%day)

      else if (variable_info%id .eq. ID%year) then
         call return_year(variable_info, g%year)

      else if (variable_info%id .eq. ID%timestep) then
         call return_timestep(variable_info, g%timestep)

      else if (variable_info%id .eq. ID%day_Of_Month) then
         call jday_to_date (thisdate(1), thisdate(2), thisdate(3),
     .                      g%current_date)
         call return_day_of_month(variable_info, thisdate(1))

      else if (variable_info%id .eq. ID%month) then
         call jday_to_date (thisdate(1), thisdate(2), thisdate(3),
     .                      g%current_date)
         call return_month(variable_info, thisdate(2))

      else if (variable_info%id .eq. ID%start_Week) then
         Logical_to_return = Start_week (g%day, g%year)
         call return_start_week(variable_info, Logical_to_return)

      else if (variable_info%id .eq. ID%end_Week) then
         Logical_to_return = End_week (g%day, g%year)
         call return_end_week(variable_info, Logical_to_return)

      else if (variable_info%id .eq. ID%start_Month) then
         Logical_to_return = Start_month (g%day, g%year)
         call return_start_month(variable_info, Logical_to_return)

      else if (variable_info%id .eq. ID%end_Month) then
         Logical_to_return = End_month (g%day, g%year)
         call return_end_month(variable_info, Logical_to_return)

      else if (variable_info%id .eq. ID%end_Year) then
         Logical_to_return = End_year (g%day, g%year)
         call return_end_year(variable_info, Logical_to_return)

      else if (variable_info%id .eq. ID%today) then
         call return_today(variable_info,
     .         Date_to_jday(thisdate(1), thisdate(2), thisdate(3)))

      else if (variable_info%id .eq. ID%today_day) then
         call return_today_day(variable_info, thisdate(1))

      else if (variable_info%id .eq. ID%today_month) then
         call return_today_month(variable_info, thisdate(2))

      else if (variable_info%id .eq. ID%today_year) then
         call return_today_year(variable_info, thisdate(3))

      else if (variable_info%id .eq. ID%today_day_Of_Year) then
         call return_today_day_of_year(variable_info, g%day)

      else if (variable_info%id .eq. ID%today_month_Str) then
         call return_today_month_str(variable_info,
     .         Get_month_string(thisdate(2)))

      else if (variable_info%id .eq. ID%today_ddmm) then
         write (str, '(i2,a,i2)')
     .        thisdate(1), '/', thisdate(2)

         if (str(1:1) .eq. Blank) then
            str(1:1) = '0'
         endif

         if (str(4:4) .eq. Blank) then
            str(4:4) = '0'
         endif
         call return_today_ddmm(variable_info, str)

      else if (variable_info%id .eq. ID%today_ddmmyyyy) then
         write (str, '(i2,a,i2,a,i4)')
     .        thisdate(1), '/', thisdate(2), '/', thisdate(3)

         if (str(1:1) .eq. Blank) then
            str(1:1) = '0'
         endif

         if (str(4:4) .eq. Blank) then
            str(4:4) = '0'
         endif
         call return_today_ddmmyyyy(variable_info, str)

      else if (variable_info%id .eq. ID%today_dd_mmm_yyyy) then
         write (str, '(i2,a,a,a,i4)')
     .        thisdate(1), '_', Get_month_string(thisdate(2)),
     .        '_', thisdate(3)

         if (str(1:1) .eq. Blank) then
            str(1:1) = '0'
         endif

         call return_today_dd_mmm_yyyy(variable_info, str)

      else if (variable_info%id .eq. ID%today_ddmmmyyyy) then
         write (str, '(i2,a,a,a,i4)')
     .        thisdate(1), '/', Get_month_string(thisdate(2)),
     .        '/', thisdate(3)

         if (str(1:1) .eq. Blank) then
            str(1:1) = '0'
         endif

         call return_today_ddmmmyyyy(variable_info, str)

      else if (variable_info%id .eq. ID%today_dd_mmm) then
         write (str, '(i2,a,a)')
     .        thisdate(1), '_', Get_month_string(thisdate(2))

         if (str(1:1) .eq. Blank) then
            str(1:1) = '0'
         endif

         call return_today_dd_mmm(variable_info, str)

      else if (variable_info%id .eq. ID%time) then
         time = clock_get_time()
         call return_time(variable_info, time)

      else if (variable_info%id .eq. ID%time_string) then
         time_string = clock_time_string()
         call return_time_string(variable_info, time_string)

      else if (variable_info%id .eq. ID%simulation_Start_Day) then
         call jday_to_day_of_year (g%Start_date
     .                            ,doy
     .                            ,year)
         call return_simulation_start_day(variable_info, doy)

      else if (variable_info%id .eq. ID%simulation_Start_Year) then
         call jday_to_day_of_year (g%Start_date
     .                            ,doy
     .                            ,year)
         call return_simulation_start_year(variable_info, year)

      else if (variable_info%id .eq. ID%simulation_End_Day) then
         call jday_to_day_of_year (g%End_date
     .                            ,doy
     .                            ,year)
         call return_simulation_end_day(variable_info, doy)

      else if (variable_info%id .eq. ID%simulation_End_Year) then
         call jday_to_day_of_year (g%End_date
     .                            ,doy
     .                            ,year)
         call return_simulation_end_year(variable_info, year)

      else if (variable_info%id .eq. ID%simulation_Start_Date) then
         call return_simulation_start_date(variable_info, g%start_date)

      else if (variable_info%id .eq. ID%simulation_End_Date) then
         call return_simulation_end_date(variable_info, g%end_date)

      endif

      call pop_routine(This_routine)
      return
      end subroutine

* ====================================================================
       logical function respondToSet (fromID, VariableID, variant)
* ====================================================================
      use ClockModule
      implicit none
      ml_external respondToSet


!+  Sub-Program Arguments
      integer, intent(in) :: fromID
      integer, intent(in)     :: VariableID
      integer, intent(in out) :: variant

*+  Purpose
*     Set one of our variables altered by some other module

*+  Changes
*      21-06-96 NIH Changed respond2set calls to collect calls

*+  Calls

*+  Local Variables

*- Implementation Section ----------------------------------
      respondToSet = .true.
      return
      end

* ====================================================================
       subroutine do_commence()
* ====================================================================
      use ClockModule
      implicit none
      ml_external do_commence

*+  Purpose
*     start the clock going. ie. start the simulation

*+  Changes
*        DPH - 26/11/96
*        dph - 19/12/00 removed call to send init messages to all modules.

*+  Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='clock_start')

*- Implementation Section ----------------------------------

      call push_routine (This_routine)

      ! tell summary service to enter the diary state ie. not the
      ! initialisation state.
!      call Summary_enter_diary_state ()

      ! enter an infinate loop until end of run is signalled.
      do while (.not. g%end_current_run)
         call clock_next_phase()
      end do

      call pop_routine (This_routine)
      return
      end subroutine



* ====================================================================
       subroutine Clock_next_phase ()
* ====================================================================
      use ClockModule
      implicit none

*+  Purpose
*     Cycle through all phases for an entire simulation.  Exit routine
*     when simulation has completed.

*+  Changes
*      DPH 26/11/96
*      NIH 25/08/99 - Added Tick Event

*+  Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='Clock_do_timestep')

*+  Local variables
      type(MessagePtr) :: amessagePtr

*- Implementation Section ----------------------------------

      call push_routine (This_routine)

      call publish_null(g%timestepEvents(g%currentTimestepEvent)
     :                  ,.false.)

      g%currentTimestepEvent = g%currentTimestepEvent + 1
      if (g%currentTimestepEvent .gt. g%numTimestepEvents .and.
     .    .not. g%end_current_run) then
         call clock_advance_clock()
      endif

      call pop_routine (This_routine)

      return
      end subroutine

* ====================================================================
       character*(*) function Clock_time_string ()
* ====================================================================
      use ClockModule
      implicit none

*+  Purpose
*     Create a string giving the daily time in 24 hour format
*     of hh:mm

*+  Changes
*      NIH 04/05/99

*+  Notes
*      NIH 04/05/99 - this will fail is the starting time is not
*                     the beginning of a day

*+  Local Variables
      character temp*5
      integer time_mins  !time since start of day (min)
      integer hour
      integer mins

*- Implementation Section ----------------------------------

      time_mins = mod(int(g%current_time), mins_in_day)

      hour = int(time_mins/60)
      mins = mod(time_mins, 60)

      if (len(clock_time_String).ge.5) then

         if (hour.lt.10) then
            if (mins.lt.10) then
               write (temp,'(''0'',i1,'':0'',i1)') hour,mins
            else
               write (temp,'(''0'',i1,'':'',i2)') hour,mins
            endif
         else
            if (mins.lt.10) then
               write (temp,'(i2,'':0'',i1)') hour,mins
            else
               write (temp,'(i2,'':'',i2)') hour,mins
            endif
         endif

         call assign_string (clock_time_String, temp)
      else
         call error (
     :               'Time string requires at least 5 chars', .true.)
      endif

      return
      end function

! ====================================================================
       subroutine Clock_DoTick ()
! ====================================================================
      use ClockModule
      implicit none

!+  Purpose
!     Notify all modules of a clock tick and the new timestep

!+  Changes
!      NIH 25/08/99

*+  Calls
      type(TimeType) :: clock_get_time

!+  Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='clock_dotick')

!+  Local Variables
      type(TimeType) :: tick

!- Implementation Section ----------------------------------

      call push_routine (This_routine)

      tick = Clock_get_time()
      call publish_time(ID%tick, tick, .false.)

      call pop_routine (This_routine)

      return
      end

! ====================================================================
       function Clock_get_time()
! ====================================================================
      use ClockModule
      implicit none

!+  Sub-Program Arguments
      type(TimeType) :: Clock_get_time


!+  Calls
      character Clock_time_string*(5)      ! function

!+  Purpose
!     Notify all modules of a clock tick and the new timestep

!+  Changes
!      NIH 25/08/99

!+  Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='Clock_get_time')

!+  Local Variables
      character time*(5)               ! time in 24 hour format
      integer time_mins                !time since start of day (min)
      integer date(3)

!- Implementation Section ----------------------------------

      call push_routine (This_routine)

      time = clock_time_string()

      ! Work out which timestep we're in.
      time_mins = mod(int(g%current_time), mins_in_day)

      call day_of_year_to_date(g%day, g%year, date);

      ! New tick event.
      clock_get_time%startday = Date_to_jday(date(1), date(2), date(3))
      clock_get_time%startsec = time_mins*60.0
      clock_get_time%startsecpart = 0.0;
      clock_get_time%endday = clock_get_time%startday
      clock_get_time%endsec = (time_mins+g%timestep) * 60.0 - 1.0
      clock_get_time%endsecpart = 1.0;

      call pop_routine (This_routine)

      return
      end

