C     Last change:  E     5 Dec 2000    8:52 am
      module ClockModule

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
      end type ClockData

      ! Constant values
      integer mins_in_day
      parameter (mins_in_day = 1440)

      character Module_name*(*)       ! Module name
      parameter (Module_name='clock')

      ! instance variables.
      type (ClockData), pointer :: g
      integer MAX_NUM_INSTANCES
      parameter (MAX_NUM_INSTANCES=10)
      integer MAX_INSTANCE_NAME_SIZE
      parameter (MAX_INSTANCE_NAME_SIZE=50)
      type ClockDataPtr
         type (ClockData), pointer :: ptr
         character Name*(MAX_INSTANCE_NAME_SIZE)
      end type ClockDataPtr
      type (ClockDataPtr), dimension(MAX_NUM_INSTANCES) :: Instances

      end module ClockModule

!     ===========================================================
      subroutine AllocInstance (InstanceName, InstanceNo)
!     ===========================================================
      use ClockModule
      Use infrastructure
      implicit none

!+  Sub-Program Arguments
      character InstanceName*(*)       ! (INPUT) name of instance
      integer   InstanceNo             ! (INPUT) instance number to allocate

!+  Purpose
!      Module instantiation routine.

!- Implementation Section ----------------------------------

      allocate (Instances(InstanceNo)%ptr)
      Instances(InstanceNo)%Name = InstanceName

      return
      end

!     ===========================================================
      subroutine FreeInstance (anInstanceNo)
!     ===========================================================
      use ClockModule
      Use infrastructure
      implicit none

!+  Sub-Program Arguments
      integer anInstanceNo             ! (INPUT) instance number to allocate

!+  Purpose
!      Module de-instantiation routine.

!- Implementation Section ----------------------------------

      deallocate (Instances(anInstanceNo)%ptr)

      return
      end

!     ===========================================================
      subroutine SwapInstance (anInstanceNo)
!     ===========================================================
      use ClockModule
      Use infrastructure
      implicit none

!+  Sub-Program Arguments
      integer anInstanceNo             ! (INPUT) instance number to allocate

!+  Purpose
!      Swap an instance into the global 'g' pointer

!- Implementation Section ----------------------------------

      g => Instances(anInstanceNo)%ptr

      return
      end


* ====================================================================
      subroutine Main (Action, Data)
* ====================================================================
       use ClockModule
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
       character Action*(*)            ! Message action to perform
       character Data*(*)              ! Message data

*+  Purpose
*      This module makes various clock variables available to rest
*      of system.

*+  Changes
*     dph 25/11/96

*- Implementation Section ----------------------------------

      if (Action.eq.ACTION_Get_variable) then
         call clock_send_my_variable (Data)

      else if (Action .eq. ACTION_Init) then
         call clock_init ()

      else if (Action.eq.ACTION_Start) then
         call clock_start ()

      else if (Action.eq.ACTION_Pause) then
         g%pause_current_run = .true.

      else if (Action.eq.ACTION_Continue) then
         g%pause_current_run = .false.

      else if (Action.eq.ACTION_Finish) then
         ! must have been a fatal error better tell crops
         ! that we're about to end.

         call Action_send_to_all_comps (ACTION_End_run)

      else
         ! Not our variable

         call Message_unused ()
      endif

      return
      end



* ====================================================================
      subroutine clock_init ()
* ====================================================================
      use ClockModule
      Use infrastructure
      implicit none

*+  Purpose
*     Initialise the clock module

*+  Changes
*     dph 25/11/96
*     nih 28/04/99 - added simple sub-daily timestep function
*     dph 19/12/00 - removed the advance_clock

*+  Calls

*+  Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='clock_init')

*+  Local Variables
      character msg*400                ! message to write to summary file
      integer day, month, year

*- Implementation Section ----------------------------------

      call push_routine (this_routine)

      ! read in all parameters for clock module.

      call clock_read_params ()

      ! set the clock to start_day.

      g%end_current_run = .false.
      g%pause_current_run = .false.
      g%percent_complete = -1

      g%current_date = g%start_date
      g%current_time = -g%timestep
      call jday_to_day_of_year (g%current_date,
     .                          g%day,
     .                          g%year)

      call clock_advance_clock()

      ! write parameters to summary file.
      call jday_to_date (day, month, year, g%start_date)
      write (msg, '(a, i2,a,i2,a,i4)')
     .      'Simulation start date = ',
     .      day, '/', month, '/', year
      if (msg(28:28) .eq. Blank) then
         msg(28:28) = '0'
      endif
      call Write_string (msg)

      call jday_to_date (day, month, year, g%end_date)
      write (msg, '(a, i2,a,i2,a,i4)')
     .      'Simulation end date   = ',
     .      day, '/', month, '/', year
      if (msg(28:28) .eq. Blank) then
         msg(28:28) = '0'
      endif
      call Write_string (msg)

      write (msg, '(a, i4, a)')
     .   'Time step =           = ', g%timestep, ' (mins)'
      call Write_string (msg)

!      call Clock_DoTick()

      call pop_routine (this_routine)
      return
      end subroutine



* ====================================================================
      subroutine clock_read_params ()
* ====================================================================
      use ClockModule
      Use infrastructure
      implicit none

*+  Purpose
*     read in all parameters

*+  Changes
*     dph 25/11/96
*     nih 28/04/99 - added timestep parameter
*     nih 17/05/99 - changing name of start/end to simulation_start_??? etc - C191
*     dph 23/10/00 - changed to read dates instead of day and year.
*     sdb 28/03/01 - externalized read params for the purposes of demo module

*+  Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='clock_read_params')

*+  Local Variables


*- Implementation Section ----------------------------------

      call push_routine (this_routine)

      call read_params ()
      call pop_routine (this_routine)
      return
      end subroutine



* ====================================================================
      subroutine clock_advance_clock ()
* ====================================================================
      use ClockModule
      Use infrastructure
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

      ! check for end of run conditions.

      if (int(g%current_date) .eq. int(g%end_date + 1)) then
         call Write_string (
     .       'Simulation is terminating due to end ' //
     .       'criteria being met.')
         g%end_current_run = .true.
      else
         ! convert julian day to day and year for speed reasons later.

         call jday_to_day_of_year (g%current_date,
     .                             g%day,
     .                             g%year)

         g%end_current_run = .false.

         call Clock_DoTick()

      endif

      call clock_print_percent_complete ()

      call pop_routine (This_routine)
      return
      end subroutine

* ====================================================================
       subroutine clock_print_percent_complete ()
* ====================================================================
      use ClockModule
      Use infrastructure
      implicit none

*+  Sub-Program Arguments

*+  Calls
      dll_import screen_writepercentcomplete

*+  Purpose
*      print a percentage complete if necessary.

*+  Changes
*     dph 3/8/99
*     dph 3/11/99 modified to only call Screen_writepercentcomplete
*                 if percent is a multiple of 5.

*+  Constant Values

*+  Local Variables
      integer New_percent_complete         ! percentage of simulation completed.

*- Implementation Section ----------------------------------

      ! print out percent complete to screen if necessary
      New_percent_complete = (g%current_date - g%start_date)
     .                   / (g%end_date - g%start_date) * 100.0
      if (New_percent_complete - g%Percent_complete .ge. 5 .or.
     .    g%Percent_complete .eq. -1) then
         call Screen_WritePercentComplete (New_percent_complete)
         g%Percent_complete = New_percent_complete
      endif
      return
      end subroutine

* ====================================================================
       subroutine clock_send_my_variable (Variable_name)
* ====================================================================
      use ClockModule
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
       character Variable_name*(*)     ! (INPUT) Variable name to search for

*+  Calls
      character Clock_time_string*(5)      ! function

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
      character time*(5)               ! time in 24 hour format
      integer   doy                    ! day of year
      integer   year                   ! year

*- Implementation Section ----------------------------------

      call push_routine(This_routine)
      if (variable_name .eq. 'day') then
         call respond2get_integer_var (Variable_name,
     .                                 '(day)',
     .                                 g%day)
      else if (variable_name .eq. 'year') then
         call respond2get_integer_var (Variable_name,
     .                                 '(year)',
     .                                 g%year)
      else if (variable_name .eq. 'timestep') then
         call respond2get_integer_var (Variable_name,
     .                                 '(min)',
     .                                 g%timestep)
      else if (variable_name .eq. 'day_of_month') then
         call jday_to_date (thisdate(1), thisdate(2), thisdate(3),
     .                      g%current_date)
         call respond2get_integer_var (Variable_name,
     .                                 '(day)',
     .                                 thisdate(1))
      else if (variable_name .eq. 'month') then
         call jday_to_date (thisdate(1), thisdate(2), thisdate(3),
     .                      g%current_date)
         call respond2get_integer_var (Variable_name,
     .                                 '(month)',
     .                                 thisdate(2))

      else if (Variable_name .eq. 'start_week') then
         Logical_to_return = Start_week (g%day, g%year)
         call Respond2get_logical_var
     .       (variable_name, '(0-1)', Logical_to_return)

      else if (Variable_name .eq. 'end_week') then
         Logical_to_return = End_week (g%day, g%year)
         call Respond2get_logical_var
     .       (variable_name, '(0-1)', Logical_to_return)

      else if (Variable_name .eq. 'start_month') then
         Logical_to_return = Start_month (g%day, g%year)
         call Respond2get_logical_var
     .       (variable_name, '(0-1)', Logical_to_return)

      else if (Variable_name .eq. 'end_month') then
         Logical_to_return = End_month (g%day, g%year)
         call Respond2get_logical_var
     .       (variable_name, '(0-1)', Logical_to_return)

      else if (Variable_name .eq. 'end_year') then
         Logical_to_return = End_year (g%day, g%year)
         call Respond2get_logical_var
     .       (variable_name, '(0-1)', Logical_to_return)

      else if (index(variable_name, 'today') .eq. 1) then
         call clock_today_object(variable_name(6:))

      else if (variable_name .eq. 'time') then
         time = clock_time_string()
         call respond2get_char_var (Variable_name,
     .                                 '()',
     .                                 time)

      else if (variable_name .eq. 'simulation_start_day') then
         call jday_to_day_of_year (g%Start_date
     .                            ,doy
     .                            ,year)
         call respond2get_integer_var (Variable_name,
     .                                 '()',
     .                                 doy)

      else if (variable_name .eq. 'simulation_start_year') then
         call jday_to_day_of_year (g%Start_date
     .                            ,doy
     .                            ,year)
         call respond2get_integer_var (Variable_name,
     .                                 '()',
     .                                 year)

      else if (variable_name .eq. 'simulation_end_day') then
         call jday_to_day_of_year (g%End_date
     .                            ,doy
     .                            ,year)
         call respond2get_integer_var (Variable_name,
     .                                 '()',
     .                                 doy)

      else if (variable_name .eq. 'simulation_end_year') then
         call jday_to_day_of_year (g%End_date
     .                            ,doy
     .                            ,year)
         call respond2get_integer_var (Variable_name,
     .                                 '()',
     .                                 year)

      else
         ! Not our variable

         call Message_unused ()
      endif

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine clock_today_object (Variable_name)
* ====================================================================
      use ClockModule
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) name of variable

*+  Purpose
*     Get the value of a variable or constant.

*+  Changes
*        DPH - 11/4/96
*        EW  - 05/12/00 - modified to change the date output from eg "1/04/1990" to "01/04/1990"

*+  Calls

*+  Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='clock_today_object')

*+  Local Variables
      integer thisdate(3)                  ! day, month, year of todays date
      character str*100                ! string for date formatting

*- Implementation Section ----------------------------------

      call push_routine (This_routine)

      call day_of_year_to_date (g%day, g%year, thisdate)

      if (variable_name .eq. Blank) then
         call Respond2get_double_var
     .        ('today', '()',
     .         Date_to_jday(thisdate(1), thisdate(2), thisdate(3)))

      else if (variable_name .eq. '.day') then
         call Respond2get_integer_var
     .        ('today.day', '()', thisdate(1))

      else if (variable_name .eq. '.month') then
         call Respond2get_integer_var
     .        ('today.month', '()', thisdate(2))

      else if (variable_name .eq. '.year') then
         call Respond2get_integer_var
     .        ('today.year', '()', thisdate(3))

      else if (variable_name .eq. '.day_of_year') then
         call Respond2get_integer_var
     .        ('today.day_of_year', '()', g%day)

      else if (variable_name .eq. '.month_str') then
         call Respond2get_char_var
     .        ('today.month_str', '()', Get_month_string(thisdate(2)))

      else if (variable_name .eq. '.dd/mm') then
         write (str, '(i2,a,i2)')
     .        thisdate(1), '/', thisdate(2)

         if (str(1:1) .eq. Blank) then
            str(1:1) = '0'
         endif

         if (str(4:4) .eq. Blank) then
            str(4:4) = '0'
         endif
         call Respond2get_char_var
     .        ('today.dd/mm', '()', str)

      else if (variable_name .eq. '.dd/mm/yyyy') then
         write (str, '(i2,a,i2,a,i4)')
     .        thisdate(1), '/', thisdate(2), '/', thisdate(3)

         if (str(1:1) .eq. Blank) then
            str(1:1) = '0'
         endif

         if (str(4:4) .eq. Blank) then
            str(4:4) = '0'
         endif
         call Respond2get_char_var
     .        ('today.dd/mm/yyyy', '()', str)

      else if (variable_name .eq. '.dd_mmm_yyyy') then
         write (str, '(i2,a,a,a,i4)')
     .        thisdate(1), '_', Get_month_string(thisdate(2)),
     .        '_', thisdate(3)

         if (str(1:1) .eq. Blank) then
            str(1:1) = '0'
         endif

         call Respond2get_char_var
     .        ('today.dd_mmm_yyyy', '()', str)

      else if (variable_name .eq. '.dd/mmm/yyyy') then
         write (str, '(i2,a,a,a,i4)')
     .        thisdate(1), '/', Get_month_string(thisdate(2)),
     .        '/', thisdate(3)

         if (str(1:1) .eq. Blank) then
            str(1:1) = '0'
         endif

         call Respond2get_char_var
     .        ('today.dd/mmm/yyyy', '()', str)

      else if (variable_name .eq. '.dd_mmm') then
         write (str, '(i2,a,a)')
     .        thisdate(1), '_', Get_month_string(thisdate(2))

         if (str(1:1) .eq. Blank) then
            str(1:1) = '0'
         endif

         call Respond2get_char_var
     .        ('today.dd_mmm', '()', str)

      else
         write (str, '(2a)' )
     .      'The TODAY object doesnt have a method called :- ',
     .      variable_name
         call Fatal_error (ERR_user, str)

      endif

      call pop_routine (This_routine)
      return
      end subroutine



* ====================================================================
       subroutine clock_start ()
* ====================================================================
      use ClockModule
      Use infrastructure
      implicit none

*+  Purpose
*     start the clock going. ie. start the simulation

*+  Changes
*        DPH - 26/11/96
*        dph - 19/12/00 removed call to send init messages to all modules.

*+  Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='clock_start')

*+  Local Variables

*- Implementation Section ----------------------------------

      call push_routine (This_routine)

      ! tell summary service to enter the diary state ie. not the
      ! initialisation state.
      call Summary_enter_diary_state ()

      ! do all timesteps for simulation

!      g%current_date = g%current_date - 1

      call Clock_timestep_loop ()

      call pop_routine (This_routine)
      return
      end subroutine



* ====================================================================
       subroutine Clock_timestep_loop ()
* ====================================================================
      use ClockModule
      Use infrastructure
      implicit none

*+  Purpose
*     Cycle through all phases for an entire simulation.  Exit routine
*     when simulation has completed.

*+  Changes
*      DPH 26/11/96
*      NIH 25/08/99 - Added Tick Event

*+  Calls

*+  Constant Values
       integer Num_instructions
       parameter (Num_instructions=4)  ! Number of instructions to send

      character This_routine*(*)       ! name of this routine
      parameter (This_routine='clock_timestep_loop')

*+  Local Variables
       character Instructions(Num_instructions)*8
       integer Instruction_Index       ! index into instruction list

*+  Initial Data Values
       data Instructions(1) /ACTION_Prepare/
       data Instructions(2) /ACTION_Process/
       data Instructions(3) /ACTION_Post/
       data Instructions(4) /ACTION_Report/

*- Implementation Section ----------------------------------

      call push_routine (This_routine)

      ! Main timestep loop

10    continue
      do 20 Instruction_index = 1, Num_instructions

         ! Send message to all modules.
         call new_postbox()
         call event_send (Instructions(Instruction_Index))
         call delete_postbox()

         ! Check the end of simulation flag and exit if necessary

         if (g%End_current_run) then
            goto 100
         endif

         ! Check the pause flag and enter a idle loop if necessary.

         if (g%Pause_current_run) then
            call clock_idle_loop ()
         endif

20    continue

      ! loop back to next timestep if necessary.

      call clock_advance_clock()
      if (.not. g%End_current_run) then
         goto 10
      endif

      ! thats it - exit routine and simulation.
100   continue

      call pop_routine (This_routine)

      return
      end subroutine



* ====================================================================
       subroutine Clock_idle_loop ()
* ====================================================================
      use ClockModule
      Use infrastructure
      implicit none

*+  Purpose
*     Enter a idle loop where only idle messages are sent to all modules.
*     This routine is called whenever the simulation goes into pause
*     mode.

*+  Changes
*      DPH 26/11/96

*- Implementation Section ----------------------------------

10    continue
      if (g%Pause_current_run) then
         call Action_send_to_all_comps (ACTION_Idle)
         goto 10
      endif

      return
      end subroutine


* ====================================================================
       character*(*) function Clock_time_string ()
* ====================================================================
      use ClockModule
      Use infrastructure
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
         call fatal_error (Err_internal
     :                    ,'Time string requires at least 5 chars')
      endif

      return
      end function

! ====================================================================
       subroutine Clock_DoTick ()
! ====================================================================
      use ClockModule
      Use infrastructure
      implicit none

!+  Calls
      character Clock_time_string*(5)      ! function

!+  Purpose
!     Notify all modules of a clock tick and the new timestep

!+  Changes
!      NIH 25/08/99

!+  Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='clock_do_tick')

!+  Local Variables
      character time*(5)               ! time in 24 hour format

!- Implementation Section ----------------------------------

      call push_routine (This_routine)

      call new_postbox()

      call post_integer_var (DATA_day
     :                     , '()'
     :                     , g%day)

      call post_integer_var (DATA_year
     :                     , '()'
     :                     , g%year)

      time = clock_time_string()
      call post_char_var (DATA_time
     :                   , '(hh:mm)'
     :                   , time)

      call post_integer_var (DATA_timestep
     :                     , '(min)'
     :                     , g%timestep)

      call event_send (EVENT_tick)

      call delete_postbox()

      call pop_routine (This_routine)

      return
      end
      