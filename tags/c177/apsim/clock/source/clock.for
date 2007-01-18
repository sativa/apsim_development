* ====================================================================
      subroutine APSIM_clock (Action, Data)
* ====================================================================
      implicit none
      dll_export apsim_clock
       include 'const.inc'             ! Global common block
       include 'clock.inc'
      include 'engine.pub'                        
      include 'write.pub'                         

*+  Sub-Program Arguments
       character Action*(*)            ! Message action to perform
       character Data*(*)              ! Message data

*+  Purpose
*      This module makes various clock variables available to rest
*      of system.

*+  Changes
*     dph 25/11/96

*- Implementation Section ----------------------------------

      if (Action.eq.MES_Get_variable) then
         call clock_send_my_variable (Data)
 
      else if (Action .eq. MES_Init) then
         call clock_init ()
 
      else if (Action.eq.MES_Start) then
         call clock_start ()
 
      else if (Action.eq.MES_Pause) then
         g_pause_current_run = .true.
 
      else if (Action.eq.MES_Continue) then
         g_pause_current_run = .false.
 
      else if (Action.eq.MES_End_run) then
         g_end_current_run = .true.
 
      else
         ! Don't use message
 
         call Message_unused ()
      endif
 
      return
      end



* ====================================================================
      subroutine clock_init ()
* ====================================================================
      implicit none
      include 'const.inc'              ! Constant definitions
      include 'clock.inc'              ! clock common block
      include 'write.pub'                         
      include 'error.pub'                         

*+  Purpose
*     Initialise the clock module

*+  Changes
*     dph 25/11/96
*     nih 28/04/99 - added simple sub-daily timestep function

*+  Calls
      logical Clock_advance_clock      ! function

*+  Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='clock_init')

*+  Local Variables
!       character msg*400               ! message to write to summary file
       logical ok                      ! did clock advance to next day ok?

*- Implementation Section ----------------------------------
 
      call push_routine (this_routine)
 
      ! read in all parameters for clock module.

      call clock_read_params ()
 
      ! set the clock to start_day.
cih
      g_end_current_run = .false.
 
      g_current_date = g_start_date - 1.
      g_current_time = -g_timestep

      ok = clock_advance_clock ()
 
      ! tell user we have initialised.
 
!      msg = ' CLOCK Initialised, Version: '
 
!      call Write_string (LU_scr_sum, msg)
!      msg =
!     .    '---------------------------------------------------------'
!      call Write_string (LU_scr_sum, msg)
 
      call pop_routine (this_routine)
      return
      end



* ====================================================================
      subroutine clock_read_params ()
* ====================================================================
      implicit none
      include 'const.inc'              ! Constant definitions
      include 'clock.inc'              ! clock common block
      include 'date.pub'                          
      include 'read.pub'                          
      include 'error.pub'                         

*+  Purpose
*     read in all parameters

*+  Changes
*     dph 25/11/96
*     nih 28/04/99 - added timestep parameter
*     nih 17/05/99 - changing name of start/end to simulation_start_??? etc - C191

*+  Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='clock_read_params')

*+  Local Variables
      integer day_of_year              ! day of year
      integer year                     ! year
      integer thisdate(3)              ! day, month, year
      integer numvals                  ! used in call to read_integer_var routine

*- Implementation Section ----------------------------------
 
      call push_routine (this_routine)
 
      call read_integer_var_optional ('parameters',
     .                       'simulation_start_day',
     .                       '(day)',
     .                       day_of_year,
     .                       numvals,
     .                       1,
     .                       366)
      if (numvals.eq.0) then
         call warning_error (ERR_User
     .                      ,'Please change input for starting day to '
     .                      //'simulation_start_day = ... as later '
     .                      //'versions of APSIM will not support the '
     .                      //'old syntax.')
         call read_integer_var ('parameters',
     .                       'start_day',
     .                       '(day)',
     .                       day_of_year,
     .                       numvals,
     .                       1,
     .                       366)

      endif

      call read_integer_var_optional ('parameters',
     .                       'simulation_start_year',
     .                       '(year)',
     .                       year,
     .                       numvals,
     .                       1700,
     .                       2100)
      if (numvals.eq.0) then
         call warning_error (ERR_User
     .                      ,'Please change input for starting year to '
     .                      //'simulation_start_year = ... as later '
     .                      //'versions of APSIM will not support the '
     .                      //'old syntax.')
         call read_integer_var ('parameters',
     .                       'start_year',
     .                       '(year)',
     .                       year,
     .                       numvals,
     .                       1700,
     .                       2100)

      endif

      call day_of_year_to_date (day_of_year, year, thisdate)
      g_Start_date = Date_to_jday 
     .    (thisdate(1), thisdate(2), thisdate(3))
 
      call read_integer_var_optional ('parameters',
     .                       'simulation_end_day',
     .                       '(day)',
     .                       day_of_year,
     .                       numvals,
     .                       1,
     .                       366)
      if (numvals.eq.0) then
         call warning_error (ERR_User
     .                      ,'Please change input for ending day to '
     .                      //'simulation_end_day = ... as later '
     .                      //'versions of APSIM will not support the '
     .                      //'old syntax.')
         call read_integer_var ('parameters',
     .                       'end_day',
     .                       '(day)',
     .                       day_of_year,
     .                       numvals,
     .                       1,
     .                       366)

      endif

      call read_integer_var_optional ('parameters',
     .                       'simulation_end_year',
     .                       '(year)',
     .                       year,
     .                       numvals,
     .                       1700,
     .                       2100)
      if (numvals.eq.0) then
         call warning_error (ERR_User
     .                      ,'Please change input for ending year to '
     .                      //'simulation_end_year = ... as later '
     .                      //'versions of APSIM will not support the '
     .                      //'old syntax.')
         call read_integer_var ('parameters',
     .                       'end_year',
     .                       '(year)',
     .                       year,
     .                       numvals,
     .                       1700,
     .                       2100)

      endif


      call day_of_year_to_date (day_of_year, year, thisdate)
      g_end_date = Date_to_jday (thisdate(1), thisdate(2), thisdate(3))

      call read_integer_var_optional ('parameters',
     .                       'timestep',
     .                       '(min)',
     .                       g_timestep,
     .                       numvals,
     .                       1,
     .                       mins_in_day)
 
      if (numvals.eq.0) then
         g_timestep = mins_in_day
      else
      endif

      if (mod(mins_in_day,g_timestep).ne.0) then
         call fatal_error (Err_User, 
     :       'Timestep must be factor of 1440 minutes (one day)')
      else
      endif

      call pop_routine (this_routine)
      return
      end



* ====================================================================
      logical function clock_advance_clock ()
* ====================================================================
      implicit none
      include 'const.inc'              ! Constant definitions
      include 'clock.inc'              ! clock common block
      include 'date.pub'                          
      include 'write.pub'                         
      include 'error.pub'                         

*+  Purpose
*     advance the simulation to the next timestep.  REturn .true. if
*     simulation should continue.  .false. if simulation should end.

*+  Changes
*     dph 25/11/96
*     nih 28/04/99 - added simple sub-daily timestep function

*+  Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='clock_next_timestep')

*- Implementation Section ----------------------------------
 
      call push_routine (This_routine)
      g_current_time = g_current_time + g_timestep

      g_current_date = g_start_date
     :               + int(g_current_time/dble(mins_in_day))

      ! check for end of run conditions.

      if (g_current_date .eq. g_end_date + 1) then
         call Write_string (lu_scr_sum,
     .       'Simulation is terminating due to end ' //
     .       'criteria being met.')
         Clock_advance_clock = .false.
      else
         ! convert julian day to day and year for speed reasons later.
 
         call jday_to_day_of_year (g_current_date,
     .                             g_day,
     .                             g_year)
 
         Clock_advance_clock = .true.

         call Clock_DoTick()

      endif
 
      call pop_routine (This_routine)
      return
      end



* ====================================================================
       subroutine clock_send_my_variable (Variable_name)
* ====================================================================
      implicit none
       include 'const.inc'             ! constant definitions
       include 'clock.inc'             ! Common block
      include 'engine.pub'                        
      include 'date.pub'                          
      include 'intrface.pub'                      
      include 'error.pub'                         

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
     .                                 g_day)
      else if (variable_name .eq. 'year') then
         call respond2get_integer_var (Variable_name,
     .                                 '(year)',
     .                                 g_year)
      else if (variable_name .eq. 'timestep') then
         call respond2get_integer_var (Variable_name,
     .                                 '(min)',
     .                                 g_timestep)
      else if (variable_name .eq. 'day_of_month') then
         call jday_to_date (thisdate(1), thisdate(2), thisdate(3),
     .                      g_current_date)
         call respond2get_integer_var (Variable_name,
     .                                 '(day)',
     .                                 thisdate(1))
      else if (variable_name .eq. 'month') then
         call jday_to_date (thisdate(1), thisdate(2), thisdate(3),
     .                      g_current_date)
         call respond2get_integer_var (Variable_name,
     .                                 '(month)',
     .                                 thisdate(2))
 
      else if (Variable_name .eq. 'start_week') then
         Logical_to_return = Start_week (g_day, g_year)
         call Respond2get_logical_var
     .       (variable_name, '(0-1)', Logical_to_return)
 
      else if (Variable_name .eq. 'end_week') then
         Logical_to_return = End_week (g_day, g_year)
         call Respond2get_logical_var
     .       (variable_name, '(0-1)', Logical_to_return)
  
      else if (Variable_name .eq. 'start_month') then
         Logical_to_return = Start_month (g_day, g_year)
         call Respond2get_logical_var
     .       (variable_name, '(0-1)', Logical_to_return)
 
      else if (Variable_name .eq. 'end_month') then
         Logical_to_return = End_month (g_day, g_year)
         call Respond2get_logical_var
     .       (variable_name, '(0-1)', Logical_to_return)
 
      else if (Variable_name .eq. 'end_year') then
         Logical_to_return = End_year (g_day, g_year)
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
         call jday_to_day_of_year (g_Start_date
     .                            ,doy
     .                            ,year)
         call respond2get_integer_var (Variable_name,
     .                                 '()',
     .                                 doy)

      else if (variable_name .eq. 'simulation_start_year') then
         call jday_to_day_of_year (g_Start_date
     .                            ,doy
     .                            ,year)
         call respond2get_integer_var (Variable_name,
     .                                 '()',
     .                                 year)

      else if (variable_name .eq. 'simulation_end_day') then
         call jday_to_day_of_year (g_End_date
     .                            ,doy
     .                            ,year)
         call respond2get_integer_var (Variable_name,
     .                                 '()',
     .                                 doy)

      else if (variable_name .eq. 'simulation_end_year') then
         call jday_to_day_of_year (g_End_date
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
      end



* ====================================================================
       subroutine clock_today_object (Variable_name)
* ====================================================================
      implicit none
      include 'const.inc'              ! constant definitions
      include 'clock.inc'              ! clock common block
      include 'intrface.pub'                      
      include 'date.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) name of variable

*+  Purpose
*     Get the value of a variable or constant.

*+  Changes
*        DPH - 11/4/96

*+  Calls
      character get_month_string*(3)
                                       ! function

*+  Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='clock_today_object')

*+  Local Variables
      integer thisdate(3)                  ! day, month, year of todays date
      character str*100                ! string for date formatting

*- Implementation Section ----------------------------------
 
      call push_routine (This_routine)
 
      call day_of_year_to_date (g_day, g_year, thisdate)
 
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
     .        ('today.day_of_year', '()', g_day)
 
      else if (variable_name .eq. '.month_str') then
         call Respond2get_char_var
     .        ('today.month_str', '()', Get_month_string(thisdate(2)))
 
      else if (variable_name .eq. '.dd/mm') then
         write (str, '(i2,a,i2)')
     .        thisdate(1), '/', thisdate(2)
         if (str(4:4) .eq. Blank) then
            str(4:4) = '0'
         endif
         call Respond2get_char_var
     .        ('today.dd/mm', '()', str)
 
      else if (variable_name .eq. '.dd/mm/yyyy') then
         write (str, '(i2,a,i2,a,i4)')
     .        thisdate(1), '/', thisdate(2), '/', thisdate(3)
         if (str(4:4) .eq. Blank) then
            str(4:4) = '0'
         endif
         call Respond2get_char_var
     .        ('today.dd/mm/yyyy', '()', str)

      else if (variable_name .eq. '.dd_mmm_yyyy') then
         write (str, '(i2,a,a,a,i4)')
     .        thisdate(1), '_', Get_month_string(thisdate(2)),
     .        '_', thisdate(3)
         call Respond2get_char_var
     .        ('today.dd_mmm_yyyy', '()', str)

      else if (variable_name .eq. '.dd/mmm/yyyy') then
         write (str, '(i2,a,a,a,i4)')
     .        thisdate(1), '/', Get_month_string(thisdate(2)),
     .        '/', thisdate(3)
         call Respond2get_char_var
     .        ('today.dd/mmm/yyyy', '()', str)
 
      else if (variable_name .eq. '.dd_mmm') then
         write (str, '(i2,a,a)')
     .        thisdate(1), '_', Get_month_string(thisdate(2))
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
      end



* ====================================================================
       subroutine clock_start ()
* ====================================================================
      implicit none
      include 'const.inc'              ! constant definitions
      include 'clock.inc'              ! clock common block
      include 'write.pub'                         
      include 'engine.pub'                        
      include 'error.pub'                         

*+  Purpose
*     start the clock going. ie. start the simulation

*+  Changes
*        DPH - 26/11/96

*+  Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='clock_start')

*+  Local Variables
       character Line*200              ! line output to summary file and screen.

*- Implementation Section ----------------------------------
 
      call push_routine (This_routine)
 
      ! send initialisation message to all modules.

      call Message_send_immediate (All_active_modules, MES_Init, Blank)
 
      ! write header line to summary file.
 
      write (Line, '(30a)')
     . New_line //
     . '======= Start of APSIM simulation ============================'
     . // New_line
      call Write_string (LU_Scr_sum, Line)
 
      ! do all timesteps for simulation
 
      call Clock_timestep_loop ()
 
      ! send end run message to all modules.
 
      call Message_send_immediate
     .       (All_active_modules, MES_End_run, 'End criteria met')
 
      ! write footer line to summary file.
 
      write (Line, '(30a)')
     . New_line //
     . '======= End of APSIM simulation =============================='
     . // New_line
      call Write_string (LU_Scr_sum, Line)
 
      call pop_routine (This_routine)
      return
      end



* ====================================================================
       subroutine Clock_timestep_loop ()
* ====================================================================
      implicit none
      include 'const.inc'             ! Constant definitions
      include 'event.inc'
      include 'clock.inc'
      include 'error.pub'                         
      include 'engine.pub'                        

*+  Purpose
*     Cycle through all phases for an entire simulation.  Exit routine
*     when simulation has completed.

*+  Changes
*      DPH 26/11/96
*      NIH 25/08/99 - Added Tick Event

*+  Calls
                                       ! function
       logical clock_advance_clock     ! function

*+  Constant Values
       integer Num_instructions
       parameter (Num_instructions=4)  ! Number of instructions to send

*+  Local Variables
       character Instructions(Num_instructions)*8
       integer Instruction_Index       ! index into instruction list

*+  Initial Data Values
       data Instructions(1) /MES_Prepare/
       data Instructions(2) /MES_Process/
       data Instructions(3) /MES_Post/
       data Instructions(4) /MES_Report/

*- Implementation Section ----------------------------------
 
      ! Main timestep loop
 
10    continue

      do 20 Instruction_index = 1, Num_instructions
 
         ! Send message to all modules.
 
         call Message_send_immediate (All_active_modules,
     .                                Instructions(Instruction_Index),
     .                                Blank)
 
         ! check the fatal error flag.
 
         g_End_current_run = (g_End_current_run .or.
     .                        fatal_error_flagged_found())
 
         ! Check the end of simulation flag and exit if necessary
 
         if (g_End_current_run) then
            goto 100
         endif
 
         ! Check the pause flag and enter a idle loop if necessary.
 
         if (g_Pause_current_run) then
            call clock_idle_loop ()
         endif
 
20    continue
 
      ! loop back to next timestep if necessary.
 
      if (clock_advance_clock ()) then
         goto 10
      endif
 
      ! thats it - exit routine and simulation.
100   continue
 
      return
      end



* ====================================================================
       subroutine Clock_idle_loop ()
* ====================================================================
      implicit none
       include 'const.inc'             ! Constant definitions
       include 'clock.inc'
      include 'engine.pub'                        

*+  Purpose
*     Enter a idle loop where only idle messages are sent to all modules.
*     This routine is called whenever the simulation goes into pause
*     mode.

*+  Changes
*      DPH 26/11/96

*- Implementation Section ----------------------------------
 
10    continue
      if (g_Pause_current_run) then
         call Message_send_immediate (All_active_modules,
     .                                MES_Idle,
     .                                Blank)
         goto 10
      endif
 
      return
      end


* ====================================================================
       character*(*) function Clock_time_string ()
* ====================================================================
      implicit none
       include 'const.inc'             ! Constant definitions
       include 'clock.inc'
      include 'engine.pub' 
      include 'string.pub'                       

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

      time_mins = mod(int(g_current_time), mins_in_day)

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
      end


* ====================================================================
       subroutine Clock_DoTick ()
* ====================================================================
      implicit none
      include 'const.inc'             ! Constant definitions
      include 'event.inc'
      include 'clock.inc'
      include 'engine.pub'                        
      include 'intrface.pub'

*+  Calls
      character Clock_time_string*(5)      ! function

*+  Purpose
*     Notify all modules of a clock tick and the new timestep

*+  Changes
*      NIH 25/08/99

*+  Local Variables
      character time*(5)               ! time in 24 hour format

*- Implementation Section ----------------------------------
 
      call new_postbox()

      call post_integer_var (DATA_day
     :                     , '()'
     :                     , g_day)

      call post_integer_var (DATA_year
     :                     , '()'
     :                     , g_year)

      time = clock_time_string()
      call post_char_var (DATA_time
     :                   , '(hh:mm)'
     :                   , time)

      call post_integer_var (DATA_timestep
     :                     , '(min)'
     :                     , g_timestep)

      call event_send (EVENT_tick)

      call delete_postbox() 


 
      return
      end