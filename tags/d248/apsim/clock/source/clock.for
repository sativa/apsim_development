*     ===========================================================
      character*(*) function clock_version ()
*     ===========================================================
      implicit none

*+  Purpose
*       return version number of clock module

*+  Changes
*       DPH - 25/2/94

*+  Constant Values
      character  version_number*(*)    ! version number of module
      parameter (version_number = 'V1.0 25/11/96')

*- Implementation Section ----------------------------------
 
      clock_version = version_number
 
      return
      end



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

*+  Calls
      character  clock_version*20      ! function

*- Implementation Section ----------------------------------

      if (Action .eq. MES_Presence) then
         call Write_string (LU_Scr_sum,
     .       'Module = clock ' // clock_version())
 
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
 
      else if (Action.eq.MES_Get_variable) then
         call clock_send_my_variable (Data)
 
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
      character  clock_version*20      ! function
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
 
!      msg = ' CLOCK Initialised, Version: ' // clock_version()
 
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
 
      call read_integer_var ('parameters',
     .                       'start_day',
     .                       '(day)',
     .                       day_of_year,
     .                       numvals,
     .                       1,
     .                       366)
      call read_integer_var ('parameters',
     .                       'start_year',
     .                       '(year)',
     .                       year,
     .                       numvals,
     .                       1700,
     .                       2100)
      call day_of_year_to_date (day_of_year, year, thisdate)
      g_Start_date = Date_to_jday 
     .    (thisdate(1), thisdate(2), thisdate(3))
 
      call read_integer_var ('parameters',
     .                       'end_day',
     .                       '(day)',
     .                       day_of_year,
     .                       numvals,
     .                       1,
     .                       366)
      call read_integer_var ('parameters',
     .                       'end_year',
     .                       '(year)',
     .                       year,
     .                       numvals,
     .                       1700,
     .                       2100)
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

*+  Purpose
*      Return the value of a variable

*+  Changes
*     dph 25/11/96
*     nih 28/04/99 - added timestep output

*+  Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='clock_send_my_variable')

*+  Local Variables
      integer thisdate(3)              ! day, month, year
      logical logical_to_return        ! logical value to return to calling module

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
       include 'clock.inc'
      include 'error.pub'                         
      include 'engine.pub'                        

*+  Purpose
*     Cycle through all phases for an entire simulation.  Exit routine
*     when simulation has completed.

*+  Changes
*      DPH 26/11/96

*+  Calls
                                       ! function
       logical clock_advance_clock     ! function

*+  Constant Values
       integer Num_instructions
       parameter (Num_instructions=5)  ! Number of instructions to send

*+  Local Variables
       character Instructions(Num_instructions)*8
       integer Instruction_Index       ! index into instruction list

*+  Initial Data Values
       data Instructions(1) /MES_Inter_timestep/
       data Instructions(2) /MES_Prepare/
       data Instructions(3) /MES_Process/
       data Instructions(4) /MES_Post/
       data Instructions(5) /MES_Report/

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



