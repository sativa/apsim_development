* ====================================================================
      subroutine read_params ()
* ====================================================================
      use ClockModule
      implicit none
      include 'const.inc'              ! Constant definitions
      include 'date.pub'                          
      include 'read.pub'                          
      include 'error.pub'                         

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

*- Implementation Section ----------------------------------

      call push_routine (this_routine)

      ! go get a start date
      call read_char_var ('parameters',
     .                    'start_date',
     .                    '(date)',
     .                    date_st,
     .                    numvals)
      call String_to_jday (date_st, g%start_date, numvals, 0.0)

      if (numvals.eq.0) then
         call fatal_error (ERR_User
     .                    ,'Cannot convert the date:'
     .                    // TRIM(date_st)
     .                    //' to a valid date (dd/mm/yyyy)')
      endif

      ! go get an end date
      call read_char_var ('parameters',
     .                    'end_date',
     .                    '(date)',
     .                    date_st,
     .                    numvals)
      call String_to_jday (date_st, g%end_date, numvals, 0.0)

      if (numvals.eq.0) then
         call fatal_error (ERR_User
     .                    ,'Cannot convert the date:'
     .                    // TRIM(date_st)
     .                    //' to a valid date (dd/mm/yyyy)')
      endif

      call read_integer_var_optional ('parameters',
     .                       'timestep',
     .                       '(min)',
     .                       g%timestep,
     .                       numvals,
     .                       1,
     .                       mins_in_day)

      if (numvals.eq.0) then
         g%timestep = mins_in_day
      else
      endif

      if (mod(mins_in_day,g%timestep).ne.0) then
         call fatal_error (Err_User, 
     :       'Timestep must be factor of 1440 minutes (one day)')
      else
      endif

      call pop_routine (this_routine)
      return
      end subroutine
