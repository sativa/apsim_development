*     ===========================================================
      character*(*) function SOI_version ()
*     ===========================================================


*   Short description:
*       return version number of SOI module

*   Assumptions:
*       none

*   Notes:
*       none

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:

*   Calls:
*       none

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
*       none

*   Internal variables
*       none

*   Constant values
      character  version_number*(*)    ! version number of module
      parameter (version_number = 'V1.00 01/09/97')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      SOI_version = version_number

      return
      end

* ====================================================================
      subroutine APSIM_SOI (action, data)
* ====================================================================

*   short description:

*   assumptions:
*      none

*   notes:
*      none

*   procedure attributes:
*      version:         any hardware/fortran77
*      extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       implicit none

*   changes:

*   calls:

* ----------------------- declaration section ------------------------

      implicit none

*   subroutine arguments
      character  action*(*)            ! (INPUT) Message action to perform
      character  data*(*)              ! (INPUT) Message data

*   global variables
      include 'const.inc'              ! Constant definitions

      character SOI_version*20        ! function

*   internal variables

*   constant values
      character module_name*(*)        ! name of this module
      parameter (module_name = 'SOI')

*   initial data values
*     none

* --------------------- executable code section ----------------------

      if (action .eq. mes_presence) then
         write(*, *) 'Module_name = ', Module_name 
     :              // ', Version : ' // SOI_version()

      else if (action .eq. mes_init) then

         ! initialise variables for run (called once only)

         call SOI_init ()
         
      else if (action .eq. mes_get_variable) then

         ! return one of our variables to calling module

      call SOI_send_my_variable (data)

      else
         ! don't use message

         call Message_unused ()
      endif

      return
      end

*     ===========================================================
      subroutine SOI_init ()
*     ===========================================================

*   short description:
*       Initialise module - called once only at beginning of run

*   assumptions:
*       none

*   notes:
*       none

*   procedure attributes:
*      version:         any hardware/fortran77
*      extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       implicit none

*   changes:

*   calls:

* ----------------------- declaration section ------------------------

      implicit none

*   subroutine arguments
*       none

*   global variables
      include 'const.inc'              ! Constant definitions
      include 'soi.inc'                ! Constant definitions
      integer open_param_file          ! function
      character SOI_version*20         ! function

*   Internal variables
      character  string*300            ! output string

*   constant values
      character this_routine*(*)       ! name of this routine
      parameter (this_routine='SOI_init')

*   initial data values
*     none

* --------------------- executable code section ----------------------

      call push_routine (this_routine)

      String = ' - Initialised ,Version : ' // SOI_version ()
      call report_event (String)


      ! Zero variables
      call SOI_zero_variables()

      ! Open SOI file for input
      LU_SOI = open_param_file ('soi')

      ! Read in all parameters from parameter file
      call SOI_read_phases ()
      
      ! Close SOI file
      call close_unit (LU_SOI)

      call pop_routine (this_routine)
      return
      end

*     ===========================================================
      subroutine SOI_zero_variables ()
*     ===========================================================

*   short description:
*       Zero all variables.  Called at initialisation

*   assumptions:
*       none

*   notes:
*       none

*   procedure attributes:
*      version:         any hardware/fortran77
*      extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       implicit none

*   changes:

*   calls:

* ----------------------- declaration section ------------------------

      implicit none

*   subroutine arguments
*       none

*   global variables
      include 'soi.inc'                ! Constant definitions

*   internal variables
        integer x                       ! do loop counter
        integer y                       ! do loop counter

*   constant values
      character this_routine*(*)       ! name of this routine
      parameter (this_routine='SOI_zero_variables')


*   initial data values
*       none

* --------------------- executable code section ----------------------

      call push_routine (this_routine)

      ! zero SOI array

      do x = SOI_min, SOI_max
        do y = 1, 12
            SOI_array(x,y) = 0
        enddo
      enddo

      SOI_phase = 0
      LU_SOI = 0


      call pop_routine (this_routine)

      return
      end

* ====================================================================
      subroutine SOI_read_phases ()
* ====================================================================

*   Short description:
*      Read in all phases from SOI file.

*   Assumptions:
*      None

*   Notes:
*       none

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:

*   Calls:

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
*      none

*   Global variables
      include 'const.inc'              ! Constant definitions
      include 'soi.inc'                ! Constant definitions

*   Internal variables
      integer Values(3)                ! Values found on line.
      integer Num_values_found         ! number of values found on line.   
      integer IOStatus                 ! i/o status
      character Line*(300)             ! Line read from climate file
      integer Record_num               ! Record number in file
      integer record_ok                ! bounds checker
      integer warning_error_found      ! function

*   Constant values
      character this_routine*(*)       ! name of this routine
      parameter (this_routine='SOI_read_phases')


*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      call push_routine (this_routine)

      
      ! Loop through the SOI file, reding the phases into an array
      Record_num = 0

10    continue

      ! Read line from SOI file
      call Read_line (LU_SOI, 
     .                Record_num, 
     .                Line, 
     .                IOStatus)


      if (IOStatus .eq. 0) then

         call String_to_integer_array (Line, 
     .                              Values, 
     .                              3, 
     .                              Num_values_found)

         ! did we find the expected number of values?
      
         if (Num_values_found .ne. 3) then
            ! no - issue error.
         
            call Fatal_error(ERR_Internal, 
     .         'The SOI file has too many columns of data in it.')

         else
            ! check if input values are ok

            record_ok = 1
            call bound_check_integer_var(Values(1),
     .                                   SOI_min,
     .                                   SOI_max,
     .                                   'SOI_inp_year')

            call bound_check_integer_var(Values(2),
     .                                  1,
     .                                  12,
     .                                  'SOI_inp_month')

            SOI_array(Values(1),Values(2)) = Values(3)

            ! get the next record
            goto 10
         endif
      else
         ! end of file

      endif

      

      call pop_routine (this_routine)
      return
      end

*     ================================================================
      subroutine SOI_send_my_variable (variable_name)
*     ================================================================

*   short description:
*      return the value of a variable to caller through the postbox.

*   assumptions:
*      none

*   notes:

*   procedure attributes:
*      version:         any hardware/fortran77
*      extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       implicit none

*   changes:

*   calls:

* ----------------------- declaration section ------------------------

      implicit none

*   subroutine arguments
      character variable_name*(*)      ! (input) variable name to search for

*   global variables
      include 'const.inc'              ! constant definitions
      include 'soi.inc'                ! Constant definitions

*   internal variables

*   constant values
      character this_routine*(*)       ! name of this routine
      parameter (this_routine='SOI_send_my_variable')


*   initial data values
*      none

* --------------------- executable code section ----------------------

      call push_routine (this_routine)

      if (variable_name(1:4) .eq. 'soi[') then

         call SOI_get_phase (variable_name)

         call respond2get_integer_var(variable_name, ! external name
     .                             '()',       ! units of var.
     .                             SOI_phase)     ! internal var. name


      else
         call Message_unused ()

      endif

      call pop_routine (this_routine)

      return
      end

*     ================================================================
      subroutine SOI_get_phase (variable_name)
*     ================================================================             

*   short description:
*      return the SOI phase for the month.

*   assumptions:
*      none

*   notes:

*   procedure attributes:
*      version:         any hardware/fortran77
*      extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       implicit none

*   changes:

*   calls:

* ----------------------- declaration section ------------------------

      implicit none

*   subroutine arguments
      character variable_name*(*)      ! (input) variable name to search for

*   global variables
      include 'const.inc'              ! constant definitions
      include 'soi.inc'                ! Constant definitions

*   internal variables
      integer   numvals                ! number of values found on line.   
      integer   SOI_jday               ! System jday
      integer   SOI_year               ! System year
      integer   SOI_date(3)            ! Date format ddmmyy
      integer   SOI_units              ! SOI lag or month

      integer   SOI_get_units          ! function

*   constant values
      character this_routine*(*)       ! name of this routine
      parameter (this_routine='SOI_get_phase')


*   initial data values
*      none

* --------------------- executable code section ----------------------

      call push_routine (this_routine)

      ! get date from system

      call get_integer_var (Unknown_module,
     .                      'day',
     .                      '(days)',
     .                      SOI_jday,
     .                      numvals,
     .                      1,
     .                      366)


      call get_integer_var (Unknown_module,
     .                      'year',
     .                      '(year)',
     .                      SOI_year,
     .                      numvals,
     .                      SOI_min,
     .                      SOI_max)



      ! get the month from the jday

      call day_of_year_to_date (SOI_jday, SOI_year, SOI_date)

      ! get the month or lag from the variable name

      SOI_units = SOI_get_units (variable_name)

      ! Check for a valid month. We only do 1 year ahead or behind

      if (SOI_units .gt. 12) then
         SOI_units = 12
      endif
        
      if (SOI_units .lt. -12) then
         SOI_units = -12
      endif

      ! If the Units are less than one then treat it as a lag

      if (SOI_units .lt. 1) then
          SOI_date(2) = SOI_date(2) + SOI_units
          if (SOI_date(2) .lt. 1) then
             SOI_year = SOI_year - 1
             SOI_date(2) = SOI_date(2) + 12
          endif
      else
          SOI_date(2) = SOI_units
      endif

      ! get the SOI phase for the given month

      SOI_phase = SOI_array(SOI_year, SOI_date(2))
      ! print *, SOI_units,SOI_date(2),SOI_year,SOI_phase

      call pop_routine (this_routine)

      return
      end


* =============================================================
      integer function SOI_get_units (record)
* =============================================================

*   Short description:
*     Get units from variable string

*   Assumptions:
*      None

*   Notes:

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*   Calls:
*       assign_string
*       pop_routine
*       push_routine
*       Split_line

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      character  record*(*)            ! (INPUT) record string of array

*   Global variables
      include   'const.inc'            ! err_user

*   Internal variables
      integer    numvals               ! number of values found on line.   
      integer    SOI_error             ! string to int error code
      integer    SOI_day               ! SOI day
      integer    SOI_month
      integer    SOI_year
      integer    SOI_jday
      character  remainder*100         ! rest of string
      character  unit_plus*100         ! unit + rest string
      character  units*10              ! units string
      character  values*100            ! values string
      
      integer    date                  ! function
      
*   Constant values
      character  blank_string*(*)
      parameter (blank_string = ' ')

      character  this_routine*(*)            ! Name of subroutine
      parameter (this_routine = 'SOI_get_units')

      character  unit_start*(*)        ! delimiter to start units
      parameter (unit_start = '[')

      character  unit_end*(*)          ! delimiter to end units
      parameter (unit_end = ']')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      call push_routine (this_routine)

      ! Get the string between the square brackets
      
      call split_line (record, values, unit_plus, unit_start)
      call split_line (unit_plus, units, remainder, unit_end)

      SOI_get_units = 0

      if (units .ne. ' ') then

         ! Test to see if the string is a date in string format or not
         
         read (units,*,iostat = SOI_error) SOI_get_units
         if (SOI_error .ne. 0) then

            ! Convert the date sring to a month number

            SOI_jday = date(units)
            call jday_to_date(SOI_day,SOI_get_units,SOI_year,SOI_jday)
         endif
      endif

      call pop_routine (this_routine)
      return
      end
