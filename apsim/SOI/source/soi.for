      module SOIModule
! ========================================================================
!     SOI constants
! ========================================================================

!   Short description:
!      General variables used in SOI model

!   Assumptions:
!      None

!   Notes:

!   Changes:

! ====================Declaration section=================================

!   Global variables
      integer   SOI_min                          ! Start year of SOI array
      parameter (SOI_min = 1800)
      integer   SOI_max                          ! End year of SOI array
      parameter (SOI_max = 2100)

      type SOIGlobals
         sequence
         integer   LU_SOI                        ! Unit number for SOI file
         integer   SOI_phase
         integer   SOI_array(SOI_min:SOI_max,12) ! SOI phases array
         double precision SOI_jday               ! System jday
      end type SOIGlobals

! ========================================================================

      ! instance variables.
      common /InstancePointers/ ID,g,p,c
      save InstancePointers
      type (SOIGlobals),pointer :: g


      contains


*     ===========================================================
      subroutine SOI_init ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*       Initialise module - called once only at beginning of run

*+  Changes
*     dph 7/5/99 removed presence report

*+  Calls

*+  Constant Values
      character this_routine*(*)       ! name of this routine
      parameter (this_routine='SOI_init')
*+  Calls

*+  Local Variables
      character Table_name*100         ! name of table to open
      logical ok

*- Implementation Section ----------------------------------

      call push_routine (this_routine)

      ! Zero variables
      call SOI_zero_variables()

      ! create an external table object and open it
      g%LU_SOI = component_gettable(componentData, 'soi', ' ')

      if (g%LU_SOI .ne. 0) then
         ! Read in all parameters from parameter file
         call SOI_read_phases ()
      else
         call Fatal_error (ERR_User, 'Cannot find soi file')
      endif
      call component_freetable(g%LU_SOI)

      call pop_routine (this_routine)
      return
      end subroutine



*     ===========================================================
      subroutine SOI_zero_variables ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*       Zero all variables.  Called at initialisation

*+  Changes
*     <insert here>

*+  Constant Values
      character this_routine*(*)       ! name of this routine
      parameter (this_routine='SOI_zero_variables')

*+  Local Variables
        integer x                       ! do loop counter
        integer y                       ! do loop counter

*- Implementation Section ----------------------------------

      call push_routine (this_routine)

      ! zero SOI array

      do x = SOI_min, SOI_max
        do y = 1, 12
            g%SOI_array(x,y) = 0
        enddo
      enddo

      g%SOI_phase = 0
      g%LU_SOI = 0


      call pop_routine (this_routine)

      return
      end subroutine



* ====================================================================
      subroutine SOI_read_phases ()
* ====================================================================
      Use infrastructure
      implicit none

*+  Purpose
*      Read in all phases from SOI file.

*+  Changes
*     <insert here>

*+  Constant Values
      character this_routine*(*)       ! name of this routine
      parameter (this_routine='SOI_read_phases')

*+  Local Variables
      integer Year,Month,Phase         ! Values found on line.
      character st*(100)               ! Line read from climate file
      logical ok                       ! all ok?
      integer numvals

*- Implementation Section ----------------------------------

      call push_routine (this_routine)

      ! Loop through the SOI file, reding the phases into an array

10    continue
      ok = ExternalTable_GetValueByIndex (g%LU_SOI, 0, St)
      if (ok) then
         call String_to_integer_var(St, Year, numvals)
         ok = ExternalTable_GetValueByIndex (g%LU_SOI, 1, St)
      endif
      if (ok) then
         call String_to_integer_var(St, Month, numvals)
         ok = ExternalTable_GetValueByIndex (g%LU_SOI, 2, St)
      endif
      if (ok) then
         call String_to_integer_var(St, Phase, numvals)
      endif
      if (ok) then
         ! check if input values are ok

         call bound_check_integer_var(Year,
     .                                SOI_min,
     .                                SOI_max,
     .                                'SOI_inp_year')

         call bound_check_integer_var(Month,
     .                                1,
     .                                12,
     .                                'SOI_inp_month')

         g%SOI_array(Year, Month) = Phase
         call ExternalTable_Next(g%LU_SOI)
         if (.not. ExternalTable_eof(g%LU_SOI)) then
            goto 10
         endif
      endif

      call pop_routine (this_routine)
      return
      end subroutine



*     ================================================================
      recursive subroutine SOI_send_my_variable (variable_name)
*     ================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      character variable_name*(*)      ! (input) variable name to search for

*+  Purpose
*      return the value of a variable to caller through the postbox.

*+  Changes
*     <insert here>

*+  Constant Values
      character this_routine*(*)       ! name of this routine
      parameter (this_routine='SOI_send_my_variable')

*- Implementation Section ----------------------------------

      call push_routine (this_routine)

      if (variable_name(1:4) .eq. 'soi[') then

         call SOI_get_phase (variable_name)

         call respond2get_integer_var(variable_name, ! external name
     .                             '()',       ! units of var.
     .                             g%SOI_phase)     ! internal var. name

      else
         call Message_unused ()

      endif

      call pop_routine (this_routine)

      return
      end subroutine



*     ================================================================
      recursive subroutine SOI_get_phase (variable_name)
*     ================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      character variable_name*(*)      ! (input) variable name to search for

*+  Purpose
*      return the SOI phase for the month.

*+  Changes
*     <insert here>

*+  Constant Values
      character this_routine*(*)       ! name of this routine
      parameter (this_routine='SOI_get_phase')

*+  Local Variables
      integer   numvals                ! number of values found on line.
      integer   SOI_day                ! System day
      integer   SOI_month              ! System month
      integer   SOI_year               ! System year
      integer   SOI_units              ! SOI lag or month
*
      integer   SOI_get_units          ! function

*- Implementation Section ----------------------------------

      call push_routine (this_routine)

      ! get date from system

      call get_double_var (Unknown_module,
     .                      'today',
     .                      '()',
     .                      g%SOI_jday,
     .                      numvals,
     .                      0.0d0,
     .                      3660000000.0d0)


      call jday_to_date (SOI_Day, SOI_Month, SOI_Year, g%SOI_jday)

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
          SOI_Month = SOI_Month + SOI_units
          if (SOI_Month .lt. 1) then
             SOI_year = SOI_year - 1
             SOI_Month = SOI_Month + 12
          endif
      else
          SOI_Month = SOI_units
      endif

      ! get the SOI phase for the given month

      g%SOI_phase = g%SOI_array(SOI_year, SOI_Month)
      ! print *, SOI_units,SOI_Month,SOI_year,g%SOI_phase

      call pop_routine (this_routine)

      return
      end subroutine



* =============================================================
      integer function SOI_get_units (record)
* =============================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      character  record*(*)            ! (INPUT) record string of array

*+  Purpose
*     Get units from variable string

*+  Changes
*     <insert here>

*+  Constant Values
      character  this_routine*(*)            ! Name of subroutine
      parameter (this_routine = 'SOI_get_units')
*
      character  unit_start*(*)        ! delimiter to start units
      parameter (unit_start = '[')
*
      character  unit_end*(*)          ! delimiter to end units
      parameter (unit_end = ']')

*+  Local Variables
      integer    SOI_error             ! string to int error code
      integer    SOI_day               ! SOI day
      integer    SOI_year
      double precision SOI_jday
      character  remainder*100         ! rest of string
      character  unit_plus*100         ! unit + rest string
      character  units*10              ! units string
      character  values*100            ! values string

*- Implementation Section ----------------------------------

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

            SOI_jday = date(units, g%SOI_jday)
            call jday_to_date(SOI_day,SOI_get_units,SOI_year,SOI_jday)
         endif
      endif

      call pop_routine (this_routine)
      return
      end subroutine



      end module SOIModule

!     ===========================================================
      subroutine alloc_dealloc_instance(doAllocate)
!     ===========================================================
      use SOIModule
      implicit none  
      ml_external alloc_dealloc_instance

!+  Sub-Program Arguments
      logical, intent(in) :: doAllocate

!+  Purpose
!      Module instantiation routine.

!- Implementation Section ----------------------------------

      if (doAllocate) then
         allocate(g)
      else
         deallocate(g)
      end if
      return
      end subroutine



* ====================================================================
      recursive subroutine Main (action, data)
* ====================================================================
      Use infrastructure
      implicit none
      ml_external Main

*+  Sub-Program Arguments
      character  action*(*)            ! (INPUT) Message action to perform
      character  data*(*)              ! (INPUT) Message data

*+  Purpose
*     <insert here>

*+  Changes
*     dph 7/5/99 removed version and presence report c186

*+  Calls

*+  Constant Values

*- Implementation Section ----------------------------------

      if (action .eq. ACTION_init) then

         ! initialise variables for run (called once only)

         call SOI_init ()

      else if (action .eq. ACTION_get_variable) then

         ! return one of our variables to calling module

      call SOI_send_my_variable (data)

      else
         ! don't use message

         call Message_unused ()
      endif

      return
      end subroutine


