      module Tamet2Module
      use Registrations

!     ================================================================
!      tamet2 constants
!     ================================================================

      integer offset
      parameter (offset = 3)

!     ================================================================
      type Tamet2Globals
      sequence

      character string*80
      character type*10
      character field*10


      logical   wind_found
      integer   a_day(offset)
      integer   a_year(offset)
      real   a_rain(offset)
      real   a_min(offset)
      real   a_max(offset)
      real   a_evap(offset)
      real   a_radn(offset)
      real   a_wind(offset)
      integer   day
      integer   year
      integer   error
      real   rain
      real   evap
      real   maxt
      real   mint
      real   wind
      real   radn
      double precision   last_day
      real      latitude
      real   rd(31)
      integer nv
      real    radn_rcal
      real   dis_evap
      real   dis_maxt
      real   dis_mint
      real   dis_wind
      real   dis_radn

      end type Tamet2Globals
!     ================================================================
      type Tamet2Parameters
      sequence
      real     dis_evap
      real     dish_evap
      real     disvh_evap
      real     dis_maxt
      real     dis_maxt_dry
      real     dis_maxt_other
      real     dis_mint
      real     Maxt_to_minT
      real     rain_lb
      real     rain_ub
      real     gauge_capacity
      real     maxt_lb
      real     maxt_ub
      real     mint_lb
      real     mint_ub
      real     evap_lb
      real     evap_ub
      real     wind_lb
      real     wind_ub
      real     radn_lb
      real     radn_ub
      real     radn_low
      real     radn_vlow
      end type Tamet2Parameters
!     ================================================================
      type Tamet2Constants
         sequence
         integer       dummy
      end type Tamet2Constants
!     ================================================================

      ! instance variables.
      common /InstancePointers/ ID,g,p,c
      save InstancePointers
      type (Tamet2Globals),pointer :: g
      type (Tamet2Parameters),pointer :: p
      type (Tamet2Constants),pointer :: c
      type (IDsType), pointer :: id

      contains





*====================================================================
      subroutine tamet2_zero_variables ()
*====================================================================
      Use infrastructure
      implicit none

*+  Purpose
*     Set all variables in this module to zero.

*+  Mission statement
*     Zero variables

*   Changes:
*       210995 jngh programmed

*   Constant values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'tamet2_zero_variables')

*   Initial data values
*      none

*- Implementation Section ----------------------------------
      call push_routine (myname)

         !variables

      g%a_day   = 0
      g%a_year  = 0
      g%a_min   = 0.0
      g%a_max   = 0.0
      g%a_evap  = 0.0
      g%a_rain  = 0.0
      g%a_radn  = 0.0
      g%a_wind  = 0.0
      g%day   = 0
      g%year  = 0
      g%rain  = 0.0
      g%maxt  = 0.0
      g%mint  = 0.0
      g%evap  = 0.0
      g%wind  = 0.0
      g%radn  = 0.0
      g%dis_maxt  = 0.0
      g%dis_mint  = 0.0
      g%dis_evap  = 0.0
      g%dis_wind  = 0.0
      g%dis_radn  = 0.0
      g%string = blank
      g%rd(:)    = 0.0
      g%nv = 0
      g%radn_rcal = 0.0

      call tamet2_zero_daily_variables ()

      call pop_routine (myname)

      return
      end subroutine
*====================================================================
      subroutine tamet2_zero_daily_variables ()
*====================================================================

      Use infrastructure
      implicit none

*+  Purpose
*     Set all variables in this module to zero.

*+  Mission statement
*     Set all variables in this module to zero

*   Changes:
*       210995 jngh programmed


*   Internal variables
      integer   i

*   Constant values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'tamet2_zero_daily_variables')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      do i = 1, offset-1
         g%a_day(i) = g%a_day(i+1)
         g%a_year(i) = g%a_year(i+1)
         g%a_min(i) = g%a_min(i+1)
         g%a_max(i) = g%a_max(i+1)
         g%a_evap(i) = g%a_evap(i+1)
         g%a_rain(i) = g%a_rain(i+1)
         g%a_radn(i) = g%a_radn(i+1)
         g%a_wind(i) = g%a_wind(i+1)
      enddo

      g%a_day(offset) = 0
      g%a_year(offset) = 0
      g%a_min(offset) = 0
      g%a_max(offset) = 0
      g%a_evap(offset) = 0
      g%a_rain(offset) = 0
      g%a_radn(offset) = 0
      g%a_wind(offset) = 0

      g%type = blank
      g%field = blank

      call pop_routine (myname)

      return
      end subroutine
*====================================================================
      subroutine tamet2_init ()
*====================================================================

      Use infrastructure
      implicit none

*+  Purpose
*      Initialise tamet2 module

*+  Mission statement
*      Initialise tamet2 module

*   Changes:
*       210995 jngh programmed

*   Internal variables
      character  Event_string*40       ! String to output

*   Constant values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'tamet2_init')

*- Implementation Section ----------------------------------
      call push_routine (myname)

         ! notify system that we have initialised

      Event_string = 'Initialising Version : '
      call write_string (Event_string)

         ! get all parameters from parameter file

      call tamet2_read_param ()

         ! get all constants from constants file

      call tamet2_read_constants ()

      call write_string (' ERROR LIST')

       call pop_routine (myname)

      return
      end subroutine
*===========================================================
      subroutine tamet2_read_param ()
*===========================================================

      Use infrastructure
      implicit none

*+  Purpose
*      Read in all parameters from parameter file.

*+  Mission Statement
*     Read Parameters from par file

*   Changes:
*       210995 jngh programmed
*       020398 jngh added in reading and reporting for reference height,
*                   disp and Z0 at instrument site.
*                   changed z0soil to mm


*   Internal variables
      integer    numvals               ! number of values read
      integer    ds
      integer    idate(3)

*   Constant values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'tamet2_read_param')

      character section_name*(*)
      parameter (section_name = 'parameters')

*- Implementation Section ----------------------------------

      call push_routine (myname)

      call write_string (
     :          new_line//'   - Reading Tamet2 Parameters')

      call read_real_var (
     :     section_name,          ! Section header
     :     'dis_evap',            ! Keyword
     :     '(mm)',               ! Units
     :     p%dis_evap,                 ! Variable
     :     numvals,               ! Number of values returned
     :     0.0,         ! Lower Limit for bound checking
     :     100.0)         ! Upper Limit for bound checking

      call read_real_var (
     :     section_name,          ! Section header
     :     'dish_evap',            ! Keyword
     :     '(mm)',               ! Units
     :     p%dish_evap,                 ! Variable
     :     numvals,               ! Number of values returned
     :     -100.0,         ! Lower Limit for bound checking
     :     0.0)         ! Upper Limit for bound checking

      call read_real_var (
     :     section_name,          ! Section header
     :     'disvh_evap',            ! Keyword
     :     '(mm)',               ! Units
     :     p%disvh_evap,                 ! Variable
     :     numvals,               ! Number of values returned
     :     -100.0,         ! Lower Limit for bound checking
     :     0.0)         ! Upper Limit for bound checking

      call read_real_var (
     :     section_name,          ! Section header
     :     'dis_maxt_dry_season',            ! Keyword
     :     '(oC)',               ! Units
     :     p%dis_maxt_dry,                 ! Variable
     :     numvals,               ! Number of values returned
     :     0.0,         ! Lower Limit for bound checking
     :     60.0)         ! Upper Limit for bound checking

      call read_real_var (
     :     section_name,          ! Section header
     :     'dis_maxt_other',            ! Keyword
     :     '(oC)',               ! Units
     :     p%dis_maxt_other,                 ! Variable
     :     numvals,               ! Number of values returned
     :     0.0,         ! Lower Limit for bound checking
     :     60.0)         ! Upper Limit for bound checking

      call read_real_var (
     :     section_name,          ! Section header
     :     'dis_mint',            ! Keyword
     :     '(oC)',               ! Units
     :     p%dis_mint,                 ! Variable
     :     numvals,               ! Number of values returned
     :     0.0,         ! Lower Limit for bound checking
     :     60.0)         ! Upper Limit for bound checking



      call read_real_var (
     :     section_name,          ! Section header
     :     'Maxt_to_minT',            ! Keyword
     :     '(oC)',               ! Units
     :     p%Maxt_to_minT,                 ! Variable
     :     numvals,               ! Number of values returned
     :     0.0,         ! Lower Limit for bound checking
     :     60.0)         ! Upper Limit for bound checking




      call read_real_var (
     :     section_name,          ! Section header
     :     'rain_lb',            ! Keyword
     :     '(mm)',               ! Units
     :     p%rain_lb,                 ! Variable
     :     numvals,               ! Number of values returned
     :     0.0,         ! Lower Limit for bound checking
     :     100.0)         ! Upper Limit for bound checking




      call read_real_var (
     :     section_name,          ! Section header
     :     'rain_ub',            ! Keyword
     :     '(mm)',               ! Units
     :     p%rain_ub,                 ! Variable
     :     numvals,               ! Number of values returned
     :     0.0,         ! Lower Limit for bound checking
     :     2000.0)         ! Upper Limit for bound checking




      call read_real_var (
     :     section_name,          ! Section header
     :     'gauge_capacity',            ! Keyword
     :     '(mm)',               ! Units
     :     p%gauge_capacity,                 ! Variable
     :     numvals,               ! Number of values returned
     :     0.0,         ! Lower Limit for bound checking
     :     1000.0)         ! Upper Limit for bound checking




      call read_real_var (
     :     section_name,          ! Section header
     :     'maxt_lb',            ! Keyword
     :     '(oC)',               ! Units
     :     p%maxt_lb,                 ! Variable
     :     numvals,               ! Number of values returned
     :     0.0,         ! Lower Limit for bound checking
     :     30.0)         ! Upper Limit for bound checking




      call read_real_var (
     :     section_name,          ! Section header
     :     'maxt_ub',            ! Keyword
     :     '(oC)',               ! Units
     :     p%maxt_ub,                 ! Variable
     :     numvals,               ! Number of values returned
     :     0.0,         ! Lower Limit for bound checking
     :     60.0)         ! Upper Limit for bound checking




      call read_real_var (
     :     section_name,          ! Section header
     :     'mint_lb',            ! Keyword
     :     '(oC)',               ! Units
     :     p%mint_lb,                 ! Variable
     :     numvals,               ! Number of values returned
     :     -20.0,         ! Lower Limit for bound checking
     :     30.0)         ! Upper Limit for bound checking




      call read_real_var (
     :     section_name,          ! Section header
     :     'mint_ub',            ! Keyword
     :     '(oC)',               ! Units
     :     p%mint_ub,                 ! Variable
     :     numvals,               ! Number of values returned
     :     0.0,         ! Lower Limit for bound checking
     :     40.0)         ! Upper Limit for bound checking




      call read_real_var (
     :     section_name,          ! Section header
     :     'evap_lb',            ! Keyword
     :     '(mm)',               ! Units
     :     p%evap_lb,                 ! Variable
     :     numvals,               ! Number of values returned
     :     0.0,         ! Lower Limit for bound checking
     :     100.0)         ! Upper Limit for bound checking




      call read_real_var (
     :     section_name,          ! Section header
     :     'evap_ub',            ! Keyword
     :     '(mm)',               ! Units
     :     p%evap_ub,                 ! Variable
     :     numvals,               ! Number of values returned
     :     0.0,         ! Lower Limit for bound checking
     :     100.0)         ! Upper Limit for bound checking




      call read_real_var (
     :     section_name,          ! Section header
     :     'wind_lb',            ! Keyword
     :     '(km)',               ! Units
     :     p%wind_lb,                 ! Variable
     :     numvals,               ! Number of values returned
     :     0.0,         ! Lower Limit for bound checking
     :     100.0)         ! Upper Limit for bound checking




      call read_real_var (
     :     section_name,          ! Section header
     :     'wind_ub',            ! Keyword
     :     '(km)',               ! Units
     :     p%wind_ub,                 ! Variable
     :     numvals,               ! Number of values returned
     :     0.0,         ! Lower Limit for bound checking
     :     2000.0)         ! Upper Limit for bound checking




      call read_real_var (
     :     section_name,          ! Section header
     :     'radn_lb',            ! Keyword
     :     '()',               ! Units
     :     p%radn_lb,                 ! Variable
     :     numvals,               ! Number of values returned
     :     0.0,         ! Lower Limit for bound checking
     :     2.0)         ! Upper Limit for bound checking




      call read_real_var (
     :     section_name,          ! Section header
     :     'radn_ub',            ! Keyword
     :     '()',               ! Units
     :     p%radn_ub,                 ! Variable
     :     numvals,               ! Number of values returned
     :     0.0,         ! Lower Limit for bound checking
     :     2.0)         ! Upper Limit for bound checking




      call read_real_var (
     :     section_name,          ! Section header
     :     'radn_low',            ! Keyword
     :     '()',               ! Units
     :     p%radn_low,                 ! Variable
     :     numvals,               ! Number of values returned
     :     0.0,         ! Lower Limit for bound checking
     :     1.0)         ! Upper Limit for bound checking




      call read_real_var (
     :     section_name,          ! Section header
     :     'radn_vlow',            ! Keyword
     :     '()',               ! Units
     :     p%radn_vlow,                 ! Variable
     :     numvals,               ! Number of values returned
     :     0.0,         ! Lower Limit for bound checking
     :     1.0)         ! Upper Limit for bound checking



         ! met parameter names
!      call read_char_array (
!     :           section_name
!     :          ,'names'
!     :          ,numvars
!     :          ,'(-)'
!     :          ,p%names
!     :          ,p%numvars)

      call pop_routine  (myname)
      return
      end subroutine


*===========================================================
      subroutine tamet2_read_constants ()
*===========================================================

      Use infrastructure
      implicit none


*+  Purpose
*      Read in all parameters from parameter file.

*+  Mission Statement
*     Read Constants from Ini file

*   Changes:
*       210995 jngh programmed

*   Internal variables
      integer    numvals               ! number of values read

*   Constant values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'tamet2_read_constants')

      character section_name*(*)
      parameter (section_name = 'constants')

*- Implementation Section ----------------------------------

      call push_routine (myname)

      call pop_routine  (myname)
      return
      end subroutine

*================================================================
      subroutine tamet2_prepare ()
*================================================================
      use Infrastructure
      implicit   none

*+  Purpose
*     Perform calculations before the current timestep.

*+  Mission statement
*     Perform calculations before the current timestep

*   changes:
*       210995 jngh programmed

*   constant values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'tamet2_prepare')

* --------------------- executable code section ----------------------

      call push_routine (myname)

      call pop_routine (myname)
      return
      end subroutine
*====================================================================
      subroutine tamet2_get_other_variables_init ()
*====================================================================

      Use infrastructure
      implicit none


*+  Purpose
*      Get the values of variables from other modules

*+  Mission statement
*      Get the values of variables from other modules

*   Changes:
*       210995 jngh programmed

*   Internal variables
      integer    numvals               ! number of values returned
      integer    i
      integer    j
      real       value
      integer    idate(3)


*   Constant values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'tamet2_get_other_variables_init')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call get_real_var (
     :           unknown_module
     :          ,'latitude'
     :          ,'(deg)'
     :          ,g%latitude
     :          ,numvals
     :          ,-90.0
     :          ,90.0)


       call get_integer_var (
     :      unknown_module
     :     ,'day'
     :     ,'()'
     :     ,g%day
     :     ,numvals
     :     ,1
     :     ,366)

       call get_integer_var (
     :      unknown_module
     :     ,'year'
     :     ,'()'
     :     ,g%year
     :     ,numvals
     :     ,1800
     :     ,2050)

      call day_of_year_to_date (g%day, g%year, idate)
      g%last_day = Date_to_jday (idate(1), idate(2), idate(3))
     :           - 1.0D0


      call get_real_var_optional (
     :      unknown_module
     :     ,'wind'
     :     ,'(km/day)'
     :     ,g%wind
     :     ,numvals
     :     ,0.0
     :     ,1000.0)

      if (numvals.gt.0) then
         g%wind_found = .true.
      else
         g%wind_found = .false.
      endif


!      g%numvars = 0
!      do i=1, p%numvars
!      if (p%names(i).ne.blank) then

!      call get_real_var_optional (
!     :      unknown_module
!     :     ,p%names(i)
!     :     ,'()'
!     :     ,value
!     :     ,numvals
!     :     ,-1000.0
!     :     ,10000.0)

!         if (numvals.gt.0) then
!            g%numvars = g%numvars + 1
!            g%name_found(g%numvars) = p%names(i)
!         else
!         endif

!      else
!      endif

!      end do

      call pop_routine (myname)
      return
      end subroutine
*====================================================================
      subroutine tamet2_get_other_variables ()
*====================================================================

      Use infrastructure
      implicit none

*+  Purpose
*      Get the values of variables from other modules

*+  Mission Statement
*     Get Other Variables

*   Changes:
*       210995 jngh programmed

*   Internal variables
      integer    numvals               ! number of values returned
      integer    i


*   Constant values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'tamet2_get_other_variables')

*- Implementation Section ----------------------------------
      call push_routine (myname)

!   D(I,3-5)  DAY, MONTH, YEAR
!   D(I,6)    RAIN
!   D(I,7)    EVAPORATION
!   D(I,8-12) TEMPERATURES: DRY, WET, MAX, MIN, TERR MIN
!   D(I,13)   WIND RUN
!   D(I,14)   RADIATION
      call get_integer_var (
     :      Unknown_module
     :    , 'year'
     :    , '()'
     :    , g%year
     :    , numvals
     :    , 1800, 2050)

      g%a_year(offset) = g%year

      call get_integer_var (
     :      Unknown_module
     :    , 'day'
     :    , '()'
     :    , g%day
     :    , numvals
     :    , 1
     :    , 366)

      g%a_day(offset) = g%day

      call get_real_var (
     :      unknown_module
     :     ,'radn'
     :     ,'(MJ/m2)'
     :     ,g%radn
     :     ,numvals
     :     ,0.0
     :     ,100.0)

      g%a_radn(offset) = g%radn

      call Get_real_var(
     :      unknown_module  ! Module that responds (Not Used)
     :    , 'maxt'          ! Variable Name
     :    , '(oC)'          ! Units                (Not Used)
     :    , g%maxt          ! Variable
     :    , numvals         ! Number of values returned
     :    , 0.0             ! Lower Limit for bound checking
     :    , 50.0)           ! Upper Limit for bound checking

      g%a_max(offset) = g%maxt

      call Get_real_var(
     :      unknown_module  ! Module that responds (Not Used)
     :    , 'mint'          ! Variable Name
     :    , '(oC)'          ! Units                (Not Used)
     :    , g%mint          ! Variable
     :    , numvals         ! Number of values returned
     :    , -20.0           ! Lower Limit for bound checking
     :    , 40.0)           ! Upper Limit for bound checking

      g%a_min(offset) = g%mint

      call Get_real_var(
     :      unknown_module  ! Module that responds (Not Used)
     :    , 'rain'          ! Variable Name
     :    , '(mm)'          ! Units                (Not Used)
     :    , g%rain          ! Variable
     :    , numvals         ! Number of values returned
     :    , 0.0             ! Lower Limit for bound checking
     :    , 1000.0)         ! Upper Limit for bound checking

      g%a_rain(offset) = g%rain

      call Get_real_var_optional(
     :      unknown_module  ! Module that responds (Not Used)
     :    , 'evap'          ! Variable Name
     :    , '(mm)'          ! Units                (Not Used)
     :    , g%evap          ! Variable
     :    , numvals         ! Number of values returned
     :    , 0.0             ! Lower Limit for bound checking
     :    , 1000.0)         ! Upper Limit for bound checking

      g%a_evap(offset) = g%evap

      call get_real_var_optional (
     :      unknown_module
     :     ,'wind'
     :     ,'(km/day)'
     :     ,g%wind
     :     ,numvals
     :     ,0.0
     :     ,1000.0)

      g%a_wind(offset) = g%wind


      call pop_routine (myname)
      return
      end subroutine
*====================================================================
      subroutine tamet2_send_my_variable (Variable_name)
*====================================================================
      use Infrastructure
      implicit none

*+  Purpose
*      Return the value of one of our variables to caller

*+  Mission Statement
*     Send Value of Requested Variable

*   Changes:
*       210995 jngh programmed

*   Subroutine arguments
      character  Variable_name*(*)     ! (INPUT) Variable name to search for

*   Constant values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'tamet2_send_my_variable')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      if (variable_name .eq. 'error') then
         call respond2get_integer_var (
     :               variable_name
     :              ,'()'
     :              ,g%error)

      elseif (variable_name .eq. 'field') then
         call respond2get_char_var (
     :               variable_name
     :              ,'()'
     :              ,g%field)

      elseif (variable_name .eq. 'type') then
         call respond2get_char_var (
     :               variable_name
     :              ,'()'
     :              ,g%type)

      elseif (variable_name .eq. 'dis_radn') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(MJ/m2)'
     :              ,g%dis_radn)


      elseif (variable_name .eq. 'dis_maxt') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(oC)'
     :              ,g%dis_maxt)


      elseif (variable_name .eq. 'dis_mint') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(oC)'
     :              ,g%dis_mint)


      elseif (variable_name .eq. 'dis_evap') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(mm)'
     :              ,g%dis_evap)


      elseif (variable_name .eq. 'dis_wind') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(km)'
     :              ,g%dis_wind)


      elseif (variable_name .eq. 'radn_rcal') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(MJ/m2)'
     :              ,g%radn_rcal)

      else

         call Message_unused ()
      endif

      call pop_routine (myname)
      return
      end subroutine
*================================================================
      subroutine tamet2_process ()
*================================================================
      use Infrastructure
      implicit   none


*+  Purpose
*      perform actions for current day

*+  Mission Statement
*     Perform all APSIM Timestep calculations

*   changes:
*       210995 jngh programmed

*   constant values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'tamet2_process')


*- Implementation Section ----------------------------------

      call push_routine (myname)
      call tamet2_main()
      call pop_routine (myname)
      return
      end subroutine


*================================================================
      subroutine tamet2_main()
*================================================================

      Use infrastructure
      implicit none

      character string*80

      real  radn_cal

      DOUBLE PRECISION LAT
      INTEGER DS, YR
      integer idate(3)
      CHARACTER STN*4
      integer  lag
      integer  lag2
      integer  j
      integer  nv
      real     rcal
      real     rcal20
      real     bottom_limit
      real     top_limit
      real     dec
      integer  i
      integer  ia
      integer  ib
      real    dk
      real    dkj
      integer  k
      integer  nm
      integer  mth

*+  Purpose
*      Calculations for current day

*+  Mission Statement
*     Perform calculations for current day

! ***************
!   AV      R MONTH AVERAGE
!   BL      R LOWER LIMIT FOR RADIATION
!   CT      I COUNT OF NO OF DATA VALUES OF EACH ELEMENT
!   D       I INPUT DATA ARRAY
!   D(I,1)    STATION CODE
!   D(I,2)    MET BUREAU STATION NO, THEN USED FOR WET BULB DEPRESSION
!   D(I,3-5)  DAY, MONTH, YEAR
!   D(I,6)    RAIN
!   D(I,7)    EVAPORATION
!   D(I,8-12) TEMPERATURES: DRY, WET, MAX, MIN, TERR MIN
!   D(I,13)   WIND RUN
!   D(I,14)   RADIATION
!   DEC     R SOLAR DECLINATION
!   DPR     I WET BULB DEPRESSION
!   DS      I 1 IN DRY SEASON, 0 OTHERWISE
!   EV      R EVAPORATION
!   F6      I LOCATIONS OF F6.1 MEANS IN PRIN
!   HR      R HOURS OF RAIN
!   I,IA,IB,J  INDEXING VARIABLES
!   ICENT   I CENTURY NO
!   IDT     I PARAMETERS OF STATION BEING PROCESSED
!   IRD     I RADIATION EXPRESSED AS % OF THEORETICAL FOR EACH DAY
!   JA,JB,JC  COUNTERS FOR * STRINGS
!   K,L,M,N,P,Q  I TEMPORARY OR INDEXING VARIABLES
!   NUMERR(0)   I NO OF OCCURRENCES, IN ONE RECORD, OF NON-NUMERIC INPWT
!             FIELDS WHICH ARE USUALLY NUMERIC
!   LAG     I ERROR FLAG   0 NO ERRORS, TAPE1 WILL BE OUTPUT
!                          1 POSSIBLE ERRORS, TAPES1,2,4 OUTPUT
!                          2 DEFINITE ERRORS, TAPES2,4 OUTPUT
!   LA      I SPECIFIES WHICH COLUMN IN PRIN THE CURRENT MONTH'S RAINFAL
!             IS TO GO INTO. SET BY RAINFLAG RF
!   LB      I LIMITS NO OF FIELDS READ IN WHEN RF=1
!   LAT     R LATITUDE IN DEGREES
!   LIMDIS  I DISCONTINUITY LIMITS
!   LYR     I 1 IN LEAP YEAR, 0 OTHERWISE
!   MAP     I ARRAY OF FLAGS, CORRESPONDING TO ARRAY D
!             FLAGS ARE:-
!                   0  NORMAL DATA
!                   1  BLANK
!                   2  *
!                   3  * STRINGS WHICH BEGIN ON FIRST CARD OR END ON
!                       CARD OF MONTH IN WIND, RADIATION, OR EVAPORATION
!                   4  DATA WHICH FOLLOWS * STRINGS FLAGGED WITH 3
!                   5  DATA WHICH FOLLOWS * STRINGS FLAGGED WITH 2
!   MEAN    I MONTH AVERAGE
!   MES     I CONTAINS DISCONTINUITY ERROR MESSAGES
!   MP      I VALUES OF MAP(NCP,7-13-14)
!   MTH     I MONTH
!   NC      I NO OF DAYS IN MONTH
!   NCP     I NO OF DAYS IN PREVIOUS MONTH (INITIALLY SET AT 30)
!   NM      I MONTHLY PART OF DAY NO
!   NV      I NO OF LOW RADIATION VALUES IN A MONTH
!   PARS    I  STATION PARAMETERS OF ALL STATIONS (NAME, COORDINATES,
!             ELEVATION, STATION NO, SOLARIMETER TYPE, STATION CODE)
!   PRIN    I ARRAY OF OUTPUT DATA FOR PRINTING
!   R       I 1 STATION UNIDENTIFIED, RADIATION CHECKS CANNOT BE DONE, 0
!   RA      R RAIN
!   RAINYR  I YEAR USED IN RAIN ONLY PRINTOUT
!   RCAL    R CALCULATED CLEAR SKY RADIATION
!   RF      I RAINFLAG. IDENTIFIED BY AN 'R' IN COL 5 OF INPUT DATA
!             SIGNIFIES RECORD CONTAINS ONLY RAINFALL DATA AND SETS
!             'RF' TO 1, 0 OTHERWISE
!   RH      I RELATIVE HUMIDITY
!   RHP     I RH PREVIOUSLY CALCULATED
!   RN      R RADIATION
!   STN     I STATION CODE
!   TE      R TEMPERATURE
!   TL      R UPPER LIMIT FOR RADIATION
!   TTL     R MONTH TOTALS OF EACH ELEMENT
!   VP      R VAPOUR PRESSURE
!   Y       I INDEXING VARIABLE FOR ARRAY IRD
!   YR      I YEAR

!   UNITS USED ARE AS FOLLOWS:-
!                          1   OUTPUT DATA FOR PRINTING
!                          2   ERROR MESSAGES
!                          3   INPUT DATA
! *************
      DATA STN, MTH, YR/'    ', 0, 0/

      LAG = 0

      call day_of_year_to_date (g%day, g%year, idate)
!         COMPUTE SEASON, COMPUTE MONTHLY PART OF DAY NUMBER
!         ASSUMING FEB ALWAYS 28 DAYS.   SET UP DISCONTINUITY LIMITS

      IF (g%latitude.LT.-18. .OR. g%latitude.GT.-11.) then
         DS = 0
      else
         DS = idate(2)/6*(1-idate(2)/10)  ! 1 = dry season (6-9)

      endif
      if (ds.eq.0) then
         p%dis_maxt = p%dis_maxt_other
      else
         p%dis_maxt = p%dis_maxt_dry
      endif

      call jday_to_date (I, mth, yr, g%last_day)
      call day_of_year_to_date (g%day, g%year, idate)
      MTH = idate(2)
!65    continue
!      LYR = (YR-(YR-1)/4*4)/4
      I = MTH + MTH/8

      LAT = g%latitude
!      IF (LAT.EQ.0E0) GO TO 10


      K = MTH - 1
      IF (K.GE.2) then
         NM = K*30 + (K+K/8-1)/2 - 1
      else
         NM = 31*K
      endif
!*************

!   SET DISCONTINUITY LIMITS


            call day_of_year_to_date (g%day, g%year, idate)
!        FIND INDEXES FOR PREVIOUS 2 CARDS
         I = 3
         IA = 1
         IB = 2


      if (g%last_day + 1.0D0
     :   .eq. Date_to_jday (iDate(1), idate(2), idate(3))) then
         ! dates ok
      else
         call t2_error (LAG,2,STN,g%day,g%year,
     :                 'Dates inconsistant')
         WRITE (string, '(15x, a, 2I3, I5)')
     :                    ' Expected date =', I, MTH, YR
         call write_string (string)

      endif

      g%last_day = Date_to_jday (iDate(1), idate(2), idate(3))

!********************    TEMPERATURES    *******************

!         CHECK DB NOT LESS THAN WB, COMPUTE WB DEPRESSION
!       CHECK IF DB=WB AND RAIN=0
!       COMPUTE AND CHECK VP,RH,ENTER RH
!       KEEP YESTERDAYS RHP(9AM) AND RH(3PM)


      if (g%a_day(1).gt.0) then
            g%dis_radn = ABS((g%a_radn(IA) - g%a_radn(IB))
     :         +  (g%radn - g%a_radn(IB)))
            g%dis_wind = ABS((g%a_wind(IA) - g%a_wind(IB))
     :         +  (g%wind - g%a_wind(IB)))
            g%dis_evap = ABS((g%a_evap(IA) - g%a_evap(IB))
     :              + (g%evap - g%a_evap(IB)))
!          *****     + EVAP X RAIN
!         CHECK FOR DISCONTINUITY IN EVAP, MAX AND MIN

         !print*, ' 1'
         IF (g%a_rain(IB).LE.3.0) then
         else
            dk = (g%a_evap(IA) - g%a_evap(IB))
     :         + (g%evap - g%a_evap(IB))
            IF (dk.GE.p%dish_evap) then
!            IF (dk.GE.-8.5) then
            else
               IF (dk.LE.p%disvh_evap) then
!               IF (dk.LE.-12.0) then
                  call t2_error (LAG,2,STN, g%a_day(IB), g%a_year(IB)
     :                           ,'Very high Evaporation with rain')
               else
                  call t2_error (LAG,LAG2,STN, g%a_day(IB), g%a_year(IB)
     :            ,'High Evaporation with rain    Possible error')
               endif
               LAG2 = 1 + LAG/2
               write (string, '(15x, a, 10f6.1)') ' Values = ', g%a_evap
               call write_string (string)
               WRITE (string, '(15x, a, f6.1)')
     :                          ' Discontinuity =', g%dis_evap
               call write_string (string)

               g%field = 'evap'
               g%type = 'discont'

            endif
         endif
         !print*, ' 2'

            g%dis_mint = ABS((g%a_min(IA) - g%a_min(IB))
     :              + (g%mint - g%a_min(IB)))
            IF (g%dis_mint.LE.p%dis_mint) then
            else
               LAG2 = 1 + LAG/2
               call t2_error (LAG,LAG2,STN,g%a_day(IB), g%a_year(IB)
     :                     , 'Discontinuity in minT')
               write (string, '(15x, a, 10f6.1)') ' Values = ', g%a_min
               call write_string (string)
               WRITE (string, '(15x, a, f6.1)')
     :               ' Possible error. Discontinuity =', g%dis_mint
               call write_string (string)
               IF (g%rain.EQ.0) then
                  WRITE (string, '(15x, a)') ' No rain next day'
                  call write_string (string)
               else
               endif
               g%field = 'minT'
               g%type = 'discont'

            endif
         !print*, ' 3'
            g%dis_maxt = ABS((g%a_max(IA) - g%a_max(IB))
     :              + (g%maxt- g%a_max(IB)))
            IF (g%dis_maxt.LE.p%dis_maxt) then
            else
               LAG2 = 1 + LAG/2
         !print*, ' 31, lag, lag2, stn, ib, g%a_day(ib)'
         !print*, ' 31', lag, lag2, stn, ib, g%a_day(ib)
               call t2_error (LAG,LAG2,STN,g%a_day(IB), g%a_year(IB)
     :                     , 'Discontinuity in maxT')
         !print*, ' 32'
               write (string, '(15x, a, 10f6.1)') ' Values = ',g%a_max
               call write_string (string)
               WRITE (string, '(15x, a, f6.1)')
     :               ' Possible error. Discontinuity =', g%dis_maxt
               call write_string (string)
         !print*, ' 33'
               IF (g%a_rain(IB).EQ.0)  then
                  WRITE (string, '(15x, a)') ' No rain this day'
                  call write_string (string)
               else
               endif
               IF (g%rain.EQ.0)  then
                  WRITE (string, '(15x, a)') ' No rain next day'
                  call write_string (string)
               else
               endif
               g%field = 'maxT'
               g%type = 'discont'

            endif
         !print*, ' 34'
            IF (g%dis_evap.LE.p%dis_evap) then
            else
               LAG2 = 1 + LAG/2
               call t2_error (LAG,LAG2,STN,g%a_day(IB), g%a_year(IB)
     :                     , 'Discontinuity in evap')
               write (string,'(15x, a, 10f6.1)') ' Values = ',g%a_evap
               call write_string (string)
               WRITE (string, '(15x, a, f6.1)')
     :               ' Possible error. Discontinuity =', g%dis_evap
               call write_string (string)
               IF (g%rain.EQ.0) then
                  WRITE (string, '(15x, a)') ' No rain next day'
                  call write_string (string)
               else
               endif

               g%field = 'evap'
               g%type = 'discont'

            endif

         !print*, ' 4'
      else
      endif

      NV = 0

      I = idate(1)


!******************    RAINFALL    *******************
!       CHECK RAINFALL


      IF (g%rain.GE.p%rain_lb .AND. g%rain.LE.p%rain_ub) then
         LAG2 = 1 + LAG/2
         IF (g%rain.GT.p%gauge_capacity) then
            call t2_errorR (LAG,LAG2,STN,g%day,g%year,g%rain
     :   ,' mm Rainfall exceeds capacity of gauge   Possible error')
         g%field = 'rain'
         g%type = 'gauge'

         else
         endif
      else
         call t2_errorR (LAG,2,STN,g%day,g%year,g%rain
     :            ,' mm Rainfall outside limits')
         g%field = 'rain'
         g%type = 'limits'

      endif

         !print*, ' 5'

!*******************    EVAPORATION    *********************
      !print*, evap
      IF (g%evap.GE.p%evap_lb .AND. g%evap.LE.p%evap_ub) then
      else
         call t2_errorr (LAG,2,STN,g%day,g%year, g%evap
     :               ,'Evaporation outside limits')
         g%field = 'evap'
         g%type = 'limits'

      endif


!********************    MAX TEMPERATURE    *********************
!               CHECK MAX WITHIN LIMITS

      IF (g%maxt.LT.p%maxt_lb .OR. g%maxt.GT.p%maxt_ub) then
         call t2_errorr (LAG,2,STN,g%day,g%year, g%maxt
     :               ,'MaxT outside limits')
         g%field = 'MaxT'
         g%type = 'limits'

      else
      endif

!*********************    MIN TEMPERATURE    ********************
!  CHECK MIN WITHIN LIMITS

      IF (g%mint.GE.p%mint_lb .AND. g%mint.LE.p%mint_ub) then !GO TO 530
      else
         call t2_errorr (LAG,2,STN,g%day,g%year, g%mint
     :               ,'MinT outside limits')
         g%field = 'MinT'
         g%type = 'limits'

      endif

!********************    WIND    ***********************
      if (g%wind_found) then
         IF (g%wind.GE.p%wind_lb .AND. g%wind.LE.p%wind_ub) then
         else
            call t2_errorr (LAG,2,STN,g%day,g%year, g%wind
     :               , 'Wind run outside limitS')
         endif
         g%field = 'Wind'
         g%type = 'limits'

      else
      endif
         !print*, ' 6'

!********************    RADIATION    *******************

      DEC = 23.45 * DSIN(.0172 * DBLE(NM + I - 83))
      RCAL  = (318.24 * DCOS((DEC-LAT)/57.3) - 17.46)
     :      * (1E0 + DEC * LAT/5157.0)
     :      / 10.0

      radn_cal = radn_max (g%day, g%latitude, -0.83)
      !print*, g%latitude, lat, g%day
      !print*, g%radn, rcal, radn_cal
      g%radn_rcal = rcal - g%radn
      bottom_limit = p%radn_lb*RCAL
      top_limit = p%radn_ub*RCAL

      g%RD(idate(1)) = g%radn*100.0/RCAL
      IF (g%radn.LT.p%radn_low*RCAL-0.05) then
         g%NV = g%NV + 1
      else
      endif

      IF (g%radn.GE.bottom_limit-0.5
     :   .AND. g%radn.LE.top_limit+0.5) then
         RCAL20 = p%radn_vlow*RCAL
         IF (g%radn.GE.int(RCAL20-0.5)) then
         else
            call t2_errorr (LAG,LAG2,STN,g%day,g%year, g%radn
     :               ,'Radiation very low   Possible error ')
            WRITE (string, 99890) g%radn, RCAL20
99890       FORMAT (15X, ' Recorded =', F7.1, ' Min =', F6.2)
            call write_string (string)
            g%field = 'radn'
            g%type = 'VLow'

         endif
      else
         LAG2 = 1 + LAG/2
         call t2_errorr (LAG,LAG2,STN,g%day,g%year, g%radn
     :               ,'Radiation outside limits   Possible error ')
         WRITE (string, 99990)  g%radn, bottom_limit, top_limit
99990    FORMAT(15x, ' Recorded =', F7.1, ' Limits =', 2F10.2)
         call write_string (string)
         g%field = 'radn'
         g%type = 'limits'

      endif


      if (End_month (g%day, g%Year)) then
         IF (NV.LE.idate(1)-2) then
         else
            LAG2 = 1 + LAG/2
            call t2_error (LAG,LAG2,STN,g%day,g%year,
     :  'Radiation values well below clear sky value   Possible error')
         WRITE (string, 99989)  g%NV, ' 65% of ', RCAL20
99989    FORMAT (15x, I8, ' Values below', a, F6.2)
         call write_string (string)

         write (string, '(a)')' Observed values,'//
     : ' expressed as Percentage of calculated clear sky value, are:-'
         call write_string (string)

         write (string, '(1x, 16f6.1)') (g%rd(j), j=1, 16)
         call write_string (string)
         write (string, '(1x, 16f6.1)') (g%rd(j), j=17, idate(1))
         call write_string (string)
!         LAG = 1 + LAG/2
         endif
         g%rd(:) = 0.0
         g%nv = 0
      else
      endif

      IF (g%maxt.le. g%mint) then
         call t2_errorr (LAG,2,STN,g%day,g%year, g%maxt
     :               ,' MaxT < or = MinT')
         write (string, '(15x, a, 2f6.1)')
     :               ' MaxT, MinT = ', g%maxt, g%mint
         call write_string (string)
         g%field = 'MaxT'
         g%type = 'MinT'

      elseIF (g%maxt.le. g%mint+p%Maxt_to_minT
     :   .and. g%rain.le.0.0001) then
         if ( g%radn .gt.p%radn_low*RCAL) then
            call t2_errorr (LAG,2,STN,g%day,g%year, g%maxt
     :       ,' MaxT is close to MinT and not overcast. Possible error')
            write (string, '(15x, a, 2f6.1)')
     :               ' MaxT, MinT = ', g%maxt, g%mint
            call write_string (string)
            g%field = 'MaxT'
            g%type = 'MinT'
         else
         endif

      else

      endif

         !print*, ' 7'
      g%error = lag
      return
      end subroutine

*================================================================
      SUBROUTINE t2_ERROR (LAG,N2,STN,K1,K3,RMESS)
*================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER*4 STN
      CHARACTER RMESS*(*)
      character string*80
      integer LAG, N2, K1, K3
*+  Purpose
*      Produce error string

*+  Mission Statement
*     Produce error string

      LAG = N2

         !print*, ' e1'

      WRITE (string, 1000) STN,K1,K3,trim(rmess)
 1000 FORMAT(A1, I4, I5, 3X, a)
         !print*, ' e2'
      call write_string (string)
      RETURN
      end subroutine

*================================================================
      SUBROUTINE t2_ERRORR (LAG,N2,STN,K1,K3,r4,RMESS)
*================================================================
      Use infrastructure
      implicit none
      character string*80
      CHARACTER*4 STN
      CHARACTER RMESS*(*)
      integer lag, N2, k1, k3
      real r4

      LAG = N2

*+  Purpose
*      Produce error string

*+  Mission Statement
*     Produce error string

      WRITE (string, 1000) STN,K1,K3,r4,trim(rmess)
 1000 FORMAT(A1,I4,I5,1X,f7.1,1x,a)
      call write_string (string)
      RETURN
      end subroutine


*     ===========================================================
      real function radn_max (dyoyr, lat, sun_angle)
*     ===========================================================
      Use infrastructure
      implicit none


*+ Sub-Program Arguments
      real       sun_angle             ! (INPUT) angle to measure time between
                                       ! such as twilight (deg).
                                       ! angular distance between 90 deg
                                       ! and end of twilight - altitude
                                       ! of sun. +ve up, -ve down.
      integer    dyoyr                 ! (INPUT) day of year number
      real       lat                   ! (INPUT) latitude of site (deg)

*+ Purpose
*      return the time elasped in hours between the specified sun angle
*      from 90 deg in am and pm. +ve above the horizon, -ve below the horizon.

*+ Notes
*                    there is a small err in cos (90), thus a special
*                    case is made for this.

*+  Mission Statement
*      day length for %1 and %2

*+ Changes
*       020392 jngh specified and programmed
*       130592 jngh limited altitude for twilight to increase range_of
*                   of latitudes. - cr324
*                   limited cos of the hourangle between -1 and 1 - cr324
*       190592 jngh renamed day_length routine - cr323
*       290592 jngh set cos hourangle to +/-1 when latitude is +/- 90 - cr350
*                   corrected descriptions - cr352
*       230792 jngh corrected coshra to take account of latitude sign - cr401
*       200893 jngh corrected problem with precision which occurred when
*                   latitude is very close to tropic line and declination
*                   is also very close, abs(slsd-clcd) may go slightly above
*                   1.0, which asin doesn't like.
*       071293 jngh added sun (twilight) angle to arguments
*       270295 jngh put in function to test for equal reals.


*+ Constant Values
      real       aeqnox                ! average day number of autumnal
      parameter (aeqnox = 82.25)       !   equinox
*
      real       pi                    ! pi
      parameter (pi =  3.14159265359)
*
      real       dg2rdn                ! convert degrees to radians
      parameter (dg2rdn = (2.0*pi) /360.0)
*
      real       decsol                ! amplitude of declination of sun
                                       !   - declination of sun at solstices.
      parameter (decsol = 23.45116 * dg2rdn)
                                       ! cm says here that the maximum
                                       ! declination is 23.45116 or 23 degrees
                                       ! 27 minutes.
                                       ! I have seen else_where that it should
                                       ! be 23 degrees 26 minutes 30 seconds -
                                       ! 23.44167
*
      real       dy2rdn                ! convert days to radians
      parameter (dy2rdn = (2.0*pi) /365.25)
*
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'radn_max')
*
      real       rdn2hr                ! convert radians to hours
      parameter (rdn2hr = 24.0/(2.0*pi))

      real      water_vap_abs          ! water vapour absorption
      parameter (water_vap_abs = 0.07)

      real      ozone_abs          ! ozone absorption
      parameter (ozone_abs = 0.02)

      real       transmission_coef
      parameter (transmission_coef = 0.6)

*+ Local Variables
      real       alt                   ! twilight altitude limited to max/min
                                       !   sun altitudes end of twilight
                                       !   - altitude of sun. (radians)
      real       altmn                 ! altitude of sun at midnight
      real       altmx                 ! altitude of sun at midday
      real       clcd                  ! cos of latitude * cos of declination
      real       coshra                ! cos of hour angle - angle between the
                                       !   sun and the meridian.
      real       dec                   ! declination of sun in radians - this
                                       !   is the angular distance at solar
                                       !   noon between the sun and the equator.
      real       hrangl                ! hour angle - angle between the sun
                                       !   and the meridian (radians).
      real       hrlt                  ! day_length in hours
      real       latrn                 ! latitude in radians
      real       slsd                  ! sin of latitude * sin of declination
      real       sun_alt               ! angular distance between
                                       ! sunset and end of twilight - altitude
                                       ! of sun. (radians)
                                       ! Twilight is defined as the interval
                                       ! between sunrise or sunset and the
                                       ! time when the true centre of the sun
                                       ! is 6 degrees below the horizon.
                                       ! Sunrise or sunset is defined as when
                                       ! the true centre of the sun is 50'
                                       ! below the horizon.
      real       radius_vector         ! radius vector of earth
      real       ra                    ! radiation at top of atmosphere (MJ/m2)
      real       rs                    ! radiation at surface (MJ/m2)
      real       diffuse_sky_radn
      real       cosz

      real       solar_const           ! MJ/m2/hr
      parameter (solar_const = 4.871)
      real      A
      parameter (A=0.25)
      real      B
      parameter (B=0.50)

*- Implementation Section ----------------------------------


      sun_alt = sun_angle * dg2rdn

          ! calculate daylangth in hours by getting the
          ! solar declination (radians) from the day of year, then using
          ! the sin and cos of the latitude.

          ! declination ranges from -.41 to .41 (summer and winter solstices)

      dec = decsol*sin (dy2rdn* (real(dyoyr) - aeqnox))

          ! get the max and min altitude of sun for today and limit
          ! the twilight altitude between these.

      if (reals_are_equal(abs(lat), 90.0)) then
         coshra = sign (1.0, -dec) * sign (1.0, lat)
      else
         latrn = lat*dg2rdn
         slsd = sin(latrn)*sin(dec)
         clcd = cos(latrn)*cos(dec)

         altmn = asin (bound (slsd - clcd, -1.0, 1.0))
         altmx = asin (bound (slsd + clcd, -1.0, 1.0))
         alt = bound (sun_alt, altmn, altmx)

             ! get cos of the hour angle

         coshra = (sin (alt) - slsd) /clcd
         coshra = bound (coshra, -1.0, 1.0)
      endif

          ! now get the hour angle and the hours of light

      hrangl = acos (coshra)
      radius_vector = 1.0/sqrt(1.0+0.033*cos(dy2rdn*real(dyoyr)))
      ra = (24.0/pi)
     :   * (solar_const/radius_vector**2)
     :   * (hrangl*sin(latrn)*sin(dec)
     :      + sin(hrangl)*cos(latrn)*cos(dec))
!      cosz = (slsd + clcd*cos(hrangl))/dg2rdn
      rs = ra*(A + B)
!      rs = transmission_coef ** (1.0/cosz) * ra
      diffuse_sky_radn = (ra*(1.0-ozone_abs-water_vap_abs) - rs)*0.5
      radn_max = rs  + diffuse_sky_radn

      !print*, hrangl, radius_vector, lat, latrn, dec

      return
      end function


      end module Tamet2Module

!     ===========================================================
      subroutine alloc_dealloc_instance(doAllocate)
!     ===========================================================
      use Tamet2Module
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
      subroutine main (Action, Data_string)
*====================================================================
      Use infrastructure    
      Use Tamet2Module
      implicit none
      ml_external Main

*+  Purpose
*      This routine is the interface between the main system and the
*      Tamet2 module.

*+  Mission statement
*      Handles all the communications for the Tamet2 module.

*   Changes:
*       210995 jngh programmed
*       090696 jngh changed presence report to standard

*   subroutine arguments
      character  Action*(*)            ! Message action to perform
      character  Data_string*(*)       ! Message data

*   Internal variables
      character  module_name*8         ! name of this module

*   Constant values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'apsim_tamet2')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      if (Action.eq.ACTION_Init) then
         call tamet2_zero_variables ()
         call tamet2_get_other_variables_init ()
         call tamet2_init ()

      else if (Action.eq.ACTION_Create) then
         call doRegistrations(id)

      elseif (Action.eq.ACTION_Prepare) then
         call tamet2_prepare ()

      elseif (Action.eq.ACTION_Get_variable) then
         call tamet2_send_my_variable (Data_string)

      elseif (Action.eq.ACTION_Process) then
         call tamet2_zero_daily_variables ()
         call tamet2_get_other_variables ()
         call tamet2_process ()

      elseif (Action.eq.ACTION_End_Run) then

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
