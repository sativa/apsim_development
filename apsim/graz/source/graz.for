! Source: Rickert, K.G. and McKeon, G.M. (1984).  A computer model of the 
!   integration of forage options for beef production.  Proceedings Australian 
!   Society Animal Production.  15:  15-19
!

      module GrazModule
      use Registrations

!   Global variables
!     ================================================================
      type GrazGlobals
      sequence

!   Short description:
!      grazing general common block

      integer day             ! julian day of year
      integer year            ! year
      real stocking_rate      ! stocking rate (beasts / ha).
      real alw                ! animal live wgt. (kg)
      real green_leaf         ! daily green leaf from grasp (kg/ha)
      real dead_leaf          ! daily dead leaf from grasp (kg/ha)
      real green_stem         ! daily green stem from grasp (kg/ha)
      real dead_stem          ! daily dead stem from grasp (kg/ha)
      real acc_eaten          ! accumulated intake by animals
      real acc_growth         ! accumulated growth of sward from grasp
      real intake_restr       ! final intake restriction (0-1)
      character  crop_type*50 ! crop type from grasp


!   Short description:
!      outputs
      real dlt_green_leaf
      real dlt_green_stem
      real dlt_dead_leaf
      real dlt_dead_stem
      real dlt_lwg
      real dead_leaf_tramp
      real dead_stem_tramp

      end type GrazGlobals

!     ================================================================
      type GrazParameters
      sequence

!   Short description:
!      coefficients of animals.

      real intake_util_intercept ! Parameter for feed quality
                                ! restriction. Restriction of intake
                                ! by animal.
      real intake_util_slope  ! Restriction of intake by animal.
      real yld_eat_restr      ! Parameter for low quality restriction.
      real summer_lwg         ! potential lwg for season
      real autumn_lwg         ! potential lwg for season
      real winter_lwg         ! potential lwg for season
      real spring_lwg         ! potential lwg for season
      real leaf_diet          ! something to do with competition
                                ! curve. see "curve_factor"
      real std_alw            ! standard alw for beast (200 kg)
      real metabol_expon      ! ???
      real prop_can_eat       ! ???
      real stocking_rate_init     ! initial stocking rate (still needs a
                                ! sow message to start)
      real alw_init           ! initial live weight
      integer acc_eaten_reset ! Day that pool is reset

      end type GrazParameters

!     ================================================================
      type GrazConstants  
      sequence
      logical   dummy_Constantr

      end type GrazConstants
!     ================================================================

      ! instance variables.
      common /InstancePointers/ ID,g,p,c
      save InstancePointers
      type (GrazGlobals),pointer :: g
      type (GrazParameters),pointer :: p
      type (GrazConstants),pointer :: c
                 
      contains

*     ===========================================================
      subroutine graz_process ()
*     ===========================================================
      Use infrastructure
      implicit none

*   Short description:
*       simulate grazing processes.

*   Assumptions:
*       none

*   Notes:
*       none

*   Procedure attributes:
*      Version:         any hardware/fortran77
*      Extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       Use infrastructure

*   Changes:
*      250894 jngh specified and programmed

*   Calls:
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'graz_process')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      call graz_eat ()

      call graz_update ()

      call graz_event ()

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine graz_eat ()
*     ===========================================================
      Use infrastructure
      implicit none


*   Short description:
*       simulate grazing processes.

*   Assumptions:
*       none

*   Notes:
*       none

*   Procedure attributes:
*      Version:         any hardware/fortran77
*      Extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       Use infrastructure

*   Changes:
*      250894 jngh specified and programmed

*   Calls:
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

*   Subroutine arguments
*       none

*   Global variables

*   Internal variables
      real green_pool, dead_pool
      real tsdm
      real green_prop           ! Proportion of green in tsdm (kg/ha)
      real green_prop_leaf      ! Proportion of green leaf in tsdm (kg/ha)
      real dead_prop_leaf       ! Proportion of dead leaf in tsdm (kg/ha)
      real mod_green_prop       ! adjusted green biomass ratio (0.1-1)
      real green_diet           ! proportion of green in diet
      real prop_consumed        ! (biomass eaten : biomass grown) since
                                ! start of season
      real intake_restr_growth  ! restriction of intake by proportion of
                                ! growth over season (0-1)
      real intake_restr_tsdm    ! restriction of intake by low level of tsdm
      real intake_restr         ! final intake restriction (0-1)
      real anim_intake          ! intake of biomass (kg/beast)
      real tsdm_eaten           ! biomass eaten     (kg/ha)
      real green_eaten, dead_eaten ! pool eaten     (kg/ha)
      real curve_factor         ! competition curve
      real trampled             ! trampled dead leaf + stem
      real trampled_stem        ! trampled dead stem
      real trampled_leaf        ! trampled dead leaf


*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'graz_eat')

cPdeV. This should be a parameter.
      real MIN_ALW
      parameter (MIN_ALW = 10.0)

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      green_pool = g%green_leaf + g%green_stem
      dead_pool = g%dead_leaf + g%dead_stem
      tsdm = green_pool + dead_pool

      green_prop = divide (green_pool, tsdm, 0.0)

      green_prop_leaf = divide (g%green_leaf,
     :     green_pool, 0.0)
      dead_prop_leaf = divide (g%dead_leaf,
     :     dead_pool, 0.0)

      mod_green_prop = (green_prop - 0.10) / 0.90
      mod_green_prop = bound (mod_green_prop, 0.0, 1.0)

*     If green is less than 10%, no active selection for green by stock
      if (mod_green_prop .gt. 0.0) then
         green_diet = graz_comp_curve(mod_green_prop, 19.0)
      else
         green_diet = green_prop
      endif

*     Calculate utilization and its effects on intake
      if (g%acc_growth .gt. 1.0) then
         prop_consumed = divide(g%acc_eaten,
     :        g%acc_growth, 0.0)
         prop_consumed = min(1.0, prop_consumed)
      else
         prop_consumed = 0.0
      endif

*     restriction by proportion of growth:
      intake_restr_growth = p%intake_util_intercept +
     :     p%intake_util_slope * prop_consumed

*     restriction by low tsdm:
      intake_restr_tsdm = divide (tsdm,
     :     p%yld_eat_restr, 0.0)

      intake_restr = min (intake_restr_tsdm, intake_restr_growth)
      intake_restr = bound (intake_restr, 0.0, 1.0)

*     This is the animal lwg model.
      anim_intake = intake_restr *
     :      divide (graz_pot_lwg () + 1.058, 0.304, 0.0)

*     Restrict intake such that the herd cannot eat more than is
*     in sward, then adjust individual animal intake accordingly
      tsdm_eaten = graz_stock_equiv () * anim_intake
      tsdm_eaten = bound (tsdm_eaten, 0.0, tsdm)

      green_eaten = green_diet * tsdm_eaten
      dead_eaten = (1.0 - green_diet) * tsdm_eaten

*     Lwg calculation:
      anim_intake = divide(tsdm_eaten,
     :     graz_stock_equiv (), 0.0)

*     Should formally name and define 0.304 & 1.058.
      g%dlt_lwg = anim_intake * 0.304 - 1.058

*     Restrict the lwg, so that alw never goes below minimum
      g%dlt_lwg = max (MIN_ALW - g%alw, g%dlt_lwg)

      curve_factor = divide (
     :     0.5 * p%leaf_diet - p%leaf_diet,
     :     0.5 * p%leaf_diet - 0.5, 0.0)

      g%dlt_green_leaf = - green_eaten *
     :     graz_comp_curve(green_prop_leaf, curve_factor)

      g%dlt_dead_leaf = - dead_eaten *
     :     graz_comp_curve(dead_prop_leaf, curve_factor)

      g%dlt_green_stem = -( green_eaten + g%dlt_green_leaf )
      g%dlt_dead_stem = -( dead_eaten + g%dlt_dead_leaf )

*     Trampling
      trampled = tsdm_eaten * (
     :     divide(1.0, p%prop_can_eat, 0.0) - 1.0)

*     Apportion to leaf and stem
      trampled_leaf = trampled *
     :     graz_comp_curve(dead_prop_leaf, curve_factor)
      trampled_stem = trampled - trampled_leaf

*     Limit the trampling so that we don't trample more than is
*     actually there.
      g%dead_leaf_tramp = min(
     :     g%dead_leaf + g%dlt_dead_leaf, trampled_leaf)
      g%dead_stem_tramp = min(
     :     g%dead_stem + g%dlt_dead_stem, trampled_stem)

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      real function  graz_stock_equiv ()
*     ===========================================================
      Use infrastructure
      implicit none


*   Short description:
*       Calculate stock equivalent

*   Assumptions:
*       none

*   Notes:
*       none

*   Procedure attributes:
*      Version:         any hardware/fortran77
*      Extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       Use infrastructure

*   Changes:
*      250894 jngh specified and programmed

*   Calls:
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------


*   Constant values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'graz_stock_equiv')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      graz_stock_equiv = g%stocking_rate *
     &     (divide (g%alw, p%std_alw, 0.0) ** p%metabol_expon)

      call pop_routine (my_name)
      return
      end function


*     ===========================================================
      real function graz_pot_lwg ()
*     ===========================================================
      Use infrastructure
      implicit none

*   Short description:
*       Calculate potential lwg.

*   Assumptions:
*       none

*   Notes:
*       none

*   Procedure attributes:
*      Version:         any hardware/fortran77
*      Extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       Use infrastructure

*   Changes:
*      250894 jngh specified and programmed

*   Calls:
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

*   Internal variables
      double precision   julday
      integer            day, month, year
      integer            season(12)

*   Constant values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'graz_pot_lwg')

      integer SUMMER, AUTUMN, WINTER, SPRING
      parameter (SUMMER = 1, AUTUMN = 2, WINTER = 3, SPRING = 4)

      real DAYS_PER_SEASON
      parameter (DAYS_PER_SEASON = 91.25)

*   Initial data values
      data season/1,1,2,2,2,3,3,3,4,4,4,1/

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      graz_pot_lwg = 0.0

c     This is lifted from a very old piece of code. If there was a manual
c     describing these functions available, there would be a better way.

      julday = date_to_jday(1, 1, g%year) - 1.0d0
     :      + real(g%day)

      call jday_to_date (day, month, year, julday)

      call bound_check_integer_var (month, 1, 12, my_name)

      if (month .lt. 1 .or. month .gt. 12) then
         month = 1  ! shouldn't be needed.....
      endif

      if (season(month) .eq. SUMMER) then
         graz_pot_lwg = divide (p%summer_lwg,
     :        DAYS_PER_SEASON, 0.0)
      else if (season(month) .eq. AUTUMN) then
         graz_pot_lwg = divide (p%autumn_lwg,
     :        DAYS_PER_SEASON, 0.0)
      else if (season(month) .eq. WINTER) then
         graz_pot_lwg = divide (p%winter_lwg,
     :        DAYS_PER_SEASON, 0.0)
      else if (season(month) .eq. SPRING) then
         graz_pot_lwg = divide (p%spring_lwg,
     :        DAYS_PER_SEASON, 0.0)
      else
         call fatal_error (Err_User, 'Unknown season??')
      endif

      call pop_routine (my_name)
      return
      end function

*     ===========================================================
      subroutine graz_update ()
*     ===========================================================
      Use infrastructure
      implicit none

*   Short description:
*       Update states

*   Assumptions:
*       none

*   Notes:
*       none

*   Procedure attributes:
*      Version:         any hardware/fortran77
*      Extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       Use infrastructure

*   Changes:
*      250894 jngh specified and programmed

*   Calls:
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'graz_update')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      g%acc_eaten = g%acc_eaten +
     :     (-g%dlt_green_leaf) +
     :     (-g%dlt_green_stem) +
     :     (-g%dlt_dead_leaf) +
     :     (-g%dlt_dead_stem)

      g%alw = g%alw + g%dlt_lwg

      call pop_routine (my_name)
      return
      end subroutine
*     ===========================================================
      subroutine graz_event ()
*     ===========================================================
      Use infrastructure
      implicit none

*   Short description:
*       report occurence of event and the current status of specific
*       variables.

*   Assumptions:
*       none

*   Notes:
*       none

*   Procedure attributes:
*      Version:         any hardware/fortran77
*      Extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       Use infrastructure

*   Changes:
*     010994 jngh specified and programmed

*   Calls:
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'graz_event')

*   Initial data values
*       none
* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      if (g%day .eq. p%acc_eaten_reset) then
         g%acc_eaten = 0.0
      else
                                ! Nothing
      endif

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine graz_zero_variables ()
*     ===========================================================
      Use infrastructure
      implicit none

*   Short description:
*       zero graz_ variables & arrays

*   Assumptions:
*       none

*   Notes:
*       none

*   Procedure attributes:
*      Version:         any hardware/fortran77
*      Extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       Use infrastructure

*   Changes:
*     010994 jngh specified and programmed

*   Calls:
*     pop_routine
*     push_routine
*     fill_real_array

* ----------------------- Declaration section ------------------------

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'graz_zero_variables')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      g%acc_eaten = 0.0
      g%alw = 0.0
      g%stocking_rate = 0.0

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine graz_init ()
*     ===========================================================
      Use infrastructure
      implicit none

*   Short description:
*       crop initialisation

*   Assumptions:
*       none

*   Notes:
*       none

*   Procedure attributes:
*      Version:         any hardware/fortran77
*      Extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       Use infrastructure

*   Changes:
*     010994 jngh specified and programmed


* ----------------------- Declaration section ------------------------

*   Constant values

      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'graz_init')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      call Write_string ('Initialising: ')

           ! initialize crop variables
      call graz_read_parameters ()

      g%alw = p%alw_init
      g%stocking_rate = p%stocking_rate_init

      call pop_routine (my_name)
      return
      end subroutine

*     ================================================================
      subroutine graz_get_other_variables ()
*     ================================================================
      Use infrastructure
      implicit none

*   Short description:
*      get the value/s of variables/arrays.

*   Assumptions:
*      none

*   Notes:
*      none

*   Procedure attributes:
*      Version:         any hardware/fortran77
*      Extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       Use infrastructure

*   Changes:
*     010994 jngh specified and programmed

*   Calls:
*     get_real_var

* ----------------------- Declaration section ------------------------

*   Internal variables
      integer   numvals

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'graz_get_other_variables')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      call get_integer_var (unknown_module, 'day'
     :     , '()', g%day, numvals, 1, 366)

      call get_integer_var (unknown_module, 'year'
     :     , '()', g%year, numvals, 1800, 2000)
                         
                         
      call get_real_var (unknown_module, 
     :      'green_leaf', '(kg/ha)'
     :     , g%green_leaf, numvals
     :     , 0.0, 100000.0)

      call get_real_var (unknown_module, 'green_stem', '(kg/ha)'
     :     , g%green_stem, numvals
     :     , 0.0, 100000.0)

      call get_real_var (unknown_module, 'dead_leaf', '(kg/ha)'
     :     , g%dead_leaf, numvals
     :     , 0.0, 100000.0)

      call get_real_var (unknown_module, 'dead_stem', '(kg/ha)'
     :     , g%dead_stem, numvals
     :     , 0.0, 100000.0)

      call get_real_var (unknown_module, 'acc_growth', '(kg/ha)'
     :     , g%acc_growth, numvals
     :     , 0.0, 1000000.0)

      call get_char_var (unknown_module, 'crop_type', '()'
     :     , g%crop_type, numvals)

      call pop_routine (my_name)
      return
      end subroutine
*     ================================================================
      subroutine graz_set_other_variables ()
*     ================================================================
      Use infrastructure
      implicit none

*   Short description:
*      set the value of a variable or array in other module/s.

*   Assumptions:
*      none

*   Notes:
*      a flag is set if any of the totals is requested.  The totals are
*      reset during the next process phase when this happens.

*   Procedure attributes:
*      Version:         any hardware/fortran77
*      Extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       Use infrastructure

*   Changes:
*     010994 jngh specified and programmed

* ----------------------- Declaration section ------------------------

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'graz_set_other_variables')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      call set_real_var (unknown_module, 'dlt_green_leaf', '(kg/ha)'
     :     , g%dlt_green_leaf)

      call set_real_var (unknown_module, 'dlt_green_stem', '(kg/ha)'
     :                    , g%dlt_green_stem)

      call set_real_var (unknown_module, 'dlt_dead_leaf', '(kg/ha)'
     :     , g%dlt_dead_leaf - g%dead_leaf_tramp)

      call set_real_var (unknown_module, 'dlt_dead_stem', '(kg/ha)'
     :     , g%dlt_dead_stem - g%dead_stem_tramp)

Cpdev Needs to have better indication of N content in residue.
      if (g%dead_leaf_tramp + g%dead_stem_tramp .gt. 0.0 ) then
c        write (*,*) 'graz is adding residue :'
c        write (*,*) ' type = ', g%crop_type
c        write (*,*) ' wt = ', g%dead_leaf_tramp + g%dead_stem_tramp
c        write (*,*) ' n = ', 0.0

        call new_postbox()
        call post_char_var('dlt_residue_type', '()',
     :       g%crop_type)
        call post_real_var('dlt_residue_wt', '(kg/ha)',
     :       g%dead_leaf_tramp + g%dead_stem_tramp)
        call post_real_var('dlt_residue_n', '(kg/ha)',
     :       0.0)

        call event_send('add_residue')
        call delete_postbox ()

      else
          !nothing
      endif

      call pop_routine (my_name)
      return
      end subroutine
*     ===============================================================
      subroutine graz_set_my_variable (Variable_name)
*     ===============================================================
      Use infrastructure
      implicit none

*   Short description:
*      set a variable in this module as requested by another.

*   Assumptions:
*      none

*   Notes:
*      none

*   Procedure attributes:
*      Version:         any hardware/fortran77
*      Extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       Use infrastructure

*   Changes:
*      290393 jngh

*   Calls:
*      none

* ----------------------- Declaration section ------------------------

*   Subroutine arguments
      character  Variable_name*(*)     ! (INPUT) Variable name to search for

*   Internal variables
      integer   numvals

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'graz_set_my_variable')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      if (variable_name .eq. 'stocking_rate') then
         call collect_real_var ('stocking_rate', '()'
     :        , g%stocking_rate, numvals
     :        , 0.0, 100.0)

      elseif (variable_name .eq. 'alw') then
         call collect_real_var ('alw', '()'
     :        , g%alw, numvals
     :        , 0.0, 1000.0)

      else
         call message_unused ()
      endif


      call pop_routine (my_name)
      return
      end subroutine
*     ================================================================
      subroutine graz_send_my_variable (variable_name)
*     ================================================================
      Use infrastructure
      implicit none

*   Short description:
*      return the value of a variable requested by other modules.

*   Assumptions:
*      none

*   Notes:

*   Procedure attributes:
*      Version:         any hardware/fortran77
*      Extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       Use infrastructure

*   Changes:


* ----------------------- Declaration section ------------------------

*   Subroutine arguments
      character variable_name*(*)      ! (INPUT) variable name to search for

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'graz_send_my_variable')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      if (variable_name .eq. 'alw') then
         call respond2get_real_var ('alw'
     :        , '(kg)'
     :        , g%alw)

      elseif (variable_name .eq. 'lwg') then
         call respond2get_real_var ('lwg', '()'
     :        , g%dlt_lwg)

      elseif (variable_name .eq. 'acc_eaten') then
         call respond2get_real_var ('acc_eaten'
     :        , '(kg)'
     :        , g%acc_eaten)

      elseif (variable_name .eq. 'intake_restr') then
         call respond2get_real_var ('intake_restr'
     :        , '()'
     :        , g%intake_restr)

      else
         call message_unused ()
      endif

      call pop_routine (my_name)
      return
      end subroutine
*     ===========================================================
      subroutine graz_read_parameters ()
*     ===========================================================
      Use infrastructure
      implicit none

*   Short description:
*       crop initialisation - reads parameters from coefficient file

*   Assumptions:
*       none

*   Notes:
*       none

*   Procedure attributes:
*      Version:         any hardware/fortran77
*      Extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       Use infrastructure

*   Changes:
*     010994 jngh specified and programmed


* ----------------------- Declaration section ------------------------

*   Internal variables
      integer    numvals               ! number of values returned

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'graz_read_parameters')

      character  section_name*(*)
      parameter (section_name = 'parameters')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      call write_string (new_line//'    - Reading parameters')

      call read_real_var (section_name
     :                    , 'intake_util_intercept', '()'
     :                    , p%intake_util_intercept, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'intake_util_slope', '()'
     :                    , p%intake_util_slope, numvals
     :                    , -100.0, 100.0) !FIXME

      call read_real_var (section_name
     :                    , 'yld_eat_restr', '()'
     :                    , p%yld_eat_restr, numvals
     :                    , 0.0, 300.0)

      call read_real_var (section_name
     :                    , 'summer_lwg', '()'
     :                    , p%summer_lwg, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'autumn_lwg', '()'
     :                    , p%autumn_lwg, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'winter_lwg', '()'
     :                    , p%winter_lwg, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'spring_lwg', '()'
     :                    , p%spring_lwg, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'leaf_diet', '()'
     :                    , p%leaf_diet, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'std_alw', '()'
     :                    , p%std_alw, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'metabol_expon', '()'
     :                    , p%metabol_expon, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'prop_can_eat', '()'
     :                    , p%prop_can_eat, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'stocking_rate_init', '()'
     :                    , p%stocking_rate_init, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'alw_init', '()'
     :                    , p%alw_init, numvals
     :                    , 0.0, 1000.0)

      call read_integer_var (section_name
     :                    , 'acc_eaten_reset', '()'
     :                    , p%acc_eaten_reset, numvals
     :                    , 0, 366)

      call pop_routine (my_name)
      return
      end subroutine
*     ===========================================================
      real function graz_comp_curve(ndx, a)
*     ===========================================================
      Use infrastructure
      implicit none

*     Short description:
*     Standard competition curve (or at least so McKeon calls it) This
*     function is used by McKeon in several places to transform an index
*     in the range [0-1] to another index in the same range, but
*     weighted in a different way. The weighting is controlled by the a
*     parameter. An a value of 1 leaves the index untransformed.

*   Assumptions:
*       none

*   Notes:
*       none

*   Procedure attributes:
*      Version:         any hardware/fortran77
*      Extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       Use infrastructure

*   Changes:
*     010994 jngh specified and programmed

*   Calls:
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

*   Subroutine arguments
      real ndx, a

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'graz_comp_curve')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      graz_comp_curve = divide(a * ndx, (ndx * (a - 1) + 1), 0.0)

      call pop_routine(my_name)

      return
      end function

      end module GrazModule
      
!     ===========================================================
      subroutine alloc_dealloc_instance(doAllocate)
!     ===========================================================
      use GrazModule
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
      else
         deallocate(g)
         deallocate(p)
         deallocate(c)
      end if
      return
      end subroutine

* ====================================================================
      Subroutine Main (action, data_string)
* ====================================================================
      use GrazModule
      Use infrastructure
      implicit none                                                   
      ml_external Main

*+  Sub-Program Arguments
      character Action*(*)            ! Message action to perform
      character data_string*(*)

*+  Purpose
*      This routine is the interface between the main system and the
*      Graz module.

*+  Changes
*      250894 jngh specified and programmed
*      060596 pdeV upgraded to postbox

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'apsim_Graz')

*- Implementation Section ----------------------------------
      call push_routine(myname)

      if (Action.eq.ACTION_init) then
         call graz_zero_variables ()
            ! Get constants
         call graz_init ()
            ! request and receive variables from owner-modules
         call graz_get_other_variables ()

      else if (Action.eq.ACTION_Process) then
         if (g%stocking_rate .gt. 0.0) then
               ! request and receive variables from owner-modules
            call graz_get_other_variables ()
               ! do processes
            call graz_process ()
               ! send changes to owner-modules
            call graz_set_other_variables ()
         else
            ! no cows...
         endif

      else if (Action.eq.ACTION_Get_variable) then
            ! respond to request for variable values - from modules
         call graz_send_my_variable (Data_string)

      else if (Action .eq. ACTION_Set_variable) then
            ! respond to request to reset variable values - from modules
         call graz_set_my_variable (data_string)

      else
            ! Don't use message
         call Message_unused ()

      endif

      call pop_routine(myname)
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

      
      
      
CPdeV
C Original surfair grasp code:
c$$$C###########################################################################
c$$$C SUBROUTINE GRAZING
c$$$C###########################################################################
c$$$C
c$$$C Inputs:
c$$$C       state:          beasts_ha, green_leaf, green_stem, dead_leaf,
c$$$C                       dead_stem, acc_growth2, acc_eaten, alw
c$$$C       grass_P:        intake_util_intercept, intake_util_slope,
c$$$C                       yld_eat_restr, summer_lwg, autumn_lwg, winter_lwg,
c$$$C                       spring_lwg, leaf_diet
c$$$C       animal_P:       metabol_expon, std_alw
c$$$C       month
c$$$C Outputs:
c$$$C       state:          acc_eaten, alw, green_leaf, green_stem,
c$$$C                       dead_leaf, dead_stem
c$$$C       rate:           pot_lwg, anim_intake, daily_lwg, eaten, green_eaten,
c$$$C                       dead_eaten, green_leaf_eaten, dead_leaf_eaten,
c$$$C                       green_stem_eaten, dead_stem_eaten
c$$$C       ratio:          prop_green, prop_green_leaf, prop_dead_leaf,
c$$$C                       green_diet, prop_consumed, intake_restr, stock_equiv,
c$$$C                       curve_factor
c$$$C
c$$$        subroutine grazing(state, animal_P, grass_P, rate, ratio, month)
c$$$        include 'structures.h'
c$$$        integer month, season(12), SUMMER, AUTUMN, WINTER, SPRING
c$$$        parameter (SUMMER = 1, AUTUMN = 2, WINTER = 3, SPRING = 4)
c$$$        real DAYS_PER_SEASON, MIN_ALW
c$$$        parameter (DAYS_PER_SEASON = 91.25, MIN_ALW = 10.0)
c$$$        real green_pool, dead_pool, tsdm, mod_prop_green, comp_curve
c$$$        data season/1,1,2,2,2,3,3,3,4,4,4,1/
c$$$
c$$$
c$$$        if (state_beasts_ha .gt. 0.0) then
c$$$           green_pool = g%green_leaf + g%green_stem
c$$$           dead_pool = g%dead_leaf + g%dead_stem
c$$$           tsdm = g%green_pool + g%dead_pool
c$$$
c$$$           ratio_prop_green = divide (g%green_pool, tsdm, 0.0)
c$$$           ratio_prop_green_leaf = divide (g%green_leaf,
c$$$     :          g%green_pool, 0.0)
c$$$           ratio_prop_dead_leaf = divide (g%dead_leaf / g%dead_pool
c$$$
c$$$
c$$$           mod_prop_green = (ratio_prop_green - 0.10) / 0.90
c$$$           mod_prop_green = max(0.0, mod_prop_green)
c$$$
c$$$           ratio_green_diet = comp_curve(mod_prop_green, 19.0)
c$$$
c$$$C     If green is less than 10%, no active selection for green by stock
c$$$           if ((mod_prop_green .le. 0.0) .and.
c$$$     &          (green_pool .ge. 0.0)) then
c$$$              ratio_green_diet = ratio_prop_green
c$$$           endif
c$$$
c$$$C           Calculate utilization and its effects on intake
c$$$           ratio_prop_consumed = 0.0
c$$$           if (state_acc_growth2 .gt. 1.0) then
c$$$              ratio_prop_consumed =
c$$$     &             state_acc_eaten / state_acc_growth2
c$$$              ratio_prop_consumed = min(1.0, ratio_prop_consumed)
c$$$           endif
c$$$
c$$$
c$$$            ratio_intake_restr = grass_P_intake_util_intercept +
c$$$     &              ratio_prop_consumed * grass_P_intake_util_slope
c$$$            ratio_intake_restr = min(tsdm/grass_P_yld_eat_restr,
c$$$     &              ratio_intake_restr, 1.0)
c$$$            ratio_intake_restr = max(ratio_intake_restr, 0.0)
c$$$
c$$$
c$$$            if (season(month) .eq. SUMMER) then
c$$$                rate_pot_lwg = grass_P_summer_lwg
c$$$            else if (season(month) .eq. AUTUMN) then
c$$$                rate_pot_lwg = grass_P_autumn_lwg
c$$$            else if (season(month) .eq. WINTER) then
c$$$                rate_pot_lwg = grass_P_winter_lwg
c$$$            else if (season(month) .eq. SPRING) then
c$$$                rate_pot_lwg = grass_P_spring_lwg
c$$$            endif
c$$$
c$$$
c$$$C       This is the animal lwg model. We should do more shit with this
c$$$C       to allow different models here.
c$$$
c$$$C N.B. remember to put in a range check on in pot_lwg parameter inputs
c$$$            rate_anim_intake = ratio_intake_restr *
c$$$     &          (rate_pot_lwg / DAYS_PER_SEASON + 1.058) / 0.304
c$$$
c$$$
c$$$C           Restrict intake such that the herd cannot eat more than is
c$$$C           in sward, then adjust individual animal intake accordingly
c$$$            rate_eaten = ratio_stock_equiv * rate_anim_intake
c$$$            rate_eaten = min(tsdm, rate_eaten)
c$$$            rate_anim_intake = rate_eaten / ratio_stock_equiv
c$$$
c$$$            rate_daily_lwg = rate_anim_intake * 0.304 - 1.058
c$$$C           Restrict the lwg, so that alw never goes below 1kg
c$$$            rate_daily_lwg = max((MIN_ALW - state_alw), rate_daily_lwg)
c$$$C Should formally name and define 0.304 & 1.058.
c$$$
c$$$
c$$$            rate_green_eaten = ratio_green_diet * rate_eaten
c$$$            rate_dead_eaten = (1.0 - ratio_green_diet) * rate_eaten
c$$$
c$$$
c$$$        curve_factor =
c$$$     &          (0.5 * p%leaf_diet - p%leaf_diet) /
c$$$     &          (0.5 * p%leaf_diet - 0.5)
c$$$
c$$$            rate_green_leaf_eaten = rate_green_eaten *
c$$$     &          comp_curve(ratio_prop_green_leaf, ratio_curve_factor)
c$$$
c$$$            rate_dead_leaf_eaten = rate_dead_eaten *
c$$$     &          comp_curve(ratio_prop_dead_leaf, ratio_curve_factor)
c$$$            rate_green_stem_eaten = rate_green_eaten -
c$$$     &          rate_green_leaf_eaten
c$$$            rate_dead_stem_eaten = rate_dead_eaten -
c$$$     &          rate_dead_leaf_eaten
c$$$
c$$$        else
c$$$            rate_eaten = 0.0
c$$$            rate_green_eaten = 0.0
c$$$            rate_dead_eaten = 0.0
c$$$            rate_green_leaf_eaten = 0.0
c$$$            rate_dead_leaf_eaten = 0.0
c$$$            rate_green_stem_eaten = 0.0
c$$$            rate_dead_stem_eaten = 0.0
c$$$            rate_daily_lwg = 0.0
c$$$        endif
c$$$
c$$$        state_acc_eaten = state_acc_eaten + rate_eaten
c$$$        state_alw = state_alw + rate_daily_lwg
c$$$        state_green_leaf = state_green_leaf - rate_green_leaf_eaten
c$$$        state_green_stem = state_green_stem - rate_green_stem_eaten
c$$$        state_dead_leaf = state_dead_leaf - rate_dead_leaf_eaten
c$$$        state_dead_stem = state_dead_stem - rate_dead_stem_eaten
c$$$
c$$$
c$$$        return
c$$$        end
c$$$
