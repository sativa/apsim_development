*     ===========================================================
      character*(*) function sugar_version ()
*     ===========================================================

*   Short description:
*       Return version number of sugar module

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
*       060495 nih taken from template

*   Calls:
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
*       none

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_version')

      character  version_number*(*)    ! version number of module
      parameter (version_number = 'V0.12 230896')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      sugar_version = version_number

      call pop_routine (my_name)
      return
      end
*     ================================================================
      subroutine APSIM_sugar (action, data_string)
*     ================================================================

*   Short description:
*     Sugar module main routine

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
*                       implicit none

*   Changes:
*      250894 jngh specified and programmed
*
*   Calls:
*     get_current_module
*     lastnb
*     set_fatal_off
*     set_fatal_on
*     sugar_version
*     sugar_get_other_variables
*     sugar_init
*     sugar_kill_crop
*     sugar_my_type
*     sugar_process
*     sugar_send_my_variable
*     sugar_set_my_variable
*     sugar_set_other_variables
*     sugar_start_crop
*     sugar_zero_daily_variables
*     sugar_zero_variables
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      character  action*(*)            ! (INPUT) Message action to perform
      character  data_string*(*)       ! (INPUT) Message data

*   Global variables
      include   'sugar.inc'            !
      include   'const.inc'            ! mes_presence, mes_init, mes_process
                                       ! mes_report

      integer    lastnb                ! function
      logical    sugar_my_type         ! function
      character  sugar_version*15      ! function

*   Internal variables
      character  module_name*8         ! module name

*   Constant values
      character  my_name*(*)           ! name of this procedure
      parameter (my_name='sugar')

*   Initial data values
*     none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

         ! initialise error flags
      call set_warning_off ()

      if (action.eq.mes_presence) then      ! report presence
         call get_current_module (module_name)
         write(*, *) 'module_name = '
     :              , module_name(:lastnb(module_name))
     :              // blank
     :              // sugar_version ()

      elseif (action.eq.mes_init) then
            ! Get constants
         call sugar_init ()

      elseif (action.eq.mes_set_variable) then
            ! respond to request to reset variable values - from modules
         call sugar_set_my_variable (data_string)

      elseif (action.eq.mes_get_variable) then
            ! respond to request for variable values - from modules
         call sugar_send_my_variable (Data_string)

      elseif (action.eq.mes_prepare) then
         call sugar_prepare ()

      elseif (action.eq.mes_sow) then
         if (sugar_my_type ()) then
               ! start crop and do  more initialisations
            call sugar_start_crop ()
         else
            ! not my type!
            call message_unused ()
         endif

      elseif (action.eq.mes_process) then
         if (g_crop_status.ne.crop_out) then
               ! do crop processes
            call sugar_process ()
         else
            ! crop not in
            call sugar_zero_variables ()
         endif
      elseif (action.eq.mes_harvest) then
         if (sugar_my_type ()) then
               ! harvest crop - turn into residue
            call sugar_harvest ()
         else
            ! not my type!
            call message_unused ()
         endif

      elseif (action.eq.mes_end_crop) then
         if (sugar_my_type ()) then
               ! end crop - turn into residue
            call sugar_end_crop ()
         else
            ! not my type!
            call message_unused ()
         endif

      elseif (action.eq.mes_kill_crop) then
         if (sugar_my_type ()) then
               ! kill crop - die
            call sugar_kill_crop ()
         else
            ! not my type!
            call message_unused ()
         endif
      else
               ! don't use message
         call message_unused ()
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_process ()
*     ===========================================================

*   Short description:
*       Simulate crop processes.  These include biomass production,
*       phenological stages, plant component development,
*       water uptake and nitrogen uptake, and plant senescense.

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
*                       implicit none

*   Changes:
*      060495 nih taken from template

*   Calls:
*     sugar_biomass
*     sugar_crop_totals
*     sugar_event
*     sugar_leaf_area
*     sugar_leaf_area_potential
*     sugar_Nitrogen
*     sugar_phenology
*     sugar_plant_death
*     sugar_senescence
*     sugar_transpiration
*     sugar_update
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'sugar.inc'

*   Internal variables

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_process')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      ! request and receive variables from owner-modules
      call sugar_get_other_variables ()

      call sugar_transpiration ()

      if (g_crop_status.eq.crop_alive) then
         call sugar_biomass ()
         call sugar_leaf_area ()
         call sugar_root_length_growth ()
         call sugar_senescence ()
         call sugar_Nitrogen ()
         call sugar_water_content()
         call sugar_plant_death ()

      else
      endif

      call sugar_detachment ()
      call sugar_update ()
      call sugar_crop_totals ()
      call sugar_event ()

      ! send changes to owner-modules
      call sugar_set_other_variables ()

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_phenology ()
*     ===========================================================

*   Short description:
*     Use temperature, photoperiod and genetic characteristics
*     to determine when the crop begins a new growth phase.
*     The initial daily thermal time and height are also set.

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
*                       implicit none

*   Changes:
*     060495 nih taken from template

*   Calls:
*     accumulate
*     pop_routine
*     push_routine
*     sugar_canopy_height
*     sugar_devel
*     sugar_phenology_init
*     sugar_tt
*     sugar_tt_curv

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'sugar.inc'

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_phenology')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      g_previous_stage = g_current_stage

         ! get thermal times

      call sugar_tt (g_dlt_tt)


         ! initialise phenology phase targets

      call sugar_phenology_init (g_phase_tt)
      call sugar_devel (g_dlt_stage, g_current_stage)

         ! update thermal time states and day count

      call accumulate (g_dlt_tt, g_tt_tot
     :               , g_previous_stage, g_dlt_stage)

      call accumulate (1.0, g_days_tot
     :               , g_previous_stage, g_dlt_stage)

          ! canopy height

      call sugar_canopy_height (g_dlt_canopy_height)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_tt (dlt_tt)
*     ===========================================================

*   Short description:
*     Growing degree day (thermal time) is calculated.

*   Assumptions:
*       none

*   Notes:
*     Eight interpolations of the air temperature are
*     calculated using a three-hour correction factor.
*     For each air three-hour air temperature, a value of growing
*     degree day is calculated.  The eight three-hour estimates
*     are then averaged to obtain the daily value of growing degree
*     days.

*   Procedure attributes:
*      Version:         Any hardware/fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       Implicit none

*   Changes:
*       070495 nih taken from template

*   Calls:
*     linint_3hrly_temp
*     pop_routine
*     push_routine
*     real
*     sugar_swdef
*     stage_is_between

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       dlt_tt                ! (OUTPUT) daily thermal time (oC)
*   Global variables
      include    'sugar.inc'

      real       linint_3hrly_temp     ! function
c      real       sugar_swdef           ! function
c      logical    stage_is_between      ! function

*   Internal variables
      real       dly_therm_time        ! thermal time for the day (deg day)

*   Constant values

      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_tt')

*   Initial data values
*     none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      dly_therm_time = linint_3hrly_temp (g_maxt, g_mint
     :                 , c_x_temp, c_y_tt
     :                 , c_num_temp)

c      if (stage_is_between (emerg, flag_leaf, g_current_stage)) then

c         dlt_tt = dly_therm_time *sugar_swdef (pheno)
c      else

         dlt_tt = dly_therm_time
c      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_phenology_init (phase_tt)
*     ===========================================================

*   Short description:
*       Returns cumulative thermal time targets required for the
*       individual growth stages.

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
*                       implicit none

*   Changes:
*     060495 nih taken from template

*   Calls:
*     bound
*     day_length
*     offset_day_of_year
*     on_day_of
*     pop_routine
*     push_routine
*     stage_is_between
*     sum_between

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       phase_tt (*)          ! (INPUT/OUTPUT) cumulative growing
                                       ! degree days required for
                                       ! each stage (deg days)

*   Global variables
      include   'sugar.inc'

      logical    on_day_of             ! function

*   Internal variables

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_phenology_init')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      if (on_day_of (sprouting, g_current_stage, g_days_tot)) then
         phase_tt(sprouting_to_emerg) = c_shoot_lag
     :                                + g_sowing_depth*c_shoot_rate

      elseif (on_day_of (emerg, g_current_stage, g_days_tot)) then
         phase_tt(emerg_to_begcane) = p_tt_emerg_to_begcane

      elseif (on_day_of (begcane, g_current_stage, g_days_tot)) then
         phase_tt(begcane_to_flowering) = p_tt_begcane_to_flowering

      elseif (on_day_of (flowering, g_current_stage, g_days_tot)) then
         phase_tt(flowering_to_crop_end) = p_tt_flowering_to_crop_end

      else
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_devel (dlt_stage, current_stage)
*     ===========================================================

*   Short description:
*     Determine the curent stage of development.

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
*                       implicit none

*   Changes:
*     060495 nih taken from template

*   Calls:
*     pop_routine
*     push_routine
*     sugar_phase_devel

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       dlt_stage             ! (OUTPUT) change in growth stage
      real       current_stage         ! (OUTPUT) new stage no.

*   Global variables
      include   'sugar.inc'

*   Internal variables
      real       new_stage             ! new stage number
      real       phase_devel           ! fraction of current phase elapsed ()

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_devel')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

         ! mechanical operation - not to be changed

         ! now calculate the new delta and the new stage

      call sugar_phase_devel (phase_devel)
      new_stage = aint (g_current_stage) + phase_devel
      dlt_stage = new_stage - g_current_stage

      if (phase_devel.ge.1.0) then
         current_stage = aint (new_stage)

      else
         current_stage = new_stage

      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_phase_devel (phase_devel)
*     ===========================================================

*   Short description:
*     Determine the fraction of current phase elapsed ().

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
*                       implicit none

*   Changes:
*     060495 nih taken from template

*   Calls:
*     pop_routine
*     push_routine
*     sugar_sprouting
*     sugar_phase_tt
*     stage_is_between

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       phase_devel           ! (OUTPUT) fraction of current phase
                                       ! elapsed ()

*   Global variables
      include   'sugar.inc'

      real       sugar_sprouting       ! function
      real       sugar_phase_tt        ! function
      logical    stage_is_between      ! function

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_phase_devel')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      if (stage_is_between (sowing, sprouting, g_current_stage)) then
         phase_devel = sugar_sprouting (g_current_stage)

      elseif (stage_is_between (sprouting, crop_end
     :                        , g_current_stage)) then

         phase_devel =  sugar_phase_tt (g_current_stage)

      else
         phase_devel = 0.0

      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      real function sugar_phase_tt (stage_no)
*     ===========================================================

*   Short description:
*       Return fraction of thermal time we are through the current
*       phenological phase (0-1)

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
*                       implicit none

*   Changes:
*     060495 nih taken from template

*   Calls:
*     bound
*     divide
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       stage_no              ! (INPUT) stage number

*   Global variables
      include   'sugar.inc'

      real       divide                ! function
      real       bound                 ! function

*   Internal variables
      integer    phase                 ! phase number containing stage

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_phase_tt')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      phase = int (stage_no)
      sugar_phase_tt = divide (g_tt_tot(phase) + g_dlt_tt
     :                       , g_phase_tt(phase), 0.0)
      sugar_phase_tt = bound (sugar_phase_tt, 0.0, 1.999999)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_canopy_height (dlt_canopy_height)
*     ===========================================================

*   Short description:
*       Get change in plant canopy height

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
*                       implicit none

*   Changes:
*       060495 nih taken from template

*   Calls:
*     bound
*     divide
*     pop_routine
*     push_routine
*     stage_is_between

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       dlt_canopy_height     ! (INPUT) canopy height change (mm)

*   Global variables
      include   'sugar.inc'

      real       bound                 ! function
      real       divide                ! function
      logical    stage_is_between      ! function

*   Internal variables
      real       dm_stem_plant         ! dry matter of stem (g/plant)
      real       pot_height            ! potential height (mm)

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_canopy_height')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      if (stage_is_between (emerg, flowering
     :                    , g_current_stage)) then

         dm_stem_plant = divide (g_dm_green(sstem), g_plants, 0.0)
         pot_height = c_height_stem_slope * dm_stem_plant
         pot_height = bound (pot_height, 0.0, c_height_max)
         dlt_canopy_height = pot_height - g_canopy_height

      else
         dlt_canopy_height = 0.0

      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_transpiration ()
*     ===========================================================

*   Short description:
*       Plant transpiration and soil water extraction

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
*                       implicit none

*   Changes:
*      060495 nih taken from template

*   Calls:
*     sugar_check_sw
*     sugar_root_depth
*     sugar_sw_avail
*     sugar_sw_avail_pot
*     sugar_sw_demand
*     sugar_sw_supply
*     sugar_sw_uptake
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'const.inc'
      include   'sugar.inc'

*   Internal variables
      integer layer

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_transpiration')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      call sugar_root_depth (g_dlt_root_depth)   ! increase in root depth

            ! WATER UPTAKE
      call sugar_check_sw ()
      call sugar_sw_avail_pot (g_sw_avail_pot) ! potential extractable sw (dul-l
      call sugar_sw_avail (g_sw_avail)       ! actual extractable sw (sw-ll)
cnh moved sw_demand to prepare stage
!      call sugar_sw_demand (g_sw_demand)

      if (g_uptake_source.eq.'calc') then
         call sugar_sw_supply (g_sw_supply)
         call sugar_sw_uptake (g_dlt_sw_dep)
      else
         If (g_num_uptake_water.gt.0) then
            do 100 layer = 1, g_num_uptake_water
               g_sw_supply (layer) =        g_uptake_water(layer)
               g_dlt_sw_dep(layer) = -1.0 * g_uptake_water(layer)
  100       continue
         else
            call Fatal_Error (Err_Internal,
     :          'No soil water uptake information has been provided')
         endif
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_root_depth (dlt_root_depth)
*     ===========================================================

*   Short description:
*       Return the increase in root depth (mm)

*   Assumptions:
*       none

*   Notes:
*         there is a discrepency when the root crosses into another
*         layer. - cr380

*   Procedure attributes:
*      Version:         any hardware/fortran77
*      Extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       implicit none

*   Changes:
*      060495 nih taken from template
*      041095 nih change init of root depth to sprouting (was emergence)
*      200396 nih changed max root depth to deepest xf>0

*   Calls:
*     find_layer_no
*     on_day_of
*     pop_routine
*     push_routine
*     sugar_sw_avail_fac
*     stage_is_between
*     sum_between
*     sum_real_array
*     u_bound


* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       dlt_root_depth        ! (OUTPUT) increase in root depth (mm)

*   Global variables
      include   'sugar.inc'

      real       u_bound               ! function
      integer    count_of_real_vals    ! function
      integer    find_layer_no         ! function
      real       sugar_sw_avail_fac    ! function
      logical    on_day_of             ! function
      logical    stage_is_between      ! function
      real       sum_real_array        ! function

*   Internal variables
      integer    current_phase         ! current phase number
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      integer    num_root_layers       !
      real       root_depth_max        ! maximum depth to which roots can
                                       ! go (mm)
      integer    root_layer_max        ! deepest layer that roots can grow
                                       ! into.
*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_root_depth')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      if (on_day_of (sprouting, g_current_stage, g_days_tot)) then

             ! initialise root depth
             ! this version does not take account of sowing depth.
cnh it used to do this on first day of sprouting
cnh         dlt_root_depth = c_initial_root_depth

cnh now I say roots are at bottom of deepest layer that user said had a value
cnh for rlv at initialisation.
            num_root_layers = count_of_real_vals (g_rlv,max_layer)
            dlt_root_depth =
     :                 sum_real_array (g_dlayer, num_root_layers)
     :                 - g_root_depth

      elseif (stage_is_between (emerg, flowering
     :                        , g_current_stage)) then
            ! we have root growth (in a vegetative phase)

            ! this equation allows soil water in the deepest
            ! layer in which roots are growing
            ! to affect the daily increase in rooting depth.

            ! Root extension rate factor is used to slow roots in layers
            ! of heavier soil.

         deepest_layer = find_layer_no (g_root_depth, g_dlayer
     :                                , max_layer)
         current_phase = int (g_current_stage)
         dlt_root_depth  = c_root_depth_rate(current_phase)
     :                   * sugar_sw_avail_fac (deepest_layer)
     :                   * p_xf (deepest_layer)

            ! constrain it by the maximum
            ! depth that roots are allowed to grow.

cnh         root_depth_max = sum_real_array (g_dlayer, g_num_layers)

         root_layer_max = count_of_real_vals (p_xf,max_layer)
         root_depth_max = sum_real_array (g_dlayer, root_layer_max)

         dlt_root_depth = u_bound (dlt_root_depth
     :                           , root_depth_max - g_root_depth)

      else  ! we have no root growth

         dlt_root_depth = 0.0
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      real function sugar_sw_avail_fac (layer)
*     ===========================================================

*   Short description:
*      Get the soil water availability factor in a layer.  For a layer,
*      it is 1.0 unless the plant-extractable soil water declines
*      below a fraction of plant-extractable soil water capacity for
*      that layer.

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
*                       implicit none

*   Changes:
*     060495 nih taken from template

*   Calls:
*     divide
*     linear_interp_real
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer    layer                 ! (INPUT) soil profile layer number

*   Global variables
      include   'sugar.inc'

      real       divide                ! function
      real       linear_interp_real    ! function

*   Internal variables
      real       pesw                  ! plant extractable soil-water (mm/mm)
      real       pesw_capacity         ! plant extractable soil-water capacity
                                       ! (mm/mm)
      real       sw_avail_ratio        ! soil water availability ratio (0-1)

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_sw_avail_fac')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      pesw = g_sw_dep(layer) - p_ll_dep(layer)
      pesw_capacity = g_dul_dep(layer) - p_ll_dep(layer)

      sw_avail_ratio = divide (pesw, pesw_capacity, 10.0)
      sugar_sw_avail_fac = linear_interp_real (sw_avail_ratio
     :                           , c_x_sw_ratio, c_y_sw_fac_root
     :                           , c_num_sw_ratio)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_sw_avail_pot (sw_avail_pot)
*     ===========================================================

*   Short description:
*       Return potential water uptake from each layer in the soil profile
*       by the crop (mm water) from a fully wet profile

*   Assumptions:
*       none

*   Notes:
*       see cr474 for limitations and potential problems.

*   Procedure attributes:
*      Version:         any hardware/fortran77
*      Extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       implicit none

*   Changes:
*       060495 nih taken from template

*   Calls:
*     fill_real_array
*     find_layer_no
*     pop_routine
*     push_routine
*     root_proportion

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       sw_avail_pot(*)       ! (OUTPUT) crop water potential uptake
                                       ! for each full layer (mm)

*   Global variables
      include   'sugar.inc'

      integer    find_layer_no         ! function
      real       root_proportion       ! function

*   Internal variables
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      integer    layer                 ! soil profile layer number

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_sw_avail_pot')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

            ! get potential uptake

      call fill_real_array (sw_avail_pot, 0.0, max_layer)

      deepest_layer = find_layer_no (g_root_depth, g_dlayer, max_layer)
      do 1000 layer = 1, deepest_layer
         sw_avail_pot(layer) = g_dul_dep(layer) - p_ll_dep(layer)
1000  continue

            ! correct bottom layer for actual root penetration
      sw_avail_pot(deepest_layer) = sw_avail_pot(deepest_layer)
     :                            * root_proportion
     :                            (deepest_layer
     :                           , g_dlayer
     :                           , g_root_depth)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_sw_avail (sw_avail)
*     ===========================================================

*   Short description:
*       Return actual water available for extraction from each layer in the
*       soil profile by the crop (mm water)

*   Assumptions:
*       none

*   Notes:
*       see cr474 for limitations and potential problems.

*   Procedure attributes:
*      Version:         any hardware/fortran77
*      Extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       implicit none

*   Changes:
*       060495 nih taken from template

*   Calls:
*     fill_real_array
*     find_layer_no
*     l_bound
*     pop_routine
*     push_routine
*     root_proportion

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       sw_avail(*)           ! (OUTPUT) crop water potential uptake
                                       ! for each full layer (mm)

*   Global variables
      include   'sugar.inc'

      integer    find_layer_no         ! function
      real       l_bound               ! function
      real       root_proportion       ! function

*   Internal variables
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      integer    layer                 ! soil profile layer number

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_sw_avail')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

            ! get potential uptake

      call fill_real_array (sw_avail, 0.0, max_layer)

      deepest_layer = find_layer_no (g_root_depth, g_dlayer, max_layer)
      do 1000 layer = 1, deepest_layer
         sw_avail(layer) = g_sw_dep(layer) - p_ll_dep(layer)
         sw_avail(layer) = l_bound (sw_avail(layer), 0.0)
1000  continue

            ! correct bottom layer for actual root penetration
      sw_avail(deepest_layer) = sw_avail(deepest_layer)
     :                        * root_proportion
     :                         (deepest_layer, g_dlayer, g_root_depth)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_sw_demand (sw_demand)
*     ===========================================================

*   Short description:
*       Return crop water demand from soil by the crop (mm)

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
*                       implicit none

*   Changes:
*       060495 nih taken from template

*   Calls:
*     divide
*     exp
*     pop_routine
*     push_routine
*     sugar_dm_potential
*     sugar_transp_eff

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       sw_demand             ! (OUTPUT) crop water demand (mm)

*   Global variables
      include   'sugar.inc'

      real       divide                ! function
      real       sugar_transp_eff      ! function

*   Internal variables
      real       dlt_dm_pot            ! potential dry matter production with
                                       ! optimum water and nitrogen and
                                       ! temperature stress conditions (g/m^2)

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_sw_demand')

*   Initial data values
*     none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

            ! calculate potential above-ground biomass production

      call sugar_dm_potential (dlt_dm_pot)

            ! get potential transpiration from potential
            ! carbohydrate production and transpiration efficiency

      sw_demand = divide (dlt_dm_pot, sugar_transp_eff (), 0.0)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_dm_potential (dlt_dm_pot)
*     ===========================================================

*   Short description:
*       Potential biomass (carbohydrate) production from
*       photosynthesis (g/m^2)

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
*                       implicit none

*   Changes:
*       060495 nih taken from template

*   Calls:
*     pop_routine
*     push_routine
*     sugar_radn_int
*     sugar_rue_reduction

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       dlt_dm_pot            ! (OUTPUT) potential dry matter
                                       ! (carbohydrate) production (g/m^2)

*   Global variables
      include   'sugar.inc'

      real       sugar_rue_reduction   ! function

*   Internal variables
      integer    current_phase         ! current phase number
      real       rue                   ! radiation use efficiency under
                                       ! no stress (g biomass/mj)
      real       radn_int              ! radn intercepted by leaves (mj/m^2)

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_dm_potential')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      current_phase = int (g_current_stage)
      rue = c_rue(current_phase) * sugar_rue_reduction ()

         ! potential dry matter production with temperature
         ! and N content stresses is calculated.
         ! This is g of dry biomass produced per MJ of intercepted
         ! radiation under stressed conditions.

      call sugar_radn_int (radn_int)
      dlt_dm_pot = rue * radn_int

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      real function sugar_rue_reduction ()
*     ===========================================================

*   Short description:
*       Effect of non-optimal N and Temp conditions on RUE

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
*                       implicit none

*   Changes:
*       060495 nih taken from template

*   Calls:
*     bound
*     linear_interp_real
*     min
*     pop_routine
*     push_routine
*     sugar_nfact

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*     none

*   Global variables
      include   'sugar.inc'

      real       bound                 ! function
      real       linear_interp_real    ! function
      real       sugar_nfact           ! function
      real       sugar_water_log_fact  ! function
      
*   Internal variables
      real       temp_stress_photo     ! photosynthetic reduction factor for
                                       ! temperature stress (0-1)
      real       ave_temp              ! mean temperature for the day (oC)

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_rue_reduction')

*   Initial data values

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

         ! now get the temperature stress factor that reduces
         ! photosynthesis (0-1)

      ave_temp = (g_maxt + g_mint) /2.0

      temp_stress_photo = linear_interp_real (ave_temp
     :                          , c_x_ave_temp, c_y_stress_photo
     :                          , c_num_ave_temp)
      temp_stress_photo = bound (temp_stress_photo, 0.0, 1.0)

      sugar_rue_reduction = min (temp_stress_photo
     :                      , sugar_nfact (photo)
     :                      ,sugar_water_log_fact())

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_radn_int (radn_int)
*     ===========================================================

*   Short description:
*       Radiation intercepted by leaves (mj/m^2)

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
*                       implicit none

*   Changes:
*     060495 nih taken from template

*   Calls:
*     exp
*     max
*     pop_routine
*     push_routine
*     reals_are_equal

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       radn_int              ! (OUTPUT) radiation intercepted
                                       ! by leaves (mj/m^2)

*   Global variables
      include   'sugar.inc'

      logical    reals_are_equal       ! function

*   Internal variables
      real       cover                 ! fraction of radn that is intercepted
                                       ! by leaves (0-1) (m^2/m^2)

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_radn_int')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      if (reals_are_equal (g_fr_intc_radn, 0.0)) then
            ! we need to calculate our own interception

            ! this equation implies that leaf interception of
            ! solar radiation obeys Beer's law

         cover = 1.0 - exp (-c_extinction_coef*g_lai)
         radn_int = cover * g_radn

      else
            ! interception has already been calculated for us
         radn_int = g_fr_intc_radn * g_radn
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      real function sugar_transp_eff ()
*     ===========================================================

*   Short description:
*       Calculate today's transpiration efficiency from min and max
*       temperatures and converting mm water to g dry matter
*       (g dm/m^2/mm water)

*   Assumptions:
*       the temperatures are > -237.3 oC for the svp function.

*   Notes:
*       Average saturation vapour pressure for ambient temperature
*       during transpiration is calculated as part-way between that
*       for minimum temperature and that for the maximum temperature.
*       Tanner & Sinclair (1983) used .75 and .67 of the distance as
*       representative of the positive net radiation (rn).  Daily SVP
*       should be integrated from about 0900 hours to evening when Radn
*       becomes negetive.

*   Procedure attributes:
*      Version:         any hardware/fortran77
*      Extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       implicit none

*   Changes:
*       060495 nih taken from template

*   Calls:
*     divide
*     exp
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*     none

*   Global variables
      include   'convert.inc'          ! g2mm, mb2kpa
      include   'sugar.inc'

      real       divide                ! function

*   Internal variables
      real       svp                   ! function to get saturation vapour
                                       ! pressure for a given temperature
                                       ! in oC (kpa)
      real       temp_arg              ! dummy temperature for function (oC)
      real       vpd                   ! vapour pressure deficit (kpa)

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_transp_eff')

*   Initial data values
      ! set up saturation vapour pressure function

      svp(temp_arg) = 6.1078
     :              * exp (17.269*temp_arg/ (237.3 + temp_arg))
     :              * mb2kpa


* --------------------- Executable code section ----------------------

      call push_routine (my_name)

            ! get vapour pressure deficit when net radiation is positive.

      vpd = c_svp_fract* (svp (g_maxt) - svp (g_mint))
      sugar_transp_eff = divide (c_transp_eff_cf, vpd, 0.0) /g2mm

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_sw_supply (sw_supply)
*     ===========================================================

*   Short description:
*       Return potential water uptake from each layer of the soil profile
*       by the crop (mm water). This represents the maximum amount in each
*       layer regardless of lateral root distribution but takes account of
*       root depth in bottom layer.

*   Assumptions:
*       none

*   Notes:
*      This code still allows water above dul to be taken - cnh

*   Procedure attributes:
*      Version:         any hardware/fortran77
*      Extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       implicit none

*   Changes:
*     060495 nih taken from template

*   Calls:
*     fill_real_array
*     find_layer_no
*     l_bound
*     pop_routine
*     push_routine
*     root_proportion

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       sw_supply(*)          ! (OUTPUT) potential crop water uptake
                                       ! from each layer (mm) (supply to roots)

*   Global variables
      include   'sugar.inc'

      integer    find_layer_no         ! function
      real       root_proportion       ! function
      real       l_bound               ! function

*   Internal variables
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      integer    layer                 ! soil profile layer number
      real       sw_avail              ! water available (mm)

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_sw_supply')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

            ! get potential uptake

      call fill_real_array (sw_supply, 0.0, max_layer)

      deepest_layer = find_layer_no (g_root_depth, g_dlayer, max_layer)
      do 1000 layer = 1, deepest_layer
         sw_avail = (g_sw_dep(layer) - p_ll_dep(layer))
         sw_supply(layer) = sw_avail * p_kl(layer)
         sw_supply(layer) = l_bound (sw_supply(layer), 0.0)

1000  continue

            ! now adjust bottom layer for depth of root
      sw_supply(deepest_layer) = sw_supply(deepest_layer)
     :                         * root_proportion
     :                          (deepest_layer, g_dlayer, g_root_depth)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      real function sugar_swdef (type)
*     ===========================================================

*   Short description:
*       Get the soil water availability factor (0-1), commonly
*       called soil water deficit factor. 1 is no stress, 0 is full stress.

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
*                       implicit none

*   Changes:
*       010994 jngh specified and programmed

*   Calls:
*     bound
*     divide
*     fatal_error
*     find_layer_no
*     linear_interp_real
*     pop_routine
*     push_routine
*     sum_real_array

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer    type                  ! (INPUT) factor type

*   Global variables
      include   'const.inc'
      include   'sugar.inc'

      real       bound                 ! function
      real       divide                ! function
      integer    find_layer_no         ! function
      real       linear_interp_real    ! function
      real       sum_real_array        ! function

*   Internal variables
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      character  err_messg*100         ! error message
      real       sw_avail_ratio        ! water availability ratio
      real       sw_demand_ratio       ! water supply:demand ratio
      real       sw_avail_pot_sum      ! potential extractable soil water (mm)
      real       sw_avail_sum          ! actual extractable soil water (mm)
      real       sw_supply_sum         ! total supply over profile (mm)

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_swdef')

*   Initial data values
*       none
* --------------------- Executable code section ----------------------

      call push_routine (my_name)
      deepest_layer = find_layer_no (g_root_depth, g_dlayer, max_layer)

      if (type.eq.pheno) then
         sw_avail_pot_sum = sum_real_array (g_sw_avail_pot
     :                                    , deepest_layer)
         sw_avail_sum = sum_real_array (g_sw_avail, deepest_layer)

         sw_avail_ratio = divide (sw_avail_sum
     :                          , sw_avail_pot_sum, 1.0) !???
         sw_avail_ratio = bound (sw_avail_ratio , 0.0, 1.0)

         sugar_swdef = linear_interp_real (sw_avail_ratio
     :                       , c_x_sw_avail_ratio, c_y_swdef_pheno
     :                       , c_num_sw_avail_ratio)

      elseif (type.eq.photo) then
            ! get potential water that can be taken up when profile is full

         sw_supply_sum = sum_real_array (g_sw_supply, deepest_layer)
         sw_demand_ratio = divide (sw_supply_sum, g_sw_demand, 1.0)
         sugar_swdef = bound (sw_demand_ratio , 0.0, 1.0)

      elseif (type.eq.expansion) then
            ! get potential water that can be taken up when profile is full

         sw_supply_sum = sum_real_array (g_sw_supply, deepest_layer)
         sw_demand_ratio = divide (sw_supply_sum, g_sw_demand, 10.0)

         sugar_swdef = linear_interp_real (sw_demand_ratio
     :                       , c_x_sw_demand_ratio, c_y_swdef_leaf
     :                       , c_num_sw_demand_ratio)


      else
         ! we have an unknown type
         write (err_messg, '(a, i5)')
     :                   ' Unknown type of sw deficit. Type ='
     :                   , type
         call fatal_error (err_internal, err_messg)
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_sw_uptake (dlt_sw_dep)
*     ===========================================================

*   Short description:
*       Returns actual water uptake from each layer of the soil
*       profile by the crop (mm).

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
*                       implicit none

*   Changes:
*       060495 nih taken from template

*   Calls:
*     divide
*     fill_real_array
*     find_layer_no
*     pop_routine
*     push_routine
*     sum_real_array

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       dlt_sw_dep (*)        ! (OUTPUT) root water uptake (mm)

*   Global variables
      include   'sugar.inc'

      real       divide                ! function
      integer    find_layer_no         ! function
      real       sum_real_array        ! function

*   Internal variables
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      integer    layer                 ! layer number of profile ()
      real       sw_supply_sum         ! total potential over profile (mm)

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_sw_uptake')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)


            ! find total root water potential uptake as sum of all layers

      deepest_layer = find_layer_no (g_root_depth, g_dlayer, max_layer)
      sw_supply_sum = sum_real_array (g_sw_supply, deepest_layer)

      if (sw_supply_sum.le.0.0 .or. g_sw_demand.le.0.0) then
            ! we have no uptake - there is no demand or potential

         call fill_real_array (dlt_sw_dep, 0.0, max_layer)

      else
               ! get actual uptake

         call fill_real_array (dlt_sw_dep, 0.0, max_layer)

         if (g_sw_demand.lt.sw_supply_sum) then

               ! demand is less than what roots could take up.
               ! water is non-limiting.
               ! distribute demand proportionately in all layers.

            do 1000 layer = 1, deepest_layer
               dlt_sw_dep(layer) = - divide (g_sw_supply(layer)
     :                                    , sw_supply_sum, 0.0)
     :                            * g_sw_demand

1000        continue

         else
                ! water is limiting - not enough to meet demand so take
                ! what is available (potential)

            do 1100 layer = 1, deepest_layer
               dlt_sw_dep(layer) = - g_sw_supply(layer)

1100        continue

         endif
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_leaf_area_potential ()
*     ===========================================================

*   Short description:
*       Simulate potential crop leaf area development - may be limited by
*       DM production in subsequent routine

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
*                       implicit none

*   Changes:
*      070495 nih taken from template

*   Calls:
*     sugar_leaf_appearance
*     sugar_leaf_area_devel
*     sugar_leaf_area_devel_plant
*     sugar_leaf_area_init
*     sugar_leaf_no_final
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'sugar.inc'

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_leaf_area_potential')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

            ! Plant leaf development
            ! initialise total leaf number
      call sugar_leaf_area_init (g_lai)
      call sugar_leaf_appearance (g_dlt_leaf_no) ! fraction of leaf emerged
      call sugar_leaf_area_devel (g_dlt_lai_pot) ! individual leaf approach

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_leaf_area_init (lai)
*     ===========================================================

*   Short description:
*       Initialise leaf area.

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
*                       implicit none

*   Changes:
*     070495 nih taken from template

*   Calls:
*     on_day_of
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       lai                   ! (OUTPUT) total plant leaf area

*   Global variables
      include   'convert.inc'
      include   'sugar.inc'

      logical    on_day_of             ! function

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_leaf_area_init')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      if (on_day_of (emerg, g_current_stage, g_days_tot)) then
         lai = c_initial_tpla * smm2sm * g_plants
      else
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_leaf_appearance (dlt_leaf_no)
*     ===========================================================

*   Short description:
*       Return the fractional increase in emergence of the oldest
*       expanding leaf.
*       Note ! this does not take account of the other younger leaves
*       that are currently expanding

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
*                       implicit none

*   Changes:
*       070495 nih taken from template

*   Calls:
*     bound
*     divide
*     on_day_of
*     pop_routine
*     push_routine
*     sum_between

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       dlt_leaf_no           ! (OUTPUT) new fraction of oldest
                                       ! expanding leaf

*   Global variables
      include   'sugar.inc'

      real       bound                 ! function
      real       divide                ! function
      real       linear_interp_real    ! function
      logical    on_day_of             ! function
      logical    stage_is_between      ! function
      real       sum_between           ! function

*   Internal variables
      real       leaf_app_rate
      real       leaf_no_remaining     ! number of leaves to go before all
                                       ! are fully expanded
      real       leaf_no_today

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_leaf_appearance')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      if (on_day_of (emerg, g_current_stage, g_days_tot)) then

             ! initialise first leaf

         dlt_leaf_no = c_leaf_no_at_emerg

      elseif (stage_is_between (emerg, flowering, g_current_stage)) then

             ! we  haven't reached full number of leaves yet

             ! if leaves are still growing, the cumulative number of
             ! phyllochrons or fully expanded leaves is calculated from
             ! daily thermal time for the day.

         leaf_no_today = sum_between (emerg, now, g_leaf_no)
         leaf_no_remaining = max_leaf - leaf_no_today

         leaf_app_rate = linear_interp_real
     :                     (
     :                      leaf_no_today
     :                     ,c_leaf_app_rate_lfno
     :                     ,c_leaf_app_rate
     :                     ,c_num_leaf_app_rate
     :                     )

         dlt_leaf_no = divide (g_dlt_tt, leaf_app_rate, 0.0)
         dlt_leaf_no = bound (dlt_leaf_no, 0.0, leaf_no_remaining)

      else
             ! start no more leaves

         dlt_leaf_no = 0.0
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_leaf_area_devel (dlt_lai_pot)
*     ===========================================================

*   Short description:
*       Return the potential increase in leaf area development (mm^2)
*       calculated on an individual leaf basis.

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
*                       implicit none

*   Changes:
*     070495 nih taken from template
*     120196 nih made this really a potential dlt_lai by removing
*                stress factors.  g_dlt_lai_pot can now be used
*                in different places and stress applied only when
*                required.

*   Calls:
*     min
*     pop_routine
*     push_routine
*     sugar_leaf_size
*     sugar_swdef
*     sum_between
cbak
*     sugar_nfact

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       dlt_lai_pot           ! (OUTPUT) change in leaf area

*   Global variables
      include   'convert.inc'
      include   'sugar.inc'

      real       sugar_leaf_size       ! function
cnh      real       sugar_swdef           ! function
cbak
cnh      real       sugar_nfact           ! function
      real       sum_between           ! function

*   Internal variables
      real       area                  ! potential maximum area of oldest
                                       ! expanding leaf (mm^2) in today's
                                       ! conditions
      real       leaf_no_effective     ! effective leaf no - includes
                                       ! younger leaves that have emerged
                                       ! after the current one

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_leaf_area_devel')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

           ! once leaf no is calculated leaf area of largest expanding leaf
           ! is determined

      leaf_no_effective = sum_between (emerg, now, g_leaf_no)
     :                  + c_leaf_no_correction
      area = sugar_leaf_size (leaf_no_effective)

cbak

      dlt_lai_pot = g_dlt_leaf_no * area * smm2sm * g_plants
cnh     :            * min (sugar_swdef(expansion), sugar_nfact(expansion))


      call pop_routine (my_name)
      return
      end
*     ===========================================================
      real function sugar_leaf_size (leaf_no)
*     ===========================================================

*   Short description:
*       Return the leaf area (mm^2) of a specified leaf no.

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
*                       implicit none

*   Changes:
*       070495 nih taken from template

*   Calls:
*     divide
*     min
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       leaf_no               ! (INPUT) nominated leaf number

*   Global variables
      include   'sugar.inc'

      real       linear_interp_real    ! function

*   Internal variables
      real leaf_size
      real tiller_factor

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_leaf_size')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      leaf_size = linear_interp_real
     :                     (real(leaf_no)
     :                     ,c_leaf_size_no
     :                     ,c_leaf_size
     :                     ,c_num_leaf_size
     :                     )

      tiller_factor = linear_interp_real
     :                     (real(leaf_no)
     :                     ,c_tillerf_leaf_size_no
     :                     ,c_tillerf_leaf_size
     :                     ,c_num_tillerf_leaf_size
     :                     )

      sugar_leaf_size = leaf_size * tiller_Factor

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_leaf_area ()
*     ===========================================================

*   Short description:
*       Simulate actual crop leaf area development - checks that leaf area
*       development matches DM production.

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
*                       implicit none

*   Changes:
*      070495 nih taken from template

*   Calls:
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'convert.inc'
      include   'sugar.inc'

      real       u_bound               ! function
      real       sugar_nfact           ! function
      real       sugar_sla_max         ! function
      real       sugar_swdef           ! function
      real       sum_between           ! function

*   Internal variables
      real leaf_no_today      ! total number of leaves today
      real optfr              ! fraction of optimum conditions(0-1)
      real sla_max            ! maximum allowable specific leaf
                              ! area (cm2/g)

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_leaf_area')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

         ! limit the delta leaf area by carbon supply
         ! and stress factors

      leaf_no_today = sum_between (emerg, now, g_leaf_no)
     :              + g_dlt_leaf_no

      sla_max = sugar_sla_max (leaf_no_today)

      optfr = min (sugar_swdef(expansion), sugar_nfact(expansion))

      g_dlt_lai = u_bound (g_dlt_lai_pot * optfr
     :                   , g_dlt_dm_green(leaf) * sla_max * smm2sm)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_senescence ()
*     ===========================================================

*   Short description:
*       Simulate plant senescence.

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
*                       implicit none

*   Changes:
*      070495 nih taken from template

*   Calls:
*     sugar_dm_senescence
*     sugar_leaf_area_sen
*     sugar_leaf_death
*     sugar_N_senescence
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'sugar.inc'

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_senescence')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      call sugar_leaf_death (g_dlt_leaf_no_dead)

      call sugar_leaf_area_sen (g_dlt_slai)

      call sugar_dm_senescence (g_dlt_dm_senesced)
      call sugar_N_senescence (g_dlt_N_senesced)

      call sugar_root_length_senescence (g_dlt_rlv_senesced)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_leaf_death (dlt_leaf_no_dead)
*     ===========================================================

*   Short description:
*       Return the fractional death of oldest green leaf.

*   Assumptions:
*       none

*   Notes:
*      none
*   Procedure attributes:
*      Version:         any hardware/fortran77
*      Extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       implicit none

*   Changes:
*     070495 nih taken from template

*   Calls:
*     bound
*     on_day_of
*     pop_routine
*     push_routine
*     stage_is_between
*     sum_between

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       dlt_leaf_no_dead      ! (OUTPUT) new fraction of oldest
                                       ! green leaf

*   Global variables
      include   'sugar.inc'

      real       bound                 ! function
      real       l_bound               ! function
      logical    on_day_of             ! function
      logical    stage_is_between      ! function
      real       sum_between           ! function

*   Internal variables
      real       leaf_no_today
      real       leaf_no_dead_today    ! total number of dead leaves today
      real       leaf_no_dead_yesterday ! total number of dead leaves
                                        ! yesterday
      real       total_leaf_no         ! total number of leaves today

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_leaf_death')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      leaf_no_dead_yesterday = sum_between (emerg, now, g_leaf_no_dead)

      if (stage_is_between (emerg, crop_end, g_current_stage)) then

         ! this approach won't work if the growing point gets killed
         ! we will require an approach that integrates the app rate
         ! function to create a dlfno vs tt curve.
         ! this is quick and dirty to allow testing of green leaf
         ! approach

         leaf_no_today = sum_between (emerg, now, g_leaf_no)
     :                 + g_dlt_leaf_no
         leaf_no_dead_today = leaf_no_today - c_green_leaf_no
         leaf_no_dead_today = l_bound(leaf_no_dead_today,0.0)


      elseif (on_day_of (crop_end
     :                 , g_current_stage, g_days_tot)) then

         total_leaf_no = sum_between (emerg, now, g_leaf_no)
         leaf_no_dead_today = total_leaf_no

      else
         leaf_no_dead_today = 0.0
      endif

      leaf_no_dead_today = bound (leaf_no_dead_today
     :                           , leaf_no_dead_yesterday
     :                           , real(max_leaf))
      dlt_leaf_no_dead = leaf_no_dead_today - leaf_no_dead_yesterday

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_leaf_area_sen (dlt_slai)
*     ===========================================================
*   Short description:
*       Return the lai that senesces on the current day

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
*                       implicit none

*   Changes:
*     070495 nih taken from template

*   Calls:
*     pop_routine
*     push_routine
*     sugar_leaf_area_sen_age
*     sugar_leaf_area_sen_age1
*     sugar_leaf_area_sen_frost
*     sugar_leaf_area_sen_frost1
*     sugar_leaf_area_sen_light
*     sugar_leaf_area_sen_light1
*     sugar_leaf_area_sen_water
*     sugar_leaf_area_sen_water1

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       dlt_slai              ! (OUTPUT) lai senesced

*   Global variables
      include   'sugar.inc'

*   Internal variables
      real       dlt_slai_age     !
      real       dlt_slai_light   !
      real       dlt_slai_water   !
      real       dlt_slai_frost   !

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_leaf_area_sen')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

         ! get senescense from age, light competition, temperature
         ! and water stresses.

      call sugar_leaf_area_sen_age (dlt_slai_age)

      call sugar_leaf_area_sen_light (dlt_slai_light)

      call sugar_leaf_area_sen_water (dlt_slai_water)

      call sugar_leaf_area_sen_frost (dlt_slai_frost)

         ! now take largest of deltas
      dlt_slai = max (dlt_slai_age
     :              , dlt_slai_light
     :              , dlt_slai_water
     :              , dlt_slai_frost)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_leaf_area_sen_age (dlt_slai_age)
*     ===========================================================
*   Short description:
*       Return the lai that would senesce on the
*       current day due to ageing

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
*                       implicit none

*   Changes:
*     070495 nih taken from template

*   Calls:
*     bound
*     divide
*     l_bound
*     pop_routine
*     push_routine
*     stage_is_between
*     sum_between

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       dlt_slai_age          ! (OUTPUT) new senesced lai from
                                       ! phasic devel.

*   Global variables
      include   'convert.inc'
      include   'sugar.inc'

      real       bound                 ! function
      real       sum_between           ! function
      real       sum_real_array        ! function

*   Internal variables
      real       dlt_leaf_area         ! potential senesced leaf area from
                                       ! highest leaf no. senescing (mm^2)
      integer    leaf_no_dead          ! current leaf number dying ()
      real       slai_age              ! lai senesced by natural ageing

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_leaf_area_sen_age')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

         ! now calculate the leaf senescence
         ! due to normal phenological (phasic) development

         ! get highest leaf no. senescing today

      leaf_no_dead = int (1.0
     :                   + sum_between (emerg, now, g_leaf_no_dead))

         ! get area senesced from highest leaf no.

      dlt_leaf_area = mod (g_dlt_leaf_no_dead, 1.0)
     :                 * g_leaf_area(leaf_no_dead)

      slai_age = (sum_real_array (g_leaf_area, leaf_no_dead - 1)
     :         + dlt_leaf_area)
     :         * smm2sm * g_plants

      dlt_slai_age = bound (slai_age - g_slai, 0.0, g_lai)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_leaf_area_sen_water (dlt_slai_water)
*     ===========================================================
*   Short description:
*       Return the lai that would senesce on the
*       current day due to water stress

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
*                       implicit none

*   Changes:
*     070495 nih taken from template

*   Calls:
*     bound
*     pop_routine
*     push_routine
*     sugar_swdef

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       dlt_slai_water        ! (OUTPUT) water stress senescense

*   Global variables
      include   'sugar.inc'

      real       bound                 ! function
      real       sugar_swdef           ! function

*   Internal variables
      real       slai_water_fac        ! drought stress factor (0-1)

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_leaf_area_sen_water')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

         ! get senescense from stresses.

         ! calculate 0-1 factors for leaf senescence due to drought
         ! stress

         ! drought stress factor

      slai_water_fac = c_sen_rate_water* (1.0 - sugar_swdef (photo))

      dlt_slai_water = g_lai * slai_water_fac
      dlt_slai_water = bound (dlt_slai_water, 0.0, g_lai)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_leaf_area_sen_light (dlt_slai_light)
*     ===========================================================
*   Short description:
*       Return the lai that would senesce on the
*       current day due to light competition

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
*                       implicit none

*   Changes:
*     070495 nih taken from template

*   Calls:
*     bound
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       dlt_slai_light        ! (OUTPUT) lai senesced by low light

*   Global variables
      include   'sugar.inc'

      real       bound                 ! function
      real       divide                ! function

*   Internal variables
      real       cover
      real       cover_reduction
      real       new_cover
      real       new_lai
      real       lai_reduction

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_leaf_area_sen_light')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

         ! calculate 0-1 factor for leaf senescence due to
         ! competition for light.

c+!!!!!!!! this doesnt account for other growing crops
c+!!!!!!!! should be based on reduction of intercepted light and k*lai
         ! competition for light factor

      cover = 1.0 - exp (-c_extinction_coef * g_lai)

      if (cover.gt.c_lai_sen_light) then
         cover_reduction = c_sen_light_slope * (cover - c_lai_sen_light)
         new_cover = cover - cover_reduction
         new_lai = divide (log (1.0-new_cover), -c_extinction_coef, 0.0)
         lai_reduction = new_lai - g_lai

      else
         lai_reduction = 0.0

      endif

      dlt_slai_light = bound (-lai_reduction, 0.0, g_lai)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_leaf_area_sen_frost (dlt_slai_frost)
*     ===========================================================
*   Short description:
*       Return the lai that would senesce on the
*       current day from low temperatures

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
*                       implicit none

*   Changes:
*     070495 nih taken from template

*   Calls:
*     bound
*     divide
*     min
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       dlt_slai_frost        ! (OUTPUT) lai frosted today

*   Global variables
      include   'sugar.inc'

      real       bound                 ! function
      real       linear_interp_real    ! function

*   Internal variables
      real       dlt_slai_low_temp     ! lai senesced from low temps
      real       sen_fac_temp          ! low temperature factor (0-1)

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_leaf_area_sen_frost')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

          ! low temperature factor
      sen_fac_temp = linear_interp_real (
     :                                   g_mint
     :                                  ,c_frost_temp
     :                                  ,c_frost_fraction
     :                                  ,c_num_frost_temp)

      dlt_slai_low_temp = sen_fac_temp * g_lai
      dlt_slai_frost = bound (dlt_slai_low_temp, 0.0, g_lai)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_dm_senescence (dlt_dm_senesced)
*     ===========================================================

*   Short description:
*       Derives seneseced plant dry matter (g/m^2)

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
*                       implicit none

*   Changes:
*       070495 nih taken from template

*   Calls:
*     l_bound
*     sugar_leaf_no_from_lai
*     sum_real_array
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments

      real       dlt_dm_senesced(*)    ! (OUTPUT) actual biomass senesced
                                       ! from plant parts (g/m^2)

*   Global variables
      include   'convert.inc'
      include   'sugar.inc'

      real       divide                 ! function
      real       l_bound                ! function
      real       sugar_leaf_no_from_lai ! function
      real       sum_real_array         ! function

*   Internal variables
      real       dm_senesced_leaf       ! today's dm of senesced leaves
                                        ! (g/m^2)
      real       dm_senesced_leaf_plant ! today's dm of senesced leaves
                                        ! (g/plant)
      real       lai_today             ! today's green lai
      real       slai_today            ! today's senesced lai
      real       leaf_no_senesced      ! number of senesced leaves today
      integer    leaf_no_senescing     ! leaf number senescing today

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_dm_senescence')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

         ! first we zero all plant component deltas

      call fill_real_array (dlt_dm_senesced, 0.0, max_part)

      lai_today = g_lai + g_dlt_lai

      if (g_dlt_slai .lt. lai_today) then
         slai_today = g_slai + g_dlt_slai
         leaf_no_senesced = sugar_leaf_no_from_lai (slai_today)
         leaf_no_senescing = int (leaf_no_senesced + 1.0)
         dm_senesced_leaf_plant =
     :        sum_real_array (g_leaf_dm, int (leaf_no_senesced))
     :      + mod (leaf_no_senesced, 1.0) * g_leaf_dm(leaf_no_senescing)

         dm_senesced_leaf = dm_senesced_leaf_plant * g_plants
         dm_senesced_leaf = l_bound (dm_senesced_leaf
     :                                   , g_dm_senesced(leaf))

         dlt_dm_senesced(leaf) = dm_senesced_leaf
     :                         - g_dm_senesced(leaf)

         ! Take related cabbage with the dying leaf

         dlt_dm_senesced(cabbage) = divide (
     :                                      dlt_dm_senesced (leaf)
     :                                     ,c_leaf_cabbage_ratio
     :                                     ,0.0)
c         dlt_dm_senesced(cabbage) =
c     :         u_bound(dlt_dm_senesced(cabbage),
c     :         g_dm_green(cabbage)+g_dlt_dm_green(cabbage))

      else
         dlt_dm_senesced(leaf) = g_dm_green(leaf)
     :                         + g_dlt_dm_green(leaf)

         dlt_dm_senesced(cabbage) = g_dm_green(cabbage)
     :                         + g_dlt_dm_green(cabbage)
      endif

      dlt_dm_senesced(root) = g_dm_green(root) * c_dm_root_sen_frac

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      real function sugar_leaf_no_from_lai (lai)
*     ===========================================================

*   Short description:
*       Derives leaf no from lai and leaf area

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
*                       implicit none

*   Changes:
*       070495 nih taken from template

*   Calls:
*     divide
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments

      real       lai                   ! (INPUT) lai of leaves

*   Global variables
      include   'convert.inc'
      include   'sugar.inc'

      real       divide                ! function
      integer    get_cumulative_index_real ! function
      real       sum_real_array        ! function

*   Internal variables
      real       leaf_area             ! plant leaf area from lai (mm^2)
      integer    leaf_no               ! number of leaves containing leaf
                                       ! leaf area (0-max_leaf)
      real       leaf_area_whole       ! number of complete leaves ()
      real       leaf_area_part        ! area from last leaf (mm^2)
      real       leaf_fract            ! fraction of last leaf (0-1)

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_leaf_no_from_lai')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      leaf_area = divide (lai, g_plants, 0.0) * sm2smm
      leaf_no = get_cumulative_index_real (leaf_area, g_leaf_area
     :                                   , max_leaf)

      leaf_area_whole = sum_real_array (g_leaf_area, leaf_no - 1)
      leaf_area_part = leaf_area - leaf_area_whole
      leaf_fract = divide (leaf_area_part, g_leaf_area(leaf_no), 0.0)
      sugar_leaf_no_from_lai = real (leaf_no - 1) + leaf_fract

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_N_senescence (dlt_N_senesced)
*     ===========================================================

*   Short description:
*       Derives seneseced plant nitrogen (g N/m^2)

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
*                       implicit none

*   Changes:
*       070495 nih taken from template

*   Calls:
*     fill_real_array
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments

      real       dlt_N_senesced(*)     ! (OUTPUT) actual nitrogen senesced
                                       ! from plant parts (g/m^2)

*   Global variables
      include   'sugar.inc'


*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_N_senescence')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

         ! first we zero all plant component deltas

      call fill_real_array (dlt_N_senesced, 0.0, max_part)

      dlt_N_senesced(leaf) = g_dlt_dm_senesced(leaf)
     :                     * c_N_leaf_sen_conc
      dlt_N_senesced(cabbage) = g_dlt_dm_senesced(cabbage)
     :                     * c_N_cabbage_sen_conc
      dlt_N_senesced(root) = g_dlt_dm_senesced(root)
     :                     * c_N_root_sen_conc
cnh what checks are there that there is enough N in plant to provide this

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_detachment ()
*     ===========================================================

*   Short description:
*       Simulate plant detachment.

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
*                       implicit none

*   Changes:
*      070495 nih taken from template

*   Calls:
*     sugar_dm_detachment
*     sugar_N_detachment
*     sugar_N_senescence
*     sugar_slai_detachment
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'sugar.inc'

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_detachment')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      call sugar_dm_detachment (g_dlt_dm_detached)
      call sugar_slai_detachment (g_dlt_slai_detached)
      call sugar_N_detachment (g_dlt_N_detached)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_dm_detachment (dlt_dm_detached)
*     ===========================================================

*   Short description:
*       Derives detachment of seneseced plant dry matter (g/m^2)

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
*                       implicit none

*   Changes:
*       070495 nih taken from template

*   Calls:
*     fill_real_array
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments

      real       dlt_dm_detached(*)    ! (OUTPUT) actual biomass detached
                                       ! from senesced plant parts (g/m^2)

*   Global variables
      include   'sugar.inc'


*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_dm_detachment')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

         ! first we zero all plant component deltas

      call fill_real_array (dlt_dm_detached, 0.0, max_part)

      dlt_dm_detached(leaf) = g_dm_senesced(leaf)
     :                      * c_dm_leaf_detach_frac
      dlt_dm_detached(root) = g_dlt_dm_senesced(root)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_slai_detachment (dlt_slai_detached)
*     ===========================================================

*   Short description:
*       Derives detachment of seneseced plant slai (g/m^2)

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
*                       implicit none

*   Changes:
*       070495 nih taken from template

*   Calls:
*     fill_real_array
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments

      real       dlt_slai_detached     ! (OUTPUT) lai detached from senesced
                                       ! plant leaf

*   Global variables
      include   'sugar.inc'


*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_slai_detachment')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      dlt_slai_detached = g_slai * c_dm_leaf_detach_frac

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_N_detachment (dlt_N_detached)
*     ===========================================================

*   Short description:
*       Derives seneseced plant nitrogen (g N/m^2)

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
*                       implicit none

*   Changes:
*       070495 nih taken from template

*   Calls:
*     fill_real_array
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments

      real       dlt_N_detached(*)     ! (OUTPUT) actual nitrogen senesced
                                       ! from plant parts (g/m^2)

*   Global variables
      include   'sugar.inc'


*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_N_detachment')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

         ! first we zero all plant component deltas

      call fill_real_array (dlt_N_detached, 0.0, max_part)

      dlt_N_detached(leaf) = g_dlt_N_senesced(leaf)
     :                     * c_dm_leaf_detach_frac
      dlt_N_detached(root) = g_dlt_N_senesced(root)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_biomass ()
*     ===========================================================

*   Short description:
*       Simulate crop biomass processes.

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
*                       implicit none

*   Changes:
*      060495 nih taken from template

*   Calls:
*     sugar_dm_init
*     sugar_dm_partition
*     sugar_dm_production
*     sugar_dm_retranslocate
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'sugar.inc'
      real sugar_swdef
      real sugar_nfact
*   Internal variables
      real optfr

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_biomass')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)
      call sugar_dm_init (g_dm_green, g_dm_plant_min)

            ! drymatter production
      call sugar_dm_production (g_dlt_dm)

      optfr = min (sugar_swdef(expansion), sugar_nfact(expansion))
      call sugar_dm_partition (g_dlt_dm
     :                        ,g_dlt_lai_pot * optfr
     :                        ,g_dlt_dm_green
     :                        ,g_partition_xs)

      call sugar_dm_retranslocate (g_dlt_dm_green_retrans)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_dm_init (dm_green, dm_plant_min)
*     ===========================================================

*   Short description:
*       Initialise plant weights and plant weight minimums
*       at required instances.

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
*                       implicit none

*   Changes:
*     060495 nih taken from template

*   Calls:
*     divide
*     on_day_of
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       dm_green(*)           ! (INPUT/OUTPUT) plant part weights
                                       ! (g/m^2)
      real       dm_plant_min(*)       ! (OUTPUT) minimum weight of each
                                       ! plant part (g/plant)

*   Global variables
      include   'convert.inc'
      include   'sugar.inc'

      integer    count_of_real_vals    ! function
      real       divide                ! function
      logical    on_day_of             ! function

*   Internal variables
      integer layer
      real    layer_volume
      integer num_layers
      real    root_wt_layer
      real    root_length_layer

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_dm_init')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

         ! initialise plant weight
         ! initialisations - set up dry matter for leaf, stem,..etc,
         ! and root

      if (on_day_of (emerg, g_current_stage, g_days_tot)) then
             ! seedling has just emerged.

             ! we initialise root_wt no by adding all root together
             ! as specified by the rlv given by user at sowing.
         num_layers = count_of_real_vals (g_dlayer, max_layer)
         dm_green(root) = 0.0
         do 100 layer = 1, num_layers
            layer_volume   = 1.0 * sm2smm   *   g_dlayer(layer)
            root_length_layer = g_rlv(layer) * layer_volume
            root_wt_layer  = divide (root_length_layer
     :                              ,c_specific_root_length
     :                              ,0.0)
            dm_green(root) = dm_green(root) +   root_wt_layer
  100    continue

         dm_green(sstem) = c_dm_sstem_init * g_plants
         dm_green(leaf) = c_dm_leaf_init * g_plants
         dm_green(cabbage) = c_dm_cabbage_init * g_plants
         dm_green(sucrose) = c_dm_sucrose_init * g_plants

cnh     NO MINIMUMS SET AS YET

      else   ! no changes
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_dm_production (dlt_dm)
*     ===========================================================

*   Short description:
*       Actual dm production (g/m^2)

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
*                       implicit none

*   Changes:
*       060495 nih taken from template

*   Calls:
*     find_layer_no
*     pop_routine
*     push_routine
*     sugar_dm_potential
*     sugar_transp_eff
*     sum_real_array

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       dlt_dm                ! (OUTPUT) actual dry matter
                                       ! production (g/m^2)

*   Global variables
      include   'sugar.inc'

      integer    find_layer_no         ! function
      real       sugar_transp_eff      ! function
      real       sum_real_array        ! function

*   Internal variables
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      real       dlt_dm_transp         ! potential dry matter production
                                       ! by transpiration (g/m^2)
      real       dlt_dm_pot            ! potential dry matter production with
                                       ! optimum water and nitrogen and
                                       ! temperature stress conditions (g/m^2)
      real       sw_supply_sum         ! Water available to roots (mm)

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_dm_production')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

         ! potential (supply) by transpiration

      deepest_layer = find_layer_no (g_root_depth, g_dlayer, max_layer)
      sw_supply_sum = sum_real_array (g_sw_supply, deepest_layer)
      dlt_dm_transp = sw_supply_sum*sugar_transp_eff ()

         ! potential by photosynthesis

      call sugar_dm_potential (dlt_dm_pot)

         ! use whichever is limiting
      dlt_dm = min (dlt_dm_pot, dlt_dm_transp)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_dm_partition (dlt_dm, dlt_lai_pot, dlt_dm_green
     :                              , partition_xs)
*     ===========================================================

*   Short description:
*       Partitions new dm (assimilate) between plant components (g/m^2)

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
*                       implicit none

*   Changes:
*       060495 nih taken from template
*       110196 nih added dlt_dm to argument list to make this routine
*                  more like a utility routine for partioning dry matter

*   Calls:
*     bound
*     bound_check_real_array
*     bound_check_real_var
*     divide
*     fill_real_array
*     int
*     pop_routine
*     push_routine
*     stage_is_between
*     sum_between
*     sum_real_array
*     u_bound

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       dlt_dm                ! (INPUT) dry matter to partition
      real       dlt_lai_pot           ! (INPUT) increase in lai if 
                                       ! unconstrained by carbon supply.
      real       dlt_dm_green (*)      ! (OUTPUT) actual biomass partitioned
                                       ! to plant parts (g/m^2)
      real       partition_xs          ! xs dry matter to that required
                                       ! to supply all demands. (g/m^2)

*   Global variables
      include   'convert.inc'
      include   'sugar.inc'

      real       divide                ! function
      logical    stage_is_between      ! function
      real       sugar_sla_min         ! function
      real       sugar_sucrose_fraction! function
      real       sum_real_array        ! function
      real       sum_between           ! function
      real       u_bound               ! function

*   Internal variables
      integer    current_phase         ! current phase no.
      real       dlt_dm_green_tot      ! total of partitioned dm (g/m^2)
      real       dlt_leaf_max          ! max increase in leaf wt (g/m2)
      real       sla_min               ! minimum specific leaf area (mm2/g)
      real       dlt_cane              ! increase in cane wt (g/m2)
      real       dlt_cane_min          ! min increase in cane wt (g/m2)
      real       leaf_no_today
      real       sucrose_fraction      ! fraction of cane C going to sucrose
      real       tt_since_begcane      ! thermal time since the beginning
                                       ! of cane growth (deg days)
*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_dm_partition')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

         ! Root must be satisfied. The roots don't take any of the
         ! carbohydrate produced - that is for tops only.  Here we assume
         ! that enough extra was produced to meet demand. Thus the root
         ! growth is not removed from the carbo produced by the model.

         ! first we zero all plant component deltas

      call fill_real_array (dlt_dm_green, 0.0, max_part)
      partition_xs = 0.0

         ! now we get the root delta for all stages - partition scheme
         ! specified in coeff file

      current_phase = int (g_current_stage)
      dlt_dm_green(root) = c_ratio_root_shoot(current_phase)*dlt_dm

      leaf_no_today = sum_between (emerg, now, g_leaf_no)
     :              + g_dlt_leaf_no

      sla_min = sugar_sla_min (leaf_no_today)

      dlt_leaf_max = divide (dlt_lai_pot
     :                      ,sla_min*smm2sm
     :                      ,0.0)

      if (stage_is_between (emerg, begcane, g_current_stage)) then
            ! we have leaf and cabbage development only

         dlt_dm_green(leaf) = dlt_dm
     :                      * (1.0 - 1.0/(c_leaf_cabbage_ratio+1.0))

         dlt_dm_green(leaf) = u_bound (dlt_dm_green(leaf),dlt_leaf_max)

         dlt_dm_green(cabbage) = dlt_dm_green(leaf)/c_leaf_cabbage_ratio

         partition_xs       = dlt_dm
     :                      - dlt_dm_green(leaf)
     :                      - dlt_dm_green(cabbage)

         ! Put the excess dry matter in sstem
         dlt_dm_green (sstem) = partition_xs

      elseif (stage_is_between (begcane, crop_end
     :                        , g_current_stage)) then

         ! if leaf component makes leaves too thick extra goes to sstem

         dlt_cane_min = c_cane_fraction * dlt_dm

         dlt_dm_green(leaf) = (dlt_dm - dlt_cane_min)
     :                      * (1.0 - 1.0/(c_leaf_cabbage_ratio+1.0))

         dlt_dm_green(leaf) = u_bound (dlt_dm_green(leaf),dlt_leaf_max)
         dlt_dm_green(cabbage) = dlt_dm_green(leaf)/c_leaf_cabbage_ratio
         dlt_cane = dlt_dm - dlt_dm_green(leaf)
     :                       - dlt_dm_green(cabbage)

         tt_since_begcane = sum_between (begcane,now,g_tt_tot)

         if ((tt_since_begcane .gt. c_sucrose_delay)
     :                        .and.
     :       (g_dm_green(SStem).gt. c_min_sstem_sucrose))
     :   then
            ! the SStem pool gets (1 - c_sucrose_fraction) of the DEMAND
            ! for C. Extra C above the demand for cane goes only into
            ! the sucrose pool.

            sucrose_fraction = sugar_sucrose_fraction()
            dlt_dm_green(SStem) = dlt_cane_min * (1.-sucrose_fraction)
            dlt_dm_green(Sucrose) = dlt_cane_min * sucrose_fraction

            partition_xs = dlt_cane - dlt_cane_min
            dlt_dm_green(Sucrose) = dlt_dm_green(Sucrose) + partition_xs

         else
            ! nih - should excess C go into sucrose here too even though
            ! we have not started into the sugar accumulation phase????
            dlt_dm_green(SStem) = dlt_cane
            partition_xs = dlt_cane - dlt_cane_min

         endif

      else
            ! no partitioning
      endif

cnh Due to small rounding errors I will say that small errors are ok

         ! do mass balance check - roots are not included
      dlt_dm_green_tot = sum_real_array (dlt_dm_green, max_part)
     :                 - dlt_dm_green(root)

      call bound_check_real_var (dlt_dm_green_tot
     :                        , dlt_dm - 1.e-6
     :                        , dlt_dm + 1.e-6
     :                        , 'dlt_dm_green_tot mass balance')

         ! check that deltas are in legal range

      call bound_check_real_array (dlt_dm_green
     :                        , -1.e-6
     :                        , dlt_dm + 1.e-6
     :                        , 'dlt_dm_green'
     :                        , max_part)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_dm_retranslocate (dm_retranslocate)
*     ===========================================================

*   Short description:
*     Calculate plant dry matter delta's due to retranslocation (g/m^2)

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
*                       implicit none

*   Changes:
*       060495 nih taken from template

*   Calls:
*     bound
*     bound_check_real_var
*     fill_real_array
*     l_bound
*     pop_routine
*     push_routine
*     stage_is_between
*     sum_real_array

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments

      real       dm_retranslocate(*)   ! (INPUT) actual change in plant part
                                       ! weights due to translocation (g/m^2)

*   Global variables
      include   'sugar.inc'

      real       sum_real_array        ! function

*   Internal variables
      real       mass_balance          ! sum of translocated carbo (g/m^2)

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_dm_retranslocate')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

         ! now translocate carbohydrate between plant components

      call fill_real_array (dm_retranslocate, 0.0, max_part)

         ! now check that we have mass balance

      mass_balance = sum_real_array (dm_retranslocate, max_part)
      call bound_check_real_var (mass_balance, 0.0, 0.0
     :                         , 'dm_retranslocate mass balance')

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_nitrogen ()
*     ===========================================================

*   Short description:
*       Simulate crop nitrogen processes.

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
*                       implicit none

*   Changes:
*      060495 nih taken from template

*   Calls:
*     pop_routine
*     push_routine
*     sugar_N_demand
*     sugar_N_init
*     sugar_N_retranslocate
*     sugar_N_uptake

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'sugar.inc'

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_nitrogen')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      call sugar_N_init (g_N_green)
cnh      call sugar_N_retranslocate (g_dlt_N_retrans)

cnh n_demand now in prepare stage
cnh      call sugar_N_demand (g_N_demand)

      call sugar_N_uptake (g_dlt_NO3gsm, g_dlt_N_green)

cnh
      call sugar_N_retranslocate (g_dlt_N_retrans)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_N_retranslocate (dlt_N_retrans)
*     ===========================================================

*   Short description:

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
*                       implicit none

*   Changes:
*       060495 nih taken from template

*   Calls:
*     bound_check_real_var
*     fill_real_array
*     pop_routine
*     push_routine
*     sugar_N_retrans_avail
*     sum_real_array

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       dlt_N_retrans (*)     ! (OUTPUT) plant N taken out from
                                       ! plant parts (g N/m^2)

*   Global variables
      include   'sugar.inc'

*   Internal variables
      real       N_avail(max_part)     ! N available for transfer to grain
                                       ! (g/m^2)
      integer    part                  ! plant part number

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_N_retranslocate')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      call sugar_N_retrans_avail (N_avail)  ! grain N potential (supply)

          ! limit retranslocation to total available N

      call fill_real_array (dlt_N_retrans, 0.0, max_part)

             ! just check that we got the maths right.

      do 1000 part = 1, max_part
         call bound_check_real_var (abs (dlt_N_retrans(part))
     :                            , 0.0, N_avail(part)
     :                            , 'dlt_N_retrans(part)')
1000  continue

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_N_retrans_avail (N_avail)
*     ===========================================================

*   Short description:
*     Calculate N available for transfer (g/m^2)
*     from each plant part.

*   Assumptions:
*       none

*   Notes:
*     NB. No translocation from roots.

*   Procedure attributes:
*      Version:         any hardware/fortran77
*      Extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       implicit none

*   Changes:
*       060495 nih taken from template

*   Calls:
*       l_bound
*       pop_routine
*       push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       N_avail (*)           ! (OUTPUT) total N available for
                                       ! transfer to grain (g/m^2)

*   Global variables
      include   'sugar.inc'

      real       l_bound               ! function

*   Internal variables
      real       N_min                 ! nitrogen minimum level (g/m^2)
      integer    part                  ! plant part number

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_N_retrans_avail')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

         ! now find the available N of each part.

      do 1000 part = 1, max_part
         N_min = g_N_conc_min(part) * g_dm_green(part)
         N_avail(part) = l_bound (g_N_green(part) - N_min, 0.0)
1000  continue

      N_avail(sucrose) = 0.0
      N_avail(root) = 0.0

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_N_demand (N_demand)
*     ===========================================================

*   Short description:
*       Return plant nitrogen demand for each plant component

*   Assumptions:
*       none

*   Notes:

*           N demand consists of two components:
*           Firstly, the demand for nitrogen by the potential new growth.
*           Secondly, the demand due to the difference between
*           the actual N concentration and the critical N concentration
*           of the tops (stover), which can be positive or negative


*   Procedure attributes:
*      Version:         any hardware/fortran77
*      Extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       implicit none

*   Changes:
*     060495 nih taken from template

*   Calls:
*     bound
*     bound_check_real_var
*     divide
*     l_bound
*     pop_routine
*     push_routine
*     sugar_radn_int
*     sum_real_array

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       N_demand (*)          ! (OUTPUT) plant nitrogen demand
                                       ! (g/m^2)

*   Global variables
      include   'sugar.inc'

cnh      real       bound                 ! function
cnh      real       divide                ! function
      real       l_bound               ! function
      real       sum_real_array        ! function

*   Internal variables
      integer    current_phase         ! current phase number
      real       N_crit                ! critical N amount (g/m^2)
      real       N_demand_new          ! demand for N by new growth
                                       ! (g/m^2)
      real       N_demand_old          ! demand for N by old biomass
                                       ! (g/m^2)
      integer    part                  ! plant part
      real       dlt_dm_pot_radn       ! potential dry matter production with
                                       ! optimum water and nitrogen and
                                       ! temperature stress conditions (g/m^2)
      real       dlt_dm_pot (max_part) ! potential dry weight increase
                                       ! (g/m^2)
cnh      real       part_fract            ! plant part fraction of dm  (0-1)
      real       partition_xs          ! dry matter partioned excess to demand
      real       radn_int              ! radn intercepted by leaves (mj/m^2)

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_N_demand')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

         ! calculate potential new shoot and root growth

      current_phase = int (g_current_stage)

            ! need to calculate dm using potential rue not affected by
            ! N and temperature

      call sugar_radn_int (radn_int)
      dlt_dm_pot_radn = c_rue(current_phase)*radn_int

      call sugar_dm_partition (dlt_dm_pot_radn
     :                        ,g_dlt_lai_pot
     :                        ,dlt_dm_pot
     :                        ,partition_xs)

cnh      do 500 part = 1, max_part
cnh         part_fract = divide (g_dlt_dm_green(part), g_dlt_dm, 0.0)
cnh         dlt_dm_pot(part) = dlt_dm_pot_radn * part_fract
cnh         dlt_dm_pot(part) = bound (dlt_dm_pot(part)
cnh     :                           , 0.0, dlt_dm_pot_radn)
cnh500   continue

            ! recalculate roots because today's drymatter production
            ! does not include roots

      dlt_dm_pot(root) = dlt_dm_pot_radn
     :                 * c_ratio_root_shoot(current_phase)


         ! g_dlt_dm_pot is above ground biomass only so leave roots
         ! out of comparison

      call bound_check_real_var (
     :             sum_real_array (dlt_dm_pot, max_part)
     :           - dlt_dm_pot(root)
     :           , 0.0, dlt_dm_pot_radn
     :           , 'dlt_dm_pot - dlt_dm_pot(root)')


      ! NIH - note stem stuff is redone down later.

      do 1000 part = 1, max_part
         if (g_dm_green(part).gt.0.0) then

               ! get N demands due to difference between actual N concentrations
               ! and critical N concentrations of tops (stover) and roots.

            N_crit = g_dm_green(part) * g_N_conc_crit(part)
            N_demand_old = N_crit - g_N_green(part)


               ! get potential N demand (critical N) of potential growth

            N_demand_new = dlt_dm_pot(part) * g_N_conc_crit(part)

            N_demand(part) = N_demand_old + N_demand_new
            N_demand(part) = l_bound (N_demand(part), 0.0)

         else
            N_demand(part) = 0.0

         endif

1000  continue

cnh I am not 100% happy with this but as this is a first attempt at fully
cnh utilizing a sucrose pool I shall put in this quick fix for now and
cnh re-evaluate later.  Note that g_N_conc_crit(Sstem) is really the crit.
cnh conc for CANE.

      ! SStem demand for N is based on N conc in cane (i.e SStem+sucrose)

      N_crit = (g_dm_green(sstem)+g_dm_green(sucrose))
     :                    * g_N_conc_crit(sstem)
      N_demand_old = N_crit - g_N_green(sstem)
      N_demand_new = (dlt_dm_pot(sstem)+dlt_dm_pot(sucrose))
     :                    * g_N_conc_crit(sstem)
      N_demand(sstem) = N_demand_old + N_demand_new
      N_demand(sstem) = l_bound (N_demand(sstem), 0.0)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_N_uptake (dlt_NO3gsm, dlt_N_green)
*     ===========================================================

*   Short description:
*       Return actual plant nitrogen uptake to each plant part and from
*       each soil layer.

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
*                       implicit none

*   Changes:
*       060495 nih taken from template
*       130396 nih added fix to stop N above critical conc evaporating

*   Calls:
*     bound
*     divide
*     fill_real_array
*     find_layer_no
*     l_bound
*     pop_routine
*     push_routine
*     sugar_N_diffusion
*     sugar_N_mass_flow
*     sum_real_array

* ----------------------- Declaration section ------------------------

      implicit none

*   Global variables
      include   'const.inc'
      include   'convert.inc'          ! gm2kg, sm2ha
      include   'sugar.inc'

      real       bound                 ! function
      real       divide                ! function
      integer    find_layer_no         ! function
      real       l_bound               ! function
      real       sum_real_array        ! function

*   Subroutine arguments
      real       dlt_N_green(max_part) ! (OUTPUT) actual plant N uptake
                                       ! into each plant part (g/m^2)

      real       dlt_NO3gsm(max_layer) ! (OUTPUT) actual plant N uptake
                                       ! from NO3 in each layer (g/m^2)


*   Internal variables
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      real       NO3gsm_diffn          ! actual N available (supply) for
                                       ! plant (g/m^2) by diffusion
      real       NO3gsm_mflow          ! actual N available (supply) for
                                       ! plant (g/m^2) by mass flow
      real       NO3gsm_diffn_supply   ! total potential N uptake (supply)
                                       ! for plant (g/m^2) by diffusion
      real       NO3gsm_diffn_avail(max_layer) ! potential NO3 (supply) from
                                       ! soil (g/m^2), by diffusion
      real       NO3gsm_mflow_supply   ! total potential N uptake (supply)
                                       ! for plant (g/m^2) by mass flow
      real       NO3gsm_mflow_avail(max_layer) ! potential NO3 (supply) from
                                       ! soil (g/m^2) by mass flow
      real       diffn_fract           ! fraction of nitrogen to use (0-1)
                                       ! for diffusion
      real       mflow_fract           ! fraction of nitrogen to use (0-1)
                                       ! for mass flow
      real       plant_part_fract      ! fraction of nitrogen to use (0-1)
                                       ! for plant part
      integer    layer                 ! soil layer number of profile
      real       N_demand              ! total nitrogen demand (g/m^2)
      integer    part                  ! plant part number
      real       N_uptake_sum          ! total plant N uptake (g/m^2)
      real       NO3gsm_uptake         ! plant NO3 uptake from layer (g/m^2)

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_N_uptake')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      call fill_real_array (dlt_NO3gsm, 0.0, max_layer)
      call fill_real_array (dlt_N_green, 0.0, max_part)
      deepest_layer = find_layer_no(g_root_depth, g_dlayer, max_layer)

      N_demand = sum_real_array (g_N_demand, max_part)

      if (g_uptake_source.eq.'calc') then
            ! find potential N uptake (supply, available N)
            ! Get it for nitrate by diffusion and mass flow
            ! Note: the N available by diffusion is really the total N
            ! available to the roots by mass flow and diffusion.

         call sugar_N_mass_flow (NO3gsm_mflow_avail)
         call sugar_N_diffusion (NO3gsm_diffn_avail)

         do 1000 layer = 1, deepest_layer
            NO3gsm_diffn_avail(layer) = NO3gsm_diffn_avail(layer)
     :                                - NO3gsm_mflow_avail(layer)
            NO3gsm_diffn_avail(layer) =
     :                          l_bound (NO3gsm_diffn_avail(layer)
     :                         ,0.0)
1000     continue

            ! get potential N uptake (supply) from the root profile.
            ! get totals for diffusion and mass flow.

         NO3gsm_mflow_supply = sum_real_array (NO3gsm_mflow_avail
     :                                     , deepest_layer)
         NO3gsm_diffn_supply = sum_real_array (NO3gsm_diffn_avail
     :                                     , deepest_layer)

            ! get actual total nitrogen uptake for diffusion and mass flow.
            ! If demand is not satisfied by mass flow, then use diffusion.

         if (NO3gsm_mflow_supply.ge.N_demand) then
            NO3gsm_mflow = N_demand
            NO3gsm_diffn = 0.0

         else
            NO3gsm_mflow = NO3gsm_mflow_supply
            NO3gsm_diffn = bound (N_demand - NO3gsm_mflow, 0.0
     :                        , NO3gsm_diffn_supply)
            NO3gsm_diffn = divide (NO3gsm_diffn, c_NO3_diffn_const, 0.0)

         endif

            ! get actual change in N contents

         do 1100 layer = 1,deepest_layer

               ! allocate nitrate
               ! Find proportion of nitrate uptake to be taken from layer
               ! by diffusion and mass flow

            mflow_fract = divide (NO3gsm_mflow_avail(layer)
     :                          , NO3gsm_mflow_supply, 0.0)

            diffn_fract = divide (NO3gsm_diffn_avail(layer)
     :                          , NO3gsm_diffn_supply, 0.0)

               ! now find how much nitrate the plant removes from
               ! the layer by both processes

               ! Note - barrenness reduces the number of heads/ha
               ! below the number of plants /ha.  This means that during
               ! grain filling, the N uptake should be calculated on the
               ! number of heads and not the number of plants.  This can
               ! be done because grain is the only plant component
               ! taking up N during grain filling.

            NO3gsm_uptake = NO3gsm_mflow * mflow_fract
     :                    + NO3gsm_diffn * diffn_fract
            dlt_NO3gsm(layer) = - NO3gsm_uptake

1100     continue
      else
         if (g_num_uptake_no3.gt.0) then
            do 1200 layer = 1, g_num_uptake_no3
               dlt_NO3gsm(layer) = - g_uptake_no3(layer)/10.
 1200       continue
         else
            call fatal_error (Err_Internal,
     :           'No Soil NO3 uptake information was provided')
         endif
      endif
               ! find proportion of uptake to be
               ! distributed to each plant part and distribute it.
      N_uptake_sum = - sum_real_array (dlt_NO3gsm, deepest_layer)

      ! Partition N, according to relative demand, to each plant
      ! part but do not allow supply to exceed demand.  Any excess
      ! supply is to go into cane. - NIH 13/3/96

      do 1300 part = 1, max_part
         plant_part_fract = divide (g_N_demand(part), N_demand, 0.0)
         dlt_N_green(part) = min(N_uptake_sum, N_demand)
     :                     * plant_part_fract
1300  continue

      if (N_uptake_sum.gt.N_demand) then
         dlt_N_green(sstem) = dlt_N_green(sstem)
     :                      + (N_uptake_sum - N_demand)
      else
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_N_mass_flow (NO3gsm_mflow_pot)
*     ===========================================================

*   Short description:
*       Return potential nitrogen uptake (supply) by mass flow (water
*       uptake) (g/m^2)

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
*                       implicit none

*   Changes:
*       060495 nih taken from template

*   Calls:
*     divide
*     fill_real_array
*     find_layer_no
*     pop_routine
*     push_routine
*     u_bound

* ----------------------- Declaration section ------------------------

      implicit none

*   Global variables
      include   'convert.inc'          ! ha2sm, kg2gm
      include   'sugar.inc'

      real       divide                ! function
      integer    find_layer_no         ! function
      real       u_bound               ! function

*   Subroutine arguments
      real       NO3gsm_mflow_pot (max_layer) ! (OUTPUT) potential plant NO3
                                              ! uptake (supply) g/m^2,
                                              ! by mass flow

*   Internal variables
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      integer    layer                 ! layer number of soil
      real       NO3_conc              ! nitrogen concentration (g/m^2/mm)
      real       NO3gsm_mflow          ! potential nitrogen uptake (g/m^2)

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_N_mass_flow')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      call fill_real_array (NO3gsm_mflow_pot, 0.0, max_layer)

         ! only take the layers in which roots occur

      deepest_layer = find_layer_no (g_root_depth, g_dlayer, max_layer)
      do 1000 layer = 1, deepest_layer

            ! get  NO3 concentration

         NO3_conc = divide (g_NO3gsm(layer)
     :                    , g_sw_dep(layer), 0.0)

            ! get potential uptake by mass flow

         NO3gsm_mflow = NO3_conc * (-g_dlt_sw_dep(layer))
         NO3gsm_mflow_pot(layer) = u_bound (NO3gsm_mflow,
     :                            g_NO3gsm(layer) - g_NO3gsm_min(layer))

1000  continue

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_N_diffusion (NO3gsm_diffn_pot)
*     ===========================================================

*   Short description:
*       Return potential nitrogen uptake (supply) by diffusion
*       for a plant (g/m^2)

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
*                       implicit none

*   Changes:
*       060495 nih taken from template

*   Calls:
*     bound
*     divide
*     fill_real_array
*     find_layer_no
*     pop_routine
*     push_routine
*     u_bound

* ----------------------- Declaration section ------------------------

      implicit none

*   Global variables
      include   'convert.inc'          ! ha2sm, kg2gm
      include   'sugar.inc'

      real       bound                 ! function
      real       divide                ! function
      integer    find_layer_no         ! function
      real       u_bound               ! function

*   Subroutine arguments
      real       NO3gsm_diffn_pot (max_layer) ! (OUTPUT) potential plant NO3
                                              ! uptake (supply) g/m^2,
                                              !  by diffusion

*   Internal variables
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      integer    layer                 ! layer number of soil
      real       NO3gsm_diffn          ! potential nitrogen uptake (g/m^2)
      real       sw_avail_fract        ! fraction of extractable soil water ()

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_N_diffusion')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

         ! only take the layers in which roots occur

      call fill_real_array (NO3gsm_diffn_pot, 0.0, max_layer)

      deepest_layer = find_layer_no (g_root_depth, g_dlayer, max_layer)
      do 1000 layer = 1, deepest_layer

         sw_avail_fract = divide (g_sw_avail(layer)
     :                  , g_sw_avail_pot(layer), 0.0)
         sw_avail_fract = bound (sw_avail_fract, 0.0, 1.0)

            ! get extractable NO3
            ! restricts NO3 available for diffusion to NO3 in plant
            ! available water range

         NO3gsm_diffn = sw_avail_fract * g_NO3gsm(layer)
         NO3gsm_diffn_pot(layer) = u_bound (NO3gsm_diffn,
     :                           g_NO3gsm(layer) - g_NO3gsm_min(layer))

1000  continue

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_N_conc_limits (N_conc_crit, N_conc_min)
*     ===========================================================

*   Short description:
*       Calculate the critical N concentration below which plant growth
*       is affected.  Also minimum N concentration below which it is not
*       allowed to fall.  These are analogous to the water concentrations
*       of dul and ll.

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
*                       implicit none

*   Changes:
*       070495 nih taken from template

*   Calls:
*     count_of_real_vals
*     exp
*     fill_real_array
*     linear_interp_real
*     pop_routine
*     push_routine
*     sugar_stage_code
*     stage_is_between

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       N_conc_crit(*)        ! (OUTPUT) critical N concentration
                                       ! (g N/g part)
      real       N_conc_min(*)         ! (OUTPUT) minimum N concentration
                                       ! (g N/g part)

*   Global variables
      include   'sugar.inc'

      integer    count_of_real_vals    ! function
      real       linear_interp_real    ! function
      real       sugar_stage_code      ! function
      logical    stage_is_between      ! function

*   Internal variables
      integer    numvals               ! number of values in stage code table
      real       stage_code            ! interpolated current stage code

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_N_conc_limits')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      call fill_real_array (N_conc_crit, 0.0, max_part)
      call fill_real_array (N_conc_min, 0.0, max_part)

      if (stage_is_between (emerg, crop_end, g_current_stage)) then

         N_conc_crit(root) = c_N_conc_crit_root
         N_conc_min(root) = c_N_conc_min_root

             ! the tops critical N percentage concentration is the stover
             ! (non-grain shoot) concentration below which N concentration
             ! begins to affect plant growth.

         numvals = count_of_real_vals (c_x_stage_code, max_stage)
         stage_code = sugar_stage_code (g_current_stage, c_x_stage_code
     :                                , numvals)
         ! nih - I put cane critical conc in the sstem element of the
         ! array because there is no 'cane' (sstem+sucrose) pool
         N_conc_crit(sstem) = linear_interp_real (stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_crit_cane
     :                             , numvals)
         N_conc_crit(leaf) = linear_interp_real (stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_crit_leaf
     :                             , numvals)
         N_conc_crit(cabbage) = linear_interp_real (stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_crit_cabbage
     :                             , numvals)

             ! the  minimum N concentration is the N concentration
             ! below which N does not fall.

         ! nih - I put cane minimum conc in the sstem element of the
         ! array because there is no 'cane' (sstem+sucrose) pool
         N_conc_min(sstem) = linear_interp_real (stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_min_cane
     :                             , numvals)

         N_conc_min(leaf) = linear_interp_real (stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_min_leaf
     :                             , numvals)

         N_conc_min(cabbage) = linear_interp_real (stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_min_cabbage
     :                             , numvals)

      else
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      real function sugar_nfact (type)
*     ===========================================================

*   Short description:
*         Use shoot nitrogen concentrations to calculate 0-1 N availability
*         factors.  Since all plant processes are not equally susceptible
*         to N stress, N deficiency factors are calculated from a 0-1 N
*         factor to affect different processes.
*           0 affects grain N potential
*           1 affects photosynthesis
*           2 affects leaf senescence, grain N concentration & cell expansion
*           3 affects grain number

*           nfac range is 0.001 to 0.95 or 1.0 for optimum conditions.
*           N_def - 1 range is 0.2012 to .98 or 1 for optimum conditions.
*           N_def - 2 range is .00095 to .9025 or .95 for optimum conditions.
*           N_def - 3 range is .201 to 1 for optimum conditions.

*         ???? check that returns 1 & 0 for optimum and zero conditions.

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
*                       implicit none

*   Changes:
*     060495 nih taken from template
*     090895 bak reestablished N deficiency routines based on leaf N


*   Calls:
*     bound
*     divide
*     exp
*     fatal_error
*     pop_routine
*     Push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer    type                  ! (INPUT) factors type

*   Global variables
      include   'const.inc'
      include   'sugar.inc'

      real       bound                 ! function
      real       divide                ! function

*   Internal variables
cbak      real       N_conc_stover         ! tops (stover) actual N concentratio
                                       ! (0-1)
      real       N_conc_leaf           ! leaf actual N concentration
                                       ! (0-1)
cbak      real       dm_stover             ! tops (stover) plant weight (g/m^2)
      character  err_messg*100           ! error message
      real       N_def                 ! N factor (0-1)
cbak      real       N_cabbage_crit
cbak      real       N_cabbage_min
cbak      real       N_conc_stover_crit    ! tops (stover) critical N concentrat
                                       ! (0-1)
cbak      real       N_conc_stover_min     ! tops (stover) minimum N concentrati
                                       ! (0-1)
      real       N_conc_leaf_crit    ! tops (stover) critical N concentration
                                       ! (0-1)
      real       N_conc_leaf_min     ! tops (stover) minimum N concentration
                                       ! (0-1)

      real       N_leaf_crit           ! critical leaf nitrogen (g/m^2)
      real       N_leaf_min            ! minimum leaf nitrogen (g/m^2)
cbak      real       N_stem_crit           ! critical stem nitrogen (g/m^2)
cbak      real       N_stem_min            ! minimum stem nitrogen (g/m^2)
cbak      real       N_stover              ! tops (stover) plant nitrogen (g/m^2
cbak      real       N_stover_crit         ! critical top nitrogen (g/m^2)
cbak      real       N_stover_min          ! minimum top nitrogen (g/m^2)
      real       N_conc_ratio          ! available N as fraction of N capacity
                                       ! (0-1)

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_nfact')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

         ! calculate actual N concentrations

c      dm_stover = g_dm_green(leaf) + g_dm_green(sstem)
c     :          + g_dm_green(cabbage)
c      N_stover = g_N_green(leaf) + g_N_green(sstem)
c     :          + g_N_green(cabbage)
c
c      N_conc_stover = divide (N_stover, dm_stover, 0.0)

       N_conc_leaf = divide (g_N_green(leaf), g_dm_green(leaf), 0.0)

         ! calculate critical N concentrations
cbak   Base N deficiency on leaf N concentrations

       N_leaf_crit = g_N_conc_crit(leaf) * g_dm_green(leaf)

c      N_stem_crit = g_N_conc_crit(sstem) * g_dm_green(sstem)
c      N_cabbage_crit = g_N_conc_crit(cabbage) * g_dm_green(cabbage)
c      N_stover_crit = N_leaf_crit + N_stem_crit + N_cabbage_crit
cbak
       N_conc_leaf_crit = divide (N_leaf_crit, g_dm_green(leaf), 0.0)


         ! calculate minimum N concentrations

       N_leaf_min = g_N_conc_min(leaf) * g_dm_green(leaf)

c      N_stem_min = g_N_conc_min(sstem) * g_dm_green(sstem)
c      N_cabbage_min = g_N_conc_min(cabbage) * g_dm_green(cabbage)
c      N_stover_min = N_leaf_min + N_stem_min + N_cabbage_min

       N_conc_leaf_min = divide (N_leaf_min, g_dm_green(leaf), 0.0)

         ! calculate shortfall in N concentrations

      N_conc_ratio = divide ((N_conc_leaf - N_conc_leaf_min)
     :              , (N_conc_leaf_crit - N_conc_leaf_min), 0.0)

         ! calculate 0-1 N deficiency factors

      if (type.eq.photo) then

          N_def = 1.5 * N_conc_ratio
          sugar_nfact = bound (N_def, 0.0, 1.0)

      elseif (type.eq.expansion) then

         N_def = 1.25 * N_conc_ratio
         sugar_nfact = bound (N_def, 0.0, 1.0)

      else
         ! we have an unknown type
         write (err_messg, '(a, i5)')
     :                   ' Unknown type of N factor. Type ='
     :                   , type
         call fatal_error (err_internal, err_messg)
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_update ()
*     ===========================================================

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
*                       implicit none

*   Changes:
*      070495 nih taken from template

*   Calls:
*     accumulate
*     add_real_array
*     divide
*     pop_routine
*     push_routine
*     sugar_top_residue
*     sugar_root_incorp
*     sugar_nfact
*     sugar_N_conc_limits
*     sugar_root_distrib
*     sugar_swdef
*     sum_between
*     sum_real_array

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'convert.inc'
      include   'sugar.inc'

      real       divide                ! function
      real       sugar_nfact           ! function
      real       sugar_swdef           ! function
      real       sum_between           ! function
      real       sum_real_array        ! function

*   Internal variables
      real       dlt_dm_plant          ! dry matter increase (g/plant)
      real       dlt_leaf_area         ! leaf area increase (mm^2/plant)
      real       dlt_dm_green_dead     ! dry matter of green plant part dying
                                       ! (g/m^2)
      real       dlt_dm_senesced_dead  ! dry matter of senesced plant part
                                       ! dying (g/m^2)
      real       dlt_dm_plant_leaf     ! increase in plant leaf dm (g/plant)
      real       dlt_N_green_dead      ! N content of green plant part dying
                                       ! (g/m^2)
      real       dlt_N_senesced_dead   ! N content of senesced plant part
                                       ! dying (g/m^2)
      real       dlt_lai_dead          ! lai of green leaf of plants dying ()
      real       dlt_slai_dead         ! lai of senesced leaf of plant dying ()
      real       dm_residue            ! dry matter added to residue (g/m^2)
      real       dying_fract           ! fraction op population dying (0-1)
      real       N_residue             ! nitrogen added to residue (g/m^2)
      real       leaf_no               ! currently expanding leaf no.
      integer    part                  ! plant part index

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_update')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

         ! Note.
         ! Accumulate is used to add a value into a specified array element.
         ! If a specified increment of the element indicates a new element
         ! is started, the value is distributed proportionately between the
         ! two elements of the array

         ! Add is used to add the elements of one array into the corresponding
         ! elements of another array.

         ! now update with deltas

         ! The following table describes the transfer of material that should
         ! take place
         !                        POOLS
         !                 green senesced  dead
         ! dlt_green         +                     (incoming only)
         ! dlt_retrans       +-
         ! dlt_senesced      -      +
         ! dlt_dead          -      -       +
         ! dlt_detached             -       -      (outgoing only)

         ! transfer N

      dying_fract = divide (-g_dlt_plants, g_plants, 0.0)

      do 1000 part = 1, max_part
         dlt_N_green_dead = g_N_green(part) * dying_fract
         g_N_green(part) = g_N_green(part) - dlt_N_green_dead
         g_N_dead(part) = g_N_dead(part) + dlt_N_green_dead

         dlt_N_senesced_dead = g_N_senesced(part) * dying_fract
         g_N_senesced(part) = g_N_senesced(part) - dlt_N_senesced_dead
         g_N_dead(part) = g_N_dead(part) + dlt_N_senesced_dead
1000  continue

      call subtract_real_array (g_dlt_N_dead_detached, g_N_dead
     :                        , max_part)

      call add_real_array (g_dlt_N_green, g_N_green, max_part)
      call add_real_array (g_dlt_N_retrans, g_N_green, max_part)
      call subtract_real_array (g_dlt_N_senesced, g_N_green
     :                        , max_part)

      call add_real_array (g_dlt_N_senesced, g_N_senesced
     :                   , max_part)
      call subtract_real_array (g_dlt_N_detached, g_N_senesced
     :                        , max_part)


         ! Transfer plant dry matter

      dlt_dm_plant = divide (g_dlt_dm, g_plants, 0.0)

      call accumulate (dlt_dm_plant, g_dm_plant_top_tot
     :               , g_previous_stage, g_dlt_stage)

      do 2000 part = 1, max_part
         dlt_dm_green_dead = g_dm_green(part) * dying_fract
         g_dm_green(part) = g_dm_green(part) - dlt_dm_green_dead
         g_dm_dead(part) = g_dm_dead(part) + dlt_dm_green_dead

         dlt_dm_senesced_dead = g_dm_senesced(part) * dying_fract
         g_dm_senesced(part) = g_dm_senesced(part)
     :                       - dlt_dm_senesced_dead
         g_dm_dead(part) = g_dm_dead(part) + dlt_dm_senesced_dead
2000  continue

      call subtract_real_array (g_dlt_dm_dead_detached, g_dm_dead
     :                        , max_part)

      call add_real_array (g_dlt_dm_green, g_dm_green, max_part)
      call add_real_array (g_dlt_dm_green_retrans, g_dm_green, max_part)
      call subtract_real_array (g_dlt_dm_senesced, g_dm_green
     :                        , max_part)

      call add_real_array (g_dlt_dm_senesced, g_dm_senesced
     :                   , max_part)
      call subtract_real_array (g_dlt_dm_detached, g_dm_senesced
     :                        , max_part)

cnh
      call add_real_array (g_dlt_plant_wc, g_plant_wc, max_part)

      dm_residue = (sum_real_array (g_dlt_dm_detached, max_part)
     :           - g_dlt_dm_detached(root))
      N_residue = (sum_real_array (g_dlt_N_detached, max_part)
     :          - g_dlt_N_detached(root))

      call sugar_top_residue (dm_residue, N_residue)

             ! put roots into root residue

      call sugar_root_incorp (g_dlt_dm_detached(root)
     :                    , g_dlt_N_detached(root))



         ! transfer plant leaf area
      dlt_lai_dead  = g_lai  * dying_fract
      dlt_slai_dead = g_slai * dying_fract

      g_lai = g_lai + g_dlt_lai - dlt_lai_dead - g_dlt_slai
      g_slai = g_slai + g_dlt_slai - dlt_slai_dead - g_dlt_slai_detached
      g_tlai_dead = g_tlai_dead + dlt_lai_dead + dlt_slai_dead
     :            - g_dlt_tlai_dead_detached


         ! plant leaf development
         ! need to account for truncation of partially developed leaf (add 1)
      leaf_no = 1.0 + sum_between (emerg, now, g_leaf_no)
      dlt_leaf_area = divide (g_dlt_lai, g_plants, 0.0) * sm2smm
      call accumulate (dlt_leaf_area, g_leaf_area
     :               , leaf_no, g_dlt_leaf_no)

      dlt_dm_plant_leaf = divide (g_dlt_dm_green(leaf), g_plants, 0.0)
      call accumulate (dlt_dm_plant_leaf, g_leaf_dm
     :               , leaf_no, g_dlt_leaf_no)

      call accumulate (g_dlt_leaf_no, g_leaf_no
     :               , g_previous_stage, g_dlt_stage)

      call accumulate (g_dlt_leaf_no_dead, g_leaf_no_dead
     :               , g_previous_stage, g_dlt_stage)

         ! plant stress

      call accumulate (1.0 - sugar_swdef(photo), g_cswd_photo
     :               , g_previous_stage, g_dlt_stage)
      call accumulate (1.0 - sugar_swdef(expansion), g_cswd_expansion
     :               , g_previous_stage, g_dlt_stage)
      call accumulate (1.0 - sugar_swdef(pheno), g_cswd_pheno
     :               , g_previous_stage, g_dlt_stage)

      call accumulate (1.0 - sugar_nfact(photo), g_cnd_photo
     :               , g_previous_stage, g_dlt_stage)

         ! other plant states

      g_canopy_height = g_canopy_height + g_dlt_canopy_height
      g_plants = g_plants + g_dlt_plants
      g_root_depth = g_root_depth + g_dlt_root_depth
      call add_real_array      (g_dlt_rlv, g_rlv, max_layer)
      call subtract_real_array (g_dlt_rlv_senesced, g_rlv, max_layer)

      call sugar_N_conc_limits (g_N_conc_crit, g_N_conc_min)  ! plant N concentr

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_plant_death ()
*     ===========================================================

*   Short description:
*      Determine plant death in crop

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
*                       implicit none

*   Changes:
*       070495 nih taken from template

*   Calls:
*     pop_routine
*     push_routine
*     sugar_dm_dead_detachment
*     sugar_N_dead_detachment
*     sugar_tlai_dead_detachment
*     sugar_plants

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'sugar.inc'

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_plant_death')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      call sugar_plants (g_dlt_plants)
      call sugar_dm_dead_detachment (g_dlt_dm_dead_detached)
      call sugar_tlai_dead_detachment (g_dlt_tlai_dead_detached)
      call sugar_N_dead_detachment (g_dlt_N_dead_detached)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_plants (dlt_plants)
*     ===========================================================

*   Short description:
*      Determine plant death.

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
*                       implicit none

*   Changes:
*       070495 nih taken from template

*   Calls:
*     bound
*     pop_routine
*     push_routine
*     reals_are_equal
*     report_event
*     sugar_kill_crop
*     sugar_swdef
*     stage_is_between
*     sum_between
*     write_string

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       dlt_plants            ! (OUTPUT) change in plant number

*   Global variables
      include   'const.inc'            ! new_line
      include   'sugar.inc'

      real       bound                 ! function
      logical    reals_are_equal       ! function
      real       sugar_swdef           ! function
      logical    stage_is_between      ! function
      real       sum_between           ! function

*   Internal variables
      real       cswd_photo            ! cumulative water stress for photoperiod
      real       leaf_no               ! number of leaves
      real       killfr                ! fraction of crop population to kill
      character  string*200            ! output string

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_plants')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      cswd_photo = sum_between (emerg, crop_end, g_cswd_photo)
      leaf_no = sum_between (emerg, now, g_leaf_no)

      if (stage_is_between (sowing, sprouting, g_current_stage)
     :   .and. sum_between (sowing, now, g_days_tot)
     :         .ge.c_days_germ_limit) then

         dlt_plants = - g_plants

         write (string, '(3a, i4, a)')
     :                 ' crop failure because of lack of'
     :                  ,new_line
     :                  ,'         germination within'
     :                  , c_days_germ_limit
     :                  , ' days of sowing'
         call write_string (lu_scr_sum, string)
         call sugar_kill_crop ()

      elseif (stage_is_between (sprouting, emerg, g_current_stage)
     :       .and. sum_between (sprouting, now, g_tt_tot)
     :       .gt.c_tt_emerg_limit) then

         dlt_plants = - g_plants

         write (string, '(a)')
     :                 ' failed emergence due to deep planting'
         call write_string (lu_scr_sum, string)
         call sugar_kill_crop ()

      elseif (reals_are_equal (g_lai, 0.0)
     :       .and. stage_is_between (emerg, crop_end
     :                             , g_current_stage)) then

         dlt_plants = - g_plants

         write (string, '(3a)')
     :                ' crop failure because of total leaf senescence.'
         call write_string (lu_scr_sum, string)
         call sugar_kill_crop ()

      elseif (leaf_no.lt.c_leaf_no_crit
     :       .and. cswd_photo.gt.c_swdf_photo_limit
     :       .and. sugar_swdef(photo).lt.1.0) then

         killfr = c_swdf_photo_rate* (cswd_photo - c_swdf_photo_limit)
         killfr = bound (killfr, 0.0, 1.0)
         dlt_plants = - g_plants*killfr

         write (string, '(a, i4, a)')
     :          'plant_kill.',
     :          nint (killfr*100.0)
     :         , '% failure because of water stress.'

cnh         call report_event (string)
         call report_date_and_event (g_day_of_year,g_year,string)

      else
         dlt_plants = 0.0

      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_dm_dead_detachment (dlt_dm_dead_detached)
*     ===========================================================

*   Short description:
*      Plant dry matter loss from dead plants

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
*                       implicit none

*   Changes:
*       070495 nih taken from template

*   Calls:
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       dlt_dm_dead_detached(*)   ! (OUTPUT) change in dm of dead
                                           ! plants (g/m^2)

*   Global variables
      include   'sugar.inc'

*   Internal variables
      integer    part                  ! part index

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_dm_dead_detachment')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      do 1000 part = 1, max_part
         dlt_dm_dead_detached(part) = g_dm_dead(part)
     :                              * c_dead_detach_frac(part)
1000  continue

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_tlai_dead_detachment (dlt_tlai_dead_detached)
*     ===========================================================

*   Short description:
*      Plant leaf area loss from dead plants

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
*                       implicit none

*   Changes:
*       070495 nih taken from template

*   Calls:
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       dlt_tlai_dead_detached   ! (OUTPUT) change in lai of dead
                                          ! plants

*   Global variables
      include   'sugar.inc'

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_tlai_dead_detachment')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      dlt_tlai_dead_detached = g_tlai_dead * c_dead_detach_frac(leaf)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_N_dead_detachment (dlt_N_dead_detached)
*     ===========================================================

*   Short description:
*      Plant Nitrogen loss from dead plants

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
*                       implicit none

*   Changes:
*       070495 nih taken from template

*   Calls:
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       dlt_N_dead_detached(*)   ! (OUTPUT) change in dm of dead
                                          ! plants (g/m^2)

*   Global variables
      include   'sugar.inc'

*   Internal variables
      integer    part                  ! part index

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_N_dead_detachment')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      do 1000 part = 1, max_part
         dlt_N_dead_detached(part) = g_N_dead(part)
     :                             * c_dead_detach_frac(part)
1000  continue

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_event ()
*     ===========================================================

*   Short description:
*       Report occurence of event and the current status of specific
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
*                       implicit none

*   Changes:
*     070495 nih taken from template

*   Calls:
*     divide
*     find_layer_no
*     l_bound
*     on_day_of
*     pop_routine
*     push_routine
*     report_event
*     sugar_kill_crop
*     stage_is_between
*     sum_between
*     sum_real_array
*     write_string

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'const.inc'            ! new_line, lu_scr_sum, blank,
                                       ! lu_scr_sum
      include   'convert.inc'
      include   'sugar.inc'

      real       divide                ! function
      integer    find_layer_no         ! function
      real       l_bound               ! function
      logical    on_day_of             ! function
      logical    stage_is_between      ! function
      real       sum_real_array        ! function

*   Internal variables
      real       biomass               ! total above ground plant wt (g/m^2)
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      integer    layer                 ! profile layer number
      real       pesw_tot              ! total plant extractable sw (mm)
      real       pesw(max_layer)       ! plant extractable soil water (mm)
      real       N_green               ! plant nitrogen of tops (g/m^2)
                                       ! less flower
      real       dm_green              ! plant wt of tops (g/m^2) less flower
      integer    stage_no              ! stage number at beginning of phase
      character  string*200            ! message
      real       N_green_conc_percent  ! n% of tops less flower (incl grain)

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_event')

*   Initial data values
*       none
* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      stage_no = int (g_current_stage)
      if (on_day_of (stage_no, g_current_stage, g_days_tot)) then
             ! new phase has begun.
         write (string, '(a, f6.1, 1x, a)')
     :                   ' stage '
     :                  , c_stage_code_list(stage_no)
     :                  , c_stage_names(stage_no)
cnh         call report_event (string)
         call report_date_and_event (g_day_of_year,g_year,string)

         biomass = sum_real_array (g_dm_green, max_part)
     :           - g_dm_green(root)

     :           + sum_real_array (g_dm_senesced, max_part)
     :           - g_dm_senesced(root)

     :           + sum_real_array (g_dm_dead, max_part)
     :           - g_dm_dead(root)

         dm_green = sum_real_array (g_dm_green, max_part)
     :            - g_dm_green(root)
         N_green = sum_real_array (g_N_green, max_part)
     :           - g_N_green(root)

         N_green_conc_percent = divide (N_green, dm_green, 0.0)
     :                        * fract2pcnt

         deepest_layer = find_layer_no (g_root_depth, g_dlayer
     :                                , max_layer)
         do 1000 layer = 1, deepest_layer
            pesw(layer) = g_sw_dep(layer) - p_ll_dep(layer)
            pesw(layer) = l_bound (pesw(layer), 0.0)
1000     continue
         pesw_tot = sum_real_array (pesw, deepest_layer)

         if (stage_is_between (emerg, crop_end, g_current_stage)) then
            write (string, '(2(a, g16.7e2), a, 2(a, g16.7e2))')
     :              '                     biomass =       '
     :            , biomass
     :            , '   lai = '
     :            , g_lai
     :            , new_line
     :            ,'                     stover N conc ='
     :            , N_green_conc_percent
     :            , '   extractable sw ='
     :            , pesw_tot
            call write_string (lu_scr_sum, string)
         else
         endif

      else
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_harvest ()
*     ===========================================================

*   Short description:
*       Report occurence of harvest and the current status of specific
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
*                       implicit none

*   Changes:
*     070495 nih taken from template

*   Calls:
*     divide
*     find_layer_no
*     l_bound
*     on_day_of
*     pop_routine
*     push_routine
*     report_event
*     sugar_kill_crop
*     stage_is_between
*     sum_between
*     sum_real_array
*     write_string

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'const.inc'            ! new_line, lu_scr_sum, blank,
                                       ! lu_scr_sum
      include   'convert.inc'          ! gm2kg, sm2ha, sm2smm
      include   'sugar.inc'

      real       divide                ! function
      real       sum_between           ! function
      real       sum_real_array        ! function
      character  string_concat*50      ! function

*   Internal variables
      real       biomass_dead          ! above ground dead plant wt (kg/ha)
      real       biomass_green         ! above ground green plant wt (kg/ha)
      real       biomass_senesced      ! above ground senesced plant wt (kg/ha)
      character  cultivar_ratoon*30    !
      real       dm                    ! above ground total dry matter (kg/ha)
      real       leaf_no               ! total leaf number
      real       N_dead                ! above ground dead plant N (kg/ha)
      real       N_green               ! above ground green plant N (kg/ha)
      real       N_senesced            ! above ground senesced plant N (kg/ha)
      real       N_total               ! total gross nitrogen content (kg/ha)
      integer    phase                 ! phenological phase number
      real       si1                   ! mean water stress type 1
      real       si2                   ! mean water stress type 2
      real       si4                   ! mean nitrogen stress type 1
      character  string*400            ! message

      real       dm_root
      real       N_root
      real       dm_residue
      real       N_residue

      integer layer
      real hold_ratoon_no
      real hold_dm_root
      real hold_n_root
      real hold_num_layers
      real hold_root_depth
      real hold_plants
      real hold_rlv(max_layer)

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_harvest')

*   Initial data values
*       none
* --------------------- Executable code section ----------------------

      call push_routine (my_name)

          ! crop harvested. Report status

      biomass_green = (sum_real_array (g_dm_green, max_part)
     :              - g_dm_green(root))
     :              * gm2kg / sm2ha

      biomass_senesced = (sum_real_array (g_dm_senesced, max_part)
     :                 - g_dm_senesced(root))
     :                 * gm2kg / sm2ha

      biomass_dead = (sum_real_array (g_dm_dead, max_part)
     :             - g_dm_dead(root))
     :             * gm2kg / sm2ha

      dm = (biomass_green + biomass_senesced + biomass_dead)

      leaf_no = sum_between (emerg, now, g_leaf_no)

      N_green = (sum_real_array (g_N_green, max_part)
     :        - g_N_green(root))
     :        * gm2kg / sm2ha

      N_senesced = (sum_real_array (g_N_senesced, max_part)
     :           - g_N_senesced(root))
     :           * gm2kg / sm2ha

      N_dead = (sum_real_array (g_N_dead, max_part)
     :       - g_N_dead(root))
     :       * gm2kg / sm2ha

      N_total = N_green + N_senesced + N_dead

      call write_string (lu_scr_sum, new_line//new_line)

      write (string, '(a,i4)')
     :            ' flowering day  = ',g_isdate
      call write_string (lu_scr_sum, string)

      write (string, '(a,f6.3)')
     :            ' maximum lai =', g_lai_max
      call write_string (lu_scr_sum, string)

      write (string, '(a,f10.1)')
     :            ' total above ground biomass (kg/ha) =', dm
      call write_string (lu_scr_sum, string)

      write (string, '(a,f10.1)')
     :         ' live above ground biomass (kg/ha) =', biomass_green
     :                                               + biomass_senesced
      call write_string (lu_scr_sum, string)

      write (string, '(a,f10.1)')
     :            ' green above ground biomass (kg/ha) =', biomass_green
      call write_string (lu_scr_sum, string)

      write (string, '(a,f10.1)')
     :      ' senesced above ground biomass (kg/ha) =', biomass_senesced
      call write_string (lu_scr_sum, string)

      write (string, '(a,f10.1)')
     :            ' dead above ground biomass (kg/ha) =', biomass_dead
      call write_string (lu_scr_sum, string)

      write (string, '(a,f6.1)')
     :            ' number of leaves =', leaf_no
      call write_string (lu_scr_sum, string)

      write (string, '(a,f10.2,t40,a,f10.2)')
     :            ' total N content (kg/ha) =', N_total
     :          , ' senesced N content (kg/ha) =', N_senesced

      call write_string (lu_scr_sum, string)

      write (string, '(a,f10.2,t40,a,f10.2)')
     :            ' green N content (kg/ha) =', N_green
     :          , ' dead N content (kg/ha) =', N_dead
      call write_string (lu_scr_sum, string)

      do 2000 phase = emerg_to_begcane, flowering_to_crop_end
         si1 = divide (g_cswd_photo(phase)
     :               , g_days_tot(phase), 0.0)
         si2 = divide (g_cswd_expansion(phase)
     :               , g_days_tot(phase), 0.0)
         si4 = divide (g_cnd_photo(phase)
     :               , g_days_tot(phase), 0.0)

         call write_string (lu_scr_sum, new_line//new_line)

         write (string,'(2a)')
     :         ' stress indices for ', c_stage_names(phase)
         call write_string (lu_scr_sum, string)

         write (string,'(2(a, g16.7e2))')
     :         ' water stress 1 =', si1
     :         , '   nitrogen stress 1 =', si4
         call write_string (lu_scr_sum, string)

         write (string,'(a, g16.7e2)')
     :         ' water stress 2 =', si2
         call write_string (lu_scr_sum, string)
2000  continue

      ! the following is a copy/adaption of sugar_end_crop

      if (g_crop_status.ne.crop_out) then

                ! report

             ! now do post harvest processes

         dm_root = g_dm_green(root)* c_root_die_back_fr
     :           + g_dm_dead(root)
     :           + g_dm_senesced(root)

         N_root  = g_N_green(root)* c_root_die_back_fr
     :           + g_N_dead(root)
     :           + g_N_senesced(root)


             ! put stover into surface residue

         dm_residue = (sum_real_array (g_dm_green, max_part)
     :              - g_dm_green(root) - g_dm_green(sstem)
     :              - g_dm_green(sucrose))

     :              + (sum_real_array (g_dm_senesced, max_part)
     :              - g_dm_senesced(root))

     :              + (sum_real_array (g_dm_dead, max_part)
     :              - g_dm_dead(root))

         N_residue = (sum_real_array (g_N_green, max_part)
     :             - g_N_green(root) - g_N_green (sstem)
     :             - g_N_green(sucrose))

     :             + (sum_real_array (g_N_senesced, max_part)
     :             - g_N_senesced(root))

     :             + (sum_real_array (g_N_dead, max_part)
     :             - g_N_dead(root))


         write (string, '(40x, a, f7.1, a, 3(a, 40x, a, f6.1, a))')
     :                  '  straw residue ='
     :                  , dm_residue * gm2kg /sm2ha, ' kg/ha'
     :                  , new_line
     :                  , '  straw N = '
     :                  , N_residue * gm2kg /sm2ha, ' kg/ha'

     :                  , new_line
     :                  , '  root residue = '
     :                  , dm_root * gm2kg /sm2ha, ' kg/ha'
     :                  , new_line
     :                  , '  root N = '
     :                  , N_root * gm2kg /sm2ha, ' kg/ha'

         call write_string (lu_scr_sum, string)

         call sugar_root_incorp (dm_root, N_root)
         call sugar_top_residue (dm_residue, N_residue)

         hold_ratoon_no = g_ratoon_no
         hold_dm_root   = g_dm_green (root)*(1.0 - c_root_die_back_fr)
         hold_n_root    = g_N_green (root)*(1.0 - c_root_die_back_fr)
         hold_num_layers= g_num_layers
         hold_root_depth= g_root_depth
         hold_plants    = g_plants
         do 101 layer=1,max_layer
            hold_rlv(layer) = g_rlv(layer)*(1.0 - c_root_die_back_fr)
  101    continue


         call sugar_zero_globals ()
         call sugar_zero_daily_variables ()

         g_current_stage   = real (sprouting)
         g_ratoon_no       = hold_ratoon_no +1
         g_dm_green (root) = hold_dm_root
         g_N_green (root)  = hold_n_root
         g_num_layers      = hold_num_layers
         g_root_depth      = hold_root_depth
         g_plants          = hold_plants
         do 102 layer=1,max_layer
            g_rlv(layer) = hold_rlv(layer)
  102    continue

         ! now update constants if need be
         If (g_ratoon_no .eq. 1) then

            call sugar_read_crop_constants ('ratoon_crop')

            cultivar_ratoon = string_concat(g_crop_cultivar
     :                                     ,'_ratoon')
            call sugar_read_cultivar_params
     :         (cultivar_ratoon)

         else
            ! only need to update constants when we move from a plant
            ! crop to a ratoon crop.
         endif
      else
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_N_init (N_green)
*     ===========================================================

*   Short description:
*       Set plant nitrogen

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
*     060495 nih taken from template

*   Calls:
*     on_day_of
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       N_green(*)            ! plant nitrogen (g/m^2)

*   Global variables
      include   'sugar.inc'

      logical    on_day_of             ! function

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_N_init')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      if (on_day_of (emerg, g_current_stage, g_days_tot)) then
         N_green(root) = c_N_root_init_conc*g_dm_green(root)
         N_green(sstem) = c_N_sstem_init_conc*g_dm_green(sstem)
         N_green(leaf) = c_N_leaf_init_conc*g_dm_green(leaf)
         N_green(cabbage) = c_N_cabbage_init_conc*g_dm_green(cabbage)
         N_green(sucrose) = 0.0

      else
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_check_sw ()
*     ===========================================================

*   Short description:
*       Check validity of soil water parameters for all soil profile layers.

*   Assumptions:
*       none

*   Notes:
*           Reports an error if
*           - ll_dep and dul_dep are not in ascending order
*           - ll is below c_minsw
*           - sw < c_minsw

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*     060495 nih taken from template

*   Calls:
*     divide
*     pop_routine
*     push_routine
*     warning_error

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'const.inc'            ! err_internal
      include   'sugar.inc'

      real       divide                ! function

*   Internal variables
      real       dul                   ! drained upper limit water content
                                       ! of layer (mm water/mm soil)
      character  err_messg*200         ! error message
      integer    layer                 ! layer number
      real       ll                    ! lower limit water content
                                       ! of layer (mm water/mm soil)
      real       sw                    ! soil water content of layer l
                                       ! (mm water/mm soil)

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_check_sw')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      do 2000 layer = 1, g_num_layers

         sw = divide (g_sw_dep(layer), g_dlayer(layer), 0.0)
         dul = divide (g_dul_dep(layer), g_dlayer(layer), 0.0)
         ll = divide (p_ll_dep(layer), g_dlayer(layer), 0.0)

         if (ll.lt.c_minsw) then
            write (err_messg, '(a,f8.2,a,i3,2a,f8.2)')
     :           ' lower limit of ', ll
     :          ,' in layer ', layer
     :          , new_line
     :          ,'         is below acceptable value of ', c_minsw
            call warning_error (err_internal, err_messg)
         else
         endif

         if (dul.le.ll) then
            write (err_messg, '(a,f8.2,a,i3,2a,f8.2)')
     :            ' Drained upper limit of ',dul
     :           ,' in layer ', layer
     :           ,new_line
     :           ,'         is at or below lower limit of ', ll
            call warning_error (err_internal, err_messg)
         else
         endif

         if (sw.lt.c_minsw) then
            write (err_messg, '(a,f8.2,a,i3,2a,f8.2)')
     :            ' Soil water of ', sw
     :           ,' in layer ', layer
     :           ,new_line
     :           ,'         is below acceptable value of ', c_minsw
            call warning_error (err_internal, err_messg)

         else
         endif
2000  continue

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_zero_variables ()
*     ===========================================================

*   Short description:
*       Zero crop variables & arrays

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
*                       implicit none

*   Changes:
*     060495 nih taken from template

*   Calls:
*     fill_real_array
*     pop_routine
*     push_routine
*     sugar_zero_daily_variables

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'sugar.inc'

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_zero_variables')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

          ! zero pools etc.
      call sugar_zero_globals ()
      call sugar_zero_daily_variables ()
      call sugar_zero_parameters ()

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_zero_daily_variables ()
*     ===========================================================

*   Short description:
*       Zero crop daily variables & arrays

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
*                       implicit none

*   Changes:
*     060495 nih taken from template

*   Calls:
*     fill_real_array
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'sugar.inc'

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_zero_daily_variables')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

          ! zero pools etc.

      call fill_real_array (g_dlt_dm_green, 0.0, max_part)
      call fill_real_array (g_dlt_dm_green_retrans, 0.0, max_part)
      call fill_real_array (g_dlt_N_green, 0.0, max_part)
      call fill_real_array (g_dlt_N_retrans, 0.0, max_part)
      call fill_real_array (g_dlt_NO3gsm, 0.0, max_layer)
      call fill_real_array (g_dlt_sw_dep, 0.0, max_layer)
      call fill_real_array (g_dm_green_demand, 0.0, max_part)
      call fill_real_array (g_N_demand, 0.0, max_part)

      call fill_real_array (g_dlt_dm_dead_detached, 0.0, max_part)
      call fill_real_array (g_dlt_dm_detached, 0.0, max_part)
      call fill_real_array (g_dlt_dm_senesced, 0.0, max_part)
      call fill_real_array (g_dlt_N_dead_detached, 0.0, max_part)
      call fill_real_array (g_dlt_N_detached, 0.0, max_part)
      call fill_real_array (g_dlt_N_senesced, 0.0, max_part)
      call fill_real_array (g_sw_avail, 0.0, max_layer)
      call fill_real_array (g_sw_avail_pot, 0.0, max_layer)
      call fill_real_array (g_sw_supply, 0.0, max_layer)


      g_dlt_tlai_dead_detached = 0.0
      g_dlt_slai_detached = 0.0
      g_dlt_canopy_height = 0.0
      g_dlt_dm = 0.0
      g_partition_xs = 0.0
      g_dlt_leaf_no = 0.0
      g_dlt_leaf_no_dead = 0.0
      g_dlt_plants = 0.0
      g_dlt_root_depth = 0.0
      g_dlt_slai = 0.0
      g_dlt_stage = 0.0
      g_dlt_lai = 0.0
      g_dlt_tt = 0.0

      g_sw_demand = 0.0

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_init ()
*     ===========================================================

*   Short description:
*       Crop initialisation

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
*                       implicit none

*   Changes:
*     060495 nih taken from template

*   Calls:
*     pop_routine
*     push_routine
*     report_event
*     sugar_read_constants
*     sugar_version

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'sugar.inc'

      character  sugar_version*20      ! function

*   Internal variables
*       none

*   Constant values

      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_init')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      call sugar_zero_variables ()
      call report_date_and_event (g_day_of_year,g_year,
     :                 ' Initialising, Version : '
     :                  // sugar_version ())

           ! initialize crop variables

      call sugar_read_constants ()

      g_current_stage = real (crop_end)
      g_crop_status = crop_out

      call sugar_get_other_variables ()

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_start_crop ()
*     ===========================================================

*   Short description:
*       Start crop using parameters specified in passed record

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
*                       implicit none

*   Changes:
*     060495 nih taken from template
*     041095 nih changed start of ratton crop from emergence to sprouting
*     060696 nih changed extract routines to collect routine calls
*                removed datastring from argument list

*   Calls:
*     collect_char_var
*     collect_real_var
*     fatal_error
*     pop_routine
*     push_routine
*     sugar_read_cultivar_params
*     sugar_read_root_params
*     write_string

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*     none

*   Global variables
      include   'const.inc'            ! lu_scr_sum, blank
      include   'sugar.inc'
      character string_concat*50       ! function

*   Internal variables
      character  cultivar*20           ! name of cultivar
      integer    numvals               ! number of values found in array
      character  string*200            ! output string
      character  cultivar_ratoon*30    ! name of cultivar ratoon section

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_start_crop')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      ! request and receive variables from owner-modules
      call sugar_get_other_variables ()


cnh      call report_event ( 'Sowing initiate')
         call report_date_and_event
     :           (g_day_of_year,g_year,'Sowing initiate')


         call collect_real_var ('plants', '()'
     :                        , g_plants, numvals, 0.0, 100.0)

         call collect_integer_var_optional ('ratoon', '()'
     :                        , g_ratoon_no, numvals, 0, 10)
         if (numvals.eq.0) then
            g_ratoon_no = 0
         else
         endif

         call collect_real_var ('sowing_depth', '(mm)'
     :                        , g_sowing_depth, numvals
     :                        , 0.0, 500.0)

         call collect_char_var ('cultivar', '()'
     :                        , cultivar, numvals)

             ! report

         call write_string (lu_scr_sum, new_line//new_line)

         string = '                 Crop Sowing Data'
         call write_string (lu_scr_sum, string)

         string = '    ------------------------------------------------'
         call write_string (lu_scr_sum, string)

         call write_string (lu_scr_sum
     :                    , '    Sowing  Depth Plants Cultivar')

         call write_string (lu_scr_sum
     :                    , '    Day no   mm     m^2    Name   ')

         string = '    ------------------------------------------------'
         call write_string (lu_scr_sum, string)

         write (string, '(3x, i7, 2f7.1, 1x, a10)')
     :                   g_day_of_year, g_sowing_depth
     :                 , g_plants, cultivar
         call write_string (lu_scr_sum, string)

         string = '    ------------------------------------------------'
         call write_string (lu_scr_sum, string)

                 ! get cultivar parameters

         if (g_ratoon_no .eq. 0) then
            call sugar_read_crop_constants ('plant_crop')
            call sugar_read_cultivar_params (cultivar)
         else
            call sugar_read_crop_constants ('ratoon_crop')
            cultivar_ratoon = string_concat(cultivar,'_ratoon')
            call sugar_read_cultivar_params (cultivar_ratoon)
         endif

                 ! get root profile parameters

         call sugar_read_root_params ()

         if (g_ratoon_no.eq.0) then
            g_current_stage = real (sowing)
         else
            g_current_stage = real (sprouting)
         endif

         g_crop_status = crop_alive
         g_crop_cultivar = cultivar

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_read_cultivar_params (section_name)
*     ===========================================================

*   Short description:
*       Crop initialisation - reads constants from constants file

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
*                       implicit none

*   Changes:
*       25-07-96 - NIH/MJR added sucrose/water stress partitioning factor

*   Calls:
*     pop_routine
*     push_routine
*     read_integer_var
*     read_real_var
*     read_real_array
*     sugar_version
*     write_string

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      character section_name*(*)

*   Global variables
      include   'const.inc'
      include   'sugar.inc'

*   Internal variables
      integer    numvals               ! number of values returned

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_read_cultivar_params')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      call write_string (lu_scr_sum
     :   ,new_line//'    - Reading constants from '//section_name)

         !    sugar_leaf_size

      call read_real_array (section_name
     :                     , 'leaf_size', max_table, '()'
     :                     , c_leaf_size, c_num_leaf_size
     :                     , 1000.0, 100000.0)

      call read_real_array (section_name
     :                     , 'leaf_size_no', max_table, '()'
     :                     , c_leaf_size_no, c_num_leaf_size
     :                     , 0.0, real(max_leaf))

      call read_real_var (section_name
     :                    , 'cane_fraction', '()'
     :                    , c_cane_fraction, numvals
     :                    , 0.0, 1.0)

      call read_real_var (section_name
     :                    , 'sucrose_fraction', '()'
     :                    , c_sucrose_fraction, numvals
     :                    , 0.0, 1.0)

      call read_real_var (section_name
     :                    , 'sucrose_delay', '()'
     :                    , c_sucrose_delay, numvals
     :                    , 0.0, 2000.)

      call read_real_var (section_name
     :                    , 'min_sstem_sucrose', '(g/m2)'
     :                    , c_min_sstem_sucrose, numvals
     :                    , 0.0, 5000.)

      call read_real_var (section_name
     :                    , 'tt_emerg_to_begcane', '()'
     :                    , p_tt_emerg_to_begcane, numvals
     :                    , 0.0, c_tt_emerg_to_begcane_ub)

      call read_real_var (section_name
     :                    , 'tt_begcane_to_flowering', '()'
     :                    , p_tt_begcane_to_flowering, numvals
     :                    , 0.0, c_tt_begcane_to_flowering_ub)

      call read_real_var (section_name
     :                    , 'tt_flowering_to_crop_end', '()'
     :                    , p_tt_flowering_to_crop_end, numvals
     :                    , 0.0, c_tt_flowering_to_crop_end_ub)

         !    sugar_leaf_death


      call read_real_var  (section_name
     :                    , 'green_leaf_no', '()'
     :                    , c_green_leaf_no, numvals
     :                    , 0.0, real(max_leaf))

         !    sugar_leaf_size

      call read_real_array (section_name
     :                     , 'tillerf_leaf_size', max_table, '()'
     :                     , c_tillerf_leaf_size,c_num_tillerf_leaf_size
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                  , 'tillerf_leaf_size_no', max_table, '()'
     :                  , c_tillerf_leaf_size_no,c_num_tillerf_leaf_size
     :                  , 0.0, real(max_leaf))

         !    sucrose partitioning adjustment for stressed conditions

      call read_real_array (section_name
     :                     , 'x_swdef_cellxp', max_table, '()'
     :                     , c_x_swdef_cellxp,c_num_x_swdef_cellxp
     :                     , 0.0, 1.0)

      call read_real_array (section_name
     :                  , 'y_sw_fac_sucrose', max_table, '()'
     :                  , c_y_sw_fac_sucrose,c_num_x_swdef_cellxp
     :                  , 0.0, 10.0)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_read_root_params ()
*     ===========================================================

*   Short description:
*       Get root profile parameters

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
*                       implicit none

*   Changes:
*       060495 nih taken from template


*   Calls:
*     fill_real_array
*     pop_routine
*     push_routine
*     read_real_array
*     write_string

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'const.inc'            ! new_line, lu_scr_sum, blank,
                                       ! lu_scr_sum, Err_User
      include   'sugar.inc'            ! dlayer(max_layer)

*   Internal variables
      integer    layer                 ! layer number
      real       ll (max_layer)        ! lower limit of plant-extractable
                                       ! soil water for soil layer l
                                       ! (mm water/mm soil)
      integer    num_layers            ! number of layers in profile
      integer    numvals               !
      character  string*200            ! output string

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_read_root_params')

      character  section_name*(*)
      parameter (section_name = 'parameters')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      call write_string (lu_scr_sum
     :                  ,new_line
     :                  //'   - Reading root profile parameters')


      call read_char_var   (section_name
     :                     , 'uptake_source'
     :                     , '()'
     :                     , g_uptake_source
     :                     , numvals)

      call read_real_array (section_name
     :                     , 'xf', max_layer, '()'
     :                     , p_xf, num_layers
     :                     , 0.0, 1.0)

         !       sugar_sw_supply

      call read_real_array (section_name
     :                     , 'll', max_layer, '()'
     :                     , ll, num_layers
     :                     , 0.0, c_ll_ub)

      call fill_real_array (p_ll_dep, 0.0, max_layer)
      do 1000 layer = 1, num_layers
         p_ll_dep(layer) = ll(layer)*g_dlayer(layer)
1000  continue

      call read_real_array (section_name
     :                     , 'kl', max_layer, '()'
     :                     , p_kl, num_layers
     :                     , 0.0, c_kl_ub)

      call read_real_array (section_name
     :                     , 'rlv', max_layer, '()'
     :                     , g_rlv, num_layers
     :                     , 0.0, 20.0)

      if (g_uptake_source.eq.'calc') then

          ! report
         call write_string (lu_scr_sum, new_line//new_line)
         call write_string (lu_scr_sum,
     :      'Sugar module is calculating its own soil uptakes')
         call write_string (lu_scr_sum, new_line//new_line)

      else if (g_uptake_source .eq. 'apsim') then

          ! report
         call write_string (lu_scr_sum, new_line//new_line)
         call write_string (lu_scr_sum,
     :   'Sugar module is using uptakes provided from another module')
         call write_string (lu_scr_sum, new_line//new_line)

      else
         ! the user has not specified 'calc' or 'apsim'
         ! so give out an error message
         call fatal_error (ERR_USER, 'Bad value for uptake_source')
      endif

         write (string,'(4x, a)') '                Root Profile'
         call write_string (lu_scr_sum, string)

         string = '  --------------------------------------------------'
         call write_string (lu_scr_sum, string)

         string = '    Layer depth  Kl factor   Lower limit Root Factor'
         call write_string (lu_scr_sum, string)

         string = '         (mm)         ()        (mm/mm)     (0-1)'
         call write_string (lu_scr_sum, string)

         string = '  --------------------------------------------------'
         call write_string (lu_scr_sum, string)

         do 2000 layer = 1, num_layers
            write (string,'(1x, 4f12.3)')
     :            g_dlayer(layer)
     :          , p_kl(layer)
     :          , ll(layer)
     :          , p_xf(layer)
            call write_string (lu_scr_sum, string)
2000     continue

         string = '   -------------------------------------------------'
         call write_string (lu_scr_sum, string)

         call write_string (lu_scr_sum, new_line//new_line)


      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_kill_crop ()
*     ===========================================================

*   Short description:
*       Kill crop

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
*                       implicit none

*   Changes:
*       070495 nih taken from template

*   Calls:
*     pop_routine
*     push_routine
*     report_event
*     sum_real_array

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'convert.inc'          ! gm2kg, sm2ha, mm2cm, cmm2cc
      include   'sugar.inc'

      real       sum_real_array        ! function

*   Internal variables
      real       biomass               ! above ground dm (kg/ha)
      character  string*200            ! output string

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_kill_crop')


*   Initial data values
*     none

* --------------------- Executable code section ----------------------

c+!!!!!! fix problem with deltas in update when change from alive to dead ?zero
      call push_routine (my_name)

      if (g_crop_status.eq.crop_alive) then
         g_crop_status = crop_dead

         biomass = (sum_real_array (g_dm_green, max_part)
     :           - g_dm_green(root)) * gm2kg /sm2ha

     :           + (sum_real_array (g_dm_senesced, max_part)
     :           - g_dm_senesced(root)) * gm2kg /sm2ha

     :           + (sum_real_array (g_dm_dead, max_part)
     :           - g_dm_dead(root)) * gm2kg /sm2ha


                ! report

         write (string, '(3x, a, f7.1, a)')
     :                  ' crop_kill. Standing above-ground dm = '
     :                  , biomass, ' (kg/ha)'
cnh         call report_event (string)
         call report_date_and_event (g_day_of_year,g_year,string)
         
      else
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_end_crop ()
*     ===========================================================

*   Short description:
*       End crop

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
*                       implicit none

*   Changes:
*       070495 nih taken from template

*   Calls:
*     pop_routine
*     push_routine
*     report_event
*     sugar_top_residue
*     sugar_root_incorp
*     sugar_root_distrib
*     sum_real_array
*     write_string

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'const.inc'            ! new_line, lu_scr_sum
      include   'convert.inc'          ! gm2kg, sm2ha, mm2cm, cmm2cc
      include   'sugar.inc'

      real       sum_real_array        ! function

*   Internal variables
      real       dm_residue            ! dry matter added to residue (g/m^2)
      real       N_residue             ! nitrogen added to residue (g/m^2)
      real       dm_root               ! dry matter added to soil (g/m^2)
      real       N_root                ! nitrogen added to soil (g/m^2)
      character  string*400            ! output string

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_end_crop')

*   Initial data values
*     none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      if (g_crop_status.ne.crop_out) then
         g_crop_status = crop_out
         g_current_stage = real (crop_end)

                ! report

             ! now do post harvest processes

         dm_root = g_dm_green(root)
     :           + g_dm_dead(root)
     :           + g_dm_senesced(root)

         N_root  = g_N_green(root)
     :           + g_N_dead(root)
     :           + g_N_senesced(root)

         call sugar_root_incorp (dm_root, N_root)

             ! put stover into surface residue

         dm_residue = (sum_real_array (g_dm_green, max_part)
     :              - g_dm_green(root))

     :              + (sum_real_array (g_dm_senesced, max_part)
     :              - g_dm_senesced(root))

     :              + (sum_real_array (g_dm_dead, max_part)
     :              - g_dm_dead(root))

         N_residue = (sum_real_array (g_N_green, max_part)
     :             - g_N_green(root))

     :             + (sum_real_array (g_N_senesced, max_part)
     :             - g_N_senesced(root))

     :             + (sum_real_array (g_N_dead, max_part)
     :             - g_N_dead(root))

         call sugar_top_residue (dm_residue, N_residue)

         write (string, '(40x, a, f7.1, a, 3(a, 40x, a, f6.1, a))')
     :                  '  straw residue ='
     :                  , dm_residue * gm2kg /sm2ha, ' kg/ha'
     :                  , new_line
     :                  , '  straw N = '
     :                  , N_residue * gm2kg /sm2ha, ' kg/ha'

     :                  , new_line
     :                  , '  root residue = '
     :                  , dm_root * gm2kg /sm2ha, ' kg/ha'
     :                  , new_line
     :                  , '  root N = '
     :                  , N_root * gm2kg /sm2ha, ' kg/ha'

         call write_string (lu_scr_sum, string)

      else
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_root_distrib (root_array, root_sum)
*     ===========================================================

*   Short description:
*       Distribute root material over profile

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
*                       implicit none

*   Changes:
*       070495 nih taken from template

*   Calls:
*     divide
*     fill_real_array
*     find_layer_no
*     pop_routine
*     push_routine
*     sum_real_array
*     u_bound

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       root_array(*)         ! (OUTPUT) array to contain
                                       ! distributed material
      real       root_sum              ! (INPUT) Material to be distributed

*   Global variables
      include   'sugar.inc'

      real       divide                ! function
      integer    find_layer_no         ! function
      real       sum_real_array        ! function

*   Internal variables
      integer    layer                 ! layer number ()
      integer    deepest_layer         ! deepest layer in which the
                                       ! roots are growing
      real       root_length(max_layer) ! root length (mm/m2)
      real       root_length_sum       ! sum of root distribution array

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_root_distrib')

*   Initial data values
*     none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)
             ! distribute roots over profile to root_depth

      call fill_real_array (root_array, 0.0, max_layer)
      call fill_real_array (root_length, 0.0, max_layer)

      deepest_layer = find_layer_no (g_root_depth, g_dlayer
     :                                , max_layer)
      do 1000 layer = 1, deepest_layer
         root_length(layer) = g_rlv(layer) * g_dlayer(layer)
1000  continue

      root_length_sum = sum_real_array (root_length, deepest_layer)

      do 2000 layer = 1, deepest_layer
         root_array(layer) = root_sum * divide (root_length(layer)
     :                                        , root_length_sum, 0.0)
2000  continue

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_top_residue (dlt_residue_weight, dlt_residue_N)
*     ===========================================================

*   Short description:
*       Add residue to residue pool

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
*                       implicit none

*   Changes:
*       070495 nih taken from template
*       070596 nih changed to use postbox message method

*   Calls:
*     message_pass_to_module
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       dlt_residue_weight    ! (INPUT) new surface residue (g/m^2)
      real       dlt_residue_N         ! (INPUT) new surface residue N (g/m^2)

*   Global variables
      include   'const.inc'            ! all_active_modules
      include   'convert.inc'
      include   'sugar.inc'

*   Internal variables
cnh      character  string*200            ! output string

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_top_residue')

*   Initial data values
*     none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      if (dlt_residue_weight.gt.0.0) then
            ! send out surface residue

cnh         write(string, '(2a, 2(a, g16.7e3, a))' )
c     :           'dlt_residue_type = ', c_crop_type
c     :         , ', dlt_residue_wt = '
c     :         , dlt_residue_weight * gm2kg /sm2ha, '(kg/ha)'
c     :         , ', dlt_residue_n = '
c     :         , dlt_residue_N * gm2kg /sm2ha, '(kg/ha)'
c
c         call message_pass_to_module
cnh     :           (all_active_modules, 'add_residue', string)

         call New_postbox ()

         call post_char_var('dlt_residue_type','()',c_crop_type)

         call post_real_var ('dlt_residue_wt'
     :                        ,'(kg/ha)'
     :                        ,dlt_residue_weight* gm2kg /sm2ha)

         call post_real_var ('dlt_residue_n'
     :                        ,'(kg/ha)'
     :                        ,dlt_residue_N * gm2kg /sm2ha)

         call message_send_immediate (
     :                              unknown_module
     :                            , 'add_residue'
     :                            , Blank
     :                            )
     
         call Delete_postbox ()

      else
         ! no surface residue
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_root_incorp (dlt_dm_root, dlt_N_root)
*     ===========================================================

*   Short description:
*       Add root residue to root residue pool

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
*                       implicit none

*   Changes:
*       070495 nih taken from template
*       070596 nih changed to use postbox message method
*       100696 jngh removed unused strings and functions

*   Calls:
*     find_layer_no
*     lastnb
*     message_pass_to_module
*     pop_routine
*     push_routine
*     string_concat

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       dlt_dm_root           ! (INPUT) new root residue dm (g/m^2)
      real       dlt_N_root            ! (INPUT) new root residue N (g/m^2)

*   Global variables
      include   'const.inc'            ! all_active_modules
      include   'convert.inc'
      include   'sugar.inc'

      integer    find_layer_no         ! function
cjh      integer    lastnb                ! function
cjh      character  string_concat*(1000) ! function

*   Internal variables
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      real       dlt_dm_incorp(max_layer) ! root residue (kg/ha)
      real       dlt_N_incorp(max_layer)  ! root residue N (kg/ha)

cjh      integer    layer                 ! layer number
cjh      character  string*(1000) ! output string

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_root_incorp')


*   Initial data values
*     none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      if (dlt_dm_root.gt.0.0) then

            ! send out root residue

         call sugar_root_distrib (dlt_dm_incorp
     :                          , dlt_dm_root * gm2kg /sm2ha)
         call bound_check_real_array (dlt_dm_incorp
     :                        , 0.0
     :                        , dlt_dm_root * gm2kg/sm2ha
     :                        , 'dlt_dm_incorp'
     :                        , max_layer)

         call sugar_root_distrib (dlt_N_incorp
     :                          , dlt_N_root * gm2kg /sm2ha)
         call bound_check_real_array (dlt_n_incorp
     :                        , 0.0
     :                        , dlt_n_root * gm2kg/sm2ha
     :                        , 'dlt_n_incorp'
     :                        , max_layer)

         deepest_layer = find_layer_no (g_root_depth, g_dlayer
     :                                , max_layer)

cnh         string = 'dlt_fom_type='// c_crop_type
c
c         write (string(lastnb(string)+1:), '(a, 20g16.7e3)' )
c     :              ', dlt_fom_wt = '
c     :               , (dlt_dm_incorp(layer), layer = 1, deepest_layer)
c         string =  string_concat (string, '(kg/ha)')
c
c         write (string(lastnb(string)+1:), '(a, 20g16.7e3)')
c     :              ', dlt_fom_n = '
c     :               , (dlt_N_incorp(layer), layer = 1, deepest_layer)
c         string = string_concat (string, '(kg/ha)')
c
c         call message_pass_to_module
c     :           (all_active_modules, 'incorp_fom', string)

         call New_postbox ()

         call post_char_var('dlt_fom_type=','()',c_crop_type)

         call post_real_array ('dlt_fom_wt'
     :                        ,'(kg/ha)'
     :                        ,dlt_dm_incorp
     :                        ,deepest_layer)

         call post_real_array ('dlt_fom_n'
     :                        ,'(kg/ha)'
     :                        ,dlt_n_incorp
     :                        ,deepest_layer)

         call message_send_immediate (
     :                              unknown_module
     :                            , 'incorp_fom'
     :                            , Blank
     :                            )
     
         call Delete_postbox ()


      else
         ! no roots to incorporate
      endif

      call pop_routine (my_name)
      return
      end
*     ================================================================
      subroutine sugar_get_other_variables ()
*     ================================================================

*   Short description:
*      Get the values of variables/arrays from other modules.

*   Assumptions:

*   Notes:
*      none

*   Procedure attributes:
*      Version:         any hardware/fortran77
*      Extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       implicit none

*   Changes:
*     060495 nih taken from template
*     210896 nih added module name as suffice to intercepted radiation

*   Calls:
*     add_real_array
*     divide
*     fill_real_array
*     get_integer_var
*     get_real_array
*     get_real_array_optional
*     get_real_var
*     get_real_var_optional
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
*      none

*   Global variables
      include   'const.inc'
      include   'convert.inc'
      include   'sugar.inc'

      real       divide                ! function

*   Internal variables
      integer    layer                 ! layer number
      integer    numvals               ! number of values put into array
      character  mod_name*12           ! module name
      real       dlayer(max_layer)     ! soil layer depths (mm)
      real       NO3(max_layer)        ! soil NO3 content (kg/ha)
      real       NO3_min(max_layer)    ! soil NO3 minimum (kg/ha)

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_get_other_variables')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)


      ! INPUT module
      ! ------------
      call get_integer_var (unknown_module, 'day', '()'
     :                                    , g_day_of_year, numvals
     :                                    , 1, 366)
      call get_integer_var (unknown_module, 'year', '()'
     :                                    , g_year, numvals
     :                                    , c_year_lb, c_year_ub)
      call get_real_var (unknown_module, 'latitude', '(oL)'
     :                                  , g_latitude, numvals
     :                                  , c_latitude_lb, c_latitude_ub)
      call get_real_var (unknown_module, 'maxt', '(oC)'
     :                                  , g_maxt, numvals
     :                                  , c_maxt_lb, c_maxt_ub)
      call get_real_var (unknown_module, 'mint', '(oC)'
     :                                  , g_mint, numvals
     :                                  , c_mint_lb, c_mint_ub)
      call get_real_var (unknown_module, 'radn', '(Mj/m^2)'
     :                                  , g_radn, numvals
     :                                  , c_radn_lb, c_radn_ub)

      ! Canopy Module
      ! -------------
      call get_current_module (mod_name)
      call get_real_var_optional (unknown_module
     :                           , 'fr_intc_radn_'//mod_name
     :                           , '()'
     :                           , g_fr_intc_radn
     :                           , numvals
     :                           , 0.0
     :                           , 1.0)

      ! Soil Water module
      ! -----------------
      call get_real_array (unknown_module, 'dlayer', max_layer
     :                                    , '(mm)'
     :                                    , dlayer, numvals
     :                                    , c_dlayer_lb, c_dlayer_ub)

      if (g_num_layers.eq.0) then
            ! we assume dlayer hasn't been initialised yet.
cnh why do we keep incrementing dlayer??????????
cnh         call add_real_array (dlayer, g_dlayer, numvals)
         do 999 layer = 1,numvals
            g_dlayer(layer) = dlayer(layer)
  999    continue
         g_num_layers = numvals

      else
            ! dlayer may be changed from its last setting
         do 1000 layer = 1, numvals
            p_ll_dep(layer) = divide (p_ll_dep(layer)
     :                              , g_dlayer(layer), 0.0)
     :                      * dlayer(layer)

            g_dlayer(layer) = dlayer(layer)
1000     continue
         g_num_layers = numvals
      endif
      call get_real_array (unknown_module, 'dul_dep', max_layer
     :                                    , '(mm)'
     :                                    , g_dul_dep, numvals
     :                                    , c_dul_dep_lb, c_dul_dep_ub)
      call get_real_array (unknown_module, 'sw_dep', max_layer
     :                                    , '(mm)'
     :                                    , g_sw_dep, numvals
     :                                    , c_sw_dep_lb, c_sw_dep_ub)
      call get_real_array (unknown_module, 'sat_dep', max_layer
     :                                    , '(mm)'
     :                                    , g_sat_dep, numvals
     :                                    , c_sw_dep_lb, c_sw_dep_ub)
      call get_real_array (unknown_module, 'll15_dep', max_layer
     :                                    , '(mm)'
     :                                    , g_ll15_dep, numvals
     :                                    , c_sw_dep_lb, c_sw_dep_ub)
      if (g_uptake_source.eq.'apsim') then
         call get_real_array_optional (unknown_module
     :                             ,'uptake_water_sugar'
     :                             ,max_layer
     :                             ,'(mm)'
     :                             ,g_uptake_water
     :                             ,g_num_uptake_water
     :                             ,0.0
     :                             ,500.0)
         call get_real_array_optional (unknown_module
     :                             ,'uptake_no3_sugar'
     :                             ,max_layer
     :                             ,'(kg/ha)'
     :                             ,g_uptake_no3
     :                             ,g_num_uptake_no3
     :                             ,0.0
     :                             ,500.0)
      else
      endif

      ! soil nitrogen module
      ! --------------------
      call get_real_array_optional (unknown_module, 'no3', max_layer
     :                                  ,  '(kg/ha)'
     :                                  , NO3, numvals
     :                                  , c_NO3_lb, c_NO3_ub)
      if (numvals.eq.0) then
            ! we have no N supply - make non-limiting.
         call fill_real_array (NO3, 10000.0, g_num_layers)
      else
      endif
      do 2000 layer = 1, g_num_layers
         g_NO3gsm(layer) = NO3(layer) * kg2gm /ha2sm
2000  continue
      call get_real_array_optional (unknown_module, 'no3_min',max_layer
     :                                  ,  '(kg/ha)'
     :                                  , NO3_min, numvals
     :                                  , c_NO3_min_lb, c_NO3_min_ub)
      do 3000 layer = 1, g_num_layers
         g_NO3gsm_min(layer) = NO3_min(layer) * kg2gm /ha2sm
3000  continue

      call pop_routine (my_name)
      return
      end
*     ================================================================
      subroutine sugar_set_other_variables ()
*     ================================================================

*   Short description:
*      Set the value of a variable or array in other module/s.

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
*                       implicit none

*   Changes:
*     070495 nih taken from template

*   Calls:
*     count_of_real_vals
*     pop_routine
*     push_routine
*     set_real_array

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*      none

*   Global variables
      include   'const.inc'
      include   'convert.inc'
      include   'sugar.inc'

      integer    count_of_real_vals    ! function

*   Internal variables
      real       dlt_NO3(max_layer)    ! soil NO3 change (kg/ha)
      integer    layer                 ! soil layer no.
      integer    num_layers            ! number of layers

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_set_other_variables')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      if (g_uptake_source.eq.'calc') then
         num_layers = count_of_real_vals (g_dlayer, max_layer)

         do 1000 layer = 1, num_layers
            dlt_NO3(layer) = g_dlt_NO3gsm(layer) * gm2kg /sm2ha
1000     continue

         call new_postbox()
         call post_real_array ('dlt_no3', '(kg/ha)'
     :                    , dlt_NO3, num_layers)
         call message_send_immediate (unknown_module
     :                               ,MES_set_variable
     :                               ,'dlt_no3')
         call delete_postbox()

         call new_postbox()
         call post_real_array ('dlt_sw_dep', '(mm)'
     :                    , g_dlt_sw_dep, num_layers)
         call message_send_immediate (unknown_module
     :                               ,MES_set_variable
     :                               ,'dlt_sw_dep')
         call delete_postbox()

      else
         ! assume that the module that calculated uptake has also
         ! updated these pools.
      endif
      
      call pop_routine (my_name)
      return
      end
*     ===============================================================
      subroutine sugar_set_my_variable (Variable_name)
*     ===============================================================

*   Short description:
*      Set a variable in this module as requested by another.

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
*                       implicit none

*   Changes:
*      060495 nih - taken from template
*      060696 nih - changed respond2set routines to collect routines

*   Calls:
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      character  Variable_name*(*)     ! (INPUT) Variable name to search for

*   Global variables
      include 'const.inc'
      include 'sugar.inc'

*   Internal variables
      integer numvals

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_set_my_variable')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      if (variable_name .eq. 'plants') then

         call collect_real_var (variable_name
     :                         ,'(m-2)'
     :                         ,g_plants
     :                         ,numvals
     :                         ,0.0
     :                         ,100.0)

        if (g_current_stage.gt.emerg) then
           call warning_error (ERR_User,
     :            'You have updated plant number after emergence')
        else
        endif

      else
            ! Don't know this variable name
            call message_unused ()
      endif

      call pop_routine (my_name)
      return
      end
*     ================================================================
      subroutine sugar_send_my_variable (variable_name)
*     ================================================================

*   Short description:
*      Return the value of a variable requested by other modules.

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
*                       implicit none

*   Changes:
*      060495 nih - taken from template

*   Calls:
*     count_of_real_vals
*     divide
*     find_layer_no
*     pop_routine
*     push_routine
*     respond2get_char_var
*     respond2get_integer_var
*     respond2get_real_array
*     respond2get_real_var
*     sugar_swdef
*     sum_between
*     sum_real_array

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      character variable_name*(*)      ! (INPUT) variable name to search for

*   Global variables
      include   'convert.inc'          ! gm2kg, sm2ha, mm2cm, cmm2cc
      include   'sugar.inc'

      integer    count_of_real_vals    ! function
      integer    find_layer_no         ! function
      real       l_bound               ! function
      real       sugar_nfact           ! function
      real       sugar_profile_fasw    ! function
      real       sugar_swdef           ! function
      real       sugar_water_log_fact  ! function
      real       sum_real_array        ! function
      real       sum_between           ! function
cbak
      real       divide                ! function

*   Internal variables
      real       act_N_up              ! cumulative total N uptake by plant
                                       ! (kg/ha)
      real       biomass               ! above ground biomass (alive+dead)
      real       biomass_n             ! N in above ground biomass (alive+dead)
      real       cane_dmf              !
      real       cane_wt               ! cane weight (sstem + sucrose)
      real       ccs                   ! commercial cane sugar(g/g)
      real       cover                 ! crop cover fraction (0-1)
      real       radn_int              ! daily radn intercepted (MJ)
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      real       fasw
      real       lai_sum               ! leaf area index of all leaf material
                                       ! live + dead
      real       lai_dead              ! dead leaf area index
                                       ! (m^2 leaf/m^2 soil)
      integer    num_layers            ! number of layers in profile
      integer    stage_no              ! current stage no.
      real       NO3gsm_tot            ! total NO3 in the root profile (g/m^2)
      real       N_demand              ! sum N demand for plant parts (g/m^2)
      real       N_supply              ! N supply for grain (g/m^2)
cbak
      real       conc_n_leaf           ! Current N concentration in leaves
      real       conc_n_cab            ! Current N concentration in cabbage
      real       conc_n_cane           ! Current N concentration in cane


      real       n_leaf_crit           ! Weight of N in leaves at the critical c
      real       n_leaf_min            ! Weight of N in leaves at the min concen
      real       green_biomass_n       ! Weight of N in green tops (g/m^2)
cmjr
      real       canefw                ! Weight of fresh cane at 30% dry matter
      real       water_log_fact        ! 0-1 stress factor for water logging
      real       scmstf                ! sucrose conc in fresh millable stalk
      real       scmst                 ! sucrose conc in dry millable stalk
*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_send_my_variable')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      if (variable_name .eq. 'crop_status') then
         call respond2get_char_var (variable_name
     :                             , '()'
     :                             , g_crop_status)

      elseif (variable_name .eq. 'stage') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g_current_stage)

      elseif (variable_name .eq. 'stage_code') then
         stage_no = int (g_current_stage)
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , c_stage_code_list(stage_no))

      elseif (variable_name .eq. 'stage_name') then
         stage_no = int (g_current_stage)
         call respond2get_char_var (variable_name
     :                             , '()'
     :                             , c_stage_names(stage_no))

      elseif (variable_name .eq. 'crop_type') then
         if (c_crop_type.ne.' ') then
            call respond2get_char_var (variable_name
     :                             , '()'
     :                             , c_crop_type)
         else
             call message_unused ()
         endif

      elseif (variable_name .eq. 'ratoon_no') then
         call respond2get_integer_var (variable_name
     :                             , '()'
     :                             , g_ratoon_no)

      elseif (variable_name .eq. 'phase_tt') then
         call respond2get_real_array (variable_name
     :                             , '(oC)'
     :                             , g_phase_tt
     :                             , max_stage)

      elseif (variable_name .eq. 'tt_tot') then
         call respond2get_real_array (variable_name
     :                             , '(oC)'
     :                             , g_tt_tot
     :                             , max_stage)

c      elseif (variable_name .eq. 'days_tot') then
c I removed this NIH

      elseif (variable_name .eq. 'leaf_no') then
         call respond2get_real_array (variable_name
     :                              , '()'
     :                              , g_leaf_no
     :                              , max_stage)

      elseif (variable_name .eq. 'leaf_no_dead') then
         call respond2get_real_array (variable_name
     :                              , '()'
     :                              , g_leaf_no_dead
     :                              , max_stage)

      elseif (variable_name .eq. 'leaf_area') then
         call respond2get_real_array (variable_name
     :                              , '()'
     :                              , g_leaf_area
     :                              , max_leaf)

      elseif (variable_name .eq. 'leaf_dm') then
         call respond2get_real_array (variable_name
     :                              , '()'
     :                              , g_leaf_dm
     :                              , max_leaf)

      elseif (variable_name .eq. 'height') then
         call respond2get_real_var (variable_name
     :                             , '(mm)'
     :                             , g_canopy_height)

      elseif (variable_name .eq. 'root_depth') then
         call respond2get_real_var (variable_name
     :                             , '(mm)'
     :                             , g_root_depth)

      elseif (variable_name .eq. 'cover_green') then
         cover = 1.0 - exp (-c_extinction_coef * g_lai)

         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , cover)

      elseif (variable_name .eq. 'radn_int') then
         cover = 1.0 - exp (-c_extinction_coef * g_lai)
         radn_int = cover * g_radn
         call respond2get_real_var (variable_name
     :                             , '(mj/m2)'
     :                             , radn_int)

      elseif (variable_name .eq. 'cover_tot') then
         lai_dead = g_slai + g_tlai_dead
         cover = 1.0
     :         - exp (-c_extinction_coef * g_lai
     :                -c_extinction_coef_dead * lai_dead)

         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , cover)

      elseif (variable_name .eq. 'lai_sum') then
         lai_sum = g_lai + g_slai + g_tlai_dead
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , lai_sum)

      elseif (variable_name .eq. 'tlai') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g_lai + g_slai)

      elseif (variable_name .eq. 'slai') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g_slai)

      elseif (variable_name .eq. 'lai') then
         call respond2get_real_var (variable_name
     :                             , '(m^2/m^2)'
     :                             , g_lai)

            ! plant biomass

      elseif (variable_name .eq. 'root_wt') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g_dm_green(root))

      elseif (variable_name .eq. 'leaf_wt') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g_dm_green(leaf))

      elseif (variable_name .eq. 'sstem_wt') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g_dm_green(sstem))

      elseif (variable_name .eq. 'canefw') then
         canefw = (g_dm_green(sstem) + g_dm_green(sucrose)
     :          + g_plant_wc(sstem))*g2t/sm2ha
         call respond2get_real_var (variable_name
     :                             , '(t/ha)'
     :                             , canefw)

      elseif (variable_name .eq. 'ccs') then
         canefw = (g_dm_green(sstem) + g_dm_green(sucrose)
     :          + g_plant_wc(sstem))
         scmstf = divide(g_dm_green(sucrose),canefw,0.0)
         ccs = 1.23*scmstf - 0.029
         ccs = l_bound(ccs,0.0)
         ccs = ccs * 100.          ! convert to %
         call respond2get_real_var (variable_name
     :                             , '(%)'
     :                             , ccs)

      elseif (variable_name .eq. 'scmstf') then
         canefw = (g_dm_green(sstem) + g_dm_green(sucrose)
     :          + g_plant_wc(sstem))
         scmstf = divide(g_dm_green(sucrose),canefw,0.0)
         call respond2get_real_var (variable_name
     :                             , '(g/g)'
     :                             , scmstf)

      elseif (variable_name .eq. 'scmst') then
         cane_wt = g_dm_green(sstem) + g_dm_green(sucrose)
         scmst = divide(g_dm_green(sucrose),cane_wt,0.0)
         call respond2get_real_var (variable_name
     :                             , '(g/g)'
     :                             , scmst)

      elseif (variable_name .eq. 'sucrose_wt') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g_dm_green(sucrose))

      elseif (variable_name .eq. 'cabbage_wt') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g_dm_green(cabbage))

      elseif (variable_name .eq. 'cane_wt') then
         cane_wt = g_dm_green(sstem)+g_dm_green(sucrose)

         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , cane_wt)

      elseif (variable_name .eq. 'biomass') then

         biomass =
     :        sum_Real_array(g_dm_green,max_part)-g_dm_green(root)
     :      + sum_real_array(g_dm_senesced,max_part)-g_dm_senesced(root)

         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , biomass)

      elseif (variable_name .eq. 'green_biomass') then

         biomass =
     :        sum_Real_array(g_dm_green,max_part)-g_dm_green(root)

         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , biomass)

      elseif (variable_name .eq. 'dm_green') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g_dm_green
     :                             , max_part)

      elseif (variable_name .eq. 'dm_senesced') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g_dm_senesced
     :                             , max_part)

      elseif (variable_name .eq. 'dm_dead') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g_dm_dead
     :                             , max_part)

      elseif (variable_name .eq. 'dlt_dm') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g_dlt_dm)

      elseif (variable_name .eq. 'partition_xs') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g_partition_xs)

      elseif (variable_name .eq. 'dlt_dm_green') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g_dlt_dm_green
     :                             , max_part)

      elseif (variable_name .eq. 'dlt_dm_detached') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g_dlt_dm_detached
     :                             , max_part)

cbak Reporting of N concentrations

      elseif (variable_name .eq. 'n_critical') then
         call respond2get_real_array (variable_name
     :                             , '(g/g)'
     :                             , g_N_conc_crit
     :                             , max_part)

      elseif (variable_name .eq. 'n_minimum') then
         call respond2get_real_array (variable_name
     :                             , '(g/g)'
     :                             , g_N_conc_min
     :                             , max_part)

cbak
      elseif (variable_name .eq. 'n_conc_leaf') then
        Conc_N_leaf = divide (g_N_green(leaf) ,g_dm_green(leaf), 0.0)
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , Conc_N_leaf)

      elseif (variable_name .eq. 'n_conc_cab') then
      Conc_N_cab = divide (g_N_green(cabbage), g_dm_green(cabbage), 0.0)
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , Conc_N_cab)

      elseif (variable_name .eq. 'n_conc_cane') then
        Conc_N_cane = divide (g_N_green(sstem)+g_N_green(sucrose),
     :                 g_dm_green(sstem)+g_dm_green(sucrose), 0.0)
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , Conc_N_cane)

cbak Weights of N in plant

      elseif (variable_name .eq. 'n_leaf_crit') then
       N_leaf_crit = g_N_conc_crit(leaf) * g_dm_green(leaf)

         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , N_leaf_crit)

      elseif (variable_name .eq. 'n_leaf_min') then
       N_leaf_min = g_N_conc_min(leaf) * g_dm_green(leaf)

         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , N_leaf_min)


      elseif (variable_name .eq. 'biomass_n') then
         biomass_n =
     :        sum_Real_array(g_n_green,max_part)-g_n_green(root)
     :      + sum_real_array(g_n_senesced,max_part)-g_n_senesced(root)
cbak
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , biomass_n)

      elseif (variable_name .eq. 'green_biomass_n') then
         green_biomass_n =
     :        sum_Real_array(g_n_green,max_part)-g_n_green(root)
cbak
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , green_biomass_n)


      elseif (variable_name .eq. 'n_green') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g_N_green
     :                             , max_part)

      elseif (variable_name .eq. 'n_senesced') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g_N_senesced
     :                             , max_part)

cbak   Delta N in plant tops

      elseif (variable_name .eq. 'dlt_n_green') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g_dlt_N_green
     :                             , max_part)

      elseif (variable_name .eq. 'swdef_pheno') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , sugar_swdef (pheno))

      elseif (variable_name .eq. 'swdef_photo') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , sugar_swdef (photo))

      elseif (variable_name .eq. 'swdef_expan') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , sugar_swdef (expansion))

      elseif (variable_name .eq. 'nfact_photo') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , sugar_nfact (photo))

cbak
      elseif (variable_name .eq. 'nfact_expan') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , sugar_nfact (expansion))

      elseif (variable_name .eq. 'ep') then
         num_layers = count_of_real_vals (g_dlayer, max_layer)
         call respond2get_real_array (variable_name
     :                               , '(mm)'
     :                               , g_dlt_sw_dep
     :                               , num_layers)

      elseif (variable_name .eq. 'cep') then
         call respond2get_real_var (variable_name
     :                             , '(mm)'
     :                             , - g_transpiration_tot)

      elseif (variable_name .eq. 'sw_demand') then
         call respond2get_real_var (variable_name
     :                             , '(mm)'
     :                             , g_sw_demand)

      elseif (variable_name .eq. 'fasw') then
         fasw = sugar_profile_fasw ()
         call respond2get_real_var (variable_name
     :                             , '(0-1)'
     :                             , fasw)

      elseif (variable_name .eq. 'cane_dmf') then
         cane_dmf = divide (
     :                      g_dm_green(sstem)+g_dm_green(sucrose)
     :                     ,g_dm_green(sstem)+g_dm_green(sucrose)
     :                                       +g_plant_wc(sstem)
     :                     ,0.0)
         call respond2get_real_var (variable_name
     :                             , '(0-1)'
     :                             , cane_dmf)

      elseif (variable_name .eq. 'tt_for_dmf') then
         call respond2get_real_var (variable_name
     :                             , '(tt)'
     :                             , g_tt_for_dmf)

      elseif (variable_name .eq. 'water_log_fact') then
         water_log_fact = sugar_water_log_fact ()
         call respond2get_real_var (variable_name
     :                             , '(0-1)'
     :                             , water_log_fact)

      elseif (variable_name .eq. 'das') then
         call respond2get_real_var (variable_name
     :                             , '(days)'
     :                             , sum_between (sowing, now
     :                                          , g_days_tot))

            ! plant nitrogen

      elseif (variable_name .eq. 'n_uptake') then
         act_N_up = g_N_uptake_tot*gm2kg /sm2ha
         call respond2get_real_var (variable_name
     :                             , '(kg/ha)'
     :                             , act_N_up)

      elseif (variable_name .eq. 'no3_tot') then
         deepest_layer = find_layer_no (g_root_depth, g_dlayer
     :                                , max_layer)
         NO3gsm_tot = sum_real_array (g_NO3gsm, deepest_layer)
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , NO3gsm_tot)

      elseif (variable_name .eq. 'n_demand') then
         N_demand = sum_real_array (g_N_demand, max_part)
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , N_demand)
      elseif (variable_name .eq. 'no3_demand') then
         N_demand = sum_real_array (g_N_demand, max_part)*10.

         call respond2get_real_var (variable_name
     :                             , '(kg/ha)'
     :                             , N_demand)

      elseif (variable_name .eq. 'n_supply') then
         N_supply = sum_real_array ( g_dlt_N_green, max_part)
     :            - g_dlt_N_green(root)
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , N_supply)


      elseif (variable_name .eq. 'rlv') then
         num_layers = count_of_real_vals (g_dlayer, max_layer)
         call respond2get_real_array (variable_name
     :                               , '(mm/mm3)'
     :                               , g_rlv
     :                               , num_layers)

      else
         ! not my variable
         call message_unused ()
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      real function sugar_stage_code (stage_no, stage_table, numvals)
*     ===========================================================

*   Short description:
*       Return an interpolated stage code from a table of stage_codes
*       and a nominated stage number. Returns 0 if the stage number is not
*       found. Interpolation is done on thermal time.

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
*       070495 nih taken from template

*   Calls:
*     divide
*     pop_routine
*     push_routine
*     stage_is_between
*     stage_no_of
*     sum_between
*     warning_error

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       stage_no              ! (INPUT) stage number to convert
      real       stage_table(*)        ! (INPUT) table of stage codes
      integer    numvals               ! (INPUT) size_of of table

*   Global variables
      include   'const.inc'            ! err_user
      include   'sugar.inc'

      real       divide                ! function
      logical    stage_is_between      ! function
      integer    stage_no_of           ! function
      real       sum_between           ! function

*   Internal variables
      real       phase_tt              ! required thermal time between stages
                                       ! (oC)
      character  error_message*100     ! error message
      real       fraction_of           !
      integer    i                     ! array index - counter
      integer    next_stage            ! next stage number to use
      real       tt_tot                ! elapsed thermal time between stages
                                       ! (oC)
      integer    this_stage            ! this stage to use
      real       x_stage_code          ! interpolated stage code

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_stage_code')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      if (numvals.ge.2) then
            ! we have a valid table
         this_stage = stage_no_of (stage_table(1)
     :                           , c_stage_code_list, max_stage)

         do 1000 i = 2, numvals
            next_stage = stage_no_of (stage_table(i)
     :                              , c_stage_code_list, max_stage)

            if (stage_is_between (this_stage, next_stage, stage_no))
     :         then
                  ! we have found its place
               tt_tot = sum_between (this_stage, next_stage, g_tt_tot)
               phase_tt = sum_between (this_stage, next_stage
     :                               , g_phase_tt)
               fraction_of = divide (tt_tot, phase_tt, 0.0)
               x_stage_code = stage_table(i-1)
     :                      + (stage_table(i) - stage_table(i-1))
     :                      * fraction_of
               goto 2000

            else
               x_stage_code = 0.0
               this_stage = next_stage

            endif
1000     continue
2000     continue
      else
            ! we have no valid table

         x_stage_code = 0.0

         write (error_message,'(a, i10)')
     :               'Invalid lookup table - number of values ='
     :              , numvals
         call warning_error (err_user, error_message)

      endif
      sugar_stage_code = x_stage_code

      call pop_routine (my_name)

      return
      end
*     ===========================================================
      logical function sugar_my_type ()
*     ===========================================================

*   Short description:
*       Returns true if 'type' is equal to the crop type or is absent.

*   Assumptions:
*       If type is not specified, it is assumed the message was addressed
*        directly to the module.

*   Notes:
*       none

*   Procedure attributes:
*      Version:         any hardware/fortran77
*      Extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       implicit none

*   Changes:
*      060495 nih taken from template
*      060696 nih changed extract routines to collect routine calls
*                 removed datastring from argument list

*   Calls:
*     collect_char_var_optional
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*    none

*   Global variables
      include   'sugar.inc'

*   Internal variables
      character  crop_type*50          ! crop type in data string
      integer    numvals               ! number of values returned

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_my_type')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      call collect_char_var_optional ('type', '()'
     :                              , crop_type, numvals)

      if (crop_type.eq.c_crop_type .or. numvals.eq.0) then
         sugar_my_type = .true.
      else
         sugar_my_type = .false.
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_read_constants ()
*     ===========================================================

*   Short description:
*       Crop initialisation - reads constants from constants file

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
*                       implicit none

*   Changes:
*     060495 nih taken from template

*   Calls:
*     pop_routine
*     push_routine
*     read_integer_var
*     read_real_var
*     read_real_array
*     sugar_version
*     write_string

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'const.inc'
      include   'sugar.inc'

*   Internal variables
      integer    numvals               ! number of values returned

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_read_constants')

      character  section_name*(*)
      parameter (section_name = 'constants')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      call write_string (lu_scr_sum
     :                  ,new_line//'    - Reading constants')

      call read_real_var (section_name
     :                    , 'tt_emerg_to_begcane_ub', '()'
     :                    , c_tt_emerg_to_begcane_ub, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'tt_begcane_to_flowering_ub', '()'
     :                    , c_tt_begcane_to_flowering_ub, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'tt_flowering_to_crop_end_ub', '()'
     :                    , c_tt_flowering_to_crop_end_ub, numvals
     :                    , 0.0, 10000.0)

         !    sugar_N_uptake

      call read_real_var (section_name
     :                    , 'no3_diffn_const', '(days)'
     :                    , c_NO3_diffn_const, numvals
     :                    , 0.0, 100.0)

         !    sugar_get_root_params

      call read_real_var (section_name
     :                    , 'll_ub', '()'
     :                    , c_ll_ub, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'kl_ub', '()'
     :                    , c_kl_ub, numvals
     :                    , 0.0, 1000.0)

         !    sugar_watck

      call read_real_var (section_name
     :                    , 'minsw', '()'
     :                    , c_minsw, numvals
     :                    , 0.0, 1000.0)


         !    sugar_get_other_variables

         ! checking the bounds of the bounds..
      call read_integer_var (section_name
     :                    , 'year_ub', '()'
     :                    , c_year_ub, numvals
     :                    , 1800, 2000)

      call read_integer_var (section_name
     :                    , 'year_lb', '()'
     :                    , c_year_lb, numvals
     :                    , 1800, 2000)

      call read_real_var (section_name
     :                    , 'latitude_ub', '(oL)'
     :                    , c_latitude_ub, numvals
     :                    , -90.0, 90.0)

      call read_real_var (section_name
     :                    , 'latitude_lb', '(oL)'
     :                    , c_latitude_lb, numvals
     :                    , -90.0, 90.0)

      call read_real_var (section_name
     :                    , 'maxt_ub', '(oC)'
     :                    , c_maxt_ub, numvals
     :                    , 0.0, 60.0)

      call read_real_var (section_name
     :                    , 'maxt_lb', '(oC)'
     :                    , c_maxt_lb, numvals
     :                    , 0.0, 60.0)

      call read_real_var (section_name
     :                    , 'mint_ub', '(oC)'
     :                    , c_mint_ub, numvals
     :                    , 0.0, 40.0)

      call read_real_var (section_name
     :                    , 'mint_lb', '(oC)'
     :                    , c_mint_lb, numvals
     :                    , -100.0, 100.0)

      call read_real_var (section_name
     :                    , 'radn_ub', '(MJ/m^2)'
     :                    , c_radn_ub, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'radn_lb', '(MJ/m^2)'
     :                    , c_radn_lb, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'dlayer_ub', '(mm)'
     :                    , c_dlayer_ub, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'dlayer_lb', '(mm)'
     :                    , c_dlayer_lb, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'dul_dep_ub', '(mm)'
     :                    , c_dul_dep_ub, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'dul_dep_lb', '(mm)'
     :                    , c_dul_dep_lb, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'sw_dep_ub', '(mm)'
     :                    , c_sw_dep_ub, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'sw_dep_lb', '(mm)'
     :                    , c_sw_dep_lb, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'no3_ub', '(kg/ha)'
     :                    , c_NO3_ub, numvals
     :                    , 0.0, 100000.0)

      call read_real_var (section_name
     :                    , 'no3_lb', '(kg/ha)'
     :                    , c_NO3_lb, numvals
     :                    , 0.0, 100000.0)

      call read_real_var (section_name
     :                    , 'no3_min_ub', '(kg/ha)'
     :                    , c_NO3_min_ub, numvals
     :                    , 0.0, 100000.0)

      call read_real_var (section_name
     :                    , 'no3_min_lb', '(kg/ha)'
     :                    , c_NO3_min_lb, numvals
     :                    , 0.0, 100000.0)


      call pop_routine (my_name)
      return
      end
*     ===========================================================
      real function sugar_sprouting (current_stage)
*     ===========================================================

*   Short description:
*      Determine sprouting based on soil water availability

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
*                       implicit none

*   Changes:
*     060495 nih taken from template (was sugar_germination)

*   Calls:
*     divide
*     find_layer_no
*     on_day_of
*     pop_routine
*     push_routine
*     stage_is_between

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       current_stage         ! (OUTPUT) phenological stage number

*   Global variables
      include   'sugar.inc'

      integer    find_layer_no         ! function
      real       divide                ! function
      logical    on_day_of             ! function
      logical    stage_is_between      ! function

*   Internal variables
      integer    layer_no_seed         ! seedling layer number
      real       pesw_seed             ! plant extractable soil water in
                                       ! seedling layer available for
                                       ! germination ( mm/mm)

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_sprouting')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

         ! determine if soil water content is sufficient to allow sprouting.
         ! Soil water content of the seeded layer must be > the
         ! lower limit to be adequate for germination.

      if (stage_is_between (sowing, sprouting, current_stage)) then

         layer_no_seed = find_layer_no (g_sowing_depth, g_dlayer
     :                                , max_layer)
         pesw_seed = divide (g_sw_dep(layer_no_seed)
     :                     - p_ll_dep(layer_no_seed)
     :                     , g_dlayer(layer_no_seed), 0.0)

            ! can't sprout on same day as sowing, because miss out on
            ! day of sowing else_where

         if (pesw_seed.gt.c_pesw_germ
     :   .and.
     :   .not. on_day_of (sowing, g_current_stage, g_days_tot)) then
               ! we have sprouting
               ! set the current stage so it is on the point of sprouting
            sugar_sprouting = 1.0 + mod (g_current_stage, 1.0)

         else
                ! no germination yet but indicate that we are on the way.
            sugar_sprouting = 0.0001
         endif
      else
             ! no sowing yet
         sugar_sprouting = 0.0
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_crop_totals ()
*     ===========================================================

*   Short description:
*         Collect totals of crop variables for output

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
*                       implicit none

*   Changes:
*     070495 nih taken from template

*   Calls:
*     divide
*     find_layer_no
*     on_day_of
*     pop_routine
*     push_routine
*     sum_real_array

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'sugar.inc'

      real       divide                ! function
      integer    find_layer_no         ! function
      logical    on_day_of             ! function
      real       sum_real_array        ! function

*   Internal variables
      real       N_conc_stover         ! tops actual N concentration
                                       ! (g N/g part)
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      real       N_green_demand        ! plant N demand (g/m^2)

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_crop_totals')

*   Initial data values
*     none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)


cnh I have removed most of the variables because they were either calculated
cnh wrongly or irrelevant.

             ! get totals
      N_conc_stover = divide ((g_N_green(leaf)
     :                       + g_N_green(sstem)
     :                       + g_N_green(cabbage)
     :                       + g_N_green(sucrose))
     :                      , (g_dm_green(leaf)
     :                       + g_dm_green(sstem)
     :                       + g_dm_green(cabbage)
     :                       + g_dm_green(sucrose))
     :                       , 0.0)

          ! note - g_N_conc_crit should be done before the stages change
cnh wrong!!!
c      N_conc_stover_crit = (g_N_conc_crit(leaf) + g_N_conc_crit(stem))
c     :                   * 0.5
      N_green_demand = sum_real_array (g_N_demand, max_part)

      deepest_layer = find_layer_no (g_root_depth, g_dlayer, max_layer)

      if (on_day_of (sowing, g_current_stage, g_days_tot)) then
         g_transpiration_tot =
     :           - sum_real_array (g_dlt_sw_dep, deepest_layer)
         g_N_conc_act_stover_tot = N_conc_stover
         g_N_demand_tot = N_green_demand

      else
         g_transpiration_tot = g_transpiration_tot
     :                       + (-sum_real_array (g_dlt_sw_dep
     :                                         , deepest_layer))
         g_N_conc_act_stover_tot = N_conc_stover
         g_N_demand_tot = g_N_demand_tot + N_green_demand
      endif

      g_lai_max = max (g_lai_max, g_lai)
      if (on_day_of (flowering, g_current_stage, g_days_tot)) then
         g_isdate = g_day_of_year
      else
      endif



      call pop_routine (my_name)
      return
      end
*     ===========================================================
      real function sugar_sla_min (leaf_no)
*     ===========================================================

*   Short description:
*       Return the minimum specific leaf area (mm^2/g)
*       of a specified leaf no.

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
*                       implicit none

*   Changes:
*       05/05/95 nih specified and programmed

*   Calls:
*     divide
*     min
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       leaf_no               ! (INPUT) nominated leaf number

*   Global variables
      include   'sugar.inc'

      real       linear_interp_real    ! function

*   Internal variables
*     none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_sla_min')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      sugar_sla_min = linear_interp_real
     :                     (real(leaf_no)
     :                     ,c_sla_lfno
     :                     ,c_sla_min
     :                     ,c_num_sla_lfno
     :                     )

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      real function sugar_sla_max (leaf_no)
*     ===========================================================

*   Short description:
*       Return the maximum specific leaf area (mm^2/g)
*       of a specified leaf no.

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
*                       implicit none

*   Changes:
*       05/05/95 nih specified and programmed

*   Calls:
*     divide
*     min
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       leaf_no               ! (INPUT) nominated leaf number

*   Global variables
      include   'sugar.inc'

      real       linear_interp_real    ! function

*   Internal variables
*     none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_sla_min')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      sugar_sla_max = linear_interp_real
     :                     (real(leaf_no)
     :                     ,c_sla_lfno
     :                     ,c_sla_max
     :                     ,c_num_sla_lfno
     :                     )

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_zero_globals ()
*     ===========================================================

*   Short description:
*       Zero global variables and arrays

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
*                       implicit none

*   Changes:
*     150595 nih created from sugar_zero_variables

*   Calls:
*     fill_real_array
*     pop_routine
*     push_routine
*     sugar_zero_daily_variables

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'sugar.inc'

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_zero_globals')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

          ! zero pools etc.

      call fill_real_array (g_cnd_photo, 0.0, max_stage)
      call fill_real_array (g_cswd_expansion, 0.0, max_stage)
      call fill_real_array (g_cswd_pheno, 0.0, max_stage)
      call fill_real_array (g_cswd_photo, 0.0, max_stage)
      call fill_real_array (g_days_tot, 0.0, max_stage)
      call fill_real_array (g_dm_dead, 0.0, max_part)
      call fill_real_array (g_dm_green, 0.0, max_part)
      call fill_real_array (g_dm_plant_min, 0.0, max_part)
      call fill_real_array (g_plant_wc, 0.0, max_part)
      call fill_real_array (g_dm_plant_top_tot, 0.0, max_stage)
      call fill_real_array (g_leaf_area, 0.0, max_leaf)
      call fill_real_array (g_leaf_area, 0.0, max_leaf)
      call fill_real_array (g_leaf_dm, 0.0, max_leaf)
      call fill_real_array (g_leaf_no, 0.0, max_stage)
      call fill_real_array (g_leaf_no_dead, 0.0, max_stage)
      call fill_real_array (g_N_conc_crit, 0.0, max_part)
      call fill_real_array (g_N_conc_min, 0.0, max_part)
      call fill_real_array (g_N_green, 0.0, max_part)
      call fill_real_array (g_phase_tt, 0.0, max_stage)
      call fill_real_array (g_tt_tot, 0.0, max_stage)
      call fill_real_array (g_dm_senesced, 0.0, max_part)
      call fill_real_array (g_N_dead, 0.0, max_part)
      call fill_real_array (g_N_senesced, 0.0, max_part)
      call fill_real_array (g_rlv, 0.0, max_layer)

      g_num_layers = 0
      g_canopy_height = 0.0
      g_isdate = 0
      g_mdate = 0
      g_leaf_no_final = 0.0
      g_lai_max = 0.0
      g_N_conc_act_stover_tot = 0.0
      g_N_conc_crit_stover_tot = 0.0
      g_N_demand_tot = 0.0
      g_N_uptake_stover_tot = 0.0
      g_N_uptake_tot = 0.0
      g_plants = 0.0
      g_root_depth = 0.0
      g_sowing_depth = 0.0
      g_slai = 0.0
      g_lai = 0.0
      g_transpiration_tot = 0.0
      g_previous_stage = 0.0
      g_ratoon_no = 0

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_zero_parameters ()
*     ===========================================================

*   Short description:
*       Zero parameter variables and arrays

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
*                       implicit none

*   Changes:
*     150595 nih created from sugar_zero_variables

*   Calls:
*     fill_real_array
*     pop_routine
*     push_routine
*     sugar_zero_daily_variables

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'sugar.inc'

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_zero_parameters')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

          ! zero pools etc.

      call fill_real_array (p_ll_dep, 0.0, max_layer)
      g_uptake_source = ' '
      c_crop_type = ' '

      call pop_routine (my_name)
      return
      end
* ====================================================================
       subroutine sugar_root_length_growth ()
* ====================================================================

*   Short description:

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
*   neilh - 13-06-1995 - Programmed and Specified

*   Calls:
*   Pop_routine
*   Push_routine

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
*     none

*   Global variables
      include 'sugar.inc'
      include 'convert.inc'

      real    divide                     ! function
      integer find_layer_no              ! function
      real    sugar_sw_avail_fac         ! function
      real    sum_real_array             ! function

*   Internal variables
      integer deepest_layer
      real    dlt_length_tot
      real    dlt_length_layer
      integer layer
      real    layer_volume
      real    rlv_factor (max_layer)
      real    rlv_factor_tot

*   Constant values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'sugar_root_length_growth')

*   Initial data values
*     none

* --------------------- Executable code section ----------------------
      call push_routine (myname)

      call fill_real_array (g_dlt_rlv, 0.0, max_layer)

      deepest_layer = find_layer_no (g_root_depth+g_dlt_root_depth
     :                                , g_dlayer
     :                                , max_layer)

      do 100 layer = 1, deepest_layer
         rlv_factor (layer) =
     :                 sugar_sw_avail_fac (layer)  ! moisture factor
     :               * p_xf (layer)                ! growth factor
     :               * divide(g_dlayer(layer)      ! space weighting
     :                       ,g_root_depth         !       factor
     :                       ,0.0)
  100 continue

      rlv_factor_tot = sum_real_array (rlv_factor, deepest_layer)

      dlt_length_tot = g_dlt_dm_green(root) * c_specific_root_length

      do 200 layer = 1, deepest_layer

         dlt_length_layer = dlt_length_tot
     :               * divide (rlv_factor(layer), rlv_factor_tot, 0.0)

                      !   1 sqm
                      !  /
         layer_volume = 1.0 * sm2smm * g_dlayer(layer)

         g_dlt_rlv (layer) = divide (dlt_length_layer
     :                            ,layer_volume
     :                            ,0.0)

  200 continue

      call pop_routine (myname)
      return
      end
* ====================================================================
       subroutine sugar_root_length_senescence (dlt_rlv_senesced)
* ====================================================================

*   Short description:

*   Assumptions:
*      None

*   Notes:
*   nih - I know there is a simpler way of doing this but if we make the
*         calculation of senescence rate more complex this aproach will
*         automatically handle it.

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*   neilh - 14-06-1995 - Programmed and Specified

*   Calls:
*   Pop_routine
*   Push_routine

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      real dlt_rlv_senesced (*)

*   Global variables
      include 'sugar.inc'
      include 'convert.inc'
      
      real    divide
      integer find_layer_no

*   Internal variables
      integer deepest_layer          ! deepest layer with roots ()
      real dlt_length(max_layer)     ! change in root length for each
                                     ! layer (mm/m2)
      integer layer
      real senesced_length           ! length of root to senesce (mm/m2)

*   Constant values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'sugar_root_length_senescence')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------
      call push_routine (myname)

      call fill_real_array (dlt_rlv_senesced, 0.0, max_layer)

      senesced_length = g_dlt_dm_senesced(root) * c_specific_root_length

      call sugar_root_distrib (dlt_length, senesced_length)

      if (g_root_depth .gt. 0.0) then
         deepest_layer = find_layer_no (g_root_depth, g_dlayer
     :                                  , max_layer)
      else
         deepest_layer = 0
      endif

      do 100 layer = 1, deepest_layer
         dlt_rlv_senesced(layer) = divide (dlt_length(layer)
     :                                    ,g_dlayer(layer)
     :                                    ,0.0)
     :                                    * smm2sm
  100 continue

      call pop_routine (myname)
      return
      end
* ====================================================================
       real function sugar_profile_fasw ()
* ====================================================================

*   Short description:

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
*   neilh - 30-06-1995 - Programmed and Specified

*   Calls:
*   Pop_routine
*   Push_routine

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
*      none

*   Global variables
      include 'sugar.inc'
      real    divide                     ! function
      integer find_layer_no              ! function
      real    u_bound                    ! function

*   Internal variables
      real    asw
      real    asw_pot
      integer deepest_layer
      integer layer
      real sw_avail_pot (max_layer)
      real sw_avail     (max_layer)

*   Constant values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'sugar_profile_fasw')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------
      call push_routine (myname)

      call sugar_sw_avail_pot (sw_avail_pot)
      call sugar_sw_avail (sw_avail)

      deepest_layer = find_layer_no (g_root_depth, g_dlayer, max_layer)
      asw_pot = 0.0
      asw     = 0.0
      do 100 layer = 1, deepest_layer
         asw_pot = asw_pot + sw_avail_pot (layer)
         asw = asw + u_bound (sw_avail(layer), sw_avail_pot(layer))
  100 continue

      sugar_profile_fasw = divide (asw, asw_pot, 0.0)

      call pop_routine (myname)
      return
      end
* ====================================================================
       subroutine sugar_prepare ()
* ====================================================================

*   Short description:

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
*   neilh - 05-07-1995 - Programmed and Specified

*   Calls:
*   Pop_routine
*   Push_routine

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
*      none

*   Global variables
      include 'sugar.inc'

*   Internal variables
*      none

*   Constant values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'sugar_prepare')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------
      call push_routine (myname)

      call sugar_zero_daily_variables ()
      call sugar_get_other_variables ()
cnh
      call sugar_phenology ()
      if (g_crop_status.eq.crop_alive) then
         call sugar_leaf_area_potential ()
         call sugar_sw_demand (g_sw_demand)
         call sugar_N_demand (g_N_demand)
      else
      endif

cnh

      call pop_routine (myname)
      return
      end
* ====================================================================
       real function sugar_water_log_fact ()
* ====================================================================

*   Short description:
*   Calculate 0-1 factor for water logging effect on growth

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
*   neilh - 11-10-1995 - Programmed and Specified

*   Calls:
*   Pop_routine
*   Push_routine

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
*      none

*   Global variables
       include 'sugar.inc'
       real    bound                     ! function
       integer count_of_real_vals        ! function
       real    divide                    ! function
       real    linear_interp_real        ! function

*   Internal variables
       integer layer
       integer num_root_layers
       real    wet_root_fr
       real    wfps
       real    root_fr(max_layer)
       real    tot_root_fr

*   Constant values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'sugar_water_log_fact')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------
      call push_routine (myname)

      tot_root_fr = 1.0
      call sugar_root_distrib (root_fr, tot_root_fr)

      num_root_layers = count_of_real_vals (root_fr, max_layer)

      wet_root_fr = 0.0

      do 100 layer = 1, num_root_layers
         wfps = divide (g_sw_dep(layer)- g_ll15_dep(layer)
     :                 ,g_sat_dep(layer) - g_ll15_dep(layer)
     :                 ,0.0)
         wfps = bound (wfps, 0.0, 1.0)

         wet_root_fr = wet_root_fr + wfps * root_fr(layer)
  100 continue

      sugar_water_log_fact = linear_interp_real
     :                       (wet_root_fr
     :                       ,c_water_log_rtfr
     :                       ,c_water_log_fact
     :                       ,c_num_water_log_fact)


      call pop_routine (myname)
      return
      end
* ====================================================================
       subroutine sugar_water_content ()
* ====================================================================

*   Short description:

*   Assumptions:
*      None

*   Notes:
*   NIH - Eventually this routine will need to be broken down into
*         subroutines.

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*   neilh - 11-10-1995 - Programmed and Specified

*   Calls:
*   Pop_routine
*   Push_routine

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
*      none

*   Global variables
       include 'sugar.inc'
       real      divide                  ! function
       real      linear_interp_real      ! function
       logical   on_day_of               ! function
       logical   stage_is_between        ! function

*   Internal variables
       real pot_cane_dmf                 ! potential dm fraction in
                                         ! cane(sstem+sucrose)
       real pot_cane_wc                  ! potential water content in
                                         ! cane (g/m2)

*   Constant values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'sugar_water_content')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------
      call push_routine (myname)

      ! initialise water contents
      ! -------------------------
      if (on_day_of (begcane, g_current_stage, g_days_tot)) then
          g_tt_for_dmf = 0.0
          pot_cane_dmf = linear_interp_real
     :                            (0.0
     :                            ,c_cane_dmf_tt
     :                            ,c_cane_dmf
     :                            ,c_num_cane_dmf)
         g_plant_wc(sstem) = divide(1.0 - pot_cane_dmf
     :                             ,pot_cane_dmf
     :                             ,0.0)
     :                     * (g_dm_green(sstem)+g_dm_green(sucrose))
      else
         ! ignore plant water content in this phase
      endif

      ! calculate change in plant water content
      ! ---------------------------------------
      if (stage_is_between (begcane, crop_end
     :                        , g_current_stage)) then
          g_tt_for_dmf = g_tt_for_dmf
     :                 + max(((g_maxt + g_mint)/2 - 8.),0.0)

          pot_cane_dmf = linear_interp_real
     :                            (g_tt_for_dmf
     :                            ,c_cane_dmf_tt
     :                            ,c_cane_dmf
     :                            ,c_num_cane_dmf)
         pot_cane_wc = divide(1.0 - pot_cane_dmf
     :                        ,pot_cane_dmf
     :                        ,0.0)
     :                  * (g_dm_green(sstem)  +g_dlt_dm_green(sstem)
     :                    +g_dm_green(sucrose)+g_dlt_dm_green(sucrose))

         g_dlt_plant_wc(sstem) = pot_cane_wc - g_plant_wc(sstem)

      else
         ! ignore plant water content in this phase
      endif


      call pop_routine (myname)
      return
      end
*     ===========================================================
      subroutine sugar_read_crop_constants (section_name)
*     ===========================================================

*   Short description:
*       Crop initialisation - reads constants from constants file

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
*                       implicit none

*   Changes:

*   Calls:
*     pop_routine
*     push_routine
*     read_integer_var
*     read_real_var
*     read_real_array
*     sugar_version
*     write_string

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      character section_name*(*)

*   Global variables
      include   'const.inc'
      include   'sugar.inc'

*   Internal variables
      integer    numvals               ! number of values returned

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_read_crop_constants')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      call write_string (lu_scr_sum
     :    ,new_line//'    - Reading constants from '//section_name)

         !    sugar_get_cultivar_params

      call read_char_var (section_name
     :                     , 'crop_type', '()'
     :                     , c_crop_type, numvals)

      call read_char_array (section_name
     :                     , 'stage_names', max_stage, '()'
     :                     , c_stage_names, numvals)

      call read_real_array (section_name
     :                     , 'stage_code', max_stage, '()'
     :                     , c_stage_code_list, numvals
     :                     , 0.0, 1000.0)

      call read_real_array (section_name
     :                     , 'rue', max_stage, '(g dm/mj)'
     :                     , c_rue, numvals
     :                     , 0.0, 1000.0)

      call read_real_array (section_name
     :                     , 'root_depth_rate', max_stage, '(mm)'
     :                     , c_root_depth_rate, numvals
     :                     , 0.0, 1000.0)

      call read_real_array (section_name
     :                     , 'ratio_root_shoot', max_stage, '()'
     :                     , c_ratio_root_shoot, numvals
     :                     , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'extinction_coef', '()'
     :                    , c_extinction_coef, numvals
     :                    , 0.0, 10.0)

      call read_real_var (section_name
     :                    , 'extinction_coef_dead', '()'
     :                    , c_extinction_coef_dead, numvals
     :                    , 0.0, 10.0)

         ! crop failure

      call read_real_var (section_name
     :                    , 'leaf_no_crit', '()'
     :                    , c_leaf_no_crit, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'tt_emerg_limit', '(oC)'
     :                    , c_tt_emerg_limit, numvals
     :                    , 0.0, 365.0)

      call read_real_var (section_name
     :                    , 'days_germ_limit', '(days)'
     :                    , c_days_germ_limit, numvals
     :                    , 0.0, 365.0)

      call read_real_var (section_name
     :                    , 'swdf_pheno_limit', '()'
     :                    , c_swdf_pheno_limit, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'swdf_photo_limit', '()'
     :                    , c_swdf_photo_limit, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'swdf_photo_rate', '()'
     :                    , c_swdf_photo_rate, numvals
     :                    , 0.0, 1.0)


         !    sugar_root_depth

      call read_real_var (section_name
     :                    , 'initial_root_depth', '(mm)'
     :                    , c_initial_root_depth, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'specific_root_length', '(mm)'
     :                    , c_specific_root_length, numvals
     :                    , 0.0, 50000.0)

      call read_real_var (section_name
     :                    , 'root_die_back_fr', '(0-1)'
     :                    , c_root_die_back_fr, numvals
     :                    , 0.0, 1.0)

         !    sugar_leaf_area_init

      call read_real_var (section_name
     :                    , 'initial_tpla', '(mm^2)'
     :                    , c_initial_tpla, numvals
     :                    , 0.0, 100000.0)

         !    sugar_leaf_area_devel
      call read_real_array (section_name
     :                    , 'sla_lfno',max_table,'()'
     :                    , c_sla_lfno, c_num_sla_lfno
     :                    , 0.0, 100.0)

      call read_real_array (section_name
     :                    , 'sla_max',max_table,'(mm^2/g)'
     :                    , c_sla_max, numvals
     :                    , 0.0, 50000.0)

      call read_real_array (section_name
     :                    , 'sla_min',max_table,'(mm^2/g)'
     :                    , c_sla_min, numvals
     :                    , 0.0, 50000.0)

         !    sugar_height

      call read_real_var (section_name
     :                    , 'height_max', '(mm)'
     :                    , c_height_max, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'height_stem_slope', '(mm/g/stem)'
     :                    , c_height_stem_slope, numvals
     :                    , 0.0, 1000.0)

         !    sugar_transp_eff

      call read_real_var (section_name
     :                    , 'svp_fract', '()'
     :                    , c_svp_fract, numvals
     :                    , 0.0, 1.0)

      call read_real_var (section_name
     :                    , 'transp_eff_cf', '(kpa)'
     :                    , c_transp_eff_cf, numvals
     :                    , 0.0, 1.0)

         !    sugar_germination

      call read_real_var (section_name
     :                    , 'pesw_germ', '(mm/mm)'
     :                    , c_pesw_germ, numvals
     :                    , 0.0, 1.0)

         !    sugar_leaf_appearance

      call read_real_var (section_name
     :                    , 'leaf_no_at_emerg', '()'
     :                    , c_leaf_no_at_emerg, numvals
     :                    , 0.0, 100.0)

         !    sugar_phenology_init

      call read_real_var (section_name
     :                    , 'shoot_lag', '(oC)'
     :                    , c_shoot_lag, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'shoot_rate', '(oC/mm)'
     :                    , c_shoot_rate, numvals
     :                    , 0.0, 100.0)

      call read_real_array (section_name
     :                    , 'leaf_app_rate', max_table,'(oC)'
     :                    , c_leaf_app_rate
     :                    , c_num_leaf_app_rate
     :                    , 0.0, 1000.0)

      call read_real_array (section_name
     :                    , 'leaf_app_rate_lfno', max_table,'(oC)'
     :                    , c_leaf_app_rate_lfno
     :                    , c_num_leaf_app_rate
     :                    , 0.0, 1000.0)

         !    sugar_dm_init

      call read_real_var (section_name
     :                    , 'dm_leaf_init', '(g/plant)'
     :                    , c_dm_leaf_init, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'dm_root_init', '(g/plant)'
     :                    , c_dm_root_init, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'dm_sstem_init', '(g/plant)'
     :                    , c_dm_sstem_init, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'dm_cabbage_init', '(g/plant)'
     :                    , c_dm_cabbage_init, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'dm_sucrose_init', '(g/plant)'
     :                    , c_dm_sucrose_init, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'leaf_cabbage_ratio', '()'
     :                    , c_leaf_cabbage_ratio, numvals
     :                    , 0.0, 10.0)


         !    sugar_dm_senescence

      call read_real_var (section_name
     :                    , 'dm_root_sen_frac', '()'
     :                    , c_dm_root_sen_frac, numvals
     :                    , 0.0, 1.0)

         !    sugar_dm_dead_detachment

      call read_real_array (section_name
     :                    , 'dead_detach_frac', max_part, '()'
     :                    , c_dead_detach_frac, numvals
     :                    , 0.0, 1.0)

      call read_real_var (section_name
     :                    , 'dm_leaf_detach_frac', '()'
     :                    , c_dm_leaf_detach_frac, numvals
     :                    , 0.0, 1.0)

         !    sugar_leaf_area_devel

      call read_real_var (section_name
     :                    , 'leaf_no_correction', '()'
     :                    , c_leaf_no_correction, numvals
     :                    , 0.0, 100.0)

         !    sugar_leaf_area_sen_light

      call read_real_var (section_name
     :                   , 'cover_sen_light', '(m^2/m^2)'
     :                   , c_lai_sen_light, numvals
     :                   , 0.0, 1.0)

      call read_real_var (section_name
     :                    , 'sen_light_slope', '()'
     :                    , c_sen_light_slope, numvals
     :                    , 0.0, 100.0)


         !    sugar_leaf_area_sen_frost

      call read_real_array (section_name
     :                   , 'frost_temp', max_table, '(oC)'
     :                   , c_frost_temp, c_num_frost_temp
     :                   , -20.0, 100.0)

      call read_real_array (section_name
     :                   , 'frost_fraction', max_table, '(oC)'
     :                   , c_frost_fraction, numvals
     :                   , 0.0, 1.0)

         !    sugar_leaf_area_sen_water

      call read_real_var (section_name
     :                    , 'sen_rate_water', '()'
     :                    , c_sen_rate_water, numvals
     :                    , 0.0, 100.0)

         !    sugar_phenology_init

      call read_real_var (section_name
     :                   , 'twilight', '(o)'
     :                   , c_twilight, numvals
     :                   , -90.0, 90.0)

         !    sugar_N_conc_limits

      call read_real_array (section_name
     :                     , 'x_stage_code', max_stage, '()'
     :                     , c_x_stage_code, c_num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_crit_leaf', max_stage, '()'
     :                     , c_y_N_conc_crit_leaf, c_num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_min_leaf', max_stage, '()'
     :                     , c_y_N_conc_min_leaf, c_num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_crit_cane', max_stage, '()'
     :                     , c_y_N_conc_crit_cane, c_num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_min_cane', max_stage, '()'
     :                     , c_y_N_conc_min_cane, c_num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_crit_cabbage', max_stage, '()'
     :                     , c_y_N_conc_crit_cabbage, c_num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_min_cabbage', max_stage, '()'
     :                     , c_y_N_conc_min_cabbage, c_num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'n_conc_crit_root', '()'
     :                   , c_N_conc_crit_root, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'n_conc_min_root', '()'
     :                   , c_N_conc_min_root, numvals
     :                   , 0.0, 100.0)

         !    sugar_N_init

      call read_real_var (section_name
     :                   , 'n_leaf_init_conc', '()'
     :                   , c_N_leaf_init_conc, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'n_root_init_conc', '()'
     :                   , c_N_root_init_conc, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'n_sstem_init_conc', '()'
     :                   , c_N_sstem_init_conc, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'n_cabbage_init_conc', '()'
     :                   , c_N_cabbage_init_conc, numvals
     :                   , 0.0, 100.0)

         !    sugar_N_senescence

      call read_real_var (section_name
     :                   , 'n_leaf_sen_conc', '()'
     :                   , c_N_leaf_sen_conc, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'n_root_sen_conc', '()'
     :                   , c_N_root_sen_conc, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'n_cabbage_sen_conc', '()'
     :                   , c_N_cabbage_sen_conc, numvals
     :                   , 0.0, 100.0)

         !    sugar_rue_reduction

      call read_real_array (section_name
     :                     , 'x_ave_temp', max_table, '(oC)'
     :                     , c_x_ave_temp, c_num_ave_temp
     :                     , 0.0, 100.0)


      call read_real_array (section_name
     :                     , 'y_stress_photo', max_table, '()'
     :                     , c_y_stress_photo, c_num_factors
     :                     , 0.0, 1.0)

         !    sugar_tt

      call read_real_array (section_name
     :                     , 'x_temp', max_table, '(oC)'
     :                     , c_x_temp, c_num_temp
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_tt', max_table, '(oC)'
     :                     , c_y_tt, c_num_temp
     :                     , 0.0, 100.0)

         !    sugar_swdef

      call read_real_array (section_name
     :                     , 'x_sw_demand_ratio', max_table, '()'
     :                     , c_x_sw_demand_ratio, c_num_sw_demand_ratio
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_swdef_leaf', max_table, '()'
     :                     , c_y_swdef_leaf, c_num_sw_demand_ratio
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'x_sw_avail_ratio', max_table, '()'
     :                     , c_x_sw_avail_ratio, c_num_sw_avail_ratio
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_swdef_pheno', max_table, '()'
     :                     , c_y_swdef_pheno, c_num_sw_avail_ratio
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'x_sw_ratio', max_table, '()'
     :                     , c_x_sw_ratio, c_num_sw_ratio
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_sw_fac_root', max_table, '()'
     :                     , c_y_sw_fac_root, c_num_sw_ratio
     :                     , 0.0, 100.0)

      ! Water logging function
      ! ----------------------
      call read_real_array (section_name
     :                     , 'water_log_rtfr', max_table, '()'
     :                     , c_water_log_rtfr, c_num_water_log_fact
     :                     , 0.0, 1.0)
      call read_real_array (section_name
     :                     , 'water_log_fact', max_table, '()'
     :                     , c_water_log_fact, c_num_water_log_fact
     :                     , 0.0, 1.0)

      ! Plant Water Content function
      ! ----------------------------
      call read_real_array (section_name
     :                     , 'cane_dmf', max_table, '()'
     :                     , c_cane_dmf, c_num_cane_dmf
     :                     , 0.0, 1.0)
      call read_real_array (section_name
     :                     , 'cane_dmf_tt', max_table, '()'
     :                     , c_cane_dmf_tt, c_num_cane_dmf
     :                     , 0.0, 10000.)

      call pop_routine (my_name)
      return
      end

*     ===========================================================
      real function sugar_sucrose_fraction ()
*     ===========================================================

*   Short description:
*     Returns the fraction of Cane C partioned to sucrose based
*     upon severity of water stress(cell expansion)

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
*                       implicit none

*   Changes:
*       240796 nih/mjr programmed and specified

*   Calls:
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*     none

*   Global variables
      include   'sugar.inc'
      real       bound                 ! function
      real       linear_interp_real    ! function
      real       sugar_swdef           ! function

*   Internal variables
      real       sucrose_fraction      ! fraction of cane C partitioned
                                       ! to sucrose (0-1)
      real       swdef_cellxp          ! 0-1 stress factor on
                                       ! cell expansion
      real       Adjustment_factor     ! scalar adjustment factor upon
                                       ! sucrose_fraction

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_sucrose_fraction')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      swdef_cellxp = sugar_swdef(expansion)

      Adjustment_factor = linear_interp_real
     :                     (swdef_cellxp
     :                     ,c_x_swdef_cellxp
     :                     ,c_y_sw_fac_sucrose
     :                     ,c_num_x_swdef_cellxp
     :                     )

      sucrose_fraction = c_sucrose_fraction * Adjustment_factor

      call bound_check_real_var (sucrose_fraction
     :                        , 0.0
     :                        , 1.0
     :                        , 'fraction of Cane C to sucrose')

      sucrose_fraction = bound (sucrose_fraction, 0.0, 1.0)

      sugar_sucrose_fraction = sucrose_fraction

      call pop_routine (my_name)
      return
      end

