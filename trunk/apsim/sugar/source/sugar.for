*     ===========================================================
      subroutine sugar_phenology (Option)
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
*     010994 jngh specified and programmed

*   Calls:
*     accumulate
*     pop_routine
*     push_routine
*     sugar_canopy_height
*     sugar_devel
*     sugar_phenology_init
*     sugar_tt
*     sugar_tt_curv
*     sugar_tt_other

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer    Option                ! (INPUT) option number

*   Global variables
      include   'const.inc'
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

      if (Option .eq. 1) then

         g_previous_stage = g_current_stage

            ! get thermal times

         call sugar_thermal_time
     :               (
     :                C_num_temp
     :              , C_x_temp
     :              , C_y_tt
     :              , G_maxt
     :              , G_mint
     :              , g_current_Stage
     :              , emerg
     :              , g_st
     :              , g_sowing_depth
     :              , g_dlayer
     :              , max_layer
     :              , g_dlt_tt
     :               )

            ! initialise phenology phase targets

         call sugar_phenology_init
     :               (
     :                C_shoot_lag
     :              , C_shoot_rate
     :              , G_current_stage
     :              , G_days_tot
     :              , G_sowing_depth
     :              , G_Ratoon_no
     :              , P_tt_begcane_to_flowering
     :              , P_tt_emerg_to_begcane
     :              , P_tt_flowering_to_crop_end
     :              , g_phase_tt
     :               )
         call sugar_phase_devel
     :               (
     :                G_current_stage
     :              , C_pesw_germ
     :              , c_fasw_emerg
     :              , c_rel_emerg_rate
     :              , c_num_fasw_emerg
     :              , G_days_tot
     :              , G_dlayer
     :              , G_sowing_depth
     :              , G_sw_dep
     :              , P_ll_dep
     :              , G_dul_dep
     :              , G_dlt_tt
     :              , G_phase_tt
     :              , G_tt_tot
     :              , g_phase_devel
     :               )
         call sugar_devel
     :               (
     :                G_current_stage
     :              , G_phase_devel
     :              , g_dlt_stage, g_current_stage
     :               )

            ! update thermal time states and day count

         call accumulate (g_dlt_tt, g_tt_tot
     :                  , g_previous_stage, g_dlt_stage)

         call accumulate (1.0, g_days_tot
     :                   , g_previous_stage, g_dlt_stage)


      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_thermal_time 
     :               (
     :                C_num_temp
     :              , C_x_temp
     :              , C_y_tt
     :              , G_maxt
     :              , G_mint
     :              , g_current_Stage
     :              , emergence
     :              , g_st
     :              , g_sowing_depth
     :              , g_dlayer
     :              , max_layer
     :              , dlt_tt
     :               )
*     ===========================================================

*   Short description:
*     Today's thermal time (expresses using "growing degree days"
*     is calculated from a specified crop developmental response
*     to temperature.  Three hourly temperature values are calculated
*     from a set distribution between maximum and minimum temperature
*     for the day being simulated.

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
      INTEGER    C_num_temp            ! (INPUT)  size_of of table
      REAL       C_x_temp(*)           ! (INPUT)  temperature table for photosynthesis degree days
      REAL       C_y_tt(*)             ! (INPUT)  degree days
      REAL       G_maxt                ! (INPUT)  maximum air temperature (oC)
      REAL       G_mint                ! (INPUT)  minimum air temperature (oC)
      real       dlt_tt                ! (OUTPUT) daily thermal time (oC)
      real       g_sowing_depth
      real       g_st(*)
      integer    emergence
      real       g_current_stage
      real       g_dlayer(*)
      integer    max_layer

*   Global variables
cnh      include    'crop3.inc'

      real       linint_3hrly_temp     ! function
c      logical    stage_is_between      ! function
c      integer    find_layer_no         ! function
c      real       linear_interp_Real    ! function

*   Internal variables
      real       dly_therm_time        ! thermal time for the day (deg day)
c      integer    sowing_layer

*   Constant values

      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_thermal_time')

*   Initial data values
*     none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

cnh !
c      if (g_current_stage.ge.emergence) then

         dly_therm_time = linint_3hrly_temp (g_maxt, g_mint
     :                    , c_x_temp, c_y_tt
     :                    , c_num_temp)
c      else
c         sowing_layer = find_layer_no (g_sowing_depth
c     :                                ,g_dlayer
c     :                                ,max_layer)
c
c         dly_therm_time = linear_interp_real (g_st(sowing_layer)
c     :                                       ,c_x_temp
c     :                                       ,c_y_tt
c     :                                       ,c_num_temp)
c      endif

c      if (stage_is_between (emerg, flag_leaf, g_current_stage)) then

c         dlt_tt = dly_therm_time *g_swdef_pheno
c      else

         dlt_tt = dly_therm_time
c      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_phenology_init 
     :               (
     :                C_shoot_lag
     :              , C_shoot_rate
     :              , G_current_stage
     :              , G_days_tot
     :              , G_sowing_depth
     :              , G_Ratoon_no
     :              , P_tt_begcane_to_flowering
     :              , P_tt_emerg_to_begcane
     :              , P_tt_flowering_to_crop_end
     :              , phase_tt
     :               )
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
      REAL       C_shoot_lag           ! (INPUT)  minimum growing degree days for germination (deg days)
      REAL       C_shoot_rate          ! (INPUT)  growing deg day increase with depth for germination (deg day/mm depth)
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
      REAL       G_sowing_depth        ! (INPUT)  sowing depth (mm)
      REAL       G_Ratoon_no           ! (INPUT)  ratoon no (mm)
      REAL       P_tt_begcane_to_flowering ! (INPUT)
      REAL       P_tt_emerg_to_begcane ! (INPUT)
      REAL       P_tt_flowering_to_crop_end ! (INPUT)
      real       phase_tt (*)          ! (INPUT/OUTPUT) cumulative growing
                                       ! degree days required for
                                       ! each stage (deg days)

*   Global variables
      include   'crop3.inc'

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
         if (G_ratoon_no .eq. 0) then
            phase_tt(sprouting_to_emerg) = c_shoot_lag
     :                                   + g_sowing_depth*c_shoot_rate
         else
            ! Assume the mean depth of shooting is half way between the
            ! set depth and the soil surface.
            phase_tt(sprouting_to_emerg) = c_shoot_lag
     :                                   + g_sowing_depth/2.0
     :                                   * c_shoot_rate
         endif
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
      subroutine sugar_devel
     :               (
     :                G_current_stage
     :              , G_phase_devel
     :              , dlt_stage, current_stage
     :               )
*     ===========================================================

*   Short description:
*     Determine the current stage of development.

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
*     300996 nih added fix to template phenology error

*   Calls:
*     pop_routine
*     push_routine
*     sugar_phase_devel

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_phase_devel         ! (INPUT)  development of current phase (
      real       dlt_stage             ! (OUTPUT) change in growth stage
      real       current_stage         ! (OUTPUT) new stage no.

*   Global variables
      include   'crop3.inc'

*   Internal variables
      real       new_stage             ! new stage number

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_devel')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

         ! mechanical operation - not to be changed

         ! now calculate the new delta and the new stage

      new_stage = aint (g_current_stage) + g_phase_devel
      dlt_stage = new_stage - g_current_stage

      if (g_phase_devel.ge.1.0) then
cnh         current_stage = aint (new_stage)
         current_stage = aint (current_stage + 1.0)

      else
         current_stage = new_stage

      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_phase_devel
     :               (
     :                G_current_stage
     :              , C_pesw_germ
     :              , c_fasw_emerg
     :              , c_rel_emerg_rate
     :              , c_num_fasw_emerg
     :              , G_days_tot
     :              , G_dlayer
     :              , G_sowing_depth
     :              , G_sw_dep
     :              , P_ll_dep
     :              , G_dul_dep
     :              , G_dlt_tt
     :              , G_phase_tt
     :              , G_tt_tot
     :              , phase_devel
     :               )
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
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       C_pesw_germ           ! (INPUT)  plant extractable soil water i
      REAL       C_fasw_emerg(*)       ! (INPUT)  plant extractable soil water i
      REAL       c_rel_emerg_rate(*)   ! (INPUT)
      REAL       c_num_fasw_emerg      ! (INPUT)
      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL       G_sowing_depth        ! (INPUT)  sowing depth (mm)
      REAL       G_sw_dep(*)           ! (INPUT)  soil water content of layer L
      REAL       P_ll_dep(*)           ! (INPUT)  lower limit of plant-extractab
      REAL       G_dul_dep(*)          ! (INPUT)  drained upper limit
      REAL       G_dlt_tt              ! (INPUT)  daily thermal time (growing de
      REAL       G_phase_tt(*)         ! (INPUT)  Cumulative growing degree days
      REAL       G_tt_tot(*)           ! (INPUT)  the sum of growing degree days
      real       phase_devel           ! (OUTPUT) fraction of current phase
                                       ! elapsed ()

*   Global variables
      include   'crop3.inc'

      real       sugar_sprouting       ! function
      real       sugar_phase_tt        ! function
      logical    stage_is_between      ! function

*   Internal variables
      real dlt_tt

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_phase_devel')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      if (stage_is_between (sowing, sprouting, g_current_stage)) then
         phase_devel = sugar_sprouting
     :               (
     :                C_pesw_germ
     :              , G_current_stage
     :              , G_days_tot
     :              , G_dlayer
     :              , G_sowing_depth
     :              , G_sw_dep
     :              , P_ll_dep
     :              , g_current_stage
     :               )

      elseif (stage_is_between (sprouting, crop_end
     :                        , g_current_stage)) then


      call sugar_phase_dlt_tt
     :               (
     :                C_fasw_emerg
     :              , c_rel_emerg_rate
     :              , c_num_fasw_emerg
     :              , G_current_stage
     :              , G_days_tot
     :              , G_dlayer
     :              , G_sowing_depth
     :              , G_sw_dep
     :              , P_ll_dep
     :              , g_dul_dep
     :              , g_dlt_tt
     :              , dlt_tt
     :               )
         phase_devel =  sugar_phase_tt
     :               (
     :                dlt_tt
     :              , G_phase_tt
     :              , G_tt_tot
     :              , g_current_stage
     :               )

      else
         phase_devel = 0.0

      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      real function sugar_phase_tt
     :               (
     :                dlt_tt
     :              , G_phase_tt
     :              , G_tt_tot
     :              , stage_no
     :               )
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
      REAL       dlt_tt                ! (INPUT)  daily thermal time (growing de
      REAL       G_phase_tt(*)         ! (INPUT)  Cumulative growing degree days
      REAL       G_tt_tot(*)           ! (INPUT)  the sum of growing degree days
      real       stage_no              ! (INPUT) stage number

*   Global variables
      include   'crop3.inc'

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
cnh this phenology code is RIDICULOUS - I think the following
cnh change will still work and remove the link to g_tt_tot
cnh      sugar_phase_tt = divide (g_tt_tot(phase) + g_dlt_tt
cnh     :                       , g_phase_tt(phase), 0.0)

      sugar_phase_tt = mod(stage_no,1.)
     :               + divide (dlt_tt
     :                        ,g_phase_tt(phase)
     :                        ,0.0)

      sugar_phase_tt = bound (sugar_phase_tt, 0.0, 1.999999)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_canopy_height 
     :               (
     :                C_height_max
     :              , C_height_stem_slope
     :              , G_canopy_height
     :              , G_current_stage
     :              , G_dm_green
     :              , G_plants
     :              , dlt_canopy_height
     :               )
*     ===========================================================

*   Short description:
*       This routine calculates the daily change in canopy height.

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
      REAL       C_height_max          ! (INPUT)  maximum canopy height (mm)
      REAL       C_height_stem_slope   ! (INPUT)  rate of height growth (mm/g/stem)
      REAL       G_canopy_height       ! (INPUT)  canopy height (mm)
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass) (g/m^2)
      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
      real       dlt_canopy_height     ! (INPUT) canopy height change (mm)

*   Global variables
      include   'crop3.inc'

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
      subroutine sugar_root_depth (Option)
*     ===========================================================

*   Short description:
*       Plant root depth calculations

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
*      250894 jngh specified and programmed

*   Calls:
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer    Option                ! (INPUT) option number

*   Global variables
      include   'const.inc'
      include   'sugar.inc'

      integer    find_layer_no         ! function
      real       sugar_sw_avail_fac    ! function

*   Internal variables
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_root_depth')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
c+!!!!!!!!! check order dependency of deltas
      call push_routine (my_name)

      if (Option .eq. 1) then

         deepest_layer = find_layer_no (g_root_depth, g_dlayer
     :                                 , max_layer)
         g_sw_avail_fac_deepest_layer = sugar_sw_avail_fac 
     :               (
     :                C_num_sw_ratio
     :              , C_x_sw_ratio
     :              , C_y_sw_fac_root
     :              , G_dul_dep
     :              , G_sw_dep
     :              , P_ll_dep
     :              , deepest_layer
     :               )
         call sugar_root_depth_increase 
     :               (
     :                C_root_depth_rate
     :              , G_current_stage
     :              , G_dlayer
     :              , G_root_depth
     :              , P_xf
     :              , C_num_sw_ratio
     :              , C_x_sw_ratio
     :              , C_y_sw_fac_root
     :              , G_dul_dep
     :              , G_sw_dep
     :              , P_ll_dep
     :              , g_dlt_root_depth
     :               )
         call sugar_root_depth_init 
     :               (
     :                G_dlayer
     :              , G_rlv
     :              , G_root_depth
     :              , g_dlt_root_depth
     :               )
                               !NOTE THIS IS STILL THE DELTA
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      real function sugar_sw_avail_fac 
     :               (
     :                C_num_sw_ratio
     :              , C_x_sw_ratio
     :              , C_y_sw_fac_root
     :              , G_dul_dep
     :              , G_sw_dep
     :              , P_ll_dep
     :              , layer
     :               )
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
      INTEGER    C_num_sw_ratio        ! (INPUT)
      REAL       C_x_sw_ratio(*)       ! (INPUT)
      REAL       C_y_sw_fac_root(*)    ! (INPUT)
      REAL       G_dul_dep(*)          ! (INPUT)  drained upper limit soil water content for soil layer L (mm water)
      REAL       G_sw_dep(*)           ! (INPUT)  soil water content of layer L (mm)
      REAL       P_ll_dep(*)           ! (INPUT)  lower limit of plant-extractable soil water for soil layer L (mm)
      integer    layer                 ! (INPUT) soil profile layer number

*   Global variables
      include   'crop3.inc'

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
      subroutine sugar_sw_avail_pot 
     :               (
     :                G_dlayer
     :              , G_dul_dep
     :              , G_root_depth
     :              , P_ll_dep
     :              , sw_avail_pot
     :               )
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
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL       G_dul_dep(*)          ! (INPUT)  drained upper limit soil water content for soil layer L (mm water)
      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
      REAL       P_ll_dep(*)           ! (INPUT)  lower limit of plant-extractable soil water for soil layer L (mm)
      real       sw_avail_pot(*)       ! (OUTPUT) crop water potential uptake
                                       ! for each full layer (mm)

*   Global variables
      include   'crop3.inc'

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
      subroutine sugar_sw_avail 
     :               (
     :                G_dlayer
     :              , G_root_depth
     :              , G_sw_dep
     :              , P_ll_dep
     :              , sw_avail
     :               )
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
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
      REAL       G_sw_dep(*)           ! (INPUT)  soil water content of layer L (mm)
      REAL       P_ll_dep(*)           ! (INPUT)  lower limit of plant-extractable soil water for soil layer L (mm)
      real       sw_avail(*)           ! (OUTPUT) crop water potential uptake
                                       ! for each full layer (mm)

*   Global variables
      include   'crop3.inc'

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
      subroutine sugar_sw_demand 
     :               (
     :                G_dlt_dm_pot_rue
     :              , G_transp_eff
     :              , sw_demand
     :               )
*     ===========================================================

*   Short description:
*       This routine calculates the water demand from soil by the crop
*       by applying a transpiration efficiency to a soil supply non-limiting
*       growth rate.

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
      REAL       G_dlt_dm_pot_rue      ! (INPUT)
      REAL       G_transp_eff          ! (INPUT)
      real       sw_demand             ! (OUTPUT) crop water demand (mm)

*   Global variables
      include   'crop3.inc'

      real       divide                ! function

*   Internal variables
*     none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_sw_demand')

*   Initial data values
*     none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

            ! calculate potential above-ground biomass production

cnh      call sugar_dm_potential (dlt_dm_pot)

            ! get potential transpiration from potential
            ! carbohydrate production and transpiration efficiency

      sw_demand = divide (g_dlt_dm_pot_rue, g_transp_eff, 0.0)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      real function sugar_rue_reduction
     :               (
     :                G_nfact_photo
     :              , G_temp_stress_photo
     :              , G_water_log_fact
     :               )
*     ===========================================================

*   Short description:
*       The overall fractional effect of non-optimal N, Temperature,
*       and water logging conditions on radiation use efficiency.

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

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      REAL       G_nfact_photo         ! (INPUT)
      REAL       G_temp_stress_photo   ! (INPUT)
      REAL       G_water_log_fact      ! (INPUT)
*     none

*   Global variables
      include   'crop3.inc'

*   Internal variables
*     none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_rue_reduction')

*   Initial data values

* --------------------- Executable code section ----------------------

      call push_routine (my_name)


      sugar_rue_reduction = min (g_temp_stress_photo
     :                      , g_nfact_photo
     :                      ,g_water_log_fact)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_radn_int 
     :               (
     :                C_extinction_coef
     :              , G_fr_intc_radn
     :              , G_lai
     :              , G_radn
     :              , radn_int
     :               )
*     ===========================================================

*   Short description:
*       This routine returns the radiation intercepted by leaves (mj/m^2)

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
      REAL       C_extinction_coef     ! (INPUT)  radiation extinction coefficient ()
      REAL       G_fr_intc_radn        ! (INPUT)  fraction of radiation intercepted by canopy
      REAL       G_lai                 ! (INPUT)  live plant green lai
      REAL       G_radn                ! (INPUT)  solar radiation (Mj/m^2/day)
      real       radn_int              ! (OUTPUT) radiation intercepted
                                       ! by leaves (mj/m^2)

*   Global variables
      include   'crop3.inc'

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
      subroutine sugar_transp_eff 
     :               (
     :                C_svp_fract
     :              , C_transp_eff_cf
     :              , G_maxt
     :              , G_mint
     :              , transp_eff
     :               )
*     ===========================================================

*   Short description:
*       Calculate today's transpiration efficiency from a vapour
*       pressure deficit calculated from daily temperatures as
*       follows.
*
*       Average saturation vapour pressure for ambient temperature
*       during transpiration is calculated as part-way between that
*       for minimum temperature and that for the maximum temperature.
*       Tanner & Sinclair (1983) used .75 and .67 of the distance as
*       representative of the positive net radiation (rn).  Daily SVP
*       should be integrated from about 0900 hours to evening when Radn
*       becomes negetive.

*   Assumptions:
*       the temperatures are > -237.3 oC for the svp function.

*   Notes:

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
      REAL       C_svp_fract           ! (INPUT)  fraction of distance between svp at min temp and svp at max temp where average svp during transpiration lies. (0-1)
      REAL       C_transp_eff_cf       ! (INPUT)  transpiration efficiency coefficient to convert vpd to transpiration efficiency (kpa) although this is expressed as a pressure it is really in the form kpa*g carbo per m^2 / g water per m^2 and this
      REAL       G_maxt                ! (INPUT)  maximum air temperature (oC)
      REAL       G_mint                ! (INPUT)  minimum air temperature (oC)
      real       transp_eff            ! (OUTPUT)

*   Global variables
      include   'convert.inc'          ! g2mm, mb2kpa
      include   'crop3.inc'

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
      transp_eff = divide (c_transp_eff_cf, vpd, 0.0) /g2mm

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_sw_supply
     :               (
     :                G_dlayer
     :              , G_root_depth
     :              , G_sw_dep
     :              , P_kl
     :              , P_ll_dep
     :              , sw_supply
     :               )
*     ===========================================================

*   Short description:
*       Return potential water uptake from each layer of the soil profile
*       by the crop (mm water/day). This represents the maximum amount in
*       each layer regardless of lateral root distribution but takes
*       account of root depth in bottom layer.

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
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
      REAL       G_sw_dep(*)           ! (INPUT)  soil water content of layer L
      REAL       P_kl(*)               ! (INPUT)  root length density factor for
      REAL       P_ll_dep(*)           ! (INPUT)  lower limit of plant-extractab
      real       sw_supply(*)          ! (OUTPUT) potential crop water uptake
                                       ! from each layer (mm) (supply to roots)

*   Global variables
      include   'crop3.inc'

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
      subroutine sugar_sw_uptake 
     :               (
     :                G_dlayer
     :              , G_root_depth
     :              , G_sw_demand
     :              , G_sw_supply
     :              , dlt_sw_dep
     :               )
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
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
      REAL       G_sw_demand           ! (INPUT)  total crop demand for water (mm)
      REAL       G_sw_supply(*)        ! (INPUT)  potential water to take up (supply) from current soil water (mm)
      real       dlt_sw_dep (*)        ! (OUTPUT) root water uptake (mm)

*   Global variables
      include   'crop3.inc'

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
      subroutine sugar_leaf_area_potential (Option)
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
*      250894 jngh specified and programmed

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
      integer    Option                ! (INPUT) option number

*   Global variables
      include   'const.inc'
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
      if (Option .eq. 1) then

            ! initialise total leaf number
         call sugar_leaf_area_init 
     :               (
     :                C_initial_tpla
     :              , G_current_stage
     :              , G_days_tot
     :              , G_plants
     :              , g_lai
     :              , g_leaf_area
     :               )
         call sugar_leaf_area_devel 
     :               (
     :                C_leaf_no_correction
     :              , G_dlt_leaf_no
     :              , G_leaf_no
     :              , G_plants
     :              , C_leaf_size
     :              , C_leaf_size_no
     :              , C_num_leaf_size
     :              , C_num_tillerf_leaf_size
     :              , C_tillerf_leaf_size
     :              , C_tillerf_leaf_size_no
     :              , g_dlt_lai_pot
     :               )

c         g_dlt_lai_stressed = g_dlt_lai_pot
c     :                    * g_nfact_expansion
c     :                    * g_swdef_expansion

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_leaf_area_init 
     :               (
     :                C_initial_tpla
     :              , G_current_stage
     :              , G_days_tot
     :              , G_plants
     :              , lai
     :              , leaf_area
     :               )
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
      REAL       C_initial_tpla        ! (INPUT)  initial plant leaf area (mm^2)
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
      real       lai                   ! (OUTPUT) total plant leaf area
      real       leaf_area(*)          ! (OUTPUT) plant leaf areas

*   Global variables
      include   'convert.inc'
      include   'crop3.inc'

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
         leaf_area(1) = c_initial_tpla
      else
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_leaf_appearance
     :               (
     :                C_leaf_app_rate
     :              , C_leaf_app_rate_lfno
     :              , C_num_leaf_app_rate
     :              , G_current_stage
     :              , G_days_tot
     :              , G_dlt_tt
     :              , G_leaf_no
     :              , g_leaf_area
     :              , dlt_leaf_no
     :               )
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
*     300996 nih added fix to template leaf no error (ie dlt_leaf_no
*                is zero on first day after emergence.  The leaf_no_init
*                routine sets the value from the value in the ini file.)

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
      REAL       C_leaf_app_rate(*)    ! (INPUT)
      REAL       C_leaf_app_rate_lfno(*) ! (INPUT)
      INTEGER    C_num_leaf_app_rate   ! (INPUT)
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
      REAL       G_dlt_tt              ! (INPUT)  daily thermal time (growing de
      REAL       G_leaf_no(*)          ! (INPUT)  number of fully expanded leave
      REAL       G_leaf_area(*)        ! (INPUT)  record of area of each attache
      real       dlt_leaf_no           ! (OUTPUT) new fraction of oldest
                                       ! expanding leaf

*   Global variables
      include   'crop3.inc'

      integer    count_of_real_vals    ! function
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

cnh         dlt_leaf_no = c_leaf_no_at_emerg

         ! leaf growth on first day is set in the leaf_no_init routine
         dlt_leaf_no = 0.0

      elseif (stage_is_between (emerg, flowering, g_current_stage)) then

             ! we  haven't reached full number of leaves yet

             ! if leaves are still growing, the cumulative number of
             ! phyllochrons or fully expanded leaves is calculated from
             ! daily thermal time for the day.

         leaf_no_today = sum_between (emerg, now, g_leaf_no)
cnh leaf no remaining now reflects the number of empty leaf records
cnh we have left to fill.
cnh         leaf_no_remaining = max_leaf - leaf_no_today

         leaf_no_remaining = max_leaf
     :                     - count_of_real_vals(g_leaf_area,max_leaf)

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
      subroutine sugar_leaf_area_devel
     :               (
     :                C_leaf_no_correction
     :              , G_dlt_leaf_no
     :              , G_leaf_no
     :              , G_plants
     :              , C_leaf_size
     :              , C_leaf_size_no
     :              , C_num_leaf_size
     :              , C_num_tillerf_leaf_size
     :              , C_tillerf_leaf_size
     :              , C_tillerf_leaf_size_no
     :              , dlt_lai_pot
     :               )
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

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      REAL       C_leaf_no_correction  ! (INPUT)  corrects for other growing lea
      REAL       G_dlt_leaf_no         ! (INPUT)  fraction of oldest leaf expand
      REAL       G_leaf_no(*)          ! (INPUT)  number of fully expanded leave
      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
      REAL       C_leaf_size(*)        ! (INPUT)
      REAL       C_leaf_size_no(*)     ! (INPUT)
      INTEGER    C_num_leaf_size       ! (INPUT)
      INTEGER    C_num_tillerf_leaf_size ! (INPUT)
      REAL       C_tillerf_leaf_size(*) ! (INPUT)
      REAL       C_tillerf_leaf_size_no(*) ! (INPUT)
      real       dlt_lai_pot           ! (OUTPUT) change in leaf area

*   Global variables
      include   'convert.inc'
      include   'crop3.inc'

      real       sugar_leaf_size       ! function
cbak
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
      area = sugar_leaf_size
     :               (
     :                C_leaf_size
     :              , C_leaf_size_no
     :              , C_num_leaf_size
     :              , C_num_tillerf_leaf_size
     :              , C_tillerf_leaf_size
     :              , C_tillerf_leaf_size_no
     :              , leaf_no_effective
     :               )

cbak

      dlt_lai_pot = g_dlt_leaf_no * area * smm2sm * g_plants
cnh     :            * min (g_swdef_expansion, g_nfact_expansion)


      call pop_routine (my_name)
      return
      end
*     ===========================================================
      real function sugar_leaf_size 
     :               (
     :                C_leaf_size
     :              , C_leaf_size_no
     :              , C_num_leaf_size
     :              , C_num_tillerf_leaf_size
     :              , C_tillerf_leaf_size
     :              , C_tillerf_leaf_size_no
     :              , leaf_no
     :               )
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
      REAL       C_leaf_size(*)        ! (INPUT)
      REAL       C_leaf_size_no(*)     ! (INPUT)
      INTEGER    C_num_leaf_size       ! (INPUT)
      INTEGER    C_num_tillerf_leaf_size ! (INPUT)
      REAL       C_tillerf_leaf_size(*) ! (INPUT)
      REAL       C_tillerf_leaf_size_no(*) ! (INPUT)
      real       leaf_no               ! (INPUT) nominated leaf number

*   Global variables
      include   'crop3.inc'

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
      subroutine sugar_leaf_area
     :               (
     :                G_dlt_dm_green
     :              , G_dlt_lai
     :              , G_dlt_lai_stressed
     :              , G_dlt_leaf_no
     :              , G_leaf_no
     :              , C_num_sla_lfno
     :              , C_sla_lfno
     :              , C_sla_max
     :               )
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
      REAL       G_dlt_dm_green(*)     ! (INPUT)  plant biomass growth (g/m^2)
      REAL       G_dlt_lai             ! (INPUT)  actual change in live plant la
      REAL       G_dlt_lai_stressed    ! (INPUT)  potential change in live plant
      REAL       G_dlt_leaf_no         ! (INPUT)  fraction of oldest leaf expand
      REAL       G_leaf_no(*)          ! (INPUT)  number of fully expanded leave
      INTEGER    C_num_sla_lfno        ! (INPUT)
      REAL       C_sla_lfno(*)         ! (INPUT)
      REAL       C_sla_max(*)          ! (INPUT)  maximum specific leaf area for
*       none

*   Global variables
      include   'convert.inc'
      include   'crop3.inc'

      real       sugar_sla_max         ! function
      real       sum_between           ! function

*   Internal variables
      real dlt_lai_carbon     ! maximum daily increase in leaf area
                              ! index from carbon supply
      real leaf_no_today      ! total number of leaves today
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

      sla_max = sugar_sla_max
     :               (
     :                C_num_sla_lfno
     :              , C_sla_lfno
     :              , C_sla_max
     :              , leaf_no_today
     :               )
      dlt_lai_carbon = g_dlt_dm_green(leaf) * sla_max * smm2sm

      g_dlt_lai = min (g_dlt_lai_stressed, dlt_lai_carbon)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_leaf_death (Option)
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
*     010994 jngh specified and programmed

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
      integer    Option                ! (INPUT) option number

*   Global variables
      include   'const.inc'
      include   'sugar.inc'

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_leaf_death')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      if (Option .eq. 1) then
         call sugar_leaf_death_grass  
     :               (
     :                C_green_leaf_no
     :              , G_current_stage
     :              , G_days_tot
     :              , G_dlt_leaf_no
     :              , G_leaf_no
     :              , G_leaf_no_dead
     :              , g_dlt_leaf_no_dead
     :               )
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_leaf_area_sen_age (Option)
*     ===========================================================
*   Short description:
*       Return the lai that senesces on the current day from age

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
*     010994 jngh specified and programmed

*   Calls:
*     pop_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer    Option                ! (INPUT) option number

*   Global variables
      include   'const.inc'
      include   'sugar.inc'

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_leaf_area_sen_age')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

         ! get senescense from age

      if (Option .eq. 1) then

         call sugar_leaf_area_sen_age0  
     :               (
     :                G_dlt_leaf_no_dead
     :              , G_lai
     :              , G_leaf_area
     :              , G_leaf_no_dead
     :              , G_plants
     :              , G_slai
     :              , G_leaf_no_detached
     :              , C_leaf_no_at_emerg
     :              , g_dlt_slai_age
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_leaf_area_sen_water (Option)
*     ===========================================================
*   Short description:
*       Return the lai that senesces on the current day  from water

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
*     010994 jngh specified and programmed

*   Calls:
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer    Option                ! (INPUT) option number

*   Global variables
      include   'const.inc'
      include   'sugar.inc'

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_leaf_area_sen_water')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      if (Option .eq. 1) then

         call sugar_leaf_area_sen_water0 
     :               (
     :                C_sen_rate_water
     :              , G_lai
     :              , G_swdef_photo
     :              , g_dlt_slai_water
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_leaf_area_sen_light (Option)
*     ===========================================================
*   Short description:
*       Return the lai that senesces on the current day from light

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
*     010994 jngh specified and programmed

*   Calls:
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer    Option                ! (INPUT) option number

*   Global variables
      include   'const.inc'
      include   'sugar.inc'

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_leaf_area_sen_light')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)


      if (Option .eq. 1) then

         call sugar_leaf_area_sen_light0  
     :               (
     :                C_extinction_coef
     :              , C_lai_sen_light
     :              , C_sen_light_slope
     :              , G_lai
     :              , g_dlt_slai_light
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_leaf_area_sen_frost (Option)
*     ===========================================================
*   Short description:
*       Return the lai that senesces on the current day from frost

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
*     010994 jngh specified and programmed

*   Calls:
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer    Option                ! (INPUT) option number

*   Global variables
      include   'const.inc'
      include   'sugar.inc'

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_leaf_area_sen_frost')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      if (Option .eq. 1) then

         call sugar_leaf_area_sen_frost0  
     :               (
     :                C_frost_fraction
     :              , C_frost_temp
     :              , C_num_frost_temp
     :              , G_lai
     :              , G_mint
     :              , g_dlt_slai_frost
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_dm_senescence 
     :               (
     :                C_dm_root_sen_frac
     :              , C_leaf_cabbage_ratio
     :              , C_cabbage_sheath_fr
     :              , G_dlt_dm_green
     :              , G_dlt_lai
     :              , G_dlt_slai
     :              , G_dm_green
     :              , G_dm_senesced
     :              , G_lai
     :              , G_leaf_dm
     :              , G_plants
     :              , G_slai
     :              , G_leaf_area
     :              , dlt_dm_senesced
     :               )
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
      REAL       C_dm_root_sen_frac    ! (INPUT)  fraction of root dry matter senescing each day (0-1)
      REAL       C_leaf_cabbage_ratio  ! (INPUT)  ratio of leaf wt to cabbage wt ()
      REAL       C_cabbage_sheath_fr   ! (INPUT)  fraction of cabbage that is leaf sheath (0-1)
      REAL       G_dlt_dm_green(*)     ! (INPUT)  plant biomass growth (g/m^2)
      REAL       G_dlt_lai             ! (INPUT)  actual change in live plant lai
      REAL       G_dlt_slai            ! (INPUT)  area of leaf that senesces from plant
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass) (g/m^2)
      REAL       G_dm_senesced(*)      ! (INPUT)  senesced plant dry wt (g/m^2)
      REAL       G_lai                 ! (INPUT)  live plant green lai
      REAL       G_leaf_dm(*)          ! (INPUT)  dry matter of each leaf (g)
      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
      REAL       G_slai                ! (INPUT)  area of leaf that senesces from plant
      REAL       G_leaf_area(*)        ! (INPUT)  leaf area of each leaf (mm^2)

      real       dlt_dm_senesced(*)    ! (OUTPUT) actual biomass senesced
                                       ! from plant parts (g/m^2)

*   Global variables
      include   'convert.inc'
      include   'crop3.inc'

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
         leaf_no_senesced = sugar_leaf_no_from_lai 
     :               (
     :                G_leaf_area
     :              , G_plants
     :              , slai_today
     :               )
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
     :                            * c_cabbage_sheath_fr

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
      real function sugar_leaf_no_from_lai 
     :               (
     :                G_leaf_area
     :              , G_plants
     :              , lai
     :               )
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
      REAL       G_leaf_area(*)        ! (INPUT)  leaf area of each leaf (mm^2)
      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)

      real       lai                   ! (INPUT) lai of leaves

*   Global variables
      include   'convert.inc'
      include   'crop3.inc'

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
      subroutine sugar_N_senescence 
     :               (
     :                C_n_cabbage_sen_conc
     :              , C_n_leaf_sen_conc
     :              , C_n_root_sen_conc
     :              , G_dlt_dm_senesced
     :              , dlt_N_senesced
     :               )
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
      REAL       C_n_cabbage_sen_conc  ! (INPUT)  N concentration of senesced cabbage (gN/gdm)
      REAL       C_n_leaf_sen_conc     ! (INPUT)  N concentration of senesced leaf (gN/gdm)
      REAL       C_n_root_sen_conc     ! (INPUT)  N concentration of senesced root (gN/gdm)
      REAL       G_dlt_dm_senesced(*)  ! (INPUT)  plant biomass senescence (g/m^2)

      real       dlt_N_senesced(*)     ! (OUTPUT) actual nitrogen senesced
                                       ! from plant parts (g/m^2)

*   Global variables
      include   'crop3.inc'


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
      subroutine sugar_detachment (Option)
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
*      091294 jngh specified and programmed

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
      integer    Option                ! (INPUT) option number

*   Global variables
      include   'const.inc'
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

      if (Option .eq. 1) then

         call sugar_dm_detachment
     :               (
     :                C_dm_leaf_detach_frac
     :              , C_leaf_cabbage_ratio
     :              , G_dlt_dm_senesced
     :              , G_dm_senesced
     :              , g_dlt_dm_detached
     :               )
         call sugar_slai_detachment
     :               (
     :                C_dm_leaf_detach_frac
     :              , G_slai
     :              , g_dlt_slai_detached
     :               )
         call sugar_N_detachment
     :               (
     :                C_dm_leaf_detach_frac
     :              , G_dlt_n_senesced
     :              , G_n_senesced
     :              , g_dlt_N_detached
     :               )

         call sugar_dm_dead_detachment
     :               (
     :                C_dead_detach_frac
     :              , G_dm_dead
     :              , g_dlt_dm_dead_detached
     :               )
         call sugar_tlai_dead_detachment
     :               (
     :                C_dead_detach_frac
     :              , G_tlai_dead
     :              , g_dlt_tlai_dead_detached
     :               )
         call sugar_N_dead_detachment
     :               (
     :                C_dead_detach_frac
     :              , G_n_dead
     :              , g_dlt_N_dead_detached
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_dm_detachment
     :               (
     :                C_dm_leaf_detach_frac
     :              , C_leaf_cabbage_ratio
     :              , G_dlt_dm_senesced
     :              , G_dm_senesced
     :              , dlt_dm_detached
     :               )
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
      REAL       C_dm_leaf_detach_frac ! (INPUT)  fraction of senesced leaf dry
      REAL       C_leaf_cabbage_ratio  ! (INPUT)  ratio of leaf wt to cabbage wt
      REAL       G_dlt_dm_senesced(*)  ! (INPUT)  plant biomass senescence (g/m^
      REAL       G_dm_senesced(*)      ! (INPUT)  senesced plant dry wt (g/m^2)

      real       dlt_dm_detached(*)    ! (OUTPUT) actual biomass detached
                                       ! from senesced plant parts (g/m^2)

*   Global variables
      include   'crop3.inc'


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
cnh senesesced leaf:cabbage is not the same as green - some cabbage is
c   reallocated to sstem
c      dlt_dm_detached(cabbage) = dlt_dm_detached(leaf)
c     :                         / c_leaf_cabbage_ratio
      dlt_dm_detached(cabbage) = g_dm_senesced(cabbage)
     :                      * c_dm_leaf_detach_frac

      dlt_dm_detached(root) = g_dlt_dm_senesced(root)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_slai_detachment 
     :               (
     :                C_dm_leaf_detach_frac
     :              , G_slai
     :              , dlt_slai_detached
     :               )
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
      REAL       C_dm_leaf_detach_frac ! (INPUT)  fraction of senesced leaf dry matter detaching from live plant each day (0-1)
      REAL       G_slai                ! (INPUT)  area of leaf that senesces from plant

      real       dlt_slai_detached     ! (OUTPUT) lai detached from senesced
                                       ! plant leaf

*   Global variables
      include   'crop3.inc'


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
      subroutine sugar_N_detachment
     :               (
     :                C_dm_leaf_detach_frac
     :              , G_dlt_n_senesced
     :              , G_n_senesced
     :              , dlt_N_detached
     :               )
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
      REAL       C_dm_leaf_detach_frac ! (INPUT)  fraction of senesced leaf dry
      REAL       G_dlt_n_senesced(*)   ! (INPUT)  actual N loss with senesced pl
      REAL       G_n_senesced(*)       ! (INPUT)  plant N content of senesced pl

      real       dlt_N_detached(*)     ! (OUTPUT) actual nitrogen senesced
                                       ! from plant parts (g/m^2)

*   Global variables
      include   'crop3.inc'


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

cnh does not seem correct
c      dlt_N_detached(leaf) = g_dlt_N_senesced(leaf)
c     :                     * c_dm_leaf_detach_frac

      dlt_N_detached(leaf) = g_N_senesced(leaf)
     :                     * c_dm_leaf_detach_frac
      dlt_N_detached(cabbage) = g_N_senesced(cabbage)
     :                     * c_dm_leaf_detach_frac

      dlt_N_detached(root) = g_dlt_N_senesced(root)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_dm_init 
     :               (
     :                C_dm_cabbage_init
     :              , C_dm_leaf_init
     :              , C_dm_sstem_init
     :              , C_dm_sucrose_init
     :              , C_specific_root_length
     :              , G_current_stage
     :              , G_days_tot
     :              , G_dlayer
     :              , G_plants
     :              , G_rlv
     :              , dm_green, dm_plant_min
     :              , leaf_dm
     :               )
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
      REAL       C_dm_cabbage_init     ! (INPUT)  cabbage "    "        "        "
      REAL       C_dm_leaf_init        ! (INPUT)  leaf growth before emergence (g/plant)
      REAL       C_dm_sstem_init       ! (INPUT)  stem growth before emergence (g/plant)
      REAL       C_dm_sucrose_init     ! (INPUT)  sucrose "    "        "        "
      REAL       C_specific_root_length ! (INPUT)  length of root per unit wt (mm/g)
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
      REAL       G_rlv(*)              ! (INPUT)
      real       dm_green(*)           ! (INPUT/OUTPUT) plant part weights
                                       ! (g/m^2)
      real       dm_plant_min(*)       ! (OUTPUT) minimum weight of each
                                       ! plant part (g/plant)
      real       leaf_dm(*)            ! (OUTOUT) leaf wts
      
*   Global variables
      include   'convert.inc'
      include   'crop3.inc'

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
         leaf_dm(1) = c_dm_leaf_init
         dm_green(cabbage) = c_dm_cabbage_init * g_plants
         dm_green(sucrose) = c_dm_sucrose_init * g_plants

cnh     NO MINIMUMS SET AS YET

      else   ! no changes
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_dm_partition
     :               (
     :                C_cane_fraction
     :              , C_leaf_cabbage_ratio
     :              , G_min_sstem_sucrose
     :              , C_ratio_root_shoot
     :              , C_sucrose_delay
     :              , G_current_stage
     :              , G_dm_green
     :              , G_sla_min
     :              , G_sucrose_fraction
     :              , G_tt_tot
     :              , dlt_dm
     :                              , dlt_lai_pot
     :                              , dlt_dm_green
     :                              , partition_xs
     :               )
*     ===========================================================

*   Short description:
*       Partitions assimilate between individual plant pools.  The rules
*       for partitioning change with stage of crop growth.

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
      REAL       C_cane_fraction       ! (INPUT)
      REAL       C_leaf_cabbage_ratio  ! (INPUT)  ratio of leaf wt to cabbage wt
      REAL       G_min_sstem_sucrose   ! (INPUT)
      REAL       C_ratio_root_shoot(*) ! (INPUT)  root:shoot ratio of new dm ()
      REAL       C_sucrose_delay       ! (INPUT)
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
      REAL       G_sla_min             ! (INPUT)  minimum specific leaf area (mm
      REAL       G_sucrose_fraction    ! (INPUT)  fraction of cane C going to su
      REAL       G_tt_tot(*)           ! (INPUT)  the sum of growing degree days
      real       dlt_dm                ! (INPUT) dry matter to partition
      real       dlt_lai_pot           ! (INPUT) increase in lai if
                                       ! unconstrained by carbon supply.
      real       dlt_dm_green (*)      ! (OUTPUT) actual biomass partitioned
                                       ! to plant parts (g/m^2)
      real       partition_xs          ! xs dry matter to that required
                                       ! to supply all demands. (g/m^2)

*   Global variables
      include   'convert.inc'
      include   'crop3.inc'

*   Internal variables
*     none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_dm_partition')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      call sugar_dm_partition_rules
     :               (
     :                C_cane_fraction
     :              , C_leaf_cabbage_ratio
     :              , G_min_sstem_sucrose
     :              , C_ratio_root_shoot
     :              , C_sucrose_delay
     :              , G_current_stage
     :              , G_dm_green
     :              , G_sla_min
     :              , G_sucrose_fraction
     :              , G_tt_tot
     :              , dlt_dm
     :                        ,dlt_lai_pot
     :                        ,dlt_dm_green
     :                        ,partition_xs
     :               )

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_dm_retranslocate
     :               (
     :                dm_retranslocate
     :               )
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
      include   'crop3.inc'

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
      subroutine sugar_N_retranslocate 
     :               (
     :                G_dm_green
     :              , G_n_conc_min
     :              , G_n_green
     :              , dlt_N_retrans
     :               )
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
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass) (g/m^2)
      REAL       G_n_conc_min(*)       ! (INPUT)  minimum N concentration (g N/g biomass)
      REAL       G_n_green(*)          ! (INPUT)  plant nitrogen content (g N/m^2)
      real       dlt_N_retrans (*)     ! (OUTPUT) plant N taken out from
                                       ! plant parts (g N/m^2)

*   Global variables
      include   'crop3.inc'

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

      call sugar_N_retrans_avail 
     :               (
     :                G_dm_green
     :              , G_n_conc_min
     :              , G_n_green
     :              , N_avail
     :               )  ! grain N potential (supply)

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
      subroutine sugar_N_retrans_avail
     :               (
     :                G_dm_green
     :              , G_n_conc_min
     :              , G_n_green
     :              , N_avail
     :               )
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
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
      REAL       G_n_conc_min(*)       ! (INPUT)  minimum N concentration (g N/g
      REAL       G_n_green(*)          ! (INPUT)  plant nitrogen content (g N/m^
      real       N_avail (*)           ! (OUTPUT) total N available for
                                       ! transfer to grain (g/m^2)

*   Global variables
      include   'crop3.inc'

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
      subroutine sugar_N_demand
     :               (
     :                G_dlt_dm_green_pot
     :              , G_dlt_dm_pot_rue_pot
     :              , G_dm_green
     :              , G_n_conc_crit
     :              , G_n_green
     :              , N_demand
     :               )
*     ===========================================================

*   Short description:
*       Return plant nitrogen demand for each plant component.  The
*       demand for Nitrogen for each plant pool occurs as the plant
*       tries to maintain a critical nitrogen concentration in each
*       plant pool.

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
      REAL       G_dlt_dm_green_pot(*) ! (INPUT)  plant biomass growth (g/m^2)
      REAL       G_dlt_dm_pot_rue_pot  ! (INPUT)
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
      REAL       G_n_conc_crit(*)      ! (INPUT)  critical N concentration (g N/
      REAL       G_n_green(*)          ! (INPUT)  plant nitrogen content (g N/m^
      real       N_demand (*)          ! (OUTPUT) plant nitrogen demand
                                       ! (g/m^2)

*   Global variables
      include   'crop3.inc'

cnh      real       bound                 ! function
cnh      real       divide                ! function
      real       l_bound               ! function
      real       sum_real_array        ! function

*   Internal variables
c      integer    current_phase         ! current phase number
      real       N_crit                ! critical N amount (g/m^2)
      real       N_demand_new          ! demand for N by new growth
                                       ! (g/m^2)
      real       N_demand_old          ! demand for N by old biomass
                                       ! (g/m^2)
      integer    part                  ! plant part

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_N_demand')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

         ! calculate potential new shoot and root growth

c      current_phase = int (g_current_stage)

            ! need to calculate dm using potential rue not affected by
            ! N and temperature

cnh      do 500 part = 1, max_part
cnh         part_fract = divide (g_dlt_dm_green(part), g_dlt_dm, 0.0)
cnh         dlt_dm_pot(part) = dlt_dm_pot_radn * part_fract
cnh         dlt_dm_pot(part) = bound (dlt_dm_pot(part)
cnh     :                           , 0.0, dlt_dm_pot_radn)
cnh500   continue

            ! recalculate roots because today's drymatter production
            ! does not include roots

C      dlt_dm_pot(root) = g_dlt_dm_pot_rue_pot
C     :                 * c_ratio_root_shoot(current_phase)


         ! g_dlt_dm_pot is above ground biomass only so leave roots
         ! out of comparison

      call bound_check_real_var (
     :             sum_real_array (G_dlt_dm_green_pot, max_part)
     :           - g_dlt_dm_green_pot(root)
     :           , 0.0, g_dlt_dm_pot_rue_pot
     :           , 'dlt_dm_pot - dlt_dm_pot(root)')


      ! NIH - note stem stuff is redone down later.

      do 1000 part = 1, max_part
         if (g_dm_green(part).gt.0.0) then

               ! get N demands due to difference between actual N concentrations
               ! and critical N concentrations of tops (stover) and roots.

            N_crit = g_dm_green(part) * g_N_conc_crit(part)
            N_demand_old = N_crit - g_N_green(part)


               ! get potential N demand (critical N) of potential growth

            N_demand_new = g_dlt_dm_green_pot(part)
     :                   * g_N_conc_crit(part)

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
      N_demand_new = (g_dlt_dm_green_pot(sstem)
     :                + g_dlt_dm_green_pot(sucrose))
     :             * g_N_conc_crit(sstem)
      N_demand(sstem) = N_demand_old + N_demand_new
      N_demand(sstem) = l_bound (N_demand(sstem), 0.0)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_N_uptake
     :               (
     :                C_no3_diffn_const
     :              , G_dlayer
     :              , G_no3gsm_diffn_pot
     :              , G_no3gsm_mflow_avail
     :              , G_num_uptake_no3
     :              , G_n_demand
     :              , G_root_depth
     :              , G_uptake_no3
     :              , G_uptake_source
     :              , dlt_NO3gsm
     :               )
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
      include   'crop3.inc'

      real       bound                 ! function
      real       divide                ! function
      integer    find_layer_no         ! function
      real       l_bound               ! function
      real       sum_real_array        ! function

*   Subroutine arguments
      REAL       C_no3_diffn_const     ! (INPUT)  time constant for uptake by di
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL       G_no3gsm_diffn_pot(*) ! (INPUT)  potential NO3 (supply) from so
      REAL       G_no3gsm_mflow_avail(*) ! (INPUT)  potential NO3 (supply) from
      INTEGER    G_num_uptake_no3      ! (INPUT)  number of layers in g_uptake_n
      REAL       G_n_demand(*)         ! (INPUT)  plant nitrogen demand (g/m^2)
      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
      REAL       G_uptake_no3(*)       ! (INPUT)  uptake of no3 as provided by a
      CHARACTER  G_uptake_source   *(*) ! (INPUT)
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
      real       diffn_fract           ! fraction of nitrogen to use (0-1)
                                       ! for diffusion
      real       mflow_fract           ! fraction of nitrogen to use (0-1)
                                       ! for mass flow
      integer    layer                 ! soil layer number of profile
      real       N_demand              ! total nitrogen demand (g/m^2)
      real       NO3gsm_uptake         ! plant NO3 uptake from layer (g/m^2)

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_N_uptake')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      call fill_real_array (dlt_NO3gsm, 0.0, max_layer)
      deepest_layer = find_layer_no(g_root_depth, g_dlayer, max_layer)

      N_demand = sum_real_array (g_N_demand, max_part)

      if (g_uptake_source.eq.'calc') then
            ! find potential N uptake (supply, available N)
            ! Get it for nitrate by diffusion and mass flow
            ! Note: the N available by diffusion is really the total N
            ! available to the roots by mass flow and diffusion.

cjh         call sugar_N_mass_flow (NO3gsm_mflow_avail)
cjh         call sugar_N_diffusion (NO3gsm_diffn_avail)

         do 1000 layer = 1, deepest_layer
            NO3gsm_diffn_avail(layer) = g_NO3gsm_diffn_pot(layer)
     :                                - g_NO3gsm_mflow_avail(layer)
            NO3gsm_diffn_avail(layer) =
     :                          l_bound (NO3gsm_diffn_avail(layer)
     :                         ,0.0)
1000     continue

            ! get potential N uptake (supply) from the root profile.
            ! get totals for diffusion and mass flow.

         NO3gsm_mflow_supply = sum_real_array (g_NO3gsm_mflow_avail
     :                                     , deepest_layer)
         NO3gsm_diffn_supply = sum_real_array (NO3gsm_diffn_avail
     :                                     , deepest_layer)

            ! get actual total nitrogen uptake for diffusion and mass flow.
            ! If demand is not satisfied by mass flow, then use diffusion.

         if (NO3gsm_mflow_supply.ge.N_demand) then
cnh comment out next line for luxury uptake.
            NO3gsm_mflow = N_demand
cnh            NO3gsm_mflow = NO3gsm_mflow_supply
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

            mflow_fract = divide (g_NO3gsm_mflow_avail(layer)
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

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_N_mass_flow 
     :               (
     :                G_dlayer
     :              , G_dlt_sw_dep
     :              , G_no3gsm
     :              , G_no3gsm_min
     :              , G_root_depth
     :              , G_sw_dep
     :              , NO3gsm_mflow_pot
     :               )
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
      include   'crop3.inc'

      real       divide                ! function
      integer    find_layer_no         ! function
      real       u_bound               ! function

*   Subroutine arguments
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL       G_dlt_sw_dep(*)       ! (INPUT)  water uptake in each layer (mm water)
      REAL       G_no3gsm(*)           ! (INPUT)  nitrate nitrogen in layer L (g N/m^2)
      REAL       G_no3gsm_min(*)       ! (INPUT)  minimum allowable NO3 in soil (g/m^2)
      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
      REAL       G_sw_dep(*)           ! (INPUT)  soil water content of layer L (mm)
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
      subroutine sugar_N_diffusion 
     :               (
     :                G_dlayer
     :              , G_no3gsm
     :              , G_no3gsm_min
     :              , G_root_depth
     :              , G_sw_avail
     :              , G_sw_avail_pot
     :              , NO3gsm_diffn_pot
     :               )
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
      include   'crop3.inc'

      real       bound                 ! function
      real       divide                ! function
      integer    find_layer_no         ! function
      real       u_bound               ! function

*   Subroutine arguments
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL       G_no3gsm(*)           ! (INPUT)  nitrate nitrogen in layer L (g N/m^2)
      REAL       G_no3gsm_min(*)       ! (INPUT)  minimum allowable NO3 in soil (g/m^2)
      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
      REAL       G_sw_avail(*)         ! (INPUT)  actual extractable soil water (mm)
      REAL       G_sw_avail_pot(*)     ! (INPUT)  potential extractable soil water (mm)
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
      subroutine sugar_N_conc_limits
     :               (
     :                C_n_conc_crit_root
     :              , C_n_conc_min_root
     :              , C_x_stage_code
     :              , C_y_n_conc_crit_cabbage
     :              , C_y_n_conc_crit_cane
     :              , C_y_n_conc_crit_leaf
     :              , C_y_n_conc_min_cabbage
     :              , C_y_n_conc_min_cane
     :              , C_y_n_conc_min_leaf
     :              , G_current_stage
     :              , C_stage_code_list
     :              , G_phase_tt
     :              , G_tt_tot
     :              , N_conc_crit, N_conc_min
     :               )
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
      REAL       C_n_conc_crit_root    ! (INPUT)  critical N concentration of ro
      REAL       C_n_conc_min_root     ! (INPUT)  minimum N concentration of roo
      REAL       C_x_stage_code(*)     ! (INPUT)  stage table for N concentratio
      REAL       C_y_n_conc_crit_cabbage(*) ! (INPUT)  critical N concentration
      REAL       C_y_n_conc_crit_cane(*) ! (INPUT)  critical N concentration of
      REAL       C_y_n_conc_crit_leaf(*) ! (INPUT)  critical N concentration of
      REAL       C_y_n_conc_min_cabbage(*) ! (INPUT)  minimum N concentration of
      REAL       C_y_n_conc_min_cane(*) ! (INPUT)  minimum N concentration of fl
      REAL       C_y_n_conc_min_leaf(*) ! (INPUT)  minimum N concentration of le
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       C_stage_code_list(*)  ! (INPUT)  list of stage numbers
      REAL       G_phase_tt(*)         ! (INPUT)  Cumulative growing degree days
      REAL       G_tt_tot(*)           ! (INPUT)  the sum of growing degree days
      real       N_conc_crit(*)        ! (OUTPUT) critical N concentration
                                       ! (g N/g part)
      real       N_conc_min(*)         ! (OUTPUT) minimum N concentration
                                       ! (g N/g part)

*   Global variables
      include   'crop3.inc'

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
         stage_code = sugar_stage_code
     :               (
     :                C_stage_code_list
     :              , G_phase_tt
     :              , G_tt_tot
     :              , g_current_stage, c_x_stage_code
     :                                , numvals
     :               )
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
      subroutine sugar_update
     :               (
     :                G_canopy_height
     :              , G_cnd_photo
     :              , G_cswd_expansion
     :              , G_cswd_pheno
     :              , G_cswd_photo
     :              , G_dlt_canopy_height
     :              , G_dlt_dm
     :              , G_dlt_dm_dead_detached
     :              , G_dlt_dm_detached
     :              , G_dlt_dm_green
     :              , G_dlt_dm_green_retrans
     :              , G_dlt_dm_senesced
     :              , G_dlt_dm_realloc
     :              , G_dlt_lai
     :              , G_dlt_leaf_no
     :              , G_dlt_leaf_no_dead
     :              , G_dlt_n_dead_detached
     :              , G_dlt_n_detached
     :              , G_dlt_n_green
     :              , G_dlt_n_retrans
     :              , G_dlt_n_senesced
     :              , G_dlt_n_realloc
     :              , G_dlt_plants
     :              , G_dlt_plant_wc
     :              , G_dlt_rlv
     :              , G_dlt_rlv_senesced
     :              , G_dlt_root_depth
     :              , G_dlt_slai
     :              , G_dlt_slai_detached
     :              , G_dlt_stage
     :              , G_dlt_tlai_dead_detached
     :              , G_dm_dead
     :              , G_dm_green
     :              , G_dm_plant_top_tot
     :              , G_dm_senesced
     :              , G_lai
     :              , G_leaf_area
     :              , G_leaf_dm
     :              , G_leaf_no
     :              , G_leaf_no_dead
     :              , G_nfact_photo
     :              , G_n_conc_crit
     :              , G_n_conc_min
     :              , G_n_dead
     :              , G_n_green
     :              , G_n_senesced
     :              , G_plants
     :              , G_plant_wc
     :              , G_previous_stage
     :              , G_rlv
     :              , G_root_depth
     :              , G_slai
     :              , G_swdef_expansion
     :              , G_swdef_pheno
     :              , G_swdef_photo
     :              , G_tlai_dead
     :              , C_n_conc_crit_root
     :              , C_n_conc_min_root
     :              , C_x_stage_code
     :              , C_y_n_conc_crit_cabbage
     :              , C_y_n_conc_crit_cane
     :              , C_y_n_conc_crit_leaf
     :              , C_y_n_conc_min_cabbage
     :              , C_y_n_conc_min_cane
     :              , C_y_n_conc_min_leaf
     :              , G_current_stage
     :              , C_stage_code_list
     :              , G_phase_tt
     :              , G_tt_tot
     :              , G_leaf_no_detached
     :              , C_leaf_no_at_emerg
     :               )
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
*      030996 nih added detachment accounting

*   Calls:
*     accumulate
*     add_real_array
*     divide
*     pop_routine
*     push_routine
*     sugar_top_residue
*     sugar_root_incorp
*     sugar_N_conc_limits
*     sugar_root_distrib
*     sugar_swdef
*     sum_between
*     sum_real_array

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      REAL       G_canopy_height       ! (INPUT)  canopy height (mm)
      REAL       G_cnd_photo(*)        ! (INPUT)  cumulative nitrogen stress typ
      REAL       G_cswd_expansion(*)   ! (INPUT)  cumulative water stress type 2
      REAL       G_cswd_pheno(*)       ! (INPUT)  cumulative water stress type 3
      REAL       G_cswd_photo(*)       ! (INPUT)  cumulative water stress type 1
      REAL       G_dlt_canopy_height   ! (INPUT)  change in canopy height (mm)
      REAL       G_dlt_dm              ! (INPUT)  the daily biomass production (
      REAL       G_dlt_dm_dead_detached(*) ! (INPUT)  plant biomass detached fro
      REAL       G_dlt_dm_detached(*)  ! (INPUT)  plant biomass detached (g/m^2)
      REAL       G_dlt_dm_green(*)     ! (INPUT)  plant biomass growth (g/m^2)
      REAL       G_dlt_dm_green_retrans(*) ! (INPUT)  plant biomass retranslocat
      REAL       G_dlt_dm_senesced(*)  ! (INPUT)  plant biomass senescence (g/m^
      REAL       G_dlt_dm_realloc(*)   ! (INPUT)  
      REAL       G_dlt_lai             ! (INPUT)  actual change in live plant la
      REAL       G_dlt_leaf_no         ! (INPUT)  fraction of oldest leaf expand
      REAL       G_dlt_leaf_no_dead    ! (INPUT)  fraction of oldest green leaf
      REAL       G_dlt_n_dead_detached(*) ! (INPUT)  actual N loss with detached
      REAL       G_dlt_n_detached(*)   ! (INPUT)  actual N loss with detached pl
      REAL       G_dlt_n_green(*)      ! (INPUT)  actual N uptake into plant (g/
      REAL       G_dlt_n_retrans(*)    ! (INPUT)  nitrogen retranslocated out fr
      REAL       G_dlt_n_senesced(*)   ! (INPUT)  actual N loss with senesced pl
      REAL       G_dlt_n_realloc(*)   ! (INPUT)  
      REAL       G_dlt_plants          ! (INPUT)  change in Plant density (plant
      REAL       G_dlt_plant_wc(*)     ! (INPUT)
      REAL       G_dlt_rlv(*)          ! (INPUT)
      REAL       G_dlt_rlv_senesced(*) ! (INPUT)
      REAL       G_dlt_root_depth      ! (INPUT)  increase in root depth (mm)
      REAL       G_dlt_slai            ! (INPUT)  area of leaf that senesces fro
      REAL       G_dlt_slai_detached   ! (INPUT)  plant senesced lai detached
      REAL       G_dlt_stage           ! (INPUT)  change in stage number
      REAL       G_dlt_tlai_dead_detached ! (INPUT)  plant lai detached from dea
      REAL       G_dm_dead(*)          ! (INPUT)  dry wt of dead plants (g/m^2)
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
      REAL       G_dm_plant_top_tot(*) ! (INPUT)  total carbohydrate production
      REAL       G_dm_senesced(*)      ! (INPUT)  senesced plant dry wt (g/m^2)
      REAL       G_lai                 ! (INPUT)  live plant green lai
      REAL       G_leaf_area(*)        ! (INPUT)  leaf area of each leaf (mm^2)
      REAL       G_leaf_dm(*)          ! (INPUT)  dry matter of each leaf (g)
      REAL       G_leaf_no(*)          ! (INPUT)  number of fully expanded leave
      REAL       G_leaf_no_dead(*)     ! (INPUT)  no of dead leaves ()
      REAL       G_nfact_photo         ! (INPUT)
      REAL       G_n_conc_crit(*)      ! (INPUT)  critical N concentration (g N/
      REAL       G_n_conc_min(*)       ! (INPUT)  minimum N concentration (g N/g
      REAL       G_n_dead(*)           ! (INPUT)  plant N content of dead plants
      REAL       G_n_green(*)          ! (INPUT)  plant nitrogen content (g N/m^
      REAL       G_n_senesced(*)       ! (INPUT)  plant N content of senesced pl
      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
      REAL       G_plant_wc(*)         ! (INPUT)
      REAL       G_previous_stage      ! (INPUT)  previous phenological stage
      REAL       G_rlv(*)              ! (INPUT)
      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
      REAL       G_slai                ! (INPUT)  area of leaf that senesces fro
      REAL       G_swdef_expansion     ! (INPUT)
      REAL       G_swdef_pheno         ! (INPUT)
      REAL       G_swdef_photo         ! (INPUT)
      REAL       G_tlai_dead           ! (INPUT)  total lai of dead plants
      REAL       C_n_conc_crit_root    ! (INPUT)  critical N concentration of ro
      REAL       C_n_conc_min_root     ! (INPUT)  minimum N concentration of roo
      REAL       C_x_stage_code(*)     ! (INPUT)  stage table for N concentratio
      REAL       C_y_n_conc_crit_cabbage(*) ! (INPUT)  critical N concentration
      REAL       C_y_n_conc_crit_cane(*) ! (INPUT)  critical N concentration of
      REAL       C_y_n_conc_crit_leaf(*) ! (INPUT)  critical N concentration of
      REAL       C_y_n_conc_min_cabbage(*) ! (INPUT)  minimum N concentration of
      REAL       C_y_n_conc_min_cane(*) ! (INPUT)  minimum N concentration of fl
      REAL       C_y_n_conc_min_leaf(*) ! (INPUT)  minimum N concentration of le
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       C_stage_code_list(*)  ! (INPUT)  list of stage numbers
      REAL       G_phase_tt(*)         ! (INPUT)  Cumulative growing degree days
      REAL       G_tt_tot(*)           ! (INPUT)  the sum of growing degree days
      REAL       G_leaf_no_detached    ! (INPUT)  number of detached leaves
      REAL       C_leaf_no_at_emerg    ! (INPUT)  number of leaves at emergence

*       none

*   Global variables
      include   'convert.inc'
      include   'crop3.inc'

      integer    count_of_Real_vals    ! function
      real       divide                ! function
      real       sum_between           ! function

*   Internal variables
      real       dlt_dm_plant          ! dry matter increase (g/plant)
      real       dlt_leaf_area         ! leaf area increase (mm^2/plant)
      real       dlt_leaf_dm           !
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
      real       dying_fract           ! fraction op population dying (0-1)
      real       leaf_no               ! currently expanding leaf no.
      integer    part                  ! plant part index
      integer    num_leaves            ! number of leaves on plant
      integer    empty_leaves          ! number of empty leaf records
      integer    leaf_rec              ! leaf record number
      integer    leaf_rec_new          ! new leaf record number

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

cnh
      ! take out water with detached stems
      g_plant_wc(sstem) = g_plant_wc(sstem) 
     :                  * (1.0 - divide (g_dlt_dm_dead_detached(sstem)
     :                                  ,g_dm_dead(sstem)
     :                                   +g_dm_green(sstem)
     :                                  ,0.0)
     :                    )
      call add_real_array (g_dlt_plant_wc, g_plant_wc, max_part)

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
      call add_real_array (g_dlt_N_realloc, g_N_green, max_part)
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
      call add_real_array (g_dlt_dm_realloc, g_dm_green, max_part)
      call subtract_real_array (g_dlt_dm_senesced, g_dm_green
     :                        , max_part)

      call add_real_array (g_dlt_dm_senesced, g_dm_senesced
     :                   , max_part)
      call subtract_real_array (g_dlt_dm_detached, g_dm_senesced
     :                        , max_part)


c      dm_residue = (sum_real_array (g_dlt_dm_detached, max_part)
c     :           - g_dlt_dm_detached(root))
c      N_residue = (sum_real_array (g_dlt_N_detached, max_part)
c     :          - g_dlt_N_detached(root))
c
c      call sugar_top_residue (dm_residue, N_residue)

c             ! put roots into root residue

c      call sugar_root_incorp (g_dlt_dm_detached(root)
c     :                    , g_dlt_N_detached(root))


      call sugar_update_other_variables ()

         ! transfer plant leaf area
      dlt_lai_dead  = g_lai  * dying_fract
      dlt_slai_dead = g_slai * dying_fract

      g_lai = g_lai + g_dlt_lai - dlt_lai_dead - g_dlt_slai
      g_slai = g_slai + g_dlt_slai - dlt_slai_dead - g_dlt_slai_detached
      g_tlai_dead = g_tlai_dead + dlt_lai_dead + dlt_slai_dead
     :            - g_dlt_tlai_dead_detached


         ! plant leaf development
         ! need to account for truncation of partially developed leaf (add 1)
c      leaf_no = 1.0 + sum_between (emerg, now, g_leaf_no)
         ! need to add leaf at emergence because we now remove records of detach
         ! and so whereever detached leaves are used we need to account for the
         ! are set at emergence as these offset the records.
         ! THIS NEEDS CHANGING!!!!
      leaf_no = 1.0 + sum_between (emerg, now, g_leaf_no)
     :              - g_leaf_no_detached
     :              - c_leaf_no_at_emerg

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


      ! detached leaf area needs to be accounted for

      dlt_leaf_area = divide (g_dlt_slai_detached, g_plants, 0.0)
     :              * sm2smm
      num_leaves = count_of_Real_vals(g_leaf_area,max_leaf)
      num_leaves = max_leaf
      dlt_leaf_dm = g_dlt_dm_detached(leaf)/g_plants

      empty_leaves = -1
      do 111 leaf_rec = 1,num_leaves
        if (g_leaf_area(leaf_rec).le.dlt_leaf_area) then
           dlt_leaf_area = dlt_leaf_area - g_leaf_area(leaf_rec)
           g_leaf_area(leaf_rec) = 0.0
        else
           g_leaf_area(leaf_rec) = g_leaf_area(leaf_rec) - dlt_leaf_area
           dlt_leaf_area = 0.0
        endif
        if (g_leaf_dm(leaf_rec).le.dlt_leaf_dm) then
           dlt_leaf_dm = dlt_leaf_dm - g_leaf_dm(leaf_rec)
           g_leaf_dm(leaf_rec) = 0.0
        else
           g_leaf_dm(leaf_rec) = g_leaf_dm(leaf_rec) - dlt_leaf_dm
           dlt_leaf_dm = 0.0
        endif
        if ((g_leaf_dm(leaf_rec).gt.0.0).and.(empty_leaves.eq.-1)) then
           empty_leaves = leaf_rec - 1
        else
        endif
  111 continue

      if (empty_leaves.gt.0) then
         g_leaf_no_detached = g_leaf_no_detached + empty_leaves
         !kludgy solution for now
         do 112 leaf_rec=empty_leaves+1, num_leaves
            leaf_rec_new = leaf_rec - empty_leaves
            g_leaf_dm(leaf_rec_new)=g_leaf_dm(leaf_rec)
            g_leaf_area(leaf_rec_new)=g_leaf_area(leaf_rec)
            g_leaf_dm(leaf_rec) = 0.0
            g_leaf_area(leaf_rec) = 0.0
  112    continue
      else
      endif


         ! plant stress

      call accumulate (1.0 - g_swdef_photo, g_cswd_photo
     :               , g_previous_stage, g_dlt_stage)
      call accumulate (1.0 - g_swdef_expansion, g_cswd_expansion
     :               , g_previous_stage, g_dlt_stage)
      call accumulate (1.0 - g_swdef_pheno, g_cswd_pheno
     :               , g_previous_stage, g_dlt_stage)

      call accumulate (1.0 - g_nfact_photo, g_cnd_photo
     :               , g_previous_stage, g_dlt_stage)

         ! other plant states

      g_canopy_height = g_canopy_height + g_dlt_canopy_height
      g_plants = g_plants + g_dlt_plants
      g_root_depth = g_root_depth + g_dlt_root_depth
      call add_real_array      (g_dlt_rlv, g_rlv, max_layer)
      call subtract_real_array (g_dlt_rlv_senesced, g_rlv, max_layer)

      call sugar_N_conc_limits
     :               (
     :                C_n_conc_crit_root
     :              , C_n_conc_min_root
     :              , C_x_stage_code
     :              , C_y_n_conc_crit_cabbage
     :              , C_y_n_conc_crit_cane
     :              , C_y_n_conc_crit_leaf
     :              , C_y_n_conc_min_cabbage
     :              , C_y_n_conc_min_cane
     :              , C_y_n_conc_min_leaf
     :              , G_current_stage
     :              , C_stage_code_list
     :              , G_phase_tt
     :              , G_tt_tot
     :              , g_N_conc_crit, g_N_conc_min
     :               )  ! plant N concentr

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_plant_death (Option)
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
*       290994 jngh specified and programmed

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
      integer    Option                ! (INPUT) option number

*   Global variables
      include   'const.inc'
      include   'sugar.inc'

      logical    reals_are_equal       ! function

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_plant_death')

*   Initial data values
*       none


* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      if (Option .eq. 1) then

         call sugar_failure_germination
     :               (
     :                C_days_germ_limit
     :              , G_current_stage
     :              , G_days_tot
     :              , G_plants
     :              , g_dlt_plants_failure_germ
     :               )

         call sugar_failure_emergence
     :               (
     :                C_tt_emerg_limit
     :              , G_current_stage
     :              , G_plants
     :              , G_tt_tot
     :              , g_dlt_plants_failure_emergence
     :               )

         call sugar_failure_leaf_sen
     :               (
     :                G_current_stage
     :              , G_lai
     :              , G_plants
     :              , g_dlt_plants_failure_leaf_sen
     :               )

         call sugar_death_drought
     :               (
     :                C_leaf_no_crit
     :              , C_swdf_photo_limit
     :              , C_swdf_photo_rate
     :              , G_cswd_photo
     :              , G_leaf_no
     :              , G_plants
     :              , G_swdef_photo
     :              , g_dlt_plants_death_drought
     :               )

         call sugar_death_lodging
     :               (
     :                g_lodge_flag
     :              , G_swdef_photo
     :              , G_water_log_fact
     :              , c_stress_lodge
     :              , c_death_fr_lodge
     :              , c_num_stress_lodge
     :              , G_plants
     :              , g_dlt_plants_death_lodging
     :               )

c         call sugar_death_external_action (g_dlt_plants_death_external)
         call sugar_death_actual
     :               (
     :                G_dlt_plants_death_drought
     :              , G_dlt_plants_failure_emergence
     :              , G_dlt_plants_failure_germ
     :              , G_dlt_plants_failure_leaf_sen
     :              , G_dlt_plants_death_lodging
     :              , g_dlt_plants
     :               )
         if (reals_are_equal (g_dlt_plants + g_plants, 0.0)) then
            call sugar_kill_crop
     :               (
     :                G_crop_status
     :              , G_day_of_year
     :              , G_dm_dead
     :              , G_dm_green
     :              , G_dm_senesced
     :              , G_year
     :               )
         else
         endif

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_dm_dead_detachment
     :               (
     :                C_dead_detach_frac
     :              , G_dm_dead
     :              , dlt_dm_dead_detached
     :               )
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
      REAL       C_dead_detach_frac(*) ! (INPUT)  fraction of dead plant parts d
      REAL       G_dm_dead(*)          ! (INPUT)  dry wt of dead plants (g/m^2)
      real       dlt_dm_dead_detached(*)   ! (OUTPUT) change in dm of dead
                                           ! plants (g/m^2)

*   Global variables
      include   'crop3.inc'

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
      subroutine sugar_tlai_dead_detachment 
     :               (
     :                C_dead_detach_frac
     :              , G_tlai_dead
     :              , dlt_tlai_dead_detached
     :               )
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
      REAL       C_dead_detach_frac(*) ! (INPUT)  fraction of dead plant parts detaching each day (0-1)
      REAL       G_tlai_dead           ! (INPUT)  total lai of dead plants
      real       dlt_tlai_dead_detached   ! (OUTPUT) change in lai of dead
                                          ! plants

*   Global variables
      include   'crop3.inc'

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
      subroutine sugar_N_dead_detachment 
     :               (
     :                C_dead_detach_frac
     :              , G_n_dead
     :              , dlt_N_dead_detached
     :               )
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
      REAL       C_dead_detach_frac(*) ! (INPUT)  fraction of dead plant parts detaching each day (0-1)
      REAL       G_n_dead(*)           ! (INPUT)  plant N content of dead plants (g N/m^2)
      real       dlt_N_dead_detached(*)   ! (OUTPUT) change in dm of dead
                                          ! plants (g/m^2)

*   Global variables
      include   'crop3.inc'

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
      subroutine sugar_event
     :               (
     :                C_stage_code_list
     :              , C_stage_names
     :              , G_current_stage
     :              , G_days_tot
     :              , G_day_of_year
     :              , G_dlayer
     :              , G_dm_dead
     :              , G_dm_green
     :              , G_dm_senesced
     :              , G_lai
     :              , G_n_green
     :              , G_root_depth
     :              , G_sw_dep
     :              , G_year
     :              , P_ll_dep
     :               )
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
      REAL       C_stage_code_list(*)  ! (INPUT)  list of stage numbers
      CHARACTER  C_stage_names(*)*(*)  ! (INPUT)  full names of stages for repor
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
      INTEGER    G_day_of_year         ! (INPUT)  day of year
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL       G_dm_dead(*)          ! (INPUT)  dry wt of dead plants (g/m^2)
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
      REAL       G_dm_senesced(*)      ! (INPUT)  senesced plant dry wt (g/m^2)
      REAL       G_lai                 ! (INPUT)  live plant green lai
      REAL       G_n_green(*)          ! (INPUT)  plant nitrogen content (g N/m^
      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
      REAL       G_sw_dep(*)           ! (INPUT)  soil water content of layer L
      INTEGER    G_year                ! (INPUT)  year
      REAL       P_ll_dep(*)           ! (INPUT)  lower limit of plant-extractab
*       none

*   Global variables
      include   'const.inc'            ! new_line, lu_scr_sum, blank,
                                       ! lu_scr_sum
      include   'convert.inc'
      include   'crop3.inc'

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
      subroutine sugar_N_init
     :               (
     :                C_n_cabbage_init_conc
     :              , C_n_leaf_init_conc
     :              , C_n_root_init_conc
     :              , C_n_sstem_init_conc
     :              , G_current_stage
     :              , G_days_tot
     :              , G_dm_green
     :              , N_green
     :               )
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
      REAL       C_n_cabbage_init_conc ! (INPUT)     "   cabbage    "
      REAL       C_n_leaf_init_conc    ! (INPUT)  initial leaf N concentration (
      REAL       C_n_root_init_conc    ! (INPUT)  initial root N concentration (
      REAL       C_n_sstem_init_conc   ! (INPUT)  initial stem N concentration (
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
      real       N_green(*)            ! plant nitrogen (g/m^2)

*   Global variables
      include   'crop3.inc'

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
         if (N_green(root).eq.0.0) then
            ! There is no root system currently operating from
            ! a previous crop
            N_green(root) = c_N_root_init_conc*g_dm_green(root)
         else
            ! There IS a root system currently operating from
            ! a previous crop
         endif
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
      subroutine sugar_check_sw 
     :               (
     :                C_minsw
     :              , G_dlayer
     :              , G_dul_dep
     :              , G_num_layers
     :              , G_sw_dep
     :              , P_ll_dep
     :               )
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
      REAL       C_minsw               ! (INPUT)  lowest acceptable value for ll
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL       G_dul_dep(*)          ! (INPUT)  drained upper limit soil water content for soil layer L (mm water)
      INTEGER    G_num_layers          ! (INPUT)  number of layers in profile ()
      REAL       G_sw_dep(*)           ! (INPUT)  soil water content of layer L (mm)
      REAL       P_ll_dep(*)           ! (INPUT)  lower limit of plant-extractable soil water for soil layer L (mm)
*       none

*   Global variables
      include   'const.inc'            ! err_internal
      include   'crop3.inc'

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
      subroutine sugar_root_distrib 
     :               (
     :                G_dlayer
     :              , G_rlv
     :              , G_root_depth
     :              , root_array, root_sum
     :               )
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
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL       G_rlv(*)              ! (INPUT)
      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
      real       root_array(*)         ! (OUTPUT) array to contain
                                       ! distributed material
      real       root_sum              ! (INPUT) Material to be distributed

*   Global variables
      include   'crop3.inc'

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

         call sugar_root_distrib
     :               (
     :                G_dlayer
     :              , G_rlv
     :              , G_root_depth
     :              , dlt_dm_incorp
     :              , dlt_dm_root * gm2kg /sm2ha
     :               )
         call bound_check_real_array (dlt_dm_incorp
     :                        , 0.0
     :                        , dlt_dm_root * gm2kg/sm2ha
     :                        , 'dlt_dm_incorp'
     :                        , max_layer)

         call sugar_root_distrib
     :               (
     :                G_dlayer
     :              , G_rlv
     :              , G_root_depth
     :              , dlt_N_incorp
     :              , dlt_N_root * gm2kg /sm2ha
     :               )
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
*     ===========================================================
      real function sugar_sprouting
     :               (
     :                C_pesw_germ
     :              , G_current_stage
     :              , G_days_tot
     :              , G_dlayer
     :              , G_sowing_depth
     :              , G_sw_dep
     :              , P_ll_dep
     :              , current_stage
     :               )
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
*     300996 nih added fix to template phenology error

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
      REAL       C_pesw_germ           ! (INPUT)  plant extractable soil water i
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL       G_sowing_depth        ! (INPUT)  sowing depth (mm)
      REAL       G_sw_dep(*)           ! (INPUT)  soil water content of layer L
      REAL       P_ll_dep(*)           ! (INPUT)  lower limit of plant-extractab
      real       current_stage         ! (OUTPUT) phenological stage number

*   Global variables
      include   'crop3.inc'

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
cnh            sugar_sprouting = 0.0001
               sugar_sprouting = 0.999
         endif
      else
             ! no sowing yet
         sugar_sprouting = 0.0
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_sla_min 
     :               (
     :                C_num_sla_lfno
     :              , C_sla_lfno
     :              , C_sla_min
     :              , leaf_no, sla_min
     :               )
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
      INTEGER    C_num_sla_lfno        ! (INPUT)
      REAL       C_sla_lfno(*)         ! (INPUT)
      REAL       C_sla_min(*)          ! (INPUT)  minimum specific leaf area for new leaf area (mm^2/g)
      real       leaf_no               ! (INPUT) nominated leaf number
      real       sla_min               ! (OUTPUT)

*   Global variables
      include   'crop3.inc'

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

      sla_min = linear_interp_real
     :                     (real(leaf_no)
     :                     ,c_sla_lfno
     :                     ,c_sla_min
     :                     ,c_num_sla_lfno
     :                     )

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      real function sugar_sla_max 
     :               (
     :                C_num_sla_lfno
     :              , C_sla_lfno
     :              , C_sla_max
     :              , leaf_no
     :               )
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
      INTEGER    C_num_sla_lfno        ! (INPUT)
      REAL       C_sla_lfno(*)         ! (INPUT)
      REAL       C_sla_max(*)          ! (INPUT)  maximum specific leaf area for new leaf area (mm^2/g)
      real       leaf_no               ! (INPUT) nominated leaf number

*   Global variables
      include   'crop3.inc'

      real       linear_interp_real    ! function

*   Internal variables
*     none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_sla_max')

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
* ====================================================================
       subroutine sugar_root_length_growth 
     :               (
     :                C_specific_root_length
     :              , G_dlayer
     :              , G_dlt_dm_green
     :              , G_dlt_rlv
     :              , G_dlt_root_depth
     :              , G_root_depth
     :              , P_xf
     :              , C_num_sw_ratio
     :              , C_x_sw_ratio
     :              , C_y_sw_fac_root
     :              , G_dul_dep
     :              , G_sw_dep
     :              , P_ll_dep
     :               )
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
*   neilh - 28-02-1997 - Made root factor constraint

*   Calls:
*   Pop_routine
*   Push_routine

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      REAL       C_specific_root_length ! (INPUT)  length of root per unit wt (mm/g)
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL       G_dlt_dm_green(*)     ! (INPUT)  plant biomass growth (g/m^2)
      REAL       G_dlt_rlv(*)          ! (INPUT)
      REAL       G_dlt_root_depth      ! (INPUT)  increase in root depth (mm)
      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
      REAL       P_xf(*)               ! (INPUT)  eXtension rate Factor (0-1)
      INTEGER    C_num_sw_ratio        ! (INPUT)
      REAL       C_x_sw_ratio(*)       ! (INPUT)
      REAL       C_y_sw_fac_root(*)    ! (INPUT)
      REAL       G_dul_dep(*)          ! (INPUT)  drained upper limit soil water content for soil layer L (mm water)
      REAL       G_sw_dep(*)           ! (INPUT)  soil water content of layer L (mm)
      REAL       P_ll_dep(*)           ! (INPUT)  lower limit of plant-extractable soil water for soil layer L (mm)
*     none

*   Global variables
      include 'crop3.inc'
      include 'convert.inc'

      real    divide                     ! function
      integer find_layer_no              ! function
      real    l_bound                    ! function
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
     :                 sugar_sw_avail_fac 
     :               (
     :                C_num_sw_ratio
     :              , C_x_sw_ratio
     :              , C_y_sw_fac_root
     :              , G_dul_dep
     :              , G_sw_dep
     :              , P_ll_dep
     :              , layer
     :               )  ! moisture factor
     :               * p_xf (layer)                ! growth factor
     :               * divide(g_dlayer(layer)      ! space weighting
     :                       ,g_root_depth         !       factor
     :                       ,0.0)

         rlv_factor (layer) = l_bound(rlv_factor(layer),1e-6)
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
       subroutine sugar_root_length_senescence
     :               (
     :                C_specific_root_length
     :              , G_dlayer
     :              , G_dlt_dm_senesced
     :              , G_rlv
     :              , G_root_depth
     :              , dlt_rlv_senesced
     :               )
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
      REAL       C_specific_root_length ! (INPUT)  length of root per unit wt (m
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL       G_dlt_dm_senesced(*)  ! (INPUT)  plant biomass senescence (g/m^
      REAL       G_rlv(*)              ! (INPUT)
      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
      real dlt_rlv_senesced (*)

*   Global variables
      include 'crop3.inc'
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

      call sugar_root_distrib
     :               (
     :                G_dlayer
     :              , G_rlv
     :              , G_root_depth
     :              , dlt_length, senesced_length
     :               )

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

*   Constant values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'sugar_profile_fasw')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------
      call push_routine (myname)

      deepest_layer = find_layer_no (g_root_depth, g_dlayer, max_layer)
      asw_pot = 0.0
      asw     = 0.0
      do 100 layer = 1, deepest_layer
         asw_pot = asw_pot + g_sw_avail_pot (layer)
         asw = asw + u_bound (g_sw_avail(layer), g_sw_avail_pot(layer))
  100 continue

      sugar_profile_fasw = divide (asw, asw_pot, 0.0)

      call pop_routine (myname)
      return
      end
* ====================================================================
       subroutine sugar_water_log_fact
     :               (
     :                C_num_water_log_fact
     :              , C_water_log_fact
     :              , C_water_log_rtfr
     :              , G_ll15_dep
     :              , G_sat_dep
     :              , G_sw_dep
     :              , G_dlayer
     :              , G_rlv
     :              , G_root_depth
     :              , water_log_fact
     :               )
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
      INTEGER    C_num_water_log_fact  ! (INPUT)
      REAL       C_water_log_fact(*)   ! (INPUT)
      REAL       C_water_log_rtfr(*)   ! (INPUT)
      REAL       G_ll15_dep(*)         ! (INPUT)
      REAL       G_sat_dep(*)          ! (INPUT)
      REAL       G_sw_dep(*)           ! (INPUT)  soil water content of layer L
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL       G_rlv(*)              ! (INPUT)
      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
       real    water_log_fact            ! (OUTPUT)

*   Global variables
       include 'crop3.inc'
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
      call sugar_root_distrib
     :               (
     :                G_dlayer
     :              , G_rlv
     :              , G_root_depth
     :              , root_fr, tot_root_fr
     :               )

      num_root_layers = count_of_real_vals (root_fr, max_layer)

      wet_root_fr = 0.0

      do 100 layer = 1, num_root_layers
         wfps = divide (g_sw_dep(layer)- g_ll15_dep(layer)
     :                 ,g_sat_dep(layer) - g_ll15_dep(layer)
     :                 ,0.0)
         wfps = bound (wfps, 0.0, 1.0)

         wet_root_fr = wet_root_fr + wfps * root_fr(layer)
  100 continue

      water_log_fact = linear_interp_real
     :                       (wet_root_fr
     :                       ,c_water_log_rtfr
     :                       ,c_water_log_fact
     :                       ,c_num_water_log_fact)


      call pop_routine (myname)
      return
      end
* ====================================================================
       subroutine sugar_water_content
     :               (
     :                C_cane_dmf_tt
     :              , C_cane_dmf_min
     :              , C_cane_dmf_max
     :              , C_num_cane_dmf
     :              , C_cane_dmf_rate
     :              , g_swdef_stalk
     :              , g_nfact_stalk
     :              , g_temp_stress_stalk
     :              , G_dlt_dm_green
     :              , g_dm_green
     :              , G_dlt_plant_wc
     :              , G_plant_wc
     :              , G_tt_tot
     :               )
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
      REAL       C_cane_dmf_tt(*)      ! (INPUT)
      REAL       C_cane_dmf_min(*)     ! (INPUT)
      REAL       C_cane_dmf_max(*)     ! (INPUT)
      INTEGER    C_num_cane_dmf        ! (INPUT)
      REAL       C_cane_dmf_rate       ! (INPUT)
      REAL       G_swdef_stalk         ! (INPUT)
      REAL       G_nfact_stalk         ! (INPUT)
      REAL       G_temp_stress_stalk   ! (INPUT)
      REAL       G_dlt_dm_green(*)     ! (INPUT)  plant biomass growth (g/m^2)
      REAL       G_dm_green(*)         ! (INPUT)
      REAL       G_dlt_plant_wc(*)     ! (INPUT)
      REAL       G_plant_wc(*)         ! (OUTPUT)
      REAL       G_tt_tot              ! (INPUT)

*   Global variables
       include 'crop3.inc'
       real      bound                   ! function
       real      divide                  ! function
       real      l_bound                 ! function
       real      linear_interp_real      ! function
       real      sum_between             ! function

*   Internal variables
       real tt                  ! thermal time (deg. day)
       real cane_dmf_max        ! max dm fraction in
                                ! cane(sstem+sucrose)
       real cane_dmf_min        ! min dm fraction in
                                ! cane(sstem+sucrose)
       real cane_dmf
       real cane_dmf_new
       real deficit
       real fr_dmf
       real new_fr_dmf
       real new_plant_wc
       real stress_factor_min
       real sucrose_fraction

*   Constant values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'sugar_water_content')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------
      call push_routine (myname)

      call fill_real_array (g_dlt_plant_wc, 0.0, max_part)

      tt = sum_between (begcane,now,g_tt_tot)

      cane_dmf_max = linear_interp_real (tt
     :                                  ,c_cane_dmf_tt
     :                                  ,c_cane_dmf_max
     :                                  ,c_num_cane_dmf)

      cane_dmf_min = linear_interp_real (tt
     :                                  ,c_cane_dmf_tt
     :                                  ,c_cane_dmf_min
     :                                  ,c_num_cane_dmf)

      stress_factor_min = min (g_swdef_stalk
     :                        ,g_nfact_stalk
     :                        ,g_temp_stress_stalk)

      cane_dmf = cane_dmf_max
     :         - stress_factor_min * (cane_dmf_max-cane_dmf_min)

      sucrose_fraction =
     :        divide (g_dlt_dm_green(sucrose)
     :               ,g_dlt_dm_green(sstem) + g_dlt_dm_green(sucrose)
     :               ,0.0)

      g_dlt_plant_wc(sstem) = divide (g_dlt_dm_green(sstem)
     :                               ,cane_dmf
     :                               ,0.0)
     :                      * (1.0 - sucrose_fraction)

      call pop_routine (myname)
      return
      end
*     ===========================================================
      subroutine sugar_sucrose_fraction
     :               (
     :                C_num_stress_factor_stalk
     :              , C_stress_Factor_stalk
     :              , C_sucrose_fraction_Stalk
     :              , G_swdef_stalk
     :              , G_nfact_stalk
     :              , g_temp_stress_stalk
     :              , sucrose_fraction
     :               )
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
      INTEGER    c_num_stress_factor_Stalk ! (INPUT)
      REAL       C_stress_factor_stalk(*) ! (INPUT)
      REAL       C_Sucrose_fraction_stalk(*) ! (INPUT)
      REAL       G_swdef_stalk     ! (INPUT)
      REAL       G_nfact_stalk     ! (INPUT)
      REAL       G_temp_stress_stalk     ! (INPUT)
      real       sucrose_fraction      ! (OUTPUT) fraction of cane C
                                       ! partitioned to sucrose (0-1)

*   Global variables
      include   'crop3.inc'
      real       bound                 ! function
      real       linear_interp_real    ! function

*   Internal variables
      real       stress_Factor_min     ! minimum of all 0-1 stress
                                       ! factors on stalk growth

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_sucrose_fraction')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      stress_factor_min = min (g_swdef_stalk
     :                        ,g_nfact_stalk
     :                        ,g_temp_stress_stalk)

      ! this should give same results as old version for now

      sucrose_fraction = linear_interp_real (stress_Factor_min
     :                                      ,c_stress_factor_stalk
     :                                      ,c_sucrose_fraction_stalk
     :                                      ,c_num_stress_factor_Stalk
     :                                      )

      call bound_check_real_var (sucrose_fraction
     :                        , 0.0
     :                        , 1.0
     :                        , 'fraction of Cane C to sucrose')

      sucrose_fraction = bound (sucrose_fraction, 0.0, 1.0)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_graze ()
*     ===========================================================

*   Short description:
*       remove part of the green material as if grazed

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
*     050996 nih specified and programmed

*   Calls:

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*     none

*   Global variables
      include   'const.inc'            ! lu_scr_sum, blank
      include   'convert.inc'          ! smm2sm
      include   'sugar.inc'

      real       sum_between           ! function
      real       sugar_leaf_no_from_lai! function

*   Internal variables
      real       c_eff                 ! fraction of C returned to soil
      real       dm_residue            ! dry matter going to residue
      real       fraction              ! fraction of green material grazed
      integer    leaf_no               ! index for leaves
      real       leaf_no_dead          ! number of dead or dying leaves
      real       n_eff                 ! fraction of N returned to soil
      real       n_residue             ! N going to residue
      integer    numvals               ! number of values found in array
      character  report*10             ! report flag
      character  string*150            ! output string
      real       grn_fr               ! fraction of bottom leaf that is dead
      integer    start_leaf            ! leaf to start grazing from
      real       fraction_removed      ! fraction of each leaf grazed

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_graze')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      ! request and receive variables from owner-modules
c      call sugar_get_other_variables ()

      call collect_real_var ('fraction', '()'
     :                      , fraction, numvals, 0.0, 1.0)

      call collect_real_var ('n_eff', '()'
     :                      , n_eff, numvals, 0.0, 1.0)

      call collect_real_var ('c_eff', '()'
     :                      , c_eff, numvals, 0.0, 1.0)

      call collect_char_var_optional ('report', '()'
     :                               , report, numvals)
      if (numvals.eq.0) then
         report = 'no'
      else
      endif

      ! Note - I could use loops here but I want to be specific.
      dm_residue = 0.0
      n_Residue = 0.0

      g_dm_graze = g_dm_graze + g_dm_green(leaf)*fraction
      g_n_graze = g_n_graze + g_n_green(leaf)*fraction
      dm_residue = dm_residue + g_dm_green(leaf)*fraction*c_eff
      n_residue = n_residue + g_n_green(leaf)*fraction*n_eff
      g_dm_green(leaf) = g_dm_green(leaf) * (1. - fraction)
      g_n_green(leaf) = g_n_green(leaf) * (1. - fraction)
      g_plant_wc(leaf) = g_plant_wc(leaf) * (1. - fraction)

      g_dm_graze = g_dm_graze + g_dm_green(cabbage)*fraction
      g_n_graze = g_n_graze + g_n_green(cabbage)*fraction
      dm_residue = dm_residue + g_dm_green(cabbage)*fraction*c_eff
      n_residue = n_residue + g_n_green(cabbage)*fraction*n_eff
      g_dm_green(cabbage) = g_dm_green(cabbage) * (1. - fraction)
      g_n_green(cabbage) = g_n_green(cabbage) * (1. - fraction)
      g_plant_wc(cabbage) = g_plant_wc(cabbage) * (1. - fraction)

      g_dm_graze = g_dm_graze + g_dm_green(sstem)*fraction
      g_n_graze = g_n_graze + g_n_green(sstem)*fraction
      dm_residue = dm_residue + g_dm_green(sstem)*fraction*c_eff
      n_residue = n_residue + g_n_green(sstem)*fraction*n_eff
      g_dm_green(sstem)= g_dm_green(sstem) * (1. - fraction)
      g_n_green(sstem)= g_n_green(sstem) * (1. - fraction)
      g_plant_wc(sstem) = g_plant_wc(sstem) * (1. - fraction)

      g_dm_graze = g_dm_graze + g_dm_green(sucrose)*fraction
      g_n_graze = g_n_graze + g_n_green(sucrose)*fraction
      dm_residue = dm_residue + g_dm_green(sucrose)*fraction*c_eff
      n_residue = n_residue + g_n_green(sucrose)*fraction*n_eff
      g_dm_green(sucrose)= g_dm_green(sucrose) * (1. - fraction)
      g_n_green(sucrose)= g_n_green(sucrose) * (1. - fraction)
      g_plant_wc(sucrose) = g_plant_wc(sucrose) * (1. - fraction)

      call sugar_top_residue (dm_residue, N_residue)

      ! Now we need to update the leaf tracking info

      g_lai = g_lai * (1. - fraction)

         ! get highest senescing leaf


      leaf_no_dead = sugar_leaf_no_from_lai
     :               (
     :                G_leaf_area
     :              , G_plants
     :              , g_slai
     :               )
      start_leaf = int(leaf_no_dead + 1.)
      do 100 leaf_no = start_leaf, max_leaf
         if (leaf_no .eq. start_leaf) then
            grn_fr = 1.0 - mod(leaf_no_dead,1.)
         else
            grn_fr = 1.0
         endif
         fraction_removed = fraction * grn_fr
         g_leaf_area(leaf_no) = g_leaf_area(leaf_no)
     :                        *(1.-fraction_removed)
         g_leaf_dm (leaf_no) = g_leaf_dm (leaf_no)
     :                        *(1.-fraction_removed)
  100 continue

             ! report

      if (report.eq.'yes') then
         write(string,'(1x,A,f4.1,A,f4.2,A,f4.2,A)')
     :              'Grazing '
     :             ,fraction*100
     :             ,'% of green material (N_eff = '
     :             ,N_eff
     :             ,', C_eff = '
     :             ,C_eff
     :             ,')'
         call report_event(string)
      else
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_leaf_no_init 
     :               (
     :                C_leaf_no_at_emerg
     :              , G_current_stage
     :              , G_days_tot
     :              , leaf_no
     :               )
*     ===========================================================

*   Short description:
*       Initialise leaf number

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
*     300996 nih created from sugar_leaf_area_init.

*   Calls:

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      REAL       C_leaf_no_at_emerg    ! (INPUT)  leaf number at emergence ()
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
      real       leaf_no(*)            ! (OUTPUT) leaf number

*   Global variables
      include   'crop3.inc'

      logical    on_day_of             ! function

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_leaf_no_init')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      if (on_day_of (emerg, g_current_stage, g_days_tot)) then
         leaf_no(emerg) = c_leaf_no_at_emerg
      else
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_root_depth_init
     :               (
     :                G_dlayer
     :              , G_rlv
     :              , G_root_depth
     :              , dlt_root_depth
     :               )
*     ===========================================================

*   Short description:
*       This routine returns the increase in root depth.  The
*       approach used here utilises a potential root front velocity
*       affected by relative moisture content at the rooting front.

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
*      300996 nih changed test for init of root depth due to limitation
*                 in on_day_of routine

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
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL       G_rlv(*)              ! (INPUT)
      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
      real       dlt_root_depth        ! (OUTPUT) increase in root depth (mm)

*   Global variables
      include   'crop3.inc'

      integer    count_of_real_vals    ! function
      real       sum_real_array        ! function

*   Internal variables
      integer    num_root_layers       !

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_root_depth_init')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

cnh      if (on_day_of (sprouting, g_current_stage, g_days_tot)) then
      if (g_root_depth .eq. 0.0) then

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

      else  ! we have no root growth

         ! do nothing
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_root_depth_increase 
     :               (
     :                C_root_depth_rate
     :              , G_current_stage
     :              , G_dlayer
     :              , G_root_depth
     :              , P_xf
     :              , C_num_sw_ratio
     :              , C_x_sw_ratio
     :              , C_y_sw_fac_root
     :              , G_dul_dep
     :              , G_sw_dep
     :              , P_ll_dep
     :              , dlt_root_depth
     :               )
*     ===========================================================

*   Short description:
*       This routine returns the increase in root depth.  The
*       approach used here utilises a potential root front velocity
*       affected by relative moisture content at the rooting front.

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
*      300996 nih changed test for init of root depth due to limitation
*                 in on_day_of routine

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
      REAL       C_root_depth_rate(*)  ! (INPUT)  root growth rate potential (mm depth/day)
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
      REAL       P_xf(*)               ! (INPUT)  eXtension rate Factor (0-1)
      INTEGER    C_num_sw_ratio        ! (INPUT)
      REAL       C_x_sw_ratio(*)       ! (INPUT)
      REAL       C_y_sw_fac_root(*)    ! (INPUT)
      REAL       G_dul_dep(*)          ! (INPUT)  drained upper limit soil water content for soil layer L (mm water)
      REAL       G_sw_dep(*)           ! (INPUT)  soil water content of layer L (mm)
      REAL       P_ll_dep(*)           ! (INPUT)  lower limit of plant-extractable soil water for soil layer L (mm)
      real       dlt_root_depth        ! (OUTPUT) increase in root depth (mm)

*   Global variables
      include   'crop3.inc'

      real       u_bound               ! function
      integer    count_of_real_vals    ! function
      integer    find_layer_no         ! function
      real       sugar_sw_avail_fac    ! function
      logical    stage_is_between      ! function
      real       sum_real_array        ! function

*   Internal variables
      integer    current_phase         ! current phase number
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      real       root_depth_max        ! maximum depth to which roots can
                                       ! go (mm)
      integer    root_layer_max        ! deepest layer that roots can grow
                                       ! into.
*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_root_depth_increase')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)


      if (stage_is_between (emerg, flowering
     :                     , g_current_stage)) then
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
     :                   * sugar_sw_avail_fac 
     :               (
     :                C_num_sw_ratio
     :              , C_x_sw_ratio
     :              , C_y_sw_fac_root
     :              , G_dul_dep
     :              , G_sw_dep
     :              , P_ll_dep
     :              , deepest_layer
     :               )
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
      subroutine sugar_root_dist (Option)
*     ===========================================================

*   Short description:
*       Plant root distribution calculations

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
*      250894 jngh specified and programmed

*   Calls:
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer    Option                ! (INPUT) option number

*   Global variables
      include   'const.inc'
      include   'sugar.inc'

*   Internal variables
*     none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_root_dist')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      if (Option .eq. 1) then

         call sugar_root_length_growth
     :               (
     :                C_specific_root_length
     :              , G_dlayer
     :              , G_dlt_dm_green
     :              , G_dlt_rlv
     :              , G_dlt_root_depth
     :              , G_root_depth
     :              , P_xf
     :              , C_num_sw_ratio
     :              , C_x_sw_ratio
     :              , C_y_sw_fac_root
     :              , G_dul_dep
     :              , G_sw_dep
     :              , P_ll_dep
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_water_supply (Option)
*     ===========================================================

*   Short description:
*       Plant water supply

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
*      250894 jngh specified and programmed

*   Calls:
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer    Option                ! (INPUT) option number

*   Global variables
      include   'const.inc'
      include   'sugar.inc'

*   Internal variables
      integer layer

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_water_supply')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
c+!!!!!!!!! check order dependency of deltas
      call push_routine (my_name)


      if (Option .eq. 1) then

            ! WATER UPTAKE
         call sugar_check_sw 
     :               (
     :                C_minsw
     :              , G_dlayer
     :              , G_dul_dep
     :              , G_num_layers
     :              , G_sw_dep
     :              , P_ll_dep
     :               )
         call sugar_sw_avail_pot 
     :               (
     :                G_dlayer
     :              , G_dul_dep
     :              , G_root_depth
     :              , P_ll_dep
     :              , g_sw_avail_pot
     :               )
         call sugar_sw_avail 
     :               (
     :                G_dlayer
     :              , G_root_depth
     :              , G_sw_dep
     :              , P_ll_dep
     :              , g_sw_avail
     :               )

         if (g_uptake_source.eq.'calc') then
            call sugar_sw_supply 
     :               (
     :                G_dlayer
     :              , G_root_depth
     :              , G_sw_dep
     :              , P_kl
     :              , P_ll_dep
     :              , g_sw_supply
     :               )


         else
            ! Use the water uptake values given by some other
            ! module in the APSIM system. (eg APSWIM)
            If (g_num_uptake_water.gt.0) then
               do 100 layer = 1, g_num_uptake_water
                  g_sw_supply (layer) =        g_uptake_water(layer)
  100          continue
            else
               call Fatal_Error (Err_Internal,
     :             'No soil water uptake information has been provided')
            endif
         endif


      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_water_uptake (Option)
*     ===========================================================

*   Short description:
*       Plant water uptake

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
*      250894 jngh specified and programmed

*   Calls:
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer    Option                ! (INPUT) option number

*   Global variables
      include   'const.inc'
      include   'sugar.inc'

*   Internal variables
      integer layer

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_water_uptake')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
c+!!!!!!!!! check order dependency of deltas
      call push_routine (my_name)


      if (Option .eq. 1) then

         if (g_uptake_source.eq.'calc') then
            call sugar_sw_uptake
     :               (
     :                G_dlayer
     :              , G_root_depth
     :              , G_sw_demand
     :              , G_sw_supply
     :              , g_dlt_sw_dep
     :               )

         else
            ! Use the water uptake values given by some other
            ! module in the APSIM system. (eg APSWIM)
            If (g_num_uptake_water.gt.0) then
               do 100 layer = 1, g_num_uptake_water
                  g_dlt_sw_dep(layer) = -1.0 * g_uptake_water(layer)
  100          continue
            else
               call Fatal_Error (Err_Internal,
     :             'No soil water uptake information has been provided')
            endif
         endif

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_water_demand (Option)
*     ===========================================================

*   Short description:
*       Plant water demand

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
*      250894 jngh specified and programmed

*   Calls:
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer    Option                ! (INPUT) option number

*   Global variables
      include   'const.inc'
      include   'sugar.inc'

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_water_demand')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
c+!!!!!!!!! check order dependency of deltas
      call push_routine (my_name)

      if (Option .eq. 1) then

         call sugar_sw_demand 
     :               (
     :                G_dlt_dm_pot_rue
     :              , G_transp_eff
     :              , g_sw_demand
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_light_supply (Option)
*     ===========================================================

*   Short description:
*       light supply

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
*      5/9/96 dph

*   Calls:

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer    Option                ! (INPUT) option number

*   Global variables
      include   'const.inc'
      include   'sugar.inc'

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_light_supply')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call sugar_radn_int 
     :               (
     :                C_extinction_coef
     :              , G_fr_intc_radn
     :              , G_lai
     :              , G_radn
     :              , g_radn_int
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_bio_RUE (Option)
*     ===========================================================

*   Short description:
*       biomass light

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
*      5/9/96 dph

*   Calls:

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer    Option                ! (INPUT) option number

*   Global variables
      include     'const.inc'
      include     'sugar.inc'

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_bio_RUE')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call sugar_dm_pot_rue
     :               (
     :                C_rue
     :              , G_current_stage
     :              , G_radn_int
     :              , G_nfact_photo
     :              , G_temp_stress_photo
     :              , G_water_log_fact
     :              , g_dlt_dm_pot_rue
     :               )

         call sugar_dm_pot_rue_pot
     :               (
     :                C_rue
     :              , G_current_stage
     :              , G_radn_int
     :              , g_dlt_dm_pot_rue_pot
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_dm_pot_rue
     :               (
     :                C_rue
     :              , G_current_stage
     :              , G_radn_int
     :              , G_nfact_photo
     :              , G_temp_stress_photo
     :              , G_water_log_fact
     :              , dlt_dm_pot
     :               )
*     ===========================================================

*   Short description:
*       This routine calculates the potential biomass (carbohydrate)
*       production for conditions where soil supply is non-limiting.
*

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
      REAL       C_rue(*)              ! (INPUT)  radiation use efficiency (g dm
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_radn_int            ! (INPUT)
      REAL       G_nfact_photo         ! (INPUT)
      REAL       G_temp_stress_photo   ! (INPUT)
      REAL       G_water_log_fact      ! (INPUT)
      real       dlt_dm_pot            ! (OUTPUT) potential dry matter
                                       ! (carbohydrate) production (g/m^2)

*   Global variables
      include   'crop3.inc'

      real       sugar_rue_reduction   ! function

*   Internal variables
      integer    current_phase         ! current phase number
      real       rue                   ! radiation use efficiency under
                                       ! no stress (g biomass/mj)

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_dm_pot_rue')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      current_phase = int (g_current_stage)
      rue = c_rue(current_phase)
     :    * sugar_rue_reduction
     :               (
     :                G_nfact_photo
     :              , G_temp_stress_photo
     :              , G_water_log_fact
     :               )

         ! potential dry matter production with temperature
         ! and N content stresses is calculated.
         ! This is g of dry biomass produced per MJ of intercepted
         ! radiation under stressed conditions.

cnh      call sugar_radn_int (radn_int)
      dlt_dm_pot = rue * g_radn_int

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_dm_pot_rue_pot 
     :               (
     :                C_rue
     :              , G_current_stage
     :              , G_radn_int
     :              , dlt_dm_pot
     :               )
*     ===========================================================

*   Short description:
*       This routine calculates the potential biomass (carbohydrate)
*       production for conditions where soil supply is non-limiting.
*

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
      REAL       C_rue(*)              ! (INPUT)  radiation use efficiency (g dm/mj)
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_radn_int            ! (INPUT)
      real       dlt_dm_pot            ! (OUTPUT) potential dry matter
                                       ! (carbohydrate) production (g/m^2)

*   Global variables
      include   'crop3.inc'

*   Internal variables
      integer    current_phase         ! current phase number
      real       rue                   ! radiation use efficiency under
                                       ! no stress (g biomass/mj)

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_dm_pot_rue_pot')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      current_phase = int (g_current_stage)
      rue = c_rue(current_phase)

         ! potential dry matter production with temperature
         ! and N content stresses is calculated.
         ! This is g of dry biomass produced per MJ of intercepted
         ! radiation under stressed conditions.

cnh      call sugar_radn_int (radn_int)
      dlt_dm_pot = rue * g_radn_int

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_transpiration_eff (Option)
*     ===========================================================

*   Short description:
*       Calculate today's transpiration efficiency from min and max
*       temperatures and converting mm water to g dry matter
*       (g dm/m^2/mm water)

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
*      5/9/96 dph

*   Calls:

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer    Option                ! (INPUT) option number

*   Global variables
      include     'const.inc'
      include     'sugar.inc'

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_transpiration_efficiency')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call sugar_transp_eff 
     :               (
     :                C_svp_fract
     :              , C_transp_eff_cf
     :              , G_maxt
     :              , G_mint
     :              , g_transp_eff
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end
* ====================================================================
       subroutine sugar_temperature_stress
     :               (
     :                C_num_ave_temp
     :              , C_x_ave_temp
     :              , C_y_stress_photo
     :              , G_maxt
     :              , G_mint
     :              , tfac
     :               )
* ====================================================================

*   Short description:
*      Temperature stress factor for photosynthesis.

*   Assumptions:
*      None

*   Notes:
*      None

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*     14-01-1997 - neilh - Programmed and Specified

*   Calls:
*     Pop_routine
*     Push_routine

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      INTEGER    C_num_ave_temp        ! (INPUT)  size_of of critical temperatur
      REAL       C_x_ave_temp(*)       ! (INPUT)  critical temperatures for phot
      REAL       C_y_stress_photo(*)   ! (INPUT)  Factors for critical temperatu
      REAL       G_maxt                ! (INPUT)  maximum air temperature (oC)
      REAL       G_mint                ! (INPUT)  minimum air temperature (oC)
       real tfac

*   Global variables
       include 'crop3.inc'

      real bound
      real linear_interp_real

*   Internal variables
      real       ave_temp              ! mean temperature for the day (oC)

*   Constant values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'sugar_temperature_stress')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------
      call push_routine (myname)


         ! now get the temperature stress factor that reduces
         ! photosynthesis (0-1)

      ave_temp = (g_maxt + g_mint) /2.0

      tfac = linear_interp_real (ave_temp
     :                          , c_x_ave_temp, c_y_stress_photo
     :                          , c_num_ave_temp)
      tfac = bound (tfac, 0.0, 1.0)

      call pop_routine (myname)
      return
      end
*     ===========================================================
      subroutine sugar_swdef_pheno 
     :               (
     :                C_num_sw_avail_ratio
     :              , C_x_sw_avail_ratio
     :              , C_y_swdef_pheno
     :              , G_dlayer
     :              , G_root_depth
     :              , G_sw_avail
     :              , G_sw_avail_pot
     :              , swdef
     :               )
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
      INTEGER    C_num_sw_avail_ratio  ! (INPUT)
      REAL       C_x_sw_avail_ratio(*) ! (INPUT)
      REAL       C_y_swdef_pheno(*)    ! (INPUT)
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
      REAL       G_sw_avail(*)         ! (INPUT)  actual extractable soil water (mm)
      REAL       G_sw_avail_pot(*)     ! (INPUT)  potential extractable soil water (mm)
      real      swdef                 ! (OUTPUT) sw stress factor (0-1)

*   Global variables
      include   'const.inc'
      include   'crop3.inc'

      real       bound                 ! function
      real       divide                ! function
      integer    find_layer_no         ! function
      real       linear_interp_real    ! function
      real       sum_real_array        ! function

*   Internal variables
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      real       sw_avail_ratio        ! water availability ratio
      real       sw_avail_pot_sum      ! potential extractable soil water (mm)
      real       sw_avail_sum          ! actual extractable soil water (mm)

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_swdef_pheno')

*   Initial data values
*       none
* --------------------- Executable code section ----------------------

      call push_routine (my_name)
      deepest_layer = find_layer_no (g_root_depth, g_dlayer, max_layer)

         sw_avail_pot_sum = sum_real_array (g_sw_avail_pot
     :                                    , deepest_layer)
         sw_avail_sum = sum_real_array (g_sw_avail, deepest_layer)

         sw_avail_ratio = divide (sw_avail_sum
     :                          , sw_avail_pot_sum, 1.0) !???
         sw_avail_ratio = bound (sw_avail_ratio , 0.0, 1.0)

         swdef = linear_interp_real (sw_avail_ratio
     :                       , c_x_sw_avail_ratio, c_y_swdef_pheno
     :                       , c_num_sw_avail_ratio)


      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_swdef_photo 
     :               (
     :                G_dlayer
     :              , G_root_depth
     :              , G_sw_demand
     :              , G_sw_supply
     :              , swdef
     :               )
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
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
      REAL       G_sw_demand           ! (INPUT)  total crop demand for water (mm)
      REAL       G_sw_supply(*)        ! (INPUT)  potential water to take up (supply) from current soil water (mm)
      real      swdef                 ! (OUTPUT) sw stress factor (0-1)

*   Global variables
      include   'const.inc'
      include   'crop3.inc'

      real       bound                 ! function
      real       divide                ! function
      integer    find_layer_no         ! function
      real       sum_real_array        ! function

*   Internal variables
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      real       sw_demand_ratio       ! water supply:demand ratio
      real       sw_supply_sum         ! total supply over profile (mm)

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_swdef_photo')

*   Initial data values
*       none
* --------------------- Executable code section ----------------------

      call push_routine (my_name)
      deepest_layer = find_layer_no (g_root_depth, g_dlayer, max_layer)

            ! get potential water that can be taken up when profile is full

         sw_supply_sum = sum_real_array (g_sw_supply, deepest_layer)
         sw_demand_ratio = divide (sw_supply_sum, g_sw_demand, 1.0)
         swdef = bound (sw_demand_ratio , 0.0, 1.0)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_water_stress_pheno (Option)
*     ===========================================================

*   Short description:
*         Get current water stress factors (0-1)

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
*     010994 jngh specified and programmed

*   Calls:

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer    Option                ! (INPUT) option number

*   Global variables
      include   'const.inc'
      include   'sugar.inc'

c      real       mungb_swdef           ! function

*   Internal variables
*     none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_water_stress_pheno')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      if (Option .eq. 1) then
         call sugar_swdef_pheno 
     :               (
     :                C_num_sw_avail_ratio
     :              , C_x_sw_avail_ratio
     :              , C_y_swdef_pheno
     :              , G_dlayer
     :              , G_root_depth
     :              , G_sw_avail
     :              , G_sw_avail_pot
     :              , g_swdef_pheno
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_water_stress_photo (Option)
*     ===========================================================

*   Short description:
*         Get current water stress factors (0-1)

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
*     010994 jngh specified and programmed

*   Calls:

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer    Option                ! (INPUT) option number

*   Global variables
      include   'const.inc'
      include   'sugar.inc'

c      real       mungb_swdef           ! function

*   Internal variables
*     none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_water_stress_photo')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      if (Option .eq. 1) then
         call sugar_swdef_photo 
     :               (
     :                G_dlayer
     :              , G_root_depth
     :              , G_sw_demand
     :              , G_sw_supply
     :              , g_swdef_photo
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_water_stress_expansion (Option)
*     ===========================================================

*   Short description:
*         Get current water stress factors (0-1)

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
*     010994 jngh specified and programmed

*   Calls:

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer    Option                ! (INPUT) option number

*   Global variables
      include   'const.inc'
      include   'sugar.inc'

c      real       mungb_swdef           ! function

*   Internal variables
*     none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_water_stress_expansion')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      if (Option .eq. 1) then
         call sugar_swdef_demand_ratio
     :               (
     :                C_num_sw_demand_ratio
     :              , C_x_sw_demand_ratio
     :              , C_y_swdef_leaf
     :              , G_dlayer
     :              , G_root_depth
     :              , G_sw_demand
     :              , G_sw_supply
     :              , g_swdef_expansion
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_nit_stress_photo (Option)
*     ===========================================================

*   Short description:
*         Get current Nitrogen stress factors (0-1)

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
*     010994 jngh specified and programmed

*   Calls:

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer    Option                ! (INPUT) option number

*   Global variables
      include   'const.inc'
      include   'sugar.inc'

*   Internal variables
*     none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_nit_stress_photo')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      if (Option .eq. 1) then

         call sugar_nfact
     :               (
     :                G_dm_green
     :              , G_n_conc_crit
     :              , G_n_conc_min
     :              , G_n_green
     :              , c_k_nfact_photo
     :              , g_nfact_photo
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_nit_stress_expansion (Option)
*     ===========================================================

*   Short description:
*         Get current Nitrogen stress factors (0-1)

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
*     010994 jngh specified and programmed

*   Calls:

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer    Option                ! (INPUT) option number

*   Global variables
      include   'const.inc'
      include   'sugar.inc'

*   Internal variables
*     none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_nit_stress_expansion')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      if (Option .eq. 1) then

         call sugar_nfact
     :               (
     :                G_dm_green
     :              , G_n_conc_crit
     :              , G_n_conc_min
     :              , G_n_green
     :              , c_k_nfact_expansion
     :              , g_nfact_expansion
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_height (Option)
*     ===========================================================

*   Short description:
*     Canopy height.

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
*     010994 jngh specified and programmed

*   Calls:

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer    Option                ! (INPUT) option number

*   Global variables
      include   'sugar.inc'
      include   'const.inc'

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_height')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      if (Option .eq. 1) then

         call sugar_canopy_height 
     :               (
     :                C_height_max
     :              , C_height_stem_slope
     :              , G_canopy_height
     :              , G_current_stage
     :              , G_dm_green
     :              , G_plants
     :              , g_dlt_canopy_height
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif


      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_phen_leaf (Option)
*     ===========================================================

*   Short description:
*       Leaf number development

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
*      250894 jngh specified and programmed

*   Calls:
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer    Option                ! (INPUT) option number

*   Global variables
      include   'const.inc'
      include   'sugar.inc'

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_phen_leaf')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

            ! Plant leaf development
      if (Option .eq. 1) then

            ! initialise total leaf number
         call sugar_leaf_no_init 
     :               (
     :                C_leaf_no_at_emerg
     :              , G_current_stage
     :              , G_days_tot
     :              , g_leaf_no
     :               )
         call sugar_leaf_appearance 
     :               (
     :                C_leaf_app_rate
     :              , C_leaf_app_rate_lfno
     :              , C_num_leaf_app_rate
     :              , G_current_stage
     :              , G_days_tot
     :              , G_dlt_tt
     :              , G_leaf_no
     :              , G_leaf_area
     :              , g_dlt_leaf_no
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_dm_pot_TE 
     :               (
     :                G_dlayer
     :              , G_root_depth
     :              , G_sw_supply
     :              , G_transp_eff
     :              , dlt_dm_pot_te
     :               )
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
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
      REAL       G_sw_supply(*)        ! (INPUT)  potential water to take up (supply) from current soil water (mm)
      REAL       G_transp_eff          ! (INPUT)
      real       dlt_dm_pot_te         ! (OUTPUT) actual dry matter
                                       ! production (g/m^2)

*   Global variables
      include   'crop3.inc'

      integer    find_layer_no         ! function
      real       sum_real_array        ! function

*   Internal variables
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
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
      dlt_dm_pot_te = sw_supply_sum*g_transp_eff

         ! potential by photosynthesis

c      call sugar_dm_potential (dlt_dm_pot)

         ! use whichever is limiting
c      dlt_dm = min (dlt_dm_pot, dlt_dm_transp)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_bio_TE (Option)
*     ===========================================================

*   Short description:
*       bio transpiration efficiency

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
*      5/9/96 dph

*   Calls:

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer    Option                ! (INPUT) option number

*   Global variables
      include     'const.inc'
      include     'sugar.inc'

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_bio_TE')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call sugar_dm_pot_te  
     :               (
     :                G_dlayer
     :              , G_root_depth
     :              , G_sw_supply
     :              , G_transp_eff
     :              , g_dlt_dm_pot_te
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_bio_actual (Option)
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
*      250894 jngh specified and programmed

*   Calls:
*     sugar_dm_grain
*     sugar_dm_grain_hi
*     sugar_dm_init
*     sugar_dm_partition
*     sugsr_dm_retranslocate
*     sugar_dm_stress_max
*     sugar_grain_no
*     sugar_heat_stress
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer    Option                ! (INPUT) option number

*   Global variables
      include   'const.inc'
      include   'sugar.inc'

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_bio_actual')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call sugar_dm_init 
     :               (
     :                C_dm_cabbage_init
     :              , C_dm_leaf_init
     :              , C_dm_sstem_init
     :              , C_dm_sucrose_init
     :              , C_specific_root_length
     :              , G_current_stage
     :              , G_days_tot
     :              , G_dlayer
     :              , G_plants
     :              , G_rlv
     :              , g_dm_green, g_dm_plant_min
     :              , g_leaf_dm
     :               )
                               
            ! use whichever is limiting
         g_dlt_dm = min (g_dlt_dm_pot_rue, g_dlt_dm_pot_te)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_bio_partition (Option)
*     ===========================================================

*   Short description:
*       Partition biomass.

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
*      250894 jngh specified and programmed

*   Calls:
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer    Option                ! (INPUT) option number

*   Global variables
      include   'const.inc'
      include   'sugar.inc'

      real       sum_between           ! function

*   Internal variables
      real       leaf_no_today

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_bio_partition')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         leaf_no_today = sum_between (emerg, now, g_leaf_no)
     :              + g_dlt_leaf_no

         call sugar_sla_min
     :               (
     :                C_num_sla_lfno
     :              , C_sla_lfno
     :              , C_sla_min
     :              , leaf_no_today, g_sla_min
     :               )
         call sugar_sucrose_fraction
     :               (
     :                c_num_stress_Factor_stalk
     :              , c_stress_factor_Stalk
     :              , c_sucrose_fraction_stalk
     :              , G_swdef_stalk
     :              , g_nfact_stalk
     :              , g_temp_stress_stalk
     :              , g_sucrose_fraction
     :               )

         call sugar_dm_partition
     :               (
     :                C_cane_fraction
     :              , C_leaf_cabbage_ratio
     :              , G_min_sstem_sucrose
     :              , C_ratio_root_shoot
     :              , C_sucrose_delay
     :              , G_current_stage
     :              , G_dm_green
     :              , G_sla_min
     :              , G_sucrose_fraction
     :              , G_tt_tot
     :              , g_dlt_dm
     :                          , g_dlt_lai_stressed
     :                          , g_dlt_dm_green
     :                          , g_partition_xs
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_bio_partition_pot (Option)
*     ===========================================================

*   Short description:
*       Partition biomass.

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
*      250894 jngh specified and programmed

*   Calls:
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer    Option                ! (INPUT) option number

*   Global variables
      include   'const.inc'
      include   'sugar.inc'

      real       sum_between           ! function

*   Internal variables
      real       leaf_no_today

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_bio_partition_pot')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         leaf_no_today = sum_between (emerg, now, g_leaf_no)
     :              + g_dlt_leaf_no

         call sugar_sla_min
     :               (
     :                C_num_sla_lfno
     :              , C_sla_lfno
     :              , C_sla_min
     :              , leaf_no_today, g_sla_min
     :               )
         call sugar_sucrose_fraction
     :               (
     :                c_num_stress_Factor_stalk
     :              , c_stress_factor_Stalk
     :              , c_sucrose_fraction_stalk
     :              , G_swdef_stalk
     :              , g_nfact_stalk
     :              , g_temp_stress_stalk
     :              , g_sucrose_fraction
     :               )

         call sugar_dm_partition_pot
     :               (
     :                C_cane_fraction
     :              , C_leaf_cabbage_ratio
     :              , G_min_sstem_sucrose
     :              , C_ratio_root_shoot
     :              , C_sucrose_delay
     :              , G_current_stage
     :              , G_dm_green
     :              , G_sla_min
     :              , G_sucrose_fraction
     :              , G_tt_tot
     :              , g_dlt_dm_pot_rue_pot
     :                          , g_dlt_lai_pot
     :                          , g_dlt_dm_green_pot
     :                          , g_partition_xs_pot
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_dm_partition_rules
     :               (
     :                C_cane_fraction
     :              , C_leaf_cabbage_ratio
     :              , G_min_sstem_sucrose
     :              , C_ratio_root_shoot
     :              , C_sucrose_delay
     :              , G_current_stage
     :              , G_dm_green
     :              , G_sla_min
     :              , G_sucrose_fraction
     :              , G_tt_tot
     :              , dlt_dm
     :                              , dlt_lai_pot
     :                              , dlt_dm_green
     :                              , partition_xs
     :               )
*     ===========================================================

*   Short description:
*       Partitions assimilate between individual plant pools.  The rules
*       for partitioning change with stage of crop growth.

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
      REAL       C_cane_fraction       ! (INPUT)
      REAL       C_leaf_cabbage_ratio  ! (INPUT)  ratio of leaf wt to cabbage wt
      REAL       G_min_sstem_sucrose   ! (INPUT)
      REAL       C_ratio_root_shoot(*) ! (INPUT)  root:shoot ratio of new dm ()
      REAL       C_sucrose_delay       ! (INPUT)
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
      REAL       G_sla_min             ! (INPUT)  minimum specific leaf area (mm
      REAL       G_sucrose_fraction    ! (INPUT)  fraction of cane C going to su
      REAL       G_tt_tot(*)           ! (INPUT)  the sum of growing degree days
      real       dlt_dm                ! (INPUT) dry matter to partition
      real       dlt_lai_pot           ! (INPUT) increase in lai if
                                       ! unconstrained by carbon supply.
      real       dlt_dm_green (*)      ! (OUTPUT) actual biomass partitioned
                                       ! to plant parts (g/m^2)
      real       partition_xs          ! xs dry matter to that required
                                       ! to supply all demands. (g/m^2)

*   Global variables
      include   'convert.inc'
      include   'crop3.inc'

      real       divide                ! function
      logical    stage_is_between      ! function
      real       sum_real_array        ! function
      real       sum_between           ! function
      real       u_bound               ! function

*   Internal variables
      integer    current_phase         ! current phase no.
      real       dlt_dm_green_tot      ! total of partitioned dm (g/m^2)
      real       dlt_leaf_max          ! max increase in leaf wt (g/m2)
      real       dlt_cane              ! increase in cane wt (g/m2)
      real       dlt_cane_min          ! min increase in cane wt (g/m2)
      real       tt_since_begcane      ! thermal time since the beginning
                                       ! of cane growth (deg days)
*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_dm_partition_rules')

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


      dlt_leaf_max = divide (dlt_lai_pot
     :                      , g_sla_min*smm2sm
     :                      , 0.0)

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
     :       (g_dm_green(SStem).gt. g_min_sstem_sucrose))
     :   then
            ! the SStem pool gets (1 - c_sucrose_fraction) of the DEMAND
            ! for C. Extra C above the demand for cane goes only into
            ! the sucrose pool.

            dlt_dm_green(SStem) = dlt_cane_min
     :                          * (1.- g_sucrose_fraction)
            dlt_dm_green(Sucrose) = dlt_cane_min * g_sucrose_fraction

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
      subroutine sugar_dm_partition_pot
     :               (
     :                C_cane_fraction
     :              , C_leaf_cabbage_ratio
     :              , G_min_sstem_sucrose
     :              , C_ratio_root_shoot
     :              , C_sucrose_delay
     :              , G_current_stage
     :              , G_dm_green
     :              , G_sla_min
     :              , G_sucrose_fraction
     :              , G_tt_tot
     :              , dlt_dm
     :                              , dlt_lai_pot
     :                              , dlt_dm_green
     :                              , partition_xs
     :               )
*     ===========================================================

*   Short description:
*       Partitions assimilate between individual plant pools.  The rules
*       for partitioning change with stage of crop growth.

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
      REAL       C_cane_fraction       ! (INPUT)
      REAL       C_leaf_cabbage_ratio  ! (INPUT)  ratio of leaf wt to cabbage wt
      REAL       G_min_sstem_sucrose   ! (INPUT)
      REAL       C_ratio_root_shoot(*) ! (INPUT)  root:shoot ratio of new dm ()
      REAL       C_sucrose_delay       ! (INPUT)
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
      REAL       G_sla_min             ! (INPUT)  minimum specific leaf area (mm
      REAL       G_sucrose_fraction    ! (INPUT)  fraction of cane C going to su
      REAL       G_tt_tot(*)           ! (INPUT)  the sum of growing degree days
      real       dlt_dm                ! (INPUT) dry matter to partition
      real       dlt_lai_pot           ! (INPUT) increase in lai if
                                       ! unconstrained by carbon supply.
      real       dlt_dm_green (*)      ! (OUTPUT) actual biomass partitioned
                                       ! to plant parts (g/m^2)
      real       partition_xs          ! xs dry matter to that required
                                       ! to supply all demands. (g/m^2)

*   Global variables
      include   'convert.inc'
      include   'crop3.inc'

*   Internal variables
*      none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_dm_partition_pot')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      call sugar_dm_partition_rules
     :               (
     :                C_cane_fraction
     :              , C_leaf_cabbage_ratio
     :              , G_min_sstem_sucrose
     :              , C_ratio_root_shoot
     :              , C_sucrose_delay
     :              , G_current_stage
     :              , G_dm_green
     :              , G_sla_min
     :              , G_sucrose_fraction
     :              , G_tt_tot
     :              , dlt_dm
     :                        ,dlt_lai_pot
     :                        ,dlt_dm_green
     :                        ,partition_xs
     :               )

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_leaf_area_stressed (Option)
*     ===========================================================

*   Short description:
*       Simulate potential stressed crop leaf area development - may
*       be limited by DM production in subsequent routine

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
*      250894 jngh specified and programmed

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
      integer    Option                ! (INPUT) option number

*   Global variables
      include   'const.inc'
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
      if (Option .eq. 1) then

         g_dlt_lai_stressed = g_dlt_lai_pot
     :      * min(g_nfact_expansion, g_swdef_expansion)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_leaf_actual (Option)
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
*      250894 jngh specified and programmed

*   Calls:
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer    Option                ! (INPUT) option number

*   Global variables
      include   'const.inc'
      include   'sugar.inc'

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_leaf_actual')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

            ! limit the delta leaf area by carbon supply
         call sugar_leaf_area 
     :               (
     :                G_dlt_dm_green
     :              , G_dlt_lai
     :              , G_dlt_lai_stressed
     :              , G_dlt_leaf_no
     :              , G_leaf_no
     :              , C_num_sla_lfno
     :              , C_sla_lfno
     :              , C_sla_max
     :               )
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_bio_retrans (Option)
*     ===========================================================

*   Short description:
*       Retranslocate biomass.

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
*      250894 jngh specified and programmed

*   Calls:
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer    Option                ! (INPUT) option number

*   Global variables
      include   'const.inc'
      include   'sugar.inc'

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_bio_retrans')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call sugar_dm_retranslocate 
     :               (
     :                g_dlt_dm_green_retrans
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_leaf_death_grass 
     :               (
     :                C_green_leaf_no
     :              , G_current_stage
     :              , G_days_tot
     :              , G_dlt_leaf_no
     :              , G_leaf_no
     :              , G_leaf_no_dead
     :              , dlt_leaf_no_dead
     :               )
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
      REAL       C_green_leaf_no       ! (INPUT)
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
      REAL       G_dlt_leaf_no         ! (INPUT)  fraction of oldest leaf expanded ()
      REAL       G_leaf_no(*)          ! (INPUT)  number of fully expanded leaves ()
      REAL       G_leaf_no_dead(*)     ! (INPUT)  no of dead leaves ()
      real       dlt_leaf_no_dead      ! (OUTPUT) new fraction of oldest
                                       ! green leaf

*   Global variables
      include   'crop3.inc'

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
      subroutine sugar_leaf_area_sen_age0 
     :               (
     :                G_dlt_leaf_no_dead
     :              , G_lai
     :              , G_leaf_area
     :              , G_leaf_no_dead
     :              , G_plants
     :              , G_slai
     :              , G_leaf_no_detached
     :              , C_leaf_no_at_emerg
     :              , dlt_slai_age
     :               )
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
      REAL       G_dlt_leaf_no_dead    ! (INPUT)  fraction of oldest green leaf senesced ()
      REAL       G_lai                 ! (INPUT)  live plant green lai
      REAL       G_leaf_area(*)        ! (INPUT)  leaf area of each leaf (mm^2)
      REAL       G_leaf_no_dead(*)     ! (INPUT)  no of dead leaves ()
      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
      REAL       G_slai                ! (INPUT)  area of leaf that senesces from plant
      REAL       G_leaf_no_detached    ! (INPUT)  number of detached leaves
      REAL       C_leaf_no_at_emerg    ! (INPUT)  number of leaves at emergence
      real       dlt_slai_age          ! (OUTPUT) new senesced lai from
                                       ! phasic devel.

*   Global variables
      include   'convert.inc'
      include   'crop3.inc'

      real       bound                 ! function
      real       sum_between           ! function
      real       sum_real_array        ! function

*   Internal variables
      real       dlt_leaf_area         ! potential senesced leaf area from
                                       ! highest leaf no. senescing (mm^2)
      integer    leaf_no_dead          ! current leaf number dying ()
      real       slai_age              ! lai senesced by natural ageing
      real       dead_fr_highest_dleaf

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_leaf_area_sen_age0')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

         ! now calculate the leaf senescence
         ! due to normal phenological (phasic) development

         ! get highest leaf no. senescing today

c      leaf_no_dead = int (1.0
c     :                   + sum_between (emerg, now, g_leaf_no_dead))

      ! note that the first leaf record really contains
      ! 1+c_leaf_no_at_emerg leaves in it - not 1.
      leaf_no_dead = int (1.0
     :                   + sum_between (emerg, now, g_leaf_no_dead))
     :                   - g_leaf_no_detached
     :                   - c_leaf_no_at_emerg
      leaf_no_dead = max(leaf_no_dead,1)

      dead_fr_highest_dleaf = mod(
     :                   1.0 + sum_between (emerg, now, g_leaf_no_dead)
     :                   - g_leaf_no_detached
     :                   - c_leaf_no_at_emerg
     :                   , 1.0)

         ! get area senesced from highest leaf no.

      dlt_leaf_area = mod (g_dlt_leaf_no_dead, 1.0)
     :                 * g_leaf_area(leaf_no_dead)

      slai_age = (sum_real_array (g_leaf_area, leaf_no_dead - 1)
     :         + dead_fr_highest_dleaf * g_leaf_area (leaf_no_dead)
     :         + dlt_leaf_area)
     :         * smm2sm * g_plants

      dlt_slai_age = bound (slai_age - g_slai, 0.0, g_lai)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_leaf_area_sen_light0 
     :               (
     :                C_extinction_coef
     :              , C_lai_sen_light
     :              , C_sen_light_slope
     :              , G_lai
     :              , dlt_slai_light
     :               )
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
      REAL       C_extinction_coef     ! (INPUT)  radiation extinction coefficient ()
      REAL       C_lai_sen_light       ! (INPUT)  critical lai above which light
      REAL       C_sen_light_slope     ! (INPUT)  slope of linear relationship between lai and light competition factor for determining leaf senesence rate.
      REAL       G_lai                 ! (INPUT)  live plant green lai
      real       dlt_slai_light        ! (OUTPUT) lai senesced by low light

*   Global variables
      include   'crop3.inc'

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
      subroutine sugar_leaf_area_sen_water0 
     :               (
     :                C_sen_rate_water
     :              , G_lai
     :              , G_swdef_photo
     :              , dlt_slai_water
     :               )
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
      REAL       C_sen_rate_water      ! (INPUT)  slope in linear eqn relating soil water stress during photosynthesis to leaf senesense rate
      REAL       G_lai                 ! (INPUT)  live plant green lai
      REAL       G_swdef_photo         ! (INPUT)
      real       dlt_slai_water        ! (OUTPUT) water stress senescense

*   Global variables
      include   'crop3.inc'

      real       bound                 ! function

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

      slai_water_fac = c_sen_rate_water* (1.0 - g_swdef_photo)

      dlt_slai_water = g_lai * slai_water_fac
      dlt_slai_water = bound (dlt_slai_water, 0.0, g_lai)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_leaf_area_sen_frost0 
     :               (
     :                C_frost_fraction
     :              , C_frost_temp
     :              , C_num_frost_temp
     :              , G_lai
     :              , G_mint
     :              , dlt_slai_frost
     :               )
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
      REAL       C_frost_fraction(*)   ! (INPUT)
      REAL       C_frost_temp(*)       ! (INPUT)
      INTEGER    C_num_frost_temp      ! (INPUT)
      REAL       G_lai                 ! (INPUT)  live plant green lai
      REAL       G_mint                ! (INPUT)  minimum air temperature (oC)
      real       dlt_slai_frost        ! (OUTPUT) lai frosted today

*   Global variables
      include   'crop3.inc'

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
      subroutine sugar_leaf_area_sen_actual (Option)
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
*     010994 jngh specified and programmed

*   Calls:
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer    Option                ! (INPUT) option number

*   Global variables
      include   'const.inc'
      include   'sugar.inc'

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_leaf_area_sen_actual')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      if (Option .eq. 1) then

         ! now take largest of deltas
         g_dlt_slai = max (g_dlt_slai_age
     :                   , g_dlt_slai_light
     :                   , g_dlt_slai_water
     :                   , g_dlt_slai_frost)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_sen_nit (Option)
*     ===========================================================

*   Short description:
*       Simulate plant nitrogen senescence.

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
*      091294 jngh specified and programmed

*   Calls:
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer    Option                ! (INPUT) option number

*   Global variables
      include   'const.inc'
      include   'sugar.inc'

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_sen_nit')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call sugar_N_senescence 
     :               (
     :                C_n_cabbage_sen_conc
     :              , C_n_leaf_sen_conc
     :              , C_n_root_sen_conc
     :              , G_dlt_dm_senesced
     :              , g_dlt_N_senesced
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_sen_bio (Option)
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
*      091294 jngh specified and programmed

*   Calls:
*     sugar_dm_senescence
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer    Option                ! (INPUT) option number

*   Global variables
      include   'const.inc'
      include   'sugar.inc'

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_sen_bio')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call sugar_dm_senescence 
     :               (
     :                C_dm_root_sen_frac
     :              , C_leaf_cabbage_ratio
     :              , C_cabbage_sheath_fr
     :              , G_dlt_dm_green
     :              , G_dlt_lai
     :              , G_dlt_slai
     :              , G_dm_green
     :              , G_dm_senesced
     :              , G_lai
     :              , G_leaf_dm
     :              , G_plants
     :              , G_slai
     :              , G_leaf_area
     :              , g_dlt_dm_senesced
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_sen_rlv (Option)
*     ===========================================================

*   Short description:
*       Simulate plant nitrogen senescence.

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
*      091294 jngh specified and programmed

*   Calls:
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer    Option                ! (INPUT) option number

*   Global variables
      include   'const.inc'
      include   'sugar.inc'

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_sen_rlv')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call sugar_root_length_senescence 
     :               (
     :                C_specific_root_length
     :              , G_dlayer
     :              , G_dlt_dm_senesced
     :              , G_rlv
     :              , G_root_depth
     :              , g_dlt_rlv_senesced
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_nit_init (Option)
*     ===========================================================

*   Short description:
*       Initialise plant nitrogen.

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
*      250894 jngh specified and programmed

*   Calls:
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer    Option                ! (INPUT) option number

*   Global variables
      include   'const.inc'
      include   'sugar.inc'

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_nit_init')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call sugar_N_init
     :               (
     :                C_n_cabbage_init_conc
     :              , C_n_leaf_init_conc
     :              , C_n_root_init_conc
     :              , C_n_sstem_init_conc
     :              , G_current_stage
     :              , G_days_tot
     :              , G_dm_green
     :              , g_N_green
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_nit_supply (Option)
*     ===========================================================

*   Short description:
*       Find nitrogen supply.

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
*      250894 jngh specified and programmed

*   Calls:
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer    Option                ! (INPUT) option number

*   Global variables
      include   'const.inc'
      include   'sugar.inc'

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_nit_supply')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

            ! find potential N uptake (supply, available N)
      if (Option .eq. 1) then

               ! Get it for nitrate by diffusion and mass flow
               ! Note: the N available by diffusion is really the total N
               ! available to the roots by mass flow and diffusion.

         call sugar_N_mass_flow 
     :               (
     :                G_dlayer
     :              , G_dlt_sw_dep
     :              , G_no3gsm
     :              , G_no3gsm_min
     :              , G_root_depth
     :              , G_sw_dep
     :              , g_NO3gsm_mflow_avail
     :               )
         call sugar_N_diffusion 
     :               (
     :                G_dlayer
     :              , G_no3gsm
     :              , G_no3gsm_min
     :              , G_root_depth
     :              , G_sw_avail
     :              , G_sw_avail_pot
     :              , g_NO3gsm_diffn_pot
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_nit_retrans (Option)
*     ===========================================================

*   Short description:
*       Do nitrogen retranslocation.

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
*      250894 jngh specified and programmed

*   Calls:
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer    Option                ! (INPUT) option number

*   Global variables
      include   'const.inc'
      include   'sugar.inc'

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_nit_retrans')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call sugar_N_retranslocate 
     :               (
     :                G_dm_green
     :              , G_n_conc_min
     :              , G_n_green
     :              , g_dlt_N_retrans
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_nit_demand (Option)
*     ===========================================================

*   Short description:
*       Find nitrogen demand.

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
*      250894 jngh specified and programmed

*   Calls:
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer    Option                ! (INPUT) option number

*   Global variables
      include   'const.inc'
      include   'sugar.inc'

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_nit_demand')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call sugar_N_demand 
     :               (
     :                G_dlt_dm_green_pot
     :              , G_dlt_dm_pot_rue_pot
     :              , G_dm_green
     :              , G_n_conc_crit
     :              , G_n_green
     :              , g_N_demand
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_nit_uptake (Option)
*     ===========================================================

*   Short description:
*       Find nitrogen uptake.

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
*      250894 jngh specified and programmed

*   Calls:
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer    Option                ! (INPUT) option number

*   Global variables
      include   'const.inc'
      include   'sugar.inc'

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_nit_uptake')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

c      call sugar_N_uptake (g_dlt_NO3gsm, g_dlt_N_green)
         call sugar_N_uptake 
     :               (
     :                C_no3_diffn_const
     :              , G_dlayer
     :              , G_no3gsm_diffn_pot
     :              , G_no3gsm_mflow_avail
     :              , G_num_uptake_no3
     :              , G_n_demand
     :              , G_root_depth
     :              , G_uptake_no3
     :              , G_uptake_source
     :              , g_dlt_NO3gsm
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_nit_partition (Option)
*     ===========================================================

*   Short description:
*       Find nitrogen partitioning.

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
*      250894 jngh specified and programmed

*   Calls:
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer    Option                ! (INPUT) option number

*   Global variables
      include   'const.inc'
      include   'sugar.inc'

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_nit_partition')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call sugar_N_partition
     :               (
     :                G_dlayer
     :              , G_dlt_no3gsm
     :              , G_n_demand
     :              , G_root_depth
     :              , g_dlt_N_green
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_cleanup ()
*     ===========================================================

*   Short description:
*       cleanup after crop processes

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
*      250894 jngh specified and programmed

*   Calls:

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
       include 'sugar.inc'

*   Internal variables

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_cleanup')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      call sugar_update
     :               (
     :                G_canopy_height
     :              , G_cnd_photo
     :              , G_cswd_expansion
     :              , G_cswd_pheno
     :              , G_cswd_photo
     :              , G_dlt_canopy_height
     :              , G_dlt_dm
     :              , G_dlt_dm_dead_detached
     :              , G_dlt_dm_detached
     :              , G_dlt_dm_green
     :              , G_dlt_dm_green_retrans
     :              , G_dlt_dm_senesced
     :              , G_dlt_dm_realloc
     :              , G_dlt_lai
     :              , G_dlt_leaf_no
     :              , G_dlt_leaf_no_dead
     :              , G_dlt_n_dead_detached
     :              , G_dlt_n_detached
     :              , G_dlt_n_green
     :              , G_dlt_n_retrans
     :              , G_dlt_n_senesced
     :              , G_dlt_n_realloc
     :              , G_dlt_plants
     :              , G_dlt_plant_wc
     :              , G_dlt_rlv
     :              , G_dlt_rlv_senesced
     :              , G_dlt_root_depth
     :              , G_dlt_slai
     :              , G_dlt_slai_detached
     :              , G_dlt_stage
     :              , G_dlt_tlai_dead_detached
     :              , G_dm_dead
     :              , G_dm_green
     :              , G_dm_plant_top_tot
     :              , G_dm_senesced
     :              , G_lai
     :              , G_leaf_area
     :              , G_leaf_dm
     :              , G_leaf_no
     :              , G_leaf_no_dead
     :              , G_nfact_photo
     :              , G_n_conc_crit
     :              , G_n_conc_min
     :              , G_n_dead
     :              , G_n_green
     :              , G_n_senesced
     :              , G_plants
     :              , G_plant_wc
     :              , G_previous_stage
     :              , G_rlv
     :              , G_root_depth
     :              , G_slai
     :              , G_swdef_expansion
     :              , G_swdef_pheno
     :              , G_swdef_photo
     :              , G_tlai_dead
     :              , C_n_conc_crit_root
     :              , C_n_conc_min_root
     :              , C_x_stage_code
     :              , C_y_n_conc_crit_cabbage
     :              , C_y_n_conc_crit_cane
     :              , C_y_n_conc_crit_leaf
     :              , C_y_n_conc_min_cabbage
     :              , C_y_n_conc_min_cane
     :              , C_y_n_conc_min_leaf
     :              , G_current_stage
     :              , C_stage_code_list
     :              , G_phase_tt
     :              , G_tt_tot
     :              , G_leaf_no_detached
     :              , C_leaf_no_at_emerg
     :               )
      call sugar_totals
     :               (
     :                G_current_stage
     :              , G_days_tot
     :              , G_day_of_year
     :              , G_dlayer
     :              , G_dlt_sw_dep
     :              , G_dm_green
     :              , G_isdate
     :              , G_lai
     :              , G_lai_max
     :              , G_n_conc_act_stover_tot
     :              , G_n_demand
     :              , G_n_demand_tot
     :              , G_n_green
     :              , G_root_depth
     :              , G_transpiration_tot
     :               )
      call sugar_event
     :               (
     :                C_stage_code_list
     :              , C_stage_names
     :              , G_current_stage
     :              , G_days_tot
     :              , G_day_of_year
     :              , G_dlayer
     :              , G_dm_dead
     :              , G_dm_green
     :              , G_dm_senesced
     :              , G_lai
     :              , G_n_green
     :              , G_root_depth
     :              , G_sw_dep
     :              , G_year
     :              , P_ll_dep
     :               )

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_totals 
     :               (
     :                G_current_stage
     :              , G_days_tot
     :              , G_day_of_year
     :              , G_dlayer
     :              , G_dlt_sw_dep
     :              , G_dm_green
     :              , G_isdate
     :              , G_lai
     :              , G_lai_max
     :              , G_n_conc_act_stover_tot
     :              , G_n_demand
     :              , G_n_demand_tot
     :              , G_n_green
     :              , G_root_depth
     :              , G_transpiration_tot
     :               )
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
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
      INTEGER    G_day_of_year         ! (INPUT)  day of year
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL       G_dlt_sw_dep(*)       ! (INPUT)  water uptake in each layer (mm water)
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass) (g/m^2)
      INTEGER    G_isdate              ! (INPUT)  flowering day number
      REAL       G_lai                 ! (INPUT)  live plant green lai
      REAL       G_lai_max             ! (INPUT)  maximum lai - occurs at flowering
      REAL       G_n_conc_act_stover_tot ! (INPUT)  sum of tops actual N concentration (g N/g biomass)
      REAL       G_n_demand(*)         ! (INPUT)  plant nitrogen demand (g/m^2)
      REAL       G_n_demand_tot        ! (INPUT)  sum of N demand since last output (g/m^2)
      REAL       G_n_green(*)          ! (INPUT)  plant nitrogen content (g N/m^2)
      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
      REAL       G_transpiration_tot   ! (INPUT)  cumulative transpiration (mm)
*       none

*   Global variables
      include   'crop3.inc'

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
      subroutine sugar_water_content_cane (Option)
*     ===========================================================

*   Short description:
*       bio transpiration efficiency

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
*      5/9/96 dph

*   Calls:

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer    Option                ! (INPUT) option number

*   Global variables
      include     'const.inc'
      include     'sugar.inc'

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_water_content_cane')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call sugar_water_content
     :               (
     :                C_cane_dmf_tt
     :              , C_cane_dmf_min
     :              , C_cane_dmf_max
     :              , C_num_cane_dmf
     :              , C_cane_dmf_rate
     :              , g_swdef_stalk
     :              , g_nfact_stalk
     :              , g_temp_stress_stalk
     :              , G_dlt_dm_green
     :              , g_dm_green
     :              , G_dlt_plant_wc
     :              , G_plant_wc
     :              , G_tt_tot)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_failure_germination  
     :               (
     :                C_days_germ_limit
     :              , G_current_stage
     :              , G_days_tot
     :              , G_plants
     :              , dlt_plants
     :               )
*     ===========================================================

*   Short description:
*      Crop failure from lack of germination.

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
*       290994 jngh specified and programmed
*       110695 psc  added plant death from high soil temp
*       100795 jngh moved sugar_kill crop to end of routine

*   Calls:

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      REAL       C_days_germ_limit     ! (INPUT)  maximum days allowed after sowing for germination to take place (days)
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
      real       dlt_plants            ! (OUTPUT) change in plant number

*   Global variables
      include   'const.inc'            ! new_line
      include   'crop3.inc'

      logical    stage_is_between      ! function
      real       sum_between           ! function

*   Internal variables
      character  string*200            ! output string

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_failure_germination')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

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
c         call sugar_kill_crop ()

      else
         dlt_plants = 0.0

      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_failure_emergence 
     :               (
     :                C_tt_emerg_limit
     :              , G_current_stage
     :              , G_plants
     :              , G_tt_tot
     :              , dlt_plants
     :               )
*     ===========================================================

*   Short description:
*      Crop failure from lack of emergence.

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
*       290994 jngh specified and programmed
*       110695 psc  added plant death from high soil temp
*       100795 jngh moved sugar_kill crop to end of routine

*   Calls:

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      REAL       C_tt_emerg_limit      ! (INPUT)  maximum degree days allowed for emergence to take place (deg day)
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
      REAL       G_tt_tot(*)           ! (INPUT)  the sum of growing degree days for a phenological stage (oC d)
      real       dlt_plants            ! (OUTPUT) change in plant number

*   Global variables
      include   'const.inc'            ! new_line
      include   'crop3.inc'

      logical    stage_is_between      ! function
      real       sum_between           ! function

*   Internal variables
      character  string*200            ! output string

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_failure_emergence')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      if (stage_is_between (sprouting, emerg, g_current_stage)
     :       .and. sum_between (sprouting, now, g_tt_tot)
     :       .gt.c_tt_emerg_limit) then

         dlt_plants = - g_plants

         write (string, '(a)')
     :                 ' failed emergence due to deep planting'
         call write_string (lu_scr_sum, string)
c         call sugar_kill_crop ()

      else
         dlt_plants = 0.0

      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_failure_leaf_sen  
     :               (
     :                G_current_stage
     :              , G_lai
     :              , G_plants
     :              , dlt_plants
     :               )
*     ===========================================================

*   Short description:
*      Determine plant death from all leaf area senescing.

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
*       290994 jngh specified and programmed
*       110695 psc  added plant death from high soil temp
*       100795 jngh moved sugar_kill crop to end of routine

*   Calls:

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_lai                 ! (INPUT)  live plant green lai
      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
      real       dlt_plants            ! (OUTPUT) change in plant number

*   Global variables
      include   'const.inc'            ! new_line
      include   'crop3.inc'

      logical    reals_are_equal       ! function
      logical    stage_is_between      ! function

*   Internal variables
      character  string*200            ! output string

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_failure_leaf_sen')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      if (reals_are_equal (g_lai, 0.0)
     :       .and. stage_is_between (emerg, crop_end
     :                             , g_current_stage)) then

         dlt_plants = - g_plants

         write (string, '(3a)')
     :                ' crop failure because of total leaf senescence.'
         call write_string (lu_scr_sum, string)
c         call sugar_kill_crop ()

      else
         dlt_plants = 0.0

      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_death_actual
     :               (
     :                G_dlt_plants_death_drought
     :              , G_dlt_plants_failure_emergence
     :              , G_dlt_plants_failure_germ
     :              , G_dlt_plants_failure_leaf_sen
     :              , G_dlt_plants_death_lodging
     :              , dlt_plants
     :               )
*     ===========================================================

*   Short description:
*      Determine actual plant death.

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
*       290994 jngh specified and programmed
*       110695 psc  added plant death from high soil temp
*       100795 jngh moved mungb_kill crop to end of routine

*   Calls:

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      REAL       G_dlt_plants_death_drought ! (INPUT)
      REAL       G_dlt_plants_failure_emergence ! (INPUT)
      REAL       G_dlt_plants_failure_germ ! (INPUT)
      REAL       G_dlt_plants_failure_leaf_sen ! (INPUT)
      REAL       G_dlt_plants_death_lodging ! (INPUT)
      real       dlt_plants            ! (OUTPUT) change in plant number

*   Global variables
      include   'const.inc'            ! new_line
      include   'crop3.inc'

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_death_actual')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)


      dlt_plants = min (g_dlt_plants_failure_germ
     :                , g_dlt_plants_failure_emergence
     :                , g_dlt_plants_failure_leaf_sen
     :                , g_dlt_plants_death_drought
     :                , G_dlt_plants_death_lodging)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_death_drought 
     :               (
     :                C_leaf_no_crit
     :              , C_swdf_photo_limit
     :              , C_swdf_photo_rate
     :              , G_cswd_photo
     :              , G_leaf_no
     :              , G_plants
     :              , G_swdef_photo
     :              , dlt_plants
     :               )
*     ===========================================================

*   Short description:
*      Determine plant death from drought.

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
*       290994 jngh specified and programmed
*       110695 psc  added plant death from high soil temp
*       100795 jngh moved sugar_kill crop to end of routine

*   Calls:

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      REAL       C_leaf_no_crit        ! (INPUT)  critical number of leaves below which portion of the crop may die due to water stress
      REAL       C_swdf_photo_limit    ! (INPUT)  critical cumulative photosynthesis water stress above which the crop partly fails (unitless)
      REAL       C_swdf_photo_rate     ! (INPUT)  rate of plant reduction with photosynthesis water stress
      REAL       G_cswd_photo(*)       ! (INPUT)  cumulative water stress type 1
      REAL       G_leaf_no(*)          ! (INPUT)  number of fully expanded leaves ()
      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
      REAL       G_swdef_photo         ! (INPUT)
      real       dlt_plants            ! (OUTPUT) change in plant number

*   Global variables
      include   'const.inc'            ! new_line
      include   'crop3.inc'

      real       bound                 ! function
      real       sum_between           ! function

*   Internal variables
      real       cswd_photo            ! cumulative water stress for photoperiod
      real       leaf_no               ! number of leaves
      real       killfr                ! fraction of crop population to kill
      character  string*200            ! output string

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_death_drought')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      cswd_photo = sum_between (emerg, crop_end, g_cswd_photo)
      leaf_no = sum_between (emerg, now, g_leaf_no)

      if (leaf_no.lt.c_leaf_no_crit
     :       .and. cswd_photo.gt.c_swdf_photo_limit
     :       .and. g_swdef_photo.lt.1.0) then

         killfr = c_swdf_photo_rate* (cswd_photo - c_swdf_photo_limit)
         killfr = bound (killfr, 0.0, 1.0)
         dlt_plants = - g_plants*killfr

         write (string, '(a, i4, a)')
     :          'plant_kill.',
     :          nint (killfr*100.0)
     :         , '% failure because of water stress.'

         call report_event (string)

      else
         dlt_plants = 0.0

      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_N_partition 
     :               (
     :                G_dlayer
     :              , G_dlt_no3gsm
     :              , G_n_demand
     :              , G_root_depth
     :              , dlt_N_green
     :               )
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
      include   'crop3.inc'

      real       divide                ! function
      integer    find_layer_no         ! function
      real       sum_real_array        ! function

*   Subroutine arguments
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL       G_dlt_no3gsm(*)       ! (INPUT)  actual NO3 uptake from soil (g/m^2)
      REAL       G_n_demand(*)         ! (INPUT)  plant nitrogen demand (g/m^2)
      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
      real       dlt_N_green(max_part) ! (OUTPUT) actual plant N uptake
                                       ! into each plant part (g/m^2)

*   Internal variables
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      real       plant_part_fract      ! fraction of nitrogen to use (0-1)
                                       ! for plant part
      integer    part                  ! plant part number
      real       N_demand              ! total nitrogen demand (g/m^2)
      real       N_uptake_sum          ! total plant N uptake (g/m^2)

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_N_partition')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      call fill_real_array (dlt_N_green, 0.0, max_part)
      deepest_layer = find_layer_no(g_root_depth, g_dlayer, max_layer)

               ! find proportion of uptake to be
               ! distributed to each plant part and distribute it.
      N_uptake_sum = - sum_real_array (g_dlt_NO3gsm, deepest_layer)
      N_demand = sum_real_array (g_N_demand, max_part)

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
      subroutine sugar_water_log (Option)
*     ===========================================================

*   Short description:
*         Get current water stress factors (0-1)

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
*     010994 jngh specified and programmed

*   Calls:

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer    Option                ! (INPUT) option number

*   Global variables
      include   'const.inc'
      include   'sugar.inc'

c      real       mungb_swdef           ! function

*   Internal variables
*     none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_water_log')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      if (Option .eq. 1) then
         call sugar_water_log_fact
     :               (
     :                C_num_water_log_fact
     :              , C_water_log_fact
     :              , C_water_log_rtfr
     :              , G_ll15_dep
     :              , G_sat_dep
     :              , G_sw_dep
     :              , G_dlayer
     :              , G_rlv
     :              , G_root_depth
     :              , g_water_log_fact
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_water_stress_stalk (Option)
*     ===========================================================

*   Short description:
*         Get current water stress factors (0-1)

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
*     010994 jngh specified and programmed

*   Calls:

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer    Option                ! (INPUT) option number

*   Global variables
      include   'const.inc'
      include   'sugar.inc'

c      real       mungb_swdef           ! function

*   Internal variables
*     none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_water_stress_expansion')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      if (Option .eq. 1) then
         call sugar_swdef_demand_ratio
     :               (
     :                C_num_demand_ratio_stalk
     :              , C_x_demand_ratio_stalk
     :              , C_y_swdef_stalk
     :              , G_dlayer
     :              , G_root_depth
     :              , G_sw_demand
     :              , G_sw_supply
     :              , g_swdef_stalk
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_swdef_demand_ratio
     :               (
     :                C_num_sw_demand_ratio
     :              , C_x_sw_demand_ratio
     :              , C_y_swdef_leaf
     :              , G_dlayer
     :              , G_root_depth
     :              , G_sw_demand
     :              , G_sw_supply
     :              , swdef
     :               )
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
      INTEGER    C_num_sw_demand_ratio ! (INPUT)
      REAL       C_x_sw_demand_ratio(*) ! (INPUT)
      REAL       C_y_swdef_leaf(*)     ! (INPUT)
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
      REAL       G_sw_demand           ! (INPUT)  total crop demand for water (m
      REAL       G_sw_supply(*)        ! (INPUT)  potential water to take up (su
      real      swdef                 ! (OUTPUT) sw stress factor (0-1)

*   Global variables
      include   'const.inc'
      include   'crop3.inc'

      real       divide                ! function
      integer    find_layer_no         ! function
      real       linear_interp_real    ! function
      real       sum_real_array        ! function

*   Internal variables
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      real       sw_demand_ratio       ! water supply:demand ratio
      real       sw_supply_sum         ! total supply over profile (mm)

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_swdef_demand_ratio')

*   Initial data values
*       none
* --------------------- Executable code section ----------------------

      call push_routine (my_name)
      deepest_layer = find_layer_no (g_root_depth, g_dlayer, max_layer)

            ! get potential water that can be taken up when profile is full

         sw_supply_sum = sum_real_array (g_sw_supply, deepest_layer)
         sw_demand_ratio = divide (sw_supply_sum, g_sw_demand, 10.0)

         swdef = linear_interp_real (sw_demand_ratio
     :                       , c_x_sw_demand_ratio, c_y_swdef_leaf
     :                       , c_num_sw_demand_ratio)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_temp_stress_photo (Option)
*     ===========================================================

*   Short description:
*         Get current temperature stress factors (0-1)

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
*     010994 jngh specified and programmed

*   Calls:

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer    Option                ! (INPUT) option number

*   Global variables
      include   'const.inc'
      include   'sugar.inc'

*   Internal variables
*     none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_temp_stress_photo')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      if (Option .eq. 1) then

cnh I made it a subroutine like all the rest
          call sugar_temperature_stress
     :               (
     :                C_num_ave_temp
     :              , C_x_ave_temp
     :              , C_y_stress_photo
     :              , G_maxt
     :              , G_mint
     :              , g_temp_stress_photo
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_temp_stress_stalk (Option)
*     ===========================================================

*   Short description:
*         Get current temperature stress factors (0-1)

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
*     010994 jngh specified and programmed

*   Calls:

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer    Option                ! (INPUT) option number

*   Global variables
      include   'const.inc'
      include   'sugar.inc'

*   Internal variables
*     none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_temp_stress_stalk')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      if (Option .eq. 1) then

cnh I made it a subroutine like all the rest
          call sugar_temperature_stress
     :               (
     :                C_num_ave_temp_stalk
     :              , C_x_ave_temp_stalk
     :              , C_y_stress_stalk
     :              , G_maxt
     :              , G_mint
     :              , g_temp_stress_stalk
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_nit_stress_stalk (Option)
*     ===========================================================

*   Short description:
*         Get current Nitrogen stress factors (0-1)

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
*     010994 jngh specified and programmed

*   Calls:

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer    Option                ! (INPUT) option number

*   Global variables
      include   'const.inc'
      include   'sugar.inc'

*   Internal variables
*     none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_nit_stress_stalk')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      if (Option .eq. 1) then

         call sugar_nfact
     :               (
     :                G_dm_green
     :              , G_n_conc_crit
     :              , G_n_conc_min
     :              , G_n_green
     :              , c_k_nfact_stalk
     :              , g_nfact_stalk
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_nfact
     :               (
     :                G_dm_green
     :              , G_n_conc_crit
     :              , G_n_conc_min
     :              , G_n_green
     :              , k_nfact
     :              , nfact
     :               )
*     ===========================================================

*   Short description:
*     The concentration of Nitrogen in leaves is used to derive a
*     series of Nitrogen stress indices.  The stress indices for
*     photosynthesis and cell expansion are calculated from today's
*     relative nutritional status between a critical and minimum
*     leaf Nitrogen concentration.

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
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
      REAL       G_n_conc_crit(*)      ! (INPUT)  critical N concentration (g N/
      REAL       G_n_conc_min(*)       ! (INPUT)  minimum N concentration (g N/g
      REAL       G_n_green(*)          ! (INPUT)  plant nitrogen content (g N/m^
      REAL       k_nfact               ! (INPUT)  k value for stress factor
      real      nfact                 ! (OUTPUT) N stress factor

*   Global variables
      include   'const.inc'
      include   'crop3.inc'

      real       bound                 ! function
      real       divide                ! function

*   Internal variables
cbak      real       N_conc_stover         ! tops (stover) actual N concentratio
                                       ! (0-1)
      real       N_conc_leaf           ! leaf actual N concentration
                                       ! (0-1)
cbak      real       dm_stover             ! tops (stover) plant weight (g/m^2)
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


          N_def = k_nfact * N_conc_ratio
          nfact = bound (N_def, 0.0, 1.0)

      call pop_routine (my_name)
      return
      end
* ====================================================================
       subroutine sugar_death_lodging
     :               (
     :                g_lodge_flag
     :              , G_swdef_photo
     :              , g_water_log_fact
     :              , c_stress_lodge
     :              , c_death_fr_lodge
     :              , c_num_stress_lodge
     :              , G_plants
     :              , g_dlt_plants_death_lodging
     :               )

* ====================================================================

*   Short description:
*      None

*   Assumptions:
*      None

*   Notes:
*      None

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*     25-08-1997 - unknown - Programmed and Specified

*   Calls:
*     Pop_routine
*     Push_routine

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      logical g_lodge_flag
      real    g_swdef_photo
      real    g_water_log_fact
      real    c_stress_lodge(*)
      real    c_death_fr_lodge(*)
      integer c_num_stress_lodge
      real    g_plants
      real    g_dlt_plants_death_lodging

*   Global variables
      real linear_interp_real

*   Internal variables
      real min_stress_factor
      real death_fraction

*   Constant values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'sugar_death_lodging')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------
      call push_routine (myname)

      if (g_lodge_flag) then

         min_stress_factor = min(g_swdef_photo, g_water_log_fact)

         death_fraction = linear_interp_real (min_stress_factor
     :                                       ,c_stress_lodge
     :                                       ,c_death_fr_lodge
     :                                       ,c_num_stress_lodge)

         g_dlt_plants_death_lodging = - g_plants * death_fraction
      else
         g_dlt_plants_death_lodging = 0.0
      endif

      call pop_routine (myname)
      return
      end
*     ===========================================================
      subroutine sugar_min_sstem_sucrose (Option)
*     ===========================================================

*   Short description:
*       Set limit on SStem for start of sucrose partitioning

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
*      260897 nih specified and programmed

*   Calls:
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer    Option                ! (INPUT) option number

*   Global variables
      include   'const.inc'
      include   'sugar.inc'
      real      u_bound                ! function
      logical   on_day_of              ! function
      logical   stage_is_between       ! function

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_min_sstem_sucrose')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      if (Option .eq. 1) then
         ! These ideally should go in new routines but it seems overkill for
         ! a simple.  This is a patch job

         if (on_day_of (begcane, g_current_stage, g_days_tot)) then
            g_min_sstem_sucrose = c_min_sstem_sucrose
         else
         endif
         if (stage_is_between (begcane, crop_end
     :                        , g_current_stage)) then

            g_dlt_min_sstem_sucrose = c_min_sstem_sucrose_redn
     :                     * (1.0 - min(g_nfact_stalk, g_swdef_stalk))
            g_dlt_min_sstem_sucrose = u_bound(g_dlt_min_sstem_sucrose
     :                                       ,g_min_sstem_sucrose)
            g_min_sstem_sucrose = g_min_sstem_sucrose
     :                          - g_dlt_min_sstem_sucrose
         else
         endif
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_phase_dlt_tt
     :               (
     :                C_fasw_emerg
     :              , c_rel_emerg_rate
     :              , c_num_fasw_emerg
     :              , G_current_stage
     :              , G_days_tot
     :              , G_dlayer
     :              , G_sowing_depth
     :              , G_sw_dep
     :              , P_ll_dep
     :              , g_dul_dep
     :              , g_dlt_tt
     :              , dlt_tt
     :               )
*     ===========================================================

*   Short description:
*      Calculate change in phase thermal time as influenced by stress

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
*     divide
*     find_layer_no
*     on_day_of
*     pop_routine
*     push_routine
*     stage_is_between

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      REAL       C_fasw_emerg(*)       ! (INPUT)  plant extractable soil water i
      REAL       c_rel_emerg_rate(*)   ! (INPUT)
      REAL       c_num_fasw_emerg      ! (INPUT)
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL       G_sowing_depth        ! (INPUT)  sowing depth (mm)
      REAL       G_sw_dep(*)           ! (INPUT)  soil water content of layer L
      REAL       P_ll_dep(*)           ! (INPUT)  lower limit of plant-extractab
      REAL       G_dul_dep(*)          ! (INPUT)  drained upper limit(mm)
      real       g_dlt_tt
      real       dlt_tt

*   Global variables
      include   'crop3.inc'
      real       bound                 ! function
      integer    find_layer_no         ! function
      real       divide                ! function
      logical    on_day_of             ! function
      logical    stage_is_between      ! function
      real       linear_interp_real    ! function
      
*   Internal variables
      integer    layer_no_seed         ! seedling layer number
      real       pesw_seed             ! plant extractable soil water in
                                       ! seedling layer available for
                                       ! germination ( mm/mm)
      real       fasw_seed
      real       rel_emerg_rate        ! relative emergence rate (0-1)

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_phase_dlt_tt')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      if (stage_is_between (sprouting, emerg, g_current_stage)) then

         layer_no_seed = find_layer_no (g_sowing_depth, g_dlayer
     :                                , max_layer)
         pesw_seed = g_sw_dep(layer_no_seed)-p_ll_dep(layer_no_seed)
         fasw_seed = divide (
     :               g_sw_dep(layer_no_seed)-p_ll_dep(layer_no_seed)
     :              ,g_dul_dep(layer_no_seed)-p_ll_dep(layer_no_seed)
     :              ,0.0)
         fasw_seed = bound (fasw_seed, 0.0, 1.0)

         rel_emerg_rate = linear_interp_real (fasw_seed
     :                                       ,c_fasw_emerg
     :                                       ,c_rel_emerg_rate
     :                                       ,c_num_fasw_emerg)
         dlt_tt = g_dlt_tt * rel_emerg_rate
         
      else
         dlt_tt = g_dlt_tt
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_realloc (Option)
*     ===========================================================

*   Short description:
*       Reallocate cabbage to cane as plant develops to maintain
*       a fixed leaf:cabbage ratio

*   Assumptions:
*       none

*   Notes:
*       NIH - Not a generic realloc routine but will do for now

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
*     sugar_dm_senescence
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer    Option                ! (INPUT) option number

*   Global variables
      include   'const.inc'
      include   'sugar.inc'

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_realloc')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call sugar_realloc_cabbage
     :               (
     :                leaf
     :              , cabbage
     :              , sstem
     :              , max_part
     :              , C_cabbage_sheath_fr
     :              , G_dm_green
     :              , g_dlt_dm_senesced
     :              , G_n_green
     :              , g_dlt_dm_realloc
     :              , g_dlt_n_realloc
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_realloc_cabbage
     :               (
     :                leaf
     :              , cabbage
     :              , sstem
     :              , max_part
     :              , C_cabbage_sheath_fr
     :              , G_dm_green
     :              , g_dlt_dm_senesced
     :              , G_n_green
     :              , g_dlt_dm_realloc
     :              , g_dlt_n_realloc
     :               )

*     ===========================================================

*   Short description:
*       Reallocate cabbage to cane as plant develops to maintain
*       a fixed leaf:cabbage ratio

*   Assumptions:
*       none

*   Notes:
*       NIH - Not a generic realloc routine but will do for now

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
*     sugar_dm_senescence
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer leaf
      integer cabbage
      integer sstem
      integer max_part
      real C_cabbage_sheath_fr
      real G_dm_green(*)
      real g_dlt_dm_senesced(*)
      real G_n_green(*)
      real g_dlt_dm_realloc(*)
      real g_dlt_n_realloc(*)

*   Global variables
      real divide

*   Internal variables
      real realloc_wt
      real realloc_n

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_realloc_cabbage')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      call fill_real_array (g_dlt_dm_realloc, 0.0, max_part)
      call fill_real_array (g_dlt_n_realloc, 0.0, max_part)

      realloc_wt = g_dlt_dm_senesced(cabbage)
     :           * (divide (1.0,c_cabbage_sheath_fr,0.0) - 1.0)
      g_dlt_dm_realloc(cabbage) = - realloc_wt
      g_dlt_dm_realloc(sstem) = realloc_wt

      ! this is not 100% accurate but swings and round-abouts will look after
      ! it - I hope (NIH)
      realloc_n = divide (g_n_green (cabbage), g_dm_green(cabbage),0.0)
     :          * realloc_wt
      g_dlt_n_realloc(cabbage) = - realloc_n
      g_dlt_n_realloc(sstem) = realloc_n

      call pop_routine (my_name)
      return
      end
