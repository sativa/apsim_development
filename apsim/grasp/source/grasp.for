*     ===========================================================
      character*(*) function grasp_version ()
*     ===========================================================

*   Short description:
*       return version number of grasp module

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
*       011092 jngh specified and programmed

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
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'grasp_version')

      character  version_number*(*)    ! version number of module
      parameter (version_number = 'V0.2 170398')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      grasp_version = version_number

      call pop_routine (my_name)
      return
      end
*     ================================================================
      subroutine APSIM_grasp (action, data_string)
*     ================================================================
*   Short description:
*      this module models a sward of grass.

*      requirements :-
*        input - daily timestep
*             from other modules:-
*                day of year
*                year
*                minimum temperature (oC),
*                maximum temperature (oC)
*                solar radiation (mj/m^2),
*                latitude (olat)

*                layer depth (mm soil)
*                drained upper limit (mm water)

*                nitrate nitrogen in each layer (kg N/ha)
*                water content mm water

*             from parameter file, grasp section:-
*                ll = n1 ... nm  ! lower limit mm water/mm soil

*             from manager:-


*        output -
*             to other modules:-

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
*      050996 pdev upgraded to postbox (1.35)
*      261197 pdev added swim communication
*      170398 pdev max_n changed to distribution over profile. (EP)
*      310398 pdev bugs in root_proportion() causing max_n weirdness

*   Calls:
*     grasp_version
*     grasp_get_other_variables
*     grasp_harvest_crop
*     grasp_harvest_ready
*     grasp_init
*     grasp_process
*     grasp_send_my_variable
*     grasp_set_other_variables
*     grasp_start_crop
*     pop_routine
*     push_routine
*     send_variable_value
*     split_line

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      character  action*(*)     ! (INPUT) Message action to perform
      character  data_string*(*) ! (INPUT) Message data

*   Global variables
      include   'const.inc'     ! mes_presence, mes_init, mes_process
                                ! mes_report
      integer    lastnb
      character  grasp_version*15

*   Internal variables
      character  module_name*(max_module_name_size) ! module name

*   Constant values
      character  my_name*(*)    ! name of this procedure
      parameter (my_name='grasp')

*   Initial data values
*     none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

         ! initialise error flags
      call set_fatal_off ()
      call set_warning_off ()

      if (action.eq.mes_presence) then ! report presence
         call get_current_module (module_name)
         write(*, *) 'module_name = '
     :              , module_name(:lastnb(module_name))
     :              // blank
     :              // grasp_version ()

      elseif (action.eq.mes_init) then
            ! zero pools
         call grasp_zero_variables ()
            ! Get constants
         call grasp_init ()

      elseif (action.eq.mes_set_variable) then
                                ! respond to request to reset
                                ! variable values - from modules
         call grasp_set_my_variable (data_string)

      elseif (action.eq.mes_get_variable) then
                                ! respond to request for
                                ! variable values - from modules
         call grasp_send_my_variable (Data_string)

      elseif (action.eq.mes_prepare) then
              
         call grasp_prepare ()  ! Calculate potentials for swim

      elseif (action.eq.mes_process) then
         call grasp_zero_daily_variables ()
                                ! request and receive variables
                                ! from owner-modules
         call grasp_get_other_variables ()
                                ! do crop processes
         call grasp_process ()
                                ! send changes to owner-modules
         call grasp_set_other_variables ()

      else
         call message_unused ()

      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine grasp_process ()
*     ===========================================================

*   Short description:
*       simulate crop processes.  These include biomass production,
*       plant component development,
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
*      250894 jngh specified and programmed

*   Calls:
*     grasp_biomass
*     grasp_store_report_vars
*     grasp_event
*     grasp_harvest_ready
*     grasp_leaf_area
*     grasp_Nitrogen
*     grasp_phenology
*     grasp_plant_death
*     grasp_transpiration
*     grasp_update
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*     none

*   Global variables

*   Internal variables
*     none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'grasp_process')

*   Initial data values
*     none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      call grasp_save_yesterday () ! save for mass balance check

      call grasp_soil_loss ()      ! erode N from profile

c     do N at start of day to calculate N indexes for growth.
      call grasp_nitrogen ()   ! N uptake

      call grasp_transpiration () ! water uptake

      call grasp_phenology ()  ! phenological processes

      call grasp_biomass ()    ! biomass production

      call grasp_plant_death () ! see if sward has died (unused)

      call grasp_store_report_vars () ! collect totals for output

      call grasp_update ()     ! update pools

      call grasp_balance_check () ! check we haven't gone silly

      call grasp_event ()      ! do events of interest (date resets etc)

      call pop_routine (my_name)
      return
      end

*     ===========================================================
      subroutine grasp_prepare ()
*     ===========================================================

*   Short description:
*       prepare variables for SWIM

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
*     none

*   Global variables
      include   'const.inc'
      include   'grasp.inc'
      real      grasp_sw_pot
      real      grasp_total_cover

*   Internal variables
*     none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'grasp_prepare')

*   Initial data values
*     none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      call grasp_zero_daily_variables ()
      call grasp_get_other_variables ()

      call grasp_calculate_swi ()

      g_out_sw_demand = grasp_sw_pot ()        !!  = f(pan)
      g_out_total_cover = grasp_total_cover () !!  = f(pool size)

      call pop_routine (my_name)
      return
      end

*     ===========================================================
      subroutine grasp_phenology ()
*     ===========================================================

*   Short description:
*     It uses temperature, photoperiod and genetic characteristics
*     to determine the dates the crop begins a new growth phase.
*     The initial daily thermal time and height are also set.

*   Assumptions:
*       none

*   Notes:
*       Departure from standards here to comply with original CM ordering.

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
*     grasp_phenology_init
*     grasp_phase_devel
*     grasp_tt

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'grasp.inc'

*   Internal variables
*     none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'grasp_phenology')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      g_previous_stage = g_current_stage

      call grasp_devel (g_dlt_stage, g_current_stage)

                                ! canopy height
      call grasp_canopy_height (g_dlt_canopy_height)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine grasp_devel (dlt_stage, current_stage)
*     ===========================================================

*   Short description:
*     It uses temperature, photoperiod and genetic characteristics
*     to determine the dates the crop begins a new growth phase.
*     The initial daily thermal time and g_xstage are also set.

*     NB. Grasp is always in the `established' phase, so this routine
*     does nothing for now. Whether the phase concept can be applied
*     to a sward is doubtful, but seasonal growth patterns could be
*     applied here.

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
*     aint
*     pop_routine
*     push_routine
*     grasp_phase

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       dlt_stage      ! (OUTPUT) change in growth stage
      real       current_stage  ! (OUTPUT) new stage no.

*   Global variables
      include   'grasp.inc'

      logical    stage_is_between ! function

*   Internal variables
      real       new_stage      ! new stage number
      real       stage_devel    ! fraction of current phase elapsed ()

*   Constant values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_devel')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

                                ! kick immediately into est. phase
      if (stage_is_between (sowing, germ,
     :     g_current_stage)) then

         stage_devel = (establishment - sowing)

      elseif (stage_is_between (germ, emerg,
     :        g_current_stage)) then

         stage_devel =  (establishment - germ)

      elseif (stage_is_between (emerg, establishment,
     :        g_current_stage)) then

         stage_devel =  (establishment - emerg)

      else

         stage_devel = 0.0

      endif

         ! now calculate the new delta and the new stage
         ! mechanical operation - not to be changed

      new_stage = aint (g_current_stage) + stage_devel
      dlt_stage = new_stage - g_current_stage

      if (stage_devel.ge.1.0) then
         current_stage = aint (new_stage)

      else
         current_stage = new_stage

      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine grasp_canopy_height (dlt_canopy_height)
*     ===========================================================

*   Short description:
*       get change in plant canopy height
*       height = k * tsdm

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
*       231093 jngh specified and programmed

*   Calls:
*       bound

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       dlt_canopy_height ! (OUTPUT) canopy height change (mm)

*   Global variables
      include   'convert.inc'
      include   'grasp.inc'
      real       sum_real_array

*   Internal variables
      real       tsdm_tonne     ! tsdm in tonnes
      real       canopy_height

*   Constant values
      character  my_name*(*)    ! name of procedure
      parameter (my_name  = 'grasp_canopy_height')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      tsdm_tonne = kg2t *
     :     (sum_real_array (g_dm_green, max_part) - g_dm_green(root) +
     :      sum_real_array (g_dm_dead, max_part) - g_dm_dead(root))

      canopy_height = c_height_1000kg * tsdm_tonne
      dlt_canopy_height =  canopy_height - g_canopy_height

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine grasp_transpiration ()
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
*      250894 jngh specified and programmed

*   Calls:
*     grasp_check_sw
*     grasp_root_depth
*     grasp_sw_uptake
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'const.inc'     ! Constant definitions
      include   'grasp.inc'
      character string_concat*32 ! ?Really? 

*   Internal variables
      integer   layer
      integer   numvals
      character dlt_name*32

*   Constant values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_transpiration')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

                                ! sanity check against minsw
      call grasp_check_sw ()

                                ! increase in root depth
      call grasp_root_depth (g_dlt_root_depth)

      call grasp_calculate_swi ()

                                ! actual uptake
      call fill_real_array (g_dlt_sw_dep, 0.0, max_layer)
      if (p_uptake_source .eq. 'calc') then

                                ! actual uptake is calculated by grasp
         call grasp_sw_uptake (g_dlt_sw_dep)

      else if (p_uptake_source .eq. 'apsim') then

                                ! actual uptake is done by swim
         dlt_name = string_concat('uptake_water_',p_crop_type)
         call Get_real_array (
     :      unknown_module  ! Module that responds (Not Used)
     :     ,dlt_name        ! Variable Name
     :     ,max_layer       ! Array Size
     :     ,'(mm)'          ! Units                (Not Used)
     :     ,g_dlt_sw_dep    ! Variable
     :     ,numvals         ! Number of values returned
     :     ,0.0             ! Lower Limit for bound checking
     :     ,1000.)          ! Upper Limit for bound checking

         do 2000 layer = 1, numvals
             g_dlt_sw_dep(layer) = - g_dlt_sw_dep(layer) ! convert uptake to delta
 2000    continue

      else
         ! Whoops!!!
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine grasp_calculate_swi ()
*     ===========================================================

*   Short description:
*       Calculate soil water indices

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
*       none

*   Global variables
      include   'const.inc'     ! Constant definitions
      include   'grasp.inc'
      integer   find_layer_no
      real      bound
      real      grasp_swi

*   Internal variables
      integer   layer
      integer   deepest_layer

*   Constant values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_calculate_swi')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

c     There are 3 swi globals used
c     throughout grasp; rawswi_total, swi_total, swi(layer).

                                ! calculate sw supply indices
      call fill_real_array (g_swi, 0.0, max_layer)
      g_rawswi_total = 0.0

      deepest_layer = find_layer_no (g_root_depth, g_dlayer,
     :     max_layer)

      do 1000 layer = 1, deepest_layer
         g_swi(layer) = grasp_swi (layer)
         g_rawswi_total = g_rawswi_total + g_swi(layer)
 1000 continue

                                ! restricted swi
      g_swi_total = bound (g_rawswi_total, 0.0, 1.0)

      call pop_routine (my_name)
      return
      end

*     ===========================================================
      subroutine grasp_root_depth (dlt_root_depth)
*     ===========================================================

*   Short description:
*       returns the increase in root depth (mm)

*   Assumptions:
*       none

*   Notes:
*     none

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
*     bound
*     find_layer_no
*     on_day_of
*     pop_routine
*     push_routine
*     sum_real_array
*     sum_between

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       dlt_root_depth ! (OUTPUT) increase in root depth (mm)

*   Global variables
      include   'grasp.inc'

*   Internal variables

*   Constant values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_root_depth')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      dlt_root_depth = 0.0

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      real function  grasp_sw_pot ()
*     ===========================================================

*   Short description:
*       returns potential water uptake from the sward

*   Assumptions:
*       none

*   Notes:
*       from graspsub.for (pot_trans)

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
*     find_layer_no
*     grasp_layer_no
*     root_proportion
*     pop_routine
*     push_routine
*     fill_real_array

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*     none

*   Global variables
      include   'grasp.inc'
      real      grasp_transp_cover
      real      grasp_clothesline
      real      bound

*   Internal variables
      real      max_uptake

*   Constant values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_sw_pot')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      grasp_sw_pot = 0.0

      grasp_sw_pot = g_pan * grasp_transp_cover () *
     :     grasp_clothesline ()

cPdeV This limit is mentioned in the manual, but doesn't
c     appear in surfgrasp. As well, tree_sw_demand should
c     be removed. FIXME!
      max_uptake = 1.3 * g_pan - g_tree_sw_demand
      if (max_uptake .ge. 0.0) then
         grasp_sw_pot = bound (grasp_sw_pot, 0.0, max_uptake)
      else
         grasp_sw_pot = 0.0
      endif

      call pop_routine (my_name)
      return
      end

*     ===========================================================
      subroutine grasp_sw_uptake (dlt_sw_dep)
*     ===========================================================

*   Short description:
*       actual water usage

*   Assumptions:
*       none

*   Notes:
*       from graspsub.for (pot_trans)

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
*     find_layer_no
*     grasp_layer_no
*     root_proportion
*     pop_routine
*     push_routine
*     fill_real_array

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real      dlt_sw_dep(*)   ! (OUT) change in sw (mm)

*   Global variables
      include   'grasp.inc'
      integer   find_layer_no
      real      grasp_sw_pot
      real      divide

*   Internal variables
      integer   layer
      integer   deepest_layer
      real      sw_demand_tot

*   Constant values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_sw_uptake')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      deepest_layer = find_layer_no (g_root_depth, g_dlayer,
     :     max_layer)

      sw_demand_tot = grasp_sw_pot () * g_swi_total

      do 2000 layer = 1, deepest_layer
         dlt_sw_dep(layer) = -1.0 * sw_demand_tot *
     :        divide (g_swi(layer), g_rawswi_total, 0.0)

c         write (*,*) 'dlt_sw(',layer,') =', dlt_sw_dep(layer)
c         write (*,*) 'g_sw(',layer,') =', g_sw_dep(layer)

         call bound_check_real_var (dlt_sw_dep(layer),
     :        - g_sw_dep(layer), 0.0,
     :        'sw_uptake')

 2000 continue

      call pop_routine (my_name)
      return
      end

*     ===========================================================
      real function grasp_swi (layer)
*     ===========================================================

*   Short description:
*       Returns soil water index for a layer

*   Assumptions:
*       none

*   Notes:
*       PdeV 22/6/97. Greg hardwired 3 layers for grasp, whereas
*       apsim can have any number of layers. His equations for swi()
*       implicitly included this, ie:
*
*         swi(i) = (1.0 + sin ((awr(i) - 0.5)* PI)) * 0.5
*                                                     ^^^
*       for the top 2 layers.
*
*       I've used layer_proportion to cater for all number of profile
*       layers.
*
*       This has yet to be checked with an authoritative source..


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
*       divide
*       exp
*       pop_routine
*       push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer    layer          ! profile layer of interest

*   Global variables
      include   'const.inc'
      include   'grasp.inc'
      real       grasp_sw_supply ! functions
      real       bound
      integer    find_layer_no
      real       root_proportion

*   Internal variables
      real       sw_avail_ratio
      integer    deepest_layer

*   Constant values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_swi')
      real       PI
      parameter  (PI = 3.14159)

*   Initial data values
*     none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      grasp_swi = 0.0
      deepest_layer = find_layer_no (g_root_depth, g_dlayer,
     :     max_layer)

      if (layer .lt. 1 .or. layer .gt. deepest_layer) then
         call fatal_error(err_internal, 'Layer index bound')
      else

         sw_avail_ratio = grasp_sw_supply(layer)
         if (layer .lt. deepest_layer) then

            grasp_swi = (1.0 + sin ((sw_avail_ratio
     :           - 0.5)* PI)) * p_kl(layer)
         else
                                ! deepest layer:
            grasp_swi = (1.0 - cos (sw_avail_ratio
     :           * PI * 0.5)) * p_kl(layer) * 
     :           root_proportion (layer, g_dlayer, g_root_depth)

         endif
      endif

      grasp_swi = bound (grasp_swi, 0.0, 1.0)

      call pop_routine (my_name)
      return
      end

*     ===========================================================
      real function grasp_sw_supply (layer)
*     ===========================================================

*   Short description:
*     returns soil water supply ratio for a layer.

*   Assumptions:
*       none

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
*       010994 jngh specified and programmed

*   Calls:
*     find_layer_no
*     root_proportion
*     pop_routine
*     push_routine
*     fill_real_array

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer   layer

*   Global variables
      include   'grasp.inc'
c$$$      integer    find_layer_no  ! function
      real       bound          ! function
      real       divide

*   Internal variables
c$$$      integer    deepest_layer  ! deepest layer in which the roots are
c$$$                                ! growing
      real       pesw
      real       pesw_capacity

*   Constant values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_sw_supply')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      grasp_sw_supply = 0.0

cPdeV This check is redundant.
c$$$      deepest_layer = find_layer_no (g_root_depth, g_dlayer,
c$$$     :     max_layer)
c$$$      if (layer .lt. 1 .or. layer .gt. deepest_layer) then
c$$$         WARNING HERE
c$$$         call pop_routine (my_name)
c$$$         return
c$$$      endif

      pesw = g_sw_dep(layer) - g_ll_dep(layer)
      pesw_capacity  = g_dul_dep(layer) - g_ll_dep(layer)
      grasp_sw_supply = divide (pesw, pesw_capacity, 0.0)

      grasp_sw_supply = bound (grasp_sw_supply, 0.0, 1.0)

      call pop_routine (my_name)
      return
      end

*     ===========================================================
      real function grasp_clothesline ()
*     ===========================================================

*   Short description:
*     Mckeons clothesline effect

*   Assumptions:
*     none

*   Notes:
*     Uses swi of layer 1 for clothesline calculation.

*   Procedure attributes:
*      Version:         any hardware/fortran77
*      Extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       implicit none

*   Changes:
*       090994 jngh specified and programmed

*   Calls:
*     grasp_radn_int
*     divide
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments

*   Global variables
      include   'grasp.inc'
      real      grasp_vpd_hgt_ndx
      real      grasp_swi

*   Internal variables
      real vpd_hgt_ndx

*   Constant values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_clothesline')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

c NB this should return 0 if trees are present....
c you can test g_fr_intc_radn > 0, but only if trees are supplying
c cover...  FIXME!

      vpd_hgt_ndx = grasp_vpd_hgt_ndx (g_canopy_height)

      grasp_clothesline = 1.0 + (vpd_hgt_ndx - 1.0) *
     :     (1.0 - grasp_swi (1) )

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      real function grasp_vpd_hgt_ndx (sward_mm)
*     ===========================================================

*   Short description:
*      height adjusted vpd

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
*       090994 jngh specified and programmed

*   Calls:
*     divide
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real      sward_mm              ! (INPUT) height of sward (mm)

*   Global variables
      include   'grasp.inc'
      include   'convert.inc'
      real       bound
      real       divide

*   Internal variables
      real       factor
      real       sward_cm
      real       screen_cm

*   Constant values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_vpd_hgt_ndx')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      sward_cm = sward_mm * mm2cm
      screen_cm = c_hgt_vpd_screen * mm2cm

      factor = divide (c_vpd_grnd_mult - 1.0,   ! hmmm.
     :     0.0 - screen_cm, 0.0)

      grasp_vpd_hgt_ndx = 1 +
     :     (sward_cm - screen_cm) * factor

      grasp_vpd_hgt_ndx =  bound (grasp_vpd_hgt_ndx, 1.0,
     :     c_vpd_grnd_mult)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      real function grasp_dm_photo ()
*     ===========================================================

*   Short description:
*       potential biomass (carbohydrate) production from
*       photosynthesis - temperature and N effects.

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
*       090994 jngh specified and programmed
*       220797 pdev changed nix*tix to min(tix,nix) on advice of mckeon.

*   Calls:
*     grasp_radn_int
*     divide
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none
*   Subroutine arguments

*   Global variables
      include   'grasp.inc'
      real       grasp_nfact
      real       grasp_tfact

*   Internal variables
      integer    current_phase  ! current phase number
      real       rue            ! radiation use efficiency under
                                ! no stress (g biomass/mj)
      real       radn_int       ! Radiation intercepted by leaves (mj/m^2)

*   Constant values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_dm_photo')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      current_phase = int (g_current_stage)
      rue = p_rue(current_phase)

      call grasp_radn_int (radn_int)

c     potential dry matter production with temperature
c     and N content stresses is calculated.
c     This is kg of dry biomass produced per MJ of intercepted
c     radiation under stressed conditions.

      grasp_dm_photo = radn_int *
     :      rue *
     :      min(grasp_tfact (), grasp_nfact ())

      call pop_routine (my_name)
      return
      end

*     ===========================================================
      real function grasp_tfact ()
*     ===========================================================

*   Short description:
*     photosynthetic reduction factor for
*     temperature stress (0-1)

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
*       090994 jngh specified and programmed

*   Calls:
*     bound
*     linear_interp_real
*     min
*     pop_routine
*     push_routine
*     grasp_nfact

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*     none

*   Global variables
      include   'grasp.inc'

      real       bound          ! function
      real       linear_interp_real ! function

*   Internal variables
      real       ave_temp       ! mean temperature for the day (oC)

*   Constant values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_tfact')

*   Initial data values

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

c     Get the temperature stress factor that reduces
c     photosynthesis (0-1)

      ave_temp = (g_maxt + g_mint) / 2.0

      grasp_tfact = linear_interp_real (ave_temp
     :     , c_x_ave_temp, c_y_stress_photo
     :     , c_num_ave_temp)

      grasp_tfact = bound (grasp_tfact, 0.0, 1.0)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine grasp_radn_int (radn_int)
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
*     010994 jngh specified and programmed

*   Calls:
*     exp
*     l_bound
*     max
*     pop_routine
*     push_routine
*     reals_are_equal

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       radn_int       ! (OUTPUT) radiation intercepted
                                !   by leaves (mj/m^2)

*   Global variables
      include   'convert.inc'   ! sm2smm
      include   'grasp.inc'
      logical    reals_are_equal ! function
      real       grasp_radn_cover

*   Internal variables
*     none

*   Constant values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_radn_int')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

*     NB. g_fr_intc_radn is positive if the canopy module is running.
*     This would imply that trees or some other crop is present.

      if (reals_are_equal (g_fr_intc_radn, 0.0)) then
                                ! we need to calculate our
                                ! own interception
         radn_int = grasp_radn_cover () * g_radn

      else
                                ! interception has already
                                ! been calculated for us
         radn_int = g_fr_intc_radn * g_radn
      endif

      call pop_routine (my_name)
      return
      end

*     ===========================================================
      real function grasp_transp_eff ()
*     ===========================================================

*   Short description:
*     calculate today's transpiration efficiency from min and max
*     temperatures. Transpiration efficiency in converting mm water
*     to kg dry matter (kg dm/m^2/mm water)

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
*       240894 jngh specified and programmed

*   Calls:
*       exp
*       pop_routine
*       push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*     none

*   Global variables
      include   'convert.inc'   ! g2mm, mb2kpa
      include   'grasp.inc'
      real       grasp_vpd_hgt_ndx ! function
      real       divide
      real       l_bound

*   Internal variables
      real       vpd            ! vapour pressure deficit (kpa)
      real       vpd_sward

*   Constant values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_transp_eff')

*   Initial data values

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

                                ! Effective VPD
      vpd = c_svp_fract * g_vpd

c     Adjust transpiration-efficiency (TE) from standard 20mb to
c     actual vpd. If vpd is less than 1, assume that it has no
c     effect on TE.
      vpd_sward = l_bound (1.0, vpd *
     :     grasp_vpd_hgt_ndx (g_canopy_height))

      grasp_transp_eff =  divide(p_te_std * c_std_vpd, vpd_sward,
     :     0.0)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      real function grasp_vpd ()
*     ===========================================================

*   Short description:
*     Calculate today's vpd (hpa). This routine is not called if
*     vpd is available from elsewhere.

*   Assumptions:
*     the temperatures are > -237.3 oC for the svp function.

*   Notes:
*     average saturation vapour pressure for ambient temperature
*     during transpiration is calculated as part-way between that
*     for minimum temperature and that for the maximum temperature.
*     tanner & sinclair (1983) used .75 and .67 of the distance as
*     representative of the positive net radiation (rn).  Daily svp
*     should be integrated from about 0900 hours to evening when rn
*     becomes negetive.
*
*     1 mbar = 1 hpa??
*     Called only if VPD is unavailable from met file.

*   Procedure attributes:
*      Version:         any hardware/fortran77
*      Extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       implicit none

*   Changes:
*     240894 jngh specified and programmed

*   Calls:
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*     none

*   Global variables
      include   'grasp.inc'

*   Internal variables
      real       svp            ! function to get saturation vapour
                                ! pressure for a given temperature
                                ! in oC (mbar)
      real       temp_arg       ! dummy temperature for function (oC)

*   Constant values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_vpd')

*   Initial data values
c     set up saturation vapour pressure function
      svp(temp_arg) = 6.1078
     :     * exp (17.269 * temp_arg / (237.3 + temp_arg))


* --------------------- Executable code section ----------------------
      call push_routine (my_name)

c     Get vapour pressure deficit when net radiation is positive.

      grasp_vpd =  svp (g_maxt) - svp (g_mint)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine grasp_biomass ()
*     ===========================================================

*   Short description:
*     Simulate crop biomass processes.  These include biomass
*     production, and plant senescense.

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
*     grasp_dm_init
*     grasp_head_no
*     grasp_grain_no
*     grasp_dm_production
*     grasp_dm_partition
*     grasp_dm_retranslocate
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'grasp.inc'

*   Internal variables

*   Constant values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_biomass')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      call grasp_basal_area_init (g_basal_area)

                                ! drymatter production
      call grasp_dm_production (g_dlt_dm,
     :     g_out_growth_transp,
     :     g_out_growth_photo,
     :     g_out_growth_regrow)

                                ! distribute to plant parts
      call grasp_dm_partition (g_dlt_dm_plant)

                                ! death processes
      call grasp_dm_sen (g_dlt_dm_sen,
     :     g_out_death_frost,
     :     g_out_death_pheno,
     :     g_out_death_water)

                                ! detachment of dead material
      call grasp_detachment (g_detach)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine grasp_basal_area_init (basal_area)
*     ===========================================================

*   Short description:
*       sets basal area

*   Assumptions:
*       none

*   Notes:
*       This isn't quite the apsim methodology...

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
*     bound_check_real_array
*     bound_check_real_var
*     bound
*     int
*     pop_routine
*     push_routine
*     fill_real_array
*     stage_is_between
*     sum_between
*     sum_real_array

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       basal_area       ! (OUTPUT)

*   Global variables
      include   'grasp.inc'
      real      bound

*   Internal variables
*     none

*   Constant values
      character  my_name*(*)    ! name of procedure
      parameter (my_name  = 'grasp_basal_area_init')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      if (g_day_of_year .eq. c_day_end_summer) then

         basal_area = (g_acc_growth_last_summer +
     :        g_acc_et_summer * c_et_use_efficiency) * 0.001

         basal_area = bound(basal_area, c_ba_ll, c_ba_ul)

      else
                                ! Nothing
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine grasp_dm_sen (dlt_dm_sen, dlt_dm_frost,
     :     dlt_dm_pheno, dlt_dm_water)
*     ===========================================================

*   Short description:
*     Calculate grass death. Death comes from three
*     sources; drought, frost, and general background death.
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
*       090994 jngh specified and programmed

*   Calls:
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real      dlt_dm_sen(*)   ! (Out) dead dm
      real      dlt_dm_frost(*) ! (Out) death by temp
      real      dlt_dm_pheno(*) ! (Out) background death
      real      dlt_dm_water(*) ! (Out) death by water

*   Global variables
      include   'grasp.inc'

      real      bound
*   Internal variables
      real      sen_fac
      real      sen_fac_frost(max_part)
      real      sen_fac_pheno(max_part)
      real      sen_fac_water(max_part)
      integer   part

*   Constant values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_dm_senescence')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      call fill_real_array (dlt_dm_sen, 0.0, max_part)

      call grasp_sen_frost (sen_fac_frost)
      call grasp_sen_pheno (sen_fac_pheno)
      call grasp_sen_water (sen_fac_water)

*     Actual death is max of the various deaths sofar calculated.
      do 1000 part = 1, max_part

         sen_fac = max(
     :        sen_fac_frost(part),
     :        sen_fac_pheno(part),
     :        sen_fac_water(part))

*     Prevent death from being more than is actually present.
         sen_fac = bound(sen_fac, 0.0, 1.0)
         dlt_dm_sen(part) =  sen_fac * g_dm_green(part)

*     Save individual deaths for reporting
         dlt_dm_frost(part) =  sen_fac_frost(part) *
     :        g_dm_green(part)
         dlt_dm_pheno(part) =  sen_fac_pheno(part) *
     :        g_dm_green(part)
         dlt_dm_water(part) =  sen_fac_water(part) *
     :        g_dm_green(part)
1000  continue

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine grasp_sen_frost(sen_fac)
*     ===========================================================

*   Short description:

*     Calculates the potential death due to frost, in both leaf and
*     stem. The two death rates are later combined with other potential
*     death rates to give an actual death rate for the day.  The death
*     occurs between two temperatures, frost_start and
*     frost_kill. frost_start is the temperature at which killing begins,
*     and total death occurs at frost_kill.  The amount of death is varied
*     linearly between these limits.  The death does not actually take place
*     in this routine, only the rate of death is calculated. The actual
*     death needs to be combined with other potential death rates

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
*       090994 jngh specified and programmed

*   Calls:
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       sen_fac(*)     ! (OUTPUT)

*   Global variables
      include   'grasp.inc'

      real      divide
      real      bound
*   Internal variables
      real frost_effect

*   Constant values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_sen_frost')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      call fill_real_array (sen_fac, 0.0, max_part)

      if (g_mint .lt. c_frost_start) then
         frost_effect = divide (c_frost_start - g_mint,
     :        c_frost_start - c_frost_kill, 0.0)

         frost_effect = bound (frost_effect, 0.0, 1.0)

*        Proportion death to leaf and stem equally
         sen_fac(leaf) =  frost_effect
         sen_fac(stem) =  frost_effect
      else
                                ! Nothing
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine grasp_sen_pheno(sen_fac)
*     ===========================================================

*   Short description:
*     The background death rate is a function of
*     available soil moisture, with species-specific parameters, and is
*     apportioned in a species-specific way between leaf and stem.


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
*       090994 jngh specified and programmed

*   Calls:
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       sen_fac(*) ! (OUTPUT)

*   Global variables
      include   'grasp.inc'
      real      bound

*   Internal variables
      real      death_prop

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name =
     :     'grasp_sen_pheno')

*   Initial data values
*     none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      call fill_real_array (sen_fac, 0.0, max_part)

*     Background death of both leaf and stem, as influenced by soil moisture

      death_prop = c_death_slope *
     :     (1.0 - g_swi_total) + c_death_intercept

      sen_fac(leaf) = c_leaf_death_ratio * death_prop

      sen_fac(stem) = c_stem_death_ratio * death_prop

      sen_fac(leaf) = bound (sen_fac(leaf), 0.0, 1.0)
      sen_fac(stem) = bound (sen_fac(stem), 0.0, 1.0)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine grasp_sen_water (sen_fac)
*     ===========================================================

*   Short description:
*     Drought induced death is the application of a (species-specific)
*     limit on the amount of green matter allowed at the current soil
*     moisture. Anything above this MUST die.

*   Assumptions:
*     none

*   Notes:
*     none

*   Procedure attributes:
*      Version:         any hardware/fortran77
*      Extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       implicit none

*   Changes:
*       090994 jngh specified and programmed

*   Calls:
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       sen_fac(*)     ! (OUTPUT)

*   Global variables
      include   'grasp.inc'
      real       divide
      real       u_bound

*   Internal variables
      real       green_pool
      real       green_death
      real       grn_cov_mx
      real       max_pot_cov
      real       cfact
      real       leaf_prop
*   Constant values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_sen_water')

*   Initial data values
*     none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      call fill_real_array (sen_fac, 0.0, max_part)

C     Calculate maximum green allowed for this level of soil moisture

      grn_cov_mx = divide (g_swi_total, p_swi_fullgreen, 0.0)

c      write (*,*) 'grn_cov_mx = ', grn_cov_mx

      grn_cov_mx = u_bound (grn_cov_mx, 0.99)

      cfact = divide (p_yld_cover_slope,
     :                p_yld_cov50, 0.0)

      max_pot_cov = divide(-alog(1.0 - grn_cov_mx),
     :                     cfact, 0.0)

C     Limit cover to potential maximum
      green_pool = g_dm_green(leaf) + g_dm_green(stem)

      if (green_pool .gt. max_pot_cov) then
         green_death = green_pool - max_pot_cov

         leaf_prop = divide(
     :        g_dm_green(leaf),
     :        green_pool,
     :        0.0)

         sen_fac (leaf) = divide (
     :        leaf_prop * green_death,
     :        green_pool,
     :        0.0)

         sen_fac (stem) = divide (
     :        (1.0 - leaf_prop) * green_death,
     :        green_pool,
     :        0.0)

      else
         sen_fac (leaf) = 0.0
         sen_fac (stem) = 0.0
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine grasp_dm_production (dlt_dm, dlt_dm_transp,
     :     dlt_dm_photo, dlt_dm_regrow)
*     ===========================================================

*   Short description:
*       actual dm production (kg/ha)

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
*       090994 jngh specified and programmed

*   Calls:
*     grasp_radn_int
*     divide
*     find_layer_no
*     pop_routine
*     push_routine
*     grasp_transp_eff

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       dlt_dm         ! (Out) actual dry matter
                                ! production (kg/ha)
      real       dlt_dm_transp  ! (Out) potential dm by transp
      real       dlt_dm_photo   ! (Out) potential dm by photosynthesis
      real       dlt_dm_regrow  ! (Out) potential dm by regrowth

*   Global variables
      include   'grasp.inc'
      real       grasp_transp_eff
      real       grasp_sw_pot
      real       grasp_dm_photo
      real       grasp_dm_regrowth
      real       sum_real_array

*   Internal variables
      real       transpiration

*   Constant values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_dm_production')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

                                ! potential by mass flow
      if (p_uptake_source .eq. 'calc') then

                                ! By us
         transpiration = g_swi_total * grasp_sw_pot () 

      else if (p_uptake_source .eq. 'apsim') then

                                ! By swim
         transpiration = -1.0 * 
     :       sum_real_array(g_dlt_sw_dep, max_layer) 
      else

         transpiration = 0.0    ! ??
      endif

      dlt_dm_transp = transpiration * grasp_transp_eff ()

                                ! potential by photosynthesis
      dlt_dm_photo = grasp_dm_photo ()

                                ! use whichever is limiting
      dlt_dm = min(dlt_dm_transp, dlt_dm_photo)

                                ! potential by BA
      dlt_dm_regrow = grasp_dm_regrowth ()

      dlt_dm = max(dlt_dm, dlt_dm_regrow)

                                ! Limit by soil water index
      if (g_swi_total .le. p_swi_nogrow) then
          dlt_dm = 0.0
      else
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine grasp_dm_partition (dlt_dm_plant)
*     ===========================================================

*   Short description:
*       Partitions new dm (assimilate) between plant components (kg)

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
*     bound_check_real_array
*     bound_check_real_var
*     bound
*     int
*     pop_routine
*     push_routine
*     fill_real_array
*     stage_is_between
*     sum_between
*     sum_real_array

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       dlt_dm_plant (*) ! (OUTPUT) actual biomass partitioned
                                ! to plant parts (kg/ha)

*   Global variables
      include   'grasp.inc'
      real       sum_real_array ! function

*   Internal variables
      real       dlt_dm_plant_tot ! total of partitioned dm
      real       dm_tot

*   Constant values
      character  my_name*(*)    ! name of procedure
      parameter (my_name  = 'grasp_dm_partition')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

                                ! first, zero all plant component deltas
      call fill_real_array (dlt_dm_plant, 0.0, max_part)

      dm_tot = sum_real_array (g_dm_green, max_part) -
     :     g_dm_green(root)

      if (dm_tot .le. c_stem_thresh) then

         dlt_dm_plant(leaf) = g_dlt_dm

      else

         dlt_dm_plant(leaf) = c_frac_leaf2total * g_dlt_dm
         dlt_dm_plant(stem) = g_dlt_dm - dlt_dm_plant(leaf)

      endif

                                ! do quick mass balance check - roots
                                ! are not included
      dlt_dm_plant_tot = sum_real_array (dlt_dm_plant, max_part)
     :     - dlt_dm_plant(root)

      call bound_check_real_var (dlt_dm_plant_tot, 0.0, g_dlt_dm
     :     , 'dlt_dm_plant_tot mass balance')

                                ! check that deltas are in legal range
      call bound_check_real_array (dlt_dm_plant, 0.0, g_dlt_dm
     :     , 'dlt_dm_plant', max_part)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      real function grasp_dm_regrowth ()
*     ===========================================================

*   Short description:
*     growth by regrowth (basal area)

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
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments

*   Global variables
      include   'grasp.inc'
      real      grasp_nfact
      real      grasp_tfact
      real      grasp_rfact

*   Internal variables

*   Constant values
      character  my_name*(*)    ! name of procedure
      parameter (my_name  = 'grasp_dm_regrowth')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)
    
*     Potential growth from existing grass basal area
      grasp_dm_regrowth =
     :     p_pot_regrow *
     :     g_basal_area *
     :     grasp_nfact () *
     :     grasp_tfact () *
     :     grasp_rfact () *
     :     g_swi_total

      call pop_routine (my_name)
      return
      end

*     ===========================================================
      real function grasp_rfact ()
*     ===========================================================
*   Short description:
*     Index of radiation (ie lack of) stress

*   Assumptions:
*       none

*   Notes:
*       PdeV 6/4/98. This ignores radiaton shading from canopy. FIXME???

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
*     pop_routine
*     push_routine
*     divide

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments

*   Global variables
      include   'grasp.inc'
      real      divide

*   Internal variables

*   Constant values
      character  my_name*(*)    ! name of procedure
      parameter (my_name  = 'grasp_rfact')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

c     NB. straight from grasp - may be another method:
      grasp_rfact = 1.0 - exp(- divide
     :     (g_radn, p_rad_factor, 0.0) )

      call pop_routine (my_name)
      return
      end

*     ===========================================================
      subroutine grasp_nitrogen ()
*     ===========================================================

*   Short description:
*       simulate crop nitrogen processes.

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
*       none

*   Global variables
      include   'grasp.inc'

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'grasp_nitrogen')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

                                ! find N for soiln
      call grasp_N_uptake (g_N_uptake, g_dlt_No3)

      call pop_routine (my_name)
      return
      end

*     ===========================================================
      subroutine grasp_N_uptake ( N_uptake, dlt_No3 )
*     ===========================================================

*   Short description:
*       Find how much N is taken up by the sward as a function
*       of total transpiration for the season.

*   Assumptions:
*       none

*   Notes:
*      This needs looking at.

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
*     bound_check_real_var
*     bound
*     divide
*     pop_routine
*     push_routine
*     sum_real_array

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real      dlt_No3 (*)     ! (OUTPUT)
      real      N_uptake        ! (OUTPUT) N uptake for the season (kg)

*   Global variables
      include   'grasp.inc'
      real      sum_real_array
      real      bound
      real      divide
      integer   find_layer_no
      real      root_proportion

*   Internal variables
      real      dlt_N_uptake       ! todays N uptake
      real      N_avail(max_layer) ! N profile
      real      N_avail_sum        ! sum of N over profile
      real      max_N_sum          ! ditto

      integer   layer
      integer   deepest_layer

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'grasp_N_uptake')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      call fill_real_array (N_avail, 0.0, max_layer)
      deepest_layer = find_layer_no (g_root_depth,
     :     g_dlayer, max_layer)

      max_n_sum = 0.0
      do 500 layer = 1, deepest_layer

         max_n_sum = max_n_sum + p_max_n_avail(layer) *
     :        root_proportion (layer, g_dlayer,
     :        g_root_depth)
 500  continue

      N_uptake = c_residual_plant_N +
     :     c_N_uptk_per100 * g_acc_trans_for_N / 100.0

      N_uptake = bound (N_uptake, c_residual_plant_N, max_N_sum)
      
      dlt_N_uptake = bound (N_uptake - g_N_uptake, 0.0, N_uptake)


c     PdeV 7/96.
*     WARNING: this isn't present in grasp. If there isn't a N module
*     plugged in, g_No3 is impossibly high, and no limiting (apart from
*     gregs 25kg/ha/yr) occurs.
*     If there is a N module plugged in, grasp_nfact() NEEDS TO BE
*     CHANGED to know about it. FIXME!

*     Limit this to what is available
      do 1000 layer = 1, deepest_layer
         N_avail(layer) = g_No3(layer) - g_No3_min(layer)
 1000 continue

*     Adjust for root penetration into lowest layer
      N_avail(deepest_layer) =
     :     N_avail(deepest_layer) *
     :     root_proportion (deepest_layer, g_dlayer,
     :                      g_root_depth)

      N_avail_sum = sum_real_array (N_avail, max_layer)

      dlt_N_uptake = bound (dlt_N_uptake, 0.0, N_avail_sum)

      do 2000 layer = 1, deepest_layer
         dlt_No3(layer) = -1.0 * dlt_N_uptake *
     :        divide (N_avail(layer), N_avail_sum, 0.0)

 2000 continue

      call pop_routine (my_name)
      return
      end

*     ===========================================================
      real function grasp_nfact ()
*     ===========================================================

*   Short description:
*     Nitrogen is accumulated by plant in proportion to water
*     transpired. This is accumulated daily for up to a year, to
*     account for storage in the plant. In this simple model,
*     annual uptake is limited to the size of the soil store of
*     N (c_max_N_avail).

*     The rate of N uptake for transpired water is given as kg
*     of N per 100 mm of transpired water, to make the parameters
*     reasonable sized figures.

*     Requires two accumulated pools. Total water transpired by grass,
*     and total grass growth. These are reset to zero annually.

*     The total percentage N in sward is also limited by a species
*     parameter, as  is the percentage N when growth is at maximum,
*     and when growth is zero.

*     It is assumed that "other processes" are putting nitrogen back in to
*     the soil, without us really worrying about it.

*   Assumptions:
*       none

*   Notes:
*     *********Doesn't cooperate with soiln.************

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
*     divide
*     exp
*     fatal_error

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments

*   Global variables
      include   'const.inc'
      include   'grasp.inc'
      real       bound          ! function
      real       divide

*   Internal variables
      real       dm_N_conc

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'grasp_nfact')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

CPdeV. The names for these variables are screwy. c_N_conc_dm_crit is a soil
c      N property, but c_N_conc_dm_[min,max] are plant N properties. These names
c      need to be changed. FIXME!

                        ! if acc_growth is zero (ie reset yesterday),
                        ! then assume no N stress. this test is only
                        ! required for the first day after reset..
      if (g_acc_growth_for_N .gt. 0.00000001) then
        dm_N_conc = 100.0 * divide (g_N_uptake,
     :            g_acc_growth_for_N, 0.0)
        dm_N_conc = bound (dm_N_conc, 0.0, c_N_conc_dm_crit )
      else
        dm_N_conc = c_N_conc_dm_crit
      endif

      grasp_nfact = divide((dm_N_conc - c_N_conc_dm_min),
     :     (c_N_conc_dm_max - c_N_conc_dm_min), 0.0)

      grasp_nfact = bound (grasp_nfact, 0.0, 1.0)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine grasp_update ()
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
*      250894 jngh specified and programmed

*   Calls:
*     accumulate
*     add_real_array
*     bound
*     pop_routine
*     push_routine
*     remove
*     fill_real_array
*     sum_between
*     sum_real_array

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'grasp.inc'
      real       sum_real_array ! function

*   Internal variables
      integer    part

*   Constant values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_update')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

                                ! Update with deltas
      g_root_depth = g_root_depth + g_dlt_root_depth
      g_canopy_height = g_canopy_height + g_dlt_canopy_height

                                ! Plant dry matter.
      do 1000 part = 1, max_part
         g_dm_green(part) = g_dm_green(part) + g_dlt_dm_plant(part)
         g_dm_green(part) = g_dm_green(part) - g_dlt_dm_sen(part)
         g_dm_dead(part) = g_dm_dead(part) + g_dlt_dm_sen(part)
         g_dm_dead(part) = g_dm_dead(part) - g_detach(part)
         g_litter = g_litter + g_detach(part)
 1000    continue

      call grasp_add_residue (g_litter, c_litter_n * g_litter)

C     Accumulate soilevap + grass transpiration (evapotranspiration) for
C     basal area calculation. Note the obscureness of the date
C     condition. This is because of the wrap-around between years.

      if ((g_day_of_year .ge. c_day_start_summer) .or.
     :     (g_day_of_year .le. c_day_end_summer)) then
         g_acc_et_summer = g_acc_et_summer +
     :        g_es +
     :        (-1.0 * sum_real_array(g_dlt_sw_dep, max_layer))
      else
                                !nothing
      endif

      g_acc_trans_for_n = g_acc_trans_for_n +
     :     (-1.0 * sum_real_array(g_dlt_sw_dep, max_layer))

      g_acc_growth_for_N = g_acc_growth_for_N +
     :     sum_real_array(g_dlt_dm_plant, max_part)

      g_acc_growth = g_acc_growth +
     :     sum_real_array(g_dlt_dm_plant, max_layer)

cplp
c      write (*,*) 'g_acc_et_summer: ', g_acc_et_summer
c      write (*,*) 'dm_green:     ', g_dm_green
c      write (*,*) 'dlt_dm:       ', g_dlt_dm
c      write (*,*) 'dlt_dm_plant: ', g_dlt_dm_plant
c      write (*,*) 'dlt_dm_sen:   ', g_dlt_dm_sen
c      write (*,*) 'detach:       ', g_detach
c      write (*,*) 'growth_n:     ', g_acc_growth_for_n
c      write (*,*) 'trans_n:      ', g_acc_trans_for_n

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine grasp_plant_death ()
*     ===========================================================

*   Short description:
*      determine if crop dies
*      NB. even if all biomass is killed, grass will regrow by
*          basal area. This routine is an anachronism.

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
*     grasp_fail
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'grasp.inc'

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'grasp_plant_death')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine grasp_detachment (detach)
*     ===========================================================

*   Short description:
*     Grass detachment of dead leaf and stem. Background detachment
*     is just the "normal" way in which dead matter falls off
*     the plant.

*     The background detachment of leaf/stem is done as a proportion
*     of the amount of standing dead leaf/stem, the proportion
*     varying with season.

*     The detachment rates are limited so that no more can detach
*     than is actually present, ensuring that the dead leaf/stem
*     pools remain non-negative.

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

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real detach (*)           ! todays detachment for each plant part

*   Global variables
      include   'grasp.inc'
      real      bound

*   Internal variables
      integer   part
      logical   dry_season

*   Constant values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_detachment')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      call fill_real_array (detach, 0.0, max_part)

C     Proportions are different for wet season or dry season.
      dry_season = (g_day_of_year .ge. c_day_start_dry) .and.
     :     (g_day_of_year .le. c_day_start_wet)

      do 1000 part = 1, max_part
         if (dry_season) then
            detach(part) =
     :           c_detach_dryseason(part) *
     :           g_dm_dead(part)
         else
            detach(part) =
     :           c_detach_wetseason(part) *
     :           g_dm_dead(part)
         endif
         detach(part) = bound (detach(part),
     :                         0.0, g_dm_dead(part))
 1000 continue

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine grasp_store_report_vars ()
*     ===========================================================

*   Short description:
*     Collect totals of crop variables for output. Called before
*     update(), as most are functions of pool size.

*   Assumptions:
*       none

*   Notes:                        
*     There has to be a better way for this.

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
*     grasp_sw_pot
*     grasp_radn_cover
*     grasp_transp_cover
*     grasp_clothesline
*     grasp_rue_reduction
*     grasp_tfact
*     grasp_nfact

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'convert.inc'
      include   'grasp.inc'

      real       grasp_sw_pot
      real       grasp_radn_cover
      real       grasp_transp_cover
      real       grasp_total_cover
      real       grasp_clothesline
      real       grasp_rfact
      real       grasp_tfact
      real       grasp_nfact

*   Internal variables

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'grasp_store_report_vars')

*   Initial data values
*     none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      g_out_radn_cover = grasp_radn_cover ()
      g_out_transp_cover = grasp_transp_cover ()
      g_out_total_cover = grasp_total_cover ()

      g_out_clothesline = grasp_clothesline ()

      g_out_sw_pot = grasp_sw_pot ()
      g_out_sw_demand = grasp_sw_pot () * g_swi_total

      g_out_rfact = grasp_rfact ()
      g_out_nfact = grasp_nfact ()
      g_out_tfact = grasp_tfact ()

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      real function grasp_total_cover ()
*     ===========================================================

*   Short description:
*      Surface cover for soilwat. Total green and dead cover.

*   Assumptions:
*       none

*   Notes:
*

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
*       none

*   Global variables
      include   'grasp.inc'
      real      bound
      real      sum_real_array

*   Internal variables
      real       green_pool
      real       dead_pool
      real       green_cover
      real       dead_cover

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'grasp_total_cover')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      green_pool = sum_real_array(g_dm_green, max_part) -
     &                 g_dm_green(root)

      dead_pool = sum_real_array(g_dm_dead, max_part) -
     &                 g_dm_dead(root)

      if (green_pool .gt. 1.0) then
          green_cover = 1.0 -
     &          exp(green_pool *
     &          (-p_yld_cover_slope / p_yld_COV50))
      else
          green_cover = 0.0
      endif

      dead_cover = bound (c_dead_cover_slope * dead_pool
     &                    , 0.0, 1.0)

c     Beers law:??
      grasp_total_cover = 1.0 - (1.0 - green_cover) *
     &          (1.0 - dead_cover)

c     Bound to reasonable values:
      grasp_total_cover =
     &     bound(grasp_total_cover, 0.0, 1.0)

      call pop_routine (my_name)
      return
      end

*     ===========================================================
      subroutine grasp_event ()
*     ===========================================================

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
*                       implicit none

*   Changes:
*     010994 jngh specified and programmed

*   Calls:
*     caldat
*     divide
*     find_layer_no
*     get_a_word
*     l_bound
*     on_day_of
*     stage_is_between
*     pop_routine
*     push_routine
*     report_event
*     sum_real_array
*     sum_between
*     write_string

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'const.inc'            ! new_line, lu_scr_sum, blank,
                                       !   lu_scr_sum
      include   'convert.inc'          ! gm2kg, sm2ha, sm2smm
      include   'grasp.inc'
      real      sum_real_array

*   Internal variables

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'grasp_event')

*   Initial data values
*       none
* --------------------- Executable code section ----------------------

      call push_routine (my_name)

*     Reset several accumulators at particular times of year. These
*     resets are usually the result of kludges. A goal should be to
*     eliminate this routine.

*     This is called after everything else, and this fact is important
*     in some cases, e.g. the grass_basal_area accumulators. We reset
*     after the end of the summer growth period.

      if (g_day_of_year .eq. c_growth_for_n_reset) then
         g_acc_growth_for_N = -1.0 * sum_real_array(g_dlt_dm_plant,
     :                                       max_part)
      endif

      if (g_day_of_year .eq. c_trans_for_n_reset) then
         g_acc_trans_for_N = -1.0 * sum_real_array(g_dlt_sw_dep,
     :                                             max_layer)
      endif

      if (g_day_of_year .eq. c_acc_growth_reset) then
         g_acc_growth = 0.0
      endif
      if (g_day_of_year .eq. c_acc_et_reset) then
         g_acc_ET_summer = 0.0
      endif

      if (g_day_of_year .eq. c_day_end_summer) then

         g_acc_growth_last_summer = g_acc_et_summer *
     :        c_et_use_efficiency

      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine grasp_check_sw ()
*     ===========================================================

*   Short description:
*     checks validity of soil water parameters for a soil profile layer

*   Assumptions:
*     none

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
*     010994 jngh specified and programmed

*   Calls:
*     divide
*     warning_error

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'const.inc'     ! err_internal
      include   'grasp.inc'
      real       divide         ! function

*   Internal variables
      real       dul            ! drained upper limit water content
                                !   of layer (mm water/mm soil)
      character  err_messg*200  ! error message
      integer    layer          ! layer number
      real       ll             ! lower limit water content
                                !   of layer (mm water/mm soil)
      real       sw             ! soil water content of layer l
                                !   (mm water/mm soil)

*   Constant values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_check_sw')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      do 2000 layer = 1, g_num_layers

         sw = divide (g_sw_dep(layer), g_dlayer(layer), 0.0)
         dul = divide (g_dul_dep(layer), g_dlayer(layer), 0.0)
         ll = divide (g_ll_dep(layer), g_dlayer(layer), 0.0)

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
      subroutine grasp_zero_variables ()
*     ===========================================================

*   Short description:
*       zero grasp_ variables & arrays

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
*     fill_real_array

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'grasp.inc'

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)    ! name of procedure
      parameter (my_name  = 'grasp_zero_variables')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)
                              
          !  zero pools etc.

      call grasp_zero_daily_variables ()

      call fill_real_array (g_ll_dep, 0.0, max_layer)
      call fill_real_array (p_max_n_avail, 0.0, max_layer)
      g_num_layers = 0
      g_canopy_height = 0.0
      g_previous_stage = 0.0
      g_root_depth = 0.0
      g_acc_growth_last_summer = 0.0
      g_acc_growth = 0.0
      g_acc_et_summer = 0.0

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine grasp_zero_daily_variables ()
*     ===========================================================

*   Short description:
*       zero grasp daily variables & arrays

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
*     fill_real_array

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'grasp.inc'

*   Internal variables
*       none

*   Constant values
      character  my_name*(*)    ! name of procedure
      parameter (my_name  = 'grasp_zero_daily_variables')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

                                !  zero pool deltas etc.
      call fill_real_array (g_dlt_dm_plant, 0.0, max_part)
      call fill_real_array (g_detach, 0.0, max_part)
      call fill_real_array (g_dlt_No3, 0.0, max_layer)
      call fill_real_array (g_dlt_sw_dep, 0.0, max_layer)
      call fill_real_array (g_bd, 0.0, max_layer)
      call fill_real_array (g_layer_fract, 1.0, max_layer) ! ie. no change

      g_soil_loss = 0.0
      g_dlt_canopy_height = 0.0
      g_dlt_dm = 0.0
      g_dlt_root_depth = 0.0
      g_dlt_stage = 0.0
      g_litter = 0.0
      g_tree_sw_demand = 0.0

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine grasp_init ()
*     ===========================================================

*   Short description:
*       model initialisation

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
*     lastnb
*     pop_routine
*     push_routine
*     report_event
*     grasp_read_constants
*     grasp_version
*     write_string

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'const.inc'     ! new_line, lu_scr_sum, blank
                                ! lu_scr_sum
      include   'grasp.inc'
      integer    count_of_real_vals

      character  grasp_version*20 ! function

*   Internal variables
      integer layer
      integer num_layers

*   Constant values

      character  my_name*(*)    ! name of procedure
      parameter (my_name  = 'grasp_init')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      call report_event (' Initialising, Version : '
     :     // grasp_version ())

                                ! initialize crop variables
      call grasp_read_constants ()

                                ! pull in soil properties for
                                ! ll_dep calculation
      call grasp_get_other_variables ()

                                ! parameter file
      call grasp_read_parameters ()

                                ! Initial conditions
      g_dm_green(root) = p_dm_green_root_init
      g_dm_green(stem) = p_dm_green_stem_init
      g_dm_green(leaf) = p_dm_green_leaf_init

      g_dm_dead(root) = p_dm_dead_root_init
      g_dm_dead(stem) = p_dm_dead_stem_init
      g_dm_dead(leaf) = p_dm_dead_leaf_init

      g_basal_area = p_basal_area_init
      g_root_depth = p_root_depth_init

      g_acc_trans_for_n = p_acc_trans_for_n_init
      g_acc_growth_for_n = p_acc_growth_for_n_init

      g_current_stage = real (establishment)
      g_crop_status = crop_alive

      num_layers = count_of_real_vals (g_dlayer, max_layer)
      do 100 layer = 1, num_layers
         g_rlv(layer) = p_kl(layer) * p_kl2rlv
100   continue

                                ! write summary
      call grasp_write_summary ()

      call pop_routine (my_name)
      return      
      end

*     ===========================================================
      subroutine grasp_add_residue (dlt_residue_weight, dlt_residue_N)
*     ===========================================================

*   Short description:
*       add residue to residue pool

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
*       220794 jngh specified and programmed

*   Calls:
*     message_send
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       dlt_residue_weight ! (INPUT) new surface residue (kg/ha)
      real       dlt_residue_N  ! (INPUT) new surface residue N (kg/ha)

*   Global variables
      include   'const.inc'     ! global_ative
      include   'grasp.inc'

*   Internal variables
*     none

*   Constant values
      character  my_name*(*)    ! name of procedure
      parameter (my_name  = 'grasp_add_residue')

*   Initial data values
*     none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      if (dlt_residue_weight .gt. 0.0) then
c         write (*,*) 'grasp is adding residue :'
c         write (*,*) ' type = ', p_crop_type
c         write (*,*) ' wt = ', dlt_residue_weight
c         write (*,*) ' n = ', dlt_residue_N

                                  ! send out surface residue
         call new_postbox ()
         call post_char_var ('dlt_residue_type',
     :        '()', p_crop_type)
         call post_real_var ('dlt_residue_wt',
     :        '(kg/ha)', dlt_residue_weight)
         call post_real_var ('dlt_residue_n',
     :        '(kg/ha)', dlt_residue_n)

         call message_send_immediate (unknown_module,
     :        'add_residue', blank)

         call delete_postbox ()

      else
                                ! no surface residue
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine grasp_save_yesterday ()
*     ===========================================================

*   Short description:
*       save yesterdays biomass for balance check later.

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
*       220794 jngh specified and programmed

*   Calls:
*     sum_real_array
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments

*   Global variables
      include   'grasp.inc'
      real      sum_real_array

*   Internal variables

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'grasp_save_yesterday')

*   Initial data values
*     none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

C     Save the total biomass before we do anything to it. This is used
C     only to do a mass balance check at the end of the day.

      g_biomass_yesterday = sum_real_array (g_dm_green, max_part) +
     :     sum_real_array (g_dm_dead, max_part)
c     write (*,*) 'biomass = ', g_biomass_yesterday

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine grasp_balance_check ()
*     ===========================================================

*   Short description:
*     Check the mass balance of biomass. We have saved yesterday's
*     total biomass (at start of routine grass_growth). Today's total
*     biomass should be equal to yesterday's plus/minus all
*     incomings/outgoings.

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
*       220794 jngh specified and programmed

*   Calls:
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments

*   Global variables
      include   'const.inc'
      include   'grasp.inc'
      real      sum_real_array

*   Internal variables
      real total_biomass        ! tsdm after today's processing
      real yesterday            ! yesterdays tsdm plus todays deltas
      real biomass_check        ! difference between these
      character string*100

*   Constant values
      character  my_name*(*)    ! name of procedure
      parameter (my_name  = 'grasp_balance_check')

*   Initial data values
*     none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      total_biomass = sum_real_array (g_dm_green, max_part) +
     :     sum_real_array (g_dm_dead, max_part)

      yesterday = g_biomass_yesterday +
     :     sum_real_array (g_dlt_dm_plant, max_part) -
     :     g_litter

      biomass_check = abs(yesterday - total_biomass)

      if (biomass_check .gt. 0.01) then
         write(string, '(a,i3,a,i4)') ' Day: ',
     :        g_day_of_year, '/', g_year
         call write_string(lu_scr_sum, string)

         write(string, '(a)') ' Mass balance check Error'
         call write_string(lu_scr_sum, string)

         write(string, '(a,f12.4)') ' Yesterday''s biomass = ',
     :        g_biomass_yesterday
         call write_string(lu_scr_sum, string)

         write(string, '(a,f12.4)') ' Today''s biomass     = ',
     :        total_biomass
         call write_string(lu_scr_sum, string)

         write(string, '(a,a,g10.4)')
     :        ' Difference between today''s ',
     :        '& (yesterday''s +/- rates) = ', biomass_check
         call write_string(lu_scr_sum, string)

         write(string, '(a)') ' Pools:'
         call write_string(lu_scr_sum, string)

         write(string, '(a,f12.4)') ' green leaf = ',
     :        g_dm_green(leaf)
         call write_string(lu_scr_sum, string)

         write(string, '(a,f12.4)') ' green stem = ',
     :        g_dm_green(stem)
         call write_string(lu_scr_sum, string)

         write(string, '(a,f12.4)') ' dead leaf = ',
     :        g_dm_dead(leaf)
         call write_string(lu_scr_sum, string)

         write(string, '(a,f12.4)') ' dead stem = ',
     :        g_dm_dead(stem)
         call write_string(lu_scr_sum, string)

         write(string, '(a,f12.4)') ' litter = ',
     :        g_litter
         call write_string(lu_scr_sum, string)

         write(string, '(a)') ' Deltas:'
         call write_string(lu_scr_sum, string)

         write(string, '(a,f12.4)') ' delta green leaf = ',
     :        g_dlt_dm_plant(leaf)
         call write_string(lu_scr_sum, string)

         write(string, '(a,f12.4)') ' delta green stem = ',
     :        g_dlt_dm_plant(stem)
         call write_string(lu_scr_sum, string)

         write(string, '(a,f12.4)') ' delta dead leaf = ',
     :        g_dlt_dm_sen(leaf)
         call write_string(lu_scr_sum, string)

         write(string, '(a,f12.4)') ' delta dead stem = ',
     :        g_dlt_dm_sen(stem)
         call write_string(lu_scr_sum, string)

         write(string, '(a,f12.4)') ' detached leaf = ',
     :        g_detach(leaf)
         call write_string(lu_scr_sum, string)

         write(string, '(a,f12.4)') ' detached stem = ',
     :        g_detach(stem)
         call write_string(lu_scr_sum, string)

         call fatal_error(err_internal, ' Mass Balance Error')
      endif

C     Check that none of the pools is negative
      if ((g_dm_green(leaf) .lt. 0.0) .or.
     :     (g_dm_green(stem) .lt. 0.0) .or.
     :     (g_dm_dead(leaf) .lt. 0.0) .or.
     :     (g_dm_dead(stem) .lt. 0.0) .or.
     :     (g_litter .lt. 0.0)) then
         write(string, '(a)') ' Negative pool error'
         call write_string(lu_scr_sum, string)

         write(string, '(a,i3,a,i4)') 'Day = ',
     :        g_day_of_year, ', Year = ', g_year
         call write_string(lu_scr_sum, string)

         write(string, '(a,f12.4)') 'green leaf = ',
     :        g_dm_green(leaf)
         call write_string(lu_scr_sum, string)

         write(string, '(a,f12.4)') 'green stem = ',
     :        g_dm_green(stem)
         call write_string(lu_scr_sum, string)

         write(string, '(a,f12.4)') 'dead leaf = ',
     :        g_dm_dead(leaf)
         call write_string(lu_scr_sum, string)

         write(string, '(a,f12.4)') 'dead stem = ',
     :        g_dm_dead(stem)
         call write_string(lu_scr_sum, string)

         write(string, '(a,f12.4)') 'litter = ',
     :        g_litter
         call write_string(lu_scr_sum, string)

         write(string, '(a)') 'Deltas:'
         call write_string(lu_scr_sum, string)

         write(string, '(a,f12.4)') 'delta green leaf = ',
     :        g_dlt_dm_plant(leaf)
         call write_string(lu_scr_sum, string)

         write(string, '(a,f12.4)') 'delta green stem = ',
     :        g_dlt_dm_plant(stem)
         call write_string(lu_scr_sum, string)

         write(string, '(a,f12.4)') 'delta dead leaf = ',
     :        g_dlt_dm_sen(leaf)
         call write_string(lu_scr_sum, string)

         write(string, '(a,f12.4)') 'delta dead stem = ',
     :        g_dlt_dm_sen(stem)
         call write_string(lu_scr_sum, string)

         write(string, '(a,f12.4)') 'detached leaf = ',
     :        g_detach(leaf)
         call write_string(lu_scr_sum, string)

         write(string, '(a,f12.4)') 'detached stem = ',
     :        g_detach(stem)
         call write_string(lu_scr_sum, string)

cplp         call fatal_error(err_internal, 'Negative Pool Error')
      endif

      call pop_routine (my_name)
      return
      end

*     ================================================================
      real function grasp_radn_cover ()
*     ================================================================

*   Short description:
*      cover for radiation purposes

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
*     010994 jngh specified and programmed

*   Calls:

* ----------------------- Declaration section ------------------------

      implicit none                   

*   Subroutine arguments
*      none

*   Global variables
      include   'const.inc'
      include   'grasp.inc'
      real      sum_real_array  ! function
      real      divide

*   Internal variables
      real      green_biomass
      real      factor

*   Constant values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_radn_cover')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      green_biomass = sum_real_array(g_dm_green, max_part) -
     :        g_dm_green(root)

      factor = divide(p_yld_cover_slope, p_yld_fpc50, 0.0)

      grasp_radn_cover = 1.0 -
     :      exp(-factor * green_biomass)

cpdev  bound required..

      call pop_routine (my_name)
      return
      end

*     ================================================================
      real function grasp_transp_cover ()
*     ================================================================

*   Short description:
*      cover for transpiration purposes

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
*     010994 jngh specified and programmed

*   Calls:

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*      none

*   Global variables
      include   'const.inc'
      include   'grasp.inc'
      real      sum_real_array  ! function
      real      divide

*   Internal variables
      real green_biomass
      real factor

*   Constant values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_transp_cover')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      green_biomass = sum_real_array(g_dm_green, max_part) -
     :        g_dm_green(root)

      factor = divide(p_yld_cover_slope, p_yld_cov50, 0.0)

      grasp_transp_cover = 1.0 - exp(-factor * green_biomass)

cpdev  bound required?..

      call pop_routine (my_name)
      return
      end

*     ================================================================
      subroutine grasp_get_other_variables ()
*     ================================================================

*   Short description:
*     get the value/s of variables/arrays from other modules.

*   Assumptions:
*     assumes variable has the following format
*     <variable_name> = <variable_value/s> (<units>)

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
*     divide
*     get_integer_var
*     get_real_var
*     get_real_array
*     get_real_array_optional

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
*      none

*   Global variables
      include   'const.inc'
      include   'grasp.inc'
      real       divide         ! function
      real       grasp_vpd
      real       sum_real_array

*   Internal variables
      integer    layer          ! layer number
      integer    numvals        ! number of values put into array
      real       temp(max_layer) ! temporaries
      real       value
      character  module_name*(max_module_name_size)

*   Constant values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_get_other_variables')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

                                ! date
      call get_integer_var (unknown_module, 'day', '()'
     :     , g_day_of_year, numvals, 1, 366)

      call get_integer_var (unknown_module, 'year', '()'
     :     , g_year, numvals, c_year_lb, c_year_ub)

                                ! canopy
      call get_current_module (module_name)
      call get_real_var_optional (unknown_module,
     :     'fr_intc_radn_'//module_name,
     :     '()'
     :     , g_fr_intc_radn,
     :     numvals, 0.0, 1.0)
      if (numvals .eq. 0) then
         g_fr_intc_radn = 0.0
      else
      endif
                                ! climate
      call get_real_var (unknown_module, 'maxt', '(oC)'
     :     , g_maxt, numvals, c_maxt_lb, c_maxt_ub)

      call get_real_var (unknown_module, 'mint', '(oC)'
     :     , g_mint, numvals, c_mint_lb, c_mint_ub)

      call get_real_var (unknown_module, 'radn', '(Mj/m^2)'
     :     , g_radn, numvals, c_radn_lb, c_radn_ub)

      call get_real_var_optional (unknown_module, 'pan', '(mm)'
     :     , g_pan, numvals, c_pan_lb, c_pan_ub)
      if (numvals .le. 0) then
         call get_real_var (unknown_module, 'eo', '(mm)'
     :        , value, numvals, c_pan_lb, c_pan_ub)
         g_pan = value
      else
                                ! nothing
      endif

      call get_real_var_optional (unknown_module, 'vpd', '(hPa)'
     :     , g_vpd, numvals, c_vpd_lb, c_vpd_ub)
      if (numvals .le. 0) then
         g_vpd = grasp_vpd ()  ! Must have todays maxt, mint for this
      else
                                ! nothing
      endif

      call get_real_var (unknown_module, 'es', '(mm)'
     :     , g_es, numvals, c_es_lb, c_es_ub)

                                ! soil profile and soil water
      call get_real_array (unknown_module, 'dlayer', max_layer
     :     , '(mm)', temp, numvals, c_dlayer_lb, c_dlayer_ub)

      if (g_num_layers.eq.0) then
                                ! we assume dlayer hasn't been
                                ! initialised yet.
         call add_real_array (temp, g_dlayer, numvals)
         g_num_layers = numvals

      else
                                ! dlayer may be changed from its
                                ! last setting (ie eroded) so estimate what
                                ! ll should be from the new profile:
         do 1000 layer = 1, numvals

            g_ll_dep(layer) = divide (g_ll_dep(layer)
     :           , g_dlayer(layer), 0.0) * temp(layer)
            g_layer_fract(layer) = divide (temp(layer),
     :           g_dlayer(layer), 0.0)
            g_dlayer(layer) = temp(layer)

1000     continue
         g_num_layers = numvals
      endif

      value = sum_real_array (g_dlayer, max_layer)
      if (g_root_depth .gt. value) then
         g_root_depth = value
         call warning_error (err_internal, 
     :              'roots exceeded profile depth')
      endif

      call get_real_array (unknown_module, 
     :     'bd', max_layer
     :     , '(mm)', g_bd, numvals, 0.0, 10.0)

      call get_real_array (unknown_module, 'dul_dep', max_layer
     :     , '(mm)', g_dul_dep, numvals, c_dul_dep_lb, c_dul_dep_ub)

      call get_real_array (unknown_module, 'sw_dep', max_layer
     :     , '(mm)', g_sw_dep, numvals, c_sw_dep_lb, c_sw_dep_ub)

      call get_real_array_optional (unknown_module, 'no3', max_layer
     :     ,  '(kg/ha)', g_No3, numvals, c_NO3_lb, c_NO3_ub)
      if (numvals.eq.0) then
            ! we have no N supply - make non-limiting.
         call fill_real_array (g_No3, 10000.0, g_num_layers)
      else
! PdeV 6/4/98 - This is probably a good idea.
!         call warning_error (err_internal, 
!     :                   'Grasp does not work sensibly with soiln.')
      endif

      call fill_real_array (g_No3_min, c_No3_min_lb, max_layer)
      call get_real_array_optional (unknown_module, 'no3_min', max_layer
     :     ,  '(kg/ha)', g_No3_min, numvals, c_No3_min_lb, c_No3_min_ub)

                                !  For profile erosion
      call get_real_var_optional (unknown_module
     :     ,'soil_loss', '(t/ha)'
     :     ,g_soil_loss, numvals, 0.0, 50000.0)

      call get_real_var_optional ('tree', 'sw_demand', '(mm)'
     :     , g_tree_sw_demand, numvals, c_tree_sw_lb, c_tree_sw_ub)

      call pop_routine (my_name)
      return
      end
*     ================================================================
      subroutine grasp_set_other_variables ()
*     ================================================================

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
*                       implicit none

*   Changes:
*     010994 jngh specified and programmed

*   Calls:
*      count_of_real_vals
*      fill_real_array

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*      none

*   Global variables
      include   'const.inc'
      include   'grasp.inc'
      integer   count_of_real_vals                 ! function
      real      sum_real_array

*   Internal variables
      integer    num_layers            ! number of layers

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'grasp_set_other_variables')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      num_layers = count_of_real_vals (g_dlayer, max_layer)

      call new_postbox ()

!     If there isn't an N module plugged in, then sending out
!     N uptake fills the summary file with needless garbage.
!     However, this check is a bit of a fudge.
      if (sum_real_array(g_No3, max_layer) .lt. 10000.0) then
        call post_real_array( 'dlt_no3',
     :     '(kg/ha)',
     :     g_dlt_No3, num_layers)

        call message_send_immediate( unknown_module,
     :     mes_set_variable,
     :     'dlt_no3')
      else
                                          ! No N module runing
      endif

      if (p_uptake_source .eq. 'calc') then

         call post_real_array ('dlt_sw_dep',
     :        '(mm)',
     :        g_dlt_sw_dep, num_layers)

         call message_send_immediate( unknown_module,
     :        mes_set_variable,
     :        'dlt_sw_dep')
      else
      endif

      call delete_postbox ()

      call pop_routine (my_name)
      return
      end
*     ===============================================================
      subroutine grasp_set_my_variable (Variable_name)
*     ===============================================================

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
*                       implicit none

*   Changes:
*      290393 jngh

*   Calls:
*      none

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      character  Variable_name*(*)     ! (INPUT) Variable name to search for

*   Global variables
      include  'grasp.inc'
      integer  count_of_real_vals
      real     divide
      real     bound

*   Internal variables
      real     temp
      real     frac_leaf
      integer  layer
      integer  num_layers
      integer  numvals

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'grasp_set_my_variable')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      if (variable_name .eq. 'green_leaf') then
         call collect_real_var ('green_leaf', '(kg/ha)'
     :                               , temp, numvals
     :                               , 0.0, 10000.0)
         g_dm_green(leaf) = temp

      elseif (variable_name .eq. 'dlt_green_leaf') then
         call collect_real_var ('dlt_green_leaf', '(kg/ha)'
     :                               , temp, numvals
     :                               , -10000.0, 10000.0)
         g_dm_green(leaf) = g_dm_green(leaf) + temp


      elseif (variable_name .eq. 'green_stem') then
         call collect_real_var ('green_stem', '(kg/ha)'
     :                               , temp, numvals
     :                               , 0.0, 10000.0)
         g_dm_green(stem) = temp

      elseif (variable_name .eq. 'dlt_green_stem') then
         call collect_real_var ('dlt_green_stem', '(kg/ha)'
     :                               , temp, numvals
     :                               , -10000.0, 10000.0)
         g_dm_green(stem) = g_dm_green(stem) + temp

      elseif (variable_name .eq. 'green_root') then
         call collect_real_var ('green_root', '(kg/ha)'
     :                               , temp, numvals
     :                               , 0.0, 10000.0)
         g_dm_green(root) = temp

      elseif (variable_name .eq. 'dlt_green_root') then
         call collect_real_var ('dlt_green_root', '(kg/ha)'
     :                               , temp, numvals
     :                               , 10000.0, 10000.0)
         g_dm_green(root) = g_dm_green(root) + temp

      elseif (variable_name .eq. 'dead_leaf') then
         call collect_real_var ('dead_leaf', '(kg/ha)'
     :                               , temp, numvals
     :                               , 0.0, 10000.0)
         g_dm_dead(leaf) = temp

      elseif (variable_name .eq. 'dlt_dead_leaf') then
         call collect_real_var ('dlt_dead_leaf', '(kg/ha)'
     :                               , temp, numvals
     :                               , -10000.0, 10000.0)
         g_dm_dead(leaf) = g_dm_dead(leaf) + temp

      elseif (variable_name .eq. 'dead_stem') then
         call collect_real_var ('dead_stem', '(kg/ha)'
     :                               , temp, numvals
     :                               , 0.0, 10000.0)
         g_dm_dead(stem) = temp

      elseif (variable_name .eq. 'dlt_dead_stem') then
         call collect_real_var ('dlt_dead_stem', '(kg/ha)'
     :                               , temp, numvals
     :                               , -10000.0, 10000.0)
         g_dm_dead(stem) = g_dm_dead(stem) + temp

      elseif (variable_name .eq. 'dead_root') then
         call collect_real_var ('dead_root', '(kg/ha)'
     :                               , temp, numvals
     :                               , 0.0, 10000.0)
         g_dm_dead(root) = temp

      elseif (variable_name .eq. 'dlt_dead_root') then
         call collect_real_var ('dlt_dead_root', '(kg/ha)'
     :                               , temp, numvals
     :                               , -10000.0, 10000.0)
         g_dm_dead(root) = g_dm_dead(root) + temp

      elseif (variable_name .eq. 'green_pool') then
         call collect_real_var ('green_pool', '(kg/ha)'
     :                               , temp, numvals
     :                               , 0.0, 10000.0)
         frac_leaf = divide (g_dm_green(leaf),
     :        g_dm_green(leaf) + g_dm_green(stem), 0.5)
         frac_leaf = bound (frac_leaf, 0.0, 1.0)
         g_dm_green(leaf) = temp * frac_leaf
         g_dm_green(stem) = temp * (1.0 - frac_leaf)

      elseif (variable_name .eq. 'dlt_green_pool') then
         call collect_real_var ('dlt_green_pool', '(kg/ha)'
     :                               , temp, numvals
     :                               , -10000.0, 10000.0)
         frac_leaf = divide (g_dm_green(leaf),
     :        g_dm_green(leaf) + g_dm_green(stem), 0.5)
         frac_leaf = bound (frac_leaf, 0.0, 1.0)
         g_dm_green(leaf) = g_dm_green(leaf) + temp * frac_leaf
         g_dm_green(stem) = g_dm_green(stem) +
     :                        temp * (1.0 - frac_leaf)

      elseif (variable_name .eq. 'dead_pool') then
         call collect_real_var ('dead_pool', '(kg/ha)'
     :                               , temp, numvals
     :                               , 0.0, 10000.0)
         frac_leaf = divide (g_dm_dead(leaf),
     :        g_dm_dead(leaf) + g_dm_dead(stem), 0.5)
         frac_leaf = bound (frac_leaf, 0.0, 1.0)
         g_dm_dead(leaf) = temp * frac_leaf
         g_dm_dead(stem) = temp * (1.0 - frac_leaf)

      elseif (variable_name .eq. 'dlt_dead_pool') then
         call collect_real_var ('dlt_dead_pool', '(kg/ha)'
     :                               , temp, numvals
     :                               , -10000.0, 10000.0)
         frac_leaf = divide (g_dm_dead(leaf),
     :        g_dm_dead(leaf) + g_dm_dead(stem), 0.5)
         frac_leaf = bound (frac_leaf, 0.0, 1.0)
         g_dm_dead(leaf) = g_dm_dead(leaf) + temp * frac_leaf
         g_dm_dead(stem) = g_dm_dead(leaf) +
     :                        temp * (1.0 - frac_leaf)

      elseif (variable_name .eq. 'basal_area') then
         call collect_real_var ('basal_area', '(%)'
     :                               , temp, numvals
     :                               , 0.0, 100.0)
         g_basal_area = temp

      elseif (variable_name .eq. 'root_depth') then
         call collect_real_var ('root_depth', '(mm)'
     :                               , temp, numvals
     :                               , 0.0, 100.0)
         g_root_depth = temp

      elseif (variable_name .eq. 'height_1000kg') then
         call collect_real_var ('height_1000kg', '(mm)'
     :        , temp, numvals
     :        , 0.0, 100.0)
         c_height_1000kg = temp

      elseif (variable_name .eq. 'kl2rlv') then
         call collect_real_var ('kl2rlv', '()'
     :        , p_kl2rlv, numvals
     :        , 0.0, 10000.0)
         num_layers = count_of_real_vals (g_dlayer, max_layer)
         do 100 layer = 1, num_layers
            g_rlv(layer) = p_kl(layer) * p_kl2rlv
 100     continue

      else
         call message_unused ()

      endif


      call pop_routine (my_name)
      return
      end
*     ================================================================
      subroutine grasp_send_my_variable (variable_name)
*     ================================================================

*   Short description:
*      return the value of a variable requested by other modules.

*   Assumptions:
*      none

*   Notes:
*      This routine is why APSIM is so slow. There has to be a better way.

*   Procedure attributes:
*      Version:         any hardware/fortran77
*      Extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       implicit none

*   Changes:
*      string_concat

*   Calls:
*     count_of_real_vals
*     divide
*     find_layer_no
*     l_bound
*     stage_is_between
*     pop_routine
*     push_routine
*     sum_real_array
*     sum_between

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      character variable_name*(*)      ! (INPUT) variable name to search for

*   Global variables
      include 'convert.inc'            ! gm2kg, sm2ha, mm2cm, cmm2cc
      include 'grasp.inc'
      real       grasp_transp_eff
      real       grasp_vpd_hgt_ndx
      real       grasp_total_cover
      integer    count_of_real_vals    ! function
      real       sum_real_array
      real       root_proportion
      integer    find_layer_no

*   Internal variables
c     real       act_n_up       ! cumulative total N uptake by plant
                                !   (kg/ha)
      integer    deepest_layer  ! deepest layer in which the roots are
                                ! growing
      real       biomass
      integer    num_layers     ! number of layers in profile
      character  stage_name*30  ! name of stage
      integer    stage_no       ! current stage no.
c     real       No3_tot               ! total No3 in the root profile (kg/ha)
c     real       N_demand              ! sum N demand for plant parts (g/plant)
      real       temp(max_layer)
      integer    layer          ! Loop counter
*   Constant values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_send_my_variable')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)


                                ! management
      if (variable_name .eq. 'crop_type') then
          call respond2get_char_var (
     :        'crop_type',
     :        '()', p_crop_type)


      elseif (variable_name .eq. 'crop_status') then
         call respond2get_integer_var (
     :        'crop_status',
     :        '()', g_crop_status)

      elseif (variable_name .eq. 'stage') then
         call respond2get_real_var (
     :        'stage',
     :        '()', g_current_stage)

      elseif (variable_name .eq. 'stage_code') then
         stage_no = int (g_current_stage)
         call respond2get_real_var (
     :        'stage_code',
     :        '()', p_stage_code_list(stage_no))

      elseif (variable_name .eq. 'stage_name') then
         stage_no = int (g_current_stage)
         call get_a_word (p_stage_names, stage_no, stage_name)
         call respond2get_char_var (
     :        'stage_name',
     :        '()', stage_name)

                                ! plant biomass
      elseif (variable_name .eq. 'height') then
         call respond2get_real_var (
     :        'height',
     :        '(mm)', g_canopy_height)

! Covers.
! total          green + dead
! green          green                  (radiation intercepting)
! transpiring    green                  (internal)

      elseif (variable_name .eq. 'cover_tot') then
         call respond2get_real_var (
     :        'cover_tot',
     :        '()', grasp_total_cover() )

cpdev. One of these is right. I don't know which...
      elseif (variable_name .eq. 'green_cover') then
         call respond2get_real_var (
     :        'green_cover',
     :        '()', g_out_radn_cover)

      elseif (variable_name .eq. 'cover_green') then
         call respond2get_real_var (
     :        'cover_green',
     :        '()', g_out_radn_cover)

      elseif (variable_name .eq. 'radn_cover') then
         call respond2get_real_var (
     :        'radn_cover',
     :        '()', g_out_radn_cover)

      elseif (variable_name .eq. 'transp_cover') then
         call respond2get_real_var (
     :        'transp_cover',
     :        '()', g_out_transp_cover)

      elseif (variable_name .eq. 'clothesline') then
         call respond2get_real_var (
     :        'clothesline',
     :        '()', g_out_clothesline)

      elseif (variable_name .eq. 'tfact') then
         call respond2get_real_var (
     :        'tfact',
     :        '()', g_out_tfact)

      elseif (variable_name .eq. 'nfact') then
         call respond2get_real_var (
     :        'nfact',
     :        '()', g_out_nfact)

      elseif (variable_name .eq. 'vpd_estimation') then
         call respond2get_real_var (
     :        'vpd_estimation',
     :        '()', g_vpd)

      elseif (variable_name .eq. 'tsdm') then
         biomass = sum_real_array(g_dm_green, max_part) +
     :        sum_real_array(g_dm_dead, max_part) -
     :        g_dm_green(root) - g_dm_dead(root)

         call respond2get_real_var (
     :        'tsdm',
     :        '(kg/ha)', biomass)

      elseif (variable_name .eq. 'root_depth') then
         call respond2get_real_var (
     :        'root_depth',
     :        '(mm)', g_root_depth)

      elseif (variable_name .eq. 'green_root') then
         call respond2get_real_var (
     :        'green_root',
     :        '(kg/ha)', g_dm_green(root))

      elseif (variable_name .eq. 'green_leaf') then
         call respond2get_real_var (
     :        'green_leaf',
     :        '(kg/ha)', g_dm_green(leaf))

      elseif (variable_name .eq. 'green_stem') then
         call respond2get_real_var (
     :        'green_stem',
     :        '(kg/ha)', g_dm_green(stem))

      elseif (variable_name .eq. 'green_pool') then
         call respond2get_real_var (
     :        'green_pool',
     :        '(kg/ha)', g_dm_green(stem) + g_dm_green(leaf))

      elseif (variable_name .eq. 'dead_pool') then
         call respond2get_real_var (
     :        'dead_pool',
     :        '(kg/ha)', g_dm_dead(stem) + g_dm_dead(leaf))

      elseif (variable_name .eq. 'dead_root') then
         call respond2get_real_var (
     :        'dead_root',
     :        '(kg/ha)', g_dm_dead(root))

      elseif (variable_name .eq. 'dead_leaf') then
         call respond2get_real_var (
     :        'dead_leaf',
     :        '(kg/ha)', g_dm_dead(leaf))

      elseif (variable_name .eq. 'dead_stem') then
         call respond2get_real_var (
     :        'dead_stem',
     :        '(kg/ha)', g_dm_dead(stem))

      elseif (variable_name .eq. 'basal_area') then
         call respond2get_real_var (
     :        'basal_area',
     :        '(m^2/ha)', g_basal_area)

      elseif (variable_name .eq. 'acc_growth') then
         call respond2get_real_var (
     :        'acc_growth',
     :        '(kg/ha)', g_acc_growth)

      elseif (variable_name .eq. 'ep') then
         num_layers = count_of_real_vals (g_dlayer, max_layer)
         call respond2get_real_array (
     :        'ep',
     :        '(mm)', g_dlt_sw_dep, num_layers)

      elseif (variable_name .eq. 'sw_pot') then
         call respond2get_real_var (
     :        'sw_pot',
     :        '(mm)', g_out_sw_pot)

      elseif (variable_name .eq. 'growth') then
         call respond2get_real_var (
     :        'growth',
     :        '(kg/ha)', g_dlt_dm)

      elseif (variable_name .eq. 'growth_transp') then
         call respond2get_real_var (
     :        'growth_transp',
     :        '(kg/ha)', g_out_growth_transp)

      elseif (variable_name .eq. 'growth_photo') then
         call respond2get_real_var (
     :        'growth_photo',
     :        '(kg/ha)', g_out_growth_photo)

      elseif (variable_name .eq. 'growth_regrowth') then
         call respond2get_real_var (
     :        'growth_regrowth',
     :        '(kg/ha)', g_out_growth_regrow)

      elseif (variable_name .eq. 'death') then
         call respond2get_real_var (
     :        'death',
     :        '(kg/ha)', g_dlt_dm_sen(leaf) +
     :        g_dlt_dm_sen(stem))

      elseif (variable_name .eq. 'death_frost') then
         call respond2get_real_var (
     :        'death_frost',
     :        '(kg/ha)', g_out_death_frost(leaf) +
     :        g_out_death_frost(stem))

      elseif (variable_name .eq. 'death_frost_leaf') then
         call respond2get_real_var (
     :        'death_frost_leaf',
     :        '(kg/ha)', g_out_death_frost(leaf))

      elseif (variable_name .eq. 'death_frost_stem') then
         call respond2get_real_var (
     :        'death_frost_stem',
     :        '(kg/ha)', g_out_death_frost(stem))

      elseif (variable_name .eq. 'death_water') then
         call respond2get_real_var (
     :        'death_water',
     :        '(kg/ha)', g_out_death_water(leaf) +
     :        g_out_death_water(stem))

      elseif (variable_name .eq. 'death_water_leaf') then
         call respond2get_real_var (
     :        'death_water_leaf',
     :        '(kg/ha)', g_out_death_water(leaf))

      elseif (variable_name .eq. 'death_water_stem') then
         call respond2get_real_var (
     :        'death_water_stem',
     :        '(kg/ha)', g_out_death_water(stem))

      elseif (variable_name .eq. 'death_pheno') then
         call respond2get_real_var (
     :        'death_pheno',
     :        '(kg/ha)', g_out_death_pheno(leaf) +
     :        g_out_death_pheno(stem))

      elseif (variable_name .eq. 'death_pheno_leaf') then
         call respond2get_real_var (
     :        'death_pheno_leaf',
     :        '(kg/ha)', g_out_death_pheno(leaf))

      elseif (variable_name .eq. 'death_pheno_stem') then
         call respond2get_real_var (
     :        'death_pheno_stem',
     :        '(kg/ha)', g_out_death_pheno(stem))

      elseif (variable_name .eq. 'sw_demand') then
         call respond2get_real_var (
     :        'sw_demand',
     :        '(mm)', g_out_sw_demand)

      elseif (variable_name .eq. 'n_uptake') then
         call respond2get_real_var (
     :        'n_uptake',
     :        '(mm)', g_N_uptake)

      elseif (variable_name .eq. 'n_index') then
         call respond2get_real_var (
     :        'n_index',
     :        '()', g_out_nfact)

      elseif (variable_name .eq. 'rad_index') then
         call respond2get_real_var (
     :        'rad_index',
     :        '()', g_out_rfact)

      elseif (variable_name .eq. 'sw_index') then
         call respond2get_real_var (               
     :        'sw_index',
     :        '()', g_swi_total)

      elseif (variable_name .eq. 'swi') then
         num_layers = count_of_real_vals (g_dlayer, max_layer)
         call respond2get_real_array (
     :        'swi',
     :        '(mm)', g_swi, num_layers)

      elseif (variable_name .eq. 'temp_index') then
         call respond2get_real_var (
     :        'temp_index',
     :        '()', g_out_tfact)

      elseif (variable_name .eq. 'growth_index') then
         call respond2get_real_var (
     :        'growth_index',
     :        '()', g_out_tfact * g_out_rfact *
     :         g_swi_total)

      elseif (variable_name .eq. 'transp_eff_adj') then
         call respond2get_real_var (
     :        'transp_eff_adj',
     :        '()', grasp_transp_eff() )

      elseif (variable_name .eq. 'vpd_hgt_ndx') then
         call respond2get_real_var (
     :        'vpd_hgt_ndx',
     :        '()', grasp_vpd_hgt_ndx(g_canopy_height) )

      elseif (variable_name .eq. 'rlv') then
         num_layers = count_of_real_vals (g_dlayer, max_layer)
         call respond2get_real_array (
     :        'rlv',
     :        '()', g_rlv, num_layers)
         
      elseif (variable_name .eq. 'max_n_avail') then
         deepest_layer = find_layer_no (g_root_depth, g_dlayer
     :                                , max_layer)
         do 500 layer = 1, deepest_layer
            temp(layer) =  p_max_n_avail(layer) *
     :           root_proportion (layer, g_dlayer,
     :           g_root_depth)
 500  continue

         call respond2get_real_array (
     :        'max_n_avail',
     :        '()', temp, deepest_layer)

      else
         call message_unused ()

      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine grasp_read_constants ()
*     ===========================================================

*   Short description:
*       crop initialisation - reads constants from coefficient file

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
*     read_integer_var
*     read_real_var
*     read_real_array
*     grasp_version
*     write_string

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'const.inc'
      include   'grasp.inc'

*   Internal variables
      integer    numvals               ! number of values returned

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'grasp_read_constants')

      character  section_name*(*)
      parameter (section_name = 'constants')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      call write_string (lu_scr_sum
     :                  ,new_line//'    - Reading constants')

                                ! Bounds
      call read_real_var (section_name
     :                    , 'll_ub', '()'
     :                    , c_ll_ub, numvals
     :                    , 0.0, 3000.0)

      call read_integer_var (section_name
     :                    , 'year_ub', '()'
     :                    , c_year_ub, numvals
     :                    , 1800, 2300)

      call read_integer_var (section_name
     :                    , 'year_lb', '()'
     :                    , c_year_lb, numvals
     :                    , 1800, 2300)

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
     :                    , c_No3_ub, numvals
     :                    , 0.0, 100000.0)

      call read_real_var (section_name
     :                    , 'no3_lb', '(kg/ha)'
     :                    , c_No3_lb, numvals
     :                    , 0.0, 100000.0)

      call read_real_var (section_name
     :                    , 'no3_min_ub', '(kg/ha)'
     :                    , c_No3_min_ub, numvals
     :                    , 0.0, 100000.0)

      call read_real_var (section_name
     :                    , 'no3_min_lb', '(kg/ha)'
     :                    , c_No3_min_lb, numvals
     :                    , 0.0, 100000.0)


      call read_real_var (section_name
     :                   , 'ba_ll', '()'
     :                   , c_ba_ll, numvals
     :                   , 0.0, 10.0)

      call read_real_var (section_name
     :                   , 'ba_ul', '()'
     :                   , c_ba_ul, numvals
     :                   , 0.0, 20.0)

      call read_real_var (section_name
     :                   , 'pan_lb', '()'
     :                   , c_pan_lb, numvals
     :                   , 0.0, 1000.0)

      call read_real_var (section_name
     :                   , 'pan_ub', '()'
     :                   , c_pan_ub, numvals
     :                   , 0.0, 1000.0)

      call read_real_var (section_name
     :                   , 'vpd_lb', '()'
     :                   , c_vpd_lb, numvals
     :                   , 0.0, 1000.0)

      call read_real_var (section_name
     :                   , 'vpd_ub', '()'
     :                   , c_vpd_ub, numvals
     :                   , 0.0, 1000.0)

      call read_real_var (section_name
     :                   , 'es_lb', '()'
     :                   , c_es_lb, numvals
     :                   , 0.0, 1000.0)

      call read_real_var (section_name
     :                   , 'es_ub', '()'
     :                   , c_es_ub, numvals
     :                   , 0.0, 1000.0)

      call read_real_var (section_name
     :                   , 'tree_sw_ub', '()'
     :                   , c_tree_sw_ub, numvals
     :                   , 0.0, 1000.0)

      call read_real_var (section_name
     :                   , 'tree_sw_lb', '()'
     :                   , c_tree_sw_lb, numvals
     :                   , 0.0, 1000.0)

      call read_char_var (section_name
     :                     , 'stage_names', '()'
     :                     , p_stage_names, numvals)

      call read_real_array (section_name
     :                     , 'stage_number', max_stage, '()'
     :                     , p_stage_code_list, numvals
     :                     , 0.0, 1000.0)

      call read_real_array (section_name
     :                     , 'rue', max_stage, '(g dm/mj)'
     :                     , p_rue, numvals
     :                     , 0.0, 1000.0)

c      call read_real_array (section_name
c     :                     , 'root_depth_rate', max_stage, '(mm)'
c     :                     , p_root_depth_rate, numvals
c     :                     , 0.0, 1000.0)

c      call read_real_var (section_name
c     :                    , 'root_depth_lag', '(days)'
c     :                    , c_root_depth_lag, numvals
c     :                    , 0.0, 365.0)

      call read_real_var (section_name
     :                    , 'svp_fract', '()'
     :                    , c_svp_fract, numvals
     :                    , 0.0, 1.0)


      call read_real_array (section_name
     :                     , 'x_ave_temp', max_table, '(oC)'
     :                     , c_x_ave_temp, c_num_ave_temp
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_stress_photo', max_table, '()'
     :                     , c_y_stress_photo, c_num_factors
     :                     , 0.0, 1.0)

      call read_real_array (section_name
     :                     , 'y_swdef_leaf', max_table, '()'
     :                     , c_y_swdef_leaf, c_num_sw_demand_ratio
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'x_sw_demand_ratio', max_table, '()'
     :                     , c_x_sw_demand_ratio, c_num_sw_demand_ratio
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

      call read_real_var (section_name
     :                   , 'vpd_grnd_mult', '()'
     :                   , c_vpd_grnd_mult, numvals
     :                   , 1.0, 10.0)

      call read_real_var (section_name
     :                   , 'std_vpd', '()'
     :                   , c_std_vpd, numvals
     :                   , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'minsw', '()'
     :                    , c_minsw, numvals
     :                    , 0.0, 3000.0)


      call read_real_var (section_name
     :                    , 'dead_cover_slope', '()'
     :                    , c_dead_cover_slope, numvals
     :                    , 0.0, 0.001)

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine grasp_read_parameters ()
*     ===========================================================

*   Short description:
*       get parameters

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
*       090994 jngh specified and programmed

*   Calls:
*     read_real_array
*     pop_routine
*     push_routine
*     fill_real_array
*     write_string

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'const.inc'            ! new_line, lu_scr_sum, blank,
                                       !   lu_scr_sum
      include   'grasp.inc'            ! dlayer(max_layer)

*   Internal variables
      integer    layer                 ! layer number
      real       ll (max_layer)        ! lower limit of plant-extractable
                                       !   soil water for soil layer l
                                       !   (mm water/mm soil)
      integer    num_layers            ! number of layers in profile
      integer    numvals
      real       max_n_avail            ! initial max_n_avail
      real  max_n_avail_dist(max_layer) ! initial distribution of N 
                                        ! over profile (sum=1)

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'grasp_read_parameters')

      character  section_name*(*)
      parameter (section_name = 'parameters')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      call write_string (lu_scr_sum
     :                  ,new_line
     :                  //'   - Reading parameters')

      call read_char_var (section_name
     :     , 'uptake_source', '()'
     :     , p_uptake_source, numvals)
      if (p_uptake_source .ne. 'calc' .and. 
     :     p_uptake_source .ne. 'apsim') then
         call fatal_error(err_user, 'Unknown uptake_source.')
      endif

      call read_char_var (section_name
     :                     , 'crop_type', '()'
     :                     , p_crop_type, numvals)

                                ! Initial values
      call read_real_var (section_name
     :                    , 'root_depth_init', '(mm)'
     :                    , p_root_depth_init, numvals
     :                    , 0.0, 20000.0)

      call read_real_var (section_name
     :                    , 'dm_green_leaf_init', '(kg/ha)'
     :                    , p_dm_green_leaf_init, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'dm_green_stem_init', '(kg/ha)'
     :                    , p_dm_green_stem_init, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'dm_green_root_init', '(kg/ha)'
     :                    , p_dm_green_root_init, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                   , 'dm_dead_leaf_init', '(kg/ha)'
     :                   , p_dm_dead_leaf_init, numvals
     :                   , 0.0, 10000.0)

      call read_real_var (section_name
     :                   , 'dm_dead_stem_init', '(kg/ha)'
     :                   , p_dm_dead_stem_init, numvals
     :                   , 0.0, 10000.0)

      call read_real_var (section_name
     :                   , 'dm_dead_root_init', '(kg/ha)'
     :                   , p_dm_dead_root_init, numvals
     :                   , 0.0, 10000.0)

      call read_real_var (section_name
     :                   , 'basal_area_init', '()'
     :                   , p_basal_area_init, numvals
     :                   , 0.0, 10.0)

      call read_real_var (section_name
     :                   , 'acc_trans_for_n_init', '()'
     :                   , p_acc_trans_for_N_init, numvals
     :                   , 0.0, 1000.0)

      call read_real_var (section_name
     :                   , 'acc_growth_for_n_init', '()'
     :                   , p_acc_growth_for_N_init, numvals
     :                   , 0.0, 10000.0)

      call read_real_var (section_name
     :                   , 'max_n_avail', '()'
     :                   , max_N_avail, numvals
     :                   , 0.0, 10000.0)

      call read_real_array (section_name
     :                   , 'max_n_avail_dist', max_layer, '()'
     :                   , max_n_avail_dist, num_layers
     :                   , 0.0, 1.0)
      
      do 500 layer = 1, num_layers
         p_max_n_avail(layer) = max_n_avail * 
     :        max_n_avail_dist(layer)
 500  continue

       call read_real_var (section_name
     :                   , 'enr_a_coeff', '()'
     :                   , p_enr_a_coeff, numvals
     :                   , 0.0, 10.0)

      call read_real_var (section_name
     :                   , 'enr_b_coeff', '()'
     :                   , p_enr_b_coeff, numvals
     :                   , 0.0, 10.0)

                                ! Soil properties
      call read_real_array (section_name
     :                     , 'll', max_layer, '()'
     :                     , ll, num_layers
     :                     , 0.0, c_ll_ub)

      call fill_real_array (g_ll_dep, 0.0, max_layer)
      do 1000 layer = 1, num_layers
         g_ll_dep(layer) = ll(layer)*g_dlayer(layer)
1000  continue

      call read_real_array (section_name
     :                     , 'kl', max_layer, '()'
     :                     , p_kl, num_layers
     :                     , 0.0, 5.0)

      call read_real_var (section_name
     :                    , 'kl2rlv', '(mm)'
     :                    , p_kl2rlv, numvals
     :                    , 0.0, 10000.0)

                                ! Plant properties
c      call read_real_var (section_name
c     :                    , 'height_max', '(mm)'
c     :                    , c_height_max, numvals
c     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                   , 'hgt_vpd_screen', '(mm)'
     :                   , c_hgt_vpd_screen, numvals
     :                   , 0.0, 1500.0)

      call read_real_var (section_name
     :                   , 'height_1000kg', '(mm)'
     :                   , c_height_1000kg , numvals
     :                   , 0.0, 10000.0)

      call read_real_var (section_name
     :                   , 'et_use_efficiency', '()'
     :                   , c_et_use_efficiency, numvals
     :                   , 0.0, 1000.0)

      call read_real_var (section_name
     :                   , 'frac_leaf2total', '()'
     :                   , c_frac_leaf2total, numvals
     :                   , 0.0, 1.0)

      call read_real_var (section_name
     :                   , 'yld_cover_slope', '()'
     :                   , p_yld_cover_slope, numvals
     :                   , 0.0, 5.0)

      call read_real_var (section_name
     :                    , 'yld_fpc50', '()'
     :                    , p_yld_fpc50, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                   , 'yld_cov50', '()'
     :                   , p_yld_cov50, numvals
     :                   , 0.0, 5000.0)

      call read_real_var (section_name
     :                   , 'swi_fullgreen', '()'
     :                   , p_swi_fullgreen, numvals
     :                   , 0.0, 1.0)

      call read_real_var (section_name
     :                   , 'swi_nogrow', '()'
     :                   , p_swi_nogrow, numvals
     :                   , 0.0, 1.0)

      call read_real_var (section_name
     :                   , 'pot_regrow', '()'
     :                   , p_pot_regrow, numvals
     :                   , 0.0, 1000.0)

      call read_real_var (section_name
     :                   , 'te_std', '()'
     :                   , p_te_std, numvals
     :                   , 0.0, 1000.0)

      call read_real_var (section_name
     :                   , 'rad_factor', '()'
     :                   , p_rad_factor, numvals
     :                   , 0.0, 10.0)

      call read_real_var (section_name
     :                   , 'residual_plant_N', '()'
     :                   , c_residual_plant_N, numvals
     :                   , 0.0, 1000.0)

      call read_real_var (section_name
     :                   , 'litter_n', '()'
     :                   , c_litter_n, numvals
     :                   , 0.0, 1000.0)

      call read_real_var (section_name
     :                   , 'n_uptk_per100 ', '()'
     :                   , c_N_uptk_per100 , numvals
     :                   , 0.0, 10000.0)

      call read_real_var (section_name
     :                   , 'frost_start', '()'
     :                   , c_frost_start, numvals
     :                   , -100.0, 100.0)

      call read_real_var (section_name
     :                   , 'frost_kill', '()'
     :                   , c_frost_kill, numvals
     :                   , -100.0, 100.0)

      call read_real_var (section_name
     :                   , 'death_slope', '()'
     :                   , c_death_slope, numvals
     :                   , 0.0, 10.0)

      call read_real_var (section_name
     :                   , 'death_intercept', '()'
     :                   , c_death_intercept, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'leaf_death_ratio', '()'
     :                   , c_leaf_death_ratio, numvals
     :                   , 0.0, 1.0)

      call read_real_var (section_name
     :                   , 'stem_death_ratio', '()'
     :                   , c_stem_death_ratio, numvals
     :                   , 0.0, 1.0)

      call read_real_var (section_name
     :                   , 'n_conc_dm_crit', '()'
     :                   , c_N_conc_dm_crit, numvals
     :                   , 0.0, 10.0)

      call read_real_var (section_name
     :                   , 'n_conc_dm_min', '()'
     :                   , c_N_conc_dm_min, numvals
     :                   , 0.0, 10.0)

      call read_real_var (section_name
     :                   , 'n_conc_dm_max', '()'
     :                   , c_N_conc_dm_max, numvals
     :                   , 0.0, 10.0)

      call read_real_var (section_name
     :                   , 'stem_thresh', '()'
     :                   , c_stem_thresh, numvals
     :                   , 0.0, 10000.0)

      call read_real_array (section_name
     :                     , 'detach_wetseason', max_part, '(mm)'
     :                     , c_detach_wetseason, numvals
     :                     , 0.0, 1.0)

      call read_real_array (section_name
     :                     , 'detach_dryseason', max_part, '(mm)'
     :                     , c_detach_dryseason, numvals
     :                     , 0.0, 1.0)

                                ! Grasp date resets
      call read_integer_var (section_name
     :                   , 'day_start_summer', '()'
     :                   , c_day_start_summer, numvals
     :                   , 0, 366)

      call read_integer_var (section_name
     :                   , 'day_end_summer', '()'
     :                   , c_day_end_summer, numvals
     :                   , 0, 366)

      call read_integer_var (section_name
     :                   , 'acc_et_reset', '()'
     :                   , c_acc_et_reset, numvals
     :                   , 0, 366)

      call read_integer_var (section_name
     :                   , 'acc_growth_reset', '()'
     :                   , c_acc_growth_reset, numvals
     :                   , 0, 366)

      call read_integer_var (section_name
     :                   , 'trans_for_n_reset', '()'
     :                   , c_trans_for_n_reset, numvals
     :                   , 0, 366)

      call read_integer_var (section_name
     :                   , 'growth_for_n_reset', '()'
     :                   , c_growth_for_n_reset, numvals
     :                   , 0, 366)

      call read_integer_var (section_name
     :                   , 'day_start_wet', '()'
     :                   , c_day_start_wet, numvals
     :                   , 0, 366)

      call read_integer_var (section_name
     :                   , 'day_start_dry', '()'
     :                   , c_day_start_dry, numvals
     :                   , 0, 366)



      call pop_routine (my_name)
      return
      end

*     ===========================================================
      subroutine grasp_write_summary ()
*     ===========================================================

*   Short description:
*       write summary info to summary file.

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
*     grasp_get_cultivar_params
*     pop_routine
*     push_routine
*     write_string

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments

*   Global variables
      include   'const.inc'            ! lu_scr_sum, blank
      include   'grasp.inc'
      integer   lastnb

*   Internal variables
      character string*200
      character owner_module*200
      integer   layer
      integer   numvals
      real      value

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'grasp_write_summary')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      call report_event ( 'Establishing Sward')

      write (string, '(a)')
     :     'Parameters: '
      call write_string (lu_scr_sum, string)

      write (string, '(a, f8.2, a, f4.1, a)')
     :     '  Transpiration Efficiency:  ', p_te_std ,
     :     ' kg/ha/mm at ', c_std_vpd, ' hPa'
      call write_string (lu_scr_sum, string)

      write (string, '(a, f8.2, a)')
     :     '  Potential regrowth:        ', p_pot_regrow,
     :     ' kg/ha/day'
      call write_string (lu_scr_sum, string)

      write (string, '(a, f8.2, a)')
     :     '  Radiation use eff.:        ', p_rue(establishment),
     :     ' ()'
      call write_string (lu_scr_sum, string)

      write (string, '(a, f8.2, a)')
     :     '  SWI full green:            ', p_swi_fullgreen,
     :     ' ()'
      call write_string (lu_scr_sum, string)

      write (string, '(a, f8.2, a)')
     :     '  fpc50 yield(radn):         ', p_yld_fpc50,
     :     ' kg/ha'
      call write_string (lu_scr_sum, string)

      write (string, '(a, f8.2, a)')
     :     '  fcov50 yield(evap):        ', p_yld_cov50,
     :     ' kg/ha'
      call write_string (lu_scr_sum, string)

      write (string, '(a, f8.2, a, f8.2, a)')
     :     '  Frost start:', c_frost_start,
     :     ' oC, kill: ', c_frost_kill, ' oC.'
      call write_string (lu_scr_sum, string)

      write (string,'(a)') '  Root Profile:'
      call write_string (lu_scr_sum, string)

      string = '      Layer    Lower limit       Kl       Max N'
      call write_string (lu_scr_sum, string)

      string = '       ()        (mm)            ()      (kg/ha)'
      call write_string (lu_scr_sum, string)

      string = '    --------------------------------------------'
      call write_string (lu_scr_sum, string)

      do 2000 layer = 1, g_num_layers
         write (string,'(3x, i8, f12.3,f12.3,f12.2)')
     :        layer
     :        , g_ll_dep(layer) 
     :        , p_kl(layer) 
     :        , p_max_n_avail(layer)
         call write_string (lu_scr_sum, string)
2000  continue                           

      string = '    --------------------------------------------'
      call write_string (lu_scr_sum, string)

      write (string, '(a)')
     :     'Initial conditions:'
      call write_string (lu_scr_sum, string)

      write (string, '(a, f8.2, a)')
     :     '  Basal area :', g_basal_area, ' %'
      call write_string (lu_scr_sum, string)

      string = '  Pools:'
      call write_string (lu_scr_sum, string)

      write (string, '(a)')
     :         '           root     stem     leaf'
      call write_string (lu_scr_sum, string)

      string = '        +--------+--------+--------+'
      call write_string (lu_scr_sum, string)

      write (string, '(a, 3f9.1)')
     :         ' green  |', g_dm_green(root),
     :         g_dm_green(stem), g_dm_green(leaf)
      call write_string (lu_scr_sum, string)

      write (string, '(a, 3f9.1)')
     :         ' dead   |', g_dm_dead(root),
     :         g_dm_dead(stem), g_dm_dead(leaf)
      call write_string (lu_scr_sum, string)

      string = '        +--------+--------+--------+'
      call write_string (lu_scr_sum, string)

      write (string, '(a, f8.1, a)')
     :     '  Root depth :', g_root_depth, ' mm'
      call write_string (lu_scr_sum, string)

      call get_real_var_optional (unknown_module, 'vpd', '(hPa)'
     :     , value, numvals, c_vpd_lb, c_vpd_ub)

      if (numvals .le. 0) then
         string = '  Using vpd approximated from maxt, mint.'
      else
         call get_posting_module (owner_module)
         write (string, '(a, a, a)')
     :        '  Using vpd from ', 
     :        owner_module(:lastnb(owner_module)), ' module.'
      endif
      call write_string (lu_scr_sum, string)

      call get_real_var_optional (unknown_module, 'pan', '(mm)'
     :     , value, numvals, c_pan_lb, c_pan_ub)

      if (numvals .le. 0) then
         call get_real_var_optional (unknown_module, 'eo', '(mm)'
     :     , value, numvals, c_pan_lb, c_pan_ub)
         call get_posting_module (owner_module)
         write (string, '(a,a,a)')
     :        '  NB. Pan evap approximated by ',
     :        owner_module(:lastnb(owner_module)),
     :        '.eo'
      else
         call get_posting_module (owner_module)
         write (string, '(a, a, a)')
     :        '  Using Pan evap from ', 
     :        owner_module(:lastnb(owner_module)), ' module.'
      endif
      call write_string (lu_scr_sum, string)

      call write_string (lu_scr_sum, new_line//new_line)

      call pop_routine (my_name)
      return
      end

*     ===========================================================
      subroutine grasp_soil_loss ()
*     ===========================================================

*   Short description:
*        Soil loss effects on grasp's "max_n_avail". This is a 
*     kludge forced by our lack a coherent interface to soiln.

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
*     grasp_get_cultivar_params
*     pop_routine
*     push_routine
*     write_string

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments

*   Global variables
      include   'const.inc'            ! lu_scr_sum, blank
      include   'grasp.inc'
      real      divide
      integer   count_of_real_vals

*   Internal variables
      real      enr                    ! enrichment ratio
      real      n_conc
      real      n_loss, n_gain
      integer   layer, num_layers

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'grasp_soil_loss')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      if (g_soil_loss .gt. 0.0) then
         
         num_layers = count_of_real_vals (g_dlayer, max_layer)

         enr = p_enr_a_coeff * 
     :        (1000.0 * g_soil_loss)**(-1.0 * p_enr_b_coeff)

         enr = amin1(p_enr_a_coeff, enr)
         enr = amax1(enr, 1.0)

                                ! Concentration in layer 1
         n_conc = divide( p_max_n_avail(1),
     :        1000.0*g_bd(1)*g_dlayer(1)*10.0, 0.0)   ! N (kg/kg)

                                ! Loss from layer 1
         n_loss = g_soil_loss * 1000.0 * enr * n_conc ! N (kg/ha)

                                ! Gain to layer 1
         n_gain = p_max_n_avail(2) * (1.0 - g_layer_fract(2))

         p_max_n_avail(1) = p_max_n_avail(1) + n_gain - n_loss 

                                ! remaining layers 
         do 100 layer = 2, num_layers

            n_loss = p_max_n_avail(layer) * 
     :           (1.0 - g_layer_fract(layer))
            if (layer .lt. num_layers) then
               n_gain = p_max_n_avail(layer+1) * 
     :              (1.0 - g_layer_fract(layer+1))
            else
               n_gain = n_loss  ! Assume no bedrock. 
                                !(typically, the lowest layer is 
                                ! close to zero anyway...)
            endif

            p_max_n_avail(layer) = p_max_n_avail(layer) + n_gain -
     :           n_loss 

 100     continue

      endif

      call pop_routine (my_name)
      return
      end

c$$$*     ===========================================================
c$$$      real function grasp_sw_avail_fac (layer)
c$$$*     ===========================================================
c$$$
c$$$*   Short description:
c$$$
c$$$*      Get the soil water availability factor in a layer.  For a layer,
c$$$*      it is 1.0 unless the plant-extractable soil water declines
c$$$*      below a fraction of plant-extractable soil water capacity for
c$$$*      that layer.
c$$$
c$$$*   Assumptions:
c$$$*       none
c$$$
c$$$*   Notes:
c$$$*       Unused! what's it doing here?
c$$$
c$$$*   Procedure attributes:
c$$$*      Version:         any hardware/fortran77
c$$$*      Extensions:      long names <= 20 chars.
c$$$*                       lowercase
c$$$*                       underscore
c$$$*                       inline comments
c$$$*                       include
c$$$*                       implicit none
c$$$
c$$$*   Changes:
c$$$*     010994 jngh specified and programmed
c$$$
c$$$*   Calls:
c$$$*     bound
c$$$*     divide
c$$$*     linear_interp_real
c$$$
c$$$* ----------------------- Declaration section ------------------------
c$$$
c$$$      implicit none
c$$$
c$$$*   Subroutine arguments
c$$$      integer    layer          ! (INPUT) soil profile layer number
c$$$
c$$$*   Global variables
c$$$      include   'grasp.inc'
c$$$      real       divide         ! function
c$$$      real       linear_interp_real ! function
c$$$
c$$$*   Internal variables
c$$$      real       pesw           ! plant extractable soil-water (mm/mm)
c$$$      real       pesw_capacity  ! plant extractable soil-water capacity
c$$$                                !   (mm/mm)
c$$$      real       sw_avail_ratio ! soil water availability ratio (0-1)
c$$$
c$$$*   Constant values
c$$$      character  my_name*(*)    ! name of procedure
c$$$      parameter (my_name = 'grasp_sw_avail_fac')
c$$$
c$$$*   Initial data values
c$$$*     none
c$$$
c$$$* --------------------- Executable code section ----------------------
c$$$
c$$$      call push_routine (my_name)
c$$$
c$$$      pesw = g_sw_dep(layer) - g_ll_dep(layer)
c$$$      pesw_capacity = g_dul_dep(layer) - g_ll_dep(layer)
c$$$
c$$$      sw_avail_ratio = divide (pesw, pesw_capacity, 10.0)
c$$$      grasp_sw_avail_fac = linear_interp_real (sw_avail_ratio
c$$$     :                           , c_x_sw_ratio, c_y_sw_fac_root
c$$$     :                           , c_num_sw_ratio)
c$$$
c$$$      call pop_routine (my_name)
c$$$      return
c$$$      end
c$$$*     ===========================================================
c$$$      subroutine grasp_kill_crop ()
c$$$*     ===========================================================
c$$$
c$$$*   Short description:
c$$$*       Kil crop
c$$$
c$$$*   Assumptions:
c$$$*       none
c$$$
c$$$*   Notes:
c$$$*       none
c$$$
c$$$*   Procedure attributes:
c$$$*      Version:         any hardware/fortran77
c$$$*      Extensions:      long names <= 20 chars.
c$$$*                       lowercase
c$$$*                       underscore
c$$$*                       inline comments
c$$$*                       include
c$$$*                       implicit none
c$$$
c$$$*   Changes:
c$$$*       290994 jngh specified and programmed
c$$$
c$$$*   Calls:
c$$$*     pop_routine
c$$$*     push_routine
c$$$*     report_event
c$$$*     fill_real_array
c$$$*     sum_real_array
c$$$*     write_string
c$$$
c$$$* ----------------------- Declaration section ------------------------
c$$$
c$$$      implicit none
c$$$
c$$$*   Subroutine arguments
c$$$*       none
c$$$
c$$$*   Global variables
c$$$      include   'convert.inc'   ! gm2kg, sm2ha, mm2cm, cmm2cc
c$$$      include   'grasp.inc'
c$$$
c$$$      real       sum_real_array ! function
c$$$
c$$$*   Internal variables
c$$$      real       biomass        ! above ground dm (kg/ha)
c$$$      character  string*200     ! output string
c$$$
c$$$*   Constant values
c$$$      character  my_name*(*)    ! name of procedure
c$$$      parameter (my_name  = 'grasp_kill_crop')
c$$$
c$$$
c$$$*   Initial data values
c$$$*     none
c$$$
c$$$* --------------------- Executable code section ----------------------
c$$$      call push_routine (my_name)
c$$$
c$$$      if (g_crop_status.eq.crop_alive) then
c$$$         g_crop_status = crop_dead
c$$$
c$$$         biomass = (sum_real_array (g_dm_green, max_part) -
c$$$     :        g_dm_green(root)) +
c$$$     :        (sum_real_array (g_dm_dead, max_part) -
c$$$     :        g_dm_dead(root))
c$$$
c$$$                ! report
c$$$
c$$$         write (string, '(3x, a, f7.1, a)')
c$$$     :                  ' crop_kill. Above-ground dm = '
c$$$     :                  , biomass, ' (kg/ha)'
c$$$         call report_event (string)
c$$$
c$$$      else
c$$$
c$$$      endif
c$$$
c$$$      call pop_routine (my_name)
c$$$      return
c$$$      end
c$$$*     ===========================================================
c$$$      subroutine grasp_end_crop ()
c$$$*     ===========================================================
c$$$
c$$$*   Short description:
c$$$*       End crop
c$$$
c$$$*   Assumptions:
c$$$*       none
c$$$
c$$$*   Notes:
c$$$*       none
c$$$
c$$$*   Procedure attributes:
c$$$*      Version:         any hardware/fortran77
c$$$*      Extensions:      long names <= 20 chars.
c$$$*                       lowercase
c$$$*                       underscore
c$$$*                       inline comments
c$$$*                       include
c$$$*                       implicit none
c$$$
c$$$*   Changes:
c$$$*       290994 jngh specified and programmed
c$$$
c$$$*   Calls:
c$$$*     divide
c$$$*     pop_routine
c$$$*     push_routine
c$$$*     report_event
c$$$*     fill_real_array
c$$$*     sum_real_array
c$$$*     write_string
c$$$
c$$$* ----------------------- Declaration section ------------------------
c$$$
c$$$      implicit none
c$$$
c$$$*   Subroutine arguments
c$$$*       none
c$$$
c$$$*   Global variables
c$$$      include   'const.inc'            ! new_line, lu_scr_sum
c$$$      include   'convert.inc'          ! gm2kg, sm2ha, mm2cm, cmm2cc
c$$$      include   'grasp.inc'
c$$$
c$$$c      real       divide                ! function
c$$$      real       sum_real_array                  ! function
c$$$
c$$$*   Internal variables
c$$$      real       rcn            ! root c:n ratio
c$$$c     real       residue_N             ! nitrogen in residue (kg/ha)
c$$$c     real       residue_wt            ! residue (kg/ha)
c$$$      real       dm_root        ! total root weight (kg/ha)
c$$$      real       dm_stover      ! stover wt (kg/ha)
c$$$      character  string*200     ! output string
c$$$      real       yield          ! grain wt (kg/ha)
c$$$
c$$$*   Constant values
c$$$      character  my_name*(*)    ! name of procedure
c$$$      parameter (my_name  = 'grasp_end_crop')
c$$$
c$$$*   Initial data values
c$$$*     none
c$$$
c$$$* --------------------- Executable code section ----------------------
c$$$
c$$$      call push_routine (my_name)
c$$$
c$$$      if (g_crop_status.ne.crop_out) then
c$$$         g_crop_status = crop_out
c$$$         g_current_stage = crop_end
c$$$
c$$$         dm_stover = (sum_real_array (g_dm_green, max_part) -
c$$$     :        g_dm_green(root)) +
c$$$     :        (sum_real_array (g_dm_dead, max_part) -
c$$$     :        g_dm_dead(root))
c$$$
c$$$         dm_root = g_dm_green(root) + g_dm_dead(root)
c$$$         yield = dm_stover
c$$$
c$$$                ! report
c$$$
c$$$         write (string, '(3x, a, f7.1)')
c$$$     :                  ' ended. Yield (dw) = ', yield
c$$$         call report_event (string)
c$$$
c$$$             ! now do post harvest processes
c$$$
c$$$             ! get rcn ratio of roots
c$$$cjh      need to do something about c_focp ?????????????
c$$$
c$$$c$$$         rcn = divide (g_dm_green(root)*c_focp, g_N_plant(root), 0.0)
c$$$c$$$
c$$$c$$$         write (string, '(27x, a, f7.1, 3a, f6.1, 2a, f6.2)')
c$$$c$$$     :                    '  straw residue =', dm_stover, ' kg/ha'
c$$$c$$$     :                  , new_line
c$$$c$$$     :                  , '  root residue = ', dm_root, ' kg/ha'
c$$$c$$$     :                  , '  root C:N = ', rcn
c$$$c$$$         call write_string (lu_scr_sum, string)
c$$$c$$$
c$$$c$$$             ! put stover into surface residue
c$$$c$$$
c$$$c$$$         residue_wt = (sum_real_array (g_dm_green, max_part) -
c$$$c$$$     :        g_dm_green(root)) +
c$$$c$$$     :        (sum_real_array (g_dm_dead, max_part) -
c$$$c$$$     :        g_dm_dead(root))
c$$$c$$$     :
c$$$c$$$         residue_N = 0.0
c$$$cpdev was : (sum_real_array (g_N_plant, max_part)
c$$$cpdev     :             - g_N_plant(root) )
c$$$cpdev     :             * gm2kg *g_plants/sm2ha ! NB
c$$$
c$$$c$$$         call grasp_add_residue (residue_wt, residue_N)
c$$$
c$$$             ! put roots into root residue
c$$$
c$$$cjh       don't like using rcn - inconsistent with residue and have to
c$$$cjh       get c_focp just for this ??????????????
c$$$
c$$$c         call grasp_add_roots (dm_root, rcn)
c$$$
c$$$      else
c$$$      endif
c$$$
c$$$      call pop_routine (my_name)
c$$$      return
c$$$      end
c$$$*     ===========================================================
c$$$      subroutine grasp_add_roots (dlt_dm_root, root_CN)
c$$$*     ===========================================================
c$$$
c$$$*   Short description:
c$$$*     add root residue to root residue pool
c$$$
c$$$*   Assumptions:
c$$$*       none
c$$$
c$$$*   Notes:
c$$$*       none
c$$$
c$$$*   Procedure attributes:
c$$$*      Version:         any hardware/fortran77
c$$$*      Extensions:      long names <= 20 chars.
c$$$*                       lowercase
c$$$*                       underscore
c$$$*                       inline comments
c$$$*                       include
c$$$*                       implicit none
c$$$
c$$$*   Changes:
c$$$*       220794 jngh specified and programmed
c$$$
c$$$*   Calls:
c$$$*     message_send
c$$$*     pop_routine
c$$$*     push_routine
c$$$
c$$$* ----------------------- Declaration section ------------------------
c$$$
c$$$      implicit none
c$$$
c$$$*   Subroutine arguments
c$$$      real       dlt_dm_root    ! (INPUT) new roote residue (kg/ha)
c$$$      real       root_CN        ! (INPUT) new root residue CN ()
c$$$
c$$$*   Global variables
c$$$      include   'const.inc'     ! global_ative
c$$$
c$$$*   Internal variables
c$$$      character  string*200     ! output string
c$$$
c$$$*   Constant values
c$$$      character  my_name*(*)    ! name of procedure
c$$$      parameter (my_name  = 'grasp_add_roots')
c$$$
c$$$
c$$$*   Initial data values
c$$$*     none
c$$$
c$$$* --------------------- Executable code section ----------------------
c$$$
c$$$      call push_routine (my_name)
c$$$
c$$$c+!!!!!!!! needs to be sorted out with nitrogen mob
c$$$
c$$$      write(string, '(a, 2f9.1)' ) dlt_dm_root, root_CN
c$$$      call message_send (all_active_modules, 'add_roots', string)
c$$$C
c$$$C
c$$$C
c$$$c how it should be done !
c$$$c      if (dlt_dm_root.gt.0.0) then
c$$$c
c$$$c            ! send out root residue
c$$$c
c$$$c         call maize_root_distrib (dlt_dm_incorp
c$$$c     :                          , dlt_dm_root * gm2kg /sm2ha)
c$$$c         call maize_root_distrib (dlt_N_incorp
c$$$c     :                          , dlt_N_root * gm2kg /sm2ha)
c$$$c
c$$$c         deepest_layer = find_layer_no (g_root_depth, g_dlayer
c$$$c     :                                , max_layer)
c$$$c
c$$$c         string = 'dlt_fom_type='// c_crop_type
c$$$c
c$$$c         write (string(lastnb(string)+1:), '(a, 20g16.7e3)' )
c$$$c     :              ', dlt_fom_wt = '
c$$$c     :               , (dlt_dm_incorp(layer), layer = 1, deepest_layer)
c$$$c         string =  string_concat (string, '(kg/ha)')
c$$$c
c$$$c         write (string(lastnb(string)+1:), '(a, 20g16.7e3)')
c$$$c     :              ', dlt_fom_n = '
c$$$c     :               , (dlt_N_incorp(layer), layer = 1, deepest_layer)
c$$$c         string = string_concat (string, '(kg/ha)')
c$$$c
c$$$c         call message_pass_to_module (all_active_modules
c$$$c     :                               , 'incorp_fom'
c$$$c     :                               , string)
c$$$c
c$$$c      else
c$$$c         ! no roots to incorporate
c$$$c      endif
c$$$c
c$$$      call pop_routine (my_name)
c$$$      return
c$$$      end
