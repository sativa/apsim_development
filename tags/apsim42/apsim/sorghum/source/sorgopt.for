*     ===========================================================
      subroutine sorg_dm_init (
     .          g_current_stage,
     .          g_days_tot,
     .          c_dm_root_init,
     .          g_plants,
     .          c_dm_stem_init,
     .          c_dm_leaf_init,
     .          c_stem_trans_frac,
     .          c_leaf_trans_frac,
     .                    dm_green, dm_plant_min)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
       real g_current_stage
       real g_days_tot(*)
       real c_dm_root_init
       real g_plants
       real c_dm_stem_init
       real c_dm_leaf_init
       real c_stem_trans_frac
       real c_leaf_trans_frac
      real       dm_green(*)           ! (INPUT/OUTPUT) plant part weights
                                       ! (g/m^2)
      real       dm_plant_min(*)       ! (OUTPUT) minimum weight of each
                                       ! plant part (g/plant)

*+  Purpose
*       Initialise plant weights and plant weight minimums
*       at required instances.

*+  Changes
*     010994 jngh specified and programmed
*     970317 slw new template form

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Sorg_dm_init')

*+  Local Variables
      real       dm_plant_leaf         ! dry matter in leaves (g/plant)
      real       dm_plant_stem         ! dry matter in stems (g/plant)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

         ! initialise plant weight
         ! initialisations - set up dry matter for leaf, stem, flower, grain
         ! and root

      if (on_day_of (emerg, g_current_stage, g_days_tot)) then
             ! seedling has just emerged.

             ! initialise root, stem and leaf.

         dm_green(root) = c_dm_root_init * g_plants
         dm_green(stem) = c_dm_stem_init * g_plants
         dm_green(leaf) = c_dm_leaf_init * g_plants
         dm_green(grain) = 0.0
         dm_green(flower) = 0.0

!changed from start_grain_fill

      elseif (on_day_of (flowering
     :                 , g_current_stage, g_days_tot)) then

             ! we are at first day of grainfill.
             ! set the minimum weight of leaf; used for translocation to grain
             ! and stem!

         dm_plant_leaf = divide (dm_green(leaf), g_plants, 0.0)
         dm_plant_min(leaf) = dm_plant_leaf * (1.0 - c_leaf_trans_frac)
         dm_plant_stem = divide (dm_green(stem), g_plants, 0.0)
         dm_plant_min(stem) = dm_plant_stem * (1.0 - c_stem_trans_frac)

      else   ! no changes
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine srop_leaf_area_lai_max_possible (
     .          start_stage_SPLA,
     .          end_stage_TPLA,
     .          end_stage_SPLA,
     .          g_current_stage,
     .          g_swdef_lai_loss,
     .          g_lai_max_possible,
     .          g_lai,
     .          g_slai,
     .          g_dlt_lai_pot,
     .          g_dlt_lai_stressed)

*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    start_stage_SPLA
      integer    end_stage_TPLA
      integer    end_stage_SPLA
      real       g_current_stage
      real       g_swdef_lai_loss
      real       g_lai_max_possible
      real       g_lai
      real       g_slai
      real       g_dlt_lai_pot           !lai potential from TT
      real       g_dlt_lai_stressed      !g_dlt_lai_pot limited by stress

*+  Purpose
*     Calculate the stressed dlt_lai from the potential dlt_lai.
*     Returns both as well as an upper limit on potential lai
*     due to irretrivable stress losses
*
*   Called by srop_leaf_area_potential(2) in croptree.for

*+  Changes
*     26/2/96  sb made it up.
*     27/5/97 scc tried to fix it.

*+  Calls
*      include   'cropdefcons.inc'
*      include 'const.inc'

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_leaf_area_lai_max_possible')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

cscc/glh Need a proper target for SPLA. Although I think that we should
c not have a target at all, but rather a 'longevity' approach. This makes
c it easier to implement N stress also

c (Trying to..) calculate LAI that is (irretreivably) lost due to stress

      if (stage_is_between (start_stage_SPLA, end_stage_SPLA
     :                     , g_current_stage)) then

!replace following g_dlt_lai_stressed w. g_dlt_lai_actual
         g_swdef_lai_loss = g_swdef_lai_loss
     :      + g_dlt_lai_pot - g_dlt_lai_stressed

c Calculate max. LAI possible, accounting for losses due to stress
c Unfortunately, this sometimes goes negative! Need to have another
c look at its calculation. Not used at present.

         g_lai_max_possible = g_lai + g_slai - g_swdef_lai_loss

!scc after flag leaf, tplamax for senescence is set to the tpla reached

         if (stage_is_between (end_stage_TPLA, end_stage_SPLA
     :                     , g_current_stage)) then
            g_lai_max_possible = g_lai + g_slai
         else
            ! do nothing
         endif

      else
      ! Before floral initiation

         g_lai_max_possible = 0.0

      endif

      call pop_routine (my_name)
      return
      end subroutine



* ====================================================================
       subroutine crop_cover1 (
     .          g_row_spacing,
     .          c_x_row_spacing,
     .          c_y_extinct_coef,
     .          c_num_row_spacing,
     .          g_lai,
     .          g_cover_green)
* ====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real g_row_spacing
      real c_x_row_spacing(*)
      real c_y_extinct_coef(*)
      integer c_num_row_spacing
      real g_lai
      real g_cover_green

*+  Purpose
*scc Does crop cover calculation for green, senesced or dead LAI
!scc This is an excellent general routine

*+  Changes
*     15-08-1997 - huth - Programmed and Specified
*
*   Called by srop_water_demand(1) in crop

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'crop_cover1')

*+  Local Variables
      real extinct_coef

*- Implementation Section ----------------------------------
      call push_routine (myname)

      extinct_coef = linear_interp_real (g_row_spacing
     :                                  ,c_x_row_spacing
     :                                  ,c_y_extinct_coef
     :                                  ,c_num_row_spacing)

      g_cover_green = (1.0 -exp (-extinct_coef*g_lai))

      call pop_routine (myname)
      return
      end subroutine



*     ===========================================================
      subroutine cproc_leaf_area_pot2 (
     .          begin_stage,
     .          end_stage_TPLA_plateau,
     .          now,
     .          g_phase_tt,

     .          g_days_tot,
     .          g_current_stage,
     .          g_leaf_no_final,
     .          c_initial_tpla,
     .          g_tiller_no_fertile,
     .          c_tiller_coef,
     .          p_main_stem_coef,
     .          g_tt_tot,
     .          c_tpla_inflection_ratio,
     .          g_tpla_today,
     .          g_tpla_yesterday,
     .          p_tpla_prod_coef,
     .          g_plants,
     .          g_lai,
     .          g_dlt_lai_pot)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    begin_stage            !stage number of start
      integer    end_stage_TPLA_plateau !stage number to stop TPLA growth
      integer    now                    !stage number now
      real       g_phase_tt(*)
*
      real       g_days_tot(*)
      real       g_current_stage
      real       g_leaf_no_final
      real       c_initial_tpla
      real       g_tiller_no_fertile
      real       c_tiller_coef
      real       p_main_stem_coef
      real       g_tt_tot(*)
      real       c_tpla_inflection_ratio
      real       g_tpla_today
      real       g_tpla_yesterday
      real       p_tpla_prod_coef
      real       g_plants
      real       g_lai
      real       g_dlt_lai_pot           ! (OUTPUT) change in leaf area

*+  Purpose
*       Return the potential increase in leaf area development (mm^2)
*       calculated on a whole plant basis as determined by thermal time
*
*   Called by srop_leaf_area_potential(2) in croptree.for

*+  Changes
*     010994 jngh specified and programmed
*     26/2/97  sb moved stressing out to another routine.

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_leaf_area_pot2')

*+  Local Variables
      real       tpla_max              ! maximum total plant leaf area (mm^2)
      real       tt_since_begin        ! deg days since begin TPLA Period
*
      real       tpla_inflection       ! inflection adjusted for leaf no.
*
      real       tt_begin_to_end_TPLA  ! thermal time for TPLA period

*- Implementation Section ----------------------------------

      call push_routine (my_name)

           ! once leaf no is calculated maximum plant leaf area
           ! is determined

      if (on_day_of (begin_stage, g_current_stage, g_days_tot)) then
         g_lai = c_initial_tpla * smm2sm * g_plants
      endif


      if (stage_is_between (begin_stage, end_stage_TPLA_plateau,
     .           g_current_stage) .and.
     .        g_phase_tt(end_stage_TPLA_plateau) .gt.0.0) then

         tt_begin_to_end_TPLA = sum_between(begin_stage,
     :                          end_stage_TPLA_plateau,g_phase_tt)

         tpla_max = (((g_tiller_no_fertile + 1.0) ** c_tiller_coef)
     :            * g_leaf_no_final ** p_main_stem_coef) * scm2smm

         tt_since_begin = sum_between (begin_stage, now, g_tt_tot)

cscc 10/95 fixing the beta inflection coefficient as halfway to thermal
c time of flag_leaf expanded. Code needs work as the halfway point jumps
c around a bit as we progress (espec. when final_leaf_no is reset at floral in
c Note that tpla_inflection needs to be removed as a 'read-in' parameter
c maybe the number is more like .66 of the distance?
c can work out from the shape of a leaf area distribution - where is the biggest
c leaf appearing...

c  scc - generalise tpla_inflection  - needs more work

         tpla_inflection = tt_begin_to_end_TPLA *
     :           c_tpla_inflection_ratio

c scc end of changes for tpla (more below)

         g_tpla_today = divide (Tpla_max
     :              , (1.0 + exp(-p_tpla_prod_coef
     :                        * (tt_since_begin - tpla_inflection)))
     :              , 0.0)

         if (g_tpla_today .lt. g_tpla_yesterday)then
            g_tpla_today = g_tpla_yesterday
         endif

         g_dlt_lai_pot = (g_tpla_today - g_tpla_yesterday)
     .                  *smm2sm * g_plants

         g_tpla_yesterday = g_tpla_today

      else
!Beyond TPLA growth stage
         g_dlt_lai_pot = 0.0

      endif


      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sorg_leaf_number1(
     .          g_current_stage,
     .          g_days_tot,
     .          g_phase_tt,

     .          c_leaf_init_rate,
     .          c_leaf_no_seed,
     .          c_leaf_no_min,
     .          c_leaf_no_max,
     .          g_leaf_no_final,
     .          g_leaf_no,
     .          c_leaf_no_rate_change,
     .          c_leaf_app_rate2,
     .          c_leaf_app_rate1,
     .          g_dlt_tt,
     .          g_dlt_leaf_no,
     .          g_node_no)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       g_current_stage
      real       g_days_tot(*)
      real       g_phase_tt(*)
      real       c_leaf_init_rate
*
      real       c_leaf_no_seed
      real       c_leaf_no_min
      real       c_leaf_no_max
      real       g_leaf_no_final         ! (OUTPUT) maximum total leaf number
*
      real       g_leaf_no(*)
      real       c_leaf_no_rate_change
      real       c_leaf_app_rate2
      real       c_leaf_app_rate1
      real       g_dlt_tt
      real       g_dlt_leaf_no           ! (OUTPUT) new fraction of oldest
                                         ! expanding leaf
      real       g_node_no(*)

*+  Purpose
*     Initialises final leaf number and controls leaf appearance
*
*   Called by _process
*
*   Number of options: 1
*   Option 1:
*     Applies to cereals
*   Calls srop_leaf_number_final1, srop_leaf_appearance1 in crop.for

*+  Changes
*      5/9/96 dph

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sproc_leaf_number1')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

         call sorg_leaf_number_final1 (
     .          emerg,
     .          floral_init,
     .          plant_end,

     .          g_current_stage,
     .          g_days_tot,
     .          g_phase_tt,
     .          c_leaf_init_rate,
     .          c_leaf_no_seed,
     .          c_leaf_no_min,
     .          c_leaf_no_max,
     .          g_leaf_no_final)

         call sorg_leaf_appearance1 (
     .          g_leaf_no,
     .          g_leaf_no_final,
     .          c_leaf_no_rate_change,
     .          c_leaf_app_rate2,
     .          c_leaf_app_rate1,
     .          g_current_stage,
     .          g_days_tot,
     .          g_dlt_tt,
     .          g_dlt_leaf_no) ! fraction of leaf emerged

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sorg_leaf_number_final1 (
     .          start_leaf_init,
     .          end_leaf_init,
     .          reset_stage,

     .          g_current_stage,
     .          g_days_tot,
     .          g_phase_tt,
     .          c_leaf_init_rate,
     .          c_leaf_no_seed,
     .          c_leaf_no_min,
     .          c_leaf_no_max,
     .          g_leaf_no_final)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    start_leaf_init !stage to begin (e.g. emerg) est. final leaf no.
      integer    end_leaf_init !stage to end (e.g. floral_init) est. final leaf no.
      integer    reset_stage   !stage to reset final leaf no.
*
      real       g_current_stage
      real       g_days_tot(*)
      real       g_phase_tt(*)
      real       c_leaf_init_rate
      real       c_leaf_no_seed
      real       c_leaf_no_min
      real       c_leaf_no_max
      real       g_leaf_no_final         ! (OUTPUT) maximum total leaf number

*+  Purpose
*       Calculate total leaf number.  This is set at floral_initialisation and
*       is set at an approximated number at germination to allow
*       other calculations to proceed until the correct number is known.
*
*   Called by srop_leaf_number(1) in croptree.for

*+  Changes
*     010994 jngh specified and programmed
*     070495 psc  changed from emerg to germ
*     0596   glh  fixed it up

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sorg_leaf_number_final1')

*+  Local Variables
      real       tt_floral_init        ! cumulative dtt from sowing
                                       ! to true floral initiation (deg day)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

         ! set total leaf number

      if (stage_is_between(start_leaf_init, end_leaf_init
     .     , g_current_stage)
     .      .or.
     .      on_day_of (end_leaf_init, g_current_stage, g_days_tot))
     .      then

          ! estimate the final leaf no from an approximated thermal
          ! time for the period from emergence to floral initiation.

        tt_floral_init = sum_between(start_leaf_init, end_leaf_init
     .     ,g_phase_tt)

        g_leaf_no_final = divide (tt_floral_init
     :                         , c_leaf_init_rate, 0.0)
     :                 + c_leaf_no_seed

         call bound_check_real_var (g_leaf_no_final
     :                            , c_leaf_no_min, c_leaf_no_max
     :                            , 'g_leaf_no_final')

      elseif (on_day_of (reset_stage, g_current_stage, g_days_tot))
     . then
         g_leaf_no_final = 0.0

      endif
      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sorg_leaf_appearance1 (
     .          g_leaf_no,
     .          g_leaf_no_final,
     .          c_leaf_no_rate_change,
     .          c_leaf_app_rate2,
     .          c_leaf_app_rate1,
     .          g_current_stage,
     .          g_days_tot,
     .          g_dlt_tt,
     .          g_dlt_leaf_no)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       g_leaf_no(*)
      real       g_leaf_no_final
      real       c_leaf_no_rate_change
      real       c_leaf_app_rate2
      real       c_leaf_app_rate1
      real       g_current_stage
      real       g_days_tot(*)
      real       g_dlt_tt
      real       g_dlt_leaf_no           ! (OUTPUT) new fraction of oldest
                                       ! expanding leaf

*+  Purpose
*       Return the fractional increase in emergence of the oldest
*       expanding leaf.
*       Note ! this does not take account of the other younger leaves
*       that are currently expanding
*
*   Called by srop_leaf_number(1) in croptree.for

*+  Changes
*       031194 jngh specified and programmed
*       070495 psc  added 2nd leaf appearance rate
*       260596 glh  corrected error in leaf no calcn

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sorg_leaf_appearance1')

*+  Local Variables
      real       leaf_no_remaining     ! number of leaves to go before all
                                       ! are fully expanded
      real       leaf_no_now           ! number of fully expanded leaves
      real       leaf_app_rate         ! rate of leaf appearance (oCd/leaf)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

cglh uses sowing, not emerg to calc leaf no.

      leaf_no_now = sum_between (emerg, now, g_leaf_no)
      leaf_no_remaining = g_leaf_no_final - leaf_no_now

!scc Peter's 2 stage version used here, modified to apply
! to last few leaves before flag
!i.e. c_leaf_no_rate_change is leaf number from the top down (e.g. 4)

      if (leaf_no_remaining .le. c_leaf_no_rate_change) then

         leaf_app_rate = c_leaf_app_rate2

      else

         leaf_app_rate = c_leaf_app_rate1

      endif


      if (on_day_of (emerg, g_current_stage, g_days_tot)) then

             ! initialisation done elsewhere.
         g_dlt_leaf_no = 0.0

      elseif (leaf_no_remaining.gt.0.0) then

             ! if leaves are still growing, the cumulative number of
             ! phyllochrons or fully expanded leaves is calculated from
             ! daily thermal time for the day.

         g_dlt_leaf_no = divide (g_dlt_tt, leaf_app_rate, 0.0)
         g_dlt_leaf_no = bound (g_dlt_leaf_no, 0.0, leaf_no_remaining)

      else
             ! we have full number of leaves.

         g_dlt_leaf_no = 0.0

      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sorg_phenology2 (
     .       g_previous_stage,
     .       g_current_stage,

     .       g_maxt, g_mint,
     .       c_x_temp, c_y_tt,
     .       c_num_temp, g_dlt_tt,

     :       C_num_sw_avail_ratio,
     :       C_x_sw_avail_ratio, C_y_swdef_pheno, G_dlayer,
     :       g_root_depth, g_sw_avail, g_sw_avail_pot, g_swdef_pheno,

     .       g_dm_green,
     .       g_N_conc_crit, g_N_conc_min, g_N_green,
     .       c_N_fact_pheno, g_nfact_pheno,

     .          g_days_tot,
     .          g_sowing_depth,
     .          g_tt_tot,
     .          g_phase_tt,

     .          g_sw_dep,
     .          p_ll_dep,
     .          c_pesw_germ,

     .          g_dlt_stage,

     .          c_tt_base,
     .          c_tt_opt,
     .          g_tt_tot_fm,
     .          g_dlt_tt_fm,
     .          g_sw_supply_demand_ratio)


*     ===========================================================
      Use infrastructure
      implicit none
!      include 'stress.inc' !to set value of photo - not sure if correct way

*+  Purpose
*     Use temperature, photoperiod and genetic characteristics
*     to determine when the crop begins a new growth phase.
*     The initial daily thermal time and height are also set.
*     Has a different thermal time during grain filling....
*
*   Called by _process
*
*   Number of options: 1
*   Option 1:
*     Designed for cereals...???
*   Calls srop_pheno_swdef_fact1, srop_pheno_n_fact1
*         srop_thermal_time1,
*         srop_phenology_init1, srop_phase_devel1, srop_devel1 in crop.for

*+  Notes
cscc 030997 HAVE to generalise this routine. Could do so by being able to
c specify init routine and stages to apply water and N stress
cscc Needs to incorporate water stress and low N effects on phenology
c usually by slowing down leaf appearance in vegetative phase
c and often hastening leaf senescence in grainfilling phase
c Water stress effect during grainfilling is partly because the canopy heats up
c more than it would if it were irrigated. Really need to predict canopy temp.
c somehow ...
c But if slow down leaf appearance etc. need to relate that to the leaf area mod
c (how do we do this w. TPLA approach?)

*+  Changes
*     010994 jngh specified and programmed

*+  Calls
      real       g_maxt
      real       g_mint
      real       c_x_temp(*)
      real       c_y_tt(*)
      integer    c_num_temp
      real       g_dlt_tt                ! (OUTPUT) daily thermal time (oC)
*
      INTEGER c_num_sw_avail_ratio  ! (INPUT)
      REAL    c_x_sw_avail_ratio(*) ! (INPUT)
      REAL    c_y_swdef_pheno(*)    ! (INPUT)
      REAL    g_dlayer(max_layer)           ! (INPUT)  thickness of soil layer I (mm)
      REAL    g_root_depth          ! (INPUT)  depth of roots (mm)
      REAL    g_sw_avail(max_layer)         ! (INPUT)  actual extractable soil water (mm
      REAL    g_sw_avail_pot(max_layer)     ! (INPUT)  potential extractable soil water
      REAL    G_swdef_pheno         ! (OUTPUT) sw stress factor (0-1)
      REAL    G_nfact_pheno         ! (OUTPUT) sw stress factor (0-1)
*
      REAL       g_dm_green(max_part)         ! (INPUT)  live plant dry weight (biomass
      REAL       g_n_conc_crit(max_part)      ! (INPUT)  critical N concentration (g N/
      REAL       g_n_conc_min(max_part)       ! (INPUT)  minimum N concentration (g N/g
      REAL       g_n_green(max_part)          ! (INPUT)  plant nitrogen content (g N/m^
      REAL       c_n_fact_pheno        ! (INPUT)  multipler for N deficit effect
*
      real      g_days_tot(*)
!      real      c_shoot_lag
      real      g_sowing_depth
!      real      c_shoot_rate
!      real      p_tt_emerg_to_endjuv
!      real      p_tt_endjuv_to_init
!      integer   g_day_of_year
!      real      g_latitude
!      real      c_twilight
!      real      p_photoperiod_crit1
!      real      p_photoperiod_crit2
!      real      p_photoperiod_slope
!      real      g_leaf_no_final
!      real      c_leaf_no_rate_change
!      real      c_leaf_no_at_emerg
!      real      c_leaf_app_rate1
!      real      c_leaf_app_rate2
      real      g_tt_tot(*)
!      real      p_tt_flag_to_flower
!      real      p_tt_flower_to_start_grain
!      real      p_tt_flower_to_maturity
!      real      p_tt_maturity_to_ripe
      real      G_phase_tt (*) ! (INPUT/OUTPUT) cumulative growing
                               ! degree days required for stage (deg days)
*
      real       g_sw_dep(max_layer)
      real       p_ll_dep(max_layer)
      real       c_pesw_germ
      real       G_phase_dvl           ! (OUTPUT) fraction of current phase elap
*
      real       g_dlt_stage             ! (OUTPUT) change in growth stage
      real       g_current_stage         ! (OUTPUT) new stage no.
      real       g_previous_stage         ! (OUTPUT) new stage no.
*
      real       c_tt_base      !variables used in thermal time for GF
      real       c_tt_opt       !variables used in thermal time for GF
      real       g_tt_tot_fm(*)    !variables used in thermal time for GF
      real       g_dlt_tt_fm    !variables used in thermal time for GF
      real       g_sw_supply_demand_ratio
      character  string*200            ! output string

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sproc_phenology2')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

!alternative option for thermal time during grain_fill
!has 2 extra variables -
!     .          g_tt_tot_fm,
!     .          g_dlt_tt_fm,

         g_previous_stage = g_current_stage

         call srop_thermal_time1 (
     .          g_maxt, g_mint,
     .          c_x_temp, c_y_tt,
     .          c_num_temp, g_dlt_tt)

         call srop_thermal_time2 (
     .          g_maxt, g_mint,
     .          c_tt_base, c_tt_opt,
     .          g_dlt_tt_fm)

            !Modify g_dlt_tt by stress factors

       if (stage_is_between(sowing, flowering, g_current_stage)) then

!       g_swdef_pheno left alone


         if (stage_is_between(sowing, endjuv, g_current_stage))
     :          g_nfact_pheno = 1.0
         if (g_nfact_pheno .lt. 0.5) g_nfact_pheno = 0.5

          g_dlt_tt = g_dlt_tt *
     :             min (g_swdef_pheno, g_nfact_pheno)

         else

            g_dlt_tt = g_dlt_tt

         endif

         ! initialise phenology phase targets

!Determine fraction of phase that has elapsed
!    If stage is between flowering and maturity, use this function to calc
!       daily thermal time for phenology decisions only

         if (stage_is_between(flowering, maturity,
     .                g_current_stage)) then
            call srop_phase_devel1(
     .          g_current_stage,
     .          g_sowing_depth,
     .          g_dlayer,
     .          g_sw_dep,
     .          p_ll_dep,
     .          c_pesw_germ,
     .          g_days_tot,
     .          g_tt_tot_fm,
     .          g_dlt_tt_fm,
     .          g_phase_tt,
     .          g_phase_dvl)
        else
           call srop_phase_devel1(
     .          g_current_stage,
     .          g_sowing_depth,
     .          g_dlayer,
     .          g_sw_dep,
     .          p_ll_dep,
     .          c_pesw_germ,
     .          g_days_tot,
     .          g_tt_tot,
     .          g_dlt_tt,
     .          g_phase_tt,
     .          g_phase_dvl)
        endif

! calculate new delta and the new stage

         call srop_devel1 (
     .          g_dlt_stage,
     .          g_current_stage,
     .          g_phase_dvl,
     .          max_stage)

         call accumulate (g_dlt_tt, g_tt_tot
     :               , g_previous_stage, g_dlt_stage)

         call accumulate (1.0, g_days_tot
     :               , g_previous_stage, g_dlt_stage)

         call accumulate (g_dlt_tt_fm, g_tt_tot_fm
     :               , g_previous_stage, g_dlt_stage)

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sorg_phen_init (
!     .          germ_stage,
!     .          emerg_stage,
!     .          begin_PP_sensitive_stage, !end_juv
!     .          germ_stage,

     .          g_current_stage,
     .          g_days_tot,
     .          c_shoot_lag,
     .          g_sowing_depth,
     .          c_shoot_rate,
     .          p_tt_emerg_to_endjuv,
     .          p_tt_endjuv_to_init,
     .          g_day_of_year,
     .          g_latitude,
     .          c_twilight,
     .          p_photoperiod_crit1,
     .          p_photoperiod_crit2,
     .          p_photoperiod_slope,
     .          g_leaf_no_final,
     .          c_leaf_no_rate_change,
     .          c_leaf_no_at_emerg,
     .          c_leaf_app_rate1,
     .          c_leaf_app_rate2,
     .          p_tt_flag_to_flower,
     .          p_tt_flower_to_start_grain,
     .          p_tt_flower_to_maturity,
     .          p_tt_maturity_to_ripe,
     .          g_phase_tt)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real      g_current_stage
      real      g_days_tot(*)
      real      c_shoot_lag
      real      g_sowing_depth
      real      c_shoot_rate
      real      p_tt_emerg_to_endjuv
      real      p_tt_endjuv_to_init
      integer   g_day_of_year
      real      g_latitude
      real      c_twilight
      real      p_photoperiod_crit1
      real      p_photoperiod_crit2
      real      p_photoperiod_slope
      real      g_leaf_no_final
      real      c_leaf_no_rate_change
      real      c_leaf_no_at_emerg
      real      c_leaf_app_rate1
      real      c_leaf_app_rate2
      real      p_tt_flag_to_flower
      real      p_tt_flower_to_start_grain
      real      p_tt_flower_to_maturity
      real      p_tt_maturity_to_ripe
      real      g_phase_tt (*)           ! (INPUT/OUTPUT) cumulative growing
                                       ! degree days required for
                                       ! each stage (deg days)

*+  Purpose
*       Returns cumulative thermal time targets required for the
*       individual developmental stages.
*
*   Called by srop_phenology(1) in croptree.for

*+  Changes
*     010994 jngh specified and programmed
*     070495 psc added 2nd leaf appearance rate
*     090695 psc l_bound added (otherwise won't progress if g_phase_tt=0)
*     120995 glh restructured routine

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sorg_phen_init')

*+  Local Variables
      real       tt_emerg_to_flag_leaf ! thermal time to develop
                                       ! and fully expand all leaves (oC)
      real       photoperiod           ! daylength (hours)
*
      real       leaf_no               ! leaf no. above which app. rate changes

*- Implementation Section ----------------------------------

      call push_routine (my_name)

         ! set estimates of phase thermal time targets at germination

      if (on_day_of (germ, g_current_stage, g_days_tot)) then
         g_phase_tt(germ_to_emerg) = c_shoot_lag
     :                             + g_sowing_depth*c_shoot_rate
         g_phase_tt(emerg_to_endjuv) = p_tt_emerg_to_endjuv
         g_phase_tt(endjuv_to_init) = p_tt_endjuv_to_init

         ! revise thermal time target for floral initialisation at emergence

      elseif (on_day_of (emerg, g_current_stage, g_days_tot) .or.
     :        stage_is_between (emerg, endjuv, g_current_stage) .or.
     :        on_day_of (endjuv, g_current_stage, g_days_tot)) then

         photoperiod = day_length (g_day_of_year, g_latitude,c_twilight)
         if (photoperiod.le.p_photoperiod_crit1) then
            g_phase_tt(endjuv_to_init) = p_tt_endjuv_to_init
         elseif (photoperiod.lt.p_photoperiod_crit2) then
            g_phase_tt(endjuv_to_init) = p_tt_endjuv_to_init +
     :      p_photoperiod_slope*(photoperiod - p_photoperiod_crit1)
         elseif (photoperiod.ge.p_photoperiod_crit2) then
            g_phase_tt(endjuv_to_init) = p_tt_endjuv_to_init +
     :      p_photoperiod_slope*(p_photoperiod_crit2
     :                              - p_photoperiod_crit1)
         else
         endif

         leaf_no = max (g_leaf_no_final - c_leaf_no_rate_change,
     :                 c_leaf_no_at_emerg)
         leaf_no = min (leaf_no, g_leaf_no_final)
         tt_emerg_to_flag_leaf = (leaf_no - c_leaf_no_at_emerg)
     :                         * c_leaf_app_rate1
     :                         + (g_leaf_no_final - leaf_no)
     :                         * c_leaf_app_rate2

         g_phase_tt(init_to_flag) = tt_emerg_to_flag_leaf
     :              - g_phase_tt(emerg_to_endjuv)
     :              - g_phase_tt(endjuv_to_init)

         g_phase_tt(flag_to_flower) = p_tt_flag_to_flower

         g_phase_tt(flower_to_start_grain) =
     :                    p_tt_flower_to_start_grain

         g_phase_tt(end_grain_to_maturity) =
     :                  0.05*p_tt_flower_to_maturity

         g_phase_tt(start_to_end_grain) = p_tt_flower_to_maturity
     :                  - g_phase_tt(flower_to_start_grain)
     :                  - g_phase_tt(end_grain_to_maturity)
         g_phase_tt(maturity_to_ripe) = p_tt_maturity_to_ripe
      else
          ! do nothing
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sorg_phenology3 (
     .       g_previous_stage,
     .       g_current_stage,

     .       g_maxt, g_mint,
     .       c_x_temp, c_y_tt,
     .       c_num_temp, g_dlt_tt,

     :       C_num_sw_avail_ratio,
     :       C_x_sw_avail_ratio, C_y_swdef_pheno, G_dlayer,
     :       g_root_depth, g_sw_avail, g_sw_avail_pot, g_swdef_pheno,

     .       g_dm_green,
     .       g_N_conc_crit, g_N_conc_min, g_N_green,
     .       c_N_fact_pheno, g_nfact_pheno,

     .          g_days_tot,
     .          c_shoot_lag,
     .          g_sowing_depth,
     .          c_shoot_rate,
     .          p_tt_emerg_to_endjuv,
     .          p_tt_endjuv_to_init,
     .          g_day_of_year,
     .          g_latitude,
     .          c_twilight,
     .          p_photoperiod_crit1,
     .          p_photoperiod_crit2,
     .          p_photoperiod_slope,
     .          g_leaf_no_final,
     .          c_leaf_no_rate_change,
     .          c_leaf_no_at_emerg,
     .          c_leaf_app_rate1,
     .          c_leaf_app_rate2,
     .          g_tt_tot,
     .          p_tt_flag_to_flower,
     .          p_tt_flower_to_start_grain,
     .          p_tt_flower_to_maturity,
     .          p_tt_maturity_to_ripe,
     .          g_phase_tt,

     .          g_sw_dep,
     .          p_ll_dep,
     .          c_pesw_germ,

     .          g_dlt_stage,

     .          c_tt_base,
     .          c_tt_opt,
     .          g_tt_tot_fm,
     .          g_dlt_tt_fm,
     .          g_sw_supply_demand_ratio)



*     ===========================================================
      Use infrastructure
      implicit none
!      include 'stress.inc' !to set value of photo - not sure if correct way

*+  Purpose
*     Use temperature, photoperiod and genetic characteristics
*     to determine when the crop begins a new growth phase.
*     The initial daily thermal time and height are also set.
*     Has a different thermal time during grain filling....
*
*   Called by _process
*
*   Number of options: 1
*   Option 1:
*     Designed for cereals...???
*   Calls srop_pheno_swdef_fact1, srop_pheno_n_fact1
*         srop_thermal_time1,
*         srop_phenology_init1, srop_phase_devel1, srop_devel1 in crop.for

*+  Notes
cscc 030997 HAVE to generalise this routine. Could do so by being able to
c specify init routine and stages to apply water and N stress
cscc Needs to incorporate water stress and low N effects on phenology
c usually by slowing down leaf appearance in vegetative phase
c and often hastening leaf senescence in grainfilling phase
c Water stress effect during grainfilling is partly because the canopy heats up
c more than it would if it were irrigated. Really need to predict canopy temp.
c somehow ...
c But if slow down leaf appearance etc. need to relate that to the leaf area mod
c (how do we do this w. TPLA approach?)

*+  Changes
*     010994 jngh specified and programmed

*+  Calls
      real       g_maxt
      real       g_mint
      real       c_x_temp(*)
      real       c_y_tt(*)
      integer    c_num_temp
      real       g_dlt_tt                ! (OUTPUT) daily thermal time (oC)
*
      INTEGER c_num_sw_avail_ratio  ! (INPUT)
      REAL    c_x_sw_avail_ratio(*) ! (INPUT)
      REAL    c_y_swdef_pheno(*)    ! (INPUT)
      REAL    g_dlayer(max_layer)           ! (INPUT)  thickness of soil layer I (mm)
      REAL    g_root_depth          ! (INPUT)  depth of roots (mm)
      REAL    g_sw_avail(max_layer)         ! (INPUT)  actual extractable soil water (mm
      REAL    g_sw_avail_pot(max_layer)     ! (INPUT)  potential extractable soil water
      REAL    G_swdef_pheno         ! (OUTPUT) sw stress factor (0-1)
      REAL    G_nfact_pheno         ! (OUTPUT) sw stress factor (0-1)
*
      REAL       g_dm_green(max_part)         ! (INPUT)  live plant dry weight (biomass
      REAL       g_n_conc_crit(max_part)      ! (INPUT)  critical N concentration (g N/
      REAL       g_n_conc_min(max_part)       ! (INPUT)  minimum N concentration (g N/g
      REAL       g_n_green(max_part)          ! (INPUT)  plant nitrogen content (g N/m^
      REAL       c_n_fact_pheno        ! (INPUT)  multipler for N deficit effect
*
      real      g_days_tot(*)
      real      c_shoot_lag
      real      g_sowing_depth
      real      c_shoot_rate
      real      p_tt_emerg_to_endjuv
      real      p_tt_endjuv_to_init
      integer   g_day_of_year
      real      g_latitude
      real      c_twilight
      real      p_photoperiod_crit1
      real      p_photoperiod_crit2
      real      p_photoperiod_slope
      real      g_leaf_no_final
      real      c_leaf_no_rate_change
      real      c_leaf_no_at_emerg
      real      c_leaf_app_rate1
      real      c_leaf_app_rate2
      real      g_tt_tot(*)
      real      p_tt_flag_to_flower
      real      p_tt_flower_to_start_grain
      real      p_tt_flower_to_maturity
      real      p_tt_maturity_to_ripe
      real      G_phase_tt (*) ! (INPUT/OUTPUT) cumulative growing
                               ! degree days required for stage (deg days)
*
      real       g_sw_dep(max_layer)
      real       p_ll_dep(max_layer)
      real       c_pesw_germ
      real       G_phase_dvl           ! (OUTPUT) fraction of current phase elap
*
      real       g_dlt_stage             ! (OUTPUT) change in growth stage
      real       g_current_stage         ! (OUTPUT) new stage no.
      real       g_previous_stage         ! (OUTPUT) new stage no.
*
      real       c_tt_base      !variables used in thermal time for GF
      real       c_tt_opt       !variables used in thermal time for GF
      real       g_tt_tot_fm(*)    !variables used in thermal time for GF
      real       g_dlt_tt_fm    !variables used in thermal time for GF
      real       g_sw_supply_demand_ratio

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sorg_phenology3')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

!alternative option for thermal time during grain_fill
!has 2 extra variables -
!     .          g_tt_tot_fm,
!     .          g_dlt_tt_fm,

         g_previous_stage = g_current_stage

         call crop_thermal_time
     :               (
     :                C_num_temp
     :              , C_x_temp
     :              , C_y_tt
     :              , G_current_stage
     :              , G_maxt
     :              , G_mint
     :              , emerg             !start_stress_stage
     :              , flowering         !end_stress_stage
     :              , G_nfact_pheno
     :              , G_swdef_pheno
     :              , g_dlt_tt
     :               )

!         call srop_thermal_time1 (
!     .          g_maxt, g_mint,
!     .          c_x_temp, c_y_tt,
!     .          c_num_temp, g_dlt_tt)

!should generalise this....
         call srop_thermal_time2 (
     .          g_maxt, g_mint,
     .          c_tt_base, c_tt_opt,
     .          g_dlt_tt_fm)

         if (stage_is_between(emerg, flowering, g_current_stage))then

            !Modify g_dlt_tt by stress factors

          if(stage_is_between(emerg,floral_init,g_current_stage))then

             call srop_pheno_swdef_fact1(C_num_sw_avail_ratio,
     :        C_x_sw_avail_ratio, C_y_swdef_pheno, max_layer,G_dlayer,
     :        G_root_depth, G_sw_avail, G_sw_avail_pot, g_swdef_pheno)

          else

             call srop_pheno_swdef_fact2 (
     .        g_sw_supply_demand_ratio,
     .        g_swdef_pheno)

          endif
!         prevent N stress on phenology before end juvenile  GMC
          if (stage_is_between(sowing, endjuv, g_current_stage))then
             g_nfact_pheno = 1.0
          else
             call srop_pheno_N_fact1(leaf, stem, g_dm_green,
     .      g_N_conc_crit, g_N_conc_min, g_N_green,
     .      c_N_fact_pheno, g_nfact_pheno)
          endif

!         g_nfact_pheno = 1.0 !Needed for SLN model at present...

          g_dlt_tt = g_dlt_tt *
     :             min (g_swdef_pheno, g_nfact_pheno)

         else

            g_dlt_tt = g_dlt_tt

         endif

         ! initialise phenology phase targets

!Determine fraction of phase that has elapsed
!    If stage is between flowering and maturity, use this function to calc
!       daily thermal time for phenology decisions only

         if (stage_is_between(flowering, maturity,
     .                g_current_stage)) then
            call srop_phase_devel1(
     .          g_current_stage,
     .          g_sowing_depth,
     .          g_dlayer,
     .          g_sw_dep,
     .          p_ll_dep,
     .          c_pesw_germ,
     .          g_days_tot,
     .          g_tt_tot_fm,
     .          g_dlt_tt_fm,
     .          g_phase_tt,
     .          g_phase_dvl)
        else
           call srop_phase_devel1(
     .          g_current_stage,
     .          g_sowing_depth,
     .          g_dlayer,
     .          g_sw_dep,
     .          p_ll_dep,
     .          c_pesw_germ,
     .          g_days_tot,
     .          g_tt_tot,
     .          g_dlt_tt,
     .          g_phase_tt,
     .          g_phase_dvl)
        endif

! calculate new delta and the new stage

!         call srop_devel1 (
!     .          g_dlt_stage,
!     .          g_current_stage,
!     .          g_phase_dvl,
!     .          max_stage)

         call crop_devel
     :               (
     :                G_current_stage
     :              , max_stage
     :              , G_phase_dvl
     :              , g_dlt_stage, g_current_stage
     :               )

         call accumulate (g_dlt_tt, g_tt_tot
     :               , g_previous_stage, g_dlt_stage)

         call accumulate (1.0, g_days_tot
     :               , g_previous_stage, g_dlt_stage)

         call accumulate (g_dlt_tt_fm, g_tt_tot_fm
     :               , g_previous_stage, g_dlt_stage)

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
*     Routines from CropLib and CropProc ========================
*     CropLib Routines ==========================================
*     ===========================================================
      subroutine srop_phase_devel1 (
     .          g_current_stage,
     .          g_sowing_depth,
     .          g_dlayer,
     .          g_sw_dep,
     .          p_ll_dep,
     .          c_pesw_germ,
     .          g_days_tot,
     .          g_tt_tot,
     .          g_dlt_tt,
     .          g_phase_tt,
     .          phase_dvl)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       g_current_stage
      real       g_sowing_depth
      real       g_dlayer(*)
      real       g_sw_dep(*)
      real       p_ll_dep(*)
      real       c_pesw_germ
      real       g_days_tot(*)
      real       g_tt_tot(*)
      real       g_dlt_tt
      real       g_phase_tt(*)
      real       phase_dvl           ! (OUTPUT) fraction of current phase elap

*+  Purpose
*     Determine the fraction of current phase elapsed ().
*
*   Called by srop_phenology(1) in croptree.for
*   Calls srop_germination and srop_phase_tt in crop.for

*+  Changes
*     010994 jngh specified and programmed

*+  Calls

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_phase_devel1')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (stage_is_between (sowing, germ, g_current_stage)) then
         phase_dvl = srop_germination (
     .          g_sowing_depth,
     .          g_dlayer,
     .          g_sw_dep,
     .          p_ll_dep,
     .          c_pesw_germ,
     .          g_current_stage,
     .          g_days_tot)

      elseif (stage_is_between (germ, harvest_ripe
     :                        , g_current_stage)) then

         phase_dvl =  srop_phase_tt (
     .          g_current_stage,
     .          g_tt_tot,
     .          g_dlt_tt,
     .          g_phase_tt)

      else
         phase_dvl = mod(g_current_stage, 1.0)

      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine srop_pheno_N_fact1(
     .          leaf, stem,
     .          g_dm_green,
     .          g_n_conc_crit,
     .          g_n_conc_min,
     .          g_n_green,
     .          c_n_fact_pheno,
     .          g_nfact)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer leaf
      integer stem
      REAL       g_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
      REAL       g_n_conc_crit(*)      ! (INPUT)  critical N concentration (g N/
      REAL       g_n_conc_min(*)       ! (INPUT)  minimum N concentration (g N/g
      REAL       g_n_green(*)          ! (INPUT)  plant nitrogen content (g N/m^
      REAL       c_n_fact_pheno        ! (INPUT)  multipler for N deficit effect
      real       g_nfact                 ! (OUTPUT) N stress factor

*+  Purpose
*     The concentration of Nitrogen in leaves is used to derive a
*     series of Nitrogen stress indices.  The stress indices for
*     photosynthesis and cell expansion are calculated from today's
*     relative nutritional status between a critical and minimum
*     leaf Nitrogen concentration
*
*   Called by srop_phenology(1) in croptree.for
*   Calls srop_n_conc_ratio

*+  Changes
*     060495 nih taken from template
*     970215 slw split from mungb_nfact

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_pheno_N_fact1')

*+  Local Variables
      real       N_conc_ratio          ! available N as fraction of N capacity
      real       N_def                 ! N factor (0-1)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call srop_N_conc_ratio(leaf, stem, g_dm_green,
     :                       g_n_conc_crit, g_n_conc_min,
     :                       g_n_green, N_conc_ratio)

      N_def = c_n_fact_pheno * N_conc_ratio
      g_nfact = bound (N_def, 0.0, 1.0)

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine srop_pheno_swdef_fact1(
     .          c_num_sw_avail_ratio,
     .          c_x_sw_avail_ratio,
     .          c_y_swdef_pheno,
     .          num_layer,
     .          g_dlayer,
     .          g_root_depth,
     .          g_sw_avail,
     .          g_sw_avail_pot,
     .          g_swdef)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      INTEGER c_num_sw_avail_ratio  ! (INPUT)
      REAL    c_x_sw_avail_ratio(*) ! (INPUT)
      REAL    c_y_swdef_pheno(*)    ! (INPUT)
      INTEGER num_layer           ! (INPUT)  number of layers in profile
      REAL    g_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL    g_root_depth          ! (INPUT)  depth of roots (mm)
      REAL    g_sw_avail(*)         ! (INPUT)  actual extractable soil water (mm
      REAL    g_sw_avail_pot(*)     ! (INPUT)  potential extractable soil water
      REAL    g_swdef               ! (OUTPUT) sw stress factor (0-1)

*+  Purpose
*       Get the soil water availability factor (0-1), commonly
*       called soil water deficit factor. 1 is no stress, 0 is full stress.
*
*   Called by srop_phenology(1) in croptree.for

*+  Changes
*       010994 jngh specified and programmed
*       970216 slw generalised to avoid common blocks , added num_layer paramete

*+  Constant Values
      character  my_name*(*)      ! name of procedure
      parameter (my_name = 'srop_pheno_swdef_fact1')

*+  Local Variables
      integer deepest_layer       ! deepest layer in which the roots are growing
      real    sw_avail_ratio      ! water availability ratio
      real    sw_avail_pot_sum    ! potential extractable soil water (mm)
      real    sw_avail_sum        ! actual extractable soil water (mm)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      deepest_layer = find_layer_no (g_root_depth, g_dlayer, num_layer)
      sw_avail_pot_sum = sum_real_array (g_sw_avail_pot, deepest_layer)
      sw_avail_sum = sum_real_array (g_sw_avail, deepest_layer)
      sw_avail_ratio = divide (sw_avail_sum, sw_avail_pot_sum, 1.0) !???
      sw_avail_ratio = bound (sw_avail_ratio , 0.0, 1.0)
      g_swdef = linear_interp_real(sw_avail_ratio, c_x_sw_avail_ratio,
     :                           c_y_swdef_pheno, c_num_sw_avail_ratio)

      call pop_routine (my_name)
      return
      end subroutine



* ====================================================================
       subroutine srop_pheno_swdef_fact2 (
     .    g_sw_supply_demand_ratio,
     .    g_swdef_pheno)

* ====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real g_sw_supply_demand_ratio
      real g_swdef_pheno

*+  Purpose
*     <insert here>

*+  Changes
*     10-10-1997 - chapman - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'srop_pheno_swdef_fact2')

*+  Local Variables
      real c_critical_sd_ratio
      real c_sd_slope

*- Implementation Section ----------------------------------
      call push_routine (myname)

!cscc 8/11/95 Change in stress approach to one that effectively
! extends targets...
!rlv 9/10/97 changed to affect dlt_tt not extend target
!
! Donatelli et al. Crop Sci. 1992. trans. ratio effect on phenology
!3/11/97 SCC - GMC to change this to a table function

        c_critical_sd_ratio= 0.55
        c_sd_slope= 0.61

        if(g_sw_supply_demand_ratio.ge.c_critical_sd_ratio) then
            g_swdef_pheno = 1.0
        else
            g_swdef_pheno = 1.0/(1.0 + c_sd_slope
     :      * (c_critical_sd_ratio - g_sw_supply_demand_ratio))
        endif

      call pop_routine (myname)
      return
      end subroutine



*     ===========================================================
      subroutine srop_N_Conc_Ratio(
     .          leaf,stem,
     .          g_dm_green,
     .          g_n_conc_crit,
     .          g_n_conc_min,
     .          g_n_green,
     .          g_N_conc_ratio)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer leaf
      integer stem
      REAL       g_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
      REAL       g_n_conc_crit(*)      ! (INPUT)  critical N concentration (g N/
      REAL       g_n_conc_min(*)       ! (INPUT)  minimum N concentration (g N/g
      REAL       g_n_green(*)          ! (INPUT)  plant nitrogen content (g N/m^
      real       g_N_conc_ratio          ! (OUTPUT) available N as fraction of N

*+  Purpose
*         Use shoot nitrogen concentrations to calculate 0-1 N availability
*         factors.  Since all plant processes are not equally susceptible
*         to N stress, N deficiency factors are calculated from a 0-1 N
*         factor to affect different processes.
*           0 affects grain N potential
*           1 affects photosynthesis
*           2 affects leaf senescence, grain N concentration & cell expansion
*           3 affects grain number
*
*           nfac range is 0.001 to 0.95 or 1.0 for optimum conditions.
*           N_def - 1 range is 0.2012 to .98 or 1 for optimum conditions.
*           N_def - 2 range is .00095 to .9025 or .95 for optimum conditions.
*           N_def - 3 range is .201 to 1 for optimum conditions.
*
*         ???? check that returns 1 & 0 for optimum and zero conditions.
*
*    Called by srop_RUE_N_fact1, srop_pheno_n_fact, srop_grainconc_N_fact1
*      in crop.for

*+  Changes
*     010994 jngh specified and programmed
*     090695 psc  added N_fact for phenology & externalise multipliers for ndef
*     970314 slw extracted this common code from nitrogen stress routines

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_N_conc_ratio')

*+  Local Variables
      real       N_conc_stover         ! tops (stover) actual N concentration
                                       ! (0-1)
      real       dm_stover             ! tops (stover) plant weight (g/m^2)
      real       N_conc_stover_crit    ! tops (stover) critical N concentration
                                       ! (0-1)
      real       N_conc_stover_min     ! tops (stover) minimum N concentration
                                       ! (0-1)
      real       N_leaf_crit           ! critical leaf nitrogen (g/m^2)
      real       N_leaf_min            ! minimum leaf nitrogen (g/m^2)
      real       N_stem_crit           ! critical stem nitrogen (g/m^2)
      real       N_stem_min            ! minimum stem nitrogen (g/m^2)
      real       N_stover              ! tops (stover) plant nitrogen (g/m^2)
      real       N_stover_crit         ! critical top nitrogen (g/m^2)
      real       N_stover_min          ! minimum top nitrogen (g/m^2)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

         ! calculate actual N concentrations

      dm_stover = g_dm_green(leaf) + g_dm_green(stem)
      N_stover = g_n_green(leaf) + g_n_green(stem)

      N_conc_stover = divide (N_stover, dm_stover, 0.0)

         ! calculate critical N concentrations

      N_leaf_crit = g_n_conc_crit(leaf) * g_dm_green(leaf)
      N_stem_crit = g_n_conc_crit(stem) * g_dm_green(stem)
      N_stover_crit = N_leaf_crit + N_stem_crit

      N_conc_stover_crit = divide (N_stover_crit, dm_stover, 0.0)

         ! calculate minimum N concentrations

      N_leaf_min = g_n_conc_min(leaf) * g_dm_green(leaf)
      N_stem_min = g_n_conc_min(stem) * g_dm_green(stem)
      N_stover_min = N_leaf_min + N_stem_min

      N_conc_stover_min = divide (N_stover_min, dm_stover, 0.0)

         ! calculate shortfall in N concentrations

      g_N_conc_ratio = divide ((N_conc_stover - N_conc_stover_min)
     :              , (N_conc_stover_crit - N_conc_stover_min), 0.0)

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      real function srop_phase_tt (
     .          g_current_stage,
     .          g_tt_tot,
     .          g_dlt_tt,
     .          g_phase_tt)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       g_current_stage              ! (INPUT) stage number
      real       g_tt_tot(*)
      real       g_dlt_tt
      real       g_phase_tt(*)

*+  Purpose
*       Return fraction of thermal time we are through the current
*       phenological phase (0-1)
*
*   Called by srop_phase_devel1 in cropopt.for

*+  Changes
*     010994 jngh specified and programmed
*     970518 scc modified, according to JNGH phen bug fix

*+  Calls
!      real       bound                 ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_phase_tt')

*+  Local Variables
      integer    phase                 ! phase number containing stage
      character  string*200
*- Implementation Section ----------------------------------

      call push_routine (my_name)

      phase = int (g_current_stage)
cjh  changed 0.0 to 1.0
      srop_phase_tt = divide (g_tt_tot(phase) + g_dlt_tt
     :                       , g_phase_tt(phase), 1.0)
!      srop_phase_tt = bound (srop_phase_tt, 0.0, 1.999999)

      call pop_routine (my_name)
      return
      end function



*     ===========================================================
      real function srop_germination (
     .          g_sowing_depth,
     .          g_dlayer,
     .          g_sw_dep,
     .          p_ll_dep,
     .          c_pesw_germ,
     .          g_current_stage,
     .          g_days_tot)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       g_sowing_depth
      real       g_dlayer(*)
      real       g_sw_dep(*)
      real       p_ll_dep(*)
      real       c_pesw_germ
      real       g_current_stage
      real       g_days_tot(*)

*+  Purpose
*      Determine germination based on soil water availability
*
*   Called by srop_phase_devel1 in cropopt.for
*
*   Returns srop_germination as a fraction of current phase elapsed

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_germination')

*+  Local Variables
      integer    layer_no_seed         ! seedling layer number
      real       pesw_seed             ! plant extractable soil water in
                                       ! seedling layer available for
                                       ! germination ( mm/mm)

*- Implementation Section ----------------------------------
      call push_routine (my_name)

         ! determine if soil water content is sufficient to allow germination.
         ! Soil water content of the seeded layer must be > the
         ! lower limit to be adequate for germination.

      if (stage_is_between (sowing, germ, g_current_stage)) then

         layer_no_seed = find_layer_no (g_sowing_depth, g_dlayer
     :                                             , max_layer)
         pesw_seed = divide (g_sw_dep(layer_no_seed)
     :                     - p_ll_dep(layer_no_seed)
     :                     , g_dlayer(layer_no_seed), 0.0)

            ! can't germinate on same day as sowing, because miss out on
            ! day of sowing else_where

         if (pesw_seed.gt.c_pesw_germ
     :   .and.
     :   .not. on_day_of (sowing, g_current_stage, g_days_tot)) then
               ! we have germination
               ! set the fraction of the current phase
               ! so it is on the point of germination
            srop_germination = 1.0 + mod (g_current_stage, 1.0)
         else
                ! no germination yet but indicate that we are on the way.
            srop_germination = 0.999
         endif
      else
             ! no sowing yet
         srop_germination = 0.0
      endif

      call pop_routine (my_name)
      return
      end function



*     ===========================================================
      subroutine srop_thermal_time1 (
     .          g_maxt,
     .          g_mint,
     .          c_x_temp,
     .          c_y_tt,
     .          c_num_temp,
     .          g_dlt_tt)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       g_maxt
      real       g_mint
      real       c_x_temp(*)
      real       c_y_tt(*)
      integer    c_num_temp
      real       g_dlt_tt                ! (OUTPUT) daily thermal time (oC)

*+  Purpose
*     Growing degree day (thermal time) is calculated.
*
*   Called by srop_phenology(1) in croptree.for

*+  Notes
*     Eight interpolations of the air temperature are
*     calculated using a three-hour correction factor.
*     For each air three-hour air temperature, a value of growing
*     degree day is calculated.  The eight three-hour estimates
*     are then averaged to obtain the daily value of growing degree
*     days.

*+  Changes
*     140994 jngh specified and programmed
*     090695 psc  added N_fact for phenology stress
*     030997 scc  removed all stress factors - they belong in CROPTREE!

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_thermal_time1')

*+  Local Variables
      real       dly_therm_time        ! thermal time for the day (deg day)

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      dly_therm_time = linint_3hrly_temp (g_maxt, g_mint
     :                 , c_x_temp, c_y_tt
     :                 , c_num_temp)

      g_dlt_tt = dly_therm_time

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine srop_thermal_time2 (
     .          g_maxt,
     .          g_mint,
     .          c_tt_base,
     .          c_tt_opt,
     .          g_dlt_tt_fm)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       g_maxt
      real       g_mint
      real       c_tt_base
      real       c_tt_opt
      real       g_dlt_tt_fm                ! (OUTPUT) daily thermal time (oC)

*+  Purpose
*     Growing degree day (thermal time) is calculated.
*
*     This function used between flowering and maturity
*
*   Called by srop_phenology(1) in croptree.for

*+  Notes
*   G_dlt_tt = 0                  { av_temp <= tt_base oC
*            = av_temp - tt_base  { tt_base < av_temp < tt_opt
*            = tt_opt             { av_temp >= tt_opt
*
*   default values for tt_base = 5.7 and tt_opt = 23.5
*

*+  Changes
*     190997 gmc programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_thermal_time2')

*+  Local Variables
      real       av_temp               ! average daily temp
      real       dly_therm_time        ! thermal time for the day (deg day)

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      av_temp = (g_maxt + g_mint) / 2.0

      if(av_temp .le. c_tt_base) then
         dly_therm_time = 0.0
      else if(av_temp .le. c_tt_opt) then
         dly_therm_time = av_temp - c_tt_base
      else
         dly_therm_time = c_tt_opt - c_tt_base
      endif

      g_dlt_tt_fm = dly_therm_time

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine srop_devel1 (
     .          g_dlt_stage,
     .          g_current_stage,
     .          phase_devel,
     .          max_stage)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       g_dlt_stage             ! (OUTPUT) change in growth stage
      real       g_current_stage         ! (OUTPUT) new stage no.
      real       phase_devel

*+  Purpose
*     Determine the current stage of development.
*
*   Called by srop_phenology(1) in croptree.for

*+  Changes
*     010994 jngh specified and programmed
*     070495 psc add l_bound to dlt-stage
*     970518 scc Fixed Phen bug (JNH notes)

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_devel1')

*+  Local Variables
      real       new_stage             ! new stage number
      integer    max_stage              !New code
      character  string*200            ! output string

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      ! calculate the new delta and the new stage

      new_stage = aint (g_current_stage) + phase_devel
      g_dlt_stage = new_stage - g_current_stage


      if (phase_devel.ge.1.0) then
         g_current_stage = aint (g_current_stage + 1.0)
         if (int(g_current_stage).eq.max_stage) then
            g_current_stage = 1.0
         endif
      else
         g_current_stage = new_stage

      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine srop_leaf_area_sen_frost1(
     .          c_frost_fraction,
     .          c_frost_temp,
     .          c_num_frost_temp,
     .          g_lai,
     .          g_mint,
     .          g_dlt_slai_frost)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      REAL    c_frost_fraction(*) ! (INPUT)
      REAL    c_frost_temp(*)     ! (INPUT)
      INTEGER c_num_frost_temp    ! (INPUT)
      REAL    g_lai               ! (INPUT)  live plant green lai
      REAL    g_mint              ! (INPUT)  minimum air temperature (oC)
      real    g_dlt_slai_frost    ! (OUTPUT) lai frosted today

*+  Purpose
*       Return the lai that would senesce on the
*       current day from low temperatures
*
*   Called from srop_leaf_area_sen(1) in croptree.for

*+  Changes
*     070495 nih taken from template
*     970317 slw templated

*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'srop_leaf_area_sen_frost1')

*+  Local Variables
      real dlt_slai_low_temp    ! lai senesced from low temps
      real sen_fac_temp         ! low temperature factor (0-1)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

          ! low temperature factor
      sen_fac_temp = linear_interp_real(g_mint,c_frost_temp,
     :                      c_frost_fraction,c_num_frost_temp)

      dlt_slai_low_temp = sen_fac_temp * g_lai
      g_dlt_slai_frost = bound (dlt_slai_low_temp, 0.0, g_lai)

      if (g_dlt_slai_frost .gt. 0.0 .and.
     :    reals_are_equal(g_dlt_slai_frost, g_lai)) then
         call Write_string ('Frost kills all leaves.')
      endif
      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine srop_leaf_area_sen_frost2(
     .          c_frost_kill,
     .          g_lai,
     .          g_mint,
     .          g_dlt_slai_frost)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      REAL c_frost_kill        ! (INPUT)  temperature threshold for leaf death (oC
      REAL g_lai               ! (INPUT)  live plant green lai
      REAL g_mint              ! (INPUT)  minimum air temperature (oC)
      real g_dlt_slai_frost    ! (OUTPUT) lai frosted today

*+  Purpose
*+      Return the lai that would senesce on the
*       current day from frosting
*
*   Called from srop_leaf_area_sen(2) in croptree.for

*+  Changes
*     010994 jngh specified and programmed
*     970317 slw templated

*+  Constant Values
      character  my_name*(*) ! name of procedure
      parameter (my_name = 'srop_leaf_area_sen_frost2')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

          ! calculate senecence due to frost
      if (g_mint.le.c_frost_kill) then
         g_dlt_slai_frost = g_lai
      else
         g_dlt_slai_frost = 0.0
      endif
      g_dlt_slai_frost = bound (g_dlt_slai_frost, 0.0, g_lai)

      if (g_dlt_slai_frost .gt. 0.0 .and.
     :    reals_are_equal(g_dlt_slai_frost, g_lai)) then
         call Write_string ('Frost kills all leaves.')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine srop_leaf_area_sen_age2 (
     .          g_current_stage,
     .          g_tt_tot,
     .          p_spla_intercept,
     .          c_spla_slope,
     .          g_leaf_no_final,
     .          g_lai_max_possible,
     .          p_spla_prod_coef,
     .          g_slai,
     .          g_days_tot,
     .          g_lai,
     .          g_dlt_lai,
     .          g_dlt_slai_age)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       g_current_stage
      real       g_tt_tot(*)
      real       p_spla_intercept
      real       c_spla_slope
      real       g_leaf_no_final
      real       g_lai_max_possible
      real       p_spla_prod_coef
      real       g_slai
      real       g_days_tot(*)
      real       g_lai
      real       g_dlt_lai
      real       g_dlt_slai_age     ! (OUTPUT)

*+  Purpose
*     Return the lai that would senesce  on the
*     current day from natural ageing
*
*   Called by srop_leaf_area_sen(2) in croptree.for

*+  Notes
cscc This function needs to be the rate of sen that occurs under
c non-limiting conditions of water and N (pref. from experiments w. N
c applied at flowering also)

*+  Changes
*     010994 jngh specified and programmed
*     970318 slw new template version

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_leaf_area_sen_age2')

*+  Local Variables
      real       spla_inflection       ! inflection point of leaf area
                                       ! senescence function (oC)
      real       slai_today            ! total senescence up to today
      real       tt_since_emerg        ! thermal time since emergence (oC)

*- Implementation Section ----------------------------------

      call push_routine (my_name)
         ! calculate senescence due to ageing
      if (stage_is_between (floral_init, harvest_ripe
     :                     , g_current_stage)) then

cscc This aging should really be linked better to phenology. The spla_inflection
c could be a function of pred. time from floral_init and to harvest_ripe or at
c least be top-limited by the actual tplamax cf. intitial pred. of tplamax. This
c would be similar to the change made to TPLA prediction. Obviously though there
c still need to feedback to actual production etc.

         tt_since_emerg = sum_between (emerg, now, g_tt_tot)
         spla_inflection = p_spla_intercept
     :                   + c_spla_slope * g_leaf_no_final

cscc The Senescence paper on sorghum says that the numerator below is supposed
c to be tplamax. I guess that after flag leaf, the below will be tplamax, but be
c the slai_today equation is not really doing what it should be, and is prob.
c underestimating senescence.
c Up to flag leaf, need to adjust the numerator daily, depending on stresses.
c The g_lai_max_possible is calculated in leaf (leaf_Area_Devel_plant)
!scc May 96. This not doing anything at present as g_lai_max_possible has been s
!to lai+g+g_slai. Need to fix code in leaf_Area_Devel_plant.

!         slai_today = divide ((g_lai + g_slai)
          slai_today = divide ((g_lai_max_possible)
     :              , (1.0 + exp(-p_spla_prod_coef
     :                        * (tt_since_emerg - spla_inflection)))
     :              , 0.0)

         g_dlt_slai_age = l_bound (slai_today - g_slai, 0.0)

         ! all leaves senesce at harvest ripe

cscc Does this make sense? I know we are supposed to harvest at PM, but leaves
c of sorghum don't instantly senescence when you harvest.
c What if you harvest the crop and leave it to rattoon?

      elseif (on_day_of (harvest_ripe
     :                 , g_current_stage, g_days_tot)) then
          g_dlt_slai_age = g_lai + g_dlt_lai

      else
         g_dlt_slai_age = 0.0
      endif

      g_dlt_slai_age = bound (g_dlt_slai_age, 0.0, g_lai)

c      write(*,900)
c900   format(" tt_since_emerg, g_lai, g_slai, slai_today"
c     :   , " g_lai_max_possible, g_dlt_slai_age")
c      write(*,1000)tt_since_emerg, g_lai, g_slai, slai_today
c     :   , g_lai_max_possible, g_dlt_slai_age

1000  format(6f10.3)
      if (g_dlt_slai_age .gt. 0.0 .and.
     :    reals_are_equal(g_dlt_slai_age, g_lai)) then
         call Write_string ('Age kills all leaves.')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine srop_leaf_area_sen_age1 (
     :          first_stage, last_stage,
     .          g_leaf_no_dead,
     .          g_dlt_leaf_no_dead,
     .          g_leaf_area,
     .          g_plants,
     .          g_slai,
     .          g_lai,
     .          g_dlt_slai_age)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
       integer first_stage
       integer last_stage
       real g_leaf_no_dead(*)
       real g_dlt_leaf_no_dead
       real g_leaf_area(*)
       real g_plants
       real g_slai
       real g_lai
       real g_dlt_slai_age          ! (OUTPUT) new senesced lai from
                                       ! phasic devel.

*+  Purpose
*       Return the lai that would senesce on the
*       current day due to ageing
*
*   Called from srop_leaf_area_sen(1) in croptree.for

*+  Changes
*     010994 jngh specified and programmed
*     970317 slw templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_leaf_area_sen_age1')

*+  Local Variables
      real       area_sen_dying_leaf   ! senesced leaf area from
                                       ! current leaf dying (mm^2)
      integer    dying_leaf            ! current leaf number dying ()
      real       leaf_no_dead_today    ! today's number of dead leaves ()
      real       slai_age              ! lai senesced by natural ageing

*- Implementation Section ----------------------------------

      call push_routine (my_name)

         ! now calculate the leaf senescence
         ! due to normal phenological (phasic) development

         ! get highest leaf no. senescing today
      leaf_no_dead_today = sum_between (first_stage, last_stage,
     :                                  g_leaf_no_dead)
     :             + g_dlt_leaf_no_dead
      dying_leaf = int (1.0 + leaf_no_dead_today)
         ! get area senesced from highest leaf no.

      area_sen_dying_leaf = mod (leaf_no_dead_today, 1.0)
     :                    * g_leaf_area(dying_leaf)

      slai_age = (sum_real_array (g_leaf_area, dying_leaf - 1)
     :         + area_sen_dying_leaf)
     :         * smm2sm * g_plants

      g_dlt_slai_age = bound (slai_age - g_slai, 0.0, g_lai)

      if (g_dlt_slai_age .gt. 0.0 .and.
     :    reals_are_equal(g_dlt_slai_age, g_lai)) then
         call Write_string ('Age kills all leaves.')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine srop_leaf_area_sen_light1 (
     .          c_lai_sen_light,
     .          c_sen_light_slope,
     .          g_lai,
     .          g_dlt_slai_light)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
       real c_lai_sen_light
       real c_sen_light_slope
       real g_lai
       real g_dlt_slai_light        ! (OUTPUT) lai senesced by low light

*+  Purpose
*       Return the lai that would senesce on the
*       current day due to light competition
*
*   Called from srop_leaf_area_sen(1) in croptree.for

*+  Changes
*     010994 jngh specified and programmed
*     970317 slw templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_leaf_area_sen_light1')

*+  Local Variables
      real       slai_light_fac        ! light competition factor (0-1)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

         ! calculate 0-1 factor for leaf senescence due to
         ! competition for light.

c+!!!!!!!! this doesnt account for other growing crops
c+!!!!!!!! should be based on reduction of intercepted light and k*lai
         ! competition for light factor

      if (g_lai.gt.c_lai_sen_light) then
         slai_light_fac = c_sen_light_slope * (g_lai - c_lai_sen_light)
      else
         slai_light_fac = 0.0
      endif

      g_dlt_slai_light = g_lai * slai_light_fac
      g_dlt_slai_light = bound (g_dlt_slai_light, 0.0, g_lai)

      if (g_dlt_slai_light .gt. 0.0 .and.
     :    reals_are_equal(g_dlt_slai_light, g_lai)) then
         call Write_string ('Low light kills all leaves.')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine srop_leaf_area_sen_water1(
     .          c_sen_rate_water,
     .          g_lai,
     .          g_swdef_photo,
     .          g_dlt_slai_water)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      REAL c_sen_rate_water       ! (INPUT)  slope in linear eqn relating soil wat
      REAL g_lai                  ! (INPUT)  live plant green lai
      REAL g_swdef_photo          ! (INPUT)
      REAL g_dlt_slai_water       ! (OUTPUT) water stress senescense

*+  Purpose
*       Return the lai that would senesce on the
*       current day due to water stress
*
*   Called from srop_leaf_area_sen(1) in croptree.for

*+  Changes
*     010994 jngh specified and programmed
*     970216 slw generalised to avoid common blocks

*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'srop_leaf_area_sen_water1')

*+  Local Variables
      real       slai_water_fac ! drought stress factor (0-1)

*- Implementation Section ----------------------------------

      call push_routine (my_name)
        ! drought stress factor
      slai_water_fac = c_sen_rate_water* (1.0 - g_swdef_photo)
      g_dlt_slai_water = g_lai * slai_water_fac
      g_dlt_slai_water = bound (g_dlt_slai_water, 0.0, g_lai)

      if (g_dlt_slai_water .gt. 0.0 .and.
     :    reals_are_equal(g_dlt_slai_water, g_lai)) then
         call Write_string ('Lack of water kills all leaves.')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine srop_leaf_area_sen_water2(
     .          g_day_of_year,
     .          g_year,
     .          c_sen_threshold,
     .          c_sen_water_time_const,
     .          num_layer,
     .          g_dlayer,
     .          g_lai,
     .          g_lai_equilib_water,
     .          g_root_depth,
     .          g_sw_demand,
     .          g_sw_supply,
     .          g_dlt_slai_water)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      INTEGER g_day_of_year              ! (INPUT)  day of year
      INTEGER g_year                     ! (INPUT)  year
      REAL    c_sen_threshold            ! (INPUT)  supply:demand ratio for onset
      REAL    c_sen_water_time_const     ! (INPUT)  delay factor for water senesce
      INTEGER num_layer                ! (INPUT)  number of layers in profile
      REAL    g_dlayer(*)                ! (INPUT)  thickness of soil layer I (mm)
      REAL    g_lai                      ! (INPUT)  live plant green lai
      REAL    g_lai_equilib_water(*)     ! (INPUT)  lai threshold for water senesc
      REAL    g_root_depth               ! (INPUT)  depth of roots (mm)
      REAL    g_sw_demand                ! (INPUT)  total crop demand for water (m
      REAL    g_sw_supply(*)             ! (INPUT)  potential water to take up (su
      REAL    g_dlt_slai_water           ! (OUTPUT) water stress senescense

*+  Purpose
*       Return the lai that would senesce on the
*       current day from water stress
*
*   Called from srop_leaf_area_sen(2) in croptree.for
*   Calls srop_running_ave in  crop.for

*+  Changes
*     010994 jngh specified and programmed
*     970216 slw generalised to avoid common blocks , added num_layer parameter

*+  Calls

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_leaf_area_sen_water2')

*+  Local Variables
      real    ave_lai_equilib_water    ! running mean lai threshold for water se
      integer deepest_layer            ! deepest layer in which the roots are gr
      real    sw_demand_ratio          ! water supply:demand ratio
      real    sw_supply_sum            ! total supply over profile (mm)

*- Implementation Section ----------------------------------

      call push_routine (my_name)
         ! calculate senescense from water stress
         ! NOTE needs rework for multiple crops

      deepest_layer = find_layer_no (g_root_depth, g_dlayer, num_layer)
      sw_supply_sum = sum_real_array (g_sw_supply, deepest_layer)
      sw_demand_ratio = divide (sw_supply_sum, g_sw_demand, 1.0)

      if (sw_demand_ratio.lt.c_sen_threshold) then
         ave_lai_equilib_water = srop_running_ave(g_day_of_year,
     :                            g_year, g_lai_equilib_water, 10)

         g_dlt_slai_water = (g_lai - ave_lai_equilib_water)
     :                  / c_sen_water_time_const

         g_dlt_slai_water = l_bound (g_dlt_slai_water, 0.0)

      else
         g_dlt_slai_water = 0.0

      endif
      g_dlt_slai_water = bound (g_dlt_slai_water, 0.0, g_lai)

      if (g_dlt_slai_water .gt. 0.0 .and.
     :    reals_are_equal(g_dlt_slai_water, g_lai)) then
         call Write_string ('Lack of water kills all leaves.')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine srop_leaf_area_sen_light2 (
     .          g_radn_int,
     .          g_radn,
     .          c_sen_radn_crit,
     .          g_year,
     .          g_day_of_year,
     .          g_lai_equilib_light,
     .          g_lai,
     .          c_sen_light_time_const,
     .          g_dlt_slai_light)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
       real g_radn_int
       real g_radn
       real c_sen_radn_crit
       integer g_year
       integer g_day_of_year
       real g_lai_equilib_light(*)
       real g_lai
       real c_sen_light_time_const
       real g_dlt_slai_light        ! (OUTPUT) lai senesced by low light

*+  Purpose
*       Return the lai that would senesce on the
*       current day from low light
*
*   Called from srop_leaf_area_sen(2) in croptree.for
*   Calls srop_running_ave in  crop.for

*+  Changes
*     010994 jngh specified and programmed
*     970317 slw templated

*+  Calls


*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_leaf_area_sen_light2')

*+  Local Variables
      real       ave_lai_equilib_light ! running mean lai threshold for light
                                       ! senescence ()
      real       radn_transmitted      ! radn transmitted through canopy
                                       ! (mj/m^2)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

         ! calculate senescense from water stress

c+!!!!!!!! this doesnt account for other growing crops
c+!!!!!!!! should be based on reduction of intercepted light and k*lai
c+!!!!!!!!
             ! calculate senescence due to low light
cglh - This works out se. based on when light drops below ps compensation point
c the leaf can't sustain itself.

      radn_transmitted = g_radn - g_radn_int

      if (radn_transmitted.lt.c_sen_radn_crit) then

         ave_lai_equilib_light = srop_running_ave
     .         (g_day_of_year, g_year, g_lai_equilib_light, 10)
         g_dlt_slai_light = divide (g_lai - ave_lai_equilib_light
     :                          , c_sen_light_time_const , 0.0)
         g_dlt_slai_light = l_bound (g_dlt_slai_light, 0.0)
      else
         g_dlt_slai_light = 0.0
      endif

      g_dlt_slai_light = bound (g_dlt_slai_light, 0.0, g_lai)

      if (g_dlt_slai_light .gt. 0.0 .and.
     :    reals_are_equal(g_dlt_slai_light, g_lai)) then
         call Write_string ('Low light kills all leaves.')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine srop_failure_germination1(
     .          sowing, germ, now,
     .          c_days_germ_limit,
     .          g_current_stage,
     .          g_days_tot,
     .          g_plants,
     .          g_dlt_plants)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer sowing
      integer germ
      integer now
      REAL       c_days_germ_limit     ! (INPUT)  maximum days allowed after sowin
      REAL       g_current_stage       ! (INPUT)  current phenological stage
      REAL       g_days_tot(*)         ! (INPUT)  duration of each phase (days)
      REAL       g_plants              ! (INPUT)  Plant density (plants/m^2)
      real       g_dlt_plants            ! (OUTPUT) change in plant number

*+  Purpose
*      Crop failure from lack of germination.

*+  Changes
*       290994 jngh specified and programmed
*       970317 slw extracted from Mungbean

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_failure_germination1')

*+  Local Variables
      character  string*200            ! output string

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (stage_is_between (sowing, germ, g_current_stage)
     :   .and. sum_between (sowing, now, g_days_tot)
     :         .ge.c_days_germ_limit) then

         g_dlt_plants = - g_plants

         write (string, '(3a, i4, a)')
     :                 ' crop failure because of lack of'
     :                  ,new_line
     :                  ,'         germination within'
     :                  , c_days_germ_limit
     :                  , ' days of sowing'
         call write_string (string)

      else
         g_dlt_plants = 0.0

      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine srop_failure_emergence1(
     .          germ, emerg, now,
     .          c_tt_emerg_limit,
     .          g_current_stage,
     .          g_plants,
     .          g_tt_tot,
     .          g_dlt_plants)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer germ
      integer emerg
      integer now
      REAL       c_tt_emerg_limit      ! (INPUT)  maximum degree days allowed fo
      REAL       g_current_stage       ! (INPUT)  current phenological stage
      REAL       g_plants              ! (INPUT)  Plant density (plants/m^2)
      REAL       g_tt_tot(*)           ! (INPUT)  the sum of growing degree days
      real       g_dlt_plants            ! (OUTPUT) change in plant number

*+  Purpose
*      Crop failure from lack of emergence.

*+  Changes
*       290994 jngh specified and programmed
*       970317 slw extracted from Mungbean
*
*   Called by srop_plant_death(1) in crop

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_failure_emergence1')

*+  Local Variables
      character  string*200            ! output string

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (stage_is_between (germ, emerg, g_current_stage)
     :       .and. sum_between (germ, now, g_tt_tot)
     :       .gt. c_tt_emerg_limit) then

         g_dlt_plants = - g_plants

         write (string, '(a)')
     :                 ' failed emergence due to deep planting'
         call write_string (string)

      else
         g_dlt_plants = 0.0

      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      real function srop_running_ave(
     .          g_day_of_year,
     .          g_year,
     .          array,
     .          number_of_days)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      INTEGER g_day_of_year        ! (INPUT)  day of year
      INTEGER g_year               ! (INPUT)  year
      real    array(*)           ! (INPUT) array to use for average
      integer number_of_days     ! (INPUT) number of days to average over

*+  Purpose
*       return the running average of an array over the last specified
*       number of days.
*
*   Called by srop_leaf_area_sen_light2, srop_leaf_area_sen_water2 in crop.for

*+  Changes
*     010994 jngh specified and programmed
*     970317 slw templated

*+  Constant Values
      character  my_name*(*)     ! name of procedure
      parameter (my_name = 'srop_running_ave')

*+  Local Variables
      integer start_day          ! day of year to start running

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      start_day = offset_day_of_year(g_year,
     :                              g_day_of_year, - number_of_days)

      srop_running_ave = divide(sum_part_of_real(array, start_day,
     :                                           g_day_of_year, 366)
     :                          , real (abs (number_of_days)), 0.0)

      call pop_routine (my_name)
      return
      end function



*     ===========================================================
      subroutine srop_lai_equilib_water(
     .           g_day_of_year,
     .           g_year,
     .           c_rue,
     .           g_cover_green,
     .           g_current_stage,
     .           g_lai,
     .           g_nfact_photo,
     .           g_radn,
     .           g_radn_int,
     .           g_sw_supply_sum,
     .           g_temp_stress_photo,
     .           g_transp_eff,
     .           g_lai_equilib_water)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      INTEGER g_day_of_year          ! (INPUT)  day of year
      INTEGER g_year                 ! (INPUT)  year
      REAL    c_extinction_coef      ! (INPUT)  radiation extinction coefficient
      REAL    c_rue(*)               ! (INPUT)  radiation use efficiency (g dm/m
      REAL    g_cover_green          ! (INPUT)  fraction of radiation reaching t
      REAL    g_current_stage        ! (INPUT)  current phenological stage
      REAL    g_lai                  ! (INPUT)  live plant green lai
      REAL    g_nfact_photo          ! (INPUT)
      REAL    g_radn                 ! (INPUT)  solar radiation (Mj/m^2/day)
      REAL    g_radn_int             ! (INPUT)  g_radn intercepted by leaves (mj
      REAL    g_sw_supply_sum        ! (INPUT)  potential water to take up (supp
      REAL    g_temp_stress_photo    ! (INPUT)
      REAL    g_transp_eff           ! (INPUT)  transpiration efficiency (g dm/m
      real    g_lai_equilib_water(*) ! (INPUT/OUTPUT) lai threshold for water se

*+  Purpose
*       Return the lai equilibrium water.
*
*   Called from srop_leaf_area_sen(2) in croptree.for
*   Calls srop_store_value in crop.for

*+  Changes
*     010994 jngh specified and programmed
*     070795 jngh corrected for case of rue = 0
*     040895 jngh corrected for intercropping
*     970216 slw generalised to avoid common blocks , added num_layer parameter

*+  Constant Values
      character  my_name*(*)       ! name of procedure
      parameter (my_name = 'srop_lai_equilib_water')

*+  Local Variables
      real       dlt_dm_transp     ! potential dry matter production
                                   ! by transpiration (g/m^2)
      real       lai_equilib_water_today ! lai threshold for water senescence
      real       lrue              ! radiation use efficiency (g dm/mj)
      real       rue_reduction     ! Effect of non-optimal N and Temp
                                   ! conditions on RUE (0-1)
      integer    stage_no          ! current stage no.
      real       intc_crit         ! critical interception (0-1)
      real       radn_canopy       ! radiation reaching canopy mj/m^2)
      real       sen_radn_crit     ! critical radiation (mj/m^2)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      c_extinction_coef = 0.4

      stage_no = int (g_current_stage)

      dlt_dm_transp = g_sw_supply_sum * g_transp_eff
      rue_reduction = min (g_temp_stress_photo, g_nfact_photo)
      lrue = c_rue(stage_no) * rue_reduction

      call bound_check_real_var (lrue, 0.0, c_rue(stage_no), 'c_rue')

      radn_canopy = divide (g_radn_int, g_cover_green, g_radn)
      sen_radn_crit = divide (dlt_dm_transp, lrue, radn_canopy)
      intc_crit = divide (sen_radn_crit, radn_canopy, 1.0)

      if (intc_crit.lt.1.0) then
            ! needs rework for row spacing
         lai_equilib_water_today = -log (1.0 - intc_crit)
     :                           / c_extinction_coef

      else
         lai_equilib_water_today =  g_lai
      endif

      call srop_store_value(g_day_of_year, g_year,
     :          g_lai_equilib_water, lai_equilib_water_today)

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine srop_store_value(
     .          g_day_of_year,
     .          g_year,
     .          array,
     .          value)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      INTEGER g_day_of_year    ! (INPUT)  day of year
      INTEGER g_year           ! (INPUT)  year
      REAL    array(*)       ! (OUTPUT) storage array
      REAL    value          ! (INPUT) value to be stored

*+  Purpose
*       Stores a value in an annual circular array
*
*   Called by srop_lai_equlib_light, srop_lai_equilib_water in crop.for

*+  Changes
*     230695 jngh specified and programmed
*     970317 slw templated

*+  Constant Values
      character  my_name*(*) ! name of procedure
      parameter (my_name = 'srop_store_value')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      array(g_day_of_year) = value

      if (g_day_of_year.eq.365
     :   .and. leap_year (g_year - 1)) then
         array(366) = 0.0
      else
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine srop_lai_equilib_light (
     .          g_radn_int,
     .          g_cover_green,
     .          c_sen_radn_crit,
     .          c_extinction_coef,
     .          g_lai,
     .          g_day_of_year,
     .          g_year,
     .          g_lai_eqlb_light)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
       real g_radn_int
       real g_cover_green
       real c_sen_radn_crit
       real c_extinction_coef
       real g_lai
       integer g_day_of_year
       integer g_year
       real g_lai_eqlb_light(*)  ! (IN/OUT) lai threshold for light senescence

*+  Purpose
*       Return the lai equilibrium light
*
*   Called from srop_leaf_area_sen(2) in croptree.for
*   Calls srop_store_value

*+  Changes
*     010994 jngh specified and programmed
*     040895 jngh corrected for intercropping
*     970317 slw templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_lai_equilib_light')

*+  Local Variables
      real       lai_eqlb_light_today ! lai threshold for light senescence
      real       radn_canopy           ! radiation reaching canopy mj/m^2)
      real       trans_crit            ! critical transmission (0-1)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      radn_canopy = divide (g_radn_int, g_cover_green, 0.0)
      trans_crit = divide (c_sen_radn_crit, radn_canopy, 0.0)

      c_extinction_coef = 0.4

      if (trans_crit.gt.0.0) then
            ! needs rework for row spacing
         lai_eqlb_light_today = -log (trans_crit)/c_extinction_coef
      else
         lai_eqlb_light_today = g_lai
      endif

      call srop_store_value (
     .          g_day_of_year,
     .          g_year,
     .          g_lai_eqlb_light,
     .          lai_eqlb_light_today)

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine srop_phenology_init1 (
     .          g_current_stage,
     .          g_days_tot,
     .          c_shoot_lag,
     .          g_sowing_depth,
     .          c_shoot_rate,
     .          p_tt_emerg_to_endjuv,
     .          p_tt_endjuv_to_init,
     .          g_day_of_year,
     .          g_latitude,
     .          c_twilight,
     .          p_photoperiod_crit1,
     .          p_photoperiod_crit2,
     .          p_photoperiod_slope,
     .          g_leaf_no_final,
     .          c_leaf_no_rate_change,
     .          c_leaf_no_at_emerg,
     .          c_leaf_app_rate1,
     .          c_leaf_app_rate2,
     .          g_tt_tot,
     .          p_tt_flag_to_flower,
     .          p_tt_flower_to_start_grain,
     .          p_tt_flower_to_maturity,
     .          p_tt_maturity_to_ripe,
     .          g_phase_tt)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real      g_current_stage
      real      g_days_tot(*)
      real      c_shoot_lag
      real      g_sowing_depth
      real      c_shoot_rate
      real      p_tt_emerg_to_endjuv
      real      p_tt_endjuv_to_init
      integer   g_day_of_year
      real      g_latitude
      real      c_twilight
      real      p_photoperiod_crit1
      real      p_photoperiod_crit2
      real      p_photoperiod_slope
      real      g_leaf_no_final
      real      c_leaf_no_rate_change
      real      c_leaf_no_at_emerg
      real      c_leaf_app_rate1
      real      c_leaf_app_rate2
      real      g_tt_tot(*)
      real      p_tt_flag_to_flower
      real      p_tt_flower_to_start_grain
      real      p_tt_flower_to_maturity
      real      p_tt_maturity_to_ripe
      real      g_phase_tt (*)           ! (INPUT/OUTPUT) cumulative growing
                                       ! degree days required for
                                       ! each stage (deg days)

*+  Purpose
*       Returns cumulative thermal time targets required for the
*       individual developmental stages.
*
*   Called by srop_phenology(1) in croptree.for

*+  Changes
*     010994 jngh specified and programmed
*     070495 psc added 2nd leaf appearance rate
*     090695 psc l_bound added (otherwise won't progress if g_phase_tt=0)
*     120995 glh restructured routine

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_phenology_init1')

*+  Local Variables
      real       tt_emerg_to_flag_leaf ! thermal time to develop
                                       ! and fully expand all leaves (oC)
      real       photoperiod           ! daylength (hours)
*
      real       leaf_no               ! leaf no. above which app. rate changes

*- Implementation Section ----------------------------------

      call push_routine (my_name)

         ! set estimates of phase thermal time targets at germination

      if (on_day_of (germ, g_current_stage, g_days_tot)) then
         g_phase_tt(germ_to_emerg) = c_shoot_lag
     :                             + g_sowing_depth*c_shoot_rate
         g_phase_tt(emerg_to_endjuv) = p_tt_emerg_to_endjuv
         g_phase_tt(endjuv_to_init) = p_tt_endjuv_to_init

         ! revise thermal time target for floral initialisation at emergence

      elseif (on_day_of (emerg, g_current_stage, g_days_tot)) then

         photoperiod = day_length (g_day_of_year, g_latitude,c_twilight)
         if (photoperiod.le.p_photoperiod_crit1) then
            g_phase_tt(endjuv_to_init) = p_tt_endjuv_to_init
         elseif (photoperiod.lt.p_photoperiod_crit2) then
            g_phase_tt(endjuv_to_init) = p_tt_endjuv_to_init +
     :      p_photoperiod_slope*(photoperiod - p_photoperiod_crit1)
         elseif (photoperiod.ge.p_photoperiod_crit2) then
            g_phase_tt(endjuv_to_init) = p_tt_endjuv_to_init +
     :      p_photoperiod_slope*(p_photoperiod_crit2
     :                              - p_photoperiod_crit1)
         else
         endif

! revise thermal time target for floral initialisation up to initialisation
! glh revise thermal time target for floral initialisation up to endjuv
!     pp at start of period more influential than pp at end of period

      elseif (stage_is_between (emerg, floral_init
     :                        , g_current_stage)) then

         if (stage_is_between (emerg, endjuv
     :                        , g_current_stage)) then
         photoperiod = day_length (g_day_of_year, g_latitude
     :                           , c_twilight)
          if (photoperiod.le.p_photoperiod_crit1) then
            g_phase_tt(endjuv_to_init) = p_tt_endjuv_to_init
          elseif (photoperiod.lt.p_photoperiod_crit2) then
            g_phase_tt(endjuv_to_init) = p_tt_endjuv_to_init +
     :      p_photoperiod_slope*(photoperiod - p_photoperiod_crit1)
          elseif (photoperiod.ge.p_photoperiod_crit2) then
            g_phase_tt(endjuv_to_init) = p_tt_endjuv_to_init +
     :      p_photoperiod_slope*(p_photoperiod_crit2
     :                                 - p_photoperiod_crit1)
          else
          endif
         endif

! set estimates of phase thermal time targets at initiation

cscc/glh should this be endjuv too
c      elseif (on_day_of (end_juv, g_current_stage
c     :                 , g_days_tot)) then


!      elseif (on_day_of (floral_init, g_current_stage
!     :                 , g_days_tot)) then


c scc/glh changed this to speed up last few leaves before
c flag leaf (as opposed to psc 'slow down the first leaves' approach)
cpsc

         leaf_no = max (g_leaf_no_final - c_leaf_no_rate_change,
     :                 c_leaf_no_at_emerg)
         leaf_no = min (leaf_no, g_leaf_no_final)
         tt_emerg_to_flag_leaf = (leaf_no - c_leaf_no_at_emerg)
     :                         * c_leaf_app_rate1
     :                         + (g_leaf_no_final - leaf_no)
     :                         * c_leaf_app_rate2

!         tt_emerg_to_flag_leaf = (g_leaf_no_final - c_leaf_no_at_emerg)
!     :                         * c_leaf_app_rate

         g_phase_tt(init_to_flag) = tt_emerg_to_flag_leaf
     :              - sum_between (emerg, floral_init, g_tt_tot)

         g_phase_tt(flag_to_flower) = p_tt_flag_to_flower

         g_phase_tt(flower_to_start_grain) =
     :                    p_tt_flower_to_start_grain

         g_phase_tt(end_grain_to_maturity) =
     :                  0.05*p_tt_flower_to_maturity

         g_phase_tt(start_to_end_grain) = p_tt_flower_to_maturity
     :                  - g_phase_tt(flower_to_start_grain)
     :                  - g_phase_tt(end_grain_to_maturity)
         g_phase_tt(maturity_to_ripe) = p_tt_maturity_to_ripe

      else
          ! do nothing
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      real function srop_N_dlt_grain_conc(
     .              grain,
     .              c_sfac_slope,
     .              c_sw_fac_max,
     .              c_temp_fac_min,
     .              c_tfac_slope,
     .              g_maxt,
     .              g_mint,
     .              g_nfact_grain_conc,
     .              g_n_conc_crit,
     .              g_n_conc_min,
     .              g_swdef_expansion)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    grain
      REAL       c_sfac_slope          ! (INPUT)  soil water stress factor slope
      REAL       c_sw_fac_max          ! (INPUT)  soil water stress factor maximum
      REAL       c_temp_fac_min        ! (INPUT)  temperature stress factor minimu
      REAL       c_tfac_slope          ! (INPUT)  temperature stress factor slope
      REAL       g_maxt                ! (INPUT)  maximum air temperature (oC)
      REAL       g_mint                ! (INPUT)  minimum air temperature (oC)
      REAL       g_nfact_grain_conc    ! (INPUT)
      REAL       g_n_conc_crit(*)      ! (INPUT)  critical N concentration (g N/g
      REAL       g_n_conc_min(*)       ! (INPUT)  minimum N concentration (g N/g b
      REAL       g_swdef_expansion     ! (INPUT)

*+  Purpose
*     Calculate the nitrogen concentration required to meet the increase
*     from daily grain growth (0-1)

*+  Notes
*     First, two factors are calculated and used to estimate the
*     effects of mean temperature and drought stress on the N
*     concentration in grain growth for the day.  High temperature
*     or drought stress can cause the factors to exceed 1.
*     N deficiency can cause nfac < 1.  The net effect of these
*     equations is to allow grain nitrogen concentration to range
*     from less than .01 when N deficiency is severe to about .018
*     when adequate N is available but high temperature or drought
*     stress limit grain growth.
*     Here, optimum N concentration = 1.7%
*
*       called by srop_N_retranslocate1

*+  Changes
*       090994 jngh specified and programmed
*       970317 slw extracted from Mungbean

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_N_dlt_grain_conc')

*+  Local Variables
      real       N_conc_pot            ! potential grain N concentration
                                       ! (0-1) (g N/g part)
      real       N_grain_sw_fac        ! soil water stress factor for N
                                       ! uptake
      real       N_grain_temp_fac      ! temperature stress factor for N
                                       ! uptake
      real       ave_temp              ! mean temperature (oC)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      ave_temp = (g_maxt + g_mint) /2.0

c+!!!!!!!!!! return to orig cm
      N_grain_temp_fac = c_temp_fac_min + c_tfac_slope* ave_temp
      N_grain_sw_fac = c_sw_fac_max - c_sfac_slope * g_swdef_expansion

            ! N stress reduces grain N concentration below critical

      N_conc_pot = g_n_conc_min(grain)
     :           + (g_n_conc_crit(grain) - g_n_conc_min(grain))
     :           * g_nfact_grain_conc

            ! Temperature and water stresses can decrease/increase grain
            ! N concentration

            ! when there is no N stress, the following can be a higher N conc th
            ! the crit and thus the N conc of the grain can exceed N critical.

      srop_N_dlt_grain_conc = N_conc_pot
     :                       * max (N_grain_temp_fac, N_grain_sw_fac)

      call pop_routine (my_name)
      return
      end function



*     ===========================================================
      subroutine srop_N_retrans_avail(
     .          num_part, root, grain,
     .          g_N_conc_min,
     .          g_dm_green,
     .          g_N_green,
     .          N_avail)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
       integer num_part
       integer root
       integer grain
       real g_N_conc_min(*)
       real g_dm_green(*)
       real g_N_green(*)
       real N_avail(*)

*+  Purpose
*     Calculate N available for transfer to grain (g/m^2)
*     from each plant part.  By definition, available grain N
*     is set to 0.

*+  Notes
*     N available for translocation to the grain is the sum of
*     N available in the stover.
*     N available in stover is the difference of its N content
*     and the minimum it's allowed to fall to.
*     NB. No translocation from roots.

*+  Changes
*       080994 jngh specified and programmed
*       970318 slw extracted from Sorg
*
*       Called by srop_N_retranslocate1

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_N_retrans_avail')

*+  Local Variables
      real       N_min                 ! nitrogen minimum level (g/m^2)
      integer    part                  ! plant part number

*- Implementation Section ----------------------------------

      call push_routine (my_name)

         ! get grain N potential (supply) -----------
         ! now find the available N of each part.

!scc Propose a change to this section to stop all N being
!available in 1 day and limited it to a 5 day process

      do 1000 part = 1, num_part
         N_min = g_N_conc_min(part) * g_dm_green(part)
         N_avail(part) = l_bound (g_N_green(part) - N_min, 0.0)
!scc
!         N_avail(part) = N_avail(part) * 0.2
1000  continue

      N_avail(grain) = 0.0
      N_avail(root) = 0.0

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine srop_death_drought1 (
     .           g_cswd_photo,
     .           g_leaf_no,
     .           c_leaf_no_crit,
     .           c_swdf_photo_limit,
     .           g_swdef_photo,
     .           c_swdf_photo_rate,
     .           g_plants,
     .           g_dlt_plants_water)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
       real g_cswd_photo(*)
       real g_leaf_no(*)
       real c_leaf_no_crit
       real c_swdf_photo_limit
       real g_swdef_photo
       real c_swdf_photo_rate
       real g_plants
*
       real g_dlt_plants_water

*+  Purpose
*      Determine percentage plant failure due to water stress

*+  Changes
*       290994 jngh specified and programmed
*
*   Called by srop_plant_death(1) in crop

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_death_drought1')

*+  Local Variables
      real       cswd_photo            ! cumulative water stress for photoperiod
      real       leaf_no               ! number of leaves
      real       killfr                ! fraction of crop population to kill
      character  string*200            ! output string

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      cswd_photo = sum_between (emerg, flag_leaf, g_cswd_photo)
      leaf_no = sum_between (emerg, now, g_leaf_no)

      if (leaf_no.lt.c_leaf_no_crit
     :       .and. cswd_photo.gt.c_swdf_photo_limit
     :       .and. g_swdef_photo .lt.1.0) then

         killfr = c_swdf_photo_rate* (cswd_photo - c_swdf_photo_limit)
         killfr = bound (killfr, 0.0, 1.0)
         g_dlt_plants_water = - g_plants*killfr

         write (string, '(a, i4, a)')
     :          'plant_kill.'
     :         , nint (killfr*100.0)
     :         , '% failure because of water stress.'

         call Write_string (string)

      else
         g_dlt_plants_water = 0.0

      endif

      call pop_routine (my_name)
      return
      end subroutine

*     Routines from CropLib and CropProc ========================
*     CropProc Routines =========================================
*     ===========================================================
      subroutine sproc_phenology1 (
     .       g_previous_stage,
     .       g_current_stage,

     .       g_maxt, g_mint,
     .       c_x_temp, c_y_tt,
     .       c_num_temp, g_dlt_tt,

     :       C_num_sw_avail_ratio,
     :       C_x_sw_avail_ratio, C_y_swdef_pheno, G_dlayer,
     :       g_root_depth, g_sw_avail, g_sw_avail_pot, g_swdef_pheno,

     .       g_dm_green,
     .       g_N_conc_crit, g_N_conc_min, g_N_green,
     .       c_N_fact_pheno, g_nfact_pheno,

     .          g_days_tot,
     .          c_shoot_lag,
     .          g_sowing_depth,
     .          c_shoot_rate,
     .          p_tt_emerg_to_endjuv,
     .          p_tt_endjuv_to_init,
     .          g_day_of_year,
     .          g_latitude,
     .          c_twilight,
     .          p_photoperiod_crit1,
     .          p_photoperiod_crit2,
     .          p_photoperiod_slope,
     .          g_leaf_no_final,
     .          c_leaf_no_rate_change,
     .          c_leaf_no_at_emerg,
     .          c_leaf_app_rate1,
     .          c_leaf_app_rate2,
     .          g_tt_tot,
     .          p_tt_flag_to_flower,
     .          p_tt_flower_to_start_grain,
     .          p_tt_flower_to_maturity,
     .          p_tt_maturity_to_ripe,
     .          g_phase_tt,

     .          g_sw_dep,
     .          p_ll_dep,
     .          c_pesw_germ,

     .          g_dlt_stage)

*     ===========================================================
      Use infrastructure
      implicit none
!      include 'stress.inc' !to set value of photo - not sure if correct way

*+  Purpose
*     Use temperature, photoperiod and genetic characteristics
*     to determine when the crop begins a new growth phase.
*     The initial daily thermal time and height are also set.
*
*   Called by _process
*
*   Number of options: 1
*   Option 1:
*     Designed for cereals...???
*   Calls srop_pheno_swdef_fact1, srop_pheno_n_fact1
*         srop_thermal_time1,
*         srop_phenology_init1, srop_phase_devel1, srop_devel1 in crop.for

*+  Notes
cscc 030997 HAVE to generalise this routine. Could do so by being able to
c specify init routine and stages to apply water and N stress
cscc Needs to incorporate water stress and low N effects on phenology
c usually by slowing down leaf appearance in vegetative phase
c and often hastening leaf senescence in grainfilling phase
c Water stress effect during grainfilling is partly because the canopy heats up
c more than it would if it were irrigated. Really need to predict canopy temp.
c somehow ...
c But if slow down leaf appearance etc. need to relate that to the leaf area mod
c (how do we do this w. TPLA approach?)

*+  Changes
*     010994 jngh specified and programmed

*+  Calls
      real       g_maxt
      real       g_mint
      real       c_x_temp(*)
      real       c_y_tt(*)
      integer    c_num_temp
      real       g_dlt_tt                ! (OUTPUT) daily thermal time (oC)
*
      INTEGER c_num_sw_avail_ratio  ! (INPUT)
      REAL    c_x_sw_avail_ratio(*) ! (INPUT)
      REAL    c_y_swdef_pheno(*)    ! (INPUT)
      REAL    g_dlayer(max_layer)           ! (INPUT)  thickness of soil layer I (mm)
      REAL    g_root_depth          ! (INPUT)  depth of roots (mm)
      REAL    g_sw_avail(max_layer)         ! (INPUT)  actual extractable soil water (mm
      REAL    g_sw_avail_pot(max_layer)     ! (INPUT)  potential extractable soil water
      REAL    G_swdef_pheno         ! (OUTPUT) sw stress factor (0-1)
      REAL    G_nfact_pheno         ! (OUTPUT) sw stress factor (0-1)
*
      REAL       g_dm_green(max_part)         ! (INPUT)  live plant dry weight (biomass
      REAL       g_n_conc_crit(max_part)      ! (INPUT)  critical N concentration (g N/
      REAL       g_n_conc_min(max_part)       ! (INPUT)  minimum N concentration (g N/g
      REAL       g_n_green(max_part)          ! (INPUT)  plant nitrogen content (g N/m^
      REAL       c_n_fact_pheno        ! (INPUT)  multipler for N deficit effect
*
      real      g_days_tot(*)
      real      c_shoot_lag
      real      g_sowing_depth
      real      c_shoot_rate
      real      p_tt_emerg_to_endjuv
      real      p_tt_endjuv_to_init
      integer   g_day_of_year
      real      g_latitude
      real      c_twilight
      real      p_photoperiod_crit1
      real      p_photoperiod_crit2
      real      p_photoperiod_slope
      real      g_leaf_no_final
      real      c_leaf_no_rate_change
      real      c_leaf_no_at_emerg
      real      c_leaf_app_rate1
      real      c_leaf_app_rate2
      real      g_tt_tot(*)
      real      p_tt_flag_to_flower
      real      p_tt_flower_to_start_grain
      real      p_tt_flower_to_maturity
      real      p_tt_maturity_to_ripe
      real      G_phase_tt (*) ! (INPUT/OUTPUT) cumulative growing
                               ! degree days required for stage (deg days)
*
      real       g_sw_dep(max_layer)
      real       p_ll_dep(max_layer)
      real       c_pesw_germ
      real       G_phase_dvl           ! (OUTPUT) fraction of current phase elap
*
      real       g_dlt_stage             ! (OUTPUT) change in growth stage
      real       g_current_stage         ! (OUTPUT) new stage no.
      real       g_previous_stage         ! (OUTPUT) new stage no.

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sproc_phenology1')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

         g_previous_stage = g_current_stage

         call srop_thermal_time1 (
     .          g_maxt, g_mint,
     .          c_x_temp, c_y_tt,
     .          c_num_temp, g_dlt_tt)

         if (stage_is_between(emerg, flag_leaf, g_current_stage))then

            !Modify g_dlt_tt by stress factors

            call srop_pheno_swdef_fact1(C_num_sw_avail_ratio,
     :       C_x_sw_avail_ratio, C_y_swdef_pheno, max_layer,G_dlayer,
     :       G_root_depth, G_sw_avail, G_sw_avail_pot, g_swdef_pheno)

            call srop_pheno_N_fact1(leaf, stem, g_dm_green,
     .        g_N_conc_crit, g_N_conc_min, g_N_green,
     .        c_N_fact_pheno, g_nfact_pheno)

              g_dlt_tt = g_dlt_tt *
     :             min (g_swdef_pheno, g_nfact_pheno)

         else

            g_dlt_tt = g_dlt_tt

         endif

         ! initialise phenology phase targets

*Check against cropsid.for
*Could split this into inits for different stages

         call srop_phenology_init1 (
     .          g_current_stage,
     .          g_days_tot,
     .          c_shoot_lag,
     .          g_sowing_depth,
     .          c_shoot_rate,
     .          p_tt_emerg_to_endjuv,
     .          p_tt_endjuv_to_init,
     .          g_day_of_year,
     .          g_latitude,
     .          c_twilight,
     .          p_photoperiod_crit1,
     .          p_photoperiod_crit2,
     .          p_photoperiod_slope,
     .          g_leaf_no_final,
     .          c_leaf_no_rate_change,
     .          c_leaf_no_at_emerg,
     .          c_leaf_app_rate1,
     .          c_leaf_app_rate2,
     .          g_tt_tot,
     .          p_tt_flag_to_flower,
     .          p_tt_flower_to_start_grain,
     .          p_tt_flower_to_maturity,
     .          p_tt_maturity_to_ripe,
     .          g_phase_tt)

!Determine fraction of phase that has elapsed

         call srop_phase_devel1(
     .          g_current_stage,
     .          g_sowing_depth,
     .          g_dlayer,
     .          g_sw_dep,
     .          p_ll_dep,
     .          c_pesw_germ,
     .          g_days_tot,
     .          g_tt_tot,
     .          g_dlt_tt,
     .          g_phase_tt,
     .          g_phase_dvl)

! calculate new delta and the new stage

         call srop_devel1 (
     .          g_dlt_stage,
     .          g_current_stage,
     .          g_phase_dvl,
     .          max_stage)

         call accumulate (g_dlt_tt, g_tt_tot
     :               , g_previous_stage, g_dlt_stage)

         call accumulate (1.0, g_days_tot
     :               , g_previous_stage, g_dlt_stage)

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sproc_leaf_area_pot2 (
     .          g_days_tot,
     .          g_current_stage,
     .          g_leaf_no_final,
     .          c_initial_tpla,
     .          c_leaf_no_rate_change,
     .          c_leaf_no_at_emerg,
     .          c_leaf_app_rate1,
     .          c_leaf_app_rate2,
     .          p_tt_flag_to_flower,
     .          g_tiller_no_fertile,
     .          c_tiller_coef,
     .          p_main_stem_coef,
     .          g_tt_tot,
     .          c_tpla_inflection_ratio,
     .          g_tpla_today,
     .          g_tpla_yesterday,
     .          p_tpla_prod_coef,
     .          g_plants,
     .          g_lai,
     .          g_dlt_lai_pot)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
!      integer    emerg
!      integer    flag_leaf
!      integer    now
*
      real       g_days_tot(*)
      real       g_current_stage
      real       g_leaf_no_final
      real       c_initial_tpla
      real       c_leaf_no_rate_change
      real       c_leaf_no_at_emerg
      real       c_leaf_app_rate1
      real       c_leaf_app_rate2
      real       p_tt_flag_to_flower
      real       g_tiller_no_fertile
      real       c_tiller_coef
      real       p_main_stem_coef
      real       g_tt_tot(*)
      real       c_tpla_inflection_ratio
      real       g_tpla_today
      real       g_tpla_yesterday
      real       p_tpla_prod_coef
      real       g_plants
      real       g_lai
      real       g_dlt_lai_pot           ! (OUTPUT) change in leaf area

*+  Purpose
*       Return the potential increase in leaf area development (mm^2)
*       calculated on a whole plant basis as determined by thermal time
*
*   Called by srop_leaf_area_potential(2) in croptree.for

*+  Changes
*     010994 jngh specified and programmed
*     26/2/97  sb moved stressing out to another routine.

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sproc_leaf_area_pot2')

*+  Local Variables
      real       tpla_max              ! maximum total plant leaf area (mm^2)
!      real       tlai_today            ! total lai today
      real       tt_since_emerg        ! deg days since emergence (oC)
*
      real       tpla_inflection       ! inflection adjusted for leaf no.
*
      real       leaf_no               ! leaf no.
      real       emerg_to_flag_leaf    ! thermal time for emerg to flag_leaf
 !     real       emerg_to_flower       ! thermal time for emerg to flower

*- Implementation Section ----------------------------------

      call push_routine (my_name)

           ! once leaf no is calculated maximum plant leaf area
           ! is determined

cscc Changed the ending stage from flowering to flag_leaf
cglh still wants to use flowering

         if (on_day_of (emerg, g_current_stage, g_days_tot)) then
            g_lai = c_initial_tpla * smm2sm * g_plants
         endif

      if (stage_is_between (emerg, flag_leaf, g_current_stage)) then
c      if (stage_is_between (emerg, flowering, g_current_stage)) then

c Below code is from PHEN. Needed for thermal time to flag or flower for TPLA
c calculations. Have to do this here, as these stages are only calcualted
c in PHEN after floral initiation, but TPLA starts before then.
         leaf_no = max (g_leaf_no_final - c_leaf_no_rate_change,
     :                 c_leaf_no_at_emerg)
         leaf_no = min (leaf_no, g_leaf_no_final)
         emerg_to_flag_leaf = (leaf_no - c_leaf_no_at_emerg)
     :                         * c_leaf_app_rate1
     :                         + (g_leaf_no_final - leaf_no)
     :                         * c_leaf_app_rate2
!         emerg_to_flower = emerg_to_flag_leaf +
!     :                         p_tt_flag_to_flower

c end of PHEN code

         tpla_max = (((g_tiller_no_fertile + 1.0) ** c_tiller_coef)
     :            * g_leaf_no_final ** p_main_stem_coef) * scm2smm

         tt_since_emerg = sum_between (emerg, now, g_tt_tot)

cscc 10/95 fixing the beta inflection coefficient as halfway to thermal
c time of flag_leaf expanded. Code needs work as the halfway point jumps
c around a bit as we progress (espec. when final_leaf_no is reset at floral in
c Note that tpla_inflection needs to be removed as a 'read-in' parameter
c maybe the number is more like .66 of the distance?
c can work out from the shape of a leaf area distribution - where is the biggest
c leaf appearing...

c  scc - generalise tpla_inflection  - needs more work

      tpla_inflection = emerg_to_flag_leaf * c_tpla_inflection_ratio

!      tpla_inflection = emerg_to_flower * c_tpla_inflection_ratio

c          tpla_inflection = tpla_inflection +
c     :                         10.0*(g_leaf_no_final - 16.0)

c scc end of changes for tpla (more below)

          g_tpla_today = divide (Tpla_max
     :              , (1.0 + exp(-p_tpla_prod_coef
     :                        * (tt_since_Emerg - tpla_inflection)))
     :              , 0.0)


c gmc replace tpla_yesterday difference to calculate dlt_lai_pot

         g_dlt_lai_pot = (g_tpla_today - g_tpla_yesterday)
     .                  *smm2sm * g_plants

         g_tpla_yesterday = g_tpla_today

!should limit tpla_max somehow - to remove the LAI already foregone
c gmc following 2 lines commented out and replaced by 2 above

!         tlai_today = g_tpla_today * smm2sm * g_plants

!         g_dlt_lai_pot = (tlai_today - (g_lai + g_slai))

      else

         g_dlt_lai_pot = 0.0

      endif


      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sproc_bio_partition1 (
     .          g_current_stage,
     .          c_ratio_root_shoot,
     .          g_dlt_dm,
     .          g_leaf_no,
     .          c_partition_rate_leaf,
     .          g_dlt_lai_stressed,
     .          c_sla_min,
     .          c_frac_stem2flower,
     .          g_dlt_dm_grain_demand,
     .          g_dlt_dm_green)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real g_current_stage
      real c_ratio_root_shoot(*)
      real g_dlt_dm
      real g_leaf_no(*)
      real c_partition_rate_leaf
      real g_dlt_lai_stressed
      real c_sla_min
      real c_frac_stem2flower
      real g_dlt_dm_grain_demand
      real g_dlt_dm_green (*)      ! (OUTPUT) actual biomass partitioned
                                       ! to plant parts (g/m^2)

*+  Purpose
*     Partitions new dm (assimilate) between plant components (g/m^2)

*+  Changes
*     010994 jngh specified and programmed
*     250495 psc  modified dlt_dm_green(grain) to account for barren heads
*     970317 slw new template form

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sproc_bio_partition1')

*+  Local Variables
      integer    current_phase         ! current phase no.
      real       dlt_dm_green_tot      ! total of partitioned dm (g/m^2)
      real       dlt_dm_leaf_max       ! max increase in leaf dm (g/m^2)
      real       internode_no          ! internode no of stem (leaves emerged
                                       ! since emergence)
      real       partition_coef_leaf   ! partitioning coefficient of dm to
                                       ! leaf (0-1)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

         ! Root must be satisfied. The roots don't take any of the
         ! carbohydrate produced - that is for tops only.  Here we assume
         ! that enough extra was produced to meet demand. Thus the root
         ! growth is not removed from the carbo produced by the model.

         ! first we zero all plant component deltas

      call fill_real_array (g_dlt_dm_green, 0.0, max_part)

         ! now we get the root delta for all stages - partition scheme
         ! specified in coeff file

      current_phase = int (g_current_stage)
      g_dlt_dm_green(root) =c_ratio_root_shoot(current_phase)*g_dlt_dm

      if (stage_is_between (emerg, floral_init, g_current_stage)) then
            ! we have leaf development only
c Changed by SCC/GLH. Gatton data indicates stem growth also
c occurs before FI!

         g_dlt_dm_green(leaf) = g_dlt_dm

         internode_no = sum_between (emerg, now, g_leaf_no)
         partition_coef_leaf = 1.0
     :            /(1.0 + c_partition_rate_leaf * internode_no**2)

         g_dlt_dm_green(leaf) = partition_coef_leaf * g_dlt_dm
             ! limit the delta leaf area to maximum
         dlt_dm_leaf_max = divide (g_dlt_lai_stressed
     :                           , c_sla_min * smm2sm, 0.0)
         g_dlt_dm_green(leaf) = u_bound (g_dlt_dm_green(leaf)
     :                               , dlt_dm_leaf_max)

         g_dlt_dm_green(stem) = g_dlt_dm
     :                    - g_dlt_dm_green(leaf)

      elseif (stage_is_between (floral_init, flag_leaf
     :                        , g_current_stage)) then

            ! stem elongation and flower development start
            ! Each new leaf demands an increasing proportion of dry matter
            ! partitioned to stem and flower

c scc Does plant really do this, or does the head have priority
c over leaf as well as stem ?
c The following function is VERY sensitive to the c_partition_rate_leaf
c and has great effects on total bio also.
         internode_no = sum_between (emerg, now, g_leaf_no)
         partition_coef_leaf = 1.0
     :            /(1.0 + c_partition_rate_leaf * internode_no**2)

         g_dlt_dm_green(leaf) = partition_coef_leaf * g_dlt_dm

c limit the delta leaf area to maximum
c scc This effect must cut in a bit, as changing c_sla_min seems to affect thing
         dlt_dm_leaf_max = divide (g_dlt_lai_stressed
     :                           , c_sla_min * smm2sm, 0.0)

         g_dlt_dm_green(leaf) = u_bound (g_dlt_dm_green(leaf)
     :                               , dlt_dm_leaf_max)

         g_dlt_dm_green(flower) = (g_dlt_dm - g_dlt_dm_green(leaf))
     :                        * c_frac_stem2flower

         g_dlt_dm_green(stem) = g_dlt_dm
     :             - (g_dlt_dm_green(flower) + g_dlt_dm_green(leaf))


      elseif (stage_is_between (flag_leaf, flowering
     :                        , g_current_stage)) then

            ! we only have flower and stem growth here
         g_dlt_dm_green(flower) = g_dlt_dm*c_frac_stem2flower
         g_dlt_dm_green(stem) = g_dlt_dm - g_dlt_dm_green(flower)

      elseif (stage_is_between (flowering, maturity
     :                        , g_current_stage)) then

            ! grain filling starts - stem continues when it can

         g_dlt_dm_green(grain) = bound (g_dlt_dm_grain_demand
     :                              , 0.0, g_dlt_dm)
         g_dlt_dm_green(stem) = g_dlt_dm - g_dlt_dm_green(grain)

      elseif (stage_is_between (maturity, plant_end
     :                        , g_current_stage)) then

            ! put into stem
         g_dlt_dm_green(stem) = g_dlt_dm

      else
            ! no partitioning
      endif

         ! do mass balance check - roots are not included
      dlt_dm_green_tot = sum_real_array (g_dlt_dm_green, max_part)
     :                 - g_dlt_dm_green(root)
      call bound_check_real_var (dlt_dm_green_tot, g_dlt_dm, g_dlt_dm
     :                        , 'dlt_dm_green_tot mass balance')

         ! check that deltas are in legal range

      call bound_check_real_array (g_dlt_dm_green, 0.0, g_dlt_dm
     :                          , 'dlt_dm_green', max_part)

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sproc_N_retranslocate1 (
     .          g_dlt_dm_green,
     .          g_maxt,
     .          g_mint,
     .          c_temp_fac_min,
     .          c_tfac_slope,
     .          c_sw_fac_max,
     .          c_sfac_slope,
     .          g_N_conc_min,
     .          g_N_conc_crit,
     .          g_dm_green,
     .          g_N_green,
     .          g_N_conc_max,
     .          g_swdef_expansion,
     .          g_nfact_grain_conc,
     .          o_dlt_N_retrans)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
       real g_dlt_dm_green(*)
       real g_maxt
       real g_mint
       real c_temp_fac_min
       real c_tfac_slope
       real c_sw_fac_max
       real c_sfac_slope
       real g_N_conc_min(*)
       real g_N_conc_crit(*)
       real g_dm_green(*)
       real g_N_green(*)
       real g_N_conc_max(*)
       real g_swdef_expansion
       real g_nfact_grain_conc
       real o_dlt_N_retrans (*)     ! (OUTPUT) plant N taken out from
                                       ! plant parts (g N/m^2)

*+  Purpose
*     Calculate the nitrogen retranslocation from the various plant parts
*     to the grain.
*
*       Called by srop_nit_retrans(1) in croptree
*       Calls srop_N_dlt_grain_conc,  srop_N_retrans_avail   in crop

*+  Changes
*     080994 jngh specified and programmed

*+  Calls


*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sproc_N_retranslocate1')

*+  Local Variables
      real       grain_N_demand        ! grain N demand (g/m^2)
      real       N_avail(max_part)     ! N available for transfer to grain
                                       ! (g/m^2)
      real       N_avail_stover        ! total N available in stover
                                       ! (g/m^2)
      real       N_potential           ! maximum grain N demand (g/m^2)
      integer    part                  ! plant part number

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      grain_N_demand = g_dlt_dm_green(grain) * srop_N_dlt_grain_conc(
     :          grain,
     .          c_sfac_slope,
     .          c_sw_fac_max,
     .          c_temp_fac_min,
     .          c_tfac_slope,
     .          g_maxt,
     .          g_mint,
     .          g_nfact_grain_conc,
     .          g_N_conc_crit,
     .          g_N_conc_min,
     .          g_swdef_expansion)

      N_potential  = (g_dm_green(grain) + g_dlt_dm_green(grain))
     :             * g_N_conc_max(grain)

      grain_N_demand = u_bound (grain_N_demand
     :                        , N_potential - g_N_green(grain))

      call srop_N_retrans_avail (max_part, root, grain,
     .          g_N_conc_min,
     .          g_dm_green,
     .          g_N_green,N_avail)  ! grain N potential (supply)

            ! available N does not include roots or grain
cjh  this should not presume roots and grain are 0.
csc  true....

      N_avail_stover  =  sum_real_array (N_avail, max_part)

          ! get actual grain N uptake

          ! limit retranslocation to total available N

      call fill_real_array (o_dlt_N_retrans, 0.0, max_part)

      if (grain_N_demand.ge.N_avail_stover) then

             ! demand greater than or equal to supply
             ! retranslocate all available N

         o_dlt_N_retrans(leaf) = - N_avail(leaf)
         o_dlt_N_retrans(stem) = - N_avail(stem)
         o_dlt_N_retrans(flower) = - N_avail(flower)
         o_dlt_N_retrans(grain) = N_avail_stover

      else
             ! supply greater than demand.
             ! Retranslocate what is needed

         o_dlt_N_retrans(leaf) = - grain_N_demand
     :                         * divide (N_avail(leaf)
     :                                 , N_avail_stover, 0.0)

         o_dlt_N_retrans(flower) = - grain_N_demand
     :                         * divide (N_avail(flower)
     :                                 , N_avail_stover, 0.0)

         o_dlt_N_retrans(stem) = - grain_N_demand
     :                         - o_dlt_N_retrans(leaf)   ! note - these are
     :                         - o_dlt_N_retrans(flower) ! -ve values.

         o_dlt_N_retrans(grain) = grain_N_demand

      endif
             ! just check that we got the maths right.

      do 1000 part = root, flower
         call bound_check_real_var (abs (o_dlt_N_retrans(part))
     :                            , 0.0, N_avail(part)
     :                            , 'o_dlt_N_retrans(part)')
1000  continue

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sproc_N_partition1(
     .          g_root_depth,
     .          g_dlayer,
     .          g_N_demand,
     .          g_N_max,
     .          dlt_NO3gsm,
     .          dlt_N_green
     .                     )
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
       real g_root_depth
       real g_dlayer(*)
       real g_N_demand(*)
       real g_N_max(*)
      real       dlt_N_green(max_part) ! (OUTPUT) actual plant N uptake
                                       ! into each plant part (g/m^2)
      real       dlt_NO3gsm(max_layer) ! (OUTPUT) actual plant N uptake
                                       ! from NO3 in each layer (g/m^2)

*+  Purpose
*     Return actual plant nitrogen uptake to each plant part and from
*     each soil layer.

*+  Changes
*      080994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sproc_N_partition1')

*+  Local Variables
      real       plant_part_fract      ! fraction of nitrogen to use (0-1)
                                       ! for plant part
      real       N_uptake_sum          ! total plant N uptake (g/m^2)
      real       N_excess              ! N uptake above N crit (g/m^2)
      real       N_capacity(max_part)  ! amount of N that can be stored in
                                       ! plant part above Ncrit (g/m^2)
      real       N_capacity_sum
      real       N_demand              ! total nitrogen demand (g/m^2)
      integer    part                  ! plant part number
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing

*- Implementation Section ----------------------------------
      call push_routine (my_name)

               ! find proportion of uptake to be
               ! distributed to to each plant part and distribute it.

      deepest_layer = find_layer_no (g_root_depth, g_dlayer, max_layer)
      N_uptake_sum = - sum_real_array (dlt_NO3gsm, deepest_layer)
      N_demand = sum_real_array (g_N_demand, max_part)

      N_excess = N_uptake_sum - N_demand
      N_excess = l_bound (N_excess, 0.0)

      if (N_excess.gt.0.0) then
         do 1200 part = 1, max_part
            N_capacity(part) = g_N_max(part) - g_N_demand(part)
1200     continue
         N_capacity(grain) = 0.0
      else
         call fill_real_array (N_capacity, 0.0, max_part)
      endif

      N_capacity_sum = sum_real_array (N_capacity, max_part)

!scc RCM found that this partitioning was biased toward leaf...
!60:40 vs stem. Can achieve same effect via concentration I guess.

!scc Should this happen - could probably put excess into preferentially
!stem, leaf, flower, root (reverse order of usage)


      do 1300 part = 1, max_part
         if (N_excess.gt.0.0) then
            plant_part_fract = divide (N_capacity(part)
     :                               , N_capacity_sum, 0.0)
            dlt_N_green(part) = g_N_demand(part)
     :                        + N_excess * plant_part_fract
          else
            plant_part_fract = divide (g_N_demand(part)
     :                            , N_demand, 0.0)
            dlt_N_green(part) = N_uptake_sum * plant_part_fract
          endif
1300  continue

      dlt_N_green(grain) = 0.0

      call bound_check_real_var (
     :             sum_real_array (dlt_N_green, max_part)
     :           , N_uptake_sum, N_uptake_sum
     :           , 'dlt_N_green mass balance')

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sproc_plant_death1 (
     .          c_tt_emerg_limit,
     .          g_current_stage,
     .          g_plants,
     .          g_tt_tot,
     .          g_dlt_plants_all,

     .          g_lai,
     .          g_dlt_slai,

     .          g_cswd_photo,
     .          g_leaf_no,
     .          c_leaf_no_crit,
     .          c_swdf_photo_limit,
     .          g_swdef_photo,
     .          c_swdf_photo_rate,
     .          g_dlt_plants_water,
     .          g_dlt_plants_dead)

*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      REAL c_tt_emerg_limit      ! (INPUT)  maximum degree days allowed fo
      REAL g_current_stage       ! (INPUT)  current phenological stage
      REAL g_plants              ! (INPUT)  Plant density (plants/m^2)
      REAL g_tt_tot(*)           ! (INPUT)  the sum of growing degree days
      real g_dlt_plants_all      ! (OUTPUT) change in plant number
*
      real g_lai
      real g_dlt_slai
*
      real g_cswd_photo(*)
      real g_leaf_no(*)
      real c_leaf_no_crit
      real c_swdf_photo_limit
      real g_swdef_photo
      real c_swdf_photo_rate
      real g_dlt_plants_water
      real g_dlt_plants_dead

*+  Purpose
*       crop death
*       works out how many plants to kill on an area basis

*+  Changes
*      5/9/96 dph
*      970912 scc - simplified the thing!
*
*   Called by _process in _main
*   Calls: srop_failure_emergence1,srop_death_drought1 in crop

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sproc_plant_death1')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

         call srop_failure_emergence1 (sowing, emerg, now,
     .          c_tt_emerg_limit,
     .          g_current_stage,
     .          g_plants,
     .          g_tt_tot,
     .          g_dlt_plants_all)
          call srop_death_drought1 (
     .          g_cswd_photo,
     .          g_leaf_no,
     .          c_leaf_no_crit,
     .          c_swdf_photo_limit,
     .          g_swdef_photo,
     .          c_swdf_photo_rate,
     .          g_plants,
     .          g_dlt_plants_water)

!scc Don't really need a call to calculate a minimum!!!!

        g_dlt_plants_dead = min (g_dlt_plants_all
     :          ,g_dlt_plants_water)

!         call srop_death_actual1 (
!     .          g_dlt_plants_all,
!     .          g_dlt_plants_water,
!     .          dlt_plants
!     .            )

!        if leaves are killed from frost, g_dlt_slai is set to g_lai
!        need to kill plant if lai = 0
!        gmc & rlv
!
         if (stage_is_between(flag_leaf,maturity,
     .      g_current_stage)) then
            if (g_lai - g_dlt_slai .lt. 0.1) then
               call Write_string (
     .            'plant death due to complete leaf senescence')
               g_dlt_plants_dead = -g_plants
            endif
         endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sproc_leaf_area_actual1 (
     .          g_current_stage,
     .          g_dlt_lai,
     .          g_dlt_lai_stressed,
     .          g_dlt_dm_green,
     .          c_sla_max
     .          )
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       g_current_stage
      real       g_dlt_lai
      real       g_dlt_lai_stressed
      real       g_dlt_dm_green(*)
      real       c_sla_max

*+  Purpose
*       Simulate actual crop leaf area development - checks that leaf area
*       development matches DM production and calculates tiller production.
*       Requires g_dlt_lai_stressed from srop_leaf_area_stressed1 after
*       dlt_lai_pot has been calculated by srop_leaf_area_devel_plant1

*+  Changes
*      250894 jngh specified and programmed
*      240596 glh  added tillering

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sproc_leaf_area_actual1')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

         ! limit the delta leaf area by carbon supply
!glh/scc but don't do when crop small - results in many errors

      if (stage_is_between (emerg, endjuv
     :                        , g_current_stage))  then

           g_dlt_lai = g_dlt_lai_stressed

      elseif (stage_is_between (endjuv, maturity
     :      , g_current_stage))  then

            g_dlt_lai = u_bound (g_dlt_lai_stressed
     :                   , g_dlt_dm_green(leaf)*c_sla_max * smm2sm)

      else

           g_dlt_lai = g_dlt_lai_stressed

      endif

      call pop_routine (my_name)
      return
      end subroutine



* ====================================================================

      subroutine sorg_dm_grain_source_sink (
     .          stem_trans_frac,
     .          leaf_trans_frac,
     .          current_stage,
     .          days_tot,
     .          dlt_dm,
     .          dlt_dm_grain_demand,
     .          grain_no,
     .          dlt_tt_fm,
     .          tt_flower_to_start_grain,
     .          tt_flower_to_maturity,
     .          dm_green,
     .          dm_dead,
     .          p_dm_per_seed,
     .          g_dm_green_tot_fi)

*     ===========================================================

c################################################################
*     ===========================================================

c################################################################
c
c   Yield component version--rlv
c
c################################################################

*+  Short description:
*     Find grain demand for carbohydrate using Heiniger et al., 1997 approach

*   Called by srop_bio_grain_demand(2) in croptree.for

*+  Changes:
*      Programmed by rlv-8/20/97
*      Revised 9/16/97 to calculate seed number on a per plant basis
*      Revised 10/5/97 to reset maximum amount translocated from stem and leaf

*+  Declaration section -----------------------------------------------
      Use infrastructure
      implicit none
*   Subroutine arguments
      real       stem_trans_frac
      real       leaf_trans_frac
      real       current_stage
      real       days_tot(*)
      real       dlt_dm
      real       dlt_dm_grain_demand  !(OUTPUT) grain dry matter potential(g/m^2
      real       grain_no
      real       dlt_tt_fm
      real       tt_flower_to_start_grain
      real       tt_flower_to_maturity
      real       dm_green(*)
      real       dm_dead(*)
      real       p_dm_per_seed
      real       g_dm_green_tot_fi

*   Global variables


*      logical    stage_is_between      ! function
*      logical    on_day_of
*      real       sum_real_array

*   Internal variables
      real       dlt_dm_grain          ! grain demand for carbohydrate(g/m^2)
      real       dm_plant
      real       growth_rate
      real       frac_gf
      real       tot_dm_caryopsis
      real       grain_wt_max
      real       add_grain_wt
      real       grain_wt
      real       lag

      save       add_grain_wt

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sorg_dm_grain_source_sink')

*-  Executable code section -------------------------------------------

      call push_routine (my_name)

      if (on_day_of (floral_init, current_stage,days_tot)) then

!note that this includes the roots......
         g_dm_green_tot_fi = sum_real_array (dm_green, max_part)

      endif

!ramp this up between flowering and start grain fill
        if(on_day_of(start_grain_fill, current_stage,days_tot)) then


!  Reset limits on translocation from stem and leaf!!!!!!
!  Part of the reason for this approach is that limits should not be needed!!!

        stem_trans_frac = 0.5

        leaf_trans_frac = 0.3

! Calculate grain number per plant
        dm_plant = sum_real_array (dm_green, max_part)-
     :            g_dm_green_tot_fi

        growth_rate = dm_plant / ( days_tot(floral_init) +
     :        days_tot(flag_leaf) + days_tot(flowering))

        grain_no = growth_rate / p_dm_per_seed

      endif


      if (stage_is_between (start_grain_fill, maturity
     :                         , current_stage)) then

!proportion of period of current_stage completed

         frac_gf = current_stage - start_grain_fill

         lag = (140-tt_flower_to_start_grain)/
     :         (tt_flower_to_maturity - tt_flower_to_start_grain)

         if (frac_gf .le. lag) then
            dlt_dm_grain = dlt_dm * 0.25

         elseif (frac_gf < 0.78) then

! fraction 0.78 used because end_grain_fill is 95% of total flowering to maturity
! Ronnie started levelling off at 515 GDD which is 0.78 of 95% of 695


            tot_dm_caryopsis = dlt_dm/grain_no/dlt_tt_fm
            dlt_dm_grain = (0.0000319 + 0.4026 *
     :          tot_dm_caryopsis) * dlt_tt_fm * grain_no
            add_grain_wt = -1.0

         else
            if (add_grain_wt < 0.0) then
               grain_wt = (dm_green(grain)+dm_dead(grain))
     :                         /grain_no

               grain_wt_max = grain_wt / (0.85 + 0.6*
     .                                    (frac_gf-0.75))

              add_grain_wt = (grain_wt_max - grain_wt) /
     :         (tt_flower_to_maturity - tt_flower_to_maturity*frac_gf)

              endif

            dlt_dm_grain = add_grain_wt * grain_no *
     :                 dlt_tt_fm

         endif

      else

! we are out of grain fill period

         dlt_dm_grain = 0.0
      endif


      dlt_dm_grain_demand = dlt_dm_grain

      call pop_routine (my_name)
      return
      end subroutine
* ====================================================================

*     ===========================================================
      real function getrootarea(Top,Bottom, RootLength, Dist)
      Use infrastructure
      implicit none

      real Top,Bottom,RootLength,Dist

      real SDepth
      real TopArea,BottomArea,Theta,RootArea
      TopArea=0
      BottomArea=0
c   // intersection of roots and Section

      if(RootLength .le. Dist)then
         SDepth = 0
      else
         SDepth = sqrt(RootLength*RootLength - Dist*Dist)
      endif


c   // Rectangle - SDepth past bottom of this area
      if(SDepth .ge. Bottom) then
         RootArea = (Bottom - Top) * Dist
      else  !    // roots Past top
         Theta = 2 * acos(max(Top,SDepth)/RootLength)
         TopArea = ((RootLength*RootLength) / 2.0 *
     :        (Theta - sin(Theta)))/2.0

!      // bottom down
         if(RootLength .gt. Bottom) then
            Theta = 2 * acos(Bottom/RootLength);
            BottomArea = (RootLength*RootLength) /2.0 *
     :         (Theta - sin(Theta))/2.0

         endif
!      // rectangle
         if(SDepth > Top)then
            TopArea = TopArea + (SDepth - Top) * Dist
         endif
         RootArea = TopArea - BottomArea
      endif
      GetRootArea = RootArea
      return
      end function

*     ===========================================================
      real function rootproportioninlayer(Top,Bottom,RootLength,
     :   LDist,RDist)
      Use infrastructure
      implicit none


      real Top,Bottom,RootLength,LDist,RDist

c   // LDist and RDist are the distances to the left and right sections
c   // LDepth and RDepth are the intersections of the roots and the sections


      real RootArea,SoilArea


c   // roots not into this area yet
      if(RootLength .le. Top)then
         RootProportionInLayer = 0
         return
      endif

      RootArea = GetRootArea(Top,Bottom,RootLength, RDist)   ! // Right side
      RootArea = RootArea + GetRootArea(Top,Bottom,RootLength, LDist)  !  // Left Side
      SoilArea = (RDist + LDist) * (Bottom - Top)
      RootProportionInLayer =  RootArea / SoilArea
      return
      end function

*     ===========================================================


* ====================================================================
       subroutine cproc_sw_supply2 (
     :                            C_sw_lb
     :                           ,G_dlayer
     :                           ,P_ll_dep
     :                           ,G_dul_dep
     :                           ,G_sw_dep
     :                           ,max_layer
     :                           ,g_root_depth
     :                           ,g_root_front
     :                           ,p_kl
     :                           ,g_skip
     :                           ,g_row_spacing
     :                           ,g_sw_avail
     :                           ,g_sw_avail_pot
     :                           ,g_sw_supply
     :                           )
* ====================================================================
      Use Infrastructure
      implicit none

*+  Sub-Program Arguments
      real    C_sw_lb            ! (INPUT)
      real    G_dlayer (*)       ! (INPUT)
      real    P_ll_dep (*)       ! (INPUT)
      real    G_dul_dep (*)      ! (INPUT)
      real    G_sw_dep (*)       ! (INPUT)
      integer max_layer          ! (INPUT)
      real    g_root_depth       ! (INPUT)
      real    g_root_front
      real    p_kl (*)           ! (INPUT)
      real    g_sw_avail (*)     ! (OUTPUT)
      real    g_sw_avail_pot (*) ! (OUTPUT)
      real    g_sw_supply (*)    ! (OUTPUT)

      real    g_row_spacing      !Skip Row GMC
      real    g_skip

*+  Purpose
*     Calculate the crop water supply based on the KL approach.

*+  Mission Statement
*   Calculate today's soil water supply

*+  Changes
*     17-04-1998 - neilh - Programmed and Specified

*+  Calls

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'cproc_sw_supply1')

*- Implementation Section ----------------------------------
      call push_routine (myname)

         call crop_check_sw(C_sw_lb, G_dlayer, G_dul_dep, max_layer,
     :        G_sw_dep, P_ll_dep)
         call crop_sw_avail_pot(max_layer, G_dlayer, G_dul_dep,
     :        G_root_depth, P_ll_dep, g_sw_avail_pot) ! potential extractable sw
         call crop_sw_avail(max_layer, G_dlayer, G_root_depth, G_sw_dep,
     :        P_ll_dep, g_sw_avail)       ! actual extractable sw (sw-ll)
         call sorg_sw_supply(max_layer,G_dlayer,g_root_front,
     :       G_root_depth,G_sw_dep,
     :        P_kl, P_ll_dep,g_skip, g_row_spacing,g_sw_supply)

      call pop_routine (myname)
      return
      end subroutine



*     ===========================================================
      subroutine sorg_sw_supply(num_layer, dlayer, root_front,
     :              root_depth,sw_dep,
     :                kl, ll_dep, skip, row_spacing,sw_supply)
*     ===========================================================
      Use Infrastructure
      implicit none

*+  Sub-Program Arguments
      INTEGER    num_layer       ! (INPUT)  number of layers in profile
      REAL       dlayer(*)       ! (INPUT)  thickness of soil layer I (mm)
      REAL       root_front      ! (INPUT)  depth of roots (mm)
      real       root_depth
      REAL       sw_dep(*)       ! (INPUT)  soil water content of layer L (mm)
      REAL       kl(*)           ! (INPUT)  root length density factor for water
      REAL       ll_dep(*)       ! (INPUT)  lower limit of plant-extractable soi
      real       sw_supply(*)    ! (OUTPUT) potential crop water uptake
                                 ! from each layer (mm) (supply to roots)
      real       row_spacing      !Skip Row GMC
      real       skip
*+  Purpose
*       Return potential water uptake from each layer of the soil profile
*       by the crop (mm water). Row Spacing and configuration (skip) are used
*        to calculate semicircular root front to give proportion of the
*        layer occupied by the roots. This fraction is applied to the supply

*+  Mission Statement
*   Calculate today's soil water supply

*+  Notes
*      This code still allows water above dul to be taken - cnh

*+  Changes
*       010994 jngh specified and programmed - adapted from barley
*       970216 slw generalised to avoid common blocks, added num_layer

*+  Constant Values
      character  my_name*(*)     ! name of procedure
      parameter (my_name = 'sorg_sw_supply')

*+  Local Variables
      integer    deepest_layer   ! deepest layer in which the roots are growing
      integer    layer           ! soil profile layer number
      real       sw_avail        ! water available (mm)
      real       LDist,RDist
      real       Top,Bottom,prop

*- Implementation Section ----------------------------------

      call push_routine (my_name)

            ! get potential uptake

      call fill_real_array (sw_supply, 0.0, num_layer)

      deepest_layer = find_layer_no (root_depth, dlayer, num_layer)

      Top = 0
      LDist = row_spacing * 1000 * (skip - 0.5)
      RDist = row_spacing * 1000 * 0.5
      do 1000 layer = 1, deepest_layer
         Bottom = Top + dlayer(layer)
         sw_avail = (sw_dep(layer) - ll_dep(layer))
         prop= RootProportionInLayer(Top,Bottom,root_front,LDist,RDist)
         sw_supply(layer) = sw_avail * kl(layer) *  prop

         sw_supply(layer) = l_bound (sw_supply(layer), 0.0)
         Top = Bottom


1000  continue


      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine cproc_root_depth3 (
     :                              g_dlayer
     :                             ,C_num_sw_ratio
     :                             ,C_x_sw_ratio
     :                             ,C_y_sw_fac_root
     :                             ,G_dul_dep
     :                             ,G_sw_dep
     :                             ,P_ll_dep
     :                             ,C_root_depth_rate
     :                             ,G_current_stage
     :                             ,p_xf
     :                             , g_row_spacing
     :                             , g_skip
     :                             ,g_dlt_root_front
     :                             ,g_dlt_root_depth
     :                             ,g_root_front
     :                             ,g_root_depth
     :                             )
*     ===========================================================
      Use Infrastructure
      implicit none

*+  Sub-Program Arguments
      real    g_dlayer(*)             ! (INPUT)  layer thicknesses (mm)
      integer C_num_sw_ratio          ! (INPUT) number of sw lookup pairs
      real    C_x_sw_ratio(*)         ! (INPUT) sw factor lookup x
      real    C_y_sw_fac_root(*)      ! (INPUT) sw factor lookup y
      real    G_dul_dep(*)            ! (INPUT) DUL (mm)
      real    G_sw_dep(*)             ! (INPUT) SW (mm)
      real    P_ll_dep(*)             ! (INPUT) LL (mm)
      real    C_root_depth_rate(*)    ! (INPUT) root front velocity (mm)
      real    G_current_stage         ! (INPUT) current growth stage
      real    p_xf(*)                 ! (INPUT) exploration factor
      real    g_row_spacing
      real    g_skip
      real    g_dlt_root_front        ! (OUTPUT) increase in rooting front (mm)
      real    g_dlt_root_depth        ! (OUTPUT) increase in rooting depth (mm)
      real    g_root_front            ! (OUTPUT) root front (mm)
      real    g_root_Depth            ! (OUTPUT) root depth (mm)

*+  Purpose
*       Calculate plant rooting depth through time limited by soil water content
*       in layer through which roots are penetrating.

*+  Mission Statement
*   Calculate today's rooting depth

*+  Changes
*     170498 nih specified and programmed

*+  Calls

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_root_depth2')

*+  Local Variables
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      real sw_avail_fac_deepest_layer  !

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      deepest_layer = find_layer_no (g_root_depth, g_dlayer
     :                              , crop_max_layer)

      sw_avail_fac_deepest_layer = crop_sw_avail_fac               ! slw
     :              (
     :                C_num_sw_ratio
     :              , C_x_sw_ratio
     :              , C_y_sw_fac_root
     :              , G_dul_dep
     :              , G_sw_dep
     :              , P_ll_dep
     :              , deepest_layer
     :               )
         call sorg_root_depth_increase
     :               (
     :                C_root_depth_rate
     :              , G_current_stage
     :              , G_dlayer
     :              , G_root_depth
     :              , sw_avail_fac_deepest_layer             !slw
     :              , p_xf
     :              , g_dlt_root_depth
     :               )

         call sorg_root_front_increase
     :               (
     :                C_root_depth_rate
     :              , G_current_stage
     :              , G_dlayer
     :              , G_root_front
     :              , G_root_depth
     :              , sw_avail_fac_deepest_layer             !slw
     :              , p_xf
     :              , g_row_spacing
     :              , g_skip
     :              , g_dlt_root_front
     :               )
      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sorg_root_depth_increase
     :               (
     :                C_root_depth_rate
     :              , G_current_stage
     :              , G_dlayer
     :              , G_root_depth
     :              , G_sw_avail_fac_deepest_layer
     :              , p_xf
     :              , dlt_root_depth
     :               )
*     ===========================================================
      Use Infrastructure
      implicit none

*+  Sub-Program Arguments
      REAL       C_root_depth_rate(*)  ! (INPUT)  root growth rate potential (mm
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
      REAL       G_sw_avail_fac_deepest_layer ! (INPUT)
      REAL       P_XF (*)              ! (INPUT) eXploration Factor (0-1)
      real       dlt_root_depth        ! (OUTPUT) increase in root depth (mm)

*+  Purpose
*       Return the increase in root depth (mm)

*+  Mission Statement
*   Calculate the increase in rooting depth.

*+  Notes
*         there is a discrepency when the root crosses into another
*         layer. - cr380

*+  Changes
*      031097 nih specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sorg_root_depth_increase')

*+  Local Variables
      integer    current_phase         ! current phase number
      real       root_depth_max        ! maximum depth to which roots can
                                       ! go (mm)
      integer    current_layer         ! layer of root front
      integer    deepest_layer         ! deepest layer for rooting

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      current_layer = find_layer_no(g_root_depth
     :                             ,g_dlayer
     :                             ,crop_max_layer)
      current_phase = int (g_current_stage)

         ! this equation allows soil water in the deepest
         ! layer in which roots are growing
         ! to affect the daily increase in rooting depth.

      dlt_root_depth  = c_root_depth_rate(current_phase)
     :                * g_sw_avail_fac_deepest_layer
     :                * p_xf(current_layer)

         ! constrain it by the maximum
         ! depth that roots are allowed to grow.

      deepest_layer = count_of_real_vals (p_xf, crop_max_layer)
      root_depth_max = sum_real_array (g_dlayer, deepest_layer)
      dlt_root_depth = u_bound (dlt_root_depth
     :                        , root_depth_max - g_root_depth)

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine sorg_root_front_increase
     :               (
     :                C_root_depth_rate
     :              , G_current_stage
     :              , G_dlayer
     :              , G_root_front
     :              , G_root_depth
     :              , G_sw_avail_fac_deepest_layer
     :              , p_xf
     :              , g_row_spacing
     :              , g_skip
     :              , dlt_root_front
     :               )
*     ===========================================================
      Use Infrastructure
      implicit none

*+  Sub-Program Arguments
      REAL       C_root_depth_rate(*)  ! (INPUT)  root growth rate potential (mm
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL       G_root_front          ! (INPUT)  front of roots (mm)
      REAL       G_root_depth          ! (INPUT)  front of roots (mm)
      REAL       G_sw_avail_fac_deepest_layer ! (INPUT)
      REAL       P_XF (*)              ! (INPUT) eXploration Factor (0-1)
      real       g_row_spacing          !
      real       g_skip                 !
      real       dlt_root_front        ! (OUTPUT) increase in root front (mm)

*+  Purpose
*       Return the increase in root front (mm)

*+  Mission Statement
*   Calculate the increase in rooting front.

*+  Notes
*         there is a discrepency when the root crosses into another
*         layer. - cr380

*+  Changes
*      031097 nih specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sorg_root_front_increase')

*+  Local Variables
      integer    current_phase         ! current phase number
      real       root_front_max        ! maximum front to which roots can
                                       ! go (mm)
      integer    current_layer         ! layer of root front
      integer    deepest_layer         ! deepest layer for rooting
      real LDist ! left distance of plant section
      real rootdepth

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      current_layer = find_layer_no(g_root_depth
     :                             ,g_dlayer
     :                             ,crop_max_layer)
      current_phase = int (g_current_stage)

         ! this equation allows soil water in the deepest
         ! layer in which roots are growing
         ! to affect the daily increase in rooting depth.

      dlt_root_front  = c_root_depth_rate(current_phase)
     :                * g_sw_avail_fac_deepest_layer
     :                * p_xf(current_layer)

         ! constrain it by the maximum
         ! root front.

      deepest_layer = count_of_real_vals (p_xf, crop_max_layer)
      LDist = g_row_spacing * 1000 * (g_skip - 0.5)
      rootdepth = sum_real_array (g_dlayer, deepest_layer)
      root_front_max = sqrt(rootdepth**2 + LDist**2)
      dlt_root_front = u_bound (dlt_root_front
     :                        , root_front_max - g_root_front)


      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine cproc_root_front_init1
     :               (
     :                initial_root_depth
     :              , current_stage
     :              , initialisation_stage
     :              , days_tot
     :              , root_front
     :               )
*     ===========================================================
      Use Infrastructure
      implicit none

*+  Sub-Program Arguments
      REAL       initial_root_depth  ! (INPUT)  initial depth of roots (mm)
      REAL       current_stage       ! (INPUT)  current phenological stage
      INTEGER    initialisation_stage! (INPUT)  stage at which to initialise
      REAL       days_tot(*)         ! (INPUT)  duration of each phase (days)
      real       root_front          ! (OUTPUT) initial root depth (mm)

*+  Purpose
*       Return the initial root depth (mm)

*+  Mission Statement
*   Initialise rooting depth (on the first day of %3)

*+  Changes
*      160498 nih specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_root_depth_init1')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (on_day_of (initialisation_stage
     :              ,current_stage, days_tot)) then

             ! initialise root depth

         root_front = initial_root_depth

      else
              ! we have no initial root depth today

      endif

      call pop_routine (my_name)
      return
      end subroutine


