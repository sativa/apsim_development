
C     Last change:  E     1 Oct 2001   11:38 am


*     ===========================================================
      subroutine maize_phen_init (
     :          g_current_stage
     :        , g_days_tot
     :        , c_shoot_lag
     :        , g_sowing_depth
     :        , c_shoot_rate
     :        , p_tt_emerg_to_endjuv
     :        , p_tt_endjuv_to_init
     :        , g_day_of_year
     :        , g_latitude
     :        , c_twilight
     :        , p_photoperiod_crit1
     :        , p_photoperiod_crit2
     :        , p_photoperiod_slope
     :        , g_leaf_no_final
     :        , c_leaf_no_rate_change
     :        , c_leaf_no_at_emerg
     :        , c_leaf_app_rate1
     :        , c_leaf_app_rate2
     :        , g_tt_tot
     :        , p_tt_flag_to_flower
     :        , p_tt_flower_to_start_grain
     :        , p_tt_flower_to_maturity
     :        , p_tt_maturity_to_ripe
     :        , phase_tt)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       g_current_stage
      real       g_days_tot(*)
      real       c_shoot_lag
      real       g_sowing_depth
      real       c_shoot_rate
      real       p_tt_emerg_to_endjuv
      real       p_tt_endjuv_to_init
      integer    g_day_of_year
      real       g_latitude
      real       c_twilight
      real       p_photoperiod_crit1
      real       p_photoperiod_crit2
      real       p_photoperiod_slope
      real       g_leaf_no_final
      real       c_leaf_no_rate_change
      real       c_leaf_no_at_emerg
      real       c_leaf_app_rate1
      real       c_leaf_app_rate2
      real       g_tt_tot(*)
      real       p_tt_flag_to_flower
      real       p_tt_flower_to_start_grain
      real       p_tt_flower_to_maturity
      real       p_tt_maturity_to_ripe
      real       phase_tt (*)          ! (INPUT/OUTPUT) cumulative growing
                                       ! degree days required for
                                       ! each stage (deg days)

*+  Purpose
*       Returns cumulative thermal time targets required for the
*       individual growth stages.

*+  Mission Statement
*     Get the cumulative thermal time targets for growth phases

*+  Changes
*     010994 jngh specified and programmed
*     070495 psc added 2nd leaf appearance rate
*     090695 psc l_bound added (otherwise won't progress if phase_tt=0)
*     120995 glh restructured routine
*     1107-1 jngh tidied up

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'maize_phen_init')

*+  Local Variables
      real       tt_emerg_to_flag_leaf ! thermal time to develop
                                       ! and fully expand all leaves (oC)
      real       photoperiod           ! daylength (hours)
      real       leaf_no               ! leaf no. above which app. rate changes
      real       tt_adjust             ! adjustment due to photoperiod (oC)

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)

! set estimates of phase thermal time targets at germination

      if (on_day_of (germ, g_current_stage, g_days_tot)) then
         phase_tt(germ_to_emerg) = c_shoot_lag
     :                           + g_sowing_depth*c_shoot_rate
         phase_tt(emerg_to_endjuv) = p_tt_emerg_to_endjuv
         phase_tt(endjuv_to_init) = p_tt_endjuv_to_init

! revise thermal time target for floral initialisation at emergence

      elseif (on_day_of (emerg, g_current_stage, g_days_tot)) then

         photoperiod = day_length (g_day_of_year, g_latitude,
     :                                                  c_twilight)
         if (photoperiod .le. p_photoperiod_crit1) then
            phase_tt(endjuv_to_init) = p_tt_endjuv_to_init

         elseif (photoperiod.lt.p_photoperiod_crit2) then
            tt_adjust = p_photoperiod_slope
     :                * (photoperiod - p_photoperiod_crit1)
            phase_tt(endjuv_to_init) = p_tt_endjuv_to_init + tt_adjust

         elseif (photoperiod.ge.p_photoperiod_crit2) then
            tt_adjust = p_photoperiod_slope
     :                * (p_photoperiod_crit2 - p_photoperiod_crit1)
            phase_tt(endjuv_to_init) = p_tt_endjuv_to_init + tt_adjust

         else
         endif

! revise thermal time target for floral initialisation up to initialisation
! glh revise thermal time target for floral initialisation up to endjuv
!     pp at start of period more influential than pp at end of period

! glh      elseif (stage_is_between (emerg, floral_init
      elseif (stage_is_between (emerg, endjuv
     :                        , g_current_stage)) then

         photoperiod = day_length (g_day_of_year, g_latitude
     :                           , c_twilight)
         if (photoperiod.le.p_photoperiod_crit1) then
            phase_tt(endjuv_to_init) = p_tt_endjuv_to_init

         elseif (photoperiod.lt.p_photoperiod_crit2) then
            tt_adjust = p_photoperiod_slope
     :                * (photoperiod - p_photoperiod_crit1)
            phase_tt(endjuv_to_init) = p_tt_endjuv_to_init + tt_adjust

         elseif (photoperiod.ge.p_photoperiod_crit2) then
            tt_adjust = p_photoperiod_slope
     :                * (p_photoperiod_crit2 - p_photoperiod_crit1)
            phase_tt(endjuv_to_init) = p_tt_endjuv_to_init + tt_adjust

         else
         endif

! set estimates of phase thermal time targets at initiation

      elseif (on_day_of (floral_init, g_current_stage
     :                 , g_days_tot)) then

c scc/glh changed this to speed up last few leaves before
c flag leaf (as opposed to psc 'slow down the first leaves' approach)
cpsc
         leaf_no = max (g_leaf_no_final - c_leaf_no_rate_change,
     :                 c_leaf_no_at_emerg)
cjh
         leaf_no = min (leaf_no, g_leaf_no_final)
         tt_emerg_to_flag_leaf = (leaf_no - c_leaf_no_at_emerg)
     :                         * c_leaf_app_rate1
     :                         + (g_leaf_no_final - leaf_no)
     :                         * c_leaf_app_rate2

!         tt_emerg_to_flag_leaf = (g_leaf_no_final - c_leaf_no_at_emerg)
!     :                         * c_leaf_app_rate

         phase_tt(init_to_flag) = tt_emerg_to_flag_leaf
     :              - sum_between (emerg, floral_init, g_tt_tot)

         phase_tt(flag_to_flower) = p_tt_flag_to_flower

         phase_tt(flower_to_start_grain) = p_tt_flower_to_start_grain

!         phase_tt(end_grain_to_maturity) = 0.05*p_tt_flower_to_maturity
          phase_tt(end_grain_to_maturity) = 0.01*p_tt_flower_to_maturity

         phase_tt(start_to_end_grain) = p_tt_flower_to_maturity
     :                                - phase_tt(flower_to_start_grain)
     :                                - phase_tt(end_grain_to_maturity)
         phase_tt(maturity_to_ripe) = p_tt_maturity_to_ripe

      else
          ! do nothing
      endif

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine maize_leaf_number_final (
     :          g_current_stage
     :        , g_days_tot
     :        , g_phase_tt
     :        , start_of_leaf_init
     :        , c_leaf_init_rate
     :        , c_leaf_no_seed
     :        , c_leaf_no_min
     :        , c_leaf_no_max
     :        , g_tt_tot
     :        , leaf_no_final)

*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       g_current_stage
      real       g_days_tot(*)
      real       g_phase_tt(*)
      integer    start_of_leaf_init !stage at which leaf initiation starts
      real       c_leaf_init_rate
      real       c_leaf_no_seed
      real       c_leaf_no_min
      real       c_leaf_no_max
      real       g_tt_tot(*)
      real       leaf_no_final    ! (OUTPUT) maximum total leaf number

*+  Purpose
*       Calculate total leaf number.  This is set at floral initiation and
*       is set to an approximated number at germination to allow
*       other calculations to proceed until the correct number is known.

*+ Mission statement
*       Calculate total leaf number.

*+  Changes
*     010994 jngh specified and programmed
*     070495 psc  changed from emerg to germ
*     0596   glh  fixed it up

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'maize_leaf_number_final')

*+  Local Variables
      real       tt_floral_init        ! cumulative dtt from sowing
                                       ! to true floral initiation (deg day)

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)

          ! set total leaf number

      if (stage_is_between (start_of_leaf_init
     :                     , floral_init, g_current_stage)) then

               ! estimate the final leaf no from an approximated thermal
               ! time for the period from emergence to floral initiation.

        tt_floral_init = sum_between (start_of_leaf_init
     :                              , floral_init, g_phase_tt)

        leaf_no_final = divide (tt_floral_init
     :                         , c_leaf_init_rate, 0.0)
     :                 + c_leaf_no_seed

         call bound_check_real_var (leaf_no_final
     :                            , c_leaf_no_min, c_leaf_no_max
     :                            , 'leaf_no_final')

      elseif (on_day_of (floral_init, g_current_stage, g_days_tot)) then

               ! now we know the thermal time, get the actual final leaf no.

         tt_floral_init = sum_between (start_of_leaf_init
     :                               , floral_init, g_tt_tot)


         leaf_no_final = divide (tt_floral_init
     :                         , c_leaf_init_rate, 0.0)
     :                 + c_leaf_no_seed
         call bound_check_real_var (leaf_no_final
     :                            , c_leaf_no_min, c_leaf_no_max
     :                            , 'leaf_no_final')

      elseif (on_day_of (plant_end, g_current_stage, g_days_tot)) then

         leaf_no_final = 0.0

      else

      endif
      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine Maize_dm_init (
     :          g_current_stage
     :        , g_days_tot
     :        , c_dm_root_init
     :        , g_plants
     :        , c_dm_stem_init
     :        , c_dm_leaf_init
     :        , c_stem_trans_frac
     :        , c_leaf_trans_frac
     :        , dm_green, dm_plant_min)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       g_current_stage
      real       g_days_tot(*)
      real       c_dm_root_init
      real       g_plants
      real       c_dm_stem_init
      real       c_dm_leaf_init
      real       c_stem_trans_frac
      real       c_leaf_trans_frac
      real       dm_green(*)           ! (INPUT/OUTPUT) plant part weights
                                       ! (g/m^2)
      real       dm_plant_min(*)       ! (OUTPUT) minimum weight of each
                                       ! plant part (g/plant)

*+  Purpose
*       Initialise plant weights and plant weight minimums
*       at required instances.

*+  Mission Statement
*     Initialise plant weights and plant weight minimums at required instances.

*+  Changes
*     010994 jngh specified and programmed
*     970317 slw new template form

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_dm_init')

*+  Local Variables
      real       dm_plant_leaf         ! dry matter in leaves (g/plant)
      real       dm_plant_stem         ! dry matter in stems (g/plant)

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)

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

      elseif (on_day_of (start_grain_fill
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
      subroutine Maize_heat_stress (g_maxt
     :                      , c_temp_grain_crit_stress
     :                      , dlt_tt_heat_stress)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       g_maxt                ! (INPUT) maximum temperature (oC)
      real       c_temp_grain_crit_stress
                                       ! (INPUT) temperature above which
                                       ! heat stress occurs
      real       dlt_tt_heat_stress    ! (OUTPUT) heat stress (oC)

*+  Purpose
*     Calculate heat stress on grain number for the current day.

*+  Mission statement
*     Calculate heat stress on grain number for the current day.

*+  Changes
*     250894 jngh specified and programmed
*     970317 slw new template form

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_heat_stress')

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)

            ! high temperature stress reduces grain no via 'htsf'

      if (g_maxt.gt.c_temp_grain_crit_stress) then
         dlt_tt_heat_stress = g_maxt - c_temp_grain_crit_stress
      else
         dlt_tt_heat_stress = 0.0
      endif

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine Maize_grain_no2 (
     :          g_current_stage
     :        , g_days_tot
     :        , g_dm_plant_top_tot
     :        , c_grno_grate
     :        , c_grno_fract
     :        , c_num_grno_grate
     :        , p_head_grain_no_max
     :        , g_heat_stress_tt
     :        , c_htstress_coeff
     :        , g_N_conc_min
     :        , g_dm_green
     :        , g_N_green
     :        , g_plants
     :        , c_seed_wt_min
     :        , c_grain_N_conc_min
     :        , grain_num)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       g_current_stage
      real       g_days_tot(*)
      real       g_dm_plant_top_tot(*)
      real       c_grno_grate (*)
      real       c_grno_fract (*)
      integer    c_num_grno_grate
      real       p_head_grain_no_max
      real       g_heat_stress_tt(*)
      real       c_htstress_coeff
      real       g_N_conc_min(*)
      real       g_dm_green(*)
      real       g_N_green(*)
      real       g_plants
      real       c_seed_wt_min
      real       c_grain_N_conc_min
      real       grain_num

*+  Purpose
*     Calculate the grains per m^2 and heads per m^2
*     Same as maize_grain_no but with bound of grain_no_fract.

*+  Mission statement
*      Calculate the grains per m^2 and heads per m^2

*+  Changes
*     111094 jngh specified and programmed
*     250495 psc added head no to output
*     970317 slw new template form

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'Maize_grain_no2')

*+  Local Variables
      real       dm_plant              ! dm per plant (g/plant)
      real       head_grain_no         ! grains per plant
      real       head_grain_no_max     ! maximum grains per plant
      real       temp_fac              ! high temperature stress factor (0-1)
      real       N_avail_plant_sum     ! total N available for transfer to
                                       ! grain (g/plant)
      real       N_avail(max_part)     ! nitrogen available for grain
                                       ! (g/m^2)
      real       head_grain_no_optimum ! grains per plant in optimum conditions
      real       grain_no_fract        ! fraction of potential grains/
                                       ! plant
      real       growth_rate           ! average rate of
                                       ! photosynthesis during flowering
                                       ! (g/plant).

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)

         ! ------------- find actual grain uptake ---------------

      if (on_day_of (start_grain_fill
     :             , g_current_stage, g_days_tot)) then

            ! calculate number of grains/plant
            ! Grains/plant is calculated from a genotype-specific
            ! coefficient for potential kernel number and the average
            ! rate of photosynthesis during this stage.  The function
            ! used to predict grains/plant is derived from Edmeades and
            ! Daynard (1979).

         dm_plant = sum_between (flag_leaf, now, g_dm_plant_top_tot)
         growth_rate = divide (
     :                   dm_plant
     :                 , sum_between (flag_leaf, now, g_days_tot)
     :                 , 0.0)

            ! note - this function will never reach 1. Thus head_grain_no_max
            ! will never be achieved.

         grain_no_fract = linear_interp_real(growth_rate
     :                                      , c_grno_grate
     :                                      , c_grno_fract
     :                                      , c_num_grno_grate)
         grain_no_fract = bound (grain_no_fract, 0.0, 1.0)

         head_grain_no_optimum = p_head_grain_no_max * grain_no_fract

c         call bound_check_real_var (grain_no_fract, 0.0, 1.0
c     :                           , 'grain_no_fract')

            ! grain numbers are reduced by heat stress during flowering.

         temp_fac = 1.0
     :            - sum_between (flag_leaf, now, g_heat_stress_tt)
     :            * c_htstress_coeff

         temp_fac = bound (temp_fac, 0.0, 1.0)

            ! Grain numbers are sensitive to N deficiency occurring
            ! after flowering.  These may be reduced by post-flowering N
            ! deficiency. Calculate the maximum number of grains that can
            ! be produced from the plant's currently available nitrogen
            ! pool - assuming minimum grain weights and grain nitrogen
            ! concentrations to be achieved.

            ! In Maize_N_conc_limits, the min grain N conc is 0.007

         call crop_N_retrans_avail (max_part, root, grain,
     :          g_N_conc_min,
     :          g_dm_green,
     :          g_N_green,N_avail)
         N_avail_plant_sum  = divide (sum_real_array (N_avail, max_part)
     :                              , g_plants, 0.0)
         head_grain_no_max = divide (N_avail_plant_sum
     :                      , (c_seed_wt_min * c_grain_N_conc_min), 0.0)

         head_grain_no = head_grain_no_optimum * temp_fac

         grain_num  =  u_bound (head_grain_no
     :                       , head_grain_no_max)
     :             * g_plants

      else
            ! do nothing

      endif

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine Maize_dm_grain (
     :          g_current_stage
     :        , g_maxt
     :        , g_mint
     :        , c_x_temp_grain
     :        , c_y_grain_rate
     :        , c_num_temp_grain
     :        , c_swdf_grain_min
     :        , g_grain_no
     :        , p_grain_gth_rate
     :        , g_N_conc_min
     :        , g_dm_green
     :        , g_N_green
     :        , c_temp_fac_min
     :        , c_tfac_slope
     :        , c_sw_fac_max
     :        , c_sfac_slope
     :        , g_N_conc_crit
     :        , g_swdef_photo
     :        , g_pfact_grain
     :        , g_swdef_expansion
     :        , g_nfact_grain_conc
     :        , dlt_dm_grain_demand)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       g_current_stage
      real       g_maxt
      real       g_mint
      real       c_x_temp_grain(*)
      real       c_y_grain_rate(*)
      integer    c_num_temp_grain
      real       c_swdf_grain_min
      real       g_grain_no
      real       p_grain_gth_rate
      real       g_N_conc_min(*)
      real       g_dm_green(*)
      real       g_N_green(*)
      real       c_temp_fac_min
      real       c_tfac_slope
      real       c_sw_fac_max
      real       c_sfac_slope
      real       g_N_conc_crit(*)
      real       g_swdef_photo
      real       g_pfact_grain
      real       g_swdef_expansion
      real       g_nfact_grain_conc
      real       dlt_dm_grain_demand

*+  Purpose
*     Find grain demand for carbohydrate (g/m^2)

*+  Mission statement
*     Calculate the grain demand for carbohydrate (g/m^2)

*+  Changes
*     010994 jngh specified and programmed
*     970317 slw new template form

*+  Calls


*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_dm_grain')

*+  Local Variables
      real       dlt_dm_grain          ! grain demand for carbohydrate
                                       ! (g/m^2)
      real       fract_of_optimum      ! fraction of optimum conditions (0-1)
      real       dlt_dm_grain_optm     ! potential grain growth (g/m^2)
      real       rgfill                ! relative rate of grain fill for the
                                       ! day (0-1) due to temperature
                                       ! response (average of periods)
      real       sw_def_fac            ! water stress factor (0-1)

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)

      if (stage_is_between (start_grain_fill, end_grain_fill
     :                    , g_current_stage)) then

            ! effective grain filling period

            ! calculate the relative rate of grain fill (0-1) eight times
            ! for the day, using interpolated 3 hourly mean temperatures.
            ! this is a temperature stress factor.


            ! The old cm function had the optimum temperature at 26
            ! with the range being 6-46 and was a quadratic.
            ! This was changed with the optimum at 30 with the range
            ! being 17.57-42.43, still a quadratic.
            ! It now has a range 3.68 - 56.32 and is stepwise linear

         rgfill = linint_3hrly_temp (g_maxt, g_mint
     :                             , c_x_temp_grain, c_y_grain_rate
     :                             , c_num_temp_grain)


            ! get water stress factor

         sw_def_fac = (c_swdf_grain_min
     :              + (1.0 - c_swdf_grain_min) * g_swdef_photo)

         fract_of_optimum = rgfill * sw_def_fac !!* g_pfact_grain

            ! now calculate the grain growth demand for the day in g/m^2

         dlt_dm_grain_optm = g_grain_no * (p_grain_gth_rate * mg2gm)
         dlt_dm_grain = bound (dlt_dm_grain_optm * fract_of_optimum
     :                       , 0.0,
     :                         Maize_dm_grain_max
     :         (g_N_conc_min
     :        , g_dm_green
     :        , g_N_green
     :        , g_maxt
     :        , g_mint
     :        , c_temp_fac_min
     :        , c_tfac_slope
     :        , c_sw_fac_max
     :        , c_sfac_slope
     :        , g_N_conc_crit
     :        , g_swdef_expansion
     :        , g_nfact_grain_conc))


      else
            ! we are out of grain fill period

         dlt_dm_grain = 0.0
      endif

      dlt_dm_grain_demand = dlt_dm_grain

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      real       function Maize_dm_grain_max (
     :          g_N_conc_min
     :        , g_dm_green
     :        , g_N_green
     :        , g_maxt
     :        , g_mint
     :        , c_temp_fac_min
     :        , c_tfac_slope
     :        , c_sw_fac_max
     :        , c_sfac_slope
     :        , g_N_conc_crit
     :        , g_swdef_expansion
     :        , g_nfact_grain_conc)
*     ===========================================================
      Use infrastructure
            Use CropLibrary
      implicit none

*+  Sub-Program Arguments
      real       g_N_conc_min(*)
      real       g_dm_green(*)
      real       g_N_green(*)
      real       g_maxt
      real       g_mint
      real       c_temp_fac_min
      real       c_tfac_slope
      real       c_sw_fac_max
      real       c_sfac_slope
      real       g_N_conc_crit(*)
      real       g_swdef_expansion
      real       g_nfact_grain_conc

*+  Purpose
*     Maximum grain growth for available nitrogen (g/m^2)

*+  Mission statement
*     Maximum grain growth for available nitrogen (g/m^2)

*+  Changes
*     141093 jngh specified and programmed
*     970317 slw new template form

*+  Calls

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_dm_grain_max')

*+  Local Variables
      real       N_avail(max_part)     ! nitrogen available for grain uptake
                                       ! from each part (g/m^2)
*
      real       N_avail_sum           ! total nitrogen available for grain
                                       ! uptake (g/m^2)

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)

      call crop_N_retrans_avail (max_part, root, grain
     :        , g_N_conc_min
     :        , g_dm_green
     :        , g_N_green, N_avail)
      N_avail_sum = sum_real_array (N_avail, max_part)

      Maize_dm_grain_max = divide (N_avail_sum
     :                     , crop_N_dlt_grain_conc(grain
     :        , c_sfac_slope
     :        , c_sw_fac_max
     :        , c_temp_fac_min
     :        , c_tfac_slope
     :        , g_maxt
     :        , g_mint
     :        , g_nfact_grain_conc
     :        , g_N_conc_crit
     :        , g_N_conc_min
     :        , g_swdef_expansion)
     :                     , 0.0)

      call pop_routine (my_name)
      return
      end function

*     ===========================================================
      subroutine Maize_dm_partition (
     :          g_current_stage
     :        , c_ratio_root_shoot
     :        , g_dlt_dm
     :        , g_leaf_no
     :        , c_partition_rate_leaf
     :        , g_dlt_lai_stressed
     :        , c_sla_min
     :        , c_frac_stem2flower
     :        , g_dlt_dm_grain_demand
     :        , dlt_dm_green)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       g_current_stage
      real       c_ratio_root_shoot(*)
      real       g_dlt_dm
      real       g_leaf_no(*)
      real       c_partition_rate_leaf
      real       g_dlt_lai_stressed
      real       c_sla_min
      real       c_frac_stem2flower
      real       g_dlt_dm_grain_demand
      real       dlt_dm_green (*)      ! (OUTPUT) actual biomass partitioned
                                       ! to plant parts (g/m^2)

*+  Purpose
*     Partitions new dm (assimilate) between plant components (g/m^2)

*+  Mission Statement
*     Partitions new biomass between plant components

*+  Changes
*     010994 jngh specified and programmed
*     250495 psc  modified dlt_dm_green(grain) to account for barren heads
*     970317 slw new template form

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'Maize_dm_partition')

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
      call print_routine (my_name)

         ! Root must be satisfied. The roots don't take any of the
         ! carbohydrate produced - that is for tops only.  Here we assume
         ! that enough extra was produced to meet demand. Thus the root
         ! growth is not removed from the carbo produced by the model.

         ! first we zero all plant component deltas

      call fill_real_array (dlt_dm_green, 0.0, max_part)

         ! now we get the root delta for all stages - partition scheme
         ! specified in coeff file

      current_phase = int (g_current_stage)
      dlt_dm_green(root) = c_ratio_root_shoot(current_phase)*g_dlt_dm

      if (stage_is_between (emerg, floral_init, g_current_stage)) then
            ! we have leaf development only
c Changed by SCC/GLH. Gatton data indicates stem growth also
c occurs before FI!

         dlt_dm_green(leaf) = g_dlt_dm

         internode_no = sum_between (emerg, now, g_leaf_no)
         partition_coef_leaf = 1.0
     :            /(1.0 + c_partition_rate_leaf * internode_no**2)

         dlt_dm_green(leaf) = partition_coef_leaf * g_dlt_dm
             ! limit the delta leaf area to maximum
         dlt_dm_leaf_max = divide (g_dlt_lai_stressed
     :                           , c_sla_min * smm2sm, 0.0)
         dlt_dm_green(leaf) = u_bound (dlt_dm_green(leaf)
     :                               , dlt_dm_leaf_max)

         dlt_dm_green(stem) = g_dlt_dm
     :                    - dlt_dm_green(leaf)


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

         dlt_dm_green(leaf) = partition_coef_leaf * g_dlt_dm

c limit the delta leaf area to maximum
c scc This effect must cut in a bit, as changing c_sla_min seems to affect thing
         dlt_dm_leaf_max = divide (g_dlt_lai_stressed
     :                           , c_sla_min * smm2sm, 0.0)

         dlt_dm_green(leaf) = u_bound (dlt_dm_green(leaf)
     :                               , dlt_dm_leaf_max)

         dlt_dm_green(flower) = (g_dlt_dm - dlt_dm_green(leaf))
     :                        * c_frac_stem2flower

         dlt_dm_green(stem) = g_dlt_dm
     :                    - (dlt_dm_green(flower) + dlt_dm_green(leaf))


      elseif (stage_is_between (flag_leaf, start_grain_fill
     :                        , g_current_stage)) then

            ! we only have flower and stem growth here
         dlt_dm_green(flower) = g_dlt_dm*c_frac_stem2flower
         dlt_dm_green(stem) = g_dlt_dm - dlt_dm_green(flower)

      elseif (stage_is_between (start_grain_fill, maturity
     :                        , g_current_stage)) then

            ! grain filling starts - stem continues when it can

         dlt_dm_green(grain) = bound (g_dlt_dm_grain_demand
     :                              , 0.0, g_dlt_dm)
         dlt_dm_green(stem) = g_dlt_dm - dlt_dm_green(grain)

      elseif (stage_is_between (maturity, plant_end
     :                        , g_current_stage)) then

            ! put into stem
         dlt_dm_green(stem) = g_dlt_dm

      else
            ! no partitioning
      endif

         ! do mass balance check - roots are not included
      dlt_dm_green_tot = sum_real_array (dlt_dm_green, max_part)
     :                 - dlt_dm_green(root)
      call bound_check_real_var (dlt_dm_green_tot, g_dlt_dm, g_dlt_dm
     :                        , 'dlt_dm_green_tot mass balance')

         ! check that deltas are in legal range

      call bound_check_real_array (dlt_dm_green, 0.0, g_dlt_dm
     :                          , 'dlt_dm_green', max_part)

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine Maize_leaf_death0 (
     :          g_leaf_no_dead
     :        , g_current_stage
     :        , c_leaf_no_dead_const
     :        , c_leaf_no_dead_slope
     :        , g_tt_tot
     :        , g_leaf_no_final
     :        , g_days_tot
     :        , dlt_leaf_no_dead)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       g_leaf_no_dead(*)
      real       g_current_stage
      real       c_leaf_no_dead_const
      real       c_leaf_no_dead_slope
      real       g_tt_tot(*)
      real       g_leaf_no_final
      real       g_days_tot(*)
      real       dlt_leaf_no_dead      ! (OUTPUT) new fraction of oldest
                                       ! green leaf

*+  Purpose
*       Return the fractional death of oldest green leaf.

*+  Mission Statement
*     Get the fractional death of oldest green leaf

*+  Changes
*     010994 jngh specified and programmed
*     970318 slw new template version

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_leaf_death0')

*+  Local Variables
      real       leaf_no_dead_today    ! total number of dead leaves today
      real       leaf_no_dead_yesterday ! total number of dead leaves
                                        ! yesterday

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)

cpsc need to develop leaf senescence functions for crop

      leaf_no_dead_yesterday = sum_between (emerg, now, g_leaf_no_dead)

      if (stage_is_between (emerg, harvest_ripe, g_current_stage)) then
         leaf_no_dead_today = (c_leaf_no_dead_const
     :                        + c_leaf_no_dead_slope
     :                        * sum_between (emerg, now, g_tt_tot))
     :                        * g_leaf_no_final

      elseif (on_day_of (harvest_ripe
     :                 , g_current_stage, g_days_tot)) then
         leaf_no_dead_today = g_leaf_no_final

      else
         leaf_no_dead_today = 0.0
      endif

      leaf_no_dead_today = bound (leaf_no_dead_today
     :                           , leaf_no_dead_yesterday
     :                           , g_leaf_no_final)
      dlt_leaf_no_dead = leaf_no_dead_today - leaf_no_dead_yesterday

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine maize_failure_phen_delay (
     :                      g_cswd_pheno
     :                    , g_current_stage
     :                    , c_swdf_pheno_limit
     :                    , g_plants
     :                    , dlt_plants)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       g_cswd_pheno(*)
      real       g_current_stage
      real       c_swdf_pheno_limit
      real       g_plants
      real       dlt_plants

*+  Purpose
*      Determine plant death due to water stress

*+  Mission Statement
*     Determine plant death from prolonged phenology delay

*+  Changes
*       290994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'maize_failure_phen_delay')

*+  Local Variables
      real       cswd_pheno
      character  string*200            ! output string

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)

      cswd_pheno = sum_between (emerg, flag_leaf, g_cswd_pheno)

      if (stage_is_between (emerg, flag_leaf, g_current_stage)
     :   .and. cswd_pheno .ge. c_swdf_pheno_limit) then

         dlt_plants = - g_plants

         write (string, '(3a)')
     :                 '         crop failure because of prolonged'
     :                ,new_line
     :                ,'         phenology delay through water stress.'
         call write_string (string)

      endif

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine maize_death_barrenness0 (
     :                      g_current_stage
     :                    , g_days_tot
     :                    , c_head_grain_no_crit
     :                    , p_head_grain_no_max
     :                    , c_barren_crit
     :                    , g_grain_no
     :                    , g_plants
     :                    , dlt_plants)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       g_current_stage
      real       g_days_tot(*)
      real       c_head_grain_no_crit
      real       p_head_grain_no_max
      real       c_barren_crit
      real       g_grain_no
      real       g_plants
*
      real       dlt_plants

*+  Purpose
*      Determine percent plant failure due to barreness

*+   Mission statement
*      Determine percent plant failure due to barreness

*+  Changes
*       290994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'maize_death_barrenness0')

*+  Local Variables
      real       killfr                ! fraction of crop population to kill
      character  string*200            ! output string

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)

      if (on_day_of (start_grain_fill
     :             , g_current_stage, g_days_tot)) then
         call maize_plants_barren0 (
     :          c_head_grain_no_crit
     :        , p_head_grain_no_max
     :        , c_barren_crit
     :        , g_grain_no
     :        , g_plants
     :        , killfr)
         dlt_plants = - g_plants*killfr

         if (killfr .gt. 0.0) then
            write (string, '(a, i4, a)')
     :             'plant_kill.'
     :            , nint (killfr*100.0)
     :            , '% failure because of barreness.'

         call Write_string (string)

         else
                  ! do nothing
         endif

      else
         dlt_plants = 0.0

      endif

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine maize_plants_barren0 (
     :          c_head_grain_no_crit
     :        , p_head_grain_no_max
     :        , c_barren_crit
     :        , g_grain_no
     :        , g_plants
     :        , killfr)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       c_head_grain_no_crit
      real       p_head_grain_no_max
      real       c_barren_crit
      real       g_grain_no
      real       g_plants
      real       killfr                ! (OUTPUT) fraction of plants killed
                                       ! (plants/m^2)

*+  Purpose
*        Calculate fraction of barren heads (0-1).
*        Allows no more than 1 head per plant.

*+ Mission statement
*        Calculate fraction of barren heads (0-1)

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'maize_plants_barren0')

*+  Local Variables
      real       fract_of_optimum      ! fraction of optimum no. of heads due
                                       ! to barrenness below which some
                                       ! heads become barren. (0-1)
      real       head_grain_no         ! (grains/head)

*- Implementation Section ----------------------------------
      call push_routine (my_name)
      call print_routine (my_name)

      call maize_check_grain_no (
     :          c_head_grain_no_crit,
     :          p_head_grain_no_max,
     :          c_barren_crit)

         ! determine barrenness

      head_grain_no = divide (g_grain_no, g_plants, 0.0)

      if (head_grain_no.lt.c_head_grain_no_crit) then
            ! all heads barren
         fract_of_optimum = 0.0

      elseif (head_grain_no.lt.p_head_grain_no_max * c_barren_crit) then
            ! we have some barren heads
         fract_of_optimum =
     :              (divide (head_grain_no - c_head_grain_no_crit
     :                     , p_head_grain_no_max * c_barren_crit
     :                       - c_head_grain_no_crit
     :                     , 0.0))

      else
            ! we have no barren heads
         fract_of_optimum = 1.0
      endif

      fract_of_optimum = bound (fract_of_optimum, 0.0, 1.0)
      killfr = 1.0 - fract_of_optimum

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine maize_check_grain_no (
     :          c_head_grain_no_crit
     :        , p_head_grain_no_max
     :        , c_barren_crit)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       c_head_grain_no_crit
      real       p_head_grain_no_max
      real       c_barren_crit

*+  Purpose
*        Check validity of grain no. parameters

*+ Mission statement
*        Check validity of grain number parameters

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'maize_check_grain_no')

*+  Local Variables
      character  err_messg*200         ! error message

*- Implementation Section ----------------------------------
      call push_routine (my_name)
      call print_routine (my_name)

      if (c_head_grain_no_crit .gt. p_head_grain_no_max * c_barren_crit
     :   .and. p_head_grain_no_max .gt. 0.0) then
         write (err_messg,'(a, g16.7e2, a, g16.7e2, 3a, g16.7e2, a)')
     :               'critical grain no. ('
     :              , c_head_grain_no_crit
     :              ,') exceeds  ('
     :              , p_head_grain_no_max*c_barren_crit
     :              ,')'
     :              ,new_line
     :              ,'        which is '
     :              , c_barren_crit
     :              ,' of potential.'
         call warning_error (err_user, err_messg)

      else
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Maize_P_init (
     :          init_stage
     :        , g_current_stage
     :        , g_days_tot
     :        , g_dm_green
     :        , max_part
     :        , g_p_conc_max
     :        , g_plant_p)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    init_stage
      real       g_current_stage
      real       g_days_tot(*)
      real       g_dm_green(*)
      integer    max_part
      real       g_p_conc_max
      real       g_plant_p

*+  Purpose
*     Set initial plant p

*+  Mission Statement
*     Set initial plant p

*+  Changes:
*     270697 nih specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_P_init')

*+  Local Variables
      real       biomass

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)

      if (on_day_of (init_stage, g_current_stage, g_days_tot)) then
         biomass = sum_real_array (g_dm_green, max_part)
         g_plant_p = g_p_conc_max * biomass
      else
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine maize_P_demand (
     :          g_current_stage
     :        , g_radn_int
     :        , g_rue
     :        , c_ratio_root_shoot
     :        , g_dm_green
     :        , g_dm_senesced
     :        , g_dm_dead
     :        , max_part
     :        , g_P_conc_max
     :        , g_plant_P
     :        , c_p_uptake_factor
     :        , g_P_demand)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*     Calculate the plant p demand

*+  Mission Statement
*     Calculate the plant p demand

*+  Sub-Program Arguments

      REAL       g_current_stage
      REAL       g_radn_int
      REAL       g_rue
      REAL       c_ratio_root_shoot(*)
      REAL       g_dm_green(*)
      REAL       g_dm_senesced(*)
      REAL       g_dm_dead(*)
      INTEGER    max_part
      REAL       g_P_conc_max
      REAL       g_plant_P
      REAL       c_P_uptake_Factor
      REAL       g_P_demand

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'maize_p_demand')

*+  Local Variables
      real       biomass               ! total plant biomass (g/m2)
      integer    current_phase         ! current growth phase
      real       dlt_dm_pot            ! potential dm increase (g/m2)
      real       P_demand_new          ! demand for P by new growth
                                       ! (g/m^2)
      real       P_demand_old          ! demand for P by old biomass
                                       ! (g/m^2)
      real       deficit               ! deficit of total plant p (g/m2)
      real       p_demand_max          ! maximum P demand (g/m2)


*- Implementation Section ----------------------------------
      call push_routine (my_name)
      call print_routine (my_name)

         ! calculate potential new shoot and root growth

      current_phase = int (g_current_stage)
      dlt_dm_pot = g_rue * g_radn_int
     :           * (1.0 + c_ratio_root_shoot(current_phase))

      biomass    =  sum_real_array (g_dm_green, max_part)
     :           +  sum_real_array (g_dm_senesced, max_part)
     :           +  sum_real_array (g_dm_dead, max_part)

      P_demand_new = dlt_dm_pot * g_P_conc_max
      P_demand_old = (biomass * g_P_conc_max) - g_plant_p

      deficit = P_demand_old + P_demand_new
      deficit = l_bound (deficit, 0.0)

      p_demand_max = p_demand_new * c_p_uptake_factor

      g_P_demand = u_bound (deficit, p_demand_max)

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine Maize_P_conc_limits (
     :          g_current_stage
     :        , c_p_stage_code
     :        , c_stage_code_list
     :        , g_tt_tot
     :        , g_phase_tt
     :        , c_P_conc_max
     :        , c_P_conc_min
     :        , P_conc_max
     :        , P_conc_min)
*     ===========================================================
      Use infrastructure
            Use CropLibrary
      implicit none

*+  Sub-Program Arguments
      real       g_current_stage
      real       c_p_stage_code(*)
      real       c_stage_code_list(*)
      real       g_tt_tot(*)
      real       g_phase_tt(*)
      real       c_p_conc_min(*)
      real       c_p_conc_max(*)
      real       P_conc_max   ! (OUTPUT) maximum P conc
                              ! (g N/g part)
      real       P_conc_min   ! (OUTPUT) minimum P conc
                              ! (g N/g part)

*+  Purpose
*       Calculate the critical p concentration below which plant growth
*       is affected.  Also minimum and maximum p concentrations below
*       and above which it is not allowed to fall or rise.

*+  Mission Statement
*       Calculate the critical p concentration below which plant growth
*       is affected.

*+  Changes
*     080994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_P_conc_limits')

*+  Local Variables
      integer    numvals               ! number of values in stage code table
      real       current_stage_code            ! interpolated current stage code

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)

      if (stage_is_between (emerg, maturity, g_current_stage)) then

         numvals = count_of_real_vals (c_P_stage_code, max_stage)

         current_stage_code = Crop_stage_code (
     :          c_stage_code_list
     :        , g_tt_tot
     :        , g_phase_tt
     :        , g_current_stage
     :        , c_P_stage_code
     :        , numvals
     :        , max_stage)

cnh         P_conc_max = linear_interp_real (current_stage_code
         P_conc_max = linear_interp_real (g_current_stage
     :                                   , c_P_stage_code
     :                                   , c_P_conc_max
     :                                   , numvals)

cnh         P_conc_min = linear_interp_real (current_stage_code
         P_conc_min = linear_interp_real (g_current_stage
     :                                   , c_P_stage_code
     :                                   , c_P_conc_min
     :                                   , numvals)


      else

         P_conc_max = 0.0
         P_conc_min = 0.0

      endif

      call pop_routine (my_name)
      return
      end subroutine

!!*     ===========================================================
!!      subroutine maize_pfact
!!     :               (
!!     :                G_dm_green
!!     :              , G_dm_dead
!!     :              , G_dm_senesced
!!     :              , max_part
!!     :              , G_p_conc_max
!!     :              , G_p_conc_min
!!     :              , G_plant_p
!!     :              , k_pfact
!!     :              , pfact
!!     :               )
!!*     ===========================================================
!!      Use infrastructure
!!      implicit none
!!
!!*+  Sub-Program Arguments
!!      REAL       G_dm_green(*)    ! (INPUT)  live plant biomass (g/m2)
!!      REAL       G_dm_dead(*)     ! (INPUT)  dead plant biomass (g/m2)
!!      REAL       G_dm_senesced(*) ! (INPUT)  senesced plant biomass (g/m2)
!!      INTEGER    max_part         ! (INPUT)  number of plant parts
!!      REAL       G_p_conc_max     ! (INPUT)  max P conc (g N/g biomass)
!!      REAL       G_p_conc_min     ! (INPUT)  min P conc (g N/g biomass)
!!      REAL       G_plant_p        ! (INPUT)  plant P content (g N/m^2)
!!      REAL       k_pfact          ! (INPUT)  k value for stress factor
!!      real      pfact             ! (OUTPUT) P stress factor
!!
!!*+  Purpose
!!*     The concentration of P in the entire plant is used to derive a
!!*     series of Phosphorus stress indices.  The stress indices for
!!*     today's growth are calculated from yesterday's
!!*     relative nutritional status between a critical and minimum
!!*     total plant Phosphorus concentration.
!!
!!*+  Mission Statement
!!*      Calculate P stress indicies
!!
!!*+   Changes
!!*     270697 nih
!!
!!*+  Constant Values
!!      character  my_name*(*)           ! name of procedure
!!      parameter (my_name = 'maize_pfact')
!!
!!*+  Local Variables
!!      real       biomass               ! total crop biomass
!!      real       P_conc                ! actual P concentration (g/g)
!!
!!      real       P_def                 ! P factor (0-1)
!!      real       P_conc_ratio          ! available P as fraction of P capacity
!!                                       ! (0-1)
!!
!!*- Implementation Section ----------------------------------
!!
!!      call push_routine (my_name)
!!      call print_routine (my_name)
!!
!!         ! calculate actual P conc
!!      biomass    =  sum_real_array (g_dm_green, max_part)
!!     :           +  sum_real_array (g_dm_senesced, max_part)
!!     :           +  sum_real_array (g_dm_dead, max_part)
!!
!!      P_conc = divide (g_plant_p, biomass, 0.0)
!!
!!      P_conc_ratio = divide ((P_conc - g_P_conc_min)
!!     :                      ,(g_P_conc_max - g_P_conc_min)
!!     :                      , 0.0)
!!
!!         ! calculate 0-1 P deficiency factors
!!
!!      P_def = k_pfact * P_conc_ratio
!!      pfact = bound (P_def, 0.0, 1.0)
!!
!!      call pop_routine (my_name)
!!      return
!!      end subroutine
!!

* ====================================================================
       subroutine maize_nit_demand_est (Option)
* ====================================================================
            Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option

*+  Purpose
*      Calculate an approximate nitrogen demand for today's growth.
*      The estimate basically = n to fill the plant up to maximum
*      nitrogen concentration.

*+  Mission Statement
*     Calculate nitrogen demand for growth

*+  Changes
*     14-05-1997 - huth - Programmed and Specified

*+  Constant Values
      integer    num_demand_parts
      parameter (num_demand_parts = 4)
*
      character*(*) myname               ! name of current procedure
      parameter (myname = 'maize_nit_demand_est')

*+  Local Variables
      integer    current_phase
      real    dlt_dm_green_pot (max_part) ! potential (est) dlt dm green
      real    dlt_dm_pot_radn         ! pot dm production given radn
      real    dlt_N_retrans(max_part) ! retranslocated N
      real    dm_green_tot            ! total dm green
      integer    part                    ! simple plant part counter
*
      integer    demand_parts(num_demand_parts)
      data demand_parts /root,leaf,stem,flower/
      save /demand_parts/

*- Implementation Section ----------------------------------
      call push_routine (myname)

      if (Option .eq. 1) then
            ! Option 1 is to assume that the distribution of plant
            ! C will be similar after today and so N demand is that
            ! required to raise all plant parts to critical N conc.

         ! calculate potential new shoot and root growth
      current_phase = int (g%current_stage)
         ! need to calculate dm using potential rue not affected by
         ! N and temperature
      dlt_dm_pot_radn = g%rue * g%radn_int
      dm_green_tot = sum_real_array (g%dm_green, max_part)
      do 100 part = 1, max_part
         dlt_dm_green_pot(part) = dlt_dm_pot_radn
     :                          * divide (g%dm_green(part)
     :                                   , dm_green_tot
     :                                   , 0.0)
         dlt_N_retrans(part) = 0.0
  100 continue

         call cproc_N_demand1
     :               (
     :                max_part
     :              , demand_parts
     :              , num_demand_parts
     :              , dlt_dm_pot_radn
     :              , dlt_dm_green_pot
     :              , g%dlt_dm_light
     :              , dlt_n_retrans
     :              , g%dm_green
     :              , g%n_conc_crit
     :              , g%n_conc_max
     :              , g%n_green
     :              , g%N_demand, g%N_max
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (myname)
      return
      end subroutine

*     ===========================================================
      subroutine Read_Constants_Maize ()
*     ===========================================================
            Use infrastructure
      implicit none

*+  Purpose
*       Crop initialisation - reads constants from constants file

*+  Mission Statement
*     Read in the constants for maize


*+  Changes
*     010994 sc   specified and programmed
*     070495 psc added extra constants (leaf_app etc.)
*     110695 psc added soil temp effects on plant establishment
*     270995 scc added leaf area options
*     100497 mjr added advection factor
*     0209998 sb deleted c%year_lb and c%year_ub. Used min_year and max_year.
*     201200 ew  generalised

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'Read_Constants_Maize')
*
      character  section_name*(*)
      parameter (section_name = 'constants')

*+  Local Variables
      integer    numvals               ! number of values returned

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)

      call write_string (new_line//'    - Reading constants')

      call read_char_var (section_name
     :                     , 'crop_type', '()'
     :                     , c%crop_type, numvals)

      call read_char_array (section_name
     :                     , 'stage_names', max_stage, '()'
     :                     , c%stage_names, numvals)

      call read_real_array (section_name
     :                     , 'stage_code', max_stage, '()'
     :                     , c%stage_code_list, numvals
     :                     , 0.0, 1000.0)

      call read_real_array (section_name
     :                     , 'rue', max_stage, '(g dm/mj)'
     :                     , c%rue, numvals
     :                     , 0.0, 1000.0)

      call read_real_array (section_name
     :                     , 'root_depth_rate', max_stage, '(mm)'
     :                     , c%root_depth_rate, numvals
     :                     , 0.0, 1000.0)

      call read_real_array (section_name
     :                     , 'ratio_root_shoot', max_stage, '()'
     :                     , c%ratio_root_shoot, numvals
     :                     , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'row_spacing_default', '(m)'
     :                    , c%row_spacing_default, numvals
     :                    , 0.0, 2.0)

      call read_real_var (section_name
     :                     , 'skiprow_default', '()'
     :                     , c%skip_row_default, numvals
     :                     , 0.0, 2.0)

      call read_real_array (section_name
     :                     , 'x_row_spacing', max_table, '(mm)'
     :                     , c%x_row_spacing, c%num_row_spacing
     :                     , 0.0, 2000.0)
      call read_real_array (section_name
     :                     , 'y_extinct_coef', max_table, '()'
     :                     , c%y_extinct_coef, c%num_row_spacing
     :                     , 0.0, 1.0)
       call read_real_array (section_name
     :                     , 'y_extinct_coef_dead', max_table, '()'
     :                     , c%y_extinct_coef_dead, c%num_row_spacing
     :                     , 0.0, 1.0)

         ! crop failure

      call read_real_var (section_name
     :                    , 'leaf_no_crit', '()'
     :                    , c%leaf_no_crit, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'tt_emerg_limit', '(oC)'
     :                    , c%tt_emerg_limit, numvals
     :                    , 0.0, 365.0)

      call read_real_var (section_name
     :                    , 'days_germ_limit', '(days)'
     :                    , c%days_germ_limit, numvals
     :                    , 0.0, 365.0)

      call read_real_var (section_name
     :                    , 'swdf_pheno_limit', '()'
     :                    , c%swdf_pheno_limit, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'swdf_photo_limit', '()'
     :                    , c%swdf_photo_limit, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'swdf_photo_rate', '()'
     :                    , c%swdf_photo_rate, numvals
     :                    , 0.0, 1.0)


         !    Maize_root_depth

      call read_real_var (section_name
     :                    , 'initial_root_depth', '(mm)'
     :                    , c%initial_root_depth, numvals
     :                    , 0.0, 1000.0)
      call read_real_var (section_name
     :                    , 'specific_root_length', '(mm/g)'
     :                    , c%specific_root_length, numvals
     :                    , 0.0, 1.e6)

      call read_real_array (section_name
     :                     , 'x_plant_rld', max_table, '(mm)'
     :                     , c%x_plant_rld, c%num_plant_rld
     :                     , 0.0, 0.1)
      call read_real_array (section_name
     :                     , 'y_rel_root_rate', max_table, '()'
     :                     , c%y_rel_root_rate, c%num_plant_rld
     :                     , 0.0, 1.0)

         !    Maize_leaf_area_init

      call read_real_var (section_name
     :                    , 'initial_tpla', '(mm^2)'
     :                    , c%initial_tpla, numvals
     :                    , 0.0, 100000.0)


         ! ANOTHER TEMPLATE OPTION!!!!
      call read_real_array (section_name
     :                     , 'x_lai', max_table, '()'
     :                     , c%x_lai, c%num_x_lai
     :                     , 0.0, 100.0)
      call read_real_array (section_name
     :                     , 'y_lai_sla_max', max_table, '()'
     :                     , c%y_lai_sla_max, c%num_x_lai
     :                     , 0.0, 100000.0)
      call read_real_array (section_name
     :                     , 'lai_sla_min', max_table, '()'
     :                     , c%lai_sla_min, c%num_x_lai
     :                     , 0.0, 100000.0)


         ! TEMPLATE OPTION
         !    Maize_leaf_area_devel_plant


      call read_real_var (section_name
     :                    , 'tpla_min', '()'
     :                    , c%tpla_min , numvals
     :                    , 0.0, 1000.0)

         !    Maize_get_cultivar_params

      call read_real_var (section_name
     :                    , 'head_grain_no_max_ub', '()'
     :                    , c%head_grain_no_max_ub, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'grain_gth_rate_ub', '()'
     :                    , c%grain_gth_rate_ub, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'tt_emerg_to_endjuv_ub', '()'
     :                    , c%tt_emerg_to_endjuv_ub, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'tt_flower_to_maturity_ub', '()'
     :                    , c%tt_flower_to_maturity_ub, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'tt_maturity_to_ripe_ub', '()'
     :                    , c%tt_maturity_to_ripe_ub, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'tt_flower_to_start_grain_ub', '()'
     :                    , c%tt_flower_to_start_grain_ub, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'tt_flag_to_flower_ub', '()'
     :                    , c%tt_flag_to_flower_ub, numvals
     :                    , 0.0, 1000.0)

         !    Maize_transp_eff

      call read_real_var (section_name
     :                    , 'svp_fract', '()'
     :                    , c%svp_fract, numvals
     :                    , 0.0, 1.0)

         !    cproc_sw_demand_bound

      call read_real_var (section_name
     :                    , 'eo_crop_factor_default', '()'
     :                    , c%eo_crop_factor_default, numvals
     :                    , 0.0, 100.)

      call read_real_array (section_name
     :                     , 'transp_eff_cf', max_stage, '(kpa)'
     :                     , c%transp_eff_cf, numvals
     :                     , 0.0, 1.0)

      call read_real_array (section_name
     :                     , 'n_fix_rate', max_stage, '()'
     :                     , c%n_fix_rate, numvals
     :                     , 0.0, 1.0)

         ! TEMPLATE OPTION
         !    Maize_grain_no

      call read_real_var (section_name
     :                    , 'head_grain_no_crit', '()'
     :                    , c%head_grain_no_crit, numvals
     :                    , 0.0, 1000.0)


         !    Maize_plants_barren

      call read_real_var (section_name
     :                    , 'barren_crit', '()'
     :                    , c%barren_crit, numvals
     :                    , 0.0, 1.0)

         !    Maize_germination

      call read_real_var (section_name
     :                    , 'pesw_germ', '(mm/mm)'
     :                    , c%pesw_germ, numvals
     :                    , 0.0, 1.0)
      call read_real_array (section_name
     :                     , 'fasw_emerg', max_table, '()'
     :                     , c%fasw_emerg, c%num_fasw_emerg
     :                     , 0.0, 1.0)
      call read_real_array (section_name
     :                     , 'rel_emerg_rate', max_table, '()'
     :                     , c%rel_emerg_rate, c%num_fasw_emerg
     :                     , 0.0, 1.0)

         ! TEMPLATE OPTION
         !    Maize_grain_no

      call read_real_var (section_name
     :                    , 'grain_n_conc_min', '()'
     :                    , c%grain_N_conc_min, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'seed_wt_min', '(g/seed)'
     :                    , c%seed_wt_min, numvals
     :                    , 0.0, 100.0)


         ! ANOTHER TEMPLATE OPTION!!!!!

      call read_real_array (section_name
     :                    , 'grno_grate', max_table, '()'
     :                    , c%grno_grate, c%num_grno_grate
     :                    , 0.0, 10.0)
      call read_real_array (section_name
     :                    , 'grno_fract', max_table, '()'
     :                    , c%grno_fract, c%num_grno_grate
     :                    , 0.0, 1.0)

         !    Maize_leaf_appearance

      call read_real_var (section_name
     :                    , 'leaf_no_at_emerg', '()'
     :                    , c%leaf_no_at_emerg, numvals
     :                    , 0.0, 100.0)

         !    Maize_N_uptake

      call read_real_var (section_name
     :                    , 'no3_diffn_const', '(days)'
     :                    , c%NO3_diffn_const, numvals
     :                    , 0.0, 100.0)

      call read_char_var (section_name
     :                     , 'n_supply_preference', '()'
     :                     , c%n_supply_preference, numvals)

         !    Maize_phenology_init

      call read_real_var (section_name
     :                    , 'shoot_lag', '(oC)'
     :                    , c%shoot_lag, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'shoot_rate', '(oC/mm)'
     :                    , c%shoot_rate, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'leaf_app_rate', '(oC)'
     :                    , c%leaf_app_rate, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'leaf_app_rate1', '(oC)'
     :                    , c%leaf_app_rate1, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'leaf_app_rate2', '(oC)'
     :                    , c%leaf_app_rate2, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'leaf_no_rate_change', '()'
     :                    , c%leaf_no_rate_change, numvals
     :                    , 0.0, 30.0)

         !    Maize_dm_init

      call read_real_var (section_name
     :                    , 'dm_leaf_init', '(g/plant)'
     :                    , c%dm_leaf_init, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'dm_root_init', '(g/plant)'
     :                    , c%dm_root_init, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'dm_stem_init', '(g/plant)'
     :                    , c%dm_stem_init, numvals
     :                    , 0.0, 1000.0)

         !    Maize_get_root_params

      call read_real_var (section_name
     :                    , 'll_ub', '()'
     :                    , c%ll_ub, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'kl_ub', '()'
     :                    , c%kl_ub, numvals
     :                    , 0.0, 1000.0)

         !    Maize_leaf_no_final

      call read_real_var (section_name
     :                    , 'leaf_init_rate', '(oC)'
     :                    , c%leaf_init_rate, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'leaf_no_seed', '(leaves)'
     :                    , c%leaf_no_seed, numvals
     :                    , 0.0, 100.0)

c      call read_real_var (section_name
c     :                    , 'floral_init_error', '(oc)'
c     :                    , c%floral_init_error, numvals
c     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'leaf_no_min', '()'
     :                   , c%leaf_no_min, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'leaf_no_max', '()'
     :                   , c%leaf_no_max, numvals
     :                   , 0.0, 100.0)

         !    Maize_retranslocate

      call read_real_var (section_name
     :                    , 'stem_trans_frac', '()'
     :                    , c%stem_trans_frac, numvals
     :                    , 0.0, 1.0)

      call read_real_var (section_name
     :                    , 'leaf_trans_frac', '()'
     :                    , c%leaf_trans_frac, numvals
     :                    , 0.0, 1.0)

         !    Maize_watck

      call read_real_var (section_name
     :                    , 'minsw', '()'
     :                    , c%minsw, numvals
     :                    , 0.0, 1000.0)

         ! TEMPLATE OPTION
         !    Maize_dm_grain

      call read_real_var (section_name
     :                    , 'swdf_grain_min', '()'
     :                    , c%swdf_grain_min, numvals
     :                    , 0.0, 100.0)

         ! TEMPLATE OPTION

         !    Maize_N_dlt_grain_conc

      call read_real_var (section_name
     :                    , 'sw_fac_max', '()'
     :                    , c%sw_fac_max, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'temp_fac_min', '()'
     :                    , c%temp_fac_min, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'sfac_slope', '()'
     :                    , c%sfac_slope, numvals
     :                    , -10.0, 0.0)

      call read_real_var (section_name
     :                    , 'tfac_slope', '()'
     :                    , c%tfac_slope, numvals
     :                    , 0.0, 100.0)

         !    Maize_leaf_death

cSCC changed lower limit from 10.0 to 0.0
c      call read_real_var (section_name
c     :                    , 'leaf_no_dead_const', '()'
c     :                    , c%leaf_no_dead_const, numvals
c     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'leaf_no_dead_slope', '()'
     :                    , c%leaf_no_dead_slope, numvals
     :                    , 0.0, 100.0)

         !    Maize_get_other_variables

         ! checking the bounds of the bounds..
      call read_real_var (section_name
     :                    , 'latitude_ub', '(oL)'
     :                    , c%latitude_ub, numvals
     :                    , -90.0, 90.0)

      call read_real_var (section_name
     :                    , 'latitude_lb', '(oL)'
     :                    , c%latitude_lb, numvals
     :                    , -90.0, 90.0)

      call read_real_var (section_name
     :                    , 'maxt_ub', '(oC)'
     :                    , c%maxt_ub, numvals
     :                    , 0.0, 60.0)

      call read_real_var (section_name
     :                    , 'maxt_lb', '(oC)'
     :                    , c%maxt_lb, numvals
     :                    , 0.0, 60.0)

      call read_real_var (section_name
     :                    , 'mint_ub', '(oC)'
     :                    , c%mint_ub, numvals
     :                    , 0.0, 40.0)

      call read_real_var (section_name
     :                    , 'mint_lb', '(oC)'
     :                    , c%mint_lb, numvals
     :                    , -100.0, 100.0)

      call read_real_var (section_name
     :                    , 'radn_ub', '(MJ/m^2)'
     :                    , c%radn_ub, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'radn_lb', '(MJ/m^2)'
     :                    , c%radn_lb, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'dlayer_ub', '(mm)'
     :                    , c%dlayer_ub, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'dlayer_lb', '(mm)'
     :                    , c%dlayer_lb, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'dul_dep_ub', '(mm)'
     :                    , c%dul_dep_ub, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'dul_dep_lb', '(mm)'
     :                    , c%dul_dep_lb, numvals
     :                    , 0.0, 10000.0)

                                ! 8th block
      call read_real_var (section_name
     :                    , 'sw_dep_ub', '(mm)'
     :                    , c%sw_dep_ub, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'sw_dep_lb', '(mm)'
     :                    , c%sw_dep_lb, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'no3_ub', '(kg/ha)'
     :                    , c%NO3_ub, numvals
     :                    , 0.0, 100000.0)

      call read_real_var (section_name
     :                    , 'no3_lb', '(kg/ha)'
     :                    , c%NO3_lb, numvals
     :                    , 0.0, 100000.0)

      call read_real_var (section_name
     :                    , 'no3_min_ub', '(kg/ha)'
     :                    , c%NO3_min_ub, numvals
     :                    , 0.0, 100000.0)

      call read_real_var (section_name
     :                    , 'no3_min_lb', '(kg/ha)'
     :                    , c%NO3_min_lb, numvals
     :                    , 0.0, 100000.0)


      call read_real_var (section_name
     :                    , 'nh4_ub', '(kg/ha)'
     :                    , c%Nh4_ub, numvals
     :                    , 0.0, 100000.0)

      call read_real_var (section_name
     :                    , 'nh4_lb', '(kg/ha)'
     :                    , c%Nh4_lb, numvals
     :                    , 0.0, 100000.0)

      call read_real_var (section_name
     :                    , 'nh4_min_ub', '(kg/ha)'
     :                    , c%Nh4_min_ub, numvals
     :                    , 0.0, 100000.0)

      call read_real_var (section_name
     :                    , 'nh4_min_lb', '(kg/ha)'
     :                    , c%Nh4_min_lb, numvals
     :                    , 0.0, 100000.0)



           call read_real_var (section_name
     :                    , 'canopy_height_max', '()'
     :                    , c%height_max, numvals
     :                    , 0.0, 5000.0)




         !    Maize_event

      call read_real_var (section_name
     :                    , 'grn_water_cont', '(g/g)'
     :                    , c%grn_water_cont, numvals
     :                    , 0.0, 1.0)

         !    Maize_dm_partition

      call read_real_var (section_name
     :                    , 'partition_rate_leaf', '()'
     :                    , c%partition_rate_leaf, numvals
     :                    , 0.0, 1.0)

      call read_real_var (section_name
     :                    , 'frac_stem2flower', '()'
     :                    , c%frac_stem2flower, numvals
     :                    , 0.0, 1.0)

         ! TEMPLATE OPTION
         !    Maize_grain_no

      call read_real_var (section_name
     :                    , 'htstress_coeff', '()'
     :                    , c%htstress_coeff, numvals
     :                    , 0.0, 1.0)

         !    Maize_dm_senescence

      call read_real_var (section_name
     :                    , 'dm_root_sen_frac', '()'
     :                    , c%dm_root_sen_frac, numvals
     :                    , 0.0, 1.0)

      call read_real_var (section_name
     :                    , 'dm_leaf_sen_frac', '()'
     :                    , c%dm_leaf_sen_frac, numvals
     :                    , 0.0, 1.0)

         !    Maize_dm_dead_detachment

      call read_real_array (section_name
     :                    , 'dead_detach_frac', max_part, '()'
     :                    , c%dead_detach_frac, numvals
     :                    , 0.0, 1.0)

      call read_real_array (section_name
     :                    , 'sen_detach_frac', max_part, '()'
     :                    , c%sen_detach_frac, numvals
     :                    , 0.0, 1.0)

      call read_real_var (section_name
     :                    , 'dm_leaf_detach_frac', '()'
     :                    , c%dm_leaf_detach_frac, numvals
     :                    , 0.0, 1.0)


         ! TEMPLATE OPTION
         !    Maize_leaf_area_devel

c      call read_real_var (section_name
c     :                    , 'leaf_no_correction', '()'
c     :                    , c%leaf_no_correction, numvals
c     :                    , 0.0, 100.0)

         ! TEMPLATE OPTION
         !    Maize_leaf_size

      call read_real_var (section_name
     :                    , 'x0_const', '()'
     :                    , c%x0_const, numvals
     :                    , -10.0, 100.0)

      call read_real_var (section_name
     :                    ,'x0_slope', '()'
     :                    , c%x0_slope, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'y0_const', '()'
     :                    , c%y0_const, numvals
     :                    , -10000.0, 100000.0)

      call read_real_var (section_name
     :                    , 'y0_slope', '()'
     :                    , c%y0_slope, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'a_const', '()'
     :                    , c%a_const, numvals
     :                    , -100.0, 0.0)

      call read_real_var (section_name
     :                    , 'a_slope1', '()'
     :                    , c%a_slope1, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'a_slope2', '()'
     :                    , c%a_slope2, numvals
     :                    , -100.0, 0.0)

      call read_real_var (section_name
     :                    , 'b_const', '()'
     :                    , c%b_const, numvals
     :                    , -10.0, 100.0)

      call read_real_var (section_name
     :                    , 'b_slope1', '()'
     :                    , c%b_slope1, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'b_slope2', '()'
     :                    , c%b_slope2, numvals
     :                    , -100.0, 0.0)

         ! TEMPLATE OPTION
         !    Maize_leaf_area_sen_light

      call read_real_var (section_name
     :                   , 'lai_sen_light', '(m^2/m^2)'
     :                   , c%lai_sen_light, numvals
     :                   , 3.0, 20.0)

      call read_real_var (section_name
     :                    , 'sen_light_slope', '()'
     :                    , c%sen_light_slope, numvals
     :                    , 0.0, 100.0)


         ! TEMPLATE OPTION
         !    Maize_leaf_area_sen_frost

      call read_real_array (section_name
     :                   , 'x_temp_senescence', max_table, '(oC)'
     :                   , c%x_temp_senescence, c%num_temp_senescence
     :                   , -20.0, 20.0)

      call read_real_array (section_name
     :                   , 'y_senescence_fac', max_table, '()'
     :                   , c%y_senescence_fac, c%num_temp_senescence
     :                   , 0.0, 1.0)

         ! TEMPLATE OPTION
         !    Maize_leaf_area_sen_water

      call read_real_var (section_name
     :                    , 'sen_rate_water', '()'
     :                    , c%sen_rate_water, numvals
     :                    , 0.0, 100.0)


         !    Maize_phenology_init

      call read_real_var (section_name
     :                   , 'twilight', '(o)'
     :                   , c%twilight, numvals
     :                   , -90.0, 90.0)

         ! TEMPLATE OPTION
         !    Maize_heat_stress

      call read_real_var (section_name
     :                   , 'temp_grain_crit_stress', '(oC)'
     :                   , c%temp_grain_crit_stress, numvals
     :                   , 20.0, 50.0)

         !    Maize_N_conc_limits

      call read_real_array (section_name
     :                     , 'x_stage_code', max_stage, '()'
     :                     , c%x_stage_code, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_crit_leaf', max_stage, '()'
     :                     , c%y_N_conc_crit_leaf, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_max_leaf', max_stage, '()'
     :                     , c%y_N_conc_max_leaf, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_min_leaf', max_stage, '()'
     :                     , c%y_N_conc_min_leaf, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_crit_stem', max_stage, '()'
     :                     , c%y_N_conc_crit_stem, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_max_stem', max_stage, '()'
     :                     , c%y_N_conc_max_stem, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_min_stem', max_stage, '()'
     :                     , c%y_N_conc_min_stem, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_crit_flower', max_stage, '()'
     :                     , c%y_N_conc_crit_flower, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_max_flower', max_stage, '()'
     :                     , c%y_N_conc_max_flower, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_min_flower', max_stage, '()'
     :                     , c%y_N_conc_min_flower, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'n_conc_crit_grain', '()'
     :                   , c%N_conc_crit_grain, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'n_conc_max_grain', '()'
     :                   , c%N_conc_max_grain, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'n_conc_min_grain', '()'
     :                   , c%N_conc_min_grain, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'n_conc_crit_root', '()'
     :                   , c%N_conc_crit_root, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'n_conc_max_root', '()'
     :                   , c%N_conc_max_root, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'n_conc_min_root', '()'
     :                   , c%N_conc_min_root, numvals
     :                   , 0.0, 100.0)


      call read_real_array (section_name
     :                     , 'y_n_conc_min_root', max_stage, '()'
     :                     , c%y_N_conc_min_root, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_crit_root', max_stage, '()'
     :                     , c%y_N_conc_crit_root, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_max_root', max_stage, '()'
     :                     , c%y_N_conc_max_root, c%num_N_conc_stage
     :                     , 0.0, 100.0)



         !    Maize_N_init

      call read_real_array (section_name
     :                     , 'n_init_conc', max_part, '()'
     :                     , c%n_init_conc, numvals
     :                     , 0.0, 100.0)

         !    Maize_N_senescence

      call read_real_array (section_name
     :                     , 'n_sen_conc', max_part, '()'
     :                     , c%n_sen_conc, numvals
     :                     , 0.0, 1.0)

         !    nfact

      call read_real_var (section_name
     :                   , 'N_fact_photo', '()'
     :                   , c%N_fact_photo, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'N_fact_pheno', '()'
     :                   , c%N_fact_pheno, numvals
     :                   , 0.0, 100.0)

!scc put this in for consistence w. sugar

      call read_real_var (section_name
     :                   , 'N_fact_expansion', '()'
     :                   , c%N_fact_expansion, numvals
     :                   , 0.0, 100.0)

!!      ! Phosphorus
!!      ! ----------
!!
!!      call read_real_array (section_name
!!     :                     , 'p_stage_code', max_stage, '()'
!!     :                     , c%p_stage_code, c%num_P_conc_stage
!!     :                     , 0.0, 100.0)
!!
!!      call read_real_array (section_name
!!     :                     , 'p_conc_max', max_stage, '()'
!!     :                     , c%p_conc_max, c%num_P_conc_stage
!!     :                     , 0.0, 100.0)
!!
!!      call read_real_array (section_name
!!     :                     , 'p_conc_min', max_stage, '()'
!!     :                     , c%p_conc_min, c%num_P_conc_stage
!!     :                     , 0.0, 100.0)
!!
!!      call read_real_var (section_name
!!     :                   , 'k_pfact_photo', '()'
!!     :                   , c%k_pfact_photo, numvals
!!     :                   , 0.0, 100.0)
!!
!!      call read_real_var (section_name
!!     :                   , 'k_pfact_pheno', '()'
!!     :                   , c%k_pfact_pheno, numvals
!!     :                   , 0.0, 100.0)
!!
!!      call read_real_var (section_name
!!     :                   , 'k_pfact_expansion', '()'
!!     :                   , c%k_pfact_expansion, numvals
!!     :                   , 0.0, 100.0)
!!
!!      call read_real_var (section_name
!!     :                   , 'k_pfact_grain', '()'
!!     :                   , c%k_pfact_grain, numvals
!!     :                   , 0.0, 100.0)
!!
!!      call read_real_var (section_name
!!     :                   , 'p_uptake_factor', '()'
!!     :                   , c%p_uptake_factor, numvals
!!     :                   , 0.0, 10.0)

         !    Maize_rue_reduction

      call read_real_array (section_name
     :                     , 'x_ave_temp', max_table, '(oC)'
     :                     , c%x_ave_temp, c%num_ave_temp
     :                     , 0.0, 100.0)

!cscc added the following to do 3-hour effect on RUE

      call read_real_array (section_name
     :                     , 'y_stress_photo', max_table, '()'
     :                     , c%y_stress_photo, c%num_factors
     :                     , 0.0, 1.0)

         ! TEMPLATE OPTION
         !    Maize_dm_grain

      call read_real_array (section_name
     :                     , 'x_temp_grain', max_table, '(oC)'
     :                     , c%x_temp_grain, c%num_temp_grain
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_grain_rate', max_table, '()'
     :                     , c%y_grain_rate, c%num_temp_grain
     :                     , 0.0, 1.0)

         !    Maize_tt

      call read_real_array (section_name
     :                     , 'x_temp', max_table, '(oC)'
     :                     , c%x_temp, c%num_temp
     :                     , -10.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_tt', max_table, '(oC)'
     :                     , c%y_tt, c%num_temp
     :                     , -10.0, 100.0)
!cpsc
      call read_real_array (section_name
     :                     , 'x_weighted_temp', max_table, '(oC)'
     :                     , c%x_weighted_temp, c%num_weighted_temp
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_plant_death', max_table, '(oC)'
     :                     , c%y_plant_death, c%num_weighted_temp
     :                     , 0.0, 100.0)

         ! TEMPLATE OPTION
         !    Maize_tt_other

      ! call read_real_array (section_name
      !:                     , 'x_temp_other', max_table, '(oC)'
      !:                     , c%x_temp_other, c%num_temp_other
      !:                     , 0.0, 100.0)

      ! call read_real_array (section_name
      !:                     , 'y_tt_other', max_table, '(oC)'
      !:                     , c%y_tt_other, c%num_temp_other
      !:                     , 0.0, 100.0)

         ! TEMPLATE OPTION
         !    Maize_tt_curv

      ! call read_real_var (section_name
      !:                    , 'imin', '()'
      !:                    , c%imin, numvals
      !:                    , 0.0, 100.0)

      ! call read_real_var (section_name
      !:                    , 'iopt', '()'
      !:                    , c%iopt, numvals
      !:                    , 0.0, 100.0)

      ! call read_real_var (section_name
      !:                    , 'imax', '()'
      !:                    , c%imax, numvals
      !:                    , 0.0, 100.0)

      ! call read_real_var (section_name
      !:                    , 'ioptr', '()'
      !:                    , c%ioptr, numvals
      !:                    , 0.0, 100.0)

      ! call read_real_var (section_name
      !:                    , 'amin', '()'
      !:                    , c%amin, numvals
      !:                    , 0.0, 100.0)

      ! call read_real_var (section_name
      !:                    , 'aopt', '()'
      !:                    , c%aopt, numvals
      !:                    , 0.0, 100.0)

      ! call read_real_var (section_name
      !:                    , 'amax', '()'
      !:                    , c%amax, numvals
      !:                    , 0.0, 100.0)

      ! call read_real_var (section_name
      !:                    , 'aoptr', '()'
      !:                    , c%aoptr, numvals
      !:                    , 0.0, 100.0)

         !    swdef

      call read_real_array (section_name
     :                     , 'x_sw_demand_ratio', max_table, '()'
     :                     , c%x_sw_demand_ratio, c%num_sw_demand_ratio
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_swdef_leaf', max_table, '()'
     :                     , c%y_swdef_leaf, c%num_sw_demand_ratio
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'x_sw_avail_ratio', max_table, '()'
     :                     , c%x_sw_avail_ratio, c%num_sw_avail_ratio
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_swdef_pheno', max_table, '()'
     :                     , c%y_swdef_pheno, c%num_sw_avail_ratio
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'x_sw_ratio', max_table, '()'
     :                     , c%x_sw_ratio, c%num_sw_ratio
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_sw_fac_root', max_table, '()'
     :                     , c%y_sw_fac_root, c%num_sw_ratio
     :                     , 0.0, 100.0)


      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine Read_Cultivar_Params_Maize (cultivar)
*     ===========================================================
            Use infrastructure
      implicit none

*+  Sub-Program Arguments
      character  cultivar*(*)          ! (INPUT) keyname of cultivar in crop
                                       ! parameter file


*+  Purpose
*       Get cultivar parameters for named cultivar, from crop parameter file.

*+  Mission Statement
*     Get cultivar parameters for named cultivar

*+  Changes
*       090994 sc   specified and programmed
*       201200 ew   generalised




*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Crop_Read_Cultivar_Params')

*+  Local Variables
      character  string*300            ! output string
      integer    numvals               ! number of values read
      integer    i

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)

      call write_string (
     :                 new_line//'   - Reading Cultivar Parameters')

         ! TEMPLATE OPTION
         !   Maize_leaf_area_devel_plant


         ! TEMPLATE OPTION
         !   Maize_check_grain_no  Maize_grain_no

      call read_real_var (cultivar
     :                    , 'head_grain_no_max', '()'
     :                    , p%head_grain_no_max, numvals
     :                    , 0.0, c%head_grain_no_max_ub)

         ! TEMPLATE OPTION
         !   Maize_dm_grain

      call read_real_var (cultivar
     :                    , 'grain_gth_rate', '()'
     :                    , p%grain_gth_rate, numvals
     :                    , 0.0, c%grain_gth_rate_ub)

         !   Maize_phenology_init

      call read_real_var (cultivar
     :                    , 'tt_emerg_to_endjuv', '()'
     :                    , p%tt_emerg_to_endjuv, numvals
     :                    , 0.0, c%tt_emerg_to_endjuv_ub)

      call read_integer_var (cultivar
     :                    , 'est_days_endjuv_to_init', '()'
     :                    , p%est_days_endjuv_to_init, numvals
     :                    , 0, 100)

      call read_real_var (cultivar
     :                    , 'tt_endjuv_to_init', '()'
     :                    , p%tt_endjuv_to_init, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (cultivar
     :                    , 'photoperiod_crit1', '()'
     :                    , p%photoperiod_crit1, numvals
     :                    , 0.0, 24.0)

      call read_real_var (cultivar
     :                    , 'photoperiod_crit2', '()'
     :                    , p%photoperiod_crit2, numvals
     :                    , 0.0, 24.0)

      call read_real_var (cultivar
     :                    , 'photoperiod_slope', '()'
     :                    , p%photoperiod_slope, numvals
     :                    , 0.0, 200.0)

      call read_real_var (cultivar
     :                    , 'tt_flower_to_maturity', '()'
     :                    , p%tt_flower_to_maturity, numvals
     :                    , 0.0, c%tt_flower_to_maturity_ub)

      call read_real_var (cultivar
     :                    , 'tt_flag_to_flower', '()'
     :                    , p%tt_flag_to_flower, numvals
     :                    , 0.0, c%tt_flag_to_flower_ub)

      call read_real_var (cultivar
     :                    , 'tt_flower_to_start_grain', '()'
     :                    , p%tt_flower_to_start_grain, numvals
     :                    , 0.0, c%tt_flower_to_start_grain_ub)


      call read_real_var (cultivar
     :                    , 'tt_maturity_to_ripe', '()'
     :                    , p%tt_maturity_to_ripe, numvals
     :                    , 0.0, c%tt_maturity_to_ripe_ub)

      call read_real_array (cultivar
     :                     , 'x_stem_wt', max_table, '()'
     :                     , p%x_stem_wt, p%num_stem_wt
     :                     , 0.0, 1000.0)

      call read_real_array (cultivar
     :                     , 'y_height', max_table, '()'
     :                     , p%y_height, p%num_stem_wt
     :                     , 0.0, 5000.0)


             ! report

      string = '    ------------------------------------------------'
      call write_string (string)

      write (string, '(4x,2a)')
     :                'Cultivar                 = ', cultivar
      call write_string (string)

      write (string, '(4x, a, i7)')
     :                'est_days_endjuv_to_init  = '
     :               , p%est_days_endjuv_to_init
      call write_string (string)

      write (string, '(4x, a, f7.2)')
     :                'tt_emerg_to_endjuv       = '
     :               , p%tt_emerg_to_endjuv
      call write_string (string)

      write (string, '(4x, a, f7.2)')
     :                'tt_flower_to_maturity    = '
     :               , p%tt_flower_to_maturity
      call write_string (string)

      write (string, '(4x, a, f7.2)')
     :                'head_grain_no_max        = '
     :               , p%head_grain_no_max
      call write_string (string)

      write (string, '(4x, a, f7.2)')
     :                'grain_gth_rate           = '
     :               , p%grain_gth_rate
      call write_string (string)

      write (string, '(4x, a, f7.2)')
     :                'tt_flag_to_flower        = '
     :               , p%tt_flag_to_flower
      call write_string (string)

      write (string, '(4x, a, f7.2)')
     :                'tt_flower_to_start_grain = '
     :               , p%tt_flower_to_start_grain
      call write_string (string)

      write (string, '(4x, a, f7.2)')
     :                'tt_maturity_to_ripe      = '
     :               , p%tt_maturity_to_ripe
      call write_string (string)


         ! TEMPLATE OPTION
!       write (string, '(4x, a, f7.3)')
!     :                'tiller_no_fertile        = '
!     :               , p%tiller_no_fertile
!       call write_string (string)


      write (string, '(4x, a, 10f7.1)')
     :                'x_stem_wt      = '
     :               , (p%x_stem_wt(i), i=1,p%num_stem_wt)
      call write_string (string)

      write (string, '(4x, a, 10f7.1)')
     :                'y_height      = '
     :               , (p%y_height(i), i=1,p%num_stem_wt)
      call write_string (string)


      string = '    ------------------------------------------------'
      call write_string (string)

      call write_string (new_line//new_line)

      call pop_routine (my_name)

      return
      end subroutine




*     ===========================================================
      subroutine Maize_bio_retrans ()
*     ===========================================================
            Use infrastructure
      implicit none

*+  Sub-Program Arguments

*+  Purpose
*       Retranslocate biomass.

*+  Mission Statement
*     Retranslocate biomass

*+  Changes
*     5/9/96 dph
*     970317 slw new template form

*+  Constant Values
      integer    num_supply_pools
      parameter (num_supply_pools = 2)
*
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_bio_retrans')

*+  Local Variables
      integer    supply_pools(num_supply_pools)
      data supply_pools /stem,leaf/
      save /supply_pools/

*- Implementation Section ----------------------------------
      call push_routine (my_name)
      call print_routine (my_name)


         call cproc_dm_retranslocate1
     :               (
     :                g%current_stage
     :              , start_grain_fill
     :              , maturity
     :              , grain
     :              , max_part
     :              , supply_pools
     :              , num_supply_pools
     :              , g%dlt_dm_grain_demand
     :              , g%dlt_dm_green
     :              , g%dm_green
     :              , g%dm_plant_min
     :              , g%plants
     :              , g%dlt_dm_green_retrans
     :               )


      call pop_routine (my_name)
      return
      end subroutine





