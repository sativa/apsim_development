
      include 'Millet.inc'




*     ===========================================================
      subroutine millet_dm_init (dm_green, dm_plant_min)
*     ===========================================================
      use MilletModule
      implicit none
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dm_green(*)           ! (INPUT/OUTPUT) plant part weights
                                       ! (g/m^2)
      real       dm_plant_min(*)       ! (OUTPUT) minimum weight of each
                                       ! plant part (g/plant)

*+  Purpose
*       Initialise plant weights and plant weight minimums
*       at required instances.

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_dm_init')

*+  Local Variables
      real       dm_plant_leaf         ! dry matter in leaves (g/plant)
      real       dm_plant_stem         ! dry matter in stems (g/plant)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
         ! initialise plant weight
         ! initialisations - set up dry matter for leaf, stem, flower, grain
         ! and root
 
      if (on_day_of (emerg, g%current_stage, g%days_tot)) then
             ! seedling has just emerged.
 
             ! initialise root, stem and leaf.
 
         dm_green(root) = c%dm_root_init * g%plants
         dm_green(stem) = c%dm_stem_init * g%plants
         dm_green(leaf) = c%dm_leaf_init * g%plants
         dm_green(tiller) = 0.0
         dm_green(grain) = 0.0
         dm_green(flower) = 0.0
 
      elseif (on_day_of (flowering, g%current_stage, g%days_tot)) then
             ! we are at first day of flowering
             ! set the minimum weight of stem; used for retranslocation to grain
 
         dm_plant_stem = divide (dm_green(stem), g%plants, 0.0)
         dm_plant_min(stem) = dm_plant_stem * (1.0 - c%stem_trans_frac)
 
      elseif (on_day_of (start_grain_fill
     :                 , g%current_stage, g%days_tot)) then
 
             ! we are at first day of grainfill.
             ! set the minimum weight of leaf; used for translocation to grain
 
         dm_plant_leaf = divide (dm_green(leaf), g%plants, 0.0)
         dm_plant_min(leaf) = dm_plant_leaf * (1.0 - c%leaf_trans_frac)
 
      else   ! no changes
      endif
 
      call pop_routine (my_name)
      return
      end











*     ===========================================================
      subroutine millet_dm_retranslocate (dm_retranslocate)
*     ===========================================================
      use MilletModule
      implicit none
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dm_retranslocate(*)   ! (INPUT) actual change in plant part
                                       ! weights due to translocation (g/m^2)

*+  Purpose
*     Calculate plant dry matter delta's due to retranslocation (g/m^2)

*+  Changes
*       010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'millet_dm_retranslocate')

*+  Local Variables
      real       dlt_dm_retrans_leaf   ! carbohydrate removed from leaf
                                       ! (g/m^2)
      real       dlt_dm_retrans_stem   ! carbohydrate removed from stem in
                                       ! (g/m^2)
      real       dm_grain_differential ! demand in excess of available supply
                                       ! (g/m^2)
      real       dm_leaf_avail         ! carbohydrate avail from leaf(g/m^2)
      real       dm_stem_avail         ! carb available from stem (g/m^2)
      real       dm_leaf_pot           ! potential leaf weight (g/m^2)
      real       dm_stem_pot           ! potential stem weight (g/m^2)
      real       mass_balance          ! sum of translocated carbo (g/m^2)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
         ! now translocate carbohydrate between plant components
         ! this is different for each stage
 
      call fill_real_array (dm_retranslocate, 0.0, max_part)
 
      if (stage_is_between (start_grain_fill, maturity
     :                    , g%current_stage)) then
 
         if (g%dlt_dm_grain_demand .gt. g%dlt_dm_green(grain)) then
               ! we can translocate stem and leaf carbohydrate
               ! to grain if needed
 
            dm_grain_differential = g%dlt_dm_grain_demand
     :                            - g%dlt_dm_green(grain)
 
               ! get available carbohydrate in stem and leaf for transfer
 
            dm_stem_pot = g%dm_green(stem) + g%dlt_dm_green(stem)
            dm_stem_avail = dm_stem_pot - g%dm_plant_min(stem)*g%plants
            dm_stem_avail = l_bound (dm_stem_avail, 0.0)
 
            dm_leaf_pot = g%dm_green(leaf) + g%dlt_dm_green(leaf)
            dm_leaf_avail = dm_leaf_pot - g%dm_plant_min(leaf)*g%plants
            dm_leaf_avail = l_bound (dm_leaf_avail, 0.0)
 
               ! now find out how much to translocate.
               ! first chop at stem
 
            dlt_dm_retrans_stem = min (dm_grain_differential
     :                               , dm_stem_avail)
 
               ! second chop at leaf
 
            dm_grain_differential = dm_grain_differential
     :                            - dlt_dm_retrans_stem
            dlt_dm_retrans_leaf = min (dm_grain_differential
     :                               , dm_leaf_avail)
            dlt_dm_retrans_leaf = bound (dlt_dm_retrans_leaf
     :                                 , 0.0, dm_leaf_avail)
 
               ! get actual growth/withdrawal
 
            call fill_real_array (dm_retranslocate, 0.0, max_part)
            dm_retranslocate(stem) = - dlt_dm_retrans_stem
            dm_retranslocate(leaf) = - dlt_dm_retrans_leaf
            dm_retranslocate(grain) = - sum_real_array (dm_retranslocate
     :                                                , max_part)
 
               ! ??? check that stem and leaf are >= min wts
         else
               ! we have no retranslocation
            call fill_real_array (dm_retranslocate, 0.0, max_part)
         endif
 
      else
 
            ! we have no retranslocation
         call fill_real_array (dm_retranslocate, 0.0, max_part)
 
      endif
 
         ! now check that we have mass balance
 
      mass_balance = sum_real_array (dm_retranslocate, max_part)
      call bound_check_real_var (mass_balance, 0.0, 0.0
     :                         , 'dm_retranslocate mass balance')
 
      call pop_routine (my_name)
      return
      end











*     ===========================================================
      subroutine millet_leaf_number_final (leaf_no_final)
*     ===========================================================
      use MilletModule
      implicit none
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       leaf_no_final         ! (OUTPUT) maximum total leaf number

*+  Purpose
*       Calculate total leaf number.  This is set at floral_initialising and
*       is set at an approximated number at germination to allow
*       other calculations to proceed until the correct number is known.

*+  Changes
*     010994 jngh specified and programmed
*     070495 psc  changed from emerg to germ

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_leaf_number_final')

*+  Local Variables
cglh      real       tt_cum                ! cumulative dtt from sowing (deg day)
      real       tt_floral_init        ! cumulative dtt from sowing
                                       ! to true floral initiation (deg day)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
          ! set total leaf number
 
      if (on_day_of (emerg, g%current_stage, g%days_tot)) then
 
               ! estimate the final leaf no from an approximated thermal
               ! time for the period from emergence to floral initiation.
 
         tt_floral_init = sum_between (germ, floral_init, g%phase_tt)
cglh         tt_cum = sum_between (emerg, floral_init, g%phase_tt)
cglh         tt_floral_init = tt_cum - c%floral_init_error
 
            ! just check that maths is ok.
cglh         call bound_check_real_var (tt_floral_init, 0.0, tt_cum
cglh     :                             , 'tt_floral_init')
         leaf_no_final = divide (tt_floral_init
     :                         , c%leaf_init_rate, 0.0)
     :                 + c%leaf_no_seed
 
         call bound_check_real_var (leaf_no_final
     :                            , c%leaf_no_min, c%leaf_no_max
     :                            , 'leaf_no_final')
 
      elseif (on_day_of (floral_init, g%current_stage, g%days_tot)) then
 
               ! now we know the thermal time, get the actual final leaf no.
 
         tt_floral_init = sum_between (germ, floral_init, g%tt_tot)
 
cglh         tt_cum = sum_between (emerg, floral_init, g%tt_tot)
cglh         tt_floral_init = tt_cum - c%floral_init_error
 
            ! just check that maths is ok.
cglh         call bound_check_real_var (tt_floral_init, 0.0, tt_cum
cglh     :                             , 'tt_floral_init')
 
         if (g%stem_class .eq. class_main) then
 
            ! calculate main stem final leaf no.
 
            leaf_no_final = divide (tt_floral_init
     :                         , c%leaf_init_rate, 0.0)
     :                 + c%leaf_no_seed
 
cgd
            g%leaf_no_ref = leaf_no_final
 
         else
            ! tillers use main stem leaf no final
         endif
 
 
         leaf_no_final = g%leaf_no_ref - c%leaf_no_diff
 
!         write (*,*) leaf_no_final,g%leaf_no_ref,c%leaf_no_diff
!         write (*,*) 'fln',leaf_no_final
!
!         write (*,*) g%leaf_no_ref,leaf_no_final
!
!         write (*,*) g%current_stage
!         write (*,*) leaf_no_final,tt_floral_init,c%leaf_init_rate
         call bound_check_real_var (leaf_no_final
     :                            , c%leaf_no_min, c%leaf_no_max
     :                            , 'leaf_no_final')
 
      elseif (on_day_of (plant_end, g%current_stage, g%days_tot)) then
         leaf_no_final = 0.0
 
      else
         ! leave leaf_no_final as is.
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_leaf_appearance (dlt_leaf_no_pot)
*     ===========================================================
      use MilletModule
      implicit none
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_leaf_no_pot       ! (OUTPUT) new fraction of oldest
                                       ! expanding leaf

*+  Purpose
*       Return the fractional increase in emergence of the oldest
*       expanding leaf.
*       Note ! this does not take account of the other younger leaves
*       that are currently expanding

*+  Changes
*       031194 jngh specified and programmed
*       070495 psc  added 2nd leaf appearance rate

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_leaf_appearance')

*+  Local Variables
      real       leaf_no_remaining     ! number of leaves to go before all
                                       ! are fully expanded
      real       leaf_no_now           ! number of fully expanded leaves
      real       leaf_app_rate         ! rate of leaf appearance (oCd/leaf)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      leaf_no_now = sum_between (emerg, now, g%leaf_no)
      leaf_no_remaining = g%leaf_no_final - leaf_no_now
 
      if (leaf_no_now .le. c%leaf_no_rate_change) then
 
         leaf_app_rate = c%leaf_app_rate1
 
      else
 
         leaf_app_rate = c%leaf_app_rate2
 
      endif
 
      if (on_day_of (emerg, g%current_stage, g%days_tot)) then
 
             ! no leaf growth on first day because initialised elsewhere ???
 
         dlt_leaf_no_pot = 0.0
 
      elseif (leaf_no_remaining.gt.0.0) then
 
             ! we  haven't reached full number of leaves yet
 
             ! if leaves are still growing, the cumulative number of
             ! phyllochrons or fully expanded leaves is calculated from
             ! daily thermal time for the day.
 
         dlt_leaf_no_pot = divide (g%dlt_tt, leaf_app_rate, 0.0)
         dlt_leaf_no_pot = bound (dlt_leaf_no_pot, 0.0
     :                          , leaf_no_remaining)
 
      else
             ! we have full number of leaves.
 
         dlt_leaf_no_pot = 0.0
      endif
 
      call pop_routine (my_name)
      return
      end







*     ===========================================================
      subroutine millet_leaf_area ()
*     ===========================================================
      use MilletModule
      implicit none
      include   'convert.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Purpose
*       Simulate actual crop leaf area development - checks that leaf area
*       development matches DM production.

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_leaf_area')

*+  Local Variables
cejvo
      real       extinct_coef          ! extinction coeficient used to back
                                       !  calculate LAI from intercropped canopies
      real       lai_sum               ! total LAI of all crops (in units of this
                                       !  crop's LAI
      real       lai_ratio             ! ratio of actual to potential lai ()
      real       leaf_no_frac          ! ratio of actual to potential leaf
                                       ! appearance ()
      real       sla                   ! maximum SLA allowed as a function of LAI

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
         ! limit the delta leaf area by carbon supply
cejvo
      if (g%fr_intc_radn .gt. 0.0) then
 
         extinct_coef = linear_interp_real (g%row_spacing
     :                        , c%x_row_spacing, c%y_extinct_coef
     :                        , c%num_row_spacing)
 
!         lai_sum = g%lai +
!     :             divide (-alog(1.0 - g%fr_intc_radn),extinct_coef,0.0)
 
         lai_sum =  divide (-alog(1.0 - g%cover_green_sum)
     :                           ,extinct_coef,0.0)
 
!      write (*,*) g%lai,lai_sum,g%cover_green_sum,extinct_coef
!     :                        , g%fr_intc_radn
 
 
      else
 
         lai_sum = g%lai
 
      endif
 
      sla = linear_interp_real (lai_sum
     :                        , c%x_lai, c%y_sla_max
     :                        , c%num_lai)
 
      g%dlt_lai = u_bound (g%dlt_lai_pot
     :                   , g%dlt_dm_green(leaf) * sla * smm2sm)
 
!      g%dlt_lai = u_bound (g%dlt_lai_pot
!     :                   , g%dlt_dm_green(leaf)*c%sla_max * smm2sm)
 
      lai_ratio = divide (g%dlt_lai, g%dlt_lai_pot, 0.0)
 
      leaf_no_frac= linear_interp_real (lai_ratio
     :                        , c%x_lai_ratio, c%y_leaf_no_frac
     :                        , c%num_lai_ratio)
!cgd
!      write (*,*)lai_ratio, c%x_lai_ratio, c%y_leaf_no_frac,
!     :c%num_lai_ratio,leaf_no_frac
      g%dlt_leaf_no = g%dlt_leaf_no_pot * leaf_no_frac
 
      call pop_routine (my_name)
      return
      end




*     ===========================================================
      subroutine millet_N_init (N_green)
*     ===========================================================
      use MilletModule
      implicit none
      include 'science.pub'                       
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       N_green(*)            ! plant nitrogen (g/m^2)

*+  Purpose
*       Set plant nitrogen

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_N_init')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (on_day_of (emerg, g%current_stage, g%days_tot)) then
         N_green(root) = c%N_root_init_conc*g%dm_green(root)
         N_green(stem) = c%N_stem_init_conc*g%dm_green(stem)
         N_green(leaf) = c%N_leaf_init_conc*g%dm_green(leaf)
         N_green(flower) = 0.0
         N_green(grain) = 0.0
 
      else
      endif
 
      call pop_routine (my_name)
      return
      end




*     ===========================================================
      subroutine millet_tt (dlt_tt)
*     ===========================================================
      use MilletModule
      implicit none
      include 'science.pub'                       
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_tt                ! (OUTPUT) daily thermal time (oC)

*+  Purpose
*     Growing degree day (thermal time) is calculated.

*+  Notes
*     Eight interpolations of the air temperature are
*     calculated using a three-hour correction factor.
*     For each air three-hour air temperature, a value of growing
*     degree day is calculated.  The eight three-hour estimates
*     are then averaged to obtain the daily value of growing degree
*     days.

*+  Changes
*       140994 jngh specified and programmed
*       090695 psc  added N_fact for phenology stress

*+  Calls
!x      real  millet_swdef

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_tt')

*+  Local Variables
      real       dly_therm_time        ! thermal time for the day (deg day)

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      dly_therm_time = linint_3hrly_temp (g%maxt, g%mint
     :                 , c%x_temp, c%y_tt
     :                 , c%num_temp)
 
      if (stage_is_between (emerg, flag_leaf, g%current_stage)) then
 
!cpsc
         dlt_tt = dly_therm_time *
     :             min (g%swdef_pheno, g%nfact_pheno)
 
      else
 
         dlt_tt = dly_therm_time
      endif
 
      call pop_routine (my_name)
      return
      end






*     ===========================================================
      subroutine millet_phenology_init_o (phase_tt)
*     ===========================================================
      use MilletModule
      implicit none
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       phase_tt (*)          ! (INPUT/OUTPUT) cumulative growing
                                       ! degree days required for
                                       ! each stage (deg days)

*+  Purpose
*       Returns cumulative thermal time targets required for the
*       individual growth stages.

*+  Changes
*     010994 jngh specified and programmed
*     070495 psc added 2nd leaf appearance rate
*     090695 psc l_bound added (otherwise won't progress if phase_tt=0)

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_phenology_init_o')

*+  Local Variables
      integer    est_day_of_floral_init ! estimated day of year of floral
                                        ! initiation
      real       tt_emerg_to_flag_leaf ! growing degree days to develop
                                       ! and fully expand all leaves
                                       ! (deg day).  This is the gdd
                                       ! required from emergence to end of
                                       ! leaf growth
      real       photoperiod           ! hours of photosynthetic light (hours)
      real       photoperiod_active    ! hours of light in excess of threshold
                                       ! (hours)
      real       leaf_no               ! leaf no. above which app. rate changes

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (on_day_of (germ, g%current_stage, g%days_tot)) then
         phase_tt(germ_to_emerg) = c%shoot_lag
     :                           + g%sowing_depth*c%shoot_rate
         phase_tt(emerg_to_endjuv) = p%tt_emerg_to_endjuv
 
      elseif (on_day_of (emerg, g%current_stage, g%days_tot)) then
 
         est_day_of_floral_init = offset_day_of_year (g%year
     :                                  , g%day_of_year
     :                                  , p%est_days_emerg_to_init)
     :
         photoperiod = day_length (est_day_of_floral_init
     :                           , g%latitude, c%twilight)
 
         photoperiod_active = photoperiod - c%photoperiod_base
         photoperiod_active = bound (photoperiod_active, 0.0, 24.0)
!cpsc                          need below to exert stage when phase_tt = 0
cjh         phase_tt(endjuv_to_init) = l_bound (p%pp_endjuv_to_init
cjh     :                            * photoperiod_active, 0.1)
         phase_tt(endjuv_to_init) = p%pp_endjuv_to_init
     :                            * photoperiod_active
 
      elseif (stage_is_between (endjuv, floral_init
     :                        , g%current_stage)) then
         photoperiod = day_length (g%day_of_year, g%latitude
     :                           , c%twilight)
         photoperiod_active = photoperiod - c%photoperiod_base
         photoperiod_active = bound (photoperiod_active, 0.0, 24.0)
!cpsc                          need below to exert stage when phase_tt = 0
cjh         phase_tt(endjuv_to_init) = l_bound (p%pp_endjuv_to_init
cjh     :                            * photoperiod_active, 0.1)
         phase_tt(endjuv_to_init) = p%pp_endjuv_to_init
     :                            * photoperiod_active
 
      elseif (on_day_of (floral_init, g%current_stage
     :                 , g%days_tot)) then
cpsc
         leaf_no = max (c%leaf_no_rate_change, c%leaf_no_at_emerg)
cjh
         leaf_no = min (leaf_no, g%leaf_no_final)
         tt_emerg_to_flag_leaf = (leaf_no - c%leaf_no_at_emerg)
     :                         * c%leaf_app_rate1
     :                         + (g%leaf_no_final - leaf_no)
     :                         * c%leaf_app_rate2
         phase_tt(init_to_flag) = tt_emerg_to_flag_leaf
     :                          - sum_between (emerg, floral_init
     :                                       , g%tt_tot)
 
         phase_tt(flag_to_flower) = p%tt_flag_to_flower
 
         phase_tt(flower_to_start_grain) = p%tt_flower_to_start_grain
 
         phase_tt(end_grain_to_maturity) = 0.05*p%tt_flower_to_maturity
 
         phase_tt(start_to_end_grain) = p%tt_flower_to_maturity
     :                                - phase_tt(flower_to_start_grain)
     :                                - phase_tt(end_grain_to_maturity)
         phase_tt(maturity_to_ripe) = p%tt_maturity_to_ripe
 
      else
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_devel (dlt_stage, current_stage)
*     ===========================================================
      use MilletModule
      implicit none
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_stage             ! (OUTPUT) change in growth stage
      real       current_stage         ! (OUTPUT) new stage no.

*+  Purpose
*     Determine the curent stage of development.

*+  Changes
*     010994 jngh specified and programmed
*     070495 psc add l_bound to dlt-stage

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_devel')

*+  Local Variables
      real       new_stage             ! new stage number
      real       phase_devel           ! fraction of current phase elapsed ()

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
         ! mechanical operation - not to be changed
 
         ! now calculate the new delta and the new stage
 
      call millet_phase_devel (phase_devel)
      new_stage = aint (g%current_stage) + phase_devel
cjh      dlt_stage = l_bound(new_stage - g%current_stage,0.0)
      dlt_stage = new_stage - g%current_stage
 
      if (phase_devel.ge.1.0) then
         current_stage = aint (current_stage + 1.0)
         if (int(current_stage).eq.max_stage) then
            current_stage = 1.0
         else
         endif
 
      else
         current_stage = new_stage
 
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_phase_devel (phase_devel)
*     ===========================================================
      use MilletModule
      implicit none
      include 'science.pub'                       
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       phase_devel           ! (OUTPUT) fraction of current phase
                                       ! elapsed ()

*+  Purpose
*     Determine the fraction of current phase elapsed ().

*+  Changes
*     010994 jngh specified and programmed

*+  Calls
      real  millet_phase_tt
      real  millet_germination

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_phase_devel')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (stage_is_between (sowing, germ, g%current_stage)) then
         phase_devel = millet_germination (g%current_stage)
 
      elseif (stage_is_between (germ, harvest_ripe
     :                        , g%current_stage)) then
 
         phase_devel =  millet_phase_tt (g%current_stage)
 
      else
cjh         phase_devel = 0.0
         phase_devel = mod(g%current_stage, 1.0)
 
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      real function millet_germination (current_stage)
*     ===========================================================
      use MilletModule
      implicit none
      include 'data.pub'                          
      include 'science.pub'                       
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       current_stage         ! (OUTPUT) phenological stage number

*+  Purpose
*      Determine germination based on soil water availability

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_germination')

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
 
      if (stage_is_between (sowing, germ, current_stage)) then
 
         layer_no_seed = find_layer_no (g%sowing_depth, g%dlayer
     :                                , max_layer)
         pesw_seed = divide (g%sw_dep(layer_no_seed)
     :                     - p%ll_dep(layer_no_seed)
     :                     , g%dlayer(layer_no_seed), 0.0)
 
            ! can't germinate on same day as sowing, because miss out on
            ! day of sowing else_where
 
         if (pesw_seed.gt.c%pesw_germ
     :   .and.
     :   .not. on_day_of (sowing, g%current_stage, g%days_tot)) then
               ! we have germination
               ! set the current stage so it is on the point of germination
            millet_germination = 1.0 + mod (g%current_stage, 1.0)
 
         else
                ! no germination yet but indicate that we are on the way.
            millet_germination = 0.999
         endif
      else
             ! no sowing yet
         millet_germination = 0.0
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      real function millet_phase_tt (stage_no)
*     ===========================================================
      use MilletModule
      implicit none
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       stage_no              ! (INPUT) stage number

*+  Purpose
*       Return fraction of thermal time we are through the current
*       phenological phase (0-1)

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_phase_tt')

*+  Local Variables
      integer    phase                 ! phase number containing stage

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      phase = int (stage_no)
cjh  changed 0.0 to 1.0
      millet_phase_tt = divide (g%tt_tot(phase) + g%dlt_tt
     :                       , g%phase_tt(phase), 1.0)
      call pop_routine (my_name)
      return
      end








*     ===========================================================
      subroutine millet_leaf_death_o (dlt_leaf_no_dead)
*     ===========================================================
      use MilletModule
      implicit none
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_leaf_no_dead      ! (OUTPUT) new fraction of oldest
                                       ! green leaf

*+  Purpose
*       Return the fractional death of oldest green leaf.

*+  Changes
*     010994 jngh specified and programmed

*+  Calls

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_leaf_death_o')

*+  Local Variables
      real       leaf_no_dead_today    ! total number of dead leaves today
      real       leaf_no_dead_yesterday ! total number of dead leaves
                                        ! yesterday
      real       leaf_no_dead_now      !
*
cejvo
      real       temp                  ! temporary variable to calculate
                                       ! slope 2
      real       ttsum                 ! temperature sum

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
cpsc need to develop leaf senescence functions for crop
 
      leaf_no_dead_yesterday = sum_between (emerg, now, g%leaf_no_dead)
 
cejvo made it absolute leafnumber and added second slope
 
      if ((stage_is_between (emerg, harvest_ripe, g%current_stage))
     : .and. (g%leaf_no_effective .lt. g%leaf_no_final)) then
 
cgol added upper and lower bounds
!         leaf_no_dead_today = amax1(leaf_no_dead_yesterday,
!     :    amin1((c%leaf_no_dead_const + c%leaf_no_dead_slope1
!     :          * sum_between (emerg, now, g%tt_tot))
!     :          , g%leaf_no_final))
cgd
      ttsum= sum_between (emerg, now, g%tt_tot)
      leaf_no_dead_now = c%leaf_no_dead_const + c%leaf_no_dead_slope1
     :                   * ttsum
      leaf_no_dead_now = u_bound (leaf_no_dead_now,g%leaf_no_final)
      leaf_no_dead_today = l_bound (leaf_no_dead_yesterday
     :                             , leaf_no_dead_now)
 
      g%lf_no_dead_at_flaglf = leaf_no_dead_today
!      ttsum= sum_between (emerg, now, g%tt_tot)
 
      elseif ((stage_is_between (emerg, harvest_ripe, g%current_stage))
     :   .and. (g%leaf_no_effective .ge. g%leaf_no_final) .and.
     :   (g%leaf_no_dead_const2 .eq. 0.0)) then
 
!         leaf_no_dead_today = amax1(leaf_no_dead_yesterday,
!     :    amin1((c%leaf_no_dead_const + c%leaf_no_dead_slope1
!     :          * sum_between (emerg, now, g%tt_tot))
!     :          , g%leaf_no_final))
 
      ttsum= sum_between (emerg, now, g%tt_tot)
      leaf_no_dead_now = c%leaf_no_dead_const + c%leaf_no_dead_slope1
     :                   * ttsum
      leaf_no_dead_now = u_bound (leaf_no_dead_now,g%leaf_no_final)
      leaf_no_dead_today = l_bound (leaf_no_dead_yesterday
     :                             , leaf_no_dead_now)
 
      temp = g%lf_no_dead_at_flaglf - (c%leaf_no_dead_slope2
     :         * sum_between (emerg, now, g%tt_tot))
      g%leaf_no_dead_const2 = temp
!      ttsum= sum_between (emerg, now, g%tt_tot)
 
      elseif((stage_is_between (emerg, harvest_ripe, g%current_stage))
     :   .and. (g%leaf_no_effective .ge. g%leaf_no_final) .and.
     :   (g%leaf_no_dead_const2 .ne. 0.0)) then
 
!         leaf_no_dead_today = amax1(leaf_no_dead_yesterday,
!     :    amin1((g%leaf_no_dead_const2 + c%leaf_no_dead_slope2
!     :          * sum_between (emerg, now, g%tt_tot))
!     :          , g%leaf_no_final))
 
      ttsum= sum_between (emerg, now, g%tt_tot)
      leaf_no_dead_now = g%leaf_no_dead_const2 + c%leaf_no_dead_slope2
     :                   * ttsum
      leaf_no_dead_now = u_bound (leaf_no_dead_now,g%leaf_no_final)
      leaf_no_dead_today = l_bound (leaf_no_dead_yesterday
     :                             , leaf_no_dead_now)
 
      elseif (on_day_of (harvest_ripe
     :                 , g%current_stage, g%days_tot)) then
         leaf_no_dead_today = g%leaf_no_final
 
      else
      leaf_no_dead_today = 0.0
 
      endif
 
cgol removed bound check
!      leaf_no_dead_today = bound (leaf_no_dead_today
!     :                           , leaf_no_dead_yesterday
!     :                           , g%leaf_no_final)
!
cgol added lower bound of zero
!      dlt_leaf_no_dead = amax1(0.0, leaf_no_dead_today -
!     : leaf_no_dead_yesterday)
      dlt_leaf_no_dead = l_bound (0.0, leaf_no_dead_today -
     : leaf_no_dead_yesterday)
 
cccc
!     write (*,*) 'fln',g%leaf_no_final,'slno',leaf_no_dead_today
!     write (*,*) 'efln',g%leaf_no_effective
!     write (*,*) 'slnfl',g%lf_no_dead_at_flaglf
!     write (*,*) 'temp',temp,'ttsum',ttsum
!     write (*,*) 'int2',g%leaf_no_dead_const2
cccc
 
 
      call pop_routine (my_name)
      return
      end









*     ===========================================================
      subroutine millet_N_senescence (dlt_N_senesced)
*     ===========================================================
      use MilletModule
      implicit none
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_N_senesced(*)     ! (OUTPUT) actual nitrogen senesced
                                       ! from plant parts (g/m^2)

*+  Purpose
*       Derives seneseced plant nitrogen (g N/m^2)

*+  Changes
*       091294 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'millet_N_senescence')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
         ! first we zero all plant component deltas
 
      call fill_real_array (dlt_N_senesced, 0.0, max_part)
 
      dlt_N_senesced(leaf) = g%dlt_dm_senesced(leaf)
     :                     * c%N_leaf_sen_conc
      dlt_N_senesced(root) = g%dlt_dm_senesced(root)
     :                     * c%N_root_sen_conc
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_update ()
*     ===========================================================
      use MilletModule
      implicit none
      include   'convert.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Purpose
*       Update states

*+  Changes
*      250894 jngh specified and programmed

*+  Calls
!x      real  millet_swdef

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_update')

*+  Local Variables
      real       dlt_dm_plant          ! dry matter increase (g/plant)
      real       dlt_leaf_area         ! leaf area increase (mm^2/plant)
      real       dlt_dm_green_dead     ! dry matter of green plant part dying
                                       ! (g/m^2)
      real       dlt_dm_senesced_dead  ! dry matter of senesced plant part
                                       ! dying (g/m^2)
      real       dlt_N_green_dead      ! N content of green plant part dying
                                       ! (g/m^2)
      real       dlt_N_senesced_dead   ! N content of senesced plant part
                                       ! dying (g/m^2)
      real       dlt_grain_no_lost     ! grain no lost from barrenness
                                       ! (grains/m^2)
      real       dlt_lai_dead          ! lai of green leaf of plants dying ()
      real       dlt_slai_dead         ! lai of senesced leaf of plant dying ()
!cjh      real       dm_residue            ! dry matter added to residue (g/m^2)
      real       dying_fract           ! fraction op population dying (0-1)
!cjh      real       extinct_coef          ! extinction coef of green leaves
!cjh      real       extinct_coef_dead     ! extinction coef of dead leaves
!cjh      real       N_residue             ! nitrogen added to residue (g/m^2)
      real       leaf_no               ! currently expanding leaf no.
      integer    part                  ! plant part index
*
! gd
!      character  module_name*8         ! module name

*- Implementation Section ----------------------------------
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
 
      call subtract_real_array (g%dlt_N_dead_detached, g%N_dead
     :                        , max_part)
 
      call add_real_array (g%dlt_N_green, g%N_green, max_part)
      call add_real_array (g%dlt_N_retrans, g%N_green, max_part)
      call subtract_real_array (g%dlt_N_senesced, g%N_green
     :                        , max_part)
      g%N_green(tiller) = g%N_green(tiller) - g%N_tiller_independence
 
      call add_real_array (g%dlt_N_senesced, g%N_senesced
     :                   , max_part)
      call subtract_real_array (g%dlt_N_detached, g%N_senesced
     :                        , max_part)
 
      dying_fract = divide (-g%dlt_plants, g%plants, 0.0)
      dying_fract = bound (dying_fract, 0.0, 1.0)
 
      do 1000 part = 1, max_part
         dlt_N_green_dead = g%N_green(part) * dying_fract
         g%N_green(part) = g%N_green(part) - dlt_N_green_dead
         g%N_dead(part) = g%N_dead(part) + dlt_N_green_dead
 
         dlt_N_senesced_dead = g%N_senesced(part) * dying_fract
         g%N_senesced(part) = g%N_senesced(part) - dlt_N_senesced_dead
         g%N_dead(part) = g%N_dead(part) + dlt_N_senesced_dead
1000  continue
 
         ! Transfer plant dry matter
 
      dlt_dm_plant = divide (g%dlt_dm, g%plants, 0.0)
 
      call accumulate (dlt_dm_plant, g%dm_plant_top_tot
     :               , g%previous_stage, g%dlt_stage)
 
      call subtract_real_array (g%dlt_dm_dead_detached, g%dm_dead
     :                        , max_part)
 
      call add_real_array (g%dlt_dm_green, g%dm_green, max_part)
      call add_real_array (g%dlt_dm_green_retrans, g%dm_green, max_part)
      call subtract_real_array (g%dlt_dm_senesced, g%dm_green
     :                        , max_part)
      call subtract_real_array (g%dlt_dm_sen_retrans, g%dm_senesced
     :                        , max_part)
      g%dm_green(tiller) = g%dm_green(tiller) - g%dm_tiller_independence
 
      call add_real_array (g%dlt_dm_senesced, g%dm_senesced
     :                   , max_part)
      call subtract_real_array (g%dlt_dm_detached, g%dm_senesced
     :                        , max_part)
 
      do 2000 part = 1, max_part
         dlt_dm_green_dead = g%dm_green(part) * dying_fract
         g%dm_green(part) = g%dm_green(part) - dlt_dm_green_dead
         g%dm_dead(part) = g%dm_dead(part) + dlt_dm_green_dead
 
         dlt_dm_senesced_dead = g%dm_senesced(part) * dying_fract
         g%dm_senesced(part) = g%dm_senesced(part)
     :                       - dlt_dm_senesced_dead
         g%dm_dead(part) = g%dm_dead(part) + dlt_dm_senesced_dead
2000  continue
 
         ! initiate new tiller and transfer any DM and N content.
 
      call millet_tiller_initiate (g%dm_tiller_independence
     :                          , g%N_tiller_independence)
 
         ! dispose of detached material from senesced parts in
         ! live population
 
 !cjh     dm_residue = (sum_real_array (g%dlt_dm_detached, max_part)
 !cjh    :           - g%dlt_dm_detached(root))
 !cjh     N_residue = (sum_real_array (g%dlt_N_detached, max_part)
 !cjh    :          - g%dlt_N_detached(root))
 
!cjh      call millet_top_residue (dm_residue, N_residue)
 
         ! put roots into root residue
 
!cjh      call millet_root_incorp (g%dlt_dm_detached(root)
!cjh     :                    , g%dlt_N_detached(root))
 
         ! now dispose of dead population detachments
 
!cjh      dm_residue = (sum_real_array (g%dlt_dm_dead_detached, max_part)
!cjh     :           - g%dlt_dm_dead_detached(root))
!cjh      N_residue = (sum_real_array (g%dlt_N_dead_detached, max_part)
!cjh     :          - g%dlt_N_dead_detached(root))
 
!cjh      call millet_top_residue (dm_residue, N_residue)
 
         ! put roots into root residue
 
!cjh      call millet_root_incorp (g%dlt_dm_dead_detached(root)
!cjh     :                      , g%dlt_N_dead_detached(root))
 
         ! tiller development
      call accumulate (g%dlt_tiller_no, g%tiller_no
     :               , g%previous_stage, g%dlt_stage)
 
!         call get_current_module (module_name)
!
! gd
!      write (*,*) 'module name', module_name
!      write (*,*) 'dlt_tiller_no = ',g%dlt_tiller_no
!      write (*,*) 'tiller_no = ', g%tiller_no
!      write (*,*) 'previous_stage = ', g%previous_stage
!      write (*,*) 'dlt_stage = ', g%dlt_stage
cjh
         ! transfer plant grain no.
      dlt_grain_no_lost  = g%grain_no * dying_fract
      g%grain_no = g%grain_no - dlt_grain_no_lost
 
         ! transfer plant leaf area
 
      g%lai = g%lai + g%dlt_lai - g%dlt_slai
      g%slai = g%slai + g%dlt_slai - g%dlt_slai_detached
 
      dlt_lai_dead  = g%lai  * dying_fract
      dlt_slai_dead = g%slai * dying_fract
 
      g%lai = g%lai - dlt_lai_dead
      g%slai = g%slai - dlt_slai_dead
      g%tlai_dead = g%tlai_dead + dlt_lai_dead + dlt_slai_dead
     :            - g%dlt_tlai_dead_detached
 
         ! now update new canopy covers
! ejvo
!cjh      extinct_coef = linear_interp_real (g%row_spacing
!cjh     :                        , c%x_row_spacing, c%y_extinct_coef
!cjh     :                        , c%num_row_spacing)
 
!cjh      extinct_coef_dead = linear_interp_real (g%row_spacing
!cjh     :                        , c%x_row_spacing, c%y_extinct_coef_dead
!cjh     :                        , c%num_row_spacing)
 
!      call millet_cover (g%cover_green, extinct_coef, g%lai)
!      call millet_cover (g%cover_sen, extinct_coef_dead, g%slai)
!      call millet_cover (g%cover_dead, extinct_coef_dead
!     :                 , g%tlai_dead)
       call millet_cover1 (
     :                                 g%row_spacing
     :                                ,c%x_row_spacing
     :                                ,c%y_extinct_coef
     :                                ,c%num_row_spacing
     :                                ,g%lai
     :                                ,g%cover_green
     :               )
      call millet_cover1 (
     :                                 g%row_spacing
     :                                ,c%x_row_spacing
     :                                ,c%y_extinct_coef_dead
     :                                ,c%num_row_spacing
     :                                ,g%slai
     :                                ,g%cover_sen
     :               )
      call millet_cover1 (
     :                                 g%row_spacing
     :                                ,c%x_row_spacing
     :                                ,c%y_extinct_coef_dead
     :                                ,c%num_row_spacing
     :                                ,g%tlai_dead
     :                                ,g%cover_dead
     :               )
 
 
         ! plant leaf development
         ! need to account for truncation of partially developed leaf (add 1)
      leaf_no = 1.0 + sum_between (emerg, now, g%leaf_no)
      dlt_leaf_area = divide (g%dlt_lai, g%plants, 0.0) * sm2smm
      call accumulate (dlt_leaf_area, g%leaf_area
     :               , leaf_no, g%dlt_leaf_no)
 
      call accumulate (g%dlt_leaf_no, g%leaf_no
     :               , g%previous_stage, g%dlt_stage)
 
      call accumulate (g%dlt_leaf_no_dead, g%leaf_no_dead
     :               , g%previous_stage, g%dlt_stage)
 
         ! plant stress
 
      call accumulate (g%dlt_heat_stress_tt, g%heat_stress_tt
     :               , g%previous_stage, g%dlt_stage)
 
      call accumulate (g%dlt_dm_stress_max, g%dm_stress_max
     :               , g%current_stage, g%dlt_stage)
 
      call accumulate (1.0 - g%swdef_photo, g%cswd_photo
     :               , g%previous_stage, g%dlt_stage)
      call accumulate (1.0 - g%swdef_expansion, g%cswd_expansion
     :               , g%previous_stage, g%dlt_stage)
      call accumulate (1.0 - g%swdef_pheno, g%cswd_pheno
     :               , g%previous_stage, g%dlt_stage)
 
      call accumulate (1.0 - g%nfact_photo, g%cnd_photo
     :               , g%previous_stage, g%dlt_stage)
      call accumulate (1.0 - g%nfact_grain_conc, g%cnd_grain_conc
     :               , g%previous_stage, g%dlt_stage)
 
         ! other plant states
 
      g%canopy_height = g%canopy_height + g%dlt_canopy_height
      g%plants = g%plants + g%dlt_plants
      g%root_depth = g%root_depth + g%dlt_root_depth
 
      ! Phosphorus
      ! ----------
      g%plant_p = g%plant_p + g%dlt_plant_p

      call Millet_N_conc_limits (
     :          g%current_stage
     :        , c%N_conc_crit_grain
     :        , c%N_conc_max_grain
     :        , c%N_conc_min_grain
     :        , c%N_conc_crit_root
     :        , c%N_conc_max_root
     :        , c%N_conc_min_root
     :        , c%x_stage_code
     :        , c%stage_code_list
     :        , g%tt_tot
     :        , g%phase_tt
     :        , c%y_N_conc_crit_stem
     :        , c%y_N_conc_crit_leaf
     :        , c%y_N_conc_crit_flower
     :        , c%y_N_conc_min_stem
     :        , c%y_N_conc_min_leaf
     :        , c%y_N_conc_min_flower
     :        , c%y_N_conc_max_stem
     :        , c%y_N_conc_max_leaf
     :        , c%y_N_conc_max_flower
     :        , g%N_conc_crit
     :        , g%N_conc_max
     :        , g%N_conc_min)  ! plant N concentr
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_check_bounds ()
*     ===========================================================
      use MilletModule
      implicit none
      include 'data.pub'                          
      include 'error.pub'                         

*+  Purpose
*         Check bounds of internal pools

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_check_bounds')

*+  Local Variables
     :                                 ! top (g/m^2)

*- Implementation Section ----------------------------------
 
 
      call push_routine (my_name)
 
      call bound_check_real_var
     :           (sum_real_array (g%leaf_no, max_stage)
     :          , 0.0
     :          , real (max_leaf)
     :          , 'leaf_no')
 
      call bound_check_real_var
     :           (sum_real_array (g%leaf_no_dead, max_stage)
     :          , 0.0
     :          , real (max_leaf)
     :          , 'leaf_no_dead')
 
      call bound_check_real_var
     :           (g%root_depth
     :          , 0.0
     :          , sum_real_array (g%dlayer, max_layer)
     :          , 'root_depth')
 
      call bound_check_real_var
     :           (g%grain_no
     :          , 0.0
     :          , p%head_grain_no_max * g%plants
     :          , 'grain_no')
 
      call bound_check_real_var
     :           (g%current_stage
     :          , 0.0
     :          , real (max_stage)
     :          , 'current_stage')
 
      call bound_check_real_var
     :           (sum_real_array (g%phase_tt, max_stage)
     :          , 0.0
     :          , 1000000.0
     :          , 'phase_tt')
 
      call bound_check_real_var
     :           (sum_real_array (g%days_tot, max_stage)
     :          , 0.0
     :          , 40000.0
     :          , 'days_tot')
 
      call bound_check_real_var
     :           (sum_real_array (g%tt_tot, max_stage)
     :          , 0.0
     :          , 40000.0
     :          , 'tt_tot')
 
      call bound_check_real_var
     :           (g%plants
     :          , 0.0
     :          , 10000.0
     :          , 'plants')
 
!      call bound_check_real_var
!     :           (g%canopy_height
!     :          , 0.0
!     :          , c%height_max
!     :          , 'canopy_height')
 
      call bound_check_real_var
     :           (g%lai
     :          , 0.0
     :          , 30.0 - g%slai - g%tlai_dead
     :          , 'lai')
 
      call bound_check_real_var
     :           (g%slai
     :          , 0.0
     :          , 30.0 - g%lai - g%tlai_dead
     :          , 'slai')
 
      call bound_check_real_var
     :           (g%tlai_dead
     :          , 0.0
     :          , 30.0 - g%slai - g%lai
     :          , 'tlai_dead')
 
      call bound_check_real_var
     :           (g%cover_green
     :          , 0.0
     :          , 1.0
     :          , 'cover_green')
 
      call bound_check_real_var
     :           (g%cover_sen
     :          , 0.0
     :          , 1.0
     :          , 'cover_sen')
 
      call bound_check_real_var
     :           (g%cover_dead
     :          , 0.0
     :          , 1.0
     :          , 'cover_dead')
 
      call bound_check_real_var
     :           (sum_real_array (g%leaf_area, max_leaf)
     :          , 0.0
     :          , 10000000.0
     :          , 'leaf_area')
 
      call bound_check_real_var
     :           (sum_real_array (g%heat_stress_tt, max_stage)
     :          , 0.0
     :          , 1000000.0
     :          , 'heat_stress_tt')
      call bound_check_real_var
     :           (sum_real_array (g%dm_stress_max, max_stage)
     :          , 0.0
     :          , 1000000.0
     :          , 'dm_stress_max')
 
      call bound_check_real_var
     :           (sum_real_array (g%N_conc_crit, max_part)
     :          , sum_real_array (g%N_conc_min, max_part)
     :          , sum_real_array (g%N_conc_max, max_part)
     :          , 'N_conc_crit')
 
      call bound_check_real_var
     :           (sum_real_array (g%N_conc_max, max_part)
     :          , sum_real_array (g%N_conc_crit, max_part)
     :          , 1.0
     :          , 'N_conc_max')
 
      call bound_check_real_var
     :           (sum_real_array (g%N_conc_min, max_part)
     :          , 0.0
     :          , sum_real_array (g%N_conc_crit, max_part)
     :          , 'N_conc_min')
 
      call bound_check_real_var
     :           (sum_real_array (g%N_dead, max_part)
     :          , 0.0
     :          , 10000.0 - sum_real_array (g%N_green, max_part)
     :                    - sum_real_array (g%N_senesced, max_part)
     :          , 'N_dead')
 
      call bound_check_real_var
     :           (sum_real_array (g%N_green, max_part)
     :          , 0.0
     :          , 10000.0 - sum_real_array (g%N_dead, max_part)
     :                    - sum_real_array (g%N_senesced, max_part)
     :          , 'N_green')
 
      call bound_check_real_var
     :           (sum_real_array (g%N_senesced, max_part)
     :          , 0.0
     :          , 10000.0 - sum_real_array (g%N_green, max_part)
     :                    - sum_real_array (g%N_dead, max_part)
     :          , 'N_senesced')
 
      call bound_check_real_var
     :           (sum_real_array (g%dm_dead, max_part)
     :          , 0.0
     :          , 10000.0 - sum_real_array (g%dm_green, max_part)
     :                    - sum_real_array (g%dm_senesced, max_part)
     :          , 'dm_dead')
 
      call bound_check_real_var
     :           (sum_real_array (g%dm_green, max_part)
     :          , 0.0
     :          , 10000.0 - sum_real_array (g%dm_dead, max_part)
     :                    - sum_real_array (g%dm_senesced, max_part)
     :          , 'dm_green')
 
      call bound_check_real_var
     :           (sum_real_array (g%dm_senesced, max_part)
     :          , 0.0
     :          , 10000.0 - sum_real_array (g%dm_green, max_part)
     :                    - sum_real_array (g%dm_dead, max_part)
     :          , 'dm_senesced')
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_totals ()
*     ===========================================================
      use MilletModule
      implicit none
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Purpose
*         Collect totals of crop variables for output

*+  Changes
*     010994 jngh specified and programmed

*+  Calls

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_totals')

*+  Local Variables
      real       N_conc_stover         ! tops actual N concentration
                                       ! (g N/g part)
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      real       N_conc_stover_crit    ! tops critical N concentration
                                       ! (g N/g part)
      real       N_green_demand        ! plant N demand (g/m^2)
      real       N_uptake              ! nitrogen uptake from soil (g/m^2)
      real       N_uptake_stover       ! nitrogen uptake from soil by veg.
                                       ! top (g/m^2)
cpsc add below
      real       N_grain               ! total grain N uptake
      real       N_dead                ! above ground dead plant N
      real       N_green               ! above ground green plant N
      real       N_senesced            ! above ground senesced plant N
      real       N_stover              ! nitrogen content of stover

*- Implementation Section ----------------------------------
 
 
      call push_routine (my_name)
 
             ! get totals
      N_conc_stover = divide ((g%N_green(leaf)
     :                       + g%N_green(stem)
     :                       + g%N_green(flower))
 
     :                      , (g%dm_green(leaf)
     :                       + g%dm_green(stem)
     :                       + g%dm_green(flower))
     :                       , 0.0)
 
      N_uptake = sum_real_array (g%dlt_N_retrans, max_part)
      N_uptake_stover =  g%dlt_N_retrans(leaf) + g%dlt_N_retrans(stem)
 
          ! note - g%N_conc_crit should be done before the stages change
 
      N_conc_stover_crit = (g%N_conc_crit(leaf) + g%N_conc_crit(stem))
     :                   * 0.5
      N_green_demand = sum_real_array (g%N_demand, max_part)
 
      deepest_layer = find_layer_no (g%root_depth, g%dlayer, max_layer)
 
      if (on_day_of (sowing, g%current_stage, g%days_tot)) then
         g%N_uptake_tot = N_uptake
         g%transpiration_tot =
     :           - sum_real_array (g%dlt_sw_dep, deepest_layer)
         g%N_conc_act_stover_tot = N_conc_stover
         g%N_conc_crit_stover_tot = N_conc_stover_crit
         g%N_demand_tot = N_green_demand
         g%N_uptake_stover_tot = N_uptake_stover
         g%N_uptake_grain_tot = sum_real_array (g%dlt_N_retrans
     :                                        , max_part)
 
      else
         g%N_uptake_tot = g%N_uptake_tot + N_uptake
         g%transpiration_tot = g%transpiration_tot
     :                       + (-sum_real_array (g%dlt_sw_dep
     :                                         , deepest_layer))
         g%N_conc_act_stover_tot = N_conc_stover
         g%N_conc_crit_stover_tot = N_conc_stover_crit
         g%N_demand_tot = g%N_demand_tot + N_green_demand
         g%N_uptake_stover_tot = g%N_uptake_stover_tot
     :                         + N_uptake_stover
         g%N_uptake_grain_tot = g%N_uptake_grain_tot
     :                        + sum_real_array (g%dlt_N_retrans
     :                                        , max_part)
 
      endif
 
      g%lai_max = max (g%lai_max, g%lai)
      if (on_day_of (flowering, g%current_stage, g%days_tot)) then
         g%isdate = g%day_of_year
      else if (on_day_of (maturity, g%current_stage, g%days_tot)) then
         g%mdate = g%day_of_year
      else
      endif
 
cpsc add below 07/04/95
 
      N_grain = (g%N_green(grain) + g%N_dead(grain))
 
      N_green = (sum_real_array (g%N_green, max_part)
     :        - g%N_green(root) - g%N_green(grain))
 
      N_senesced = (sum_real_array (g%N_senesced, max_part)
     :           - g%N_senesced(root) - g%N_senesced(grain))
 
      N_dead = (sum_real_array (g%N_dead, max_part)
     :       - g%N_dead(root) - g%N_dead(grain))
 
      N_stover = N_green + N_senesced + N_dead
 
      g%N_uptake_grain_tot = N_grain
      g%N_uptake_stover_tot = N_stover
      g%N_uptake_tot = N_grain + N_stover
 
cpsc  add above
 
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_event ()
*     ===========================================================
      use MilletModule
      implicit none
      include 'const.inc'
      include   'convert.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Purpose
*       Report occurence of event and the current status of specific
*       variables.

*+  Changes
*     010994 jngh specified and programmed

*+  Calls

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_event')

*+  Local Variables
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

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      stage_no = int (g%current_stage)
 
      if (on_day_of (stage_no, g%current_stage, g%days_tot)) then
             ! new phase has begun.
         write (string, '(a, f6.1, 1x, a)')
     :                   ' stage '
     :                  , c%stage_code_list(stage_no)
     :                  , c%stage_names(stage_no)
         call Write_string (string)
 
         biomass = sum_real_array (g%dm_green, max_part)
     :           - g%dm_green(root)
 
     :           + sum_real_array (g%dm_senesced, max_part)
     :           - g%dm_senesced(root)
 
     :           + sum_real_array (g%dm_dead, max_part)
     :           - g%dm_dead(root)
 
         dm_green = sum_real_array (g%dm_green, max_part)
     :            - g%dm_green(root)
         N_green = sum_real_array (g%N_green, max_part)
     :           - g%N_green(root)
 
         N_green_conc_percent = divide (N_green, dm_green, 0.0)
     :                        * fract2pcnt
 
         deepest_layer = find_layer_no (g%root_depth, g%dlayer
     :                                , max_layer)
         do 1000 layer = 1, deepest_layer
            pesw(layer) = g%sw_dep(layer) - p%ll_dep(layer)
            pesw(layer) = l_bound (pesw(layer), 0.0)
1000     continue
         pesw_tot = sum_real_array (pesw, deepest_layer)
 
         if (stage_is_between (emerg, plant_end, g%current_stage)) then
            write (string, '(2(a, g16.7e2), a, 2(a, g16.7e2))')
     :              '                     biomass =       '
     :            , biomass
     :            , '   lai = '
     :            , g%lai
     :            , new_line
     :            ,'                     stover N conc ='
     :            , N_green_conc_percent
     :            , '   extractable sw ='
     :            , pesw_tot
            call write_string (string)
         else
         endif
 
      else
      endif
 
      call pop_routine (my_name)
      return
      end




*     ===========================================================
      subroutine millet_root_distrib (root_array, root_sum)
*     ===========================================================
      use MilletModule
      implicit none
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       root_array(*)         ! (OUTPUT) array to contain
                                       ! distributed material
      real       root_sum              ! (INPUT) Material to be distributed

*+  Purpose
*       Distribute root material over profile

*+  Changes
*       290994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'millet_root_distrib')

*+  Local Variables
      real       cum_depth             ! cumulative depth (mm)
      integer    layer                 ! layer number ()
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      real       root_distrb(max_layer) ! root distribution ()
      real       root_distrb_sum       ! sum of root distribution array

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
             ! distribute roots over profile to root_depth
 
      call fill_real_array (root_array, 0.0, max_layer)
      call fill_real_array (root_distrb, 0.0, max_layer)
 
      deepest_layer = find_layer_no (g%root_depth, g%dlayer
     :                                , max_layer)
      cum_depth = 0.0
      do 1000 layer = 1, deepest_layer
         cum_depth = cum_depth + g%dlayer(layer)
         cum_depth = u_bound (cum_depth, g%root_depth)
         root_distrb(layer) = exp (-c%root_extinction
     :                      * divide (cum_depth, g%root_depth, 0.0))
1000  continue
 
      root_distrb_sum = sum_real_array (root_distrb, deepest_layer)
      do 2000 layer = 1, deepest_layer
         root_array(layer) = root_sum * divide (root_distrb(layer)
     :                                        , root_distrb_sum, 0.0)
 
2000  continue
 
      call pop_routine (my_name)
      return
      end




*     ===========================================================
      subroutine millet_root_incorp (dlt_dm_root, dlt_N_root)
*     ===========================================================
      use MilletModule
      implicit none
      include 'const.inc'
      include 'action.inc'
      include   'convert.inc'
      include 'science.pub'                       
      include 'intrface.pub'                      
      include 'error.pub'                         
      include 'postbox.pub'

*+  Sub-Program Arguments
      real       dlt_dm_root           ! (INPUT) new root residue dm (g/m^2)
      real       dlt_N_root            ! (INPUT) new root residue N (g/m^2)

*+  Purpose
*       Add root residue to root residue pool

*+  Changes
*       220794 jngh specified and programmed
*       170895 jngh changed message send to message pass to module
*       220696 jngh changed to post_ construct

*+  Calls

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'millet_root_incorp')

*+  Local Variables
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      real       dlt_dm_incorp(max_layer) ! root residue (kg/ha)
      real       dlt_N_incorp(max_layer)  ! root residue N (kg/ha)
*
cjh      integer    layer                 ! layer number
cjh      character  string*(ACTION_data_size) ! output string

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (dlt_dm_root.gt.0.0) then
 
            ! send out root residue
 
         call millet_root_distrib (dlt_dm_incorp
     :                          , dlt_dm_root * gm2kg /sm2ha)
         call millet_root_distrib (dlt_N_incorp
     :                          , dlt_N_root * gm2kg /sm2ha)
 
         deepest_layer = find_layer_no (g%root_depth, g%dlayer
     :                                , max_layer)
 
cjh         string = 'dlt_fom_type='// c%crop_type
 
cjh         write (string(len_trim(string)+1:), '(a, 20g16.7e3)' )
cjh     :              ', dlt_fom_wt = '
cjh     :               , (dlt_dm_incorp(layer), layer = 1, deepest_layer)
cjh         string =  string_concat (string, '(kg/ha)')
 
cjh         write (string(len_trim(string)+1:), '(a, 20g16.7e3)')
cjh     :              ', dlt_fom_n = '
cjh     :               , (dlt_N_incorp(layer), layer = 1, deepest_layer)
cjh         string = string_concat (string, '(kg/ha)')
 
cjh         call message_pass_to_module (all_active_modules
cjh     :                               , 'incorp_fom'
cjh     :                               , string)
 
         call New_postbox ()
 
         call post_char_var ('dlt_fom_type=','()',c%crop_type)
 
         call post_real_array ('dlt_fom_wt'
     :                        ,'(kg/ha)'
     :                        ,dlt_dm_incorp
     :                        ,deepest_layer)
 
         call post_real_array ('dlt_fom_n'
     :                        ,'(kg/ha)'
     :                        ,dlt_n_incorp
     :                        ,deepest_layer)
 
         call Action_send (
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
      subroutine millet_tillering ()
*     ===========================================================
      use MilletModule
      implicit none
      include 'const.inc'
      include 'error.pub'                         

*+  Purpose
*       Tiller routine

*+  Changes
*       091095 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'millet_tillering')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (c%tiller_no_pot.gt.0) then
 
         if (c%tiller_appearance.eq.'tt') then
            call millet_tiller_appearance_tt (g%dlt_tiller_no)
 
         elseif (c%tiller_appearance.eq.'dm') then
            call millet_tiller_appearance_dm (g%dlt_tiller_no)
 
         else
            ! no tillers simulated
            call fatal_error (err_user
     :                     , 'No tiller appearance method supplied')
         endif
         call millet_tiller_independence (g%tiller_independence
     :                                 , g%dm_tiller_independence
     :                                 , g%N_tiller_independence)
 
      else
         ! no tillers simulated
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_tiller_appearance_tt (dlt_tiller_no)
*     ===========================================================
      use MilletModule
      implicit none
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_tiller_no         ! (OUTPUT) new fraction of next
                                       ! tiller to emerge

*+  Purpose
*       Uses thermal time to return the fractional increase in
*       appearance of the next tiller to emerge.

*+  Changes
*       091095 jngh specified and programmed
*       261097 gol added global variable 'g%daylength_at_emerg' and global
*                  constant 'c%photo_tiller_crit' and new section to amend
*                  tiller initiation thermal time

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_tiller_appearance_tt')

*+  Local Variables
c gol added 'g%daylength_at_emerg (h)' and 'c%photo_tiller_crit (h)' to constants
c the thermal time for first tiller initiation (global constant 'c%y_tiller_tt')
c is increased in a one-step process
*
      real       tiller_no_remaining   ! number of tillers to go before
                                       ! potential no. is reached  ()
      real       tiller_no_now         ! number of tillers ()
      real       tiller_no_next        ! next tiller number ()
      real       tiller_app_rate       ! rate of tiller appearance (oCd/tiller)
!      real       ttsum                 ! temperature sum

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
cejvo made leaf appearance function of plant density
      g%y_tiller_tt_adj(2) = c%y_tiller_tt(2)
     :                       + c%tiller_appearance_slope
     :                       * g%plants
 
cgol added tiller initiation adjustment for daylength
 
      if (on_day_of (emerg, g%current_stage, g%days_tot)) then
 
         g%daylength_at_emerg = day_length (g%day_of_year, g%latitude
     :        ,                             c%twilight)
 
         if (g%daylength_at_emerg.gt.c%photo_tiller_crit) then
 
            g%y_tiller_tt_adj(1) = c%y_tiller_tt(1)
     :                             + g%y_tiller_tt_adj(2)
 
         else
 
             g%y_tiller_tt_adj(1) = c%y_tiller_tt(1)
 
         endif
 
      endif
 
cgol check calculations (now turned off)
!      write (*,*) g%day_of_year, g%latitude, c%twilight,
!     : g%daylength_at_emerg, c%photo_tiller_crit, g%y_tiller_tt_adj(1),
!     : c%y_tiller_tt(2)
!     : g%y_tiller_tt_adj(2)
 
      if (stage_is_between (emerg, flag_leaf, g%current_stage)) then
 
cgol bounds added to tiller number determination
!     for tiller_no_now and tiller_no_remaining to stop model initating
!     tillers beyond the potential tiller number (set previously to 5)
 
!         tiller_no_now = amin1(amax1(0.0, sum_between (emerg, now,
!     :                         g%tiller_no)), c%tiller_no_pot)
!         tiller_no_remaining = amax1(0.0, (real (c%tiller_no_pot) -
!     :                               tiller_no_now))
 
         tiller_no_now = sum_between (emerg, now, g%tiller_no)
 
         tiller_no_now = l_bound (0.0, tiller_no_now)
 
         tiller_no_now = u_bound (tiller_no_now, real(c%tiller_no_pot))
 
         tiller_no_remaining = l_bound(0.0, real(c%tiller_no_pot
     :                                       - tiller_no_now))
 
         tiller_no_next = aint (tiller_no_now) + 1.0
 
!gd
!      write (*,*) g%current_stage,
!     : tiller_no_now,tiller_no_next,tiller_no_remaining
!       write (*,*) 'ttsum',ttsum, 'yt',g%y_tiller_tt_adj
!     : c%num_tiller_no_next
!      write (*,*) c%x_tiller_no_next, c%y_tiller_tt,c%num_tiller_no_next
 
         tiller_app_rate = linear_interp_real (tiller_no_next
     :                       , c%x_tiller_no_next, g%y_tiller_tt_adj
     :                       , c%num_tiller_no_next)
!
!      write (*,*) tiller_app_rate
!
         dlt_tiller_no = divide (g%dlt_tt, tiller_app_rate, 0.0)
         dlt_tiller_no = bound (dlt_tiller_no, 0.0, tiller_no_remaining)
 
      else
             ! we have full number of leaves.
 
         dlt_tiller_no = 0.0
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_tiller_appearance_dm (dlt_tiller_no)
*     ===========================================================
      use MilletModule
      implicit none
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_tiller_no         ! (OUTPUT) new fraction of next
                                       ! tiller to emerge

*+  Purpose
*       Uses assimilate flux to determine the fractional increase in
*       appearance of the next tiller to emerge.

*+  Changes
*       091095 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_tiller_appearance_dm')

*+  Local Variables
      real       tiller_no_remaining   ! number of tillers to go before
                                       ! potential no. is reached
      real       tiller_no_now         ! number of tillers

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (stage_is_between (emerg, flag_leaf, g%current_stage)) then
 
         tiller_no_now = sum_between (emerg, now, g%tiller_no)
         tiller_no_remaining = real (c%tiller_no_pot) - tiller_no_now
         dlt_tiller_no = divide (g%dm_green(tiller)
     :                         , c%dm_tiller_crit*g%plants, 0.0)
 
         dlt_tiller_no = bound (dlt_tiller_no, 0.0, tiller_no_remaining)
 
      else
             ! we have full number of leaves.
 
         dlt_tiller_no = 0.0
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_tiller_independence (tiller_independence
     :                                    , dm_tiller_independence
     :                                    , N_tiller_independence)
*     ===========================================================
      use MilletModule
      implicit none
      include   'convert.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    tiller_independence    ! (OUTPUT) new tiller ready for
                                        ! independence ()
      real       dm_tiller_independence ! (OUTPUT) new tiller DM (g/m^2)
      real       N_tiller_independence  ! (OUTPUT) new tiller N (g/m^2)

*+  Purpose
*       Initiate millet tiller module

*+  Changes
*       101095 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'millet_tiller_independence')

*+  Local Variables
      real       dm_tiller_fract       ! fraction of DM to this tiller ()
      real       tiller_no             ! number of tillers developed ()

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      tiller_no = sum_between (emerg, now, g%tiller_no)
      if (aint (tiller_no + g%dlt_tiller_no) .gt. tiller_no) then
            ! tiller is independent
 
         tiller_independence = 1
         if (c%tiller_appearance.eq.'dm') then
            dm_tiller_independence = c%dm_tiller_crit * g%plants
            dm_tiller_fract = divide (dm_tiller_independence
     :                              , g%dm_green(tiller), 0.0)
            N_tiller_independence = g%N_green(tiller) * dm_tiller_fract
 
         else
               ! tiller not collecting DM or N
            dm_tiller_independence = 0.0
            N_tiller_independence = 0.0
 
         endif
 
      else
            ! tiller not independent yet
         tiller_independence = 0
         dm_tiller_independence = 0.0
         N_tiller_independence = 0.0
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_tiller_initiate (dm_tiller_independence
     :                                , N_tiller_independence)
*     ===========================================================
      use MilletModule
      implicit none
      include 'const.inc'
      include 'action.inc'
      include 'string.pub'                        
      include 'data.pub'                          
      include 'intrface.pub'                      
      include 'error.pub'                         
      include 'postbox.pub'

*+  Sub-Program Arguments
      real       dm_tiller_independence ! (INPUT) new tiller DM (g/m^2)
      real       N_tiller_independence  ! (INPUT) new tiller N (g/m^2)

*+  Purpose
*       Initiate millet tiller module

*+  Changes
*       101095 jngh specified and programmed
*       220696 jngh changed to post_ construct 
*       180500 jngh changed 'tiller_N' to lower case

*+  Calls
      character  no_spaces*8           ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'millet_tiller_initiate')

*+  Local Variables
cjh      character  string*200            ! output string
      character  tiller_module*20      ! tiller module name
      character  module_name*8         ! this module name
      real       dm_tiller_plant       ! dry matter of tiller (g/plant)
      real       N_tiller_plant        ! N content of tiller (g/plant)
      integer    tiller_no             ! number of tillers developed ()

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (g%tiller_independence.gt.0) then
            ! send out new tiller
 
         tiller_no = int (sum_between (emerg, now, g%tiller_no)) + 1
 
!         write (*,*) 'tiller_no(int(sum(emerg,now))) = ', tiller_no
 
         call get_current_module (module_name)
         write (tiller_module, '(a, i2)') module_name, tiller_no
 
!         write (*,*) 'tiller_module', tiller_module
 
         tiller_module = no_spaces (tiller_module)
 
!         write (*,*) 'no_spaces(tiller_module)', tiller_module
 
         if (len_trim(tiller_module).le.8) then
 
            dm_tiller_plant = divide (dm_tiller_independence
     :                              , g%plants, 0.0)
            N_tiller_plant = divide (N_tiller_independence
     :                             , g%plants, 0.0)

!cjh      print *, dm_tiller_plant, N_tiller_independence
!cjh      print *, N_tiller_plant, N_tiller_independence
 
!cjh            write(string, '(4(a, g16.7e3, a), 2a)' )
!cjh     :           'plants = '       , g%plants        , '(plants/m2)'
!cjh     :         , ',tiller_wt = '   , dm_tiller_plant , '(g/plant)'
!cjh     :         , ',tiller_N = '    , N_tiller_plant  , '(g/plant)'
!cjh     :         , ', row_spacing = ', g%row_spacing   , '(m)'
!cjh     :         , ', cultivar = '   , g%cultivar
 
!cjh            call message_pass_to_module (tiller_module
!cjh     :                                  , ACTION_initiate_crop
!cjh     :                                  , string)
 
            call New_postbox ()
 
            call post_real_var ('plants'
     :                         ,'(plants/m2)'
     :                         ,g%plants)
 
            call post_real_var ('tiller_wt'
     :                        ,'(g/plant)'
     :                        ,dm_tiller_plant)
 
            call post_real_var ('tiller_n'
     :                        ,'(g/plant)'
     :                        ,N_tiller_plant)
 
            call post_real_var ('row_spacing'
     :                        ,'(m)'
     :                        ,g%row_spacing)
 
            call post_char_var ('cultivar'
     :                        ,'()'
     :                        ,g%cultivar)
 
            call Action_send (
     :                              tiller_module
     :                            , ACTION_initiate_crop
     :                            , Blank
     :                            )
 
            call Delete_postbox ()
 
         else
            call Fatal_Error (err_internal
     :        ,              ' Tiller module name too long - '
     :                       // Tiller_module)
 
         endif
 
      else
         ! no tiller ready for independence
      endif
 
      call pop_routine (my_name)
      return
      end














*     ===========================================================
      subroutine millet_leaf_area_devel1 (
     :          g_leaf_no
     :        , leaf_no_effective
     :        , c_leaf_no_correction
     :        , c_x0_const
     :        , c_x0_slope
     :        , g_leaf_no_final
     :        , c_y0_const
     :        , c_y0_slope
     :        , c_a_const
     :        , c_a_slope1
     :        , c_a_slope2
     :        , c_b_const
     :        , c_b_slope1
     :        , c_b_slope2
     :        , g_dlt_leaf_no
     :        , g_plants
     :        , g_swdef_expansion
     :        , dlt_lai_pot)
*     ===========================================================
      use MilletModule
      implicit none
      include   'convert.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
       real g_leaf_no(*)
       real leaf_no_effective
       real c_leaf_no_correction
       real c_x0_const
       real c_x0_slope
       real g_leaf_no_final
       real c_y0_const
       real c_y0_slope
       real c_a_const
       real c_a_slope1
       real c_a_slope2
       real c_b_const
       real c_b_slope1
       real c_b_slope2
       real g_dlt_leaf_no
       real g_plants
      real g_swdef_expansion
      real       dlt_lai_pot           ! (OUTPUT) change in leaf area

*+  Purpose
*       Return the potential increase in leaf area development (mm^2)
*       calculated on an individual leaf basis.

*+  Changes
*     210397 nih/mjr specified and programmed

*+  Calls
      real       Millet_leaf_size1       ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_leaf_area_devel1')

*+  Local Variables
      real       area                  ! potential maximum area of oldest
                                       ! expanding leaf (mm^2) in today's
                                       ! conditions
!      real       leaf_no_effective     ! effective leaf no - includes
                                       ! younger leaves that have emerged
                                       ! after the current one

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
           ! once leaf no is calculated leaf area of largest expanding leaf
           ! is determined
 
!glh This should also be from sowing, as above? (changed from emerg (scc))
      leaf_no_effective = sum_between (emerg, now, g_leaf_no)
     :                  + c_leaf_no_correction
      area = Millet_leaf_size1 (
     :          c_x0_const
     :        , c_x0_slope
     :        , g_leaf_no_final
     :        , c_y0_const
     :        , c_y0_slope
     :        , c_a_const
     :        , c_a_slope1
     :        , c_a_slope2
     :        , c_b_const
     :        , c_b_slope1
     :        , c_b_slope2
     :        , leaf_no_effective)
 
      dlt_lai_pot = g_dlt_leaf_no * area * smm2sm * g_plants
      call pop_routine (my_name)
      return
      end

*     ===========================================================
      real function Millet_leaf_size1 (
     :          c_x0_const
     :        , c_x0_slope
     :        , g_leaf_no_final
     :        , c_y0_const
     :        , c_y0_slope
     :        , c_a_const
     :        , c_a_slope1
     :        , c_a_slope2
     :        , c_b_const
     :        , c_b_slope1
     :        , c_b_slope2
     :        , leaf_no)
*     ===========================================================
      implicit none
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
       real c_x0_const
       real c_x0_slope
       real g_leaf_no_final
       real c_y0_const
       real c_y0_slope
       real c_a_const
       real c_a_slope1
       real c_a_slope2
       real c_b_const
       real c_b_slope1
       real c_b_slope2
       real       leaf_no               ! (INPUT) nominated leaf number

*+  Purpose
*       Return the leaf area (mm^2) of a specified leaf no.

*+  Changes
*       210397 nih/mjr specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Millet_leaf_size1')

*+  Local Variables
      real       area                  ! potential area of nominated leaf
                                       ! no (mm^2)
      real       area_max              ! potential area of largest leaf (mm^2)
      real       breadth               ! breadth coef of leaf
      real       largest_leaf          ! leaf no of largeat leaf
      real       skewness              ! skewness coef of leaf

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
           ! Once leaf no is calculated leaf area of largest expanding leaf
           ! is determined with quadratic relationship. Coefficients for this
           ! curve are functions of total leaf no.
 
      largest_leaf = c_x0_const + (c_x0_slope * g_leaf_no_final)
      area_max     = c_y0_const + (c_y0_slope * g_leaf_no_final)
 
      breadth  = c_a_const
     :         + divide (c_a_slope1
     :                , 1.0 + c_a_slope2 * g_leaf_no_final
     :                , 0.0)
      skewness = c_b_const
     :         + divide (c_b_slope1
     :                , 1.0 + c_b_slope2 * g_leaf_no_final
     :                , 0.0)
 
      area = area_max * exp (breadth * (leaf_no - largest_leaf)**2
     :                      + skewness * (leaf_no - largest_leaf)**3)
 
      Millet_leaf_size1 = area
 
      call pop_routine (my_name)
      return
      end


*     ===========================================================
      subroutine millet_dm_partition (
     :          g_current_stage
     :        , c_ratio_root_shoot
     :        , g_dlt_dm
     :        , g_leaf_no
     :        , c_partition_rate_leaf
     :        , g_dlt_lai_stressed
     :        , c_sla_min
     :        , c_frac_stem2flower
     :        , g_dlt_dm_grain_demand
     :        , dlt_dm_green
     :        , c_tiller_no_pot
     :        , g_dlt_lai_pot
     :        , p_hi_max_pot
     :        , g_dm_green
     :        , g_dm_senesced)
*     ===========================================================
      use MilletModule
      implicit none
      include   'convert.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

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
       real       dlt_dm_green (*)      ! (OUTPUT) actual biomass partitioned
                                       ! to plant parts (g/m^2)

       integer  c_tiller_no_pot
       real     g_dlt_lai_pot
       real     p_hi_max_pot
       real     g_dm_green(*)
       real     g_dm_senesced(*)

*+  Purpose
*       Partitions new dm (assimilate) between plant components (g/m^2)

*+  Changes
*       010994 jngh specified and programmed
*       250495 psc  modified dlt_dm_green(grain) to account for barren heads

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'millet_dm_partition')

*+  Local Variables
      integer    current_phase         ! current phase no.
      real       dlt_dm_axis           ! dry weight partitioned to
                                       ! axes (excluding tillers)(g/m^2)
      real       dlt_dm_green_tot      ! total of partitioned dm (g/m^2)
      real       dlt_dm_leaf_max       ! max increase in leaf dm (g/m^2)
!cpsc
      real       dlt_dm_grain_max      ! limit to dlt grain wt (g/m^2)
      real       dm_grain_max          ! limit of grain wt based on max. HI (g/m^2)
      real       dm_tops               ! drymatter of tops (g/m^2)
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
 
      call fill_real_array (dlt_dm_green, 0.0, max_part)
 
         ! now we get the root delta for all stages - partition scheme
         ! specified in coeff file
 
      current_phase = int (g_current_stage)
      dlt_dm_green(root) = c_ratio_root_shoot(current_phase)*g_dlt_dm
 
      if (stage_is_between (emerg, floral_init, g_current_stage)) then
            ! we have leaf development only
         dlt_dm_green(leaf) = g_dlt_dm
 
         if (c_tiller_no_pot.gt.0) then
 
            dlt_dm_leaf_max = divide (g_dlt_lai_pot
     :                              , c_sla_min * smm2sm, 0.0)
            dlt_dm_green(leaf) = u_bound (dlt_dm_green(leaf)
     :                                  , dlt_dm_leaf_max)
            dlt_dm_green(tiller) = g_dlt_dm - dlt_dm_green(leaf)
 
            dlt_dm_green(tiller) = l_bound (dlt_dm_green(tiller), 0.0)
 
         else
               ! no tillering simulated
            dlt_dm_green(tiller) = 0.0
         endif
 
      elseif (stage_is_between (floral_init, flag_leaf
     :                        , g_current_stage)) then
 
            ! stem elongation and flower development start
            ! Each new leaf demands an increasing proportion of dry matter
            ! partitioned to stem and flower
 
         internode_no = sum_between (floral_init, now, g_leaf_no)
         partition_coef_leaf = 1.0
     :            /(1.0 + c_partition_rate_leaf * internode_no**2)
 
         dlt_dm_green(leaf) = partition_coef_leaf * g_dlt_dm
             ! limit the delta leaf area to maximum
         dlt_dm_leaf_max = divide (g_dlt_lai_pot
     :                           , c_sla_min * smm2sm, 0.0)
         dlt_dm_green(leaf) = u_bound (dlt_dm_green(leaf)
     :                               , dlt_dm_leaf_max)
 
         if (c_tiller_no_pot.gt.0) then
 
            dlt_dm_axis = divide (dlt_dm_green(leaf)
     :                          , partition_coef_leaf, g_dlt_dm)
 
            dlt_dm_green(tiller) = g_dlt_dm - dlt_dm_axis
 
            dlt_dm_green(tiller) = l_bound (dlt_dm_green(tiller), 0.0)
 
         else
            dlt_dm_axis = g_dlt_dm
            dlt_dm_green(tiller) = 0.0
 
         endif
 
         dlt_dm_green(flower) = (dlt_dm_axis - dlt_dm_green(leaf))
     :                        * c_frac_stem2flower
 
         dlt_dm_green(stem) = dlt_dm_axis
     :                      - (dlt_dm_green(flower)
     :                      + dlt_dm_green(leaf))
 
         dlt_dm_green(flower) = l_bound (dlt_dm_green(flower), 0.0)
         dlt_dm_green(stem) = l_bound (dlt_dm_green(stem), 0.0)
 
 
      elseif (stage_is_between (flag_leaf, start_grain_fill
     :                        , g_current_stage)) then
 
            ! we only have flower and stem growth here
         dlt_dm_green(flower) = g_dlt_dm*c_frac_stem2flower
         dlt_dm_green(stem) = g_dlt_dm - dlt_dm_green(flower)
         dlt_dm_green(stem) = l_bound (dlt_dm_green(stem), 0.0)
 
      elseif (stage_is_between (start_grain_fill, maturity
     :                        , g_current_stage)) then
 
            ! grain filling starts - stem continues when it can
 
!cpsc  bound HI to a maximum value
         dm_tops = sum_real_array (g_dm_green, max_part)
     :           - g_dm_green(root)
     :           + sum_real_array (g_dm_senesced, max_part)
     :           - g_dm_senesced(root)
 
         dm_grain_max = (dm_tops + g_dlt_dm) * p_hi_max_pot
         dlt_dm_grain_max = bound (dm_grain_max - g_dm_green(grain)
     :                           , 0.0, g_dlt_dm)
 
         dlt_dm_green(grain) = bound (g_dlt_dm_grain_demand
     :                              , 0.0, dlt_dm_grain_max)
 
!cpsc
!         dlt_dm_green(grain) = bound (g_dlt_dm_grain_demand
!     :                              , 0.0, g_dlt_dm)
 
         dlt_dm_green(stem) = g_dlt_dm - dlt_dm_green(grain)
 
         dlt_dm_green(stem) = l_bound (dlt_dm_green(stem), 0.0)
 
 
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
      end


*     ===========================================================
      subroutine millet_heat_stress (g_maxt
     :        ,               c_temp_grain_crit_stress
     :        ,               dlt_tt_heat_stress)
*     ===========================================================
      implicit none
      include 'error.pub'

*+  Sub-Program Arguments
      real       g_maxt                ! (INPUT) maximum temperature (oC)
      real       c_temp_grain_crit_stress
                                       ! (INPUT) temperature above which
                                       ! heat stress occurs
      real       dlt_tt_heat_stress    ! (OUTPUT) heat stress (oC)

*+  Purpose
*     Calculate heat stress on grain number for the current day.

*+  Changes
*     250894 jngh specified and programmed
*     970317 slw new template form

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_heat_stress')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
            ! high temperature stress reduces grain no via 'htsf'
 
      if (g_maxt.gt.c_temp_grain_crit_stress) then
         dlt_tt_heat_stress = g_maxt - c_temp_grain_crit_stress
      else
         dlt_tt_heat_stress = 0.0
      endif
 
      call pop_routine (my_name)
      return
      end

*     ===========================================================
      subroutine millet_grain_no (
     :          g_current_stage
     :        , g_days_tot
     :        , g_dm_plant_top_tot
     :        , c_growth_rate_min
     :        , c_growth_rate_crit
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
      use MilletModule
      implicit none
      include 'science.pub'                       
      include 'crp_nitn.pub'                      
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
       real g_current_stage
       real g_days_tot(*)
       real g_dm_plant_top_tot(*)
       real c_growth_rate_min
       real c_growth_rate_crit
       real p_head_grain_no_max
       real g_heat_stress_tt(*)
       real c_htstress_coeff
       real g_N_conc_min(*)
       real g_dm_green(*)
       real g_N_green(*)
       real g_plants
       real c_seed_wt_min
       real c_grain_N_conc_min
       real grain_num

*+  Purpose
*     Calculate the grains per m^2 and heads per m^2

*+  Changes
*     111094 jngh specified and programmed
*     250495 psc added head no to output
*     970317 slw new template form

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'millet_grain_no')

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
      real       effective_growth_rate ! effective rate of
                                       ! photosynthesis during flowering
                                       ! to allow grain no. development
                                       ! (g/plant).

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
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
 
!cjh added next lines to prevent negative values
         effective_growth_rate = growth_rate - c_growth_rate_min
         effective_growth_rate = l_bound (effective_growth_rate, 0.0)
         grain_no_fract = divide (effective_growth_rate
     :                          , (c_growth_rate_crit
     :                             + effective_growth_rate)
     :                          , 0.0)
 
         head_grain_no_optimum = p_head_grain_no_max * grain_no_fract
 
         call bound_check_real_var (grain_no_fract, 0.0, 1.0
     :                           , 'grain_no_fract')
 
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
 
            ! In millet_N_conc_limits_o, the min grain N conc is 0.007
 
         call crop_N_retrans_avail (max_part, root, grain
     :        , g_N_conc_min
     :        , g_dm_green
     :        , g_N_green,N_avail)
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
      end


*     ===========================================================
      subroutine millet_grain_no1 (
     :          g_current_stage
     :        , g_days_tot
     :        , g_dm_plant_top_tot
     :        , c_growth_rate_min
     :        , c_growth_rate_crit
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
      use MilletModule
      implicit none
      include 'science.pub'                       
      include 'data.pub'                          
      include 'crp_nitn.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
       real g_current_stage
       real g_days_tot(*)
       real g_dm_plant_top_tot(*)
       real c_growth_rate_min
       real c_growth_rate_crit
       real p_head_grain_no_max
       real g_heat_stress_tt(*)
       real c_htstress_coeff
       real g_N_conc_min(*)
       real g_dm_green(*)
       real g_N_green(*)
       real g_plants
       real c_seed_wt_min
       real c_grain_N_conc_min
       real grain_num

*+  Purpose
*     Calculate the grains per m^2 and heads per m^2
*     Same as millet_grain_no but with bound of grain_no_fract
*     It seems strange to bound check the value when it is possible to
*     get out of bounds values (-ve's) from sensible numbers.  If
*     growth rate is small the fraction will be -ve and it is sensible
*     to constrain this above 0.

*+  Changes
*     111094 jngh specified and programmed
*     250495 psc added head no to output
*     970317 slw new template form

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'millet_grain_no1')

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
 
         grain_no_fract = divide ((growth_rate - c_growth_rate_min)
     :                          , (c_growth_rate_crit
     :                             + (growth_rate - c_growth_rate_min))
     :                          , 0.0)
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
 
            ! In millet_N_conc_limits_o, the min grain N conc is 0.007
 
         call crop_N_retrans_avail (max_part, root, grain
     :        , g_N_conc_min
     :        , g_dm_green
     :        , g_N_green,N_avail)
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
      end


*     ===========================================================
      subroutine millet_dm_grain (
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
      use MilletModule
      implicit none
      include   'convert.inc'          ! mg2gm
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
       real g_current_stage
       real g_maxt
       real g_mint
       real c_x_temp_grain(*)
       real c_y_grain_rate(*)
       integer c_num_temp_grain
       real c_swdf_grain_min
       real g_grain_no
       real p_grain_gth_rate
       real g_N_conc_min(*)
       real g_dm_green(*)
       real g_N_green(*)
       real c_temp_fac_min
       real c_tfac_slope
       real c_sw_fac_max
       real c_sfac_slope
       real g_N_conc_crit(*)
       real g_swdef_photo
       real g_pfact_grain
       real g_swdef_expansion
       real g_nfact_grain_conc
       real dlt_dm_grain_demand

*+  Purpose
*     Find grain demand for carbohydrate (g/m^2)

*+  Changes
*     010994 jngh specified and programmed
*     970317 slw new template form

*+  Calls
      real       millet_dm_grain_max    ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_dm_grain')

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
 
         fract_of_optimum = rgfill * sw_def_fac * g_pfact_grain
 
            ! now calculate the grain growth demand for the day in g/m^2
 
         dlt_dm_grain_optm = g_grain_no * (p_grain_gth_rate * mg2gm)
         dlt_dm_grain = bound (dlt_dm_grain_optm * fract_of_optimum
     :                       , 0.0
     :        ,                millet_dm_grain_max
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
      end

*     ===========================================================
      real function millet_dm_grain_max (
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
      use MilletModule
      implicit none
      include 'data.pub'                          
      include 'crp_nitn.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
       real g_N_conc_min(*)
       real g_dm_green(*)
       real g_N_green(*)
       real g_maxt
       real g_mint
       real c_temp_fac_min
       real c_tfac_slope
       real c_sw_fac_max
       real c_sfac_slope
       real g_N_conc_crit(*)
       real g_swdef_expansion
       real g_nfact_grain_conc

*+  Purpose
*     Maximum grain growth for available nitrogen (g/m^2)

*+  Changes
*     141093 jngh specified and programmed
*     970317 slw new template form

*+  Calls

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_dm_grain_max')

*+  Local Variables
      real       N_avail(max_part)     ! nitrogen available for grain uptake
                                       ! from each part (g/m^2)
*
      real       N_avail_sum           ! total nitrogen available for grain
                                       ! uptake (g/m^2)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      call crop_N_retrans_avail (max_part, root, grain
     :        , g_N_conc_min
     :        , g_dm_green
     :        , g_N_green, N_avail)
      N_avail_sum = sum_real_array (N_avail, max_part)
 
      millet_dm_grain_max = divide (N_avail_sum
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
      end





*     ===========================================================
      subroutine Millet_water_supply (Option)
*     ===========================================================
      use MilletModule
      implicit none
      include 'const.inc'
      include 'crp_watr.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     Soil water supply

*+  Changes
*     5/9/96 dph
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Millet_water_supply')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
         call cproc_sw_supply1 (
     :                 c%minsw
     :                ,g%dlayer
     :                ,p%ll_dep
     :                ,g%dul_dep
     :                ,g%sw_dep
     :                ,g%num_layers
     :                ,g%root_depth
     :                ,p%kl
     :                ,g%sw_avail
     :                ,g%sw_avail_pot
     :                ,g%sw_supply
     :                )
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end

*     ===========================================================
      subroutine Millet_nit_stress(Option)
*     ===========================================================
      use MilletModule
      implicit none
      include   'const.inc'
      include 'crp_nitn.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*     Get current Nitrogen stress factors (0-1)

*+  Changes
*     010994 jngh specified and programmed
*     970225 slw modified to split stress factors

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Millet_nit_stress')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
         call crop_nfact_pheno(leaf, stem, g%dm_green
     :        ,                g%N_conc_crit
     :        ,                g%N_conc_min
     :        ,                g%N_green
     :        ,                c%N_fact_pheno, g%nfact_pheno)
         call crop_nfact_photo(leaf, stem
     :        ,            g%dm_green
     :        ,            g%N_conc_crit
     :        ,            g%N_conc_min
     :        ,            g%N_green
     :        ,            c%N_fact_photo, g%nfact_photo)
         call crop_nfact_grain_conc(leaf, stem
     :        ,            g%dm_green
     :        ,            g%N_conc_crit
     :        ,            g%N_conc_min
     :        ,            g%N_green, g%nfact_grain_conc)
         call crop_nfact_expansion(leaf
     :        ,            g%dm_green
     :        ,            g%N_conc_crit
     :        ,            g%N_conc_min
     :        ,            g%N_green
     :        ,            c%N_fact_expansion
     :        ,            g%nfact_expansion)
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end

*     ===========================================================
      subroutine Millet_temp_stress(Option)
*     ===========================================================
      use MilletModule
      implicit none
      include 'const.inc'
      include 'crp_temp.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option         ! (INPUT) option number

*+  Purpose
*     Get current temperature stress factors (0-1)

*+  Changes
*     010994 jngh specified and programmed
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*) ! name of procedure
      parameter (my_name = 'Millet_temp_stress')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (Option .eq. 1) then
          call crop_temperature_stress_photo
     :               (c%num_ave_temp, c%x_ave_temp, c%y_stress_photo
     :        ,       g%maxt, g%mint, g%temp_stress_photo)
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end


*     ===========================================================
      subroutine Millet_light_supply (Option)
*     ===========================================================
      use MilletModule
      implicit none
      include 'const.inc'
      include 'crp_util.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     light supply

*+  Changes
*     5/9/96 dph
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Millet_light_supply')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 1) then
       call crop_radn_int0(g%cover_green
     :        ,            g%fr_intc_radn, g%radn, g%radn_int)
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end


*     ===========================================================
      subroutine millet_bio_TE (Option)
*     ===========================================================
      use milletModule
      implicit none
      include 'const.inc'
      include 'crp_watr.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*      bio water

*+  Changes
*     5/9/96 dph
*     970317 slw new template form

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_bio_TE')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
         call cproc_bio_water1(
     :           max_layer
     :         , g%dlayer
     :         , g%root_depth
     :         , g%sw_supply
     :         , g%transp_eff
     :         , g%dlt_dm_water
     :         )
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end

*     ===========================================================
      subroutine Millet_bio_RUE (Option)
*     ===========================================================
      use MilletModule
      implicit none
      include 'const.inc'
      include 'crp_biom.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     biomass light

*+  Changes
*     5/9/96 dph
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Millet_bio_RUE')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 1) then
         ! potential by photosynthesis
 
         call crop_dm_pot_rue(
     :          g%current_stage
     :        , c%rue
     :        , g%radn_int
     :        , g%temp_stress_photo
     :        , min(g%nfact_photo,g%pfact_photo)
!cjh     :        , g%nfact_photo
     :        , g%dlt_dm_light)
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end


*     ===========================================================
      subroutine Millet_transpiration_eff (Option)
*     ===========================================================
      use MilletModule
      implicit none
      include     'const.inc'
      include 'crp_watr.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*     Calculate today's transpiration efficiency from min and max
*     temperatures and converting mm water to g dry matter
*     (g dm/m^2/mm water)

*+  Changes
*     5/9/96 dph
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Millet_transpiration_eff')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
         call cproc_transp_eff1(
     :               c%svp_fract
     :             , c%transp_eff_cf
     :             , g%current_stage
     :             , g%maxt
     :             , g%mint
     :             , g%transp_eff
     :             )
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end

*     ===========================================================
      subroutine Millet_water_demand (Option)
*     ===========================================================
      use MilletModule
      implicit none
      include 'const.inc'
      include 'crp_watr.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     Soil water demand

*+  Changes
*     5/9/96 dph
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Millet_water_demand')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
         call cproc_sw_demand1(
     :           g%dlt_dm_light
     :         , g%transp_eff
     :         , g%sw_demand
     :         )
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end


*     ===========================================================
      subroutine Millet_root_depth (Option)
*     ===========================================================
      use MilletModule
      implicit none
      include 'const.inc'
      include 'crp_root.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     root distribution

*+  Changes
*     5/9/96 dph
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Millet_root_depth')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 1) then
         call cproc_root_depth1 (
     :                              g%dlayer
     :                             ,c%num_sw_ratio
     :                             ,c%x_sw_ratio
     :                             ,c%y_sw_fac_root
     :                             ,g%dul_dep
     :                             ,g%sw_dep
     :                             ,p%ll_dep
     :                             ,c%root_depth_rate
     :                             ,g%current_stage
     :                             ,p%xf
     :                             ,g%dlt_root_depth
     :                             ,g%root_depth
     :                             )
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end


*     ===========================================================
      subroutine Millet_root_depth_init (Option)
*     ===========================================================
      use MilletModule
      implicit none
      include 'const.inc'
      include 'crp_root.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     root distribution

*+  Changes
*     5/9/96 dph
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Millet_root_depth_init')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
         call cproc_root_depth_init1
     :               (
     :                c%initial_root_depth
     :              , g%current_stage
     :              , germ
     :              , g%days_tot
     :              , g%root_depth
!cjh     :              , g%dlt_root_depth
     :               )
 
      elseif (Option .eq. 2) then
 
         call cproc_root_depth_init1
     :               (
     :                g%sowing_depth
     :              , g%current_stage
     :              , germ
     :              , g%days_tot
     :              , g%root_depth
!cjh     :              , g%dlt_root_depth
     :               )
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end

*     ===========================================================
      subroutine Millet_water_stress(Option)
*     ===========================================================
      use MilletModule
      implicit none
      include 'const.inc'
      include 'crp_watr.pub'                      
      include 'crp_comm.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option       ! (INPUT) option number

*+  Purpose
*     Get current water stress factors (0-1)

*+  Changes
*     010994 jngh specified and programmed
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*) ! name of procedure
      parameter (my_name = 'crop_water_stress')

*+  Local Variables
      real ext_sw_supply (max_layer) ! external sw supply (mm)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (Option .eq. 1) then
         if (p%uptake_source .eq. 'apsim') then
            ! this would have been avoided if we have
            ! each stress factor in its own routine! - NIH
            ! photo requires (really) actually water uptake
            ! but expansion requires pot water uptake.
            ! we only have one supply variable.
 
            call crop_get_ext_uptakes(
     :                 p%uptake_source   ! uptake flag
     :                ,c%crop_type       ! crop type
     :                ,'water'           ! uptake name
     :                ,1.0               ! unit conversion factor
     :                ,0.0               ! uptake lbound
     :                ,100.0             ! uptake ubound
     :                ,ext_sw_supply     ! uptake array
     :                ,max_layer         ! array dim
     :                )
            call crop_swdef_photo(
     :                           max_layer
     :                         , g%dlayer
     :                         , g%root_depth
     :                         , g%sw_demand
     :                         , ext_sw_supply
     :                         , g%swdef_photo)
         else
            call crop_swdef_photo(
     :                           max_layer
     :                         , g%dlayer
     :                         , g%root_depth
     :                         , g%sw_demand
     :                         , g%sw_supply
     :                         , g%swdef_photo)
         endif
 
         call crop_swdef_expansion(
     :                           c%num_sw_demand_ratio
     :                         , c%x_sw_demand_ratio
     :                         , c%y_swdef_leaf
     :                         , max_layer
     :                         , g%dlayer
     :                         , g%root_depth
     :                         , g%sw_demand
     :                         , g%sw_supply
     :                         , g%swdef_expansion)
         call crop_swdef_pheno(
     :                           c%num_sw_avail_ratio
     :                         , c%x_sw_avail_ratio
     :                         , c%y_swdef_pheno
     :                         , max_layer
     :                         , g%dlayer
     :                         , g%root_depth
     :                         , g%sw_avail
     :                         , g%sw_avail_pot
     :                         , g%swdef_pheno)
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine Millet_water_uptake (Option)
*     ===========================================================
      use MilletModule
      implicit none
      include 'const.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'crp_watr.pub'                      
      include 'crp_comm.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     Soil water uptake

*+  Changes
*     5/9/96 dph
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Millet_water_uptake')

*+  Local Variables
      integer    layer                 ! layer number of profile ()
      real       ext_sw_supply(max_layer)

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (p%uptake_source .eq. 'apsim') then
         call crop_get_ext_uptakes(
     :                 p%uptake_source   ! uptake flag
     :                ,c%crop_type       ! crop type
     :                ,'water'           ! uptake name
     :                ,1.0               ! unit conversion factor
     :                ,0.0               ! uptake lbound
     :                ,100.0             ! uptake ubound
     :                ,ext_sw_supply     ! uptake array
     :                ,max_layer         ! array dim
     :                )
 
         do 100 layer = 1, g%num_layers
            g%dlt_sw_dep(layer) = -ext_sw_supply(layer)
  100    continue
 
 
      elseif (Option .eq. 1) then
         call crop_sw_uptake0 (max_layer
     :                       , g%dlayer
     :                       , g%root_depth
     :                       , g%sw_demand
     :                       , g%sw_supply
     :                       , g%dlt_sw_dep)
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine Millet_leaf_area_init (Option)
*     ===========================================================
      use MilletModule
      implicit none
      include 'const.inc'
      include 'crp_cnpy.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*

*+  Changes
*      28/4/98 igh

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Millet_leaf_area_init')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
         call cproc_leaf_area_init1 (
     :                c%initial_tpla
     :              , emerg
     :              , g%current_stage
     :              , g%days_tot
     :              , g%plants
     :              , g%lai
     :              )
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end


*     ===========================================================
      subroutine Millet_leaf_no_init (Option)
*     ===========================================================
      use MilletModule
      implicit none
      include 'const.inc'
      include 'crp_cnpy.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*

*+  Changes
*      28/4/98 igh

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Millet_leaf_no_init')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
         call cproc_leaf_no_init1
     :               (
     :                c%leaf_no_at_emerg
     :              , g%current_stage
     :              , emerg
     :              , g%days_tot
     :              , g%leaf_no
     :              , g%node_no
     :               )
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end

*     ===========================================================
      subroutine Millet_leaf_no_pot (Option)
*     ===========================================================
      use MilletModule
      implicit none
      include 'const.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*       Phenology leaf

*+  Changes
*      28/4/98 igh

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Millet_leaf_no_pot')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 101) then
 
 
!cjh         call Millet_leaf_number_final (g%leaf_no_final)
cjh special for erik - start
      if (.not. g%set_leaf_no_final) then
         call Millet_leaf_number_final (g%leaf_no_final)
      else
         ! final leaf no already set
      endif
cjh special for erik - end      
!         call Millet_leaf_number_final (
!     :          g%current_stage,
!     :          g%days_tot,
!     :          g%phase_tt,
!     :          emerg,
!     :          c%leaf_init_rate,
!     :          c%leaf_no_seed,
!     :          c%leaf_no_min,
!     :          c%leaf_no_max,
!     :          g%tt_tot,
!     :          g%leaf_no_final)
 
 
      call millet_leaf_appearance (g%dlt_leaf_no_pot) ! fraction of leaf emerged
!         call Millet_leaf_appearance0 (
!     :          g%leaf_no,
!     :          g%leaf_no_final,
!     :          c%leaf_no_rate_change,
!     :          c%leaf_app_rate2,
!     :          c%leaf_app_rate1,
!     :          g%current_stage,
!     :          g%days_tot,
!     :          g%dlt_tt,
!     :          g%dlt_leaf_no) ! fraction of leaf emerged
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end

*     ===========================================================
      subroutine Millet_leaf_area_potential (Option)
*     ===========================================================
      use MilletModule
      implicit none
      include 'const.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) option number

*+  Purpose
*       Simulate potential crop leaf area development - may be limited by
*       DM production in subsequent routine

*+  Changes
*      250894 jngh specified and programmed
*      270995 scc added leaf area routine option
*      5/9/96 dph added option argument

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Millet_leaf_area_potential')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
            ! Plant leaf development
            ! initialise total leaf number
         ! TEMPLATE OPTION
         ! Two alternative leaf area routines
 
      if (Option .eq. 101) then
 
         call millet_leaf_area_devel1 (
     :          g%leaf_no
     :        , g%leaf_no_effective
     :        , c%leaf_no_correction
     :        , c%x0_const
     :        , c%x0_slope
     :        , g%leaf_no_final
     :        , p%y0_const
     :        , p%y0_slope
     :        , c%a_const
     :        , c%a_slope1
     :        , c%a_slope2
     :        , c%b_const
     :        , c%b_slope1
     :        , c%b_slope2
!cjh     :        , g%dlt_leaf_no
     :        , g%dlt_leaf_no_pot
     :        , g%plants
     :        , g%swdef_expansion
     :        , g%dlt_lai_pot) ! individual leaf approach
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
      call pop_routine (my_name)
      return
      end


*     ===========================================================
      subroutine Millet_leaf_area_stressed (Option)
*     ===========================================================
      use MilletModule
      implicit none
      include 'const.inc'
      include 'crp_cnpy.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) option number

*+  Purpose
*     Calculate the stressed dlt_lai from the potential dlt_lai.

*+  Changes
*     26/2/96  sb made it up.

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Millet_leaf_area_stressed')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 101) then
 
         call cproc_leaf_area_stressed1 (
     :                       g%dlt_lai_pot
     :                      ,g%swdef_expansion
     :                      ,1.0
!cjh     :                      ,min(g%nfact_expansion
!cjh     :                          ,g%pfact_expansion)
     :                      ,g%dlt_lai_stressed
     :                      )

      else
 
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
      g%dlt_lai_pot = g%dlt_lai_stressed
      call pop_routine (my_name)
      return
      end


*     ===========================================================
      subroutine millet_bio_actual (Option)
*     ===========================================================
      use milletModule
      implicit none
      include 'const.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     bio actual

*+  Changes
*     5/9/96 dph
*     970317 slw new template form

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_bio_actual')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 101) then
         ! use whichever is limiting
 
         call millet_dm_init (g%dm_green, g%dm_plant_min)
!         call millet_dm_init (g%current_stage,
!     :          g%days_tot,
!     :          c%dm_root_init,
!     :          g%plants,
!     :          c%dm_stem_init,
!     :          c%dm_leaf_init,
!     :          c%stem_trans_frac,
!     :          c%leaf_trans_frac,
!     :          g%dm_green, g%dm_plant_min)

cjh special for erik - start
      if (.not. g%stop_growth) then
         g%dlt_dm = min (g%dlt_dm_light, g%dlt_dm_water)
      else
         ! no drymatter production
         g%dlt_dm = 0.0
      endif
cjh special for erik - end
!cjh         g%dlt_dm = min (g%dlt_dm_light, g%dlt_dm_water)
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end


*     ===========================================================
      subroutine millet_bio_partition (Option)
*     ===========================================================
      use milletModule
      implicit none
      include 'const.inc'
      include 'data.pub'                          
      include 'science.pub'                       
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     bio partition

*+  Changes
*     5/9/96 dph
*     970317 slw new template form

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_bio_partition')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (Option .eq. 101) then
         call millet_dm_partition (
     :          g%current_stage
     :        , c%ratio_root_shoot
     :        , g%dlt_dm
     :        , g%leaf_no
     :        , c%partition_rate_leaf
     :        , g%dlt_lai_stressed
     :        , c%sla_min
     :        , c%frac_stem2flower
     :        , g%dlt_dm_grain_demand
     :        , g%dlt_dm_green
     :        , c%tiller_no_pot
     :        , g%dlt_lai_pot
     :        , p%hi_max_pot
     :        , g%dm_green
     :        , g%dm_senesced)
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end


*     ===========================================================
      subroutine millet_bio_retrans (Option)
*     ===========================================================
      use milletModule
      implicit none
      include 'const.inc'
      include 'crp_biom.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     bio retrans

*+  Changes
*     5/9/96 dph
*     970317 slw new template form

*+  Constant Values
!      integer num_supply_pools
!      parameter (num_supply_pools = 2)
*
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_bio_retrans')

*+  Local Variables
!      integer supply_pools(num_supply_pools)
!      data supply_pools /stem,leaf/
!      save /supply_pools/

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 101) then
 
         call millet_dm_retranslocate (g%dlt_dm_green_retrans)
!         call cproc_dm_retranslocate1
!     :               (
!     :                g%current_stage
!     :              , start_grain_fill
!     :              , maturity
!     :              , grain
!     :              , max_part
!     :              , supply_pools
!     :              , num_supply_pools
!     :              , g%dlt_dm_grain_demand
!     :              , g%dlt_dm_green
!     :              , g%dm_green
!     :              , g%dm_plant_min
!     :              , g%plants
!     :              , g%dlt_dm_green_retrans
!     :               )
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_leaf_actual(Option)
*     ===========================================================
      use milletModule
      implicit none
      include 'const.inc'
      include 'data.pub'                          
      include 'science.pub'                       
      include 'crp_cnpy.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*       biomass light

*+  Changes
*      5/9/96 dph

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_leaf_actual')

*+  Local Variables
!      real leaf_no_now
!      real interp_sla_max

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 101) then
         ! potential by photosynthesis
 
         call millet_leaf_area ()
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end

*     ===========================================================
      subroutine millet_height (Option)
*     ===========================================================
      use milletModule
      implicit none
      include 'science.pub'                       
      include 'const.inc'
      include 'crp_cnpy.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*       canopy height

*+  Changes
*      5/9/96 dph

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_height')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 101) then
         if (stage_is_between (emerg, flag_leaf
     :                    , g%current_stage)) then
 
            call cproc_canopy_height
     :               (
     :                g%canopy_height
     :              , p%x_stem_wt
     :              , p%y_height
     :              , p%num_stem_wt
     :              , g%dm_green
     :              , g%plants
     :              , stem
     :              , g%dlt_canopy_height
     :               )
 
         else
            g%dlt_canopy_height = 0.0
    
         endif
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end

*     ===========================================================
      subroutine millet_bio_grain_demand (Option)
*     ===========================================================
      use milletModule
      implicit none
      include 'const.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*       contains grain, partition and retrans grain routines
*       bio grain

*+  Changes
*     5/9/96 dph
*     970317 slw new template form

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_bio_grain_demand')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
         ! note: The dm partition and retranslocate subroutines
         ! implicitly account for both grain no and harvest index
         ! approaches in calculating delta grain.
 
      if (Option .eq. 1) then
         ! Standard routines (4) simulate grain no approach (Ceres)
           call millet_heat_stress (g%maxt
     :        ,               c%temp_grain_crit_stress
     :        ,               g%dlt_heat_stress_tt)     ! high temperature stres
           call millet_grain_no(g%current_stage
     :        , g%days_tot
     :        , g%dm_plant_top_tot
     :        , c%growth_rate_min
     :        , c%growth_rate_crit
     :        , p%head_grain_no_max
     :        , g%heat_stress_tt
     :        , c%htstress_coeff
     :        , g%N_conc_min
     :        , g%dm_green
     :        , g%N_green
     :        , g%plants
     :        , c%seed_wt_min
     :        , c%grain_N_conc_min
     :        , g%grain_no)              ! set grain number
           call millet_dm_grain (
     :          g%current_stage
     :        , g%maxt
     :        , g%mint
     :        , c%x_temp_grain
     :        , c%y_grain_rate
     :        , c%num_temp_grain
     :        , c%swdf_grain_min
     :        , g%grain_no
     :        , p%grain_gth_rate
     :        , g%N_conc_min
     :        , g%dm_green
     :        , g%N_green
     :        , c%temp_fac_min
     :        , c%tfac_slope
     :        , c%sw_fac_max
     :        , c%sfac_slope
     :        , g%N_conc_crit
     :        , g%swdef_photo
!cjh     :         , 1.0
     :        , g%pfact_grain
     :        , g%swdef_expansion
     :        , g%nfact_grain_conc
     :        , g%dlt_dm_grain_demand)
 
!      else if (Option .eq. 2) then
!         ! Alternative routines (3) simulate harvest index approach (GLH)
!           call millet_dm_stress_max (
!     :          g%swdef_photo,
!     :          g%nfact_photo,
!     :          g%temp_stress_photo,
!     :          g%dlt_dm_stress_max)
!           call millet_dm_grain_hi (
!     :          g%current_stage,
!     :          g%dm_stress_max,
!     :          g%days_tot,
!     :          p%hi_max_pot,
!     :          c%hi_min,
!     :          g%dm_green,
!     :          g%dm_senesced,
!     :          g%dlt_dm,
!     :          p%hi_incr,
!     :          g%dlt_dm_grain_demand)
      elseif (Option .eq. 3) then
         ! same as option 1 but with grain no routine fix
           call millet_heat_stress (g%maxt
     :        ,               c%temp_grain_crit_stress
     :        ,               g%dlt_heat_stress_tt)     ! high temperature stres
           call millet_grain_no1(g%current_stage
     :        , g%days_tot
     :        , g%dm_plant_top_tot
     :        , c%growth_rate_min
     :        , c%growth_rate_crit
     :        , p%head_grain_no_max
     :        , g%heat_stress_tt
     :        , c%htstress_coeff
     :        , g%N_conc_min
     :        , g%dm_green
     :        , g%N_green
     :        , g%plants
     :        , c%seed_wt_min
     :        , c%grain_N_conc_min
     :        , g%grain_no)              ! set grain number
           call millet_dm_grain (
     :          g%current_stage
     :        , g%maxt
     :        , g%mint
     :        , c%x_temp_grain
     :        , c%y_grain_rate
     :        , c%num_temp_grain
     :        , c%swdf_grain_min
     :        , g%grain_no
     :        , p%grain_gth_rate
     :        , g%N_conc_min
     :        , g%dm_green
     :        , g%N_green
     :        , c%temp_fac_min
     :        , c%tfac_slope
     :        , c%sw_fac_max
     :        , c%sfac_slope
     :        , g%N_conc_crit
     :        , g%swdef_photo
     :        , g%pfact_grain
     :        , g%swdef_expansion
     :        , g%nfact_grain_conc
     :        , g%dlt_dm_grain_demand)
!      elseif (Option .eq. 4) then
!         ! same as option 3 but with different grain no option
!           call millet_heat_stress (g%maxt,
!     :                        c%temp_grain_crit_stress,
!     :                        g%dlt_heat_stress_tt)     ! high temperature stres
!           call millet_grain_no2(g%current_stage,
!     :          g%days_tot,
!     :          g%dm_plant_top_tot,
!     :          c%grno_grate,
!     :          c%grno_fract,
!     :          c%num_grno_grate,
!     :          p%head_grain_no_max,
!     :          g%heat_stress_tt,
!     :          c%htstress_coeff,
!     :          g%N_conc_min,
!     :          g%dm_green,
!     :          g%N_green,
!     :          g%plants,
!     :          c%seed_wt_min,
!     :          c%grain_N_conc_min,
!     :          g%grain_no)              ! set grain number
!           call millet_dm_grain (
!     :          g%current_stage,
!     :          g%maxt,
!     :          g%mint,
!     :          c%x_temp_grain,
!     :          c%y_grain_rate,
!     :          c%num_temp_grain,
!     :          c%swdf_grain_min,
!     :          g%grain_no,
!     :          p%grain_gth_rate,
!     :          g%N_conc_min,
!     :          g%dm_green,
!     :          g%N_green,
!     :          c%temp_fac_min,
!     :          c%tfac_slope,
!     :          c%sw_fac_max,
!     :          c%sfac_slope,
!     :          g%N_conc_crit,
!     :          g%swdef_photo,
!     :          g%pfact_grain,
!     :          g%swdef_expansion,
!     :          g%nfact_grain_conc,
!     :      g%dlt_dm_grain_demand)
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end


*     ===========================================================
      subroutine millet_leaf_death (Option)
*     ===========================================================
      use milletModule
      implicit none
      include 'const.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     leaf death.

*+  Changes
*     5/9/96 dph
*     970318 slw new template version

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_leaf_death')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (Option .eq. 101) then
         call millet_leaf_death_o (g%dlt_leaf_no_dead)
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end

*     ===========================================================
      subroutine millet_leaf_area_sen (Option)
*     ===========================================================
      use milletModule
      implicit none
      include 'const.inc'
      include 'crp_cnpy.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     leaf area senesence - age

*+  Changes
*     5/9/96 dph
*     970318 slw new template version

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_leaf_area_sen')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
         ! Standard routine derived from Ceres - simpler ?
         !TEMPLATE OPTION alternative routine (1) developed by GLH - mechanistic
 
      if (Option .eq. 1) then
 
         call cproc_leaf_area_sen1
     :               (
     :                emerg
     :              , now
     :              , g%dlt_lai_stressed
     :              , g%dlt_leaf_no
     :              , g%dlt_leaf_no_dead
     :              , g%lai
     :              , g%leaf_area
     :              , g%leaf_no_dead
     :              , g%plants
     :              , g%slai
     :              , c%tpla_min
     :              , g%dlt_slai_age
     :              , c%lai_sen_light
     :              , c%sen_light_slope
     :              , g%dlt_slai_light
     :              , c%sen_rate_water
     :              , g%swdef_photo
     :              , g%dlt_slai_water
     :              , c%x_temp_senescence
     :              , c%y_senescence_fac
     :              , c%num_temp_senescence
     :              , g%mint
     :              , g%dlt_slai_frost
     :              , g%dlt_slai
     :               )
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end


*     ===========================================================
      subroutine millet_sen_bio (Option)
*     ===========================================================
      use milletModule
      implicit none
      include 'const.inc'
      include 'crp_biom.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     biomass senescence

*+  Changes
*      5/9/96 dph
*     970318 slw new template version
*     991116 ew changed the crop_dm_senescence0 call

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_sen_bio')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (Option .eq. 1) then
         call crop_dm_senescence0(max_part, root, leaf, stem
     :        , c%dm_leaf_sen_frac
     :        , c%dm_root_sen_frac
     :        , g%dlt_dm_green
     :        , g%dlt_dm_green_retrans
     :        , g%dlt_lai
     :        , g%dlt_slai
     :        , g%dm_green
     :        , g%lai
     :        , g%dlt_dm_senesced
     :        , g%dlt_dm_sen_retrans)
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end

*     ===========================================================
      subroutine millet_sen_nit (Option)
*     ===========================================================
      use milletModule
      implicit none
      include 'const.inc'
      include 'crp_nitn.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     biomass senescence

*+  Changes
*     5/9/96 dph
*     970318 slw new template version

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_sen_nit')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (Option .eq. 101) then
 
         call millet_N_senescence (g%dlt_N_senesced)
!         call cproc_N_senescence1 (max_part
!     :                              , c%n_sen_conc
!     :                              , g%dlt_dm_senesced
!     :                              , g%n_green
!     :                              , g%dm_green
!     :                              , g%dlt_N_senesced)
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end




*     ===========================================================
      subroutine millet_nit_init (Option)
*     ===========================================================
      use milletModule
      implicit none
      include   'const.inc'
      include 'crp_nitn.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*      Initialise plant nitrogen.

*+  Changes
*     250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_nit_init')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
         call millet_N_init (g%N_green)
!         call cproc_N_init1
!     :               (
!     :                c%n_init_conc
!     :              , max_part
!     :              , emerg
!     :              , g%current_stage
!     :              , g%days_tot
!     :              , g%dm_green
!     :              , g%N_green
!     :               )
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end


*     ===========================================================
      subroutine millet_nit_supply (Option)
*     ===========================================================
      use milletModule
      implicit none
      include 'const.inc'
      include 'convert.inc'
      include 'data.pub'                          
      include 'crp_nitn.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*      n supply

*+  Changes
*     5/9/96 dph

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_nit_supply')

*+  Local Variables
      real    fixation_determinant

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (p%uptake_source .eq. 'apsim') then
         ! do nothing here for now
         ! I assume that the retrans routine does not need the
         ! call below as it is called on its own from process routine.
         ! -NIH
 
      elseif (Option .eq. 1) then
 
         fixation_determinant = sum_real_array(g%dm_green, max_part)
     :                        - g%dm_green(root)
 
         call cproc_n_supply1 (
     :            g%dlayer
     :          , max_layer
     :          , g%dlt_sw_dep
     :          , g%NO3gsm
     :          , g%NO3gsm_min
     :          , g%root_depth
     :          , g%sw_dep
     :          , g%NO3gsm_mflow_avail
     :          , g%sw_avail
     :          , g%NO3gsm_diffn_pot
     :          , g%current_stage
     :          , c%n_fix_rate
     :          , fixation_determinant
     :          , g%swdef_fixation
     :          , g%N_fix_pot
     :          )
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end


*     ===========================================================
      subroutine millet_nit_retrans (Option)
*     ===========================================================
      use milletModule
      implicit none
      include 'const.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     nitrogen retranslocation

*+  Changes
*     5/9/96 dph

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_nit_retrans')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
           call millet_N_retranslocate (
     :          g%dlt_dm_green
     :        , g%maxt
     :        , g%mint
     :        , c%temp_fac_min
     :        , c%tfac_slope
     :        , c%sw_fac_max
     :        , c%sfac_slope
     :        , g%N_conc_min
     :        , g%N_conc_crit
     :        , g%dm_green
     :        , g%N_green
     :        , g%N_conc_max
     :        , g%swdef_expansion
     :        , g%nfact_grain_conc
     :        , g%dlt_N_retrans
     :                    )
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end




*     ===========================================================
      subroutine millet_nit_demand (Option)
*     ===========================================================
      use milletModule
      implicit none
      include 'const.inc'
      include 'crp_nitn.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     n demand

*+  Changes
*     5/9/96 dph

*+  Constant Values
!jh      integer num_demand_parts
!jh      parameter (num_demand_parts = 4)
*
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_nit_demand')

*+  Local Variables
      real    dlt_dm_pot_radn         ! pot dm production given radn
      integer current_phase
!jh      integer demand_parts(num_demand_parts)
!jh      data demand_parts /root,leaf,stem,flower/
!jh      save /demand_parts/

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 1) then
         ! calculate potential new shoot and root growth
         current_phase = int (g%current_stage)
         ! need to calculate dm using potential rue not affected by
         ! N and temperature
         dlt_dm_pot_radn = c%rue(current_phase)*g%radn_int
 
         call cproc_N_demand1
     :               (
     :                max_part
     :              , demand_parts
     :              , num_demand_parts
     :              , g%dlt_dm
     :              , g%dlt_dm_green
     :              , dlt_dm_pot_radn
     :              , g%dlt_n_retrans
     :              , g%dm_green
     :              , g%n_conc_crit
     :              , g%n_conc_max
     :              , g%n_green
     :              , g%N_demand, g%N_max
     :               )
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_nit_uptake (Option)
*     ===========================================================
      use milletModule
      implicit none
      include 'const.inc'
      include 'convert.inc'
      include 'crp_nitn.pub'                      
      include 'crp_comm.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     n uptake

*+  Changes
*     5/9/96 dph

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_nit_uptake')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (p%uptake_source .eq. 'apsim') then
         ! NIH - note that I use a -ve conversion
         ! factor FOR NOW to make it a delta.
         call crop_get_ext_uptakes(
     :                 p%uptake_source   ! uptake flag
     :                ,c%crop_type       ! crop type
     :                ,'no3'             ! uptake name
     :                ,-kg2gm/ha2sm      ! unit conversion factor
     :                ,0.0               ! uptake lbound
     :                ,100.0             ! uptake ubound
     :                ,g%dlt_no3gsm      ! uptake array
     :                ,max_layer         ! array dim
     :                )
 
      elseif (Option .eq. 1) then
 
         call cproc_N_uptake1
     :               (
     :                c%no3_diffn_const
     :              , g%dlayer
     :              , max_layer
     :              , g%no3gsm_diffn_pot
     :              , g%no3gsm_mflow_avail
     :              , g%N_fix_pot
     :              , c%n_supply_preference
     :              , g%n_demand
     :              , g%n_max
     :              , max_part
     :              , g%root_depth
     :              , g%dlt_NO3gsm
     :               )
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end




*     ===========================================================
      subroutine millet_nit_partition (Option)
*     ===========================================================
      use milletModule
      implicit none
      include 'const.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     nitrogen partition

*+  Changes
*     5/9/96 dph

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_nit_partition')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
         call millet_N_partition (
     :          g%root_depth
     :        , g%dlayer
     :        , g%N_demand
     :        , g%N_max
     :        , g%dlt_NO3gsm
     :        , g%dlt_N_green
     :                  )
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end


*     ===========================================================
      subroutine millet_N_retranslocate (
     :          g_dlt_dm_green
     :        , g_maxt
     :        , g_mint
     :        , c_temp_fac_min
     :        , c_tfac_slope
     :        , c_sw_fac_max
     :        , c_sfac_slope
     :        , g_N_conc_min
     :        , g_N_conc_crit
     :        , g_dm_green
     :        , g_N_green
     :        , g_N_conc_max
     :        , g_swdef_expansion
     :        , g_nfact_grain_conc
     :        , dlt_N_retrans)
*     ===========================================================
      use milletModule
      implicit none
      include 'data.pub'                          
      include 'crp_nitn.pub'                      
      include 'error.pub'                         

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
      real       dlt_N_retrans (*)     ! (OUTPUT) plant N taken out from
                                       ! plant parts (g N/m^2)

*+  Purpose
*     Calculate the nitrogen retranslocation from the various plant parts
*     to the grain.

*+  Changes
*     080994 jngh specified and programmed

*+  Calls

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_N_retranslocate')

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
 
      grain_N_demand = g_dlt_dm_green(grain) * crop_N_dlt_grain_conc(
     :          grain
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
 
      N_potential  = (g_dm_green(grain) + g_dlt_dm_green(grain))
     :             * g_N_conc_max(grain)
 
      grain_N_demand = u_bound (grain_N_demand
     :                        , N_potential - g_N_green(grain))
 
      call crop_N_retrans_avail (max_part, root, grain
     :        , g_N_conc_min
     :        , g_dm_green
     :        , g_N_green,N_avail)  ! grain N potential (supply)
 
            ! available N does not include roots or grain
cjh  this should not presume roots and grain are 0.
csc  true....
 
      N_avail_stover  =  sum_real_array (N_avail, max_part)
 
          ! get actual grain N uptake
 
          ! limit retranslocation to total available N
 
      call fill_real_array (dlt_N_retrans, 0.0, max_part)
 
      if (grain_N_demand.ge.N_avail_stover) then
 
             ! demand greater than or equal to supply
             ! retranslocate all available N
 
         dlt_N_retrans(leaf) = - N_avail(leaf)
         dlt_N_retrans(stem) = - N_avail(stem)
         dlt_N_retrans(flower) = - N_avail(flower)
         dlt_N_retrans(grain) = N_avail_stover
 
      else
 
             ! supply greater than demand.
             ! Retranslocate what is needed
 
         dlt_N_retrans(leaf) = - grain_N_demand
     :                         * divide (N_avail(leaf)
     :                                 , N_avail_stover, 0.0)
 
         dlt_N_retrans(flower) = - grain_N_demand
     :                         * divide (N_avail(flower)
     :                                 , N_avail_stover, 0.0)
 
         dlt_N_retrans(stem) = - grain_N_demand
     :                         - dlt_N_retrans(leaf)   ! note - these are
     :                         - dlt_N_retrans(flower) ! -ve values.
 
         dlt_N_retrans(grain) = grain_N_demand
 
      endif
 
             ! just check that we got the maths right.
 
      do 1000 part = root, flower
         call bound_check_real_var (abs (dlt_N_retrans(part))
     :                            , 0.0, N_avail(part)
     :                            , 'dlt_N_retrans(part)')
1000  continue
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_N_partition(
     :          g_root_depth
     :        , g_dlayer
     :        , g_N_demand
     :        , g_N_max
     :        , dlt_NO3gsm
     :        , dlt_N_green
     :                     )
*     ===========================================================
      use milletModule
      implicit none
      include 'data.pub'                          
      include 'error.pub'                         
      include 'science.pub'                         

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
      parameter (my_name = 'millet_N_partition')

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
      end



*     ===========================================================
      subroutine millet_N_conc_limits (
     :          g_current_stage
     :        , c_N_conc_crit_grain
     :        , c_N_conc_max_grain
     :        , c_N_conc_min_grain
     :        , c_N_conc_crit_root
     :        , c_N_conc_max_root
     :        , c_N_conc_min_root
     :        , c_x_stage_code
     :        , c_stage_code_list
     :        , g_tt_tot
     :        , g_phase_tt
     :        , c_y_N_conc_crit_stem
     :        , c_y_N_conc_crit_leaf
     :        , c_y_N_conc_crit_flower
     :        , c_y_N_conc_min_stem
     :        , c_y_N_conc_min_leaf
     :        , c_y_N_conc_min_flower
     :        , c_y_N_conc_max_stem
     :        , c_y_N_conc_max_leaf
     :        , c_y_N_conc_max_flower
     :        , N_conc_crit
     :        , N_conc_max
     :        , N_conc_min)
*     ===========================================================
      use milletModule
      implicit none
      include 'crp_phen.pub'                      
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
       real g_current_stage
       real c_N_conc_crit_grain
       real c_N_conc_max_grain
       real c_N_conc_min_grain
       real c_N_conc_crit_root
       real c_N_conc_max_root
       real c_N_conc_min_root
       real c_x_stage_code(*)
       real c_stage_code_list(*)
       real g_tt_tot(*)
       real g_phase_tt(*)
       real c_y_n_conc_crit_stem(*)
       real c_y_n_conc_crit_leaf(*)
       real c_y_n_conc_crit_flower(*)
       real c_y_n_conc_min_stem(*)
       real c_y_n_conc_min_leaf(*)
       real c_y_n_conc_min_flower(*)
       real c_y_n_conc_max_stem(*)
       real c_y_n_conc_max_leaf(*)
       real c_y_n_conc_max_flower(*)
      real       N_conc_crit(*)        ! (OUTPUT) critical N concentration
                                       ! (g N/g part)
      real       N_conc_max(*)         ! (OUTPUT) maximum N concentration
                                       ! (g N/g part)
      real       N_conc_min(*)         ! (OUTPUT) minimum N concentration
                                       ! (g N/g part)

*+  Purpose
*       Calculate the critical N concentration below which plant growth
*       is affected.  Also minimum and maximum N concentrations below
*       and above which it is not allowed to fall or rise.
*       These are analogous to the water concentrations
*       of sat, dul and ll.

*+  Changes
*     080994 jngh specified and programmed

*+  Calls

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_N_conc_limits')

*+  Local Variables
      integer    numvals               ! number of values in stage code table
      real       current_stage_code            ! interpolated current stage code

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      call fill_real_array (N_conc_crit, 0.0, max_part)
      call fill_real_array (N_conc_min, 0.0, max_part)
 
      if (stage_is_between (emerg, maturity, g_current_stage)) then
         N_conc_crit(grain) = c_N_conc_crit_grain
         N_conc_max(grain) = c_N_conc_max_grain
         N_conc_min(grain) = c_N_conc_min_grain
 
         N_conc_crit(root) = c_N_conc_crit_root
         N_conc_max(root) = c_N_conc_max_root
         N_conc_min(root) = c_N_conc_min_root
 
             ! the tops critical N percentage concentration is the stover
             ! (non-grain shoot) concentration below which N concentration
             ! begins to affect plant growth.
 
         numvals = count_of_real_vals (c_x_stage_code, max_stage)
         current_stage_code = Crop_stage_code (
     :          c_stage_code_list
     :        , g_tt_tot
     :        , g_phase_tt
     :        , g_current_stage
     :        , c_x_stage_code
     :        , numvals
     :        , max_stage)
         N_conc_crit(stem) = linear_interp_real (current_stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_crit_stem
     :                             , numvals)
         N_conc_crit(leaf) = linear_interp_real (current_stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_crit_leaf
     :                             , numvals)
         N_conc_crit(flower) = linear_interp_real (current_stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_crit_flower
     :                             , numvals)
 
             ! the  minimum N concentration is the N concentration
             ! below which N does not fall.
 
         N_conc_min(stem) = linear_interp_real (current_stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_min_stem
     :                             , numvals)
 
         N_conc_min(leaf) = linear_interp_real (current_stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_min_leaf
     :                             , numvals)
 
         N_conc_min(flower) = linear_interp_real (current_stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_min_flower
     :                             , numvals)
 
             ! the  maximum N concentration is the N concentration
             ! above which N does not rise.
 
         N_conc_max(stem) = linear_interp_real (current_stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_max_stem
     :                             , numvals)
 
         N_conc_max(leaf) = linear_interp_real (current_stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_max_leaf
     :                             , numvals)
 
         N_conc_max(flower) = linear_interp_real (current_stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_max_flower
     :                             , numvals)
 
      else
      endif
 
      call pop_routine (my_name)
      return
      end

* ====================================================================
       subroutine millet_cover1 (
     :                                 g_row_spacing
     :                                ,c_x_row_spacing
     :                                ,c_y_extinct_coef
     :                                ,c_num_row_spacing
     :                                ,g_lai
     :                                ,g_cover_green
     :                                )
* ====================================================================
      implicit none
      include 'science.pub'                       
      include 'error.pub'                         

*+  Sub-Program Arguments
      real g_row_spacing
      real c_x_row_spacing(*)
      real c_y_extinct_coef(*)
      integer c_num_row_spacing
      real g_lai
      real g_cover_green

*+  Purpose
*     <insert here>

*+  Changes
*     15-08-1997 - huth - Programmed and Specified
*     10-02-1999 - huth - added pod cover component

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'millet_cover1')

*+  Local Variables
      real extinct_coef

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      extinct_coef = linear_interp_real (g_row_spacing
     :                                  ,c_x_row_spacing
     :                                  ,c_y_extinct_coef
     :                                  ,c_num_row_spacing)
 
      g_cover_green = 1.0 - exp(-extinct_coef*g_lai)

      call pop_routine (myname)
      return
      end

* ====================================================================
       subroutine millet_nit_demand_est (Option)
* ====================================================================
      use milletModule
      implicit none
      include 'const.inc'
      include 'data.pub'                          
      include 'crp_nitn.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option

*+  Purpose
*     <insert here>

*+  Changes
*     14-05-1997 - huth - Programmed and Specified

*+  Constant Values
!jh      integer num_demand_parts
!jh      parameter (num_demand_parts = 4)
*
      character*(*) myname               ! name of current procedure
      parameter (myname = 'millet_nit_demand_est')

*+  Local Variables
      integer current_phase
      real    dlt_dm_green_pot (max_part) ! potential (est) dlt dm green
      real    dlt_dm_pot_radn         ! pot dm production given radn
      real    dlt_N_retrans(max_part) ! retranslocated N
      real    dm_green_tot            ! total dm green
      integer part                    ! simple plant part counter
*
!jh      integer demand_parts(num_demand_parts)
!jh      data demand_parts /root,leaf,stem,flower/
!jh      save /demand_parts/

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
      dlt_dm_pot_radn = c%rue(current_phase)*g%radn_int
      dm_green_tot = sum_real_array (g%dm_green, max_part)
      do 100 part = 1, max_part
         dlt_dm_green_pot(part) = dlt_dm_pot_radn
     :                          * divide (g%dm_green(part)
     :                                   ,dm_green_tot
     :                                   ,0.0)
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
      end


* ====================================================================
      subroutine millet_P_uptake (Option)
* ====================================================================
      use milletModule
      implicit none
      include 'const.inc'
      include 'convert.inc'
      include 'data.pub'                          
      include 'error.pub'                         
      include 'intrface.pub'                      

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number
 
*+ Purpose
*      Get P uptake from P module and convert to require units
*      for internal use.
 
*+  Changes
*     26-06-1997 - huth - Programmed and Specified
 
*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'millet_P_uptake')
 
*+  Local Variables
      real layered_p_uptake(max_layer)
      integer numvals
 
*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      if (Option.eq.1) then
         call fill_real_array (layered_p_uptake,0.0,max_layer)

         call get_real_array_Optional 
     :                        (unknown_module
     :                       ,'uptake_p_millet'
     :                       ,max_layer
     :                       ,'()'
     :                       ,layered_p_uptake
     :                       ,numvals
     :                       ,0.0
     :                       ,100.)
         if (numvals.gt.0) then
            g%dlt_plant_p = sum_real_array (layered_p_uptake
     :                                     ,numvals)
     :                    * kg2gm/ha2sm

         else
            g%dlt_plant_p = g%p_demand

         endif
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
 
      call pop_routine (myname)
      return
      end
*     ===========================================================
      subroutine millet_P_conc_limits (
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
      use milletModule
      implicit none
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         
      include 'crp_phen.pub'                      

*+  Sub-Program Arguments
       real g_current_stage
       real c_p_stage_code(*)
       real c_stage_code_list(*)
       real g_tt_tot(*)
       real g_phase_tt(*)
       real c_p_conc_min(*)
       real c_p_conc_max(*)
      real       P_conc_max   ! (OUTPUT) maximum P conc
                              ! (g N/g part)
      real       P_conc_min   ! (OUTPUT) minimum P conc
                              ! (g N/g part)
 
*+  Purpose
*       Calculate the critical N concentration below which plant growth
*       is affected.  Also minimum and maximum N concentrations below
*       and above which it is not allowed to fall or rise.
 
*+  Changes
*     080994 jngh specified and programmed
 
*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_P_conc_limits')

*+  Local Variables
      integer    numvals               ! number of values in stage code table
!      real       current_stage_code            ! interpolated current stage code
 
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (stage_is_between (emerg, maturity, g_current_stage)) then
 
         numvals = count_of_real_vals (c_P_stage_code, max_stage)
 
!         current_stage_code = Crop_stage_code (
!     :          c_stage_code_list,
!     :          g_tt_tot,
!     :          g_phase_tt,
!     :          g_current_stage,
!     :          c_P_stage_code,
!     :          numvals,
!     :          max_stage)
 
cnh         P_conc_max = linear_interp_real (current_stage_code
         P_conc_max = linear_interp_real (g_current_stage
     :                                   ,c_P_stage_code
     :                                   ,c_P_conc_max
     :                                   ,numvals)
 
cnh         P_conc_min = linear_interp_real (current_stage_code
         P_conc_min = linear_interp_real (g_current_stage
     :                                   ,c_P_stage_code
     :                                   ,c_P_conc_min
     :                                   ,numvals)
 
 
      else
 
         P_conc_max = 0.0
         P_conc_min = 0.0
 
      endif
 
      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine millet_pfact
     :               (
     :                G_dm_green
     :              , G_dm_dead
     :              , G_dm_senesced
     :              , max_part
     :              , G_p_conc_max
     :              , G_p_conc_min
     :              , G_plant_p
     :              , k_pfact
     :              , pfact
     :               )
*     ===========================================================
      implicit none
      include   'const.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      REAL       G_dm_green(*)    ! (INPUT)  live plant biomass (g/m2)
      REAL       G_dm_dead(*)     ! (INPUT)  dead plant biomass (g/m2)
      REAL       G_dm_senesced(*) ! (INPUT)  senesced plant biomass (g/m2)
      INTEGER    max_part         ! (INPUT)  number of plant parts
      REAL       G_p_conc_max     ! (INPUT)  max P conc (g N/g biomass)
      REAL       G_p_conc_min     ! (INPUT)  min P conc (g N/g biomass)
      REAL       G_plant_p        ! (INPUT)  plant P content (g N/m^2)
      REAL       k_pfact          ! (INPUT)  k value for stress factor
      real      pfact             ! (OUTPUT) P stress factor
 
*+  Purpose
*     The concentration of P in the entire plant is used to derive a
*     series of Phosphorus stress indices.  The stress indices for
*     today's growth are calculated from yesterday's
*     relative nutritional status between a critical and minimum
*     total plant Phosphorus concentration.
 
 
*+   Changes
*     270697 nih
 
*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_pfact')
 
*+  Local Variables
      real       biomass               ! total crop biomass
      real       P_conc                ! actual P concentration (g/g)
 
      real       P_def                 ! P factor (0-1)
      real       P_conc_ratio          ! available P as fraction of P capacity
                                       ! (0-1)
 
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
         ! calculate actual P conc
      biomass    =  sum_real_array (g_dm_green, max_part)
     :           +  sum_real_array (g_dm_senesced, max_part)
     :           +  sum_real_array (g_dm_dead, max_part)
 
      P_conc = divide (g_plant_p, biomass, 0.0)
 
      P_conc_ratio = divide ((P_conc - g_P_conc_min)
     :                      ,(g_P_conc_max - g_P_conc_min)
     :                      , 0.0)
 
         ! calculate 0-1 P deficiency factors
 
      P_def = k_pfact * P_conc_ratio
      pfact = bound (P_def, 0.0, 1.0)
 
      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine millet_p_stress_photo (Option)
*     ===========================================================
      use milletModule
      implicit none
      include   'const.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number
 
*+   Purpose
*         Get current P stress factors (0-1)
 
*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_p_stress_photo')
 
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
         call millet_pfact
     :               (
     :                g%dm_green
     :              , g%dm_dead
     :              , g%dm_senesced
     :              , max_part
     :              , g%P_conc_max
     :              , g%P_conc_min
     :              , g%plant_p
     :              , c%k_pfact_photo
     :              , g%pfact_photo
     :               )
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine millet_p_stress_pheno (Option)
*     ===========================================================
      use milletModule
      implicit none
      include   'const.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number
 
*+  Purpose
*         Get current P stress factors (0-1)
 
*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_p_stress_pheno')
 
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
         call millet_pfact
     :               (
     :                g%dm_green
     :              , g%dm_dead
     :              , g%dm_senesced
     :              , max_part
     :              , g%P_conc_max
     :              , g%P_conc_min
     :              , g%plant_p
     :              , c%k_pfact_pheno
     :              , g%pfact_pheno
     :               )
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine millet_p_stress_expansion (Option)
*     ===========================================================
      use milletModule
      implicit none
      include   'const.inc'
      include 'error.pub'                         
 
*+  Purpose
*         Get current P stress factors (0-1)
 
*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number
 
*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_p_stress_expansion')
 
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
         call millet_pfact
     :               (
     :                g%dm_green
     :              , g%dm_dead
     :              , g%dm_senesced
     :              , max_part
     :              , g%P_conc_max
     :              , g%P_conc_min
     :              , g%plant_p
     :              , c%k_pfact_expansion
     :              , g%pfact_expansion
     :               )
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine millet_p_stress_grain (Option)
*     ===========================================================
 
*   Short description:
*         Get current P stress factors (0-1)
 
      use milletModule
      implicit none
 
*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number
 
*   Global variables
      include   'const.inc'
      include 'error.pub'                         
 
*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_p_stress_grain')
 
*   Initial data values
*       none
 
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
         call millet_pfact
     :               (
     :                g%dm_green
     :              , g%dm_dead
     :              , g%dm_senesced
     :              , max_part
     :              , g%P_conc_max
     :              , g%P_conc_min
     :              , g%plant_p
     :              , c%k_pfact_grain
     :              , g%pfact_grain
     :               )
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end
 
* ====================================================================
       subroutine millet_P_demand_est (Option)
* ====================================================================
      use milletModule
      implicit none
      include 'const.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option
 
*+  Purpose
*      None
 
*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'millet_P_demand_est')
 
*- Implementation Section ----------------------------------
      call push_routine (myname)
 
       if (Option.eq.1) then
          call millet_P_demand (
     :          g%current_stage
     :        , g%radn_int
     :        , c%rue
     :        , c%ratio_root_shoot
     :        , g%dm_green
     :        , g%dm_senesced
     :        , g%dm_dead
     :        , max_part
     :        , g%P_conc_max
     :        , g%plant_P
     :        , c%P_uptake_factor
     :        , g%P_demand)
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (myname)
      return
      end
*     ===========================================================
      subroutine millet_P_demand
     :               (
     :          g_current_stage
     :        , g_radn_int
     :        , c_rue
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
      implicit none
      include 'data.pub'                          
      include 'error.pub'                         
  
*+  Sub-Program Arguments
 
      REAL       g_current_stage
      REAL       g_radn_int
      REAL       c_rue(*)
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
      parameter (my_name = 'millet_p_demand')
 
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
 
         ! calculate potential new shoot and root growth
 
      current_phase = int (g_current_stage)
      dlt_dm_pot = c_rue(current_phase) * g_radn_int
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
      end
* ====================================================================
       subroutine millet_P_conc (Option)
* ====================================================================
      use milletModule
      implicit none
      include 'const.inc'
      include 'error.pub'                         
 
*+  Sub-Program Arguments
      integer Option

*+  Purpose
*      calculate p concentration curves
 
*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'millet_P_conc')
 
*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      if (Option.eq.1) then
         call millet_P_conc_limits (
     :          g%current_stage
     :        , c%p_stage_code
     :        , c%stage_code_list
     :        , g%tt_tot
     :        , g%phase_tt
     :        , c%P_conc_max
     :        , c%P_conc_min
     :        , g%P_conc_max
     :        , g%P_conc_min)
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (myname)
      return
      end

*     ===========================================================
      subroutine millet_Phos_init (Option)
*     ===========================================================
      use milletModule
      implicit none
      include   'const.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number
 
*+  Purpose
*      Initialise plant Phosphorus
 
*+  Changes:
*     270697 nih specified and programmed
 
*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_p_init')
 
*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
         call millet_P_init (
     :          emerg
     :        , g%current_stage
     :        , g%days_tot
     :        , g%dm_green
     :        , max_part
     :        , g%p_conc_max
     :        , g%plant_p
     :               )
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine millet_P_init (
     :          init_stage
     :        , g_current_stage
     :        , g_days_tot
     :        , g_dm_green
     :        , max_part
     :        , g_p_conc_max
     :        , g_plant_p)
*     ===========================================================
      implicit none
      include 'data.pub'                          
      include 'science.pub'                       
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer init_stage
      real g_current_stage
      real g_days_tot(*)
      real g_dm_green(*)
      integer max_part
      real       g_p_conc_max
      real       g_plant_p
 
*+  Purpose
*     Set initial plant p
 
*+  Changes:
*     270697 nih specified and programmed
 
*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_P_init')

*+  Local Variables
      real biomass
 
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (on_day_of (init_stage, g_current_stage, g_days_tot)) then
         biomass = sum_real_array (g_dm_green, max_part)
         g_plant_p = g_p_conc_max * biomass
      else
      endif
 
      call pop_routine (my_name)
      return
      end


*     ===========================================================
      subroutine millet_plant_death (Option)
*     ===========================================================
      use milletModule
      implicit none
      include 'const.inc'
      include 'crp_fail.pub'                      
      include 'error.pub'                         
      include 'data.pub'                         

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*       crop death

*+  Changes
*      5/9/96 dph

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_plant_death')

*+  Local Variables
cnh      real dlt_plants

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
         g%dlt_plants_all = 0.0
         call crop_failure_germination (sowing, germ, now
     :        , c%days_germ_limit
     :        , g%current_stage
     :        , g%days_tot
     :        , g%plants
     :        , g%dlt_plants_all)
         call crop_failure_emergence (germ, emerg, now
     :        , c%tt_emerg_limit
     :        , g%current_stage
     :        , g%plants
     :        , g%tt_tot
     :        , g%dlt_plants_all)
         call failure_leaf_sen (
     :          g%lai
     :        , g%current_stage
     :        , g%plants
     :        , g%dlt_plants_all)
         call failure_phen_delay (
     :          g%cswd_pheno
     :        , g%current_stage
     :        , c%swdf_pheno_limit
     :        , g%plants
     :        , g%dlt_plants_all)
         call death_barrenness (
     :          g%current_stage
     :        , g%days_tot
     :        , c%head_grain_no_crit
     :        , p%head_grain_no_max
     :        , c%barren_crit
     :        , g%grain_no
     :        , g%plants
     :        , g%dlt_plants_barren)
         call death_seedling (
     :          g%days_tot
     :        , g%year
     :        , g%day_of_year
     :        , g%soil_temp
     :        , c%x_weighted_temp
     :        , c%y_plant_death
     :        , c%num_weighted_temp
     :        , g%plants
     :        , g%dlt_plants_temp)
         call death_drought (
     :          g%cswd_photo
     :        , g%leaf_no
     :        , c%leaf_no_crit
     :        , c%swdf_photo_limit
     :        , g%swdef_photo
     :        , c%swdf_photo_rate
     :        , g%plants
     :        , g%dlt_plants_water)
         call millet_death_actual (
     :          g%dlt_plants_all
     :        , g%dlt_plants_temp
     :        , g%dlt_plants_water
     :        , g%dlt_plants_barren
!cnh     :         , dlt_plants
     :        , g%dlt_plants
     :            )
 
         if (reals_are_equal (g%dlt_plants + g%plants, 0.0)) then
            call millet_kill_crop (
     :          g%plant_status
     :        , g%dm_green
     :        , g%dm_senesced
     :        , g%dm_dead)
         endif
 
      elseif (Option .eq. 2) then
 
         g%dlt_plants_all = 0.0
         call crop_failure_germination (sowing, germ, now
     :        , c%days_germ_limit
     :        , g%current_stage
     :        , g%days_tot
     :        , g%plants
     :        , g%dlt_plants_all)
         call crop_failure_emergence (germ, emerg, now
     :        , c%tt_emerg_limit
     :        , g%current_stage
     :        , g%plants
     :        , g%tt_tot
     :        , g%dlt_plants_all)
         call failure_leaf_sen (
     :          g%lai
     :        , g%current_stage
     :        , g%plants
     :        , g%dlt_plants_all)
         call failure_phen_delay (
     :          g%cswd_pheno
     :        , g%current_stage
     :        , c%swdf_pheno_limit
     :        , g%plants
     :        , g%dlt_plants_all)
         call millet_death_barrenness0 (
     :          g%current_stage
     :        , g%days_tot
     :        , c%head_grain_no_crit
     :        , p%head_grain_no_max
     :        , c%barren_crit
     :        , g%grain_no
     :        , g%plants
     :        , g%dlt_plants_barren)
         call death_seedling (
     :          g%days_tot
     :        , g%year
     :        , g%day_of_year
     :        , g%soil_temp
     :        , c%x_weighted_temp
     :        , c%y_plant_death
     :        , c%num_weighted_temp
     :        , g%plants
     :        , g%dlt_plants_temp)
         call death_drought (
     :          g%cswd_photo
     :        , g%leaf_no
     :        , c%leaf_no_crit
     :        , c%swdf_photo_limit
     :        , g%swdef_photo
     :        , c%swdf_photo_rate
     :        , g%plants
     :        , g%dlt_plants_water)
         call millet_death_actual (
     :          g%dlt_plants_all
     :        , g%dlt_plants_temp
     :        , g%dlt_plants_water
     :        , g%dlt_plants_barren
!cnh     :         , dlt_plants
     :        , g%dlt_plants
     :            )
 
         if (reals_are_equal (g%dlt_plants + g%plants, 0.0)) then
            call millet_kill_crop (
     :          g%plant_status
     :        , g%dm_green
     :        , g%dm_senesced
     :        , g%dm_dead)
         endif
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine failure_leaf_sen (
     :                            g_lai
     :                          , g_current_stage
     :                          , g_plants
     :                          , dlt_plants_all)
*     ===========================================================
      use milletModule
      implicit none
      include   'const.inc'
      include 'data.pub'                          
      include 'science.pub'                       
      include 'error.pub'                         

*+  Sub-Program Arguments
       real g_lai
       real g_current_stage
       real g_plants
*
       real dlt_plants_all

*+  Purpose
*      Determine plant death due to total leaf area senescence

*+  Changes
*       290994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'failure_leaf_sen')

*+  Local Variables
      character  string*200            ! output string

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (reals_are_equal (g_lai, 0.0)
     :       .and. stage_is_between (floral_init, plant_end
     :                             , g_current_stage)) then
 
         dlt_plants_all = - g_plants
 
         write (string, '(3a)')
     :                ' crop failure because of total leaf senescence.'
         call write_string (string)
 
      endif
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine failure_phen_delay (
     :              g_cswd_pheno
     :            , g_current_stage
     :            , c_swdf_pheno_limit
     :            , g_plants
     :            , dlt_plants_all)
*     ===========================================================
      use milletModule
      implicit none
      include   'const.inc'
      include 'data.pub'                          
      include 'science.pub'                       
      include 'error.pub'                         

*+  Sub-Program Arguments
       real g_cswd_pheno(*)
       real g_current_stage
       real c_swdf_pheno_limit
       real g_plants
*
       real dlt_plants_all

*+  Purpose
*      Determine plant death due to water stress

*+  Changes
*       290994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'failure_phen_delay')

*+  Local Variables
      real cswd_pheno
      character  string*200            ! output string

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      cswd_pheno = sum_between (emerg, flag_leaf, g_cswd_pheno)
 
      if (stage_is_between (emerg, flag_leaf, g_current_stage)
     :       .and. cswd_pheno.ge.c_swdf_pheno_limit) then
 
         dlt_plants_all = - g_plants
 
         write (string, '(3a)')
     :                 '         crop failure because of prolonged'
     :                ,new_line
     :                ,'         phenology delay through water stress.'
         call write_string (string)
 
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine death_seedling (
     :            g_days_tot
     :          , g_year
     :          , g_day_of_year
     :          , g_soil_temp
     :          , c_x_weighted_temp
     :          , c_y_plant_death
     :          , c_num_weighted_temp
     :          , g_plants
     :          , dlt_plants_temp)
*     ===========================================================
      use milletModule
      implicit none
      include   'const.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
       real g_days_tot(*)
       integer g_year
       integer g_day_of_year
       real g_soil_temp(*)
       real c_x_weighted_temp(*)
       real c_y_plant_death(*)
       integer c_num_weighted_temp
       real g_plants
*
       real dlt_plants_temp

*+  Purpose
*      Determine percentage plant failure due to high temperatures.

*+  Changes
*       290994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'death_seedling')

*+  Local Variables
      integer days_after_emerg
      real       killfr                ! fraction of crop population to kill
      character  string*200            ! output string

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
!cpsc  add code to kill plants for high soil surface temperatures
 
      days_after_emerg = int (sum_between (emerg, now, g_days_tot)) - 1
      if (days_after_emerg .eq. 1) then
 
         call plants_temp (
     :          g_year
     :        , g_day_of_year
     :        , g_soil_temp
     :        , c_x_weighted_temp
     :        , c_y_plant_death
     :        , c_num_weighted_temp
     :        , killfr)
         dlt_plants_temp = - g_plants*killfr
 
         if (killfr .gt. 0.0) then
         write (string, '(a, i4, a)')
     :        'plant_kill.'
     :        , nint (killfr*100.0)
     :        , '% failure because of high soil surface temperatures.'
 
         call Write_string (string)
 
         else
                  ! do nothing
         endif
 
      else
         dlt_plants_temp = 0.0
 
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine death_drought (
     :              g_cswd_photo
     :            , g_leaf_no
     :            , c_leaf_no_crit
     :            , c_swdf_photo_limit
     :            , g_swdef_photo
     :            , c_swdf_photo_rate
     :            , g_plants
     :            , dlt_plants_water)
*     ===========================================================
      use milletModule
      implicit none
      include   'const.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
       real g_cswd_photo(*)
       real g_leaf_no(*)
       real c_leaf_no_crit
       real c_swdf_photo_limit
       real g_swdef_photo
       real c_swdf_photo_rate
       real g_plants
*
       real dlt_plants_water

*+  Purpose
*      Determine percentage plant failure due to water stress

*+  Changes
*       290994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'death_drought')

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
         dlt_plants_water = - g_plants*killfr
 
         write (string, '(a, i4, a)')
     :          'plant_kill.'
     :         , nint (killfr*100.0)
     :         , '% failure because of water stress.'
 
         call Write_string (string)
 
      else
         dlt_plants_water = 0.0
 
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine death_barrenness (
     :              g_current_stage
     :            , g_days_tot
     :            , c_head_grain_no_crit
     :            , p_head_grain_no_max
     :            , c_barren_crit
     :            , g_grain_no
     :            , g_plants
     :            , dlt_plants_barren)
*     ===========================================================
      use milletModule
      implicit none
      include   'const.inc'
      include 'science.pub'                       
      include 'error.pub'                         

*+  Sub-Program Arguments
       real g_current_stage
       real g_days_tot(*)
       real c_head_grain_no_crit
       real p_head_grain_no_max
       real c_barren_crit
       real g_grain_no
       real g_plants
*
       real dlt_plants_barren

*+  Purpose
*      Determine percent plant failure due to barreness

*+  Changes
*       290994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'death_barrenness')

*+  Local Variables
      real       killfr                ! fraction of crop population to kill
      character  string*200            ! output string

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (on_day_of (start_grain_fill
     :             , g_current_stage, g_days_tot)) then
         call plants_barren (
     :          c_head_grain_no_crit
     :        , p_head_grain_no_max
     :        , c_barren_crit
     :        , g_grain_no
     :        , g_plants
     :        , killfr)
         dlt_plants_barren = - g_plants*killfr
 
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
         dlt_plants_barren = 0.0
 
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_death_actual (
     :              g_dlt_plants_all
     :            , g_dlt_plants_temp
     :            , g_dlt_plants_water
     :            , g_dlt_plants_barren
     :            , dlt_plants
     :                       )
*     ==========================================================
      use milletModule
      implicit none
      include   'const.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      real g_dlt_plants_all
      real g_dlt_plants_temp
      real g_dlt_plants_water
      real g_dlt_plants_barren
      real dlt_plants

*+  Purpose
*      Kill the entire crop if necessary.

*+  Changes
*       290994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_death_actual')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      dlt_plants = min (g_dlt_plants_all
     :                , g_dlt_plants_temp
     :                , g_dlt_plants_water
     :                , g_dlt_plants_barren)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine plants_temp (
     :          g_year
     :        , g_day_of_year
     :        , g_soil_temp
     :        , c_x_weighted_temp
     :        , c_y_plant_death
     :        , c_num_weighted_temp
     :        , killfr)
*     ===========================================================
      use milletModule
      implicit none
      include 'science.pub'                       
      include 'error.pub'                         

*+  Sub-Program Arguments
       integer g_year
       integer g_day_of_year
       real g_soil_temp(*)
       real c_x_weighted_temp(*)
       real c_y_plant_death(*)
       integer c_num_weighted_temp
      real       killfr                ! (OUTPUT) fraction of plants killed
                                       ! (plants/m^2)

*+  Purpose
*        Calculate fraction of plants killed by high temperature during
*        emergence (0-1).

*+  Changes
*     230695 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'plants_temp')

*+  Local Variables
      integer    day_before            ! day of year number of day before
                                       ! yesterday ()
      real       weighted_temp         ! 3 day weighted soil temperature (oC)
      integer    yesterday             ! day of year number of yesterday

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      yesterday = offset_day_of_year (g_year, g_day_of_year, - 1)
      day_before = offset_day_of_year (g_year, g_day_of_year, - 2)
 
      weighted_temp = 0.25 * g_soil_temp(day_before)
     :              + 0.50 * g_soil_temp(yesterday)
     :              + 0.25 * g_soil_temp(g_day_of_year)
 
      killfr = linear_interp_real (weighted_temp
     :                           , c_x_weighted_temp
     :                           , c_y_plant_death
     :                           , c_num_weighted_temp)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine plants_barren (
     :          c_head_grain_no_crit
     :        , p_head_grain_no_max
     :        , c_barren_crit
     :        , g_grain_no
     :        , g_plants
     :        , killfr)
*     ===========================================================
      implicit none
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
       real c_head_grain_no_crit
       real p_head_grain_no_max
       real c_barren_crit
       real g_grain_no
       real g_plants
      real       killfr                ! (OUTPUT) fraction of plants killed
                                       ! (plants/m^2)

*+  Purpose
*        Calculate fraction of barren heads (0-1).
*        Allows no more than 1 head per plant.

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'plants_barren')

*+  Local Variables
      real       fract_of_optimum      ! fraction of optimum no. of heads due
                                       ! to barrenness below which some
                                       ! heads become barren. (0-1)
      real       head_grain_no         ! (grains/head)

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      call check_grain_no (
     :          c_head_grain_no_crit
     :        , p_head_grain_no_max
     :        , c_barren_crit)
 
         ! determine barrenness
 
      head_grain_no = divide (g_grain_no, g_plants, 0.0)
 
!cjh      if (head_grain_no.lt.c_head_grain_no_crit) then
      if (head_grain_no.le.c_head_grain_no_crit) then
            ! all heads barren
         fract_of_optimum = 0.0
 
      elseif (head_grain_no.lt.p_head_grain_no_max * c_barren_crit) then
            ! we have some barren heads
         fract_of_optimum =
     :              (divide (head_grain_no - c_head_grain_no_crit
     :                     , p_head_grain_no_max - c_head_grain_no_crit
     :                     , 0.0))
     :              **0.33
 
      else
            ! we have no barren heads
         fract_of_optimum = 1.0
      endif
 
      fract_of_optimum = bound (fract_of_optimum, 0.0, 1.0)
      killfr = 1.0 - fract_of_optimum
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine check_grain_no (
     :          c_head_grain_no_crit
     :        , p_head_grain_no_max
     :        , c_barren_crit)
*     ===========================================================
      implicit none
      include   'const.inc'            ! err_user
      include 'error.pub'                         

*+  Sub-Program Arguments
       real c_head_grain_no_crit
       real p_head_grain_no_max
       real c_barren_crit

*+  Purpose
*        Check validity of grain no. parameters

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'check_grain_no')

*+  Local Variables
      character  err_messg*200         ! error message

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (c_head_grain_no_crit.gt.p_head_grain_no_max*c_barren_crit
     :   .and. p_head_grain_no_max.gt.0.0) then
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
      end



*     ===========================================================
      subroutine millet_kill_crop (
     :          g_plant_status
     :        , g_dm_green
     :        , g_dm_senesced
     :        , g_dm_dead)
*     ===========================================================
      use milletModule
      implicit none
      include   'convert.inc'          ! gm2kg, sm2ha, mm2cm, cmm2cc
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
       character g_plant_status*(*)
       real g_dm_green(*)
       real g_dm_senesced(*)
       real g_dm_dead(*)

*+  Purpose
*       Kill crop

*+  Changes
*       290994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'millet_kill_crop')

*+  Local Variables
      real       biomass               ! above ground dm (kg/ha)
      character  string*200            ! output string

*- Implementation Section ----------------------------------
 
c+!!!!!! fix problem with deltas in update when change from alive to dead ?zero
      call push_routine (my_name)
 
      if (g_plant_status.eq.status_alive) then
         g_plant_status = status_dead
 
         biomass = (sum_real_array (g_dm_green, max_part)
     :           - g_dm_green(root)) * gm2kg /sm2ha
 
     :           + (sum_real_array (g_dm_senesced, max_part)
     :           - g_dm_senesced(root)) * gm2kg /sm2ha
 
     :           + (sum_real_array (g_dm_dead, max_part)
     :           - g_dm_dead(root)) * gm2kg /sm2ha
 
 
                ! report
 
         write (string, '(3x, a, f7.1, a)')
     :                  ' kill. Standing above-ground dm = '
     :                  , biomass, ' (kg/ha)'
         call Write_string (string)
 
      else
      endif
 
      call pop_routine (my_name)
      return
      end


*     ===========================================================
      subroutine millet_death_barrenness0 (
     :              g_current_stage
     :            , g_days_tot
     :            , c_head_grain_no_crit
     :            , p_head_grain_no_max
     :            , c_barren_crit
     :            , g_grain_no
     :            , g_plants
     :            , dlt_plants_barren)
*     ===========================================================
      use milletModule
      implicit none
      include   'const.inc'
      include 'science.pub'                       
      include 'error.pub'                         

*+  Sub-Program Arguments
       real g_current_stage
       real g_days_tot(*)
       real c_head_grain_no_crit
       real p_head_grain_no_max
       real c_barren_crit
       real g_grain_no
       real g_plants
*
       real dlt_plants_barren

*+  Purpose
*      Determine percent plant failure due to barreness

*+  Changes
*       290994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'death_barrenness0')

*+  Local Variables
      real       killfr                ! fraction of crop population to kill
      character  string*200            ! output string

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (on_day_of (start_grain_fill
     :             , g_current_stage, g_days_tot)) then
         call millet_plants_barren0 (
     :          c_head_grain_no_crit
     :        , p_head_grain_no_max
     :        , c_barren_crit
     :        , g_grain_no
     :        , g_plants
     :        , killfr)
         dlt_plants_barren = - g_plants*killfr
 
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
         dlt_plants_barren = 0.0
 
      endif
 
      call pop_routine (my_name)
      return
      end

*     ===========================================================
      subroutine millet_plants_barren0 (
     :          c_head_grain_no_crit
     :        , p_head_grain_no_max
     :        , c_barren_crit
     :        , g_grain_no
     :        , g_plants
     :        , killfr)
*     ===========================================================
      implicit none
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
       real c_head_grain_no_crit
       real p_head_grain_no_max
       real c_barren_crit
       real g_grain_no
       real g_plants
      real       killfr                ! (OUTPUT) fraction of plants killed
                                       ! (plants/m^2)

*+  Purpose
*        Calculate fraction of barren heads (0-1).
*        Allows no more than 1 head per plant.

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_plants_barren0')

*+  Local Variables
      real       fract_of_optimum      ! fraction of optimum no. of heads due
                                       ! to barrenness below which some
                                       ! heads become barren. (0-1)
      real       head_grain_no         ! (grains/head)

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      call check_grain_no (
     :          c_head_grain_no_crit
     :        , p_head_grain_no_max
     :        , c_barren_crit)
 
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
      end

cjh special for erik - start
*     ===========================================================
      subroutine millet_stop_growth (switch)
*     ===========================================================
      use MilletModule
      implicit none
      include 'error.pub'                         

*+  Sub-Program Arguments
      logical  switch

*+  Purpose
*       Set growth flag for this module. 

*+  Changes
*     150199 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_stop_growth')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      g%stop_growth = switch

      call pop_routine (my_name)
      return
      end subroutine

cjh special for erik - end


*     ===========================================================
      subroutine millet_detachment(option)
*     ===========================================================
      use milletModule
      implicit none
      include 'const.inc'
      include 'crp_cnpy.pub'                      
      include 'crp_nitn.pub'                      
      include 'crp_biom.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer option

*+  Purpose
*       Simulate plant detachment.
cscc Detachment is also a function of the environment. We've
c noticed large diff. in detachment between wet and dry environments
c in millet

*+  Changes
*      091294 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_detachment')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
         call cproc_dm_detachment1( max_part
     :                              , c%sen_detach_frac
     :                              , g%dm_senesced
     :                              , g%dlt_dm_detached
     :                              , c%dead_detach_frac
     :                              , g%dm_dead
     :                              , g%dlt_dm_dead_detached)
 
         call cproc_n_detachment1( max_part
     :                              , c%sen_detach_frac
     :                              , g%n_senesced
     :                              , g%dlt_n_detached
     :                              , c%dead_detach_frac
     :                              , g%n_dead
     :                              , g%dlt_n_dead_detached)
 
         call cproc_lai_detachment1 (leaf
     :                             , c%sen_detach_frac
     :                             , g%slai
     :                             , g%dlt_slai_detached
     :                             , c%dead_detach_frac
     :                             , g%tlai_dead
     :                             , g%dlt_tlai_dead_detached)
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end

*     ===========================================================
      subroutine millet_phenology (option)
*     ===========================================================
      use milletModule
      implicit none
      include 'const.inc'
      include 'science.pub'                       
      include 'crp_phen.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer option

*+  Purpose
*     Use temperature, photoperiod and genetic characteristics
*     to determine when the crop begins a new growth phase.
*     The initial daily thermal time and height are also set.

*+  Changes
*     010994 jngh specified and programmed
cscc Needs to incorporate water stress and low N effects on phenology
c usually by slowing down leaf appearance in vegetative phase
c and often hastening leaf senescence in grainfilling phase
c Water stress effect during grainfilling is partly because the canopy heats up
c more than it would if it were irrigated. Really need to predict canopy temp.
c somehow ...
c But if slow down leaf appearance etc. need to relate that to the leaf area mod
c (how do we do this w. TPLA approach?)

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_phenology')

*+  Local Variables
      real       phase_dvl           ! fraction of current phase elapsed ()

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (option .eq. 101)  then
         g%previous_stage = g%current_stage
 
            ! get thermal times
 
         call millet_tt (g%dlt_tt)
!cjh         call millet_devel (g%dlt_stage, g%current_stage)
cjh special for erik - start
      if (.not. g%stop_growth) then
         call millet_devel (g%dlt_stage, g%current_stage)
 
      else
         ! no phenological development
         g%dlt_stage = 0.0
         call accumulate (g%dlt_tt, g%phase_tt
     :               , g%previous_stage, g%dlt_stage)
      endif
cjh special for erik - end
 
 
         ! update thermal time states and day count
 
         call accumulate (g%dlt_tt, g%tt_tot
     :               , g%previous_stage, g%dlt_stage)
 
         call accumulate (1.0, g%days_tot
     :               , g%previous_stage, g%dlt_stage)
 
      else if (option .eq. 1)  then
 
         call cproc_phenology1 (
     :                             g%previous_stage
     :                            ,g%current_stage
     :                            ,sowing
     :                            ,germ
     :                            ,harvest_ripe
     :                            ,emerg
     :                            ,flag_leaf
     :                            ,max_stage
     :                            ,c%num_temp
     :                            ,c%x_temp
     :                            ,c%y_tt
     :                            ,g%maxt
     :                            ,g%mint
     :                            ,min(g%nfact_pheno
     :                                ,g%pfact_pheno)
     :                            ,g%swdef_pheno
     :                            ,c%pesw_germ
     :                            ,c%fasw_emerg     !
     :                            ,c%rel_emerg_rate !
     :                            ,c%num_fasw_emerg !
     :                            ,g%dlayer
     :                            ,max_layer
     :                            ,g%sowing_depth
     :                            ,g%sw_dep
     :                            ,g%dul_dep
     :                            ,p%ll_dep
     :                            ,g%dlt_tt
     :                            ,g%phase_tt
     :                            ,phase_dvl
     :                            ,g%dlt_stage
     :                            ,g%tt_tot
     :                            ,g%days_tot
     :                            )
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end

*     ===========================================================
      subroutine millet_phenology_init (option)
*     ===========================================================
      use milletModule
      implicit none
      include 'const.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer option

*+  Purpose
*     Initialise crop growth stage targets

*+  Changes
*     240498 igh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_phenology_init')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (option .eq. 101)  then
 
         call millet_phenology_init_o (g%phase_tt)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end

*     ===========================================================
      subroutine millet_cleanup ()
*     ===========================================================
      use milletModule
      implicit none
      include 'error.pub'                         

*+  Purpose
*       clean

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_cleanup')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      call millet_update ()
      call millet_check_bounds ()
      call millet_totals ()
      call millet_event ()

      call pop_routine (my_name)
 
      return
      end





