C     Last change:  E    19 Jan 2001   11:55 am



* ====================================================================
      subroutine Crop_Vernalisation  (current_stage
     :                               ,start_stage
     :                               ,end_stage
     :                               ,maxt
     :                               ,mint
     :                               ,num_temp_vern
     :                               ,x_temp_vern
     :                               ,y_vern_rate
     :                               ,vernalisation_requirement
     :                               ,dlt_cumvd
     :                               ,dlt_vernalisation
     :                               ,vernalisation)
* ====================================================================
      implicit none
      include 'data.pub'
      include 'error.pub'
      include 'science.pub'

*+  Sub-Program Arguments
      real    current_stage  !The current development stage
      integer start_stage      !Stage vernalisation begins
      integer end_stage        !Stage vernalisation ends
      real    maxt           !Daily maximum Temperature
      real    mint           !Daily minimum temperature
      REAL    num_temp_vern
      REAL    x_temp_vern
      REAL    y_vern_rate
      REAL    vernalisation_requirement
      REAL    dlt_cumvd
      REAL    dlt_vernalisation
      REAL    vernalisation


*+  Purpose
*     Calculate daily vernalisation and accumulate to g_cumvd
 
*+  Mission Statement
*     Calculate todays vernalization (used to affect phenology)
 
*+  Notes

*+  Changes
*     2000/02/06 ew programmed
 

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'Crop_Vernalisation')
 
*+  Local Variables
      REAL max_vern_rate
      REAL rel_vern_rate


*- Implementation Section ----------------------------------
      call push_routine (myname)


      dlt_vernalisation = 0.0

      if (stage_is_between(start_stage,end_stage
     :                    ,current_stage)) then

          !vernalisation rate depends on temperature - using 3 hour temperature
          rel_vern_rate = linint_3hrly_temp (maxt, mint,
     :                  x_temp_vern, y_vern_rate,num_temp_vern)

          dlt_cumvd = rel_vern_rate

          if (vernalisation .ge. 1.0) then !Vernalisation has finished
             rel_vern_rate = 0.0
          end if


          if (vernalisation_requirement .le. 1E-4) then
             dlt_vernalisation = 1.0
          else
             max_vern_rate = divide(1.0, vernalisation_requirement,1.0)
             dlt_vernalisation = max_vern_rate * rel_vern_rate
          end if

      else
         ! out of vernalization stage
      endif


      call pop_routine (myname)
      return
      end


*     ===========================================================
      subroutine Crop_phenology_init
     :               (
     :                 g_current_stage
     :               , c_shoot_lag
     :               , c_shoot_rate
     :               , g_sowing_depth

     :               , p_vernalisation_requirement
     :               , g_dlt_cumvd
     :               , g_dlt_vernalisation
     :               , g_vernalisation

     :               , g_day_of_year
     :               , g_latitude
     :               , c_twilight
     :               , g_cum_photoperiod
     :               , g_cum_photoperiod_day

     :               , c_use_average_photoperiod

     .               , p_photoperiod_crit1
     .               , p_photoperiod_crit2
     .               , p_photoperiod_slope

     .               , c_leaf_no_at_emerg
     .               , c_leaf_no_rate_change
     :               , c_leaf_no_min
     :               , c_leaf_no_max
     .               , g_leaf_no_final

     .               , c_leaf_app_rate0
     .               , c_leaf_app_rate1
     .               , c_leaf_app_rate2

     .               , p_tt_germ_to_emerg
     .               , p_tt_emerg_to_endjuv
     .               , p_tt_endjuv_to_init
     .               , p_tt_init_to_flag
     .               , p_tt_flag_to_flower
     .               , p_tt_flower_to_start_grain
     .               , p_tt_start_to_end_grain
     .               , p_tt_end_grain_to_maturity
     .               , p_tt_maturity_to_ripe
     .               , p_tt_ripe_to_harvest

     :               , g_days_tot
     :               , g_tt_tot
     :               , g_phase_tt
     :               )
*     ===========================================================
      implicit none
      include 'CropDefCons.inc'
      include 'science.pub'
      include 'data.pub'                       
      include 'error.pub'


*+  Sub-Program Arguments
      REAL       g_current_stage
      REAL       c_shoot_lag
      REAL       c_shoot_rate
      REAL       g_sowing_depth

      REAL       p_vernalisation_requirement
      REAL       g_dlt_cumvd
      REAL       g_dlt_vernalisation
      REAL       g_vernalisation

      integer    g_day_of_year
      REAL       g_latitude
      REAL       c_twilight
      REAL       g_cum_photoperiod
      REAL       g_cum_photoperiod_day

      INTEGER    c_use_average_photoperiod

      REAL       p_photoperiod_crit1
      REAL       p_photoperiod_crit2
      REAL       p_photoperiod_slope

      REAL       c_leaf_no_at_emerg
      REAL       c_leaf_no_rate_change
      REAL       c_leaf_no_min
      REAL       c_leaf_no_max
      REAL       g_leaf_no_final

      REAL       c_leaf_app_rate0
      REAL       c_leaf_app_rate1
      REAL       c_leaf_app_rate2

      REAL       p_tt_germ_to_emerg
      REAL       p_tt_emerg_to_endjuv
      REAL       p_tt_endjuv_to_init
      REAL       p_tt_init_to_flag
      REAL       p_tt_flag_to_flower
      REAL       p_tt_flower_to_start_grain
      REAL       p_tt_start_to_end_grain
      REAL       p_tt_end_grain_to_maturity
      REAL       p_tt_maturity_to_ripe
      REAL       p_tt_ripe_to_harvest

      REAL       g_days_tot(*)
      REAL       g_tt_tot(*)
      REAL       g_phase_tt(*)



*+  Purpose
*       Returns cumulative thermal time targets required for the
*       individual growth stages.

*+  Changes
*     <insert here>

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Crop_phenology_init')

*+  Local Variables

       REAL photoperiod
       REAL leaf_no
       REAL tt_emerg_to_flag_leaf

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)

* On the sowing day, calculate the tt for emergence
      if (on_day_of (sowing, g_current_stage, g_days_tot)) then

       if (p_tt_germ_to_emerg.le.0.0) then
          g_phase_tt(germ_to_emerg) = c_shoot_lag
     :                         + g_sowing_depth*c_shoot_rate
       else
          g_phase_tt(germ_to_emerg) = p_tt_germ_to_emerg
       end if

       !This is to avoid a varning in leaf number final
       g_phase_tt(emerg_to_endjuv)       = p_tt_emerg_to_endjuv
       g_phase_tt(endjuv_to_init)        = p_tt_endjuv_to_init
       g_phase_tt(init_to_flag)          = p_tt_init_to_flag
       g_phase_tt(flag_to_flower)        = p_tt_flag_to_flower
       g_phase_tt(flower_to_start_grain) = p_tt_flower_to_start_grain
       g_phase_tt(start_to_end_grain)    = p_tt_start_to_end_grain
       g_phase_tt(end_grain_to_maturity) = p_tt_end_grain_to_maturity
       g_phase_tt(maturity_to_ripe)      = p_tt_maturity_to_ripe
       g_phase_tt(ripe_to_harvest)       = p_tt_ripe_to_harvest

* On the day of emergence,make an estimate of phase duration for endjuv to floral init  
      elseif (stage_is_between(endjuv,floral_init,g_current_stage)) then


         photoperiod = day_length (g_day_of_year,
     :                             g_latitude,
     :                             c_twilight)

         g_cum_photoperiod_day = g_cum_photoperiod_day +1
         g_cum_photoperiod     = g_cum_photoperiod     + photoperiod

         if (c_use_average_photoperiod .gt. 0.0) then
             photoperiod = divide(g_cum_photoperiod,
     :                            g_cum_photoperiod_day, 0.0)
         end if


         if (photoperiod.le.p_photoperiod_crit1) then
            g_phase_tt(endjuv_to_init) = p_tt_endjuv_to_init
         elseif (photoperiod.lt.p_photoperiod_crit2) then
            g_phase_tt(endjuv_to_init) = p_tt_endjuv_to_init +
     :      p_photoperiod_slope*(photoperiod - p_photoperiod_crit1)
         elseif (photoperiod.ge.p_photoperiod_crit2) then
            g_phase_tt(endjuv_to_init) = p_tt_endjuv_to_init +
     :      p_photoperiod_slope*(p_photoperiod_crit2
     :                              - p_photoperiod_crit1)
         endif

         leaf_no = max (g_leaf_no_final - c_leaf_no_rate_change,
     :                 c_leaf_no_at_emerg)

         leaf_no = min (leaf_no, g_leaf_no_final)

         tt_emerg_to_flag_leaf = (leaf_no - c_leaf_no_at_emerg)
     :                         * c_leaf_app_rate1
     :                         + (g_leaf_no_final - leaf_no)
     :                         * c_leaf_app_rate2
 
         g_phase_tt(init_to_flag) = tt_emerg_to_flag_leaf
     :                            - g_phase_tt(emerg_to_endjuv)
     :                            - g_phase_tt(endjuv_to_init)

      else
 
      endif


      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine Crop_dm_init (
     .          g_current_stage,
     .          g_days_tot,
     .          g_plants,
     .          c_dm_root_init,
     .          c_dm_stem_init,
     .          c_dm_leaf_init,
     .          c_dm_seed_reserve,
     .          c_dm_grain_embryo,
     .          c_stem_trans_frac,
     .          c_leaf_trans_frac,
     .          g_dm_green,
     .          g_dm_plant_min,
     .          g_dm_seed_reserve,
     .          g_obs_grain_no_psm,
     .          g_dm_green_grainno,
     .          p_grain_num_coeff,
     .          g_grain_no,
     .          g_dm_green_retrans_pool )
*     ===========================================================
      implicit none
      include 'CropDefCons.inc'
      include 'science.pub'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
       real g_current_stage     !(INPUT) current phenological stage
       real g_days_tot(*)       !(INPUT) total days accumulated in each stage
       real g_plants            !(INPUT) plant density (plants/m^2)
       real c_dm_root_init      !(INPUT) initial root weight per plant at emergence (g/plant)
       real c_dm_stem_init      !(INPUT) initial stem weight per plant at emergence (g/plant)
       real c_dm_leaf_init      !(INPUT) initial leaf weight per plant at emergence (g/plant)
       REAL c_dm_seed_reserve   !(INPUT) weight of seed reserves at emergence (g/plant)
       real c_dm_grain_embryo   !(INPUT) grain embryo weight at start of grain filling (g/grain)
       real c_stem_trans_frac   !(INPUT) fraction of stem dm can be translocated to grain
       real c_leaf_trans_frac   !(INPUT) fraction of leaf dm can be translocated to grain
       real g_dm_green(*)       !(INPUT/OUTPUT) plant part weights (g/m^2)
       real g_dm_plant_min(*)   !(OUTPUT) minimum weight of each plant part (g/plant)
       real g_dm_seed_reserve   !(OUTPUT) seed reserve weight (g/m^2)
       REAL g_obs_grain_no_psm  !
       REAL g_dm_green_grainno
       REAL p_grain_num_coeff   !(INPUT) grain number per g stem (grains/g stem)
       REAL g_grain_no          !(OUTPUT) grain number per square meter (grains/m^2)
       REAL g_dm_green_retrans_pool(*)


*+  Purpose
*       Initialise plant weights and plant weight minimums at required instances.

*+  Changes
*    19940109 jngh specified and programmed
*    19970317 slw new template form
*    20000818 ew reprogrammed


*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Crop_dm_init')

*+  Local Variables
      real       dm_plant_leaf         ! dry matter in leaves (g/plant)
      real       dm_plant_stem         ! dry matter in stems (g/plant)
      real       dm_stem_retrans

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
         ! initialisations - set up dry matter for leaf, stem, flower, grain! and root

      if (on_day_of (emerg, g_current_stage, g_days_tot)) then
         ! seedling has just emerged. initialise root, stem and leaf.
 
         g_dm_green(root)  = c_dm_root_init * g_plants
         g_dm_green(stem)  = c_dm_stem_init * g_plants
         g_dm_green(leaf)  = c_dm_leaf_init * g_plants
         g_dm_green(grain) = 0.0
         g_dm_green(flower)= 0.0

         g_dm_seed_reserve = c_dm_seed_reserve * g_plants       ! (g/m2)   !  ew

      !   g_dm_green_grainno = g_dm_green(stem)

c for nwheat min stem weight at beginning of grain filling stage, no carbon mobile from leaves
      elseif (on_day_of (start_grain_fill
     :                 , g_current_stage, g_days_tot)) then
 
         ! we are at first day of grainfill.
         ! set the minimum weight of leaf; used for translocation to grain and stem
 
         dm_plant_leaf       = divide (g_dm_green(leaf), g_plants, 0.0)
c        g_dm_plant_min(leaf)= dm_plant_leaf * (1.0 - c_leaf_trans_frac)
         g_dm_plant_min(leaf)= dm_plant_leaf -
     :              divide(g_dm_green_retrans_pool(leaf)
c    :               + c_dm_leaf_init * g_plants
     :               , g_plants, 0.0)
     :              * c_leaf_trans_frac

         dm_plant_stem       = divide (g_dm_green(stem), g_plants, 0.0)
c        g_dm_plant_min(stem)= dm_plant_stem * (1.0 - c_stem_trans_frac)
         g_dm_plant_min(stem)= dm_plant_stem -
     :              divide(g_dm_green_retrans_pool(stem)
c    :               + c_dm_stem_init * g_plants
     :              , g_plants, 0.0)
     :              * c_stem_trans_frac



         ! Initial grain weigth is taken from this immobile stem as simplification to
         ! having grain filling prior to grain filling.
         ! In Nwheat stem did not include leaf sheath and so a leaf sheath approximation is removed below.

         if (g_obs_grain_no_psm .ne. 0.0) then
             g_grain_no = g_obs_grain_no_psm
         else
             g_grain_no = p_grain_num_coeff * g_dm_green_grainno  !g_dm_green(stem)
         end if

         dm_stem_retrans      = g_dm_green(stem) * c_stem_trans_frac

         g_dm_green(grain)    = min(c_dm_grain_embryo*g_grain_no
     :                             ,dm_stem_retrans)
         g_dm_green(stem)     = g_dm_green(stem) - g_dm_green(grain)

         g_dm_plant_min(grain)= g_dm_green(grain)/ g_plants
         g_dm_plant_min(stem) = g_dm_plant_min(stem)
     :                        - g_dm_plant_min(grain)

      else   ! no changes
      endif
 
      call pop_routine (my_name)
      return
      end


*     ===========================================================
      subroutine cereal_leaf_number_final (
     .           start_leaf_init_stage,
     .           end_leaf_init_stage,
     .           leaf_no_reset_stage,
     .           g_current_stage,
     .           g_days_tot,
     .           g_phase_tt,
     .           c_leaf_init_rate,
     .           c_leaf_no_seed,
     .           c_leaf_no_min,
     .           c_leaf_no_max,
     .           g_leaf_no_final)
*     ===========================================================
      implicit none
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    start_leaf_init_stage !stage to begin (e.g. emerg) est. final leaf no.
      integer    end_leaf_init_stage   !stage to end (e.g. floral_init) est. final leaf no.
      integer    leaf_no_reset_stage   !stage to reset final leaf no.
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

*+  Changes
*     010994    jngh specified and programmed
*     070495    psc  changed from emerg to germ
*     0596      glh  fixed it up
*     20000818  ew   templated from sorg_leaf_no_final

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cereal_leaf_number_final1')

*+  Local Variables
      real       tt_floral_init        ! cumulative dtt from sowing
                                       ! to true floral initiation (deg day)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 

      if (stage_is_between(start_leaf_init_stage,
     .                     end_leaf_init_stage,
     .                     g_current_stage) .or.
     .    on_day_of       (end_leaf_init_stage,
     .                     g_current_stage,
     .                     g_days_tot))     then
 
      ! estimate the final leaf no from an approximated thermal
      ! time for the period from emergence to floral initiation.
 
        tt_floral_init = sum_between(start_leaf_init_stage,
     .                               end_leaf_init_stage,
     .                               g_phase_tt)
 
        g_leaf_no_final = divide (tt_floral_init
     :                         , c_leaf_init_rate, 0.0)
     :                   + c_leaf_no_seed
 
         call bound_check_real_var (g_leaf_no_final
     :                            , c_leaf_no_min, c_leaf_no_max
     :                            , 'g_leaf_no_final')
 
      elseif (on_day_of (leaf_no_reset_stage,
     .                    g_current_stage, g_days_tot)) then
         g_leaf_no_final = 0.0
 
      endif
      call pop_routine (my_name)
      return
      end



*======================================================================
      subroutine Crop_Leaf_Initiation(
     .          start_leaf_init,
     .          end_leaf_init,
     .          reset_stage,
     .          dlt_tt,
     .          current_stage,
     .          days_tot,
     .          leaf_init_rate,
     .          leaf_no_min,
     .          leaf_no_max,
     .          leaf_no_final,
     .          leaf_primodia,
     .          dlt_leaf_primodia)
*======================================================================
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


      implicit none
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    start_leaf_init !stage to begin (e.g. emerg) est. final leaf no.
      integer    end_leaf_init   !stage to end (e.g. floral_init) est. final leaf no.
      integer    reset_stage     !stage to reset final leaf no.
      real       dlt_tt
      real       current_stage
      real       days_tot(*)
      real       leaf_init_rate
      real       leaf_no_min
      real       leaf_no_max
      real       leaf_no_final         ! (OUTPUT) maximum total leaf number
      REAL       leaf_primodia
      REAL       dlt_leaf_primodia

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Crop_Leaf_Initiation')

*+  Local Variables

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)


      if (stage_is_between(start_leaf_init,end_leaf_init,current_stage)
     .  .or. on_day_of (end_leaf_init, current_stage, days_tot)) then
 
          ! estimate the final leaf no from an approximated thermal
          ! time for the period from emergence to floral initiation.

        dlt_leaf_primodia =  divide(dlt_tt, leaf_init_rate, 0.0)


        if (leaf_primodia .gt. leaf_no_final) then
           dlt_leaf_primodia = 0.0 !leaf_no_final - leaf_primodia
        end if

      else

        dlt_leaf_primodia = 0.0

      endif


c      call bound_check_real_var (leaf_no_final
c     :                            , leaf_no_min, leaf_no_max
c     :                            , 'leaf_no_final')
 

      if (on_day_of (reset_stage, current_stage, days_tot)) then
         leaf_no_final = 0.0

      endif

      call pop_routine (my_name)
      return
      end


*======================================================================
      subroutine crop_leaf_appearance (
     .          g_leaf_no,
     .          g_leaf_no_final,
     .          c_leaf_no_rate_change,
     .          c_leaf_app_rate2,
     .          c_leaf_app_rate1,
     .          g_current_stage,
     .          g_days_tot,
     .          g_dlt_tt,
     .          g_dlt_leaf_no)
*======================================================================
      implicit none
      include   'CropDefCons.inc'
      include 'science.pub'
      include 'data.pub'                          
      include 'error.pub'                         

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
*       20000818   ew templated


*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'crop_leaf_appearance')

*+  Local Variables
      real       leaf_no_remaining     ! number of leaves to go before all are fully expanded
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
      end










**********************************************************************************

* SUBROUTINES NEW TO THE CROP PROCESS LIBRARY
* SUBROUTINES NEW TO THE CROP PROCESS LIBRARY
* SUBROUTINES NEW TO THE CROP PROCESS LIBRARY
* SUBROUTINES NEW TO THE CROP PROCESS LIBRARY

**********************************************************************************





*     ===========================================================
      subroutine zadok_stage_decimal_code(
     .          emerg,
     .          now,
     .          max_stage,
     .          zadok_code_list,
     .          current_stage,
     .          phase_tt,
     .          tt_tot,
     .          leaf_no,
     .          tiller_no,
     .          zadok_stage)
*     ===========================================================
      implicit none
      !dll_export zadok_stage_decimal_code
      include 'convert.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      INTEGER    emerg
      INTEGER    now
      INTEGER    max_stage
      real       zadok_code_list(*)
      real       current_stage
      real       phase_tt(*)
      real       tt_tot(*)
      real       leaf_no
      real       tiller_no
      real       zadok_stage

*+  Purpose
*       Return the Zadok stage code estimated from APSIM stages and leaf/tiller numbers

*+  Changes
*      20001129  ew generalised

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'zadok_stage_decimal_code')

*+  Local Variables
      INTEGER istage
      REAL    leaf_no_now
      REAL    tt_frac
      REAL    zk_dist
      REAL    zk_sum



*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
          !Crop not in the field
          if (current_stage .lt.1.0 .and. current_stage.gt.12.0) then
             zadok_stage = 0.0
          end if

          !Sowing to emergence
          if (current_stage .ge.1.0 .and. current_stage.le.3.0) then
             zadok_stage = 5.0 * (current_stage -1.0)
          end if


          !Emergence to flag leaf
          if (current_stage .gt.3.0 .and. current_stage.lt.6.0 ) then


            if (tiller_no.le.1.0) then
                leaf_no_now = sum_between (emerg, now, leaf_no)
                zadok_stage = 10.0 + leaf_no_now
            else
                zadok_stage = 20.0 + tiller_no -1.0
            end if

            istage = INT(current_stage)
            tt_frac = divide(tt_tot(istage), phase_tt(istage), 1.0)

            if (current_stage .gt. 5.0 .and. tt_frac .ge. 0.5) then
                zadok_stage = 30.0 + 10.0*2.0*(tt_frac - 0.5)
            end if

          end if

          ! stage_code = 1   2   3    4      5     6     7    8    9    10   11   12
          ! Zadok_stage= 0   5  10   10     15    40    60   71   87    90   93  100

          !Flag leaf  to maturity
          if (current_stage .ge.6.0 .and. current_stage.lt.12.0 ) then

             zk_sum = sum_real_array (zadok_code_list, max_stage)

             if (zk_sum .lt. 80.0) then
                 zadok_code_list( 6) =  40.0
                 zadok_code_list( 7) =  60.0
                 zadok_code_list( 8) =  71.0
                 zadok_code_list( 9) =  87.0
                 zadok_code_list(10) =  90.0
                 zadok_code_list(11) =  93.0
                 zadok_code_list(12) = 100.0
             end if

             istage = INT(current_stage)
             tt_frac = divide(tt_tot(istage), phase_tt(istage), 1.0)
             tt_frac = MIN(1.0, tt_frac)

             zk_dist = zadok_code_list(istage+1)
     :               - zadok_code_list(istage)

             zadok_stage = zadok_code_list(istage)
     :                   + zk_dist * tt_frac
           end if


      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine crop_extinction_coefficient
     :          (crop_type,
     :           flowering,
     :           g_current_stage,
     :           g_lai,
     :           c_x_extinct_coeff_lai,
     :           c_y_extinct_coeff_lai,
     :           c_num_extinct_coeff_lai,
     :           c_extinct_coeff_post_anthesis,
     :           g_extinction_coeff)
*     ===========================================================
      implicit none
      include 'const.inc'
      include 'error.pub'
      include 'science.pub'
      include 'data.pub'

*+  Sub-Program Arguments
      CHARACTER crop_type*(*)
      integer   flowering
      real      g_current_stage
      real      g_lai
      real      c_x_extinct_coeff_lai(*)
      real      c_y_extinct_coeff_lai(*)
      integer   c_num_extinct_coeff_lai
      REAL      c_extinct_coeff_post_anthesis
      real      g_extinction_coeff

*+  Purpose
*     light supply

*+  Changes
*     5/9/96 dph
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'crop_extinction_coefficient')

*+  Local Variables
      real extinct_coef

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 

      !WHEAT CROP
      if (crop_type .eq. 'wheat') then
      
        extinct_coef = linear_interp_real(g_lai
     :                                   ,c_x_extinct_coeff_lai
     :                                   ,c_y_extinct_coeff_lai
     :                                   ,c_num_extinct_coeff_lai)

         if (g_current_stage.lt. real(flowering)) then
             g_extinction_coeff = extinct_coef
         else
             g_extinction_coeff = c_extinct_coeff_post_anthesis   !0.42
         end if



      !SUNFLOWER CROP
      else if (crop_type .eq. 'sunflower') then

        !EW extinction coefficient needs value when lai is small
        !   extinction coefficient should not increase after anthesis

        if (g_lai.gt.0.0) then
            extinct_coef = 3.76 * g_lai ** (-0.81)   !!!MCW
        else
            extinct_coef = 0.5
        endif
        
        extinct_coef = bound(extinct_coef, 0.4, 1.0)

        if (g_current_stage .le. REAL(flowering))
     :     c_extinct_coeff_post_anthesis = extinct_coef
        
        
        if (g_current_stage .ge. REAL(flowering))
     :     extinct_coef = c_extinct_coeff_post_anthesis
      

        g_extinction_coeff = extinct_coef
        

      else
         call Fatal_error (ERR_internal, 'Invalid crop type')
      endif
 
      call pop_routine (my_name)
      return
      end




