C     Last change:  E    18 Dec 2000    2:53 pm

*     ===========================================================
      subroutine cproc_transp_eff_co2(svp_fract,
     :                                transp_eff_cf,
     :                                current_stage,
     :                                maxt,
     :                                mint,
     :                                co2level,
     :                                co2_level_te,
     :                                te_co2_modifier,
     :                                num_co2_level_te,
     :                                transp_eff)
*     ===========================================================
      implicit none
      !dll_export cproc_transp_eff_co2
      include 'convert.inc'  ! g2mm, mb2kpa
      include 'science.pub'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      REAL       svp_fract           ! (INPUT)  fraction of distance between svp at mi
      REAL       transp_eff_cf(*)    ! (INPUT)  transpiration efficiency coefficien
      REAL       current_stage       ! (INPUT)  current phenological stages
      REAL       maxt                ! (INPUT)  maximum air temperature (oC)
      REAL       mint                ! (INPUT)  minimum air temperature (oC)
      REAL       co2level            ! (INPUT)  current co2 level (ppm)
      REAL       co2_level_te(*)     ! (INPUT)  co2 levels (ppm)
      REAL       te_co2_modifier(*)  ! (INPUT)  te modifiers of co2 levels (0-1)
      INTEGER    num_co2_level_te    ! (INPUT)  number of table elements in co2-te modifier table
      REAL       transp_eff          ! (OUTPUT) transpiration coefficient

*+  Purpose
*       Calculate today's transpiration efficiency from min,max temperatures and co2 level
*       and converting mm water to g dry matter (g dm/m^2/mm water)

*+  Mission Statement
*   Calculate today's transpiration efficiency from VPD

*+  Assumptions
*       the temperatures are > -237.3 oC for the svp function.
*       if co2_level=0.0 then co2_level=350ppm

*+  Notes
*       Average saturation vapour pressure for ambient temperature
*       during transpiration is calculated as part-way between that
*       for minimum temperature and that for the maximum temperature.
*       Tanner & Sinclair (1983) used .75 and .67 of the distance as
*       representative of the positive net radiation (rn).  Daily SVP
*       should be integrated from about 0900 hours to evening when Radn
*       becomes negative.

*+  Changes
*       20000721 ew developed from crop_transp_eff1 and added co2 effect on transp_eff

*+  Constant Values
      character  my_name*(*)   ! name of procedure
      parameter (my_name = 'cproc_transp_eff_co2')

*+  Local Variables
      real       svp           ! function to get saturation vapour pressure for a given temperature in oC (kpa)
      real       temp_arg      ! dummy temperature for function (oC)
      real       vpd           ! vapour pressure deficit (kpa)
      integer    current_phase
      REAL       co2_modifier
*

      svp(temp_arg) = 6.1078
     :              * exp (17.269*temp_arg/ (237.3 + temp_arg))
     :              * mb2kpa

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      current_phase = int(current_stage)
 
      !get vapour pressure deficit when net radiation is positive.
 
      vpd = svp_fract* (svp (maxt) - svp (mint))
      vpd = l_bound (vpd, 0.01)

      transp_eff = divide (transp_eff_cf(current_phase), vpd, 0.0) /g2mm


      if (co2level .lt. 1.0) then
          co2_modifier =1.0
      else
          co2_modifier = linear_interp_real(co2level,
     :                                  co2_level_te,
     :                                  te_co2_modifier,
     :                                  num_co2_level_te)
      end if

      transp_eff = transp_eff *co2_modifier
 
      call pop_routine (my_name)
      return
      end


*     ===========================================================
      subroutine cproc_rue_co2_modifier(
     :                 crop_type,
     :                 co2,
     :                 maxt,
     :                 mint,
     :                 modifier)
*     ===========================================================
      implicit none
!      dll_export cproc_transp_eff1
      include 'data.pub'
      include 'error.pub'                         

*+  Sub-Program Arguments
      CHARACTER  crop_type *(*)
      REAL       co2
      REAL       maxt
      REAL       mint
      REAL       modifier


*+  Purpose
*     Calculation of the CO2 modification on rue

*+  References
*     Reyenga, Howden, Meinke, Mckeon (1999), Modelling global change impact on wheat cropping in
*              south-east Queensland, Australia. Enivironmental Modelling & Software 14:297-306
*
*+  Changes
*     20000717   ew programmed

*+  Constant Values
      character  my_name*(*)   ! name of procedure
      parameter (my_name = 'cproc_rue_co2_modifier')

*+  Local Variables
      REAL       temp  !daily average temperature (C)
      real       TT    !co2 compensation point (ppm)
      CHARACTER  croptype*(5)


*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if     ((crop_type.eq.'wheat')     .or.
     :        (crop_type.eq.'barley')    .or.
     :        (crop_type.eq.'sunflower')) then

              croptype = 'c3'

      elseif ((crop_type.eq.'maize')     .or.
     :        (crop_type.eq.'sorghum')   .or.
     :        (crop_type.eq.'millet')) then

          croptype = 'c4'

      else
          croptype = 'c3'

      endif



      if (croptype .eq. 'c3') then  !C3 plants

           temp = 0.5*(maxt + mint)
           TT  = divide(163.0 - temp, 5.0 - 0.1*temp, 0.0)
           modifier = divide( (co2 -    TT)*(350.0 + 2.0*TT),
     :                        (co2 +2.0*TT)*(350.0 -     TT), 1.0)

      else

           modifier = 0.000143 * co2 + 0.95 !Mark Howden, personal communication

      end if


      if (co2 .lt. 0.1) then !assuming the switch is not on
          modifier = 1.0
      end if


      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine crop_dm_pot_rue_co2 (current_stage,
     .                                rue,
     .                                radn_int,
     .                                temp_stress_photo,
     .                                nfact_photo,
     .                                co2_modifier,
     .                                dlt_dm_pot)
*     ===========================================================
       implicit none
       !dll_export crop_dm_pot_rue_co2
       include 'error.pub'

*+  Sub-Program Arguments
       real current_stage
       real rue(*)
       real radn_int
       real temp_stress_photo
       real nfact_photo
       REAL co2_modifier
       real dlt_dm_pot           ! (OUTPUT) potential dry matter
                                       ! (carbohydrate) production (g/m^2)

*+  Purpose
*       Potential biomass (carbohydrate) production from
*       photosynthesis (g/m^2)

*+  Mission Statement
*   Calculate the water non-limiting biomass production (referred to as %6)

*+  Changes
*       090994 jngh specified and programmed
*       970317 slw templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'crop_dm_pot_rue_co2')

*+  Local Variables
      integer    current_phase         ! current phase number
      real       usrue                   ! radiation use efficiency under
                                       ! no stress (g biomass/mj)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      current_phase = int (current_stage)
      usrue = rue(current_phase) * co2_modifier *
     .            min(temp_stress_photo, nfact_photo)
 
         ! potential dry matter production with temperature
         ! and N content stresses is calculated.
         ! This is g of dry biomass produced per MJ of intercepted
         ! radiation under stressed conditions.
 
      dlt_dm_pot = usrue * radn_int
 
      call pop_routine (my_name)
      return
      end



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
     .          g_grain_no )
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
         g_dm_plant_min(leaf)= dm_plant_leaf * (1.0 - c_leaf_trans_frac)
        
         dm_plant_stem       = divide (g_dm_green(stem), g_plants, 0.0)
         g_dm_plant_min(stem)= dm_plant_stem * (1.0 - c_stem_trans_frac)

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
      subroutine cproc_RUE_N_Gradients
     :               (
     :                day,
     :                latitude,
     :                radiation,
     :                tempmax,
     :                tempmin,
     :                lai_green,
     :                sln_gradient,
     :                pmaxmax,
     :                shadow_projection,
     :                biomass_conversion,
     :                scatter_coeff,
     :                rue_sln)
*     ===========================================================
      implicit none
      dll_export cproc_RUE_N_Gradients
      include   'const.inc'
      include   'cmxlayer.inc'
      include   'science.pub'
      include   'data.pub'
      include   'error.pub'

*+  Purpose
*       Return the daily RUE of a crop canopy

*+  Mission Statement

*	Program to calculate RUE from SLN and LAI profiles for peanut
*	taking account of sun angle and light intensity during the day.
*	Uses Gaussian integration to determine daily RUE.

*+  Changes
*       20001001 ew specified and programmed


*+  Sub-Program Arguments
      INTEGER   day                 !day of the year
      REAL      latitude            !latitude in degree
      REAL      radiation           !daily global radiation (MJ/m2/d)
      REAL      tempmax             !daily maximum tempeature (C)
      REAL      tempmin             !daily minimum tempeature (C)
      REAL      lai_green           !leaf area index (-)
      REAL      sln_gradient        !SLN gradients in canopy (g N/m2 leaf)
      REAL      pmaxmax             !potential assimilation rate (SLN ASYMPTOTE) (mg CO2/m2.s)
      REAL      shadow_projection   !shadow projection (=0.5)
      REAL      biomass_conversion  !biomass coversion for biochemical coversion and maintenance respiration (mg DM / mgCO2)
      REAL      scatter_coeff       !scattering coefficients (=0.15)
      REAL      rue_sln             !rue based on SLN gradients in the canopy (g DM / MJ)

*+  Constant Values

      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_RUE_N_Gradients')

      REAL       PI
      parameter (PI = 3.14159)

*+  Local Variables

c	REAL LAI(5), LAISUN(5), LAISH(5), ISUN(5), ISH(5), ITOT, IDIR,
c     1		IDIF, PMAX(5), K, LAICAN, IMAX, LAT, NEWLAT
c	DIMENSION SLN(5), SUMLAI(5), SUMF(5), F(5), SLAISN(5),
c     1 	CSUN(5), CSH(5), RUE(5), BIOMAS(3), RADINT(3), DIR(3), DIF(3)


*	Glossary of variables (mostly defined by canopy layer)

	real RUE(5)     ! radiation use efficiency g/MJ; instantaneous
	real RUEDAY     ! RUE for whole canopy for the day g/MJ
	real SLNGRAD    ! slope of SLN vs SUMLAI line
	real SLN(5)     ! specific leaf nitrogen mg N/m2 leaf area
	real AVSLN      ! average SLN for canopy mg N/m2 leaf area
	real LAI(5)     ! leaf area index m2 leaf/m2 ground
	real LAICAN     ! LAI of whole canopy
	real LAISUN(5)  ! sunlit LAI
	real LAISH(5)   ! shaded LAI
	real IMAX       ! solar noon total incoming solar radiation MJ/m2/s
	real ITOT       ! total incoming solar radiation MJ/m2/s
	real IDIR       ! direct beam incoming solar radiation MJ/m2/s
	real DIRRAD     ! direct beam solar radiation MJ/m2/day
	real IDIF       ! diffuse incoming solar radiation MJ/m2/s
	real DIFRAD     ! diffuse solar radiation MJ/m2/day
	real ISUN(5)    ! light intensity on sunlit leaves MJ/m2/s/m2 leaf area
	real ISH(5)     ! light intensity on shaded leaves MJ/m2/s/m2 leaf area
	real PMAX(5)    ! asymptote of photosynthesis-light response curve mg/m2 leaf area/s
	real SUMLAI(5)  ! cumulative LAI from top of canopy m2/m2
	real SUMF(5)    ! proportion light intercepted to that point in the canopy
	real F(5)       ! proportion light intercepted in canopy layer
	real SLAISN(5)  ! cumulative sunlit LAI from top of canopy m2/m2
	real CSUN(5)    ! photosynthesis by sunlit leaves mg/m2/s
	real CSH(5)     ! photosynthesis by shaded leaves mg/m2/s
	real K          ! extinction coefficient
	real G          ! shade projection coefficient
	real ALPHA      ! sun angle (solar elevation above horizon)
	real B          ! conversion coefficient for CHO to biomass g/g
	real BIOMAS(3)  ! canopy assimilation g/m2/sec
	real BIO        ! canopy assimilation g/m2/day
	real RADINT(3)  ! canopy radiation interception MJ/m2/sec
	real RAD        ! canopy radiation interception MJ/m2/day
	real SCAT       ! leaf light scattering coefficient
	real A          ! Asymptote of Pmax - SLN response curve
	real TIME       ! time of day expressed as fraction of daylength
	real SOLAR      ! extraterrestrial global radiation (MJ/m2/day)
	real RATIO      ! radiation attenuation factor for atmosphere; 0.75 if sunny
	real DAYL       ! daylength (hours)
	real LAT        ! latitude (degrees); negative for south
        REAL SOLARDEC   ! solar declination

        INTEGER II
        INTEGER J
        INTEGER L
        INTEGER ITIME
        REAL    NEWLAT
        REAL    ALPHA1
        REAL    SOLAR1
        REAL    CON
        REAL    SLNTOP
        REAL    SLNBOT
        REAL    DIR(3)
        REAL    DIF(3)



	
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)


	SLNGRAD = sln_gradient
	G       = shadow_projection
	B       = biomass_conversion
	SCAT    = scatter_coeff
	A       = pmaxmax
	LAT     = latitude

        LAI(:)  = LAI_green/5.0

********************************************************************
*THE FOLLOWING PART ARE ADAPTED FROM GRAEME HAMMER'S CODE
********************************************************************

C	Set up loop for radiation levels by varying RATIO and DAY
C
	DO 90 II=1,6
        RATIO = 0.25 + 0.1*FLOAT(II-1)
C
C  	Reduce ratio by 60% for inside a glasshouse
C
C	RATIO = 0.60*RATIO
C	RATIO = 0.25
C
C	Output iteration attributes and headings
C
*	WRITE(20,25) LAT,RATIO,DAY,SLNGRAD
*25	FORMAT(1X,6HLAT = ,F6.1,5X,8HRATIO = ,F5.2,5X,6HDAY = ,F6.1,
*     1	5X,10HSLNGRAD = ,F6.2/1X,46H ETRAD  RADN   DIR   DIF    IMAX   A
*     2VSLN  RUE /)
C
C	Set up loop for average canopy SLN - 0.7 to 3.1 g/m2
C
	DO 80 J=1,9
	AVSLN = 0.7 + 0.3*FLOAT(J-1)
C
C	Set up loop for Gaussian integration of diurnal response 
C	using three times of day
C
	DO 65 ITIME = 1,3

	TIME = 0.06 + 0.19*FLOAT(ITIME-1)
C
C	Calculate global radiation from latitude, day of year, and time of day
C
	NEWLAT = (PI/180)*LAT
	SOLARDEC = (PI/180) * 23.45 * SIN(2*PI*(284+DAY)/365)
	DAYL = ACOS(-TAN(NEWLAT) * TAN(SOLARDEC))
	SOLAR = 24*3600*1360 * (DAYL*SIN(NEWLAT)*SIN(SOLARDEC) +
     1		COS(NEWLAT)*COS(SOLARDEC)*SIN(DAYL)) / (PI*1000000)
	DAYL = (180/PI)*(2./15.)*DAYL
	ALPHA = SIN(NEWLAT)*SIN(SOLARDEC) + 
     1		COS(NEWLAT)*COS(SOLARDEC)*COS((PI/12)*DAYL*(TIME - 0.5))
	ALPHA = ASIN(ALPHA)
	ALPHA1 = ALPHA*180/PI
	SOLAR1 = RATIO*SOLAR




        SOLAR1 = radiation !ew - use the actual daily radation for the calculation



	IMAX = SOLAR1*(1.0 + SIN(2*PI*0.5 + 1.5*PI))/(DAYL*60*60)
	ITOT = SOLAR1*(1.0 + SIN(2*PI*TIME + 1.5*PI))/(DAYL*60*60)


C       IDIF = 0.0
	IDIF = 0.17*1360*SIN(ALPHA)/1000000
C
C       Increase IDIF by 20% for glasshouse conditions
C
C       IDIF = 1.20*IDIF
	IF(ITOT.LT.IDIF) IDIF=ITOT
	IDIR = ITOT - IDIF 
C
C	Do calculation by canopy layers; layer 1 is at top of canopy
C
	DO 10 L=1,5
	SUMLAI(L) = 0.
	SUMF(L) = 0.
10	SLAISN(L) =0.
C
C	Calculate light intercepted and sunlit and shaded leaf area 
C	for each canopy layer
C
	K = G/SIN(ALPHA)
	SUMLAI(1) = LAI(1)
	SUMF(1) = 1.0 - EXP(-K*SUMLAI(1))
	F(1) = SUMF(1)
	SLAISN(1) = SUMF(1)/K
	LAISUN(1) = SLAISN(1)
	LAISH(1) = LAI(1) - LAISUN(1)
	DO 20 L=2,5
	SUMLAI(L) = SUMLAI(L-1) + LAI(L)
	SUMF(L) = 1.0 - EXP(-K*SUMLAI(L))
	F(L) = AMAX1(SUMF(L) - SUMF(L-1), 0.000001)
	SLAISN(L) = SUMF(L)/K
	LAISUN(L) = AMAX1(SLAISN(L) - SLAISN(L-1), 0.000001)
20	LAISH(L) = LAI(L) - LAISUN(L)
	LAICAN = SUMLAI(5)
C
C	Calculate light intensity for sunlit and shaded leaf area 
C	for each canopy layer
C
	DO 30 L=1,5
30	ISUN(L) = IDIR*F(L)/LAISUN(L) + IDIF*F(L)/LAI(L)
	ISH(1) = IDIF*F(1)/LAI(1) + 
     1		 SCAT*(ISUN(1)*LAISUN(1))/(LAISH(1)+LAISH(2))
	DO 40 L=2,4
40	ISH(L) = IDIF*F(L)/LAI(L) +
     1		 SCAT*(ISUN(L-1)*LAISUN(L-1))/(LAISH(L-1)+LAISH(L)) +
     2		 SCAT*(ISUN(L)*LAISUN(L))/(LAISH(L)+LAISH(L+1))
	ISH(5) = IDIF*F(5)/LAI(5) +
     1		 SCAT*(ISUN(4)*LAISUN(4))/(LAISH(4)+LAISH(5)) +
     2		 SCAT*(ISUN(5)*LAISUN(5))/LAISH(5)
C
C	Calculate SLN for each layer using SLNGRAD
C
	CON = AVSLN + SLNGRAD*(LAICAN/2.)
	SLN(1) = (CON + (CON - SLNGRAD*SUMLAI(1)))/2.
	DO 45 L=2,5
	SLNTOP = CON - SLNGRAD*SUMLAI(L-1)
	SLNBOT = CON - SLNGRAD*SUMLAI(L)
	SLN(L) = (SLNTOP + SLNBOT)/2.
45	IF (SLN(L).LE.0.61) SLN(L) = 0.61
C
C	Calculate RUE for each canopy layer from 
C	photosynthesis of sunlit and shade leaves in each layer
C
	DO 50 L=1,5
	PMAX(L) = A*(2.0/(1.0+EXP(-0.9*(SLN(L)-0.6)))-1.0)
	CSUN(L) = LAISUN(L)*PMAX(L)*(1.0-EXP(-5000.0*ISUN(L)/PMAX(L)))
	CSH(L) = LAISH(L)*PMAX(L)*(1.0-EXP(-5000.0*ISH(L)/PMAX(L)))
50	RUE(L) = B/1000.*(CSUN(L)+CSH(L))/(F(L)*ITOT)
C
C	Calculate assimilation and radiation intercepted for the entire canopy
C
	BIOMAS(ITIME) = 0.
	DO 60 L=1,5
60	BIOMAS(ITIME) = BIOMAS(ITIME) + CSUN(L) + CSH(L)
	BIOMAS(ITIME) = B/1000.*BIOMAS(ITIME)
	RADINT(ITIME) = SUMF(5)*ITOT
	DIR(ITIME) = IDIR
	DIF(ITIME) = IDIF
65	CONTINUE
C
C	Calculate BIO, RAD & RUE for the day; Gaussian integration
C	Calculate DIRRAD & DIFRAD for the day; Gaussian integration
C
	BIO = 3600.*DAYL*(BIOMAS(1) + 1.6*BIOMAS(2) + BIOMAS(3))/3.6
	RAD = 3600.*DAYL*(RADINT(1) + 1.6*RADINT(2) + RADINT(3))/3.6
	RUEDAY = BIO/RAD
	DIRRAD = 3600.*DAYL*(DIR(1) + 1.6*DIR(2) + DIR(3))/3.6
	DIFRAD = 3600.*DAYL*(DIF(1) + 1.6*DIF(2) + DIF(3))/3.6
C
C	Calculate average SLN for canopy
C
	AVSLN = 0.
	DO 70 L=1,5
70	AVSLN = AVSLN + (LAI(L)*SLN(L))/SUMLAI(5)
C
C	Output
C
*	WRITE(20,100) SOLAR,SOLAR1,DIRRAD,DIFRAD,IMAX,AVSLN,RUEDAY
*100	FORMAT(1X,4F6.2,F10.6,2F6.2)
80	CONTINUE
90	CONTINUE
********************************************************************




        rue_sln = RUEDAY

      call pop_routine (my_name)
      return
      end


*     ===========================================================
      real function leaf_size_bellshapecurve (
     .          c_x0_const,
     .          c_x0_slope,
     .          g_leaf_no_final,
     .          c_y0_const,
     .          c_y0_slope,
     .          c_a_const,
     .          c_a_slope1,
     .          c_a_slope2,
     .          c_b_const,
     .          c_b_slope1,
     .          c_b_slope2,
     .          leaf_no)
*     ===========================================================
      implicit none

c     dll_export leaf_size_bellshapecurveellshapecurve

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
       real leaf_no               ! (INPUT) nominated leaf number

*+  Purpose
*       Return the leaf area (mm^2) of a specified leaf no.

*+  Changes
*       210397 nih/mjr specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'leaf_size_bellshapecurve')

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
 
      leaf_size_bellshapecurve = area

      call pop_routine (my_name)

      return

      end





*     ===========================================================
      subroutine cproc_leaf_area_pot_bellshapecurve (
     .          begin_stage,
     .          now,
     .          g_leaf_no,
     .          c_leaf_no_correction,
     .          c_x0_const,
     .          c_x0_slope,
     .          g_leaf_no_final,
     .          c_y0_const,
     .          c_y0_slope,
     .          c_a_const,
     .          c_a_slope1,
     .          c_a_slope2,
     .          c_b_const,
     .          c_b_slope1,
     .          c_b_slope2,
     .          g_dlt_leaf_no,
     .          g_plants,
     .          g_swdef_expansion,
     .          dlt_lai_pot)
*     ===========================================================
      implicit none
      dll_export cproc_leaf_area_pot_bellshapecurve
      include  'convert.inc'
      include 'data.pub'
      include 'error.pub'                         

*+  Sub-Program Arguments
       INTEGER begin_stage
       INTEGER now

       real g_leaf_no(*)
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
      real  leaf_size_bellshapecurve       ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_leaf_area_pot_bellshapecurve')

*+  Local Variables
      real       area                  ! potential maximum area of oldest
                                       ! expanding leaf (mm^2) in today's
                                       ! conditions
      real       leaf_no_effective     ! effective leaf no - includes
                                       ! younger leaves that have emerged
                                       ! after the current one

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
         !once leaf no is calculated leaf area of largest expanding leaf is determined
 
         !glh This should also be from sowing, as above? (changed from emerg (scc))
         leaf_no_effective = sum_between (begin_stage, now, g_leaf_no)
     :                       + c_leaf_no_correction

         area = leaf_size_bellshapecurve (
     .          c_x0_const,
     .          c_x0_slope,
     .          g_leaf_no_final,
     .          c_y0_const,
     .          c_y0_slope,
     .          c_a_const,
     .          c_a_slope1,
     .          c_a_slope2,
     .          c_b_const,
     .          c_b_slope1,
     .          c_b_slope2,
     .         leaf_no_effective)

         dlt_lai_pot = g_dlt_leaf_no * area * smm2sm * g_plants
 
      call pop_routine (my_name)

      return

      end



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




