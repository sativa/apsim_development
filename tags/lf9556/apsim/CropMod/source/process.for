C     Last change:  E     1 Sep 2000    4:50 pm

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

      co2_modifier = linear_interp_real(co2level,
     :                                  co2_level_te,
     :                                  te_co2_modifier,
     :                                  num_co2_level_te)

      if (co2level .lt. 1.0) then
          co2_modifier =1.0
      end if

c       PRINT *, 'co2level    = ',co2level
c       PRINT *, 'te_modifier = ',co2_modifier

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

c      PRINT *, crop_type, croptype
c      PRINT *, "maxt =", maxt
c      PRINT *, "mint =", mint
c      PRINT *, "temp =", temp
c      PRINT *, "TT   =", TT
c      PRINT *, "Modi =", modifier


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

     .          p_grain_num_coeff,
     .          g_grain_no)
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

         g_grain_no = p_grain_num_coeff * g_dm_green(stem)

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


























* ====================================================================
       subroutine cproc_n_supply1_wang (
     :            g_dlayer
     :          , max_layer
     :          , g_dlt_sw_dep
     :          , g_NO3gsm
     :          , g_NO3gsm_min
     :          , g_root_depth
     :          , g_sw_dep
     :          , g_NO3gsm_mflow_avail
     :          , g_sw_avail
     :          , g_sw_avail_pot
     :          , g_NO3gsm_diffn_pot
     :          , G_current_stage
     :          , C_n_fix_rate
     :          , fixation_determinant
     :          , G_swdef_fixation
     :          , g_N_fix_pot
     :          )
* ====================================================================
      implicit none
c     dll_export cproc_n_supply1
      include 'error.pub'                         

*+  Sub-Program Arguments
      real g_dlayer(*)             ! (INPUT)
      integer max_layer            ! (INPUT)
      real g_dlt_sw_dep(*)         ! (INPUT)
      real g_NO3gsm(*)             ! (INPUT)
      real g_NO3gsm_min(*)         ! (INPUT)
      real g_root_depth            ! (INPUT)
      real g_sw_dep(*)             ! (INPUT)
      real g_NO3gsm_mflow_avail(*) ! (OUTPUT)
      real g_sw_avail(*)           ! (INPUT)
      real g_sw_avail_pot(*)       ! (INPUT)
      real g_NO3gsm_diffn_pot(*)   ! (OUTPUT)
      real G_current_stage         ! (INPUT)
      real C_n_fix_rate(*)         ! (INPUT)
      real fixation_determinant    ! (INPUT)
      real G_swdef_fixation        ! (INPUT)
      real g_N_fix_pot             ! (INPUT)

*+  Purpose
*      Calculate nitrogen supplys from soil and fixation

*+  Mission Statement
*   Calculate crop Nitrogen supplies (soil + fixation)

*+  Changes
*     21-04-1998 - neilh - Programmed and Specified

*+  Calls
      dll_import crop_n_mass_flow1
      dll_import crop_n_diffusion1
      dll_import crop_n_fixation_pot1

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'cproc_n_supply1')

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
         call crop_N_mass_flow1_wang  (max_layer,
     .          g_dlayer,
     .          g_dlt_sw_dep,
     .          g_NO3gsm,
     .          g_NO3gsm_min,
     .          g_root_depth,
     .          g_sw_dep,
     .          g_NO3gsm_mflow_avail)
 
         call crop_N_diffusion1_wang (max_layer,
     .          g_dlayer,
     .          g_NO3gsm,
     .          g_NO3gsm_min,
     .          g_root_depth,
     .          g_sw_avail,
     .          g_sw_avail_pot,
     .          g_NO3gsm_diffn_pot)



                  ! determine N from fixation

          call crop_N_fixation_pot1_wang
     :               (
     :                G_current_stage
     :              , C_n_fix_rate
     :              , fixation_determinant
     :              , G_swdef_fixation
     :              , g_N_fix_pot
     :               )
 
      call pop_routine (myname)
      return
      end


*     ===========================================================
      subroutine crop_N_mass_flow1_wang(num_layer,
     :                             dlayer,
     :                             dlt_sw_dep,
     :                             no3gsm,
     :                             no3gsm_min,
     :                             root_depth,
     :                             sw_dep,
     :                             NO3gsm_mflow_pot)
*     ===========================================================
      implicit none
c     dll_export crop_n_mass_flow1
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      INTEGER num_layer        ! (INPUT)  number of layers in profile
      REAL    dlayer(*)         ! (INPUT)  thickness of soil layer I (mm)
      REAL    dlt_sw_dep(*)     ! (INPUT)  water uptake in each layer (mm water)
      REAL    no3gsm(*)         ! (INPUT)  nitrate nitrogen in layer L (g N/m^2)
      REAL    no3gsm_min(*)     ! (INPUT)  minimum allowable NO3 in soil (g/m^2)
      REAL    root_depth        ! (INPUT)  depth of roots (mm)
      REAL    sw_dep(*)         ! (INPUT)  soil water content of layer L (mm)
      real NO3gsm_mflow_pot(*) ! (OUTPUT) potential plant NO3
                                              ! uptake (supply) g/m^2,
                                              ! by mass flow

*+  Purpose
*       Return potential nitrogen uptake (supply) by mass flow (water
*       uptake) (g/m^2)

*+  Mission Statement
*   Calculate crop nitrogen supply from mass flow, %8.

*+  Changes
*      090994 jngh specified and programmed
*      970216 slw generalised to avoid common blocks , added num_layer parameter

*+  Constant Values
      character  my_name*(*)   ! name of procedure
      parameter (my_name = 'crop_N_mass_flow1')

*+  Local Variables
      integer deepest_layer    ! deepest layer in which the roots are growing
      integer layer            ! layer number of soil
      real NO3_conc            ! nitrogen concentration (g/m^2/mm)
      real NO3gsm_mflow        ! potential nitrogen uptake (g/m^2)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      call fill_real_array (NO3gsm_mflow_pot, 0.0, num_layer)
         ! only take the layers in which roots occur
      deepest_layer = find_layer_no(root_depth, dlayer, num_layer)
      do 1000 layer = 1, deepest_layer
            ! get  NO3 concentration
         NO3_conc = divide(NO3gsm(layer), sw_dep(layer), 0.0)
            ! get potential uptake by mass flow
         NO3gsm_mflow = NO3_conc * (-dlt_sw_dep(layer))
         NO3gsm_mflow_pot(layer) = u_bound (NO3gsm_mflow,
     :                            NO3gsm(layer) - NO3gsm_min(layer))
 
1000  continue

c       PRINT *,'=========================================='
c       PRINT *,'mflow_supply =',sum_real_array
c     :                         (NO3gsm_mflow_pot, deepest_layer)
c       PRINT *,'wateruptake  =',- sum_real_array
c     :                         (dlt_sw_dep, deepest_layer)
c       PRINT *,'no3gsm       =',sum_real_array
c     :                         (no3gsm, deepest_layer)
c       PRINT *,'no3gsm_min   =',sum_real_array
c     :                         (no3gsm_min, deepest_layer)
c       PRINT *,'sw_dep       =',sum_real_array
c     :                         (sw_dep, deepest_layer)




c      write(*,101) sum_real_array (NO3gsm_mflow_pot, deepest_layer)
c     :,sum_real_array (NO3gsm, deepest_layer)
c     :,sum_real_array (NO3gsm_min, deepest_layer)
c     :,divide (sum_real_array (NO3gsm, deepest_layer)
c     :            ,sum_real_array (sw_dep, deepest_layer), 0.0)
c     :,sum_real_array (-dlt_sw_dep, deepest_layer)
101   format(1x,5f10.6)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine crop_N_diffusion1_wang (num_layer,
     :                              dlayer,
     :                              no3gsm,
     :                              no3gsm_min,
     :                              root_depth,
     :                              sw_avail,
     :                              sw_avail_pot,
     :                              NO3gsm_diffn_pot)
*     ===========================================================
      implicit none
      dll_export crop_n_diffusion1
      include 'convert.inc'       ! ha2sm, kg2gm
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      INTEGER num_layer           ! (INPUT)  number of layers in profile
      REAL    dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL    no3gsm(*)           ! (INPUT)  nitrate nitrogen in layer L (g N/m^
      REAL    no3gsm_min(*)       ! (INPUT)  minimum allowable NO3 in soil (g/m^
      REAL    root_depth          ! (INPUT)  depth of roots (mm)
      REAL    sw_avail(*)         ! (INPUT)  actual extractable soil water (mm)
      REAL    sw_avail_pot(*)     ! (INPUT)  potential extractable soil water (m
      real    NO3gsm_diffn_pot(*) ! (OUTPUT) potential plant NO3
                                              ! uptake (supply) g/m^2,
                                              !  by diffusion

*+  Purpose
*       Return potential nitrogen uptake (supply) by diffusion
*       for a plant (g/m^2)

*+  Mission Statement
*   Calculate crop nitrogen supply from active uptake, %8.

*+  Changes
*      060495 nih taken from template
*      160297 slw generalised to avoid common blocks , added num_layer parameter

*+  Constant Values
      character  my_name*(*)      ! name of procedure
      parameter (my_name = 'crop_N_diffusion1')

*+  Local Variables
      integer deepest_layer       ! deepest layer in which the roots are growing
      integer layer               ! layer number of soil
      real    NO3gsm_diffn        ! potential nitrogen uptake (g/m^2)
      real    sw_avail_fract      ! fraction of extractable soil water ()

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
           ! only take the layers in which roots occur
      call fill_real_array(NO3gsm_diffn_pot, 0.0, num_layer)
 
      deepest_layer = find_layer_no(root_depth, dlayer, num_layer)
      do 1000 layer = 1, deepest_layer
 
         sw_avail_fract = divide(sw_avail(layer),
     :                           sw_avail_pot(layer), 0.0)
         sw_avail_fract = bound(sw_avail_fract, 0.0, 1.0)

c       PRINT *, 'acr/pot =', sw_avail_fract


            ! get extractable NO3
            ! restricts NO3 available for diffusion to NO3 in plant
            ! available water range
         NO3gsm_diffn = sw_avail_fract * NO3gsm(layer)
         NO3gsm_diffn_pot(layer) = u_bound(NO3gsm_diffn,
     :                             NO3gsm(layer) - NO3gsm_min(layer))
 
1000  continue


c       PRINT *,'diffusion_sup  =',sum_real_array
c     :                         (NO3gsm_diffn_pot, deepest_layer)

c       PRINT *,'no3gsm       =',sum_real_array
c     :                         (no3gsm, deepest_layer)


 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine crop_N_fixation_pot1_wang
     :               (
     :                G_current_stage
     :              , C_n_fix_rate
     :              , fixation_determinant
     :              , G_swdef_fixation
     :              , N_fix_pot
     :               )
*     ===========================================================
      implicit none
      dll_export crop_n_fixation_pot1
      include 'error.pub'                         

*+  Sub-Program Arguments
      REAL       G_Current_stage       ! (INPUT) Current stage
      REAL       C_n_fix_rate(*)       ! (INPUT)  potential rate of N fixation (
      REAL       fixation_determinant  ! (INPUT)
      REAL       G_swdef_fixation      ! (INPUT)
      real       N_fix_pot                   ! (OUTPUT) N fixation potential (g/

*+  Purpose
*           calculate the quantity of atmospheric nitrogen fixed
*          per unit carbohydrate per day (mgN_fixed/g plant)

*+  Mission Statement
*   Calculate crop nitrogen supply from fixation, %8.

*+  Changes
*       240595  psc   specified

*+  Constant Values
      character  my_name*(*)                 ! name of subroutine
      parameter (my_name = 'crop_N_fixation1')

*+  Local Variables
      integer current_phase                 ! guess

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      current_phase = int(g_current_stage)
 
      N_fix_pot = c_N_fix_rate(current_phase)
     :          * fixation_determinant
     :          * g_swdef_fixation
 
      call pop_routine (my_name)
      return
      end




*     ===========================================================
      subroutine cproc_N_uptake1_wang
     :               (
     :                C_no3_diffn_const
     :              , G_dlayer
     :              , max_layer
     :              , G_no3gsm_diffn_pot
     :              , G_no3gsm_mflow_avail
     :              , G_N_fix_pot
     :              , c_n_supply_preference
     :              , G_n_demand
     :              , G_n_max
     :              , max_part
     :              , G_root_depth
     :              , dlt_NO3gsm
     :              , dlt_NO3gsm_massflow
     :              , dlt_NO3gsm_diffusion
     :               )
*     ===========================================================
      implicit none
c      dll_export cproc_n_uptake1
      include   'const.inc'
      include   'cmxlayer.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      REAL       C_no3_diffn_const     ! (INPUT)  time constant for uptake by di
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      INTEGER    max_layer             ! (INPUT)  max number of soil layers
      REAL       G_no3gsm_diffn_pot(*) ! (INPUT)  potential NO3 (supply) from so
      REAL       G_no3gsm_mflow_avail(*) ! (INPUT)  potential NO3 (supply) from
      REAL       G_N_Fix_Pot           ! (INPUT) potential N fixation (g/m2)
      CHARACTER  c_n_supply_preference*(*) !(INPUT)
      REAL       G_n_demand(*)         ! (INPUT)  critical plant nitrogen demand
      INTEGER    max_part              ! (INPUT)  number of plant parts
      REAL       G_n_max(*)            ! (INPUT)  maximum plant nitrogen demand
      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
      real       dlt_NO3gsm(*)         ! (OUTPUT) actual plant N uptake
                                       ! from NO3 in each layer (g/m^2)



      real       dlt_NO3gsm_massflow(*)
      real       dlt_NO3gsm_diffusion(*)



*+  Purpose
*       Return actual plant nitrogen uptake from
*       each soil layer.

*+  Mission Statement
*   Calculate crop Nitrogen Uptake

*+  Changes
*       160498 nih specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_N_uptake1')

*+  Local Variables
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      real       NO3gsm_diffn          ! actual N available (supply) for
                                       ! plant (g/m^2) by diffusion
      real       NO3gsm_mflow          ! actual N available (supply) for
                                       ! plant (g/m^2) by mass flow
      real       NO3gsm_diffn_avail(crop_max_layer) ! potential NO3 (supply)
                                       ! from soil (g/m^2), by diffusion
      real       NO3gsm_diffn_supply   ! total potential N uptake (supply)
                                       ! for plant (g/m^2) by diffusion
      real       NO3gsm_mflow_supply   ! total potential N uptake (supply)
                                       ! for plant (g/m^2) by mass flow
      real       diffn_fract           ! fraction of nitrogen to use (0-1)
                                       ! for diffusion
      real       mflow_fract           ! fraction of nitrogen to use (0-1)
                                       ! for mass flow
      integer    layer                 ! soil layer number of profile
      real       N_demand              ! total nitrogen demand (g/m^2)
      real       NO3gsm_uptake         ! plant NO3 uptake from layer (g/m^2)
      real       N_max                 ! potential N uptake per plant (g/m^2)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
            ! get potential N uptake (supply) from the root profile.
            ! get totals for diffusion and mass flow.
 
      deepest_layer = find_layer_no (g_root_depth
     :                              ,g_dlayer
     :                              ,max_layer)
      do 1000 layer = 1, deepest_layer
         NO3gsm_diffn_avail(layer) = g_NO3gsm_diffn_pot(layer)
     :                             - g_NO3gsm_mflow_avail(layer)
         NO3gsm_diffn_avail(layer) = l_bound (NO3gsm_diffn_avail(layer)
     :                                       , 0.0)
1000  continue
 
      NO3gsm_mflow_supply = sum_real_array (g_NO3gsm_mflow_avail
     :                                     , deepest_layer)
      NO3gsm_diffn_supply = sum_real_array (NO3gsm_diffn_avail
     :                                     , deepest_layer)
 
            ! get actual total nitrogen uptake for diffusion and mass flow.
            ! If demand is not satisfied by mass flow, then use diffusion.
            ! N uptake above N critical can only happen via mass flow.
 
      N_demand = sum_real_array (g_N_demand, max_part)
      N_max    = sum_real_array (g_N_max, max_part)
 
      if (NO3gsm_mflow_supply.ge.N_demand) then
         NO3gsm_mflow = NO3gsm_mflow_supply
         NO3gsm_mflow = u_bound (NO3gsm_mflow, N_max)
         NO3gsm_diffn = 0.0
 
      else
 
         NO3gsm_mflow = NO3gsm_mflow_supply
 
         if (c_n_supply_preference.eq.'active') then
            NO3gsm_diffn = bound (N_demand - NO3gsm_mflow, 0.0
     :                        , NO3gsm_diffn_supply)
 
         elseif (c_n_supply_preference.eq.'fixation') then
            NO3gsm_diffn = bound (N_demand - NO3gsm_mflow - g_N_fix_pot
     :                        , 0.0
     :                        , NO3gsm_diffn_supply)
 
         else
            call fatal_error (ERR_USER, 'bad n supply preference')
         endif
 
         NO3gsm_diffn = divide (NO3gsm_diffn, c_NO3_diffn_const, 0.0)
 
      endif
 
            ! get actual change in N contents
 
      call fill_real_array (dlt_NO3gsm, 0.0, max_layer)
 
      do 1100 layer = 1,deepest_layer
 
               ! allocate nitrate
               ! Find proportion of nitrate uptake to be taken from layer
               ! by diffusion and mass flow
 
         mflow_fract = divide (g_NO3gsm_mflow_avail(layer)
     :                       , NO3gsm_mflow_supply, 0.0)
 
         diffn_fract = divide (NO3gsm_diffn_avail(layer)
     :                       , NO3gsm_diffn_supply, 0.0)
 
               ! now find how much nitrate the plant removes from
               ! the layer by both processes
 
         NO3gsm_uptake = NO3gsm_mflow * mflow_fract
     :                 + NO3gsm_diffn * diffn_fract
         dlt_NO3gsm(layer) = - NO3gsm_uptake







         dlt_NO3gsm_massflow (layer) = - NO3gsm_mflow * mflow_fract
         dlt_NO3gsm_diffusion(layer) = - NO3gsm_diffn * diffn_fract





1100  continue
 
      call pop_routine (my_name)
      return
      end

