* ====================================================================
      subroutine APSIM_soilwat2 (action, data_string)
* ====================================================================
      implicit none
      dll_export apsim_soilwat2
      include   'const.inc'            ! mes_presence, mes_init, mes_process
      include   'event.inc'
      include 'engine.pub'
      include 'error.pub'
 
*+  Sub-Program Arguments
      character action*(*)             ! (input) action to perform
      character data_string*(*)        ! (input) data for action
 
*+  Purpose
*      this module performs ceres_maize water balance
*       simulates runoff, infiltration, flux (drainage), unsaturated flow,
*       evaporation, solute movement (nitrate, urea), total transpiration.
 
*+  Mission Statement
*     SoilWat2
 
*+  Changes
* =====================================================
*     Notes transferred from version routine record
*     011092 jngh  specified and programmed
*     161292 jngh  changed to new engine
*     170393 jngh  changed to next new engine
*     131093 markl residue effects for sevap & curve number added from perfect
*                  p_cona (2nd stage evap coeff) added as input.
*                  p_swcon made into an array for layers.
*     190194 jpd   air_dry(layer) added as an input array
*                  changed 'soilwat2_soil_evaporation' to perfect sequencing.
*                             ie.1st stage re-starts with any rainfall.
*     290194 jpd   made compatible with residue module
*                  added eos, residue_wt,residue_cover to apswtrsd.blk
*     120294 jpd   added p_diffus_const,p_diffus_slope as inputs
*                  new variables added to apswtspr.blk
*     150294 mep   modified soilwat2_unsat_flow routine
*
*     130994 jpd   residue_cover is passed as fraction.
*                  crop_cover is passed as fraction also.
*                  (crop_cover - from crop module, calc using intercepted
*                  radn)
*                  (residue at harvest is passed from CM_SAT.for)
*     160994 jpd   add basal_cover request
*     180895 nih   added multi-solute movement capability
*     021296 pdev  incorporate different evaporation models
*     270897 pdev  better handling of observed runoff
*     270897 pdev  Eo from system if required
*     270897 pdev  cn_red, cn_cov changeable from manager
* =====================================================

*      260692 jngh specified and programmed
*      090992 jngh removed include of global.cmn
*      161292 jngh changed to new engine
*      180895 nih  added "add_water" message stuff
*      261095 DPH  added call to message_unused
*      070696 nih  removed data_string from add_water arguments
*      190897 nih  added MES_reset and MES_Sum_Report
*      071097 PdeV added tillage message
*      090298 jngh changed init phase to only get met variables
*      170599 nih  Added new solute handler 

*+  Constant Values
      character  my_name*(*)           ! name of this module
      parameter (my_name = 'soilwat2')
 
*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      call set_warning_off ()

      if (action.eq.mes_get_variable) then
               ! respond to request for variable values - from modules
         call soilwat2_send_my_variable (Data_string)

      else if (action.eq.EVENT_tick) then
         call soilwat2_ONtick()
  
      else if (action.eq.mes_init) then
         call soilwat2_zero_variables ()
         call soilwat2_get_met_variables ()

         call soilwat2_init ()
         call soilwat2_sum_report ()
 
      else if (action.eq.mes_reset) then
         call soilwat2_zero_variables ()
         call soilwat2_get_other_variables ()
 
         call soilwat2_init ()
 
      else if (action.eq.mes_sum_report) then
         call soilwat2_sum_report ()
 
      else if (action.eq.mes_post) then
 
      else if (action.eq.mes_set_variable) then
               ! respond to request to reset variable values - from modules
         call soilwat2_set_my_variable (data_string)
 
      else if (action.eq.mes_process) then
         call soilwat2_zero_daily_variables ()
               ! request and receive variables from owner-modules
         call soilwat2_get_other_variables ()
               ! do soil water balance
         call soilwat2_process ()
 
               ! send changes to owner-modules
         call soilwat2_set_other_variables ()
 
      else if (action.eq.'add_water') then
               ! respond to addition of irrigation
         call soilwat2_add_water ()
 
      else if (action .eq. mes_till) then
         call soilwat2_tillage ()

      else if (action .eq. EVENT_new_solute) then
         call soilwat2_on_new_solute ()
 
      else
             ! don't use message
 
         call Message_unused ()
      endif
 
      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      subroutine soilwat2_process
*     ===========================================================
      implicit none
      include   'soilwat2.inc'
      include 'data.pub'
      include 'error.pub'
 
*+  Purpose
*       simulates runoff, infiltration, flux (drainage), unsaturated flow,
*       evaporation, solute movement, transpiration.
*
*         this needs further redesign and cleaning up. this is a test
*         version only.
 
*+  Mission Statement
*     Perform all APSIM Timestep calculations
 
*+  Changes
*       221090 specified (jngh)
*       290591 jngh set idrsw to 1 if tirr>0 - cr100
*                   fixed fac problem - cr104
*       221091 removed include nmove.blk   jngh
*       100392 jngh tidied up code.
*       260692 jngh removed nitrogen and drainage flags.
*       290892 jngh changed soil water to depth of water
*       151292 jngh changed common blocks
*       300695 jngh changed pot_eo to a global g_eo and removed from
*                    argument of call to evaporation
*       170895 nih  changed to handle user defined list of solutes
*                   and addition of solutes in irrigation water.
*       270897 pdev Cleaned up. Runoff, solute handing in separate subroutines.
 
*+  Constant Values
      character  my_name*(*)           ! this subroutine name
      parameter (my_name = 'soilwat2_process')
 
*+  Local Variables
      integer    layer                 ! layer number counter variable
      integer    num_layers            ! number of layers
 
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
            ! water balance
 
      num_layers = count_of_real_vals (p_dlayer, max_layer)
 
         ! runoff
 
      call soilwat2_runoff (g_rain, g_runoff)
 
      call soilwat2_infiltration (g_infiltration)
 
            ! all infiltration and solutes(from irrigation)
            ! go into the top layer.
 
      g_sw_dep(1) = g_sw_dep(1) + g_infiltration
 
            ! save solutes from irrigation
      call soilwat2_irrig_solute ()
 
      ! NIH 180895
      ! in order to continue capturing irrigation information we zero
      ! the value here.  If we zero the value at the beginning of the day
      ! we may zero it after irrigation has already been specified and the
      ! information would be lost.  The safest way is to hold onto the
      ! information until it is used then reset the record.
 
      g_irrigation = 0.0
      call fill_real_array (g_irrigation_solute, 0.0, max_solute)
 
            ! drainage
            ! get flux
      call soilwat2_drainage (g_flux)
 
            ! move water down
      call move_down_real (g_flux, g_sw_dep, num_layers)
 
            ! drainage out of bottom layer
      g_drain = g_flux(num_layers)
 
            ! now move the solutes with g_flux
            ! flux -  flow > dul
      call soilwat2_move_solute_down ()
 
                          ! potential: sevap + transpiration:
      call soilwat2_pot_evapotranspiration (g_eo)
 
                          ! actual soil evaporation:
      call soilwat2_evaporation (g_es_layers, g_eos)
 
            ! ** take away evaporation
      do 1500 layer = 1, num_layers
         g_sw_dep(layer) = g_sw_dep(layer) - g_es_layers(layer)
 
 1500 continue
 
            ! flow
            ! get unsaturated flow
      call soilwat2_unsat_flow (g_flow)
 
            ! move water up
      call move_up_real (g_flow, g_sw_dep, num_layers)
 
            ! now check that the soil water is not silly
      do 2000 layer = 1,num_layers
         call soilwat2_check_profile (layer)
2000  continue
 
            ! now move the solutes with flow
      call soilwat2_move_solute_up ()
 
            ! end
      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      subroutine soilwat2_runoff ( rain, runoff )
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'soilwat2.inc'
      include 'error.pub'
 
*+  Sub-Program Arguments
      real       rain            ! (INPUT) rainfall (mm)
      real       runoff          ! (OUTPUT) runoff (mm)
 
*+  Purpose
*       Runoff. Either predicted or observed.
 
*+  Notes
*    for phillipine job removed rain condition. sometimes measured runoff on
*    days of no rain - exfiltration ??
*
*    (jh)need to be able run irrigation off at a different curve no.
 
*+  Mission Statement
*     Calculate Runoff
 
*+  Changes
*       221090 specified (jngh)
 
*+  Constant Values
      character  my_name*(*)           ! this subroutine name
      parameter (my_name = 'soilwat2_runoff')
 
*+  Local Variables
      character  string*200            ! message string
 
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      runoff = 0.0
 
      if (rain .gt. 0.0) then
         if (g_obsrunoff_name .eq. blank ) then
            call soilwat2_scs_runoff (rain, runoff)
         else
           if ( g_obsrunoff_found ) then
               runoff = g_obsrunoff
           else
               write (string, '(a,i4,a,i3,a)')
     :      'Year = ', g_year,
     :      ', day = ', g_day,
     :      ', Using predicted runoff for missing observation'
 
               call warning_error (err_user, string)
               call soilwat2_scs_runoff (rain, runoff)
           endif
         endif
 
         call soilwat2_tillage_addrain(g_rain)  ! Update rain since tillage accumulator
                                                ! NB. this needs to be done _after_ cn
                                                ! calculation.
 
      else
               ! nothing
      endif
 
      call pop_routine(my_name)
      return
      end
 
 
 
*     ===========================================================
      subroutine soilwat2_scs_runoff (rain, runoff)
*     ===========================================================
      implicit none
      include   'soilwat2.inc'
      include 'data.pub'
      include 'error.pub'
 
*+  Sub-Program Arguments
      real       rain                  ! (input) rainfall for day (mm)
      real       runoff                ! (output) runoff for day (mm)
 
*+  Purpose
*        calculate runoff using scs curve number method
 
*+  Mission Statement
*        Calculate runoff using scs curve number method      
 
*+  Changes
*        210191   specified and programmed jngh (j hargreaves
*        290892   jngh changed soil water to depth of water
*        091092   jngh removed old commented out code
*        151292   jngh changed common blocks
*        131093   markl added cover vs cn response
*        290194   jpd  added apswtrsd.blk for passing residue_cover
*        060994   jpd  added apswtcrp.blk for passing crop_cover
*        070994   jpd  changed total_cover from fraction to percentage ground
*                      cover so that units are compatiable with 'cn_cov'
*        300994   jpd  hydrol_effective_depth added - read from parameter
*                       file.
*                      'depth of soil' for calc 'wx' can now vary by user.
*                       Code could be made so that 450mm is default if
*                       'hydrol_effctve_depth' is not in parameter file.
*        190595 jngh added bound check on runoff and
*                    changed result of 100/cn when cn=0 to be large number.
*        200896 jngh corrected lower limit of cn2_new.
*        200896 jngh changed cn2 to cn2_bare
*        210896 jngh removed the bound check on the sum of WF.
*                    removed redundant l_bound of cn2_new
*        071097 pdev added tillage reduction on CN.
*        081298 jngh added zeroing of cnpd before accumulation
 
*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_scs_runoff')
 
*+  Local Variables
      real       cn                    ! scs curve number
      real       cn1                   ! curve no. for dry soil (antecedant)
                                       !    moisture
      real       cn3                   ! curve no. for wet soil (antecedant)
                                       !    moisture
      real       cover_fract           ! proportion of maximum cover effect on
                                       !    runoff (0-1)
      real       runoff_cover          ! effective cover for runoff (0-1)
      real       cnpd                  ! cn proportional in dry range
                                       !    (dul to ll15)
      integer    layer                 ! layer counter
      integer    num_layers            ! number of layers
      real       s                     ! potential max retention
                                       !    (surface ponding + infiltration)
      real       xpb                   ! intermedite variable for deriving
                                       !    runof
*
      real       runoff_wf(max_layer)   ! weighting factor for depth for each la
      real       tillage_reduction     ! reduction in cn due to tillage
 
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
            ! revision of the runoff calculation according to scs curve number
            ! cnpd  : fractional avail. soil water weighted over the
            !         hyd.eff. depth  <dms 7-7-95>
            ! cn1   : curve number for dry soil
            ! cn3   : curve number for wet soil
            ! s     : s value from scs equation, transfer to mm scale
            !         = max. pot. retention (~infiltration) (mm)
 
      num_layers = count_of_real_vals (p_dlayer, max_layer)
 
           ! check if hydro_effective_depth applies for eroded profile.
 
      call soilwat2_runoff_depth_factor (runoff_wf)
 
      cnpd = 0.0
      do 100 layer = 1, num_layers
         cnpd = cnpd
     :        + divide (g_sw_dep(layer) - g_ll15_dep(layer)
     :                 , g_dul_dep(layer) - g_ll15_dep(layer)
     :                 , 0.0) *runoff_wf(layer)
  100 continue
      cnpd = bound (cnpd, 0.0, 1.0)
 
      call soilwat2_runoff_cover (runoff_cover)
 
          ! reduce CN2 for the day due to cover effect
 
      cover_fract = divide (runoff_cover, p_cn_cov, 0.0)
      cover_fract = bound (cover_fract, 0.0, 1.0)
 
      g_cn2_new = p_cn2_bare - (p_cn_red * cover_fract)
 
          ! Tillage reduction on CN
      if (g_tillage_cn_rain .gt. 0.0 ) then
        tillage_reduction = g_tillage_cn_red *
     :    ( divide (g_tillage_rain_sum, g_tillage_cn_rain, 0.0) -
     :      1.0)
c        write (*,*) 'tillred = ', tillage_reduction
        g_cn2_new = g_cn2_new + tillage_reduction
      else
                                   ! Nothing
      endif
 
          ! cut off response to cover at high covers if p_cn_red < 100.
          ! <dms7/95> - this bit was missing altogether ??
 
cjh         this is redundant because the previous bound of cover_frac and the
cjh         calculation of cn2_new make it impossible to go lower.
cjh      g_cn2_new = l_bound (g_cn2_new, p_cn2_bare - p_cn_red)
      g_cn2_new = bound (g_cn2_new, 0.0, 100.0)
 
      cn1 = divide (g_cn2_new, (2.334 - 0.01334*g_cn2_new), 0.0)
      cn3 = divide (g_cn2_new, (0.4036 + 0.005964*g_cn2_new), 0.0)
      cn = cn1 + (cn3 - cn1) *cnpd
 
          ! curve number will be decided from scs curve number table ??dms
 
      s = 254.0* (divide (100.0, cn, 1000000.0) - 1.0)
      xpb = rain - 0.2*s
      xpb = l_bound (xpb, 0.0)
 
      runoff = divide (xpb*xpb, (rain + 0.8*s), 0.0)
      call bound_check_real_var (runoff, 0.0, rain, 'runoff')
 
      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      subroutine soilwat2_runoff_cover (runoff_cover)
*     ===========================================================
      implicit none
      include   'soilwat2.inc'
      include 'science.pub'
      include 'error.pub'
 
*+  Sub-Program Arguments
      real       runoff_cover          ! (output) effective runoff cover (0-1)
 
*+  Purpose
*       calculate the effective runoff cover
 
*+  Assumptions
*       Assumes that if canopy height is negative it is missing.
 
*+  Mission Statement
*     Calculate the Effective Runoff Cover
 
*+  Changes
*        200896 jngh specified and programmed
 
*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_runoff_cover')
 
*+  Local Variables
      real       canopy_fact           ! canopy factor (0-1)
      integer    crop                  ! crop number
      real       effective_crop_cover  ! effective crop cover (0-1)
      real       effective_cover_tot   ! efective total cover (0-1)
 
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
          ! cover cn response from perfect   - ML  & dms 7-7-95
          ! nb. perfect assumed crop canopy was 1/2 effect of mulch
          ! This allows the taller canopies to have less effect on runoff
          ! and the cover close to ground to have full effect (jngh)
 
          ! weight effectiveness of crop canopies
          !    0 (no effect) to 1 (full effect)
 
      effective_cover_tot = 0.0
      do 1000 crop = 1, g_num_crops
         if (g_canopy_height(crop).ge.0.0) then
            canopy_fact = linear_interp_real (g_canopy_height(crop)
     :                                       , c_canopy_fact_height
     :                                       , c_canopy_fact
     :                                       , g_num_canopy_fact)
         else
            canopy_fact = c_canopy_fact_default
         endif
 
         effective_crop_cover = g_cover_tot(crop) * canopy_fact
         effective_cover_tot = add_cover (effective_cover_tot
     :                                   , effective_crop_cover)
1000  continue
          ! add cover known to affect runoff
          !    ie residue with canopy shading residue
 
      runoff_cover = add_cover (effective_cover_tot
     :                         , g_residue_cover)
 
 
      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      subroutine soilwat2_pot_evapotranspiration (eo)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'soilwat2.inc'
      include 'error.pub'
 
*+  Sub-Program Arguments
      real       eo                    ! (output) potential evapotranspiration
 
*+  Purpose
*       calculate potential evapotranspiration
 
*+  Notes
*       Eventually eo will be in a separate module entirely, and
*       will appear to soilwat when get_other_varaibles() runs.
*       But, for now we use either priestly-taylor, or whatever
*       the user specified.
 
*+  Mission Statement
*     Calculate Potential EvapoTranspiration
 
*+  Changes
*        210191   specified and programmed jngh (j hargreaves
 
*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_pot_evapotranspiration')
 
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (g_eo_source .eq. blank) then
          call soilwat2_priestly_taylor (eo) ! eo from priestly taylor
      else
          eo = g_eo_system                   ! eo is provided by system
      endif
 
      call pop_routine (my_name)
      end
 
 
 
*     ===========================================================
      subroutine soilwat2_priestly_taylor (eo)
*     ===========================================================
      implicit none
      include   'soilwat2.inc'
      include 'science.pub'
      include 'error.pub'
 
*+  Sub-Program Arguments
      real       eo                    ! (output) potential evapotranspiration
 
*+  Purpose
*       calculate potential evapotranspiration via priestly-taylor
 
*+  Mission Statement
*       Calculate potential evapotranspiration using priestly-taylor method
 
*+  Changes
*        210191   specified and programmed jngh (j hargreaves
*        290591   jngh removed max_layer.con - cr87
*        051191   jngh updated documentation
*        151292   jngh changed common blocks
*        290393   jngh changed to use lai factor
*        110195   jngh changed to use green cover instead of lai
 
*+  Calls
      real       soilwat2_eeq_fac       ! function
 
*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_priestly_taylor')
 
*+  Local Variables
      real       albedo                ! albedo taking into account plant
                                       !    material
      real       cover_green_sum       ! sum of crop green covers (0-1)
      real       eeq                   ! equilibrium evaporation rate (mm)
      real       wt_ave_temp           ! weighted mean temperature for the
                                       !    day (oC)
 
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
*  ******* calculate potential evaporation from soil surface (eos) ******
 
                ! find equilibrium evap rate as a
                ! function of radiation, albedo, and temp.
 
      cover_green_sum = sum_cover_array (g_cover_green, g_num_crops)
      albedo = c_max_albedo
     :       - (c_max_albedo - p_salb) * (1.0 - cover_green_sum)
 
                ! wt_ave_temp is mean temp, weighted towards max.
 
      wt_ave_temp = 0.60*g_maxt + 0.40*g_mint
 
      eeq = g_radn*23.8846* (0.000204 - 0.000183*albedo)
     :    * (wt_ave_temp + 29.0)
 
                ! find potential evapotranspiration (eo)
                ! from equilibrium evap rate
 
      eo = eeq*soilwat2_eeq_fac ()
 
      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      real function soilwat2_eeq_fac ()
*     ===========================================================
      implicit none
      include   'soilwat2.inc'
      include 'error.pub'
 
*+  Purpose
*                 calculate coefficient for equilibrium evaporation rate
 
*+  Mission Statement
*     Calculate the Equilibrium Evaporation Rate
 
*+  Changes
*        210191   specified and programmed jngh (j hargreaves
*        151292   jngh changed common blocks
 
*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_eeq_fac')
 
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (g_maxt.gt.c_max_crit_temp) then
 
                ! at very high max temps eo/eeq increases
                ! beyond its normal value of 1.1
 
         soilwat2_eeq_fac =  ((g_maxt - c_max_crit_temp) *0.05 + 1.1)
      else if (g_maxt.lt.c_min_crit_temp) then
 
                ! at very low max temperatures eo/eeq
                ! decreases below its normal value of 1.1
                ! note that there is a discontinuity at tmax = 5
                ! it would be better at tmax = 6.1, or change the
                ! .18 to .188 or change the 20 to 21.1
 
         soilwat2_eeq_fac = 0.01*exp (0.18* (g_maxt + 20.0))
      else
 
                ! temperature is in the normal range, eo/eeq = 1.1
 
         soilwat2_eeq_fac = 1.1
      endif
 
      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      subroutine soilwat2_evaporation (esoil, eos)
*     ===========================================================
      implicit none
      include   'soilwat2.inc'
      include 'data.pub'
      include 'error.pub'
 
*+  Sub-Program Arguments
      real       eos                   ! (output) potential soil evap after
                                       ! modification for crop cover & residue_wt
      real       esoil(*)              ! (output) actual soil evaporation (mm)
 
*+  Purpose
*       calculate actual soil evaporation
 
*+  Mission Statement
*     Calculate Actual Soil Evaporation
 
*+  Changes
*       031296 pdev removed g_es, replaced with g_es_layers.
 
*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_evaporation')
 
*+  Local Variables
      real       asw1                  ! available soil water in top layer for
                                       ! actual soil evaporation (mm)
 
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      ! 1. get potential soil water evaporation
      call soilwat2_pot_soil_evaporation (eos)
 
      ! 2. get available soil water for evaporation
         ! NB. ritchie + b&s evaporate from layer 1, but rickert
         !     can evaporate from L1 + L2.
      asw1 = g_sw_dep(1) - g_air_dry_dep(1)
      asw1 = bound (asw1, 0.0, g_eo)
 
      ! 3. get actual soil water evaporation
      call soilwat2_soil_evaporation (esoil, eos, asw1)
 
      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      subroutine soilwat2_pot_soil_evaporation (eos)
*     ===========================================================
      implicit none
      include   'soilwat2.inc'
      include 'science.pub'
      include 'data.pub'
      include 'error.pub'
 
*+  Sub-Program Arguments
      real       eos                   ! (output) potential soil evap after
                                       ! modification for crop cover & residue_w
 
*+  Purpose
*       calculate potential soil evaporation
 
*+  Mission Statement
*       Calculate potential soil evaporation
 
*+  Changes
*       290591 jngh removed l_bound from external calls and declaration
*                     - cr68
*       100392 jngh rewrote expressions for eos for clarity.
*                     calculate total lai here
*                     limited available soil water to 0.0 for evaporation
*       180592 jngh temporary change - set tplant, tsenla, totpla here
*                     added crop blocks and block descriptions
*       290892 jngh changed soil water to depth of water
*       160992 jngh moved arguments and their includes into
*       soilwat2_soil_evaporation
*       151292 jngh changed common blocks
*       290393 jngh changed to use lai
*       131093 markl added effects of residue on potential soil evap.
*       190194 jpd   replaced function soilwat2_sw_evap_fac() with
*                    air_dry_dep(1)
*       110195 jngh  changed to use green cover instead of lai
*       250195 jngh changed cover to include residue cover using Beers Law.
*                   Also use residue cover as calculated by the residue
*                   module.
*       080595 jngh reversed above change because no data to calibrate.
*       290695 dms  put back BEERS LAW (canopy & mulch = product NOT min.
*                   Revised canopy effect = exp( fn of canopy cover)
*                   & mulch effect generalized for various types
*                   & mixes of residues.  Externalized 2 coef's.
*       300695 jngh changed pot_eo to global g_eo
*       130896 jngh removed g_cover_tot_sum
*       260897 nih  added test to avoid log of zero error
*       031296 pdev removed g_es, replaced with g_es_layers.
 
*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_pot_soil_evaporation')
 
*+  Local Variables
      real       cover_tot_sum         !
      real       eos_canopy_fract      ! fraction of potential soil evaporation
                                       ! limited by crop canopy (mm)
      real       eos_residue_fract     ! fraction of potential soil evaporation
                                       ! limited by crop residue (mm)
      real       resid_area            ! area "A" of residue (ha/kg)
                                       ! same as in residue.for but re-calc here
      real       eos_resid_coef        ! coefficient in Adam's type residue
                                       ! effect on Eos
 
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
         ! 1. get potential soil water evaporation
 
         !---------------------------------------+
         ! reduce Eo to that under plant CANOPY                    <DMS June 95>
         !---------------------------------------+
 
         !  Based on Adams, Arkin & Ritchie (1976) Soil Sci. Soc. Am. J. 40:436-
         !  Reduction in potential soil evaporation under a canopy is determined
         !  the "% shade" (ie cover) of the crop canopy - this should include th
         !  green & dead canopy ie. the total canopy cover (but NOT near/on-grou
         !  residues).  From fig. 5 & eqn 2.                       <dms June 95>
         !  Default value for c_canopy_eos_coef = 1.7
         !              ...minimum reduction (at cover =0.0) is 1.0
         !              ...maximum reduction (at cover =1.0) is 0.183.
 
      cover_tot_sum = sum_cover_array (g_cover_tot, g_num_crops)
      eos_canopy_fract = exp (-c_canopy_eos_coef * cover_tot_sum)
 
         !-----------------------------------------------+
         ! reduce Eo under canopy to that under mulch            <DMS June 95>
         !-----------------------------------------------+
 
         !1a. adjust potential soil evaporation to account for
         !    the effects of surface residue (Adams et al, 1975)
         !    as used in Perfect
         ! BUT taking into account that residue can be a mix of
         ! residues from various crop types <dms june 95>
 
      if (g_residue_cover.ge.1.0) then
         ! We test for 100% to avoid log function failure.
         ! The algorithm applied here approaches 0 as cover approaches
         ! 100% and so we use zero in this case.
         eos_residue_fract = 0.0
      else
 
         ! Calculate coefficient of residue_wt effect on reducing first
         ! stage soil evaporation rate from residue specific area "A"
 
         ! a) back-calculate area from residue_cover & residue_wt
 
         resid_area = -divide (log (1.0 - g_residue_cover)
     :                                 , g_residue_wt, 0.0)
 
 
         ! b) estimate 1st stage soil evap reduction power of
         !    mixed residues from the area of mixed residues.
         !    [DM. Silburn unpublished data, June 95 ]
         !    <temporary value - will reproduce Adams et al 75 effect>
         !     c_A_to_evap_fact = 0.00022 / 0.0005 = 0.44
 
         eos_resid_coef = resid_area * c_A_to_evap_fact
         eos_residue_fract = exp(-eos_resid_coef * g_residue_wt)
      endif
 
         ! Reduce potential soil evap under canopy to that under residue (mulch)
 
      eos  = g_eo * eos_canopy_fract * eos_residue_fract
 
      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      subroutine soilwat2_soil_evaporation (es, eos, eos_max)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'soilwat2.inc'
      include   'error.pub'
      include   'data.pub'
 
*+  Sub-Program Arguments
      real       es(*)          ! (output) actual evaporation
                                ! (mm) over profile
*
      real       eos            ! (input) potential rate of
                                !    evaporation (mm/day)
*
      real       eos_max        ! (input) upper limit of soil
                                !        evaporation (mm/day)
 
*+  Purpose
*     Wrapper for various evaporation models. Returns actual
*     evaporation from soil surface (es).
 
*+  Mission Statement
*     Soil Evaporation from Soil Surface
 
*+  Changes
*       210191 specified and programmed jngh (j hargreaves
*       270897 PdeV
 
*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_soil_evaporation')
 
*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      call fill_real_array(es, 0.0, max_layer)
 
      if (c_evap_method .eq. c_ritchie_method) then
         call soilwat2_ritchie_evaporation (es(1), eos, eos_max)
 
      else if (c_evap_method .eq. c_bs_a_method) then
         call soilwat2_bs_a_evaporation (es(1), eos, eos_max)
 
      else if (c_evap_method .eq. c_bs_b_method) then
         call soilwat2_bs_b_evaporation (es(1), eos, eos_max)
 
      else if (c_evap_method .eq. c_bs_acs_method) then
         call soilwat2_bs_acs_evaporation (es(1), eos, eos_max)
 
      else if (c_evap_method .eq. c_rickert_method) then
         call soilwat2_rickert_evaporation (es, eos)
 
      else
 
         call fatal_error(err_user,
     :      'Undefined evaporation method')
 
      endif
 
      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      subroutine soilwat2_ritchie_evaporation (es, eos, eos_max)
*     ===========================================================
      implicit none
      include   'soilwat2.inc'
      include 'data.pub'
      include 'error.pub'
 
*+  Sub-Program Arguments
      real       es                    ! (output) actual evaporation (mm)
*
      real       eos                   ! (input) potential rate of
                                       !    evaporation (mm/day)
*
      real       eos_max               ! (input) upper limit of soil
                                       !        evaporation (mm/day)
 
*+  Purpose
*          ****** calculate actual evaporation from soil surface (es) ******
*          most es takes place in two stages: the constant rate stage
*          and the falling rate stage (philip, 1957).  in the constant
*          rate stage (stage 1), the soil is sufficiently wet for water
*          be transported to the surface at a rate at least equal to the
*          evaporation potential (eos).
*          in the falling rate stage (stage 2), the surface soil water
*          content has decreased below a threshold value, so that es
*          depends on the flux of water through the upper layer of soil
*          to the evaporating site near the surface.
 
*+  Notes
*       This changes globals - sumes1/2 and t.
 
*+  Mission Statement
*       Calculate evaporation Ritchie model      
 
*+  Changes
*       210191 specified and programmed jngh (j hargreaves
*       160992 jngh moved arguments out  and included common blocks.
*       131093 markl added p_cona
*       190194 jpd  changed code to perfect (w.r.t. 1st stage evap & rainfall)
*       190194 jpd  add new variables: esoil1,esoil2. drop: pesoil & exces1
*       130394 jpd  fixed bug in 2nd stage for day when rain occurs
 
*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_ritchie_evaporation')
 
*+  Local Variables
      real       esoil1                ! actual soil evap in stage 1
      real       esoil2                ! actual soil evap in stage 2
*
      real       sumes1_max            ! upper limit of sumes1
      real       w_inf                 ! infiltration into top layer (mm)
 
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      sumes1_max = p_u
      w_inf = g_infiltration
 
         ! if infiltration, reset sumes1
         ! reset sumes2 if infil exceeds sumes1
 
      if (w_inf.gt.0.0) then
 
         g_sumes2 = max (0.0, g_sumes2 - max (0.0, w_inf-g_sumes1))
         g_sumes1 = max (0.0, g_sumes1 - w_inf)
 
            ! update t (incase sumes2 changed)
 
         g_t = (divide (g_sumes2, p_cona, 0.0))**2
 
      else
         ! no infiltration, no re-set.
      endif
 
         ! are we in stage1 ?
 
      if (g_sumes1.lt.sumes1_max) then
 
            ! we are in stage1
            ! set esoil1 = potential, or limited by u.
 
          esoil1 = min (eos, sumes1_max - g_sumes1)
 
          if (eos.gt.esoil1 .and. esoil1.lt.eos_max) then
 
*           !  eos not satisfied by 1st stage drying,
*           !  & there is evaporative sw excess to air_dry, allowing for esoil1.
*           !  need to calc. some stage 2 drying(esoil2).
 
*  if g_sumes2.gt.0.0 then esoil2 =f(sqrt(time),p_cona,g_sumes2,g_eos-esoil1).
*  if g_sumes2 is zero, then use ritchie's empirical transition constant (0.6).
 
            if (g_sumes2.gt.0.0) then
               g_t = g_t + 1.0
               esoil2 = min (eos - esoil1, p_cona*g_t**0.5 - g_sumes2)
            else
               esoil2 = 0.6*(eos - esoil1)
            endif
         else
               ! no deficit (or esoil1.eq.eos_max,) no esoil2 on this day
            esoil2 = 0.0
         endif
 
               ! check any esoil2 with lower limit of evaporative sw.
         esoil2 = min (esoil2, eos_max - esoil1)
 
               !  update 1st and 2nd stage soil evaporation.
 
         g_sumes1 = g_sumes1 + esoil1
         g_sumes2 = g_sumes2 + esoil2
         g_t = (divide (g_sumes2, p_cona, 0.0))**2
 
      else
 
            ! no 1st stage drying. calc. 2nd stage
 
         esoil1 = 0.0
 
         g_t = g_t + 1.0
         esoil2 = min (eos, p_cona*g_t**0.5 - g_sumes2)
 
            ! check with lower limit of evaporative sw.
 
         esoil2 = min (esoil2, eos_max)
 
            !   update 2nd stage soil evaporation.
 
         g_sumes2 = g_sumes2 + esoil2
 
      endif
 
      es = esoil1 + esoil2
 
         ! make sure we are within bounds
      es = bound (es,  0.0, eos)
      es = bound (es, 0.0, eos_max)
 
      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      subroutine soilwat2_bs_a_evaporation (es, eos, eos_max)
*     ===========================================================
      implicit none
      include   'soilwat2.inc'
      include 'data.pub'
      include 'error.pub'
 
*+  Sub-Program Arguments
      real       es             ! (output) actual evaporation
                                ! from top layer(mm)
*
      real       eos            ! (input) potential rate of
                                !    evaporation (mm/day)
*
      real       eos_max        ! (input) upper limit of soil
                                !        evaporation (mm/day)
 
*+  Purpose
*     B&S (in their paper this is Option A. Fig 2)
 
*+  Notes
*       This changes globals - sumes1,2.
 
*+  Mission Statement
*       Calculate evaporation using B&S (A) model      
 
*+  Changes
*       210191 specified and programmed jngh (j hargreaves
 
*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_bs_a_evaporation')
 
*+  Local Variables
      real       sumes1_max            ! upper limit of sumes1
      real       w_inf                 ! infiltration into top layer (mm)
      real       espot                 ! temporary
 
*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      sumes1_max = p_beta**2
      w_inf = g_infiltration
 
*     if infiltration is greater than eos, es is = eos.
 
*     &&&&&&&&&& Below here - as coded by B&S in Fig 2 &&&&&&&&&&&&&&&&&&&&&&
      if (w_inf .ge. eos) then
 
         g_sumes = max(0.0, g_sumes - (w_inf - eos))
         es = eos
         espot = divide (g_sumes**2, p_beta**2, 0.0)
         g_sumeos = max(g_sumes, espot)
      else
                                ! Infiltration is less than eos
         g_sumeos = g_sumeos + (eos - w_inf)
         es = w_inf + (min(g_sumeos, p_beta*g_sumeos**0.5)
     :        - g_sumes)
         g_sumes = min(g_sumeos, p_beta*g_sumeos**0.5)
      endif
*     &&&&&&&&&& Above here - as coded by B&S in Fig 2 &&&&&&&&&&&&&&&&&&&&&&
 
*     next 2 conditions added because g_sumes was zero
*     after larger rain and at the same time es was = eos.
 
      if(es .gt. g_sumes) then
         g_sumes = es
      endif
 
      if(g_sumes.le.sumes1_max) then
         g_sumeos = g_sumes
      else
         g_sumeos = (divide (g_sumes, p_beta, 0.0))**2
      endif
 
 
      if (g_sumes.ge. sumes1_max) then
         g_sumes1 = sumes1_max
         g_sumes2 = g_sumes - g_sumes1
      else
         g_sumes1 = g_sumes
         g_sumes2 = 0.0
      endif
 
                                ! make sure we are within bounds
      es = bound (es, 0.0, eos)
      es = bound (es, 0.0, eos_max)
 
      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      subroutine soilwat2_bs_b_evaporation (es, eos, eos_max)
*     ===========================================================
      implicit none
      include   'soilwat2.inc'
      include 'data.pub'
      include 'error.pub'
 
*+  Sub-Program Arguments
      real       es                    ! (output) actual evaporation (mm)
*
      real       eos                   ! (input) potential rate of
                                       !    evaporation (mm/day)
*
      real       eos_max               ! (input) upper limit of soil
                                       !        evaporation (mm/day)
 
*+  Purpose
*     B&S. This code tries to achieve the result stated for their Option B.
*     Evaporate small rainfall events & then step back to the
*     original state
 
*+  Notes
*       This changes globals - sumes1/2 and t.
 
*+  Mission Statement
*       Calculate evaporation using B&S (B) model      
 
*+  Changes
*       210191 specified and programmed jngh (j hargreaves
 
*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_bs_b_evaporation')
 
*+  Local Variables
      real       sumes1_max            ! upper limit of sumes1
      real       w_inf                 ! infiltration into top layer (mm)
      real       esoil1                ! actual soil evap in stage 1
      real       esoil2                ! actual soil evap in stage 2
      real       todays_es             ! today's actual evap as
                                       ! f(beta,sumeos+eos)
 
*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      sumes1_max = p_beta**2
 
      w_inf = g_infiltration
 
                                ! if infiltration, reset sumes1
                                ! reset sumes2 if infil exceeds sumes1
      if (w_inf .gt. 0.0) then
 
         g_sumes2 = max (0.0, g_sumes2 - max (0.0, w_inf
     :        - g_sumes1))
         g_sumes1 = max (0.0, g_sumes1 - w_inf)
 
                                ! update sumes & sumeos
         g_sumes = g_sumes1 + g_sumes2
         if(g_sumes.le.sumes1_max) then
            g_sumeos = g_sumes
         else
            g_sumeos = (divide (g_sumes, p_beta, 0.0))**2
         endif
 
      else
                                ! no infiltration, no re-set.
      endif
 
 
*     Today's actual evap calculated for today's eos
*     If todays_es is limited by soil water then g_sumeos will
*     be adjusted later
 
      g_sumeos = g_sumeos + eos
      if(g_sumeos .le. sumes1_max) then
 
         todays_es = eos
         g_sumes = g_sumes + todays_es
      else
 
         todays_es = p_beta * g_sumeos**0.5 - g_sumes
         todays_es = bound (todays_es,  0.0, eos)
         g_sumes  = g_sumes + todays_es
         g_sumeos = (divide (g_sumes, p_beta, 0.0))**2
      endif
 
                                ! are we in stage1 ?
      if (g_sumes1 .lt. sumes1_max) then
*     We are in stage1.
*     set esoil1 = eos, or limited by sumes1_max (beta**2).
*     todays_es is overriden by 1st stage evap.
 
         esoil1 = min (eos, sumes1_max - g_sumes1)
 
         if (eos .gt. esoil1 .and. esoil1 .lt. eos_max) then
 
*     eos not satisfied by 1st stage drying,
*     & there is evaporative sw excess to air_dry, allowing for esoil1.
*     need to calc. some stage 2 drying(esoil2).
*     For comparing versions, include Ritchie's transition constant 0.6
            esoil2 = (eos - esoil1) * 0.6
 
         else
*     no deficit (or esoil1 .eq. eos_max,) no esoil2 on this day
            esoil2 = 0.0
 
         endif
 
*     check any esoil2 with upper limit of evaporative sw.
         esoil2 = min (esoil2, eos_max - esoil1)
 
                                !  update 1st and 2nd stage soil evaporation.
         g_sumes1 = g_sumes1 + esoil1
         g_sumes2 = g_sumes2 + esoil2
 
      else
 
                                ! no 1st stage drying. todays_es is all
                                ! 2nd stage
         esoil1 = 0.0
 
         esoil2 = todays_es
 
                                ! check with upper limit of evaporative sw.
         esoil2 = min (esoil2, eos_max)
 
                                !   update 2nd stage soil evaporation.
         g_sumes2 = g_sumes2 + esoil2
      endif
 
*     update sumes & sumeos incase esoil1&2 limited by eos_max
*     or ritchie transition constant
 
      g_sumes = g_sumes1 + g_sumes2
      g_sumeos = (divide (g_sumes, p_beta, 0.0))**2
 
      es = esoil1 + esoil2
 
                                ! make sure we are within bounds
      es = bound (es, 0.0, todays_es)
      es = bound (es, 0.0, eos_max)
 
      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      subroutine soilwat2_bs_acs_evaporation (es, eos, eos_max)
*     ===========================================================
      implicit none
      include   'soilwat2.inc'
      include 'data.pub'
      include 'error.pub'
 
*+  Sub-Program Arguments
      real       es                    ! (output) actual evaporation (mm)
*
      real       eos                   ! (input) potential rate of
                                       !    evaporation (mm/day)
*
      real       eos_max               ! (input) upper limit of soil
                                       !        evaporation (mm/day)
 
*+  Purpose
*     acs attempt at B&S Option B
*     infiltration > evap at stage 1, then return to original
*     (pdev - I think this is work in progress, incomplete)
 
*+  Notes
*       This changes globals - inf_pool, sumes_yest, sumes_last
 
*+  Mission Statement
*       Calculate evaporation using B&S (B, ACS) model      
 
*+  Changes
*       210191 specified and programmed jngh (j hargreaves
 
*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_bs_acs_evaporation')
 
*+  Local Variables
      real      sumes1_max
      real      w_inf
      real      surplus_es
 
*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      sumes1_max = p_beta**2
 
      w_inf = g_infiltration
 
      g_sumes_yest = g_sumes
 
                                ! reset for infiltration
      if ((g_inf_pool .EQ. 0.0) .AND. (w_inf .GT. 0.0)) then
         g_sumes_yest = 0.0
         g_sumes_last = g_sumes
         g_sumeos_last = g_sumeos
      else
                                ! no need to store last values
      endif
 
      g_inf_pool = g_inf_pool + w_inf
 
      if (g_inf_pool .GT. 0.0) then
         g_sumes = 0.0
         g_sumeos = 0.0
      else
                                ! Nothing in inf pool to be
                                ! evap at stage 1, no reset
      endif
 
                                ! dodgy logic for a massive reset
                                ! on 90% AWR (1)   ho ho ho !!
      if ( g_sw_dep(1) .GT. (0.9*(g_dul_dep(1)-g_air_dry_dep(1))) )
     +     then
         g_sumes_last = 0.0
         g_sumeos_last = 0.0
      else
                                ! no need for massive reset
      endif
 
 
                                ! Do the B&S ...
      g_sumeos = g_sumeos + eos
 
      if (g_sumes .LT. sumes1_max) then !first stage
         g_sumes = g_sumeos
 
      else                      ! second stage
         g_sumes = p_beta * g_sumeos**0.5
 
      endif
 
                                ! calc esoil and update inf_pool, sumes/eos
      es = g_sumes - g_sumes_yest
      g_inf_pool = max (0.0, g_inf_pool - es)
 
*     Put things back how they were before infil and adjust for over evaping
      if (g_inf_pool .LE. 0.0) then !evaped all away
         if (g_sumes_last .GT. 0.0) then
            surplus_es = 0.0 - g_inf_pool
            g_inf_pool = 0.0
 
                                ! carry surplus evap over to last posi
            g_sumes = g_sumes_last + surplus_es
            if (surplus_es .LT. sumes1_max) then
               g_sumeos = g_sumeos_last + surplus_es
            else
               g_sumeos = g_sumeos_last + (surplus_es/p_beta)**2
            endif
         else                   ! g_sumes_last = 0.0 ie had massive reset
                                ! and no need to change g_sumes and g_sumeos
         endif
 
      else
                                ! keep evaping infil pool tommorrow
      endif
 
      es = bound (es, 0.0, eos_max)
 
      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      subroutine soilwat2_rickert_evaporation (es, eos)
*     ===========================================================
      implicit none
      include    'const.inc'
      include    'soilwat2.inc'
      include 'data.pub'
      include 'error.pub'
 
*+  Sub-Program Arguments
      real       es(*)                 ! (output) actual evaporation (mm)
*
      real       eos                   ! (input) potential rate of
                                       !    evaporation (mm/day)
 
*+  Purpose
*     Ex Grasp                <<< need better description here - dms!!!>>>
*     Evaporate moisture from the soil surface. The moisture can come
*     from layers 1 & 2, if the supply is sufficient. Total evap is limited
*     by an upper bound, max_evap, from soil parameters.
*
 
*+  Mission Statement
*     Calculate evaporation using Rickert model      
 
*+  Changes
*       210191 specified and programmed jngh (j hargreaves
 
*+  Calls
      real       soilwat2_comp_curve
 
*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_rickert_evaporation')
 
*+  Local Variables
      real       supply_ratio_L1
      real       supply_ratio_L1_L2
      real       supply_ratio_L2
      real       avail_water_L1
      real       avail_water_L1_L2
      real       avail_capacity_L1
      real       avail_capacity_L1_L2
      real       evap_L1
      real       evap_L2
      real       eos_max               ! upper limit of soil
                                       ! evaporation (mm/day)
 
*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      call fill_real_array(es, 0.0, max_layer)
 
      avail_water_L1 = g_sw_dep(1) - g_air_dry_dep(1)
 
      avail_capacity_L1 = g_dul_dep(1) - g_air_dry_dep(1)
 
      supply_ratio_L1 = divide (avail_water_L1,
     :     avail_capacity_L1, 0.0)
 
c     PdeV - Should send this magic number to constants file:
      supply_ratio_L1 = soilwat2_comp_curve (supply_ratio_L1, 0.285)
 
      supply_ratio_L1 = bound (supply_ratio_L1, 0.0, 1.0)
 
      avail_water_L1_L2 = g_sw_dep(1) + g_sw_dep(2) -
     :     g_air_dry_dep(1) - g_ll15_dep(2)
 
      avail_capacity_L1_L2 = g_dul_dep(1) + g_dul_dep(2) -
     :     g_air_dry_dep(1) - g_ll15_dep(2)
 
      supply_ratio_L1_L2 = divide (avail_water_L1_L2,
     :     avail_capacity_L1_L2, 0.0)
      supply_ratio_L1_L2 = soilwat2_comp_curve(supply_ratio_L1_L2,
     :     0.117)
 
      supply_ratio_L1_L2 = bound (supply_ratio_L1_L2, 0.0, 1.0)
 
 
      evap_L1 = supply_ratio_L1 * eos
      eos_max = min(p_max_evap, g_sw_dep(1) - g_air_dry_dep(1))
      evap_L1 = bound(evap_L1, 0.0, eos_max)
 
CPdeV - should resolve whether we can evaporate L2 down to airdry.
      if (supply_ratio_L1_L2 .gt. supply_ratio_L1 .and.
     :    g_sw_dep(2) .gt. g_ll15_dep(2)) then
         supply_ratio_L2 = supply_ratio_L1_L2 - supply_ratio_L1
 
         evap_L2 = supply_ratio_L2 * eos
         eos_max = min(p_max_evap - evap_L1,
     :                  g_sw_dep(2) - g_ll15_dep(2))
cdms         eos_max = min(p_max_evap - evap_L1,
cdms     :                     g_sw_dep(2) - g_air_dry_dep(2))  ! more realistic
         evap_L2 = bound(evap_L2, 0.0, eos_max)
 
      else
         evap_L2 = 0.0
      endif
cdms  pete - this looks to be limited to available sw above
cdms         is a further check needed ?????
c     Can't use g_eo as upper bound to evaporation (like all the
c     other routines) as g_eo is specific to the top layer. This check
c     should suffice.
      eos_max = g_sw_dep(1) - g_air_dry_dep(1) +
     :    g_sw_dep(2) - g_ll15_dep(2)
 
      if (evap_L1 + evap_L2 .gt. eos_max) then
         call warning_error (err_internal,
     :        'Evaporation exceeds asw - help!')
      else
                                ! Nothing
      endif
 
      es(1) = evap_L1
      es(2) = evap_L2
 
      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      real function soilwat2_comp_curve (ndx, a)
*     ===========================================================
      implicit none
      include 'data.pub'
      include 'error.pub'
 
*+  Sub-Program Arguments
      real ndx                  ! input index (0-1)
      real a                    ! weighting
 
*+  Purpose
*     .... from GRASP (Surfair)
*     Standard competition curve (or at least so McKeon
*     calls it) This function is used by McKeon in several places to
*     transform an index in the range [0-1] to another index in the
*     same range, but weighted in a different way. The weighting is
*     controlled by the a parameter. An "a" value  of 1 leaves the
*     index untransformed.
 
*+  Mission Statement
*      Competition curve %1, weighting %2
 
*+  Changes
*       210191 specified and programmed jngh (j hargreaves
 
*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_comp_curve')
 
*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      soilwat2_comp_curve = divide (a * ndx,
     :     ndx * (a - 1.0) + 1.0, 0.0)
 
      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      subroutine soilwat2_drainage (flux)
*     ===========================================================
      implicit none
      include   'soilwat2.inc'
      include 'data.pub'
      include 'error.pub'
 
*+  Sub-Program Arguments
      real       flux (*)              ! (output) water moving out of
                                       ! layer (mm)
 
*+  Purpose
*       calculate flux - drainage from each layer
 
*+  Mission Statement
*     Calculate Drainage from each layer
 
*+  Changes
*        210191   specified and programmed jngh (j hargreaves
*        051191   jngh fixed drainage lower limit and
*                 restructured excess and drainage algorithms - cr196
*        260692   jngh changed l to layer & commented includes
*        290892   jngh changed soil water to depth of water
*        151292   jngh changed common blocks
*        250893   jngh firstly changed drainage criteria to match cm v1 code
*                      then removed .003 part to allow proper denitrification
*                      in nitrogrn module.
*        131093   markl changes p_swcon to an array for each soil layer
 
*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_drainage')
 
*+  Local Variables
      real       excess                ! amount above saturation(overflow)(mm)
      integer    layer                 ! counter for layer no.
      integer    num_layers            ! number of layers
      real       w_drain               ! water draining by gravity (mm)
      real       w_in                  ! water coming into layer (mm)
      real       w_out                 ! water going out of layer (mm)
      real       w_tot                 ! total water in layer at start (mm)
 
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
                ! flux into layer 1 = infiltration (mm).
 
      w_in = 0.0
 
                ! calculate drainage and water
                ! redistribution.
 
      call fill_real_array (flux, 0.0, max_layer)
      num_layers = count_of_real_vals (p_dlayer, max_layer)
 
      do 240 layer = 1, num_layers
 
             ! get total water concentration in layer
 
         w_tot = g_sw_dep(layer) + w_in
 
             ! get excess water above saturation & then water left
             ! to drain between sat and dul.  Only this water is
             ! subject to swcon. The excess is not - treated as a
             ! bucket model. (mm)
 
         if (w_tot.gt.g_sat_dep(layer)) then
            excess = w_tot - g_sat_dep(layer)
            w_tot = g_sat_dep(layer)
         else
            excess = 0.0
         endif
 
         if (w_tot.gt. g_dul_dep(layer)) then
            w_drain = (w_tot - g_dul_dep(layer)) *p_swcon(layer)
         else
            w_drain = 0.0
         endif
 
             ! get water draining out of layer (mm)
 
         w_out = excess + w_drain
         flux(layer) = w_out
 
             ! drainage out of this layer goes into next layer down
 
         w_in = w_out
240   continue
 
      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      subroutine soilwat2_unsat_flow (flow)
*     ===========================================================
      implicit none
      include   'soilwat2.inc'
      include 'data.pub'
      include 'error.pub'
 
*+  Sub-Program Arguments
      real       flow (*)              ! (output) water movement out of
                                       !    each layer (mm)
 
*+  Purpose
*       calculate unsaturated flow below drained upper limit
 
*+  Mission Statement
*     Calculate Unsaturated Solute and Water Flow
 
*+  Changes
*        210191   specified and programmed jngh (j hargreaves
*        290892   jngh changed soil water to depth of water
*        151292   jngh changed common blocks
*        120294   jpd add apswtspr.blk for p_diffus_const,p_diffus_slope
*        150294   mep added variable flow_max to constrain flow(layer) to
*                    a zero gradient for adjacent layers.
*        100795 jngh added limits for flow_max to esw_dep as sw was going
*                    below air_dry.
 
*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_unsat_flow')
 
*+  Local Variables
      real       esw_dep1              ! extractable soil water in current
                                       ! layer (mm)
      real       esw_dep2              ! extractable soil water in next
                                       ! layer below (mm)
      real       dbar                  ! average diffusivity used to calc
                                       !    unsaturated flow between layers
      integer    layer                 ! layer counter for current layer
      integer    second_last_layer     ! last layer for flow
      integer    num_layers            ! number of layers
      integer    next_layer            ! layer counter for next lower layer
      real       flow_max              ! maximum flow to make gradient between
                                       ! layers equal zero
      real       theta1                ! sw content above ll15 for current
                                       !    layer (cm/cm)
      real       theta2                ! sw content above ll15 for next lower
                                       !    layer (cm/cm)
      real       w_out                 ! water moving up out of this layer (mm)
                                       ! +ve = up to next layer
                                       ! -ve = down into this layer
*
      real       this_layer_cap        ! capacity of this layer to accept water
                                       ! from layer below (mm)
      real       next_layer_cap        ! capacity of nxt layer to accept water
                                       ! from layer above (mm)
*
      real       sw1                   ! sw for current layer (mm/mm)
      real       sw2                   ! sw for next lower layer (mm/mm)
      real       gradient              ! driving force for flow
      real       sum_inverse_dlayer    !
*
      real       dlayer1               ! depth of current layer (mm)
      real       dlayer2               ! depth of next lower layer (mm)
      real       ave_dlayer            ! average depth of current and next
                                       ! layers (mm)
*
      real       sw_dep1               ! soil water depth in current layer (mm)
      real       sw_dep2               ! soil water depth in next layer (mm)
*
      real       ll15_dep1             ! 15 bar lower limit sw depth in current
                                       ! layer (mm)
      real       ll15_dep2             ! 15 bar lower limit sw depth in next
                                       ! layer (mm)
*
      real       sat_dep1              ! saturated sw depth in current layer
                                       ! (mm)
      real       sat_dep2              ! saturated sw depth in next layer (mm)
      real       swg                   ! sw differential due to gravitational
                                       ! pressure head (mm)
 
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      num_layers = count_of_real_vals (p_dlayer, max_layer)
 
        ! *** calculate unsaturated flow below drained upper limit (flow)***
 
      call fill_real_array (flow, 0.0, max_layer)
 
                ! second_last_layer is bottom layer but 1.
 
      second_last_layer = num_layers - 1
 
      w_out = 0.0
      do 500 layer = 1, second_last_layer
         next_layer = layer + 1
 
         dlayer1    = p_dlayer(layer)
         dlayer2    = p_dlayer(next_layer)
         ave_dlayer = (dlayer1 + dlayer2) *0.5
 
         sw_dep1    = g_sw_dep(layer)
         sw_dep2    = g_sw_dep(next_layer)
 
         ll15_dep1  = g_ll15_dep(layer)
         ll15_dep2  = g_ll15_dep(next_layer)
 
         sat_dep1   = g_sat_dep(layer)
         sat_dep2   = g_sat_dep(next_layer)
 
         esw_dep1   = l_bound ((sw_dep1 - w_out) - ll15_dep1, 0.0)
         esw_dep2   = l_bound (sw_dep2 - ll15_dep2, 0.0)
 
                ! theta1 is excess of water content above lower limit,
                ! theta2 is the same but for next layer down.
 
         theta1 = divide (esw_dep1, dlayer1, 0.0)
         theta2 = divide (esw_dep2, dlayer2, 0.0)
 
           ! find diffusivity, a function of mean thet.
 
         dbar  = p_diffus_const
     :         * exp (p_diffus_slope * (theta1 + theta2) * 0.5)
 
            ! testing found that a limit of 10000 (as used in ceres-maize)
            ! for dbar limits instability for flow direction for consecutive
            ! days in some situations.
 
         dbar = bound (dbar, 0.0, 10000.0)
 
         sw1 = divide ((sw_dep1 - w_out), dlayer1, 0.0)
         sw1 = l_bound (sw1, 0.0)
 
         sw2 = divide (sw_dep2, dlayer2, 0.0)
         sw2 = l_bound (sw2, 0.0)
 
            ! gradient is defined in terms of absolute sw content
 
cjh          subtract gravity gradient to prevent gradient being +ve when
cjh          flow_max is -ve, resulting in sw > sat.
 
         gradient  = divide ((sw2 - sw1), ave_dlayer, 0.0)
     :             - c_gravity_gradient
 
            !  flow (positive up) = diffusivity * gradient in water content
 
         flow(layer) = dbar * gradient
 
            ! flow will cease when the gradient, adjusted for gravitational
            ! effect, becomes zero.
 
         swg = c_gravity_gradient* ave_dlayer
 
            ! calculate maximum flow
 
         sum_inverse_dlayer = divide (1.0, dlayer1, 0.0)
     :                      + divide (1.0, dlayer2, 0.0)
         flow_max = divide ((sw2 - sw1 - swg), sum_inverse_dlayer, 0.0)
 
         if (flow(layer) .lt. 0.0) then
            ! flow is down to layer below
            ! check capacity of layer below for holding water from this layer
            ! and the ability of this layer to supply the water
 
            next_layer_cap = l_bound (sat_dep2 - sw_dep2, 0.0)
            flow_max = l_bound (flow_max, -next_layer_cap)
cjh
            flow_max = l_bound (flow_max, -esw_dep1)
            flow(layer) = l_bound (flow(layer), flow_max)
 
         elseif (flow(layer) .gt. 0.0) then
            ! flow is up from layer below
            ! check capacity of this layer for holding water from layer below
            ! and the ability of the layer below to supply the water
 
            this_layer_cap = l_bound (sat_dep1 - (sw_dep1 - w_out), 0.0)
            flow_max = u_bound (flow_max, this_layer_cap)
cjh
            flow_max = u_bound (flow_max, esw_dep2)
            flow(layer) = u_bound (flow(layer), flow_max)
         else
            ! no flow
         endif
 
 
            ! For conservation of water, store amount of water moving
            ! between adjacent layers to use for next pair of layers in profile
            ! when calculating theta1 and sw1.
 
          w_out = flow(layer)
 
  500 continue
 
      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      subroutine soilwat2_check_profile (layer)
*     ===========================================================
      implicit none
      include   'const.inc'            ! err_internal
      include   'soilwat2.inc'
      include 'science.pub'
      include 'data.pub'
      include 'error.pub'
 
*+  Sub-Program Arguments
      integer    layer                 ! (input) layer counter
 
*+  Purpose
*       checks validity of soil water parameters for a soil profile layer
 
*+  Notes
*           reports an error if
*           - g_ll15_dep, dul_dep, and sat_dep are not in ascending order
*           - ll15 is below min_sw
*           - sat is above max_sw
*           - sw > sat or sw < min_sw
 
*+  Mission Statement
*     Check Soil Water Parameters for each layer
 
*+  Changes
*       180789 specified and programmed (jngh)
*       280491 jngh - reworked error messages and their formats.- cr54
*       290591 jngh removed else if stmt for better checking - cr55
*                   removed double call to lyrchk - cr56
*       270592 jngh removed lyrchk from external calls and call
*                   that was commented out.  also corrected error
*                   conditions sections - cr303
*                   reformatted error messages for better layout - cr305
*       290892 jngh changed soil water to depth of water
*       151292 jngh changed common blocks
*       250594 jngh added margins for rounding errors. - jpd pers comm.
*       190595 jngh changed max sw to be calculated from specific bulk density
*       300695 jngh changed min of sw to be airdry
*       050795 jngh fixed error message for sw vs air_dry error.
 
*+  Constant Values
      real       min_sw                ! lowest acceptable value for sw
                                       !   (mm water/mm soil)
      parameter (min_sw  = 0.0)
*
      real      max_sw_margin          ! margin for measurement error (mm/mm)
      parameter (max_sw_margin = 0.01)
 
*+  Local Variables
      real       dul                   ! drained upper limit water content
                                       !   of layer (mm water/mm soil)
      real       dul_errmargin         ! rounding error margin for dulc
      character  err_messg*300         ! error message
      real       ll15                  ! lower limit at 15 bars water content
                                       !   of layer (mm water/mm soil)
      real       air_dry_errmargin     ! rounding error margin for air_dryc
      real       air_dry               ! lower limit at air dry water content
                                       !   of layer (mm water/mm soil)
      real       ll15_errmargin        ! rounding error margin for ll15c
      real       sat                   ! saturated water content of layer
                                       !   (mm water/mm soil)
      real       sat_errmargin         ! rounding error margin for satc
      real       sw                    ! soil water content of layer l
                                       !   (mm water/mm soil)
      real       sw_errmargin          ! rounding error margin for swc
*
      real       max_sw                ! largest acceptable value for sat
                                       !   (mm water/mm soil)
 
*- Implementation Section ----------------------------------
      max_sw = 1.0 - divide (g_bd(layer), c_specific_bd, 0.0)
         ! ie Total Porosity
 
      sw = divide (g_sw_dep(layer), p_dlayer(layer), 0.0)
      sat = divide (g_sat_dep(layer), p_dlayer(layer), 0.0)
      dul = divide (g_dul_dep(layer), p_dlayer(layer), 0.0)
      ll15 = divide (g_ll15_dep(layer), p_dlayer(layer), 0.0)
      air_dry = divide (g_air_dry_dep(layer), p_dlayer(layer), 0.0)
 
      sw_errmargin = error_margin (sw)
      sat_errmargin = error_margin (sat)
      dul_errmargin = error_margin (dul)
      ll15_errmargin = error_margin (ll15)
      air_dry_errmargin = error_margin (air_dry)
 
      if (air_dry + air_dry_errmargin .lt. min_sw) then
         write (err_messg, '(a, g17.6e3, a, i3, 2a, g17.6e3)')
     :           ' Air dry lower limit of ', air_dry
     :          ,' in layer ', layer
     :          , new_line
     :          ,'         is below acceptable value of ', min_sw
         call warning_error (err_internal, err_messg)
      else
      endif
 
      if (ll15 + ll15_errmargin .lt. air_dry - air_dry_errmargin) then
         write (err_messg, '(a, g17.6e3, a, i3, 2a, g17.6e3)')
     :           ' 15 bar lower limit of ', ll15
     :          ,' in layer ', layer
     :          , new_line
     :          ,'         is below air dry value of ', air_dry
         call warning_error (err_internal, err_messg)
      else
      endif
 
      if (dul + dul_errmargin .le. ll15 - ll15_errmargin) then
         write (err_messg, '(a, g17.6e3, a, i3, 2a, g17.6e3)')
     :            ' drained upper limit of ',dul
     :           ,' in layer ', layer
     :           , new_line
     :           ,'         is at or below lower limit of ', ll15
         call warning_error (err_internal, err_messg)
      else
      endif
 
      if (sat + sat_errmargin .le. dul - dul_errmargin) then
         write (err_messg, '(a, g17.6e3, a, i3, 2a, g17.6e3)')
     :            ' saturation of ', sat
     :           ,' in layer ', layer
     :           , new_line
     :           ,'         is at or below drained upper limit of '
     :           , dul
         call warning_error (err_internal, err_messg)
      else
      endif
 
      if (sat - sat_errmargin .gt. max_sw + max_sw_margin) then
 
         write (err_messg, '(a, g17.6e3, a, i3, 3(2a, g17.6e3))')
     :            ' saturation of ', sat
     :           ,' in layer ', layer
     :           , new_line
     :           ,'         is above acceptable value of ', max_sw
     :           , new_line
     :           , 'You must adjust bulk density (bd) to below '
     :           , (1.0 - sat) * c_specific_bd
     :           , new_line
     :           , 'OR saturation (sat) to below ', max_sw
         call warning_error (err_internal, err_messg)
 
      else
      endif
 
      if (sw - sw_errmargin .gt. sat + sat_errmargin) then
         write (err_messg, '(a, g17.6e3, a, i3, 2a, g17.6e3)')
     :            ' soil water of ', sw
     :           ,' in layer ', layer
     :           , new_line
     :           ,'         is above saturation of ', sat
         call warning_error (err_internal, err_messg)
      else
      endif
 
      if (sw + sw_errmargin .lt. air_dry - air_dry_errmargin) then
         write (err_messg, '(a, g17.6e3, a, i3, 2a, g17.6e3)')
     :            ' soil water of ', sw
     :           ,' in layer ', layer
     :           , new_line
     :           ,'         is below air-dry value of ', air_dry
         call warning_error (err_internal, err_messg)
 
      else
      endif
 
      return
      end
 
 
 
*     ===========================================================
      subroutine soilwat2_layer_check (layer)
*     ===========================================================
      implicit none
      include   'const.inc'            ! err_user
      include   'soilwat2.inc'
      include 'data.pub'
      include 'read.pub'
      include 'write.pub'
      include 'error.pub'
 
*+  Sub-Program Arguments
      integer    layer                 ! (input) layer counter
 
*+  Purpose
*       checks that layer lies in range of 1 - num_layers
 
*+  Notes
*             reports error if layer < min_layer
*             or layer > num_layers
 
*+  Mission Statement
*     Check Soil Water Parameters for a given layer
 
*+  Changes
*       180789 specified and programmed (jngh)
*       221191 jngh expanded messages, l to layer and removed unused
*              common blocks. sprofl and swater - cr33
*                   parameterised min_layer - cr32
*       270592 jngh moved count_of_real_vals to global section
*                   declared count_of_real_vals in external calls - cr302
*                   moved num_layers to internal section - cr302
*                   num_layers changed to come from function and
*                   no longer from arguments.  included sprofl
*                   common block and max_layer.  also used count_of_real_vals.
 
*+  Constant Values
      integer    min_layer             ! lowest value for a layer number
      parameter (min_layer = 1)
 
*+  Local Variables
      character  err_messg*200         ! error message
      integer    num_layers            ! max layers
 
*- Implementation Section ----------------------------------
 
      num_layers = count_of_real_vals (p_dlayer, max_layer)
 
      if (layer.lt.min_layer) then
         write (err_messg,'(2(a,i3))')
     :                         ' soil layer no. ', layer
     :                        ,' is below mimimum of ', min_layer
         call warning_error (err_user, err_messg)
 
      else if (layer.gt.num_layers) then
         write (err_messg,'(2(a,i3))')
     :                         ' soil layer no. ', layer
     :                        ,' is above maximum of ', num_layers
         call warning_error (err_user, err_messg)
 
      endif
 
      return
      end
 
 
 
* ====================================================================
      subroutine soilwat2_read_constants ()
* ====================================================================
      implicit none
       include 'const.inc'
       include 'soilwat2.inc'
      include 'read.pub'
      include 'write.pub'
      include 'error.pub'
 
*+  Purpose
*      Read in all coefficients from coefficient file.
 
*+  Mission Statement
*     Read Constants from Ini file
 
*+  Changes
*     ???
*     190595 jngh added specific bulk density
*     040995 nih  added mobile and immobile solutes
*     200896 jngh changed N_flow/flux to Solute_Flow/flux
*     210896 jngh changed upper bound of c_canopy_eos_coef from 1 to 10
 
*+  Constant Values
       character  my_name*(*)          ! name of this procedure
       parameter (my_name = 'soilwat2_read_constants')
*
       character  section_name*(*)
       parameter (section_name = 'constants')
 
*+  Local Variables
       integer numvals                 ! number of values read from file
       character  evap_method*300
 
*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      call write_string (lu_scr_sum
     :                  ,new_line//'    - Reading constants')
 
      call read_real_var (section_name
     :                   , 'min_crit_temp', '(oC)'
     :                   , c_min_crit_temp, numvals
     :                   , 0.0, 10.0)
 
      call read_real_var (section_name
     :                   , 'max_crit_temp', '(oC)'
     :                   , c_max_crit_temp, numvals
     :                   , 0.0, 50.0)
 
      call read_real_var (section_name
     :                   , 'max_albedo', '()'
     :                   , c_max_albedo, numvals
     :                   , 0.0, 1.0)
 
      call read_real_var (section_name
     :                   , 'A_to_evap_fact', '()'
     :                   , c_A_to_evap_fact, numvals
     :                   , 0.0, 1.0)
 
      call read_real_var (section_name
     :                   , 'canopy_eos_coef', '()'
     :                   , c_canopy_eos_coef, numvals
     :                   , 0.0, 10.0)
 
      call read_real_var (section_name
     :                   , 'sw_top_crit', '()'
     :                   , c_sw_top_crit, numvals
     :                   , 0.0, 1.0)
 
      call read_real_var (section_name
     :                   , 'sumes1_max', '()'
     :                   , c_sumes1_max, numvals
     :                   , 0.0, 1000.0)
 
      call read_real_var (section_name
     :                   , 'sumes2_max', '()'
     :                   , c_sumes2_max, numvals
     :                   , 0.0, 1000.0)
 
      call read_real_var (section_name
     :                   , 'solute_flow_eff', '()'
     :                   , c_Solute_flow_eff, numvals
     :                   , 0.0, 1.0)
 
      call read_real_var (section_name
     :                   , 'solute_flux_eff', '()'
     :                   , c_Solute_flux_eff, numvals
     :                   , 0.0, 1.0)
 
      call read_real_var (section_name
     :                   , 'gravity_gradient', '()'
     :                   , c_gravity_gradient, numvals
     :                   , 0.0, 1.0)
 
      call read_real_var (section_name
     :                   , 'specific_bd', '()'
     :                   , c_specific_bd, numvals
     :                   , 0.0, 3.0)
 
      call read_real_var (section_name
     :                   , 'hydrol_effective_depth', '(mm)'
     :                   , c_hydrol_effective_depth, numvals
     :                   , 1.0, 1000.0)
 
      call read_char_array (section_name
     :                   ,'mobile_solutes'
     :                   , max_solute
     :                   , '()'
     :                   , c_mobile_solutes
     :                   , numvals)
 
      call read_char_array (section_name
     :                   ,'immobile_solutes'
     :                   , max_solute
     :                   , '()'
     :                   , c_immobile_solutes
     :                   , numvals)
 
      call read_real_array (section_name
     :                   , 'canopy_fact', max_coeffs, '()'
     :                   , c_canopy_fact, g_num_canopy_fact
     :                   , 0.0, 1.0)
 
      call read_real_array (section_name
     :                   , 'canopy_fact_height', max_coeffs, '(mm)'
     :                   , c_canopy_fact_height, numvals
     :                   , 0.0, 100000.0)
      if (numvals.ne. g_num_canopy_fact) then
         call fatal_error (err_user
     :                    , 'No. of canopy_fact coeffs do not match '
     :                    //'no. of canopy_fact_height coeffs.')
      else
         ! matching number of coeffs
      endif
 
      call read_real_var (section_name
     :                   , 'canopy_fact_default', '()'
     :                   , c_canopy_fact_default, numvals
     :                   , 0.0, 1.0)
 
      evap_method = 'unknown'
      call read_char_var(section_name
     :                   , 'act_evap_method', '()'
     :                   , evap_method, numvals)
 
      if (evap_method .eq. 'ritchie') then
         c_evap_method = c_ritchie_method
 
      else if (evap_method .eq. 'bs_a') then
         c_evap_method = c_bs_a_method
 
      else if (evap_method .eq. 'bs_b') then
         c_evap_method = c_bs_b_method
 
      else if (evap_method .eq. 'bs_acs_jd') then
         c_evap_method = c_bs_acs_method
 
      else if (evap_method .eq. 'rickert') then
         c_evap_method = c_rickert_method
 
      else
         c_evap_method = -1  ! Force error somewhere later..
 
      endif
 
      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      subroutine soilwat2_soil_property_param ()
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'soilwat2.inc'
      include 'read.pub'
      include 'write.pub'
      include 'error.pub'
 
*+  Purpose
*       input initial values from soil property file.
 
*+  Mission Statement
*     Read Soil Parameters
 
*+  Changes
*       210191 specified and programmed jngh (j hargreaves
*       290591 jngh declared true as logical variable, defined
*               true=.true. - cr41
*               removed ferror=false - cr74
*       160992 jngh introduced write_string function for dual output
*       051093 jngh added fatal error to halt simulation
*       131093 markl added input of c_cn_red and c_cn_cov for cover/cn response
*       131093 markl added input of p_cona for soil evaporation
*       131093 markl added input of residue_wt for effects of residue on
*                          potential soil evaporation
*       131093 markl removed input of p_swcon from this subroutine and added
*                             it in soilwat2_soil_profile_param
*       190194 jpd  changed ulmcona from 5. to 10.
*                               query limits for cnred&cncov
*       290194 jpd  removed input of residue_wt,residue_cover, now in residue.
*       for
*       010994 jpd  removed input for crop_cover from parameter file.
*                           Currently (6/9/94) use 'lai' from crop modules
*       150994 jpd  added input for crop cover/ runoff  switch -
*                   'crpcov_rnof_switch'
*
*       300994 jpd added c_hydrol_effective_depth
*       181094 jpd removed crpcov_rnof option with '!!'
*       120195 jngh removed crop cover runoff switch
*                   removed combining residue cover and crop cover as done
*                   when getting other total cover from modules
*       210395 jngh changed from soilwat2_section to a parameters section
*       200896 jngh changed cn2 to cn2_bare
*                   changed reading of runoff filename to be optional
 
*+  Constant Values
      character  my_name*(*)            ! name of this module
      parameter (my_name = 'soilwat2_soil_property_param')
*
       character  section_name*(*)
       parameter (section_name = 'parameters')
 
*+  Local Variables
      integer    numvals               ! number of values returned
 
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      call write_string (lu_scr_sum
     :          ,new_line//'   - Reading Soil Property Parameters')
 
          ! get runoff source
      call read_char_var_optional (section_name
     :                   ,'observed_runoff', '()'
     :                   , g_obsrunoff_name
     :                   , numvals)
 
      if ( numvals .eq. 0 .or.
     :     g_obsrunoff_name .eq. 'blank') then
         g_obsrunoff_name = blank              ! blank != 'blank' !!!
      else
         ! nothing - there's a valid string in g_obsrunoff_name
      endif
 
          ! get sw parameters
 
      call read_real_var (section_name
     :                   , 'insoil', '()'
     :                   , p_insoil, numvals
     :                   , 0.0, 10.0)
 
      call read_real_var (section_name
     :                   , 'diffus_const', '()'
     :                   , p_diffus_const, numvals
     :                   , 0.0, 1000.0)
 
      call read_real_var (section_name
     :                   , 'diffus_slope', '()'
     :                   , p_diffus_slope, numvals
     :                   , 0.0, 100.0)
 
      call read_real_var (section_name
     :                   , 'cn2_bare', '()'
     :                   , p_cn2_bare, numvals
     :                   , 1.0, 100.0)
 
      call read_real_var (section_name
     :                   , 'cn_red', '()'
     :                   , p_cn_red, numvals
     :                   , 0.0, p_cn2_bare - 0.00009)
 
      call read_real_var (section_name
     :                   , 'cn_cov', '()'
     :                   , p_cn_cov, numvals
     :                   , 0.0, 1.0)
 
      call read_real_var (section_name
     :                   , 'salb', '()'
     :                   , p_salb, numvals
     :                   , 0.0001, 1.0)
 
*     Extra parameters for evaporation models:
      if (c_evap_method .eq. c_ritchie_method .or.
     :     c_evap_method .eq. c_bs_acs_method) then
         call read_real_var (section_name
     :        , 'cona', '()'
     :        , p_cona, numvals
     :        , 0.0001, 10.0)
 
      else
 
         p_cona = 0.0001
      endif
 
      if (c_evap_method .eq. c_ritchie_method .or.
     :     c_evap_method .eq. c_bs_acs_method) then
         call read_real_var (section_name
     :        , 'u', '()'
     :        , p_u, numvals
     :        , 0.0001, 40.0)
      else
         p_u = 0.0001
      endif
 
      if (c_evap_method .eq. c_bs_a_method .or.
     :     c_evap_method .eq. c_bs_b_method .or.
     :     c_evap_method .eq. c_bs_acs_method) then
         call read_real_var (section_name
     :        , 'beta', '()'
     :        , p_beta, numvals
     :        , 0.0, 3.5)
 
      else
 
         p_beta = 0.0
      endif
 
      if (c_evap_method .eq. c_rickert_method) then
         call read_real_var (section_name
     :        , 'max_evap', '()'
     :        , p_max_evap, numvals
     :        , 0.9, 20.0)
 
      else
 
         p_max_evap = 0.0
      endif
 
      call read_char_var_optional (section_name
     :                   ,'eo_source', '()'
     :                   , g_eo_source
     :                   , numvals)
      if (numvals .le. 0) then
          g_eo_source = blank
      else
      endif
 
      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      subroutine soilwat2_soil_profile_param ()
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'soilwat2.inc'
      include 'data.pub'
      include 'error.pub'
      include 'write.pub'
      include 'read.pub'
 
*+  Purpose
*       input initial values from soil parameter file.
 
*+  Mission Statement
*     Read Initial Soil Profile Values
 
*+  Changes
*       210191 specified and programmed jngh (j hargreaves
*       281191 jngh tidy up as per cr257.
*                   changed error checking to be more responsive.
*                   removed unused variables & corrected lmm to llm - cr256
*                   check ios flag instead of ferror - cr258
*       290892 jngh changed soil water to depth of water
*       160992 jngh introduced write_string function for dual output
*       131093 markl added p_swcon as an array for each soil layer
*       190194 jpd   added air_dry_dep as an array for each soil layer
*       210395 jngh changed from soilwat2_section to a parameters section
*       190595 jngh added bulk density
*       190897 nih  moved insoil sw set from higher level for reuse reasons
 
*+  Constant Values
      character  my_name*(*)         ! name of this module
      parameter (my_name = 'soilwat2_soil_profile_param')
*
       character  section_name*(*)
       parameter (section_name = 'parameters')
 
*+  Local Variables
      integer    layer                 ! layer number in loop
      integer    num_layers            ! number of layers in profile
      integer    numvals               ! number of values returned
      real       air_dry (max_layer)   ! air dry soilwat2er content
                                       ! for soil layer l (mm water/mm soil)
      real       dul (max_layer)       ! drained upper limit soilwat2er content
                                       ! for soil layer l (mm water/mm soil)
      real       ll15 (max_layer)      ! 15 bar lower limit of extractable
                                       ! soil water (mm water/mm soil)
      real       sat (max_layer)       ! saturated water content for layer l
                                       ! (mm water/mm soil)
      real       sw(max_layer)         ! soil water content (mm water/mm soil)
      character msg*200                ! message to summary file
 
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      call write_string (lu_scr_sum
     :          ,new_line//'   - Reading Soil Profile Parameters')
 
                 ! get sw properties
 
      call read_real_array (section_name
     :                     , 'dlayer', max_layer, '(mm)'
     :                     , p_dlayer, numvals
     :                     , 0.0, 10000.0)
 
      call read_real_array (section_name
     :                     , 'sat', max_layer, '()'
     :                     , sat, numvals
     :                     , 0.0, 1000.0)
 
      call read_real_array (section_name
     :                     , 'dul', max_layer, '()'
     :                     , dul, numvals
     :                     , 0.0, 1000.0)
 
      call read_real_array (section_name
     :                     , 'sw', max_layer, '()'
     :                     , sw, numvals
     :                     , 0.0, 1000.0)
 
      call read_real_array (section_name
     :                     , 'll15', max_layer, '()'
     :                     , ll15, numvals
     :                     , 0.0, 1000.0)
 
      call read_real_array (section_name
     :                     , 'air_dry', max_layer, '()'
     :                     , air_dry, numvals
     :                     , 0.0, 10.0)
 
      call read_real_array (section_name
     :                     , 'swcon', max_layer, '()'
     :                     , p_swcon, numvals
     :                     , 0.0, 1000.0)
 
      call read_real_array ( section_name
     :                     , 'bd', max_layer, '(g/cc)'
     :                     , g_bd, numvals
     :                     , 0.01, 10.0)
 
      num_layers = count_of_real_vals (p_dlayer, max_layer)
      do 1010 layer = 1,num_layers
 
         g_air_dry_dep(layer) = air_dry(layer)*p_dlayer(layer)
         g_dul_dep(layer)     = dul(layer)    *p_dlayer(layer)
         g_ll15_dep(layer)    = ll15(layer)   *p_dlayer(layer)
         g_sat_dep(layer)     = sat(layer)    *p_dlayer(layer)
         g_sw_dep(layer)      = sw(layer)     *p_dlayer(layer)
 
         call soilwat2_check_profile (layer)
1010  continue
 
          ! get sw parameters
 
      if (p_insoil.ge.0.0 .and. p_insoil.le.1.0) then
 
         msg = 'Soil water in parameter file is being overridden by' //
     .         new_line //
     .         'the insoil parameter which is between 0 and 1'
         call write_string (lu_scr_sum
     :                     ,new_line // msg)
         call soilwat2_set_default ()
      else
      endif
 
 
      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      subroutine soilwat2_set_default ()
*     ===========================================================
      implicit none
      include   'soilwat2.inc'
      include 'data.pub'
      include 'error.pub'
 
*+  Purpose
*       set default soil water values
 
*+  Mission Statement
*     Set Default Soil Water Values
 
*+  Changes
*        210191   specified and programmed jngh (j hargreaves
*        290892   jngh changed soil water to depth of water
 
*+  Calls
                                       ! cells used in array
 
*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_set_default')
 
*+  Local Variables
      integer    layer                 ! layer number in loop
      integer    num_layers            ! number of layers used in profile
 
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
                ! initialize sw
                ! set up default soil water profile
 
                ! we want to calculate default
 
      call fill_real_array (g_sw_dep, 0.0, max_layer)
      num_layers = count_of_real_vals (p_dlayer, max_layer)
 
      do 1000 layer = 1,num_layers
 
                 ! set default according to insoil fraction of plant-
                 ! available water
 
         g_sw_dep(layer) = g_ll15_dep(layer)
     :                   + (g_dul_dep(layer) - g_ll15_dep(layer))
     :                   * p_insoil
 
         call soilwat2_layer_check (layer)
         call soilwat2_check_profile (layer)
 
1000  continue
 
      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      subroutine soilwat2_evap_init
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'soilwat2.inc'
      include 'error.pub'
 
*+  Purpose
*     Wrapper for evaporation methods
 
*+  Mission Statement
*     Evaporation Initialisation
 
*+  Changes
*       210191 specified and programmed jngh (j hargreaves
 
*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_evap_init')
 
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
      if (c_evap_method .eq. c_ritchie_method) then
         call soilwat2_ritchie_init ()
 
      else if (c_evap_method .eq. c_bs_a_method) then
         call soilwat2_bs_a_init ()
 
      else if (c_evap_method .eq. c_bs_b_method) then
         call soilwat2_bs_b_init ()
 
      else if (c_evap_method .eq. c_bs_acs_method) then
         call soilwat2_bs_acs_init ()
 
      else if (c_evap_method .eq. c_rickert_method) then
         call soilwat2_rickert_init ()
 
      else
         call fatal_error(err_user,
     :        'Tried to initialise unknown evaporation method')
 
      endif
 
      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      subroutine soilwat2_ritchie_init
*     ===========================================================
      implicit none
      include   'soilwat2.inc'
      include 'data.pub'
      include 'error.pub'
 
*+  Purpose
*       initialize ritchie evaporation model
 
*+  Mission Statement
*       Initialise ritchie evaporation model      
 
*+  Changes
*       210191 specified and programmed jngh (j hargreaves
*       290892 jngh changed soil water to depth of water
*       160992 jngh changed constants to named constants
*       131093 markl added replaced 3.5 constant with p_cona variable
*
*       190194 jpd initialization for evap needs re-working.
*      no check on sumes1 wrt. u. sumes1 will be =>10mm if swr_top >sw_crit_top
 
*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_ritchie_init')
 
*+  Local Variables
                                       ! stage 2 evaporation occurs
      real       swr_top               ! ratio available sw :
                                       !    potentially available sw
                                       ! in top layer
 
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
          ! set up evaporation stage
 
      swr_top = divide (g_sw_dep(1) - g_ll15_dep(1)
     :                , g_dul_dep(1) - g_ll15_dep(1), 0.0)
      swr_top = bound (swr_top, 0.0, 1.0)
 
          ! are we in stage1 or stage2 evap?
      if (swr_top.lt.c_sw_top_crit) then
 
             ! stage 2 evap
         g_sumes2 = c_sumes2_max
     :            - c_sumes2_max * divide (swr_top, c_sw_top_crit, 0.0)
         g_sumes1 = p_u
         g_t = (divide (g_sumes2, p_cona, 0.0))**2
      else
 
             ! stage 1 evap
         g_sumes2 = 0.0
         g_sumes1 = c_sumes1_max - c_sumes1_max *swr_top
         g_t = 0.0
      endif
 
      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      subroutine soilwat2_bs_a_init
*     ===========================================================
      implicit none
      include   'soilwat2.inc'
      include 'data.pub'
      include 'error.pub'
 
*+  Purpose
*       B&S option A initialisation
 
*+  Mission Statement
*       Initialise B&S (A) model for evaporation
 
*+  Changes
*       210191 specified and programmed jngh (j hargreaves
 
*+  Calls
!      real       bound                 ! function to contain within bounds
 
*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_bs_a_init')
 
*+  Local Variables
*      character err_mesg*300    ! message string
      real       sumes_max
 
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
*     B&S evaporation:
*      cumulative actual evap = beta * sqrt(cumulative eos)
 
      sumes_max = g_dul_dep(1) - g_air_dry_dep(1)
      g_sumes = g_dul_dep(1) - g_sw_dep(1)
 
      if (g_sumes .le. 0.0) then
 
*     ! initial sw is at, or above, DUL.
 
         g_sumes = 0.0
         g_sumes1 = 0.0
         g_sumes2 = 0.0
         g_sumeos = 0.0
 
      else if (g_sumes .gt. sumes_max) then
 
*     Initial sw is less than air_dry(1):
*     Write a warning message to summary file.
*PdeV. This check is in soilwat2_check_profile. Is it necessary here?
*         write (err_messg, '(a, g17.6e3, a, 2a, g17.6e3)')
*     :        ' soil water of ', g_sw_dep(1)
*     :        ,' in top layer '
*     :        , new_line
*     :        ,' is below air_dry value of ', g_air_dry_dep(1)
*         call warning_error (err_internal, err_messg)
 
         g_sumes = sumes_max
         g_sumes1 = p_beta**2
         g_sumes2 = g_sumes - g_sumes1
         g_sumeos = divide (g_sumes**2, p_beta**2, 0.0)
 
 
      elseif (g_sumes .ge. (p_beta**2)) then
 
*     Initial sw is not close to DUL.
*      1st stage evaporation is finished, start in 2nd stage
 
         g_sumes1 = p_beta**2
         g_sumes2 = g_sumes - g_sumes1
         g_sumeos = divide (g_sumes**2, p_beta**2, 0.0)
 
      else
 
*     Initial sw is close to DUL.
*      We're in 1st stage evaporation.
 
         g_sumes1 = g_sumes
         g_sumes2 = 0.0
         g_sumeos = g_sumes
 
      endif
 
      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      subroutine soilwat2_bs_b_init
*     ===========================================================
      implicit none
      include   'soilwat2.inc'
      include 'error.pub'
 
*+  Purpose
*     B&S option B initialisation
 
*+  Mission Statement
*     Initialise B&S (B) model for evaporation
 
*+  Changes
*       210191 specified and programmed jngh (j hargreaves
 
*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_bs_b_init')
 
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
cpdev Andy didn't write anything for option B initialisation. But I think
c     he should have. Any ideas? Perhaps
      call soilwat2_bs_a_init()
 
                                ! Nothing
      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      subroutine soilwat2_bs_acs_init
*     ===========================================================
      implicit none
      include   'soilwat2.inc'
      include 'data.pub'
      include 'error.pub'
 
*+  Purpose
*     B&S option B initialisation. (Andy smith + John dimes' version)
*
*     PdeV - I hope andy wasn't recycling variable names here, as this is
*     the only time cona and u are used with this model.
 
*+  Mission Statement
*     Initialise B&S (B, ACS) model for evaporation
 
*+  Changes
*       210191 specified and programmed jngh (j hargreaves
 
*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_bs_acs_init')
 
*+  Local Variables
*     acs/jpd
*     NOTE: sumes2_max & sumes_max only apply for initialization.
*     For model run, evaporation can continue from layr(1)indefinitly
*     because water is moving up from layer below (by unsaturated flow).
      real       sumes2_max     ! upper limit of sumes2. Related to
                                ! evaporative water capacity of layr(1)
!      real       sumes_max      ! upper limit of cumulative soil evap
!                                ! for B&S. Also f(evap.wat.cap layr(1))
 
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      sumes2_max = g_dul_dep(1) - g_air_dry_dep(1) - p_u
 
      if ((g_dul_dep(1) - g_sw_dep(1)) .LT. p_u) then
                                ! In first stage
         g_sumes1 = g_dul_dep(1) - g_sw_dep(1)
 
         if(g_sumes1 .lt. 0.0) then
            g_sumes1 = 0.0      ! initial sw greater than DUL
         else
                                !
         endif
         g_sumes2 = 0.0
         g_t = 0.0
 
      else
                                ! In second stage
         g_sumes1 = p_u
         g_sumes2 = g_dul_dep(1) - g_sw_dep(1) - p_u
 
         if (g_sumes2 .GT. sumes2_max) then
 
*     init sw must be .lt. air_dry
            g_sumes2 = sumes2_max
 
         endif
 
         g_t = divide(g_sumes2, p_cona, 0.0) **2
 
      endif
 
      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      subroutine soilwat2_rickert_init
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'soilwat2.inc'
      include 'data.pub'
      include 'intrface.pub'
      include 'error.pub'
 
*+  Purpose
*     Rickert initialisation
 
*+  Mission Statement
*     Calculate evaporation using Rickert model      
 
*+  Changes
*       210191 specified and programmed jngh (j hargreaves
 
*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_rickert_init')
 
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (p_diffus_const .gt. 0.0 .or.
     :     p_diffus_slope .gt. 0.0) then
         call warning_error (err_user,
     :     'diffus_const and diffus_slope should be off for rickert')
      else
                                ! Nothing
      endif
      call pop_routine (my_name)
      return
      end
 
 
 
* ====================================================================
      subroutine soilwat2_get_other_variables ()
* ====================================================================
      implicit none
      include 'error.pub'
 
*+  Purpose
*      get the value/s of a variable/array.
 
*+  Assumptions
*      assumes variable has the following format
*         <variable_name> = <variable_value/s> (<units>)
 
*+  Mission Statement
*     Get Other Variables
 
*+  Changes
*     090299 jngh put contents of routine into lower level routines
 
*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_get_other_variables')
 
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      call soilwat2_get_met_variables ()

      call soilwat2_get_residue_variables ()
 
      call soilwat2_get_crop_variables ()
 
      call soilwat2_get_solute_variables ()
 
      call soilwat2_get_environ_variables ()
 
      call pop_routine (my_name)
      return
      end
 
 
 
* ====================================================================
      subroutine soilwat2_get_met_variables ()
* ====================================================================
      implicit none
      include   'const.inc'            ! mes_get_variable, global_active
      include   'soilwat2.inc'
      include 'string.pub'
      include 'data.pub'
      include 'engine.pub'
      include 'intrface.pub'
      include 'error.pub'
 
*+  Purpose
*      get the value/s of a variable/array.
 
*+  Assumptions
*      assumes variable has the following format
*         <variable_name> = <variable_value/s> (<units>)
 
*+  Mission Statement
*     Get Other Variables
 
*+  Changes
*     301192 jngh
*     110393 jngh altered to new engine - immediate messages
*     010994 jpd  Added request for 'crop_cover' from crop modules
*     160994 jpd  add basal_cover request
*     230994  pdev  added cover_extra
*      191094 jngh changed interface routines
*     300695 jngh changed upper limit of residue wt to 100000
*     170895 nih  added read for solute information
*                 (removed old code for no3 and nh4)
*     070696 nih  changed get other for optimal speed
*     130896 jngh removed getting cover from canopy module.
*                 stored covers (green and total) in arrays
*     200896 jngh added capture of crop heights.
*     210896 jngh removed check of crops owning heights not being the same
*                 as crops owning green_cover.
 
*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_get_met_variables')
 
*+  Local Variables
      integer    numvals               ! number of values put into array
 
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      call get_real_var (unknown_module, 'maxt', '(oC)'
     :                                  , g_maxt, numvals
     :                                  , -100.0, 100.0)
 
      call get_real_var (unknown_module, 'mint', '(oC)'
     :                                  , g_mint, numvals
     :                                  , -100.0, 100.0)
 
      call get_real_var (unknown_module, 'radn', '(MJ/m^2)'
     :                                  , g_radn, numvals
     :                                  , 0.0, 1000.0)
 
      call get_real_var (unknown_module, 'rain', '(mm)'
     :                                  , g_rain, numvals
     :                                  , 0.0, 10000.0)
 
      call pop_routine (my_name)
      return
      end
 
 
 
* ====================================================================
      subroutine soilwat2_get_residue_variables ()
* ====================================================================
      implicit none
      include   'const.inc'            ! mes_get_variable, global_active
      include   'soilwat2.inc'
      include 'string.pub'
      include 'data.pub'
      include 'engine.pub'
      include 'intrface.pub'
      include 'error.pub'
 
*+  Purpose
*      get the value/s of a variable/array.
 
*+  Assumptions
*      assumes variable has the following format
*         <variable_name> = <variable_value/s> (<units>)
 
*+  Mission Statement
*     Get Met Variables
 
*+  Changes
*     301192 jngh
*     110393 jngh altered to new engine - immediate messages
*      191094 jngh changed interface routines
*     070696 nih  changed get other for optimal speed
 
 
*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_get_residue_variables')
 
*+  Local Variables
      integer    numvals               ! number of values put into array

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      call get_real_var_optional (unknown_module
     :                                  , 'residue_wt', '(kg/ha)'
     :                                  , g_residue_wt, numvals
     :                                  , 0.0, 100000.0)
 
      call get_real_var_optional (unknown_module, 'residue_cover', '()'
     :                                  , g_residue_cover, numvals
     :                                  , 0.0, 1.0)
  
      call pop_routine (my_name)
      return
      end
 
 
 
* ====================================================================
      subroutine soilwat2_get_crop_variables ()
* ====================================================================
      implicit none
      include   'const.inc'            ! mes_get_variable, global_active
      include   'soilwat2.inc'
      include 'string.pub'
      include 'data.pub'
      include 'engine.pub'
      include 'intrface.pub'
      include 'error.pub'
 
*+  Purpose
*      get the value/s of a variable/array.
 
*+  Assumptions
*      assumes variable has the following format
*         <variable_name> = <variable_value/s> (<units>)
 
*+  Mission Statement
*     Get crop Variables
 
*+  Changes
*     301192 jngh
*     110393 jngh altered to new engine - immediate messages
*     010994 jpd  Added request for 'crop_cover' from crop modules
*     160994 jpd  add basal_cover request
*     230994  pdev  added cover_extra
*      191094 jngh changed interface routines
*     070696 nih  changed get other for optimal speed
*     130896 jngh removed getting cover from canopy module.
*                 stored covers (green and total) in arrays
*     200896 jngh added capture of crop heights.
*     210896 jngh removed check of crops owning heights not being the same
*                 as crops owning green_cover.
 
*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_get_crop_variables')
 
*+  Local Variables
      real       canopy_height         ! height of canopy (mm)
      real       cover                 ! temporary cover variable (0-1)
      integer    crop                  ! loop index
      integer    crop_index            ! array index
      integer    numvals               ! number of values put into array
      character  owner_module*(max_module_name_size) ! owner module of variable
 
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
 
             ! Get green cover of each crop
             ! g_cover_green is all canopys green
 
      crop = 0
1000  continue
 
         call get_real_vars (crop+1, 'cover_green', '()'
     :                              , cover, numvals
     :                              , 0.0, 1.0)
 
         if (numvals.ne.0) then
            if (crop+1.le.max_crops) then
               crop = crop + 1
               call get_posting_Module (Owner_module)
               g_crop_module(crop) = owner_module
               g_cover_green(crop) = cover
               goto 1000
            else
               call fatal_error (err_user
     :            , 'Too many modules with green cover. Last module ='
     :            // owner_module)
            endif
         else
         endif
 
      g_num_crops = crop
 
            ! Get total cover of each crop
            ! g_cover_tot is all canopys green + dead
 
      crop = 0
2000  continue
         call get_real_vars (crop+1, 'cover_tot', '(mm)'
     :                              , cover, numvals
     :                              , 0.0, 1.0)
         if (numvals.ne.0) then
            if (crop+1.le.max_crops) then
               crop = crop + 1
               call get_posting_Module (Owner_module)
               if (owner_module.eq.g_crop_module(crop)) then
               g_cover_tot(crop) = cover
                  goto 2000
               else
                  call fatal_error (err_user
     :              , 'Modules with total cover do not match '
     :             // 'modules with green cover')
               endif
            else
               call fatal_error (err_user
     :            , 'Too many modules with total cover. Last module ='
     :            // owner_module)
            endif
         else
         endif
 
         if (crop.ne.g_num_crops) then
            call fatal_error (err_user
     :              , 'Number of modules with total cover different to '
     :              // 'number of modules with green cover.')
         else
         endif
 
            ! Get height of each crop
 
      crop = 0
      call fill_real_array (g_canopy_height, -1.0, g_num_crops)
2500  continue
         call get_real_vars (crop+1, 'height', '(mm)'
     :                              , canopy_height, numvals
     :                              , 0.0, 100000.0)
         if (numvals.ne.0) then
            if (crop+1.le.max_crops) then
               crop = crop + 1
               call get_posting_Module (Owner_module)
               crop_index = position_in_char_array
     :                (Owner_module, g_crop_module, g_num_crops)
               if (crop_index.ne.0) then
               g_canopy_height(crop_index) = canopy_height
                  goto 2500
               else
                  call fatal_error (err_user
     :              , 'Modules with height do not match '
     :             // 'modules with green cover')
               endif
            else
               call fatal_error (err_user
     :            , 'Too many modules with height. Last module ='
     :            // owner_module)
            endif
         else
         endif
 
cjh         if (crop.ne.g_num_crops) then
cjh            call warning_error (err_internal
cjh     :              , 'Number of modules with height is different to '
cjh     :              // 'number of modules with green cover.')
cjh         else
cjh         endif
 
      call pop_routine (my_name)
      return
      end
 
 
 
* ====================================================================
      subroutine soilwat2_get_solute_variables ()
* ====================================================================
      implicit none
      include   'const.inc'            ! mes_get_variable, global_active
      include   'soilwat2.inc'
      include 'string.pub'
      include 'data.pub'
      include 'engine.pub'
      include 'intrface.pub'
      include 'error.pub'
 
*+  Purpose
*      get the value/s of a variable/array.
 
*+  Assumptions
*      assumes variable has the following format
*         <variable_name> = <variable_value/s> (<units>)
 
*+  Mission Statement
*     Get solute Variables
 
*+  Changes
*     301192 jngh
*     110393 jngh altered to new engine - immediate messages
*      191094 jngh changed interface routines
*     170895 nih  added read for solute information
*                 (removed old code for no3 and nh4)
*     070696 nih  changed get other for optimal speed
 
*+  Calls
      character string_concat*32       ! function
 
*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_get_solute_variables')
 
*+  Local Variables
      integer    layer                 ! soil layer number counter
      character  min_name*32           ! name of solute minimum variable
      integer    numvals               ! number of values put into array
      integer    solnum                ! solute number counter
      real       temp_solute(max_layer)! temp solute array (kg/ha)
 
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      ! --------------- GET SOLUTE INFORMATION --------------
 
         ! Now find information for each of these solutes
         ! ----------------------------------------------
         do 3400 solnum = 1, g_num_solutes
 
            ! initialise tempory array
            call fill_real_array (temp_solute, 0.0, max_layer)
 
            call get_real_array
     :                       (g_solute_owners(solnum)
     :                       ,g_solute_names(solnum)
     :                       , max_layer
     :                       , '(kg/ha)'
     :                       , temp_solute
     :                       , numvals
     :                       , 0.0
     :                       , 30000.0)
 
            ! assign temp array to our global array
            do 3200 layer=1,max_layer
               g_solute(solnum,layer) = temp_solute(layer)
 3200       continue
 
            ! reinitialise tempory array
            call fill_real_array (temp_solute, 0.0, max_layer)
 
            ! Ask for solute info from APSIM system
            ! - NOTE if it does not exist in the system then
            ! the default of zero will be used.
            min_name = string_concat(g_solute_names(solnum),'_min')
 
            call get_real_array_optional
     :                       (g_solute_owners(solnum)
     :                       ,min_name
     :                       , max_layer
     :                       , '(kg/ha)'
     :                       , temp_solute
     :                       , numvals
     :                       , 0.0
     :                       , 1000.0)
 
            ! assign temp array to our global array
            do 3300 layer=1,max_layer
               g_solute_min(solnum,layer) = temp_solute(layer)
 3300       continue
 
 3400    continue

      call pop_routine (my_name)
      return
      end
 
 
* ====================================================================
      subroutine soilwat2_get_environ_variables ()
* ====================================================================
      implicit none
      include   'const.inc'            ! mes_get_variable, global_active
      include   'soilwat2.inc'
      include 'string.pub'
      include 'data.pub'
      include 'engine.pub'
      include 'intrface.pub'
      include 'error.pub'
 
*+  Purpose
*      get the value/s of a variable/array.
 
*+  Assumptions
*      assumes variable has the following format
*         <variable_name> = <variable_value/s> (<units>)
 
*+  Mission Statement
*     Get environment Variables
 
*+  Changes
*     301192 jngh
*      191094 jngh changed interface routines
*     070696 nih  changed get other for optimal speed
 

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_get_environ_variables')
 
*+  Local Variables
      integer    numvals               ! number of values put into array
 
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (g_eo_source .ne. blank) then
         g_eo_system = 0.0
         call get_real_var (unknown_module, g_eo_source, '()'
     :                                , g_eo_system, numvals
     :                                , 0.0, 100.0)
      else
      endif
 
      if (g_obsrunoff_name .ne. blank) then
 
         call get_real_var_optional (unknown_module,
     :                               g_obsrunoff_name, '()',
     :                               g_obsrunoff, numvals,
     :                               0.0, 1000.0)
 
         g_obsrunoff_found = numvals .gt. 0
      endif
 
      call pop_routine (my_name)
      return
      end
 
 
 
 
* ====================================================================
      subroutine soilwat2_set_my_variable (variable_name)
* ====================================================================
      implicit none
      include   'soilwat2.inc'
      include 'engine.pub'
      include 'intrface.pub'
      include 'data.pub'
      include 'error.pub'
 
*+  Sub-Program Arguments
      character variable_name*(*)         ! (input) variable name to search for
 
*+  Purpose
*      set a variable in this module as requested by another.
 
*+  Mission Statement
*     Set Variable as Requested
 
*+  Changes
*      031292 jngh
*      170393 jngh changed for new engine interface
*      020893 jngh added pond
*      300994 jpd?? included p_cn2 - can't remember why?
*           could be to accomodate p_cn2 re-set by manager following tillage
*      ??0994 pdev added re-sets for erosion
*      191094 jngh changed interface routines and added resets of contents
*      180895 nih  removed pond
*      261095 DPH Added call to message_unused
*      070696 nih changed respond2set calls to collect calls
*      200896 jngh changed cn2 to cn2_bare
 
*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_set_my_variable')
 
*+  Local Variables
      real       fract                 ! temporary fraction
      integer    layer                 ! layer counter
      integer    num_layers            ! number of layers
      integer    numvals               ! number of values returned in array
      real       temp(max_layer)       ! temporary array
 
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (variable_name .eq. 'sw') then
         call collect_real_array (variable_name, max_layer, '()'
     :                               , temp, numvals
     :                               , 0.0, 1.0)
 
         num_layers = count_of_real_vals (p_dlayer, max_layer)
         do 1000 layer = 1,num_layers
            g_sw_dep(layer) = temp(layer)*p_dlayer(layer)
            call soilwat2_check_profile (layer)
1000     continue
 
      elseif (variable_name .eq. 'sw_dep') then
         call collect_real_array (variable_name, max_layer, '(mm)'
     :                               , g_sw_dep, numvals
     :                               , 0.0, 10000.0)
 
         num_layers = count_of_real_vals (p_dlayer, max_layer)
         do 2000 layer = 1,num_layers
            call soilwat2_check_profile (layer)
2000     continue
 
      elseif (variable_name .eq. 'dlt_sw') then
 
         call collect_real_array (variable_name, max_layer, '()'
     :                               , temp, numvals
     :                               , -1.0, 1.0)
 
         num_layers = count_of_real_vals (p_dlayer, max_layer)
         do 3000 layer = 1,num_layers
            g_sw_dep(layer) = g_sw_dep(layer)
     :                      + temp(layer)*p_dlayer(layer)
            call soilwat2_check_profile (layer)
3000     continue
 
      elseif (variable_name .eq. 'dlt_sw_dep') then
         call collect_real_array (variable_name, max_layer, '(mm)'
     :                               , temp, numvals
     :                               , -10000.0, 10000.0)
 
         num_layers = count_of_real_vals (p_dlayer, max_layer)
         do 4000 layer = 1,num_layers
            g_sw_dep(layer) = g_sw_dep(layer) + temp(layer)
            call soilwat2_check_profile (layer)
4000     continue
 
* code for erosion
 
      elseif (variable_name .eq. 'dul_dep') then
         call collect_real_array (variable_name, max_layer, '(mm)'
     :                               , g_dul_dep, numvals
     :                               , 0.0, 10000.0)
 
         num_layers = count_of_real_vals (p_dlayer, max_layer)
         do 4100 layer = 1,num_layers
            call soilwat2_check_profile (layer)
4100     continue
 
      elseif (variable_name .eq. 'dul') then
         call collect_real_array (variable_name, max_layer, '()'
     :                               , temp, numvals
     :                               , 0.0, 1.0)
 
         num_layers = count_of_real_vals (p_dlayer, max_layer)
         do 4110 layer = 1,num_layers
            g_dul_dep(layer) = temp(layer)*p_dlayer(layer)
            call soilwat2_check_profile (layer)
4110     continue
 
      elseif (variable_name .eq. 'll15_dep') then
         call collect_real_array (variable_name, max_layer, '(mm)'
     :                               , g_ll15_dep, numvals
     :                               , 0.0, 10000.0)
 
         num_layers = count_of_real_vals (p_dlayer, max_layer)
         do 4200 layer = 1,num_layers
            call soilwat2_check_profile (layer)
4200     continue
 
      elseif (variable_name .eq. 'll15') then
         call collect_real_array (variable_name, max_layer, '()'
     :                               , temp, numvals
     :                               , 0.0, 1.0)
 
         num_layers = count_of_real_vals (p_dlayer, max_layer)
         do 4210 layer = 1,num_layers
            g_ll15_dep(layer) = temp(layer)*p_dlayer(layer)
            call soilwat2_check_profile (layer)
4210     continue
 
      elseif (variable_name .eq. 'sat_dep') then
         call collect_real_array (variable_name, max_layer, '(mm)'
     :                               , g_sat_dep, numvals
     :                               , 0.0, 10000.0)
 
         num_layers = count_of_real_vals (p_dlayer, max_layer)
         do 4300 layer = 1,num_layers
            call soilwat2_check_profile (layer)
4300     continue
 
      elseif (variable_name .eq. 'sat') then
         call collect_real_array (variable_name, max_layer, '()'
     :                               , temp, numvals
     :                               , 0.0, 1.0)
 
         num_layers = count_of_real_vals (p_dlayer, max_layer)
         do 4310 layer = 1,num_layers
            g_sat_dep(layer) = temp(layer)*p_dlayer(layer)
            call soilwat2_check_profile (layer)
4310     continue
 
      elseif (variable_name .eq. 'air_dry_dep') then
         call collect_real_array (variable_name, max_layer, '(mm)'
     :                               , g_air_dry_dep, numvals
     :                               , 0.0, 10000.0)
 
         num_layers = count_of_real_vals (p_dlayer, max_layer)
         do 4500 layer = 1,num_layers
            call soilwat2_check_profile (layer)
4500     continue
 
      elseif (variable_name .eq. 'air_dry') then
         call collect_real_array (variable_name, max_layer, '()'
     :                               , temp, numvals
     :                               , 0.0, 1.0)
 
         num_layers = count_of_real_vals (p_dlayer, max_layer)
         do 4510 layer = 1,num_layers
            g_air_dry_dep(layer) = temp(layer)*p_dlayer(layer)
            call soilwat2_check_profile (layer)
4510     continue
 
      elseif (variable_name .eq. 'dlayer') then
         call collect_real_array (variable_name, max_layer, '(mm)'
     :                               , temp, numvals
     :                               , 0.0, 10000.0)
 
         do 5000 layer = 1, numvals
            fract = divide (temp(layer), p_dlayer(layer), 0.0)
 
            g_air_dry_dep(layer) = g_air_dry_dep(layer) * fract
            g_dul_dep(layer) = g_dul_dep(layer) * fract
            g_ll15_dep(layer) = g_ll15_dep(layer) * fract
            g_sat_dep(layer) = g_sat_dep(layer) * fract
            g_sw_dep(layer) = g_sw_dep(layer) * fract
            p_dlayer(layer) = temp(layer)
 
            call soilwat2_check_profile (layer)
5000     continue
 
         do 5100 layer = numvals, max_layer
 
            g_air_dry_dep(layer) = 0.0
            g_dul_dep(layer) = 0.0
            g_ll15_dep(layer) = 0.0
            g_sat_dep(layer) = 0.0
            g_sw_dep(layer) = 0.0
            p_dlayer(layer) = 0.0
 
5100     continue
 
      elseif (variable_name .eq. 'dlt_dlayer') then
         call collect_real_array (variable_name, max_layer, '(mm)'
     :                               , temp, numvals
     :                               ,-10000.0, 10000.0)
 
         do 6000 layer = 1, numvals
            temp(layer) = p_dlayer(layer) + temp(layer)
            fract = divide (temp(layer), p_dlayer(layer), 0.0)
 
            g_air_dry_dep(layer) = g_air_dry_dep(layer) * fract
            g_dul_dep(layer) = g_dul_dep(layer) * fract
            g_ll15_dep(layer) = g_ll15_dep(layer) * fract
            g_sat_dep(layer) = g_sat_dep(layer) * fract
            g_sw_dep(layer) = g_sw_dep(layer) * fract
            p_dlayer(layer) = temp(layer)
 
            call soilwat2_check_profile (layer)
6000     continue
 
* end code for erosion
 
      elseif (variable_name .eq. 'cn2_bare') then
         call collect_real_var (variable_name, '()'
     :                             , p_cn2_bare, numvals
     :                             , 0.0, 100.0)
 
      elseif (variable_name .eq. 'cn_cov') then
         call collect_real_var (variable_name, '()'
     :                             , p_cn_cov, numvals
     :                             , 0.0, 1.0)
 
      elseif (variable_name .eq. 'cn_red') then
         call collect_real_var (variable_name, '()'
     :                             , p_cn_red, numvals
     :                             , 0.0, p_cn2_bare - 0.00009)
 
      else
         call Message_unused ()
 
      endif
 
      call pop_routine (my_name)
      return
      end
 
 
 
* ====================================================================
      subroutine soilwat2_set_other_variables ()
* ====================================================================
      implicit none
      include   'const.inc'            ! global_active, mes_set_variable
      include   'soilwat2.inc'
      include 'string.pub'
      include 'data.pub'
      include 'engine.pub'
      include 'intrface.pub'
      include 'error.pub'
 
*+  Purpose
*      set the value of a variable or array in other module/s.
 
*+  Notes
*      a flag is set if any of the totals is requested.  the totals are
*      reset during the next process phase when this happens.
 
*+  Mission Statement
*     Set Variables in other Modules
 
*+  Changes
*      100393 jngh specified and programmed
*      170895 nih  converted to update any solutes it knows about
*      070696 nih  changed set calls to post_var constructs
 
*+  Calls
      character  string_concat*32      ! function
 
*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_set_other_variables')
 
*+  Local Variables
      character  dlt_name*32           ! name of solute delta variable
      integer    layer                 ! layer number counter
      integer    num_layers            ! number of layers
      integer    solnum                ! solute number counter
      real       temp_dlt_solute(max_layer) ! temp array for
                                       ! changes in solute (kg/ha)
 
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      num_layers = count_of_real_vals (p_dlayer, max_layer)
      do 100 solnum = 1, g_num_solutes
 
         call fill_real_array (temp_dlt_solute ,0.0, max_layer)
 
         do 50 layer=1,max_layer
            temp_dlt_solute(layer) = g_dlt_solute (solnum, layer)
   50    continue
 
         dlt_name = string_concat ('dlt_',g_solute_names(solnum))
 
         call new_postbox()
         call post_real_array (dlt_name
     :                       , '(kg/ha)'
     :                       , temp_dlt_solute
     :                       , num_layers)
         call message_send_immediate (g_solute_owners(solnum)
     :                               ,MES_set_variable
     :                               ,dlt_name)
         call delete_postbox()
  100 continue
 
      call pop_routine (my_name)
      return
      end
 
 
 
* ====================================================================
      subroutine soilwat2_send_my_variable (variable_name)
* ====================================================================
      implicit none
      include   'soilwat2.inc'
      include 'string.pub'
      include 'science.pub'
      include 'data.pub'
      include 'error.pub'
      include 'engine.pub'
      include 'intrface.pub'
 
*+  Sub-Program Arguments
      character variable_name*(*)      ! (input) variable name to search for
 
*+  Purpose
*      return the value of a variable in return_string.  used to return
*      values of variables requested by other modules.
 
*+  Notes
*      a flag is set if any of the totals is requested.  the totals are
*      reset during the next process phase when this happens.
 
*+  Mission Statement
*     Send Value of Requested Variable
 
*+  Changes
*      031292 jngh
*      170393 jngh changed for units and new engine
*      020893 jngh added more variables to report and changed total names.
*      030294 jpd added eos
*      060994 jpd added crop_cover, crop_cover_max, total_cover,cn2new
*      120195 jngh removed crop_cover_max as crop module should maintain
*                  a similar figure in total cover.
*      190595 jngh added bulk density
*      300695 jngh added eo to output
*      180895 nih  upgraded the solute output stuff to match muti-solute
*                  approached that is now used.
*      261095 DPH  Added call to message_unused
*      130896 jngh removed crop_cover (g_cover_green_sum)
*      260897 nih  Added output for flow_water and flow_(solute_name)
*      970910 slw  fix problem with es reporting as zero
*      990323 nih  Added output for effective rainfall (eff_rain)
 
*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_send_my_variable')
 
*+  Local Variables
      real       crop_cover            ! sum of crop covers (0-1)
      real       esw                   ! potential extractable sw in profile
      integer    layer                 ! layer counter
      integer    num_layers            ! number of layers
      integer    solnum                ! solute no. counter
      character  solute_name*32        ! solute name
      real       temp_array(max_layer) ! temporary array
      real       total_cover           ! total ground cover (0-1)
      real       es                    ! total es
      real       eff_rain              ! daily effective rainfall (mm)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (variable_name .eq. 'es') then
         es = sum_real_array(g_es_layers, max_layer)
         call respond2get_real_var (variable_name, '(mm)', es)
 
      else if (variable_name .eq. 'eo') then
         call respond2get_real_var (variable_name, '(mm)', g_eo)
 
      else if (variable_name .eq. 'eos') then
         call respond2get_real_var (variable_name, '(mm)', g_eos)
 
      else if (variable_name .eq. 'total_cover') then
         crop_cover = sum_cover_array (g_cover_tot, g_num_crops)
         total_cover = add_cover (crop_cover, g_residue_cover)
         call respond2get_real_var (variable_name, '()'
     :                             , total_cover)
 
      else if (variable_name .eq. 'cn2_new') then
         call respond2get_real_var (variable_name, '()', g_cn2_new)
 
      else if (variable_name .eq. 'runoff') then
         call respond2get_real_var (variable_name, '(mm)', g_runoff)
 
      else if (variable_name .eq. 'drain') then
         call respond2get_real_var (variable_name, '(mm)', g_drain)
 
      else if (variable_name .eq. 'infiltration') then
         call respond2get_real_var (variable_name, '(mm)'
     :                             , g_infiltration)

      else if (variable_name .eq. 'eff_rain') then
         es = sum_real_array(g_es_layers, max_layer)
         eff_rain = g_rain - g_runoff - g_drain
         call respond2get_real_var (variable_name, '(mm)'
     :                             , eff_rain)
 
      else if (variable_name .eq. 'salb') then
         call respond2get_real_var (variable_name, '(mm)', p_salb)
 
      elseif (variable_name .eq. 'bd') then
         num_layers = count_of_real_vals (p_dlayer, max_layer)
         call respond2get_real_array (variable_name, '(g/cc)'
     :                               , g_bd, num_layers)
 
      else if (variable_name .eq. 'esw') then
 
         num_layers = count_of_real_vals (p_dlayer, max_layer)
         esw = 0.0
         do 1000 layer = 1, num_layers
            esw = esw + l_bound (g_sw_dep(layer) - g_ll15_dep(layer)
     :                        , 0.0)
1000     continue
         call respond2get_real_var (variable_name, '(mm)', esw)
 
      else if (variable_name .eq. 'sw_dep') then
 
         num_layers = count_of_real_vals (p_dlayer, max_layer)
         call respond2get_real_array (variable_name, '(mm)'
     :                               , g_sw_dep, num_layers)
 
      else if (variable_name .eq. 'sw') then
 
         num_layers = count_of_real_vals (p_dlayer, max_layer)
         do 2000 layer = 1, num_layers
            temp_array(layer) = divide (g_sw_dep(layer)
     :                                , p_dlayer(layer), 0.0)
2000     continue
         call respond2get_real_array (variable_name, '(mm/mm)'
     :                               , temp_array, num_layers)
 
      else if (variable_name .eq. 'dlayer') then
 
         num_layers =  count_of_real_vals (p_dlayer, max_layer)
         call respond2get_real_array (variable_name, '(mm)'
     :                               , p_dlayer, num_layers)
 
      else if (variable_name .eq. 'll15_dep') then
 
         num_layers =  count_of_real_vals (p_dlayer, max_layer)
         call respond2get_real_array (variable_name, '(mm)'
     :                               , g_ll15_dep, num_layers)
 
      else if (variable_name .eq. 'll15') then
 
         num_layers =  count_of_real_vals (p_dlayer, max_layer)
         do 3000 layer = 1, num_layers
            temp_array(layer) = divide (g_ll15_dep(layer)
     :                                , p_dlayer(layer), 0.0)
3000     continue
         call respond2get_real_array (variable_name, '(mm/mm)'
     :                               , temp_array, num_layers)
 
      else if (variable_name .eq. 'dul_dep') then
 
         num_layers =  count_of_real_vals (p_dlayer, max_layer)
         call respond2get_real_array (variable_name, '(mm)'
     :                               , g_dul_dep, num_layers)
 
      else if (variable_name .eq. 'dul') then
 
         num_layers =  count_of_real_vals (p_dlayer, max_layer)
         do 4000 layer = 1, num_layers
            temp_array(layer) = divide (g_dul_dep(layer)
     :                                , p_dlayer(layer), 0.0)
4000     continue
         call respond2get_real_array (variable_name, '(mm/mm)'
     :                               , temp_array, num_layers)
 
      else if (variable_name .eq. 'sat_dep') then
 
         num_layers =  count_of_real_vals (p_dlayer, max_layer)
         call respond2get_real_array (variable_name, '(mm)'
     :                               , g_sat_dep, num_layers)
 
      else if (variable_name .eq. 'sat') then
 
         num_layers =  count_of_real_vals (p_dlayer, max_layer)
         do 5000 layer = 1, num_layers
            temp_array(layer) = divide (g_sat_dep(layer)
     :                                , p_dlayer(layer), 0.0)
5000     continue
         call respond2get_real_array (variable_name, '(mm/mm)'
     :                               , temp_array, num_layers)
 
      else if (variable_name .eq. 'air_dry_dep') then
 
         num_layers =  count_of_real_vals (p_dlayer, max_layer)
         call respond2get_real_array (variable_name, '(mm)'
     :                               , g_air_dry_dep, num_layers)
 
      else if (variable_name .eq. 'air_dry') then
 
         num_layers =  count_of_real_vals (p_dlayer, max_layer)
         do 6000 layer = 1, num_layers
            temp_array(layer) = divide (g_air_dry_dep(layer)
     :                                , p_dlayer(layer), 0.0)
6000     continue
         call respond2get_real_array (variable_name, '(mm/mm)'
     :                               , temp_array, num_layers)
 
      else if (variable_name .eq. 'flux') then
 
         num_layers =  count_of_real_vals (p_dlayer, max_layer)
         call respond2get_real_array (variable_name, '(mm)'
     :                               , g_flux, num_layers)
 
      else if (variable_name .eq. 'flow') then
 
         num_layers =  count_of_real_vals (p_dlayer, max_layer)
         call respond2get_real_array (variable_name, '(mm)'
     :                               , g_flow, num_layers)
 
      ! --- Resultant water and solute flow output variables ---
      else if (variable_name .eq. 'flow_water') then
         num_layers =  count_of_real_vals (p_dlayer, max_layer)
         do 6100 layer = 1, num_layers
            temp_array(layer) = g_flux (layer)
     :                        - g_flow (layer)
 6100    continue
         call respond2get_real_array (variable_name, '(mm)'
     :                               , temp_array, num_layers)
 
      else if (index(variable_name, 'flow_').eq.1) then
         solute_name = variable_name(len('flow_')+1:)
         solnum = position_in_char_array (solute_name
     :                                   ,g_solute_names
     :                                   ,max_solute)
         if (solnum.ne.0) then
            num_layers = count_of_real_vals (p_dlayer, max_layer)
            do 6200 layer = 1, num_layers
               temp_array(layer) = g_solute_leach(solnum,layer)
     :                           - g_solute_up(solnum,layer)
 6200       continue
 
            call respond2get_real_array (variable_name, '(kg/ha)'
     :                               , temp_array, num_layers)
         else
            call Message_unused ()
         endif
 
 
      ! Oh boy isn't this messy
      ! If variable name has _leach or _up as the last thing in its
      ! name then see if we can supply some output.
 
      else if  ((index(variable_name,'_leach').ne.0) .and.
     :         (index(variable_name,'_leach').eq.
     :         lastnb(variable_name)-len('_leach')+1)) then
 
         solute_name = variable_name(:index(variable_name,'_leach')-1)
         solnum = position_in_char_array (solute_name
     :                                   ,g_solute_names
     :                                   ,max_solute)
 
         if (solnum.ne.0) then
            num_layers = count_of_real_vals (p_dlayer, max_layer)
            do 7000 layer = 1, num_layers
               temp_array(layer) = g_solute_leach(solnum,layer)
 7000       continue
 
            call respond2get_real_array (variable_name, '(kg/ha)'
     :                               , temp_array, num_layers)
         else
            call Message_unused ()
         endif
 
      else if  ((index(variable_name,'_up').ne.0) .and.
     :         (index(variable_name,'_up').eq.
     :         lastnb(variable_name)-len('_up')+1)) then
 
         solute_name = variable_name(:index(variable_name,'_up')-1)
         solnum = position_in_char_array (solute_name
     :                                   ,g_solute_names
     :                                   ,max_solute)
 
         if (solnum.ne.0) then
            num_layers = count_of_real_vals (p_dlayer, max_layer)
            do 8000 layer = 1, num_layers
               temp_array(layer) = g_solute_up(solnum,layer)
 8000       continue
 
            call respond2get_real_array (variable_name, '(kg/ha)'
     :                               , temp_array, num_layers)
         else
            call Message_unused ()
         endif
      else
         ! not my variable
 
         call Message_unused ()
      endif
 
      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      subroutine soilwat2_zero_variables ()
*     ===========================================================
      implicit none
      include   'const.inc'            ! blank
      include   'soilwat2.inc'
      include 'data.pub'
      include 'error.pub'
 
*+  Purpose
*       zero variables & arrays
 
*+  Mission Statement
*     Zero Variables
 
*+  Changes
*       191094 jngh specified and programmed
*       190595 jngh added bulk density
 
*+  Constant Values
      character  my_name*(*)           ! module name
      parameter (my_name  = 'soilwat2_zero_variables')
 
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
          !  zero pools etc.
 
      call soilwat2_zero_daily_variables ()
 
      call fill_real_array (g_air_dry_dep , 0.0, max_layer)
      call fill_real_array (g_dul_dep     , 0.0, max_layer)
      call fill_real_array (g_ll15_dep    , 0.0, max_layer)
      call fill_real_array (g_sat_dep     , 0.0, max_layer)
      call fill_real_array (g_sw_dep      , 0.0, max_layer)
      call fill_real_array (g_bd          , 0.0, max_layer)
      call fill_real_array (c_canopy_fact , 0.0, max_coeffs)
      call fill_real_array (c_canopy_fact_height , 0.0, max_coeffs)
 
      call fill_char_array (g_solute_names, ' ', max_solute)
      call fill_char_array (g_solute_owners, ' ', max_solute)
      call fill_logical_array (g_solute_mobility, .false., max_solute)
      g_num_solutes = 0

      c_canopy_fact_default = 0.0
      g_num_canopy_fact    = 0
      g_sumes1             = 0.0
      g_sumes2             = 0.0
      g_t                  = 0.0
      g_obsrunoff_name     = blank
 
      g_inf_pool           = 0.0
      g_sumes              = 0.0
      g_sumes_last         = 0.0
      g_sumes_yest         = 0.0
      g_sumeos_last        = 0.0
      g_eo_source          = blank
      g_tillage_rain_sum   = 0.0
      g_tillage_cn_rain    = 0.0
      g_tillage_cn_red     = 0.0
 
      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      subroutine soilwat2_zero_daily_variables ()
*     ===========================================================
      implicit none
      include   'soilwat2.inc'
      include 'data.pub'
      include 'error.pub'
 
*+  Purpose
*       zero variables & arrays
 
*+  Mission Statement
*     Zero Daily Variables
 
*+  Changes
*       191094 jngh specified and programmed
*       170895 nih  added initialisation of solute information
*       130896 jngh removed g_total_cover
*                   removed g_cover_green_sum
*                   removed g_cover_tot_sum
*                   added g_cover_tot and g_cover_green and g_crop_module
*                   added g_num_crops
 
*+  Constant Values
      character  my_name*(*)           ! module name
      parameter (my_name  = 'soilwat2_zero_daily_variables')
 
*+  Local Variables
      integer layer                    ! soil layer number counter
      integer solnum                   ! solute number counter
 
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
          !  zero pools etc.
 
      call fill_real_array (g_flow, 0.0, max_layer)
      call fill_real_array (g_flux, 0.0, max_layer)
      call fill_real_array (g_es_layers, 0.0, max_layer)
      call fill_real_array (g_cover_tot, 0.0, max_crops)
      call fill_real_array (g_cover_green, 0.0, max_crops)
      call fill_char_array (g_crop_module, ' ', max_crops)
      call fill_real_array (g_canopy_height, 0.0, max_crops)
 
      g_rain               = 0.0
      g_radn               = 0.0
      g_mint               = 0.0
      g_maxt               = 0.0
      g_year               = 0
      g_day                = 0
      g_residue_wt         = 0.0
      g_residue_cover      = 0.0
      g_eo                 = 0.0
      g_eos                = 0.0
      g_cn2_new            = 0.0
      g_drain              = 0.0
      g_infiltration       = 0.0
      g_runoff             = 0.0
      g_num_crops          = 0
      g_obsrunoff          = 0.0
      g_obsrunoff_found    = .false.
 
      ! initialise all solute information
 
      do 200 solnum = 1, max_solute
         do 100 layer = 1, max_layer
            g_solute (solnum, layer) = 0.0
            g_solute_min (solnum, layer) = 0.0
            g_solute_leach(solnum, layer) = 0.0
            g_solute_up (solnum,layer) = 0.0
            g_dlt_solute (solnum,layer) = 0.0
  100    continue
  200 continue
 
 
      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      subroutine soilwat2_solute_flux (solute_out
     :                                , solute_kg
     :                                , solute_min)
*     ===========================================================
      implicit none
      include   'soilwat2.inc'
      include 'data.pub'
      include 'error.pub'
 
*+  Sub-Program Arguments
      real       solute_out(*)         ! (output) solute leaching out of
                                       !    each layer (kg/ha)
      real       solute_min(*)         ! (input) minimum solute allowed
                                       !     (kg/ha)
      real       solute_kg(*)          ! (input) solute in each layer
                                       !    (kg/ha)
 
*+  Purpose
*         calculate the downward movement of solute with percolating water
 
*+  Mission Statement
*     Calculate the Solute Movement with Saturated Water Flux
 
*+  Changes
*        210191   specified and programmed jngh (j hargreaves)
*        031091   jngh declared count_of_real_vals, max_layer.con, swater.blk &
*                      sprofl.blk - cr162
*                      added comma in argument declaration of nut_min - cr163
*                      declared num_layers - cr164
*        251091   fixed upper limit of out_n becoming -ve.  jngh - cr221
*        290892   jngh changed soil water to depth of water
*        151292   jngh changed common blocks
*        170895   nih  renamed to generic solute name
*        200896   jngh renamed n, nut and nutrient references to solute
 
*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_solute_flux')
 
*+  Local Variables
      real       in_solute             ! solute leaching into layer from
                                       !    above (kg/ha)
      integer    layer                 ! layer counter
      integer    num_layers            ! number of layers in profile
      real       out_max               ! max. solute allowed to leach out of
                                       !    layer (kg/ha)
      real       out_solute            ! solute leaching out of layer
                                       !    (kg/ha)
      real       out_w                 ! water draining out of layer (mm)
      real       solute_kg_layer       ! quantity of solute in layer (kg/ha)
      real       water                 ! quantity of water in layer (mm)
 
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
            ! flux section - drainage out, down to next layer
 
      call fill_real_array (solute_out, 0.0, max_layer)
      num_layers = count_of_real_vals (p_dlayer, max_layer)
      in_solute = 0.0
 
      do 1000 layer = 1,num_layers
 
             ! get water draining out of layer and n content of layer
             ! includes that leaching down
 
         out_w = g_flux(layer)
         solute_kg_layer = solute_kg(layer) + in_solute
 
             ! n leaching out of layer is proportional to the water draining
             ! out.
* ?????????????? 21 mar 91 - jngh. should the water draining into this
* ?????????????? layer be removed also?
 
         water = g_sw_dep(layer) + out_w
         out_solute = solute_kg_layer
     :         * divide (out_w, water, 0.0)
     :         * c_Solute_flux_eff
 
             ! don't allow the n to be reduced below a minimum level
 
         out_max = l_bound (solute_kg_layer - solute_min(layer), 0.0)
         out_solute = bound (out_solute, 0.0, out_max)
 
             ! keep the leaching and set the input for the next layer
 
         solute_out(layer) = out_solute
         in_solute = out_solute
 
 1000 continue
 
      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      subroutine soilwat2_solute_flow (solute_up, solute_kg, solute_min)
*     ===========================================================
      implicit none
      include   'soilwat2.inc'
      include 'data.pub'
      include 'intrface.pub'
      include 'error.pub'
 
*+  Sub-Program Arguments
      real       solute_up (*)         ! (output) solute moving upwards
                                       !    into each layer (kg/ha)
      real       solute_kg (*)         ! (input/output) solute in each
                                       !    layer (kg/ha)
      real       solute_min(*)         ! (input) minimum solute allowed
                                       !     (kg/ha)
 
*+  Purpose
*       movement of solute in response to differences in
*       water content of adjacent soil layers when the soil water
*       content is < the drained upper limit (unsaturated flow)
 
*+  Notes
*       170895 nih The variable names and comments need to be cleaned
*                  up.  When this is done some references to no3 or
*                  nitrogen need to be changed to 'solute'
 
*+  Mission Statement
*     Calculate the Solute Movement with Unsaturated Water Flow
 
*+  Changes
*       051191 jngh previously programmed and now changed
*       251191 jngh corrected n flow into top layer - cr218
*       251091 jngh changed count_of_real_vals from real to integer - cr215
*                   added comment re 0.5 - cr216
*                   corrected downward flow - cr219
*       290892 jngh changed soil water to depth of water
*       151292 jngh changed common blocks
*       250893 jngh corrected adjustment of soil water for previous movement
*       170895 nih  renamed to generic solute name
*       200896 jngh renamed n, nut and nutrient references to solute
 
*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_solute_flow')
 
*+  Local Variables
      real       bottomw               ! water movement to/from next layer
                                       ! (kg/ha)
      real       in_solute                  ! solute moving into layer from
                                       !    above (kg/ha)
      integer    layer                 ! layer counter
      real       solute_down (max_layer) ! solute moving downwards out of
                                       !    each layer (kg/ha)
      integer    num_layers            ! number of layers
      real       out_solute            ! solute moving out of layer (kg/ha)
      real       out_w                 ! water draining out of layer (mm)
      real       remain (max_layer)    ! n remaining in each layer between
                                       !    movement up (kg/ha)
      real       solute_kg_layer       ! quantity of solute in layer (kg/ha)
      real       top_w                 ! water movement to/from above layer
                                       ! (kg/ha)
      real       water                 ! quantity of water in layer (mm)
 
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      call fill_real_array (solute_up, 0.0, max_layer)
 
            ! flow  up from lower layer:  + up, - down
 
            ! + ve flow : upward movement. go from bottom to top layer
 
      num_layers = count_of_real_vals (p_dlayer, max_layer)
      in_solute = 0.0
      do 1000 layer = num_layers,2,-1
 
             ! keep the nflow upwards
 
         solute_up(layer) = in_solute
 
             ! get water moving up and out of layer to the one above
 
         out_w = g_flow(layer-1)
         if (out_w .le. 0.0) then
            out_solute = 0.0
         else
                ! get water movement between this and next layer
 
            bottomw = g_flow(layer)
 
                ! get n content of layer includes that moving from other layer
 
            solute_kg_layer = solute_kg(layer) + in_solute
            water = g_sw_dep(layer) + out_w - bottomw
 
                ! n moving out of layer is proportional to the water moving
                ! out.
 
                ! jngh 19-3-91 i think the *0.5 should be removed
                ! jngh 25-10-91 john dimes thinks the 0.5 is to allow
                ! for losses through diffusion. pjr called it a diffusion
                ! coefficient.  it seems that the water movement is incorrect
                ! and this compensates for it.
 
cjh            out_solute = solute_kg_layer*divide (out_w, water, 0.0) *0.5
            out_solute = solute_kg_layer
     :                 * divide (out_w, water, 0.0)
     :                 * c_Solute_flow_eff
 
                ! don't allow the n to be reduced below a minimum level
 
            out_solute = bound (out_solute
     :                         , 0.0
     :                         , solute_kg_layer - solute_min(layer))
 
         endif
 
             ! set the input for the next layer
 
         in_solute = out_solute
1000  continue
 
      solute_up (1) = in_solute
 
          ! now get n remaining in each layer between movements
 
          ! this is needed to adjust the n in each layer before calculating
          ! downwards movement.  i think we shouldn't do this within a time
          ! step. i.e. there should be no movement within a time step. jngh
 
      remain(1) = solute_up(1)
      do 1010 layer = 2, num_layers
         remain(layer) = solute_up(layer) - solute_up(layer - 1)
1010  continue
 
           ! -ve flow - downward movement
 
      call fill_real_array (solute_down, 0.0, max_layer)
      in_solute = 0.0
      top_w = 0.0
 
      do 1100 layer = 1,num_layers
 
             ! get water moving out of layer
 
         out_w = - g_flow(layer)
         if (out_w.le.0.0) then
            out_solute = 0.0
         else
 
                ! get n content of layer includes that moving from other layer
 
            solute_kg_layer = solute_kg(layer)
     :                      + in_solute
     :                      + remain(layer)
            water = g_sw_dep(layer) + out_w - top_w
 
                ! n moving out of layer is proportional to the water moving
                ! out.
                ! jngh 19-3-91 i think the *0.5 should be removed.
                ! 25-10-91 see note in up movement about this.
 
            out_solute = solute_kg_layer
     :            * divide (out_w, water, 0.0)
     :            * c_Solute_flow_eff
 
                ! don't allow the n to be reduced below a minimum level
 
            out_solute = bound (out_solute
     :                         , 0.0
     :                         , solute_kg_layer - solute_min(layer))
 
         endif
         solute_down(layer) = out_solute
         in_solute = out_solute
         top_w = out_w
1100  continue
 
      do 1200 layer = 1, num_layers
         solute_up(layer) =  solute_up(layer) - solute_down(layer)
1200  continue
 
      call pop_routine (my_name)
      return
      end
 
 
 
* ====================================================================
       subroutine soilwat2_add_water ()
* ====================================================================
      implicit none
       include 'soilwat2.inc'
      include 'error.pub'
      include 'intrface.pub'
 
*+  Purpose
*     <insert here>
 
*+  Mission Statement
*     Add Water
 
*+  Changes
*   neilh - 18-08-1995 - Programmed and Specified
*   neilh - 07-06-1996 - removed data_String from argument list
*                      - changed extract calls to collect calls
 
*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'soilwat2_add_water')
 
*+  Local Variables
       real             amount           ! amount of irrigation (mm)
       integer          numvals          ! no. of values read from string
       real             solconc          ! solute conc in water(kg/ha)
       integer          solnum           ! solute no. counter variable
 
*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      call collect_real_var ('amount'
     :                      ,'(mm)'
     :                      ,amount
     :                      ,numvals
     :                      ,0.0
     :                      ,1000.)
 
      g_irrigation = g_irrigation + amount

      do 100 solnum = 1, g_num_solutes
 
         call collect_real_var_optional (
     :                         g_solute_names(solnum)
     :                        ,'(kg/ha)'
     :                        ,solconc
     :                        ,numvals
     :                        ,0.0
     :                        ,1000.)

        if (numvals.gt.0) then
           g_irrigation_solute(solnum) = g_irrigation_solute(solnum)
     :                                 + solconc
        else
        endif
  100 continue

      call pop_routine (myname)
      return
      end
 
 
 
* ====================================================================
      subroutine soilwat2_sum_report ()
* ====================================================================
      implicit none
      include   'const.inc'
      include   'soilwat2.inc'
      include 'string.pub'
      include 'data.pub'
      include 'write.pub'
      include 'error.pub'
 
*+  Purpose
*     <insert here>
 
*+  Mission Statement
*      Report SoilWat module summary details      
 
*+  Changes
*   NeilH - 19-10-1994 - Programmed and Specified
*       190595 jngh added bulk density
*       300695 jngh changed format for insoil from i8 to f8.2
*       190897 nih  renamed from soilwat2_init_report
*       260897 nih  Added extra information to summary report
 
*+  Constant Values
      character  my_name*(*)           ! name of current procedure
      parameter (my_name = 'soilwat2_sum_report')
 
*+  Local Variables
      real       depth_layer_top       ! depth to top of layer (mm)
      real       depth_layer_bottom    ! depth to bottom of layer (mm)
      integer    layer                 ! layer number
      integer    num_layers            ! number of soil profile layers
      character  line*100              ! temp output record
      real       runoff_wf(max_layer)  ! weighting factor for runoff
      real       usw(max_layer)        ! unavail. sw (mm)
      real       asw(max_layer)        ! avail. sw (mm)
      real       masw(max_layer)       ! max unavail. sw (mm)
      real       dsw(max_layer)        ! drainable sw (mm)
 
*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      call write_string (lu_scr_sum, new_line//new_line)
 
      line = '                 Soil Profile Properties'
      call write_string (lu_scr_sum, line)
 
      line =
     :'   -----------------------------------------------------------'
     ://'----------'
      call write_string (lu_scr_sum, line)
 
      line =
     :'         Depth  Air_Dry  LL15   Dul    Sat     Sw     BD   '
     ://'Runoff  SWCON'
      call write_string (lu_scr_sum, line)
 
      line =
     :'           mm     mm/mm  mm/mm  mm/mm  mm/mm  mm/mm  g/cc    wf'
      call write_string (lu_scr_sum, line)
 
      line =
     :'   -----------------------------------------------------------'
     ://'----------'
      call write_string (lu_scr_sum, line)
 
      num_layers = count_of_real_vals (p_dlayer, max_layer)
      depth_layer_top = 0.0
      call soilwat2_runoff_depth_factor (runoff_wf)
 
      do 1000 layer = 1,num_layers
         depth_layer_bottom = depth_layer_top + p_dlayer(layer)
 
         write (line,'(3x, f6.0, a, f6.0, 8f7.3)')
     :            depth_layer_top, '-', depth_layer_bottom
     :          , divide (g_air_dry_dep(layer)
     :                  , p_dlayer(layer), 0.0)
     :          , divide (g_ll15_dep(layer)
     :                  , p_dlayer(layer), 0.0)
     :          , divide (g_dul_dep(layer), p_dlayer(layer), 0.0)
     :          , divide (g_sat_dep(layer), p_dlayer(layer), 0.0)
     :          , divide (g_sw_dep(layer), p_dlayer(layer), 0.0)
     :          , g_bd(layer)
     :          , runoff_wf(layer)
     :          , p_swcon(layer)
 
         call write_string (lu_scr_sum, line)
         depth_layer_top = depth_layer_bottom
1000  continue
 
      line =
     :'   -----------------------------------------------------------'
     ://'----------'
      call write_string (lu_scr_sum, line)
 
      call write_string (lu_scr_sum, new_line//new_line)
 
      line = '             Soil Water Holding Capacity'
      call write_string (lu_scr_sum, line)
 
      line =
     :'     ---------------------------------------------------------'
 
      call write_string (lu_scr_sum, line)
 
      line =
     :'         Depth    Unavailable Available  Max Avail.  Drainable'
      call write_string (lu_scr_sum, line)
      line =
     :'                      (LL)     (SW-LL)    (DUL-LL)   (SAT-DUL)'
      call write_string (lu_scr_sum, line)
 
      line =
     :'                       mm        mm          mm         mm'
      call write_string (lu_scr_sum, line)
 
      line =
     :'     ---------------------------------------------------------'
      call write_string (lu_scr_sum, line)
 
      num_layers = count_of_real_vals (p_dlayer, max_layer)
      depth_layer_top = 0.0
 
      do 2000 layer = 1,num_layers
         depth_layer_bottom = depth_layer_top + p_dlayer(layer)
         usw(layer) = g_ll15_dep(layer)
         asw(layer) = l_bound(g_sw_dep(layer)-g_ll15_dep(layer),0.0)
         masw(layer) = g_dul_dep(layer) - g_ll15_dep(layer)
         dsw(layer) = g_sat_dep(layer) - g_dul_dep(layer)
 
         write (line,'(3x, f6.0, a, f6.0, 4f11.2)')
     :            depth_layer_top, '-', depth_layer_bottom
     :           ,usw(layer)
     :           ,asw(layer)
     :           ,masw(layer)
     :           ,dsw(layer)
 
         call write_string (lu_scr_sum, line)
         depth_layer_top = depth_layer_bottom
2000  continue
 
      line =
     :'     ---------------------------------------------------------'
      call write_string (lu_scr_sum, line)
 
      write (line,'(10x,''Totals'', 4f11.2)')
     :               sum_real_array (usw,  num_layers)
     :             , sum_real_array (asw,  num_layers)
     :             , sum_real_array (masw, num_layers)
     :             , sum_real_array (dsw,  num_layers)
 
 
      call write_string (lu_scr_sum, line)
 
      line =
     :'     ---------------------------------------------------------'
      call write_string (lu_scr_sum, line)
 
             ! echo sw parameters
 
      call write_string (lu_scr_sum, new_line//new_line)
      call write_string (lu_scr_sum, new_line//new_line)
 
      line = '             Initial Soil Parameters'
      call write_string (lu_scr_sum, line)
 
      line =
     :  '     ---------------------------------------------------------'
      call write_string (lu_scr_sum, line)
 
      line =
     : '            Insoil        Salb     Dif_Con   Dif_Slope'
      call write_string (lu_scr_sum, line)
 
      line =
     :  '     ---------------------------------------------------------'
      call write_string (lu_scr_sum, line)
 
      write (line, '(6x, 4f12.2)')
     :               p_insoil
     :             , p_salb
     :             , p_diffus_const
     :             , p_diffus_slope
      call write_string (lu_scr_sum, line)
 
      line =
     :  '     ---------------------------------------------------------'
      call write_string (lu_scr_sum, line)
      call write_string (lu_scr_sum, new_line//new_line)
 
      if (g_obsrunoff_name .ne. blank) then
         write (line, '(6x,a,a,a)')
     :          '             Observed runoff data ( ',
     :          g_obsrunoff_name(1:lastNB(g_obsrunoff_name)),
     :          ' ) is used in water balance'
 
         call write_string (lu_scr_sum, line)
 
      else
            ! no observed data
         call write_string (lu_scr_sum
     :  ,'             Runoff is predicted using scs curve number:')
         line =
     : '           Cn2  Cn_Red  Cn_Cov   H_Eff_Depth '
         call write_string (lu_scr_sum, line)
 
         line =
     : '                                      mm     '
         call write_string (lu_scr_sum, line)
 
         line =
     :  '     ---------------------------------------------------------'
         call write_string (lu_scr_sum, line)
 
         write (line, '(6x, 4f8.2)')
     :       p_cn2_bare, p_cn_red, p_cn_cov,
     :       c_hydrol_effective_depth
         call write_string (lu_scr_sum, line)
 
         line =
     :  '     ---------------------------------------------------------'
         call write_string (lu_scr_sum, line)
      endif
 
      call write_string (lu_scr_sum, new_line//new_line)
 
      if (c_evap_method .eq. c_ritchie_method) then
         line = '      Using Ritchie evaporation model'
         call write_string (lu_scr_sum, line)
 
         write (line, '(7x, a, f8.2, a)') 'Cuml evap (U):        ',
     :        p_u, ' (mm^0.5)'
         call write_string (lu_scr_sum, line)
 
         write (line, '(7x, a, f8.2, a)') 'CONA:                 ',
     :        p_cona, ' ()'
         call write_string (lu_scr_sum, line)
 
      else if (c_evap_method .eq. c_bs_a_method) then
         line = '      Using B&S option A evaporation method'
         call write_string (lu_scr_sum, line)
 
         write (line, '(7x, a, f8.2, a)') 'Beta:                 ',
     :        p_beta, ' (mm^0.5)'
         call write_string (lu_scr_sum, line)
 
      else if (c_evap_method .eq. c_bs_b_method) then
         line = '      Using B&S option B evaporation method'
         call write_string (lu_scr_sum, line)
 
         write (line, '(7x, a, f8.2, a)') 'Beta:                 ',
     :        p_beta, ' (mm^0.5)'
         call write_string (lu_scr_sum, line)
 
      else if (c_evap_method .eq. c_bs_acs_method) then
         line = '      Using B&S option B method with acs/jd mods'
         call write_string (lu_scr_sum, line)
 
         write (line, '(7x, a, f8.2, a)') 'Cuml evap (U):        ',
     :        p_u, ' (mm)'
         call write_string (lu_scr_sum, line)
 
         write (line, '(7x, a, f8.2, a)') 'CONA:                 ',
     :        p_cona, ' ()'
         call write_string (lu_scr_sum, line)
 
         write (line, '(7x, a, f8.2, a)') 'Beta:                 ',
     :        p_beta, ' (mm^0.5)'
         call write_string (lu_scr_sum, line)
 
      else if (c_evap_method .eq. c_rickert_method) then
         line = '      Using Rickert evaporation method'
         call write_string (lu_scr_sum, line)
 
         write (line, '(7x, a, f8.2, a)') 'Max daily evaporation:',
     :        p_max_evap, ' (mm)'
         call write_string (lu_scr_sum, line)
 
      else
         line = '     Using unknown evaporation method!'
         call write_string (lu_scr_sum, line)
 
      endif
 
      if (g_eo_source .ne. blank) then
         write (line, '(6x, a, a)') 'Eo source:             ',
     :        g_eo_source
         call write_string (lu_scr_sum, line)
      else
         write (line, '(6x, a)') 'Eo from priestly-taylor'
         call write_string (lu_scr_sum, line)
      endif
 
      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      subroutine soilwat2_init ()
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'soilwat2.inc'
      include   'error.pub'
      include   'write.pub'
 
*+  Purpose
*       input initial values from soil water parameter files.
 
*+  Mission Statement
*       Initialise SoilWat module      
 
*+  Changes
*        210191   specified and programmed jngh (j hargreaves
*        290591   jngh corrected external call list - cr91
*                      removed sprpty.blk & winit.blk - cr92
*        290892   jngh changed soil water to depth of water
*        051093   jngh added fatal error call.
*                      changed l to layer.
*        190194   jpd  add air_dry_tot for output
*        25/7/96  dph  added code to report to summary file when p_insoil < 1
*        190897   nih  renamed from soilwat2_init and
*                      adapted as part of MES_reset development
*        090299   jngh changed name from reset to init
*                       removed calls to zero variables and get other variables
 
*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name  = 'soilwat2_init')
 
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
            ! zero pools
      call report_event (' Initialising ')
 
          ! Get all coefficients from file
 
      call soilwat2_read_constants ()
 
      call soilwat2_soil_property_param ()
      call soilwat2_soil_profile_param ()
 
      call soilwat2_evap_init ()
 
      call pop_routine (my_name)
      return
      end
 
 
 
* ====================================================================
       subroutine Soilwat2_runoff_depth_factor (runoff_wf)
* ====================================================================
      implicit none
      include 'soilwat2.inc'
      include 'science.pub'
      include 'data.pub'
      include 'error.pub'
 
*+  Sub-Program Arguments
      real    runoff_wf(*)              ! (OUTPUT) weighting factor for runoff
 
*+  Purpose
*      Calculate the weighting factor hydraulic effectiveness used
*      to weight the effect of soil moisture on runoff.
 
*+  Mission Statement
*      Calculate soil moisture effect on runoff
 
*+  Changes
*     26-08-1997 - Neil Huth - Programmed and Specified
 
*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'Soilwat2_runoff_depth_factor')
 
*+  Local Variables
      real       profile_depth         ! current depth of soil profile
                                       ! - for when erosion turned on
      real       cum_depth             ! cumulative depth (mm)
      real       hydrol_effective_depth ! hydrologically effective depth for
                                        ! runoff (mm)
      integer    hydrol_effective_layer ! layer number that the effective
                                        ! depth occurs in ()
      integer    layer                 ! layer counter
      integer    num_layers            ! number of layers
      real       scale_fact            ! scaling factor for wf function to
                                       ! sum to 1
      real       wf_tot                ! total of wf ()
      real       wx                    ! depth weighting factor for current
                                       !    total depth.
                                       !    intermediate variable for
                                       !    deriving wf
                                       !    (total wfs to current layer)
      real       xx                    ! intermediate variable for deriving wf
                                       ! total wfs to previous layer
 
*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      call fill_real_array (runoff_wf, 0.0, max_layer)
      xx     = 0.0
      cum_depth = 0.0
      wf_tot = 0.0
      num_layers = count_of_real_vals (p_dlayer, max_layer)
 
           ! check if hydro_effective_depth applies for eroded profile.
 
      profile_depth = sum_real_array (p_dlayer, num_layers)
      hydrol_effective_depth = min (c_hydrol_effective_depth
     :                            , profile_depth)
 
      scale_fact = 1.0/(1.0 - exp(-4.16))
      hydrol_effective_layer = find_layer_no (hydrol_effective_depth
     :                                       , p_dlayer
     :                                       , num_layers)
 
      do 100 layer = 1, hydrol_effective_layer
         cum_depth = cum_depth + p_dlayer(layer)
         cum_depth = u_bound (cum_depth, hydrol_effective_depth)
 
            ! assume water content to c_hydrol_effective_depth affects runoff
            ! sum of wf should = 1 - may need to be bounded? <dms 7-7-95>
 
         wx = scale_fact * (1.0 - exp( - 4.16* divide (cum_depth
     :                                         , hydrol_effective_depth
     :                                         , 0.0)))
         runoff_wf(layer) = wx - xx
         xx = wx
 
         wf_tot = wf_tot + runoff_wf(layer)
 
  100 continue
 
      call bound_check_real_var (wf_tot, 0.9999, 1.0001, 'wf_tot')
 
      call pop_routine (myname)
      return
      end
 
 
 
* ====================================================================
       subroutine soilwat2_irrig_solute ()
* ====================================================================
      implicit none
      include 'const.inc'
      include 'soilwat2.inc'
      include 'data.pub'
      include 'error.pub'
 
*+  Purpose
*     <insert here>
 
*+  Mission Statement
*      Add solutes with irrigation
 
*+  Changes
*   neilh - 04-09-1995 - Programmed and Specified
 
*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'soilwat2_irrig_solute')
 
*+  Local Variables
      integer    solnum                ! solute number counter variable
 
*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      do 1000 solnum = 1, g_num_solutes
         g_solute(solnum,1)     = g_solute(solnum,1)
     :                          + g_irrigation_solute(solnum)
         g_dlt_solute(solnum,1) = g_dlt_solute(solnum,1)
     :                          + g_irrigation_solute(solnum)
 
 1000 continue
 
      call pop_routine (myname)
      return
      end
 
 
 
* ====================================================================
       subroutine soilwat2_move_solute_down ()
* ====================================================================
      implicit none
      include 'const.inc'
      include 'soilwat2.inc'
      include 'data.pub'
      include 'error.pub'
 
*+  Purpose
*     <insert here>
 
*+  Mission Statement
*      Calculate downward movement of solutes
 
*+  Changes
*   neilh - 04-09-1995 - Programmed and Specified
 
*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'soilwat2_move_solute_down')
 
*+  Local Variables
      integer    num_layers
      integer    layer                 ! layer number counter variable
      integer    solnum                ! solute number counter variable
      real       leach (max_layer)     ! amount of a solute leached from
                                       ! each soil layer (kg/ha)
      real       temp_solute(max_layer)! temp array for solute content(kg/ha)
      real       temp_solute_min (max_layer)! temp array for minimum solute
                                       ! content (kg/ha)
      real       temp_dlt_solute(max_layer) ! temp array of changes in
                                       ! solute concentration (kg/ha)
 
*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      ! Now for each mobile solute put the global solute info into a
      ! temp solute array, pass this solute information to the solute
      ! flux routine then insert moved solute back into the global
      ! record.
 
      num_layers = count_of_real_vals (p_dlayer, max_layer)
 
      do 1300 solnum = 1, g_num_solutes
         if (g_solute_mobility(solnum)) then
 
            do 1100 layer = 1, num_layers
               temp_solute(layer) = g_solute(solnum, layer)
               leach(layer) = 0.0
               temp_solute_min(layer) = g_solute_min(solnum,layer)
               temp_dlt_solute(layer) = g_dlt_solute(solnum,layer)
 1100       continue
 
            call soilwat2_solute_flux (leach
     :                                 , temp_solute
     :                                 , temp_solute_min)
            call move_down_real (leach, temp_solute, num_layers)
            call move_down_real (leach, temp_dlt_solute, num_layers)
 
            do 1200 layer = 1, num_layers
               g_solute (solnum, layer) = temp_solute (layer)
               g_solute_leach (solnum, layer) = leach (layer)
               g_dlt_solute (solnum, layer) = temp_dlt_solute (layer)
 1200       continue
 
         else
            ! solute was not in the mobile list - do not move it
         endif
 
 1300 continue
 
      call pop_routine (myname)
      return
      end
 
 
 
* ====================================================================
       subroutine soilwat2_move_solute_up ()
* ====================================================================
      implicit none
      include 'const.inc'
      include 'soilwat2.inc'
      include 'data.pub'
      include 'error.pub'
 
*+  Purpose
*     <insert here>
 
*+  Mission Statement
*      Calculate upward movement of solutes
 
*+  Changes
*   neilh - 04-09-1995 - Programmed and Specified
 
*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'soilwat2_move_solute_up')
 
*+  Local Variables
      integer    layer                 ! layer number counter variable
      real       leach (max_layer)     ! amount of a solute leached from
                                       ! each soil layer (kg/ha)
      integer    num_layers            ! number of layers
      integer    solnum                ! solute number counter variable
      real       temp_solute(max_layer)! temp array for solute content(kg/ha)
      real       temp_solute_min (max_layer)! temp array for minimum solute
                                       ! content (kg/ha)
      real       temp_dlt_solute(max_layer) ! temp array of changes in
                                       ! solute concentration (kg/ha)
 
*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      ! Now for each mobile solute put the global solute info into a
      ! temp solute array, pass this solute information to the solute
      ! flux routine then insert moved solute back into the global
      ! record.
 
      num_layers = count_of_real_vals (p_dlayer, max_layer)
 
      do 2300 solnum = 1, g_num_solutes
 
         if (g_solute_mobility(solnum)) then
 
            do 2100 layer = 1, max_layer
               temp_solute(layer) = g_solute(solnum, layer)
               leach(layer) = 0.0
               temp_solute_min(layer) = g_solute_min(solnum,layer)
               temp_dlt_solute(layer) = g_dlt_solute(solnum,layer)
 2100       continue
 
            call soilwat2_solute_flow (leach
     :                                , temp_solute
     :                                , temp_solute_min)
            call move_up_real (leach, temp_solute, num_layers)
            call move_up_real (leach, temp_dlt_solute, num_layers)
 
            do 2200 layer = 1, max_layer
               g_solute (solnum, layer) = temp_solute (layer)
               g_solute_up (solnum, layer) = leach (layer)
               g_dlt_solute (solnum, layer) = temp_dlt_solute (layer)
 2200       continue
         else
            ! solute was not in the mobile list - do not move it
         endif
 
 2300 continue
 
      call pop_routine (myname)
      return
      end
 
 
 
*     ===========================================================
      subroutine soilwat2_infiltration ( infiltration )
*     ===========================================================
      implicit none
      include   'soilwat2.inc'
      include 'read.pub'
      include 'write.pub'
      include 'intrface.pub'
      include 'error.pub'
 
*+  Sub-Program Arguments
      real       infiltration          ! (OUTPUT) infiltration into top layer (mm)
 
*+  Purpose
*     infiltration into top layer after runoff.
 
*+  Mission Statement
*      Calculate infiltration into top layer
 
*+  Changes
*       221090 specified (jngh)
 
*+  Constant Values
      character  my_name*(*)           ! this subroutine name
      parameter (my_name = 'soilwat2_infiltration')
 
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
            ! infiltration (mm) = (g_rain+irrigation) - g_runoff
            ! Note: no irrigation runs off.
 
      infiltration =  g_irrigation + g_rain - g_runoff
 
      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      subroutine soilwat2_tillage ()
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'soilwat2.inc'
      include 'data.pub'
      include 'write.pub'
      include 'error.pub'
      include 'read.pub'
      include 'intrface.pub'
 
*+  Purpose
*     Set up for CN reduction after tillage operation
 
*+  Notes
*       This code is borrowed from residue module.
 
*+  Mission Statement
*       Calculate tillage effects
 
*+  Changes
*       221090 specified (jngh)
*       071097 PdeV
 
*+  Constant Values
      character  my_name*(*)           ! this subroutine name
      parameter (my_name = 'soilwat2_tillage')
*
      character*(*) tillage_section    ! section name for tillage info in
      parameter (tillage_section = 'tillage') ! lookup file
 
*+  Local Variables
      character string*300             ! message string
      character type*30                ! name of implement used for tillage
      real      type_info(2)           ! Array containing information about
                                       ! a certain type (from table)
      integer   numvals                ! Number of values found in data string
      integer   numvals_cnred, numvals_cnrain
 
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
                              ! 1. Find which implement was used.
      call collect_char_var ('type'
     :                      ,'()'
     :                      ,type
     :                      ,numvals)
 
      call collect_real_var_optional ('cn_red'
     :                      ,'()'
     :                      ,g_tillage_cn_red
     :                      ,numvals_cnred, 0.0, 100.0)
 
      call collect_real_var_optional ('cn_rain'
     :                      ,'()'
     :                      ,g_tillage_cn_rain
     :                      ,numvals_cnrain, 0.0, 1000.0)
 
      if (numvals_cnred .le. 0 .or. numvals_cnrain .le. 0) then
 
        call write_string (lu_scr_sum
     :               ,new_line//'    - Reading tillage CN info')
 
        call read_real_array_optional (
     :           tillage_section      ! Section header
     :         , type                 ! Keyword
     :         , 2                    ! size of array
     :         , '()'                 ! Units
     :         , type_info            ! Variable
     :         , numvals              ! Number of values returned
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 1000.0)               ! Upper Limit for bound checking
 
        if (numvals.ne.2) then
               ! We have an unspecified tillage type
            g_tillage_cn_red = 0.0
            g_tillage_cn_rain = 0.0
 
            string = 'Cannot find info for tillage:- '//type
            call FATAL_ERROR (ERR_user, string)
 
        else
          if (numvals_cnred .le. 0) then
            g_tillage_cn_red = type_info(1)
          else
          endif
 
          if (numvals_cnrain .le. 0) then
            g_tillage_cn_rain = type_info(2)
          else
          endif
        endif
      endif
 
      ! Ensure cn equation won't go silly
      g_tillage_cn_red = bound (g_tillage_cn_red, 0.0, p_cn2_bare)
 
      write (string, '(3a,40x,a,f8.2,a,40x,a, f8.2)' )
     :      'Soil tilled using ', type, New_Line
     :     ,'CN reduction = ', g_tillage_cn_red, New_Line
     :     ,'Acc rain     = ', g_tillage_cn_rain
 
      call report_event (string)
 
                                     ! 3. Reset the accumulator
      g_tillage_rain_sum = 0.0
 
      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      subroutine soilwat2_tillage_addrain ( rain )
*     ===========================================================
      implicit none
      include   'soilwat2.inc'
      include 'write.pub'
      include 'error.pub'
 
*+  Sub-Program Arguments
      real      rain                   ! (INPUT) today's rainfall (mm)
 
*+  Purpose
*     accumulate rainfall for tillage cn reduction
 
*+  Mission Statement
*      Accumulate rainfall for tillage cn reduction
 
*+  Changes
*       221090 specified (jngh)
 
*+  Constant Values
      character  my_name*(*)           ! this subroutine name
      parameter (my_name = 'soilwat2_tillage_addrain')
 
*+  Local Variables
      character  string*100            ! message string
 
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      g_tillage_rain_sum = g_tillage_rain_sum + rain
 
      if (g_tillage_cn_rain .gt. 0.0 .and.
     :    g_tillage_rain_sum .gt. g_tillage_cn_rain) then
 
           ! This tillage has lost all effect on cn. CN reduction
           !  due to tillage is off until the next tillage operation.
         g_tillage_cn_rain = 0.0
         g_tillage_cn_red = 0.0
 
         write (string, '(a)') 'Tillage CN reduction finished'
         call report_event (string)
 
      else
      endif
 
      call pop_routine (my_name)
      return
      end
 

*     ===========================================================
      subroutine soilwat2_on_new_solute ()
*     ===========================================================
      implicit none
      include 'const.inc' 
      include 'event.inc'
      include 'soilwat2.inc'
      include 'error.pub'
      include 'data.pub'
      include 'intrface.pub'
 
*+  Purpose
*     Add new solute to internal list of system solutes
 
*+  Mission Statement
*      Add new solute information to list of system solutes
 
*+  Changes
*       170599 nih - specified
 
*+  Constant Values
      character  my_name*(*)           ! this subroutine name
      parameter (my_name = 'soilwat2_on_new_solute')
 
*+  Local Variables
      integer numvals
      character names(max_solute)*32
      character sender*(max_module_name_size)
      integer counter
      integer mobile_no
      integer immobile_no

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)

      call collect_char_var (DATA_sender
     :                      ,'()'
     :                      ,sender
     :                      ,numvals)

      call collect_char_array (DATA_new_solute_names
     :                        ,max_solute
     :                        ,'()'
     :                        ,names
     :                        ,numvals)

      if (g_num_solutes+numvals.gt.max_solute) then
         call fatal_error (ERR_Internal
     :                    ,'Too many solutes for Soilwat2')
      else
         do 100 counter = 1, numvals
            g_num_solutes = g_num_solutes + 1
            g_solute_names(g_num_solutes) = names(counter)
            g_solute_owners(g_num_solutes) = sender

            mobile_no = position_in_char_array(
     :                        g_solute_names(g_num_solutes)
     :                       ,c_mobile_solutes
     :                       ,max_solute)

            immobile_no = position_in_char_array(
     :                        g_solute_names(g_num_solutes)
     :                       ,c_immobile_solutes
     :                       ,max_solute)


            if (mobile_no.ne.0) then
               g_solute_mobility(g_num_solutes) = .true.

            elseif (immobile_no.ne.0) then
               g_solute_mobility(g_num_solutes) = .false.

            else
               call fatal_error(ERR_Internal,
     :                 'No solute mobility information for '//
     :                 g_solute_names(g_num_solutes))
            endif

  100    continue
      endif

      call pop_routine (my_name)
      return
      end
 
 
*     ===========================================================
      subroutine soilwat2_ONtick ()
*     ===========================================================
      implicit none
      include   'soilwat2.inc'
      include 'error.pub'
      include 'event.pub'
 
*+  Purpose
*     Update internal time record and reset daily state variables.
 
*+  Mission Statement
*     Update internal time record and reset daily state variables.
 
*+  Changes
*        260899 nih 

*+  Local Variables
      character temp1*5
      integer   temp2
 
*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'soilwat2_ONtick')
 
*- Implementation Section ----------------------------------
      call push_routine (myname)

      ! Note that time and timestep information is not required
      ! and so dummy variables are used in their place.

      call handler_ONtick(g_day, g_year, temp1, temp2)

      call pop_routine (myname)
      return
      end
 
