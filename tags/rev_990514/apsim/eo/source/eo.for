*====================================================================
      subroutine apsim_Eo (Action, Data_string)
*====================================================================
      implicit none
      dll_export apsim_eo
      include   'const.inc'            ! Global constant definitions
      include   'Eo.inc'               ! Eo common block
      include 'engine.pub'                        
      include 'error.pub'                         

*+  Sub-Program Arguments
      character  Action*(*)            ! Message action to perform
      character  Data_string*(*)       ! Message data

*+  Purpose
*      This routine is the interface between the main system and the
*      Eo module.

*+  Changes
*       210995 jngh programmed
*       090696 jngh changed presence report to standard

*+  Constant Values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'apsim_Eo')

*+  Local Variables
      character  module_name*8         ! name of this module

*- Implementation Section ----------------------------------
      call push_routine (myname)
      
      !print*, ' action/data is: ', trim(action), ' : ',trim(data_string)
      
 
      if (Action.eq.MES_Presence) then
         call get_current_module (Module_name)
         write(*, *) 'Module_name = ', Module_name
 
      elseif (Action.eq.MES_Init) then
         !open (200,'debug.out')
         call Eo_zero_variables ()
         call Eo_init ()
 
      elseif (Action.eq.MES_Prepare) then
         call Eo_zero_daily_variables ()
         call Eo_get_other_variables ()
         call Eo_prepare ()
 
      elseif (Action.eq.MES_Get_variable) then
         call Eo_send_my_variable (Data_string)
 
      elseif (Action.eq.MES_Process) then
         call Eo_process ()
 
      else if (Action .eq. MES_Set_variable) then
         call Eo_set_my_variable (Data_String)
 
      else
            ! don't use message
         call Message_unused ()
 
      endif
 
      call pop_routine (myname)
      return
      end



*====================================================================
      subroutine Eo_zero_variables ()
*====================================================================
      implicit none
      include   'const.inc'            ! Global constant definitions
      include   'Eo.inc'               ! Eo common block
      include 'error.pub'                         

*+  Purpose
*     Set all variables in this module to zero.

*+  Changes
*       210995 jngh programmed

*+  Constant Values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'Eo_zero_variables')

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
         !variables for penman-monteith
 
!      p_e_method   = blank
      g_vpd_source = blank
      c_reference_height_base=blank
      p_vpd_fac = 0.0
      g_day_of_year = 0
      g_year      = 0
      g_wind_ms_instrum   = 0.0
      g_wind_ms_multiplier_height   = 0.0
      g_wind_ms_reference = 0.0
      g_wind_adj = 0.0
      g_wind = 0.0
      c_reference_height  = 0.0
      p_disp_instrum      = 0.0
      p_z0_instrum        = 0.0
      g_maxt      = 0.0
      g_mint      = 0.0
      g_n_hrs     = 0.0
      g_Eo_pm     = 0.0
      g_pa        = 0.0
      g_ra        = 0.0
      g_radn      = 0.0
      g_radn_net  = 0.0
      g_rc        = 0.0
      g_rh        = 0.0
      p_albedo    = 0.0
      p_z0soil    = 0.0
      g_epsilon   = 0.0
      g_vpd_mb    = 0.0
      g_da        = 0.0
      g_fr_intc_radn   = 0.0
      g_fg             = 0.0
      g_canopy_height  = 0.0
      g_instrum_height = 0.0
      g_lai            = 0.0
      g_lai_tot        = 0.0
      g_latitude       = 0.0
      c_zc_conversion  = 0.0
      c_rsmin_canopy   = 0.0
      c_rc             = 0.0
      c_pen_mon_ub     = 0.0
 
      call Eo_zero_daily_variables ()
 
      call pop_routine (myname)
 
      return
      end



*====================================================================
      subroutine Eo_zero_daily_variables ()
*====================================================================
      implicit none
      include    'Eo.inc'              ! Eo common block
      include 'error.pub'                         

*+  Purpose
*     Set all variables in this module to zero.

*+  Changes
*       210995 jngh programmed

*+  Constant Values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'Eo_zero_daily_variables')

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
 
      call pop_routine (myname)
 
      return
      end



*====================================================================
      subroutine Eo_init ()
*====================================================================
      implicit none
      include 'write.pub'                         
      include 'error.pub'                         

*+  Purpose
*      Initialise Eo module

*+  Changes
*       210995 jngh programmed

*+  Constant Values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'Eo_init')

*+  Local Variables
      character  Event_string*40       ! String to output

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
         ! notify system that we have initialised
 
      Event_string = 'Initialising:'
      
      call report_event (Event_string)
 
         ! get all constants from constants file
 
      call Eo_read_constants ()
 
         ! get all parameters from parameter file
 
      call Eo_read_param ()
 
         ! get other variables needed for initialisation
 
      call Eo_get_other_var_ini ()
 
      call pop_routine (myname)
 
      return
      end



*===========================================================
      subroutine Eo_read_param ()
*===========================================================
      implicit none
      include   'const.inc'            ! Global constant definitions
      include   'Eo.inc'               ! Eo common block
      include 'read.pub'                          
      include 'write.pub'                         
      include 'error.pub'                         

*+  Purpose
*       Read all module parameters.

*+  Changes
*       210995 jngh programmed
*       020398 jngh added in reading and reporting for reference height,
*                   disp and Z0 at instrument site.
*                   changed z0soil to mm

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Eo_read_param')
*
      character section_name*(*)
      parameter (section_name = 'parameters')

*+  Local Variables
      integer    numvals               ! number of values read
      character  line*80               ! output string

*- Implementation Section ----------------------------------
 
      call push_routine (myname)
 
      call write_string (lu_scr_sum
     :          ,new_line//'   - Reading Eo Parameters')
 
         ! vpd_fac
      call read_real_var (
     :           section_name
     :          ,'vpd_fac'
     :          ,'(-)'
     :          ,p_vpd_fac
     :          ,numvals
     :          ,0.0
     :          ,1.0)
 
         ! e_method
!        call read_char_var (
!     :           section_name
!     :          ,'e_method'
!     :          ,'()'
!     :          ,p_e_method
!     :          ,numvals)
 
         ! albedo
      call read_real_var (
     :           section_name
     :          ,'albedo'
     :          ,'(-)'
     :          ,p_albedo
     :          ,numvals
     :          ,0.0
     :          ,1.0)
 
      call read_real_var (section_name
     :                   , 'max_albedo', '()'
     :                   , p_max_albedo, numvals
     :                   , 0.0, 1.0)
 
         ! Z0soil
      call read_real_var (
     :           section_name
     :          ,'z0soil'
     :          ,'(mm)'
     :          ,p_z0soil
     :          ,numvals
     :          ,0.0
     :          ,1000.0)
 
         ! default wind
      call read_real_var (
     :           section_name
     :          ,'default_wind'
     :          ,'(km/day)'
     :          ,p_default_wind
     :          ,numvals
     :          ,0.0
     :          ,1000.0)
 
         ! default pa
      call read_real_var (
     :           section_name
     :          ,'default_pa'
     :          ,'(hpa)'
     :          ,p_default_pa
     :          ,numvals
     :          ,800.0
     :          ,1200.0)
 
         ! default instrum_height
      call read_real_var (
     :           section_name
     :          ,'default_instrum_height'
     :          ,'(mm)'
     :          ,p_default_instrum_height
     :          ,numvals
     :          ,0.0
     :          ,50000.0)
 
         ! disp_instrum
      call read_real_var (
     :           section_name
     :          ,'disp_instrum'
     :          ,'(mm)'
     :          ,p_disp_instrum
     :          ,numvals
     :          ,0.0
     :          ,50000.0)
 
         ! z0_instrum
      call read_real_var (
     :           section_name
     :          ,'z0_instrum'
     :          ,'(mm)'
     :          ,p_z0_instrum
     :          ,numvals
     :          ,0.0
     :          ,50000.0)

      call read_real_var (
     :           section_name
     :         , 'extinct_coef'
     :         , '()'
     :         , p_extinct_coef
     :         , numvals
     :         , 0.0
     :         , 1.0)
 
 
          ! eo_plant_method
        call read_char_var (
     :           section_name
     :          ,'eo_plant_method'
     :          ,'()'
     :          ,p_eo_plant_method
     :          ,numvals)
 
         ! p_wind_day_fraction
      call read_real_var_optional (
     :           section_name
     :          ,'wind_day_fraction'
     :          ,'(-)'
     :          ,p_wind_day_fraction
     :          ,numvals
     :          ,0.0
     :          ,1.0)
     
      if (numvals.eq.0) then
         p_wind_day_fraction = c_default_wind_day_fraction
      else
      endif

         ! p_adjustment_factor
      call read_real_var_optional (
     :           section_name
     :          ,'adjustment_factor'
     :          ,'(-)'
     :          ,p_adjustment_factor
     :          ,numvals
     :          ,0.0
     :          ,2.0)
     
         ! p_adjustment_factor
      call read_real_var_optional (
     :           section_name
     :          ,'wind_multiplier'
     :          ,'(-)'
     :          ,p_wind_multiplier
     :          ,numvals
     :          ,0.0
     :          ,2.0)
     
 

         ! now report out what we have read in
 
      call write_string (lu_scr_sum, new_line//new_line)
 
      line = '                 Eo Parameters'
      call write_string (lu_scr_sum, line)
 
      line =
     :  '     ---------------------------------------------------------'
      call write_string (lu_scr_sum, line)
 
      line =
     :'      Albedo   Z0soil   Dflt_Wind  Dflt_Pa Dflt_instrum_ht'
!     ://' E_method'
      call write_string (lu_scr_sum, line)
 
      line =
     :'       (-)       (mm)   (km/day)   (hpa)      (mm)'
      call write_string (lu_scr_sum, line)
 
      line =
     :  '     ---------------------------------------------------------'
      call write_string (lu_scr_sum, line)
 
      write (line, '(f10.2, f10.3, 3f10.2, 8x, a)')
     :              p_albedo
     :            , p_z0soil
     :            , p_default_wind
     :            , p_default_pa
     :            , p_default_instrum_height
!     :            , p_e_method
      call write_string (lu_scr_sum, line)
 
      line =
     :  '     ---------------------------------------------------------'
      call write_string (lu_scr_sum, line)
      call write_string (lu_scr_sum, new_line//new_line)
 
      line =
     :  '     -------------------'
      call write_string (lu_scr_sum, line)
 
      line =
     :'    instrum_d instrum_z0'
      call write_string (lu_scr_sum, line)
 
      line =
     :'     (mm)      (mm)     '
      call write_string (lu_scr_sum, line)
 
      line =
     :'     -------------------'
      call write_string (lu_scr_sum, line)
 
      write (line, '(4x, 2f10.2)')
     :              p_disp_instrum
     :            , p_z0_instrum
      call write_string (lu_scr_sum, line)
 
      line =
     :'     -------------------'
      call write_string (lu_scr_sum, line)
      call write_string (lu_scr_sum, new_line//new_line)
 
      call pop_routine (myname)
      return
      end



*===========================================================
      subroutine Eo_read_constants ()
*===========================================================
      implicit none
      include   'const.inc'            ! Global constant definitions
      include   'Eo.inc'               ! Eo common block
      include 'read.pub'                          
      include 'error.pub'                         

*+  Purpose
*       Read all module constants.

*+  Changes
*       210995 jngh programmed

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Eo_read_constants')
*
      character section_name*(*)
      parameter (section_name = 'constants')

*+  Local Variables
      integer    numvals               ! number of values read

*- Implementation Section ----------------------------------
 
      call push_routine (myname)
 
         ! reference_height
      call read_real_var (
     :           section_name
     :          ,'reference_height'
     :          ,'(mm)'
     :          ,c_reference_height
     :          ,numvals
     :          ,0.0
     :          ,50000.0)
 
         ! reference_height_base
      call read_char_var (
     :           section_name
     :          ,'reference_height_base'
     :          ,'()'
     :          ,c_reference_height_base
     :          ,numvals)
 
         ! conversion_height
      call read_real_var (
     :           section_name
     :          ,'zc_conversion'
     :          ,'(mm)'
     :          ,c_zc_conversion
     :          ,numvals
     :          ,0.0
     :          ,50000.0)
 
         ! multiplier_height
      call read_real_var (
     :           section_name
     :          ,'multiplier_height'
     :          ,'(mm)'
     :          ,c_multiplier_height
     :          ,numvals
     :          ,0.0
     :          ,50000.0)
 
         ! rsmin for canopy
      call read_real_var (
     :           section_name
     :          ,'rsmin_canopy'
     :          ,'(s/m)'
     :          ,c_rsmin_canopy
     :          ,numvals
     :          ,0.0
     :          ,200.0)
 
         ! radn_crit for rc increase in low light
      call read_real_var (
     :           section_name
     :          ,'radn_crit'
     :          ,'(w/m2)'
     :          ,c_radn_crit
     :          ,numvals
     :          ,0.0
     :          ,1000.0)
 
         ! vpd_crit for rc increase in high vpd
      call read_real_var (
     :           section_name
     :          ,'vpd_crit'
     :          ,'(kg/kg)'
     :          ,c_vpd_crit
     :          ,numvals
     :          ,0.0
     :          ,10.0)
 
         ! lai_crit for rc increase in high vpd
      call read_real_var (
     :           section_name
     :          ,'lai_crit'
     :          ,'(kg/kg)'
     :          ,c_lai_crit
     :          ,numvals
     :          ,0.0
     :          ,10.0)
 
         ! rc_method
        call read_char_var (
     :           section_name
     :          ,'rc_method'
     :          ,'()'
     :          ,c_rc_method
     :          ,numvals)
 
         ! rc for fixed
      call read_real_var_optional (
     :           section_name
     :          ,'rc'
     :          ,'(s/m)'
     :          ,c_rc
     :          ,numvals
     :          ,0.0
     :          ,200.0)
 
         ! c_pen_mon_ub
      call read_real_var (
     :           section_name
     :          ,'pen_mon_ub'
     :          ,'(mm)'
     :          ,c_pen_mon_ub
     :          ,numvals
     :          ,0.0
     :          ,100.0)
 
         ! c_default_wind_day_fraction
      call read_real_var (
     :           section_name
     :          ,'default_wind_day_fraction'
     :          ,'(-)'
     :          ,c_default_wind_day_fraction
     :          ,numvals
     :          ,0.0
     :          ,1.0)
 
         ! c_ra_ub
      call read_real_var (
     :           section_name
     :          ,'ra_ub'
     :          ,'(-)'
     :          ,c_ra_ub
     :          ,numvals
     :          ,0.0
     :          ,3000.0)
 
         ! c_ra_ub
      call read_real_var (
     :           section_name
     :          ,'ra_ub'
     :          ,'(-)'
     :          ,c_ra_ub
     :          ,numvals
     :          ,0.0
     :          ,300.0)
 
         ! c_alt_photo_radn
      call read_real_var (
     :           section_name
     :          ,'alt_photo_radn'
     :          ,'(-)'
     :          ,c_alt_photo_radn
     :          ,numvals
     :          ,-90.0
     :          ,90.0)
 
         ! c_wind_hrs
      call read_real_var (
     :           section_name
     :          ,'wind_hrs'
     :          ,'(hrs)'
     :          ,c_wind_hrs
     :          ,numvals
     :          ,-1.0
     :          ,24.0)
 
         ! c_wind_min
      call read_real_var (
     :           section_name
     :          ,'wind_min'
     :          ,'(m/s)'
     :          ,c_wind_min
     :          ,numvals
     :          ,0.0
     :          ,10.0)
 
         ! c_soil_heat_flux
      call read_real_var (
     :           section_name
     :          ,'soil_heat_flux'
     :          ,'(-)'
     :          ,c_soil_heat_flux
     :          ,numvals
     :          ,0.0
     :          ,1.0)
 
         ! c_penman_fU2_coef_a
      call read_real_var (
     :           section_name
     :          ,'penman_fU2_coef_a'
     :          ,'(-)'
     :          ,c_penman_fU2_coef_a
     :          ,numvals
     :          ,0.0
     :          ,1.0)
 
         ! c_penman_fU2_coef_b
      call read_real_var (
     :           section_name
     :          ,'penman_fU2_coef_b'
     :          ,'(-)'
     :          ,c_penman_fU2_coef_b
     :          ,numvals
     :          ,0.0
     :          ,1.0)
 
 
      call pop_routine  (myname)
      return
      end



*====================================================================
      subroutine Eo_get_other_var_ini ()
*====================================================================
      implicit none
      include   'const.inc'            ! Global constant definitions
      include   'Eo.inc'               ! Eo common block
      include 'write.pub'                         
      include 'intrface.pub'                      
      include 'error.pub'                         

*+  Purpose
*      Get the values of variables from other modules

*+  Changes
*       210995 jngh programmed

*+  Constant Values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'Eo_get_other_variables_ini')

*+  Local Variables
      integer    numvals               ! number of values returned
      character  string*80             ! temporary string
      real       pa                    ! atmospheric pressure (mb)
      real       wind                  ! wind speed (km/day)

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
         !latitude latitude
      call get_real_var (
     :           unknown_module
     :          ,'latitude'
     :          ,'(deg)'
     :          ,g_latitude
     :          ,numvals
     :          ,-90.0
     :          ,90.0)
 
         !instrum_height
      call get_real_var_optional (
     :           unknown_module
     :          ,'instrum_height'
     :          ,'(mm)'
     :          ,g_instrum_height
     :          ,numvals
     :          ,0.0
     :          ,50000.0)
 
      if (numvals.eq.0) then
         g_instrum_height = p_default_instrum_height
 
         write (string, '(a, f10.0, a)')
     :                        '     Default instrument height used = '
     :                        , p_default_instrum_height, ' (mm)'
 
         call write_string (lu_scr_sum, string)
 
      else
         ! instrum_height returned ok
      endif
 
         !vpd
 
      g_vpd_source = blank
      call get_real_var_optional (
     :      unknown_module
     :     ,'vpd'
     :     ,'(mb)'
     :     ,g_vpd_mb
     :     ,numvals
     :     ,0.0
     :     ,200.0)
 
      if (numvals.gt.0) then
         ! vpd returned ok
         g_vpd_source = source_vpd
      else
         call get_real_var_optional (
     :         unknown_module
     :         ,'rh'
     :         ,'(%)'
     :         ,g_rh
     :         ,numvals
     :         ,0.0
     :         ,100.0)
 
         if (numvals.gt.0) then
            ! rhd returned ok
            g_vpd_source = source_rh
         else
            call get_real_var (
     :            unknown_module
     :            ,'mint'
     :            ,'(oC)'
     :            ,g_mint
     :            ,numvals
     :            ,-30.0
     :            ,50.0)
 
            if (numvals.gt.0) then
               ! vpd returned ok
               g_vpd_source = source_mint
            else
               g_vpd_source = source_none
               call warning_error (err_user, ' No data to derive VPD' )
            endif    ! mint
         endif    ! rh
 
      endif    ! vpd
 
      string = '     Source of VPD is '//g_vpd_source
      call write_string (lu_scr_sum, string)
 
         !pa
      call get_real_var_optional (
     :      unknown_module
     :     ,'pa'
     :     ,'(hPa)'
     :     ,pa
     :     ,numvals
     :     ,800.0
     :     ,1200.0)
 
      if (numvals.eq.0) then
         write (string, '(a, f8.1, a)')
     :         '     Default atmospheric pressure used = '
     :         , p_default_pa, ' (mb)'
         call write_string (lu_scr_sum, string)
 
      else
         ! pa returned ok
      endif
 
         !wind
      call get_real_var_optional (
     :      unknown_module
     :     ,'wind'
     :     ,'(km/day)'
     :     ,wind
     :     ,numvals
     :     ,0.0
     :     ,1000.0)
 
      if (numvals.eq.0) then
         write (string, '(a, f8.1, a)')
     :         '     Default wind used = '
     :         , p_default_wind, ' (km/day)'
         call write_string (lu_scr_sum, string)
      else
         ! wind returned ok
      endif
 
         ! check for existance of more than one crop
      call get_real_vars (
     :      1
     :     ,'lai'
     :     ,'(-)'
     :     ,g_lai
     :     ,numvals
     :     ,0.0
     :     ,20.0)
 
      if (numvals.eq.0) then
         g_lai = 0.0
      else
            ! lai returned ok
            ! check that there is only one crop in the system
         call get_real_vars (
     :         2
     :        ,'lai'
     :        ,'(-)'
     :        ,g_lai
     :        ,numvals
     :        ,0.0
     :        ,20.0)
 
         if (numvals.eq.0) then
            g_lai = 0.0
         else
               ! lai returned ok
            call fatal_error (err_user
     :                       , 'Can''t handle more than one crop')
         endif
      endif
 
      call pop_routine (myname)
      return
      end



*================================================================
      subroutine Eo_prepare ()
*================================================================
      implicit none
      include 'error.pub'                         

*+  Purpose
*     perform calculations before the current timestep.

*+  Changes
*       210995 jngh programmed

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_prepare')

*- Implementation Section ----------------------------------
 
      call push_routine (myname)

      call Eo_pen_mon ()
 
      call pop_routine (myname)
      return
      end



*====================================================================
      subroutine Eo_get_other_variables ()
*====================================================================
      implicit none
      include   'const.inc'            ! Constant definitions
      include   'convert.inc'          ! conversion constants
      include   'Eo.inc'               ! Eo common block
      include 'intrface.pub'                      
      include 'error.pub'                         
      include 'science.pub'                       
      include 'data.pub'                          

*+  Purpose
*      Get the values of variables from other modules

*+  Changes
*       210995 jngh programmed

*+  Constant Values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'Eo_get_other_variables')
*
!      character  fr_intc_radn_name*(*) ! name of fr_intc_radn variable
!      parameter (fr_intc_radn_name = 'fr_intc_radn_')
*
!      integer    fr_intc_radn_name_length ! length of name
!      parameter (fr_intc_radn_name_length = 13)
*
*   Internal variables - second round
!      character  temp_variable_name*(fr_intc_radn_name_length)
!                                       ! temporary storage of first part of
!                                       !  variable name

*+  Local Variables
      real       canopy_height         ! height of canopy (mm)
      integer    numvals               ! number of values returned
      real       wind_multiplier_height ! wind run at multiplier height (km/day)

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
cjh   this will never be got as nothing supplies this. To test for crops get the
cjh   crop type.
         !fr_intc_radn
      call get_real_var_optional (
     :      unknown_module
     :     ,'fr_intc_radn'
     :     ,'(-)'
     :     ,g_fr_intc_radn
     :     ,numvals
     :     ,0.0
     :     ,1.0)
 
      if (numvals.eq.0) then
         g_fr_intc_radn = 1.0
 
      elseif (g_fr_intc_radn.lt.1.0) then
         call fatal_error (err_user
     :                    , 'Can''t handle more than one canopy')
 
      else
         ! only one canopy, so ok
 
      endif
 
         !day
      call get_integer_var (
     :      unknown_module
     :     ,'day'
     :     ,'(-)'
     :     ,g_day_of_year
     :     ,numvals
     :     ,1
     :     ,366)
 
         !year
      call get_integer_var (
     :      unknown_module
     :     ,'year'
     :     ,'(-)'
     :     ,g_year
     :     ,numvals
     :     ,min_year
     :     ,max_year)

         !num_hrs calcuates the maximum number of hours of bright sunlight
         ! recordable
      g_n_hrs = day_length (g_day_of_year, g_latitude
     :                     , c_alt_photo_radn)
      if (c_wind_hrs .ge .0.0) then
         g_wind_hrs = c_wind_hrs
      else
         g_wind_hrs = g_n_hrs
      endif
 
 
         !maxt
      call get_real_var (
     :      unknown_module
     :     ,'maxt'
     :     ,'(degC)'
     :     ,g_maxt
     :     ,numvals
     :     ,-30.0
     :     ,50.0)
 
         !mint
      call get_real_var (
     :      unknown_module
     :     ,'mint'
     :     ,'(degC)'
     :     ,g_mint
     :     ,numvals
     :     ,-30.0
     :     ,50.0)
 
         !pa
      call get_real_var_optional (
     :      unknown_module
     :     ,'pa'
     :     ,'(hPa)'
     :     ,g_pa
     :     ,numvals
     :     ,800.0
     :     ,1200.0)
 
      if (numvals.eq.0) then
         g_pa = p_default_pa
      else
         ! pa returned ok
      endif
 
         !radn
      call get_real_var (
     :      unknown_module
     :     ,'radn'
     :     ,'(MJ/m2)'
     :     ,g_radn
     :     ,numvals
     :     ,0.0
     :     ,100.0)
 
         ! convert to W/m2
!      g_radn_wm2 = g_radn*g_fr_intc_radn * divide(1e6
!     :                                  , g_n_hrs*hr2s, -10.0)
 
      g_radn_wm2 = g_radn* divide(1e6
     :                         , g_n_hrs*hr2s, -10.0)
 
         !t_sh
!      call get_real_var (
!     :      unknown_module
!     :     ,'t_sh'
!     :     ,'(hrs)'
!     :     ,g_t_sh
!     :     ,numvals
!     :     ,0.0
!     :     ,100.0)
 
         !lai
      call get_real_var_optional (
     :      unknown_module
     :     ,'lai'
     :     ,'(-)'
     :     ,g_lai
     :     ,numvals
     :     ,0.0
     :     ,20.0)
 
      if (numvals.eq.0) then
         g_lai = 0.0
         g_lai_tot = 0.0
      else
            ! lai returned ok
      endif
      if (g_lai .gt. 0.0) then
         ! ok
         g_lai_tot = max (g_lai, g_lai_tot)
      else
         g_lai_tot = 0.0
      endif
 
         ! canopy height
      call get_real_var_optional (
     :      unknown_module
     :     ,'canopy_height'
     :     ,'(mm)'
     :     ,canopy_height
     :     ,numvals
     :     ,0.0
     :     ,20000.0)
 
      if (numvals.eq.0) then
         g_canopy_height = 0.0
      else
            ! canopy height returned ok
         g_canopy_height =  canopy_height
      endif
 
            ! green cover
      call get_real_var_optional (
     :      unknown_module
     :     ,'cover_green'
     :     ,'()'
     :     ,g_cover_green
     :     ,numvals
     :     ,0.0
     :     ,1.0)
 
      if (numvals.eq.0) then
         g_cover_green = 0.0
      else
            ! cover_green returned ok
      endif
 
         !wind
      call get_real_var_optional (
     :      unknown_module
     :     ,'wind'
     :     ,'(km/day)'
     :     ,g_wind
     :     ,numvals
     :     ,0.0
     :     ,1000.0)
 
      if (numvals.eq.0) then
         g_wind = p_default_wind
      else
         ! wind returned ok
      endif

      call eo_wind_conv (
     :        g_instrum_height, p_disp_instrum, p_z0_instrum
     :      , g_wind
     :      , c_multiplier_height, p_disp_instrum, p_z0_instrum
     :      , wind_multiplier_height)
 
         ! multiply wind by factor
      wind_multiplier_height = wind_multiplier_height 
     :                       * p_wind_multiplier
         ! convert to m/s for daylight hours
      g_wind_ms_multiplier_height = wind_multiplier_height
     :                            * p_wind_day_fraction 
     :                            * km2m
     :                            / (g_wind_hrs*hr2s)
      
      g_wind_ms_multiplier_height = l_bound 
     :                  (g_wind_ms_multiplier_height, c_wind_min)
 
         ! convert back to km/day for penman calculation
      g_wind_adj = g_wind_ms_multiplier_height * 24.0*hr2s/km2m
 
 
      if (g_vpd_source .eq. source_vpd) then
            !vpd
         call get_real_var (
     :         unknown_module
     :         ,'vpd'
     :         ,'(mb)'
     :         ,g_vpd_mb
     :         ,numvals
     :         ,0.0
     :         ,200.0)
 
      elseif (g_vpd_source .eq. source_rh) then
         call get_real_var (
     :         unknown_module
     :         ,'rh'
     :         ,'(%)'
     :         ,g_rh
     :         ,numvals
     :         ,0.0
     :         ,100.0)
 
      elseif (g_vpd_source .eq. source_mint) then
         call get_real_var (
     :         unknown_module
     :         ,'mint'
     :         ,'(oC)'
     :         ,g_mint
     :         ,numvals
     :         ,-30.0
     :         ,50.0)
 
      else
         ! we have an unknown source - none?
         call fatal_error (err_user, ' Insufficient data to derive vpd')
 
      endif
 
      call pop_routine (myname)
      return
      end



*====================================================================
      subroutine Eo_pen_mon ()
*====================================================================
      implicit none
      include   'Eo.inc'               ! Eo common block
      include 'science.pub'                       
      include 'error.pub'                         

*+  Purpose
*     calculate the potential evapotranspiration Eo by penman-monteith

*+  Changes
*       210995 jngh programmed
*       220299 jngh added call to _trans

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'Eo_pen_mon')

*- Implementation Section ----------------------------------
 
      call push_routine (myname)
 
         !  calculate net radiation
 
      call Eo_radiation (g_radn_net)
 
         ! assume that soil and vegetation heat flux is 0.1 of the net radiation
 
      g_fg = c_soil_heat_flux * g_radn_net
!      g_fg = 0.0
 
         ! get specific humidity deficit
 
      call Eo_da (g_da)
 
         ! calculate aeordynamic resistance
 
      call Eo_aerodynamic (g_ra)
 
         ! calculate epsilon
      call Eo_epsilon (g_epsilon)
 
         ! calculate canopy resistance
 
      call Eo_canopy (g_rc
     :               , g_rc_fixed
     :               , g_rc_simple
     :               , g_rc_simulat
     :               , g_rc_kelliher)
 
         ! finally calculate the penman-monteith eo
      call Eo_penman_monteith (g_Eo_pm)
 
         ! finally calculate the penman eo
      call Eo_penman (g_Eo_penman)
 
      call Eo_penman_x_cover (g_Eo_penman_x_cover)
 
         ! finally calculate the penman (doorenbos) eo
      call Eo_penman_doorenbos (g_Eo_penman_doorenbos)
 
         ! calculate the penman eo transpiration
      call Eo_penman_doorenbos_x_cover (g_Eo_penman_doorenbos_x_cover)
 
         ! calculate the penman-monteith eo transpiration
      call Eo_penman_monteith_transp (g_Eo_pm_transp)
 
         ! calculate the penman-monteith eo transpiration
      call Eo_pm_x_cover (g_Eo_pm_x_cover)
 
         ! calculate the penman-monteith eo transpiration
      call Eo_pm_x_kfunction (g_Eo_pm_x_kfunction)
 
         ! calculate the penman-monteith eo transpiration
      call Eo_radn_x_kfunction (g_Eo_radn_x_kfunction)
 
         ! calculate the priestly taylor eo
      call Eo_priestly_taylor (g_Eo_priestly_taylor)
 
         ! calculate the ritchie eo
      call Eo_ritchie (g_Eo_ritchie)
 
         ! calculate the penman-monteith eo transpiration
      call Eo_pm_plant (g_Eo_pm_plant)
 
      !print*, ' eo_plant calc as ', g_eo_pm_plant
 
      call pop_routine (myname)
      return
      end



*====================================================================
      subroutine Eo_radiation (radn_net)
*====================================================================
      implicit none
      include   'const.inc'            ! Global constant definitions
      include   'convert.inc'          ! conversion constants
      include   'Eo.inc'               ! Eo common block
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       radn_net              ! (OUTPUT) net inward radiant flux
                                       ! density (W/m2)

*+  Purpose
*     calculate net radiation

*+  Changes
*       210995 jngh programmed

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'Eo_radiation')
*
!      real       c_cloud
!      parameter (c_cloud = 0.1)
*
!      real       emmis_canopy
!      parameter (emmis_canopy = 0.96)
*
      real       stef_boltz            ! Stefan-Boltzmann constant
      parameter (stef_boltz = 5.67e-8) ! (W/m2/K4)

*+  Local Variables
      real       albedo                ! fraction of radiation reflected (0-1)
      real       ave_temp              ! average daily temp (oC)
      real       ea_mb                 ! vapour pressure (mb)
      real       long_wave_in          ! net incoming long wave radiation (W/m2)
      real       emiss_sky             ! clear sky emissivity

*- Implementation Section ----------------------------------
 
      call push_routine (myname)
 
      if (g_radn_wm2.lt.0)then
         call fatal_error(err_user, '-ve radiation cos daylength = 0')
      else
      endif
 
      call Eo_vp (ea_mb, g_mint)
 
      ave_temp = (g_maxt + g_mint) * 0.5
 
!      emmis_sky = 9.37e-6*(ave_temp + abs_temp)**2
      emiss_sky = 0.70 + 5.95e-5 * ea_mb
     :          * (2.718282**(1500.0/(ave_temp + abs_temp)))
 
!      long_wave_in = (c_cloud + (1.0 - c_cloud)*g_t_sh/g_n_hrs)*
!     :               (emmis_canopy - emmis_sky)*stef_boltz*
!     :               (ave_temp + abs_temp)**4
 
      long_wave_in = (emiss_sky - 1.0) * stef_boltz
     :             * (ave_temp + abs_temp)**4
 
      albedo = p_max_albedo
     :       - (p_max_albedo - p_albedo) * (1.0 - g_cover_green)
 
      radn_net = (1.0 - albedo) * g_radn_wm2 + long_wave_in
 
!      print*, 'fln, fsd, (1-albedo)*fsd,fn,emissa, ea, ta, sboltz'
!      print*, long_wave_in, g_radn_wm2, (1-albedo)*g_radn_wm2,radn_net
!     :      ,emiss_sky, ea_mb, ave_temp, stef_boltz
 
      call pop_routine (myname)
      return
      end



*====================================================================
      subroutine Eo_vp (vapour_pressure, temperature)
*====================================================================
      implicit none
      include   'Eo.inc'               ! Eo common block
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       vapour_pressure       ! (OUTPUT) vapour pressure (mb)
      real       temperature           ! (INPUT) temperature (oC)

*+  Purpose
*     calculate the vapour pressure at a given temperature

*+  Changes
*       210995 jngh programmed

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_vp')

*- Implementation Section ----------------------------------
 
      call push_routine (myname)
 
         ! vapour pressure in millibars
      vapour_pressure = A*exp (B*temperature/(temperature + C))
 
      call pop_routine (myname)
      return
      end



*====================================================================
      subroutine Eo_esat (esat)
*====================================================================
      implicit none
      include   'Eo.inc'               ! Eo common block
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       esat                  ! (OUTPUT) saturated vapour pressure
                                       ! (mb)

*+  Purpose
*     calculate the saturated vapour pressure

*+  Changes
*       210995 jngh programmed

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_esat')

*+  Local Variables
      real       ave_temp              ! average daily temp (oC)
!      real       esat_maxt
!      real       esat_mint

*- Implementation Section ----------------------------------
 
      call push_routine (myname)
 
      ave_temp = (g_maxt + g_mint) * 0.5
 
         ! saturated vapour in millibars
      call Eo_vp (esat, ave_temp)
!      call Eo_vp (esat_maxt, g_maxt)
!      call Eo_vp (esat_mint, g_mint)
!      esat = (esat_maxt + esat_mint)*0.5
 
      call pop_routine (myname)
      return
      end



*====================================================================
      subroutine Eo_da (da)
*====================================================================
      implicit none
      include   'const.inc'            ! Global constant definitions
      include   'Eo.inc'               ! Eo common block
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       da                    ! (OUTPUT) specific humidity deficit
                                       ! (kg/ha)

*+  Purpose
*     calculate the specific humidity deficit (kg/kg)

*+  Changes
*       210995 jngh programmed

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_da')

*+  Local Variables
      real       ea_mb                 ! vapour pressure (mb)
      real       q                     ! specific humidity (kg/kg)
      real       esat                  ! saturated vapour pressure (mb)
      real       qsat                  ! sat specific humidity  (kg/kg)

*- Implementation Section ----------------------------------
 
      call push_routine (myname)
 
      if (g_vpd_source .eq. source_vpd) then
         da = g_vpd_mb*molef/g_pa
 
      elseif (g_vpd_source .eq. source_rh) then
 
            !the vpd stuff - see raupach
 
            !saturated vapour in millibars
         call Eo_esat (esat)
 
            !and in kg/kg
         qsat = molef*esat/g_pa
            ! and in kg/kg
         q = qsat*g_rh/100.0        ! molef*e/(pa)
 
         da = qsat - q
 
      elseif (g_vpd_source .eq. source_mint) then
 
            !saturated vapour in millibars
!         call Eo_esat (esat)
         call Eo_vp (esat, g_maxt)
 
            !and in kg/kg
         qsat = molef*esat/g_pa
 
            ! vapour pressure in millibars
         call Eo_vp (ea_mb, g_mint)
         q = molef*ea_mb/g_pa
         da = p_vpd_fac*(qsat - q)
 
      else
 
         call fatal_error (err_user,
     :        ' Insufficient data to derive specific humidity deficit')
 
      endif
cjh      print*, 'da, source = ', da,'  ', trim(g_vpd_source)
 
      call pop_routine (myname)
      return
      end



*====================================================================
      subroutine Eo_aerodynamic (ra)
*====================================================================
      implicit none
      include   'const.inc'            ! Global constant definitions
      include   'Eo.inc'               ! Eo common block
      include   'convert.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       ra                    ! (OUTPUT) aerodynamic resistance (s/m)

*+  Purpose
*     calculate the aerodynamic resistance, which is the gradient-diffusion
*     aerodynamic resistance in a thermally neutral atmosphere

*+  Changes
*       210995 jngh programmed
*       020398 jngh added adjustment for wind from another site
*                     added adjustment that accounts for the difference
*                     in transfer of heat/water vapour compared to momentum.
*                   changed z0soil to mm

*+  Calls
*   Internal variable
      real       reference_height      ! height above soil for wind clculation (mm)
      real       usuhl                 ! ? (?)
      real       usuh                  ! ? (?)
      real       dh                    ! ? (?)
      real       xx                    ! ? (?)
      real       disp                  ! zero plane displacement under canopy (mm)
      real       psih                  ! ? (?)
      real       z0h                   ! ? (?)
      real       z0                    ! roughness length for momentum transfer (mm)
      real       z0he                  ! roughness length for heat and water
                                       ! vapour transfer (mm)

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_aerodynamic')
*
      real       von_k                 !von Karmen's constant
      parameter (von_k = 0.41)
*
      real       cr                    ! element drag coefficient
      parameter (cr    = 0.3)
*
      real       cs                    ! substrate drag coefficient
      parameter (cs    = 0.003)
*
      real       ccd                   ! constant in d/h equation
      parameter (ccd   = 15.0)
*
      real       ccw                   ! ccw=(zw-d)/(h-d)
      parameter (ccw   = 2.0)
*
      real       usuhm                 ! (max of us/uh)
      parameter (usuhm = 0.3)
c     *           usuhm = 1.0,          ! (max of us/uh)
*
!      real       m2mm
!      parameter (m2mm = 1.0/mm2m)       ! convert metres to mm

      real       ra_open_pan
      parameter (ra_open_pan = 200.0)   ! Open pan Ra (s/m)

!      real       ra_grass
!      parameter (ra_grass = 115.0)      ! Grass Ra (s/m)

*- Implementation Section ----------------------------------
 
      call push_routine (myname)
 
         !now the displacement height, d, roughness for momentum,
         !z0, and roughness for heat, z0he.  use these to calculate
         !aerodynamic resistance, ra.  all from raupach.
 
      if (g_wind_ms_multiplier_height.gt.0.0) then
 
         if (g_canopy_height.gt.0.0 .and. g_lai_tot .gt. 0.0) then
               ! we have some vegetative cover
 
               ! NOTE: this doesn't seem to behave well for low lai
 
               ! find uh/us
            usuhl = sqrt (cs + cr*g_lai_tot)
            usuh  = u_bound (usuhl, usuhm)
!            usuh  = u_bound (usuhl, c_usuh_ub)
 
               ! find d/h and d
               ! when lai < 0.5076, dh becomes -ve
            xx = sqrt (ccd * max(g_lai_tot, 0.001))
            dh = 1.0 - divide (1.0 - exp (-xx), xx, 0.0)
            disp  = dh * g_canopy_height
 
               ! find z0h and z0:
               ! Note: when usuh < usuhm, z0h curve becomes quite different.
            psih = log (ccw) - 1.0 + 1.0/ccw
            z0h = (1.0 - dh) * exp (psih - divide (von_k, usuh, 1.0e20))
            
            z0 = z0h * g_canopy_height
            z0 = l_bound (z0, p_z0soil)
            
!         print*, 'z0, z0h, psih, disp, dh, xx, usuh, usuhl, g_lai_tot'
!     :      //', g_canopy_height'
 
!         print*, z0, z0h, psih, disp, dh, xx, usuh, usuhl, g_lai_tot
!     :         , g_canopy_height
 
         else
               ! soil is bare
            disp = 0.0
            z0 = p_z0soil
 
         endif
         z0he = z0/5.0
 
         if (c_reference_height_base .eq. 'canopy') then
            reference_height = g_canopy_height + c_reference_height
         else
            reference_height = c_reference_height
         endif
 
         call eo_wind_conv (
     :        C_multiplier_height, p_disp_instrum, p_z0_instrum
     :      , g_wind_ms_multiplier_height
     :      , reference_height, disp, z0, g_wind_ms_reference)
 
 
         if (g_canopy_height .le. reference_height) then
            ! reference height sufficient
         else
 
            call fatal_error (err_user,
     :        ' canopy height is above reference height')
 
         endif
 
         ! calculate ratot (from d to za). No stability corrections at
         ! this stage
 
!            ra  = log ((d_za - disp) /z0)*log ((za - disp) /z0he) / ((von_k**2)*ua)
!         ra  = log ((g_instrum_height - disp) /z0)**2
!     :       / ((von_k**2)*g_wind_ms)
 
         ra  = log ((reference_height - disp) /z0)
     :       * log ((reference_height - disp) /z0he)
     :       / ((von_k**2)*g_wind_ms_reference)
 
!      print*,'ra, reference_height, dh, disp, reference_height-disp, z0'
!     :        //', z0h, z0he, g_wind_ms_reference, g_lai_tot'
!      print*, ra, reference_height, dh, disp, reference_height-disp, z0
!     :        ,z0h, z0he, g_wind_ms_reference, g_lai_tot
 
!         ra = ra*.3
 
      else
         ra = ra_open_pan
         g_wind_ms_reference = 0.0
!         ra = 1.0e20
      endif
      ra = u_bound (ra, c_ra_ub)
 
      call pop_routine (myname)
      return
      end



*====================================================================
      subroutine Eo_canopy (rc
     :                     , rc_fixed
     :                     , rc_simple
     :                     , rc_simulat
     :                     , rc_kelliher)
*====================================================================
      implicit none
      include   'const.inc'            ! Global constant definitions
      include   'Eo.inc'               ! Eo common block
      include   'convert.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       rc                    ! (OUTPUT) canopy resistance (s/m)
      real       rc_fixed              ! (OUTPUT) canopy resistance (s/m)
      real       rc_simple             ! (OUTPUT) canopy resistance (s/m)
      real       rc_simulat            ! (OUTPUT) canopy resistance (s/m)
      real       rc_kelliher           ! (OUTPUT) canopy resistance (s/m)

*+  Purpose
*     calculate the canopy resistance, which accounts for the biological
*     control upon evaporation, and is the most sensitive part of the
*     eo_pm equation in typical conditions.

*+  Changes
*       040398 jngh programmed

*+  Calls
      real       Eo_lambda             ! function
*
*
*   Internal variable
!      real       rsmin                 ! minimum bulk vegetation surface resistance (s/m)
!      real       gsmax                 ! maximum value of stomatal conductance of
!                                       ! individual leaves (mm/s)
!      real       cq                    ! extinction coefficient for the attenuation of
!                                       ! photosynthetically active radiation
!      real       q                     ! photosynthetically active radiation (micromol/m2/s)
!      real       qa50_fract
!      real       par
!      real       qh                    ! q incident at the top of the plant canopy (micromol/m2/s)
!      real       qa50                  ! value of q absorbed by an individual leaf when stomatal
!                                       ! conductance is at 50% of its maximum (micromol/m2/s)
!      real       q50                   ! value of q when stomatal conductance is at 50% of
!                                       ! its maximum (micromol/m2/s)
      real       rsmin_canopy          ! minimum leaf stomatal resistance (s/m)
      real       rsmin_soil            ! minimum soil resistance (s/m)
      real       gsmax_canopy          ! maximum stomatal conductance (m/s)
      real       gsmax_soil            ! maximum soil conductance (m/s)
      real       gc                    ! bulk plant canopy conductance (m/s)
      real       soil_part             ! fraction of radiation reaching soil surface (0-1)
      real       canopy_part           ! fraction of radiation absorbed by canopy (0-1)
      real       ave_temp              ! average daily temp (oC)
      real       density_air           ! dry air density (kg/m3)
      real       gs                    ! bulk surface conductance (mm/s)
      real       gj                    ! isothermal conductance proportional to the ratio of
                                       ! avail energy flux density and air saturation deficit
                                       ! (micromol/m2/s)
      real       term1
      real       term2
      real       ga                    ! bulk vegetation aerodynamic conductance (mm/s)
      real       rc_raupach
      real       R0
      real       D0
      real       L0
      real       fr
      real       fd
      real       fl
      real       rcmin
      
!      real       par_fract

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_canopy')
*
!      real       k                     ! extinction coefficient
!      parameter (k = 0.4)

*- Implementation Section ----------------------------------
 
      call push_routine (myname)
 
            ! SIMULAT method
         rsmin_soil = 1.0
         rsmin_canopy = c_rsmin_canopy
 
         soil_part = 1.0 - g_cover_green
         canopy_part = 1.0 - soil_part
 
         gsmax_canopy = canopy_part / rsmin_canopy
         gsmax_soil = soil_part / rsmin_soil
 
         gc = gsmax_canopy + gsmax_soil
         rc_simulat = divide (1.0, gc, 0.0)
 
            ! simple
         rc_simple = divide (c_rsmin_canopy, g_lai, 0.0)
 
            ! raupach
         R0 = c_radn_crit
         D0 = c_vpd_crit
         L0 = c_lai_crit
         rcmin = c_rsmin_canopy
         fl = min (g_lai/L0, 1.0)
!         fl = 1.0
!         rcmin = rc_simple
         
         fr = min (g_radn_wm2/R0,1.0)
         fd = max (1.0 - g_da/D0, 0.0)
!         fd = max (g_da/D0, 0.0)
!       fd = 1.0
       rc_raupach = divide (rcmin, fr*fd*fl, 0.0)
 
 
            ! kelliher
         gsmax_canopy = divide (g_cover_green, c_rsmin_canopy, 0.0)
         gc = g_lai * gsmax_canopy
 
            ! Kelliher et al. method
   !      rsmin = 87.0
   !      qa50_fract = 0.75
   !      par_fract = 1.0
 
   !      par = g_radn_net * par_fract
 
   !      if (g_lai .ne. 0.0) then
   !         par_fract = 1.0
   !         cq = -divide(log(1.0-g_cover_green * par_fract), g_lai, k)
 
   !         gsmax = (1.0/rsmin) / 1000.0
 
   !         qh = par
   !         q = par * g_cover_green
   !         qa50 = q * qa50_fract
   !         q50 = qa50/cq
 
   !         gc = gsmax/cq
   !     :      * log((qh + qa50)/(qh * (1.0-g_cover_green) + q50))
   !      else
   !         gc = 0.0
   !      endif
 
         ga = divide (1.0, g_ra, 0.0)
 
         ave_temp = (g_maxt + g_mint) * 0.5
         density_air = mwair*g_pa*100.0 / ((ave_temp + abs_temp)* r_gas)
 
         gj = (g_radn_net - g_fg)/(density_air * Eo_lambda () * g_da)
         term1 = ga/(g_epsilon*gj)
         term2 = ga/(g_epsilon+1.0)
 
         gs = divide ((gc + gc*term1 + (1.0-g_cover_green)*term2)
     :      , (g_cover_green + term1), 0.0)
         rc_kelliher = divide (1.0, gs, 0.0)
 
            ! fixed
         rc_fixed = c_rc
 
      if (c_rc_method .eq. 'simulat') then
 
         rc = rc_simulat
 
      else if (c_rc_method .eq. 'simple') then
         rc = rc_simple
 
      else if (c_rc_method .eq. 'kelliher') then
 
         rc = rc_kelliher
 
      else if (c_rc_method .eq. 'fixed') then
 
         rc = rc_fixed
 
      else if (c_rc_method .eq. 'raupach') then
 
         rc = rc_raupach
      else
         call fatal_error (err_user,
     :        c_rc_method//' rc method is not recognised')
 
 
      endif
 
      call pop_routine (myname)
      return
      end



*====================================================================
      subroutine Eo_wind_conv (z1, d1, zruff1, windz1
     :                        , z2, d2, zruff2, windz2)
*====================================================================
      implicit none
      include   'const.inc'            ! Global constant definitions
      include   'Eo.inc'               ! Eo common block
      include   'convert.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       z1                    ! (INPUT) original instrument height (mm)
      real       d1                    ! (INPUT) original zero plane displacement (mm)
      real       zruff1                ! (INPUT) original roughness length ()
      real       windz1                ! (INPUT) original wind speed (m/s)
      real       z2                    ! (INPUT) new instrument height (mm)
      real       d2                    ! (INPUT) new zero plane displacement (mm)
      real       zruff2                ! (INPUT) new roughness length ()
      real       windz2                ! (OUTPUT) new wind speed (m/s)

*+  Purpose
*     calculate the wind speed at another site with different conditions

*+  Changes
*       260298 jngh programmed

*+  Calls
*   Internal variable
      real       conversion_height     ! height which is not affected by surface (mm)
      real       ustar_old             ! old friction velocity (?)
      real       ustar_new             ! new friction velocity (?)
      real       windz10               ! wind at 10m (m/s)

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_wind_conv')
*
      real       von_k                 !von Karmen's constant
      parameter (von_k = 0.41)

*- Implementation Section ----------------------------------
 
      call push_routine (myname)
 
      if (c_reference_height_base .eq. 'canopy') then
cjh!         conversion_height = c_zc_conversion + g_canopy_height
         conversion_height = c_zc_conversion
      else
         conversion_height = c_zc_conversion
      endif
 
      if (g_canopy_height .le. 0.5*conversion_height) then
         ! conversion height sufficient
      else
 
         call warning_error (err_user,
     :        ' canopy height is above 0.5 * conversion height')
 
      endif
 
 
*        translate input windspeed to 10 m, using zo and d appropriate for that site
 
      ustar_old  = von_k * windz1 / (log((z1-d1)/zruff1))
      windz10    = ustar_old / von_k
     :           * (log((conversion_height-d1)/zruff1))
 
*        and then adjust ustar for new roughness and zero plane displacement
 
      ustar_new  = von_k * windz10
     :           / (log((conversion_height-d2)/zruff2))
 
*        then calculate new U(z2)
 
      windz2 = ustar_new / von_k * (log((z2-d2)/zruff2))
 
      call pop_routine (myname)
      return
      end



*====================================================================
      real function Eo_lambda ()
*====================================================================
      implicit none
      include   'Eo.inc'               ! Eo common block
      include 'error.pub'                         

*+  Purpose
*     calculate the lambda (latent heat of vapourisation for water)(J/kg/oK)
*              also known as the specific heat of air at constant pressure.

*+  Changes
*       210995 jngh programmed

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_lambda')

*+  Local Variables
      real       ave_temp              ! average daily temp (oC)

*- Implementation Section ----------------------------------
 
      call push_routine (myname)
 
         ! temperature functions.  lambda is the slope
 
      ave_temp = (g_maxt + g_mint) * 0.5
      Eo_lambda = (2501.0 - 2.38*ave_temp)*1000.0      ! J/kg
 
      call pop_routine (myname)
      return
      end



*====================================================================
      subroutine Eo_epsilon (epsilon)
*====================================================================
      implicit none
      include   'Eo.inc'               ! Eo common block
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       epsilon               ! (OUTPUT) the slope of saturation 
                                       ! vapour pressure curve (mb/oK)

*+  Purpose
*     calculate epsilon, the slope of saturation vapour pressure curve (mb/oK)
*     d(sat spec humidity)/dT ((kg/kg)/K) FROM TETEN FORMULA

*+  Changes
*       210995 jngh programmed

*+  Calls
      real       Eo_lambda             ! function

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_epsilon')
*
      real       capp                  ! Specific heat of air at constant pressure
      parameter (capp = 1004.0)        ! (J/kg/K)

*+  Local Variables
      real       ave_temp              ! average daily temp (oC)
      real       desdt                 ! d(sat VP)/dT: (mb/K)
      real       dqsdt                 ! d(sat spec hum)/dT: (kg/kg)/K
      real       esat                  ! saturated vapour pressure (mb)

*- Implementation Section ----------------------------------
 
      call push_routine (myname)
 
         !temperature functions.  epsilon is the slope       if e_sat with
         !temperature, data_non is the non-dimenional form according to raupach
 
      ave_temp = (g_maxt + g_mint) * 0.5
      call Eo_esat (esat)
 
      desdt = esat*B*C/ (C + ave_temp)**2   ! d(sat VP)/dT: (mb/K)
      dqsdt = (mwh2o/mwair) *desdt/g_pa     ! d(sat spec hum)/dT: (kg/kg)/K
      epsilon = (Eo_lambda ()/capp) *dqsdt  ! dimensionless
 
      call pop_routine (myname)
      return
      end



*====================================================================
      subroutine Eo_penman (pen_mon)
*====================================================================
      implicit none
      include   'convert.inc'          ! conversion constants
      include   'Eo.inc'               ! Eo common block
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       pen_mon               ! (OUTPUT) evaporation rate ()

*+  Purpose
*     calculate the Penman evaporation rate

*+  Changes
*       210995 jngh programmed

*+  Calls
      real       Eo_lambda             ! function

*+  Constant Values
      real       penman_reference_height      ! reference height (mm)
      parameter (penman_reference_height = 2000.0)

      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_penman')

*+  Local Variables
      real       fe                    ! latent heat flux (W/m2)
      real       fU2                   ! wind function (cal/cm2/day)
      real       conv                  ! conversion of fU2 to kg/m2/s
      real       U2                    ! wind speed at reference height (km/day)

*- Implementation Section ----------------------------------
 
      call push_routine (myname)
 
!      wind_min_km = c_wind_min * g_wind_hrs*hr2s/km2m
!     :            / p_wind_day_fraction

         ! g_wind is already converted for daylight hours proportion
         
!      call eo_wind_conv (
!     :        g_instrum_height, p_disp_instrum, p_z0_instrum
!     :      , g_wind
!     :      , reference_height, p_disp_instrum, p_z0_instrum
!     :      , U2)
 
!      U2 = U2 * p_wind_multiplier
      U2 = g_wind_adj 
     :   * (penman_reference_height / c_multiplier_height)**(1.0/6.0)
      fU2 = c_penman_fU2_coef_a*(1.0 + c_penman_fU2_coef_b*U2)
      conv = (g_pa / molef) / (g_n_hrs * hr2s) * gm2kg/(scm2smm*smm2sm)
 
         !and now  penman
 
      fe = (g_epsilon * (g_radn_net)
     :    + (Eo_lambda () * fU2 * g_da * conv))
     :    / (g_epsilon + 1.0)
 
      pen_mon = fe/Eo_lambda ()*g_n_hrs*hr2s   ! to convert back to a daily basis
     :        * p_adjustment_factor
 
      call Bound_check_real_var (pen_mon, 0.0, c_pen_mon_ub, 'pen')
      pen_mon = l_bound (pen_mon, 0.0)
 
      call pop_routine (myname)
      return
      end

*====================================================================
      subroutine Eo_penman_doorenbos (pen_mon)
*====================================================================
      implicit none
      include   'convert.inc'          ! conversion constants
      include   'Eo.inc'               ! Eo common block
      include 'data.pub'                          
      include 'error.pub' 
      include 'science.pub'                  

*+  Sub-Program Arguments
      real       pen_mon               ! (OUTPUT) evaporation rate ()

*+  Purpose
*     calculate the Penman evaporation rate

*+  Changes
*       210995 jngh programmed

*+  Calls
      real       Eo_lambda             ! function

*+  Constant Values
      real       penman_reference_height      ! reference height (mm)
      parameter (penman_reference_height = 2000.0)

      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_penman_doorenbos')

*+  Local Variables
      real       fe                    ! latent heat flux (W/m2)
      real       fU2                   ! wind function (cal/cm2/day)
      real       conv                  ! conversion of fU2 to kg/m2/s
      real       U2                    ! wind speed at reference height (km/day)
      real       wind_min_km           ! minimum wind speed at reference height (km/day)
      real       cvf(4,4,3)              ! adjustment factor - cvf(wind,radn,vpd)
      real       radn_crit(4)
      real       wind_crit(4)
      real       wind_ms
      real       rh_crit(3)
      real       rh
      real       rh_max
      integer    radn_index
      integer    wind_index
      integer    rh_index
      real       y_index(4)
      real       esat
      real       esat_m
      real       esat_a
      real       qsat
      real       ave_t
      real       ea_mb
      real       da
      real       q
      real       nightT
      real       rh_da
      real       rh_gda
      real       rh_avt
      real       rh_maxt
      real       rh_avea
      real       rh_avead


*- Implementation Section ----------------------------------
 
      call push_routine (myname)
 
      cvf(1,1,1)= 0.86
      cvf(2,1,1)= 0.69
      cvf(3,1,1)= 0.53
      cvf(4,1,1)= 0.37
      cvf(1,2,1)= 0.9
      cvf(2,2,1) = 0.76
      cvf(3,2,1)=0.61
      cvf(4,2,1)=0.48
      cvf(1,3,1)=1.0
      cvf(2,3,1)=0.85
      cvf(3,3,1)=0.74
      cvf(4,3,1)=0.65
      cvf(1,4,1)=1.0
      cvf(2,4,1)=0.92
      cvf(3,4,1)=0.84
      cvf(4,4,1)=0.76

      cvf(1,1,2)= 0.96
      cvf(2,1,2)= 0.83
      cvf(3,1,2)= 0.70
      cvf(4,1,2)= 0.59
      cvf(1,2,2)= 0.98
      cvf(2,2,2) = 0.91
      cvf(3,2,2)=0.80
      cvf(4,2,2)=0.70
      cvf(1,3,2)=1.05
      cvf(2,3,2)=0.99
      cvf(3,3,2)=0.94
      cvf(4,3,2)=0.84
      cvf(1,4,2)=1.05
      cvf(2,4,2)=1.05
      cvf(3,4,2)=1.02
      cvf(4,4,2)=0.95

      cvf(1,1,3)= 1.02
      cvf(2,1,3)= 0.89
      cvf(3,1,3)= 0.79
      cvf(4,1,3)= 0.71
      cvf(1,2,3)= 1.06
      cvf(2,2,3)= 0.98
      cvf(3,2,3)=0.92
      cvf(4,2,3)=0.81
      cvf(1,3,3)=1.10
      cvf(2,3,3)=1.10
      cvf(3,3,3)=1.05
      cvf(4,3,3)=0.96
      cvf(1,4,3)=1.10
      cvf(2,4,3)=1.14
      cvf(3,4,3)=1.12
      cvf(4,4,3)=1.06

      radn_crit(1)=7.5
      radn_crit(2)=15.0
      radn_crit(3)=22.5
      radn_crit(4)=30.0

      wind_crit(1)=0.0
      wind_crit(2)=3.0
      wind_crit(3)=6.0
      wind_crit(4)=9.0

      rh_crit(1)=0.3
      rh_crit(2)=0.6
      rh_crit(3)=0.9

      y_index(1)=1.0
      y_index(2)=2.0
      y_index(3)=3.0
      y_index(4)=4.0

      radn_index = int(0.5 +
     :             linear_interp_real (g_radn, radn_crit, y_index, 4))
      ave_t = (g_maxt + g_mint)*0.5
      nightT = 0.29*g_maxT + 0.71*g_minT 
      call Eo_vp (esat_m, g_maxt)
      call Eo_vp (esat_a, ave_t)
      call Eo_vp (esat, nightT)
      qsat = molef*esat_m/g_pa
 
            ! vapour pressure in millibars
      call Eo_vp (ea_mb, g_mint)
      q = molef*ea_mb/g_pa
      da = (qsat - q)
!      da = g_da
      rh_avt = ea_mb/esat_a
      rh_avea = ea_mb/(0.5*esat_m+0.5*ea_mb)
      rh_avead = ea_mb/(0.75*esat_m+0.25*ea_mb)
      rh_maxt = ea_mb/esat_m
      rh_gda = 1.0-g_da/(molef*esat_m/g_pa)
      rh_da = 1.0-da/(molef*esat_m/g_pa)
      rh_max = ea_mb/esat
    
      rh_index = int(0.5 + 
     :           linear_interp_real (rh_avead, rh_crit, y_index, 3))

         ! g_wind is already converted for daylight hours proportion
         
      call eo_wind_conv (
     :        g_instrum_height, p_disp_instrum, p_z0_instrum
     :      , g_wind * p_wind_multiplier
     :      , penman_reference_height, p_disp_instrum, p_z0_instrum
     :      , U2)
 
      wind_min_km = c_wind_min * g_wind_hrs*hr2s/km2m
     :            / p_wind_day_fraction
      
      wind_ms = U2
     :          * 0.66 
     :          * km2m
     :          / (12.0*hr2s)
!     :          / (24.0*hr2s)
      
      wind_index = int(0.5 +
     :            linear_interp_real (wind_ms, wind_crit, y_index, 4))
      
      U2 = l_bound (U2, wind_min_km)

!      U2 = g_wind_adj 
!     :   * (penman_reference_height / c_multiplier_height)**(1.0/6.0)

      fU2 = 0.027*(1.0 + 0.01*U2)
      conv = (g_pa / molef) / (g_n_hrs * hr2s) * gm2kg/(scm2smm*smm2sm)
 
         !and now  penman
 
      fe = (g_epsilon * (g_radn_net)
     :    + (Eo_lambda () * fU2 * g_da * conv))
     :    / (g_epsilon + 1.0)
 
      pen_mon = fe/Eo_lambda ()*g_n_hrs*hr2s   ! to convert back to a daily basis
     :        * p_adjustment_factor
     :        * cvf(wind_index,radn_index,rh_index)

      
      !write(200,*) 'rh_avt, rh_maxt,rh_max, rh_da, rh_gda'
      !write(200,*) rh_avt, rh_maxt,rh_max, rh_da, rh_gda
      !write(200,*) rh_avt, rh_maxt,rh_max, rh_da, rh_gda
!      write(200,*) cvf(wind_index,radn_index,int(0.5 + 
!     :           linear_interp_real (rh_avt, rh_crit, y_index, 3)))
!     :            , cvf(wind_index,radn_index,int(0.5 + 
!     :           linear_interp_real (rh_maxt, rh_crit, y_index, 3)))
!     :            , cvf(wind_index,radn_index,int(0.5 + 
!     :           linear_interp_real (rh_max, rh_crit, y_index, 3)))
!     :            , cvf(wind_index,radn_index,int(0.5 + 
!     :           linear_interp_real (rh_da, rh_crit, y_index, 3)))
!     :            , cvf(wind_index,radn_index,int(0.5 + 
!     :           linear_interp_real (rh_gda, rh_crit, y_index, 3)))
      !write(200,*) wind_ms, g_radn, rh_max
      !write(200,*) linear_interp_real (wind_ms, wind_crit, y_index, 4)
      !write(200,*) wind_crit
      !write(200,*) y_index
      !write(200,*)  wind_index,radn_index,rh_index
      !write(200,*)  cvf(wind_index,radn_index,rh_index)
 
      call Bound_check_real_var (pen_mon, 0.0, c_pen_mon_ub, 'pen')
      pen_mon = l_bound (pen_mon, 0.0)
 
      call pop_routine (myname)
      return
      end

*====================================================================
      subroutine Eo_penman_monteith (pen_mon)
*====================================================================
      implicit none
      include   'convert.inc'          ! conversion constants
      include   'Eo.inc'               ! Eo common block
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       pen_mon               ! (OUTPUT) evaporation rate ()

*+  Purpose
*     calculate the Penman-Monteith evaporation rate

*+  Changes
*       210995 jngh programmed

*+  Calls
      real       Eo_lambda             ! function

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_penman_monteith')

*+  Local Variables
      real       ave_temp              ! average daily temp (oC)
      real       density_air           ! dry air density (kg/m3)
      real       fe                    ! latent heat flux (W/m2)

*- Implementation Section ----------------------------------
 
      call push_routine (myname)
 
      ave_temp = (g_maxt + g_mint) * 0.5
      density_air = mwair*g_pa*100.0 / ((ave_temp + abs_temp)* r_gas)
 
         !and now raupach's version of penman-monteith
 
      fe = (g_epsilon * (g_radn_net - g_fg)
     :    + divide (density_air * Eo_lambda () * g_da, g_ra, 1.0e20))
     :    / (g_epsilon + divide (g_rc, g_ra, 1.0e20) + 1.0)
 
      !print*, 'fe, epsi, fn, fg, rho, rlam,da,ratot,rsv'
      !print*, fe, g_epsilon, g_radn_net, g_fg, density_air
      !:      , Eo_lambda (),g_da,g_ra,g_rc
 
      pen_mon = fe/Eo_lambda ()*g_n_hrs*hr2s   ! to convert back to a daily basis
     :        * p_adjustment_factor
 
      call Bound_check_real_var (pen_mon, 0.0, c_pen_mon_ub, 'pen_mon')
      pen_mon = l_bound (pen_mon, 0.0)
 
      call pop_routine (myname)
      return
      end



*====================================================================
      subroutine Eo_penman_monteith_transp (pen_mon)
*====================================================================
      implicit none
      include   'convert.inc'          ! conversion constants
      include   'Eo.inc'               ! Eo common block
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       pen_mon               ! (OUTPUT) transpiration rate ()

*+  Purpose
*     calculate the Penman-Monteith transpiration rate for plant

*+  Changes
*       210995 jngh programmed
*       220299 jngh changed name to _trans from _plant

*+  Calls
      real       Eo_lambda             ! function

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_penman_monteith_trans')

*+  Local Variables
      real       ave_temp              ! average daily temp (oC)
      real       density_air           ! dry air density (kg/m3)
      real       fe                    ! latent heat flux (W/m2)

*- Implementation Section ----------------------------------
 
      call push_routine (myname)
 
      ave_temp = (g_maxt + g_mint) * 0.5
      density_air = mwair*g_pa*100.0 / ((ave_temp + abs_temp)* r_gas)
 
         !and now raupach's version of penman-monteith
      if (g_cover_green .gt. 0.0) then
 
         fe = (g_epsilon * g_radn_net * g_cover_green
     :      + divide (density_air * Eo_lambda () * g_da, g_ra, 1.0e20))
     :      / (g_epsilon + divide (g_rc, g_ra, 1.0e20) + 1.0)
 
      else
         fe = 0.0
      endif
 
      pen_mon = fe/Eo_lambda ()*g_n_hrs*hr2s   ! to convert back to a daily basis
     :        * p_adjustment_factor
cjh      print*, 'g_da, fe = ', g_da, fe 
      call pop_routine (myname)
      return
      end


*====================================================================
      subroutine Eo_radn_x_Kfunction (pen_mon)
*====================================================================
      implicit none
      include   'convert.inc'          ! conversion constants
      include   'Eo.inc'               ! Eo common block
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       pen_mon               ! (OUTPUT) transpiration rate ()

*+  Purpose
*     calculate the adjusted Penman-Monteith evaporation rate for plant

*+  Changes
*       220299 jngh programmed

*+  Calls
      real       Eo_lambda             ! function

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_radn_x_Kfunction')

*+  Local Variables
      real       ave_temp              ! average daily temp (oC)
      real       density_air           ! dry air density (kg/m3)
      real       fe                    ! latent heat flux (W/m2)
      real       kfunction             ! surrogate cover using
                                       ! a fixed extinction coeff

*- Implementation Section ----------------------------------
 
      call push_routine (myname)
 
      ave_temp = (g_maxt + g_mint) * 0.5
      density_air = mwair*g_pa*100.0 / ((ave_temp + abs_temp)* r_gas)
 
         !and now raupach's version of penman-monteith
      if (g_lai .gt. 0.0) then
 
         kfunction = (1.0 - exp (-p_extinct_coef*g_lai))
         
         fe = (g_epsilon * (g_radn_net - g_fg) * kfunction
     :      + divide (density_air * Eo_lambda () * g_da, g_ra, 1.0e20))
     :      / (g_epsilon + divide (g_rc, g_ra, 1.0e20) + 1.0)
 
      else
         fe = 0.0
      endif
 
      pen_mon = fe/Eo_lambda ()*g_n_hrs*hr2s   ! to convert back to a daily basis
     :        * p_adjustment_factor
 
      call pop_routine (myname)
      return
      end


*====================================================================
      subroutine Eo_pm_x_kfunction (pen_mon)
*====================================================================
      implicit none
      include   'convert.inc'          ! conversion constants
      include   'Eo.inc'               ! Eo common block
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       pen_mon               ! (OUTPUT) transpiration rate ()

*+  Purpose
*     calculate the adjusted Penman-Monteith evaporation rate for plant

*+  Changes
*       220299 jngh programmed

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_pm_x_k_function')

*+  Local Variables
*- Implementation Section ----------------------------------
 
      call push_routine (myname)
      
      if (g_lai.gt.0.0) then
         pen_mon = g_Eo_pm * (1.0 - exp (-p_extinct_coef*g_lai))       
      else
         pen_mon = 0.0      
      endif

      call pop_routine (myname)
      return
      end


*====================================================================
      subroutine Eo_pm_x_cover (pen_mon)
*====================================================================
      implicit none
      include   'convert.inc'          ! conversion constants
      include   'Eo.inc'               ! Eo common block
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       pen_mon               ! (OUTPUT) transpiration rate ()

*+  Purpose
*     calculate the adjusted Penman-Monteith evaporation rate for plant

*+  Changes
*       220299 jngh programmed

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_pm_x_cover')

*+  Local Variables
*- Implementation Section ----------------------------------
 
      call push_routine (myname)
      
      pen_mon = g_Eo_pm * g_cover_green

      call pop_routine (myname)
      return
      end

*====================================================================
      subroutine Eo_penman_x_cover (pen_mon)
*====================================================================
      implicit none
      include   'convert.inc'          ! conversion constants
      include   'Eo.inc'               ! Eo common block
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       pen_mon               ! (OUTPUT) transpiration rate ()

*+  Purpose
*     calculate the adjusted Penman-Monteith evaporation rate for plant

*+  Changes
*       220299 jngh programmed

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_penman_x_cover')

*+  Local Variables
*- Implementation Section ----------------------------------
 
      call push_routine (myname)
      
      pen_mon = g_Eo_penman * g_cover_green

      call pop_routine (myname)
      return
      end



*====================================================================
      subroutine Eo_penman_doorenbos_x_cover (pen_mon)
*====================================================================
      implicit none
      include   'convert.inc'          ! conversion constants
      include   'Eo.inc'               ! Eo common block
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       pen_mon               ! (OUTPUT) transpiration rate ()

*+  Purpose
*     calculate the adjusted Penman-Monteith evaporation rate for plant

*+  Changes
*       220299 jngh programmed

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_penman_doorenbos_x_cover')

*+  Local Variables
*- Implementation Section ----------------------------------
 
      call push_routine (myname)
      
      pen_mon = g_Eo_penman_doorenbos * g_cover_green

      call pop_routine (myname)
      return
      end


*====================================================================
      subroutine Eo_pm_plant (pen_mon)
*====================================================================
      implicit none
      include   'const.inc'
      include   'convert.inc'          ! conversion constants
      include   'Eo.inc'               ! Eo common block
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       pen_mon               ! (OUTPUT) transpiration rate ()

*+  Purpose
*     calculate the adjusted Penman-Monteith evaporation rate for plant

*+  Changes
*       220299 jngh programmed

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_pm_plant')

*+  Local Variables
*- Implementation Section ----------------------------------
 
      call push_routine (myname)
      
      if (p_eo_plant_method.eq.'eo_transpiration') then
         pen_mon = g_eo_pm_transp
      elseif (p_eo_plant_method.eq.'eo_pm') then
         pen_mon = g_eo_pm
      elseif (p_eo_plant_method.eq.'eo_penman') then
         pen_mon = g_eo_penman
      elseif (p_eo_plant_method.eq.'eo_penman_x_cover') then
         pen_mon = g_eo_penman_x_cover
      elseif (p_eo_plant_method.eq.'eo_penman_d_x_cover') then
         pen_mon = g_eo_penman_doorenbos_x_cover
      elseif (p_eo_plant_method.eq.'eo_pm_x_cover') then
         pen_mon = g_eo_pm_x_cover
      elseif (p_eo_plant_method.eq.'eo_pm_x_kfunction') then
         pen_mon = g_eo_pm_x_Kfunction
      elseif (p_eo_plant_method.eq.'eo_radn_x_kfunction') then
         pen_mon = g_eo_radn_x_Kfunction
      elseif (p_eo_plant_method.eq.'eo_priestly_taylor') then
         pen_mon = g_eo_priestly_taylor
      elseif (p_eo_plant_method.eq.'eo_ritchie') then
         pen_mon = g_eo_ritchie
      elseif (p_eo_plant_method.eq.'null') then
         pen_mon = 0.0
      else
         pen_mon = 0.0
         call fatal_error (err_user,
     :        ' Unknown eo_plant method selected '//p_eo_plant_method)
      endif
      
      if (g_lai.gt.0.0) then
         
      else
         pen_mon = 0
      endif

      call pop_routine (myname)
      return
      end





*====================================================================
      subroutine Eo_priestly_taylor (priestly_taylor)
*====================================================================
      implicit none
      include   'convert.inc'          ! conversion constants
      include   'Eo.inc'               ! Eo common block
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       priestly_taylor       ! (OUTPUT) evaporation rate ()

*+  Purpose
*     calculate the Priestly Taylor evaporation rate

*+  Changes
*       060398 jngh programmed

*+  Calls
      real       Eo_lambda             ! function

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_priestly_taylor')

*+  Local Variables
      real       fe                    ! latent heat flux (W/m2)
      real       W                     ! dimensionless weighting factor
                                       ! that accounts for effects of
                                       ! temperature and pressure
      real       Qstar                 ! net radiation
      real       alpha                 ! 

*- Implementation Section ----------------------------------
 
      call push_routine (myname)
 
      W = g_epsilon/(g_epsilon + 1.0)
      Qstar =  g_radn_net - g_fg
      alpha = 1.26
      fe = alpha * W * Qstar
 
      priestly_taylor = fe/Eo_lambda ()*g_n_hrs*hr2s   ! to convert back to a daily basis
 
      call pop_routine (myname)
      return
      end


*====================================================================
      subroutine Eo_send_my_variable (Variable_name)
*====================================================================
      implicit none
      include   'Eo.inc'               ! Eo common block
      include 'convert.inc'
      include 'engine.pub'                        
      include 'intrface.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      character  Variable_name*(*)     ! (INPUT) Variable name to search for

*+  Purpose
*            return the value of one of our variables to       caller

*+  Changes
*       210995 jngh programmed

*+  Local Variables
      real     vpd                     ! vpd (kpa)

*+  Constant Values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'Eo_send_my_variable')

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
         !  penman-monteith evaporation
      if (variable_name .eq. 'eo_pm') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(mm)'
     :              ,g_Eo_pm)
 
         !  penman potential evaporation
      else if (variable_name .eq. 'eo_penman') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(mm)'
     :              ,g_Eo_penman)
 
         !  penman doorenbos potential evaporation
      else if (variable_name .eq. 'eo_penman_d') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(mm)'
     :              ,g_Eo_penman_doorenbos)
 
         !  penman-monteith plant transpiration
      else if (variable_name .eq. 'eo_transp') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(mm)'
     :              ,g_Eo_pm_transp)
 
         ! adjusted  penman-monteith by cover
      else if (variable_name .eq. 'eo_pm_x_cover') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(mm)'
     :              ,g_Eo_pm_x_cover)
 
         ! adjusted  penman by cover
      else if (variable_name .eq. 'eo_penman_x_cover') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(mm)'
     :              ,g_Eo_penman_x_cover)
 
         ! adjusted  penman doorenbos by cover
      else if (variable_name .eq. 'eo_penman_d_x_cover') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(mm)'
     :              ,g_Eo_penman_doorenbos_x_cover)
 
         ! adjusted  penman-monteith by k function
      else if (variable_name .eq. 'eo_pm_x_kfunction') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(mm)'
     :              ,g_Eo_pm_x_kfunction)
 
         ! adjusted  penman-monteith by k function
      else if (variable_name .eq. 'eo_radn_x_kfunction') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(mm)'
     :              ,g_Eo_radn_x_kfunction)
 
         ! adjusted  penman-monteith plant transpiration
      else if (variable_name .eq. 'eo_plant'
     :     .and. p_eo_plant_method .ne. 'null') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(mm)'
     :              ,g_Eo_pm_plant)
      !print*, ' eo_plant sent as ', g_eo_pm_plant
 
         !  priestly taylor soil evaporation
      else if (variable_name .eq. 'eo_soil') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(mm)'
     :              ,g_eo_priestly_taylor*(1-g_cover_green))
 
         !  priestly taylor PET
      else if (variable_name .eq. 'eo_priestly_taylor') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(mm)'
     :              ,g_eo_priestly_taylor)
 
         !  ritchie PET
      else if (variable_name .eq. 'eo_ritchie') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(mm)'
     :              ,g_eo_ritchie)
 
      else if (variable_name .eq. 'eo_vpd') then
         vpd = g_da/molef*g_pa*mb2kpa
         call respond2get_real_var (
     :               variable_name
     :              ,'(kpa)'
     :              , vpd)
 
      else if (variable_name .eq. 'g_canopy_height') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(mm)'
     :              ,g_canopy_height)
 
      else if (variable_name .eq. 'wind_ms_multiplier_height') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(m/s)'
     :              ,g_wind_ms_multiplier_height)
 
      else if (variable_name .eq. 'wind_ms_reference') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(m/s)'
     :              ,g_wind_ms_reference)
 
      else if (variable_name .eq. 'wind_adj') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(km/day)'
     :              ,g_wind_adj)
 
      else if (variable_name .eq. 'n_hrs') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(hour)'
     :              ,g_n_hrs)
 
      else if (variable_name .eq. 'radn_net') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(W/m2)'
     :              ,g_radn_net)
 
      else if (variable_name .eq. 'da') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(kg/kg)'
     :              , g_da)
 
      else if (variable_name .eq. 'ra') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(s/m)'
     :              ,g_ra)
 
      else if (variable_name .eq. 'rc') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(s/m)'
     :              ,g_rc)
 
      else if (variable_name .eq. 'epsilon') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(kg/kg/K)'
     :              ,g_epsilon)
 
      else if (variable_name .eq. 'eo_daylength') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(hrs)'
     :              ,g_n_hrs)
 
      else
         call Message_unused ()
 
      endif
 
      call pop_routine (myname)
      return
      end



*================================================================
      subroutine Eo_process ()
*================================================================
      implicit none
      include 'error.pub'                         

*+  Purpose
*      perform actions for current day

*+  Changes
*       210995 jngh programmed

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_process')

*- Implementation Section ----------------------------------
 
      call push_routine (myname)
 
      call pop_routine (myname)
      return
      end

*     ================================================================
      subroutine Eo_set_my_variable (Variable_name)
*     ================================================================
      implicit none
      include   'Eo.inc'           ! Eo common block
      include 'engine.pub'                        
      include 'intrface.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      character  Variable_name*(*)     ! (INPUT) Variable name to search for

*+  Purpose
*     Set one of our variables altered by some other module

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) my_name
      parameter (my_name = 'Eo_set_my_variable')

*+  Local Variables
      integer   numvals                ! number of values returned

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (Variable_name .eq. 'eo_plant'
     :     .and. p_eo_plant_method .ne. 'null') then
         !                        ---
         call collect_real_var (
     :                variable_name    ! array name
     :             ,  '(mm)'           ! units
     :             ,  g_eo_pm_plant            ! array
     :             ,  numvals          ! number of elements returned
     :             ,  0.0              ! lower limit for bounds checking
     :             ,  100.0)            ! upper limit for bounds checking
      !print*, ' eo_plant set to ', g_eo_pm_plant
 
      else
            ! Don't know this variable name
         call Message_unused ()
      endif
 
      call pop_routine (my_name)
      return
      end

*     ===========================================================
      subroutine eo_ritchie (eo)
*     ===========================================================
      implicit none
      include   'eo.inc'
      include 'science.pub'
      include 'error.pub'
 
*+  Sub-Program Arguments
      real       eo                    ! (output) potential evapotranspiration
 
*+  Purpose
*       calculate potential evapotranspiration via ritchie
 
*+  Mission Statement
*       Calculate potential evapotranspiration using ritchie method
 
*+  Changes
*        210191   specified and programmed jngh (j hargreaves
*        290591   jngh removed max_layer.con - cr87
*        051191   jngh updated documentation
*        151292   jngh changed common blocks
*        290393   jngh changed to use lai factor
*        110195   jngh changed to use green cover instead of lai
 
*+  Calls
      real       eo_eeq_fac       ! function
 
*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'eo_ritchie')
 
*+  Local Variables
      real       albedo                ! albedo taking into account plant
                                       !    material
      real       eeq                   ! equilibrium evaporation rate (mm)
      real       wt_ave_temp           ! weighted mean temperature for the
                                       !    day (oC)
 
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
*  ******* calculate potential evaporation from soil surface (eos) ******
 
                ! find equilibrium evap rate as a
                ! function of radiation, albedo, and temp.
 
      albedo = p_max_albedo
     :       - (p_max_albedo - p_albedo) * (1.0 - g_cover_green)
 
                ! wt_ave_temp is mean temp, weighted towards max.
 
      wt_ave_temp = 0.60*g_maxt + 0.40*g_mint
 
      eeq = g_radn*23.8846* (0.000204 - 0.000183*albedo)
     :    * (wt_ave_temp + 29.0)
 
                ! find potential evapotranspiration (eo)
                ! from equilibrium evap rate
 
      eo = eeq*eo_eeq_fac ()
 
      call pop_routine (my_name)
      return
      end
 
 
 
*     ===========================================================
      real function eo_eeq_fac ()
*     ===========================================================
      implicit none
      include   'eo.inc'
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
      parameter (my_name = 'eo_eeq_fac')
 
*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (g_maxt.gt.35.0) then
 
                ! at very high max temps eo/eeq increases
                ! beyond its normal value of 1.1
 
         eo_eeq_fac =  ((g_maxt - 35.0) *0.05 + 1.1)
      else if (g_maxt.lt.5.0) then
 
                ! at very low max temperatures eo/eeq
                ! decreases below its normal value of 1.1
                ! note that there is a discontinuity at tmax = 5
                ! it would be better at tmax = 6.1, or change the
                ! .18 to .188 or change the 20 to 21.1
 
         eo_eeq_fac = 0.01*exp (0.18* (g_maxt + 20.0))
      else
 
                ! temperature is in the normal range, eo/eeq = 1.1
 
         eo_eeq_fac = 1.1
      endif
 
      call pop_routine (my_name)
      return
      end
 

