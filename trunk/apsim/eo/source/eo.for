*===========================================================
      character*(*) function Eo_version ()
*===========================================================

*   Short description:
*             return version number of Eo module

*   Assumptions:
*       none

*   Notes:
*       none

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <=20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       include
*                       implicit none

*   Changes:
*       210995 jngh programmed

*   Calls:
*       pop_routine
*       push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
*       none

*   Internal variables
*       none

*   Constant values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'Eo_version')

      character  version_number*(*)    ! version number of module
      parameter (version_number = 'V0.04 29/10/96')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (myname)

      Eo_version = version_number

      call pop_routine (myname)
      return
      end

*====================================================================
      subroutine apsim_Eo (Action, Data_string)
*====================================================================

*   Short description:
*      This routine is the interface between the main system and the
*      Eo module.

*   Assumptions:
*       none

*   Notes:
*       none

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <=20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       include
*                       implicit none

*   Changes:
*       210995 jngh programmed
*       090696 jngh changed presence report to standard

*   Calls:
*       Eo_zero_variables
*       Eo_zero_daily_variables
*       Eo_init
*       Eo_get_other_variables
*       Eo_prepare
*       Eo_process
*       Eo_set_other_variables
*       Eo_send_my_variable
*       get_current_module
*       message_unused
*       pop_routine
*       push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   subroutine arguments
      character  Action*(*)            ! Message action to perform
      character  Data_string*(*)       ! Message data

*   Global variables
      include   'const.inc'            ! Global constant definitions
      include   'Eo.inc'               ! Eo common block

      character  Eo_version*20         ! function

*   Internal variables
      character  module_name*8         ! name of this module

*   Constant values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'apsim_Eo')

*   Initial data values
*      None

* --------------------- Executable code section ----------------------
      call push_routine (myname)

      if (Action.eq.MES_Presence) then
         call get_current_module (Module_name)
         write(*, *) 'Module_name = ', Module_name
     :              // ', Version : ' // Eo_version()

      elseif (Action.eq.MES_Init) then
         call Eo_zero_variables ()
         call Eo_init ()

      elseif (Action.eq.MES_Prepare) then
         call Eo_prepare ()

      elseif (Action.eq.MES_Get_variable) then
         call Eo_send_my_variable (Data_string)

      elseif (Action.eq.MES_Process) then
         call Eo_zero_daily_variables ()
         call Eo_get_other_variables ()
         call Eo_process ()

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

*   Short description:
*     Set all variables in this module to zero.

*   Assumptions:
*      None

*   Notes:
*       none

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <=20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       include
*                       implicit none

*   Changes:
*       210995 jngh programmed

*   Calls:
*       pop_routine
*       push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*      none

*   Global variables
      include   'const.inc'            ! Global constant definitions
      include   'Eo.inc'               ! Eo common block

*   Internal variables
*      none

*   Constant values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'Eo_zero_variables')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------
      call push_routine (myname)

         !variables for penman-monteith

      p_e_method   = blank 
      g_vpd_source = blank
      g_day_of_year = 0
      g_year      = 0
      g_wind_ms   = 0.0
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
      g_latitude       = 0.0     
      
      
      call Eo_zero_daily_variables ()

      call pop_routine (myname)

      return
      end
*====================================================================
      subroutine Eo_zero_daily_variables ()
*====================================================================

*   Short description:
*     Set all variables in this module to zero.

*   Assumptions:
*      None

*   Notes:
*       none

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <=20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       include
*                       implicit none

*   Changes:
*       210995 jngh programmed

*   Calls:
*       pop_routine
*       push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*      none

*   Global variables
      include    'Eo.inc'              ! Eo common block

*   Internal variables
*      none

*   Constant values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'Eo_zero_daily_variables')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------
      call push_routine (myname)


      call pop_routine (myname)

      return
      end
*====================================================================
      subroutine Eo_init ()
*====================================================================

*   Short description:
*      Initialise Eo module

*   Assumptions:
*      None

*   Notes:
*       none

*   Procedure attributes:
*      Version:         Any hardwfare/Fortran77
*      Extensions:      Long names <=20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       include
*                       implicit none

*   Changes:
*       210995 jngh programmed

*   calls:
*       Eo_version
*       Eo_read_param 
*       Eo_read_constants 
*       Eo_get_other_var_ini
*       report_event
*       pop_routine
*       push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*      none

*   Global variables
      character  Eo_version*15         ! function

*   Internal variables
      character  Event_string*40       ! String to output

*   Constant values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'Eo_init')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------
      call push_routine (myname)

         ! notify system that we have initialised

      Event_string = 'Initialising Version : ' // Eo_version ()
      call report_event (Event_string)

         ! get all parameters from parameter file

      call Eo_read_param ()

         ! get all constants from constants file

      call Eo_read_constants ()

         ! get other variables needed for initialisation

      call Eo_get_other_var_ini ()

      call pop_routine (myname)

      return
      end
*===========================================================
      subroutine Eo_read_param ()
*===========================================================

*   Short description:
*       Read all module parameters.

*   Assumptions:
*       none

*   Notes:
*       none

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <=20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       include
*                       implicit none

*   Changes:
*       210995 jngh programmed

*   Calls:
*       pop_routine
*       push_routine
*       read_char_var
*       read_real_var

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'const.inc'            ! Global constant definitions
      include   'Eo.inc'               ! Eo common block

*   Internal variables
      integer    numvals               ! number of values read
      character  line*80               ! output string

*   Constant values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Eo_read_param')

      character section_name*(*)
      parameter (section_name = 'parameters')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      call push_routine (myname)

      call write_string (lu_scr_sum
     :          ,new_line//'   - Reading Eo Parameters')
     
         ! e_method
        call read_char_var (
     :           section_name   
     :          ,'e_method'     
     :          ,'()'           
     :          ,p_e_method       
     :          ,numvals)       

         ! albedo
      call read_real_var (
     :           section_name   
     :          ,'albedo'       
     :          ,'(-)'          
     :          ,p_albedo       
     :          ,numvals        
     :          ,0.0            
     :          ,1.0)           

         ! Z0soil
      call read_real_var (
     :           section_name   
     :          ,'z0soil'       
     :          ,'(m)'          
     :          ,p_z0soil     
     :          ,numvals        
     :          ,0.0            
     :          ,1.0)           

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
     :          ,500000.0)           

         ! now report out what we have read in

      call write_string (lu_scr_sum, new_line//new_line)

      line = '                 Eo Parameters'
      call write_string (lu_scr_sum, line)

      line =
     :  '     ---------------------------------------------------------'
      call write_string (lu_scr_sum, line)

      line =
     :'      Albedo   Z0soil   Dflt_Wind  Dflt_Pa Dflt_instrum_ht'
     ://' E_method'
      call write_string (lu_scr_sum, line)

      line =
     :'       (-)       (m)    (km/day)   (hpa)      (mm)'
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
     :            , p_e_method
      call write_string (lu_scr_sum, line)

      line =
     :  '     ---------------------------------------------------------'
      call write_string (lu_scr_sum, line)
      call write_string (lu_scr_sum, new_line//new_line)

      call pop_routine (myname)
      return
      end

*===========================================================
      subroutine Eo_read_constants ()
*===========================================================

*   Short description:
*       Read all module constants.

*   Assumptions:
*       none

*   Notes:
*       none

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <=20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       include
*                       implicit none

*   Changes:
*       210995 jngh programmed

*   Calls:
*       pop_routine
*       push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'const.inc'            ! Global constant definitions
      include   'Eo.inc'               ! Eo common block

*   Internal variables
      integer    numvals               ! number of values read

*   Constant values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Eo_read_constants')

      character section_name*(*)
      parameter (section_name = 'constants')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      call push_routine (myname)

      call pop_routine  (myname)
      return
      end

*====================================================================
      subroutine Eo_get_other_var_ini ()
*====================================================================

*   Short description:
*      Get the values of variables from other modules

*   Assumptions:
*      None

*   Notes:
*       none

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <=20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       include
*                       implicit none

*   Changes:
*       210995 jngh programmed

*   Calls:
*       get_real_var
*       get_real_var_optional
*       pop_routine
*       push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   subroutine arguments
*      none

*   Global variables
      include   'const.inc'            ! Global constant definitions
      include   'Eo.inc'               ! Eo common block

*   Internal variables
      integer    numvals               ! number of values returned
      character  string*80             ! temporary string
      real       pa                    ! atmospheric pressure (mb)
      real       wind                  ! wind speed (km/day) 

*   Constant values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'Eo_get_other_variables_ini')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------
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

*   short description:
*     perform calculations before the current timestep.

*   assumptions:
*      none

*   notes:
*     none

*   procedure attributes:
*      version:         any hardware/fortran77
*      extensions:      long names <=20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                             implicit   none

*   changes:
*       210995 jngh programmed

*   Calls:
*       Eo_pen_mon
*       pop_routine
*       push_routine

* ----------------------- declaration section ------------------------

      implicit   none

*   Subroutine arguments
*      none

*   global variables
*      none

*   internal variables
*      none

*   constant values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_prepare')

*   initial data values
*      none

* --------------------- executable code section ----------------------

      call push_routine (myname)

      call pop_routine (myname)
      return
      end
*====================================================================
      subroutine Eo_get_other_variables ()
*====================================================================

*   Short description:
*      Get the values of variables from other modules

*   Assumptions:
*      None

*   Notes:
*       none

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <=20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       include
*                       implicit none

*   Changes:
*       210995 jngh programmed

*   Calls:
*       get_real_var
*       get_real_var_optional
*       get_integer_var
*       pop_routine
*       push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*      none

*   Global variables
      include   'const.inc'            ! Constant definitions
      include   'convert.inc'          ! conversion constants
      include   'Eo.inc'               ! Eo common block

*   Internal variables
      real       canopy_height         ! height of canopy (mm)              
      integer    numvals               ! number of values returned
      real       wind                  ! wind (km/day)

*   Constant values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'Eo_get_other_variables')

      character  fr_intc_radn_name*(*) ! name of fr_intc_radn variable
      parameter (fr_intc_radn_name = 'fr_intc_radn_')
      
      integer    fr_intc_radn_name_length ! length of name
      parameter (fr_intc_radn_name_length = 13)

*   Internal variables - second round
      character  temp_variable_name*(fr_intc_radn_name_length) 
                                       ! temporary storage of first part of
                                       !  variable name
*   Initial data values
*      none

* --------------------- Executable code section ----------------------
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
     :     ,1900         
     :     ,2020)        

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
      else
            ! lai returned ok
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
         g_wind_ms = p_default_wind*km2m/(day2hr*hr2s)
      else
         ! wind returned ok
         g_wind_ms = wind*km2m/(day2hr*hr2s)
      endif     


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

*   short description:
*     calculate the potential evapotranspiration Eo by penman-monteith

*   assumptions:
*      none

*   notes:
*       none

*   procedure attributes:
*      version:         any hardware/fortran77
*      extensions:      long names <=20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       implicit none

*   changes:
*       210995 jngh programmed

*   Calls:
*       Eo_radiation 
*       Eo_da 
*       Eo_aerodynamic
*       Eo_epsilon 
*       Eo_penman_monteith
*       pop_routine
*       push_routine

* ----------------------- declaration section ------------------------

      implicit none

*   Subroutine arguments
*     none

*   global variables
      include   'Eo.inc'               ! Eo common block

      real       day_length            ! function

*   internal variables
*      none

*   constant values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'Eo_pen_mon')

*   initial data values
*      none

* --------------------- executable code section ----------------------

      call push_routine (myname)

         !num_hrs calcuates the maximum number of hours of bright sunlight
         ! recordable
      g_n_hrs = day_length (g_day_of_year, g_latitude, -0.83)

         !  calculate net radiation
         
      call Eo_radiation (g_radn_net)

         ! assume that soil heat flux is 0.1 of the net radiation

      g_fg = 0.1 * g_radn_net

         ! get specific humidity deficit
         
      call Eo_da (g_da)

         ! calculate aeordynamic resistance
         
      call Eo_aerodynamic (g_ra)

         ! we assume that the canopy resistance is zero

      g_rc = 0.0

         ! calculate epsilon
      call Eo_epsilon (g_epsilon)

         ! finally calculate the penman-monteith eo
      call Eo_penman_monteith (g_Eo_pm)

      call pop_routine (myname)
      return
      end
*====================================================================
      subroutine Eo_radiation (radn_net)
*====================================================================

*   Short description:
*     calculate net radiation 

*   Assumptions:
*      None

*   Notes:
*       none

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <=20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       include
*                       implicit none

*   Changes:
*       210995 jngh programmed

*   Calls:
*       divide
*       pop_routine
*       push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       radn_net              ! (OUTPUT) net inward radiant flux 
                                       ! density (W/m2)

*   Global variables
      include   'const.inc'            ! Global constant definitions
      include   'convert.inc'          ! conversion constants
      include   'Eo.inc'               ! Eo common block

      real       divide                ! function
      
*   Internal variables
      real       ave_temp              ! average daily temp (oC)
      real       ea_mb                 ! vapour pressure (mb)
      real       long_wave_in          ! net incoming long wave radiation (W/m2)
      real       intc_radn             ! intercepted radiation by canopy (W/m2)
      real       emiss_sky             ! clear sky emissivity
                  
*   Constant values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'Eo_radiation')

!      real       c_cloud
!      parameter (c_cloud = 0.1)

!      real       emmis_canopy
!      parameter (emmis_canopy = 0.96)

      real       stef_boltz            ! Stefan-Boltzmann constant
      parameter (stef_boltz = 5.67e-8) ! (W/m2/K4)

*   Initial data values
*      None

* --------------------- Executable code section ----------------------

      call push_routine (myname)

         ! convert to W/m2
      intc_radn = g_radn*g_fr_intc_radn * divide(1e6
     :                                  , g_n_hrs*hr2s, -10.0) 
     
      if (intc_radn.lt.0)then
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

      radn_net = (1.0 - p_albedo) * intc_radn + long_wave_in

      call pop_routine (myname)
      return
      end
*====================================================================
      subroutine Eo_vp (vapour_pressure, temperature)
*====================================================================

*   Short description:
*     calculate the vapour pressure at a given temperature

*   Assumptions:
*      None

*   Notes:
*       none

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <=20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       include
*                       implicit none

*   Changes:
*       210995 jngh programmed

*   calls:
*       pop_routine
*       push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       vapour_pressure       ! (OUTPUT) vapour pressure (mb)
      real       temperature           ! (INPUT) temperature (oC)

*   Global variables
      include   'Eo.inc'               ! Eo common block

*   Internal variables
*      none

*   Constant values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_vp')
      
*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      call push_routine (myname)

         ! vapour pressure in millibars
      vapour_pressure = A*exp (B*temperature/(temperature + C))

      call pop_routine (myname)
      return
      end
*====================================================================
      subroutine Eo_esat (esat)
*====================================================================

*   Short description:
*     calculate the saturated vapour pressure

*   Assumptions:
*      None

*   Notes:
*       none

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <=20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       include
*                       implicit none

*   Changes:
*       210995 jngh programmed

*   calls:
*       pop_routine
*       push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       esat                  ! (OUTPUT) saturated vapour pressure
                                       ! (mb)

*   Global variables
      include   'Eo.inc'               ! Eo common block

*   Internal variables
      real       ave_temp              ! average daily temp (oC)

*   Constant values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_esat')
      
*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      call push_routine (myname)

      ave_temp = (g_maxt + g_mint) * 0.5

         ! saturated vapour in millibars
      call Eo_vp (esat, ave_temp)

      call pop_routine (myname)
      return
      end
*====================================================================
      subroutine Eo_da (da)
*====================================================================

*   Short description:
*     calculate the specific humidity deficit (kg/kg)

*   Assumptions:
*      None

*   Notes:
*       none

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <=20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       include
*                       implicit none

*   Changes:
*       210995 jngh programmed

*   calls:
*       eo_esat
*       pop_routine
*       push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       da                    ! (OUTPUT) specific humidity deficit
                                       ! (kg/ha)

*   Global variables
      include   'const.inc'            ! Global constant definitions
      include   'Eo.inc'               ! Eo common block

*   Internal variables
      real       ea_mb                 ! vapour pressure (mb)
      real       q                     ! specific humidity (kg/kg)
      real       esat                  ! saturated vapour pressure (mb)
      real       qsat                  ! sat specific humidity  (kg/kg)

*   Constant values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_da')
      
*   Initial data values
*      none

* --------------------- Executable code section ----------------------

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
         call Eo_esat (esat)      
   
            !and in kg/kg
         qsat = molef*esat/g_pa

            ! vapour pressure in millibars
         call Eo_vp (ea_mb, g_mint)
         q = molef*ea_mb/g_pa
         da = qsat - q
     
      else

         call fatal_error (err_user, 
     :        ' Insufficient data to derive specific humidity deficit')

      endif
      
      call pop_routine (myname)
      return
      end
*====================================================================
      subroutine Eo_aerodynamic (ra)
*====================================================================

*   Short description:
*     calculate the aerodynamic resistance

*   Assumptions:
*      None

*   Notes:
*       none

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <=20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       include
*                       implicit none

*   Changes:
*       210995 jngh programmed

*   calls:
*       pop_routine
*       push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       ra                    ! (OUTPUT) aerodynamic resistance ()

*   Global variables
      include   'Eo.inc'               ! Eo common block
      include   'convert.inc'
      
      real       divide                ! function
      real       u_bound               ! function

*   Internal variable
      real       usuhl                 ! ? (?)
      real       usuh                  ! ? (?)
      real       dh                    ! ? (?)
      real       xx                    ! ? (?)
      real       disp                  ! ? (?)
      real       psih                  ! ? (?)
      real       z0h                   ! ? (?)
      real       z0                    ! roughness length ()

*   Constant values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_aerodynamic')

      real       von_k                 !von Karmen's constant
      parameter (von_k = 0.41)

      real       cr                    ! element drag coefficient
      parameter (cr    = 0.3)          

      real       cs                    ! substrate drag coefficient
      parameter (cs    = 0.003)        

      real       ccd                   ! constant in d/h equation
      parameter (ccd   = 15.0)         

      real       ccw                   ! ccw=(zw-d)/(h-d)
      parameter (ccw   = 2.0)          

      real       usuhm                 ! (max of us/uh)
      parameter (usuhm = 0.3)          
c     *           usuhm = 1.0,          ! (max of us/uh)

*   Initial data values
*      None

* --------------------- Executable code section ----------------------

      call push_routine (myname)

         !now the displacement height, d, roughness for momentum,
         !z0, and roughness for heat, z0he.  use these to calculate
         !aerodynamic resistance, ra.  all from raupach.

      if (g_wind_ms.gt.0.0) then
      
         if (g_canopy_height.gt.0.0 .and. g_lai .gt. 0.0) then
               ! we have some vegetative cover

               ! NOTE: this doesn't seem to behave well for low lai
               
               ! find uh/us
            usuhl = sqrt (cs + cr*g_lai)
            usuh  = u_bound (usuhl, usuhm)
      
               ! find d/h and d
               ! when lai < 0.5076, dh becomes -ve
            xx = sqrt (ccd * g_lai)
            dh = 1.0 - divide (1.0 - exp (-xx), xx, 0.0)
            disp  = dh * g_canopy_height*mm2m
      
               ! find z0h and z0:
               ! Note: when usuh < usuhm, z0h curve becomes quite different.
            psih = log (ccw) - 1.0 + 1.0/ccw
            z0h = (1.0 - dh) * exp (psih - divide (von_k, usuh, 1.0e20))
            z0 = z0h * g_canopy_height*mm2m
      
   !            z0he = z0/5.0
         else
               ! soil is bare
            disp = 0.0
            z0 = p_z0soil
     
         endif
   
         ! calculate ratot (from d to za). No stability corrections at 
         ! this stage 
   
!            ra  = log ((d_za - disp) /z0)*log ((za - disp) /z0he) / ((von_k**2)*ua)   
         ra  = log ((g_instrum_height*mm2m - disp) /z0)**2 
     :       / ((von_k**2)*g_wind_ms)
         
      else
         ra = 1.0e20
      endif   

      call pop_routine (myname)
      return
      end
*====================================================================
      real function Eo_lambda ()
*====================================================================

*   Short description:
*     calculate the lambda (latent heat of vapourisation for water)(kg/m3)

*   Assumptions:
*      None

*   Notes:
*       none

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <=20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       include
*                       implicit none

*   Changes:
*       210995 jngh programmed

*   calls:
*       pop_routine
*       push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*      None

*   Global variables
      include   'Eo.inc'               ! Eo common block

*   Internal variables
      real       ave_temp              ! average daily temp (oC)

*   Constant values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_lambda')

*   Initial data values
*      None

* --------------------- Executable code section ----------------------

      call push_routine (myname)

         ! temperature functions.  lambda is the slope
         
      ave_temp = (g_maxt + g_mint) * 0.5
      Eo_lambda = (2501.0 - 2.38*ave_temp)*1000.0      ! kg/m3

      call pop_routine (myname)
      return
      end
*====================================================================
      subroutine Eo_epsilon (epsilon)
*====================================================================

*   Short description:
*     calculate the non-dimensioal epsilon
*     d(sat spec humidity)/dT ((kg/kg)/K) FROM TETEN FORMULA

*   Assumptions:
*      None

*   Notes:
*       none

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <=20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       include
*                       implicit none

*   Changes:
*       210995 jngh programmed

*   calls:
*       Eo_esat
*       Eo_lambda
*       pop_routine
*       push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments               
      real       epsilon               ! (OUTPUT) non-dimensioal epsilon

*   Global variables
      include   'Eo.inc'               ! Eo common block
      
      real       Eo_lambda             ! function

*   Internal variables
      real       ave_temp              ! average daily temp (oC)
      real       desdt                 ! d(sat VP)/dT: (mb/K)
      real       dqsdt                 ! d(sat spec hum)/dT: (kg/kg)/K
      real       esat                  ! saturated vapour pressure (mb)

*   Constant values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_epsilon')

      real       capp                  ! Specific heat of air at constant pressure 
      parameter (capp = 1004.0)        ! (J/kg/K)
      
*   Initial data values
*      None

* --------------------- Executable code section ----------------------

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
      subroutine Eo_penman_monteith (pen_mon)
*====================================================================

*   Short description:
*     calculate the Penman-Monteith evaporation rate

*   Assumptions:
*      None

*   Notes:
*       none

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <=20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       include
*                       implicit none

*   Changes:
*       210995 jngh programmed

*   calls:
*       pop_routine
*       push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      real       pen_mon               ! (OUTPUT) evaporation rate ()

*   Global variables
      include   'convert.inc'          ! conversion constants
      include   'Eo.inc'               ! Eo common block
      
      real       divide                ! function
      real       Eo_lambda             ! function

*   Internal variables
      real       ave_temp              ! average daily temp (oC)
      real       density_air           ! dry air density (kg/m3)
      real       fe                    ! latent heat flux (W/m2)

*   Constant values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_penman_monteith')

      real       r_gas                 ! universal gas constant (J/mol/K)
      parameter (r_gas = 8.3143)        

*   Initial data values
*      None

* --------------------- Executable code section ----------------------

      call push_routine (myname)

      ave_temp = (g_maxt + g_mint) * 0.5
      density_air = mwair*g_pa*100.0 / ((ave_temp + abs_temp)* r_gas)

         !and now raupach's version of penman-monteith

      fe = (g_epsilon * (g_radn_net - g_fg) 
     :    + divide (density_air * Eo_lambda () * g_da, g_ra, 1.0e20))
     :    / (g_epsilon + divide (g_rc, g_ra, 1.0e20) + 1.0)

      pen_mon = fe/Eo_lambda ()*g_n_hrs*hr2s   ! to convert back to a daily basis

      call pop_routine (myname)
      return
      end
*====================================================================
      subroutine Eo_send_my_variable (Variable_name)
*====================================================================

*   Short description:
*            return the value of one of our variables to       caller

*   Assumptions:
*      None

*   Notes:
*       none

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <=20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       include
*                       implicit none

*   Changes:
*       210995 jngh programmed

*   calls:
*       respond2get_real_var
*       pop_routine
*       push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      character  Variable_name*(*)     ! (INPUT) Variable name to search for

*   Global variables
      include   'Eo.inc'               ! Eo common block

*   Internal variables
*      none

*   Constant values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'Eo_send_my_variable')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------
      call push_routine (myname)

         ! pen_mon penman-monteith evaporation
      if (variable_name .eq. 'eo_pm') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(mm)'         
     :              ,g_Eo_pm)
     
      else if (variable_name .eq. 'g_canopy_height') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(mm)'         
     :              ,g_canopy_height)
     
      else
         call Message_unused ()

      endif         

      call pop_routine (myname)
      return
      end
*================================================================
      subroutine Eo_process ()
*================================================================

*   short description:
*      perform actions for current day

*   assumptions:
*      none

*   notes:
*       none

*   procedure attributes:
*      version:         any hardware/fortran77
*      extensions:      long names <=20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                             implicit   none

*   changes:
*       210995 jngh programmed

*   calls:
*       pop_routine
*       push_routine

* ----------------------- declaration section ------------------------

      implicit   none

*   Subroutine arguments
*      none

*   global variables
*      None

*   internal variables
*      None

*   constant values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_process')

*   initial data values
*      None

* --------------------- executable code section ----------------------

      call push_routine (myname)

      call Eo_pen_mon ()

      call pop_routine (myname)
      return
      end


