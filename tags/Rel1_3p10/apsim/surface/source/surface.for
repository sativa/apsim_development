*     ===========================================================
      character*(*) function surface_version ()
*     ===========================================================

*   Short description:
*       return version number of surface module

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

*   Calls:
*     pop_routine
*     push_routine

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
      parameter (my_name = 'surface_version')

      character  version_number*(*)    ! version number of module
      parameter (version_number = 'V0.0  150197')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      surface_version = version_number

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine APSIM_surface (Action, Data_String)
*     ===========================================================

*   Short description:
*      This routine is the interface between the main system and the
*      surface module.  The communications and responses for this module
*      are as follows.  The module will get Surface seal state variables
*      at the start of the APSwim calculations for the current APSim
*      timestep.  Just prior to the attempt at an APSwim timestep solution
*      this module resets the surface seal value to the value this module
*      wishes APSwim to use.  This will override any internally calculated
*      value of surface conductance.

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

*   Calls:
*   none

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      character  Action*(*)            ! Message action to perform
      character  Data_String*(*)       ! Message data

*   Global variables
      include   'const.inc'            ! Global constant definitions
      include   'surface.inc'         ! surface common block

      character  surface_version*20   ! function
      integer    lastnb                ! function

*   Internal variables
      character  Module_name*10        ! name of module

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'surface')

*   Initial data values
*      None

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

         ! initialise error flags
      call set_warning_off ()

      if (Action.eq.MES_Presence) then
         Call Get_Current_Module (Module_Name)
         write (*, *) 'module_name = '
     :              , module_name(:lastnb (module_name))
     :              // blank
     :              // surface_version ()

      else if (Action.eq.MES_Init) then
         call surface_zero_variables ()
         call surface_Init ()

c      else if (Action.eq.MES_Inter_Timestep) then
c         call surface_Inter_Timestep()

c      else if (Action.eq.MES_Process) then
c         call surface_get_other_variables ()
c         call surface_process ()

c      else if ((Action.eq.'surface').or.(Action.eq.'apply')) then
c         call surface_get_other_variables ()
c         call surface_surface ()

      else if (Action.eq.MES_Get_variable) then
         call surface_Send_my_variable (Data_String)

      else if (Action.eq.'swim_timestep_preparation') then
         call surface_timestep_preparation ()

      else if (Action.eq.'pre_swim_timestep') then
         call surface_calc_scon ()

      else if (Action .eq. MES_Set_variable) then
         call surface_set_my_variable (Data_String)

c      else if (Action .eq. 'post_swim_timestep') then


c      else if (Action .eq. 'tillage') then
c         call surface_tillage ()

      else
            ! Don't use message
         call Message_unused ()

      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine surface_Init ()
*     ===========================================================

*   Short description:
*      Initialise surface module

*   Assumptions:
*      None

*   Notes:
*       none

*   Procedure attributes:
*      Version:         Any hardwfare/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:

*   Calls:
*     surface_version
*     surface_get_other_variables
*     report_event

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*      none

*   Global variables
      include   'const.inc'            ! Constant definitions
      include   'surface.inc'         ! surface model common

      character  surface_version*15   ! function

*   Internal variables
      character  Event_string*79       ! String to output

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'surface_init')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      call surface_get_other_variables ()

         ! Notify system that we have initialised

      Event_string = ' Initialising, Version : ' // surface_version ()
      call report_event (Event_string)

         ! Get all parameters from parameter file

      call surface_read_param ()


      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine surface_read_param ()
*     ===========================================================

*   Short description:
*      Read in all parameters from parameter file.

*   Assumptions:
*      None

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

*   Calls:
*      Get_param
*      Open_param_file

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*      none

*   Global variables
      include   'const.inc'
      include   'surface.inc'         ! surface model common block

*   Internal variables
      integer    numvals               ! number of values returned

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'surface_read_param')

      character  section_name*(*)
      parameter (section_name = 'parameters')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      call write_string (lu_scr_sum
     :                 ,new_line//'   - Reading Parameters')

      call read_integer_var (
     :           section_name         ! Section header
     :         , 'model_no'           ! Keyword
     :         , '()'                 ! Units
     :         , p_model_no           ! Variable
     :         , numvals              ! Number of values returned
     :         , 1                    ! Lower Limit for bound checking
     :         , 2)                   ! Upper Limit for bound checking

      call read_double_var (
     :           section_name         ! Section header
     :         , 'precip_const'       ! Keyword
     :         , '(mm)'               ! Units
     :         , p_precip_const       ! Variable
     :         , numvals              ! Number of values returned
     :         , 0d0                  ! Lower Limit for bound checking
     :         , 1000d0)              ! Upper Limit for bound checking

      call read_double_var (
     :           section_name         ! Section header
     :         , 'effpar'             ! Keyword
     :         , '()'                 ! Units
     :         , p_effpar             ! Variable
     :         , numvals              ! Number of values returned
     :         , 0d0                  ! Lower Limit for bound checking
     :         , 10d0)                ! Upper Limit for bound checking

      call read_double_var (
     :           section_name         ! Section header
     :         , 'seal_decay_rate'    ! Keyword
     :         , '(/MJ/m2)'           ! Units
     :         , p_seal_decay_rate    ! Variable
     :         , numvals              ! Number of values returned
     :         , 0d0                  ! Lower Limit for bound checking
     :         , 1d0)                 ! Upper Limit for bound checking

      call read_double_var (
     :           section_name         ! Section header
     :         , 'rr_decay_rate'    ! Keyword
     :         , '(/MJ/m2)'           ! Units
     :         , p_rr_decay_rate    ! Variable
     :         , numvals              ! Number of values returned
     :         , 0d0                  ! Lower Limit for bound checking
     :         , 1d0)                 ! Upper Limit for bound checking

      call read_double_var (
     :           section_name         ! Section header
     :         , 'rr_max'             ! Keyword
     :         , '(/MJ/m2)'           ! Units
     :         , p_rr_max             ! Variable
     :         , numvals              ! Number of values returned
     :         , 0d0                  ! Lower Limit for bound checking
     :         , 3.99d0)              ! Upper Limit for bound checking

      call read_double_var (
     :           section_name         ! Section header
     :         , 'rr_min'             ! Keyword
     :         , '(/MJ/m2)'           ! Units
     :         , p_rr_min             ! Variable
     :         , numvals              ! Number of values returned
     :         , 0d0                  ! Lower Limit for bound checking
     :         , 3.99d0)              ! Upper Limit for bound checking

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine surface_zero_variables ()
*     ===========================================================

*   Short description:
*     Set all variables in this module to zero.

*   Assumptions:
*      None

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

*   Calls:
*       none

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*      none

*   Global variables
      include   'surface.inc'         ! surface common block

*   Internal variables
*     none

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'surface_zero_variables')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      g_year = 0
      g_day = 0
      g_cover = 0d0
      g_RR = 0d0
      p_RR_max = 0d0
      p_RR_min = 0d0
      g_Scon = 0d0
      g_Scon_max = 0d0
      g_Scon_min = 0d0
      p_model_no = 0
      p_effpar = 0d0
      p_precip_const = 0d0
      p_seal_decay_rate = 0d0
      p_RR_decay_rate = 0d0
      
      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine surface_get_other_variables ()
*     ===========================================================

*   Short description:
*      Get the values of variables from other modules

*   Assumptions:
*      None

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

*   Calls:
*     get_variable_value

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*      none

*   Global variables
      include   'const.inc'            ! Constant definitions
      include   'surface.inc'         ! surface common block

*   Internal variables
      integer    numvals               ! number of values returned

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'surface_get_other_variables')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      call Get_integer_var (
     :      unknown_module  ! Module that responds (Not Used)
     :    , 'year'          ! Variable Name
     :    , '()'            ! Units                (Not Used)
     :    , g_year          ! Variable
     :    , numvals         ! Number of values returned
     :    , 1800            ! Lower Limit for bound checking
     :    , 2000)           ! Upper Limit for bound checking

      call Get_integer_var (
     :      unknown_module  ! Module that responds (Not Used)
     :    , 'day'           ! Variable Name
     :    , '()'            ! Units                (Not Used)
     :    , g_day           ! Variable
     :    , numvals         ! Number of values returned
     :    , 0               ! Lower Limit for bound checking
     :    , 366)            ! Upper Limit for bound checking

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine surface_Send_my_variable (Variable_name)
*     ===========================================================

*   Short description:
*      Return the value of one of our variables to caller

*   Assumptions:
*      None

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
*      011195 jngh  added call to message_unused

*   Calls:
*     message_unused

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      character  Variable_name*(*)     ! (INPUT) Variable name to search for

*   Global variables
      include   'surface.inc'         ! surface Common block

*   Internal variables


*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'surface_send_my_variable')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      if (Variable_name .eq. 'rr') then
         call respond2get_double_var (
     :                              variable_name
     :                            , '(cm)'
     :                            , g_RR)
      else
         call Message_unused ()
      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine surface_set_my_variable (Variable_name)
*     ===========================================================

*   Short description:
*     Set one of our variables altered by some other module

*   Assumptions:
*      None

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
*      011195 jngh  added call to message_unused
*      060695 jngh changed respond2set to collect routines

*   Calls:
*     message_unused

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      character  Variable_name*(*)     ! (INPUT) Variable name to search for

*   Global variables
      include   'const.inc'
      include   'surface.inc'         ! surface common block

*   Internal variables
      integer    numvals               ! number of values returned

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'surface_set_my_variable')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

c      if (Variable_name .eq. 'scon') then
c         call collect_double_var (
c     :                variable_name        ! variable name
c     :              , '(/h)'               ! units
c     :              , g_Scon               ! array
c     :              , numvals              ! number of elements returned
c     :              ,0d0
c     :              ,1000d0)
c      else
            ! Don't know this variable name
         call Message_unused ()
c      endif

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine surface_Calc_Scon ()
*     ===========================================================

*   Short description:
*      Return the value of one of our variables to caller

*   Assumptions:
*      None

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
*      011195 jngh  added call to message_unused

*   Calls:
*     message_unused

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*     none

*   Global variables
      include   'const.inc'
      include   'surface.inc'         ! surface Common block

*   Internal variables
      double precision Scon
      double precision rainfall
      double precision duration
      integer          numvals

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'surface_calc_scon')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      call Get_double_var (
     :      'apswim'        ! Module that responds (Not Used)
     :    , 'dr'            ! Variable Name
     :    , '(mm)'          ! Units                (Not Used)
     :    , rainfall        ! Variable
     :    , numvals         ! Number of values returned
     :    , 0d0             ! Lower Limit for bound checking
     :    , 1000d0)         ! Upper Limit for bound checking

      call Get_double_var (
     :      'apswim'        ! Module that responds (Not Used)
     :    , 'dt'            ! Variable Name
     :    , '(min)'         ! Units                (Not Used)
     :    , duration        ! Variable
     :    , numvals         ! Number of values returned
     :    , 0d0             ! Lower Limit for bound checking
     :    , 1440d0)         ! Upper Limit for bound checking

      if (p_model_no .eq. 1) then
         call surface_Scon_calc1(rainfall,duration,Scon)
      elseif (p_model_no .eq. 2) then
         call surface_Scon_calc2(rainfall,duration,Scon)
      else
      endif


      call new_postbox()

      call Post_double_var (
     :                      'scon'
     :                    , '(/h)'
     :                    , Scon)

      call message_send_immediate(
     :                            'apswim'
     :                           ,MES_Set_variable
     :                           ,'scon'
     :                           )

      call delete_postbox ()

      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine surface_get_swim_variables ()
*     ===========================================================

*   Short description:
*      Get the values of surface seal variables from apswim

*   Assumptions:
*      None

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

*   Calls:
*     get_variable_value

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*      none

*   Global variables
      include   'const.inc'            ! Constant definitions
      include   'surface.inc'         ! surface common block

*   Internal variables
      integer    numvals               ! number of values returned
      double precision residue_cover   ! residue cover (0-1)
      
*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'surface_get_swim_variables')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      call Get_double_var (
     :      'apswim'        ! Module that responds (Not Used)
     :    , 'scon'          ! Variable Name
     :    , '(/h)'          ! Units                (Not Used)
     :    , g_scon          ! Variable
     :    , numvals         ! Number of values returned
     :    , 0d0             ! Lower Limit for bound checking
     :    , 1000d0)         ! Upper Limit for bound checking

      call Get_double_var (
     :      'apswim'        ! Module that responds (Not Used)
     :    , 'scon_min'      ! Variable Name
     :    , '(/h)'          ! Units                (Not Used)
     :    , g_scon_min      ! Variable
     :    , numvals         ! Number of values returned
     :    , 0d0             ! Lower Limit for bound checking
     :    , 1000d0)         ! Upper Limit for bound checking

      call Get_double_var (
     :      'apswim'        ! Module that responds (Not Used)
     :    , 'scon_max'      ! Variable Name
     :    , '(/h)'          ! Units                (Not Used)
     :    , g_scon_max      ! Variable
     :    , numvals         ! Number of values returned
     :    , 0d0             ! Lower Limit for bound checking
     :    , 1000d0)         ! Upper Limit for bound checking

      call Get_double_var (
     :      'apswim'        ! Module that responds (Not Used)
     :    , 'crop_cover'    ! Variable Name
     :    , '(0-1)'         ! Units                (Not Used)
     :    , g_cover         ! Variable
     :    , numvals         ! Number of values returned
     :    , 0d0             ! Lower Limit for bound checking
     :    , 1d0)            ! Upper Limit for bound checking

      ! When apswim starts to use residue information we should
      ! use its value of total cover.

      residue_cover = 0d0
      call Get_double_var_optional (
     :      unknown_module  ! Module that responds (Not Used)
     :    , 'residue_cover' ! Variable Name
     :    , '(0-1)'         ! Units                (Not Used)
     :    , residue_cover   ! Variable
     :    , numvals         ! Number of values returned
     :    , 0d0             ! Lower Limit for bound checking
     :    , 1d0)            ! Upper Limit for bound checking
      

      ! add residue to cover variable
      g_cover = g_cover + residue_cover * (1d0 - g_cover)

      call pop_routine (my_name)
      return
      end
* =====================================================================
      subroutine surface_scon_calc1(rainfall,duration,Scon)
* =====================================================================
*     Short Description:
*     Update the surface conductance given some rainfall.
*     Ideally, if timesteps are small we could just use
*     dScon/dEnergy = -1/k x (SCon - Scom_min)
*     but because this is really just a linear approximation of the
*     curve for longer timesteps we had better be explicit and
*     calculate the difference from the exponential decay curve.
*     In order to calculate our position on the decay curve we
*     back-calculate the energy required to acheive the current
*     decayed state.  We do this because APSwim "owns" the seal
*     and it may have it reset at any time via tillage etc.

*   Assumptions:
*      None

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

*   Calls:
*     none

* ----------------------- Declaration section ------------------------

      implicit none

*     Global Variables
      include 'surface.inc'

*     Subroutine Arguments
      double precision duration
      double precision rainfall
      double precision Scon

*     Internal Variables
      double precision decay_fraction
      double precision Eo
      double precision Es
      double precision dEs
      double precision avinten

*     Constant Values
*     none
*

* --------------------- Executable code section ----------------------

      ! first calculate the amount of Energy that must have been
      ! applied to reach the current conductance.

      decay_Fraction = (g_scon-g_scon_min)/(g_scon_max-g_scon_min)
      Es = -p_precip_const * log(decay_Fraction)


      ! now add rainfall energy for this timestep

      if (rainfall .gt. 0.d0) then

         avinten = rainfall/duration

         Eo = (1d0+p_effpar*log(avinten/(25d0/60d0)))
         dEs = Eo*rainfall

      else
         dEs = 0.d0
      endif

      Es = Es + dEs

      ! now calculate new surface storage from new energy
      Scon = g_Scon_min
     :       + (g_Scon_max-g_Scon_min)*exp(-Es/p_precip_const)


      return
      end
* =====================================================================
      subroutine surface_scon_calc2(rainfall,duration,Scon)
* =====================================================================
*     Short Description:
*     Update the surface conductance using the approach of Silburn
*     and Connolly, (Journal of Hydrology 172 (1995) 87-104).
*     Ideally, if timesteps are small we could just use
*     dScon/dEnergy = -1/k x (SCon - Scom_min)
*     but because this is really just a linear approximation of the
*     curve for longer timesteps we had better be explicit and
*     calculate the difference from the exponential decay curve.
*     In order to calculate our position on the decay curve we
*     back-calculate the energy required to acheive the current
*     decayed state.  We do this because APSwim "owns" the seal
*     and it may have it reset at any time via tillage etc.
*     Similarly from APSwim's state for surface sealing we can
*     deduce the state for random roughness, as they are both
*     related to cumulative rainfall energy.
*     Things to note:
*     1) Really all numbers in equations should be double precision
*        (e.g. 1d0 not 1.0) to avoid mixed mode arithmetic.
*     2) In the current coding the decay of RR is autoregressive in
*        that the ernergy term for RR decay is effected by RR.
*     3) The error within Eqn 4 from the above-mentioned paper
*        has been corrected in this implementation.
*     4) The implementation of cumulative energy effect is different
*        to the algorithm found in the source code used in the
*        above-mentioned paper.  In the previous source code the
*        seal was decayed each day using all rainfall up to that
*        point in time instead of the rainfall for just that time period.

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

*   Calls:
*     none

* ----------------------- Declaration section ------------------------

      implicit none

*     Global Variables
      include 'surface.inc'

*     Subroutine Arguments
      double precision duration  !(INPUT) Duration of timestep
      double precision rainfall  !(INPUT) Rainfall for timestep
      double precision Scon      !(OUTPUT) Surface conductance (/h)

*     Internal Variables
      double precision decay_fraction
      double precision Eo
      double precision Es
      double precision dEs
      double precision avinten

*     Constant Values
*     none
*

* --------------------- Executable code section ----------------------

      ! first calculate the amount of Energy that must have been
      ! applied to reach the current conductance.

      decay_Fraction = (g_scon-g_scon_min)/(g_scon_max-g_scon_min)
      Es = -1.0/p_seal_decay_rate * log(decay_Fraction)

      !  Calculate the random roughness that would exist after
      !  this amount of rainfall energy.
      g_RR = p_RR_min
     :       + (p_RR_max-p_RR_min)*exp(-p_RR_decay_rate*Es)


      ! Now calculate the rainfal energy for this SWIM timestep.

      if (rainfall .gt. 0.d0) then

         avinten = rainfall/(duration/60d0)

cnh note that the following equation from Rosewell is nonsensical for
cnh low rainfall intensities.  A better option may be to use the
cnh approach used internally in swim.  Tests show that a precipitation
cnh constant of 0.27 gives the same response for most rainfall intensities
cnh but is more sensible at low rainfall intensities.
         Eo  = 26.35 * (1.0 - 0.669*exp(-0.0349*avinten))

         dEs = (1.0 - g_cover)*(1.0-g_RR/4.0)*Eo*rainfall

      else
         dEs = 0.d0
      endif

      ! now add rainfall energy for this timestep
      Es = Es + dEs

      ! now calculate new surface storage from new energy
      Scon = g_Scon_min
     :       + (g_Scon_max-g_Scon_min)*exp(-p_seal_decay_rate*Es)

      return
      end
* ====================================================================
       subroutine surface_timestep_preparation ()
* ====================================================================

*   Short description:
*      APSwim broadcasts a message just prior to performing calculations
*      to prepare for taking an attempt at a timestep.  We use this
*      opportunity to get state variables just prior to calculations
*      based upon swim's proposed timestep.  In this case we obtain
*      the state of the surface seal before swim changes these states
*      based upon the proposed timestep.  The values obtained at this
*      point will not have any changes based upon apswim calculations.
*      We record the state here and then perform our own calculations
*      at the pre_timestep stage of swim calculations.

*   Assumptions:
*      None

*   Notes:
*      None

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*     31-01-1997 - huth - Programmed and Specified

*   Calls:
*     Pop_routine
*     Push_routine

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
*      none

*   Global variables
*      none

*   Internal variables
*      none

*   Constant values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'surface_timestep_preparation')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------
      call push_routine (myname)

      call surface_get_swim_variables ()

      call pop_routine (myname)
      return
      end
