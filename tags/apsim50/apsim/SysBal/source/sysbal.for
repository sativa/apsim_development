      module SysBalModule
      use Registrations
! ====================================================================
!     sysbal constants
! ====================================================================

!   Short description:
!      sysbal module constants

!   Notes:
!      none

!   Attributes:
!      Version:         Any hardware/Fortran77
!      Extensions:      Long names <= 20 chars.
!                       Lowercase
!                       Underscore
!                       Inline comments

!   Changes:
!      201093 jngh programmed

! ----------------------- Declaration section ------------------------

!   Constant values
      integer    max_layer             ! Maximum number of layers in soil
      parameter (max_layer = 100)

      integer    max_modules                    ! maximum number of modules in at once
      parameter (max_modules = 20)

      integer    module_name_size             ! maximum length of module name
      parameter (module_name_size = 32)

      real       fraction_C_FOM             ! Fraction Cin FOM
      parameter (fraction_C_FOM = 0.4)


      type SysBalGlobals
         sequence
         integer    sysbal_index(max_modules) ! index to sorted sysbal height ()
         real       height(max_modules)       ! sysbal height of modules (mm)
         integer    num_modules               ! number of modules ()
         integer  module_name(max_modules)        ! list of modules replying

         real Nloss_system
         real Ngain_system
         real Ndlt_system
         real Ndlt_surface
         real Ndlt_crop
         real Ndlt_soil
         real Nerror_system
         real Ncum_error_system
         real Nstate_system_yest
         real Nstate_surface_yest
         real Nstate_crop_yest
         real Nstate_soil_yest
         real Ploss_system
         real Pgain_system
         real Pdlt_system
         real Pdlt_surface
         real Pdlt_crop
         real Pdlt_soil
         real Perror_system
         real Pcum_error_system
         real Pstate_system_yest
         real Pstate_surface_yest
         real Pstate_crop_yest
         real Pstate_soil_yest
         real Closs_system
         real Cgain_system
         real Cdlt_system
         real Cdlt_surface
         real Cdlt_crop
         real Cdlt_soil
         real Cerror_system
         real Ccum_error_system
         real Cstate_system_yest
         real Cstate_surface_yest
         real Cstate_crop_yest
         real Cstate_soil_yest
         real DMloss_system
         real DMgain_system
         real DMdlt_system
         real DMdlt_surface
         real DMdlt_crop
         real DMdlt_soil
         real DMerror_system
         real DMcum_error_system
         real DMstate_system_yest
         real DMstate_surface_yest
         real DMstate_crop_yest
         real DMstate_soil_yest
         real SWloss_system
         real SWgain_system
         real SWdlt_system
         real SWdlt_surface
         real SWdlt_crop
         real SWdlt_soil
         real SWerror_system
         real SWcum_error_system
         real SWstate_system_yest
         real SWstate_surface_yest
         real SWstate_crop_yest
         real SWstate_soil_yest


      end type SysBalGlobals
! ====================================================================
      ! instance variables.
      common /InstancePointers/ ID,g,p,c
      save InstancePointers
      type (SysBalGlobals),pointer :: g
      type (IDsType),pointer :: id

      contains


*     ===========================================================
      subroutine sysbal_init ()
*     ===========================================================

      Use Infrastructure
      implicit none

*+  Purpose
*      Initialise sysbal module. Output mesage and get list from control file.

*+  Changes
*     201093 jngh specified and programmed
*     210395 jngh changed from unknown_section to a defined section
*     280999 sdb removed version reference


*+  Constant Values
      character  my_name*(*)           ! procedure name
      parameter (my_name  = 'sysbal_init')
*
      character  section_name*(*)      ! name of parameter section
      parameter (section_name = 'parameters')

*+  Local Variables
      integer    num_modules           ! number of module names in list
      character  line*200              ! message
      integer    i                     ! loop counter

*- Implementation Section ----------------------------------

      call push_routine (my_name)

            ! initialisation message

!      call Write_string (' Initialising')
!
!            ! now get  list from control file
!
!      call read_char_array_optional (section_name
!     :                   , 'intermodule', max_modules, '()'
!     :                   , g%intermodule_list, num_modules)
!
!      call bound_check_integer_var (num_modules, 0, max_modules
!     :                            , 'num_modules')
!
!         ! now report initial conditions
!
!      if (num_modules.gt.1) then
!         write (line, '(a)')  ' Module rotation for intermoduleping :'
!         call write_string (line)
!
!         write (line, '(100a)')  (g%intermodule_list(i), i=1, num_modules)
!         call write_string (line)
!
!      else
!         ! no swapping required
!         write (line,'(a)')
!     :             ' No module rotation for intermoduleping'
!         call write_string (line)
!      endif

      call pop_routine (my_name)
      return
      end subroutine



* ====================================================================
      subroutine sysbal_find_modules ()
* ====================================================================

      Use Infrastructure
      implicit none

*+  Purpose
*      Find what modules are in system

*+  Changes
*     090896 jngh - Programmed and Specified
*     261196 jngh lengthened module_type to 100 from 20 and set it blank before us

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'sysbal_find_modules')

*+  Local Variables
      integer    module                  ! index for modules
      character  module_type*100         ! type of module
      integer    numvals               ! number of values in string
      integer owner_module             ! owner module of variable

*- Implementation Section ----------------------------------
      call push_routine (myname)


      module = 0
      module_type = blank
1000  continue

         call get_char_vars(
     :             module + 1
     :           , 'module_type'
     :           , '()'
     :           , module_type
     :           , numvals)

         if (numvals.ne.0) then
            if (module+1.le.max_modules) then
               module = module + 1
               Owner_module = get_posting_Module ()

               g%module_name(module) = owner_module
               goto 1000
            else
               call fatal_error (err_user
     :            , 'Too many modules with module type.')
            endif
         else
         endif

      g%num_modules = module

      call pop_routine (myname)
      return
      end subroutine



*     ===========================================================
      subroutine sysbal_zero_all_variables ()
*     ===========================================================

      Use Infrastructure
      implicit none

*+  Purpose
*     Set all variables in this module to zero.

*+  Changes
*      201093 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! procedure name
      parameter (my_name  = 'sysbal_zero_all_variables')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      g%sysbal_index = 0
      g%height       = 0.0
      g%num_modules = 0
      g%module_name    = 0

      g%Nloss_system         = 0.0
      g%Ngain_system         = 0.0
      g%Ndlt_system          = 0.0
      g%Ndlt_surface         = 0.0
      g%Ndlt_crop          = 0.0
      g%Ndlt_soil            = 0.0
      g%Nerror_system        = 0.0
      g%Ncum_error_system    = 0.0
      g%Nstate_system_yest   = 0.0
      g%Nstate_surface_yest  = 0.0
      g%Nstate_crop_yest   = 0.0
      g%Nstate_soil_yest     = 0.0
      g%Ploss_system         = 0.0
      g%Pgain_system         = 0.0
      g%Pdlt_system          = 0.0
      g%Pdlt_surface         = 0.0
      g%Pdlt_crop          = 0.0
      g%Pdlt_soil            = 0.0
      g%Perror_system        = 0.0
      g%Pcum_error_system    = 0.0
      g%Pstate_system_yest   = 0.0
      g%Pstate_surface_yest  = 0.0
      g%Pstate_crop_yest   = 0.0
      g%Pstate_soil_yest     = 0.0
      g%Closs_system         = 0.0
      g%Cgain_system         = 0.0
      g%Cdlt_system          = 0.0
      g%Cdlt_surface         = 0.0
      g%Cdlt_crop          = 0.0
      g%Cdlt_soil            = 0.0
      g%Cerror_system        = 0.0
      g%Ccum_error_system    = 0.0
      g%Cstate_system_yest   = 0.0
      g%Cstate_surface_yest  = 0.0
      g%Cstate_crop_yest   = 0.0
      g%Cstate_soil_yest     = 0.0
      g%DMloss_system        = 0.0
      g%DMgain_system        = 0.0
      g%DMdlt_system         = 0.0
      g%DMdlt_surface        = 0.0
      g%DMdlt_crop         = 0.0
      g%DMdlt_soil           = 0.0
      g%DMerror_system       = 0.0
      g%DMcum_error_system   = 0.0
      g%DMstate_system_yest  = 0.0
      g%DMstate_surface_yest = 0.0
      g%DMstate_crop_yest  = 0.0
      g%DMstate_soil_yest    = 0.0
      g%SWloss_system        = 0.0
      g%SWgain_system        = 0.0
      g%SWdlt_system         = 0.0
      g%SWdlt_surface        = 0.0
      g%SWdlt_crop         = 0.0
      g%SWdlt_soil           = 0.0
      g%SWerror_system       = 0.0
      g%SWcum_error_system   = 0.0
      g%SWstate_system_yest  = 0.0
      g%SWstate_surface_yest = 0.0
      g%SWstate_crop_yest  = 0.0
      g%SWstate_soil_yest    = 0.0



      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine sysbal_zero_variables ()
*     ===========================================================

      Use Infrastructure
      implicit none

*+  Purpose
*     Set all variables in this module to zero.

*+  Changes
*      201093 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! procedure name
      parameter (my_name  = 'sysbal_zero_variables')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call fill_integer_array (g%sysbal_index, 0, max_modules)
      call fill_real_array (g%height, 0.0, max_modules)

      g%num_modules = 0

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sysbal_get_N_variables (
     :                                    state_crop
     :                                  , state_soil
     :                                  , state_surface
     :                                  , loss_soil
     :                                  , loss_crop
     :                                  , loss_surface
     :                                  , gain_soil
     :                                  , gain_crop
     :                                  , gain_surface
     :                                  )
*     ===========================================================

      Use Infrastructure
      implicit none

      real       state_crop
      real       state_soil
      real       state_surface
      real       loss_soil
      real       loss_crop
      real       loss_surface
      real       gain_soil
      real       gain_crop
      real       gain_surface

*+  Purpose
*      Get the values of variables from other modules

*+  Changes
*      201093 jngh specified and programmed
*      261196 jngh tested incoming cover for 1. Set log to 100.0 if it is.

*+  Constant Values
*
      character  my_name*(*)           ! procedure name
      parameter (my_name='sysbal_get_N_variables')

      character  mm*10
      parameter (mm = '(mm)')

      character  gm2*10
      parameter (gm2 = '(g/m2)')

      character  kgha*10
      parameter (kgha = '(kg/ha)')

      character  n_green*30
      parameter (n_green = 'n_green()')

      character  n_senesced*30
      parameter (n_senesced = 'n_senesced()')

      character  n_dead*30
      parameter (n_dead = 'n_dead()')

      character  nit_tot*30
      parameter (nit_tot = 'nit_tot()')

      character  surfaceom_n*30
      parameter (surfaceom_n = 'surfaceom_n')

      character  dlt_no3_dnit*30
      parameter (dlt_no3_dnit = 'dlt_no3_dnit()')

      character  leach_NO3*30
      parameter (leach_NO3 = 'leach_no3')

      character  leach_NH4*30
      parameter (leach_NH4 = 'leach_nh4')

      character  dlt_n_fixed*30
      parameter (dlt_n_fixed = 'dlt_n_fixed')

      character  dlayer*30
      parameter (dlayer = 'dlayer')

*+  Local Variables

*- Implementation Section ----------------------------------

      call push_routine (my_name)

  ! ensure crop variables are registered with fortran modules (e.g. cropmod)
  !-------------------------------------------------------------------------
      call sysbal_reg_variable ('sorghum', n_green, gm2)
      call sysbal_reg_variable ('sorghum', n_senesced, gm2)
      call sysbal_reg_variable ('sorghum', n_dead, gm2)
      call sysbal_reg_variable ('sorghum', dlt_n_fixed, gm2)

      call sysbal_reg_variable ('maize', n_green, gm2)
      call sysbal_reg_variable ('maize', n_senesced, gm2)
      call sysbal_reg_variable ('maize', n_dead, gm2)
      call sysbal_reg_variable ('maize', dlt_n_fixed, gm2)

      call sysbal_reg_variable ('sunflower', n_green, gm2)
      call sysbal_reg_variable ('sunflower', n_senesced, gm2)
      call sysbal_reg_variable ('sunflower', n_dead, gm2)
      call sysbal_reg_variable ('sunflower', dlt_n_fixed, gm2)

  ! define system states
  !---------------

      state_crop =
     :      sysbal_get_variable(n_green, gm2) * gm2kg/sm2ha
     :    + sysbal_get_variable(n_senesced, gm2) * gm2kg/sm2ha
     :    + sysbal_get_variable(n_dead, gm2) * gm2kg/sm2ha

      state_soil = sysbal_get_variable(nit_tot, kgha)
      state_surface = sysbal_get_variable(surfaceom_n, kgha)

  ! define system losses
  !--------------
      loss_soil = sysbal_get_variable(dlt_no3_dnit, kgha)
     :          + sysbal_get_variable(leach_NO3, kgha)
     :          + sysbal_get_variable(leach_NH4, kgha)

      loss_crop    = 0.0
      loss_surface = 0.0

  ! define system gains
  !-------------
      gain_soil    = 0.0
      gain_crop    = sysbal_get_variable(dlt_n_fixed, gm2) * gm2kg/sm2ha
      gain_surface = 0.0

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine sysbal_get_P_variables (
     :                                    state_crop
     :                                  , state_soil
     :                                  , state_surface
     :                                  , loss_soil
     :                                  , loss_crop
     :                                  , loss_surface
     :                                  , gain_soil
     :                                  , gain_crop
     :                                  , gain_surface
     :                                  )
*     ===========================================================

      Use Infrastructure
      implicit none

      real       state_crop
      real       state_soil
      real       state_surface
      real       loss_soil
      real       loss_crop
      real       loss_surface
      real       gain_soil
      real       gain_crop
      real       gain_surface

*+  Purpose
*      Get the values of variables from other crops

*+  Changes
*      201093 jngh specified and programmed
*      261196 jngh tested incoming cover for 1. Set log to 100.0 if it is.

*+  Constant Values
*
      character  my_name*(*)           ! procedure name
      parameter (my_name='sysbal_get_P_variables')

      character  gm2*10
      parameter (gm2 = '(g/m2)')

      character  kgha*10
      parameter (kgha = '(kg/ha)')

      character  p_green*30
      parameter (p_green = 'p_green()')

      character  p_senesced*30
      parameter (p_senesced = 'p_senesced()')

      character  p_dead*30
      parameter (p_dead = 'p_dead()')

      character  fom_p*30
      parameter (fom_p = 'fom_p()')

      character  hum_p*30
      parameter (hum_p = 'hum_p()')

      character  biom_p*30
      parameter (biom_p = 'biom_p()')

      character  rock_p*30
      parameter (rock_p = 'rock_p()')

      character  banded_p*30
      parameter (banded_p = 'banded_p()')

      character  unavail_p*30
      parameter (unavail_p = 'unavail_p()')

      character  labile_p*30
      parameter (labile_p = 'labile_p()')

      character  surfaceom_p*30
      parameter (surfaceom_p = 'surfaceom_p')

*+  Local Variables

*- Implementation Section ----------------------------------

      call push_routine (my_name)

  ! ensure crop variables are registered with fortran modules (e.g. cropmod)
  !-------------------------------------------------------------------------
      call sysbal_reg_variable ('sorghum', p_green, gm2)
      call sysbal_reg_variable ('sorghum', p_senesced, gm2)
      call sysbal_reg_variable ('sorghum', p_dead, gm2)

      call sysbal_reg_variable ('maize', p_green, gm2)
      call sysbal_reg_variable ('maize', p_senesced, gm2)
      call sysbal_reg_variable ('maize', p_dead, gm2)

      call sysbal_reg_variable ('sunflower', p_green, gm2)
      call sysbal_reg_variable ('sunflower', p_senesced, gm2)
      call sysbal_reg_variable ('sunflower', p_dead, gm2)

  ! define system states
  !---------------

      state_crop =
     :      sysbal_get_variable(p_green, gm2) * gm2kg/sm2ha
     :    + sysbal_get_variable(p_senesced, gm2) * gm2kg/sm2ha
     :    + sysbal_get_variable(p_dead, gm2) * gm2kg/sm2ha

      state_soil = sysbal_get_variable(fom_p, kgha)
     :           + sysbal_get_variable(hum_p, kgha)
     :           + sysbal_get_variable(biom_p, kgha)
     :           + sysbal_get_variable(rock_p, kgha)
     :           + sysbal_get_variable(banded_p, kgha)
     :           + sysbal_get_variable(unavail_p, kgha)
     :           + sysbal_get_variable(labile_p, kgha)
      state_surface = sysbal_get_variable(surfaceom_p, kgha)

  ! define system losses
  !--------------
      loss_soil = 0.0
      loss_crop    = 0.0
      loss_surface = 0.0

  ! define system gains
  !-------------
      gain_soil    = 0.0
      gain_crop    = 0.0
      gain_surface = 0.0

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine sysbal_get_C_variables (
     :                                    state_crop
     :                                  , state_soil
     :                                  , state_surface
     :                                  , loss_soil
     :                                  , loss_crop
     :                                  , loss_surface
     :                                  , gain_soil
     :                                  , gain_crop
     :                                  , gain_surface
     :                                  )
*     ===========================================================

      Use Infrastructure
      implicit none

      real       state_crop
      real       state_soil
      real       state_surface
      real       loss_soil
      real       loss_crop
      real       loss_surface
      real       gain_soil
      real       gain_crop
      real       gain_surface

*+  Purpose
*      Get the values of variables from other modules

*+  Changes
*      201093 jngh specified and programmed
*      261196 jngh tested incoming cover for 1. Set log to 100.0 if it is.

*+  Constant Values
*
      character  my_name*(*)           ! procedure name
      parameter (my_name='sysbal_get_C_variables')

      character  gm2*10
      parameter (gm2 = '(g/m2)')

      character  kgha*10
      parameter (kgha = '(kg/ha)')

      character  dm_green*30
      parameter (dm_green = 'dm_green()')

      character  dm_senesced*30
      parameter (dm_senesced = 'dm_senesced()')

      character  dm_dead*30
      parameter (dm_dead = 'dm_dead()')

      character  carbon_tot*30
      parameter (carbon_tot = 'carbon_tot()')

      character  surfaceom_c*30
      parameter (surfaceom_c = 'surfaceom_c')

      character  dlt_fom_c_atm*30
      parameter (dlt_fom_c_atm = 'dlt_fom_c_atm()')

      character  dlt_hum_c_atm*30
      parameter (dlt_hum_c_atm = 'dlt_hum_c_atm()')

      character  dlt_biom_c_atm*30
      parameter (dlt_biom_c_atm = 'dlt_biom_c_atm()')

      character  dlt_dm_oil_conv*30
      parameter (dlt_dm_oil_conv = 'dlt_dm_oil_conv')

      character  dlt_dm_oil_conv_retrans*30
      parameter (dlt_dm_oil_conv_retrans = 'dlt_dm_oil_conv_retrans')

      character  dlt_res_c_atm*30
      parameter (dlt_res_c_atm = 'dlt_res_c_atm')

      character  dlt_dm_green*30
      parameter (dlt_dm_green = 'dlt_dm_green()')


*+  Local Variables

*- Implementation Section ----------------------------------

      call push_routine (my_name)

  ! ensure crop variables are registered with fortran modules (e.g. cropmod)
  !-------------------------------------------------------------------------
      call sysbal_reg_variable ('sorghum', dm_green, gm2)
      call sysbal_reg_variable ('sorghum', dm_senesced, gm2)
      call sysbal_reg_variable ('sorghum', dm_dead, gm2)
      call sysbal_reg_variable ('sorghum', dlt_dm_oil_conv_retrans, gm2)
      call sysbal_reg_variable ('sorghum', dlt_dm_green, gm2)

      call sysbal_reg_variable ('maize', dm_green, gm2)
      call sysbal_reg_variable ('maize', dm_senesced, gm2)
      call sysbal_reg_variable ('maize', dm_dead, gm2)
      call sysbal_reg_variable ('maize', dlt_dm_oil_conv_retrans, gm2)
      call sysbal_reg_variable ('maize', dlt_dm_green, gm2)

      call sysbal_reg_variable ('sunflower', dm_green, gm2)
      call sysbal_reg_variable ('sunflower', dm_senesced, gm2)
      call sysbal_reg_variable ('sunflower', dm_dead, gm2)
      call sysbal_reg_variable ('sunflower',dlt_dm_oil_conv_retrans,gm2)
      call sysbal_reg_variable ('sunflower', dlt_dm_green, gm2)

  ! define system states
  !---------------

      state_crop = sysbal_get_variable(dm_green, gm2)* gm2kg/sm2ha * 0.4
     :       + sysbal_get_variable(dm_senesced, gm2) * gm2kg/sm2ha * 0.4
     :       + sysbal_get_variable(dm_dead, gm2) * gm2kg/sm2ha * 0.4

      state_soil = sysbal_get_variable(carbon_tot, kgha)
      state_surface = sysbal_get_variable(surfaceom_c, kgha)

  ! define system losses
  !--------------

      loss_soil = sysbal_get_variable(dlt_fom_c_atm, kgha)
     :          + sysbal_get_variable(dlt_hum_c_atm, kgha)
     :          + sysbal_get_variable(dlt_biom_c_atm, kgha)
      loss_crop    =
!     :   sysbal_get_variable(dlt_dm_oil_conv, gm2) * gm2kg/sm2ha * 0.4     ! conversion is not included in the dlt_dm_green
     : + sysbal_get_variable(dlt_dm_oil_conv_retrans, gm2)
     :   * gm2kg/sm2ha * 0.4
      loss_surface = sysbal_get_variable(dlt_res_c_atm, kgha)

  ! define system gains
  !-------------

      gain_soil = 0.0
      gain_crop = sysbal_get_variable(dlt_dm_green, gm2)
     :          * gm2kg/sm2ha * 0.4
      gain_surface = 0.0

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine sysbal_get_DM_variables (
     :                                    state_crop
     :                                  , state_soil
     :                                  , state_surface
     :                                  , loss_soil
     :                                  , loss_crop
     :                                  , loss_surface
     :                                  , gain_soil
     :                                  , gain_crop
     :                                  , gain_surface
     :                                  )
*     ===========================================================

      Use Infrastructure
      implicit none

      real       state_crop
      real       state_soil
      real       state_surface
      real       loss_soil
      real       loss_crop
      real       loss_surface
      real       gain_soil
      real       gain_crop
      real       gain_surface

*+  Purpose
*      Get the values of variables from other modules

*+  Changes
*      201093 jngh specified and programmed
*      261196 jngh tested incoming cover for 1. Set log to 100.0 if it is.

*+  Constant Values
*
      character  my_name*(*)           ! procedure name
      parameter (my_name='sysbal_get_DM_variables')

      character  gm2*10
      parameter (gm2 = '(g/m2)')

      character  kgha*10
      parameter (kgha = '(kg/ha)')

      character  dm_green*30
      parameter (dm_green = 'dm_green()')

      character  dm_senesced*30
      parameter (dm_senesced = 'dm_senesced()')

      character  dm_dead*30
      parameter (dm_dead = 'dm_dead()')

      character  carbon_tot*30
      parameter (carbon_tot = 'carbon_tot()')

      character  surfaceom_wt*30
      parameter (surfaceom_wt = 'surfaceom_wt')

      character  dlt_fom_c_atm*30
      parameter (dlt_fom_c_atm = 'dlt_fom_c_atm()')

      character  dlt_hum_c_atm*30
      parameter (dlt_hum_c_atm = 'dlt_hum_c_atm()')

      character  dlt_biom_c_atm*30
      parameter (dlt_biom_c_atm = 'dlt_biom_c_atm()')

      character  dlt_dm_oil_conv*30
      parameter (dlt_dm_oil_conv = 'dlt_dm_oil_conv')

      character  dlt_dm_oil_conv_retrans*30
      parameter (dlt_dm_oil_conv_retrans = 'dlt_dm_oil_conv_retrans')

      character  dlt_res_c_atm*30
      parameter (dlt_res_c_atm = 'dlt_res_c_atm')

      character  dlt_dm_green*30
      parameter (dlt_dm_green = 'dlt_dm_green()')

*+  Local Variables

*- Implementation Section ----------------------------------

      call push_routine (my_name)

  ! ensure crop variables are registered with fortran modules (e.g. cropmod)
  !-------------------------------------------------------------------------
      call sysbal_reg_variable ('sorghum', dm_green, gm2)
      call sysbal_reg_variable ('sorghum', dm_senesced, gm2)
      call sysbal_reg_variable ('sorghum', dm_dead, gm2)
      call sysbal_reg_variable ('sorghum', dlt_dm_oil_conv_retrans, gm2)
      call sysbal_reg_variable ('sorghum', dlt_dm_green, gm2)

      call sysbal_reg_variable ('maize', dm_green, gm2)
      call sysbal_reg_variable ('maize', dm_senesced, gm2)
      call sysbal_reg_variable ('maize', dm_dead, gm2)
      call sysbal_reg_variable ('maize', dlt_dm_oil_conv_retrans, gm2)
      call sysbal_reg_variable ('maize', dlt_dm_green, gm2)

      call sysbal_reg_variable ('sunflower', dm_green, gm2)
      call sysbal_reg_variable ('sunflower', dm_senesced, gm2)
      call sysbal_reg_variable ('sunflower', dm_dead, gm2)
      call sysbal_reg_variable ('sunflower',dlt_dm_oil_conv_retrans,gm2)
      call sysbal_reg_variable ('sunflower', dlt_dm_green, gm2)

  ! define system states
  !---------------

      state_crop = sysbal_get_variable(dm_green, gm2) * gm2kg/sm2ha
     :           + sysbal_get_variable(dm_senesced, gm2) * gm2kg/sm2ha
     :           + sysbal_get_variable(dm_dead, gm2) * gm2kg/sm2ha

      state_soil = sysbal_get_variable(carbon_tot, kgha) / 0.4
      state_surface = sysbal_get_variable(surfaceom_wt, kgha)

  ! define system losses
  !--------------
      loss_soil = sysbal_get_variable(dlt_fom_c_atm, kgha) / 0.4
     :          + sysbal_get_variable(dlt_hum_c_atm, kgha) / 0.4
     :          + sysbal_get_variable(dlt_biom_c_atm, kgha) / 0.4
      loss_crop    =
!     :   sysbal_get_variable(dlt_dm_oil_conv, gm2) * gm2kg/sm2ha          ! conversion is not included in the dlt_dm_green
     : + sysbal_get_variable(dlt_dm_oil_conv_retrans, gm2) * gm2kg/sm2ha
      loss_surface = sysbal_get_variable(dlt_res_c_atm, kgha) / 0.4

  ! define system gains
  !-------------
      gain_soil = 0.0
      gain_crop = sysbal_get_variable(dlt_dm_green, gm2) * gm2kg/sm2ha
      gain_surface = 0.0

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine sysbal_get_SW_variables (
     :                                    state_crop
     :                                  , state_soil
     :                                  , state_surface
     :                                  , loss_soil
     :                                  , loss_crop
     :                                  , loss_surface
     :                                  , gain_soil
     :                                  , gain_crop
     :                                  , gain_surface
     :                                  )
*     ===========================================================

      Use Infrastructure
      implicit none

      real       state_crop
      real       state_soil
      real       state_surface
      real       loss_soil
      real       loss_crop
      real       loss_surface
      real       gain_soil
      real       gain_crop
      real       gain_surface

*+  Purpose
*      Get the values of variables from other modules

*+  Changes
*      201093 jngh specified and programmed
*      261196 jngh tested incoming cover for 1. Set log to 100.0 if it is.

*+  Constant Values
*
      character  my_name*(*)           ! procedure name
      parameter (my_name='sysbal_get_SW_variables')

      character  mm*10
      parameter (mm = '(mm)')

      character  sw_dep*30
      parameter (sw_dep = 'sw_dep()')

      character  pond*30
      parameter (pond = 'pond')

      character  es*30
      parameter (es = 'es')

      character  drain*30
      parameter (drain = 'drain')

      character  ep*30
      parameter (ep = 'ep')

      character  runoff*30
      parameter (runoff = 'runoff')

      character  rain*30
      parameter (rain = 'rain')

*+  Local Variables

*- Implementation Section ----------------------------------

      call push_routine (my_name)

  ! ensure crop variables are registered with fortran modules (e.g. cropmod)
  !-------------------------------------------------------------------------
      call sysbal_reg_variable ('sorghum', ep, mm)
      call sysbal_reg_variable ('maize', ep, mm)
      call sysbal_reg_variable ('sunflower', ep, mm)

  ! define system states
  !---------------

      state_crop = 0.0
      state_soil = sysbal_get_variable(sw_dep, mm)
      state_surface = sysbal_get_variable(pond, mm)

  ! define system losses
  !--------------
      loss_soil = sysbal_get_variable(es, mm)
     :          + sysbal_get_variable(drain, mm)
      loss_crop    = sysbal_get_variable(ep, mm)
      loss_surface = sysbal_get_variable(runoff, mm)

  ! define system gains
  !-------------
      gain_soil    = 0.0
      gain_crop    = 0.0
      gain_surface = sysbal_get_variable(rain, mm)

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      real function sysbal_get_variable (var_name, units)
*     ===========================================================

      Use Infrastructure
      implicit none

      character var_name*(*)
      character units*(*)

*+  Purpose
*      Get the values of variables from other modules

*+  Changes
*      201093 jngh specified and programmed
*      261196 jngh tested incoming cover for 1. Set log to 100.0 if it is.

*+  Constant Values
      character  my_name*(*)           ! procedure name
      parameter (my_name='sysbal_get_variable')

*+  Local Variables
      integer    module                  ! index for modules
      real       temp                  !
      integer    numvals               ! number of values in string
      integer    owner_module          ! owner module of variable
      real       value
      character  name*30
      character  unit*30
      character  string*400            ! output string

*- Implementation Section ----------------------------------

      call push_routine (my_name)


      module = 0
      value = 0.0
      temp = 0.0
      name = var_name
      unit = units

1000  continue

         call get_real_vars (module+1, name, unit
     :                              , temp, numvals
     :                              , 0.0, 1e6)

         if (numvals.ne.0) then
            if (module+1.le.max_modules) then
               module = module + 1
               Owner_module = get_posting_Module ()
               g%module_name(module) = owner_module
               value = value + temp
               goto 1000
            else
               call fatal_error (err_user
     :            , 'Too many modules with '//trim(var_name))
            endif
         else
         endif

!         if (module .eq. 0) then
!         write (string, *)
!     :                   'No values returned for '//trim(var_name)
!         call Write_string (string)
!
!         else
!         endif

      sysbal_get_variable = value

      call pop_routine (my_name)
      return
      end function

*     ===========================================================
      subroutine sysbal_reg_variable (mod_name, var_name, units)
*     ===========================================================

      Use Infrastructure
      implicit none

      character mod_name*(*)
      character var_name*(*)
      character units*(*)

*+  Purpose
*      Register get variables from other modules

*+  Changes
*      201093 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! procedure name
      parameter (my_name='sysbal_reg_variable')

*+  Local Variables
      real dummy
      integer numvals
      logical ok
      integer modNameID


*- Implementation Section ----------------------------------

      call push_routine (my_name)

      ok = component_name_to_id(Mod_name, modNameID)
      if (ok) then
         call Get_real_var_optional (modNameID, trim(var_name)
     :                             , trim(units), dummy, numvals
     :                             , 0.0, 1.0e6)
      else
      endif


      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      integer function sysbal_get_last_index (var_name, units)
*     ===========================================================

      Use Infrastructure
      implicit none

      character var_name*(*)
      character units*(*)

*+  Purpose
*      Get the last index of an array

*+  Changes
*      201093 jngh specified and programmed
*      261196 jngh tested incoming cover for 1. Set log to 100.0 if it is.

*+  Constant Values
      character  my_name*(*)           ! procedure name
      parameter (my_name='sysbal_get_variable')

*+  Local Variables
      integer    module                  ! index for modules
      real       temp                  !
      integer    numvals               ! number of values in string
      integer    owner_module          ! owner module of variable
      real       array(max_layer)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      array(:) = 0.0
      call get_real_array (unknown_module, var_name, max_layer
     :                                    , units
     :                                    , array, numvals
     :                                    , 0.0, 1e6)


      sysbal_get_last_index = count_of_real_vals (array, max_layer)

      call pop_routine (my_name)
      return
      end function



*     ===========================================================
      subroutine sysbal_send_my_variable (Variable_name)
*     ===========================================================

      Use Infrastructure
      implicit none

*+  Sub-Program Arguments
      character  Variable_name*(*)     ! (INPUT) Variable name to search for

*+  Purpose
*      Return the value of one of our variables to caller

*+  Changes
*      201093 jngh specified and programmed
*      011195 jngh  added call to message_unused
*      010896 jngh changed method of getting module name for gets
*      120996 jngh removed print statement
*      021199 jngh added export of cover_tot_all and cover_height_all arrays

*+  Calls
c      integer    sysbal_module_number    ! function

*+  Constant Values
      character  my_name*(*)           ! procedure name
      parameter (my_name='sysbal_send_my_variable')
*+  Local Variables
      real       cover                 ! temporary cover variable
      integer    module                ! module counter
      character  module_string*(max_module_name_size) ! module name
      real       cover_tot_all(max_modules)   ! total cover of each module (0-1)
      real       cover_green_all(max_modules) ! green cover of each module (0-1)
      integer    moduleID
      integer    numvals
      logical    found

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (variable_name.eq.'n_loss_system') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Nloss_system)

      else if (variable_name.eq.'n_gain_system') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Ngain_system)

      else if (variable_name.eq.'n_dlt_system') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Ndlt_system)

      else if (variable_name.eq.'n_dlt_surface') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :
     :                            , g%Ndlt_surface)

      else if (variable_name.eq.'n_dlt_crop') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Ndlt_crop)

      else if (variable_name.eq.'n_dlt_soil') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Ndlt_soil)

      else if (variable_name.eq.'n_error_system') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Nerror_system)

      else if (variable_name.eq.'n_cum_error_system') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Ncum_error_system)

      else if (variable_name.eq.'n_state_system') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Nstate_system_yest)

      else if (variable_name.eq.'n_state_surface') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Nstate_surface_yest)

      else if (variable_name.eq.'n_state_crop') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Nstate_crop_yest)

      else if (variable_name.eq.'n_state_soil') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Nstate_soil_yest)

      else if (variable_name.eq.'p_loss_system') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Ploss_system)

      else if (variable_name.eq.'p_gain_system') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Pgain_system)

      else if (variable_name.eq.'p_dlt_system') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Pdlt_system)

      else if (variable_name.eq.'p_dlt_surface') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Pdlt_surface)

      else if (variable_name.eq.'p_dlt_crop') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Pdlt_crop)

      else if (variable_name.eq.'p_dlt_soil') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Pdlt_soil)

      else if (variable_name.eq.'p_error_system') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Perror_system)

      else if (variable_name.eq.'p_cum_error_system') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Pcum_error_system)

      else if (variable_name.eq.'p_state_system') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Pstate_system_yest)

      else if (variable_name.eq.'p_state_surface') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Pstate_surface_yest)

      else if (variable_name.eq.'p_state_crop') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Pstate_crop_yest)

      else if (variable_name.eq.'p_state_soil') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Pstate_soil_yest)

      else if (variable_name.eq.'c_loss_system') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Closs_system)

      else if (variable_name.eq.'c_gain_system') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Cgain_system)

      else if (variable_name.eq.'c_dlt_system') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Cdlt_system)

      else if (variable_name.eq.'c_dlt_surface') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Cdlt_surface)

      else if (variable_name.eq.'c_dlt_crop') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Cdlt_crop)

      else if (variable_name.eq.'c_dlt_soil') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Cdlt_soil)

      else if (variable_name.eq.'c_error_system') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Cerror_system)

      else if (variable_name.eq.'c_cum_error_system') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Ccum_error_system)

      else if (variable_name.eq.'c_state_system') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Cstate_system_yest)

      else if (variable_name.eq.'c_state_surface') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Cstate_surface_yest)

      else if (variable_name.eq.'c_state_crop') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Cstate_crop_yest)

      else if (variable_name.eq.'c_state_soil') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Cstate_soil_yest)

      else if (variable_name.eq.'dm_loss_system') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%DMloss_system)

      else if (variable_name.eq.'dm_gain_system') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%DMgain_system)

      else if (variable_name.eq.'dm_dlt_system') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%DMdlt_system)

      else if (variable_name.eq.'dm_dlt_surface') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%DMdlt_surface)

      else if (variable_name.eq.'dm_dlt_crop') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%DMdlt_crop)

      else if (variable_name.eq.'dm_dlt_soil') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%DMdlt_soil)

      else if (variable_name.eq.'dm_error_system') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%DMerror_system)

      else if (variable_name.eq.'dm_cum_error_system') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%DMcum_error_system)

      else if (variable_name.eq.'dm_state_system') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%DMstate_system_yest)

      else if (variable_name.eq.'dm_state_surface') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%DMstate_surface_yest)

      else if (variable_name.eq.'dm_state_crop') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%DMstate_crop_yest)

      else if (variable_name.eq.'dm_state_soil') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%DMstate_soil_yest)

      else if (variable_name.eq.'sw_loss_system') then
         call respond2get_real_var (variable_name, '(mm)'
     :                            , g%SWloss_system)

      else if (variable_name.eq.'sw_gain_system') then
         call respond2get_real_var (variable_name, '(mm)'
     :                            , g%SWgain_system)

      else if (variable_name.eq.'sw_dlt_system') then
         call respond2get_real_var (variable_name, '(mm)'
     :                            , g%SWdlt_system)

      else if (variable_name.eq.'sw_dlt_surface') then
         call respond2get_real_var (variable_name, '(mm)'
     :                            , g%SWdlt_surface)

      else if (variable_name.eq.'sw_dlt_crop') then
         call respond2get_real_var (variable_name, '(mm)'
     :                            , g%SWdlt_crop)

      else if (variable_name.eq.'sw_dlt_soil') then
         call respond2get_real_var (variable_name, '(mm)'
     :                            , g%SWdlt_soil)

      else if (variable_name.eq.'sw_error_system') then
         call respond2get_real_var (variable_name, '(mm)'
     :                            , g%SWerror_system)

      else if (variable_name.eq.'sw_cum_error_system') then
         call respond2get_real_var (variable_name, '(mm)'
     :                            , g%SWcum_error_system)

      else if (variable_name.eq.'sw_state_system') then
         call respond2get_real_var (variable_name, '(mm)'
     :                            , g%SWstate_system_yest)

      else if (variable_name.eq.'sw_state_surface') then
         call respond2get_real_var (variable_name, '(mm)'
     :                            , g%SWstate_surface_yest)

      else if (variable_name.eq.'sw_state_crop') then
         call respond2get_real_var (variable_name, '(mm)'
     :                            , g%SWstate_crop_yest)

      else if (variable_name.eq.'sw_state_soil') then
         call respond2get_real_var (variable_name, '(mm)'
     :                            , g%SWstate_soil_yest)

      else
            ! don't own the variable
         call Message_unused ()
      endif

      call pop_routine (my_name)
      return
      end subroutine



* ====================================================================
       integer function sysbal_module_number (module_id)
* ====================================================================

      Use Infrastructure
      implicit none

*+  Sub-Program Arguments
      integer  module_id         ! (INPUT) id of module to locate

*+  Purpose
*     Return the position of the module_name in module_names array

*+  Changes
*        090896 jngh - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'sysbal_module_number')

*+  Local Variables
      integer    module                  ! module counter
      integer    module_num              ! position of module in array

*- Implementation Section ----------------------------------
      call push_routine (myname)


      do 1000 module = 1, g%num_modules

         if (module_id.eq.g%module_name(module)) then

            module_num = module
            goto 1100
         else
         endif

1000  continue
      module_num = 0

1100  continue

      sysbal_module_number = module_num

      call pop_routine (myname)
      return
      end function



*     ===========================================================
      subroutine sysbal_prepare ()
*     ===========================================================

      Use Infrastructure
      implicit none

*+  Purpose
*     Perform calculations before the current timestep. This is the main
*     processing for the arbitrator

*+  Changes
*      201093 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! procedure name
      parameter (my_name='sysbal_prepare')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

            ! determine modules now

      call sysbal_modules_present (g%sysbal_index, g%num_modules)

      if (g%num_modules.gt.0) then

!               ! get light transmitted through each layer
!
!         call sysbal_top_layer_light (g%top_layer_light)
!
!               ! get light intercepted by each module sysbal
!
!         call sysbal_intc_light (g%intc_light)

      else
            ! no modules present
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sysbal_modules_present (sysbal_index, num_modules)
*     ===========================================================

      Use Infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    sysbal_index(*)       ! (OUTPUT) presence of sysbal and order
      integer    num_modules          ! (OUTPUT) number of modules present

*+  Purpose
*     Determine which modules are present and their order from top down.

*+  Changes
*      201093 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! procedure name
      parameter (my_name='sysbal_modules_present')

*+  Local Variables
      real       temp(max_modules)       ! temporary height array for sorting
      real       temp1(max_modules)      ! temporary height array for counting

*- Implementation Section ----------------------------------

      call push_routine (my_name)

            ! determine modules with modules now

            ! We put the heights into a temporary array as negative numbers,
            ! sort that into ascending order, with a key to their original
            ! position before sortine.  This gives us an index to the
            ! height array in descending order of height.

      call fill_real_array (temp, 0.0, max_modules)
      call subtract_real_array (g%height, temp, max_modules)
      call fill_integer_array (sysbal_index, 0, max_modules)

            ! determine order of modules from top down

      call shell_sort_real (temp, -max_modules, sysbal_index)

      call fill_real_array (temp1, 0.0, max_modules)
      call subtract_real_array (temp, temp1, max_modules)
      num_modules = count_of_real_vals (temp1, max_modules)

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine sysbal_post ()
*     ===========================================================

      Use Infrastructure
      implicit none

*+  Purpose
*     Perform calculations after the current timestep.

*+  Changes
*      201093 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! procedure name
      parameter (my_name='sysbal_post')

*+  Local Variables
      integer    num_in_list           ! number of names in module list
      character  string*400            ! output string

      real       error_threshold
      Parameter (error_threshold = 0.1)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call sysbal_Bal (sysbal_get_N_variables
     :               , g%Nloss_system
     :               , g%Ngain_system
     :               , g%Ndlt_system
     :               , g%Ndlt_surface
     :               , g%Ndlt_crop
     :               , g%Ndlt_soil
     :               , g%Nerror_system
     :               , g%Ncum_error_system
     :               , g%Nstate_system_yest
     :               , g%Nstate_surface_yest
     :               , g%Nstate_crop_yest
     :               , g%Nstate_soil_yest
     :               )

      if (g%Nerror_system .gt. error_threshold*0.1) then
         write (string, *)
     :                   '**** N balance - unaccounted gain (kg/ha) = '
     :                  , g%Nerror_system
         call Write_string (string)

      elseif (g%Nerror_system .lt. -error_threshold*0.1) then
         write (string, *)
     :                   '**** N balance - unaccounted loss (kg/ha) = '
     :                  , g%Nerror_system
         call Write_string (string)

      else
         ! balance is ok
      endif

      call sysbal_Bal (sysbal_get_P_variables
     :               , g%Ploss_system
     :               , g%Pgain_system
     :               , g%Pdlt_system
     :               , g%Pdlt_surface
     :               , g%Pdlt_crop
     :               , g%Pdlt_soil
     :               , g%Perror_system
     :               , g%Pcum_error_system
     :               , g%Pstate_system_yest
     :               , g%Pstate_surface_yest
     :               , g%Pstate_crop_yest
     :               , g%Pstate_soil_yest
     :               )

      if (g%Perror_system .gt. error_threshold*0.1/8.0) then
         write (string, *)
     :                   '**** P balance - unaccounted gain (kg/ha) = '
     :                  , g%Perror_system
         call Write_string (string)

      elseif (g%Perror_system .lt. -error_threshold*0.1/8.0) then
         write (string, *)
     :                   '**** P balance - unaccounted loss (kg/ha) = '
     :                  , g%Perror_system
         call Write_string (string)

      else
         ! balance is ok
      endif


      call sysbal_Bal (sysbal_get_C_variables
     :               , g%Closs_system
     :               , g%Cgain_system
     :               , g%Cdlt_system
     :               , g%Cdlt_surface
     :               , g%Cdlt_crop
     :               , g%Cdlt_soil
     :               , g%Cerror_system
     :               , g%Ccum_error_system
     :               , g%Cstate_system_yest
     :               , g%Cstate_surface_yest
     :               , g%Cstate_crop_yest
     :               , g%Cstate_soil_yest
     :               )

      if (g%Cerror_system .gt. error_threshold) then
         write (string, *)
     :                   '**** C balance - unaccounted gain (kg/ha) = '
     :                  , g%Cerror_system
         call Write_string (string)
         write (string, *)
     :                   '****        equivalent DM as FOM  (kg/ha) = '
     :                  , g%Cerror_system / 0.4
         call Write_string (string)

      elseif (g%Cerror_system .lt. -error_threshold) then
         write (string, *)
     :                   '**** C balance - unaccounted loss (kg/ha) = '
     :                  , g%Cerror_system
         call Write_string (string)
         write (string, *)
     :                   '****        equivalent DM as FOM  (kg/ha) = '
     :                  , g%Cerror_system / 0.4
         call Write_string (string)

      else
         ! balance is ok
      endif


      call sysbal_Bal (sysbal_get_DM_variables
     :               , g%DMloss_system
     :               , g%DMgain_system
     :               , g%DMdlt_system
     :               , g%DMdlt_surface
     :               , g%DMdlt_crop
     :               , g%DMdlt_soil
     :               , g%DMerror_system
     :               , g%DMcum_error_system
     :               , g%DMstate_system_yest
     :               , g%DMstate_surface_yest
     :               , g%DMstate_crop_yest
     :               , g%DMstate_soil_yest
     :               )

!      if (g%DMerror_system .gt. error_threshold) then
!         write (string, *)
!     :                   '**** DM balance - unaccounted gain (kg/ha) = '
!     :                  , g%DMerror_system
!         call Write_string (string)
!
!      elseif (g%DMerror_system .lt. -error_threshold) then
!         write (string, *)
!     :                   '**** DM balance - unaccounted loss (kg/ha) = '
!     :                  , g%DMerror_system
!         call Write_string (string)
!
!      else
!         ! balance is ok
!      endif

      call sysbal_Bal (sysbal_get_SW_variables
     :               , g%SWloss_system
     :               , g%SWgain_system
     :               , g%SWdlt_system
     :               , g%SWdlt_surface
     :               , g%SWdlt_crop
     :               , g%SWdlt_soil
     :               , g%SWerror_system
     :               , g%SWcum_error_system
     :               , g%SWstate_system_yest
     :               , g%SWstate_surface_yest
     :               , g%SWstate_crop_yest
     :               , g%SWstate_soil_yest
     :               )

      if (g%SWerror_system .gt. error_threshold) then
         write (string, *)
     :                   '**** SW balance - unaccounted gain (mm) = '
     :                  , g%SWerror_system
         call Write_string (string)

      elseif (g%SWerror_system .lt. -error_threshold) then
         write (string, *)
     :                   '**** SW balance - unaccounted loss (mm) = '
     :                  , g%SWerror_system
         call Write_string (string)

      else
         ! balance is ok
      endif


      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine sysbal_Bal ( fun
     :                      , loss_system
     :                      , gain_system
     :                      , dlt_system
     :                      , dlt_surface
     :                      , dlt_crop
     :                      , dlt_soil
     :                      , error_system
     :                      , cum_error_system
     :                      , state_system_yest
     :                      , state_surface_yest
     :                      , state_crop_yest
     :                      , state_soil_yest
     :                      )
*     ===========================================================

      Use Infrastructure
      implicit none


!      real fun
      real  loss_system
      real  gain_system

      real  dlt_system
      real  dlt_surface
      real  dlt_crop
      real  dlt_soil

      real  error_system
      real  cum_error_system

      real  state_system_yest
      real  state_surface_yest
      real  state_crop_yest
      real  state_soil_yest

*+  Purpose
*     Perform calculations after the current timestep.

*+  Changes
*      201093 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! procedure name
      parameter (my_name='sysbal_bal')

*+  Local Variables
      real  state_soil
      real  state_surface
      real  state_crop
      real  state_system

      real  loss_soil
      real  loss_surface
      real  loss_crop

      real  gain_soil
      real  gain_surface
      real  gain_crop


*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call fun (
     :          state_crop
     :        , state_soil
     :        , state_surface
     :        , loss_soil
     :        , loss_crop
     :        , loss_surface
     :        , gain_soil
     :        , gain_crop
     :        , gain_surface
     :        )

      loss_system =  loss_soil + loss_crop + loss_surface
      gain_system =  gain_soil + gain_crop + gain_surface

      state_system = state_surface + state_soil + state_crop
      dlt_system = state_system - state_system_yest

      error_system =  dlt_system + loss_system - gain_system

      if (state_system_yest >= 0.00001) then
         cum_error_system = cum_error_system + error_system

      else
         error_system = 0.0
      endif

      dlt_soil    =  state_soil    - state_soil_yest
      dlt_surface =  state_surface - state_surface_yest
      dlt_crop    =  state_crop    - state_crop_yest

      state_system_yest  = state_system
      state_soil_yest    = state_soil
      state_surface_yest = state_surface
      state_crop_yest    = state_crop


      call pop_routine (my_name)
      return
      end subroutine



      end module SysBalModule

!     ===========================================================
      subroutine alloc_dealloc_instance(doAllocate)
!     ===========================================================
      use SysBalModule
      implicit none
      ml_external alloc_dealloc_instance

!+  Sub-Program Arguments
      logical, intent(in) :: doAllocate

!+  Purpose
!      Module instantiation routine.

!- Implementation Section ----------------------------------

      if (doAllocate) then
         allocate(g)
         allocate(id)
      else
         deallocate(g)
         deallocate(id)
      end if
      return
      end subroutine




*     ===========================================================
      subroutine Main (Action, Data_string)
*     ===========================================================

      Use Infrastructure
      use SysBalModule
      implicit none
      ml_external Main

*+  Sub-Program Arguments
      character  Action*(*)            ! (INPUT) Message action to perform
      character  Data_string*(*)       ! (INPUT) Message data

*+  Purpose
*      This routine is the interface between the main system and the
*      sysbal module.

*+  Changes
*      201093 jngh specified and programmed
*      011195 jngh  added call to message_unused
*      090299 jngh removed find modules and get other variables from init
*      100299 jngh added find modules back in
*      280999 sdb removed version reference

*+  Constant Values
      character  my_name*(*)
      parameter (my_name = 'sysbal_main')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Action.eq.ACTION_Get_variable) then
            ! respond to requests from other modules
         call sysbal_send_my_variable (Data_string)

      elseif (Action .eq. ACTION_Prepare) then
         call sysbal_zero_variables ()
         call sysbal_find_modules ()
!         call sysbal_get_other_variables ()
!         call sysbal_prepare ()

      else if (Action .eq. ACTION_Post) then
         call sysbal_find_modules ()
!         call sysbal_get_other_variables ()
         call sysbal_post ()

      else if (Action.eq.ACTION_Init) then
         call sysbal_zero_all_variables ()
         call sysbal_init ()
         call sysbal_find_modules ()
!         call sysbal_get_other_variables ()

      else if (Action.eq.ACTION_Create) then
         call doRegistrations(id)

      else
            ! Don't use message

         call Message_unused ()

      endif

      call pop_routine (my_name)
      return
      end subroutine

 ! ====================================================================
! This routine is the event handler for all events
! ====================================================================
      subroutine respondToEvent(fromID, eventID, variant)
      Use infrastructure
      implicit none
      ml_external respondToEvent

      integer, intent(in) :: fromID
      integer, intent(in) :: eventID
      integer, intent(in) :: variant

      return
      end subroutine respondToEvent
