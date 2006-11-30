module SoilPModule
   use Registrations

! ====================================================================
!      SoilP_constants
! ====================================================================

!   Short description:
!      array size_of settings and constants

!   Notes:

! ----------------------- Declaration section ------------------------

!   Constant values
   integer    max_wf_values         ! maximum no. of index/values pairs for
   parameter (max_wf_values = 10)   ! specifying water factor on
                                    ! loss of availability

   integer    max_layer             ! Maximum number of layers
   parameter (max_layer = 100)

   integer    max_crops             ! maximum no. of crops
   parameter (max_crops = 5)

   real       residue_c_frac        ! fraction of c in resiudes
   parameter (residue_c_frac = 0.4)

   integer    nfract                      ! number of fractions of fresh organic
   parameter (nfract = 3)                 ! matter


! ====================================================================
   type SoilPGlobals
      sequence
      character*10   crop_names (max_crops)
      integer        crop_owners (max_crops)

      integer      num_crops            ! Number of crops in the system
      integer      nveg


      real      labile_p (max_layer)   ! Labile P content for each layer (ppm)
      real      unavail_p (max_layer)  ! Unavailable P content for each layer (kg/ha)
      real      rock_p (max_layer)     ! Rock P content for each layer (kg/ha)
                                       ! ie. no water soluble
      real      banded_p (max_layer)   ! Banded P content for each layer (kg/ha)
      real      effective_p (max_layer)! NOT USED
      real      rlv (max_crops, max_layer)
                                       ! rlv for each layer of each crop
      real      root_depth(max_crops)  ! root depth of each crop (mm)
      real      crop_p_demand (max_crops)
                                       ! p demand for each crop (kg/ha)
      real      uptake_p_crop (max_crops, max_layer)
                                       ! uptake for each layer of each crop (kg/ha)
      real      fom_p  (max_layer)
      real      fom_p_pool(nfract,max_layer) ! fresh organic P in each pool in each layer
      real      dlt_fom_c_pool1(max_layer)        ! change in C in pool 1 in each layer
      real      dlt_fom_c_pool2(max_layer)        ! change in C in pool 2 in each layer
      real      dlt_fom_c_pool3(max_layer)        ! change in C in pool 3 in each layer
      integer   num_fom_types          ! number of fom types, from soiln2
      real      biom_p (max_layer)
      real      hum_p  (max_layer)

      real      dlt_fom_c_hum  (max_layer)
      real      dlt_fom_c_biom (max_layer)
      real      dlt_fom_c_atm  (max_layer)
      real      dlt_hum_c_biom (max_layer)
      real      dlt_hum_c_atm  (max_layer)
      real      dlt_biom_c_hum (max_layer)
      real      dlt_biom_c_atm (max_layer)

      real      fom_cp (max_layer)  ! c:p ratio of fom pool
      real      fom_cp_pool(nfract,max_layer) ! c:p ratio in each pool in each layer
      real      p_decomp

      real      dul_dep  (max_layer)   ! drained upper limit soil water content
                                       ! for each soil layer (mm water)
      real      ll15_dep (max_layer)   ! 15 bar lower limit of extractable
                                       ! soil water for each soil layer
                                       ! (mm water)
      real      sat_dep  (max_layer)   ! saturated water content for each
                                       ! soil layer (mm water)
      real      sw_dep   (max_layer)   ! soil water content for each
                                       ! soil layer (mm water)
      real      dlayer   (max_layer)   ! thickness of soil for each soil layer (mm)
      real      soil_t   (max_layer)   ! soil temperature for each soil layer (oC)
      real      bd       (max_layer)   ! soil bulk density (g/cc)

   end type SoilPGlobals
! ====================================================================
   type SoilPParameters
      sequence
      real         rate_dissol_rock_p  ! Rate at which rock P source
                                       ! becomes available (/yr)
      real         sorption (max_layer)
                                       ! Soils P sorption characteristic
      real      root_cp                ! C:P ratio of roots at initialisation
      real      root_cp_pool(3)        ! C:P ratio of roots at initialisation in each pool

   end type SoilPParameters
! ====================================================================
   type SoilPConstants
      sequence
      character*20   crop_table_name (max_crops)
      real         rate_loss_avail_p   ! Fraction of P lost per yr
                                       ! (less then 1) specified at 25 oC
      real         act_energy_loss_avail_p
                                       ! Effect of soil temperature
                                       ! on P availability
      real         availp_ratio        ! Ratio of available P : unavailable P
                                       ! at a steady state
      real         rate_decr_placement ! Fractional loss of placement
                                       ! effect per yr ( < 1)
      real         eff_band            ! Relative effectiveness of banded_p
                                       ! compared with broadcast_p
      real         biom_cp             ! c:p ratio of biom pool
      real         hum_cp              ! c:p ratio of hum pool
      real         wf_loss_index (max_wf_values)
                                       ! Index specifying water content
                                       ! for water factor for loss of
                                       ! availability
      real         wf_loss_values (max_wf_values)
                                       ! Value of water factor function
                                       ! at given index values

      real         sorption_coeff      ! exponent of Freundlich isotherm
      real         p_supply_factor (max_crops)
                                       !  factor to calc potential P supply from soil P status

      real         crit_p_rlv (max_crops)
                                       ! critical rlv above which p status is maximum

      real         lb_labile_p         ! lower bound for labile P (ppm)
      real         ub_labile_p         ! upper bound for labile P (ppm)
      real         lb_unavail_p        ! lower bound for unavailable P (kg/ha)
      real         ub_unavail_p        ! upper bound for unavailable P (kg/ha)
      real         lb_banded_p         ! lower bound for banded P (kg/ha)
      real         ub_banded_p         ! upper bound for banded P (kg/ha)
      real         lb_rock_p           ! lower bound for rock P (kg/ha)
      real         ub_rock_p           ! upper bound for rock P (kg/ha)

      real         fr_carb             ! fom fraction in carbon pool
      real         fr_cell             ! fom fraction in cellulose pool
      real         fr_lign             ! fom fraction in lignin pool

   end type SoilPConstants
! ====================================================================

   ! instance variables.
   common /InstancePointers/ ID,g,p,c
   save InstancePointers
   type (SoilPGlobals),pointer :: g
   type (SoilPParameters),pointer :: p
   type (SoilPConstants),pointer :: c
   type (IDsType), pointer :: id

   contains



! ====================================================================
subroutine soilp_Init ()
! ====================================================================
   Use infrastructure
   implicit none

!+  Purpose
!      Initialise soilp module

!+  Mission Statement
!      Initialise SoilP


!+  Constant Values
   character  myname*(*)            ! name of this procedure
   parameter (myname = 'soilp_init')

!+  Local Variables
    character Event_string*40       ! String to output

!- Implementation Section ----------------------------------

   call push_routine (myname)

   call soilp_zero_variables ()

   call soilp_get_other_variables ()

   ! Notify system that we have initialised

   Event_string = 'Initialising'
   call Write_string (Event_string)

   ! Get all parameters from parameter file

   call soilp_read_constants ()

   call soilp_read_param ()

   call soilp_get_other_init_variables ()

   call soilp_sum_report ()

   call pop_routine (myname)

   return
end subroutine



! ====================================================================
subroutine soilp_zero_variables ()
! ====================================================================
   Use infrastructure
   implicit none

!+  Purpose
!     Set all variables to initial state.  i.e. zero or blank.

!+  Mission Statement
!     Zero variables

!+  Constant Values
   character  myname*(*)            ! name of this procedure
   parameter (myname = 'soilp_zero_variables')

!- Implementation Section ----------------------------------

   call push_routine (myname)

   g%num_crops        = 0
   g%nveg             = 0

   c%crop_table_name  = ' '
   g%crop_names       = ' '
   g%crop_owners      = 0

   g%sat_dep          = 0.0
   g%dul_dep          = 0.0
   g%sw_dep           = 0.0
   g%ll15_dep         = 0.0
   g%dlayer           = 0.0
   g%soil_t           = 0.0
   g%bd               = 0.0

   g%rlv              = 0.0
   g%crop_p_demand    = 0.0
   g%uptake_p_crop    = 0.0
   g%fom_cp           = 0.0
   g%p_decomp         = 0.0

   c%act_energy_loss_avail_p = 0.0
   c%wf_loss_index           = 0.0
   c%wf_loss_values          = 0.0
   c%lb_labile_p             = 0.0
   c%ub_labile_p             = 0.0
   c%lb_unavail_p            = 0.0
   c%ub_unavail_p            = 0.0
   c%lb_banded_p             = 0.0
   c%ub_banded_p             = 0.0
   c%lb_rock_p               = 0.0
   c%ub_rock_p               = 0.0


   call fill_real_array (g%labile_p, 0.0, max_layer)
   call fill_real_array (g%unavail_p, 0.0, max_layer)
   call fill_real_array (g%rock_p, 0.0, max_layer)
   call fill_real_array (g%banded_p, 0.0, max_layer)
   call fill_real_array (g%effective_p, 0.0, max_layer)
   call fill_real_array (p%sorption, 0.0, max_layer)
   call fill_real_array (c%crit_p_rlv, 0.0, max_crops)
   call fill_real_array (c%p_supply_factor, 0.0, max_crops)
   call fill_real_array (g%dlt_fom_c_hum, 0.0, max_layer)
   call fill_real_array (g%dlt_fom_c_biom, 0.0, max_layer)
   call fill_real_array (g%dlt_fom_c_atm, 0.0, max_layer)
   call fill_real_array (g%dlt_hum_c_biom, 0.0, max_layer)
   call fill_real_array (g%dlt_hum_c_atm, 0.0, max_layer)
   call fill_real_array (g%dlt_biom_c_hum, 0.0, max_layer)
   call fill_real_array (g%dlt_biom_c_atm, 0.0, max_layer)
   call fill_real_array (g%fom_p, 0.0, max_layer)
   call fill_real_array (g%hum_p, 0.0, max_layer)
   call fill_real_array (g%biom_p, 0.0, max_layer)

   p%root_cp = 0.0
   p%rate_dissol_rock_p = 0.0

   c%rate_loss_avail_p = 0.0
   c%act_energy_loss_avail_p = 0.0
   c%availp_ratio = 0.0
   c%rate_decr_placement = 0.0
   c%eff_band = 0.0
   c%biom_cp = 0.0
   c%hum_cp = 0.0

   call pop_routine (myname)
   return
end subroutine



! ====================================================================
subroutine soilp_Send_my_variable (Variable_name)
! ====================================================================
   Use infrastructure
   implicit none

!+  Sub-Program Arguments
    character Variable_name*(*)     ! (INPUT) Variable name to search for

!+  Purpose
!       Return the value of one of our variables to caller.

!+  Mission Statement
!       Supply variable to system as requested

!+  Constant Values
   character  myname*(*)            ! name of this procedure
   parameter (myname = 'soilp_send_my_variable')

!+  Local Variables
    integer num_layers              ! number of layers
    real soil_test_p (max_layer)    ! ?????
    integer crpnum                  ! crop number
    real crop_p_uptake (max_layer)  ! crop p uptake
    integer layer                   ! layer number
    real   fom_p_pool1(max_layer)
    real   fom_p_pool2(max_layer)
    real   fom_p_pool3(max_layer)

!- Implementation Section ----------------------------------

   call push_routine (myname)

   if (variable_name .eq. 'labile_p') then
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call respond2get_real_array (variable_name ,'(kg/ha)' , g%labile_p , num_layers)

   elseif (variable_name .eq. 'unavail_p') then
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call respond2get_real_array (variable_name ,'(kg/ha)' , g%unavail_p , num_layers)

   elseif (variable_name .eq. 'banded_p') then
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call respond2get_real_array (variable_name ,'(kg/ha)' , g%banded_p , num_layers)

   elseif (variable_name .eq. 'rock_p') then
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call respond2get_real_array (variable_name ,'(kg/ha)' , g%rock_p , num_layers)

   elseif (variable_name .eq. 'soil_test_p') then
      num_layers = count_of_real_vals (g%dlayer, max_layer)

      call fill_real_array (soil_test_p, 0.0, max_layer)
      call add_real_array(g%labile_p, soil_test_p, num_layers)
      call add_real_array(g%banded_p, soil_test_p, num_layers)

      call respond2get_real_array (variable_name ,'(kg/ha)' , soil_test_p , num_layers)

   elseif (variable_name .eq. 'biom_p') then
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call respond2get_real_array (variable_name ,'()' , g%biom_p , num_layers)

   elseif (variable_name .eq. 'hum_p') then
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call respond2get_real_array (variable_name ,'()' , g%hum_p , num_layers)

   elseif (variable_name .eq. 'fom_p') then
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call respond2get_real_array (variable_name ,'(kg/ha)' , g%fom_p , num_layers)

   elseif (variable_name .eq. 'fom_p_pool1') then
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call fill_real_array (fom_p_pool1, 0.0, max_layer)
      do layer = 1, num_layers
         fom_p_pool1(layer) = g%fom_p_pool(1,layer)
      end do
      call respond2get_real_array (variable_name ,'(kg/ha)' , fom_p_pool1 , num_layers)

   elseif (variable_name .eq. 'fom_p_pool2') then
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call fill_real_array (fom_p_pool2, 0.0, max_layer)
      do layer = 1, num_layers
         fom_p_pool2(layer) = g%fom_p_pool(2,layer)
      end do
      call respond2get_real_array (variable_name ,'(kg/ha)' , fom_p_pool2 , num_layers)

   elseif (variable_name .eq. 'fom_p_pool3') then
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call fill_real_array (fom_p_pool3, 0.0, max_layer)
      do layer = 1, num_layers
         fom_p_pool3(layer) = g%fom_p_pool(3,layer)
      end do
      call respond2get_real_array (variable_name ,'(kg/ha)' , fom_p_pool3 , num_layers)

   elseif (variable_name .eq. 'fom_cp') then
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call respond2get_real_array (variable_name ,'()' , g%fom_cp , num_layers)

   else if (index(Variable_name,'uptake_p_').eq.1) then
      crpnum = position_in_char_array (Variable_name(10:), g%crop_names, max_crops)
      if (crpnum .gt. 0.0) then
         !crop found so send uptake
         num_layers = count_of_real_vals (g%dlayer, max_layer)
         do layer=1,num_layers
            crop_p_uptake (layer) = g%uptake_p_crop (crpnum,layer)
         end do
         call respond2Get_real_array ( Variable_name, '(kg/ha)', crop_p_uptake, num_layers)
      else
         ! Don't use message
         call Message_Unused ()
      endif


   else
      ! Don't use message
      call Message_Unused ()

   endif

   call pop_routine (myname)
   return
end subroutine



!     ===========================================================
subroutine soilp_read_param ()
!     ===========================================================
   Use infrastructure
   implicit none

!+  Purpose
!       Read in all parameters from parameter file.

!+  Mission Statement
!       Read parameters

!+  Calls


!+  Constant Values
   character  myname*(*)    ! name of this procedure
   parameter (myname = 'soilp_read_param')
!
   character section_name*(*)
   parameter (section_name = 'parameters')

!+  Local Variables
   integer i                ! counter
   integer numvals          ! number of values read
   integer layer            ! layer number
   real lab_p (max_layer)   ! labile P (ppm)

!- Implementation Section ----------------------------------

   call push_routine (myname)

   call write_string ( new_line//'   - Reading Parameters')

   call read_real_array (section_name,'labile_p',max_layer,'(ppm)',lab_p,numvals,c%lb_labile_p,c%ub_labile_p)
   ! convert ppm to kg/ha
   do layer=1, max_layer
      g%labile_p (layer) = divide (lab_p (layer),soilp_fac (layer), 0.0)
   end do

   call read_real_array_optional (section_name,'unavail_p',max_layer,'(kg/ha)',g%unavail_p,numvals,c%lb_unavail_p,c%ub_unavail_p)
   if (numvals .eq. 0.0) then
      ! default unavailable P to steady state ratio of available P
      do layer = 1, max_layer
         g%unavail_p (layer) = divide (g%labile_p (layer),c%availp_ratio, 0.0)
      end do
   else
   endif

   call read_real_array_optional (section_name,'banded_p',max_layer,'(kg/ha)',g%banded_p,numvals,c%lb_banded_p,c%ub_banded_p)
   if (numvals .eq. 0.0) then
      ! default banded P to zero
      call fill_real_array (g%banded_p, 0.0, max_layer)
   else
   endif

   call read_real_array_optional (section_name,'rock_p',max_layer,'(kg/ha)',g%rock_p,numvals,c%lb_rock_p,c%ub_rock_p)
   if (numvals .eq. 0.0) then
      ! default rock P to zero
      call fill_real_array (g%rock_p, 0.0, max_layer)
   else
   endif

   call read_real_array (section_name,'sorption',max_layer,'()',p%sorption,numvals,0.0,10000.0)

   call read_real_var (section_name,'root_cp','()',p%root_cp,numvals,0.0,300.0)


   ! Read in CP ratio in each of the fractions
   call read_real_array_optional (section_name, 'root_cp_pool', 3, '()', p%root_cp_pool, numvals, 0.0, 1000.0)
   ! Check if all values supplied.  If not use average C:P ratio in all pools
   if (numvals.lt.3) then
     do i = 1,3
       p%root_cp_pool(i)=p%root_cp
     end do
   endif

   call read_real_var (section_name,'rate_dissol_rock_p','()',p%rate_dissol_rock_p,numvals,0.0,1.0)
   ! Now change rate coefficient from fraction per
   ! year to fraction per day
   p%rate_dissol_rock_P = - alog (1 - p%rate_dissol_rock_p) / 365.0

   call pop_routine (myname)
   return
end subroutine



! ====================================================================
subroutine soilp_set_my_variable (Variable_name)
! ====================================================================
   Use infrastructure
   implicit none

!+  Sub-Program Arguments
   character Variable_name*(*)      ! (INPUT) Variable name to search for

!+  Purpose
!      Set one of our variables altered by some other module.

!+  Mission Statement
!      Set internal variable as requested

!+  Constant Values
   character  myname*(*)            ! name of this procedure
   parameter (myname = 'soilp_set_my_variable')

!+  Local Variables
   real Tarray(max_layer)           ! temporary array
   integer Numvals                  ! number of values read

!- Implementation Section ----------------------------------

   call push_routine (myname)
   if (Variable_name .eq. 'labile_p') then

      call collect_real_array(variable_name, max_layer, '(kg/ha)', g%labile_p, Numvals, 0.0, 1000.0)

   elseif (Variable_name .eq. 'banded_p') then

      call collect_real_array(variable_name, max_layer, '(kg/ha)', g%banded_p, Numvals, 0.0, 1000.0)

   elseif (Variable_name .eq. 'rock_p') then

      call collect_real_array(variable_name, max_layer, '(ppm)', g%rock_p, Numvals, 0.0, 1000.0)

   elseif (Variable_name .eq. 'dlt_labile_p') then

      call collect_real_array(variable_name, max_layer, '(kg/ha)', Tarray, Numvals,-1000.0, 1000.0)

      call add_real_array (Tarray, g%labile_p, Numvals)
      call bound_check_real_array (g%labile_p, 0.0, 1000.0, 'g%labile_p', Numvals)

   elseif (Variable_name .eq. 'dlt_banded_p') then

      call collect_real_array(variable_name, max_layer, '(kg/ha)', Tarray, Numvals,-1000.0, 1000.0)

      call add_real_array (Tarray, g%banded_p, Numvals)
      call bound_check_real_array (g%banded_p, 0.0, 1000.0, 'g%banded_p', Numvals)

   elseif (Variable_name .eq. 'dlt_rock_p') then

      call collect_real_array &
            (variable_name, max_layer, '(kg/ha)', Tarray, Numvals,-1000.0, 1000.0)

      call add_real_array (Tarray, g%rock_p, Numvals)
      call bound_check_real_array (g%rock_p, 0.0, 1000.0, 'g%rock_p', Numvals)

   else
      ! Don't know this variable name
      call Message_unused ()
   endif

   call pop_routine (myname)
   return
end subroutine



! ====================================================================
subroutine soilp_read_constants ()
! ====================================================================
   Use infrastructure
   implicit none

!+  Purpose
!      Read in all constants from ini file.

!+  Mission Statement
!     Read constants

!+  Constant Values
   character*(*) section_name
   parameter (section_name = 'constants')
!
   character*(*) myname             ! name of current procedure
   parameter (myname = 'soilp_read_constants')

!+  Local Variables
   integer    numvals               ! number of values read from file

!- Implementation Section ----------------------------------
   call push_routine (myname)

   call write_string (new_line//'   - Reading Constants')

   call read_real_var (section_name, 'rate_loss_avail_p', '()', c%rate_loss_avail_p, numvals, 0.0, 1.0)

   call read_real_var (section_name, 'act_energy_loss_avail_p', '()', c%act_energy_loss_avail_p, numvals, 0.0, 100.0)

   call read_real_var (section_name, 'availp_ratio', '()', c%availp_ratio, numvals, 0.01, 0.25)


   call read_real_var (section_name, 'rate_decr_placement', '()', c%rate_decr_placement, numvals, 0.0, 1.0)

   call read_real_var (section_name, 'eff_band', '()', c%eff_band, numvals, 0.0, 100.0)

   call read_real_var (section_name, 'biom_cp', '()', c%biom_cp, numvals, 0.0, 100.0)

   call read_real_var (section_name, 'hum_cp', '()', c%hum_cp, numvals, 0.0, 100.0)

   call read_real_array (section_name, 'wf_loss_index', max_wf_values, '()', c%wf_loss_index, numvals, 0.0, 2.0)

   call read_real_array (section_name,  'wf_loss_values',  max_wf_values,  '()',  c%wf_loss_values,  numvals,  0.0,  1.0)


   ! Now change rate coefficients from fraction per year to fraction per day
   c%rate_loss_avail_p = - alog (1 - c%rate_loss_avail_p) / 365.0
   c%rate_decr_placement = - alog (1 - c%rate_decr_placement) / 365.0

   !  read crop constants
   call read_real_var (section_name,'sorption_coeff','()',c%sorption_coeff,numvals,0.0,2.0)

   call Read_char_array(section_name,'crop_name',max_crops,'()',c%crop_table_name,numvals)

   call read_real_array (section_name,'p_supply_factor',max_crops,'()',c%p_supply_factor,numvals,0.0,10.0)

   call read_real_array (section_name,'crit_p_rlv',max_crops,'()',c%crit_p_rlv,numvals,0.0,100.0)

   ! read P pool bounds
   call read_real_var (section_name,'lb_labile_p','(ppm)',c%lb_labile_p,numvals,0.0,100.0)

   call read_real_var (section_name,'ub_labile_p','(ppm)',c%ub_labile_p,numvals,0.0,1000.0)

   call read_real_var (section_name,'lb_unavail_p','(kg/ha)',c%lb_unavail_p,numvals,0.0,100.0)

   call read_real_var (section_name,'ub_unavail_p','(kg/ha)',c%ub_unavail_p,numvals,0.0,10000.0)

   call read_real_var (section_name,'lb_banded_p','(kg/ha)',c%lb_banded_p,numvals,0.0,100.0)

   call read_real_var (section_name,'ub_banded_p','(kg/ha)',c%ub_banded_p,numvals,0.0,1000.0)

   call read_real_var (section_name,'lb_rock_p','(kg/ha)',c%lb_rock_p,numvals,0.0,100.0)

   call read_real_var (section_name,'ub_rock_p','(kg/ha)',c%ub_rock_p,numvals,0.0,1000.0)


   call pop_routine (myname)
   return
end subroutine



! ====================================================================
subroutine soilp_get_other_variables ()
! ====================================================================
   Use infrastructure
   implicit none

!+  Purpose
!      Get the values of variables from other modules

!+  Mission Statement
!      Get variables from system

!+  Constant Values
   character  myname*(*)            ! name of this procedure
   parameter (myname = 'soilp_get_other_variables')

!+  Local Variables
    integer numvals                 ! number of values returned
!       real    temp_array(max_layer)   ! tempory array

!- Implementation Section ----------------------------------

   call push_routine (myname)

   call Get_real_array (unknown_module,'dlayer',max_layer,'(mm)',g%dlayer,numvals,0.0,1000.0)

   call Get_real_array (unknown_module,'st',max_layer,'()',g%soil_t,numvals,-20.0,50.0)

   call Get_real_array (unknown_module,'sw_dep',max_layer,'(mm)',g%sw_dep,numvals,0.00001,1000.0)

   call Get_real_array (unknown_module,'ll15_dep',max_layer,'(mm)',g%ll15_dep,numvals,0.00001,1000.0)

   call Get_real_array (unknown_module,'dul_dep',max_layer,'(mm)',g%dul_dep,numvals,0.00001,1000.0)

   call Get_real_array (unknown_module,'sat_dep',max_layer,'(mm)',g%sat_dep,numvals,0.00001,1000.0)

   call Get_real_array (unknown_module,'bd',max_layer,'(g/cc)',g%bd,numvals,0.01,3.0)

   call Get_real_array (unknown_module,'dlt_fom_c_hum',max_layer,'()',g%dlt_fom_c_hum,numvals,0.0,1000.)

   call Get_real_array (unknown_module,'dlt_fom_c_biom',max_layer,'()',g%dlt_fom_c_biom,numvals,0.0,1000.)

   call Get_real_array (unknown_module,'dlt_fom_c_atm',max_layer,'()',g%dlt_fom_c_atm,numvals,0.0,1000.)

   call Get_real_array (unknown_module,'dlt_hum_c_biom',max_layer,'()',g%dlt_hum_c_biom,numvals,0.0,1000.)

   call Get_real_array (unknown_module,'dlt_hum_c_atm',max_layer,'()',g%dlt_hum_c_atm,numvals,0.0,1000.)

   call Get_real_array (unknown_module,'dlt_biom_c_hum',max_layer,'()',g%dlt_biom_c_hum,numvals,0.0,1000.)

   call Get_real_array (unknown_module,'dlt_biom_c_atm',max_layer,'()',g%dlt_biom_c_atm,numvals,0.0,1000.)

   call Get_real_array (unknown_module,'dlt_fom_c_pool1',max_layer,'()',g%dlt_fom_c_pool1,numvals,0.0,5000.)

   call Get_real_array (unknown_module,'dlt_fom_c_pool2',max_layer,'()',g%dlt_fom_c_pool2,numvals,0.0,5000.)

   call Get_real_array (unknown_module,'dlt_fom_c_pool3',max_layer,'()',g%dlt_fom_c_pool3,numvals,0.0,5000.)


   call pop_routine (myname)
   return
end subroutine



! ====================================================================
subroutine soilp_process ()
! ====================================================================
   Use infrastructure
   implicit none

!+  Purpose
!      Calculates changes in soil P.
!      Processes considered are:
!        - loss of available P
!        - dissolution of a non water soluble source
!        - loss of effectiveness of banded P fertilizer
!        - organic P transformations
!        - addition of P from residues including roots
!        - P uptake by crops

!+  Mission Statement
!      Timestep calculations

!+  Constant Values
   character*(*) myname               ! name of current procedure
   parameter (myname = 'soilp_process')

!- Implementation Section ----------------------------------
   call push_routine (myname)

   ! Update P balance

   call soilp_availability_loss ()
   call soilp_rock_p_dissolution ()
   call soilp_decrease_banded_p_effect ()

   ! Organic P transformations

   call soilp_min_hum ()
   call soilp_min_biom ()
   call soilp_min_fom ()

   ! Do crop stuff

   call soilp_find_crops ()
   call soilp_get_crop_variables ()
   call soilp_crop_p_uptake ()

   call pop_routine (myname)
   return
end subroutine



! ====================================================================
subroutine soilp_rock_p_dissolution ()
! ====================================================================
   Use infrastructure
   implicit none

!+  Purpose
!      Describes release of Labile P from a non-water soluble source.

!+  Mission Statement
!      Release labile P from non-water soluble source

!+  Constant Values
   character*(*) myname             ! name of current procedure
   parameter (myname = 'soilp_rock_p_dissolution')

!+  Local Variables
   integer    layer                 ! soil layer count
   integer    num_layers            ! number of soil layers used
   real       flux_p                ! amount of p to shift states

!- Implementation Section ----------------------------------
   call push_routine (myname)

   num_layers = count_of_real_vals (g%dlayer, max_layer)

   do layer = 1, num_layers

      flux_p = g%rock_p (layer) * p%rate_dissol_rock_p

      g%labile_p (layer) = g%labile_p (layer) + flux_p

      g%rock_p (layer) = g%rock_p (layer) - flux_p

   end do

   call soilp_bound_check (num_layers)

   call pop_routine (myname)
   return
end subroutine



! ====================================================================
subroutine soilp_availability_loss ()
! ====================================================================
   Use infrastructure
   implicit none

!+  Purpose
!      Calculate changes in P availability.
!      Transformation of labile P to unavailable P assumed to depend on
!      temperature (defined in terms of activation energy, after Barrow)
!      and soil moisture
!      Both labile P and banded P are affected by loss in availability.
!      Reverse process converts unavailable P to labile P assuming rate
!      constants are in ratio of availP_ratio

!+  Mission Statement
!      Change P availability

!+  Calls


!+  Constant Values
   character*(*) myname             ! name of current procedure
   parameter (myname = 'soilp_availability_loss')

!+  Local Variables
   integer    layer                 ! soil layer count
   integer    num_layers            ! number of soil layers used
   real       flux_p1               ! amount of p labile to unavailable
   real       flux_p2               ! amount of p banded to unavailable
   real       flux_p3               ! amount of p unavailable to labile
   real       adj_rate_loss_avail_p ! rate adjusted to soil temperature
   real       wf                    ! water factor

!- Implementation Section ----------------------------------
   call push_routine (myname)

   num_layers = count_of_real_vals (g%dlayer, max_layer)

   do layer = 1, num_layers

      ! calculate rate of loss of available P at soil temperature
      ! based on activation energy and value at 25oC
      adj_rate_loss_avail_p = c%rate_loss_avail_p* exp (c%act_energy_loss_avail_P* 1000.0 / 8.314* ((1.0 / 298.0) - (1.0 / (273.0 + g%soil_t (layer)))))

      ! calculate water factor
      wf = soilp_wf (layer)

      ! calculate loss of labile P
      flux_p1 = g%labile_p (layer) * wf * adj_rate_loss_avail_p

      ! calculate loss of banded p
      flux_p2 = g%banded_p (layer) * wf * adj_rate_loss_avail_p

      ! calculate gain from unavailable P
      flux_p3 = g%unavail_p (layer) * wf * adj_rate_loss_avail_p* c%availp_ratio

      ! modify pool sizes
      g%unavail_p (layer)= g%unavail_p (layer) + flux_p1+ flux_p2 - flux_p3
      g%labile_p (layer) = g%labile_p (layer) - flux_p1 + flux_p3
      g%banded_p (layer) = g%banded_p (layer) - flux_p2


   end do

   call soilp_bound_check (num_layers)

   call pop_routine (myname)
   return
end subroutine



! ====================================================================
subroutine soilp_decrease_banded_p_effect ()
! ====================================================================
   Use infrastructure
   implicit none

!+  Purpose
!      Fertiliser effectiveness is higher when applied as a band
!      than when broadcast.  However effect decreases with time.
!      Tillage destroys any banding.

!+  Mission Statement
!      Decrease effectiveness of banded P

!+  Constant Values
   character*(*) myname               ! name of current procedure
   parameter (myname = 'soilp_decrease_banded_p_effect')

!+  Local Variables
   integer    layer                 ! soil layer count
   integer    num_layers            ! number of soil layers used
   real       flux_p                ! amount of p to shift states
!- Implementation Section ----------------------------------
   call push_routine (myname)

   num_layers = count_of_real_vals (g%dlayer, max_layer)

   do layer = 1, num_layers
      flux_p = g%banded_p (layer) * c%rate_decr_placement
      g%labile_p (layer) = g%labile_p (layer) + flux_p
      g%banded_p (layer) = g%banded_p (layer) - flux_p
   end do

   call soilp_bound_check (num_layers)

   call pop_routine (myname)
   return
end subroutine



!     ===========================================================
real function soilp_wf (layer)
!     ===========================================================
   Use infrastructure
   implicit none

!+  Sub-Program Arguments
   integer    layer              ! (INPUT) layer number

!+  Purpose
!       Calculates a 0-1 water factor for loss of availability.

!+  Mission Statement
!       Loss of availability water factor for %1

!+  Assumptions
!       1 < layer < num_layers

!+  Constant Values
   character  my_name*(*)        ! name of subroutine
   parameter (my_name = 'soilp_wf')

!+  Local Variables
   real       wfd                ! temporary water factor (0-1)

!- Implementation Section ----------------------------------

   call push_routine (my_name)

   if (g%sw_dep(layer).gt.g%dul_dep(layer)) then

      ! saturated (1 < wfd < 2)
      wfd = 1.0 + divide (g%sw_dep(layer) - g%dul_dep(layer), g%sat_dep(layer) - g%dul_dep(layer), 0.0)
      wfd = bound (wfd, 1.0, 2.0)

   else
       ! unsaturated (0 < wfd < 1)

       wfd = divide (g%sw_dep(layer) - g%ll15_dep(layer), (g%dul_dep(layer) - g%ll15_dep(layer)), 0.0)
       wfd = bound (wfd, 0.0, 1.0)

   endif

   soilp_wf =linear_interp_real (wfd, c%wf_loss_index, c%wf_loss_values, max_wf_values)

   call pop_routine (my_name)
   return
end function



! ====================================================================
subroutine soilp_find_crops ()
! ====================================================================
   Use infrastructure
   implicit none

!+  Purpose
!      Find all crops in the system.

!+  Mission Statement
!      Find crops in the system

!+  Constant Values
   character*(*) myname           ! name of current procedure
   parameter (myname = 'soilp_find_crops')

!+  Local Variables
    integer owner_module          ! owner module id
    character crpname*10          ! crop name
    integer numvals               ! number of values returned
    integer request_no            ! request number

!- Implementation Section ----------------------------------
   call push_routine (myname)

   request_no = 0
   g%num_crops = 0

   10 continue

   request_no = request_no + 1

   call get_char_vars(request_no,'crop_type','()',crpname,numvals)

   if (numvals.eq.0) then
      ! no more crops out there - get out of here!!!
      goto 999

   else
      if (g%num_crops.lt.max_crops) then
         owner_module = get_posting_module ()
         g%num_crops = g%num_crops + 1
         g%crop_names(g%num_crops) = crpname
         g%crop_owners(g%num_crops) = owner_module

      else
         call fatal_error (err_internal, 'too many crops')
      endif

   endif

   goto 10

   999 continue

   g%nveg = g%num_crops

   call pop_routine (myname)
   return
end subroutine



! ====================================================================
subroutine soilp_get_crop_variables ()
! ====================================================================
   Use infrastructure
   implicit none

!+  Purpose
!      Get crop variables from crop modules.

!+  Mission Statement
!      Request variables from crops

!+  Constant Values
   character*(*) myname             ! name of current procedure
   parameter (myname = 'soilp_get_crop_variables')

!+  Local Variables
   real rlv (max_layer)
   integer vegnum                   ! solute array index counter
   integer layer                    ! layer number specifier
   integer numvals                  ! number of values returned

!- Implementation Section ----------------------------------
   call push_routine (myname)

   do vegnum = 1, g%num_crops

      ! Initialise tempory varaibles to zero
      call fill_real_array (rlv, 0.0, max_layer)

      ! check that each crop is on the crop table

      if (position_in_char_array (g%crop_names (vegnum),c%crop_table_name, max_crops).eq. 0.0) then
         call fatal_error (err_internal,'Crop not specified in SoilP - '// g%crop_names (vegnum))
      else
         ! crop has been specified so end do

         call get_real_array (g%crop_owners(vegnum),'rlv',max_layer,'(mm/mm^3)',rlv,numvals,0.0,1.0)

         if (numvals.gt.0) then
            do layer = 1,numvals
               g%rlv(vegnum,layer) = rlv(layer)
            end do

         else
            call fatal_error (Err_Internal,'no rlv returned from '//g%crop_names(vegnum))
         endif

         !  mep/dsg 200302  added get for root_depth
         call get_real_var (g%crop_owners(vegnum),'root_depth','(mm)',g%root_depth(vegnum),numvals,0.0,10000.0)

         if (numvals .eq. 0) then
            call fatal_error (Err_Internal,'no root_depth returned from '//g%crop_names(vegnum))
         endif


         call get_real_var (g%crop_owners(vegnum),'p_demand','(g/m2)',g%crop_p_demand (vegnum),numvals,0.0,2.0)
         g%crop_p_demand = g%crop_p_demand * gm2kg/sm2ha

         if (numvals .eq. 0) then
            call fatal_error (Err_Internal,'no p demand returned from '//g%crop_names(vegnum))
         endif

      endif

   end do

   call pop_routine (myname)
   return
end subroutine



! ====================================================================
subroutine soilp_crop_p_uptake ()
! ====================================================================
   Use infrastructure
   implicit none

!+  Purpose
!      Calculate Phosphorus uptake for each crop based on phosphorus
!      status in each layer.  P status depends on labile p in soil and
!      is modified by soil P sorption, roots and soil water content.

!+  Mission Statement
!      Uptake P for each crop

!+  Calls


!+  Constant Values
   character*(*) myname               ! name of current procedure
   parameter (myname = 'soilp_crop_p_uptake')

!+  Local Variables
    integer layer                     ! layer number
    integer num_layers                ! number of layers
    real status_profile               ! soil profile P status
    real status (max_layer)           ! soil layer P status
!       real fr_supply                    ! fraction of
    integer crop                      ! crop number
    integer crop_num                  ! number of crops
    real uptake_p_tot                 ! total plant P uptake
    real fr_labile                    ! fraction of uptake from labile pool
    real effective_p (max_layer)      !
    real p_layer                      ! temporary calculator
    real b_layer                      ! inverse of c%sorption_coeff

!- Implementation Section ----------------------------------
   call push_routine (myname)

   b_layer = divide(1.0,c%sorption_coeff, 0.0)

   num_layers = count_of_real_vals (g%dlayer, max_layer)

   do crop = 1, g%num_crops

      status_profile = 0.0

      do layer = 1, num_layers
         effective_p(layer) = g%labile_p (layer) + c%eff_band* g%banded_p (layer)

         ! mep/dsg 060302   p uptake modified
         ! mep/dsg 200302  convert effective_p from kg/ha to mg/kg for this calculation
         p_layer = divide(effective_p(layer)* soilp_fac(layer),p%sorption (layer), 0.0)
         p_layer = p_layer ** b_layer

         ! mep/dsg 200302  Allow for soil depth in layer
         status (layer) = p_layer* soilp_root_fac (crop, layer)* soilp_sw_fn (layer)* g%dlayer(layer)
         status_profile = status_profile + status (layer)
      end do

      ! calculate potential p uptake
      crop_num = position_in_char_array (g%crop_names (crop),c%crop_table_name, max_crops)
      uptake_p_tot = status_profile *c%p_supply_factor(crop_num)
      uptake_p_tot = bound (uptake_p_tot,0.0,g%crop_p_demand (crop))

      ! calculate p uptake from each layer
      do layer = 1, num_layers
         g%uptake_p_crop (crop, layer) = uptake_p_tot *divide (status (layer), status_profile, 0.0)

         ! modify p balance
         fr_labile = divide (g%labile_p (layer),effective_p(layer),0.0)
         g%labile_p (layer) =  g%labile_p (layer) -g%uptake_p_crop (crop, layer) *fr_labile
         g%banded_p (layer) =  g%banded_p (layer) -g%uptake_p_crop (crop, layer) *(1.0 - fr_labile)
      end do

      call soilp_bound_check (num_layers)

   end do

   call pop_routine (myname)
   return
end subroutine



! ====================================================================
real function soilp_sorption_fn (layer)
! ====================================================================
   Use infrastructure
   implicit none

!+  Sub-Program Arguments
    integer layer               ! (INPUT) layer number

!+  Purpose
!      Soil P sorption factor.

!+  Mission Statement
!      P sorption factor for %1

!+  Constant Values
   character*(*) myname         ! name of current procedure
   parameter (myname = 'soilp_sorption_fn')

!- Implementation Section ----------------------------------
   call push_routine (myname)

   soilp_sorption_fn = divide (1.0, p%sorption (layer), 0.0)

   call pop_routine (myname)
   return
end function



! ====================================================================
real function soilp_root_fac (crop, layer)
! ====================================================================
   Use infrastructure
   implicit none

!+  Sub-Program Arguments
    integer crop                     ! (INPUT) crop number
    integer layer                    ! (INPUT) layer number

!+  Purpose
!      Crop root factor.

!+  Mission Statement
!      Root factor for %1 in %2

!+  Constant Values
   character*(*) myname              ! name of current procedure
   parameter (myname = 'soilp_root_fac')

!+  Local Variables
    integer crop_num                 ! crop position in string

!- Implementation Section ----------------------------------
   call push_routine (myname)

   ! mep/dsg  200302  we require current root_depth

   soilp_root_fac = root_proportion(layer,g%dlayer(layer),g%root_depth(crop))

   soilp_root_fac = bound (soilp_root_fac, 0.0, 1.0)

   call pop_routine (myname)
   return
end function



! ====================================================================
real function soilp_sw_fn (layer)
! ====================================================================
   Use infrastructure
   implicit none

!+  Sub-Program Arguments
    integer layer               ! (INPUT) layer number

!+  Purpose
!      Soil water function.

!+  Mission Statement
!      Water function in %1

!+  Constant Values
   character*(*) myname               ! name of current procedure
   parameter (myname = 'soilp_sw_fn')

!+  Local Variables
    real sw                     ! soil water
    real ll                     ! ll15
    real dul                    ! drained upper limit

!- Implementation Section ----------------------------------
   call push_routine (myname)

   sw = divide (g%sw_dep (layer), g%dlayer (layer), 0.0)
   ll = divide (g%ll15_dep (layer), g%dlayer (layer), 0.0)
   dul = divide (g%dul_dep (layer), g%dlayer (layer), 0.0)

   soilp_sw_fn = min (sw * (sw - ll), dul * (dul - ll))

   soilp_sw_fn = l_bound (soilp_sw_fn, 0.0)

   call pop_routine (myname)
   return
end function



!     ================================================================
subroutine soilp_Tillage ()
!     ================================================================
   Use infrastructure
   implicit none

!+  Purpose
!      Capture tillage events to destroy banding.

!+  Mission Statement
!      Calculate effects of tillage

!+  Constant Values
   character*(*) my_name             ! name of current procedure
   parameter (my_name = 'soilp_tillage')
!
!      character*(*) Tillage_section    ! section name for tillage info in
!      parameter (Tillage_section = 'tillage') ! lookup file

!+  Local Variables
   real      F_incorp               ! Fraction of residue incorporated
                                    ! by tillage. (0-1)
   integer   numvals                ! Number of values found in data string
   real      Tillage_depth          ! depth of tillage (mm)
   integer   tilled_layers          ! number of layers tilled
   integer   num_layers             ! number of layers
   integer   layer                  ! layer number

!- Implementation Section ----------------------------------
   call push_routine (my_name)

   ! --------------------------------------------------------------
   !       Get User defined tillage effects on residue
   ! --------------------------------------------------------------

   call collect_real_var_optional ('f_incorp', '()', f_incorp, numvals, 0.0, 1.0)

   if (numvals .eq. 0) then
      f_incorp = 1.0
   else
   endif

   call collect_real_var_optional ('tillage_depth', '()', tillage_depth, numvals, 0.0, 1000.0)

   if (numvals .eq. 0) then
      tillage_depth = g%dlayer (1)
   else
   endif

   if ((tillage_depth .gt. 0.0) .and. (f_incorp .gt. 0.0)) then

      ! -------------------------------------------------------------
      !              Now destroy banded P
      ! -------------------------------------------------------------

      num_layers = count_of_real_vals (g%dlayer, max_layer)
      tilled_layers = find_layer_no (tillage_depth, g%dlayer,num_layers)

      do layer = 1, tilled_layers
         g%labile_p (layer) = g%labile_p (layer) + g%banded_p (layer)
         g%banded_p (layer) = 0.0
      end do

      ! -------------------------------------------------------------
      !             Redistribute each of the P pools
      ! -------------------------------------------------------------

      call soilp_redistribute_p (g%labile_p, tillage_depth)
      call soilp_redistribute_p (g%rock_p, tillage_depth)
      call soilp_redistribute_p (g%effective_p, tillage_depth)

      call bound_check_real_array (g%labile_p,0.0, 1000.0, 'g%labile_p', num_layers)
      call Write_string ('Banded P destroyed')

   else
      ! This type of tillage does not affect banded P.
   endif

   call pop_routine (my_name)
   return
end subroutine



! ====================================================================
subroutine soilp_redistribute_p (parray, depth)
! ====================================================================
   Use infrastructure
   implicit none

!+  Sub-Program Arguments
    real  parray (max_layer)
    real  depth

!+  Purpose
!      Evenly redistribute P to specified depth.

!+  Mission Statement
!      Redistribute %1 over %2

!+  Constant Values
   character*(*) myname            ! name of current procedure
   parameter (myname = 'soilp_redistribute_p')

!+  Local Variables
    integer num_layers             ! number of layers
    integer redistribute_layer     ! layer to redistribute to
    integer layer                  ! layer number
    real frac_bottom_layer         ! fraction of bottom layer to redistribute
    real redistributed_p           ! total P to redistribute

!- Implementation Section ----------------------------------
   call push_routine (myname)

   ! Calulate which layer to redistribute to
   num_layers = count_of_real_vals (g%dlayer, max_layer)
   redistribute_layer = find_layer_no (depth, g%dlayer,num_layers)

   ! Calulate the fraction of bottom layer to redistribute
   frac_bottom_layer = divide (depth -sum_real_array (g%dlayer, redistribute_layer - 1),g%dlayer (redistribute_layer), 0.0)

   ! Calulate the total P to redistribute
   redistributed_p = sum_real_array (parray, redistribute_layer) -(1.0 - frac_bottom_layer) *parray (redistribute_layer)

   ! Redistribute P through layers
   if (redistribute_layer .gt. 1) then
      do layer = 1, redistribute_layer
         if (layer .eq. redistribute_layer) then
            parray (layer) = (1.0 - frac_bottom_layer)* parray (layer) + redistributed_p* divide (g%dlayer (layer) *frac_bottom_layer, depth, 0.0)
         else
            parray (layer) = redistributed_p *divide (g%dlayer (layer), depth, 0.0)
         endif
      end do
   else
      ! Only 1 layer to redistribute so do nothing
   endif

   call pop_routine (myname)
   return
end subroutine



!     ===========================================================
real function soilp_fac (layer)
!     ===========================================================
   Use infrastructure
   implicit none

!+  Sub-Program Arguments
   integer    layer                 ! (INPUT) layer number

!+  Purpose
!      Convert kg/ha to ppm of soil in a given layer

!+  Mission Statement
!      kg/ha to ppm conversion for %1

!+  Constant Values
   character  my_name*(*)           ! name of subroutine
   parameter (my_name = 'soilp_fac')

!- Implementation Section ----------------------------------

   call push_routine (my_name)

   ! calculate conversion factor from kg/ha to ppm (mg/kg)

   soilp_fac = divide (100.0, g%bd (layer) * g%dlayer (layer), 0.0)

   call pop_routine (my_name)
   return
end function



! ====================================================================
subroutine soilp_bound_check (num_layers)
! ====================================================================
   Use infrastructure
   implicit none

!+  Sub-Program Arguments
    integer num_layers            ! number of layers to check

!+  Purpose
!      Bound check all P pools

!+  Mission Statement
!      Check P pools

!+  Constant Values
   character*(*) myname           ! name of current procedure
   parameter (myname = 'soilp_bound_check')

!- Implementation Section ----------------------------------
   call push_routine (myname)

   call bound_check_real_array (g%labile_p,c%lb_labile_p, c%ub_labile_p,'g%labile_p', num_layers)

   call bound_check_real_array (g%banded_p,c%lb_banded_p, c%ub_banded_p,'g%banded_p', num_layers)

   call bound_check_real_array (g%rock_p,c%lb_rock_p, c%ub_rock_p,'g%rock_p', num_layers)

   call bound_check_real_array (g%unavail_p,c%lb_unavail_p, c%ub_unavail_p,'g%unavail_p', num_layers)

   call pop_routine (myname)
   return
end subroutine



! ====================================================================
subroutine soilp_get_other_init_variables ()
! ====================================================================
   Use infrastructure
   implicit none

!+  Purpose
!      Get the values of variables from other modules that
!      only needed for initialisation.

!+  Mission Statement
!      Request varibles needed for initialisation

!+  Constant Values
   character  myname*(*)            ! name of this procedure
   parameter (myname = 'soilp_get_other_init_variables')

!+  Local Variables
    integer numvals                 ! number of values returned
    integer layer                   ! layer counter
    real    temp_c (max_layer)      ! temporary array
    real    temp_c1 (max_layer)      ! temporary array
    real    temp_c2 (max_layer)      ! temporary array
    real    temp_c3 (max_layer)      ! temporary array
    integer num_layers               ! number of soil layers
!- Implementation Section ----------------------------------

   call push_routine (myname)
   temp_c(:) = 0.0
   temp_c1(:) = 0.0
   temp_c2(:) = 0.0
   temp_c3(:) = 0.0

   call Get_real_array (unknown_module,'biom_c',max_layer,'()',temp_c,numvals,0.0,10000.0)

   do layer = 1, max_layer
      g%biom_p (layer) = temp_c (layer) / c%biom_cp
   end do

   call Get_real_array (unknown_module,'hum_c',max_layer,'()',temp_c,numvals,0.0,100000.0)

   do layer = 1, max_layer
      g%hum_p  (layer) = temp_c  (layer) / c%hum_cp
   end do

   call Get_real_array (unknown_module,'fom_c_pool1',max_layer,'()',temp_c1,numvals,0.0,5000.0)
   call Get_real_array (unknown_module,'fom_c_pool2',max_layer,'()',temp_c2,numvals,0.0,5000.0)
   call Get_real_array (unknown_module,'fom_c_pool3',max_layer,'()',temp_c3,numvals,0.0,5000.0)

   num_layers = count_of_real_vals (g%dlayer, max_layer)

   do layer = 1, num_layers
      g%fom_p_pool(1,layer) = temp_c1(layer) / p%root_cp_pool(1)
      g%fom_p_pool(2,layer) = temp_c2(layer) / p%root_cp_pool(2)
      g%fom_p_pool(3,layer) = temp_c3(layer) / p%root_cp_pool(3)

      g%fom_cp_pool(1,layer) = p%root_cp_pool(1)
      g%fom_cp_pool(2,layer) = p%root_cp_pool(2)
      g%fom_cp_pool(3,layer) = p%root_cp_pool(3)
   end do

   call pop_routine (myname)
   return
end subroutine



! ====================================================================
subroutine soilp_min_hum ()
! ====================================================================
   Use infrastructure
   implicit none

!+  Purpose
!      Mineralise P from hum pool

!+  Mission Statement
!      Mineralise P from humic pool

!+  Constant Values
   character*(*) myname               ! name of current procedure
   parameter (myname = 'soilp_min_hum')

!+  Local Variables
   integer layer                    ! layer counter
   integer num_layers
   real    tot_hum_c_decomposed
   real    hum_p_decomposed
   real    min_p

!- Implementation Section ----------------------------------
   call push_routine (myname)

   num_layers = count_of_real_vals (g%dlayer , max_layer)

   do layer = 1, num_layers
      ! calculate total c decomposed from hum pool
      tot_hum_c_decomposed = g%dlt_hum_c_atm (layer) +g%dlt_hum_c_biom (layer)

      ! calculate p decomposed from hum pool
      hum_p_decomposed = tot_hum_c_decomposed / c%hum_cp

      ! calculate p mineralised from hum pool
      min_p = hum_p_decomposed -g%dlt_hum_c_biom (layer) / c%biom_cp

      ! update pools
      g%hum_p (layer) = g%hum_p (layer) - hum_p_decomposed
      g%biom_p (layer) = g%biom_p (layer) +g%dlt_hum_c_biom (layer) / c%biom_cp
      g%labile_p (layer) = g%labile_p (layer) + min_p
   end do

   call pop_routine (myname)
   return
end subroutine



! ====================================================================
subroutine soilp_min_fom ()
! ====================================================================
   Use infrastructure
   implicit none

!+  Purpose
!      Mineralise P from fom pool

!+  Mission Statement
!      Mineralise P from fom pool

!+  Constant Values
   character*(*) myname               ! name of current procedure
   parameter (myname = 'soilp_min_fom')

!+  Local Variables
   integer layer                    ! layer counter
   integer num_layers
   real    tot_fom_c_decomposed
   real    fom_p_decomposed
   real    min_p
   real    fom_p_decomp_pool1    !  fom P decomposed from pool1
   real    fom_p_decomp_pool2    !  fom P decomposed from pool2
   real    fom_p_decomp_pool3    !  fom P decomposed from pool3

!- Implementation Section ----------------------------------
   call push_routine (myname)

   num_layers = count_of_real_vals (g%dlayer , max_layer)

   do layer = 1, num_layers

      ! calculate total c decomposed from fom pool
      tot_fom_c_decomposed = g%dlt_fom_c_atm (layer) +g%dlt_fom_c_biom (layer) +g%dlt_fom_c_hum (layer)

      ! calculate p decomposed from fom pool
      fom_p_decomp_pool1 = g%dlt_fom_c_pool1(layer) /g%fom_cp_pool(1,layer)
      fom_p_decomp_pool2 = g%dlt_fom_c_pool2(layer) /g%fom_cp_pool(2,layer)
      fom_p_decomp_pool3 = g%dlt_fom_c_pool3(layer) /g%fom_cp_pool(3,layer)
      fom_p_decomposed = fom_p_decomp_pool1 +fom_p_decomp_pool2 +fom_p_decomp_pool3

      ! calculate p mineralised from fom pool
      min_p = fom_p_decomposed -g%dlt_fom_c_biom (layer) / c%biom_cp -g%dlt_fom_c_hum (layer)  / c%hum_cp

      ! bound_check (min_p, 0.0)

      ! update pools
      g%fom_p_pool(1,layer) = g%fom_p_pool(1,layer) -fom_p_decomp_pool1
      g%fom_p_pool(2,layer) = g%fom_p_pool(2,layer) -fom_p_decomp_pool2
      g%fom_p_pool(3,layer) = g%fom_p_pool(3,layer) -fom_p_decomp_pool3

      g%fom_p(layer) = g%fom_p_pool(1,layer) + g%fom_p_pool(2,layer) +g%fom_p_pool(3,layer)
      g%hum_p (layer) = g%hum_p (layer) +g%dlt_fom_c_hum (layer) / c%hum_cp
      g%biom_p (layer) = g%biom_p (layer) +g%dlt_fom_c_biom (layer) / c%biom_cp
      g%labile_p (layer) = g%labile_p (layer) + min_p

   end do

   call pop_routine (myname)
   return
end subroutine



! ====================================================================
subroutine soilp_min_biom ()
! ====================================================================
   Use infrastructure
   implicit none

!+  Purpose
!      Mineralise P from biom pool

!+  Mission Statement
!      Mineralise P from biom pool

!+  Constant Values
   character*(*) myname               ! name of current procedure
   parameter (myname = 'soilp_min_biom')

!+  Local Variables
   integer layer                    ! layer counter
   integer num_layers
   real    tot_biom_c_decomposed
   real    biom_p_decomposed
   real    min_p

!- Implementation Section ----------------------------------
   call push_routine (myname)

   num_layers = count_of_real_vals (g%dlayer , max_layer)

   do layer = 1, num_layers

      ! calculate total c decomposed from biom pool
      tot_biom_c_decomposed = g%dlt_biom_c_atm (layer) +g%dlt_biom_c_hum (layer)

      ! calculate p decomposed from biom pool
      biom_p_decomposed = tot_biom_c_decomposed / c%biom_cp

      ! calculate p mineralised from biom pool
      min_p = biom_p_decomposed -g%dlt_biom_c_hum (layer) / c%hum_cp

      ! update pools
      g%biom_p (layer) = g%biom_p (layer) - biom_p_decomposed
      g%hum_p (layer) = g%hum_p (layer) +g%dlt_biom_c_hum (layer) / c%hum_cp
      g%labile_p (layer) = g%labile_p (layer) + min_p

   end do

   call pop_routine (myname)
   return
end subroutine



! ====================================================================
subroutine soilp_min_residues ()
! ====================================================================
   Use infrastructure
   implicit none

!+  Purpose
!      Mineralise P from fom pool

!+  Mission Statement
!      Mineralise residues

!+  Constant Values
   character*(*) myname               ! name of current procedure
   parameter (myname = 'soilp_min_residues')

!+  Local Variables
   integer layer                    ! layer counter
   integer num_layers               ! number of soil layers
   integer numvals                  ! number of values
   real    res_p_decomposed
   real    min_p
   real    dlt_org_p      (max_layer) ! delta organic p
   real    dlt_res_c_hum  (max_layer) ! delta c from res to hum
   real    dlt_res_c_biom (max_layer) ! delta c from res to biom
   real    dlt_res_c_atm  (max_layer) ! delta c from res to atm

!- Implementation Section ----------------------------------
   call push_routine (myname)

   dlt_res_c_atm = 0.0
   dlt_res_c_hum = 0.0
   dlt_res_c_biom = 0.0
   dlt_org_p = 0.0

   call collect_real_array ('dlt_res_c_atm',  max_layer,  '()',  dlt_res_c_atm,  numvals,  0.0,  1000.0)
   call collect_real_array ('dlt_res_c_hum',  max_layer,  '()',  dlt_res_c_hum,  numvals,  0.0,  1000.0)
   call collect_real_array ('dlt_res_c_biom',  max_layer,  '()',  dlt_res_c_biom,  numvals,  0.0,  1000.0)
   call collect_real_array ('dlt_org_p',  max_layer,  '()',  dlt_org_p,  numvals,  0.0,  1000.0)

   num_layers = count_of_real_vals (dlt_res_c_biom , max_layer)

   ! calculate p decomposed from residues
   res_p_decomposed = sum_real_array (dlt_org_p, num_layers)

   ! calculate p mineralised from residues
   min_p = res_p_decomposed -sum_real_array(dlt_res_c_biom, num_layers)/c%biom_cp -sum_real_array(dlt_res_c_hum, num_layers)/c%hum_cp

   ! bound_check (min_p, 0.0)

   ! update pools
   do layer = 1, num_layers

      g%hum_p(layer) = g%hum_p(layer) +dlt_res_c_hum(layer) / c%hum_cp
      g%biom_p(layer)= g%biom_p(layer) +dlt_res_c_biom(layer) / c%biom_cp

      ! mineralised P distributed to layers in same proportion as carbon converted to soil biomass.
      g%labile_p (layer) = g%labile_p (layer) +min_p * divide(dlt_res_c_biom (layer),sum_real_array(dlt_res_c_biom, num_layers), 0.0)

   end do

   call soilp_bound_check (num_layers)

   call pop_routine (myname)
   return
end subroutine



! ====================================================================
subroutine soilp_incorp_residues ()
! ====================================================================
   Use infrastructure
   implicit none

!+  Purpose
!      Transfers P from surface residues to fom pool.
!      Presently assumes residues c:p is invariant.

!+  Mission Statement
!      Incorporate residues

!+  Constant Values
   character*(*) myname               ! name of current procedure
   parameter (myname = 'soilp_incorp_residues')

!+  Local Variables
   character  err_string*80           ! Error message string
   real dlt_fom_incorp (max_layer)
   real dlt_fom_p_incorp (max_layer)
   real dlt_fom_cpr_incorp (max_layer)
   real dlt_labile_p (max_layer)
   real fom_c_pool1(max_layer)!   C in fom pool 1
   real fom_c_pool2(max_layer)!   C in fom pool 2
   real fom_c_pool3(max_layer)!   C in fom pool 3
   real dlt_fom_p_pool1(max_layer) ! change in p in pool1
   real dlt_fom_p_pool2(max_layer) ! change in p in pool2
   real dlt_fom_p_pool3(max_layer) ! change in p in pool3
   integer numval_fom
   integer numval_p
   integer numval_cpr
   integer numvals
   integer numvals1
   integer numvals2
   integer numvals3
   integer layer                      ! layer counter
   integer i
   character flag*5
   integer num_layers

!- Implementation Section ----------------------------------
   call push_routine (myname)

   dlt_fom_incorp(:) = 0.0
   dlt_fom_p_incorp(:) = 0.0
   dlt_fom_cpr_incorp(:) = 0.0
   dlt_labile_p(:) = 0.0
   dlt_fom_p_pool1(:) = 0.0
   dlt_fom_p_pool2(:) = 0.0
   dlt_fom_p_pool3(:) = 0.0

   call collect_real_array_optional ('dlt_fom_p_pool1', max_layer, '(kg/ha)', dlt_fom_p_pool1, numvals1, -100000.0, 100000.0)
   call collect_real_array_optional ('dlt_fom_p_pool2', max_layer, '(kg/ha)', dlt_fom_p_pool2, numvals2, -100000.0, 100000.0)
   call collect_real_array_optional ('dlt_fom_p_pool3', max_layer, '(kg/ha)', dlt_fom_p_pool3, numvals3, -100000.0, 100000.0)

   numvals = numvals1 + numvals2 + numvals3

   !dsg if the above arrays are sent, then we don't need anything else.
   !     ie. go straight to the pool incrementing section
   !     if, however, the above arrays are not sent then we must partition
   !     the P into fractions in each layer.  We will do this by assuming
   !     that the CP ratios of all fractions are equal


   !dsg start of big loop
   if(numvals.eq.0) then

      call collect_real_array ('dlt_fom_wt', max_layer, '(kg/ha)', dlt_fom_incorp, numval_fom, 0.0, 10000.0)
      call collect_real_array_optional ('dlt_fom_p', max_layer, '(kg/ha)', dlt_fom_p_incorp, numval_p, 0.0, 10000.0)

      if (numval_p.eq.0) then

         call collect_real_array_optional ('dlt_fom_cpr', max_layer, '()', dlt_fom_cpr_incorp, numval_cpr, 0.0, 10000.0)

         do layer = 1, numval_cpr
            dlt_fom_p_incorp(layer) = divide (dlt_fom_incorp(layer)* residue_c_frac, dlt_fom_cpr_incorp(layer), 0.0)
         end do
      else
      endif

      if (numval_p .eq.0 .and. numval_cpr.eq.0) then

         ! Use constant C:P for roots etc
         call collect_char_var_optional ('use_default_p', '()', flag, numvals)

         if ((flag.eq.'true').or.(numvals.eq.0)) then
            do layer = 1, numval_fom
               dlt_fom_p_incorp(layer) = divide (dlt_fom_incorp(layer)* residue_c_frac, p%root_cp, 0.0)
            end do
         else
          ! data will come in a later message
          dlt_fom_p_incorp(:) = 0.0
         endif
      else
      ! all ok
      endif

      ! dsg get fraction info from soiln2 as a function of fom_type
      ! NIH fractions now scalar (as sent from soiln2)  fractions will eventually
      ! be sent with data on flow of OM (ie we send amount and fractions together in an event)
      call Get_Integer_var(unknown_module, 'num_fom_types', '()',  g%num_fom_types, numvals, 0, 100)
      call Get_real_var (unknown_module,'fr_carb','()',c%fr_carb,numvals,0.0,1000.)
      call Get_real_var (unknown_module,'fr_cell','()',c%fr_cell,numvals,0.0,1000.)
      call Get_real_var (unknown_module,'fr_lign','()',c%fr_lign,numvals,0.0,1000.)

      do layer = 1, numval_fom
         ! dsg  break into pools assuming default fom_type (1).  The facility
         !       is there to use the other types, however the type name is
         !        currently not sent with the event.
         dlt_fom_p_pool1(layer)=dlt_fom_p_incorp(layer)*c%fr_carb
         dlt_fom_p_pool2(layer)=dlt_fom_p_incorp(layer)*c%fr_cell
         dlt_fom_p_pool3(layer)=dlt_fom_p_incorp(layer)*c%fr_lign
      end do

   !dsg  end of big loop
   endif

   !dsg need to get g%fom_c_pools from soiln2 to calculate cp ratios
   call Get_real_array (unknown_module,'fom_c_pool1',max_layer,'()',fom_c_pool1,numvals,0.0,5000.)
   call Get_real_array (unknown_module,'fom_c_pool2',max_layer,'()',fom_c_pool2,numvals,0.0,5000.)
   call Get_real_array (unknown_module,'fom_c_pool3',max_layer,'()',fom_c_pool3,numvals,0.0,5000.)
   num_layers = count_of_real_vals(g%dlayer,max_layer)

   do layer = 1, num_layers
      ! now update fom P information
      g%fom_p_pool(1,layer) = g%fom_p_pool(1,layer) +dlt_fom_p_pool1(layer)
      g%fom_p_pool(2,layer) = g%fom_p_pool(2,layer) +dlt_fom_p_pool2(layer)
      g%fom_p_pool(3,layer) = g%fom_p_pool(3,layer) +dlt_fom_p_pool3(layer)

      g%fom_p (layer) = g%fom_p_pool(1,layer) + g%fom_p_pool(2,layer)+ g%fom_p_pool(3,layer)

      g%fom_cp_pool(1,layer) =  divide(fom_c_pool1(layer),g%fom_p_pool(1,layer),0.0)
      g%fom_cp_pool(2,layer) =  divide(fom_c_pool2(layer),g%fom_p_pool(2,layer),0.0)
      g%fom_cp_pool(3,layer) =  divide(fom_c_pool3(layer),g%fom_p_pool(3,layer),0.0)

      g%fom_cp(layer) = divide((fom_c_pool1(layer)+fom_c_pool2(layer) +fom_c_pool3(layer)),(g%fom_p_pool(1,layer)+g%fom_p_pool(2,layer) + g%fom_p_pool(3,layer)),0.0)
   end do


   ! now do mineral P
   call collect_real_array_optional ('dlt_labile_p', max_layer, '(kg/ha)', dlt_labile_p, numval_p, 0.0, 10000.0)

   do layer = 1, numval_p
      g%labile_p(layer) = g%labile_p(layer) + dlt_labile_p(layer)
   end do

   call soilp_bound_check (numval_p)

   call pop_routine (myname)
   return
end subroutine



!     ================================================================
subroutine soilp_Sum_Report ()
!     ================================================================
   Use infrastructure
   implicit none

!+  Purpose
!      Output P module summary details.

!+  Mission Statement
!      Report module details

!+  Constant Values
   character*(*) my_name
   parameter (my_name = 'soilp_sum_report')

!+  Local Variables
   character string*300
   integer   num_layers             ! number of layers
   integer   layer                  ! layer counter

!- Implementation Section ----------------------------------

   call push_routine (my_name)

   call write_string (new_line//new_line)

   string = '              Phosphorus Initial Data'
   call write_string (string)

   string ='     ---------------------------------------------------------'
   call write_string (string)

   string ='      Layer    Labile  Unavailable'//' Banded     Rock    Sorption'
   call write_string (string)

   string ='               (kg/ha)   (kg/ha)   (kg/ha)   (kg/ha)     () '
   call write_string (string)

   string ='     ---------------------------------------------------------'
   call write_string (string)

   num_layers = count_of_real_vals(g%dlayer,max_layer)
   do layer = 1,num_layers
      write (string, '(5x, i4, 3x, f8.1, 4f10.1)')layer, g%labile_p(layer), g%unavail_p(layer), g%banded_p (layer), g%rock_p (layer), p%sorption (layer)

      call write_string (string)
   end do

   string ='     ---------------------------------------------------------'
   call write_string (string)

   write (string, '(6x, ''Totals'', f8.1, 4f10.1)')sum_real_array (g%labile_p, max_layer), sum_real_array (g%unavail_p, max_layer), sum_real_array (g%banded_p, max_layer), sum_real_array (g%rock_p, max_layer)

   call write_string ( string)

   string ='     ---------------------------------------------------------'
   call write_string (string)

   call write_string (new_line//new_line)

   write (string, '(6x, ''Dissolution rate of rock P :    '',f7.5, 3x, ''(/yr)'')')p%rate_dissol_rock_p
   call write_string (string)

   write (string, '(6x, ''Root C:P :                  '',f7.1, 3x )')p%root_cp
   call write_string (string)

   call pop_routine (my_name)
   return
end subroutine

! ====================================================================
subroutine soilp_incorp_residue_P ()
! ====================================================================
   Use infrastructure
   implicit none

!+  Purpose

!+  Mission Statement
!      Incorporate residue P

!+  Constant Values
   character*(*) myname               ! name of current procedure
   parameter (myname = 'soilp_incorp_residue_P')

!+  Local Variables
   character  err_string*80           ! Error message string
   real dlt_fom_p_incorp (max_layer)
   real dlt_labile_p (max_layer)
   real fom_c_pool1(max_layer)!   C in fom pool 1
   real fom_c_pool2(max_layer)!   C in fom pool 2
   real fom_c_pool3(max_layer)!   C in fom pool 3
   real dlt_fom_p_pool1(max_layer) ! change in p in pool1
   real dlt_fom_p_pool2(max_layer) ! change in p in pool2
   real dlt_fom_p_pool3(max_layer) ! change in p in pool3
   integer numval_fom
   integer numval_p
   integer numval_cpr
   integer numvals
   integer numvals1
   integer numvals2
   integer numvals3
   integer layer                      ! layer counter
   integer i

!- Implementation Section ----------------------------------
   call push_routine (myname)

   dlt_fom_p_incorp(:) = 0.0
   dlt_labile_p(:) = 0.0
   dlt_fom_p_pool1(:) = 0.0
   dlt_fom_p_pool2(:) = 0.0
   dlt_fom_p_pool3(:) = 0.0

   call collect_real_array_optional ('dlt_fom_p_pool1', max_layer, '(kg/ha)', dlt_fom_p_pool1, numvals1, -100000.0, 100000.0)
   call collect_real_array_optional ('dlt_fom_p_pool2', max_layer, '(kg/ha)', dlt_fom_p_pool2, numvals2, -100000.0, 100000.0)
   call collect_real_array_optional ('dlt_fom_p_pool3', max_layer, '(kg/ha)', dlt_fom_p_pool3, numvals3, -100000.0, 100000.0)
   numvals = numvals1 + numvals2 + numvals3

   if(numvals.eq.0) then
      call collect_real_array ('dlt_fom_p', max_layer, '(kg/ha)', dlt_fom_p_incorp, numval_p, 0.0, 10000.0)
      call Get_integer_var(unknown_module, 'num_fom_types', '()',  g%num_fom_types, numvals, 0, 100)
      call Get_real_var (unknown_module,'fr_carb','()',c%fr_carb,numvals,0.0,1000.)
      call Get_real_var (unknown_module,'fr_cell','()',c%fr_cell,numvals,0.0,1000.)
      call Get_real_var (unknown_module,'fr_lign','()',c%fr_lign,numvals,0.0,1000.)

      do layer = 1, numval_p
         dlt_fom_p_pool1(layer)=dlt_fom_p_incorp(layer)*c%fr_carb
         dlt_fom_p_pool2(layer)=dlt_fom_p_incorp(layer)*c%fr_cell
         dlt_fom_p_pool3(layer)=dlt_fom_p_incorp(layer)*c%fr_lign
      end do

   endif


   call Get_real_array (unknown_module,'fom_c_pool1',max_layer,'()',fom_c_pool1,numvals,0.0,5000.)
   call Get_real_array (unknown_module,'fom_c_pool2',max_layer,'()',fom_c_pool2,numvals,0.0,5000.)
   call Get_real_array (unknown_module,'fom_c_pool3',max_layer,'()',fom_c_pool3,numvals,0.0,5000.)

   do layer = 1, max_layer
      g%fom_p_pool(1,layer) = g%fom_p_pool(1,layer) +dlt_fom_p_pool1(layer)
      g%fom_p_pool(2,layer) = g%fom_p_pool(2,layer) +dlt_fom_p_pool2(layer)
      g%fom_p_pool(3,layer) = g%fom_p_pool(3,layer) +dlt_fom_p_pool3(layer)

      g%fom_p (layer) = g%fom_p_pool(1,layer) + g%fom_p_pool(2,layer)+ g%fom_p_pool(3,layer)

      g%fom_cp_pool(1,layer) =  divide(fom_c_pool1(layer),g%fom_p_pool(1,layer),0.0)
      g%fom_cp_pool(2,layer) =  divide(fom_c_pool2(layer),g%fom_p_pool(2,layer),0.0)
      g%fom_cp_pool(3,layer) =  divide(fom_c_pool3(layer),g%fom_p_pool(3,layer),0.0)

      g%fom_cp(layer) = divide((fom_c_pool1(layer)+fom_c_pool2(layer)+ fom_c_pool3(layer)),(g%fom_p_pool(1,layer)+g%fom_p_pool(2,layer) + g%fom_p_pool(3,layer)),0.0)
   end do

   ! now do mineral P
   call collect_real_array_optional ('dlt_labile_p', max_layer, '(kg/ha)', dlt_labile_p, numval_p, 0.0, 10000.0)

   do layer = 1, numval_p
      g%labile_p(layer) = g%labile_p(layer) + dlt_labile_p(layer)
   end do

   call soilp_bound_check (numval_p)

   call pop_routine (myname)
   return
end subroutine

end module SoilPModule

!     ===========================================================
subroutine alloc_dealloc_instance(doAllocate)
!     ===========================================================
   use SoilPModule
   implicit none
   ml_external alloc_dealloc_instance

!+  Sub-Program Arguments
   logical, intent(in) :: doAllocate

!+  Purpose
!      Module instantiation routine.

!- Implementation Section ----------------------------------

   if (doAllocate) then
      allocate(g)
      allocate(p)
      allocate(c)
      allocate(id)
   else
      deallocate(g)
      deallocate(p)
      deallocate(c)
      deallocate(id)
   end if
   return
end subroutine



! ====================================================================
subroutine Main (Action, Data_string)
! ====================================================================
   Use infrastructure
   Use SoilPModule
   implicit none
   ml_external Main

!+  Sub-Program Arguments
    character Action*(*)            ! Message action to perform
    character Data_string*(*)       ! Message data

!+  Purpose
!      This routine is the interface between the main system and the
!      soilp module.

!+  Mission Statement
!     Handles communications for Soilp

!+  Constant Values
   character  myname*(*)            ! name of this procedure
   parameter (myname = 'SoilP_main')


!- Implementation Section ----------------------------------

   call push_routine (myname)

   if (Action.eq.ACTION_Init) then
      call soilp_Init ()

   else if (Action.eq.ACTION_Create) then
      call doRegistrations(id)

   else if (Action.eq.ACTION_Sum_Report) then
      call soilp_sum_report ()

   else if (Action.eq.ACTION_Process) then
      call soilp_get_other_variables ()
      call soilp_process ()

   else if (Action.eq.ACTION_Get_variable) then
      call soilp_Send_my_variable (Data_string)

   else if (Action.eq.ACTION_Set_variable) then
      call soilp_Set_my_variable (Data_string)

   else if (Action.eq.ACTION_Till) then
      call soilp_tillage ()

   else if (Action.eq.ACTION_incorp_fom) then
      call soilp_incorp_residues ()

   else if (Action.eq.ACTION_incorp_fom_p) then
      call soilp_incorp_residue_p ()

   else if (Action.eq.ACTION_Decomposed) then
      call soilp_min_residues ()

   else
      ! Don't use message
      call Message_Unused ()
   endif

   call pop_routine (myname)
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