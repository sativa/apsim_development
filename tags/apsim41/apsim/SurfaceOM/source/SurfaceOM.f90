module SurfaceOMModule
   use ComponentInterfaceModule
   use Registrations
 
! ====================================================================
!     SurfaceOM constants
! ====================================================================

   integer    max_table                   ! number of points allowed for
   parameter (max_table = 10)             ! specifying coeff tables

   integer    max_layer
   parameter (max_layer = 100)

   integer    max_residues                ! maximum number of residues in at once
   parameter (max_residues = 100)

   integer    MaxStringSize           ! max length of the crop type.
   parameter (MaxStringSize = 100)

   integer    MaxArraySize                 ! maximum number of dry matter types
   parameter (MaxArraySize = 10)

   integer    MaxFr
   parameter (MaxFr = 3)

!     ================================================================

!  dsg 190803  The following two 'types' have been defined within the code because,
!              dean's datatypes.f90 generator cannot yet properly generate the
!              type definition from datatypes.interface for a structures within a
!              structure.
   type OMFractionType
     sequence
     real  amount
     real  C
     real  N
     real  P
     real  AshAlk
   end type OMFractionType

   type SurfOrganicMatterType
     sequence
     character name*(MaxStringSize)
     character OrganicMatterType*(MaxStringSize)
     real      PotDecompRate
     real      no3
     real      nh4
     real      po4
     type(OMFractionType),dimension (MaxFr)::Standing
     type(OMFractionType),dimension (MaxFr)::Lying
   end type SurfOrganicMatterType

   type SurfaceOMGlobals
      sequence
      type(SurfOrganicMatterType),dimension (max_residues)::SurfOM
      type(newmetType) :: MetData
      integer    num_surfom
      real       irrig
      real       init_resid_cnr
      real       init_resid_cpr
      real       eos
      real       cumeos
      real       dlayer (max_layer)
      real       leaching_fr
      logical    phosphorus_aware
   end type SurfaceOMGlobals
!     ================================================================
   type SurfaceOMParameters
      sequence
      real       standing_fraction(max_residues) ! standing fraction array
      character  report_additions*5
   end type SurfaceOMParameters
!     ================================================================
   type SurfaceOMConstants
      sequence
      character  fom_types(max_residues)*(MaxStringSize)
      integer    num_fom_types
      integer    cf_contrib(max_residues) ! determinant of whether a residue type
                                          ! contributes to the calculation of contact factor (1 or 0)
      real   cnrf_coeff                ! coeff for rate of change in decomp
                                       ! with C:N
      real   cnrf_optcn                ! C:N above which decomp is
                                       ! slowed.
      real   opt_temp                  ! temperature at which decomp
                                       ! reaches optimum (oC)
      real   cum_eos_max               ! cumeos at which decomp rate
                                       ! becomes zero. (mm H2O)
      real   C_fract(max_residues)     ! Fraction of Carbon in plant
                                       ! material (0-1)
      real   crit_residue_wt           ! critical residue weight below which
                                       ! Thorburn's cover factor equals one
      real   leach_rain_tot            ! total amount of 'leaching' rain to remove
                                       !      all soluble N from surfom
      real   min_rain_to_leach         ! threshold rainfall amount for leaching to occur
      real   crit_min_surfom_orgC         ! critical minimum org C below which potential
                                       ! decomposition rate is 100% (to avoid numerical imprecision)

      real   default_cpr               ! Default C:P ratio
      real   default_standing_fraction ! Default fraction of residue isolated from soil (standing up)

      real, dimension(MaxFr,max_residues)::  fr_pool_C   ! carbohydrate fraction in fom C pool (0-1)
      real, dimension(MaxFr,max_residues)::  fr_pool_N   ! carbohydrate fraction in fom N pool (0-1)
      real, dimension(MaxFr,max_residues)::  fr_pool_P   ! carbohydrate fraction in fom P pool (0-1)
      real  nh4ppm(max_residues)       ! ammonium component of residue (ppm)
      real  no3ppm(max_residues)       ! nitrate component of residue (ppm)
      real  po4ppm(max_residues)       ! ammonium component of residue (ppm)
      real  specific_area(max_residues)! specific area of residue (ha/kg)

      end type SurfaceOMConstants
!     ================================================================

   ! instance variables.
   common /InstancePointers/ ID,g,p,c
   save InstancePointers
   type (SurfaceOMGlobals),pointer :: g
   type (SurfaceOMParameters),pointer :: p
   type (SurfaceOMConstants),pointer :: c
   type (IDsType), pointer :: id


contains


!===========================================================
subroutine surfom_zero_all_globals ()
!===========================================================
   Use Infrastructure
   implicit none

!+  Purpose
!       Zero all global variables & arrays

!+  Constant Values
   character  my_name*(*)           ! name of procedure
   parameter (my_name  = 'surfom_zero_all_globals')

!- Implementation Section ----------------------------------

   call push_routine (my_name)

   ! NIH - there must be a lot of things missing here

         ! Globals
   g%MetData%rain         = 0.0
   g%irrig                = 0.0
   g%MetData%maxt         = 0.0
   g%MetData%mint         = 0.0
   g%init_resid_cnr       = 0.0
   g%init_resid_cpr       = 0.0
   g%eos                  = 0.0
   g%cumeos               = 0.0
   g%dlayer (:)           = 0.0
   g%phosphorus_aware    = .false.
   c%fom_types(:)        = blank
   c%num_fom_types       = 0

      ! Parameters
   p%report_additions    = blank

      ! Constants

   c%cnrf_coeff               = 0.0
   c%cnrf_optcn               = 0.0
   c%opt_temp                 = 0.0
   c%cum_eos_max              = 0.0
   c%C_fract                  = 0.0
   c%default_cpr              = 0.0


   call pop_routine (my_name)
   return
end subroutine



!================================================================
subroutine surfom_zero_event_data ()
!================================================================
   Use Infrastructure
   implicit none

!+  Purpose
!     Set all variables in this module to zero.

!+  Constant Values
   character*(*) my_name
   parameter (my_name = 'surfom_zero_event_data')

!- Implementation Section ----------------------------------
   call push_routine (my_name)

   g%MetData%maxt = 0.0
   g%MetData%mint = 0.0
   g%MetData%rain = 0.0

   call pop_routine (my_name)
   return
end subroutine



!================================================================
subroutine surfom_Reset ()
!================================================================
   Use Infrastructure
   implicit none

!+  Purpose
!      Initialise residue module

!+  Constant Values
   character*(*) my_name
   parameter (my_name = 'surfom_reset')

!- Implementation Section ----------------------------------

   call push_routine (my_name)

   call surfom_zero_variables ()
   call surfom_get_other_variables ()
   call surfom_read_coeff ()
   call surfom_read_param ()

   call pop_routine (my_name)
   return
end subroutine


!================================================================
subroutine surfom_zero_variables ()
!================================================================
   Use Infrastructure
   implicit none

!+  Purpose
!     Set all variables in this module to zero.

!+  Constant Values
   character*(*) my_name
   parameter (my_name = 'surfom_zero_variables')

!- Implementation Section ----------------------------------
   call push_routine (my_name)

   g%cumeos = 0.0
   g%irrig = 0.0
   g%init_resid_cnr = 0.0
   g%init_resid_cpr = 0.0
   g%eos = 0.0
   g%dlayer = 0.0

   g%phosphorus_aware = .false.
   p%report_additions = 'no'


   call pop_routine (my_name)
   return
end subroutine


!================================================================
subroutine surfom_get_other_variables ()
!================================================================
   Use Infrastructure
   implicit none

!+  Purpose
!      Get the values of variables from other modules

!+  Constant Values
   character*(*) my_name
   parameter (my_name = 'surfom_get_other_variables')

!+  Local Variables
   integer   numvals

!- Implementation Section ----------------------------------

   call push_routine (my_name)

   call Get_real_var(unknown_module, 'eos', '(mm)', g%eos, numvals, 0.0, 100.0)
   call Get_real_array(unknown_module, 'dlayer', max_layer, '(mm)',g%dlayer, numvals, 1.0, 1000.0)

   call pop_routine (my_name)
   return
end subroutine


!================================================================
subroutine surfom_read_coeff ()
!================================================================
   Use Infrastructure
   implicit none

!+  Purpose
!   Read module coefficients from coefficient file

!+  Constant Values
   character*(*) my_name             ! name of current procedure
   parameter (my_name = 'surfom_read_coeff')
!
   character*(*) Section_Name
   parameter (section_name = 'constants')

!+  Local Variables
   integer    numvals               ! number of values read from file

!- Implementation Section ----------------------------------
   call push_routine (my_name)

   call write_string (new_line//   '    - Reading constants')

   call read_real_var (section_name, 'opt_temp', '(oC)', c%opt_temp, numvals, 0.0, 100.0)
   call read_real_var (section_name, 'cum_eos_max', '(mm)', c%cum_eos_max, numvals, 0.0, 100.0)
   call read_real_var (section_name, 'cnrf_coeff', '()', c%cnrf_coeff, numvals, 0.0, 10.0)
   call read_real_var (section_name, 'cnrf_optcn', '()', c%cnrf_optcn, numvals, 5.0, 100.0)
   call read_real_var (section_name, 'crit_residue_wt', '()', c%Crit_residue_wt, numvals, 0.0, 1.0e7)
   call read_real_var (section_name, 'leach_rain_tot', '()', c%leach_rain_tot, numvals, 0.0, 100.0)
   call read_real_var (section_name, 'min_rain_to_leach', '()', c%min_rain_to_leach, numvals, 0.0, 100.0)
   call read_real_var (section_name, 'crit_min_surfom_orgC', '(kg/ha)', c%crit_min_surfom_orgC, numvals, 0.0, 10.)
   call read_real_var (section_name, 'default_cpr', '()', c%default_cpr, numvals, 0.0, 1000.)
   call read_real_var (section_name, 'default_standing_fraction', '()', c%default_standing_fraction, numvals, 0.0, 1.0)

   call pop_routine (my_name)
   return
end subroutine


!================================================================
subroutine surfom_read_param ()
!================================================================
   Use Infrastructure
   implicit none

!+  Purpose
!      Read in all parameters from parameter file.

!+  Constant Values
   character*(*) my_name
   parameter (my_name = 'surfom_read_param')
!
   character*(*) Section_name
   parameter (section_name = 'parameters')

!+  Local Variables
   integer   i                               ! simple counter
   integer   numvals1                        ! number of values read from file
   integer   cnr_pool_flag                   ! flag indicating whether cnr fpool info is provided
   integer   cpr_pool_flag                   ! flag indicating whether cpr fpool info is provided
   integer   residue_number                  ! number of residue type in the c%fom_types array
   character temp_type(max_residues)*32      ! temporary array for residue types
   character temp_name(max_residues)*32      ! temporary array for resiude names
   real      temp_wt(max_residues)
   character check_type*32                   ! 'check' name for type information
   real      temp_cnr_pool1(max_residues)    ! temporary cnr_pool1 array
   real      temp_cnr_pool2(max_residues)    ! temporary cnr_pool2 array
   real      temp_cnr_pool3(max_residues)    ! temporary cnr_pool3 array
   real      temp_residue_cnr(max_residues)  ! temporary residue_cnr array
   real      temp_cpr_pool1(max_residues)    ! temporary cpr_pool1 array
   real      temp_cpr_pool2(max_residues)    ! temporary cpr_pool1 array
   real      temp_cpr_pool3(max_residues)    ! temporary cpr_pool1 array
   real      temp_residue_cpr(max_residues)  ! temporary residue_cpr array
   real      temp_no3ppm(max_residues)       ! temporary nitrate array
   real      temp_nh4ppm(max_residues)       ! temporary ammonium array
   real      temp_po4ppm(max_residues)       ! temporary po4 array
   real      tot_c(max_residues)             ! total C in residue
   real      tot_n(max_residues)             ! total N in residue
   real      tot_p(max_residues)             ! total P in residue

          character err_string*200

!- Implementation Section ----------------------------------

   call push_routine (my_name)

   call write_string (new_line//'    - Reading parameters')


      ! Read in residue type from parameter file
      !         ------------
   call read_char_array (section_name, 'name', max_residues, '()', temp_name, g%num_surfom)
   call read_char_array (section_name, 'type', max_residues, '()', temp_type, numvals1)
   if (g%num_surfom.ne.numvals1) then
      call fatal_error (ERR_USER,'residue types and names do not match')
   endif

      ! Read in residue weight from parameter file
      !         --------------
   call read_real_array (section_name, 'mass', max_residues, '(kg/ha)', temp_wt, numvals1, 0.0, 20000.0)
   if (g%num_surfom.ne.numvals1) then
      call fatal_error (ERR_USER,'Number of residue names and weights do not match')
   endif


      ! Read in 'standing_fraction' from parameter file
      !         --------------
   call read_real_array_optional (section_name, 'standing_fraction', max_residues, '()', p%standing_fraction, numvals1, 0.0, 1.0)
   if (numvals1.eq.0) then
      p%standing_fraction(:) = c%default_standing_fraction
   endif

      ! Read in Nutrient Information from parameter file
      !         --------------------
    call read_real_array (section_name, 'cnr ', max_residues, '()', temp_residue_cnr, numvals1, 0.0, 200.0)
    call read_real_array_optional (section_name, 'cpr ', max_residues, '()', temp_residue_cpr, numvals1, 0.0, 200.0)
    if (numvals1.eq.0) then
        temp_residue_cpr(:) = c%default_cpr
    else
       g%phosphorus_aware = .true.
    endif


   call read_char_var_optional (section_name, 'report_additions', '()', p%report_additions, numvals1)
   if (numvals1.eq.0) then
      p%report_additions = 'no'
   else
   endif



   ! NOW, PUT ALL THIS INFO INTO THE 'SurfaceOM' STRUCTURE
   do  i = 1,g%num_surfom

     ! collect relevant type-specific constants from the ini-file
     call surfom_read_type_specific_constants(temp_type(i),i)

      g%SurfOM(i)%name = temp_name(i)
      g%SurfOM(i)%OrganicMatterType = temp_type(i)

      ! convert the ppm figures into kg/ha
      g%SurfOM(i)%no3 = c%no3ppm(i)/1000000.0* temp_wt(i)
      g%SurfOM(i)%nh4 = c%nh4ppm(i)/1000000.0* temp_wt(i)
      g%SurfOM(i)%po4 = c%po4ppm(i)/1000000.0* temp_wt(i)

      tot_c(i) = temp_wt(i) * c%c_fract(i)
      tot_n(i) = divide(tot_c(i),temp_residue_cnr(i),0.0)
      tot_p(i) = divide(tot_c(i),temp_residue_cpr(i),0.0)

      g%SurfOM(i)%Standing(1:MaxFr)%amount =temp_wt(i)*c%fr_pool_c(1:MaxFr,i) * p%standing_fraction(i)
      g%SurfOM(i)%Standing(1:MaxFr)%C =tot_c(i)* c%fr_pool_c(1:MaxFr,i) * p%standing_fraction(i)
      g%SurfOM(i)%Standing(1:MaxFr)%N =tot_n(i)* c%fr_pool_n(1:MaxFr,i) * p%standing_fraction(i)
      g%SurfOM(i)%Standing(1:MaxFr)%P =tot_p(i)* c%fr_pool_p(1:MaxFr,i) * p%standing_fraction(i)
      g%SurfOM(i)%Standing(1:MaxFr)%AshAlk = 0.0

      g%SurfOM(i)%Lying(1:MaxFr)%amount =temp_wt(i) * c%fr_pool_c(1:MaxFr,i)* (1 - p%standing_fraction(i))
      g%SurfOM(i)%Lying(1:MaxFr)%C =tot_c(i)* c%fr_pool_c(1:MaxFr,i) * (1 - p%standing_fraction(i))
      g%SurfOM(i)%Lying(1:MaxFr)%N =tot_n(i)* c%fr_pool_n(1:MaxFr,i) * (1 - p%standing_fraction(i))
      g%SurfOM(i)%Lying(1:MaxFr)%P =tot_p(i)* c%fr_pool_p(1:MaxFr,i) * (1 - p%standing_fraction(i))
      g%SurfOM(i)%Lying(1:MaxFr)%AshAlk = 0.0

   end do

   call surfom_Sum_Report ()

   call pop_routine (my_name)
   return
end subroutine


! ====================================================================
    integer function surfom_number (surfomname)
! ====================================================================
   Use infrastructure
   implicit none


!+  Sub-Program Arguments
    character surfomname*(*)

!+  Purpose
!     Get the solutes number

!+  Constant Values
   character myname*(*)               ! name of current procedure
   parameter (myname = 'surfom_number')

!+  Local Variables
    integer counter
    integer resnum

!- Implementation Section ----------------------------------
   call push_routine (myname)

   resnum = 0
   do  counter = 1, g%num_surfom
      if (g%SurfOM(counter)%name.eq.surfomname) then
         resnum = counter
         exit
      else
      endif
   end do

   surfom_number = resnum

   call pop_routine (myname)
   return
end function


!===========================================================
subroutine surfom_ONnewmet (variant)
!===========================================================
   Use infrastructure
   implicit none

   integer, intent(in) :: variant

!+  Purpose
!     Get new met data

!+  Local Variables
   type(newmetType) :: newmet
   integer numvals

!+  Constant Values
   character*(*) myname               ! name of current procedure
   parameter (myname = 'surfom_ONnewmet')

!- Implementation Section ----------------------------------
   call push_routine (myname)

   call unpack_newmet(variant, g%MetData)

   call pop_routine (myname)
   return
end subroutine



!================================================================
subroutine surfom_Pot_Decomp (C_decomp, N_decomp, P_decomp)
!================================================================
   Use infrastructure
   implicit none

!+  Sub-Program Arguments
   real      C_decomp(max_residues) ! (OUTPUT)
   real      N_decomp(max_residues) ! (OUTPUT)
   real      P_decomp(max_residues) ! (OUTPUT)

!+  Purpose
!   Performs manure decomposition taking into account environmental
!   and manure factors (independant to soil N but N balance can modify
!   actual decomposition rates if desired by N model - this is possible
!   because pools are not updated until end of time step - see post routine)

!+  Constant Values
   character*(*) my_name
   parameter (my_name = 'surfom_pot_decomp')

!+  Local Variables
   integer   residue                ! simple residue counter
   real      Fdecomp                ! decomposition fraction for the given surfom
   real      mf                     ! moisture factor for decomp (0-1)
   real      tf                     ! temperature factor for decomp (0-1)
   real      cf                     ! manure/soil contact factor for decomp (0-1)
   real      cnrf                   ! C:N factor for decomp (0-1) for surfom under consideration
   character  Err_string*80         ! Error message string

!- Implementation Section ----------------------------------
   call push_routine (my_name)

      ! calculate environmental factors on decomposition
   mf = surfom_wf ()
   tf = surfom_tf ()
   cf = surfom_cf ()

   do residue = 1,g%num_surfom
      cnrf = surfom_cnrf (residue)

      if (sum(g%SurfOM(residue)%Lying(1:MaxFr)%C).lt.c%crit_min_surfom_orgC) then
         ! Residue wt is sufficiently low to suggest decomposing all
         ! material to avoid low numbers which can cause problems
         ! with numerical precision
         Fdecomp = 1.0
      else
         ! Calculate today's decomposition  as a fraction of potential rate
         Fdecomp = g%SurfOM(residue)%PotDecompRate *mf*tf*cnrf*cf
      endif


      ! Now calculate pool decompositions for this residue

      C_decomp(residue) = Fdecomp* sum(g%SurfOM(residue)%Lying(1:MaxFr)%C)
      N_decomp(residue) = Fdecomp* sum(g%SurfOM(residue)%Lying(1:MaxFr)%N)
      P_decomp(residue) = Fdecomp* sum(g%SurfOM(residue)%Lying(1:MaxFr)%P)


   end do

   call pop_routine (my_name)
   return
end subroutine


!================================================================
   real function surfom_tf ()
!================================================================
   use infrastructure
   implicit none

!+  Purpose
!   Calculate temperature factor for manure decomposition (0-1).

!+  Notes
!   The temperature factor is a simple function of the square of daily
!   average temperature.  The user only needs to give an optimum temperature
!   and the code will back calculate the necessary coefficient at compile
!   time.

!+  Constant Values
   character*(*) my_name             !  name of current procedure
   parameter (my_name = 'surfom_tf')

!+  Local Variables
   real       ave_temp              ! today's average air temp (oC)
   real       tf                    ! temperature factor

!- Implementation Section ----------------------------------
   call push_routine (my_name)

   ave_temp = (g%MetData%maxt + g%MetData%mint)/2.0

   if (ave_temp.gt.0.0) then
      tf = divide (ave_temp, c%opt_temp, 0.0)**2.0
      tf = bound (tf, 0.0, 1.0)
   else
      ! it is too cold for decomposition
      tf = 0.0
   endif

   surfom_tf = tf

   call pop_routine (my_name)
   return
end function



!================================================================
   real function surfom_cf ()
!================================================================
   Use infrastructure
   implicit none

!+  Purpose
!   Calculate manure/soil contact factor for manure decomposition (0-1).

!+  Constant Values
   character*(*) my_name             ! name of current procedure
   parameter (my_name = 'surfom_cf')

!+  Local Variables
   real       cf                    ! manure/soil contact factor for
                                    ! decomposition (0-1)
   real       eff_surfom_wt          ! Total residue wt across all instances
   integer    residue               ! counter for surfom instances

!- Implementation Section ----------------------------------
   call push_routine (my_name)

   ! Sum the effective mass of surface residues considering lying fraction only.
   ! The 'effective' weight takes into account the haystack effect and is governed by the
   ! cf_contrib factor (ini file).  ie some residue types do not contribute to the haystack effect.

   eff_surfom_wt= 0
   do residue = 1, g%num_surfom
     eff_surfom_wt = eff_surfom_wt + sum(g%SurfOM(residue)%Lying(1:MaxFr)%amount) * c%cf_contrib(residue)
   end do

   if (eff_surfom_wt.le.c%crit_residue_wt) then
      cf = 1.0
   else
      cf = divide(c%crit_residue_wt,eff_surfom_wt,0.0)
      cf = bound (cf, 0.0, 1.0)
   endif

   surfom_cf = cf

   call pop_routine (my_name)
   return
end function



!================================================================
   real function surfom_cnrf (residue)
!================================================================
   Use infrastructure
   implicit none

!+  Sub-Program Arguments
   integer residue                ! residue number(INPUT)

!+  Purpose
!   Calculate C:N factor for decomposition (0-1).

!+  Constant Values
   character*(*) my_name             ! name of current procedure
   parameter (my_name = 'surfom_cnrf')

!+  Local Variables
   real       cnr               ! C:N for this residue  (unitless)
   real       cnrf              ! C:N factor for decomposition(0-1)
   real       total_c           ! organic C component of this residue (kg/ha)
   real       total_n           ! organic N component of this residue (kg/ha)
   real       total_mineral_n   ! mineral N component of this surfom (no3+nh4)(kg/ha)

!- Implementation Section ----------------------------------
   call push_routine (my_name)

   ! Note: C:N ratio factor only based on lying fraction
   total_C = sum(g%SurfOM(residue)%Lying(1:MaxFr)%C)
   total_N = sum(g%SurfOM(residue)%Lying(1:MaxFr)%N)
   total_mineral_n = g%SurfOM(residue)%no3 + g%SurfOM(residue)%nh4
   cnr= divide(total_C,(total_N+total_mineral_n),0.0)

   ! As C:N increases above optcn cnrf decreases exponentially toward zero
   ! As C:N decreases below optcn cnrf is constrained to one

   if (c%cnrf_optcn.eq.0) then
      cnrf = 1.0
   else
      cnrf = exp ( - c%cnrf_coeff * (cnr - c%cnrf_optcn)/ c%cnrf_optcn)
   endif

   cnrf = bound (cnrf, 0.0, 1.0)

   surfom_cnrf = cnrf

   call pop_routine (my_name)
   return
end function


!================================================================
   real function surfom_wf ()
!================================================================
   Use infrastructure
   implicit none

!+  Purpose
!   Calculate moisture factor for manure decomposition (0-1).

!+  Constant Values
   character*(*) my_name             ! name of current procedure
   parameter (my_name = 'surfom_wf')

!+  Local Variables
   real      mf                     ! moisture factor for decomp (0-1)

!- Implementation Section ----------------------------------
   call push_routine (my_name)

   ! moisture factor decreases from 1. at start of g%cumeos and decreases
   ! linearly to zero at cum_eos_max

   mf = 1.0 - divide (g%cumeos, c%cum_eos_max, 0.0)

   mf = bound(mf, 0.0, 1.0)
   surfom_wf = mf


   call pop_routine (my_name)
   return
end function



!================================================================
subroutine surfom_Process ()
!================================================================
   Use infrastructure
   implicit none

!+  Purpose
!      Perform actions for current day.

!+  Constant Values
   character*(*) my_name
   parameter (my_name = 'surfom_process')

!+  Local Variables
   real leach_rain          ! 'leaching' rainfall (if rain>10mm)


!- Implementation Section ----------------------------------

   call push_routine (my_name)

   call surfom_set_phosphorus_aware ()

   call surfom_set_vars (g%cumeos, leach_rain)

   if (leach_rain .gt.0.0) then
      call surfom_leach (leach_rain)
   else
       ! no mineral N or P is leached today
   endif

   call surfom_Send_Potdecomp_event()

   call pop_routine (my_name)
   return
end subroutine


!================================================================
subroutine surfom_set_vars (cumeos, leach_rain)
!================================================================
   Use infrastructure
   implicit none

!+  Sub-Program Arguments
   real       cumeos                ! (OUTPUT)
   real       leach_rain            ! (OUTPUT)

!+  Purpose
!   Calculates variables required for today's decomposition and
!   leaching factors.

!+  Constant Values
   character*(*) my_name             ! name of current procedure
   parameter (my_name = 'surfom_set_vars')

!+  Local Variables
   real       precip                ! daily precipitation (g%rain +
                                    ! irrigation) (mm H2O)

!- Implementation Section ----------------------------------
   call push_routine (my_name)

   precip = g%MetData%rain + g%irrig

   if (precip.gt. 4.0) then
      ! reset cumulative to just today's g%eos
      cumeos = g%eos - precip

   else
      ! keep accumulating g%eos
      cumeos = g%cumeos + g%eos - precip

   endif
   cumeos = l_bound (cumeos, 0.0)

   if (precip .ge. c%min_rain_to_leach) then
      leach_rain  = precip
   else
      leach_rain = 0.0
   endif

   ! reset irrigation log now that we have used that information
   g%irrig = 0.0

   call pop_routine (my_name)
   return
end subroutine


!================================================================
subroutine surfom_Leach (leach_rain)
!================================================================
   Use infrastructure
   implicit none

!+  Sub-Program Arguments
   real  leach_rain                ! (INPUT)

!+  Purpose
!      Remove mineral N and P from surfom with leaching rainfall and
!      pass to Soil N and Soil P modules.

!+  Constant Values
   character*(*) my_name
   parameter (my_name = 'surfom_leach')

!+  Local Variables
      real    nh4_incorp(max_layer)
      real    no3_incorp(max_layer)
      real    po4_incorp(max_layer)
      integer deepest_layer

!- Implementation Section ----------------------------------
   call push_routine (my_name)

   no3_incorp(:) = 0.0
   nh4_incorp(:) = 0.0
   po4_incorp(:) = 0.0

   !  Calculate Leaching Fraction
   g%leaching_fr = divide(leach_rain,c%leach_rain_tot,0.0)
   g%leaching_fr = bound(g%leaching_fr,0.0,1.0)


   ! Apply leaching fraction to all mineral pools
   ! Put all mineral NO3,NH4 and PO4 into top layer
   no3_incorp(1) = sum(g%SurfOM(1:g%Num_surfom)%no3) * g%leaching_fr
   nh4_incorp(1) = sum(g%SurfOM(1:g%Num_surfom)%nh4) * g%leaching_fr
   po4_incorp(1) = sum(g%SurfOM(1:g%Num_surfom)%po4) * g%leaching_fr

   ! If neccessary, Send the mineral N & P leached to the Soil N&P modules
   if(no3_incorp(1).gt.0.0.or.nh4_incorp(1).gt.0.0.or.po4_incorp(1).gt.0.0) then
       deepest_layer = count_of_real_vals (g%dlayer, max_layer)
       call set_real_array (unknown_module, 'dlt_no3','(kg/ha)', no3_incorp,deepest_layer)
       call set_real_array (unknown_module, 'dlt_nh4','(kg/ha)', nh4_incorp,deepest_layer)
       if ( g%phosphorus_aware ) then
          call set_real_array (unknown_module, 'dlt_labile_p','(kg/ha)', po4_incorp,deepest_layer)
       else
       endif
   endif

   ! Update globals
   g%SurfOM(1:g%Num_surfom)%no3=g%SurfOM(1:g%Num_surfom)%no3 * (1.-g%Leaching_Fr)
   g%SurfOM(1:g%Num_surfom)%nh4=g%SurfOM(1:g%Num_surfom)%nh4 * (1.-g%Leaching_Fr)
   g%SurfOM(1:g%Num_surfom)%po4=g%SurfOM(1:g%Num_surfom)%po4 * (1.-g%Leaching_Fr)

   call pop_routine (my_name)
   return
end subroutine

! ====================================================================
 subroutine surfom_Send_PotDecomp_Event ()
! ====================================================================
   Use infrastructure
   implicit none

!+  Purpose
!     Notify other modules of the potential to decompose.

!+  Local Variables
   integer  residue                      ! simple surfom counter
   real   Pot_C_decomp(max_residues)     ! array containing the potential C decomposition for each residue
   real   Pot_N_decomp(max_residues)     ! array containing the potential N decomposition for each residue
   real   Pot_P_decomp(max_residues)     ! array containing the potential P decomposition for each residue
   type (SurfaceOrganicMatterDecompType), dimension(max_residues)::SOMDecomp

!+  Constant Values
   character*(*) myname               ! name of current procedure
   parameter (myname = 'surfom_Send_PotDecomp_Event')

!- Implementation Section ----------------------------------
   call push_routine (myname)

   call surfom_Pot_Decomp (Pot_C_decomp,Pot_N_decomp,Pot_P_decomp)

   do residue = 1, g%num_surfom
       SOMDecomp(residue)%name = g%SurfOM(residue)%name
       SOMDecomp(residue)%OrganicMatterType =g%SurfOM(residue)%OrganicMatterType
       SOMDecomp(residue)%amount = divide(Pot_C_decomp(residue),c%c_fract(residue),0.0)
       SOMDecomp(residue)%C = Pot_C_decomp(residue)
       SOMDecomp(residue)%N = Pot_N_decomp(residue)
       SOMDecomp(residue)%P = Pot_P_decomp(residue)
       SOMDecomp(residue)%AshAlk = 0.0
   end do

   call publish_SurfaceOrganicMatterDecomp(id%PotentialResidueDecompositionCalculated, SOMDecomp, g%num_surfom)


   call pop_routine (myname)
   return
end subroutine


! ====================================================================
 subroutine surfom_ONirrigated ()
! ====================================================================
   Use infrastructure
   implicit none

!+  Purpose
!     Get irrigation information from an Irrigated event.

!+  Local Variables
   real   amount
   integer numvals

!+  Constant Values
   character*(*) myname               ! name of current procedure
   parameter (myname = 'surfom_ONirrigated')

!- Implementation Section ----------------------------------
   call push_routine (myname)

   call collect_real_var (DATA_irrigate_amount,'(mm)',amount,numvals,0.0,1000.)

   ! now increment internal irrigation log
   g%irrig = g%irrig + amount

   call pop_routine (myname)
   return
end subroutine

! ====================================================================
subroutine surfom_decompose_surfom (variant)
! ====================================================================
   Use Infrastructure
   implicit none

!+  Purpose
!     <insert here>

!+  Sub-Program Arguments
   integer, intent(in) :: variant

!+  Constant Values
   character*(*) myname               ! name of current procedure
   parameter (myname = 'surfom_decompose_surfom')

!+  Local Variables
   integer numvals                   ! number of values from postbox
   integer layer                     ! layer counter
   integer num_surfom                   ! local surfom counter from received event
   integer residue_no                ! Index into the global array
   integer counter
   real c_pot_decomp(max_residues)   ! pot amount of C to decompose (kg/ha)
   real n_pot_decomp(max_residues)   ! pot amount of N to decompose (kg/ha)
   real p_pot_decomp(max_residues)   ! pot amount of P to decompose (kg/ha)
   real tot_c_decomp                 ! total amount of c to decompose
   real tot_n_decomp                 ! total amount of c to decompose
   real tot_p_decomp                 ! total amount of c to decompose
   real       residue_incorp_fraction(max_layer)
   real       dlt_residue_fraction
   real       dlt_res_c_decomp(max_layer)
   integer    deepest_layer
   type (SurfaceOrganicMatterDecompType), dimension(max_residues)::SOMDecomp
   real SOMcnr
   real SOMc
   real SOMn
   character  Err_string*200         ! Error message string

!- Implementation Section ----------------------------------
   call push_routine (myname)

   call unpack_SurfaceOrganicMatterDecomp(variant,SOMDecomp,num_surfom)

   ! calculate potential decompostion of C, N, and P
   call surfom_pot_decomp (c_pot_decomp,n_pot_decomp,p_pot_decomp)

   do counter = 1, num_surfom

      ! Determine which residue pool corresponds to this index in the array
      residue_no = surfom_number(SOMDecomp(counter)%name)

      ! Collect actual decompostion of C and N from supplying module (soiln2)
      tot_c_decomp = SOMDecomp(counter)%C
      tot_n_decomp = SOMDecomp(counter)%N

      call Bound_check_real_var (tot_n_decomp, 0.0, n_pot_decomp(residue_no), 'total n decompostion')

      ! check if C:N ratio of decomposed material is correct
      SOMc = sum(g%SurfOM(residue_no)%Standing(1:MaxFr)%c) + sum(g%SurfOM(residue_no)%Lying(1:MaxFr)%c)
      SOMn = sum(g%SurfOM(residue_no)%Standing(1:MaxFr)%n) + sum(g%SurfOM(residue_no)%Lying(1:MaxFr)%n)
      SOMcnr = divide(SOMc,SOMn,0.0)

      if (reals_are_equal(tot_c_decomp,0.0).and.reals_are_equal(tot_n_decomp,0.0)) then
            ! all OK - nothing happening

      elseif (tot_c_decomp .gt.c_pot_decomp(residue_no)+ error_margin(c_pot_decomp(residue_no)))then


            call fatal_error (Err_internal,'C decomposition exceeds potential rate')

      elseif (tot_n_decomp .gt.n_pot_decomp(residue_no)+ error_margin(n_pot_decomp(residue_no)))then
            call fatal_error (Err_internal,'N decomposition exceeds potential rate')

        ! NIH - If both the following tests are empty then they can both be deleted.

      elseif(reals_are_equal(divide(tot_c_decomp,tot_n_decomp,0.0),SOMcnr)) then
           !all ok - decomposition and residue pool have same C:N

      else
!               call fatal_error (Err_internal,
!     :                 'C:N ratio of decomposed residues is not valid')
      endif


      !calculate total p decomposition
      tot_p_decomp = tot_c_decomp * divide(p_pot_decomp(residue_no),c_pot_decomp(residue_no),0.0)

      ! Do actual decomposing - update pools for C, N, and P
      call surfom_decomp (tot_c_decomp,tot_n_decomp,tot_p_decomp,residue_no)

   end do

   call pop_routine (myname)
   return
end subroutine

!================================================================
subroutine surfom_Decomp (C_decomp, N_decomp, P_decomp,residue)
!================================================================
   Use infrastructure
   implicit none

!+  Sub-Program Arguments
   real      C_decomp               ! (Input) C to be decomposed
   real      N_decomp               ! (Input) N to be decomposed
   real      P_decomp               ! (Input) P to be decomposed
   integer   residue                ! residue number being dealt with

!+  Purpose
!   Performs updating of pools due to surfom decomposition

!+  Constant Values
   character*(*) my_name
   parameter (my_name = 'surfom_decomp')

!+  Local Variables
    character err_string*100

   real       Fdecomp         ! decomposing fraction
   real       lying_c         ! total Carbon in the 'lying fraction' for this residue (kg/ha)
   real       lying_n         ! total Nitrogen in the 'lying fraction' for this residue (kg/ha)
   real       lying_p         ! total Phosphorus in the 'lying fraction' for this residue (kg/ha)

!- Implementation Section ----------------------------------
   call push_routine (my_name)

!______________________________________________________________________

   lying_c =   sum(g%SurfOM(residue)%Lying(1:MaxFr)%C)
   Fdecomp = divide(C_decomp,lying_c,0.0)
   g%SurfOM(residue)%Lying(1:MaxFr)%C =   g%SurfOM(residue)%Lying(1:MaxFr)%C * (1 - Fdecomp)
   g%SurfOM(residue)%Lying(1:MaxFr)%amount =   g%SurfOM(residue)%Lying(1:MaxFr)%amount * (1 - Fdecomp)

!______________________________________________________________________

   lying_n =   sum(g%SurfOM(residue)%Lying(1:MaxFr)%N)
   Fdecomp = divide(N_decomp,lying_n,0.0)
   g%SurfOM(residue)%Lying(1:MaxFr)%N =   g%SurfOM(residue)%Lying(1:MaxFr)%N * (1 - Fdecomp)

!______________________________________________________________________

   lying_p =   sum(g%SurfOM(residue)%Lying(1:MaxFr)%P)
   Fdecomp = divide(P_decomp,lying_p,0.0)
   g%SurfOM(residue)%Lying(1:MaxFr)%P =   g%SurfOM(residue)%Lying(1:MaxFr)%P * (1 - Fdecomp)

   call pop_routine (my_name)
   return
end subroutine

!================================================================
subroutine surfom_Tillage ()
!================================================================
   Use Infrastructure
   implicit none

!+  Purpose
!   Calculates surfom incorporation as a result of tillage operations.

!+  Constant Values
   character*(*) my_name             ! name of current procedure
   parameter (my_name = 'surfom_tillage')
!
   character*(*) Tillage_section    ! section name for tillage info in
   parameter (Tillage_section = 'tillage') ! lookup file

!+  Local Variables
   character String*300             ! message string
   real      F_incorp               ! Fraction of residue incorporated
                                    ! by tillage. (0-1)
   character till_type*30                ! name of implement used for tillage
   real      type_info(2)           ! Array containing information about
                                    ! a certain type (from table)
   integer   Numvals                ! Number of values found in data string
   integer   Numvals_F              ! Number of values found in data string
   integer   Numvals_T              ! Number of values found in data string
   real      Tillage_depth          ! depth of residue incorp (mm)

!- Implementation Section ----------------------------------
   call push_routine (my_name)


   ! ----------------------------------------------------------
   !       Get User defined tillage effects on residue
   ! ----------------------------------------------------------
   call collect_char_var ('type', '()', till_type, numvals)
   call collect_real_var_optional ('f_incorp', '()', f_incorp, numvals_f, 0.0, 1.0)
   call collect_real_var_optional ('tillage_depth', '()', tillage_depth, numvals_t, 0.0, 1000.0)

   ! ----------------------------------------------------------
   !    If no user defined characteristics then use the
   !      lookup table compiled from expert knowledge
   ! ----------------------------------------------------------
   If (Numvals_t .eq.0 .or. numvals_f.eq.0) then
      call write_string (new_line//'    - Reading residue tillage info')

      call read_real_array_optional (tillage_section, till_type, 2, '()', type_info, numvals, 0.0, 1000.0)

      ! If we still have no values then stop
      If (numvals.ne.2) then
         ! We have an unspecified tillage type
         f_incorp = 0.0
         tillage_depth = 0.0
         string = 'Cannot find info for tillage:- '//till_type
         call FATAL_ERROR (ERR_user, string)

      Else
         F_incorp = type_info(1)
         Tillage_depth = type_info(2)

      Endif
   Else
   Endif

   ! ----------------------------------------------------------
   !              Now incorporate the residues
   ! ----------------------------------------------------------
   Call surfom_incorp (till_type, F_incorp, Tillage_Depth)

   Write (string, '(3a,40x,a,f8.2,a,40x,a, f8.2)' )   &
         'Residue removed using ', till_type, New_Line   &
        ,'Fraction Incorporated = ', F_incorp, New_Line   &
        ,'Incorporated Depth    = ', Tillage_Depth

   call Write_string (string)

   call pop_routine (my_name)
   return
end subroutine

!================================================================
subroutine surfom_incorp (action_type, F_incorp, Tillage_depth)
!================================================================
   Use Infrastructure
   implicit none

!+  Sub-Program Arguments
   character  action_type*30                ! name of implement used for tillage
   real       F_incorp
   real       Tillage_Depth

!+  Purpose
!   Calculate surfom incorporation as a result of tillage and update
!   residue and N pools.

!+  Notes
!   I do not like updating the pools here but we need to be able to handle
!   the case of multiple tillage events per day.

!+  Constant Values
   character*(*) my_name             ! name of current procedure
   parameter (my_name = 'surfom_incorp')

!+  Local Variables
   character*200   message          !
   real       cum_depth             !
   integer    Deepest_Layer         !
   real       Depth_to_go           !
   real       F_incorp_layer        !
   real       residue_incorp_fraction(max_layer)
   integer    layer                 !
   integer    residue
   integer    pool
   real       layer_incorp_depth    !
   real       C_pool(MaxFr,max_layer)      ! total C in each Om fraction and layer (from all surfOM's) incorporated
   real       N_pool(MaxFr,max_layer)      ! total N in each Om fraction and layer (from all surfOM's) incorporated
   real       P_pool(MaxFr,max_layer)      ! total P in each Om fraction and layer (from all surfOM's) incorporated
   real       AshAlk_pool(MaxFr,max_layer) ! total AshAlk in each Om fraction and layer (from all surfOM's) incorporated
   real       no3(max_layer)               ! total no3 to go into each soil layer (from all surfOM's)
   real       nh4(max_layer)               ! total nh4 to go into each soil layer (from all surfOM's)
   real       po4(max_layer)               ! total po4 to go into each soil layer (from all surfOM's)
   type (FPoolProfileLayerType), dimension(max_layer)::FPoolProfileLayer


!- Implementation Section ----------------------------------
   call push_routine (my_name)

   F_incorp = bound (F_incorp, 0.0, 1.0)

   Deepest_Layer = get_cumulative_index_real (Tillage_depth, g%dlayer, max_layer)

   C_Pool(:,:) = 0.0
   N_Pool(:,:) = 0.0
   P_Pool(:,:) = 0.0
   AshAlk_Pool(:,:) = 0.0
   no3(:)=0.0
   nh4(:)=0.0
   po4(:)=0.0

   cum_depth = 0.0

   do layer = 1, Deepest_Layer

      do residue = 1,g%num_surfom

         depth_to_go = tillage_depth - cum_depth
         layer_incorp_depth = min (depth_to_go, g%dlayer(layer))
         F_incorp_layer = divide (layer_incorp_depth, tillage_depth, 0.0)

         C_pool(1:MaxFr,layer) = C_pool(1:MaxFr,layer)+ (g%SurfOM(residue)%Lying(1:MaxFr)%C &
                               + g%SurfOM(residue)%Standing(1:MaxFr)%C) * F_incorp * F_incorp_layer
         N_pool(1:MaxFr,layer) = N_pool(1:MaxFr,layer)+ (g%SurfOM(residue)%Lying(1:MaxFr)%N &
                               + g%SurfOM(residue)%Standing(1:MaxFr)%N) * F_incorp * F_incorp_layer
         P_pool(1:MaxFr,layer) = P_pool(1:MaxFr,layer)+ (g%SurfOM(residue)%Lying(1:MaxFr)%P &
                               + g%SurfOM(residue)%Standing(1:MaxFr)%P) * F_incorp * F_incorp_layer
         AshAlk_pool(1:MaxFr,layer) = AshAlk_pool(1:MaxFr,layer)+ (g%SurfOM(residue)%Lying(1:MaxFr)%AshAlk &
                               + g%SurfOM(residue)%Standing(1:MaxFr)%AshAlk) * F_incorp * F_incorp_layer
         no3(layer) = no3(layer) + g%SurfOM(residue)%no3 * F_incorp * F_incorp_layer
         nh4(layer) = nh4(layer) + g%SurfOM(residue)%nh4 * F_incorp * F_incorp_layer
         po4(layer) = po4(layer) + g%SurfOM(residue)%po4 * F_incorp * F_incorp_layer
      end do

      cum_depth = cum_depth + g%dlayer(layer)

      ! dsg 160104  Remove the following variable after Res_removed_Event is scrapped
      residue_incorp_fraction(layer) = F_incorp_layer

   end do

   if (sum(C_pool(1:MaxFr,1:Max_layer)) .gt. 0.0) then

      ! Pack up the incorporation info and send to SOILN2 and SOILP as part of a
      ! FreshOrganicMatterIncorporated Event

      do layer = 1, Deepest_Layer

         FPoolProfileLayer(layer)%thickness = g%dlayer(layer)
         FPoolProfileLayer(layer)%no3 = no3(layer)
         FPoolProfileLayer(layer)%nh4 = nh4(layer)
         FPoolProfileLayer(layer)%po4 = po4(layer)
         FPoolProfileLayer(layer)%num_fpool = 3
         FPoolProfileLayer(layer)%fpool(1:MaxFr)%C = C_pool(1:MaxFr,layer)
         FPoolProfileLayer(layer)%fpool(1:MaxFr)%N = N_pool(1:MaxFr,layer)
         FPoolProfileLayer(layer)%fpool(1:MaxFr)%P = P_pool(1:MaxFr,layer)
         FPoolProfileLayer(layer)%fpool(1:MaxFr)%AshAlk = AshAlk_pool(1:MaxFr,layer)

      end do

      call publish_FPoolProfileLayer(id%FreshOrganicMatterIncorporated, FPoolProfileLayer, Deepest_Layer)
      ! dsg 160104  Keep this event for the time being - will be replaced by ResidueChanged
      call residue2_Send_Res_removed_Event(action_type, F_incorp, residue_incorp_fraction, deepest_layer)

   else
      ! no residue incorporated
   endif


   ! Now update globals.  They must be updated here because there is the possibility of
   ! more than one incorporation on any given day

   do pool = 1,3

      g%SurfOM(:)%Lying(pool)%amount =g%SurfOM(:)%Lying(pool)%amount * (1-F_incorp)
      g%SurfOM(:)%Standing(pool)%amount =g%SurfOM(:)%Standing(pool)%amount * (1-F_incorp)

      g%SurfOM(:)%Lying(pool)%C =g%SurfOM(:)%Lying(pool)%C * (1-F_incorp)
      g%SurfOM(:)%Standing(pool)%C =g%SurfOM(:)%Standing(pool)%C * (1-F_incorp)

      g%SurfOM(:)%Lying(pool)%N =g%SurfOM(:)%Lying(pool)%N * (1-F_incorp)
      g%SurfOM(:)%Standing(pool)%N =g%SurfOM(:)%Standing(pool)%N * (1-F_incorp)

      g%SurfOM(:)%Lying(pool)%P =g%SurfOM(:)%Lying(pool)%P * (1-F_incorp)
      g%SurfOM(:)%Standing(pool)%P =g%SurfOM(:)%Standing(pool)%P * (1-F_incorp)

      g%SurfOM(:)%Lying(pool)%AshAlk =g%SurfOM(:)%Lying(pool)%AshAlk * (1-F_incorp)
      g%SurfOM(:)%Standing(pool)%AshAlk =g%SurfOM(:)%Standing(pool)%AshAlk * (1-F_incorp)

   end do

      g%SurfOM(:)%no3 = g%SurfOM(:)%no3 * (1-F_incorp)
      g%SurfOM(:)%nh4 = g%SurfOM(:)%nh4 * (1-F_incorp)
      g%SurfOM(:)%po4 = g%SurfOM(:)%po4 * (1-F_incorp)

   call pop_routine (my_name)
   return
end subroutine

!================================================================
subroutine surfom_add_surfom ()
!================================================================
   Use Infrastructure
   implicit none

!+  Purpose
!   Calculates surfom addition as a result of add_surfom message

!+  Constant Values
   character*(*) my_name               ! name of current procedure
   parameter (my_name = 'surfom_add_surfom')
!
!+  Local Variables
   character  Err_string*300         ! Error message string
   integer    numvals               ! number of values read from file
   integer    numval_n              ! number of N values read from file
   integer    numval_cnr            ! number of cnr values read from file
   integer    numval_p              ! number of N values read from file
   integer    numval_cpr            ! number of cnr values read from file
   integer    residue               ! residue counter
   integer    SOMNo     ! specific system number for this residue name
   character  surfom_name*30        ! unique 'name' of residue to be added ()
   character  surfom_type*30        ! 'type' of residue to be added ()
   real       surfom_added          ! Mass of new surfom added (kg/ha)
   real       surfom_n_added        ! N added in new material (kg/ha)
   real       surfom_cnr_added      ! C:N ratio of new material
   real       surfom_p_added        ! P added in new material (kg/ha)
   real       surfom_cpr_added      ! C:P ratio of new material
   real       added_wt

!- Implementation Section ----------------------------------
   call push_routine (my_name)

   call collect_char_var ('name', '()', surfom_name, numvals)


   SOMNo = surfom_number(surfom_name)
   if (SOMNo.eq.0) then
      ! This is a new type - build internal record for it

       g%num_surfom = g%num_surfom + 1
       SOMNo = g%num_surfom
       g%SurfOM(SOMNo)%name = surfom_name

       surfom_type = ' '
       call collect_char_var ('type','()',g%SurfOM(SOMNo)%OrganicMatterType, numvals)

       g%SurfOM(SOMNo)%PotDecompRate=0.0
       g%SurfOM(SOMNo)%no3 =0.0
       g%SurfOM(SOMNo)%nh4 =0.0
       g%SurfOM(SOMNo)%po4 =0.0
       g%SurfOM(SOMNo)%Standing(:)%amount = 0.0
       g%SurfOM(SOMNo)%Standing(:)%C = 0.0
       g%SurfOM(SOMNo)%Standing(:)%N = 0.0
       g%SurfOM(SOMNo)%Standing(:)%P = 0.0
       g%SurfOM(SOMNo)%Standing(:)%AshAlk = 0.0
       g%SurfOM(SOMNo)%Lying(:)%amount = 0.0
       g%SurfOM(SOMNo)%Lying(:)%C = 0.0
       g%SurfOM(SOMNo)%Lying(:)%N = 0.0
       g%SurfOM(SOMNo)%Lying(:)%P = 0.0
       g%SurfOM(SOMNo)%Lying(:)%AshAlk = 0.0

        call surfom_read_type_specific_constants(g%SurfOM(SOMNo)%OrganicMatterType,SOMNo)

   else
      ! This type already exists
   endif

   ! Get Mass of material added
   call collect_real_var ('mass', '(kg/ha)', surfom_added, numvals, -100000.0, 100000.0)

   ! Get N content of material added
   call collect_real_var_optional ('n', '(kg/ha)', surfom_N_added, numval_n, -10000.0, 10000.0)
   if (numval_n.eq.0) then
      call collect_real_var_optional ('cnr', '()', surfom_cnr_added, numval_cnr, 0.0, 10000.0)
      surfom_N_added = divide ((surfom_added * c%C_fract(SOMNo)), surfom_cnr_added, 0.0)

      ! If no N info provided, and no cnr info provided then throw error
      if (numval_cnr.eq.0) then
         Err_string = 'SurfaceOM N or SurfaceOM CN ratio not specified.'
         call Fatal_ERROR (ERR_user, Err_string)
      else
         ! all ok
      endif
   else
   endif

   ! collect P information from this new member
   surfom_p_added = 0.0
   call collect_real_var_optional ('p', '(kg/ha)', surfom_p_added, numval_p, -10000.0, 10000.0)
   if (numval_p.eq.0) then
      surfom_cpr_added = 0.0
      call collect_real_var_optional ('cpr', '()', surfom_cpr_added, numval_cpr, 0.0, 10000.0)
      surfom_P_added = divide ((surfom_added* c%C_fract(SOMNo)), surfom_cpr_added, 0.0)
      ! If no P info provided, and no cpr info provided then
      ! use default cpr and throw warning error to notify user
      If (numval_CPr .eq. 0) then
         surfom_p_added = divide ((surfom_added*c%C_fract(SOMNo)),c%default_cpr,0.0)
         Err_string = 'SurfOM P or SurfaceOM C:P ratio not specified - Default value applied.'
         call Warning_ERROR (ERR_user, Err_string)
      Endif
   else
   endif

   ! convert the ppm figures into kg/ha
   g%SurfOM(SOMNo)%no3 = g%SurfOM(SOMNo)%no3+  c%no3ppm(SOMNo)/ 1000000.0 * surfom_added
   g%SurfOM(SOMNo)%nh4 = g%SurfOM(SOMNo)%nh4+  c%nh4ppm(SOMNo)/ 1000000.0 * surfom_added
   g%SurfOM(SOMNo)%po4 = g%SurfOM(SOMNo)%po4+  c%po4ppm(SOMNo)/ 1000000.0 * surfom_added


   ! Assume all residue added is in the LYING pool, ie No STANDING component
   g%SurfOM(SOMNo)%Lying(1:MaxFr)%amount = g%SurfOM(SOMNo)%Lying(1:MaxFr)%amount + surfom_added   * c%fr_pool_c(1:MaxFr,SOMNo)
   g%SurfOM(SOMNo)%Lying(1:MaxFr)%C      = g%SurfOM(SOMNo)%Lying(1:MaxFr)%C      + surfom_added   * c%c_fract(SOMNo)* c%fr_pool_c(1:MaxFr,SOMNo)
   g%SurfOM(SOMNo)%Lying(1:MaxFr)%N      = g%SurfOM(SOMNo)%Lying(1:MaxFr)%N      + surfom_N_added * c%fr_pool_n(1:MaxFr,SOMNo)
   g%SurfOM(SOMNo)%Lying(1:MaxFr)%P      = g%SurfOM(SOMNo)%Lying(1:MaxFr)%P      + surfom_P_added * c%fr_pool_p(1:MaxFr,SOMNo)
   g%SurfOM(SOMNo)%Lying(1:MaxFr)%AshAlk = 0.0


   ! Report Additions
   if (p%report_additions.eq.'yes') then
      Write (Err_string,*)   &
           ' Added SurfaceOM', New_Line   &
          ,'SurfaceOM name         = '   &
          , trim(g%SurfOM(SOMNo)%name)   &
          , New_Line   &
          ,'SurfaceOM Type         = '   &
          , trim(g%SurfOM(SOMNo)%OrganicMatterType)   &
          , New_Line   &
          ,'Amount Added (kg/ha) = ', surfom_added, New_Line
      call Write_string (Err_string)
   else
      ! The user has asked for no reports for additions of surfom
      ! in the summary file.
   endif

   call residue2_Send_Res_added_Event (g%SurfOM(SOMNo)%OrganicMatterType   &
                                       , g%SurfOM(SOMNo)%OrganicMatterType   &
                                       , surfom_added   &
                                       , surfom_N_added   &
                                       , surfom_P_added)

   call pop_routine (my_name)
   return
end subroutine


!================================================================
subroutine surfom_read_type_specific_constants(surfom_type,i)
!================================================================
   Use Infrastructure
   implicit none

!+  Sub-Program Arguments
   character  surfom_type*30      ! type name of residue
   integer    i                   ! system number of this specific residue pool

!+  Purpose
!   Reads type-specific residue constants from ini-file and places them in c% constants

!+  Constant Values
   character*(*) my_name               ! name of current procedure
   parameter (my_name = 'surfom_read_type_specific_constants')
!
!+  Local Variables
   character check_type*32                   ! 'check' name for type information
   integer   numvals                         ! number of values returned
!- Implementation Section ----------------------------------
   call push_routine (my_name)


   ! check that there is 'type' information in the ini file for the specified 'type'
   call read_char_var (surfom_type, 'fom_type', '()', check_type, numvals)

   if (numvals.eq.0) then
      ! We do not have any data for the specified 'type'
      call fatal_error (ERR_USER,'No ini-file information available on specified residue-type')
   else

	   call read_real_var (surfom_type, 'fraction_C', '()', c%c_fract(i), numvals, 0.0, 1.0)
      call read_real_var (surfom_type, 'po4ppm', '(ppm)', c%po4ppm(i), numvals, 0.0, 1000.0)
	   call read_real_var (surfom_type, 'nh4ppm', '(ppm)', c%nh4ppm(i), numvals, 0.0, 1000.0)
	   call read_real_var (surfom_type, 'no3ppm', '(ppm)', c%no3ppm(i), numvals, 0.0, 1000.0)
	   call read_real_var (surfom_type, 'specific_area', '(ha/kg)', c%specific_area(i), numvals, 0.0, 0.01)
	   call read_integer_var (surfom_type, 'cf_contrib', '()', c%cf_contrib(i), numvals, 0, 1)

	   call read_real_var (surfom_type, 'pot_decomp_rate', '(/day)', g%SurfOM(i)%PotDecompRate, numvals, 0.0, 1.0)
	   call read_real_array (surfom_type, 'fr_c', MaxFr, '()', c%fr_pool_c(1:MaxFr,i), numvals, 0.0, 1.0)
	   call read_real_array (surfom_type, 'fr_n', MaxFr, '()', c%fr_pool_n(1:MaxFr,i), numvals, 0.0, 1.0)
	   call read_real_array (surfom_type, 'fr_p', MaxFr, '()', c%fr_pool_p(1:MaxFr,i), numvals, 0.0, 1.0)

   endif

   call pop_routine (my_name)
   return
end subroutine

!================================================================
subroutine surfom_Send_my_variable (Variable_name)
!================================================================
   Use Infrastructure
   implicit none

!+  Sub-Program Arguments
   character  Variable_name*(*)     ! (INPUT) Variable name to search for

!+  Purpose
!   return the value of one of our variables to caller

!+  Constant Values
   character*(*) my_name
   parameter (my_name = 'residue2_send_my_variable')

!+  Local Variables
   integer   i                      ! simple counter
   integer   surfnum                ! surfom number in system array
   real      cover1                 ! fraction of ground covered by residue (0-1)
   real      cover2                 ! fraction of ground covered by residue (0-1)
   real      combined_cover         ! effective combined cover from covers 1 & 2 (0-1)
   real      c_decomp(max_residues) ! C in decomposing material (kg/ha)
   real      n_decomp(max_residues) ! N in decomposing material (kg/ha)
   real      p_decomp(max_residues) ! P in decomposing material (kg/ha)
   real      factor                 ! modifying factor for decomposition (0-1)
   real      total_wt               ! summation of weights over all surfom's (kg/ha)
   real      total_c                ! summation of C over all surfom's (kg/ha)
   real      total_n                ! summation of N over all surfom's (kg/ha)
   real      total_p                ! summation of P over all surfom's (kg/ha)
   real      total_ashalk           ! summation of AshAlk over all surfom's (kg/ha)
   real      total_no3              ! summation of no3 pool over all surfom's (kg/ha)
   real      total_nh4              ! summation of nh4 pool over all surfom's (kg/ha)
   real      total_po4              ! summation of po4 pool over all surfom's (kg/ha)
   real      standing_fraction      ! fraction of surfom standing isolated from soil ()


!- Implementation Section ----------------------------------

   call push_routine (my_name)

   if (Variable_name .eq. 'surfaceom_wt') then
   !                       --------
      total_wt = 0.0
      do i = 1,g%num_surfom
          total_wt = total_wt + sum(g%SurfOM(i)%Standing(1:MaxFr)%amount) + sum(g%SurfOM(i)%Lying(1:MaxFr)%amount)
      end do
      call respond2get_real_var (variable_name, '(kg/ha)', total_wt)

   else if (Variable_name .eq. 'surfaceom_c') then
   !                       --------
      total_c = 0.0
      do i = 1,g%num_surfom
         total_c = total_c + sum(g%SurfOM(i)%Standing(1:MaxFr)%c) + sum(g%SurfOM(i)%Lying(1:MaxFr)%c)
      end do
      call respond2get_real_var (variable_name, '(kg/ha)', total_c)


   else if (Variable_name .eq. 'surfaceom_n') then
   !                       --------
      total_n = 0.0
      do i = 1,g%num_surfom
         total_n = total_n + sum(g%SurfOM(i)%Standing(1:MaxFr)%n) + sum(g%SurfOM(i)%Lying(1:MaxFr)%n)
      end do
      call respond2get_real_var (variable_name, '(kg/ha)', total_n)

   else if (Variable_name .eq. 'surfaceom_p') then
   !                       --------
      total_p = 0.0
      do i = 1,g%num_surfom
         total_p = total_p + sum(g%surfom(i)%Standing(1:MaxFr)%P) + sum(g%surfom(i)%Lying(1:MaxFr)%P)
      end do
      call respond2get_real_var (variable_name, '(kg/ha)', total_p)

   else if (Variable_name .eq. 'surfaceom_ashalk') then
   !                       --------
      total_ashalk = 0.0
      do i = 1,g%num_surfom
         Total_AshAlk = Total_AshAlk+ sum(g%surfom(i)%Standing(1:MaxFr)%AshAlk)+ sum(g%surfom(i)%Lying(1:MaxFr)%AshAlk)
      end do
      call respond2get_real_var (variable_name, '(kg/ha)', total_ashalk)


   else if (Variable_name .eq. 'surfaceom_no3') then
      total_no3 = 0.0
      do i = 1,g%num_surfom
         total_no3 = total_no3 + g%SurfOM(i)%no3
      end do
      call respond2get_real_var (variable_name, '(kg/ha)', total_no3)


   else if (Variable_name .eq. 'surfaceom_nh4') then
      total_nh4 = 0.0
      do i = 1,g%num_surfom
         total_nh4 = total_nh4 + g%SurfOM(i)%nh4
      end do
      call respond2get_real_var (variable_name, '(kg/ha)', total_nh4)

   else if (Variable_name .eq. 'surfaceom_labile_p') then
      total_po4 = 0.0
      do i = 1,g%num_surfom
         total_po4 = total_po4 + g%SurfOM(i)%po4
      end do
      call respond2get_real_var (variable_name, '(kg/ha)', total_po4)

   ! Individual surfom weights can be requested here in the format 'surfaceom_wt_wheat' (for example)
   !             where 'wheat' is the name of the surfom
   else if (index(Variable_name,'surfaceom_wt_').eq.1) then

      surfnum = surfom_number (Variable_name(14:))

      if (surfnum.gt.0)then
         total_wt =sum(g%SurfOM(surfnum)%Standing(1:MaxFr)%amount)+sum(g%SurfOM(surfnum)%Lying(1:MaxFr)%amount)
         call respond2Get_real_var (Variable_name,'(kg/ha)',total_wt)
      endif

   ! Individual surfom C can be requested here in the format 'surfaceom_c_wheat' (for example)
   ! where 'wheat' is the name of the surfom
   else if (index(Variable_name,'surfaceom_c_').eq.1) then

      surfnum = surfom_number (Variable_name(13:))
      if (surfnum.gt.0)then
         total_c =sum(g%SurfOM(surfnum)%Standing(1:MaxFr)%c)+sum(g%SurfOM(surfnum)%Lying(1:MaxFr)%c)
         call respond2Get_real_var (Variable_name,'(kg/ha)',total_c)
      endif


   ! Individual surfom N can be requested here in the format 'surfaceom_n_wheat' (for example)
   ! where 'wheat' is the name of the surfom
   else if (index(Variable_name,'surfaceom_n_').eq.1) then

      surfnum = surfom_number (Variable_name(13:))
      if (surfnum.gt.0)then
         total_n =sum(g%SurfOM(surfnum)%Standing(1:MaxFr)%n)+sum(g%SurfOM(surfnum)%Lying(1:MaxFr)%n)
         call respond2Get_real_var (Variable_name,'(kg/ha)',total_n)
      endif

   ! Individual surfom P can be requested here in the format 'surfaceom_p_wheat' (for example)
   ! where 'wheat' is the name of the surfom
   else if (index(Variable_name,'surfaceom_p_').eq.1) then
      surfnum = surfom_number (Variable_name(13:))
      if (surfnum.gt.0)then
         total_p =sum(g%surfom(surfnum)%Standing(1:MaxFr)%P)+ sum(g%surfom(surfnum)%Lying(1:MaxFr)%P)
         call respond2Get_real_var (Variable_name,'(kg/ha)',Total_P)
      endif


   ! Individual surfom AshAlk can be requested here in the format 'surfaceom_ashalk_wheat' (for example)
   ! where 'wheat' is the name of the surfom
   else if (index(Variable_name,'surfaceom_ashalk_').eq.1) then
      surfnum = surfom_number (Variable_name(18:))
      if (surfnum.gt.0)then
         total_AshAlk =sum(g%surfom(surfnum)%Standing(1:MaxFr)%AshAlk) + sum(g%surfom(surfnum)%Lying(1:MaxFr)%AshAlk)
         call respond2Get_real_var (Variable_name,'(kg/ha)',total_AshAlk)
      endif


   ! Individual surfom NO3 pool can be requested here in the format 'surfaceom_no3_wheat' (for example)
   ! where 'wheat' is the name of the surfom
   else if (index(Variable_name,'surfaceom_no3_').eq.1) then
      surfnum = surfom_number (Variable_name(15:))
      if (surfnum.gt.0)then
         call respond2Get_real_var (Variable_name,'(kg/ha)',g%SurfOM(surfnum)%no3)
      endif


   ! Individual surfom NH4 pool can be requested here in the format 'surfaceom_nh4_wheat' (for example)
   ! where 'wheat' is the name of the surfom
   else if (index(Variable_name,'surfaceom_nh4_').eq.1) then
      surfnum = surfom_number (Variable_name(15:))
      if (surfnum.gt.0)then
         call respond2Get_real_var (Variable_name,'(kg/ha)',g%SurfOM(surfnum)%nh4)
      endif


   ! Individual surfom PO4 pool can be requested here in the format 'surfaceom_labile_p_wheat' (for example)
   ! where 'wheat' is the name of the surfom
   else if (index(Variable_name,'surfaceom_labile_p_').eq.1) then
      surfnum = surfom_number (Variable_name(20:))
      if (surfnum.gt.0)then
         call respond2Get_real_var (Variable_name,'(kg/ha)',g%SurfOM(surfnum)%po4)
      endif


   ! Individual surfom C in pool1 can be requested here in the format 'surfaceom_c1_wheat' (for example)
   ! where 'wheat' is the name of the surfom
   else if (index(Variable_name,'surfaceom_c1_').eq.1) then
      surfnum = surfom_number (Variable_name(14:))
      if (surfnum.gt.0)then
         call respond2Get_real_var (Variable_name,'(kg/ha)',(g%SurfOM(surfnum)%Standing(1)%C+ g%SurfOM(surfnum)%Lying(1)%C))
      endif


   ! Individual surfom C in pool2 can be requested here in the format 'surfaceom_c2_wheat' (for example)
   ! where 'wheat' is the name of the surfom
   else if (index(Variable_name,'surfaceom_c2_').eq.1) then
      surfnum = surfom_number (Variable_name(14:))
      if (surfnum.gt.0)then
         call respond2Get_real_var (Variable_name,'(kg/ha)',(g%SurfOM(surfnum)%Standing(2)%C+ g%SurfOM(surfnum)%Lying(2)%C))
      endif


	! Individual surfom C in pool3 can be requested here in the format 'surfaceom_c3_wheat' (for example)
	! where 'wheat' is the name of the surfom
   else if (index(Variable_name,'surfaceom_c3_').eq.1) then
      surfnum = surfom_number (Variable_name(14:))
      if (surfnum.gt.0)then
         call respond2Get_real_var (Variable_name,'(kg/ha)',(g%SurfOM(surfnum)%Standing(3)%C+ g%SurfOM(surfnum)%Lying(3)%C))
      endif


	! Individual surfom N in pool1 can be requested here in the format 'surfaceom_n1_wheat' (for example)
	! where 'wheat' is the name of the surfom
   else if (index(Variable_name,'surfaceom_n1_').eq.1) then
      surfnum = surfom_number (Variable_name(14:))
      if (surfnum.gt.0)then
         call respond2Get_real_var (Variable_name,'(kg/ha)',(g%SurfOM(surfnum)%Standing(1)%N+ g%SurfOM(surfnum)%Lying(1)%N))
      endif


	! Individual surfom N in pool2 can be requested here in the format 'surfaceom_n2_wheat' (for example)
	! where 'wheat' is the name of the surfom
   else if (index(Variable_name,'surfaceom_n2_').eq.1) then
      surfnum = surfom_number (Variable_name(14:))
      if (surfnum.gt.0)then
         call respond2Get_real_var (Variable_name,'(kg/ha)',(g%SurfOM(surfnum)%Standing(2)%N+ g%SurfOM(surfnum)%Lying(2)%N))
      endif

	! Individual surfom N in pool3 can be requested here in the format 'surfaceom_n3_wheat' (for example)
	! where 'wheat' is the name of the surfom
   else if (index(Variable_name,'surfaceom_n3_').eq.1) then
      surfnum = surfom_number (Variable_name(14:))
      if (surfnum.gt.0)then
         call respond2Get_real_var (Variable_name,'(kg/ha)',(g%SurfOM(surfnum)%Standing(3)%N+ g%SurfOM(surfnum)%Lying(3)%N))
      endif


	! Individual surfom P in pool1 can be requested here in the format 'surfaceom_p1_wheat' (for example)
	! where 'wheat' is the name of the surfom
   else if (index(Variable_name,'surfaceom_p1_').eq.1) then
      surfnum = surfom_number (Variable_name(14:))
      if (surfnum.gt.0)then
         call respond2Get_real_var (Variable_name,'(kg/ha)',(g%SurfOM(surfnum)%Standing(1)%P+ g%SurfOM(surfnum)%Lying(1)%P))
      endif

	! Individual surfom P in pool2 can be requested here in the format 'surfaceom_p2_wheat' (for example)
	! where 'wheat' is the name of the surfom
   else if (index(Variable_name,'surfaceom_p2_').eq.1) then
      surfnum = surfom_number (Variable_name(14:))
      if (surfnum.gt.0)then
         call respond2Get_real_var (Variable_name,'(kg/ha)',(g%SurfOM(surfnum)%Standing(2)%P+ g%SurfOM(surfnum)%Lying(2)%P))
      endif


	! Individual surfom P in pool3 can be requested here in the format 'surfaceom_p3_wheat' (for example)
	! where 'wheat' is the name of the surfom
   else if (index(Variable_name,'surfaceom_p3_').eq.1) then
      surfnum = surfom_number (Variable_name(14:))
      if (surfnum.gt.0)then
         call respond2Get_real_var (Variable_name,'(kg/ha)',(g%SurfOM(surfnum)%Standing(3)%P+ g%SurfOM(surfnum)%Lying(3)%P))
      endif


	! Individual potential decompositions can be requested here in the
	! format 'pot_c_decomp_wheat' (for example), where 'wheat' is the name
	! of the surfom
   else if (index(variable_name,'pot_c_decomp_').eq.1) then
      surfnum = surfom_number (Variable_name(14:))
      call Surfom_Pot_Decomp (c_decomp, n_decomp,p_decomp)
      call respond2get_real_var (variable_name, '(kg/ha)', c_decomp(surfnum))

   else if (index(variable_name,'pot_n_decomp_').eq.1) then
      surfnum = surfom_number (Variable_name(14:))
      call Surfom_Pot_Decomp (c_decomp, n_decomp,p_decomp)
      call respond2get_real_var (variable_name, '(kg/ha)', n_decomp(surfnum))

   else if (index(variable_name,'pot_p_decomp_').eq.1) then
      surfnum = surfom_number (Variable_name(14:))
      call Surfom_Pot_Decomp (c_decomp, n_decomp,p_decomp)
      call respond2get_real_var (variable_name, '(kg/ha)', p_decomp(surfnum))

	! Standing fraction for individual surfom's
   else if (index(variable_name,'standing_fr_').eq.1) then
      surfnum = surfom_number (Variable_name(13:))
      standing_fraction = divide(g%SurfOM(surfnum)%Standing(1)%amount,(g%SurfOM(surfnum)%Standing(1)%amount+ g%SurfOM(surfnum)%Lying(1)%amount),0.0)
      call respond2get_real_var (variable_name, '()', standing_fraction)

	! The total cover is calculated here by combining all surfom's in system.
   else if (Variable_name .eq. 'surfaceom_cover') then
      cover1=0.0
      do i = 1,g%num_surfom
         cover2 = surfom_Cover(i)
         combined_cover = add_cover(cover1,cover2)
         cover1 = combined_cover
      end do
      call respond2get_real_var (variable_name, '(m^2/m^2)', combined_cover)

	! Individual surfom cover is reported here in the format 'surfaceom_cover_wheat'
   else if (index(variable_name,'surfaceom_cover_').eq.1) then
      surfnum = surfom_number (Variable_name(17:))
      cover1 = surfom_Cover(surfnum)
      call respond2get_real_var (variable_name, '(m^2/m^2)', cover1)

   else if (Variable_name .eq. 'tf') then
      factor = surfom_tf()
      call respond2get_real_var (variable_name, '(0-1)', factor)
   else if (Variable_name .eq. 'wf') then
      factor = surfom_wf()
      call respond2get_real_var (variable_name, '(0-1)', factor)

   else if (index(variable_name,'cnrf_').eq.1) then
      surfnum = 0
      surfnum = surfom_number (Variable_name(6:))
      factor = surfom_cnrf(surfnum)
      call respond2get_real_var (variable_name, '(0-1)', factor)

   else if (Variable_name .eq. 'cf') then
      factor = surfom_cf()
      call respond2get_real_var (variable_name, '(0-1)', factor)

   else if (Variable_name .eq. 'leaching_fr') then

      call respond2get_real_var (variable_name, '(0-1)', g%leaching_fr)

   else
      call Message_unused ()

   endif

   call pop_routine (my_name)
   return
end subroutine


!================================================================
real function Surfom_Cover (SOMindex)
!================================================================
   Use Infrastructure
   implicit none

!+  Sub-Program Arguments
   integer   SOMindex ! (INPUT)

!+  Purpose
!   This function returns the fraction of the soil surface covered by
!   residue according to the relationship from Gregory (1982).

!+  Notes
!   Gregory's equation is of the form
!           Fc = 1.0 - exp (- Am * M)   where Fc = Fraction covered
!                                             Am = Specific Area (ha/kg)
!                                              M = Mulching rate (kg/ha)
!   This residue model keeps track of the total residue area and so we can
!   substitute this value (area residue/unit area) for the product_of Am * M.

!+  Constant Values
   character*(*) my_name             ! name of current procedure
   parameter (my_name = 'Surfom_Cover')

!+  Local Variables
   real      F_Cover                ! Fraction of soil surface covered
                                    ! by residue (0-1)
   real      Area

!- Implementation Section ----------------------------------
   call push_routine (my_name)

   ! calculate fraction of cover and check bounds (0-1).  Bounds checking
   ! is required only for detecting internal rounding error.

   Area = c%specific_area(SOMindex)*(sum(g%surfOM(SOMindex)%Standing(1:MaxFr)%amount)   &
                                   + sum(g%surfOM(SOMindex)%Lying(1:MaxFr)%amount))

   F_Cover = 1.0 - exp (-Area)
   F_Cover = Bound (F_Cover, 0.0, 1.0)

   Surfom_Cover = F_Cover

   call pop_routine (my_name)
   return
end function

!================================================================
subroutine surfom_set_my_variable (Variable_name)
!================================================================
   Use Infrastructure
   implicit none

!+  Sub-Program Arguments
   character  Variable_name*(*)     ! (INPUT) Variable name to search for

!+  Purpose
!     Set one of our variables altered by some other module

!+  Constant Values
   character*(*) my_name
   parameter (my_name = 'surfom_set_my_variable')

!+  Local Variables
   integer   numvals                ! number of values returned

!- Implementation Section ----------------------------------

   call push_routine (my_name)

   if (Variable_name .eq. 'eos') then
      !                    ---
      call collect_real_var (variable_name,  '(mm)',  g%eos,  numvals,  0.0,  20.0)

   elseif (Variable_name .eq. 'dlayer') then
      call collect_real_array (variable_name,  max_layer,  '(mm)',  g%dlayer,  numvals,  0.0,  1000.0)

! dsg 150104  If this functionality is to be retained in SurfOM, then this needs to be modified so
!             as to specify which surfom pot_decomp_rate is being set.

!      elseif (Variable_name .eq. 'pot_decomp_rate') then
!
!         call collect_real_var (
!     :                variable_name    ! variable name
!     :         , '(day^-1)'           ! Units
!     :         , g%pot_decomp_rate    ! Variable
!     :         , numvals              ! Number of values returned
!     :         , 0.0                  ! Lower Limit for bound checking
!     :         , 1.0)                 ! Upper Limit for bound checking


   else
         ! Don't know this variable name
      call Message_unused ()
   endif

   call pop_routine (my_name)
   return
end subroutine


!================================================================
subroutine surfom_set_phosphorus_aware ()
!================================================================
   Use infrastructure
   implicit none

!+  Purpose
!      Check that soil phosphorus is in system

!+  Constant Values
   character*(*) my_name
   parameter (my_name = 'surfom_set_phosphorus_aware')

!+  Local Variables
   integer   numvals
   real labile_p(max_layer)      ! labile p from soil phosphorous

!- Implementation Section ----------------------------------

   call push_routine (my_name)
   numvals = 0
   call Get_real_array_optional(unknown_module, 'labile_p', max_layer, '(kg/ha)', labile_p, numvals, 1.0, 1000.0)

   if(numvals .gt. 0) then
      !manure is p aware
      g%phosphorus_aware = .true.
   else
      g%phosphorus_aware = .false.

   endif

   call pop_routine (my_name)
   return
end subroutine

!===========================================================
subroutine surfom_ON_Crop_chopped ()
!===========================================================
   Use Infrastructure
   implicit none

!+  Purpose
!     Get information on surfom added from the crops

!+  Local Variables
   character crop_type*(30)
   character dm_type(MaxArraySize)*(30)
   real      dlt_crop_dm(MaxArraySize)
   real      dlt_dm_N(MaxArraySize)
   real      dlt_dm_P(MaxArraySize)
   real      fraction_to_Residue(MaxArraySize)
   character  Err_string*100         ! Error message string
   character  Event_string*400      ! Event message string
   character  flag*5                ! p data flag
   integer    NumVals               ! number of values read from file
   integer    NumVal_dm             ! number of values read from file
   integer    NumVal_N              ! number of values read from file
   integer    NumVal_P              ! number of values read from file
   integer    SOMNo     ! system number of the surface organic matter added
   integer    residue               ! system surfom counter
   real       surfom_added          ! amount of residue added (kg/ha)
   real       surfom_N_added        ! amount of residue N added (kg/ha)
   real       surfom_P_added       ! amount of residue N added (kg/ha)

!+  Constant Values
   character  myname*(*)               ! name of current procedure
   parameter (myname = 'surfom_ON_Crop_chopped')

!- Implementation Section ----------------------------------
   call push_routine (myname)

   fraction_to_Residue(:) = 0.0
   call collect_real_array (DATA_fraction_to_Residue, MaxArraySize, '()', fraction_to_Residue, numvals, 0.0, 100000.0)

   if (sum(fraction_to_Residue) .eq. 0.0) then
      ! no surfom in this stuff
   else
	      call collect_char_var (DATA_crop_type, '()', crop_type, numvals)


	   ! Find the amount of surfom to be added today
	   dlt_crop_dm(:) = 0.0
	   call collect_real_array (DATA_dlt_crop_dm, MaxArraySize, '()', dlt_crop_dm, numval_dm, 0.0, 100000.0)
	   surfom_added = sum(dlt_crop_dm(:) * fraction_to_Residue(:))


	   ! Find the amount of N added in surfom today
	   dlt_dm_N(:) = 0.0
	   call collect_real_array(DATA_dlt_dm_n, MaxArraySize, '(kg/ha)', dlt_dm_n, numval_n, -10000.0, 10000.0)
	   surfom_N_added = sum(dlt_dm_N(:) * fraction_to_Residue(:))

	   ! Find the amount of P added in surfom today, if phosphorus aware

	   if ( g%phosphorus_aware ) then
	      dlt_dm_P(:) = 0.0
	      call collect_real_array_optional (DATA_dlt_dm_p, MaxArraySize, '(kg/ha)', dlt_dm_p, numval_p, -10000.0, 10000.0)
	      surfom_P_added = sum(dlt_dm_P(:) * fraction_to_Residue(:))
	   else
	      ! Not phosphorus aware
	      dlt_dm_P(:) = 0.0
	      surfom_P_added = 0.0
	   endif

	   ! Report Additions
	   if (p%report_additions.eq.'yes') then
	      Write (Event_string, '(1x, 2a, 2(40x, 3a), 40x, a, f8.2, a, 40x, a, f8.5, a)' )   &
	            ' Added surfom', New_Line   &
	            ,'surfom Type         = ', trim(crop_type), New_Line   &
	            , New_Line   &
	            ,'Amount Added (kg/ha) = ', surfom_added, New_Line
	      call Write_string (Event_string)
	   else
	      ! The user has asked for no reports for additions of surfom
	      ! in the summary file.
	   endif

      SOMNo = surfom_number(crop_type)


	   ! Assume the 'crop_type' is the unique name.  Now check whether this unique 'name' already exists in the system.

	   if (SOMNo.eq.0) then
	      ! THIS IS A NEW COMPONENT TO THE SYSTEM
	      g%num_surfom = g%num_surfom + 1
	      SOMNo = g%num_surfom
	      g%SurfOM(SOMNo)%name = crop_type
	      g%SurfOM(SOMNo)%OrganicMatterType = crop_type
	      g%SurfOM(SOMNo)%PotDecompRate=0.0
	      g%SurfOM(SOMNo)%no3 =0.0
	      g%SurfOM(SOMNo)%nh4 =0.0
	      g%SurfOM(SOMNo)%po4 =0.0
	      g%SurfOM(SOMNo)%Standing(:)%amount = 0.0
	      g%SurfOM(SOMNo)%Standing(:)%C = 0.0
	      g%SurfOM(SOMNo)%Standing(:)%N = 0.0
	      g%SurfOM(SOMNo)%Standing(:)%P = 0.0
	      g%SurfOM(SOMNo)%Standing(:)%AshAlk = 0.0
	      g%SurfOM(SOMNo)%Lying(:)%amount = 0.0
	      g%SurfOM(SOMNo)%Lying(:)%C = 0.0
	      g%SurfOM(SOMNo)%Lying(:)%N = 0.0
	      g%SurfOM(SOMNo)%Lying(:)%P = 0.0
	      g%SurfOM(SOMNo)%Lying(:)%AshAlk = 0.0

         ! NOW UPDATE ALL VARIABLES
         call surfom_read_type_specific_constants(g%SurfOM(SOMNo)%OrganicMatterType,SOMNo)

      else
         ! THIS ADDITION IS AN EXISTING COMPONENT OF THE SURFOM SYSTEM
      endif

      ! convert the ppm figures into kg/ha
      g%SurfOM(SOMNo)%no3 = g%SurfOM(SOMNo)%no3+  c%no3ppm(SOMNo)/ 1000000.0 * surfom_added
      g%SurfOM(SOMNo)%nh4 = g%SurfOM(SOMNo)%nh4+  c%nh4ppm(SOMNo)/ 1000000.0 * surfom_added
      g%SurfOM(SOMNo)%po4 = g%SurfOM(SOMNo)%po4+  c%po4ppm(SOMNo)/ 1000000.0 * surfom_added

      ! Assume all surfom added is in the LYING pool, ie No STANDING component
      g%SurfOM(SOMNo)%Lying(1:MaxFr)%amount =g%SurfOM(SOMNo)%Lying(1:MaxFr)%amount+ surfom_added*c%fr_pool_c(1:MaxFr,SOMNo)
      g%SurfOM(SOMNo)%Lying(1:MaxFr)%C =g%SurfOM(SOMNo)%Lying(1:MaxFr)%C+ surfom_added * c%c_fract(SOMNo)* c%fr_pool_c(1:MaxFr,SOMNo)
      g%SurfOM(SOMNo)%Lying(1:MaxFr)%N =g%SurfOM(SOMNo)%Lying(1:MaxFr)%N +surfom_N_added * c%fr_pool_n(1:MaxFr,SOMNo)
      g%SurfOM(SOMNo)%Lying(1:MaxFr)%P =g%SurfOM(SOMNo)%Lying(1:MaxFr)%P +surfom_P_added * c%fr_pool_p(1:MaxFr,SOMNo)
      g%SurfOM(SOMNo)%Lying(1:MaxFr)%AshAlk = 0.0



      call residue2_Send_Res_added_Event (g%SurfOM(SOMNo)%OrganicMatterType   &
                                         , g%SurfOM(SOMNo)%OrganicMatterType   &
                                         , surfom_added   &
                                         , surfom_N_added   &
                                         , surfom_P_added)

   endif

   call pop_routine (myname)
   return
end subroutine

! ====================================================================
subroutine residue2_Send_Res_added_Event(residue_type, dm_type, dlt_residue_wt, dlt_residue_N_wt, dlt_residue_P_wt)

! ====================================================================
   Use Infrastructure
   implicit none

!+  Sub-Program Arguments
   character residue_type*(*)      ! (INPUT)
   character dm_type*(*)           ! (INPUT)
   real      dlt_residue_wt        ! (INPUT)
   real      dlt_residue_N_wt      ! (INPUT)
   real      dlt_residue_P_wt      ! (INPUT)

!+  Purpose
!     Notify other modules of residue added to residue pool.

!+  Constant Values
   character*(*) myname               ! name of current procedure
   parameter (myname = 'residue2_Send_Res_added_Event')

!- Implementation Section ----------------------------------
   call push_routine (myname)

   call new_postbox ()

   call post_char_var   (DATA_residue_type,'()', residue_type)
   call post_char_var   (DATA_dm_type,'()', dm_type)
   call post_real_var   (DATA_dlt_residue_wt,'(kg/ha)', dlt_residue_wt)
   call post_real_var   (DATA_dlt_dm_n,'(kg/ha)', dlt_residue_N_wt)
   call post_real_var   (DATA_dlt_dm_p,'(kg/ha)', dlt_residue_P_wt)
   call event_send (EVENT_Residue_added)

   call delete_postbox ()

   call pop_routine (myname)
   return
end subroutine

! ====================================================================
subroutine residue2_Send_Res_removed_Event(residue_removed_action, dlt_residue_fraction, residue_incorp_fraction, deepest_layer)
! ====================================================================
   Use Infrastructure
   implicit none

!+  Sub-Program Arguments
   integer   deepest_layer                   ! (INPUT)
   character residue_removed_action*(*)      ! (INPUT)
   real      dlt_residue_fraction            ! (INPUT)
   real      residue_incorp_fraction(deepest_layer)! (INPUT)

!+  Purpose
!     Notify other modules of residue removed from residue pool.

!+  Constant Values
   character*(*) myname               ! name of current procedure
   parameter (myname = 'residue2_Send_Res_removed_Event')

!- Implementation Section ----------------------------------
   call push_routine (myname)

   call new_postbox ()

   call post_char_var   (DATA_residue_removed_action,'()', residue_removed_action)
   call post_real_var   (DATA_dlt_residue_fraction,'()', dlt_residue_fraction)
   call post_real_array   (DATA_residue_incorp_fraction,'()', residue_incorp_fraction, deepest_layer)
   call event_send (EVENT_Residue_removed)

   call delete_postbox ()

   call pop_routine (myname)
   return
end subroutine


! ================================================================
subroutine surfom_Sum_Report ()
! ================================================================
   Use Infrastructure
   implicit none

!+  Purpose
!      Output residue module summary details.

!+  Mission Statement
!      Report Residue module summary details

!+  Changes
!     190897  igh  created

!+  Calls


!+  Constant Values
   character*(*) my_name
   parameter (my_name = 'surfom_sum_report')

!+  Local Variables
   character string*300
   character name*100
   character somtype*100
   real      mass
   real      C
   real      N
   real      P
   real      cover
   real      standfr
   real      cover1                 ! fraction of ground covered by residue (0-1)
   real      cover2                 ! fraction of ground covered by residue (0-1)
   real      combined_cover         ! effective combined cover from covers 1 & 2 (0-1)
   integer   i

!- Implementation Section ----------------------------------

   call push_routine (my_name)

! report

   call write_string (new_line//new_line)

   string = '                    Initial Surface Organic Matter Data'
   call write_string (string)

   string = '    ----------------------------------------------------------------------'
   call write_string (string)

   call write_string ('       Name   Type        Dry matter   C        N        P    Cover  Standing_fr')

   call write_string ('                           (kg/ha)  (kg/ha)  (kg/ha)  (kg/ha) (0-1)     (0-1)')

   string = '    ----------------------------------------------------------------------'
   call write_string (string)

   do  i = 1,g%num_surfom

      name = g%SurfOM(i)%name
      somtype = g%SurfOM(i)%OrganicMatterType
      mass = sum(g%SurfOM(i)%Standing(1:MaxFr)%amount) + sum(g%SurfOM(i)%Lying(1:MaxFr)%amount)
      C = sum(g%SurfOM(i)%Standing(1:MaxFr)%C) + sum(g%SurfOM(i)%Lying(1:MaxFr)%C)
      N = sum(g%SurfOM(i)%Standing(1:MaxFr)%N) + sum(g%SurfOM(i)%Lying(1:MaxFr)%N)
      P = sum(g%SurfOM(i)%Standing(1:MaxFr)%P) + sum(g%SurfOM(i)%Lying(1:MaxFr)%P)
      cover = surfom_Cover(i)
      standfr = divide(sum(g%SurfOM(i)%Standing(1:MaxFr)%C), C, 0.0)
      
      write (string, '(5x, a10, a12, 4f8.1, f8.3, f8.1)')name, somtype, mass, C, N, P, cover, standfr
      call write_string (string)

   end do

   string = '    ----------------------------------------------------------------------'
   call write_string (string)

   do i = 1,g%num_surfom
       cover2 = surfom_Cover(i)
       combined_cover = add_cover(cover1,cover2)
       cover1 = cover2
   end do

   string = '    '
   call write_string (string)

   write(string,'(a58,f5.1)')'                 Effective Cover from Surface Materials = ', combined_cover
   call write_string (string)

   string = '    '
   call write_string (string)

   call pop_routine (my_name)
   return
end subroutine


end module SurfaceOMModule


!===========================================================
subroutine alloc_dealloc_instance(doAllocate)
!===========================================================
   use SurfaceOMModule
   implicit none
   ml_external alloc_dealloc_instance

!+  Sub-Program Arguments
   logical, intent(in) :: doAllocate

!+  Purpose
!      Module instantiation routine.

!- Implementation Section ----------------------------------

   if (doAllocate) then
      allocate(id)
      allocate(g)
      allocate(p)
      allocate(c)
   else
      deallocate(id)
      deallocate(g)
      deallocate(p)
      deallocate(c)
   end if

   return
end subroutine



! ====================================================================
subroutine Main (action, data_string)
! ====================================================================
   Use Infrastructure
   use SurfaceOMModule
   implicit none
   ml_external Main

!+  Sub-Program Arguments
   character action*(*)             ! (input) action to perform
   character data_string*(*)        ! (input) data for action

!+  Constant Values
   character  my_name*(*)           ! name of this module
   parameter (my_name = 'SurfaceOM')
   character  text*100
!- Implementation Section ----------------------------------

   call push_routine (my_name)

   if (Action.eq.ACTION_Get_variable) then
      call surfom_Send_my_variable (data_string)

   else if (Action .eq. ACTION_Set_variable) then
      call surfom_set_my_variable (Data_String)

   else if (Action.eq.ACTION_Process) then
      call surfom_get_other_variables ()
      call surfom_Process ()

   else if (Action.eq.ACTION_Till) then
      call surfom_tillage ()

   else if (Action.eq.'add_surfaceom') then
      call surfom_Add_surfom ()

   elseif (Action .eq. EVENT_Crop_Chopped) then
      call surfom_ON_Crop_Chopped ()

   else if (Action.eq.EVENT_irrigated) then
      call surfom_ONirrigated ()

   else if ((Action.eq.ACTION_Reset).or.(Action.eq.ACTION_user_init)) then
      call surfom_Reset ()

   else if (Action.eq.ACTION_Sum_Report) then
      call surfom_Sum_Report ()

   else if (Action.eq.ACTION_Init) then
      call surfom_reset ()

   elseif (Action.eq.ACTION_Create) then
      call doRegistrations(id)
      call surfom_zero_all_globals ()
      call surfom_zero_event_data ()
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
   use SurfaceOMModule
   Use infrastructure
   implicit none
   ml_external respondToEvent

   integer, intent(in) :: fromID
   integer, intent(in) :: eventID
   integer, intent(in) :: variant

   if (eventID .eq. id%newmet) then
      call surfom_ONnewmet(variant)
   elseif (eventID .eq.id%ActualResidueDecompositionCalculated) then
      call surfom_decompose_surfom(variant)
   endif
   return
end subroutine respondToEvent