! ===========================================================================
      module EvapModule
! ===========================================================================

      private  ! ALL MEMBERS ARE PRIVATE BY DEFAULT!!!
               ! =====================================

! CONSTANTS
! =========
      integer max_layer
      parameter (max_layer = 50)

      integer max_table
      parameter (max_table = 10)


! ===========================================================================
      type EvapGlobals
! ===========================================================================
         sequence
         integer num_layers
         real    dlayer (max_layer)
         real    air_dry_dep(max_layer)
         real    dul_dep(max_layer)
         real    swf(max_layer)

      end type EvapGlobals
! ===========================================================================
      type EvapParameters
! ===========================================================================
         sequence
         real max_evap_depth
         real max_evap
         real first_stage_evap

      end type EvapParameters

! ===========================================================================
      type EvapConstants
! ===========================================================================
         sequence
         real evap_swf_curvature
         real relative_evap(max_table)
         real relative_swc(max_table)
         integer num_relative_swc
      end type EvapConstants
! ===========================================================================
!      Module-Level Variables
! ===========================================================================

      ! instance variables.
      common /InstancePointers/ ID,g,p,c
      save InstancePointers
      type (EvapGlobals),pointer :: g
      type (EvapParameters),pointer :: p
      type (EvapConstants),pointer :: c


! ===========================================================================
!      Module Source Code
! ===========================================================================

! Public Interface to Module
! ==========================
      public Evap_alloc_dealloc_instance
      public Evap_Create
      public Evap_Init
      public Evap_Read
      public Evap_Prepare
      public Evap_Process

      contains


* ====================================================================
       subroutine Evap_zero_variables ()
* ====================================================================
      Use Infrastructure
      implicit none

*+  Purpose
*     Set all variables in this module to zero.

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'Evap_zero_variables')

*+  Local Variables

*- Implementation Section ----------------------------------
      call push_routine (myname)


      ! Parameters
      ! ==========

         p%max_evap_depth = 0.0
         p%max_evap = 0.0

      ! Globals
      ! =======
         g%num_layers = 0
         g%dlayer(:) = 0
         g%dul_dep(:) = 0
         g%air_dry_dep(:) = 0
         g%swf(:) = 0


      ! Constants
      ! =========
        c%evap_swf_curvature = 0
        c%relative_evap(:) = 0
        c%relative_swc(:) = 0
        c%num_relative_swc = 0

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine Evap_get_other_variables ()
* ====================================================================
      Use Infrastructure
      implicit none

*+  Purpose
*      Get the values of variables from other modules

*+  Changes
*     <insert here>

*+  Calls

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'Evap_get_other_variables')

*+  Local Variables
       integer numvals              ! number of values returned

*- Implementation Section ----------------------------------
      call push_routine (myname)

c      call Get_real_array (
c     :      unknown_module  ! Module that responds (Not Used)
c     :     ,'dlayer'        ! Variable Name
c     :     ,max_layer       ! Array Size
c     :     ,'(mm)'          ! Units                (Not Used)
c     :     ,g%dlayer        ! Variable
c     :     ,numvals         ! Number of values returned
c     :     ,0.0             ! Lower Limit for bound checking
c     :     ,1000.)          ! Upper Limit for bound checking

      call pop_routine (myname)
      return
      end subroutine



*     ===========================================================
      subroutine Evap_read_param ()
*     ===========================================================
      Use Infrastructure
      implicit none

*+  Purpose
*       Read all module parameters.

*+  Changes
*     <insert here>

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Evap_read_param')
*
      character section_name*(*)
      parameter (section_name = 'parameters')

*+  Local Variables
      integer    numvals               ! number of values read

*- Implementation Section ----------------------------------

      call push_routine (myname)

      call read_real_var (
     :           section_name,          ! Section header
     :           'max_evap_depth',      ! Keyword
     :           '(mm)',                ! Units
     :           p%max_evap_depth,      ! Array
     :           numvals,               ! Number of values returned
     :           0.0,                   ! Lower Limit for bound checking
     :           1000.)                 ! Upper Limit for bound checking

      call read_real_var (
     :           section_name,          ! Section header
     :           'max_evap',            ! Keyword
     :           '(mm)',                ! Units
     :           p%max_evap,            ! Array
     :           numvals,               ! Number of values returned
     :           0.0,                   ! Lower Limit for bound checking
     :           100.)                  ! Upper Limit for bound checking

      call read_real_var (
     :           section_name,          ! Section header
     :           'first_stage_evap',    ! Keyword
     :           '(mm)',                ! Units
     :           p%first_stage_evap,    ! Array
     :           numvals,               ! Number of values returned
     :           0.0,                   ! Lower Limit for bound checking
     :           100.)                  ! Upper Limit for bound checking

      call pop_routine  (myname)
      return
      end subroutine


*     ===========================================================
      subroutine Evap_read_constants ()
*     ===========================================================

      Use Infrastructure
      implicit none

*+  Calls

*+  Sub-Program Arguments


*+  Purpose
*       Read all module constants.

*+  Changes
*

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Evap_read_constants')

      character*(*) section_name
      parameter (section_name = 'constants')

*+  Local Variables
      integer    numvals               ! number of values read

*- Implementation Section ----------------------------------

      call push_routine (myname)

      call write_string (new_line//'   - Reading Constants')

      call read_real_var (
     :              section_name,          ! Section header
     :              'evap_swf_curvature', ! Keyword
     :              '(m)',                 ! Units
     :              c%evap_swf_curvature, ! Variable
     :              numvals,               ! Number of values returned
     :              0.0,                   ! Lower Limit for bound checking
     :              1.0)                   ! Upper Limit for bound checking

c      call read_real_array (
c     :              section_name,          ! Section header
c     :              'relative_evap',       ! Keyword
c     :              max_table,             ! array size
c     :              '(mm/mm)',             ! Units
c     :              c%relative_evap,       ! Variable
c     :              c%num_relative_swc,    ! Number of values returned
c     :              0.0,                   ! Lower Limit for bound checking
c     :              1.0)                   ! Upper Limit for bound checking
c
c      call read_real_array (
c     :              section_name,          ! Section header
c     :              'relative_swc',        ! Keyword
c     :              max_table,             ! array size
c     :              '(mm/mm)',             ! Units
c     :              c%relative_swc,        ! Variable
c     :              c%num_relative_swc,    ! Number of values returned
c     :              0.0,                   ! Lower Limit for bound checking
c     :              1.0)                   ! Upper Limit for bound checking

      call pop_routine  (myname)
      return
      end subroutine



* ====================================================================
       subroutine Evap_prepare ()
* ====================================================================
       Use Infrastructure
      implicit none

*+  Sub-Program Arguments

*+  Purpose
*     <insert here>

*+  Changes
*

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'Evap_prepare')

*- Implementation Section ----------------------------------
      call push_routine (myname)


      call pop_routine (myname)
      return
      end subroutine


* ====================================================================
       subroutine Evap_process (sw_dep
     :                            ,surface_water
     :                            ,Infiltration
     :                            ,eo
     :                            ,eos
     :                            ,es)
* ====================================================================
      Use Infrastructure
      implicit none

*+  Sub-Program Arguments
      real sw_dep(*)
      real surface_water
      real Infiltration
      real eo
      real eos
      real es(*)

*+  Purpose
*     <insert here>

*+  Changes
*

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'Evap_process')

*+  Local Variables
      real rwc(max_layer)
      real avg_rwc
      integer layer
      real    remaining_evap
      real    relative_Evap
      real    evap
      real    max_evap
      real    cover_fact
      real    soil_fact
      real    max_soil_evap
      real    RainEvap
      real    Resistance
      real       profile_depth         ! current depth of soil profile
                                       ! - for when erosion turned on
      real       max_evap_depth        !  (mm)
      integer    max_evap_layer        ! layer number in which the max evap
                                       ! depth occurs ()
      real       mwc(max_layer)        ! max water content

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call Evap_get_other_variables ()

      if (surface_Water.gt.0.0) then
         call warning_error(err_internal
     :         ,'cannot handle surface water evaporation')
      else
      endif


      profile_depth = sum_real_array (g%dlayer, g%num_layers)

      max_evap_depth = min (p%max_evap_depth
     :                     ,profile_depth)

      max_evap_layer = find_layer_no (max_evap_depth
     :                               , g%dlayer
     :                               , g%num_layers)

      do layer = 1, max_evap_layer

         rwc(layer) = divide(sw_dep(layer)  - g%air_dry_dep(layer)
     :                      ,g%dul_dep(layer) - g%air_dry_dep(layer)
     :                      ,0.0)
         rwc(layer) = bound(rwc(layer), 0.0, 1.0)

         mwc(layer) = g%dul_dep(layer) - g%air_dry_dep(layer)

      end do
      avg_rwc = sum(rwc(:)*g%swf(:))

      soil_Fact = min(1.0
     :               ,0.05
     :               +(min(1.0,p%max_evap/eo)-0.05)
     :                  * avg_rwc**2)
      soil_fact = bound(soil_Fact,0.0,1.0)

      cover_fact = divide (eos,eo,1.0)
      cover_fact = bound(cover_Fact,0.0,1.0)


      resistance = divide (1.0,soil_fact,1e10)
     :           + divide(1.0,cover_fact,1e10)
     :           - 1.0
      relative_evap = 1.0/resistance
      evap = eo*relative_evap

      evap = min(p%max_evap, evap)

      if (infiltration.gt.0.0) then
         RainEvap = infiltration
         RainEvap = min(p%max_evap,RainEvap,eos)
         if (RainEvap.gt.Evap) then
            Evap = RainEvap
         endif
      else
      endif

      es(1:max_layer) = 0.0
      remaining_evap = evap

      do layer = 1, g%num_layers

         es(layer) =  divide(rwc(layer)*g%swf(layer)
     :                      ,sum(rwc(layer:g%num_layers)
     :                           *g%swf(layer:g%num_layers)
     :                           )
     :                      ,0.0)
     :             *  remaining_evap

!         es(layer) = remaining_Evap

         max_evap = max(sw_dep(layer) - g%air_dry_dep(layer), 0.0)

         es(layer) = bound(es(layer),0.0,max_evap)

         remaining_evap = remaining_evap - es(layer)

      end do


      call pop_routine (myname)
      return
      end subroutine


* ====================================================================
       subroutine Evap_zero_daily_variables ()
* ====================================================================
      Use Infrastructure
      implicit none

*+  Purpose
*     <insert here>

*+  Changes
*

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'Evap_zero_daily_variables')

*- Implementation Section ----------------------------------
      call push_routine (myname)


      call pop_routine (myname)
      return
      end subroutine


* ====================================================================
       subroutine Evap_Create ()
* ====================================================================
      Use Infrastructure
      implicit none

*+  Sub-Program Arguments

*+  Purpose
*      Create Evap module

*+  Changes
*     <insert here>

*+  Calls

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'Evap_create')

*+  Local Variables

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call Evap_zero_variables ()

      call pop_routine (myname)
      return
      end subroutine
* ====================================================================
       subroutine Evap_Init (num_layers,dlayer,air_dry_dep,dul_dep)
* ====================================================================
      Use Infrastructure
      implicit none

*+  Sub-Program Arguments
      integer num_layers
      real dlayer(*)
      real air_dry_dep(*)
      real dul_dep(*)

*+  Purpose
*      Initialise Evap module

*+  Changes
*     <insert here>

*+  Calls

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'Evap_init')

*+  Local Variables

*- Implementation Section ----------------------------------
      call push_routine (myname)

      g%num_layers = num_layers
      g%dlayer(1:num_layers) = dlayer(1:num_layers)
      g%air_dry_dep(1:num_layers) = air_dry_dep(1:num_layers)
      g%dul_dep(1:num_layers) = dul_dep(1:num_layers)

      call Evap_get_other_variables ()

      call Evap_init_calc ()


      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       subroutine Evap_Read ()
* ====================================================================
      Use Infrastructure
      implicit none

*+  Sub-Program Arguments

*+  Purpose
*      Initialise Evap module

*+  Changes
*     <insert here>

*+  Calls

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'Evap_read')

*+  Local Variables

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call Evap_read_param ()

      call Evap_read_constants ()

      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       subroutine Evap_init_calc ()
* ====================================================================
      Use Infrastructure
      implicit none

*+  Calls

*+  Purpose
*      Perform initialisation calculations

*+  Mission Statement
*      Perform initialisation calculations

*+  Changes
*     31-12-1999 - nih - Programmed and Specified

*+  Constant Values

      character*(*) myname               ! name of current procedure
      parameter (myname = 'Evap_init_calc')

*+  Local Variables
      integer layer
      character line*200

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call evap_space_weighting_factor(g%swf)

      do layer = 1,g%num_layers

         write (line,'(3x, i2, a, f10.4)')
     :            layer, '-', g%swf(layer)
         call write_string (line)
      end do

      call pop_routine (myname)
      return
      end subroutine


* ====================================================================
       subroutine Evap_space_weighting_factor (evap_swf)
* ====================================================================

      Use Infrastructure
      implicit none

*+  Sub-Program Arguments
      real    evap_swf(*)              ! (OUTPUT) weighting factor for evaporation

*+  Purpose
*      Calculate the space weighting factor used
*      to weight the effect of soil moisture on evaporation.

*+  Mission Statement
*      Calculate space weighting factor for soil moisture effect on evaporation

*+  Changes
*     04-01-2002 - Neil Huth - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'Evap_space_weighting_factor')

*+  Local Variables
      real       profile_depth         ! current depth of soil profile
                                       ! - for when erosion turned on
      real       cum_depth             ! cumulative depth (mm)
      real       max_evap_depth        !  (mm)
      integer    max_evap_layer        ! layer number in which the max evap
                                       ! depth occurs ()
      integer    layer                 ! layer counter
      real       scale_fact            ! scaling factor for wf function to
                                       ! sum to 1
      real       swf_tot               ! total of swf ()
      real       wx                    ! depth weighting factor for current
                                       !    total depth.
                                       !    intermediate variable for
                                       !    deriving swf
                                       !    (total swfs to current layer)
      real       xx                    ! intermediate variable for deriving swf
                                       ! total swfs to previous layer

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call fill_real_array (evap_swf, 0.0, max_layer)
      xx     = 0.0
      cum_depth = 0.0
      swf_tot = 0.0

           ! check if hydro_effective_depth applies for eroded profile.

      profile_depth = sum_real_array (g%dlayer, g%num_layers)

      max_evap_depth = min (p%max_evap_depth
     :                     ,profile_depth)


      max_evap_layer = find_layer_no (max_evap_depth
     :                               , g%dlayer
     :                               , g%num_layers)

      if (c%evap_swf_curvature.eq.0.0) then

         do 100 layer = 1, max_evap_layer
            evap_swf(layer) = divide(g%dlayer(layer)
     :                              ,max_evap_depth
     :                              ,0.0)
  100    continue

      else

         scale_fact = 1.0/(1.0 - exp(-c%evap_swf_curvature))
         do 200 layer = 1, max_evap_layer
            cum_depth = cum_depth + g%dlayer(layer)
            cum_depth = u_bound (cum_depth, max_evap_depth)

            wx = scale_fact * (1.0 - exp( - c%evap_swf_curvature
     :                                 * divide (cum_depth
     :                                          , max_evap_depth
     :                                          , 0.0)))
            evap_swf(layer) = wx - xx

            xx = wx

            swf_tot = swf_tot + evap_swf(layer)

  200    continue

      endif

      call bound_check_real_var (swf_tot, 0.9999, 1.0001, 'swf_tot')

      call pop_routine (myname)
      return
      end subroutine



      end module EvapModule

!     ===========================================================
      subroutine Evap_alloc_dealloc_instance(doAllocate)
!     ===========================================================
      use EvapModule
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
      else
         deallocate(g)
         deallocate(p)
         deallocate(c)
      end if
      return
      end subroutine

