! ===========================================================================
      module LateralModule
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
      type LateralGlobals
! ===========================================================================
         real    outflow_lat(max_layer)
         integer num_layers
      end type LateralGlobals
! ===========================================================================
      type LateralParameters
! ===========================================================================
         real slope
         real discharge_width     ! basal width of discharge area (m)
         real catchment_area      ! area over which lateral flow is occuring (m2)
         real Klat(max_layer)
      end type LateralParameters

! ===========================================================================
      type LateralConstants
! ===========================================================================
         real dummy
      end type LateralConstants
! ===========================================================================
!      Module-Level Variables
! ===========================================================================

      ! instance variables.
      type (LateralGlobals), pointer :: g
      type (LateralParameters), pointer :: p
      type (LateralConstants), pointer :: c
      integer MAX_NUM_INSTANCES
      parameter (MAX_NUM_INSTANCES=50)
      integer MAX_INSTANCE_NAME_SIZE
      parameter (MAX_INSTANCE_NAME_SIZE=50)
      type LateralDataPtr
         private
         type (LateralGlobals), pointer ::    gptr
         type (LateralParameters), pointer :: pptr
         type (LateralConstants), pointer ::  cptr
         character Name*(MAX_INSTANCE_NAME_SIZE)
      end type LateralDataPtr
      type (LateralDataPtr), dimension(MAX_NUM_INSTANCES) :: Instances


! ===========================================================================
!      Module Source Code
! ===========================================================================

! Public Interface to Module
! ==========================
      public LateralAllocInstance
      public LateralSwapInstance
      public LateralFreeInstance
      public Lateral_Init
      public Lateral_Prepare
      public Lateral_Process
      public Lateral_Send_My_Variable

      contains


!     ===========================================================
      subroutine LateralAllocInstance (InstanceName, InstanceNo)
!     ===========================================================
      Use Infrastructure
      implicit none

!+  Sub-Program Arguments
      character InstanceName*(*)       ! (INPUT) name of instance
      integer   InstanceNo             ! (INPUT) instance number to allocate

!+  Purpose
!      Module instantiation routine.

!- Implementation Section ----------------------------------

      allocate (Instances(InstanceNo)%gptr)
      allocate (Instances(InstanceNo)%pptr)
      allocate (Instances(InstanceNo)%cptr)
      Instances(InstanceNo)%Name = InstanceName

      return
      end subroutine

!     ===========================================================
      subroutine LateralFreeInstance (anInstanceNo)
!     ===========================================================
      Use Infrastructure
      implicit none

!+  Sub-Program Arguments
      integer anInstanceNo             ! (INPUT) instance number to allocate

!+  Purpose
!      Module de-instantiation routine.

!- Implementation Section ----------------------------------

      deallocate (Instances(anInstanceNo)%gptr)
      deallocate (Instances(anInstanceNo)%pptr)
      deallocate (Instances(anInstanceNo)%cptr)

      return
      end subroutine

!     ===========================================================
      subroutine LateralSwapInstance (anInstanceNo)
!     ===========================================================
      Use Infrastructure
      implicit none

!+  Sub-Program Arguments
      integer anInstanceNo             ! (INPUT) instance number to allocate

!+  Purpose
!      Swap an instance into the global 'g' pointer

!- Implementation Section ----------------------------------

      g => Instances(anInstanceNo)%gptr
      p => Instances(anInstanceNo)%pptr
      c => Instances(anInstanceNo)%cptr

      return
      end subroutine



* ====================================================================
       subroutine Lateral_zero_variables ()
* ====================================================================
      Use Infrastructure
      implicit none

*+  Purpose
*     Set all variables in this module to zero.

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'Lateral_zero_variables')

*+  Local Variables

*- Implementation Section ----------------------------------
      call push_routine (myname)


      ! Parameters
      ! ==========

         p%slope = 0.0
         p%discharge_width = 0.0
         p%catchment_area = 0.0
         p%Klat(:) = 0.0

      ! Globals
      ! =======
         g%outflow_lat(:) = 0.0

      ! Constants
      ! =========


      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine Lateral_get_other_variables ()
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
      parameter (myname = 'Lateral_get_other_variables')

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
      subroutine Lateral_read_param ()
*     ===========================================================
      Use Infrastructure
      implicit none

*+  Purpose
*       Read all module parameters.

*+  Changes
*     <insert here>

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Lateral_read_param')
*
      character section_name*(*)
      parameter (section_name = 'parameters')

*+  Local Variables
      integer    numvals               ! number of values read

*- Implementation Section ----------------------------------

      call push_routine (myname)

      call read_real_var_optional (
     :           section_name,          ! Section header
     :           'slope',               ! Keyword
     :           '()',                  ! Units
     :           p%slope,               ! Array
     :           numvals,               ! Number of values returned
     :           0.0,                   ! Lower Limit for bound checking
     :           1.0)                   ! Upper Limit for bound checking

      if (numvals.eq.0) then
         p%slope = 0.0
      endif

      call read_real_var_optional (
     :           section_name,          ! Section header
     :           'discharge_width',        ! Keyword
     :           '(m)',                  ! Units
     :           p%discharge_width,        ! Array
     :           numvals,               ! Number of values returned
     :           0.0,                   ! Lower Limit for bound checking
     :           1e8)                   ! Upper Limit for bound checking

      if (numvals.eq.0) then
         p%discharge_width = 0.0
      endif


      call read_real_var_optional (
     :           section_name,          ! Section header
     :           'catchment_area',        ! Keyword
     :           '(m2)',                  ! Units
     :           p%catchment_area,        ! Array
     :           numvals,               ! Number of values returned
     :           0.0,                   ! Lower Limit for bound checking
     :           1e8)                   ! Upper Limit for bound checking

      if (numvals.eq.0) then
         p%catchment_area = 0.0
      endif

      call read_real_array_optional (
     :           section_name,          ! Section header
     :           'klat',                ! Keyword
     :           max_layer,             ! Array size
     :           '(mm/d)',              ! Units
     :           p%klat,                ! Array
     :           numvals,               ! Number of values returned
     :           0.0,                   ! Lower Limit for bound checking
     :           1e3)                   ! Upper Limit for bound checking

       if (numvals.eq.0) then
         p%klat(:) = 0.0
      endif

      call pop_routine  (myname)
      return
      end subroutine


*     ===========================================================
      subroutine Lateral_read_constants ()
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
      parameter (myname = 'Lateral_read_constants')

      character*(*) section_name
      parameter (section_name = 'constants')

*+  Local Variables
      integer    numvals               ! number of values read

*- Implementation Section ----------------------------------

      call push_routine (myname)

c      call write_string (new_line//'   - Reading Constants')

c      call read_real_var (
c     :              section_name,          ! Section header
c     :              'Lateral_swf_curvature', ! Keyword
c     :              '(m)',                 ! Units
c     :              c%Lateral_swf_curvature, ! Variable
c     :              numvals,               ! Number of values returned
c     :              0.0,                   ! Lower Limit for bound checking
c     :              1.0)                   ! Upper Limit for bound checking

      call pop_routine  (myname)
      return
      end subroutine



* ====================================================================
       subroutine Lateral_prepare ()
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
      parameter (myname = 'Lateral_prepare')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call Lateral_zero_daily_variables()

      call pop_routine (myname)
      return
      end subroutine


* ====================================================================
       subroutine Lateral_process (sw_dep
     :                            ,dul_dep
     :                            ,sat_dep
     :                            ,dlayer)
* ====================================================================
      Use Infrastructure
      implicit none

*+  Sub-Program Arguments
      real sw_dep(*)
      real dul_dep(*)
      real sat_dep(*)
      real dlayer (*)

*+  Purpose
*     <insert here>

*+  Changes
*

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'Lateral_process')

*+  Local Variables

      integer layer
      integer numvals
      real    d      ! depth of water table in a layer (mm)
      real    max_flow
      real    inflow_lat(max_layer)    ! inflowing lateral water

*- Implementation Section ----------------------------------
      call push_routine (myname)

      g%num_layers = count_of_real_vals(dlayer,max_layer)
      inflow_lat(:) = 0.0
! dsg 150302   get lateral inflow additions

      call get_real_array_optional (
     :      unknown_module  ! Module that responds (Not Used)
     :    , 'inflow_lat'        ! Variable Name
     :    , max_layer       ! size of array
     :    , '(mm)'          ! Units                (Not Used)
     :    , inflow_lat        ! Variable
     :    , numvals         ! Number of values returned
     :    , 0.0             ! Lower Limit for bound checking
     :    , 1000.0)         ! Upper Limit for bound checking

      if (numvals.eq.0) then
          inflow_lat(:) = 0.0
      endif

      do layer = 1, g%num_layers

! dsg 150302   add the inflowing lateral water
        sw_dep(layer) = sw_dep(layer) + inflow_lat(layer)


         d = dlayer(layer) * divide (sw_dep(layer)-dul_dep(layer)
     :                              ,sat_Dep(layer)-dul_Dep(layer)
     :                              ,0.0)
         d = max(0.0,d) ! water table depth in layer must be +ve

!         g%outflow_lat(layer) = p%Klat(layer)
!     :                     * d
!     :                     * (p%discharge_width/mm2m)
!     :                     / (p%catchment_area*sm2smm)
!     :                     * p%slope
!     :                     /(1.0+p%slope**2)**0.5

         g%outflow_lat(layer) = divide((p%Klat(layer)* d *
     :    (p%discharge_width/mm2m)* p%slope),((p%catchment_area*sm2smm)
     :                     *(1.0+p%slope**2)**0.5),0.0)


         ! Cannot drop sw below dul
         max_flow = max(0.0,sw_dep(layer)-dul_dep(layer))



         g%outflow_lat(layer) = bound(g%outflow_lat(layer)
     :                            ,0.0
     :                            ,max_flow)


         sw_dep(layer) = sw_dep(layer) - g%outflow_lat(layer)

      end do

      call pop_routine (myname)
      return
      end subroutine


* ====================================================================
       subroutine Lateral_zero_daily_variables ()
* ====================================================================
      Use Infrastructure
      implicit none

*+  Purpose
*     <insert here>

*+  Changes
*

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'Lateral_zero_daily_variables')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      g%outflow_lat(:) = 0.0

      call pop_routine (myname)
      return
      end subroutine


* ====================================================================
       subroutine Lateral_Init ()
* ====================================================================
      Use Infrastructure
      implicit none

*+  Sub-Program Arguments

*+  Purpose
*      Create Lateral module

*+  Changes
*     <insert here>

*+  Calls

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'Lateral_init')

*+  Local Variables

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call Lateral_zero_variables ()
      call Lateral_read()

      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       subroutine Lateral_Read ()
* ====================================================================
      Use Infrastructure
      implicit none

*+  Sub-Program Arguments

*+  Purpose
*      Initialise Lateral module

*+  Changes
*     <insert here>

*+  Calls

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'Lateral_read')

*+  Local Variables

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call Lateral_read_param ()

      call Lateral_read_constants ()

      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       logical function Lateral_Send_my_variable (variable_name)
* ====================================================================
      Use Infrastructure
      implicit none

*+  Sub-Program Arguments
      character variable_name*(*)

*+  Purpose
*      Initialise Lateral module

*+  Changes
*     <insert here>

*+  Calls

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'Lateral_send_my_variable')

*+  Local Variables

*- Implementation Section ----------------------------------
      call push_routine (myname)

      if (Variable_name .eq. 'outflow_lat') then
         call respond2Get_real_array (
     :            variable_name,
     :            '(mm)',
     :            g%outflow_lat,
     :            g%num_layers)

         Lateral_send_my_variable =.true.

      else
         call Message_Unused ()
         Lateral_send_my_variable =.false.
      endif


      call pop_routine (myname)
      return
      end function

      end module LateralModule

