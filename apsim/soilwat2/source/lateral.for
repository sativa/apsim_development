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
      include 'Lateral_code.for'

      end module LateralModule

