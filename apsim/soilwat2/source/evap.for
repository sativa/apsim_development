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
         integer num_layers
         real    dlayer (max_layer)
         real    air_dry_dep(max_layer)
         real    dul_dep(max_layer)
         real    swf(max_layer)

      end type EvapGlobals
! ===========================================================================
      type EvapParameters
! ===========================================================================
         real max_evap_depth
         real max_evap
         real first_stage_evap
                  
      end type EvapParameters

! ===========================================================================
      type EvapConstants
! ===========================================================================
         real evap_swf_curvature
         real relative_evap(max_table)
         real relative_swc(max_table)
         integer num_relative_swc
      end type EvapConstants
! ===========================================================================
!      Module-Level Variables
! ===========================================================================

      ! instance variables.
      type (EvapGlobals), pointer :: g
      type (EvapParameters), pointer :: p
      type (EvapConstants), pointer :: c
      integer MAX_NUM_INSTANCES
      parameter (MAX_NUM_INSTANCES=50)  
      integer MAX_INSTANCE_NAME_SIZE
      parameter (MAX_INSTANCE_NAME_SIZE=50)
      type EvapDataPtr
         private
         type (EvapGlobals), pointer ::    gptr
         type (EvapParameters), pointer :: pptr
         type (EvapConstants), pointer ::  cptr
         character Name*(MAX_INSTANCE_NAME_SIZE)
      end type EvapDataPtr
      type (EvapDataPtr), dimension(MAX_NUM_INSTANCES) :: Instances


! ===========================================================================
!      Module Source Code
! ===========================================================================

! Public Interface to Module
! ==========================
      public EvapAllocInstance
      public EvapSwapInstance
      public EvapFreeInstance
      public Evap_Create
      public Evap_Init
      public Evap_Read
      public Evap_Prepare
      public Evap_Process

      contains
      include 'Evap_code.for'

      end module EvapModule

