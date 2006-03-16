C     Last change:  P    31 Oct 2000   11:55 am
      integer MAX_NAME_SIZE
      parameter (MAX_NAME_SIZE=100)  
  
      integer MAX_VARNAME_SIZE
      parameter (MAX_VARNAME_SIZE=100)  
 
      integer MAX_NUM_INSTANCE_NOS
      parameter (MAX_NUM_INSTANCE_NOS=50)  

      common /ComponentInterfaceData/
     .   MessageUsed,
     .   InstanceNoStack,
     .   CurrentInstanceIndex,
     .   EventInterface,
     .   ComponentData,
     .   EventInterfaces,
     .   ComponentDatas,
     .   SavedMessageUsed,
     .   SavedEventInterface,
     .   SavedComponentData

      ! module variables. 
      logical MessageUsed
      integer InstanceNoStack(MAX_NUM_INSTANCE_NOS)
      integer CurrentInstanceIndex
      integer EventInterface
      integer ComponentData
      integer EventInterfaces(MAX_NUM_INSTANCE_NOS)
      integer ComponentDatas(MAX_NUM_INSTANCE_NOS)
      logical SavedMessageUsed
      integer SavedEventInterface
      integer SavedComponentData

      save /ComponentInterfaceData/