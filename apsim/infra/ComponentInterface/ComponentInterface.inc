      integer MAX_NAME_SIZE
      parameter (MAX_NAME_SIZE=100)  
  
      integer MAX_VARNAME_SIZE
      parameter (MAX_VARNAME_SIZE=100)  
 
      integer MAX_NUM_INSTANCE_NOS
      parameter (MAX_NUM_INSTANCE_NOS=10)  

      common /ComponentInterfaceData/
     .   MessageUsed,
     .   InstanceNoStack,
     .   CurrentInstanceIndex

      ! module variables. 
      logical MessageUsed
      integer InstanceNoStack(MAX_NUM_INSTANCE_NOS)
      integer CurrentInstanceIndex

      save /ComponentInterfaceData/