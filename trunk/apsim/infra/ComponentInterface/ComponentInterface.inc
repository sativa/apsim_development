      common /ComponentInterfaceData/
     .   MessageUsed

      ! module variables. 
      logical :: MessageUsed

      integer MAX_NAME_SIZE
      parameter (MAX_NAME_SIZE=100)  
  
      integer MAX_VARNAME_SIZE
      parameter (MAX_VARNAME_SIZE=100)  

      save /ComponentInterfaceData/