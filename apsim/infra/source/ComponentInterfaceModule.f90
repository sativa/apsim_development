module ComponentInterfaceModule
   implicit none
                  
   integer ,parameter :: MAX_NAME_SIZE = 100
   integer ,parameter :: MAX_VARNAME_SIZE = 100
   integer ,parameter :: MAX_NUM_INSTANCE_NOS = 50
   integer ,parameter :: MAX_LEVELS_RECURSION = 20

   common /ComponentInterfaceData/    &
         MessageUsed,                 &
         InstanceNoStack,             &
         CurrentInstanceIndex,        &
         EventInterface,              &
         ComponentData,               &
         EventInterfaces,             &
         ComponentDatas,              &
         SavedMessageUsed,            &
         SavedEventInterface,         &
         SavedComponentData,          &
         LevelOfRecursion

      ! module variables. 
   logical :: MessageUsed
   integer :: InstanceNoStack(MAX_NUM_INSTANCE_NOS)
   integer :: CurrentInstanceIndex
   integer :: EventInterface
   integer :: ComponentData
   integer :: EventInterfaces(MAX_NUM_INSTANCE_NOS)
   integer :: ComponentDatas(MAX_NUM_INSTANCE_NOS)
   logical :: SavedMessageUsed(MAX_LEVELS_RECURSION)
   integer :: SavedEventInterface(MAX_LEVELS_RECURSION)
   integer :: SavedComponentData(MAX_LEVELS_RECURSION)
   integer :: LevelOfRecursion
   save /ComponentInterfaceData/   

   dll_import ei_getvariable
   dll_import ei_setvariable
   dll_import ei_componentresponded        
   dll_import ei_create
   dll_import ei_free  
   
   dll_import newApsimComponentData 
   dll_import deleteApsimComponentData

   interface
                       
      function ei_create(computation)
      integer, intent(in)           :: computation
      integer                       :: ei_create
      end function ei_create

      subroutine ei_free(computation)
      integer, intent(in)           :: computation
      end subroutine ei_free
                       
      function ei_getvariable(eventInterface, variableName)
      integer, intent(in)           :: eventInterface
      character (len=*), intent(in) :: variableName
      logical                       :: ei_getvariable
      end function ei_getvariable  
      
      function ei_componentresponded(eventInterface)
      integer, intent(in)           :: eventInterface
      logical                       :: ei_componentresponded
      end function ei_componentresponded
          
      function ei_setvariable(eventInterface, variableName)
      integer, intent(in)           :: eventInterface
      character (len=*), intent(in) :: variableName
      logical                       :: ei_setvariable
      end function ei_setvariable  

      function read_parameter_internal(sectionName, variableName, &
                                       variableValue, optionalRead)
      character (len=*), intent(in)  :: sectionName
      character (len=*), intent(in)  :: variableName
      character (len=*), intent(out) :: variableValue
      logical          , intent(in)  :: optionalRead
      logical                        :: read_parameter_internal
      end function read_parameter_internal
                                       
      function newApsimComponentData(ssdl)
      character (len=*), intent(in)  :: ssdl
      integer                        :: newApsimComponentData
      end function newApsimComponentData
      
   end interface

end module ComponentInterfaceModule
