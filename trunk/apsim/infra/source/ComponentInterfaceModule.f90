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
   dll_import ei_setcomponentresponded  
   dll_import ei_getname
   dll_import ei_broadcastaction
   dll_import ei_sendaction
   dll_import ei_publishevent
   dll_import ei_registersubscribedevent
   dll_import ei_registerpublishedevent
   dll_import ei_changecomponentorder
   dll_import screen_writepercentcomplete
   dll_import apsimsystem_callstack_push
   dll_import apsimsystem_callstack_pop
   dll_import summary_writeline
   dll_import apsimsystem_error_fatal
   dll_import apsimsystem_error_warning
   dll_import summary_enter_diary_state
   
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
      
      subroutine ei_setcomponentresponded(eventInterface, messageIsUsed)
      integer, intent(in)           :: eventInterface
      logical                       :: messageIsUsed
      end subroutine ei_setcomponentresponded  

      subroutine ei_getname(eventInterface, moduleName)
      integer, intent(in)            :: eventInterface
      character (len=*), intent(out) :: moduleName
      end subroutine ei_getname  

      subroutine ei_broadcastaction(eventInterface, action, data)
      integer, intent(in)           :: eventInterface
      character (len=*), intent(in) :: action
      character (len=*), intent(in) :: data
      end subroutine ei_broadcastaction

      subroutine ei_sendaction(eventInterface, moduleName, action, data)
      integer, intent(in)           :: eventInterface
      character (len=*), intent(in) :: moduleName
      character (len=*), intent(in) :: action
      character (len=*), intent(in) :: data
      end subroutine ei_sendaction

      subroutine ei_publishevent(eventInterface, eventName)
      integer, intent(in)           :: eventInterface
      character (len=*), intent(in) :: eventName
      end subroutine ei_publishevent

      subroutine ei_registersubscribedevent(eventInterface, eventName)
      integer, intent(in)           :: eventInterface
      character (len=*), intent(in) :: eventName
      end subroutine ei_registersubscribedevent

      subroutine ei_registerpublishedevent(eventInterface, eventName)
      integer, intent(in)           :: eventInterface
      character (len=*), intent(in) :: eventName
      end subroutine ei_registerpublishedevent

      subroutine ei_changecomponentorder(eventInterface, components, numComponents)
      integer, intent(in)           :: eventInterface
      character (len=*), intent(in) :: components(*)
      integer, intent(in)           :: numComponents
      end subroutine ei_changecomponentorder

      subroutine screen_writepercentcomplete(percent)
      integer, intent(in)           :: percent
      end subroutine screen_writepercentcomplete

      subroutine apsimsystem_callstack_push(routineName)
      character (len=*), intent(in) :: routineName
      end subroutine apsimsystem_callstack_push
      
      subroutine apsimsystem_callstack_pop(routineName)
      character (len=*), intent(in) :: routineName
      end subroutine apsimsystem_callstack_pop

      subroutine apsimsystem_error_fatal(msg, moduleName)
      character (len=*), intent(in) :: msg
      character (len=*), intent(in) :: moduleName                
      end subroutine apsimsystem_error_fatal

      subroutine apsimsystem_error_warning(msg, moduleName)
      character (len=*), intent(in) :: msg                
      character (len=*), intent(in) :: moduleName                
      end subroutine apsimsystem_error_warning

      subroutine summary_writeline(moduleName, msg)
      character (len=*), intent(in) :: moduleName                
      character (len=*), intent(in) :: msg                
      end subroutine summary_writeline

      subroutine summary_enter_diary_state()
      end subroutine summary_enter_diary_state
       
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
