module ComponentInterfaceModule
   implicit none
                  
   integer ,parameter :: MAX_NAME_SIZE = 100
   integer ,parameter :: MAX_VARNAME_SIZE = 100
   integer ,parameter :: MAX_NUM_INSTANCE_NOS = 50
   integer ,parameter :: MAX_LEVELS_RECURSION = 20

   ! get_simulation_information kind parameters.
   integer, parameter :: respondToGetInfo = 2
   integer, parameter :: respondToSetInfo = 3
   integer, parameter :: respondToGetSetInfo = 4
   integer, parameter :: eventInfo = 5
   integer, parameter :: respondToEventInfo = 6
   integer, parameter :: respondToMethodInfo = 7
   integer, parameter :: componentInfo = 8
   integer, parameter :: methodCallInfo = 9

   interface
      function add_registration(kind, name, typeString, alias, componentNameOrID)
      ml_external add_registration
      integer, intent(in)           :: kind
      character (len=*), intent(in) :: name
      character (len=*), intent(in) :: typeString
      character (len=*), intent(in) :: alias
      character (len=*), intent(in) :: componentNameOrID
      integer                       :: add_registration

      end function add_registration

      subroutine error(msg, isFatal)
      ml_external error
      character (len=*), intent(in) :: msg
      logical          , intent(in) :: isFatal
      end subroutine error

      subroutine terminate_simulation()
      ml_external terminate_simulation
      end subroutine terminate_simulation

      subroutine message_unused()
      ml_external message_unused
      end subroutine message_unused

      subroutine get_name(name)
      ml_external get_name
      character (len=*), intent(in out) :: name
      end subroutine get_name

!      function get_variable_internal(registrationID, value, optionalVar)
!      ml_external get_variable_internal
!      integer, intent(in)  :: registrationID
!      integer, intent(out) :: value
!      logical, intent(in)  :: optionalVar
!      logical              :: get_variable_internal
!      end function get_variable_internal
!
!      function get_variables(registrationID, values)
!      ml_external get_variables
!      integer, intent(in)  :: registrationID
!      integer, intent(out) :: values
!      logical              :: get_variables
!      end function get_variables
!
      subroutine write_string(st)
      ml_external write_string
      character (len=*), intent(in) :: st
      end subroutine write_string

      function read_parameter(parameterName, sectionName, value, isOptional)
      ml_external read_parameter
      character(len=*), intent(in) :: parameterName
      character(len=*), intent(in) :: sectionName
      character(len=*), intent(in out) :: value
      logical                          :: isOptional
      logical                          :: read_parameter
      end function

      function get_componentData( )
      ml_external get_componentData
      integer :: get_componentData
      end function                                          

      function get_componentID( )
      ml_external get_componentID
      integer :: get_componentID
      end function                                          

      subroutine apsimcomponentdata_getrulenames(componentData,      &
                                                 ruleNames,          &
                                                 maxNumRules,        &
                                                 numRules) 
      ml_external ApsimComponentData_getRuleNames
      integer, intent(in)          :: componentData
      character(len=*), intent(in) :: ruleNames(*)        
      integer, intent(in)          :: maxNumRules
      integer, intent(in)          :: numRules
      end subroutine       
      
      subroutine apsimcomponentdata_loadrule(componentData,          &
                                             ruleNames)
      ml_external ApsimComponentData_loadRule
      integer, intent(in)          :: componentData
      character(len=*), intent(in) :: ruleNames(*)        
      end subroutine
      
      function ApsimComponentData_getNumRuleLines( )
      ml_external ApsimComponentData_getNumRuleLines
      integer :: apsimcomponentdata_getnumrulelines
      end function
       
      subroutine ApsimComponentData_getRuleLine(lineNumber, Line)
      ml_external ApsimComponentData_getRuleLine 
      integer, intent(in) :: lineNumber        
      character(len=*), intent(in out) :: line
      end subroutine
                          
!      subroutine send_message(amessage)
!      use ProtocolModule
!      ml_external send_message
!      type(Message), intent(in) :: amessage
!      end subroutine send_message
!
!
!      subroutine get_registration_type_string(registrationID, typeString)
!      ml_external get_registration_type_string
!      integer         , intent(in)  :: registrationID
!      character(len=*), intent(out) :: typeString
!      end subroutine get_registration_type_string

       
       subroutine new_postbox( )
       ml_external new_postbox
       end subroutine new_postbox

       subroutine delete_postbox( )
       ml_external delete_postbox
       end subroutine delete_postbox
       
       function store_in_postbox(str)
       ml_external store_in_postbox
       character(len=*), intent(in) :: str
       logical :: store_in_postbox
       end function store_in_postbox                                    
                        
       function get_posting_module()
       ml_external get_posting_module
       integer :: get_posting_module
       end function

       function component_id_to_name(id, name)
       ml_external component_id_to_name 
       integer, intent(in)              :: id
       character(len=*), intent(in out) :: name
       logical                          :: component_id_to_name
       end function component_id_to_name

       function component_name_to_id(name, id)
       ml_external component_name_to_id 
       character(len=*), intent(in) :: name
       integer, intent(out)         :: id
       logical                      :: component_name_to_id
       end function component_name_to_id
                          
       subroutine event_send(eventName)
       ml_external event_send   
       character(len=*), intent(in) :: eventName
       end subroutine event_send   
       
!       subroutine action_send_to_all_comps(actionName)
!       ml_external action_send_to_all_comps
!       character(len=*), intent(in) :: actionName
!       end subroutine action_send_to_all_comps

       subroutine action_send(moduleName, actionName)
       ml_external action_send
       character(len=*), intent(in) :: moduleName
       character(len=*), intent(in) :: actionName
       end subroutine action_send

       subroutine get_integer_var(componentID, variableName, units, value, numvals, lower, upper)   
       ml_external get_integer_var
       integer, intent(in)          :: componentID
       character(len=*), intent(in) :: variableName
       character(len=*), intent(in) :: units
       integer, intent(in out)         :: value         
       integer, intent(in out)      :: numvals
       integer, intent(in)             :: lower
       integer, intent(in)             :: upper
       end subroutine get_integer_var 

       subroutine get_real_var(componentID, variableName, units, value, numvals, lower, upper)   
       ml_external get_real_var
       integer, intent(in)          :: componentID
       character(len=*), intent(in) :: variableName
       character(len=*), intent(in) :: units
       real, intent(in out)         :: value         
       integer, intent(in out)      :: numvals
       real, intent(in)             :: lower
       real, intent(in)             :: upper
       end subroutine get_real_var 
       
       subroutine get_real_array(componentID, variableName, arraySize, units, value, numvals, lower, upper)   
       ml_external get_real_array
       integer, intent(in)          :: componentID
       character(len=*), intent(in) :: variableName
       integer, intent(in)          :: arraySize
       character(len=*), intent(in) :: units
       real, intent(in out)         :: value(*)
       integer, intent(in out)      :: numvals
       real, intent(in)             :: lower
       real, intent(in)             :: upper
       end subroutine get_real_array 
       
       subroutine get_real_array_optional(componentID, variableName, arraySize, units, value, numvals, lower, upper)   
       ml_external get_real_array_optional
       integer, intent(in)          :: componentID
       character(len=*), intent(in) :: variableName
       integer, intent(in)          :: arraySize
       character(len=*), intent(in) :: units
       real, intent(in out)         :: value(*)         
       integer, intent(in out)      :: numvals
       real, intent(in)             :: lower
       real, intent(in)             :: upper
       end subroutine get_real_array_optional 
       
       subroutine get_real_var_optional(componentID, variableName, units, value, numvals, lower, upper)   
       ml_external get_real_var_optional
       integer, intent(in)          :: componentID
       character(len=*), intent(in) :: variableName
       character(len=*), intent(in) :: units
       real, intent(in out)         :: value         
       integer, intent(in out)      :: numvals
       real, intent(in)             :: lower
       real, intent(in)             :: upper
       end subroutine get_real_var_optional 
                          
       subroutine get_real_vars(requestNo, variableName, units, value, numvals, lower, upper)
       ml_external get_real_vars
       integer, intent(in)          :: requestNo
       character(len=*), intent(in) :: variableName
       character(len=*), intent(in) :: units
       real, intent(in out)         :: value         
       integer, intent(in out)      :: numvals
       real, intent(in)             :: lower
       real, intent(in)             :: upper
       end subroutine get_real_vars 
                          

       subroutine get_double_var(componentID, variableName, units, value, numvals, lower, upper)   
       ml_external get_double_var
       integer, intent(in)              :: componentID
       character(len=*), intent(in)     :: variableName
       character(len=*), intent(in)     :: units
       double precision, intent(in out) :: value         
       integer, intent(in out)          :: numvals
       double precision, intent(in)     :: lower
       double precision, intent(in)     :: upper
       end subroutine get_double_var 

       subroutine get_double_var_optional(componentID, variableName, units, value, numvals, lower, upper)   
       ml_external get_double_var_optional
       integer, intent(in)          :: componentID
       character(len=*), intent(in) :: variableName
       character(len=*), intent(in) :: units
       double precision, intent(in out) :: value         
       integer, intent(in out)          :: numvals
       double precision, intent(in)     :: lower
       double precision, intent(in)     :: upper
       end subroutine get_double_var_optional 

       subroutine get_double_vars(requestNo, variableName, units, value, numvals, lower, upper)
       ml_external get_double_vars
       integer, intent(in)          :: requestNo
       character(len=*), intent(in) :: variableName
       character(len=*), intent(in) :: units
       double precision, intent(in out) :: value         
       integer, intent(in out)          :: numvals
       double precision, intent(in)     :: lower
       double precision, intent(in)     :: upper
       end subroutine get_double_vars 

       subroutine get_double_array(componentID, variableName, arraySize, units, value, numvals, lower, upper)   
       ml_external get_double_array
       integer, intent(in)          :: componentID
       character(len=*), intent(in) :: variableName
       integer, intent(in)          :: arraySize
       character(len=*), intent(in) :: units
       double precision, intent(in out) :: value(*)
       integer, intent(in out)          :: numvals
       double precision, intent(in)     :: lower
       double precision, intent(in)     :: upper
       end subroutine get_double_array 

       subroutine get_char_var(componentID, variableName, units, value, numvals)   
       ml_external get_char_var
       integer, intent(in)              :: componentID
       character(len=*), intent(in)     :: variableName
       character(len=*), intent(in)     :: units
       character(len=*), intent(in out) :: value         
       integer, intent(in out)          :: numvals
       end subroutine get_char_var 

       subroutine get_char_var_optional(componentID, variableName, units, value, numvals)   
       ml_external get_char_var_optional
       integer, intent(in)          :: componentID
       character(len=*), intent(in)     :: variableName
       character(len=*), intent(in)     :: units
       character(len=*), intent(in out) :: value         
       integer, intent(in out)          :: numvals
       end subroutine get_char_var_optional 

       subroutine get_char_vars(requestNo, variableName, units, value, numvals)
       ml_external get_char_vars
       integer, intent(in)          :: requestNo
       character(len=*), intent(in) :: variableName
       character(len=*), intent(in) :: units
       character(len=*), intent(in out) :: value         
       integer, intent(in out)      :: numvals
       end subroutine get_char_vars 

       subroutine set_char_var(componentID, variableName, units, value)   
       ml_external set_char_var
       integer, intent(in)              :: componentID
       character(len=*), intent(in)     :: variableName
       character(len=*), intent(in)     :: units
       character(len=*), intent(in)     :: value         
       end subroutine set_char_var 
                                 
       subroutine set_real_array(componentID, variableName, units, value, numvals)                                 
       ml_external set_real_array
       integer, intent(in)              :: componentID
       character(len=*), intent(in)     :: variableName
       character(len=*), intent(in)     :: units
       real, intent(in)                 :: value(*)
       integer, intent(in)              :: numvals         
       end subroutine set_real_array 
       
       subroutine set_double_array(componentID, variableName, units, value, numvals)                                 
       ml_external set_double_array
       integer, intent(in)              :: componentID
       character(len=*), intent(in)     :: variableName
       character(len=*), intent(in)     :: units
       double precision, intent(in)     :: value(*)
       integer, intent(in)              :: numvals         
       end subroutine set_double_array 
       
       
       subroutine respond2get_integer_var(variableName, units, value)   
       ml_external respond2get_integer_var
       character(len=*), intent(in) :: variableName
       character(len=*), intent(in) :: units
       integer, intent(in) :: value
       end subroutine respond2get_integer_var

       subroutine respond2get_integer_array(variableName, units, value, numvals)   
       ml_external respond2get_integer_array
       character(len=*), intent(in) :: variableName
       character(len=*), intent(in) :: units
       integer, intent(in) :: value(*)          
       integer, intent(in) :: numvals
       end subroutine respond2get_integer_array

       subroutine respond2get_real_var(variableName, units, value)   
       ml_external respond2get_real_var
       character(len=*), intent(in) :: variableName
       character(len=*), intent(in) :: units
       real, intent(in) :: value
       end subroutine respond2get_real_var
  
       subroutine respond2get_real_array(variableName, units, value, numvals)   
       ml_external respond2get_real_array
       character(len=*), intent(in) :: variableName
       character(len=*), intent(in) :: units
       real, intent(in) :: value(*)          
       integer, intent(in) :: numvals
       end subroutine respond2get_real_array
  
       subroutine respond2get_double_var(variableName, units, value)   
       ml_external respond2get_double_var
       character(len=*), intent(in) :: variableName
       character(len=*), intent(in) :: units
       double precision, intent(in) :: value
       end subroutine respond2get_double_var 

       subroutine respond2get_double_array(variableName, units, value, numvals)   
       ml_external respond2get_double_array
       character(len=*), intent(in) :: variableName
       character(len=*), intent(in) :: units
       double precision, intent(in) :: value(*)          
       integer, intent(in) :: numvals
       end subroutine respond2get_double_array

       subroutine respond2get_logical_var(variableName, units, value)   
       ml_external respond2get_logical_var
       character(len=*), intent(in) :: variableName
       character(len=*), intent(in) :: units
       logical, intent(in) :: value
       end subroutine respond2get_logical_var

       subroutine respond2get_char_var(variableName, units, value)   
       ml_external respond2get_char_var
       character(len=*), intent(in) :: variableName
       character(len=*), intent(in) :: units
       character(len=*), intent(in) :: value
       end subroutine respond2get_char_var

       subroutine respond2get_char_array(variableName, units, value, numvals)   
       ml_external respond2get_char_array
       character(len=*), intent(in) :: variableName
       character(len=*), intent(in) :: units
       character(len=*), intent(in) :: value(*)          
       integer, intent(in) :: numvals
       end subroutine respond2get_char_array

       subroutine post_integer_var(variableName, units, value)   
       ml_external post_integer_var
       character(len=*), intent(in) :: variableName
       character(len=*), intent(in) :: units
       integer, intent(in) :: value
       end subroutine post_integer_var 

       subroutine post_real_var(variableName, units, value)   
       ml_external post_real_var
       character(len=*), intent(in) :: variableName
       character(len=*), intent(in) :: units
       real, intent(in) :: value
       end subroutine post_real_var 

       subroutine post_real_array(variableName, units, value, numvals)   
       ml_external post_real_array
       character(len=*), intent(in) :: variableName
       character(len=*), intent(in) :: units
       real, intent(in) :: value(*) 
       integer, intent(in) :: numvals
       end subroutine post_real_array 

       subroutine post_double_var(variableName, units, value)   
       ml_external post_double_var
       character(len=*), intent(in) :: variableName
       character(len=*), intent(in) :: units
       double precision, intent(in) :: value
       end subroutine post_double_var 

       subroutine post_double_array(variableName, units, value, numvals)   
       ml_external post_double_array
       character(len=*), intent(in) :: variableName
       character(len=*), intent(in) :: units
       double precision, intent(in) :: value(*) 
       integer, intent(in) :: numvals
       end subroutine post_double_array 

       subroutine post_char_var(variableName, units, value)   
       ml_external post_char_var
       character(len=*), intent(in) :: variableName
       character(len=*), intent(in) :: units
       character(len=*), intent(in) :: value
       end subroutine post_char_var 

       subroutine post_char_array(variableName, units, value, numvals)   
       ml_external post_char_array
       character(len=*), intent(in) :: variableName
       character(len=*), intent(in) :: units
       character(len=*), intent(in) :: value(*)
       integer, intent(in)          :: numvals
       end subroutine post_char_array 

       subroutine collect_char_var(variableName, units, value, numvals)   
       ml_external collect_char_var
       character(len=*), intent(in)     :: variableName
       character(len=*), intent(in)     :: units
       character(len=*), intent(in out) :: value
       integer, intent(in out)          :: numvals
       end subroutine collect_char_var 

       subroutine collect_char_var_optional(variableName, units, value, numvals)   
       ml_external collect_char_var_optional
       character(len=*), intent(in)     :: variableName
       character(len=*), intent(in)     :: units
       character(len=*), intent(in out) :: value
       integer, intent(in out)          :: numvals
       end subroutine collect_char_var_optional 
       
       subroutine collect_char_array(variableName, arraySize, units, value, numvals)   
       ml_external collect_char_array
       character(len=*), intent(in)     :: variableName
       integer, intent(in)              :: arraySize
       character(len=*), intent(in)     :: units
       character(len=*), intent(in out) :: value(*)
       integer, intent(in out)          :: numvals
       end subroutine collect_char_array 
       
       subroutine collect_real_var(variableName, units, value, numvals, lower, upper)   
       ml_external collect_real_var
       character(len=*), intent(in)     :: variableName
       character(len=*), intent(in)     :: units
       real, intent(in out)             :: value
       integer, intent(in out)          :: numvals
       real, intent(in)                 :: lower
       real, intent(in)                 :: upper
       end subroutine collect_real_var 

       subroutine collect_real_var_optional(variableName, units, value, numvals, lower, upper)   
       ml_external collect_real_var_optional
       character(len=*), intent(in)     :: variableName
       character(len=*), intent(in)     :: units
       real, intent(in out)             :: value
       integer, intent(in out)          :: numvals
       real, intent(in)                 :: lower
       real, intent(in)                 :: upper
       end subroutine collect_real_var_optional 

       subroutine collect_real_array(variableName, arraySize, units, value, numvals, lower, upper)   
       ml_external collect_real_array
       character(len=*), intent(in)     :: variableName
       integer, intent(in)              :: arraySize
       character(len=*), intent(in)     :: units
       real, intent(in out)             :: value(*)
       integer, intent(in out)          :: numvals
       real, intent(in)                 :: lower
       real, intent(in)                 :: upper
       end subroutine collect_real_array 

       subroutine collect_real_array_optional(variableName, arraySize, units, value, numvals, lower, upper)   
       ml_external collect_real_array_optional
       character(len=*), intent(in)     :: variableName
       integer, intent(in)              :: arraySize
       character(len=*), intent(in)     :: units
       real, intent(in out)             :: value(*)
       integer, intent(in out)          :: numvals
       real, intent(in)                 :: lower
       real, intent(in)                 :: upper
       end subroutine collect_real_array_optional
        
       subroutine collect_integer_var(variableName, units, value, numvals, lower, upper)   
       ml_external collect_integer_var
       character(len=*), intent(in)     :: variableName
       character(len=*), intent(in)     :: units
       integer, intent(in out)          :: value
       integer, intent(in out)          :: numvals
       integer, intent(in)              :: lower
       integer, intent(in)              :: upper
       end subroutine collect_integer_var 

       subroutine collect_double_var(variableName, units, value, numvals, lower, upper)   
       ml_external collect_double_var
       character(len=*), intent(in)     :: variableName
       character(len=*), intent(in)     :: units
       double precision, intent(in out) :: value
       integer, intent(in out)          :: numvals
       double precision, intent(in)     :: lower
       double precision, intent(in)     :: upper
       end subroutine collect_double_var 

       subroutine collect_double_var_optional(variableName, units, value, numvals, lower, upper)   
       ml_external collect_double_var_optional
       character(len=*), intent(in)     :: variableName
       character(len=*), intent(in)     :: units
       double precision, intent(in out) :: value
       integer, intent(in out)          :: numvals
       double precision, intent(in)     :: lower
       double precision, intent(in)     :: upper
       end subroutine collect_double_var_optional 

       subroutine collect_double_array(variableName, arraySize, units, value, numvals, lower, upper)   
       ml_external collect_double_array
       character(len=*), intent(in)     :: variableName
       integer, intent(in)              :: arraySize
       character(len=*), intent(in)     :: units
       double precision, intent(in out) :: value(*)
       integer, intent(in out)          :: numvals
       double precision, intent(in)     :: lower
       double precision, intent(in)     :: upper
       end subroutine collect_double_array 

       subroutine change_component_order(module_list, numvals)
       ml_external change_component_order
       character(len=*), intent(in)     :: module_list(*)
       integer, intent(in)              :: numvals
       end subroutine change_component_order
       
   end interface

end module ComponentInterfaceModule
