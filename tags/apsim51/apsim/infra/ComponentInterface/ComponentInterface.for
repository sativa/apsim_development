C     Last change:  P     1 Nov 2000    9:53 am
! ====================================================================
      subroutine UseInstance(anInstanceNo)
! ====================================================================
      Use infrastructure
      implicit none

!+ Sub-Program Arguments
      integer anInstanceNo            ! (INPUT) the particular instance number
                                      !         of this computation
                     
!+ Purpose
!      swaps the specified instance number into memory.
 
!+  Mission Statement
 
!+ Changes
!      DPH 14/7/99

!+ Calls
      dll_import set_ei
 
!- Implementation Section ----------------------------------

      CurrentInstanceIndex = CurrentInstanceIndex + 1
      if (CurrentInstanceIndex .gt. MAX_NUM_INSTANCE_NOS) then
         call Fatal_error (ERR_Internal, 
     .      'Too many instances in component interface')
      else
         ! These routines are recursive.  We need to save the current
         ! value of messageused, eventinterface and componentdata.
         !  This whole "MessageUsed" thing needs
         ! a rethink.  I would prefer to have the Main routine return
         ! true or false if a message is used or not.
         SavedMessageUsed = MessageUsed
         SavedEventInterface = EventInterface
         SavedComponentData = ComponentData

         EventInterface = EventInterfaces(anInstanceNo)
         ComponentData = ComponentDatas(anInstanceNo)
         InstanceNoStack(CurrentInstanceIndex) = anInstanceNo

         call Set_EI(EventInterface)
         MessageUsed = .true.

         call SwapInstance(anInstanceNo)
      endif

      return
      end 

! ====================================================================
      subroutine RestoreInstance()
! ====================================================================
      Use infrastructure
      implicit none

!+ Sub-Program Arguments

!+ Purpose
!      restores the previous instance
 
!+  Mission Statement
 
!+ Changes
!      DPH 14/7/99

!+ Calls
      dll_import restore_ei

!- Implementation Section ----------------------------------

      call Restore_EI()
      CurrentInstanceIndex = CurrentInstanceIndex - 1
      if (CurrentInstanceIndex .gt. 0) then
         call SwapInstance(InstanceNoStack(CurrentInstanceIndex))

         MessageUsed    = SavedMessageUsed
         EventInterface = SavedEventInterface
         ComponentData  = SavedComponentData
      endif

      return
      end 

! ====================================================================
      recursive subroutine Create 
     .   (aName, anInstanceNo, aComputation, ssdl)
! ====================================================================
      Use infrastructure
      implicit none
      dll_export create

!+ Sub-Program Arguments
      character aName*(*)             ! (INPUT) the name of the protocol component
                                      !         owning this computational DLL 
      integer anInstanceNo            ! (INPUT) the particular instance number
                                      !         of this computation
      integer aComputation            ! (INPUT) pointer to a computation object
      character ssdl*(*)              ! (INPUT) SIMScript

!+ Purpose
!      Initialises the DLL and establishes communication with the system
 
!+  Mission Statement
 
!+ Changes
!      DPH 14/7/99

!+ Calls
 
!- Implementation Section ----------------------------------

      CurrentInstanceIndex = 0

      ! create the componentdata and the event interface
      EventInterfaces(anInstanceNo) = EI_CREATE(aComputation)
      ComponentDatas(anInstanceNo) = SOMComponent_create(ssdl)

      ! allocate an instance of the data
      call AllocInstance(aName, anInstanceNo)

      ! swap in the proper instance.
      call UseInstance (anInstanceNo) 

      ! call the init routine.
      call Main (ACTION_Create, "")

      ! restore existing instance
      call RestoreInstance()

      return
      end
      

! ====================================================================
      recursive subroutine Init (anInstanceNo)
! ====================================================================
      Use infrastructure
      implicit none
      dll_export init

!+ Sub-Program Arguments
      integer anInstanceNo            ! (INPUT) the particular instance number
                                      !         of this computation

!+ Purpose
!      Initialises the DLL and establishes communication with the system
 
!+  Mission Statement
 
!+ Changes
!      DPH 14/7/99

!+ Local variables
 
!- Implementation Section ----------------------------------

      ! swap in the proper instance.
      call UseInstance (anInstanceNo) 

      ! call the init routine.
      call Main (ACTION_Init, "")

      ! restore existing instance
      call RestoreInstance()
      
      return
      end
      
! ====================================================================
       subroutine Term (anInstanceNo)
! ====================================================================
      Use infrastructure
      implicit none
      dll_export term
       
!+ Sub-Program Arguments
      integer anInstanceNo            ! (INPUT) the particular instance number
                                      !         of this computation
!+ Purpose
!      Initialises the DLL and establishes communication with the system
 
!+  Mission Statement
 
!+ Changes
!      DPH 14/7/99

!+ Calls
      
!- Implementation Section ----------------------------------

      call FreeInstance(anInstanceNo)

      ! terminate the postbox stuff.
      call postbox_term()

      ! free the componentdata and the event interface
      call EI_Free(EventInterfaces(anInstanceNo))
      call SOMComponent_free(ComponentDatas(anInstanceNo))

      return
      end
      
! ====================================================================
      recursive subroutine Action (anInstanceNo, anAction, 
     .                    aDnbytes, apData, aTnbytes, apTypDsc)
! ====================================================================
      Use infrastructure
      implicit none
      dll_export action
 
!+ Sub-Program Arguments
      integer anInstanceNo            ! (INPUT) the particular instance number
                                      !         of this computation
      character anAction*(*)          ! (INPUT) the name of the action to be executed
      integer aDnbytes                ! (INPUT) number of data bytes
      character apData*(*)            ! (INPUT) string - the data of the message
      integer aTnbytes                ! (INPUT) number of type bytes
      integer apTypDsc                ! (INPUT) the type of the data with message.
 
!+ Purpose
!      Performs the specified computation with the name anAction using
!      the provided data
 
!+  Mission Statement
 
!+ Changes
!      DPH 14/7/99
 
!- Implementation Section ----------------------------------

      ! swap in the proper instance.
      call UseInstance (anInstanceNo)

      call Main (anAction, apData)

      call EI_setComponentResponded(EventInterface, MessageUsed)

      ! restore existing instance
      call RestoreInstance()

      return
      end
      
! ====================================================================
       subroutine InEvent (anInstanceNo, anAction, aDnbytes, 
     .                     apData, aTnbytes, apTypDsc)
! ====================================================================
      Use infrastructure
      implicit none
      dll_export inevent
 
!+ Sub-Program Arguments
      integer anInstanceNo            ! (INPUT) the particular instance number
                                      !         of this computation
      character anAction*(*)          ! (INPUT) the name of the action to be executed
      integer aDnbytes                ! (INPUT) number of data bytes
      integer apData                  ! (INPUT) C string - the data of the message
      integer aTnbytes                ! (INPUT) number of type bytes
      integer apTypDsc                ! (INPUT) the type of the data with message.
 
!+ Purpose
!      Executed whenever an event with name anEvent occurs (with nbytes
!      of data provided)
 
!+  Mission Statement
 
!+ Changes
!      DPH 14/7/99

!- Implementation Section ----------------------------------

      ! swap in the proper instance.
      call UseInstance (anInstanceNo) 

      call Main (anAction, apData)

      call EI_SetComponentResponded(EventInterface, MessageUsed)

      ! restore existing instance
      call RestoreInstance()
     
      return
      end
      
! ====================================================================
       subroutine OutEvent (anInstanceNo, anAction, aDnbytes, 
     .                      apData, aTnbytes, apTypDsc)
! ====================================================================
      Use infrastructure
      implicit none
      dll_export outevent
      
 
!+ Sub-Program Arguments
      integer anInstanceNo            ! (INPUT) the particular instance number
                                      !         of this computation
      character anAction*(*)          ! (INPUT) the name of the action to be executed
      integer aDnbytes                ! (INPUT) number of data bytes
      integer apData                  ! (INPUT) C string - the data of the message
      integer aTnbytes                ! (INPUT) number of type bytes
      integer apTypDsc                ! (INPUT) the type of the data with message.
 
!+ Purpose
!      signals the event anEvent to the system (with provided data)
 
!+  Mission Statement
 
!+ Changes
!      DPH 14/7/99
 
!+ Calls
       
!+ Constant Values
 
!+ Local Variables
 
!- Implementation Section ----------------------------------


      return
      end
! ====================================================================
       subroutine  Message_unused ()
! ====================================================================
      Use infrastructure
      implicit none
 
!+ Sub-Program Arguments
 
!+ Purpose
!     message was not used by this module.

!+ Changes
!      DPH 14/7/99
 
!+ Calls
 
!- Implementation Section ----------------------------------

      MessageUsed = .false.
      return
      end
      
! ====================================================================
       subroutine  Get_current_module (ModuleName)
! ====================================================================
      Use infrastructure
      implicit none
 
!+ Sub-Program Arguments
      character ModuleName*(*)         ! (OUTPUT) current module name
 
!+ Purpose
!     message was not used by this module.

!+ Changes
!      DPH 14/7/99
 
!+ Calls

!+ Constant Values
      character my_name*(*)
      parameter (my_name='Get_current_module')
 
!- Implementation Section ----------------------------------

      call Push_routine (my_name)
      call EI_GETNAME(EventInterface, ModuleName)
      call Pop_routine (my_name)
      return
      end

! ====================================================================
       subroutine Write_string (String)
! ====================================================================
      Use infrastructure
      implicit none
 
!+ Sub-Program Arguments
       character String*(*)            ! (INPUT) String to write out.
 
!+ Purpose
!      Write a string with new_line delimiters to the logical unit
!      passed in.
 
!+ Assumptions
!      Assumes a message is less than or equal to 80 chars wide.
 
!+  Mission Statement
!     Write a string to a file
 
!+ Changes
!      DPH 2/07/92
!      DPH - taken out of utility and put in report module
!     DPH 14/1/94 - Modified to allow user to select whether
!                   writing to screen is permitted.
!     jngh 5/8/94 used assign_string to trap truncations
!     170295 jngh changed * format to (a)
!     SB 22/4/98 - taken out of report and put in write dll.
!     dph 20/7/99 - uses new interface routine and moved to component interface.
 
!+ Calls
      dll_import summary_writeline
      dll_import ei_getname
  
!+ Constant Values
      character my_name*(*)
      parameter (my_name='Write_string')
 
!+ Local Variables
      character name*(100)             ! name of this component

!- Implementation Section ----------------------------------

      call Push_routine (my_name)
      call ei_getname(EventInterface, name)
      call Summary_WriteLine (name, String)
      call Pop_routine  (my_name)
      return
      end

! ====================================================================
       subroutine  Action_Send (ModuleName, ActionName, Dat)
! ====================================================================
      Use infrastructure
      implicit none
 
!+ Sub-Program Arguments
      character ModuleName*(*)          ! (INPUT) the module name
      character ActionName*(*)          ! (INPUT) the action name
      character Dat*(*)                 ! (INPUT) the data
 
!+ Purpose
!     send an action to all other modules

!+ Changes
!      DPH 14/7/99
 
!+ Calls

!+ Local Variables
      logical ok
      character msg*200
 
!- Implementation Section ----------------------------------

      if (ModuleName .eq. Unknown_module) then
         print *, 'in action_send - shouldnt be here'
         pause
         !ok = Loader_SendActionToFirstComp (ActionName, Dat)
      else if (ModuleName .eq. All_active_modules) then
         call EI_BroadcastAction (EventInterface, Actionname, Dat)
         ok = .true.
      else
         call EI_SendAction(EventInterface, ModuleName, ActionName, Dat)
         ok = .true.
      endif
      
      if (.not. ok) then
         write(msg, '(6a)')
     .      'Cannot send message to module.',
     .      new_line,
     .      'Module name = ',
     .      ModuleName,
     .      'Action = ',
     .      Actionname
         call Fatal_error (ERR_User, msg) 
      endif
      return
      end

! ====================================================================
       subroutine  Action_Send_to_all_comps (ActionName)
! ====================================================================
      Use infrastructure
      implicit none
 
!+ Sub-Program Arguments
      character ActionName*(*)          ! (INPUT) the event name we're sending
 
!+ Purpose
!     send an action to all other modules

!+ Changes
!      DPH 14/7/99
 
!+ Calls
 
!- Implementation Section ----------------------------------

      call EI_BroadcastAction (EventInterface, ActionName, "");
      return
      end

! ====================================================================
       subroutine  Event_Send (EventName)
! ====================================================================
      Use infrastructure
      implicit none
 
!+ Sub-Program Arguments
      character EventName*(*)          ! (INPUT) the event name we're sending
 
!+ Purpose
!     send an event to another module. 
!+ Changes
!      DPH 14/7/99
 
!+ Calls

!+ Local variables
      character name*(100)             ! name of this component

!- Implementation Section ----------------------------------

      call ei_getname(EventInterface, name)
      call post_char_var ('sender'
     :                     , '()'
     :                     , name)
      call EI_PublishEvent (EventInterface, EventName)
      return
      end
