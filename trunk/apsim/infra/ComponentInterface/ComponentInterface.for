! ====================================================================
      subroutine UseInstance(anInstanceNo)
! ====================================================================
      implicit none
      dll_export Create
      include 'const.inc'
      include 'ComponentInterface.inc'

!+ Sub-Program Arguments
      integer anInstanceNo            ! (INPUT) the particular instance number
                                      !         of this computation

!+ Purpose
!      swaps the specified instance number into memory.
 
!+  Mission Statement
 
!+ Changes
!      DPH 14/7/99

!+ Calls
 
!- Implementation Section ----------------------------------

      CurrentInstanceIndex = CurrentInstanceIndex + 1
      if (CurrentInstanceIndex .gt. MAX_NUM_INSTANCE_NOS) then
         call Fatal_error (ERR_Internal, 
     .      'Too many instances in component interface')
      else
         InstanceNoStack(CurrentInstanceIndex) = anInstanceNo
         call SwapInstance(anInstanceNo)
      endif

      return
      end 

! ====================================================================
      subroutine RestoreInstance()
! ====================================================================
      implicit none
      dll_export Create
      include 'const.inc'
      include 'ComponentInterface.inc'

!+ Sub-Program Arguments

!+ Purpose
!      restores the previous instance
 
!+  Mission Statement
 
!+ Changes
!      DPH 14/7/99

!+ Calls
 
!- Implementation Section ----------------------------------

      CurrentInstanceIndex = CurrentInstanceIndex - 1
      if (CurrentInstanceIndex .gt. 0) then
         call SwapInstance(InstanceNoStack(CurrentInstanceIndex))
      endif

      return
      end 

! ====================================================================
      recursive subroutine Create 
     .   (aName, anInstanceNo, aCallBack, aCompUnit, ID)
! ====================================================================
      implicit none
      dll_export Create
      include 'action.inc'
      include 'ComponentInterface.inc'

!+ Sub-Program Arguments
      character aName*(*)             ! (INPUT) the name of the protocol component
                                      !         owning this computational DLL 
      integer anInstanceNo            ! (INPUT) the particular instance number
                                      !         of this computation
      integer aCallBack               ! (INPUT) Callback procedure to communicate with calling process
      integer aCompUnit               ! (INPUT) pointer to an object as the subject of the callback
      integer ID                      ! (INPUT) ????

!+ Purpose
!      Initialises the DLL and establishes communication with the system
 
!+  Mission Statement
 
!+ Changes
!      DPH 14/7/99

!+ Calls
 
!- Implementation Section ----------------------------------

      CurrentInstanceIndex = 0

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
      implicit none
      dll_export Init
      include 'const.inc'
      include 'action.inc'
      include 'ComponentInterface.inc'

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
      implicit none
      dll_export Term
      include 'const.inc'
      include 'ComponentInterface.inc'
       
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
      
      return
      end
      
! ====================================================================
      recursive subroutine Action (anInstanceNo, anAction, 
     .                    aDnbytes, apData, aTnbytes, apTypDsc)
! ====================================================================
      implicit none
      dll_export Action
      include 'ComponentInterface.inc'
 
!+ Sub-Program Arguments
      integer anInstanceNo            ! (INPUT) the particular instance number
                                      !         of this computation
      character anAction*(*)          ! (INPUT) the name of the action to be executed
      integer aDnbytes                ! (INPUT) number of data bytes
      integer apData                  ! (INPUT) C string - the data of the message
      integer aTnbytes                ! (INPUT) number of type bytes
      integer apTypDsc                ! (INPUT) the type of the data with message.
 
!+ Purpose
!      Performs the specified computation with the name anAction using
!      the provided data
 
!+  Mission Statement
 
!+ Changes
!      DPH 14/7/99
 
!+ Calls
      dll_import CString2FString
      dll_import Loader_MessageUsed

!+ Constant Values
 
!+ Local Variables
      character DataString*(500)       ! FORTRAN version of C data string
 
!- Implementation Section ----------------------------------

      call CString2FString(apData, DataString)      

      ! swap in the proper instance.
      call UseInstance (anInstanceNo) 

      MessageUsed = .true.
      call Main (anAction, DataString)
      if (MessageUsed) then
         call Loader_MessageUsed()
      endif

      ! restore existing instance
      call RestoreInstance()
      
      return
      end
      
! ====================================================================
       subroutine InEvent (anInstanceNo, anAction, aDnbytes, 
     .                     apData, aTnbytes, apTypDsc)
! ====================================================================
      implicit none
      dll_export InEvent
      include 'ComponentInterface.inc'
 
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
 
!+ Calls
 
!+ Constant Values
 
!+ Local Variables
 
!- Implementation Section ----------------------------------

      
      return
      end
      
! ====================================================================
       subroutine OutEvent (anInstanceNo, anAction, aDnbytes, 
     .                      apData, aTnbytes, apTypDsc)
! ====================================================================
      implicit none
      dll_export OutEvent
      include 'ComponentInterface.inc'
      
 
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
      implicit none
      include 'ComponentInterface.inc'
 
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
      implicit none
 
!+ Sub-Program Arguments
      character ModuleName*(*)         ! (OUTPUT) current module name
 
!+ Purpose
!     message was not used by this module.

!+ Changes
!      DPH 14/7/99
 
!+ Calls
      dll_import LOADER_GETCURRENTCOMPONENT
 
!- Implementation Section ----------------------------------

      call LOADER_GETCURRENTCOMPONENT(ModuleName)
      return
      end

! ====================================================================
       subroutine Write_string (String)
! ====================================================================
      implicit none
       include 'const.inc'             ! Constant definitions
 
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
      dll_import Summary_WriteLine
  
!+ Constant Values
 
!+ Local Variables

!- Implementation Section ----------------------------------

      call Summary_WriteLine (String)

      return
      end

! ====================================================================
       subroutine  Action_Send (ModuleName, ActionName, Dat)
! ====================================================================
      implicit none
      include 'ComponentInterface.inc'
      include 'const.inc'
 
!+ Sub-Program Arguments
      character ModuleName*(*)          ! (INPUT) the module name
      character ActionName*(*)          ! (INPUT) the action name
      character Dat*(*)                 ! (INPUT) the data
 
!+ Purpose
!     send an action to all other modules

!+ Changes
!      DPH 14/7/99
 
!+ Calls
      dll_import Loader_SendAction
      dll_import Loader_SendActionToFirstComp
 
!- Implementation Section ----------------------------------

      if (ModuleName .eq. Unknown_module) then
         call Loader_SendActionToFirstComp (ActionName, Dat)
      else if (ModuleName .eq. All_active_modules) then
         call Loader_SendActionToAllComps (Actionname, Dat)
      else
         call Loader_SendAction (ModuleName, ActionName, Dat);
      endif
      return
      end

! ====================================================================
       subroutine  Action_Send_to_all_comps (ActionName)
! ====================================================================
      implicit none
      include 'ComponentInterface.inc'
 
!+ Sub-Program Arguments
      character ActionName*(*)          ! (INPUT) the event name we're sending
 
!+ Purpose
!     send an action to all other modules

!+ Changes
!      DPH 14/7/99
 
!+ Calls
      dll_import Loader_SendActionToAllComps
 
!- Implementation Section ----------------------------------

      call Loader_SendActionToAllComps (ActionName, "");
      return
      end

! ====================================================================
       subroutine  Event_Send (EventName)
! ====================================================================
      implicit none
      include 'ComponentInterface.inc'
 
!+ Sub-Program Arguments
      character EventName*(*)          ! (INPUT) the event name we're sending
 
!+ Purpose
!     send an event to another module. 
!+ Changes
!      DPH 14/7/99
 
!+ Calls
      dll_import Loader_SendEvent
 
!- Implementation Section ----------------------------------

      call Loader_SendEvent (EventName)
      return
      end
