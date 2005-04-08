      module sorghummodule
      use croplibrary
      use cropmoddata
      use Registrations

      contains

      include 'cropmodcomms.for'
      include 'sorgmain.for'
      include 'sorgn.for'
      include 'sorgopt.for'
      include 'sorgtree.for'

      end module


!     ===========================================================
      subroutine alloc_dealloc_instance(doAllocate)
!     ===========================================================
      use sorghummodule
      implicit none
      ml_external alloc_dealloc_instance

!+  Sub-Program Arguments
      logical, intent(in) :: doAllocate

!+  Purpose
!      Module instantiation routine.

!- Implementation Section ----------------------------------

      if (doAllocate) then
         allocate(g)
         allocate(p)
         allocate(c)
         allocate(id)
      else
         deallocate(g)
         deallocate(p)
         deallocate(c)
         deallocate(id)
      end if
      return
      end subroutine


*=====================================================================
      subroutine Main (action, data_string)
*=====================================================================
      Use SorghumModule
      Use infrastructure
      implicit none
      ml_external Main


*+  Sub-Program Arguments
      character  action*(*)            ! (INPUT) Message action to perform
      character  data_string*(*)       ! (INPUT) Message data

*+  Purpose
*     This module performs crop growth simulation simulates crop phenological
*     development, growth of root, leaf, head, stem and grain,
*     Water and  nitrogen uptake, leaf and root senescense.

*+  Changes
*      271198 ew
*      250894 sc    specified and programmed
*      011195 jngh  added call to message_unused

*+  Calls

*+  Constant Values
      character  my_name*(*)         ! name of this procedure
      parameter (my_name='CropMod')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (action.eq.ACTION_init) then

         !Zero pools inlcuding contants and parameters
         call Zero_Variables (.true.)

         !Read the crop specific contants from ini files
         call CropMod_Initialisation ()

         !Request and receive variables from owner-modules
         call Get_Other_Variables ()

      else if (Action.eq.ACTION_Create) then
         call doRegistrations(id)
         call doSysbalRegistrations()

      elseif (action.eq.ACTION_set_variable) then

         ! Respond to request to reset variable values of variables from other modules
         call Set_My_Variable (data_string)

      elseif (action.eq.ACTION_get_variable) then

         !Respond to request for variable values - from other modules
         call Send_My_Variable (Data_string)

      elseif (action.eq.ACTION_prepare) then !this happens each time step (daily)

         if (g%plant_status.ne.status_out) then

            !Zero all daily rate variables
            call Zero_Daily_Variables ()

            !Request and receive variables from owner-modules
            call Get_Other_Variables ()

            !Do crop processes prepare
            call Simulation_Prepare ()

         else
            ! Crop not in the field, do nothing
            call Zero_Variables (.false.)
         endif

      elseif (action.eq.ACTION_process) then


         if (g%plant_status.ne.status_out) then

            !request and receive variables from owner-modules
            call Get_Other_Variables ()

            !Crop processes - Dynamic prcess sub-modules
            call Crop_Process ()

            !Send changes of other variables to owner-modules
            call Set_Other_Variables ()

         else
            !crop not in, do nothing
         endif

      elseif (action.eq.ACTION_sow) then

         if (crop_my_type (c%crop_type)) then

            !request and receive variables from owner-modules
            call Get_Other_Variables ()

            !start crop, read the sow information and do  more initialisations
            call Start_Crop ()

         else

            ! not my type!
            call Message_unused ()

         endif

      elseif (action.eq.ACTION_harvest) then

         if (Crop_my_type (c%crop_type)) then
               ! harvest crop - report harvest information
               call Crop_Harvest (
     .          g%dm_green,
     .          g%dm_dead,
     .          c%grn_water_cont,
     .          g%grain_no,
     .          g%plants,
     .          g%dm_senesced,
     .          g%leaf_no,
     .          g%N_green,
     .          g%N_dead,
     .          g%N_senesced,
     .          g%flowering_date,
     .          g%maturity_date,
     .          g%flowering_das,
     .          g%maturity_das,
     .          g%lai_max,
     .          g%cswd_photo,
     .          g%days_tot,
     .          g%cswd_expansion,
     .          g%cnd_photo,
     .          g%cnd_grain_conc,
     .          c%stage_names)
         else
            ! not my type!
            call Message_unused ()
         endif

      elseif (action.eq.ACTION_end_crop) then

         if (crop_my_type (c%crop_type)) then

            !end crop - turn the stover into residue
            call End_Crop ()

            !Zero all the globals, but not the contants and parameters
            !call Zero_Variables (.false.)

            !Set plant status to status_out and stage to plant_end subroutine
            if (g%plant_status.ne.status_out) then
                g%plant_status  = status_out
                g%current_stage = real (plant_end)
            end if

         else
            ! not my type!
            call Message_unused ()
         endif


      elseif (action.eq.ACTION_kill_crop) then
         if (crop_my_type (c%crop_type)) then
            ! kill crop - died, but biomass remain in field
            call Kill_Crop (
     .          g%plant_status,
     .          g%dm_green,
     .          g%dm_senesced,
     .          g%dm_dead)
         else
            ! not my type!
            call Message_unused ()
         endif
      else
         ! don't use message
         call Message_unused ()
      endif


      call pop_routine (my_name)
      return
      end subroutine

! ====================================================================
! This routine is the event handler for all events
! ====================================================================
      subroutine respondToEvent(fromID, eventID, variant)
      Use infrastructure
      implicit none
      ml_external respondToEvent

      integer, intent(in) :: fromID
      integer, intent(in) :: eventID
      integer, intent(in) :: variant

      return
      end subroutine respondToEvent

! ====================================================================
! This routine registers variables needec by sysbal
! ====================================================================
      subroutine doSysbalRegistrations()
      use Infrastructure
      integer :: id
      character DDML*128

      DDML = '<type kind="single"  array="T">'

         ! WATER
      id = add_registration(respondToGetReg, 'ep'
     :                        , singleTypeDDML, '', '')

         ! P
      id = add_registration(respondToGetReg, 'p_green'
     :                        , DDML , '', '')
      id = add_registration(respondToGetReg, 'p_senesced'
     :                        , DDML, '', '')
      id = add_registration(respondToGetReg, 'p_dead'
     :                        , DDML, '', '')

         ! N
      id = add_registration(respondToGetReg, 'n_green'
     :                        , DDML, '', '')
      id = add_registration(respondToGetReg, 'n_senesced'
     :                        , DDML, '', '')
      id = add_registration(respondToGetReg, 'n_dead'
     :                        , DDML, '', '')

         ! DM
      id = add_registration(respondToGetReg, 'dm_green'
     :                        , DDML, '', '')
      id = add_registration(respondToGetReg, 'dm_senesced'
     :                        , DDML, '', '')
      id = add_registration(respondToGetReg, 'dm_dead'
     :                        , DDML, '', '')
      id = add_registration(respondToGetReg, 'dlt_dm_green'
     :                        , DDML, '', '')

      return
      end subroutine
