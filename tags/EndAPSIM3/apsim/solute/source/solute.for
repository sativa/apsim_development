      include 'Solute.inc'

!     ===========================================================
      subroutine alloc_dealloc_instance(doAllocate)
!     ===========================================================
      use SoluteModule
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
         allocate(ID)

      else
         deallocate(g)
         deallocate(p)
         deallocate(c)
         deallocate(ID)

      end if
      return
      end

!     ===========================================================
      subroutine do_init1(sdml)
!     ===========================================================
      use SoluteModule
      implicit none
      ml_external do_init1

!+  Purpose
!      Perform all registrations and zeroing

!+  Sub-Program Arguments
      character (len=*), intent(in) :: sdml

!- Implementation Section ----------------------------------

      call do_registrations(ID)
      call solute_zero_variables()
      call solute_read_constants ()
      call solute_read_param ()

      return
      end

!     ===========================================================
      subroutine do_commence()
!     ===========================================================
      implicit none
      ml_external do_commence

!+  Purpose
!      Perform all registrations and zeroing

!- Implementation Section ----------------------------------

      return
      end

* ====================================================================
      subroutine do_init2 ()
* ====================================================================
      use SoluteModule
      use ComponentInterfaceModule
      implicit none
      ml_external do_init2

*+  Purpose
*     Initialise the apswim module

*+  Changes

*+  Calls

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'solute_init')

*+  Local Variables
       character Event_string*40       ! String to output

*- Implementation Section ----------------------------------

      call push_routine (myname)

      Event_string = 'Initialising'
      call Write_string (Event_string)

cnh Cannot do it here as we may not have soil layer specs yet
c      call publish_SoluteProfile(ID%SolutesChanged,
c     :     g%solute_profiles, g%num_solutes, .false.)
     
      call pop_routine (myname)
      return
      end


!     ===========================================================
      subroutine respondToEvent(fromID, eventID, variant)
!     ===========================================================
      use SoluteModule
      use ComponentInterfaceModule
      implicit none
      ml_external respondToEvent

!+  Purpose
!      Event handler for all events coming into module.

!+  Sub-Program Arguments
      integer, intent(in) :: fromID
      integer, intent(in) :: eventID
      integer, intent(in) :: variant

!- Implementation Section ----------------------------------

      if (eventID .eq. ID%SoluteFluxesCalculated) then
         call on_solute_fluxes_calculated(variant)

      else if (eventID .eq. ID%SoilWaterProfileChanged) then
         call on_soil_water_profile_changed(variant)
         
      else
         call error('bad event ID',.true.)
      endif
      return
      end
!     ===========================================================
      subroutine respondToMethod(fromID, methodID, variant)
!     ===========================================================
      use SoluteModule
      use ComponentInterfaceModule
      implicit none
      ml_external respondToMethod

!+  Purpose
!      Method handler for all method calls coming into module.

!+  Sub-Program Arguments
      integer, intent(in) :: fromID
      integer, intent(in) :: methodID
      integer, intent(in) :: variant

!- Implementation Section ----------------------------------

      call error('bad method ID',.true.)

      return
      end

!     ===========================================================
      subroutine notify_termination()
!     ===========================================================
      use SoluteModule
      implicit none
      ml_external notify_termination

!+  Purpose
!      Prepare for termination

!- Implementation Section ----------------------------------

      return
      end

* ====================================================================
       subroutine respondToGet (fromID, Variable_info)
* ====================================================================
      use SoluteModule
      use ComponentInterfaceModule
      implicit none
      ml_external respondToGet

*+  Sub-Program Arguments
      integer, intent(in) :: fromID
      type(QueryData), intent(in) :: variable_info

*+  Purpose
*      Return the value of one of our variables to caller

*+  Changes
*       29/08/97 NIH check for output unknown solute for 'flow_' and others
*       02/11/99 jngh removed crop_cover

*+  Calls

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'solute_send_my_variable')

*+  Local Variables
       integer layer
       integer solnum
       real sol(max_layer)
       logical found

*- Implementation Section ----------------------------------

      call push_routine (myname)

      found = .false.

      do 200 solnum = 1,g%num_solutes

         if (Variable_info%id .eq. g%solute_ids(solnum)) then

            if (g%num_layers.eq.0) then
               ! water balance is not initialised yet
               g%num_layers = 1
            else
            endif

            do 100 layer = 1,max_layer
               sol(layer) = g%solute_profiles(solnum)
     :                              %layer(layer)%amount
  100       continue

            call return_solute(Variable_info,
     :                         sol,
     :                        g%num_layers)

            found = .true.

         elseif (Variable_info%id .eq. g%solute_ppm_ids(solnum)) then

            if (g%num_layers.eq.0) then
               ! water balance is not initialised yet
               g%num_layers = 1
            else
            endif

            do layer = 1,max_layer

               sol(layer) = g%solute_profiles(solnum)
     :                          %layer(layer)%amount
     :              * divide (100.0
     :              ,g%soil_water_profile_layers(layer)%BulkDensity *
     :              g%soil_water_profile_layers(layer)%thickness
     :                             ,0.0)






            enddo

            call return_solute_ppm(Variable_info,
     :                             sol,
     :                             g%num_layers)

            found = .true.
         else

         endif
  200 continue

      if (.not. found) then
         ! We have checked all solutes and we did not respond to anything
         call error('Bad solute id', .true.)
      else
         ! we found the solute so no error
      endif

      call pop_routine (myname)

      return
      end
* ====================================================================
      logical function respondToSet (fromID, VariableID, variant)
* ====================================================================
      use SoluteModule
      use ComponentInterfaceModule

      implicit none
      ml_external respondToSet

!+  Sub-Program Arguments
      integer, intent(in) :: fromID
      integer, intent(in)     :: VariableID
      integer, intent(in out) :: variant


*+  Purpose
*       Set one of our variables altered by some other module.
*       Solute information is stored in a two dimensional array
*       so for desired solute, read updated layer information into a
*       single dimension array and update into the two dimensional
*       array.

*+  Changes
*      21-06-96 NIH Changed respond2set calls to collect calls

*+  Calls

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'solute_set_my_variable')

*+  Local Variables
      integer layer
      integer solnum
      integer numvals                  ! number of values returned
      real sol(max_layer)
      real dlt_sol(max_layer)
      logical found

*- Implementation Section ----------------------------------

      call push_routine (myname)
      found = .false.

      do 200 solnum = 1, g%num_solutes

         if (VariableID .eq. g%solute_ids(solnum)) then

            call Unpack_solute (Variant,
     :                 sol,
     :                 numvals)

            do 100 layer = 1, numvals
               g%solute_profiles(solnum)%layer(layer)%amount 
     :             = sol(layer)
  
  100       continue
            found = .true.

         elseif (VariableID .eq. g%solute_ppm_ids(solnum)) then

            call Unpack_solute_ppm (Variant,
     :                 sol,
     :                 numvals)

            do 300 layer = 1, numvals
               g%solute_profiles(solnum)%layer(layer)%amount 
     :                                                 = sol(layer)
     :      *  divide (g%soil_water_profile_layers(layer)%BulkDensity*
     :       g%soil_water_profile_layers(layer)%thickness, 100., 0.0)
  300       continue
            found = .true.


         else if (VariableID .eq. g%solute_dlt_ids(solnum)) then

            call Unpack_dlt_solute (Variant,
     :                 dlt_sol,
     :                 numvals)


            do 150 layer = 1, numvals
            g%solute_profiles(solnum)%layer(layer)%amount
     :        = g%solute_profiles(solnum)%layer(layer)%amount
     :                            + dlt_sol(layer)
  150       continue
            found = .true.

         else

         endif

  200 continue

      if (.not.found) then
         call error('Cannot set unknown solute', .true.)
      endif
      respondToSet = .true.
      call pop_routine (myname)
      return
      end

* ====================================================================
      subroutine on_solute_fluxes_calculated(variant) 
* ====================================================================
      use SoluteModule
      use ComponentInterfaceModule

      implicit none

!+  Sub-Program Arguments
      integer, intent(in out) :: variant


*+  Purpose

*+  Changes
*      18-02-02 DSG Added

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'on_solute_fluxes_calculated')

*+  Local Variables
      type(SoluteProfileType),dimension(max_solutes)::solute_profiles
      integer num_profiles
      integer profilenum
      integer solnum
      logical found

*- Implementation Section ----------------------------------

      call push_routine (myname)
       found = .false.
      call unpack_SoluteProfile(variant,solute_profiles,num_profiles)

      do 200 profilenum = 1, num_profiles
      
        do 100 solnum = 1,g%num_solutes
        
        if (solute_profiles(profilenum)%name.eq.
     :      g%solute_profiles(solnum)%name) then
            g%solute_profiles(solnum) = solute_profiles(profilenum)
            found = .true.
        endif      

  100 continue        
         
  200 continue

      if(found) then
!    send an event
          call publish_SoluteProfile(ID%SolutesChanged,
     :     g%solute_profiles, g%num_solutes, .false.)
      endif          

      call pop_routine (myname)
      return
      end



* ====================================================================
      subroutine on_soil_water_profile_changed(variant) 
* ====================================================================
      use SoluteModule
      use ComponentInterfaceModule

      implicit none

!+  Sub-Program Arguments
      integer, intent(in out) :: variant

*+  Purpose

*+  Changes
*      18-02-02 DSG Added

*+  Calls

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'on_soil_water_profile_changed')

*+  Local Variables
      type(SoilWaterProfileLayerType)::soil_water_profile_layers
     ! (max_layer)
      integer solnum
      integer layer

*- Implementation Section ----------------------------------

      call push_routine (myname)

      call unpack_SoilWaterProfileLayer(variant
     :                                     ,soil_water_profile_layers
     :                                     ,g%num_layers)

c   dsg 180202    when we want to remap solutes for varying layers, insert here

		 g%soil_water_profile_layers = soil_water_profile_layers
		           
       do 100 solnum = 1,g%num_solutes
       g%solute_profiles(solnum)%NumLayers = g%num_layers
       do 50 layer = 1,g%num_layers
      g%solute_profiles(solnum)%layer(layer)%thickness  = 
     :                        soil_water_profile_layers(layer)%thickness 
     
 50     continue
 100    continue          

!    send an event
          call publish_SoluteProfile(ID%SolutesChanged,
     :     g%solute_profiles, g%num_solutes, .false.)

      call pop_routine (myname)
      return
      end


* ====================================================================
       subroutine solute_zero_variables ()
* ====================================================================
      use SoluteModule
      use ComponentInterfaceModule
      implicit none

*+  Purpose
*     Set all variables to initial state.  i.e. zero or blank.

*+  Mission Statement
*     Set internal state variables to zero

*+  Changes
*     <insert here>

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'solute_zero_variables')

*+  Local Variables
       integer layer
       integer solnum

*- Implementation Section ----------------------------------

      call push_routine (myname)

      g%num_solutes = 0

      p%solute_names(:) = ' '
!      g%solute_profiles(:)%solute_layers(:)%amount = 0.0
!      g%solute_profiles(:)%solute_layers(:)%thickness = 0.0
      
      g%soil_water_profile_layers(:)%BulkDensity = 0.0
      g%soil_water_profile_layers(:)%thickness = 0.0


      call pop_routine (myname)
      return
      end



*     ===========================================================
      subroutine solute_read_param ()
*     ===========================================================
      use SoluteModule
      use ComponentInterfaceModule
      implicit none

*+  Purpose
*       Read in all parameters from parameter file.  Solute information
*       is stored in a two dimensional array so for each solute, read
*       layer information into a single dimension array and insert
*       into the two dimensional array.

*+  Mission Statement
*     Read parameters from parameter file

*+  Changes
*       NIH specified and coded

*+  Calls
                                       ! lu_summary_file

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'solute_read_param')
*
      character section_name*(*)
      parameter (section_name = 'parameters')

*+  Local Variables
      integer    layer
      integer    solnum
      integer    numvals               ! number of values read
      real       sol(max_layer)
      logical ok

*- Implementation Section ----------------------------------

      call push_routine (myname)


      ! Read in solute name from parameter file
      !         -----------
         ok = read_parameter (
     :           section_name,        ! Section header
     :           'solute_names',      ! Keyword
     :           p%solute_names,      ! Array
     :           g%num_solutes)       ! Number of values returned


      do 200 solnum = 1, g%num_solutes

        g%solute_profiles(solnum)%name = p%solute_names(solnum)


         if (p%solute_names(solnum).ne.blank) then

      !     Read in solute in profile from parameter file
      !             -----------------

            ok = read_parameter (
     :           section_name,          ! Section header
     :           p%solute_names(solnum),! Keyword
     :           sol,                   ! Array
     :           numvals,               ! Number of values returned
     :           c%lb_solute,           ! Lower Limit for bound checking
     :           c%ub_solute)           ! Upper Limit for bound checking

            ! register each solute as a get/set and a dlt_ solute as a set
            g%solute_ids(solnum) = add_registration
     :          (respondToGetSetReg,
     :           p%solute_names(solnum),
     :           soluteddml)

            g%solute_dlt_ids(solnum) = add_registration
     :          (respondToSetReg,
     :           'dlt_' // p%solute_names(solnum),
     :           dlt_soluteddml)

            g%solute_ppm_ids(solnum) = add_registration
     :          (respondToGetSetReg,
     :           trim(p%solute_names(solnum))//'ppm',
     :           solute_ppmddml)

            do 100 layer = 1, numvals
               g%solute_profiles(solnum)%layer(layer)%amount 
     :             = sol(layer)
  100       continue
         else
            ! solute is blank so ignore it.
         endif
  200 continue

      call pop_routine (myname)
      return
      end

* ====================================================================
       subroutine solute_read_constants ()
* ====================================================================
      use SoluteModule
      use ComponentInterfaceModule
      implicit none

*+  Purpose
*      Read in all constants from ini file.

*+  Mission Statement
*     Read constants from ini file

*+  Changes
*     17-03-1997 - huth - Programmed and Specified

*+  Constant Values
      character*(*) section_name
      parameter (section_name = 'constants')
*
      character*(*) myname               ! name of current procedure
      parameter (myname = 'solute_read_constants')

*+  Local Variables
      integer    numvals               ! number of values read from file
      logical ok

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call write_string (new_line//'   - Reading Constants')

      ok = read_parameter (
     :           section_name         ! Section header
     :         , 'ub_solute'          ! Keyword
     :         , c%ub_solute          ! Variable
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 1E10)                ! Upper Limit for bound checking

      ok = read_parameter(
     :           section_name         ! Section header
     :         , 'lb_solute'          ! Keyword
     :         , c%lb_solute          ! Variable
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 1E10)                ! Upper Limit for bound checking


      call pop_routine (myname)
      return
      end

