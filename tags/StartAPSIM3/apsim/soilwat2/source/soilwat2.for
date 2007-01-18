      include 'soilwat2.inc'
!     ===========================================================
      subroutine alloc_dealloc_instance(doAllocate)
!     ===========================================================
      use soilwat2Module
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

      else
         deallocate(g)
         deallocate(p)
         deallocate(c)

      end if
      return
      end

!     ===========================================================
      subroutine do_init1(sdml)
!     ===========================================================
      use soilwat2Module
      implicit none
      ml_external do_init1

!+  Purpose
!      Perform all registrations and zeroing

!+  Sub-Program Arguments
      character (len=*), intent(in) :: sdml

!- Implementation Section ----------------------------------

      call do_registrations()

!      call soilwat2_zero_module_links()
      call soilwat2_zero_variables()

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

!     ===========================================================
      subroutine notify_termination()
!     ===========================================================
      use soilwat2Module
      implicit none
      ml_external notify_termination

!+  Purpose
!      Prepare for termination

!- Implementation Section ----------------------------------

      return
      end

* ====================================================================
      subroutine do_init2 ()
* ====================================================================
      use soilwat2Module
      use ComponentInterfaceModule
      implicit none
      ml_external do_init2

*+  Purpose
*     Initialise the soilwat2 module

*+  Changes

*+  Calls

*+  Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='soilwat2_init2')

*+  Local Variables

*- Implementation Section ----------------------------------

      call push_routine (this_routine)

      call soilwat2_zero_variables ()
      call soilwat2_zero_data_links ()
      call soilwat2_zero_event_data ()
      call soilwat2_init ()
      call soilwat2_sum_report ()

      call pop_routine (this_routine)
      return
      end subroutine

!     ===========================================================
      subroutine respondToEvent(fromID,eventID, variant)
!     ===========================================================
      use soilwat2Module
      use ComponentInterfaceModule
      implicit none
      ml_external respondToEvent

!+  Purpose
!      Event handler for all events coming into module.

!+  Sub-Program Arguments
      integer, intent(in) :: fromID
      integer, intent(in) :: eventID
      integer, intent(in out) :: variant

!- Implementation Section ----------------------------------

      if (eventID .eq. DoSoilWaterBalanceID) then
         call soilwat2_zero_daily_variables ()
               ! request and receive variables from owner-modules
!         call soilwat2_get_other_variables ()
               ! do soil water balance
         call soilwat2_DoSoilWaterBalance ()

               ! send changes to owner-modules
         call soilwat2_Publish_other_variables ()
         call soilwat2_zero_data_links ()

      else if (eventID .eq. SolutesChangedID) then
         call soilwat2_OnSolutesChanged (variant)

      else if (eventID .eq. CropDemandCalculatedID) then
         call soilwat2_OnCropDemandCalculated (variant)

      else if (eventID .eq. SurfaceWaterChangedID) then
         call soilwat2_OnSurfaceWaterChanged (variant)

      else if (eventID .eq. EosCalculatedID) then
         call soilwat2_OnEosCalculated (variant)

      else
         call error('bad event ID',IsFatal)
      endif

      return
      end


!     ===========================================================
      subroutine respondToMethod(fromID,methodID, variant)
!     ===========================================================
      use soilwat2Module
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

      if (methodID .eq. ResetID
     :    .or. methodID .eq. UserInitID) then
         call soilwat2_zero_variables ()
!         call soilwat2_get_other_variables ()
         call soilwat2_init ()

      else if (methodID .eq. Sum_ReportID) then
         call soilwat2_sum_report ()

!      else if (methodID .eq. TillageID) then
!         call soilwat2_tillage ()

      else if (methodID .eq. Evap_InitID) then
         call soilwat2_evap_init ()

      else
         call error('bad method ID',IsFatal)
      endif

      return
      end


*     ===========================================================
      subroutine soilwat2_OnSolutesChanged (variant)
*     ===========================================================
      use soilwat2Module
      use ComponentInterfaceModule

      implicit none

*+  Purpose
*     Capture changes to surface water conditions

!+  Sub-Program Arguments
      integer, intent(in out) :: variant

*+  Mission Statement
*      Find the owner of individual solutes

*+  Changes
*       170599 nih - specified

*+  Calls
      integer soilwat2_solute_number

*+  Constant Values
      character  my_name*(*)           ! this subroutine name
      parameter (my_name = 'soilwat2_OnSolutesChanged')

*+  Local Variables
      type (SoluteProfileType), dimension(max_solute)
     :                           :: SoluteProfilesChanged
      integer NumSolutesChanged
      integer solnum                   ! solute array index counter
      integer counter
      integer layer
      integer num_layers

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call unpack_SoluteProfile (variant, SoluteProfilesChanged
     :                                    , NumSolutesChanged)

      if (g%num_solutes + NumSolutesChanged .gt. max_solute) then
         call error ('Too many solutes for Soilwat2', IsFatal)
      else
         num_layers = count_of_real_vals (p%dlayer, max_layer)
         do counter = 1, NumSolutesChanged
            solnum = soilwat2_solute_number
     :               (SoluteProfilesChanged(counter)%name)
            if (solnum .eq. 0) then
               g%num_solutes = g%num_solutes + 1
               solnum = g%num_solutes
               g%solute_names(solnum)
     :                     = SoluteProfilesChanged(counter)%Solute_name
            else
            endif
            mobile_no = position_in_char_array(
     :                        g%solute_names(solnum)
     :                       ,c%mobile_solutes
     :                       ,max_solute)

            immobile_no = position_in_char_array(
     :                        g%solute_names(solnum)
     :                       ,c%immobile_solutes
     :                       ,max_solute)


            if (mobile_no .ne. 0) then
               g%solute_mobility(solnum) = IsMobile

            elseif (immobile_no .ne. 0) then
               g%solute_mobility(solnum) = IsNotMobile

            else
               call error(
     :                 'No solute mobility information for '//
     :                 g%solute_names(g%num_solutes)
     :               , IsFatal)
            endif
!            g%solute_owners(g%num_solutes) = sender
            g%solute(solnum, :) = 0.0
            g%solute(solnum, 1:num_layers)
     :                      = SoluteProfilesChanged(counter)
     :                        %Layer(1:num_layers)
     :                        %amount

         enddo

      endif

      call pop_routine (my_name)
      return
      end

* ====================================================================
       integer function Soilwat2_solute_number (solname)
* ====================================================================
      use Soilwat2Module
      use ComponentInterfaceModule

      implicit none

*+  Sub-Program Arguments
       character solname*(*)

*+  Purpose
*     <insert here>

*+  Changes
*   NeilH - 16-02-1995 - Programmed and Specified

*+  Constant Values
      character myname*(*)               ! name of current procedure
      parameter (myname = 'Soilwat2_solute_number')

*+  Local Variables
       integer counter
       integer solnum

*- Implementation Section ----------------------------------
      call push_routine (myname)

      solnum = 0
      do 100 counter = 1, g%num_solutes
         if (g%solute_names(counter) .eq. solname) then
            solnum = counter
            exit
         else
         endif
  100 continue

      Soilwat2_solute_number = solnum

      call pop_routine (myname)
      return
      end


*     ===========================================================
      subroutine soilwat2_OnSurfaceWaterChanged (variant)
*     ===========================================================
      use soilwat2Module
      use ComponentInterfaceModule

      implicit none

*+  Purpose
*     Capture changes to surface water conditions

!+  Sub-Program Arguments
      integer, intent(in out) :: variant

*+  Mission Statement
*      Find the owner of individual solutes

*+  Changes
*       170599 nih - specified

*+  Constant Values
      character  my_name*(*)           ! this subroutine name
      parameter (my_name = 'soilwat2_OnSurfaceWaterChanged')

*+  Local Variables
      type (SurfaceWatertype) :: Surface_Water

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call unpackSurfaceWater (variant, Surface_Water)
      g%infiltration_pot = Surface_Water%amount

      call pop_routine (my_name)
      return
      end


*     ===========================================================
      subroutine soilwat2_OnEosCalculated (variant)
*     ===========================================================
      use soilwat2Module
      use ComponentInterfaceModule

      implicit none

*+  Purpose
*     Capture changes to surface water conditions

!+  Sub-Program Arguments
      integer, intent(in out) :: variant

*+  Mission Statement
*      Find the owner of individual solutes

*+  Changes
*       170599 nih - specified

*+  Calls
      integer soilwat2_solute_number

*+  Constant Values
      character  my_name*(*)           ! this subroutine name
      parameter (my_name = 'soilwat2_OnEosCalculated')

*+  Local Variables
      real Eos_calculated

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call unpack_Eos_calculated (variant, g%Eos)

      call pop_routine (my_name)
      return
      end

* ====================================================================
       subroutine soilwat2_OnCropDemandCalculated (variant)
* ====================================================================
      use soilwat2Module
      use ComponentInterfaceModule

      implicit none

!+  Sub-Program Arguments
      integer, intent(in out) :: variant

*+  Purpose
*     Add new data describing crop water demands

*+  Changes
*     <insert here>

*+  Local Variables
      type(CropDemandType), dimension(max_array_size)
     :                             :: CropDemands
      integer crop_no
      integer num_layers

*- Implementation Section ----------------------------------

      call unpackCropDemand(variant, CropDemands
     :                                  , NumCropDemands)

      do 100 crop_no = 1, NumCropDemands

         if (g%num_crops.lt.max_crops) then
            g%num_crops = g%num_crops + 1
            g%Crop(g%num_crops)%Name = CropDemands(crop_no)
     :                                  %CropType
            g%crop_module(g%num_crops) = CropDemands(crop_no)
     :                                    %name
            g%Crop(g%num_crops)%numLayers = CropDemands(crop_no)
     :                          %numRootLayers
            g%Crop(g%num_crops)%RootLayer(1:num_layers)%rld =
     :                           CropDemands(crop_no)
     :                           %RootLayers(1:num_layers)
     :                           %RootLengthDensity
            g%Crop(g%num_crops)%RootLayer(1:num_layers)%uptakePot =
     :                           CropDemands(crop_no)
     :                           %RootLayers(1:num_layers)
     :                           %PotentialUptake
            g%Crop(g%num_crops)%demand = CropDemands(crop_no)
     :                                   %amount
         else
            call error ('too many crops',IsFatal)
         endif

  100 continue

      return
      end

* ====================================================================
       subroutine soilwat2_Publish_other_variables ()
* ====================================================================
      use soilwat2Module
      use ComponentInterfaceModule
      implicit none

*+  Purpose
*     Update variables owned by other modules.

*+  Changes
*     <insert here>

*- Implementation Section ----------------------------------

      call soilwat2_Publish_solute_variables()
      call soilwat2_Publish_crop_variables()
      call soilwat2_Publish_soil_water()
      call soilwat2_Publish_soil_water_balance()

      return
      end

* ====================================================================
       subroutine soilwat2_Publish_crop_variables ()
* ====================================================================
      use soilwat2Module
      use ComponentInterfaceModule

      implicit none

*+  Purpose
*     Update variables owned by crop modules.

*+  Changes
*     <insert here>

*+  Local variables
      type(CropSupplyType), dimension(max_array_size)
     :                              :: CropSupplies
      integer crop_no
      integer num_layers

*- Implementation Section ----------------------------------

      num_layers = count_of_real_vals (p%dlayer, max_layer)
      do crop_no = 1, g%num_crops
         CropSupplies(crop_no)%name = g%crop(crop_no)%name
         CropSupplies(crop_no)%numlayers = num_layers
         CropSupplies(crop_no)%layer(1:num_layers)
     :                             %thickness = p%dlayer(1:num_layers)
         CropSupplies(crop_no)%layer(1:num_layers)
     :                             %supply = g%Crop(crop_no)
     :                                        %RootLayer(1:num_layers)
     :                                        %Uptake
      enddo

      call publishCropSupply (
     :                                CropSupplyCalculatedID
     :                              , CropSupplies
     :                              , g%num_crops
     :                              , .false.)

      return
      end

* ====================================================================
       subroutine soilwat2_Publish_soil_water ()
* ====================================================================
      use soilwat2Module
      use ComponentInterfaceModule

      implicit none

*+  Purpose
*     Update variables owned by crop modules.

*+  Changes
*     <insert here>

*+  Local variables
      type(SoilWaterLayerType), dimension(max_array_size)
     :                              :: SoilWaterLayers
      integer num_layers

*- Implementation Section ----------------------------------

      num_layers = count_of_real_vals (p%dlayer, max_layer)
      SoilWaterLayers(1:num_layers)
     :               %thickness = p%dlayer(1:num_layers)
      SoilWaterLayers(1:num_layers)
     :               %amount = g%sw_dep(1:num_layers)

      call publishSoilWaterLayer (SoilWaterChangedID
     :                              , SoilWaterLayers
     :                              , num_layers
     :                              , .false.)

      return
      end

* ====================================================================
       subroutine soilwat2_Publish_soil_water_balance ()
* ====================================================================
      use soilwat2Module
      use ComponentInterfaceModule

      implicit none

*+  Purpose
*     Update variables owned by crop modules.

*+  Changes
*     <insert here>

*+  Local variables
      type(SoilWaterBalanceType) :: SoilWaterBalance
      integer num_layers

*- Implementation Section ----------------------------------

      num_layers = count_of_real_vals (p%dlayer, max_layer)
      SoilWaterBalance%infiltration = g%infiltration
      SoilWaterBalance%drainage = g%drain
      SoilWaterBalance%evaporation =
     :                  sum_real_array (g%es_layers, max_layer)

      SoilWaterBalance%lateral_flow_layers(1:num_layers)
     :                %thickness = p%dlayer(1:num_layers)
      SoilWaterBalance%lateral_flow_layers(:)%flow = 0.0

      call publishSoilWaterBalance (
     :                               SoilWaterBalanceCalculatedID
     :                              , SoilWaterBalance
     :                              , .false.)

      return
      end

* ====================================================================
       subroutine soilwat2_Publish_soil_water_profile ()
* ====================================================================
      use soilwat2Module
      use ComponentInterfaceModule

      implicit none

*+  Purpose
*     Update variables owned by crop modules.

*+  Changes
*     <insert here>

*+  Local variables
      type(SoilWaterProfileLayerType), dimension(max_array_size)
     :                                    :: SoilWaterProfileLayers
      integer num_layers

*- Implementation Section ----------------------------------

      num_layers = count_of_real_vals (p%dlayer, max_layer)

      SoilWaterProfileLayers(1:num_layers)
     :                        %thickness = p%dlayer(1:num_layers)
      SoilWaterProfileLayers(1:num_layers)
     :                        %BulkDensity = g%bd(1:num_layers)
      SoilWaterProfileLayers(1:num_layers)
     :                        %SatDep = g%sat_dep(1:num_layers)
      SoilWaterProfileLayers(1:num_layers)
     :                        %DulDep = g%dul_dep(1:num_layers)
      SoilWaterProfileLayers(1:num_layers)
     :                        %Ll15Dep = g%ll15_dep(1:num_layers)
      SoilWaterProfileLayers(1:num_layers)
     :                        %AirdryDep = g%air_dry_dep(1:num_layers)
      SoilWaterProfileLayers(1:num_layers)
     :                        %SwDep = g%sw_dep(1:num_layers)

      call publishSoilWaterProfileLayer
     :                              (SoilWaterProfileChangedID
     :                              , SoilWaterProfileLayers
     :                              , num_layers
     :                              , .false.)

      return
      end

* ====================================================================
       subroutine soilwat2_Publish_solute_variables ()
* ====================================================================
      use soilwat2Module
      use ComponentInterfaceModule

      implicit none

*+  Purpose
*      Set the values of solute variables from other modules

*+  Changes
*   21-6-96 NIH - Changed set_double_array to post construct

*+  Local Variables

      type (SoluteProfilesType), dimension(nsol) :: SoluteProfiles
      integer solnum                   ! solute array index counter
      integer layer

*- Implementation Section ----------------------------------

      do 100 solnum = 1, g%num_solutes
         do 50 layer=1,max_layer

            SoluteProfiles(solnum)%solute_layers(layer)
     :                            %thickness = p%dlayer(layer)
            SoluteProfiles(solnum)%solute_layers(layer)
     :                            %amount = g%dlt_solute (solnum, layer)
   50    continue
         SoluteProfiles(solnum)%solute_name = g%solute_names(solnum)

  100 continue

      call publishSoluteProfiles (SoluteFluxesCalculatedID
     :                                    , SoluteProfiles
     :                                    , g%num_solutes
     :                                    , .false.)

      return
      end

*     ===========================================================
      subroutine soilwat2_DoSoilWaterBalance ()
*     ===========================================================
      use Soilwat2Module
      use ComponentInterfaceModule

      implicit none

*+  Purpose
*       simulates runoff, infiltration, flux (drainage), unsaturated flow,
*       evaporation, solute movement, transpiration.
*
*         this needs further redesign and cleaning up. this is a test
*         version only.

*+  Mission Statement
*     Perform all APSIM Timestep calculations

*+  Changes
*       221090 specified (jngh)
*       290591 jngh set idrsw to 1 if tirr>0 - cr100
*                   fixed fac problem - cr104
*       221091 removed include nmove.blk   jngh
*       100392 jngh tidied up code.
*       260692 jngh removed nitrogen and drainage flags.
*       290892 jngh changed soil water to depth of water
*       151292 jngh changed common blocks
*       300695 jngh changed pot_eo to a global g_eo and removed from
*                    argument of call to evaporation
*       170895 nih  changed to handle user defined list of solutes
*                   and addition of solutes in irrigation water.
*       270897 pdev Cleaned up. Runoff, solute handing in separate subroutines.
*       021199 jngh added call to cover_surface_runoff
*       041200 dsg  added ponding and impermeable soil layer features

*+  Calls
      real  soilwat_water_table  !  function

*+  Constant Values
      character  my_name*(*)           ! this subroutine name
      parameter (my_name = 'soilwat2_process')

*+  Local Variables
      real       extra_runoff          ! water backed up from flux calculations
                                       ! that was unable to enter profile
      integer    layer                 ! layer number counter variable
      integer    num_layers            ! number of layers

*- Implementation Section ----------------------------------

      call push_routine (my_name)

            ! water balance

      num_layers = count_of_real_vals (p%dlayer, max_layer)

         ! runoff

!      call soilwat2_cover_surface_runoff (g%cover_surface_runoff)

!      call soilwat2_runoff (g%rain, g%runoff_pot)

      ! DSG  041200
      ! g%runoff_pot is the runoff which would have occurred without
      ! ponding.  g%runoff is the ammended runoff after taking any
      ! ponding into account

!      g%pond = g%pond + g%runoff_pot
!      g%runoff = max(g%pond - p%max_pond, 0.0)
!      g%pond = min (g%pond, p%max_pond)




      call soilwat2_infiltration (g%infiltration)

            ! all infiltration and solutes(from irrigation)
            ! go into the top layer.

      g%sw_dep(1) = g%sw_dep(1) + g%infiltration

      if (p%irrigation_layer.gt.0) then
        g%sw_dep(p%irrigation_layer) = g%sw_dep(p%irrigation_layer) +
     :                                 g%irrigation
      else
      endif

            ! save solutes from irrigation
      call soilwat2_irrig_solute ()

      ! NIH 180895
      ! in order to continue capturing irrigation information we zero
      ! the value here.  If we zero the value at the beginning of the day
      ! we may zero it after irrigation has already been specified and the
      ! information would be lost.  The safest way is to hold onto the
      ! information until it is used then reset the record.

      g%irrigation = 0.0
      call fill_real_array (g%irrigation_solute, 0.0, max_solute)

            ! drainage
            ! get flux

      call soilwat2_drainage (g%flux, extra_runoff)

!      g%pond = min (extra_runoff, p%max_pond)
!      g%runoff = g%runoff + extra_runoff - g%pond
      g%infiltration = g%infiltration - extra_runoff
      g%sw_dep(1) = g%sw_dep(1) - extra_runoff



            ! move water down
      call move_down_real (g%flux, g%sw_dep, num_layers)

            ! drainage out of bottom layer
      g%drain = g%flux(num_layers)

            ! now move the solutes with g%flux
            ! flux -  flow > dul
      call soilwat2_move_solute_down ()

                          ! potential: sevap + transpiration:
!      call soilwat2_pot_evapotranspiration (g%eo)

                          ! actual soil evaporation:
      call soilwat2_evaporation (g%es_layers, g%eos)

            ! ** take away evaporation
      do 1500 layer = 1, num_layers
         g%sw_dep(layer) = g%sw_dep(layer) - g%es_layers(layer)

 1500 continue

            ! flow
            ! get unsaturated flow
      call soilwat2_unsat_flow (g%flow)

            ! move water up
      call move_up_real (g%flow, g%sw_dep, num_layers)

            ! now check that the soil water is not silly
      do 2000 layer = 1,num_layers
         call soilwat2_check_profile (layer)
2000  continue

      g%water_table = soilwat_water_table()

      num_layers = count_of_real_vals (p%dlayer, max_layer)

      do 5000 layer = 1,num_layers
         g%sws(layer) = divide (g%sw_dep(layer),p%dlayer(layer), 0.0)
5000  continue

            ! now move the solutes with flow
      call soilwat2_move_solute_up ()

      call Do_Crop_Water_Uptake ()

            ! end
      call pop_routine (my_name)
      return
      end


*     ===========================================================
      subroutine soilwat2_infiltration ( infiltration )
*     ===========================================================
      use Soilwat2Module
      use ComponentInterfaceModule

      implicit none

*+  Sub-Program Arguments
      real       infiltration          ! (OUTPUT) infiltration into top layer (mm)

*+  Purpose
*     infiltration into top layer after runoff.

*+  Mission Statement
*      Calculate infiltration into top layer

*+  Changes
*       221090 specified (jngh)
*       051200 dsg  ponding feature incorporated

*+  Constant Values
      character  my_name*(*)           ! this subroutine name
      parameter (my_name = 'soilwat2_infiltration')

*+  Local Variables

      real       infiltration_1    ! amount of infiltration from rain, irrigation - runoff
      real       infiltration_2    ! amount of infiltration from ponding



*- Implementation Section ----------------------------------

      call push_routine (my_name)

    ! DSG 041200
    ! with the addition of the ponding feature, infiltration is now
    ! considered as consisting of two components - that from the (rain +
    ! irrigation) and that from ponding.

!      if (p%irrigation_layer.eq.0) then
!         g%infiltration =  g%irrigation + g%infiltration_pot
!      else
!         g%infiltration =  g%infiltration_pot
!
!      endif

      g%infiltration =  g%infiltration_pot
      g%infiltration_pot = 0.0

      call pop_routine (my_name)
      return
      end




*     ===========================================================
      subroutine soilwat2_soil_evaporation (es, eos, eos_max)
*     ===========================================================
      use Soilwat2Module
      use ComponentInterfaceModule

      implicit none

*+  Sub-Program Arguments
      real       es(*)          ! (output) actual evaporation
                                ! (mm) over profile
*
      real       eos            ! (input) potential rate of
                                !    evaporation (mm/day)
*
      real       eos_max        ! (input) upper limit of soil
                                !        evaporation (mm/day)

*+  Purpose
*     Wrapper for various evaporation models. Returns actual
*     evaporation from soil surface (es).

*+  Mission Statement
*     Soil Evaporation from Soil Surface

*+  Changes
*       210191 specified and programmed jngh (j hargreaves
*       270897 PdeV

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_soil_evaporation')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call fill_real_array(es, 0.0, max_layer)

      if (c%evap_method .eq. ritchie_method) then
         call soilwat2_ritchie_evaporation (es(1), eos, eos_max)

      else if (c%evap_method .eq. bs_a_method) then
         call soilwat2_bs_a_evaporation (es(1), eos, eos_max)

      else if (c%evap_method .eq. bs_b_method) then
         call soilwat2_bs_b_evaporation (es(1), eos, eos_max)

      else if (c%evap_method .eq. bs_acs_method) then
         call soilwat2_bs_acs_evaporation (es(1), eos, eos_max)

      else if (c%evap_method .eq. rickert_method) then
         call soilwat2_rickert_evaporation (es, eos)

      else

         call error(
     :      'Undefined evaporation method'
     :           , IsFatal)

      endif

      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine soilwat2_ritchie_evaporation (es, eos, eos_max)
*     ===========================================================
      use Soilwat2Module
      use ComponentInterfaceModule

      implicit none

*+  Sub-Program Arguments
      real       es                    ! (output) actual evaporation (mm)
*
      real       eos                   ! (input) potential rate of
                                       !    evaporation (mm/day)
*
      real       eos_max               ! (input) upper limit of soil
                                       !        evaporation (mm/day)

*+  Purpose
*          ****** calculate actual evaporation from soil surface (es) ******
*          most es takes place in two stages: the constant rate stage
*          and the falling rate stage (philip, 1957).  in the constant
*          rate stage (stage 1), the soil is sufficiently wet for water
*          be transported to the surface at a rate at least equal to the
*          evaporation potential (eos).
*          in the falling rate stage (stage 2), the surface soil water
*          content has decreased below a threshold value, so that es
*          depends on the flux of water through the upper layer of soil
*          to the evaporating site near the surface.

*+  Notes
*       This changes globals - sumes1/2 and t.

*+  Mission Statement
*       Calculate evaporation Ritchie model

*+  Changes
*       210191 specified and programmed jngh (j hargreaves
*       160992 jngh moved arguments out  and included common blocks.
*       131093 markl added p_cona
*       190194 jpd  changed code to perfect (w.r.t. 1st stage evap & rainfall)
*       190194 jpd  add new variables: esoil1,esoil2. drop: pesoil & exces1
*       130394 jpd  fixed bug in 2nd stage for day when rain occurs

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_ritchie_evaporation')

*+  Local Variables
      real       esoil1                ! actual soil evap in stage 1
      real       esoil2                ! actual soil evap in stage 2
*
      real       sumes1_max            ! upper limit of sumes1
      real       w_inf                 ! infiltration into top layer (mm)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      sumes1_max = p%u
      w_inf = g%infiltration

         ! if infiltration, reset sumes1
         ! reset sumes2 if infil exceeds sumes1

      if (w_inf.gt.0.0) then

         g%sumes2 = max (0.0, g%sumes2 - max (0.0, w_inf-g%sumes1))
         g%sumes1 = max (0.0, g%sumes1 - w_inf)

            ! update t (incase sumes2 changed)

         g%t = (divide (g%sumes2, p%cona, 0.0))**2

      else
         ! no infiltration, no re-set.
      endif

         ! are we in stage1 ?

      if (g%sumes1.lt.sumes1_max) then

            ! we are in stage1
            ! set esoil1 = potential, or limited by u.

          esoil1 = min (eos, sumes1_max - g%sumes1)

          if (eos.gt.esoil1 .and. esoil1.lt.eos_max) then

*           !  eos not satisfied by 1st stage drying,
*           !  & there is evaporative sw excess to air_dry, allowing for esoil1.
*           !  need to calc. some stage 2 drying(esoil2).

*  if g%sumes2.gt.0.0 then esoil2 =f(sqrt(time),p%cona,g%sumes2,g%eos-esoil1).
*  if g%sumes2 is zero, then use ritchie's empirical transition constant (0.6).

            if (g%sumes2.gt.0.0) then
               g%t = g%t + 1.0
               esoil2 = min (eos - esoil1, p%cona*g%t**0.5 - g%sumes2)
            else
               esoil2 = 0.6*(eos - esoil1)
            endif
         else
               ! no deficit (or esoil1.eq.eos_max,) no esoil2 on this day
            esoil2 = 0.0
         endif

               ! check any esoil2 with lower limit of evaporative sw.
         esoil2 = min (esoil2, eos_max - esoil1)

               !  update 1st and 2nd stage soil evaporation.

         g%sumes1 = g%sumes1 + esoil1
         g%sumes2 = g%sumes2 + esoil2
         g%t = (divide (g%sumes2, p%cona, 0.0))**2

      else

            ! no 1st stage drying. calc. 2nd stage

         esoil1 = 0.0

         g%t = g%t + 1.0
         esoil2 = min (eos, p%cona*g%t**0.5 - g%sumes2)

            ! check with lower limit of evaporative sw.

         esoil2 = min (esoil2, eos_max)

            !   update 2nd stage soil evaporation.

         g%sumes2 = g%sumes2 + esoil2

      endif

      es = esoil1 + esoil2

         ! make sure we are within bounds
      es = bound (es,  0.0, eos)
      es = bound (es, 0.0, eos_max)

      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine soilwat2_bs_a_evaporation (es, eos, eos_max)
*     ===========================================================
      use Soilwat2Module
      use ComponentInterfaceModule

      implicit none

*+  Sub-Program Arguments
      real       es             ! (output) actual evaporation
                                ! from top layer(mm)
*
      real       eos            ! (input) potential rate of
                                !    evaporation (mm/day)
*
      real       eos_max        ! (input) upper limit of soil
                                !        evaporation (mm/day)

*+  Purpose
*     B&S (in their paper this is Option A. Fig 2)

*+  Notes
*       This changes globals - sumes1,2.

*+  Mission Statement
*       Calculate evaporation using B&S (A) model

*+  Changes
*       210191 specified and programmed jngh (j hargreaves

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_bs_a_evaporation')

*+  Local Variables
      real       sumes1_max            ! upper limit of sumes1
      real       w_inf                 ! infiltration into top layer (mm)
      real       espot                 ! temporary

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      sumes1_max = p%beta**2
      w_inf = g%infiltration

*     if infiltration is greater than eos, es is = eos.

*     &&&&&&&&&& Below here - as coded by B&S in Fig 2 &&&&&&&&&&&&&&&&&&&&&&
      if (w_inf .ge. eos) then

         g%sumes = max(0.0, g%sumes - (w_inf - eos))
         es = eos
         espot = divide (g%sumes**2, p%beta**2, 0.0)
         g%sumeos = max(g%sumes, espot)
      else
                                ! Infiltration is less than eos
         g%sumeos = g%sumeos + (eos - w_inf)
         es = w_inf + (min(g%sumeos, p%beta*g%sumeos**0.5)
     :        - g%sumes)
         g%sumes = min(g%sumeos, p%beta*g%sumeos**0.5)
      endif
*     &&&&&&&&&& Above here - as coded by B&S in Fig 2 &&&&&&&&&&&&&&&&&&&&&&

*     next 2 conditions added because g%sumes was zero
*     after larger rain and at the same time es was = eos.

      if(es .gt. g%sumes) then
         g%sumes = es
      endif

      if(g%sumes.le.sumes1_max) then
         g%sumeos = g%sumes
      else
         g%sumeos = (divide (g%sumes, p%beta, 0.0))**2
      endif


      if (g%sumes.ge. sumes1_max) then
         g%sumes1 = sumes1_max
         g%sumes2 = g%sumes - g%sumes1
      else
         g%sumes1 = g%sumes
         g%sumes2 = 0.0
      endif

                                ! make sure we are within bounds
      es = bound (es, 0.0, eos)
      es = bound (es, 0.0, eos_max)

      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine soilwat2_bs_b_evaporation (es, eos, eos_max)
*     ===========================================================
      use Soilwat2Module
      use ComponentInterfaceModule

      implicit none

*+  Sub-Program Arguments
      real       es                    ! (output) actual evaporation (mm)
*
      real       eos                   ! (input) potential rate of
                                       !    evaporation (mm/day)
*
      real       eos_max               ! (input) upper limit of soil
                                       !        evaporation (mm/day)

*+  Purpose
*     B&S. This code tries to achieve the result stated for their Option B.
*     Evaporate small rainfall events & then step back to the
*     original state

*+  Notes
*       This changes globals - sumes1/2 and t.

*+  Mission Statement
*       Calculate evaporation using B&S (B) model

*+  Changes
*       210191 specified and programmed jngh (j hargreaves

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_bs_b_evaporation')

*+  Local Variables
      real       sumes1_max            ! upper limit of sumes1
      real       w_inf                 ! infiltration into top layer (mm)
      real       esoil1                ! actual soil evap in stage 1
      real       esoil2                ! actual soil evap in stage 2
      real       todays_es             ! today's actual evap as
                                       ! f(beta,sumeos+eos)

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      sumes1_max = p%beta**2

      w_inf = g%infiltration

                                ! if infiltration, reset sumes1
                                ! reset sumes2 if infil exceeds sumes1
      if (w_inf .gt. 0.0) then

         g%sumes2 = max (0.0, g%sumes2 - max (0.0, w_inf
     :        - g%sumes1))
         g%sumes1 = max (0.0, g%sumes1 - w_inf)

                                ! update sumes & sumeos
         g%sumes = g%sumes1 + g%sumes2
         if(g%sumes.le.sumes1_max) then
            g%sumeos = g%sumes
         else
            g%sumeos = (divide (g%sumes, p%beta, 0.0))**2
         endif

      else
                                ! no infiltration, no re-set.
      endif


*     Today's actual evap calculated for today's eos
*     If todays_es is limited by soil water then g%sumeos will
*     be adjusted later

      g%sumeos = g%sumeos + eos
      if(g%sumeos .le. sumes1_max) then

         todays_es = eos
         g%sumes = g%sumes + todays_es
      else

         todays_es = p%beta * g%sumeos**0.5 - g%sumes
         todays_es = bound (todays_es,  0.0, eos)
         g%sumes  = g%sumes + todays_es
         g%sumeos = (divide (g%sumes, p%beta, 0.0))**2
      endif

                                ! are we in stage1 ?
      if (g%sumes1 .lt. sumes1_max) then
*     We are in stage1.
*     set esoil1 = eos, or limited by sumes1_max (beta**2).
*     todays_es is overriden by 1st stage evap.

         esoil1 = min (eos, sumes1_max - g%sumes1)

         if (eos .gt. esoil1 .and. esoil1 .lt. eos_max) then

*     eos not satisfied by 1st stage drying,
*     & there is evaporative sw excess to air_dry, allowing for esoil1.
*     need to calc. some stage 2 drying(esoil2).
*     For comparing versions, include Ritchie's transition constant 0.6
            esoil2 = (eos - esoil1) * 0.6

         else
*     no deficit (or esoil1 .eq. eos_max,) no esoil2 on this day
            esoil2 = 0.0

         endif

*     check any esoil2 with upper limit of evaporative sw.
         esoil2 = min (esoil2, eos_max - esoil1)

                                !  update 1st and 2nd stage soil evaporation.
         g%sumes1 = g%sumes1 + esoil1
         g%sumes2 = g%sumes2 + esoil2

      else

                                ! no 1st stage drying. todays_es is all
                                ! 2nd stage
         esoil1 = 0.0

         esoil2 = todays_es

                                ! check with upper limit of evaporative sw.
         esoil2 = min (esoil2, eos_max)

                                !   update 2nd stage soil evaporation.
         g%sumes2 = g%sumes2 + esoil2
      endif

*     update sumes & sumeos incase esoil1&2 limited by eos_max
*     or ritchie transition constant

      g%sumes = g%sumes1 + g%sumes2
      g%sumeos = (divide (g%sumes, p%beta, 0.0))**2

      es = esoil1 + esoil2

                                ! make sure we are within bounds
      es = bound (es, 0.0, todays_es)
      es = bound (es, 0.0, eos_max)

      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine soilwat2_bs_acs_evaporation (es, eos, eos_max)
*     ===========================================================
      use Soilwat2Module
      use ComponentInterfaceModule

      implicit none

*+  Sub-Program Arguments
      real       es                    ! (output) actual evaporation (mm)
*
      real       eos                   ! (input) potential rate of
                                       !    evaporation (mm/day)
*
      real       eos_max               ! (input) upper limit of soil
                                       !        evaporation (mm/day)

*+  Purpose
*     acs attempt at B&S Option B
*     infiltration > evap at stage 1, then return to original
*     (pdev - I think this is work in progress, incomplete)

*+  Notes
*       This changes globals - inf_pool, sumes_yest, sumes_last

*+  Mission Statement
*       Calculate evaporation using B&S (B, ACS) model

*+  Changes
*       210191 specified and programmed jngh (j hargreaves

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_bs_acs_evaporation')

*+  Local Variables
      real      sumes1_max
      real      w_inf
      real      surplus_es

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      sumes1_max = p%beta**2

      w_inf = g%infiltration

      g%sumes_yest = g%sumes

                                ! reset for infiltration
      if ((g%inf_pool .EQ. 0.0) .AND. (w_inf .GT. 0.0)) then
         g%sumes_yest = 0.0
         g%sumes_last = g%sumes
         g%sumeos_last = g%sumeos
      else
                                ! no need to store last values
      endif

      g%inf_pool = g%inf_pool + w_inf

      if (g%inf_pool .GT. 0.0) then
         g%sumes = 0.0
         g%sumeos = 0.0
      else
                                ! Nothing in inf pool to be
                                ! evap at stage 1, no reset
      endif

                                ! dodgy logic for a massive reset
                                ! on 90% AWR (1)   ho ho ho !!
      if ( g%sw_dep(1) .GT. (0.9*(g%dul_dep(1)-g%air_dry_dep(1))) )
     +     then
         g%sumes_last = 0.0
         g%sumeos_last = 0.0
      else
                                ! no need for massive reset
      endif


                                ! Do the B&S ...
      g%sumeos = g%sumeos + eos

      if (g%sumes .LT. sumes1_max) then !first stage
         g%sumes = g%sumeos

      else                      ! second stage
         g%sumes = p%beta * g%sumeos**0.5

      endif

                                ! calc esoil and update inf_pool, sumes/eos
      es = g%sumes - g%sumes_yest
      g%inf_pool = max (0.0, g%inf_pool - es)

*     Put things back how they were before infil and adjust for over evaping
      if (g%inf_pool .LE. 0.0) then !evaped all away
         if (g%sumes_last .GT. 0.0) then
            surplus_es = 0.0 - g%inf_pool
            g%inf_pool = 0.0

                                ! carry surplus evap over to last posi
            g%sumes = g%sumes_last + surplus_es
            if (surplus_es .LT. sumes1_max) then
               g%sumeos = g%sumeos_last + surplus_es
            else
               g%sumeos = g%sumeos_last + (surplus_es/p%beta)**2
            endif
         else                   ! g%sumes_last = 0.0 ie had massive reset
                                ! and no need to change g%sumes and g%sumeos
         endif

      else
                                ! keep evaping infil pool tommorrow
      endif

      es = bound (es, 0.0, eos_max)

      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine soilwat2_rickert_evaporation (es, eos)
*     ===========================================================
      use Soilwat2Module
      use ComponentInterfaceModule

      implicit none

*+  Sub-Program Arguments
      real       es(*)                 ! (output) actual evaporation (mm)
*
      real       eos                   ! (input) potential rate of
                                       !    evaporation (mm/day)

*+  Purpose
*     Ex Grasp                <<< need better description here - dms!!!>>>
*     Evaporate moisture from the soil surface. The moisture can come
*     from layers 1 & 2, if the supply is sufficient. Total evap is limited
*     by an upper bound, max_evap, from soil parameters.
*

*+  Mission Statement
*     Calculate evaporation using Rickert model

*+  Changes
*       210191 specified and programmed jngh (j hargreaves

*+  Calls
      real       soilwat2_comp_curve

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_rickert_evaporation')

*+  Local Variables
      real       supply_ratio_L1
      real       supply_ratio_L1_L2
      real       supply_ratio_L2
      real       avail_water_L1
      real       avail_water_L1_L2
      real       avail_capacity_L1
      real       avail_capacity_L1_L2
      real       evap_L1
      real       evap_L2
      real       eos_max               ! upper limit of soil
                                       ! evaporation (mm/day)

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call fill_real_array(es, 0.0, max_layer)

      avail_water_L1 = g%sw_dep(1) - g%air_dry_dep(1)

      avail_capacity_L1 = g%dul_dep(1) - g%air_dry_dep(1)

      supply_ratio_L1 = divide (avail_water_L1,
     :     avail_capacity_L1, 0.0)

c     PdeV - Should send this magic number to constants file:
      supply_ratio_L1 = soilwat2_comp_curve (supply_ratio_L1, 0.285)

      supply_ratio_L1 = bound (supply_ratio_L1, 0.0, 1.0)

      avail_water_L1_L2 = g%sw_dep(1) + g%sw_dep(2) -
     :     g%air_dry_dep(1) - g%ll15_dep(2)

      avail_capacity_L1_L2 = g%dul_dep(1) + g%dul_dep(2) -
     :     g%air_dry_dep(1) - g%ll15_dep(2)

      supply_ratio_L1_L2 = divide (avail_water_L1_L2,
     :     avail_capacity_L1_L2, 0.0)
      supply_ratio_L1_L2 = soilwat2_comp_curve(supply_ratio_L1_L2,
     :     0.117)

      supply_ratio_L1_L2 = bound (supply_ratio_L1_L2, 0.0, 1.0)


      evap_L1 = supply_ratio_L1 * eos
      eos_max = min(p%max_evap, g%sw_dep(1) - g%air_dry_dep(1))
      evap_L1 = bound(evap_L1, 0.0, eos_max)

CPdeV - should resolve whether we can evaporate L2 down to airdry.
      if (supply_ratio_L1_L2 .gt. supply_ratio_L1 .and.
     :    g%sw_dep(2) .gt. g%ll15_dep(2)) then
         supply_ratio_L2 = supply_ratio_L1_L2 - supply_ratio_L1

         evap_L2 = supply_ratio_L2 * eos
         eos_max = min(p%max_evap - evap_L1,
     :                  g%sw_dep(2) - g%ll15_dep(2))
cdms         eos_max = min(p%max_evap - evap_L1,
cdms     :                     g%sw_dep(2) - g%air_dry_dep(2))  ! more realistic
         evap_L2 = bound(evap_L2, 0.0, eos_max)

      else
         evap_L2 = 0.0
      endif
cdms  pete - this looks to be limited to available sw above
cdms         is a further check needed ?????
c     Can't use g%eo as upper bound to evaporation (like all the
c     other routines) as g%eo is specific to the top layer. This check
c     should suffice.
      eos_max = g%sw_dep(1) - g%air_dry_dep(1) +
     :    g%sw_dep(2) - g%ll15_dep(2)

      if (evap_L1 + evap_L2 .gt. eos_max) then
         call error ('Evaporation exceeds asw - help!'
     :               , IsNotFatal)
      else
                                ! Nothing
      endif

      es(1) = evap_L1
      es(2) = evap_L2

      call pop_routine (my_name)
      return
      end



*     ===========================================================
      real function soilwat2_comp_curve (ndx, a)
*     ===========================================================
      use Soilwat2Module
      use ComponentInterfaceModule

      implicit none

*+  Sub-Program Arguments
      real ndx                  ! input index (0-1)
      real a                    ! weighting

*+  Purpose
*     .... from GRASP (Surfair)
*     Standard competition curve (or at least so McKeon
*     calls it) This function is used by McKeon in several places to
*     transform an index in the range [0-1] to another index in the
*     same range, but weighted in a different way. The weighting is
*     controlled by the a parameter. An "a" value  of 1 leaves the
*     index untransformed.

*+  Mission Statement
*      Competition curve %1, weighting %2

*+  Changes
*       210191 specified and programmed jngh (j hargreaves

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_comp_curve')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      soilwat2_comp_curve = divide (a * ndx,
     :     ndx * (a - 1.0) + 1.0, 0.0)

      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine soilwat2_drainage (flux, extra_runoff)
*     ===========================================================
      use Soilwat2Module
      use ComponentInterfaceModule

      implicit none

*+  Sub-Program Arguments
      real       flux (*)              ! (output) water moving out of
      real       extra_runoff          ! (output) water to add to runoff
                                       ! layer (mm)

*+  Purpose
*       calculate flux - drainage from each layer

*+  Mission Statement
*     Calculate Drainage from each layer

*+  Changes
*        210191   specified and programmed jngh (j hargreaves
*        051191   jngh fixed drainage lower limit and
*                 restructured excess and drainage algorithms - cr196
*        260692   jngh changed l to layer & commented includes
*        290892   jngh changed soil water to depth of water
*        151292   jngh changed common blocks
*        250893   jngh firstly changed drainage criteria to match cm v1 code
*                      then removed .003 part to allow proper denitrification
*                      in nitrogrn module.
*        131093   markl changes p%swcon to an array for each soil layer

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_drainage')

*+  Local Variables

      real       add                   ! water to add to layer
      real       backup                ! water to backup
      real       excess                ! amount above saturation(overflow)(mm)
      real       new_sw_dep(max_layer) ! record of results of sw calculations
                                       ! ensure mass balance. (mm)
      integer    l                     ! counter
      integer    layer                 ! counter for layer no.
      integer    num_layers            ! number of layers
      real       w_drain               ! water draining by gravity (mm)
      real       w_in                  ! water coming into layer (mm)
      real       w_out                 ! water going out of layer (mm)
      real       w_tot                 ! total water in layer at start (mm)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

                ! flux into layer 1 = infiltration (mm).

      w_in = 0.0
      extra_runoff = 0.0

                ! calculate drainage and water
                ! redistribution.

      call fill_real_array (flux, 0.0, max_layer)
      num_layers = count_of_real_vals (p%dlayer, max_layer)

      do 240 layer = 1, num_layers

             ! get total water concentration in layer

         w_tot = g%sw_dep(layer) + w_in

             ! get excess water above saturation & then water left
             ! to drain between sat and dul.  Only this water is
             ! subject to swcon. The excess is not - treated as a
             ! bucket model. (mm)

         if (w_tot.gt.g%sat_dep(layer)) then
            excess = w_tot - g%sat_dep(layer)
            w_tot = g%sat_dep(layer)
         else
            excess = 0.0
         endif

         if (w_tot.gt. g%dul_dep(layer)) then
            w_drain = (w_tot - g%dul_dep(layer)) *p%swcon(layer)
         else
            w_drain = 0.0
         endif




             ! get water draining out of layer (mm)

         if (excess.gt.0.0) then

            if (p%mwcon(layer).ge.1.0) then
               ! all this excess goes on down so do nothing
               w_out = excess + w_drain
               new_sw_dep(layer)=g%sw_dep(layer) + w_in - w_out
               flux(layer) = w_out

            else
               ! Calculate amount of water to backup and push down

               ! Firstly top up this layer (to saturation)
               add = min (excess, w_drain)
               excess = excess - add
               new_sw_dep(layer) = g%sat_dep(layer) - w_drain + add

               ! partition between flow back up and flow down
               backup = (1. - p%mwcon(layer))*excess
               excess = p%mwcon(layer) * excess

               w_out = excess + w_drain
               flux(layer) = w_out

               ! now back up to saturation for this layer up out of the
               ! backup water keeping account for reduction of actual
               ! flow rates (flux) for N movement.

               do 100 l=layer-1,1,-1
                  flux(l) = flux(l) - backup
                  add = min(g%sat_dep(l) - new_sw_dep(l),backup)
                  new_sw_dep(l) = new_sw_dep(l) + add
                  backup = backup - add
  100          continue
               extra_runoff = extra_runoff + backup


            endif

         else
            ! there is no excess so do nothing
            w_out = w_drain
            flux(layer) = w_out
            new_sw_dep(layer) = g%sw_dep(layer) + w_in - w_out

         endif

             ! drainage out of this layer goes into next layer down

         w_in = w_out
240   continue

      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine soilwat2_unsat_flow (flow)
*     ===========================================================
      use Soilwat2Module
      use ComponentInterfaceModule

      implicit none

*+  Sub-Program Arguments
      real       flow (*)              ! (output) water movement out of
                                       !    each layer (mm)

*+  Purpose
*       calculate unsaturated flow below drained upper limit

*+  Mission Statement
*     Calculate Unsaturated Solute and Water Flow

*+  Changes
*        210191   specified and programmed jngh (j hargreaves
*        290892   jngh changed soil water to depth of water
*        151292   jngh changed common blocks
*        120294   jpd add apswtspr.blk for p%diffus_const,p%diffus_slope
*        150294   mep added variable flow_max to constrain flow(layer) to
*                    a zero gradient for adjacent layers.
*        100795 jngh added limits for flow_max to esw_dep as sw was going
*                    below air_dry.

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_unsat_flow')

*+  Local Variables
      real       esw_dep1              ! extractable soil water in current
                                       ! layer (mm)
      real       esw_dep2              ! extractable soil water in next
                                       ! layer below (mm)
      real       dbar                  ! average diffusivity used to calc
                                       !    unsaturated flow between layers
      integer    layer                 ! layer counter for current layer
      integer    second_last_layer     ! last layer for flow
      integer    num_layers            ! number of layers
      integer    next_layer            ! layer counter for next lower layer
      real       flow_max              ! maximum flow to make gradient between
                                       ! layers equal zero
      real       theta1                ! sw content above ll15 for current
                                       !    layer (cm/cm)
      real       theta2                ! sw content above ll15 for next lower
                                       !    layer (cm/cm)
      real       w_out                 ! water moving up out of this layer (mm)
                                       ! +ve = up to next layer
                                       ! -ve = down into this layer
*
      real       this_layer_cap        ! capacity of this layer to accept water
                                       ! from layer below (mm)
      real       next_layer_cap        ! capacity of nxt layer to accept water
                                       ! from layer above (mm)
*
      real       sw1                   ! sw for current layer (mm/mm)
      real       sw2                   ! sw for next lower layer (mm/mm)
      real       gradient              ! driving force for flow
      real       sum_inverse_dlayer    !
*
      real       dlayer1               ! depth of current layer (mm)
      real       dlayer2               ! depth of next lower layer (mm)
      real       ave_dlayer            ! average depth of current and next
                                       ! layers (mm)
*
      real       sw_dep1               ! soil water depth in current layer (mm)
      real       sw_dep2               ! soil water depth in next layer (mm)
*
      real       ll15_dep1             ! 15 bar lower limit sw depth in current
                                       ! layer (mm)
      real       ll15_dep2             ! 15 bar lower limit sw depth in next
                                       ! layer (mm)
*
      real       sat_dep1              ! saturated sw depth in current layer
                                       ! (mm)
      real       sat_dep2              ! saturated sw depth in next layer (mm)
      real       swg                   ! sw differential due to gravitational
                                       ! pressure head (mm)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      num_layers = count_of_real_vals (p%dlayer, max_layer)

        ! *** calculate unsaturated flow below drained upper limit (flow)***

      call fill_real_array (flow, 0.0, max_layer)

                ! second_last_layer is bottom layer but 1.

      second_last_layer = num_layers - 1

      w_out = 0.0
      do 500 layer = 1, second_last_layer
         next_layer = layer + 1

         dlayer1    = p%dlayer(layer)
         dlayer2    = p%dlayer(next_layer)
         ave_dlayer = (dlayer1 + dlayer2) *0.5

         sw_dep1    = g%sw_dep(layer)
         sw_dep2    = g%sw_dep(next_layer)

         ll15_dep1  = g%ll15_dep(layer)
         ll15_dep2  = g%ll15_dep(next_layer)

         sat_dep1   = g%sat_dep(layer)
         sat_dep2   = g%sat_dep(next_layer)

         esw_dep1   = l_bound ((sw_dep1 - w_out) - ll15_dep1, 0.0)
         esw_dep2   = l_bound (sw_dep2 - ll15_dep2, 0.0)

                ! theta1 is excess of water content above lower limit,
                ! theta2 is the same but for next layer down.

         theta1 = divide (esw_dep1, dlayer1, 0.0)
         theta2 = divide (esw_dep2, dlayer2, 0.0)

           ! find diffusivity, a function of mean thet.

         dbar  = p%diffus_const
     :         * exp (p%diffus_slope * (theta1 + theta2) * 0.5)

            ! testing found that a limit of 10000 (as used in ceres-maize)
            ! for dbar limits instability for flow direction for consecutive
            ! days in some situations.

         dbar = bound (dbar, 0.0, 10000.0)

         sw1 = divide ((sw_dep1 - w_out), dlayer1, 0.0)
         sw1 = l_bound (sw1, 0.0)

         sw2 = divide (sw_dep2, dlayer2, 0.0)
         sw2 = l_bound (sw2, 0.0)

            ! gradient is defined in terms of absolute sw content

cjh          subtract gravity gradient to prevent gradient being +ve when
cjh          flow_max is -ve, resulting in sw > sat.

         gradient  = divide ((sw2 - sw1), ave_dlayer, 0.0)
     :             - c%gravity_gradient

            !  flow (positive up) = diffusivity * gradient in water content

         flow(layer) = dbar * gradient

            ! flow will cease when the gradient, adjusted for gravitational
            ! effect, becomes zero.

         swg = c%gravity_gradient* ave_dlayer

            ! calculate maximum flow

         sum_inverse_dlayer = divide (1.0, dlayer1, 0.0)
     :                      + divide (1.0, dlayer2, 0.0)
         flow_max = divide ((sw2 - sw1 - swg), sum_inverse_dlayer, 0.0)

c dsg    un-incorporated code from senthold version
!         if (g%sw_dep(layer).gt.g%dul_Dep(layer)) then
!            flow(layer) = 0.0
!
!         elseif (g%sw_dep(next_layer).gt.g%dul_Dep(next_layer)) then
!            flow(layer) = 0.0
!
!         endif

         if (flow(layer) .lt. 0.0) then
            ! flow is down to layer below
            ! check capacity of layer below for holding water from this layer
            ! and the ability of this layer to supply the water

            next_layer_cap = l_bound (sat_dep2 - sw_dep2, 0.0)
            flow_max = l_bound (flow_max, -next_layer_cap)
cjh
            flow_max = l_bound (flow_max, -esw_dep1)
            flow(layer) = l_bound (flow(layer), flow_max)

         elseif (flow(layer) .gt. 0.0) then
            ! flow is up from layer below
            ! check capacity of this layer for holding water from layer below
            ! and the ability of the layer below to supply the water

            this_layer_cap = l_bound (sat_dep1 - (sw_dep1 - w_out), 0.0)
            flow_max = u_bound (flow_max, this_layer_cap)
cjh
            flow_max = u_bound (flow_max, esw_dep2)
            flow(layer) = u_bound (flow(layer), flow_max)
         else
            ! no flow
         endif


            ! For conservation of water, store amount of water moving
            ! between adjacent layers to use for next pair of layers in profile
            ! when calculating theta1 and sw1.

          w_out = flow(layer)

  500 continue

      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine soilwat2_check_profile (layer)
*     ===========================================================
      use Soilwat2Module
      use ComponentInterfaceModule

      implicit none

*+  Sub-Program Arguments
      integer    layer                 ! (input) layer counter

*+  Purpose
*       checks validity of soil water parameters for a soil profile layer

*+  Notes
*           reports an error if
*           - g%ll15_dep, dul_dep, and sat_dep are not in ascending order
*           - ll15 is below min_sw
*           - sat is above max_sw
*           - sw > sat or sw < min_sw

*+  Mission Statement
*     Check Soil Water Parameters for each layer

*+  Changes
*       180789 specified and programmed (jngh)
*       280491 jngh - reworked error messages and their formats.- cr54
*       290591 jngh removed else if stmt for better checking - cr55
*                   removed double call to lyrchk - cr56
*       270592 jngh removed lyrchk from external calls and call
*                   that was commented out.  also corrected error
*                   conditions sections - cr303
*                   reformatted error messages for better layout - cr305
*       290892 jngh changed soil water to depth of water
*       151292 jngh changed common blocks
*       250594 jngh added margins for rounding errors. - jpd pers comm.
*       190595 jngh changed max sw to be calculated from specific bulk density
*       300695 jngh changed min of sw to be airdry
*       050795 jngh fixed error message for sw vs air_dry error.

*+  Constant Values
      real       min_sw                ! lowest acceptable value for sw
                                       !   (mm water/mm soil)
      parameter (min_sw  = 0.0)
*
      real      max_sw_margin          ! margin for measurement error (mm/mm)
      parameter (max_sw_margin = 0.01)

*+  Local Variables
      real       dul                   ! drained upper limit water content
                                       !   of layer (mm water/mm soil)
      real       dul_errmargin         ! rounding error margin for dulc
      character  err_messg*300         ! error message
      real       ll15                  ! lower limit at 15 bars water content
                                       !   of layer (mm water/mm soil)
      real       air_dry_errmargin     ! rounding error margin for air_dryc
      real       air_dry               ! lower limit at air dry water content
                                       !   of layer (mm water/mm soil)
      real       ll15_errmargin        ! rounding error margin for ll15c
      real       sat                   ! saturated water content of layer
                                       !   (mm water/mm soil)
      real       sat_errmargin         ! rounding error margin for satc
      real       sw                    ! soil water content of layer l
                                       !   (mm water/mm soil)
      real       sw_errmargin          ! rounding error margin for swc
*
      real       max_sw                ! largest acceptable value for sat
                                       !   (mm water/mm soil)

*- Implementation Section ----------------------------------
      max_sw = 1.0 - divide (g%bd(layer), c%specific_bd, 0.0)
         ! ie Total Porosity

      sw = divide (g%sw_dep(layer), p%dlayer(layer), 0.0)
      sat = divide (g%sat_dep(layer), p%dlayer(layer), 0.0)
      dul = divide (g%dul_dep(layer), p%dlayer(layer), 0.0)
      ll15 = divide (g%ll15_dep(layer), p%dlayer(layer), 0.0)
      air_dry = divide (g%air_dry_dep(layer), p%dlayer(layer), 0.0)

      sw_errmargin = error_margin (sw)
      sat_errmargin = error_margin (sat)
      dul_errmargin = error_margin (dul)
      ll15_errmargin = error_margin (ll15)
      air_dry_errmargin = error_margin (air_dry)

      if (air_dry + air_dry_errmargin .lt. min_sw) then
         write (err_messg, '(a, g17.6e3, a, i3, 2a, g17.6e3)')
     :           ' Air dry lower limit of ', air_dry
     :          ,' in layer ', layer
     :          , new_line
     :          ,'         is below acceptable value of ', min_sw
         call error (err_messg, IsNotFatal)
      else
      endif

      if (ll15 + ll15_errmargin .lt. air_dry - air_dry_errmargin) then
         write (err_messg, '(a, g17.6e3, a, i3, 2a, g17.6e3)')
     :           ' 15 bar lower limit of ', ll15
     :          ,' in layer ', layer
     :          , new_line
     :          ,'         is below air dry value of ', air_dry
         call error (err_messg, IsNotFatal)
      else
      endif

      if (dul + dul_errmargin .le. ll15 - ll15_errmargin) then
         write (err_messg, '(a, g17.6e3, a, i3, 2a, g17.6e3)')
     :            ' drained upper limit of ',dul
     :           ,' in layer ', layer
     :           , new_line
     :           ,'         is at or below lower limit of ', ll15
         call error (err_messg, IsNotFatal)
      else
      endif

      if (sat + sat_errmargin .le. dul - dul_errmargin) then
         write (err_messg, '(a, g17.6e3, a, i3, 2a, g17.6e3)')
     :            ' saturation of ', sat
     :           ,' in layer ', layer
     :           , new_line
     :           ,'         is at or below drained upper limit of '
     :           , dul
         call error (err_messg, IsNotFatal)
      else
      endif

      if (sat - sat_errmargin .gt. max_sw + max_sw_margin) then

         write (err_messg, '(a, g17.6e3, a, i3, 3(2a, g17.6e3))')
     :            ' saturation of ', sat
     :           ,' in layer ', layer
     :           , new_line
     :           ,'         is above acceptable value of ', max_sw
     :           , new_line
     :           , 'You must adjust bulk density (bd) to below '
     :           , (1.0 - sat) * c%specific_bd
     :           , new_line
     :           , 'OR saturation (sat) to below ', max_sw
         call error (err_messg, IsNotFatal)

      else
      endif

      if (sw - sw_errmargin .gt. sat + sat_errmargin) then
         write (err_messg, '(a, g17.6e3, a, i3, 2a, g17.6e3)')
     :            ' soil water of ', sw
     :           ,' in layer ', layer
     :           , new_line
     :           ,'         is above saturation of ', sat
         call error (err_messg, IsNotFatal)
      else
      endif

      if (sw + sw_errmargin .lt. air_dry - air_dry_errmargin) then
         write (err_messg, '(a, g17.6e3, a, i3, 2a, g17.6e3)')
     :            ' soil water of ', sw
     :           ,' in layer ', layer
     :           , new_line
     :           ,'         is below air-dry value of ', air_dry
         call error (err_messg, IsNotFatal)

      else
      endif

      return
      end



*     ===========================================================
      subroutine soilwat2_layer_check (layer)
*     ===========================================================
      use Soilwat2Module
      use ComponentInterfaceModule

      implicit none

*+  Sub-Program Arguments
      integer    layer                 ! (input) layer counter

*+  Purpose
*       checks that layer lies in range of 1 - num_layers

*+  Notes
*             reports error if layer < min_layer
*             or layer > num_layers

*+  Mission Statement
*     Check Soil Water Parameters for a given layer

*+  Changes
*       180789 specified and programmed (jngh)
*       221191 jngh expanded messages, l to layer and removed unused
*              common blocks. sprofl and swater - cr33
*                   parameterised min_layer - cr32
*       270592 jngh moved count_of_real_vals to global section
*                   declared count_of_real_vals in external calls - cr302
*                   moved num_layers to internal section - cr302
*                   num_layers changed to come from function and
*                   no longer from arguments.  included sprofl
*                   common block and max_layer.  also used count_of_real_vals.

*+  Constant Values
      integer    min_layer             ! lowest value for a layer number
      parameter (min_layer = 1)

*+  Local Variables
      character  err_messg*200         ! error message
      integer    num_layers            ! max layers

*- Implementation Section ----------------------------------

      num_layers = count_of_real_vals (p%dlayer, max_layer)

      if (layer.lt.min_layer) then
         write (err_messg,'(2(a,i3))')
     :                         ' soil layer no. ', layer
     :                        ,' is below mimimum of ', min_layer
         call error (err_messg, IsNotFatal)

      else if (layer.gt.num_layers) then
         write (err_messg,'(2(a,i3))')
     :                         ' soil layer no. ', layer
     :                        ,' is above maximum of ', num_layers
         call error (err_messg, IsNotFatal)

      endif

      return
      end



* ====================================================================
      subroutine soilwat2_read_constants ()
* ====================================================================
      use Soilwat2Module
      use ComponentInterfaceModule

      implicit none

*+  Purpose
*      Read in all coefficients from coefficient file.

*+  Mission Statement
*     Read Constants from Ini file

*+  Changes
*     ???
*     190595 jngh added specific bulk density
*     040995 nih  added mobile and immobile solutes
*     200896 jngh changed N_flow/flux to Solute_Flow/flux
*     210896 jngh changed upper bound of c%canopy_eos_coef from 1 to 10

*+  Constant Values
       character  my_name*(*)          ! name of this procedure
       parameter (my_name = 'soilwat2_read_constants')
*
       character  section_name*(*)
       parameter (section_name = 'constants')

*+  Local Variables
       integer numvals                 ! number of values read from file
       character  evap_method*300

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call write_string (
     :                   new_line//'    - Reading constants')

!      found = read_parameter (section_name
!     :                   , 'max_albedo'          !, '()'
!     :                   , c%max_albedo, numvals
!     :                   , 0.0, 1.0)

      found = read_parameter (section_name
     :                   , 'A_to_evap_fact'          !, '()'
     :                   , c%A_to_evap_fact, numvals
     :                   , 0.0, 1.0)

!      found = read_parameter (section_name
!     :                   , 'canopy_eos_coef'          !, '()'
!     :                   , c%canopy_eos_coef, numvals
!     :                   , 0.0, 10.0)

      found = read_parameter (section_name
     :                   , 'sw_top_crit'          !, '()'
     :                   , c%sw_top_crit, numvals
     :                   , 0.0, 1.0)

      found = read_parameter (section_name
     :                   , 'sumes1_max'          !, '()'
     :                   , c%sumes1_max, numvals
     :                   , 0.0, 1000.0)

      found = read_parameter (section_name
     :                   , 'sumes2_max'          !, '()'
     :                   , c%sumes2_max, numvals
     :                   , 0.0, 1000.0)

      found = read_parameter (section_name
     :                   , 'solute_flow_eff'          !, '()'
     :                   , c%Solute_flow_eff, numvals
     :                   , 0.0, 1.0)

      found = read_parameter (section_name
     :                   , 'solute_flux_eff'          !, '()'
     :                   , c%Solute_flux_eff, numvals
     :                   , 0.0, 1.0)

      found = read_parameter (section_name
     :                   , 'gravity_gradient'          !, '()'
     :                   , c%gravity_gradient, numvals
     :                   , 0.0, 1.0)

      found = read_parameter (section_name
     :                   , 'specific_bd'          !, '()'
     :                   , c%specific_bd, numvals
     :                   , 0.0, 3.0)

!      found = read_parameter (section_name
!     :                   , 'hydrol_effective_depth'          !, '(mm)'
!     :                   , c%hydrol_effective_depth, numvals
!     :                   , 1.0, 1000.0)
!
      found = read_parameter (section_name
     :                   , 'mobile_solutes'
     :                   , max_solute
     :                   , '()'
     :                   , c%mobile_solutes
     :                   , numvals)

      found = read_parameter (section_name
     :                   , 'immobile_solutes'
     :                   , max_solute
     :                   , '()'
     :                   , c%immobile_solutes
     :                   , numvals)

!      found = read_parameter (section_name
!     :                   , 'canopy_fact', max_coeffs     !, '()'
!     :                   , c%canopy_fact, g%num_canopy_fact
!     :                   , 0.0, 1.0)
!
!      found = read_parameter (section_name
!     :                   , 'canopy_fact_height', max_coeffs    !, '(mm)'
!     :                   , c%canopy_fact_height, numvals
!     :                   , 0.0, 100000.0)
!      if (numvals.ne. g%num_canopy_fact) then
!         call error ('No. of canopy_fact coeffs do not match '
!     :               //'no. of canopy_fact_height coeffs.'
!     :               , IsFatal)
!      else
!         ! matching number of coeffs
!      endif
!
!      found = read_parameter (section_name
!     :                   , 'canopy_fact_default'          !, '()'
!     :                   , c%canopy_fact_default, numvals
!     :                   , 0.0, 1.0)

      evap_method = 'unknown'
      found = read_parameter (section_name
     :                   , 'act_evap_method'          !, '()'
     :                   , evap_method
     :                   , numvals)

      if (evap_method .eq. 'ritchie') then
         c%evap_method = ritchie_method

      else if (evap_method .eq. 'bs_a') then
         c%evap_method = bs_a_method

      else if (evap_method .eq. 'bs_b') then
         c%evap_method = bs_b_method

      else if (evap_method .eq. 'bs_acs_jd') then
         c%evap_method = bs_acs_method

      else if (evap_method .eq. 'rickert') then
         c%evap_method = rickert_method

      else
         c%evap_method = -1  ! Force error somewhere later..

      endif

      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine soilwat2_soil_property_param ()
*     ===========================================================
      use Soilwat2Module
      use ComponentInterfaceModule

      implicit none

*+  Purpose
*       input initial values from soil property file.

*+  Mission Statement
*     Read Soil Parameters

*+  Changes
*       210191 specified and programmed jngh (j hargreaves
*       290591 jngh declared true as logical variable, defined
*               true=.true. - cr41
*               removed ferror=false - cr74
*       160992 jngh introduced write_string function for dual output
*       051093 jngh added fatal error to halt simulation
*       131093 markl added input of c%cn_red and c%cn_cov for cover/cn response
*       131093 markl added input of p%cona for soil evaporation
*       131093 markl added input of residue_wt for effects of residue on
*                          potential soil evaporation
*       131093 markl removed input of p%swcon from this subroutine and added
*                             it in soilwat2_soil_profile_param
*       190194 jpd  changed ulmcona from 5. to 10.
*                               query limits for cnred&cncov
*       290194 jpd  removed input of residue_wt,residue_cover, now in residue.
*       for
*       010994 jpd  removed input for crop_cover from parameter file.
*                           Currently (6/9/94) use 'lai' from crop modules
*       150994 jpd  added input for crop cover/ runoff  switch -
*                   'crpcov_rnof_switch'
*
*       300994 jpd added c%hydrol_effective_depth
*       181094 jpd removed crpcov_rnof option with '!!'
*       120195 jngh removed crop cover runoff switch
*                   removed combining residue cover and crop cover as done
*                   when getting other total cover from modules
*       210395 jngh changed from soilwat2_section to a parameters section
*       200896 jngh changed cn2 to cn2_bare
*                   changed reading of runoff filename to be optional

*+  Constant Values
      character  my_name*(*)            ! name of this module
      parameter (my_name = 'soilwat2_soil_property_param')
*
       character  section_name*(*)
       parameter (section_name = 'parameters')

*+  Local Variables
      integer    numvals               ! number of values returned

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call write_string (
     :          new_line//'   - Reading Soil Property Parameters')

          ! get runoff source
!      found = read_parameter (section_name
!     :                   ,'observed_runoff'          !, '()'
!     :                   , g%obsrunoff_name
!     :                   , numvals
!     :                   , IsOptional)   ! optional data

!      if ( numvals .eq. 0 .or.
!     :     g%obsrunoff_name .eq. 'blank') then
!         g%obsrunoff_name = blank              ! blank != 'blank' !!!
!      else
!         ! nothing - there's a valid string in g%obsrunoff_name
!      endif

          ! get sw parameters

      found = read_parameter (section_name
     :                   , 'insoil'          !, '()'
     :                   , p%insoil, g%numvals_insoil
     :                   , 0.0, 10.0
     :                   , IsOptional)   ! optional data

      found = read_parameter (section_name
     :                 , 'profile_esw_depth'          !, '(mm)'
     :                 , p%profile_esw_depth
     :                 , g%numvals_profile_esw_depth
     :                 , 0.0, 10000.0
     :                   , IsOptional)  ! optional data

      found = read_parameter (section_name
     :                 , 'wet_soil_depth'          !, '(mm)'
     :                 , p%wet_soil_depth
     :                 , g%numvals_wet_soil_depth
     :                 , 0.0, 10000.0
     :                   , IsOptional)   ! optional data

      found = read_parameter (section_name
     :                 , 'profile_fesw'          !, '()'
     :                 , p%profile_fesw
     :                 , g%numvals_profile_fesw
     :                 , 0.0, 1.0
     :                   , IsOptional)   ! optional data

      found = read_parameter (section_name
     :                   , 'diffus_const'          !, '()'
     :                   , p%diffus_const, numvals
     :                   , 0.0, 1000.0)

      found = read_parameter (section_name
     :                   , 'diffus_slope'          !, '()'
     :                   , p%diffus_slope, numvals
     :                   , 0.0, 100.0)

!      found = read_parameter (section_name
!     :                   , 'cn2_bare'          !, '()'
!     :                   , p%cn2_bare, numvals
!     :                   , 1.0, 100.0)

!      found = read_parameter (section_name
!     :                   , 'cn_red'          !, '()'
!     :                   , p%cn_red, numvals
!     :                   , 0.0, p%cn2_bare - 0.00009)

!      found = read_parameter (section_name
!     :                   , 'cn_cov'          !, '()'
!     :                   , p%cn_cov, numvals
!     :                   , 0.0, 1.0)

!      found = read_parameter (section_name
!     :                   , 'max_pond'          !, '()'
!     :                   , p%max_pond, numvals
!     :                   , 0.0, 1000.0
!     :                   , IsOptional)  ! optional data


!      found = read_parameter (section_name
!     :                   , 'salb'          !, '()'
!     :                   , p%salb, numvals
!     :                   , 0.0001, 1.0)

*     Extra parameters for evaporation models:
      if (c%evap_method .eq. ritchie_method .or.
     :     c%evap_method .eq. bs_acs_method) then
         found = read_parameter (section_name
     :        , 'cona'          !, '()'
     :        , p%cona, numvals
     :        , 0.0001, 10.0)

      else

         p%cona = 0.0001
      endif

      if (c%evap_method .eq. ritchie_method .or.
     :     c%evap_method .eq. bs_acs_method) then
         found = read_parameter (section_name
     :        , 'u'          !, '()'
     :        , p%u, numvals
     :        , 0.0001, 40.0)
      else
         p%u = 0.0001
      endif

      if (c%evap_method .eq. bs_a_method .or.
     :     c%evap_method .eq. bs_b_method .or.
     :     c%evap_method .eq. bs_acs_method) then
         found = read_parameter (section_name
     :        , 'beta'          !, '()'
     :        , p%beta, numvals
     :        , 0.0, 3.5)

      else

         p%beta = 0.0
      endif

      if (c%evap_method .eq. rickert_method) then
         found = read_parameter (section_name
     :        , 'max_evap'          !, '()'
     :        , p%max_evap, numvals
     :        , 0.9, 20.0)

      else

         p%max_evap = 0.0
      endif

!      found = read_parameter (section_name
!     :                   ,'eo_source'          !, '()'
!     :                   , g%eo_source
!     :                   , numvals
!     :                   , IsOptional)       ! optional data
!      if (numvals .le. 0) then
!          g%eo_source = blank
!      else
!      endif

      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine soilwat2_soil_profile_param ()
*     ===========================================================
      use Soilwat2Module
      use ComponentInterfaceModule

      implicit none

*+  Purpose
*       input initial values from soil parameter file.

*+  Mission Statement
*     Read Initial Soil Profile Values

*+  Changes
*       210191 specified and programmed jngh (j hargreaves
*       281191 jngh tidy up as per cr257.
*                   changed error checking to be more responsive.
*                   removed unused variables & corrected lmm to llm - cr256
*                   check ios flag instead of ferror - cr258
*       290892 jngh changed soil water to depth of water
*       160992 jngh introduced write_string function for dual output
*       131093 markl added p%swcon as an array for each soil layer
*       190194 jpd   added air_dry_dep as an array for each soil layer
*       210395 jngh changed from soilwat2_section to a parameters section
*       190595 jngh added bulk density
*       190897 nih  moved insoil sw set from higher level for reuse reasons

*+  Constant Values
      character  my_name*(*)         ! name of this module
      parameter (my_name = 'soilwat2_soil_profile_param')
*
       character  section_name*(*)
       parameter (section_name = 'parameters')

*+  Local Variables
      integer    layer                 ! layer number in loop
      integer    num_layers            ! number of layers in profile
      integer    numvals               ! number of values returned
      real       air_dry (max_layer)   ! air dry soilwat2er content
                                       ! for soil layer l (mm water/mm soil)
      real       dul (max_layer)       ! drained upper limit soilwat2er content
                                       ! for soil layer l (mm water/mm soil)
      real       ll15 (max_layer)      ! 15 bar lower limit of extractable
                                       ! soil water (mm water/mm soil)
      real       sat (max_layer)       ! saturated water content for layer l
                                       ! (mm water/mm soil)
      real       sw(max_layer)         ! soil water content (mm water/mm soil)
      character msg*200                ! message to summary file

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call write_string (
     :          new_line//'   - Reading Soil Profile Parameters')

                 ! get sw properties

      found = read_parameter (section_name
     :                     , 'irrigation_layer'          !,'()'
     :                     , p%irrigation_layer, numvals
     :                     , 0, 100
     :                   , IsOptional)     ! optional data

      found = read_parameter (section_name
     :                     , 'dlayer', max_layer      !, '(mm)'
     :                     , p%dlayer, numvals
     :                     , 0.0, 10000.0)

      found = read_parameter (section_name
     :                     , 'sat', max_layer      !, '()'
     :                     , sat, numvals
     :                     , 0.0, 1000.0)

      found = read_parameter (section_name
     :                     , 'dul', max_layer      !, '()'
     :                     , dul, numvals
     :                     , 0.0, 1000.0)

      found = read_parameter (section_name
     :                     , 'sw', max_layer    !, '()'
     :                     , sw, g%numvals_sw
     :                     , 0.0, 1000.0
     :                   , IsOptional)    ! optional data

      found = read_parameter (section_name
     :                     , 'll15', max_layer     !, '()'
     :                     , ll15, numvals
     :                     , 0.0, 1000.0)

      found = read_parameter (section_name
     :                     , 'air_dry', max_layer     !, '()'
     :                     , air_dry, numvals
     :                     , 0.0, 10.0)

      found = read_parameter (section_name
     :                     , 'swcon', max_layer    !, '()'
     :                     , p%swcon, numvals
     :                     , 0.0, 1000.0)

      found = read_parameter (section_name
     :                     , 'mwcon', max_layer    !, '()'
     :                     , p%mwcon, numvals
     :                     , 0.0, 1000.0
     :                   , IsOptional)     ! optional data

c dsg - if there is no impermeable layer specified, then mwcon must
c       be set to '1' in all layers by default

      if (numvals.eq.0) then
          p%mwcon(:) = 1.0
      endif


      found = read_parameter ( section_name
     :                     , 'bd', max_layer    !, '(g/cc)'
     :                     , g%bd, numvals
     :                     , 0.01, 10.0)

      num_layers = count_of_real_vals (p%dlayer, max_layer)
      do 1010 layer = 1,num_layers

         g%air_dry_dep(layer) = air_dry(layer)*p%dlayer(layer)
         g%dul_dep(layer)     = dul(layer)    *p%dlayer(layer)
         g%ll15_dep(layer)    = ll15(layer)   *p%dlayer(layer)
         g%sat_dep(layer)     = sat(layer)    *p%dlayer(layer)
         g%sw_dep(layer)      = sw(layer)     *p%dlayer(layer)

1010  continue

          ! get sw parameters

      if (g%numvals_insoil .gt. 0
     :    .and. p%insoil.ge.0.0 .and. p%insoil.le.1.0) then

         msg = 'Soil water in parameter file is being overridden by' //
     :         new_line //
     :         'the insoil parameter which is between 0 and 1'

         call write_string (new_line // msg)
         g%numvals_sw = 0
      else
         g%numvals_insoil = 0
      endif

      call soilwat2_set_default ()
      do 1020 layer = 1,num_layers
         call soilwat2_check_profile (layer)
1020  continue

      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine soilwat2_set_default ()
*     ===========================================================
      use Soilwat2Module
      use ComponentInterfaceModule

      implicit none

*+  Purpose
*       set default soil water values

*+  Mission Statement
*     Set Default Soil Water Values

*+  Changes
*        210191   specified and programmed jngh (j hargreaves
*        290892   jngh changed soil water to depth of water
*        100801   jngh added profile_esw_depth

*+  Calls
                                       ! cells used in array

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_set_default')

*+  Local Variables
      integer    layer                 ! layer number in loop
      integer    num_layers            ! number of layers used in profile
      integer    num_layers_filled     ! number of layers filled in profile
      real       esw_remaining         ! esw left after distribution top down (mm)
      real       depth_remaining       ! depth left after distribution top down (mm)
      real       esw_avail             ! esw available for distribution (mm)
      real       profile_esw_depth     ! depth of esw in profie to fill (mm)
      character  line*100              ! temp output record

*- Implementation Section ----------------------------------

      call push_routine (my_name)
               ! check for exclusiveness
      if (g%numvals_profile_esw_depth .gt. 0) then
         if (g%numvals_insoil .gt. 0
     :       .or. g%numvals_sw .gt. 0
     :       .or. g%numvals_profile_fesw .gt. 0
     :       .or. g%numvals_wet_soil_depth .gt. 0) then
               ! others present
            call error (
     :             'Insoil, Sw, profile_fesw or '
     :           //'wet_soil_depth cannot be '
     :           //'specified with "profile_esw_depth".'
     :           , IsFatal)
         else
            ! numvals_profile_esw_depth present only
            line = 'Initial soilwater distributed from top down '
     :           //'using "profile_esw_depth" parameter.'
            call write_string (line)
         endif

      elseif (g%numvals_wet_soil_depth .gt. 0) then
            ! numvals_profile_esw_depth absent
         if (g%numvals_insoil .gt. 0
     :       .or. g%numvals_profile_fesw .gt. 0
     :       .or. g%numvals_sw .gt. 0) then
               ! others present
            call error (
     :             'Insoil, Profile_fesw or Sw '
     :           //'cannot be specified with '
     :           //'"wet_soil_depth".'
     :           , IsFatal)
         else
            line = 'Initial soilwater distributed from top down '
     :           //'using "wet_soil_depth" parameter.'
            call write_string (line)
         endif
      elseif (g%numvals_profile_fesw .gt. 0) then
            ! numvals_profile_esw_depth absent
         if (g%numvals_insoil .gt. 0
     :       .or. g%numvals_sw .gt. 0) then
               ! others present
            call error (
     :             'Insoil or Sw '
     :           //'cannot be specified with '
     :           //'"profile_fesw".'
     :           , IsFatal)
         else
            line = 'Initial soilwater distributed from top down '
     :           //'using "profile_fesw" parameter.'
            call write_string (line)
         endif
      elseif (g%numvals_insoil .gt. 0) then
         if (g%numvals_sw .gt. 0) then
               ! note - this never activates because the switches are set previously
            call error (
     :             'Sw cannot be specified with '
     :           //'"insoil".'
     :           , IsFatal)
            call write_string (line)

         else
            ! only insoil present
            line = 'Initial soilwater distributed evenly '
     :           //'using "insoil" parameter.'
            call write_string (line)
         endif

      elseif (g%numvals_sw .gt. 0)  then
         ! ok - only sw present
            line = 'Initial soilwater distributed '
     :           //'using "sw" parameter.'
            call write_string (line)
      else
               ! all absent - must have one
            call error (
     :             'Must specify one of '
     :           //'Insoil, Sw, wet_soil_depth, '
     :           //'Profile_fesw or Profile_esw_depth '
     :           //'to specify initial soilwater distribution.'
     :           , IsFatal)
      endif

                ! initialize sw
                ! set up default soil water profile

                ! we want to calculate default

      if (g%numvals_insoil .gt. 0) then
            ! insoil parameter set - distibute evenly
         g%sw_dep(:) = 0.0
         num_layers = count_of_real_vals (p%dlayer, max_layer)

         do 1000 layer = 1,num_layers

                 ! set default according to insoil fraction of plant-
                 ! available water

            g%sw_dep(layer) = g%ll15_dep(layer)
     :                   + (g%dul_dep(layer) - g%ll15_dep(layer))
     :                   * p%insoil

            call soilwat2_layer_check (layer)
            call soilwat2_check_profile (layer)

1000     continue
      elseif (g%numvals_wet_soil_depth .gt. 0) then
            ! wet_soil_depth parameter set - distribute top down
         g%sw_dep(:) = 0.0
         num_layers = count_of_real_vals (p%dlayer, max_layer)
         g%sw_dep(1:num_layers) = g%ll15_dep(1:num_layers)

         num_layers_filled = find_layer_no (
     :                              p%wet_soil_depth
     :                            , p%dlayer
     :                            , num_layers)

         do 2000 layer = 1,num_layers_filled

                 ! set default according to wet_soil_depth of plant-
                 ! available water

            g%sw_dep(layer) = g%dul_dep(layer)

2000     continue
         ! adjust last layer
         g%sw_dep(num_layers_filled) = g%ll15_dep(num_layers_filled)
     :                               + (g%dul_dep(num_layers_filled)
     :                                 - g%ll15_dep(num_layers_filled))
     :                            * root_proportion (num_layers_filled
     :                                 , p%dlayer
     :                                 , p%wet_soil_depth)

         if (sum(p%dlayer)+precision_sw_dep .lt. p%wet_soil_depth) then
            write (line, *) 'Can''t fit wet soil depth of '
     :                         , p%wet_soil_depth
     :                         , ' into profile depth of '
     :                         , sum(p%dlayer)
           call error (line, IsFatal)

         else
            ! depth fits in profile
         endif

      elseif (g%numvals_profile_fesw .gt. 0) then
            ! profile_fesw parameter set - distribute top down
         g%sw_dep(:) = 0.0
         num_layers = count_of_real_vals (p%dlayer, max_layer)
         g%sw_dep(1:num_layers) = g%ll15_dep(1:num_layers)
         profile_esw_depth = sum(g%dul_dep(:) - g%ll15_dep(:))
     :                     * p%profile_fesw
         esw_remaining = profile_esw_depth

         do 3000 layer = 1, num_layers
                 ! set default according to profile_esw_depth of plant-
                 ! available water
            esw_avail =  bound (esw_remaining
     :                         , 0.0
     :                         , g%dul_dep(layer) - g%ll15_dep(layer))

            g%sw_dep(layer) = g%ll15_dep(layer) + esw_avail
            esw_remaining = esw_remaining - esw_avail

3000     continue
         if (esw_remaining .gt. precision_sw_dep) then
!         if (esw_remaining .gt. 0.0) then
              ! we have too much water to distirbute - won't fit in profile
            write (line, *) 'Can''t fit profile esw of '
     :                         , profile_esw_depth + esw_remaining
     :                         , ' into profile esw depth of '
     :                         , profile_esw_depth
           call error (line, IsFatal)

         else
            ! it fits
         endif

      elseif (g%numvals_profile_esw_depth .gt. 0) then
            ! profile_esw_depth parameter set - distribute top down
         g%sw_dep(:) = 0.0
         num_layers = count_of_real_vals (p%dlayer, max_layer)
         g%sw_dep(1:num_layers) = g%ll15_dep(1:num_layers)

         esw_remaining = p%profile_esw_depth

         do 4000 layer = 1, num_layers
                 ! set default according to profile_esw_depth of plant-
                 ! available water
            esw_avail =  bound (esw_remaining
     :                         , 0.0
     :                         , g%dul_dep(layer) - g%ll15_dep(layer))

            g%sw_dep(layer) = g%ll15_dep(layer) + esw_avail
            esw_remaining = esw_remaining - esw_avail

4000     continue
         if (esw_remaining .gt. precision_sw_dep) then
              ! we have too much water to distirbute - won't fit in profile
            profile_esw_depth = sum(g%dul_dep(:) - g%ll15_dep(:))
            write (line, *) 'Can''t fit profile esw of '
     :                         , p%profile_esw_depth
     :                         , ' into profile esw depth of '
     :                         , profile_esw_depth
           call error (line, IsFatal)

         else
            ! it fits
         endif

      elseif (g%numvals_sw .gt. 0) then
         ! do nothing
      else
         call error (
     :   'Initial soilwater distribution method not defined.'
     :           , IsFatal)


      endif

      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine soilwat2_evap_init
*     ===========================================================
      use Soilwat2Module
      use ComponentInterfaceModule

      implicit none

*+  Purpose
*     Wrapper for evaporation methods

*+  Mission Statement
*     Evaporation Initialisation

*+  Changes
*       210191 specified and programmed jngh (j hargreaves

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_evap_init')

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      if (c%evap_method .eq. ritchie_method) then
         call soilwat2_ritchie_init ()

      else if (c%evap_method .eq. bs_a_method) then
         call soilwat2_bs_a_init ()

      else if (c%evap_method .eq. bs_b_method) then
         call soilwat2_bs_b_init ()

      else if (c%evap_method .eq. bs_acs_method) then
         call soilwat2_bs_acs_init ()

      else if (c%evap_method .eq. rickert_method) then
         call soilwat2_rickert_init ()

      else
         call error(
     :        'Tried to initialise unknown evaporation method'
     :           , IsFatal)

      endif

      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine soilwat2_ritchie_init
*     ===========================================================
      use Soilwat2Module
      use ComponentInterfaceModule

      implicit none

*+  Purpose
*       initialize ritchie evaporation model

*+  Mission Statement
*       Initialise ritchie evaporation model

*+  Changes
*       210191 specified and programmed jngh (j hargreaves
*       290892 jngh changed soil water to depth of water
*       160992 jngh changed constants to named constants
*       131093 markl added replaced 3.5 constant with p%cona variable
*
*       190194 jpd initialization for evap needs re-working.
*      no check on sumes1 wrt. u. sumes1 will be =>10mm if swr_top >sw_crit_top

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_ritchie_init')

*+  Local Variables
                                       ! stage 2 evaporation occurs
      real       swr_top               ! ratio available sw :
                                       !    potentially available sw
                                       ! in top layer

*- Implementation Section ----------------------------------

      call push_routine (my_name)

          ! set up evaporation stage

      swr_top = divide (g%sw_dep(1) - g%ll15_dep(1)
     :                , g%dul_dep(1) - g%ll15_dep(1), 0.0)
      swr_top = bound (swr_top, 0.0, 1.0)

          ! are we in stage1 or stage2 evap?
      if (swr_top.lt.c%sw_top_crit) then

             ! stage 2 evap
         g%sumes2 = c%sumes2_max
     :            - c%sumes2_max * divide (swr_top, c%sw_top_crit, 0.0)
         g%sumes1 = p%u
         g%t = (divide (g%sumes2, p%cona, 0.0))**2
      else

             ! stage 1 evap
         g%sumes2 = 0.0
         g%sumes1 = c%sumes1_max - c%sumes1_max *swr_top
         g%t = 0.0
      endif

      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine soilwat2_bs_a_init
*     ===========================================================
      use Soilwat2Module
      use ComponentInterfaceModule

      implicit none

*+  Purpose
*       B&S option A initialisation

*+  Mission Statement
*       Initialise B&S (A) model for evaporation

*+  Changes
*       210191 specified and programmed jngh (j hargreaves

*+  Calls
!      real       bound                 ! function to contain within bounds

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_bs_a_init')

*+  Local Variables
*      character err_mesg*300    ! message string
      real       sumes_max

*- Implementation Section ----------------------------------

      call push_routine (my_name)

*     B&S evaporation:
*      cumulative actual evap = beta * sqrt(cumulative eos)

      sumes_max = g%dul_dep(1) - g%air_dry_dep(1)
      g%sumes = g%dul_dep(1) - g%sw_dep(1)

      if (g%sumes .le. 0.0) then

*     ! initial sw is at, or above, DUL.

         g%sumes = 0.0
         g%sumes1 = 0.0
         g%sumes2 = 0.0
         g%sumeos = 0.0

      else if (g%sumes .gt. sumes_max) then

*     Initial sw is less than air_dry(1):
*     Write a warning message to summary file.
*PdeV. This check is in soilwat2_check_profile. Is it necessary here?
*         write (err_messg, '(a, g17.6e3, a, 2a, g17.6e3)')
*     :        ' soil water of ', g%sw_dep(1)
*     :        ,' in top layer '
*     :        , new_line
*     :        ,' is below air_dry value of ', g%air_dry_dep(1)
*         call warning_error (err_internal, err_messg)

         g%sumes = sumes_max
         g%sumes1 = p%beta**2
         g%sumes2 = g%sumes - g%sumes1
         g%sumeos = divide (g%sumes**2, p%beta**2, 0.0)


      elseif (g%sumes .ge. (p%beta**2)) then

*     Initial sw is not close to DUL.
*      1st stage evaporation is finished, start in 2nd stage

         g%sumes1 = p%beta**2
         g%sumes2 = g%sumes - g%sumes1
         g%sumeos = divide (g%sumes**2, p%beta**2, 0.0)

      else

*     Initial sw is close to DUL.
*      We're in 1st stage evaporation.

         g%sumes1 = g%sumes
         g%sumes2 = 0.0
         g%sumeos = g%sumes

      endif

      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine soilwat2_bs_b_init
*     ===========================================================
      use Soilwat2Module
      use ComponentInterfaceModule

      implicit none

*+  Purpose
*     B&S option B initialisation

*+  Mission Statement
*     Initialise B&S (B) model for evaporation

*+  Changes
*       210191 specified and programmed jngh (j hargreaves

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_bs_b_init')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

cpdev Andy didn't write anything for option B initialisation. But I think
c     he should have. Any ideas? Perhaps
      call soilwat2_bs_a_init()

                                ! Nothing
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine soilwat2_bs_acs_init
*     ===========================================================
      use Soilwat2Module
      use ComponentInterfaceModule

      implicit none

*+  Purpose
*     B&S option B initialisation. (Andy smith + John dimes' version)
*
*     PdeV - I hope andy wasn't recycling variable names here, as this is
*     the only time cona and u are used with this model.

*+  Mission Statement
*     Initialise B&S (B, ACS) model for evaporation

*+  Changes
*       210191 specified and programmed jngh (j hargreaves

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_bs_acs_init')

*+  Local Variables
*     acs/jpd
*     NOTE: sumes2_max & sumes_max only apply for initialization.
*     For model run, evaporation can continue from layr(1)indefinitly
*     because water is moving up from layer below (by unsaturated flow).
      real       sumes2_max     ! upper limit of sumes2. Related to
                                ! evaporative water capacity of layr(1)
!      real       sumes_max      ! upper limit of cumulative soil evap
!                                ! for B&S. Also f(evap.wat.cap layr(1))

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      sumes2_max = g%dul_dep(1) - g%air_dry_dep(1) - p%u

      if ((g%dul_dep(1) - g%sw_dep(1)) .LT. p%u) then
                                ! In first stage
         g%sumes1 = g%dul_dep(1) - g%sw_dep(1)

         if(g%sumes1 .lt. 0.0) then
            g%sumes1 = 0.0      ! initial sw greater than DUL
         else
                                !
         endif
         g%sumes2 = 0.0
         g%t = 0.0

      else
                                ! In second stage
         g%sumes1 = p%u
         g%sumes2 = g%dul_dep(1) - g%sw_dep(1) - p%u

         if (g%sumes2 .GT. sumes2_max) then

*     init sw must be .lt. air_dry
            g%sumes2 = sumes2_max

         endif

         g%t = divide(g%sumes2, p%cona, 0.0) **2

      endif

      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine soilwat2_rickert_init
*     ===========================================================
      use Soilwat2Module
      use ComponentInterfaceModule

      implicit none

*+  Purpose
*     Rickert initialisation

*+  Mission Statement
*     Calculate evaporation using Rickert model

*+  Changes
*       210191 specified and programmed jngh (j hargreaves

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_rickert_init')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (p%diffus_const .gt. 0.0 .or.
     :     p%diffus_slope .gt. 0.0) then
         call error (
     :     'diffus_const and diffus_slope should be off for rickert'
     :           , IsNotFatal)
      else
                                ! Nothing
      endif
      call pop_routine (my_name)
      return
      end

* ====================================================================
      subroutine soilwat2_zero_default_variables ()
* ====================================================================
      use Soilwat2Module
      use ComponentInterfaceModule

      implicit none

*+  Purpose
*      zero default soil water initialisation parameters

*+  Mission Statement
*     zero default soil water initialisation parameters

*+  Changes
*     150801 jngh

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_zero_default_variables')

*+  Local Variables
      integer    numvals               ! number of values put into array

*- Implementation Section ----------------------------------

      call push_routine (my_name)

         g%numvals_insoil = 0
         g%numvals_sw = 0
         g%numvals_profile_esw_depth = 0
         g%numvals_wet_soil_depth = 0
         g%numvals_profile_fesw = 0

         p%insoil = 0
         g%sw_dep(:) = 0
         p%profile_esw_depth = 0
         p%wet_soil_depth = 0
         p%profile_fesw = 0

      call pop_routine (my_name)
      return
      end

* ====================================================================
       logical function respondToSet (fromID,VariableID, variant)
* ====================================================================
      use Soilwat2Module
      use ComponentInterfaceModule

      implicit none

      ml_external respondToSet

!+  Sub-Program Arguments
      integer, intent(in) :: fromID
      integer, intent(in)     :: VariableID
      integer, intent(in out) :: variant

*+  Purpose
*     Set one of our variables altered by some other module

*+  Changes
*      21-06-96 NIH Changed respond2set calls to collect calls

      real       fract                 ! temporary fraction
      integer    layer                 ! layer counter
      integer    num_layers            ! number of layers
      integer    numvals               ! number of values returned in array
      real       temp(max_layer)       ! temporary array

*- Implementation Section ----------------------------------

!      if (variable_name .eq. 'sw') then
      if (VariableID .eq. sw_id) then
         call soilwat2_zero_default_variables ()

!         call collect_real_array (variable_name, max_layer, '()'
!     :                               , temp, g%numvals_sw
!     :                               , 0.0, 1.0)
         call Unpack_sw (Variant, temp, g%numvals_sw)

!jh         call soilwat2_set_default ()   ! this causes output to occur whenever a module changes "sw", such as nwheat!
         num_layers = count_of_real_vals (p%dlayer, max_layer)
         do 1000 layer = 1,num_layers
            g%sw_dep(layer) = temp(layer)*p%dlayer(layer)
            call soilwat2_check_profile (layer)
1000     continue

      elseif (VariableID .eq. sw_dep_ID) then
         call soilwat2_zero_default_variables ()

         call Unpack_sw (Variant, g%sw_dep, g%numvals_sw)
         num_layers = count_of_real_vals (p%dlayer, max_layer)
         do 2000 layer = 1,num_layers
            call soilwat2_check_profile (layer)
2000     continue

      elseif (VariableID .eq. insoil_ID) then

         call soilwat2_zero_default_variables ()
         call Unpack_sw (Variant, p%insoil, g%numvals_insoil)

         call soilwat2_set_default ()
         num_layers = count_of_real_vals (p%dlayer, max_layer)
         do 2100 layer = 1,num_layers
            call soilwat2_check_profile (layer)
2100     continue

      elseif (VariableID .eq. profile_esw_depth_ID) then
         call soilwat2_zero_default_variables ()

         call Unpack_sw (Variant,
     :                 , p%profile_esw_depth
     :                 , g%numvals_profile_esw_depth)

         call soilwat2_set_default ()
         num_layers = count_of_real_vals (p%dlayer, max_layer)
         do 2200 layer = 1,num_layers
            call soilwat2_check_profile (layer)
2200     continue

      elseif (VariableID .eq. wet_soil_depth_ID) then
         call soilwat2_zero_default_variables ()

         call Unpack_sw (Variant,
     :                 , p%wet_soil_depth
     :                 , g%numvals_wet_soil_depth)

         call soilwat2_set_default ()
         num_layers = count_of_real_vals (p%dlayer, max_layer)
         do 2300 layer = 1,num_layers
            call soilwat2_check_profile (layer)
2300     continue

      elseif (VariableID .eq. profile_fesw_ID) then
         call soilwat2_zero_default_variables ()

         call Unpack_sw (Variant,
     :                 , p%profile_fesw
     :                 , g%numvals_profile_fesw)

         call soilwat2_set_default ()
         num_layers = count_of_real_vals (p%dlayer, max_layer)
         do 2400 layer = 1,num_layers
            call soilwat2_check_profile (layer)
2400     continue


      elseif (VariableID .eq. dlt_sw_ID) then

         call Unpack_sw (Variant, temp, numvals)

         num_layers = count_of_real_vals (p%dlayer, max_layer)
         do 3000 layer = 1,num_layers
            g%sw_dep(layer) = g%sw_dep(layer)
     :                      + temp(layer)*p%dlayer(layer)
            call soilwat2_check_profile (layer)
3000     continue

      elseif (VariableID .eq. dlt_sw_dep_ID) then
         call Unpack_sw (Variant, temp, numvals)

         num_layers = count_of_real_vals (p%dlayer, max_layer)
         do 4000 layer = 1,num_layers
            g%sw_dep(layer) = g%sw_dep(layer) + temp(layer)
            call soilwat2_check_profile (layer)
4000     continue

* code for erosion

      elseif (VariableID .eq. dul_dep_ID) then
         call Unpack_sw (Variant, g%dul_dep, numvals)

         num_layers = count_of_real_vals (p%dlayer, max_layer)
         do 4100 layer = 1,num_layers
            call soilwat2_check_profile (layer)
4100     continue

      elseif (VariableID .eq. dul_ID) then
         call Unpack_sw (Variant, temp, numvals)

         num_layers = count_of_real_vals (p%dlayer, max_layer)
         do 4110 layer = 1,num_layers
            g%dul_dep(layer) = temp(layer)*p%dlayer(layer)
            call soilwat2_check_profile (layer)
4110     continue

      elseif (VariableID .eq. ll15_dep_ID) then
         call Unpack_sw (Variant, g%ll15_dep, numvals)

         num_layers = count_of_real_vals (p%dlayer, max_layer)
         do 4200 layer = 1,num_layers
            call soilwat2_check_profile (layer)
4200     continue

      elseif (VariableID .eq. ll15_ID) then
         call Unpack_sw (Variant, temp, numvals)

         num_layers = count_of_real_vals (p%dlayer, max_layer)
         do 4210 layer = 1,num_layers
            g%ll15_dep(layer) = temp(layer)*p%dlayer(layer)
            call soilwat2_check_profile (layer)
4210     continue

      elseif (VariableID .eq. sat_dep_ID) then
         call Unpack_sw (Variant, g%sat_dep, numvals)

         num_layers = count_of_real_vals (p%dlayer, max_layer)
         do 4300 layer = 1,num_layers
            call soilwat2_check_profile (layer)
4300     continue

      elseif (VariableID .eq. sat_ID) then
         call Unpack_sw (Variant, temp, numvals)

         num_layers = count_of_real_vals (p%dlayer, max_layer)
         do 4310 layer = 1,num_layers
            g%sat_dep(layer) = temp(layer)*p%dlayer(layer)
            call soilwat2_check_profile (layer)
4310     continue

      elseif (VariableID .eq. air_dry_dep_ID) then
         call Unpack_sw (Variant, g%air_dry_dep, numvals)

         num_layers = count_of_real_vals (p%dlayer, max_layer)
         do 4500 layer = 1,num_layers
            call soilwat2_check_profile (layer)
4500     continue

      elseif (VariableID .eq. air_dry_ID) then
         call Unpack_sw (Variant, temp, numvals)

         num_layers = count_of_real_vals (p%dlayer, max_layer)
         do 4510 layer = 1,num_layers
            g%air_dry_dep(layer) = temp(layer)*p%dlayer(layer)
            call soilwat2_check_profile (layer)
4510     continue

      elseif (VariableID .eq. dlayer_ID) then
         call Unpack_sw (Variant, temp, numvals)

         do 5000 layer = 1, numvals
            fract = divide (temp(layer), p%dlayer(layer), 0.0)

            g%air_dry_dep(layer) = g%air_dry_dep(layer) * fract
            g%dul_dep(layer) = g%dul_dep(layer) * fract
            g%ll15_dep(layer) = g%ll15_dep(layer) * fract
            g%sat_dep(layer) = g%sat_dep(layer) * fract
            g%sw_dep(layer) = g%sw_dep(layer) * fract
            p%dlayer(layer) = temp(layer)

            call soilwat2_check_profile (layer)
5000     continue
         do 5100 layer = numvals+1, max_layer

            g%air_dry_dep(layer) = 0.0
            g%dul_dep(layer) = 0.0
            g%ll15_dep(layer) = 0.0
            g%sat_dep(layer) = 0.0
            g%sw_dep(layer) = 0.0
            p%dlayer(layer) = 0.0

5100     continue

         call soilwat2_Publish_soil_water_profile ()

      elseif (VariableID .eq. dlt_dlayer_ID) then
         call Unpack_sw (Variant, temp, numvals)

         do 6000 layer = 1, numvals
            temp(layer) = p%dlayer(layer) + temp(layer)
            fract = divide (temp(layer), p%dlayer(layer), 0.0)

            g%air_dry_dep(layer) = g%air_dry_dep(layer) * fract
            g%dul_dep(layer) = g%dul_dep(layer) * fract
            g%ll15_dep(layer) = g%ll15_dep(layer) * fract
            g%sat_dep(layer) = g%sat_dep(layer) * fract
            g%sw_dep(layer) = g%sw_dep(layer) * fract
            p%dlayer(layer) = temp(layer)

            call soilwat2_check_profile (layer)
6000     continue
         do 6100 layer = numvals+1, max_layer

            g%air_dry_dep(layer) = 0.0
            g%dul_dep(layer) = 0.0
            g%ll15_dep(layer) = 0.0
            g%sat_dep(layer) = 0.0
            g%sw_dep(layer) = 0.0
            p%dlayer(layer) = 0.0
 6100     continue

         call soilwat2_Publish_soil_water_profile ()
* end code for erosion

      elseif (VariableID .eq. cona_ID) then
         call Unpack_sw (Variant, p%cona, numvals)

      elseif (VariableID .eq. u_ID) then
         call Unpack_sw (Variant, p%u, numvals)
      else
         call Message_unused ()

      endif

      respondToSet = .true.
      return
      end





* ====================================================================
       subroutine respondToGet (fromID, Variable_info)
* ====================================================================
      use Soilwat2Module
      use ComponentInterfaceModule
      implicit none
      ml_external respondToGet

*+  Sub-Program Arguments
      integer, intent(in) :: fromID
      type(QueryData), intent(in) :: variable_info

*+  Purpose
*      return the value of a variable in return_string.  used to return
*      values of variables requested by other modules.

*+  Notes
*      a flag is set if any of the totals is requested.  the totals are
*      reset during the next process phase when this happens.

*+  Mission Statement
*     Send Value of Requested Variable

*+  Changes
*      031292 jngh
*      170393 jngh changed for units and new engine
*      020893 jngh added more variables to report and changed total names.
*      030294 jpd added eos
*      060994 jpd added crop_cover, crop_cover_max, total_cover,cn2new
*      120195 jngh removed crop_cover_max as crop module should maintain
*                  a similar figure in total cover.
*      190595 jngh added bulk density
*      300695 jngh added eo to output
*      180895 nih  upgraded the solute output stuff to match muti-solute
*                  approached that is now used.
*      261095 DPH  Added call to message_unused
*      130896 jngh removed crop_cover (g%cover_green_sum)
*      260897 nih  Added output for flow_water and flow_(solute_name)
*      970910 slw  fix problem with es reporting as zero
*      990323 nih  Added output for effective rainfall (eff_rain)
*      021199 jngh removed export of total_cover

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_send_my_variable')

*+  Local Variables
!      real       crop_cover            ! sum of crop covers (0-1)
      real       esw                   ! potential extractable sw in profile
      integer    layer                 ! layer counter
      integer    num_layers            ! number of layers
      integer    solnum                ! solute no. counter
      character  solute_name*32        ! solute name
      real       temp_array(max_layer) ! temporary array
!      real       total_cover           ! total ground cover (0-1)
      real       es                    ! total es
!      real       eff_rain              ! daily effective rainfall (mm)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Variable_info%id .eq. es_ID) then
         es = sum_real_array(g%es_layers, max_layer)
         call return_es (Variable_info,           ! '(mm)'
     :                              , es)

!      else if (Variable_info%id .eq. eo_ID) then
!         call return_eo (Variable_info,           ! '(mm)'
!     :                              , g%eo)
!
!      else if (Variable_info%id .eq. eos_ID) then
!         call return_eos (Variable_info,           ! '(mm)'
!     :                              , g%eos)
!
!      else if (Variable_info%id .eq. total_cover_ID) then
!         crop_cover = sum_cover_array (g%cover_tot, g%num_crops)
!         total_cover = add_cover (crop_cover, g%residue_cover)
!         call return_ (Variable_info,           ! '()'
!     :                             , total_cover)

!      else if (Variable_info%id .eq. cover_surface_runoff_ID) then
!         call return_cover_surface_runoff (Variable_info,           ! '()'
!     :                             , g%cover_surface_runoff)

!      else if (Variable_info%id .eq. cn2_new_ID) then
!         call return_cn2_new (Variable_info,           ! '()'
!     :                              , g%cn2_new)

!      else if (Variable_info%id .eq. runoff_ID) then
!         call return_runoff (Variable_info,           ! '(mm)'
!     :                              , g%runoff)

!      else if (Variable_info%id .eq. pond_ID) then
!         call return_pond (Variable_info%id,           ! '(mm)'
!     :                              , g%pond)

      else if (Variable_info%id .eq. drain_ID) then
         call return_drain (Variable_info,           ! '(mm)'
     :                              , g%drain)

      else if (Variable_info%id .eq. infiltration_ID) then
         call return_infiltration (Variable_info,           ! '(mm)'
     :                             , g%infiltration)

!      else if (Variable_info%id .eq. eff_rain_ID) then
!         es = sum_real_array(g%es_layers, max_layer)
!         eff_rain = g%rain - g%runoff - g%drain
!         call return_eff_rain (Variable_info,           ! '(mm)'
!     :                             , eff_rain)

!      else if (Variable_info%id .eq. salb_ID) then
!         call return_salb (Variable_info,           ! '(mm)'
!     :                              , p%salb)

      elseif (Variable_info%id .eq. bd_ID) then
         num_layers = count_of_real_vals (p%dlayer, max_layer)
         call return_bd (Variable_info,           ! '(g/cc)'
     :                               , g%bd, num_layers)

      else if (Variable_info%id .eq. esw_ID) then

         num_layers = count_of_real_vals (p%dlayer, max_layer)
         esw = 0.0
         do 1000 layer = 1, num_layers
            esw = esw + l_bound (g%sw_dep(layer) - g%ll15_dep(layer)
     :                        , 0.0)
1000     continue
         call return_esw (Variable_info,           ! '(mm)'
     :                              , esw)

      else if (Variable_info%id .eq. sw_dep_ID) then

         num_layers = count_of_real_vals (p%dlayer, max_layer)
         call return_sw_dep (Variable_info,           ! '(mm)'
     :                               , g%sw_dep, num_layers)

      else if (Variable_info%id .eq. sw_ID) then

         num_layers = count_of_real_vals (p%dlayer, max_layer)
         do 2000 layer = 1, num_layers
            temp_array(layer) = divide (g%sw_dep(layer)
     :                                , p%dlayer(layer), 0.0)
2000     continue
         call return_sw (Variable_info,           ! '(mm/mm)'
     :                               , temp_array, num_layers)

      else if (Variable_info%id .eq. dlayer_ID) then
         num_layers =  count_of_real_vals (p%dlayer, max_layer)
         call return_dlayer (Variable_info,           ! '(mm)'
     :                               , p%dlayer, num_layers)

      else if (Variable_info%id .eq. ll15_dep_ID) then

         num_layers =  count_of_real_vals (p%dlayer, max_layer)
         call return_ll15_dep (Variable_info,           ! '(mm)'
     :                               , g%ll15_dep, num_layers)

      else if (Variable_info%id .eq. ll15_ID) then

         num_layers =  count_of_real_vals (p%dlayer, max_layer)
         do 3000 layer = 1, num_layers
            temp_array(layer) = divide (g%ll15_dep(layer)
     :                                , p%dlayer(layer), 0.0)
3000     continue
         call return_ll15 (Variable_info,           ! '(mm/mm)'
     :                               , temp_array, num_layers)

      else if (Variable_info%id .eq. dul_dep_ID) then

         num_layers =  count_of_real_vals (p%dlayer, max_layer)
         call return_dul_dep (Variable_info,           ! '(mm)'
     :                               , g%dul_dep, num_layers)

      else if (Variable_info%id .eq. dul_ID) then

         num_layers =  count_of_real_vals (p%dlayer, max_layer)
         do 4000 layer = 1, num_layers
            temp_array(layer) = divide (g%dul_dep(layer)
     :                                , p%dlayer(layer), 0.0)
4000     continue
         call return_dul (Variable_info,           ! '(mm/mm)'
     :                               , temp_array, num_layers)

      else if (Variable_info%id .eq. sat_dep_ID) then

         num_layers =  count_of_real_vals (p%dlayer, max_layer)
         call return_sat_dep (Variable_info,           ! '(mm)'
     :                               , g%sat_dep, num_layers)

      else if (Variable_info%id .eq. sat_ID) then

         num_layers =  count_of_real_vals (p%dlayer, max_layer)
         do 5000 layer = 1, num_layers
            temp_array(layer) = divide (g%sat_dep(layer)
     :                                , p%dlayer(layer), 0.0)
5000     continue
         call return_sat (Variable_info,           ! '(mm/mm)'
     :                               , temp_array, num_layers)

      else if (Variable_info%id .eq. air_dry_dep_ID) then

         num_layers =  count_of_real_vals (p%dlayer, max_layer)
         call return_air_dry_dep (Variable_info,           ! '(mm)'
     :                               , g%air_dry_dep, num_layers)

      else if (Variable_info%id .eq. air_dry_ID) then

         num_layers =  count_of_real_vals (p%dlayer, max_layer)
         do 6000 layer = 1, num_layers
            temp_array(layer) = divide (g%air_dry_dep(layer)
     :                                , p%dlayer(layer), 0.0)
6000     continue
         call return_air_dry (Variable_info,           ! '(mm/mm)'
     :                               , temp_array, num_layers)

      else if (Variable_info%id .eq. flux_ID) then

         num_layers =  count_of_real_vals (p%dlayer, max_layer)
         call return_flux (Variable_info,           ! '(mm)'
     :                               , g%flux, num_layers)

      else if (Variable_info%id .eq. flow_ID) then

         num_layers =  count_of_real_vals (p%dlayer, max_layer)
         call return_flow (Variable_info,           ! '(mm)'
     :                    , g%flow, num_layers)

      ! --- Resultant water and solute flow output variables ---
      else if (Variable_info%id .eq. flow_water_ID) then
         num_layers =  count_of_real_vals (p%dlayer, max_layer)
         do 6100 layer = 1, num_layers
            temp_array(layer) = g%flux (layer)
     :                        - g%flow (layer)
 6100    continue
         call return_flow_water (Variable_info,           ! '(mm)'
     :                          , temp_array, num_layers)

      else if (Variable_info%id .eq. water_table_ID) then
         call return_water_table (Variable_info,     ! '(mm)'
     :                           , g%water_table)

      else if (Variable_info%id .eq. sws_ID) then
         num_layers = count_of_real_vals (p%dlayer, max_layer)
         call return_sws (Variable_info,           ! '(mm/mm)'
     :                   , g%sws, num_layers)

      else if (soilwat2_solute_output(Variable_info%id)) then
         ! this ID matches one of our solute output IDs

      else
         ! not my variable

         call Message_unused ()
      endif

      call pop_routine (my_name)
      return
      end

!     ===========================================================
      logical function soilwat2_solute_output(Variable_info)
!     ===========================================================
      use Soilwat2Module
      implicit none

!+  Purpose
!      Perform all registrations and zeroing

!+  Sub-Program Arguments
      type(QueryData), intent(in) :: variable_info


*+  Local variables
      real flow_array(max_layers)
      integer layer
      integer num_layers
      integer solnum

!- Implementation Section ----------------------------------

      soilwat2_solute_output = .false.

      do solnum = 1, g%num_solutes
         if (Variable_info%ID .eq. g%soluteIDs(solnum)%flow) then
            flow_array(:) = 0.0
            num_layers = count_of_real_vals (p%dlayer, max_layer)
            flow_array(:num_layers) = g%solute_leach(solnum,:num_layers)
     :                              - g%solute_up(solnum,:num_layers)
            call return_Solute_N_Flow (Variable_info
     :                             , flow_array
     :                             , num_layers)
            soilwat2_solute_output = .true.
            exit

         elseif (Variable_info%ID .eq. g%soluteIDs(solnum)%leach) then
            call return_Solute_N_Leach (Variable_info
     :                             , g%solute_leach(solnum,:num_layers)
     :                             , num_layers)
            soilwat2_solute_output = .true.
            exit

         elseif (Variable_info%ID .eq. g%soluteIDs(solnum)%up) then
            call return_Solute_N_Up (Variable_info
     :                            , g%solute_up(solnum,:num_layers)
     :                            , num_layers)
            soilwat2_solute_output = .true.
            exit
         else
            ! solute not found yet
         endif
      end do

      return
      end


*     ===========================================================
      subroutine soilwat2_zero_variables ()
*     ===========================================================
      use Soilwat2Module
      use ComponentInterfaceModule

      implicit none

*+  Purpose
*       zero variables & arrays

*+  Mission Statement
*     Zero Variables

*+  Changes
*       191094 jngh specified and programmed
*       190595 jngh added bulk density
*       201099 dph  zeroed g%irrigation
*       240800 jngh moved rain, eadn, mint, maxt, day and year to separate s/r
*       250800 jngh removed g%num_solutes

*+  Constant Values
      character  my_name*(*)           ! module name
      parameter (my_name  = 'soilwat2_zero_variables')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

* ====================================================================
* Globals
!         g%cover_surface_extra = 0.0          ! extra surface cover (0-1)
!         g%cover_surface_runoff = 0.0         ! effective total cover (0-1)
!         g%cover_tot(:) = 0.0                 ! total canopy cover of crops (0-1)
!         g%cover_green(:) = 0.0               ! green canopy cover of crops (0-1)
!         g%canopy_height(:) = 0.0             ! canopy heights of each crop (mm)
!         g%num_crops = 0                      ! number of crops ()
         g%sumes1 = 0.0                       ! cumulative soil evaporation in stage 1 (mm)
         g%sumes2 = 0.0                       ! cumulative soil evaporation in stage 2 (mm)
         g%t = 0.0                            ! time after 2nd-stage soil evaporation
                                              ! begins (d)
         g%solute_min(:,:) = 0.0              ! minimum allowable solute
                                              ! in soil (kg/ha)
         g%solute (:, :) = 0.0                ! solute in each layer (kg/ha)
         g%dlt_solute (:, :) = 0.0            ! change in solute each in
                                              ! layer (kg n/ha)
         g%solute_leach (:,:) = 0.0           ! amount of solute leached
                                              ! from each layer (kg/ha)
         g%solute_up (:,:) = 0.0              ! amount of solute upped
                                              ! from each layer (kg/ha)
                                              ! nih - I dont like these
                                              ! names.
         g%irrigation_solute(:) = 0.0         ! amount of solute in
                                              ! irrigation water (kg/ha)
!        Zeroed in zero_module_links routine
!         g%num_solutes = 0                    ! number of solutes in
!                                              ! APSIM ()
         g%num_irrigation_solutes = 0         ! number of solutes traced
                                              ! in irrigation water

!         g%residue_cover = 0.0                ! residue cover reduces  cn2_bare
!         g%eo = 0.0                           ! potential evapotranspiration (mm)
         g%eos = 0.0                          ! pot sevap after modification for green cover &
                                              ! residue wt
!         g%cn2_new = 0.0                      ! New cn2  after modification for crop
                                              ! cover &
                                              ! residue cover
         g%air_dry_dep(:) = 0.0               ! air dry soil water content (mm
                                              ! water)
         g%bd(:) = 0.0                        ! moist bulk density of soil (g/cm^3)
         g%dul_dep (:) = 0.0                  ! drained upper limit soil water content
                                              ! for each soil layer (mm water)
         g%ll15_dep (:) = 0.0                 ! 15 bar lower limit of extractable
                                              ! soil water for each soil layer
                                              ! (mm water)
         g%sat_dep (:) = 0.0                  ! saturated water content for layer l
                                              ! (mm water)
         g%flow (:) = 0.0                     ! depth of water moving from layer l+1
                                              ! into layer l because of unsaturated
                                              ! flow; positive value indicates upward
                                              ! movement into layer l, negative value
                                              ! indicates downward movement (mm) out of
                                              ! layer l
         g%flux (:) = 0.0                     ! initially, water moving downward into
                                              ! layer l (mm), then water moving downward
                                              ! out of layer l (mm)
         g%sw_dep (:) = 0.0                   ! soil water content of layer l (mm)
         g%es_layers(:) = 0.0                 ! actual soil evaporation (mm)

         g%drain = 0.0                        ! drainage rate from bottom layer (cm/d)
         g%infiltration = 0.0                 ! infiltration (mm)
!         g%runoff = 0.0                       ! runoff (mm)
!         g%irrigation = 0.0                   ! irrigation (mm)
!         g%obsrunoff = 0.0                    ! observed runoff (mm)
!         g%tillage_cn_red = 0.0               ! reduction in CN due to tillage ()
!         g%tillage_cn_rain = 0.0              ! cumulative rainfall below which
                                              ! tillage reduces CN (mm)
!         g%tillage_rain_sum = 0.0             ! cumulative rainfall for
                                              ! tillage CN reduction (mm)
!         g%obsrunoff_found = .false.          ! whether obserevd runoff was returned from system
!         g%obsrunoff_name = ' '               ! system name of observed runoff
         g%numvals_profile_esw_depth = 0      ! number of values returned for profile_esw_depth
         g%numvals_insoil = 0                 ! number of values returned for insoil
         g%numvals_wet_soil_depth = 0         ! number of values returned for wet_soil_depth
         g%numvals_profile_fesw = 0           ! number of values returned for profile_fesw
         g%numvals_sw = 0                     ! number of values returned for sw

c        Zeroed in zero_module_links routine
c         g%solute_names(:) = ' '              ! names of solutes in the
c                                              ! soil system that will
c                                              ! be leached by soilwat2
c         g%solute_owners(:) = ' '             ! names of owner module for each
c                                              ! solutes in the system
c         g%solute_mobility (:) = ' '
c
c         g%crop_module(:) = ' '               ! list of modules
                                              ! replying
!         g%num_canopy_fact = 0                ! number of canopy factors read ()
         g%inf_pool = 0.0                     ! infiltration pool to be evap at reset sumes
         g%sumes_last = 0.0                   ! sumes before inf reset
         g%sumes = 0.0                        ! summed es
         g%sumes_yest = 0.0                   ! yesterdays sumes
         g%sumeos = 0.0                       ! summed eos
         g%sumeos_last = 0.0                  ! sumeos before inf reset
!         g%eo_system = 0.0                    ! eo from somewhere else in the system
!         g%eo_source = ' '                    ! system variable name of external eo source

         g%infiltration_pot  =  0.0                       ! surface ponding depth (mm)
         g%water_table = 0.0                  ! water table depth (mm)
         g%sws (:) = 0.0                      ! soil water (mm/layer)
         g%Crop(:)%RootLayer(:)%Uptake= 0.0            ! Water uptake by crops over soil profile
         g%Crop(:)%demand = 0.0                    !
         g%Crop(:)%RootLayer(:)%uptakePot = 0.0
         g%Crop(:)%RootLayer(:)%rld = 0.0
         g%Crop(:)%numLayers  = 0
         g%Crop(:)%name    = ' '

* ====================================================================
* Parameters
         p%irrigation_layer = 0                  ! trickle irrigation input layer
         p%dlayer (:) = 0.0                   ! thickness of soil layer i (mm)
         p%swcon (:) = 0.0                    ! soil water conductivity constant (1/d)
                                              ! ie day**-1 for each soil layer
!         p%cn2_bare = 0.0                     ! curve number input used to calculate
!                                              ! daily g_runoff
!         p%cn_cov = 0.0                       ! cover at which c_cn_red occurs
!         p%cn_red = 0.0                       ! maximum reduction in p_cn2_bare due to cover
         p%cona = 0.0                         ! stage 2 drying coefficient
         p%diffus_const = 0.0                 ! diffusivity constant for soil testure
         p%diffus_slope = 0.0                 ! slope for diffusivity/soil water content
                                              ! relationship
!         p%salb = 0.0                         ! bare soil albedo (unitless)
         p%u = 0.0                            ! upper limit of stage 1 soil evaporation
                                              ! (mm)
         p%insoil = 0.0                       ! switch describing initial soil water distributed evenly
         p%profile_esw_depth = 0.0            ! initial depth of esw in profile filled top down with soil water (mm)
         p%wet_soil_depth = 0.0                ! initial depth profile filled top down with soil water (mm)
         p%profile_fesw = 0.0                 ! initial fraction of profile esw filled top down with soil water (mm)
         p%max_evap = 0.0                     ! maximum daily evaporation for rickert
         p%beta = 0.0                         ! beta for b&s model

!         p%max_pond = 0.0                     ! maximum allowable surface storage (ponding) mm
         p%mwcon (:) = 0.0                   ! layer permeability factor (zero or one)

* ====================================================================
* Constants
!         c%hydrol_effective_depth = 0.0       ! hydrologically effective depth for
!                                              ! runoff (mm)
         c%mobile_solutes(:) = ' '            ! names of all possible
                                              ! mobile solutes
         c%immobile_solutes(:) = ' '          ! names of all possible
                                              ! immobile solutes
!        c%min_crit_temp = 0.0                ! temperature below which eeq decreases (oC)
!         c%max_crit_temp = 0.0                ! temperature above which eeq increases (oC)
!         c%max_albedo = 0.0                   ! maximum bare ground soil albedo (0-1)
         c%A_to_evap_fact = 0.0               ! factor to convert "A" to coefficient
                                              ! in Adam's type residue effect on Eos
!         c%canopy_eos_coef = 0.0              ! coef in cover Eos reduction eqn
         c%sw_top_crit = 0.0                  ! critical sw ratio in top layer
                                              ! below which stage 2 evaporation occurs
         c%sumes1_max = 0.0                   ! upper limit of sumes1
         c%sumes2_max = 0.0                   ! upper limit of sumes2
         c%Solute_flux_eff = 0.0              ! efficiency of moving solute with flux (0-1)
         c%Solute_flow_eff = 0.0              ! efficiency of moving solute with flow (0-1)
         c%gravity_gradient = 0.0             ! gradient due to hydraulic differentials
                                              ! (0-1)
         c%specific_bd = 0.0                  ! specific bulk density (g/cc)
!         c%canopy_fact(:) = 0.0               ! canopy factors for cover runoff effect ()
!         c%canopy_fact_height(:) = 0.0        ! heights for canopy factors (mm)
!         c%canopy_fact_default = 0.0          ! default canopy factor in absence of height ()
         c%evap_method = 0                    ! actual soil evaporation model being used




      call pop_routine (my_name)
      return
      end

*     ===========================================================
      subroutine soilwat2_zero_data_links ()
*     ===========================================================
      use Soilwat2Module
      use ComponentInterfaceModule

      implicit none

*+  Purpose
*     Zero information describing data links with other modules

*+  Mission Statement
*     Zero information describing data links with other modules

*+  Changes
*       090999 nih specified and programmed

*+  Constant Values
      character  my_name*(*)           ! module name
      parameter (my_name  = 'soilwat2_zero_data_links')

*+  Local Variables
       integer solnum
       type(SoluteID)::empty

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call fill_char_array (g%solute_names, ' ', max_solute)
!      call fill_char_array (g%solute_owners, ' ', max_solute)
      call fill_logical_array (g%solute_mobility, .false., max_solute)

      g%Crop(:)%RootLayer(:)%Uptake = 0.0
      g%Crop(:)%demand = 0.0
      g%Crop(:)%RootLayer(:)%uptakePot = 0.0
      g%Crop(:)%RootLayer(:)%rld = 0.0
      g%Crop(:)%numLayers  = 0
      g%num_crops = 0
      g%Crop(:)%name    = ' '

      empty%get = 0
      empty%set = 0
      empty%flow = 0
      empty%leach = 0
      empty%up = 0

      g%num_solutes = 0
      p%solute_names(:) = ' '
      g%soluteIDs(:) = empty

!      g%num_crop_modules = 0
!      g%crop_module_ids(:) = 0
!      g%CropSupplyID(:) = 0

      call pop_routine (my_name)
      return
      end

*     ===========================================================
      subroutine soilwat2_zero_event_data ()
*     ===========================================================
      use Soilwat2Module
      use ComponentInterfaceModule

      implicit none

*+  Purpose
*     Zero information describing event data from other modules

*+  Mission Statement
*     Zero information describing event data from other modules

*+  Changes
*       240800 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! module name
      parameter (my_name  = 'soilwat2_zero_event_data')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

!         g%year = 0                           ! year
!         g%day  = 0                           ! day of year
!         g%rain = 0.0                         ! precipitation (mm/d)
!         g%radn = 0.0                         ! solar radiation (mj/m^2/day)
!         g%mint = 0.0                         ! minimum air temperature (oC)
!         g%maxt = 0.0                         ! maximum air temperature (oC)

      call pop_routine (my_name)
      return
      end


*     ===========================================================
      subroutine soilwat2_zero_daily_variables ()
*     ===========================================================
      use Soilwat2Module
      use ComponentInterfaceModule

      implicit none

*+  Purpose
*       zero variables & arrays

*+  Mission Statement
*     Zero Daily Variables

*+  Changes
*       191094 jngh specified and programmed
*       170895 nih  added initialisation of solute information
*       130896 jngh removed g%total_cover
*                   removed g%cover_green_sum
*                   removed g%cover_tot_sum
*                   added g%cover_tot and g%cover_green and g%crop_module
*                   added g%num_crops
*     011199 jngh removed residue_wt
*     170101 dph  removed the loop that zeroed solute arrays - replaced with simple assignment

*+  Constant Values
      character  my_name*(*)           ! module name
      parameter (my_name  = 'soilwat2_zero_daily_variables')

*+  Local Variables
      integer layer                    ! soil layer number counter
      integer solnum                   ! solute number counter

*- Implementation Section ----------------------------------

      call push_routine (my_name)

          !  zero pools etc.

      call fill_real_array (g%flow, 0.0, max_layer)
      call fill_real_array (g%flux, 0.0, max_layer)
      call fill_real_array (g%es_layers, 0.0, max_layer)
!      call fill_real_array (g%cover_tot, 0.0, max_crops)
!      call fill_real_array (g%cover_green, 0.0, max_crops)
!      call fill_char_array (g%crop_module, ' ', max_crops)
!      call fill_real_array (g%canopy_height, 0.0, max_crops)

!      g%residue_cover      = 0.0
!      g%eo                 = 0.0
      g%eos                = 0.0
!      g%cn2_new            = 0.0
      g%drain              = 0.0
      g%infiltration       = 0.0
!      g%runoff             = 0.0
      g%runoff_pot         = 0.0
!      g%num_crops          = 0
!      g%obsrunoff          = 0.0
!      g%obsrunoff_found    = .false.

      ! initialise all solute information

      g%solute (:, :) = 0.0
      g%solute_min (:, :) = 0.0
      g%solute_leach(:, :) = 0.0
      g%solute_up (:, :) = 0.0
      g%dlt_solute (:, :) = 0.0

      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine soilwat2_solute_flux (solute_out
     :                                , solute_kg
     :                                , solute_min)
*     ===========================================================
      use Soilwat2Module
      use ComponentInterfaceModule

      implicit none

*+  Sub-Program Arguments
      real       solute_out(*)         ! (output) solute leaching out of
                                       !    each layer (kg/ha)
      real       solute_min(*)         ! (input) minimum solute allowed
                                       !     (kg/ha)
      real       solute_kg(*)          ! (input) solute in each layer
                                       !    (kg/ha)

*+  Purpose
*         calculate the downward movement of solute with percolating water

*+  Mission Statement
*     Calculate the Solute Movement with Saturated Water Flux

*+  Changes
*        210191   specified and programmed jngh (j hargreaves)
*        031091   jngh declared count_of_real_vals, max_layer.con, swater.blk &
*                      sprofl.blk - cr162
*                      added comma in argument declaration of nut_min - cr163
*                      declared num_layers - cr164
*        251091   fixed upper limit of out_n becoming -ve.  jngh - cr221
*        290892   jngh changed soil water to depth of water
*        151292   jngh changed common blocks
*        170895   nih  renamed to generic solute name
*        200896   jngh renamed n, nut and nutrient references to solute

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_solute_flux')

*+  Local Variables
      real       in_solute             ! solute leaching into layer from
                                       !    above (kg/ha)
      integer    layer                 ! layer counter
      integer    num_layers            ! number of layers in profile
      real       out_max               ! max. solute allowed to leach out of
                                       !    layer (kg/ha)
      real       out_solute            ! solute leaching out of layer
                                       !    (kg/ha)
      real       out_w                 ! water draining out of layer (mm)
      real       solute_kg_layer       ! quantity of solute in layer (kg/ha)
      real       water                 ! quantity of water in layer (mm)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

            ! flux section - drainage out, down to next layer

      call fill_real_array (solute_out, 0.0, max_layer)
      num_layers = count_of_real_vals (p%dlayer, max_layer)
      in_solute = 0.0

      do 1000 layer = 1,num_layers

             ! get water draining out of layer and n content of layer
             ! includes that leaching down

         out_w = g%flux(layer)
         solute_kg_layer = solute_kg(layer) + in_solute

             ! n leaching out of layer is proportional to the water draining
             ! out.
* ?????????????? 21 mar 91 - jngh. should the water draining into this
* ?????????????? layer be removed also?

         water = g%sw_dep(layer) + out_w
         out_solute = solute_kg_layer
     :         * divide (out_w, water, 0.0)
     :         * c%Solute_flux_eff

             ! don't allow the n to be reduced below a minimum level

         out_max = l_bound (solute_kg_layer - solute_min(layer), 0.0)
         out_solute = bound (out_solute, 0.0, out_max)

             ! keep the leaching and set the input for the next layer

         solute_out(layer) = out_solute
         in_solute = out_solute

 1000 continue

      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine soilwat2_solute_flow (solute_up, solute_kg, solute_min)
*     ===========================================================
      use Soilwat2Module
      use ComponentInterfaceModule

      implicit none

*+  Sub-Program Arguments
      real       solute_up (*)         ! (output) solute moving upwards
                                       !    into each layer (kg/ha)
      real       solute_kg (*)         ! (input/output) solute in each
                                       !    layer (kg/ha)
      real       solute_min(*)         ! (input) minimum solute allowed
                                       !     (kg/ha)

*+  Purpose
*       movement of solute in response to differences in
*       water content of adjacent soil layers when the soil water
*       content is < the drained upper limit (unsaturated flow)

*+  Notes
*       170895 nih The variable names and comments need to be cleaned
*                  up.  When this is done some references to no3 or
*                  nitrogen need to be changed to 'solute'

*+  Mission Statement
*     Calculate the Solute Movement with Unsaturated Water Flow

*+  Changes
*       051191 jngh previously programmed and now changed
*       251191 jngh corrected n flow into top layer - cr218
*       251091 jngh changed count_of_real_vals from real to integer - cr215
*                   added comment re 0.5 - cr216
*                   corrected downward flow - cr219
*       290892 jngh changed soil water to depth of water
*       151292 jngh changed common blocks
*       250893 jngh corrected adjustment of soil water for previous movement
*       170895 nih  renamed to generic solute name
*       200896 jngh renamed n, nut and nutrient references to solute
*       151200 jngh added round_to_zero

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_solute_flow')

*+  Local Variables
      real       bottomw               ! water movement to/from next layer
                                       ! (kg/ha)
      real       in_solute                  ! solute moving into layer from
                                       !    above (kg/ha)
      integer    layer                 ! layer counter
      real       solute_down (max_layer) ! solute moving downwards out of
                                       !    each layer (kg/ha)
      integer    num_layers            ! number of layers
      real       out_solute            ! solute moving out of layer (kg/ha)
      real       out_w                 ! water draining out of layer (mm)
      real       remain (max_layer)    ! n remaining in each layer between
                                       !    movement up (kg/ha)
      real       solute_kg_layer       ! quantity of solute in layer (kg/ha)
      real       top_w                 ! water movement to/from above layer
                                       ! (kg/ha)
      real       water                 ! quantity of water in layer (mm)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call fill_real_array (solute_up, 0.0, max_layer)

            ! flow  up from lower layer:  + up, - down

            ! + ve flow : upward movement. go from bottom to top layer

      num_layers = count_of_real_vals (p%dlayer, max_layer)
      in_solute = 0.0
      do 1000 layer = num_layers,2,-1

             ! keep the nflow upwards

         solute_up(layer) = in_solute

             ! get water moving up and out of layer to the one above

         out_w = g%flow(layer-1)
         if (out_w .le. 0.0) then
            out_solute = 0.0
         else
                ! get water movement between this and next layer

            bottomw = g%flow(layer)

                ! get n content of layer includes that moving from other layer

            solute_kg_layer = solute_kg(layer) + in_solute
            water = g%sw_dep(layer) + out_w - bottomw

                ! n moving out of layer is proportional to the water moving
                ! out.

                ! jngh 19-3-91 i think the *0.5 should be removed
                ! jngh 25-10-91 john dimes thinks the 0.5 is to allow
                ! for losses through diffusion. pjr called it a diffusion
                ! coefficient.  it seems that the water movement is incorrect
                ! and this compensates for it.

cjh            out_solute = solute_kg_layer*divide (out_w, water, 0.0) *0.5
            out_solute = solute_kg_layer
     :                 * divide (out_w, water, 0.0)
     :                 * c%Solute_flow_eff

                ! don't allow the n to be reduced below a minimum level

            out_solute = bound (out_solute
     :                         , 0.0
     :                         , solute_kg_layer - solute_min(layer))

         endif

             ! set the input for the next layer

         in_solute = out_solute
1000  continue

      solute_up (1) = in_solute

          ! now get n remaining in each layer between movements

          ! this is needed to adjust the n in each layer before calculating
          ! downwards movement.  i think we shouldn't do this within a time
          ! step. i.e. there should be no movement within a time step. jngh

      remain(1) = solute_up(1)
      do 1010 layer = 2, num_layers
         remain(layer) = solute_up(layer) - solute_up(layer - 1)
1010  continue

           ! -ve flow - downward movement

      call fill_real_array (solute_down, 0.0, max_layer)
      in_solute = 0.0
      top_w = 0.0

      do 1100 layer = 1,num_layers

             ! get water moving out of layer

         out_w = - g%flow(layer)
         if (out_w.le.0.0) then
            out_solute = 0.0
         else

                ! get n content of layer includes that moving from other layer

            solute_kg_layer = solute_kg(layer)
     :                      + in_solute
     :                      + remain(layer)
            water = g%sw_dep(layer) + out_w - top_w

                ! n moving out of layer is proportional to the water moving
                ! out.
                ! jngh 19-3-91 i think the *0.5 should be removed.
                ! 25-10-91 see note in up movement about this.

            out_solute = solute_kg_layer
     :            * divide (out_w, water, 0.0)
     :            * c%Solute_flow_eff

                ! don't allow the n to be reduced below a minimum level

            out_solute = round_to_zero (out_solute)
            out_solute = bound (out_solute
     :                         , 0.0
     :                         , solute_kg_layer - solute_min(layer))

         endif
         solute_down(layer) = out_solute
         in_solute = out_solute
         top_w = out_w
1100  continue

      do 1200 layer = 1, num_layers
         solute_up(layer) =  solute_up(layer) - solute_down(layer)
1200  continue

      call pop_routine (my_name)
      return
      end




* ====================================================================
      subroutine soilwat2_sum_report ()
* ====================================================================
      use Soilwat2Module
      use ComponentInterfaceModule

      implicit none

*+  Purpose
*     <insert here>

*+  Mission Statement
*      Report SoilWat module summary details

*+  Changes
*   NeilH - 19-10-1994 - Programmed and Specified
*       190595 jngh added bulk density
*       300695 jngh changed format for insoil from i8 to f8.2
*       190897 nih  renamed from soilwat2_init_report
*       260897 nih  Added extra information to summary report

*+  Constant Values
      character  my_name*(*)           ! name of current procedure
      parameter (my_name = 'soilwat2_sum_report')

*+  Local Variables
      real       depth_layer_top       ! depth to top of layer (mm)
      real       depth_layer_bottom    ! depth to bottom of layer (mm)
      integer    layer                 ! layer number
      integer    num_layers            ! number of soil profile layers
      character  line*100              ! temp output record
      real       runoff_wf(max_layer)  ! weighting factor for runoff
      real       usw(max_layer)        ! unavail. sw (mm)
      real       asw(max_layer)        ! avail. sw (mm)
      real       masw(max_layer)       ! max unavail. sw (mm)
      real       dsw(max_layer)        ! drainable sw (mm)

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call write_string (new_line//new_line)

      line = '                 Soil Profile Properties'
      call write_string (line)

      line =
     :'   -----------------------------------------------------------'
     ://'----------'
      call write_string (line)

      line =
     :'         Depth  Air_Dry  LL15   Dul    Sat     Sw     BD   '
     ://'Runoff  SWCON'
      call write_string (line)

      line =
     :'           mm     mm/mm  mm/mm  mm/mm  mm/mm  mm/mm  g/cc    wf'
      call write_string (line)

      line =
     :'   -----------------------------------------------------------'
     ://'----------'
      call write_string (line)

      num_layers = count_of_real_vals (p%dlayer, max_layer)
      depth_layer_top = 0.0
      call soilwat2_runoff_depth_factor (runoff_wf)

      do 1000 layer = 1,num_layers
         depth_layer_bottom = depth_layer_top + p%dlayer(layer)

         write (line,'(3x, f6.0, a, f6.0, 8f7.3)')
     :            depth_layer_top, '-', depth_layer_bottom
     :          , divide (g%air_dry_dep(layer)
     :                  , p%dlayer(layer), 0.0)
     :          , divide (g%ll15_dep(layer)
     :                  , p%dlayer(layer), 0.0)
     :          , divide (g%dul_dep(layer), p%dlayer(layer), 0.0)
     :          , divide (g%sat_dep(layer), p%dlayer(layer), 0.0)
     :          , divide (g%sw_dep(layer), p%dlayer(layer), 0.0)
     :          , g%bd(layer)
     :          , runoff_wf(layer)
     :          , p%swcon(layer)

         call write_string (line)
         depth_layer_top = depth_layer_bottom
1000  continue

      line =
     :'   -----------------------------------------------------------'
     ://'----------'
      call write_string (line)

      call write_string (new_line//new_line)

      line = '             Soil Water Holding Capacity'
      call write_string (line)

      line =
     :'     ---------------------------------------------------------'

      call write_string (line)

      line =
     :'         Depth    Unavailable Available  Max Avail.  Drainable'
      call write_string (line)
      line =
     :'                     (LL15)   (SW-LL15)  (DUL-LL15)  (SAT-DUL)'
      call write_string (line)

      line =
     :'                       mm        mm          mm         mm'
      call write_string (line)

      line =
     :'     ---------------------------------------------------------'
      call write_string (line)

      num_layers = count_of_real_vals (p%dlayer, max_layer)
      depth_layer_top = 0.0

      do 2000 layer = 1,num_layers
         depth_layer_bottom = depth_layer_top + p%dlayer(layer)
         usw(layer) = g%ll15_dep(layer)
         asw(layer) = l_bound(g%sw_dep(layer)-g%ll15_dep(layer),0.0)
         masw(layer) = g%dul_dep(layer) - g%ll15_dep(layer)
         dsw(layer) = g%sat_dep(layer) - g%dul_dep(layer)

         write (line,'(3x, f6.0, a, f6.0, 4f11.2)')
     :            depth_layer_top, '-', depth_layer_bottom
     :           ,usw(layer)
     :           ,asw(layer)
     :           ,masw(layer)
     :           ,dsw(layer)

         call write_string (line)
         depth_layer_top = depth_layer_bottom
2000  continue

      line =
     :'     ---------------------------------------------------------'
      call write_string (line)

      write (line,'(10x,''Totals'', 4f11.2)')
     :               sum_real_array (usw,  num_layers)
     :             , sum_real_array (asw,  num_layers)
     :             , sum_real_array (masw, num_layers)
     :             , sum_real_array (dsw,  num_layers)


      call write_string (line)

      line =
     :'     ---------------------------------------------------------'
      call write_string (line)

             ! echo sw parameters

      call write_string (new_line//new_line)
      call write_string (new_line//new_line)

      line = '             Initial Soil Parameters'
      call write_string (line)

      line =
     :  '     ---------------------------------------------------------'
      call write_string (line)

      line =
     : '            Insoil     Dif_Con   Dif_Slope'
      call write_string (line)

      line =
     :  '     ---------------------------------------------------------'
      call write_string (line)

      write (line, '(6x, 4f12.2)')
     :               p%insoil
!     :             , p%salb
     :             , p%diffus_const
     :             , p%diffus_slope
      call write_string (line)

      line =
     :  '     ---------------------------------------------------------'
      call write_string (line)
      call write_string (new_line//new_line)

!      if (g%obsrunoff_name .ne. blank) then
!         write (line, '(6x,a,a,a)')
!     :          '             Observed runoff data ( ',
!     :          g%obsrunoff_name(1:lastNB(g%obsrunoff_name)),
!     :          ' ) is used in water balance'
!
!         call write_string (line)
!
!      else
!            ! no observed data
!         call write_string (
!     :  '             Runoff is predicted using scs curve number:')
!         line =
!     : '           Cn2  Cn_Red  Cn_Cov   H_Eff_Depth '
!         call write_string (line)
!
!         line =
!     : '                                      mm     '
!         call write_string (line)
!
!         line =
!     :  '     ---------------------------------------------------------'
!         call write_string (line)
!
!         write (line, '(6x, 4f8.2)')
!     :       p%cn2_bare, p%cn_red, p%cn_cov,
!     :       c%hydrol_effective_depth
!         call write_string (line)
!
!         line =
!     :  '     ---------------------------------------------------------'
!         call write_string (line)
!      endif

      call write_string (new_line//new_line)

      if (c%evap_method .eq. ritchie_method) then
         line = '      Using Ritchie evaporation model'
         call write_string (line)

         write (line, '(7x, a, f8.2, a)') 'Cuml evap (U):        ',
     :        p%u, ' (mm^0.5)'
         call write_string (line)

         write (line, '(7x, a, f8.2, a)') 'CONA:                 ',
     :        p%cona, ' ()'
         call write_string (line)

      else if (c%evap_method .eq. bs_a_method) then
         line = '      Using B&S option A evaporation method'
         call write_string (line)

         write (line, '(7x, a, f8.2, a)') 'Beta:                 ',
     :        p%beta, ' (mm^0.5)'
         call write_string (line)

      else if (c%evap_method .eq. bs_b_method) then
         line = '      Using B&S option B evaporation method'
         call write_string (line)

         write (line, '(7x, a, f8.2, a)') 'Beta:                 ',
     :        p%beta, ' (mm^0.5)'
         call write_string (line)

      else if (c%evap_method .eq. bs_acs_method) then
         line = '      Using B&S option B method with acs/jd mods'
         call write_string (line)

         write (line, '(7x, a, f8.2, a)') 'Cuml evap (U):        ',
     :        p%u, ' (mm)'
         call write_string (line)

         write (line, '(7x, a, f8.2, a)') 'CONA:                 ',
     :        p%cona, ' ()'
         call write_string (line)

         write (line, '(7x, a, f8.2, a)') 'Beta:                 ',
     :        p%beta, ' (mm^0.5)'
         call write_string (line)

      else if (c%evap_method .eq. rickert_method) then
         line = '      Using Rickert evaporation method'
         call write_string (line)

         write (line, '(7x, a, f8.2, a)') 'Max daily evaporation:',
     :        p%max_evap, ' (mm)'
         call write_string (line)

      else
         line = '     Using unknown evaporation method!'
         call write_string (line)

      endif

!      if (g%eo_source .ne. blank) then
!         write (line, '(6x, a, a)') 'Eo source:             ',
!     :        g%eo_source
!         call write_string (line)
!      else
!         write (line, '(6x, a)') 'Eo from priestly-taylor'
!         call write_string (line)
!      endif

      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine soilwat2_init ()
*     ===========================================================
      use Soilwat2Module
      use ComponentInterfaceModule

      implicit none

*+  Purpose
*       input initial values from soil water parameter files.

*+  Mission Statement
*       Initialise SoilWat module

*+  Changes
*        210191   specified and programmed jngh (j hargreaves
*        290591   jngh corrected external call list - cr91
*                      removed sprpty.blk & winit.blk - cr92
*        290892   jngh changed soil water to depth of water
*        051093   jngh added fatal error call.
*                      changed l to layer.
*        190194   jpd  add air_dry_tot for output
*        25/7/96  dph  added code to report to summary file when p%insoil < 1
*        190897   nih  renamed from soilwat2_init and
*                      adapted as part of ACTION_reset development
*        090299   jngh changed name from reset to init
*                       removed calls to zero variables and get other variables

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name  = 'soilwat2_init')

*- Implementation Section ----------------------------------

      call push_routine (my_name)
            ! zero pools

          ! Get all coefficients from file

      call soilwat2_read_constants ()

      call soilwat2_soil_property_param ()
      call soilwat2_soil_profile_param ()

      call soilwat2_evap_init ()

      call soilwat2_Publish_soil_water_profile ()

      call pop_routine (my_name)
      return
      end



* ====================================================================
       subroutine soilwat2_irrig_solute ()
* ====================================================================
      use Soilwat2Module
      use ComponentInterfaceModule

      implicit none

*+  Purpose
*     <insert here>

*+  Mission Statement
*      Add solutes with irrigation

*+  Changes
*   neilh - 04-09-1995 - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'soilwat2_irrig_solute')

*+  Local Variables
      integer    solnum                ! solute number counter variable
      integer    layer                 ! soil layer

*- Implementation Section ----------------------------------
      call push_routine (myname)

      if(p%irrigation_layer.eq.0) then
         !addition at surface
      else
        layer = p%irrigation_layer
         do solnum = 1, g%num_solutes
            g%solute(solnum,layer)    = g%solute(solnum,layer)
     :                                + g%irrigation_solute(solnum)
            g%dlt_solute(solnum,layer) = g%dlt_solute(solnum,layer)
     :                                 + g%irrigation_solute(solnum)

         end do
      endif


      call pop_routine (myname)
      return
      end



* ====================================================================
       subroutine soilwat2_move_solute_down ()
* ====================================================================
      use Soilwat2Module
      use ComponentInterfaceModule

      implicit none

*+  Purpose
*     <insert here>

*+  Mission Statement
*      Calculate downward movement of solutes

*+  Changes
*   neilh - 04-09-1995 - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'soilwat2_move_solute_down')

*+  Local Variables
      integer    num_layers
      integer    layer                 ! layer number counter variable
      integer    solnum                ! solute number counter variable
      real       leach (max_layer)     ! amount of a solute leached from
                                       ! each soil layer (kg/ha)
      real       temp_solute(max_layer)! temp array for solute content(kg/ha)
      real       temp_solute_min (max_layer)! temp array for minimum solute
                                       ! content (kg/ha)
      real       temp_dlt_solute(max_layer) ! temp array of changes in
                                       ! solute concentration (kg/ha)

*- Implementation Section ----------------------------------
      call push_routine (myname)

      ! Now for each mobile solute put the global solute info into a
      ! temp solute array, pass this solute information to the solute
      ! flux routine then insert moved solute back into the global
      ! record.

      num_layers = count_of_real_vals (p%dlayer, max_layer)

      do 1300 solnum = 1, g%num_solutes
         if (g%solute_mobility(solnum)) then

            do 1100 layer = 1, num_layers
               temp_solute(layer) = g%solute(solnum, layer)
               leach(layer) = 0.0
               temp_solute_min(layer) = g%solute_min(solnum,layer)
               temp_dlt_solute(layer) = g%dlt_solute(solnum,layer)
 1100       continue

            call soilwat2_solute_flux (leach
     :                                 , temp_solute
     :                                 , temp_solute_min)
            call move_down_real (leach, temp_solute, num_layers)
            call move_down_real (leach, temp_dlt_solute, num_layers)

            do 1200 layer = 1, num_layers
               g%solute (solnum, layer) = temp_solute (layer)
               g%solute_leach (solnum, layer) = leach (layer)
               g%dlt_solute (solnum, layer) = temp_dlt_solute (layer)
 1200       continue

         else
            ! solute was not in the mobile list - do not move it
         endif

 1300 continue

      call pop_routine (myname)
      return
      end



* ====================================================================
       subroutine soilwat2_move_solute_up ()
* ====================================================================
      use Soilwat2Module
      use ComponentInterfaceModule

      implicit none

*+  Purpose
*     <insert here>

*+  Mission Statement
*      Calculate upward movement of solutes

*+  Changes
*   neilh - 04-09-1995 - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'soilwat2_move_solute_up')

*+  Local Variables
      integer    layer                 ! layer number counter variable
      real       leach (max_layer)     ! amount of a solute leached from
                                       ! each soil layer (kg/ha)
      integer    num_layers            ! number of layers
      integer    solnum                ! solute number counter variable
      real       temp_solute(max_layer)! temp array for solute content(kg/ha)
      real       temp_solute_min (max_layer)! temp array for minimum solute
                                       ! content (kg/ha)
      real       temp_dlt_solute(max_layer) ! temp array of changes in
                                       ! solute concentration (kg/ha)

*- Implementation Section ----------------------------------
      call push_routine (myname)

      ! Now for each mobile solute put the global solute info into a
      ! temp solute array, pass this solute information to the solute
      ! flux routine then insert moved solute back into the global
      ! record.

      num_layers = count_of_real_vals (p%dlayer, max_layer)

      do 2300 solnum = 1, g%num_solutes

         if (g%solute_mobility(solnum)) then

            do 2100 layer = 1, max_layer
               temp_solute(layer) = g%solute(solnum, layer)
               leach(layer) = 0.0
               temp_solute_min(layer) = g%solute_min(solnum,layer)
               temp_dlt_solute(layer) = g%dlt_solute(solnum,layer)
 2100       continue

            call soilwat2_solute_flow (leach
     :                                , temp_solute
     :                                , temp_solute_min)
            call move_up_real (leach, temp_solute, num_layers)
            call move_up_real (leach, temp_dlt_solute, num_layers)

            do 2200 layer = 1, max_layer
               g%solute (solnum, layer) = temp_solute (layer)
               g%solute_up (solnum, layer) = leach (layer)
               g%dlt_solute (solnum, layer) = temp_dlt_solute (layer)
 2200       continue
         else
            ! solute was not in the mobile list - do not move it
         endif

 2300 continue

      call pop_routine (myname)
      return
      end





* ====================================================================
       real function soilwat_water_table ()
* ====================================================================
      use Soilwat2Module
      use ComponentInterfaceModule

      implicit none

*+  Purpose
*     Calculate the water table

*+  Mission statement
*     Calculate the water table

*+  Changes
*   neilh - 28-03-1996 - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'soilwat_water_table')

*+  Local Variables
      integer layer
      integer num_layers
      integer sat_layer

*- Implementation Section ----------------------------------
      call push_routine (myname)

      num_layers = count_of_real_vals (p%dlayer, max_layer)

      do 100 layer = 1, num_layers
         if (reals_are_equal(g%sw_dep(layer),g%sat_dep(layer))) then
            sat_layer = layer
            goto 200
         else
            sat_layer = 0
         endif
  100 continue
  200 continue
      if (sat_layer.eq.0) then
         soilwat_water_table = 10000.

      elseif (sat_layer.eq.1) then
         soilwat_water_table = 0.0
      else
         if (g%sw_dep(sat_layer-1).gt.g%dul_dep(sat_layer-1)) then
            soilwat_water_table = sum_real_array(p%dlayer,sat_layer-1)
     :         -  divide (g%sw_dep(sat_layer-1)-g%dul_dep(sat_layer-1)
     :                   ,g%sat_Dep(sat_layer-1)-g%dul_dep(sat_layer-1)
     :                   ,0.0) * p%dlayer(sat_layer-1)
         else
            soilwat_water_table = sum_real_array(p%dlayer,sat_layer-1)
         endif
      endif

      call pop_routine (myname)
      return
      end

*     ===========================================================
      subroutine soilwat2_Do_Crop_Water_Uptake ()
*     ===========================================================
      use Soilwat2Module
      use ComponentInterfaceModule

      implicit none

*+  Purpose
*     Update internal time record and reset daily state variables.

*+  Mission Statement
*     Update internal time record and reset daily state variables.

*+  Changes
*        260899 nih

*+  Local Variables
      integer   num_layers
      integer   crop_no

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'soilwat2_Do_Crop_Water_Uptake')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      do 1000 crop_no = 1, g%num_crops
         num_layers = g%Crop(crop_no)%numLayers
         call soilwat2_sw_uptake(
     :          num_layers
     :        , g%Crop(crop_no)%demand
     :        , g%Crop(crop_no)%RootLayer(1:num_layers)%uptakePot
     :        , g%Crop(crop_no)%RootLayer(1:num_layers)%Uptake)

      call pop_routine (myname)
      return
      end

*     ===========================================================
      subroutine soilwat2_sw_uptake( num_layer
     :                              , sw_demand
     :                              , sw_supply
     :                              , dlt_sw_dep)
*     ===========================================================
      use ComponentInterfaceModule

      implicit none

*+  Sub-Program Arguments
      INTEGER    num_layer       ! (INPUT)  number of layers in profile
      REAL       sw_demand       ! (INPUT)  total crop demand for water (mm)
      REAL       sw_supply(*)    ! (INPUT)  potential water to take up (supply)
      real       dlt_sw_dep (*)  ! (OUTPUT) root water uptake (mm)

*+  Purpose
*       Returns actual water uptake from each layer of the soil
*       profile by the crop (mm).

*+  Mission Statement
*   Calculate the crop uptake of soil water

*+  Changes
*       200498 nih created from crop_sw_uptake0

*+  Constant Values
      character  my_name*(*)     ! name of procedure
      parameter (my_name = 'soilwat2_sw_uptake')

*+  Local Variables
      integer    deepest_layer   ! deepest layer in which the roots are growing
      integer    layer           ! layer number of profile ()
      real       sw_supply_sum   ! total potential over profile (mm)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

            ! find total root water potential uptake as sum of all layers

      deepest_layer = num_layer
      sw_supply_sum = sum_real_array (sw_supply, deepest_layer)
      if (sw_supply_sum.le.0.0 .or. sw_demand.le.0.0) then
            ! we have no uptake - there is no demand or potential

         call fill_real_array (dlt_sw_dep, 0.0, num_layer)

      else
               ! get actual uptake

         call fill_real_array (dlt_sw_dep, 0.0, num_layer)
         if (sw_demand.lt.sw_supply_sum) then

               ! demand is less than what roots could take up.
               ! water is non-limiting.
               ! distribute demand proportionately in all layers.

            do 1000 layer = 1, deepest_layer
               dlt_sw_dep(layer) = - divide (sw_supply(layer)
     :                                    , sw_supply_sum, 0.0)
     :                            * sw_demand

1000        continue

         else
                ! water is limiting - not enough to meet demand so take
                ! what is available (potential)

            do 1100 layer = 1, deepest_layer
               dlt_sw_dep(layer) = - sw_supply(layer)

1100        continue

         endif
      endif

      call pop_routine (my_name)
      return
      end

*     ===========================================================
      subroutine soilwat2_evaporation (esoil, eos)
*     ===========================================================
      use Soilwat2Module
      use ComponentInterfaceModule

      implicit none

*+  Sub-Program Arguments
      real       eos                   ! (output) potential soil evap after
                                       ! modification for crop cover & residue_wt
      real       esoil(*)              ! (output) actual soil evaporation (mm)

*+  Purpose
*       calculate actual soil evaporation

*+  Mission Statement
*     Calculate Actual Soil Evaporation

*+  Changes
*       031296 pdev removed g_es, replaced with g_es_layers.

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_evaporation')

*+  Local Variables
      real       asw1                  ! available soil water in top layer for
                                       ! actual soil evaporation (mm)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      ! 1. get potential soil water evaporation
!      call soilwat2_pot_soil_evaporation (eos)

      ! 2. get available soil water for evaporation
         ! NB. ritchie + b&s evaporate from layer 1, but rickert
         !     can evaporate from L1 + L2.
      asw1 = g%sw_dep(1) - g%air_dry_dep(1)
!      asw1 = bound (asw1, 0.0, g%eo)
      asw1 = l_bound (asw1, 0.0)

      ! 3. get actual soil water evaporation
      call soilwat2_soil_evaporation (esoil, eos, asw1)

      call pop_routine (my_name)
      return
      end


