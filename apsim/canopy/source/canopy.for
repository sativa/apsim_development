! #Issue Flags for  further work
!     ===========================================================
      subroutine alloc_dealloc_instance(doAllocate)
!     ===========================================================
      use CanopyModule
      implicit none
      ml_external alloc_dealloc_instance

!+  Sub-Program Arguments
      logical, intent(in) :: doAllocate

!+  Purpose
!      Module instantiation routine.

!- Implementation Section ----------------------------------

      if (doAllocate) then
         allocate(id)
         allocate(g)
         allocate(p)
         allocate(c)

      else
         deallocate(id)
         deallocate(g)
         deallocate(p)
         deallocate(c)

      end if
      return
      end

!     ===========================================================
      subroutine do_init1(sdml)
!     ===========================================================
      use CanopyModule
      implicit none
      ml_external do_init1

!+  Purpose
!      Perform all registrations and zeroing

!+  Sub-Program Arguments
      character (len=*), intent(in) :: sdml

!- Implementation Section ----------------------------------

      call do_registrations(id)

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
      use CanopyModule
      use ComponentInterfaceModule
      implicit none
      ml_external do_init2

*+  Purpose
*     Initialise the micromet module

*+  Changes

*+  Calls

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Canopy_init')

*+  Local Variables
       character Event_string*40       ! String to output

*- Implementation Section ----------------------------------

      call push_routine (myname)

      call Canopy_zero_variables ()

      ! Notify system that we have initialised

      Event_string = 'Initialising'
      call Write_string (Event_string)

      ! Get all parameters from parameter file

      call Canopy_read_constants ()

      call Canopy_read_param ()

      call pop_routine (myname)
      return
      end


!     ===========================================================
      subroutine respondToEvent(fromID, eventID, variant)
!     ===========================================================
      use CanopyModule
      use ComponentInterfaceModule
      implicit none
      ml_external respondToEvent

!+  Purpose
!      Event handler for all events coming into module.

!+  Sub-Program Arguments
      integer, intent(in out) :: fromID
      integer, intent(in) :: eventID
      integer, intent(in out) :: variant

!- Implementation Section ----------------------------------

      if (eventID .eq. id%DoMicromet) then
         call on_do_micromet()

      else if (eventID .eq. id%CanopyChanged) then
         call on_canopy_changed(variant)

      else if (eventID .eq. id%ResidueChanged) then
         call OnResidueChanged(variant)
         
      else if (eventID .eq. id%Tick) then
         call on_tick(variant)

      else if (eventID .eq. id%newmet) then
         call on_newmet(variant)

      else
         call error('bad event ID',.true.)
      endif
      return
      end
!     ===========================================================
      subroutine respondToMethod(fromID, methodID, variant)
!     ===========================================================
      use CanopyModule
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

      if (methodID.eq.id%lai_table) then
         call Canopy_table ('LAI',g%LAI)
      else if (methodID.eq.id%cover_table) then
         call Canopy_table ('Cover',g%Cover)
      else if (methodID.eq.id%f_table) then
         call Canopy_table ('F',g%F)
      else if (methodID.eq.id%rs_table) then
         call Canopy_table ('Rs',g%Rs)
      else if (methodID.eq.id%rl_table) then
         call Canopy_table ('Rl',g%Rl)
      else if (methodID.eq.id%gc_table) then
         call Canopy_table ('Gc',g%Gc)
      else if (methodID.eq.id%ga_table) then
         call Canopy_table ('Ga',g%Ga)
      else if (methodID.eq.id%pet_table) then
         call Canopy_table ('PET',g%PET)
      else if (methodID.eq.id%petr_table) then
         call Canopy_table ('PETr',g%PETr)
      else if (methodID.eq.id%peta_table) then
         call Canopy_table ('PETa',g%PETa)
      else if (methodID.eq.id%omega_table) then
         call Canopy_table ('Omega',g%Omega)
      else
         call error('bad method ID',.true.)
      endif

      return
      end

!     ===========================================================
      subroutine notify_termination()
!     ===========================================================
      use CanopyModule
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
      use CanopyModule
      implicit none
      ml_external respondToGet

*+  Sub-Program Arguments
      integer, intent(in) :: fromID
      type(QueryData), intent(in) :: variable_info

*+  Purpose
*      Return the value of one of our variables to caller

*+  Changes

*+  Calls

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'respondToGet')

*+  Local Variables
       integer i
       integer j
       real Total_interception

*- Implementation Section ----------------------------------

      call push_routine (myname)

      if (Variable_info%id .eq. id%interception) then

         Total_Interception = 0.0

         do 200 i = 1, g%NumLayers
            do 100 j = 1, g%NumComponents
               Total_Interception = Total_Interception
     :                            + g%Interception(i,j)
  100       continue
  200    continue

         call return_interception (variable_info, Total_interception)

      elseif (Variable_info%id .eq. id%eo) then
         call return_eo (variable_info, g%eo)
      else

      endif

      call pop_routine (myname)

      return
      end
* ====================================================================
      logical function respondToSet (fromID, VariableID, variant)
* ====================================================================
      use ComponentInterfaceModule

      implicit none
      ml_external respondToSet

!+  Sub-Program Arguments
      integer, intent(in) :: fromID
      integer, intent(in)     :: VariableID
      integer, intent(in out) :: variant


*+  Purpose
*       Set one of our variables altered by some other module.

*+  Changes
*      21-06-96 NIH Changed respond2set calls to collect calls

*+  Calls

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'respondToSet')

*+  Local Variables
*- Implementation Section ----------------------------------

      respondToSet = .false.
      return
      end

* ====================================================================
       subroutine Canopy_zero_variables ()
* ====================================================================
      use CanopyModule
      use ComponentInterfaceModule
      implicit none

*+  Purpose
*     Set all variables to initial state.  i.e. zero or blank.

*+  Mission Statement
*     Set internal state variables to zero

*+  Changes
*     NIH 28/3/00 Specified

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Canopy_zero_variables')

*+  Local Variables

*- Implementation Section ----------------------------------

      call push_routine (myname)

      g%NumComponents = 0
c      g%ComponentName(:) = ' '
c      g%ComponentType(:) = ' '
c      g%ComponentLAI(:) = 0.0
c      g%ComponentCover(:) = 0.0
      g%K(:,:) = 0.0
c      g%ComponentHeight(:) = 0.0
c      g%ComponentDepth(:) = 0.0
      g%ComponentGsmax(:) = 0.0
      g%ComponentR50(:) = 0.0
      g%ComponentAlbedo(:) = 0.0
      g%ComponentEmissivity(:) = 0.0
c      g%ComponentFrgr(:) = 0.0

      g%DeltaZ(:) = 0.0
      g%NumLayers = 0

      g%LayerK(:) = 0.0

      g%LAI(:,:) = 0.0
      g%Cover(:,:) = 0.0
      g%CoverTotal(:,:) = 0.0
      g%F(:,:) = 0.0
      g%Rs(:,:) = 0.0
      g%Rl(:,:) = 0.0
      g%Gc(:,:) = 0.0
      g%Ga(:,:) = 0.0
      g%PET(:,:) = 0.0
      g%PETr(:,:) = 0.0
      g%PETa(:,:) = 0.0
      g%Omega(:,:) = 0.0
      g%Interception(:,:) = 0.0
      g%albedo = 0.0
      g%Emissivity = 0.0


      g%latitude = 0.0
      g%day = 0
      g%year = 0
      g%AverageT = 0.0
      g%SunshineHours = 0.0
      g%DayLength = 0.0

      c%air_pressure = 0.0
      c%soil_emissivity = 0.0

      p%soil_albedo = 0.0
      p%layer_ga = 0.0
      p%A_interception = 0.0
      p%B_interception = 0.0
      p%C_interception = 0.0
      p%D_interception = 0.0

      call pop_routine (myname)
      return
      end

*     ===========================================================
      subroutine Canopy_read_param ()
*     ===========================================================
      use CanopyModule
      use ComponentInterfaceModule
      implicit none

*+  Purpose
*       Read in all parameters from parameter file.

*+  Mission Statement
*     Read parameters from parameter file

*+  Changes
*     NIH 28/3/00 Specified

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Canopy_read_param')
*
      character section_name*(*)
      parameter (section_name = 'parameters')

*+  Local Variables
      logical found

*- Implementation Section ----------------------------------

      call push_routine (myname)

      call write_string (new_line//'   - Reading Parameters')

      found = read_parameter(
     :        section_name,          ! Section header
     :        'soil_albedo',         ! Keyword
     :        p%soil_albedo,         ! Parameter
     :        0.0,                   ! Lower Limit for bound checking
     :        1.0)                   ! Upper Limit for bound checking

      found = read_parameter (
     :        section_name,          ! Section header
     :        'layer_ga',            ! Keyword
     :        p%layer_ga,            ! Parameter
     :        0.0,                   ! Lower Limit for bound checking
     :        1.0)                   ! Upper Limit for bound checking

      found = read_parameter (
     :        section_name,          ! Section header
     :        'a_interception',      ! Keyword
     :        p%a_interception,      ! Parameter
     :        0.0,                   ! Lower Limit for bound checking
     :        10.0,                  ! Upper Limit for bound checking
     :        .true.)                ! is optional
      if(.not. found) then
         p%a_interception = 0.0
      else
      endif

      found = read_parameter(
     :        section_name,          ! Section header
     :        'b_interception',      ! Keyword
     :        p%b_interception,      ! Parameter
     :        0.0,                   ! Lower Limit for bound checking
     :        5.0,                   ! Upper Limit for bound checking
     :        .true.)                ! is optional
      if(.not. found) then
         p%b_interception = 1.0
      else
      endif

      found = read_parameter(
     :        section_name,          ! Section header
     :        'c_interception',      ! Keyword
     :        p%c_interception,      ! Parameter
     :        0.0,                   ! Lower Limit for bound checking
     :        10.0,                  ! Upper Limit for bound checking
     :        .true.)                ! is optional
      if(.not. found) then
         p%c_interception = 0.0
      else
      endif

      found = read_parameter (
     :        section_name,          ! Section header
     :        'd_interception',      ! Keyword
     :        p%d_interception,      ! Parameter
     :        0.0,                   ! Lower Limit for bound checking
     :        20.0,                  ! Upper Limit for bound checking
     :        .true.)                ! is optional
      if(.not. found) then
         p%d_interception = 0.0
      else
      endif

      found = read_parameter(
     :        section_name,          ! Section header
     :        'eo_source',           ! Keyword
     :        p%eo_source,           ! Parameter
     :        .true.)

      if(.not.found) then
         p%eo_source = blank
      else
         ! better register my interest in this data
         g%EoSourceID = add_registration(GetVariableReg
     :                                  ,p%eo_source
     :                                  ,Eoddml)
      endif


      call pop_routine (myname)
      return
      end



* ====================================================================
       subroutine Canopy_read_constants ()
* ====================================================================
      use CanopyModule
      use ComponentInterfaceModule
      implicit none

*+  Purpose
*      Read in all constants from ini file.

*+  Mission Statement
*     Read constants from ini file

*+  Changes
*     NIH 28/3/00 Specified

*+  Constant Values
      character*(*) section_name
      parameter (section_name = 'constants')
*
      character*(*) myname               ! name of current procedure
      parameter (myname = 'Canopy_read_constants')

*+  Local Variables
      logical found

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call write_string (new_line//'   - Reading Constants')

      found = read_parameter (
     :           section_name         ! Section header
     :         , 'air_pressure'       ! Keyword
     :         , c%air_pressure       ! Variable
     :         , 900.                 ! Lower Limit for bound checking
     :         , 1100.)               ! Upper Limit for bound checking

      found = read_parameter(
     :           section_name         ! Section header
     :         , 'soil_emissivity'    ! Keyword
     :         , c%soil_emissivity    ! Variable
     :         , 0.9                  ! Lower Limit for bound checking
     :         , 1.0)                 ! Upper Limit for bound checking

      found = read_parameter(
     :           section_name         ! Section header
     :         , 'min_crit_temp'      ! Keyword
     :         , c%min_crit_temp      ! Variable
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 10.)                 ! Upper Limit for bound checking

      found = read_parameter(
     :           section_name         ! Section header
     :         , 'max_crit_temp'      ! Keyword
     :         , c%max_crit_temp      ! Variable
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 50.)                 ! Upper Limit for bound checking

          
      call pop_routine (myname)
      return
      end


*     ===========================================================
      subroutine on_canopy_changed(variant)
*     ===========================================================
      use CanopyModule
      use ComponentInterfaceModule
      implicit none

*+  Purpose
*       Obtain updated information about a plant canopy

!+  Sub-Program Arguments
      integer, intent(in out) :: variant

*+  Mission Statement
*       Obtain updated information about a plant canopy

*+  Changes
*     NIH 30/3/00 Specified

*+  Calls
      integer Canopy_Component_Number ! function

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'on_canopy_changed')

*+  Local Variables
      integer    numvals               ! number of values read
      character  sender*32
      integer    ComponentNo
      type (canopyType), dimension(max_canopies) :: canopies
      integer num_canopies
      integer counter
*- Implementation Section ----------------------------------

      call push_routine (myname)

      call unpack_canopy(variant, canopies, num_canopies)

      call Write_string ('got it')
!      print*,canopies(1)%cropType
!      print*,canopies(1)%NumLayers
!      print*,canopies(1)%layer(1)%thickness
!      print*,canopies(1)%layer(1)%Lai
!      print*,canopies(1)%layer(1)%CoverGreen
!      print*,canopies(1)%layer(1)%CoverTotal
!      print*,canopies(1)%frgr
!      pause

      do 100 counter = 1, num_canopies

         ComponentNo = Canopy_Component_Number
     :                   (canopies(counter)%name)

         if (ComponentNo.eq.0) then

            g%NumComponents = g%NumComponents + 1
            g%Canopies(g%NumComponents) = canopies(counter)

            ! Read Component Specific Constants
            ! ---------------------------------
            call Canopy_component_constants(g%NumComponents)

         else

            g%Canopies(ComponentNo) = canopies(counter)

         endif
  100 continue

      call pop_routine (myname)
      return
      end

*     ===========================================================
      subroutine on_do_micromet()
*     ===========================================================
      use CanopyModule
      implicit none

*+  Purpose
*       Perform Prepare Phase Calculations

*+  Mission Statement
*       Perform Prepare Phase Calculations

*+  Changes
*     NIH 30/3/00 Specified

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'on_do_micromet')

*+  Local Variables

*- Implementation Section ----------------------------------

      call push_routine (myname)

      call Canopy_Met_Variables ()
      call Canopy_Canopy_Compartments ()
      call Canopy_Canopy_Energy_Balance ()
      call Canopy_Reference_Et()

      call Canopy_Energy_Balance_Event()
      call Canopy_Water_Balance_Event()

      call pop_routine (myname)
      return
      end

*     ===========================================================
      subroutine Canopy_Process ()
*     ===========================================================
      use CanopyModule
      use ComponentInterfaceModule
      implicit none

*+  Purpose
*       Perform Process Phase Calculations

*+  Mission Statement
*       Perform Process Phase Calculations

*+  Changes
*     NIH 30/3/00 Specified

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Canopy_Process')

*+  Local Variables

*- Implementation Section ----------------------------------

      call push_routine (myname)

      call Canopy_Calculate_Gc ()
      call Canopy_Calculate_Ga ()
      call Canopy_Calculate_Interception ()
      call Canopy_Calculate_PM()
      call Canopy_Calculate_Omega()


      call Canopy_Energy_Balance_Event()
      call Canopy_Water_Balance_Event()

      call pop_routine (myname)
      return
      end

*     ===========================================================
      subroutine Canopy_Canopy_Compartments ()
*     ===========================================================
      use CanopyModule
      use ComponentInterfaceModule
      implicit none

*+  Purpose
*       Break the combined Canopy into functional compartments

*+  Mission Statement
*       Break the combined Canopy into functional compartments

*+  Changes
*     NIH 30/3/00 Specified

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Canopy_Canopy_Compartments')

*+  Local Variables

*- Implementation Section ----------------------------------

      call push_routine (myname)

      Call Canopy_Define_Layers ()

      Call Canopy_Divide_Components ()

      Call Canopy_Light_Extinction()

      call pop_routine (myname)
      return
      end

*     ===========================================================
      subroutine Canopy_Define_Layers ()
*     ===========================================================
      use CanopyModule
      use ComponentInterfaceModule
      implicit none

*+  Purpose
*       Break the combined Canopy into layers

*+  Mission Statement
*       Break the combined Canopy into layers

*+  Changes
*     NIH 30/3/00 Specified

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Canopy_Define_Layers')

*+  Local Variables
      real nodes(2*max_components - 1)
      integer NumNodes
      integer ComponentNo
      integer Node
      real    CanopyBase
      integer key(2*max_components - 1)
      integer layer
      real    CumHeight

*- Implementation Section ----------------------------------

      call push_routine (myname)

      nodes(:) = 0.0
      NumNodes = 0

      ! Bottom layer will start at ground surface
      NumNodes = 1
      Nodes(1) = 0.0

      do 100 ComponentNo = 1, g%NumComponents

         CumHeight = 0.0
         do 50 layer = 1, max_layer
               CumHeight = CumHeight
     :            + g%Canopies(ComponentNo)%Layer(layer)%thickness
            if (position_in_real_array
     :         (CumHeight
     :         ,Nodes
     :         ,NumNodes)
     :       .eq.0) then
               NumNodes = NumNodes + 1
               Nodes(NumNodes) = CumHeight

            else
               ! it is already there - ignore it
            endif
   50    continue
  100 continue

      ! Sort into Ascending order
      call shell_sort_real (Nodes,NumNodes,key)

      g%NumLayers = NumNodes - 1

      do 200 Node = 1, NumNodes - 1
         g%DeltaZ(Node) = Nodes(Node+1) - Nodes(Node)
  200 continue

      call pop_routine (myname)
      return
      end


*     ===========================================================
      subroutine Canopy_Divide_Components ()
*     ===========================================================
      use CanopyModule
      use ComponentInterfaceModule
      implicit none

*+  Purpose
*       Break the components into layers

*+  Mission Statement
*       Break the components into layers

*+  Changes
*     NIH 30/3/00 Specified

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Canopy_Divide_Components')

*+  Local Variables
      integer i
      integer j
      real    KLAI(max_layer)
      real    KLAInew(max_layer)
      real    KLAItot(max_layer)
      real    KLAItotnew(max_layer)

*- Implementation Section ----------------------------------

      call push_routine (myname)

      g%LAI(:,:) = 0.0

         ! Calculate LAI for layer i and component j
         ! =========================================

      do 200 j = 1, g%NumComponents

         do 100 i=1,g%Canopies(j)%NumLayers
            if(g%Canopies(j)%Layer(i)%CoverGreen.gt.0) then
               KLAI(i) = - log(1.0-g%Canopies(j)%Layer(i)%CoverGreen)
               KLAItot(i) = - log(1.0-g%Canopies(j)%Layer(i)%CoverTotal)
            else
               KLAI(i) = 0.0
               KLAItot(i) = 0.0
            endif
  100    continue

         call map(g%Canopies(j)%NumLayers
     :           ,g%Canopies(j)%Layer(:)%thickness
     :           ,g%Canopies(j)%Layer(:)%LAI
     :           ,g%NumLayers
     :           ,g%DeltaZ
     :           ,g%LAI(:,j))

         call map(g%Canopies(j)%NumLayers
     :           ,g%Canopies(j)%Layer(:)%thickness
     :           ,KLAI
     :           ,g%NumLayers
     :           ,g%DeltaZ
     :           ,KLAInew)

         call map(g%Canopies(j)%NumLayers
     :           ,g%Canopies(j)%Layer(:)%thickness
     :           ,KLAItot
     :           ,g%NumLayers
     :           ,g%DeltaZ
     :           ,KLAItotnew)

         do 150 i=1,g%NumLayers
            g%Cover(i,j) = 1.0-exp(-KLAInew(i))
            g%CoverTotal(i,j) = 1.0-exp(-KLAItotnew(i))
  150    continue

  200 continue

         ! Calculate fractional contribution for layer i and component j
         ! =============================================================

      do 300 i=1, g%NumLayers

         do 250 j = 1, g%NumComponents
            g%F(i,j) = divide(g%LAI(i,j)
     :                       ,sum(g%LAI(i,1:g%NumComponents))
     :                       ,0.0)
  250    continue

  300 continue

      call pop_routine (myname)
      return
      end

*     ===========================================================
      subroutine Canopy_Table (Title,Array)
*     ===========================================================
      use CanopyModule
      use ComponentInterfaceModule
      implicit none

*+  Sub-Program Arguments
      character Title*(*)
      real      Array(1:max_layer,1:max_components)

*+  Purpose
*       Print out a 2-Dimensional table for a given state variable

*+  Mission Statement
*       Print out a 2-Dimensional table for a given state variable

*+  Changes
*     NIH 30/3/00 Specified

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Canopy_Table')

*+  Local Variables
      integer i
      integer j
      character string*200
      character line_rule*200
      real      top
      real      bottom

*- Implementation Section ----------------------------------

      call push_routine (myname)

      write(line_rule,'(5x,70(''-''))')


      call write_string('Table for '//Title)

      call write_string(Line_Rule)

      write(string,'(5x,a,4x,11a10)')
     :       'Canopy Layer Height'
     :      ,(g%Canopies(j)%name,j=1,g%NumComponents)
     :      ,'Total'
      call write_string(String)

      call write_string(Line_Rule)

      do 100 i = g%NumLayers, 1, -1
         top = sum(g%DeltaZ(1:i))
         if (i.eq.1) then
            bottom = 0.0
         else
            bottom = top - g%DeltaZ(i)
         endif

         write(string,'(x,f7.1,'' - '',f7.1,5x,11f10.3)')
     :              bottom
     :           ,  top
     :           , (array(i,j),j=1,g%NumComponents)
     :           , sum(array(i,1:g%NumComponents))

         call write_string(String)

  100 continue

      call write_string(Line_Rule)

         write(string,'(x,''       Total     '',5x,11f10.3)')
     :            (sum(array(1:g%NumLayers,j)),j=1,g%NumComponents)
     :            ,sum(array(:,:))
         call write_string(String)

      call write_string(Line_Rule)


      call pop_routine (myname)
      return
      end

*     ===========================================================
      subroutine Canopy_Light_Extinction ()
*     ===========================================================
      use CanopyModule
      use ComponentInterfaceModule
      implicit none

*+  Purpose
*       Calculate light extinction parameters

*+  Mission Statement
*       Calculate light extinction parameters

*+  Changes
*     NIH 30/3/00 Specified

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Canopy_Light_Extinction')

*+  Local Variables
      integer i
      integer j

*- Implementation Section ----------------------------------

      call push_routine (myname)

         ! Calculate effective K from LAI and Cover
         ! ========================================

      do 150 i = 1, g%NumLayers
         do 100 j = 1, g%NumComponents


            g%K(i,j) = divide(-log(1.-g%Cover(i,j))
     :                           ,g%LAI(i,j)
     :                           ,0.0)

  100    continue
  150 continue

         ! Calculate extinction for individual layers
         ! ==========================================


      do 200 i = 1, g%NumLayers

         g%LayerK(i) = Sum(g%F(i,1:g%NumComponents)
     :                   * g%K(i,1:g%NumComponents))

  200 continue

      call pop_routine (myname)
      return
      end

*     ===========================================================
      subroutine Canopy_Canopy_Energy_Balance ()
*     ===========================================================
      use CanopyModule
      use ComponentInterfaceModule
      implicit none

*+  Purpose
*       Perform the Overall Canopy Energy Balance

*+  Mission Statement
*       Perform the Overall Canopy Energy Balance

*+  Changes
*     NIH 30/3/00 Specified

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Canopy_Canopy_Energy_Balance')

*+  Local Variables

*- Implementation Section ----------------------------------

      call push_routine (myname)

      Call Canopy_short_wave_radiation ()
      Call Canopy_Energy_Terms ()
      Call Canopy_Long_Wave_Radiation ()

      call pop_routine (myname)
      return
      end

*     ===========================================================
      subroutine Canopy_short_wave_radiation ()
*     ===========================================================
      use CanopyModule
      use ComponentInterfaceModule
      implicit none

*+  Purpose
*       Calculate interception of short wave by canopy compartments

*+  Mission Statement
*       Calculate interception of short wave by canopy compartments

*+  Changes
*     NIH 30/3/00 Specified

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Canopy_short_wave_radiation')

*+  Local Variables
      integer i
      integer j
      real    Rin
      real    Rint
*- Implementation Section ----------------------------------

      call push_routine (myname)

         ! Perform Top-Down Light Balance
         ! ==============================

      Rin = g%met%Radn

      do 200 i = g%NumLayers,1,-1

         Rint = Rin
     :        * (1. - exp(-g%LayerK(i)
     :                    *sum(g%LAI(i,1:g%NumComponents))))

         !  #Issue NIH 20/08/02
         !  Need to Take into account interception by dead leaves

         do 100 j = 1, g%NumComponents
            g%Rs(i,j) = Rint
     :                * divide(g%F(i,j)*g%K(i,j)
     :                        ,g%LayerK(i)
     :                        ,0.0)
  100    continue

         Rin = Rin - Rint

  200 continue

      call pop_routine (myname)
      return
      end


*     ===========================================================
      subroutine on_newmet (variant)
*     ===========================================================
      use CanopyModule
      use ComponentInterfaceModule
      implicit none

*+  Purpose
*       Obtain all relevant met data

!+  Sub-Program Arguments
      integer, intent(in out) :: variant

*+  Mission Statement
*       Obtain all relevant met data

*+  Changes
*     NIH 30/3/00 Specified

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'on_newmet')

*+  Local Variables

*- Implementation Section ----------------------------------

      call push_routine (myname)

      
      call unpack_newmet(variant, g%met)

      call pop_routine (myname)
      return
      end

*     ===========================================================
      subroutine Canopy_Long_Wave_Radiation ()
*     ===========================================================
      use CanopyModule
      use MicrometScienceModule
      use ComponentInterfaceModule
      implicit none

*+  Purpose
*       Calculate Net Long Wave Radiation Balance

*+  Mission Statement
*       Calculate Net Long Wave Radiation Balance

*+  Changes
*     NIH 30/3/00 Specified

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Canopy_Long_Wave_Radiation')

*+  Local Variables
      integer i
      integer j
      real    Net_Long_Wave
      real    FractionClearSky

*- Implementation Section ----------------------------------

      call push_routine (myname)

      FractionClearSky = divide(g%SunshineHours
     :                         ,g%DayLength
     :                         ,0.0)

      Net_Long_Wave = Canopy_longwave(g%AverageT
     :                                  ,FractionClearSky
     :                                  ,g%Emissivity)
     :              * day2hr * hr2s / 1.0e6  ! W to MJ


         ! Long Wave Balance Proportional to Short Wave Balance
         ! ====================================================

      do 200 i = g%NumLayers,1,-1


         do 100 j = 1, g%NumComponents
            g%Rl(i,j) = divide(g%Rs(i,j)
     :                        ,g%met%Radn
     :                        ,0.0)
     :                * Net_Long_Wave
  100    continue


  200 continue

      call pop_routine (myname)
      return
      end

*     ===========================================================
      subroutine Canopy_Met_Variables ()
*     ===========================================================
      use CanopyModule
      use MicrometScienceModule
      use ComponentInterfaceModule
      implicit none

*+  Purpose
*       Calculate Daily Met Variables

*+  Mission Statement
*       Calculate Daily Met Variables

*+  Changes
*     NIH 30/3/00 Specified

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Canopy_Met_Variables')

*+  Local Variables

*- Implementation Section ----------------------------------

      call push_routine (myname)


      g%Daylength = Canopy_DayLength(g%latitude,g%day)

      g%AverageT = (g%met%maxt + g%met%mint)/2.0

      g%SunshineHours = Canopy_Sunshine_Hours(g%met%Radn
     :                                         ,g%latitude
     :                                         ,g%day)


      call pop_routine (myname)
      return
      end

*     ===========================================================
      subroutine on_tick (variant)
*     ===========================================================
      use CanopyModule
      use ComponentInterfaceModule
      implicit none

*+  Purpose
*     Update internal time record and reset daily state variables.

!+  Sub-Program Arguments
      integer, intent(in out) :: variant

*+  Mission Statement
*     Update internal time record and reset daily state variables.

*+  Changes
*        140400 nih

*+  Local Variables
      type(timeType) :: time

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'on_tick')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      ! Note that time and timestep information is not required
      ! and so dummy variables are used in their place.

      call unpack_time(variant, time)
      call jday_to_day_of_year (dble(time%startday), g%day, g%year)

      call pop_routine (myname)
      return
      end

*     ===========================================================
      subroutine Canopy_Energy_Terms ()
*     ===========================================================
      use CanopyModule
      use ComponentInterfaceModule
      implicit none

*+  Purpose
*       Calculate the overall system energy terms

*+  Mission Statement
*       Calculate the overall system energy terms

*+  Changes
*     NIH 14/4/00 Specified

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Canopy_Energy_Terms')

*+  Local Variables
      integer i
      integer j

*- Implementation Section ----------------------------------

      call push_routine (myname)

      ! Each term is a radiation weighted average of component terms
      ! ============================================================

      g%albedo = 0.0
      g%Emissivity = 0.0

      do 200 i = g%NumLayers,1,-1

         do 100 j = 1, g%NumComponents
            g%albedo = g%albedo
     :               + divide(g%Rs(i,j)
     :                       ,g%met%Radn
     :                       ,0.0)
     :                   * g%ComponentAlbedo(j)
            g%Emissivity = g%Emissivity
     :               + divide(g%Rs(i,j)
     :                       ,g%met%Radn
     :                       ,0.0)
     :                   * g%ComponentEmissivity(j)

  100    continue

  200 continue

      g%albedo = g%albedo
     :         + (1. - divide(sum(g%Rs(:,:))
     :                       ,g%met%Radn
     :                       ,0.0))
     :             * p%Soil_Albedo

      g%Emissivity = g%Emissivity
     :         + (1. - divide(sum(g%Rs(:,:))
     :                       ,g%met%Radn
     :                       ,0.0))
     :             * c%Soil_Emissivity

      call pop_routine (myname)
      return
      end

*     ===========================================================
      subroutine Canopy_component_constants (Cno)
*     ===========================================================
      use CanopyModule
      use ComponentInterfaceModule
      implicit none

*+  Sub-Program Arguments
      integer Cno ! Component Number

*+  Purpose
*       Read constants for a given canopy component

*+  Mission Statement
*       Read constants for a given canopy component

*+  Changes
*     NIH 30/3/00 Specified

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Canopy_component_constants')

*+  Local Variables
      integer    numvals               ! number of values read
      character  search_order(max_table)*32 ! sections to search
      integer    num_sections          ! number of sections to search
      logical    found

*- Implementation Section ----------------------------------

      call push_routine (myname)

         ! Read Component Specific Constants
         ! ---------------------------------
         ! (should be in dedicated routine)

         found = read_parameter (
     :           g%canopies(Cno)%croptype
     :         , 'albedo'             ! Keyword
     :         , g%ComponentAlbedo(Cno)  ! Variable
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 1.0)                 ! Upper Limit for bound checking

         found = read_parameter (
     :           g%canopies(Cno)%croptype
     :         , 'emissivity'         ! Keyword
     :         , g%ComponentEmissivity(Cno)  ! Variable
     :         , 0.9                  ! Lower Limit for bound checking
     :         , 1.0)                 ! Upper Limit for bound checking

         found = read_parameter (
     :           g%canopies(Cno)%croptype
     :         , 'gsmax'              ! Keyword
     :         , g%ComponentGsmax(Cno)  ! Variable
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 1.0)                 ! Upper Limit for bound checking

         found = read_parameter (
     :           g%canopies(Cno)%croptype
     :         , 'r50'                ! Keyword
     :         , g%ComponentR50(Cno)  ! Variable
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 1e3)                 ! Upper Limit for bound checking


      call pop_routine (myname)
      return
      end

*     ===========================================================
      subroutine Canopy_Calculate_Gc ()
*     ===========================================================
      use CanopyModule
      use MicrometScienceModule
      use ComponentInterfaceModule
      implicit none

*+  Purpose
*       Calculate the canopy conductance for system compartments

*+  Mission Statement
*       Calculate the canopy conductance for system compartments

*+  Changes
*     NIH 19/4/00 Specified

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Canopy_Calculate_Gc')

*+  Local Variables
      integer i
      integer j
      real    layerLAI
      real    Rin
      real    Rint
      real    Rflux

*- Implementation Section ----------------------------------

      call push_routine (myname)

      Rin = g%met%Radn

      do 200 i = g%NumLayers,1,-1
         LayerLAI = sum(g%LAI(i,:))

         Rflux = Rin * 10**6 / (g%DayLength *3600.0) ! should use convert.inc
     :         * (1. - g%albedo)

         do 100 j = 1, g%NumComponents
            g%Gc(i,j) = Canopy_CanopyConductance
     :                    (g%ComponentGsmax(j)
     :                    ,g%ComponentR50(j)
     :                    ,g%Canopies(j)%Frgr
     :                    ,g%F(i,j)
     :                    ,g%LayerK(i)
     :                    ,LayerLAI
     :                    ,Rflux)

  100    continue

         ! Calculate Rin for next layer down
         Rint = sum(g%Rs(i,:))
         Rin = Rin - Rint

  200 continue

      call pop_routine (myname)
      return
      end

*     ===========================================================
      subroutine Canopy_Calculate_Ga ()
*     ===========================================================
      use CanopyModule
      use ComponentInterfaceModule
      implicit none

*+  Purpose
*       Calculate the aerodynamic conductance for system compartments

*+  Mission Statement
*       Calculate the aerodynamic conductance for system compartments

*+  Changes
*     NIH 30/5/00 Specified

*+  Calls

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Canopy_Calculate_Ga')

      real       WindSpeed
      parameter (WindSpeed = 3.0)

*+  Local Variables
      integer i
      integer j
      real layer_ga

*- Implementation Section ----------------------------------

      call push_routine (myname)

      do 200 i = 1, g%NumLayers

         if (i .eq. 1) then
!            layer_ga = Canopy_AerodynamicCondNew (
!     :                         WindSpeed
!     :                       , MetHeight
!     :                       , CropHeight
!     :                       , CropLAI
!     :                       , Meas_ZeroPlane)
            layer_ga = p%layer_ga
         else
!            layer_ga = Canopy_AerodynamicCondSub (
!     :                                        WindSpeed
!     :                                      , WindAttenuation
!     :                                      , MetHeight
!     :                                      , CropLAI
!     :                                      , CropHeight
!     :                                      , SourceTop
!     :                                      , SourceSub
!     :                                      , Meas_ZeroPlane)
            layer_ga = p%layer_ga
         endif


         do 100 j = 1, g%NumComponents

            g%Ga(i,j) = layer_ga
     :                * g%F(i,j)

  100    continue

  200 continue

      call pop_routine (myname)
      return
      end

*     ===========================================================
      subroutine Canopy_Calculate_Interception ()
*     ===========================================================
      use CanopyModule
      use ComponentInterfaceModule
      implicit none

*+  Purpose
*       Calculate the interception loss of water from the canopy

*+  Mission Statement
*       Calculate the interception loss of water from the canopy

*+  Changes
*     NIH 23/8/00 Specified

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Canopy_Calculate_Interception')

*+  Local Variables
      real Total_LAI
      real Total_Interception
      integer i
      integer j

*- Implementation Section ----------------------------------

      call push_routine (myname)

      Total_LAI = sum(g%LAI(:,:))

      Total_Interception = p%A_interception
     :              * g%met%rain**p%B_interception
     :               + p%C_interception * Total_LAI
     :               + p%D_interception

      Total_Interception = bound(Total_Interception, 0.0,
     :                           0.99*g%met%Rain)

      do 200 i = 1, g%NumLayers
         do 100 j = 1, g%NumComponents

            g%Interception(i,j) = divide(g%LAI(i,j)
     :                                  ,sum(g%LAI(:,:))
     :                                  ,0.0)
     :                          * Total_Interception

  100    continue
  200 continue
      call pop_routine (myname)
      return
      end


*     ===========================================================
      subroutine Canopy_Calculate_Omega ()
*     ===========================================================
      use CanopyModule
      use MicrometScienceModule
      use ComponentInterfaceModule
      implicit none

*+  Purpose
*       Calculate the aerodynamic decoupling for system compartments

*+  Mission Statement
*       Calculate the aerodynamic decoupling for system compartments

*+  Changes
*     NIH 30/5/00 Specified

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Canopy_Calculate_Omega')

*+  Local Variables
      integer i
      integer j

*- Implementation Section ----------------------------------

      call push_routine (myname)

      do 200 i = 1, g%NumLayers
         do 100 j = 1, g%NumComponents

            g%Omega(i,j) = Canopy_omega(g%met%mint
     :                                   ,g%met%maxt
     :                                   ,c%air_pressure
     :                                   ,g%Ga(i,j)
     :                                   ,g%Gc(i,j)
     :                                   )

  100    continue

  200 continue

      call pop_routine (myname)
      return
      end

*====================================================================
      subroutine Canopy_calculate_PM ()
*====================================================================
      Use CanopyModule
      use MicrometScienceModule
      use ComponentInterfaceModule
      implicit none

*+  Sub-Program Arguments

*+  Purpose
*     Calculate the Penman-Monteith water demand

*+  Notes

*+  Changes
*       270500 - NIH specified and programmed

*+  Calls
      REAL Canopy_PETa
      REAL Canopy_PETr
      REAL Canopy_Penman_Monteith

*+  Local Variables
      REAL AverageT
      REAL Lambda
      REAL NetRadiation       ! J
      INTEGER i
      INTEGER j
      REAL Free_Evap
      REAL Free_Evap_Ga
      REAL Free_Evap_Gc
      REAL Dry_Leaf_Fraction

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Canopy_calculate_PM')

*- Implementation Section ----------------------------------

      call push_routine (myname)

      NetRadiation = ((1. - g%Albedo) * sum(g%Rs(:,:))+ sum(g%Rl(:,:)))
     :             * 1e6        ! MJ/J

      Free_Evap_Ga = p%layer_ga
      Free_Evap_Gc = Free_Evap_Ga * 1e6  !=infinite surface conductance

      Free_Evap = Canopy_Penman_Monteith
     :              (
     :               NetRadiation
     :              ,g%met%mint
     :              ,g%met%maxt
     :              ,g%met%vp
     :              ,c%Air_Pressure
     :              ,g%daylength
     :              ,Free_Evap_Ga
     :              ,Free_Evap_Gc
     :              )

      Dry_Leaf_Fraction = 1.0 -  divide (sum(g%Interception(:,:))
     :                                  ,Free_Evap
     :                                  ,0.0)

      if (Dry_Leaf_Fraction.lt.0.0) then
!         call Warning_Error(Err_User,
!     :            'Interception volume > max free water evaporation')
         Dry_Leaf_Fraction = 0.0
      else
      endif

      averageT = (g%met%mint + g%met%maxt)/2.0
      Lambda = Canopy_Lambda (AverageT)

      do 200 i = 1, g%NumLayers
         do 100 j = 1, g%NumComponents

            NetRadiation = ((1. - g%Albedo) * g%Rs(i,j)+ g%Rl(i,j))
     :             * 1e6        ! MJ/J

            g%PETr(i,j) = Canopy_PETr
     :              (
     :               NetRadiation * Dry_Leaf_Fraction
     :              ,g%met%mint
     :              ,g%met%maxt
     :              ,c%Air_Pressure
     :              ,g%Ga(i,j)
     :              ,g%Gc(i,j)
     :              )

            g%PETa(i,j) = Canopy_PETa
     :                 (
     :                  g%met%mint
     :                 ,g%met%maxt
     :                 ,g%met%vp
     :                 ,c%Air_Pressure
     :                 ,g%daylength * Dry_Leaf_Fraction
     :                 ,g%Ga(i,j)
     :                 ,g%Gc(i,j)
     :                 )

            g%PET(i,j) = g%PETr(i,j) + g%PETa(i,j)

  100    continue
  200 continue

      call pop_routine (myname)

      return
      end

*====================================================================
      real function Canopy_Penman_Monteith
     :              (
     :               Rn
     :              ,mint
     :              ,maxt
     :              ,vp
     :              ,Air_Pressure
     :              ,daylength
     :              ,Ga
     :              ,Gc
     :              )
*====================================================================
      Use CanopyModule
      use MicrometScienceModule
      use ComponentInterfaceModule
      implicit none

*+  Sub-Program Arguments
      real Rn
      real mint
      real maxt
      real vp
      real Air_Pressure
      real daylength
      real Ga
      real Gc

*+  Purpose
*     Calculate the Penman-Monteith water demand

*+  Notes

*+  Changes
*       270500 - NIH specified and programmed

*+  Calls


*+  Local Variables
      REAL Non_dQs_dT
      REAL Lambda             ! J/kg
      REAL denominator         !of the Penman-Monteith equation
      REAL RhoA
      REAL SpecificVPD
      REAL averageT
      REAL PETa
      REAL PETr

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Canopy_Penman_Monteith')

*- Implementation Section ----------------------------------

      call push_routine (myname)

      averageT = (mint + maxt)/2.0
      Non_dQs_dT = Canopy_Non_dQs_dT (averageT ,Air_Pressure)
      RhoA = Canopy_RhoA (averageT, Air_Pressure)
      Lambda = Canopy_Lambda (AverageT)

      SpecificVPD = Canopy_SpecificVPD ( vp
     :                                   , mint
     :                                   , maxt
     :                                   , Air_Pressure)

      Denominator = Non_dQs_dT
     :            + Divide( Ga, Gc, 0.0)
     :            + 1.0                        ! unitless

      PETr = Divide( Non_dQs_dT * Rn
     :             , Denominator, 0.0)      ! J
     :             * 1000.0                 ! mm/m3  ????
     :             / Lambda                 ! J/kg
     :             / RhoW                   ! kg/m3


      PETa = Divide( RhoA * SpecificVPD * Ga
     :             , Denominator, 0.0)      ! kg/m3.kg/kg.m/s =
     :             * 1000.0                 ! m to mm ?
     :             * (DayLength *3600.0)    ! s
     :             / RhoW                   ! kg/m3

      Canopy_penman_monteith = PETr + PETa

      call pop_routine (myname)
      return
      end

*====================================================================
      real function Canopy_PETr
     :              (
     :               Rn
     :              ,minT
     :              ,maxT
     :              ,Air_Pressure
     :              ,Ga
     :              ,Gc
     :              )
*====================================================================
      Use CanopyModule
      use MicrometScienceModule
      use ComponentInterfaceModule
      implicit none

*+  Sub-Program Arguments
      real Rn
      real MinT
      real MaxT
      real Air_Pressure
      real Ga
      real Gc

*+  Purpose
*     Calculate the radiation-driven term for the Penman-Monteith
*     water demand

*+  Notes

*+  Changes
*       270500 - NIH specified and programmed

*+  Calls


*+  Local Variables
      REAL AverageT
      REAL Non_dQs_dT
      REAL Lambda             ! J/kg
      REAL denominator         !of the Penman-Monteith equation

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Canopy_PETr')

*- Implementation Section ----------------------------------

      call push_routine (myname)

      averageT = (mint + maxt)/2.0

      Non_dQs_dT = Canopy_Non_dQs_dT (averageT ,Air_Pressure)

      Lambda = Canopy_Lambda (AverageT)

      Denominator = Non_dQs_dT
     :            + Divide(Ga, Gc, 0.0)
     :            + 1.0                        ! unitless

      Canopy_PETr = Divide( Non_dQs_dT * Rn
     :                      , Denominator, 0.0)      ! J
     :                      * 1000.0                 ! mm/m3  ????
     :                      / Lambda                 ! J/kg
     :                      / RhoW                   ! kg/m3

      call pop_routine (myname)

      return
      end

*====================================================================
      real function Canopy_PETa
     :              (
     :               mint
     :              ,maxt
     :              ,vp
     :              ,Air_Pressure
     :              ,daylength
     :              ,Ga
     :              ,Gc
     :              )
*====================================================================
      Use CanopyModule
      use MicrometScienceModule
      use ComponentInterfaceModule
      implicit none

*+  Sub-Program Arguments
      real mint
      real maxt
      real vp
      real Air_Pressure
      real daylength
      real Ga
      real Gc

*+  Purpose
*     Calculate the aerodynamically-driven term for the Penman-Monteith
*     water demand

*+  Notes

*+  Changes
*       270500 - NIH specified and programmed

*+  Calls


*+  Local Variables
      REAL Non_dQs_dT
      REAL Lambda             ! J/kg
      REAL denominator         !of the Penman-Monteith equation
      REAL RhoA
      REAL SpecificVPD
      REAL averageT

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Canopy_PETa')

*- Implementation Section ----------------------------------

      call push_routine (myname)

      averageT = (mint + maxt)/2.0
      Non_dQs_dT = Canopy_Non_dQs_dT (averageT ,Air_Pressure)
      RhoA = Canopy_RhoA (averageT, Air_Pressure)
      Lambda = Canopy_Lambda (AverageT)

      SpecificVPD = Canopy_SpecificVPD ( vp
     :                                   , mint
     :                                   , maxt
     :                                   , Air_Pressure)

      Denominator = Non_dQs_dT
     :            + Divide( Ga, Gc, 0.0)
     :            + 1.0                        ! unitless

      Canopy_PETa = Divide( RhoA * SpecificVPD * Ga
     :                      , Denominator, 0.0)      ! kg/m3.kg/kg.m/s =
     :                      * 1000.0                 ! m to mm ?
     :                      * (DayLength *3600.0)    ! s
     :                      / RhoW                   ! kg/m3

      call pop_routine (myname)

      return
      end

*     ===========================================================
      subroutine Canopy_Energy_Balance_Event ()
*     ===========================================================
      use CanopyModule
      use ComponentInterfaceModule
      implicit none

*+  Purpose
*       Send an energy balance event

*+  Mission Statement
*       Send an energy balance event

*+  Changes
*     NIH 30/5/00 Specified

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Canopy_Energy_Balance_Event')

*+  Local Variables
      integer j
      integer i
      type (LightProfileType)::profile
      integer layer

*- Implementation Section ----------------------------------
      call push_routine (myname)

      profile%NumInterceptions = g%NumComponents

      do 100 j=1,g%NumComponents

         profile%interception(j)%name = g%canopies(j)%name
         profile%interception(j)%CropType = g%canopies(j)%CropType
         profile%interception(j)%NumLayers = g%NumLayers

         do 50 i = 1,g%NumLayers
            profile%interception(j)%layer(i)%thickness
     :                       = g%DeltaZ(i)
            profile%interception(j)%layer(i)%amount
     :                       = g%Rs(i,j)
   50    continue
  100 continue
      profile%transmission = g%met%radn - sum(g%Rs(:,:))

      call publish_LightProfile(id%LightProfileCalculated
     :            ,profile,.false.)

      call pop_routine (myname)
      return
      end

*     ===========================================================
      subroutine Canopy_Water_Balance_Event ()
*     ===========================================================
      use CanopyModule
      use ComponentInterfaceModule
      implicit none

*+  Purpose
*       Send a canopy water balance event

*+  Mission Statement
*       Send a canopy water balance event

*+  Changes
*     NIH 30/5/00 Specified

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Canopy_Water_Balance_Event')

*+  Local Variables
      integer j
      integer i
      type (CanopyWaterBalanceType)::CanopyWaterBalance
      integer layer

*- Implementation Section ----------------------------------
      call push_routine (myname)

      CanopyWaterBalance%NumCanopys = g%NumComponents

      do 100 j=1,g%NumComponents

         CanopyWaterBalance%canopy(j)%name = g%canopies(j)%name
         CanopyWaterBalance%canopy(j)%CropType = g%canopies(j)%CropType
         CanopyWaterBalance%canopy(j)%potentialEp
     :                 =  sum(g%PET(1:g%NumLayers,j))
  100 continue

      CanopyWaterBalance%interception = sum(g%Interception(:,:))
      CanopyWaterBalance%Eo = g%eo

      call publish_CanopyWaterBalance(id%CanopyWaterBalanceCalculated
     :            ,CanopyWaterBalance,.false.)

      call pop_routine (myname)
      return
      end

*     ===========================================================
      integer function Canopy_Component_Number (name)
*     ===========================================================
      use CanopyModule
      use ComponentInterfaceModule
      implicit none

*+  Sub-Program Arguments
      character name*(*)

*+  Purpose
*       Find record number for a given canopy name

*+  Mission Statement
*       Find record number for a given canopy name

*+  Changes
*     NIH 5/3/02 Specified

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Canopy_Component_Number')

*+  Local Variables
      integer    counter

*- Implementation Section ----------------------------------

      call push_routine (myname)

      Canopy_Component_Number = 0
      do 100 counter = 1, g%NumComponents
         if (g%Canopies(counter)%name.eq.name) then
            Canopy_Component_Number = counter
         else
         endif
  100 continue

      call pop_routine (myname)
      return
      end


*     ===========================================================
      subroutine Canopy_Reference_Et ()
*     ===========================================================
      use CanopyModule
      use ComponentInterfaceModule

      implicit none

*+  Sub-Program Arguments


*+  Purpose
*       calculate Reference(potential) evapotranspiration

*+  Notes


*+  Mission Statement
*     Calculate Reference(Potential) EvapoTranspiration

*+  Changes
*        190802   specified and programmed jngh (j hargreaves

*+  Local Variables
      real eo_system  ! value of Eo obtained from the comm. system
      logical found

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'Canopy_pot_evapotranspiration')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (p%eo_source .eq. blank) then
          call Canopy_priestly_taylor (g%eo) ! eo from priestly taylor
      else
          found = get_Eo(g%EoSourceId, Eo_system)
          if (.not.found) then
             ! said data is not available
             call error('Cannot find data for '//Trim(p%Eo_Source)
     :                 ,.true.)
          else
             g%eo = eo_system       ! eo is provided by system
          endif
      endif

      call pop_routine (my_name)
      end



*     ===========================================================
      subroutine Canopy_priestly_taylor (eo)
*     ===========================================================
      use CanopyModule
      use ComponentInterfaceModule

      implicit none

*+  Sub-Program Arguments
      real       eo            ! (output) potential evapotranspiration

*+  Purpose
*       calculate potential evapotranspiration via priestly-taylor

*+  Mission Statement
*       Calculate potential evapotranspiration using priestly-taylor method

*+  Changes
*        190802 NIH taken from soilwat2 module code


*+  Calls
      real       Canopy_eeq_fac       ! function

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'Canopy_priestly_taylor')

*+  Local Variables
      real       eeq                   ! equilibrium evaporation rate (mm)
      real       wt_ave_temp           ! weighted mean temperature for the
                                       !    day (oC)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

*  ******* calculate potential evaporation from soil surface (eos) ******

                ! find equilibrium evap rate as a
                ! function of radiation, albedo, and temp.

                ! wt_ave_temp is mean temp, weighted towards max.

      wt_ave_temp = 0.60*g%met%maxt + 0.40*g%met%mint

      eeq = g%met%radn*23.8846* (0.000204 - 0.000183*g%albedo)
     :    * (wt_ave_temp + 29.0)

                ! find potential evapotranspiration (eo)
                ! from equilibrium evap rate

      eo = eeq*Canopy_eeq_fac ()

      call pop_routine (my_name)
      return
      end



*     ===========================================================
      real function Canopy_eeq_fac ()
*     ===========================================================
      use CanopyModule
      use ComponentInterfaceModule

      implicit none

*+  Purpose
*    calculate coefficient for equilibrium evaporation rate

*+  Mission Statement
*     Calculate the Equilibrium Evaporation Rate

*+  Changes
*        190802 NIH Taken from soilwat2 module

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'Canopy_eeq_fac')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (g%met%maxt.gt.c%max_crit_temp) then

                ! at very high max temps eo/eeq increases
                ! beyond its normal value of 1.1

         Canopy_eeq_fac = ((g%met%maxt - c%max_crit_temp)*0.05 + 1.1)
      else if (g%met%maxt.lt.c%min_crit_temp) then

                ! at very low max temperatures eo/eeq
                ! decreases below its normal value of 1.1
                ! note that there is a discontinuity at tmax = 5
                ! it would be better at tmax = 6.1, or change the
                ! .18 to .188 or change the 20 to 21.1

         Canopy_eeq_fac = 0.01*exp (0.18* (g%met%maxt + 20.0))
      else

                ! temperature is in the normal range, eo/eeq = 1.1

         Canopy_eeq_fac = 1.1
      endif

      call pop_routine (my_name)
      return
      end

*     ===========================================================
      subroutine map(n,x,y,M,u,v)
*     ===========================================================
      implicit none
*+  Sub-Program Arguments
      integer N,M
      real x(*),y(*),u(*),v(*)

*+  Purpose
*     maps concentration in y into v so that integral is conserved
*     x and u give intervals corresponding to y and v values

*+  Mission Statement
*    Map array of amounts into new layer structure

*+  Changes
*        190802 NIH Taken from similar routine in swim module

*+  Local Variables
      double precision sx, sy, su, sv,w
      logical again
      integer i,j

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'Map')

*- Implementation Section ----------------------------------

      sx=0.
      sy=0.
      j=0
      su=u(1)
      sv=0.
      do 20 i=1,N
         sx=sx+x(i)
         sy=sy+y(i)
10       continue
            again=.FALSE.
            if((j.lt.M).and.(sx.ge.su.or.i.eq.N))then
               j=j+1
               w=sy-max(0,(sx-su))/x(i)*y(i)
               v(j)=(w-sv)
               if(j.lt.M)then
                  su=su+u(j+1)
                  sv=w
                  again=.TRUE.
               end if
            end if
         if(again)go to 10
20    continue

      return
      end
      
*     ================================================================
      subroutine OnResidueChanged (variant)
*     ================================================================
      use CanopyModule
      use ComponentInterfaceModule
      implicit none

!+  Sub-Program Arguments
      integer, intent(in out) :: variant

*+  Purpose
*     Obtain new residue information

*+  Mission Statement
*     Obtain new residue information

*+  Changes
*     <insert here>

*+  Calls
      integer Get_Residue_Number

*+  Constant Values
      character*(*) my_name
      parameter (my_name = 'OnResidueChanged')

*+  Local Variables
      type (residuetype) :: Residues(max_residues)
      integer num_Residues
      integer Counter
      integer Residue_number

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call unpack_residue (variant, Residues, num_residues)

      do 100 counter = 1, num_residues
         residue_number = get_residue_number(Residues(counter)%name)

         if (residue_number.ne.0) then
            g%residues(residue_number) = Residues(counter)
         else
            g%num_residues = g%num_residues + 1
            g%residues(g%num_residues) = Residues(counter)
         endif

  100 continue

      call pop_routine (my_name)
      return
      end

*     ================================================================
      integer function Get_Residue_number (name)
*     ================================================================
      use CanopyModule
      use ComponentInterfaceModule
      implicit none

!+  Sub-Program Arguments
      character name*(*)

*+  Purpose
*     Find record number for a given residue name

*+  Mission Statement
*     Find record number for a given residue name

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) my_name
      parameter (my_name = 'Get_Residue_number')

*+  Local Variables
      integer counter

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      Get_Residue_number = 0

      do 100 counter = 1, g%num_residues
         if (g%residues(counter)%name.eq.name) then
            Get_Residue_number = counter
         else
         endif
  100 continue

      call pop_routine (my_name)
      return
      end
      