      include 'Micromet.inc'
!     ===========================================================
      subroutine AllocInstance (InstanceName, InstanceNo)
!     ===========================================================
      use MicrometModule
      implicit none
 
!+  Sub-Program Arguments
      character InstanceName*(*)       ! (INPUT) name of instance
      integer   InstanceNo             ! (INPUT) instance number to allocate
 
!+  Purpose
!      Module instantiation routine.
 
!- Implementation Section ----------------------------------
               
      allocate (Instances(InstanceNo)%gptr)
      allocate (Instances(InstanceNo)%pptr)
      allocate (Instances(InstanceNo)%cptr)
      Instances(InstanceNo)%Name = InstanceName
 
      return
      end

!     ===========================================================
      subroutine FreeInstance (anInstanceNo)
!     ===========================================================
      use MicrometModule
      implicit none
 
!+  Sub-Program Arguments
      integer anInstanceNo             ! (INPUT) instance number to allocate
 
!+  Purpose
!      Module de-instantiation routine.
 
!- Implementation Section ----------------------------------
               
      deallocate (Instances(anInstanceNo)%gptr)
      deallocate (Instances(anInstanceNo)%pptr)
      deallocate (Instances(anInstanceNo)%cptr)
 
      return
      end
     
!     ===========================================================
      subroutine SwapInstance (anInstanceNo)
!     ===========================================================
      use MicrometModule
      implicit none
 
!+  Sub-Program Arguments
      integer anInstanceNo             ! (INPUT) instance number to allocate
 
!+  Purpose
!      Swap an instance into the global 'g' pointer
 
!- Implementation Section ----------------------------------
               
      g => Instances(anInstanceNo)%gptr
      p => Instances(anInstanceNo)%pptr
      c => Instances(anInstanceNo)%cptr
 
      return
      end

* ====================================================================
       subroutine Main (Action, Data_string)
* ====================================================================
      use MicrometModule
      implicit none
      include 'const.inc'             ! Global constant definitions
      include 'action.inc'
      include 'event.inc'
      include 'error.pub'
 
*+  Sub-Program Arguments
       character Action*(*)            ! Message action to perform
       character Data_string*(*)       ! Message data
 
*+  Purpose
*      This routine is the interface between the main system and the
*      Micromet module.
 
*+  Mission Statement
*     Apsim Micromet
 
*+  Changes
*     NIH 28/3/00 Specified
 
*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet Main')
 
*- Implementation Section ----------------------------------
 
      call push_routine (myname)
 
      if (Action.eq.ACTION_Init) then
         call Micromet_Init ()

      elseif (Action.eq.ACTION_Prepare) then
         call Micromet_Prepare ()

      elseif (Action.eq.Event_Tick) then
         call Micromet_OnTick ()

      elseif (Action.eq.Event_NewMet) then
         call Micromet_OnNewMet ()

      elseif (Action.eq.ACTION_Process) then
         call Micromet_Process ()
 
      else if (Action.eq.'new_crop') then
         call Micromet_OnNewCrop ()

      else if (Action.eq.'new_canopy') then
         call Micromet_OnNewCanopy ()

      else if (Action.eq.'new_pot_growth') then
         call Micromet_OnNewPotGrowth ()
 
      else if (Action.eq.ACTION_Get_variable) then
         call Micromet_Send_my_variable (Data_string)

      else if (Action.eq.'lai_table') then
         call Micromet_table ('LAI',g%LAI)
      else if (Action.eq.'f_table') then
         call Micromet_table ('F',g%F)
      else if (Action.eq.'rs_table') then
         call Micromet_table ('Rs',g%Rs)
      else if (Action.eq.'rl_table') then
         call Micromet_table ('Rl',g%Rl)
      else if (Action.eq.'gc_table') then
         call Micromet_table ('Gc',g%Gc)
      else if (Action.eq.'ga_table') then
         call Micromet_table ('Ga',g%Ga)
      else if (Action.eq.'pet_table') then
         call Micromet_table ('PET',g%PET)
      else if (Action.eq.'petr_table') then
         call Micromet_table ('PETr',g%PETr)
      else if (Action.eq.'peta_table') then
         call Micromet_table ('PETa',g%PETa)
      else if (Action.eq.'omega_table') then
         call Micromet_table ('Omega',g%Omega)
         
      else
         ! Don't use message
         call Message_Unused ()
      endif
 
      call pop_routine (myname)
      return
      end
 
 
 
* ====================================================================
       subroutine Micromet_Init ()
* ====================================================================
      use MicrometModule
      implicit none
      include 'const.inc'             ! Constant definitions
      include 'error.pub'
 
*+  Purpose
*      Initialise Micromet module
 
*+  Mission Statement
*     Initialise all internal state variables
 
*+  Changes
*     NIH 28/3/00 Specified
 
*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_init')
 
*+  Local Variables
       character Event_string*40       ! String to output
 
*- Implementation Section ----------------------------------
 
      call push_routine (myname)
 
      call Micromet_zero_variables ()
 
      ! Notify system that we have initialised
 
      Event_string = 'Initialising'
      call Write_string (Event_string)
 
      ! Get all parameters from parameter file
 
      call Micromet_read_constants ()
 
      call Micromet_read_param ()

      call pop_routine (myname)
      return
      end
 
 
 
* ====================================================================
       subroutine Micromet_zero_variables ()
* ====================================================================
      use MicrometModule
      implicit none
      include 'error.pub'
 
*+  Purpose
*     Set all variables to initial state.  i.e. zero or blank.
 
*+  Mission Statement
*     Set internal state variables to zero
 
*+  Changes
*     NIH 28/3/00 Specified
 
*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_zero_variables')
 
*+  Local Variables
 
*- Implementation Section ----------------------------------
 
      call push_routine (myname)

      g%NumComponents = 0
      g%ComponentName(:) = ' '
      g%ComponentType(:) = ' '
      g%ComponentLAI(:) = 0.0
      g%ComponentCover(:) = 0.0
      g%ComponentK(:) = 0.0
      g%ComponentHeight(:) = 0.0
      g%ComponentDepth(:) = 0.0
      g%ComponentGsmax(:) = 0.0
      g%ComponentR50(:) = 0.0
      g%ComponentAlbedo(:) = 0.0
      g%ComponentEmissivity(:) = 0.0
      g%ComponentFrgr(:) = 0.0

      g%DeltaZ(:) = 0.0
      g%NumLayers = 0

      g%LayerK(:) = 0.0

      g%LAI(:,:) = 0.0
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


      g%radn = 0.0
      g%maxt = 0.0
      g%mint = 0.0
      g%rain = 0.0
      g%vp = 0.0

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
 
* ====================================================================
       subroutine Micromet_Send_my_variable (Variable_name)
* ====================================================================
      use MicrometModule
      implicit none
      include 'const.inc'             ! constant definitions
      include 'data.pub'
      include 'intrface.pub'
      include 'error.pub'
 
*+  Sub-Program Arguments
       character Variable_name*(*)     ! (INPUT) Variable name to search for
 
*+  Purpose
*       Return the value of one of our variables to caller. 
 
*+  Mission Statement
*     Supply information to requesting module
 
*+  Changes
*     NIH 28/3/00 Specified
 
*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_send_my_variable')
 
*+  Local Variables
      real total_lai
      real total_interception
      integer i
      integer j
 
*- Implementation Section ----------------------------------
 
      call push_routine (myname)
 
      if (Variable_name.eq.'interception') then

      Total_Interception = 0.0

      do 200 i = 1, g%NumLayers
         do 100 j = 1, g%NumComponents
            Total_Interception = Total_Interception 
     :                         + g%Interception(i,j)
  100    continue
  200 continue

         call respond2get_real_var (
     :               variable_name       ! variable name
     :              ,'(mm)'              ! variable units
     :              ,Total_Interception) ! variable

!      elseif
!
!      call collect_char_var (DATA_sender
!     :                      ,'()'
!     :                      ,sender
!     :                      ,numvals)
!
!      ComponentNo = position_in_char_array 
!     :                   (sender
!     :                   ,g%ComponentName
!     :                   ,g%NumComponents)
!
!      if (ComponentNo.eq.0) then
!         call fatal_Error(ERR_Internal
!     :                   ,'Unknown Canopy Component: '//sender)
!



      else
         call Message_Unused ()
      endif
 
      call pop_routine (myname)
      return
      end
 
 
 
*     ===========================================================
      subroutine Micromet_read_param ()
*     ===========================================================
      use MicrometModule
      implicit none
      include 'const.inc'              ! new_line, lu_scr_sum, blank
      include 'read.pub'
      include 'error.pub'
 
*+  Purpose
*       Read in all parameters from parameter file.  
 
*+  Mission Statement
*     Read parameters from parameter file
 
*+  Changes
*     NIH 28/3/00 Specified
 
*+  Calls

 
*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_read_param')
*
      character section_name*(*)
      parameter (section_name = 'parameters')
 
*+  Local Variables
      integer    numvals               ! number of values read
 
*- Implementation Section ----------------------------------
 
      call push_routine (myname)
 
      call write_string (new_line//'   - Reading Parameters')

      call read_real_var (
     :        section_name,          ! Section header
     :        'soil_albedo',         ! Keyword
     :        '()',                  ! Units
     :        p%soil_albedo,         ! Parameter
     :        numvals,               ! Number of values returned
     :        0.0,                   ! Lower Limit for bound checking
     :        1.0)                   ! Upper Limit for bound checking
 
      call read_real_var (
     :        section_name,          ! Section header
     :        'layer_ga',            ! Keyword
     :        '(m/s)',               ! Units
     :        p%layer_ga,            ! Parameter
     :        numvals,               ! Number of values returned
     :        0.0,                   ! Lower Limit for bound checking
     :        1.0)                   ! Upper Limit for bound checking

      call read_real_var_optional (
     :        section_name,          ! Section header
     :        'a_interception',      ! Keyword
     :        '(mm/mm)',             ! Units
     :        p%a_interception,      ! Parameter
     :        numvals,               ! Number of values returned
     :        0.0,                   ! Lower Limit for bound checking
     :        10.0)                  ! Upper Limit for bound checking
      if(numvals.eq.0) then
         p%a_interception = 0.0
      else
      endif 

      call read_real_var_optional (
     :        section_name,          ! Section header
     :        'b_interception',      ! Keyword
     :        '()',                  ! Units
     :        p%b_interception,      ! Parameter
     :        numvals,               ! Number of values returned
     :        0.0,                   ! Lower Limit for bound checking
     :        5.0)                   ! Upper Limit for bound checking
      if(numvals.eq.0) then
         p%b_interception = 1.0
      else
      endif 

      call read_real_var_optional (
     :        section_name,          ! Section header
     :        'c_interception',      ! Keyword
     :        '(mm)',                ! Units
     :        p%c_interception,      ! Parameter
     :        numvals,               ! Number of values returned
     :        0.0,                   ! Lower Limit for bound checking
     :        10.0)                  ! Upper Limit for bound checking
      if(numvals.eq.0) then
         p%c_interception = 0.0
      else
      endif 

      call read_real_var_optional (
     :        section_name,          ! Section header
     :        'd_interception',      ! Keyword
     :        '(mm)',                ! Units
     :        p%d_interception,      ! Parameter
     :        numvals,               ! Number of values returned
     :        0.0,                   ! Lower Limit for bound checking
     :        20.0)                  ! Upper Limit for bound checking
      if(numvals.eq.0) then
         p%d_interception = 0.0
      else
      endif 

      call pop_routine (myname)
      return
      end
 
 
 
* ====================================================================
       subroutine Micromet_read_constants ()
* ====================================================================
      use MicrometModule
      implicit none
      include 'const.inc'
      include 'read.pub'
      include 'error.pub'
 
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
      parameter (myname = 'Micromet_read_constants')
 
*+  Local Variables
      integer    numvals               ! number of values read from file
 
*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      call write_string (new_line//'   - Reading Constants')
  
      call read_real_var (
     :           section_name         ! Section header
     :         , 'air_pressure'       ! Keyword
     :         , '(hPa)'              ! Units
     :         , c%air_pressure       ! Variable
     :         , numvals              ! Number of values returned
     :         , 900.                 ! Lower Limit for bound checking
     :         , 1100.)               ! Upper Limit for bound checking

      call read_real_var (
     :           section_name         ! Section header
     :         , 'soil_emissivity'    ! Keyword
     :         , '()'                 ! Units
     :         , c%soil_emissivity    ! Variable
     :         , numvals              ! Number of values returned
     :         , 0.9                  ! Lower Limit for bound checking
     :         , 1.0)                 ! Upper Limit for bound checking
 
      call pop_routine (myname)
      return
      end
 
 
*     ===========================================================
      subroutine Micromet_OnNewCrop ()
*     ===========================================================
      use MicrometModule
      implicit none
      include 'const.inc'
      include 'event.inc'
      include 'error.pub'
      include 'intrface.pub'
 
*+  Purpose
*       Register presence of a new crop
 
*+  Mission Statement
*       Register presence of a new crop
 
*+  Changes
*     NIH 30/3/00 Specified
 
*+  Calls

 
*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_OnNewCrop')
 
*+  Local Variables
      integer    numvals               ! number of values read
 
*- Implementation Section ----------------------------------
 
      call push_routine (myname)

      g%NumComponents = g%NumComponents + 1
 
      if (g%NumComponents.gt.max_components) then
         call fatal_Error(ERR_Internal
     :                   ,'Too many canopy components in system')

      else

         call collect_char_var (DATA_sender
     :                         ,'()'
     :                         ,g%ComponentName(g%NumComponents)
     :                         ,numvals)

         call collect_char_var ('crop_type'
     :                         ,'()'
     :                         ,g%ComponentType(g%NumComponents)
     :                         ,numvals)
 
         ! Read Component Specific Constants 
         ! ---------------------------------
         call micromet_component_constants(g%NumComponents)
    
      endif
 
      call pop_routine (myname)
      return
      end
 
*     ===========================================================
      subroutine Micromet_OnNewCanopy ()
*     ===========================================================
      use MicrometModule
      implicit none
      include 'const.inc'
      include 'event.inc'
      include 'error.pub'
      include 'intrface.pub'
      include 'Data.pub'
 
*+  Purpose
*       Obtain updated information about a plant canopy
 
*+  Mission Statement
*       Obtain updated information about a plant canopy
 
*+  Changes
*     NIH 30/3/00 Specified
 
*+  Calls

 
*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_OnNewCanopy')
 
*+  Local Variables
      integer    numvals               ! number of values read
      character  sender*32
      integer    ComponentNo

*- Implementation Section ----------------------------------
 
      call push_routine (myname)

      call collect_char_var (DATA_sender
     :                      ,'()'
     :                      ,sender
     :                      ,numvals)

      ComponentNo = position_in_char_array 
     :                   (sender
     :                   ,g%ComponentName
     :                   ,g%NumComponents)

      if (ComponentNo.eq.0) then
         call fatal_Error(ERR_Internal
     :                   ,'Unknown Canopy Component: '//sender)

      else

         call collect_real_var ('lai'
     :                         ,'()'
     :                         ,g%ComponentLAI(ComponentNo)
     :                         ,numvals
     :                         ,0.0
     :                         ,20.0)

         call collect_real_var ('cover'
     :                         ,'()'
     :                         ,g%ComponentCover(ComponentNo)
     :                         ,numvals
     :                         ,0.0
     :                         ,1.0)

         call collect_real_var ('height'
     :                         ,'()'
     :                         ,g%ComponentHeight(ComponentNo)
     :                         ,numvals
     :                         ,0.0
     :                         ,100000.0)

         call collect_real_var ('depth'
     :                         ,'()'
     :                         ,g%ComponentDepth(ComponentNo)
     :                         ,numvals
     :                         ,0.0
     :                         ,100000.0)

      endif
 
      call pop_routine (myname)
      return
      end
 
*     ===========================================================
      subroutine Micromet_Prepare ()
*     ===========================================================
      use MicrometModule
      implicit none
      include 'error.pub'
 
*+  Purpose
*       Perform Prepare Phase Calculations
 
*+  Mission Statement
*       Perform Prepare Phase Calculations
 
*+  Changes
*     NIH 30/3/00 Specified
 
*+  Calls

 
*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_Prepare')
 
*+  Local Variables
 
*- Implementation Section ----------------------------------
 
      call push_routine (myname)

      call Micromet_Met_Variables ()
      call Micromet_Canopy_Compartments ()
      call Micromet_Canopy_Energy_Balance ()

      call pop_routine (myname)
      return
      end

*     ===========================================================
      subroutine Micromet_Process ()
*     ===========================================================
      use MicrometModule
      implicit none
      include 'error.pub'
 
*+  Purpose
*       Perform Process Phase Calculations
 
*+  Mission Statement
*       Perform Process Phase Calculations
 
*+  Changes
*     NIH 30/3/00 Specified
 
*+  Calls

 
*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_Process')
 
*+  Local Variables
 
*- Implementation Section ----------------------------------
 
      call push_routine (myname)

      call Micromet_Calculate_Gc ()
      call Micromet_Calculate_Ga ()
      call Micromet_Calculate_Interception ()
      call Micromet_Calculate_PM()
      call Micromet_Calculate_Omega()


      call Micromet_Energy_Balance_Event()
      call Micromet_Water_Balance_Event()

      call pop_routine (myname)
      return
      end

*     ===========================================================
      subroutine Micromet_Canopy_Compartments ()
*     ===========================================================
      use MicrometModule
      implicit none
      include 'error.pub'
 
*+  Purpose
*       Break the combined Canopy into functional compartments
 
*+  Mission Statement
*       Break the combined Canopy into functional compartments
 
*+  Changes
*     NIH 30/3/00 Specified
 
*+  Calls

 
*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_Canopy_Compartments')
 
*+  Local Variables
 
*- Implementation Section ----------------------------------
 
      call push_routine (myname)

      Call Micromet_Define_Layers ()

      Call Micromet_Divide_Components ()

      Call Micromet_Light_Extinction()

      call pop_routine (myname)
      return
      end

*     ===========================================================
      subroutine Micromet_Define_Layers ()
*     ===========================================================
      use MicrometModule
      implicit none
      include 'error.pub'
      include 'data.pub'
      include 'science.pub'
 
*+  Purpose
*       Break the combined Canopy into layers
 
*+  Mission Statement
*       Break the combined Canopy into layers

*+  Changes
*     NIH 30/3/00 Specified
 
*+  Calls

 
*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_Define_Layers')
 
*+  Local Variables
      real nodes(2*max_components - 1) 
      integer NumNodes
      integer ComponentNo
      integer Node
      real    CanopyBase
      integer key(2*max_components - 1) 

*- Implementation Section ----------------------------------
 
      call push_routine (myname)

      nodes(:) = 0.0
      NumNodes = 0

      do 100 ComponentNo = 1, g%NumComponents
         if (position_in_real_array(g%ComponentHeight(ComponentNo)
     :                             ,Nodes
     :                             ,NumNodes)
     :       .eq.0) then
            NumNodes = NumNodes + 1
            Nodes(NumNodes) = g%ComponentHeight(ComponentNo)

         else
            ! it is already there - ignore it
         endif      

         CanopyBase = g%ComponentHeight(ComponentNo)
     :              - g%ComponentDepth(ComponentNo)

         if (position_in_real_array(CanopyBase
     :                             ,Nodes
     :                             ,NumNodes)
     :       .eq.0) then
            NumNodes = NumNodes + 1
            Nodes(NumNodes) = CanopyBase
         else
            ! it is already there - ignore it
         endif      
      
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
      subroutine Micromet_Divide_Components ()
*     ===========================================================
      use MicrometModule
      implicit none
      include 'error.pub'
      include 'data.pub'
      include 'science.pub'
 
*+  Purpose
*       Break the components into layers
 
*+  Mission Statement
*       Break the components into layers

*+  Changes
*     NIH 30/3/00 Specified
 
*+  Calls

 
*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_Divide_Components')
 
*+  Local Variables
      real Ld (max_components)
      real top
      real bottom
      integer i
      integer j

*- Implementation Section ----------------------------------
 
      call push_routine (myname)

      g%LAI(:,:) = 0.0

      do 100 j = 1, g%NumComponents
         Ld(j) = divide(g%ComponentLAI(j)
     :                 ,g%ComponentDepth(j)
     :                 ,0.0)
  100 continue

      top = 0.0
      bottom = 0.0
      do 300 i = 1, g%NumLayers
         bottom = top
         top = top + g%DeltaZ(i)

         ! Calculate LAI for layer i and component j
         ! =========================================

         do 200 j = 1, g%NumComponents
            if ((g%ComponentHeight(j).gt.bottom)
     :                 .and.
     :          (g%ComponentHeight(j)-g%ComponentDepth(j).lt.top))then
               g%LAI(i,j) = Ld(j) * g%DeltaZ(i)
            else
               ! This component is not in this layer
            endif

  200    continue

         ! Calculate fractional contribution for layer i and component j
         ! =============================================================

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
      subroutine Micromet_Table (Title,Array)
*     ===========================================================
      use MicrometModule
      implicit none
      include 'error.pub'
      include 'data.pub'
      include 'science.pub'

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
      parameter (myname = 'Micromet_Table')
 
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
     :      ,(g%ComponentName(j),j=1,g%NumComponents)
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

         write(string,'(x,f7.3,'' - '',f7.3,5x,11f10.3)')
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
      subroutine Micromet_Light_Extinction ()
*     ===========================================================
      use MicrometModule
      implicit none
      include 'error.pub'
      include 'data.pub'
      include 'science.pub'
 
*+  Purpose
*       Calculate light extinction parameters
 
*+  Mission Statement
*       Calculate light extinction parameters

*+  Changes
*     NIH 30/3/00 Specified
 
*+  Calls

 
*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_Light_Extinction')
 
*+  Local Variables
      integer i
      integer j

*- Implementation Section ----------------------------------
 
      call push_routine (myname)

         ! Calculate effective K from LAI and Cover
         ! ========================================


      do 100 j = 1, g%NumComponents

         g%ComponentK(j) = divide(-log(1.-g%ComponentCover(j))
     :                           ,g%ComponentLAI(j)
     :                           ,0.0)

  100 continue

         ! Calculate extinction for individual layers
         ! ==========================================


      do 200 i = 1, g%NumLayers

         g%LayerK(i) = Sum(g%F(i,1:g%NumComponents)
     :                     * g%ComponentK(1:g%NumComponents))

  200 continue

      call pop_routine (myname)
      return
      end

*     ===========================================================
      subroutine Micromet_Canopy_Energy_Balance ()
*     ===========================================================
      use MicrometModule
      implicit none
      include 'error.pub'
 
*+  Purpose
*       Perform the Overall Canopy Energy Balance 

*+  Mission Statement
*       Perform the Overall Canopy Energy Balance 
 
*+  Changes
*     NIH 30/3/00 Specified
 
*+  Calls

 
*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_Canopy_Energy_Balance')
 
*+  Local Variables
 
*- Implementation Section ----------------------------------
 
      call push_routine (myname)

      Call Micromet_short_wave_radiation ()
      Call Micromet_Energy_Terms ()
      Call Micromet_Long_Wave_Radiation ()

      call pop_routine (myname)
      return
      end

*     ===========================================================
      subroutine Micromet_short_wave_radiation ()
*     ===========================================================
      use MicrometModule
      implicit none
      include 'error.pub'
      include 'data.pub'
      include 'science.pub'
 
*+  Purpose
*       Calculate interception of short wave by canopy compartments 

*+  Mission Statement
*       Calculate interception of short wave by canopy compartments

*+  Changes
*     NIH 30/3/00 Specified
 
*+  Calls

 
*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_short_wave_radiation')
 
*+  Local Variables
      integer i
      integer j
      real    Rin
      real    Rint
*- Implementation Section ----------------------------------
 
      call push_routine (myname)

         ! Perform Top-Down Light Balance
         ! ==============================

      Rin = g%Radn

      do 200 i = g%NumLayers,1,-1

         Rint = Rin 
     :        * (1. - exp(-g%LayerK(i)
     :                    *sum(g%LAI(i,1:g%NumComponents))))

         do 100 j = 1, g%NumComponents
            g%Rs(i,j) = Rint 
     :                * divide(g%F(i,j)*g%ComponentK(j)
     :                        ,g%LayerK(i)
     :                        ,0.0)
  100    continue

         Rin = Rin - Rint

  200 continue

      call pop_routine (myname)
      return
      end


*     ===========================================================
      subroutine Micromet_OnNewMet ()
*     ===========================================================
      use MicrometModule
      implicit none
      include 'error.pub'
      include 'event.inc'
      include 'event.pub'
 
*+  Purpose
*       Obtain all relevant met data

*+  Mission Statement
*       Obtain all relevant met data
 
*+  Changes
*     NIH 30/3/00 Specified
 
*+  Calls

 
*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_OnNewMet')
 
*+  Local Variables
 
*- Implementation Section ----------------------------------
 
      call push_routine (myname)

      call handler_ONnewmet(g%radn, g%maxt, g%mint, g%rain, g%vp)

      call pop_routine (myname)
      return
      end

*     ===========================================================
      subroutine Micromet_Long_Wave_Radiation ()
*     ===========================================================
      use MicrometModule
      implicit none
      include 'error.pub'
      include 'micromet.pub'
      include 'data.pub'
      include 'convert.inc'
 
*+  Purpose
*       Calculate Net Long Wave Radiation Balance 

*+  Mission Statement
*       Calculate Net Long Wave Radiation Balance

*+  Changes
*     NIH 30/3/00 Specified
 
*+  Calls

 
*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_Long_Wave_Radiation')
 
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

      Net_Long_Wave = micromet_longwave(g%AverageT
     :                                  ,FractionClearSky
     :                                  ,g%Emissivity)
     :              * day2hr * hr2s / 1.0e6  ! W to MJ


         ! Long Wave Balance Proportional to Short Wave Balance
         ! ====================================================

      do 200 i = g%NumLayers,1,-1


         do 100 j = 1, g%NumComponents
            g%Rl(i,j) = divide(g%Rs(i,j)
     :                        ,g%Radn
     :                        ,0.0)
     :                * Net_Long_Wave
  100    continue


  200 continue

      call pop_routine (myname)
      return
      end

*     ===========================================================
      subroutine Micromet_Met_Variables ()
*     ===========================================================
      use MicrometModule
      implicit none
      include 'error.pub'
      include 'micromet.pub'
      include 'data.pub'
 
*+  Purpose
*       Calculate Daily Met Variables

*+  Mission Statement
*       Calculate Daily Met Variables

*+  Changes
*     NIH 30/3/00 Specified
 
*+  Calls

 
*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_Met_Variables')
 
*+  Local Variables

*- Implementation Section ----------------------------------
 
      call push_routine (myname)


      g%Daylength = Micromet_DayLength(g%latitude,g%day)

      g%AverageT = (g%maxt + g%mint)/2.0

      g%SunshineHours = Micromet_Sunshine_Hours(g%Radn
     :                                         ,g%latitude
     :                                         ,g%day)


      call pop_routine (myname)
      return
      end

*     ===========================================================
      subroutine micromet_ONtick ()
*     ===========================================================
      use MicrometModule
      implicit none
      include 'error.pub'
      include 'event.pub'
 
*+  Purpose
*     Update internal time record and reset daily state variables.
 
*+  Mission Statement
*     Update internal time record and reset daily state variables.
 
*+  Changes
*        140400 nih 

*+  Local Variables
      character temp1*5
      integer   temp2
 
*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'micromet_ONtick')
 
*- Implementation Section ----------------------------------
      call push_routine (myname)

      ! Note that time and timestep information is not required
      ! and so dummy variables are used in their place.

      call handler_ONtick(g%day, g%year, temp1, temp2)

      call pop_routine (myname)
      return
      end

*     ===========================================================
      subroutine Micromet_Energy_Terms ()
*     ===========================================================
      use MicrometModule
      implicit none
      include 'error.pub'
      include 'data.pub'
      include 'convert.inc'
 
*+  Purpose
*       Calculate the overall system energy terms

*+  Mission Statement
*       Calculate the overall system energy terms

*+  Changes
*     NIH 14/4/00 Specified
 
*+  Calls

 
*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_Energy_Terms')
 
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
     :                       ,g%Radn
     :                       ,0.0)
     :                   * g%ComponentAlbedo(j)
            g%Emissivity = g%Emissivity
     :               + divide(g%Rs(i,j)
     :                       ,g%Radn
     :                       ,0.0)
     :                   * g%ComponentEmissivity(j)

  100    continue

  200 continue

      g%albedo = g%albedo 
     :         + (1. - divide(sum(g%Rs(:,:))
     :                       ,g%Radn
     :                       ,0.0))
     :             * p%Soil_Albedo

      g%Emissivity = g%Emissivity
     :         + (1. - divide(sum(g%Rs(:,:))
     :                       ,g%Radn
     :                       ,0.0))
     :             * c%Soil_Emissivity

      call pop_routine (myname)
      return
      end

*     ===========================================================
      subroutine Micromet_component_constants (Cno)
*     ===========================================================
      use MicrometModule
      implicit none
      include 'const.inc'
      include 'event.inc'
      include 'error.pub'
      include 'intrface.pub'
      include 'read.pub'
      include 'crp_util.pub'  

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
      parameter (myname = 'Micromet_component_constants')
 
*+  Local Variables
      integer    numvals               ! number of values read
      character  search_order(max_table)*32 ! sections to search
      integer    num_sections          ! number of sections to search
 
*- Implementation Section ----------------------------------
 
      call push_routine (myname)

      ! Find search order for component constants
      ! -----------------------------------------

      call read_char_array ('constants'
     :                     , g%ComponentType(Cno)
     :                     , max_table
     :                     , '()'
     :                     , search_order
     :                     , num_sections)
 

         ! Read Component Specific Constants 
         ! ---------------------------------
         ! (should be in dedicated routine)

         call search_read_real_var (
     :           search_order
     :         , num_sections
     :         , 'albedo'             ! Keyword
     :         , '(0-1)'              ! Units
     :         , g%ComponentAlbedo(Cno)  ! Variable
     :         , numvals              ! Number of values returned
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 1.0)                 ! Upper Limit for bound checking

         call search_read_real_var (
     :           search_order
     :         , num_sections
     :         , 'emissivity'         ! Keyword
     :         , '(0-1)'              ! Units
     :         , g%ComponentEmissivity(Cno)  ! Variable
     :         , numvals              ! Number of values returned
     :         , 0.9                  ! Lower Limit for bound checking
     :         , 1.0)                 ! Upper Limit for bound checking

         call search_read_real_var (
     :           search_order
     :         , num_sections
     :         , 'gsmax'              ! Keyword
     :         , '(m/s)'              ! Units
     :         , g%ComponentGsmax(Cno)  ! Variable
     :         , numvals              ! Number of values returned
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 1.0)                 ! Upper Limit for bound checking
    
         call search_read_real_var (
     :           search_order
     :         , num_sections
     :         , 'r50'                ! Keyword
     :         , '(W/m2)'             ! Units
     :         , g%ComponentR50(Cno)  ! Variable
     :         , numvals              ! Number of values returned
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 1e3)                 ! Upper Limit for bound checking

 
      call pop_routine (myname)
      return
      end

*     ===========================================================
      subroutine Micromet_Calculate_Gc ()
*     ===========================================================
      use MicrometModule
      implicit none
      include 'error.pub'
      include 'data.pub'
      include 'convert.inc'
      include 'micromet.pub'
 
*+  Purpose
*       Calculate the canopy conductance for system compartments

*+  Mission Statement
*       Calculate the canopy conductance for system compartments

*+  Changes
*     NIH 19/4/00 Specified
 
*+  Calls

 
*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_Calculate_Gc')
 
*+  Local Variables
      integer i
      integer j
      real    layerLAI
      real    Rin
      real    Rint
      real    Rflux

*- Implementation Section ----------------------------------
 
      call push_routine (myname)

      Rin = g%Radn

      do 200 i = g%NumLayers,1,-1
         LayerLAI = sum(g%LAI(i,:))

         Rflux = Rin * 10**6 / (g%DayLength *3600.0) ! should use convert.inc
     :         * (1. - g%albedo)

         do 100 j = 1, g%NumComponents
            g%Gc(i,j) = micromet_CanopyConductance
     :                    (g%ComponentGsmax(j)
     :                    ,g%ComponentR50(j)
     :                    ,g%ComponentFrgr(j)
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
      subroutine Micromet_Calculate_Ga ()
*     ===========================================================
      use MicrometModule
      implicit none
      include 'error.pub'
      include 'data.pub'
      include 'convert.inc'
      include 'micromet.pub'
 
*+  Purpose
*       Calculate the aerodynamic conductance for system compartments

*+  Mission Statement
*       Calculate the aerodynamic conductance for system compartments

*+  Changes
*     NIH 30/5/00 Specified
 
*+  Calls

      real       micromet_AerodynamicCondNew          ! function
      dll_import micromet_AerodynamicCondNew
 
      real       micromet_AerodynamicCondSub          ! function
      dll_import micromet_AerodynamicCondSub
 
*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_Calculate_Ga')
 
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
!            layer_ga = micromet_AerodynamicCondNew (
!     :                         WindSpeed
!     :                       , MetHeight
!     :                       , CropHeight
!     :                       , CropLAI
!     :                       , Meas_ZeroPlane)
            layer_ga = p%layer_ga
         else
!            layer_ga = micromet_AerodynamicCondSub (
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
      subroutine Micromet_Calculate_Interception ()
*     ===========================================================
      use MicrometModule
      implicit none
      include 'error.pub'
      include   'data.pub'                         
 
*+  Purpose
*       Calculate the interception loss of water from the canopy

*+  Mission Statement
*       Calculate the interception loss of water from the canopy

*+  Changes
*     NIH 23/8/00 Specified
 
*+  Calls

 
*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_Calculate_Interception')
 
*+  Local Variables
      real Total_LAI
      real Total_Interception
      integer i
      integer j

*- Implementation Section ----------------------------------
 
      call push_routine (myname)

      Total_LAI = sum(g%LAI(:,:))

      Total_Interception = p%A_interception * g%rain**p%B_interception
     :               + p%C_interception * Total_LAI
     :               + p%D_interception

      Total_Interception = bound(Total_Interception, 0.0, 0.99*g%Rain)

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
      subroutine Micromet_Calculate_Omega ()
*     ===========================================================
      use MicrometModule
      implicit none
      include 'error.pub'
      include 'micromet.pub'
 
*+  Purpose
*       Calculate the aerodynamic decoupling for system compartments

*+  Mission Statement
*       Calculate the aerodynamic decoupling for system compartments

*+  Changes
*     NIH 30/5/00 Specified
 
*+  Calls

 
*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_Calculate_Omega')
 
*+  Local Variables
      integer i
      integer j

*- Implementation Section ----------------------------------
 
      call push_routine (myname)

      do 200 i = 1, g%NumLayers
         do 100 j = 1, g%NumComponents

            g%Omega(i,j) = micromet_omega(g%mint
     :                                   ,g%maxt
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
      subroutine micromet_calculate_PM ()
*====================================================================
      Use MicrometModule
      implicit none
      include   'const.inc'
      include   'data.pub'                         
      include   'science.pub'
      include   'error.pub'                         
      include   'micromet.pub'

*+  Sub-Program Arguments

*+  Purpose
*     Calculate the Penman-Monteith water demand

*+  Notes

*+  Changes
*       270500 - NIH specified and programmed

*+  Calls
      REAL micromet_PETa
      REAL micromet_PETr
      REAL micromet_Penman_Monteith

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
      parameter (myname = 'micromet_calculate_PM')

*- Implementation Section ----------------------------------
 
      call push_routine (myname)

      NetRadiation = ((1. - g%Albedo) * sum(g%Rs(:,:))+ sum(g%Rl(:,:)))
     :             * 1e6        ! MJ/J

      Free_Evap_Ga = p%layer_ga 
      Free_Evap_Gc = Free_Evap_Ga * 1e6  !=infinite surface conductance

      Free_Evap = micromet_Penman_Monteith
     :              (
     :               NetRadiation
     :              ,g%mint
     :              ,g%maxt
     :              ,g%vp
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

      averageT = (g%mint + g%maxt)/2.0
      Lambda = micromet_Lambda (AverageT)

      do 200 i = 1, g%NumLayers
         do 100 j = 1, g%NumComponents

            NetRadiation = ((1. - g%Albedo) * g%Rs(i,j)+ g%Rl(i,j))
     :             * 1e6        ! MJ/J

            g%PETr(i,j) = micromet_PETr
     :              (
     :               NetRadiation * Dry_Leaf_Fraction
     :              ,g%mint
     :              ,g%maxt
     :              ,c%Air_Pressure
     :              ,g%Ga(i,j)
     :              ,g%Gc(i,j)
     :              )

            g%PETa(i,j) = micromet_PETa
     :                 (
     :                  g%mint
     :                 ,g%maxt
     :                 ,g%vp
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
      real function micromet_Penman_Monteith
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
      Use MicrometModule
      implicit none
      include   'data.pub'                         
      include   'science.pub'
      include   'error.pub'                         

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

      real       micromet_Non_dQs_dT       ! function
      dll_import micromet_Non_dQs_dT

      real       micromet_Lambda       ! function
      dll_import micromet_Lambda

      real       micromet_SpecificVPD
      dll_import micromet_SpecificVPD

      real       micromet_RhoA
      dll_import micromet_RhoA

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
      parameter (myname = 'micromet_Penman_Monteith')

*- Implementation Section ----------------------------------
 
      call push_routine (myname)

      averageT = (mint + maxt)/2.0
      Non_dQs_dT = micromet_Non_dQs_dT (averageT ,Air_Pressure)
      RhoA = micromet_RhoA (averageT, Air_Pressure)
      Lambda = micromet_Lambda (AverageT)

      SpecificVPD = micromet_SpecificVPD ( vp
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

      micromet_penman_monteith = PETr + PETa

      call pop_routine (myname)
      return
      end

*====================================================================
      real function micromet_PETr
     :              (
     :               Rn
     :              ,minT
     :              ,maxT
     :              ,Air_Pressure
     :              ,Ga
     :              ,Gc
     :              )
*====================================================================
      Use MicrometModule
      implicit none
      include   'data.pub'                         
      include   'science.pub'
      include   'error.pub'                         

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

      real       micromet_Non_dQs_dT       ! function
      dll_import micromet_Non_dQs_dT

      real       micromet_Lambda       ! function
      dll_import micromet_Lambda

*+  Local Variables
      REAL AverageT
      REAL Non_dQs_dT
      REAL Lambda             ! J/kg
      REAL denominator         !of the Penman-Monteith equation

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_PETr')

*- Implementation Section ----------------------------------
 
      call push_routine (myname)

      averageT = (mint + maxt)/2.0

      Non_dQs_dT = micromet_Non_dQs_dT (averageT ,Air_Pressure)

      Lambda = micromet_Lambda (AverageT)

      Denominator = Non_dQs_dT
     :            + Divide(Ga, Gc, 0.0)
     :            + 1.0                        ! unitless

      micromet_PETr = Divide( Non_dQs_dT * Rn
     :                      , Denominator, 0.0)      ! J
     :                      * 1000.0                 ! mm/m3  ????
     :                      / Lambda                 ! J/kg
     :                      / RhoW                   ! kg/m3

      call pop_routine (myname)

      return
      end

*====================================================================
      real function micromet_PETa
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
      Use MicrometModule
      implicit none
      include   'data.pub'                         
      include   'science.pub'
      include   'error.pub'                         

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

      real       micromet_Non_dQs_dT       ! function
      dll_import micromet_Non_dQs_dT

      real       micromet_Lambda       ! function
      dll_import micromet_Lambda

      real       micromet_SpecificVPD
      dll_import micromet_SpecificVPD

      real       micromet_RhoA
      dll_import micromet_RhoA

*+  Local Variables
      REAL Non_dQs_dT
      REAL Lambda             ! J/kg
      REAL denominator         !of the Penman-Monteith equation
      REAL RhoA
      REAL SpecificVPD
      REAL averageT

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_PETa')

*- Implementation Section ----------------------------------
 
      call push_routine (myname)

      averageT = (mint + maxt)/2.0
      Non_dQs_dT = micromet_Non_dQs_dT (averageT ,Air_Pressure)
      RhoA = micromet_RhoA (averageT, Air_Pressure)
      Lambda = micromet_Lambda (AverageT)

      SpecificVPD = micromet_SpecificVPD ( vp
     :                                   , mint
     :                                   , maxt
     :                                   , Air_Pressure)

      Denominator = Non_dQs_dT
     :            + Divide( Ga, Gc, 0.0)
     :            + 1.0                        ! unitless

      micromet_PETa = Divide( RhoA * SpecificVPD * Ga 
     :                      , Denominator, 0.0)      ! kg/m3.kg/kg.m/s = 
     :                      * 1000.0                 ! m to mm ?
     :                      * (DayLength *3600.0)    ! s
     :                      / RhoW                   ! kg/m3

      call pop_routine (myname)

      return
      end

*     ===========================================================
      subroutine Micromet_Energy_Balance_Event ()
*     ===========================================================
      use MicrometModule
      implicit none
      include 'const.inc'
      include 'error.pub'
      include 'postbox.pub'
      include 'intrface.pub'
 
*+  Purpose
*       Send an energy balance event
 
*+  Mission Statement
*       Send an energy balance event
 
*+  Changes
*     NIH 30/5/00 Specified
 
*+  Calls

 
*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_Energy_Balance_Event')
 
*+  Local Variables
      integer j
 
*- Implementation Section ----------------------------------
      call push_routine (myname)

      call new_postbox()

      do 100 j=1,g%NumComponents

         call post_real_var ('int_radn_'//Trim(g%ComponentName(j))
     :                      , '(MJ)'
     :                      , sum(g%Rs(1:g%NumLayers,j)))

  100 continue

      call event_send ('canopy_energy_balance')

      call delete_postbox() 

      call pop_routine (myname)
      return
      end
 
*     ===========================================================
      subroutine Micromet_Water_Balance_Event ()
*     ===========================================================
      use MicrometModule
      implicit none
      include 'const.inc'
      include 'error.pub'
      include 'postbox.pub'
      include 'intrface.pub'
 
*+  Purpose
*       Send a canopy water balance event
 
*+  Mission Statement
*       Send a canopy water balance event
 
*+  Changes
*     NIH 30/5/00 Specified
 
*+  Calls

 
*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_Water_Balance_Event')
 
*+  Local Variables
      integer j
 
*- Implementation Section ----------------------------------
      call push_routine (myname)

      call new_postbox()

      do 100 j=1,g%NumComponents

         call post_real_var ('pet_'//Trim(g%ComponentName(j))
     :                      , '(mm)'
     :                      , sum(g%PET(1:g%NumLayers,j)))

  100 continue

      call post_real_var ('interception'
     :                   , '(mm)'
     :                   , sum(g%Interception(:,:)))

      call event_send ('canopy_water_balance')

      call delete_postbox() 

      call pop_routine (myname)
      return
      end

*     ===========================================================
      subroutine Micromet_OnNewPotGrowth ()
*     ===========================================================
      use MicrometModule
      implicit none
      include 'const.inc'
      include 'event.inc'
      include 'error.pub'
      include 'intrface.pub'
      include 'Data.pub'
 
*+  Purpose
*       Obtain updated information about a plant's growth capacity
 
*+  Mission Statement
*       Obtain updated information about a plant's growth capacity
 
*+  Changes
*     NIH 1/6/00 Specified
 
*+  Calls

 
*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_OnNewPotGrowth')
 
*+  Local Variables
      integer    numvals               ! number of values read
      character  sender*32
      integer    ComponentNo

*- Implementation Section ----------------------------------
 
      call push_routine (myname)

      call collect_char_var (DATA_sender
     :                      ,'()'
     :                      ,sender
     :                      ,numvals)

      ComponentNo = position_in_char_array 
     :                   (sender
     :                   ,g%ComponentName
     :                   ,g%NumComponents)

      if (ComponentNo.eq.0) then
         call fatal_Error(ERR_Internal
     :                   ,'Unknown Canopy Component: '//sender)

      else

         call collect_real_var ('frgr'
     :                         ,'()'
     :                         ,g%ComponentFrgr(ComponentNo)
     :                         ,numvals
     :                         ,0.0
     :                         ,1.0)
     
      endif
 
      call pop_routine (myname)
      return
      end
