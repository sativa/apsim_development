*     ===========================================================
      character*(*) function solute_version ()
*     ===========================================================
      implicit none
      include 'error.pub'
 
*+  Purpose
*       return version number of solute module
 
*+  Mission Statement
*     Version Number
 
*+  Changes
*     <insert here>
 
*+  Constant Values
      character  version_number*(*)    ! version number of module
      parameter (version_number = 'V1.21 04/04/97')
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'solute_version')
 
*- Implementation Section ----------------------------------
 
      call push_routine (myname)
 
      solute_version = version_number
 
      call pop_routine (myname)
      return
      end
 
 
 
* ====================================================================
       subroutine APSIM_solute (Action, Data_string)
* ====================================================================
      implicit none
      dll_export apsim_solute
       include 'const.inc'             ! Global constant definitions
      include 'engine.pub'
      include 'error.pub'
 
*+  Sub-Program Arguments
       character Action*(*)            ! Message action to perform
       character Data_string*(*)       ! Message data
 
*+  Purpose
*      This routine is the interface between the main system and the
*      solute module.
 
*+  Mission Statement
*     Apsim Solute
 
*+  Changes
*     <insert here>
 
*+  Calls
*       include 'solute.inc'          ! solute common block
       character*20 solute_version   ! function
 
*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'apsim_solute')
 
*+  Local Variables
      character Module_name*32         ! name of this module
 
*- Implementation Section ----------------------------------
 
      call push_routine (myname)
 
      if (Action.eq.MES_Presence) then
         call get_current_module (Module_name)
         print*,Module_Name,' ',solute_version()
 
      else if (Action.eq.MES_Init) then
         call solute_Init ()
 
      else if (Action.eq.MES_Process) then
         call solute_get_other_variables ()
 
      else if (Action.eq.MES_Get_variable) then
         call solute_Send_my_variable (Data_string)
 
      else if (Action.eq.MES_Set_variable) then
         call Solute_Set_my_variable (data_string)
 
      else
         ! Don't use message
         call Message_Unused ()
      endif
 
      call pop_routine (myname)
      return
      end
 
 
 
* ====================================================================
       subroutine solute_Init ()
* ====================================================================
      implicit none
       include 'const.inc'             ! Constant definitions
      include 'write.pub'
      include 'error.pub'
 
*+  Purpose
*      Initialise solute module
 
*+  Mission Statement
*     Initialise all internal state variables
 
*+  Changes
*     <insert here>
 
*+  Calls
*       include 'solute.inc'            ! solute model common
       character solute_version*15     ! function
 
*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'solute_init')
 
*+  Local Variables
       character Event_string*40       ! String to output
 
*- Implementation Section ----------------------------------
 
      call push_routine (myname)
 
      call solute_zero_variables ()
 
      call solute_get_other_variables ()
 
      ! Notify system that we have initialised
 
      Event_string = 'Initialising Version : ' // solute_version()
      call report_event (Event_string)
 
      ! Get all parameters from parameter file
 
      call solute_read_constants ()
 
      call solute_read_param ()
 
 
      call pop_routine (myname)
      return
      end
 
 
 
* ====================================================================
       subroutine solute_zero_variables ()
* ====================================================================
      implicit none
       include 'solute.inc'          ! solute common block
      include 'error.pub'
 
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
 
      do 200 solnum = 1, max_solutes
         do 100 layer = 1, max_layer
            g_solute(solnum,layer) = 0.0
  100    continue
         p_solute_names(solnum) = ' '
  200 continue
 
      call pop_routine (myname)
      return
      end
 
 
 
* ====================================================================
       subroutine solute_get_other_variables ()
* ====================================================================
      implicit none
       include 'const.inc'             ! Constant definitions
       include 'solute.inc'            ! solute common block
      include 'intrface.pub'
      include 'error.pub'
 
*+  Purpose
*      Get the values of variables from other modules
 
*+  Mission Statement
*     Get external state values from other modules
 
*+  Changes
*     <insert here>
 
*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'solute_get_other_variables')
 
*+  Local Variables
       integer numvals              ! number of values returned
 
*- Implementation Section ----------------------------------
 
      call push_routine (myname)
 
      call Get_real_array (
     :      unknown_module, ! Module that responds (Not Used)
     :      'dlayer',       ! Variable Name
     :      max_layer,      ! Array Size
     :      '(mm)',         ! Units                (Not Used)
     :      g_dlayer,       ! Variable
     :      numvals,        ! Number of values returned
     :      0,              ! Lower Limit for bound checking
     :      1000.)          ! Upper Limit for bound checking
 
 
      call pop_routine (myname)
      return
      end
 
 
 
* ====================================================================
       subroutine solute_Send_my_variable (Variable_name)
* ====================================================================
      implicit none
       include 'const.inc'             ! constant definitions
       include 'solute.inc'            ! solute Common block
      include 'data.pub'
      include 'engine.pub'
      include 'intrface.pub'
      include 'error.pub'
 
*+  Sub-Program Arguments
       character Variable_name*(*)     ! (INPUT) Variable name to search for
 
*+  Purpose
*       Return the value of one of our variables to caller.  The
*       variable is either the solute names or the solute information.
*       Solute information is stored in a two dimensional array
*       so for requested solute, read layer information into a
*       single dimension array and send to the system.
 
*+  Mission Statement
*     Supply information to requesting module
 
*+  Changes
*    130596 NIH added check for num_layers=0
 
*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'solute_send_my_variable')
 
*+  Local Variables
       integer layer
       integer num_layers
       integer solnum
       real sol(max_layer)
       logical found
 
*- Implementation Section ----------------------------------
 
      call push_routine (myname)
 
      if (variable_name .eq. 'solute_names') then
 
         call respond2get_char_array (
     :               variable_name,
     :               '()',
     :               p_solute_names,
     :               g_num_solutes)
 
      else
 
         found = .false.
 
         do 200 solnum = 1,g_num_solutes
 
            if (Variable_name .eq. p_solute_names(solnum)) then
 
               num_layers = count_of_real_vals(g_dlayer,max_layer)
 
               if (num_layers.eq.0) then
                  ! water balance is not initialised yet
                  num_layers = 1
               else
               endif
 
               do 100 layer = 1,max_layer
                  sol(layer) = g_solute(solnum,layer)
  100          continue
 
               call respond2get_real_array (
     :               p_solute_names(solnum),
     :               '(kg/ha)',
     :               sol,
     :               num_layers)
 
               found = .true.
 
            else
 
            endif
  200    continue
 
         if (.not. found) then
            ! We have checked all solutes and we did not respond to anything
            call Message_Unused ()
         else
            ! we found the solute so no message unused flag needed
         endif
 
      endif
 
      call pop_routine (myname)
      return
      end
 
 
 
*     ===========================================================
      subroutine solute_read_param ()
*     ===========================================================
      implicit none
      include 'const.inc'              ! new_line, lu_scr_sum, blank
      include 'solute.inc'
      include 'read.pub'
      include 'error.pub'
 
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
 
*- Implementation Section ----------------------------------
 
      call push_routine (myname)
 
      ! Read in solute name from parameter file
      !         -----------
         call read_char_array (
     :           section_name,        ! Section header
     :           'solute_names',      ! Keyword
     :           max_solutes,         ! array size
     :           '()',                ! Units
     :           p_solute_names,      ! Array
     :           g_num_solutes)       ! Number of values returned
 
 
      do 200 solnum = 1, g_num_solutes
 
 
         if (p_solute_names(solnum).ne.blank) then
 
      !     Read in solute in profile from parameter file
      !             -----------------
            call read_real_array (
     :           section_name,          ! Section header
     :           p_solute_names(solnum),! Keyword
     :           max_layer,             ! array size
     :           '()',                  ! Units
     :           sol,                   ! Array
     :           numvals,               ! Number of values returned
     :           c_lb_solute,           ! Lower Limit for bound checking
     :           c_ub_solute)           ! Upper Limit for bound checking
 
            do 100 layer = 1, numvals
               g_solute(solnum,layer) = sol(layer)
  100       continue
         else
            ! solute is blank so ignore it.
         endif
  200 continue
 
      call pop_routine (myname)
      return
      end
 
 
 
* ====================================================================
       subroutine solute_set_my_variable (Variable_name)
* ====================================================================
      implicit none
      include 'const.inc'
      include 'solute.inc'             ! solute common block
      include 'intrface.pub'
      include 'engine.pub'
      include 'error.pub'
 
*+  Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name to search for
 
*+  Purpose
*       Set one of our variables altered by some other module.
*       Solute information is stored in a two dimensional array
*       so for desired solute, read updated layer information into a
*       single dimension array and update into the two dimensional
*       array.
 
*+  Mission Statement
*     Set an internal variable as requested
 
*+  Changes
*    070696 nih changed respond2set calls to collect calls
 
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
      if (g_num_solutes .eq. 0) then
         call Message_Unused ()
      else
         found = .false.
 
         do 200 solnum = 1, g_num_solutes
 
            if (Variable_name .eq. p_solute_names(solnum)) then
 
               call collect_real_array (
     :                Variable_name,       ! variable name
     :                max_layer,           ! array size
     :                '(kg/ha)',           ! units
     :                sol,                 ! array
     :                numvals,             ! number of elements returned
     :                c_lb_solute,         ! lower bound
     :                c_ub_solute)         ! upper bound
 
               do 100 layer = 1, numvals
                  g_solute (solnum,layer) = sol(layer)
  100          continue
               found = .true.
 
            elseif (Variable_name .eq. 'dlt_'//
     :                p_solute_names(solnum)) then
 
               call collect_real_array (
     :                Variable_name,        ! variable name
     :                max_layer,
     :                '(kg/ha)',           ! units
     :                dlt_sol,             ! array
     :                numvals,             ! number of elements returned
     :                -c_ub_solute,
     :                c_ub_solute)
 
               do 150 layer = 1, numvals
                  g_solute (solnum,layer) = g_solute(solnum,layer)
     :                               + dlt_sol(layer)
  150          continue
               found = .true.
            else
               ! Don't know this variable name
            endif
 
  200    continue
 
         if (.not. found) then
            call Message_Unused ()
         else
            ! we found the variable so no message unused flag needed
         endif
 
      endif
 
      call pop_routine (myname)
      return
      end
 
 
 
* ====================================================================
       subroutine solute_read_constants ()
* ====================================================================
      implicit none
      include 'const.inc'
      include 'solute.inc'             ! solute common block
      include 'read.pub'
      include 'write.pub'
      include 'error.pub'
 
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
 
*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      call write_string (lu_scr_sum
     :                 ,new_line//'   - Reading Constants')
 
 
      call read_real_var (
     :           section_name         ! Section header
     :         , 'ub_solute'          ! Keyword
     :         , '()'                 ! Units
     :         , c_ub_solute          ! Variable
     :         , numvals              ! Number of values returned
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 1E10)                ! Upper Limit for bound checking
 
      call read_real_var (
     :           section_name         ! Section header
     :         , 'lb_solute'          ! Keyword
     :         , '()'                 ! Units
     :         , c_lb_solute          ! Variable
     :         , numvals              ! Number of values returned
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 1E10)                ! Upper Limit for bound checking
 
 
      call pop_routine (myname)
      return
      end
 
 
 
