      include 'Ozcot.inc'

!     ===========================================================
      subroutine AllocInstance (InstanceName, InstanceNo)
!     ===========================================================
      use OzcotModule
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
      use OzcotModule
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
      use OzcotModule
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
       subroutine Main (action, data_string)
* ====================================================================
      use OzcotModule
      implicit none
       include 'action.inc'
       include 'const.inc'             ! Global constant definitions
       include 'error.pub'

*+  Sub-Program Arguments
      character Action*(*)            ! Message action to perform
      character data_string*(*)

*+  Purpose
*      This routine is the interface between the main system and the
*      ozcot module.

*+  Changes
*      psc - 9/08/93
*      DPH - 11/7/94 Modifed routine to zero variables if required
*                    when a process message is received.
*      PdeV  16/3/95 New engine interface
*      jngh  170895  changed manager action to react to sow and harvest actions
*      jngh  250996  added version to presence report
*                    added message_unused call
*      sdb   060599  removed version reference and presence action

*+  Calls
      logical    ozcot_my_type         ! function

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'Ozcot_main')

*- Implementation Section ----------------------------------
      call push_routine(myname)

      if (Action.eq.ACTION_Get_variable) then
         call ozcot_Send_my_variable (data_string)

      else if (Action .eq. ACTION_Set_variable) then
         call ozcot_set_my_variable (data_string)

      else if (Action .eq. ACTION_prepare) then
         call ozcot_prepare ()

      else if (Action.eq.ACTION_Process) then
         if (g%zero_variables) then
            call ozcot_zero_variables()
            call ozcot_init()

         else
            ! No need to zero variables.
         endif

         call ozcot_get_other_variables ()
         call ozcot_Process ()
         call ozcot_set_other_variables ()

      else if (Action .eq. ACTION_Post) then
         call ozcot_post ()

!jh      else if (Action .eq. 'sow' .or. action .eq. 'harvest') then
!jh         call ozcot_manager (Action, data_string)

      elseif (action.eq.ACTION_sow) then
         if (ozcot_my_type ()) then
               ! request and receive variables from owner-modules
!jh            call ozcot_get_other_variables ()
               ! start crop and do  more initialisations
            call ozcot_start_crop ()
         else
            ! not my type!
            call message_unused ()
         endif
 
      elseif (action.eq.ACTION_harvest) then
         if (ozcot_my_type ()) then
               ! harvest crop - turn into residue
              call ozcot_end_crop ()
         else
            ! not my type!
            call message_unused ()
         endif
 
      else if (Action .eq. ACTION_End_run) then
         call ozcot_end_run ()

      elseif (Action.eq.ACTION_init) then
         call ozcot_zero_variables ()
         call ozcot_get_other_variables ()
         call ozcot_Init ()

      else if (Action.eq.ACTION_Create) then
         call ozcot_zero_all_globals ()
!cjh         open (100, 'out.txt')
 
      else
         ! Don't use message
         call message_unused ()

      endif

      call pop_routine(myname)
      return
      end




* ====================================================================
       subroutine ozcot_Init ()
* ====================================================================
      use OzcotModule
      implicit none
      include 'error.pub'

*+  Purpose
*      Initialise ozcot module

*+  Changes
*      psc - 9/08/93
*      250996 jngh removed unused includes
*      060599 sdb removed version reference

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_init')

*+  Local Variables
       character Event_string*40       ! String to output

*- Implementation Section ----------------------------------
      call push_routine(myname)
      ! Notify system that we have initialised

      Event_string = 'Initialising'
      call Write_string (Event_string)

      ! Get all parameters from parameter file

      call ozcot_read_constants ()
!jh      call ozcot_read_param ()
      call ozcot_read_root_params ()
!cpsc      call init()                      ! now called from o_zero_variables
!jh      call ozcot_initial()

      call pop_routine(myname)
      return
      end



!obsolete * ====================================================================
!obsolete        subroutine ozcot_read_param ()
!obsolete * ====================================================================
!obsolete       use OzcotModule
!obsolete       implicit none
!obsolete        include 'const.inc'             ! Constant definitions
!obsolete       include 'read.pub'
!obsolete       include 'error.pub'
!obsolete 
!obsolete *+  Purpose
!obsolete *      Read in all parameters from parameter file.
!obsolete 
!obsolete *+  Changes
!obsolete *      psc - 09/08/93 first go
!obsolete *      psc - 30/03/94 specified properly
!obsolete *      DPH - 7/7/94  Removed free format internal read to g%title.  Line now
!obsolete *                    reads g%title = param_string
!obsolete *      psc - 15/4/98 Add ll, remove g%title, g%asoil from read
!obsolete *      jngh - 30/4/98 kept numvals of ll read as global
!obsolete *                     made reading of ll optional with a warning error if not found
!obsolete *                    as ll15 will then be used.
!obsolete 
!obsolete *+  Constant Values
!obsolete       character  myname*(*)            ! name of subroutine
!obsolete       parameter (myname = 'ozcot_read_param')
!obsolete       character  section_name*(*)
!obsolete       parameter (section_name = 'parameters')
!obsolete 
!obsolete *+  Local Variables
!obsolete !       integer numvals
!obsolete 
!obsolete *- Implementation Section ----------------------------------
!obsolete       call push_routine(myname)
!obsolete !         ! read in title from parameter file
!obsolete !      call read_char_array (section_name
!obsolete !     :                     , 'title', 15, '()'
!obsolete !     :                     , title, numvals)
!obsolete 
!obsolete !         ! read in soil temperature factor from parameter file
!obsolete !      call read_real_var (section_name
!obsolete !     :                    , 'asoil', '()'
!obsolete !     :                     , asoil, numvals
!obsolete !     :                     , 0.0, 10000.0)
!obsolete 
!obsolete       call read_real_array_optional (section_name
!obsolete      :                     , 'll', max_layers, '(mm/mm)'
!obsolete      :                     , p%unul, p%num_ll_vals
!obsolete      :                     , 0.0, 1.0)
!obsolete 
!obsolete       if (p%num_ll_vals.ne.0) then
!obsolete          ! LL found
!obsolete       else
!obsolete          ! LL not found
!obsolete          call warning_error (err_user
!obsolete      :         , ' Cotton LL not found. Using Soilwat LL15 instead.' )
!obsolete       endif
!obsolete 
!obsolete       call pop_routine(myname)
!obsolete       return
!obsolete       end

*     ===========================================================
      subroutine ozcot_zero_all_globals ()
*     ===========================================================
      use ozcotModule
      implicit none
      include 'error.pub'                         

*+  Purpose
*       Zero all global variables & arrays

*+  Changes
*     041199 jngh

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'ozcot_zero_all_globals')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      g%TITLE  = ' '
!jh      g%DAY(:)   = 0.0
!jh      g%HR(:)    = 0.0
      g%TEMPMX   = 0.0
      g%TEMPMN   = 0.0
      g%SOLRAD   = 0.0
      g%RAIN     = 0.0
!jh      g%EPAN     = 0.0
      c%HUCUT    = 0.0
      c%BASET    = 0.0
      g%TEMPDY   = 0.0
      g%TEMPWT   = 0.0
      g%WIND     = 0.0
      g%TEMPAV   = 0.0
      g%tempre   = 0.0
      g%tempyd   = 0.0
      g%tmean3   = 0.0
      g%HUNITS   = 0.0
      g%ASOIL    = 0.0
      g%EOS      = 0.0
      g%QA       = 0.0
      g%SOLRTO   = 0.0
      g%Q        = 0.0
!jh      g%SUMES1   = 0.0
!jh      g%SUMES2   = 0.0
      g%EO       = 0.0
      g%ES       = 0.0
      g%EP       = 0.0
      g%ET       = 0.0
      g%HO       = 0.0
      g%G        = 0.0
      g%TR       = 0.0
      g%RRIG(:) = 0.0
      g%RTSW     = 0.0
      g%DEFIRG   = 0.0
!jh      c%AMBDA    = 0.0
      g%VPD      = 0.0
      g%BPER     = 0.0
      g%dlayr(:)            = 0.0
      g%dlayr_cm(:)         = 0.0
      g%ULLAYR(:)           = 0.0
      g%STLAYR(:)           = 0.0
      g%SWLAYR(:)           = 0.0
      g%sw_start(:)         = 0.0
      g%SW                  = 0.0
      g%UL                  = 0.0
      g%sat                 = 0.0
      c%UL1                 = 0.0
      g%BULKD(:)            = 0.0
      g%STEMP               = 0.0
      g%TRANS(:)            = 0.0
      g%DEF                 = 0.0
      g%WPWC                = 0.0
      g%WHCSUB              = 0.0
      g%ULSUB               = 0.0
      g%AVSWSM              = 0.0
      g%TSWL(:)             = 0.0
      g%SETLYR(:)           = 0.0
      g%ESUM                = 0.0
      g%ALAI                = 0.0
      g%RTDEP               = 0.0
      g%RTGROW              = 0.0
      g%CRSPCE              = 0.0
      g%PPM                 = 0.0
      g%SDEPTH              = 0.0
      g%RTDEPM              = 0.0
      g%SHEDLF              = 0.0
      g%SMI                 = 0.0
      g%S                   = 0.0
      g%RS                  = 0.0
      g%PP                  = 0.0
      g%PS                  = 0.0
      g%FLL                 = 0.0
      g%AVAILN              = 0.0
      g%UPTAKN              = 0.0
      g%VEGN                = 0.0
      g%FRUN                = 0.0
      g%PLANTN              = 0.0
      g%SEED_NC             = 0.0
      g%STRUCN              = 0.0
      g%FRUCAT(:)           = 0.0
      g%DAYSQZ              = 0.0
      g%DAYSFL              = 0.0
      g%DAYSOP              = 0.0
      g%FMKCAT(:,:)         = 0.0
      g%DD                  = 0.0
      g%DDMERG              = 0.0
      g%SUMDD               = 0.0
      g%BGRVAR              = 0.0
      g%FRUDW               = 0.0
      g%SQUARZ              = 0.0
      g%BOLLZ               = 0.0
      g%OPENZ               = 0.0
      g%SITES               = 0.0
      g%sites1              = 0.0
      g%SIZE                = 0.0
      g%BLOAD               = 0.0
      g%OPENWT              = 0.0
      p%SQCON           = 0.0
      p%respcon         = 0.0
      c%POPCON              = 0.0
      p%flai            = 0.0
      p%fcutout         = 0.0
      g%CARCAP              = 0.0
      g%CUTOUT              = 0.0
      g%VNSTRS              = 0.0
      g%FNSTRS              = 0.0
      g%RAD                 = 0.0
      g%PCLINT              = 0.0
      g%CARCAP_C            = 0.0
      g%CARCAP_N            = 0.0
      p%scboll          = 0.0
      c%FBURR               = 0.0
      g%FRUNO(:)          = 0.0
      g%FRUWT(:)          = 0.0
      g%FRMARK(:,:)       = 0.0
      g%FYZAGE(:)         = 0.0
      g%DLAI(:)           = 0.0
      g%ALAIZ               = 0.0
      g%PLNTNZ              = 0.0
      g%TWATER              = 0.0
      g%ALINT               = 0.0
      g%GROSS_MARG          = 0.0
      g%DEF_LAST            = 0.0
      g%SQZX                = 0.0
      c%OPEN_DEF            = 0.0
      g%AGRON_INP           = 0.0
      g%SOILW_INP           = 0.0
      g%COUNT_INP           = 0.0
      g%RAIN_INP            = 0.0
      g%MET_INP             = 0.0
      g%FCOT_OUT            = 0.0
      g%FRUCAL_OUT          = 0.0
      g%YIELD_OUT           = 0.0
!jh      c%SOW_SW              = 0.0
      g%s_bed_mi            = 0.0
      g%s_bed_sat           = 0.0
      g%delay               = 0.0
      g%bpsum(:)          = 0.0
      c%A_ROOT_LEAF         = 0.0
      c%A_STEM_LEAF         = 0.0
      g%BOLLGR              = 0.0
      g%DLAI_POT            = 0.0
      g%DW_BOLL             = 0.0
      g%DW_LEAF             = 0.0
      g%DW_ROOT             = 0.0
      g%DW_STEM             = 0.0
      g%DW_TOTAL            = 0.0
      g%RESERVE             = 0.0
      g%RES_CAP             = 0.0
      g%ROOT_FEEDBACK       = 0.0
      c%SPECIFIC_LW         = 0.0
      c%WT_AREA_MAX         = 0.0
!jh      c%WT_AREA_MIN         = 0.0
      g%dDW_L(:)          = 0.0
      c%e_par               = 0.0
      c%T_OPT               = 0.0
      c%T_BASE              = 0.0
      c%EMBRYO              = 0.0
      c%F_LEAF              = 0.0
      c%F_STEM              = 0.0
      c%F_ROOT              = 0.0
      g%dDW_BOLL            = 0.0
      g%dDW_LEAF            = 0.0
      g%dDW_ROOT            = 0.0
      g%dDW_STEM            = 0.0
      g%LEAF_RES            = 0.0
      g%STEM_RES            = 0.0
      g%ROOT_RES            = 0.0
      g%LEAF_RES_N          = 0.0
      g%STEM_RES_N          = 0.0
      g%ROOT_RES_N          = 0.0
      g%total_n             = 0.0
      g%dn_plant            = 0.0
      g%tsno3               = 0.0
      g%no3mn(:)            = 0.0
      g%yest_tsno3          = 0.0
      g%ano3(:)             = 0.0
      g%Last_Iday           = 0
!jh      c%MODE                = 0
      g%IMMO                = 0
      g%IMDY                = 0
      g%IMYR                = 0
      g%JDATE               = 0
      g%MDPY                = 0
      g%NLAYR               = 0
      g%ISW                 = 0
      g%IEMRG               = 0
      g%ISOW                = 0
      g%ISQ                 = 0
      g%IDATE               = 0
      g%ILAI                = 0
      g%IDAY                = 0
      g%LFRU(:)             = 0
      g%LAST                = 0
      g%NEXT                = 0
      g%LASTL               = 0
      g%NEXTL               = 0
      g%IDAYCO              = 0
      g%LAST_DAY            = 0
      g%ILAIZ               = 0
      g%IPLNTN              = 0
      g%NIRR                = 0
      g%ISQZX               = 0
      g%J_PICK              = 0
      g%N_PICK              = 0
      g%N_DEF               = 0
      g%I_DEF               = 0
      g%LAI_INP             = 0
!jh      c%IWINDOW             = 0
      g%das                 = 0
      g%iend                = 0
      g%idayx               = 0
      g%lastlf              = 0
      g%n_cutout            = 0
      g%ifrost              = 0
      g%istress             = 0
      g%ireliefco           = 0
      g%Crop_in             = .false.     ! Is a crop in ?
      g%Zero_variables      = .false.
      g%sfmcat(:)           = 0.0
      g%nsince = 0

      c%row_spacing_default           = 0.0
      c%elevation_default   = 0.0

      c%stress_wlog                = 0.0
      c%wlog_assimilate_red        = 0.0
      c%wlog_carcap_red            = 0.0
      c%wlog_carcap_red_stress     = 0.0
      c%smi_affect_wlog            = 0.0
      c%days_relief_wlog           = 0
      c%frost_kill_immediate       = 0.0
      c%frost_kill_immediate_das   = 0
      c%frost_kill_delayed         = 0.0
      c%frost_kill_delayed_das     = 0
      c%frost_kill_delayed_days    = 0
      c%rtdep_max                  = 0.0  
      c%harvest_n_frac             = 0.0
      c%cutout_smi_crit            = 0.0
      c%cutout_smi_days            = 0  
      c%cutout_smi_site_red        = 0.0  
      c%epcoef1                    = 0.0  
      c%epcoef2                    = 0.0  
      c%epcoef_smi_crit            = 0.0  
      c%fbwstr_low                 = 0.0
      c%fbwstr_high                = 0.0
      c%fbwstr_a                   = 0.0
      c%fbnstr_low                 = 0.0  
      c%fbnstr_high                = 0.0  
      c%fbnstr_a                   = 0.0  
      c%relp_smi_crit              = 0.0
      c%relp_intercept             = 0.0
      c%relp_slope                 = 0.0
      c%relp_low                   = 0.0  
      c%relp_high                  = 0.0  
      c%relp_a                     = 0.0  
      c%vsnstr_low                 = 0.0  
      c%vsnstr_high                = 0.0  
      c%vsnstr_a                   = 0.0  
      c%flfsmi_low                 = 0.0  
      c%flfsmi_high                = 0.0  
      c%flfsmi_a                   = 0.0  
      c%vlnstr_low                 = 0.0  
      c%vlnstr_high                = 0.0  
      c%vlnstr_a                   = 0.0  
      c%fw_low                     = 0.0  
      c%fw_high                    = 0.0  
      c%fw_a                       = 0.0  
      c%adjust_low                 = 0.0  
      c%adjust_high                = 0.0  
      c%adjust_a                   = 0.0  
      c%fwstrs_low                 = 0.0  
      c%fwstrs_high                = 0.0  
      c%fwstrs_a                   = 0.0
      c%smi_delay_crit               = 0.0
      c%cold_shock_delay_crit        = 0.0
      c%cold_shock_delay             = 0.0

      ! OzcotParameters                  
                                   
      p%UNUL(:)           = 0.0                             
      p%num_ll_vals       = 0
                                   
                                                                        
      call pop_routine (my_name)
      return
      end


* ====================================================================
       subroutine ozcot_zero_variables ()
* ====================================================================
      use OzcotModule
      implicit none
      include 'error.pub'

*+  Purpose
*     Set all variables in this module to zero.

*+  Changes
*      psc - 9/08/93

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_zero_variables')

*- Implementation Section ----------------------------------
      call push_routine(myname)

      call ozcot_initial()
      g%das = 0
      g%delay = 0.0
      g%idayx = 0
      p%num_ll_vals = 0
      g%crop_in = .false.
      g%zero_variables = .false.

      call pop_routine(myname)
      return
      end



* ====================================================================
       subroutine ozcot_manager (Event_action, event_data)
* ====================================================================
      use OzcotModule
      implicit none
      include 'const.inc'              ! global_active
      include 'intrface.pub'
      include 'error.pub'
      include 'postbox.pub'

*+  Sub-Program Arguments
      character Event_action*(*)       ! (INPUT) Action to be performed
      character Event_data*(*)         ! (INPUT) Data sent with event

*+  Purpose
*     The manager has sent an event to this module.  Process it.

*+  Notes
*     Event_action is the action specified in the management parameter
*     file.  e.g%g. 'sow'

*+  Changes
*      psc - 9/08/93
*      07/07/94 - jngh changed residue module reference to global_active
*      170895 jngh changed message send to message pass to module
*      250996 jngh changed to post_ construct
*      191200 dph  changed from unknown_module to all_active_modules
*                  unknown_module not supported in APSIM2.

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_manager')

*+  Local Variables
       real    res_dm                  ! Residue dry weight (kg/ha)
       real    res_N                   ! Amount of N in residue (kg/ha)

*- Implementation Section ----------------------------------
      call push_routine(myname)
      ! **** Repeat for each action

      if (Event_action .eq. 'sow') then

         g%crop_in = .true.

         ! Report the event to the rest of the system

         call Write_string (Event_action)

         call ozcot_sow (event_data)

      else if (Event_action .eq. 'harvest') then

         ! Report the event to the rest of the system

         call Write_string (Event_action)

         res_dm = (g%dw_total - g%openwt / g%rs ) * 10.
         if (res_dm.le.0.) res_dm = 0.
         res_N = res_dm * 0.4 / 100.0

         call New_postbox ()

         call post_char_var('dlt_residue_type','()','cotton')

         call post_real_var ('dlt_residue_wt'
     :                        ,'(kg/ha)'
     :                        ,res_dm)

         call post_real_var ('dlt_residue_n'
     :                        ,'(kg/ha)'
     :                        ,res_N)

         call Action_send (
     :                              All_active_modules
     :                            , 'add_residue'
     :                            , Blank
     :                            )

         call Delete_postbox ()

         g%crop_in = .false.
         g%zero_variables = .true.

      else
         ! Don't know about this event !!!

      endif


      call pop_routine(myname)
      return
      end



*     ===========================================================
      subroutine ozcot_sow (myrecd)
*     ===========================================================
      use OzcotModule
      implicit none
      include 'const.inc'              ! blank
      include 'error.pub'

*+  Sub-Program Arguments
      character  myrecd*(*)            ! (INPUT) message received

*+  Purpose
*       start crop using parameters specified in passed record

*+  Changes
*       300394 psc  taken from cm_sat module

*+  Constant Values
      character  myname*(*)            ! procedure name
      parameter (myname  = 'ozcot_sow')

*+  Local Variables
!cpsc      character  cv_name*20            ! name of cultivar
      character  string*300            ! output string

*- Implementation Section ----------------------------------

      call push_routine (myname)

      if (myrecd.ne.blank) then

         ! variety,seed depth,rowspace,plants per m row

         read (myrecd,*) g%ivar,g%sdepth,g%rs,g%ppm

         g%isow = g%jdate
           g%rtdep=g%sdepth
         g%ppm = g%ppm/g%rs  !  adjust for non standard rows incl skip
           g%pp= g%ppm*g%rs
           g%ps=(1.0/g%rs)/g%pp
           g%s=g%ps/g%rs
         g%rrig(sw_sowing) = g%sw               ! soil water at sowing
         g%iend = 1

             ! report

         write (string, '(a)')
     :                  ' sowing  depth plants row sp'
         call write_string (string)

         write (string, '(a)')
     :                  ' g%day no   mm     m^2    m  '
         call write_string (string)

         write (string, '(i7, 3f7.1, 1x, a10)')
     :                   g%isow, g%sdepth, g%pp, g%rs
         call write_string (string)

         call write_string (blank)

!cpcs                 ! get cultivar parameters

!cpsc         call cm_cultv (cv_name)

      else
            ! report empty sowing record
         call fatal_error (err_user, 'No sowing criteria supplied')

      endif

      call pop_routine (myname)
      return
      end



* ====================================================================
       subroutine ozcot_get_other_variables ()
* ====================================================================
      use OzcotModule
      implicit none
       include 'const.inc'             ! Constant definitions
      include 'intrface.pub'
      include 'error.pub'

*+  Purpose
*      Get the values of variables from other modules

*+  Changes
*      psc - 9/08/93
*      psc - 28/03/94  used this routine properly
*      DPH - 7/7/94 Changed call to nt_fac to ozcot_nt_fac
*                   Added check for N module.  If N module doesn't
*                   exist then seed no3ppm with some high values.
*      DPH - 11/7/94   Fixed bug in detection of existence of N module.
*      JNGH - 12/7/94 Changed dlayer in cm to g%dlayr_cm
*      psc - commented out read of LL_DEP

*+  Calls
                                       ! function

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_get_other_variables')

*+  Local Variables
      logical N_in_system              ! Is there any N in system ?
      integer layer                    ! layer number
      real    no3(max_layers)        ! soil nitrate kg/ha in layer
      integer numvals

*- Implementation Section ----------------------------------
      call push_routine(myname)

      call get_integer_var (unknown_module, 'day', '()'
     :                      , g%jdate, numvals
     :                      , 1, 366)
      call get_integer_var (unknown_module, 'year', '()'
     :                      , g%imyr, numvals
     :                      , min_year, max_year)

      call get_real_var (unknown_module, 'maxt', '(oC)'
     :                                  , g%tempmx, numvals
     :                                  , -100.0, 100.0)

      call get_real_var (unknown_module, 'mint', '(oC)'
     :                                  , g%tempmn, numvals
     :                                  , -100.0, 100.0)


      g%tempav = (g%tempmx + g%tempmn)/2.
      g%tmean3 = (g%tempav+g%tempyd+g%tempre)/3. ! mean temperature for g%last 3 days
      g%tempre = g%tempyd                 ! update previous days temp for tomorrow
      g%tempyd = g%tempav                 ! update yesterdays temp for tomorrow

      call get_real_var (unknown_module, 'radn', '(Mj/m^2)'
     :                                  , g%solrad, numvals
     :                                  , 0.0, 1000.0)

      g%solrad = g%solrad / 0.04186            ! convert to langleys

      call get_real_var (unknown_module, 'rain', '(mm)'
     :                                  , g%rain, numvals
     :                                  , 0.0, 1000.0)

      g%rain = g%rain /10.                     ! convert to cm

      ! Get depths of each layer

                                ! get depth of each soil water layer
      call get_real_array (unknown_module, 'dlayer', max_layers
     :                                    , '(mm)'
     :                                    , g%dlayr, g%nlayr
     :                                    , 0.0, 1000.0)

      ! Get moist bulk density
      call get_real_array (unknown_module, 'bd', max_layers
     :                                    , '(mm)'
     :                                    , g%bulkd, numvals
     :                                    , 0.0, 1000.0)

      if (p%num_ll_vals .eq.0) then
         ! Get unavailable g%sw - use ll15 because crop ll is unavailable
         call get_real_array (unknown_module, 'll15', max_layers
     :                                    , '(mm/mm)'
     :                                    , p%unul, numvals
     :                                    , 0.0, 1.0)
      else
         ! ll had been read at init
      endif

      ! Get upper limit of available g%sw
      call get_real_array (unknown_module, 'dul_dep', max_layers
     :                                    , '(mm)'
     :                                    , g%ullayr, numvals
     :                                    , 0.0, 1000.0)

      ! Get upper limit of available g%sw
      call get_real_array (unknown_module, 'sat_dep', max_layers
     :                                    , '(mm)'
     :                                    , g%stlayr, numvals
     :                                    , 0.0, 1000.0)

      ! Convert field capacity relative to wilting point.
      ! convert units to cm and cm/cm

      g%rtdepm=0.0
      g%ul=0.0
!cpc
      g%sat = 0.0
      g%wpwc=0.0

!c      ullayr(j) = ullayr(j)*2.      !   simulate skip row
!c      unul(j)   = unul(j)*2.        !          ditto

      do 10 Layer = 1, g%nlayr
         p%unul(layer) = p%unul(layer) * g%dlayr(layer)
         g%ullayr(Layer) = g%ullayr(Layer) - p%unul(Layer)
         g%ullayr(layer) = g%ullayr(layer) / g%dlayr(layer)
         g%stlayr(Layer) = g%stlayr(Layer) - p%unul(Layer)
         g%stlayr(layer) = g%stlayr(layer) / g%dlayr(layer)
         p%unul(layer) = p%unul(layer) / g%dlayr(layer)
         g%dlayr_cm(layer)=g%dlayr(layer)/10.

         g%rtdepm=g%rtdepm+g%dlayr_cm(layer)       ! depth of profile
         g%ul=g%ul+g%ullayr(layer)*g%dlayr_cm(layer) ! upper limit for profile
         g%sat=g%sat+g%stlayr(layer)*g%dlayr_cm(layer) ! saturated limit for profile
         g%wpwc=g%wpwc+p%unul(layer)*g%dlayr_cm(layer) ! unavailable water content
10    continue

      call get_real_var (unknown_module, 'es', '(mm)'
     :                                  , g%es, numvals
     :                                  , 0.0, 1000.0)

      g%es = g%es / 10.                        ! convert to cm

      call get_real_var (unknown_module, 'runoff', '(mm)'
     :                                  , g%q, numvals
     :                                  , 0.0, 1000.0)


      call get_real_array (unknown_module, 'sw_dep', max_layers, '(mm)'
     :                     , g%swlayr, g%nlayr
     :                     , 0.0, 1000.0)

      ! Convert water to plant available  (cm/cm)

      do 12 Layer = 1, g%nlayr
        g%swlayr(Layer) = g%swlayr(Layer) / 10. / g%dlayr_cm(layer)
     :                - p%unul(Layer)
        g%swlayr(Layer) = max(0.0, g%swlayr(Layer))
        g%sw_start(layer) = g%swlayr(Layer)
12    continue

      g%s_bed_mi = g%swlayr(1)/g%ullayr(1)        ! seed bed moisture index
      g%s_bed_sat = max(g%s_bed_sat,g%s_bed_mi)   ! top layer saturated?

      !   get initial estimate of available soil no3
      call get_real_array (unknown_module, 'no3_min', max_layers
     :                                    , '(mm)'
     :                                    , g%no3mn, numvals
     :                                    , 0.0, 1000.0)

      call get_real_array_optional (unknown_module, 'no3'
     :                                  , max_layers
     :                                  ,'(kg/ha)'
     :                                  , no3, numvals
     :                                  , 0.0, 1000.0)

      ! Need to check for situation of no N in system.

      N_in_system = (numvals .eq. g%nlayr)

      if (N_in_system) then
         !nothing

      else
         ! There is no N model in system.  Feed ozcot 150 units of N
         ! distributed throughout the profile

         do 15 Layer = 1, g%nlayr

            no3(Layer) = (150. / g%nlayr * 100.) / g%dlayr_cm(Layer)
15       continue
      endif

      ! Sum soil nitrate over all layers  (kg/ha)

      g%tsno3 = 0.
      do 20 Layer = 1, g%nlayr
        g%ano3(layer) = no3(layer)-g%no3mn(layer)
        g%tsno3 = g%tsno3 + g%ano3(layer)
20    continue
!cpc
         g%availn = g%tsno3

      if (.not. N_in_system) then
         g%availn = g%tsno3

      else if(g%yest_tsno3.ne.0.) then
         g%availn = g%availn + g%tsno3 - g%yest_tsno3

      else
         g%availn = g%availn
      endif
      call pop_routine(myname)
      return
      end



* ====================================================================
       subroutine ozcot_set_other_variables ()
* ====================================================================
      use OzcotModule
      implicit none
      include 'const.inc'
      include 'intrface.pub'
      include 'data.pub'
      include 'error.pub'

*+  Purpose
*     Update variables owned by other modules.

*+  Changes
*      psc - 9/08/93
*      psc   300394  specified properly
*      DPH   7/7/94  Put 0.0 in max function call instead of 0
*                   Changed call to nt_fac to ozcot_nt_fac
*                    Changed call to set_variable_value to call to
*                    Set_real_array
*      JNGH 18/7/94 Corrected conversion of min no3 from g%ppm to kg/ha
*      JNGH - 12/7/94 Changed dlayer in cm to g%dlayr_cm

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_set_other_variables')

*+  Local Variables
      integer Layer                    ! Layer number
      real    sno3(max_layers)       ! available soil nitrate in layer kg/ha
      real    dlt_sw_dep(max_layers) ! soil water uptake in layer mm
!cjh      real    sw_dep(max_layers) ! soil water uptake in layer mm

*- Implementation Section ----------------------------------
      call push_routine(myname)
      ! Convert water from plant available  (cm/cm)

      dlt_sw_dep(:) = 0.0
!cjh      sw_dep(:) = 0.0

      do 10 Layer = 1, g%nlayr
        dlt_sw_dep(Layer) = (g%swlayr(Layer)- g%sw_start(layer))
     :                       * g%dlayr_cm(layer)*10.0
     :                    
        dlt_sw_dep(Layer) = min(0.0, dlt_sw_dep(Layer))
         call bound_check_real_var (dlt_sw_dep(Layer)
     :                           , -g%sw_start(layer)
     :                              * g%dlayr_cm(layer)*10.0
     :                           , 0.0
     :                           , 'dlt_sw_dep(Layer)')

10    continue

      ! Send updated soil water

!cjh      call set_real_array('sw_dep', swlayr, max_layers, '(mm)')
      call Set_real_array (unknown_module, 'dlt_sw_dep', '(mm)'
     :                    , dlt_sw_dep, g%nlayr)

!cjh      call get_real_array (unknown_module, 'sw_dep', max_layers, '(mm)'
!cjh     :                     , sw_dep, g%nlayr
!cjh     :                     , 0.0, 1000.0)

!cjh      do 15 Layer = 1, g%nlayr
!cjh        g%swlayr(Layer) = (g%swlayr(Layer) + p%unul(Layer))*10.
!cjh     :                * g%dlayr_cm(layer)
!cjh        g%swlayr(Layer) = max(0.0, g%swlayr(Layer)) - sw_dep(layer)
!cjh15    continue
!cjh      write(100,*) sum(g%swlayr)
      ! Send updated soil water

!      call Set_real_array (unknown_module, 'sw_dep', '(mm)'
!     :                    , g%swlayr, g%nlayr)
      ! extract soil NO3

      do 20 Layer = 1, g%nlayr
        g%ano3(Layer) = g%ano3(Layer) - (g%dn_plant*10. * g%ano3(Layer)
     :  / g%tsno3)
        g%ano3(Layer) = max(0.0, g%ano3(Layer))
        sNO3(layer) = g%ano3(layer) + g%no3mn(layer)
20    continue
      g%yest_tsno3 = g%tsno3 - (g%dn_plant*10.)

      ! Send updated soil N


!cjh      call set_real_array('no3', sno3, nlayr, '(kg/ha)' )
      call Set_real_array (unknown_module, 'no3', '(kg/ha)'
     :                    , sno3, g%nlayr)

      call pop_routine(myname)
      return
      end



* ====================================================================
       subroutine ozcot_Send_my_variable
     .    (Variable_name)
* ====================================================================
      use OzcotModule
      implicit none
      include 'data.pub'
      include 'intrface.pub'
      include 'error.pub'

*+  Sub-Program Arguments
       character Variable_name*(*)     ! (INPUT) Variable name to search for

*+  Purpose
*      Return the value of one of our variables to caller

*+  Changes
*      psc - 9/08/93
*      DPH 7/7/94 Changed g%crop_in variable to ozcot_crop_in.
*      250996 jngh added message_unused to else block
*                  replaced litteral names to variable (in arguments)
*                  removed unused include

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_send_my_variable')

*+  Local Variables
      real    yield                    ! lint yield kg/ha
      real    dm                       ! total dry matter kg/ha
      real    totnup                   ! N uptake kg/ha
      real    d_nup                    ! daily N uptake kg/ha
      real    cover

*- Implementation Section ----------------------------------
      call push_routine(myname)
      ! **** Repeat for each variable

      if (Variable_name .eq. 'das') then
         call respond2get_integer_var (variable_name
     :        , '(days)', g%das)

      else if (variable_name .eq. 'sumdd') then
         call respond2get_real_var (variable_name
     :        , '(oCd)', g%sumdd)

      else if (Variable_name .eq. 'sites') then
         call respond2get_real_var (variable_name
     :        , '()', g%sites)

      else if (Variable_name .eq. 'squarz') then
         call respond2get_real_var (variable_name
     :        , '()', g%squarz)

      else if (Variable_name .eq. 'bollz') then
         call respond2get_real_var (variable_name
     :        , '()', g%bollz)

      else if (Variable_name .eq. 'openz') then
         call respond2get_real_var (variable_name
     :        , '()', g%openz)

      else if (Variable_name .eq. 'alint') then
         call respond2get_real_var (variable_name
     :        , '()', g%alint)

      else if (Variable_name .eq. 'dm') then
         dm = g%dw_total * 10.
         call respond2get_real_var (variable_name
     :        , '(kg/ha)', dm)

      else if (Variable_name .eq. 'totnup') then
         totnup = g%total_n * 10.
         call respond2get_real_var (variable_name
     :        , '(kg/ha)', totnup)

      else if (variable_name .eq. 'yield') then
         yield = g%alint / 227.
         call respond2get_real_var (variable_name
     :        , '(bales/ha)', yield)

      else if (variable_name .eq. 'lai') then
         call respond2get_real_var (variable_name
     :        , '(m^2/m^2)', g%alai)

      elseif (variable_name .eq. 'cover_green') then
         cover = l_bound (1.0 - exp (-ozcot_kvalue * g%alai), 0.0)

         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , cover)

      elseif (variable_name .eq. 'cover_tot') then
         cover = 1.0 - exp (-ozcot_kvalue * g%alaiz)

         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , cover)

      elseif (variable_name .eq. 'height') then
!cnh this is a simple fix only due to the limited future
!cnh for this module!!!!!
         call respond2get_real_var (variable_name
     :                             , '(mm)'
     :                             , 900.0)

      else if (Variable_name .eq. 'availn') then
         call respond2get_real_var (variable_name
     :        , '(kg/ha)', g%availn)

      else if (Variable_name .eq. 'tsno3') then
         call respond2get_real_var (variable_name
     :        , '(kg/ha)', g%tsno3)

      else if (Variable_name .eq. 'ysno3') then
         call respond2get_real_var (variable_name
     :        , '(kg/ha)', g%yest_tsno3)

      else if (Variable_name .eq. 'd_nup') then
         d_nup = g%dn_plant * 10.
         call respond2get_real_var (variable_name
     :        , '(kg/ha)', d_nup)

      else if (variable_name .eq. 'rtdep') then
         call respond2get_real_var (variable_name
     :        , '(cm)', g%rtdep)

      else if (variable_name .eq. 'tmean3') then
         call respond2get_real_var (variable_name
     :        , '(oC)', g%tmean3)

      else if (variable_name .eq. 's_bed_sat') then
         call respond2get_real_var (variable_name
     :        , '()', g%s_bed_sat)

      else if (variable_name .eq. 's_bed_mi') then
         call respond2get_real_var (variable_name
     :        , '()', g%s_bed_mi)

      else if (variable_name .eq. 'smi') then
         call respond2get_real_var (variable_name
     :        , '()', g%smi)

      else if (variable_name .eq. 'evap_plant') then
         call respond2get_real_var (variable_name
     :        , '(cm)', g%ep)

      else if (variable_name .eq. 'evap_soil') then
         call respond2get_real_var (variable_name
     :        , '(cm)', g%es)

      else if (variable_name .eq. 'evap_pot') then
         call respond2get_real_var (variable_name
     :        , '(cm)', g%eo)

      else if (variable_name .eq. 'evap_tot') then
         call respond2get_real_var (variable_name
     :        , '(cm)', g%et)

      else if (variable_name .eq. 'ozcot_crop_in') then
         call respond2get_logical_var (variable_name
     :        , '()', g%crop_in)

      else if (variable_name .eq. 'ozcot_status') then
         call respond2get_integer_var (variable_name
     :        , '()', g%iend)

      else
            ! Nothing
         call message_unused ()
      endif

      call pop_routine(myname)
      return
      end



* ====================================================================
       subroutine ozcot_set_my_variable (Variable_name)
* ====================================================================
      use OzcotModule
      implicit none

*+  Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name to search for

*+  Purpose
*     Set one of our variables altered by some other module

*+  Changes
*      psc - 9/08/93
*      250996 jngh updated interface

*- Implementation Section ----------------------------------

*      if (variable_name .eq. '????') then
*         call collect_real_array (variable_name, '()', max_layer
*     :                               , ????, numvals
*     :                               , 0.0, 1.0)

*      else
            ! Don't know this variable name
         call Message_unused ()
*      endif


      return
      end



* ====================================================================
       subroutine ozcot_Process ()
* ====================================================================
      use OzcotModule
      implicit none
      include 'error.pub'

*+  Purpose
*      Perform actions for current g%day.

*+  Changes
*      psc - 9/08/93
*      250996 jngh removed unused include

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_process')

*- Implementation Section ----------------------------------

      call push_routine(myname)
!     call patched-in ozcot model

      if (g%crop_in) then

         if(g%jdate.ne.g%isow) g%das = g%das + 1

         call ozcot2 ()

         if (g%iend .eq. 2) then
            call ozcot_manager('harvest', ' ')
         endif

      else

      endif

      call pop_routine(myname)
      return
      end



* ====================================================================
       subroutine ozcot_Prepare ()
* ====================================================================
      use OzcotModule
      implicit none

*+  Purpose
*     Perform calculations before the current timestep.

*+  Changes
*      psc - 9/08/93

*- Implementation Section ----------------------------------

      return
      end



* ====================================================================
       subroutine ozcot_post ()
* ====================================================================
      use OzcotModule
      implicit none

*+  Purpose
*     Perform calculations after the current timestep.

*+  Changes
*      psc - 9/08/93

*- Implementation Section ----------------------------------

      return
      end



* ====================================================================
       subroutine ozcot_end_run ()
* ====================================================================
      use OzcotModule
      implicit none

*+  Purpose
*     Perform cleanup because the current simulation is about to end.

*+  Changes
*      psc - 9/08/93

*- Implementation Section ----------------------------------

      return
      end



!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cc                                                                cc
!cc                                                                cc
!cc                       program ozcot                            cc
!cc                                                                cc
!cc                          27/5/83                               cc
!cc                                                                cc
!cc                                                                cc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c                                                                  c
!c     begun during a visit to  t.a.e.s. blackland research center  c
!c     temple, texas 1981 by a.b.hearn.                             c
!c                                                                  c
!c     developed at nars for namoi valley cotton  1982 to 1988      c
!c     by hearn and da roza.                                        c
!c         components:                                              c
!c           water balance   - ritchie model taken largely from     c
!c                             cornf(stapper & arkin)               c
!c           nitrogen model -  developed at nars by hearn & da roza c
!c           fruit submodel  - taken from siratac by hearn          c
!c                                                                  c
!c     ozcot1 - version for retrospective simulation of specific    c
!c              crops; runs for one season only, irrigation dates   c
!c              given.                                              c
!c     ozcot2 - version for predictive simulation  through many     c
!c              seasons; irrigation dates predicted.                c
!c     ozcot3 - version for optimising irrigation                   c
!c     ozcot4 - version for calibration with minos5                 c
!c     ozcot5 - version for physiological analyis                   c
!c                                                                  c
!c                                                                  c
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c     structure
!c       ozcot2  init
!c               cinput2
!c               newdate
!c               metdat2 hhunc
!c                       evap    fn satvp
!c               decide_irg
!c               solwat  sevap   fn watco
!c                       swbal
!c               sowday
!c               emerg
!c               snbal   n_fertilise
!c               pltgrw  actlai
!c                       laigen  fn senlf, fn stress
!c                       cropn
!c                       istsq
!c                       fruit   bollwt  fn stress
!c                               carrying_capacity fn stress
!c                               overload
!c                               actfru
!c                               update
!c                               fn frugen       fn stress
!c                               fn survive
!c               harvest
!c               dayout2
!c               (day_dudley    option for norm dudley)
!c               yield
!c               reset
!c
!c       note modules ozcot2, cinput2, metdat2, dayout2, decide_irg
!c                    newdate, reset are specific to ozcot2.
!c
!c       ozcot.inc common blocks
!c
!c       input files: met.inp, soilw.inp, agron.inp - 1
!c       output files: fruit.out, yield.out, calib.out - units = 2,3
!c
!c link/exe=ozcot2 ozcot2,init,cinput2,metdat2,hfunc,evap,satvp, -
!c                 decide_irg,solwat,sevap,watco,swbal, -
!c                 sowday,emerg,snbal,n_fertilise, -
!c                 pltgrw,actlai,laigen,senlf,stress,cropn,istsq, -
!c                 fruit,bollwt,carrying_capacity,overload,actfru, -
!c                 update,frugen,survive, -
!c                 harvest,dayout2,yield,reset
!c
!c
!c                 ozcot2 - calling program
!c                 ---------------------------
!c
!c
!c      program ozcot2
* ====================================================================
      subroutine OZCOT2
* ====================================================================
      use OzcotModule
      implicit none
      include 'error.pub'

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot2')

*- Implementation Section ----------------------------------
      call push_routine(myname)
!c
!c     data agron_inp/'agron.inp'/, soilw_inp/'soilw.inp'/,
!c    *count_inp/'count.inp'/, lai_inp/'lai.inp'/,
!c    *rain_inp/'rain.inp'/, met_inp/'met.inp'/, fcot_out/'fcot.out'/,
!c    *frucal_out/'frucal.out'/, yield_out/'yield.out'/
!c
!c      data iend/0/
!c
!c
!cpsc      i=i+1

!c
!c      do 10 nszn = 1,100                     ! seasonal loop
!c          do 20 i = 1,1000                   ! daily loop through whole year
!c             call metdat2 (i,iend)       ! get met data
!c              if(iend.eq.1)go to 31       ! end of met data
              CALL ozcot_metdat2
!c             if(defirr(2).ne.0.) call decide_irg (i)  ! irrigated crop?
!c              call solwat (i,dayir,npre)  ! soil water
              CALL ozcot_solwat                 ! soil water
!c             if(defirr(2).ne.0.) call decide_irg       ! irrigated crop?
!c             if(isow.le.0)then           ! crop sown yet?
!c                  call sowday (i,iend)    ! sow tomorrow?
!c                  call sowday             ! sow tomorrow?
!c                  if(iend.ge.3) go to 32  ! passed sowing window or fallow
!c              elseif(i.gt.isow .and. iemerg.le.0)then     ! crop emerged yet?
              if(g%iemrg.le.0) then
!c                  call emerg (i)          ! emerge today?
                  CALL ozcot_emerg              ! emerge today?
              ENDIF
!c              call snbal(i)               ! soil n balance
!c              call snbal                  ! soil n balance
!c              if(isow.gt.0 .and. i.gt.isow) call pltgrw (i,iend,nszn)
!c              if(openz.gt.0.0) call harvest(iend)
              IF(g%isow.GT.0 .AND. g%das.GT.0) CALL ozcot_pltgrw
              IF(g%openz.GT.0.0) CALL ozcot_harvest
!c              if(iend.eq.2)  go to 32     ! end of season
!c              if(iend.ne.2)  then
!c                call dayout2(i)             ! daily output
!cpsc                call dayout2             ! daily output
!c               call day_dudley             ! output for norm dudley
!c20             continue                           ! end of daily loop
!c                return
!c              endif
!c32           continue
!c              call yield(nszn,iend)           ! calculate yield
!c              call reset(iend)                ! reset variables for new season
              CALL ozcot_yield                      ! calculate yield
!cpsc              call reset                      ! reset variables for new season
!c10       continue                               ! end of seasonal loop
!c31        continue
!c          call exit
!c      stop

       call pop_routine(myname)
       return
       END

!c


* ====================================================================
!c      subroutine pltgrw (i,iend,nszn)
      subroutine ozcot_pltgrw
* ====================================================================

!c-------------------------------------------------------------------
!c      calls the various plant growing routines.  at this point    !
!c      there are also some variable conversions to allow merging   !
!c      of the independently derived soil and plant portions of the !
!c      model.                                                      !
!c-------------------------------------------------------------------

      use OzcotModule
      implicit none
      include 'error.pub'

 !     real percent_l
!cpc   integer ifrost
      integer j

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_pltgrw')

!      DIMENSION PERCENT_L(10)
!      DATA PERCENT_L/0.38,0.38,0.39,0.42,0.4,5*0.0/        ! lint percent
!c      data fburr /1.23/                       ! factor sc/boll to sc+burr/boll
!cpc   data ifrost/0/                          ! flag for simulated frost

!c----- housekeeping -----------------------------------------------------------
*- Implementation Section ----------------------------------
      call push_routine(myname)
!cpsc      iday=i-isow ! replaced ncrpdy throughout, 15 nov 1983
      g%iday=g%das
      g%dd=g%hunits
      g%rad=g%solrad
      g%pclint = p%percent_l             ! set lint percent for variety

      IF(g%iday.EQ.1) THEN
          g%ifrost = 0                         ! 1st g%day, reset local variable
      ELSE
          DO 10 J=1,g%iday
              g%fyzage(J)=g%fyzage(J)+g%dd     ! physiological aging
10        continue
      ENDIF
      g%sumdd=g%sumdd+g%dd

!c----- increase root depth ----------------------------------------------------

      IF(g%iday.LE.36)g%rtdep=g%sdepth+((20.-g%sdepth)/36.)*real(g%iday) ! W*N          !const  rtdep_das_crit, rtdep_sdepth_crit
!cpsc        changed maximum rooting depth
!cpsc  if(iday.ge.37.0)rtdep=122.38*(1.-2.65*exp(-.03*iday)) ! 82/8
      IF(g%iday.GE.37)g%rtdep=c%rtdep_max
     :                       *(1.-2.65*EXP(-.03*real(g%iday))) ! 82/8                !const
      IF(g%rtdep.GT.g%rtdepm)g%rtdep=g%rtdepm

!c---- check if frost terminates crop -----------------------------------------

!c      if(tempmn.le.2. .and. iday.gt.120) then   ! frost? if so end of season
      IF(g%tempmn.LE.c%frost_kill_immediate 
     :   .AND. g%iday.GT.c%frost_kill_immediate_das) THEN ! frost? if so end of season           !const  frost_temp_kill_immediate, frost_das_kill_immediate
          g%iend=2 ! flag for frost - g%last g%day, open bolls > 80% mature
      ELSE IF(g%tempmn.LT.c%frost_kill_delayed
     :   .AND. g%iday.GT.c%frost_kill_delayed_das) THEN ! frost? if so end of season     !const  frost_temp_kill_delayed, frost_das_kill_delayed
          g%ifrost = g%ifrost+1                 ! count days to simulate frost
          IF(g%ifrost.EQ.c%frost_kill_delayed_days) THEN                                                          !const   frost_days_kill_delayed
              g%iend = 2                        ! frost                                  !const
          ENDIF
      ELSE
          g%ifrost = 0                          ! reset because sequence broken
      ENDIF

!c      if(iend.eq.2) write(2,777)jdate,iday,i
!c777        format(' frost on julian day ',i3,', ',i3,
!c     *    'days from sowing, index=',i3,' *** end of season ***')

!c----- emergence ( why not call ozcot_emerg here ------------------------------------

      IF(g%das.LT.g%iemrg.OR.g%iemrg.EQ.0) GO TO 25
      IF(g%das.EQ.g%iemrg)  g%ddmerg=g%sumdd-g%dd

!c----- increase leaf area -----------------------------------------------------

!c      if(i.le.ilai)then
!c       call actlai(i)
!c       call actlai
!c      else
!c       call laigen(i)
        CALL ozcot_laigen
!c      end if

      call ozcot_dryxmatter
      call ozcot_plant_n

!c----- crop nitrogen ---------------------------------------------------------

!c      call cropn(i)
      CALL ozcot_cropn

!c---- grow plant -------------------------------------------------------------

      IF(g%isq.EQ.0) THEN
!c          call istsq (i,nszn)
          CALL ozcot_istsq
      ELSE
!c          if(i.gt.isq) call fruit (i,iend)
          IF(g%das.GT.g%isq) CALL ozcot_fruit
      ENDIF

!c      if(isyr.eq.82 .and. jdate.eq.354) call hail(i) ! hail in 1982-83 experiment
!c      if(isyr.eq.82 .and. jdate.eq.354) call hail    ! hail in 1982-83 experiment

25    continue

!c---- crop development complete? ---------------------------------------------

      IF(g%openz.GT.0.0 .AND. g%bollz.EQ.0.0)
     * g%iend=2                                 ! all bolls open, end of season        !const

!c------ following are for use in s/r yield -----------------------------------

      IF(g%alai.GT.g%alaiz)THEN
          g%alaiz=g%alai                        ! max LAI
          g%ilaiz=g%iday                        ! g%day of max LAI
          g%plntnz=g%plantn                     ! g%plantn on g%day of max LAI
          g%iplntn=g%ilaiz
      ENDIF

      IF(g%squarz.GT.g%sqzx) THEN
          g%sqzx = g%squarz                     ! peak square numbers
          g%isqzx = g%iday                      ! g%day of peak squares
      ENDIF

!c------------------------------------------------------------------------------
      call pop_routine(myname)

      RETURN
      END


* ====================================================================
!c      subroutine bollwt(idayx,l)
      subroutine ozcot_bollwt(das)
* ====================================================================

!c     calculates increase in weight of each days's bolls.
!c     bollgrowth rate is driven by dd,limited by water,
!c     n and c(incl water effects on photosynthesis) stress

      use OzcotModule
      implicit none
      include 'error.pub'


!c------stuff done on 1st call of the day - stresses & growth rate -------------
      !  functions
      real ozcot_stress

      ! locals (i hope!)
      integer das
      real fbcstr
      real fbwstr
      real fbnstr
      real strsbl
      real f_temp
      real boll

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_bollwt')

*- Implementation Section ----------------------------------
      call push_routine(myname)

      IF(g%idayx.EQ.0 .OR. g%iday.EQ.g%idayx+1) THEN ! 1st call for this day?
        g%idayx = g%iday                     ! if so, set flag to stop more calls
        FBCSTR = 1.0                         ! Cc stress factor to reduce g%bollgr
        IF(g%bload.GT.0.)FBCSTR = g%carcap_c/g%bload ! supply/demand ratio for when
        IF(FBCSTR.GT.1.)FBCSTR = 1.          ! boll growth limited by Cc supply
        FBWSTR = ozcot_stress(c%fbwstr_low
     :                        ,c%fbwstr_high
     :                        ,c%fbwstr_a
     :                        ,g%smi)   ! water stress on bolls         !const  sw_stress_boll_min, sw_stress_boll_max, sw_stress_boll_pwr
        FBWSTR = 1.                          ! try no direct stress - 24/4/92
        IF(g%bollz+g%openz .LT. g%carcap_n) THEN ! final boll req < uptake
            FBNSTR = 1.0                     ! do not apply N stress
        ELSE                                 ! final boll req < uptake
            FBNSTR = ozcot_stress(c%fbnstr_low
     :                           ,c%fbnstr_high
     :                           ,c%fbnstr_a
     :                           ,g%fnstrs)    ! apply N stress       !const  n_stress_boll_min, n_stress_boll_max, n_stress_boll_pwr
        ENDIF
        STRSBL = AMIN1(FBCSTR,FBWSTR,FBNSTR) ! minimum of Cc, water & N stress

        IF(g%tempav.LT.20.0) THEN                                                  !const   x_temp_boll   15 20 30 35
          F_TEMP = -3.0+0.2*g%tempav          ! temperature scaling factor         !const   y_stress_boll  0  1  1  0
        ELSE IF(g%tempav.GT.30.0) THEN        ! for boll weight                    !const
          F_TEMP = 7.0-0.2*g%tempav           ! derived from Hesketh and Low 1968   !const
        ELSE
          F_TEMP = 1.0                                                              !const
        ENDIF
        IF(F_TEMP.LT.0.) F_TEMP = 0.
        IF(F_TEMP.GT.1.) F_TEMP = 1.

        BOLL = p%scboll*F_TEMP        ! effect of temperature
        g%bgrvar = BOLL*g%bper                ! boll growth rate this g%day

      ENDIF

      IF(das.GT.g%lfru(cat4) .OR. g%lfru(cat4).EQ.0) then
         call pop_routine(myname)
         RETURN
      else
      endif

!c------         increase in weight of bolls (seed cotton) ---------------------------

      g%bollgr = g%bgrvar*STRSBL              ! todays rate per boll
      g%bollgr = g%bollgr *1.3                ! potential unstressed rate            !const     bollgr_pot_fac
      IF(g%bollgr.LT.0.) g%bollgr = 0.
      g%fruwt(das) = g%fruwt(das)+g%fruno(das)*g%bollgr ! increase today for das g%day bolls

!c------ shed fruit not growing ------------------------------------------------

      IF(das.GT.g%lfru(cat5) 
     :   .AND. das.LE.g%lfru(cat7) 
     :   .AND. g%fruno(das).GT.0.) THEN
           IF(g%fruwt(das)/g%fruno(das).LT.0.1) 
     :          g%frmark(das,age6) = g%fruno(das)
      ENDIF

!c------------------------------------------------------------------------------
      call pop_routine(myname)
      RETURN
      END


* ====================================================================
!c      subroutine carrying_capacity(i)
      subroutine ozcot_carrying_capacity
* ====================================================================

!c     estimates carrying capacity of crop on basis of photosynthesis.
!c     selects parameter for variety. adjusted for water stress.
!c     carcap is carrying capacity, maximum number of bolls the crop
!c     can carry, therefore the boll load that causes 100% shedding.

      use OzcotModule
      implicit none
      include 'error.pub'

      real ozcot_stress

      real alight
      real radn_watts
      real p_gossym
      real pot_pn
      real pn
      real relp
      real templf
      real rfac
      real rm


!c      data from "siratac" - 1987-88  hearn (pers. comm.)

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_carrying_capacity')
!cpc   data istress/0/, ireliefco/0/

*- Implementation Section ----------------------------------
      call push_routine(myname)

!cpsc      if(i.eq. isq+1) then
      IF(g%das.EQ. g%isq+1) THEN
          g%istress = 0                 ! reset stress flag
          g%ireliefco = 0               ! reset stress relief counter
      ENDIF

!cpsc      if(bload.gt.cutout .and. smi.lt.0.75) istress = 1 ! set stress cutout flag

!cpsc      if(smi.gt.0.75 .and. istress.gt.0) then

      if(g%bload.gt.g%cutout.and.g%smi.lt.c%smi_affect_wlog) g%istress=1 ! 0.25 replaced 0.75 - ABH 5/11/96    !const  wlog_relief_smi
      if(g%smi.gt.c%smi_affect_wlog.and.g%istress.gt.0) then       ! 0.25 replaced 0.75 - ABH 5/11/96

          g%ireliefco = g%ireliefco + 1 ! count days since relief of stress
          IF(g%ireliefco.EQ.c%days_relief_wlog) THEN                                                     !const        wlog_relief_days
              g%istress = 0             ! end stress effect on wterlogging
              g%ireliefco = 0           ! reset counter
          ENDIF
      ENDIF

!c----photosynthetic capacity --------------------------------------------------
!c-----light interception modified to give hedgerow effect with skip row - abh 5/11/96 ------

      g%alai = g%alai*g%rs         ! lai in hedgerow
      ALIGHT = (1.-EXP(-ozcot_kvalue*g%alai)) ! original code  - now gives interception in
                                                ! hedgerow
      ALIGHT =ALIGHT/g%rs          ! interception on ground area basis
      g%alai = g%alai/g%rs         ! restore LAI to ground area basis

      RADN_watts = g%solrad*0.8942      ! convert RADN from ly to watts m**2            !const
      P_gossym = 2.391+RADN_watts*(1.374-RADN_watts*0.0005414) ! GOSSYM line1275        !const
      POT_PN = P_gossym*0.068           ! potential photosynthesis g%g/m2 CH2O          !const
      PN = POT_PN*ALIGHT                ! net photosynthesis term
!c----- effect of water stress on photosysthesis -------------------------------

      IF(g%smi .LT. c%relp_smi_crit) THEN                                                           !const    sw_stress_pn_crit  
          RELP =c%relp_intercept+c%relp_slope*g%smi              ! effect of water stress on Pp              !const    sw_stress_pn_smi_intc, sw_stress_smi_pn_slope
          IF(RELP.GT.1.)RELP = 1.           ! (TURNER g%et al 1986).                    !const
          RELP = RELP -c%relp_intercept                                                             !const
          RELP = ozcot_stress(c%relp_low,c%relp_high,c%relp_a,RELP)   ! increase severity of stress         !const   sw_stress_relp_min, sw_stress_relp_max, sw_stress_relp_pwr, 
          RELP = RELP + c%relp_intercept                                                             !const
          PN = PN*RELP                      ! photosynthesis adjust for water stress
      ENDIF

!c----- waterlogging effect additional to n uptake - hearn & constable 1984 eqn 4

!c      if(istress.gt.0) then
!c        if(sw/ul.gt.0.87) pn = pn * 0.1 ! carrying capacity reduced
!c      else
!c        if(sw/ul.gt.0.87) pn = pn * 0.1 ! carrying capacity reduced
!c      endif

!c---- maintenance respiration -------------------------------------------------

      TEMPLF = g%tempav+5.-10.*g%smi    ! leaf temperature                             !const
      IF(TEMPLF.LT.g%tempav) TEMPLF=g%tempav
      RFAC = 2.2**((TEMPLF-35.)/10.)    ! resp temp factor, Horie(Constable)           !const
      RM = g%sites*p%respcon*RFAC ! maintenance respiration term

!c----- carrying capacity carbon - photosynthesis divided by boll growth rate

      IF(g%bgrvar.GT.0.)
     * g%carcap_c = (PN-RM)/(g%bgrvar*c%FBURR) ! carrying capacity, carbon, no stress
      IF(g%carcap_c.LT.0.) g%carcap_c = 0. ! trap

!c----- waterlogging effect additional to n uptake - hearn & constable 1984 eqn 4
      IF(g%istress.GT.0) THEN
        IF(g%sw/g%ul.GT.c%stress_wlog) 
     :      g%carcap_c = g%carcap_c  * c%wlog_carcap_red_stress ! carrying capacity reduced    !const   ! ? 0.01
      ELSE
        IF(g%sw/g%ul.GT.c%stress_wlog) 
     :      g%carcap_c = g%carcap_c * c%wlog_carcap_red ! carrying capacity reduced    !const
      ENDIF

      g%cutout = g%carcap_c*p%fcutout ! boll load for g%cutout, sq prodn stops
!c-------------------------------------------------------------------------------
      call pop_routine(myname)
      RETURN
      END


* ====================================================================
!c      subroutine cropn(i)
      subroutine ozcot_cropn
* ====================================================================

      use OzcotModule
      implicit none
      include 'error.pub'

      real harvest_n
      real bgr

!c     assumes all n available for season (availn) from snbal
!c     is taken up (uptakn).
!c     computes: n harvested on basis of constable and rochester 1988
!c               n in fruit frun
!c               carrying capacity (carcap_n), number of bolls for which
!c                       harvest_n is adequate
!c                       used in frugen and survive for squares
!c               stresses:
!c                       vegetative (vnstrs for laigen) = f(1-harvest_n/uptakn)
!c                               ie function of proportion of n remaining in veg
!c                         fruit (fnstrs for bollwt) 1- frun/harvest_n
!c                               ie function of n to be harvested not in fruit


      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_cropn')

*- Implementation Section ----------------------------------
      call push_routine(myname)

!c----------------------------------------------------------------------------
      g%uptakn = g%availn   ! potential uptake for season based on available N

!c-----  compute potential n harvested ----------------------------------------

      HARVEST_N = g%uptakn * c%harvest_n_frac    ! harvestable N; 0.85 fromConstable & Rochester       !const
      HARVEST_N = HARVEST_N/10.      !  kg/ha to g%g/m**2, N limiting fruit.               !const

!c----- compute n already in fruit -------------------------------------------
!cpdev this is generating an underflow warning:
!cpdev ---------------------------------vvvvvvvvvv
      g%seed_nc = .02407+0.000147*g%uptakn-0.00000034*g%uptakn**2 ! GAC dat 3/6/88         !const
      g%frun = (g%frudw+g%openwt)*((1.-g%pclint)*g%seed_nc+(c%FBURR-1.)*
     :.005) ! N in frt                                                                     !const

!c----- compute n carrying capacity --------------------------------------------

      BGR = p%scboll                ! seed coton per boll
      g%carcap_n = HARVEST_N /(BGR*0.6*0.03) ! bolls per m                                 !const

!c----- compute n stresses -----------------------------------------------------

      g%vnstrs = (g%uptakn-10.)/g%uptakn ! vegetative stress 10=uptake @ 1st boll          !const

      IF(g%bollz.EQ.0.) THEN            ! before 1st boll
          g%fnstrs = 1.0
      ELSE
          g%fnstrs = 1.- g%frun/HARVEST_N ! fraction of harvestable not in fruit
      ENDIF

      IF(g%vnstrs.GT.1.0) g%vnstrs=1.0
      IF(g%vnstrs.LT.0.0) g%vnstrs=0.0
      IF(g%fnstrs.GT.1.0) g%fnstrs=1.0
      IF(g%fnstrs.LT.0.0) g%fnstrs=0.0
!c----------------------------------------------------------------------------

      call pop_routine(myname)
      RETURN
      END


* ====================================================================
!c      subroutine emerg (i)
      subroutine ozcot_emerg
* ====================================================================
!c-------------------------------------------------------------------
!c
!c     simulates  emergence
!c
!c-------------------------------------------------------------------

      use OzcotModule
      implicit none
      include 'error.pub'

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_emerg')

*- Implementation Section ----------------------------------
      call push_routine(myname)

      IF(g%iemrg.GT.0) then
         !RETURN
      else
!c----- simple heat sum as an alternative to wanjura's function -----

         IF(g%sumdd .LT. 60.) then                                            !const
            !RETURN
         else
            g%iemrg = g%das
         endif
      endif

      call pop_routine(myname)
      RETURN
      END

!c-------------------------------------------------------------------

!c      previous version jackson/arkin

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c      calculates hypocotle elongation leading up to emergence     c
!c      based on the growing-degree-day concept.                    c
!c      the elongation rate of 0.12 cm/gdd is use to a maximum rate c
!c      of 2.04 cm/gdd at about 32 c.                               c
!c      the gdd base temperature is set in the "init" subroutine.   c
!c      elongation rate data are from wanjura et.al. 1970.          c
!c                                                                  c
!c      key variables:                                              c
!c          sdepth = seed sowing depth                              c
!c          stemp = soil temperature (calc. in subroutine "metdat") c
!c          iemrg = emergence date                                  c
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!c       t = stemp                             ! soil temp
!c       if(stemp.lt.14.44)t = 14.44+(14.44-t) ! to comput lag whenstemp < 14.44
!c       hypoel = 0.0853-0.0057/(41.9-t)*(t-34.44)**2 ! wanjura's function
!c       hypoel = hypoel*24.                   ! convert to daily rate
!c       hypoel = hypoel*0.6                   ! tune to namoi valley
!c       if(stemp.lt.14.44)hypoel = hypoel*(-0.5) ! delay below 14.44
!c       esum = esum+hypoel
!c
!c      if(esum .lt. sdepth) return
!c      iemrg = i
!c
!c      return
!c      end


* ====================================================================
!c      subroutine evap (i)
      subroutine ozcot_evap
* ====================================================================
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c      calculates potential evapotransperation from modified pen-  c
!c      man equation as reported by ritchie.                        c
!c                                                                  c
!c      key variables:                                              c
!c          delta = slope of the water vapor/ air temp. curve       c
!c          albedo = combined plant-soil albedo                     c
!c          h = net radiation balance                               c
!c          eo = potential et over bare soil                        c
!c          eos = potential et below plant canopy                   c
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      use OzcotModule
      implicit none
      include 'error.pub'

      real ozcot_satvp

      real elev
      real Pp
      real gamma
      real tk
      real delta
      real d
      real xn1
      real svpmax
      real svpdry
      real svpwet
      real vpdry
      real salpha
      real albedo
      real r4
      real r6
      real h
      real at
      real rns
      real F_INT

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_evap')

*- Implementation Section ----------------------------------
      call push_routine(myname)

!c---------- calculate bowen ratio term -----------------------------------------

        ELEV=c%elevation_default                                    ! ELEVATION (M)                  !const
!jh        ELEV=200.0                                    ! ELEVATION (M)                  !const
        Pp=1013.0-0.105*ELEV                          ! MEAN PRESSURE(MB)              !const
        GAMMA=6.6E-04*Pp*(1.0+0.00115*g%tempwt)    ! PSYCHROMETER CONSTANT             !const
        TK=g%tempav+273.                                ! MEAN TEMP(DEG KELVIN)        !const
        DELTA=(EXP(21.255-5304./TK))*(5304./(TK**2.)) !SLOPE g%sat VAP PRES CURVE      !const
        D=DELTA/(DELTA+GAMMA)

!c------ calculate fraction radiation reaching surface(tr) ----------------------
!c       culls functions(unpublished)


        XN1=0.404*ALOG(g%s)+1.49                                                      !const
!cpsc
        g%alai = g%alai*g%rs        ! lai in hedgerow
        IF(g%alai.LT.XN1) THEN     ! when LAI below XN1 threshold
            g%tr=EXP(-0.6*g%alai)                                                    !const
        ELSE                        ! when LAI above XN1 threshold
            g%tr=EXP(-0.398*g%alai)                                                  !const
        ENDIF

!cpsc
        F_INT = 1.0-g%tr              ! intercetion in hedgerow
        F_INT = F_INT/g%rs          ! interception on ground area basis
        g%tr =  1.-F_INT            ! transmission on ground area basis
        g%alai = g%alai/g%rs        ! restore LAI to ground area basis

!c        if(rs.gt.1.) tr = (tr+rs-1.)/rs ! adjust for rows wider than im

!c-----vapor pressure deficit: mean of vpd at 9am and 3pm ,assuming atmos.
!c     vp.(vpdry) remains constant throughout day, and tmax=t3pm-----------------

        SVPMAX=ozcot_satvp(g%tempmx)      ! g%sat VP AT TMAX=g%sat VP AT T3PM
        SVPDRY=ozcot_satvp(g%tempdy)      ! g%sat VP AT 9AM TDRY
        SVPWET=ozcot_satvp(g%tempwt)      ! g%sat VP AT 9AM TWET

           VPDRY=SVPWET-GAMMA*(g%tempdy-g%tempwt) ! atmospheric VP
           IF(VPDRY.LE.0.) VPDRY = 0.1         ! cannot be -ve.
           g%vpd=((SVPDRY-VPDRY)+(SVPMAX-VPDRY))/2.

!c------ calculate potential evaporation (eo) -----------------------------------

        SALPHA=0.09                                                                  !const
        ALBEDO=0.3067+0.3333*SALPHA-((0.23-SALPHA)/0.75)*g%tr ! ??                   !const

!c------net longwave radiation(r6,cal/cm2)--------------------------------------

!c      ritchie(1975)unpub.;idso & jackson(1969)j geophys res,74:5397-5403;
!c      jensen et al(1969)proc am soc civil engin,96:61-79                             ! temple,texas
!c        r4=1.-.261*exp(-7.77e-04*tempav**2.)        ! sky emissivity,ea
!c        r6=(r4-.96)*1.17e-07*tk**4.*(.2+.8*solrto)  ! ((ea-es)*@*tk**4)*(-)

        R4=1.08*(1.-EXP(-VPDRY**(TK/2016.)))                                           !const
        R6=(R4-1.)*1.*1.17E-07*TK**4.*(.2+.8*g%solrto) ! ((EA-g%es)*@*TK**4)*(-)       !const
                                                     ! g%es=EMISSIVITY OF EVAP SURFACE  !const

!c-------net radiation(h,cal/cm2)----------------------------------------------

        H=(1.-ALBEDO )*g%solrad+R6
        IF(H.LT.0.0) H=0.0

        g%ho=H/583.                                 ! NET RADIATION(CM)             !const
!c       go=g/583.                                   ! soil heat flux(cm)

!c-------advection or aerodynamic term ------------------------------------------

        IF(g%wind.NE.0..AND.g%tempwt.NE.0.) THEN
            AT = (1.-D)*0.01*(1.+.028*g%wind)*g%vpd ! advection: g%wind & g%vpd       !const
        ELSE IF(g%wind.EQ.0..AND.g%tempwt.NE.0.) THEN
            AT = -11.9813+0.012*g%vpd+0.0404*TK     ! advection: g%vpd but no g%wind   !const
        ELSE
            AT = -20.4355+0.0697*TK                 ! advection:  no g%vpd or g%wind   !const
        ENDIF
        IF(AT.LT.0.0) AT = 0.0

        g%eo=g%ho*D+AT                              ! GdaR Mar'85, update ABH Mar88.

!c------ calculate potential below canopy evaporation (eos) ---------------------

        RNS=g%ho*g%tr ! GdaR Mar'85
        g%eos=D*RNS ! GdaR Mar'85
        IF(g%eos.LT.0.0)g%eos=0.0
        IF(g%eos.GT.g%eo)g%eos=g%eo

        call pop_routine(myname)
        RETURN
        END


* ====================================================================
!c      function frugen(i)
      real FUNCTION ozcot_frugen(ndas)
* ====================================================================
!c
!c     estimates generation of new fruit as function of existing
!c     fruiting sites and bolload adjusted for nitrogen stress

!c      data from "siratac" - 1987-88 hearn (pers. comm.).

      use OzcotModule
      implicit none
      include 'data.pub'
      include 'error.pub'

      integer ndas

      real ozcot_stress

!cpc   real sites1
      real blr
      real dfru
      real vsnstr
      real ppm_row
      real popfac

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_frugen')

*- Implementation Section ----------------------------------
      call push_routine(myname)

!c     psc      i=icount
!c-----------------------------------------------------------------------------
!c     initialise on first call for new crop
!c-----------------------------------------------------------------------------

!c      if(i.eq.isq+1)then           ! 1st call,ie day after 1st square
      IF(ndas.EQ.g%isq+1)THEN         ! 1st call,ie g%day after 1st square
          g%n_cutout=0             ! flag to show g%cutout, >= 1 = not squaring
          g%sites1=0.0             ! to save current value of g%sites
!c          alag=0.0                 ! reset lag accumulator
      ENDIF
      ozcot_frugen=0.                    ! delete y'day's value and reset to zero
!c-----------------------------------------------------------------------------
!c     is this 1st cycle, cutout or 2nd cycle (regrowth)?
!c-----------------------------------------------------------------------------

      BLR = 1.                             ! boll load ratio =1 when g%cutout=0
!c      if(i.eq.isq+1) blr = 0.              ! except day after 1st square
      IF(ndas.EQ.g%isq+1) BLR = 0.            ! except g%day after 1st square
      IF(g%cutout.GT.0.) BLR=g%bload/(g%cutout) ! ratio of 0 is no boll load
      IF(BLR.GT.1.) BLR=1.                 ! ratio of 1 is full boll load

      IF(BLR.LT.1.0 .AND. g%n_cutout.le.0)GO TO 30 ! squaring continues
      IF(BLR.LT.1.0 .AND. g%n_cutout.GE.1)GO TO 20 ! squaring stopped, may resume
      IF (reals_are_equal(BLR,1.0)) then
         if (g%n_cutout.GE.1) then 
                 GO TO 10 ! squaring stopped, no restart
         else
         endif
      else
      endif
      g%n_cutout=1 ! ie (BLR.EQ.1.0 .AND. g%n_cutout.EQ.0) and squaring stops today

10    continue

!c-----------------------------------------------------------------------------
!c     if coutout due to water stress or full boll load ie smi < 0.75
!c     10% of fruiting sites become inactive for frugen every day after 5 days
!c-----------------------------------------------------------------------------

      IF(g%smi.LT.c%cutout_smi_crit) g%n_cutout = g%n_cutout + 1   ! count days g%cutout                !const
      IF(g%n_cutout.GT.c%cutout_smi_days) 
     :                 g%sites1 = g%sites1 
     :                          + c%cutout_smi_site_red*g%size*g%ppm ! inactive g%sites          !const

      call pop_routine(myname)
      RETURN

20    continue
      IF(g%sites1.GT.g%sites) g%sites1 = g%sites
      g%n_cutout=0                  ! recovery complete, squaring resumes

30    continue

!c-----------------------------------------------------------------------------
!c     square production for this day
!c-----------------------------------------------------------------------------

      g%size = (g%sites-g%sites1)/g%ppm ! active g%sites per plant for FRUGEN & SURVIV
      IF(g%size.LT.1.0) g%size = 1.0 ! average  plants has 1 site
      IF(g%size.LT.0.5/g%ppm) g%size = 0.5/g%ppm ! average  plants has 1 site              !const

      IF(g%carcap_c.EQ.0.) THEN     ! either 1st g%day afer squaring or defoliated
!c          if(i.eq.isq+1) then                             ! day after 1st square
          IF(ndas.EQ.g%isq+1) THEN                           ! g%day after 1st square
             DFRU = p%SQCON*SQRT(g%size)*g%ppm    ! g%sites per g%dd
          ELSE
             call pop_routine(myname)
             RETURN                                       ! defoliated FRUGEN=0.
          ENDIF
      ELSE
          DFRU = p%SQCON*SQRT(g%size)*g%ppm*(1.-g%bload/
     :    g%cutout) ! g%sites per g%dd
          VSNSTR = ozcot_stress(c%vsnstr_low
     :                         ,c%vsnstr_high
     :                         ,c%vsnstr_a
     :                         ,g%vnstrs)                              !const
          DFRU = DFRU * VSNSTR
          IF((g%bollz+g%openz)/g%carcap_n .GE. 1.0) DFRU = 0. ! N limiting

      ENDIF

      PPM_ROW = g%ppm*g%rs               ! plants per m row for POPFAC
      POPFAC = 1./(1.+c%POPCON*PPM_ROW)   ! plant population factor within row
      DFRU = DFRU * POPFAC               ! adjust for plant population
      ozcot_frugen = DFRU * g%dd               ! today's squares
      IF(ozcot_frugen.LT.0.0) ozcot_frugen=0.0
!c      if(isyr.eq.77 .and. i.lt.isq+15) ozcot_frugen=ozcot_frugen*0.1 ! delay for 77/78 season - 23/6/88

      call pop_routine(myname)
      RETURN
      END


* ====================================================================
!c     subroutine fruit (i,iend)
       subroutine ozcot_fruit
* ====================================================================

!c      ages fruit, sums fruit in categories, estimates physiological
!c      shedding and survival of fruit.  calls s/r actfru to estimate
!c      new squares flowers and open bolls from counts. alternatively
!c      calls s/r ozcot_frugen to estimate new squares when counts not
!c      available.

      use OzcotModule
      implicit none
      include 'error.pub'

      real ozcot_frugen
      real ozcot_survive

!jh      real frudd
!jh      real wt
      integer cat
      integer age
      integer ndayfl
!jh      real bltme
      real surv
      integer das 
      integer lf
      integer mm
      integer mmm
      integer nfl
      integer if

!c      dimension frudd(8),wt(8),sfmcat(8),bltme(8),bpsum(300)
!jh      DIMENSION FRUDD(8),WT(8),SFMCAT(8),BLTME(8)
!jh      DATA FRUDD/50.,180.,350.,380.,520.,660.,870.,1100./
!jh      DATA BLTME/3*0.0,0.07,0.21,0.33,0.55,1.0/
!jh      DATA WT/.0104,.0272,.1441,.0988,.5042,.9617,1.0,.5785/

!c      data scboll /5.0,5.0,4.7,4.5,5.5,4*.0,7./  ! dp16,dp61,dp90,siok,sica
!c      data respcon/.025, .025, .02306, .01593, .02306, 4*.0,.025/ !  ditto
!c      data sqcon  /.021, .021, .02057, .02283, .02057, 4*.0,.021/ !   ditto
!c      data fcutout/.4789, .4789, .4789, .5411, .4789,  4*.0,.48/ !  ditto
!c      data flai   /1.0, 1.0, 0.87, 0.52, 0.87, 4*0.0,1./         !  ditto
!c      data popcon /.03633/
!c
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_fruit')

*- Implementation Section ----------------------------------
      call push_routine(myname)

!c----  re-intialise arrays and variables to zero where appropriate -------------

!c      if(i.eq.isq+1)then        ! 1st call of new season on day after 1st square
!c          do 10 n=1,300
!c              bpsum(n)=0.0
!c10        continue
!c          idayx = 0             ! flag to bollwt, called for 1st time this day
!c      endif

      DO 100 cat=1,Max_categories-1
          g%frucat(cat)=0.
          g%sfmcat(cat)=0.
          IF(cat.GT.7)GO TO 100
          DO 20 age=1,7
              g%fmkcat(cat,age)=0.
20        continue
100   continue

      g%frudw = 0.
      NDAYFL=0

!c---- compute boll period ------------------------------------------------------

      g%bper = 1.0/EXP(5.385-.0512*g%tempav) ! boll period Constable                   !const

!c-----------------------------------------------------------------------------
!c     the next loop ( do 200 l=....) goes through the fruit arrays
!c     starting with the element with oldest fruit and therefore the smallest
!c     index, in order to develop or abscise fruit, and put in categories.
!c     before entering the loop, find the oldest fruit.
!c-----------------------------------------------------------------------------

      LF=g%lfru(cat9)    ! 1st active element in fruit array, oldest immature fruit
!cpsc      if(lf.eq.0)lf=isq-isow-10 ! start in fruno 10 days before 1st square
      IF(LF.EQ.0)LF=g%isq-10 ! start in g%fruno 10 days before 1st square             !const

!cpsc      do 200 l=lf,i-isow-1 ! loop from oldest growing boll to previous day
      DO 200 das=LF,g%das-1 ! loop from oldest growing boll to previous g%day
          IF(das.GT.Max_das)GO TO 200
!cpsc          call bollwt(idayx,l)
          CALL ozcot_bollwt(das)

!c---- age & abscise marked fruit----------------------------------------

          DO 30 age = 1,max_age-1
              MM = (max_age-1)-age+1
              MMM = MM+1
              g%frmark (das,MMM) = g%frmark(das,MM)
30        continue
          g%frmark(das,age1)=0.                 ! CLEAR FIRST g%day
          IF(g%fruno(das).GT.0. .AND. g%frmark(das,age7).GT.0.)THEN ! fruit shed today?
              IF(g%frmark(das,age7).GT.g%fruno(das))THEN
!c                  write(2,999) i,jdate,l,fruno(l),frmark(l,7)
!c999               format
!c     *            (' too many marked fruit on i, jdate:',3i4,2f5.1)
!c                  frmark(l,7) = fruno(l)
              ENDIF
              g%fruwt(das) = g%fruwt(das)
     :                     * (g%fruno(das)-g%frmark(das,age7))
     :                     / g%fruno(das) ! adjust wt sheds
              g%fruno (das)=g%fruno(das)-g%frmark(das,age7) ! remove marked fruit

          ENDIF

!c---- sort fruit and marked fruit into age categories  -----------------------

          IF(g%fyzage(das).GT.c%FRUDD(cat3))
     *    g%bpsum(das)=g%bpsum(das)+g%bper          ! develop g%day das's bolls
          IF(g%n_def.GT.0) g%bpsum(das)=g%bpsum(das)/0.99 ! develop faster after defoliation      !const
          IF(g%iend.EQ.2 .AND. g%bpsum(das).GE.0.9) g%bpsum(das)=1.0 ! frost opens phys mat bolls !const

          DO 40 cat=1,Max_categories-1                              ! stay in loop until category found
              IF(cat.LT.cat4)THEN                    ! if yes, squares
                  IF(g%fyzage(das).LT.c%FRUDD(cat)) GO TO 204
              ELSE                              ! no, therefore flowers or bolls
                  IF(g%bpsum(das).LT.c%BLTME(cat))GO TO 204
              ENDIF
40        continue

!c------- if last loop completed, fruit are open bolls -------------------------

          IF(das.LE.g%lfru(cat9))GO TO 200 ! this days bolls already open?
          g%lfru(cat9)=das               ! this days bolls open today, reset marker
!c          if(i.le.idate)go to 202   ! use actual counts or not?
          g%openz=g%openz+g%fruno(das) ! simulated bolls open added to total
          g%openwt=g%openwt+g%fruwt(das)

!c202       continue

          g%fruno(das)=0.0 ! delete open bolls from array
          g%fruwt(das)=0.
          GO TO 200

204       continue
          IF(das.LE.g%lfru(cat))GO TO 206 ! this days fruit in category cat  yet?
          g%lfru(cat)=das               ! this days fruit into category cat today.
          IF(cat.NE.cat4)GO TO 206       ! category is flowers?
          NDAYFL=NDAYFL+1           ! count this days flowering
!c          if(i.gt.idate)go to 206   ! using actual counts or not?
!c          fruno(l)=0.0              ! clear for actual counts
!c          go to 200

206       continue
          g%frucat(cat)=g%frucat(cat)+g%fruno(das)       ! sum fruit nos in categories
          IF(cat.GE.cat4) g%frudw = g%frudw+g%fruwt(das)  ! sum dry wt of fruit
          IF (cat.GT.cat7) GO TO 200
          DO 50 age=2,6                                 ! loop thro marked fruit
              g%fmkcat(cat,age)=g%fmkcat(cat,age)+g%frmark(das,age) ! sum marked fruit
              g%sfmcat(cat)=g%sfmcat(cat)+g%frmark(das,age)    ! sum marked fruit for g%bload
50        continue

200   continue

!c---- total squares, green (growing) bolls, open bolls---------------------

      g%squarz=0.
      g%bollz=0.
      g%bload=0.                                     ! reset
      DO 60 cat=1,Max_categories-1
          IF (cat.LE.cat3) g%squarz=g%squarz+g%frucat(cat)  ! total squares
          IF (cat.GE.cat4.AND.cat.LE.cat8) g%bollz=g%bollz+g%frucat(cat) ! total bolls
          g%bload=g%bload+(g%frucat(cat)-g%sfmcat(cat))*c%WT(cat)  !  boll load
60    continue

!c---- this day's production of fruit -----------------------------------------

!c      call carrying_capacity(i)
      CALL ozcot_carrying_capacity
      IF(g%bload.GT.g%carcap_c .OR. g%bollz+g%openz.GT.g%carcap_n)
     *CALL ozcot_overload                                  ! abort excess fruit

!c      if (i.le.idate)then                            ! use counted fruit?
!c      if (das.le.idate)then                            ! use counted fruit?
!c          call actfru (i)
!c          call actfru
!c          call update(1,1,daysqz,squarz,frudd)
!c          if(lfru(4).ne.0..or.daysfl.ne.0.)
!c     *        call update(4,ndayfl,daysfl,bollz,frudd)
!c          if(lfru(9).ne.0..or.daysop.ne.0.)
!c     *        call update(9,1,daysop,openz,frudd)
!c      else
!cpsc           icount = i               ! dummy parameter to use frugen
!c          fruno(i-isow)=frugen(i)  ! frugen is function generating squares
          g%fruno(g%das)=ozcot_frugen(g%das) ! ozcot_frugen is function generating squares

!c      end if

!c---- mark this day's fruit for physiological shedding      -----------------

      SURV = ozcot_survive(g%carcap_c,g%bload) ! square survival rate

!c      if(jdate.lt.59) surv = 0.1       ! for krs with delayed protection
!c      surv =surv*0.33                  ! for pest damage in 1972/3 namoi

!c      frmark(i-isow,1)=fruno(i-isow)*(1.-surv)
!c      if(frmark(i-isow,1).lt.0.) frmark(i-isow,1)=0.0
!c      fmkcat(1,1)=fmkcat(1,1)+frmark(i-isow,1)
      g%frmark(g%das,age1)=g%fruno(g%das)*(1.-SURV)
      IF(g%frmark(g%das,age1).LT.0.) g%frmark(g%das,age1)=0.0
      g%fmkcat(cat1,age1)=g%fmkcat(cat1,age1)+g%frmark(g%das,age1)
      IF(NDAYFL.EQ.0)GO TO 501
      SURV = ozcot_survive(g%carcap_c,g%bload) ! boll survival rate
!c      surv =surv*0.33                  ! for pest damage in 1972/3 namoi

      DO 70 NFL = 1,NDAYFL
          IF = g%lfru(cat4)-NFL+1
          g%frmark(IF,age1) = g%fruno(IF)*(1.-SURV)
          IF(g%frmark(IF,age1).LT.0.)g%frmark(IF,age1) = 0.0
          g%fmkcat(cat4,age1) = g%fmkcat(cat4,age1)+g%frmark(IF,age1)
70    continue

!c---- add new fruit to totals ------------------------------------------------

  501 CONTINUE

!c      sites = sites+fruno(i-isow)
!c      if(i.le.idate)return ! squarz & frucat updated in update
!c      squarz = squarz+fruno(i-isow)
!c      frucat(1) = frucat(1)+fruno(i-isow)
      g%sites = g%sites+g%fruno(g%das)
      IF(g%das.LE.g%idate) then
         !RETURN ! g%squarz & g%frucat updated in UPDATE
      else
         g%squarz = g%squarz+g%fruno(g%das)
         g%frucat(cat1) = g%frucat(cat1)+g%fruno(g%das)
      endif
!c-----------------------------------------------------------------------------

      call pop_routine(myname)
      RETURN
      END


* ====================================================================
!c      subroutine harvest(iend)
      subroutine ozcot_harvest
* ====================================================================

!c     this subroutine simulates defoliation and picking
!c     use n_def, n_pick and j_pick for cost of defoliation:       nb action
!c         cost of defoliant is n_def * $25                 <-- for gross margins
!c         cost of picking is n_pick * $?                   <-- for gross margins
!c         cost of delay is (j_pick - 91) * $ per day       <-- for gross margins
!c              if 1st april (day 91) is reference date.
!c     likelihood of 2nd pick is indicated. currently 10 boll/m (bollz=10)
!c     assumed to be worth picking, depends on price of cotton and cost of
!c     picking, ask growers. should be part of management model.
!c     date of 2nd pick and partition of yield between 1st and 2nd pick are
!c     subjects for future development in a more comprehensive whole farm
!c     management model.

      use OzcotModule
      implicit none
      include 'error.pub'

      character  string*200            ! output string

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_harvest')

*- Implementation Section ----------------------------------
      call push_routine(myname)

      IF(g%n_def.EQ.0 .AND. g%squarz/g%ppm.GT.2.) then
         call pop_routine(myname)
         RETURN ! too many squares
      else
      endif
      IF(g%openz/(g%bollz+g%openz).GT.c%OPEN_DEF/100. .AND. g%j_pick.EQ.
     :0) THEN
          IF(g%n_def.EQ.0) THEN
               g%n_def = 1                               ! 1st defoliant spray     !const
               g%i_def = g%iday                          ! g%day of 1st defol.
!c               write(2,100) n_def, jdate, i_def
               write (string, '(4x, a, i4, a)') 
     :                'First defoliant spray '
     :               , g%iday,' days from sowing.'
               call write_string (string)
           ENDIF
          IF(g%n_def.EQ.1 .AND. g%iday-g%i_def.GE.10) THEN ! 10 days since 1st?
               IF(g%alai.GT.0.2) THEN
                   g%n_def=2                             ! 2nd defoliant spray      !const
                   g%i_def=g%iday                        ! g%day of 2nd defol
!c               write(2,100) n_def, jdate, i_def
                  write (string, '(4x, a, i4, a)') 
     :                'Second defoliant spray '
     :               , g%iday,' days from sowing.'
                  call write_string (string)
               ELSE
                   g%j_pick = g%jdate                    ! date of picking
                   g%n_pick = 1                          ! count picks               !const
                   write (string, '(4x, a, i4, a)') 
     :                'First Picking '
     :               , g%iday,' days from sowing.'
                  call write_string (string)
!c                   write(2,101) j_pick
                   IF(g%bollz.GT.10.) THEN                ! 10 bolls worth picking
                       g%n_pick = 2                      ! count picks                !const
!c                       write(2,102)
                     write (string, '(4x, a)')
     :                'There are sufficient bolls for a Second Picking.'
                     call write_string (string)
                   ENDIF
               ENDIF
          ENDIF
          IF(g%n_def.EQ.2 .AND. g%iday-g%i_def.EQ.10) THEN
              g%j_pick = g%jdate                         ! date of picking
              g%n_pick = 1                               ! count picks
!c              write(2,101) j_pick
                  write (string, '(4x, a, i4, a)') 
     :                'First Picking '
     :               , g%iday,' days from sowing.'
                  call write_string (string)
              IF(g%bollz.GT.10.) THEN
                  g%n_pick = 2                           ! count picks
!c                  write(2,102)
                     write (string, '(4x, a)')
     :                'There are sufficient bolls for a Second Picking.'
                     call write_string (string)
              ENDIF
          ENDIF
      ENDIF
      IF(g%j_pick.NE.0 .AND. g%bollz.LT.1.0) THEN
              g%iend = 2                                 ! terminate crop                 !const
!c              write(2,103) jdate, iday
              write (string, '(4x, a)')
     :                'Crop terminated'
     :              //': no further boll growth, all bolls forced open.'
              call write_string (string)
      ENDIF
!c
!c100   format(' defoliant spray',i2,' on day',i4,',',
!c     *       i4,' days from sowing.')
!c101   format(' first pick 0n',i4)
!c102   format(' there are sufficient bolls for a 2nd pick')
!c103   format(' simulation terminated on day',i4,','i4,
!c     *': no further boll growth, all bolls forced open.')
      call pop_routine(myname)
      RETURN
      END


* ====================================================================
!c      subroutine hfunc (i)
      subroutine ozcot_hfunc
* ====================================================================
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c      calculates daily heat units based on a 12.0 deg c base      c
!c      temperature.  the heat unit sum is based an integrated      c
!c      diurnal sin-wave with the maximum and minimum daily temp-   c
!c      eratures as the peak and trough, respectively.  heat units  c
!c      are not allowed to accumulate above a cutoff of 30.0 c. Changed to 40    c
!c      the base (baset) and cutoff (hucut) temperatures are set in c
!c      the 'init' subroutine.                                      c
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
      use OzcotModule
      implicit none
      include 'error.pub'

      real pi
      real amp
      real tmax
      real zeta

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_hfunc')

*- Implementation Section ----------------------------------
      call push_routine(myname)

!c
      PI=3.14159                                                       !const
!c      tempav=(tempmn+tempmx)/2.
      AMP=g%tempmx-g%tempav
      TMAX=g%tempmx
      IF(TMAX.GE.c%HUCUT) TMAX=c%HUCUT
      IF(g%tempmn.GE.c%BASET) GO TO 10
      IF(TMAX.LE.c%BASET) GO TO 20
      ZETA=ASIN((c%BASET-g%tempav)/AMP)
      g%hunits=1./PI*(AMP*COS(ZETA)+(g%tempav-c%BASET)*(PI/2.-ZETA))
      GO TO 30
   10 g%hunits=(TMAX+g%tempmn)/2.-c%BASET
      GO TO 30
   20 CONTINUE
      g%hunits=0.0
   30 CONTINUE

      call pop_routine(myname)
      RETURN
      END


* ====================================================================
!c      subroutine init
      subroutine ozcot_INITIAL()
* ====================================================================
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c      initializes variables in all common blocks                  c
!c                                                                  c
!c      key variables:                                              c
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
      use OzcotModule
      implicit none
      include 'error.pub'

!cpc      real soltro, spi, cumep, cumes, cumet, deltsw, dal
!cpc      real bolln, bolnc
!cpc      integer nfert
!      integer lastiday
      integer j
!      integer isdex
!      integer ncnt
!      integer lastfr
      integer k

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_initial')

*- Implementation Section ----------------------------------
      call push_routine(myname)

!      block data ozcot_initials
!c
!c
!      g%scboll = (/5.0,5.0,4.7,4.5,5.5    
!     :            , .0, .0, .0, .0,7./) ! DP16,DP61,DP90,SIOK,SICA
!      g%respcon= (/.025, .025, .02306, .01593, .02306
!     :            , .0, .0, .0, .0,.025/) !  ditto
!      g%sqcon  = (/.021, .021, .02057, .02283, .02057
!     :            , .0, .0, .0, .0,.021/) !   ditto
!      g%fcutout= (/.4789, .4789, .4789, .5411, .4789
!     :            , .0, .0, .0, .0,.48/) !  ditto
!      g%flai   = (/1.0, 1.0, 0.87, 0.52, 0.87
!     :            , .0, .0, .0, .0,1./)       !  ditto
!jh      g%popcon =.03633
!c
!jh      g%fburr = 1.23                     ! factor sc/boll to sc+burr/boll
!c
!jh      data leaf_res_n_conc/0.02/
!c
!      end

!c------------------------------------------------------------------------------
!c      'bclim' climate parameters
!c------------------------------------------------------------------------------
!c
!jh      g%mode=2 ! OZCOT1: -1=calib'n, 1=valid'n; OZCOT2: 2=full simulation
!c
!cpsc      nszn = 1
!cpsc      i = 0

      g%iend=0
      g%iday=0
!cpc   lastiday = 0
!cpsc      jdate=0
!cpsc      imet=0
      g%tempav=0.0
      g%tempre=0.0
      g%tempyd=0.0
      g%alint=0.0
      g%hunits=0.0
!jh      g%hucut=40.0 ! ABH changed from 30 to 40 - 11/11/83
!jh      g%baset=12.0
      g%asoil=0.0
!cpc   soltro=0.0
      g%stemp=0.0
!c      eo=0.0
!c      eos=0.0
!cpc     spi=0.0
!cpc     cumep=0.0
!cpc     cumes=0.0
!cpc     cumet=0.0
!c
!c      tempmx=0.0
!c      tempmn=0.0
!c      solrad=0.0
!c      rain=0.0
!c      newran= 0
!c------------------------------------------------------------------------------
!c     irrigation variables
!c------------------------------------------------------------------------------
      DO 10 J=1,Max_rrig
!c          igday(j)=0
          g%rrig(J)=0.0
10    continue
!c------------------------------------------------------------------------------
!c      'bsoil' soil profile parameters
!c------------------------------------------------------------------------------
      g%isw = 0  ! flag to initialise water balance in SOLWAT
!jh      g%ambda=1.44 ! Priestly-Taylor  daRosa 1983
!jh      g%ul1=1.4  ! limit for stage 1 g%es  daRosa 1983
!jh      g%cona=0.35 ! g%es rate in stage 2  daRosa 1983
!cpc   isdex=0.0
      g%nlayr=0
!c      sw=0.0
!c      ul=0.0
      g%def=0.0
      g%nirr=0
      g%twater=0.
!cpc   deltsw=0.0
        g%rtsw=1.0
      DO 20 J=1,6
!c      dlayr(j)=0.0
!c     ullayr(j)=0.0
!c      swlayr(j)=0.0
      g%trans(J)=0.0
   20 CONTINUE
!c------------------------------------------------------------------------------
!c      'bplnt' plant parameters
!c------------------------------------------------------------------------------
!cpc     ncnt=0
      g%idate=0
      g%ilai=0
      g%iemrg=0
      g%isow=0
      g%isq=0
      g%ivar=0
      g%crspce=0.0
      g%ppm=0.0
      g%esum=0.0
      g%alai=0.0
!cpc   dal=0.0
      g%shedlf=0.0
      g%frudw=0.0
      g%smi=0.0
      g%sdepth=0.0
      g%rtdep=0.0
      g%rtgrow=0.0
!c------------------------------------------------------------------------------
!c      'bnitr' soil nitrogen transformation parameters
!c------------------------------------------------------------------------------
!cpc   nfert=0
      g%uptakn=0.0
        g%availn=0.0

!c      do 30 j=1,2
!c      nday(j)=0
!c      snaplc(j)=0.0
!c   30 continue
!c------------------------------------------------------------------------------
!c      'fruits' parameters are numbers,weights,abscission counts
!c      for bolls and squares.
!c------------------------------------------------------------------------------
!cpc   lastfr=0
      g%bgrvar=0.0
      g%dd=0.0
      g%ddmerg=0.0
      g%sumdd=0.0
      DO 40 J=1,Max_categories-1
      g%frucat(J)=0.0
      g%lfru(J)=0
   40 CONTINUE
      g%lfru(cat9)=0

      DO 50 J=1,Max_das
      g%dlai(J)=0.0
      g%ddw_l(J) = 0.0
      g%bpsum(j) = 0.0
      g%fyzage(J)=0.0
      g%fruno(J)=0.0
      g%fruwt(J)=0.0
      DO 50 K=1,7
      g%frmark(J,K)=0.0
      IF(J.LT.Max_categories-1) g%fmkcat(J,K)=0.0
   50 CONTINUE

!c------------------------------------------------------------------------------
!c      'totals' counts of squares, bolls, and sites at any given time
!c------------------------------------------------------------------------------

      g%bload=0.0
      g%bollz=0.0
      g%openz=0.0
      g%openwt=0.0
      g%sites=0.0
!cpc
      g%sites1 = 0.0
      g%squarz=0.0
!c------------------------------------------------------------------------------
!c      'index' stress and survival indices.
!c------------------------------------------------------------------------------
      g%carcap=0.0
      g%carcap_c = 0.0
      g%carcap_n = 0.0
      g%cutout=0.0
      g%vnstrs=1.0
      g%fnstrs=1.0
      g%idayco = 0
      g%last_day = 0
!c------------------------------------------------------------------------------
!c      'counts'
!c------------------------------------------------------------------------------
!c      do 60 j=1,25
!c      jco(j)=0
!c      sqcnt(j)=0.0
!c      blcnt(j)=0.0
!c      opcnt(j)=0.0
!c   60 continue
!c------------------------------------------------------------------------------
!c      'bpnitr' are for plant nitrogen.
!c------------------------------------------------------------------------------
      g%uptakn=0.0
      g%vegn=0.0
!cpc   bolln=0.0
      g%plantn=0.0
!cpc   bolnc=0.0
      g%strucn=0.0
      g%frun=0.0
!c------------------------------------------------------------------------------
!c     /yield/ for output with yield
!c------------------------------------------------------------------------------
      g%alaiz=0.
      g%ilaiz=0
      g%plntnz=0.
      g%iplntn=0
      g%sqzx = 0.
      g%isqzx = 0
      g%def_last=0.
!c------------------------------------------------------------------------------
!c     /pick/ variables to simulated defoliation and picking
!c------------------------------------------------------------------------------
      g%j_pick=0
      g%n_pick=0
      g%n_def=0
      g%i_def=0
!jh      c%OPEN_DEF = 60.
!c------------------------------------------------------------------------------
!c     /sow/
!c------------------------------------------------------------------------------
!jh      g%iwindow = 60     ! width of sowing window
!jh      g%sow_sw = 0.0     ! for subsoil water rule
!c------------------------------------------------------------------------------
!c     /drywt/ variables for simulation of dry weight increase
!c------------------------------------------------------------------------------
!jh      g%a_root_leaf = 1.01 ! allometric constant root:leaf. Huxley's data 1964
!jh      g%a_stem_leaf = 1.25 ! allometric constant stem:leaf. Huxley's data 1964
!jh      g%e_par = 2.5        ! g%g/MJ Charles-edwards g%et al 1986
!jh      g%fburr = 1.23       ! ABH
!jh      g%specific_lw = 58.0 ! g%g/m2  GAC 71/72, Hoffman & Rawlins 1971, Ben-Porath
!jh      g%t_opt = 25.        ! Constable 1981
!jh      g%t_base = 8.        ! Constable 1981
!jh      g%wt_area_max = 150. ! Hesketh and Low 1968, Hoffman & Rawlins = 80.
!jh      g%wt_area_min = 30.0 ! Huxley 1964
!jh      g%embryo = 0.75      ! dry weight of seedling at emergence
!jh      g%f_leaf = 0.6       ! proportion of leaf dry weight at emergence
!jh      g%f_stem = 0.15      ! ditto for stem
!jh      g%f_root = 0.25      ! ditto for root  -  data by extrapolation from Huxlry

      g%bollgr = 0.0
      g%bper = 0.0
      g%dlai_pot = 0.0
      g%dw_boll = 0.0
      g%dw_leaf = 0.0
      g%dw_root = 0.0
      g%dw_stem = 0.0
      g%dw_total = 0.0
      g%total_n = 0.0
      g%reserve = 0.0
      g%res_cap = 0.0
      g%root_feedback = 0.0

      call pop_routine(myname)
      RETURN
      END


* ====================================================================
!c      subroutine istsq (i,nszn)
      subroutine ozcot_istsq
* ====================================================================

!c     identifies first square event and gives initial value for
!c     fruno(1),sites & squarz.
!c     delayed by water stress (smi < 0.25) and cold shock (tempmn < 11)
!c     currently sets isq=i, should be isq=iday for consistency between seasons
!c     when convenient, change and check all refs to isq

      use OzcotModule
      implicit none
      include 'error.pub'

!      real ddisq


!      DIMENSION DDISQ(10)

!c      data iszn/0/                         ! flag for new season
!      DATA DDISQ/9*420.,320./ ! Constable (pers. comm. 1983), 10=Empire

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_istsq')

*- Implementation Section ----------------------------------
      call push_routine(myname)

      IF(g%idate.NE.0) GO TO 40

!c     no counts - simulate first square

!c      if(iszn.ne.nszn) then                ! is this a new season?
!c          iszn = nszn                      ! reset flag
!c          delay = 0.0                      ! delay in day degrees
!c      end if

!cpsc    add delay to common block

      IF(g%smi.LT.c%smi_delay_crit) THEN               ! water stress delays squaring
          g%delay = g%delay+(1.-(g%smi/c%smi_delay_crit))*g%hunits
      END IF

      IF(g%tempmn.LT.c%cold_shock_delay_crit) 
     :                        g%delay = g%delay+c%cold_shock_delay ! cold shock Constable (pers. comm)

      IF(g%sumdd.LT.p%DDISQ+g%delay)then
         call pop_routine(myname)
         RETURN
      else
      endif
!c      fruno(i-isow)=1.0*ppm  ! average plant has 1 square
!c      fruno(i-isow)=0.5      ! as in 1983/84 version
      g%fruno(g%das)=1.0*g%ppm ! average plant has 1 square
      g%fruno(g%das)=0.5  ! as in 1983/84 version
      GO TO 43
!c
!c      using counts
!c
   40 CONTINUE
!c      do 41 n=1,25
!c      if(jco(n).eq.0)return !  no squares counted yet
!c      if(i.lt.jco(n)+1)return
!c      if(sqcnt(n+1).gt.0.)go to 42 ! when jco(n) eq i and squares at next count
!c   41 continue
!c      return
!c   42 continue
!c      fruno(i-isow)=sqcnt(n+1)/(jco(n+1)-jco(n))
!c      next=n ! for actfru
!c
!c     square & site production on first day
!c
   43 CONTINUE
!c      squarz=squarz+fruno(i-isow) ! sum squares
!c      sites=sites+fruno(i-isow) ! sum sites
!c      isq=i                     ! should be isq=iday see above
      g%squarz=g%squarz+g%fruno(g%das) ! sum SQUARES
      g%sites=g%sites+g%fruno(g%das) ! sum g%sites
      g%isq=g%das                ! should be g%isq=g%iday see above

      call pop_routine(myname)
      RETURN
      END


* ====================================================================
!c      subroutine laigen (i)
      subroutine ozcot_laigen
* ====================================================================
!c
!c     estimates current lai. generates new leaves daily as a
!c     function of dd and fruiting site production. initial area
!c     is a function of dd to first square then a function of new
!c     sites. relative rate of area expansion a function of dd
!c     to 1st square, then a function of dsites .
!c      all leaf areas are square meters per plant except alai
!c      which is m**2/m**2 i.e. lai.
!c
      use OzcotModule
      implicit none
      include 'error.pub'

      real ozcot_stress
      real ozcot_senlf

      real dlds
      real dlds_x
      real vlnstr
      real flfsmi
      real actrgr
      real alaix
      real ddleaf
      integer index
      integer in
      integer l

!jh      DATA ACOTYL /.00035/
!jh      DATA RLAI /.00835/
!jh      DATA DLDS /0.0362/
!jh      DATA LEAF_RES_N_conc /0.02/

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_laigen')

*- Implementation Section ----------------------------------
      call push_routine(myname)


!c------- initialise leaf area on day of emergence -----------------------------

        IF(g%das.EQ.g%iemrg) THEN                    ! already emerged?
!c           dlai(iemrg-isow)=acotyl*ppm            ! initial area
!c           alai=dlai(iemrg-isow)
!c           lastlf=iemrg-isow                      ! set index for oldest leaf
            g%dlai(g%iemrg)=c%acotyl*g%ppm      ! initial area
            g%alai=g%dlai(g%iemrg)
            g%lastlf=g%iemrg                  ! set index for oldest leaf
            call pop_routine(myname)
            RETURN
        ENDIF

!c------- calculate rate of leaf area expansion --------------------------------

!jh        A  =   0.1847  !
!jh        B1 =  -0.1165  ! constants for LAI calibration eqn
!jh        B2 =  -0.01514 ! 29 April 1988
!jh        B3 =   0.01984 ! expts WN8283 & 8384

        dLdS_X = (c%a+c%b1*0.75+c%b2*g%vpd+c%b3*0.75*g%vpd) ! sqrt area/site, no water stress
        IF(dLdS_X.LT.0.) dLdS_X = 0.
        dLdS_X = dLdS_X**2                      ! area per site, no water stress

        dLdS = (c%a+c%b1*g%smi+c%b2*g%vpd+c%b3*g%smi*g%vpd) ! sqrt area per site
        IF(dLdS.LT.0.) dLdS = 0.
        dLdS = dLdS**2                          ! area per site

        FLFSMI = ozcot_stress(c%flfsmi_low
     :                        ,c%flfsmi_high
     :                        ,c%flfsmi_a
     :                        ,g%smi) ! pre-squaring

!c-------------------------------------------------------------------------------

        IF(g%isq.EQ.0) THEN               ! crop not yet squaring

            ACTRGR=c%rlai                   ! actual RGR
            ALAIX=g%alai                  ! save previous LAI
            INDEX=IFIX(g%dd)              ! index for DO loop below to grow leaf

            DO 10 IN=1,INDEX
                ALAIX=ALAIX*(1.+ACTRGR)   ! grow leaf without water stress
10          continue
            g%dlai_pot=ALAIX-g%alai ! days increase without water stress

            ACTRGR=ACTRGR*FLFSMI          ! water stress
            ALAIX=g%alai                  ! save previous LAI again

            DO 11 IN=1,INDEX
                ALAIX=ALAIX*(1.+ACTRGR)   ! grow leaf with water stress
11          continue
            g%dlai(g%iday)=ALAIX-g%alai   ! days increase with water

        ELSE                              ! crop now squaring
                                              ! without water stress
            dLdS_X = dLdS_X*p%flai    ! adjust for variety, 87 MKI calib'n
            g%dlai_pot = g%fruno(g%iday-1)*dLdS_X ! days incr in LAI
                                              ! with water stress
            dLdS = dLdS*p%flai        ! adjust for variety, 87 MKI calib'n
            g%dlai(g%iday) = g%fruno(g%iday-1)*dLdS ! days incr in LAI

        ENDIF
        VLNSTR = ozcot_stress(c%vlnstr_low
     :                        ,c%vlnstr_high
     :                        ,c%vlnstr_a
     :                        ,g%vnstrs)
        g%dlai(g%iday) = g%dlai(g%iday)*VLNSTR ! adjust for N stress
        g%dlai_pot = g%dlai_pot*VLNSTR ! adjust for N stress
        g%alai=g%alai+g%dlai(g%iday)

!c*******senescence ***************************************************

        DDLEAF=ozcot_senlf(g%bload,g%alai,g%carcap_c,g%smi)
        IF(g%n_def.EQ.1 .AND. g%iday-g%i_def.GT.7) DDLEAF = DDLEAF*0.33 ! 1st defol'n
        IF(g%n_def.EQ.2 .AND. g%iday-g%i_def.GT.7) DDLEAF = DDLEAF*0.0 ! 2nd defol'n
        g%shedlf=0.
        g%leaf_res = 0.                 ! initialise for this g%day
        IF(g%lastlf.EQ.0)GO TO 21       ! called after measured LAI finished
!c       do 20 l=lastlf,i-isow           ! loop thro unshed leaves
        DO 20 L=g%lastlf,g%das         ! loop thro unshed leaves
        IF(g%fyzage(L).LT.DDLEAF)GO TO 21 ! are this days leaves shed today?
        g%alai=g%alai-g%dlai(L)         ! reduce LAI
        g%shedlf=g%shedlf+g%dlai(L)     ! sum area of shed leaves
        g%dlai(L)=0.                    ! g%day,g%s area now zero
      g%dw_leaf = g%dw_leaf-g%ddw_l(L) ! reduce leaf dry matter
      g%leaf_res = g%leaf_res+g%ddw_l(L) ! sum this g%day's residues
      g%ddw_l(L) = 0.0                ! this g%day's leaf wt now zero
      g%lastlf=L+1 ! set index for oldest remaining leaves.
20      continue
21      continue

      g%leaf_res_n = g%leaf_res * c%leaf_res_n_conc ! N content of leaf residues

      call pop_routine(myname)
      RETURN
      END


* ====================================================================
!c      subroutine metdat2 (i,iend)
      subroutine ozcot_metdat2
* ====================================================================
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c      subroutine to check input climate data for missing or er-   c
!c      roneous values.  input variable units are assumed input     c
!c      and converted as follows:                                   c
!c          temperature -      deg c*10 to deg c                    c
!c          rainfall    -      mm/day*10 to cm/day                  c
!c          rainfall    -      inches/day to cm/day                 c
!c          solrad      -      ly/day (not converted)               c
!c                                                                  c
!c      unique variables:                                           c
!c          qa = an upper boundary for solrad                       c
!c          solrto = needed constant for net radiation in "evap"    c
!c                                                                  c
!c       when temperature (including wet and dry bulb) and          c
!c       radiation are missing or not avaialble, ezstimates are     c
!c       made as function of days since last rain.                  c
!c       consequently a full suite of met data elements can be      c
!c       generated from rainfall alone.                             c
!c                                                                  c
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      use OzcotModule
      implicit none
      include 'data.pub'
      include 'error.pub'

!cjh      integer nsince
      integer ndate
      real root
      real rclear
      real srad
      real wet_depress
      real date

!cjh      DATA NSINCE/0/
!c
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_metdat2')

*- Implementation Section ----------------------------------
      call push_routine(myname)

!c         **** read met data  ***************************************
!c
!c       read(1,151,end=153) jdate,imyr,
!c     *  rain,epan,tempmx,tempmn,tempdy,tempwt,wind,solrad
!c151    format(4x,i3,5x,i4,f5.0,f4.1,2f4.0,2f4.1,f4.0,f4.0) ! new

!c     **** climate data now read in as part of interface. ****

!cpsc        solrad = solrad / 0.04186
!cpsc        rain = rain / 10.0
        g%wind = 0.
        g%tempdy = 0.
        g%tempwt = 0.
!cpsc        epan = 0.
!c

!cjh
       IF(g%jdate.EQ.1) THEN               ! new year?
         g%mdpy = 365                     ! reset days per year
         IF((g%imyr/4*4).EQ.g%imyr) g%mdpy=366 ! leap year
       ENDIF


!c       if(epan.eq.0. .or. epan.eq.99.9)      epan=epanx
!c       epanx=epan

!c       if(wind.eq.0. .or. wind.eq.999)  then
!c           if(windx.ne.999) then
!c               wind=windx
!c           else
!c               wind=0
!c           endif
!c       endif
!c       windx=wind

!c      go to 155
!c153   iend=1 ! flag end of data
!c      return
!c
!c155   continue
!cpsc      epan=epan/10.
!c
!c         **** check and convert rainfall (cm) ****
!c
!c        if(rain.lt.0.0) rain=0.0
!c       rain=rain/100.0

!c  local rain data

!c       if(newran.eq.0) go to 100
!c       if(jdate.ge.nrnsdy(1).or.jdate.le.nrnsdy(newran)) rain=0.0
!c        do 10 j=1,newran
!c         if(jdate.eq.nrnsdy(j)) rain=ransub(j)/10.
!c10      continue
!c100    continue

!c  days since rain

      IF(g%isow.GT.0) THEN  ! from sowing to first boll
        IF(g%bollz.EQ.0.0) THEN
          g%rrig(rain_preboll) = g%rrig(rain_preboll)+g%rain     ! accumulate rainfall before bolling
        ELSE IF(g%bollz.GT.0.0 .AND. g%openz/(g%openz+g%bollz).LT.0.2)
     :  THEN ! bolling
          g%rrig(rain_postboll) = g%rrig(rain_postboll)+g%rain     ! accumulate rainfall in bolling
        ENDIF
      ELSE
          g%rrig(rain_fallow) = g%rrig(rain_fallow)+g%rain     ! accumulate rainfall in fallow
      ENDIF

      IF(g%rain.LE.0.1) THEN
        IF(g%NSINCE.LT.1) g%NSINCE=1
        g%NSINCE = g%NSINCE + 1
      ELSEIF(g%NSINCE.GT.1) THEN
        g%NSINCE=1                    ! 1st g%day of g%rain period, light g%rain
        IF(g%rain.GT.10.) g%NSINCE = 0 ! heavy g%rain
      ELSE
        g%NSINCE = 0                  ! g%rain period
      ENDIF
      ROOT =FLOAT(g%NSINCE)
      IF(ROOT.GT.5.) ROOT=5.
      IF(ROOT.GT.0.) ROOT = SQRT(ROOT)
      IF (g%NSINCE.GT.9) g%NSINCE=9


!c      **** check solar radiation and fill in for missing data. ****
!c      **** please notice that in the following lines location  ****
!c      **** specific equations are used.    (cal/cm2)           ****

!c  calculate extraterrestrial radiation at nars

!c       xrad=(jdate+9)*(360./365.)*.0174533 ! day of year as angle in radians
!c        qa=749.6+(302.4*sin(1.562+xrad)) ! extra terrestrial radiation(nars)

!c       if(solrad.lt.0.0)solrad=0.0
!c        if(solrad.gt.0.0 .and. solrad.ne.999.) go to 30

!c estimate missing ground measured solar radiation

!c       if(jdate.gt.135)go to 152
!c       qqa=0.66-0.4708*exp(-0.75*nsince)        ! q/qa for days 1-135
!c        if(nsince.le.1)qqa=0.4658-0.003485*rain
!c        go to 160
!c152    continue
!c       if(jdate.ge.225)go to 154
!c       qqa=0.5892-0.7986*exp(-1.219*nsince)     ! q/qa for days 136-225
!c        if(nsince.le.1)qqa=0.1382+0.2777*exp(-0.04375*rain)
!c        go to 160
!c154    continue
!c       qqa=0.63324-0.7693*exp(-1.0*nsince)
!c        if(nsince.le.1)qqa=0.2148+0.2087*exp(-0.01875*rain)
!c160    continue
!c       solrad=qa*qqa           ! est of ground rad =f(days since rain)
!c        solrad_min = qa*0.18      ! minimum - 0.18 from brutsart(1982)
!c       if(solrad.lt.solrad_min)solrad=solrad_min
!c30     continue
!c
!c   actual solar/clear day solar radiation ratio for longwave estimate
!c
        RCLEAR=551.52+246.40*SIN(0.0172*(g%jdate+99.96)) !CLEAR g%day SOL g%rad(NARS)
        SRAD=g%solrad
        IF(SRAD.GT.RCLEAR) SRAD=RCLEAR
        g%solrto=SRAD/RCLEAR
!c        solrad=srad

!c       **** check and convert air temperatures (deg c) ****
!c
!c       tempmx=tempmx/10.0
!c       tempmn=tempmn/10.0

!c      if(tempmx.eq.0. .or. tempmx.eq.99.9) then

!c  estimate missing tempmx

!c       tmaxxx=26.24+8.325*sin(1.172+xrad)   ! tmax 8 or more days after rain
!c       ftmax=1.03-0.1812*exp(-0.1953*nsince)! tmax/tmaxxx
!c       tempmx=tmaxxx*ftmax               ! tmax=f(days since rain)
!c       if(rain.gt.4.0)tempmx=tmaxxx*.83
!c       if(rain.gt.5.0)tempmx=tmaxxx*.8

!c      endif

!c      if(tempmn.eq.99.9) then

!c  estimate missing tempmn

!c       tminxx=11.45+8.144*sin(1.078+xrad)             ! tmin on dry days
!c       if(nsince.le.1)tminxx=13.47+5.949*sin(1.+xrad) ! tmin on wetdays
!c       if(nsince.eq.2)ftmin=.993                      ! first day after rain
!c       if(nsince.gt.2)ftmin=.925+.01321*nsince
!c       if(nsince.le.1)ftmin=1.003+.005169*rain-.0001039*rain**2
!c       tempmn=tminxx*ftmin                         ! estimate of tmin

!c      end if

!c       estimate wet and dry bulb when odd day missing
!c
      IF((reals_are_equal(g%tempdy,0.0)) .OR. 
     :   (reals_are_equal(g%tempdy,99.9))) THEN
          g%tempdy = -.54+0.57*g%tempmx+0.40*g%tempmn
      ENDIF
      IF((reals_are_equal(g%tempwt,0.0)) .OR.
     :   (reals_are_equal(g%tempwt,99.9))) THEN
          WET_DEPRESS = -3.103+0.28*g%tempmx-0.07*g%tempmn+0.62*ROOT
          IF(WET_DEPRESS.LT.0.) WET_DEPRESS=0.
          g%tempwt = g%tempdy-WET_DEPRESS
      ENDIF
!c
!c          **** calculate soil heat flux (cal/cm2) ****
!c
        NDATE=g%jdate+183               ! CONVERT TO NORTHERN HEMISPHERE
        IF(NDATE.GT.g%mdpy) NDATE=NDATE-g%mdpy
        DATE=real(NDATE)
        g%g=1.7+14.6*SIN(0.0172*(DATE-51.0))           ! TEMPLE,TEXAS
        IF(g%g.LT.0.0) g%g=0.0
!c
!c      call hfunc (i) ! calculate heat units or daydegrees
      CALL ozcot_hfunc
      g%stemp=(g%tempmx+g%tempmn)/2.0*g%asoil
!c      call evap (i) ! calculate potential evaporation

      call ozcot_evap
      call pop_routine(myname)
      RETURN
      END


!obsolete * ====================================================================
!obsolete       SUBROUTINE ozcot_n_fertilise (APPLIED,availn,APPLIED_AVAIL)
!obsolete * ====================================================================
!obsolete !c
!obsolete !c      simulates uptake of fertiliser nitrogen. assumes that there is an upper
!obsolete !c      limit to the amount of nitrogen a crop can take up and the rate of uptake
!obsolete !c      or recovery of fertiliser n decreases linearly from a maximum initial
!obsolete !c      value to zero when uptake limit is reached (basinski et al 1975, cot
!obsolete !c      gr rev). assume intial rate is 1.0 (100% recovery) and maximum uptake
!obsolete !c      is 240 kg/ha.
!obsolete !c
!obsolete       implicit none
!obsolete 
!obsolete       real uptakn_max
!obsolete       real rate_reducer
!obsolete       real availnx
!obsolete       real fraction
!obsolete       real applied
!obsolete       real applied_avail
!obsolete       real availn
!obsolete       integer n
!obsolete       integer nkg
!obsolete        
!obsolete        DATA UPTAKN_MAX /240./
!obsolete !c
!obsolete       character  myname*(*)            ! name of subroutine
!obsolete       parameter (myname = 'ozcot_n_fertilise')
!obsolete 
!obsolete *- Implementation Section ----------------------------------
!obsolete       call push_routine(myname)
!obsolete 
!obsolete        NKG = IFIX(APPLIED)             !  integer of kgs, index for DO loop
!obsolete        RATE_REDUCER = 1./UPTAKN_MAX    !  recovery decreases with uptake
!obsolete !c
!obsolete        AVAILNX   = availn            ! available N before application
!obsolete        DO 1000 N=1,NKG
!obsolete            FRACTION = 1.0-RATE_REDUCER*availn ! fraction of next kg available
!obsolete            availn = availn + FRACTION
!obsolete 1000  continue
!obsolete        APPLIED_AVAIL = availn-AVAILNX ! N applied now that will be available
!obsolete !c
!obsolete        call pop_routine(myname)
!obsolete        RETURN
!obsolete        END


* ====================================================================
      subroutine ozcot_overload
* ====================================================================

!c-------------------------------------------------------------------------------
!c     simulates abscission or abortion of older fruit under extreme stress
!c     ie when boll load exceeds carrying capacity.
!c-------------------------------------------------------------------------------

      use OzcotModule
      implicit none
      include 'error.pub'

      real over_c
      real over_n
      real fload
      real capacity
      real excess
      real available
      real abort
      integer das
      integer icat
      integer age

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_overload')

*- Implementation Section ----------------------------------
      call push_routine(myname)

!c----- determine if overload called for c or n ---------------------------------

      OVER_C = 999.                           ! very large when g%carcap_c=0.
      IF(g%carcap_c .GT. 0.) OVER_C = g%bload/g%carcap_c
      OVER_N = 999.                           ! very large when g%carcap_n=0.
      IF(g%carcap_n .GT. 0.) OVER_N = g%bollz/g%carcap_n

      IF(OVER_C .GT. OVER_N) THEN             ! Cc is limiting
          FLOAD = g%bload                     ! fruit load
          CAPACITY = g%carcap_c               ! cpacity
      ELSE                                    ! N is limiting
          FLOAD = g%bollz                     ! fruit load
          CAPACITY = g%carcap_n               ! cpacity
      ENDIF

!c----- count days fruit load exceeds capacity ---------------------------------

      IF(g%iday-g%last_iday.GT.1)g%idayco = 0 ! reset for break
      IF(FLOAD.GT.CAPACITY) g%idayco = g%idayco+1 ! count days FLOAD>g%carcap
      g%last_iday = g%iday                    ! g%last g%day counted
      IF(g%idayco.LT.3)then
         call pop_routine(myname)
         RETURN                 ! buffered by reserves
      else
      endif

!c----- compute excess fruit and buffer effect ---------------------------------

      EXCESS = FLOAD-CAPACITY                 ! bolls in excess of Cc supply
      EXCESS = EXCESS*0.1                     ! damp effect for carbon stress

!c----- loop through arrays to find available fruit ----------------------------

      DO 2 das=g%lfru(cat5),g%lfru(cat8)-1,-1           ! loop through categories 5, 6, 7
        IF(das.LT.1) GO TO 1                    ! no fruit to abort
        ICAT = cat4
        IF(das.LE.g%lfru(cat5)) ICAT = cat5
        IF(das.LE.g%lfru(cat6)) ICAT = cat6
        IF(das.LE.g%lfru(cat7)) ICAT = cat7
        IF(g%fruno(das).EQ.0.)GO TO 2                    ! no fruit
        AVAILABLE = g%fruno(das)                         ! fruit available to abort
        DO 10 age=1,6
            AVAILABLE = AVAILABLE-g%frmark(das,age)        ! less fruit marked
10      continue


        IF(ICAT.EQ.cat7.AND.g%fruwt(das)/g%fruno(das).LT.0.1)THEN ! fruit not grown yet ?
           g%frmark(das,6) = g%fruno(das)                  ! abort such fruit
           AVAILABLE = AVAILABLE-g%frmark(das,6)         ! adjust fruit available
        ENDIF

        IF(AVAILABLE.GT.0.)THEN
          AVAILABLE = AVAILABLE*0.1                    ! damp effect
          ABORT = AVAILABLE                            ! abort available fruit
          IF(ABORT.GT.EXCESS) ABORT=EXCESS             ! limit to requirement
          g%frmark(das,age6) = g%frmark(das,age6)+ABORT
          g%fmkcat(ICAT,age6) = g%fmkcat(ICAT,age6)+ABORT
          EXCESS = EXCESS-ABORT                        ! reduce excess no. bolls
          IF(EXCESS.LE.0.) GO TO 1                     ! excess depleted
        ENDIF
2     continue

1     continue

!c-------------------------------------------------------------------------------

      call pop_routine(myname)
      RETURN
      END


* ====================================================================
        real FUNCTION ozcot_satvp(Tdeg)
* ====================================================================
!c
        implicit none
      include 'error.pub'

        real Tdeg

        real tabs
        real tr
        real trlog
        real ts
        real tt
        real ewslog
        real ew

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_satvp')

*- Implementation Section ----------------------------------
      call push_routine(myname)

        TABS=Tdeg+273.16
        tr=373.16/TABS
        TRLOG=ALOG10(tr)
        tr=tr-1.
        TS=(10.**(11.344*(1.-TABS/373.16))-1.)/10.**7.
        TT=(10.**(-3.49149*tr)-1.)/10.**3.
        EWSLOG=ALOG10(1013.246)
        EW=-7.90298*tr+5.02808*TRLOG-1.3816*TS+8.1328*TT+EWSLOG
        ozcot_satvp=10.**EW

        call pop_routine(myname)
        RETURN
        END
!c
* ====================================================================
      real FUNCTION ozcot_senlf(bload,alai,carcap_c,smi)
* ====================================================================
!c
!c     estimates leaf longevity. ranges between 833 dd & 1110 dd
!c     reduced by water stress, boll load and self shading of
!c     canopy when lai gt 3.
!c
      use OzcotModule
      implicit none
      include 'error.pub'

      real ozcot_stress

      real fb
      real fw
      real carcap_c
      real bload
      real alai
      real smi

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_senlf')

*- Implementation Section ----------------------------------
      call push_routine(myname)

        FB=1.
        IF(carcap_c.GT.0.)FB=1.-bload/carcap_c ! reduce by boll load
        IF(FB.GT.1.)FB=1.
        IF(FB.LT.0.)FB=0.
        FW=ozcot_stress(c%fw_low,c%fw_high,c%fw_a,smi) ! effect of water stress
      ozcot_senlf=833.+277.*FB*FW

      call pop_routine(myname)
      RETURN
      END


!obsolete * ====================================================================
!obsolete         subroutine ozcot_sevap(RAINSI)
!obsolete * ====================================================================
!obsolete !c
!obsolete !c       ****** calculate actual es (use eos,ul1,time since ul1) ******
!obsolete !c
!obsolete       use OzcotModule
!obsolete       implicit none
!obsolete 
!obsolete       real Td
!obsolete       real esx
!obsolete       real rainsi
!obsolete !c
!obsolete       character  myname*(*)            ! name of subroutine
!obsolete       parameter (myname = 'ozcot_sevap')
!obsolete 
!obsolete *- Implementation Section ----------------------------------
!obsolete       call push_routine(myname)
!obsolete 
!obsolete         IF(g%sumes1.GE.c%UL1) GO TO 180
!obsolete         IF(RAINSI.GE.g%sumes1) GO TO 120
!obsolete         g%sumes1=g%sumes1-RAINSI
!obsolete         GO TO 140
!obsolete 120     g%sumes1=0.
!obsolete 140     g%sumes1=g%sumes1+g%eos
!obsolete         IF(g%sumes1.LT.0.0) g%sumes1=0.0
!obsolete         IF(g%sumes1.GT.c%UL1) GO TO 160
!obsolete         g%es=g%eos
!obsolete         GO TO 260
!obsolete 160     g%es=g%eos-0.4*(g%sumes1-c%UL1)
!obsolete         g%sumes2=0.6*(g%sumes1-c%UL1)
!obsolete         Td=(g%sumes2/c%CONA)**2
!obsolete         GO TO 260
!obsolete 180     if(rainsi.lt.g%sumes2) go to 200
!obsolete         RAINSI=RAINSI-g%sumes2
!obsolete         g%sumes1=c%UL1-RAINSI
!obsolete         g%sumes2=0.0
!obsolete         Td=0.
!obsolete         IF(RAINSI.GT.c%UL1) GO TO 120
!obsolete         GO TO 140
!obsolete 200     td=td+1.
!obsolete         g%es=c%CONA*Td**0.5-g%sumes2
!obsolete         IF(RAINSI.GT.0.) GO TO 220
!obsolete         IF(g%es.GT.g%eos) g%es=g%eos
!obsolete         GO TO 240
!obsolete 220     esx=0.8*rainsi
!obsolete         IF(ESX.LE.g%es) ESX=g%es+RAINSI
!obsolete         IF(ESX.GT.g%eos) ESX=g%eos
!obsolete         g%es=ESX
!obsolete 240     g%sumes2=g%sumes2+g%es-rainsi
!obsolete         Td=(g%sumes2/c%CONA)**2
!obsolete 260     if(g%es.lt.0.) g%es=0.
!obsolete 
!obsolete         call pop_routine(myname)
!obsolete         RETURN
!obsolete         END


* ====================================================================
!c     subroutine solwat (i)
      subroutine ozcot_solwat
* ====================================================================
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c      calculates the soil water balance as a function of soil     c
!c      layer depth.  modified from the model of ritchie (1972).    c
!c                                                                  c
!c      key variables:                                              c
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      use OzcotModule
      implicit none
      include 'error.pub'

      real ozcot_watco

      real depth
      real rtul
      real swi
!c      real t, rdef
      integer l
!c
!c      dimension stor(20) ,u(20)
!c      data jir/1/ ! index for current element of igday
!c      data initial/0/ ! flag for initial call

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_solwat')

*- Implementation Section ----------------------------------
      call push_routine(myname)

!cpsc      do 99 ll=1,nlayr
!cpsc 99       dlayr(ll)=dlayr(ll)/10.

!c-------initialising -----------------------------------------------------------

!c        if(i.eq.1) jir = 1            ! irrigation counter for ozcot2
        IF(g%isw.NE.1) THEN           ! replaces INITIAL.GT.1 for OZCOT2&4 May89
            g%isw = 1
!c            t=0.
            g%smi=g%swlayr(1)/g%ullayr(1)
!c           if(smi.lt.0.9 ) then
!c               sumes1=g%ul1
!c               sumes2=2.5-2.78*smi
!c           else
!c                sumes1=10.-smi*10.
!c               sumes2=0.
!c            endif
        ENDIF

!c------ remove water from layers 1&2 during land preparation -------------------

!c       jtest = jdate-(jdate/30)*30          ! test if 30th day
!c       if(isow.eq.0 .and. jtest.eq.0) call cultivate (0.25,1.0)
!c       if(jdate.eq.220) call cultivate (0.0,0.5)

!c------ add irrigation water ---------------------------------------------------

!c       rainsi=rain
!c       if(jdate.eq.igday(jir))then
!c         defirg=def
!c         rainsi=rain+defirg
!c         jir=jir+1
!c       end if

!c------ calculate runoff -------------------------------------------------------

!c       sw=sw+rainsi
!c       if(sw.le.ul) then
!c           q=0.0
!c       else
!c           q=sw-ul
!c        endif
!cpsc         q=runoff
!c        rainef=rainsi-q
!c       call sevap(rainsi)

!c------ calculate potential ep -------------------------------------------------
!c-------light interception modified to give hedgerow effect with skip row - abh 5/11/96 ------
!cpsc
        g%alai = g%alai*g%rs             ! lai in hedgerow

        IF(g%alai.GT.3.) THEN            !const
            g%ep=g%eo-g%es
        ELSE IF(g%alai.GT.1.6)THEN
            g%ep=(0.08+0.3066*g%alai)*g%eo ! L.Mateos 1987, ABH 1988   !const
        ELSE
            g%ep=(1.-EXP(-0.5186*g%alai))*g%eo ! L.Mateos 1987, ABH 1988   !const
        ENDIF

        IF(g%alai.EQ.0.0) g%ep=0.0
        IF(g%ep.LT.0.) g%ep=0.
!cpsc
        g%ep = g%ep/g%rs                 ! g%ep on ground area basis
        g%alai = g%alai/g%rs             ! restore LAI to ground area basis

!c------ limit ep using watco(smi) stress factor --------------------------------

        g%et=g%es+g%ep
        IF(g%eo.LT.g%et) THEN
            g%et=g%eo
            g%ep=g%et-g%es
            IF(g%ep.LT.0.0)g%ep=0.0
        ENDIF
        g%ep=g%ep*ozcot_watco(g%smi,g%eo,0.4,0.)               !const
        g%et=g%es+g%ep

        g%rrig(et_accum) = g%rrig(et_accum) + g%et ! accumulate g%et for output

!c       call swbal(i,rainef)
        CALL ozcot_swbal

!c------ calculate total sw before ep for sr: snbal ----------------------------

!c       swwep=0.0
!c       do 10 l=1,nlayr
!c         swwep=swwep+swlayr(l)*dlayr(l)
!c         sweplr(l)=swlayr(l)
!c10      continue

!c---- calculate soil water stress factor 'smi'  ---------------------------------

!c       if(i.lt.isow.or.isow.eq.0) go to 500
        IF(g%das.le.0.OR.g%isow.EQ.0) GO TO 500
        DEPTH=0.
        g%rtsw=0.
        RTUL=0.

        DO 20 L=1,g%nlayr
          DEPTH=DEPTH+g%dlayr_cm(L)
          IF(g%rtdep.LE.DEPTH) GO TO 460
          RTUL=RTUL+g%ullayr(L)*g%dlayr_cm(L)
          g%rtsw=g%rtsw+g%swlayr(L)*g%dlayr_cm(L)
20      continue
        GO TO 480

460     g%rtsw=g%rtsw+g%swlayr(l)*(g%rtdep+g%dlayr_cm(l)-depth)
        RTUL=RTUL+g%ullayr(L)*(g%rtdep+g%dlayr_cm(L)-DEPTH)
        SWI=g%swlayr(1)/g%ullayr(1)
        IF(g%dlayr_cm(1).GE.30.) GO TO 480                                    !const
        SWI=
     *  (SWI*g%dlayr_cm(1)+(g%swlayr(2)/g%ullayr(2))*(30.-g%dlayr_cm(1))      !const
     :  )/30.
480     g%smi=amax1(g%rtsw/rtul,swi)

!cpc
        g%smi = g%rtsw/RTUL

!c        if(rs.gt.1. .and. rtdep.lt.rtdepm) then  ! when rs > 1m & roots growing,
!c            smi = 1-(1-smi)*(tr/rs+1-tr)*rs      ! adjust for lateral root
!c            if(smi.lt.0.) smi = 0.
!c        endif                                    ! growth, limited to 1m row

!c------ calculate some quantities for writing only -----------------------------

!c       rtsmi=rtsw/rtul
!c
!c       flim=ozcot_watco(smi,eo,0.3,0.)
!cpc     rdef=rtul-rtsw
!c       rperd=(rdef/rtul)*100.0

  500 CONTINUE

!c------ calculate total soil water & deficit -----------------------------------

        g%sw=0.0
        DO 30 L=1,g%nlayr
            g%sw=g%sw+g%swlayr(L)*g%dlayr_cm(L)
            IF(L.LE.6)g%tswl(L)=(g%swlayr(L)+p%unul(L))*g%dlayr_cm(L)
!cpsc
!cpsc            swdep(l)=swlayr(l)*10.*dlayr_cm(l)+
!cpsc     *                 (duldep(l)-ullayr(l)*10.*dlayr_cm(l))
!cpsc

30      continue
        g%def=g%ul-g%sw

!cpsc      do 199 ll=1,nlayr
!cpsc 199       dlayr_cm(ll)=dlayr_cm(ll)*10.

        call pop_routine(myname)
        RETURN
        END


* ====================================================================
!c      subroutine cultivate (c_lyr1,c_lyr2)
* ====================================================================

!c     reduces soil water content of layers 1 and 2 in response to cultivation
!c     to levels passed in c_lyr1 & c_lyr2. if these are 1.0, no reduction as
!c     cultsw will be > swlayr

!c      use OzcotModule

!c      cultsw = ullayr(1)*c_lyr1          ! soil water content after cultivation
!c      if(cultsw.lt.swlayr(1)) swlayr(1) = cultsw
!c      cultsw = ullayr(2)*c_lyr2          ! soil water content after cultivation
!c      if(cultsw.lt.swlayr(2)) swlayr(2) = cultsw
!c      return
!c      end


!obsolete * ====================================================================
!obsolete !c      subroutine sowday (i,iend)
!obsolete        subroutine ozcot_sowday
!obsolete * ====================================================================
!obsolete 
!obsolete !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!obsolete !c      derives a sowing date if one is not given as an initial     c
!obsolete !c      condition.  sowing requires 3 consecutive days with soil    c
!obsolete !c      temperatures >= 15.0c and soil moisture sumes1 >= ul1       c
!obsolete !c      at sowing, root depth is assigned seed depth until          c
!obsolete !c      emergence.                                                  c
!obsolete !c                                                                  c
!obsolete !c      key variables:                                              c
!obsolete !c
!obsolete !c      isow = sowing date (as day of year); if input > 0 or after
!obsolete !c             selected in this s/r, s/r sowday not called;
!obsolete !c             if < 0 (-ve), this s/r will not look for a owing day
!obsolete !c             before -isow (i.e. the -ve value made +ve).
!obsolete !c      iwindow = time window for sowing when selected in this s/r,
!obsolete !c                starting with earliest day
!obsolete !c      tempre = mean temp day before yesterday
!obsolete !c      tempyd = mean temp yesterday
!obsolete !c      tmean  = mean temperature for last three days, when > 18
!obsolete !c               soil temp > 15 for three successive days - abh 8/8/89
!obsolete !c      yr1_last = flag to show season started last
!obsolete !c               year or this.
!obsolete !c      window = logical flag to show if in sowing window
!obsolete !c      span_nu_yr = logical flag to show if new year started in sowing window
!obsolete !c      lastyr = needed when sowing window spans new year
!obsolete !c               for number of days in previous year;
!obsolete !c               acts as a flag to show if sow_this_yr to be set to 0 or 1.
!obsolete !c      nosow  = flag to show if crop to be sown this year, mainly for
!obsolete !c               long fallowing with dry land cropping;
!obsolete !c               -1 for no crop, 1 for crop.
!obsolete !c      iswitch= toggle switch to change nosow.
!obsolete !c
!obsolete !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!obsolete !c
!obsolete       use OzcotModule
!obsolete       implicit none
!obsolete 
!obsolete !c      logical window/.false./
!obsolete !c      logical warm/.false./, wet/.false./
!obsolete !c      logical traffic/.false./, profile/.false./
!obsolete !c      logical span_nu_yr
!obsolete !c      logical yr1_last
!obsolete !c      dimension npm(12)
!obsolete !c      data npm /0,31,59,90,120,151,181,212,243,273,304,334/
!obsolete !c      data tempyd/0.0/,tempre/0.0/
!obsolete !c      data initial/0/,lastyr/0/
!obsolete 
!obsolete       character  myname*(*)            ! name of subroutine
!obsolete       parameter (myname = 'ozcot_sowday')
!obsolete 
!obsolete *- Implementation Section ----------------------------------
!obsolete       call push_routine(myname)
!obsolete 
!obsolete 
!obsolete !c      if(isow.gt.0) return
!obsolete 
!obsolete !c---------------- initial conditions set on first call -------------------------
!obsolete 
!obsolete !c      if(initial.eq.0 .and. isdy.lt.0) then  ! 1st time sowday called?
!obsolete !c          initial = 1                        ! set flag
!obsolete !c          jstart = npm(ismo)-isdy            ! earliest day of year for sowing
!obsolete !c          if(jstart.gt.mdpy) jstart=jstart-mdpy ! into next year
!obsolete !c          jstop  = jstart+iwindow            ! latest day to sow (can be >365)
!obsolete !c         iswitch = 1                        ! crop every year
!obsolete !c         if(isyr.lt.0) iswitch = -1         ! crop alternate years
!obsolete !c          nosow = 1                          ! crop every yr or even yrs if alt
!obsolete !c          if(isyr.eq.-2) nosow = -1          ! crop in odd years
!obsolete !c          yr1_last = .true.                  ! season starts last year
!obsolete !c      end if
!obsolete 
!obsolete !c-------------- reset on first call of new season ------------------------------
!obsolete 
!obsolete !c      if(i.eq.1) then                        ! 1st day of season? if so...
!obsolete !c          nosow=nosow*iswitch                ! toggle nosow for fallow/crop
!obsolete !c          window = .false.                   ! reset sowing window flag
!obsolete !c          span_nu_yr = .false.               ! reset
!obsolete !c          lastyr = 0                         !  reset  for next season
!obsolete !c          tempre = 0.                        !  reset  for next season
!obsolete !c          tempyd = 0.                        !  reset  for next season
!obsolete !c      endif
!obsolete 
!obsolete 
!obsolete !c      if(nosow.eq.-1) then                   ! if no crop this season (fallow)..
!obsolete !c          ncount = ncount+1                  ! count days of fallow season
!obsolete !c          if(ncount.lt.365)return            ! end of fallow season?
!obsolete !c          iend=4                             ! yes, end of fallow season
!obsolete !c          ncount=0                           ! reset counter
!obsolete !c          return
!obsolete !c      end if
!obsolete 
!obsolete !c---------- compute sowing conditions -----------------------------------------
!obsolete 
!obsolete       g%tmean3 = (g%tempav+g%tempyd+g%tempre)/3. ! mean temperature for g%last 3 days
!obsolete 
!obsolete       g%tempre = g%tempyd               ! update previous days temp for tomorrow
!obsolete       g%tempyd = g%tempav               ! update yesterdays temp for tomorrow
!obsolete 
!obsolete !c      if(tmean.ge.18.0) then            ! check soil temperature
!obsolete !c          warm = .true.                 ! soil warm enough to sow
!obsolete !c      else
!obsolete !c          warm = .false.                ! soil not warm enough to sow
!obsolete !c      endif
!obsolete 
!obsolete       g%s_bed_mi = g%swlayr(1)/g%ullayr(1) ! seed bed moisture index
!obsolete 
!obsolete       g%s_bed_sat = max(g%s_bed_sat,g%s_bed_mi)
!obsolete 
!obsolete !c      if(s_bed_mi.ge.0.75) then         ! seed bed saturated?
!obsolete !c          wet = .true.                  ! seed bed has been wetted
!obsolete !c      else
!obsolete !c          if(s_bed_mi.lt.0.5) wet = .false. ! seed bed has dried out
!obsolete !c      endif
!obsolete 
!obsolete !c      if(s_bed_mi.le.0.67) then         ! trafficable? bridge & muchow 1982
!obsolete !c          traffic = .true.              ! seed bed trafficable
!obsolete !c      else
!obsolete !c          traffic = .false.             ! seed bed not trafficable
!obsolete !c      endif
!obsolete 
!obsolete !c      if(sw.ge.sow_sw) then             ! soil water content of profile
!obsolete !c          profile = .true.              ! sufficient water in profile
!obsolete !c      else                              ! 11cm is for 1m from fawcet 1977
!obsolete !c          profile = .false.             ! insufficient water in profile
!obsolete !c      endif
!obsolete 
!obsolete !c------------- check if sowing window open--------------------------------------
!obsolete 
!obsolete !c      if(.not. window) then                  ! sowing window yet?
!obsolete !c          if(jdate.eq.1.and.i.ne.1) yr1_last=.true. ! window starts in new year
!obsolete !c          if(jdate.eq.jstart) then           ! ....starts today?
!obsolete !c              window = .true.                ! if so, set flag
!obsolete !c          else                               ! if not, ....
!obsolete !c              return                         ! ...return to avoid start in
!obsolete !c          endif                              ! middle of window
!obsolete !c      endif
!obsolete 
!obsolete !c      if(jdate.eq.1) then                    ! window spans new year?
!obsolete !c          span_nu_yr = .true.                ! set flag
!obsolete !c          lastyr = 365                                ! days in last year
!obsolete !c          if((((imyr-1)/4)*4).eq.(imyr-1)) lastyr=366 ! leap year
!obsolete !c      end if
!obsolete !c      jtry = jdate+lastyr                    ! allows window to span new year
!obsolete 
!obsolete !c      if(jtry.ge.jstop) then                 ! passed the sowing window?
!obsolete !c          iend = 3                           ! if so, set flag
!obsolete !c          window = .false.
!obsolete !c          return                             ! no crop this season
!obsolete !c      end if
!obsolete 
!obsolete !c------------- check if sowing conditions satisfied ----------------------------
!obsolete 
!obsolete !c      if(warm .and. wet .and. traffic .and. profile) then ! all conditions met?
!obsolete !c          isow=i+1                       ! sow tomorrow
!obsolete !c          rrig(2) = sw                   ! soil water at sowing
!obsolete !c          window = .false.
!obsolete !c      endif
!obsolete 
!obsolete       call pop_routine(myname)
!obsolete       RETURN
!obsolete       END


* ====================================================================
        real FUNCTION ozcot_stress(LOW,HIGH,A,STRS)
* ====================================================================
        implicit none
      include 'error.pub'

        real HIGH
        real A
        real STRS
!c
!c       computes or adjusts a factor.
!c       input is state variable strs with upper and lower limits, high,low.
!c       output is between 0 and 1.
!c       a =  1 gives factor a linear fn of ratio of strs to high - low
!c       a gt 1 accentuates effect of strs on value
!c       a lt 1 damps effect.
!c
        REAL LOW

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_stress')

*- Implementation Section ----------------------------------
      call push_routine(myname)

        ozcot_stress=(STRS-LOW)/(HIGH-LOW)
        IF(ozcot_stress.GT.1.)ozcot_stress=1.
        IF(ozcot_stress.LT.0.)ozcot_stress=0.
        ozcot_stress=(1.-(1.-ozcot_stress)**A)

        call pop_routine(myname)
        RETURN
        END
* ====================================================================
      real FUNCTION ozcot_survive(CAPACITY,bload)
* ====================================================================
      implicit none
      include 'error.pub'

      real CAPACITY
      real bload
      real a
      real b
!c
!c     estimates survival of fruit as function of boll load.
!c
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_survive')

*- Implementation Section ----------------------------------
      call push_routine(myname)

      ozcot_survive = 0.
      IF(CAPACITY.EQ.0.)then
         call pop_routine(myname)
         RETURN
      else
      endif
      A=1.0 ! intercept of survival function                 !const
      B=A/CAPACITY ! slope of survival function
      ozcot_survive=A-B*bload ! prortion surviving
      IF(ozcot_survive.LT.0.)ozcot_survive=0.
      IF(ozcot_survive.GT.1.)ozcot_survive=1.
      ozcot_survive = ozcot_survive*0.8  ! background, sub-threshold shedding

      call pop_routine(myname)
      RETURN
      END


* ====================================================================
!c       subroutine swbal(i,rainef)
        subroutine ozcot_swbal
* ====================================================================

!c
!c   **** soil & plant water balance including rain and soil evaporation, ****
!c   **** beginning with the top soil layer.                      ****
!c
      use OzcotModule
      implicit none
      include 'error.pub'

      real ux
      real epcoef
      real uob
      real sum
      real stran
      real depth
      real tdepth
      real epd
      real swlr
      real dswmx
      integer l

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_swbal')

*- Implementation Section ----------------------------------
      call push_routine(myname)

!c
!c       percol=rainef
!c       exes=es
        UX=0.0
        IF(g%smi.GE.c%epcoef_smi_crit)EPCOEF=c%epcoef1            !  W*N                 !const
        IF(g%smi.LT.c%epcoef_smi_crit)EPCOEF=c%epcoef2            !  82/83               !const
        UOB=g%ep/(1.-EXP(-EPCOEF))
        SUM=0.0
        STRAN=0. ! sum of g%trans(L) down profile
        DEPTH=0.
        TDEPTH=0.
        EPD=0.0
!c       w=0.
!c
!c***********************************************************************
!c
          DO 10 L=1,g%nlayr
!cpsc
!cpsc          swlayr(l)=swdep(l)/10./dlayr_cm(l)
!cpsc     *                 -(duldep(l)/10./dlayr_cm(l)-ullayr(l))
!cpsc
          g%trans(L)=0.
            SWLR=g%swlayr(L)*g%dlayr_cm(L)
!c           swmax=ullayr(l)*dlayr_cm(l)
!c           if(percol.eq.0..and.exes.eq.0.)go to 2
!c
!c**** rain percolates layer 'l'
!c
!c       swlr=swlr+percol
!c       percol=0.
!c       if(swlr.le.swmax)go to 1
!c       percol=swlr-swmax ! surplus percolates to next layer
!c       swlr=swmax        ! this layer full
!c
!c**** extract es from layer 'l'
!c
!c 1     swlr=swlr-exes    ! extract es from this layer
!c       exes=0.
!c       if(swlr.ge.0.)go to 2
!c       exes=-swlr        ! es carried down to next layer
!c       swlr=0.
!c
!c**** extract ep from this layer
!c
!c2        continue
          DEPTH=DEPTH+g%dlayr_cm(L)         ! moved here 29/6/88
!c         if(i.lt.iemrg) depth=0.0       !       ditto
          IF(g%das.LT.g%iemrg) DEPTH=0.0   !       ditto
          IF(g%rtdep.LE.TDEPTH) GO TO 11 !       ditto
          IF(g%rtdep.LT.DEPTH) DEPTH=g%rtdep !       ditto
          SUM=UOB*(1.-EXP(-EPCOEF*DEPTH/g%rtdep))
          DSWMX=SUM-UX
          SWLR=SWLR-DSWMX    ! EXTRACT g%ep FROM THIS LAYER
          IF(SWLR.GE.0.) GO TO 3
          EPD=SWLR
          SWLR=0.
          DSWMX=g%dlayr_cm(L)*g%swlayr(L)
3         g%trans(l)=dswmx
          TDEPTH=DEPTH
          UX=SUM+EPD   ! EPD CORRECTS UX IF LAYER IS DRY
          EPD=0.0
11        continue ! moved from after next statement 29 jun 88
          g%swlayr(L)=SWLR/g%dlayr_cm(L)          ! water_uptake
          STRAN=STRAN+g%trans(L)
          g%setlyr(L)=g%setlyr(L)+STRAN ! cumulative transp. thro season down profile
10      continue

        call pop_routine(myname)
        RETURN
        END
                                                 
!c
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c      water stress function for root growth.                      c
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
* ====================================================================
      real FUNCTION ozcot_watco(smi,eo,X3,X1)
* ====================================================================
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c      water stress function for root growth.                      c
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
      implicit none
      include 'error.pub'

      real smi
      real eo
      real X3
      real X1
      real x2
      real y0
      real slope

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_watco')

*- Implementation Section ----------------------------------
      call push_routine(myname)

      X2=X3*eo/0.8
      IF(X2.GT.1.0) X2=1.0
      IF(smi.LT.X2) GO TO 20
      ozcot_watco=1.0
      GO TO 30
!c
   20 CONTINUE
      SLOPE=1.0/(X2-X1)
      Y0=1.0-SLOPE*X2
      ozcot_watco=SLOPE*smi+Y0
      IF(ozcot_watco.LT.0.0) ozcot_watco=0.0
!c
   30   CONTINUE

        call pop_routine(myname)
        RETURN
        END


* ====================================================================
!c      subroutine yield(nszn,iend)
      subroutine ozcot_yield
* ====================================================================

!c     estimates yield and gross margin at end of season

      use OzcotModule
      implicit none
      include 'error.pub'

!      real bollsc

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_yield')

*- Implementation Section ----------------------------------
      call push_routine(myname)

!c     calculate yield **********************************************************

      g%alint = g%openwt*10.*g%pclint    ! g%g sc/m to kg lint/ha
!cpsc      alint = alint/rs                   ! adjust for row spacing 2/5/90

!c      plntz = uptakn                     ! nitrogen uptake
!cpc   bollsc = 0.
!cpc   if(openz.gt.0.) bollsc = openwt/openz ! g sc/boll
      g%rrig(rain_pre_post_boll) = g%rrig(rain_preboll) 
     :                           + g%rrig(rain_postboll)  ! commulative g%rain pre + post boll

      call ozcot_residues                      ! to calculate stem and root residues
!c     calculate gross margin ***************************************************

!c     currently redundant!!!!

!c      cot_price = 2.                     ! $ per kg
!c      wat_cost = 12.                     ! $ per ml pumped
!c      spray_cost =2.                     ! $ day of protection

!c      cot_price = 2.                      ! cotton price $ per kg
!c      gro_cost = 1200.                    ! growing costs - irrigated
!c      gro_cost = 450.                     ! growing costs - rain grown
!c      wat_cost  = 12.                     ! $ per ml (pumping & carges)
!c      spray_cost = 2.                     ! $ per day of protection
!c      cot_inc = alint*cot_price           ! cotton income
!c      wat_exp = rrig(2)/10.*wat_cost      ! water expenditure
!c      spray_save = (150-ilaiz)*spray_cost ! saving of spray cost
!c      gross_marg = cot_inc - gro_cost - wat_exp -  spray_save ! gross margin

!c     sowing date for output ***************************************************

!c      jsow = isow+imet-1                  ! sowing date in j time
!c      mdpy_prev = 365
!c      if((imyr-1)/4*4 .eq. imyr_1) mdpy_prev = 366
!c      if(jsow.gt.mdpy_prev) jsow = jsow - mdpy_prev

!c     this section for when crop not sown **************************************

!c      if(iend.ge.3) then
!c          jsow = 0                        ! crop not sown, window passed or fallow
!c          gross_marg = 0.0                ! gross margin nil
!c      end if

!c     this section does stuff needed with 1st call of yield ********************


!c      if (imyr.gt.1800) then              ! year in 10s or 100s?
!c          imyr = imyr-1800                ! change year to 10s
!c          if(imyr.ge.100) imyr=imyr-100   ! for 1900s
!c      endif                               ! above done for output

!c      if(nszn.eq.1) then                     ! first season

!c          open(3,file='yield.out',status='unknown')
!c          write(3,7772) title

!c          if(mode.le.1) then                  ! validation or calibration
!c              write(3,600)
!c          else                                ! simulation for strategy etc
!c              if(defirr(2).gt.0.) then        ! irrigated crop
!c                  write(3,700)
!c              else                            ! rain-fed crop
!c                  write(3,800)
!c              endif
!c          endif

!c          if(jdate.lt.244) then               ! last year was year of sowing
!c              iyr1 = imyr-1                   ! year sown in first season
!c              iyr2 = imyr                     ! year harvested first season
!c          else                                ! this year was year of sowing,
!c              iyr1 = imyr                     ! year of sowing
!c              iyr2 = imyr+1                   ! year of harvest
!c          endif

!c      else                                    ! not first season

!c              iyr1 = iyr1+1                   ! year of sowing of this season
!c              iyr2 = iyr2+1                   ! year of harvest of this season


!c      endif

!c      if(iyr1.eq.100) iyr1 = 0                 ! new century
!c      if(iyr2.eq.100) iyr2 = 0                 ! new century

!c    fallow soil water *********************************************************


!c      if(jsow.gt.0) then    ! crop sown; deal with fallow sw gain
!c          gain = rrig(2)-rrig(6) ! gain in sw during fallow
!c          fraction = 0.0    ! fraction of rainfall conserved by fallow
!c          if(rrig(5).ne.0.) fraction = gain/rrig(5)
!c          write(4,999) iyr1,iyr2,jsow,rrig(6),rrig(2),gain,rrig(5),fraction
!c999       format(3i4,5f8.2) ! yrs, sow d, initial sw, final sw, gain, rain, fraction
!c          rrig(5) = 0.0     ! reset fallow rain. do not reset in reset!
!c          rrig(6) = sw      ! initial sw for next fallow. do not reset in reset!
!c      endif

!c     write output *************************************************************

!c      if(mode.le.1) then                  ! validation or calibration
!c          write(3,7770) iyr1,iyr2,jsow,openz,alint,bollsc,
!c     *    alaiz,plntz,ilaiz,sqzx,isqzx
!c          write(2,7770) iyr1,iyr2,jsow,openz,alint,bollsc,
!c     *    alaiz,plntz,ilaiz,sqzx,isqzx
!c      else                                ! simulation for strategy etc
!c          if(defirr(2).gt.0.) then        ! irrigated crop
!c              write(3,7771) iyr1,iyr2,jsow,openz,alint,bollsc,
!c     *        alaiz,plntz,ilaiz,ifix(rrig(1)),rrig(2),rrig(7),rrig(8)
!c     *        alaiz,plntz,ilaiz,ifix(rrig(1)),rrig(2),rrig(7),def_last ! norn d
!c              write(2,7771) iyr1,iyr2,jsow,openz,alint,bollsc,
!c     *        alaiz,plntz,ilaiz,ifix(rrig(1)),rrig(2),rrig(7),rrig(8)
!c     *        alaiz,plntz,ilaiz,ifix(rrig(1)),rrig(2),rrig(7),def_last ! norn d
!c          else                            ! rain-fed crop
!c              write(3,7771) iyr1,iyr2,jsow,openz,alint,bollsc,
!c     *        alaiz,plntz,ilaiz,ifix(rrig(1)),rrig(2),rrig(3),rrig(4)
!c              write(2,7771) iyr1,iyr2,jsow,openz,alint,bollsc,
!c     *        alaiz,plntz,ilaiz,ifix(rrig(1)),rrig(2),rrig(3),rrig(4)
!c          endif
!c      endif

      call pop_routine(myname)
      RETURN
!c
!c600   format(' year sown  bolls/m  lint sc/boll max_lai  n_uptk day
!c     *   sqz day')
!c700   format(' year sown  bolls/m  lint sc/boll max_lai  n_uptk day
!c     * no  water  rain  cum_et')
!c     * no  water  rain def_l')                                    ! for norm d
!c800   format(' year sown  bolls/m  lint sc/boll max_lai  n_uptk day
!c     * no  water rain1  rain2')
!c7770  format(x,2i2,i4,f8.1,f8.0,2f8.2,f8.0,i4,f6.1,i4)
!c7771  format(x,2i2,i4,f8.1,f8.0,2f8.2,f8.0,i4,i3,3f7.1)
!c7772  format(15a4)

      END


* ====================================================================
      subroutine ozcot_plant_n
* ====================================================================
!c     call from pltgrw before ozcot_cropn
!c     calculates nitrogen content of dry matter increments and sums them
!c     adjusts n increments as soil n supply diminishes
!c     variable ddw_leaf etc from s/r dry_matter
!c              supply_n from system
!c               dn_plant = daily increment of plant n to system

      use OzcotModule
      implicit none
      include 'error.pub'

      real ozcot_stress

      real supply_n
      real conc_l
      real conc_s
      real conc_r
      real conc_b
      real dn_leaf
      real dn_stem
      real dn_root
      real dn_boll
      real sup_dem
      real adjust

      data supply_n /2.0/

      DATA CONC_L /0.04/
      DATA CONC_S /0.02/
      DATA CONC_R /0.02/
      DATA CONC_B /0.015/

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_plant_n')

*- Implementation Section ----------------------------------
      call push_routine(myname)

!c      calculate daily increment for components of dry matter

      dN_LEAF = g%ddw_leaf * CONC_L ! leaf nitrogen
      dN_STEM = g%ddw_stem * CONC_S ! stem nitrogen
      dN_ROOT = g%ddw_root * CONC_R ! root nitrogen
      dN_BOLL = g%ddw_boll * CONC_B ! boll nitrogen

      g%dn_plant = dN_LEAF + dN_STEM + dN_ROOT + dN_BOLL ! plant N increment

!c      adjust uptake when soil supply limiting

      if (g%dn_plant.gt.0.) then
        SUP_DEM = SUPPLY_N/g%dn_plant ! supply/demand ratio
      else
        SUP_DEM = 0.
      endif
      ADJUST = ozcot_stress(c%adjust_low
     :                     ,c%adjust_high
     :                     ,c%adjust_a
     :                     ,SUP_DEM) ! factor to adjust
      IF(ADJUST.LT.1.0) THEN
          dN_LEAF = dN_LEAF * ADJUST   ! leaf nitrogen adjusted
          dN_STEM = dN_STEM * ADJUST   ! stem nitrogen adjusted
          dN_ROOT = dN_ROOT * ADJUST   ! root nitrogen adjusted
          dN_BOLL = dN_BOLL * ADJUST   ! boll nitrogen adjusted
          g%dn_plant = dN_LEAF + dN_STEM + dN_ROOT + dN_BOLL ! plant N increment
      ENDIF

      g%total_n =g%total_n + g%dn_plant ! accumulate uptake for season

!c     compare accumulated uptake with projected uptake for season
!c     if accumulated exceeds projected, assume requirements met by remobilisation
!c     and set this day's increments to zero

      IF(g%total_n.GE.g%uptakn/10.) THEN
          g%total_n =g%total_n - g%dn_plant ! adjust uptake for season
          dN_LEAF = 0.0                ! leaf nitrogen adjusted
          dN_STEM = 0.0                ! stem nitrogen adjusted
          dN_ROOT = 0.0                ! root nitrogen adjusted
          dN_BOLL = 0.0                ! boll nitrogen adjusted
          g%dn_plant = 0.0             ! plant N increment
      ENDIF


!c      write(4,222) iday,supply_n,dn_plant,total_n,uptakn,dw_total
!c222   format(i5,5f8.3)


      call pop_routine(myname)
      RETURN
      END

* ====================================================================
      subroutine ozcot_residues
* ====================================================================

!c      called from s/r yield to calculate stem and root residues

      use OzcotModule
      implicit none
      include 'error.pub'

      real conc_res

      DATA CONC_RES /0.005/           ! N concentration of residues

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_residues')

*- Implementation Section ----------------------------------
      call push_routine(myname)

      g%stem_res = g%dw_stem         ! stem residues dry matter
      g%stem_res_n = g%dw_stem*CONC_RES ! N content of stem residues
      g%root_res = g%dw_root         ! root residues dry matter
      g%root_res_n = g%dw_root*CONC_RES ! N content of root residues

      call pop_routine(myname)
      RETURN
      END



* ====================================================================
!c     subroutine dry_matter (i)
      subroutine ozcot_dryxmatter
* ====================================================================

!c     this subroutine is for ozcot6 (version 6).

!c     first demand for assimilate is estimated for leaf, stem, root and boll.
!c     leaf demand is determined from potential increase in area. water stress
!c     may reduce actual area increase, thus giving thicker leaves. inadequate
!c     assimilate supply may reduce actal weight increase giving thinner leaves.
!c     upper and lower limits are set by leaf weight:area ratio. if the upper
!c     limit is exceeded, weight increase is reduced. if lower limit is not
!c     reached, area increase is reduced. stem and root demand are determined
!c     from potential leaf demand using allometric relationships.

!c     calls s/r assimilation to compute assimilate production for the day.
!c     the supply available to meet demand is obtained from sum of the days
!c     assimilate production and any reserves.

!c     supply and demand are then compared in order to partition dry matter
!c     increase between leaf, stem, boll and root.
!c     if supply exceeds demand, up to two days supply can be stored as reserves.
!c     if demand exceeds supply, distribution is simulated on the basis
!c     of proximity to the source of supply. leaf, stem and fruit are assumed to
!c     be equidistant from the source. if supply exceeds the sum of leaf, stem
!c     and fruit demand, their needs are met in full and the balance goes to
!c     root. if the sum of leaf, stem and fruit demand exceeds supply,
!c     their needs are met in proportion to the supply/demand ratio and the
!c     root receives none. the root supply:demand ratio or a decline in root
!c     growth provide feedback to reduce increase in root depth in s/r pltgrw.

!c     local variables:
!c       assimilate new dry matter passed daily from s/r assimilation
!c       ddw_boll   day's increase in boll dry weight
!c       ddw_leaf   day's increase in leaf dry weight
!c       ddw_stem   day's increase in stem dry weight
!c       ddw_root   day's increase in root dry weight
!c       ddw_root_max   maximum value of increase in root dry weight
!c       demand     demand for assimilate for potential day's growth
!c       fnstrs2    n stress for boll growth - on/off
!c       fwstrs     water stress for boll growth
!c       sd_ratio   supply:demand ratio for leaf, stem and boll growth
!c       sd_root    supply:demand ratio for root growth
!c       strsbl     stress factor for boll growth, minimum of n and water
!c       supply     day's supply of assimilate available for potential growth
!c       wt_area    leaf weight:area ratio

      use OzcotModule
      implicit none
      include 'data.pub'
      include 'error.pub'

      real ozcot_stress

      real wt_area
      real fwstrs
      real fnstrs2
      real strsbl
      real assimilate
      real supply
      real demand
      real sd_ratio
      real sd_root
      real ddw_root_max

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_dryxmatter')

*- Implementation Section ----------------------------------
      call push_routine(myname)

!c------------------------------------------------------------------------------
!c     initialise leaf, stem and root dry matter at start of new crop
!c------------------------------------------------------------------------------

      IF(g%dw_leaf.EQ.0.) THEN              ! leaf weight initialise to zero?
          g%dw_leaf = c%EMBRYO*c%F_LEAF*g%ppm ! initial leaf dry weight per m
          g%dw_stem = c%EMBRYO*c%F_STEM*g%ppm ! initial stem dry weight per m
          g%dw_root = c%EMBRYO*c%F_ROOT*g%ppm ! initial root dry weight per m
      ENDIF

!c------------------------------------------------------------------------------
!c     calculate demand (potential growth) for leaf, stem and root
!c------------------------------------------------------------------------------

      g%ddw_leaf = g%dlai_pot*c%SPECIFIC_LW               ! leaf demand
      g%ddw_stem = divide (c%A_STEM_LEAF*g%ddw_leaf*g%dw_stem
     :                  ,g%dw_leaf, 0.0)                  ! ditto for stem
      g%ddw_root = divide (c%A_ROOT_LEAF*g%ddw_leaf*g%dw_root
     :                  ,g%dw_leaf, 0.0)                  ! ditto for root

!c------------------------------------------------------------------------------
!c     feed back of leaf weight/area ratio
!c------------------------------------------------------------------------------

      IF(g%dlai(g%iday).GT.0.) THEN                       ! leaf growth today?
          WT_AREA = divide (g%ddw_leaf, g%dlai(g%iday), 0.0) ! leaf weight/are ratio
          IF(WT_AREA.GT.c%WT_AREA_MAX) THEN               ! too thick
              g%ddw_leaf = g%dlai(g%iday)*c%WT_AREA_MAX   ! reduce weight
!c          else if(wt_area.lt.wt_area_min) then            ! too thin
!c              dlai(iday) = ddw_leaf/wt_area_min           ! reduce area
          ENDIF
      ENDIF

!c------------------------------------------------------------------------------
!c     calculate demand for bolls
!c------------------------------------------------------------------------------

!c      if(isq.gt.0 .and. i.ge.isq+2) then        ! fruit called yet?
      IF(g%isq.GT.0 .AND. g%das.GE.g%isq+2) THEN  ! FRUIT called yet?             !const
          FWSTRS = ozcot_stress(c%fwstrs_low
     :                           ,c%fwstrs_high
     :                           ,c%fwstrs_a
     :                           ,g%smi)    ! water stress on bolls     !const
          FNSTRS2 = 1.                          ! N stress for bolls off
          IF(g%fnstrs.EQ.0.) FNSTRS2 = 0.       ! N stress for bolls on
          STRSBL = AMIN1(FWSTRS,FNSTRS2)        ! minimum of water or N stress
          g%bollgr = p%scboll*g%bper*c%FBURR ! boll gr rate this g%day
          g%bollgr = g%bollgr*STRSBL            ! adjust for stress
          IF(g%bollgr.LT.0.) g%bollgr = 0.
          g%ddw_boll = g%bollz*g%bollgr         ! boll demand - potential growth

      ENDIF

!c------------------------------------------------------------------------------
!c   determine supply of assimilate
!c------------------------------------------------------------------------------

!c      call assimilation(assimilate,i)                  ! day's assimilate
      CALL ozcot_assimilation(ASSIMILATE)                  ! g%day's assimilate
      SUPPLY = ASSIMILATE+g%reserve                    ! compute supply
      g%reserve = 0.                                   ! g%reserve used

!c------------------------------------------------------------------------------
!c   compare total demand with supply to partition assimilate
!c------------------------------------------------------------------------------

      DEMAND = g%ddw_leaf+g%ddw_boll+g%ddw_stem+g%ddw_root ! compute total demand

      IF(SUPPLY.GE.DEMAND) THEN        ! demand met, potential growth achieved
         g%reserve = g%reserve+SUPPLY-DEMAND           ! excess becomes g%reserve
         IF(g%reserve.GT.g%res_cap) g%reserve = g%res_cap ! limit to g%reserve
         SD_RATIO = 1.0                ! supply:demand ratio for leaf,stem,boll          !const
         SD_ROOT = 1.0                 ! supply:demand for root                          !const
      ELSE                             ! demand not met, reduce potential growth
         DEMAND = DEMAND-g%ddw_root    ! demand for leaf, stem and fruit
         IF(SUPPLY.GE.DEMAND) THEN     ! their potential growth achieved
             SD_RATIO = 1.0            ! supply:demand ratio for leaf,stem,boll          !const
             SD_ROOT = divide ((SUPPLY-DEMAND)
     :                         ,g%ddw_root, 0.0)       ! supply:demand for root
             g%ddw_root = SUPPLY-DEMAND ! rest to root
         ELSE                          ! leaf, stem and fruit demand not met
             SD_RATIO = divide (SUPPLY, DEMAND, 0.0)   ! supply:demand ratio
             g%ddw_leaf = g%ddw_leaf*SD_RATIO          ! actual leaf growth
             g%ddw_boll = g%ddw_boll*SD_RATIO          ! actual fruit growth
             g%ddw_stem = g%ddw_stem*SD_RATIO          ! actual stem growth
             g%ddw_root = 0.                           ! no root growth
             SD_ROOT = 0.                              ! supply:demand for root
             g%bollgr = g%bollgr*SD_RATIO              ! adjust boll growth rate
         ENDIF
      ENDIF

!c------------------------------------------------------------------------------
!c     grow crop by updating dry weights for leaf, stem, bolls and roots
!c------------------------------------------------------------------------------

      g%dw_leaf = g%dw_leaf+g%ddw_leaf                 ! total leaf dry weight
      g%dw_stem = g%dw_stem+g%ddw_stem                 ! total stem dry weight
      g%dw_boll = g%dw_boll+g%ddw_boll                 ! total boll dry weight
      g%dw_root = g%dw_root+g%ddw_root                 ! total root dry weight
      g%dw_total = g%dw_leaf+g%dw_stem+g%dw_boll+g%dw_root ! total dry weight

      g%ddw_l(g%iday) = g%ddw_leaf                     ! this g%day's leaf dry wt
!c      alai = alai+dlai(iday)                           ! update lai with increase

!c------------------------------------------------------------------------------
!c     feed back from root grow to root depth
!c------------------------------------------------------------------------------

      IF(g%iday.EQ.1) dDW_ROOT_MAX = g%ddw_root        ! initialise max root rate

      IF(g%ddw_root.GT.dDW_ROOT_MAX) THEN
          dDW_ROOT_MAX = g%ddw_root                    ! save maximum root rate
          g%root_feedback = 1.0                        ! feedback of dw on depth
      ELSE
          IF(dDW_ROOT_MAX.EQ.0.) THEN
              g%root_feedback = 1.0
          ELSE
              g%root_feedback = divide (g%ddw_root, dDW_ROOT_MAX, 0.0) ! feedback of dw on depth
          ENDIF
      ENDIF

      g%root_feedback = AMIN1(g%root_feedback,SD_ROOT) ! feedback of dw on depth
      IF(g%root_feedback.GT.0.) g%root_feedback=g%root_feedback**0.333 ! cubic to linear      !const

!c------------------------------------------------------------------------------

      call pop_routine(myname)
      RETURN
      END



* ====================================================================
!c      subroutine assimilation (assimilate,i)
      subroutine ozcot_assimilation (ASSIMILATE)
* ====================================================================

!c     assimilate production for the day is estimated from intercepted
!c     photosynthetically active radiation (montieth 1977).
!c     adjusted for effects of water stress using data of turner et al 1986
!c     collected at narrabri ars.
!c     effect of water logging based on observation of hearn and constable 1984
!c     on yield and hodgson on photosynthesis.
!c     effect of temperature, see constables thesis 1981, direct effect on
!c     photosynthesis and respiration, using angus and wilson's 1976 expression
!c     for a scalar and constables value for base and optimum.
!c     carrying capacity, carcap,estimated. it is maximum number of bolls the
!c     crop can carry, and is therefore the boll load that causes 100% shedding.
!c     local variables:
!c       assim_mean      3 day mean of assimilate supply
!c       assim_1         previous day's assimilate supply
!c       assim_2         day before previous day's supply
!c       rad_mj          radiation in mega joules
!c       rel_p           relative photosynthesis, scalar for water stress
!c       par_int         intercepted photosynthetically active radiation
!c       tf              temperature scalar for dry matter production

      use OzcotModule
      implicit none
      include 'error.pub'

!      real assim_1
!      real assim_2
      real rad_mj
      real alight
      real par_int
      real assimilate
      real rel_p
      real tf

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_assimilation')

*- Implementation Section ----------------------------------
      call push_routine(myname)

!c------------------------------------------------------------------------------
!c     initialise for new season
!c------------------------------------------------------------------------------

!cpc   if(iday.eq.1) then
!cpc       assim_1 = 0.0                    ! previous day's assimilation
!cpc       assim_2 = 0.0                    ! day before that
!cpsc  endif

!c------------------------------------------------------------------------------
!c     photosynthesis
!c------------------------------------------------------------------------------

      RAD_MJ = g%rad/23.87                 ! langleys to Mjoules                  !const
!c      par_int = rad_mj*(1.-tr)*0.5         ! intercepted par

      ALIGHT = 1.-EXP(-1.*g%alai)          ! light interception, Beer's law.
      PAR_INT = RAD_MJ*(ALIGHT)*0.5        ! intercepted PAR, ex old OZCOT        !const

      ASSIMILATE = PAR_INT*c%e_par         ! assimilation
      IF(ASSIMILATE*2..GT.g%res_cap) THEN
          g%res_cap = ASSIMILATE*2.        ! capacity to store reserves           !const
      ENDIF

!c------------------------------------------------------------------------------
!c     effect of water stress on assimilation
!c------------------------------------------------------------------------------

      REL_P =.25+.864*g%smi                ! effect of water stress on Pp          !const
      IF(REL_P.GT.1.) REL_P = 1.           ! (TURNER g%et al 1986).                !const
      ASSIMILATE = ASSIMILATE*REL_P        ! adjust for water stress

!c------------------------------------------------------------------------------
!c     effect of temperature on dry matter production
!c------------------------------------------------------------------------------

      TF = (g%tempav-c%T_BASE)/(c%T_OPT-c%T_BASE) ! temperature scalar after
      TF = 2*TF-TF**2                      ! Angus & Wilson 1976, Constable 1981     !const
      ASSIMILATE = ASSIMILATE*TF           ! adjust assimilate for temp

!c------------------------------------------------------------------------------
!c     effect of waterlogging on photosynthesis - hearn & constable 1984 eqn 4
!c------------------------------------------------------------------------------

!c      if(def.lt.2.5) then                 ! waterlogged?
      IF(g%sw/g%ul.GT.c%stress_wlog) THEN           ! waterlogged?                            !const
          ASSIMILATE = ASSIMILATE*c%wlog_assimilate_red  ! adjust for water logging - old OZCOT    !const
      ENDIF

!c      if(isq.eq.0 .or. i.lt.isq+2) return  ! do not proceed to carrying capacity
      IF(g%isq.EQ.0 .OR. g%das.LT.g%isq+2) then                                       !const
         call pop_routine(myname)
         RETURN ! do not proceed to carrying capacity
      else
      endif

!c------------------------------------------------------------------------------
!c     carrying capacity - photosynthetic capacity divided by boll growth rate
!c------------------------------------------------------------------------------

!c      disable rest of subroutine for use with ozcot2 in apsru system

!c      if(assim_1.gt.0.0 .and.assim_1.gt.0.0) then      ! not 1st or 2nd crop day
!c          assim_mean = (assimilate+assim_1+assim_2)/3. ! 3 day running mean
!c      else                                             ! is 1st or 2nd crop day
!c          assim_mean = assimilate
!c      endif
                                                   ! use mean to buffer g%carcap
!c      assim_2 = assim_1                            ! 3rd day's for tomorrow
!c      assim_1 = assimilate                         ! 2nd day's for tomorrow

!c      if(bollgr.gt.0.0) then
!c          carcap_c = assim_mean/bollgr             ! carrying capacity
!c      else
!c          carcap_c = 0.0                           ! zero when bolls not growing
!c      endif

!c      if(carcap_c.lt.0.) carcap_c = 0.             ! trap
!c      carcap  = carcap_c
!c      cutout = carcap*fcutout(ivar)                ! boll load for cutout

!c      cutout = carcap*1.00                         ! sensitivity to fcutout

!c------------------------------------------------------------------------------

      call pop_routine(myname)
      RETURN
      END

*     ===========================================================
      logical function ozcot_my_type ()
*     ===========================================================
      use ozcotModule
      implicit none
      include 'intrface.pub'                      
      include 'error.pub'                         

*+  Purpose
*       Returns true if 'type' is equal to the crop type or is absent.

*+  Assumptions
*       If type is not specified, it is assumed the message was addressed
*        directly to the module.

*+  Changes
*      211294 jngh specified and programmed
*     220696 jngh changed extract to collect

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'ozcot_my_type')

*+  Local Variables
      character  crop_type*50          ! crop type in data string
      integer    numvals               ! number of values returned

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      call collect_char_var_optional ('type', '()'
     :                              , crop_type, numvals)
 
      if (crop_type.eq.c%crop_type .or. numvals.eq.0) then
         ozcot_my_type = .true.
      else
         ozcot_my_type = .false.
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine ozcot_read_constants ()
*     ===========================================================
      use ozcotModule
      implicit none
      include   'const.inc'
      include 'read.pub'                          
      include 'error.pub'                         

*+  Purpose
*       Crop initialisation - reads constants from constants file

*+  Changes
*     010994 jngh specified and programmed
*     070495 psc added extra constants (leaf_app etc.)
*     110695 psc added soil temp effects on plant establishment
*     250996 jngh corrected type of lower limit of read_integer_var
*     010998 sb removed year upper and lower bounds.

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'ozcot_read_constants')
*
      character  section_name*(*)
      parameter (section_name = 'constants')

*+  Local Variables
      integer    numvals               ! number of values returned

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      call write_string (new_line//'    - Reading constants')

      call read_char_var (section_name
     :                     , 'crop_type', '()'
     :                     , c%crop_type, numvals)

      call read_real_array (section_name
     :                     , 'frudd', max_categories, '(dd)'
     :                     , c%frudd, numvals
     :                     , 0.0, 2000.0)

      call read_real_array (section_name
     :                     , 'wt', max_categories, '()'
     :                     , c%wt, numvals
     :                     , 0.0, 1.0)

      call read_real_array (section_name
     :                     , 'bltme', max_categories, '()'
     :                     , c%bltme, numvals
     :                     , 0.0, 1.0)

      call read_real_var (section_name
     :                     , 'row_spacing_default', '(m)'
     :                     , c%row_spacing_default, numvals
     :                     , 0.0, 2000.)
 
      call read_real_var (section_name
     :                     , 'popcon', '()'
     :                     , c%popcon, numvals
     :                     , 0.0, 1.0)
 
      call read_real_var (section_name
     :                     , 'fburr', '()'
     :                     , c%fburr, numvals
     :                     , 0.0, 5.0)
 
      call read_real_var (section_name
     :                     , 'acotyl', '()'
     :                     , c%ACOTYL, numvals
     :                     , 0.0, 1.0)
 
      call read_real_var (section_name
     :                     , 'rlai', '()'
     :                     , c%rlai, numvals
     :                     , 0.0, 1.0)
 
!jh      call read_real_var (section_name
!jh     :                     , 'dlds', '()'
!jh     :                     , c%dlds, numvals
!jh     :                     , 0.0, 1.0)
 
      call read_real_var (section_name
     :                     , 'leaf_res_n_conc', '()'
     :                     , c%leaf_res_n_conc, numvals
     :                     , 0.0, 1.0)
 
 
      call read_real_var (section_name
     :                     , 'a', '()'
     :                     , c%a, numvals
     :                     , 0.0, 1.0)
 
      call read_real_var (section_name
     :                     , 'b1', '()'
     :                     , c%b1, numvals
     :                     , -1.0, 1.0)
 
      call read_real_var (section_name
     :                     , 'b2', '()'
     :                     , c%b2, numvals
     :                     , -1.0, 1.0)
 
      call read_real_var (section_name
     :                     , 'b3', '()'
     :                     , c%b3, numvals
     :                     , 0.0, 1.0)
 
!jh      call read_integer_var (section_name
!jh     :                     , 'mode', '()'
!jh     :                     , c%mode, numvals
!jh     :                     , -1, 2)
 
      call read_real_var (section_name
     :                     , 'hucut', '()'
     :                     , c%hucut, numvals
     :                     , 0.0, 100.0)
 
      call read_real_var (section_name
     :                     , 'baset', '()'
     :                     , c%baset, numvals
     :                     , 0.0, 30.0)
 
!jh      call read_real_var (section_name
!jh     :                     , 'ambda', '()'
!jh     :                     , c%ambda, numvals
!jh     :                     , 0.0, 10.0)
 
      call read_real_var (section_name
     :                     , 'ul1', '()'
     :                     , c%ul1, numvals
     :                     , 0.0, 10.0)
 
      call read_real_var (section_name
     :                     , 'cona', '()'
     :                     , c%cona, numvals
     :                     , 0.0, 1.0)
 
      call read_real_var (section_name
     :                     , 'open_def', '()'
     :                     , c%open_def, numvals
     :                     , 0.0, 100.0)
 
!jh      call read_integer_var (section_name
!jh     :                     , 'iwindow', '()'
!jh     :                     , c%iwindow, numvals
!jh     :                     , 0, 100)
 
!jh      call read_real_var (section_name
!jh     :                     , 'sow_sw', '()'
!jh     :                     , c%sow_sw, numvals
!jh     :                     , 0.0, 10.0)
 
      call read_real_var (section_name
     :                     , 'a_root_leaf', '()'
     :                     , c%a_root_leaf, numvals
     :                     , 0.0, 10.0)
 
      call read_real_var (section_name
     :                     , 'a_stem_leaf', '()'
     :                     , c%a_stem_leaf, numvals
     :                     , 0.0, 10.0)
 
      call read_real_var (section_name
     :                     , 'e_par', '(g/mj)'
     :                     , c%e_par, numvals
     :                     , 0.0, 10.0)
 
      call read_real_var (section_name
     :                     , 'specific_lw', '(g/m2)'
     :                     , c%specific_lw, numvals
     :                     , 0.0, 100.0)
 
      call read_real_var (section_name
     :                     , 't_opt', '(oC)'
     :                     , c%t_opt, numvals
     :                     , 0.0, 50.0)
 
      call read_real_var (section_name
     :                     , 't_base', '(oC)'
     :                     , c%t_base, numvals
     :                     , 0.0, 20.0)
 
      call read_real_var (section_name
     :                     , 'wt_area_max', '()'
     :                     , c%wt_area_max, numvals
     :                     , 0.0, 400.0)
 
!jh      call read_real_var (section_name
!jh     :                     , 'wt_area_min', '()'
!jh     :                     , c%wt_area_min, numvals
!jh     :                     , 0.0, 100.0)
 
      call read_real_var (section_name
     :                     , 'embryo', '()'
     :                     , c%embryo, numvals
     :                     , 0.0, 1.0)
 
      call read_real_var (section_name
     :                     , 'f_leaf', '()'
     :                     , c%f_leaf, numvals
     :                     , 0.0, 1.0)
 
      call read_real_var (section_name
     :                     , 'f_stem', '()'
     :                     , c%f_stem, numvals
     :                     , 0.0, 1.0)
 
      call read_real_var (section_name
     :                     , 'f_root', '()'
     :                     , c%f_root, numvals
     :                     , 0.0, 1.0)
       call read_real_var (section_name
     :                     , 'elevation_default', '()'
     :                     , c%elevation_default, numvals
     :                     , -100.0, 1000.0)
 
      call read_real_var (section_name
     :                     , 'stress_wlog', '()'
     :                     , c%stress_wlog, numvals
     :                     , 0.0, 1.0)

      call read_real_var (section_name
     :                     , 'wlog_assimilate_red', '()'
     :                     , c%wlog_assimilate_red, numvals
     :                     , 0.0, 1.0)

      call read_real_var (section_name
     :                     , 'wlog_carcap_red', '()'
     :                     , c%wlog_carcap_red, numvals
     :                     , 0.0, 1.0)
      
      call read_real_var (section_name
     :                     , 'wlog_carcap_red_stress', '()'
     :                     , c%wlog_carcap_red_stress, numvals
     :                     , 0.0, 1.0)
      
      call read_real_var (section_name
     :                     , 'smi_affect_wlog', '()'
     :                     , c%smi_affect_wlog, numvals
     :                     , 0.0, 1.0)
      
      call read_integer_var (section_name
     :                     , 'days_relief_wlog', '(days)'
     :                     , c%days_relief_wlog, numvals
     :                     , 0, 20)
      
      call read_real_var (section_name
     :                     , 'frost_kill_immediate', '(oC)'
     :                     , c%frost_kill_immediate, numvals
     :                     , -5.0, 5.0)
      
      call read_integer_var (section_name
     :                     , 'frost_kill_immediate_das', '(days)'
     :                     , c%frost_kill_immediate_das, numvals
     :                     , 0, 200)
      
      call read_real_var (section_name
     :                     , 'frost_kill_delayed', '(oC)'
     :                     , c%frost_kill_delayed, numvals
     :                     , 0.0, 10.0)
      
      call read_integer_var (section_name
     :                     , 'frost_kill_delayed_das', '()'
     :                     , c%frost_kill_delayed_das, numvals
     :                     , 0, 200)
      
      call read_integer_var (section_name
     :                     , 'frost_kill_delayed_days', '()'
     :                     , c%frost_kill_delayed_days, numvals
     :                     , 0, 10)
        
      call read_real_var (section_name
     :                     , 'rtdep_max', '(cm)'
     :                     , c%rtdep_max, numvals
     :                     , 0.0, 500.0)
      
      call read_real_var (section_name
     :                     , 'harvest_n_frac', '()'
     :                     , c%harvest_n_frac, numvals
     :                     , 0.0, 1.0)
      
      call read_real_var (section_name
     :                     , 'cutout_smi_crit', '()'
     :                     , c%cutout_smi_crit, numvals
     :                     , 0.0, 1.0)
      
      call read_integer_var (section_name
     :                     , 'cutout_smi_days', '()'
     :                     , c%cutout_smi_days, numvals
     :                     , 0, 10)
       
      call read_real_var (section_name
     :                     , 'cutout_smi_site_red', '()'
     :                     , c%cutout_smi_site_red, numvals
     :                     , 0.0, 1.0)
        
      call read_real_var (section_name
     :                     , 'epcoef1', '()'
     :                     , c%epcoef1, numvals
     :                     , 0.0, 10.0)
        
      call read_real_var (section_name
     :                     , 'epcoef2', '()'
     :                     , c%epcoef2, numvals
     :                     , 0.0, 10.0)
       
      call read_real_var (section_name
     :                     , 'epcoef_smi_crit', '()'
     :                     , c%epcoef_smi_crit, numvals
     :                     , 0.0, 1.0)
      
      call read_real_var (section_name
     :                     , 'fbwstr_low', '()'
     :                     , c%fbwstr_low, numvals
     :                     , 0.0, 10.0)
      
      call read_real_var (section_name
     :                     , 'fbwstr_high', '()'
     :                     , c%fbwstr_high, numvals
     :                     , 0.0, 10.0)
      
      call read_real_var (section_name
     :                     , 'fbwstr_a', '()'
     :                     , c%fbwstr_a, numvals
     :                     , 0.0, 10.0)
      
      call read_real_var (section_name
     :                     , 'fbnstr_low', '()'
     :                     , c%fbnstr_low, numvals
     :                     , 0.0, 10.0)
      
      call read_real_var (section_name
     :                     , 'fbnstr_high', '()'
     :                     , c%fbnstr_high, numvals
     :                     , 0.0, 10.0)
      
      call read_real_var (section_name
     :                     , 'fbnstr_a', '()'
     :                     , c%fbnstr_a, numvals
     :                     , 0.0, 10.0)
      
      call read_real_var (section_name
     :                     , 'relp_smi_crit', '()'
     :                     , c%relp_smi_crit, numvals
     :                     , 0.0, 10.0)
      
      call read_real_var (section_name
     :                     , 'relp_intercept', '()'
     :                     , c%relp_intercept, numvals
     :                     , 0.0, 10.0)
      
      call read_real_var (section_name
     :                     , 'relp_slope', '()'
     :                     , c%relp_slope, numvals
     :                     , 0.0, 10.0)
      
      call read_real_var (section_name
     :                     , 'relp_low', '()'
     :                     , c%relp_low, numvals
     :                     , 0.0, 10.0)
      
      call read_real_var (section_name
     :                     , 'relp_high', '()'
     :                     , c%relp_high, numvals
     :                     , 0.0, 10.0)
      
      call read_real_var (section_name
     :                     , 'relp_a', '()'
     :                     , c%relp_a, numvals
     :                     , 0.0, 10.0)
      
      call read_real_var (section_name
     :                     , 'vsnstr_low', '()'
     :                     , c%vsnstr_low, numvals
     :                     , 0.0, 10.0)
      
      call read_real_var (section_name
     :                     , 'vsnstr_high', '()'
     :                     , c%vsnstr_high, numvals
     :                     , 0.0, 10.0)
      
      call read_real_var (section_name
     :                     , 'vsnstr_a', '()'
     :                     , c%vsnstr_a, numvals
     :                     , 0.0, 10.0)
      
      call read_real_var (section_name
     :                     , 'flfsmi_low', '()'
     :                     , c%flfsmi_low, numvals
     :                     , 0.0, 10.0)
      
      call read_real_var (section_name
     :                     , 'flfsmi_high', '()'
     :                     , c%flfsmi_high, numvals
     :                     , 0.0, 10.0)
      
      call read_real_var (section_name
     :                     , 'flfsmi_a', '()'
     :                     , c%flfsmi_a, numvals
     :                     , 0.0, 10.0)
      
      call read_real_var (section_name
     :                     , 'vlnstr_low', '()'
     :                     , c%vlnstr_low, numvals
     :                     , 0.0, 10.0)
      
      call read_real_var (section_name
     :                     , 'vlnstr_high', '()'
     :                     , c%vlnstr_high, numvals
     :                     , 0.0, 10.0)
      
      call read_real_var (section_name
     :                     , 'vlnstr_a', '()'
     :                     , c%vlnstr_a, numvals
     :                     , 0.0, 10.0)
      
      call read_real_var (section_name
     :                     , 'fw_low', '()'
     :                     , c%fw_low, numvals
     :                     , 0.0, 10.0)
      
      call read_real_var (section_name
     :                     , 'fw_high', '()'
     :                     , c%fw_high, numvals
     :                     , 0.0, 10.0)
      
      call read_real_var (section_name
     :                     , 'fw_a', '()'
     :                     , c%fw_a, numvals
     :                     , 0.0, 10.0)
      
      call read_real_var (section_name
     :                     , 'adjust_low', '()'
     :                     , c%adjust_low, numvals
     :                     , 0.0, 10.0)
      
      call read_real_var (section_name
     :                     , 'adjust_high', '()'
     :                     , c%adjust_high, numvals
     :                     , 0.0, 10.0)
      
      call read_real_var (section_name
     :                     , 'adjust_a', '()'
     :                     , c%adjust_a, numvals
     :                     , 0.0, 10.0)
      
      call read_real_var (section_name
     :                     , 'fwstrs_low', '()'
     :                     , c%fwstrs_low, numvals
     :                     , 0.0, 10.0)
      
      call read_real_var (section_name
     :                     , 'fwstrs_high', '()'
     :                     , c%fwstrs_high, numvals
     :                     , 0.0, 10.0)
      
      call read_real_var (section_name
     :                     , 'fwstrs_a', '()'
     :                     , c%fwstrs_a, numvals
     :                     , 0.0, 10.0)

      call read_real_var (section_name
     :                     , 'smi_delay_crit', '()'
     :                     , c%smi_delay_crit, numvals
     :                     , 0.0, 10.0)

      call read_real_var (section_name
     :                     , 'cold_shock_delay_crit', '()'
     :                     , c%cold_shock_delay_crit, numvals
     :                     , 0.0, 20.0)

      call read_real_var (section_name
     :                     , 'cold_shock_delay', '()'
     :                     , c%cold_shock_delay, numvals
     :                     , 0.0, 20.0)

      call pop_routine (my_name)
      return
      end


*     ===========================================================
      subroutine ozcot_start_crop ()
*     ===========================================================
      use ozcotModule
      implicit none
      include   'const.inc'            ! lu_scr_sum, blank
      include 'intrface.pub'                      
      include 'error.pub'                         

*+  Purpose
*       Start crop using parameters specified in passed record

*+  Changes
*     010994 jngh specified and programmed
*     090695 psc  add row spacing read
*     220696 jngh changed extract to collect

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'ozcot_start_crop')

*+  Local Variables
      integer    numvals               ! number of values found in array
      character  string*200            ! output string
      real       sdepth_mm             ! sowing depth in mm
      real       rs_mm                 ! row spacing in mm
!      character  module_name*8         ! module name

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      call Write_string ( 'Sow')
 
!      call get_current_module (module_name)

 
              ! get cultivar parameters

      sdepth_mm = 0.0
      rs_mm = 0.0
 
      call collect_char_var ('cultivar', '()'
     :                      , g%cultivar, numvals)
 
      call ozcot_read_cultivar_params ()
 
 
              ! get other sowing criteria
 
         ! variety,seed depth,rowspace,plants per m row
         ! cultivar, sowing_depth, row_spacing, plants_pm


      call collect_real_var ('plants_pm', '()'
     :                      , g%ppm, numvals, 0.0, 1000.0)
 
      call collect_real_var (
     :                       'sowing_depth', '(mm)'
     :                      , sdepth_mm, numvals
     :                      , 0.0, 100.0)
 
      call collect_real_var_optional (
     :                         'row_spacing', '(mm)'
     :                        , rs_mm, numvals
     :                        , 0.0, 2000.)
      if (numvals.eq.0) then
         g%rs = c%row_spacing_default
      else
      endif
       
      g%crop_in = .true.
      g%sdepth = sdepth_mm / 10.0
      g%rs = rs_mm /1000.0
         g%isow = g%jdate
           g%rtdep=g%sdepth
         g%ppm = g%ppm/g%rs  !  adjust for non standard rows incl skip
           g%pp= g%ppm*g%rs
           g%ps=(1.0/g%rs)/g%pp
           g%s=g%ps/g%rs
         g%rrig(sw_sowing) = g%sw               ! soil water at sowing
         g%iend = 1

 
 
          ! report
 
      call write_string (new_line//new_line)
 
      string = '                 Crop Sowing Data'
      call write_string (string)
 
      string = '    ------------------------------------------------'
      call write_string (string)
 
      call write_string ('    Sowing  Depth Plants Spacing Cultivar')
 
      call write_string ('    Day no   mm     m       mm     Name   ')
 
      string = '    ------------------------------------------------'
      call write_string (string)
 
      write (string, '(3x, i7, f7.1, f6.1, f9.1, 1x, a10)')
     :                g%isow, sdepth_mm
     :              , g%pp, rs_mm, g%cultivar
      call write_string (string)
 
      string = '    ------------------------------------------------'
      call write_string (string)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine ozcot_read_cultivar_params ()
*     ===========================================================
      use ozcotModule
      implicit none
      include   'const.inc'            ! new_line, lu_scr_sum, blank
      include 'read.pub'                          
      include 'error.pub'                         

*+  Purpose
*       Get cultivar parameters for named cultivar, from crop parameter file.

*+  Changes
*       090994 jngh specified and programmed

*+  Calls
                                       ! lu_src_sum

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'ozcot_read_cultivar_params')

*+  Local Variables
      character  string*200            ! output string
      integer    numvals               ! number of values read

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      call write_string (new_line//'   - Reading Cultivar Parameters')
 
 
      call read_real_var (g%cultivar
     :                    , 'percent_l', '()'
     :                    , p%percent_l, numvals
     :                    , 0.0, 1.0)
 
      call read_real_var (g%cultivar
     :                    , 'scboll', '()'
     :                    , p%scboll, numvals
     :                    , 0.0, 10.0)
 
      call read_real_var (g%cultivar
     :                    , 'respcon', '()'
     :                    , p%respcon, numvals
     :                    , 0.0, 1.0)
 
      call read_real_var (g%cultivar
     :                    , 'sqcon', '()'
     :                    , p%sqcon, numvals
     :                    , 0.0, 1.0)
 
      call read_real_var (g%cultivar
     :                    , 'fcutout', '()'
     :                    , p%fcutout, numvals
     :                    , 0.0, 1.0)
 
      call read_real_var (g%cultivar
     :                    , 'flai', '()'
     :                    , p%flai, numvals
     :                    , 0.0, 1.0)
 
      call read_real_var (g%cultivar
     :                    , 'ddisq', '()'
     :                    , p%DDISQ, numvals
     :                    , 0.0, 1000.0)
 
             ! report
 
      string = '    ------------------------------------------------'
      call write_string (string)
 
      write (string, '(4x,2a)')
     :                'Cultivar   = ', g%cultivar
      call write_string (string)
 
      write (string, '(4x, a, f7.2)')
     :                'percent_L  = '
     :               , p%percent_l
      call write_string (string)
 
      write (string, '(4x, a, f7.1)')
     :                'scboll     = '
     :               , p%scboll
      call write_string (string)
 
      write (string, '(4x, a, f7.3)')
     :                'respcon    = '
     :               , p%respcon
      call write_string (string)
 
      write (string, '(4x, a, f7.3)')
     :                'sqcon      = '
     :               , p%sqcon
      call write_string (string)
 
      write (string, '(4x, a, f7.4)')
     :                'fcutout    = '
     :               , p%fcutout
      call write_string (string)
 
 
      write (string, '(4x, a, f7.1)')
     :                'flai       = '
     :               , p%flai
      call write_string (string)
 
      write (string, '(4x, a, f7.1)')
     :                'ddisq      = '
     :               , p%DDISQ
      call write_string (string)
 
 
      string = '    ------------------------------------------------'
      call write_string (string)
 
      call write_string (new_line//new_line)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine ozcot_read_root_params ()
*     ===========================================================
      use ozcotModule
      implicit none
      include   'const.inc'            ! new_line, lu_scr_sum, blank,
      include 'data.pub'                          
      include 'read.pub'                          
      include 'error.pub'                         

*+  Purpose
*       Get root profile parameters

*+  Changes
*       090994 jngh specified and programmed
*     210395 jngh changed from ozcot_section to a parameters section

*+  Calls
                                       ! lu_scr_sum

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'ozcot_read_root_params')
*
      character  section_name*(*)
      parameter (section_name = 'parameters')

*+  Local Variables
      integer    layer                 ! layer number
  !    integer    num_layers            ! number of layers in profile
      character  string*200            ! output string

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      call write_string (new_line
     :                  //'   - Reading root profile parameters')
 
         !       ozcot_sw_supply
 
 
      call read_real_array_optional (section_name
     :                     , 'll', max_layers, '(mm/mm)'
     :                     , p%unul, p%num_ll_vals
     :                     , 0.0, 1.0)

      if (p%num_ll_vals.ne.0) then
         ! LL found
      else
         ! LL not found
         call warning_error (err_user
     :         , ' Ozcot LL not found. Using Soilwat LL15 instead.' )
      endif

          ! report
      call write_string (new_line//new_line)
 
      write (string,'(4x, a)') '    Root Profile'
      call write_string (string)
 
      string = '------------------------'
      call write_string (string)
 
 
      string = '     Layer       Lower '
      call write_string (string)
      string = '     Depth       Limit '
      call write_string (string)
 
      string = '     (cm)      (mm/mm) '
      call write_string (string)
 
      string = '------------------------'
      call write_string (string)
 
      do 2000 layer = 1, p%num_ll_vals
         write (string,'(1x, f9.1,f15.3)')
     :            g%dlayr_cm(layer)
     :          , P%unul(layer)
         call write_string (string)
2000  continue
 
      string = '------------------------'
      call write_string (string)
 
      call write_string (new_line//new_line)
 
      call pop_routine (my_name)
      return
      end

*     ===========================================================
      subroutine ozcot_end_crop ()
*     ===========================================================
      use ozcotModule
      implicit none
      include   'const.inc'            ! new_line, lu_scr_sum, blank,
      include   'convert.inc'          ! gm2kg, sm2ha, sm2smm
      include   'action.inc'
      include   'EVENT.inc'
      include 'intrface.pub'                      
      include 'postbox.pub'
      include 'error.pub'                         

*+  Purpose
*       Report occurence of harvest and the current status of specific
*       variables.

*+  Changes
*     010994 jngh specified and programmed
*      191200 dph  changed from unknown_module to all_active_modules
*                  unknown_module not supported in APSIM2.


*+  Calls
                                       ! lu_scr_sum

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'ozcot_end_crop')

*+  Local Variables
       real    res_dm                  ! Residue dry weight (kg/ha)
       real    res_N                   ! Amount of N in residue (kg/ha)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (g%crop_in) then
          ! crop harvested. Report status
 
         call Write_string ('End crop')

         res_dm = (g%dw_total - g%openwt / g%rs ) * 10.
         if (res_dm.le.0.) res_dm = 0.
         res_N = res_dm * 0.4 / 100.0

         call New_postbox ()

         call post_char_var('dlt_residue_type','()','cotton')

         call post_real_var ('dlt_residue_wt'
     :                        ,'(kg/ha)'
     :                        ,res_dm)

         call post_real_var ('dlt_residue_n'
     :                        ,'(kg/ha)'
     :                        ,res_N)

         call Action_send (
     :                              All_active_modules
     :                            , 'add_residue'
     :                            , Blank
     :                            )

         call Delete_postbox ()

         g%crop_in = .false.
         g%zero_variables = .true.

      else
      endif
      call pop_routine (my_name)
      return
      end

