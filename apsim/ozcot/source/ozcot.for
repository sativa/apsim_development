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

      else if (Action .eq. 'sow' .or. action .eq. 'harvest') then
         call ozcot_manager (Action, data_string)

      else if (Action .eq. ACTION_End_run) then
         call ozcot_end_run ()

      elseif (Action.eq.ACTION_init) then
         call ozcot_zero_variables ()
         call ozcot_Init ()

      else if (Action.eq.ACTION_Create) then
         call ozcot_zero_all_globals ()
 
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

      call ozcot_read_param ()

cpsc      call init()                      ! now called from o_zero_variables
      call ozcot_initial()
      call ozcot_get_other_variables ()
      call pop_routine(myname)
      return
      end



* ====================================================================
       subroutine ozcot_read_param ()
* ====================================================================
      use OzcotModule
      implicit none
       include 'const.inc'             ! Constant definitions
      include 'read.pub'
      include 'error.pub'

*+  Purpose
*      Read in all parameters from parameter file.

*+  Changes
*      psc - 09/08/93 first go
*      psc - 30/03/94 specified properly
*      DPH - 7/7/94  Removed free format internal read to g%title.  Line now
*                    reads g%title = param_string
*      psc - 15/4/98 Add ll, remove g%title, g%asoil from read
*      jngh - 30/4/98 kept numvals of ll read as global
*                     made reading of ll optional with a warning error if not found
*                    as ll15 will then be used.

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_read_param')
      character  section_name*(*)
      parameter (section_name = 'parameters')

*+  Local Variables
       integer numvals

*- Implementation Section ----------------------------------
      call push_routine(myname)
!         ! read in title from parameter file
!      call read_char_array (section_name
!     :                     , 'title', 15, '()'
!     :                     , title, numvals)

!         ! read in soil temperature factor from parameter file
!      call read_real_var (section_name
!     :                    , 'asoil', '()'
!     :                     , asoil, numvals
!     :                     , 0.0, 10000.0)

      call read_real_array_optional (section_name
     :                     , 'll', max_layers, '(mm/mm)'
     :                     , p%unul, p%num_ll_vals
     :                     , 0.0, 1.0)

      if (p%num_ll_vals.ne.0) then
         ! LL found
      else
         ! LL not found
         call warning_error (err_user
     :         , ' Cotton LL not found. Using Soilwat LL15 instead.' )
      endif

      call pop_routine(myname)
      return
      end

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
      g%DAY(:)   = 0.0
      g%HR(:)    = 0.0
      g%TEMPMX   = 0.0
      g%TEMPMN   = 0.0
      g%SOLRAD   = 0.0
      g%RAIN     = 0.0
      g%EPAN     = 0.0
      g%HUCUT    = 0.0
      g%BASET    = 0.0
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
      g%SUMES1   = 0.0
      g%SUMES2   = 0.0
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
      g%AMBDA    = 0.0
      g%CONA     = 0.0
      g%VPD      = 0.0
      g%BPER     = 0.0
      g%dlayr(:)            = 0.0
      g%dlayr_cm(:)         = 0.0
      g%ULLAYR(:)           = 0.0
      g%STLAYR(:)           = 0.0
      g%SWLAYR(:)           = 0.0
      g%SW                  = 0.0
      g%UL                  = 0.0
      g%sat                 = 0.0
      g%UL1                 = 0.0
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
      g%SQCON(:)           = 0.0
      g%RESPCON(:)         = 0.0
      g%POPCON              = 0.0
      g%FLAI(:)            = 0.0
      g%FCUTOUT(:)         = 0.0
      g%CARCAP              = 0.0
      g%CUTOUT              = 0.0
      g%VNSTRS              = 0.0
      g%FNSTRS              = 0.0
      g%RAD                 = 0.0
      g%PCLINT              = 0.0
      g%CARCAP_C            = 0.0
      g%CARCAP_N            = 0.0
      g%SCBOLL(:)          = 0.0
      g%FBURR               = 0.0
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
      g%OPEN_DEF            = 0.0
      g%AGRON_INP           = 0.0
      g%SOILW_INP           = 0.0
      g%COUNT_INP           = 0.0
      g%RAIN_INP            = 0.0
      g%MET_INP             = 0.0
      g%FCOT_OUT            = 0.0
      g%FRUCAL_OUT          = 0.0
      g%YIELD_OUT           = 0.0
      g%SOW_SW              = 0.0
      g%s_bed_mi            = 0.0
      g%s_bed_sat           = 0.0
      g%delay               = 0.0
      g%bpsum(:)          = 0.0
      g%A_ROOT_LEAF         = 0.0
      g%A_STEM_LEAF         = 0.0
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
      g%SPECIFIC_LW         = 0.0
      g%WT_AREA_MAX         = 0.0
      g%WT_AREA_MIN         = 0.0
      g%dDW_L(:)          = 0.0
      g%E_PAR               = 0.0
      g%T_OPT               = 0.0
      g%T_BASE              = 0.0
      g%EMBRYO              = 0.0
      g%F_LEAF              = 0.0
      g%F_STEM              = 0.0
      g%F_ROOT              = 0.0
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
      g%MODE                = 0
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
      g%IVAR                = 0
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
      g%IWINDOW             = 0
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
     :                              unknown_module
     :                            , 'add_residue'
     :                            , Blank
     :                            )

         call Delete_postbox ()

         g%crop_in = .false.
         g%zero_variables = .true.

      else
         ! Don't know about this event !!!

      endif

      ! Report the event to the rest of the system

      call Write_string ( Event_action)
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
cpsc      character  cv_name*20            ! name of cultivar
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
         g%rrig(2) = g%sw               ! soil water at sowing
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

cpcs                 ! get cultivar parameters

cpsc         call cm_cultv (cv_name)

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
cpc
      g%sat = 0.0
      g%wpwc=0.0

c      ullayr(j) = ullayr(j)*2.      !   simulate skip row
c      unul(j)   = unul(j)*2.        !          ditto

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


      call get_real_array (unknown_module, 'sw_dep', max_layers, '(mm)
     :'
     :                     , g%swlayr, g%nlayr
     :                     , 0.0, 1000.0)

      ! Convert water to plant available  (cm/cm)

      do 12 Layer = 1, g%nlayr
        g%swlayr(Layer) = g%swlayr(Layer) / 10. / g%dlayr_cm(layer)
     :                - p%unul(Layer)
        g%swlayr(Layer) = max(0.0, g%swlayr(Layer))
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
cpc
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

*- Implementation Section ----------------------------------
      call push_routine(myname)
      ! Convert water from plant available  (cm/cm)

      do 10 Layer = 1, g%nlayr
        g%swlayr(Layer) = (g%swlayr(Layer) + p%unul(Layer))*10.
     :                * g%dlayr_cm(layer)
        g%swlayr(Layer) = max(0.0, g%swlayr(Layer))
10    continue

      ! Send updated soil water

cjh      call set_real_array('sw_dep', swlayr, max_layers, '(mm)')
      call Set_real_array (unknown_module, 'sw_dep', '(mm)'
     :                    , g%swlayr, g%nlayr)

      ! extract soil NO3

      do 20 Layer = 1, g%nlayr
        g%ano3(Layer) = g%ano3(Layer) - (g%dn_plant*10. * g%ano3(Layer)
     :  / g%tsno3)
        g%ano3(Layer) = max(0.0, g%ano3(Layer))
        sNO3(layer) = g%ano3(layer) + g%no3mn(layer)
20    continue
      g%yest_tsno3 = g%tsno3 - (g%dn_plant*10.)

      ! Send updated soil N


cjh      call set_real_array('no3', sno3, nlayr, '(kg/ha)' )
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
cnh this is a simple fix only due to the limited future
cnh for this module!!!!!
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



cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc                                                                cc
cc                                                                cc
cc                       program ozcot                            cc
cc                                                                cc
cc                          27/5/83                               cc
cc                                                                cc
cc                                                                cc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c                                                                  c
c     begun during a visit to  t.a.e.s. blackland research center  c
c     temple, texas 1981 by a.b.hearn.                             c
c                                                                  c
c     developed at nars for namoi valley cotton  1982 to 1988      c
c     by hearn and da roza.                                        c
c         components:                                              c
c           water balance   - ritchie model taken largely from     c
c                             cornf(stapper & arkin)               c
c           nitrogen model -  developed at nars by hearn & da roza c
c           fruit submodel  - taken from siratac by hearn          c
c                                                                  c
c     ozcot1 - version for retrospective simulation of specific    c
c              crops; runs for one season only, irrigation dates   c
c              given.                                              c
c     ozcot2 - version for predictive simulation  through many     c
c              seasons; irrigation dates predicted.                c
c     ozcot3 - version for optimising irrigation                   c
c     ozcot4 - version for calibration with minos5                 c
c     ozcot5 - version for physiological analyis                   c
c                                                                  c
c                                                                  c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     structure
c       ozcot2  init
c               cinput2
c               newdate
c               metdat2 hhunc
c                       evap    fn satvp
c               decide_irg
c               solwat  sevap   fn watco
c                       swbal
c               sowday
c               emerg
c               snbal   n_fertilise
c               pltgrw  actlai
c                       laigen  fn senlf, fn stress
c                       cropn
c                       istsq
c                       fruit   bollwt  fn stress
c                               carrying_capacity fn stress
c                               overload
c                               actfru
c                               update
c                               fn frugen       fn stress
c                               fn survive
c               harvest
c               dayout2
c               (day_dudley    option for norm dudley)
c               yield
c               reset
c
c       note modules ozcot2, cinput2, metdat2, dayout2, decide_irg
c                    newdate, reset are specific to ozcot2.
c
c       ozcot.inc common blocks
c
c       input files: met.inp, soilw.inp, agron.inp - 1
c       output files: fruit.out, yield.out, calib.out - units = 2,3
c
c link/exe=ozcot2 ozcot2,init,cinput2,metdat2,hfunc,evap,satvp, -
c                 decide_irg,solwat,sevap,watco,swbal, -
c                 sowday,emerg,snbal,n_fertilise, -
c                 pltgrw,actlai,laigen,senlf,stress,cropn,istsq, -
c                 fruit,bollwt,carrying_capacity,overload,actfru, -
c                 update,frugen,survive, -
c                 harvest,dayout2,yield,reset
c
c
c                 ozcot2 - calling program
c                 ---------------------------
c
c
c      program ozcot2
      subroutine OZCOT2
      use OzcotModule
      implicit none
      include 'error.pub'

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot2')
      call push_routine(myname)
c
c     data agron_inp/'agron.inp'/, soilw_inp/'soilw.inp'/,
c    *count_inp/'count.inp'/, lai_inp/'lai.inp'/,
c    *rain_inp/'rain.inp'/, met_inp/'met.inp'/, fcot_out/'fcot.out'/,
c    *frucal_out/'frucal.out'/, yield_out/'yield.out'/
c
c      data iend/0/
c
c
cpsc      i=i+1

c
c      do 10 nszn = 1,100                     ! seasonal loop
c          do 20 i = 1,1000                   ! daily loop through whole year
c             call metdat2 (i,iend)       ! get met data
c              if(iend.eq.1)go to 31       ! end of met data
              CALL ozcot_metdat2
c             if(defirr(2).ne.0.) call decide_irg (i)  ! irrigated crop?
c              call solwat (i,dayir,npre)  ! soil water
              CALL ozcot_solwat                 ! soil water
c             if(defirr(2).ne.0.) call decide_irg       ! irrigated crop?
c             if(isow.le.0)then           ! crop sown yet?
c                  call sowday (i,iend)    ! sow tomorrow?
c                  call sowday             ! sow tomorrow?
c                  if(iend.ge.3) go to 32  ! passed sowing window or fallow
c              elseif(i.gt.isow .and. iemerg.le.0)then     ! crop emerged yet?
              if(g%iemrg.le.0) then
c                  call emerg (i)          ! emerge today?
                  CALL ozcot_emerg              ! emerge today?
              ENDIF
c              call snbal(i)               ! soil n balance
c              call snbal                  ! soil n balance
c              if(isow.gt.0 .and. i.gt.isow) call pltgrw (i,iend,nszn)
c              if(openz.gt.0.0) call harvest(iend)
              IF(g%isow.GT.0 .AND. g%das.GT.0) CALL ozcot_pltgrw
              IF(g%openz.GT.0.0) CALL ozcot_harvest
c              if(iend.eq.2)  go to 32     ! end of season
c              if(iend.ne.2)  then
c                call dayout2(i)             ! daily output
cpsc                call dayout2             ! daily output
c               call day_dudley             ! output for norm dudley
c20             continue                           ! end of daily loop
c                return
c              endif
c32           continue
c              call yield(nszn,iend)           ! calculate yield
c              call reset(iend)                ! reset variables for new season
              CALL ozcot_yield                      ! calculate yield
cpsc              call reset                      ! reset variables for new season
c10       continue                               ! end of seasonal loop
c31        continue
c          call exit
c      stop

       call pop_routine(myname)
       return
       END

c


c      subroutine pltgrw (i,iend,nszn)
      SUBROUTINE ozcot_pltgrw

c-------------------------------------------------------------------
c      calls the various plant growing routines.  at this point    !
c      there are also some variable conversions to allow merging   !
c      of the independently derived soil and plant portions of the !
c      model.                                                      !
c-------------------------------------------------------------------

      use OzcotModule
      implicit none
      include 'error.pub'

      real percent_l
cpc   integer ifrost
      integer j

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_pltgrw')
      DIMENSION PERCENT_L(10)
      DATA PERCENT_L/0.38,0.38,0.39,0.42,0.4,5*0.0/        ! lint percent
c      data fburr /1.23/                       ! factor sc/boll to sc+burr/boll
cpc   data ifrost/0/                          ! flag for simulated frost

c----- housekeeping -----------------------------------------------------------
      call push_routine(myname)
cpsc      iday=i-isow ! replaced ncrpdy throughout, 15 nov 1983
      g%iday=g%das
      g%dd=g%hunits
      g%rad=g%solrad
      g%pclint = PERCENT_L(g%ivar)             ! set lint percent for variety

      IF(g%iday.EQ.1) THEN
          g%ifrost = 0                         ! 1st g%day, reset local variable
      ELSE
          DO 10 J=1,g%iday
              g%fyzage(J)=g%fyzage(J)+g%dd     ! physiological aging
10        continue
      ENDIF
      g%sumdd=g%sumdd+g%dd

c----- increase root depth ----------------------------------------------------

      IF(g%iday.LE.36)g%rtdep=g%sdepth+((20.-g%sdepth)/36.)*real(g%iday) ! W*N
cpsc        changed maximum rooting depth
cpsc  if(iday.ge.37.0)rtdep=122.38*(1.-2.65*exp(-.03*iday)) ! 82/8
      IF(g%iday.GE.37.0)g%rtdep=182.38*(1.-2.65*EXP(-.03*g%iday)) ! 82/8
      IF(g%rtdep.GT.g%rtdepm)g%rtdep=g%rtdepm

c---- check if frost terminates crop -----------------------------------------

c      if(tempmn.le.2. .and. iday.gt.120) then   ! frost? if so end of season
      IF(g%tempmn.LE.0. .AND. g%iday.GT.90) THEN ! frost? if so end of season
          g%iend=2 ! flag for frost - g%last g%day, open bolls > 80% mature
      ELSE IF(g%tempmn.LT.5. .AND. g%iday.GT.120) THEN ! frost? if so end of season
          g%ifrost = g%ifrost+1                 ! count days to simulate frost
          IF(g%ifrost.EQ.3) THEN
              g%iend = 2                        ! frost
          ENDIF
      ELSE
          g%ifrost = 0                          ! reset because sequence broken
      ENDIF

c      if(iend.eq.2) write(2,777)jdate,iday,i
c777        format(' frost on julian day ',i3,', ',i3,
c     *    'days from sowing, index=',i3,' *** end of season ***')

c----- emergence ( why not call ozcot_emerg here ------------------------------------

      IF(g%das.LT.g%iemrg.OR.g%iemrg.EQ.0) GO TO 25
      IF(g%das.EQ.g%iemrg)  g%ddmerg=g%sumdd-g%dd

c----- increase leaf area -----------------------------------------------------

c      if(i.le.ilai)then
c       call actlai(i)
c       call actlai
c      else
c       call laigen(i)
        CALL ozcot_laigen
c      end if

      call ozcot_dryxmatter
      call ozcot_plant_n

c----- crop nitrogen ---------------------------------------------------------

c      call cropn(i)
      CALL ozcot_cropn

c---- grow plant -------------------------------------------------------------

      IF(g%isq.EQ.0) THEN
c          call istsq (i,nszn)
          CALL ozcot_istsq
      ELSE
c          if(i.gt.isq) call fruit (i,iend)
          IF(g%das.GT.g%isq) CALL ozcot_fruit
      ENDIF

c      if(isyr.eq.82 .and. jdate.eq.354) call hail(i) ! hail in 1982-83 experiment
c      if(isyr.eq.82 .and. jdate.eq.354) call hail    ! hail in 1982-83 experiment

25    continue

c---- crop development complete? ---------------------------------------------

      IF(g%openz.GT.0.0 .AND. g%bollz.EQ.0.0)
     * g%iend=2                                 ! all bolls open, end of season

c------ following are for use in s/r yield -----------------------------------

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

c------------------------------------------------------------------------------
      call pop_routine(myname)

      RETURN
      END


c      subroutine bollwt(idayx,l)
      SUBROUTINE ozcot_bollwt(L)

c     calculates increase in weight of each days's bolls.
c     bollgrowth rate is driven by dd,limited by water,
c     n and c(incl water effects on photosynthesis) stress

      use OzcotModule
      implicit none
      include 'error.pub'


c------stuff done on 1st call of the day - stresses & growth rate -------------
      !  functions
      real ozcot_stress

      ! locals (i hope!)
      integer l
      real fbcstr
      real fbwstr
      real fbnstr
      real strsbl
      real f_temp
      real boll
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_bollwt')

      call push_routine(myname)

      IF(g%idayx.EQ.0 .OR. g%iday.EQ.g%idayx+1) THEN ! 1st call for this day?
        g%idayx = g%iday                     ! if so, set flag to stop more calls
        FBCSTR = 1.0                         ! Cc stress factor to reduce g%bollgr
        IF(g%bload.GT.0.)FBCSTR = g%carcap_c/g%bload ! supply/demand ratio for when
        IF(FBCSTR.GT.1.)FBCSTR = 1.          ! boll growth limited by Cc supply
        FBWSTR = ozcot_stress(0.0,0.5,1.0,g%smi)   ! water stress on bolls
        FBWSTR = 1.                          ! try no direct stress - 24/4/92
        IF(g%bollz+g%openz .LT. g%carcap_n) THEN ! final boll req < uptake
            FBNSTR = 1.0                     ! do not apply N stress
        ELSE                                 ! final boll req < uptake
            FBNSTR = ozcot_stress(0.0,0.5,1.0,g%fnstrs)    ! apply N stress
        ENDIF
        STRSBL = AMIN1(FBCSTR,FBWSTR,FBNSTR) ! minimum of Cc, water & N stress

        IF(g%tempav.LT.20.0) THEN
          F_TEMP = -3.0+0.2*g%tempav          ! temperature scaling factor
        ELSE IF(g%tempav.GT.30.0) THEN        ! for boll weight
          F_TEMP = 7.0-0.2*g%tempav           ! derived from Hesketh and Low 1968
        ELSE
          F_TEMP = 1.0
        ENDIF
        IF(F_TEMP.LT.0.) F_TEMP = 0.
        IF(F_TEMP.GT.1.) F_TEMP = 1.

        BOLL = g%scboll(g%ivar)*F_TEMP        ! effect of temperature
        g%bgrvar = BOLL*g%bper                ! boll growth rate this g%day

      ENDIF

      IF(L.GT.g%lfru(4) .OR. g%lfru(4).EQ.0) then
         call pop_routine(myname)
         RETURN
      else
      endif

c------         increase in weight of bolls (seed cotton) ---------------------------

      g%bollgr = g%bgrvar*STRSBL              ! todays rate per boll
      g%bollgr = g%bollgr *1.3                ! potential unstressed rate
      IF(g%bollgr.LT.0.) g%bollgr = 0.
      g%fruwt(L) = g%fruwt(L)+g%fruno(L)*g%bollgr ! increase today for L g%day bolls

c------ shed fruit not growing ------------------------------------------------

      IF(L.GT.g%lfru(5) .AND. L.LE.g%lfru(7) .AND. g%fruno(L).GT.0.)
     :THEN
           IF(g%fruwt(L)/g%fruno(L).LT.0.1) g%frmark(L,6) = g%fruno(L)
      ENDIF

c------------------------------------------------------------------------------
      call pop_routine(myname)
      RETURN
      END


c      subroutine carrying_capacity(i)
      SUBROUTINE ozcot_carrying_capacity

c     estimates carrying capacity of crop on basis of photosynthesis.
c     selects parameter for variety. adjusted for water stress.
c     carcap is carrying capacity, maximum number of bolls the crop
c     can carry, therefore the boll load that causes 100% shedding.

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


c      data from "siratac" - 1987-88  hearn (pers. comm.)

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot')
cpc   data istress/0/, ireliefco/0/
      call push_routine(myname)

cpsc      if(i.eq. isq+1) then
      IF(g%das.EQ. g%isq+1) THEN
          g%istress = 0                 ! reset stress flag
          g%ireliefco = 0               ! reset stress relief counter
      ENDIF

cpsc      if(bload.gt.cutout .and. smi.lt.0.75) istress = 1 ! set stress cutout flag

cpsc      if(smi.gt.0.75 .and. istress.gt.0) then

      if(g%bload.gt.g%cutout.and.g%smi.lt.0.25) g%istress=1 ! 0.25 replaced 0.75 - ABH 5/11/96
      if(g%smi.gt.0.25.and.g%istress.gt.0) then       ! 0.25 replaced 0.75 - ABH 5/11/96

          g%ireliefco = g%ireliefco + 1 ! count days since relief of stress
          IF(g%ireliefco.EQ.7) THEN
              g%istress = 0             ! end stress effect on wterlogging
              g%ireliefco = 0           ! reset counter
          ENDIF
      ENDIF

c----photosynthetic capacity --------------------------------------------------
c-----light interception modified to give hedgerow effect with skip row - abh 5/11/96 ------

      g%alai = g%alai*g%rs         ! lai in hedgerow
      ALIGHT = (1.-EXP(-ozcot_kvalue*g%alai)) ! original code  - now gives interception in
                                                ! hedgerow
      ALIGHT =ALIGHT/g%rs          ! interception on ground area basis
      g%alai = g%alai/g%rs         ! restore LAI to ground area basis

      RADN_watts = g%solrad*0.8942      ! convert RADN from ly to watts m**2
      P_gossym = 2.391+RADN_watts*(1.374-RADN_watts*0.0005414) ! GOSSYM line1275
      POT_PN = P_gossym*0.068           ! potential photosynthesis g%g/m2 CH2O
      PN = POT_PN*ALIGHT                ! net photosynthesis term
c----- effect of water stress on photosysthesis -------------------------------

      IF(g%smi .LT. 0.5) THEN
          RELP =.25+.864*g%smi              ! effect of water stress on Pp
          IF(RELP.GT.1.)RELP = 1.           ! (TURNER g%et al 1986).
          RELP = RELP -.25
          RELP = ozcot_stress(0.0,1.0,0.5,RELP)   ! increase severity of stress
          RELP = RELP + .25
          PN = PN*RELP                      ! photosynthesis adjust for water stress
      ENDIF

c----- waterlogging effect additional to n uptake - hearn & constable 1984 eqn 4

c      if(istress.gt.0) then
c        if(sw/ul.gt.0.87) pn = pn * 0.1 ! carrying capacity reduced
c      else
c        if(sw/ul.gt.0.87) pn = pn * 0.1 ! carrying capacity reduced
c      endif

c---- maintenance respiration -------------------------------------------------

      TEMPLF = g%tempav+5.-10.*g%smi    ! leaf temperature
      IF(TEMPLF.LT.g%tempav) TEMPLF=g%tempav
      RFAC = 2.2**((TEMPLF-35.)/10.)    ! resp temp factor, Horie(Constable)
      RM = g%sites*g%respcon(g%ivar)*RFAC ! maintenance respiration term

c----- carrying capacity carbon - photosynthesis divided by boll growth rate

      IF(g%bgrvar.GT.0.)
     * g%carcap_c = (PN-RM)/(g%bgrvar*g%fburr) ! carrying capacity, carbon, no stress
      IF(g%carcap_c.LT.0.) g%carcap_c = 0. ! trap

c----- waterlogging effect additional to n uptake - hearn & constable 1984 eqn 4
      IF(g%istress.GT.0) THEN
        IF(g%sw/g%ul.GT.0.87) g%carcap_c = g%carcap_c * 0.0 ! carrying capacity reduced
      ELSE
        IF(g%sw/g%ul.GT.0.87) g%carcap_c = g%carcap_c * 0.2 ! carrying capacity reduced
      ENDIF

      g%cutout = g%carcap_c*g%fcutout(g%ivar) ! boll load for g%cutout, sq prodn stops
c-------------------------------------------------------------------------------
      call pop_routine(myname)
      RETURN
      END


c      subroutine cropn(i)
      SUBROUTINE ozcot_cropn

      use OzcotModule
      real harvest_n
      real bgr

c     assumes all n available for season (availn) from snbal
c     is taken up (uptakn).
c     computes: n harvested on basis of constable and rochester 1988
c               n in fruit frun
c               carrying capacity (carcap_n), number of bolls for which
c                       harvest_n is adequate
c                       used in frugen and survive for squares
c               stresses:
c                       vegetative (vnstrs for laigen) = f(1-harvest_n/uptakn)
c                               ie function of proportion of n remaining in veg
c                         fruit (fnstrs for bollwt) 1- frun/harvest_n
c                               ie function of n to be harvested not in fruit


c----------------------------------------------------------------------------
      g%uptakn = g%availn   ! potential uptake for season based on available N

c-----  compute potential n harvested ----------------------------------------

      HARVEST_N = g%uptakn * 0.85    ! harvestable N; 0.85 fromConstable & Rochester
      HARVEST_N = HARVEST_N/10.      !  kg/ha to g%g/m**2, N limiting fruit.

c----- compute n already in fruit -------------------------------------------
cpdev this is generating an underflow warning:
cpdev ---------------------------------vvvvvvvvvv
      g%seed_nc = .02407+0.000147*g%uptakn-0.00000034*g%uptakn**2 ! GAC dat 3/6/88
      g%frun = (g%frudw+g%openwt)*((1.-g%pclint)*g%seed_nc+(g%fburr-1.)*
     :.005) ! N in frt

c----- compute n carrying capacity --------------------------------------------

      BGR = g%scboll(g%ivar)                ! seed coton per boll
      g%carcap_n = HARVEST_N /(BGR*0.6*0.03) ! bolls per m

c----- compute n stresses -----------------------------------------------------

      g%vnstrs = (g%uptakn-10.)/g%uptakn ! vegetative stress 10=uptake @ 1st boll

      IF(g%bollz.EQ.0.) THEN            ! before 1st boll
          g%fnstrs = 1.0
      ELSE
          g%fnstrs = 1.- g%frun/HARVEST_N ! fraction of harvestable not in fruit
      ENDIF

      IF(g%vnstrs.GT.1.0) g%vnstrs=1.0
      IF(g%vnstrs.LT.0.0) g%vnstrs=0.0
      IF(g%fnstrs.GT.1.0) g%fnstrs=1.0
      IF(g%fnstrs.LT.0.0) g%fnstrs=0.0
c----------------------------------------------------------------------------

      RETURN
      END


c      subroutine emerg (i)
      SUBROUTINE ozcot_emerg
c-------------------------------------------------------------------
c
c     simulates  emergence
c
c-------------------------------------------------------------------

      use OzcotModule

      IF(g%iemrg.GT.0) RETURN

c----- simple heat sum as an alternative to wanjura's function -----

       IF(g%sumdd .LT. 60.) RETURN
       g%iemrg = g%das
       RETURN
       END

c-------------------------------------------------------------------

c      previous version jackson/arkin

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      calculates hypocotle elongation leading up to emergence     c
c      based on the growing-degree-day concept.                    c
c      the elongation rate of 0.12 cm/gdd is use to a maximum rate c
c      of 2.04 cm/gdd at about 32 c.                               c
c      the gdd base temperature is set in the "init" subroutine.   c
c      elongation rate data are from wanjura et.al. 1970.          c
c                                                                  c
c      key variables:                                              c
c          sdepth = seed sowing depth                              c
c          stemp = soil temperature (calc. in subroutine "metdat") c
c          iemrg = emergence date                                  c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c       t = stemp                             ! soil temp
c       if(stemp.lt.14.44)t = 14.44+(14.44-t) ! to comput lag whenstemp < 14.44
c       hypoel = 0.0853-0.0057/(41.9-t)*(t-34.44)**2 ! wanjura's function
c       hypoel = hypoel*24.                   ! convert to daily rate
c       hypoel = hypoel*0.6                   ! tune to namoi valley
c       if(stemp.lt.14.44)hypoel = hypoel*(-0.5) ! delay below 14.44
c       esum = esum+hypoel
c
c      if(esum .lt. sdepth) return
c      iemrg = i
c
c      return
c      end


c      subroutine evap (i)
      SUBROUTINE ozcot_evap
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      calculates potential evapotransperation from modified pen-  c
c      man equation as reported by ritchie.                        c
c                                                                  c
c      key variables:                                              c
c          delta = slope of the water vapor/ air temp. curve       c
c          albedo = combined plant-soil albedo                     c
c          h = net radiation balance                               c
c          eo = potential et over bare soil                        c
c          eos = potential et below plant canopy                   c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      use OzcotModule
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

c---------- calculate bowen ratio term -----------------------------------------

        ELEV=200.0                                    ! ELEVATION (M)
        Pp=1013.0-0.105*ELEV                          ! MEAN PRESSURE(MB)
        GAMMA=6.6E-04*Pp*(1.0+0.00115*g%tempwt)    ! PSYCHROMETER CONSTANT
        TK=g%tempav+273.                                ! MEAN TEMP(DEG KELVIN)
        DELTA=(EXP(21.255-5304./TK))*(5304./(TK**2.)) !SLOPE g%sat VAP PRES CURVE
        D=DELTA/(DELTA+GAMMA)

c------ calculate fraction radiation reaching surface(tr) ----------------------
c       culls functions(unpublished)


        XN1=0.404*ALOG(g%s)+1.49
cpsc
        g%alai = g%alai*g%rs        ! lai in hedgerow
        IF(g%alai.LT.XN1) THEN     ! when LAI below XN1 threshold
            g%tr=EXP(-0.6*g%alai)
        ELSE                        ! when LAI above XN1 threshold
            g%tr=EXP(-0.398*g%alai)
        ENDIF

cpsc
        F_INT = 1-g%tr              ! intercetion in hedgerow
        F_INT = F_INT/g%rs          ! interception on ground area basis
        g%tr =  1.-F_INT            ! transmission on ground area basis
        g%alai = g%alai/g%rs        ! restore LAI to ground area basis

c        if(rs.gt.1.) tr = (tr+rs-1.)/rs ! adjust for rows wider than im

c-----vapor pressure deficit: mean of vpd at 9am and 3pm ,assuming atmos.
c     vp.(vpdry) remains constant throughout day, and tmax=t3pm-----------------

        SVPMAX=ozcot_satvp(g%tempmx)      ! g%sat VP AT TMAX=g%sat VP AT T3PM
        SVPDRY=ozcot_satvp(g%tempdy)      ! g%sat VP AT 9AM TDRY
        SVPWET=ozcot_satvp(g%tempwt)      ! g%sat VP AT 9AM TWET

           VPDRY=SVPWET-GAMMA*(g%tempdy-g%tempwt) ! atmospheric VP
           IF(VPDRY.LE.0.) VPDRY = 0.1         ! cannot be -ve.
           g%vpd=((SVPDRY-VPDRY)+(SVPMAX-VPDRY))/2.

c------ calculate potential evaporation (eo) -----------------------------------

        SALPHA=0.09
        ALBEDO=0.3067+0.3333*SALPHA-((0.23-SALPHA)/0.75)*g%tr ! ??

c------net longwave radiation(r6,cal/cm2)--------------------------------------

c      ritchie(1975)unpub.;idso & jackson(1969)j geophys res,74:5397-5403;
c      jensen et al(1969)proc am soc civil engin,96:61-79                             ! temple,texas
c        r4=1.-.261*exp(-7.77e-04*tempav**2.)        ! sky emissivity,ea
c        r6=(r4-.96)*1.17e-07*tk**4.*(.2+.8*solrto)  ! ((ea-es)*@*tk**4)*(-)

        R4=1.08*(1.-EXP(-VPDRY**(TK/2016.)))
        R6=(R4-1.)*1.*1.17E-07*TK**4.*(.2+.8*g%solrto) ! ((EA-g%es)*@*TK**4)*(-)
                                                     ! g%es=EMISSIVITY OF EVAP SURFACE

c-------net radiation(h,cal/cm2)----------------------------------------------

        H=(1.-ALBEDO )*g%solrad+R6
        IF(H.LT.0.0) H=0.0

        g%ho=H/583.                                 ! NET RADIATION(CM)
c       go=g/583.                                   ! soil heat flux(cm)

c-------advection or aerodynamic term ------------------------------------------

        IF(g%wind.NE.0..AND.g%tempwt.NE.0.) THEN
            AT = (1.-D)*0.01*(1.+.028*g%wind)*g%vpd ! advection: g%wind & g%vpd
        ELSE IF(g%wind.EQ.0..AND.g%tempwt.NE.0.) THEN
            AT = -11.9813+0.012*g%vpd+0.0404*TK     ! advection: g%vpd but no g%wind
        ELSE
            AT = -20.4355+0.0697*TK                 ! advection:  no g%vpd or g%wind
        ENDIF
        IF(AT.LT.0.0) AT = 0.0

        g%eo=g%ho*D+AT                              ! GdaR Mar'85, update ABH Mar88.

c------ calculate potential below canopy evaporation (eos) ---------------------

        RNS=g%ho*g%tr ! GdaR Mar'85
        g%eos=D*RNS ! GdaR Mar'85
        IF(g%eos.LT.0.0)g%eos=0.0
        IF(g%eos.GT.g%eo)g%eos=g%eo

        RETURN
        END


c      function frugen(i)
      real FUNCTION ozcot_frugen(ndas)
c
c     estimates generation of new fruit as function of existing
c     fruiting sites and bolload adjusted for nitrogen stress

c      data from "siratac" - 1987-88 hearn (pers. comm.).

      use OzcotModule
      include 'data.pub'
      integer ndas

      real ozcot_stress

cpc   real sites1
      real blr
      real dfru
      real vsnstr
      real ppm_row
      real popfac

c     psc      i=icount
c-----------------------------------------------------------------------------
c     initialise on first call for new crop
c-----------------------------------------------------------------------------

c      if(i.eq.isq+1)then           ! 1st call,ie day after 1st square
      IF(ndas.EQ.g%isq+1)THEN         ! 1st call,ie g%day after 1st square
          g%n_cutout=0             ! flag to show g%cutout, >= 1 = not squaring
          g%sites1=0.0             ! to save current value of g%sites
c          alag=0.0                 ! reset lag accumulator
      ENDIF
      ozcot_frugen=0.                    ! delete y'day's value and reset to zero
c-----------------------------------------------------------------------------
c     is this 1st cycle, cutout or 2nd cycle (regrowth)?
c-----------------------------------------------------------------------------

      BLR = 1.                             ! boll load ratio =1 when g%cutout=0
c      if(i.eq.isq+1) blr = 0.              ! except day after 1st square
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

c-----------------------------------------------------------------------------
c     if coutout due to water stress or full boll load ie smi < 0.75
c     10% of fruiting sites become inactive for frugen every day after 5 days
c-----------------------------------------------------------------------------

      IF(g%smi.LT.0.75) g%n_cutout = g%n_cutout + 1   ! count days g%cutout
      IF(g%n_cutout.GT.5) g%sites1 =g%sites1 + 0.1*g%size*g%ppm ! inactive g%sites

      RETURN

20    continue
      IF(g%sites1.GT.g%sites) g%sites1 = g%sites
      g%n_cutout=0                  ! recovery complete, squaring resumes

30    continue

c-----------------------------------------------------------------------------
c     square production for this day
c-----------------------------------------------------------------------------

      g%size = (g%sites-g%sites1)/g%ppm ! active g%sites per plant for FRUGEN & SURVIV
      IF(g%size.LT.1.0) g%size = 1.0 ! average  plants has 1 site
      IF(g%size.LT.0.5/g%ppm) g%size = 0.5/g%ppm ! average  plants has 1 site

      IF(g%carcap_c.EQ.0.) THEN     ! either 1st g%day afer squaring or defoliated
c          if(i.eq.isq+1) then                             ! day after 1st square
          IF(ndas.EQ.g%isq+1) THEN                           ! g%day after 1st square
             DFRU = g%sqcon(g%ivar)*SQRT(g%size)*g%ppm    ! g%sites per g%dd
          ELSE
             RETURN                                       ! defoliated FRUGEN=0.
          ENDIF
      ELSE
          DFRU = g%sqcon(g%ivar)*SQRT(g%size)*g%ppm*(1.-g%bload/
     :    g%cutout) ! g%sites per g%dd
          VSNSTR = ozcot_stress(0.0,0.9,1.0,g%vnstrs)
          DFRU = DFRU * VSNSTR
          IF((g%bollz+g%openz)/g%carcap_n .GE. 1.0) DFRU = 0. ! N limiting

      ENDIF

      PPM_ROW = g%ppm*g%rs               ! plants per m row for POPFAC
      POPFAC = 1./(1.+g%popcon*PPM_ROW)   ! plant population factor within row
      DFRU = DFRU * POPFAC               ! adjust for plant population
      ozcot_frugen = DFRU * g%dd               ! today's squares
      IF(ozcot_frugen.LT.0.0) ozcot_frugen=0.0
c      if(isyr.eq.77 .and. i.lt.isq+15) ozcot_frugen=ozcot_frugen*0.1 ! delay for 77/78 season - 23/6/88

      RETURN
      END


c     subroutine fruit (i,iend)
       SUBROUTINE ozcot_fruit

c      ages fruit, sums fruit in categories, estimates physiological
c      shedding and survival of fruit.  calls s/r actfru to estimate
c      new squares flowers and open bolls from counts. alternatively
c      calls s/r ozcot_frugen to estimate new squares when counts not
c      available.

      use OzcotModule
      real ozcot_frugen, ozcot_survive

      real frudd
      real wt
      integer n, m, ndayfl
      real sfmcat, bltme, surv
      integer l , lf, mm, mmm, nfl, if

c      dimension frudd(8),wt(8),sfmcat(8),bltme(8),bpsum(300)
      DIMENSION FRUDD(8),WT(8),SFMCAT(8),BLTME(8)
      DATA FRUDD/50.,180.,350.,380.,520.,660.,870.,1100./
      DATA BLTME/3*0.0,0.07,0.21,0.33,0.55,1.0/
      DATA WT/.0104,.0272,.1441,.0988,.5042,.9617,1.0,.5785/

c      data scboll /5.0,5.0,4.7,4.5,5.5,4*.0,7./  ! dp16,dp61,dp90,siok,sica
c      data respcon/.025, .025, .02306, .01593, .02306, 4*.0,.025/ !  ditto
c      data sqcon  /.021, .021, .02057, .02283, .02057, 4*.0,.021/ !   ditto
c      data fcutout/.4789, .4789, .4789, .5411, .4789,  4*.0,.48/ !  ditto
c      data flai   /1.0, 1.0, 0.87, 0.52, 0.87, 4*0.0,1./         !  ditto
c      data popcon /.03633/
c
c----  re-intialise arrays and variables to zero where appropriate -------------

c      if(i.eq.isq+1)then        ! 1st call of new season on day after 1st square
c          do 10 n=1,300
c              bpsum(n)=0.0
c10        continue
c          idayx = 0             ! flag to bollwt, called for 1st time this day
c      endif

      DO 100 N=1,8
          g%frucat(N)=0.
          SFMCAT(N)=0.
          IF(N.GT.7)GO TO 100
          DO 20 M=1,7
              g%fmkcat(N,M)=0.
20        continue
100   continue

      g%frudw = 0.
      NDAYFL=0

c---- compute boll period ------------------------------------------------------

      g%bper = 1.0/EXP(5.385-.0512*g%tempav) ! boll period Constable

c-----------------------------------------------------------------------------
c     the next loop ( do 200 l=....) goes through the fruit arrays
c     starting with the element with oldest fruit and therefore the smallest
c     index, in order to develop or abscise fruit, and put in categories.
c     before entering the loop, find the oldest fruit.
c-----------------------------------------------------------------------------

      LF=g%lfru(9)    ! 1st active element in fruit array, oldest immature fruit
cpsc      if(lf.eq.0)lf=isq-isow-10 ! start in fruno 10 days before 1st square
      IF(LF.EQ.0)LF=g%isq-10 ! start in g%fruno 10 days before 1st square

cpsc      do 200 l=lf,i-isow-1 ! loop from oldest growing boll to previous day
      DO 200 L=LF,g%das-1 ! loop from oldest growing boll to previous g%day
          IF(L.GT.300)GO TO 200
cpsc          call bollwt(idayx,l)
          CALL ozcot_bollwt(L)

c---- age & abscise marked fruit----------------------------------------

          DO 30 M = 1,6
              MM = 6-M+1
              MMM = MM+1
              g%frmark (L,MMM) = g%frmark(L,MM)
30        continue
          g%frmark(L,1)=0.                 ! CLEAR FIRST g%day
          IF(g%fruno(L).GT.0. .AND. g%frmark(L,7).GT.0.)THEN ! fruit shed today?
              IF(g%frmark(L,7).GT.g%fruno(L))THEN
c                  write(2,999) i,jdate,l,fruno(l),frmark(l,7)
c999               format
c     *            (' too many marked fruit on i, jdate:',3i4,2f5.1)
c                  frmark(l,7) = fruno(l)
              ENDIF
              g%fruwt(L) = g%fruwt(L)*(g%fruno(L)-g%frmark(L,7))/
     :        g%fruno(L) ! adjust wt sheds
              g%fruno (L)=g%fruno(L)-g%frmark(L,7) ! remove marked fruit

          ENDIF

c---- sort fruit and marked fruit into age categories  -----------------------

          IF(g%fyzage(L).GT.FRUDD(3))
     *    g%bpsum(L)=g%bpsum(L)+g%bper          ! develop g%day L's bolls
          IF(g%n_def.GT.0) g%bpsum(L)=g%bpsum(L)/0.99 ! develop faster after defoliation
          IF(g%iend.EQ.2 .AND. g%bpsum(L).GE.0.9) g%bpsum(L)=1.0 ! frost opens phys mat bolls

          DO 40 N=1,8                              ! stay in loop until category found
              IF(N.LT.4)THEN                    ! if yes, squares
                  IF(g%fyzage(L).LT.FRUDD(N)) GO TO 204
              ELSE                              ! no, therefore flowers or bolls
                  IF(g%bpsum(L).LT.BLTME(N))GO TO 204
              ENDIF
40        continue

c------- if last loop completed, fruit are open bolls -------------------------

          IF(L.LE.g%lfru(9))GO TO 200 ! this days bolls already open?
          g%lfru(9)=L               ! this days bolls open today, reset marker
c          if(i.le.idate)go to 202   ! use actual counts or not?
          g%openz=g%openz+g%fruno(L) ! simulated bolls open added to total
          g%openwt=g%openwt+g%fruwt(L)

c202       continue

          g%fruno(L)=0.0 ! delete open bolls from array
          g%fruwt(L)=0.
          GO TO 200

204       continue
          IF(L.LE.g%lfru(N))GO TO 206 ! this days fruit in category N  yet?
          g%lfru(N)=L               ! this days fruit into category N today.
          IF(N.NE.4)GO TO 206       ! category is flowers?
          NDAYFL=NDAYFL+1           ! count this days flowering
c          if(i.gt.idate)go to 206   ! using actual counts or not?
c          fruno(l)=0.0              ! clear for actual counts
c          go to 200

206       continue
          g%frucat(N)=g%frucat(N)+g%fruno(L)       ! sum fruit nos in categories
          IF(N.GE.4) g%frudw = g%frudw+g%fruwt(L)  ! sum dry wt of fruit
          IF (N.GT.7) GO TO 200
          DO 50 M=2,6                                 ! loop thro marked fruit
              g%fmkcat(N,M)=g%fmkcat(N,M)+g%frmark(L,M) ! sum marked fruit
              SFMCAT(N)=SFMCAT(N)+g%frmark(L,M)    ! sum marked fruit for g%bload
50        continue

200   continue

c---- total squares, green (growing) bolls, open bolls---------------------

      g%squarz=0.
      g%bollz=0.
      g%bload=0.                                     ! reset
      DO 60 N=1,8
          IF (N.LE.3) g%squarz=g%squarz+g%frucat(N)  ! total squares
          IF (N.GE.4.AND.N.LE.8) g%bollz=g%bollz+g%frucat(N) ! total bolls
          g%bload=g%bload+(g%frucat(N)-SFMCAT(N))*WT(N)  !  boll load
60    continue

c---- this day's production of fruit -----------------------------------------

c      call carrying_capacity(i)
      CALL ozcot_carrying_capacity
      IF(g%bload.GT.g%carcap_c .OR. g%bollz+g%openz.GT.g%carcap_n)
     *CALL ozcot_overload                                  ! abort excess fruit

c      if (i.le.idate)then                            ! use counted fruit?
c      if (das.le.idate)then                            ! use counted fruit?
c          call actfru (i)
c          call actfru
c          call update(1,1,daysqz,squarz,frudd)
c          if(lfru(4).ne.0..or.daysfl.ne.0.)
c     *        call update(4,ndayfl,daysfl,bollz,frudd)
c          if(lfru(9).ne.0..or.daysop.ne.0.)
c     *        call update(9,1,daysop,openz,frudd)
c      else
cpsc           icount = i               ! dummy parameter to use frugen
c          fruno(i-isow)=frugen(i)  ! frugen is function generating squares
          g%fruno(g%das)=ozcot_frugen(g%das) ! ozcot_frugen is function generating squares

c      end if

c---- mark this day's fruit for physiological shedding      -----------------

      SURV = ozcot_survive(g%carcap_c,g%bload) ! square survival rate

c      if(jdate.lt.59) surv = 0.1       ! for krs with delayed protection
c      surv =surv*0.33                  ! for pest damage in 1972/3 namoi

c      frmark(i-isow,1)=fruno(i-isow)*(1.-surv)
c      if(frmark(i-isow,1).lt.0.) frmark(i-isow,1)=0.0
c      fmkcat(1,1)=fmkcat(1,1)+frmark(i-isow,1)
      g%frmark(g%das,1)=g%fruno(g%das)*(1.-SURV)
      IF(g%frmark(g%das,1).LT.0.) g%frmark(g%das,1)=0.0
      g%fmkcat(1,1)=g%fmkcat(1,1)+g%frmark(g%das,1)
      IF(NDAYFL.EQ.0)GO TO 501
      SURV = ozcot_survive(g%carcap_c,g%bload) ! boll survival rate
c      surv =surv*0.33                  ! for pest damage in 1972/3 namoi

      DO 70 NFL = 1,NDAYFL
          IF = g%lfru(4)-NFL+1
          g%frmark(IF,1) = g%fruno(IF)*(1.-SURV)
          IF(g%frmark(IF,1).LT.0.)g%frmark(IF,1) = 0.0
          g%fmkcat(4,1) = g%fmkcat(4,1)+g%frmark(IF,1)
70    continue

c---- add new fruit to totals ------------------------------------------------

  501 CONTINUE

c      sites = sites+fruno(i-isow)
c      if(i.le.idate)return ! squarz & frucat updated in update
c      squarz = squarz+fruno(i-isow)
c      frucat(1) = frucat(1)+fruno(i-isow)
      g%sites = g%sites+g%fruno(g%das)
      IF(g%das.LE.g%idate)RETURN ! g%squarz & g%frucat updated in UPDATE
      g%squarz = g%squarz+g%fruno(g%das)
      g%frucat(1) = g%frucat(1)+g%fruno(g%das)

c-----------------------------------------------------------------------------

      RETURN
      END


c      subroutine harvest(iend)
      SUBROUTINE ozcot_harvest

c     this subroutine simulates defoliation and picking
c     use n_def, n_pick and j_pick for cost of defoliation:       nb action
c         cost of defoliant is n_def * $25                 <-- for gross margins
c         cost of picking is n_pick * $?                   <-- for gross margins
c         cost of delay is (j_pick - 91) * $ per day       <-- for gross margins
c              if 1st april (day 91) is reference date.
c     likelihood of 2nd pick is indicated. currently 10 boll/m (bollz=10)
c     assumed to be worth picking, depends on price of cotton and cost of
c     picking, ask growers. should be part of management model.
c     date of 2nd pick and partition of yield between 1st and 2nd pick are
c     subjects for future development in a more comprehensive whole farm
c     management model.

      use OzcotModule

      IF(g%n_def.EQ.0 .AND. g%squarz/g%ppm.GT.2.) RETURN ! too many squares
      IF(g%openz/(g%bollz+g%openz).GT.g%open_def/100. .AND. g%j_pick.EQ.
     :0) THEN
          IF(g%n_def.EQ.0) THEN
               g%n_def = 1                               ! 1st defoliant spray
               g%i_def = g%iday                          ! g%day of 1st defol.
c               write(2,100) n_def, jdate, i_def
          ENDIF
          IF(g%n_def.EQ.1 .AND. g%iday-g%i_def.GE.10) THEN ! 10 days since 1st?
               IF(g%alai.GT.0.2) THEN
                   g%n_def=2                             ! 2nd defoliant spray
                   g%i_def=g%iday                        ! g%day of 2nd defol
c               write(2,100) n_def, jdate, i_def
               ELSE
                   g%j_pick = g%jdate                    ! date of picking
                   g%n_pick = 1                          ! count picks
c                   write(2,101) j_pick
                   IF(g%bollz.GT.10.) THEN                ! 10 bolls worth picking
                       g%n_pick = 2                      ! count picks
c                       write(2,102)
                   ENDIF
               ENDIF
          ENDIF
          IF(g%n_def.EQ.2 .AND. g%iday-g%i_def.EQ.10) THEN
              g%j_pick = g%jdate                         ! date of picking
              g%n_pick = 1                               ! count picks
c              write(2,101) j_pick
              IF(g%bollz.GT.10.) THEN
                  g%n_pick = 2                           ! count picks
c                  write(2,102)
              ENDIF
          ENDIF
      ENDIF
      IF(g%j_pick.NE.0 .AND. g%bollz.LT.1.0) THEN
              g%iend = 2                                 ! terminate crop
c              write(2,103) jdate, iday
      ENDIF
c
c100   format(' defoliant spray',i2,' on day',i4,',',
c     *       i4,' days from sowing.')
c101   format(' first pick 0n',i4)
c102   format(' there are sufficient bolls for a 2nd pick')
c103   format(' simulation terminated on day',i4,','i4,
c     *': no further boll growth, all bolls forced open.')
      RETURN
      END


c      subroutine hfunc (i)
      SUBROUTINE ozcot_hfunc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      calculates daily heat units based on a 12.0 deg c base      c
c      temperature.  the heat unit sum is based an integrated      c
c      diurnal sin-wave with the maximum and minimum daily temp-   c
c      eratures as the peak and trough, respectively.  heat units  c
c      are not allowed to accumulate above a cutoff of 30.0 c.     c
c      the base (baset) and cutoff (hucut) temperatures are set in c
c      the 'init' subroutine.                                      c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      use OzcotModule
      real pi
      real amp
      real tmax
      real zeta

c
      PI=3.14159
c      tempav=(tempmn+tempmx)/2.
      AMP=g%tempmx-g%tempav
      TMAX=g%tempmx
      IF(TMAX.GE.g%hucut) TMAX=g%hucut
      IF(g%tempmn.GE.g%baset) GO TO 10
      IF(TMAX.LE.g%baset) GO TO 20
      ZETA=ASIN((g%baset-g%tempav)/AMP)
      g%hunits=1./PI*(AMP*COS(ZETA)+(g%tempav-g%baset)*(PI/2.-ZETA))
      GO TO 30
   10 g%hunits=(TMAX+g%tempmn)/2.-g%baset
      GO TO 30
   20 CONTINUE
      g%hunits=0.0
   30 CONTINUE

      RETURN
      END


c      subroutine init
      SUBROUTINE ozcot_INITIAL()
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      initializes variables in all common blocks                  c
c                                                                  c
c      key variables:                                              c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      use OzcotModule
cpc      real soltro, spi, cumep, cumes, cumet, deltsw, dal
cpc      real bolln, bolnc
cpc      integer nfert
      integer lastiday, j, isdex, ncnt, lastfr, k

!      block data ozcot_initials
c
c
      g%scboll = (/5.0,5.0,4.7,4.5,5.5    
     :            , .0, .0, .0, .0,7./) ! DP16,DP61,DP90,SIOK,SICA
      g%respcon= (/.025, .025, .02306, .01593, .02306
     :            , .0, .0, .0, .0,.025/) !  ditto
      g%sqcon  = (/.021, .021, .02057, .02283, .02057
     :            , .0, .0, .0, .0,.021/) !   ditto
      g%fcutout= (/.4789, .4789, .4789, .5411, .4789
     :            , .0, .0, .0, .0,.48/) !  ditto
      g%flai   = (/1.0, 1.0, 0.87, 0.52, 0.87
     :            , .0, .0, .0, .0,1./)       !  ditto
      g%popcon =.03633
c
      g%fburr = 1.23                     ! factor sc/boll to sc+burr/boll
c
c      data leaf_res_n_conc/0.02/
c
!      end

c------------------------------------------------------------------------------
c      'bclim' climate parameters
c------------------------------------------------------------------------------
c
      g%mode=2 ! OZCOT1: -1=calib'n, 1=valid'n; OZCOT2: 2=full simulation
c
cpsc      nszn = 1
cpsc      i = 0

      g%iend=0
      g%iday=0
cpc   lastiday = 0
cpsc      jdate=0
cpsc      imet=0
      g%tempav=0.0
      g%tempre=0.0
      g%tempyd=0.0
      g%alint=0.0
      g%hunits=0.0
      g%hucut=40.0 ! ABH changed from 30 to 40 - 11/11/83
      g%baset=12.0
      g%asoil=0.0
cpc   soltro=0.0
      g%stemp=0.0
c      eo=0.0
c      eos=0.0
cpc     spi=0.0
cpc     cumep=0.0
cpc     cumes=0.0
cpc     cumet=0.0
c
c      tempmx=0.0
c      tempmn=0.0
c      solrad=0.0
c      rain=0.0
c      newran= 0
c------------------------------------------------------------------------------
c     irrigation variables
c------------------------------------------------------------------------------
      DO 10 J=1,10
c          igday(j)=0
          g%rrig(J)=0.0
10    continue
c------------------------------------------------------------------------------
c      'bsoil' soil profile parameters
c------------------------------------------------------------------------------
      g%isw = 0  ! flag to initialise water balance in SOLWAT
      g%ambda=1.44 ! Priestly-Taylor  daRosa 1983
      g%ul1=1.4  ! limit for stage 1 g%es  daRosa 1983
      g%cona=0.35 ! g%es rate in stage 2  daRosa 1983
cpc   isdex=0.0
      g%nlayr=0
c      sw=0.0
c      ul=0.0
      g%def=0.0
      g%nirr=0
      g%twater=0.
cpc   deltsw=0.0
        g%rtsw=1.0
      DO 20 J=1,6
c      dlayr(j)=0.0
c     ullayr(j)=0.0
c      swlayr(j)=0.0
      g%trans(J)=0.0
   20 CONTINUE
c------------------------------------------------------------------------------
c      'bplnt' plant parameters
c------------------------------------------------------------------------------
cpc     ncnt=0
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
cpc   dal=0.0
      g%shedlf=0.0
      g%frudw=0.0
      g%smi=0.0
      g%sdepth=0.0
      g%rtdep=0.0
      g%rtgrow=0.0
c------------------------------------------------------------------------------
c      'bnitr' soil nitrogen transformation parameters
c------------------------------------------------------------------------------
cpc   nfert=0
      g%uptakn=0.0
        g%availn=0.0

c      do 30 j=1,2
c      nday(j)=0
c      snaplc(j)=0.0
c   30 continue
c------------------------------------------------------------------------------
c      'fruits' parameters are numbers,weights,abscission counts
c      for bolls and squares.
c------------------------------------------------------------------------------
cpc   lastfr=0
      g%bgrvar=0.0
      g%dd=0.0
      g%ddmerg=0.0
      g%sumdd=0.0
      DO 40 J=1,8
      g%frucat(J)=0.0
      g%lfru(J)=0
   40 CONTINUE
      g%lfru(9)=0

      DO 50 J=1,300
      g%dlai(J)=0.0
      g%ddw_l(J) = 0.0
      g%bpsum(j) = 0.0
      g%fyzage(J)=0.0
      g%fruno(J)=0.0
      g%fruwt(J)=0.0
      DO 50 K=1,7
      g%frmark(J,K)=0.0
      IF(J.LT.8) g%fmkcat(J,K)=0.0
   50 CONTINUE

c------------------------------------------------------------------------------
c      'totals' counts of squares, bolls, and sites at any given time
c------------------------------------------------------------------------------

      g%bload=0.0
      g%bollz=0.0
      g%openz=0.0
      g%openwt=0.0
      g%sites=0.0
cpc
      g%sites1 = 0.0
      g%squarz=0.0
c------------------------------------------------------------------------------
c      'index' stress and survival indices.
c------------------------------------------------------------------------------
      g%carcap=0.0
      g%carcap_c = 0.0
      g%carcap_n = 0.0
      g%cutout=0.0
      g%vnstrs=1.0
      g%fnstrs=1.0
      g%idayco = 0
      g%last_day = 0
c------------------------------------------------------------------------------
c      'counts'
c------------------------------------------------------------------------------
c      do 60 j=1,25
c      jco(j)=0
c      sqcnt(j)=0.0
c      blcnt(j)=0.0
c      opcnt(j)=0.0
c   60 continue
c------------------------------------------------------------------------------
c      'bpnitr' are for plant nitrogen.
c------------------------------------------------------------------------------
      g%uptakn=0.0
      g%vegn=0.0
cpc   bolln=0.0
      g%plantn=0.0
cpc   bolnc=0.0
      g%strucn=0.0
      g%frun=0.0
c------------------------------------------------------------------------------
c     /yield/ for output with yield
c------------------------------------------------------------------------------
      g%alaiz=0.
      g%ilaiz=0
      g%plntnz=0.
      g%iplntn=0
      g%sqzx = 0.
      g%isqzx = 0
      g%def_last=0.
c------------------------------------------------------------------------------
c     /pick/ variables to simulated defoliation and picking
c------------------------------------------------------------------------------
      g%j_pick=0
      g%n_pick=0
      g%n_def=0
      g%i_def=0
      g%open_def = 60.
c------------------------------------------------------------------------------
c     /sow/
c------------------------------------------------------------------------------
      g%iwindow = 60     ! width of sowing window
      g%sow_sw = 0.0     ! for subsoil water rule
c------------------------------------------------------------------------------
c     /drywt/ variables for simulation of dry weight increase
c------------------------------------------------------------------------------
      g%a_root_leaf = 1.01 ! allometric constant root:leaf. Huxley's data 1964
      g%a_stem_leaf = 1.25 ! allometric constant stem:leaf. Huxley's data 1964
      g%e_par = 2.5        ! g%g/MJ Charles-edwards g%et al 1986
      g%fburr = 1.23       ! ABH
      g%specific_lw = 58.0 ! g%g/m2  GAC 71/72, Hoffman & Rawlins 1971, Ben-Porath
      g%t_opt = 25.        ! Constable 1981
      g%t_base = 8.        ! Constable 1981
      g%wt_area_max = 150. ! Hesketh and Low 1968, Hoffman & Rawlins = 80.
      g%wt_area_min = 30.0 ! Huxley 1964
      g%embryo = 0.75      ! dry weight of seedling at emergence
      g%f_leaf = 0.6       ! proportion of leaf dry weight at emergence
      g%f_stem = 0.15      ! ditto for stem
      g%f_root = 0.25      ! ditto for root  -  data by extrapolation from Huxlry

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

      RETURN
      END


c      subroutine istsq (i,nszn)
      SUBROUTINE ozcot_istsq

c     identifies first square event and gives initial value for
c     fruno(1),sites & squarz.
c     delayed by water stress (smi < 0.25) and cold shock (tempmn < 11)
c     currently sets isq=i, should be isq=iday for consistency between seasons
c     when convenient, change and check all refs to isq

      use OzcotModule
      real ddisq


      DIMENSION DDISQ(10)

c      data iszn/0/                         ! flag for new season
      DATA DDISQ/9*420.,320./ ! Constable (pers. comm. 1983), 10=Empire

      IF(g%idate.NE.0) GO TO 40

c     no counts - simulate first square

c      if(iszn.ne.nszn) then                ! is this a new season?
c          iszn = nszn                      ! reset flag
c          delay = 0.0                      ! delay in day degrees
c      end if

cpsc    add delay to common block

      IF(g%smi.LT.0.25) THEN               ! water stress delays squaring
          g%delay = g%delay+(1.-(g%smi/0.25))*g%hunits
      END IF

      IF(g%tempmn.LT.11.) g%delay = g%delay+5.2 ! cold shock Constable (pers. comm)

      IF(g%sumdd.LT.DDISQ(g%ivar)+g%delay)RETURN
c      fruno(i-isow)=1.0*ppm  ! average plant has 1 square
c      fruno(i-isow)=0.5      ! as in 1983/84 version
      g%fruno(g%das)=1.0*g%ppm ! average plant has 1 square
      g%fruno(g%das)=0.5  ! as in 1983/84 version
      GO TO 43
c
c      using counts
c
   40 CONTINUE
c      do 41 n=1,25
c      if(jco(n).eq.0)return !  no squares counted yet
c      if(i.lt.jco(n)+1)return
c      if(sqcnt(n+1).gt.0.)go to 42 ! when jco(n) eq i and squares at next count
c   41 continue
c      return
c   42 continue
c      fruno(i-isow)=sqcnt(n+1)/(jco(n+1)-jco(n))
c      next=n ! for actfru
c
c     square & site production on first day
c
   43 CONTINUE
c      squarz=squarz+fruno(i-isow) ! sum squares
c      sites=sites+fruno(i-isow) ! sum sites
c      isq=i                     ! should be isq=iday see above
      g%squarz=g%squarz+g%fruno(g%das) ! sum SQUARES
      g%sites=g%sites+g%fruno(g%das) ! sum g%sites
      g%isq=g%das                ! should be g%isq=g%iday see above

      RETURN
      END


c      subroutine laigen (i)
      SUBROUTINE ozcot_laigen
c
c     estimates current lai. generates new leaves daily as a
c     function of dd and fruiting site production. initial area
c     is a function of dd to first square then a function of new
c     sites. relative rate of area expansion a function of dd
c     to 1st square, then a function of dsites .
c      all leaf areas are square meters per plant except alai
c      which is m**2/m**2 i.e. lai.
c
      use OzcotModule
      real ozcot_stress, ozcot_senlf

      real leaf_res_n_conc
      real rlai, dlds, acotyl, dlds_x, vlnstr
      real a, b1, b2, b3, flfsmi, actrgr, alaix
      real ddleaf
      integer index, in, l
      DATA ACOTYL/.00035/,RLAI/.00835/,DLDS/0.0362/
      DATA LEAF_RES_N_conc/0.02/


c------- initialise leaf area on day of emergence -----------------------------

        IF(g%das.EQ.g%iemrg) THEN                    ! already emerged?
c           dlai(iemrg-isow)=acotyl*ppm            ! initial area
c           alai=dlai(iemrg-isow)
c           lastlf=iemrg-isow                      ! set index for oldest leaf
            g%dlai(g%iemrg)=ACOTYL*g%ppm      ! initial area
            g%alai=g%dlai(g%iemrg)
            g%lastlf=g%iemrg                  ! set index for oldest leaf
            RETURN
        ENDIF

c------- calculate rate of leaf area expansion --------------------------------

        A  =   0.1847  !
        B1 =  -0.1165  ! constants for LAI calibration eqn
        B2 =  -0.01514 ! 29 April 1988
        B3 =   0.01984 ! expts WN8283 & 8384

        dLdS_X = (A+B1*0.75+B2*g%vpd+B3*0.75*g%vpd) ! sqrt area/site, no water stress
        IF(dLdS_X.LT.0.) dLdS_X = 0.
        dLdS_X = dLdS_X**2                      ! area per site, no water stress

        dLdS = (A+B1*g%smi+B2*g%vpd+B3*g%smi*g%vpd) ! sqrt area per site
        IF(dLdS.LT.0.) dLdS = 0.
        dLdS = dLdS**2                          ! area per site

        FLFSMI = ozcot_stress(0.0,0.5,1.0,g%smi) ! pre-squaring

c-------------------------------------------------------------------------------

        IF(g%isq.EQ.0) THEN               ! crop not yet squaring

            ACTRGR=RLAI                   ! actual RGR
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
            dLdS_X = dLdS_X*g%flai(g%ivar)    ! adjust for variety, 87 MKI calib'n
            g%dlai_pot = g%fruno(g%iday-1)*dLdS_X ! days incr in LAI
                                              ! with water stress
            dLdS = dLdS*g%flai(g%ivar)        ! adjust for variety, 87 MKI calib'n
            g%dlai(g%iday) = g%fruno(g%iday-1)*dLdS ! days incr in LAI

        ENDIF
        VLNSTR = ozcot_stress(0.0,0.9,1.0,g%vnstrs)
        g%dlai(g%iday) = g%dlai(g%iday)*VLNSTR ! adjust for N stress
        g%dlai_pot = g%dlai_pot*VLNSTR ! adjust for N stress
        g%alai=g%alai+g%dlai(g%iday)

c*******senescence ***************************************************

        DDLEAF=ozcot_senlf(g%bload,g%alai,g%carcap_c,g%smi)
        IF(g%n_def.EQ.1 .AND. g%iday-g%i_def.GT.7) DDLEAF = DDLEAF*0.33 ! 1st defol'n
        IF(g%n_def.EQ.2 .AND. g%iday-g%i_def.GT.7) DDLEAF = DDLEAF*0.0 ! 2nd defol'n
        g%shedlf=0.
        g%leaf_res = 0.                 ! initialise for this g%day
        IF(g%lastlf.EQ.0)GO TO 21       ! called after measured LAI finished
c       do 20 l=lastlf,i-isow           ! loop thro unshed leaves
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

      g%leaf_res_n = g%leaf_res * LEAF_RES_N_CONC ! N content of leaf residues

      RETURN
      END


c      subroutine metdat2 (i,iend)
      SUBROUTINE ozcot_metdat2
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      subroutine to check input climate data for missing or er-   c
c      roneous values.  input variable units are assumed input     c
c      and converted as follows:                                   c
c          temperature -      deg c*10 to deg c                    c
c          rainfall    -      mm/day*10 to cm/day                  c
c          rainfall    -      inches/day to cm/day                 c
c          solrad      -      ly/day (not converted)               c
c                                                                  c
c      unique variables:                                           c
c          qa = an upper boundary for solrad                       c
c          solrto = needed constant for net radiation in "evap"    c
c                                                                  c
c       when temperature (including wet and dry bulb) and          c
c       radiation are missing or not avaialble, ezstimates are     c
c       made as function of days since last rain.                  c
c       consequently a full suite of met data elements can be      c
c       generated from rainfall alone.                             c
c                                                                  c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      use OzcotModule
      include 'data.pub'
      integer nsince, ndate
      real root, rclear, srad, wet_depress, date
      DATA NSINCE/0/
c
c         **** read met data  ***************************************
c
c       read(1,151,end=153) jdate,imyr,
c     *  rain,epan,tempmx,tempmn,tempdy,tempwt,wind,solrad
c151    format(4x,i3,5x,i4,f5.0,f4.1,2f4.0,2f4.1,f4.0,f4.0) ! new

c     **** climate data now read in as part of interface. ****

cpsc        solrad = solrad / 0.04186
cpsc        rain = rain / 10.0
        g%wind = 0.
        g%tempdy = 0.
        g%tempwt = 0.
cpsc        epan = 0.
c

cjh
       IF(g%jdate.EQ.1) THEN               ! new year?
         g%mdpy = 365                     ! reset days per year
         IF((g%imyr/4*4).EQ.g%imyr) g%mdpy=366 ! leap year
       ENDIF


c       if(epan.eq.0. .or. epan.eq.99.9)      epan=epanx
c       epanx=epan

c       if(wind.eq.0. .or. wind.eq.999)  then
c           if(windx.ne.999) then
c               wind=windx
c           else
c               wind=0
c           endif
c       endif
c       windx=wind

c      go to 155
c153   iend=1 ! flag end of data
c      return
c
c155   continue
cpsc      epan=epan/10.
c
c         **** check and convert rainfall (cm) ****
c
c        if(rain.lt.0.0) rain=0.0
c       rain=rain/100.0

c  local rain data

c       if(newran.eq.0) go to 100
c       if(jdate.ge.nrnsdy(1).or.jdate.le.nrnsdy(newran)) rain=0.0
c        do 10 j=1,newran
c         if(jdate.eq.nrnsdy(j)) rain=ransub(j)/10.
c10      continue
c100    continue

c  days since rain

      IF(g%isow.GT.0) THEN  ! from sowing to first boll
        IF(g%bollz.EQ.0.0) THEN
          g%rrig(3) = g%rrig(3)+g%rain     ! accumulate rainfall before bolling
        ELSE IF(g%bollz.GT.0.0 .AND. g%openz/(g%openz+g%bollz).LT.0.2)
     :  THEN ! bolling
          g%rrig(4) = g%rrig(4)+g%rain     ! accumulate rainfall in bolling
        ENDIF
      ELSE
          g%rrig(5) = g%rrig(5)+g%rain     ! accumulate rainfall in fallow
      ENDIF

      IF(g%rain.LE.0.1) THEN
        IF(NSINCE.LT.1) NSINCE=1
        NSINCE = NSINCE + 1
      ELSEIF(NSINCE.GT.1) THEN
        NSINCE=1                    ! 1st g%day of g%rain period, light g%rain
        IF(g%rain.GT.10.) NSINCE = 0 ! heavy g%rain
      ELSE
        NSINCE = 0                  ! g%rain period
      ENDIF
      ROOT =FLOAT(NSINCE)
      IF(ROOT.GT.5.) ROOT=5.
      IF(ROOT.GT.0.) ROOT = SQRT(ROOT)
      IF (NSINCE.GT.9) NSINCE=9


c      **** check solar radiation and fill in for missing data. ****
c      **** please notice that in the following lines location  ****
c      **** specific equations are used.    (cal/cm2)           ****

c  calculate extraterrestrial radiation at nars

c       xrad=(jdate+9)*(360./365.)*.0174533 ! day of year as angle in radians
c        qa=749.6+(302.4*sin(1.562+xrad)) ! extra terrestrial radiation(nars)

c       if(solrad.lt.0.0)solrad=0.0
c        if(solrad.gt.0.0 .and. solrad.ne.999.) go to 30

c estimate missing ground measured solar radiation

c       if(jdate.gt.135)go to 152
c       qqa=0.66-0.4708*exp(-0.75*nsince)        ! q/qa for days 1-135
c        if(nsince.le.1)qqa=0.4658-0.003485*rain
c        go to 160
c152    continue
c       if(jdate.ge.225)go to 154
c       qqa=0.5892-0.7986*exp(-1.219*nsince)     ! q/qa for days 136-225
c        if(nsince.le.1)qqa=0.1382+0.2777*exp(-0.04375*rain)
c        go to 160
c154    continue
c       qqa=0.63324-0.7693*exp(-1.0*nsince)
c        if(nsince.le.1)qqa=0.2148+0.2087*exp(-0.01875*rain)
c160    continue
c       solrad=qa*qqa           ! est of ground rad =f(days since rain)
c        solrad_min = qa*0.18      ! minimum - 0.18 from brutsart(1982)
c       if(solrad.lt.solrad_min)solrad=solrad_min
c30     continue
c
c   actual solar/clear day solar radiation ratio for longwave estimate
c
        RCLEAR=551.52+246.40*SIN(0.0172*(g%jdate+99.96)) !CLEAR g%day SOL g%rad(NARS)
        SRAD=g%solrad
        IF(SRAD.GT.RCLEAR) SRAD=RCLEAR
        g%solrto=SRAD/RCLEAR
c        solrad=srad

c       **** check and convert air temperatures (deg c) ****
c
c       tempmx=tempmx/10.0
c       tempmn=tempmn/10.0

c      if(tempmx.eq.0. .or. tempmx.eq.99.9) then

c  estimate missing tempmx

c       tmaxxx=26.24+8.325*sin(1.172+xrad)   ! tmax 8 or more days after rain
c       ftmax=1.03-0.1812*exp(-0.1953*nsince)! tmax/tmaxxx
c       tempmx=tmaxxx*ftmax               ! tmax=f(days since rain)
c       if(rain.gt.4.0)tempmx=tmaxxx*.83
c       if(rain.gt.5.0)tempmx=tmaxxx*.8

c      endif

c      if(tempmn.eq.99.9) then

c  estimate missing tempmn

c       tminxx=11.45+8.144*sin(1.078+xrad)             ! tmin on dry days
c       if(nsince.le.1)tminxx=13.47+5.949*sin(1.+xrad) ! tmin on wetdays
c       if(nsince.eq.2)ftmin=.993                      ! first day after rain
c       if(nsince.gt.2)ftmin=.925+.01321*nsince
c       if(nsince.le.1)ftmin=1.003+.005169*rain-.0001039*rain**2
c       tempmn=tminxx*ftmin                         ! estimate of tmin

c      end if

c       estimate wet and dry bulb when odd day missing
c
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
c
c          **** calculate soil heat flux (cal/cm2) ****
c
        NDATE=g%jdate+183               ! CONVERT TO NORTHERN HEMISPHERE
        IF(NDATE.GT.g%mdpy) NDATE=NDATE-g%mdpy
        DATE=real(NDATE)
        g%g=1.7+14.6*SIN(0.0172*(DATE-51.0))           ! TEMPLE,TEXAS
        IF(g%g.LT.0.0) g%g=0.0
c
c      call hfunc (i) ! calculate heat units or daydegrees
      CALL ozcot_hfunc
      g%stemp=(g%tempmx+g%tempmn)/2.0*g%asoil
c      call evap (i) ! calculate potential evaporation
      call ozcot_evap
      RETURN
      END


      SUBROUTINE ozcot_n_fertilise (APPLIED,availn,APPLIED_AVAIL)
c
c      simulates uptake of fertiliser nitrogen. assumes that there is an upper
c      limit to the amount of nitrogen a crop can take up and the rate of uptake
c      or recovery of fertiliser n decreases linearly from a maximum initial
c      value to zero when uptake limit is reached (basinski et al 1975, cot
c      gr rev). assume intial rate is 1.0 (100% recovery) and maximum uptake
c      is 240 kg/ha.
c
      real uptakn_max, rate_reducer, availnx, fraction
      real applied, applied_avail, availn
      integer n, nkg
       DATA UPTAKN_MAX/240./
c
       NKG = IFIX(APPLIED)             !  integer of kgs, index for DO loop
       RATE_REDUCER = 1./UPTAKN_MAX    !  recovery decreases with uptake
c
       AVAILNX   = availn            ! available N before application
       DO 1000 N=1,NKG
           FRACTION = 1.0-RATE_REDUCER*availn ! fraction of next kg available
           availn = availn + FRACTION
1000  continue
       APPLIED_AVAIL = availn-AVAILNX ! N applied now that will be available
c
       RETURN
       END


      SUBROUTINE ozcot_overload

c-------------------------------------------------------------------------------
c     simulates abscission or abortion of older fruit under extreme stress
c     ie when boll load exceeds carrying capacity.
c-------------------------------------------------------------------------------

      use OzcotModule
      real over_c, over_n, fload, capacity, excess, available
      real abort
      integer l, icat, m
c----- determine if overload called for c or n ---------------------------------

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

c----- count days fruit load exceeds capacity ---------------------------------

      IF(g%iday-g%last_iday.GT.1)g%idayco = 0 ! reset for break
      IF(FLOAD.GT.CAPACITY) g%idayco = g%idayco+1 ! count days FLOAD>g%carcap
      g%last_iday = g%iday                    ! g%last g%day counted
      IF(g%idayco.LT.3)RETURN                 ! buffered by reserves

c----- compute excess fruit and buffer effect ---------------------------------

      EXCESS = FLOAD-CAPACITY                 ! bolls in excess of Cc supply
      EXCESS = EXCESS*0.1                     ! damp effect for carbon stress

c----- loop through arrays to find available fruit ----------------------------

      DO 2 L=g%lfru(5),g%lfru(8)-1,-1           ! loop through categories 5, 6, 7
        IF(L.LT.1) GO TO 1                    ! no fruit to abort
        ICAT = 4
        IF(L.LE.g%lfru(5)) ICAT = 5
        IF(L.LE.g%lfru(6)) ICAT = 6
        IF(L.LE.g%lfru(7)) ICAT = 7
        IF(g%fruno(L).EQ.0.)GO TO 2                    ! no fruit
        AVAILABLE = g%fruno(L)                         ! fruit available to abort
        DO 10 M=1,6
            AVAILABLE = AVAILABLE-g%frmark(L,M)        ! less fruit marked
10      continue


        IF(ICAT.EQ.7.AND.g%fruwt(L)/g%fruno(L).LT.0.1)THEN ! fruit not grown yet ?
           g%frmark(L,6) = g%fruno(L)                  ! abort such fruit
           AVAILABLE = AVAILABLE-g%frmark(L,6)         ! adjust fruit available
        ENDIF

        IF(AVAILABLE.GT.0.)THEN
          AVAILABLE = AVAILABLE*0.1                    ! damp effect
          ABORT = AVAILABLE                            ! abort available fruit
          IF(ABORT.GT.EXCESS) ABORT=EXCESS             ! limit to requirement
          g%frmark(L,6) = g%frmark(L,6)+ABORT
          g%fmkcat(ICAT,6) = g%fmkcat(ICAT,6)+ABORT
          EXCESS = EXCESS-ABORT                        ! reduce excess no. bolls
          IF(EXCESS.LE.0.) GO TO 1                     ! excess depleted
        ENDIF
2     continue

1     continue

c-------------------------------------------------------------------------------

      RETURN
      END


        real FUNCTION ozcot_satvp(Tdeg)
c
        real Tdeg

        real tabs, tr,trlog, ts, tt, ewslog, ew

        TABS=Tdeg+273.16
        tr=373.16/TABS
        TRLOG=ALOG10(tr)
        tr=tr-1.
        TS=(10.**(11.344*(1.-TABS/373.16))-1.)/10.**7.
        TT=(10.**(-3.49149*tr)-1.)/10.**3.
        EWSLOG=ALOG10(1013.246)
        EW=-7.90298*tr+5.02808*TRLOG-1.3816*TS+8.1328*TT+EWSLOG
        ozcot_satvp=10.**EW
        RETURN
        END
c
      real FUNCTION ozcot_senlf(bload,alai,carcap_c,smi)
c
c     estimates leaf longevity. ranges between 833 dd & 1110 dd
c     reduced by water stress, boll load and self shading of
c     canopy when lai gt 3.
c
      real ozcot_stress
      real fb, fw, carcap_c, bload, alai, smi

        FB=1.
        IF(carcap_c.GT.0.)FB=1.-bload/carcap_c ! reduce by boll load
        IF(FB.GT.1.)FB=1.
        IF(FB.LT.0.)FB=0.
        FW=ozcot_stress(0.0,0.25,1.0,smi) ! effect of water stress
      ozcot_senlf=833.+277.*FB*FW
      RETURN
      END


        SUBROUTINE ozcot_sevap(RAINSI)
c
c       ****** calculate actual es (use eos,ul1,time since ul1) ******
c
      use OzcotModule
      real Td, esx, rainsi
c
        IF(g%sumes1.GE.g%ul1) GO TO 180
        IF(RAINSI.GE.g%sumes1) GO TO 120
        g%sumes1=g%sumes1-RAINSI
        GO TO 140
120     g%sumes1=0.
140     g%sumes1=g%sumes1+g%eos
        IF(g%sumes1.LT.0.0) g%sumes1=0.0
        IF(g%sumes1.GT.g%ul1) GO TO 160
        g%es=g%eos
        GO TO 260
160     g%es=g%eos-0.4*(g%sumes1-g%ul1)
        g%sumes2=0.6*(g%sumes1-g%ul1)
        Td=(g%sumes2/g%cona)**2
        GO TO 260
180     if(rainsi.lt.g%sumes2) go to 200
        RAINSI=RAINSI-g%sumes2
        g%sumes1=g%ul1-RAINSI
        g%sumes2=0.0
        Td=0.
        IF(RAINSI.GT.g%ul1) GO TO 120
        GO TO 140
200     td=td+1.
        g%es=g%cona*Td**0.5-g%sumes2
        IF(RAINSI.GT.0.) GO TO 220
        IF(g%es.GT.g%eos) g%es=g%eos
        GO TO 240
220     esx=0.8*rainsi
        IF(ESX.LE.g%es) ESX=g%es+RAINSI
        IF(ESX.GT.g%eos) ESX=g%eos
        g%es=ESX
240     g%sumes2=g%sumes2+g%es-rainsi
        Td=(g%sumes2/g%cona)**2
260     if(g%es.lt.0.) g%es=0.
        RETURN
        END


c     subroutine solwat (i)
      SUBROUTINE ozcot_solwat
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      calculates the soil water balance as a function of soil     c
c      layer depth.  modified from the model of ritchie (1972).    c
c                                                                  c
c      key variables:                                              c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      use OzcotModule
      real ozcot_watco
      real depth, rtul, swi
c      real t, rdef
      integer l
c
c      dimension stor(20) ,u(20)
c      data jir/1/ ! index for current element of igday
c      data initial/0/ ! flag for initial call

cpsc      do 99 ll=1,nlayr
cpsc 99       dlayr(ll)=dlayr(ll)/10.

c-------initialising -----------------------------------------------------------

c        if(i.eq.1) jir = 1            ! irrigation counter for ozcot2
        IF(g%isw.NE.1) THEN           ! replaces INITIAL.GT.1 for OZCOT2&4 May89
            g%isw = 1
c            t=0.
            g%smi=g%swlayr(1)/g%ullayr(1)
c           if(smi.lt.0.9 ) then
c               sumes1=g%ul1
c               sumes2=2.5-2.78*smi
c           else
c                sumes1=10.-smi*10.
c               sumes2=0.
c            endif
        ENDIF

c------ remove water from layers 1&2 during land preparation -------------------

c       jtest = jdate-(jdate/30)*30          ! test if 30th day
c       if(isow.eq.0 .and. jtest.eq.0) call cultivate (0.25,1.0)
c       if(jdate.eq.220) call cultivate (0.0,0.5)

c------ add irrigation water ---------------------------------------------------

c       rainsi=rain
c       if(jdate.eq.igday(jir))then
c         defirg=def
c         rainsi=rain+defirg
c         jir=jir+1
c       end if

c------ calculate runoff -------------------------------------------------------

c       sw=sw+rainsi
c       if(sw.le.ul) then
c           q=0.0
c       else
c           q=sw-ul
c        endif
cpsc         q=runoff
c        rainef=rainsi-q
c       call sevap(rainsi)

c------ calculate potential ep -------------------------------------------------
c-------light interception modified to give hedgerow effect with skip row - abh 5/11/96 ------
cpsc
        g%alai = g%alai*g%rs             ! lai in hedgerow

        IF(g%alai.GT.3.) THEN
            g%ep=g%eo-g%es
        ELSE IF(g%alai.GT.1.6)THEN
            g%ep=(0.08+0.3066*g%alai)*g%eo ! L.Mateos 1987, ABH 1988
        ELSE
            g%ep=(1.-EXP(-0.5186*g%alai))*g%eo ! L.Mateos 1987, ABH 1988
        ENDIF

        IF(g%alai.EQ.0.0) g%ep=0.0
        IF(g%ep.LT.0.) g%ep=0.
cpsc
        g%ep = g%ep/g%rs                 ! g%ep on ground area basis
        g%alai = g%alai/g%rs             ! restore LAI to ground area basis

c------ limit ep using watco(smi) stress factor --------------------------------

        g%et=g%es+g%ep
        IF(g%eo.LT.g%et) THEN
            g%et=g%eo
            g%ep=g%et-g%es
            IF(g%ep.LT.0.0)g%ep=0.0
        ENDIF
        g%ep=g%ep*ozcot_watco(g%smi,g%eo,0.4,0.)
        g%et=g%es+g%ep

        g%rrig(8) = g%rrig(8) + g%et ! accumulate g%et for output

c       call swbal(i,rainef)
        CALL ozcot_swbal

c------ calculate total sw before ep for sr: snbal ----------------------------

c       swwep=0.0
c       do 10 l=1,nlayr
c         swwep=swwep+swlayr(l)*dlayr(l)
c         sweplr(l)=swlayr(l)
c10      continue

c---- calculate soil water stress factor 'smi'  ---------------------------------

c       if(i.lt.isow.or.isow.eq.0) go to 500
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
        IF(g%dlayr_cm(1).GE.30.) GO TO 480
        SWI=
     *  (SWI*g%dlayr_cm(1)+(g%swlayr(2)/g%ullayr(2))*(30.-g%dlayr_cm(1))
     :  )/30.
480     g%smi=amax1(g%rtsw/rtul,swi)

cpc
        g%smi = g%rtsw/RTUL

c        if(rs.gt.1. .and. rtdep.lt.rtdepm) then  ! when rs > 1m & roots growing,
c            smi = 1-(1-smi)*(tr/rs+1-tr)*rs      ! adjust for lateral root
c            if(smi.lt.0.) smi = 0.
c        endif                                    ! growth, limited to 1m row

c------ calculate some quantities for writing only -----------------------------

c       rtsmi=rtsw/rtul
c
c       flim=ozcot_watco(smi,eo,0.3,0.)
cpc     rdef=rtul-rtsw
c       rperd=(rdef/rtul)*100.0

  500 CONTINUE

c------ calculate total soil water & deficit -----------------------------------

        g%sw=0.0
        DO 30 L=1,g%nlayr
            g%sw=g%sw+g%swlayr(L)*g%dlayr_cm(L)
            IF(L.LE.6)g%tswl(L)=(g%swlayr(L)+p%unul(L))*g%dlayr_cm(L)
cpsc
cpsc            swdep(l)=swlayr(l)*10.*dlayr_cm(l)+
cpsc     *                 (duldep(l)-ullayr(l)*10.*dlayr_cm(l))
cpsc

30      continue
        g%def=g%ul-g%sw

cpsc      do 199 ll=1,nlayr
cpsc 199       dlayr_cm(ll)=dlayr_cm(ll)*10.

        RETURN
        END


c      subroutine cultivate (c_lyr1,c_lyr2)

c     reduces soil water content of layers 1 and 2 in response to cultivation
c     to levels passed in c_lyr1 & c_lyr2. if these are 1.0, no reduction as
c     cultsw will be > swlayr

c      use OzcotModule

c      cultsw = ullayr(1)*c_lyr1          ! soil water content after cultivation
c      if(cultsw.lt.swlayr(1)) swlayr(1) = cultsw
c      cultsw = ullayr(2)*c_lyr2          ! soil water content after cultivation
c      if(cultsw.lt.swlayr(2)) swlayr(2) = cultsw
c      return
c      end


c      subroutine sowday (i,iend)
       SUBROUTINE ozcot_sowday

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      derives a sowing date if one is not given as an initial     c
c      condition.  sowing requires 3 consecutive days with soil    c
c      temperatures >= 15.0c and soil moisture sumes1 >= ul1       c
c      at sowing, root depth is assigned seed depth until          c
c      emergence.                                                  c
c                                                                  c
c      key variables:                                              c
c
c      isow = sowing date (as day of year); if input > 0 or after
c             selected in this s/r, s/r sowday not called;
c             if < 0 (-ve), this s/r will not look for a owing day
c             before -isow (i.e. the -ve value made +ve).
c      iwindow = time window for sowing when selected in this s/r,
c                starting with earliest day
c      tempre = mean temp day before yesterday
c      tempyd = mean temp yesterday
c      tmean  = mean temperature for last three days, when > 18
c               soil temp > 15 for three successive days - abh 8/8/89
c      yr1_last = flag to show season started last
c               year or this.
c      window = logical flag to show if in sowing window
c      span_nu_yr = logical flag to show if new year started in sowing window
c      lastyr = needed when sowing window spans new year
c               for number of days in previous year;
c               acts as a flag to show if sow_this_yr to be set to 0 or 1.
c      nosow  = flag to show if crop to be sown this year, mainly for
c               long fallowing with dry land cropping;
c               -1 for no crop, 1 for crop.
c      iswitch= toggle switch to change nosow.
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      use OzcotModule

c      logical window/.false./
c      logical warm/.false./, wet/.false./
c      logical traffic/.false./, profile/.false./
c      logical span_nu_yr
c      logical yr1_last
c      dimension npm(12)
c      data npm /0,31,59,90,120,151,181,212,243,273,304,334/
c      data tempyd/0.0/,tempre/0.0/
c      data initial/0/,lastyr/0/

c      if(isow.gt.0) return

c---------------- initial conditions set on first call -------------------------

c      if(initial.eq.0 .and. isdy.lt.0) then  ! 1st time sowday called?
c          initial = 1                        ! set flag
c          jstart = npm(ismo)-isdy            ! earliest day of year for sowing
c          if(jstart.gt.mdpy) jstart=jstart-mdpy ! into next year
c          jstop  = jstart+iwindow            ! latest day to sow (can be >365)
c         iswitch = 1                        ! crop every year
c         if(isyr.lt.0) iswitch = -1         ! crop alternate years
c          nosow = 1                          ! crop every yr or even yrs if alt
c          if(isyr.eq.-2) nosow = -1          ! crop in odd years
c          yr1_last = .true.                  ! season starts last year
c      end if

c-------------- reset on first call of new season ------------------------------

c      if(i.eq.1) then                        ! 1st day of season? if so...
c          nosow=nosow*iswitch                ! toggle nosow for fallow/crop
c          window = .false.                   ! reset sowing window flag
c          span_nu_yr = .false.               ! reset
c          lastyr = 0                         !  reset  for next season
c          tempre = 0.                        !  reset  for next season
c          tempyd = 0.                        !  reset  for next season
c      endif


c      if(nosow.eq.-1) then                   ! if no crop this season (fallow)..
c          ncount = ncount+1                  ! count days of fallow season
c          if(ncount.lt.365)return            ! end of fallow season?
c          iend=4                             ! yes, end of fallow season
c          ncount=0                           ! reset counter
c          return
c      end if

c---------- compute sowing conditions -----------------------------------------

      g%tmean3 = (g%tempav+g%tempyd+g%tempre)/3. ! mean temperature for g%last 3 days

      g%tempre = g%tempyd               ! update previous days temp for tomorrow
      g%tempyd = g%tempav               ! update yesterdays temp for tomorrow

c      if(tmean.ge.18.0) then            ! check soil temperature
c          warm = .true.                 ! soil warm enough to sow
c      else
c          warm = .false.                ! soil not warm enough to sow
c      endif

      g%s_bed_mi = g%swlayr(1)/g%ullayr(1) ! seed bed moisture index

      g%s_bed_sat = max(g%s_bed_sat,g%s_bed_mi)

c      if(s_bed_mi.ge.0.75) then         ! seed bed saturated?
c          wet = .true.                  ! seed bed has been wetted
c      else
c          if(s_bed_mi.lt.0.5) wet = .false. ! seed bed has dried out
c      endif

c      if(s_bed_mi.le.0.67) then         ! trafficable? bridge & muchow 1982
c          traffic = .true.              ! seed bed trafficable
c      else
c          traffic = .false.             ! seed bed not trafficable
c      endif

c      if(sw.ge.sow_sw) then             ! soil water content of profile
c          profile = .true.              ! sufficient water in profile
c      else                              ! 11cm is for 1m from fawcet 1977
c          profile = .false.             ! insufficient water in profile
c      endif

c------------- check if sowing window open--------------------------------------

c      if(.not. window) then                  ! sowing window yet?
c          if(jdate.eq.1.and.i.ne.1) yr1_last=.true. ! window starts in new year
c          if(jdate.eq.jstart) then           ! ....starts today?
c              window = .true.                ! if so, set flag
c          else                               ! if not, ....
c              return                         ! ...return to avoid start in
c          endif                              ! middle of window
c      endif

c      if(jdate.eq.1) then                    ! window spans new year?
c          span_nu_yr = .true.                ! set flag
c          lastyr = 365                                ! days in last year
c          if((((imyr-1)/4)*4).eq.(imyr-1)) lastyr=366 ! leap year
c      end if
c      jtry = jdate+lastyr                    ! allows window to span new year

c      if(jtry.ge.jstop) then                 ! passed the sowing window?
c          iend = 3                           ! if so, set flag
c          window = .false.
c          return                             ! no crop this season
c      end if

c------------- check if sowing conditions satisfied ----------------------------

c      if(warm .and. wet .and. traffic .and. profile) then ! all conditions met?
c          isow=i+1                       ! sow tomorrow
c          rrig(2) = sw                   ! soil water at sowing
c          window = .false.
c      endif

      RETURN
      END


        real FUNCTION ozcot_stress(LOW,HIGH,A,STRS)
        real HIGH,A,STRS
c
c       computes or adjusts a factor.
c       input is state variable strs with upper and lower limits, high,low.
c       output is between 0 and 1.
c       a =  1 gives factor a linear fn of ratio of strs to high - low
c       a gt 1 accentuates effect of strs on value
c       a lt 1 damps effect.
c
        REAL LOW
        ozcot_stress=(STRS-LOW)/(HIGH-LOW)
        IF(ozcot_stress.GT.1.)ozcot_stress=1.
        IF(ozcot_stress.LT.0.)ozcot_stress=0.
        ozcot_stress=(1.-(1.-ozcot_stress)**A)
        RETURN
        END
      real FUNCTION ozcot_survive(CAPACITY,bload)
      real CAPACITY,bload
      real a,b
c
c     estimates survival of fruit as function of boll load.
c
      ozcot_survive = 0.
      IF(CAPACITY.EQ.0.)RETURN
      A=1.0 ! intercept of survival function
      B=A/CAPACITY ! slope of survival function
      ozcot_survive=A-B*bload ! prortion surviving
      IF(ozcot_survive.LT.0.)ozcot_survive=0.
      IF(ozcot_survive.GT.1.)ozcot_survive=1.
      ozcot_survive = ozcot_survive*0.8  ! background, sub-threshold shedding
      RETURN
      END


c       subroutine swbal(i,rainef)
        SUBROUTINE ozcot_swbal

c
c   **** soil & plant water balance including rain and soil evaporation, ****
c   **** beginning with the top soil layer.                      ****
c
      use OzcotModule
      real ux, epcoef, uob, sum, stran, depth, tdepth, epd, swlr
      real dswmx
      integer l

c
c       percol=rainef
c       exes=es
        UX=0.0
        IF(g%smi.GE.0.5)EPCOEF=3.051            !  W*N
        IF(g%smi.LT.0.5)EPCOEF=2.436            !  82/83
        UOB=g%ep/(1.-EXP(-EPCOEF))
        SUM=0.0
        STRAN=0. ! sum of g%trans(L) down profile
        DEPTH=0.
        TDEPTH=0.
        EPD=0.0
c       w=0.
c
c***********************************************************************
c
          DO 10 L=1,g%nlayr
cpsc
cpsc          swlayr(l)=swdep(l)/10./dlayr_cm(l)
cpsc     *                 -(duldep(l)/10./dlayr_cm(l)-ullayr(l))
cpsc
          g%trans(L)=0.
            SWLR=g%swlayr(L)*g%dlayr_cm(L)
c           swmax=ullayr(l)*dlayr_cm(l)
c           if(percol.eq.0..and.exes.eq.0.)go to 2
c
c**** rain percolates layer 'l'
c
c       swlr=swlr+percol
c       percol=0.
c       if(swlr.le.swmax)go to 1
c       percol=swlr-swmax ! surplus percolates to next layer
c       swlr=swmax        ! this layer full
c
c**** extract es from layer 'l'
c
c 1     swlr=swlr-exes    ! extract es from this layer
c       exes=0.
c       if(swlr.ge.0.)go to 2
c       exes=-swlr        ! es carried down to next layer
c       swlr=0.
c
c**** extract ep from this layer
c
c2        continue
          DEPTH=DEPTH+g%dlayr_cm(L)         ! moved here 29/6/88
c         if(i.lt.iemrg) depth=0.0       !       ditto
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
          g%swlayr(L)=SWLR/g%dlayr_cm(L)
          STRAN=STRAN+g%trans(L)
          g%setlyr(L)=g%setlyr(L)+STRAN ! cumulative transp. thro season down profile
10      continue
        RETURN
        END

c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      water stress function for root growth.                      c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      real FUNCTION ozcot_watco(smi,eo,X3,X1)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      water stress function for root growth.                      c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      real smi,eo,X3,X1
      real x2, y0, slope
      X2=X3*eo/0.8
      IF(X2.GT.1.0) X2=1.0
      IF(smi.LT.X2) GO TO 20
      ozcot_watco=1.0
      GO TO 30
c
   20 CONTINUE
      SLOPE=1.0/(X2-X1)
      Y0=1.0-SLOPE*X2
      ozcot_watco=SLOPE*smi+Y0
      IF(ozcot_watco.LT.0.0) ozcot_watco=0.0
c
   30   CONTINUE

        RETURN
        END


c      subroutine yield(nszn,iend)
      SUBROUTINE ozcot_yield

c     estimates yield and gross margin at end of season

      use OzcotModule
      real bollsc

c     calculate yield **********************************************************

      g%alint = g%openwt*10.*g%pclint    ! g%g sc/m to kg lint/ha
cpsc      alint = alint/rs                   ! adjust for row spacing 2/5/90

c      plntz = uptakn                     ! nitrogen uptake
cpc   bollsc = 0.
cpc   if(openz.gt.0.) bollsc = openwt/openz ! g sc/boll
      g%rrig(7) = g%rrig(3) + g%rrig(4)  ! commulative g%rain pre + post boll

      call ozcot_residues                      ! to calculate stem and root residues
c     calculate gross margin ***************************************************

c     currently redundant!!!!

c      cot_price = 2.                     ! $ per kg
c      wat_cost = 12.                     ! $ per ml pumped
c      spray_cost =2.                     ! $ day of protection

c      cot_price = 2.                      ! cotton price $ per kg
c      gro_cost = 1200.                    ! growing costs - irrigated
c      gro_cost = 450.                     ! growing costs - rain grown
c      wat_cost  = 12.                     ! $ per ml (pumping & carges)
c      spray_cost = 2.                     ! $ per day of protection
c      cot_inc = alint*cot_price           ! cotton income
c      wat_exp = rrig(2)/10.*wat_cost      ! water expenditure
c      spray_save = (150-ilaiz)*spray_cost ! saving of spray cost
c      gross_marg = cot_inc - gro_cost - wat_exp -  spray_save ! gross margin

c     sowing date for output ***************************************************

c      jsow = isow+imet-1                  ! sowing date in j time
c      mdpy_prev = 365
c      if((imyr-1)/4*4 .eq. imyr_1) mdpy_prev = 366
c      if(jsow.gt.mdpy_prev) jsow = jsow - mdpy_prev

c     this section for when crop not sown **************************************

c      if(iend.ge.3) then
c          jsow = 0                        ! crop not sown, window passed or fallow
c          gross_marg = 0.0                ! gross margin nil
c      end if

c     this section does stuff needed with 1st call of yield ********************


c      if (imyr.gt.1800) then              ! year in 10s or 100s?
c          imyr = imyr-1800                ! change year to 10s
c          if(imyr.ge.100) imyr=imyr-100   ! for 1900s
c      endif                               ! above done for output

c      if(nszn.eq.1) then                     ! first season

c          open(3,file='yield.out',status='unknown')
c          write(3,7772) title

c          if(mode.le.1) then                  ! validation or calibration
c              write(3,600)
c          else                                ! simulation for strategy etc
c              if(defirr(2).gt.0.) then        ! irrigated crop
c                  write(3,700)
c              else                            ! rain-fed crop
c                  write(3,800)
c              endif
c          endif

c          if(jdate.lt.244) then               ! last year was year of sowing
c              iyr1 = imyr-1                   ! year sown in first season
c              iyr2 = imyr                     ! year harvested first season
c          else                                ! this year was year of sowing,
c              iyr1 = imyr                     ! year of sowing
c              iyr2 = imyr+1                   ! year of harvest
c          endif

c      else                                    ! not first season

c              iyr1 = iyr1+1                   ! year of sowing of this season
c              iyr2 = iyr2+1                   ! year of harvest of this season


c      endif

c      if(iyr1.eq.100) iyr1 = 0                 ! new century
c      if(iyr2.eq.100) iyr2 = 0                 ! new century

c    fallow soil water *********************************************************


c      if(jsow.gt.0) then    ! crop sown; deal with fallow sw gain
c          gain = rrig(2)-rrig(6) ! gain in sw during fallow
c          fraction = 0.0    ! fraction of rainfall conserved by fallow
c          if(rrig(5).ne.0.) fraction = gain/rrig(5)
c          write(4,999) iyr1,iyr2,jsow,rrig(6),rrig(2),gain,rrig(5),fraction
c999       format(3i4,5f8.2) ! yrs, sow d, initial sw, final sw, gain, rain, fraction
c          rrig(5) = 0.0     ! reset fallow rain. do not reset in reset!
c          rrig(6) = sw      ! initial sw for next fallow. do not reset in reset!
c      endif

c     write output *************************************************************

c      if(mode.le.1) then                  ! validation or calibration
c          write(3,7770) iyr1,iyr2,jsow,openz,alint,bollsc,
c     *    alaiz,plntz,ilaiz,sqzx,isqzx
c          write(2,7770) iyr1,iyr2,jsow,openz,alint,bollsc,
c     *    alaiz,plntz,ilaiz,sqzx,isqzx
c      else                                ! simulation for strategy etc
c          if(defirr(2).gt.0.) then        ! irrigated crop
c              write(3,7771) iyr1,iyr2,jsow,openz,alint,bollsc,
c     *        alaiz,plntz,ilaiz,ifix(rrig(1)),rrig(2),rrig(7),rrig(8)
c     *        alaiz,plntz,ilaiz,ifix(rrig(1)),rrig(2),rrig(7),def_last ! norn d
c              write(2,7771) iyr1,iyr2,jsow,openz,alint,bollsc,
c     *        alaiz,plntz,ilaiz,ifix(rrig(1)),rrig(2),rrig(7),rrig(8)
c     *        alaiz,plntz,ilaiz,ifix(rrig(1)),rrig(2),rrig(7),def_last ! norn d
c          else                            ! rain-fed crop
c              write(3,7771) iyr1,iyr2,jsow,openz,alint,bollsc,
c     *        alaiz,plntz,ilaiz,ifix(rrig(1)),rrig(2),rrig(3),rrig(4)
c              write(2,7771) iyr1,iyr2,jsow,openz,alint,bollsc,
c     *        alaiz,plntz,ilaiz,ifix(rrig(1)),rrig(2),rrig(3),rrig(4)
c          endif
c      endif

      RETURN
c
c600   format(' year sown  bolls/m  lint sc/boll max_lai  n_uptk day
c     *   sqz day')
c700   format(' year sown  bolls/m  lint sc/boll max_lai  n_uptk day
c     * no  water  rain  cum_et')
c     * no  water  rain def_l')                                    ! for norm d
c800   format(' year sown  bolls/m  lint sc/boll max_lai  n_uptk day
c     * no  water rain1  rain2')
c7770  format(x,2i2,i4,f8.1,f8.0,2f8.2,f8.0,i4,f6.1,i4)
c7771  format(x,2i2,i4,f8.1,f8.0,2f8.2,f8.0,i4,i3,3f7.1)
c7772  format(15a4)

      END


      SUBROUTINE ozcot_plant_n
c     call from pltgrw before ozcot_cropn
c     calculates nitrogen content of dry matter increments and sums them
c     adjusts n increments as soil n supply diminishes
c     variable ddw_leaf etc from s/r dry_matter
c              supply_n from system
c               dn_plant = daily increment of plant n to system

      use OzcotModule
      real ozcot_stress
      real supply_n
      real conc_l, conc_s, conc_r, conc_b
      real dn_leaf, dn_stem, dn_root, dn_boll
      real sup_dem, adjust
      data supply_n/2.0/

      DATA CONC_L/0.04/, CONC_S/0.02/, CONC_R/0.02/, CONC_B/0.015/

c      calculate daily increment for components of dry matter

      dN_LEAF = g%ddw_leaf * CONC_L ! leaf nitrogen
      dN_STEM = g%ddw_stem * CONC_S ! stem nitrogen
      dN_ROOT = g%ddw_root * CONC_R ! root nitrogen
      dN_BOLL = g%ddw_boll * CONC_B ! boll nitrogen

      g%dn_plant = dN_LEAF + dN_STEM + dN_ROOT + dN_BOLL ! plant N increment

c      adjust uptake when soil supply limiting

      if (g%dn_plant.gt.0.) then
        SUP_DEM = SUPPLY_N/g%dn_plant ! supply/demand ratio
      else
        SUP_DEM = 0.
      endif
      ADJUST = ozcot_stress(0.,5.0,1.0,SUP_DEM) ! factor to adjust
      IF(ADJUST.LT.1.0) THEN
          dN_LEAF = dN_LEAF * ADJUST   ! leaf nitrogen adjusted
          dN_STEM = dN_STEM * ADJUST   ! stem nitrogen adjusted
          dN_ROOT = dN_ROOT * ADJUST   ! root nitrogen adjusted
          dN_BOLL = dN_BOLL * ADJUST   ! boll nitrogen adjusted
          g%dn_plant = dN_LEAF + dN_STEM + dN_ROOT + dN_BOLL ! plant N increment
      ENDIF

      g%total_n =g%total_n + g%dn_plant ! accumulate uptake for season

c     compare accumulated uptake with projected uptake for season
c     if accumulated exceeds projected, assume requirements met by remobilisation
c     and set this day's increments to zero

      IF(g%total_n.GE.g%uptakn/10.) THEN
          g%total_n =g%total_n - g%dn_plant ! adjust uptake for season
          dN_LEAF = 0.0                ! leaf nitrogen adjusted
          dN_STEM = 0.0                ! stem nitrogen adjusted
          dN_ROOT = 0.0                ! root nitrogen adjusted
          dN_BOLL = 0.0                ! boll nitrogen adjusted
          g%dn_plant = 0.0             ! plant N increment
      ENDIF


c      write(4,222) iday,supply_n,dn_plant,total_n,uptakn,dw_total
c222   format(i5,5f8.3)


      RETURN
      END

      SUBROUTINE ozcot_residues

c      called from s/r yield to calculate stem and root residues

      use OzcotModule
      real conc_res

      DATA CONC_RES/0.005/           ! N concentration of residues

      g%stem_res = g%dw_stem         ! stem residues dry matter
      g%stem_res_n = g%dw_stem*CONC_RES ! N content of stem residues
      g%root_res = g%dw_root         ! root residues dry matter
      g%root_res_n = g%dw_root*CONC_RES ! N content of root residues

      RETURN
      END



c     subroutine dry_matter (i)
      SUBROUTINE ozcot_dryxmatter

c     this subroutine is for ozcot6 (version 6).

c     first demand for assimilate is estimated for leaf, stem, root and boll.
c     leaf demand is determined from potential increase in area. water stress
c     may reduce actual area increase, thus giving thicker leaves. inadequate
c     assimilate supply may reduce actal weight increase giving thinner leaves.
c     upper and lower limits are set by leaf weight:area ratio. if the upper
c     limit is exceeded, weight increase is reduced. if lower limit is not
c     reached, area increase is reduced. stem and root demand are determined
c     from potential leaf demand using allometric relationships.

c     calls s/r assimilation to compute assimilate production for the day.
c     the supply available to meet demand is obtained from sum of the days
c     assimilate production and any reserves.

c     supply and demand are then compared in order to partition dry matter
c     increase between leaf, stem, boll and root.
c     if supply exceeds demand, up to two days supply can be stored as reserves.
c     if demand exceeds supply, distribution is simulated on the basis
c     of proximity to the source of supply. leaf, stem and fruit are assumed to
c     be equidistant from the source. if supply exceeds the sum of leaf, stem
c     and fruit demand, their needs are met in full and the balance goes to
c     root. if the sum of leaf, stem and fruit demand exceeds supply,
c     their needs are met in proportion to the supply/demand ratio and the
c     root receives none. the root supply:demand ratio or a decline in root
c     growth provide feedback to reduce increase in root depth in s/r pltgrw.

c     local variables:
c       assimilate new dry matter passed daily from s/r assimilation
c       ddw_boll   day's increase in boll dry weight
c       ddw_leaf   day's increase in leaf dry weight
c       ddw_stem   day's increase in stem dry weight
c       ddw_root   day's increase in root dry weight
c       ddw_root_max   maximum value of increase in root dry weight
c       demand     demand for assimilate for potential day's growth
c       fnstrs2    n stress for boll growth - on/off
c       fwstrs     water stress for boll growth
c       sd_ratio   supply:demand ratio for leaf, stem and boll growth
c       sd_root    supply:demand ratio for root growth
c       strsbl     stress factor for boll growth, minimum of n and water
c       supply     day's supply of assimilate available for potential growth
c       wt_area    leaf weight:area ratio

      use OzcotModule
      implicit none
      include 'data.pub'

      real ozcot_stress
      real wt_area, fwstrs, fnstrs2
      real strsbl, assimilate, supply, demand
      real sd_ratio, sd_root, ddw_root_max
c------------------------------------------------------------------------------
c     initialise leaf, stem and root dry matter at start of new crop
c------------------------------------------------------------------------------

      IF(g%dw_leaf.EQ.0.) THEN              ! leaf weight initialise to zero?
          g%dw_leaf = g%embryo*g%f_leaf*g%ppm ! initial leaf dry weight per m
          g%dw_stem = g%embryo*g%f_stem*g%ppm ! initial stem dry weight per m
          g%dw_root = g%embryo*g%f_root*g%ppm ! initial root dry weight per m
      ENDIF

c------------------------------------------------------------------------------
c     calculate demand (potential growth) for leaf, stem and root
c------------------------------------------------------------------------------

      g%ddw_leaf = g%dlai_pot*g%specific_lw               ! leaf demand
      g%ddw_stem = divide (g%a_stem_leaf*g%ddw_leaf*g%dw_stem
     :                  ,g%dw_leaf, 0.0)                  ! ditto for stem
      g%ddw_root = divide (g%a_root_leaf*g%ddw_leaf*g%dw_root
     :                  ,g%dw_leaf, 0.0)                  ! ditto for root

c------------------------------------------------------------------------------
c     feed back of leaf weight/area ratio
c------------------------------------------------------------------------------

      IF(g%dlai(g%iday).GT.0.) THEN                       ! leaf growth today?
          WT_AREA = divide (g%ddw_leaf, g%dlai(g%iday), 0.0) ! leaf weight/are ratio
          IF(WT_AREA.GT.g%wt_area_max) THEN               ! too thick
              g%ddw_leaf = g%dlai(g%iday)*g%wt_area_max   ! reduce weight
c          else if(wt_area.lt.wt_area_min) then            ! too thin
c              dlai(iday) = ddw_leaf/wt_area_min           ! reduce area
          ENDIF
      ENDIF

c------------------------------------------------------------------------------
c     calculate demand for bolls
c------------------------------------------------------------------------------

c      if(isq.gt.0 .and. i.ge.isq+2) then        ! fruit called yet?
      IF(g%isq.GT.0 .AND. g%das.GE.g%isq+2) THEN  ! FRUIT called yet?
          FWSTRS = ozcot_stress(0.0,0.5,4.0,g%smi)    ! water stress on bolls
          FNSTRS2 = 1.                          ! N stress for bolls off
          IF(g%fnstrs.EQ.0.) FNSTRS2 = 0.       ! N stress for bolls on
          STRSBL = AMIN1(FWSTRS,FNSTRS2)        ! minimum of water or N stress
          g%bollgr = g%scboll(g%ivar)*g%bper*g%fburr ! boll gr rate this g%day
          g%bollgr = g%bollgr*STRSBL            ! adjust for stress
          IF(g%bollgr.LT.0.) g%bollgr = 0.
          g%ddw_boll = g%bollz*g%bollgr         ! boll demand - potential growth

      ENDIF

c------------------------------------------------------------------------------
c   determine supply of assimilate
c------------------------------------------------------------------------------

c      call assimilation(assimilate,i)                  ! day's assimilate
      CALL ozcot_assimilation(ASSIMILATE)                  ! g%day's assimilate
      SUPPLY = ASSIMILATE+g%reserve                    ! compute supply
      g%reserve = 0.                                   ! g%reserve used

c------------------------------------------------------------------------------
c   compare total demand with supply to partition assimilate
c------------------------------------------------------------------------------

      DEMAND = g%ddw_leaf+g%ddw_boll+g%ddw_stem+g%ddw_root ! compute total demand

      IF(SUPPLY.GE.DEMAND) THEN        ! demand met, potential growth achieved
         g%reserve = g%reserve+SUPPLY-DEMAND           ! excess becomes g%reserve
         IF(g%reserve.GT.g%res_cap) g%reserve = g%res_cap ! limit to g%reserve
         SD_RATIO = 1.0                ! supply:demand ratio for leaf,stem,boll
         SD_ROOT = 1.0                 ! supply:demand for root
      ELSE                             ! demand not met, reduce potential growth
         DEMAND = DEMAND-g%ddw_root    ! demand for leaf, stem and fruit
         IF(SUPPLY.GE.DEMAND) THEN     ! their potential growth achieved
             SD_RATIO = 1.0            ! supply:demand ratio for leaf,stem,boll
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

c------------------------------------------------------------------------------
c     grow crop by updating dry weights for leaf, stem, bolls and roots
c------------------------------------------------------------------------------

      g%dw_leaf = g%dw_leaf+g%ddw_leaf                 ! total leaf dry weight
      g%dw_stem = g%dw_stem+g%ddw_stem                 ! total stem dry weight
      g%dw_boll = g%dw_boll+g%ddw_boll                 ! total boll dry weight
      g%dw_root = g%dw_root+g%ddw_root                 ! total root dry weight
      g%dw_total = g%dw_leaf+g%dw_stem+g%dw_boll+g%dw_root ! total dry weight

      g%ddw_l(g%iday) = g%ddw_leaf                     ! this g%day's leaf dry wt
c      alai = alai+dlai(iday)                           ! update lai with increase

c------------------------------------------------------------------------------
c     feed back from root grow to root depth
c------------------------------------------------------------------------------

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
      IF(g%root_feedback.GT.0.) g%root_feedback=g%root_feedback**0.333 ! cubic to linear

c------------------------------------------------------------------------------

      RETURN
      END



c      subroutine assimilation (assimilate,i)
      SUBROUTINE ozcot_assimilation (ASSIMILATE)

c     assimilate production for the day is estimated from intercepted
c     photosynthetically active radiation (montieth 1977).
c     adjusted for effects of water stress using data of turner et al 1986
c     collected at narrabri ars.
c     effect of water logging based on observation of hearn and constable 1984
c     on yield and hodgson on photosynthesis.
c     effect of temperature, see constables thesis 1981, direct effect on
c     photosynthesis and respiration, using angus and wilson's 1976 expression
c     for a scalar and constables value for base and optimum.
c     carrying capacity, carcap,estimated. it is maximum number of bolls the
c     crop can carry, and is therefore the boll load that causes 100% shedding.
c     local variables:
c       assim_mean      3 day mean of assimilate supply
c       assim_1         previous day's assimilate supply
c       assim_2         day before previous day's supply
c       rad_mj          radiation in mega joules
c       rel_p           relative photosynthesis, scalar for water stress
c       par_int         intercepted photosynthetically active radiation
c       tf              temperature scalar for dry matter production

      use OzcotModule
      real assim_1, assim_2, rad_mj, alight, par_int
      real assimilate, rel_p, tf
c------------------------------------------------------------------------------
c     initialise for new season
c------------------------------------------------------------------------------

cpc   if(iday.eq.1) then
cpc       assim_1 = 0.0                    ! previous day's assimilation
cpc       assim_2 = 0.0                    ! day before that
cpsc  endif

c------------------------------------------------------------------------------
c     photosynthesis
c------------------------------------------------------------------------------

      RAD_MJ = g%rad/23.87                 ! langleys to Mjoules
c      par_int = rad_mj*(1.-tr)*0.5         ! intercepted par

      ALIGHT = 1.-EXP(-1.*g%alai)          ! light interception, Beer's law.
      PAR_INT = RAD_MJ*(ALIGHT)*0.5        ! intercepted PAR, ex old OZCOT

      ASSIMILATE = PAR_INT*g%e_par         ! assimilation
      IF(ASSIMILATE*2..GT.g%res_cap) THEN
          g%res_cap = ASSIMILATE*2.        ! capacity to store reserves
      ENDIF

c------------------------------------------------------------------------------
c     effect of water stress on assimilation
c------------------------------------------------------------------------------

      REL_P =.25+.864*g%smi                ! effect of water stress on Pp
      IF(REL_P.GT.1.) REL_P = 1.           ! (TURNER g%et al 1986).
      ASSIMILATE = ASSIMILATE*REL_P        ! adjust for water stress

c------------------------------------------------------------------------------
c     effect of temperature on dry matter production
c------------------------------------------------------------------------------

      TF = (g%tempav-g%t_base)/(g%t_opt-g%t_base) ! temperature scalar after
      TF = 2*TF-TF**2                      ! Angus & Wilson 1976, Constable 1981
      ASSIMILATE = ASSIMILATE*TF           ! adjust assimilate for temp

c------------------------------------------------------------------------------
c     effect of waterlogging on photosynthesis - hearn & constable 1984 eqn 4
c------------------------------------------------------------------------------

c      if(def.lt.2.5) then                 ! waterlogged?
      IF(g%sw/g%ul.GT.0.87) THEN           ! waterlogged?
          ASSIMILATE = ASSIMILATE*0.2      ! adjust for water logging - old OZCOT
      ENDIF

c      if(isq.eq.0 .or. i.lt.isq+2) return  ! do not proceed to carrying capacity
      IF(g%isq.EQ.0 .OR. g%das.LT.g%isq+2) RETURN ! do not proceed to carrying capacity

c------------------------------------------------------------------------------
c     carrying capacity - photosynthetic capacity divided by boll growth rate
c------------------------------------------------------------------------------

c      disable rest of subroutine for use with ozcot2 in apsru system

c      if(assim_1.gt.0.0 .and.assim_1.gt.0.0) then      ! not 1st or 2nd crop day
c          assim_mean = (assimilate+assim_1+assim_2)/3. ! 3 day running mean
c      else                                             ! is 1st or 2nd crop day
c          assim_mean = assimilate
c      endif
                                                   ! use mean to buffer g%carcap
c      assim_2 = assim_1                            ! 3rd day's for tomorrow
c      assim_1 = assimilate                         ! 2nd day's for tomorrow

c      if(bollgr.gt.0.0) then
c          carcap_c = assim_mean/bollgr             ! carrying capacity
c      else
c          carcap_c = 0.0                           ! zero when bolls not growing
c      endif

c      if(carcap_c.lt.0.) carcap_c = 0.             ! trap
c      carcap  = carcap_c
c      cutout = carcap*fcutout(ivar)                ! boll load for cutout

c      cutout = carcap*1.00                         ! sensitivity to fcutout

c------------------------------------------------------------------------------

      RETURN
      END
      