*     ===========================================================
      character*(*) function ozcot_version ()
*     ===========================================================
      implicit none

*+  Purpose
*       return version number of ozcot module

*+  Changes
*      psc - 9/08/93
*      07/07/94 - jngh changed names from ozcot_3 to ozcot
*                      prefixed all subroutines and functions with ozcot
*      20/7/94 - jngh put divide function in dryxmatter s/r

*+  Constant Values
      character  version_number*(*)    ! version number of module
      parameter (version_number = 'V3.3  30/04/98' )

*- Implementation Section ----------------------------------
 
      ozcot_version = version_number
 
      return
      end



* ====================================================================
       subroutine APSIM_ozcot (action, data_string)
* ====================================================================
       implicit none
       dll_export apsim_ozcot
       include 'ozcot.inc'            ! ozcot common block
       include 'const.inc'             ! Global constant definitions
       include 'engine.pub'                        
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

*+  Calls
      character  ozcot_version*15     ! function

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'apsim_ozcot')

*- Implementation Section ----------------------------------
      call push_routine(myname)
 
      if (Action.eq.MES_Presence) then
         write(*, *) 'Module_name = ', Module_name
     :              // ', Version : ' // ozcot_version()
 
      else if (Action.eq.mes_init) then
         call ozcot_zero_variables ()
         call ozcot_Init ()
 
      else if (Action .eq. mes_prepare) then
         call ozcot_prepare ()
 
      else if (Action.eq.MES_Process) then
         if (Zero_variables) then
            call ozcot_zero_variables()
            call ozcot_init()
 
         else
            ! No need to zero variables.
         endif
 
         call ozcot_get_other_variables ()
         call ozcot_Process ()
         call ozcot_set_other_variables ()
 
      else if (Action .eq. MES_Post) then
         call ozcot_post ()
 
      else if (Action.eq.MES_Get_variable) then
         call ozcot_Send_my_variable (data_string)
 
      else if (Action .eq. MES_Set_variable) then
         call ozcot_set_my_variable (data_string)
 
      else if (Action .eq. 'sow' .or. action .eq. 'harvest') then
         call ozcot_manager (Action, data_string)
 
      else if (Action .eq. MES_End_run) then
         call ozcot_end_run ()
 
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
      implicit none
      include 'write.pub'                         
      include 'error.pub'                         

*+  Purpose
*      Initialise ozcot module

*+  Changes
*      psc - 9/08/93
*      250996 jngh removed unused includes

*+  Calls
       character ozcot_version*15   ! function

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_init')

*+  Local Variables
       character Event_string*40       ! String to output

*- Implementation Section ----------------------------------
      call push_routine(myname)
      ! Notify system that we have initialised
 
      Event_string = 'Initialising, Version : ' // ozcot_version()
      call report_event (Event_string)
 
      ! Get all parameters from parameter file
 
      call ozcot_read_param ()
 
cpsc      call Init()                      ! now called from o_zero_variables
      call ozcot_initial()
      call ozcot_get_other_variables ()
      call pop_routine(myname)
      return
      end



* ====================================================================
       subroutine ozcot_read_param ()
* ====================================================================
      implicit none
       include 'ozcot.inc'            ! ozcot model common block
       include 'const.inc'             ! Constant definitions
      include 'read.pub'                          
      include 'error.pub'                         

*+  Purpose
*      Read in all parameters from parameter file.

*+  Changes
*      psc - 09/08/93 first go
*      psc - 30/03/94 specified properly
*      DPH - 7/7/94  Removed free format internal read to title.  Line now
*                    reads title = param_string
*      psc - 15/4/98 Add ll, remove title, asoil from read
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
!         ! Read in title from parameter file
!      call read_char_array (section_name
!     :                     , 'title', 15, '()'
!     :                     , title, numvals)
 
!         ! Read in soil temperature factor from parameter file
!      call read_real_var (section_name
!     :                    , 'asoil', '()'
!     :                     , asoil, numvals
!     :                     , 0.0, 10000.0)
 
      call read_real_array_optional (section_name
     :                     , 'll', max_layers, '(mm/mm)'
     :                     , unul, g_num_ll_vals
     :                     , 0.0, 1.0)
 
      if (g_num_ll_vals.ne.0) then
         ! LL found
      else
         ! LL not found
         call warning_error (err_user
     :         , ' Cotton LL not found. Using Soilwat LL15 instead.' )
      endif
 
      call pop_routine(myname)
      return
      end



* ====================================================================
       subroutine ozcot_zero_variables ()
* ====================================================================
      implicit none
       include 'ozcot.inc'            ! ozcot common block
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
      das = 0
      delay = 0.0
      idayx = 0
      g_num_ll_vals = 0
      Crop_in = .false.
      Zero_variables = .false.
      call pop_routine(myname)
      return
      end



* ====================================================================
       subroutine ozcot_manager (Event_action, event_data)
* ====================================================================
      implicit none
      include 'const.inc'              ! global_active
      include 'ozcot.inc'              ! ozcot common block
      include 'engine.pub'                        
      include 'intrface.pub'                      
      include 'write.pub'                         
      include 'error.pub'                         

*+  Sub-Program Arguments
      character Event_action*(*)       ! (INPUT) Action to be performed
      character Event_data*(*)         ! (INPUT) Data sent with event

*+  Purpose
*     The manager has sent an event to this module.  Process it.

*+  Notes
*     Event_action is the action specified in the management parameter
*     file.  e.g. 'sow'

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
 
         Crop_in = .true.
 
         ! Report the event to the rest of the system
 
         call report_event (Event_action)
 
         call ozcot_sow (event_data)
 
      else if (Event_action .eq. 'harvest') then
 
         ! Report the event to the rest of the system
 
         call report_event (Event_action)
 
         res_dm = (dw_total - openwt / rs ) * 10.
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
 
         call message_send_immediate (
     :                              unknown_module
     :                            , 'add_residue'
     :                            , Blank
     :                            )
 
         call Delete_postbox ()
 
         Crop_in = .false.
         Zero_variables = .true.
 
      else
         ! Don't know about this event !!!
 
      endif
 
      ! Report the event to the rest of the system
 
      call Report_event ( Event_action)
      call pop_routine(myname)
      return
      end



*     ===========================================================
      subroutine ozcot_sow (myrecd)
*     ===========================================================
      implicit none
      include 'const.inc'              ! lu_summary_file, blank
      include 'ozcot.inc'              ! ozcot
      include 'write.pub'                         
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
 
         read (myrecd,*) ivar,sdepth,rs,ppm
 
         isow = jdate
           RTDEP=SDEPTH
         PPM = PPM/RS        !  adjust for non standard rows incl skip
           PP= PPM*RS
           PS=(1.0/RS)/PP
           S=PS/RS
         RRIG(2) = SW                   ! soil water at sowing
         iend = 1
 
             ! report
 
         write (string, '(a)')
     :                  ' sowing  depth plants row sp'
         call write_string (lu_summary_file, string)
 
         write (string, '(a)')
     :                  ' day no   mm     m^2    m  '
         call write_string (lu_summary_file, string)
 
         write (string, '(i7, 3f7.1, 1x, a10)')
     :                   isow, sdepth, pp, rs
         call write_string (lu_summary_file, string)
 
         call write_string (lu_summary_file, blank)
 
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
      implicit none
       include 'const.inc'             ! Constant definitions
       include 'ozcot.inc'             ! ozcot common block
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
*      JNGH - 12/7/94 Changed dlayer in cm to dlayr_cm
*      psc - commented out read of LL_DEP

*+  Calls
                                       ! function

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_get_other_variables')

*+  Local Variables
      logical N_in_system              ! Is there any N in system ?
      integer layer                    ! layer number
      real    no3(Max_layers)          ! soil nitrate kg/ha in layer
      integer numvals

*- Implementation Section ----------------------------------
      call push_routine(myname)
 
      call get_integer_var (unknown_module, 'day', '()'
     :                      , jdate, numvals
     :                      , 1, 366)
 
      call get_integer_var (unknown_module, 'year', '()'
     :                      , imyr, numvals
     :                      , 1800, 2000)
 
      call get_real_var (unknown_module, 'maxt', '(oC)'
     :                                  , tempmx, numvals
     :                                  , -100.0, 100.0)
 
      call get_real_var (unknown_module, 'mint', '(oC)'
     :                                  , tempmn, numvals
     :                                  , -100.0, 100.0)
 
 
      tempav = (tempmx + tempmn)/2.
      TMEAN3 = (TEMPAV+TEMPYD+TEMPRE)/3.  ! mean temperature for last 3 days
      TEMPRE = TEMPYD                     ! update previous days temp for tomorrow
      TEMPYD = TEMPAV                     ! update yesterdays temp for tomorrow
 
      call get_real_var (unknown_module, 'radn', '(Mj/m^2)'
     :                                  , solrad, numvals
     :                                  , 0.0, 1000.0)
 
      solrad = solrad / 0.04186                ! convert to langleys
 
      call get_real_var (unknown_module, 'rain', '(mm)'
     :                                  , rain, numvals
     :                                  , 0.0, 1000.0)
 
      rain = rain /10.                         ! convert to cm
 
      ! Get depths of each layer
 
                                ! get depth of each soil water layer
      call get_real_array (unknown_module, 'dlayer', max_layers
     :                                    , '(mm)'
     :                                    , dlayr, nlayr
     :                                    , 0.0, 1000.0)
 
      ! Get moist bulk density
      call get_real_array (unknown_module, 'bd', max_layers
     :                                    , '(mm)'
     :                                    , bulkd, numvals
     :                                    , 0.0, 1000.0)
 
      if (g_num_ll_vals .eq.0) then
         ! Get unavailable sw - use ll15 because crop ll is unavailable
         call get_real_array (unknown_module, 'll15', max_layers
     :                                    , '(mm/mm)'
     :                                    , unul, numvals
     :                                    , 0.0, 1.0)
      else
         ! ll had been read at init
      endif
 
      ! Get upper limit of available sw
      call get_real_array (unknown_module, 'dul_dep', max_layers
     :                                    , '(mm)'
     :                                    , ullayr, numvals
     :                                    , 0.0, 1000.0)
 
      ! Get upper limit of available sw
      call get_real_array (unknown_module, 'sat_dep', max_layers
     :                                    , '(mm)'
     :                                    , stlayr, numvals
     :                                    , 0.0, 1000.0)
 
      ! Convert field capacity relative to wilting point.
      ! convert units to cm and cm/cm
 
      RTDEPM=0.0
      UL=0.0
cpc
      sat = 0.0
      WPWC=0.0
 
C      ULLAYR(J) = ULLAYR(J)*2.      !   simulate skip row
C      UNUL(J)   = UNUL(J)*2.        !          ditto
 
      do 10 Layer = 1, nlayr
         unul(layer) = unul(layer) * dlayr(layer)
         ullayr(Layer) = ullayr(Layer) - unul(Layer)
         ullayr(layer) = ullayr(layer) / dlayr(layer)
         stlayr(Layer) = stlayr(Layer) - unul(Layer)
         stlayr(layer) = stlayr(layer) / dlayr(layer)
         unul(layer) = unul(layer) / dlayr(layer)
         dlayr_cm(layer)=dlayr(layer)/10.
 
         RTDEPM=RTDEPM+DLAYR_cm(layer)             ! depth of profile
         UL=UL+ULLAYR(layer)*DLAYR_cm(layer)       ! upper limit for profile
         sat=sat+STLAYR(layer)*DLAYR_cm(layer)     ! saturated limit for profile
         WPWC=WPWC+UNUL(layer)*DLAYR_cm(layer)     ! unavailable water content
10    continue
 
      call get_real_var (unknown_module, 'es', '(mm)'
     :                                  , es, numvals
     :                                  , 0.0, 1000.0)
 
      es = es / 10.                            ! convert to cm
 
      call get_real_var (unknown_module, 'runoff', '(mm)'
     :                                  , q, numvals
     :                                  , 0.0, 1000.0)
 
 
      call get_real_array (unknown_module, 'sw_dep', max_layers, '(mm)'
     :                     , swlayr, nlayr
     :                     , 0.0, 1000.0)
 
      ! Convert water to plant available  (cm/cm)
 
      do 12 Layer = 1, nlayr
        swlayr(Layer) = swlayr(Layer) / 10. / dlayr_cm(layer)
     :                - unul(Layer)
        swlayr(Layer) = max(0.0, swlayr(Layer))
12    continue
 
      S_BED_MI = SWLAYR(1)/ULLAYR(1)              ! seed bed moisture index
      s_bed_sat = max(s_bed_sat,s_bed_mi)         ! top layer saturated?
 
      !   get initial estimate of available soil no3
      call get_real_array (unknown_module, 'no3_min', max_layers
     :                                    , '(mm)'
     :                                    , no3mn, numvals
     :                                    , 0.0, 1000.0)
 
      call get_real_array_optional (unknown_module, 'no3'
     :                                  , max_layers
     :                                  ,'(kg/ha)'
     :                                  , no3, numvals
     :                                  , 0.0, 1000.0)
 
      ! Need to check for situation of no N in system.
 
      N_in_system = (numvals .eq. nlayr)
 
      if (N_in_system) then
         !nothing
 
      else
         ! There is no N model in system.  Feed ozcot 150 units of N
         ! distributed throughout the profile
 
         do 15 Layer = 1, nlayr
 
            no3(Layer) = (150. / nlayr * 100.) / dlayr_cm(Layer)
15       continue
      endif
 
      ! Sum soil nitrate over all layers  (kg/ha)
 
      tsno3 = 0.
      do 20 Layer = 1, nlayr
        aNO3(layer) = no3(layer)-NO3mn(layer)
        tsno3 = tsno3 + ano3(layer)
20    continue
cpc
         availn = tsno3
 
      if (.not. N_in_system) then
         availn = tsno3
 
      else if(yest_tsno3.ne.0.) then
         availn = availn + tsno3 - yest_tsno3
 
      else
         availn = availn
      endif
 
      call pop_routine(myname)
      return
      end



* ====================================================================
       subroutine ozcot_set_other_variables ()
* ====================================================================
      implicit none
      include 'const.inc'
      include 'ozcot.inc'            ! ozcot common block
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
*      JNGH 18/7/94 Corrected conversion of min no3 from ppm to kg/ha
*      JNGH - 12/7/94 Changed dlayer in cm to dlayr_cm

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_set_other_variables')

*+  Local Variables
      integer Layer                    ! Layer number
      real    sno3(Max_layers)         ! available soil nitrate in layer kg/ha

*- Implementation Section ----------------------------------
      call push_routine(myname)
      ! Convert water from plant available  (cm/cm)
 
      do 10 Layer = 1, nlayr
        swlayr(Layer) = (swlayr(Layer) + unul(Layer))*10.
     :                * dlayr_cm(layer)
        swlayr(Layer) = max(0.0, swlayr(Layer))
10    continue
 
      ! Send updated soil water
 
cjh      call Set_real_array('sw_dep', swlayr, Max_layers, '(mm)')
      call Set_real_array (unknown_module, 'sw_dep', '(mm)'
     :                    , swlayr, nlayr)
 
      ! extract soil NO3
 
      do 20 Layer = 1, nlayr
        ano3(Layer) = ano3(Layer) - (dn_plant*10. * ano3(Layer) / tsno3)
        ano3(Layer) = max(0.0, ano3(Layer))
        sNO3(layer) = ano3(layer) + no3mn(layer)
20    continue
      yest_tsno3 = tsno3 - (dn_plant*10.)
 
      ! Send updated soil N
 
 
cjh      call Set_real_array('no3', sno3, nlayr, '(kg/ha)' )
      call Set_real_array (unknown_module, 'no3', '(kg/ha)'
     :                    , sno3, nlayr)
 
      call pop_routine(myname)
      return
      end



* ====================================================================
       subroutine ozcot_Send_my_variable
     .    (Variable_name)
* ====================================================================
      implicit none
       include 'ozcot.inc'           ! ozcot Common block
      include 'data.pub'                          
      include 'engine.pub'                        
      include 'intrface.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
       character Variable_name*(*)     ! (INPUT) Variable name to search for

*+  Purpose
*      Return the value of one of our variables to caller

*+  Changes
*      psc - 9/08/93
*      DPH 7/7/94 Changed crop_in variable to ozcot_crop_in.
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
     :        , '(days)', das)
 
      else if (variable_name .eq. 'sumdd') then
         call respond2get_real_var (variable_name
     :        , '(oCd)', sumdd)
 
      else if (Variable_name .eq. 'sites') then
         call respond2get_real_var (variable_name
     :        , '()', sites)
 
      else if (Variable_name .eq. 'squarz') then
         call respond2get_real_var (variable_name
     :        , '()', squarz)
 
      else if (Variable_name .eq. 'bollz') then
         call respond2get_real_var (variable_name
     :        , '()', bollz)
 
      else if (Variable_name .eq. 'openz') then
         call respond2get_real_var (variable_name
     :        , '()', openz)
 
      else if (Variable_name .eq. 'alint') then
         call respond2get_real_var (variable_name
     :        , '()', alint)
 
      else if (Variable_name .eq. 'dm') then
         dm = dw_total * 10.
         call respond2get_real_var (variable_name
     :        , '(kg/ha)', dm)
 
      else if (Variable_name .eq. 'totnup') then
         totnup = total_n * 10.
         call respond2get_real_var (variable_name
     :        , '(kg/ha)', totnup)
 
      else if (variable_name .eq. 'yield') then
         yield = alint / 227.
         call respond2get_real_var (variable_name
     :        , '(bales/ha)', yield)
 
      else if (variable_name .eq. 'lai') then
         call respond2get_real_var (variable_name
     :        , '(m^2/m^2)', alai)
 
      elseif (variable_name .eq. 'cover_green') then
         cover = l_bound (1.0 - exp (-ozcot_kvalue * alai), 0.0)
 
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , cover)
 
      elseif (variable_name .eq. 'cover_tot') then
         cover = 1.0 - exp (-ozcot_kvalue * alaiz)
 
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , cover)
 
 
 
      else if (Variable_name .eq. 'availn') then
         call respond2get_real_var (variable_name
     :        , '(kg/ha)', availn)
 
      else if (Variable_name .eq. 'tsno3') then
         call respond2get_real_var (variable_name
     :        , '(kg/ha)', tsno3)
 
      else if (Variable_name .eq. 'ysno3') then
         call respond2get_real_var (variable_name
     :        , '(kg/ha)', yest_tsno3)
 
      else if (Variable_name .eq. 'd_nup') then
         d_nup = dn_plant * 10.
         call respond2get_real_var (variable_name
     :        , '(kg/ha)', d_nup)
 
      else if (variable_name .eq. 'rtdep') then
         call respond2get_real_var (variable_name
     :        , '(cm)', rtdep)
 
      else if (variable_name .eq. 'tmean3') then
         call respond2get_real_var (variable_name
     :        , '(oC)', tmean3)
 
      else if (variable_name .eq. 's_bed_sat') then
         call respond2get_real_var (variable_name
     :        , '()', s_bed_sat)
 
      else if (variable_name .eq. 's_bed_mi') then
         call respond2get_real_var (variable_name
     :        , '()', s_bed_mi)
 
      else if (variable_name .eq. 'smi') then
         call respond2get_real_var (variable_name
     :        , '()', smi)
 
      else if (variable_name .eq. 'evap_plant') then
         call respond2get_real_var (variable_name
     :        , '(cm)', ep)
 
      else if (variable_name .eq. 'evap_soil') then
         call respond2get_real_var (variable_name
     :        , '(cm)', es)
 
      else if (variable_name .eq. 'evap_pot') then
         call respond2get_real_var (variable_name
     :        , '(cm)', eo)
 
      else if (variable_name .eq. 'evap_tot') then
         call respond2get_real_var (variable_name
     :        , '(cm)', et)
 
      else if (variable_name .eq. 'ozcot_crop_in') then
         call respond2get_logical_var (variable_name
     :        , '()', crop_in)
 
      else if (variable_name .eq. 'ozcot_status') then
         call respond2get_integer_var (variable_name
     :        , '()', iend)
 
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
      implicit none
      include 'ozcot.inc'             ! ozcot common block
      include 'engine.pub'                        

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
      implicit none
       include 'ozcot.inc'
      include 'error.pub'                         

*+  Purpose
*      Perform actions for current day.

*+  Changes
*      psc - 9/08/93
*      250996 jngh removed unused include

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_process')

*- Implementation Section ----------------------------------
 
      call push_routine(myname)
!     call patched-in ozcot model
 
      if (Crop_in) then
 
         if(jdate.ne.isow) das = das + 1
 
         call ozcot2 ()
 
         if (iend .eq. 2) then
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
      implicit none

*+  Purpose
*     Perform cleanup because the current simulation is about to end.

*+  Changes
*      psc - 9/08/93

*- Implementation Section ----------------------------------
 
      return
      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC                                                                CC
CC                                                                CC
CC                       PROGRAM OZCOT                            CC
CC                                                                CC
CC                          27/5/83                               CC
CC                                                                CC
CC                                                                CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                  C
C     BEGUN DURING A VISIT TO  T.A.E.S. BLACKLAND RESEARCH CENTER  C
C     TEMPLE, TEXAS 1981 BY A.B.HEARN.                             C
C                                                                  C
C     DEVELOPED AT NARS FOR NAMOI VALLEY COTTON  1982 TO 1988      C
C     BY HEARN AND DA ROZA.                                        C
C         COMPONENTS:                                              C
C           WATER BALANCE   - RITCHIE MODEL TAKEN LARGELY FROM     C
C                             CORNF(STAPPER & ARKIN)               C
C           NITROGEN MODEL -  DEVELOPED AT NARS BY HEARN & DA ROZA C
C           FRUIT SUBMODEL  - TAKEN FROM SIRATAC BY HEARN          C
C                                                                  C
C     OZCOT1 - version for retrospective simulation of specific    C
C              crops; runs for one season only, irrigation dates   C
C              given.                                              C
C     OZCOT2 - version for predictive simulation  through many     C
C              seasons; irrigation dates predicted.                C
C     OZCOT3 - version for optimising irrigation                   C
C     OZCOT4 - version for calibration with MINOS5                 C
C     OZCOT5 - version for physiological analyis                   C
C                                                                  C
C                                                                  C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     STRUCTURE
C       OZCOT2  INIT
C               CINPUT2
C               NEWDATE
C               METDAT2 HHUNC
C                       EVAP    FN SATVP
C               DECIDE_IRG
C               SOLWAT  SEVAP   FN WATCO
C                       SWBAL
C               SOWDAY
C               EMERG
C               SNBAL   N_FERTILISE
C               PLTGRW  ACTLAI
C                       LAIGEN  FN SENLF, FN STRESS
C                       CROPN
C                       ISTSQ
C                       FRUIT   BOLLWT  FN STRESS
C                               CARRYING_CAPACITY FN STRESS
C                               OVERLOAD
C                               ACTFRU
C                               UPDATE
C                               FN FRUGEN       FN STRESS
C                               FN SURVIVE
C               HARVEST
C               DAYOUT2
C               (DAY_DUDLEY    option for Norm Dudley)
C               YIELD
C               RESET
C
C       NOTE modules OZCOT2, CINPUT2, METDAT2, DAYOUT2, DECIDE_IRG
C                    NEWDATE, RESET are specific to OZCOT2.
C
C       ozcot.inc common blocks
C
C       INPUT FILES: MET.INP, SOILW.INP, AGRON.INP - 1
C       OUTPUT FILES: FRUIT.OUT, YIELD.OUT, CALIB.OUT - UNITS = 2,3
C
C LINK/EXE=OZCOT2 OZCOT2,INIT,CINPUT2,METDAT2,HFUNC,EVAP,SATVP, -
C                 DECIDE_IRG,SOLWAT,SEVAP,WATCO,SWBAL, -
C                 SOWDAY,EMERG,SNBAL,N_FERTILISE, -
C                 PLTGRW,ACTLAI,LAIGEN,SENLF,STRESS,CROPN,ISTSQ, -
C                 FRUIT,BOLLWT,CARRYING_CAPACITY,OVERLOAD,ACTFRU, -
C                 UPDATE,FRUGEN,SURVIVE, -
C                 HARVEST,DAYOUT2,YIELD,RESET
C
C
C                 OZCOT2 - CALLING PROGRAM
C                 ---------------------------
C
C
c      PROGRAM OZCOT2
      subroutine OZCOT2
      implicit none
      include 'ozcot.inc'
      include 'error.pub'

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot2')
      call push_routine(myname)
c
c     DATA AGRON_INP/'AGRON.INP'/, SOILW_INP/'SOILW.INP'/,
c    *COUNT_INP/'COUNT.INP'/, LAI_INP/'LAI.INP'/,
c    *RAIN_INP/'RAIN.INP'/, MET_INP/'MET.INP'/, FCOT_OUT/'FCOT.OUT'/,
c    *FRUCAL_OUT/'FRUCAL.OUT'/, YIELD_OUT/'YIELD.OUT'/
c
c      DATA IEND/0/
C
c
cpsc      i=i+1

c
c      DO 10 NSZN = 1,100                     ! seasonal loop
c          DO 20 I = 1,1000                   ! daily loop through whole year
c             CALL METDAT2 (I,IEND)       ! get met data
c              IF(IEND.EQ.1)GO TO 31       ! end of met data
              CALL ozcot_metdat2
c             IF(DEFIRR(2).NE.0.) CALL DECIDE_IRG (I)  ! irrigated crop?
c              CALL SOLWAT (I,DAYIR,NPRE)  ! soil water
              CALL ozcot_solwat                 ! soil water
c             IF(DEFIRR(2).NE.0.) CALL DECIDE_IRG       ! irrigated crop?
c             IF(ISOW.LE.0)THEN           ! crop sown yet?
c                  CALL SOWDAY (I,IEND)    ! sow tomorrow?
c                  CALL SOWDAY             ! sow tomorrow?
c                  IF(IEND.GE.3) GO TO 32  ! passed sowing window or fallow
c              ELSEIF(I.GT.ISOW .AND. IEMERG.LE.0)THEN     ! crop emerged yet?
              if(iemrg.le.0) then
c                  CALL EMERG (I)          ! emerge today?
                  CALL ozcot_emerg              ! emerge today?
              ENDIF
c              CALL SNBAL(I)               ! soil N balance
c              CALL SNBAL                  ! soil N balance
c              IF(ISOW.GT.0 .AND. I.GT.ISOW) CALL PLTGRW (I,IEND,NSZN)
c              IF(OPENZ.GT.0.0) CALL HARVEST(IEND)
              IF(ISOW.GT.0 .AND. das.GT.0) CALL ozcot_pltgrw
              IF(OPENZ.GT.0.0) CALL ozcot_harvest
c              IF(IEND.EQ.2)  GO TO 32     ! end of season
c              IF(IEND.ne.2)  then
c                CALL DAYOUT2(I)             ! daily output
cpsc                CALL DAYOUT2             ! daily output
C               CALL DAY_DUDLEY             ! output for Norm Dudley
c20             continue                           ! end of daily loop
c                return
c              endif
c32           CONTINUE
c              CALL YIELD(NSZN,IEND)           ! calculate yield
c              CALL RESET(IEND)                ! reset variables for new season
              CALL ozcot_yield                      ! calculate yield
cpsc              CALL RESET                      ! reset variables for new season
c10       continue                               ! end of seasonal loop
c31        CONTINUE
c          CALL EXIT
c      STOP

       call pop_routine(myname)   
       return
       END

c
      block data ozcot_initials
c
      INCLUDE 'ozcot.inc'
c
      DATA SCBOLL /5.0,5.0,4.7,4.5,5.5,4*.0,7./  ! DP16,DP61,DP90,SIOK,SICA
      DATA RESPCON/.025, .025, .02306, .01593, .02306, 4*.0,.025/ !  ditto
      DATA SQCON  /.021, .021, .02057, .02283, .02057, 4*.0,.021/ !   ditto
      DATA FCUTOUT/.4789, .4789, .4789, .5411, .4789,  4*.0,.48/ !  ditto
      DATA FLAI   /1.0, 1.0, 0.87, 0.52, 0.87, 4*0.0,1./         !  ditto
      DATA POPCON /.03633/
c
      DATA FBURR /1.23/                       ! factor sc/boll to sc+burr/boll
c
c      DATA LEAF_RES_N_conc/0.02/
c
      end


c      SUBROUTINE PLTGRW (I,IEND,NSZN)
      SUBROUTINE ozcot_pltgrw

C-------------------------------------------------------------------
C      CALLS THE VARIOUS PLANT GROWING ROUTINES.  AT THIS POINT    !
C      THERE ARE ALSO SOME VARIABLE CONVERSIONS TO ALLOW MERGING   !
C      OF THE INDEPENDENTLY DERIVED SOIL AND PLANT PORTIONS OF THE !
C      MODEL.                                                      !
C-------------------------------------------------------------------

      implicit none
      include 'ozcot.inc'
      include 'error.pub'

      real percent_l
cpc   integer ifrost
      integer j

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_pltgrw')
      DIMENSION PERCENT_L(10)
      DATA PERCENT_L/0.38,0.38,0.39,0.42,0.4,5*0.0/        ! lint percent
c      DATA FBURR /1.23/                       ! factor sc/boll to sc+burr/boll
cpc   DATA IFROST/0/                          ! flag for simulated frost

C----- housekeeping -----------------------------------------------------------
      call push_routine(myname)
cpsc      IDAY=I-ISOW ! replaced NCRPDY throughout, 15 Nov 1983
      iday=das
      DD=HUNITS
      RAD=SOLRAD
      PCLINT = PERCENT_L(IVAR)                 ! set lint percent for variety

      IF(IDAY.EQ.1) THEN
          IFROST = 0                           ! 1st day, reset local variable
      ELSE
          DO 10 J=1,IDAY
              FYZAGE(J)=FYZAGE(J)+DD           ! physiological aging
10        continue
      ENDIF
      SUMDD=SUMDD+DD

C----- increase root depth ----------------------------------------------------

      IF(IDAY.LE.36)RTDEP=SDEPTH+((20.-SDEPTH)/36.)*real(IDAY)  ! W*N
cpsc        changed maximum rooting depth
cpsc  IF(IDAY.GE.37.0)RTDEP=122.38*(1.-2.65*EXP(-.03*IDAY)) ! 82/8
      IF(IDAY.GE.37.0)RTDEP=182.38*(1.-2.65*EXP(-.03*IDAY)) ! 82/8
      IF(RTDEP.GT.RTDEPM)RTDEP=RTDEPM

C---- check if frost terminates crop -----------------------------------------

c      IF(TEMPMN.LE.2. .AND. IDAY.GT.120) THEN   ! frost? if so end of season
      IF(TEMPMN.LE.0. .AND. IDAY.GT.90) THEN   ! frost? if so end of season
          IEND=2 ! flag for frost - last day, open bolls > 80% mature
      ELSE IF(TEMPMN.LT.5. .AND. IDAY.GT.120) THEN ! frost? if so end of season
          IFROST = IFROST+1                     ! count days to simulate frost
          IF(IFROST.EQ.3) THEN
              IEND = 2                          ! frost
          ENDIF
      ELSE
          IFROST = 0                            ! reset because sequence broken
      ENDIF

c      IF(IEND.EQ.2) WRITE(2,777)JDATE,IDAY,I
c777        FORMAT(' FROST ON JULIAN DAY ',I3,', ',I3,
c     *    'DAYS FROM SOWING, INDEX=',I3,' *** END OF SEASON ***')

C----- emergence ( why not call ozcot_emerg here ------------------------------------

      IF(das.LT.IEMRG.OR.IEMRG.EQ.0) GO TO 25
      IF(das.EQ.IEMRG)  DDMERG=SUMDD-DD

C----- increase leaf area -----------------------------------------------------

c      IF(I.LE.ILAI)THEN
c       CALL ACTLAI(I)
c       CALL ACTLAI
c      ELSE
c       CALL LAIGEN(I)
        CALL ozcot_laigen
c      END IF

      call ozcot_dryxmatter
      call ozcot_plant_n

C----- crop nitrogen ---------------------------------------------------------

c      CALL CROPN(I)
      CALL ozcot_cropn

C---- grow plant -------------------------------------------------------------

      IF(ISQ.EQ.0) THEN
c          CALL ISTSQ (I,NSZN)
          CALL ozcot_istsq
      ELSE
c          IF(I.GT.ISQ) CALL FRUIT (I,IEND)
          IF(das.GT.ISQ) CALL ozcot_fruit
      ENDIF

C      IF(ISYR.EQ.82 .AND. JDATE.EQ.354) CALL HAIL(I) ! hail in 1982-83 experiment
c      IF(ISYR.EQ.82 .AND. JDATE.EQ.354) CALL HAIL    ! hail in 1982-83 experiment

25    CONTINUE

C---- crop development complete? ---------------------------------------------

      IF(OPENZ.GT.0.0 .AND. BOLLZ.EQ.0.0)
     * IEND=2                                   ! all bolls open, end of season

C------ FOLLOWING ARE FOR USE IN S/R YIELD -----------------------------------

      IF(ALAI.GT.ALAIZ)THEN
          ALAIZ=ALAI                            ! max LAI
          ILAIZ=IDAY                            ! day of max LAI
          PLNTNZ=PLANTN                         ! PLANTN on day of max LAI
          IPLNTN=ILAIZ
      ENDIF

      IF(SQUARZ.GT.SQZX) THEN
          SQZX = SQUARZ                         ! peak square numbers
          ISQZX = IDAY                          ! day of peak squares
      ENDIF

C------------------------------------------------------------------------------
      call pop_routine(myname)

      RETURN
      END


c      SUBROUTINE BOLLWT(IDAYX,L)
      SUBROUTINE ozcot_bollwt(L)

C     CALCULATES INCREASE IN WEIGHT OF EACH DAYS'S BOLLS.
C     BOLLGROWTH RATE IS DRIVEN BY DD,LIMITED BY WATER,
C     N AND C(incl water effects on photosynthesis) STRESS

      implicit none
      include 'ozcot.inc'
      include 'error.pub'


C------stuff done on 1st call of the day - stresses & growth rate -------------
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

      IF(IDAYX.EQ.0 .OR. IDAY.EQ.IDAYX+1) THEN ! 1st call for this day?
        IDAYX = IDAY                         ! if so, set flag to stop more calls
        FBCSTR = 1.0                         ! C stress factor to reduce BOLLGR
        IF(BLOAD.GT.0.)FBCSTR = CARCAP_C/BLOAD ! supply/demand ratio for when
        IF(FBCSTR.GT.1.)FBCSTR = 1.          ! boll growth limited by C supply
        FBWSTR = ozcot_stress(0.0,0.5,1.0,SMI)     ! water stress on bolls
        FBWSTR = 1.                          ! try no direct stress - 24/4/92
        IF(BOLLZ+OPENZ .LT. CARCAP_N) THEN   ! final boll req < uptake
            FBNSTR = 1.0                     ! do not apply N stress
        ELSE                                 ! final boll req < uptake
            FBNSTR = ozcot_stress(0.0,0.5,1.0,FNSTRS)      ! apply N stress
        ENDIF
        STRSBL = AMIN1(FBCSTR,FBWSTR,FBNSTR) ! minimum of C, water & N stress

        IF(TEMPAV.LT.20.0) THEN
          F_TEMP = -3.0+0.2*TEMPAV            ! temperature scaling factor
        ELSE IF(TEMPAV.GT.30.0) THEN          ! for boll weight
          F_TEMP = 7.0-0.2*TEMPAV             ! derived from Hesketh and Low 1968
        ELSE
          F_TEMP = 1.0
        ENDIF
        IF(F_TEMP.LT.0.) F_TEMP = 0.
        IF(F_TEMP.GT.1.) F_TEMP = 1.

        BOLL = SCBOLL(IVAR)*F_TEMP            ! effect of temperature
        BGRVAR = BOLL*BPER                    ! boll growth rate this day

      ENDIF

      IF(L.GT.LFRU(4) .OR. LFRU(4).EQ.0) then
         call pop_routine(myname)
         RETURN
      else
      endif

C------         INCREASE IN WEIGHT OF BOLLS (SEED COTTON) ---------------------------

      BOLLGR = BGRVAR*STRSBL                  ! todays rate per boll
      BOLLGR = BOLLGR *1.3                    ! potential unstressed rate
      IF(BOLLGR.LT.0.) BOLLGR = 0.
      FRUWT(L) = FRUWT(L)+FRUNO(L)*BOLLGR     ! increase today for L day bolls

C------ shed fruit not growing ------------------------------------------------

      IF(L.GT.LFRU(5) .AND. L.LE.LFRU(7) .AND. FRUNO(L).GT.0.) THEN
           IF(FRUWT(L)/FRUNO(L).LT.0.1) FRMARK(L,6) = FRUNO(L)
      ENDIF

C------------------------------------------------------------------------------
      call pop_routine(myname)
      RETURN
      END


c      SUBROUTINE CARRYING_CAPACITY(I)
      SUBROUTINE ozcot_carrying_capacity

C     ESTIMATES CARRYING CAPACITY OF CROP ON BASIS OF PHOTOSYNTHESIS.
C     SELECTS PARAMETER FOR VARIETY. ADJUSTED FOR WATER STRESS.
C     CARCAP is carrying capacity, maximum number of bolls the crop
C     can carry, therefore the boll load that causes 100% shedding.
  
      implicit none
      include 'ozcot.inc'
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


C      DATA FROM "SIRATAC" - 1987-88  HEARN (PERS. COMM.)

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot')
cpc   DATA ISTRESS/0/, IRELIEFCO/0/
      call push_routine(myname)

cpsc      IF(I.EQ. ISQ+1) THEN
      IF(das.EQ. ISQ+1) THEN
          ISTRESS = 0                   ! reset stress flag
          IRELIEFCO = 0                 ! reset stress relief counter
      ENDIF

cpsc      IF(BLOAD.GT.CUTOUT .AND. SMI.LT.0.75) ISTRESS = 1 ! set stress cutout flag

cpsc      IF(SMI.GT.0.75 .AND. ISTRESS.GT.0) THEN

      if(bload.gt.cutout.and.smi.lt.0.25) istress=1   ! 0.25 replaced 0.75 - ABH 5/11/96
      if(smi.gt.0.25.and.istress.gt.0) then           ! 0.25 replaced 0.75 - ABH 5/11/96

          IRELIEFCO = IRELIEFCO + 1     ! count days since relief of stress
          IF(IRELIEFCO.EQ.7) THEN
              ISTRESS = 0               ! end stress effect on wterlogging
              IRELIEFCO = 0             ! reset counter
          ENDIF
      ENDIF

C----Photosynthetic capacity --------------------------------------------------
C-----light interception modified to give hedgerow effect with skip row - ABH 5/11/96 ------      
      
      ALAI = ALAI*RS               ! lai in hedgerow
      ALIGHT = (1.-EXP(-ozcot_kvalue*ALAI)) ! original code  - now gives interception in hedgerow
      ALIGHT =ALIGHT/RS            ! interception on ground area basis
      ALAI = ALAI/RS               ! restore LAI to ground area basis

      RADN_watts = SOLRAD*0.8942        ! convert RADN from ly to watts m**2
      P_gossym = 2.391+RADN_watts*(1.374-RADN_watts*0.0005414) ! GOSSYM line1275
      POT_PN = P_gossym*0.068           ! potential photosynthesis g/m2 CH2O
      PN = POT_PN*ALIGHT                ! net photosynthesis term
C----- effect of water stress on photosysthesis -------------------------------

      IF(SMI .LT. 0.5) THEN
          RELP =.25+.864*SMI                ! effect of water stress on P
          IF(RELP.GT.1.)RELP = 1.           ! (TURNER et al 1986).
          RELP = RELP -.25
          RELP = ozcot_stress(0.0,1.0,0.5,RELP)   ! increase severity of stress
          RELP = RELP + .25
          PN = PN*RELP                      ! photosynthesis adjust for water stress
      ENDIF

C----- waterlogging effect additional to N uptake - Hearn & Constable 1984 eqn 4

C      IF(ISTRESS.GT.0) THEN
C        IF(SW/UL.GT.0.87) PN = PN * 0.1 ! carrying capacity reduced
C      ELSE
C        IF(SW/UL.GT.0.87) PN = PN * 0.1 ! carrying capacity reduced
C      ENDIF

C---- Maintenance respiration -------------------------------------------------

      TEMPLF = TEMPAV+5.-10.*SMI        ! leaf temperature
      IF(TEMPLF.LT.TEMPAV) TEMPLF=TEMPAV
      RFAC = 2.2**((TEMPLF-35.)/10.)    ! resp temp factor, Horie(Constable)
      RM = SITES*RESPCON(IVAR)*RFAC     ! maintenance respiration term

C----- carrying capacity carbon - photosynthesis divided by boll growth rate

      IF(BGRVAR.GT.0.)
     * CARCAP_C = (PN-RM)/(BGRVAR*FBURR) ! carrying capacity, carbon, no stress
      IF(CARCAP_C.LT.0.) CARCAP_C = 0.  ! trap

C----- waterlogging effect additional to N uptake - Hearn & Constable 1984 eqn 4
      IF(ISTRESS.GT.0) THEN
        IF(SW/UL.GT.0.87) CARCAP_C = CARCAP_C * 0.0 ! carrying capacity reduced
      ELSE
        IF(SW/UL.GT.0.87) CARCAP_C = CARCAP_C * 0.2 ! carrying capacity reduced
      ENDIF

      CUTOUT = CARCAP_C*FCUTOUT(IVAR)     ! boll load for cutout, sq prodn stops
C-------------------------------------------------------------------------------
      call pop_routine(myname)
      RETURN
      END


c      SUBROUTINE CROPN(I)
      SUBROUTINE ozcot_cropn
      real harvest_n
      real bgr

C     assumes all N available for season (AVAILN) from SNBAL
C     is taken up (UPTAKN).
C     computes: N harvested on basis of Constable and Rochester 1988
C               N in fruit FRUN
C               carrying capacity (CARCAP_N), number of bolls for which
C                       HARVEST_N is adequate
C                       used in FRUGEN and SURVIVE for squares
C               stresses:
C                       vegetative (VNSTRS for LAIGEN) = f(1-HARVEST_N/UPTAKN)
C                               ie function of proportion of N remaining in veg
C                         fruit (FNSTRS for BOLLWT) 1- FRUN/HARVEST_N
C                               ie function of N to be harvested not in fruit

      INCLUDE 'ozcot.inc'

C----------------------------------------------------------------------------

      UPTAKN = AVAILN       ! potential uptake for season based on available N

C-----  compute potential N harvested ----------------------------------------

      HARVEST_N = UPTAKN * 0.85      ! harvestable N; 0.85 fromConstable & Rochester
      HARVEST_N = HARVEST_N/10.      !  kg/ha to g/m**2, N limiting fruit.

C----- compute N already in fruit -------------------------------------------
cpdev this is generating an underflow warning:
cpdev ---------------------------------vvvvvvvvvv
      SEED_NC = .02407+0.000147*UPTAKN-0.00000034*UPTAKN**2 ! GAC dat 3/6/88
      FRUN = (FRUDW+OPENWT)*((1.-PCLINT)*SEED_NC+(FBURR-1.)*.005) ! N in frt

C----- compute N carrying capacity --------------------------------------------

      BGR = SCBOLL(IVAR)                    ! seed coton per boll
      CARCAP_N = HARVEST_N /(BGR*0.6*0.03)  ! bolls per m

C----- compute N stresses -----------------------------------------------------

      VNSTRS = (UPTAKN-10.)/UPTAKN      ! vegetative stress 10=uptake @ 1st boll

      IF(BOLLZ.EQ.0.) THEN              ! before 1st boll
          FNSTRS = 1.0
      ELSE
          FNSTRS = 1.- FRUN/HARVEST_N   ! fraction of harvestable not in fruit
      ENDIF

      IF(VNSTRS.GT.1.0) VNSTRS=1.0
      IF(VNSTRS.LT.0.0) VNSTRS=0.0
      IF(FNSTRS.GT.1.0) FNSTRS=1.0
      IF(FNSTRS.LT.0.0) FNSTRS=0.0
C----------------------------------------------------------------------------

      RETURN
      END


c      SUBROUTINE EMERG (I)
      SUBROUTINE ozcot_emerg
C-------------------------------------------------------------------
C
C     Simulates  emergence
C
C-------------------------------------------------------------------

      INCLUDE 'ozcot.inc'

      IF(IEMRG.GT.0) RETURN

C----- Simple heat sum as an alternative to Wanjura's function -----

       IF(SUMDD .LT. 60.) RETURN
       IEMRG = das
       RETURN
       END

C-------------------------------------------------------------------

C      Previous version Jackson/Arkin

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      CALCULATES HYPOCOTLE ELONGATION LEADING UP TO EMERGENCE     C
C      BASED ON THE GROWING-DEGREE-DAY CONCEPT.                    C
C      THE ELONGATION RATE OF 0.12 CM/GDD IS USE TO A MAXIMUM RATE C
C      OF 2.04 CM/GDD AT ABOUT 32 C.                               C
C      THE GDD BASE TEMPERATURE IS SET IN THE "INIT" SUBROUTINE.   C
C      ELONGATION RATE DATA ARE FROM WANJURA ET.AL. 1970.          C
C                                                                  C
C      KEY VARIABLES:                                              C
C          SDEPTH = SEED SOWING DEPTH                              C
C          STEMP = SOIL TEMPERATURE (CALC. IN SUBROUTINE "METDAT") C
C          IEMRG = EMERGENCE DATE                                  C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C       T = STEMP                             ! soil temp
C       IF(STEMP.LT.14.44)T = 14.44+(14.44-T) ! to comput lag whenSTEMP < 14.44
C       HYPOEL = 0.0853-0.0057/(41.9-T)*(T-34.44)**2 ! Wanjura's function
C       HYPOEL = HYPOEL*24.                   ! convert to daily rate
C       HYPOEL = HYPOEL*0.6                   ! tune to Namoi valley
C       IF(STEMP.LT.14.44)HYPOEL = HYPOEL*(-0.5) ! delay below 14.44
C       ESUM = ESUM+HYPOEL
C
C      IF(ESUM .LT. SDEPTH) RETURN
C      IEMRG = I
C
C      RETURN
C      END


c      SUBROUTINE EVAP (I)
      SUBROUTINE ozcot_evap
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      CALCULATES POTENTIAL EVAPOTRANSPERATION FROM MODIFIED PEN-  C
C      MAN EQUATION AS REPORTED BY RITCHIE.                        C
C                                                                  C
C      KEY VARIABLES:                                              C
C          DELTA = SLOPE OF THE WATER VAPOR/ AIR TEMP. CURVE       C
C          ALBEDO = COMBINED PLANT-SOIL ALBEDO                     C
C          H = NET RADIATION BALANCE                               C
C          EO = POTENTIAL ET OVER BARE SOIL                        C
C          EOS = POTENTIAL ET BELOW PLANT CANOPY                   C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      INCLUDE 'ozcot.inc'
      real ozcot_satvp

      real elev
      real p
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

C---------- CALCULATE BOWEN RATIO TERM -----------------------------------------

        ELEV=200.0                                    ! ELEVATION (M)
        P=1013.0-0.105*ELEV                           ! MEAN PRESSURE(MB)
        GAMMA=6.6E-04*P*(1.0+0.00115*TEMPWT)       ! PSYCHROMETER CONSTANT
        TK=TEMPAV+273.                                  ! MEAN TEMP(DEG KELVIN)
        DELTA=(EXP(21.255-5304./TK))*(5304./(TK**2.)) !SLOPE SAT VAP PRES CURVE
        D=DELTA/(DELTA+GAMMA)

C------ CALCULATE FRACTION RADIATION REACHING SURFACE(TR) ----------------------
C       CULLS FUNCTIONS(UNPUBLISHED)


        XN1=0.404*ALOG(S)+1.49
cpsc
        ALAI = ALAI*RS              ! lai in hedgerow
        IF(ALAI.LT.XN1) THEN       ! when LAI below XN1 threshold
            TR=EXP(-0.6*ALAI)
        ELSE                        ! when LAI above XN1 threshold
            TR=EXP(-0.398*ALAI)
        ENDIF

cpsc
        F_INT = 1-TR                ! intercetion in hedgerow
        F_INT = F_INT/RS            ! interception on ground area basis
        TR =  1.-F_INT              ! transmission on ground area basis
        ALAI = ALAI/RS              ! restore LAI to ground area basis

C        IF(RS.GT.1.) TR = (TR+RS-1.)/RS ! adjust for rows wider than Im

C-----VAPOR PRESSURE DEFICIT: MEAN OF VPD AT 9AM AND 3PM ,ASSUMING ATMOS.
C     VP.(VPDRY) REMAINS CONSTANT THROUGHOUT DAY, AND TMAX=T3PM-----------------

        SVPMAX=ozcot_satvp(TEMPMX)        ! SAT VP AT TMAX=SAT VP AT T3PM
        SVPDRY=ozcot_satvp(TEMPDY)        ! SAT VP AT 9AM TDRY
        SVPWET=ozcot_satvp(TEMPWT)        ! SAT VP AT 9AM TWET

           VPDRY=SVPWET-GAMMA*(TEMPDY-TEMPWT)  ! atmospheric VP
           IF(VPDRY.LE.0.) VPDRY = 0.1         ! cannot be -ve.
           VPD=((SVPDRY-VPDRY)+(SVPMAX-VPDRY))/2.

C------ CALCULATE POTENTIAL EVAPORATION (EO) -----------------------------------

        SALPHA=0.09
        ALBEDO=0.3067+0.3333*SALPHA-((0.23-SALPHA)/0.75)*TR  ! ??

C------NET LONGWAVE RADIATION(R6,CAL/CM2)--------------------------------------

C      RITCHIE(1975)UNPUB.;IDSO & JACKSON(1969)J GEOPHYS RES,74:5397-5403;
C      JENSEN ET AL(1969)PROC AM SOC CIVIL ENGIN,96:61-79                             ! TEMPLE,TEXAS
C        R4=1.-.261*EXP(-7.77E-04*TEMPAV**2.)        ! SKY EMISSIVITY,EA
C        R6=(R4-.96)*1.17E-07*TK**4.*(.2+.8*SOLRTO)  ! ((EA-ES)*@*TK**4)*(-)

        R4=1.08*(1.-EXP(-VPDRY**(TK/2016.)))
        R6=(R4-1.)*1.*1.17E-07*TK**4.*(.2+.8*SOLRTO) ! ((EA-ES)*@*TK**4)*(-)
                                                     ! ES=EMISSIVITY OF EVAP SURFACE

C-------NET RADIATION(H,CAL/CM2)----------------------------------------------

        H=(1.-ALBEDO )*SOLRAD+R6
        IF(H.LT.0.0) H=0.0

        HO=H/583.                                   ! NET RADIATION(CM)
C       GO=G/583.                                   ! SOIL HEAT FLUX(CM)

C-------advection or aerodynamic term ------------------------------------------

        IF(WIND.NE.0..AND.TEMPWT.NE.0.) THEN
            AT = (1.-D)*0.01*(1.+.028*WIND)*VPD     ! advection: wind & VPD
        ELSE IF(WIND.EQ.0..AND.TEMPWT.NE.0.) THEN
            AT = -11.9813+0.012*VPD+0.0404*TK       ! advection: VPD but no wind
        ELSE
            AT = -20.4355+0.0697*TK                 ! advection:  no VPD or wind
        ENDIF
        IF(AT.LT.0.0) AT = 0.0

        EO=HO*D+AT                                  ! GdaR Mar'85, update ABH Mar88.

C------ CALCULATE POTENTIAL BELOW CANOPY EVAPORATION (EOS) ---------------------

        RNS=HO*TR ! GdaR Mar'85
        EOS=D*RNS ! GdaR Mar'85
        IF(EOS.LT.0.0)EOS=0.0
        IF(EOS.GT.EO)EOS=EO

        RETURN
        END


c      FUNCTION FRUGEN(I)
      real FUNCTION ozcot_frugen(ndas)
C
C     ESTIMATES GENERATION OF NEW FRUIT AS FUNCTION OF EXISTING
C     FRUITING SITES AND BOLLOAD ADJUSTED FOR NITROGEN STRESS

C      DATA FROM "SIRATAC" - 1987-88 HEARN (PERS. COMM.).

      INCLUDE 'ozcot.inc'
      integer ndas

      real ozcot_stress

cpc   real sites1
      real blr
      real dfru
      real vsnstr
      real ppm_row
      real popfac

c     psc      i=icount
C-----------------------------------------------------------------------------
C     initialise on first call for new crop
C-----------------------------------------------------------------------------

c      IF(I.EQ.ISQ+1)THEN           ! 1st call,ie day after 1st square
      IF(ndas.EQ.ISQ+1)THEN           ! 1st call,ie day after 1st square
          N_CUTOUT=0               ! flag to show cutout, >= 1 = not squaring
          SITES1=0.0               ! to save current value of SITES
c          ALAG=0.0                 ! reset lag accumulator
      ENDIF
      ozcot_frugen=0.                    ! delete y'day's value and reset to zero
C-----------------------------------------------------------------------------
C     IS THIS 1ST CYCLE, CUTOUT OR 2ND CYCLE (REGROWTH)?
C-----------------------------------------------------------------------------

      BLR = 1.                             ! boll load ratio =1 when CUTOUT=0
c      IF(I.EQ.ISQ+1) BLR = 0.              ! except day after 1st square
      IF(ndas.EQ.ISQ+1) BLR = 0.              ! except day after 1st square
      IF(CUTOUT.GT.0.) BLR=BLOAD/(CUTOUT)  ! ratio of 0 is no boll load
      IF(BLR.GT.1.) BLR=1.                 ! ratio of 1 is full boll load

      IF(BLR.LT.1.0 .AND. N_CUTOUT.le.0)GO TO 30 ! squaring continues
      IF(BLR.LT.1.0 .AND. N_CUTOUT.GE.1)GO TO 20 ! squaring stopped, may resume
      IF(BLR.EQ.1.0 .AND. N_CUTOUT.GE.1)GO TO 10 ! squaring stopped, no restart

      N_CUTOUT=1   ! ie (BLR.EQ.1.0 .AND. N_CUTOUT.EQ.0) and squaring stops today

10    CONTINUE

C-----------------------------------------------------------------------------
C     if coutout due to water stress or full boll load ie SMI < 0.75
C     10% of fruiting sites become inactive for FRUGEN every day after 5 days
C-----------------------------------------------------------------------------

      IF(SMI.LT.0.75) N_CUTOUT = N_CUTOUT + 1         ! count days cutout
      IF(N_CUTOUT.GT.5) SITES1 =SITES1 + 0.1*SIZE*PPM ! inactive SITES

      RETURN

20    CONTINUE
      IF(SITES1.GT.SITES) SITES1 = SITES
      N_CUTOUT=0                    ! recovery complete, squaring resumes

30    CONTINUE

C-----------------------------------------------------------------------------
C     SQUARE PRODUCTION FOR THIS DAY
C-----------------------------------------------------------------------------

      SIZE = (SITES-SITES1)/PPM     ! active SITES per plant for FRUGEN & SURVIV
      IF(SIZE.LT.1.0) SIZE = 1.0    ! average  plants has 1 site
      IF(SIZE.LT.0.5/PPM) SIZE = 0.5/PPM    ! average  plants has 1 site

      IF(CARCAP_C.EQ.0.) THEN       ! either 1st day afer squaring or defoliated
c          IF(I.EQ.ISQ+1) THEN                             ! day after 1st square
          IF(ndas.EQ.ISQ+1) THEN                             ! day after 1st square
             DFRU = SQCON(IVAR)*SQRT(SIZE)*PPM            ! sites per DD
          ELSE
             RETURN                                       ! defoliated FRUGEN=0.
          ENDIF
      ELSE
          DFRU = SQCON(IVAR)*SQRT(SIZE)*PPM*(1.-BLOAD/CUTOUT) ! sites per DD
          VSNSTR = ozcot_stress(0.0,0.9,1.0,VNSTRS)
          DFRU = DFRU * VSNSTR
          IF((BOLLZ+OPENZ)/CARCAP_N .GE. 1.0) DFRU = 0.      ! N limiting
          
      ENDIF

      PPM_ROW = PPM*RS                   ! plants per m row for POPFAC
      POPFAC = 1./(1.+POPCON*PPM_ROW)     ! plant population factor within row
      DFRU = DFRU * POPFAC               ! adjust for plant population
      ozcot_frugen = DFRU * DD                 ! today's squares
      IF(ozcot_frugen.LT.0.0) ozcot_frugen=0.0
c      IF(ISYR.EQ.77 .AND. I.LT.ISQ+15) ozcot_frugen=ozcot_frugen*0.1 ! delay for 77/78 season - 23/6/88

      RETURN
      END


c     subroutine FRUIT (I,IEND)
       SUBROUTINE ozcot_fruit

C      AGES FRUIT, SUMS FRUIT IN CATEGORIES, ESTIMATES PHYSIOLOGICAL
C      SHEDDING AND SURVIVAL OF FRUIT.  CALLS S/R ACTFRU TO ESTIMATE
C      NEW SQUARES FLOWERS AND OPEN BOLLS FROM COUNTS. ALTERNATIVELY
C      CALLS S/R ozcot_frugen TO ESTIMATE NEW SQUARES WHEN COUNTS NOT
C      AVAILABLE.

      INCLUDE 'ozcot.inc'
      real ozcot_frugen, ozcot_survive

      real frudd
      real wt
      integer n, m, ndayfl
      real sfmcat, bltme, surv
      integer l , lf, mm, mmm, nfl, if 

c      DIMENSION FRUDD(8),WT(8),SFMCAT(8),BLTME(8),BPSUM(300)
      DIMENSION FRUDD(8),WT(8),SFMCAT(8),BLTME(8)
      DATA FRUDD/50.,180.,350.,380.,520.,660.,870.,1100./
      DATA BLTME/3*0.0,0.07,0.21,0.33,0.55,1.0/
      DATA WT/.0104,.0272,.1441,.0988,.5042,.9617,1.0,.5785/
c      DATA SCBOLL /5.0,5.0,4.7,4.5,5.5,4*.0,7./  ! DP16,DP61,DP90,SIOK,SICA
c      DATA RESPCON/.025, .025, .02306, .01593, .02306, 4*.0,.025/ !  ditto
c      DATA SQCON  /.021, .021, .02057, .02283, .02057, 4*.0,.021/ !   ditto
c      DATA FCUTOUT/.4789, .4789, .4789, .5411, .4789,  4*.0,.48/ !  ditto
c      DATA FLAI   /1.0, 1.0, 0.87, 0.52, 0.87, 4*0.0,1./         !  ditto
c      DATA POPCON /.03633/
c
C----  re-intialise arrays and variables to zero where appropriate -------------

c      IF(I.EQ.ISQ+1)THEN        ! 1st call of new season on day after 1st square
c          DO 10 N=1,300
c              BPSUM(N)=0.0
c10        continue
c          IDAYX = 0             ! flag to BOLLWT, called for 1st time this day
c      ENDIF

      DO 100 N=1,8
          FRUCAT(N)=0.
          SFMCAT(N)=0.
          IF(N.GT.7)GO TO 100
          DO 20 M=1,7
              FMKCAT(N,M)=0.
20        continue
100   continue

      FRUDW = 0.
      NDAYFL=0

C---- compute boll period ------------------------------------------------------

      BPER = 1.0/EXP(5.385-.0512*TEMPAV)    ! boll period Constable

C-----------------------------------------------------------------------------
C     The next loop ( DO 200 L=....) goes through the fruit arrays
C     starting with the element with oldest fruit and therefore the smallest
C     index, in order to develop or abscise fruit, and put in categories.
C     Before entering the loop, find the oldest fruit.
C-----------------------------------------------------------------------------

      LF=LFRU(9)      ! 1st active element in fruit array, oldest immature fruit
cpsc      IF(LF.EQ.0)LF=ISQ-ISOW-10 ! start in FRUNO 10 days before 1st square
      IF(LF.EQ.0)LF=ISQ-10 ! start in FRUNO 10 days before 1st square

cpsc      DO 200 L=LF,I-ISOW-1 ! loop from oldest growing boll to previous day
      DO 200 L=LF,das-1 ! loop from oldest growing boll to previous day
          IF(L.GT.300)GO TO 200
cpsc          CALL BOLLWT(IDAYX,L)
          CALL ozcot_bollwt(L)

C---- age & abscise marked fruit----------------------------------------

          DO 30 M = 1,6
              MM = 6-M+1
              MMM = MM+1
              FRMARK (L,MMM) = FRMARK(L,MM)
30        continue
          FRMARK(L,1)=0.                   ! CLEAR FIRST DAY
          IF(FRUNO(L).GT.0. .AND. FRMARK(L,7).GT.0.)THEN ! fruit shed today?
              IF(FRMARK(L,7).GT.FRUNO(L))THEN
c                  WRITE(2,999) I,JDATE,L,FRUNO(L),FRMARK(L,7)
c999               FORMAT
c     *            (' Too many marked fruit on I, JDATE:',3I4,2F5.1)
c                  FRMARK(L,7) = FRUNO(L)
              ENDIF
              FRUWT(L) = FRUWT(L)*(FRUNO(L)-FRMARK(L,7))/FRUNO(L) ! adjust wt sheds
              FRUNO (L)=FRUNO(L)-FRMARK(L,7)   ! remove marked fruit

          ENDIF

C---- sort fruit and marked fruit into age categories  -----------------------

          IF(FYZAGE(L).GT.FRUDD(3))
     *    BPSUM(L)=BPSUM(L)+BPER                ! develop day L's bolls
          IF(N_DEF.GT.0) BPSUM(L)=BPSUM(L)/0.99 ! develop faster after defoliation
          IF(IEND.EQ.2 .AND. BPSUM(L).GE.0.9) BPSUM(L)=1.0 ! frost opens phys mat bolls

          DO 40 N=1,8                              ! stay in loop until category found
              IF(N.LT.4)THEN                    ! if yes, squares
                  IF(FYZAGE(L).LT.FRUDD(N)) GO TO 204
              ELSE                              ! no, therefore flowers or bolls
                  IF(BPSUM(L).LT.BLTME(N))GO TO 204
              ENDIF
40        continue

C------- if last loop completed, fruit are open bolls -------------------------

          IF(L.LE.LFRU(9))GO TO 200 ! this days bolls already open?
          LFRU(9)=L                 ! this days bolls open today, reset marker
c          IF(I.LE.IDATE)GO TO 202   ! use actual counts or not?
          OPENZ=OPENZ+FRUNO(L)      ! simulated bolls open added to total
          OPENWT=OPENWT+FRUWT(L)

c202       CONTINUE

          FRUNO(L)=0.0 ! delete open bolls from array
          FRUWT(L)=0.
          GO TO 200

204       CONTINUE
          IF(L.LE.LFRU(N))GO TO 206 ! this days fruit in category N  yet?
          LFRU(N)=L                 ! this days fruit into category N today.
          IF(N.NE.4)GO TO 206       ! category is flowers?
          NDAYFL=NDAYFL+1           ! count this days flowering
c          IF(I.GT.IDATE)GO TO 206   ! using actual counts or not?
c          FRUNO(L)=0.0              ! clear for actual counts
c          GO TO 200

206       CONTINUE
          FRUCAT(N)=FRUCAT(N)+FRUNO(L)             ! sum fruit nos in categories
          IF(N.GE.4) FRUDW = FRUDW+FRUWT(L)        ! sum dry wt of fruit
          IF (N.GT.7) GO TO 200
          DO 50 M=2,6                                 ! loop thro marked fruit
              FMKCAT(N,M)=FMKCAT(N,M)+FRMARK(L,M)  ! sum marked fruit
              SFMCAT(N)=SFMCAT(N)+FRMARK(L,M)      ! sum marked fruit for BLOAD
50        continue

200   continue

C---- total squares, green (growing) bolls, open bolls---------------------

      SQUARZ=0.
      BOLLZ=0.
      BLOAD=0.                                       ! reset
      DO 60 N=1,8
          IF (N.LE.3) SQUARZ=SQUARZ+FRUCAT(N)        ! total squares
          IF (N.GE.4.AND.N.LE.8) BOLLZ=BOLLZ+FRUCAT(N)   ! total bolls
          BLOAD=BLOAD+(FRUCAT(N)-SFMCAT(N))*WT(N)        !  boll load
60    continue

C---- this day's production of fruit -----------------------------------------

c      CALL CARRYING_CAPACITY(I)
      CALL ozcot_carrying_capacity
      IF(BLOAD.GT.CARCAP_C .OR. BOLLZ+OPENZ.GT.CARCAP_N)
     *CALL ozcot_overload                                  ! abort excess fruit

c      IF (I.LE.IDATE)THEN                            ! use counted fruit?
c      IF (das.LE.IDATE)THEN                            ! use counted fruit?
c          CALL ACTFRU (I)
c          CALL ACTFRU
c          CALL UPDATE(1,1,DAYSQZ,SQUARZ,FRUDD)
c          IF(LFRU(4).NE.0..OR.DAYSFL.NE.0.)
c     *        CALL UPDATE(4,NDAYFL,DAYSFL,BOLLZ,FRUDD)
c          IF(LFRU(9).NE.0..OR.DAYSOP.NE.0.)
c     *        CALL UPDATE(9,1,DAYSOP,OPENZ,FRUDD)
c      ELSE
cpsc           icount = i               ! dummy parameter to use frugen
c          FRUNO(I-ISOW)=FRUGEN(I)  ! FRUGEN is function generating squares
          FRUNO(das)=ozcot_frugen(das)      ! ozcot_frugen is function generating squares

c      END IF

C---- mark this day's fruit for physiological shedding      -----------------

      SURV = ozcot_survive(CARCAP_C,BLOAD)   ! square survival rate

C      IF(JDATE.LT.59) SURV = 0.1       ! for KRS with delayed protection
C      SURV =SURV*0.33                  ! for pest damage in 1972/3 Namoi

c      FRMARK(I-ISOW,1)=FRUNO(I-ISOW)*(1.-SURV)
c      IF(FRMARK(I-ISOW,1).LT.0.) FRMARK(I-ISOW,1)=0.0
c      FMKCAT(1,1)=FMKCAT(1,1)+FRMARK(I-ISOW,1)
      FRMARK(das,1)=FRUNO(das)*(1.-SURV)
      IF(FRMARK(das,1).LT.0.) FRMARK(das,1)=0.0
      FMKCAT(1,1)=FMKCAT(1,1)+FRMARK(das,1)
      IF(NDAYFL.EQ.0)GO TO 501
      SURV = ozcot_survive(CARCAP_C,BLOAD)   ! boll survival rate
C      SURV =SURV*0.33                  ! for pest damage in 1972/3 Namoi

      DO 70 NFL = 1,NDAYFL
          IF = LFRU(4)-NFL+1
          FRMARK(IF,1) = FRUNO(IF)*(1.-SURV)
          IF(FRMARK(IF,1).LT.0.)FRMARK(IF,1) = 0.0
          FMKCAT(4,1) = FMKCAT(4,1)+FRMARK(IF,1)
70    continue

C---- add new fruit to totals ------------------------------------------------

  501 CONTINUE

c      SITES = SITES+FRUNO(I-ISOW)
c      IF(I.LE.IDATE)RETURN ! SQUARZ & FRUCAT updated in UPDATE
c      SQUARZ = SQUARZ+FRUNO(I-ISOW)
c      FRUCAT(1) = FRUCAT(1)+FRUNO(I-ISOW)
      SITES = SITES+FRUNO(das)
      IF(das.LE.IDATE)RETURN ! SQUARZ & FRUCAT updated in UPDATE
      SQUARZ = SQUARZ+FRUNO(das)
      FRUCAT(1) = FRUCAT(1)+FRUNO(das)

C-----------------------------------------------------------------------------

      RETURN
      END


c      SUBROUTINE HARVEST(IEND)
      SUBROUTINE ozcot_harvest

C     This subroutine simulates defoliation and picking
C     Use N_DEF, N_PICK and J_PICK for cost of defoliation:       NB ACTION
C         cost of defoliant is N_DEF * $25                 <-- for gross margins
C         cost of picking is N_PICK * $?                   <-- for gross margins
C         cost of delay is (J_PICK - 91) * $ per day       <-- for gross margins
C              if 1st April (day 91) is reference date.
C     Likelihood of 2nd pick is indicated. Currently 10 boll/m (BOLLZ=10)
C     assumed to be worth picking, depends on price of cotton and cost of
C     picking, ask growers. Should be part of management model.
C     Date of 2nd pick and partition of yield between 1st and 2nd pick are
C     subjects for future development in a more comprehensive whole farm
C     management model.

      INCLUDE 'ozcot.inc'

      IF(N_DEF.EQ.0 .AND. SQUARZ/PPM.GT.2.) RETURN       ! too many squares
      IF(OPENZ/(BOLLZ+OPENZ).GT.OPEN_DEF/100. .AND. J_PICK.EQ.0) THEN
          IF(N_DEF.EQ.0) THEN
               N_DEF = 1                                 ! 1st defoliant spray
               I_DEF = IDAY                              ! day of 1st defol.
c               WRITE(2,100) N_DEF, JDATE, I_DEF
          ENDIF
          IF(N_DEF.EQ.1 .AND. IDAY-I_DEF.GE.10) THEN     ! 10 days since 1st?
               IF(ALAI.GT.0.2) THEN
                   N_DEF=2                               ! 2nd defoliant spray
                   I_DEF=IDAY                            ! day of 2nd defol
c               WRITE(2,100) N_DEF, JDATE, I_DEF
               ELSE
                   J_PICK = JDATE                        ! date of picking
                   N_PICK = 1                            ! count picks
c                   WRITE(2,101) J_PICK
                   IF(BOLLZ.GT.10.) THEN                  ! 10 bolls worth picking
                       N_PICK = 2                        ! count picks
c                       WRITE(2,102)
                   ENDIF
               ENDIF
          ENDIF
          IF(N_DEF.EQ.2 .AND. IDAY-I_DEF.EQ.10) THEN
              J_PICK = JDATE                             ! date of picking
              N_PICK = 1                                 ! count picks
c              WRITE(2,101) J_PICK
              IF(BOLLZ.GT.10.) THEN
                  N_PICK = 2                             ! count picks
c                  WRITE(2,102)
              ENDIF
          ENDIF
      ENDIF
      IF(J_PICK.NE.0 .AND. BOLLZ.LT.1.0) THEN
              IEND = 2                                   ! terminate crop
c              WRITE(2,103) JDATE, IDAY
      ENDIF
C
c100   FORMAT(' DEFOLIANT SPRAY',I2,' ON DAY',I4,',',
c     *       I4,' DAYS FROM SOWING.')
c101   FORMAT(' FIRST PICK 0N',I4)
c102   FORMAT(' There are sufficient bolls for a 2nd pick')
c103   FORMAT(' Simulation terminated on day',I4,','I4,
c     *': no further boll growth, all bolls forced open.')
      RETURN
      END


c      SUBROUTINE HFUNC (I)
      SUBROUTINE ozcot_hfunc
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      CALCULATES DAILY HEAT UNITS BASED ON A 12.0 DEG C BASE      C
C      TEMPERATURE.  THE HEAT UNIT SUM IS BASED AN INTEGRATED      C
C      DIURNAL SIN-WAVE WITH THE MAXIMUM AND MINIMUM DAILY TEMP-   C
C      ERATURES AS THE PEAK AND TROUGH, RESPECTIVELY.  HEAT UNITS  C
C      ARE NOT ALLOWED TO ACCUMULATE ABOVE A CUTOFF OF 30.0 C.     C
C      THE BASE (BASET) AND CUTOFF (HUCUT) TEMPERATURES ARE SET IN C
C      THE 'INIT' SUBROUTINE.                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      INCLUDE 'ozcot.inc'
      real pi
      real amp
      real tmax
      real zeta

C
      PI=3.14159
c      TEMPAV=(TEMPMN+TEMPMX)/2.
      AMP=TEMPMX-TEMPAV
      TMAX=TEMPMX
      IF(TMAX.GE.HUCUT) TMAX=HUCUT
      IF(TEMPMN.GE.BASET) GO TO 10
      IF(TMAX.LE.BASET) GO TO 20
      ZETA=ASIN((BASET-TEMPAV)/AMP)
      HUNITS=1./PI*(AMP*COS(ZETA)+(TEMPAV-BASET)*(PI/2.-ZETA))
      GO TO 30
   10 HUNITS=(TMAX+TEMPMN)/2.-BASET
      GO TO 30
   20 CONTINUE
      HUNITS=0.0
   30 CONTINUE
   
      RETURN
      END


c      SUBROUTINE INIT
      SUBROUTINE ozcot_INITIAL()
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      INITIALIZES VARIABLES IN ALL COMMON BLOCKS                  C
C                                                                  C
C      KEY VARIABLES:                                              C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      INCLUDE 'ozcot.inc'
cpc      real soltro, spi, cumep, cumes, cumet, deltsw, dal
cpc      real bolln, bolnc
cpc      integer nfert
      integer lastiday, j, isdex, ncnt, lastfr, k


C------------------------------------------------------------------------------
C      'BCLIM' CLIMATE PARAMETERS
C------------------------------------------------------------------------------
c
      MODE=2 ! OZCOT1: -1=calib'n, 1=valid'n; OZCOT2: 2=full simulation
c
cpsc      nszn = 1
cpsc      i = 0

      IEND=0
      IDAY=0
cpc   LastIDAY = 0
cpsc      JDATE=0
cpsc      IMET=0
      TEMPAV=0.0
      tempre=0.0
      tempyd=0.0
      alint=0.0
      HUNITS=0.0
      HUCUT=40.0 ! ABH changed from 30 to 40 - 11/11/83
      BASET=12.0
      ASOIL=0.0
cpc   SOLTRO=0.0
      STEMP=0.0
c      EO=0.0
c      EOS=0.0
cpc     SPI=0.0
cpc     CUMEP=0.0
cpc     CUMES=0.0
cpc     CUMET=0.0
C
c      TEMPMX=0.0
c      TEMPMN=0.0
c      SOLRAD=0.0
c      RAIN=0.0
c      NEWRAN= 0
C------------------------------------------------------------------------------
C     IRRIGATION VARIABLES
C------------------------------------------------------------------------------
      DO 10 J=1,10
c          IGDAY(J)=0
          RRIG(J)=0.0
10    continue
C------------------------------------------------------------------------------
C      'BSOIL' SOIL PROFILE PARAMETERS
C------------------------------------------------------------------------------
      ISW = 0    ! flag to initialise water balance in SOLWAT
      AMBDA=1.44 ! Priestly-Taylor  daRosa 1983
      UL1=1.4    ! limit for stage 1 ES  daRosa 1983
      CONA=0.35  ! ES rate in stage 2  daRosa 1983
cpc   ISDEX=0.0
      NLAYR=0
c      SW=0.0
c      UL=0.0
      DEF=0.0
      NIRR=0
      TWATER=0.
cpc   DELTSW=0.0
        RTSW=1.0
      DO 20 J=1,6
c      DLAYR(J)=0.0
c     ULLAYR(J)=0.0
c      SWLAYR(J)=0.0
      TRANS(J)=0.0
   20 CONTINUE
C------------------------------------------------------------------------------
C      'BPLNT' PLANT PARAMETERS
C------------------------------------------------------------------------------
cpc     NCNT=0
      IDATE=0
      ILAI=0
      IEMRG=0
      ISOW=0
      ISQ=0
      IVAR=0
      CRSPCE=0.0
      PPM=0.0
      ESUM=0.0
      ALAI=0.0
cpc   DAL=0.0
      SHEDLF=0.0
      FRUDW=0.0
      SMI=0.0
      SDEPTH=0.0
      RTDEP=0.0
      RTGROW=0.0
C------------------------------------------------------------------------------
C      'BNITR' SOIL NITROGEN TRANSFORMATION PARAMETERS
C------------------------------------------------------------------------------
cpc   NFERT=0
      UPTAKN=0.0
        AVAILN=0.0

c      DO 30 J=1,2
c      NDAY(J)=0
c      SNAPLC(J)=0.0
c   30 CONTINUE
C------------------------------------------------------------------------------
C      'FRUITS' PARAMETERS ARE NUMBERS,WEIGHTS,ABSCISSION COUNTS
C      FOR BOLLS AND SQUARES.
C------------------------------------------------------------------------------
cpc   LASTFR=0
      BGRVAR=0.0
      DD=0.0
      DDMERG=0.0
      SUMDD=0.0
      DO 40 J=1,8
      FRUCAT(J)=0.0
      LFRU(J)=0
   40 CONTINUE
      LFRU(9)=0

      DO 50 J=1,300
      DLAI(J)=0.0
      dDW_L(J) = 0.0
      bpsum(j) = 0.0
      FYZAGE(J)=0.0
      FRUNO(J)=0.0
      FRUWT(J)=0.0
      DO 50 K=1,7
      FRMARK(J,K)=0.0
      IF(J.LT.8) FMKCAT(J,K)=0.0
   50 CONTINUE

C------------------------------------------------------------------------------
C      'TOTALS' COUNTS OF SQUARES, BOLLS, AND SITES AT ANY GIVEN TIME
C------------------------------------------------------------------------------

      BLOAD=0.0
      BOLLZ=0.0
      OPENZ=0.0
      OPENWT=0.0
      SITES=0.0
cpc
      sites1 = 0.0
      SQUARZ=0.0
C------------------------------------------------------------------------------
C      'INDEX' STRESS AND SURVIVAL INDICES.
C------------------------------------------------------------------------------
      CARCAP=0.0
      CARCAP_C = 0.0
      CARCAP_N = 0.0
      CUTOUT=0.0
      VNSTRS=1.0
      FNSTRS=1.0
      IDAYCO = 0
      LAST_DAY = 0
C------------------------------------------------------------------------------
C      'COUNTS'
C------------------------------------------------------------------------------
c      DO 60 J=1,25
c      JCO(J)=0
c      SQCNT(J)=0.0
c      BLCNT(J)=0.0
c      OPCNT(J)=0.0
c   60 CONTINUE
C------------------------------------------------------------------------------
C      'BPNITR' ARE FOR PLANT NITROGEN.
C------------------------------------------------------------------------------
      UPTAKN=0.0
      VEGN=0.0
cpc   BOLLN=0.0
      PLANTN=0.0
cpc   BOLNC=0.0
      STRUCN=0.0
      FRUN=0.0
C------------------------------------------------------------------------------
C     /YIELD/ FOR OUTPUT WITH YIELD
C------------------------------------------------------------------------------
      ALAIZ=0.
      ILAIZ=0
      PLNTNZ=0.
      IPLNTN=0
      SQZX = 0.
      ISQZX = 0
      DEF_LAST=0.
C------------------------------------------------------------------------------
C     /PICK/ variables to simulated defoliation and picking
C------------------------------------------------------------------------------
      J_PICK=0
      N_PICK=0
      N_DEF=0
      I_DEF=0
      OPEN_DEF = 60.
c------------------------------------------------------------------------------
C     /SOW/
C------------------------------------------------------------------------------
      IWINDOW = 60       ! width of sowing window
      SOW_SW = 0.0       ! for subsoil water rule
C------------------------------------------------------------------------------
C     /DRYWT/ variables for simulation of dry weight increase
C------------------------------------------------------------------------------
      A_ROOT_LEAF = 1.01   ! allometric constant root:leaf. Huxley's data 1964
      A_STEM_LEAF = 1.25   ! allometric constant stem:leaf. Huxley's data 1964
      E_PAR = 2.5          ! g/MJ Charles-edwards et al 1986
      FBURR = 1.23         ! ABH
      SPECIFIC_LW = 58.0   ! g/m2  GAC 71/72, Hoffman & Rawlins 1971, Ben-Porath
      T_OPT = 25.          ! Constable 1981
      T_BASE = 8.          ! Constable 1981
      WT_AREA_MAX = 150.   ! Hesketh and Low 1968, Hoffman & Rawlins = 80.
      WT_AREA_MIN = 30.0   ! Huxley 1964
      EMBRYO = 0.75        ! dry weight of seedling at emergence
      F_LEAF = 0.6         ! proportion of leaf dry weight at emergence
      F_STEM = 0.15        ! ditto for stem
      F_ROOT = 0.25        ! ditto for root  -  data by extrapolation from Huxlry

      BOLLGR = 0.0
      BPER = 0.0
      DLAI_POT = 0.0
      DW_BOLL = 0.0
      DW_LEAF = 0.0
      DW_ROOT = 0.0
      DW_STEM = 0.0
      DW_TOTAL = 0.0
      total_n = 0.0
      RESERVE = 0.0
      RES_CAP = 0.0
      ROOT_FEEDBACK = 0.0

      RETURN
      END


c      SUBROUTINE ISTSQ (I,NSZN)
      SUBROUTINE ozcot_istsq

C     IDENTIFIES FIRST SQUARE EVENT AND GIVES INITIAL VALUE FOR
C     FRUNO(1),SITES & SQUARZ.
C     delayed by water stress (SMI < 0.25) and cold shock (TEMPMN < 11)
C     currently sets ISQ=I, should be ISQ=IDAY for consistency between seasons
C     when convenient, change and check all refs to ISQ

      INCLUDE 'ozcot.inc'
      real ddisq
      

      DIMENSION DDISQ(10)

c      DATA ISZN/0/                         ! flag for new season
      DATA DDISQ/9*420.,320./ ! Constable (pers. comm. 1983), 10=Empire

      IF(IDATE.NE.0) GO TO 40

C     NO COUNTS - SIMULATE FIRST SQUARE

c      IF(ISZN.NE.NSZN) THEN                ! is this a new season?
c          ISZN = NSZN                      ! reset flag
c          DELAY = 0.0                      ! delay in day degrees
c      END IF

cpsc    add delay to common block

      IF(SMI.LT.0.25) THEN                 ! water stress delays squaring
          DELAY = DELAY+(1.-(SMI/0.25))*HUNITS
      END IF

      IF(TEMPMN.LT.11.) DELAY = DELAY+5.2  ! cold shock Constable (pers. comm)

      IF(SUMDD.LT.DDISQ(IVAR)+DELAY)RETURN
c      FRUNO(I-ISOW)=1.0*PPM  ! average plant has 1 square
c      FRUNO(I-ISOW)=0.5      ! as in 1983/84 version
      FRUNO(das)=1.0*PPM  ! average plant has 1 square
      FRUNO(das)=0.5      ! as in 1983/84 version
      GO TO 43
C
C      USING COUNTS
C
   40 CONTINUE
c      DO 41 N=1,25
c      IF(JCO(N).EQ.0)RETURN !  no squares counted yet
c      IF(I.LT.JCO(N)+1)RETURN
c      IF(SQCNT(N+1).GT.0.)GO TO 42 ! when JCO(N) EQ I and squares at next count
c   41 CONTINUE
c      RETURN
c   42 CONTINUE
c      FRUNO(I-ISOW)=SQCNT(N+1)/(JCO(N+1)-JCO(N))
c      NEXT=N ! for ACTFRU
C
C     SQUARE & SITE PRODUCTION ON FIRST DAY
C
   43 CONTINUE
c      SQUARZ=SQUARZ+FRUNO(I-ISOW) ! sum SQUARES
c      SITES=SITES+FRUNO(I-ISOW) ! sum SITES
c      ISQ=I                     ! should be ISQ=IDAY see above
      SQUARZ=SQUARZ+FRUNO(das)   ! sum SQUARES
      SITES=SITES+FRUNO(das)     ! sum SITES
      ISQ=das                    ! should be ISQ=IDAY see above

      RETURN
      END


c      SUBROUTINE LAIGEN (I)
      SUBROUTINE ozcot_laigen
C
C     ESTIMATES CURRENT LAI. GENERATES NEW LEAVES DAILY AS A
C     FUNCTION OF DD AND FRUITING SITE PRODUCTION. INITIAL AREA
C     IS A FUNCTION OF DD TO FIRST SQUARE THEN A FUNCTION OF NEW
C     SITES. RELATIVE RATE OF AREA EXPANSION A FUNCTION OF DD
C     TO 1ST SQUARE, THEN A FUNCTION OF dSITES .
C      ALL LEAF AREAS ARE SQUARE METERS PER PLANT EXCEPT ALAI
C      WHICH IS M**2/M**2 I.E. LAI.
C
      INCLUDE 'ozcot.inc'
      real ozcot_stress, ozcot_senlf

      real leaf_res_n_conc
      real rlai, dlds, acotyl, dlds_x, vlnstr
      real a, b1, b2, b3, flfsmi, actrgr, alaix
      real ddleaf
      integer index, in, l
      DATA ACOTYL/.00035/,RLAI/.00835/,DLDS/0.0362/
      DATA LEAF_RES_N_conc/0.02/

C------- INITIALISE LEAF AREA ON DAY OF EMERGENCE -----------------------------

        IF(das.EQ.IEMRG) THEN                        ! already emerged?
c           DLAI(IEMRG-ISOW)=ACOTYL*PPM            ! initial area
c           ALAI=DLAI(IEMRG-ISOW)
c           LASTLF=IEMRG-ISOW                      ! set index for oldest leaf
            DLAI(IEMRG)=ACOTYL*PPM            ! initial area
            ALAI=DLAI(IEMRG)
            LASTLF=IEMRG                      ! set index for oldest leaf
            RETURN
        ENDIF

C------- CALCULATE RATE OF LEAF AREA EXPANSION --------------------------------

        A  =   0.1847  !
        B1 =  -0.1165  ! constants for LAI calibration eqn
        B2 =  -0.01514 ! 29 April 1988
        B3 =   0.01984 ! expts WN8283 & 8384

        dLdS_X = (A+B1*0.75+B2*VPD+B3*0.75*VPD) ! sqrt area/site, no water stress
        IF(dLdS_X.LT.0.) dLdS_X = 0.
        dLdS_X = dLdS_X**2                      ! area per site, no water stress

        dLdS = (A+B1*SMI+B2*VPD+B3*SMI*VPD)     ! sqrt area per site
        IF(dLdS.LT.0.) dLdS = 0.
        dLdS = dLdS**2                          ! area per site

        FLFSMI = ozcot_stress(0.0,0.5,1.0,SMI) ! pre-squaring

C-------------------------------------------------------------------------------

        IF(ISQ.EQ.0) THEN                 ! crop not yet squaring

            ACTRGR=RLAI                   ! actual RGR
            ALAIX=ALAI                    ! save previous LAI
            INDEX=IFIX(DD)                ! index for DO loop below to grow leaf

            DO 10 IN=1,INDEX
                ALAIX=ALAIX*(1.+ACTRGR)   ! grow leaf without water stress
10          continue
            DLAI_POT=ALAIX-ALAI     ! days increase without water stress

            ACTRGR=ACTRGR*FLFSMI          ! water stress
            ALAIX=ALAI                    ! save previous LAI again

            DO 11 IN=1,INDEX
                ALAIX=ALAIX*(1.+ACTRGR)   ! grow leaf with water stress
11          continue
            DLAI(IDAY)=ALAIX-ALAI         ! days increase with water

        ELSE                              ! crop now squaring
                                              ! without water stress
            dLdS_X = dLdS_X*FLAI(IVAR)        ! adjust for variety, 87 MKI calib'n
            DLAI_POT = FRUNO(IDAY-1)*dLdS_X ! days incr in LAI
                                              ! with water stress
            dLdS = dLdS*FLAI(IVAR)            ! adjust for variety, 87 MKI calib'n
            DLAI(IDAY) = FRUNO(IDAY-1)*dLdS   ! days incr in LAI

        ENDIF
        VLNSTR = ozcot_stress(0.0,0.9,1.0,VNSTRS)
        DLAI(IDAY) = DLAI(IDAY)*VLNSTR        ! adjust for N stress
        DLAI_POT = DLAI_POT*VLNSTR ! adjust for N stress
        ALAI=ALAI+DLAI(IDAY)

C*******SENESCENCE ***************************************************

        DDLEAF=ozcot_senlf(BLOAD,ALAI,CARCAP_C,SMI)
        IF(N_DEF.EQ.1 .AND. IDAY-I_DEF.GT.7) DDLEAF = DDLEAF*0.33 ! 1st defol'n
        IF(N_DEF.EQ.2 .AND. IDAY-I_DEF.GT.7) DDLEAF = DDLEAF*0.0  ! 2nd defol'n
        SHEDLF=0.
        LEAF_RES = 0.                   ! initialise for this day
        IF(LASTLF.EQ.0)GO TO 21         ! called after measured LAI finished
c       DO 20 L=LASTLF,I-ISOW           ! loop thro unshed leaves
        DO 20 L=LASTLF,das             ! loop thro unshed leaves
        IF(FYZAGE(L).LT.DDLEAF)GO TO 21 ! are this days leaves shed today?
        ALAI=ALAI-DLAI(L)               ! reduce LAI
        SHEDLF=SHEDLF+DLAI(L)           ! sum area of shed leaves
        DLAI(L)=0.                      ! day,s area now zero
      DW_LEAF = DW_LEAF-dDW_L(L)      ! reduce leaf dry matter
      LEAF_RES = LEAF_RES+dDW_L(L)    ! sum this day's residues
      dDW_L(L) = 0.0                  ! this day's leaf wt now zero
      LASTLF=L+1 ! set index for oldest remaining leaves.
20      CONTINUE
21      CONTINUE

      LEAF_RES_N = LEAF_RES * LEAF_RES_N_CONC ! N content of leaf residues

      RETURN
      END


c      SUBROUTINE METDAT2 (I,IEND)
      SUBROUTINE ozcot_metdat2
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      SUBROUTINE TO CHECK INPUT CLIMATE DATA FOR MISSING OR ER-   C
C      RONEOUS VALUES.  INPUT VARIABLE UNITS ARE ASSUMED INPUT     C
C      AND CONVERTED AS FOLLOWS:                                   C
C          TEMPERATURE -      DEG C*10 TO DEG C                    C
C          RAINFALL    -      MM/DAY*10 TO CM/DAY                  C
C          RAINFALL    -      INCHES/DAY TO CM/DAY                 C
C          SOLRAD      -      LY/DAY (NOT CONVERTED)               C
C                                                                  C
C      UNIQUE VARIABLES:                                           C
C          QA = AN UPPER BOUNDARY FOR SOLRAD                       C
C          SOLRTO = NEEDED CONSTANT FOR NET RADIATION IN "EVAP"    C
C                                                                  C
C       WHEN TEMPERATURE (INCLUDING WET AND DRY BULB) AND          C
C       RADIATION ARE MISSING OR NOT AVAIALBLE, EZSTIMATES ARE     C
C       MADE AS FUNCTION OF DAYS SINCE LAST RAIN.                  C
C       CONSEQUENTLY A FULL SUITE OF MET DATA ELEMENTS CAN BE      C
C       GENERATED FROM RAINFALL ALONE.                             C
C                                                                  C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      INCLUDE 'ozcot.inc'
      integer nsince, ndate
      real root, rclear, srad, wet_depress, date
      DATA NSINCE/0/
C
C         **** READ MET DATA  ***************************************
C
c       READ(1,151,END=153) JDATE,IMYR,
c     *  RAIN,EPAN,TEMPMX,TEMPMN,TEMPDY,TEMPWT,WIND,SOLRAD
c151    FORMAT(4X,I3,5X,I4,F5.0,F4.1,2F4.0,2F4.1,F4.0,F4.0) ! new

c     **** Climate data now read in as part of interface. ****

cpsc        solrad = solrad / 0.04186
cpsc        rain = rain / 10.0
        wind = 0.
        tempdy = 0.
        tempwt = 0.
cpsc        epan = 0.
c

cjh
       IF(JDATE.EQ.1) THEN                 ! new year?
         MDPY = 365                       ! reset days per year
         IF((IMYR/4*4).EQ.IMYR) MDPY=366  ! leap year
       ENDIF


c       IF(EPAN.EQ.0. .OR. EPAN.EQ.99.9)      EPAN=EPANX
c       EPANX=EPAN

c       IF(WIND.EQ.0. .OR. WIND.EQ.999)  THEN
c           IF(WINDX.NE.999) THEN
c               WIND=WINDX
c           ELSE
c               WIND=0
c           ENDIF
c       ENDIF
c       WINDX=WIND

c      GO TO 155
c153   IEND=1 ! flag end of data
c      RETURN
C
c155   CONTINUE
cpsc      EPAN=EPAN/10.
C
C         **** CHECK AND CONVERT RAINFALL (CM) ****
C
c        IF(RAIN.LT.0.0) RAIN=0.0
c       RAIN=RAIN/100.0

C  LOCAL RAIN DATA

c       IF(NEWRAN.EQ.0) GO TO 100
c       IF(JDATE.GE.NRNSDY(1).OR.JDATE.LE.NRNSDY(NEWRAN)) RAIN=0.0
c        DO 10 J=1,NEWRAN
c         IF(JDATE.EQ.NRNSDY(J)) RAIN=RANSUB(J)/10.
c10      continue
c100    CONTINUE

C  DAYS SINCE RAIN

      IF(ISOW.GT.0) THEN    ! from sowing to first boll
        IF(BOLLZ.EQ.0.0) THEN
          RRIG(3) = RRIG(3)+RAIN           ! accumulate rainfall before bolling
        ELSE IF(BOLLZ.GT.0.0 .AND. OPENZ/(OPENZ+BOLLZ).LT.0.2) THEN ! bolling
          RRIG(4) = RRIG(4)+RAIN           ! accumulate rainfall in bolling
        ENDIF
      ELSE
          RRIG(5) = RRIG(5)+RAIN           ! accumulate rainfall in fallow
      ENDIF

      IF(RAIN.LE.0.1) THEN
        IF(NSINCE.LT.1) NSINCE=1
        NSINCE = NSINCE + 1
      ELSEIF(NSINCE.GT.1) THEN
        NSINCE=1                    ! 1st day of rain period, light rain
        IF(RAIN.GT.10.) NSINCE = 0  ! heavy rain
      ELSE
        NSINCE = 0                  ! rain period
      ENDIF
      ROOT =FLOAT(NSINCE)
      IF(ROOT.GT.5.) ROOT=5.
      IF(ROOT.GT.0.) ROOT = SQRT(ROOT)
      IF (NSINCE.GT.9) NSINCE=9


C      **** CHECK SOLAR RADIATION AND FILL IN FOR MISSING DATA. ****
C      **** PLEASE NOTICE THAT IN THE FOLLOWING LINES LOCATION  ****
C      **** SPECIFIC EQUATIONS ARE USED.    (CAL/CM2)           ****

C  CALCULATE EXTRATERRESTRIAL RADIATION AT NARS

c       XRAD=(JDATE+9)*(360./365.)*.0174533 ! DAY OF YEAR AS ANGLE IN RADIANS
c        QA=749.6+(302.4*SIN(1.562+XRAD)) ! EXTRA TERRESTRIAL RADIATION(NARS)

c       IF(SOLRAD.LT.0.0)SOLRAD=0.0
c        IF(SOLRAD.GT.0.0 .AND. SOLRAD.NE.999.) GO TO 30

C ESTIMATE MISSING GROUND MEASURED SOLAR RADIATION

c       IF(JDATE.GT.135)GO TO 152
c       QQA=0.66-0.4708*EXP(-0.75*NSINCE)        ! Q/QA FOR DAYS 1-135
c        IF(NSINCE.LE.1)QQA=0.4658-0.003485*RAIN
c        GO TO 160
c152    CONTINUE
c       IF(JDATE.GE.225)GO TO 154
c       QQA=0.5892-0.7986*EXP(-1.219*NSINCE)     ! Q/QA FOR DAYS 136-225
c        IF(NSINCE.LE.1)QQA=0.1382+0.2777*EXP(-0.04375*RAIN)
c        GO TO 160
c154    CONTINUE
c       QQA=0.63324-0.7693*EXP(-1.0*NSINCE)
c        IF(NSINCE.LE.1)QQA=0.2148+0.2087*EXP(-0.01875*RAIN)
c160    CONTINUE
c       SOLRAD=QA*QQA           ! EST OF GROUND RAD =F(DAYS SINCE RAIN)
c        SOLRAD_MIN = QA*0.18      ! minimum - 0.18 from Brutsart(1982)
c       IF(SOLRAD.LT.SOLRAD_MIN)SOLRAD=SOLRAD_MIN
c30     CONTINUE
C
C   ACTUAL SOLAR/CLEAR DAY SOLAR RADIATION RATIO FOR LONGWAVE ESTIMATE
C
        RCLEAR=551.52+246.40*SIN(0.0172*(JDATE+99.96)) !CLEAR DAY SOL RAD(NARS)
        SRAD=SOLRAD
        IF(SRAD.GT.RCLEAR) SRAD=RCLEAR
        SOLRTO=SRAD/RCLEAR
c        SOLRAD=SRAD

C       **** CHECK AND CONVERT AIR TEMPERATURES (DEG C) ****
C
c       TEMPMX=TEMPMX/10.0
c       TEMPMN=TEMPMN/10.0

c      IF(TEMPMX.EQ.0. .OR. TEMPMX.EQ.99.9) THEN

C  ESTIMATE MISSING TEMPMX

c       TMAXXX=26.24+8.325*SIN(1.172+XRAD)   ! TMAX 8 OR MORE DAYS AFTER RAIN
c       FTMAX=1.03-0.1812*EXP(-0.1953*NSINCE)! TMAX/TMAXXX
c       TEMPMX=TMAXXX*FTMAX               ! TMAX=F(DAYS SINCE RAIN)
c       IF(RAIN.GT.4.0)TEMPMX=TMAXXX*.83
c       IF(RAIN.GT.5.0)TEMPMX=TMAXXX*.8

c      ENDIF

c      IF(TEMPMN.EQ.99.9) THEN

C  ESTIMATE MISSING TEMPMN

c       TMINXX=11.45+8.144*SIN(1.078+XRAD)             ! TMIN ON DRY DAYS
c       IF(NSINCE.LE.1)TMINXX=13.47+5.949*SIN(1.+XRAD) ! TMIN ON WETDAYS
c       IF(NSINCE.EQ.2)FTMIN=.993                      ! FIRST DAY AFTER RAIN
c       IF(NSINCE.GT.2)FTMIN=.925+.01321*NSINCE
c       IF(NSINCE.LE.1)FTMIN=1.003+.005169*RAIN-.0001039*RAIN**2
c       TEMPMN=TMINXX*FTMIN                         ! ESTIMATE OF TMIN

c      END IF

C       ESTIMATE WET AND DRY BULB WHEN ODD DAY MISSING
C
      IF(TEMPDY.EQ.0. .OR. TEMPDY.EQ.99.9) THEN
          TEMPDY = -.54+0.57*TEMPMX+0.40*TEMPMN
      ENDIF
      IF(TEMPWT.EQ.0. .OR. TEMPWT.EQ.99.9) THEN
          WET_DEPRESS = -3.103+0.28*TEMPMX-0.07*TEMPMN+0.62*ROOT
          IF(WET_DEPRESS.LT.0.) WET_DEPRESS=0.
          TEMPWT = TEMPDY-WET_DEPRESS
      ENDIF
C
C          **** CALCULATE SOIL HEAT FLUX (CAL/CM2) ****
C
        NDATE=JDATE+183                 ! CONVERT TO NORTHERN HEMISPHERE
        IF(NDATE.GT.MDPY) NDATE=NDATE-MDPY
        DATE=real(NDATE)
        G=1.7+14.6*SIN(0.0172*(DATE-51.0))             ! TEMPLE,TEXAS
        IF(G.LT.0.0) G=0.0
c
c      CALL HFUNC (I) ! calculate heat units or daydegrees
      CALL ozcot_hfunc
      STEMP=(TEMPMX+TEMPMN)/2.0*ASOIL
c      CALL EVAP (I) ! calculate potential evaporation
      call ozcot_evap
      RETURN
      END


      SUBROUTINE ozcot_n_fertilise (APPLIED,AVAILN,APPLIED_AVAIL)
C
C      Simulates uptake of fertiliser nitrogen. Assumes that there is an upper
C      limit to the amount of nitrogen a crop can take up and the rate of uptake
C      or recovery of fertiliser N decreases linearly from a maximum initial
C      value to zero when uptake limit is reached (Basinski et al 1975, Cot
C      gr Rev). Assume intial rate is 1.0 (100% recovery) and maximum uptake
C      is 240 kg/ha.
C
      real uptakn_max, rate_reducer, availnx, fraction
      real applied, applied_avail, availn
      integer n, nkg
       DATA UPTAKN_MAX/240./
C
       NKG = IFIX(APPLIED)             !  integer of kgs, index for DO loop
       RATE_REDUCER = 1./UPTAKN_MAX    !  recovery decreases with uptake
C
       AVAILNX   = AVAILN              ! available N before application
       DO 1000 N=1,NKG
           FRACTION = 1.0-RATE_REDUCER*AVAILN ! fraction of next kg available
           AVAILN = AVAILN + FRACTION
1000  continue
       APPLIED_AVAIL = AVAILN-AVAILNX  ! N applied now that will be available
C
       RETURN
       END


      SUBROUTINE ozcot_overload

C-------------------------------------------------------------------------------
C     simulates abscission or abortion of older fruit under extreme stress
C     ie when boll load exceeds carrying capacity.
C-------------------------------------------------------------------------------

      INCLUDE 'ozcot.inc'
      real over_c, over_n, fload, capacity, excess, available
      real abort
      integer l, icat, m
C----- determine if OVERLOAD called for C or N ---------------------------------

      OVER_C = 999.                           ! very large when CARCAP_C=0.
      IF(CARCAP_C .GT. 0.) OVER_C = BLOAD/CARCAP_C
      OVER_N = 999.                           ! very large when CARCAP_N=0.
      IF(CARCAP_N .GT. 0.) OVER_N = BOLLZ/CARCAP_N

      IF(OVER_C .GT. OVER_N) THEN             ! C is limiting
          FLOAD = BLOAD                       ! fruit load
          CAPACITY = CARCAP_C                 ! cpacity
      ELSE                                    ! N is limiting
          FLOAD = BOLLZ                       ! fruit load
          CAPACITY = CARCAP_N                 ! cpacity
      ENDIF

C----- count days fruit load exceeds capacity ---------------------------------

      IF(IDAY-LAST_IDAY.GT.1)IDAYCO = 0       ! reset for break
      IF(FLOAD.GT.CAPACITY) IDAYCO = IDAYCO+1 ! count days FLOAD>CARCAP
      LAST_IDAY = IDAY                        ! last day counted
      IF(IDAYCO.LT.3)RETURN                   ! buffered by reserves

C----- compute excess fruit and buffer effect ---------------------------------

      EXCESS = FLOAD-CAPACITY                 ! bolls in excess of C supply
      EXCESS = EXCESS*0.1                     ! damp effect for carbon stress

C----- loop through arrays to find available fruit ----------------------------

      DO 2 L=LFRU(5),LFRU(8)-1,-1               ! loop through categories 5, 6, 7
        IF(L.LT.1) GO TO 1                    ! no fruit to abort
        ICAT = 4
        IF(L.LE.LFRU(5)) ICAT = 5
        IF(L.LE.LFRU(6)) ICAT = 6
        IF(L.LE.LFRU(7)) ICAT = 7
        IF(FRUNO(L).EQ.0.)GO TO 2                      ! no fruit
        AVAILABLE = FRUNO(L)                           ! fruit available to abort
        DO 10 M=1,6
            AVAILABLE = AVAILABLE-FRMARK(L,M)          ! less fruit marked
10      continue


        IF(ICAT.EQ.7.AND.FRUWT(L)/FRUNO(L).LT.0.1)THEN ! fruit not grown yet ?
           FRMARK(L,6) = FRUNO(L)                      ! abort such fruit
           AVAILABLE = AVAILABLE-FRMARK(L,6)           ! adjust fruit available
        ENDIF

        IF(AVAILABLE.GT.0.)THEN
          AVAILABLE = AVAILABLE*0.1                    ! damp effect
          ABORT = AVAILABLE                            ! abort available fruit
          IF(ABORT.GT.EXCESS) ABORT=EXCESS             ! limit to requirement
          FRMARK(L,6) = FRMARK(L,6)+ABORT
          FMKCAT(ICAT,6) = FMKCAT(ICAT,6)+ABORT
          EXCESS = EXCESS-ABORT                        ! reduce excess no. bolls
          IF(EXCESS.LE.0.) GO TO 1                     ! excess depleted
        ENDIF
2     continue

1     CONTINUE

C-------------------------------------------------------------------------------

      RETURN
      END


        real FUNCTION ozcot_satvp(T)
C
        real t

        real tabs, tr,trlog, ts, tt, ewslog, ew

        TABS=T+273.16
        TR=373.16/TABS
        TRLOG=ALOG10(TR)
        TR=TR-1.
        TS=(10.**(11.344*(1.-TABS/373.16))-1.)/10.**7.
        TT=(10.**(-3.49149*TR)-1.)/10.**3.
        EWSLOG=ALOG10(1013.246)
        EW=-7.90298*TR+5.02808*TRLOG-1.3816*TS+8.1328*TT+EWSLOG
        ozcot_satvp=10.**EW
        RETURN
        END
C
      real FUNCTION ozcot_senlf(BLOAD,ALAI,CARCAP_C,SMI)
C
C     ESTIMATES LEAF LONGEVITY. RANGES BETWEEN 833 DD & 1110 DD
C     REDUCED BY WATER STRESS, BOLL LOAD AND SELF SHADING OF
C     CANOPY WHEN LAI GT 3.
C
      real ozcot_stress
      real fb, fw, carcap_c, bload, alai, smi 

        FB=1.
        IF(CARCAP_C.GT.0.)FB=1.-BLOAD/CARCAP_C ! reduce by boll load
        IF(FB.GT.1.)FB=1.
        IF(FB.LT.0.)FB=0.
        FW=ozcot_stress(0.0,0.25,1.0,SMI) ! effect of water stress
      ozcot_senlf=833.+277.*FB*FW
      RETURN
      END


        SUBROUTINE ozcot_sevap(RAINSI)
C
C       ****** CALCULATE ACTUAL ES (USE EOS,UL1,TIME SINCE UL1) ******
C
      INCLUDE 'ozcot.inc'
      real t, esx, rainsi
C
        IF(SUMES1.GE.UL1) GO TO 180
        IF(RAINSI.GE.SUMES1) GO TO 120
        SUMES1=SUMES1-RAINSI
        GO TO 140
120     SUMES1=0.
140     SUMES1=SUMES1+EOS
        IF(SUMES1.LT.0.0) SUMES1=0.0
        IF(SUMES1.GT.UL1) GO TO 160
        ES=EOS
        GO TO 260
160     ES=EOS-0.4*(SUMES1-UL1)
        SUMES2=0.6*(SUMES1-UL1)
        T=(SUMES2/CONA)**2
        GO TO 260
180     IF(RAINSI.LT.SUMES2) GO TO 200
        RAINSI=RAINSI-SUMES2
        SUMES1=UL1-RAINSI
        SUMES2=0.0
        T=0.
        IF(RAINSI.GT.UL1) GO TO 120
        GO TO 140
200     T=T+1.
        ES=CONA*T**0.5-SUMES2
        IF(RAINSI.GT.0.) GO TO 220
        IF(ES.GT.EOS) ES=EOS
        GO TO 240
220     ESX=0.8*RAINSI
        IF(ESX.LE.ES) ESX=ES+RAINSI
        IF(ESX.GT.EOS) ESX=EOS
        ES=ESX
240     SUMES2=SUMES2+ES-RAINSI
        T=(SUMES2/CONA)**2
260     IF(ES.LT.0.) ES=0.
        RETURN
        END


c     subroutine SOLWAT (I)
      SUBROUTINE ozcot_solwat
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      CALCULATES THE SOIL WATER BALANCE AS A FUNCTION OF SOIL     C
C      LAYER DEPTH.  MODIFIED FROM THE MODEL OF RITCHIE (1972).    C
C                                                                  C
C      KEY VARIABLES:                                              C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      INCLUDE 'ozcot.inc'
      real ozcot_watco
      real depth, rtul, swi
c      real t, rdef
      integer l
c
c      DIMENSION STOR(20) ,U(20)
c      DATA JIR/1/ ! index for current element of IGDAY
c      DATA INITIAL/0/ ! flag for initial call

cpsc      do 99 ll=1,nlayr
cpsc 99       dlayr(ll)=dlayr(ll)/10.

C-------INITIALISING -----------------------------------------------------------

c        IF(I.EQ.1) JIR = 1            ! irrigation counter for OZCOT2
        IF(ISW.NE.1) THEN             ! replaces INITIAL.GT.1 for OZCOT2&4 May89
            ISW = 1
c            T=0.
            SMI=SWLAYR(1)/ULLAYR(1)
c           IF(SMI.LT.0.9 ) THEN
c               SUMES1=UL1
c               SUMES2=2.5-2.78*SMI
c           ELSE
c                SUMES1=10.-SMI*10.
c               SUMES2=0.
c            ENDIF
        ENDIF

C------ REMOVE WATER FROM LAYERS 1&2 DURING LAND PREPARATION -------------------

c       JTEST = JDATE-(JDATE/30)*30          ! test if 30th day
C       IF(ISOW.EQ.0 .AND. JTEST.EQ.0) CALL CULTIVATE (0.25,1.0)
c       IF(JDATE.EQ.220) CALL CULTIVATE (0.0,0.5)

C------ ADD IRRIGATION WATER ---------------------------------------------------

c       RAINSI=RAIN
c       IF(JDATE.EQ.IGDAY(JIR))THEN
c         DEFIRG=DEF
c         RAINSI=RAIN+DEFIRG
c         JIR=JIR+1
c       END IF

C------ CALCULATE RUNOFF -------------------------------------------------------

c       SW=SW+RAINSI
c       IF(SW.LE.UL) THEN
c           Q=0.0
c       ELSE
c           Q=SW-UL
c        ENDIF
cpsc         q=runoff
c        RAINEF=RAINSI-Q
c       CALL SEVAP(RAINSI)

C------ CALCULATE POTENTIAL EP -------------------------------------------------
C-------light interception modified to give hedgerow effect with skip row - ABH 5/11/96 ------      
cpsc      
        ALAI = ALAI*RS                   ! lai in hedgerow

        IF(ALAI.GT.3.) THEN
            EP=EO-ES
        ELSE IF(ALAI.GT.1.6)THEN
            EP=(0.08+0.3066*ALAI)*EO     ! L.Mateos 1987, ABH 1988
        ELSE
            EP=(1.-EXP(-0.5186*ALAI))*EO ! L.Mateos 1987, ABH 1988
        ENDIF

        IF(ALAI.EQ.0.0) EP=0.0
        IF(EP.LT.0.) EP=0.
cpsc
        EP = EP/RS                       ! EP on ground area basis
        ALAI = ALAI/RS                   ! restore LAI to ground area basis

C------ LIMIT EP USING WATCO(SMI) STRESS FACTOR --------------------------------

        ET=ES+EP
        IF(EO.LT.ET) THEN
            ET=EO
            EP=ET-ES
            IF(EP.LT.0.0)EP=0.0
        ENDIF
        EP=EP*ozcot_watco(SMI,EO,0.4,0.)
        ET=ES+EP

        RRIG(8) = RRIG(8) + ET ! accumulate Et for output

c       CALL SWBAL(I,RAINEF)
        CALL ozcot_swbal

C------ CALCULATE TOTAL SW BEFORE EP FOR SR: SNBAL ----------------------------

c       SWWEP=0.0
c       DO 10 L=1,NLAYR
c         SWWEP=SWWEP+SWLAYR(L)*DLAYR(L)
c         SWEPLR(L)=SWLAYR(L)
c10      continue

C---- CALCULATE SOIL WATER STRESS FACTOR 'SMI'  ---------------------------------

c       IF(I.LT.ISOW.OR.ISOW.EQ.0) GO TO 500
        IF(das.le.0.OR.ISOW.EQ.0) GO TO 500
        DEPTH=0.
        RTSW=0.
        RTUL=0.

        DO 20 L=1,NLAYR
          DEPTH=DEPTH+DLAYR_cm(L)
          IF(RTDEP.LE.DEPTH) GO TO 460
          RTUL=RTUL+ULLAYR(L)*DLAYR_cm(L)
          RTSW=RTSW+SWLAYR(L)*DLAYR_cm(L)
20      continue
        GO TO 480

460     RTSW=RTSW+SWLAYR(L)*(RTDEP+dlayr_cm(L)-DEPTH)
        RTUL=RTUL+ULLAYR(L)*(RTDEP+dlayr_cm(L)-DEPTH)
        SWI=SWLAYR(1)/ULLAYR(1)
        IF(dlayr_cm(1).GE.30.) GO TO 480
        SWI=
     *  (SWI*dlayr_cm(1)+(SWLAYR(2)/ULLAYR(2))*(30.-dlayr_cm(1)))/30.
480     SMI=AMAX1(RTSW/RTUL,SWI)

cpc
        smi = RTSW/RTUL

C        IF(RS.GT.1. .AND. RTDEP.LT.RTDEPM) THEN  ! when RS > 1m & roots growing,
C            SMI = 1-(1-SMI)*(TR/RS+1-TR)*RS      ! adjust for lateral root
C            IF(SMI.LT.0.) SMI = 0.
C        ENDIF                                    ! growth, limited to 1m row

C------ CALCULATE SOME QUANTITIES FOR WRITING ONLY -----------------------------

c       RTSMI=RTSW/RTUL
c
c       FLIM=ozcot_watco(SMI,eo,0.3,0.)
cpc     RDEF=RTUL-RTSW
c       RPERD=(RDEF/RTUL)*100.0

  500 CONTINUE

C------ CALCULATE TOTAL SOIL WATER & DEFICIT -----------------------------------

        SW=0.0
        DO 30 L=1,NLAYR
            SW=SW+SWLAYR(L)*dlayr_cm(L)
            IF(L.LE.6)TSWL(L)=(SWLAYR(L)+UNUL(L))*dlayr_cm(L)
cpsc
cpsc            swdep(l)=swlayr(l)*10.*dlayr_cm(l)+
cpsc     *                 (duldep(l)-ullayr(l)*10.*dlayr_cm(l))
cpsc

30      continue
        DEF=UL-SW

cpsc      do 199 ll=1,nlayr
cpsc 199       dlayr_cm(ll)=dlayr_cm(ll)*10.

        RETURN
        END


c      SUBROUTINE CULTIVATE (C_LYR1,C_LYR2)

C     reduces soil water content of layers 1 and 2 in response to cultivation
C     to levels passed in C_LYR1 & C_LYR2. If these are 1.0, no reduction as
C     CULTSW will be > SWLAYR

c      INCLUDE 'ozcot.inc'

c      CULTSW = ULLAYR(1)*C_LYR1          ! soil water content after cultivation
c      IF(CULTSW.LT.SWLAYR(1)) SWLAYR(1) = CULTSW
c      CULTSW = ULLAYR(2)*C_LYR2          ! soil water content after cultivation
c      IF(CULTSW.LT.SWLAYR(2)) SWLAYR(2) = CULTSW
c      RETURN
c      END


c      SUBROUTINE SOWDAY (I,IEND)
       SUBROUTINE ozcot_sowday

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      DERIVES A SOWING DATE IF ONE IS NOT GIVEN AS AN INITIAL     C
C      CONDITION.  SOWING REQUIRES 3 CONSECUTIVE DAYS WITH SOIL    C
C      TEMPERATURES >= 15.0C AND SOIL MOISTURE SUMES1 >= UL1       C
C      AT SOWING, ROOT DEPTH IS ASSIGNED SEED DEPTH UNTIL          C
C      EMERGENCE.                                                  C
C                                                                  C
C      KEY VARIABLES:                                              C
C
C      ISOW = sowing date (as day of year); if input > 0 or after
C             selected in this S/R, S/R SOWDAY not called;
C             if < 0 (-ve), this S/R will not look for a owing day
C             before -ISOW (i.e. the -ve value made +ve).
C      IWINDOW = time window for sowing when selected in this S/R,
C                starting with earliest day
C      TEMPRE = mean temp day before yesterday
C      TEMPYD = mean temp yesterday
C      TMEAN  = mean temperature for last three days, when > 18
C               soil temp > 15 for three successive days - ABH 8/8/89
C      YR1_LAST = flag to show season started last
C               year or this.
C      WINDOW = logical flag to show if in sowing window
C      SPAN_NU_YR = logical flag to show if new year started in sowing window
C      LASTYR = needed when sowing window spans new year
C               for number of days in previous year;
C               acts as a flag to show if SOW_THIS_YR to be set to 0 or 1.
C      NOSOW  = flag to show if crop to be sown this year, mainly for
C               long fallowing with dry land cropping;
C               -1 for no crop, 1 for crop.
C      ISWITCH= toggle switch to change NOSOW.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      INCLUDE 'ozcot.inc'

c      LOGICAL WINDOW/.FALSE./
c      LOGICAL WARM/.FALSE./, WET/.FALSE./
c      LOGICAL TRAFFIC/.FALSE./, PROFILE/.FALSE./
c      logical span_nu_yr
c      logical yr1_last
c      DIMENSION NPM(12)
c      DATA NPM /0,31,59,90,120,151,181,212,243,273,304,334/
c      DATA TEMPYD/0.0/,TEMPRE/0.0/
c      DATA INITIAL/0/,LASTYR/0/

c      IF(ISOW.GT.0) RETURN

C---------------- initial conditions set on first call -------------------------

c      IF(INITIAL.EQ.0 .AND. ISDY.LT.0) THEN  ! 1st time SOWDAY called?
c          INITIAL = 1                        ! set flag
c          JSTART = NPM(ISMO)-ISDY            ! earliest day of year for sowing
c          IF(JSTART.GT.MDPY) JSTART=JSTART-MDPY ! into next year
c          JSTOP  = JSTART+IWINDOW            ! latest day to sow (can be >365)
c         ISWITCH = 1                        ! crop every year
c         IF(ISYR.LT.0) ISWITCH = -1         ! crop alternate years
c          NOSOW = 1                          ! crop every yr or even yrs if alt
c          IF(ISYR.EQ.-2) NOSOW = -1          ! crop in odd years
C          YR1_LAST = .TRUE.                  ! season starts last year
c      END IF

C-------------- reset on first call of new season ------------------------------

c      IF(I.EQ.1) THEN                        ! 1st day of season? if so...
c          NOSOW=NOSOW*ISWITCH                ! toggle NOSOW for fallow/crop
c          WINDOW = .FALSE.                   ! reset sowing window flag
c          SPAN_NU_YR = .FALSE.               ! reset
c          LASTYR = 0                         !  reset  for next season
c          TEMPRE = 0.                        !  reset  for next season
c          TEMPYD = 0.                        !  reset  for next season
c      ENDIF


c      IF(NOSOW.EQ.-1) THEN                   ! if no crop this season (fallow)..
c          NCOUNT = NCOUNT+1                  ! count days of fallow season
c          IF(NCOUNT.LT.365)RETURN            ! end of fallow season?
c          IEND=4                             ! yes, end of fallow season
c          NCOUNT=0                           ! reset counter
c          RETURN
c      END IF

C---------- compute sowing conditions -----------------------------------------

      TMEAN3 = (TEMPAV+TEMPYD+TEMPRE)/3. ! mean temperature for last 3 days

      TEMPRE = TEMPYD                   ! update previous days temp for tomorrow
      TEMPYD = TEMPAV                   ! update yesterdays temp for tomorrow

c      IF(TMEAN.GE.18.0) THEN            ! check soil temperature
c          WARM = .TRUE.                 ! soil warm enough to sow
c      ELSE
c          WARM = .FALSE.                ! soil not warm enough to sow
c      ENDIF

      S_BED_MI = SWLAYR(1)/ULLAYR(1)    ! seed bed moisture index

      s_bed_sat = max(s_bed_sat,s_bed_mi)

c      IF(S_BED_MI.GE.0.75) THEN         ! seed bed saturated?
c          WET = .TRUE.                  ! seed bed has been wetted
c      ELSE
c          IF(S_BED_MI.LT.0.5) WET = .FALSE. ! seed bed has dried out
c      ENDIF

c      IF(S_BED_MI.LE.0.67) THEN         ! trafficable? Bridge & Muchow 1982
c          TRAFFIC = .TRUE.              ! seed bed trafficable
c      ELSE
c          TRAFFIC = .FALSE.             ! seed bed NOT trafficable
c      ENDIF

c      IF(SW.GE.SOW_SW) THEN             ! soil water content of profile
c          PROFILE = .TRUE.              ! sufficient water in profile
c      ELSE                              ! 11cm is for 1m from Fawcet 1977
c          PROFILE = .FALSE.             ! insufficient water in profile
c      ENDIF

C------------- check if sowing window open--------------------------------------

c      IF(.NOT. WINDOW) THEN                  ! sowing window yet?
c          IF(JDATE.EQ.1.AND.I.NE.1) YR1_LAST=.TRUE. ! window starts in new year
c          IF(JDATE.EQ.JSTART) THEN           ! ....starts today?
c              WINDOW = .TRUE.                ! if so, set flag
c          ELSE                               ! if not, ....
c              RETURN                         ! ...return to avoid start in
c          ENDIF                              ! middle of window
c      ENDIF

c      IF(JDATE.EQ.1) THEN                    ! window spans new year?
c          SPAN_NU_YR = .TRUE.                ! set flag
c          LASTYR = 365                                ! days in last year
c          IF((((IMYR-1)/4)*4).EQ.(IMYR-1)) LASTYR=366 ! leap year
c      END IF
c      JTRY = JDATE+LASTYR                    ! allows window to span new year

c      IF(JTRY.GE.JSTOP) THEN                 ! passed the sowing window?
c          IEND = 3                           ! if so, set flag
c          WINDOW = .FALSE.
c          RETURN                             ! no crop this season
c      END IF

C------------- check if sowing conditions satisfied ----------------------------

c      IF(WARM .AND. WET .AND. TRAFFIC .AND. PROFILE) THEN ! all conditions met?
c          ISOW=I+1                       ! sow tomorrow
c          RRIG(2) = SW                   ! soil water at sowing
c          WINDOW = .FALSE.
c      ENDIF

      RETURN
      END


        real FUNCTION ozcot_stress(LOW,HIGH,A,STRS)
        real HIGH,A,STRS
C
C       computes or adjusts a factor.
C       input is state variable STRS with upper and lower limits, HIGH,LOW.
C       output is between 0 and 1.
C       A =  1 gives factor a linear fn of ratio of STRS to HIGH - LOW
C       A GT 1 accentuates effect of STRS on value
C       A LT 1 damps effect.
C
        REAL LOW
        ozcot_stress=(STRS-LOW)/(HIGH-LOW)
        IF(ozcot_stress.GT.1.)ozcot_stress=1.
        IF(ozcot_stress.LT.0.)ozcot_stress=0.
        ozcot_stress=(1.-(1.-ozcot_stress)**A)
        RETURN
        END
      real FUNCTION ozcot_survive(CAPACITY,BLOAD)
      real CAPACITY,BLOAD
      real a,b
C
C     ESTIMATES SURVIVAL OF FRUIT AS FUNCTION OF BOLL LOAD.
C
      ozcot_survive = 0.
      IF(CAPACITY.EQ.0.)RETURN
      A=1.0 ! intercept of survival function
      B=A/CAPACITY ! slope of survival function
      ozcot_survive=A-B*BLOAD ! prortion surviving
      IF(ozcot_survive.LT.0.)ozcot_survive=0.
      IF(ozcot_survive.GT.1.)ozcot_survive=1.
      ozcot_survive = ozcot_survive*0.8  ! background, sub-threshold shedding
      RETURN
      END


c       SUBROUTINE SWBAL(I,RAINEF)
        SUBROUTINE ozcot_swbal

C
C   **** SOIL & PLANT WATER BALANCE INCLUDING RAIN AND SOIL EVAPORATION, ****
C   **** BEGINNING WITH THE TOP SOIL LAYER.                      ****
C
      INCLUDE 'ozcot.inc'
      real ux, epcoef, uob, sum, stran, depth, tdepth, epd, swlr
      real dswmx
      integer l

c
c       PERCOL=RAINEF
c       EXES=ES
        UX=0.0
        IF(SMI.GE.0.5)EPCOEF=3.051              !  W*N
        IF(SMI.LT.0.5)EPCOEF=2.436              !  82/83
        UOB=EP/(1.-EXP(-EPCOEF))
        SUM=0.0
        STRAN=0. ! sum of TRANS(L) down profile
        DEPTH=0.
        TDEPTH=0.
        EPD=0.0
c       W=0.
C
C***********************************************************************
C
          DO 10 L=1,NLAYR
cpsc
cpsc          swlayr(l)=swdep(l)/10./dlayr_cm(l)
cpsc     *                 -(duldep(l)/10./dlayr_cm(l)-ullayr(l))
cpsc
          TRANS(L)=0.
            SWLR=SWLAYR(L)*dlayr_cm(L)
c           SWMAX=ULLAYR(L)*dlayr_cm(L)
c           IF(PERCOL.EQ.0..AND.EXES.EQ.0.)GO TO 2
C
C**** RAIN PERCOLATES LAYER 'L'
C
c       SWLR=SWLR+PERCOL
c       PERCOL=0.
c       IF(SWLR.LE.SWMAX)GO TO 1
c       PERCOL=SWLR-SWMAX ! SURPLUS PERCOLATES TO NEXT LAYER
c       SWLR=SWMAX        ! THIS LAYER FULL
C
C**** EXTRACT ES FROM LAYER 'L'
C
c 1     SWLR=SWLR-EXES    ! EXTRACT ES FROM THIS LAYER
c       EXES=0.
c       IF(SWLR.GE.0.)GO TO 2
c       EXES=-SWLR        ! ES CARRIED DOWN TO NEXT LAYER
c       SWLR=0.
C
C**** EXTRACT EP FROM THIS LAYER
C
c2        CONTINUE
          DEPTH=DEPTH+dlayr_cm(L)           ! moved here 29/6/88
c         IF(I.LT.IEMRG) DEPTH=0.0       !       ditto
          IF(das.LT.IEMRG) DEPTH=0.0       !       ditto
          IF(RTDEP.LE.TDEPTH) GO TO 11   !       ditto
          IF(RTDEP.LT.DEPTH) DEPTH=RTDEP !       ditto
          SUM=UOB*(1.-EXP(-EPCOEF*DEPTH/RTDEP))
          DSWMX=SUM-UX
          SWLR=SWLR-DSWMX    ! EXTRACT EP FROM THIS LAYER
          IF(SWLR.GE.0.) GO TO 3
          EPD=SWLR
          SWLR=0.
          DSWMX=dlayr_cm(L)*SWLAYR(L)
3         TRANS(L)=DSWMX
          TDEPTH=DEPTH
          UX=SUM+EPD   ! EPD CORRECTS UX IF LAYER IS DRY
          EPD=0.0
11        CONTINUE ! moved from after next statement 29 Jun 88
          SWLAYR(L)=SWLR/dlayr_cm(L)
          STRAN=STRAN+TRANS(L)
          SETLYR(L)=SETLYR(L)+STRAN ! cumulative transp. thro season down profile
10      CONTINUE
        RETURN
        END

C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      WATER STRESS FUNCTION FOR ROOT GROWTH.                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      real FUNCTION ozcot_watco(SMI,EO,X3,X1)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      WATER STRESS FUNCTION FOR ROOT GROWTH.                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      real SMI,EO,X3,X1
      real x2, y0, slope
      X2=X3*EO/0.8
      IF(X2.GT.1.0) X2=1.0
      IF(SMI.LT.X2) GO TO 20
      ozcot_watco=1.0
      GO TO 30
C
   20 CONTINUE
      SLOPE=1.0/(X2-X1)
      Y0=1.0-SLOPE*X2
      ozcot_watco=SLOPE*SMI+Y0
      IF(ozcot_watco.LT.0.0) ozcot_watco=0.0
C
   30   CONTINUE

        RETURN
        END


c      SUBROUTINE YIELD(NSZN,IEND)
      SUBROUTINE ozcot_yield

C     Estimates yield and gross margin at end of season

      INCLUDE 'ozcot.inc'
      real bollsc

C     calculate yield **********************************************************

      ALINT = OPENWT*10.*PCLINT          ! g sc/m to kg lint/ha
cpsc      ALINT = ALINT/RS                   ! adjust for row spacing 2/5/90

c      PLNTZ = UPTAKN                     ! nitrogen uptake
cpc   BOLLSC = 0.
cpc   IF(OPENZ.GT.0.) BOLLSC = OPENWT/OPENZ ! g sc/boll
      RRIG(7) = RRIG(3) + RRIG(4)        ! commulative rain pre + post boll

      call ozcot_residues                      ! to calculate stem and root residues
C     calculate gross margin ***************************************************

C     currently redundant!!!!

c      COT_PRICE = 2.                     ! $ per kg
c      WAT_COST = 12.                     ! $ per Ml pumped
c      SPRAY_COST =2.                     ! $ day of protection

c      COT_PRICE = 2.                      ! cotton price $ per kg
C      GRO_COST = 1200.                    ! growing costs - irrigated
c      GRO_COST = 450.                     ! growing costs - rain grown
c      WAT_COST  = 12.                     ! $ per Ml (pumping & carges)
c      SPRAY_COST = 2.                     ! $ per day of protection
c      COT_INC = ALINT*COT_PRICE           ! cotton income
c      WAT_EXP = RRIG(2)/10.*WAT_COST      ! water expenditure
c      SPRAY_SAVE = (150-ILAIZ)*SPRAY_COST ! saving of spray cost
c      GROSS_MARG = COT_INC - GRO_COST - WAT_EXP -  SPRAY_SAVE ! gross margin

C     sowing date for output ***************************************************

c      JSOW = ISOW+IMET-1                  ! sowing date in J time
c      MDPY_PREV = 365
c      IF((IMYR-1)/4*4 .EQ. IMYR_1) MDPY_PREV = 366
c      IF(JSOW.GT.MDPY_PREV) JSOW = JSOW - MDPY_PREV

C     This section for when crop not sown **************************************

c      IF(IEND.GE.3) THEN
c          JSOW = 0                        ! crop not sown, window passed or fallow
c          GROSS_MARG = 0.0                ! gross margin nil
c      END IF

C     This section does stuff needed with 1st call of YIELD ********************


c      IF (IMYR.GT.1800) THEN              ! year in 10s or 100s?
c          IMYR = IMYR-1800                ! change year to 10s
c          IF(IMYR.GE.100) IMYR=IMYR-100   ! for 1900s
c      ENDIF                               ! above done for output

c      IF(NSZN.EQ.1) THEN                     ! first season

c          OPEN(3,file='YIELD.OUT',status='unknown')
c          WRITE(3,7772) TITLE

c          IF(MODE.LE.1) THEN                  ! validation or calibration
c              WRITE(3,600)
c          ELSE                                ! simulation for strategy etc
c              IF(DEFIRR(2).GT.0.) THEN        ! irrigated crop
c                  WRITE(3,700)
c              ELSE                            ! rain-fed crop
c                  WRITE(3,800)
c              ENDIF
c          ENDIF

c          IF(JDATE.LT.244) THEN               ! last year was year of sowing
c              IYR1 = IMYR-1                   ! year sown in first season
c              IYR2 = IMYR                     ! year harvested first season
c          ELSE                                ! this year was year of sowing,
c              IYR1 = IMYR                     ! year of sowing
c              IYR2 = IMYR+1                   ! year of harvest
c          ENDIF

c      ELSE                                    ! not first season

c              IYR1 = IYR1+1                   ! year of sowing of this season
c              IYR2 = IYR2+1                   ! year of harvest of this season


c      ENDIF

c      IF(IYR1.EQ.100) IYR1 = 0                 ! new century
c      IF(IYR2.EQ.100) IYR2 = 0                 ! new century

C    fallow soil water *********************************************************


c      IF(JSOW.GT.0) THEN    ! crop sown; deal with fallow SW gain
c          GAIN = RRIG(2)-RRIG(6) ! gain in SW during fallow
c          FRACTION = 0.0    ! fraction of rainfall conserved by fallow
c          IF(RRIG(5).NE.0.) FRACTION = GAIN/RRIG(5)
C          WRITE(4,999) IYR1,IYR2,JSOW,RRIG(6),RRIG(2),GAIN,RRIG(5),FRACTION
c999       FORMAT(3I4,5F8.2) ! yrs, sow d, initial SW, final SW, gain, rain, fraction
c          RRIG(5) = 0.0     ! reset fallow rain. DO NOT reset in RESET!
c          RRIG(6) = SW      ! initial SW for next fallow. DO NOT reset in RESET!
c      ENDIF

C     write output *************************************************************

c      IF(MODE.LE.1) THEN                  ! validation or calibration
c          WRITE(3,7770) IYR1,IYR2,JSOW,OPENZ,ALINT,BOLLSC,
c     *    ALAIZ,PLNTZ,ILAIZ,SQZX,ISQZX
c          WRITE(2,7770) IYR1,IYR2,JSOW,OPENZ,ALINT,BOLLSC,
c     *    ALAIZ,PLNTZ,ILAIZ,SQZX,ISQZX
c      ELSE                                ! simulation for strategy etc
c          IF(DEFIRR(2).GT.0.) THEN        ! irrigated crop
c              WRITE(3,7771) IYR1,IYR2,JSOW,OPENZ,ALINT,BOLLSC,
c     *        ALAIZ,PLNTZ,ILAIZ,IFIX(RRIG(1)),RRIG(2),RRIG(7),RRIG(8)
C     *        ALAIZ,PLNTZ,ILAIZ,IFIX(RRIG(1)),RRIG(2),RRIG(7),DEF_LAST ! Norn D
c              WRITE(2,7771) IYR1,IYR2,JSOW,OPENZ,ALINT,BOLLSC,
c     *        ALAIZ,PLNTZ,ILAIZ,IFIX(RRIG(1)),RRIG(2),RRIG(7),RRIG(8)
C     *        ALAIZ,PLNTZ,ILAIZ,IFIX(RRIG(1)),RRIG(2),RRIG(7),DEF_LAST ! Norn D
c          ELSE                            ! rain-fed crop
c              WRITE(3,7771) IYR1,IYR2,JSOW,OPENZ,ALINT,BOLLSC,
c     *        ALAIZ,PLNTZ,ILAIZ,IFIX(RRIG(1)),RRIG(2),RRIG(3),RRIG(4)
c              WRITE(2,7771) IYR1,IYR2,JSOW,OPENZ,ALINT,BOLLSC,
c     *        ALAIZ,PLNTZ,ILAIZ,IFIX(RRIG(1)),RRIG(2),RRIG(3),RRIG(4)
c          ENDIF
c      ENDIF

      RETURN
C
c600   FORMAT(' YEAR SOWN  BOLLS/M  LINT SC/BOLL MAX_LAI  N_UPTK DAY
c     *   SQZ DAY')
c700   FORMAT(' YEAR SOWN  BOLLS/M  LINT SC/BOLL MAX_LAI  N_UPTK DAY
c     * No  WATER  RAIN  CUM_ET')
C     * No  WATER  RAIN DEF_L')                                    ! for Norm D
c800   FORMAT(' YEAR SOWN  BOLLS/M  LINT SC/BOLL MAX_LAI  N_UPTK DAY
c     * No  WATER RAIN1  RAIN2')
c7770  FORMAT(X,2I2,I4,F8.1,F8.0,2F8.2,F8.0,I4,F6.1,I4)
c7771  FORMAT(X,2I2,I4,F8.1,F8.0,2F8.2,F8.0,I4,I3,3F7.1)
c7772  FORMAT(15A4)

      END


      SUBROUTINE ozcot_plant_n
C     Call from PLTGRW before ozcot_cropn
C     calculates nitrogen content of dry matter increments and sums them
C     adjusts N increments as soil N supply diminishes
C     variable dDW_LEAF etc from S/R DRY_MATTER
C              SUPPLY_N from system
C               dN_PLANT = daily increment of plant N to system

      INCLUDE 'ozcot.inc'
      real ozcot_stress
      real supply_n
      real conc_l, conc_s, conc_r, conc_b
      real dn_leaf, dn_stem, dn_root, dn_boll
      real sup_dem, adjust
      data supply_n/2.0/

      DATA CONC_L/0.04/, CONC_S/0.02/, CONC_R/0.02/, CONC_B/0.015/

C      Calculate daily increment for components of dry matter

      dN_LEAF = dDW_LEAF * CONC_L   ! leaf nitrogen
      dN_STEM = dDW_STEM * CONC_S   ! stem nitrogen
      dN_ROOT = dDW_ROOT * CONC_R   ! root nitrogen
      dN_BOLL = dDW_BOLL * CONC_B   ! boll nitrogen

      dN_PLANT = dN_LEAF + dN_STEM + dN_ROOT + dN_BOLL ! plant N increment

C      Adjust uptake when soil supply limiting

      if (dN_PLANT.gt.0.) then
        SUP_DEM = SUPPLY_N/dN_PLANT   ! supply/demand ratio
      else
        SUP_DEM = 0.
      endif
      ADJUST = ozcot_stress(0.,5.0,1.0,SUP_DEM) ! factor to adjust
      IF(ADJUST.LT.1.0) THEN
          dN_LEAF = dN_LEAF * ADJUST   ! leaf nitrogen adjusted
          dN_STEM = dN_STEM * ADJUST   ! stem nitrogen adjusted
          dN_ROOT = dN_ROOT * ADJUST   ! root nitrogen adjusted
          dN_BOLL = dN_BOLL * ADJUST   ! boll nitrogen adjusted
          dN_PLANT = dN_LEAF + dN_STEM + dN_ROOT + dN_BOLL ! plant N increment
      ENDIF

      TOTAL_N =TOTAL_N + dN_PLANT      ! accumulate uptake for season

C     compare accumulated uptake with projected uptake for season
C     if accumulated exceeds projected, assume requirements met by remobilisation
C     and set this day's increments to zero

      IF(TOTAL_N.GE.UPTAKN/10.) THEN
          TOTAL_N =TOTAL_N - dN_PLANT  ! adjust uptake for season
          dN_LEAF = 0.0                ! leaf nitrogen adjusted
          dN_STEM = 0.0                ! stem nitrogen adjusted
          dN_ROOT = 0.0                ! root nitrogen adjusted
          dN_BOLL = 0.0                ! boll nitrogen adjusted
          dN_PLANT = 0.0               ! plant N increment
      ENDIF


c      write(4,222) iday,supply_n,dn_plant,total_n,uptakn,dw_total
c222   format(i5,5f8.3)


      RETURN
      END

      SUBROUTINE ozcot_residues

C      Called from s/r yield to calculate stem and root residues

      INCLUDE 'ozcot.inc'
      real conc_res

      DATA CONC_RES/0.005/           ! N concentration of residues

      STEM_RES = DW_STEM             ! stem residues dry matter
      STEM_RES_N = DW_STEM*CONC_RES  ! N content of stem residues
      ROOT_RES = DW_ROOT             ! root residues dry matter
      ROOT_RES_N = DW_ROOT*CONC_RES  ! N content of root residues

      RETURN
      END



c     SUBROUTINE DRY_MATTER (I)
      SUBROUTINE ozcot_dryxmatter

C     This subroutine is for OZCOT6 (version 6).

C     First demand for assimilate is estimated for leaf, stem, root and boll.
C     Leaf demand is determined from potential increase in area. Water stress
C     may reduce actual area increase, thus giving thicker leaves. Inadequate
C     assimilate supply may reduce actal weight increase giving thinner leaves.
C     Upper and lower limits are set by leaf weight:area ratio. If the upper
C     limit is exceeded, weight increase is reduced. If lower limit is not
C     reached, area increase is reduced. Stem and root demand are determined
C     from potential leaf demand using allometric relationships.

C     Calls S/R ASSIMILATION to compute assimilate production for the day.
C     The supply available to meet demand is obtained from sum of the days
C     assimilate production and any reserves.

C     Supply and demand are then compared in order to partition dry matter
C     increase between leaf, stem, boll and root.
C     If supply exceeds demand, up to two days supply can be stored as reserves.
C     If demand exceeds supply, distribution is simulated on the basis
C     of proximity to the source of supply. Leaf, stem and fruit are assumed to
C     be equidistant from the source. If supply exceeds the sum of leaf, stem
C     and fruit demand, their needs are met in full and the balance goes to
C     root. If the sum of leaf, stem and fruit demand exceeds supply,
C     their needs are met in proportion to the supply/demand ratio and the
C     root receives none. The root supply:demand ratio or a decline in root
C     growth provide feedback to reduce increase in root depth in S/R PLTGRW.

C     Local variables:
C       ASSIMILATE new dry matter passed daily from S/R ASSIMILATION
C       dDW_BOLL   day's increase in boll dry weight
C       dDW_LEAF   day's increase in leaf dry weight
C       dDW_STEM   day's increase in stem dry weight
C       dDW_ROOT   day's increase in root dry weight
C       dDW_ROOT_MAX   maximum value of increase in root dry weight
C       DEMAND     demand for assimilate for potential day's growth
C       FNSTRS2    N stress for boll growth - on/off
C       FWSTRS     water stress for boll growth
C       SD_RATIO   supply:demand ratio for leaf, stem and boll growth
C       SD_ROOT    supply:demand ratio for root growth
C       STRSBL     stress factor for boll growth, minimum of N and water
C       SUPPLY     day's supply of assimilate available for potential growth
C       WT_AREA    leaf weight:area ratio

      implicit none
      include 'ozcot.inc'
      include 'data.pub'

      real ozcot_stress
      real wt_area, fwstrs, fnstrs2
      real strsbl, assimilate, supply, demand
      real sd_ratio, sd_root, ddw_root_max
C------------------------------------------------------------------------------
C     initialise leaf, stem and root dry matter at start of new crop
C------------------------------------------------------------------------------

      IF(DW_LEAF.EQ.0.) THEN                ! leaf weight initialise to zero?
          DW_LEAF = EMBRYO*F_LEAF*PPM       ! initial leaf dry weight per m
          DW_STEM = EMBRYO*F_STEM*PPM       ! initial stem dry weight per m
          DW_ROOT = EMBRYO*F_ROOT*PPM       ! initial root dry weight per m
      ENDIF

C------------------------------------------------------------------------------
C     calculate demand (potential growth) for leaf, stem and root
C------------------------------------------------------------------------------

      dDW_LEAF = DLAI_POT*SPECIFIC_LW                     ! leaf demand
      dDW_STEM = divide (A_STEM_LEAF*dDW_LEAF*DW_STEM
     :                  ,DW_LEAF, 0.0)                    ! ditto for stem
      dDW_ROOT = divide (A_ROOT_LEAF*dDW_LEAF*DW_ROOT
     :                  ,DW_LEAF, 0.0)                    ! ditto for root

C------------------------------------------------------------------------------
C     feed back of leaf weight/area ratio
C------------------------------------------------------------------------------

      IF(DLAI(IDAY).GT.0.) THEN                           ! leaf growth today?
          WT_AREA = divide (dDW_LEAF, DLAI(IDAY), 0.0)    ! leaf weight/are ratio
          IF(WT_AREA.GT.WT_AREA_MAX) THEN                 ! too thick
              dDW_LEAF = DLAI(IDAY)*WT_AREA_MAX           ! reduce weight
C          ELSE IF(WT_AREA.LT.WT_AREA_MIN) THEN            ! too thin
C              DLAI(IDAY) = dDW_LEAF/WT_AREA_MIN           ! reduce area
          ENDIF
      ENDIF

C------------------------------------------------------------------------------
C     calculate demand for bolls
C------------------------------------------------------------------------------

c      IF(ISQ.GT.0 .AND. I.GE.ISQ+2) THEN        ! FRUIT called yet?
      IF(ISQ.GT.0 .AND. das.GE.ISQ+2) THEN        ! FRUIT called yet?
          FWSTRS = ozcot_stress(0.0,0.5,4.0,SMI)      ! water stress on bolls
          FNSTRS2 = 1.                          ! N stress for bolls off
          IF(FNSTRS.EQ.0.) FNSTRS2 = 0.         ! N stress for bolls on
          STRSBL = AMIN1(FWSTRS,FNSTRS2)        ! minimum of water or N stress
          BOLLGR = SCBOLL(IVAR)*BPER*FBURR      ! boll gr rate this day
          BOLLGR = BOLLGR*STRSBL                ! adjust for stress
          IF(BOLLGR.LT.0.) BOLLGR = 0.
          dDW_BOLL = BOLLZ*BOLLGR               ! boll demand - potential growth

      ENDIF

C------------------------------------------------------------------------------
C   determine supply of assimilate
C------------------------------------------------------------------------------

c      CALL ASSIMILATION(ASSIMILATE,I)                  ! day's assimilate
      CALL ozcot_assimilation(ASSIMILATE)                  ! day's assimilate
      SUPPLY = ASSIMILATE+RESERVE                      ! compute supply
      RESERVE = 0.                                     ! reserve used

C------------------------------------------------------------------------------
C   compare total demand with supply to partition assimilate
C------------------------------------------------------------------------------

      DEMAND = dDW_LEAF+dDW_BOLL+dDW_STEM+dDW_ROOT     ! compute total demand

      IF(SUPPLY.GE.DEMAND) THEN        ! demand met, potential growth achieved
         RESERVE = RESERVE+SUPPLY-DEMAND               ! excess becomes reserve
         IF(RESERVE.GT.RES_CAP) RESERVE = RES_CAP      ! limit to reserve
         SD_RATIO = 1.0                ! supply:demand ratio for leaf,stem,boll
         SD_ROOT = 1.0                 ! supply:demand for root
      ELSE                             ! demand not met, reduce potential growth
         DEMAND = DEMAND-dDW_ROOT      ! demand for leaf, stem and fruit
         IF(SUPPLY.GE.DEMAND) THEN     ! their potential growth achieved
             SD_RATIO = 1.0            ! supply:demand ratio for leaf,stem,boll
             SD_ROOT = divide ((SUPPLY-DEMAND)
     :                         ,dDW_ROOT, 0.0)         ! supply:demand for root
             dDW_ROOT = SUPPLY-DEMAND  ! rest to root
         ELSE                          ! leaf, stem and fruit demand not met
             SD_RATIO = divide (SUPPLY, DEMAND, 0.0)   ! supply:demand ratio
             dDW_LEAF = dDW_LEAF*SD_RATIO              ! actual leaf growth
             dDW_BOLL = dDW_BOLL*SD_RATIO              ! actual fruit growth
             dDW_STEM = dDW_STEM*SD_RATIO              ! actual stem growth
             dDW_ROOT = 0.                             ! no root growth
             SD_ROOT = 0.                              ! supply:demand for root
             BOLLGR = BOLLGR*SD_RATIO                  ! adjust boll growth rate
         ENDIF
      ENDIF

C------------------------------------------------------------------------------
C     grow crop by updating dry weights for leaf, stem, bolls and roots
C------------------------------------------------------------------------------

      DW_LEAF = DW_LEAF+dDW_LEAF                       ! total leaf dry weight
      DW_STEM = DW_STEM+dDW_STEM                       ! total stem dry weight
      DW_BOLL = DW_BOLL+dDW_BOLL                       ! total boll dry weight
      DW_ROOT = DW_ROOT+dDW_ROOT                       ! total root dry weight
      DW_TOTAL = DW_LEAF+DW_STEM+DW_BOLL+DW_ROOT       ! total dry weight

      dDW_L(IDAY) = dDW_LEAF                           ! this day's leaf dry wt
C      ALAI = ALAI+DLAI(IDAY)                           ! update LAI with increase

C------------------------------------------------------------------------------
C     feed back from root grow to root depth
C------------------------------------------------------------------------------

      IF(IDAY.EQ.1) dDW_ROOT_MAX = dDW_ROOT            ! initialise max root rate

      IF(dDW_ROOT.GT.dDW_ROOT_MAX) THEN
          dDW_ROOT_MAX = dDW_ROOT                      ! save maximum root rate
          ROOT_FEEDBACK = 1.0                          ! feedback of dw on depth
      ELSE
          IF(dDW_ROOT_MAX.EQ.0.) THEN
              ROOT_FEEDBACK = 1.0
          ELSE
              ROOT_FEEDBACK = divide (dDW_ROOT, dDW_ROOT_MAX, 0.0)  ! feedback of dw on depth
          ENDIF
      ENDIF

      ROOT_FEEDBACK = AMIN1(ROOT_FEEDBACK,SD_ROOT)     ! feedback of dw on depth
      IF(ROOT_FEEDBACK.GT.0.) ROOT_FEEDBACK=ROOT_FEEDBACK**0.333 ! cubic to linear

C------------------------------------------------------------------------------

      RETURN
      END



c      SUBROUTINE ASSIMILATION (ASSIMILATE,I)
      SUBROUTINE ozcot_assimilation (ASSIMILATE)

C     Assimilate production for the day is estimated from intercepted
C     photosynthetically active radiation (Montieth 1977).
C     Adjusted for effects of water stress using data of Turner et al 1986
C     collected at Narrabri ARS.
C     Effect of water logging based on observation of Hearn and Constable 1984
C     on yield and Hodgson on photosynthesis.
C     Effect of temperature, see Constables thesis 1981, direct effect on
C     photosynthesis and respiration, using Angus and Wilson's 1976 expression
C     for a scalar and Constables value for base and optimum.
C     Carrying capacity, CARCAP,estimated. It is maximum number of bolls the
C     crop can carry, and is therefore the boll load that causes 100% shedding.
C     Local variables:
C       ASSIM_MEAN      3 day mean of assimilate supply
C       ASSIM_1         previous day's assimilate supply
C       ASSIM_2         day before previous day's supply
C       RAD_MJ          radiation in mega joules
C       REL_P           relative photosynthesis, scalar for water stress
C       PAR_INT         intercepted photosynthetically active radiation
C       TF              temperature scalar for dry matter production

      INCLUDE 'ozcot.inc'
      real assim_1, assim_2, rad_mj, alight, par_int
      real assimilate, rel_p, tf
C------------------------------------------------------------------------------
C     initialise for new season
C------------------------------------------------------------------------------

cpc   IF(IDAY.EQ.1) THEN
cpc       ASSIM_1 = 0.0                    ! previous day's assimilation
cpc       ASSIM_2 = 0.0                    ! day before that
cpsc  ENDIF

C------------------------------------------------------------------------------
C     photosynthesis
C------------------------------------------------------------------------------

      RAD_MJ = RAD/23.87                   ! langleys to Mjoules
c      PAR_INT = RAD_MJ*(1.-TR)*0.5         ! intercepted PAR

      ALIGHT = 1.-EXP(-1.*ALAI)            ! light interception, Beer's law.
      PAR_INT = RAD_MJ*(ALIGHT)*0.5        ! intercepted PAR, ex old OZCOT

      ASSIMILATE = PAR_INT*E_PAR           ! assimilation
      IF(ASSIMILATE*2..GT.RES_CAP) THEN
          RES_CAP = ASSIMILATE*2.          ! capacity to store reserves
      ENDIF

C------------------------------------------------------------------------------
C     effect of water stress on assimilation
C------------------------------------------------------------------------------

      REL_P =.25+.864*SMI                  ! effect of water stress on P
      IF(REL_P.GT.1.) REL_P = 1.           ! (TURNER et al 1986).
      ASSIMILATE = ASSIMILATE*REL_P        ! adjust for water stress

C------------------------------------------------------------------------------
C     effect of temperature on dry matter production
C------------------------------------------------------------------------------

      TF = (TEMPAV-T_BASE)/(T_OPT-T_BASE)  ! temperature scalar after
      TF = 2*TF-TF**2                      ! Angus & Wilson 1976, Constable 1981
      ASSIMILATE = ASSIMILATE*TF           ! adjust assimilate for temp

C------------------------------------------------------------------------------
C     effect of waterlogging on photosynthesis - Hearn & Constable 1984 eqn 4
C------------------------------------------------------------------------------

C      IF(DEF.LT.2.5) THEN                 ! waterlogged?
      IF(SW/UL.GT.0.87) THEN               ! waterlogged?
          ASSIMILATE = ASSIMILATE*0.2      ! adjust for water logging - old OZCOT
      ENDIF

c      IF(ISQ.EQ.0 .OR. I.LT.ISQ+2) RETURN  ! do not proceed to carrying capacity
      IF(ISQ.EQ.0 .OR. das.LT.ISQ+2) RETURN  ! do not proceed to carrying capacity

C------------------------------------------------------------------------------
C     carrying capacity - photosynthetic capacity divided by boll growth rate
C------------------------------------------------------------------------------

c      disable rest of subroutine for use with OZCOT2 in APSRU system

C      IF(ASSIM_1.GT.0.0 .AND.ASSIM_1.GT.0.0) THEN      ! not 1st or 2nd crop day
C          ASSIM_MEAN = (ASSIMILATE+ASSIM_1+ASSIM_2)/3. ! 3 day running mean
C      ELSE                                             ! is 1st or 2nd crop day
C          ASSIM_MEAN = ASSIMILATE
C      ENDIF
                                                   ! use mean to buffer CARCAP
C      ASSIM_2 = ASSIM_1                            ! 3rd day's for tomorrow
C      ASSIM_1 = ASSIMILATE                         ! 2nd day's for tomorrow

C      IF(BOLLGR.GT.0.0) THEN
C          CARCAP_C = ASSIM_MEAN/BOLLGR             ! carrying capacity
C      ELSE
C          CARCAP_C = 0.0                           ! zero when bolls not growing
C      ENDIF

C      IF(CARCAP_C.LT.0.) CARCAP_C = 0.             ! trap
C      CARCAP  = CARCAP_C
C      CUTOUT = CARCAP*FCUTOUT(IVAR)                ! boll load for cutout

C      CUTOUT = CARCAP*1.00                         ! sensitivity to FCUTOUT

C------------------------------------------------------------------------------

      RETURN
      END


