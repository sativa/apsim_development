      include 'SoilT.inc'
 !     ===========================================================
      Recursive
     :Subroutine AllocInstance (InstanceName, InstanceNo)
 !     ===========================================================
      use SoilTModule
      implicit none

 !+  Sub-Program Arguments
      character InstanceName*(*)       ! (INPUT) name of instance
      integer   InstanceNo             ! (INPUT) instance number to allocate

 !+  Purpose
 !      Module instantiation routine.

 !- Implementation Section ----------------------------------

      allocate (Instances(InstanceNo)%gptr)
      allocate (Instances(InstanceNo)%eptr)
      allocate (Instances(InstanceNo)%pptr)
      allocate (Instances(InstanceNo)%cptr)
      Instances(InstanceNo)%Name = InstanceName

      return
      end

 !     ===========================================================
      Recursive
     :Subroutine FreeInstance (anInstanceNo)
 !     ===========================================================
      use SoilTModule
      implicit none

 !+  Sub-Program Arguments
      integer anInstanceNo             ! (INPUT) instance number to allocate

 !+  Purpose
 !      Module de-instantiation routine.

 !- Implementation Section ----------------------------------

      deallocate (Instances(anInstanceNo)%gptr)
      deallocate (Instances(anInstanceNo)%eptr)
      deallocate (Instances(anInstanceNo)%pptr)
      deallocate (Instances(anInstanceNo)%cptr)

      return
      end

 !     ===========================================================
      Recursive
     :Subroutine SwapInstance (anInstanceNo)
 !     ===========================================================
      use SoilTModule
      implicit none

 !+  Sub-Program Arguments
      integer anInstanceNo             ! (INPUT) instance number to allocate

 !+  Purpose
 !      Swap an instance into the global 'g' pointer

 !- Implementation Section ----------------------------------

      g => Instances(anInstanceNo)%gptr
      e => Instances(anInstanceNo)%eptr
      p => Instances(anInstanceNo)%pptr
      c => Instances(anInstanceNo)%cptr

      return
      end


* ====================================================================
      Recursive
     :Subroutine Main (action, data_string)
* ====================================================================
      use SoilTModule
      implicit none
      include   'action.inc'
      include   'const.inc'           ! Global constant definitions
      include   'event.inc'
      include   'error.pub'

*+  Sub-Program Arguments
      character Action*(*)            ! Message action to perform
      character data_string*(*)

*+  Purpose
*      This routine is the interface between the main system and the
*      soilt module.

*+  Changes
*      psc - 300595
*      011195 jngh  added call to message_unused

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'apsim_soilt')

*- Implementation Section ----------------------------------
      call push_routine(myname)

      if (Action.eq.ACTION_init) then
         call soilt_zero_variables ()
         call soilt_Init ()

      else if (Action .eq. ACTION_prepare) then
         call soilt_prepare ()

      else if (Action.eq.ACTION_Process) then
         if (g%Zero_variables) then
            call soilt_zero_variables()
            call soilt_init()

         else
            ! No need to zero variables.
         endif

         call soilt_get_other_variables ()
         call soilt_Process ()
         call soilt_set_other_variables ()

      else if (Action .eq. ACTION_Post) then
         call soilt_post ()

      else if (Action.eq.ACTION_Get_variable) then
         call soilt_Send_my_variable (data_string)

      else if (Action .eq. ACTION_Set_variable) then
         call soilt_set_my_variable (data_string)

      else if (Action .eq. ACTION_End_run) then
         call soilt_end_run ()

      else
            ! Don't use message
         call Message_unused ()

      endif

      call pop_routine(myname)
      return
      end

* ====================================================================
      Recursive
     :Subroutine soilt_Init ()
* ====================================================================
      use SoilTModule
      implicit none
      include 'const.inc'             ! Constant definitions
      include   'error.pub'

*+  Purpose
*      Initialise soilt module

*+  Changes
*      psc - 300595

*+  Local Variables
       character Event_string*40       ! String to output

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'soilt_init')

*- Implementation Section ----------------------------------
      call push_routine(myname)
      ! Notify system that we have initialised

      Event_string = 'Initialising: ' 
      call Write_string (Event_string)

      ! Get all parameters from parameter file

!      call soilt_read_param ()

      call soilt_get_other_variables ()
      call pop_routine(myname)
      return
      end

* ====================================================================
      Recursive
     :Subroutine soilt_read_param ()
* ====================================================================
      use SoilTModule
       implicit none
      include   'error.pub'

*+  Purpose
*      Read in all parameters from parameter file.

*+  Changes
*      psc - 09/08/93 first go
*      psc - 30/03/94 specified properly
*      DPH - 7/7/94  Removed free format internal read to title.  Line now
*                    reads title = param_string

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'soilt_read_param')
      character  section_name*(*)
      parameter (section_name = 'parameters')

*- Implementation Section ----------------------------------
      call push_routine(myname)
         ! Read in title from parameter file
!      call read_char_array (section_name
!     :                     , 'title', 15, '()'
!     :                     , title, numvals)

      call pop_routine(myname)
      return
      end

* ====================================================================
      Recursive
     :Subroutine soilt_zero_variables ()
* ====================================================================
      use SoilTModule
      implicit none
      include   'error.pub'

*+  Purpose
*     Set all variables in this module to zero.

*+  Changes
*      psc - 300595

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'soilt_zero_variables')

*- Implementation Section ----------------------------------
      call push_routine(myname)
!      call soilt_initial()
      g%Zero_variables = .false.
      call pop_routine(myname)
      return
      end

* ====================================================================
      Recursive
     :Subroutine soilt_manager (Event_action, Event_data)
* ====================================================================
      use SoilTModule
      implicit none
      include 'const.inc'              ! global_active
      include   'error.pub'

*+  Sub-Program Arguments
      character Event_action*(*)       ! (INPUT) Action to be performed
      character Event_data*(*)         ! (INPUT) Data sent with event

*+  Purpose
*     The manager has sent an event to this module.  Process it.

*+  Changes
*      psc - 300595
*      07/07/94 - jngh changed residue module reference to global_active

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'soilt_manager')

*- Implementation Section ----------------------------------
      call push_routine(myname)
      ! **** Repeat for each action

       ! Report the event to the rest of the system

 !     call Report_event ( Event_action)
      call pop_routine(myname)
      return
      end

* ====================================================================
      Recursive
     :Subroutine soilt_get_other_variables ()
* ====================================================================
      use SoilTModule
      implicit none
       include 'const.inc'             ! Constant definitions
      include   'intrface.pub'
      include   'error.pub'

*+  Purpose
*      Get the values of variables from other modules

*+  Changes
*      psc - 300595
*      psc - 28/03/94  used this routine properly
*      DPH - 7/7/94 Changed call to nt_fac to soilt_nt_fac
*                   Added check for N module.  If N module doesn't
*                   exist then seed no3ppm with some high values.
*      DPH - 11/7/94   Fixed bug in detection of existence of N module.
*      JNGH - 12/7/94 Changed dlayer in cm to g_dlayer_cm

      real       divide                ! function

*+  Local Variables
      integer    layer                 ! layer number
      real       residue_cover
      integer    crop                  ! loop index
      integer    numvals               ! number of values put into array
      real       bare                  ! bare soil fraction (0-1)
      real       cover                 ! temporary cover variable (0-1)
      real       cover_green           ! temporary cover variable (0-1)

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'soilt_get_other_variables')

*- Implementation Section ----------------------------------
      call push_routine(myname)
      ! Get depths of each layer

                                ! get depth of each soil water layer
      call get_real_array (unknown_module, 'dlayer', max_layers
     :                                    , '(mm)'
     :                                    , g%dlayer, g%num_layers
     :                                    , 0.0, 1000.0)

      call get_real_var (unknown_module, 'maxt', '(oC)'
     :                                  , g%maxt, numvals
     :                                  , -100.0, 100.0)

      call get_real_var (unknown_module, 'mint', '(oC)'
     :                                  , g%mint, numvals
     :                                  , -100.0, 100.0)

      call get_real_var (unknown_module, 'radn', '(Mj/m^2)'
     :                                  , g%radn, numvals
     :                                  , 0.0, 1000.0)

      g%radn = g%radn / 0.04186                ! convert to langleys

      call get_real_var (unknown_module, 'es', '(mm)'
     :                                  , g%es, numvals
     :                                  , 0.0, 1000.0)

      call get_real_var (unknown_module, 'residue_cover', '()'
     :                                  , cover, numvals
     :                                  , 0.0, 1000.0)

cjh      call get_real_var (unknown_module, 'cover_green', '(mm)'
cjh     :                                  , cover_green, numvals
cjh     :                                  , 0.0, 1000.0)
     
cjh      if (cover + cover_green .gt.0.0) then
cjh      g%estimated_lai = divide (alog (cover + cover_green), 0.5, 0.0)
cjh      else
cjh      endif
cjh      goto 1001
cjh      
      crop = 0
      bare = (1.0 - cover)
1000  continue
         crop = crop + 1
         call get_real_vars (crop, 'cover_green', '()'
     :                            , cover_green, numvals
     :                            , 0.0, 1.0)

            ! Note - this is based on a reduction of Beers law
            ! cover1+cover2 = 1 - exp (-(k1*lai1 + k2*lai2))
         if (numvals.ne.0) then
            bare = bare * (1.0 - cover_green)
            goto 1000
         else
               ! no more crops
            cover = 1.0 - bare
         endif

      g%estimated_lai = log (1.0 - cover)/(-0.5)

cjh1001  continue
      call get_real_array (unknown_module, 'sw', max_layers, '(m3/m3)'
     :                     , g%sw, g%num_layers 
     :                     , 0.0, 1.0)

cjh      ! Convert water to plant available  (cm/cm)

cjh      do 100Layer = 1, g%num_layers
cjh        g%sw(Layer) = g%sw(Layer) / g%dlayer(layer)
cjh        g%sw(Layer) = max(0.0, g%sw(Layer))
cjh100   continue

      call pop_routine(myname)
      return
      end

* ====================================================================
      Recursive
     :Subroutine soilt_set_other_variables ()
* ====================================================================
      use SoilTModule
      implicit none
      include 'const.inc'
      include   'error.pub'

*+  Purpose
*     Update variables owned by other modules.

*+  Changes
*      psc - 300595
*      psc   300394  specified properly
*      DPH   7/7/94  Put 0.0 in max function call instead of 0
*                   Changed call to nt_fac to soilt_nt_fac
*                    Changed call to set_variable_value to call to
*                    Set_real_array
*      JNGH 18/7/94 Corrected conversion of min no3 from ppm to kg/ha
*      JNGH - 12/7/94 Changed dlayer in cm to g_dlayer_cm

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'soilt_set_other_variables')

*- Implementation Section ----------------------------------
      call push_routine(myname)

      call pop_routine(myname)
      return
      end

* ====================================================================
      Recursive
     :Subroutine soilt_Send_my_variable (Variable_name)
* ====================================================================
      use SoilTModule
      implicit none
       include 'const.inc'             ! constant definitions
      include   'intrface.pub'
      include   'error.pub'

*+  Sub-Program Arguments
       character Variable_name*(*)     ! (INPUT) Variable name to search for

*+  Purpose
*      Return the value of one of our variables to caller

*+  Changes
*      psc - 300595
*      DPH 7/7/94 Changed crop_in variable to soilt_crop_in.
*      011195 jngh  added call to message_unused

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'soilt_send_my_variable')

*- Implementation Section ----------------------------------
      call push_routine(myname)
      ! **** Repeat for each variable

c      write (6,630) iyr,jdate,g%temp0,wattm2,rb0,rd,rl,p0,pav,g%estimated_lai,tr,
c     1     ttav,hv,g%es,beta2,g%ys,g%phis

      if (Variable_name .eq. 'soil_temp') then
         call respond2get_real_var ('soil_temp'
     :        , '(oC)', g%temp0)

      else

                                ! Nothing
         call Message_unused ()
      endif
      call pop_routine(myname)
      return
      end

* ====================================================================
      Recursive
     :Subroutine soilt_set_my_variable (Variable_name)
* ====================================================================
      use SoilTModule
      implicit none
      include   'error.pub'

*+  Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name to search for

*+  Purpose
*     Set one of our variables altered by some other module

*+  Changes
*      psc - 300595

*- Implementation Section ----------------------------------

c     if (Variable_name .eq. '????') then
c        read(Values_str, *, iostat=Read_code) ????

c     else
         ! Don't know this variable name
c     endif

      return
      end

* ====================================================================
      Recursive
     :Subroutine soilt_Process ()
* ====================================================================
      use SoilTModule
      implicit none
       include 'const.inc'             ! Constant definitions
      include   'error.pub'

*+  Purpose
*      Perform actions for current day.

*+  Changes
*      psc - 300595

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'soilt_process')

*- Implementation Section ----------------------------------

      call push_routine(myname)
!     call patched-in soilt model

      call tc1max ()

      call pop_routine(myname)
      return
      end


* ====================================================================
      Recursive
     :Subroutine soilt_Prepare ()
* ====================================================================
      implicit none
      include   'error.pub'

*+  Purpose
*     Perform calculations before the current timestep.

*+  Changes
*      psc - 300595

*- Implementation Section ----------------------------------

      return
      end

* ====================================================================
      Recursive
     :Subroutine soilt_post ()
* ====================================================================
      implicit none
      include   'error.pub'

*+  Purpose
*     Perform calculations after the current timestep.

*+  Changes
*      psc - 300595

*- Implementation Section ----------------------------------

      return
      end

* ====================================================================
      Recursive
     :Subroutine soilt_end_run ()
* ====================================================================
      implicit none
      include   'error.pub'

*+  Purpose
*     Perform cleanup because the current simulation is about to end.

*+  Changes
*      psc - 300595

*- Implementation Section ----------------------------------

      return
      end

* ====================================================================
      Recursive
     :Subroutine TC1MAX ()
* ====================================================================
      use SoilTModule
      implicit none
      include   'error.pub'

*+  Local Variables
      REAL RB,RD,RL,PSI,TR,ttav,HV,tt
      REAL RB0
      REAL P0,PPSI,SF2,SB2,SB1,ALPHA1,BETA2
      REAL IPSI,PAV,IAV,RHO1,RHO2,TAU2
      REAL B1,ED1,EU1,EU2,ES1,ES2,EL1,EL2
      REAL HS1,HS2,RC1,RC2,TC1,TC2,Gg,PHI
      REAL H1,H2,J11,J12,J21,J22,DJ,DTC1,DTC2
      REAL YEFF,F1,F2,F
      real x1, x2, x3, x4
      real wattm2
      real tempm

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'TC1MAX')

      REAL SIGMA,PI,OMEGA,TZ

      SIGMA = 5.67E-8
      PI = 3.1415927
      TZ = 273.16
      OMEGA = 2.0*PI / 86400.0

*- Implementation Section ----------------------------------

      call push_routine(myname)

 10   CONTINUE
c
      wattm2 = 0.30*g%radn / 180.*698.
      rb0 = 0.85*wattm2
      rd = 0.15*wattm2
c               rl calculated from Monteith 1973
      rl = 208. + 6.*g%maxt
      p0 = exp(-g%estimated_lai*2. / 4.)
      if (p0.eq.1.) p0 = 0.999
      pav = exp(-g%estimated_lai*2. / 2.6)
      if (pav.eq.1.) pav = 0.999
c
      TR = g%maxt + TZ
      tempm = (g%maxt + g%mint) / 2.
      ttav = tempm + p0*(g%maxt - tempm) + tz
      hv = 27.78*g%es
c
      sf2 = 0.22
      sb2 = 0.20
      sb1 = 0.20
      alpha1 = 1.70
      beta2 = 1.4*2.*g%estimated_lai / 0.02**0.25
      if (beta2.le.22.) beta2 = 22.
c
      call soilt ()
C       DERIVE OTHER PARAMETERS
      IAV = 1.0 - PAV
      RHO1 = SB1
      RHO2 = IAV*SB2
      TAU2 = PAV + IAV*SF2
      YEFF = g%ys*COS(g%phis)
c
      tt = 0.
 20   CONTINUE
C       CALCULATE VARIABLES
c
      PHI = OMEGA*tt
      RB = RB0*COS(PHI)
      PSI = PHI
      PPSI = P0**(1. / COS(PSI))
      IPSI = 1. - PPSI
      B1 = PPSI*RB
      ED1 = (TAU2*RD + RHO2*SB1*B1 + SF2*IPSI*RB) / (1.0 - RHO1*RHO2)
      EU1 = RHO1*ED1 + SB1*B1
      EU2 = TAU2*EU1 + RHO2*RD + SB2*IPSI*RB
      ES1 = B1 + ED1 - EU1
      ES2 = RB - B1 + RD - ED1 + EU1 - EU2
      TC1 = TR
      TC2 = TR
      DTC1 = 5.0
      DTC2 = 5.0
 30   CONTINUE
C       START NEXT ITERATION
      TC1 = TC1 + DTC1
      TC2 = TC2 + DTC2
      X1 = SIGMA*TC1**4
      X2 = SIGMA*TC2**4
      EL1 = PAV*RL + IAV*X2 - X1
      EL2 = IAV*RL - 2.0*IAV*X2 + IAV*X1
      RC1 = 1.0 / (ALPHA1*(ABS(TC1 - TR))**0.333333)
      RC2 = 1.0 / (BETA2*(ABS(TC2 - TR))**0.25)
      HS1 = (TC1 - TR) / RC1
      HS2 = (TC2 - TR) / RC2
c
      Gg = (TC1 - ttav)*YEFF
      H1 = ES1 + EL1 - HV - HS1 - Gg
      H2 = ES2 + EL2 - HS2
      J11 = -4.0*X1 / TC1 - 1.333333 / RC1 - YEFF
      J12 = 4.0*IAV*X2 / TC2
      J21 = 4.0*IAV*X1 / TC1
      J22 = -8.0*IAV*X2 / TC2 - 1.25 / RC2
      DJ = J11*J22 - J12*J21
      DTC1 = (-J22*H1 + J12*H2) / DJ
      DTC2 = (J21*H1 - J11*H2) / DJ
      IF(ABS(H1) + ABS(H2).GT.0.01)GO TO 30
      X3 = PPSI*ALOG(PPSI)
      F1 = (1. - SB1)*(PPSI + SF2*IPSI - X3*(1. - SF2))
      F2 = (1. - SB2 - SF2)*(IPSI + IAV*SB1*PPSI + X3*(1. - IAV*SB1))
      F = g%ys*SIN(g%phis) / (RB*(F1 - (J12 / J22)*F2))
c
      PHI = ATAN((TC1 - ttav)*F)

      X4 = tt
      tt = PHI / OMEGA
      IF(ABS(tt - X4).GT.0.1)GO TO 20
c
      g%temp0 = tc1 - tz
c      write (6,630) iyr,jdate,g%temp0,wattm2,rb0,rd,rl,p0,pav,g%estimated_lai,tr,
c     1     ttav,hv,g%es,beta2,g%ys,g%phis
c  630 format(i3,i4,f5.1,4f5.0,2f5.2,3f6.0,f5.0,3f5.1,f6.3)

      call pop_routine(myname)
      return
      END
c
* ====================================================================
      Recursive
     :Subroutine SOILT ()
* ====================================================================
      use SoilTModule
      implicit none
      include   'error.pub'

*+  Local Variables
      REAL Z,VWC,Cc,K,fun_A,fun_M,ll
      COMPLEX YS1,Y,RG,RT,SRG(100),SRT(100),CN
      integer nj
      integer i
      integer j
      real topsw
      real subsw
      real zd

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'SOILT')

      REAL Pp, vsw(7),tlayr(7)
      DATA Pp / 86400. / 
      data tlayr / 0.,0.,1.,1.,1.,1.,1. / 
      data vsw / 7*0.0 / 
      
      fun_M(CN) = cabs(CN)
      fun_A(CN) = aimag(clog(CN))

*- Implementation Section ----------------------------------

      call push_routine(myname)
      YS1 = (10.,.7845)
      NJ = 0
      tlayr(1) = 0.
      topsw = 0.
      subsw = 0.
      zd = 6.
      if (g%dlayer(1).ge.6.) then
        do 20 i = 3,7
          zd = zd - 1.
          vsw(i) = .0125 - 0.25*g%sw(1) + 0.25*(g%sw(1) - .01)*zd
          topsw = topsw + vsw(i)
   20   continue
        vsw(2) = (g%sw(1)*g%dlayer(1) - topsw) / (g%dlayer(1) - 5.)
        tlayr(2) = g%dlayer(1) - 5.
        do 30 j = 2,10
          tlayr(1) = tlayr(1) + g%dlayer(j)
          subsw = subsw + g%sw(j)*g%dlayer(j)
          if (tlayr(1).ge.75.) go to 40
   30   continue
   40   vsw(1) = subsw / tlayr(1)
      else
        do 50 j = 1,4
   50     vsw(j) = g%sw(4)
        vsw(5) = g%sw(3)
        vsw(6) = g%sw(2)
        vsw(7) = g%sw(1)
      endif
   10 CONTINUE
      NJ = NJ + 1
      z = tlayr(nj) / 100.
      vwc = vsw(nj)
      CALL GETCK(VWC,Cc,K)
      CALL ADMIT1(Pp,Z,Cc,K,YS1,Y,RG,RT)
      YS1 = Y
      if (nj.lt.7) GO TO 10
      g%ys = fun_m(y)
      g%phis = fun_a(y)

      call pop_routine(myname)
      return
      END
c
* ====================================================================
      Recursive
     :Subroutine GETCK(VWC,C,K)
* ====================================================================
      implicit none
      include   'error.pub'
      
*+  Sub-Program Arguments
      REAL VWC,C,K

*+  Local Variables
      real x

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'GETCK')

      REAL C0,C1,K0,K1
      DATA C0,C1,K0,K1 / 1.4E6,4.18E6,.32,1.18 / 
*- Implementation Section ----------------------------------

      call push_routine(myname)
      C = C0 + C1*VWC
      x = 2.4*vwc
      if (vwc.gt.0.075) x = x + 35.0*(vwc - 0.075)**2
      if (vwc.gt.0.15) x = x - 750.0*(vwc - 0.15)**3
      if (vwc.gt.0.225) x = 1.0
      K = K0 + K1*x

      call pop_routine(myname)
      RETURN
      END
c
* ====================================================================
      Recursive
     :Subroutine ADMIT1(P,Z,C,K,YS1,Y,RG,RT)
* ====================================================================
      implicit none
      include   'error.pub'

*+  Sub-Program Arguments
      REAL P,Z,C,K
      COMPLEX YS1,Y,RG,RT

*+  Local Variables
      REAL W,D
      COMPLEX YINF,R,SINH,COSH,TANH

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ADMIT1')

      REAL PI
      DATA PI / 3.141592654 / 

*- Implementation Section ----------------------------------

      call push_routine(myname)
      W = 2.*PI / P
      D = SQRT(2.*K / (W*C))
      YINF = (1.,1.)*SQRT(W*C*K / 2.)
      R = (1.,1.)*Z / D
      SINH = (CEXP(R) - CEXP( - R)) / 2.
      COSH = (CEXP(R) + CEXP( - R)) / 2.
      TANH = SINH / COSH
      RG = SINH*YINF / YS1 + COSH
      RT = SINH*YS1 / YINF + COSH
      Y = YINF*(TANH + YS1 / YINF) / (TANH*YS1 / YINF + 1.)

      call pop_routine(myname)
      RETURN
      END
