 
* =====================================================================
      subroutine apswim_gsurf(deqrain,g)
* =====================================================================
*     Short Description:
*     gets soil surface conductance g
*
      implicit none
 
*     Global Variables
      include 'apswim.inc'
 
      double precision ddivide
 
      include 'data.pub'                          
 
*     Subroutine Arguments
      double precision deqrain
      double precision g
 
*     Internal Variables
      double precision decay_fraction
      double precision ceqrain
 
*     Constant Values
*     none
*
      ! Ideally, if timesteps are small we could just use
      ! dgsurf/dEqr = -1/grc x (gsurf - g0)
      ! but because this is really just a linear approximation of the
      ! curve for longer timesteps we had better be explicit and
      ! calculate the difference from the exponential decay curve.
 
      if (grc.ne.0) then
         ! first calculate the amount of Energy that must have been
         ! applied to reach the current conductance.
 
         decay_Fraction = ddivide(gsurf-g0,g1-g0,0d0)
 
         if (doubles_are_equal (decay_fraction, 0d0)) then
            ! seal is totally decayed
            g = g0
         else
 
            ceqrain = -grc * log(decay_Fraction)
 
            ! now add rainfall energy for this timestep
            if (c_cover_effects.eq.'on') then
               ceqrain = ceqrain + deqrain * (1d0 - g_residue_cover)
            else
               ceqrain = ceqrain + deqrain
            endif
 
            ! now calculate new surface storage from new energy
            g = g0+(g1-g0)*exp(-ceqrain/grc)
         endif
      else
         g = gsurf
      endif
 
      return
      end
* =====================================================================
      subroutine apswim_solve(itlim,fail)
* =====================================================================
*     Short description:
*     solves for this time step
*
      implicit none
 
*     Global Variables
      include 'apswim.inc'
 
      double precision apswim_wpf    ! function
 
      include 'write.pub'                         
 
*     Subroutine Arguments
      integer itlim           !(input) limit for no. of iterations
      logical fail            !(output)fail flag
 
*     Internal Variables
 
      double precision a(-1:M)
      double precision aerr
      double precision b(-1:M)
      double precision balerr
      double precision c(-1:M)
      double precision d(-1:M)
      double precision err
      integer          i
      integer          i1
      integer          i2
      integer          i0
      integer          ibpf
      integer          iroots
      integer          it
      integer          j
      integer          neq
      integer          solnum
      double precision rhs(-1:M)
      double precision dp(-1:M)
      double precision vbp(-1:M)
      double precision wpold
 
*     Constant Values
*     none
 
      it=0
      wpold=wp
      iroots=0
*     loop until solved or too many iterations or Thomas algorithm fails
10    continue
         it=it+1
*        get balance eqns
         call apswim_baleq(it,iroots,isol,slos,csl,i1,i2,a,b,c,rhs)
*        test for convergence to soln
cnh hey - wpf has no arguments!
cnh         wp=wpf(n,dx,th)
         wp = apswim_wpf()
 
         balerr=ron-roff-q(n+1)-rex-res-
     1          (h-hold+wp-wpold+(hbp-hbpold)*sbp)/dt
         err=0.
         do 20 i=i1,i2
            aerr=abs(rhs(i))
            if(err.lt.aerr)err=aerr
20       continue
*        switch off iteration for root extraction if err small enough
         if(err.lt.errex*rex.and.iroots.eq.0)iroots=1
         if(abs(balerr).lt.ersoil.and.err.lt.ernode)then
            fail=.FALSE.
         else
            neq=i2-i1+1
            ibpf=ibp-i1+1
            call apswim_thomas(neq,ibpf,a(i1),b(i1),c(i1),rhs(i1),qbpd,
     1                  d(i1),dp(i1),vbp,fail)
            work=work+neq
cnh            if(fail)go to 90
            if(fail) then
cnh               call warning_error(Err_internal,
cnh     :            'swim will reduce timestep to solve water movement')
                  call write_event (
     :      'swim will reduce timestep to avoid error in water balance')
               goto 90
            endif
 
            fail=.TRUE.
*           limit step size_of for soil nodes
            i0=max(i1,0)
            do 30 i=i0,i2
               if(dp(i).gt.dppl)dp(i)=dppl
               if(dp(i).lt.-dpnl)dp(i)=-dpnl
30          continue
*           update solution
            j=i0
            do 40 i=i0,i2
               p(j)=p(j)+dp(i)
               if(j.gt.0.and.j.lt.n-1)then
                  if(x(j).eq.x(j+1))then
                     j=j+1
                     p(j)=p(j-1)
                  end if
               end if
               j=j+1
40          continue
            if(i1.eq.-1)h=max(0d0,h+dp(-1))
         end if
      if(fail.and.it.lt.itlim)go to 10
cnh      if(isol.ne.1.or.fail)go to 90
      if (fail) then
         call write_event (
     :   'Maximum iterations reached - swim will reduce timestep')
         goto 90
      endif
      if(isol.ne.1) then
         goto 90
      endif
 
*     solve for solute movement
cnh      call getsol(a(0),b(0),c(0),d(0),rhs(0),dp(0),vbp(0),fail)
 
      do 80 solnum = 1,num_solutes
         call apswim_getsol
     :          (solnum,a(0),b(0),c(0),d(0),rhs(0),dp(0),vbp(0),fail)
         If (fail) then
            call write_event (
     :         'swim will reduce timestep to solve solute movement')
 
            goto 85
         endif
   80 continue
   85 continue
cnh
 
90    continue
      end
* =====================================================================
      subroutine apswim_pstat (istat,tresp)
* =====================================================================
*     Short Description:
*     gets potl evap. for soil and veg., and root length densities
*
*     resp,slupf and csl were renamed to tslupf,trep,tcsl as there were
*     already variables with those names in common
 
      implicit none
 
*     Global Variables
      include 'apswim.inc'
 
cnh      double precision cevap            ! function
      double precision apswim_cevap        ! function
      double precision apswim_slupf        ! function
      double precision apswim_time         ! function
      integer          apswim_time_to_mins ! function
      double precision apswim_transp_redn  ! function
      double precision ddivide             ! function
      double precision apswim_cover_eos_redn ! function 
 
*     Subroutine Arguments
      integer istat
      double precision tresp
 
*     Internal Variables
cnh      double precision frac
      integer          i
      integer          iveg
      integer          j
      double precision rep
      double precision rldi
      double precision scale
      double precision sep          ! soil evaporation demand
cnh      double precision sfrac
      integer          solnum
cnh      double precision tfrac
      double precision tot_pep
      double precision trf(MV)
      double precision start_of_day
      double precision end_of_day
      double precision TD_Eo
 
*     Constant Values
 
c      double precision rad    ! set root radius rad (alter as required)
c      parameter (rad=0.1d0)
 
      double precision pi
      parameter (pi=3.141593d0)
*
      if(istat.eq.0)then
*        calc. potl evap.
         rep=(apswim_cevap(t)-apswim_cevap(t-dt))/dt
         if (c_cover_effects.eq.'on') then
            !Use Soilwat cover effects on evaporation. 
            sep = rep*dt * apswim_cover_eos_redn()
         else
            sep = rep*dt * (1d0 - g_crop_cover)
         endif
 
         ! Note: pep is passed to swim as total ep for a plant for the
         ! entire apsim timestep. so rate will be (CEp = cum EP)
         !   dCEp   Total daily EP     dEo
         !   ---- = -------------- x --------
         !    dt    Total daily Eo      dt
 
         start_of_day = apswim_time (year,day,
     :                               apswim_time_to_mins(apsim_time))
         end_of_day = apswim_time (year,day,
     :              apswim_time_to_mins(apsim_time)+int(apsim_timestep))
 
         TD_Eo = apswim_cevap(end_of_day)-apswim_cevap(start_of_day)
 
         tot_pep = 0d0
         do 10 iveg=1,nveg
            trf(iveg) = apswim_transp_redn (iveg)
            tot_pep = tot_pep + ddivide(pep(iveg)*trf(iveg)
     :                                 ,TD_Eo
     :                                 , 0d0)
     :                        * rep*dt
  10     continue
 
         if ((tot_pep+sep .gt. rep*dt)
     :        .and.(evap_source.ne.'sum_demands')) then
            scale = ddivide (rep*dt, tot_pep+sep, 0d0)
         else
            scale = 1d0
         endif
 
         do 50 j=1,nveg
            rtp(j) = ddivide(pep(j)*trf(j),TD_Eo, 0d0)*rep*scale
50       continue
 
         ! pot soil evap rate is not linked to apsim timestep
         tresp = sep/dt*scale
 
         do 60 iveg=1,nveg
            do 60 i=0,n
cnh               rld(i,iveg)=rld(i,iveg)/dx(i)
               if(rld(i,iveg).lt.1d-20)rld(i,iveg)=1d-20
               rldi=rld(i,iveg)
cnh now use root_raidus as in initialisation file
cnh               rc(i,iveg)=-log(pi*rad**2*rldi)/(4.*pi*rldi*dx(i))
 
               rc(i,iveg)=-log(pi*root_radius(iveg)**2*rldi)
     :                        /(4.*pi*rldi*dx(i))
 
60        continue
 
      else if(istat.eq.1)then
*        update cumulative transpiration
         do 70 i=1,nveg
            ctp(i)=ctp(i)+rtp(i)*dt
            ct(i)=ct(i)+rt(i)*dt
cnh
            do 65 j=0,n
               pwuptake(i,j) = pwuptake(i,j) + qr(j,i)*dt*10d0
                                             ! cm -> mm __/
65          continue
70       continue
 
      else if(istat.eq.2)then
*        update cumulative solute uptake
 
         do 90 i=1,nveg
            do 80 j=0,n
               do 95 solnum=1,num_solutes
                  slup(i,solnum)=slup(i,solnum)
     :                +apswim_slupf(i,solnum)*csl(solnum,j)*qr(j,i)
cnh     :                      +slupf(solnum)*csl(solnum,j)*qr(j,i)
                  ! I thought qr was a rate ----------------/
 
                  psuptake(solnum,i,j) = psuptake (solnum,i,j) +
     :              apswim_slupf(i,solnum)*csl(solnum,j)*qr(j,i)/10d0*dt
cnh     :                   slupf(solnum)*csl(solnum,j)*qr(j,i)/10d0*dt
95             continue                                   !   /
                                                          ! ppm -> kg/ha
c        this doesn't make sense....csl has already been changed from it
c        was at the start of the timestep.  need to find a different way
c        of calculating it.  what about qsl???
c        or try getting csl at start of timestep.
c        BUT NUMBERS DO ADD UP OK????? does he then update at start of next
c        timestep???????!!
 
 
80          continue
90       continue
 
      end if
 
      return
      end
* =====================================================================
      subroutine apswim_baleq
     :       (it,iroots,tisol,tslos,tcsl,ibegin,iend,a,b,c,rhs)
* =====================================================================
*     Short Description:
*     gets coefficient matrix and rhs for Newton soln of balance eqns
*
*     Some variables had the same name as some global variables and so
*     these were renamed (by prefixing with t - for temp)
*     this include isol, csl, slos
 
      implicit none
 
*     Global Variables
      include 'apswim.inc'
 
cnh      double precision grad
      double precision apswim_pf
cnh      double precision potl
 
 
*     Subroutine Argruments
      integer it                !(input) iteration no.
      integer iroots            !(input) root extraction flag
      integer tisol             !(input) solute flag
      double precision tslos(nsol)    !(input) osmotic pressure per unit solute
      double precision tcsl(nsol,0:n) !(input) solute concns
      integer ibegin            !(output) position of first equation
      integer iend              !(output) position of last equation
      double precision a(-1:n),b(-1:n),c(-1:n),rhs(-1:n)
*     output: coeff. arrays and rhs for Newton eqns
 
*     Internal Variables
      double precision accept
      double precision absgf
      double precision deltax
      double precision deltap
      double precision hkd2
      double precision hkdp1
      double precision hkdp2
      double precision psip(0:M)
      double precision psipp(0:M)
      double precision thp(0:M)
      double precision hkp(0:M)
      double precision qsp(0:M)
      double precision qexp(3,0:M)
      double precision qp1(0:M+1)
      double precision qp2(0:M+1)
      double precision psios(0:M)
      double precision g
      double precision gh
      double precision gr
      double precision hkd1
      double precision hsoil
      integer          i
      integer          i1
      integer          ifirst
      integer          ilast
      integer          j
      integer          k
      double precision q0
      double precision qbpp
      double precision qbps
      double precision qbpsp
      double precision respsi
      double precision roffd
      double precision skd
      integer          solnum
      double precision v1
      double precision value
      double precision w1
      double precision w2
      double precision wt
      logical          xidif
      logical          xipdif
 
      save ifirst,ilast,gr
 
*     Constant Values
      double precision hcon
      parameter (hcon=7.0e-7)
 
      double precision hair
      parameter (hair=0.5)
 
cnh - added initialisation to zero to eliminate ndp errors
      do 1 i=0,M
         psip(i)=0.d0
         psipp(i)=0.d0
         thp(i)=0.d0
         hkp(i)=0.d0
         qsp(i)=0.d0
         do 2 j=1,3
            qexp(j,i)=0.d0
    2    continue
         qp1(i)=0.d0
         qp2(i)=0.d0
         psios(i)=0.d0
    1 continue
      qp1(M+1) = 0.d0
      qp2(M+1) = 0.d0
cnh - end
 
*
***   initialise for first iteration
      if(it.eq.1)then
         ifirst=0
         ilast=n
         if(itbc.eq.2.and.hold.gt.0.)ifirst=-1
cnh         if(ibbc.eq.0)gr=grad(t)
cnh now uses constant gradient from input file
         if (ibbc.eq.0)gr = constant_gradient
 
         if(ibbc.eq.1)then
cnh            psi(n)=potl(t)
cnh now uses constant potential from input file
            psi(n) = constant_potential
 
            p(n)=apswim_pf(psi(n))
         end if
cnh added to allow seepage to user potential at bbc
         if(ibbc.eq.3)then
            psi(n) = constant_potential
         endif
 
      end if
***   get soil water variables and their derivatives
      do 8 i=0,n
         call apswim_watvar(i,p(i),psi(i),psip(i),psipp(i),th(i),thp(i),
     1               hk(i),hkp(i))
8     continue
***   check boundary potls
      if(itbc.eq.0.and.isbc.eq.0.and.psi(0).gt.0.)then
*        infinite conductance and no ponding allowed
         psi(0)=0.
         p(0)=apswim_pf(psi(0))
         call apswim_watvar(0,p(0),v1,psip(0),psipp(0),th(0),thp(0),
     1               hk(0),hkp(0))
      end if
cnh added to allow seepage to user potential at bbc
cnh      if(ibbc.eq.3.and.psi(n).gt.0.)then
      if(ibbc.eq.3.and.psi(n).gt.constant_potential)then
*        seepage at bottom boundary
cnh         psi(n)=0.
         psi(n)=constant_potential
         p(n)=apswim_pf(psi(n))
         call apswim_watvar(n,p(n),v1,psip(n),psipp(n),th(n),thp(n),
     1               hk(n),hkp(n))
      end if
***   get fluxes between nodes
      absgf=abs(gf)
      do 10 i=1,n
         if(x(i-1).ne.x(i))then
            deltax=x(i)-x(i-1)
            deltap=p(i)-p(i-1)
            hkd1=hk(i-1)*psip(i-1)
            hkd2=hk(i)*psip(i)
            hkdp1=hk(i-1)*psipp(i-1)+hkp(i-1)*psip(i-1)
            hkdp2=hk(i)*psipp(i)+hkp(i)*psip(i)
            skd=hkd1+hkd2
            if(swt.ge..5.and.swt.le.1.)then
*              use fixed space weighting on gravity flow
               w1=sign(2.*swt,gf)
            else
*              use central diffs for gravity flow if possible, else use
*                 just enough upstream weighting to avoid instability
*                 user may increase acceptable level for central diffs
*                 by setting swt < -1
               accept=max(1d0,-swt)
               wt=0.
               if(absgf.ne.0..and.hkp(i).ne.0.)then
                  if(it.eq.1)then
                     value=1.-accept*(skd+(p(i)-p(i-1))*hkdp2)/
     1                   (absgf*deltax*hkp(i))
                     swta(i)=sign(max(0d0,value),gf)
                  end if
                  wt=swta(i)
               end if
               w1=1.+wt
            end if
            w2=2.-w1
            q(i)=-0.5*(skd*deltap/deltax-gf*(w1*hk(i-1)+w2*hk(i)))
            qp1(i)=-0.5*((hkdp1*deltap-skd)/deltax-gf*w1*hkp(i-1))
            qp2(i)=-0.5*((hkdp2*deltap+skd)/deltax-gf*w2*hkp(i))
         end if
10    continue
***   get fluxes to storage
      do 20 i=0,n
         qs(i)=(th(i)-thold(i))*dx(i)/dt
         qsp(i)=thp(i)*dx(i)/dt
20    continue
***   get uptake fluxes to roots if still in iterations
      if(iroots.lt.2)then
cnh         if(tisol.eq.1.and.tslos.ne.0.)then
cnh            do 22 i=0,n
cnh22          psios(i)=psi(i)-tslos*tcsl(i)
cnh            call uptake(psios,hk,psip,hkp,qex,qexp)
cnh         else
cnh            call uptake(psi,hk,psip,hkp,qex,qexp)
cnh         end if
cnh replaced with the following
         do 23 i=0,n
            psios(i) = psi(i)
            do 22 solnum=1,nsol
               psios(i)=psios(i)-tslos(solnum)*tcsl(solnum,i)
   22       continue
   23    continue
         call apswim_uptake(psios,hk,psip,hkp,qex,qexp)
cnh
      end if
      rex=0.
      do 25 i=0,n
25    rex=rex+qex(i)
***   get soil surface fluxes, taking account of top boundary condition
      if(itbc.eq.0)then
**       infinite conductance
         ifirst=0
         if(psi(0).lt.0.)then
            hsoil=exp(hcon*psi(0))
            res=resp*(hsoil-hair)/(1.-hair)
            respsi=resp*hcon*hsoil/(1.-hair)
         else
            res=resp
            respsi=0.
         end if
         if(isbc.eq.0)then
*           no ponding allowed
            h=0.
            q0=ron-res+hold/dt
            if(psi(0).lt.0..or.q0.lt.qs(0)+qex(0)+q(1))then
               q(0)=q0
               qp2(0)=-respsi*psip(0)
               roff=0.
            else
*              const zero potl
               ifirst=1
               q(0)=qs(0)+qex(0)+q(1)
               roff=q0-q(0)
               roffd=-qp2(1)
            end if
         else
*           runoff zero or given by a function
            if(psi(0).lt.0.)then
               h=0.
               roff=0.
               q(0)=ron-res+hold/dt
               qp2(0)=-respsi*psip(0)
            else
               h=psi(0)
               roff=0.
               roffd=0.
               if(isbc.eq.2)then
                  call apswim_runoff (t,h,roff,roffd)
               endif
               q(0)=ron-roff-res-(h-hold)/dt
               qp2(0)=(-roffd-respsi-1./dt)*psip(0)
            end if
         end if
      end if
      if(itbc.eq.1)then
**       const potl
         ifirst=1
         if(psi(0).lt.0.)then
            hsoil=exp(hcon*psi(0))
            res=resp*(hsoil-hair)/(1.-hair)
         else
            res=resp
         end if
         h=max(psi(0),0d0)
         q(0)=qs(0)+qex(0)+q(1)
*        flow to source of potl treated as "runoff" (but no bypass flow)
         roff=ron-res-(h-hold)/dt-q(0)
      end if
      if(itbc.eq.2)then
**       conductance given by a function
         q0=ron-resp+hold/dt
         if(isbc.eq.0)then
*           no ponding allowed
            ifirst=0
            h=0.
            call apswim_scond(t,h,g,gh)
            if(q0.gt.-g*psi(0))then
               res=resp
               respsi=0.
               q(0)=-g*psi(0)
               qp2(0)=-g*psip(0)
               roff=q0-q(0)
               roffd=-qp2(0)
            else
               hsoil=exp(hcon*psi(0))
               res=resp*(hsoil-hair)/(1.-hair)
               respsi=resp*hcon*hsoil/(1.-hair)
               q0=ron-res+hold/dt
               q(0)=q0
               qp2(0)=-respsi*psip(0)
               roff=0.
            end if
         else
*           runoff zero or given by a function
            call apswim_scond(t,h,g,gh)
            if(q0.gt.-g*psi(0))then
*              initialise h if necessary
               if(ifirst.eq.0)h=max(psi(0),0d0)
               ifirst=-1
               res=resp
               roff=0.
               roffd=0.
               if(isbc.eq.2.and.h.gt.0.)then
                  call apswim_runoff(t,h,roff,roffd)
               endif
               q(0)=g*(h-psi(0))
               qp1(0)=g+gh*(h-psi(0))
               qp2(0)=-g*psip(0)
               rhs(-1)=-(ron-roff-res-q(0)-(h-hold)/dt)
               b(-1)=-roffd-qp1(0)-1./dt
               c(-1)=-qp2(0)
            else
               ifirst=0
               h=0.
               roff=0.
               hsoil=exp(hcon*psi(0))
               res=resp*(hsoil-hair)/(1.-hair)
               respsi=resp*hcon*hsoil/(1.-hair)
               q(0)=ron-res+hold/dt
               qp2(0)=-respsi*psip(0)
            end if
         end if
      end if
*     bypass flow?
      qbp=0.
      qbpd=0.
      qbpp=0.
      hbp=0.
      qbps=0.
      qbpsp=0.
      if(ibp.ne.0)then
         if(psi(ibp).gt.0.)then
*           allow for change in storage
            hbp=psi(ibp)
            qbps=(hbp-hbpold)*sbp/dt
            qbpsp=psip(ibp)*sbp/dt
         else
            qbps=-hbpold*sbp/dt
         end if
         if(roff.gt.0..or.psi(ibp).gt.gf*(x(ibp)-x(0)))then
*           get bypass flow
            if(psi(ibp).gt.0.)then
               qbp=gbp*(gf*(x(ibp)-x(0))-psi(ibp))
               qbpp=-gbp*psip(ibp)
            else
               qbp=gbp*gf*(x(ibp)-x(0))
            end if
            if(roff.lt.qbp)then
               qbp=roff
               qbpp=0.
               qbpd=roffd
            end if
            roff=roff-qbp
         end if
      end if
***   bottom boundary condition
      if(ibbc.eq.0)then
**       zero matric potl gradient
         q(n+1)=(gf+gr)*hk(n)
         qp1(n+1)=(gf+gr)*hkp(n)
      else if(ibbc.eq.1)then
**       const potl
         ilast=n-1
         q(n+1)=q(n)-qs(n)-qex(n)
         if(ibp.eq.n)then
            q(n+1)=q(n+1)+qbp-qbps
            qbpd=0.
         end if
      else if(ibbc.eq.2)then
**       zero flux
         q(n+1)=0.
         qp1(n+1)=0.
      else if(ibbc.eq.3)then
**       seepage
cnh added to allow seepage to user potential at bbc
cnh         if(psi(n).ge.0.)then
         if(psi(n).ge.constant_potential) then
            q(n+1)=q(n)-qs(n)-qex(n)
            if(ibp.eq.n)q(n+1)=q(n+1)+qbp
            if(q(n+1).ge.0.)then
               ilast=n-1
               qbpd=0.
            else
               ilast=n
            end if
         end if
         if(ilast.eq.n)then
            q(n+1)=0.
            qp1(n+1)=0.
         end if
      end if
***   get Newton-Raphson equations
      i1=max(ifirst,0)
      k=i1-1
      xidif=.TRUE.
      do 30 i=i1,ilast
*        allow for two nodes at same depth
         xipdif=.TRUE.
         if(xidif)then
            k=k+1
            j=i+1
*           j is next different node, k is equation
            if(i.gt.0.and.i.lt.n-1)then
               if(x(i).eq.x(i+1))then
                  xipdif=.FALSE.
                  j=i+2
                  q(i+1)=((x(j)-x(i))*q(i)+(x(i)-x(i-1))*q(j))/
     1                   (x(j)-x(i-1))
               end if
            end if
            rhs(k)=-(q(i)-q(j))
            a(k)=qp1(i)
            b(k)=qp2(i)-qp1(j)
            c(k)=-qp2(j)
         end if
         rhs(k)=rhs(k)+qs(i)+qex(i)
         b(k)=b(k)-qsp(i)
*        bypass flow?
         if(ibp.ne.0.and.i.eq.ibp)then
            rhs(k)=rhs(k)-qbp+qbps
            b(k)=b(k)+qbpp-qbpsp
         end if
         if(iroots.eq.0)then
*            a(k)=a(k)-qexp(1,i)
            b(k)=b(k)-qexp(2,i)
*            c(k)=c(k)-qexp(3,i)
         else
            iroots=2
         end if
         xidif=xipdif
30    continue
      ibegin=ifirst
      iend=k
      end
* =====================================================================
      double precision function apswim_wpf ()
* =====================================================================
*     Short description:
*     gets water present in profile
*
      implicit none
 
*     Global Variables
      include 'apswim.inc'
 
 
*     Subroutine Arguments
*     none
 
*     Internal Variables
      integer i
      double precision wpf
 
*     Constant Values
*     none
 
*
      wpf=0.
      do 10 i=0,n
10    wpf=wpf+th(i)*dx(i)
 
      apswim_wpf = wpf
 
      end
* =====================================================================
      subroutine apswim_getsol(solnum,a,b,c,d,rhs,c1,c2,fail)
* =====================================================================
*     Short description:
*     get and solve solute balance eqns
*
 
      implicit none
 
*     Global Variables
      include 'apswim.inc'
      double precision apswim_slupf
 
 
*     Subroutine Arguments
      integer          solnum
      double precision a(0:n)
      double precision b(0:n)
      double precision c(0:n)
      double precision d(0:n)
      double precision rhs(0:n)
      double precision c1(0:n)
      double precision c2(0:n)
      logical          fail
 
*     Internal Variables
      double precision accept
      double precision aq
      double precision cp
      double precision csl0
      double precision csln
      double precision csltemp(0:M)
      double precision d1
      double precision d2
      double precision dabs
      double precision dfac
      double precision dmax
      double precision dum
      double precision exco1
      double precision exco2
      double precision exco3
      double precision fq
      double precision fqc
      integer          i
      integer          itcnt
      integer          j
      integer          k
      integer          kk
      integer          neq
      logical          nonlin
      double precision rinf
      double precision rovr
      double precision rslout
      double precision rslovr
      double precision thav
      double precision thi
      double precision w1
      double precision w2
      double precision wt
      double precision wtime
      double precision wtime1

*     Constant Values
      integer    itmax
      parameter (itmax=20)
 
*
*     surface solute balance - assume evap. (res) comes from x0 store
      rovr=roff+qbp
      rinf=q(0)+res
      if(rinf.gt.min(ersoil,ernode))then
         cslsur(solnum)=(rslon(solnum)+hold*cslsur(solnum)/dt)
     :                     /(rovr+rinf+h/dt)
         qsl(solnum,0)=rinf*cslsur(solnum)
         rslovr=rovr*cslsur(solnum)
         if(slsur(solnum).gt.0.)then
            if(cslsur(solnum).lt.slsci(solnum))then
               if(slsur(solnum).gt.
     :                 rinf*dt*(slsci(solnum)-cslsur(solnum)))then
                  qsl(solnum,0)=rinf*slsci(solnum)
                  slsur(solnum)=slsur(solnum)-rinf*dt*(slsci(solnum)
     :                          -cslsur(solnum))
               else
                  qsl(solnum,0)=rinf*cslsur(solnum)+slsur(solnum)/dt
                  slsur(solnum)=0.
               end if
            end if
            if(cslsur(solnum).lt.slscr(solnum))then
               if(slsur(solnum).gt.
     :              rovr*dt*(slscr(solnum)-cslsur(solnum)))then
                  rslovr=rovr*slscr(solnum)
                  slsur(solnum)=slsur(solnum)-rovr*dt*(slscr(solnum)
     :                          -cslsur(solnum))
               else
                  rslovr=rovr*cslsur(solnum)+slsur(solnum)/dt
                  slsur(solnum)=0.
               end if
               if(slsur(solnum).gt.h*(slscr(solnum)-cslsur(solnum)))then
                  slsur(solnum)=slsur(solnum)
     :                          -h*(slscr(solnum)-cslsur(solnum))
                  cslsur(solnum)=slscr(solnum)
               else
                  if(h.gt.0)cslsur(solnum)=cslsur(solnum)
     :                                     +slsur(solnum)/h
                  slsur(solnum)=0.
               end if
            end if
         end if
      else
         cslsur(solnum)=0.
         qsl(solnum,0)=0.
         rslovr=0.
      end if
*     get eqn coeffs
*     get production and storage components
cnh      call slprod
      rslprd(solnum)=0.
      do 45 i=0,n
         c1(i)=csl(solnum,i)
         thi=th(i)
cnh         j=indxsl(solnum,i)
         j = i
         rslprd(solnum)=rslprd(solnum)+qslprd(solnum,i)
         nonlin=.FALSE.

*Peter's CHANGE 21/10/98 to ensure zero exchange is treated as linear
*         if (fip(solnum,j).eq.1.) then
         if ((ex(solnum,j).eq.0.).or.(fip(solnum,j).eq.1.)) then
*           linear exchange isotherm
            c2(i)=1.
            exco1=ex(solnum,j)
            exco2=betaex(solnum,j)
         else
*           nonlinear Freundlich exchange isotherm
            nonlin=.TRUE.
            c2(i)=0.
            if(c1(i).gt.0.)c2(i)=c1(i)**(fip(solnum,j)-1.)
            exco1=ex(solnum,j)*fip(solnum,j)*c2(i)
            exco2=betaex(solnum,j)*fip(solnum,j)*c2(i)
            exco3=betaex(solnum,j)*(1.-fip(solnum,j))*c2(i)
         end if
         b(i)=(-(thi+exco1)/dt+alpha(solnum,j)*thi+exco2)*dx(i)-
     1        apswim_slupf(1,solnum)*qex(i)
cnh     1        slupf(solnum)*qex(i)
         rhs(i)=-qslprd(solnum,i)
     :          -(csl(solnum,i)*((thold(i)+exco1)/dt+exco3))*dx(i)
         qsls(solnum,i)=
     :          -(csl(solnum,i)*(thold(i)+ex(solnum,j)*c2(i))/dt)*dx(i)
45    continue
*     get dispersive and convective components
*        use central diffs in time for convection, backward diffs for rest
*        use central diffs in space, but for convection may need some
*        upstream weighting to avoid instability
      do 50 i=1,n
         if(x(i-1).ne.x(i))then
            thav=0.5*(th(i-1)+th(i))
            aq=abs(q(i))
            dc(solnum,i)=dcon(solnum)*(thav-dthc(solnum))**dthp(solnum)
cnh     1            +0.5*(dis(solnum,indxsl(solnum,i-1))
     1            +0.5*(dis(solnum,i-1)
cnh     :            +dis(solnum,indxsl(solnum,i)))*(aq/thav)**disp(solnum)
     :            +dis(solnum,i))*(aq/thav)**disp(solnum)
            dfac=thav*dc(solnum,i)/(x(i)-x(i-1))
            if(slswt.ge..5.and.slswt.le.1.)then
*              use fixed space weighting on convection
               w1=sign(2.*slswt,q(i))
            else
*              use central diffs for convection if possible, else use
*                 just enough upstream weighting to avoid oscillation
*                 user may increase acceptable level for central diffs
*                 by setting slswt < -1
               accept=max(1d0,-slswt)
               wt=0.
               if(aq.ne.0.)wt=sign(max(0d0,1.-2.*accept*dfac/aq),q(i))
               w1=1.+wt
            end if
            w2=2.-w1

*Peter's CHANGE 21/10/98 to remove/restore Crank-Nicolson time weighting 
*for convection
*            fq=.25*q(i)
*            fqc=fq*(w1*csl(solnum,i-1)+w2*csl(solnum,i))
*            wtime=0.25D0
*            wtime1=1.0D0
            wtime=0.5D0
            wtime1=0.0D0
            fq=wtime*q(i)
            fqc=wtime1*fq*(w1*csl(solnum,i-1)+w2*csl(solnum,i))

*           get convective component from old time level
            qsl(solnum,i)=fqc
            b(i-1)=b(i-1)-dfac-fq*w1
            c(i-1)=dfac-fq*w2
            a(i)=dfac+fq*w1
            b(i)=b(i)-dfac+fq*w2
            rhs(i-1)=rhs(i-1)+fqc
            rhs(i)=rhs(i)-fqc
         end if
50    continue
*     allow for bypass flow
      qslbp(solnum)=0.
      if(ibp.ne.0)then
         if(qbp.ge.0.)then
            qslbp(solnum)=qbp*cslsur(solnum)
            rhs(ibp)=rhs(ibp)-qslbp(solnum)
*           note that if rinf were -ve, old cslsur would be used, except
*           that then qbp would probably be -ve
         else
            b(ibp)=b(ibp)-qbp
         end if
      end if
*     impose boundary conditions
      if(itbc.eq.1)then
*        constant concentration
         k=1
      else
         k=0
         rhs(0)=rhs(0)-qsl(solnum,0)
         if(rinf.lt.-min(ersoil,ernode))then
            b(0)=b(0)+.5*rinf
            rhs(0)=rhs(0)-.5*rinf*csl(solnum,0)
         end if
      end if
      if(ibbc.eq.1)then
*        constant concentration
cnh
         csl(solnum,n) = cslgw(solnum)
cnh
         rhs(n-1)=rhs(n-1)-c(n-1)*csl(solnum,n)
         neq=n
 
      else
*        convection only
         b(n)=b(n)-.5*q(n+1)
         rhs(n)=rhs(n)+.5*q(n+1)*csl(solnum,n)
         neq=n+1
      end if
*     allow for two nodes at same depth
      j=0
      do 60 i=1,n
         if(x(i-1).ne.x(i))then
            j=j+1
            a(j)=a(i)
            b(j)=b(i)
            rhs(j)=rhs(i)
            c(j-1)=c(i-1)
         else
            b(j)=b(j)+b(i)
            rhs(j)=rhs(j)+rhs(i)
         end if
60    continue
*     save old csl(0),csl(n)
      csl0=csl(solnum,0)
      csln=csl(solnum,n)
      neq=neq-(n-j)
      itcnt=0
*     solve for concentrations
62    continue
cnh      call thomas(neq,0,a(k),b(k),c(k),rhs(k),dum,d(k),csl(solnum,k),
cnh     :            dum,fail)
      do 63 i=0,n
        csltemp(i) = csl(solnum,i)
   63 continue
      call apswim_thomas
     :      (neq,0,a(k),b(k),c(k),rhs(k),dum,d(k),csltemp(k),dum,fail)
      do 64 i=0,n
        csl(solnum,i) = csltemp(i)
   64 continue
 
cnh end
      itcnt=itcnt+1
      slwork=slwork+neq
      if(fail)go to 90
      j=k+neq-1
      if(ibbc.ne.1)then
         csl(solnum,n)=csl(solnum,j)
         j=j-1
      end if
      do 65 i=n-1,1,-1
         if(x(i).ne.x(i+1))then
            csl(solnum,i)=csl(solnum,j)
            j=j-1
         else
            csl(solnum,i)=csl(solnum,i+1)
         end if
65    continue
      if(nonlin)then
*        test for convergence
         dmax=0.
         do 66 i=0,n
            dabs=abs(csl(solnum,i)-c1(i))
            if(dmax.lt.dabs)dmax=dabs
66       continue
         if(dmax.gt.slcerr)then
            if(itcnt.eq.itmax)then
               fail=.TRUE.
               go to 90
            end if
*           keep iterating using Newton-Raphson technique
*           next c^fip for Freundlich isotherm is approximated as
*              cn^fip=c^fip+fip*c^(fip-1)*(cn-c)
*                    =fip*c^(fip-1)*cn+(1-fip)*c^fip
            j=0
            do 67 i=0,n
               if(i.gt.0.and.x(i-1).ne.x(i))j=j+1
cnh               kk=indxsl(solnum,i)
               kk = i
               if(fip(solnum,kk).ne.1.)then
                  cp=0.
                  if(csl(solnum,i).gt.0.)then
                     cp=csl(solnum,i)**(fip(solnum,kk)-1.)
                  endif
                  d1=fip(solnum,kk)*(cp-c2(i))
                  d2=(1.-fip(solnum,kk))*(csl(solnum,i)*cp-c1(i)*c2(i))
                  c1(i)=csl(solnum,i)
                  c2(i)=cp
                  b(j)=b(j)-(ex(solnum,kk)/dt-betaex(solnum,kk))
     :                 *d1*dx(i)
                  rhs(j)=rhs(j)+(ex(solnum,kk)/dt-betaex(solnum,kk))
     :                 *d2*dx(i)
               end if
67          continue
            go to 62
         end if
      end if
*     get surface solute balance?
      if(rinf.lt.-min(ersoil,ernode))then
*        flow out of surface
*CHANGES 6/11/98 to remove/restore Crank-Nicolson time weighting for convection
*----- 
*         qsl(solnum,0)=.5*rinf*(csl0+csl(solnum,0))
         qsl(solnum,0)=.5*rinf*(wtime1*csl0+4D0*wtime*csl(solnum,0))

         rslout=-qsl(solnum,0)
         if(slsur(solnum).gt.0.)then
*           allow for surface applied solute
            if(csl(solnum,0).lt.slsci(solnum))then
               if(slsur(solnum).gt.
     :                  -rinf*dt*(slsci(solnum)-csl(solnum,0)))then
                  rslout=-rinf*slsci(solnum)
                  slsur(solnum)=slsur(solnum)
     :                          +rinf*dt*(slsci(solnum)-csl(solnum,0))
               else
                  rslout=rslout+slsur(solnum)/dt
                  slsur(solnum)=0.
               end if
            end if
         end if
*        get surface solute balance
         cslsur(solnum)=(rslon(solnum)+rslout+hold*cslsur(solnum)/dt)
     :                   /(rovr+h/dt)
         rslovr=rovr*cslsur(solnum)
      end if
*     allow for bypass flow
      if(ibp.ne.0.and.qbp.lt.0.)qslbp(solnum)=qbp*csl(solnum,ibp)
      rsloff(solnum)=rslovr-qslbp(solnum)
*     get solute fluxes
      do 70 i=1,n
         if(x(i-1).ne.x(i))then
            dfac=0.5*(th(i-1)+th(i))*dc(solnum,i)/(x(i)-x(i-1))
            aq=abs(q(i))
            accept=max(1d0,-slswt)
            wt=0.
            if(aq.ne.0.)wt=sign(max(0d0,1.-2.*accept*dfac/aq),q(i))
*Peter's CHANGES 21/10/98 to remove/restore Crank-Nicolson time weighting
*for convection
*            qsl(solnum,i)=qsl(solnum,i)
*     :                    +.25*q(i)*((1.+wt)*csl(solnum,i-1)
*     :                    +(1.-wt)*csl(solnum,i))
*     1                    +dfac*(csl(solnum,i-1)-csl(solnum,i))
            qsl(solnum,i)=qsl(solnum,i)
     :                    +wtime*q(i)*((1.+wt)*csl(solnum,i-1)
     :                    +(1.-wt)*csl(solnum,i))
     1                    +dfac*(csl(solnum,i-1)-csl(solnum,i))
         end if

70    continue
      do 75 i=2,n-1
         if(x(i-1).eq.x(i))then
            qsl(solnum,i)=
     :               (dx(i)*qsl(solnum,i-1)+dx(i-1)*qsl(solnum,i+1))
     :               /(dx(i-1)+dx(i))
         end if
75    continue
      slp(solnum)=0.
      rslex(solnum)=0.
      rsldec(solnum)=0.
      do 80 i=0,n
cnh         j=indxsl(solnum,i)
         j = i
         cp=1.
         if(fip(solnum,j).ne.1.)then
            cp=0.
            if(csl(solnum,i).gt.0.)cp=csl(solnum,i)**(fip(solnum,j)-1.)
         end if
         cslt(solnum,i)=(th(i)+ex(solnum,j)*cp)*csl(solnum,i)
         slp(solnum)=slp(solnum)+cslt(solnum,i)*dx(i)
cnh         rslex(solnum)=rslex(solnum)+qex(i)*csl(solnum,i)*slupf(solnum)
         rslex(solnum)=rslex(solnum)+qex(i)*csl(solnum,i)
     :                *apswim_slupf(1,solnum)
         rsldec(solnum)=rsldec(solnum)
     :           -(alpha(solnum,j)*th(i)+betaex(solnum,j)*cp)
     :           *dx(i)*csl(solnum,i)
         qsls(solnum,i)=qsls(solnum,i)+
     :           (csl(solnum,i)*(thold(i)+ex(solnum,j)*cp)/dt)*dx(i)
80    continue
      if(ibbc.eq.1)then
*        constant concentration
cnh         j=indxsl(solnum,n)
         j = n
         qsl(solnum,n+1)=qsl(solnum,n)-qsls(solnum,n)
cnh     :                  -qex(n)*csl(solnum,n)*slupf(solnum)
     :                  -qex(n)*csl(solnum,n)*apswim_slupf(1,solnum)
     :                  +qslprd(solnum,n)+
     :                  (alpha(solnum,j)*th(n)+betaex(solnum,j)*cp)
     :                  *dx(n)*csl(solnum,n)
      else
*        convection only
*CHANGES 6/11/98 to remove/restore Crank-Nicolson time weighting for convection
*----- 
*         qsl(solnum,n+1)=.5*q(n+1)*(csln+csl(solnum,n))
         qsl(solnum,n+1)=.5*q(n+1)*(wtime1*csln+4D0*wtime*csl(solnum,n))

      end if
90    continue
      end
* =====================================================================
      subroutine apswim_thomas(n,ib,a,b,c,rhs,rb,d,v,vb,fail)
* =====================================================================
*     Short description:
*     Thomas algorithm for solving tridiagonal system of eqns
*     Allow for bypass flow if ib from 1 to n and rb nonzero
*
      implicit none
 
*     Global Variables
 
 
*     Subroutine Arguments
 
      integer          n
      double precision a(n)
      double precision b(n)
      double precision c(n)
      double precision d(n)
      logical          fail
      integer          ib
      double precision rb
      double precision rhs(n)
      double precision v(n)
      double precision vb(n)
 
*     Local Variables
 
      double precision fac
      integer          i
      double precision piv
 
*     Constant Values
*     none
 
*
      if(b(1).eq.0.)go to 60
      piv=b(1)
      v(1)=rhs(1)/piv
      do 10 i=2,n
          d(i)=c(i-1)/piv
          piv=b(i)-a(i)*d(i)
          if(piv.eq.0.)go to 60
          v(i)=(rhs(i)-a(i)*v(i-1))/piv
10    continue
      do 20 i=n-1,1,-1
      v(i)=v(i)-d(i+1)*v(i+1)
20    continue
      if(ib.ge.1.and.ib.le.n.and.rb.ne.0.)then
         vb(1)=0.
         if(ib.eq.1)vb(1)=rb/b(1)
         do 30 i=2,n
            if(i.lt.ib)then
               vb(i)=0.
            else
               piv=b(i)-a(i)*d(i)
               if(i.eq.ib)vb(i)=rb/piv
               if(i.gt.ib)vb(i)=-a(i)*vb(i-1)/piv
            end if
30       continue
         do 40 i=n-1,1,-1
40       vb(i)=vb(i)-d(i+1)*vb(i+1)
         fac=v(1)/(1d0+vb(1))
         do 50 i=1,n
50       v(i)=v(i)-fac*vb(i)
      end if
      fail=.FALSE.
      go to 70
60    fail=.true.
70    continue
      end
* =====================================================================
      subroutine apswim_trans(p,psi,psip,psipp)
* =====================================================================
*     Short description:
*     gets psi and its partial derivitives
*
      Implicit none
 
*     Global Variables
 
 
*     Subroutine Arguments
      double precision p
      double precision psi
      double precision psip
      double precision psipp
 
*     Internal Variables
 
      double precision ep
      double precision emp
      double precision sinhp
      double precision coshp
      double precision v
 
* Constants
      double precision psi0
      parameter (psi0=-50d0)
 
      double precision psi1
      parameter (psi1=psi0/10d0)
 
*
      if(p.lt.0d0)then
         ep=exp(p)
         emp=1d0/ep
         sinhp=0.5d0*(ep-emp)
         coshp=0.5d0*(ep+emp)
         v=psi1*sinhp
         psi=psi0-v
         psip=-psi1*coshp
         psipp=-v
      else
         psi=psi0-psi1*p
         psip=-psi1
         psipp=0d0
      end if
 
      end
*
***   water functions
* =====================================================================
      double precision function apswim_pf(psi)
* =====================================================================
*     Short description:
*     returns transform p
*
      implicit none
 
*     Global Variables
*     none
 
 
*     Subroutine Arguments
      double precision psi
 
*     Internal Variables
      double precision v
 
*     Constant Values
      double precision psi0
      parameter (psi0=-50d0)
 
      double precision psi1
      parameter (psi1=psi0/10d0)
 
*
      v=-(psi-psi0)/psi1
      if(psi.lt.psi0)then
         apswim_pf=log(v+sqrt(v**2+1d0))
      else
         apswim_pf=v
      end if
 
      end
* ===================================================================
      subroutine apswim_uptake(tpsi,thk,tpsip,thkp,tqex,tqexp)
* ===================================================================
*     gets flow rates to roots and total water extraction rates
*
*  Note some variables renamed using t prefix because of clash with
*  common variables.
* psi->tpsi
* psip->tpsip
* hk->thk
* hkp->thkp
* qex->tqex
* qexp->tqexp
* q->tq
* tr->ttr
 
      implicit none
 
      include 'apswim.inc'
 
*     Subroutine Arguments
 
      double precision tpsi(0:n),thk(0:n),tpsip(0:n),thkp(0:n),tqex(0:n)
     1                 ,tqexp(3,0:n)
 
 
*     Local Variable
      double precision a
      double precision b
      logical          change
      double precision derp
      double precision g(0:M)
      integer          i
      integer          iveg
      integer          j
      integer          k
      double precision psix
      double precision qhk
      double precision qpsi
      logical          stress
      double precision ttr
      double precision tq
 
*     set root conductance gr (alter as required)
c      double precision gr  ! cm/h
c      parameter (gr=1.4d-7)
 
*
      do 10 i=0,n
         tqex(i)=0.
         do 10 j=1,3
         tqexp(j,i)=0.
cnh
        do 5 k=1,nveg
           qr(i,k) = 0d0
 5      continue
cnh
10    continue
      do 100 iveg=1,nveg
*        find transpiration rates
         rt(iveg)=0.
         ttr=rtp(iveg)
         if(ttr.gt.0.)then
            psix=psimin(iveg)
*           get soil->xylem conductances
            a=0.
            b=0.
            do 20 i=0,n
               g(i)=0.
               if(tpsi(i).gt.psix)then
cnh root conductance is not an input
cnh                  g(i)=1./(rc(i,iveg)/thk(i)+1./(gr*rld(i,iveg)*dx(i)))
                  g(i)=1./(rc(i,iveg)/thk(i)+1./
     :                     (root_conductance(iveg)*rld(i,iveg)*dx(i)))
               end if
               a=a+g(i)*tpsi(i)
               b=b+g(i)
20          continue
            if(b.eq.0.)then
               stress=.TRUE.
            else if((a-ttr)/b.lt.psix)then
               stress=.TRUE.
            else
               stress=.FALSE.
            end if
            if(.not.stress)then
*              get xylem potl
30             continue
                  change=.FALSE.
                  psix=(a-ttr)/b
                  do 40 i=0,n
                     if(tpsi(i).lt.psix.and.g(i).ne.0.)then
                        change=.TRUE.
                        a=a-g(i)*tpsi(i)
                        b=b-g(i)
                        g(i)=0.
                     end if
40                continue
               if(change)go to 30
            end if
            do 50 i=0,n
               if(g(i).ne.0.)then
                  tq=g(i)*(tpsi(i)-psix)
                  tqex(i)=tqex(i)+tq
*                 get partial derivs of tqex at i-1, i, i+1 wrt p
                  qpsi=g(i)
                  qhk=g(i)*rc(i,iveg)*tq/thk(i)**2
                  if(.not.stress)then
                     derp=qpsi*tpsip(i)+qhk*thkp(i)
                     if(i.gt.0)tqexp(3,i-1)=tqexp(3,i-1)-g(i-1)*derp/b
                     if(i.lt.n)tqexp(1,i+1)=tqexp(1,i+1)-g(i+1)*derp/b
                     qpsi=qpsi*(1d0-g(i)/b)
                     qhk=qhk*(1d0-g(i)/b)
                  end if
                  tqexp(2,i)=tqexp(2,i)+qpsi*tpsip(i)+qhk*thkp(i)
                  rt(iveg)=rt(iveg)+tq
                  qr(i,iveg)=tq
               else
                  qr(i,iveg)=0.
               end if
50          continue
         end if
100   continue
      end
* =====================================================================
      subroutine apswim_watvar(ix,tp,tpsi,psip,psipp,tth,thp,thk,hkp)
* =====================================================================
*     Short Description:
*     calculates water variables from transform value p at grid point ix
*     using cubic interpolation between given values of water content wc,
*     log10 conductivity hkl, and their derivatives wcd, hkld with respect
*     to log10 suction sl
*
*     nih - some local variables had the same name as globals so I had
*     to rename them. I added a t (for temp) to start of name for
*     psi, hk, p, th, x, dx,dc
 
      implicit none
 
*     notes
 
*         dTheta     dTheta       d(log Psi)
*         ------ = ----------  X  ---------
*           dP     d(log Psi)        d P
 
*                    dTheta        d Psi           1
*                = ----------  X  -------  X ------------
*                  d(log Psi)       d P       ln(10).Psi
 
 
*         dHK          dHK       d(log Psi)
*        ------  = ----------  X ----------
*          dP      d(log Psi)       d P
 
*                   ln(10).HK   d(log(HK))     dPsi        1
*                =  --------- X ----------  X ------ X ----------
*                        1      d(log(Psi))     dP     ln(10).Psi
 
*                    HK       d(log(HK))     dPsi
*                =  -----  X  ----------  X  ----
*                    Psi      d(log(Psi))     dP
 
*     note:- d(log(y)/dx) = 1/y . dy/dx
*
*     Therefore:-
*
*            d(log10(y))/dx = d(ln(y)/ln(10))/dx
*                           = 1/ln(10) . d(ln(y))/dx
*                           = 1/ln(10) . 1/y . dy/dx
 
*     Global Variables
      include 'apswim.inc'
 
 
*     Subroutine Arguments
      integer ix
      double precision tp              ! P - transform of Psi
      double precision tpsi            ! Psi for the given P
      double precision psip            ! dPsi/dP  - 1st derivative
      double precision psipp           ! dPsip/dP - 2nd derivative
      double precision tth             ! water content for given Psi
      double precision thp             ! dTheta/dP
      double precision thk             ! hydraulic conductivity
      double precision hkp             ! d(thk)/dP
 
*     Internal Variables
      double precision hklg
      double precision hklgd
      double precision hkv
      double precision phi
      double precision thd
      double precision thsat
 
*     Constant Values
      double precision al10
      parameter (al10=2.3025850929940457d0)
 
      double precision vcon1
      parameter (vcon1=7.28d-9)
 
      double precision vcon2
      parameter (vcon2=7.26d-7)
*
 
      call apswim_trans(tp,tpsi,psip,psipp)
 
cnh   assume no hysteresis
cnh      jhys=0...........etc  removed
 
cnh - got rid of old calculation for interpolating inputs - used the new
cnh   method.  note apswim_interp allows for hysteresis in calculating
cnh   moisture characteristic and hk curve.
      call apswim_interp (ix,tpsi,tth,thd,hklg,hklgd)
 
      thk=exp(al10*hklg)
 
      if(tpsi.ne.0d0)then
         thp=(thd*psip)/(al10*tpsi)
         hkp=(thk*hklgd*psip)/tpsi
      end if
 
cnd no hysteresis
c      if(jhys.ne.0)then
c         thp=tdc*thp
c         hkp=tdc*hkp
c      end if
 
      thsat = wc(ix,1)  ! NOTE: this assumes that the wettest wc is
                        ! first in the pairs of log suction vs wc
 
      if(ivap.ne.0)then
*        add vapour conductivity hkv
         phi=thsat/.93-tth
         hkv=vcon1*phi*exp(vcon2*tpsi)
         thk=thk+hkv
         hkp=hkp+hkv*(vcon2*psip-thp/phi)
      end if
 
      end
 
 
* =====================================================================
      subroutine apswim_scond(ttt,tth,g,gh)
* =====================================================================
*     Short Description:
*     gets soil surface conductance g and derivative gh
*
*     t was renamed to ttt as t already exists in common
*     h was renamed to tth as h already exists in common
 
      implicit none
 
*     Global Variables
      include 'apswim.inc'
 
 
*     Subroutine Arguments
      double precision ttt
      double precision tth
      double precision g
      double precision gh
 
*     Internal Variables
*     none
 
*     Constant Values
*     none
*
cnh      g=g0
cnh      gh=0.
cnh      if(grc.ne.0..and.ttt.gt.tzero)then
cnhcnh         g=g0+(g1-g0)*exp(-(eqrain(ttt)-eqr0)/grc)
cnh         g=g0+(g1-g0)*exp(-(apswim_eqrain(ttt)-eqr0)/grc)
cnh      end if
 
      g = gsurf
      gh = 0d0
 
      return
      end
 
 
* =====================================================================
      subroutine apswim_runoff(ttt,tth,ttroff,roffh)
* =====================================================================
*     Short Description:
*     gets runoff rate
*
*     t was renamed to ttt as t already exists in common
*     h was renamed to tth as h already exists in common
*     roff was renamed to ttroff as roff already exists in common
 
      implicit none
 
*     Global Variables
      include 'apswim.inc'
 
 
*     Subroutine Arguments
      double precision ttt
      double precision tth
      double precision ttroff
      double precision roffh
 
*     Internal Variables
      double precision v
 
*     Constant Values
*     none
 
*
cnh      hmin=hm0
cnh      if(hrc.ne.0..and.ttt.gt.tzero)then
cnhcnh         hmin=hm0+(hm1-hm0)*exp(-(eqrain(ttt)-eqr0)/hrc)
cnh         hmin=hm0+(hm1-hm0)*exp(-(apswim_eqrain(ttt)-eqr0)/hrc)
cnh      end if
 
      if(tth.gt.hmin)then
         v=roff0*(tth-hmin)**(roff1-1d0)
         ttroff=v*(tth-hmin)
         roffh=roff1*v
      else
         ttroff=0d0
         roffh=0d0
      end if
 
      return
      end
 
 
* =====================================================================
      integer function ihys(hyscon,d,x0,x0new,x,dc)
* =====================================================================
*     Short description:
*     allows drying curve to be used for scanning or wetting
*     d is distance between curves on linear (d<0) or log (d>0) scale_of
*     adjusts x (psi or log10(-psi)) and gets derivative correction dc
*     gets new reference point x0new on drying curve if necessary
*     returns 0, 1 or 2 for drying, scanning or wetting curve
*
      implicit none
 
*     Global Variables
 
 
*     Subroutine Arguments
cnh NOT SURE ARGUMENT TYPE IS CORRECT.
c      real d
      double precision d
      double precision dc
      double precision hyscon
      double precision x
c      real x0
c      real x0new
      double precision x0
      double precision x0new
 
*     Internal Variables
 
      double precision z
 
*     Constant Values
*     none
 
*
 
      print*,'hysteresis is in effect!!!!!!!'
      ihys=0
      x0new=x0
      dc=1d0
      if(d.eq.0.)return
      z=(x-x0)/(-d*hyscon)
      if(z.le.0d0)then
         x0new=x
      else if(z.lt.1d0)then
         ihys=1
         x=x0-d*z*(hyscon+z*(2d0*z-3d0))
         dc=1d0+6d0*z*(z-1d0)/hyscon
      else
         ihys=2
         x0new=x+d*hyscon
         x=x+d
      end if
 
      end
*
      subroutine map(n,x,y,m,u,v)
*     maps concentration in y into v so that integral is conserved
*     x and u give intervals corresponding to y and v values
      integer n,m
      double precision x(*),y(*),u(*),v(*)
      double precision sx, sy, su, sv,w
      logical again
cnh added following declarations
      integer i,j
 
      sx=0.
      sy=0.
      j=0
      su=u(1)
      sv=0.
      do 20 i=1,n
         sx=sx+x(i)
         sy=sy+y(i)*x(i)
10       continue
            again=.FALSE.
            if((j.lt.m).and.(sx.ge.su.or.i.eq.n))then
               j=j+1
               w=sy-(sx-su)*y(i)
               v(j)=(w-sv)/u(j)
               if(j.lt.m)then
                  su=su+u(j+1)
                  sv=w
                  again=.TRUE.
               end if
            end if
         if(again)go to 10
20    continue
      end
