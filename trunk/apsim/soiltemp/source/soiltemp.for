
 !     ===========================================================
      Recursive
     :subroutine AllocInstance (InstanceName, InstanceNo)
 !     ===========================================================
      use SoilTempModule
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      character InstanceName*(*)       ! (INPUT) name of instance
      integer   InstanceNo             ! (INPUT) instance number to allocate

*+  Purpose
*      Module instantiation routine.

*+  Mission Statement
*     Instantiate routine

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
     :subroutine FreeInstance (anInstanceNo)
 !     ===========================================================
      use SoilTempModule
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer anInstanceNo             ! (INPUT) instance number to allocate

*+  Purpose
*      Module de-instantiation routine.

*+  Mission Statement
*     De-Instantiate routine

 !- Implementation Section ----------------------------------

      deallocate (Instances(anInstanceNo)%gptr)
      deallocate (Instances(anInstanceNo)%eptr)
      deallocate (Instances(anInstanceNo)%pptr)
      deallocate (Instances(anInstanceNo)%cptr)

      return
      end

 !     ===========================================================
      Recursive
     :subroutine SwapInstance (anInstanceNo)
 !     ===========================================================
      use SoilTempModule
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer anInstanceNo             ! (INPUT) instance number to allocate

*+  Purpose
*      Swap an instance into the global 'g' pointer

*+  Mission Statement
*     Swap an instance into global pointer

 !- Implementation Section ----------------------------------

      g => Instances(anInstanceNo)%gptr
      e => Instances(anInstanceNo)%eptr
      p => Instances(anInstanceNo)%pptr
      c => Instances(anInstanceNo)%cptr

      return
      end

* ====================================================================
      Recursive
     :subroutine Main (Action, Data_string)
* ====================================================================
      use SoilTempModule
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
       character Action*(*)            ! Message action to perform
       character Data_string*(*)       ! Message data

*+  Purpose
*      This routine is the interface between the main system and the
*      soiltemp module.

*+  Mission Statement
*     Handles communications for Soiltemp

*+  Changes
*    ????
*    070896 jngh added message_unused call at end

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'apsim_soiltemp')

*- Implementation Section ----------------------------------
      call push_routine (myname)
      if (Action.eq.ACTION_Init) then
         call soiltemp_zero_variables ()
         call soiltemp_Init ()
 
      elseif (Action.eq.ACTION_Prepare) then
         call soiltemp_prepare ()
 
      else if (Action.eq.ACTION_Process) then
         call soiltemp_process ()
!         call soiltemp_set_other_variables ()
 
      else if (Action.eq.ACTION_Get_variable) then
         call soiltemp_Send_my_variable (Data_string)
 
!      else if (Action.eq.ACTION_Set_variable) then
!         call soiltemp_Set_my_variable (data_string)
 
!      elseif (Action.eq.ACTION_Post) then
               ! do any post processing
!         call soiltemp_post ()
 
!      elseif (action.eq.ACTION_event) then
!               ! act upon an event
!         call soiltemp_capture_event ('data')
 
!      elseif (Action.eq.ACTION_End_run) then
               ! clean up at end of run
!         call soiltemp_end_run ()
      elseif (Action.eq.ACTION_Create) then
         call soilTemp_zero_all_globals ()
 
      else
         ! Don't use message
         call Message_unused ()
 
      endif
 
      call pop_routine (myname)
      return
      end



* ====================================================================
      Recursive
     :subroutine soiltemp_Init ()
* ====================================================================
      use SoilTempModule
      Use infrastructure
      implicit none

*+  Purpose
*      Initialise soiltemp module

*+  Mission statement
*     Initialise soiltemp

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'soiltemp_init')

*+  Local Variables
       character Event_string*40       ! String to output

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
 
      ! Notify system that we have initialised
 
      Event_string = 'Initialising : ' 
      call write_string (Event_string)
 
      ! Get all constants from parameter file
 
      call soiltemp_get_ini_variables ()
 
      call soiltemp_read_constants ()
 
      ! Get all parameters from parameter file
 
      call soiltemp_read_param ()
 
      call pop_routine (myname)
      return
      end

*     ================================================================
      Recursive
     :subroutine soiltemp_process ()
*     ================================================================
      use SoilTempModule
      Use infrastructure
      implicit none

*+  Purpose
*      perform actions for current day.

*+  Mission Statement
*     Performs actions for the current day

*+  Changes
*      1/6/94  vos programmed

*+  Calls
!      include   'const.cmn'            ! constant definitions

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'soiltemp_process')

*+  Local Variables
      integer i
      real soiltemp_InterpTemp
      integer*4 time

*- Implementation Section ----------------------------------
 
      call push_routine (myname)

      g%mint_yesterday = e%mint 
      g%maxt_yesterday = e%maxt 
      call soiltemp_get_other_variables ()
   
!zero the different temperatures
      do i=1,g%nz
         g%mint_soil(i) = 0.0
         g%maxt_soil(i) = 0.0
         g%soil_temp(i) = 0.0
      enddo
 
!calculate dt and the number it iterations
      g%dt = e%timestep/48.0    !seconds  dt is real
      do time = nint(g%dt), nint(e%timestep), nint(g%dt)
         g%time = time

         if (e%timestep.lt.1440.0*60.0) then
            g%airt = 0.5 * ( e%maxt + e%mint )
         else
            g%airt = soiltemp_InterpTemp (
     :                g%time/3600.0 !convert to hours
     :              , e%maxt_time
     :              , e%mint
     :              , e%maxt
     :              , g%mint_yesterday
     :              , g%maxt_yesterday)
         endif

         g%tn(0) = g%airt
 
         call soiltemp_heat(g%heat_store)
 
         call soiltemp_therm(g%therm_cond)
 
         call soiltemp_thomas()
 
         call soiltemp_update()
 
      enddo

 
      call pop_routine (myname)
      return
      end



* ====================================================================
      Recursive
     :subroutine soiltemp_heat (l_heat_store)
* ====================================================================
      use SoilTempModule
      Use infrastructure
      implicit none

*+  Purpose
*     Calculate the storage of heat energy in the soil layer following
*     to Campbell, G.S. (1985) "Soil physics with BASIC: Transport
*     models for soil-plant systems" (Amsterdam, Elsevier)

*+  Mission statement
*     Calculate the storage of heat energy in the soil

*+  Changes
*     27-05-1995 - vals - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'soiltemp_heat')

*+  Local Variables
      real l_heat_store(max_node)
      integer i
      real porosity

*- Implementation Section ----------------------------------
      call push_routine (myname)
         do i=1,g%nz
            porosity = e%rhob(i) / 2.65
            l_heat_store(i)=(   c%vol_spec_heat_clay*(1-porosity)  +
     :                     c%vol_spec_heat_water*e%sw(i)           )
         enddo
 
!the Campbell version
!            l_heat_store(i)=(   c%vol_spec_heat_clay*(1-porosity)  +
!     :                     c%vol_spec_heat_water*e%sw(i)           )
!     :      *(g%z(i+1)-g%z(i-1))/(2*real(g%dt))
 
      call pop_routine (myname)
      return
      end



* ====================================================================
      Recursive
     :subroutine soiltemp_therm (l_therm_cond)
* ====================================================================
      use SoilTempModule
      Use infrastructure
      implicit none

*+  Purpose
*     Calculate the thermal conductivity of the soil layer following
*     to Campbell, G.S. (1985) "Soil physics with BASIC: Transport
*     models for soil-plant systems" (Amsterdam, Elsevier)

*+  Mission statement
*     Calculate the thermal conductivity of the soil

*+  Changes
*     27-05-1995 - vals - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'soiltemp_therm')

*+  Local Variables
      real l_therm_cond(0:max_node)
      real temp
      integer i
      real d1
      real d2
      real d
      real d_sum

*- Implementation Section ----------------------------------
      call push_routine (myname)
!no change needed from Campbell to my version
         do i=1,g%nz
            temp = (g%c3(i) * e%sw(i)) **4.0
            temp = temp * (-1)
            l_therm_cond(i)=
     :                    (g%c1(i) + g%c2(i)*e%sw(i) - (g%c1(i)-g%c4(i))
     :                    *exp(temp))
         enddo
            ! now get weighted average between the nodes.
         do i=2,g%nz-1
            d = sum(e%dlayer(1:i-1))
            d1 = d - g%z(i)*1000.0
            d2 = g%z(i+1)*1000.0 - d
            d_sum = d1 + d2
            l_therm_cond(i)= l_therm_cond(i)* d1/(d_sum)
     :                     + l_therm_cond(i+1)* d2/(d_sum)
         enddo
 
      call pop_routine (myname)
      return
      end



* ====================================================================
      Recursive
     :subroutine soiltemp_thomas ()
* ====================================================================
      use SoilTempModule
      Use infrastructure
      implicit none

*+  Purpose
*     Numerical solution of the differential equations. Solves the 
*     tri_diagonal matrix using the Thomas algorithm, Thomas, L.H. (1946)
*     "Elliptic problems in linear difference equations over a network"
*     Watson Sci Comput. Lab. Report., (Columbia University, New York)"

*+  Mission statement
*     Numerical solution of the differential equations.

*+  Changes
*     27-05-1995 - vals - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'soiltemp_thomas')

*+  Local Variables
      integer i
      real a(max_node)
      real b(max_node)
      real cc(max_node)
      real d(max_node)
      real heat(max_node)
      real therm(0:max_node)

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      therm(0) = g%therm_cond(0)
      do i = 1,g%nz
         heat(i) = g%heat_store(i) * 0.5 * (g%z(i+1) - g%z(i-1)) / g%dt   !rate of heat
         therm(i) = g%therm_cond(i) / (g%z(i+1)-g%z(i))     !convert to thermal conduc
      enddo
 
!My version
      a(1) = 0.0
      b(1) =   c%nu * therm(1)
     :       + c%nu * therm(0)
     :       + heat(1)
      cc(1) =  -c%nu * therm(1)
      d(1) =   g%t(0) * (1-c%nu) * therm(0)
     :       - g%t(1) * (1-c%nu) * therm(1)
     :       - g%t(1) * (1-c%nu) * therm(0)
     :       + g%t(1) * heat(1)
     :       + g%t(2) * (1-c%nu) * therm(1)
     :       + therm(0)* g%tn(0) * c%nu
      if ((e%eos - e%es) .gt. 0.2) then
         d(1) = d(1) - (e%eos - e%es) * lambda / e%timestep
      endif
!last line is unfullfilled soil water evaporation
!the main loop
      do i=2,g%nz-1
      a(i) =  -c%nu * therm(i-1)
      b(i) =   c%nu * therm(i)
     :       + c%nu * therm(i-1)
     :       + heat(i)
      cc(i) =  -c%nu * therm(i)
      d(i) =   g%t(i-1) * (1-c%nu) * therm(i-1)
     :       - g%t(i) * (1-c%nu) * therm(i)
     :       - g%t(i) * (1-c%nu) * therm(i-1)
     :       + g%t(i) * heat(i)
     :       + g%t(i+1) * (1-c%nu) * therm(i)
      enddo
!lower node
      a(g%nz) = -c%nu * therm(g%nz-1)
      b(g%nz) =  c%nu * therm(g%nz)
     :       + c%nu * therm(g%nz-1)
     :       + heat(g%nz)
      cc(g%nz) = 0.0
      d(g%nz) =  g%t(g%nz-1) * (1-c%nu) * therm(g%nz-1)
     :       - g%t(g%nz) * (1-c%nu) * therm(g%nz)
     :       - g%t(g%nz) * (1-c%nu) * therm(g%nz-1)
     :       + g%t(g%nz) * heat(g%nz)
     :       + g%t(g%nz+1) * (1-c%nu) * therm(g%nz)
     :       + therm(g%nz) * c%nu * g%tn(g%nz+1)
 
! the Thomas algorithm
         do i=1,g%nz-1
            cc(i)=cc(i)/b(i)
            d(i)=d(i)/b(i)
            b(i+1)=b(i+1)-a(i+1)*cc(i)
            d(i+1)=d(i+1)-a(i+1)*d(i)
         enddo
         g%tn(g%nz)=d(g%nz)/b(g%nz)
         do i = g%nz-1,1,-1
            g%tn(i)=d(i)-cc(i)*g%tn(i+1)
         enddo
 
 
      call pop_routine (myname)
      return
      end



* ====================================================================
      Recursive
     :subroutine soiltemp_update ()
* ====================================================================
      use SoilTempModule
      Use infrastructure
      implicit none

*+  Purpose
*     Determine min, max, and average soil temperature from the
*     half-hourly iterations.

*+  Mission statement
*     Determine min, max, and average soil temperature from the
*     half-hourly iterations

*+  Changes
*     27-05-1995 - vals - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'soiltemp_update')

*+  Local Variables
      integer i

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      do i=0,g%nz+1
         g%t(i) = g%tn(i)
      enddo
 
!set the min & max to soil temperature if this is the first iteration
      if (int(g%time) .lt. g%dt*1.2) then
         do i=1,g%nz
            g%mint_soil(i) = g%t(i)
            g%maxt_soil(i) = g%t(i)
         enddo
      endif
 
      do i=1,g%nz
         if (g%t(i) .lt. g%mint_soil(i)) g%mint_soil(i) = g%t(i)
         if (g%t(i) .gt. g%maxt_soil(i)) g%maxt_soil(i) = g%t(i)
         g%soil_temp(i) = g%soil_temp(i) + g%t(i)/48.0
      enddo
 
      call pop_routine (myname)
      return
      end



* ====================================================================
      Recursive
     :subroutine soiltemp_zero_all_globals ()
* ====================================================================
      use SoilTempModule
      Use infrastructure
      implicit none

*+  Purpose
*     Set all variables in this module to zero.

*+  Mission Statement
*     Zero all the global variables and arrays

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'soiltemp_zero_all_globals')

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
         g%time = 0
         g%nz          = 0
         g%num_layer   = 0
         g%dt_max      = 0.0
         g%dt          = 0.0
         g%C1(:)       = 0.0       
         g%C2(:)       = 0.0       
         g%C3(:)       = 0.0       
         g%C4(:)       = 0.0       
         g%heat_store(:)  = 0.0    
         g%t(:)           = 0.0  
         g%therm_cond(:)  = 0.0  
         g%tn(:)          = 0.0  
         g%z(:)           = 0.0  
         g%airt           = 0.0
         g%maxt_yesterday = 0.0
         g%mint_yesterday = 0.0
         g%soil_temp(:)   = 0.0     
         g%mint_soil(:)   = 0.0     
         g%maxt_soil(:)   = 0.0   

         e%t_ave       = 0.0              
         e%timestep    = 0.0
         e%dlayer(:)   = 0.0
         e%sw(:)       = 0.0      
         e%rhob(:)     = 0.0
         e%maxt_time   = 0.0              
         e%mint        = 0.0
         e%maxt        = 0.0
         e%eos         = 0.0
         e%es          = 0.0

         p%clay(:)              = 0.0

         c%nu                   = 0.0
         c%vol_spec_heat_clay   = 0.0  
         c%vol_spec_heat_om     = 0.0  
         c%vol_spec_heat_water  = 0.0  


      call pop_routine (myname)
 
      return
      end


* ====================================================================
      Recursive
     :subroutine soiltemp_zero_variables ()
* ====================================================================
      use SoilTempModule
      Use infrastructure
      implicit none

*+  Purpose
*     Set all variables in this module to zero.

*+  Mission statement
*     Initialise variables to zero

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'soiltemp_zero_variables')

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
!      call fill_real_array(???,?value?,?size?)
 
      call pop_routine (myname)
 
      return
      end



*     ================================================================
      Recursive
     :subroutine soiltemp_prepare ()
*     ================================================================
      use SoilTempModule
      Use infrastructure
      implicit none

*+  Purpose
*     perform calculations before the current timestep.

*+  Mission Statement
*     Perform preparatory calculations for the next timestep

*+  Changes
*      1/6/94  vos programmed

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'soiltemp_prepare')

*- Implementation Section ----------------------------------
 
      call push_routine (myname)
 
      call soiltemp_zero_daily_variables ()
 
      call pop_routine (myname)
      return
      end



* ====================================================================
      Recursive
     :subroutine soiltemp_get_other_variables ()
* ====================================================================
      use SoilTempModule
      Use infrastructure
      implicit none

*+  Purpose
*      Get the values of variables from other modules

*+  Mission Statement
*     Gets the values of variables/arrays from other modules

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'soiltemp_get_other_variables')

*+  Local Variables
       integer numvals              ! number of values returned
       integer i

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
!t_ave = annual average temperture
      call get_real_var (
     :      unknown_module ! module that responds (not used)
     :          ,'tav'               ! keyword
     :          ,'(oC)'             ! units
     :          ,e%t_ave                 ! array
     :          ,numvals             ! number of values returned
     :          ,-30.0                  !lower
     :          ,40.0)                 !upper
 
 
!timestep
      call get_real_var (
     :      unknown_module ! module that responds (not used)
     :     ,'timestep'          ! variable name
     :     ,'(min)'        ! units                (not used)
     :     ,e%timestep            ! variable
     :     ,numvals         ! number of values returned
     :     ,0.0            ! lower limit for bound checking
     :     ,1440.0)           ! upper limit for bound checking
      e%timestep = e%timestep * 60.0 !to convert to seconds
!dlayer(i)
      call get_real_array (
     :      unknown_module ! module that responds (not used)
     :     ,'dlayer'          ! variable name
     :     ,max_layer            ! array size
     :     ,'(mm)'        ! units                (not used)
     :     ,e%dlayer            ! variable
     :     ,numvals         ! number of values returned
     :     ,0.0            ! lower limit for bound checking
     :     ,1000.0)           ! upper limit for bound checking
      g%num_layer = numvals
      g%nz = g%num_layer + 1

         ! mapping of layers to nodes -
         ! layer  - air  surface    1    2  ... num_layer num_layer+1
         ! node   -  0      1       2    3         nz       nz+1
      g%z(0) = 0.0
      g%z(1) = 0.0
         g%z(2) = 0.5*e%dlayer(1) /1000.0
      do i=2,g%nz
         g%z(i+1) = (sum(e%dlayer(1:i-1)) + 0.5 * e%dlayer(i) ) /1000.0         
      enddo
      g%z(g%nz+1) = g%z(g%nz) + 10.0 !add 2 meters - should always be enough to assume c
      e%dlayer(g%num_layer+1) = 10.0 - 0.5*e%dlayer(g%num_layer)
!sw(i)
      call get_real_array (
     :      unknown_module ! module that responds (not used)
     :     ,'sw'          ! variable name
     :     ,max_layer            ! array size
     :     ,'(m3/m3)'        ! units                (not used)
     :     ,e%sw            ! variable
     :     ,numvals         ! number of values returned
     :     ,0.0            ! lower limit for bound checking
     :     ,1.0)           ! upper limit for bound checking
      if (numvals.ne.g%num_layer) call fatal_error(ERR_USER,
     :'All soil variables must have the same number of layers')
      e%sw(g%nz) = e%sw(numvals)
       !cjh do i=1,g%nz
       !cjh    e%sw(i) = 0.25
       !cjh enddo
 
!rhob(i)
      call get_real_array (
     :      unknown_module ! module that responds (not used)
     :     ,'bd'          ! variable name
     :     ,max_layer            ! array size
     :     ,'(Mg/m3)'        ! units                (not used)
     :     ,e%rhob            ! variable
     :     ,numvals         ! number of values returned
     :     ,0.0            ! lower limit for bound checking
     :     ,2.65)           ! upper limit for bound checking
      if (numvals.ne.g%num_layer) call fatal_error(ERR_USER,
     :'All soil variables must have the same number of layers')
      e%rhob(g%nz) = e%rhob(numvals)
 
 
!maxt_time time at which get maximum temperature (min)
      call get_real_var_optional (
     :           unknown_module         ! section header
     :          ,'maxt_time'               ! keyword
     :          ,'(hours)'             ! units
     :          ,e%maxt_time                 ! array
     :          ,numvals             ! number of values returned
     :          ,0.0                  !lower
     :          ,24.0)                 !upper
 
!mint
      call get_real_var (
     :      unknown_module ! module that responds (not used)
     :     ,'mint'          ! variable name
     :     ,'(oC)'        ! units                (not used)
     :     ,e%mint            ! variable
     :     ,numvals         ! number of values returned
     :     ,-100.0            ! lower limit for bound checking
     :     ,100.0)           ! upper limit for bound checking
!maxt
      call get_real_var (
     :      unknown_module ! module that responds (not used)
     :     ,'maxt'          ! variable name
     :     ,'(oC)'        ! units                (not used)
     :     ,e%maxt            ! variable
     :     ,numvals         ! number of values returned
     :     ,-100.0            ! lower limit for bound checking
     :     ,100.0)           ! upper limit for bound checking
 
!eos
      call get_real_var_optional (
     :      unknown_module ! module that responds (not used)
     :     ,'eos'          ! variable name
     :     ,'(mm)'        ! units                (not used)
     :     ,e%eos            ! variable
     :     ,numvals         ! number of values returned
     :     ,0.0            ! lower limit for bound checking
     :     ,100.0)           ! upper limit for bound checking
      if (numvals .eq. 0) e%eos =0.0
 
!es
      call get_real_var_optional (
     :      unknown_module ! module that responds (not used)
     :     ,'es'          ! variable name
     :     ,'(mm)'        ! units                (not used)
     :     ,e%es            ! variable
     :     ,numvals         ! number of values returned
     :     ,0.0            ! lower limit for bound checking
     :     ,100.0)           ! upper limit for bound checking
      if (numvals .eq. 0) e%es = 0.0
 
      call pop_routine (myname)
      return
      end



* ====================================================================
      Recursive
     :subroutine soiltemp_Send_my_variable (Variable_name)
* ====================================================================
      use SoilTempModule
      Use infrastructure
      implicit none
 

*+  Sub-Program Arguments
       character Variable_name*(*)     ! (INPUT) Variable name to search for

*+  Purpose
*      Return the value of one of our variables to caller

*+  Mission Statement
*     Return the value of a variable requested by other modules

*+  Changes
*    ????
*    070896 jngh added message_unused call at end
*                changed all literals of first argument in Respond2Get
*                to variable_name

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'soiltemp_send_my_variable')

*+  Local Variables
      real temp_array(max_layer)
      integer i

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
!final_soil_temp(0:max_layer)
      if (variable_name .eq. 'final_soil_temp') then
         do i=1,g%num_layer
            temp_array(i) = g%t(i+1)
         enddo
         call respond2get_real_array (
     :               variable_name            ! variable name
     :              ,'(oC)'           ! variable units
     :              ,temp_array              ! variable
     :              ,g%num_layer)             ! array size

      elseif (variable_name .eq. 'final_soil_temp_surface') then
         call respond2get_real_var (
     :               variable_name            ! variable name
     :              ,'(oC)'           ! variable units
     :              ,g%t(1)              ! variable
     :              )             
!soil_temp
      elseif (variable_name .eq. 'ave_soil_temp') then
         do i=1,g%num_layer
            temp_array(i) =g%soil_temp(i+1)
         enddo
         call respond2get_real_array (
     :               variable_name            ! variable name
     :              ,'(oC)'           ! variable units
     :              ,temp_array              ! variable
     :              ,g%num_layer)             ! array size

      elseif (variable_name .eq. 'ave_soil_temp_surface') then
         call respond2get_real_var (
     :               variable_name            ! variable name
     :              ,'(oC)'           ! variable units
     :              ,g%soil_temp(1)              ! variable
     :              )            
!mint_soil
      elseif (variable_name .eq. 'mint_soil') then
         do i=1,g%num_layer
            temp_array(i) = g%mint_soil(i+1)
         enddo
         call respond2get_real_array (
     :               variable_name            ! variable name
     :              ,'(oC)'           ! variable units
     :              ,temp_array              ! variable
     :              ,g%num_layer)             ! array size

      elseif (variable_name .eq. 'mint_soil_surface') then
         call respond2get_real_var (
     :               variable_name            ! variable name
     :              ,'(oC)'           ! variable units
     :              ,g%mint_soil(1)              ! variable
     :              )
!maxt_soil
      elseif (variable_name .eq. 'maxt_soil') then
         do i=1,g%num_layer
            temp_array(i) = g%maxt_soil(i+1)
         enddo
         call respond2get_real_array (
     :               variable_name            ! variable name
     :              ,'(oC)'           ! variable units
     :              ,temp_array              ! variable
     :              ,g%num_layer)             ! array size

      elseif (variable_name .eq. 'maxt_soil_surface') then
         call respond2get_real_var (
     :               variable_name            ! variable name
     :              ,'(oC)'           ! variable units
     :              ,g%maxt_soil(1)              ! variable
     :              )
!therm_cond(0:max_layer)
      elseif (variable_name .eq. 'therm_cond') then
         do i=1,g%nz
            temp_array(i) = g%therm_cond(i)
         enddo
         call respond2get_real_array (
     :               variable_name            ! variable name
     :              ,'(J/sec/m/K)'           ! variable units
     :              ,temp_array              ! variable
     :              ,g%nz)             ! array size
!heat_store(max_layer)
      elseif (variable_name .eq. 'heat_store') then
         do i=1,g%nz
            temp_array(i) = g%heat_store(i)
         enddo
         call respond2get_real_array (
     :               variable_name            ! variable name
     :              ,'(J/m3/K/s)'           ! variable units
     :              ,temp_array              ! variable
     :              ,g%nz)             ! array size

      else
         call Message_unused ()
 
      endif
 
      call pop_routine (myname)
      return
      end



*     ===========================================================
      subroutine soiltemp_read_param ()
*     ===========================================================
      use SoilTempModule
      Use infrastructure
      implicit none

*+  Purpose
*       Read all module parameters.

*+  Mission statement
*       Read all module parameters

*+  Changes
*     <insert here>

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'soiltemp_read_param')
*
      character section_name*(*)
      parameter (section_name = 'parameters')

*+  Local Variables
      integer    numvals               ! number of values read
      real temp
      real temp_array(max_layer)
      integer i

*- Implementation Section ----------------------------------
 
      call push_routine (myname)
 
!clay(i) = 0.12
      call read_real_array (
     :           section_name         ! section header
     :          ,'clay'               ! keyword
     :          ,max_layer                 ! array size
     :          ,'(-)'             ! units
     :          ,p%clay                 ! array
     :          ,numvals             ! number of values returned
     :          ,1e-6                  !lower
     :          ,1.0)                 !upper
      g%nz = numvals + 1
      p%clay(g%nz) = p%clay(numvals)

      do i = 1,g%nz
         g%c1(i) = 0.65 - 0.78*e%rhob(i) + 0.6 * e%rhob(i)**2   !A  approximation to e
         g%c2(i) = 1.06 * e%rhob(i) * e%sw(i)           !B   for mineral soil - assume
         g%c3(i) = 1.0 + 2.6* 1/(sqrt(p%clay(i)))  !c  is the water content where co
         g%c4(i) = 0.03 + 0.1 * e%rhob(i)**2  !D  assume mineral soil particle d
      enddo
 
 
!t(i) = initial temperature - if not there then set to average temperature
      call read_real_array_optional (
     :           section_name         ! section header
     :          ,'soil_temp'               ! keyword
     :          ,max_layer                 ! array size
     :          ,'(oC)'             ! units
     :          ,temp_array                 ! array
     :          ,numvals             ! number of values returned
     :          ,-30.0                  !lower
     :          ,40.0)                 !upper
      if (numvals.eq.0) then
         do i=0,g%nz+1
            g%t(i) = e%t_ave
            g%tn(i) = e%t_ave
         enddo
      elseif (numvals.ne.g%nz) then
         call fatal_error(ERR_USER,
     :'soil_temp has the wrong number of elements')
      else
         do i=1,g%nz
            g%t(i) = temp_array(i)
            g%tn(i) = temp_array(i)
         enddo
         g%t(g%nz+1) = e%t_ave
         g%tn(g%nz+1) = e%t_ave
         g%t(0) = e%t_ave
         g%tn(0) = e%t_ave
       endif
 
!therm_cond(0) = 20.0 ! boundary layer condictance W m-2 K-1
      call read_real_var (
     :           section_name         ! section header
     :          ,'bound_layer_cond'               ! keyword
     :          ,'(W/m2/K)'             ! units
     :          ,temp                 ! array
     :          ,numvals             ! number of values returned
     :          ,0.0                  !lower
     :          ,100.0)                 !upper
      g%therm_cond(0) = temp
 
 
      call pop_routine  (myname)
      return
      end



*     ===========================================================
      subroutine soiltemp_read_constants ()
*     ===========================================================
      use SoilTempModule
      Use infrastructure
      implicit none

*+  Purpose
*       Read all module constants.

*+  Mission statement
*       Get the constants for soiltemp

*+  Changes
*

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'soiltemp_read_constants')
*
      character section_name*(*)
      parameter (section_name = 'constants')

*+  Local Variables
      integer    numvals               ! number of values read

*- Implementation Section ----------------------------------
 
      call push_routine (myname)
  
!nu = 0.6 - forward or backward difference
      call read_real_var (
     :           section_name         ! section header
     :          ,'nu'               ! keyword
     :          ,'(-)'             ! units
     :          ,c%nu                 ! array
     :          ,numvals             ! number of values returned
     :          ,0.0                  !lower
     :          ,1.0)                 !upper
!vol_spec_heat_om = 5.00e6 !(Joules*m-3*K-1)
      call read_real_var (
     :           section_name         ! section header
     :          ,'vol_spec_heat_om'               ! keyword
     :          ,'(J/m3/K)'             ! units
     :          ,c%vol_spec_heat_om                 ! array
     :          ,numvals             ! number of values returned
     :          ,1e6                  !lower
     :          ,1e7)                 !upper
!vol_spec_heat_water = 4.18e6 !(Joules*m-3*K-1)
      call read_real_var (
     :           section_name         ! section header
     :          ,'vol_spec_heat_water'               ! keyword
     :          ,'(J/m3/K)'             ! units
     :          ,c%vol_spec_heat_water                 ! array
     :          ,numvals             ! number of values returned
     :          ,1e6                  !lower
     :          ,1e7)                 !upper
!vol_spec_heat_clay = 2.39e6 !(Joules*m-3*K-1)
      call read_real_var (
     :           section_name         ! section header
     :          ,'vol_spec_heat_clay'               ! keyword
     :          ,'(J/m3/K)'             ! units
     :          ,c%vol_spec_heat_clay                 ! array
     :          ,numvals             ! number of values returned
     :          ,1e6                  !lower
     :          ,1e7)                 !upper
 
      call pop_routine (myname)
      return
      end



* ====================================================================
      Recursive
     :subroutine soiltemp_zero_daily_variables ()
* ====================================================================
      use SoilTempModule
      Use infrastructure
      implicit none

*+  Purpose
*     Set all variables in this module to zero.

*+  Mission Statement
*     Set soiltemp daily variables & arrays to zero

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'soiltemp_zero_variables')

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
!      call fill_real_array(???,?value?,?size?)
      call pop_routine (myname)
 
      return
      end
*====================================================================
      Recursive
     :real function soiltemp_InterpTemp (
     :                time
     :              , maxt_time
     :              , mint
     :              , maxt
     :              , mint_yesterday
     :              , maxt_yesterday)
*====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real time           !time of day in hours
      REAL maxt_time      !time of day for minimum temperature, hours
      REAL mint           !minimum temperature, C
      REAL maxt           !maximum temperature, C
      REAL mint_yesterday !minimum temperature yesterady, C
      REAL maxt_yesterday !maximum temperature yesterady, C

*+  Purpose
*    Interpolate air temperature

*+  Notes
*    Between midinight and mint_time just a linear interpolation between
*    yesterday's midnight temperature and today's mint.  For the rest of
*    the day use a sin function

*+  Changes
*       290799 - VOS specified and programmed

*+  Calls

*+  Local Variables
      REAL p_time               !time as proportion of the day
      REAL p_maxt_time          !mint_time as proportion of the day
      REAL p_mint_time          !mint_time as proportion of the day
      REAL t_midnight           !temperature last midnight
      REAL t_scale              !0 at midnight, 1 at mint_time

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'soiltemp_InterpTemp')

      real pi
      parameter (pi = 3.14159)

*- Implementation Section ----------------------------------
 
      call push_routine (myname)

      p_time = time / 24.0
      p_maxt_time = maxt_time / 24.0
      p_mint_time = p_maxt_time - 0.5

      if (p_time .lt. p_mint_time) then
         t_midnight = SIN((0.0 + 0.25 - p_maxt_time)*2.0*pi)
     :              * (maxt_yesterday - mint_yesterday) / 2.0
     :              + (maxt_yesterday + mint_yesterday) / 2.0
         t_scale = (p_mint_time - p_time) / p_mint_time
         t_scale = bound(t_scale, 0.0, 1.0)
         soiltemp_InterpTemp = mint + t_scale*(t_midnight-mint)
      else
         soiltemp_InterpTemp = SIN((p_time + 0.25 - p_maxt_time)*2.0*pi)
     :                       * (maxt - mint) / 2.0
     :                       + (maxt + mint) / 2.0
      end if

      call pop_routine (myname)

      return
      end

* ====================================================================
      Recursive
     :subroutine soiltemp_get_ini_variables ()
* ====================================================================
      use SoilTempModule
      Use infrastructure
      implicit none

*+  Purpose
*      Get the values of variables from other modules

*+  Mission statement
*      Get all the values from other modules

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'soiltemp_get_ini_variables')

*+  Local Variables
       integer numvals              ! number of values returned
       integer i

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
!t_ave = annual average temperture
      call get_real_var (
     :      unknown_module ! module that responds (not used)
     :          ,'tav'               ! keyword
     :          ,'(oC)'             ! units
     :          ,e%t_ave                 ! array
     :          ,numvals             ! number of values returned
     :          ,-30.0                  !lower
     :          ,40.0)                 !upper
 
 
!timestep
      call get_real_var (
     :      unknown_module ! module that responds (not used)
     :     ,'timestep'          ! variable name
     :     ,'(min)'        ! units                (not used)
     :     ,e%timestep            ! variable
     :     ,numvals         ! number of values returned
     :     ,0.0            ! lower limit for bound checking
     :     ,1440.0)           ! upper limit for bound checking
      e%timestep = e%timestep * 60.0 !to convert to seconds

!dlayer(i)
      call get_real_array (
     :      unknown_module ! module that responds (not used)
     :     ,'dlayer'          ! variable name
     :     ,max_layer            ! array size
     :     ,'(mm)'        ! units                (not used)
     :     ,e%dlayer            ! variable
     :     ,numvals         ! number of values returned
     :     ,0.0            ! lower limit for bound checking
     :     ,1000.0)           ! upper limit for bound checking
      g%num_layer = numvals
      g%nz = g%num_layer + 1

         ! mapping of layers to nodes -
         ! layer  - air  surface    1    2  ... num_layer num_layer+1
         ! node   -  0      1       2    3         nz       nz+1
      g%z(0) = 0.0
      g%z(1) = 0.0
         g%z(2) = 0.5*e%dlayer(1) /1000.0
      do i=2,g%nz
         g%z(i+1) = (sum(e%dlayer(1:i-1)) + 0.5 * e%dlayer(i) ) /1000.0         
      enddo
      g%z(g%nz+1) = g%z(g%nz) + 10.0 !add 2 meters - should always be enough to assume c
      e%dlayer(g%num_layer+1) = 10.0 - 0.5*e%dlayer(g%num_layer)
!rhob(i)
      call get_real_array (
     :      unknown_module ! module that responds (not used)
     :     ,'bd'          ! variable name
     :     ,max_layer            ! array size
     :     ,'(Mg/m3)'        ! units                (not used)
     :     ,e%rhob            ! variable
     :     ,numvals         ! number of values returned
     :     ,0.0            ! lower limit for bound checking
     :     ,2.65)           ! upper limit for bound checking
      if (numvals.ne.g%num_layer) call fatal_error(ERR_USER,
     :'All soil variables must have the same number of layers')
 
      e%rhob(g%nz) = e%rhob(numvals)
      call pop_routine (myname)
      return
      end



