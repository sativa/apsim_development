module WaterSupplyModule
   use ComponentInterfaceModule
   use Registrations
    
!  ====================================================================
!  WaterSupply constants
!  ====================================================================

!  Short Description:
!  Constant values

!  Notes:

!  attributes:


!  ----------------------- Declaration section ------------------------

!  Global variables

   integer    max_solutes       ! Maximum number of solutes in the soil
   parameter (max_solutes = 20)

   integer    max_coeffs       ! Maximum number of coefficients in a table
   parameter (max_coeffs = 10)

   integer    module_name_size      ! maximum length of module name
   parameter (module_name_size = 30)

   integer    max_sources      ! maximum number of water sources allowable
   parameter (max_sources = 10)

   type WaterSupplyGlobals
      sequence
      real    rain                                 ! precipitation (mm/d)
      real    radn                                 ! solar radiation (mj/m^2/day)
      real    mint                                 ! minimum air temperature (oC)
      real    maxt                                 ! maximum air temperature (oC)
      integer year                                 ! year
      integer day                                  ! day of year
      integer source_counter                       ! counter which keeps track of the preferential source number we are up to
      integer tot_num_sources                      ! total number of water sources specified in a 'top_up' or 'apply' method call
      integer num_solutes                          ! total number of solutes present in the simulation
      integer full                                 ! flag (0 or 1) indicating whether storage is full
      integer full_yesterday                       ! flag (0 or 1) indicating whether storage was full yesterday
      integer filling_event                        ! flag (0 or 1) indicating whether today has been a filling event for the storage
      real    solute_conc(max_solutes)             ! solute concentration in watersupply for each of the system solutes (ppm)
      real    rain_capture                         ! rain captured directly by storage (Ml)
      real    total_runoff                         ! catchment and crop runoff captured by storage (Ml)
      real    evaporation                          ! daily evaporation loss from the surface of storage (Ml)
      real    seepage                              ! daily seepage loss through bottom of storage (Ml)
      real    available_water                      ! available water at any time (Ml)
      real    available_depth                      ! available depth of water at any time (m)
      real    overflow                             ! storage overflow above maximum capacity
      real    irrig_water_supplied                 ! water provided (ML) on this day in response to a 'gimme_water' event from irrigate
      character  top_up_source(max_sources)*(module_name_size)  ! String containing the #1 preference top-up water source
      character  solute_names(max_solutes)*32      ! array of system solute names
      character  solute_owners(max_solutes)*32     ! array of the 'owners' of each of the above solutes

   end type WaterSupplyGlobals
   !  ====================================================================
   type WaterSupplyParameters
      sequence
      character   source_type*9                     ! storage type (eg dam_gully, dam_ring, dam_exc, sump, river or bore)
      character   receive_catchment_runoff*3        ! string (yes/no) indicating whether catchment runoff flows into this storage
      character   receive_crop_runoff*3             ! string (yes/no) indicating whether crop runoff flows into this storage
      character   receive_rainfall*3                ! string (yes/no) indicating whether rainfall is received into this storage
      real        catchment_runoff_factor           ! water-shedding factor of catchment cf cropping area ()
      real        catchment_area                    ! catchment area (ha)
      real        runoff_solute_conc(max_solutes)   ! solute concentration in runoff for each of the system solutes (ppm)
      real        rainfall_solute_conc(max_solutes) ! solute concentration in rainfall for each of the system solutes (ppm)
      real        max_available_water               ! storage capacity or maximum allocation (Ml)
      real        max_area                          ! surface area of storage at capacity (ha)
      real        permeability                      ! permeability of sealing layer (m/day)
      real        seal_thickness                    ! thickness of low permeability seal (m)
      real        min_volume                        ! water volume available below which pumping is not possible (Ml)
      real        max_pump                          ! maximum daily delivery from the pump (Ml)
      real        annual_allocation                 ! for bore and river
      integer     renewal_day                       ! day upon which allocation is renewed
   end type WaterSupplyParameters
   !  ====================================================================
   type WaterSupplyConstants
      sequence

      real b               ! geometry factor for particular storage_design

   end type WaterSupplyConstants
   !  ====================================================================

   ! instance variables.
   common /InstancePointers/ ID,g,p,c
   save InstancePointers
   type (WaterSupplyGlobals),pointer :: g
   type (WaterSupplyParameters),pointer :: p
   type (WaterSupplyConstants),pointer :: c
   type (IDsType), pointer :: ID


   contains

!  ===========================================================
subroutine WaterSupply_read_parameters ()
!  ===========================================================

   Use Infrastructure
   implicit none

!+ Purpose
!  input initial values from parameter file.

!+ Mission Statement
!  Read Parameters

!+ Changes
!  dsg 150603  built

!+ Constant Values
   character  my_name*(*)            ! name of this module
   parameter (my_name = 'WaterSupply_read_parameters')
!
    character  section_name*(*)
    parameter (section_name = 'parameters')

!+ Local Variables
   integer    i                     ! simple counter
   integer    numvals               ! number of values returned
   character  source_type*100       ! local variable for source type
   character  dummy*100             ! first half of solute concatenation
   character  default_name*100      ! concatenated parameter name for initial solute concentration

!- Implementation Section ----------------------------------

   call push_routine (my_name)

   call write_string (new_line//'   - Reading WaterSupply Parameters')

   !********** read source type ****************************************************
   call read_char_var (section_name,'source_type', '()', source_type, numvals)

   if (source_type.eq.'dam_gully'.or. &
       source_type.eq.'dam_ring'.or. &
       source_type.eq.'dam_exc'.or. &
       source_type.eq.'sump'.or. &
       source_type.eq.'river'.or. &
       source_type.eq.'bore') then

       p%source_type = source_type

   else
        call fatal_error (ERR_USER,'Water Source type not recognised')
   endif

   !********* get parameter indicating whether this storage receives rainfall******
   ! this parameter specifically added for simulations in which a dam may or may not be active
   if (source_type.eq.'dam_gully'.or. &
       source_type.eq.'dam_ring'.or. &
       source_type.eq.'dam_exc'.or. &
       source_type.eq.'sump') then

       call read_char_var_optional (section_name,'receive_rainfall', '()', p%receive_rainfall, numvals)

       if(numvals.eq.0) then
          p%receive_rainfall = 'yes'
       endif 

   endif


   !********* get parameter indicating whether this storage receives catchment runoff******
   if (source_type.eq.'dam_gully'.or. &
       source_type.eq.'dam_exc'.or. &
       source_type.eq.'sump') then

       call read_char_var (section_name,'receive_catchment_runoff', '()', p%receive_catchment_runoff, numvals)

       if(p%receive_catchment_runoff.ne.'yes'.and.p%receive_catchment_runoff.ne.'no') then
         call fatal_error (ERR_USER,'receive_catchment_runoff parameter must be yes or no')
       endif

       if(p%receive_catchment_runoff.eq.'yes') then
          call read_real_var (section_name, 'catchment_area', '(ha)', p%catchment_area, numvals, 0.0, 10000.0)
          call read_real_var (section_name, 'catchment_runoff_factor', '()', p%catchment_runoff_factor, numvals, 0.0, 10000.0)
       endif 

   else  ! for bore, river, or dam_ring

       p%receive_catchment_runoff = 'no'
       
   endif

   !********* get parameter indicating whether this storage receives crop runoff******
   if (source_type.eq.'dam_gully'.or. &
       source_type.eq.'dam_exc'.or. &
       source_type.eq.'sump') then

       call read_char_var (section_name,'receive_crop_runoff', '()', p%receive_crop_runoff, numvals)

       if(p%receive_crop_runoff.ne.'yes'.and.p%receive_crop_runoff.ne.'no') then
         call fatal_error (ERR_USER,'receive_crop_runoff parameter must be yes or no')
      endif

   else  ! for bore, river, or dam_ring

       p%receive_crop_runoff = 'no'

   endif

   !********** get maximum available water (ML) (ie dam capacity or max bore allocation*********

   call read_real_var (section_name, 'max_available_water', '()', p%max_available_water, numvals, 0.0, 10000.0)

   !********* FOR DAMS AND SUMP ONLY : get 'maximum surface area' and 'maximum depth'**********
   !                                   permeability and seal_thickness

   if (source_type.eq.'dam_gully'.or. &
       source_type.eq.'dam_ring'.or. &
       source_type.eq.'dam_exc'.or. &
       source_type.eq.'sump') then

       call read_real_var (section_name, 'max_area', '(ha)', p%max_area, numvals, 0.0, 10000.0)

       call read_real_var (section_name, 'permeability', '(m/day)', p%permeability, numvals, 0.0, 10.0)

       call read_real_var (section_name, 'seal_thickness', '(m)', p%seal_thickness, numvals, 0.0, 100.0)

   else  ! we have a bore or a river
       p%max_area = 0.0
   endif


   !********** get initial available water (ML)************************************************

   call read_real_var (section_name, 'init_available_water', '()', g%available_water, numvals, 0.0, 10000.0)

   !********* get minimum usable volume  (ML)***************************************************

   call read_real_var (section_name, 'min_volume', '()', p%min_volume, numvals, 0.0, 100.0)

   !********* get maximum daily pumping volume (ML/day)*****************************************

   call read_real_var (section_name, 'max_pump', '()', p%max_pump, numvals, 0.0, 100.0)

   !********* FOR BORES AND RIVERS ONLY: get annual allocation (ML) & renewal day **************

   if (source_type.eq.'bore'.or.source_type.eq.'river') then
      call read_real_var (section_name, 'annual_allocation', '()', p%annual_allocation, numvals, 0.0, 10000.0)
      if ( numvals .eq. 0 ) then
         call fatal_error (ERR_USER,'Annual Allocation not provided')
      endif
      call read_integer_var (section_name, 'allocation_renewal_day', '()', p%renewal_day, numvals, 1, 365)
        if ( numvals .eq. 0 ) then
         call fatal_error (ERR_USER,'Allocation Renewal Day not provided')
        endif
   endif

   !********** check for any solute information    *********

   do i = 1,g%num_solutes
      dummy = string_concat('init_',g%solute_names(i))
      default_name = string_concat(dummy,'_conc')
      g%solute_conc(i) = 0.0
      call read_real_var_optional (section_name, default_name, '(ppm)', g%solute_conc(i), numvals, 0.0, 10000.0)
! also read in any information on runoff solute concentrations
      dummy = string_concat('runoff_',g%solute_names(i))
      default_name = string_concat(dummy,'_conc')
      p%runoff_solute_conc(i) = 0.0
      call read_real_var_optional (section_name, default_name, '(ppm)', p%runoff_solute_conc(i), numvals, 0.0, 10000.0)
! also read in any information on rainfall solute concentrations
      dummy = string_concat('rainfall_',g%solute_names(i))
      default_name = string_concat(dummy,'_conc')
      p%rainfall_solute_conc(i) = 0.0
      call read_real_var_optional (section_name, default_name, '(ppm)', p%rainfall_solute_conc(i), numvals, 0.0, 10000.0)
   end do

   !********************************************************************************************

   call pop_routine (my_name)
   return
end subroutine


!  ====================================================================
subroutine WaterSupply_read_constants ()
!  ====================================================================
   Use Infrastructure
   implicit none

!+ Purpose
!  Read in all coefficients from coefficient file.

!+ Mission Statement
!  Read Constants from Ini file

!+ Changes
!  dsg 150603  built

!+ Constant Values
   character  my_name*(*)          ! name of this procedure
   parameter (my_name = 'WaterSupply_read_constants')
   character  section_name*(*)
   parameter (section_name = 'constants')
   character  Storage_geometry*(*)           ! section name for dam geometry in
   parameter (Storage_geometry = 'geometry') ! lookup file

!+ Local Variables
   integer numvals                 ! number of values read from file
   character string*100            ! message string
   real cooper

!- Implementation Section ----------------------------------
   call push_routine (my_name)

   call write_string (new_line//'    - Reading WaterSupply constants')

   !********** get geometry factor, b *******
   if (p%source_type.eq.'dam_gully'.or. &
       p%source_type.eq.'dam_ring'.or. &
       p%source_type.eq.'dam_exc'.or. &
       p%source_type.eq.'sump') then

      call read_real_var (Storage_geometry, p%source_type, '()', c%b, numvals, 0.0, 10.0)               
      if (numvals.ne.1) then
         ! We have dodgy data
         string = 'Incorrect storage geometry data provided for '//p%source_type
         call FATAL_ERROR (ERR_user, string)
      endif

   else
      ! we are not dealing with a dam, we have a river or a bore
      c%b=0

   endif
   !*********************

   call pop_routine (my_name)
   return
end subroutine


!  ===========================================================
subroutine WaterSupply_ONprocess ()
!  ===========================================================
   Use Infrastructure
   implicit none

!+ Purpose
!  Daily calculations

!+ Mission Statement
!  Perform all APSIM Timestep calculations

!+ Changes
!  dsg 150603  built

!+ Calls

!+ Constant Values
   character  my_name*(*)           ! this subroutine name
   parameter (my_name = 'WaterSupply_process')

!+ Local Variables
    integer i

!- Implementation Section ----------------------------------

   call push_routine (my_name)

   call WaterSupply_rain_capture ()
   call WaterSupply_runoff ()
   call WaterSupply_evaporation_seepage ()
   call WaterSupply_check_allocation ()


   g%available_water = g%available_water &
                     + g%rain_capture &
                     + g%total_runoff &
                     - g%evaporation &
                     - g%seepage


   if (g%available_water.gt.p%max_available_water) then

       g%overflow = g%available_water - p%max_available_water
       g%available_water = p%max_available_water

   endif

   ! dsg 061204  These variables keep track of whether a storage is full (within the bounds of a realistic full_fraction = 0.95)
   !             and whether a 'filling_event' has ocurred today
   if (g%available_water.ge.p%max_available_water*0.95) then
       g%full = 1
       if (g%full_yesterday.eq.0) then
          g%filling_event = 1
       else
          g%filling_event = 0
       endif
       g%full_yesterday = 1
   else
       g%full = 0
       g%filling_event = 0
       g%full_yesterday = 0
   endif    
       

   call pop_routine (my_name)
   return
end subroutine



!  ===========================================================
subroutine WaterSupply_rain_capture ()
!  ===========================================================
   Use Infrastructure
   implicit none

!+ Purpose
!  Calculate rainfall input directly to the storage on a daily basis

!+ Mission Statement
!  Calculate rainfall capture

!+ Changes
!  dsg 150603  built

!+ Calls

!+ Constant Values
   character  my_name*(*)           ! this subroutine name
   parameter (my_name = 'WaterSupply_rain_capture')

!+ Local Variables
   integer    i                         ! simple counter
   real       new_solute_conc           ! solute concentration modified due to added rainwater ppm

!- Implementation Section ----------------------------------

   call push_routine (my_name)

   if (p%source_type.eq.'dam_gully'.or. &
       p%source_type.eq.'dam_ring'.or. &
       p%source_type.eq.'dam_exc'.or. &
       p%source_type.eq.'sump') then

      ! From Shaun - this seems suss
      ! g%rain_capture = p%max_area*((p%max_depth + (g%rain/1000.0))**c%b)
      ! :                  - p%max_available_water

      if(p%receive_rainfall.eq.'yes') then
        g%rain_capture = (p%max_area*g%rain)/100
      else
        g%rain_capture = 0.0  
      endif
   else

      g%rain_capture = 0.0    ! for a river or bore

   endif


   !  dsg 050803 Now modify solute concentrations
   if (g%rain_capture.gt.0.0) then
      do i=1,g%num_solutes
         if ((g%available_water + g%rain_capture).eq.0.0) then
            g%solute_conc(i) = 0.0
         else
            new_solute_conc =divide(((g%solute_conc(i)*g%available_water)+ (p%rainfall_solute_conc(i) * g%rain_capture)),(g%available_water + g%rain_capture),0.0)
            g%solute_conc(i) = new_solute_conc
            new_solute_conc = 0.0
         endif   
      end do
   endif
   
   call pop_routine (my_name)
   return
end subroutine



!  ===========================================================
subroutine WaterSupply_runoff ()
!  ===========================================================
   Use Infrastructure
   implicit none

!+ Purpose
!  To obtain and receive any catchment runoff if required

!+ Mission Statement
!  To obtain and receive any catchment runoff if required


!+ Changes
!  dsg 150603  built

!+ Calls

!+ Constant Values
   character  my_name*(*)           ! this subroutine name
   parameter (my_name = 'WaterSupply_runoff')

!+ Local Variables
   integer i                    !     simple counter
   integer  numvals             !     simple counter
   real  catchment_runoff       !     runoff from the catchment (non-crop) (ML)
   real  crop_runoff            !     runoff from the crop (ML)
   real  runoff                 !     runoff from the crop/fallow from SOILWAT2/ASPWIM
   real  crop_area              !     area simulated by apsim (ha)
   real  new_solute_conc        !     solute concentration modified due to added rainwater ppm

!- Implementation Section ----------------------------------

   call push_routine (my_name)

   crop_runoff = 0.0
   catchment_runoff = 0.0
   call get_real_var (unknown_module,'runoff','(mm)',runoff,numvals,0.0,1000.0)              

! Calculate runoff from the catchment
   if(p%receive_catchment_runoff.eq.'yes') then

      ! calculate runoff from the catchment area in ML
     catchment_runoff = runoff * p%catchment_runoff_factor * p%catchment_area /100

   endif

! Calculate runoff from the crop
   if(p%receive_crop_runoff.eq.'yes') then

      call get_real_var (unknown_module,'crop_area','(ha)',crop_area,numvals,0.0,1000.0)              
   
      ! calculate runoff from the cropping area in ML
      crop_runoff = runoff*crop_area/100

   endif 


      ! total the runoff from catchment and crop
      g%total_runoff = catchment_runoff + crop_runoff


   ! dsg 050803 Now modify solute concentrations
   do i=1,g%num_solutes
      if ((g%available_water + g%total_runoff).eq.0.0) then
          g%solute_conc(i) = 0.0
      else    
          new_solute_conc =divide(((g%solute_conc(i)*g%available_water)+ (p%runoff_solute_conc(i) * g%total_runoff)),(g%available_water + g%total_runoff),0.0)
          g%solute_conc(i) = new_solute_conc
          new_solute_conc = 0.0
      endif
   end do


   call pop_routine (my_name)
   return
end subroutine


!  ===========================================================
subroutine WaterSupply_evaporation_seepage ()
!  ===========================================================
   Use Infrastructure
   implicit none

!+ Purpose
!  To calculate daily evaporation from the storage if required

!+ Mission Statement
!  Calculate evaporation losses

!+ Changes
!  dsg 150603  built

!+ Calls

!+ Constant Values
   character  my_name*(*)           ! this subroutine name
   parameter (my_name = 'WaterSupply_evaporation')

!+ Local Variables

   integer i                    !     simple counter
   integer numvals              !     number of values returned from 'get'
   real   soil_evaporation
   real   dummy
   real  new_solute_conc        !     solute concentration modified due to added rainwater ppm

!- Implementation Section ----------------------------------

   call push_routine (my_name)

   if (p%source_type.eq.'dam_gully'.or. &
       p%source_type.eq.'dam_ring'.or. &
       p%source_type.eq.'dam_exc'.or. &
       p%source_type.eq.'sump') then

 ! if dam is inactive in simulation don't incorporate any losses
    if(p%receive_rainfall.eq.'yes') then

       ! calculate depth of water in storage (m)
       g%available_depth=(divide(g%available_water,p%max_area,0.0)**(divide(1.0,c%b,0.0)))

       ! From CERES maize soil evaporation (ref Shaun Lisson)
       soil_evaporation = g%radn*23.8846*(0.000204-(0.000183*0.1))*(29+(0.6*g%maxt+0.4*g%mint))
       dummy = (0.7*soil_evaporation/100.0)+(p%permeability*(divide(g%available_depth,p%seal_thickness,0.0))/365)

       if(g%available_depth.gt.dummy) then

           ! calculate evaporation
           g%evaporation = g%available_water-(p%max_area*((g%available_depth-(0.7*soil_evaporation/1000))**c%b))

           ! calculate seepage
           g%seepage = g%available_water-(p%max_area* &
            ((g%available_depth-(p%permeability*(divide(g%available_depth,p%seal_thickness,0.0))/365.0))**c%b))
       else
          ! evaporate all the water that is left, and assume nothing seeps
          g%evaporation = g%available_depth
          g%seepage = 0.0
       endif

    else
    endif
    
   else  ! we have a bore or a river

       g%evaporation = 0.0
       g%seepage = 0.0

   endif

   ! dsg 050803 Now modify solute concentrations due to evaporation, not seepage
   do i=1,g%num_solutes
     if((g%available_water-g%evaporation).eq.0.0) then
        g%solute_conc(i) = 0.0
     else   
        new_solute_conc =divide((g%solute_conc(i)*g%available_water),(g%available_water - g%evaporation),0.0)
        g%solute_conc(i) = new_solute_conc
        new_solute_conc = 0.0
     endif
   end do


   call pop_routine (my_name)
   return
end subroutine


!  ===========================================================
subroutine WaterSupply_check_allocation ()
!  ===========================================================
   Use Infrastructure
   implicit none

!+ Purpose
!  To check if the annual allocation is due today and to implement if required

!+ Mission Statement
!  To check if the annual allocation is due today and to implement if required

!+ Changes
!  dsg 150603  built

!+ Calls

!+ Constant Values
   character  my_name*(*)           ! this subroutine name
   parameter (my_name = 'WaterSupply_check_allocation')

!+ Local Variables
   integer   day                  ! day of year
   character msg_string*200       ! message string to summary file
   character source_name*(Max_module_name_size) ! Module (instance) name of this module
   integer numvals                ! simple counter

!- Implementation Section ----------------------------------

   call push_routine (my_name)

   if (p%source_type.eq.'bore'.or. &
       p%source_type.eq.'river') then

      call get_integer_var        (unknown_module,'day','',day,numvals,0,366)           

      ! check if today is the allocation renewal day
      if (day.eq.p%renewal_day) then
          g%available_water = g%available_water + p%annual_allocation

          ! check overshoot
          if (g%available_water.gt.p%max_available_water) then
              g%available_water = p%max_available_water
          endif

          call get_name(source_name)

          write(msg_string,*) &
          'Annual Allocation granted to ',source_name, &
          '.  Available water now equals ',g%available_water
          call write_string (msg_string)

      endif
   endif

   call pop_routine (my_name)
   return
end subroutine



!  ====================================================================
subroutine WaterSupply_send_my_variable (variable_name)
!  ====================================================================
   Use Infrastructure
   implicit none

!+ Sub-Program Arguments
   character variable_name*(*)      ! (input) variable name to search for

!+ Purpose
!  return the value of a variable in return_string.  used to return
!  values of variables requested by other modules.

!+ Notes
!  a flag is set if any of the totals is requested.  the totals are
!  reset during the next process phase when this happens.

!+ Mission Statement
!  Send Value of Requested Variable

!+ Changes
!  dsg 150603  built

!+ Constant Values
   character  my_name*(*)           ! name of subroutine
   parameter (my_name = 'WaterSupply_send_my_variable')

!+ Local Variables
   integer    solnum           !   solute number

!- Implementation Section ----------------------------------

   call push_routine (my_name)

   if (variable_name .eq. 'rain_capture') then
       call respond2get_real_var (variable_name,'(ML)', g%rain_capture)


   elseif (variable_name .eq. 'available_water') then
       call respond2get_real_var (variable_name,'(ML)', g%available_water)

   elseif (variable_name .eq. 'evaporation') then
       call respond2get_real_var (variable_name,'(ML)', g%evaporation)

   elseif (variable_name .eq. 'seepage') then
       call respond2get_real_var (variable_name,'(ML)', g%seepage)

   elseif (variable_name .eq. 'overflow') then
       call respond2get_real_var (variable_name,'(ML)', g%overflow)


   elseif (variable_name .eq. 'runoff_input') then
       call respond2get_real_var (variable_name,'(ML)', g%total_runoff)

   elseif (variable_name .eq. 'irrig_water_supplied') then
       call respond2get_real_var (variable_name,'(ML)', g%irrig_water_supplied)

   elseif (variable_name .eq. 'available_depth') then
       call respond2get_real_var (variable_name,'(m)', g%available_depth)

   elseif (variable_name .eq. 'max_available_water') then
       call respond2get_real_var (variable_name,'(Ml)', p%max_available_water)

   elseif (variable_name .eq. 'min_volume') then
       call respond2get_real_var (variable_name,'(Ml)', p%min_volume)

   elseif (variable_name .eq. 'max_pump') then
       call respond2get_real_var (variable_name,'(Ml/day)', p%max_pump)

   elseif (variable_name .eq. 'annual_allocation') then
       call respond2get_real_var (variable_name,'(Ml)', p%annual_allocation)

   elseif (variable_name .eq. 'allocation_renewal_day') then
       call respond2get_integer_var (variable_name,'()', p%renewal_day)

   elseif (variable_name .eq. 'full') then
       call respond2get_integer_var (variable_name,'()', g%full)

   elseif (variable_name .eq. 'filling_event') then
       call respond2get_integer_var (variable_name,'()', g%filling_event)

   ! solute outputs
   else if (index(Variable_name,'storage_').eq.1) then

      solnum = WaterSupply_solute_number (Variable_name(9:))

      if (solnum.gt.0)then
         call respond2Get_real_var (Variable_name,'(ppm)',g%solute_conc(solnum))
      endif

   else
      ! not my variable


      call Message_unused ()
   endif

   call pop_routine (my_name)
   return
end subroutine

!  ===========================================================
subroutine WaterSupply_ONgimme_water ()
!  ===========================================================
   Use Infrastructure
   implicit none

!+ Purpose
!  Supply Water to requesting module

!+ Mission Statement
!

!+ Changes
!  dsg 150603  built

!+ Constant Values
   character  my_name*(*)           ! name of subroutine
   parameter (my_name = 'WaterSupply_ONgimme_water')

!+ Local Variables
   character  err_string*200         ! Error message string
   real       water_requested        ! The amount of water wanted by the requesting module
   real       pot_water_supplied     ! The water which can be potentially supplied, not accounting for pumping limitations
   real       water_supplied         ! The water which can actually be supplied today by this source
   integer    numvals                ! Number of values returned
   character  water_requester*(Max_module_name_size) ! Module (instance) name sending request for water
   character  water_provider*(Max_module_name_size) ! Module (instance) name of this module
   integer i
!- Implementation Section ----------------------------------

   call push_routine (my_name)

   call get_name(water_provider)

   !****** collect information on the sender and the amount of water required ******

   call collect_char_var ('water_requester','()',water_requester,numvals)
   call collect_real_var ('amount','(Ml)',water_requested,numvals,0.0,10000.0)
   if (numvals .ne. 1) then
      call fatal_error (ERR_USER,'Request for Water from source provided no amountinformation')
   endif


   !******* check if this water amount is available ******

   if (water_requested.le.(g%available_water - p%min_volume)) then

      pot_water_supplied = water_requested

   else
      pot_water_supplied = max(g%available_water - p%min_volume, 0.0)

      write(err_string,*) &
      'Water supply request limited by available capacity from ' &
      ,trim(water_provider),' to ',pot_water_supplied,' ML'
      call write_string (err_string)

   endif

   !******* check if this potential water supply can be provided today by pump *****

   if (pot_water_supplied.le.p%max_pump) then

       water_supplied = pot_water_supplied

   else

       water_supplied = p%max_pump

       write(err_string,*) &
       'Water supply request limited by pump supply capacity from ' &
       ,trim(water_provider),' to ',water_supplied,' Ml'
       call write_string (err_string)

   endif


   !******** Update Pools *******

   g%available_water = g%available_water - water_supplied

   if(water_requester.eq.'irrigate') then
   g%irrig_water_supplied = water_supplied
   endif

   !***** Send WaterSupplied Method  ******

   call new_postbox()

   call post_char_var ('water_provider', '()', water_provider)
   call post_real_var ('water_requested', '(Ml)', water_requested)
   call post_real_var ('water_supplied', '(Ml)', water_supplied)
   call post_real_array ('solute_concentrations_supplied','(ppm)', g%solute_conc, g%num_solutes)

   call Event_send_directed(water_requester,'water_supplied')

   call delete_postbox()

   call pop_routine (my_name)
   return
end subroutine


!  ===========================================================
subroutine WaterSupply_ONwater_supplied ()
!  ===========================================================
   Use Infrastructure
   implicit none

!+ Purpose
!  Receive top_up water from a sending module

!+ Mission Statement
!

!+ Changes
!  dsg 150603  built

!+ Constant Values
   character  my_name*(*)           ! name of subroutine
   parameter (my_name = 'WaterSupply_ONwater_supplied')

!+ Local Variables
   integer    numvals                               ! Number of values returned
   integer i                                        ! simple counter

   character  water_provider*(Max_module_name_size) ! name of module providing water for top-up
   character  water_requester*(Max_module_name_size)! the name of this instance which is requesting a top-up from another source
   character  err_string*200
   real       water_requested                       ! top-up water requested by this module Ml
   real       water_supplied                        ! top-up water provided by module 'water-provider' Ml
   real       water_still_needed                    ! top-up water still required following provision Ml
   real       solute_conc_supplied(max_solutes)     ! array of solute concentrations in supplied water ppm
   real       new_solute_conc                       ! storage solute concentration modified due to added water ppm

!- Implementation Section ----------------------------------

   call push_routine (my_name)

   !****** collect information on the sender and the amount of water required ****************

   call collect_char_var ('water_provider','()',water_provider,numvals)
   if (numvals .ne. 1) then
      call fatal_error (ERR_USER,'Top up water provider not specified')
   endif

   call collect_real_var ('water_requested','(Ml)',water_requested,numvals,0.0,10000.0)
   if (numvals .ne. 1) then
      call fatal_error (ERR_USER,'Top up water amount not provided')
   endif

   call collect_real_var ('water_supplied','(Ml)',water_supplied,numvals,0.0,10000.0)
   if (numvals .ne. 1) then
         call fatal_error (ERR_USER,'Top up water amount not provided')
   endif

   call collect_real_array ('solute_concentrations_supplied',g%num_solutes,'(ppm)' &
                            ,solute_conc_supplied &
                            ,numvals &
                            ,0.0 &
                            ,10000.0)
   if (numvals .ne. g%num_solutes) then
      call fatal_error (ERR_USER,'Solute concentration array sizes do not match')
   endif


   !******* check if this water amount is enough to satisfy requirements ***************************

   if (water_supplied.eq.water_requested) then

      ! Everyone happy, simply increment water pool
      g%available_water = g%available_water + water_supplied

   else

      ! More water still need to satisfy top-up requirements
      water_still_needed = water_requested - water_supplied

      ! Update pool with the amount supplied
      g%available_water = g%available_water + water_supplied

      ! Send another gimme_water to the next preferred source with the amount still required

      write(err_string,*)'Water_supplied is less than water requested. '
      call write_string (err_string)
      write(err_string,*)'Still need ',water_still_needed,' ML of water'
      call write_string (err_string)

      call new_postbox()

      call get_name(water_requester)

      call post_char_var ('water_requester', '()', water_requester)

      call post_real_var ('amount', '(Ml)', water_still_needed)

      g%source_counter = g%source_counter + 1

      if(g%source_counter.gt.g%tot_num_sources) then
           ! in other words, all the specified sources have been tried
           write(err_string,*) &
           'WARNING : No more water available from specified sources - deficit still equals' &
           ,water_still_needed ,' ML of water'
           call write_string (err_string)
      else
           call Event_send_directed(g%top_up_source(g%source_counter),'gimme_water')
      endif

      call delete_postbox()

   endif


   ! dsg 050803 Now modify solute concentrations due to water supplied
   do i=1,g%num_solutes
     if((g%available_water + water_supplied).eq.0.0) then
        g%solute_conc(i) = 0.0
     else   
        new_solute_conc = divide(((g%solute_conc(i) * g%available_water) &
                       + (solute_conc_supplied(i) * water_supplied)),(g%available_water + water_supplied),0.0)
        g%solute_conc(i) = new_solute_conc
        new_solute_conc = 0.0
     endif   
   end do

   call pop_routine (my_name)
   return
end subroutine



!  ===========================================================
subroutine WaterSupply_ONtop_up ()
!  ===========================================================
   Use Infrastructure
   implicit none

!+ Purpose
!  Respond to 'top_up' request from manager by sending out a gimme_water action

!+ Mission Statement
!

!+ Changes
!  dsg 150603  built

!+ Constant Values
   character  my_name*(*)           ! name of subroutine
   parameter (my_name = 'WaterSupply_ONtop_up')

!+ Local Variables
   integer    numvals                ! Number of values returned
   character  water_requester*200    ! the name of this instance which is requesting a top-up from another source
   integer    i                      ! simple counter
   real       top_up_required        ! water amount required for top-up
   integer    counter
!- Implementation Section ----------------------------------

   call push_routine (my_name)

   !****** collect information on top_up amount required and preferred sources ****************

   call collect_real_var ('amount', '(Ml)', top_up_required, numvals, 0.0, 10000.0)
   if (numvals .ne. 1) then
      call fatal_error (ERR_USER,'No top-up water amount information provided')
   endif

   call collect_char_array ('source',max_sources, '()', g%top_up_source, g%tot_num_sources)
   if (g%tot_num_sources .eq. 0) then
         call fatal_error (ERR_USER,'No top-up water source information provided')
   endif


   !  Now send out a gimme_water method call to the first specified source
   call new_postbox()
   call get_name(water_requester)
   call post_char_var ('water_requester', '()', water_requester)
   call post_real_var ('amount', '(Ml)', top_up_required)
   g%source_counter = 1
   call Event_send_directed(g%top_up_source(1),'gimme_water')
   call delete_postbox()

   call pop_routine (my_name)
   return
end subroutine


!  ===========================================================
subroutine WaterSupply_zero_variables ()
!  ===========================================================
   Use Infrastructure
   implicit none

!+ Purpose
!  zero variables & arrays

!+ Mission Statement
!  Zero Variables

!+ Changes
!  dsg 150603  built

!+ Constant Values
   character  my_name*(*)           ! module name
   parameter (my_name  = 'WaterSupply_zero_variables')

!- Implementation Section ----------------------------------

   call push_routine (my_name)

!  ====================================================================
! Globals
    g%full = 0
    g%filling_event = 0

! ====================================================================
! Parameters

! ====================================================================
! Constants
!         c%storage_design = 0                    ! actual soil evaporation model being used


! =====================================================================

   call pop_routine (my_name)
   return
end subroutine


!  ===========================================================
subroutine WaterSupply_zero_event_data ()
!  ===========================================================
   Use Infrastructure
   implicit none

!+ Purpose
!  Zero information describing event data from other modules

!+ Mission Statement
!  Zero information describing event data from other modules

!+ Changes
!  dsg 150603  built

!+ Constant Values
   character  my_name*(*)           ! module name
   parameter (my_name  = 'WaterSupply_zero_event_data')

!- Implementation Section ----------------------------------

   call push_routine (my_name)

   g%year = 0                           ! year
   g%day  = 0                           ! day of year
   g%rain = 0.0                         ! precipitation (mm/d)
   g%radn = 0.0                         ! solar radiation (mj/m^2/day)
   g%mint = 0.0                         ! minimum air temperature (oC)
   g%maxt = 0.0                         ! maximum air temperature (oC)

   call pop_routine (my_name)
   return
end subroutine

!  ===========================================================
subroutine WaterSupply_zero_daily_variables ()
!  ===========================================================
   Use Infrastructure
   implicit none

!+ Purpose
!  zero variables & arrays

!+ Mission Statement
!  Zero Daily Variables

!+ Changes
!  dsg 150603  built

!+ Constant Values
   character  my_name*(*)           ! module name
   parameter (my_name  = 'WaterSupply_zero_daily_variables')

!+ Local Variables
   integer layer                    ! soil layer number counter
   integer solnum                   ! solute number counter

!- Implementation Section ----------------------------------

   call push_routine (my_name)

   g%rain_capture       = 0.0
   g%source_counter     = 0
   g%tot_num_sources    = 0
   g%total_runoff       = 0.0
   g%evaporation        = 0.0
   g%seepage            = 0.0
   g%overflow           = 0.0
   g%irrig_water_supplied     = 0.0
   call pop_routine (my_name)
   return
end subroutine


!  ====================================================================
subroutine WaterSupply_sum_report ()
!  ====================================================================
   Use Infrastructure
   implicit none

!+ Purpose
!  <insert here>

!+ Mission Statement
!  Report WaterSupply module summary details

!+ Changes
!  dsg 150603  built

!+ Constant Values
   character  my_name*(*)           ! name of current procedure
   parameter (my_name = 'WaterSupply_sum_report')

!+ Local Variables
   integer    i                     ! simple counter
   character  line*100              ! temp output record
   character  report_string*200     ! string message sent to summary file

!- Implementation Section ----------------------------------
   call push_routine (my_name)

   call write_string (new_line)

   write(report_string,*)'Type of storage specified is a ',trim(p%source_type)
   call write_string (report_string)

   if(p%receive_catchment_runoff.eq.'yes') then
      write(report_string,*)'This storage directly receives runoff from the catchment'
   else
      write(report_string,*)'This storage does not directly receive runoff from the catchment'
   endif
   call write_string (report_string)

   write(report_string,*)'The maximum storage capacity is ',p%max_available_water,' Ml'
   call write_string (report_string)

   write(report_string,*)'The initial available volume is ',g%available_water,' Ml'
   call write_string (report_string)

   write(report_string,*)'The maximum pumping rate from this storage is  ',p%max_pump,' Ml/day'
   call write_string (report_string)

   if (p%source_type.eq.'dam_gully'.or. &
       p%source_type.eq.'dam_ring'.or. &
       p%source_type.eq.'dam_exc'.or. &
       p%source_type.eq.'sump') then

      write(report_string,*)'Storage surface area at capacity is ',p%max_area,' ha'
      call write_string (report_string)

      write(report_string,*)'Storage sealing layer permeability is ',p%permeability,' m/d'
      call write_string (report_string)

      write(report_string,*)'Sealing layer thickness is ',p%seal_thickness,' m'
      call write_string (report_string)

      write(report_string,*)'Storage volume below which pumping is prohibited ',p%min_volume,' Ml'
      call write_string (report_string)

      write(report_string,*)'Storage geometry factor "b" is ',c%b
      call write_string (report_string)

   endif

   if (p%source_type.eq.'bore'.or. &
       p%source_type.eq.'river') then
      write(report_string,*)'Annual Allocation is ',p%annual_allocation,' Ml'
      call write_string (report_string)
      write(report_string,*)'Allocation renewal day is day ',p%renewal_day
      call write_string (report_string)
   endif

   call write_string (new_line)
   write(report_string,*)'STORAGE SOLUTE INFORMATION '
   call write_string (report_string)

   do i = 1,g%num_solutes
      write(report_string,*) &
      'System Solute number ',i,' is ',trim(g%solute_names(i)) &
      ,' and the initial concentration in this storage is ' &
      ,g%solute_conc(i),' ppm'
      call write_string (report_string)
   end do

   call pop_routine (my_name)
   return
end subroutine


!  ===========================================================
subroutine WaterSupply_init ()
!  ===========================================================
   Use Infrastructure
   implicit none

!+ Purpose
!  input initial values from soil water parameter files.

!+ Mission Statement
!  Initialise SoilWat module

!+ Changes
!  dsg 150603  built

!+ Constant Values
   character  my_name*(*)           ! name of subroutine
   parameter (my_name  = 'WaterSupply_init')

!- Implementation Section ----------------------------------

   call push_routine (my_name)
   ! zero pools

   ! Get all coefficients from file

   call WaterSupply_read_parameters ()
   call WaterSupply_read_constants ()

   call pop_routine (my_name)
   return
end subroutine


!  ===========================================================
subroutine WaterSupply_ONtick (variant)
!  ===========================================================
   Use Infrastructure
   implicit none

   integer, intent(in) :: variant

!+ Purpose
!  Update internal time record and reset daily state variables.

!+ Mission Statement
!  Update internal time record and reset daily state variables.

!+ Changes
!  dsg 150603  built

!+ Local Variables
   type(timeType) :: tick

!+ Constant Values
   character*(*) myname               ! name of current procedure
   parameter (myname = 'WaterSupply_ONtick')

!- Implementation Section ----------------------------------
   call push_routine (myname)

   call unpack_time(variant, tick)
   call jday_to_day_of_year(dble(tick%startday), g%day, g%year)

   call pop_routine (myname)
   return
end subroutine
!  ===========================================================
subroutine WaterSupply_ONnewmet (variant)
!  ===========================================================
   Use Infrastructure
   implicit none

   integer, intent(in) :: variant
!+ Purpose
!  Get new met data

!+ Mission Statement
!  Get new met data

!+ Changes
!  dsg 150603  built

!+ Local Variables
   type(newmetType) :: newmet
   integer numvals

!+ Constant Values
   character*(*) myname               ! name of current procedure
   parameter (myname = 'WaterSupply_ONnewmet')

!- Implementation Section ----------------------------------
   call push_routine (myname)

   call unpack_newmet(variant, newmet)

   g%radn = newmet%radn
   g%maxt = newmet%maxt
   g%mint = newmet%mint
   g%rain = newmet%rain

   call pop_routine (myname)
   return
end subroutine


!  ===========================================================
subroutine WaterSupply_ONnew_solute ()
!  ===========================================================
   Use infrastructure
   implicit none

!+ Purpose
!  Add new solute to internal list of system solutes

!+ Mission Statement
!  Add new solute information to list of system solutes

!+ Changes
!  2907039 dsg - specified

!+ Constant Values
   character  my_name*(*)           ! this subroutine name
   parameter (my_name = 'WaterSupply_ONnew_solute')

   character  section_name*(*)
   parameter (section_name = 'parameters')

!+ Calls


!+ Local Variables
   integer numvals
   character names(max_solutes)*32
   character sender*(max_module_name_size)
   integer counter1

   character err_string*200


!- Implementation Section ----------------------------------

   call push_routine (my_name)

   call collect_char_var (DATA_sender,'()',sender,numvals)


   call collect_char_array (DATA_new_solute_names,max_solutes,'()',names,numvals)
   if (g%num_solutes+numvals.gt.max_solutes) then
      call fatal_error (ERR_Internal,'Too many solutes for System')
   else
      do counter1 = 1, numvals
         g%num_solutes = g%num_solutes + 1
         g%solute_names(g%num_solutes) = names(counter1)
         g%solute_owners(g%num_solutes) = sender
      end do
   endif

   call pop_routine (my_name)
   return
end subroutine



!  ====================================================================
integer function WaterSupply_solute_number (solname)
!  ====================================================================
   Use infrastructure
   implicit none


!+ Sub-Program Arguments
    character solname*(*)

!+ Purpose
!  Get the solutes number

!+ Mission statement
!  Get the solutes number

!+ Changes
!

!+ Constant Values
   character myname*(*)               ! name of current procedure
   parameter (myname = 'WaterSupply_solute_number')

!+ Local Variables
    integer counter
    integer solnum

!- Implementation Section ----------------------------------
   call push_routine (myname)

   solnum = 0
   do counter = 1, g%num_solutes
      if (g%solute_names(counter).eq.solname) then
         solnum = counter
      else
      endif
   end do

   WaterSupply_solute_number = solnum

   call pop_routine (myname)
   return
end function


!  ====================================================================
subroutine WaterSupply_create ()
!  ====================================================================
   Use Infrastructure
   implicit none

!+ Purpose
!  Create

!+ Mission statement
!  Create

!+ Changes
!  dsg 150603  built

!+ Constant Values
   character*(*) myname               ! name of current procedure
   parameter (myname = 'WaterSupply_create')

!+ Local Variables

!- Implementation Section ----------------------------------
   call push_routine (myname)

   call doRegistrations(id)


   call WaterSupply_zero_variables ()
   call WaterSupply_zero_event_data ()


   call pop_routine (myname)
   return
end subroutine
   end module WaterSupplyModule

!  ===========================================================
subroutine alloc_dealloc_instance(doAllocate)
!  ===========================================================
   use WaterSupplyModule
   implicit none
   ml_external alloc_dealloc_instance

!+ Sub-Program Arguments
   logical, intent(in) :: doAllocate

!+ Purpose
!  Module instantiation routine.

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
end subroutine



!  ====================================================================
subroutine Main (action, data_string)
!  ====================================================================
   Use Infrastructure
   use WaterSupplyModule
   implicit none
   ml_external Main

!+ Sub-Program Arguments
   character action*(*)             ! (input) action to perform
   character data_string*(*)        ! (input) data for action

!+ Purpose
!  ???

!+ Mission Statement
!  Handles communications for WaterSupply

!+ Changes
!  ?????

!+ Constant Values
   character  my_name*(*)           ! name of this module
   parameter (my_name = 'WaterSupply')

!- Implementation Section ----------------------------------

   call push_routine (my_name)

   if (action.eq.ACTION_get_variable) then
     ! respond to request for variable values - from modules
      call WaterSupply_send_my_variable (Data_string)

   else if (action.eq.ACTION_process) then
      call WaterSupply_ONprocess ()

   else if (action.eq.ACTION_gimme_water) then
      call WaterSupply_ONgimme_water ()

   else if (action.eq.ACTION_top_up) then
      call WaterSupply_ONtop_up ()

   else if (action.eq.'water_supplied') then
      call WaterSupply_ONwater_supplied ()

   else if (action.eq.ACTION_init) then
      call WaterSupply_init ()
      call WaterSupply_sum_report ()

   else if (action.eq.ACTION_create) then
      call doRegistrations(id)
      call WaterSupply_create()

   else if (Action .eq. EVENT_new_solute) then
      call WaterSupply_ONnew_solute ()

   else
      ! don't use message
      call Message_unused ()
   endif

   call pop_routine (my_name)
   return
end subroutine


!  ====================================================================
!  This routine is the event handler for all events
!  ====================================================================
subroutine respondToEvent(fromID, eventID, variant)
   use WaterSupplyModule
   Use infrastructure
   implicit none
   ml_external respondToEvent

   integer, intent(in) :: fromID
   integer, intent(in) :: eventID
   integer, intent(in) :: variant

   if (eventID .eq. id%tick) then
      call WaterSupply_zero_daily_variables ()
      call WaterSupply_ONtick(variant)
   elseif (eventID .eq. id%newmet) then
      call WaterSupply_ONnewmet(variant)
   endif
   return
end subroutine respondToEvent

