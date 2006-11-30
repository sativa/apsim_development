      include 'Erosion.inc'
!     ===========================================================
      subroutine AllocInstance (InstanceName, InstanceNo)
!     ===========================================================
      use ErosionModule
      implicit none
 
!+  Sub-Program Arguments
      character InstanceName*(*)       ! (INPUT) name of instance
      integer   InstanceNo             ! (INPUT) instance number to allocate
 
!+  Purpose
!      Module instantiation routine.
 
!- Implementation Section ----------------------------------
               
      allocate (Instances(InstanceNo)%gptr)
      allocate (Instances(InstanceNo)%pptr)
      Instances(InstanceNo)%Name = InstanceName
 
      return
      end

!     ===========================================================
      subroutine FreeInstance (anInstanceNo)
!     ===========================================================
      use ErosionModule
      implicit none
 
!+  Sub-Program Arguments
      integer anInstanceNo             ! (INPUT) instance number to allocate
 
!+  Purpose
!      Module de-instantiation routine.
 
!- Implementation Section ----------------------------------
               
      deallocate (Instances(anInstanceNo)%gptr)
      deallocate (Instances(anInstanceNo)%pptr)
       
      return
      end
     
!     ===========================================================
      subroutine SwapInstance (anInstanceNo)
!     ===========================================================
      use ErosionModule
      implicit none
 
!+  Sub-Program Arguments
      integer anInstanceNo             ! (INPUT) instance number to allocate
 
!+  Purpose
!      Swap an instance into the global 'g' pointer
 
!- Implementation Section ----------------------------------
               
      g => Instances(anInstanceNo)%gptr
      p => Instances(anInstanceNo)%pptr
       
      return
      end
* ====================================================================
      subroutine Main (Action, Data_string)
* ====================================================================
      use ErosionModule
      implicit none
      include   'const.inc'            ! Global constant definitions
      include 'action.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      character  Action*(*)            ! Message action to perform
      character  Data_string*(*)       ! Message data

*+  Purpose
*     This routine is the interface between the main system and the
*     erosion module.

*+  Changes
*     DMS 25/02/94 (new template)
*     PdeV 25/08/94
*     011195 jngh  added call to message_unused
*     190599 jngh removed reference to version and removed ACTION_presence

*+  Constant Values
      character  my_name*(*)
      parameter (my_name = 'erosion')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
         ! initialise error flags
 
      if (Action.eq.ACTION_Init) then
            ! initilization once per run
            ! start with a clean slate
         call erosion_zero_variables ()
         call erosion_get_other_variables ()
         call erosion_init ()   ! get parameters & do initial one-off calc's
         call erosion_write_summary () ! tell summary file what we're using
 
      else if (Action.eq.ACTION_Process) then
         call erosion_zero_daily_variables ()
            ! get todays variables
         call erosion_get_other_variables ()
            ! do daily processes
         call erosion_process ()
            ! send back changed variables.
         call erosion_set_other_variables ()
 
      else if (Action.eq.ACTION_Get_variable) then
            ! respond to requests from other modules
         call erosion_send_my_variable (Data_string)
 
      else if (Action.eq.ACTION_Set_variable) then
         call erosion_set_my_variable (data_string)
 
      else if (Action.eq.ACTION_end_run) then
         call erosion_end_run ()
 
      else
            ! Do nothing..
         call Message_unused ()
      endif
 
      call pop_routine (my_name)
      return
      end



* ====================================================================
      subroutine erosion_init ()
* ====================================================================
      use ErosionModule
      implicit none
      include   'const.inc'
      include   'convert.inc'          ! pcnt2fract
      include 'data.pub'                          
      include 'error.pub'                         

*+  Purpose
*     Initialise erosion module

*+  Changes
*     DMS 25/02/94 (new template)
*     190599 jngh removed reference to version

*+  Constant Values
      character  my_name*(*)
      parameter (my_name = 'erosion_init')

*+  Local Variables
      real       s                     ! temporary (USLE LS factor) slope (0-1)
      real       a                     ! temporary (USLE LS factor)

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
         ! Notify system that we have initialised
      call Write_string (' Initialising: ')
 
         ! Get all parameters from parameter file
      call erosion_read_param ()
 
         ! Sanity checking
      if (g%bed_depth .lt. sum_real_array(g%dlayer, max_layer)) then
          call fatal_error(err_user,
     :                  'Depth to bedrock is less than profile depth')
      else
      endif
 
      if (p%model_type .ne. freeb_model .and.
     :    p%model_type .ne. rose_model ) then
         call fatal_error(err_user, 'Unknown model_type.')
      else
      endif
 
         ! Calculate USLE LS factor
      s = p%slope * pcnt2fract
      a = 0.6 * (1.0 - exp (-35.835 * s))
      p%ls_factor = ((p%slope_length / 22.1) ** a)
     :            * (65.41*s*s + 4.56*s + 0.065)
 
      if (p%profile_reduction .eq. on) then
 
            ! find soil profile to calculate
            ! initial layer_merge_mm
         p%layer_merge_mm = g%dlayer(
     :                      count_of_real_vals (g%dlayer, max_layer))
     :                    * p%profile_layer_merge
 
      else
         p%layer_merge_mm = 0.0
      endif
 
 
      call pop_routine (my_name)
      return
      end



* ====================================================================
      subroutine erosion_write_summary ()
* ====================================================================
      use ErosionModule
      implicit none
      include   'const.inc'            ! Constant definitions
      include 'error.pub'                         

*+  Purpose
*     Tell summary file what parameters we're using

*+  Changes
*     DMS 25/02/94 (new template)
*     PdeV 2/10/94

*+  Constant Values
      character  my_name*(*)
      parameter (my_name = 'erosion_write_summary')

*+  Local Variables
      character  string*(500)          ! String to output

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      call write_string (new_line//new_line)
 
      string = '                 Erosion Parameters'
      call write_string (string)
 
      string = '     -----------------------------------------------'
      call write_string (string)
 
      if (p%profile_reduction .eq. on) then
         write (string, '(a)')
     :          '      Profile reduction:                     on'
         call write_string (string)
 
         write (string, '(a, f4.3)')
     :          '      Fraction of original layer for merge: '
     :                  , p%profile_layer_merge
         call write_string (string)
 
      else
         write (string, '(a)')
     :          '      Profile reduction:                     off'
         call write_string (string)
      endif
 
      if (p%model_type .eq. freeb_model) then
         write (string, '(2a)')
     :          '      Freebairn cover-sediment concentration model'
     :             , new_line
         call write_string (string)
 
         write (string, '(a, f6.4, a)')
     :          '      LS factor:                             ',
     :          p%ls_factor, new_line
         call write_string (string)
 
                     ! susp load K is 0.0 if not being used
         if (p%k_factor_susp .le. 0.0) then
           write (string, '(a, f6.4, a)')
     :          '      K factor:                              ',
     :          p%k_factor_bed, new_line
           call write_string (string)
         else
           write (string, '(a, f6.4, a)')
     :          '      K factor (bedload):                    ',
     :          p%k_factor_bed, new_line
           call write_string (string)
           write (string, '(a, f6.4, a)')
     :          '      K factor (suspended load):             ',
     :          p%k_factor_susp, new_line
           call write_string (string)
         endif
 
         write (string, '(a, f6.4, a)')
     :          '      P factor:                              ',
     :          p%p_factor, new_line
         call write_string (string)
 
      else if (p%model_type .eq. rose_model) then
 
         write (string, '(2a)')
     :          '      Rose sediment concentration model'
     :                  , new_line
         call write_string (string)
 
         if (p%entrain_eff_susp .le. 0.0) then
           write (string, '(a, f6.4, a)')
     :          '       Efficiency of entrainment:            '
     :               , p%entrain_eff_bed, new_line
           call write_string (string)
         else
           write (string, '(a, f6.4, a)')
     :          '       Efficiency of bed load entrainment:   '
     :               , p%entrain_eff_bed, new_line
           call write_string (string)
           write (string, '(a, f6.4, a)')
     :          '       Efficiency of susp. load entrainment: '
     :               , p%entrain_eff_susp, new_line
           call write_string (string)
         endif
 
         write (string, '(a, f6.2, a)')
     :          '       Slope (%):                            ',
     :          p%slope, new_line
         call write_string (string)
 
      else
            ! whoops - whats going on?
         write (string, '(2a)')
     :          '      ? Unknown model type ?'
     :                  , new_line
         call write_string (string)
 
      endif
 
      string = '     -----------------------------------------------'
      call write_string (string)
 
      call pop_routine (my_name)
      return
      end



* ====================================================================
      subroutine erosion_read_param ()
* ====================================================================
      use ErosionModule
      implicit none
      include   'const.inc'
      include 'read.pub'                          
      include 'error.pub'                         

*+  Purpose
*     Read in all parameters from parameter file.

*+  Changes
*     DMS 25/02/94 (new template)
*     PdeV 25/08/94
*     210395 jngh changed from erosion_section to a parameters section

*+  Constant Values
      character  section_name*(*)
      parameter (section_name = 'parameters')
*
      character  my_name*(*)
      parameter (my_name = 'erosion_read_param')

*+  Local Variables
      integer    num_read              ! temporary
      integer    num_read_eteff        ! temporary
      integer    num_read_b2           ! temporary
      character  string*(80)           ! temporary

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      call write_string (
     :                 new_line//'   - Reading Parameters')
 
                                ! determine model type
      call read_char_var (
     :     section_name         ! Section header
     :   , 'model'              ! Keyword
     :   , '()'                 ! Units
     :   , string               ! Variable
     :   , num_read)            ! Number of values returned
 
      if (string(1:4) .eq. 'rose') then
         p%model_type = rose_model
 
      else if (string(1:4) .eq. 'free') then
         p%model_type = freeb_model
 
      else
         p%model_type = 0
 
      endif
 
      call read_char_var (
     :     section_name         ! Section header
     :   , 'profile_reduction'  ! Keyword
     :   , '()'                 ! Units
     :   , string               ! Variable
     :   , num_read)            ! Number of values returned
 
      if (string(1:2) .eq. 'on') then
         p%profile_reduction = on
      else
         p%profile_reduction = off
      endif
 
      call read_real_var (
     :     section_name         ! Section header
     :   , 'profile_layer_merge'  ! Keyword
     :   , '()'                 ! Units
     :   , p%profile_layer_merge  ! Variable
     :   , num_read             ! Number of values returned
     :   , 0.0                  ! Lower Limit for bound checking
     :   , 1.0)                 ! Upper Limit for bound checking
 
      call read_real_var (
     :     section_name         ! Section header
     :   , 'minimum_depth'      ! Keyword
     :   , '()'                 ! Units
     :   , p%minimum_depth      ! Variable
     :   , num_read             ! Number of values returned
     :   , 0.0                  ! Lower Limit for bound checking
     :   , 1000.0)              ! Upper Limit for bound checking
 
      call read_real_var (
     :     section_name         ! Section header
     :   , 'slope'              ! Keyword
     :   , '()'                 ! Units
     :   , p%slope              ! Variable
     :   , num_read             ! Number of values returned
     :   , 0.0                  ! Lower Limit for bound checking
     :   , 100.0)               ! Upper Limit for bound checking
 
      call read_real_var (
     :     section_name         ! Section header
     :   , 'slope_length'       ! Keyword
     :   , '()'                 ! Units
     :   , p%slope_length       ! Variable
     :   , num_read             ! Number of values returned
     :   , 0.0                  ! Lower Limit for bound checking
     :   , 100.0)               ! Upper Limit for bound checking
 
      call read_real_var (
     :     section_name         ! Section header
     :   , 'bed_depth'          ! Keyword
     :   , '()'                 ! Units
     :   , g%bed_depth          ! Variable
     :   , num_read             ! Number of values returned
     :   , 0.0                  ! Lower Limit for bound checking
     :   , 5000.0)              ! Upper Limit for bound checking
 
cPdeV - used for fudges
c      call read_real_var (
c     :     section_name         ! Section header
c     :   , 'crop_cover_wtg'     ! Keyword
c     :   , '()'                 ! Units
c     :   , p%crop_cover_wtg     ! Variable
c     :   , num_read             ! Number of values returned
c     :   , 0.0                  ! Lower Limit for bound checking
c     :   , 1.0)                 ! Upper Limit for bound checking
 
      call read_real_var_optional (
     :     section_name         ! Section header
     :   , 'cover_extra'        ! Keyword
     :   , '()'                 ! Units
     :   , g%cover_extra        ! Variable
     :   , num_read             ! Number of values returned
     :   , -1.0                 ! Lower Limit for bound checking
     :   , 1.0)                 ! Upper Limit for bound checking
 
                                ! model specific parameters..
      if (p%model_type .eq. freeb_model) then
 
         call read_real_var_optional (
     :        section_name      ! Section header
     :      , 'k_factor'        ! Keyword
     :      , '()'              ! Units
     :      , p%k_factor_bed    ! Variable
     :      , num_read          ! Number of values returned
     :      , 0.0               ! Lower Limit for bound checking
     :      , 1.0)              ! Upper Limit for bound checking
         if (num_read .le. 0) then
           call read_real_var (
     :          section_name      ! Section header
     :        , 'k_factor_bed'    ! Keyword
     :        , '()'              ! Units
     :        , p%k_factor_bed    ! Variable
     :        , num_read          ! Number of values returned
     :        , 0.0               ! Lower Limit for bound checking
     :        , 1.0)              ! Upper Limit for bound checking
           call read_real_var (
     :          section_name      ! Section header
     :        , 'k_factor_susp'   ! Keyword
     :        , '()'              ! Units
     :        , p%k_factor_susp   ! Variable
     :        , num_read          ! Number of values returned
     :        , 0.0               ! Lower Limit for bound checking
     :        , 1.0)              ! Upper Limit for bound checking
         else
            ! Nothing
         endif
         call read_real_var (
     :        section_name      ! Section header
     :      , 'p_factor'        ! Keyword
     :      , '()'              ! Units
     :      , p%p_factor        ! Variable
     :      , num_read          ! Number of values returned
     :      , 0.0               ! Lower Limit for bound checking
     :      , 1.0)              ! Upper Limit for bound checking
 
      else if (p%model_type .eq. rose_model) then
 
         call read_real_var_optional (
     :        section_name      ! Section header
     :      , 'entrain_eff'     ! Keyword
     :      , '()'              ! Units
     :      , p%entrain_eff_bed ! Variable
     :      , num_read_eteff    ! Number of values returned
     :      , 0.0               ! Lower Limit for bound checking
     :      , 2.0)              ! Upper Limit for bound checking
         call read_real_var_optional (
     :        section_name      ! Section header
     :      , 'eros_rose_b2'    ! Keyword
     :      , '()'              ! Units
     :      , p%eros_rose_b2_bed ! Variable
     :      , num_read_b2       ! Number of values returned
     :      , 0.01              ! Lower Limit for bound checking
     :      , 0.2)              ! Upper Limit for bound checking
 
         if (num_read_eteff .ne. 1 .or. num_read_b2 .ne. 1) then
           call read_real_var (
     :          section_name      ! Section header
     :        , 'entrain_eff_bed'     ! Keyword
     :        , '()'              ! Units
     :        , p%entrain_eff_bed ! Variable
     :        , num_read_eteff    ! Number of values returned
     :        , 0.0               ! Lower Limit for bound checking
     :        , 2.0)              ! Upper Limit for bound checking
           call read_real_var (
     :          section_name      ! Section header
     :        , 'eros_rose_b2_bed' ! Keyword
     :        , '()'              ! Units
     :        , p%eros_rose_b2_bed ! Variable
     :        , num_read_b2       ! Number of values returned
     :        , 0.01              ! Lower Limit for bound checking
     :        , 0.2)              ! Upper Limit for bound checking
           call read_real_var (
     :          section_name      ! Section header
     :        , 'entrain_eff_susp'     ! Keyword
     :        , '()'              ! Units
     :        , p%entrain_eff_susp ! Variable
     :        , num_read_eteff    ! Number of values returned
     :        , 0.0               ! Lower Limit for bound checking
     :        , 2.0)              ! Upper Limit for bound checking
           call read_real_var (
     :          section_name      ! Section header
     :        , 'eros_rose_b2_susp' ! Keyword
     :        , '()'              ! Units
     :        , p%eros_rose_b2_susp ! Variable
     :        , num_read_b2       ! Number of values returned
     :        , 0.01              ! Lower Limit for bound checking
     :        , 0.2)              ! Upper Limit for bound checking
         else
             ! Nothing - we're not splitting soil loss.
         endif
      else
                                ! nothing - unknown model type. A fatal error will be sent.
      endif
 
      call pop_routine (my_name)
      return
      end



* ====================================================================
      subroutine erosion_zero_variables ()
* ====================================================================
      use ErosionModule
      implicit none
      include 'error.pub'                         

*+  Purpose
*     Set all variables in this module to zero.

*+  Changes
*     DMS 25/02/94 (new template)

*+  Constant Values
      character my_name*(*)
      parameter (my_name = 'erosion_zero_variables')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      call erosion_zero_daily_variables ()
 
c      p%crop_cover_wtg       = 0.0
      p%entrain_eff_bed      = 0.0
      p%eros_rose_b2_bed     = 0.0
      p%entrain_eff_susp     = 0.0
      p%eros_rose_b2_susp    = 0.0
      p%minimum_depth        = 0.0
      p%model_type           = 0
      p%profile_reduction    = 0
      p%profile_layer_merge  = 0.0
 
      p%slope        = 0.0
      p%slope_length = 0.0
      p%ls_factor    = 0.0
      p%k_factor_bed = 0.0
      p%k_factor_susp= 0.0
      p%p_factor     = 0.0
      p%layer_merge_mm = 0.0
 
      g%bed_depth    = 0.0
      g%runoff       = 0.0
      g%soil_loss_bed  = 0.0
      g%soil_loss_susp = 0.0
      g%day_of_year  = 0
      g%erosion_cover= 0.0
      g%year         = 0
      g%cover_extra  = 0.0
c      g%contact_cover= 0.0
c      g%total_cover  = 0.0
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine erosion_zero_daily_variables ()
*     ===========================================================
      use ErosionModule
      implicit none
      include 'data.pub'                          
      include 'error.pub'                         

*+  Purpose
*       zero erosion daily variables & arrays

*+  Changes
*     010994 jngh specified and programmed
*     210498 pdev added profile resets here due to stale data left in dlayer
*                 after an entire layer was eroded.

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'erosion_zero_daily_variables')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
          !  zero pools etc.
      call fill_real_array (g%dlayer, 0.0, max_layer)
      call fill_real_array (g%dlt_dlayer, 0.0, max_layer)
      call fill_real_array (g%bd, 0.0, max_layer)
 
      g%soil_loss_bed = 0.0
      g%soil_loss_susp = 0.0
c      g%crop_cover = 0.0
c      g%basal_cover = 0.0
c      g%resid_cover = 0.0
 
 
      call pop_routine (my_name)
      return
      end



* ====================================================================
      subroutine erosion_get_other_variables ()
* ====================================================================
      use ErosionModule
      implicit none
      include   'const.inc'            ! Constant definitions
      include 'data.pub'                          
      include 'intrface.pub'                      
      include 'error.pub'                         

*+  Purpose
*     Get the values of variables from other modules

*+  Changes
*     DMS 25/02/94 (new template)
*     PdeV 27/08/94
*     02/11/99 jngh changed total_cover to cover_surface_runoff

*+  Constant Values
      character  my_name*(*)
      parameter (my_name = 'erosion_get_other_variables')

*+  Local Variables
c      real       visible_contact_cover
      integer   numvals
      real      cover_surface_runoff

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
                                ! Get Year
      call get_integer_var(
     :     unknown_module       ! Module that responds (Not Used)
     :   , 'year'               ! Variable Name
     :   , '()'                 ! Units                (Not Used)
     :   , g%year               ! Variable
     :   , numvals              ! Number of values returned
     :   , min_year             ! Lower Limit for bound checking
     :   , max_year  )          ! Upper Limit for bound checking
 
      call get_integer_var(
     :     unknown_module       ! Module that responds (Not Used)
     :   , 'day'                ! Variable Name
     :   , '()'                 ! Units                (Not Used)
     :   , g%day_of_year        ! Variable
     :   , numvals              ! Number of values returned
     :   , 0                    ! Lower Limit for bound checking
     :   , 366  )               ! Upper Limit for bound checking
 
                                ! Get runoff
      call Get_real_var(
     :     unknown_module       ! Module that responds (Not Used)
     :   , 'runoff'             ! Variable Name
     :   , '(mm)'               ! Units                (Not Used)
     :   , g%runoff             ! Variable
     :   , numvals              ! Number of values returned
     :   , 0.0                  ! Lower Limit for bound checking
     :   , 1000.0)              ! Upper Limit for bound checking
 
!      call Get_real_var (
!     :     unknown_module       ! Module that responds (Not Used)
!     :   , 'total_cover'        ! Variable Name
!     :   , '()'                 ! Units                (Not Used)
!     :   , total_cover          ! Variable
!     :   , numvals              ! Number of values returned
!     :   , 0.0                  ! Lower Limit for bound checking
!     :   , 1.0 )                ! Upper Limit for bound checking
 
      call Get_real_var (
     :     unknown_module       ! Module that responds (Not Used)
     :   , 'cover_surface_runoff' ! Variable Name
     :   , '()'                 ! Units                (Not Used)
     :   , cover_surface_runoff          ! Variable
     :   , numvals              ! Number of values returned
     :   , 0.0                  ! Lower Limit for bound checking
     :   , 1.0 )                ! Upper Limit for bound checking
 
!      g%erosion_cover = bound(total_cover + g%cover_extra, 0.0, 1.0)
      g%erosion_cover = bound(cover_surface_runoff + g%cover_extra
     :                        , 0.0, 1.0)
!  when cover extra is included in cover_surface_runoff calc, remove previous calc
!   and add the following line
!      g%erosion_cover = cover_surface_runoff

c$$$c      use this for dms' special covers..
c$$$
c$$$      call Get_real_var(
c$$$     :     unknown_module       ! Module that responds (Not Used)
c$$$     :   , 'crop_cover'         ! Variable Name
c$$$     :   , '()'                 ! Units                (Not Used)
c$$$     :   , g%crop_cover         ! Variable
c$$$     :   , numvals              ! Number of values returned
c$$$     :   , 0.0                  ! Lower Limit for bound checking
c$$$     :   , 1.0)                 ! Upper Limit for bound checking
c$$$
c$$$      call Get_real_var(
c$$$     :     unknown_module       ! Module that responds (Not Used)
c$$$     :   , 'resid_cover'        ! Variable Name
c$$$     :   , '()'                 ! Units                (Not Used)
c$$$     :   , g%resid_cover        ! Variable
c$$$     :   , numvals              ! Number of values returned
c$$$     :   , 0.0                  ! Lower Limit for bound checking
c$$$     :   , 1.0)                 ! Upper Limit for bound checking
c$$$
c$$$      call Get_real_var(
c$$$     :     unknown_module       ! Module that responds (Not Used)
c$$$     :   , 'basal_cover'        ! Variable Name
c$$$     :   , '()'                 ! Units                (Not Used)
c$$$     :   , g%basal_cover        ! Variable
c$$$     :   , numvals              ! Number of values returned
c$$$     :   , 0.0                  ! Lower Limit for bound checking
c$$$     :   , 1.0)                 ! Upper Limit for bound checking
c$$$
c$$$      g%contact_cover = g%resid_cover + g%basal_cover +
c$$$     :     g%cover_extra
c$$$
c$$$      visible_contact_cover = g%contact_cover *
c$$$     :     (1.0 - g%crop_cover * p%crop_cover_wtg)
c$$$
c$$$      g%erosion_cover = bound(visible_contact_cover +
c$$$     :     g%crop_cover * p%crop_cover_wtg, 0.0, 1.0)
      call get_real_array(
     :     unknown_module       ! Module that responds (Not Used)
     :   , 'dlayer'             ! Variable Name
     :   , max_layer            ! size of array
     :   , '(mm)'               ! Units                (Not Used)
     :   , g%dlayer             ! Variable
     :   , numvals              ! Number of values returned
     :   , 0.0                  ! Lower Limit for bound checking
     :   , 2000.0)              ! Upper Limit for bound checking
 
      call get_real_array(
     :     unknown_module       ! Module that responds (Not Used)
     :   , 'bd'                 ! Variable Name
     :   , max_layer            ! size of array
     :   , '(kg/m^3)'           ! Units                (Not Used)
     :   , g%bd                 ! Variable
     :   , numvals              ! Number of values returned
     :   , 0.0                  ! Lower Limit for bound checking
     :   , 10.0)               ! Upper Limit for bound checking
 
      call pop_routine (my_name)
      return
      end



* ====================================================================
      subroutine erosion_set_my_variable (variable_name)
* ====================================================================
      use ErosionModule
      implicit none
      include 'intrface.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      character  variable_name*20      ! (INPUT)

*+  Purpose
*     Set the values of my variables from other modules

*+  Changes
*     DMS 25/02/94 (new template)
*     PdeV 27/08/94
*      011195 jngh  added call to message_unused
*      090696 jngh changed respond2set to collect

*+  Constant Values
      character my_name*(*)
      parameter (my_name = 'erosion_set_my_variable')

*+  Local Variables
      integer    numvals
      character*20 units               ! units of variable received

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (variable_name .eq. 'cover_extra') then
 
         call collect_real_var (
     :       variable_name      ! Name of Variable  (not used)
     :      , units             ! Units of variable (not used)
     :      , g%cover_extra     ! Variable
     :      , numvals
     :      , -1.0
     :      , 1.0)
 
      else
             ! nothing
         call Message_unused ()
      endif
 
      call pop_routine (my_name)
      return
      end



* ====================================================================
      subroutine erosion_set_other_variables ()
* ====================================================================
      use ErosionModule
      implicit none
      include   'const.inc'
      include 'action.inc'
      include 'data.pub'                          
      include 'intrface.pub'                      
      include 'error.pub'
      include 'postbox.pub'                         

*+  Purpose
*     Update variables owned by other modules.

*+  Changes
*     DMS 25/02/94 (New template)
*     300695 jngh changed number of values sent from max_layer to numvals
*     090696 nih  changed set calls to post_var constructs

*+  Constant Values
      character  my_name*(*)
      parameter (my_name = 'erosion_set_other_variables')

*+  Local Variables
      integer    num_layers

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
         ! no soil loss -> no profile change
      if ((g%soil_loss_bed + g%soil_loss_susp) .gt. 0.0 .and.
     :     p%profile_reduction .eq. on) then
 
         call new_postbox()
c$$$         call post_real_array (
c$$$     :          'dlayer'
c$$$     :        , '(mm)'
c$$$     :        , g%dlayer
c$$$     :        , max_layers)
 
         num_layers = count_of_real_vals (g%dlayer, max_layer)
 
         call post_real_array (
     :          'dlt_dlayer'
     :        , '(mm)'
     :        , g%dlt_dlayer
     :        , num_layers)      ! trailing 0s
 
         call Action_send (unknown_module
     :                               ,ACTION_set_variable
     :                               ,'dlt_dlayer')
         call delete_postbox()
 
      else
         ! nothing
      endif
 
      call pop_routine (my_name)
      return
      end



* ====================================================================
      subroutine erosion_send_my_variable (variable_name)
* ====================================================================
      use ErosionModule
      implicit none
      include   'convert.inc'          ! t2g, ha2sqcm, cm2mm
      include 'data.pub'                          
      include 'intrface.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      character  variable_name*(*)     ! (INPUT) variable name to search for

*+  Purpose
*     Return the value of one of our variables to caller

*+  Changes
*     DMS 25/02/94 (new template)
*     PdeV. 27/08/94
*      011195 jngh  added call to message_unused

*+  Constant Values
      character my_name*(*)
      parameter (my_name = 'erosion_send_my_variable')

*+  Local Variables
      real       soil_loss_tha         ! soil loss from surface (t/ha)
      real       soil_loss_mm          ! soil loss from surface (mm)
      real       sed_conc              ! sediment concentration (g/l)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      soil_loss_tha = g%soil_loss_bed + g%soil_loss_susp
 
      if (Variable_name .eq. 'soil_loss') then
         call respond2get_real_var (variable_name
     :                             , '(t/ha)'
     :                             , soil_loss_tha)
 
      else if (Variable_name .eq. 'soil_loss_bed') then
         call respond2get_real_var (variable_name
     :                             , '(t/ha)'
     :                             , g%soil_loss_bed)
 
      else if (Variable_name .eq. 'soil_loss_susp') then
         call respond2get_real_var (variable_name
     :                             , '(t/ha)'
     :                             , g%soil_loss_susp)
 
      else if (Variable_name .eq. 'soil_loss_mm') then
         soil_loss_mm = divide (soil_loss_tha * t2g/ha2scm
     :                        , g%bd(1), 0.0)
     :                * cm2mm
 
         call respond2get_real_var (variable_name
     :                             , '(mm)'
     :                             , soil_loss_mm)
 
      else if (Variable_name .eq. 'sed_conc') then
         sed_conc = divide (soil_loss_tha * t2g/ha2sm
     :                    , g%runoff * mm2lpsm, 0.0)
         call respond2get_real_var (variable_name
     :                             , '(g/l)'
     :                             , sed_conc)
 
      else if (Variable_name .eq. 'sed_conc_bed') then
         sed_conc = divide (g%soil_loss_bed * t2g/ha2sm
     :                    , g%runoff * mm2lpsm, 0.0)
         call respond2get_real_var (variable_name
     :                             , '(g/l)'
     :                             , sed_conc)
 
      else if (Variable_name .eq. 'sed_conc_susp') then
         sed_conc = divide (g%soil_loss_susp * t2g/ha2sm
     :                    , g%runoff * mm2lpsm, 0.0)
         call respond2get_real_var (variable_name
     :                             , '(g/l)'
     :                             , sed_conc)
 
      else if (Variable_name .eq. 'bed_depth') then
         call respond2get_real_var (variable_name
     :                             , '(mm)'
     :                             , g%bed_depth )
 
      else if (Variable_name .eq. 'erosion_cover') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%erosion_cover)
 
      else if (Variable_name .eq. 'cover_extra') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%cover_extra)
 
      else
            ! nothing
         call Message_unused ()
      endif
 
      call pop_routine (my_name)
      return
      end



* ====================================================================
      subroutine erosion_process ( )
* ====================================================================
      use ErosionModule
      implicit none
      include   'const.inc'       ! Constant definitions
      include 'error.pub'                         

*+  Purpose
*     Perform actions for current day.

*+  Changes
*     DMS 25/02/94 (new template)
*     PdeV. 28/08/94

*+  Constant Values
      character  my_name*(*)
      parameter (my_name = 'erosion_process')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      g%soil_loss_bed = 0.0
      g%soil_loss_susp = 0.0
 
      if (p%model_type .eq. freeb_model) then
         call erosion_freeb (g%soil_loss_bed, g%soil_loss_susp)
 
      else if (p%model_type .eq. rose_model) then
         call erosion_rose (g%soil_loss_bed, g%soil_loss_susp)
 
      else
      endif
 
      if ((g%soil_loss_bed + g%soil_loss_susp .gt. 0.0) .and.
     :    p%profile_reduction .eq. on) then
            ! move profile
         call erosion_move_profile ()
      else
         ! nothing
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine erosion_freeb (bed_loss, susp_loss)
*     ===========================================================
      use ErosionModule
      implicit none
      include   'convert.inc'          ! fract2pcnt
      include 'error.pub'                         

*+  Sub-Program Arguments
      real      bed_loss               ! (OUTPUT) soil loss in bed load (t/ha)
      real      susp_loss              ! (OUTPUT) soil loss in suspended load (t/ha)

*+  Purpose
*     Freebairn cover-sediment concentration model
*     from PERFECT. returns t/ha bed and suspended loss

*+  Changes
*     PdeV. 28/08/94

*+  Constant Values
      character  my_name*(*)
      parameter (my_name = 'erosion_freeb')

*+  Local Variables
      real       erosion_cover_pcnt    ! erosion cover percent
      real       sed_conc              ! sediment concentration (%)
                                       ! ie. g soil/g water *100

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      erosion_cover_pcnt = g%erosion_cover * fract2pcnt
      if (g%erosion_cover.lt.0.5)then
         sed_conc = 16.52
     :            - 0.46 * erosion_cover_pcnt
     :            + 0.0031 * erosion_cover_pcnt * erosion_cover_pcnt
 
      else
         sed_conc = 2.54 - 0.0254 * erosion_cover_pcnt
 
      endif
 
      bed_loss = sed_conc * pcnt2fract * g2t/(g2mm * sm2ha)
     :              * p%ls_factor * p%k_factor_bed
     :              * p%p_factor * g%runoff
 
      susp_loss = sed_conc * pcnt2fract * g2t/(g2mm * sm2ha)
     :              * p%ls_factor * p%k_factor_susp
     :              * p%p_factor * g%runoff
 
cjh      erosion_freeb = sed_conc
cjh     :              * p%ls_factor * p%k_factor
cjh     :              * p%p_factor * g%runoff  / 10.0
cjh       (100*g/(1000*1000))/(g*1000/1000000) *mm  -> t/ha
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine erosion_rose (bed_loss, susp_loss)
*     ===========================================================
      use ErosionModule
      implicit none
      include   'convert.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      real      bed_loss               ! (OUTPUT) soil loss in bed load (t/ha)
      real      susp_loss              ! (OUTPUT) soil loss in suspended load (t/ha)

*+  Purpose
*     Simplified rose model from PERFECT
*     returns t/ha bed and suspended loads

*+  Notes
*******************************************************************
*                                                                 *
*  This subroutine calculates soil loss using the simplified Rose *
*  algorithm.                                                     *
*     apsim         perfect   descr                               *
*     ---           ---       ------------                        *
*     total_cover - covm   -  mulch cover     ( 0 - 1)            *
*     entrain_eff - kusle  -  efficiency of entrainment (bare conditions)*
*     runoff      - runf   -  event runoff (mm)                   *
*     (returned)  - sed    -  soil loss (t/ha)                    *
*     slope       - aslope -  slope (%)                           *
*                                                                 *
*******************************************************************

*+  Changes
*     PdeV. 28/08/94

*+  Constant Values
      character  my_name*(*)
      parameter (my_name = 'erosion_rose')

*+  Local Variables
      real       lambda_bed                ! efficiency of entrainment ?
      real       lambda_susp                ! efficiency of entrainment ?

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      lambda_bed = p%entrain_eff_bed
     :       * exp (- p%eros_rose_b2_bed * g%erosion_cover * fract2pcnt)
 
      bed_loss = 2700.0 * (p%slope * pcnt2fract)
     :             * (1.0 - g%erosion_cover)
     :             * lambda_bed * g%runoff / 100.0
 
 
      lambda_susp = p%entrain_eff_susp
     :       * exp (- p%eros_rose_b2_susp *
     :               g%erosion_cover * fract2pcnt)
 
      susp_loss = 2700.0 * (p%slope * pcnt2fract)
     :             * (1.0 - g%erosion_cover)
     :             * lambda_susp * g%runoff / 100.0
 
c      erosion_rose = 2700.0 * (p%slope * pcnt2fract)
c     :             * (1.0 - g%erosion_cover)
c     :             * lambda * g%runoff / 100.0
cjh           what is the unit conversion here???
 
      call pop_routine (my_name)
      return
      end



*     ================================================================
      subroutine erosion_move_profile ()
*     ================================================================
      use ErosionModule
      implicit none
      include 'data.pub'                          
      include 'error.pub'                         

*+  Purpose
*     move things in the profile

*+  Notes
*     N (ie kg/ha) variables move from top down.
*     profile is eroded from the bottom up.

*+  Changes
*     DMS 25/02/94 (new template)
*     PdeV. 28/08/94

*+  Constant Values
      character  my_name*(*)
      parameter (my_name = 'erosion_move_profile')

*+  Local Variables
      integer    num_layers
      real       dlt_bed_depth

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      call erosion_move_dlayr (g%dlt_dlayer, dlt_bed_depth)
 
c      write (*,*) 'xxx',g%dlt_dlayer, dlt_bed_depth
 
      num_layers = count_of_real_vals (g%dlayer, max_layer)
 
         ! was that too much?
      if (sum_real_array (g%dlayer, num_layers)
     :     + sum_real_array (g%dlt_dlayer, num_layers)
     :     .lt.  p%minimum_depth) then
 
         call erosion_bomb_run ()
      else
         ! nothing
      endif
         ! update depth to bedrock
      g%bed_depth = g%bed_depth + dlt_bed_depth
 
      call pop_routine (my_name)
      return
      end



*     ================================================================
      subroutine erosion_bomb_run ()
*     ================================================================
      use ErosionModule
      implicit none
      include   'const.inc'            ! Constant definitions
      include 'error.pub'                         

*+  Purpose
*      kill the run

*+  Changes
*     DMS 25/02/94 (new template)
*     PdeV. 28/08/94

*+  Constant Values
      character  my_name*(*)
      parameter (my_name = 'erosion_bomb_run')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      call fatal_error (err_user, 'Out of soil to erode. Giving up.')
 
      call pop_routine (my_name)
      return
      end



*     ================================================================
      subroutine erosion_move_dlayr (dlt_dlayer, dlt_bed_depth)
*     ================================================================
      use ErosionModule
      implicit none
      include   'const.inc'            ! Constant definitions
      include   'convert.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dlt_dlayer(*)         ! (OUTPUT)
      real       dlt_bed_depth         ! (OUTPUT)

*+  Purpose
*     move dlayr

*+  Notes
*     Erodes profile from bottom up.

*+  Changes
*     DMS 25/02/94 (new template)
*     PdeV. 28/08/94
*     JNGH 24/11/99 Changed test of too much erosion to top and bottom layer.

*+  Constant Values
      character  my_name*(*)
      parameter (my_name = 'erosion_move_dlayr')

*+  Local Variables
      real       tot_depth
      real       overrun
      real       new_depth
      real       top                   ! temporary
      integer    num_layers
      integer    i
      real       dlt_depth_mm ! bd based change in depth
      character  string*200               ! message string

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      call fill_real_array (dlt_dlayer, 0.0, max_layer)
      dlt_bed_depth = 0.0
 
      num_layers = count_of_real_vals (g%dlayer, max_layer)
 
         ! find density based change in each layer
 

      top = (g%soil_loss_bed + g%soil_loss_susp) * t2g/ha2scm
      dlt_depth_mm =  divide (top, g%bd(1), 0.0) * cm2mm

 
c     What happens when layer completely eroded?
      if (dlt_depth_mm .gt. g%dlayer(1) ) then
         write (string, '(a)')
     :            'Eroding more than top layer depth.'
     :          // ' This may affect SoilN loss.'
         call warning_error (err_user, string)
      else
         ! nothing
      endif
         
      dlt_depth_mm =  divide (top, g%bd(num_layers), 0.0) * cm2mm
 
c     What happens when layer completely eroded?
       if (dlt_depth_mm .gt. g%dlayer(num_layers) ) then
          write (string, '(a, i3, a)')
     :            'Eroding more than bottom layer depth. (layer '
     :          , num_layers
     :          , ').' // New_line
     :          // 'PAWC calculations may be incorrect if BD '
     :         // 'is different to layer above.'
 
         call warning_error (err_user, string)
      else
         ! nothing
      endif

 
 
         ! Check whether we've moved bedrock
         ! into the profile. If so, we have to change dlayer.
      tot_depth = sum_real_array(g%dlayer, num_layers)
     :          + dlt_depth_mm
 
      if (tot_depth .gt. g%bed_depth ) then
         overrun = tot_depth - g%bed_depth
         dlt_bed_depth = - overrun
 
         do 2000 i = num_layers, 1, -1
            if (overrun .gt. 0.0) then
                  ! yes - eroded into bedrock.
               if (overrun .le. g%dlayer(i)) then
                     ! move portion of layer
                  dlt_dlayer(i) = - overrun
                     ! find if layers merge
                  new_depth = dlt_dlayer(i) + g%dlayer(i)
                  if (new_depth .lt. p%layer_merge_mm) then
                     if (i .le. 1) then
                        call erosion_bomb_run ()
                     else
                        dlt_dlayer(i-1) = new_depth
                        dlt_dlayer(i) = - g%dlayer(i)
                        p%layer_merge_mm = g%dlayer(i - 1)
     :                                   * p%profile_layer_merge
                     endif
                  else
                     ! nothing
                  endif
                  overrun = 0.0
               else
                     ! remove entire layer
                  dlt_dlayer(i) = - g%dlayer(i)
                  overrun = overrun - g%dlayer(i)
               endif
            else
               ! nothing
            endif
 2000    continue
      endif
 
      call pop_routine (my_name)
      return
      end



*     ================================================================
      subroutine erosion_end_run ()
*     ================================================================
      use ErosionModule
      implicit none
      include 'error.pub'                         

*+  Purpose
*      Perform cleanup because the current simulation is about to end.

*+  Notes
*      closes log file if necessary

*+  Changes
*     DMS 25/02/94 (new template)
*     PdeV. 28/08/94

*+  Constant Values
      character  my_name*(*)
      parameter (my_name = 'erosion_end_run')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      call pop_routine (my_name)
      return
      end


