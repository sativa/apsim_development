
!  This is an instantiating version of millet.  It is used to allow multiple
!  instantiations of this module.  One for the main stem and one for each
!  of the 5 tillers.  It that uses some features of FORTRAN 90 as
!  opposed to only FORTRAN 77 features.
!
!  Changes include:-
!
!  *  This APSIM module has been made into a FORTRAN 90 module,
!     "millet_m".  Named constants and variables can have global scope,
!     and common blocks are therefore no longer needed.  The
!     terminating "end" statement that was used to terminate FORTRAN 77
!     subprograms has now been replaced with "end" or "end
!     function" as appropriate.
!
!  *  Some FORTRAN 90 types have been defined:-
!     *  constant_t for holding all the constants (ini file parameters held
!        in FORTRAN variables).  All the c_??? common block
!        variables that used to exist have now been replaced by a member in
!        this type and their names will not include the leading two characters
!        "c_".
!
!     *  parameter_t for holding all the other parameters.  All the
!        p_??? common block variables that used to exist have
!        now been replaced by a member in this type and their names
!        will not include the leading two characters "p_".
!
!     *  global_t for holding all the not local to a
!        subprogram.  All the g_??? variables common block variables
!        that used to exist have now been replaced by a member in this
!        type and their names will not include the leading two
!        characters "g_".
!
!     *  millet_t, which contains (pointers to) constant_t, parameter_t and
!        global_t.
!
!  *  There are only three global variables now:-
!     *  c, of type constant_t.  (pointer to)
!     *  p, of type parameter_t.  (pointer to)
!     *  g, of type global_t.  (pointer to)
!
!  *  In the subprograms, all references to common block variables have
!     been replaced with references to the corresponding members of c, p,
!     or g, i.e. a global search and replace has been performed so that all
!     "c_???" is now c%???, all "p_???" is now "p%???" and all "g_???" is
!     now "g%???".
!
!  *  The variables 'c', 'g' and 'p' are really only
!     pointers to constant_t, parameter_t and global_t, not the actual
!     variables.  When this module is being the main stem, g, c, and p
!     point to the actual variables for the main stem.  When this module is
!     being a tiller, g, c, and p point to the actual variables for that
!     tiller.
!
!  *  The only public routines that this module has are millet(),
!     millet_alloc() and millet_free():-
!
!     *  millet() is the normal entry point for the APSIM millet
!        module.  It first argument, "this" of type millet_t, is new.
!        It holds pointers to the actual variables for this module instance.
!
!        The first thing that millet() does is make assignments to c, p and
!        g, so that these global variables now point to the actual
!        variables for this module instance.
!
!        APSIM modules are recursive.  They may be called upon to give the
!        values of variables even when it is doing is days processing.
!        Apsim modules that have multiple instantiations are a bit more
!        recursive than usual.  The main stem may be providing a variable
!        value for a tiller, for example.  In this environment, global
!        variables are a bad idea in general.
!
!        The last thing that millet() does is restore its three global
!        variables to their former values so that if this there are other
!        calls of millet active, their variables will be restored when this
!        call of millet() returns.
!
!     *  millet_alloc() is called for each module instantiation of millet
!        before using it.  It allocates the actual variables for the
!        module.  It allocates its only parameter "this" and then
!        allocated the three mebers of "this", "c", P", and "g".
!
!     *  millet_free() is called for each module instantiation of millet
!        after using it in order to free the memory allocated by
!        millet_alloc().  It frees the three members of its only argument,
!        "this", and then frees "this".
!
!  *  This APSIM millet module is encapsulated in a dll.  In millet_i.for
!     there are the three dll exported routines, APSIM_millet(),
!     APSIM_millet_alloc() and APSIM_millet_free().  These are merely
!     pipelines that do nothing but pass their arguments onto routines that
!     may not be dll exported, millet(), millet_alloc() and millet_free().

      include 'Millet.inc'

!     ===========================================================
      Recursive
     :subroutine AllocInstance (InstanceName, InstanceNo)
!     ===========================================================
      use MilletModule
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
      Recursive
     :subroutine FreeInstance (anInstanceNo)
!     ===========================================================
      use MilletModule
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
      Recursive
     :subroutine SwapInstance (anInstanceNo)
!     ===========================================================
      use MilletModule
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


*     ================================================================
      Recursive
     :subroutine Main (action, data_string)
*     ================================================================
      use MilletModule
      implicit none
      include 'const.inc'
      include   'action.inc'
      include   'event.inc'
      include 'string.pub'
      include 'error.pub'

*+  Sub-Program Arguments
      character  action*(*)            ! (INPUT) Message action to perform
      character  data_string*(*)       ! (INPUT) Message data

*+  Purpose
*      This module performs crop crop growth
*       simulates root, leaf, head, stem and grain development. Water and
*       nitrogen uptake, photosynhesis, and leaf and root senescense.

*+  Changes
*      250894 jngh specified and programmed
*      220696 jngh added message_unused to else
*      190599 jngh removed reference to version and mes_presence

*+  Calls
      logical millet_my_type

*+  Constant Values
      character  my_name*(*)           ! name of this procedure
      parameter (my_name='Millet_main')

*+  Local Variables
      character  module_name*8         ! module name


*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call get_current_module (module_name)

!        call write_string('millet('// module_name // ' '
!       :         // g%last_mdl_name //' ' //g%stem_class//action)
!        g%last_mdl_name = module_name

      if (action.eq.ACTION_get_variable) then
            ! respond to request for variable values - from modules
         call millet_send_my_variable (Data_string)

      elseif (action.eq.ACTION_set_variable) then
            ! respond to request to reset variable values - from modules
         call millet_set_my_variable (data_string)

      else if (Action .eq. Event_tick) then
         call millet_Ontick()

      else if (Action .eq. Event_newmet) then
         call millet_Onnewmet()

      elseif (action.eq.ACTION_prepare) then
         if (g%plant_status.ne.status_out) then
            call millet_zero_daily_variables ()
!               ! request and receive variables from owner-modules
            call millet_get_other_variables ()
!               ! do crop prepare processes
            call millet_prepare ()
         else
!            ! crop not in
            call millet_zero_variables ()
!               ! set class
            call millet_set_my_class (module_name)
         endif

      elseif (action.eq.ACTION_process) then
         if (g%plant_status.ne.status_out) then
!            call millet_zero_daily_variables ()
               ! request and receive variables from owner-modules
            call millet_get_other_variables ()
               ! do crop processes
            call millet_process ()
               ! send changes to owner-modules
            call millet_set_other_variables ()
         else
            ! crop not in
         endif

      elseif (action.eq.ACTION_init) then
            ! zero pools
         call millet_zero_variables ()
            ! Get constants
         call millet_init ()
            ! set class
         call millet_set_my_class (module_name)
            ! request and receive variables from owner-modules
         call millet_get_other_parameters ()
         call millet_get_other_variables ()

      elseif (action.eq.ACTION_sow) then

         if (millet_my_type ()) then

            if (g%stem_class .eq. class_tiller) then
               call fatal_error (err_user,
     :                      'Cannot sow initiated tiller!')
            else

               ! request and receive variables from owner-modules
               call millet_get_other_variables ()
               ! start crop and do  more initialisations
               call millet_start_crop ()
            endif
         else
            ! not my type!
            call message_unused ()
         endif

      elseif (action.eq.ACTION_initiate_crop) then

         if (millet_my_type ()) then

            if (g%stem_class .eq. class_main) then
               call fatal_error (err_user,
     :                      'Cannot initiate main tiller!')
            else
               ! request and receive variables from owner-modules
               call millet_get_other_variables ()
               ! start crop and do  more initialisations
               call millet_initiate ()
            endif

         else
            ! not my type!
            call message_unused ()
         endif

      elseif (action.eq.ACTION_harvest) then
         if (millet_my_type ()) then
               ! harvest crop - turn into residue
              call millet_harvest ()
         else
            ! not my type!
            call message_unused ()
         endif

      elseif (action.eq.ACTION_end_crop) then
         if (millet_my_type ()) then
               ! end crop - turn into residue
            call millet_end_crop ()
         else
            ! not my type!
            call message_unused ()
         endif

      elseif (action.eq.ACTION_kill_crop) then
         if (millet_my_type ()) then
               ! kill crop - die
            call millet_kill_crop (
     :          g%plant_status,
     :          g%dm_green,
     :          g%dm_senesced,
     :          g%dm_dead)
         else
            ! not my type!
            call message_unused ()
         endif

      elseif (Action.eq.ACTION_Create) then
         call Millet_zero_all_globals ()

cjh special for erik - start
      elseif (action.eq.'stop_growth') then
         if (millet_my_type ()) then
               ! stop crop growth and development
            call millet_stop_growth (.true.)
         else
            ! not my type!
            call message_unused ()
         endif
cjh special for erik - end

      else
               ! don't use message
         call message_unused ()
      endif

      call pop_routine (my_name)
      return
      end

* ====================================================================
      Recursive
     :subroutine Millet_prepare ()
* ====================================================================
      use MilletModule
      implicit none
      include 'error.pub'

*+  Purpose
*     <insert here>

*+  Changes
*     12-05-1997 - huth - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'Millet_prepare')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call Millet_nit_stress(1)

      call Millet_p_conc(1)
      call Millet_phos_init(1)
      call Millet_p_stress_photo(1)
      call Millet_p_stress_pheno(1)
      call Millet_p_stress_expansion(1)
      call Millet_p_stress_grain(1)

      call Millet_temp_stress(1)

      call Millet_light_supply(1)
      call Millet_bio_RUE(1)
      call Millet_transpiration_eff(1)
      call Millet_water_demand(1)
      call Millet_Nit_demand_est(1)
      call Millet_P_demand_est(1)

      call pop_routine (myname)
      return
      end





*     ===========================================================
      Recursive
     :subroutine millet_process ()
*     ===========================================================
      use MilletModule
      implicit none
      include 'science.pub'
      include 'error.pub'

*+  Purpose
*       Simulate crop processes.  These include biomass production,
*       phenological stages, plant component development,
*       water uptake and nitrogen uptake, and plant senescense.

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_process')

*- Implementation Section ----------------------------------
c+!!!!!!!!! check order dependency of deltas
      call push_routine (my_name)

      call millet_root_depth(1)
      call millet_root_depth_init(1)! option 1 initial root depth = c%...
                                    ! option 2 initial root depth = sowing depth
      call millet_water_supply(1)
      call millet_water_stress(1)

      call Millet_phenology_init(101)
      call Millet_phenology(101)

      if (g%plant_status.eq.status_alive) then

         call millet_water_uptake(1)
         call millet_height(101)

         call Millet_leaf_area_init(1)
         call Millet_leaf_no_init(1)
         call Millet_leaf_no_pot(101)
         call Millet_leaf_area_potential(101) !  bell shape
         call Millet_leaf_area_stressed(101)

               ! drymatter production
         call Millet_bio_TE(1)
         call Millet_bio_RUE(1)
         call Millet_bio_actual(101)

               ! note: The dm partition and retranslocate subroutines
               ! implicitly account for both grain no and harvest index
               ! approaches in calculating delta grain.

         call Millet_bio_grain_demand(1)
!jh         call Millet_bio_partition(101)
         call Millet_bio_partition(102)
         call Millet_bio_retrans(101)

         call Millet_leaf_actual(101)

         call millet_tillering ()

         call Millet_leaf_death(101)
         call Millet_leaf_area_sen(1)
         call Millet_sen_bio(1)
         call Millet_sen_nit(101)

         call Millet_nit_supply(1)
         call Millet_nit_init(1)
         call Millet_nit_retrans(1)
         call Millet_nit_demand(1)
         call Millet_nit_uptake(1)
         call Millet_nit_partition(1)

         call Millet_p_uptake(1)

         call Millet_plant_death(1) ! 1 = barreness using cubic (+bug)
                                   ! 2 = linear barreness effect

      else
         ! crop is dead
      endif

      if (g%plant_status.eq.status_dead) then
            ! crop is dead
!*************************************************
         call millet_dead ()

      else
            ! crop is alive
      endif

      call Millet_detachment(1)
      call Millet_cleanup()

      call pop_routine (my_name)
      return
      end



*     ===========================================================
      Recursive
     :subroutine millet_dead ()
*     ===========================================================
      use MilletModule
      implicit none
      include 'error.pub'

*+  Purpose
*       Set up states for dead crop

*+  Changes
*      091095 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_dead')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      g%current_stage   = real (plant_end)

      call pop_routine (my_name)
      return
      end



*     ===========================================================
      Recursive
     :subroutine millet_harvest ()
*     ===========================================================
      use MilletModule
      implicit none
      include 'const.inc'
      include   'convert.inc'          ! gm2kg, sm2ha, sm2smm
      include 'data.pub'
      include 'error.pub'

*+  Purpose
*       Report occurence of harvest and the current status of specific
*       variables.

*+  Changes
*     010994 jngh specified and programmed

*+  Calls

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_harvest')

*+  Local Variables
      real       biomass_dead          ! above ground dead plant wt (kg/ha)
      real       biomass_green         ! above ground green plant wt (kg/ha)
      real       biomass_senesced      ! above ground senesced plant wt (kg/ha)
      real       dm                    ! above ground total dry matter (kg/ha)
      real       grain_wt              ! grain dry weight (g/kernel)
      real       head_grain_no         ! final grains /head
      real       leaf_no               ! total leaf number
      real       N_grain               ! total grain N uptake (kg/ha)
      real       N_dead                ! above ground dead plant N (kg/ha)
      real       N_green               ! above ground green plant N (kg/ha)
      real       N_senesced            ! above ground senesced plant N (kg/ha)
      real       N_stover              ! nitrogen content of stover (kg\ha)
      real       N_total               ! total gross nitrogen content (kg/ha)
      real       N_grain_conc_percent  ! grain nitrogen %
      integer    phase                 ! phenological phase number
      real       si1                   ! mean water stress type 1
      real       si2                   ! mean water stress type 2
      real       si4                   ! mean nitrogen stress type 1
      real       si5                   ! mean nitrogen stress type 2
      real       stover                ! above ground dry weight less grain
                                       ! (kg/ha)
      character  string*200            ! message
      real       yield                 ! grain yield dry wt (kg/ha)
      real       yield_wet             ! grain yield including moisture
                                       ! (kg/ha)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

          ! crop harvested. Report status

      yield = (g%dm_green(grain) + g%dm_dead(grain))
     :      * gm2kg / sm2ha

          ! include the grain water content
      yield_wet = yield / (1.0 - c%grn_water_cont)

      grain_wt = divide (g%dm_green(grain) + g%dm_dead(grain)
     :                 , g%grain_no, 0.0)
!cpsc
      head_grain_no = divide (g%grain_no, g%plants, 0.0)

      biomass_green = (sum_real_array (g%dm_green, max_part)
     :              - g%dm_green(root))
     :              * gm2kg / sm2ha

      biomass_senesced = (sum_real_array (g%dm_senesced, max_part)
     :                 - g%dm_senesced(root))
     :                 * gm2kg / sm2ha

      biomass_dead = (sum_real_array (g%dm_dead, max_part)
     :             - g%dm_dead(root))
     :             * gm2kg / sm2ha

      dm = (biomass_green + biomass_senesced + biomass_dead)

      stover = dm - yield

      g%leaf_no_total = sum_between (emerg, harvest_ripe, g%leaf_no)
      leaf_no = sum_between (emerg, harvest_ripe, g%leaf_no)
cejvo      leaf_no = sum_between (germ, harvest_ripe, g%leaf_no)
      N_grain_conc_percent = divide (g%N_green(grain) + g%N_dead(grain)
     :                            , g%dm_green(grain) + g%dm_dead(grain)
     :                            , 0.0)
     :                     * fract2pcnt

      N_grain = (g%N_green(grain) + g%N_dead(grain))
     :        * gm2kg/sm2ha

      N_green = (sum_real_array (g%N_green, max_part)
     :        - g%N_green(root) - g%N_green(grain))
     :        * gm2kg / sm2ha

      N_senesced = (sum_real_array (g%N_senesced, max_part)
     :           - g%N_senesced(root) - g%N_senesced(grain))
     :           * gm2kg / sm2ha

      N_dead = (sum_real_array (g%N_dead, max_part)
     :       - g%N_dead(root) - g%N_dead(grain))
     :       * gm2kg / sm2ha

      N_stover = N_green + N_senesced + N_dead
      N_total = N_grain + N_stover


      call write_string (new_line//new_line)

      write (string, '(a,i4,t40,a,f10.1)')
     :            ' flowering day  = ',g%isdate
     :          , ' stover (kg/ha) =',stover
      call write_string (string)

      write (string, '(a,i4,t40,a,f10.1)')
     :            ' maturity day        = ', g%mdate
     :          , ' grain yield (kg/ha) =', yield
      call write_string (string)

      write (string, '(a,f6.1,t40,a,f10.1)')
     :            ' grain % water content   = ', c%grn_water_cont
     :                                         * fract2pcnt
     :          , ' grain yield wet (kg/ha) =', yield_wet
      call write_string (string)

      write (string, '(a,f10.3,t40,a,f10.3)')
     :            ' grain wt (g) =', grain_wt
     :          , ' grains/m^2   =', g%grain_no
      call write_string (string)

      write (string, '(a,f6.1,t40,a,f6.3)')
     :            ' grains/head =', head_grain_no
     :          , ' maximum lai =', g%lai_max
      call write_string (string)

      write (string, '(a,f10.1)')
     :            ' total above ground biomass (kg/ha) =', dm
      call write_string (string)

      write (string, '(a,f10.1)')
     :         ' live above ground biomass (kg/ha) =', biomass_green
     :                                               + biomass_senesced
      call write_string (string)

      write (string, '(a,f10.1)')
     :            ' green above ground biomass (kg/ha) =', biomass_green
      call write_string (string)

      write (string, '(a,f10.1)')
     :      ' senesced above ground biomass (kg/ha) =', biomass_senesced
      call write_string (string)

      write (string, '(a,f10.1)')
     :            ' dead above ground biomass (kg/ha) =', biomass_dead
      call write_string (string)

      write (string, '(a,f6.1)')
     :            ' number of leaves =', leaf_no
      call write_string (string)

      write (string, '(a,f10.2,t40,a,f10.2)')
     :            ' grain N percent =', N_grain_conc_percent
     :          , ' total N content (kg/ha) =', N_total
      call write_string (string)

      write (string, '(a,f10.2,t40,a,f10.2)')
     :            ' grain N uptake (kg/ha) =', N_grain
     :          , ' senesced N content (kg/ha) =', N_senesced

      call write_string (string)

      write (string, '(a,f10.2,t40,a,f10.2)')
     :            ' green N content (kg/ha) =', N_green
     :          , ' dead N content (kg/ha) =', N_dead
      call write_string (string)

      do 2000 phase = emerg_to_endjuv, start_to_end_grain
         si1 = divide (g%cswd_photo(phase)
     :               , g%days_tot(phase), 0.0)
         si2 = divide (g%cswd_expansion(phase)
     :               , g%days_tot(phase), 0.0)
         si4 = divide (g%cnd_photo(phase)
     :               , g%days_tot(phase), 0.0)
         si5 = divide (g%cnd_grain_conc(phase)
     :               , g%days_tot(phase), 0.0)

         call write_string (new_line//new_line)

         write (string,'(2a)')
     :         ' stress indices for ', c%stage_names(phase)
         call write_string (string)

         write (string,'(2(a, f16.7))')
     :         ' water stress 1 =', si1
     :         , '   nitrogen stress 1 =', si4
         call write_string (string)

         write (string,'(2(a, f16.7))')
     :         ' water stress 2 =', si2
     :         , '   nitrogen stress 2 =', si5
         call write_string (string)
2000  continue

      g%dm_green(grain) = 0.0
      g%N_green(grain) = 0.0

      g%dm_dead(grain) = 0.0
      g%N_dead(grain) = 0.0

      call pop_routine (my_name)
      return
      end



*     ===========================================================
      Recursive
     :subroutine millet_zero_all_globals ()
*     ===========================================================
      use milletModule
      implicit none
      include 'error.pub'

*+  Purpose
*       Zero all global variables & arrays

*+  Changes
*     060495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'millet_zero_all_globals')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

         c%a_const                   = 0.0
         c%a_slope1                  = 0.0
         c%a_slope2                  = 0.0
         c%amax                      = 0.0
         c%amin                      = 0.0
         c%aopt                      = 0.0
         c%aoptr                     = 0.0
         c%b_const                   = 0.0
         c%b_slope1                  = 0.0
         c%b_slope2                  = 0.0
         c%barren_crit               = 0.0
         c%crop_type                 = ' '
         c%days_germ_limit           = 0.0
         c%dead_detach_frac(:)       = 0.0
         c%dlayer_lb                 = 0.0
         c%dlayer_ub                 = 0.0
         c%dm_leaf_detach_frac       = 0.0
         c%dm_leaf_init              = 0.0
         c%dm_leaf_sen_frac          = 0.0
         c%dm_root_init              = 0.0
         c%dm_root_sen_frac          = 0.0
         c%dm_stem_init              = 0.0
         c%dm_tiller_crit            = 0.0
         c%dul_dep_lb                = 0.0
         c%dul_dep_ub                = 0.0
         c%extinction_coef           = 0.0
         c%extinction_coef_change    = 0.0
         c%extinction_coef_dead      = 0.0
         c%frac_flower2grain         = 0.0
         c%frac_leaf_post_flower     = 0.0
         c%frac_leaf_pre_flower      = 0.0
         c%frac_dm_to_leaf(:)        = 0.0
         c%frac_stem2flower          = 0.0
         c%frost_kill                = 0.0
         c%grain_gth_rate_ub         = 0.0
         c%grain_N_conc_min          = 0.0
         c%grn_water_cont            = 0.0
         c%growth_rate_crit          = 0.0
         c%growth_rate_min           = 0.0
         c%head_grain_no_crit        = 0.0
         c%head_grain_no_max_ub      = 0.0
         c%height_max                = 0.0
         c%height_stem_slope         = 0.0
         c%hi_min                    = 0.0
         c%htstress_coeff            = 0.0
         c%imax                      = 0.0
         c%imin                      = 0.0
         c%initial_root_depth        = 0.0
         c%initial_tpla              = 0.0
         c%iopt                      = 0.0
         c%ioptr                     = 0.0
         c%kl_ub                     = 0.0
         c%lai_sen_light             = 0.0
         c%latitude_lb               = 0.0
         c%latitude_ub               = 0.0
         c%leaf_app_rate1            = 0.0
         c%leaf_app_rate2            = 0.0
         c%leaf_init_rate            = 0.0
         c%leaf_no_at_emerg          = 0.0
         c%leaf_no_correction        = 0.0
         c%leaf_no_crit              = 0.0
         c%leaf_no_dead_const        = 0.0
         c%leaf_no_dead_slope        = 0.0
         c%leaf_no_dead_slope1       = 0.0
         c%leaf_no_dead_slope2       = 0.0
         c%leaf_no_diff              = 0.0
         c%leaf_no_max               = 0.0
         c%leaf_no_min               = 0.0
         c%leaf_no_rate_change       = 0.0
         c%leaf_no_seed              = 0.0
         c%leaf_size_average         = 0.0
         c%leaf_size_endjuv          = 0.0
         c%leaf_trans_frac           = 0.0
         c%ll_ub                     = 0.0
         c%main_stem_coef            = 0.0
         c%maxt_lb                   = 0.0
         c%maxt_ub                   = 60.0
         c%minsw                     = 0.0
         c%mint_lb                   = -10.0
         c%mint_ub                   = 50.0
         c%N_conc_crit_grain         = 0.0
         c%N_conc_crit_root          = 0.0
         c%N_conc_max_grain          = 0.0
         c%N_conc_max_root           = 0.0
         c%N_conc_min_grain          = 0.0
         c%N_conc_min_root           = 0.0
         c%N_fact_expansion          = 0.0
         c%N_fact_pheno              = 0.0
         c%N_fact_photo              = 0.0
         c%N_fix_rate                = 0.0
         c%N_leaf_init_conc          = 0.0
         c%N_leaf_sen_conc           = 0.0
         c%N_root_init_conc          = 0.0
         c%N_root_sen_conc           = 0.0
         c%N_stem_init_conc          = 0.0
         c%NO3_diffn_const           = 0.0
         c%NO3_lb                    = 0.0
         c%NO3_min_lb                = 0.0
         c%NO3_min_ub                = 0.0
         c%NO3_ub                    = 0.0
         c%num_ave_temp              = 0
         c%num_factors               = 0
         c%num_lai                   = 0
         c%num_lai_ratio             = 0
         c%num_N_conc_stage          = 0
         c%num_row_spacing           = 0
         c%num_sw_avail_fix          = 0
         c%num_sw_avail_ratio        = 0
         c%num_sw_demand_ratio       = 0
         c%num_sw_ratio              = 0
         c%num_temp                  = 0
         c%num_temp_grain            = 0
         c%num_temp_other            = 0
         c%num_temp_senescence       = 0
         c%num_tiller_no_next        = 0
         c%num_weighted_temp         = 0
         c%partition_rate_leaf       = 0.0
         c%pesw_germ                 = 0.0
         c%photo_tiller_crit         = 0.0
         c%photoperiod_base          = 0.0
         c%photoperiod_crit          = 0.0
         c%pp_endjuv_to_init_ub      = 0.0
         c%radn_lb                   = 0.0
         c%radn_ub                   = 60.0
         c%ratio_root_shoot(:)       = 0.0
         c%root_depth_rate(:)        = 0.0
         c%root_extinction           = 0.0
         c%row_spacing_default       = 0.0
         c%rue(:)                    = 0.0
         c%seed_wt_min               = 0.0
         c%sen_detach_frac(:)        = 0.0
         c%sen_light_slope           = 0.0
         c%sen_light_time_const      = 0.0
         c%sen_radn_crit             = 0.0
         c%sen_rate_water            = 0.0
         c%sen_threshold             = 0.0
         c%sen_water_time_const      = 0.0
         c%sfac_slope                = 0.0
         c%shoot_lag                 = 0.0
         c%shoot_rate                = 0.0
         c%sla_max                   = 0.0
         c%sla_min                   = 0.0
         c%spla_slope                = 0.0
         c%stage_code_list(:)        = 0.0
         c%stage_names(:)            = ' '
         c%stem_trans_frac           = 0.0
         c%svp_fract                 = 0.0
         c%sw_dep_lb                 = 0.0
         c%sw_dep_ub                 = 0.0
         c%sw_fac_max                = 0.0
         c%swdf_grain_min            = 0.0
         c%swdf_pheno_limit          = 0.0
         c%swdf_photo_limit          = 0.0
         c%swdf_photo_rate           = 0.0
         c%temp_fac_min              = 0.0
         c%temp_grain_crit_stress    = 0.0
         c%tfac_slope                = 0.0
         c%tiller_appearance         = ' '
         c%tiller_appearance_slope   = 0.0
         c%tiller_coef               = 0.0
         c%tiller_no_pot             = 0
         c%tpla_min                  = 0.0
         c%transp_eff_cf(:)          = 0.0
         c%tt_emerg_limit            = 0.0
         c%tt_emerg_to_endjuv_ub     = 0.0
         c%tt_flag_to_flower_ub      = 0.0
         c%tt_flower_to_maturity_ub  = 0.0
         c%tt_flower_to_start_grain_ub = 0.0
         c%tt_maturity_to_ripe_ub    = 0.0
         c%twilight                  = 0.0
         c%x0_const                  = 0.0
         c%x0_slope                  = 0.0
         c%x_ave_temp(:)             = 0.0
         c%x_lai(:)                  = 0.0
         c%x_lai_ratio(:)            = 0.0
         c%x_row_spacing(:)          = 0.0
         c%x_stage_code(:)           = 0.0
         c%x_sw_avail_fix (:)        = 0.0
         c%x_sw_avail_ratio (:)      = 0.0
         c%x_sw_demand_ratio (:)     = 0.0
         c%x_sw_ratio (:)            = 0.0
         c%x_temp(:)                 = 0.0
         c%x_temp_grain(:)           = 0.0
         c%x_temp_other(:)           = 0.0
         c%x_temp_senescence(:)      = 0.0
         c%x_tiller_no_next(:)       = 0.0
         c%x_weighted_temp(:)        = 0.0
         c%y_extinct_coef(:)         = 0.0
         c%y_extinct_coef_dead(:)    = 0.0
         c%y_grain_rate(:)           = 0.0
         c%y_leaf_no_frac(:)         = 0.0
         c%y_n_conc_crit_flower(:)   = 0.0
         c%y_n_conc_crit_leaf(:)     = 0.0
         c%y_n_conc_crit_stem(:)     = 0.0
         c%y_n_conc_max_flower(:)    = 0.0
         c%y_n_conc_max_leaf(:)      = 0.0
         c%y_n_conc_max_stem(:)      = 0.0
         c%y_n_conc_min_flower(:)    = 0.0
         c%y_n_conc_min_leaf(:)      = 0.0
         c%y_n_conc_min_stem(:)      = 0.0
         c%y_plant_death(:)          = 0.0
         c%y_senescence_fac(:)       = 0.0
         c%y_sla_max(:)              = 0.0
         c%y_stress_photo(:)         = 0.0
         c%y_sw_fac_root (:)         = 0.0
         c%y_swdef_fix (:)           = 0.0
         c%y_swdef_leaf (:)          = 0.0
         c%y_swdef_pheno (:)         = 0.0
         c%y_tiller_tt(:)            = 0.0
         c%y_tt(:)                   = 0.0
         c%y_tt_other(:)             = 0.0
         c%n_supply_preference       = ' '
         c%fasw_emerg(:)             = 0.0
         c%rel_emerg_rate(:)         = 0.0

         g%canopy_height             = 0.0
         g%cnd_grain_conc (:)        = 0.0
         g%cnd_photo (:)             = 0.0
         g%cover_dead                = 0.0
         g%cover_green               = 0.0
         g%cover_green_sum           = 0.0
         g%cover_sen                 = 0.0
         g%cswd_expansion (:)        = 0.0
         g%cswd_pheno (:)            = 0.0
         g%cswd_photo (:)            = 0.0
         g%cultivar                  = ' '
         g%current_stage             = 0.0
         g%day_of_year               = 0
         g%daylength_at_emerg        = 0.0
         g%days_tot (:)              = 0.0
         g%dlayer (:)                = 0.0
         g%dlt_canopy_height         = 0.0
         g%dlt_dm                    = 0.0
         g%dlt_dm_dead_detached(:)   = 0.0
         g%dlt_dm_detached(:)        = 0.0
         g%dlt_dm_grain_demand       = 0.0
         g%dlt_dm_green(:)           = 0.0
         g%dlt_dm_green_retrans(:)   = 0.0
         g%dlt_dm_sen_retrans(:)     = 0.0
         g%dlt_dm_light              = 0.0
         g%dlt_dm_N                  = 0.0
         g%dlt_dm_senesced(:)        = 0.0
         g%dlt_dm_stress_max         = 0.0
         g%dlt_dm_water              = 0.0
         g%dlt_heat_stress_tt        = 0.0
         g%dlt_lai                   = 0.0
         g%dlt_lai_stressed          = 0.0
         g%dlt_lai_pot               = 0.0
         g%dlt_leaf_no               = 0.0
         g%dlt_leaf_no_dead          = 0.0
         g%dlt_leaf_no_pot           = 0.0
         g%dlt_N_dead_detached(:)    = 0.0
         g%dlt_N_detached(:)         = 0.0
         g%dlt_N_green(:)            = 0.0
         g%dlt_N_retrans(:)          = 0.0
         g%dlt_N_senesced(:)         = 0.0
         g%dlt_NO3gsm(:)             = 0.0
         g%dlt_plants                = 0.0
         g%dlt_plants_all          = 0.0
         g%dlt_plants_temp         = 0.0
         g%dlt_plants_water        = 0.0
         g%dlt_plants_barren       = 0.0
         g%dlt_root_depth            = 0.0
         g%dlt_slai                  = 0.0
         g%dlt_slai_detached         = 0.0
         g%dlt_stage                 = 0.0
         g%dlt_sw_dep(:)             = 0.0
         g%dlt_tiller_no             = 0.0
         g%dlt_tlai_dead_detached    = 0.0
         g%dlt_tt                    = 0.0
         g%dlt_tt_curv               = 0.0
         g%dlt_tt_other              = 0.0
         g%dm_dead(:)                = 0.0
         g%dm_green(:)               = 0.0
         g%dm_green_demand(:)        = 0.0
         g%dm_plant_min(:)           = 0.0
         g%dm_plant_top_tot(:)       = 0.0
         g%dm_senesced(:)            = 0.0
         g%dm_stress_max(:)          = 0.0
         g%dm_tiller_independence    = 0.0
         g%dul_dep (:)               = 0.0
         g%fr_intc_radn              = 0.0
         g%grain_no                  = 0.0
         g%heat_stress_tt(:)         = 0.0
         g%isdate                    = 0
         g%lai                       = 0.0
         g%lai_equilib_light(:)       = 0.0
         g%lai_equilib_water(:)       = 0.0
         g%lai_max                   = 0.0
         g%latitude                  = 0.0
         g%leaf_area(max_leaf)       = 0.0
         g%leaf_no(:)                = 0.0
         g%leaf_no_dead(:)           = 0.0
         g%leaf_no_dead_const2       = 0.0
         g%leaf_no_effective         = 0.0
         g%leaf_no_final             = 0.0
         g%leaf_no_ref               = 0.0
         g%leaf_no_total             = 0.0
         g%lf_no_dead_at_flaglf      = 0.0
         g%maxt                      = 0.0
         g%mdate                     = 0
         g%mint                      = 0.0
         g%N_conc_act_stover_tot     = 0.0
         g%N_conc_crit(:)            = 0.0
         g%N_conc_crit_stover_tot    = 0.0
         g%N_conc_max(:)             = 0.0
         g%N_conc_min(:)             = 0.0
         g%N_dead(:)                 = 0.0
         g%N_demand (:)              = 0.0
         g%N_demand_tot              = 0.0
         g%N_green(:)                = 0.0
         g%N_max (:)                 = 0.0
         g%N_senesced(:)             = 0.0
         g%N_tiller_independence     = 0.0
         g%N_uptake_grain_tot        = 0.0
         g%N_uptake_stover_tot       = 0.0
         g%N_uptake_tot              = 0.0
         g%NO3gsm (:)                = 0.0
         g%NO3gsm_min(:)             = 0.0
         g%NO3gsm_diffn_pot(:)       = 0.0
         g%NO3gsm_mflow_avail(:)     = 0.0
         g%n_fix_pot                 = 0.0
         g%num_layers                = 0
         g%phase_tt(:)               = 0.0
         g%phase_tt_curv(:)          = 0.0
         g%phase_tt_other(:)         = 0.0
         g%plant_status              = ' '
         g%plants                    = 0.0
         g%previous_stage            = 0.0
         g%radn                      = 0.0
         g%radn_int                  = 0.0
         g%root_depth                = 0.0
         g%row_spacing               = 0.0
         g%slai                      = 0.0
         g%soil_temp(:)              = 0.0
         g%sowing_depth              = 0.0
         g%stem_class                = ' '
         g%sw_avail(:)               = 0.0
         g%sw_avail_pot(:)           = 0.0
         g%sw_demand                 = 0.0
         g%sw_dep (:)                = 0.0
         g%sw_supply (:)             = 0.0
         g%tiller_independence       = 0
         g%tiller_no(:)              = 0.0
         g%tlai_dead                 = 0.0
         g%transp_eff                = 0.0
         g%transpiration_tot         = 0.0
         g%tt_curv_tot(:)            = 0.0
         g%tt_other_tot(:)           = 0.0
         g%tt_tot(:)                 = 0.0
         g%y_tiller_tt_adj(:)        = 0.0
         g%year                      = 0
         g%swdef_expansion           = 0.0
         g%swdef_photo               = 0.0
         g%swdef_pheno               = 0.0
         g%nfact_expansion           = 0.0
         g%nfact_photo               = 0.0
         g%nfact_grain_conc          = 0.0
         g%nfact_pheno               = 0.0
         g%temp_stress_photo         = 0.0
         g%swdef_fixation            = 0.0
         g%node_no(:)                = 0.0
 !       g%dlt_leaf_no_pot          = 0.0
         g%dlt_node_no_pot           = 0.0

         g%uptake_P (:)              = 0.0
         g%pfact_photo               = 0.0
         g%pfact_pheno               = 0.0
         g%pfact_expansion           = 0.0
         g%pfact_grain               = 0.0
         g%p_demand                  = 0.0
         g%plant_p                   = 0.0
         g%dlt_plant_p               = 0.0
         g%P_conc_max                = 0.0
         g%P_conc_min                = 0.0
      !..%.................millet_p_int
         g%num_uptake_P             = 0

cjh special for erik - start
      g%stop_growth = .false.
      g%set_leaf_no_final = .false.
cjh special for erik - end


         p%est_days_emerg_to_init   = 0
         p%grain_gth_rate           = 0.0
         p%head_grain_no_max        = 0.0
         p%hi_incr                  = 0.0
         p%hi_max_pot               = 0.0
         p%kl(:)                    = 0.0
         p%ll_dep(:)                = 0.0
         p%pp_endjuv_to_init        = 0.0
         p%spla_intercept           = 0.0
         p%spla_prod_coef           = 0.0
         p%tiller_no_fertile        = 0.0
         p%tpla_inflection          = 0.0
         p%tpla_prod_coef           = 0.0
         p%tt_emerg_to_endjuv       = 0.0
         p%tt_flag_to_flower        = 0.0
         p%tt_flower_to_maturity    = 0.0
         p%tt_flower_to_start_grain = 0.0
         p%tt_maturity_to_ripe      = 0.0
         p%y0_const                 = 0.0
         p%y0_slope                 = 0.0
         p%xf(:)                    = 0.0
         p%uptake_source            = ' '

      p%x_stem_wt(:)               = 0.0
      p%y_height(:)                = 0.0
      p%num_stem_wt                = 0

      call pop_routine (my_name)
      return
      end

*     ===========================================================
      Recursive
     :subroutine millet_zero_variables ()
*     ===========================================================
      use MilletModule
      implicit none
      include 'data.pub'
      include 'error.pub'

*+  Purpose
*       Zero crop variables & arrays

*+  Changes
*     010994 jngh specified and programmed
*     090695 psc  add row spacing = 0
*     040998 sb added many

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'millet_zero_variables')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      g%row_spacing = 0
      g%sowing_depth = 0
      g%fr_intc_radn = 0
      g%soil_temp = 0
      g%daylength_at_emerg = 0
      g%cnd_photo = 0
      g%cnd_grain_conc = 0
      g%cswd_photo = 0
      g%cswd_expansion = 0
      g%cswd_pheno = 0
      g%dlt_tt = 0
      g%tt_tot = 0
      g%phase_tt = 0
      g%dlt_tt_curv = 0
      g%tt_curv_tot = 0
      g%phase_tt_curv = 0
      g%dlt_tt_other = 0
      g%tt_other_tot = 0
      g%phase_tt_other = 0
      g%heat_stress_tt = 0
      g%dlt_heat_stress_tt = 0
      g%dlt_stage = 0
      g%previous_stage = 0
      g%days_tot = 0
      g%dlt_canopy_height = 0
      g%canopy_height = 0
      g%plants = 0
      g%dlt_plants = 0
      g%dlt_plants_all          = 0.0
      g%dlt_plants_temp         = 0.0
      g%dlt_plants_water        = 0.0
      g%dlt_plants_barren       = 0.0
      g%grain_no = 0
      g%dlt_root_depth = 0
      g%root_depth = 0
      g%cover_green = 0
      g%cover_sen = 0
      g%cover_dead = 0
      g%dlt_dm = 0
      g%dlt_dm_green = 0
      g%dlt_dm_senesced = 0
      g%dlt_dm_detached = 0
      g%dlt_dm_dead_detached = 0
      g%dlt_dm_green_retrans = 0
      g%dlt_dm_sen_retrans(:) = 0.0
      g%dm_stress_max = 0
      g%dlt_dm_stress_max = 0
      g%dlt_dm_grain_demand = 0
      g%dm_green_demand = 0
      g%dm_dead = 0
      g%dm_green = 0
      g%dm_senesced = 0
      g%dm_plant_top_tot = 0
      g%slai = 0
      g%dlt_slai = 0
      g%dlt_lai = 0
      g%dlt_lai_pot = 0
      g%lai = 0
      g%tlai_dead = 0
      g%dlt_slai_detached = 0
      g%dlt_tlai_dead_detached = 0
      g%leaf_no = 0
      g%leaf_no_dead = 0
      g%dlt_leaf_no = 0
      g%dlt_leaf_no_pot = 0
      g%dlt_leaf_no_dead = 0
      g%leaf_no_final = 0
      g%leaf_no_effective = 0
      g%leaf_no_ref = 0
      g%leaf_no_total = 0
      g%leaf_no_dead_const2 = 0
      g%lf_no_dead_at_flaglf = 0
      g%leaf_area = 0
      g%lai_equilib_light = 0
      g%lai_equilib_water = 0
      g%tiller_no = 0
      g%dlt_tiller_no = 0
      g%dm_tiller_independence = 0
      g%N_tiller_independence = 0
      g%y_tiller_tt_adj = 0
      g%N_demand = 0
      g%N_max = 0
      g%dlt_N_green = 0
      g%dlt_N_senesced = 0
      g%dlt_N_detached = 0
      g%dlt_N_dead_detached = 0
      g%N_dead = 0
      g%N_green = 0
      g%N_senesced = 0
      g%dlt_N_retrans = 0
      g%dlt_NO3gsm = 0
      g%NO3gsm = 0
      g%NO3gsm_min = 0
      g%NO3gsm_diffn_pot(:)     = 0.0
      g%NO3gsm_mflow_avail(:)   = 0.0
      g%n_fix_pot               = 0.0
      g%N_conc_crit = 0
      g%N_conc_max = 0
      g%N_conc_min = 0
      g%dm_plant_min = 0
      g%dlayer = 0
      g%dlt_sw_dep = 0
      g%dul_dep = 0
      g%sw_dep = 0
      g%sw_demand = 0
      g%sw_avail_pot = 0
      g%sw_avail = 0
      g%sw_supply = 0
      g%transpiration_tot = 0
      g%N_uptake_tot = 0
      g%N_demand_tot = 0
      g%N_conc_act_stover_tot = 0
      g%N_conc_crit_stover_tot = 0
      g%N_uptake_grain_tot = 0
      g%N_uptake_stover_tot = 0
      g%lai_max = 0
      g%cover_green_sum = 0
      g%tiller_independence = 0
      g%num_layers = 0
      g%isdate = 0
      g%mdate = 0

      call pop_routine (my_name)
      return
      end



*     ===========================================================
      Recursive
     :subroutine millet_zero_daily_variables ()
*     ===========================================================
      use MilletModule
      implicit none
      include 'data.pub'
      include 'error.pub'

*+  Purpose
*       Zero crop daily variables & arrays

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'millet_zero_daily_variables')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

          ! zero pools etc.

      call fill_real_array (g%dlt_dm_green, 0.0, max_part)
      call fill_real_array (g%dlt_dm_green_retrans, 0.0, max_part)
      g%dlt_dm_sen_retrans(:) = 0.0
      call fill_real_array (g%dlt_N_green, 0.0, max_part)
      call fill_real_array (g%dlt_N_retrans, 0.0, max_part)
      call fill_real_array (g%dlt_NO3gsm, 0.0, max_layer)
      call fill_real_array (g%dlt_sw_dep, 0.0, max_layer)
      call fill_real_array (g%dm_green_demand, 0.0, max_part)
      call fill_real_array (g%N_demand, 0.0, max_part)

      call fill_real_array (g%dlt_dm_dead_detached, 0.0, max_part)
      call fill_real_array (g%dlt_dm_detached, 0.0, max_part)
      call fill_real_array (g%dlt_dm_senesced, 0.0, max_part)
      call fill_real_array (g%dlt_N_dead_detached, 0.0, max_part)
      call fill_real_array (g%dlt_N_detached, 0.0, max_part)
      call fill_real_array (g%dlt_N_senesced, 0.0, max_part)
      call fill_real_array (g%sw_avail, 0.0, max_layer)
      call fill_real_array (g%sw_avail_pot, 0.0, max_layer)
      call fill_real_array (g%sw_supply, 0.0, max_layer)

      g%dlt_tiller_no = 0.0
      g%dm_tiller_independence = 0.0
      g%N_tiller_independence = 0.0
      g%tiller_independence = 0

      g%dlt_tlai_dead_detached = 0.0
      g%dlt_slai_detached = 0.0
      g%dlt_canopy_height = 0.0
      g%dlt_dm = 0.0
      g%dlt_dm_grain_demand = 0.0
      g%dlt_dm_stress_max = 0.0
      g%dlt_heat_stress_tt = 0.0
      g%dlt_leaf_no = 0.0
      g%dlt_leaf_no_pot = 0.0
      g%dlt_leaf_no_dead = 0.0
      g%dlt_plants = 0.0
      g%dlt_plants_all          = 0.0
      g%dlt_plants_temp         = 0.0
      g%dlt_plants_water        = 0.0
      g%dlt_plants_barren       = 0.0
      g%dlt_root_depth = 0.0
      g%dlt_slai = 0.0
      g%dlt_stage = 0.0
      g%dlt_lai = 0.0
      g%dlt_tt = 0.0
      g%dlt_tt_curv = 0.0
      g%dlt_tt_other = 0.0
      g%sw_demand = 0.0

      call pop_routine (my_name)
      return
      end



*     ===========================================================
      Recursive
     :subroutine millet_init ()
*     ===========================================================
      use MilletModule
      implicit none
      include 'error.pub'

*+  Purpose
*       Crop initialisation

*+  Changes
*     010994 jngh specified and programmed
*     190599 jngh removed reference to version

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'millet_init')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call Write_string (' Initialising: ')

           ! initialize crop variables

      call millet_read_constants ()

      g%current_stage = real (plant_end)
      g%plant_status = status_out

      call pop_routine (my_name)
      return
      end



*     ===========================================================
      Recursive
     :subroutine millet_start_crop ()
*     ===========================================================
      use MilletModule
      implicit none
      include 'const.inc'
      include 'intrface.pub'
      include 'error.pub'

*+  Purpose
*       Start crop using parameters specified in passed record

*+  Changes
*     010994 jngh specified and programmed
*     090695 psc  add row spacing read
*     220696 jngh changed extract to collect

*+  Calls

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'millet_start_crop')

*+  Local Variables
      integer    numvals               ! number of values found in array
      character  string*200            ! output string

*- Implementation Section ----------------------------------

      call push_routine (my_name)
!gd
!         call get_current_module (module_name)
!         write(*,*) 'len_trim', len_trim(module_name)

      call Write_string ( 'Sow')

cjh      if (data_record.ne.blank) then

         call collect_real_var ('plants', '()'
     :                        , g%plants, numvals, 0.0, 100.0)
!cpsc
         call collect_real_var_optional (
     :                          'row_spacing', '(m)'
     :                        , g%row_spacing, numvals
     :                        , 0.0, 2.0)

         if (g%row_spacing .eq. 0.0) then
           g%row_spacing = c%row_spacing_default
         endif

         call collect_real_var (
     :                          'sowing_depth', '(mm)'
     :                        , g%sowing_depth, numvals
     :                        , 0.0, 100.0)

         call collect_char_var ('cultivar', '()'
     :                        , g%cultivar, numvals)

             ! report

         call write_string (new_line//new_line)

         string = '                 Crop Sowing Data'
         call write_string (string)

         string = '    ------------------------------------------------'
         call write_string (string)
!cpsc
         call write_string (
     :                     '    Sowing  Depth Plants Spacing Cultivar')
!cpsc
         call write_string (
     :                     '    Day no   mm     m^2     m     Name   ')

         string = '    ------------------------------------------------'
         call write_string (string)
!cpsc
         write (string, '(3x, i7, 3f7.1, 1x, a10)')
     :                   g%day_of_year, g%sowing_depth
     :                 , g%plants, g%row_spacing, g%cultivar
         call write_string (string)

         string = '    ------------------------------------------------'
         call write_string (string)

                 ! get cultivar parameters

         call millet_read_cultivar_params ()

                 ! get root profile parameters

         call millet_read_root_params ()

         g%current_stage = real (sowing)
         g%plant_status = status_alive

cjh      else
            ! report empty sowing record
cjh         call fatal_error (err_user, 'No sowing criteria supplied')
cjh      endif

      call pop_routine (my_name)
      return
      end



*     ===========================================================
      Recursive
     :subroutine millet_initiate ()
*     ===========================================================
      use MilletModule
      implicit none
      include 'const.inc'
      include 'data.pub'
      include 'intrface.pub'
      include 'error.pub'

*+  Purpose
*       Initiate crop, tillers or other axes using parameters
*       specified in passed record.

*+  Changes
*     091095 jngh specified and programmed
*     220696 jngh changed extract to collect
*     180500 jngh changed the tiller wt and tiller N to be the same keywords being sent at initiation.
*                 Also removed optional from collects.

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'millet_initiate')

*+  Local Variables
      integer    numvals               ! number of values found in array
      character  string*200            ! output string
      real       N_tiller_plant        ! N content of tiller (g/plant)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call Write_string ( 'Initiate')

cjh      if (data_record.ne.blank) then

         call collect_real_var ('plants', '()'
     :                        , g%plants, numvals, 0.0, 100.0)

         call collect_real_var (
     :                          'tiller_wt', '(g/plant)'
     :                        , c%dm_leaf_init, numvals
     :                        , 0.0, 100.0)

         call collect_real_var (
     :                          'tiller_n', '(g/plant)'
     :                        , N_tiller_plant, numvals
     :                        , 0.0, 10.0)

         call collect_real_var (
     :                          'row_spacing', '(m)'
     :                        , g%row_spacing, numvals
     :                        , 0.0, 2.0)

         if (g%row_spacing .eq. 0.0) then
           g%row_spacing = c%row_spacing_default
         endif


         call collect_char_var ('cultivar', '()'
     :                        , g%cultivar, numvals)

             ! report

         call write_string (new_line//new_line)

         string = '                 Crop Initiate Data'
         call write_string (string)

         string = '    ------------------------------------------------'
         call write_string (string)
         call write_string (
     :             '   Initiate   Axes   DM      N   Spacing Cultivar')
         call write_string (
     :             '    Day no    m^2 g/plant g/plant   m      Name  ')

         string = '    ------------------------------------------------'
         call write_string (string)
         write (string, '(3x, i7, 4f7.1, 1x, a10)')
     :                   g%day_of_year
     :                 , g%plants, c%dm_leaf_init, N_tiller_plant
     :                 , g%row_spacing, g%cultivar
         call write_string (string)

         string = '    ------------------------------------------------'
         call write_string (string)

                 ! get cultivar parameters

         call millet_read_cultivar_params ()

                 ! get root profile parameters

         call millet_read_root_params ()

         c%N_leaf_init_conc = divide (N_tiller_plant
     :                              , c%dm_leaf_init, 0.0)
         c%dm_root_init = c%dm_leaf_init

cjh            this need to be changed back to emergence again. A temporary
cjh            fix to overcome problems of variables being calculated between
cjh            germ and emerg (leaf_no_final and phase_tt(germ_to emerg &
cjh            emerg_to_endjuv).
cjh         g%current_stage = real (emerg)

         g%current_stage = real (germ)
         g%plant_status = status_alive

cjh      else
            ! report empty sowing record
cjh         call fatal_error (err_user, 'No initiate criteria supplied')
cjh      endif

      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_read_cultivar_params ()
*     ===========================================================
      use MilletModule
      implicit none
      include 'const.inc'
      include 'read.pub'
      include 'error.pub'

*+  Purpose
*       Get cultivar parameters for named cultivar, from crop parameter file.

*+  Changes
*       090994 jngh specified and programmed

*+  Calls

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_read_cultivar_params')

*+  Local Variables
      character  string*200            ! output string
      integer    numvals               ! number of values read
      integer    i

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call write_string (
     :                 new_line//'   - Reading Cultivar Parameters')


         ! TEMPLATE OPTION
         !       millet_dm_grain_hi

      call read_real_var (g%cultivar
     :                    , 'hi_max_pot', '()'
     :                    , p%hi_max_pot, numvals
     :                    , 0.0, 1.0)

         ! TEMPLATE OPTION
         !   millet_check_grain_no  millet_grain_no

      call read_real_var (g%cultivar
     :                    , 'head_grain_no_max', '()'
     :                    , p%head_grain_no_max, numvals
     :                    , 0.0, c%head_grain_no_max_ub)

         ! TEMPLATE OPTION
         !   millet_dm_grain

      call read_real_var (g%cultivar
     :                    , 'grain_gth_rate', '()'
     :                    , p%grain_gth_rate, numvals
     :                    , 0.0, c%grain_gth_rate_ub)

         !   millet_phenology_init

      call read_real_var (g%cultivar
     :                    , 'tt_emerg_to_endjuv', '()'
     :                    , p%tt_emerg_to_endjuv, numvals
     :                    , 0.0, c%tt_emerg_to_endjuv_ub)

      call read_integer_var (g%cultivar
     :                    , 'est_days_emerg_to_init', '()'
     :                    , p%est_days_emerg_to_init, numvals
     :                    , 0, 100)

      call read_real_var (g%cultivar
     :                    , 'pp_endjuv_to_init', '()'
     :                    , p%pp_endjuv_to_init, numvals
     :                    , 0.0, c%pp_endjuv_to_init_ub)

      call read_real_var (g%cultivar
     :                    , 'tt_flower_to_maturity', '()'
     :                    , p%tt_flower_to_maturity, numvals
     :                    , 0.0, c%tt_flower_to_maturity_ub)

      call read_real_var (g%cultivar
     :                    , 'tt_flag_to_flower', '()'
     :                    , p%tt_flag_to_flower, numvals
     :                    , 0.0, c%tt_flag_to_flower_ub)

      call read_real_var (g%cultivar
     :                    , 'tt_flower_to_start_grain', '()'
     :                    , p%tt_flower_to_start_grain, numvals
     :                    , 0.0, c%tt_flower_to_start_grain_ub)


      call read_real_var (g%cultivar
     :                    , 'tt_maturity_to_ripe', '()'
     :                    , p%tt_maturity_to_ripe, numvals
     :                    , 0.0, c%tt_maturity_to_ripe_ub)

cgd   Eriks modifications for Leaf Area
      call read_real_var (g%cultivar
     :                    , 'y0_const', '()'
     :                    , p%y0_const, numvals
     :                    , -40000.0, 100000.0)

      call read_real_var (g%cultivar
     :                    , 'y0_slope', '()'
     :                    , p%y0_slope, numvals
     :                    , 0.0, 10000.0)

      call read_real_array (g%cultivar
     :                     , 'x_stem_wt', max_table, '()'
     :                     , p%x_stem_wt, p%num_stem_wt
     :                     , 0.0, 1000.0)

      call read_real_array (g%cultivar
     :                     , 'y_height', max_table, '()'
     :                     , p%y_height, p%num_stem_wt
     :                     , 0.0, 5000.0)

             ! report

      string = '    ------------------------------------------------'
      call write_string (string)

      write (string, '(4x,2a)')
     :                'Cultivar                 = ', g%cultivar
      call write_string (string)

      write (string, '(4x, a, i7)')
     :                'est_days_emerg_to_init  = '
     :               , p%est_days_emerg_to_init
      call write_string (string)

      write (string, '(4x, a, f7.1)')
     :                'tt_emerg_to_endjuv       = '
     :               , p%tt_emerg_to_endjuv
      call write_string (string)

      write (string, '(4x, a, f7.1)')
     :                'pp_endjuv_to_initp       = '
     :               , p%pp_endjuv_to_init
      call write_string (string)

      write (string, '(4x, a, f7.1)')
     :                'tt_flower_to_maturity    = '
     :               , p%tt_flower_to_maturity
      call write_string (string)

      write (string, '(4x, a, f7.1)')
     :                'head_grain_no_max        = '
     :               , p%head_grain_no_max
      call write_string (string)

      write (string, '(4x, a, f7.1)')
     :                'grain_gth_rate           = '
     :               , p%grain_gth_rate
      call write_string (string)

      write (string, '(4x, a, f7.1)')
     :                'tt_flag_to_flower        = '
     :               , p%tt_flag_to_flower
      call write_string (string)

      write (string, '(4x, a, f7.1)')
     :                'tt_flower_to_start_grain = '
     :               , p%tt_flower_to_start_grain
      call write_string (string)

      write (string, '(4x, a, f7.1)')
     :                'tt_maturity_to_ripe      = '
     :               , p%tt_maturity_to_ripe
      call write_string (string)


      write (string, '(4x, a, 10f7.1)')
     :                'x_stem_wt                = '
     :               , (p%x_stem_wt(i), i=1,p%num_stem_wt)
      call write_string (string)

      write (string, '(4x, a, 10f7.1)')
     :                'y_height                 = '
     :               , (p%y_height(i), i=1,p%num_stem_wt)
      call write_string (string)

      string = '    ------------------------------------------------'
      call write_string (string)

      call write_string (new_line//new_line)

      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_read_root_params ()
*     ===========================================================
      use MilletModule
      implicit none
      include 'const.inc'
      include 'data.pub'
      include 'read.pub'
      include 'error.pub'

*+  Purpose
*       Get root profile parameters

*+  Changes
*       090994 jngh specified and programmed
*     210395 jngh changed from millet_section to a parameters section

*+  Calls

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_read_root_params')
*
      character  section_name*(*)
      parameter (section_name = 'parameters')

*+  Local Variables
!      integer    numvals
      integer    layer                 ! layer number
      real       ll (max_layer)        ! lower limit of plant-extractable
                                       ! soil water for soil layer l
                                       ! (mm water/mm soil)
      integer    num_layers            ! number of layers in profile
      integer    numvals
      character  string*200            ! output string

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call write_string (
     :                  new_line
     :                  //'   - Reading root profile parameters')

         !       millet_sw_supply

      call read_char_var_optional (section_name
     :                     , 'uptake_source', '()'
     :                     , p%uptake_source, numvals)
      if (numvals.eq.0) then
         p%uptake_source = 'calc'
      else
      endif

      call read_real_array (section_name
     :                     , 'll', max_layer, '()'
     :                     , ll, num_layers
     :                     , 0.0, c%ll_ub)

      call fill_real_array (p%ll_dep, 0.0, max_layer)
      do 1000 layer = 1, num_layers
         p%ll_dep(layer) = ll(layer)*g%dlayer(layer)
1000  continue

      call read_real_array (section_name
     :                     , 'kl', max_layer, '()'
     :                     , p%kl, num_layers
     :                     , 0.0, c%kl_ub)

      call read_real_array (section_name
     :                     , 'xf', max_layer, '()'
     :                     , p%xf, num_layers
     :                     , 0.0, 1.0)


      if (p%uptake_source.eq.'apsim') then
         string = 'Uptake of NO3 and water calculated by'
     :            //' another APSIM module'

      elseif (p%uptake_source.eq.'calc') then
         string = 'Uptake of NO3 and water calculated by '
     :            //c%crop_type

      else
         string = blank

      endif

      call write_string (string)
      call write_string (blank)

          ! report
      call write_string (new_line//new_line)

      call write_string (new_line//new_line)

      write (string,'(4x, a)') '                Root Profile'
      call write_string (string)

      string = '    ------------------------------------------------'
      call write_string (string)

      string = '      Layer      Kl      Lower Exploration'
      call write_string (string)
      string = '      Depth              limit   Factor'
      call write_string (string)

      string = '      (mm)       ()     (mm/mm)    ()'
      call write_string (string)

      string = '    ------------------------------------------------'
      call write_string (string)

      do 2000 layer = 1, num_layers
         write (string,'(3x, 4f9.3)')
     :            g%dlayer(layer)
     :          , p%kl(layer)
     :          , ll(layer)
     :          , p%xf(layer)
         call write_string (string)
2000  continue

      string = '     ------------------------------------------------'
      call write_string (string)

      call write_string (new_line//new_line)

      call pop_routine (my_name)
      return
      end

*     ================================================================
      Recursive
     :subroutine millet_get_other_parameters ()
*     ================================================================
      use MilletModule
      implicit none
      include 'const.inc'
      include   'convert.inc'
      include 'data.pub'
      include 'intrface.pub'
      include 'error.pub'

*+  Purpose
*      Get the values of parameters from other modules.

*+  Assumptions
*      assumes variable has the following format
*         <variable_name> = <variable_value/s> (<units>)

*+  Changes
*     080600 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_get_other_parameters')

*+  Local Variables
      integer    numvals               ! number of values found in array

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call get_real_var (unknown_module, 'latitude', '(oL)'
     :                                  , g%latitude, numvals
     :                                  , c%latitude_lb, c%latitude_ub)


      call pop_routine (my_name)
      return
      end




*     ===========================================================
      Recursive
     :subroutine millet_end_crop ()
*     ===========================================================
      use milletModule
      implicit none
      include   'const.inc'            ! new_line, lu_scr_sum
      include   'convert.inc'          ! gm2kg, sm2ha, mm2cm, cmm2cc
      include 'data.pub'
      include 'crp_comm.pub'
      include 'error.pub'

*+  Purpose
*       End crop

*+  Changes
*       290994 jngh specified and programmed
*      191099 jngh changed to millet_Send_Crop_Chopped_Event

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'millet_end_crop')

*+  Local Variables
      real       dm_residue            ! dry matter added to residue (g/m^2)
      real       N_residue             ! nitrogen added to residue (g/m^2)
      real       dm_root               ! dry matter added to soil (g/m^2)
      real       N_root                ! nitrogen added to soil (g/m^2)
      character  string*400            ! output string
      real       yield                 ! grain wt (kg/ha)
      real       fraction_to_Residue(max_part)   ! fraction sent to residue (0-1)
      real       dlt_dm_crop(max_part) ! change in dry matter of crop (kg/ha)
      real       dlt_dm_N(max_part)    ! N content of changeed dry matter (kg/ha)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (g%plant_status.ne.status_out) then
         g%plant_status = status_out
         g%current_stage = real (plant_end)

                ! report

         yield = (g%dm_green(grain) + g%dm_dead(grain)) *gm2kg /sm2ha
         write (string, '(3x, a, f7.1)')
     :                  ' ended. Yield (dw) = ', yield
         call Write_string (string)

             ! now do post harvest processes

         dm_root = g%dm_green(root)
     :           + g%dm_dead(root)
     :           + g%dm_senesced(root)

         N_root  = g%N_green(root)
     :           + g%N_dead(root)
     :           + g%N_senesced(root)

         call millet_root_incorp (dm_root, N_root)

             ! put stover and any remaining grain into surface residue

         dm_residue = sum_real_array (g%dm_green, max_part)
     :              - g%dm_green(root)

     :              + sum_real_array (g%dm_senesced, max_part)
     :              - g%dm_senesced(root)

     :              + sum_real_array (g%dm_dead, max_part)
     :              - g%dm_dead(root)

         dlt_dm_crop(:) = (g%dm_green(:)
     :                  + g%dm_senesced(:)
     :                  + g%dm_dead(:))
     :                  * gm2kg/sm2ha

         N_residue = sum_real_array (g%N_green, max_part)
     :             - g%N_green(root)

     :             + sum_real_array (g%N_senesced, max_part)
     :             - g%N_senesced(root)

     :             + sum_real_array (g%N_dead, max_part)
     :             - g%N_dead(root)

         dlt_dm_N(:) = (g%N_green(:)
     :               + g%N_senesced(:)
     :               + g%N_dead(:))
     :               * gm2kg/sm2ha

!         call crop_top_residue (c%crop_type, dm_residue, N_residue)

         fraction_to_residue(:) = 1.0
         fraction_to_Residue(root) = 0.0

         if (sum(dlt_dm_crop) .gt. 0.0) then
            call millet_Send_Crop_Chopped_Event
     :                (c%crop_type
     :               , part_name
     :               , dlt_dm_crop
     :               , dlt_dm_N
     :               , fraction_to_Residue
     :               , max_part)
         else
            ! no surface residue
         endif


         write (string, '(40x, a, f7.1, a, 3(a, 40x, a, f6.1, a))')
     :                  '  straw residue ='
     :                  , dm_residue * gm2kg /sm2ha, ' kg/ha'
     :                  , new_line
     :                  , '  straw N = '
     :                  , N_residue * gm2kg /sm2ha, ' kg/ha'

     :                  , new_line
     :                  , '  root residue = '
     :                  , dm_root * gm2kg /sm2ha, ' kg/ha'
     :                  , new_line
     :                  , '  root N = '
     :                  , N_root * gm2kg /sm2ha, ' kg/ha'

         call write_string (string)

      else
      endif

      call pop_routine (my_name)
      return
      end




*     ===========================================================
      Recursive
     :subroutine millet_store_value (array, value)
*     ===========================================================
      use MilletModule
      implicit none
      include 'date.pub'
      include 'error.pub'

*+  Sub-Program Arguments
      real       array(*)              ! (OUTPUT) storage array
      real       value                 ! (INPUT) value to be stored

*+  Purpose
*       Stores a value in an annual circular array

*+  Changes
*     230695 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_store_value')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      array(g%day_of_year) = value

      if (g%day_of_year.eq.365
     :   .and. leap_year (g%year - 1)) then
         array(366) = 0.0
      else
      endif

      call pop_routine (my_name)
      return
      end

*     ===========================================================
      Recursive
     :subroutine millet_ONtick ()
*     ===========================================================
      use milletModule
      implicit none
      include 'const.inc'
      include   'event.inc'
      include 'intrface.pub'
      include 'error.pub'
      include 'event.pub'

*+  Purpose
*     Update internal time record and reset daily state variables.

*+  Mission Statement
*     Update internal time record and reset daily state variables.

*+  Changes
*        260899 nih

*+  Local Variables
!      character temp1*5
!      integer   temp2
      integer    numvals               ! number of values found in array

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'millet_ONtick')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      ! Note that time and timestep information is not required
      ! and so dummy variables are used in their place.

!      call handler_ONtick(g%day_of_year, g%year, temp1, temp2)

      call collect_integer_var (DATA_day
     :                      ,'()'
     :                      ,g%day_of_year
     :                      ,numvals
     :                      ,1
     :                      ,366)

      call collect_integer_var (DATA_year
     :                      ,'()'
     :                      ,g%year
     :                      ,numvals
     :                      ,min_year
     :                      ,max_year)

      call pop_routine (myname)
      return
      end

*     ===========================================================
      Recursive
     :subroutine millet_ONnewmet ()
*     ===========================================================
      use milletModule
      implicit none
      include 'error.pub'
      include 'event.pub'
      include 'event.inc'
      include 'intrface.pub'
*+  Purpose
*     Get new met data

*+  Mission Statement
*     Get new met data

*+  Changes
*        270899 nih
*        111099 jngh removed unused variables

*+  Local Variables
!      real   temp1
!      real   temp2
      integer    numvals               ! number of values found in array

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'millet_ONnewmet')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      ! Note that rain and vp are not needed
      ! and so dummy variables are used in their place.

!c      call handler_ONnewmet(g%solrad, g%tempmx, g%tempmn, temp1, temp2)

      call collect_real_var (DATA_radn
     :                      ,'(MJ)'
     :                      ,g%radn
     :                      ,numvals
     :                      ,c%radn_lb
     :                      ,c%radn_ub)

      call collect_real_var (DATA_maxt
     :                      ,'(oC)'
     :                      ,g%maxt
     :                      ,numvals
     :                      ,c%maxt_lb
     :                      ,c%maxt_ub)

      call collect_real_var (DATA_mint
     :                      ,'(oC)'
     :                      ,g%mint
     :                      ,numvals
     :                      ,c%mint_lb
     :                      ,min(c%mint_ub, g%maxt))


      call pop_routine (myname)
      return
      end





*     ================================================================
      Recursive
     :subroutine millet_get_other_variables ()
*     ================================================================
      use MilletModule
      implicit none
      include 'const.inc'
      include   'convert.inc'
      include 'data.pub'
      include 'intrface.pub'
      include 'error.pub'

*+  Purpose
*      Get the values of variables/arrays from other modules.

*+  Assumptions
*      assumes variable has the following format
*         <variable_name> = <variable_value/s> (<units>)

*+  Changes
*     010994 jngh specified and programmed
*     220696 jngh optimised order of gets
*     140896 jngh modified fr_intc_radn name to inclued a suffix of module name
*     140896 sb used min_year and max_year instead of c%year_lb and c%year_ub.

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_get_other_variables')

*+  Local Variables
      integer    layer                 ! layer number
      integer    numvals               ! number of values put into array
      real       dlayer(max_layer)     ! soil layer depths (mm)
      real       NO3(max_layer)        ! soil NO3 content (kg/ha)
      real       NO3_min(max_layer)    ! soil NO3 minimum (kg/ha)
      character  module_name*(Max_module_name_size) ! module name
      real       soil_temp             ! soil surface temperature (oC)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

                               ! canopy

      call get_current_module (module_name)

      call get_real_var_optional (unknown_module
     :                           , 'fr_intc_radn_'//module_name
     :                           , '()'
     :                           , g%fr_intc_radn
     :                           , numvals
     :                           , 0.0
     :                           , 1.0)


      if (g%stem_class .eq. class_tiller) then

cgd leaf_no_ref is the final leaf no for the main shoot needed by tillers
        call get_real_var  (unknown_module, 'leaf_no_ref', '()'
     :                             , g%leaf_no_ref,numvals
     :                             , 0.0, 100.0)

      else
         ! main tiller owns leaf_no_ref
      endif
                                ! climate
!cpsc
      call get_real_var_optional (unknown_module, 'soil_temp', '(oC)'
     :                                  , soil_temp, numvals
     :                                  , 0.0, 80.0)

      if (numvals.eq.0) then
         ! soil temp not supplied
      else
         call millet_store_value (g%soil_temp, soil_temp)
      endif

                               ! canopy

      call get_real_var_optional (unknown_module, 'cover_green_sum','()'
     :                                  , g%cover_green_sum, numvals
     :                                  , 0.0, 1.0)
c+!!!!!!!! what to do if no waterbalance variables found
            ! soil profile and soil water

      call get_real_array (unknown_module, 'dlayer', max_layer
     :                                    , '(mm)'
     :                                    , dlayer, numvals
     :                                    , c%dlayer_lb, c%dlayer_ub)

      if (g%num_layers.eq.0) then
            ! we assume dlayer hasn't been initialised yet.
         call copy_real_array(g%dlayer, dlayer, numvals)
         g%num_layers = numvals

      else
            ! dlayer may be changed from its last setting
         do 1000 layer = 1, numvals
            p%ll_dep(layer) = divide (p%ll_dep(layer)
     :                              , g%dlayer(layer), 0.0)
     :                      * dlayer(layer)

            g%dlayer(layer) = dlayer(layer)
1000     continue
         g%num_layers = numvals
      endif

      call get_real_array (unknown_module, 'dul_dep', max_layer
     :                                    , '(mm)'
     :                                    , g%dul_dep, numvals
     :                                    , c%dul_dep_lb, c%dul_dep_ub)

      call get_real_array (unknown_module, 'sw_dep', max_layer
     :                                    , '(mm)'
     :                                    , g%sw_dep, numvals
     :                                    , c%sw_dep_lb, c%sw_dep_ub)

                                ! soil nitrogen pools
      call get_real_array_optional (unknown_module, 'no3', max_layer
     :                                  ,  '(kg/ha)'
     :                                  , NO3, numvals
     :                                  , c%NO3_lb, c%NO3_ub)
      if (numvals.eq.0) then
            ! we have no N supply - make non-limiting.
         call fill_real_array (NO3, 10000.0, g%num_layers)
      else
      endif
      do 2000 layer = 1, g%num_layers
         g%NO3gsm(layer) = NO3(layer) * kg2gm /ha2sm
2000  continue


      call get_real_array_optional (unknown_module, 'no3_min',max_layer
     :                                  ,  '(kg/ha)'
     :                                  , NO3_min, numvals
     :                                  , c%NO3_min_lb, c%NO3_min_ub)
      do 3000 layer = 1, g%num_layers
         g%NO3gsm_min(layer) = NO3_min(layer) * kg2gm /ha2sm
3000  continue

      call pop_routine (my_name)
      return
      end



*     ================================================================
      Recursive
     :subroutine millet_set_other_variables ()
*     ================================================================
      use MilletModule
      implicit none
      include 'const.inc'
      include 'action.inc'
      include   'convert.inc'
      include 'data.pub'
      include 'intrface.pub'
      include 'error.pub'
      include 'postbox.pub'

*+  Purpose
*      Set the value of a variable or array in other module/s.

*+  Notes
*      a flag is set if any of the totals is requested.  The totals are
*      reset during the next process phase when this happens.

*+  Changes
*     010994 jngh specified and programmed
*     240696 jngh changed set_ to post_ construct

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_set_other_variables')

*+  Local Variables
      real       dlt_NO3(max_layer)    ! soil NO3 change (kg/ha)
      integer    layer                 ! soil layer no.
      integer    num_layers            ! number of layers

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call millet_update_other_variables ()

      if (p%uptake_source.eq.'calc') then
c+!!!! perhaps we should get number of layers at init and keep it
         num_layers = count_of_real_vals (g%dlayer, max_layer)

         do 1000 layer = 1, num_layers
            dlt_NO3(layer) = g%dlt_NO3gsm(layer) * gm2kg /sm2ha
1000     continue

         call new_postbox()
         call post_real_array ('dlt_no3', '(kg/ha)'
     :                    , dlt_NO3, num_layers)

         call post_real_array ('dlt_sw_dep', '(mm)'
     :                    , g%dlt_sw_dep, num_layers)

         call Action_send (unknown_module
     :                               ,ACTION_set_variable
     :                               ,'dlt_no3')
         call Action_send (unknown_module
     :                               ,ACTION_set_variable
     :                               ,'dlt_sw_dep')
         call delete_postbox()

      else
      endif

      call pop_routine (my_name)
      return
      end

*     ===========================================================
      Recursive
     :subroutine millet_update_other_variables ()
*     ===========================================================
      use milletModule
      implicit none
      include   'convert.inc'
      include 'data.pub'
      include 'crp_comm.pub'
      include 'error.pub'

*+  Purpose
*       Update other modules states

*+  Changes
*      250894 jngh specified and programmed
*      191099 jngh changed to millet_Send_Crop_Chopped_Event

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_update_other_variables')

*+  Local Variables
      real       dm_residue(max_part)  ! dry matter added to residue (kg/ha)
      real       N_residue(max_part)   ! nitrogen added to residue (kg/ha)
      real       fraction_to_Residue(max_part)   ! fraction sent to residue (0-1)

*- Implementation Section ----------------------------------
      call push_routine (my_name)

         ! dispose of detached material from senesced parts in
         ! live population

      dm_residue(:) = g%dlt_dm_detached(:) * gm2kg/sm2ha
      N_residue(:) = g%dlt_N_detached(:) * gm2kg/sm2ha

      fraction_to_Residue(:) = 1.0
      fraction_to_Residue(root) = 0.0

!      call crop_top_residue (g%crop_type, dm_residue, N_residue)
         if (sum(dm_residue) .gt. 0.0) then
            call millet_Send_Crop_Chopped_Event
     :                (c%crop_type
     :               , part_name
     :               , dm_residue
     :               , N_residue
     :               , fraction_to_Residue
     :               , max_part)
         else
            ! no surface residue
         endif


         ! put roots into root residue

      call millet_root_incorp (g%dlt_dm_detached(root)
     :                    , g%dlt_N_detached(root))

         ! now dispose of dead population detachments

      dm_residue(:) = g%dlt_dm_dead_detached(:) * gm2kg/sm2ha
      N_residue(:) = g%dlt_N_dead_detached(:) * gm2kg/sm2ha

      fraction_to_Residue(:) = 1.0
      fraction_to_Residue(root) = 0.0

!      call crop_top_residue (c%crop_type, dm_residue, N_residue)
         if (sum(dm_residue) .gt. 0.0) then
            call millet_Send_Crop_Chopped_Event
     :                (c%crop_type
     :               , part_name
     :               , dm_residue
     :               , N_residue
     :               , fraction_to_Residue
     :               , max_part)
         else
            ! no surface residue
         endif

         ! put roots into root residue

      call millet_root_incorp (g%dlt_dm_dead_detached(root)
     :                      , g%dlt_N_dead_detached(root))

      call pop_routine (my_name)
      return
      end

* ====================================================================
      Recursive
     :subroutine millet_Send_Crop_Chopped_Event (crop_type
     :                                           , dm_type
     :                                           , dlt_crop_dm
     :                                           , dlt_dm_n
     :                                           , fraction_to_Residue
     :                                           , max_part)
* ====================================================================
      implicit none
      include 'const.inc'
      include 'event.inc'
      include 'intrface.pub'
      include 'error.pub'
      include 'data.pub'
      include 'postbox.pub'

*+  Sub-Program Arguments
      character  crop_type*(*)              ! (INPUT) crop type
      character  dm_type(*)*(*)             ! (INPUT) residue type
      real  dlt_crop_dm(*)                  ! (INPUT) residue weight (kg/ha)
      real  dlt_dm_n(*)                     ! (INPUT) residue N weight (kg/ha)
      real  fraction_to_Residue(*)          ! (INPUT) residue fraction to residue (0-1)
      integer max_part                      ! (INPUT) number of residue types
*+  Purpose
*     Notify other modules of crop chopped.

*+  Mission Statement
*     Notify other modules of crop chopped.

*+  Changes
*   070999 jngh - Programmed and Specified
*   110700 jngh - Changed dm_tyoe to array.

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'millet_Send_Crop_Chopped_Event')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call new_postbox ()

         ! send message regardless of fatal error - will stop anyway


!cjh      write(*,*) 'millet: '//EVENT_Crop_Chopped
!cjh      write(*,*) 'millet: '//DATA_crop_type
!cjh     :               , ' '//crop_type
!cjh      write(*,*) 'millet: '//DATA_dm_type
!cjh     :               , ' '//dm_type
!cjh      write(*,*) 'millet: '//DATA_dlt_crop_dm
!cjh     :               , dlt_crop_dm
!cjh      write(*,*) 'millet: '//DATA_dlt_dm_n
!cjh     :               , dlt_dm_n
!cjh      write(*,*) 'millet: '//DATA_fraction_to_Residue
!cjh     :               , fraction_to_Residue

      call post_char_var   (DATA_crop_type
     :                        ,'()'
     :                        , crop_type)
      call post_char_array (DATA_dm_type
     :                        ,'()'
     :                        , dm_type
     :                        , max_part)
      call post_real_array (DATA_dlt_crop_dm
     :                        ,'(kg/ha)'
     :                        , dlt_crop_dm
     :                        , max_part)
      call post_real_array (DATA_dlt_dm_n
     :                        ,'(kg/ha)'
     :                        , dlt_dm_n
     :                        , max_part)
      call post_real_array (DATA_fraction_to_Residue
     :                        ,'()'
     :                        , fraction_to_Residue
     :                        , max_part)

      call event_send (EVENT_Crop_Chopped)

      call delete_postbox ()


      call pop_routine (myname)
      return
      end


*     ===============================================================
      Recursive
     :subroutine millet_set_my_variable (Variable_name)
*     ===============================================================
      use MilletModule
      implicit none
      include 'intrface.pub'
      include 'error.pub'

*+  Sub-Program Arguments
      character  Variable_name*(*)     ! (INPUT) Variable name to search for

*+  Purpose
*      Set a variable in this module as requested by another.

*+  Changes
*      290393 jngh
*      220696 jngh added message_unused to else
*                  changed respond2set to collect

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_set_my_variable')

*+  Local Variables
      integer    numvals               ! number of values found in array

*- Implementation Section ----------------------------------

      call push_routine (my_name)

             ! **** Repeat for each variable

*      if (variable_name .eq. '????') then
*         call collect_real_array (variable_name, '()', max_layer
*     :                               , ????, numvals
*     :                               , 0.0, 1.0)
cejvo used for setting from the manager
cused for validation of leaf area model only

      if (variable_name .eq. 'leaf_no_final') then
         call collect_real_var (variable_name
     :                              , '()'
     :                              , g%leaf_no_final, numvals
     :                              , 0.0, 40.0)

cjh special for erik - start
         g%set_leaf_no_final = .true.
cjh special for erik - end

      elseif (variable_name .eq. 'dlt_leaf_no_pot') then
         call collect_real_var (variable_name
     :                              , '()'
     :                              , g%dlt_leaf_no_pot, numvals
     :                              , 0.0, 40.0)

cjh special for erik - start
      elseif (variable_name .eq. 'current_stage') then
         call collect_real_var (variable_name
     :                              , '()'
     :                              , g%current_stage, numvals
     :                              , 1.0, real(max_stage))
cjh special for erik - end

      else
            ! Don't know this variable name
         call message_unused ()
      endif

      call pop_routine (my_name)
      return
      end



*     ================================================================
      Recursive
     :subroutine millet_send_my_variable (variable_name)
*     ================================================================
      use MilletModule
      implicit none
      include   'convert.inc'          ! gm2kg, sm2ha, mm2cm, cmm2cc
      include 'science.pub'
      include 'data.pub'
      include 'intrface.pub'
      include 'error.pub'

*+  Sub-Program Arguments
      character variable_name*(*)      ! (INPUT) variable name to search for

*+  Purpose
*      Return the value of a variable requested by other modules.

*+  Changes
*      string_concat
*      090495 psc  added nfact to output list
*      170495 psc  added grain_size, yield, biomass to output list
*      220696 jngh added message_unused to else

*+  Calls
!x      real  MILLET_SWDEF
!x      real  millet_N_fixation

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_send_my_variable')

*+  Local Variables
      real       act_N_up              ! cumulative total N uptake by plant
                                       ! (kg/ha)
      real       ag_N_up               ! N uptake by grain (kg/ha)
      real       apt_N_up              ! N uptake by stover (kg/ha)
      real       cover_tot             ! total crop cover fraction (0-1)
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      real       grain_N_pcnt          ! grain N concentration percent (%)
      real       lai_sum               ! leaf area index of all leaf material
                                       ! live + dead
      integer    num_layers            ! number of layers in profile
      integer    stage_no              ! current stage no.
      real       NO3gsm_tot            ! total NO3 in the root profile (g/m^2)
      real       N_demand              ! sum N demand for plant parts (g/m^2)
      real       N_supply              ! N supply for grain (g/m^2)
      real       N_uptake_sum          ! N supply from soil
      real       grain_size            ! individual grain wt (g/grain)
      real       yield                 ! grain yield (kg/ha)
      real       biomass               ! total above-ground biomass (kg/ha)
      real       biomass_tot           ! total biomass (above and below)
      real       biomass_n             ! total above-ground biomass N (kg/ha)
      real       biomass_p             ! total above-ground biomass P
      real       sw_supply_sum         ! total supply over profile (mm)
      integer    layer                 ! soil layer
      real       esw_layr(max_layer)   ! plant extractable soil water            !
      real       plant_p_max
      real       pconc
      real       hi
      real       stover

*- Implementation Section ----------------------------------

      call push_routine (my_name)


            ! management

cjh
      if (variable_name .eq. 'plant_status') then
         call respond2get_char_var (variable_name
     :                             , '()'
     :                             , g%plant_status)

      elseif (variable_name .eq. 'dlt_stage') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%dlt_stage)

      elseif (variable_name .eq. 'stage') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%current_stage)

      elseif (variable_name .eq. 'stage_code') then
         if (g%plant_status.ne.status_out) then
         stage_no = int (g%current_stage)
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , c%stage_code_list(stage_no))
         else
            call respond2get_real_var (variable_name
     :                             , '()'
     :                             , 0.0)
         endif
      elseif (variable_name .eq. 'stage_name') then
         if (g%plant_status.ne.status_out) then
         stage_no = int (g%current_stage)
         call respond2get_char_var (variable_name
     :                             , '()'
     :                             , c%stage_names(stage_no))
         else
            call respond2get_char_var (variable_name
     :                             , '()'
     :                             , status_out)
         endif
      elseif (variable_name .eq. 'crop_type') then
         call respond2get_char_var (variable_name
     :                             , '()'
     :                             , c%crop_type)

      elseif (variable_name .eq. 'dlt_tt') then
         call respond2get_real_var (variable_name
     :                             , '(oCd)'
     :                             , g%dlt_tt)

      elseif (variable_name .eq. 'phase_tt') then
         call respond2get_real_array (variable_name
     :                             , '(oC)'
     :                             , g%phase_tt
     :                             , max_stage)

      elseif (variable_name .eq. 'tt_tot') then
         call respond2get_real_array (variable_name
     :                             , '(oC)'
     :                             , g%tt_tot
     :                             , max_stage)
cgd
      elseif (variable_name .eq. 'dlt_tiller_no') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%dlt_tiller_no)


      elseif (variable_name .eq. 'days_tot') then
         call respond2get_real_array (variable_name
     :                             , '()'
     :                             , g%days_tot
     :                             , max_stage)
cgd
      elseif (variable_name .eq. 'tiller_no') then
         call respond2get_real_array (variable_name
     :                              , '()'
     :                              , g%tiller_no
     :                              , max_stage)

cgd

      elseif (variable_name .eq. 'leaf_no') then
         call respond2get_real_array (variable_name
     :                              , '()'
     :                              , g%leaf_no
     :                              , max_stage)

      elseif (variable_name .eq. 'dlt_leaf_no') then
         call respond2get_real_var (variable_name
     :                              , '()'
     :                              , g%dlt_leaf_no
     :                              )


      elseif (variable_name .eq. 'leaf_no_final') then
         call respond2get_real_var (variable_name
     :                              , '()'
     :                              , g%leaf_no_final)
cgd
      elseif (variable_name .eq. 'leaf_no_total') then
         call respond2get_real_var (variable_name
     :                              , '()'
     :                              , g%leaf_no_total)
cejvo
      elseif (variable_name .eq. 'leaf_no_effective') then
         call respond2get_real_var (variable_name
     :                              , '()'
     :                              , g%leaf_no_effective)

      elseif (variable_name .eq. 'leaf_no_dead_const2') then
         call respond2get_real_var (variable_name
     :                              , '()'
     :                              , g%leaf_no_dead_const2)

      elseif (variable_name .eq. 'lf_no_dead_at_flaglf') then
         call respond2get_real_var (variable_name
     :                              , '()'
     :                              , g%lf_no_dead_at_flaglf)

      elseif (variable_name .eq. 'leaf_no_dead') then
         call respond2get_real_array (variable_name
     :                              , '()'
     :                              , g%leaf_no_dead
     :                              , max_stage)

      elseif ((variable_name .eq. 'leaf_no_ref') .and.
     :   (g%stem_class .eq. class_main)) then

         ! only the main stem can respond

         call respond2get_real_var (variable_name
     :                              , '()'
     :                              , g%leaf_no_ref)

      elseif (variable_name .eq. 'leaf_area') then
         call respond2get_real_array (variable_name
     :                              , '()'
     :                              , g%leaf_area
     :                              , max_leaf)
cejvo
      elseif (variable_name .eq. 'y_tiller_tt_adj') then
         call respond2get_real_array (variable_name
     :                              , '()'
     :                              , g%y_tiller_tt_adj
     :                              , max_table)

      elseif (variable_name .eq. 'height') then
         call respond2get_real_var (variable_name
     :                             , '(mm)'
     :                             , g%canopy_height)

      elseif (variable_name .eq. 'root_depth') then
         call respond2get_real_var (variable_name
     :                             , '(mm)'
     :                             , g%root_depth)

      elseif (variable_name .eq. 'plants') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%plants)

      elseif (variable_name .eq. 'grain_no') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%grain_no)

      elseif (variable_name .eq. 'grain_size') then
         grain_size = divide (g%dm_green(grain) + g%dm_dead(grain)
     :                      , g%grain_no, 0.0)
         call respond2get_real_var (variable_name
     :                             , '(g)'
     :                             , grain_size)


      elseif (variable_name .eq. 'cover_green') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%cover_green)

      elseif (variable_name .eq. 'cover_tot') then
         cover_tot = 1.0
     :             - (1.0 - g%cover_green)
     :             * (1.0 - g%cover_sen)
     :             * (1.0 - g%cover_dead)

         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , cover_tot)

      elseif (variable_name .eq. 'lai_sum') then
         lai_sum = g%lai + g%slai + g%tlai_dead
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , lai_sum)

      elseif (variable_name .eq. 'tlai') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%lai + g%slai)

      elseif (variable_name .eq. 'slai') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%slai)

      elseif (variable_name .eq. 'lai') then
         call respond2get_real_var (variable_name
     :                             , '(m^2/m^2)'
     :                             , g%lai)

      elseif (variable_name .eq. 'tlai_dead') then
         call respond2get_real_var (variable_name
     :                             , '(m^2/m^2)'
     :                             , g%tlai_dead)


      elseif (variable_name .eq. 'sla') then
         call respond2get_real_var (variable_name
     :                             , '(mm2/g)'
     :                             , divide(g%lai*sm2smm
     :                             , g%dm_green(leaf), 0.0))

            ! plant biomass

      elseif (variable_name .eq. 'root_wt') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%dm_green(root))

      elseif (variable_name .eq. 'leaf_wt') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%dm_green(leaf))

      elseif (variable_name .eq. 'stem_wt') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%dm_green(stem))

      elseif (variable_name .eq. 'flower_wt') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%dm_green(flower))

      elseif (variable_name .eq. 'stem+flower_wt') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%dm_green(stem)
     :                               +g%dm_green(flower))

      elseif (variable_name .eq. 'grain_wt') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%dm_green(grain))

      elseif (variable_name .eq. 'dm_green') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g%dm_green
     :                             , max_part)

      elseif (variable_name .eq. 'dm_senesced') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g%dm_senesced
     :                             , max_part)

      elseif (variable_name .eq. 'dm_dead') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g%dm_dead
     :                             , max_part)

      elseif (variable_name .eq. 'yield') then
         yield = (g%dm_green(grain) + g%dm_dead(grain))
     :           * gm2kg / sm2ha
         call respond2get_real_var (variable_name
     :                             , '(kg/ha)'
     :                             , yield)

      elseif (variable_name .eq. 'biomass') then
         biomass = (sum_real_array (g%dm_green, max_part)
     :           - g%dm_green(root)
     :           + sum_real_array (g%dm_senesced, max_part)
     :           - g%dm_senesced(root)
     :           + sum_real_array (g%dm_dead, max_part)
     :           - g%dm_dead(root))
     :           * gm2kg / sm2ha

         call respond2get_real_var (variable_name
     :                             , '(kg/ha)'
     :                             , biomass)

!scc 10/95 output harvest index
      elseif (variable_name .eq. 'hi') then
         yield = (g%dm_green(grain) + g%dm_dead(grain))
     :           * gm2kg / sm2ha
         biomass = (sum_real_array (g%dm_green, max_part)
     :           - g%dm_green(root)
     :           + sum_real_array (g%dm_senesced, max_part)
     :           - g%dm_senesced(root)
     :           + sum_real_array (g%dm_dead, max_part)
     :           - g%dm_dead(root))
     :           * gm2kg / sm2ha
         hi = divide(yield, biomass, 0.0)
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , hi)

!mjr 05/97 output stover (kg/ha)
      elseif (variable_name .eq. 'stover') then
         yield = (g%dm_green(grain) + g%dm_dead(grain))
     :           * gm2kg / sm2ha
         biomass = (sum_real_array (g%dm_green, max_part)
     :           - g%dm_green(root)
     :           + sum_real_array (g%dm_senesced, max_part)
     :           - g%dm_senesced(root)
     :           + sum_real_array (g%dm_dead, max_part)
     :           - g%dm_dead(root))
     :           * gm2kg / sm2ha
         stover = biomass - yield
         call respond2get_real_var (variable_name
     :                             , '(kg/ha)'
     :                             , stover)

      elseif (variable_name .eq. 'green_biomass') then
         biomass = (sum_real_array (g%dm_green, max_part)
     :                 - g%dm_green(root))
     :           * gm2kg / sm2ha

         call respond2get_real_var (variable_name
     :                             , '(kg/ha)'
     :                             , biomass)


      elseif (variable_name .eq. 'biomass_wt') then
         biomass = (sum_real_array (g%dm_green, max_part)
     :           - g%dm_green(root)
     :           + sum_real_array (g%dm_senesced, max_part)
     :           - g%dm_senesced(root)
     :           + sum_real_array (g%dm_dead, max_part)
     :           - g%dm_dead(root))

         call respond2get_real_var (variable_name
     :                             , '(g/m2)'
     :                             , biomass)

      elseif (variable_name .eq. 'green_biomass_wt') then
         biomass = sum_real_array (g%dm_green, max_part)
     :           - g%dm_green(root)

         call respond2get_real_var (variable_name
     :                             , '(g/m2)'
     :                             , biomass)

      elseif (variable_name .eq. 'stover_wt') then
         yield = (g%dm_green(grain) + g%dm_dead(grain))
         biomass = (sum_real_array (g%dm_green, max_part)
     :           - g%dm_green(root)
     :           + sum_real_array (g%dm_senesced, max_part)
     :           - g%dm_senesced(root)
     :           + sum_real_array (g%dm_dead, max_part)
     :           - g%dm_dead(root))
         stover = biomass - yield
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , stover)

      elseif (variable_name .eq. 'dlt_dm') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%dlt_dm)

      elseif (variable_name .eq. 'dlt_dm_green') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g%dlt_dm_green
     :                             , max_part)

      elseif (variable_name .eq. 'dlt_dm_green_retrans') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g%dlt_dm_green_retrans
     :                             , max_part)

      elseif (variable_name .eq. 'dlt_dm_sen_retrans') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g%dlt_dm_sen_retrans
     :                             , max_part)

      elseif (variable_name .eq. 'dlt_dm_detached') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g%dlt_dm_detached
     :                             , max_part)

      elseif (variable_name .eq. 'dlt_dm_dead_detached') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g%dlt_dm_dead_detached
     :                             , max_part)

      elseif ((variable_name .eq. 'biomass_n')
     :                       .or.
     :       (variable_name .eq. 'n_uptake')) then
         biomass_n = (sum_real_array (g%n_green, max_part)
     :             - g%n_green(root)
     :             + sum_real_array (g%n_senesced, max_part)
     :             - g%n_senesced(root)
     :             + sum_real_array (g%n_dead, max_part)
     :             - g%n_dead(root))

         call respond2get_real_var (variable_name
     :                             , '(g/m2)'
     :                             , biomass_n)

      elseif (variable_name .eq. 'green_biomass_n') then
         biomass_n = (sum_real_array (g%n_green, max_part)
     :                 - g%n_green(root))

         call respond2get_real_var (variable_name
     :                             , '(g/m2)'
     :                             , biomass_n)

      elseif (variable_name .eq. 'n_green') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g%N_green
     :                             , max_part)

      elseif (variable_name .eq. 'n_senesced') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g%N_senesced
     :                             , max_part)

      elseif (variable_name .eq. 'n_dead') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g%N_dead
     :                             , max_part)

      elseif (variable_name .eq. 'dlt_n_green') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g%dlt_N_green
     :                             , max_part)

      elseif (variable_name .eq. 'dlt_n_retrans') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g%dlt_N_retrans
     :                             , max_part)

      elseif (variable_name .eq. 'dlt_n_detached') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g%dlt_N_detached
     :                             , max_part)

      elseif (variable_name .eq. 'dlt_n_dead_detached') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g%dlt_N_dead_detached
     :                             , max_part)

      elseif (variable_name .eq. 'swdef_pheno') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%swdef_pheno)

      elseif (variable_name .eq. 'swdef_photo') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%swdef_photo)

      elseif (variable_name .eq. 'swdef_expan') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%swdef_expansion)

      elseif (variable_name .eq. 'ep') then
         num_layers = count_of_real_vals (g%dlayer, max_layer)
         call respond2get_real_array (variable_name
     :                               , '(mm)'
     :                               , g%dlt_sw_dep
     :                               , num_layers)

      elseif (variable_name .eq. 'cep') then
         call respond2get_real_var (variable_name
     :                             , '(mm)'
     :                             , - g%transpiration_tot)

      elseif (variable_name .eq. 'sw_demand') then
         call respond2get_real_var (variable_name
     :                             , '(mm)'
     :                             , g%sw_demand)

      elseif (variable_name .eq. 'sw_supply') then
         deepest_layer = find_layer_no (g%root_depth, g%dlayer
     :                                , max_layer)
         sw_supply_sum = sum_real_array (g%sw_supply, deepest_layer)
         call respond2get_real_var (variable_name
     :                             , '(mm)'
     :                             , sw_supply_sum)

      elseif (variable_name .eq. 'esw_layr') then

         num_layers = count_of_real_vals (g%dlayer, max_layer)
         do 1000 layer = 1, num_layers
            esw_layr(layer) = l_bound (g%sw_dep(layer) - p%ll_dep(layer)
     :                        , 0.0)
1000     continue
         call respond2get_real_array (variable_name
     :                               , '(mm)'
     :                               , esw_layr
     :                               , num_layers)

      elseif (variable_name .eq. 'das') then
         call respond2get_integer_var (variable_name
     :                             , '(days)'
     :                             , nint(sum_between (sowing, now
     :                                          , g%days_tot)))

            ! plant nitrogen

      elseif (variable_name .eq. 'n_conc_stover') then
         call respond2get_real_var (variable_name
     :                             , '(%)'
     :                             , g%N_conc_act_stover_tot)

      elseif (variable_name .eq. 'n_conc_crit') then
         call respond2get_real_var (variable_name
     :                             , '(%)'
     :                             , g%N_conc_crit_stover_tot)

      elseif (variable_name .eq. 'n_grain_pcnt') then
         grain_N_pcnt = divide (g%N_green(grain)
     :                        , g%dm_green(grain), 0.0)
     :                        * fract2pcnt
         call respond2get_real_var (variable_name
     :                             , '(%)'
     :                             , grain_N_pcnt)


      elseif (variable_name .eq. 'n_uptake_grain') then
         ag_N_up = g%N_uptake_grain_tot*gm2kg /sm2ha
         call respond2get_real_var (variable_name
     :                             , '(kg/ha)'
     :                             , ag_N_up)


      elseif (variable_name .eq. 'n_uptake') then
         act_N_up = g%N_uptake_tot*gm2kg /sm2ha
         call respond2get_real_var (variable_name
     :                             , '(kg/ha)'
     :                             , act_N_up)


      elseif (variable_name .eq. 'n_uptake_stover') then
         apt_N_up = g%N_uptake_stover_tot*gm2kg /sm2ha
         call respond2get_real_var (variable_name
     :                             , '(kg/ha)'
     :                             , apt_N_up)

      elseif (variable_name .eq. 'no3_tot') then
         deepest_layer = find_layer_no (g%root_depth, g%dlayer
     :                                , max_layer)
         NO3gsm_tot = sum_real_array (g%NO3gsm, deepest_layer)
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , NO3gsm_tot)

      elseif (variable_name .eq. 'n_demand') then
         N_demand = sum_real_array (g%N_demand, max_part)
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , N_demand)

      elseif (variable_name .eq. 'n_supply') then
         N_supply = sum_real_array ( g%dlt_N_green, max_part)
     :            - g%dlt_N_green(grain)
     :            - g%dlt_N_green(flower)
     :            - g%dlt_N_green(root)
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , N_supply)

      elseif (variable_name .eq. 'n_supply_soil') then
         deepest_layer = find_layer_no (g%root_depth,g%dlayer,max_layer)
         N_uptake_sum = - sum_real_array (g%dlt_NO3gsm, deepest_layer)
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , N_uptake_sum)

      elseif (variable_name .eq. 'n_fix_pot') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%N_fix_pot)
      elseif (variable_name .eq. 'nfact_photo') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%nfact_photo)

      elseif (variable_name .eq. 'nfact_expan') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%nfact_expansion)

      elseif (variable_name .eq. 'nfact_grain') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%nfact_grain_conc)

      elseif (variable_name .eq. 'nfact_grain_tot') then
         call respond2get_real_array (variable_name
     :                             , '()'
     :                             , g%cnd_grain_conc
     :                             , max_stage)

      elseif (variable_name .eq. 'no3_demand') then
         N_demand = sum_real_array (g%N_demand, max_part)
     :            * gm2kg/sm2ha
         call respond2get_real_var (variable_name
     :                             , '(kg/ha)'
     :                             , N_demand)

      elseif (variable_name .eq. 'sw_demand') then
         call respond2get_real_var (variable_name
     :                             , '(mm)'
     :                             , g%sw_demand)

      elseif (variable_name .eq. 'photoperiod') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , day_length (g%day_of_year
     :                             , g%latitude, c%twilight))

      ! Crop Phosphorus Variables
      ! -------------------------
      elseif (variable_name .eq. 'pfact_photo') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%pfact_photo)

      elseif (variable_name .eq. 'pfact_pheno') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%pfact_pheno)

      elseif (variable_name .eq. 'pfact_expan') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%pfact_expansion)

      elseif (variable_name .eq. 'p%demand') then
         ! really ought to do this properly
         call respond2get_real_var (variable_name
     :                             , '(kg/ha)'
     :                             , g%p_demand*10.)

      elseif (variable_name .eq. 'plant_p') then
         call respond2get_real_var (variable_name
     :                             , '(g/m2)'
     :                             , g%plant_p)

      elseif (variable_name .eq. 'biomass_p') then
        biomass = (sum_real_array (g%dm_green, max_part)
     :           - g%dm_green(root)
     :           + sum_real_array (g%dm_senesced, max_part)
     :           - g%dm_senesced(root)
     :           + sum_real_array (g%dm_dead, max_part)
     :           - g%dm_dead(root))
        biomass_tot = sum_real_array (g%dm_green, max_part)
     :           + sum_real_array (g%dm_senesced, max_part)
     :           + sum_real_array (g%dm_dead, max_part)

         biomass_p = g%plant_p * divide (biomass
     :                                  ,biomass_tot
     :                                  ,0.0)

         call respond2get_real_var (variable_name
     :                             , '(g/m2)'
     :                             , biomass_p)

      elseif (variable_name .eq. 'plant_p_max') then
         biomass     = sum_real_array (g%dm_green, max_part)
     :               + sum_real_array (g%dm_senesced, max_part)
     :               + sum_real_array (g%dm_dead, max_part)
         plant_p_max = biomass * g%P_conc_max

         call respond2get_real_var (variable_name
     :                             , '(g/m2)'
     :                             , plant_p_max)

      elseif (variable_name .eq. 'pconc_max') then
         call respond2get_real_var (variable_name
     :                             , '(g/m2)'
     :                             , g%P_conc_max)

      elseif (variable_name .eq. 'pconc') then
         biomass     = sum_real_array (g%dm_green, max_part)
     :               + sum_real_array (g%dm_senesced, max_part)
     :               + sum_real_array (g%dm_dead, max_part)
        pconc = 100.0 * divide (g%plant_p
     :                 ,biomass
     :                 ,0.0)

         call respond2get_real_var (variable_name
     :                             , '(%)'
     :                             , pconc)


      else
         ! not my variable
         call message_unused ()

      endif

      call pop_routine (my_name)
      return
      end

*     ===========================================================
      Recursive
     :logical function millet_my_type ()
*     ===========================================================
      use MilletModule
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
      parameter (my_name = 'millet_my_type')

*+  Local Variables
      character  crop_type*50          ! crop type in data string
      integer    numvals               ! number of values returned

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call collect_char_var_optional ('type', '()'
     :                              , crop_type, numvals)

      if (crop_type.eq.c%crop_type .or. numvals.eq.0) then
         millet_my_type = .true.
      else
         millet_my_type = .false.
      endif

      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine millet_read_constants ()
*     ===========================================================
      use MilletModule
      implicit none
      include 'const.inc'
      include 'read.pub'
      include 'error.pub'

*+  Purpose
*       Crop initialisation - reads constants from constants file

*+  Changes
*     010994 jngh specified and programmed
*     070495 psc added extra constants (leaf_app etc.)
*     110695 psc added soil temp effects on plant establishment
*     261097 gol added constant 'c%photo_tiller_crit'
*     020998 sb removed c%year_lb and c%year_ub.

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'millet_read_constants')
*
      character  section_name*(*)
      parameter (section_name = 'constants')

*+  Local Variables
      integer    numvals               ! number of values returned

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call write_string (
     :                  new_line//'    - Reading constants')

      call read_char_var (section_name
     :                     , 'crop_type', '()'
     :                     , c%crop_type, numvals)

      call read_char_array (section_name
     :                     , 'stage_names', max_stage, '()'
     :                     , c%stage_names, numvals)

      call read_real_array (section_name
     :                     , 'stage_code', max_stage, '()'
     :                     , c%stage_code_list, numvals
     :                     , 0.0, 1000.0)

      call read_real_array (section_name
     :                     , 'rue', max_stage, '(g dm/mj)'
     :                     , c%rue, numvals
     :                     , 0.0, 1000.0)

      call read_real_array (section_name
     :                     , 'root_depth_rate', max_stage, '(mm)'
     :                     , c%root_depth_rate, numvals
     :                     , 0.0, 1000.0)

      call read_real_array (section_name
     :                     , 'ratio_root_shoot', max_stage, '()'
     :                     , c%ratio_root_shoot, numvals
     :                     , 0.0, 1000.0)

cejvo
      call read_real_var (section_name
     :                    , 'row_spacing_default', '()'
     :                    , c%row_spacing_default, numvals
     :                    , 0.0, 2.0)

      call read_real_array (section_name
     :                    , 'x_row_spacing', max_table, '(m)'
     :                    , c%x_row_spacing, c%num_row_spacing
     :                    , 0.0, 2.0)

      call read_real_array (section_name
     :                    , 'y_extinct_coef', max_table, '()'
     :                    , c%y_extinct_coef, c%num_row_spacing
     :                    , 0.0, 1.0)

      call read_real_array (section_name
     :                    , 'y_extinct_coef_dead', max_table, '()'
     :                    , c%y_extinct_coef_dead, c%num_row_spacing
     :                    , 0.0, 1.0)

          ! millet_root_distrib

      call read_real_var (section_name
     :                    , 'root_extinction', '()'
     :                    , c%root_extinction, numvals
     :                    , 0.0, 10.0)

         ! crop failure

      call read_real_var (section_name
     :                    , 'leaf_no_crit', '()'
     :                    , c%leaf_no_crit, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'tt_emerg_limit', '(oC)'
     :                    , c%tt_emerg_limit, numvals
     :                    , 0.0, 365.0)

      call read_real_var (section_name
     :                    , 'days_germ_limit', '(days)'
     :                    , c%days_germ_limit, numvals
     :                    , 0.0, 365.0)

      call read_real_var (section_name
     :                    , 'swdf_pheno_limit', '()'
     :                    , c%swdf_pheno_limit, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'swdf_photo_limit', '()'
     :                    , c%swdf_photo_limit, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'swdf_photo_rate', '()'
     :                    , c%swdf_photo_rate, numvals
     :                    , 0.0, 1.0)


         !    millet_root_depth

      call read_real_var (section_name
     :                    , 'initial_root_depth', '(mm)'
     :                    , c%initial_root_depth, numvals
     :                    , 0.0, 1000.0)

cglh      call read_real_var (section_name
cglh     :                    , 'root_depth_lag_start', '(days)'
cglh     :                    , c%root_depth_lag_start, numvals
cglh     :                    , 0.0, 365.0)

cglh      call read_real_var (section_name
cglh     :                    , 'root_depth_lag_end', '(days)'
cglh     :                    , c%root_depth_lag_end, numvals
cglh     :                    , 0.0, 365.0)

         !    millet_leaf_area_init

      call read_real_var (section_name
     :                    , 'initial_tpla', '(mm^2)'
     :                    , c%initial_tpla, numvals
     :                    , 0.0, 100000.0)

         ! TEMPLATE OPTION
         !    millet_leaf_area

cejvo             linear interpolate SLA_max

      call read_real_array (section_name
     :                    , 'x_lai', max_table, '(mm2/mm2)'
     :                    , c%x_lai, c%num_lai
     :                    , 0.0, 15.0)

      call read_real_array (section_name
     :                    , 'y_sla_max', max_table, '(mm2/g)'
     :                    , c%y_sla_max, c%num_lai
     :                    , 0.0, 1000000.0)

      call read_real_array (section_name
     :                    , 'x_lai_ratio', max_table, '()'
     :                    , c%x_lai_ratio, c%num_lai_ratio
     :                    , 0.0, 1.0)

      call read_real_array (section_name
     :                    , 'y_leaf_no_frac', max_table, '()'
     :                    , c%y_leaf_no_frac, c%num_lai_ratio
     :                    , 0.0, 1.0)

      call read_real_var (section_name
     :                    , 'tpla_min', '()'
     :                    , c%tpla_min , numvals
     :                    , 0.0, 1000.0)

         !    millet_height

      call read_real_var (section_name
     :                    , 'height_max', '(mm)'
     :                    , c%height_max, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'height_stem_slope', '(mm/g/stem)'
     :                    , c%height_stem_slope, numvals
     :                    , 0.0, 1000.0)

         !    millet_get_cultivar_params

      call read_real_var (section_name
     :                    , 'head_grain_no_max_ub', '()'
     :                    , c%head_grain_no_max_ub, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'grain_gth_rate_ub', '()'
     :                    , c%grain_gth_rate_ub, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'tt_emerg_to_endjuv_ub', '()'
     :                    , c%tt_emerg_to_endjuv_ub, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'pp_endjuv_to_init_ub', '()'
     :                    , c%pp_endjuv_to_init_ub, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'tt_flower_to_maturity_ub', '()'
     :                    , c%tt_flower_to_maturity_ub, numvals
     :                    , 0.0, 2000.0)

      call read_real_var (section_name
     :                    , 'tt_maturity_to_ripe_ub', '()'
     :                    , c%tt_maturity_to_ripe_ub, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'tt_flower_to_start_grain_ub', '()'
     :                    , c%tt_flower_to_start_grain_ub, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'tt_flag_to_flower_ub', '()'
     :                    , c%tt_flag_to_flower_ub, numvals
     :                    , 0.0, 1000.0)

         !    millet_transp_eff

      call read_real_var (section_name
     :                    , 'svp_fract', '()'
     :                    , c%svp_fract, numvals
     :                    , 0.0, 1.0)

      call read_real_array (section_name
     :                    , 'transp_eff_cf', max_stage, '(kpa)'
     :                    , c%transp_eff_cf, numvals
     :                    , 0.0, 1.0)

         ! TEMPLATE OPTION
         !    millet_grain_no

      call read_real_var (section_name
     :                    , 'head_grain_no_crit', '()'
     :                    , c%head_grain_no_crit, numvals
     :                    , 0.0, 1000.0)

         !    millet_plants_barren

      call read_real_var (section_name
     :                    , 'barren_crit', '()'
     :                    , c%barren_crit, numvals
     :                    , 0.0, 1.0)

         !    millet_germination

      call read_real_var (section_name
     :                    , 'pesw_germ', '(mm/mm)'
     :                    , c%pesw_germ, numvals
     :                    , 0.0, 1.0)

      call read_real_array (section_name
     :                     , 'fasw_emerg', max_table, '()'
     :                     , c%fasw_emerg, c%num_fasw_emerg
     :                     , 0.0, 1.0)
      call read_real_array (section_name
     :                     , 'rel_emerg_rate', max_table, '()'
     :                     , c%rel_emerg_rate, c%num_fasw_emerg
     :                     , 0.0, 1.0)

         ! TEMPLATE OPTION
         !    millet_grain_no

      call read_real_var (section_name
     :                    , 'grain_n_conc_min', '()'
     :                    , c%grain_N_conc_min, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'seed_wt_min', '(g/seed)'
     :                    , c%seed_wt_min, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'growth_rate_min', '(g/plant)'
     :                    , c%growth_rate_min, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'growth_rate_crit', '(g/plant)'
     :                    , c%growth_rate_crit, numvals
     :                    , 0.0, 1000.0)

         !    millet_leaf_appearance

      call read_real_var (section_name
     :                    , 'leaf_no_at_emerg', '()'
     :                    , c%leaf_no_at_emerg, numvals
     :                    , 0.0, 100.0)

         !    millet_N_uptake

      call read_real_var (section_name
     :                    , 'no3_diffn_const', '(days)'
     :                    , c%NO3_diffn_const, numvals
     :                    , 0.0, 100.0)

      call read_char_var (section_name
     :                     , 'n_supply_preference', '()'
     :                     , c%n_supply_preference, numvals)

         !    millet_N_fixation

      call read_real_var (section_name
     :                    , 'N_fix_rate', '(g N/g plant)'
     :                    , c%N_fix_rate, numvals
     :                    , 0.0, 1.0)

         !    millet_phenology_init

      call read_real_var (section_name
     :                    , 'shoot_lag', '(oC)'
     :                    , c%shoot_lag, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'shoot_rate', '(oC/mm)'
     :                    , c%shoot_rate, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'photoperiod_base', '(hr)'
     :                    , c%photoperiod_base, numvals
     :                    , 0.0, 24.0)

      call read_real_var (section_name
     :                    , 'photoperiod_crit', '(hr)'
     :                    , c%photoperiod_crit, numvals
     :                    , 0.0, 24.0)

cgol added new constant "c%photo_tiller_crit" to read from ini file
      call read_real_var (section_name
     :                    , 'photo_tiller_crit', '(hr)'
     :                    , c%photo_tiller_crit, numvals
     :                    , 0.0, 24.0)

      call read_real_var (section_name
     :                    , 'leaf_app_rate1', '(oC)'
     :                    , c%leaf_app_rate1, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'leaf_app_rate2', '(oC)'
     :                    , c%leaf_app_rate2, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'leaf_no_rate_change', '()'
     :                    , c%leaf_no_rate_change, numvals
     :                    , 0.0, 30.0)

         !    millet_dm_init

      call read_real_var (section_name
     :                    , 'dm_leaf_init', '(g/plant)'
     :                    , c%dm_leaf_init, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'dm_root_init', '(g/plant)'
     :                    , c%dm_root_init, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'dm_stem_init', '(g/plant)'
     :                    , c%dm_stem_init, numvals
     :                    , 0.0, 1000.0)

         !    millet_get_root_params

      call read_real_var (section_name
     :                    , 'll_ub', '()'
     :                    , c%ll_ub, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'kl_ub', '()'
     :                    , c%kl_ub, numvals
     :                    , 0.0, 1000.0)

         !    millet_leaf_no_final

      call read_real_var (section_name
     :                    , 'leaf_init_rate', '(oC)'
     :                    , c%leaf_init_rate, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'leaf_no_seed', '(leaves)'
     :                    , c%leaf_no_seed, numvals
     :                    , 0.0, 100.0)

cglh      call read_real_var (section_name
cglh     :                    , 'floral_init_error', '(oc)'
cglh     :                    , c%floral_init_error, numvals
cglh     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'leaf_no_min', '()'
     :                   , c%leaf_no_min, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'leaf_no_max', '()'
     :                   , c%leaf_no_max, numvals
     :                   , 0.0, 100.0)

cgd
      call read_real_var (section_name
     :                   , 'leaf_no_diff', '()'
     :                   , c%leaf_no_diff, numvals
     :                   , 0.0, 10.0)

         !    millet_retranslocate

      call read_real_var (section_name
     :                    , 'stem_trans_frac', '()'
     :                    , c%stem_trans_frac, numvals
     :                    , 0.0, 1.0)

      call read_real_var (section_name
     :                    , 'leaf_trans_frac', '()'
     :                    , c%leaf_trans_frac, numvals
     :                    , 0.0, 1.0)

         !    millet_watck

      call read_real_var (section_name
     :                    , 'minsw', '()'
     :                    , c%minsw, numvals
     :                    , 0.0, 1000.0)

         ! TEMPLATE OPTION
         !    millet_dm_grain

      call read_real_var (section_name
     :                    , 'swdf_grain_min', '()'
     :                    , c%swdf_grain_min, numvals
     :                    , 0.0, 100.0)

         !    millet_N_dlt_grain_conc

      call read_real_var (section_name
     :                    , 'sw_fac_max', '()'
     :                    , c%sw_fac_max, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'temp_fac_min', '()'
     :                    , c%temp_fac_min, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'sfac_slope', '()'
     :                    , c%sfac_slope, numvals
     :                    , -10.0, 10.0)

      call read_real_var (section_name
     :                    , 'tfac_slope', '()'
     :                    , c%tfac_slope, numvals
     :                    , 0.0, 100.0)

         !    millet_leaf_death

      call read_real_var (section_name
     :                    , 'leaf_no_dead_const', '()'
     :                    , c%leaf_no_dead_const, numvals
     :                    , -9.0, 100.0)
cejvo
       call read_real_var (section_name
     :                    , 'leaf_no_dead_slope', '()'
     :                    , c%leaf_no_dead_slope, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'leaf_no_dead_slope1', '()'
     :                    , c%leaf_no_dead_slope1, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'leaf_no_dead_slope2', '()'
     :                    , c%leaf_no_dead_slope2, numvals
     :                    , 0.0, 100.0)

         !    millet_get_other_variables

         ! checking the bounds of the bounds..
      call read_real_var (section_name
     :                    , 'latitude_ub', '(oL)'
     :                    , c%latitude_ub, numvals
     :                    , -90.0, 90.0)

      call read_real_var (section_name
     :                    , 'latitude_lb', '(oL)'
     :                    , c%latitude_lb, numvals
     :                    , -90.0, 90.0)

      call read_real_var (section_name
     :                    , 'maxt_ub', '(oC)'
     :                    , c%maxt_ub, numvals
     :                    , 0.0, 60.0)

      call read_real_var (section_name
     :                    , 'maxt_lb', '(oC)'
     :                    , c%maxt_lb, numvals
     :                    , 0.0, 60.0)

      call read_real_var (section_name
     :                    , 'mint_ub', '(oC)'
     :                    , c%mint_ub, numvals
     :                    , 0.0, 40.0)

      call read_real_var (section_name
     :                    , 'mint_lb', '(oC)'
     :                    , c%mint_lb, numvals
     :                    , -100.0, 100.0)

      call read_real_var (section_name
     :                    , 'radn_ub', '(MJ/m^2)'
     :                    , c%radn_ub, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'radn_lb', '(MJ/m^2)'
     :                    , c%radn_lb, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'dlayer_ub', '(mm)'
     :                    , c%dlayer_ub, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'dlayer_lb', '(mm)'
     :                    , c%dlayer_lb, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'dul_dep_ub', '(mm)'
     :                    , c%dul_dep_ub, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'dul_dep_lb', '(mm)'
     :                    , c%dul_dep_lb, numvals
     :                    , 0.0, 10000.0)

                                ! 8th block
      call read_real_var (section_name
     :                    , 'sw_dep_ub', '(mm)'
     :                    , c%sw_dep_ub, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'sw_dep_lb', '(mm)'
     :                    , c%sw_dep_lb, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'no3_ub', '(kg/ha)'
     :                    , c%NO3_ub, numvals
     :                    , 0.0, 100000.0)

      call read_real_var (section_name
     :                    , 'no3_lb', '(kg/ha)'
     :                    , c%NO3_lb, numvals
     :                    , 0.0, 100000.0)

      call read_real_var (section_name
     :                    , 'no3_min_ub', '(kg/ha)'
     :                    , c%NO3_min_ub, numvals
     :                    , 0.0, 100000.0)

      call read_real_var (section_name
     :                    , 'no3_min_lb', '(kg/ha)'
     :                    , c%NO3_min_lb, numvals
     :                    , 0.0, 100000.0)

         !    millet_event

      call read_real_var (section_name
     :                    , 'grn_water_cont', '(g/g)'
     :                    , c%grn_water_cont, numvals
     :                    , 0.0, 1.0)

         !    millet_dm_partition

      call read_real_var (section_name
     :                    , 'sla_min', '(mm^2/g)'
     :                    , c%sla_min, numvals
     :                    , 0.0, 100000.0)

      call read_real_array (section_name
     :                    , 'frac_dm_to_leaf', max_stage, '()'
     :                    , c%frac_dm_to_leaf, numvals
     :                    , 0.0, 1.0)

      call read_real_var (section_name
     :                    , 'partition_rate_leaf', '()'
     :                    , c%partition_rate_leaf, numvals
     :                    , 0.0, 1.0)

      call read_real_var (section_name
     :                    , 'frac_leaf_pre_flower', '()'
     :                    , c%frac_leaf_pre_flower, numvals
     :                    , 0.0, 1.0)

      call read_real_var (section_name
     :                    , 'frac_leaf_post_flower', '()'
     :                    , c%frac_leaf_post_flower, numvals
     :                    , 0.0, 1.0)

      call read_real_var (section_name
     :                    , 'frac_stem2flower', '()'
     :                    , c%frac_stem2flower, numvals
     :                    , 0.0, 1.0)

      call read_real_var (section_name
     :                    , 'frac_flower2grain', '()'
     :                    , c%frac_flower2grain, numvals
     :                    , 0.0, 1.0)

         ! TEMPLATE OPTION
         !    millet_grain_no

      call read_real_var (section_name
     :                    , 'htstress_coeff', '()'
     :                    , c%htstress_coeff, numvals
     :                    , 0.0, 1.0)

         !    millet_dm_senescence

      call read_real_var (section_name
     :                    , 'dm_root_sen_frac', '()'
     :                    , c%dm_root_sen_frac, numvals
     :                    , 0.0, 1.0)

      call read_real_var (section_name
     :                    , 'dm_leaf_sen_frac', '()'
     :                    , c%dm_leaf_sen_frac, numvals
     :                    , 0.0, 1.0)

         !    millet_dm_dead_detachment

      call read_real_array (section_name
     :                    , 'dead_detach_frac', max_part, '()'
     :                    , c%dead_detach_frac, numvals
     :                    , 0.0, 1.0)

      call read_real_array (section_name
     :                    , 'sen_detach_frac', max_part, '()'
     :                    , c%sen_detach_frac, numvals
     :                    , 0.0, 1.0)

      call read_real_var (section_name
     :                    , 'dm_leaf_detach_frac', '()'
     :                    , c%dm_leaf_detach_frac, numvals
     :                    , 0.0, 1.0)

         ! TEMPLATE OPTION
         !    millet_leaf_area_devel

      call read_real_var (section_name
     :                    , 'leaf_no_correction', '()'
     :                    , c%leaf_no_correction, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'leaf_size_average', '()'
     :                    , c%leaf_size_average, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'leaf_size_endjuv', '()'
     :                    , c%leaf_size_endjuv, numvals
     :                    , 0.0, 10000.0)

         ! TEMPLATE OPTION
         !    millet_leaf_size

      call read_real_var (section_name
     :                    , 'x0_const', '()'
     :                    , c%x0_const, numvals
     :                    , -10.0, 100.0)

      call read_real_var (section_name
     :                    ,'x0_slope', '()'
     :                    , c%x0_slope, numvals
     :                    , 0.0, 100.0)

cgol lower and upper bounds amended to accept ejvo's leaf area parameters
      call read_real_var (section_name
     :                    , 'a_const', '()'
     :                    , c%a_const, numvals
     :                    , -1000.0, 1000.0)

      call read_real_var (section_name
     :                    , 'a_slope1', '()'
     :                    , c%a_slope1, numvals
     :                    , -1000.0, 1000.0)

      call read_real_var (section_name
     :                    , 'a_slope2', '()'
     :                    , c%a_slope2, numvals
     :                    , -1000.0, 1000.0)

      call read_real_var (section_name
     :                    , 'b_const', '()'
     :                    , c%b_const, numvals
     :                    , -1000.0, 1000.0)

      call read_real_var (section_name
     :                    , 'b_slope1', '()'
     :                    , c%b_slope1, numvals
     :                    , -1000.0, 1000.0)

      call read_real_var (section_name
     :                    , 'b_slope2', '()'
     :                    , c%b_slope2, numvals
     :                    , -1000.0, 1000.0)

         !    millet_tiller

cgol set maximum tiller number to 5

      call read_integer_var (section_name
     :                       , 'tiller_no_pot', '()'
     :                       , c%tiller_no_pot, numvals
     :                       , 0, 5)

      call read_char_var (section_name
     :                     , 'tiller_appearance', '()'
     :                     , c%tiller_appearance, numvals)

cgol set maximum next tiller number to 6.0

      call read_real_array (section_name
     :                     , 'x_tiller_no_next', max_table, '()'
     :                     , c%x_tiller_no_next, c%num_tiller_no_next
     :                     , 0.0, 6.0)

      call read_real_array (section_name
     :                     , 'y_tiller_tt', max_table, '(oCd)'
     :                     , c%y_tiller_tt, c%num_tiller_no_next
     :                     , 0.0, 2000.0)

      call read_real_var (section_name
     :                     , 'tiller_appearance_slope', '()'
     :                     , c%tiller_appearance_slope, numvals
     :                     , 0.0, 5.0)

      call read_real_var (section_name
     :                   , 'dm_tiller_crit', '(g/plant)'
     :                   , c%dm_tiller_crit, numvals
     :                   , 0.0, 100.0)

         ! TEMPLATE OPTION
         !    millet_leaf_area_sen_light

      call read_real_var (section_name
     :                   , 'lai_sen_light', '(m^2/m^2)'
     :                   , c%lai_sen_light, numvals
     :                   , 3.0, 20.0)

      call read_real_var (section_name
     :                    , 'sen_light_slope', '()'
     :                    , c%sen_light_slope, numvals
     :                    , 0.0, 100.0)


         ! TEMPLATE OPTION
         !    millet_leaf_area_sen_frost

      call read_real_array (section_name
     :                   , 'x_temp_senescence', max_table, '(oC)'
     :                   , c%x_temp_senescence, c%num_temp_senescence
     :                   , -20.0, 20.0)

      call read_real_array (section_name
     :                   , 'y_senescence_fac', max_table, '()'
     :                   , c%y_senescence_fac, c%num_temp_senescence
     :                   , 0.0, 1.0)

         ! TEMPLATE OPTION
         !    millet_leaf_area_sen_water

      call read_real_var (section_name
     :                    , 'sen_rate_water', '()'
     :                    , c%sen_rate_water, numvals
     :                    , 0.0, 100.0)

         !    millet_phenology_init

      call read_real_var (section_name
     :                   , 'twilight', '(o)'
     :                   , c%twilight, numvals
     :                   , -90.0, 90.0)

         ! TEMPLATE OPTION
         !    millet_heat_stress

      call read_real_var (section_name
     :                   , 'temp_grain_crit_stress', '(oC)'
     :                   , c%temp_grain_crit_stress, numvals
     :                   , 20.0, 50.0)

         !    millet_N_conc_limits

      call read_real_array (section_name
     :                     , 'x_stage_code', max_stage, '()'
     :                     , c%x_stage_code, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_crit_leaf', max_stage, '()'
     :                     , c%y_N_conc_crit_leaf, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_max_leaf', max_stage, '()'
     :                     , c%y_N_conc_max_leaf, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_min_leaf', max_stage, '()'
     :                     , c%y_N_conc_min_leaf, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_crit_stem', max_stage, '()'
     :                     , c%y_N_conc_crit_stem, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_max_stem', max_stage, '()'
     :                     , c%y_N_conc_max_stem, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_min_stem', max_stage, '()'
     :                     , c%y_N_conc_min_stem, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_crit_flower', max_stage, '()'
     :                     , c%y_N_conc_crit_flower, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_max_flower', max_stage, '()'
     :                     , c%y_N_conc_max_flower, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_min_flower', max_stage, '()'
     :                     , c%y_N_conc_min_flower, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'n_conc_crit_grain', '()'
     :                   , c%N_conc_crit_grain, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'n_conc_max_grain', '()'
     :                   , c%N_conc_max_grain, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'n_conc_min_grain', '()'
     :                   , c%N_conc_min_grain, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'n_conc_crit_root', '()'
     :                   , c%N_conc_crit_root, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'n_conc_max_root', '()'
     :                   , c%N_conc_max_root, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'n_conc_min_root', '()'
     :                   , c%N_conc_min_root, numvals
     :                   , 0.0, 100.0)

         !    millet_N_init

      call read_real_var (section_name
     :                   , 'n_leaf_init_conc', '()'
     :                   , c%N_leaf_init_conc, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'n_root_init_conc', '()'
     :                   , c%N_root_init_conc, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'n_stem_init_conc', '()'
     :                   , c%N_stem_init_conc, numvals
     :                   , 0.0, 100.0)

         !    millet_N_senescence

      call read_real_var (section_name
     :                   , 'n_leaf_sen_conc', '()'
     :                   , c%N_leaf_sen_conc, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'n_root_sen_conc', '()'
     :                   , c%N_root_sen_conc, numvals
     :                   , 0.0, 100.0)

         !    millet_nfact

      call read_real_var (section_name
     :                   , 'N_fact_photo', '()'
     :                   , c%N_fact_photo, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'N_fact_pheno', '()'
     :                   , c%N_fact_pheno, numvals
     :                   , 0.0, 100.0)

      ! Phosphorus
      ! ----------

      call read_real_array (section_name
     :                     , 'p_stage_code', max_stage, '()'
     :                     , c%p_stage_code, c%num_P_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'p_conc_max', max_stage, '()'
     :                     , c%p_conc_max, c%num_P_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'p_conc_min', max_stage, '()'
     :                     , c%p_conc_min, c%num_P_conc_stage
     :                     , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'k_pfact_photo', '()'
     :                   , c%k_pfact_photo, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'k_pfact_pheno', '()'
     :                   , c%k_pfact_pheno, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'k_pfact_expansion', '()'
     :                   , c%k_pfact_expansion, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'k_pfact_grain', '()'
     :                   , c%k_pfact_grain, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'p_uptake_factor', '()'
     :                   , c%p_uptake_factor, numvals
     :                   , 0.0, 10.0)

         !    millet_rue_reduction

      call read_real_array (section_name
     :                     , 'x_ave_temp', max_table, '(oC)'
     :                     , c%x_ave_temp, c%num_ave_temp
     :                     , 0.0, 100.0)


      call read_real_array (section_name
     :                     , 'y_stress_photo', max_table, '()'
     :                     , c%y_stress_photo, c%num_factors
     :                     , 0.0, 1.0)

         ! TEMPLATE OPTION
         !    millet_dm_grain

      call read_real_array (section_name
     :                     , 'x_temp_grain', max_table, '(oC)'
     :                     , c%x_temp_grain, c%num_temp_grain
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_grain_rate', max_table, '()'
     :                     , c%y_grain_rate, c%num_temp_grain
     :                     , 0.0, 1.0)

         !    millet_tt

      call read_real_array (section_name
     :                     , 'x_temp', max_table, '(oC)'
     :                     , c%x_temp, c%num_temp
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_tt', max_table, '(oC)'
     :                     , c%y_tt, c%num_temp
     :                     , 0.0, 100.0)
cpsc
      call read_real_array (section_name
     :                     , 'x_weighted_temp', max_table, '(oC)'
     :                     , c%x_weighted_temp, c%num_weighted_temp
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_plant_death', max_table, '(oC)'
     :                     , c%y_plant_death, c%num_weighted_temp
     :                     , 0.0, 100.0)


         !    millet_swdef

      call read_real_array (section_name
     :                     , 'x_sw_demand_ratio', max_table, '()'
     :                     , c%x_sw_demand_ratio, c%num_sw_demand_ratio
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_swdef_leaf', max_table, '()'
     :                     , c%y_swdef_leaf, c%num_sw_demand_ratio
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'x_sw_avail_ratio', max_table, '()'
     :                     , c%x_sw_avail_ratio, c%num_sw_avail_ratio
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_swdef_pheno', max_table, '()'
     :                     , c%y_swdef_pheno, c%num_sw_avail_ratio
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'x_sw_ratio', max_table, '()'
     :                     , c%x_sw_ratio, c%num_sw_ratio
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_sw_fac_root', max_table, '()'
     :                     , c%y_sw_fac_root, c%num_sw_ratio
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'x_sw_avail_fix', max_table, '()'
     :                     , c%x_sw_avail_fix, c%num_sw_avail_fix
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_swdef_fix', max_table, '()'
     :                     , c%y_swdef_fix, c%num_sw_avail_fix
     :                     , 0.0, 100.0)

      call pop_routine (my_name)
      return
      end


*     ===========================================================
      Recursive
     :subroutine millet_set_my_class (module_name)
*     ===========================================================
      use MilletModule
      implicit none
      include 'error.pub'

*+  Sub-Program Arguments
      character module_name*(*)

*+  Purpose
*       Set class type for this module.

*+  Changes
*     280598 igh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_set_my_class')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      If (module_name .eq. name_main) then
         g%stem_class = class_main
      else
         g%stem_class = class_tiller
      endif

      call pop_routine (my_name)
      return
      end


