!      include 'sugar.inc'

!     ===========================================================
      subroutine AllocInstance (InstanceName, InstanceNo)
!     ===========================================================
      use sugarModule
      implicit none

!+  Sub-Program Arguments
      character InstanceName*(*)       ! (INPUT) name of instance
      integer   InstanceNo             ! (INPUT) instance number to allocate

!+  Purpose
!      Module instantiation routine.

*+  Mission Statement
*     Instantiate routine

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
      use sugarModule
      implicit none

!+  Sub-Program Arguments
      integer anInstanceNo             ! (INPUT) instance number to allocate

!+  Purpose
!      Module de-instantiation routine.

*+  Mission Statement
*     Deinstantiate routine

!- Implementation Section ----------------------------------

      deallocate (Instances(anInstanceNo)%gptr)
      deallocate (Instances(anInstanceNo)%pptr)
      deallocate (Instances(anInstanceNo)%cptr)

      return
      end

!     ===========================================================
      subroutine SwapInstance (anInstanceNo)
!     ===========================================================
      use sugarModule
      implicit none

!+  Sub-Program Arguments
      integer anInstanceNo             ! (INPUT) instance number to allocate

!+  Purpose
!      Swap an instance into the global 'g' pointer

*+  Mission Statement
*     Swap an instance into global pointer

!- Implementation Section ----------------------------------

      g => Instances(anInstanceNo)%gptr
      p => Instances(anInstanceNo)%pptr
      c => Instances(anInstanceNo)%cptr

      return
      end

*     ================================================================
      subroutine Main (action, data_string)
*     ================================================================
      use sugarModule
      implicit none
      include   'const.inc'            ! ACTION_presence, ACTION_init, ACTION_process
      include   'event.inc'
      include   'action.inc'
      include 'crp_comm.pub'
      include 'error.pub'

*+  Sub-Program Arguments
      character  action*(*)            ! (INPUT) Message action to perform
      character  data_string*(*)       ! (INPUT) Message data

*+  Purpose
*     This is the highest level routine for the APSIM sugar module.
*     This routines handles all the input and output communications to
*     other modules in APSIM and calls model process routines for each
*     timestep when required.

*+  Mission Statement
*     Handle all communications with Sugar

*+  Changes
*      250894 jngh specified and programmed
*      050996 nih  added graze action
*      060599 sdb  removed version reference and presence action

*+  Constant Values
      character  my_name*(*)           ! name of this procedure
      parameter (my_name='sugar main')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (action.eq.ACTION_get_variable) then
            ! respond to request for variable values - from modules
         call sugar_send_my_variable (Data_string)

      elseif (action.eq.ACTION_set_variable) then
            ! respond to request to reset variable values - from modules
         call sugar_set_my_variable (data_string)

      elseif (action.eq.EVENT_tick) then
         call sugar_ONtick()

      elseif (action.eq.ACTION_prepare) then
         call sugar_prepare ()

      elseif (action.eq.ACTION_process) then
         if (g%crop_status.ne.crop_out) then
               ! do crop processes
            call sugar_process ()
         else
            ! crop not in
            call sugar_zero_variables ()
         endif
      elseif (action.eq.ACTION_sow) then
         if (crop_my_type (c%crop_type)) then
               ! start crop and do  more initialisations
            call sugar_start_crop ()
         else
            ! not my type!
            call message_unused ()
         endif
       elseif (action.eq.ACTION_harvest) then
         if (crop_my_type (c%crop_type)) then
               ! harvest crop - turn into residue
            call sugar_harvest ()
         else
            ! not my type!
            call message_unused ()
         endif

      elseif (action.eq.ACTION_end_crop) then
         if (crop_my_type (c%crop_type)) then
               ! end crop - turn into residue
            call sugar_end_crop ()
         else
            ! not my type!
            call message_unused ()
         endif

      elseif (action.eq.ACTION_kill_crop) then
         if (crop_my_type (c%crop_type)) then
               ! kill crop - die
            call sugar_kill_crop
     :               (
     :                g%crop_status
     :              , g%day_of_year
     :              , g%dm_dead
     :              , g%dm_green
     :              , g%dm_senesced
     :              , g%year
     :               )
         else
            ! not my type!
            call message_unused ()
         endif

      elseif (action.eq.'graze') then
         call sugar_graze ()
      elseif (action.eq.'hill_up') then
         call sugar_hill_up ()
      elseif (action.eq.'lodge') then
         call sugar_lodge ()

      elseif (action.eq.ACTION_init) then
            ! Get constants
         call sugar_init ()

      else if (Action.eq.ACTION_Create) then
         call sugar_zero_all_globals ()

      else
               ! don't use message
         call message_unused ()
      endif

      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_process ()
*     ===========================================================
      use sugarModule
      implicit none
      include 'error.pub'

*+  Purpose
*       Simulate crop processes.  These include biomass production,
*       phenological stages, plant component development,
*       water uptake and nitrogen uptake, plant senescense and so on.
*       This routine is called once per day during the process stage.

*+  Mission Statement
*     Perform actions for current day

*+  Changes
*      060495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_process')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      ! request and receive variables from owner-modules
      call sugar_get_met_variables ()
      call sugar_get_soil_variables ()

      call sugar_root_depth(1)
      call sugar_root_Depth_init(1) ! after because it sets the delta
      call sugar_water_supply(1)
      call sugar_water_uptake (1)
      call sugar_water_stress_expansion (1)
      call sugar_water_stress_stalk (1)
      call sugar_water_stress_pheno (1)
      call sugar_water_stress_photo (1)

      if (g%crop_status.eq.crop_alive) then
         call sugar_min_sstem_sucrose(1)
         call sugar_phenology_init (1)
         call sugar_phenology (1)
         call sugar_height (1)

         call sugar_leaf_no_init (1)
         call sugar_leaf_no_pot (1)
         call sugar_leaf_area_init (1)
         call sugar_leaf_area_potential (1)

         call sugar_bio_water (1)
         call sugar_water_log (1)
         call sugar_bio_RUE(1)
         call sugar_bio_actual (1)
         call sugar_leaf_area_stressed (1)
         call sugar_bio_partition (1)
         call sugar_bio_retrans (1)
         call sugar_leaf_actual (1)
         call sugar_root_dist (1)

         !!!! LEAF AREA SEN !!!!

         call sugar_leaf_death (1)
         call sugar_leaf_area_sen(1)

         call sugar_sen_bio (1)
         call sugar_sen_nit (1)
         call sugar_sen_root_length (1)

         call sugar_nit_retrans (1)
         call sugar_nit_Demand(1)
         call sugar_nit_supply (1)
         call sugar_nit_init (1)
         call sugar_nit_uptake (1)
         call sugar_nit_partition (1)


         call sugar_water_content_cane (1)
         call sugar_plant_death (1)
         call sugar_realloc (1)
      else
      endif

      call sugar_detachment (1)

      call sugar_cleanup()

      ! send changes to owner-modules
      call sugar_set_other_variables ()

      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_harvest ()
*     ===========================================================
      use sugarModule
      implicit none
      include   'const.inc'            ! new_line, lu_scr_sum, blank,
      include   'convert.inc'          ! gm2kg, sm2ha, sm2smm
      include 'string.pub'
      include 'data.pub'
      include 'crp_comm.pub'
      include 'error.pub'
      include 'componentinterface.inc'

*+  Purpose
*       Report occurence of harvest and the current status of specific
*       variables.

*+  Mission Statement
*     Occurance of harvest

*+  Changes
*     070495 nih taken from template
*     191099 jngh changed to sugar_Send_Crop_Chopped_Event
*     101100 dph  added eventInterface parameter to crop_root_incorp

*+  Calls
                                       ! lu_scr_sum
*
      character  string_concat*50      ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_harvest')

*+  Local Variables
      real       biomass_dead          ! above ground dead plant wt (kg/ha)
      real       biomass_green         ! above ground green plant wt (kg/ha)
      real       biomass_senesced      ! above ground senesced plant wt (kg/ha)
      character  cultivar_ratoon*30    !
      real       dm                    ! above ground total dry matter (kg/ha)
      real       leaf_no               ! total leaf number
      real       N_dead                ! above ground dead plant N (kg/ha)
      real       N_green               ! above ground green plant N (kg/ha)
      real       N_senesced            ! above ground senesced plant N (kg/ha)
      real       N_total               ! total gross nitrogen content (kg/ha)
      integer    phase                 ! phenological phase number
      real       si1                   ! mean water stress type 1
      real       si2                   ! mean water stress type 2
      real       si4                   ! mean nitrogen stress type 1
      character  string*400            ! message
*
      real       dm_root               ! (g/m^2)
      real       N_root                ! (g/m^2)
      real       dm_residue            ! (g/m^2)
      real       N_residue             ! (g/m^2)
      real       dlt_dm_crop(max_part) ! change in crop dry matter (kg/ha)
      real       dlt_dm_N(max_part)    ! N content of dry matter change (kg/ha)
      real       fraction_to_Residue(max_part)   ! fraction sent to residue (0-1)
*
      integer    layer
      real       hold_ratoon_no
      real       hold_dm_root
      real       hold_n_root
      real       hold_num_layers
      real       hold_root_depth
      real       hold_root_length(max_layer)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

          ! crop harvested. Report status

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

      leaf_no = sum_between (emerg, now, g%leaf_no)

      N_green = (sum_real_array (g%N_green, max_part)
     :        - g%N_green(root))
     :        * gm2kg / sm2ha

      N_senesced = (sum_real_array (g%N_senesced, max_part)
     :           - g%N_senesced(root))
     :           * gm2kg / sm2ha

      N_dead = (sum_real_array (g%N_dead, max_part)
     :       - g%N_dead(root))
     :       * gm2kg / sm2ha

      N_total = N_green + N_senesced + N_dead

      call write_string (new_line//new_line)

      write (string, '(a,i4)')
     :            ' flowering day  = ',g%isdate
      call write_string (string)

      write (string, '(a,f6.3)')
     :            ' maximum lai =', g%lai_max
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
     :            ' total N content (kg/ha) =', N_total
     :          , ' senesced N content (kg/ha) =', N_senesced

      call write_string (string)

      write (string, '(a,f10.2,t40,a,f10.2)')
     :            ' green N content (kg/ha) =', N_green
     :          , ' dead N content (kg/ha) =', N_dead
      call write_string (string)

      do 2000 phase = emerg_to_begcane, flowering_to_crop_end
         si1 = divide (g%cswd_photo(phase)
     :               , g%days_tot(phase), 0.0)
         si2 = divide (g%cswd_expansion(phase)
     :               , g%days_tot(phase), 0.0)
         si4 = divide (g%cnd_photo(phase)
     :               , g%days_tot(phase), 0.0)

         call write_string (new_line//new_line)

         write (string,'(2a)')
     :         ' stress indices for ', c%stage_names(phase)
         call write_string (string)

         write (string,'(2(a, g16.7e2))')
     :         ' water stress 1 =', si1
     :         , '   nitrogen stress 1 =', si4
         call write_string (string)

         write (string,'(a, g16.7e2)')
     :         ' water stress 2 =', si2
         call write_string (string)
2000  continue

      ! the following is a copy/adaption of sugar_end_crop

      if (g%crop_status.ne.crop_out) then

                ! report

             ! now do post harvest processes

         dm_root = g%dm_green(root)* c%root_die_back_fr
     :           + g%dm_dead(root)
     :           + g%dm_senesced(root)

         N_root  = g%N_green(root)* c%root_die_back_fr
     :           + g%N_dead(root)
     :           + g%N_senesced(root)


             ! put stover into surface residue

         dm_residue = (sum_real_array (g%dm_green, max_part)
     :              - g%dm_green(root) - g%dm_green(sstem)
     :              - g%dm_green(sucrose))

     :              + (sum_real_array (g%dm_senesced, max_part)
     :              - g%dm_senesced(root))

     :              + (sum_real_array (g%dm_dead, max_part)
     :              - g%dm_dead(root))
         dlt_dm_crop(:) =  (g%dm_green(:)
     :                  + g%dm_senesced(:)
     :                  + g%dm_dead(:))
     :                  * gm2kg/sm2ha
         dlt_dm_crop(root) = dm_root * gm2kg/sm2ha

         N_residue = (sum_real_array (g%N_green, max_part)
     :             - g%N_green(root) - g%N_green (sstem)
     :             - g%N_green(sucrose))

     :             + (sum_real_array (g%N_senesced, max_part)
     :             - g%N_senesced(root))

     :             + (sum_real_array (g%N_dead, max_part)
     :             - g%N_dead(root))

          dlt_dm_N(:) =  (g%N_green(:)
     :                + g%N_senesced(:)
     :                + g%N_dead(:))
     :                  * gm2kg/sm2ha
         dlt_dm_N(root) = N_root * gm2kg/sm2ha


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

         call crop_root_incorp (dm_root
     :                      ,N_root
     :                      ,g%dlayer
     :                      ,g%root_length
     :                      ,g%root_depth
     :                      ,c%crop_type
     :                      ,max_layer
     :                      ,EventInterface
     :                      )

!         call crop_top_residue (c%crop_type, dm_residue, N_residue)
         fraction_to_residue(:) = 1.0
         fraction_to_Residue(root) = 0.0
         fraction_to_Residue(sstem) = 0.0
         fraction_to_Residue(sucrose) = 0.0

         if (sum(dlt_dm_crop) .gt. 0.0) then
            call sugar_Send_Crop_Chopped_Event
     :                (c%crop_type
     :               , part_name
     :               , dlt_dm_crop
     :               , dlt_dm_N
     :               , fraction_to_Residue
     :               , max_part)
         else
            ! no surface residue
         endif


         hold_ratoon_no = g%ratoon_no
         hold_dm_root   = g%dm_green (root)*(1.0 - c%root_die_back_fr)
         hold_n_root    = g%N_green (root)*(1.0 - c%root_die_back_fr)
         hold_num_layers= g%num_layers
         hold_root_depth= g%root_depth
         do 101 layer=1,max_layer
            hold_root_length(layer) = g%root_length(layer)
     :                                *(1.0 - c%root_die_back_fr)
  101    continue


         call sugar_zero_globals ()
         call sugar_zero_daily_variables ()

         g%current_stage   = real (sprouting)
         g%ratoon_no       = hold_ratoon_no +1
         g%dm_green (root) = hold_dm_root
         g%N_green (root)  = hold_n_root
         g%num_layers      = hold_num_layers
         g%root_depth      = hold_root_depth
         g%plants          = g%initial_plant_density

         do 102 layer=1,max_layer
            g%root_length(layer) = hold_root_length(layer)
  102    continue

         ! now update constants if need be
         If (g%ratoon_no .eq. 1) then

            call sugar_read_crop_constants ('ratoon_crop')

            cultivar_ratoon = string_concat(g%crop_cultivar
     :                                     ,'_ratoon')
            call sugar_read_cultivar_params (cultivar_ratoon)

         else
            ! only need to update constants when we move from a plant
            ! crop to a ratoon crop.
         endif
      else
      endif

      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_zero_all_globals ()
*     ===========================================================
      use sugarModule
      implicit none
      include 'const.inc'
      include 'error.pub'

*+  Purpose
*       Zero crop variables & arrays

*+  Mission Statement
*     Zero crop variables and arrays

*+  Changes
*     060495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_zero_all_globals')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      g%crop_status           = blank
      g%crop_cultivar         = blank
      g%sowing_depth          = 0.0
      g%year                  = 0
      g%day_of_year           = 0
      g%sw_avail_fac_deepest_layer = 0.0
      g%temp_stress_photo     = 0.0
      g%temp_stress_stalk     = 0.0
      g%swdef_expansion       = 0.0
      g%swdef_stalk           = 0.0
      g%swdef_photo           = 0.0
      g%swdef_pheno           = 0.0
      g%swdef_fixation        = 0.0
      g%nfact_expansion       = 0.0
      g%nfact_stalk           = 0.0
      g%nfact_photo           = 0.0
      g%nfact_pheno           = 0.0
      g%sucrose_fraction      = 0.0
      g%oxdef_photo           = 0.0
      g%lodge_redn_photo      = 0.0
      g%fr_intc_radn          = 0.0
      g%latitude              = 0.0
      g%mint                  = 0.0
      g%maxt                  = 0.0
      g%eo                    = 0.0
      g%cnd_photo (:) = 0.0
      g%cswd_photo (:)     = 0.0
      g%cswd_expansion (:) = 0.0
      g%cswd_pheno (:)     = 0.0
      g%dlt_tt                     = 0.0
      g%tt_tot(:)          = 0.0
      g%phase_tt(:)        = 0.0
      g%dlt_stage                  = 0.0
      g%current_stage              = 0.0
      g%previous_stage             = 0.0
      g%days_tot (:)       = 0.0
      g%dlt_canopy_height          = 0.0
      g%canopy_height              = 0.0
      g%phase_devel                = 0.0
      g%ratoon_no                = 0
      g%plants                     = 0.0
      g%dlt_plants                 = 0.0
      g%initial_plant_density      = 0.0
      g%dlt_root_depth             = 0.0
      g%root_depth                 = 0.0
      g%lodge_flag                  = .false.
      g%rue                         = 0.0
      g%uptake_water(:)    = 0.0
      g%num_uptake_water          = 0
      g%num_layers                = 0
      g%transpiration_tot       = 0.0
      g%N_uptake_tot            = 0.0
      g%N_demand_tot            = 0.0
      g%N_conc_act_stover_tot   = 0.0
      g%N_conc_crit_stover_tot  = 0.0
      g%N_uptake_stover_tot     = 0.0
      g%lai_max                 = 0.0
      g%isdate                  = 0
      g%mdate                   = 0
      g%dm_graze                = 0.0
      g%n_graze                 = 0.0
      g%plant_wc(:)          = 0.0
      g%dlt_plant_wc(:)      = 0.0
      g%uptake_source              = blank
      g%dlayer (:)        = 0.0
      g%dlt_sw_dep(:)     = 0.0
      g%sat_dep (:)       = 0.0
      g%dul_dep (:)       = 0.0
      g%ll15_dep (:)      = 0.0
      g%sw_dep (:)        = 0.0
      g%st (:)            = 0.0
      g%sw_demand                 = 0.0
      g%sw_demand_te      = 0.0
      g%sw_avail_pot(:)   = 0.0
      g%sw_avail(:)       = 0.0
      g%sw_supply (:)               = 0.0
      g%dlt_root_length(:)          = 0.0
      g%dlt_root_length_senesced(:) = 0.0
      g%root_length (:)             = 0.0
      g%dlt_plants_death_drought            = 0.0
      g%dlt_plants_failure_leaf_sen         = 0.0
      g%dlt_plants_failure_emergence        = 0.0
      g%dlt_plants_failure_germ             = 0.0
      g%dlt_plants_death_lodging            = 0.0
      g%dlt_dm                              = 0.0
      g%dlt_dm_green(:)              = 0.0
      g%dlt_dm_green_pot(:)          = 0.0
      g%dlt_dm_senesced(:)           = 0.0
      g%dlt_dm_realloc (:)           = 0.0
      g%dlt_dm_detached(:)           = 0.0
      g%dlt_dm_dead_detached(:)      = 0.0
      g%dlt_dm_green_retrans(:)      = 0.0
      g%dm_stress_max(:)            = 0.0
      g%dlt_dm_stress_max                   = 0.0
      g%dm_green_demand(:)           = 0.0
      g%dm_dead(:)                   = 0.0
      g%dm_green(:)                  = 0.0
      g%dm_senesced(:)               = 0.0
      g%dm_plant_top_tot(:)         = 0.0
      g%partition_xs                        = 0.0
      g%partition_xs_pot                    = 0.0
      g%dlt_dm_pot_rue                      = 0.0
      g%dlt_dm_pot_te                       = 0.0
      g%dlt_dm_pot_rue_pot                  = 0.0
      g%radn_int                            = 0.0
      g%transp_eff                          = 0.0
      g%min_sstem_sucrose                   = 0.0
      g%dlt_min_sstem_sucrose               = 0.0
      g%slai                                = 0.0
      g%dlt_slai                            = 0.0
      g%dlt_lai                             = 0.0
      g%dlt_lai_pot                         = 0.0
      g%dlt_lai_stressed                    = 0.0
      g%lai                                 = 0.0
      g%tlai_dead                           = 0.0
      g%dlt_slai_detached                   = 0.0
      g%dlt_tlai_dead_detached              = 0.0
      g%dlt_tlai_dead                       = 0.0
      g%dlt_slai_age                        = 0.0
      g%dlt_slai_light                      = 0.0
      g%dlt_slai_water                      = 0.0
      g%dlt_slai_frost                      = 0.0
      g%sla_min                             = 0.0
      g%leaf_no(:)                  = 0.0
      g%node_no(:)                  = 0.0
      g%node_no_dead(:)             = 0.0
      g%dlt_leaf_no                         = 0.0
      g%dlt_node_no                         = 0.0
      g%dlt_node_no_dead                    = 0.0
      g%leaf_no_final                       = 0.0
      g%leaf_area(:)                 = 0.0
      g%leaf_dm(:)                   = 0.0
      g%node_no_detached                    = 0.0
      g%N_demand (:)                 = 0.0
      g%N_max(:)                     = 0.0
      g%dlt_N_green(:)               = 0.0
      g%dlt_N_senesced(:)            = 0.0
      g%dlt_n_realloc (:)            = 0.0
      g%dlt_N_detached(:)            = 0.0
      g%dlt_N_dead_detached(:)       = 0.0
      g%N_dead(:)                    = 0.0
      g%N_green(:)                   = 0.0
      g%N_senesced(:)                = 0.0
      g%dlt_N_retrans(:)             = 0.0
      g%dlt_NO3gsm(:)               = 0.0
      g%NO3gsm (:)                  = 0.0
      g%NO3gsm_min(:)               = 0.0
      g%uptake_no3(:)               = 0.0
      g%NO3gsm_diffn_pot(:)         = 0.0
      g%NO3gsm_mflow_avail(:)       = 0.0
      g%n_fix_pot                           = 0.0
      g%num_uptake_no3               = 0
      g%N_conc_crit(:)               = 0.0
      g%N_conc_max(:)                = 0.0
      g%N_conc_min(:)                = 0.0
      g%dm_plant_min(:)              = 0.0

      p%tt_emerg_to_begcane                    = 0.0
      p%tt_begcane_to_flowering                = 0.0
      p%tt_flowering_to_crop_end               = 0.0
      p%kl(:)                       = 0.0
      p%xf(:)                       = 0.0
      p%ll_dep(:)                   = 0.0

      c%stage_names(:)          = blank
      c%crop_type               = blank
      c%x_sw_ratio (:)           = 0.0
      c%y_sw_fac_root (:)        = 0.0
      c%x_sw_demand_ratio (:)    = 0.0
      c%x_demand_ratio_stalk (:) = 0.0
      c%y_swdef_leaf (:)         = 0.0
      c%y_swdef_stalk (:)        = 0.0
      c%x_sw_avail_ratio (:)     = 0.0
      c%y_swdef_pheno (:)        = 0.0
      c%k_nfact_photo                    = 0.0
      c%k_nfact_expansion                = 0.0
      c%k_nfact_stalk                    = 0.0
      c%k_nfact_pheno                    = 0.0
      c%num_sw_ratio           = 0
      c%num_sw_demand_ratio    = 0
      c%num_demand_ratio_Stalk = 0
      c%num_sw_avail_ratio     = 0
      c%leaf_no_crit           = 0.0
      c%tt_emerg_limit                   = 0.0
      c%days_germ_limit                  = 0.0
      c%swdf_pheno_limit                 = 0.0
      c%swdf_photo_limit                 = 0.0
      c%swdf_photo_rate                  = 0.0
      c%initial_root_depth               = 0.0
      c%sla_max(:)               = 0.0
      c%sla_min(:)               = 0.0
      c%sla_lfno(:)              = 0.0
      c%initial_tpla                     = 0.0
      c%x_stem_wt(:)              = 0.0
      c%y_height (:)             = 0.0
      c%svp_fract                        = 0.0
      c%transp_eff_cf(:)         = 0.0
      c%n_fix_rate(:)            = 0.0
      c%pesw_germ                        = 0.0
      c%fasw_emerg(:)            = 0.0
      c%rel_emerg_rate(:)        = 0.0
      c%leaf_no_at_emerg                 = 0.0
      c%NO3_diffn_const                  = 0.0
      c%shoot_lag                        = 0.0
      c%shoot_rate                       = 0.0
      c%y_leaves_per_node(:)     = 0.0
      c%y_node_app_rate(:)       = 0.0
      c%x_node_no_app(:)         = 0.0
      c%x_node_no_leaf(:)        = 0.0
      c%dm_leaf_init                     = 0.0
      c%dm_root_init                     = 0.0
      c%dm_sstem_init                    = 0.0
      c%dm_cabbage_init                  = 0.0
      c%dm_sucrose_init                  = 0.0
      c%leaf_cabbage_ratio               = 0.0
      c%cabbage_sheath_fr                = 0.0
      c%dm_root_sen_frac                 = 0.0
      c%dead_detach_frac(:)       = 0.0
      c%sen_detach_frac(:)        = 0.0
      c%minsw                            = 0.0
      c%lai_sen_light                    = 0.0
      c%frost_kill                       = 0.0
      c%sen_rate_water                   = 0.0
      c%sen_light_slope                  = 0.0
      c%frost_temp(:)            = 0.0
      c%frost_fraction(:)        = 0.0
      c%oxdef_photo_rtfr(:)      = 0.0
      c%oxdef_photo(:)           = 0.0
      c%num_frost_temp              = 0
      c%num_oxdef_photo             = 0
      c%num_stress_factor_Stalk     = 0
      c%green_leaf_no                          = 0.0
      c%cane_Fraction                          = 0.0
      c%sucrose_fraction_stalk(:)      = 0.0
      c%stress_factor_stalk(:)         = 0.0
      c%sucrose_delay                          = 0.0
      c%min_sstem_sucrose                      = 0.0
      c%min_sstem_sucrose_redn                 = 0.0
      c%num_dead_lfno                   = 0
      c%leaf_no_correction               = 0.0
      c%leaf_size(:)             = 0.0
      c%leaf_size_no(:)          = 0.0
      c%num_leaf_size                = 0
      c%tillerf_leaf_size(:)     = 0.0
      c%tillerf_leaf_size_no(:)  = 0.0
      c%num_tillerf_leaf_size        = 0
      c%x_ave_temp(:)            = 0.0
      c%x_ave_temp_stalk(:)      = 0.0
      c%y_stress_photo(:)        = 0.0
      c%y_stress_stalk(:)        = 0.0
      c%x_temp(:)                = 0.0
      c%y_tt(:)                  = 0.0
      c%stress_lodge(:)          = 0.0
      c%death_fr_lodge(:)        = 0.0
      c%lodge_redn_photo         = 0.0
      c%x_plant_rld (:)           = 0.0
      c%y_rel_root_rate (:)       = 0.0
      c%num_temp                     = 0
      c%num_ave_temp                 = 0
      c%num_ave_temp_stalk           = 0
      c%num_factors                  = 0
      c%num_node_no_app              = 0
      c%num_node_no_leaf             = 0
      c%num_sla_lfno                 = 0
      c%num_x_swdef_cellxp           = 0
      c%num_stress_lodge             = 0
      c%num_fasw_emerg               = 0
      c%num_plant_rld                = 0
      c%num_stem_wt                  = 0
      c%tt_emerg_to_begcane_ub           = 0.0
      c%tt_begcane_to_flowering_ub       = 0.0
      c%tt_flowering_to_crop_end_ub      = 0.0
      c%ll_ub                            = 0.0
      c%kl_ub                            = 0.0
      c%sw_dep_ub                        = 0.0
      c%sw_dep_lb                        = 0.0
      c%NO3_ub                           = 0.0
      c%NO3_lb                           = 0.0
      c%NO3_min_ub                       = 0.0
      c%NO3_min_lb                       = 0.0
      c%leaf_no_min                      = 0.0
      c%leaf_no_max                      = 0.0
      c%latitude_ub                         = 0.0
      c%latitude_lb                         = 0.0
      c%maxt_ub                             = 0.0
      c%maxt_lb                             = 0.0
      c%mint_ub                             = 0.0
      c%mint_lb                             = 0.0
      c%radn_ub                             = 0.0
      c%radn_lb                             = 0.0
      c%dlayer_ub                           = 0.0
      c%dlayer_lb                           = 0.0
      c%dul_dep_ub                          = 0.0
      c%dul_dep_lb                          = 0.0
      c%n_supply_preference               = blank
      c%cane_dmf_min(:)            = 0.0
      c%cane_dmf_max(:)            = 0.0
      c%cane_dmf_tt(:)             = 0.0
      c%cane_dmf_rate                      = 0.0
      c%num_cane_dmf                = 0
      c%N_conc_crit_root                 = 0.0
      c%N_conc_min_root                  = 0.0
      c%x_stage_code(:)          = 0.0
      c%y_n_conc_crit_leaf(:)    = 0.0
      c%y_n_conc_min_leaf(:)     = 0.0
      c%y_n_conc_crit_cane(:)    = 0.0
      c%y_n_conc_min_cane(:)     = 0.0
      c%y_n_conc_crit_cabbage(:) = 0.0
      c%y_n_conc_min_cabbage(:)  = 0.0
      c%N_root_init_conc                 = 0.0
      c%N_sstem_init_conc                = 0.0
      c%N_leaf_init_conc                 = 0.0
      c%N_cabbage_init_conc              = 0.0
      c%N_leaf_sen_conc                  = 0.0
      c%N_cabbage_sen_conc               = 0.0
      c%N_root_sen_conc                  = 0.0
      c%num_N_conc_stage           = 0
      c%extinction_coef                  = 0.0
      c%extinction_coef_dead             = 0.0
      c%rue(:)                   = 0.0
      c%root_depth_rate(:)       = 0.0
      c%ratio_root_shoot(:)      = 0.0
      c%root_die_back_fr                 = 0.0
      c%specific_root_length             = 0.0
      c%stage_code_list(:)       = 0.0
      c%twilight                         = 0.0


      call pop_routine (my_name)
      return
      end
*     ===========================================================
      subroutine sugar_zero_variables ()
*     ===========================================================
      use sugarModule
      implicit none
      include 'error.pub'

*+  Purpose
*       Zero crop variables & arrays

*+  Mission Statement
*     Zero crop variables and arrays

*+  Changes
*     060495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_zero_variables')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

          ! zero pools etc.
      call sugar_zero_globals ()
      call sugar_zero_daily_variables ()
      call sugar_zero_parameters ()

      call pop_routine (my_name)
      return
      end

*     ===========================================================
      subroutine sugar_zero_soil_globals ()
*     ===========================================================
      use sugarModule
      implicit none
      include 'data.pub'
      include 'error.pub'

*+  Purpose
*       Zero soil variables & arrays

*+  Mission Statement
*     Zero soil global variables and arrays



*+  Changes
*     210199 NIH

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_zero_soil_globals')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call fill_real_array (g%dlayer, 0.0, max_layer)
      call fill_real_array (g%sat_dep, 0.0, max_layer)
      call fill_real_array (g%dul_dep, 0.0, max_layer)
      call fill_real_array (g%ll15_dep, 0.0, max_layer)
      call fill_real_array (g%sw_dep, 0.0, max_layer)
      call fill_real_array (g%st, 0.0, max_layer)

      g%num_layers = 0

      call pop_routine (my_name)
      return
      end


*     ===========================================================
      subroutine sugar_zero_daily_variables ()
*     ===========================================================
      use sugarModule
      implicit none
      include 'data.pub'
      include 'error.pub'

*+  Purpose
*       Zero crop daily variables & arrays

*+  Mission Statement
*     Zero daily variables and arrays

*+  Changes
*     060495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_zero_daily_variables')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

          ! zero pools etc.

      call fill_real_array (g%dlt_dm_green, 0.0, max_part)
      call fill_real_array (g%dlt_dm_green_retrans, 0.0, max_part)
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


      g%dlt_tlai_dead_detached = 0.0
      g%dlt_slai_detached = 0.0
      g%dlt_canopy_height = 0.0
      g%dlt_dm = 0.0
      g%partition_xs = 0.0
      g%dlt_leaf_no = 0.0
      g%dlt_node_no = 0.0
      g%dlt_node_no_dead = 0.0
      g%dlt_plants = 0.0
      g%dlt_root_depth = 0.0
      g%dlt_slai = 0.0
      g%dlt_stage = 0.0
      g%dlt_lai = 0.0
      g%dlt_tt = 0.0

      g%sw_demand = 0.0
      g%dm_graze = 0.0
      g%n_graze = 0.0

      g%dlt_min_sstem_sucrose = 0.0

      g%temp_stress_photo = 0.0
      g%temp_stress_stalk = 0.0
      g%swdef_expansion = 0.0
      g%swdef_stalk = 0.0
      g%swdef_photo = 0.0
      g%swdef_pheno = 0.0
      g%swdef_fixation = 0.0
      g%nfact_expansion = 0.0
      g%nfact_stalk = 0.0
      g%nfact_photo = 0.0
      g%nfact_pheno = 0.0
      g%oxdef_photo = 0.0
      g%lodge_redn_photo = 0.0

      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_init ()
*     ===========================================================
      use sugarModule
      implicit none
      include 'error.pub'

*+  Purpose
*       Crop initialisation

*+  Mission Statement
*     Crop initialisation

*+  Changes
*     060495 nih taken from template
*     060599 sdb  removed version reference

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_init')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call sugar_zero_variables ()
      call sugar_zero_soil_globals()

cnh      call report_date_and_event (g%day_of_year,g%year,
cnh     :                 ' Initialising')
      call Write_string (' Initialising')

           ! initialize crop variables

      call sugar_read_constants ()

      g%current_stage = real (crop_end)
      g%crop_status = crop_out

      call sugar_get_met_variables ()
      call sugar_get_soil_variables ()

      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_start_crop ()
*     ===========================================================
      use sugarModule
      implicit none
      include   'const.inc'            ! lu_scr_sum, blank
      include 'string.pub'
      include 'intrface.pub'
      include 'error.pub'

*+  Purpose
*       Start crop using parameters specified in passed record

*+  Mission Statement
*     Start the crop based on passed parameters

*+  Changes
*     060495 nih taken from template
*     041095 nih changed start of ratton crop from emergence to sprouting
*     060696 nih changed extract routines to collect routine calls
*                removed datastring from argument list

*+  Calls
      character string_concat*50       ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_start_crop')

*+  Local Variables
      character  cultivar*20           ! name of cultivar
      integer    numvals               ! number of values found in array
      character  string*200            ! output string
      character  cultivar_ratoon*30    ! name of cultivar ratoon section

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      ! request and receive variables from owner-modules
      call sugar_get_met_variables ()
      call sugar_get_soil_variables ()

      call write_string ( 'Sowing initiate')


         call collect_real_var ('plants', '()'
     :                        , g%plants, numvals, 0.0, 100.0)
         g%initial_plant_density = g%plants

         call collect_integer_var_optional ('ratoon', '()'
     :                        , g%ratoon_no, numvals, 0, 10)
         if (numvals.eq.0) then
            g%ratoon_no = 0
         else
         endif

         call collect_real_var ('sowing_depth', '(mm)'
     :                        , g%sowing_depth, numvals
     :                        , 0.0, 500.0)

         call collect_char_var ('cultivar', '()'
     :                        , cultivar, numvals)

             ! report

         call write_string (new_line//new_line)

         string = '                 Crop Sowing Data'
         call write_string (string)

         string = '    ------------------------------------------------'
         call write_string (string)

         call write_string ('    Sowing  Depth Plants Cultivar')

         call write_string ('    Day no   mm     m^2    Name   ')

         string = '    ------------------------------------------------'
         call write_string (string)

         write (string, '(3x, i7, 2f7.1, 1x, a10)')
     :                   g%day_of_year, g%sowing_depth
     :                 , g%plants, cultivar
         call write_string (string)

         string = '    ------------------------------------------------'
         call write_string (string)

                 ! get cultivar parameters

         if (g%ratoon_no .eq. 0) then
            call sugar_read_crop_constants ('plant_crop')
            call sugar_read_cultivar_params (cultivar)
         else
            call sugar_read_crop_constants ('ratoon_crop')
            cultivar_ratoon = string_concat(cultivar,'_ratoon')
            call sugar_read_cultivar_params (cultivar_ratoon)
         endif

                 ! get root profile parameters

         call sugar_read_root_params ()

         if (g%ratoon_no.eq.0) then
            g%current_stage = real (sowing)
         else
            g%current_stage = real (sowing)
         endif

         g%crop_status = crop_alive
         g%crop_cultivar = cultivar

      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_read_cultivar_params (section_name)
*     ===========================================================
      use sugarModule
      implicit none
      include   'const.inc'
      include 'read.pub'
      include 'error.pub'

*+  Sub-Program Arguments
      character section_name*(*)

*+  Purpose
*       Crop initialisation - reads constants from constants file

*+  Mission Statement
*     Crop initialisation - read cultivar parameters

*+  Changes
*       25-07-96 - NIH/MJR added sucrose/water stress partitioning factor

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_read_cultivar_params')

*+  Local Variables
      integer    numvals               ! number of values returned

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call write_string (new_line//
     :   '    - Reading constants from '//section_name)

         !    sugar_leaf_size

      call read_real_array (section_name
     :                     , 'leaf_size', max_table, '()'
     :                     , c%leaf_size, c%num_leaf_size
     :                     , 1000.0, 100000.0)

      call read_real_array (section_name
     :                     , 'leaf_size_no', max_table, '()'
     :                     , c%leaf_size_no, c%num_leaf_size
     :                     , 0.0, real(max_leaf))

      call read_real_var (section_name
     :                    , 'cane_fraction', '()'
     :                    , c%cane_fraction, numvals
     :                    , 0.0, 1.0)

      call read_real_array (section_name
     :                    , 'sucrose_fraction_stalk', max_table,'()'
     :                    , c%sucrose_fraction_stalk, numvals
     :                    , 0.0, 1.0)

      call read_real_array (section_name
     :                    , 'stress_factor_stalk', max_table,'()'
     :                    , c%stress_factor_stalk
     :                    , c%num_stress_Factor_stalk
     :                    , 0.0, 1.0)

      call read_real_var (section_name
     :                    , 'sucrose_delay', '()'
     :                    , c%sucrose_delay, numvals
     :                    , 0.0, 2000.)

      call read_real_var (section_name
     :                    , 'min_sstem_sucrose', '(g/m2)'
     :                    , c%min_sstem_sucrose, numvals
     :                    , 0.0, 5000.)

      call read_real_var (section_name
     :                    , 'min_sstem_sucrose_redn', '(g/m2)'
     :                    , c%min_sstem_sucrose_redn, numvals
     :                    , 0.0, 5000.)

      call read_real_var (section_name
     :                    , 'tt_emerg_to_begcane', '()'
     :                    , p%tt_emerg_to_begcane, numvals
     :                    , 0.0, c%tt_emerg_to_begcane_ub)

      call read_real_var (section_name
     :                    , 'tt_begcane_to_flowering', '()'
     :                    , p%tt_begcane_to_flowering, numvals
     :                    , 0.0, c%tt_begcane_to_flowering_ub)

      call read_real_var (section_name
     :                    , 'tt_flowering_to_crop_end', '()'
     :                    , p%tt_flowering_to_crop_end, numvals
     :                    , 0.0, c%tt_flowering_to_crop_end_ub)

         !    sugar_leaf_death


      call read_real_var  (section_name
     :                    , 'green_leaf_no', '()'
     :                    , c%green_leaf_no, numvals
     :                    , 0.0, real(max_leaf))

         !    sugar_leaf_size

      call read_real_array (section_name
     :                     , 'tillerf_leaf_size', max_table, '()'
     :                     , c%tillerf_leaf_size,c%num_tillerf_leaf_size
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                  , 'tillerf_leaf_size_no', max_table, '()'
     :                  , c%tillerf_leaf_size_no,c%num_tillerf_leaf_size
     :                  , 0.0, real(max_leaf))


      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_read_root_params ()
*     ===========================================================
      use sugarModule
      implicit none
      include   'const.inc'            ! new_line, lu_scr_sum, blank,
      include 'data.pub'
      include 'read.pub'
      include 'error.pub'

*+  Purpose
*       Get root profile parameters

*+  Mission Statement
*     Crop initialisation - get root profile parameters

*+  Changes
*       060495 nih taken from template

*+  Calls
                                       ! lu_scr_sum, Err_User

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_read_root_params')
*
      character  section_name*(*)
      parameter (section_name = 'parameters')

*+  Local Variables
      integer    layer                 ! layer number
      real       ll (max_layer)        ! lower limit of plant-extractable
                                       ! soil water for soil layer l
                                       ! (mm water/mm soil)
      integer    num_layers            ! number of layers in profile
      integer    numvals               !
      character  string*200            ! output string
      real       rlv (max_layer)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call write_string (new_line
     :       //'   - Reading root profile parameters')

         !       cproc_sw_demand_bound

      call read_real_var_optional (section_name
     :                     , 'eo_crop_factor', '()'
     :                     , p%eo_crop_factor, numvals
     :                     , 0.0, 100.)
      if (numvals.le.0) then
         p%eo_crop_factor = c%eo_crop_factor_default
      else
      endif


      call read_char_var   (section_name
     :                     , 'uptake_source'
     :                     , '()'
     :                     , g%uptake_source
     :                     , numvals)

      call read_real_array (section_name
     :                     , 'xf', max_layer, '()'
     :                     , p%xf, num_layers
     :                     , 0.0, 1.0)

         !       sugar_sw_supply

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
     :                     , 'rlv', max_layer, '()'
     :                     , rlv, num_layers
     :                     , 0.0, 20.0)
      call fill_real_array (g%root_depth, 0.0, max_layer)
      do 1001 layer = 1, num_layers
         g%root_length(layer) = rlv(layer)*g%dlayer(layer)
1001  continue

      if (g%uptake_source.eq.'calc') then

          ! report
         call write_string (new_line//new_line)
         call write_string
     :     ('Sugar module is calculating its own soil uptakes')
         call write_string (new_line//new_line)

      else if (g%uptake_source .eq. 'apsim') then

          ! report
         call write_string (new_line//new_line)
         call write_string
     :      ('Sugar module is using uptakes'//
     :       ' provided from another module')
         call write_string (new_line//new_line)

      else
         ! the user has not specified 'calc' or 'apsim'
         ! so give out an error message
         call fatal_error (ERR_USER, 'Bad value for uptake_source')
      endif

         write (string,'(4x, a)') '                Root Profile'
         call write_string (string)

         string = '  --------------------------------------------------'
         call write_string (string)

         string = '    Layer depth  Kl factor   Lower limit Root Factor'
         call write_string (string)

         string = '         (mm)         ()        (mm/mm)     (0-1)'
         call write_string (string)

         string = '  --------------------------------------------------'
         call write_string (string)

         do 2000 layer = 1, num_layers
            write (string,'(1x, 4f12.3)')
     :            g%dlayer(layer)
     :          , p%kl(layer)
     :          , ll(layer)
     :          , p%xf(layer)
            call write_string (string)
2000     continue

         string = '   -------------------------------------------------'
         call write_string (string)

         call write_string (new_line)

      write (string,'(2x,a,f5.1,a)')
     :          'Crop factor for bounding water use is set to '
     :        , p%eo_crop_factor
     :        , ' times Eo'

      call write_string (string)

         call write_string (new_line//new_line)


      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_kill_crop
     :               (
     :                g_crop_status
     :              , g_day_of_year
     :              , G_dm_dead
     :              , G_dm_green
     :              , G_dm_senesced
     :              , G_year
     :               )
*     ===========================================================
      implicit none
      include   'convert.inc'          ! gm2kg, sm2ha, mm2cm, cmm2cc
      include   'sugconst.inc'
      include 'data.pub'
      include 'error.pub'

*+  Sub-Program Arguments
      CHARACTER  G_crop_status   *(*)  ! (INPUT)
      INTEGER    G_day_of_year         ! (INPUT)  day of year
      REAL       G_dm_dead(*)          ! (INPUT)  dry wt of dead plants (g/m^2)
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass) (g/m^2)
      REAL       G_dm_senesced(*)      ! (INPUT)  senesced plant dry wt (g/m^2)
      INTEGER    G_year                ! (INPUT)  year

*+  Purpose
*       Kill crop

*+  Mission Statement
*     Crop death due to killing

*+  Changes
*       070495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_kill_crop')

*+  Local Variables
      real       biomass               ! above ground dm (kg/ha)
      character  string*200            ! output string

*- Implementation Section ----------------------------------

c+!!!!!! fix problem with deltas in update when change from alive to dead ?zero
      call push_routine (my_name)

      if (g_crop_status.eq.crop_alive) then
         g_crop_status = crop_dead

         biomass = (sum_real_array (g_dm_green, max_part)
     :           - g_dm_green(root)) * gm2kg /sm2ha

     :           + (sum_real_array (g_dm_senesced, max_part)
     :           - g_dm_senesced(root)) * gm2kg /sm2ha

     :           + (sum_real_array (g_dm_dead, max_part)
     :           - g_dm_dead(root)) * gm2kg /sm2ha


                ! report

         write (string, '(3x, a, f7.1, a)')
     :                  ' crop_kill. Standing above-ground dm = '
     :                  , biomass, ' (kg/ha)'
         call write_string (string)

      else
      endif

      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_end_crop ()
*     ===========================================================
      use sugarModule
      implicit none
      include   'const.inc'            ! new_line, lu_scr_sum
      include   'convert.inc'          ! gm2kg, sm2ha, mm2cm, cmm2cc
      include 'data.pub'
      include 'crp_comm.pub'
      include 'error.pub'
      include 'componentinterface.inc'

*+  Purpose
*       End crop

*+  Mission Statement
*     End the crop

*+  Changes
*       070495 nih taken from template
*       191099 jngh changed to sugar_Send_Crop_Chopped_Event
*     101100 dph  added eventInterface parameter to crop_root_incorp

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_end_crop')

*+  Local Variables
      real       dm_residue            ! dry matter added to residue (g/m^2)
      real       N_residue             ! nitrogen added to residue (g/m^2)
      real       dm_root               ! dry matter added to soil (g/m^2)
      real       N_root                ! nitrogen added to soil (g/m^2)
      character  string*400            ! output string
      real       fraction_to_Residue(max_part)   ! fraction sent to residue (0-1)
      real       dlt_dm_crop(max_part) ! change in crop dry matter (kg/ha)
      real       dlt_dm_N(max_part)    ! N content of dry matter change (kg/ha)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (g%crop_status.ne.crop_out) then
         g%crop_status = crop_out
         g%current_stage = real (crop_end)

                ! report

             ! now do post harvest processes

         dm_root = g%dm_green(root)
     :           + g%dm_dead(root)
     :           + g%dm_senesced(root)

         N_root  = g%N_green(root)
     :           + g%N_dead(root)
     :           + g%N_senesced(root)

         call crop_root_incorp (dm_root
     :                      ,N_root
     :                      ,g%dlayer
     :                      ,g%root_length
     :                      ,g%root_depth
     :                      ,c%crop_type
     :                      ,max_layer
     :                      ,EventInterface
     :                      )


             ! put stover into surface residue

         dm_residue = (sum_real_array (g%dm_green, max_part)
     :              - g%dm_green(root))

     :              + (sum_real_array (g%dm_senesced, max_part)
     :              - g%dm_senesced(root))

     :              + (sum_real_array (g%dm_dead, max_part)
     :              - g%dm_dead(root))

         dlt_dm_crop(:) = (g%dm_green(:)
     :                  + g%dm_senesced(:)
     :                  + g%dm_dead(:))
     :                  * gm2kg/sm2ha

         N_residue = (sum_real_array (g%N_green, max_part)
     :             - g%N_green(root))

     :             + (sum_real_array (g%N_senesced, max_part)
     :             - g%N_senesced(root))

     :             + (sum_real_array (g%N_dead, max_part)
     :             - g%N_dead(root))

         dlt_dm_N(:) = (g%N_green(:)
     :               + g%N_senesced(:)
     :               + g%N_dead(:))
     :               * gm2kg/sm2ha

!         call crop_top_residue (c%crop_type, dm_residue, N_residue)
         fraction_to_residue(:) = 1.0
         fraction_to_Residue(root) = 0.0

         if (sum(dlt_dm_crop) .gt. 0.0) then
            call sugar_Send_Crop_Chopped_Event
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



*     ================================================================
      subroutine sugar_get_met_variables ()
*     ================================================================
      use sugarModule
      implicit none
      include   'const.inc'
      include 'intrface.pub'
      include 'error.pub'

*+  Purpose
*      Get the values of variables/arrays from other modules.

*+  Mission Statement
*     Get the values of variables/arrays from the met module

*+  Changes
*     230399 nih taken from get_other_variables

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_get_met_variables')

*+  Local Variables
      integer    numvals               ! number of values put into array
      character  mod_name*12           ! module name

*- Implementation Section ----------------------------------

      call push_routine (my_name)


      ! INPUT module
      ! ------------
      call get_real_var (unknown_module, 'latitude', '(oL)'
     :                                  , g%latitude, numvals
     :                                  , c%latitude_lb, c%latitude_ub)
      call get_real_var (unknown_module, 'maxt', '(oC)'
     :                                  , g%maxt, numvals
     :                                  , c%maxt_lb, c%maxt_ub)
      call get_real_var (unknown_module, 'mint', '(oC)'
     :                                  , g%mint, numvals
     :                                  , c%mint_lb, c%mint_ub)
      call get_real_var (unknown_module, 'radn', '(Mj/m^2)'
     :                                  , g%radn, numvals
     :                                  , c%radn_lb, c%radn_ub)
      call get_real_var (unknown_module, 'eo', '(mm)'
     :                                  , g%eo, numvals
     :                                  , 0.0, 20.)
      ! Canopy Module
      ! -------------
      call get_current_module (mod_name)
      call get_real_var_optional (unknown_module
     :                           , 'fr_intc_radn_'//mod_name
     :                           , '()'
     :                           , g%fr_intc_radn
     :                           , numvals
     :                           , 0.0
     :                           , 1.0)

      call pop_routine (my_name)
      return
      end

*     ================================================================
      subroutine sugar_get_soil_variables ()
*     ================================================================
      use sugarModule
      implicit none
      include   'const.inc'
      include   'convert.inc'
      include 'data.pub'
      include 'intrface.pub'
      include 'error.pub'
      include 'crp_root.pub'

*+  Purpose
*      Get the values of variables/arrays from soil modules.

*+  Mission Statement
*     Get the values of variables from soil modules

*+  Changes
*     230399 nih taken from get_other_variables

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_get_soil_variables')

*+  Local Variables
      integer    layer                 ! layer number
      integer    numvals               ! number of values put into array
      real       dlayer(max_layer)     ! soil layer depths (mm)
      real       NO3(max_layer)        ! soil NO3 content (kg/ha)
      real       NO3_min(max_layer)    ! soil NO3 minimum (kg/ha)
      real       profile_depth
      real       root_depth_new

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      ! Soil Water module
      ! -----------------
      call get_real_array (unknown_module, 'dlayer', max_layer
     :                                    , '(mm)'
     :                                    , dlayer, numvals
     :                                    , c%dlayer_lb, c%dlayer_ub)

      if (g%num_layers.eq.0) then
            ! we assume dlayer hasn't been initialised yet.
         call add_real_array (dlayer, g%dlayer, numvals)
         g%num_layers = numvals

      else

           ! dlayer may be changed from its last setting
           ! due to erosion

         profile_depth = sum_real_array (dlayer, numvals)

         if (g%root_depth.gt.profile_depth) then
            root_depth_new = profile_depth
            call crop_root_redistribute
     :                        ( g%root_length
     :                        , g%root_depth
     :                        , g%dlayer
     :                        , g%num_layers
     :                        , root_depth_new
     :                        , dlayer
     :                        , numvals)

            g%root_depth = root_depth_new
         else
            ! roots are ok.
         endif

         do 1000 layer = 1, numvals
            ! What happens if crop not in ground and p%ll_dep is empty
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
      call get_real_array (unknown_module, 'sat_dep', max_layer
     :                                    , '(mm)'
     :                                    , g%sat_dep, numvals
     :                                    , c%sw_dep_lb, c%sw_dep_ub)
      call get_real_array (unknown_module, 'll15_dep', max_layer
     :                                    , '(mm)'
     :                                    , g%ll15_dep, numvals
     :                                    , c%sw_dep_lb, c%sw_dep_ub)

      ! soil nitrogen module
      ! --------------------
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

c      call get_real_array (unknown_module, 'st', max_layer
c     :                                    , '(oC)'
c     :                                    , g%st, numvals
c     :                                    , -10., 80.)

      call pop_routine (my_name)
      return
      end



*     ================================================================
      subroutine sugar_set_other_variables ()
*     ================================================================
      use sugarModule
      implicit none
      include   'const.inc'
      include   'convert.inc'
      include 'action.inc'
      include 'postbox.pub'
      include 'data.pub'
      include 'intrface.pub'
      include 'error.pub'

*+  Purpose
*      Set the value of a variable or array in other module/s.

*+  Mission Statement
*     Set value of variable or array in other module/s

*+  Notes
*      a flag is set if any of the totals is requested.  The totals are
*      reset during the next process phase when this happens.

*+  Changes
*     070495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_set_other_variables')

*+  Local Variables
      real       dlt_NO3(max_layer)    ! soil NO3 change (kg/ha)
      integer    layer                 ! soil layer no.
      integer    num_layers            ! number of layers

*- Implementation Section ----------------------------------

      call push_routine (my_name)

c      call sugar_update_other_variables ()

      if (g%uptake_source.eq.'calc') then
         num_layers = count_of_real_vals (g%dlayer, max_layer)

         do 1000 layer = 1, num_layers
            dlt_NO3(layer) = g%dlt_NO3gsm(layer) * gm2kg /sm2ha
1000     continue

         call set_real_array (unknown_module, 'dlt_no3', '(kg/ha)'
     :                    , dlt_NO3, num_layers)

         call set_real_array (unknown_module, 'dlt_sw_dep', '(mm)'
     :                    , g%dlt_sw_dep, num_layers)

      else
         ! assume that the module that calculated uptake has also
         ! updated these pools.
      endif

      call pop_routine (my_name)
      return
      end



*     ===============================================================
      subroutine sugar_set_my_variable (Variable_name)
*     ===============================================================
      use sugarModule
      implicit none
      include 'const.inc'
      include 'intrface.pub'
      include 'error.pub'

*+  Sub-Program Arguments
      character  Variable_name*(*)     ! (INPUT) Variable name to search for

*+  Purpose
*      Set a variable in this module as requested by another.

*+  Mission Statement
*     Reset an internal variable as specified by another module

*+  Changes
*      060495 nih - taken from template
*      060696 nih - changed respond2set routines to collect routines

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_set_my_variable')

*+  Local Variables
      integer numvals

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (variable_name .eq. 'plants') then

         call collect_real_var (variable_name
     :                         ,'(m-2)'
     :                         ,g%plants
     :                         ,numvals
     :                         ,0.0
     :                         ,1000.)

        if (g%current_stage.gt.emerg) then
           call warning_error (ERR_User,
     :            'You have updated plant number after emergence')
        else
        endif

      elseif (variable_name .eq. 'lodge_redn_photo') then

         call collect_real_var (variable_name
     :                         ,'(-)'
     :                         ,c%lodge_redn_photo
     :                         ,numvals
     :                         ,0.0
     :                         ,1.0)

      elseif (variable_name .eq. 'lodge_redn_sucrose') then

         call collect_real_var (variable_name
     :                         ,'(-)'
     :                         ,c%lodge_redn_sucrose
     :                         ,numvals
     :                         ,0.0
     :                         ,1.0)

      elseif (variable_name .eq. 'lodge_redn_green_leaf') then

         call collect_real_var (variable_name
     :                         ,'(-)'
     :                         ,c%lodge_redn_green_leaf
     :                         ,numvals
     :                         ,0.0
     :                         ,1.0)


      else
            ! Don't know this variable name
            call message_unused ()
      endif

      call pop_routine (my_name)
      return
      end



*     ================================================================
      subroutine sugar_send_my_variable (variable_name)
*     ================================================================
      use sugarModule
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

*+  Mission Statement
*     Provide data to a requesting module

*+  Changes
*      060495 nih - taken from template

*+  Calls
      real       sugar_profile_fasw    ! function
cbak

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_send_my_variable')

*+  Local Variables
      real       act_N_up              ! cumulative total N uptake by plant
                                       ! (kg/ha)
      real       biomass               ! above ground biomass (alive+dead)
      real       biomass_n             ! N in above ground biomass (alive+dead)
      real       cane_dmf              !
      real       cane_wt               ! cane weight (sstem + sucrose)
      real       ccs                   ! commercial cane sugar(g/g)
      real       cover                 ! crop cover fraction (0-1)
      real       das                   ! days after sowing
      integer    das1                  ! days after sowing (rounded integer of das)
      real       radn_int              ! daily radn intercepted (MJ)
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      real       fasw
      real       lai_sum               ! leaf area index of all leaf material
                                       ! live + dead
      real       lai_dead              ! dead leaf area index
                                       ! (m^2 leaf/m^2 soil)
      integer    num_layers            ! number of layers in profile
      integer    stage_no              ! current stage no.
      real       NO3gsm_tot            ! total NO3 in the root profile (g/m^2)
      real       N_demand              ! sum N demand for plant parts (g/m^2)
      real       N_supply              ! N supply for grain (g/m^2)
cbak
      real       conc_n_leaf           ! Current N concentration in leaves
      real       conc_n_cab            ! Current N concentration in cabbage
      real       conc_n_cane           ! Current N concentration in cane
*
*
      real       n_leaf_crit           ! Weight of N in leaves at the critical c
      real       n_leaf_min            ! Weight of N in leaves at the min concen
      real       green_biomass_n       ! Weight of N in green tops (g/m^2)
      real       plant_n_tot           ! Total plant N including roots (g/m2)
cmjr
      real       canefw                ! Weight of fresh cane at 30% dry matter
      real       scmstf                ! sucrose conc in fresh millable stalk
      real       scmst                 ! sucrose conc in dry millable stalk
      real       temp
      real       tla
      integer    layer
      real       rwu(max_layer)        ! root water uptake (mm)
      real       rlv(max_layer)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (variable_name .eq. 'crop_status') then
         call respond2get_char_var (variable_name
     :                             , '()'
     :                             , g%crop_status)

      elseif (variable_name .eq. 'stage') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%current_stage)

      elseif (variable_name .eq. 'stage_code') then
         stage_no = int (g%current_stage)
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , c%stage_code_list(stage_no))

      elseif (variable_name .eq. 'stage_name') then
         stage_no = int (g%current_stage)
         call respond2get_char_var (variable_name
     :                             , '()'
     :                             , c%stage_names(stage_no))

      elseif (variable_name .eq. 'crop_type') then
cnh         if (c%crop_type.ne.' ') then
            call respond2get_char_var (variable_name
     :                             , '()'
     :                             , c%crop_type)
cnh         else
cnh             call message_unused ()
cnh         endif

      elseif (variable_name .eq. 'plants') then
         call respond2get_real_var (variable_name
     :                             , '(/m2)'
     :                             , g%plants)

      elseif (variable_name .eq. 'ratoon_no') then
         call respond2get_integer_var (variable_name
     :                             , '()'
     :                             , g%ratoon_no)

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

c      elseif (variable_name .eq. 'days_tot') then
c I removed this NIH

      elseif (variable_name .eq. 'leaf_no') then
         call respond2get_real_array (variable_name
     :                              , '()'
     :                              , g%leaf_no
     :                              , max_stage)
      elseif (variable_name .eq. 'node_no_dead') then
         call respond2get_real_array (variable_name
     :                              , '()'
     :                              , g%node_no_dead
     :                              , max_stage)

      elseif (variable_name .eq. 'leaf_area') then
         call respond2get_real_array (variable_name
     :                              , '()'
     :                              , g%leaf_area
     :                              , max_leaf)

      elseif (variable_name .eq. 'leaf_dm') then
         call respond2get_real_array (variable_name
     :                              , '()'
     :                              , g%leaf_dm
     :                              , max_leaf)

      elseif (variable_name .eq. 'height') then
         call respond2get_real_var (variable_name
     :                             , '(mm)'
     :                             , g%canopy_height)

      elseif (variable_name .eq. 'root_depth') then
         call respond2get_real_var (variable_name
     :                             , '(mm)'
     :                             , g%root_depth)

      elseif (variable_name .eq. 'cover_green') then
         cover = 1.0 - exp (-c%extinction_coef * g%lai)

         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , cover)

      elseif (variable_name .eq. 'radn_int') then
         cover = 1.0 - exp (-c%extinction_coef * g%lai)
         radn_int = cover * g%radn
         call respond2get_real_var (variable_name
     :                             , '(mj/m2)'
     :                             , radn_int)

      elseif (variable_name .eq. 'cover_tot') then
         lai_dead = g%slai + g%tlai_dead
         cover = 1.0
     :         - exp (-c%extinction_coef * g%lai
     :                -c%extinction_coef_dead * lai_dead)

         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , cover)

      elseif (variable_name .eq. 'lai_sum') then
         lai_sum = g%lai + g%slai + g%tlai_dead
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , lai_sum)

      elseif (variable_name .eq. 'tlai') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%lai + g%slai)

      elseif (variable_name .eq. 'tla') then
         tla = sum_real_array (g%leaf_area, max_leaf)
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , tla)

      elseif (variable_name .eq. 'slai') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%slai)

      elseif (variable_name .eq. 'lai') then
         call respond2get_real_var (variable_name
     :                             , '(m^2/m^2)'
     :                             , g%lai)

            ! plant biomass

      elseif (variable_name .eq. 'root_wt') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%dm_green(root))

      elseif (variable_name .eq. 'leaf_wt') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%dm_green(leaf))

      elseif (variable_name .eq. 'sstem_wt') then
         ! Add dead pool for lodged crops
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%dm_green(sstem)
     :                              +g%dm_dead(sstem))

      elseif (variable_name .eq. 'canefw') then
         ! Add dead pool for lodged crops
         canefw = (g%dm_green(sstem) + g%dm_green(sucrose)
     :          +  g%dm_dead(sstem) + g%dm_dead(sucrose)
     :          + g%plant_wc(sstem))*g2t/sm2ha
         call respond2get_real_var (variable_name
     :                             , '(t/ha)'
     :                             , canefw)

      elseif (variable_name .eq. 'ccs') then
         canefw = (g%dm_green(sstem) + g%dm_green(sucrose)
     :          + g%plant_wc(sstem))
         scmstf = divide(g%dm_green(sucrose),canefw,0.0)
         ccs = 1.23*scmstf - 0.029
         ccs = l_bound(ccs,0.0)
         ccs = ccs * 100.          ! convert to %
         call respond2get_real_var (variable_name
     :                             , '(%)'
     :                             , ccs)

      elseif (variable_name .eq. 'scmstf') then
         canefw = (g%dm_green(sstem) + g%dm_green(sucrose)
     :          + g%plant_wc(sstem))
         scmstf = divide(g%dm_green(sucrose),canefw,0.0)
         call respond2get_real_var (variable_name
     :                             , '(g/g)'
     :                             , scmstf)

      elseif (variable_name .eq. 'scmst') then
         cane_wt = g%dm_green(sstem) + g%dm_green(sucrose)
         scmst = divide(g%dm_green(sucrose),cane_wt,0.0)
         call respond2get_real_var (variable_name
     :                             , '(g/g)'
     :                             , scmst)

      elseif (variable_name .eq. 'sucrose_wt') then
         ! Add dead pool to allow for lodged stalks
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%dm_green(sucrose)
     :                              +g%dm_dead(sucrose))

      elseif (variable_name .eq. 'cabbage_wt') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%dm_green(cabbage))

      elseif (variable_name .eq. 'cane_wt') then
         ! Add dead pool for lodged crops
         cane_wt = g%dm_green(sstem)+g%dm_green(sucrose)
     :           + g%dm_dead(sstem)+g%dm_dead(sucrose)

         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , cane_wt)

      elseif (variable_name .eq. 'biomass') then

         biomass =
     :        sum_Real_array(g%dm_green,max_part)-g%dm_green(root)
     :      + sum_real_array(g%dm_senesced,max_part)-g%dm_senesced(root)
     :      + sum_real_array(g%dm_dead,max_part)-g%dm_dead(root)

         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , biomass)
 

      elseif (variable_name .eq. 'green_biomass') then
         ! Add dead pool for lodged crops
         biomass =
     :        sum_Real_array(g%dm_green,max_part)-g%dm_green(root)
     :           + g%dm_dead(sstem)+g%dm_dead(sucrose)

         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , biomass)

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

      elseif (variable_name .eq. 'dlt_dm') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%dlt_dm)

      elseif (variable_name .eq. 'partition_xs') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%partition_xs)

      elseif (variable_name .eq. 'dlt_dm_green') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g%dlt_dm_green
     :                             , max_part)

      elseif (variable_name .eq. 'dlt_dm_detached') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g%dlt_dm_detached
     :                             , max_part)

cbak Reporting of N concentrations

      elseif (variable_name .eq. 'n_critical') then
         call respond2get_real_array (variable_name
     :                             , '(g/g)'
     :                             , g%N_conc_crit
     :                             , max_part)

      elseif (variable_name .eq. 'n_minimum') then
         call respond2get_real_array (variable_name
     :                             , '(g/g)'
     :                             , g%N_conc_min
     :                             , max_part)

cbak
      elseif (variable_name .eq. 'n_conc_leaf') then
        Conc_N_leaf = divide (g%N_green(leaf) ,g%dm_green(leaf), 0.0)
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , Conc_N_leaf)

      elseif (variable_name .eq. 'n_conc_cab') then
      Conc_N_cab = divide (g%N_green(cabbage), g%dm_green(cabbage), 0.0)
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , Conc_N_cab)

      elseif (variable_name .eq. 'n_conc_cane') then
        Conc_N_cane = divide (g%N_green(sstem)+g%N_green(sucrose),
     :                 g%dm_green(sstem)+g%dm_green(sucrose), 0.0)
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , Conc_N_cane)

cbak Weights of N in plant

      elseif (variable_name .eq. 'n_leaf_crit') then
       N_leaf_crit = g%N_conc_crit(leaf) * g%dm_green(leaf)

         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , N_leaf_crit)

      elseif (variable_name .eq. 'n_leaf_min') then
       N_leaf_min = g%N_conc_min(leaf) * g%dm_green(leaf)

         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , N_leaf_min)


      elseif (variable_name .eq. 'biomass_n') then
         biomass_n =
     :        sum_Real_array(g%n_green,max_part)-g%n_green(root)
     :      + sum_real_array(g%n_senesced,max_part)-g%n_senesced(root)
     :      + sum_real_array(g%n_dead,max_part)-g%n_dead(root)
cbak
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , biomass_n)


      elseif (variable_name .eq. 'plant_n_tot') then
         plant_n_tot =
     :        sum_Real_array(g%n_green,max_part)
     :      + sum_real_array(g%n_senesced,max_part)
     :      + sum_real_array(g%n_dead,max_part)

         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , plant_n_tot)


      elseif (variable_name .eq. 'green_biomass_n') then
         green_biomass_n =
     :        sum_Real_array(g%n_green,max_part)-g%n_green(root)
cbak
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , green_biomass_n)


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

cbak   Delta N in plant tops

      elseif (variable_name .eq. 'dlt_n_green') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g%dlt_N_green
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

      elseif (variable_name .eq. 'swdef_stalk') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%swdef_stalk)

      elseif (variable_name .eq. 'nfact_photo') then
cjhtemp
c      call sugar_nit_stress_photo (1)
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%nfact_photo)

      elseif (variable_name .eq. 'lodge_redn_photo') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%lodge_redn_photo)

      elseif (variable_name .eq. 'lodge_redn_sucrose') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%lodge_redn_sucrose)

      elseif (variable_name .eq. 'lodge_redn_green_leaf') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%lodge_redn_green_leaf)

cbak
      elseif (variable_name .eq. 'nfact_expan') then
cjhtemp
c      call sugar_nit_stress_expansion (1)
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%nfact_expansion)

      elseif (variable_name .eq. 'ep') then
         num_layers = count_of_real_vals (g%dlayer, max_layer)
         do 10 layer = 1, num_layers
            rwu(layer) = - g%dlt_sw_dep(layer)
   10    continue
         call respond2get_real_array (variable_name
     :                               , '(mm)'
     :                               , rwu
     :                               , num_layers)

      elseif (variable_name .eq. 'cep') then
         call respond2get_real_var (variable_name
     :                             , '(mm)'
     :                             , - g%transpiration_tot)

      elseif (variable_name .eq. 'sw_demand') then
         call respond2get_real_var (variable_name
     :                             , '(mm)'
     :                             , g%sw_demand)

      elseif (variable_name .eq. 'sw_demand_te') then
         call respond2get_real_var (variable_name
     :                             , '(mm)'
     :                             , g%sw_demand_te)

      elseif (variable_name .eq. 'fasw') then
         fasw = sugar_profile_fasw ()
         call respond2get_real_var (variable_name
     :                             , '(0-1)'
     :                             , fasw)

      elseif (variable_name .eq. 'cane_dmf') then
         cane_dmf = divide (
     :                      g%dm_green(sstem)+g%dm_green(sucrose)
     :                     ,g%dm_green(sstem)+g%dm_green(sucrose)
     :                                       +g%plant_wc(sstem)
     :                     ,0.0)
         call respond2get_real_var (variable_name
     :                             , '(0-1)'
     :                             , cane_dmf)

      elseif (variable_name .eq. 'oxdef_photo') then
         call respond2get_real_var (variable_name
     :                             , '(0-1)'
     :                             , g%oxdef_photo)

      elseif (variable_name .eq. 'das') then
         das = sum_between (sowing, now, g%days_tot)
         das1 = nint(das)
         call respond2get_integer_var (variable_name
     :                             , '(days)'
     :                             , das1)

            ! plant nitrogen

      elseif (variable_name .eq. 'n_uptake') then
         act_N_up = g%N_uptake_tot*gm2kg /sm2ha
         call respond2get_real_var (variable_name
     :                             , '(kg/ha)'
     :                             , act_N_up)

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
      elseif (variable_name .eq. 'no3_demand') then
         N_demand = sum_real_array (g%N_demand, max_part)*10.

         call respond2get_real_var (variable_name
     :                             , '(kg/ha)'
     :                             , N_demand)

      elseif (variable_name .eq. 'n_supply') then
         N_supply = sum_real_array ( g%dlt_N_green, max_part)
     :            - g%dlt_N_green(root)
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , N_supply)


      elseif (variable_name .eq. 'rlv') then
         num_layers = count_of_real_vals (g%dlayer, max_layer)
         do 2000 layer = 1, num_layers
            rlv(layer) = divide (g%root_length(layer)
     :                          ,g%dlayer(layer)
     :                          ,0.0)
 2000    continue
         call respond2get_real_array (variable_name
     :                               , '(mm/mm3)'
     :                               , rlv
     :                               , num_layers)

      elseif (variable_name .eq. 'll_dep') then
         num_layers = count_of_real_vals (g%dlayer, max_layer)
         call respond2get_real_array (variable_name
     :                               , '(mm)'
     :                               , p%ll_dep
     :                               , num_layers)

      elseif (variable_name .eq. 'dm_graze') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%dm_graze)

      elseif (variable_name .eq. 'n_graze') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%N_graze)
      elseif (variable_name .eq. 'lai2') then
         temp = sum_real_array(g%leaf_area,max_leaf)
         temp = temp * g%plants / 1000000.
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , temp)
      elseif (variable_name .eq. 'leaf_wt2') then
         temp = sum_real_array(g%leaf_dm,max_leaf)
         temp = temp * g%plants
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , temp)

      else
         ! not my variable
         call message_unused ()
      endif

      call pop_routine (my_name)
      return
      end



*     ===========================================================
      real function sugar_stage_code
     :               (
     :                C_stage_code_list
     :              , G_phase_tt
     :              , G_tt_tot
     :              , stage_no, stage_table, numvals
     :               )
*     ===========================================================
      implicit none
      include   'const.inc'            ! err_user
      include   'sugconst.inc'
      include 'science.pub'
      include 'data.pub'
      include 'error.pub'

*+  Sub-Program Arguments
      REAL       C_stage_code_list(*)  ! (INPUT)  list of stage numbers
      REAL       G_phase_tt(*)         ! (INPUT)  Cumulative growing degree days required for each stage (deg days)
      REAL       G_tt_tot(*)           ! (INPUT)  the sum of growing degree days for a phenological stage (oC d)
      real       stage_no              ! (INPUT) stage number to convert
      real       stage_table(*)        ! (INPUT) table of stage codes
      integer    numvals               ! (INPUT) size_of of table

*+  Purpose
*       Return an interpolated stage code from a table of stage_codes
*       and a nominated stage number. Returns 0 if the stage number is not
*       found. Interpolation is done on thermal time.

*+  Mission Statement
*     Get thermal time interpolated stage code from table

*+  Changes
*       070495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_stage_code')

*+  Local Variables
      real       phase_tt              ! required thermal time between stages
                                       ! (oC)
      character  error_message*100     ! error message
      real       fraction_of           !
      integer    i                     ! array index - counter
      integer    next_stage            ! next stage number to use
      real       tt_tot                ! elapsed thermal time between stages
                                       ! (oC)
      integer    this_stage            ! this stage to use
      real       x_stage_code          ! interpolated stage code

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (numvals.ge.2) then
            ! we have a valid table
         this_stage = stage_no_of (stage_table(1)
     :                           , c_stage_code_list, max_stage)

         do 1000 i = 2, numvals
            next_stage = stage_no_of (stage_table(i)
     :                              , c_stage_code_list, max_stage)

            if (stage_is_between (this_stage, next_stage, stage_no))
     :         then
                  ! we have found its place
               tt_tot = sum_between (this_stage, next_stage, g_tt_tot)
               phase_tt = sum_between (this_stage, next_stage
     :                               , g_phase_tt)
               fraction_of = divide (tt_tot, phase_tt, 0.0)
               x_stage_code = stage_table(i-1)
     :                      + (stage_table(i) - stage_table(i-1))
     :                      * fraction_of
               goto 2000

            else
               x_stage_code = 0.0
               this_stage = next_stage

            endif
1000     continue
2000     continue
      else
            ! we have no valid table

         x_stage_code = 0.0

         write (error_message,'(a, i10)')
     :               'Invalid lookup table - number of values ='
     :              , numvals
         call warning_error (err_user, error_message)

      endif
      sugar_stage_code = x_stage_code

      call pop_routine (my_name)

      return
      end



*     ===========================================================
      subroutine sugar_read_constants ()
*     ===========================================================
            use sugarModule
      implicit none
      include   'const.inc'
      include 'read.pub'
      include 'error.pub'

*+  Purpose
*       Crop initialisation - reads constants from constants file

*+  Mission Statement
*     Read constants from constants file

*+  Changes
*     060495 nih taken from template
*     020998 sb deleted c%year_lb and c%year_ub.

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_read_constants')
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

      call read_real_var (section_name
     :                    , 'tt_emerg_to_begcane_ub', '()'
     :                    , c%tt_emerg_to_begcane_ub, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'tt_begcane_to_flowering_ub', '()'
     :                    , c%tt_begcane_to_flowering_ub, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'tt_flowering_to_crop_end_ub', '()'
     :                    , c%tt_flowering_to_crop_end_ub, numvals
     :                    , 0.0, 10000.0)

         !    sugar_N_uptake

      call read_real_var (section_name
     :                    , 'no3_diffn_const', '(days)'
     :                    , c%NO3_diffn_const, numvals
     :                    , 0.0, 100.0)

      call read_char_var (section_name
     :                   , 'n_supply_preference', '()'
     :                   , c%n_supply_preference, numvals)

         !    sugar_get_root_params

      call read_real_var (section_name
     :                    , 'll_ub', '()'
     :                    , c%ll_ub, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'kl_ub', '()'
     :                    , c%kl_ub, numvals
     :                    , 0.0, 1000.0)

         !    sugar_watck

      call read_real_var (section_name
     :                    , 'minsw', '()'
     :                    , c%minsw, numvals
     :                    , 0.0, 1000.0)


         !    sugar_get_other_variables

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


      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_zero_globals ()
*     ===========================================================
      use sugarModule
      implicit none
      include 'data.pub'
      include 'error.pub'

*+  Purpose
*       Zero global variables and arrays

*+  Mission Statement
*     Zero global variables and arrays

*+  Changes
*     150595 nih created from sugar_zero_variables

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_zero_globals')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

          ! zero pools etc.

      call fill_real_array (g%cnd_photo, 0.0, max_stage)
      call fill_real_array (g%cswd_expansion, 0.0, max_stage)
      call fill_real_array (g%cswd_pheno, 0.0, max_stage)
      call fill_real_array (g%cswd_photo, 0.0, max_stage)
      call fill_real_array (g%days_tot, 0.0, max_stage)
      call fill_real_array (g%dm_dead, 0.0, max_part)
      call fill_real_array (g%dm_green, 0.0, max_part)
      call fill_real_array (g%dm_plant_min, 0.0, max_part)
      call fill_real_array (g%plant_wc, 0.0, max_part)
      call fill_real_array (g%dm_plant_top_tot, 0.0, max_stage)
      call fill_real_array (g%leaf_area, 0.0, max_leaf)
      call fill_real_array (g%leaf_dm, 0.0, max_leaf)
      call fill_real_array (g%leaf_no, 0.0, max_stage)
      call fill_real_array (g%node_no, 0.0, max_stage)
      call fill_real_array (g%node_no_dead, 0.0, max_stage)
      call fill_real_array (g%N_conc_crit, 0.0, max_part)
      call fill_real_array (g%N_conc_min, 0.0, max_part)
      call fill_real_array (g%N_green, 0.0, max_part)
      call fill_real_array (g%phase_tt, 0.0, max_stage)
      call fill_real_array (g%tt_tot, 0.0, max_stage)
      call fill_real_array (g%dm_senesced, 0.0, max_part)
      call fill_real_array (g%N_dead, 0.0, max_part)
      call fill_real_array (g%N_senesced, 0.0, max_part)
      call fill_real_array (g%root_length, 0.0, max_layer)
      call fill_real_array (g%dlt_plant_wc, 0.0, max_part)


      g%canopy_height = 0.0
      g%isdate = 0
      g%mdate = 0
      g%leaf_no_final = 0.0
      g%lai_max = 0.0
      g%N_conc_act_stover_tot = 0.0
      g%N_conc_crit_stover_tot = 0.0
      g%N_demand_tot = 0.0
      g%N_uptake_stover_tot = 0.0
      g%N_uptake_tot = 0.0
      g%plants = 0.0
cnh      g%initial_plant_density = 0.0
      g%root_depth = 0.0
      g%sowing_depth = 0.0
      g%slai = 0.0
      g%lai = 0.0
      g%transpiration_tot = 0.0
      g%previous_stage = 0.0
      g%ratoon_no = 0
      g%node_no_detached = 0.0
      g%lodge_flag = .false.
      g%min_sstem_sucrose = 0.0
      g%lodge_redn_sucrose = 0.0
      g%lodge_redn_green_leaf = 0.0

      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_zero_parameters ()
*     ===========================================================
      use sugarModule
      implicit none
      include 'data.pub'
      include 'error.pub'

*+  Purpose
*       Zero parameter variables and arrays

*+  Mission Statement
*     Zero parameter variables and arrays

*+  Changes
*     150595 nih created from sugar_zero_variables

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_zero_parameters')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

          ! zero pools etc.

      call fill_real_array (p%ll_dep, 0.0, max_layer)
      call fill_real_array (p%xf, 0.0, max_layer)
      call fill_real_array (p%kl, 0.0, max_layer)

      g%uptake_source = ' '
cnh      c%crop_type = ' '

      call pop_routine (my_name)
      return
      end



* ====================================================================
       subroutine sugar_prepare ()
* ====================================================================
      use sugarModule
      implicit none
      include 'error.pub'

*+  Purpose
*     APSim allows modules to perform calculations in preparation for
*     the standard APSim timestep.  This model uses this opportunity
*     to calculate potential growth variables for the coming day
*     and phenological development.

*+  Mission Statement
*     Perform preparatory calculations for the next timestep

*+  Changes
*   neilh - 05-07-1995 - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'sugar_prepare')

*- Implementation Section ----------------------------------
      call push_routine (myname)


      if (g%crop_status.eq.crop_alive) then
         call sugar_get_met_variables ()
         call sugar_get_soil_variables ()

         call sugar_nit_stress_photo (1)
         call sugar_nit_stress_expansion (1)
         call sugar_nit_stress_pheno (1)
         call sugar_nit_stress_stalk (1)

         call sugar_temp_stress_photo(1)
         call sugar_temp_stress_stalk(1)

         call sugar_lodge_redn_photo(1)
         call sugar_lodge_redn_sucrose(1)
         call sugar_lodge_redn_green_leaf(1)

         call sugar_light_supply(1)
         call sugar_water_log (1)
         call sugar_bio_RUE(1)
         call sugar_transpiration_eff(1)
         call sugar_water_demand (1)
         call sugar_nit_demand_est (1)

      else
         call sugar_get_soil_variables ()

      endif

      call pop_routine (myname)
      return
      end



*     ===========================================================
      subroutine sugar_read_crop_constants (section_name)
*     ===========================================================
      use sugarModule
      implicit none
      include   'const.inc'
      include 'read.pub'
      include 'error.pub'

*+  Sub-Program Arguments
      character section_name*(*)

*+  Purpose
*       Crop initialisation - reads constants from constants file

*+  Mission Statement
*     Crop initialisation - read crop constants

*+  Changes
*     <insert here>

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_read_crop_constants')

*+  Local Variables
      integer    numvals               ! number of values returned

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call write_string (new_line//
     :      '    - Reading constants from '//section_name)

         !    sugar_get_cultivar_params

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

      call read_real_array (section_name
     :                     , 'transp_eff_cf', max_stage, '()'
     :                     , c%transp_eff_cf, numvals
     :                     , 0.0, 1.0)

      call read_real_array (section_name
     :                     , 'n_fix_rate', max_stage, '()'
     :                     , c%n_fix_rate, numvals
     :                     , 0.0, 1.0)

      call read_real_var (section_name
     :                    , 'extinction_coef', '()'
     :                    , c%extinction_coef, numvals
     :                    , 0.0, 10.0)

      call read_real_var (section_name
     :                    , 'extinction_coef_dead', '()'
     :                    , c%extinction_coef_dead, numvals
     :                    , 0.0, 10.0)

         ! crop failure

      call read_real_var (section_name
     :                    , 'leaf_no_crit', '()'
     :                    , c%leaf_no_crit, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'tt_emerg_limit', '(oC)'
     :                    , c%tt_emerg_limit, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'days_germ_limit', '(days)'
     :                    , c%days_germ_limit, numvals
     :                    , 0.0, 365.0)

      call read_real_var (section_name
     :                    , 'swdf_pheno_limit', '()'
     :                    , c%swdf_pheno_limit, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'swdf_photo_limit', '()'
     :                    , c%swdf_photo_limit, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'swdf_photo_rate', '()'
     :                    , c%swdf_photo_rate, numvals
     :                    , 0.0, 1.0)


         !    sugar_root_depth

      call read_real_var (section_name
     :                    , 'initial_root_depth', '(mm)'
     :                    , c%initial_root_depth, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'specific_root_length', '(mm)'
     :                    , c%specific_root_length, numvals
     :                    , 0.0, 50000.0)

      call read_real_array (section_name
     :                     , 'x_plant_rld', max_table, '(mm/mm3/plant)'
     :                     , c%x_plant_rld, c%num_plant_rld
     :                     , 0.0, 0.1)

      call read_real_array (section_name
     :                     , 'y_rel_root_rate', max_table, '(0-1)'
     :                     , c%y_rel_root_rate, c%num_plant_rld
     :                     , 0.0, 1.0)

      call read_real_var (section_name
     :                    , 'root_die_back_fr', '(0-1)'
     :                    , c%root_die_back_fr, numvals
     :                    , 0.0, 1.0)

         !    sugar_leaf_area_init

      call read_real_var (section_name
     :                    , 'initial_tpla', '(mm^2)'
     :                    , c%initial_tpla, numvals
     :                    , 0.0, 100000.0)

         !    sugar_leaf_area_devel
      call read_real_array (section_name
     :                    , 'sla_lfno',max_table,'()'
     :                    , c%sla_lfno, c%num_sla_lfno
     :                    , 0.0, 100.0)

      call read_real_array (section_name
     :                    , 'sla_max',max_table,'(mm^2/g)'
     :                    , c%sla_max, numvals
     :                    , 0.0, 50000.0)

      call read_real_array (section_name
     :                    , 'sla_min',max_table,'(mm^2/g)'
     :                    , c%sla_min, numvals
     :                    , 0.0, 50000.0)

         !    sugar_height

      call read_real_array (section_name
     :                    , 'x_stem_wt',max_table,'(g/plant)'
     :                    , c%x_stem_wt, c%num_stem_wt
     :                    , 0.0, 10000.0)

      call read_real_array (section_name
     :                    , 'y_height',max_table,'(mm)'
     :                    , c%y_height, c%num_stem_wt
     :                    , 0.0, 10000.0)

         !    sugar_transp_eff

      call read_real_var (section_name
     :                    , 'svp_fract', '()'
     :                    , c%svp_fract, numvals
     :                    , 0.0, 1.0)


         !    cproc_sw_demand_bound

      call read_real_var (section_name
     :                    , 'eo_crop_factor_default', '()'
     :                    , c%eo_crop_factor_default, numvals
     :                    , 0.0, 100.)

         !    sugar_germination

      call read_real_var (section_name
     :                    , 'pesw_germ', '(mm/mm)'
     :                    , c%pesw_germ, numvals
     :                    , 0.0, 1.0)

      call read_real_array (section_name
     :                    , 'fasw_emerg', max_table,'(0-1)'
     :                    , c%fasw_emerg
     :                    , c%num_fasw_emerg
     :                    , 0.0, 1.0)
      call read_real_array (section_name
     :                    , 'rel_emerg_rate', max_table,'(0-1)'
     :                    , c%rel_emerg_rate
     :                    , c%num_fasw_emerg
     :                    , 0.0, 1.0)

         !    sugar_leaf_appearance

      call read_real_var (section_name
     :                    , 'leaf_no_at_emerg', '()'
     :                    , c%leaf_no_at_emerg, numvals
     :                    , 0.0, 100.0)

         !    sugar_phenology_init

      call read_real_var (section_name
     :                    , 'shoot_lag', '(oC)'
     :                    , c%shoot_lag, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'shoot_rate', '(oC/mm)'
     :                    , c%shoot_rate, numvals
     :                    , 0.0, 100.0)

      call read_real_array (section_name
     :                    , 'x_node_no_app', max_table,'(oC)'
     :                    , c%x_node_no_app
     :                    , c%num_node_no_app
     :                    , 0.0, 1000.0)

      call read_real_array (section_name
     :                    , 'y_node_app_rate', max_table,'(oC)'
     :                    , c%y_node_app_rate
     :                    , c%num_node_no_app
     :                    , 0.0, 1000.0)

      call read_real_array (section_name
     :                    , 'x_node_no_leaf', max_table,'(oC)'
     :                    , c%x_node_no_leaf
     :                    , c%num_node_no_leaf
     :                    , 0.0, 1000.0)

      call read_real_array (section_name
     :                    , 'y_leaves_per_node', max_table,'(oC)'
     :                    , c%y_leaves_per_node
     :                    , c%num_node_no_leaf
     :                    , 0.0, 1000.0)

         !    sugar_dm_init

      call read_real_var (section_name
     :                    , 'dm_leaf_init', '(g/plant)'
     :                    , c%dm_leaf_init, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'dm_root_init', '(g/plant)'
     :                    , c%dm_root_init, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'dm_sstem_init', '(g/plant)'
     :                    , c%dm_sstem_init, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'dm_cabbage_init', '(g/plant)'
     :                    , c%dm_cabbage_init, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'dm_sucrose_init', '(g/plant)'
     :                    , c%dm_sucrose_init, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'leaf_cabbage_ratio', '()'
     :                    , c%leaf_cabbage_ratio, numvals
     :                    , 0.0, 10.0)

      call read_real_var (section_name
     :                    , 'cabbage_sheath_fr', '()'
     :                    , c%cabbage_sheath_fr, numvals
     :                    , 0.0, 1.0)

         !    sugar_dm_senescence

      call read_real_var (section_name
     :                    , 'dm_root_sen_frac', '()'
     :                    , c%dm_root_sen_frac, numvals
     :                    , 0.0, 1.0)

         !    sugar_dm_dead_detachment

      call read_real_array (section_name
     :                    , 'dead_detach_frac', max_part, '()'
     :                    , c%dead_detach_frac, numvals
     :                    , 0.0, 1.0)

      call read_real_array (section_name
     :                    , 'sen_detach_frac', max_part, '()'
     :                    , c%sen_detach_frac, numvals
     :                    , 0.0, 1.0)

         !    sugar_leaf_area_devel

      call read_real_var (section_name
     :                    , 'leaf_no_correction', '()'
     :                    , c%leaf_no_correction, numvals
     :                    , 0.0, 100.0)

         !    sugar_leaf_area_sen_light

      call read_real_var (section_name
     :                   , 'lai_sen_light', '(m^2/m^2)'
     :                   , c%lai_sen_light, numvals
     :                   , 0.0, 10.0)

      call read_real_var (section_name
     :                    , 'sen_light_slope', '()'
     :                    , c%sen_light_slope, numvals
     :                    , 0.0, 100.0)


         !    sugar_leaf_area_sen_frost

      call read_real_array (section_name
     :                   , 'frost_temp', max_table, '(oC)'
     :                   , c%frost_temp, c%num_frost_temp
     :                   , -20.0, 100.0)

      call read_real_array (section_name
     :                   , 'frost_fraction', max_table, '(oC)'
     :                   , c%frost_fraction, numvals
     :                   , 0.0, 1.0)

         !    sugar_leaf_area_sen_water

      call read_real_var (section_name
     :                    , 'sen_rate_water', '()'
     :                    , c%sen_rate_water, numvals
     :                    , 0.0, 100.0)

         !    sugar_phenology_init

      call read_real_var (section_name
     :                   , 'twilight', '(o)'
     :                   , c%twilight, numvals
     :                   , -90.0, 90.0)

         !    sugar_N_conc_limits

      call read_real_array (section_name
     :                     , 'x_stage_code', max_stage, '()'
     :                     , c%x_stage_code, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_crit_leaf', max_stage, '()'
     :                     , c%y_N_conc_crit_leaf, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_min_leaf', max_stage, '()'
     :                     , c%y_N_conc_min_leaf, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_crit_cane', max_stage, '()'
     :                     , c%y_N_conc_crit_cane, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_min_cane', max_stage, '()'
     :                     , c%y_N_conc_min_cane, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_crit_cabbage', max_stage, '()'
     :                     , c%y_N_conc_crit_cabbage, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_min_cabbage', max_stage, '()'
     :                     , c%y_N_conc_min_cabbage, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'n_conc_crit_root', '()'
     :                   , c%N_conc_crit_root, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'n_conc_min_root', '()'
     :                   , c%N_conc_min_root, numvals
     :                   , 0.0, 100.0)

         !    sugar_N_init

      call read_real_var (section_name
     :                   , 'n_leaf_init_conc', '()'
     :                   , c%N_leaf_init_conc, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'n_root_init_conc', '()'
     :                   , c%N_root_init_conc, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'n_sstem_init_conc', '()'
     :                   , c%N_sstem_init_conc, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'n_cabbage_init_conc', '()'
     :                   , c%N_cabbage_init_conc, numvals
     :                   , 0.0, 100.0)

         !    sugar_N_senescence

      call read_real_var (section_name
     :                   , 'n_leaf_sen_conc', '()'
     :                   , c%N_leaf_sen_conc, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'n_root_sen_conc', '()'
     :                   , c%N_root_sen_conc, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'n_cabbage_sen_conc', '()'
     :                   , c%N_cabbage_sen_conc, numvals
     :                   , 0.0, 100.0)

         !    sugar_rue_reduction

      call read_real_array (section_name
     :                     , 'x_ave_temp', max_table, '(oC)'
     :                     , c%x_ave_temp, c%num_ave_temp
     :                     , 0.0, 100.0)


      call read_real_array (section_name
     :                     , 'y_stress_photo', max_table, '()'
     :                     , c%y_stress_photo, numvals
     :                     , 0.0, 1.0)

      call read_real_array (section_name
     :                     , 'x_ave_temp_stalk', max_table, '(oC)'
     :                     , c%x_ave_temp_stalk, c%num_ave_temp_stalk
     :                     , 0.0, 100.0)


      call read_real_array (section_name
     :                     , 'y_stress_stalk', max_table, '()'
     :                     , c%y_stress_stalk, numvals
     :                     , 0.0, 1.0)

         !    sugar_tt

      call read_real_array (section_name
     :                     , 'x_temp', max_table, '(oC)'
     :                     , c%x_temp, c%num_temp
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_tt', max_table, '(oC)'
     :                     , c%y_tt, c%num_temp
     :                     , 0.0, 100.0)

         !    sugar_swdef

      call read_real_array (section_name
     :                     , 'x_sw_demand_ratio', max_table, '()'
     :                     , c%x_sw_demand_ratio, c%num_sw_demand_ratio
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_swdef_leaf', max_table, '()'
     :                     , c%y_swdef_leaf, c%num_sw_demand_ratio
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'x_demand_ratio_stalk', max_table, '()'
     :                     , c%x_demand_ratio_stalk
     :                     , c%num_demand_ratio_stalk
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_swdef_stalk', max_table, '()'
     :                     , c%y_swdef_stalk, c%num_demand_ratio_stalk
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

      ! Nitrogen Stress Factors
      ! -----------------------
      call read_real_var (section_name
     :                   , 'k_nfact_photo', '()'
     :                   , c%k_nfact_photo, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'k_nfact_expansion', '()'
     :                   , c%k_nfact_expansion, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'k_nfact_stalk', '()'
     :                   , c%k_nfact_stalk, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'k_nfact_pheno', '()'
     :                   , c%k_nfact_pheno, numvals
     :                   , 0.0, 100.0)

      ! Water logging function
      ! ----------------------
      call read_real_array (section_name
     :                     , 'oxdef_photo_rtfr', max_table, '()'
     :                     , c%oxdef_photo_rtfr, c%num_oxdef_photo
     :                     , 0.0, 1.0)
      call read_real_array (section_name
     :                     , 'oxdef_photo', max_table, '()'
     :                     , c%oxdef_photo, c%num_oxdef_photo

     :                     , 0.0, 1.0)

      ! Plant Water Content function
      ! ----------------------------
      call read_real_array (section_name
     :                     , 'cane_dmf_max', max_table, '()'
     :                     , c%cane_dmf_max, c%num_cane_dmf
     :                     , 0.0, 1.0)
      call read_real_array (section_name
     :                     , 'cane_dmf_min', max_table, '()'
     :                     , c%cane_dmf_min, c%num_cane_dmf
     :                     , 0.0, 1.0)
      call read_real_array (section_name
     :                     , 'cane_dmf_tt', max_table, '()'
     :                     , c%cane_dmf_tt, c%num_cane_dmf
     :                     , 0.0, 10000.)
      call read_real_var (section_name
     :                   , 'cane_dmf_rate', '()'
     :                   , c%cane_dmf_rate, numvals
     :                   , 0.0, 100.0)


      ! Death by Lodging Constants
      ! --------------------------
      call read_real_array (section_name
     :                     , 'stress_lodge', max_table, '(0-1)'
     :                     , c%stress_lodge, c%num_stress_lodge
     :                     , 0.0, 1.0)
      call read_real_array (section_name
     :                     , 'death_fr_lodge', max_table, '(0-1)'
     :                     , c%death_fr_lodge, c%num_stress_lodge
     :                     , 0.0, 1.0)
      call read_real_var (section_name
     :                   , 'lodge_redn_photo', '()'
     :                   , c%lodge_redn_photo, numvals
     :                   , 0.0, 1.0)
      call read_real_var (section_name
     :                   , 'lodge_redn_sucrose', '()'
     :                   , c%lodge_redn_sucrose, numvals
     :                   , 0.0, 1.0)
      call read_real_var (section_name
     :                   , 'lodge_redn_green_leaf', '()'
     :                   , c%lodge_redn_green_leaf, numvals
     :                   , 0.0, 1.0)

      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_update_other_variables ()
*     ===========================================================
      use sugarModule
      implicit none
      include   'convert.inc'
      include 'data.pub'
      include 'crp_comm.pub'
      include 'error.pub'
      include 'componentinterface.inc'

*+  Purpose
*       Update other modules states

*+  Mission Statement
*     Update other modules states

*+  Changes
*      250894 jngh specified and programmed
*      191099 jngh changed to sugar_Send_Crop_Chopped_Event
*     101100 dph  added eventInterface parameter to crop_root_incorp

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_update_other_variables')

*+  Local Variables
      real       dm_residue(max_part)  ! dry matter removed (kg/ha)
      real       N_residue(max_part)   ! nitrogen removed (kg/ha)
      real       fraction_to_Residue(max_part)   ! fraction sent to residue (0-1)

*- Implementation Section ----------------------------------
      call push_routine (my_name)

         ! dispose of detached material from senesced parts in
         ! live population

      dm_residue(:)=(g%dlt_dm_detached(:) + g%dlt_dm_dead_detached(:))
     :                      * gm2kg/sm2ha

      N_residue(:) = (g%dlt_N_detached(:)+g%dlt_N_dead_detached(:))
     :                      * gm2kg/sm2ha

      fraction_to_Residue(:) = 1.0
      fraction_to_Residue(root) = 0.0

!      call crop_top_residue (c%crop_type, dm_residue, N_residue)
         if (sum(dm_residue) .gt. 0.0) then
            call sugar_Send_Crop_Chopped_Event
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

      call crop_root_incorp ((g%dlt_dm_detached(root)
     :                               +g%dlt_dm_dead_detached(root))
     :                      ,(g%dlt_N_detached(root)
     :                               +g%dlt_N_dead_detached(root))
     :                      ,g%dlayer
     :                      ,g%root_length
     :                      ,g%root_depth
     :                      ,c%crop_type
     :                      ,max_layer
     :                      ,EventInterface
     :                      )

      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_hill_up ()
*     ===========================================================
      use sugarModule
      implicit none
      include   'const.inc'
      include   'action.inc'
      include 'postbox.pub'
      include 'data.pub'
      include 'intrface.pub'
      include 'error.pub'

*+  Purpose
*       Mound soil around base of crop and bury some plant material

*+  Mission Statement
*     Mound soil around base of crop

*+  Changes
*     120897 nih

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_hill_up')

*+  Local Variables
      integer    numvals               ! number of values found in array
      real       canefr
      real       topsfr
      real       fom(max_layer)
      real       fon(max_layer)
      integer    leaf_no

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (int(g%current_stage).eq.emerg) then
         call collect_real_var ('topsfr', '()'
     :                      , topsfr, numvals, 0.0, 1.0)
         call collect_real_var ('canefr', '()'
     :                      , canefr, numvals, 0.0, 1.0)

         call fill_Real_array (fom,0.0,max_layer)
         call fill_Real_array (fon,0.0,max_layer)

         fom(1) = topsfr * (g%dm_green(leaf)
     :                     +g%dm_green(cabbage)
     :                     +g%dm_senesced(leaf)
     :                     +g%dm_senesced(cabbage)
     :                     +g%dm_dead(leaf)
     :                     +g%dm_dead(cabbage))
     :          + canefr * (g%dm_green(sstem)
     :                     +g%dm_green(sucrose)
     :                     +g%dm_senesced(sstem)
     :                     +g%dm_senesced(sucrose)
     :                     +g%dm_dead(sstem)
     :                     +g%dm_dead(sucrose))

         fon(1) = topsfr * (g%n_green(leaf)
     :                     +g%n_green(cabbage)
     :                     +g%n_senesced(leaf)
     :                     +g%n_senesced(cabbage)
     :                     +g%n_dead(leaf)
     :                     +g%n_dead(cabbage))
     :          + canefr * (g%n_green(sstem)
     :                     +g%n_green(sucrose)
     :                     +g%n_senesced(sstem)
     :                     +g%n_senesced(sucrose)
     :                     +g%n_dead(sstem)
     :                     +g%n_dead(sucrose))


            call New_postbox ()

            call post_char_var('dlt_fom_type=','()',c%crop_type)

            call post_real_array ('dlt_fom_wt'
     :                           ,'(kg/ha)'
     :                           ,fom
     :                           ,1)

            call post_real_array ('dlt_fom_n'
     :                           ,'(kg/ha)'
     :                           ,fon
     :                           ,1)

            call Action_send (all_active_modules
     :                            ,ACTION_incorp_fom
     :                            ,blank)


            call Delete_postbox ()

         g%dm_green(leaf) = g%dm_green(leaf)*(1.-topsfr)
         g%dm_green(cabbage) = g%dm_green(cabbage)*(1.-topsfr)
         g%dm_senesced(leaf) = g%dm_senesced(leaf)*(1.-topsfr)
         g%dm_senesced(cabbage) = g%dm_senesced(cabbage)*(1.-topsfr)
         g%dm_dead(leaf) = g%dm_dead(leaf)*(1.-topsfr)
         g%dm_dead(cabbage) = g%dm_dead(cabbage)*(1.-topsfr)

         g%dm_green(sstem) = g%dm_green(sstem)*(1.-canefr)
         g%dm_green(sucrose) = g%dm_green(sucrose)*(1.-canefr)
         g%dm_senesced(sstem) = g%dm_senesced(sstem)*(1.-canefr)
         g%dm_senesced(sucrose) = g%dm_senesced(sucrose)*(1.-canefr)
         g%dm_dead(sstem) = g%dm_dead(sstem)*(1.-canefr)
         g%dm_dead(sucrose) = g%dm_dead(sucrose)*(1.-canefr)

         g%n_green(leaf) = g%n_green(leaf)*(1.-topsfr)
         g%n_green(cabbage) = g%n_green(cabbage)*(1.-topsfr)
         g%n_senesced(leaf) = g%n_senesced(leaf)*(1.-topsfr)
         g%n_senesced(cabbage) = g%n_senesced(cabbage)*(1.-topsfr)
         g%n_dead(leaf) = g%n_dead(leaf)*(1.-topsfr)
         g%n_dead(cabbage) = g%n_dead(cabbage)*(1.-topsfr)

         g%n_green(sstem) = g%n_green(sstem)*(1.-canefr)
         g%n_green(sucrose) = g%n_green(sucrose)*(1.-canefr)
         g%n_senesced(sstem) = g%n_senesced(sstem)*(1.-canefr)
         g%n_senesced(sucrose) = g%n_senesced(sucrose)*(1.-canefr)
         g%n_dead(sstem) = g%n_dead(sstem)*(1.-canefr)
         g%n_dead(sucrose) = g%n_dead(sucrose)*(1.-canefr)

      ! Now we need to update the leaf tracking info

      g%lai = g%lai * (1. - topsfr)
      g%slai = g%slai * (1. - topsfr)

      do 100 leaf_no = 1, max_leaf
         g%leaf_area(leaf_no) = g%leaf_area(leaf_no)
     :                        *(1.-topsfr)
         g%leaf_dm (leaf_no) = g%leaf_dm (leaf_no)
     :                        *(1.-topsfr)
  100 continue

      else
         call fatal_Error(Err_User,
     :      'Can only hill up during emergence phase')
      endif

      call pop_routine (my_name)
      return
      end



* ====================================================================
       subroutine sugar_lodge ()
* ====================================================================
      use sugarModule
      implicit none
      include 'error.pub'

*+  Purpose
*     <insert here>

*+  Mission Statement
*     Crop lodging

*+  Changes
*     25-08-1997 - unknown - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'sugar_lodge')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      g%lodge_flag = .true.
      call Write_string ('crop lodging')

      call pop_routine (myname)
      return
      end

*     ===========================================================
      subroutine sugar_ONtick ()
*     ===========================================================
      use sugarModule
      implicit none
      include 'error.pub'
      include 'event.pub'

*+  Purpose
*     Update internal time record and reset daily state variables.

*+  Mission Statement
*     Update internal time record and reset daily state variables.

*+  Changes
*        260899 nih

*+  Local Variables
      character temp1*5
      integer   temp2

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'sugar_ONtick')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      ! Note that time and timestep information is not required
      ! and so dummy variables are used in their place.

      call handler_ONtick(g%day_of_year, g%year, temp1, temp2)

      call sugar_zero_daily_variables ()

      call pop_routine (myname)
      return
      end
* ====================================================================
      subroutine sugar_Send_Crop_Chopped_Event (crop_type
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
*   110700 jngh - Changed dm_type to array

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'sugar_Send_Crop_Chopped_Event')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call new_postbox ()

         ! send message regardless of fatal error - will stop anyway


!cjh      write(*,*) 'sugar: '//EVENT_Crop_Chopped
!cjh      write(*,*) 'sugar: '//DATA_crop_type
!cjh     :               , ' '//crop_type
!cjh      write(*,*) 'sugar: '//DATA_dm_type
!cjh     :               , ' '//dm_type
!cjh      write(*,*) 'sugar: '//DATA_dlt_crop_dm
!cjh     :               , dlt_crop_dm
!cjh      write(*,*) 'sugar: '//DATA_dlt_dm_n
!cjh     :               , dlt_dm_n
!cjh      write(*,*) 'sugar: '//DATA_fraction_to_Residue
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




