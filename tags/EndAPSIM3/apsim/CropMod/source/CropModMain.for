
      include 'CropMod.inc'

!     ===========================================================
      subroutine alloc_dealloc_instance(doAllocate)
!     ===========================================================
      use CropModModule
      implicit none
      ml_external alloc_dealloc_instance

!+  Sub-Program Arguments
      logical, intent(in) :: doAllocate

!+  Purpose
!      Module instantiation routine.

!- Implementation Section ----------------------------------


      if (doAllocate) then
         allocate(g)
         allocate(p)
         allocate(c)

      else
         deallocate(g)
         deallocate(p)
         deallocate(c)

      end if
      return
      end

!     ===========================================================
      subroutine do_init1(sdml)
!     ===========================================================
      use CropModModule
      implicit none
      ml_external do_init1

!+  Purpose
!      Perform all registrations and zeroing

!+  Sub-Program Arguments
      character (len=*), intent(in) :: sdml

!- Implementation Section ----------------------------------

      call do_registrations()


         !Zero pools inlcuding contants and parameters
         call Zero_Variables (.true.)

         !Read the crop specific contants from ini files
         call CropMod_Initialisation ()




      return
      end

!     ===========================================================
      subroutine do_commence()
!     ===========================================================
      implicit none
      ml_external do_commence

!+  Purpose
!      Perform all registrations and zeroing

!- Implementation Section ----------------------------------


      return
      end

* ====================================================================
      subroutine do_init2 ()
* ====================================================================
      use CropModModule
      use ComponentInterfaceModule
      implicit none
      ml_external do_init2

*+  Purpose
*     Initialise the CropMod module

*+  Changes

*+  Calls

*+  Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='CropMod_init2')

*+  Local Variables
      logical found

*- Implementation Section ----------------------------------

      call push_routine (this_routine)


      !Request and receive variables from owner-modules
      call Get_Other_Variables ()

      call pop_routine (this_routine)
      return
      end subroutine


!     ===========================================================
      subroutine respondToEvent(fromID,eventID, variant)
!     ===========================================================
      use CropModModule
      use ComponentInterfaceModule
      implicit none
      ml_external respondToEvent

!+  Purpose
!      Event handler for all events coming into module.

!+  Sub-Program Arguments
      integer, intent(in) :: fromID
      integer, intent(in) :: eventID
      integer, intent(in) :: variant

!- Implementation Section ----------------------------------

      if (eventID.eq.Prepare_id) then
         if (g%plant_status.ne.status_out) then
            !Zero all daily rate variables
            call Zero_Daily_Variables ()
            !Request and receive variables from owner-modules
            call Get_Other_Variables ()
            !Do crop processes prepare
            call Simulation_Prepare ()
         else
            ! Crop not in the field, do nothing
            !call Zero_Variables (.false.)
         endif
c ***********************************************************************
      elseif (eventID.eq.Process_id) then

         if (g%plant_status.ne.status_out) then

            !request and receive variables from owner-modules
            call Get_Other_Variables ()
            !Crop processes - Dynamic prcess sub-modules
            call Crop_Process ()
            !Send changes of other variables to owner-modules
            call Set_Other_Variables ()

         else
            !crop not in, do nothing
         endif

c ************************************************************************
      elseif (eventID.eq.PreWaterBalance_id) then
            call cropmod_send_water_demand()

c ************************************************************************
      else
         call error('bad event ID',.true.)
      endif
      return
      end

!     ===========================================================
      subroutine respondToMethod(fromID,methodID, variant)
!     ===========================================================
      use CropModModule
      use ComponentInterfaceModule
      implicit none
      ml_external respondToMethod

!+  Purpose
!      Method handler for all method calls coming into module.

!+  Sub-Program Arguments
      integer, intent(in) :: fromID
      integer, intent(in) :: methodID
      integer, intent(in) :: variant

!+  Calls
      logical  crop_my_type  !function

!- Implementation Section ----------------------------------

c ******************************************************************
      if (methodID.eq.sow_id) then

            !request and receive variables from owner-modules
            call Get_Other_Variables ()

            !start crop, read the sow information and do  more initialisations
            call Start_Crop (variant)


c ******************************************************************
      else if (methodID.eq.harvest_id) then


               ! harvest crop - report harvest information
               call Crop_Harvest (
     .          g%dm_green,
     .          g%dm_dead,
     .          c%grn_water_cont,
     .          g%grain_no,
     .          g%plants,
     .          g%dm_senesced,
     .          g%leaf_no,
     .          g%N_green,
     .          g%N_dead,
     .          g%N_senesced,
     .          g%flowering_date,
     .          g%maturity_date,
     .          g%flowering_das,
     .          g%maturity_das,
     .          g%lai_max,
     .          g%cswd_photo,
     .          g%days_tot,
     .          g%cswd_expansion,
     .          g%cnd_photo,
     .          g%cnd_grain_conc,
     .          c%stage_names)

c ******************************************************************
      else if (methodID.eq.end_crop_id) then


         !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c        if (TestTrue)   close (1)
         !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



            !end crop - turn the stover into residue
            call End_Crop ()

            !Zero all the globals, but not the contants and parameters
            call Zero_Variables (.false.)

            !Set plant status to status_out and stage to plant_end
            if (g%plant_status.ne.status_out) then
                g%plant_status  = status_out
                g%current_stage = real (plant_end)
            end if



c ******************************************************************
      else if (methodID.eq.kill_crop_id) then


            ! kill crop - died, but biomass remain in field
            call Kill_Crop (
     .          g%plant_status,
     .          g%dm_green,
     .          g%dm_senesced,
     .          g%dm_dead)

c ******************************************************************
      else if (methodID.eq.cropwatersupply_id) then
         call cropmod_crop_water_supply(variant)
c ******************************************************************


      else
         call error('bad method ID',.true.)
      endif

      return
      end

!     ===========================================================
      subroutine notify_termination()
!     ===========================================================
      use CropModModule
      implicit none
      ml_external notify_termination

!+  Purpose
!      Prepare for termination

!- Implementation Section ----------------------------------


      return
      end

* ====================================================================
       subroutine respondToGet (fromID,Variable_info)
* ====================================================================
      use CropModModule
      use ComponentInterfaceModule
      implicit none
      ml_external respondToGet

*+  Sub-Program Arguments
      integer, intent(in) :: fromID
      type(QueryData), intent(in) :: variable_info

*+  Purpose
*      Return the value of a variable requested by other modules.

*+  Changes
*      string_concat
*      090495 psc  added nfact to output list
*      170495 psc  added grain_size, yield, biomass to output list
*      220896 jngh  added call to message_unused

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Send_My_Variable')

*+  Local Variables
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
      real       N_uptake_sum          ! N supply from soil
      real       grain_size            ! individual grain wt (g/grain)
      real       yield                 ! grain yield (kg/ha)
      real       biomass               ! total above-ground biomass (kg/ha)
      real       biomass_n             ! total above-ground biomass N (kg/ha)
      real       sw_supply_sum         ! total supply over profile (mm)
      integer    layer                 ! soil layer
      real       esw_layer(max_layer)   ! plant extractable soil water
      real       rlv(max_layer)
      real       ll(max_layer)
      real       n_conc
      real       hi                    ! harvest index (yield/biomass)
      real       sw_deficit(max_layer) ! Soil water deficit below dul_dep (mm)
      integer    i
      real       stover
      real       das_real
      integer    das_int
      REAL       leaf_no_now
      REAL       esw_sum
      REAL       biomass_tot
      REAL       biomass_p
      REAL       plant_p_max
      REAL       pconc
      REAL       value

      REAL       no3_uptake(max_layer)
      REAL       nh4_uptake(max_layer)


*- Implementation Section ----------------------------------



         biomass = sum_real_array (g%dm_green, max_part)
     :           - g%dm_green(root) - g%dm_green(energy)
     :           + sum_real_array (g%dm_senesced, max_part)
     :           - g%dm_senesced(root) - g%dm_senesced(energy)
     :           + sum_real_array (g%dm_dead, max_part)
     :           - g%dm_dead(root) - g%dm_dead(energy)




      !================================================================
      !crop type
       if (variable_info%id .eq. crop_type_id) then
         call return_crop_type (variable_info
     :                             , c%crop_type)


      !================================================================

       elseif (variable_info%id .eq. extinct_coeff_id) then
         call return_extinct_coeff (variable_info
     :                             , g%extinction_coeff)


       elseif (variable_info%id .eq. radn_int_id) then
         call return_radn_int (variable_info
     :                             , g%radn_int)


       elseif (variable_info%id .eq. rue_day_id) then
         value = divide(g%dlt_dm, g%radn_int, 0.0)
         call return_rue_day (variable_info
     :                             , value)


      !================================================================
      !Days after sowing
      elseif ((variable_info%id .eq. das_id) .or.
     :        (variable_info%id .eq. das_int_id)) then

         das_real = sum_between (sowing, now, g%days_tot)
         das_int  = int(das_real+0.9999999)

         call return_das (variable_info
     :                             , das_int)

      !================================================================
      !crop status, thermal time and development stages

      elseif (variable_info%id .eq. plant_status_id) then
         call return_plant_status (variable_info
     :                             , g%plant_status)

      elseif (variable_info%id .eq. stage_name_id) then

         if (g%plant_status.ne.'status_out') then
            stage_no = int (g%current_stage)
            call return_stage_name (variable_info
     :                             , c%stage_names(stage_no))
         else
            call return_stage_name (variable_info
     :                             , status_out)
         endif

      elseif (variable_info%id .eq. today_stage_code_id) then
         if (g%plant_status.ne.'status_out') then
            stage_no = int (g%current_stage)
            call return_today_stage_code (variable_info
     :                             , c%stage_code_list(stage_no))
         else
            call return_today_stage_code (variable_info
     :                             , 0.0)
         endif

      elseif (variable_info%id .eq. stage_id) then
         call return_stage (variable_info
     :                             , g%current_stage)


      elseif ((variable_info%id .eq. today_zadok_stage_id) .or.
     :        (variable_info%id .eq. dc_stage_id))    then
         stage_no = INT(g%zadok_stage + 0.5)
         call return_today_zadok_stage (variable_info
     :                             , stage_no)


      elseif (variable_info%id .eq. dlt_stage_id) then
         call return_dlt_stage (variable_info
     :                             , g%dlt_stage)


      elseif (variable_info%id .eq. dlt_tt_id) then
         call return_dlt_tt (variable_info
     :                             , g%dlt_tt)

      elseif (variable_info%id .eq. tt_tot_id) then
         call return_tt_tot (variable_info
     :                             , g%tt_tot
     :                             , max_stage)

      elseif (variable_info%id .eq. tt_sum_id) then
         call return_tt_sum (variable_info
     :                             , g%tt_tot(INT(g%current_stage)))

      elseif (variable_info%id .eq. days_tot_id) then
         call return_days_tot (variable_info
     :                             , g%days_tot
     :                             , max_stage)

      elseif (variable_info%id .eq. phase_tt_id) then
         call return_phase_tt (variable_info
     :                             , g%phase_tt
     :                             , max_stage)

      !the following two not generalised
      elseif (variable_info%id .eq. dlt_tt_fm_id) then
         call return_dlt_tt_fm (variable_info
     :                             , g%dlt_tt_fm)

      elseif (variable_info%id .eq. tt_tot_fm_id) then
         call return_tt_tot_fm (variable_info
     :                             , g%tt_tot_fm
     :                             , max_stage)

      !================================================================
      !flowering and maturity dates

      elseif (variable_info%id .eq. flowering_date_id) then
         call return_flowering_date (variable_info
     :                             , g%flowering_date)

      elseif (variable_info%id .eq. maturity_date_id) then
         call return_maturity_date (variable_info
     :                             , g%maturity_date)

      elseif (variable_info%id .eq. flowering_das_id) then
         call return_flowering_das (variable_info
     :                             , g%flowering_das)

      elseif (variable_info%id .eq. maturity_das_id) then
         call return_maturity_das (variable_info
     :                             , g%maturity_das)


      !================================================================
      ! crop canopy - leaf no, tiller no, LAI and crop height

      elseif (variable_info%id .eq. leaf_primodia_id) then
         call return_leaf_primodia (variable_info
     :                             , g%leaf_primodia)

      elseif (variable_info%id .eq. leaf_no_final_id) then
         call return_leaf_no_final (variable_info
     :                             , g%leaf_no_final)

      elseif (variable_info%id .eq. leaf_no_id) then
        leaf_no_now = sum_between (emerg, harvest_ripe, g%leaf_no)
         call return_leaf_no (variable_info
     :                              , leaf_no_now)


      elseif (variable_info%id .eq. dlt_leaf_no_id) then
         call return_dlt_leaf_no (variable_info
     :                              , g%dlt_leaf_no
     :                              )

      elseif (variable_info%id .eq. leaf_no_dead_id) then
         call return_leaf_no_dead (variable_info
     :                              , g%leaf_no_dead
     :                              , max_stage)

      elseif (variable_info%id .eq. leaf_area_id) then
         call return_leaf_area (variable_info
     :                              , g%leaf_area
     :                              , max_leaf)


      elseif (variable_info%id .eq. cover_green_id) then
         call return_cover_green (variable_info
     :                             , g%cover_green)

      elseif (variable_info%id .eq. cover_tot_id) then
         cover_tot = 1.0
     :             - (1.0 - g%cover_green)
     :             * (1.0 - g%cover_sen)
     :             * (1.0 - g%cover_dead)


         call return_cover_tot (variable_info
     :                             , cover_tot)

      elseif (variable_info%id .eq. lai_id) then
         call return_lai (variable_info
     :                             , g%lai)

      elseif (variable_info%id .eq. lai_max_id) then
         call return_lai_max (variable_info
     :                             , g%lai_max)

      elseif (variable_info%id .eq. lai_sum_id) then
         lai_sum = g%lai + g%slai + g%tlai_dead
         call return_lai_sum (variable_info
     :                             , lai_sum)

      elseif (variable_info%id .eq. tlai_id) then
         call return_tlai (variable_info
     :                             , g%lai + g%slai)

      elseif (variable_info%id .eq. slai_id) then
         call return_slai (variable_info
     :                             , g%slai)

      elseif (variable_info%id .eq. tlai_dead_id) then
         call return_tlai_dead (variable_info
     :                             , g%tlai_dead)

      elseif (variable_info%id .eq. sla_id) then
         call return_sla (variable_info
     :                             , divide(g%lai*sm2smm
     :                             , g%dm_green(leaf), 0.0))

      elseif (variable_info%id .eq. dlt_lai_id) then
         call return_dlt_lai (variable_info
     :                             , g%dlt_lai)

      elseif (variable_info%id .eq. dlt_lai_pot_id) then
         call return_dlt_lai_pot (variable_info
     :                             , g%dlt_lai_pot)

      elseif (variable_info%id .eq. dlt_lai_stressed_id) then
         call return_dlt_lai_stressed (variable_info
     :                             , g%dlt_lai_stressed)

      elseif (variable_info%id .eq. tiller_tt_tot_id) then
         call return_tiller_tt_tot (variable_info
     :                             , g%tiller_tt_tot)


      elseif (variable_info%id .eq. dlt_slai_id) then
         call return_dlt_slai (variable_info
     :                             , g%dlt_slai)


      !...................................................
      ! These dlts are not made available yet

      elseif (variable_info%id .eq. dlt_slai_age_id) then
         call return_dlt_slai_age (variable_info
     :                             , g%dlt_slai_age)

      elseif (variable_info%id .eq. dlt_slai_light_id) then
         call return_dlt_slai_light (variable_info
     :                             , g%dlt_slai_light)

      elseif (variable_info%id .eq. dlt_slai_water_id) then
         call return_dlt_slai_water (variable_info
     :                             , g%dlt_slai_water)

      elseif (variable_info%id .eq. dlt_slai_nitrogen_id) then
         call return_dlt_slai_nitrogen (variable_info
     :                             , g%dlt_slai_nitrogen)
      !...................................................

      elseif (variable_info%id .eq. plants_id) then
         call return_plants (variable_info
     :                             , g%plants)

      elseif (variable_info%id .eq. height_id) then
         call return_height (variable_info
     :                             , g%canopy_height)

      elseif (variable_info%id .eq. tiller_no_id) then
         call return_tiller_no (variable_info
     :                             , g%tiller_no_fertile)

      elseif (variable_info%id .eq. tiller_no_fertile_id) then
         call return_tiller_no_fertile (variable_info
     :                             , g%tiller_no_fertile)

      elseif (variable_info%id .eq. grain_no_id) then
         call return_grain_no (variable_info
     :                             , g%grain_no)

      elseif (variable_info%id .eq. grain_size_id) then
         grain_size = divide (g%dm_green(grain) + g%dm_dead(grain)
     :                 , g%grain_no, 0.0)
         call return_grain_size (variable_info
     :                             , grain_size)

      !================================================================
      ! Root depth and length

      elseif (variable_info%id .eq. root_depth_id) then
         call return_root_depth (variable_info
     :                             , g%root_depth)

      elseif (variable_info%id .eq. root_length_id) then
         num_layers = count_of_real_vals (g%dlayer, max_layer)
         call return_root_length (variable_info
     :                               , g%root_length
     :                               , num_layers)

      elseif (variable_info%id .eq. rlv_id) then
         num_layers = count_of_real_vals (g%dlayer, max_layer)
         do layer = 1, num_layers
            rlv(layer) = divide (g%root_length(layer)
     :                          ,g%dlayer(layer)
     :                          ,0.0)
         enddo
         call return_rlv (variable_info
     :                               , rlv
     :                               , num_layers)


      !===============================================================
      !plant biomass partition

      elseif (variable_info%id .eq. leaf_part_id) then

         value=divide(g%dm_green   (leaf) +
     :                g%dm_senesced(leaf) +
     :                g%dm_dead    (leaf) ,  biomass,0.0)

         call return_leaf_part (variable_info, value)

      elseif (variable_info%id .eq. stem_part_id) then

         value=divide(g%dm_green   (stem) +
     :                g%dm_senesced(stem) +
     :                g%dm_dead    (stem) ,  biomass,0.0)

         call return_stem_part (variable_info, value)

      elseif (variable_info%id .eq. grain_part_id) then

         value=divide(g%dm_green   (grain) +
     :                g%dm_senesced(grain) +
     :                g%dm_dead    (grain) ,  biomass,0.0)

         call return_grain_part (variable_info, value)

      elseif (variable_info%id .eq. root_part_id) then


         value=       g%dm_green   (root) +
     :                g%dm_senesced(root) +
     :                g%dm_dead    (root)

        value= divide(value,  biomass + value, 0.0)

         call return_root_part (variable_info, value)

      !===============================================================
      !plant biomass

      !----------------------------------------------------------------
      !Biomass in g/m2

      elseif (variable_info%id .eq. leaf_wt_id) then
         call return_leaf_wt (variable_info
     :                             , g%dm_green(leaf))

      elseif (variable_info%id .eq. dleaf_wt_id) then
         call return_dleaf_wt (variable_info
     :                             , g%dm_senesced(leaf))


      elseif (variable_info%id .eq. tleaf_wt_id) then
         call return_tleaf_wt (variable_info
     :                             , g%dm_green(leaf)
     :                             + g%dm_senesced(leaf))

      elseif (variable_info%id .eq. stem_wt_id) then
         call return_stem_wt (variable_info
     :                             , g%dm_green(stem))

      elseif (variable_info%id .eq. flower_wt_id) then
         call return_flower_wt (variable_info
     :                             , g%dm_green(flower))

      elseif (variable_info%id .eq. stem_flower_wt_id) then
         call return_stem_flower_wt (variable_info
     :                             , g%dm_green(stem)
     :                               +g%dm_green(flower))

      elseif (variable_info%id .eq. grain_wt_id) then
         call return_grain_wt (variable_info
     :                             , g%dm_green(grain))


      elseif (variable_info%id .eq. root_wt_id) then
         call return_root_wt (variable_info
     :                             , g%dm_green(root))


      elseif (variable_info%id .eq. droot_wt_id) then
         call return_droot_wt (variable_info
     :                             , g%dm_senesced(root))

      elseif (variable_info%id .eq. troot_wt_id) then
         call return_troot_wt (variable_info
     :                             , g%dm_green(root)
     :                             + g%dm_senesced(root))

      elseif (variable_info%id .eq. biomass_wt_id) then

         biomass = sum_real_array (g%dm_green, max_part)
     :           - g%dm_green(root) - g%dm_green(energy)
     :           + sum_real_array (g%dm_senesced, max_part)
     :           - g%dm_senesced(root) - g%dm_senesced(energy)
     :           + sum_real_array (g%dm_dead, max_part)
     :           - g%dm_dead(root) - g%dm_dead(energy)

         call return_biomass_wt (variable_info
     :                             , biomass)


      elseif (variable_info%id .eq. green_biomass_wt_id) then
         biomass = sum_real_array (g%dm_green, max_part)
     :           - g%dm_green(root) - g%dm_green(energy)

         call return_green_biomass_wt (variable_info
     :                             , biomass)

      elseif (variable_info%id .eq. stover_wt_id) then

         yield = (g%dm_green(grain) + g%dm_dead(grain))

         biomass = sum_real_array (g%dm_green, max_part)
     :           - g%dm_green(root) - g%dm_green(energy)
     :           + sum_real_array (g%dm_senesced, max_part)
     :           - g%dm_senesced(root) - g%dm_senesced(energy)
     :           + sum_real_array (g%dm_dead, max_part)
     :           - g%dm_dead(root) - g%dm_dead(energy)

         stover = biomass - yield

         call return_stover_wt (variable_info
     :                             , stover)

      !----------------------------------------------------
      !Biomass arrays in g/m2

            elseif (variable_info%id .eq. dm_green_id) then
         call return_dm_green (variable_info
     :                             , g%dm_green
     :                             , max_part)

      elseif (variable_info%id .eq. dm_senesced_id) then
         call return_dm_senesced (variable_info
     :                             , g%dm_senesced
     :                             , max_part)

      elseif (variable_info%id .eq. dm_dead_id) then
         call return_dm_dead (variable_info
     :                             , g%dm_dead
     :                             , max_part)

      !-------------------------------------------------------------
      !Biomass output in  kg/ha
      elseif (variable_info%id .eq. yield_id) then
         yield = (g%dm_green(grain) + g%dm_dead(grain))
     :           * gm2kg / sm2ha
         call return_yield (variable_info
     :                             , yield)


      elseif (variable_info%id .eq. biomass_id) then
         !The energy part should not be deleted from the following - ew
         biomass = (sum_real_array (g%dm_green, max_part)
     :           - g%dm_green(root) - g%dm_green(energy)
     :           + sum_real_array (g%dm_senesced, max_part)
     :           - g%dm_senesced(root) - g%dm_senesced(energy)
     :           + sum_real_array (g%dm_dead, max_part)
     :           - g%dm_dead(root) - g%dm_dead(energy))
     :           * gm2kg / sm2ha

         call return_biomass (variable_info
     :                             , biomass)

      !mjr 05/97 output stover (kg/ha)
      elseif (variable_info%id .eq. stover_id) then

         yield = (g%dm_green(grain) + g%dm_dead(grain))
     :           * gm2kg / sm2ha

         !The energy part should not be deleted from the following - ew
         biomass = (sum_real_array (g%dm_green, max_part)
     :           - g%dm_green(root) - g%dm_green(energy)
     :           + sum_real_array (g%dm_senesced, max_part)
     :           - g%dm_senesced(root) - g%dm_senesced(energy)
     :           + sum_real_array (g%dm_dead, max_part)
     :           - g%dm_dead(root) - g%dm_dead(energy))
     :           * gm2kg / sm2ha

         stover = biomass - yield

         call return_stover (variable_info
     :                             , stover)

      elseif (variable_info%id .eq. green_biomass_id) then
         biomass = (sum_real_array (g%dm_green, max_part)
     :              - g%dm_green(root)- g%dm_green(energy))
     :             * gm2kg / sm2ha

         call return_green_biomass (variable_info
     :                             , biomass)


      !scc 10/95 output harvest index
      elseif (variable_info%id .eq. hi_id) then

         yield = (g%dm_green(grain) + g%dm_dead(grain))
     :           * gm2kg / sm2ha

         !The energy part should not be deleted from the following - ew
         biomass = (sum_real_array (g%dm_green, max_part)
     :           - g%dm_green(root) - g%dm_green(energy)
     :           + sum_real_array (g%dm_senesced, max_part)
     :           - g%dm_senesced(root) - g%dm_senesced(energy)
     :           + sum_real_array (g%dm_dead, max_part)
     :           - g%dm_dead(root) - g%dm_dead(energy))
     :           * gm2kg / sm2ha

         hi = divide(yield, biomass, 0.0)

         call return_hi (variable_info
     :                             , hi)

      !---------------------------------------------------------
      !biomass deltas

      elseif (variable_info%id .eq. dlt_dm_water_id) then
         call return_dlt_dm_water (variable_info
     :                             , g%dlt_dm_water)

      elseif (variable_info%id .eq. dlt_dm_light_id) then
         call return_dlt_dm_light (variable_info
     :                             , g%dlt_dm_light)


      elseif (variable_info%id .eq. dlt_dm_id) then
         call return_dlt_dm (variable_info
     :                             , g%dlt_dm)

      elseif (variable_info%id .eq. dlt_dm_green_id) then
         call return_dlt_dm_green (variable_info
     :                             , g%dlt_dm_green
     :                             , max_part)

      elseif (variable_info%id .eq. dlt_dm_green_retrans_id) then
         call return_dlt_dm_green_retrans (variable_info
     :                             , g%dlt_dm_green_retrans
     :                             , max_part)

      elseif (variable_info%id .eq. dlt_dm_senesced_id) then
         call return_dlt_dm_senesced (variable_info
     :                             , g%dlt_dm_senesced
     :                             , max_part)

      elseif (variable_info%id .eq. dlt_dm_detached_id) then
         call return_dlt_dm_detached (variable_info
     :                             , g%dlt_dm_detached
     :                             , max_part)

      elseif (variable_info%id .eq. dlt_dm_dead_detached_id) then
         call return_dlt_dm_dead_detached (variable_info
     :                             , g%dlt_dm_dead_detached
     :                             , max_part)


      !================================================================
      !Plant Water

      elseif (variable_info%id .eq. swdef_pheno_id) then
         call return_swdef_pheno (variable_info
     :                             , g%swdef_pheno)

      elseif (variable_info%id .eq. swdef_photo_id) then
         call return_swdef_photo (variable_info
     :                             , g%swdef_photo)

      elseif (variable_info%id .eq. swdef_expan_id) then
         call return_swdef_expan (variable_info
     :                             , g%swdef_expansion)

      elseif (variable_info%id .eq. swdef_tiller_id) then
         call return_swdef_tiller (variable_info
     :                             , g%swdef_tiller)


      elseif ((variable_info%id .eq. sw_uptake_id).or.
     :        (variable_info%id .eq. ep_id))        then
         num_layers = count_of_real_vals (g%dlayer, max_layer)
         call return_sw_uptake (variable_info
     :                               , g%dlt_sw_dep
     :                               , num_layers)

      elseif (variable_info%id .eq. transpiration_id) then
         deepest_layer = find_layer_no (g%root_depth, g%dlayer
     :                                , max_layer)
         sw_supply_sum = sum_real_array (g%dlt_sw_dep, deepest_layer)
         call return_transpiration (variable_info
     :                             , -sw_supply_sum)

      elseif ((variable_info%id .eq. transpiration_tot_id).or.
     :        (variable_info%id .eq. cep_id))      then
         call return_transpiration_tot (variable_info
     :                             ,  g%transpiration_tot)


      elseif (variable_info%id .eq. esw_layer_id) then

         num_layers = count_of_real_vals (g%dlayer, max_layer)

         do layer = 1, num_layers
            esw_layer(layer) = g%sw_dep(layer) - p%ll_dep(layer)
            esw_layer(layer) = l_bound (esw_layer(layer), 0.0)
         enddo

         call return_esw_layer (variable_info
     :                               , esw_layer
     :                               , num_layers)

      elseif (variable_info%id .eq. esw_profile_id) then

         num_layers = count_of_real_vals (g%dlayer, max_layer)
         esw_sum = 0.0
         do layer = 1, num_layers
            esw_layer(layer) = g%sw_dep(layer) - p%ll_dep(layer)
            esw_layer(layer) = l_bound (esw_layer(layer), 0.0)
            esw_sum = esw_sum + esw_layer(layer)
         enddo

         call return_esw_profile  (variable_info
     :                               , esw_sum)


      !cscc/glh soil water deficit below dul
      elseif (variable_info%id .eq. sw_deficit_id) then
         deepest_layer = find_layer_no (g%root_depth, g%dlayer
     :       , max_layer)
         num_layers = count_of_real_vals (g%dlayer, max_layer)
         do i=1,num_layers
            sw_deficit(i) = l_bound(g%dul_dep(i) - g%sw_dep(i),0.0)
         enddo

         call return_sw_deficit (variable_info
     :                               , sw_deficit
     :                               , num_layers)

      elseif (variable_info%id .eq. vpd_id) then
         call return_vpd (variable_info
     :                             , g%vpd)

      elseif (variable_info%id .eq. transp_eff_id) then
         call return_transp_eff (variable_info
     :                             , g%transp_eff)

      elseif (variable_info%id .eq. sw_demand_id) then
         call return_sw_demand (variable_info
     :                             , g%sw_demand)

      elseif (variable_info%id .eq. sw_supply_id) then
         deepest_layer = find_layer_no (g%root_depth, g%dlayer
     :                                , max_layer)
         sw_supply_sum = sum_real_array (g%sw_supply, deepest_layer)
         call return_sw_supply (variable_info
     :                             , sw_supply_sum)

      elseif (variable_info%id .eq. sw_supply_sum_id) then
         call return_sw_supply_sum (variable_info
     :                             , g%sw_supply_sum)

      elseif (variable_info%id .eq. sw_supply_demand_ratio_id) then
         call return_sw_supply_demand_ratio (variable_info
     :                             , divide(g%sw_supply_sum,
     :                                      g%sw_demand,0.0))

      elseif (variable_info%id .eq. ll_id)  then

         num_layers = count_of_real_vals (g%dlayer, max_layer)

         call fill_real_array(ll,0.0, max_layer)

         do layer = 1, num_layers
            ll(layer) = divide(p%ll_dep(layer), g%dlayer(layer), 0.0)
         end do

         call return_ll (variable_info
     :                               , ll
     :                               , num_layers)

      elseif (variable_info%id .eq. ll_dep_id)  then

         call return_ll_dep (variable_info
     :                               , p%ll_dep
     :                               , num_layers)

      elseif (variable_info%id .eq. kl_id)  then

         call return_kl (variable_info
     :                               , p%kl
     :                               , num_layers)

      elseif (variable_info%id .eq. xf_id)  then

         call return_xf (variable_info
     :                               , p%xf
     :                               , num_layers)

      !=============================================================
      ! plant nitrogen

      !nitrogen demand and supply

      elseif (variable_info%id .eq. leaf_nd_id) then
         call return_leaf_nd (variable_info
     :                             , g%n_demand(leaf))

      elseif (variable_info%id .eq. stem_nd_id) then
         call return_stem_nd (variable_info
     :                             , g%n_demand(stem))

      elseif (variable_info%id .eq. flower_nd_id) then
         call return_flower_nd (variable_info
     :                             , g%n_demand(flower))

      elseif (variable_info%id .eq. grain_nd_id) then
         call return_grain_nd (variable_info
     :                             , g%n_demand(grain))

      elseif (variable_info%id .eq. root_nd_id) then
         call return_root_nd (variable_info
     :                             , g%n_demand(root))

      elseif (variable_info%id .eq. n_demand_id) then
         N_demand = sum_real_array (g%N_demand, max_part)
         call return_n_demand (variable_info
     :                             , N_demand)
      !THIS IS SWIM STUFF
      elseif (variable_info%id .eq. no3_demand_id) then
         N_demand = sum_real_array (g%N_demand, max_part)
     :            * gm2kg/sm2ha
         call return_no3_demand (variable_info
     :                             , N_demand)

      elseif (variable_info%id .eq. n_supply_soil_id) then
         deepest_layer = find_layer_no (g%root_depth,g%dlayer,max_layer)
         N_uptake_sum = - sum_real_array (g%dlt_NO3gsm, deepest_layer)
     :                  - sum_real_array (g%dlt_NH4gsm, deepest_layer)
         call return_n_supply_soil (variable_info
     :                             , N_uptake_sum)

      !nitrogen uptake


      elseif (variable_info%id .eq. n_massflow_uptake_id) then
         deepest_layer = find_layer_no (g%root_depth,g%dlayer,max_layer)
         N_uptake_sum = - sum_real_array (g%dlt_NO3gsm_massflow,
     :                                   deepest_layer)
     :                  - sum_real_array (g%dlt_NH4gsm_massflow,
     :                                            deepest_layer)
         call return_n_massflow_uptake (variable_info
     :                             , N_uptake_sum)

      elseif (variable_info%id .eq. no3_massflow_uptake_id) then
         deepest_layer = find_layer_no (g%root_depth,g%dlayer,max_layer)
         N_uptake_sum = - sum_real_array (g%dlt_NO3gsm_massflow,
     :                                            deepest_layer)
         call return_no3_massflow_uptake (variable_info
     :                             , N_uptake_sum)

      elseif (variable_info%id .eq. nh4_massflow_uptake_id) then
         deepest_layer = find_layer_no (g%root_depth,g%dlayer,max_layer)
         N_uptake_sum = - sum_real_array (g%dlt_NH4gsm_massflow,
     :                                            deepest_layer)
         call return_nh4_massflow_uptake (variable_info
     :                             , N_uptake_sum)


      elseif (variable_info%id .eq. n_diffusion_uptake_id) then
         deepest_layer = find_layer_no (g%root_depth,g%dlayer,max_layer)
         N_uptake_sum = - sum_real_array (g%dlt_NO3gsm_diffusion,
     :                                   deepest_layer)
     :                  - sum_real_array (g%dlt_NH4gsm_diffusion,
     :                                                 deepest_layer)
         call return_n_diffusion_uptake (variable_info
     :                             , N_uptake_sum)

      elseif (variable_info%id .eq. no3_diffusion_uptake_id) then
         deepest_layer = find_layer_no (g%root_depth,g%dlayer,max_layer)
         N_uptake_sum = - sum_real_array (g%dlt_NO3gsm_diffusion,
     :                                   deepest_layer)
         call return_no3_diffusion_uptake (variable_info
     :                             , N_uptake_sum)

      elseif (variable_info%id .eq. nh4_diffusion_uptake_id) then
         deepest_layer = find_layer_no (g%root_depth,g%dlayer,max_layer)
         N_uptake_sum = - sum_real_array (g%dlt_NH4gsm_diffusion,
     :                                   deepest_layer)
         call return_nh4_diffusion_uptake (variable_info
     :                             , N_uptake_sum)


      elseif (variable_info%id .eq. n_total_uptake_id) then
         deepest_layer = find_layer_no (g%root_depth,g%dlayer,max_layer)
         N_uptake_sum  = - sum_real_array (g%dlt_NO3gsm, deepest_layer)
     :                   - sum_real_array (g%dlt_NH4gsm, deepest_layer)
         call return_n_total_uptake (variable_info
     :                             , N_uptake_sum)

      elseif (variable_info%id .eq. no3_total_uptake_id) then
         deepest_layer = find_layer_no (g%root_depth,g%dlayer,max_layer)
         N_uptake_sum  = - sum_real_array (g%dlt_NO3gsm, deepest_layer)
         call return_no3_total_uptake (variable_info
     :                             , N_uptake_sum)

      elseif (variable_info%id .eq. nh4_total_uptake_id) then
         deepest_layer = find_layer_no (g%root_depth,g%dlayer,max_layer)
         N_uptake_sum  = - sum_real_array (g%dlt_NH4gsm, deepest_layer)
         call return_nh4_total_uptake (variable_info
     :                             , N_uptake_sum)

      elseif (variable_info%id .eq. n_cum_uptake_id) then

         biomass_n = (sum_real_array (g%n_green,    max_part)
     :             +  sum_real_array (g%n_senesced, max_part)
     :             +  sum_real_array (g%n_dead,     max_part))

         call return_n_cum_uptake (variable_info
     :                             , biomass_n)


      !--------------------------------------------------------
      !NEED THE FOLLOWING TO WORK WITH SOILPH

      else if (variable_info%id .eq. no3_uptake_id) then

         num_layers = count_of_real_vals (g%dlayer, max_layer)

         call fill_real_array(no3_uptake,0.0, max_layer)

         no3_uptake(:) =  g%dlt_NO3gsm(:) * gm2kg/sm2ha

         call return_no3_uptake (variable_info
     :                               , no3_uptake, num_layers)


      else if (variable_info%id .eq. nh4_uptake_id) then

         num_layers = count_of_real_vals (g%dlayer, max_layer)

         call fill_real_array(nh4_uptake,0.0, max_layer)

         nh4_uptake(:) =  g%dlt_Nh4gsm(:) * gm2kg/sm2ha

         call return_nh4_uptake (variable_info
     :                               , nh4_uptake, num_layers)
      !--------------------------------------------------------

      !soil N amount

      elseif (variable_info%id .eq. no3_tot_id) then
         deepest_layer = find_layer_no (g%root_depth, g%dlayer
     :                                , max_layer)
         NO3gsm_tot = sum_real_array (g%NO3gsm, deepest_layer)
         call return_no3_tot (variable_info
     :                             , NO3gsm_tot)


      elseif (variable_info%id .eq. nh4_tot_id) then
         deepest_layer = find_layer_no (g%root_depth, g%dlayer
     :                                , max_layer)
         apt_N_up = sum_real_array (g%NH4gsm, deepest_layer)
         call return_nh4_tot (variable_info
     :                             , apt_N_up)



      !----------------------------------------------------------
      !Nitrogen content

      elseif (variable_info%id .eq. hi_n_id) then

         biomass_n = (sum_real_array (g%n_green,    max_part)
     :             - g%n_green(root) - g%n_green(energy)
     :             +  sum_real_array (g%n_senesced, max_part)
     :             - g%n_senesced(root) - g%n_senesced(energy)
     :             + sum_real_array (g%n_dead, max_part)
     :             - g%n_dead(root) - g%n_dead(energy))

         hi = divide(g%n_green(grain), biomass_n, 0.0)

         call return_hi_n (variable_info
     :                             , hi)


      elseif (variable_info%id .eq. biomass_n_id) then

         biomass_n = (sum_real_array (g%n_green, max_part)
     :             - g%n_green(root) - g%n_green(energy)
     :             + sum_real_array (g%n_senesced, max_part)
     :             - g%n_senesced(root) - g%n_senesced(energy)
     :             + sum_real_array (g%n_dead, max_part)
     :             - g%n_dead(root) - g%n_dead(energy))

         call return_biomass_n (variable_info
     :                             , biomass_n)

      elseif (variable_info%id .eq. green_biomass_n_id) then
         biomass_n = (sum_real_array (g%n_green, max_part)
     :                - g%n_green(root) - g%n_green(energy))

         call return_green_biomass_n (variable_info
     :                             , biomass_n)

      elseif (variable_info%id .eq. stover_n_id) then

         apt_N_up = g%N_green(leaf)+g%n_green(stem)+g%n_green(flower)
     :       +g%N_senesced(leaf)+g%n_senesced(stem)+g%n_senesced(flower)
     :       +g%N_dead(leaf)+g%n_dead(stem)+g%n_dead(flower)

         call return_stover_n (variable_info
     :                             , apt_N_up)

      elseif (variable_info%id .eq. grain_n_id) then
         call return_grain_n (variable_info
     :                             , g%n_green(grain))

      elseif (variable_info%id .eq. gleaf_n_id) then
         call return_gleaf_n (variable_info
     :                             , g%n_green(leaf))

      elseif (variable_info%id .eq. dleaf_n_id) then
         call return_dleaf_n (variable_info
     :                             , g%n_senesced(leaf)
     :                             + g%n_dead(leaf))

      elseif (variable_info%id .eq. tleaf_n_id) then
         call return_tleaf_n (variable_info
     :                             , g%n_senesced(leaf)
     :                             + g%n_dead(leaf)
     :                             + g%n_green(leaf))

       elseif (variable_info%id .eq. stem_n_id) then
         call return_stem_n (variable_info
     :                             , g%n_green(stem))

       elseif (variable_info%id .eq. flower_n_id) then
         call return_flower_n (variable_info
     :                             , g%n_green(flower))

      elseif (variable_info%id .eq. groot_n_id) then
         call return_groot_n (variable_info
     :                             , g%n_green(root))

      elseif (variable_info%id .eq. droot_n_id) then
         call return_droot_n (variable_info
     :                             , g%n_senesced(root)
     :                             + g%n_dead(root))

      elseif (variable_info%id .eq. troot_n_id) then
         call return_troot_n (variable_info
     :                             , g%n_senesced(root)
     :                             + g%n_dead(root)
     :                             + g%n_green(root))


      !---------------------------------------------------------------
      !Nitrogen content arrays

      elseif (variable_info%id .eq. n_green_id) then
         call return_n_green (variable_info
     :                             , g%N_green
     :                             , max_part)

      elseif (variable_info%id .eq. n_senesced_id) then
         call return_n_senesced (variable_info
     :                             , g%N_senesced
     :                             , max_part)

      elseif (variable_info%id .eq. n_dead_id) then
         call return_n_dead (variable_info
     :                             , g%N_dead
     :                             , max_part)


      !-----------------------------------------------------------------
      !Nitrogne deltas

      elseif (variable_info%id .eq. dlt_n_green_id) then
         call return_dlt_n_green (variable_info
     :                             , g%dlt_N_green
     :                             , max_part)

      elseif (variable_info%id .eq. dlt_n_retrans_id) then
         call return_dlt_n_retrans (variable_info
     :                             , g%dlt_N_retrans
     :                             , max_part)

      elseif (variable_info%id .eq. dlt_n_detached_id) then
         call return_dlt_n_detached (variable_info
     :                             , g%dlt_N_detached
     :                             , max_part)

      elseif (variable_info%id .eq. dlt_n_dead_detached_id) then
         call return_dlt_n_dead_detached (variable_info
     :                             , g%dlt_N_dead_detached
     :                             , max_part)


      !-----------------------------------------------------------------
      !Nitrogen concentrations

      elseif (variable_info%id .eq. sln_id) then
         call return_sln (variable_info
     :                             , divide(
     :                    g%N_green(leaf), g%lai, 0.0))

      elseif (variable_info%id .eq. n_conc_stover_id) then
         N_conc = divide ((g%N_green(leaf)
     :                    + g%N_green(stem)
     :                    + g%N_green(flower))
     :                  , (g%dm_green(leaf)
     :                    + g%dm_green(stem)
     :                    + g%dm_green(flower))
     :                  , 0.0) * 100.

         call return_n_conc_stover (variable_info
     :                             , N_conc)

      elseif (variable_info%id .eq. n_conc_leaf_id) then
         N_conc = divide (g%N_green(leaf)
     :                  , g%dm_green(leaf)
     :                  , 0.0) * 100.

         call return_n_conc_leaf (variable_info
     :                             , N_conc)

      elseif (variable_info%id .eq. n_conc_stem_id) then
         N_conc = divide (g%N_green(stem)
     :                  , g%dm_green(stem)
     :                  , 0.0) * 100.

         call return_n_conc_stem (variable_info
     :                             , N_conc)

      elseif (variable_info%id .eq. n_conc_root_id) then
         N_conc = divide (g%N_green(root)
     :                  , g%dm_green(root)
     :                  , 0.0) * 100.

         call return_n_conc_root(variable_info
     :                             , N_conc)

      elseif (variable_info%id .eq. n_conc_grain_id) then
         N_conc = divide (g%N_green(grain)
     :                  , g%dm_green(grain)
     :                  , 0.0) * 100.

         call return_n_conc_grain (variable_info
     :                             , N_conc)


      elseif (variable_info%id .eq. n_conc_leaf_crit_id) then
         N_conc = g%N_conc_crit(leaf) * 100.

         call return_n_conc_leaf_crit (variable_info
     :                             , N_conc)

      elseif (variable_info%id .eq. n_conc_stem_crit_id) then
         N_conc = g%N_conc_crit(stem) * 100.

         call return_n_conc_stem_crit (variable_info
     :                             , N_conc)

      elseif (variable_info%id .eq. n_conc_flower_crit_id) then
         N_conc = g%N_conc_crit(flower) * 100.

         call return_n_conc_flower_crit (variable_info
     :                             , N_conc)

      elseif (variable_info%id .eq. n_conc_root_crit_id) then
         N_conc = g%N_conc_crit(root) * 100.

         call return_n_conc_root_crit (variable_info
     :                             , N_conc)

      elseif (variable_info%id .eq. n_conc_stover_crit_id) then
         N_conc = divide ((g%N_conc_crit(leaf)*g%dm_green(leaf)
     :                    + g%N_conc_crit(stem)*g%dm_green(stem))
     :                  , (g%dm_green(leaf)
     :                    + g%dm_green(stem))
     :                  , 0.0) * 100.

         call return_n_conc_stover_crit (variable_info
     :                             , N_conc)


      elseif (variable_info%id .eq. n_conc_leaf_max_id) then
         N_conc = g%N_conc_max(leaf) * 100.

         call return_n_conc_leaf_max (variable_info
     :                             , N_conc)

      elseif (variable_info%id .eq. n_conc_stem_max_id) then
         N_conc = g%N_conc_max(stem) * 100.

         call return_n_conc_stem_max (variable_info
     :                             , N_conc)

      elseif (variable_info%id .eq. n_conc_flower_max_id) then
         N_conc = g%N_conc_max(flower) * 100.

         call return_n_conc_flower_max (variable_info
     :                             , N_conc)

      elseif (variable_info%id .eq. n_conc_root_max_id) then
         N_conc = g%N_conc_max(root) * 100.

         call return_n_conc_root_max (variable_info
     :                             , N_conc)

      elseif (variable_info%id .eq. n_conc_stover_max_id) then
         N_conc = divide ((g%N_conc_max(leaf)*g%dm_green(leaf)
     :                   + g%N_conc_max(stem)*g%dm_green(stem))
     :                  , (g%dm_green(leaf)
     :                   + g%dm_green(stem))
     :                  , 0.0) * 100.

         call return_n_conc_stover_max (variable_info
     :                             , N_conc)

      elseif (variable_info%id .eq. n_conc_leaf_min_id) then
         N_conc = g%N_conc_min(leaf) * 100.

         call return_n_conc_leaf_min (variable_info
     :                             , N_conc)

      elseif (variable_info%id .eq. n_conc_stem_min_id) then
         N_conc = g%N_conc_min(stem) * 100.

         call return_n_conc_stem_min (variable_info
     :                             , N_conc)

      elseif (variable_info%id .eq. n_conc_flower_min_id) then
         N_conc = g%N_conc_min(flower) * 100.

         call return_n_conc_flower_min (variable_info
     :                             , N_conc)

      elseif (variable_info%id .eq. n_conc_root_min_id) then
         N_conc = g%N_conc_min(root) * 100.

         call return_n_conc_root_min (variable_info
     :                             , N_conc)

      elseif (variable_info%id .eq. n_conc_stover_min_id) then
         N_conc = divide ((g%N_conc_min(leaf)*g%dm_green(leaf)
     :                    + g%N_conc_min(stem)*g%dm_green(stem))
     :                  , (g%dm_green(leaf)
     :                    + g%dm_green(stem))
     :                  , 0.0) * 100.

         call return_n_conc_stover_min (variable_info
     :                             , N_conc)

      elseif (variable_info%id .eq. n_grain_pcnt_id) then
         grain_N_pcnt = divide (g%N_green(grain)
     :                        , g%dm_green(grain), 0.0)
     :                        * fract2pcnt
         call return_n_grain_pcnt (variable_info
     :                             , grain_N_pcnt)

      elseif (variable_info%id .eq. grain_protein_id) then
         grain_N_pcnt = divide (g%N_green(grain)
     :                        , g%dm_green(grain), 0.0)
     :                        * 100.0 * 5.71
         call return_grain_protein (variable_info
     :                             , grain_N_pcnt)


      !---------------------------------------------------
      !Nitrogen stress factors

      elseif (variable_info%id .eq. nfact_photo_id) then
         call return_nfact_photo (variable_info
     :                             , g%nfact_photo)

      elseif (variable_info%id .eq. nfact_pheno_id) then
         call return_nfact_pheno (variable_info
     :                             , g%nfact_pheno)

      elseif (variable_info%id .eq. nfact_expan_id) then
         call return_nfact_expan (variable_info
     :                             , g%nfact_expansion)

      elseif (variable_info%id .eq. nfact_tiller_id) then
         call return_nfact_tiller (variable_info
     :                             , g%nfact_tiller)

      elseif (variable_info%id .eq. nfact_grain_id) then
         call return_nfact_grain (variable_info
     :                             , g%nfact_grain_conc)

      elseif (variable_info%id .eq. nfact_grain_tot_id) then
         call return_nfact_grain_tot (variable_info
     :                             , g%cnd_grain_conc
     :                             , max_stage)




      !---------------------------------------------------
      ! Crop Phosphorus Variables
      ! -------------------------
      elseif (variable_info%id .eq. pfact_photo_id) then
         call return_pfact_photo (variable_info
     :                             , g%pfact_photo)

      elseif (variable_info%id .eq. pfact_pheno_id) then
         call return_pfact_pheno (variable_info
     :                             , g%pfact_pheno)

      elseif (variable_info%id .eq. pfact_expan_id) then
         call return_pfact_expan (variable_info
     :                             , g%pfact_expansion)

      elseif (variable_info%id .eq. p_demand_id) then
         ! really ought to do this properly
         call return_p_demand (variable_info
     :                             , g%p_demand*10.)

      elseif (variable_info%id .eq. plant_p_id) then
         call return_plant_p (variable_info
     :                             , g%plant_p)

      elseif (variable_info%id .eq. biomass_p_id) then
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

         call return_biomass_p (variable_info
     :                             , biomass_p)

      elseif (variable_info%id .eq. plant_p_max_id) then
         biomass     = sum_real_array (g%dm_green, max_part)
     :               + sum_real_array (g%dm_senesced, max_part)
     :               + sum_real_array (g%dm_dead, max_part)
         plant_p_max = biomass * g%P_conc_max

         call return_plant_p_max (variable_info
     :                             , plant_p_max)

      elseif (variable_info%id .eq. pconc_max_id) then
         call return_pconc_max (variable_info
     :                             , g%P_conc_max)

      elseif (variable_info%id .eq. pconc_id) then
         biomass     = sum_real_array (g%dm_green, max_part)
     :               + sum_real_array (g%dm_senesced, max_part)
     :               + sum_real_array (g%dm_dead, max_part)
        pconc = 100.0 * divide (g%plant_p
     :                 ,biomass
     :                 ,0.0)

         call return_pconc (variable_info
     :                             , pconc)




      else
         ! No matching ID
      endif



      return
      end

* ====================================================================
      logical function respondToSet (fromID,VariableID, variant)
* ====================================================================
      use CropModModule
      use ComponentInterfaceModule

      implicit none
      ml_external respondToSet

!+  Sub-Program Arguments
      integer, intent(in) :: fromID
      integer, intent(in)     :: VariableID
      integer, intent(in out) :: variant

*+  Purpose
*     Set one of our variables altered by some other module

*+  Changes
*      21-06-96 NIH Changed respond2set calls to collect calls

*+  Calls
      integer CropMod_solute_number

*+  Local Variables

*- Implementation Section ----------------------------------

      respondToSet = .true.
      return
      end

*================================================================
      subroutine Zero_Variables (param_init)
*================================================================
*+  Purpose
*     Zero crop variables & arrays

*+  Changes
*     010994 sc   specified and programmed
*     090695 psc  add row spacing = 0

*     000121 ew   generalised for all crops

*------------------------------------------------------------
      use CropModModule
      use ComponentInterfaceModule
      implicit none


*+  Constant Values
      character  my_name*(*) ! name of procedure
      parameter (my_name  = 'Zero_Variables')


*+  Argument Values
      logical param_init  !indicate whether model constants and parameters need to be initialised


*- Implementation Section ----------------------------------

      call push_routine (my_name)

      ! zero pools etc.

      call Zero_Daily_Variables ()


*=====GLOBAL VARIABLES =============================

       g%NO3(:) = 0.0
       g%NH4(:) = 0.0

      !co2 level

      g%co2level         = 0.0


      g%cum_photoperiod     =0.0
      g%cum_photop_day      =0.0



      !name
      g%plant_status     = " ";


      !general

      g%extinction_coeff    = 0.0

      g%row_spacing         = 0.0
      g%sowing_depth        = 0.0
c      g%year                = 0
c      g%day_of_year         = 0
      g%swdef_expansion     = 0.0
      g%swdef_photo         = 0.0
      g%swdef_pheno         = 0.0
      g%nfact_expansion     = 0.0
      g%nfact_photo         = 0.0
      g%nfact_grain_conc    = 0.0
      g%nfact_pheno         = 0.0
      g%temp_stress_photo   = 0.0
      g%swdef_fixation      = 0.0


      !climate
c      g%fr_intc_radn        = 0.0
c      g%latitude            = 0.0
c      g%radn                = 0.0
c      g%mint                = 0.0
c      g%maxt                = 0.0

c     CALL fill_real_array(g%soil_temp,0.0, 366)

      g%vpd                 = 0.0
      g%eo                  = 0.0
      g%accum_rad_10d       = 0.0

      call fill_real_array(g%rad_accum,0.0,10)

      !deficits
      call fill_real_array (g%cnd_photo,      0.0, max_stage)
      call fill_real_array (g%cnd_grain_conc, 0.0, max_stage)
      call fill_real_array (g%cswd_photo,     0.0, max_stage)
      call fill_real_array (g%cswd_expansion, 0.0, max_stage)
      call fill_real_array (g%cswd_pheno,     0.0, max_stage)

      !phenology
      g%current_stage  =0.0
      g%previous_stage =0.0
      g%canopy_height  =0.0
      g%phase_devel    =0.0
      g%cumvd          =0.0

      g%zadok_stage    =0.0


      call fill_real_array(g%tt_tot,         0.0, max_stage)
      call fill_real_array(g%phase_tt,       0.0, max_stage)
      call fill_real_array(g%tt_curv_tot,    0.0, max_stage)
      call fill_real_array(g%phase_tt_curv,  0.0, max_stage)
      call fill_real_array(g%tt_other_tot,   0.0, max_stage)
      call fill_real_array(g%phase_tt_other, 0.0, max_stage)
      call fill_real_array(g%heat_stress_tt, 0.0, max_stage)
      call fill_real_array(g%days_tot,       0.0, max_stage)

      !plant
      g%plants           =0.0
      g%grain_no         =0.0
      g%obs_grain_no_psm =0.0


      g%root_depth       =0.0
      g%cover_green      =0.0
      g%cover_sen        =0.0
      g%cover_dead       =0.0

      !dry matter
      g%radn_int            =0.0
      g%transp_eff          =0.0

      call fill_real_array(g%dm_stress_max,    0.0,max_stage)
      call fill_real_array(g%dm_plant_top_tot, 0.0,max_stage)

      call fill_real_array(g%dm_green_demand,  0.0,max_part)
      call fill_real_array(g%dm_dead,          0.0,max_part)
      call fill_real_array(g%dm_green,         0.0,max_part)
      call fill_real_array(g%dm_senesced,      0.0,max_part)
      call fill_real_array(g%dm_plant_min,     0.0,max_part)

      call fill_real_array(g%dm_green_retrans,     0.0,max_part)
      call fill_real_array(g%dm_green_retrans_pool,0.0,max_part)


      g%dm_green_grainno = 0.0

      !leaf area index
      g%slai                   =0.0
      g%tpla_today             =0.0
      g%tpla_yesterday         =0.0
      g%lai                    =0.0
      g%tlai_dead              =0.0


      !leaves
      g%tiller_count    =0
      g%tiller_kill_day =0

      g%leaf_no_min       =0.0
      g%leaf_no_final     =0.0
      g%tiller_no_pot     =0.0
      g%tiller_no_fertile =0.0
      g%swdef_lai_loss    =0.0
      g%lai_max_possible  =0.0

      call fill_real_array(g%leaf_no,       0.0, max_stage)
      call fill_real_array(g%node_no,       0.0, max_stage)
      call fill_real_array(g%leaf_no_dead,  0.0, max_stage)

      call fill_real_array(g%lai_equilib_light,0.0, 366)
      call fill_real_array(g%lai_equilib_water,0.0, 366)

      call fill_real_array(g%leaf_area,            0.0, max_leaf)
      call fill_real_array(g%tiller_area_max,      0.0, max_leaf)
      call fill_real_array(g%tiller_area_pot,      0.0, max_leaf)
      call fill_real_array(g%tiller_area_act,      0.0, max_leaf)
      call fill_real_array(g%tiller_area_act_stage,0.0, max_leaf)
      call fill_real_array(g%tiller_area_sen,      0.0, max_leaf)

      call fill_real_array(g%tt_tiller_emergence,  0.0, max_leaf)


      !plant_N
      g%n_fix_pot =0.0

      call fill_real_array(g%N_demand,   0.0,max_part)
      call fill_real_array(g%N_max,      0.0,max_part)
      call fill_real_array(g%N_dead,     0.0,max_part)
      call fill_real_array(g%N_green,    0.0,max_part)
      call fill_real_array(g%N_senesced, 0.0,max_part)
      call fill_real_array(g%N_conc_crit,0.0,max_part)
      call fill_real_array(g%N_conc_max, 0.0,max_part)
      call fill_real_array(g%N_conc_min, 0.0,max_part)


      g%no3_diffn_const = 0.0


      !root_profile
      g%num_layers =0

      g%sw_demand             =0.0
      g%sw_supply_sum         =0.0
      g%sw_supply_demand_ratio=0.0

c      call fill_real_array(g%dlayer,       0.0,max_layer)
c      call fill_real_array(g%dul_dep,      0.0,max_layer)
c      call fill_real_array(g%sat_dep,              0.0, max_layer)
c      call fill_real_array(g%sw_dep,       0.0,max_layer)
      call fill_real_array(g%sw_avail_pot, 0.0,max_layer)
      call fill_real_array(g%sw_avail,     0.0,max_layer)
      call fill_real_array(g%sw_supply,    0.0,max_layer)
      call fill_real_array(g%root_length,  0.0,max_layer)

      !output_totals
      g%flowering_date =0
      g%maturity_date  =0
      g%flowering_das  =0
      g%maturity_das   =0

      g%lai_max               =0.0
      g%transpiration_tot     =0.0
      g%N_uptake_tot          =0.0
      g%N_demand_tot          =0.0
      g%N_conc_act_stover_tot =0.0
      g%N_conc_crit_stover_tot=0.0
      g%N_uptake_grain_tot    =0.0
      g%N_uptake_stover_tot   =0.0

      !newwheat_block
      g%canopy_SLN      =0.0
      g%dm_green_tot_fi =0.0

      call fill_real_array(g%tt_tot_fm,0.0,max_stage)

      !swim_comms_var
      g%num_uptake_water =0
      g%num_uptake_no3   =0

      call fill_real_array(g%uptake_water,0.0,max_layer)
      call fill_real_array(g%uptake_no3,  0.0,max_layer)

      !ew added variables
      g%dm_seed_reserve  =0.0
      g%swdef_tiller     =0.0
      g%nfact_tiller     =0.0
      g%tiller_no        =0.0
      g%tiller_no_sen    =0.0
      g%tiller_no_sq     =0.0
      g%dm_tiller_pot    =0.0





      g%vernalisation         =0.0
      g%leaf_primodia_vern   =0.0


      g%vern_eff         =0.0
      g%photop_eff       =0.0
      g%leaf_primodia    =0.0
      g%lai_stage        =0.0
      g%tiller_tt_tot    =0.0

      call fill_real_array(g%plsc,                 0.0, max_leaf)


c      call fill_real_array(g%NO3gsm,            0.0,max_layer)
c      call fill_real_array(g%NO3gsm_min,        0.0,max_layer)
c      call fill_real_array(g%NH4gsm,               0.0, max_layer)
c      call fill_real_array(g%NH4gsm_min,           0.0, max_layer)
c      call fill_real_array(g%NO3ppm,               0.0, max_layer)
c      call fill_real_array(g%NH4ppm,               0.0, max_layer)

c      call fill_real_array(g%NO3gsm_diffn_pot,     0.0,max_layer)
c      call fill_real_array(g%NO3gsm_mflow_avail,   0.0,max_layer)

c      call fill_real_array(g%NH4gsm_diffn_pot,     0.0, max_layer)
c      call fill_real_array(g%NH4gsm_mflow_avail,   0.0, max_layer)

c      call fill_real_array(g%pot_extract_NO3gsm,   0.0, max_layer)
c      call fill_real_array(g%pot_extract_NH4gsm,   0.0, max_layer)

c      g%dlt_n_uptake_stover=0.0


!These should be set to 1.0 (no stress, otherwise averaging is confusing!!!)
      g%swdef_pheno       = 0.0
      g%swdef_expansion   = 0.0
      g%swdef_photo       = 0.0

      g%nfact_pheno       = 0.0
      g%nfact_expansion   = 0.0
      g%nfact_photo       = 0.0

      g%temp_stress_photo = 0.0
      g%nfact_grain_conc  = 0.0


      !;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      !maiz p part

      !...................maiz_p_real
      g%P_conc_max  =0.0
      g%P_conc_min  =0.0

      g%p_demand    =0.0
      g%plant_p     =0.0
      g%dlt_plant_p =0.0

      call fill_real_array(g%uptake_P,  0.0,max_layer)


      g%pfact_photo      =0.0
      g%pfact_pheno      =0.0
      g%pfact_expansion  =0.0
      g%pfact_grain      =0.0

      !...................maiz_p_int
      g%num_uptake_P     =0




      !SORGHUM
      g%nfract = 0.0




*==================== Varaibles from other module need not to be kept  ==============


      CALL fill_real_array(g%soil_temp,    0.0, 366)


      if (param_init) then


*==================== Varaibles from other module need to be kept  ==================

      g%year                = 0
      g%day_of_year         = 0

      g%latitude            = 0.0
      g%radn                = 0.0
      g%mint                = 0.0
      g%maxt                = 0.0

      g%fr_intc_radn        = 0.0

      call fill_real_array(g%dlayer,       0.0, max_layer)
      call fill_real_array(g%dul_dep,      0.0, max_layer)
      call fill_real_array(g%sat_dep,      0.0, max_layer)

      call fill_real_array(g%NO3gsm_min,   0.0, max_layer)
      call fill_real_array(g%NH4gsm_min,   0.0, max_layer)

      call fill_real_array(g%sw_dep,       0.0, max_layer)
      call fill_real_array(g%NH4gsm,       0.0, max_layer)
      call fill_real_array(g%NO3ppm,       0.0, max_layer)
      call fill_real_array(g%NH4ppm,       0.0, max_layer)
      call fill_real_array(g%NO3gsm,       0.0, max_layer)

*==================== PARAMETERS ======================================================

        !phenology_parameters


      p%tt_germ_to_emerg         =0.0
      p%tt_init_to_flag          =0.0
      p%tt_start_to_end_grain    =0.0
      p%tt_end_grain_to_maturity =0.0
      p%tt_ripe_to_harvest       =0.0


      p%tt_maturity_to_ripe     =0.0
      p%tt_flag_to_flower       =0.0
      p%tt_flower_to_start_grain=0.0
      p%tt_emerg_to_endjuv      =0.0
      p%tt_endjuv_to_init       =0.0
      p%tt_flower_to_maturity   =0.0
      p%pp_endjuv_to_init       =0.0
      p%photoperiod_crit1       =0.0
      p%photoperiod_crit2       =0.0
      p%photoperiod_slope       =0.0
      p%est_days_emerg_to_init  =0.0

      p%num_x_vfac_cumvd =0
      p%num_photoperiod  =0

      call fill_real_array(p%x_vfac_cumvd, 0.0, max_table)
      call fill_real_array(p%y_vfac,       0.0, max_table)
      call fill_real_array(p%photoperiod,  0.0, max_table)
      call fill_real_array(p%phase_tt_init,0.0, max_table)


      !plant harvest index
      p%num_hi_max_pot =0
      p%hi_incr        =0.0

      call fill_real_array(p%x_hi_max_pot_stress,0.0, max_table)
      call fill_real_array(p%y_hi_max_pot,       0.0, max_table)

      !leaf area
      p%dm_per_seed        =0.0
      p%main_stem_coef     =0.0
      p%tpla_prod_coef     =0.0
      p%tpla_inflection    =0.0
      p%spla_prod_coef     =0.0
      p%spla_intercept     =0.0


      !plant property
      p%head_grain_no_max  =0.0
      p%grain_gth_rate     =0.0

      p%num_stem_wt        =0

      call fill_real_array(p%x_stem_wt,0.0, max_table)
      call fill_real_array(p%y_height, 0.0, max_table)

      !root profile
      call fill_real_array(p%kl,    0.0, max_layer)
      call fill_real_array(p%ll_dep,0.0, max_layer)
      call fill_real_array(p%xf,    0.0, max_layer)

      !swim
      p%uptake_source = " "

      !maize
      p%hi_max_pot   =0.0


*================== Constants ======================================

      c%RUE_Max        = 0.0
      c%RUE_max_exist  = .FALSE.

      call fill_real_array(c%radn_diff_fr,       0.0, max_stage)
      call fill_real_array(c%rue_diff_modifier,  0.0, max_table)
      c%num_radn_diff_fr = 0


      !co2 level
      c%co2switch  = 0
      c%co2level   = 0.0

      call fill_real_array(c%co2_level_te,       0.0, max_stage)
      call fill_real_array(c%te_co2_modifier,    0.0, max_table)
      c%num_co2_level_te = 0

      call fill_real_array(c%co2_level_nconc,       0.0, max_stage)
      call fill_real_array(c%nconc_co2_modifier,    0.0, max_table)
      c%num_co2_level_nconc = 0


      c%use_average_photoperiod = 0



      c%crop_type =" "

      call fill_char_array(c%stage_names,' ', max_stage)



      !deficits
      c%num_temp_root       =0
      c%num_ws_root         =0
      c%num_sw_ratio        =0
      c%num_sw_demand_ratio =0
      c%num_sw_avail_ratio  =0
      c%num_N_conc_stage    =0

      !plant n conc
      c%twilight             =0.0
      c%N_conc_crit_grain    =0.0
      c%N_conc_max_grain     =0.0
      c%N_conc_min_grain     =0.0
      c%N_conc_crit_root     =0.0
      c%N_conc_max_root      =0.0
      c%N_conc_min_root      =0.0
      c%N_fact_photo         =0.0
      c%N_fact_pheno         =0.0
      c%N_fact_pheno_lb      =0.0
      c%N_fact_expansion     =0.0

      call fill_real_array(c%stage_code_list,       0.0, max_stage)
      call fill_real_array(c%zadok_stage_code_list, 0.0, max_stage)

      call fill_real_array(c%x_temp_root,         0.0, max_table)
      call fill_real_array(c%y_temp_root_fac,     0.0, max_table)
      call fill_real_array(c%x_ws_root,           0.0, max_table)
      call fill_real_array(c%y_ws_root_fac,       0.0, max_table)

      call fill_real_array(c%x_sw_ratio,          0.0, max_table)
      call fill_real_array(c%y_sw_fac_root,       0.0, max_table)
      call fill_real_array(c%x_sw_demand_ratio,   0.0, max_table)
      call fill_real_array(c%y_swdef_leaf,        0.0, max_table)
      call fill_real_array(c%x_sw_avail_ratio,    0.0, max_table)
      call fill_real_array(c%y_swdef_pheno,       0.0, max_table)
      call fill_real_array(c%x_stage_code,        0.0, max_stage)
      call fill_real_array(c%y_n_conc_crit_leaf,  0.0, max_stage)
      call fill_real_array(c%y_n_conc_max_leaf,   0.0, max_stage)
      call fill_real_array(c%y_n_conc_min_leaf,   0.0, max_stage)
      call fill_real_array(c%y_n_conc_crit_stem,  0.0, max_stage)
      call fill_real_array(c%y_n_conc_max_stem,   0.0, max_stage)
      call fill_real_array(c%y_n_conc_min_stem,   0.0, max_stage)
      call fill_real_array(c%y_n_conc_crit_flower,0.0, max_stage)
      call fill_real_array(c%y_n_conc_max_flower, 0.0, max_stage)
      call fill_real_array(c%y_n_conc_min_flower, 0.0, max_stage)
      call fill_real_array(c%y_n_conc_crit_root,  0.0, max_stage)
      call fill_real_array(c%y_n_conc_max_root,   0.0, max_stage)
      call fill_real_array(c%y_n_conc_min_root,   0.0, max_stage)
      call fill_real_array(c%N_init_conc,         0.0, max_part)
      call fill_real_array(c%N_sen_conc,          0.0, max_part)

      !plant_property/
      c%extinction_coef        =0.0
      c%extinction_coef_dead   =0.0
      c%extinction_coef_change =0.0
      c%root_extinction        =0.0

      c%row_spacing_default    =0.0

      call fill_real_array(c%x_row_spacing,       0.0, max_Table)
      call fill_real_array(c%y_extinct_coef,      0.0, max_Table)
      call fill_real_array(c%y_extinct_coef_dead, 0.0, max_Table)
      call fill_real_array(c%rue,                 0.0, max_stage)
      call fill_real_array(c%root_depth_rate,     0.0, max_stage)
      call fill_real_array(c%ratio_root_shoot,    0.0, max_stage)

      c%num_Row_spacing     =0
      c%num_temp_senescence =0

      call fill_real_array(c%x_stage_partitn,    0.0, max_stage)
      call fill_real_array(c%y_leaf_fraction,    0.0, max_stage)

      c%num_stage_partitn   =0

      call fill_real_array(c%x_temp_grain_nf    ,    0.0, max_table)
      call fill_real_array(c%y_temp_grain_nf_fac,    0.0, max_table)

      c%num_temp_grain_nf = 0

      call fill_real_array(c%x_temp_grain_dmf    ,    0.0, max_table)
      call fill_real_array(c%y_temp_grain_dmf_fac,    0.0, max_table)

      c%num_temp_grain_dmf =0

      c%max_grainn_fill_rate = 0.0


      c%grain_no_intercept = 0.0

      !coeff
      c%leaf_no_crit                =0.0
      c%tt_emerg_limit              =0.0
      c%days_germ_limit             =0.0
      c%swdf_pheno_limit            =0.0
      c%swdf_photo_limit            =0.0
      c%swdf_photo_rate             =0.0
      c%initial_root_depth          =0.0
      c%specific_root_length        =0.0
      c%sla_max                     =0.0
      c%sla_min                     =0.0
      c%tiller_coef                 =0.0
      c%tpla_inflection_ratio       =0.0
      c%initial_tpla                =0.0
      c%height_max                  =0.0
      c%height_stem_slope           =0.0
      c%svp_fract                   =0.0
      c%head_grain_no_crit          =0.0
      c%barren_crit                 =0.0
      c%pesw_germ                   =0.0
      c%grain_N_conc_min            =0.0
      c%seed_wt_min                 =0.0
      c%growth_rate_min             =0.0
      c%growth_rate_crit            =0.0
      c%leaf_no_at_emerg            =0.0
      c%photoperiod_base            =0.0
      c%photoperiod_optimum         =0.0
      c%NO3_diffn_const             =0.0
      c%shoot_lag                   =0.0
      c%shoot_rate                  =0.0
      c%leaf_app_rate               =0.0
      c%leaf_app_rate0              =0.0
      c%leaf_app_rate1              =0.0
      c%leaf_app_rate2              =0.0
      c%leaf_no_rate_change         =0.0
      c%dm_leaf_init                =0.0
      c%dm_root_init                =0.0
      c%dm_stem_init                =0.0
      c%dm_seed_reserve             =0.0
      c%dm_grain_embryo             =0.0
      c%leaf_init_rate              =0.0
      c%leaf_no_seed                =0.0
      c%dm_root_sen_frac            =0.0
      c%dm_leaf_sen_frac            =0.0
      c%dm_leaf_detach_frac         =0.0
      c%minsw                       =0.0
      c%swdf_grain_min              =0.0
      c%hi_min                      =0.0
      c%sfac_slope                  =0.0
      c%tfac_slope                  =0.0
      c%lai_sen_light               =0.0
      c%sw_fac_max                  =0.0
      c%temp_fac_min                =0.0
      c%frost_kill                  =0.0
      c%spla_slope                  =0.0
      c%sen_light_time_const        =0.0
      c%sen_water_time_const        =0.0
      c%sen_threshold               =0.0
      c%sen_radn_crit               =0.0
      c%sen_rate_water              =0.0
      c%sen_light_slope             =0.0
      c%grn_water_cont              =0.0
      c%frac_stem2flower            =0.0
      c%partition_rate_leaf         =0.0
      c%stem_trans_frac             =0.0
      c%leaf_trans_frac             =0.0
      c%htstress_coeff              =0.0
      c%temp_grain_crit_stress      =0.0
      c%leaf_no_dead_const          =0.0
      c%leaf_no_dead_slope          =0.0
      c%leaf_no_correction          =0.0
      c%x0_const                    =0.0
      c%x0_slope                    =0.0
      c%y0_const                    =0.0
      c%y0_slope                    =0.0
      c%a_const                     =0.0
      c%a_slope1                    =0.0
      c%a_slope2                    =0.0
      c%b_const                     =0.0
      c%b_slope1                    =0.0
      c%b_slope2                    =0.0
      c%imin                        =0.0
      c%iopt                        =0.0
      c%imax                        =0.0
      c%ioptr                       =0.0
      c%amin                        =0.0
      c%aopt                        =0.0
      c%amax                        =0.0
      c%aoptr                       =0.0
      c%head_grain_no_max_ub        =0.0
      c%grain_gth_rate_ub           =0.0
      c%tt_emerg_to_endjuv_ub       =0.0
      c%pp_endjuv_to_init_ub        =0.0
      c%tt_flower_to_maturity_ub    =0.0
      c%tt_maturity_to_ripe_ub      =0.0
      c%tt_flower_to_start_grain_ub =0.0
      c%tt_flag_to_flower_ub        =0.0
      c%ll_ub                       =0.0
      c%kl_ub                       =0.0
      c%sw_dep_ub                   =0.0
      c%sw_dep_lb                   =0.0
      c%NO3_ub                      =0.0
      c%NO3_lb                      =0.0
      c%NO3_min_ub                  =0.0
      c%NO3_min_lb                  =0.0
      c%leaf_no_min                 =0.0
      c%leaf_no_max                 =0.0
      c%latitude_ub                 =0.0
      c%latitude_lb                 =0.0
      c%maxt_ub                     =0.0
      c%maxt_lb                     =0.0
      c%mint_ub                     =0.0
      c%mint_lb                     =0.0
      c%radn_ub                     =0.0
      c%radn_lb                     =0.0
      c%dlayer_ub                   =0.0
      c%dlayer_lb                   =0.0
      c%dul_dep_ub                  =0.0
      c%dul_dep_lb                  =0.0


      c%num_shoot_nc_trans = 0
      call fill_real_array(c%x_shoot_nc_trans,    0.0, max_table)
      call fill_real_array(c%y_stem_trans_frac,   0.0, max_table)


      call fill_real_array(c%transp_eff_cf,    0.0, max_stage)
      call fill_real_array(c%n_fix_rate,       0.0, max_stage)
      call fill_real_array(c%x_node_no_app,    0.0, max_table)
      call fill_real_array(c%y_node_app_rate,  0.0, max_table)
      call fill_real_array(c%y_leaves_per_node,0.0, max_table)
      call fill_real_array(c%dead_detach_frac, 0.0, max_part)
      call fill_real_array(c%sen_detach_frac,  0.0, max_part)
      call fill_real_array(c%x_temp_senescence,0.0, max_table)
      call fill_real_array(c%y_senescence_fac, 0.0, max_table)
      call fill_real_array(c%x_ave_temp,       0.0, max_table)
      call fill_real_array(c%y_stress_photo,   0.0, max_table)
      call fill_real_array(c%x_temp,           0.0, max_table)
      call fill_real_array(c%y_tt,             0.0, max_table)
      call fill_real_array(c%x_weighted_temp,  0.0, max_table)
      call fill_real_array(c%y_plant_death,    0.0, max_table)
      call fill_real_array(c%x_temp_grain,     0.0, max_table)
      call fill_real_array(c%y_grain_rate,     0.0, max_table)
      call fill_real_array(c%x_temp_other,     0.0, max_table)
      call fill_real_array(c%y_tt_other,       0.0, max_table)
      call fill_real_array(c%x_temp_photo,     0.0, max_table)
      call fill_real_array(c%fasw_emerg,       0.0, max_table)
      call fill_real_array(c%rel_emerg_Rate,   0.0, max_table)


      call fill_real_array(c%x_vern_temp,   0.0, max_table)
      call fill_real_array(c%y_vern_fact,   0.0, max_table)
      c%num_vern_temp      =0


      c%num_temp           =0
      c%num_ave_temp       =0
      c%num_temp_grain     =0
      c%num_factors        =0
      c%num_temp_other     =0
      c%num_weighted_temp  =0
      c%num_temp_photo     =0
      c%num_x_leaf_no      =0
      c%num_x_lai          =0
      c%num_kvalue_rowspace=0
      c%num_plant_rld      =0
      c%num_node_no_app    =0
      c%num_fasw_emerg     =0
      c%year_ub            =0
      c%year_lb            =0

      !newmaize_block/
      call fill_real_array(c%grno_grate,        0.0, max_table)
      call fill_real_array(c%grno_fract,        0.0, max_table)
      call fill_real_array(c%x_leaf_no,         0.0, max_table)
      call fill_real_array(c%x_lai,             0.0, max_table)
      call fill_real_array(c%leaf_no_sla_max,   0.0, max_table)
      call fill_real_array(c%leaf_no_sla_min,   0.0, max_table)
      call fill_real_array(c%y_lai_sla_max,     0.0, max_table)
      call fill_real_array(c%lai_sla_min,       0.0, max_table)
      call fill_real_array(c%kvalue_rowspace,   0.0, max_table)
      call fill_real_array(c%kvalue_adjustment, 0.0, max_table)
      call fill_real_array(c%x_plant_rld,       0.0, max_table)
      call fill_real_array(c%y_rel_root_rate,   0.0, max_table)
      call fill_real_array(c%x_SLN_photo,       0.0, max_table)
      call fill_real_array(c%y_SLN_photo,       0.0, max_table)

      c%num_SLN_photo  =0
      c%newleaf_SLN    =0.0
      c%tt_base        =0.0
      c%tt_opt         =0.0

      !swim_comms_var
      c%n_supply_preference   = " "
      c%nh4_uptake_preference = 0.0

      !ew added variables
      c%NH4_ub            =0.0
      c%NH4_lb            =0.0
      c%NH4_min_ub        =0.0
      c%NH4_min_lb        =0.0

      c%module_switch   =0
      c%wat_switch      = " "
      c%phen_switch     = " "
      c%carb_switch     = " "
      c%part_switch     = " "
      c%leafno_switch   = " "
      c%tiller_switch   = " "
      c%can_switch      = " "
      c%root_switch     = " "
      c%sen_switch      = " "
      c%nit_switch      = " "
      c%phos_switch     = " "
      c%die_switch      = " "


*================== what is different from crop ======================


      !--------------------------------------------------
      !WHEAT SPECIFIC PARAMETERS AND CONSTANTS

      p%photoperiod_sensitivity      =0.0
      p%vernalisation_requirement    =0.0

      p%vern_sen_internal       =0.0
      p%photop_sen_internal     =0.0
      p%vern_sen                =0.0
      p%photop_sen              =0.0
      p%startgf_to_mat          =0.0

      p%dm_tiller_max =0.0

      call fill_real_array(p%tiller_curve,   0.0, max_leaf)
      call fill_real_array(p%tiller_tt_infl, 0.0, max_leaf)


      call fill_real_array(c%x_extinct_coeff_lai,   0.0, max_table)
      call fill_real_array(c%x_extinct_coeff_lai,   0.0, max_table)

      c%num_extinct_coeff_lai = 0

      c%extinct_coeff_post_anthesis = 0.0




      c%min_grain_nc_ratio =0.0  !minimum grain nc ratio to restrict grain dm filling, if nc ratio is less than this, no grain growth
      c%max_grain_nc_ratio =0.0
      c%grain_embryo_nc    =0.0  !nitrogen concentration (%)in grain embryo at start of grain filling
      c%max_kernel_weight  =0.0  ! mg/kernal


      c%start_grainno_dm_stage = 0 !stage starting dm accumulation for grain number determination
      c%end_grainno_dm_stage   = 0 !stage ending dm accumulation for grain number determination

      c%start_retrans_dm_stage = 0
      c%end_retrans_dm_stage   = 0



      call fill_real_array(c%x_sw_avail_ratio_tiller,   0.0, max_table)
      call fill_real_array(c%y_swdef_tiller,            0.0, max_table)

      c%num_sw_avail_ratio_tiller = 0





      call fill_real_array(c%x_fract_avail_sw,   0.0,  max_table)
      call fill_real_array(c%y_fact_diffn_const, 0.0, max_table)
      c%num_fract_avail_sw = 0


      c%max_tiller_area           = 0.0  !cm^2/tiller at a plant density of 100 plants/m^2
      c%tiller_area_tt_steepness  = 0.0  !the steepness of the tiller LAI-TT curve
      c%tiller_area_tt_inflection = 0.0  !the inflection point of the tiller LAI-TT curve (Cd)



      !--------------------------------------------------
      !SUNFLOWER SPECIFIC PARAMETERS AND CONSTANTS
      p%est_days_endjuv_to_init =0.0
      p%tt_fi_to_flag           =0.0
      p%rel_leaf_init_rate      =0.0
      p%tt_switch_stage         =0

      c%flower_trans_frac       =0.0
      c%grain_energy            =0.0
      c%frac_pod2grain          =0.0

      p%determinate_crop        =0
      p%mum_hi_incr_min_temp    =0
      p%num_node_lar            =0


      call fill_real_array(p%x_hi_incr_min_temp,    0.0, max_table)
      call fill_real_array(p%y_hi_incr_reduct_fac,  0.0, max_table)
      call fill_real_array(p%x_node_num_lar,        0.0, max_table)
      call fill_real_array(p%y_node_lar,            0.0, max_table)


      !--------------------------------------------------
      !MAIZE SPECIFIC PARAMETERS AND CONSTANTS
      !--------------------------------------------------
      c%advection_fact       =0.0    !
      c%fr_pesw_germ         =0.0    ! fraction of plant extractable soil water in seedling layer inadequate for germination (0-1)
      c%pot_leaf_area_option =0.0    ! option for pot leaf area routine to use  =0.0    ! 0 = CERES, 1 = SPLA
      c%sen_leaf_area_option =0.0    ! option for sen leaf area routine to use  =0.0    ! 0 = CERES, 1 = SPLA
      c%grain_yield_option   =0.0    ! option for grain yield routine to use                ! 0 = CERES, 1 = HI

      c%num_grno_grate       =0      ! Grno option
      c%tpla_min             =0.0

       !...................maiz_p_real

      c%k_pfact_expansion   =0.0
      c%k_pfact_photo       =0.0
      c%k_pfact_pheno       =0.0
      c%k_pfact_grain       =0.0

      call fill_real_array(c%P_stage_code, 0.0, max_stage)
      call fill_real_array(c%P_conc_max,   0.0, max_stage)
      call fill_real_array(c%P_conc_min,   0.0, max_stage)

      c%P_Uptake_Factor      =0.0

      !...................maiz_p_int
      c%num_p_conc_stage     =0


      !--------------------------------------------------
      !SORGHUM SPECIFIC PARAMETERS AND CONSTANTS
      !--------------------------------------------------

      call fill_real_array(c%N_target_conc, 0.0, max_part)

      c%floral_init_error = 0.0



      end if

      call pop_routine (my_name)
      return
      end



*================================================================
      subroutine Zero_Daily_Variables ()
*================================================================
*+  Purpose
*       Zero crop daily variables & arrays

*+  Changes
*     010994 sc   specified and programmed
*     000121 ew   generalised for all crops
*--------------------------------------------------------------
      use CropModModule
      use ComponentInterfaceModule
      implicit none


*+  Constant Values
      character  my_name*(*)  ! name of procedure
      parameter (my_name  = 'Zero_Daily_Variables')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      !phenology


      g%dlt_vernalisation =0.0
      g%dlt_leaf_primodia =0.0



      g%dlt_tt             =0.0
      g%dlt_tt_curv        =0.0
      g%dlt_tt_other       =0.0
      g%dlt_heat_stress_tt =0.0
      g%dlt_stage          =0.0
      g%dlt_canopy_height  =0.0

      !plant
      g%dlt_plants                     =0.0
      g%dlt_root_depth                 =0.0
      g%dlt_plants_failure_germ        =0.0
      g%dlt_plants_failure_emergence   =0.0
      g%dlt_plants_failure_leaf_sen    =0.0
      g%dlt_plants_failure_phen_delay  =0.0
      g%dlt_plants_death_seedling      =0.0
      g%dlt_plants_death_drought       =0.0
      g%dlt_plants_death_barrenness    =0.0
      g%dlt_plants_dead                =0.0

      !dry matter
      g%dlt_dm              =0.0
      g%dlt_dm_light        =0.0
      g%dlt_dm_water        =0.0
      g%dlt_dm_N            =0.0
      g%dlt_dm_stress_max   =0.0
      g%dlt_dm_grain_demand =0.0

      call fill_real_array(g%dlt_dm_green,         0.0,max_part)
      call fill_real_array(g%dlt_dm_senesced,      0.0,max_part)
      call fill_real_array(g%dlt_dm_detached,      0.0,max_part)
      call fill_real_array(g%dlt_dm_dead_detached, 0.0,max_part)
      call fill_real_array(g%dlt_dm_green_retrans, 0.0,max_part)

      call fill_real_array(g%dlt_dm_green_retrans_pool, 0.0,max_part)

      g%dlt_dm_green_grainno   =0.0

      !leaf area index
      g%dlt_slai               =0.0
      g%dlt_lai                =0.0
      g%dlt_lai_pot            =0.0
      g%dlt_lai_stressed       =0.0
      g%dlt_slai_detached      =0.0
      g%dlt_tlai_dead_detached =0.0
      g%dlt_slai_age           =0.0
      g%dlt_slai_light         =0.0
      g%dlt_slai_water         =0.0
      g%dlt_slai_nitrogen      =0.0
      g%dlt_slai_frost         =0.0

      !leaves
      g%dlt_leaf_no_pot  =0.0
      g%dlt_node_no_pot  =0.0
      g%dlt_leaf_no      =0.0
      g%dlt_leaf_no_dead =0.0
      g%dlt_tiller_no_pot=0.0
      g%dlt_tiller_no    =0.0
      g%dlt_stiller_no   =0.0

      call fill_real_array(g%dlt_tiller_area_pot,         0.0, max_leaf)
      call fill_real_array(g%dlt_tiller_area_act,         0.0, max_leaf)
      call fill_real_array(g%dlt_tiller_sen_area,         0.0, max_leaf)
      call fill_real_array(g%dlt_tiller_sen_area_age,     0.0, max_leaf)
      call fill_real_array(g%dlt_tiller_sen_area_light,   0.0, max_leaf)
      call fill_real_array(g%dlt_tiller_sen_area_water,   0.0, max_leaf)
      call fill_real_array(g%dlt_tiller_sen_area_nitrogen,0.0, max_leaf)

      !plant_N
      call fill_real_array(g%dlt_N_green,        0.0,max_part)
      call fill_real_array(g%dlt_N_senesced,     0.0,max_part)
      call fill_real_array(g%dlt_N_detached,     0.0,max_part)
      call fill_real_array(g%dlt_N_dead_detached,0.0,max_part)
      call fill_real_array(g%dlt_N_retrans,      0.0,max_part)

      call fill_real_array(g%NO3gsm_diffn_pot,     0.0,max_layer)
      call fill_real_array(g%NO3gsm_mflow_avail,   0.0,max_layer)
      call fill_real_array(g%dlt_NO3gsm_massflow,  0.0,max_layer)
      call fill_real_array(g%dlt_NO3gsm_diffusion, 0.0,max_layer)
      call fill_real_array(g%dlt_NO3gsm,         0.0,max_layer)

      call fill_real_array(g%NH4gsm_diffn_pot,     0.0, max_layer)
      call fill_real_array(g%NH4gsm_mflow_avail,   0.0, max_layer)
      call fill_real_array(g%dlt_NH4gsm_massflow,  0.0,max_layer)
      call fill_real_array(g%dlt_NH4gsm_diffusion, 0.0,max_layer)
      call fill_real_array(g%dlt_NH4gsm,           0.0, max_layer)


      call fill_real_array(g%pot_extract_NO3gsm,   0.0, max_layer)
      call fill_real_array(g%pot_extract_NH4gsm,   0.0, max_layer)

      g%dlt_n_uptake_stover=0.0


      !root_profile
      call fill_real_array(g%dlt_root_length,         0.0,max_layer)
      call fill_real_array(g%dlt_root_length_senesced,0.0,max_layer)
      call fill_real_array(g%dlt_sw_dep,              0.0,max_layer)

      !newwheat_block
      g%dlt_canopy_SLN  =0.0
      g%dlt_tt_fm       =0.0

      !ew added variables
      g%dlt_dm_pot       =0.0
      g%dlt_dm_leaf_pot  =0.0
      g%dlt_tiller_no_sen=0.0


      call fill_real_array(g%dlt_N_sen_supply,     0.0, max_part)
      call fill_real_array(g%dlt_N_sen_retrans,    0.0, max_part)
      call fill_real_array(g%dlt_dm_sen_retrans,   0.0, max_part)




       g%dlt_cumvd          =0.0




      call pop_routine (my_name)
      return
      end

*================================================================
      subroutine CropMod_Initialisation ()
*================================================================
*+  Purpose
*       Crop initialisation

*+  Changes
*     010994 sc   specified and programmed
*----------------------------------------------------------
      use CropModModule
      use ComponentInterfaceModule
      implicit none


*+  Calls
      character  CropMod_Version*52    ! function

*+  Constant Values
      character  my_name*(*)       ! name of procedure
      parameter (my_name  = 'CropMod_Initialisation')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      !Report the initialisation process
      call Write_string (' Initialising, '// CropMod_Version ())

      !Read the crop type and sub-module switches
      call Read_Module_Switches()


      !Read the crop specific constants

       call Crop_Read_Constants()

       call dynamic_var_registration()

      g%current_stage = real (plant_end)
      g%plant_status = status_out

      call pop_routine (my_name)
      return
      end

*=====================================================================
      character*(*) function CropMod_Version ()
*=====================================================================
*     Purpose
*     Return version number of crop module

*     Changes
*     011092 jngh specified and programmed
*     220896 jngh removed version control macro
*     220499 ew used for wheat

*-----Variable declaration---------------------------------

      use CropModModule
      use ComponentInterfaceModule
      implicit none


*     Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'CropMod_Version')

      character  version_number*(*)    ! version number of module
      parameter (version_number = 'V1.0 2000.01.21')
*-----Implementation Section -------------------------------

      call push_routine (my_name)

      CropMod_Version =  version_number

      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine Read_Module_Switches ()
*     ===========================================================
      use CropModModule
      use ComponentInterfaceModule
      implicit none

*+  Purpose
*       CropMod initialisation - reads crop type and module switches

*+  Changes
*     000121  ew programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'Read_Module_Switches')
*
      character  section_name*(*)
      parameter (section_name = 'constants')

      integer   convertor
      parameter (convertor = 111111111)


*+  Local Variables
      integer    numvals               !number of values returned
      character  switch*10             !switch convertor
      logical    found

*- Implementation Section ----------------------------------

      call push_routine (my_name)



      !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      !REPORT READING, AND READ THE CROP TYPE
      !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      call write_string (new_line//'    - Reading constants')

      !Read the crop type
      found = read_parameter (
     :                       section_name,
     :                      'crop_type',
     :                      c%crop_type)


          c%wat_switch    = '111111111'
          c%phen_switch   = '111111111'
          c%leafno_switch = '111111111'
          c%carb_switch   = '111111111'
          c%part_switch   = '111111111'
          c%tiller_switch = '111111111'
          c%can_switch    = '111111111'
          c%root_switch   = '111111111'
          c%sen_switch    = '111111111'
          c%nit_switch    = '111111111'
          c%phos_switch   = '000000000'
          c%die_switch    = '111111111'

      if (c%crop_type .eq. 'wheat') then
          c%wat_switch    = '111111111'
          c%phen_switch   = '111121111'
          c%leafno_switch = '111111111'
          c%carb_switch   = '111111111'
          c%part_switch   = '111111111'
          c%tiller_switch = '111111111'
          c%can_switch    = '111111111'
          c%root_switch   = '111111111'
          c%sen_switch    = '111311111'
          c%nit_switch    = '111111111'
          c%phos_switch   = '000000000'
          c%die_switch    = '111111111'
      end if


      if (c%crop_type .eq. 'sunflower') then
          c%wat_switch    = '111111111'
          c%phen_switch   = '411141111'
          c%leafno_switch = '141411111'
          c%carb_switch   = '111111111'
          c%part_switch   = '444111111'
          c%tiller_switch = '000000000'
          c%can_switch    = '044111111'
          c%root_switch   = '111111111'
          c%sen_switch    = '411111111'
          c%nit_switch    = '111114411'
          c%phos_switch   = '000000000'
          c%die_switch    = '111111111'
      end if


      if (c%crop_type .eq. 'sorghum') then
          c%wat_switch    = '111111111'
          c%phen_switch   = '511141111'
          c%leafno_switch = '151511111'
          c%carb_switch   = '111111111'
          c%part_switch   = '455111111'
          c%tiller_switch = '000000000'
          c%can_switch    = '054111111'
          c%root_switch   = '111111111'
          c%sen_switch    = '511511111'
          c%nit_switch    = '515550551'
          c%phos_switch   = '000000000'
          c%die_switch    = '111111111'
      end if

      if (c%crop_type .eq. 'maize') then
          c%wat_switch    = '111111111'
          c%phen_switch   = '611111111'
          c%leafno_switch = '161511111'
          c%carb_switch   = '111111111'
          c%part_switch   = '666611111'
          c%tiller_switch = '000000000'
          c%can_switch    = '166111111'
          c%root_switch   = '121111111'
          c%sen_switch    = '611111111'
          c%nit_switch    = '116114411'  !'166114411'
          c%phos_switch   = '111100000'
          c%die_switch    = '611111111'
      end if

      !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      ! READ MODULE SWITCHES
      !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      found = read_parameter (section_name
     :                    , 'module_switch'
     :                    , c%module_switch
     :                    , 0, 3,.true.)

      !If module_switch is not specified, use default 1
      if (.not.found) then
         c%module_switch = 0
      endif


      !If module_switch is not zero, use the module switch
      !all the sub switches have no effect
      if (c%module_switch .ne. 0) then

         write(switch, '(G10)') c%module_switch*convertor

         c%wat_switch     = switch(2:10)
         c%phen_switch    = switch(2:10)
         c%carb_switch    = switch(2:10)
         c%part_switch    = switch(2:10)
         c%leafno_switch  = switch(2:10)
         c%tiller_switch  = switch(2:10)
         c%can_switch     = switch(2:10)
         c%root_switch    = switch(2:10)
         c%sen_switch     = switch(2:10)
         c%nit_switch     = switch(2:10)
         c%phos_switch    = switch(2:10)
         c%die_switch     = switch(2:10)

      !If module_switch is zero, use the sub module switches
      else

          switch = c%wat_switch
          found = read_parameter (section_name
     :                     , 'wat_switch'
     :                     , c%wat_switch,.true.)
          if (.not.found)  c%wat_switch = switch

          switch = c%phen_switch
          found = read_parameter (section_name
     :                     , 'phen_switch'
     :                     , c%phen_switch, .true.)
          if (.not.found)  c%phen_switch = switch

          switch = c%carb_switch
          found = read_parameter (section_name
     :                     , 'carb_switch'
     :                     , c%carb_switch,.true.)
          if (.not.found)  c%carb_switch = switch

          switch = c%part_switch
          found = read_parameter (section_name
     :                     , 'part_switch'
     :                     , c%part_switch,.true.)
          if (.not.found)  c%part_switch = switch

          switch = c%leafno_switch
          found = read_parameter (section_name
     :                     , 'leafno_switch'
     :                     , c%leafno_switch,.true.)
          if (.not.found)  c%leafno_switch = switch

          switch = c%tiller_switch
          found = read_parameter (section_name
     :                     , 'tiller_switch'
     :                     , c%tiller_switch,.true.)
          if (.not.found)  c%tiller_switch = switch

          switch = c%can_switch
          found = read_parameter (section_name
     :                     , 'can_switch'
     :                     , c%can_switch,.true.)
          if (.not.found)  c%can_switch = switch

          switch = c%root_switch
          found = read_parameter (section_name
     :                     , 'root_switch'
     :                     , c%root_switch,.true.)
          if (.not.found)  c%root_switch = switch

          switch = c%sen_switch
          found = read_parameter (section_name
     :                     , 'sen_switch'
     :                     , c%sen_switch,.true.)
          if (.not.found)  c%sen_switch = switch

          switch = c%nit_switch
          found = read_parameter (section_name
     :                     , 'nit_switch'
     :                     , c%nit_switch,.true.)
          if (.not.found)  c%nit_switch = switch


          switch = c%phos_switch
          found = read_parameter (section_name
     :                     , 'phos_switch'
     :                     , c%phos_switch,.true.)
          if (.not.found) c%phos_switch = switch


          switch = c%die_switch
          found = read_parameter (section_name
     :                     , 'die_switch'
     :                     , c%die_switch,.true.)
          if (.not.found)  c%die_switch = switch


      endif


      call pop_routine (my_name)
      return
      end

*     ===========================================================
      subroutine Crop_Read_Constants ()
*     ===========================================================
      use CropModModule
      use ComponentInterfaceModule
      implicit none

*+  Purpose
*       Crop initialisation - reads constants from constants file

*+  Changes
*     010994 sc   specified and programmed
*     070495 psc added extra constants (leaf_app etc.)
*     110695 psc added soil temp effects on plant establishment
*     270995 scc added leaf area options

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'Crop_Read_Constants')
*
      character  section_name*(*)
      parameter (section_name = 'constants')


*+  Local Variables
      integer    numvals               !number of values returned


*- Implementation Section ----------------------------------

      call push_routine (my_name)


      if (c%crop_type.eq.'wheat') then

         call Read_Constants      ()
         call Read_Constants_Wheat()

c      elseif (c%crop_type.eq.'sunflower') then
c
c         call Read_Constants      ()
c         call Read_Constants_Sunf ()


c      elseif (c%crop_type.eq.'sorghum') then
c         call Read_Constants_Sorghum ()

c      elseif (c%crop_type.eq.'maize') then
c         call Read_Constants_Maize ()


      else

         call Read_Constants_Wheat()

      endif

      call pop_routine (my_name)
      return
      end

*     ===========================================================
      subroutine Read_Constants ()
*     ===========================================================
      use CropModModule
      use ComponentInterfaceModule
      implicit none

*+  Purpose
*       Crop initialisation - reads constants from constants file

*+  Changes
*     010994 sc   specified and programmed
*     070495 psc added extra constants (leaf_app etc.)
*     110695 psc added soil temp effects on plant establishment
*     270995 scc added leaf area options

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'Read_Constants')
*
      character  section_name*(*)
      parameter (section_name = 'constants')


*+  Local Variables
      integer    numvals               !number of values returned
      logical    found

*- Implementation Section ----------------------------------

      call push_routine (my_name)


c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c       VARIABLES ADDED new
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      found = read_parameter (section_name
     :                    , 'grain_no_intercept'
     :                    , c%grain_no_intercept
     :                    , -100000.0, 100000.0,.true.)


         found = read_parameter (section_name
     :                   , 'x_shoot_nc_trans'
     :                   , c%x_shoot_nc_trans, c%num_shoot_nc_trans
     :                   , 0.0, 10.0,.true.)

         found = read_parameter (section_name
     :                   , 'y_stem_trans_frac'
     :                   , c%y_stem_trans_frac, c%num_shoot_nc_trans
     :                   , 0.0, 1.0,.true.)



         found = read_parameter (section_name
     :                   , 'x_temp_grain_dmf'
     :                   , c%x_temp_grain_dmf, c%num_temp_grain_dmf
     :                   , 0.0, 50.0,.true.)

         found = read_parameter (section_name
     :                   , 'y_temp_grain_dmf_fac'
     :                   , c%y_temp_grain_dmf_fac, c%num_temp_grain_dmf
     :                   , 0.0, 1.0,.true.)


      found = read_parameter (section_name
     :                    , 'max_grainn_fill_rate'
     :                    , c%max_grainn_fill_rate
     :                    , 0.0, 100.0,.true.)

         found = read_parameter (section_name
     :                   , 'x_temp_grain_nf'
     :                   , c%x_temp_grain_nf, c%num_temp_grain_nf
     :                   , 0.0, 50.0,.true.)

         found = read_parameter (section_name
     :                   , 'y_temp_grain_nf_fac'
     :                   , c%y_temp_grain_nf_fac, c%num_temp_grain_nf
     :                   , 0.0, 1.0,.true.)





         found = read_parameter (section_name
     :                   , 'radn_diff_fr'
     :                   , c%radn_diff_fr, c%num_radn_diff_fr
     :                   , 0.0, 1.0,.true.)

         found = read_parameter (section_name
     :                   , 'rue_diff_modifier'
     :                   , c%rue_diff_modifier, c%num_radn_diff_fr
     :                   , 0.0, 1.0,.true.)

      found = read_parameter (section_name
     :                    , 'RUE_max'
     :                    , c%RUE_max
     :                    , 0.0, 10.0,.true.)



      if ((.not.found).and. (c%num_radn_diff_fr.ne.0.0)) then
            c%RUE_max_exist = .TRUE.
      else
            c%RUE_max_exist = .false.
      endif


      found = read_parameter (section_name
     :                    , 'use_average_photoperiod'
     :                    , c%use_average_photoperiod
     :                    , -50, 50,.true.)

      found = read_parameter (section_name
     :                    , 'leaf_app_rate0'
     :                    , c%leaf_app_rate0
     :                    , 0.0, 200.0,.true.)


c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c       VARIABLES ADDED FOR CLIMATE CHANGE
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


      found = read_parameter (section_name
     :                    , 'co2level'
     :                    , c%co2level
     :                    , 0.0, 1000.0,.true.)


      if (.not.found) then
         c%co2switch = 0

         found = read_parameter (section_name
     :                   , 'co2_level_te'
     :                   , c%co2_level_te, c%num_co2_level_te
     :                   , 0.0, 1000.0,.true.)

         found = read_parameter (section_name
     :                   , 'te_co2_modifier'
     :                   , c%te_co2_modifier, c%num_co2_level_te
     :                   , 0.0, 10.0,.true.)


         found = read_parameter (section_name
     :                   , 'co2_level_nconc'
     :                   , c%co2_level_nconc, c%num_co2_level_nconc
     :                   , 0.0, 1000.0,.true.)

         found = read_parameter (section_name
     :                   , 'nconc_co2_modifier'
     :                   , c%nconc_co2_modifier, c%num_co2_level_nconc
     :                   , 0.0, 10.0,.true.)
      else
          c%co2switch = 1

         found = read_parameter (section_name
     :                   , 'co2_level_te'
     :                   , c%co2_level_te, c%num_co2_level_te
     :                   , 0.0, 1000.0)

         found = read_parameter (section_name
     :                   , 'te_co2_modifier'
     :                   , c%te_co2_modifier, c%num_co2_level_te
     :                   , 0.0, 10.0)


         found = read_parameter (section_name
     :                   , 'co2_level_nconc'
     :                   , c%co2_level_nconc, c%num_co2_level_nconc
     :                   , 0.0, 1000.0)

         found = read_parameter (section_name
     :                   , 'nconc_co2_modifier'
     :                   , c%nconc_co2_modifier, c%num_co2_level_nconc
     :                   , 0.0, 10.0)
      end if


c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c       CROP PHENOLOGY: DEVELOPMENT PARAMETERS
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      found = read_parameter (section_name
     :                     , 'stage_names'
     :                     , c%stage_names, numvals)

      found = read_parameter (section_name
     :                     , 'stage_code'
     :                     , c%stage_code_list, numvals
     :                     , 0.0, 1000.0)

      found = read_parameter (section_name
     :                     , 'zadok_stage'
     :                     , c%zadok_stage_code_list, numvals
     :                     , 0.0, 1000.0,.true.)

      !Germination
      found = read_parameter (section_name
     :                    , 'pesw_germ'
     :                    , c%pesw_germ
     :                    , 0.0, 1.0)

      !Emergence
      found = read_parameter (section_name
     :                    , 'shoot_lag'
     :                    , c%shoot_lag
     :                    , 0.0, 100.0)

      found = read_parameter (section_name
     :                    , 'shoot_rate'
     :                    , c%shoot_rate
     :                    , 0.0, 100.0)

       found = read_parameter (section_name
     :                   , 'fasw_emerg'
     :                   , c%fasw_emerg, c%num_fasw_emerg
     :                   , 0.0, 1.0,.true.)

      found = read_parameter (section_name
     :                   , 'rel_emerg_rate'
     :                   , c%rel_emerg_rate, c%num_fasw_emerg
     :                   , 0.0, 1.0,.true.)

           if (c%num_fasw_emerg.eq.0 ) then
               c%num_fasw_emerg    = 2
               c%fasw_emerg    (1) = 0.0
               c%fasw_emerg    (2) = 1.0
               c%rel_emerg_rate(1) = 1.0
               c%rel_emerg_rate(2) = 1.0
           end if

      !PHOTOPERIOD
      found = read_parameter (section_name
     :                   , 'twilight'
     :                   , c%twilight
     :                   , -90.0, 90.0)

      found = read_parameter (section_name
     :                   , 'photoperiod_optimum'
     :                   , c%photoperiod_optimum
     :                   , 0.0, 24.0,.true.)

      !Vernalisation
      found = read_parameter (section_name
     :                     , 'x_vern_temp'
     :                     , c%x_vern_temp, c%num_vern_temp
     :                     , -10.0, 100.0,.true.)

      found = read_parameter (section_name
     :                     , 'y_vern_fact'
     :                     , c%y_vern_fact, c%num_vern_temp
     :                     , -10.0, 100.0,.true.)


      !Thermal time caluclation
      found = read_parameter (section_name
     :                     , 'x_temp'
     :                     , c%x_temp, c%num_temp
     :                     , -10.0, 100.0)

      found = read_parameter (section_name
     :                     , 'y_tt'
     :                     , c%y_tt, c%num_temp
     :                     , -10.0, 100.0)

      !Leaf development
      found = read_parameter (section_name
     :                    , 'leaf_no_at_emerg'
     :                    , c%leaf_no_at_emerg
     :                    , 0.0, 100.0)

      found = read_parameter (section_name
     :                    , 'leaf_no_seed'
     :                    , c%leaf_no_seed
     :                    , 0.0, 100.0)

      found = read_parameter (section_name
     :                    , 'leaf_init_rate'
     :                    , c%leaf_init_rate
     :                    , 0.0, 100.0)

      found = read_parameter (section_name
     :                    , 'leaf_app_rate'
     :                    , c%leaf_app_rate
     :                    , 0.0, 100.0,.true.)

      found = read_parameter (section_name
     :                   , 'leaf_no_min'
     :                   , c%leaf_no_min
     :                   , 0.0, 100.0)

      found = read_parameter (section_name
     :                   , 'leaf_no_max'
     :                   , c%leaf_no_max
     :                   , 0.0, 100.0)


      found = read_parameter (section_name
     :                    , 'leaf_app_rate1'
     :                    , c%leaf_app_rate1
     :                    , 0.0, 100.0)

      found = read_parameter (section_name
     :                    , 'leaf_app_rate2'
     :                    , c%leaf_app_rate2
     :                    , 0.0, 100.0)

      found = read_parameter (section_name
     :                    , 'leaf_no_rate_change'
     :                    , c%leaf_no_rate_change
     :                    , 0.0, 30.0)


c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c       LEAF AREA GROWTH
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      found = read_parameter (section_name
     :                    , 'sla_min'
     :                    , c%sla_min
     :                    , 0.0, 100000.0)

      found = read_parameter (section_name
     :                    , 'sla_max'
     :                    , c%sla_max
     :                    , 0.0, 100000.0)

      !leaf_area_init
      found = read_parameter (section_name
     :                    , 'initial_tpla'
     :                    , c%initial_tpla
     :                    , 0.0, 100000.0)


C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C      PHOTOSYNTHESIS AND RADIATION USE EFFICIENCY (RUE)
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      !STAGE DEPENDENT RUE
      found = read_parameter (section_name
     :                     , 'rue'
     :                     , c%rue, numvals
     :                     , 0.0, 1000.0)


      !Temperature response of photosynthesis
      found = read_parameter (section_name
     :                     , 'x_ave_temp'
     :                     , c%x_ave_temp, c%num_ave_temp
     :                     , 0.0, 100.0)

      found = read_parameter (section_name
     :                     , 'y_stress_photo'
     :                     , c%y_stress_photo, c%num_ave_temp
     :                     , 0.0, 1.0)


c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c       BIOMASS INITIATION, PARTITION AND TRANSLOCATION
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      !DM initiation
      found = read_parameter (section_name
     :                    , 'dm_leaf_init'
     :                    , c%dm_leaf_init
     :                    , 0.0, 1000.0)

      found = read_parameter (section_name
     :                    , 'dm_root_init'
     :                    , c%dm_root_init
     :                    , 0.0, 1000.0)

      found = read_parameter (section_name
     :                    , 'dm_stem_init'
     :                    , c%dm_stem_init
     :                    , 0.0, 1000.0)


      found = read_parameter (section_name
     :                    , 'start_grainno_dm_stage'
     :                    , c%start_grainno_dm_stage
     :                    , 3, 8,.true.)

       if (.not.found)  c%start_grainno_dm_stage = 3


      found = read_parameter (section_name
     :                    , 'end_grainno_dm_stage'
     :                    , c%end_grainno_dm_stage
     :                    , 3, 8,.true.)

       if (.not.found)  c%end_grainno_dm_stage = 8


      found = read_parameter (section_name
     :                    , 'start_retrans_dm_stage'
     :                    , c%start_retrans_dm_stage
     :                    , 3, 8,.true.)

       if (.not.found)  c%start_retrans_dm_stage = 3


      found = read_parameter (section_name
     :                    , 'end_retrans_dm_stage'
     :                    , c%end_retrans_dm_stage
     :                    , 3, 8,.true.)

       if (.not.found)  c%end_retrans_dm_stage = 8


      !DM retranslocation
      found = read_parameter (section_name
     :                    , 'stem_trans_frac'
     :                    , c%stem_trans_frac
     :                    , 0.0, 1.0)

      found = read_parameter (section_name
     :                    , 'leaf_trans_frac'
     :                    , c%leaf_trans_frac
     :                    , 0.0, 1.0)


      !Biomass partitioning
      found = read_parameter (section_name
     :                     , 'ratio_root_shoot'
     :                     , c%ratio_root_shoot, numvals
     :                     , 0.0, 1000.0)




      found = read_parameter (section_name
     :                     , 'x_stage_partitn'
     :                     , c%x_stage_partitn, c%num_stage_partitn
     :                     , 0.0, 12.0,.true.)

      found = read_parameter (section_name
     :                     , 'y_leaf_fraction'
     :                     , c%y_leaf_fraction, c%num_stage_partitn
     :                     , 0.0, 1.0,.true.)




       found = read_parameter (section_name
     :                    , 'grn_water_cont'
     :                    , c%grn_water_cont
     :                    , 0.0, 1.0)


c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c          ROOT DEPTH AND ROOT LENGTH GROWTH
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      !root depth
      found = read_parameter (section_name
     :                     , 'root_depth_rate'
     :                     , c%root_depth_rate, numvals
     :                     , 0.0, 1000.0)


      found = read_parameter (section_name
     :                     , 'x_temp_root'
     :                     , c%x_temp_root, c%num_temp_root
     :                     , 0.0, 50.0,.true.)

      found = read_parameter (section_name
     :                     , 'y_temp_root_fac'
     :                     , c%y_temp_root_fac, c%num_temp_root
     :                     , 0.0, 1.0,.true.)

           if (c%num_temp_root.eq.0 ) then
               c%num_temp_root      = 2
               c%x_temp_root    (1) = 0.0
               c%x_temp_root    (2) = 1.0
               c%y_temp_root_fac(1) = 1.0
               c%y_temp_root_fac(2) = 1.0
           end if

      found = read_parameter (section_name
     :                     , 'x_ws_root'
     :                     , c%x_ws_root, c%num_ws_root
     :                     , 0.0, 1.0,.true.)

      found = read_parameter (section_name
     :                     , 'y_ws_root_fac'
     :                     , c%y_ws_root_fac, c%num_ws_root
     :                     , 0.0, 1.0,.true.)

           if (c%num_ws_root.eq.0 ) then
               c%num_ws_root      = 2
               c%x_ws_root    (1) = 0.0
               c%x_ws_root    (2) = 1.0
               c%y_ws_root_fac(1) = 1.0
               c%y_ws_root_fac(2) = 1.0
           end if

      found = read_parameter (section_name
     :                     , 'x_sw_ratio'
     :                     , c%x_sw_ratio, c%num_sw_ratio
     :                     , 0.0, 100.0)

      found = read_parameter (section_name
     :                     , 'y_sw_fac_root'
     :                     , c%y_sw_fac_root, c%num_sw_ratio
     :                     , 0.0, 100.0)


      !Root length calculation
      found = read_parameter (section_name
     :                    , 'specific_root_length'
     :                    , c%specific_root_length
     :                    , 0.0, 1.e6)

      found = read_parameter (section_name
     :                     , 'x_plant_rld'
     :                     , c%x_plant_rld, c%num_plant_rld
     :                     , 0.0, 0.5)
      found = read_parameter (section_name
     :                     , 'y_rel_root_rate'
     :                     , c%y_rel_root_rate, c%num_plant_rld
     :                     , 0.0, 1.0)



c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c        WATER RELATIONS AND WATER STRESS FACTORS
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


      found = read_parameter (section_name
     :                     , 'transp_eff_cf'
     :                     , c%transp_eff_cf, numvals
     :                     , 0.0, 1.0)

      found = read_parameter (section_name
     :                    , 'svp_fract'
     :                    , c%svp_fract
     :                    , 0.0, 1.0)

      !Water stress factors
      found = read_parameter (section_name
     :                     , 'x_sw_demand_ratio'
     :                     , c%x_sw_demand_ratio, c%num_sw_demand_ratio
     :                     , 0.0, 100.0)

      found = read_parameter (section_name
     :                     , 'y_swdef_leaf'
     :                     , c%y_swdef_leaf, c%num_sw_demand_ratio
     :                     , 0.0, 100.0)

      found = read_parameter (section_name
     :                     , 'x_sw_avail_ratio'
     :                     , c%x_sw_avail_ratio, c%num_sw_avail_ratio
     :                     , 0.0, 100.0)

      found = read_parameter (section_name
     :                     , 'y_swdef_pheno'
     :                     , c%y_swdef_pheno, c%num_sw_avail_ratio
     :                     , 0.0, 100.0)


      found = read_parameter (section_name
     :                     , 'x_sw_avail_ratio_tiller'
     :                     , c%x_sw_avail_ratio_tiller
     :                     , c%num_sw_avail_ratio_tiller
     :                     , 0.0, 100.0,.true.)

      found = read_parameter (section_name
     :                     , 'y_swdef_tiller'
     :                     , c%y_swdef_tiller
     :                     , c%num_sw_avail_ratio_tiller
     :                     , 0.0, 100.0,.true.)

           if (c%num_sw_avail_ratio_tiller .eq. 0) then
               c%num_sw_avail_ratio_tiller  = 2
               c%x_sw_avail_ratio_tiller(1) = 0.0
               c%x_sw_avail_ratio_tiller(2) = 1.0
               c%y_swdef_tiller         (1) = 1.0
               c%y_swdef_tiller         (2) = 1.0
           end if


C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C         NITROGEN RELATIONS, UPTAKE AND STRESS FACTORS
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      found = read_parameter (section_name
     :                     , 'n_supply_preference'
     :                     , c%n_supply_preference)



      found = read_parameter (section_name
     :                     , 'nh4_uptake_preference'
     :                     , c%nh4_uptake_preference
     :                     , 0.0, 1.0,.true. )

       if (.not.found) then
           c%nh4_uptake_preference = 0.0
       end if


      found = read_parameter (section_name
     :                    , 'no3_diffn_const'
     :                    , c%NO3_diffn_const
     :                    , 0.0, 100.0)



      found = read_parameter (section_name
     :                     , 'x_fract_avail_sw'
     :                     , c%x_fract_avail_sw
     :                     , c%num_fract_avail_sw
     :                     , 0.0, 1.0,.true.)

      found = read_parameter (section_name
     :                     , 'y_fact_diffn_const'
     :                     , c%y_fact_diffn_const
     :                     , c%num_fract_avail_sw
     :                     , 0.0, 1000.0,.true.)

           if (c%num_fract_avail_sw .eq. 0) then
               c%num_fract_avail_sw    = 2
               c%x_fract_avail_sw  (1) = 0.0
               c%x_fract_avail_sw  (2) = 1.0
               c%y_fact_diffn_const(1) = 1.0
               c%y_fact_diffn_const(2) = 1.0
           end if



      found = read_parameter (section_name
     :                     , 'n_fix_rate'
     :                     , c%n_fix_rate, numvals
     :                     , 0.0, 1.0)

      !Nitrogen stress factors
      found = read_parameter (section_name
     :                   , 'N_fact_photo'
     :                   , c%N_fact_photo
     :                   , 0.0, 100.0)

      found = read_parameter (section_name
     :                   , 'N_fact_pheno'
     :                   , c%N_fact_pheno
     :                   , 0.0, 100.0)

      found = read_parameter (section_name
     :                   , 'N_fact_expansion'
     :                   , c%N_fact_expansion
     :                   , 0.0, 100.0)



      !Niotrogen concentration limits
      found = read_parameter (section_name
     :                     , 'x_stage_code'
     :                     , c%x_stage_code, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      found = read_parameter (section_name
     :                     , 'y_n_conc_min_leaf'
     :                     , c%y_N_conc_min_leaf, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      found = read_parameter (section_name
     :                     , 'y_n_conc_crit_leaf'
     :                     , c%y_N_conc_crit_leaf, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      found = read_parameter (section_name
     :                     , 'y_n_conc_max_leaf'
     :                     , c%y_N_conc_max_leaf, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      found = read_parameter (section_name
     :                     , 'y_n_conc_min_stem'
     :                     , c%y_N_conc_min_stem, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      found = read_parameter (section_name
     :                     , 'y_n_conc_crit_stem'
     :                     , c%y_N_conc_crit_stem, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      found = read_parameter (section_name
     :                     , 'y_n_conc_max_stem'
     :                     , c%y_N_conc_max_stem, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      found = read_parameter (section_name
     :                     , 'y_n_conc_min_flower'
     :                     , c%y_N_conc_min_flower, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      found = read_parameter (section_name
     :                     , 'y_n_conc_crit_flower'
     :                     , c%y_N_conc_crit_flower, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      found = read_parameter (section_name
     :                     , 'y_n_conc_max_flower'
     :                     , c%y_N_conc_max_flower, c%num_N_conc_stage
     :                     , 0.0, 100.0)


      found = read_parameter (section_name
     :                     , 'y_n_conc_min_root'
     :                     , c%y_N_conc_min_root, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      found = read_parameter (section_name
     :                     , 'y_n_conc_crit_root'
     :                     , c%y_N_conc_crit_root, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      found = read_parameter (section_name
     :                     , 'y_n_conc_max_root'
     :                     , c%y_N_conc_max_root, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      found = read_parameter (section_name
     :                   , 'n_conc_min_grain'
     :                   , c%N_conc_min_grain
     :                   , 0.0, 100.0)

      found = read_parameter (section_name
     :                   , 'n_conc_crit_grain'
     :                   , c%N_conc_crit_grain
     :                   , 0.0, 100.0)

      found = read_parameter (section_name
     :                   , 'n_conc_max_grain'
     :                   , c%N_conc_max_grain
     :                   , 0.0, 100.0)


      !Initial concentration
      found = read_parameter (section_name
     :                     , 'n_init_conc'
     :                     , c%n_init_conc, numvals
     :                     , 0.0, 100.0)

      !N concentration in senesed parts
      found = read_parameter (section_name
     :                     , 'n_sen_conc'
     :                     , c%n_sen_conc, numvals
     :                     , 0.0, 1.0)

C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C        CROP SENESCENCE AND DETACHMENT
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      found = read_parameter (section_name
     :                    , 'dm_root_sen_frac'
     :                    , c%dm_root_sen_frac
     :                    , 0.0, 1.0)

      found = read_parameter (section_name
     :                    , 'dm_leaf_sen_frac'
     :                    , c%dm_leaf_sen_frac
     :                    , 0.0, 1.0)

       found = read_parameter (section_name
     :                    , 'dead_detach_frac'
     :                    , c%dead_detach_frac, numvals
     :                    , 0.0, 1.0)

      found = read_parameter (section_name
     :                    , 'dm_leaf_detach_frac'
     :                    , c%dm_leaf_detach_frac
     :                    , 0.0, 1.0)


C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C        CROP FAILURE
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      found = read_parameter (section_name
     :                    , 'leaf_no_crit'
     :                    , c%leaf_no_crit
     :                    , 0.0, 100.0)

      found = read_parameter (section_name
     :                    , 'tt_emerg_limit'
     :                    , c%tt_emerg_limit
     :                    , 0.0, 365.0)

      found = read_parameter (section_name
     :                    , 'swdf_photo_limit'
     :                    , c%swdf_photo_limit
     :                    , 0.0, 100.0)

      found = read_parameter (section_name
     :                    , 'swdf_photo_rate'
     :                    , c%swdf_photo_rate
     :                    , 0.0, 1.0)


      found = read_parameter (section_name
     :                    , 'days_germ_limit'
     :                    , c%days_germ_limit
     :                    , 0.0, 365.0,.true.)
      if (.not.found) c%days_germ_limit = 1000 !basically no limit


      found = read_parameter (section_name
     :                    , 'swdf_pheno_limit'
     :                    , c%swdf_pheno_limit
     :                    , 0.0, 100.0,.true.)
      if (.not.found) c%swdf_pheno_limit = 1000 !basically no limit


C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C        CROP DEATH
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      found = read_parameter (section_name
     :                     , 'x_weighted_temp'
     :                     , c%x_weighted_temp, c%num_weighted_temp
     :                     , 0.0, 100.0)

      found = read_parameter (section_name
     :                     , 'y_plant_death'
     :                     , c%y_plant_death, c%num_weighted_temp
     :                     , 0.0, 100.0)

C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C          VALUE LIMITS - MAX AND MINS
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      !-----------------------------------------------------------------
      !Canopy height
      found = read_parameter (section_name
     :                    , 'canopy_height_max'
     :                    , c%height_max
     :                    , 0.0, 5000.0)

      !-----------------------------------------------------------------
      !ROOT PARAMETERS
      found = read_parameter (section_name
     :                    , 'll_ub'
     :                    , c%ll_ub
     :                    , 0.0, 1000.0)

      found = read_parameter (section_name
     :                    , 'kl_ub'
     :                    , c%kl_ub
     :                    , 0.0, 1000.0)

      !-----------------------------------------------------------------
      !SOIL WATER
      found = read_parameter (section_name
     :                    , 'minsw'
     :                    , c%minsw
     :                    , 0.0, 1000.0)


      !-----------------------------------------------------------------
      !OTHER VARIABLES - limits set to check inputs

      found = read_parameter (section_name
     :                    , 'year_ub'
     :                    , c%year_ub
     :                    , 1800, 2100)

      found = read_parameter (section_name
     :                    , 'year_lb'
     :                    , c%year_lb
     :                    , 1800, 2100)

      found = read_parameter (section_name
     :                    , 'latitude_ub'
     :                    , c%latitude_ub
     :                    , -90.0, 90.0)

      found = read_parameter (section_name
     :                    , 'latitude_lb'
     :                    , c%latitude_lb
     :                    , -90.0, 90.0)

      found = read_parameter (section_name
     :                    , 'maxt_ub'
     :                    , c%maxt_ub
     :                    , 0.0, 60.0)

      found = read_parameter (section_name
     :                    , 'maxt_lb'
     :                    , c%maxt_lb
     :                    , -10.0, 60.0)

      found = read_parameter (section_name
     :                    , 'mint_ub'
     :                    , c%mint_ub
     :                    , 0.0, 40.0)

      found = read_parameter (section_name
     :                    , 'mint_lb'
     :                    , c%mint_lb
     :                    , -100.0, 100.0)

      found = read_parameter (section_name
     :                    , 'radn_ub'
     :                    , c%radn_ub
     :                    , 0.0, 100.0)

      found = read_parameter (section_name
     :                    , 'radn_lb'
     :                    , c%radn_lb
     :                    , 0.0, 100.0)

      found = read_parameter (section_name
     :                    , 'dlayer_ub'
     :                    , c%dlayer_ub
     :                    , 0.0, 10000.0)

      found = read_parameter (section_name
     :                    , 'dlayer_lb'
     :                    , c%dlayer_lb
     :                    , 0.0, 10000.0)

      found = read_parameter (section_name
     :                    , 'dul_dep_ub'
     :                    , c%dul_dep_ub
     :                    , 0.0, 10000.0)

      found = read_parameter (section_name
     :                    , 'dul_dep_lb'
     :                    , c%dul_dep_lb
     :                    , 0.0, 10000.0)

                                ! 8th block
      found = read_parameter (section_name
     :                    , 'sw_dep_ub'
     :                    , c%sw_dep_ub
     :                    , 0.0, 10000.0)

      found = read_parameter (section_name
     :                    , 'sw_dep_lb'
     :                    , c%sw_dep_lb
     :                    , 0.0, 10000.0)

      found = read_parameter (section_name
     :                    , 'no3_ub'
     :                    , c%NO3_ub
     :                    , 0.0, 100000.0)

      found = read_parameter (section_name
     :                    , 'no3_lb'
     :                    , c%NO3_lb
     :                    , 0.0, 100000.0)

      found = read_parameter (section_name
     :                    , 'no3_min_ub'
     :                    , c%NO3_min_ub
     :                    , 0.0, 100000.0)

      found = read_parameter (section_name
     :                    , 'no3_min_lb'
     :                    , c%NO3_min_lb
     :                    , 0.0, 100000.0)

cew - added this section

      found = read_parameter (section_name
     :                    , 'nh4_ub'
     :                    , c%NH4_ub
     :                    , 0.0, 100000.0)

      found = read_parameter (section_name
     :                    , 'nh4_lb'
     :                    , c%NH4_lb
     :                    , 0.0, 100000.0)

      found = read_parameter (section_name
     :                    , 'nh4_min_ub'
     :                    , c%NH4_min_ub
     :                    , 0.0, 100000.0)

      found = read_parameter (section_name
     :                    , 'nh4_min_lb'
     :                    , c%NH4_min_lb
     :                    , 0.0, 100000.0)


      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine Read_Constants_Wheat ()
*     ===========================================================
      use CropModModule
      use ComponentInterfaceModule
      implicit none

*+  Purpose
*       Crop initialisation - reads constants from constants file

*+  Changes
*     010994 sc   specified and programmed
*     070495 psc added extra constants (leaf_app etc.)
*     110695 psc added soil temp effects on plant establishment
*     270995 scc added leaf area options

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'Read_Constants_Wheat')
*
      character  section_name*(*)
      parameter (section_name = 'constants')


*+  Local Variables
      integer    numvals               !number of values returned
      logical    found

*- Implementation Section ----------------------------------

      call push_routine (my_name)



c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c       LEAF AREA GROWTH - TILLER BASED
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


      found = read_parameter (section_name
     :                    , 'max_tiller_area'
     :                    , c%max_tiller_area
     :                    , 0.0, 500.0)


      found = read_parameter (section_name
     :                    , 'tiller_area_tt_steepness'
     :                    , c%tiller_area_tt_steepness
     :                    , 0.0, 0.05)


      found = read_parameter (section_name
     :                    , 'tiller_area_tt_inflection'
     :                    , c%tiller_area_tt_inflection
     :                    , 0.0, 600.0)



C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C      EXTINCTION COEFFICIENT
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      !LAI determined extinction coefficient
      found = read_parameter (section_name
     :               , 'x_extinct_coeff_lai'
     :               , c%x_extinct_coeff_lai, c%num_extinct_coeff_lai
     :               , 0.0, 20.0)

      found = read_parameter (section_name
     :               , 'y_extinct_coeff_lai'
     :               , c%y_extinct_coeff_lai, c%num_extinct_coeff_lai
     :               , 0.0, 10.0)


      found = read_parameter (section_name
     :                    , 'extinct_coeff_post_anthesis'
     :                    , c%extinct_coeff_post_anthesis
     :                    , 0.0, 10.0)



c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c       BIOMASS INITIATION, PARTITION AND TRANSLOCATION
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      found = read_parameter (section_name
     :                    , 'dm_seed_reserve'
     :                    , c%dm_seed_reserve
     :                    , 0.0, 1000.0,.true.)

      found = read_parameter (section_name
     :                    , 'dm_grain_embryo'
     :                    , c%dm_grain_embryo
     :                    , 0.0, 1000.0,.true.)


      found = read_parameter (section_name
     :                    , 'max_kernel_weight'
     :                    , c%max_kernel_weight
     :                    , 0.0, 60.0,.true.)



c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c        WATER RELATIONS AND WATER STRESS FACTORS
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C         NITROGEN RELATIONS, UPTAKE AND STRESS FACTORS
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      found = read_parameter (section_name
     :                   , 'min_grain_nc_ratio'
     :                   , c%min_grain_nc_ratio
     :                   , 0.0, 1.0)


      found = read_parameter (section_name
     :                   , 'max_grain_nc_ratio'
     :                   , c%max_grain_nc_ratio
     :                   , 0.0, 1.0)


      found = read_parameter (section_name
     :                   , 'grain_embryo_nconc'
     :                   , c%grain_embryo_nc
     :                   , 0.0, 0.50)


      call pop_routine (my_name)
      return
      end



*     ================================================================
      subroutine Get_Other_Variables ()
*     ================================================================
      use CropModModule
      use ComponentInterfaceModule
      implicit none

*+  Purpose
*      Get the values of variables/arrays from other modules.

*+  Assumptions
*      assumes variable has the following format
*         <variable_name> = <variable_value/s> (<units>)

*+  Changes
*     010994 sc   specified and programmed
*     140896 jngh modified fr_intc_radn name to inclued a suffix of module name
*     000121 ew   generalised for all crops

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Get_Other_Variables')

*+  Local Variables
      integer    layer                 ! layer number
      integer    numvals               ! number of values put into array
      real       dlayer(max_layer)     ! soil layer depths (mm)
      real       NO3(max_layer)        ! soil NO3 content (kg/ha)
      real       NO3_min(max_layer)    ! soil NO3 minimum (kg/ha)
      real       NH4(max_layer)        ! soil NH4 content (kg/ha)     -EW ADDED
      real       NH4_min(max_layer)    ! soil NH4 minimum (kg/ha)     -EW ADDED
      character  module_name*(Max_module_name_size) ! module name
      real       soil_temp             ! soil surface temperature (oC)
      real       profile_depth         ! depth of soil profile (mm)
      real       root_depth_new        ! new root depth (mm)
      logical    found

*- Implementation Section ----------------------------------

      call push_routine (my_name)


      !-------------------------------------------------------------------
      ! Co2 and climate change

       if (c%co2switch .ne. 0) then

          if (c%co2level.gt.0.0) then
             g%co2level = c%co2level

          else
             found = get_co2( co2_id,
     :                               g%co2level,.true.)

             if (.not.found) c%co2switch = 0

          endif

       end if


      !use the observed or calculated grain number from other module
      found = get_obs_grainno_psm(obs_grainno_psm_id
     :                           , g%obs_grain_no_psm
     :                           ,.true.)


      !-------------------------------------------------------------------
      ! Year and date
       found = get_day ( day_id,
     :                      g%day_of_year)

      found = get_year ( year_id
     :                       , g%year)

      !-------------------------------------------------------------------
      ! climate
      found = get_latitude ( latitude_id
     :                     , g%latitude)

      found = get_maxt ( maxt_id
     :                     , g%maxt)

      found = get_mint ( mint_id
     :                     , g%mint)

      found = get_radn ( radn_id
     :                     , g%radn)

      !-------------------------------------------------------------------
      ! canopy

!      found = get_radn_int (radn_int_id
!     :                           , g%fr_intc_radn,.true.)

      !-------------------------------------------------------------------
      !Soil evap and temperature
      !mjr eo read in to limit demand in crop routines

      found = get_eo ( eo_id
     :                     , g%eo,.true.)

      !Get soil temperature
c      found = get_variable ( 'soil_temp'
c     :                     , soil_temp, numvals
c     :                     , 0.0, 80.0,.true.)


      found = get_maxt_soil_surface (maxt_soil_surface_id
     :                     , soil_temp,.true.)


      if (numvals.eq.0) then
         ! soil temp not supplied
      else
        call crop_store_value (g%day_of_year, g%year,
     .                          g%soil_temp, soil_temp)
      endif


c+!!!!!!!! what to do if no waterbalance variables found

      !-------------------------------------------------------------------
      !soil profile
      found = get_dlayer ( dlayer_id
     :                       , dlayer, numvals)

      if (g%num_layers.eq.0) then

         ! we assume dlayer hasn't been initialised yet.
         do layer = 1, numvals
            g%dlayer(layer) = dlayer(layer)
         enddo
         g%num_layers = numvals

      else

         !dlayer may be changed from its last setting
         profile_depth = sum_real_array (dlayer, numvals)

         if (g%root_depth.gt.profile_depth) then

           !Redistribute the root
           root_depth_new = profile_depth
           call Crop_root_redistribute
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

         do layer = 1, numvals
            p%ll_dep(layer) = divide (p%ll_dep(layer)
     :                              , g%dlayer(layer), 0.0)
     :                      * dlayer(layer)

            g%dlayer(layer) = dlayer(layer)
         enddo

         g%num_layers = numvals
      endif


      !-------------------------------------------------------------------
      !soil water and nitrogen

      !cew - added the sat_dep reading
      found = get_sat_dep ( sat_dep_id
     :                       , g%sat_dep,numvals)

      found = get_dul_dep ( dul_dep_id
     :                       , g%dul_dep, numvals)

      found = get_sw_dep ( sw_dep_id
     :                       , g%sw_dep, numvals)

                                ! soil nitrogen pools
      found = get_no3 ( no3_id
     :                     , NO3, numvals,.true.)
      if (.not.found) then
         ! we have no N supply - make non-limiting.
         call fill_real_array (NO3, 10000.0, g%num_layers)
      else
      endif

      do layer = 1, g%num_layers
         g%NO3gsm(layer) = NO3(layer) * kg2gm /ha2sm
      enddo

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      g%NO3(:) = NO3(:)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      found = get_no3_min ( no3_min_id
     :                     , NO3_min, numvals,.true.)
      do layer = 1, g%num_layers
         g%NO3gsm_min(layer) = NO3_min(layer) * kg2gm /ha2sm
      enddo



      !cew - added this nh4 pools
       found = get_nh4 ( nh4_id
     :                     , NH4, numvals,.true.)
      if (.not.found) then
            ! we have no N supply - make non-limiting.
         call fill_real_array (NH4, 10000.0, g%num_layers)
      else
      endif
      do layer = 1, g%num_layers
         g%NH4gsm(layer) = NH4(layer) * kg2gm /ha2sm
      end do

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      g%NH4(:) = NH4(:)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC



      found = get_nh4_min (nh4_min_id
     :                     , NH4_min, numvals,.true.)
      do layer = 1, g%num_layers
         g%NH4gsm_min(layer) = NH4_min(layer) * kg2gm /ha2sm
      end do


       found = get_no3ppm (no3ppm_id
     :                     , NO3, numvals,.true.)

      do layer = 1, g%num_layers
         g%NO3ppm(layer) = NO3(layer)
      end do


       found = get_nh4ppm (nh4ppm_id
     :                     , NH4, numvals,.true.)


      do layer = 1, g%num_layers
         g%NH4ppm(layer) = NH4(layer)
      end do



      call pop_routine (my_name)
      return
      end


!     ===========================================================
      subroutine dynamic_var_registration()
!     ===========================================================
      use CropModModule
      use ComponentInterfaceModule
      implicit none


!+  Purpose
!      Perform all registrations and zeroing

!+  Sub-Program Arguments

*+  Local Variables

      character real_type*(*)
      parameter (real_type = '<type kind="single" array="F"/>')

      character regname*100

!- Implementation Section ----------------------------------

cdsg081001      call get_current_module (module_name)

cdsg081001            regname = 'fr_intc_radn_'//module_name
            regname = 'fr_intc_radn_wheat'
            g%fr_intc_radn_ID = add_registration
     :          (GetVariableReg,
     :           regname,
     :           real_type)

      return
      end


*     ===========================================================
      subroutine Start_Crop (variant)
*     ===========================================================
      use CropModModule
      use StringModule
      use ComponentInterfaceModule
      implicit none

*+  Purpose
*       Start crop using parameters specified in passed record

*+  Changes
*     010994 sc   specified and programmed
*     090695 psc  add row spacing read
*     220896 jngh changed extract to collect
*                 removed data_record from argument

*+  Sub-Program Arguments

      integer, intent(in out) :: variant


*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'Start_Crop')

*+  Local Variables
      character  cultivar*20           ! name of cultivar
      integer    numvals               ! number of values found in array
      character  string*200            ! output string
      character  sow_data*200          ! input sowing data from Manager
      character  value*100
      real       value2
      real       value3
*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call unpack_sow(variant, sow_data)
      call get_char_variable(sow_data,'cultivar',value)
      cultivar = trim(value)
      call get_real_variable(sow_data,'plants',value2)
      g%plants=value2
      call get_real_variable(sow_data,'sowing_depth',value3)
      g%sowing_depth = value3
c            cultivar = 'hartog'
c            g%plants = 100.
c            g%sowing_depth = 50.
            g%row_spacing = c%row_spacing_default
            g%tiller_no_fertile = 0.0
      !-----------------------------------------------------------
      !Read sowing information
      !-----------------------------------------------------------
      call Write_string ( 'Sowing initiate')
cjh      if (data_record.ne.blank) then

c         call collect_char_var ('cultivar', '()'
c     :                        , cultivar, numvals)
c
c         call collect_real_var ('plants', '()'
c     :                        , g%plants, numvals, 0.0, 400.0)
c
c         call collect_real_var (
c     :                          'sowing_depth', '(mm)'
c     :                        , g%sowing_depth, numvals
c     :                        , 0.0, 100.0)

c         call collect_real_var_optional (
c     :                          'row_spacing', '(m)'
c     :                        , g%row_spacing, numvals
c     :                        , 0.0, 2.0)
c


         !scc added FTN 11/10/95
c         call collect_real_var_optional (
c     :                      'tiller_no_fertile', '()'
c     :                    , g%tiller_no_fertile, numvals
c     :                    , 0.0, 10.0)

      !-----------------------------------------------------------
      !Report sowing information
      !-----------------------------------------------------------
         call write_string ( new_line//new_line)

         string = '                 Crop Sowing Data'
         call write_string (string)

         string = '    ------------------------------------------------'
         call write_string (string)
         call write_string (
     :           '    Sowing  Depth Plants Spacing Cultivar    FTN')
         call write_string (
     :           '    Day no   mm     m^2     m     Name       no')

         string = '    ------------------------------------------------'
         call write_string ( string)

         write (string, '(3x, i7, 3f7.1, 1x, a10,1x,f7.2)')
     :                   g%day_of_year, g%sowing_depth
     :                 , g%plants, g%row_spacing, cultivar
     :                 , g%tiller_no_fertile
         call write_string (string)

         string = '    ------------------------------------------------'
         call write_string (string)


      !-----------------------------------------------------------
      !Read root profile parameters
      !-----------------------------------------------------------
         call Read_Root_Params ()

         !Read root parameters: ll, kl, xf and uptake_source

      !-----------------------------------------------------------
      !Read Culitvar information
      !-----------------------------------------------------------
        ! get cultivar parameters
        call Crop_Read_Cultivar_Params (cultivar)

      !-----------------------------------------------------------
      !Set the plant alive and stage equals sowing
      !-----------------------------------------------------------
         g%current_stage = real(sowing)
         g%plant_status = status_alive

cjh      else
            ! report empty sowing record
cjh         call fatal_error (err_user, 'No sowing criteria supplied')
cjh      endif

      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine Read_Root_Params ()
*     ===========================================================
      use CropModModule
      use ComponentInterfaceModule
      implicit none

*+  Purpose
*       Get root profile parameters

*+  Changes
*       090994 sc   specified and programmed
*     210395 jngh   changed from Maize_section to a parameters section
*     000121   ew   generalised

*+  Calls

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Read_Root_Params')
*
      character  section_name*(*)
      parameter (section_name = 'parameters')

*+  Local Variables
      integer    layer                 ! layer number
      real       ll (max_layer)        ! lower limit of plant-extractable
                                       ! soil water for soil layer l
                                       ! (mm water/mm soil)
      integer    num_layers            ! number of layers in profile
      integer    numvals
      character  string*200            ! output string
      logical    found

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call write_string (new_line
     :                  //'   - Reading root profile parameters')


      !-----------------------------------------------------------
      !Read the root parameters from section Cultivar.Module.Parameters
      !-----------------------------------------------------------

       found = read_parameter (section_name
     :                     , 'uptake_source'
     :                     , p%uptake_source,.true.)
      if (.not.found) then
         p%uptake_source = 'calc'
      else
      endif

      found = read_parameter (section_name
     :                     , 'll'
     :                     , ll, num_layers
     :                     , 0.0, c%ll_ub)

      call fill_real_array (p%ll_dep, 0.0, max_layer)
      do layer = 1, num_layers
         p%ll_dep(layer) = ll(layer)*g%dlayer(layer)
      enddo

      found = read_parameter (section_name
     :                     , 'kl'
     :                     , p%kl, num_layers
     :                     , 0.0, c%kl_ub)

      found = read_parameter (section_name
     :                     , 'xf'
     :                     , p%xf, num_layers
     :                     , 0.0, 1.0)

      !-----------------------------------------------------------
      !Report the root parameters
      !-----------------------------------------------------------

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
      call write_string ( blank)

          ! report
      call write_string ( new_line//new_line)

      write (string,'(4x, a)') '                Root Profile'
      call write_string ( string)

      string = '    ------------------------------------------------'
      call write_string (string)

      string = '      Layer      Kl      Lower Exploration'
      call write_string ( string)
      string = '      Depth              limit   Factor'
      call write_string (string)

      string = '      (mm)       ()     (mm/mm)    ()'
      call write_string ( string)

      string = '    ------------------------------------------------'
      call write_string ( string)

      do  layer = 1, num_layers
         write (string,'(3x, 4f9.3)')
     :            g%dlayer(layer)
     :          , p%kl(layer)
     :          , ll(layer)
     :          , p%xf(layer)
         call write_string ( string)
      enddo

      string = '     ------------------------------------------------'
      call write_string ( string)

      call write_string ( new_line//new_line)


      call pop_routine (my_name)
      return
      end


*     ===============================================================
      subroutine Set_My_Variable (Variable_name)
*     ===============================================================
      use CropModModule
      use ComponentInterfaceModule
      implicit none

*+  Sub-Program Arguments
      character  Variable_name*(*)     ! (INPUT) Variable name to search for

*+  Purpose
*      Set a variable in this module as requested by another.

*+  Changes
*      290393 sc
*      220896 jngh  added call to message_unused
*                   changed respond2set to collect

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Set_My_Variable')

*+  Local Variables
      integer numvals

*- Implementation Section ----------------------------------

      call push_routine (my_name)



      if (variable_name .eq. 'co2_switch') then
c         call collect_integer_var (variable_name, '()'
c     :                             , c%co2switch, numvals
c     :                             , 0, 1)

      elseif (variable_name .eq. 'co2_level') then
c         call collect_real_var (variable_name, '()'
c     :                             , c%co2level, numvals
c     :                             , 0.0, 2000.0)

c previously commented         g%co2level = c%co2level


      elseif (variable_name .eq. 'vern_sens') then
c         call collect_real_var (variable_name, '()'
c     :                             , p%vern_sen, numvals
c     :                             , 0.0, 2000.0)
         p%vern_sen_internal   = p%vern_sen   * 0.0054545 + 0.0003


      else if (variable_name .eq. 'photop_sens') then
c        call collect_real_var (variable_name, '()'
c     :                             , p%photop_sen, numvals
c     :                             , 0.0, 2000.0)
         p%photop_sen_internal = p%photop_sen * 0.002



      else if (variable_name .eq. 'tt_germ_to_emerg') then
c         call collect_real_var (variable_name, '()'
c     :                             , p%tt_germ_to_emerg, numvals
c     :                             , 0.0, 2000.0)

      else if (variable_name .eq. 'tt_emerg_to_endjuv') then
c         call collect_real_var (variable_name, '()'
c     :                             , p%tt_emerg_to_endjuv, numvals
c     :                             , 0.0, 2000.0)

      else if (variable_name .eq. 'tt_endjuv_to_init') then
c         call collect_real_var (variable_name, '()'
c     :                             , p%tt_endjuv_to_init, numvals
c     :                             , 0.0, 2000.0)

      else if (variable_name .eq. 'tt_init_to_flag') then
c         call collect_real_var (variable_name, '()'
c     :                             , p%tt_init_to_flag, numvals
c     :                             , 0.0, 2000.0)

      else if (variable_name .eq. 'tt_flag_to_flower') then
c         call collect_real_var (variable_name, '()'
c     :                             , p%tt_flag_to_flower, numvals
c     :                             , 0.0, 2000.0)

      else if (variable_name .eq. 'tt_flower_to_start_grain') then
c         call collect_real_var (variable_name, '()'
c     :                             , p%tt_flower_to_start_grain, numvals
c     :                             , 0.0, 2000.0)

      else if (variable_name .eq. 'tt_start_to_end_grain') then
c         call collect_real_var (variable_name, '()'
c     :                             , p%tt_start_to_end_grain, numvals
c     :                             , 0.0, 2000.0)

      else if (variable_name .eq. 'tt_end_grain_to_maturity') then
c         call collect_real_var (variable_name, '()'
c     :                             , p%tt_end_grain_to_maturity, numvals
c     :                             , 0.0, 2000.0)

      else if (variable_name .eq. 'tt_maturity_to_ripe') then
c         call collect_real_var (variable_name, '()'
c     :                             , p%tt_maturity_to_ripe, numvals
c     :                             , 0.0, 2000.0)

      else if (variable_name .eq. 'tt_ripe_to_harvest') then
c         call collect_real_var (variable_name, '()'
c     :                             , p%tt_ripe_to_harvest, numvals
c     :                             , 0.0, 2000.0)

      else if (variable_name .eq. 'tt_startgf_to_mat') then
c         call collect_real_var (variable_name, '()'
c     :                             , p%startgf_to_mat, numvals
c     :                             , 0.0, 2000.0)

      else if ((variable_name .eq. 'leaf_app_rate').or.
     :         (variable_name .eq. 'phyllochron')  .or.
     :         (variable_name .eq. 'phint'))    then
c         call collect_real_var (variable_name, '()'
c     :                             ,c%leaf_app_rate, numvals
c     :                             , 0.0, 2000.0)

         c%leaf_app_rate1 = c%leaf_app_rate
         c%leaf_app_rate2 = c%leaf_app_rate

      else if (variable_name .eq. 'leaf_init_rate') then
c         call collect_real_var (variable_name, '()'
c     :                             ,c%leaf_init_rate, numvals
c     :                             , 0.0, 2000.0)

      else
         ! don't know this variable name
      endif

      call pop_routine (my_name)
      return
      end

*     ================================================================
      subroutine Set_Other_Variables ()
*     ================================================================
      use CropModModule
      use ComponentInterfaceModule
      implicit none

*+  Purpose
*      Set the value of a variable or array in other module/s.

*+  Notes
*      a flag is set if any of the totals is requested.  The totals are
*      reset during the next process phase when this happens.

*+  Changes
*     010994 sc   specified and programmed
*     220896 jngh changed set_ to post_ construct
*     081100 dph  changed post_ constructs to set_

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Set_Other_Variables')

*+  Local Variables
      real       dlt_NO3(max_layer)    ! soil NO3 change (kg/ha)
      real       dlt_NH4(max_layer)    ! soil NH4 change (kg/ha)
      integer    layer                 ! soil layer no.
      integer    num_layers            ! number of layers
      logical    ok

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call Update_Other_Variables()

c      if (p%uptake_source.eq.'calc') then
c+!!!! perhaps we should get number of layers at init and keep it
         num_layers = count_of_real_vals (g%dlayer, max_layer)



         do layer = 1, num_layers
            dlt_NO3(layer) = g%dlt_NO3gsm(layer) * gm2kg /sm2ha
            dlt_Nh4(layer) = g%dlt_Nh4gsm(layer) * gm2kg /sm2ha
         end do

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

         dlt_no3(:) = - MIN(g%NO3(:), - dlt_NO3(:))
         dlt_nh4(:) = - MIN(g%NH4(:), - dlt_NH4(:))


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

c        if (g%no3(1)+ dlt_NO3(1).le.0.0) then
c             PRINT *,'-----------get---000-----------'
c             PRINT *, 'NO3kgh     = ', g%NO3(1:2)
c             PRINT *, 'NO3gsm     = ', g%NO3gsm(1:2)
c
c             PRINT *,'-----------set-----------------'
c             PRINT *, 'dlt_NO3gsm = ', g%dlt_NO3gsm(1:2)
c             PRINT *, 'dlt_NO3kgh = ', dlt_NO3(1:2)
c             PRINT *, 'NO3kgh New = ', g%no3(1)+ dlt_NO3(1)
c             pause
c        end if

         ok = set_dlt_no3 (dlt_no3_id
     :                       , dlt_NO3, num_layers)

         ok = set_dlt_nh4 (dlt_nh4_id
     :                       , dlt_Nh4, num_layers)

c         ok = set_variable (dlt_sw_depID
c     :                       , g%dlt_sw_dep, num_layers)

c      else
c      endif

      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine Update_Other_Variables()
*     ===========================================================
      use CropModModule
      use ComponentInterfaceModule
      implicit none


*+  Purpose
*       Update other module states

*+  Changes
*      250894 jngh specified and programmed
*      191099 jngh changed to legume_Send_Crop_Chopped_Event
*      280801 ew   generalised

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Update_Other_Variables')


*+  Local Variables
      real       dm_residue(max_part)            ! dry matter added to residue (kg/ha)
      real       N_residue(max_part)             ! nitrogen added to residue (kg/ha)
      real       fraction_to_Residue(max_part)   ! fraction sent to residue (0-1)

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      ! dispose of detached material from senesced parts in
      ! live population

      fraction_to_Residue(:)    = 1.0
      fraction_to_Residue(root) = 0.0

      dm_residue(:) = g%dlt_dm_detached(:) * gm2kg/sm2ha
      N_residue (:) = g%dlt_N_detached (:) * gm2kg/sm2ha

      if (sum(dm_residue) .gt. 0.0) then
            call Send_Crop_Chopped_Event
     :                (c%crop_type
     :               , part_name
     :               , dm_residue
     :               , N_residue
     :               , fraction_to_Residue
     :               , max_part)
      else
      ! no surface residue
      endif

      ! now dispose of dead population detachments

      dm_residue(:) = g%dlt_dm_dead_detached(:) * gm2kg/sm2ha
      N_residue (:) = g%dlt_N_dead_detached (:) * gm2kg/sm2ha

      if (sum(dm_residue) .gt. 0.0) then
            call Send_Crop_Chopped_Event
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

      if (g%dlt_dm_detached(root).gt.0.0) then

         call crop_root_incorp (
     .          g%dlt_dm_detached(root)
     :         ,g%dlt_N_detached(root)
     :         ,g%dlayer
     :         ,g%root_length
     :         ,g%root_depth
     :         ,c%crop_type
     :         ,max_layer)

      end if

      if (g%dlt_dm_dead_detached(root).gt.0.0) then

         call crop_root_incorp (
     .          g%dlt_dm_dead_detached(root)
     :         ,g%dlt_N_dead_detached(root)
     :         ,g%dlayer
     :         ,g%root_length
     :         ,g%root_depth
     :         ,c%crop_type
     :         ,max_layer)


      end if


      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine Crop_Harvest (
     .          g_dm_green,
     .          g_dm_dead,
     .          c_grn_water_cont,
     .          g_grain_no,
     .          g_plants,
     .          g_dm_senesced,
     .          g_leaf_no,
     .          g_N_green,
     .          g_N_dead,
     .          g_N_senesced,
     .          g_flowering_date,
     .          g_maturity_date,
     .          g_flowering_das,
     .          g_maturity_das,
     .          g_lai_max,
     .          g_cswd_photo,
     .          g_days_tot,
     .          g_cswd_expansion,
     .          g_cnd_photo,
     .          g_cnd_grain_conc,
     .          c_stage_names)
*     ===========================================================
      use ComponentInterfaceModule
      implicit none
      include   'CropDefCons.inc'

*+  Sub-Program Arguments
       real g_dm_green(*)
       real g_dm_dead(*)
       real c_grn_water_cont
       real g_grain_no
       real g_plants
       real g_dm_senesced(*)
       real g_leaf_no(*)
       real g_N_green(*)
       real g_N_dead(*)
       real g_N_senesced(*)
       integer g_flowering_date
       integer g_maturity_date
       integer g_flowering_das
       integer g_maturity_das
       real g_lai_max
       real g_cswd_photo(*)
       real g_days_tot(*)
       real g_cswd_expansion(*)
       real g_cnd_photo(*)
       real g_cnd_grain_conc(*)
       character c_stage_names(*)*(*)

*+  Purpose
*       Report occurence of harvest and the current status of specific
*       variables.

*+  Changes
*     010994 sc   specified and programmed

*+  Calls
                                       ! lu_scr_sum

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Crop_Harvest')

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

      yield = (g_dm_green(grain) + g_dm_dead(grain))
     :      * gm2kg / sm2ha

          ! include the grain water content
      yield_wet = yield / (1.0 - c_grn_water_cont)

      grain_wt = divide (g_dm_green(grain) + g_dm_dead(grain)
     :                 , g_grain_no, 0.0)
!cpsc
      head_grain_no = divide (g_grain_no, g_plants, 0.0)

      biomass_green = (sum_real_array (g_dm_green, max_part)
     :              - g_dm_green(root) - g_dm_green(energy))
     :              * gm2kg / sm2ha

      biomass_senesced = (sum_real_array (g_dm_senesced, max_part)
     :                 - g_dm_senesced(root) - g_dm_senesced(energy))
     :                 * gm2kg / sm2ha

      biomass_dead = (sum_real_array (g_dm_dead, max_part)
     :             - g_dm_dead(root)  - g_dm_dead(energy))
     :             * gm2kg / sm2ha

      dm = (biomass_green + biomass_senesced + biomass_dead)

      stover = dm - yield

      leaf_no = sum_between (emerg, harvest_ripe, g_leaf_no)
      N_grain_conc_percent = divide (g_N_green(grain) + g_N_dead(grain)
     :                            , g_dm_green(grain) + g_dm_dead(grain)
     :                            , 0.0)
     :                     * fract2pcnt

      N_grain = (g_N_green(grain) + g_N_dead(grain))
     :        * gm2kg/sm2ha

      N_green = (sum_real_array (g_N_green, max_part)
     :        - g_N_green(root) - g_N_green(grain))
     :        * gm2kg / sm2ha

      N_senesced = (sum_real_array (g_N_senesced, max_part)
     :           - g_N_senesced(root) - g_N_senesced(grain))
     :           * gm2kg / sm2ha

      N_dead = (sum_real_array (g_N_dead, max_part)
     :       - g_N_dead(root) - g_N_dead(grain))
     :       * gm2kg / sm2ha

      N_stover = N_green + N_senesced + N_dead
      N_total = N_grain + N_stover


      call write_string ( new_line//new_line)

      write (string, '(a,i4,t40,a,i4)')
     :            'flowering (DAS) =', g_flowering_das
     :            ,'maturity (DAS)  =', g_maturity_das
      call write_string ( string)

      write (string, '(a,i4,t40,a,f10.1)')
     :            ' flowering day  = ',g_flowering_date
     :          , ' stover (kg/ha) =',stover
      call write_string ( string)

      write (string, '(a,i4,t40,a,f10.1)')
     :            ' maturity day        = ', g_maturity_date
     :          , ' grain yield (kg/ha) =', yield
      call write_string ( string)

      write (string, '(a,f6.1,t40,a,f10.1)')
     :            ' grain % water content   = ', c_grn_water_cont
     :                                         * fract2pcnt
     :          , ' grain yield wet (kg/ha) =', yield_wet
      call write_string ( string)

      write (string, '(a,f10.3,t40,a,f10.3)')
     :            ' grain wt (g) =', grain_wt
     :          , ' grains/m^2   =', g_grain_no
      call write_string ( string)

      write (string, '(a,f6.1,t40,a,f6.3)')
     :            ' grains/head =', head_grain_no
     :          , ' maximum lai =', g_lai_max
      call write_string ( string)

      write (string, '(a,f10.1)')
     :            ' total above ground biomass (kg/ha) =', dm
      call write_string ( string)

      write (string, '(a,f10.1)')
     :         ' live above ground biomass (kg/ha) =', biomass_green
     :                                               + biomass_senesced
      call write_string ( string)

      write (string, '(a,f10.1)')
     :            ' green above ground biomass (kg/ha) =', biomass_green
      call write_string ( string)

      write (string, '(a,f10.1)')
     :      ' senesced above ground biomass (kg/ha) =', biomass_senesced
      call write_string ( string)

      write (string, '(a,f10.1)')
     :            ' dead above ground biomass (kg/ha) =', biomass_dead
      call write_string ( string)

      write (string, '(a,f6.1)')
     :            ' number of leaves =', leaf_no
      call write_string ( string)

      write (string, '(a,f10.2,t40,a,f10.2)')
     :            ' grain N percent =', N_grain_conc_percent
     :          , ' total N content (kg/ha) =', N_total
      call write_string ( string)

      write (string, '(a,f10.2,t40,a,f10.2)')
     :            ' grain N uptake (kg/ha) =', N_grain
     :          , ' senesced N content (kg/ha) =', N_senesced

      call write_string ( string)

      write (string, '(a,f10.2,t40,a,f10.2)')
     :            ' green N content (kg/ha) =', N_green
     :          , ' dead N content (kg/ha) =', N_dead
      call write_string ( string)

      do 2000 phase = emerg_to_endjuv, start_to_end_grain
         si1 = divide (g_cswd_photo(phase)
     :               , g_days_tot(phase), 0.0)
         si2 = divide (g_cswd_expansion(phase)
     :               , g_days_tot(phase), 0.0)
         si4 = divide (g_cnd_photo(phase)
     :               , g_days_tot(phase), 0.0)
         si5 = divide (g_cnd_grain_conc(phase)
     :               , g_days_tot(phase), 0.0)

         call write_string ( new_line//new_line)

         write (string,'(2a)')
     :         ' stress indices for ', c_stage_names(phase)
         call write_string ( string)

         write (string,'(2(a, f16.7))')
     :         ' water stress 1 =', si1
     :         , '   nitrogen stress 1 =', si4
         call write_string ( string)

         write (string,'(2(a, f16.7))')
     :         ' water stress 2 =', si2
     :         , '   nitrogen stress 2 =', si5
         call write_string ( string)
2000  continue

      g_dm_green(grain) = 0.0
      g_N_green(grain) = 0.0

      g_dm_dead(grain) = 0.0
      g_N_dead(grain) = 0.0

      call pop_routine (my_name)
      return
      end


*     ===========================================================
      subroutine End_Crop ()
*     ===========================================================
      use CropModModule
      use ComponentInterfaceModule
      implicit none

*+  Purpose
*       End crop

*+  Changes
*      191099 jngh changed to millet_Send_Crop_Chopped_Event
*      280801 ew   generalised

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'End_Crop')

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


        !Report the crop yield
         yield = (g%dm_green(grain) + g%dm_dead(grain)) *gm2kg /sm2ha
         write (string, '(3x, a, f7.1)')
     :                  ' ended. Yield (dw) = ', yield
         call Write_string (string)


       !Root residue incorporation

         dm_root = g%dm_green(root)
     :           + g%dm_dead(root)
     :           + g%dm_senesced(root)

         N_root  = g%N_green(root)
     :           + g%N_dead(root)
     :           + g%N_senesced(root)


      if (dm_root.gt.0.0) then

         call crop_root_incorp (
     :          dm_root
     :         ,N_root
     :         ,g%dlayer
     :         ,g%root_length
     :         ,g%root_depth
     :         ,c%crop_type
     :         ,max_layer)


      endif


      !Top residue - put stover into surface residue
         dm_residue = (sum_real_array (g%dm_green, max_part)
     :              - g%dm_green(root))
c    :              - g%dm_green(root) - g%dm_green(grain))

     :              + (sum_real_array (g%dm_senesced, max_part)
     :              - g%dm_senesced(root))
C    :              - g%dm_senesced(root) - g%dm_senesced(grain))

     :              + (sum_real_array (g%dm_dead, max_part)
     :              - g%dm_dead(root))
c    :              - g%dm_dead(root) - g%dm_dead(grain))

         N_residue = (sum_real_array (g%N_green, max_part)
     :             - g%N_green(root) )
c    :             - g%N_green(root) - g%N_green(grain))

     :             + (sum_real_array (g%N_senesced, max_part)
     :             - g%N_senesced(root))
c    :             - g%N_senesced(root) - g%N_senesced(grain))

     :             + (sum_real_array (g%N_dead, max_part)
     :             - g%N_dead(root))
c    :             - g%N_dead(root) - g%N_dead(grain))

         dlt_dm_crop(:) = (g%dm_green(:)
     :                  + g%dm_senesced(:)
     :                  + g%dm_dead(:))
     :                  * gm2kg/sm2ha

         dlt_dm_N   (:) = (g%N_green(:)
     :                  + g%N_senesced(:)
     :                  + g%N_dead(:))
     :                  * gm2kg/sm2ha


         fraction_to_residue(:)    = 1.0
         fraction_to_Residue(root) = 0.0

         if (sum(dlt_dm_crop) .gt. 0.0) then

            call Send_Crop_Chopped_Event
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

         call write_string ( string)

      else
      endif

      call pop_routine (my_name)
      return
      end

* ====================================================================
      subroutine Send_Crop_Chopped_Event (crop_type
     :                                    , dm_type
     :                                    , dlt_crop_dm
     :                                    , dlt_crop_n
     :                                    , fraction_to_Residue
     :                                    , max_part)
* ====================================================================
      use ComponentInterfaceModule
      use DataTypesModule
      implicit none

*+  Sub-Program Arguments
      character  crop_type*(*)              ! (INPUT) crop type
      character  dm_type(*)*(*)             ! (INPUT) residue type
      real  dlt_crop_dm(*)                  ! (INPUT) residue weight (kg/ha)
      real  dlt_crop_n(*)                     ! (INPUT) residue N weight (kg/ha)
      real  fraction_to_Residue(*)          ! (INPUT) residue fraction to residue (0-1)
      integer max_part                      ! (INPUT) number of residue types
*+  Purpose
*     Notify other modules of crop chopped.

*+  Mission Statement
*     Notify other modules of crop chopped.

*+  Changes
*   070999 jngh - Programmed and Specified
*   110700 jngh - Changed dm_tyoe to array.

*+  Local Variables

      type(cropchoppedevent_type) :: EventData

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'Send_Crop_Chopped_Event')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      EventData%crop_type = crop_type
      EventData%num_dm_type = max_part
      EventData%dm_type(1:max_part) = dm_type(1:max_part)
      EventData%dlt_crop_dm(1:max_part) = dlt_crop_dm(1:max_part)
      EventData%dlt_crop_n(1:max_part) = dlt_crop_n(1:max_part)
      EventData%fraction_to_residue(1:max_part)
     .       = fraction_to_residue(1:max_part)

      call Publish_cropchoppedevent(crop_chopped_id,EventData,.false.)

      call pop_routine (myname)
      return
      end



*     ===========================================================
      subroutine Kill_Crop (
     .          g_plant_status,
     .          g_dm_green,
     .          g_dm_senesced,
     .          g_dm_dead)
*     ===========================================================
      use ComponentInterfaceModule
      implicit none
      include   'CropDefCons.inc'


*+  Sub-Program Arguments
       character g_plant_status*(*)
       real g_dm_green(*)
       real g_dm_senesced(*)
       real g_dm_dead(*)

*+  Purpose
*       Kill crop

*+  Changes
*       290994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'Kill_Crop')

*+  Local Variables
      real       biomass               ! above ground dm (kg/ha)
      character  string*200            ! output string

*- Implementation Section ----------------------------------

c+!!!!!! fix problem with deltas in update when change from alive to dead ?zero
      call push_routine (my_name)

      if (g_plant_status.eq.status_alive) then
         g_plant_status  = status_dead

         biomass = (sum_real_array (g_dm_green, max_part)
     :           - g_dm_green(root)) * gm2kg /sm2ha

     :           + (sum_real_array (g_dm_senesced, max_part)
     :           - g_dm_senesced(root)) * gm2kg /sm2ha

     :           + (sum_real_array (g_dm_dead, max_part)
     :           - g_dm_dead(root)) * gm2kg /sm2ha


                ! report

         write (string, '(3x, a, f7.1, a)')
     :                  ' kill. Standing above-ground dm = '
     :                  , biomass, ' (kg/ha)'
         call Write_string (string)

      else
      endif

      call pop_routine (my_name)
      return
      end
















*     ===========================================================
      subroutine Crop_Cleanup ()
*     ===========================================================
      use CropModModule
      use ComponentInterfaceModule
      implicit none

*+  Purpose
*       clean

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Crop_Cleanup')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call Crop_Update ()

      call Crop_Check_Bounds (
     .          g%leaf_no,
     .          g%leaf_no_dead,
     .          g%root_depth,
     .          g%dlayer,
     .          g%grain_no,
     .          p%head_grain_no_max,
     .          g%plants,
     .          g%current_stage,
     .          g%phase_tt,
     .          g%days_tot,
     .          g%tt_tot,
     .          g%canopy_height,
     .          c%height_max,
     .          g%lai,
     .          g%slai,
     .          g%tlai_dead,
     .          g%cover_green,
     .          g%cover_sen,
     .          g%cover_dead,
     .          g%leaf_area,
     .          g%heat_stress_tt,
     .          g%dm_stress_max,
     .          g%N_conc_crit,
     .          g%N_conc_min,
     .          g%N_conc_max,
     .          g%N_dead,
     .          g%N_green,
     .          g%N_senesced,
     .          g%dm_dead,
     .          g%dm_green,
     .          g%dm_senesced)

      call Crop_Totals (
     .          g%N_green,
     .          g%dm_green,
     .          g%dlt_N_retrans,
     .          g%N_conc_crit,
     .          g%N_demand,
     .          g%root_depth,
     .          g%dlayer,
     .          g%current_stage,
     .          g%days_tot,
     .          g%N_uptake_tot,
     .          g%transpiration_tot,
     .          g%dlt_sw_dep,
     .          g%N_conc_act_stover_tot,
     .          g%N_conc_crit_stover_tot,
     .          g%N_demand_tot,
     .          g%N_uptake_stover_tot,
     .          g%N_uptake_grain_tot,
     .          g%lai_max,
     .          g%lai,
     .          g%flowering_date,
     .          g%maturity_date,
     .          g%flowering_das,
     .          g%maturity_das,
     .          g%N_dead,
     .          g%N_senesced,
     .          g%day_of_year)

      call Crop_Event (
     .          g%current_stage,
     .          g%days_tot,
     .          c%stage_code_list,
     .          c%stage_names,
     .          g%dm_green,
     .          g%dm_senesced,
     .          g%dm_dead,
     .          g%N_green,
     .          g%root_depth,
     .          g%dlayer,
     .          g%sw_dep,
     .          p%ll_dep,
     .          g%lai)

      call pop_routine (my_name)

      return
      end


*     ===========================================================
      subroutine Crop_Update ()
*     ===========================================================
      use CropModModule
      use ComponentInterfaceModule
      implicit none

*+  Purpose
*       Update states
*

*+  Changes
*      250894 jngh specified and programmed

*+  Calls
!      include   'CropMod.inc'

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Crop_Update')

*+  Local Variables
      real       dlt_dm_plant          ! dry matter increase (g/plant)
      real       dlt_leaf_area         ! leaf area increase (mm^2/plant)
      real       dlt_dm_green_dead     ! dry matter of green plant part dying
                                       ! (g/m^2)
      real       dlt_dm_senesced_dead  ! dry matter of senesced plant part
                                       ! dying (g/m^2)
      real       dlt_N_green_dead      ! N content of green plant part dying
                                       ! (g/m^2)
      real       dlt_N_senesced_dead   ! N content of senesced plant part
                                       ! dying (g/m^2)
      real       dlt_grain_no_lost     ! grain no lost from barrenness
                                       ! (grains/m^2)
      real       dlt_lai_dead          ! lai of green leaf of plants dying ()
      real       dlt_slai_dead         ! lai of senesced leaf of plant dying ()
      real       dying_fract           ! fraction op population dying (0-1)
      real       leaf_no               ! currently expanding leaf no.
      integer    part                  ! plant part index

      REAL       co2_modifier

c      INTEGER    istage


*- Implementation Section ----------------------------------
      call push_routine (my_name)

         ! Note.
         ! Accumulate is used to add a value into a specified array element.
         ! If a specified increment of the element indicates a new element
         ! is started, the value is distributed proportionately between the
         ! two elements of the array

         ! Add is used to add the elements of one array into the corresponding
         ! elements of another array.

         ! now update with deltas

         ! The following table describes the transfer of material that should
         ! take place
         !                        POOLS
         !                 green senesced  dead
         ! dlt_green         +                     (incoming only)
         ! dlt_retrans       +-
         ! dlt_senesced      -      +
         ! dlt_dead          -      -       +
         ! dlt_detached             -       -      (outgoing only)

         ! transfer N

      call subtract_real_array (g%dlt_N_dead_detached, g%N_dead
     :                        , max_part)

      call add_real_array     (g%dlt_N_green,       g%N_green, max_part)
      call add_real_array     (g%dlt_N_retrans,     g%N_green, max_part)
      call subtract_real_array(g%dlt_N_senesced,    g%N_green, max_part)

      call add_real_array     (g%dlt_N_senesced,      g%N_senesced,
     :                                                max_part)
      call subtract_real_array(g%dlt_N_detached,      g%N_senesced,
     :                                                max_part)



      !&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      !EW added parts ?????????????????????????????????/
      call subtract_real_array (g%dlt_N_sen_retrans, g%N_senesced,
     :                                                max_part)
      !&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



      dying_fract = divide (-g%dlt_plants, g%plants, 0.0)
      dying_fract = bound (dying_fract, 0.0, 1.0)

      do 1000 part = 1, max_part
         dlt_N_green_dead = g%N_green(part) * dying_fract
         g%N_green(part) = g%N_green(part) - dlt_N_green_dead
         g%N_dead(part) = g%N_dead(part) + dlt_N_green_dead

         dlt_N_senesced_dead = g%N_senesced(part) * dying_fract
         g%N_senesced(part) = g%N_senesced(part) - dlt_N_senesced_dead
         g%N_dead(part) = g%N_dead(part) + dlt_N_senesced_dead
1000  continue


         ! Transfer plant dry matter

      dlt_dm_plant = divide (g%dlt_dm, g%plants, 0.0)

      call accumulate (dlt_dm_plant, g%dm_plant_top_tot
     :               , g%previous_stage, g%dlt_stage)

      call subtract_real_array (g%dlt_dm_dead_detached, g%dm_dead
     :                        , max_part)



      call add_real_array (g%dlt_dm_green, g%dm_green, max_part)
      call add_real_array (g%dlt_dm_green_retrans, g%dm_green, max_part)
      call subtract_real_array (g%dlt_dm_senesced, g%dm_green
     :                        , max_part)

      call add_real_array (g%dlt_dm_senesced, g%dm_senesced
     :                   , max_part)
      call subtract_real_array (g%dlt_dm_sen_retrans, g%dm_green
     :                        , max_part)

      call subtract_real_array (g%dlt_dm_detached, g%dm_senesced
     :                        , max_part)




      call add_real_array (g%dlt_dm_green_retrans_pool
     :                   , g%dm_green_retrans_pool
     :                   , max_part)

      g%dm_green_grainno = g%dm_green_grainno + g%dlt_dm_green_grainno

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c      do part = 1, max_part
c         g%dm_green(part)=g%dm_green(part)+g%dlt_dm_green(part)
c     :                                    +g%dlt_dm_green_retrans(part)
c     :                                    -g%dlt_dm_senesced(part)

c         g%dm_senesced(part)= g%dm_senesced(part)
c     .                       +g%dlt_dm_senesced(part)
c     .                       -g%dlt_dm_sen_retrans(part)
c      enddo

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc




      do 2000 part = 1, max_part
         dlt_dm_green_dead = g%dm_green(part) * dying_fract
         g%dm_green(part) = g%dm_green(part) - dlt_dm_green_dead
         g%dm_dead(part) = g%dm_dead(part) + dlt_dm_green_dead

         dlt_dm_senesced_dead = g%dm_senesced(part) * dying_fract
         g%dm_senesced(part) = g%dm_senesced(part)
     :                       - dlt_dm_senesced_dead
         g%dm_dead(part) = g%dm_dead(part) + dlt_dm_senesced_dead
2000  continue




cjh
         ! transfer plant grain no.
      dlt_grain_no_lost  = g%grain_no * dying_fract
      g%grain_no = g%grain_no - dlt_grain_no_lost
cglh
         ! update fertile no (pluses and minuses are the best I can do!)



      g%tiller_no_fertile = g%tiller_no_fertile + g%dlt_tiller_no
c     :                                         - g%dlt_stiller_no

      !&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      !EW added parts ?????????????????????????????????/
      g%tiller_no_pot    = g%tiller_no_pot + g%dlt_tiller_no_pot
      g%tiller_no        = g%tiller_no     + g%dlt_tiller_no
      g%tiller_no_sen    = g%tiller_no_sen + g%dlt_stiller_no
      !&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


      g%tt_tiller_emergence(1 + INT(g%tiller_no)) =
     :                           sum_between(emerg, flag_leaf, g%tt_tot)


      ! transfer plant leaf area
      g%lai = g%lai + g%dlt_lai - g%dlt_slai
      g%slai = g%slai + g%dlt_slai - g%dlt_slai_detached

      dlt_lai_dead  = g%lai  * dying_fract
      dlt_slai_dead = g%slai * dying_fract

      g%lai = g%lai - dlt_lai_dead
      g%slai = g%slai - dlt_slai_dead
      g%tlai_dead = g%tlai_dead + dlt_lai_dead + dlt_slai_dead
     :            - g%dlt_tlai_dead_detached


      g%tpla_yesterday = g%tpla_today

        ! now update new canopy covers for erosion etc?

c=======================================================================

      if ((c%crop_type .eq. 'wheat')    .OR.
     :    (c%crop_type .eq. 'sunflower'))  then

       call crop_cover (g%extinction_coeff, g%lai,       g%cover_green)
       call crop_cover (g%extinction_coeff, g%slai,      g%cover_sen)
       call crop_cover (g%extinction_coeff, g%tlai_dead, g%cover_dead)


      elseif (c%crop_type .eq. 'maize') then

      call Maize_cover1 (
     .          g%row_spacing,
     .          c%x_row_spacing,
     .          c%y_extinct_coef,
     .          c%num_row_spacing,
     .          g%cover_green,g%lai)
      call Maize_cover1 (
     .          g%row_spacing,
     .          c%x_row_spacing,
     .          c%y_extinct_coef_dead,
     .          c%num_row_spacing,
     .          g%cover_sen,g%slai)
      call Maize_cover1 (
     .          g%row_spacing,
     .          c%x_row_spacing,
     .          c%y_extinct_coef_dead,
     .          c%num_row_spacing,
     .          g%cover_dead,
     :          g%tlai_dead)

      elseif (c%crop_type .eq. 'sorghum') then

        call crop_cover_sorghum
     :               (g%row_spacing
     :                ,c%x_row_spacing,c%y_extinct_coef
     :                ,c%num_row_spacing
     :                ,g%lai,g%cover_green)
        call crop_cover_sorghum
     :               (g%row_spacing
     :                ,c%x_row_spacing,c%y_extinct_coef_dead
     :                ,c%num_row_spacing
     :                ,g%slai,g%cover_sen)
        call crop_cover_sorghum
     :               (g%row_spacing
     :                ,c%x_row_spacing,c%y_extinct_coef_dead
     :                ,c%num_row_spacing
     :                ,g%tlai_dead,g%cover_dead)

      else

       call crop_cover (g%extinction_coeff, g%lai,       g%cover_green)
       call crop_cover (g%extinction_coeff, g%slai,      g%cover_sen)
       call crop_cover (g%extinction_coeff, g%tlai_dead, g%cover_dead)

      endif
c=======================================================================

         ! plant leaf development
         ! need to account for truncation of partially developed leaf (add 1)
      leaf_no = 1.0 + sum_between (emerg, now, g%leaf_no)
      dlt_leaf_area = divide (g%dlt_lai, g%plants, 0.0) * sm2smm

      call accumulate (dlt_leaf_area, g%leaf_area
     :               , leaf_no, g%dlt_leaf_no)

      call accumulate (g%dlt_leaf_no, g%leaf_no
     :               , g%previous_stage, g%dlt_stage)

      call accumulate (g%dlt_leaf_no_dead, g%leaf_no_dead
     :               , g%previous_stage, g%dlt_stage)

         ! plant stress

      call accumulate (g%dlt_heat_stress_tt, g%heat_stress_tt
     :               , g%previous_stage, g%dlt_stage)

      call accumulate (g%dlt_dm_stress_max, g%dm_stress_max
     :               , g%current_stage, g%dlt_stage)

      call accumulate (1.0 - g%swdef_photo
     .               , g%cswd_photo
     :               , g%previous_stage, g%dlt_stage)
      call accumulate (1.0 - g%swdef_expansion
     .               , g%cswd_expansion
     :               , g%previous_stage, g%dlt_stage)
      call accumulate (1.0 - g%swdef_pheno
     .               , g%cswd_pheno
     :               , g%previous_stage, g%dlt_stage)

      call accumulate (1.0 - g%nfact_photo
     .               , g%cnd_photo
     :               , g%previous_stage, g%dlt_stage)
      call accumulate (1.0 - g%nfact_grain_conc
     .               , g%cnd_grain_conc
     :               , g%previous_stage, g%dlt_stage)

         ! other plant states

      g%canopy_height = g%canopy_height + g%dlt_canopy_height
      g%plants = g%plants + g%dlt_plants
      g%root_depth = g%root_depth + g%dlt_root_depth

      call add_real_array      (g%dlt_root_length
     :                         ,g%root_length
     :                         ,max_layer)
      call subtract_real_array (g%dlt_root_length_senesced
     :                         ,g%root_length
     :                         ,max_layer)
      ! Phosphorus
      ! ----------
      g%plant_p = g%plant_p + g%dlt_plant_p



      !&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      !EW added parts ?????????????????????????????????/
      ! ew added this section for iwheat update
       g%tiller_tt_tot = g%tiller_tt_tot + g%dlt_tt
c     :  * MIN(g%nfact_tiller, g%swdef_tiller)
c     : *MIN(g%nfact_expansion,g%swdef_expansion)

          if (g%current_stage.lt.germ) then
c         if (istage.lt.emerg) then
                g%tiller_tt_tot = 0.0   ! in original i_wheat tt accumulated from germination - ew
          endif



       call add_real_array (g%dlt_tiller_area_pot,
     :                      g%tiller_area_pot, max_leaf)


       call add_real_array (g%dlt_tiller_area_act,
     :                      g%tiller_area_act, max_leaf)

       call subtract_real_array (g%dlt_tiller_sen_area,
     :                      g%tiller_area_act, max_leaf)

       call add_real_array (g%dlt_tiller_sen_area,
     :                      g%tiller_area_sen, max_leaf)



       g%cumvd         = g%cumvd         + g%dlt_cumvd
       g%vernalisation = g%vernalisation + g%dlt_vernalisation
       g%leaf_primodia = g%leaf_primodia + g%dlt_leaf_primodia
      !&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&




      call Crop_N_Conc_Limits (
     .          g%current_stage,
     .          c%N_conc_crit_grain,
     .          c%N_conc_max_grain,
     .          c%N_conc_min_grain,
     .          c%x_stage_code,
     .          c%stage_code_list,
     .          g%tt_tot,
     .          g%phase_tt,
     .          c%y_N_conc_crit_stem,
     .          c%y_N_conc_crit_leaf,
     .          c%y_N_conc_crit_flower,
     .          c%y_N_conc_min_stem,
     .          c%y_N_conc_min_leaf,
     .          c%y_N_conc_min_flower,
     .          c%y_N_conc_max_stem,
     .          c%y_N_conc_max_leaf,
     .          c%y_N_conc_max_flower,

     .          c%y_N_conc_crit_root,
     .          c%y_N_conc_min_root,
     .          c%y_N_conc_max_root,

     .          g%N_conc_crit,
     .          g%N_conc_max,
     .          g%N_conc_min)  ! plant N concentr




      if (c%co2switch .ne. 0) then


         co2_modifier = linear_interp_real(g%co2level,
     :                                  c%co2_level_nconc,
     :                                  c%nconc_co2_modifier,
     :                                  c%num_co2_level_nconc)


c        PRINT *,'nconc_co2modifier '
c        PRINT *,'c%co2_level_nconc    =', c%co2_level_nconc
c        PRINT *,'c%nconc_co2_modifier =', c%nconc_co2_modifier
c        PRINT *,'nconc_co2modifier    =', co2_modifier


         g%N_conc_crit(leaf)= g%N_conc_crit(leaf)*co2_modifier

c         do part = 1, max_part
c            g%N_conc_crit(part)= g%N_conc_crit(part)*co2_modifier
c         enddo

      end if


      call pop_routine (my_name)
      return
      end




*     ===========================================================
      subroutine Crop_Check_Bounds (
     .          g_leaf_no,
     .          g_leaf_no_dead,
     .          g_root_depth,
     .          g_dlayer,
     .          g_grain_no,
     .          p_head_grain_no_max,
     .          g_plants,
     .          g_current_stage,
     .          g_phase_tt,
     .          g_days_tot,
     .          g_tt_tot,
     .          g_canopy_height,
     .          c_height_max,
     .          g_lai,
     .          g_slai,
     .          g_tlai_dead,
     .          g_cover_green,
     .          g_cover_sen,
     .          g_cover_dead,
     .          g_leaf_area,
     .          g_heat_stress_tt,
     .          g_dm_stress_max,
     .          g_N_conc_crit,
     .          g_N_conc_min,
     .          g_N_conc_max,
     .          g_N_dead,
     .          g_N_green,
     .          g_N_senesced,
     .          g_dm_dead,
     .          g_dm_green,
     .          g_dm_senesced)
*     ===========================================================
      use ComponentInterfaceModule
      implicit none
      include   'CropDefCons.inc'


*+  Sub-Program Arguments
       real g_leaf_no(*)
       real g_leaf_no_dead(*)
       real g_root_depth
       real g_dlayer(*)
       real g_grain_no
       real p_head_grain_no_max
       real g_plants
       real g_current_stage
       real g_phase_tt(*)
       real g_days_tot(*)
       real g_tt_tot(*)
       real g_canopy_height
       real c_height_max
       real g_lai
       real g_slai
       real g_tlai_dead
       real g_cover_green
       real g_cover_sen
       real g_cover_dead
       real g_leaf_area(*)
       real g_heat_stress_tt(*)
       real g_dm_stress_max(*)
       real g_N_conc_crit(*)
       real g_N_conc_min(*)
       real g_N_conc_max(*)
       real g_N_dead(*)
       real g_N_green(*)
       real g_N_senesced(*)
       real g_dm_dead(*)
       real g_dm_green(*)
       real g_dm_senesced(*)

*+  Purpose
*         Check bounds of internal pools
*

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Crop_Check_Bounds')

*+  Local Variables
                                       ! top (g/m^2)

*- Implementation Section ----------------------------------


      call push_routine (my_name)

      call bound_check_single
     :           (sum_real_array (g_leaf_no, max_stage)
     :          , 0.0
     :          , real (max_leaf)
     :          , 'leaf_no')

      call bound_check_single
     :           (sum_real_array (g_leaf_no_dead, max_stage)
     :          , 0.0
     :          , real (max_leaf)
     :          , 'leaf_no_dead')

      call bound_check_single
     :           (g_root_depth
     :          , 0.0
     :          , sum_real_array (g_dlayer, max_layer)
     :          , 'root_depth')

      call bound_check_single
     :           (g_grain_no
     :          , 0.0
     :          , 200000.0 !c_head_grain_no_max_ub   !p_head_grain_no_max * g_plants  -ew
     :          , 'grain_no')

      call bound_check_single
     :           (g_current_stage
     :          , 0.0
     :          , real (max_stage)
     :          , 'current_stage')

      call bound_check_single
     :           (sum_real_array (g_phase_tt, max_stage)
     :          , 0.0
     :          , 1000000.0
     :          , 'phase_tt')

      call bound_check_single
     :           (sum_real_array (g_days_tot, max_stage)
     :          , 0.0
     :          , 40000.0
     :          , 'days_tot')

      call bound_check_single
     :           (sum_real_array (g_tt_tot, max_stage)
     :          , 0.0
     :          , 40000.0
     :          , 'tt_tot')

      call bound_check_single
     :           (g_plants
     :          , 0.0
     :          , 10000.0
     :          , 'plants')

      call bound_check_single
     :           (g_canopy_height
     :          , 0.0
     :          , c_height_max
     :          , 'canopy_height')



      call bound_check_single
     :           (g_lai
     :          , 0.0
     :          , 30.0 - g_slai - g_tlai_dead
     :          , 'lai')

      call bound_check_single
     :           (g_slai
     :          , 0.0
     :          , 30.0 - g_lai - g_tlai_dead
     :          , 'slai')

      call bound_check_single
     :           (g_tlai_dead
     :          , 0.0
     :          , 30.0 - g_slai - g_lai
     :          , 'tlai_dead')

      call bound_check_single
     :           (g_cover_green
     :          , 0.0
     :          , 1.0
     :          , 'cover_green')

      call bound_check_single
     :           (g_cover_sen
     :          , 0.0
     :          , 1.0
     :          , 'cover_sen')

      call bound_check_single
     :           (g_cover_dead
     :          , 0.0
     :          , 1.0
     :          , 'cover_dead')

      call bound_check_single
     :           (sum_real_array (g_leaf_area, max_leaf)
     :          , 0.0
     :          , 10000000.0
     :          , 'leaf_area')

      call bound_check_single
     :           (sum_real_array (g_heat_stress_tt, max_stage)
     :          , 0.0
     :          , 1000000.0
     :          , 'heat_stress_tt')
      call bound_check_single
     :           (sum_real_array (g_dm_stress_max, max_stage)
     :          , 0.0
     :          , 1000000.0
     :          , 'dm_stress_max')

      call bound_check_single
     :           (sum_real_array (g_N_conc_crit, max_part)
     :          , sum_real_array (g_N_conc_min, max_part)
     :          , sum_real_array (g_N_conc_max, max_part)
     :          , 'N_conc_crit')

      call bound_check_single
     :           (sum_real_array (g_N_conc_max, max_part)
     :          , sum_real_array (g_N_conc_crit, max_part)
     :          , 1.0
     :          , 'N_conc_max')

      call bound_check_single
     :           (sum_real_array (g_N_conc_min, max_part)
     :          , 0.0
     :          , sum_real_array (g_N_conc_crit, max_part)
     :          , 'N_conc_min')

      call bound_check_single
     :           (sum_real_array (g_N_dead, max_part)
     :          , 0.0
     :          , 10000.0 - sum_real_array (g_N_green, max_part)
     :                    - sum_real_array (g_N_senesced, max_part)
     :          , 'N_dead')

      call bound_check_single
     :           (sum_real_array (g_N_green, max_part)
     :          , 0.0
     :          , 10000.0 - sum_real_array (g_N_dead, max_part)
     :                    - sum_real_array (g_N_senesced, max_part)
     :          , 'N_green')

      call bound_check_single
     :           (sum_real_array (g_N_senesced, max_part)
     :          , 0.0
     :          , 10000.0 - sum_real_array (g_N_green, max_part)
     :                    - sum_real_array (g_N_dead, max_part)
     :          , 'N_senesced')

      call bound_check_single
     :           (sum_real_array (g_dm_dead, max_part)
     :          , 0.0
     :          , 10000.0 - sum_real_array (g_dm_green, max_part)
     :                    - sum_real_array (g_dm_senesced, max_part)
     :          , 'dm_dead')

      call bound_check_single
     :           (sum_real_array (g_dm_green, max_part)
     :          , 0.0
     :          , 10000.0 - sum_real_array (g_dm_dead, max_part)
     :                    - sum_real_array (g_dm_senesced, max_part)
     :          , 'dm_green')

      call bound_check_single
     :           (sum_real_array (g_dm_senesced, max_part)
     :          , 0.0
     :          , 10000.0 - sum_real_array (g_dm_green, max_part)
     :                    - sum_real_array (g_dm_dead, max_part)
     :          , 'dm_senesced')

      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine Crop_Totals (
     .          g_N_green,
     .          g_dm_green,
     .          g_dlt_N_retrans,
     .          g_N_conc_crit,
     .          g_N_demand,
     .          g_root_depth,
     .          g_dlayer,
     .          g_current_stage,
     .          g_days_tot,
     .          g_N_uptake_tot,
     .          g_transpiration_tot,
     .          g_dlt_sw_dep,
     .          g_N_conc_act_stover_tot,
     .          g_N_conc_crit_stover_tot,
     .          g_N_demand_tot,
     .          g_N_uptake_stover_tot,
     .          g_N_uptake_grain_tot,
     .          g_lai_max,
     .          g_lai,
     .          g_flowering_date,
     .          g_maturity_date,
     .          g_flowering_das,
     .          g_maturity_das,
     .          g_N_dead,
     .          g_N_senesced,
     .          g_day_of_year)
*     ===========================================================
      use ComponentInterfaceModule
      implicit none
      include   'CropDefCons.inc'


*+  Sub-Program Arguments
       real g_N_green(*)
       real g_dm_green(*)
       real g_dlt_N_retrans(*)
       real g_N_conc_crit(*)
       real g_N_demand(*)
       real g_root_depth
       real g_dlayer(*)
       real g_current_stage
       real g_days_tot(*)
       real g_N_uptake_tot
       real g_transpiration_tot
       real g_dlt_sw_dep(*)
       real g_N_conc_act_stover_tot
       real g_N_conc_crit_stover_tot
       real g_N_demand_tot
       real g_N_uptake_stover_tot
       real g_N_uptake_grain_tot
       real g_lai_max
       real g_lai
       integer g_flowering_date
       integer g_maturity_date
       integer g_flowering_das
       integer g_maturity_das
       real g_N_dead(*)
       real g_N_senesced(*)
       integer g_day_of_year

*+  Purpose
*         Collect totals of crop variables for output
*
*   Called by Crop_Cleanup in whtmain.for

*+  Changes
*     010994 jngh specified and programmed

*+  Calls
cpsc  add below
cjh      include   'convert.inc'          ! gm2kg, sm2ha, sm2smm

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Crop_Totals')

*+  Local Variables
      real       N_conc_stover         ! tops actual N concentration
                                       ! (g N/g part)
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      real       N_conc_stover_crit    ! tops critical N concentration
                                       ! (g N/g part)
      real       N_green_demand        ! plant N demand (g/m^2)
      real       N_uptake              ! nitrogen uptake from soil (g/m^2)
      real       N_uptake_stover       ! nitrogen uptake from soil by veg.
                                       ! top (g/m^2)
cpsc add below
      real       N_grain               ! total grain N uptake
      real       N_dead                ! above ground dead plant N
      real       N_green               ! above ground green plant N
      real       N_senesced            ! above ground senesced plant N
      real       N_stover              ! nitrogen content of stover

*- Implementation Section ----------------------------------


      call push_routine (my_name)

             ! get totals
      N_conc_stover = divide ((g_N_green(leaf)
     :                       + g_N_green(stem)
     :                       + g_N_green(flower))

     :                      , (g_dm_green(leaf)
     :                       + g_dm_green(stem)
     :                       + g_dm_green(flower))
     :                       , 0.0)

      N_uptake = sum_real_array (g_dlt_N_retrans, max_part)
      N_uptake_stover =  g_dlt_N_retrans(leaf) + g_dlt_N_retrans(stem)

          ! note - g_N_conc_crit should be done before the stages change

      N_conc_stover_crit = (g_N_conc_crit(leaf) + g_N_conc_crit(stem))
     :                   * 0.5
      N_green_demand = sum_real_array (g_N_demand, max_part)

      deepest_layer = find_layer_no (g_root_depth, g_dlayer, max_layer)

      if (on_day_of (sowing, g_current_stage, g_days_tot)) then
         g_N_uptake_tot = N_uptake
         g_transpiration_tot =
     :           - sum_real_array (g_dlt_sw_dep, deepest_layer)
         g_N_conc_act_stover_tot = N_conc_stover
         g_N_conc_crit_stover_tot = N_conc_stover_crit
         g_N_demand_tot = N_green_demand
         g_N_uptake_stover_tot = N_uptake_stover
         g_N_uptake_grain_tot = sum_real_array (g_dlt_N_retrans
     :                                        , max_part)

      else
         g_N_uptake_tot = g_N_uptake_tot + N_uptake
         g_transpiration_tot = g_transpiration_tot
     :                       + (-sum_real_array (g_dlt_sw_dep
     :                                         , deepest_layer))
         g_N_conc_act_stover_tot = N_conc_stover
         g_N_conc_crit_stover_tot = N_conc_stover_crit
         g_N_demand_tot = g_N_demand_tot + N_green_demand
         g_N_uptake_stover_tot = g_N_uptake_stover_tot
     :                         + N_uptake_stover
         g_N_uptake_grain_tot = g_N_uptake_grain_tot
     :                        + sum_real_array (g_dlt_N_retrans
     :                                        , max_part)

      endif

      g_lai_max = max (g_lai_max, g_lai)

      if (on_day_of (flowering, g_current_stage, g_days_tot)) then
         g_flowering_date = g_day_of_year
         g_flowering_das  = sum_between (sowing, now, g_days_tot)
      else if (on_day_of (maturity, g_current_stage, g_days_tot)) then
         g_maturity_date = g_day_of_year
         g_maturity_das  = sum_between (sowing, now, g_days_tot)
      else
      endif

cpsc add below 07/04/95

      N_grain = (g_N_green(grain) + g_N_dead(grain))

      N_green = (sum_real_array (g_N_green, max_part)
     :        - g_N_green(root) - g_N_green(grain))

      N_senesced = (sum_real_array (g_N_senesced, max_part)
     :           - g_N_senesced(root) - g_N_senesced(grain))

      N_dead = (sum_real_array (g_N_dead, max_part)
     :       - g_N_dead(root) - g_N_dead(grain))

      N_stover = N_green + N_senesced + N_dead

      g_N_uptake_grain_tot = N_grain
      g_N_uptake_stover_tot = N_stover
      g_N_uptake_tot = N_grain + N_stover

cpsc  add above


      call pop_routine (my_name)
      return
      end




*     ===========================================================
      subroutine Crop_Event (
     .          g_current_stage,
     .          g_days_tot,
     .          c_stage_code_list,
     .          c_stage_names,
     .          g_dm_green,
     .          g_dm_senesced,
     .          g_dm_dead,
     .          g_N_green,
     .          g_root_depth,
     .          g_dlayer,
     .          g_sw_dep,
     .          p_ll_dep,
     .          g_lai)

*     ===========================================================
      use ComponentInterfaceModule
      implicit none
      include   'CropDefCons.inc'


*+  Sub-Program Arguments
       real g_current_stage
       real g_days_tot(*)
       real c_stage_code_list(*)
       character c_stage_names(*)*(*)
       real g_dm_green(*)
       real g_dm_senesced(*)
       real g_dm_dead(*)
       real g_N_green(*)
       real g_root_depth
       real g_dlayer(*)
       real g_sw_dep(*)
       real p_ll_dep(*)
       real g_lai

*+  Purpose
*       Report occurence of event and the current status of specific
*       variables.
*
*   Called by Crop_cleanup in whtmain.for

*+  Changes
*     010994 jngh specified and programmed

*+  Calls
                                       ! lu_scr_sum

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Crop_Event')

*+  Local Variables
      real       biomass               ! total above ground plant wt (g/m^2)
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      integer    layer                 ! profile layer number
      real       pesw_tot              ! total plant extractable sw (mm)
      real       pesw(max_layer)       ! plant extractable soil water (mm)
      real       N_green               ! plant nitrogen of tops (g/m^2)
                                       ! less flower
      real       dm_green              ! plant wt of tops (g/m^2) less flower
      integer    stage_no              ! stage number at beginning of phase
      character  string*200            ! message
      real       N_green_conc_percent  ! n% of tops less flower (incl grain)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      stage_no = INT(g_current_stage)

      if (on_day_of (stage_no, g_current_stage, g_days_tot)
     .    .AND. (stage_no.ne.0)) then
             ! new phase has begun.


         write (string, '(a, f6.1, 1x, a)')
     :                   ' stage '
     :                  , c_stage_code_list(stage_no)
     :                  , c_stage_names(stage_no)
         call Write_string (string)


         biomass = sum_real_array (g_dm_green, max_part)
     :           - g_dm_green(root) - g_dm_green(energy)

     :           + sum_real_array (g_dm_senesced, max_part)
     :           - g_dm_senesced(root)- g_dm_senesced(energy)

     :           + sum_real_array (g_dm_dead, max_part)
     :           - g_dm_dead(root) - g_dm_dead(energy)

         dm_green = sum_real_array (g_dm_green, max_part)
     :            - g_dm_green(root) - g_dm_green(energy)

         N_green = sum_real_array (g_N_green, max_part)
     :           - g_N_green(root) - g_N_green(energy)

         N_green_conc_percent = divide (N_green, dm_green, 0.0)
     :                        * fract2pcnt

         deepest_layer = find_layer_no (g_root_depth, g_dlayer
     :                                , max_layer)
         do 1000 layer = 1, deepest_layer
            pesw(layer) = g_sw_dep(layer) - p_ll_dep(layer)
            pesw(layer) = l_bound (pesw(layer), 0.0)
1000     continue
         pesw_tot = sum_real_array (pesw, deepest_layer)

         if (stage_is_between (emerg, plant_end, g_current_stage)) then
            write (string, '(2(a, g16.7e2), a, 2(a, g16.7e2))')
     :              '                     biomass =       '
     :            , biomass
     :            , '   lai = '
     :            , g_lai
     :            , new_line
     :            ,'                     stover N conc ='
     :            , N_green_conc_percent
     :            , '   extractable sw ='
     :            , pesw_tot
            call write_string ( string)


         else
         endif

      else
      endif


      call pop_routine (my_name)
      return
      end


*     ===========================================================
      subroutine Crop_N_Conc_Limits (
     .          g_current_stage,
     .          c_N_conc_crit_grain,
     .          c_N_conc_max_grain,
     .          c_N_conc_min_grain,
     .          c_x_stage_code,
     .          c_stage_code_list,
     .          g_tt_tot,
     .          g_phase_tt,
     .          c_y_N_conc_crit_stem,
     .          c_y_N_conc_crit_leaf,
     .          c_y_N_conc_crit_flower,
     .          c_y_N_conc_min_stem,
     .          c_y_N_conc_min_leaf,
     .          c_y_N_conc_min_flower,
     .          c_y_N_conc_max_stem,
     .          c_y_N_conc_max_leaf,
     .          c_y_N_conc_max_flower,

     .          c_y_N_conc_crit_root,
     .          c_y_N_conc_min_root,
     .          c_y_N_conc_max_root,


     .          N_conc_crit,
     .          N_conc_max,
     .          N_conc_min)
*     ===========================================================
      use ComponentInterfaceModule
      implicit none
      include   'CropDefCons.inc'


*+  Sub-Program Arguments
       real g_current_stage
       real c_N_conc_crit_grain
       real c_N_conc_max_grain
       real c_N_conc_min_grain
       real c_x_stage_code(*)
       real c_stage_code_list(*)
       real g_tt_tot(*)
       real g_phase_tt(*)
       real c_y_n_conc_crit_stem(*)
       real c_y_n_conc_crit_leaf(*)
       real c_y_n_conc_crit_flower(*)
       real c_y_n_conc_min_stem(*)
       real c_y_n_conc_min_leaf(*)
       real c_y_n_conc_min_flower(*)
       real c_y_n_conc_max_stem(*)
       real c_y_n_conc_max_leaf(*)
       real c_y_n_conc_max_flower(*)

       real c_y_N_conc_crit_root(*)
       real c_y_N_conc_min_root(*)
       real c_y_N_conc_max_root(*)


      real       N_conc_crit(*)        ! (OUTPUT) critical N concentration
                                       ! (g N/g part)
      real       N_conc_max(*)         ! (OUTPUT) maximum N concentration
                                       ! (g N/g part)
      real       N_conc_min(*)         ! (OUTPUT) minimum N concentration
                                       ! (g N/g part)

*+  Purpose
*       Calculate the critical N concentration below which plant growth
*       is affected.  Also minimum and maximum N concentrations below
*       and above which it is not allowed to fall or rise.
*       These are analogous to the water concentrations
*       of sat, dul and ll.

*+  Changes
*     080994 jngh specified and programmed

*+  Calls
*      real       Phenology_Stage_Code      ! function
       real       Crop_stage_code           ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Crop_N_Conc_Limits')

*+  Local Variables
      integer    numvals               ! number of values in stage code table
      real       current_stage_code            ! interpolated current stage code

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call fill_real_array (N_conc_crit, 0.0, max_part)
      call fill_real_array (N_conc_min, 0.0, max_part)

      if (stage_is_between (emerg, maturity, g_current_stage)) then
         N_conc_crit(grain) = c_N_conc_crit_grain
         N_conc_max(grain) = c_N_conc_max_grain
         N_conc_min(grain) = c_N_conc_min_grain


             ! the tops critical N percentage concentration is the stover
             ! (non-grain shoot) concentration below which N concentration
             ! begins to affect plant growth.

         numvals = count_of_real_vals (c_x_stage_code, max_stage)
         current_stage_code = Crop_stage_code (
     .          c_stage_code_list,
     .          g_tt_tot,
     .          g_phase_tt,
     .          g_current_stage,
     .          c_x_stage_code,
     .          numvals,
     .          max_stage)


         N_conc_crit(stem) = linear_interp_real (current_stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_crit_stem
     :                             , numvals)
         N_conc_crit(leaf) = linear_interp_real (current_stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_crit_leaf
     :                             , numvals)
         N_conc_crit(flower) = linear_interp_real (current_stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_crit_flower
     :                             , numvals)

         N_conc_crit(root) = linear_interp_real (current_stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_crit_root
     :                             , numvals)


             ! the  minimum N concentration is the N concentration
             ! below which N does not fall.

         N_conc_min(stem) = linear_interp_real (current_stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_min_stem
     :                             , numvals)

         N_conc_min(leaf) = linear_interp_real (current_stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_min_leaf
     :                             , numvals)

         N_conc_min(flower) = linear_interp_real (current_stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_min_flower
     :                             , numvals)

         N_conc_min(root) = linear_interp_real (current_stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_min_root
     :                             , numvals)

             ! the  maximum N concentration is the N concentration
             ! above which N does not rise.

         N_conc_max(stem) = linear_interp_real (current_stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_max_stem
     :                             , numvals)

         N_conc_max(leaf) = linear_interp_real (current_stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_max_leaf
     :                             , numvals)

         N_conc_max(flower) = linear_interp_real (current_stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_max_flower
     :                             , numvals)

         N_conc_max(root) = linear_interp_real (current_stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_max_root
     :                             , numvals)

      else
      endif

      call pop_routine (my_name)
      return
      end





*     ===========================================================
      subroutine Crop_Detachment(option)
*     ===========================================================
      use CropModModule
      use ComponentInterfaceModule
      implicit none

*+  Sub-Program Arguments
      integer option

*+  Purpose
*       Simulate plant detachment.
cscc Detachment is also a function of the environment. We've
c noticed large diff. in detachment between wet and dry environments
c in maize

*+  Changes
*      091294 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Crop_Detachment')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call cproc_dm_detachment1( max_part
     :                              , c%sen_detach_frac
     :                              , g%dm_senesced
     :                              , g%dlt_dm_detached
     :                              , c%dead_detach_frac
     :                              , g%dm_dead
     :                              , g%dlt_dm_dead_detached)

         call cproc_n_detachment1( max_part
     :                              , c%sen_detach_frac
     :                              , g%n_senesced
     :                              , g%dlt_n_detached
     :                              , c%dead_detach_frac
     :                              , g%n_dead
     :                              , g%dlt_n_dead_detached)

         call cproc_lai_detachment1 (leaf
     :                             , c%sen_detach_frac
     :                             , g%slai
     :                             , g%dlt_slai_detached
     :                             , c%dead_detach_frac
     :                             , g%tlai_dead
     :                             , g%dlt_tlai_dead_detached)

      else
         call error ('Invalid template option',.true.)
      endif

      call pop_routine (my_name)
      return
      end















*     ===========================================================
      INTEGER FUNCTION GetSwitchDigit(
     .                               switch, ! the switch code integer
     .                               pos     ! position of the digital
     .                               )
*     ===========================================================
      use ComponentInterfaceModule
      implicit none

*+  Sub-Program Arguments
      INTEGER switch
      INTEGER pos

*+  Purpose
*     Get the digital code at position pos in the switch

*+  Changes
*     990405 ew - programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'GetSwitchDigit')

*+  Local variables
      REAL     X1
      REAL     X2
      REAL     X
      INTEGER  Y1
      INTEGER  Y2
      INTEGER  N
      INTEGER  code

*- Implementation Section ----------------------------------

      call push_routine (my_name)

          if (switch.gt.0) then

              !Get the length of the switch N
              X = LOG10(REAL(switch))
              N = INT(X+1.0)

              if (pos .le. N) then
                X1   = REAL(switch)/(10.0**(N-pos))
                Y1   = INT(X1)
                X2   = REAL(switch)/(10.0**(N-pos+1))
                Y2   = INT(X2)*10
                code = Y1 - Y2
              else
                code = -1
              end if

          else

            code = -1

          end if

         GetSwitchDigit = code

      call pop_routine (my_name)
      return
      end



*     ===========================================================
      INTEGER FUNCTION GetSwitchCode(
     .                               switch, ! the switch code string
     .                               pos     ! position of the digital
     .                               )
*     ===========================================================
      use ComponentInterfaceModule
      implicit none
*+  Sub-Program Arguments
      character  switch*(*)
      INTEGER    pos

*+  Purpose
*     Get the digital code at position pos in the switch

*+  Changes
*     990405 ew - programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'GetSwitchCode')

*+  Local variables
      character zcode*2
      INTEGER   code
      INTEGER   N

*- Implementation Section ----------------------------------

      call push_routine (my_name)

          N = LEN_TRIM(switch)

          if (pos.le.N) then

            zcode = switch(pos:pos)
             code =-1

c           string_to_integer_var(value_string, value, numvals)

            if (zcode.eq."0") code =0
            if (zcode.eq."1") code =1
            if (zcode.eq."2") code =2
            if (zcode.eq."3") code =3
            if (zcode.eq."4") code =4
            if (zcode.eq."5") code =5
            if (zcode.eq."6") code =6
            if (zcode.eq."7") code =7
            if (zcode.eq."8") code =8
            if (zcode.eq."9") code =9

          else

            code = -1

          end if

         GetSwitchCode = code

      call pop_routine (my_name)
      return
      end




*     ===========================================================
      subroutine Maize_cover1 (
     .          g_row_spacing,
     .          c_x_row_spacing,
     .          c_y_extinct_coef,
     .          c_num_row_spacing,
     .          cover_leaf,
     .          lai)
*     ===========================================================
      use ComponentInterfaceModule
      implicit none

*+  Sub-Program Arguments
       real g_row_spacing
*
      real       cover_leaf            ! (OUTPUT) fraction of radn that is
                                       !  intercepted by leaves (0-1)
      real       lai                   ! (INPUT) leaf area index ()
      real       c_x_row_spacing(*)    ! (INPUT) rowspace array for extinction_coef lookup
      real       c_y_extinct_coef(*)   ! (INPUT) extinction_coef lookup values
      integer    c_num_row_spacing     ! number of values in the lookup table

*+  Purpose
*       'Cover' by leaves (0-1) . Fraction of radiation reaching the
*       canopy, intercepted by the leaves of the canopy. Extinction
*       coefficient is a function of row spacing.

*+  Mission statement
*       Calculate crop cover using %6

*+  Changes
*   03-11-2000  - Don Gaydon - simplified cover calculation by removing need for
*                              extinction coefficient 'adjustment' parameter

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_cover1')

*+  Local Variables
      real       extinct_coef                ! extinction coefficient

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      extinct_coef = linear_interp_real (g_row_spacing
     :                                   ,c_x_row_spacing
     :                                   ,c_y_extinct_coef
     :                                   ,c_num_row_spacing)


      cover_leaf = 1.0 - exp (-extinct_coef * lai)

      call pop_routine (my_name)
      return
      end



* ====================================================================
       subroutine crop_cover (
     .          extinct_coef,
     .          lai,
     .          cover)
* ====================================================================
      use ComponentInterfaceModule
      implicit none

*+  Sub-Program Arguments
      real extinct_coef
      real lai
      real cover

*+  Purpose
*scc Does crop cover calculation for green, senesced or dead LAI
!scc This is an excellent general routine

*+  Changes
*     15-08-1997 - huth - Programmed and Specified
*
*   Called by srop_water_demand(1) in crop

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'crop_cover')


*- Implementation Section ----------------------------------
      call push_routine (myname)


      cover = (1.0 -exp (-extinct_coef*lai))

      call pop_routine (myname)
      return
      end



* ====================================================================
       subroutine crop_cover_sorghum (
     .          g_row_spacing,
     .          c_x_row_spacing,
     .          c_y_extinct_coef,
     .          c_num_row_spacing,
     .          g_lai,
     .          g_cover_green)
* ====================================================================
      use ComponentInterfaceModule
      implicit none

*+  Sub-Program Arguments
      real g_row_spacing
      real c_x_row_spacing(*)
      real c_y_extinct_coef(*)
      integer c_num_row_spacing
      real g_lai
      real g_cover_green

*+  Purpose
*scc Does crop cover calculation for green, senesced or dead LAI
!scc This is an excellent general routine

*+  Changes
*     15-08-1997 - huth - Programmed and Specified
*
*   Called by srop_water_demand(1) in crop

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'crop_cover_sorghum')

*+  Local Variables
      real extinct_coef

*- Implementation Section ----------------------------------
      call push_routine (myname)

      extinct_coef = linear_interp_real (g_row_spacing
     :                                  ,c_x_row_spacing
     :                                  ,c_y_extinct_coef
     :                                  ,c_num_row_spacing)

      g_cover_green = (1.0 -exp (-extinct_coef*g_lai))

      call pop_routine (myname)
      return
      end

* ====================================================================
       subroutine cropmod_crop_Water_supply(variant)
* ====================================================================
      use CropModModule
      use ComponentInterfaceModule
      implicit none


!+  Sub-Program Arguments
      integer, intent(in out) :: variant

*+  Purpose
*      Get crop water supply when provided

*+  Mission Statement
*     Initialise Slurp

*+  Changes
*

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'cropmod_crop_water_supply')

*+  Local variables
      type(cropwatersupply_type)::watersupply
      integer num_layers

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call unpack_cropwatersupply(variant, watersupply)

      num_layers = watersupply%supplies(1)%num_layers
      g%dlt_sw_dep(1:num_layers)
     :           = -watersupply%supplies(1)%supply(1:num_layers)

      call pop_routine (myname)
      return
      end
* ====================================================================
       subroutine cropmod_send_water_demand ()
* ====================================================================
      use cropmodModule
      use ComponentInterfaceModule
      implicit none

*+  Purpose
*     <insert here>

*+  Mission Statement
*     Tell water balance of the plant water requirements

*+  Changes
*   neilh - 21-09-2001

*+  Local Variables
      type (cropwaterdemand_type) :: waterdemand
      integer num_layers
      integer layer
      real       rlv(max_layer)
*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'cropmod_send_water_demand')

*- Implementation Section ----------------------------------
      call push_routine (myname)


      num_layers = count_of_real_vals(g%root_length,max_layer)
      num_layers = max(1,num_layers)

         do layer = 1, num_layers
            rlv(layer) = divide (g%root_length(layer)
     :                          ,g%dlayer(layer)
     :                          ,0.0)
         enddo



      waterdemand%num_demands = 1
      waterdemand%demands(1)%demand = g%sw_demand
      waterdemand%demands(1)%crop_type = c%crop_type
      waterdemand%demands(1)%crop_ident = c%crop_type
      waterdemand%demands(1)%rlv_layer%rlv(1:num_layers)
     :    = rlv(1:num_layers)
      waterdemand%demands(1)%rlv_layer%num_layers = num_layers
      waterdemand%demands(1)%rlv_layer%layers(1:num_layers)
     :    = g%dlayer(1:num_layers)

      call methodCall_cropwaterdemand(CropWaterDemand_id
     :                               ,waterdemand, .false.)

      call pop_routine (myname)
      return
      end

* ====================================================================
