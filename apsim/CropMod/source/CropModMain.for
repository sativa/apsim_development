C     Last change:  E    14 Feb 2001   12:41 pm

      INCLUDE 'CropMod.inc'


*=====================================================================
      subroutine Main (action, data_string)
*=====================================================================
      use CropModModule
      implicit none

      include 'action.inc'
      include 'const.inc'
      include 'crp_comm.pub'
      include 'string.pub'
      include 'error.pub'                         

*+  Sub-Program Arguments
      character  action*(*)            ! (INPUT) Message action to perform
      character  data_string*(*)       ! (INPUT) Message data

*+  Purpose
*     This module performs crop growth simulation simulates crop phenological
*     development, growth of root, leaf, head, stem and grain,
*     Water and  nitrogen uptake, leaf and root senescense.

*+  Changes
*      271198 ew   
*      250894 sc    specified and programmed
*      011195 jngh  added call to message_unused

*+  Calls

*+  Constant Values
      character  my_name*(*)         ! name of this procedure
      parameter (my_name='CropMod')


*- Implementation Section ----------------------------------
      call push_routine (my_name)
 

      if (action.eq.ACTION_init) then

         !Zero pools inlcuding contants and parameters
         call Zero_Variables (.true.)

         !Read the crop specific contants from ini files
         call CropMod_Initialisation ()

         !Request and receive variables from owner-modules
         call Get_Other_Variables ()           

      elseif (action.eq.ACTION_set_variable) then

         ! Respond to request to reset variable values of variables from other modules
         call Set_My_Variable (data_string)

      elseif (action.eq.ACTION_get_variable) then

         !Respond to request for variable values - from other modules
         call Send_My_Variable (Data_string)

      elseif (action.eq.ACTION_prepare) then !this happens each time step (daily)

         if (g%plant_status.ne.status_out) then

            !Zero all daily rate variables
            call Zero_Daily_Variables ()

            !Request and receive variables from owner-modules
            call Get_Other_Variables ()      

            !Do crop processes prepare
            call Simulation_Prepare ()

         else
            ! Crop not in the field, do nothing
         endif

      elseif (action.eq.ACTION_process) then


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

      elseif (action.eq.ACTION_sow) then

         if (crop_my_type (c%crop_type)) then

            !request and receive variables from owner-modules
            call Get_Other_Variables ()

            !start crop, read the sow information and do  more initialisations
            call Start_Crop ()

         else

            ! not my type!
            call Message_unused ()

         endif

      elseif (action.eq.ACTION_harvest) then

         if (Crop_my_type (c%crop_type)) then
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
         else
            ! not my type!
            call Message_unused ()
         endif

      elseif (action.eq.ACTION_end_crop) then

         if (crop_my_type (c%crop_type)) then

            !end crop - turn the stover into residue
            call End_Crop ()

            !Zero all the globals, but not the contants and parameters
            call Zero_Variables (.false.)

            !Set plant status to status_out and stage to plant_end
            if (g%plant_status.ne.status_out) then
                g%plant_status  = status_out
                g%current_stage = real (plant_end)
            end if

         else
            ! not my type!
            call Message_unused ()
         endif


      elseif (action.eq.ACTION_kill_crop) then
         if (crop_my_type (c%crop_type)) then
            ! kill crop - died, but biomass remain in field
            call Kill_Crop (
     .          g%plant_status,
     .          g%dm_green,
     .          g%dm_senesced,
     .          g%dm_dead)
         else
            ! not my type!
            call Message_unused ()
         endif
      else
         ! don't use message
         call Message_unused ()
      endif

 
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
      implicit none
      include 'error.pub'

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

      PRINT *,"crop type is ", c%crop_type
      PRINT *


      !Read the crop specific constants

       call Crop_Read_Constants()
       

      g%current_stage = real (plant_end)
      g%plant_status = status_out
 
      call pop_routine (my_name)
      return
      end

*     ===========================================================
      subroutine Start_Crop ()
*     ===========================================================
      use CropModModule
      implicit none
      include 'const.inc'            !  blank
      include 'intrface.pub'
      include 'error.pub'                         

*+  Purpose
*       Start crop using parameters specified in passed record

*+  Changes
*     010994 sc   specified and programmed
*     090695 psc  add row spacing read
*     220896 jngh changed extract to collect
*                 removed data_record from argument

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'Start_Crop')

*+  Local Variables
      character  cultivar*20           ! name of cultivar
      integer    numvals               ! number of values found in array
      character  string*200            ! output string

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)


      !-----------------------------------------------------------
      !Read sowing information
      !-----------------------------------------------------------
      call Write_string ( 'Sowing initiate')
 
cjh      if (data_record.ne.blank) then
 
         call collect_char_var ('cultivar', '()'
     :                        , cultivar, numvals)

         call collect_real_var ('plants', '()'
     :                        , g%plants, numvals, 0.0, 400.0)

         call collect_real_var (
     :                          'sowing_depth', '(mm)'
     :                        , g%sowing_depth, numvals
     :                        , 0.0, 100.0)

         call collect_real_var_optional (
     :                          'row_spacing', '(m)'
     :                        , g%row_spacing, numvals
     :                        , 0.0, 2.0)

         if (numvals.eq.0) then
            g%row_spacing = c%row_spacing_default
         endif


         !scc added FTN 11/10/95
         call collect_real_var_optional (
     :                      'tiller_no_fertile', '()'
     :                    , g%tiller_no_fertile, numvals
     :                    , 0.0, 10.0)
 
         if (numvals.eq.0) then
            g%tiller_no_fertile = 0.0
         else
         endif

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
      subroutine Read_Root_Params ()
*     ===========================================================
      use CropModModule
      implicit none
      include   'const.inc'            ! new_line,  blank,
      include 'data.pub'
      include 'read.pub'                          
      include 'error.pub'                         

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

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      call write_string (new_line
     :                  //'   - Reading root profile parameters')
 

      !-----------------------------------------------------------
      !Read the root parameters from section Cultivar.Module.Parameters
      !-----------------------------------------------------------

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
      do layer = 1, num_layers
         p%ll_dep(layer) = ll(layer)*g%dlayer(layer)
      enddo
 
      call read_real_array (section_name
     :                     , 'kl', max_layer, '()'
     :                     , p%kl, num_layers
     :                     , 0.0, c%kl_ub)
 
      call read_real_array (section_name
     :                     , 'xf', max_layer, '()'
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



*     ===========================================================
      subroutine Read_Module_Switches ()
*     ===========================================================
      use CropModModule
      implicit none
      include   'const.inc'
      include 'read.pub'
      include 'error.pub'
      include 'datastr.pub'

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


*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 

      !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      !REPORT READING, AND READ THE CROP TYPE
      !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      call write_string (new_line//'    - Reading constants')

      !Read the crop type
      call read_char_var (section_name
     :                     , 'crop_type', '()'
     :                     , c%crop_type, numvals)
 
 
      !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      ! READ MODULE SWITCHES
      !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      call read_integer_var_optional (section_name
     :                    , 'module_switch', '()'
     :                    , c%module_switch, numvals
     :                    , 0, 3)

      !If module_switch is not specified, use default 1
      if (numvals.eq.0) then  
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

      !If module_switch is zero, use the sub module switches
      else

          call read_char_var_optional (section_name
     :                     , 'wat_switch', '()'
     :                     , c%wat_switch, numvals)
          if (numvals.eq.0)  c%wat_switch = '111111111'


          call read_char_var_optional (section_name
     :                     , 'phen_switch', '()'
     :                     , c%phen_switch, numvals)
          if (numvals.eq.0)  c%phen_switch = '111111111'

          call read_char_var_optional (section_name
     :                     , 'carb_switch', '()'
     :                     , c%carb_switch, numvals)
          if (numvals.eq.0)  c%carb_switch = '111111111'

          call read_char_var_optional (section_name
     :                     , 'parti_switch', '()'
     :                     , c%part_switch, numvals)
          if (numvals.eq.0)  c%part_switch = '111111111'

          call read_char_var_optional (section_name
     :                     , 'leafno_switch', '()'
     :                     , c%leafno_switch, numvals)
          if (numvals.eq.0)  c%leafno_switch = '111111111'

          call read_char_var_optional (section_name
     :                     , 'tiller_switch', '()'
     :                     , c%tiller_switch, numvals)
          if (numvals.eq.0)  c%tiller_switch = '111111111'

          call read_char_var_optional (section_name
     :                     , 'can_switch', '()'
     :                     , c%can_switch, numvals)
          if (numvals.eq.0)  c%can_switch = '111111111'

          call read_char_var_optional (section_name
     :                     , 'roots_switch', '()'
     :                     , c%root_switch, numvals)
          if (numvals.eq.0)  c%root_switch = '111111111'

          call read_char_var_optional (section_name
     :                     , 'sen_switch', '()'
     :                     , c%sen_switch, numvals)
          if (numvals.eq.0)  c%sen_switch = '111111111'

          call read_char_var_optional (section_name
     :                     , 'nit_switch', '()'
     :                     , c%nit_switch, numvals)
          if (numvals.eq.0)  c%nit_switch = '111111111'

      endif


      if (c%crop_type .eq. 'wheat') then
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
          c%sen_switch    = '411411111'
          c%nit_switch    = '444414111'
      end if


      call pop_routine (my_name)
      return
      end














!     ===========================================================
      subroutine AllocInstance (InstanceName, InstanceNo)
!     ===========================================================
      use CropModModule
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
      use CropModModule
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
      use CropModModule
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
      implicit none
      include 'error.pub'

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





*     ================================================================
      subroutine Send_My_Variable (variable_name)
*     ================================================================
      use CropModModule
      implicit none
      include 'convert.inc'          ! gm2kg, sm2ha, mm2cm, cmm2cc, sm2smm
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

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 

      !================================================================
      !crop type
       if (variable_name .eq. 'crop_type') then
         call respond2get_char_var (variable_name
     :                             , '()'
     :                             , c%crop_type)
 

      !================================================================
      !Days after sowing
      elseif ((variable_name .eq. 'das'    ) .or.
     :        (variable_name .eq. 'das_int')) then
      
         das_real = sum_between (sowing, now, g%days_tot)
         das_int  = int(das_real+0.9999999)

         call respond2get_integer_var (variable_name
     :                             , '(days)'
     :                             , das_int)

      !================================================================
      !crop status, thermal time and development stages

      elseif (variable_name .eq. 'plant_status') then
         call respond2get_char_var (variable_name
     :                             , '()'
     :                             , g%plant_status)

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

      elseif (variable_name .eq. 'stage') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%current_stage)


      elseif ((variable_name .eq. 'zadok_stage') .or.
     :        (variable_name .eq. 'dc_stage'))    then
         stage_no = INT(g%zadok_stage + 0.5)
         call respond2get_integer_var (variable_name
     :                             , '()'
     :                             , stage_no)


      elseif (variable_name .eq. 'dlt_stage') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%dlt_stage)


      elseif (variable_name .eq. 'dlt_tt') then
         call respond2get_real_var (variable_name
     :                             , '(oCd)'
     :                             , g%dlt_tt)
 
      elseif (variable_name .eq. 'tt_tot') then
         call respond2get_real_array (variable_name
     :                             , '(oC)'
     :                             , g%tt_tot
     :                             , max_stage)

      elseif (variable_name .eq. 'tt_sum') then
         call respond2get_real_var (variable_name
     :                             , '(ddays)'
     :                             , g%tt_tot(INT(g%current_stage)))

      elseif (variable_name .eq. 'days_tot') then
         call respond2get_real_array (variable_name
     :                             , '()'
     :                             , g%days_tot
     :                             , max_stage)

      elseif (variable_name .eq. 'phase_tt') then
         call respond2get_real_array (variable_name
     :                             , '(oC)'
     :                             , g%phase_tt
     :                             , max_stage)

      !the following two not generalised
      elseif (variable_name .eq. 'dlt_tt_fm') then
         call respond2get_real_var (variable_name
     :                             , '(oCd)'
     :                             , g%dlt_tt_fm)
 
      elseif (variable_name .eq. 'tt_tot_fm') then
         call respond2get_real_array (variable_name
     :                             , '(oC)'
     :                             , g%tt_tot_fm
     :                             , max_stage)

      !================================================================
      !flowering and maturity dates

      elseif (variable_name .eq. 'flowering_date') then
         call respond2get_integer_var (variable_name
     :                             , '(doy)'
     :                             , g%flowering_date)
 
      elseif (variable_name .eq. 'maturity_date') then
         call respond2get_integer_var (variable_name
     :                             , '(doy)'
     :                             , g%maturity_date)
 
      elseif (variable_name .eq. 'flowering_das') then
         call respond2get_integer_var (variable_name
     :                             , '(days)'
     :                             , g%flowering_das)
 
      elseif (variable_name .eq. 'maturity_das') then
         call respond2get_integer_var (variable_name
     :                             , '(days)'
     :                             , g%maturity_das)


      !================================================================
      ! crop canopy - leaf no, tiller no, LAI and crop height

      elseif (variable_name .eq. 'leaf_primodia') then
         call respond2get_real_var (variable_name
     :                             , 'lvs'
     :                             , g%leaf_primodia)

      elseif (variable_name .eq. 'leaf_no_final') then
         call respond2get_real_var (variable_name
     :                             , 'lvs'
     :                             , g%leaf_no_final)

      elseif (variable_name .eq. 'leaf_no') then
        leaf_no_now = sum_between (emerg, harvest_ripe, g%leaf_no)
         call respond2get_real_var (variable_name
     :                              , '()'
     :                              , leaf_no_now)


      elseif (variable_name .eq. 'dlt_leaf_no') then
         call respond2get_real_var (variable_name
     :                              , '()'
     :                              , g%dlt_leaf_no
     :                              )
 
      elseif (variable_name .eq. 'leaf_no_dead') then
         call respond2get_real_array (variable_name
     :                              , '()'
     :                              , g%leaf_no_dead
     :                              , max_stage)
 
      elseif (variable_name .eq. 'leaf_area') then
         call respond2get_real_array (variable_name
     :                              , '()'
     :                              , g%leaf_area
     :                              , max_leaf)
 

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

      elseif (variable_name .eq. 'lai') then
         call respond2get_real_var (variable_name
     :                             , '(m^2/m^2)'
     :                             , g%lai)

      elseif (variable_name .eq. 'lai_max') then
         call respond2get_real_var (variable_name
     :                             , '(m^2/m^2)'
     :                             , g%lai_max)

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
 
      elseif (variable_name .eq. 'tlai_dead') then
         call respond2get_real_var (variable_name
     :                             , '(m^2/m^2)'
     :                             , g%tlai_dead)

      elseif (variable_name .eq. 'sla') then
         call respond2get_real_var (variable_name
     :                             , '(mm2/g)'
     :                             , divide(g%lai*sm2smm
     :                             , g%dm_green(leaf), 0.0))

      elseif (variable_name .eq. 'dlt_lai') then
         call respond2get_real_var (variable_name
     :                             , '(m^2/m^2)'
     :                             , g%dlt_lai)

      elseif (variable_name .eq. 'dlt_slai') then
         call respond2get_real_var (variable_name
     :                             , '(m^2/m^2)'
     :                             , g%dlt_slai)

      !...................................................
      ! These dlts are not made available yet

      elseif (variable_name .eq. 'dlt_slai_age') then
         call respond2get_real_var (variable_name
     :                             , '(m^2/m^2)'
     :                             , g%dlt_slai_age)

      elseif (variable_name .eq. 'dlt_slai_light') then
         call respond2get_real_var (variable_name
     :                             , '(m^2/m^2)'
     :                             , g%dlt_slai_light)
 
      elseif (variable_name .eq. 'dlt_slai_water') then
         call respond2get_real_var (variable_name
     :                             , '(m^2/m^2)'
     :                             , g%dlt_slai_water)

      elseif (variable_name .eq. 'dlt_slai_nitrogen') then
         call respond2get_real_var (variable_name
     :                             , '(m^2/m^2)'
     :                             , g%dlt_slai_nitrogen)
      !...................................................

      elseif (variable_name .eq. 'plants') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%plants)

      elseif (variable_name .eq. 'height') then
         call respond2get_real_var (variable_name
     :                             , '(mm)'
     :                             , g%canopy_height)
 
      elseif (variable_name .eq. 'tiller_no') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%tiller_no_fertile)

      elseif (variable_name .eq. 'tiller_no_fertile') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%tiller_no_fertile)

      elseif (variable_name .eq. 'grain_no') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%grain_no)
 
      elseif (variable_name .eq. 'grain_size') then
         grain_size = divide (g%dm_green(grain) + g%dm_dead(grain)
     :                 , g%grain_no, 0.0)
         call respond2get_real_var (variable_name
     :                             , '(g)'
     :                             , grain_size)

      !================================================================
      ! Root depth and length

      elseif (variable_name .eq. 'root_depth') then
         call respond2get_real_var (variable_name
     :                             , '(mm)'
     :                             , g%root_depth)

      elseif (variable_name .eq. 'root_length') then
         num_layers = count_of_real_vals (g%dlayer, max_layer)
         call respond2get_real_array (variable_name
     :                               , '(mm/mm2)'
     :                               , g%root_length
     :                               , num_layers)

      elseif (variable_name .eq. 'rlv') then
         num_layers = count_of_real_vals (g%dlayer, max_layer)
         do layer = 1, num_layers
            rlv(layer) = divide (g%root_length(layer)
     :                          ,g%dlayer(layer)
     :                          ,0.0)
         enddo
         call respond2get_real_array (variable_name
     :                               , '(mm/mm3)'
     :                               , rlv
     :                               , num_layers)


      !===============================================================
      !plant biomass

      !----------------------------------------------------------------
      !Biomass in g/m2

      elseif (variable_name .eq. 'leaf_wt') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%dm_green(leaf))
 
      elseif (variable_name .eq. 'dleaf_wt') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%dm_senesced(leaf))


      elseif (variable_name .eq. 'tleaf_wt') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%dm_green(leaf)
     :                             + g%dm_senesced(leaf))

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
 

      elseif (variable_name .eq. 'root_wt') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%dm_green(root))
 

      elseif (variable_name .eq. 'droot_wt') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%dm_senesced(root))
 
      elseif (variable_name .eq. 'troot_wt') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%dm_green(root)
     :                             + g%dm_senesced(root))

      elseif (variable_name .eq. 'biomass_wt') then

         biomass = sum_real_array (g%dm_green, max_part)
     :           - g%dm_green(root) - g%dm_green(energy)
     :           + sum_real_array (g%dm_senesced, max_part)
     :           - g%dm_senesced(root) - g%dm_senesced(energy)
     :           + sum_real_array (g%dm_dead, max_part)
     :           - g%dm_dead(root) - g%dm_dead(energy)

         call respond2get_real_var (variable_name
     :                             , '(g/m2)'
     :                             , biomass)


      elseif (variable_name .eq. 'green_biomass_wt') then
         biomass = sum_real_array (g%dm_green, max_part)
     :           - g%dm_green(root) - g%dm_green(energy)
 
         call respond2get_real_var (variable_name
     :                             , '(g/m2)'
     :                             , biomass)
 
      elseif (variable_name .eq. 'stover_wt') then

         yield = (g%dm_green(grain) + g%dm_dead(grain))

         biomass = sum_real_array (g%dm_green, max_part)
     :           - g%dm_green(root) - g%dm_green(energy)
     :           + sum_real_array (g%dm_senesced, max_part)
     :           - g%dm_senesced(root) - g%dm_senesced(energy)
     :           + sum_real_array (g%dm_dead, max_part)
     :           - g%dm_dead(root) - g%dm_dead(energy)

         stover = biomass - yield

         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , stover)

      !----------------------------------------------------
      !Biomass arrays in g/m2

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

      !-------------------------------------------------------------
      !Biomass output in  kg/ha
      elseif (variable_name .eq. 'yield') then
         yield = (g%dm_green(grain) + g%dm_dead(grain))
     :           * gm2kg / sm2ha
         call respond2get_real_var (variable_name
     :                             , '(kg/ha)'
     :                             , yield)
 

      elseif (variable_name .eq. 'biomass') then
         !The energy part should not be deleted from the following - ew
         biomass = (sum_real_array (g%dm_green, max_part)
     :           - g%dm_green(root) - g%dm_green(energy)
     :           + sum_real_array (g%dm_senesced, max_part)
     :           - g%dm_senesced(root) - g%dm_senesced(energy)
     :           + sum_real_array (g%dm_dead, max_part)
     :           - g%dm_dead(root) - g%dm_dead(energy))
     :           * gm2kg / sm2ha

         call respond2get_real_var (variable_name
     :                             , '(kg/ha)'
     :                             , biomass)

      !mjr 05/97 output stover (kg/ha)
      elseif (variable_name .eq. 'stover') then

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

         call respond2get_real_var (variable_name
     :                             , '(kg/ha)'
     :                             , stover)
 
      elseif (variable_name .eq. 'green_biomass') then
         biomass = (sum_real_array (g%dm_green, max_part)
     :              - g%dm_green(root)- g%dm_green(energy))
     :             * gm2kg / sm2ha
 
         call respond2get_real_var (variable_name
     :                             , '(kg/ha)'
     :                             , biomass)
 

      !scc 10/95 output harvest index
      elseif (variable_name .eq. 'hi') then

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

         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , hi)

      !---------------------------------------------------------
      !biomass deltas

      elseif (variable_name .eq. 'dlt_dm_water') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%dlt_dm_water)

      elseif (variable_name .eq. 'dlt_dm_light') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%dlt_dm_light)


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

      elseif (variable_name .eq. 'dlt_dm_senesced') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g%dlt_dm_senesced
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
 

      !================================================================
      !Plant Water

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

      elseif (variable_name .eq. 'swdef_tiller') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%swdef_tiller)


      elseif ((variable_name .eq. 'sw_uptake').or.
     :        (variable_name .eq. 'ep'))        then
         num_layers = count_of_real_vals (g%dlayer, max_layer)
         call respond2get_real_array (variable_name
     :                               , '(mm)'
     :                               , g%dlt_sw_dep
     :                               , num_layers)

      elseif (variable_name .eq. 'transpiration') then
         deepest_layer = find_layer_no (g%root_depth, g%dlayer
     :                                , max_layer)
         sw_supply_sum = sum_real_array (g%dlt_sw_dep, deepest_layer)
         call respond2get_real_var (variable_name
     :                             , '(mm)'
     :                             , -sw_supply_sum)
 
      elseif ((variable_name .eq. 'transpiration_tot').or.
     :        (variable_name .eq. 'cep'))      then
         call respond2get_real_var (variable_name
     :                             , '(mm)'
     :                             ,  g%transpiration_tot)
 

      elseif (variable_name .eq. 'esw_layer') then
 
         num_layers = count_of_real_vals (g%dlayer, max_layer)

         do layer = 1, num_layers
            esw_layer(layer) = g%sw_dep(layer) - p%ll_dep(layer)
            esw_layer(layer) = l_bound (esw_layer(layer), 0.0)
         enddo

         call respond2get_real_array (variable_name
     :                               , '(mm)'
     :                               , esw_layer
     :                               , num_layers)

      elseif (variable_name .eq. 'esw_profile') then
 
         num_layers = count_of_real_vals (g%dlayer, max_layer)
         esw_sum = 0.0
         do layer = 1, num_layers
            esw_layer(layer) = g%sw_dep(layer) - p%ll_dep(layer)
            esw_layer(layer) = l_bound (esw_layer(layer), 0.0)
            esw_sum = esw_sum + esw_layer(layer)
         enddo

         call respond2get_real_var  (variable_name
     :                               , '(mm)'
     :                               , esw_sum)


      !cscc/glh soil water deficit below dul
      elseif (variable_name .eq. 'sw_deficit') then
         deepest_layer = find_layer_no (g%root_depth, g%dlayer
     :       , max_layer)
         num_layers = count_of_real_vals (g%dlayer, max_layer)
         do i=1,num_layers
            sw_deficit(i) = l_bound(g%dul_dep(i) - g%sw_dep(i),0.0)
         enddo
 
         call respond2get_real_array (variable_name
     :                               , '(mm)'
     :                               , sw_deficit
     :                               , num_layers)

      elseif (variable_name .eq. 'vpd') then
         call respond2get_real_var (variable_name
     :                             , '(kpa)'
     :                             , g%vpd)
 
      elseif (variable_name .eq. 'transp_eff') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%transp_eff)

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

      elseif (variable_name .eq. 'sw_supply_sum') then
         call respond2get_real_var (variable_name
     :                             , '(mm)'
     :                             , g%sw_supply_sum)
 
      elseif (variable_name .eq. 'sw_supply_demand_ratio') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , divide(g%sw_supply_sum,
     :                                      g%sw_demand,0.0))
 
      elseif (variable_name .eq. 'll')  then

         num_layers = count_of_real_vals (g%dlayer, max_layer)

         call fill_real_array(ll,0.0, max_layer)

         do layer = 1, num_layers
            ll(layer) = divide(p%ll_dep(layer), g%dlayer(layer), 0.0)
         end do

         call respond2get_real_array (variable_name
     :                               , '(%)'
     :                               , ll
     :                               , num_layers)

      elseif (variable_name .eq. 'll_dep')  then

         call respond2get_real_array (variable_name
     :                               , '(mm)'
     :                               , p%ll_dep
     :                               , num_layers)

      elseif (variable_name .eq. 'kl')  then

         call respond2get_real_array (variable_name
     :                               , '()'
     :                               , p%kl
     :                               , num_layers)

      elseif (variable_name .eq. 'xf')  then

         call respond2get_real_array (variable_name
     :                               , '()'
     :                               , p%xf
     :                               , num_layers)

      !=============================================================
      ! plant nitrogen

      !nitrogen demand and supply

      elseif (variable_name .eq. 'leaf_nd') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%n_demand(leaf))

      elseif (variable_name .eq. 'stem_nd') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%n_demand(stem))

      elseif (variable_name .eq. 'flower_nd') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%n_demand(flower))

      elseif (variable_name .eq. 'grain_nd') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%n_demand(grain))

      elseif (variable_name .eq. 'root_nd') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%n_demand(root))

      elseif (variable_name .eq. 'n_demand') then
         N_demand = sum_real_array (g%N_demand, max_part)
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , N_demand)
      !THIS IS SWIM STUFF
      elseif (variable_name .eq. 'no3_demand') then
         N_demand = sum_real_array (g%N_demand, max_part)
     :            * gm2kg/sm2ha
         call respond2get_real_var (variable_name
     :                             , '(kg/ha)'
     :                             , N_demand)
 
      elseif (variable_name .eq. 'n_supply_soil') then
         deepest_layer = find_layer_no (g%root_depth,g%dlayer,max_layer)
         N_uptake_sum = - sum_real_array (g%dlt_NO3gsm, deepest_layer)
     :                  - sum_real_array (g%dlt_NH4gsm, deepest_layer)
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , N_uptake_sum)

      !nitrogen uptake

      elseif (variable_name .eq. 'n_massflow_uptake') then
         deepest_layer = find_layer_no (g%root_depth,g%dlayer,max_layer)
         N_uptake_sum = - sum_real_array (g%dlt_NO3gsm_massflow,
     :                                   deepest_layer)
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , N_uptake_sum)


      elseif (variable_name .eq. 'n_diffusion_uptake') then
         deepest_layer = find_layer_no (g%root_depth,g%dlayer,max_layer)
         N_uptake_sum = - sum_real_array (g%dlt_NO3gsm_diffusion,
     :                                   deepest_layer)
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , N_uptake_sum)

      elseif (variable_name .eq. 'n_total_uptake') then
         deepest_layer = find_layer_no (g%root_depth,g%dlayer,max_layer)
         apt_N_up      = - sum_real_array (g%dlt_NO3gsm_massflow,
     :                                   deepest_layer)
         N_uptake_sum  = - sum_real_array (g%dlt_NO3gsm_diffusion,
     :                                   deepest_layer)
         N_uptake_sum  = N_uptake_sum + apt_N_up

         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , N_uptake_sum)

      elseif (variable_name .eq. 'no3_tot') then
         deepest_layer = find_layer_no (g%root_depth, g%dlayer
     :                                , max_layer)
         NO3gsm_tot = sum_real_array (g%NO3gsm, deepest_layer)
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , NO3gsm_tot)


      elseif (variable_name .eq. 'nh4_tot') then
         deepest_layer = find_layer_no (g%root_depth, g%dlayer
     :                                , max_layer)
         apt_N_up = sum_real_array (g%NH4gsm, deepest_layer)
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , apt_N_up)

      elseif (variable_name .eq. 'n_cum_uptake') then

         biomass_n = (sum_real_array (g%n_green,    max_part)
     :             +  sum_real_array (g%n_senesced, max_part)
     :             +  sum_real_array (g%n_dead,     max_part))

         call respond2get_real_var (variable_name
     :                             , '(g/m2)'
     :                             , biomass_n)


      !----------------------------------------------------------
      !Nitrogen content

      elseif (variable_name .eq. 'biomass_n') then

         biomass_n = (sum_real_array (g%n_green, max_part)
     :             - g%n_green(root) - g%n_green(energy)
     :             + sum_real_array (g%n_senesced, max_part)
     :             - g%n_senesced(root) - g%n_senesced(energy)
     :             + sum_real_array (g%n_dead, max_part)
     :             - g%n_dead(root) - g%n_dead(energy))
 
         call respond2get_real_var (variable_name
     :                             , '(g/m2)'
     :                             , biomass_n)
 
      elseif (variable_name .eq. 'green_biomass_n') then
         biomass_n = (sum_real_array (g%n_green, max_part)
     :                - g%n_green(root) - g%n_green(energy))
 
         call respond2get_real_var (variable_name
     :                             , '(g/m2)'
     :                             , biomass_n)

      elseif (variable_name .eq. 'stover_n') then

         apt_N_up = g%N_green(leaf)+g%n_green(stem)+g%n_green(flower)
     :       +g%N_senesced(leaf)+g%n_senesced(stem)+g%n_senesced(flower)
     :       +g%N_dead(leaf)+g%n_dead(stem)+g%n_dead(flower)

         call respond2get_real_var (variable_name
     :                             , '(g/m2)'
     :                             , apt_N_up)

      elseif (variable_name .eq. 'grain_n') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%n_green(grain))

      elseif (variable_name .eq. 'gleaf_n') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%n_green(leaf))

      elseif (variable_name .eq. 'dleaf_n') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%n_senesced(leaf)
     :                             + g%n_dead(leaf))

      elseif (variable_name .eq. 'tleaf_n') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%n_senesced(leaf)
     :                             + g%n_dead(leaf)
     :                             + g%n_green(leaf))

       elseif (variable_name .eq. 'stem_n') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%n_green(stem))

       elseif (variable_name .eq. 'flower_n') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%n_green(flower))

      elseif (variable_name .eq. 'groot_n') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%n_green(root))

      elseif (variable_name .eq. 'droot_n') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%n_senesced(root)
     :                             + g%n_dead(root))

      elseif (variable_name .eq. 'troot_n') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%n_senesced(root)
     :                             + g%n_dead(root)
     :                             + g%n_green(root))


      !---------------------------------------------------------------
      !Nitrogen content arrays

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
 

      !-----------------------------------------------------------------
      !Nitrogne deltas

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


      !-----------------------------------------------------------------
      !Nitrogen concentrations

      elseif (variable_name .eq. 'sln') then
         call respond2get_real_var (variable_name
     :                             , '(gN/m2leaf)'
     :                             , divide(
     :                    g%N_green(leaf), g%lai, 0.0))
 
      elseif (variable_name .eq. 'n_conc_stover') then
         N_conc = divide ((g%N_green(leaf)
     :                    + g%N_green(stem)
     :                    + g%N_green(flower))
     :                  , (g%dm_green(leaf)
     :                    + g%dm_green(stem)
     :                    + g%dm_green(flower))
     :                  , 0.0) * 100.
 
         call respond2get_real_var (variable_name
     :                             , '(%)'
     :                             , N_conc)
 
      elseif (variable_name .eq. 'n_conc_leaf') then
         N_conc = divide (g%N_green(leaf)
     :                  , g%dm_green(leaf)
     :                  , 0.0) * 100.
 
         call respond2get_real_var (variable_name
     :                             , '(%)'
     :                             , N_conc)
 
      elseif (variable_name .eq. 'n_conc_stem') then
         N_conc = divide (g%N_green(stem)
     :                  , g%dm_green(stem)
     :                  , 0.0) * 100.
 
         call respond2get_real_var (variable_name
     :                             , '(%)'
     :                             , N_conc)

      elseif (variable_name .eq. 'n_conc_root') then
         N_conc = divide (g%N_green(root)
     :                  , g%dm_green(root)
     :                  , 0.0) * 100.
 
         call respond2get_real_var (variable_name
     :                             , '(%)'
     :                             , N_conc)
 
      elseif (variable_name .eq. 'n_conc_grain') then
         N_conc = divide (g%N_green(grain)
     :                  , g%dm_green(grain)
     :                  , 0.0) * 100.
 
         call respond2get_real_var (variable_name
     :                             , '(%)'
     :                             , N_conc)


      elseif (variable_name .eq. 'n_conc_leaf_crit') then
         N_conc = g%N_conc_crit(leaf) * 100.
 
         call respond2get_real_var (variable_name
     :                             , '(%)'
     :                             , N_conc)

      elseif (variable_name .eq. 'n_conc_stem_crit') then
         N_conc = g%N_conc_crit(stem) * 100.
 
         call respond2get_real_var (variable_name
     :                             , '(%)'
     :                             , N_conc)

      elseif (variable_name .eq. 'n_conc_flower_crit') then
         N_conc = g%N_conc_crit(flower) * 100.
 
         call respond2get_real_var (variable_name
     :                             , '(%)'
     :                             , N_conc)

      elseif (variable_name .eq. 'n_conc_root_crit') then
         N_conc = g%N_conc_crit(root) * 100.
 
         call respond2get_real_var (variable_name
     :                             , '(%)'
     :                             , N_conc)

      elseif (variable_name .eq. 'n_conc_stover_crit') then
         N_conc = divide ((g%N_conc_crit(leaf)*g%dm_green(leaf)
     :                    + g%N_conc_crit(stem)*g%dm_green(stem))
     :                  , (g%dm_green(leaf)
     :                    + g%dm_green(stem))
     :                  , 0.0) * 100.
 
         call respond2get_real_var (variable_name
     :                             , '(%)'
     :                             , N_conc)


      elseif (variable_name .eq. 'n_conc_leaf_max') then
         N_conc = g%N_conc_max(leaf) * 100.
 
         call respond2get_real_var (variable_name
     :                             , '(%)'
     :                             , N_conc)

      elseif (variable_name .eq. 'n_conc_stem_max') then
         N_conc = g%N_conc_max(stem) * 100.
 
         call respond2get_real_var (variable_name
     :                             , '(%)'
     :                             , N_conc)

      elseif (variable_name .eq. 'n_conc_flower_max') then
         N_conc = g%N_conc_max(flower) * 100.
 
         call respond2get_real_var (variable_name
     :                             , '(%)'
     :                             , N_conc)

      elseif (variable_name .eq. 'n_conc_root_max') then
         N_conc = g%N_conc_max(root) * 100.
 
         call respond2get_real_var (variable_name
     :                             , '(%)'
     :                             , N_conc)

      elseif (variable_name .eq. 'n_conc_stover_max') then
         N_conc = divide ((g%N_conc_max(leaf)*g%dm_green(leaf)
     :                   + g%N_conc_max(stem)*g%dm_green(stem))
     :                  , (g%dm_green(leaf)
     :                   + g%dm_green(stem))
     :                  , 0.0) * 100.
 
         call respond2get_real_var (variable_name
     :                             , '(%)'
     :                             , N_conc)
 
      elseif (variable_name .eq. 'n_conc_leaf_min') then
         N_conc = g%N_conc_min(leaf) * 100.
 
         call respond2get_real_var (variable_name
     :                             , '(%)'
     :                             , N_conc)

      elseif (variable_name .eq. 'n_conc_stem_min') then
         N_conc = g%N_conc_min(stem) * 100.
 
         call respond2get_real_var (variable_name
     :                             , '(%)'
     :                             , N_conc)

      elseif (variable_name .eq. 'n_conc_flower_min') then
         N_conc = g%N_conc_min(flower) * 100.
 
         call respond2get_real_var (variable_name
     :                             , '(%)'
     :                             , N_conc)

      elseif (variable_name .eq. 'n_conc_root_min') then
         N_conc = g%N_conc_min(root) * 100.
 
         call respond2get_real_var (variable_name
     :                             , '(%)'
     :                             , N_conc)

      elseif (variable_name .eq. 'n_conc_stover_min') then
         N_conc = divide ((g%N_conc_min(leaf)*g%dm_green(leaf)
     :                    + g%N_conc_min(stem)*g%dm_green(stem))
     :                  , (g%dm_green(leaf)
     :                    + g%dm_green(stem))
     :                  , 0.0) * 100.
 
         call respond2get_real_var (variable_name
     :                             , '(%)'
     :                             , N_conc)
 
      elseif (variable_name .eq. 'n_grain_pcnt') then
         grain_N_pcnt = divide (g%N_green(grain)
     :                        , g%dm_green(grain), 0.0)
     :                        * fract2pcnt
         call respond2get_real_var (variable_name
     :                             , '(%)'
     :                             , grain_N_pcnt)
 

      !---------------------------------------------------
      !Nitrogen stress factors

      elseif (variable_name .eq. 'nfact_photo') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%nfact_photo)
 
      elseif (variable_name .eq. 'nfact_pheno') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%nfact_pheno)
 
      elseif (variable_name .eq. 'nfact_expan') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%nfact_expansion)
 
      elseif (variable_name .eq. 'nfact_tiller') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%nfact_tiller)

      elseif (variable_name .eq. 'nfact_grain') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%nfact_grain_conc)
 
      elseif (variable_name .eq. 'nfact_grain_tot') then
         call respond2get_real_array (variable_name
     :                             , '()'
     :                             , g%cnd_grain_conc
     :                             , max_stage)




      !---------------------------------------------------
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
         call Message_unused ()
 
      endif


      call pop_routine (my_name)
      return
      end




*     ===============================================================
      subroutine Set_My_Variable (Variable_name)
*     ===============================================================
      use CropModModule
      implicit none
      include 'error.pub'
      include 'intrface.pub'

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
         call collect_integer_var (variable_name, '()'
     :                             , c%co2switch, numvals
     :                             , 0, 1)

      elseif (variable_name .eq. 'co2_level') then
         call collect_real_var (variable_name, '()'
     :                             , c%co2level, numvals
     :                             , 0.0, 2000.0)


      elseif (variable_name .eq. 'vern_sen') then
         call collect_real_var (variable_name, '()'
     :                             , p%vern_sen, numvals
     :                             , 0.0, 2000.0)
         p%vern_sen_internal   = p%vern_sen   * 0.0054545 + 0.0003


      else if (variable_name .eq. 'photop_sen') then
         call collect_real_var (variable_name, '()'
     :                             , p%photop_sen, numvals
     :                             , 0.0, 2000.0)
         p%photop_sen_internal = p%photop_sen * 0.002



      else if (variable_name .eq. 'tt_germ_to_emerg') then
         call collect_real_var (variable_name, '()'
     :                             , p%tt_germ_to_emerg, numvals
     :                             , 0.0, 2000.0)

      else if (variable_name .eq. 'tt_emerg_to_endjuv') then
         call collect_real_var (variable_name, '()'
     :                             , p%tt_emerg_to_endjuv, numvals
     :                             , 0.0, 2000.0)

      else if (variable_name .eq. 'tt_endjuv_to_init') then
         call collect_real_var (variable_name, '()'
     :                             , p%tt_endjuv_to_init, numvals
     :                             , 0.0, 2000.0)

      else if (variable_name .eq. 'tt_init_to_flag') then
         call collect_real_var (variable_name, '()'
     :                             , p%tt_init_to_flag, numvals
     :                             , 0.0, 2000.0)

      else if (variable_name .eq. 'tt_flag_to_flower') then
         call collect_real_var (variable_name, '()'
     :                             , p%tt_flag_to_flower, numvals
     :                             , 0.0, 2000.0)

      else if (variable_name .eq. 'tt_flower_to_start_grain') then
         call collect_real_var (variable_name, '()'
     :                             , p%tt_flower_to_start_grain, numvals
     :                             , 0.0, 2000.0)

      else if (variable_name .eq. 'tt_start_to_end_grain') then
         call collect_real_var (variable_name, '()'
     :                             , p%tt_start_to_end_grain, numvals
     :                             , 0.0, 2000.0)

      else if (variable_name .eq. 'tt_end_grain_to_maturity') then
         call collect_real_var (variable_name, '()'
     :                             , p%tt_end_grain_to_maturity, numvals
     :                             , 0.0, 2000.0)

      else if (variable_name .eq. 'tt_maturity_to_ripe') then
         call collect_real_var (variable_name, '()'
     :                             , p%tt_maturity_to_ripe, numvals
     :                             , 0.0, 2000.0)

      else if (variable_name .eq. 'tt_ripe_to_harvest') then
         call collect_real_var (variable_name, '()'
     :                             , p%tt_ripe_to_harvest, numvals
     :                             , 0.0, 2000.0)

      else if (variable_name .eq. 'tt_startgf_to_mat') then
         call collect_real_var (variable_name, '()'
     :                             , p%startgf_to_mat, numvals
     :                             , 0.0, 2000.0)

      else if ((variable_name .eq. 'leaf_app_rate').or.
     :         (variable_name .eq. 'phyllochron')  .or.
     :         (variable_name .eq. 'phint'))    then
         call collect_real_var (variable_name, '()'
     :                             ,c%leaf_app_rate, numvals
     :                             , 0.0, 2000.0)

         c%leaf_app_rate1 = c%leaf_app_rate
         c%leaf_app_rate2 = c%leaf_app_rate

      else if (variable_name .eq. 'leaf_init_rate') then
         call collect_real_var (variable_name, '()'
     :                             ,c%leaf_init_rate, numvals
     :                             , 0.0, 2000.0)

      else
         ! don't know this variable name
         call Message_Unused()
      endif




      call pop_routine (my_name)
      return
      end


*     ================================================================
      subroutine Get_Other_Variables ()
*     ================================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'convert.inc'
      include 'data.pub'
      include 'crp_root.pub'                      
      include 'crp_util.pub'                      
      include 'intrface.pub'                      
      include 'error.pub'                         

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

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)

      !-------------------------------------------------------------------
      ! Co2 and climate change

       if (c%co2switch .ne. 0) then

          if (c%co2level.gt.0.0) then
             g%co2level = c%co2level

          else
             CALL get_real_var_optional(unknown_module, 'co2', '(ppm)',
     :                                            g%co2level, numvals,
     :                                            0.0, 1000.0)

             if (numvals .eq.0) c%co2switch = 0

          endif

       end if


      !use the observed or calculated grain number from other module
      call get_real_var_optional(unknown_module,'obs_grainno_psm', '()'
     :                           , g%obs_grain_no_psm, numvals
     :                           , 0.0, 1e6)


      !-------------------------------------------------------------------
      ! Year and date
       call get_integer_var (unknown_module, 'day', '()',
     :                      g%day_of_year, numvals, 1, 366)
 
      call get_integer_var (unknown_module, 'year', '()'
     :                                    , g%year, numvals
     :                                    , min_year, max_year)
c    :                                    , c%year_lb, c%year_ub)
 
      !-------------------------------------------------------------------
      ! climate
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

      !-------------------------------------------------------------------
      ! canopy
      call get_current_module (module_name)
      call get_real_var_optional (unknown_module
     :                           , 'fr_intc_radn_'//module_name
     :                           , '()'
     :                           , g%fr_intc_radn
     :                           , numvals
     :                           , 0.0
     :                           , 1.0)

      !-------------------------------------------------------------------
      !Soil evap and temperature
      !mjr eo read in to limit demand in crop routines
 
      call get_real_var_optional (unknown_module, 'eo', '(mm)'
     :                                  , g%eo, numvals
     :                                  , 0.0, 20.0)
 
      !Get soil temperature
c      call get_real_var_optional (unknown_module, 'soil_temp', '(oC)'
c     :                                  , soil_temp, numvals
c     :                                  , 0.0, 80.0)


      call get_real_var_optional (unknown_module
     :                                  , 'maxt_soil_surface'
     :                                  , '(oC)'
     :                                  , soil_temp, numvals
     :                                  , 0.0, 80.0)


      if (numvals.eq.0) then
         ! soil temp not supplied
      else
         call crop_store_value (g%day_of_year, g%year,
     .                          g%soil_temp, soil_temp)
      endif
 

c+!!!!!!!! what to do if no waterbalance variables found

      !-------------------------------------------------------------------
      !soil profile
      call get_real_array (unknown_module, 'dlayer', max_layer
     :                                    , '(mm)'
     :                                    , dlayer, numvals
     :                                    , c%dlayer_lb, c%dlayer_ub)

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
      call get_real_array (unknown_module, 'sat_dep', max_layer
     :                                    , '(mm)'
     :                                    , g%sat_dep, numvals
     :                                    , 0.0, 1000.0)

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

      do layer = 1, g%num_layers
         g%NO3gsm(layer) = NO3(layer) * kg2gm /ha2sm
      enddo
 

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      g%NO3(:) = NO3(:)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      call get_real_array_optional (unknown_module, 'no3_min',max_layer
     :                                  ,  '(kg/ha)'
     :                                  , NO3_min, numvals
     :                                  , c%NO3_min_lb, c%NO3_min_ub)
      do layer = 1, g%num_layers
         g%NO3gsm_min(layer) = NO3_min(layer) * kg2gm /ha2sm
      enddo
 


      !cew - added this nh4 pools
       call get_real_array_optional (unknown_module, 'nh4', max_layer
     :                                  ,  '(kg/ha)'
     :                                  , NH4, numvals
     :                                  , c%NH4_lb, c%NH4_ub)
      if (numvals.eq.0) then
            ! we have no N supply - make non-limiting.
         call fill_real_array (NH4, 10000.0, g%num_layers)
      else
      endif
      do layer = 1, g%num_layers
         g%NH4gsm(layer) = NH4(layer) * kg2gm /ha2sm
      end do 
 
      call get_real_array_optional (unknown_module, 'nh4_min',max_layer
     :                                  ,  '(kg/ha)'
     :                                  , NH4_min, numvals
     :                                  , c%NH4_min_lb, c%NH4_min_ub)
      do layer = 1, g%num_layers
         g%NH4gsm_min(layer) = NH4_min(layer) * kg2gm /ha2sm
      end do 


       call get_real_array_optional (unknown_module, 'no3ppm', max_layer
     :                                  ,  'ppm'
     :                                  , NO3, numvals
     :                                  , 0.0, 10000.0)

      do layer = 1, g%num_layers
         g%NO3ppm(layer) = NO3(layer)
      end do 
   
  
       call get_real_array_optional (unknown_module, 'nh4ppm', max_layer
     :                                  ,  '(kg/ha)'
     :                                  , NH4, numvals
     :                                  , 0.0, 10000.0)


      do layer = 1, g%num_layers
         g%NH4ppm(layer) = NH4(layer)
      end do 



      call pop_routine (my_name)
      return
      end



*     ================================================================
      subroutine Set_Other_Variables ()
*     ================================================================
      use CropModModule
      implicit none
      include 'action.inc'
      include 'const.inc'
      include 'convert.inc'
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

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      call Update_Other_Variables()
 
      if (p%uptake_source.eq.'calc') then
c+!!!! perhaps we should get number of layers at init and keep it
         num_layers = count_of_real_vals (g%dlayer, max_layer)


         do layer = 1, num_layers
            dlt_NO3(layer) = g%dlt_NO3gsm(layer) * gm2kg /sm2ha
            dlt_Nh4(layer) = g%dlt_Nh4gsm(layer) * gm2kg /sm2ha
         end do 
 

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

         dlt_no3(:) = - MAX(1E-15, MIN(g%NO3(:), - dlt_NO3(:)))
c        dlt_no3(:) = -  MIN(g%NO3(:), - dlt_NO3(:))

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC



         call set_real_array (unknown_module, 'dlt_no3', '(kg/ha)'
     :                       , dlt_NO3, num_layers)
 
         call set_real_array (unknown_module, 'dlt_nh4', '(kg/ha)'
     :                       , dlt_Nh4, num_layers)

         call set_real_array (unknown_module, 'dlt_sw_dep', '(mm)'
     :                       , g%dlt_sw_dep, num_layers)
 
      else
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine Update_Other_Variables()
*     ===========================================================
      use CropModModule
      implicit none
      include   'convert.inc'
      include 'error.pub'

*+  Purpose
*       Update other module states

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Update_Other_Variables')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      call Daily_Top_Residue (1)
      call Daily_Root_Incorp (1)
 
      call pop_routine (my_name)
      return
      end


* ====================================================================
       subroutine Daily_Top_Residue (option)
* ====================================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'data.pub'
      include 'crp_comm.pub'                      
      include 'error.pub'
      include 'componentinterface.inc'

*+  Sub-Program Arguments
       integer option

*+  Purpose
*     <insert here>

*+  Changes
*     02-05-1997 - huth - Programmed and Specified
*     08-11-2000 - dph  - added EventInterface param to call to crop_top_residue

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'Daily_Top_Residue')

*+  Local Variables
      real       dm_residue            ! dry matter added to residue (g/m^2)
      real       N_residue             ! nitrogen added to residue (g/m^2)

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      if (Option .eq. 1) then
         dm_residue = (sum_real_array (g%dlt_dm_detached, max_part)
     :              - g%dlt_dm_detached(root))
         N_residue = (sum_real_array (g%dlt_N_detached, max_part)
     :             - g%dlt_N_detached(root))
 
         call crop_top_residue (
     .                          c%crop_type
     .                         ,dm_residue
     .                         ,N_residue
     .                         ,EventInterface)
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (myname)
      return
      end



* ====================================================================
       subroutine Daily_Root_Incorp (option)
* ====================================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'crp_comm.pub'
      include 'error.pub'
      include 'componentInterface.inc'

*+  Sub-Program Arguments
       integer option

*+  Purpose
*     <insert here>

*+  Changes
*     02-05-1997 - huth - Programmed and Specified
*     08-11-2000 - dph  - added EventInterface param to call to crop_root_incorp

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'Daily_Root_Incorp')

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      if (Option .eq. 1) then

         call crop_root_incorp (
     .          g%dlt_dm_detached(root)
     :         ,g%dlt_N_detached(root)
     :         ,g%dlayer
     :         ,g%root_length
     :         ,g%root_depth
     :         ,c%crop_type
     :         ,max_layer
     :         ,EventInterface)
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (myname)
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
      implicit none
      include   'const.inc'            ! new_line, lu_scr_sum, blank,
      include   'convert.inc'          ! gm2kg, sm2ha, sm2smm
      include   'CropDefCons.inc'
      include 'data.pub'
      include 'error.pub'

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
      implicit none
      include   'const.inc'            ! new_line, lu_scr_sum
      include   'convert.inc'          ! gm2kg, sm2ha, mm2cm, cmm2cc
      include 'data.pub'
      include 'error.pub'                         

*+  Purpose
*       End crop

*+  Changes
*       290994 sc   specified and programmed

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

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (g%plant_status.ne.status_out) then
         g%plant_status = status_out
         g%current_stage = real (plant_end)
 


         !cnh inserted options here
         call Harvest_Root_Incorp(1) ! 1 uses exp function for rtdp
                                           ! 2 uses calc root length dist
         call Harvest_Top_Residue(1)
 

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
 
 
             ! put stover into surface residue
 
         dm_residue = (sum_real_array (g%dm_green, max_part)
     :              - g%dm_green(root) - g%dm_green(grain))
 
     :              + (sum_real_array (g%dm_senesced, max_part)
     :              - g%dm_senesced(root) - g%dm_senesced(grain))
 
     :              + (sum_real_array (g%dm_dead, max_part)
     :              - g%dm_dead(root) - g%dm_dead(grain))
 
         N_residue = (sum_real_array (g%N_green, max_part)
     :             - g%N_green(root) - g%N_green(grain))
 
     :             + (sum_real_array (g%N_senesced, max_part)
     :             - g%N_senesced(root) - g%N_senesced(grain))
 
     :             + (sum_real_array (g%N_dead, max_part)
     :             - g%N_dead(root) - g%N_dead(grain))
 
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
       subroutine Harvest_Root_Incorp (option)
* ====================================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'crp_comm.pub'
      include 'error.pub'
      include 'componentinterface.inc'

*+  Sub-Program Arguments
       integer option

*+  Purpose
*     <insert here>

*+  Changes
*     02-05-1997 - huth - Programmed and Specified
*     08-11-2000 - dph  - added EventInterface param to call to crop_root_incorp

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'Harvest_Root_Incorp')

*+  Local Variables
      real       dm_root               ! dry matter added to soil (g/m^2)
      real       N_root                ! nitrogen added to soil (g/m^2)

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      if (Option .eq. 1) then

         dm_root = g%dm_green(root)
     :           + g%dm_dead(root)
     :           + g%dm_senesced(root)
 
         N_root  = g%N_green(root)
     :           + g%N_dead(root)
     :           + g%N_senesced(root)
 
         call crop_root_incorp (
     :          dm_root
     :         ,N_root
     :         ,g%dlayer
     :         ,g%root_length
     :         ,g%root_depth
     :         ,c%crop_type
     :         ,max_layer
     :         ,EventInterface)
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (myname)
      return
      end



* ====================================================================
       subroutine Harvest_Top_Residue (option)
* ====================================================================
      use CropModModule
      implicit none
      include 'const.inc'
      include 'data.pub'
      include 'crp_comm.pub'                      
      include 'error.pub'
      include 'componentinterface.inc'

*+  Sub-Program Arguments
       integer option

*+  Purpose
*     <insert here>

*+  Changes
*     02-05-1997 - huth - Programmed and Specified
*     08-11-2000 - dph  - added EventInterface param to call to crop_top_residue

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'Harvest_Top_Residue')

*+  Local Variables
      real       dm_residue            ! dry matter added to residue (g/m^2)
      real       N_residue             ! nitrogen added to residue (g/m^2)

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      if (Option .eq. 1) then
         dm_residue = (sum_real_array (g%dm_green, max_part)
     :              - g%dm_green(root) - g%dm_green(grain))
 
     :              + (sum_real_array (g%dm_senesced, max_part)
     :              - g%dm_senesced(root) - g%dm_senesced(grain))
 
     :              + (sum_real_array (g%dm_dead, max_part)
     :              - g%dm_dead(root) - g%dm_dead(grain))
 
         N_residue = (sum_real_array (g%N_green, max_part)
     :             - g%N_green(root) - g%N_green(grain))
 
     :             + (sum_real_array (g%N_senesced, max_part)
     :             - g%N_senesced(root) - g%N_senesced(grain))
 
     :             + (sum_real_array (g%N_dead, max_part)
     :             - g%N_dead(root) - g%N_dead(grain))
 
         call crop_top_residue (
     .                          c%crop_type
     .                         ,dm_residue
     .                         ,N_residue
     .                         ,EventInterface)
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
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
      implicit none
      include   'convert.inc'          ! gm2kg, sm2ha, mm2cm, cmm2cc
      include   'CropDefCons.inc'
      include 'data.pub'
      include 'error.pub'

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
         g_plant_status = status_dead
 
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
      implicit none
      include 'error.pub'

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
      implicit none
      include   'convert.inc'
      include 'science.pub'
      include 'data.pub'                          
      include 'error.pub'                         

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



      ! transfer plant leaf area
      g%lai = g%lai + g%dlt_lai - g%dlt_slai
      g%slai = g%slai + g%dlt_slai - g%dlt_slai_detached
 
      dlt_lai_dead  = g%lai  * dying_fract
      dlt_slai_dead = g%slai * dying_fract
 
      g%lai = g%lai - dlt_lai_dead
      g%slai = g%slai - dlt_slai_dead
      g%tlai_dead = g%tlai_dead + dlt_lai_dead + dlt_slai_dead
     :            - g%dlt_tlai_dead_detached
 

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


c        PRINT *,'nconc_co2modifier =', co2_modifier


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
      implicit none
      include   'CropDefCons.inc'
      include 'data.pub'
      include 'error.pub'                         

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
 
      call bound_check_real_var
     :           (sum_real_array (g_leaf_no, max_stage)
     :          , 0.0
     :          , real (max_leaf)
     :          , 'leaf_no')
 
      call bound_check_real_var
     :           (sum_real_array (g_leaf_no_dead, max_stage)
     :          , 0.0
     :          , real (max_leaf)
     :          , 'leaf_no_dead')
 
      call bound_check_real_var
     :           (g_root_depth
     :          , 0.0
     :          , sum_real_array (g_dlayer, max_layer)
     :          , 'root_depth')
 
      call bound_check_real_var
     :           (g_grain_no
     :          , 0.0
     :          , 200000.0 !c_head_grain_no_max_ub   !p_head_grain_no_max * g_plants  -ew
     :          , 'grain_no')
 
      call bound_check_real_var
     :           (g_current_stage
     :          , 0.0
     :          , real (max_stage)
     :          , 'current_stage')
 
      call bound_check_real_var
     :           (sum_real_array (g_phase_tt, max_stage)
     :          , 0.0
     :          , 1000000.0
     :          , 'phase_tt')
 
      call bound_check_real_var
     :           (sum_real_array (g_days_tot, max_stage)
     :          , 0.0
     :          , 40000.0
     :          , 'days_tot')
 
      call bound_check_real_var
     :           (sum_real_array (g_tt_tot, max_stage)
     :          , 0.0
     :          , 40000.0
     :          , 'tt_tot')
 
      call bound_check_real_var
     :           (g_plants
     :          , 0.0
     :          , 10000.0
     :          , 'plants')
 
      call bound_check_real_var
     :           (g_canopy_height
     :          , 0.0
     :          , c_height_max
     :          , 'canopy_height')



      call bound_check_real_var
     :           (g_lai
     :          , 0.0
     :          , 30.0 - g_slai - g_tlai_dead
     :          , 'lai')
 
      call bound_check_real_var
     :           (g_slai
     :          , 0.0
     :          , 30.0 - g_lai - g_tlai_dead
     :          , 'slai')
 
      call bound_check_real_var
     :           (g_tlai_dead
     :          , 0.0
     :          , 30.0 - g_slai - g_lai
     :          , 'tlai_dead')
 
      call bound_check_real_var
     :           (g_cover_green
     :          , 0.0
     :          , 1.0
     :          , 'cover_green')
 
      call bound_check_real_var
     :           (g_cover_sen
     :          , 0.0
     :          , 1.0
     :          , 'cover_sen')
 
      call bound_check_real_var
     :           (g_cover_dead
     :          , 0.0
     :          , 1.0
     :          , 'cover_dead')
 
      call bound_check_real_var
     :           (sum_real_array (g_leaf_area, max_leaf)
     :          , 0.0
     :          , 10000000.0
     :          , 'leaf_area')
 
      call bound_check_real_var
     :           (sum_real_array (g_heat_stress_tt, max_stage)
     :          , 0.0
     :          , 1000000.0
     :          , 'heat_stress_tt')
      call bound_check_real_var
     :           (sum_real_array (g_dm_stress_max, max_stage)
     :          , 0.0
     :          , 1000000.0
     :          , 'dm_stress_max')
 
      call bound_check_real_var
     :           (sum_real_array (g_N_conc_crit, max_part)
     :          , sum_real_array (g_N_conc_min, max_part)
     :          , sum_real_array (g_N_conc_max, max_part)
     :          , 'N_conc_crit')
 
      call bound_check_real_var
     :           (sum_real_array (g_N_conc_max, max_part)
     :          , sum_real_array (g_N_conc_crit, max_part)
     :          , 1.0
     :          , 'N_conc_max')
 
      call bound_check_real_var
     :           (sum_real_array (g_N_conc_min, max_part)
     :          , 0.0
     :          , sum_real_array (g_N_conc_crit, max_part)
     :          , 'N_conc_min')
 
      call bound_check_real_var
     :           (sum_real_array (g_N_dead, max_part)
     :          , 0.0
     :          , 10000.0 - sum_real_array (g_N_green, max_part)
     :                    - sum_real_array (g_N_senesced, max_part)
     :          , 'N_dead')
 
      call bound_check_real_var
     :           (sum_real_array (g_N_green, max_part)
     :          , 0.0
     :          , 10000.0 - sum_real_array (g_N_dead, max_part)
     :                    - sum_real_array (g_N_senesced, max_part)
     :          , 'N_green')
 
      call bound_check_real_var
     :           (sum_real_array (g_N_senesced, max_part)
     :          , 0.0
     :          , 10000.0 - sum_real_array (g_N_green, max_part)
     :                    - sum_real_array (g_N_dead, max_part)
     :          , 'N_senesced')
 
      call bound_check_real_var
     :           (sum_real_array (g_dm_dead, max_part)
     :          , 0.0
     :          , 10000.0 - sum_real_array (g_dm_green, max_part)
     :                    - sum_real_array (g_dm_senesced, max_part)
     :          , 'dm_dead')
 
      call bound_check_real_var
     :           (sum_real_array (g_dm_green, max_part)
     :          , 0.0
     :          , 10000.0 - sum_real_array (g_dm_dead, max_part)
     :                    - sum_real_array (g_dm_senesced, max_part)
     :          , 'dm_green')
 
      call bound_check_real_var
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
      implicit none
      include   'CropDefCons.inc'
      include 'science.pub'
      include 'data.pub'                          
      include 'error.pub'                         

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
      implicit none
      include 'const.inc'            ! new_line,  blank,
      include 'convert.inc'
      include 'CropDefCons.inc'
      include 'science.pub'
      include 'data.pub'                          
      include 'error.pub'

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
      implicit none
      include   'CropDefCons.inc'
      include 'science.pub'
      include 'data.pub'                          
      include 'error.pub'
      include 'crp_phen.pub'

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
      implicit none
      include 'const.inc'
      include 'crp_cnpy.pub'
      include 'crp_nitn.pub'                      
      include 'crp_biom.pub'                      
      include 'error.pub'                         

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
         call Fatal_error (ERR_internal, 'Invalid template option')
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
      implicit none
      include 'data.pub'
      include 'error.pub'

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
      implicit none
      include 'data.pub'
      include 'error.pub'
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
      implicit none
      include 'data.pub'
      include 'error.pub'                         

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
      g%year                = 0
      g%day_of_year         = 0
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
      g%fr_intc_radn        = 0.0
      g%latitude            = 0.0
      g%radn                = 0.0
      g%mint                = 0.0
      g%maxt                = 0.0

      CALL fill_real_array(g%soil_temp,0.0, 366)

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

      call fill_real_array(g%NO3gsm_diffn_pot,  0.0,max_layer)
      call fill_real_array(g%NO3gsm_mflow_avail,0.0,max_layer)
      call fill_real_array(g%NO3gsm,            0.0,max_layer)
      call fill_real_array(g%NO3gsm_min,        0.0,max_layer)


      g%no3_diffn_const = 0.0






      !root_profile
      g%num_layers =0

      g%sw_demand             =0.0
      g%sw_supply_sum         =0.0
      g%sw_supply_demand_ratio=0.0

      call fill_real_array(g%dlayer,       0.0,max_layer)
      call fill_real_array(g%dul_dep,      0.0,max_layer)
      call fill_real_array(g%sw_dep,       0.0,max_layer)
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
      call fill_real_array(g%NH4gsm,               0.0, max_layer)
      call fill_real_array(g%NH4gsm_min,           0.0, max_layer)
      call fill_real_array(g%NH4gsm_diffn_pot,     0.0, max_layer)
      call fill_real_array(g%NH4gsm_mflow_avail,   0.0, max_layer)
      call fill_real_array(g%NO3ppm,               0.0, max_layer)
      call fill_real_array(g%NH4ppm,               0.0, max_layer)
      call fill_real_array(g%sat_dep,              0.0, max_layer)
      call fill_real_array(g%pot_extract_NO3gsm,   0.0, max_layer)
      call fill_real_array(g%pot_extract_NH4gsm,   0.0, max_layer)

      g%dlt_n_uptake_stover=0.0


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



      if (param_init) then

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


      call fill_real_array(c%x_temp_vern,   0.0, max_table)
      call fill_real_array(c%y_vern_rate,   0.0, max_table)
      c%num_temp_vern      =0


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
      c%n_supply_preference = " "

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
      implicit none
      include 'data.pub'
      include 'error.pub'                         

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
      g%dlt_plants       =0.0
      g%dlt_root_depth   =0.0
      g%dlt_plants_all   =0.0
      g%dlt_plants_temp  =0.0
      g%dlt_plants_water =0.0
      g%dlt_plants_barren=0.0
      g%dlt_plants_dead  =0.0

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

      call fill_real_array(g%dlt_NO3gsm,         0.0,max_layer)


      call fill_real_array(g%dlt_NO3gsm_massflow,         0.0,max_layer)
      call fill_real_array(g%dlt_NO3gsm_diffusion,        0.0,max_layer)



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
      call fill_real_array(g%dlt_NH4gsm,           0.0, max_layer)




       g%dlt_cumvd          =0.0




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
      implicit none
      include 'science.pub'                       
      include 'error.pub'                         

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
      implicit none
      include 'science.pub'                       
      include 'error.pub'                         

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
      implicit none
      include 'science.pub'                       
      include 'error.pub'                         

*+  Sub-Program Arguments
      real g_row_spacing
      real c_x_row_spacing(*)
      real c_y_extinct_coef(*)
      real c_num_row_spacing
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


