


* ====================================================================
       subroutine PlantP_zero_variables ()
* ====================================================================
      Use infrastructure
      implicit none

*+  Purpose
*     Set all variables in this module to zero.

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'PlantP_zero_variables')

*+  Local Variables

*- Implementation Section ----------------------------------
      call push_routine (myname)

      ! Parameters
      ! ==========

c      p%crop_type = ' '       ! Characters


      ! Globals
      ! =======

      g%phosphorus_aware    = .false.

      g%growth_Stage = 0.0      ! Reals
      g%part_p_green(:) = 0.0
      g%dlt_part_p_green(:) = 0.0
      g%part_p_sen(:) = 0.0
      g%part_p_dead(:) = 0.0
      g%dlt_part_p_dead(:) = 0.0
      g%dlt_part_p_sen(:) = 0.0
      g%dlt_part_p_det(:) = 0.0
      g%dlt_part_p_retrans(:) = 0.0

      g%part_demand(:) = 0.0

      g%part_names(:) = ' '     ! Characters

      g%num_parts = 0           ! Integers

      ! Constants
      ! =========

      c%stress_determinants(:) = ' '      ! Characters

      c%x_p_stage_code(:) = 0.0           ! Reals
      c%y_p_conc_max (:,:) = 0.0
      c%y_p_conc_min (:,:) = 0.0
      c%y_p_conc_sen (:,:) = 0.0
      c%pfact_photo_slope = 0.0
      c%pfact_expansion_slope = 0.0
      c%pfact_pheno_slope = 0.0

      c%num_x_p_stage_code = 0             ! Integers

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine PlantP_get_other_variables ()
* ====================================================================
      Use infrastructure
      implicit none

*+  Purpose
*      Get the values of variables from other modules

*+  Changes
*     <insert here>


*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'PlantP_get_other_variables')

*+  Local Variables
       integer numvals              ! number of values returned

*- Implementation Section ----------------------------------
      call push_routine (myname)

c      call Get_real_array (
c     :      unknown_module  ! Module that responds (Not Used)
c     :     ,'dlayer'        ! Variable Name
c     :     ,max_layer       ! Array Size
c     :     ,'(mm)'          ! Units                (Not Used)
c     :     ,g%dlayer        ! Variable
c     :     ,numvals         ! Number of values returned
c     :     ,0.0             ! Lower Limit for bound checking
c     :     ,1000.)          ! Upper Limit for bound checking

      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       logical function PlantP_Send_my_variable (Variable_name)
* ====================================================================

      Use infrastructure
      implicit none

*+  Sub-Program Arguments
       character Variable_name*(*)     ! (INPUT) Variable name to search for

*+  Purpose
*      Return the value of one of our variables to caller

*+  Changes

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'PlantP_send_my_variable')

*+  Local Variables

*- Implementation Section ----------------------------------
      call push_routine (myname)

      PlantP_Send_my_variable = .true.

      if (variable_name .eq. 'p_green') then
          call respond2get_real_array (
     :               variable_name     ! variable name
     :              ,'(g/m2)'          ! variable units
     :              ,g%part_p_green          ! variable
     :              ,g%num_parts)      ! Array size
      elseif (variable_name .eq. 'p_sen') then
          call respond2get_real_array (
     :               variable_name     ! variable name
     :              ,'(g/m2)'          ! variable units
     :              ,g%part_p_sen          ! variable
     :              ,g%num_parts)      ! Array size
      elseif (variable_name .eq. 'p_dead') then
          call respond2get_real_array (
     :               variable_name     ! variable name
     :              ,'(g/m2)'          ! variable units
     :              ,g%part_p_dead          ! variable
     :              ,g%num_parts)      ! Array size

      elseif (variable_name .eq. 'p_demand') then
          call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'(kg/ha)'         ! variable units
     :              ,sum(g%part_demand)*gm2kg/sm2ha) ! variable
      elseif (variable_name .eq. 'pfact_photo') then
          call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'()'              ! variable units
     :              ,g%plantPfact_photo) ! variable
      elseif (variable_name .eq. 'pfact_pheno') then
          call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'()'              ! variable units
     :              ,g%plantPfact_pheno) ! variable
      elseif (variable_name .eq. 'pfact_expansion') then
          call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'()'              ! variable units
     :              ,g%plantPfact_expansion) ! variable

      else
         PlantP_Send_my_variable = .false.
      endif

      call pop_routine (myname)
      return
      end function


*     ===========================================================
      subroutine PlantP_read_param ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*       Read all module parameters.

*+  Changes
*     <insert here>

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'PlantP_read_param')
*
      character section_name*(*)
      parameter (section_name = 'parameters')

*+  Local Variables
      integer    numvals               ! number of values read

*- Implementation Section ----------------------------------

      call push_routine (myname)


c      call read_char_var (
c     :           section_name         ! Section header
c     :          ,'crop_type'          ! Keyword
c     :          ,'()'                 ! Units
c     :          ,p%crop_type          ! Array
c     :          ,numvals)             ! Number of values returned

c      call read_real_array (
c     :           section_name         ! Section header
c     :          ,'rlv'                ! Keyword
c     :          ,max_layer            ! array size
c     :          ,'(mm/cmm)'           ! Units
c     :          ,p%rlv                ! Array
c     :          ,numvals              ! Number of values returned
c     :          ,0.0                  ! Lower Limit for bound check
c     :          ,1.0)                 ! Upper Limit for bound check

      call pop_routine  (myname)
      return
      end subroutine


*     ===========================================================
      subroutine PlantP_read_constants ()
*     ===========================================================

      Use infrastructure
      implicit none


*+  Sub-Program Arguments


*+  Purpose
*       Read all module constants.

*+  Changes
*

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'PlantP_read_constants')

*+  Local Variables
      integer    numvals               ! number of values read
      character  search_order(max_table)*32 ! sections to search
      integer    num_sections          ! number of sections to search
      integer    part
      real       temp(max_table)
      character  keyword*32

*- Implementation Section ----------------------------------

      call push_routine (myname)

      call read_char_array ('constants'
     :                     , 'stress_determinants'
     :                     , max_parts, '()'
     :                     , c%stress_determinants
     :                     , numvals)

      call read_char_array ('constants'
     :                     , 'yield_parts'
     :                     , max_parts, '()'
     :                     , c%yield_parts
     :                     , numvals)

      call read_char_array ('constants'
     :                     , 'retrans_parts'
     :                     , max_parts, '()'
     :                     , c%retrans_parts
     :                     , numvals)

      call read_real_var  ('constants'
     :                     , 'pfact_photo_slope'
     :                     , '()'
     :                     , c%pfact_photo_slope
     :                     , numvals
     :                     , 1.0
     :                     , 100.0)

      call read_real_var  ('constants'
     :                     , 'pfact_expansion_slope'
     :                     , '()'
     :                     , c%pfact_expansion_slope
     :                     , numvals
     :                     , 1.0
     :                     , 100.0)

      call read_real_var  ('constants'
     :                     , 'pfact_pheno_slope'
     :                     , '()'
     :                     , c%pfact_pheno_slope
     :                     , numvals
     :                     , 1.0
     :                     , 100.0)

      call read_real_var  ('constants'
     :                     , 'pfact_grain_slope'
     :                     , '()'
     :                     , c%pfact_grain_slope
     :                     , numvals
     :                     , 1.0
     :                     , 100.0)

      call read_real_array ('constants'
     :                     , 'x_p_stage_code'
     :                     , max_table, '()'
     :                     , c%x_p_stage_code
     :                     , c%num_x_p_stage_code
     :                     , 0.0
     :                     , 100.0)

      do 100 part = 1, g%num_parts

        keyword = 'y_p_conc_max_'//trim(g%part_names(part))

      call read_real_array ('constants'
     :                     , keyword
     :                     , max_table, '(g/g)'
     :                     , temp
     :                     , c%num_x_p_stage_code
     :                     , 0.0
     :                     , 1.0)
         c%y_p_conc_max(part,1:c%num_x_p_stage_code)
     :       = temp(1:c%num_x_p_stage_code)

        keyword = 'y_p_conc_sen_'//trim(g%part_names(part))

      call read_real_array ('constants'
     :                     , keyword
     :                     , max_table, '(g/g)'
     :                     , temp
     :                     , c%num_x_p_stage_code
     :                     , 0.0
     :                     , 1.0)
         c%y_p_conc_sen(part,1:c%num_x_p_stage_code)
     :       = temp(1:c%num_x_p_stage_code)

        keyword = 'y_p_conc_min_'//trim(g%part_names(part))

      call read_real_array ('constants'
     :                     , keyword
     :                     , max_table, '(g/g)'
     :                     , temp
     :                     , c%num_x_p_stage_code
     :                     , 0.0
     :                     , 1.0)
         c%y_p_conc_min(part,1:c%num_x_p_stage_code)
     :       = temp(1:c%num_x_p_stage_code)


        keyword = 'p_conc_init_'//trim(g%part_names(part))

      call read_real_var   ('constants'
     :                     , keyword
     :                     , '(g/g)'
     :                     , c%p_conc_init(part)
     :                     , numvals
     :                     , 0.0
     :                     , 1.0)

  100 continue

      call pop_routine  (myname)
      return
      end subroutine



* ====================================================================
       subroutine PlantP_prepare (growth_stage
     :                           ,part_wts
     :                           ,dlt_dm_pot)
* ====================================================================
      Use infrastructure
       implicit none

*+  Sub-Program Arguments
      real part_wts(*)
      real growth_stage
      real dlt_dm_pot

*+  Purpose
*     <insert here>

*+  Changes
*

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'PlantP_prepare')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call zero_daily_variables()

      call PlantP_get_other_variables ()
      call PlantP_demand(growth_stage, part_wts, dlt_dm_pot)
      call PlantP_Stress(growth_stage, part_wts)

      call pop_routine (myname)
      return
      end subroutine


* ====================================================================
       subroutine PlantP_process (growth_stage,dm_green,dm_senesced
     :                           ,dlt_dm_senesced
     :                           ,dlt_dm_detached)
* ====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
       real growth_stage
       real dm_green(*)
       real dm_senesced(*)
       real dlt_dm_senesced(*)
       real dlt_dm_detached(*)

*+  Purpose
*     <insert here>

*+  Changes
*

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'PlantP_process')

*+  Local Variables
      real layered_p_uptake(max_layer)
      integer numvals
      integer part
      real p_uptake
      real total_demand


*- Implementation Section ----------------------------------
      call push_routine (myname)
      call PlantP_init_pools(dm_green)

      call PlantP_partition()
      call PlantP_senescence(growth_stage
     :                      ,dm_green
     :                      ,dlt_dm_senesced)
      call PlantP_detachment(dm_senesced,dlt_dm_detached)

      g%part_p_green(:) = g%part_p_green(:)
     :                  + g%dlt_part_p_green(:)
     :                  - g%dlt_part_p_sen(:)

      g%part_p_sen(:) = g%part_p_sen(:)
     :                + g%dlt_part_p_sen(:)
     :                - g%dlt_part_p_det(:)


      ! Now do any retranslocation to try and keep pools
      ! at target concentrations.

      call PlantP_retrans(growth_stage,dm_green)

      g%part_p_green(:) = g%part_p_green(:)
     :                  + g%dlt_part_p_retrans(:)

      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       subroutine PlantP_partition ()
* ====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments

*+  Purpose
*     <insert here>

*+  Changes
*


*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'PlantP_partition')

*+  Local Variables
      real layered_p_uptake(max_layer)
      integer numvals
      integer part
      real p_uptake
      real total_demand
      character keyword*32


*- Implementation Section ----------------------------------
      call push_routine (myname)

      call fill_real_array (layered_p_uptake,0.0,max_layer)

      keyword = 'uptake_p_'//trim(g%crop_type)

      call get_real_array_Optional
     :                        (unknown_module
     :                       ,keyword
     :                       ,max_layer
     :                       ,'()'
     :                       ,layered_p_uptake
     :                       ,numvals
     :                       ,0.0
     :                       ,100.)
      if (numvals.gt.0) then
         p_uptake = sum(layered_p_uptake(1:numvals))
     :            * kg2gm/ha2sm

      else
         p_uptake = sum(g%part_demand(1:g%num_parts))

      endif

      total_demand = sum(g%part_demand(1:g%num_parts))

      do 100 part=1,g%num_parts
         g%dlt_part_p_green(part) = p_uptake
     :                            * divide(g%part_demand(part)
     :                                     ,total_demand
     :                                     ,0.0)
  100 continue

      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       subroutine PlantP_senescence (growth_stage, part_wts
     :                              , dlt_part_sen)
* ====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real part_wts(*)
      real dlt_part_sen(*)
      real growth_stage


*+  Purpose
*     <insert here>

*+  Changes
*

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'PlantP_senescence')

*+  Local Variables
      integer part
      real p_conc_green
      real p_conc_sen

*- Implementation Section ----------------------------------
      call push_routine (myname)

      g%dlt_part_p_sen(:) = 0.0

      do 100 part=1,g%num_parts
         p_conc_green = divide (g%part_p_green(part)
     :                         ,part_wts(part)
     :                         ,0.0)
         p_conc_sen = linear_interp_real
     :                   (growth_stage
     :                   ,c%x_p_stage_code
     :                   ,c%y_p_conc_sen(part,1:c%num_x_p_stage_code)
     :                   ,c%num_x_p_stage_code)

         g%dlt_part_p_sen(part) = min(p_conc_green,p_conc_sen)
     :                          * dlt_part_sen(part)

  100 continue

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine PlantP_detachment (dm_senesced, dlt_dm_detached)
* ====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real dm_senesced(*)
      real dlt_dm_detached(*)

*+  Purpose
*     <insert here>

*+  Changes
*

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'PlantP_detachment')

*+  Local Variables
      integer part
      real    sen_detach_frac

*- Implementation Section ----------------------------------
      call push_routine (myname)

      g%dlt_part_p_det(:) = 0.0

      do 100 part=1,g%num_parts
         sen_detach_frac = divide(dlt_dm_detached(part)
     :                           ,dm_senesced(part)
     :                           ,0.0)

         g%dlt_part_p_det(part) = g%part_p_sen(part)
     :                            * sen_detach_frac
  100 continue

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine PlantP_zero_daily_variables ()
* ====================================================================
      Use infrastructure
      implicit none

*+  Purpose
*     <insert here>

*+  Changes
*

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'PlantP_zero_daily_variables')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      g%dlt_part_p_green(:) = 0.0
      g%part_demand(:) = 0.0
      g%plantPfact_photo = 0.0
      g%plantPfact_expansion = 0.0
      g%plantPfact_pheno = 0.0
      g%plantPfact_grain = 0.0

      call pop_routine (myname)
      return
      end subroutine


* ====================================================================
       subroutine PlantP_Create ()
* ====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments

*+  Purpose
*      Create PlantP module

*+  Changes
*     <insert here>

*+  Calls

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'PlantP_create')

*+  Local Variables

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call PlantP_zero_variables ()

      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       subroutine PlantP_Init (crop_type, part_names, num_parts)
* ====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      character crop_type*(*)
      character part_names(*)*(*)
      Integer num_parts

*+  Purpose
*      Initialise PlantP module

*+  Changes
*     <insert here>

*+  Calls

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'PlantP_init')

*+  Local Variables

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call PlantP_get_other_variables ()

      call PlantP_set_phosphorus_aware ()

      if (g%phosphorus_aware) then
         g%crop_type = crop_type

         call PlantP_Set_Up_Parts (part_names, num_parts)

         call PlantP_read_param ()

         call PlantP_read_constants ()
      else
      endif

      call pop_routine (myname)
      return
      end subroutine


* ====================================================================
       subroutine PlantP_Set_Up_Parts (part_names, num_parts)
* ====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      character part_names(*)*(*)
      integer   num_parts

*+  Purpose
*      Initialise PlantP parts arrays

*+  Changes
*     <insert here>

*+  Calls

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'PlantP_set_up_parts')

*+  Local Variables

*- Implementation Section ----------------------------------
      call push_routine (myname)

      g%num_parts = num_parts
      g%part_names(1:num_parts) = part_names(1:num_parts)

      ! Put further setup stuff here.....

      call pop_routine (myname)
      return
      end subroutine

*     ================================================================
      subroutine PlantP_set_phosphorus_aware ()
*     ================================================================
      Use infrastructure
      implicit none

*+  Purpose
*      Check that soil phosphorus is in system

*+  Mission statement
*     Check the phosphorus awareness of the system

*+  Changes
*     121198 jngh programmed
*     170599 jngh added include 'write.pub'

*+  Constant Values
      character*(*) my_name
      parameter (my_name = 'PlantP_set_phosphorus_aware')

*+  Local Variables
      integer   numvals
      real labile_p(max_layer)      ! labile p from soil phosphorous

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call Get_real_array_optional(
     :      unknown_module  ! Module that responds (Not Used)
     :    , 'labile_p'      ! Variable Name
     :    , max_layer       ! size of array
     :    , '(kg/ha)'       ! Units                (Not Used)
     :    , labile_p       ! Variable
     :    , numvals         ! Number of values returned
     :    , 1.0             ! Lower Limit for bound checking
     :    , 1000.0)         ! Upper Limit for bound checking

      if(numvals .gt. 0) then
         !module is p aware
         g%phosphorus_aware = .true.
         call write_string (
     :                  new_line
     :                  //'    - Module is set Phosphorous aware')
      else
         g%phosphorus_aware = .false.

      endif

      call pop_routine (my_name)
      return
      end subroutine


* ====================================================================
       subroutine PlantP_demand (growth_stage, parts_wt, dlt_dm_pot)
* ====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real parts_wt(*)
      real growth_stage
      real dlt_dm_pot

*+  Purpose
*      Calculate plant P demands

*+  Changes
*     <insert here>

*+  Calls

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'PlantP_demand')

*+  Local Variables
      real deficit
      integer part
      real p_conc_max
      integer counter
      integer num_yield_parts
      real    rel_growth_rate

*- Implementation Section ----------------------------------
      call push_routine (myname)

      g%Part_demand(:) = 0.0
      g%growth_stage = growth_stage
      num_yield_parts = count_of_char_vals(c%yield_parts
     :                                     ,max_parts)

      rel_growth_rate = divide(dlt_dm_pot
     :                        ,sum(parts_wt(1:g%num_parts))
     :                        ,0.0)

      do 100 part = 1, g%num_parts

         ! Find if this part is a yield part

         counter = position_in_char_array
     :             (g%part_names(part)
     :             ,c%yield_parts
     :             ,num_yield_parts)


         if (counter .eq. 0) then
            ! Not a yield part - therefore it contributes to demand

            p_conc_max = linear_interp_real
     :                   (g%growth_stage
     :                   ,c%x_p_stage_code
     :                   ,c%y_p_conc_max(part,1:c%num_x_p_stage_code)
     :                   ,c%num_x_p_stage_code)

            ! scale up to include potential new growth
            ! assuming partitioning today similar to current
            ! plant form - a rough approximation

            deficit = p_conc_max
     :                   * parts_wt(part)
     :                   * (1. + rel_growth_rate)
     :              - g%part_p_green(part)

            g%Part_demand(part) = l_bound(deficit, 0.0)

         else
            ! A yield part - does not contribute to soil demand
            g%Part_demand(part) = 0.0
         endif

  100 continue


      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       real function PlantP_Pfact_photo ()
* ====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments

*+  Purpose
*      Provide value of P factor for photosynthesis

*+  Changes
*     <insert here>

*+  Calls

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'PlantP_Pfact_photo')

*+  Local Variables

*- Implementation Section ----------------------------------
      call push_routine (myname)

      if (g%phosphorus_aware) then
         PlantP_Pfact_photo = g%plantPfact_photo
      else
         PlantP_Pfact_photo = 1.0
      endif

      call pop_routine (myname)
      return
      end function

* ====================================================================
       real function PlantP_Pfact_grain ()
* ====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments

*+  Purpose
*      Provide value of P factor for grain filling

*+  Changes
*     <insert here>

*+  Calls

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'PlantP_Pfact_grain')

*+  Local Variables

*- Implementation Section ----------------------------------
      call push_routine (myname)

      if (g%phosphorus_aware) then
         PlantP_Pfact_grain = g%plantPfact_grain
      else
         PlantP_Pfact_grain = 1.0
      endif

      call pop_routine (myname)
      return
      end function
* ====================================================================
       real function PlantP_Pfact_expansion ()
* ====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments

*+  Purpose
*      Provide value of P factor for cell expansion

*+  Changes
*     <insert here>

*+  Calls

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'PlantP_Pfact_expansion')

*+  Local Variables

*- Implementation Section ----------------------------------
      call push_routine (myname)

      if (g%phosphorus_aware) then
         PlantP_Pfact_expansion = g%plantPfact_expansion
      else
         PlantP_Pfact_expansion = 1.0
      endif

      call pop_routine (myname)
      return
      end function
* ====================================================================
       real function PlantP_Pfact_pheno ()
* ====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments

*+  Purpose
*      Provide value of P factor for phenology

*+  Changes
*     <insert here>

*+  Calls

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'PlantP_Pfact_pheno')

*+  Local Variables

*- Implementation Section ----------------------------------
      call push_routine (myname)

      if (g%phosphorus_aware) then
         PlantP_Pfact_pheno = g%plantPfact_pheno
      else
         PlantP_Pfact_pheno = 1.0
      endif

      call pop_routine (myname)
      return
      end function

* ====================================================================
       subroutine PlantP_add_residue (chop_fr_green, chop_fr_sen
     :                        ,chop_fr_dead, fraction_to_residue
     :                        )
* ====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real chop_fr_green(*)
      real chop_fr_sen(*)
      real chop_fr_dead(*)
      real fraction_to_residue(*)
!      integer EventInterface

*+  Purpose
*      Send P to soil or residues when plants are 'chopped'

*+  Changes
*     <insert here>

*+  Calls

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'PlantP_add_residue')

*+  Local Variables
      real    dlt_residue_p

*- Implementation Section ----------------------------------
      call push_routine (myname)

      dlt_residue_p = sum(chop_fr_green(1:g%num_parts)
     :                      * g%part_p_green(1:g%num_parts)
     :                      * fraction_to_residue(1:g%num_parts)
     :                   +
     :                    chop_fr_sen(1:g%num_parts)
     :                      * g%part_p_sen(1:g%num_parts)
     :                      * fraction_to_residue(1:g%num_parts)
     :                   +
     :                    chop_fr_dead(1:g%num_parts)
     :                      * g%part_p_dead(1:g%num_parts)
     :                      * fraction_to_residue(1:g%num_parts)
     :                    )
     :              * gm2kg/sm2ha

      g%part_p_green(1:g%num_parts) = g%part_p_green(1:g%num_parts)
     :                              * (1.-chop_fr_green(1:g%num_parts))
      g%part_p_sen(1:g%num_parts) = g%part_p_sen(1:g%num_parts)
     :                              * (1.-chop_fr_sen(1:g%num_parts))
      g%part_p_dead(1:g%num_parts) = g%part_p_dead(1:g%num_parts)
     :                              * (1.-chop_fr_dead(1:g%num_parts))

      call New_postbox ()

      call post_real_var ('dlt_residue_p'
     :                   ,'(kg/ha)'
     :                   , dlt_residue_P)

       call event_send(ACTION_add_residue_p)

!      call EI_BroadcastAction     (EventInterface
!     :                            ,ACTION_add_residue_p
!     :                            ,Blank
!     :                            )

      call Delete_postbox ()

      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       subroutine PlantP_residue_chopped (chop_fr_green, chop_fr_sen
     :                        ,chop_fr_dead, fraction_to_residue
     :                        , dlt_residue_p, dlt_dm_P
     :                        )
* ====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real chop_fr_green(*)
      real chop_fr_sen(*)
      real chop_fr_dead(*)
      real fraction_to_residue(*)
      real dlt_dm_P(*)
      real    dlt_residue_p
!      integer EventInterface

*+  Purpose
*      Send P to soil or residues when plants are 'chopped'

*+  Changes
*     <insert here>

*+  Calls

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'PlantP_residue_chopped')

*+  Local Variables

*- Implementation Section ----------------------------------
      call push_routine (myname)

      if (g%phosphorus_aware) then
         dlt_dm_P(1:g%num_parts) = chop_fr_green(1:g%num_parts)
     :                      * g%part_p_green(1:g%num_parts)
     :                      * fraction_to_residue(1:g%num_parts)
     :                   +
     :                    chop_fr_sen(1:g%num_parts)
     :                      * g%part_p_sen(1:g%num_parts)
     :                      * fraction_to_residue(1:g%num_parts)
     :                   +
     :                    chop_fr_dead(1:g%num_parts)
     :                      * g%part_p_dead(1:g%num_parts)
     :                      * fraction_to_residue(1:g%num_parts)
     :              * gm2kg/sm2ha

         dlt_residue_p = sum(dlt_dm_P(1:g%num_parts))

         g%part_p_green(1:g%num_parts) = g%part_p_green(1:g%num_parts)
     :                               * (1.-chop_fr_green(1:g%num_parts))
         g%part_p_sen(1:g%num_parts) = g%part_p_sen(1:g%num_parts)
     :                               * (1.-chop_fr_sen(1:g%num_parts))
         g%part_p_dead(1:g%num_parts) = g%part_p_dead(1:g%num_parts)
     :                                * (1.-chop_fr_dead(1:g%num_parts))

      else
         dlt_dm_P(1:g%num_parts)   = 0.0
         dlt_residue_p = 0.0
      endif

      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       subroutine PlantP_incorp_fom (incorp_fr_green, incorp_fr_sen
     :                        ,incorp_fr_dead, dlayer
     :                        ,root_length,root_depth
     :                        , p_incorporated
     :                        )
* ====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real incorp_fr_green(*)
      real incorp_fr_sen(*)
      real incorp_fr_dead(*)
      real dlayer(*)
      real root_length(*)
      real root_depth
!      integer EventInterface

*+  Purpose
*      Send P to soil or residues when plants are 'chopped'

*+  Changes
*     <insert here>

*+  Calls

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'PlantP_incorp_fom')

*+  Local Variables
      real    dlt_fom_p(max_layer)
      real    p_incorporated
      integer num_layers

*- Implementation Section ----------------------------------
      call push_routine (myname)

      p_incorporated = sum(incorp_fr_green(1:g%num_parts)
     :                      * g%part_p_green(1:g%num_parts)
     :                   +
     :                    incorp_fr_sen(1:g%num_parts)
     :                      * g%part_p_sen(1:g%num_parts)
     :                   +
     :                    incorp_fr_dead(1:g%num_parts)
     :                      * g%part_p_dead(1:g%num_parts)
     :                    )
     :               * gm2kg /sm2ha


      g%part_p_green(1:g%num_parts) = g%part_p_green(1:g%num_parts)
     :                           * (1.-incorp_fr_green(1:g%num_parts))
      g%part_p_sen(1:g%num_parts) = g%part_p_sen(1:g%num_parts)
     :                           * (1.-incorp_fr_sen(1:g%num_parts))
      g%part_p_dead(1:g%num_parts) = g%part_p_dead(1:g%num_parts)
     :                           * (1.-incorp_fr_dead(1:g%num_parts))


      call crop_root_dist
     :               (
     :                dlayer
     :              , root_length
     :              , root_depth
     :              , dlt_fom_p
     :              , p_incorporated
     :              , max_layer
     :               )

      num_layers = count_of_real_vals(root_length,max_layer)

      call New_postbox ()

      call post_real_array ('dlt_fom_p'
     :                   ,'(kg/ha)'
     :                   , dlt_fom_P
     :                   , num_layers)

       print*,'call event_send(ACTION_incorp_fom_p)'
       call event_send(ACTION_incorp_fom_p)

!      call EI_BroadcastAction     (EventInterface
!     :                            ,ACTION_incorp_fom_p
!     :                            ,Blank
!     :                            )

      call Delete_postbox ()

      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       real function PlantP_Pfact (growth_stage, dm_green)
* ====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real growth_stage
      real dm_green(*)

*+  Purpose
*      Provide value of generic P factor

*+  Changes
*     <insert here>

*+  Calls

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'PlantP_Pfact')

*+  Local Variables
      integer determinant
      integer num_determinants
      real    p_conc_max
      real    p_conc_min
      integer part
      real    max_p
      real    min_p
      real    act_p
      real    max_p_conc
      real    min_p_conc
      real    act_p_conc
      real    determinants_wt
      real    pfact

*- Implementation Section ----------------------------------
      call push_routine (myname)

      num_determinants = count_of_char_vals(c%stress_determinants
     :                                     ,max_parts)

      act_p = 0.0
      min_p = 0.0
      max_p = 0.0
      determinants_wt = 0.0

      do 100 determinant = 1, num_determinants
         part = position_in_char_array
     :             (c%stress_determinants(determinant)
     :             ,g%part_names
     :             ,g%num_parts)

         act_p = act_p + g%part_p_green(part)

         p_conc_max = linear_interp_real
     :                   (growth_stage
     :                   ,c%x_p_stage_code
     :                   ,c%y_p_conc_max(part,1:c%num_x_p_stage_code)
     :                   ,c%num_x_p_stage_code)
         max_p = max_p + p_conc_max * dm_green(part)

         p_conc_min = linear_interp_real
     :                   (growth_stage
     :                   ,c%x_p_stage_code
     :                   ,c%y_p_conc_min(part,1:c%num_x_p_stage_code)
     :                   ,c%num_x_p_stage_code)
         min_p = min_p + p_conc_min * dm_green(part)


         determinants_wt = determinants_wt
     :                   + dm_green(part)
  100 continue

      act_p_conc = divide(act_p, determinants_wt, 0.0)
      max_p_conc = divide(max_p, determinants_wt, 0.0)
      min_p_conc = divide(min_p, determinants_wt, 0.0)

      if ((determinants_wt.eq.0).or. (act_p.eq.0.0)) then
         ! appears that things are not yet initialised
         pfact = 1.0

      else
         pfact = divide(act_p_conc - min_p_conc
     :                 ,max_p_conc - min_p_conc
     :                 ,0.0)
      endif

      PlantP_Pfact = bound(pfact,0.0,1.0)

      call pop_routine (myname)
      return
      end function

* ====================================================================
       subroutine PlantP_Stress (growth_stage, dm_green)
* ====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real growth_stage
      real dm_green(*)

*+  Purpose
*      Provide value of  P stress factors

*+  Changes
*     <insert here>

*+  Calls

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'PlantP_Stress')

*+  Local Variables
      real    pfact

*- Implementation Section ----------------------------------
      call push_routine (myname)

      pfact = PlantP_Pfact(growth_stage,dm_green)

      g%plantPfact_photo = pfact * c%pfact_photo_slope
      g%plantPfact_photo = bound(g%plantPfact_photo,0.0,1.0)

      g%plantPfact_expansion = pfact * c%pfact_expansion_slope
      g%plantPfact_expansion = bound(g%plantPfact_expansion,0.0,1.0)

      g%plantPfact_pheno = pfact * c%pfact_pheno_slope
      g%plantPfact_pheno = bound(g%plantPfact_pheno,0.0,1.0)

      g%plantPfact_grain = pfact * c%pfact_grain_slope
      g%plantPfact_grain = bound(g%plantPfact_grain,0.0,1.0)

      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       subroutine PlantP_init_pools (dm_green)
* ====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real dm_green(*)

*+  Purpose
*      Initialise Plant P Pools

*+  Changes
*     <insert here>

*+  Calls

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'PlantP_init_pools')

*+  Local Variables


*- Implementation Section ----------------------------------
      call push_routine (myname)

      if ((sum(dm_green(1:g%num_parts)).ne.0.0)
     :            .and.
     :   (sum(g%part_p_green(1:g%num_parts)).eq.0.0)) then

         ! biomass has been initialised but the p pools have not
         g%part_p_green(1:g%num_parts) = dm_green(1:g%num_parts)
     :                                 * c%p_conc_init(1:g%num_parts)

      else
         ! Do nothing
      endif


      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       subroutine PlantP_retrans (growth_stage,dm_green)
* ====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real growth_stage
      real dm_green(*)

*+  Purpose
*      Calculate retranslocation between pools

*+  Changes
*     <insert here>

*+  Calls

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'PlantP_retrans')

*+  Local Variables
      real supply(max_parts)
      real demand(max_parts)

      integer counter
      integer num_supplies
      integer num_yield_parts
      integer part
      real    p_conc_min
      real    min_p
      real    p_conc_max
      real    max_p
      real    fraction

*- Implementation Section ----------------------------------
      call push_routine (myname)

      g%dlt_part_p_retrans(:) = 0.0
      supply(:) = 0.0
      demand(:) = 0.0

      num_supplies = count_of_char_vals(c%retrans_parts
     :                                     ,max_parts)
      num_yield_parts = count_of_char_vals(c%yield_parts
     :                                     ,max_parts)

      do 100 counter = 1, num_supplies

         part = position_in_char_array
     :             (c%retrans_parts(counter)
     :             ,g%part_names
     :             ,g%num_parts)

         p_conc_min = linear_interp_real
     :                   (growth_stage
     :                   ,c%x_p_stage_code
     :                   ,c%y_p_conc_min(part,1:c%num_x_p_stage_code)
     :                   ,c%num_x_p_stage_code)
         min_p = p_conc_min * dm_green(part)
         supply(part) = max(g%part_p_green(part) - min_p,0.0)

  100 continue

      do 200 counter = 1, num_yield_parts

         part = position_in_char_array
     :             (c%yield_parts(counter)
     :             ,g%part_names
     :             ,g%num_parts)

         p_conc_max = linear_interp_real
     :                   (growth_stage
     :                   ,c%x_p_stage_code
     :                   ,c%y_p_conc_max(part,1:c%num_x_p_stage_code)
     :                   ,c%num_x_p_stage_code)
         max_p = p_conc_max * dm_green(part)
         demand(part) = max(max_p - g%part_p_green(part), 0.0)

  200 continue

      do 300 part = 1, g%num_parts

         if (supply(part).gt.0.0) then
            fraction = divide(sum(demand),sum(supply),0.0)
            fraction = bound(fraction,0.0,1.0)
            g%dlt_part_p_retrans(part) = -supply(part)*fraction

         elseif (demand(part).gt.0.0) then
            fraction = divide(sum(supply),sum(demand),0.0)
            fraction = bound(fraction,0.0,1.0)
            g%dlt_part_p_retrans(part) = demand(part)*fraction

         else
            ! this part is not involved
         endif
  300 continue


      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       subroutine PlantP_summary ()
* ====================================================================
      Use infrastructure
      implicit none

*+  Purpose
*      Calculate retranslocation between pools

*+  Changes
*     <insert here>

*+  Calls

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'PlantP_summary')

*+  Local Variables
      character  string*200            ! message
      real       P_grain               ! total grain P uptake (kg/ha)
      real       P_dead                ! above ground dead plant P (kg/ha)
      real       P_green               ! above ground green plant P (kg/ha)
      real       P_senesced            ! above ground senesced plant P (kg/ha)
      real       P_stover              ! nitrogen content of stover (kg\ha)
      real       P_total               ! total gross nitrogen content (kg/ha)
      real       P_grain_conc_percent  ! grain nitrogen %

*- Implementation Section ----------------------------------          g%part_p_green(1:g%num_parts)
      call push_routine (myname)

      P_grain_conc_percent = divide (g%part_p_green(grain)
     :                              + g%part_p_dead(grain)
     :                            , g%dm_green(grain)
     :                              + g%dm_dead(grain)
     :                            , 0.0)
     :                     * fract2pcnt

      P_grain = (g%part_p_green(grain) + g%part_p_dead(grain))
     :        * gm2kg/sm2ha

      P_green = (sum_real_array (g%part_p_green, max_part)
     :        - g%part_p_green(root) - g%part_p_green(grain))
     :        * gm2kg / sm2ha

      P_senesced = (sum_real_array (g%part_p_sen, max_part)
     :           - g%part_p_sen(root) - g%part_p_sen(grain))
     :           * gm2kg / sm2ha

      P_dead = (sum_real_array (g%part_p_dead, max_part)
     :       - g%part_p_dead(root) - g%part_p_dead(grain))
     :       * gm2kg / sm2ha

      P_stover = P_green + P_senesced + P_dead
      P_total = P_grain + P_stover

      write (string, '(a,f10.2,t40,a,f10.2)')
     :            ' grain P percent =', P_grain_conc_percent
     :          , ' total P content (kg/ha) =', P_total
      call write_string ( string)

      write (string, '(a,f10.2,t40,a,f10.2)')
     :            ' grain P uptake (kg/ha) =', P_grain
     :          , ' senesced P content (kg/ha) =', P_senesced

      call write_string ( string)

      write (string, '(a,f10.2,t40,a,f10.2)')
     :            ' green P content (kg/ha) =', P_green
     :          , ' dead P content (kg/ha) =', P_dead
      call write_string ( string)

      call pop_routine (myname)
      return
      end subroutine


