*     ===========================================================
      subroutine sugar_phenology (Option)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'sugar.inc'
      include 'crp_phen.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*     Use temperature, photoperiod and genetic characteristics
*     to determine when the crop begins a new growth phase.
*     The initial daily thermal time and height are also set.

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_phenology')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
         call cproc_phenology1 (
     :                             g_previous_stage
     :                            ,g_current_stage
     :                            ,sowing
     :                            ,sprouting
     :                            ,flowering
     :                            ,emerg
     :                            ,flowering
     :                            ,max_stage
     :                            ,C_num_temp
     :                            ,C_x_temp
     :                            ,C_y_tt
     :                            ,G_maxt
     :                            ,G_mint
     :                            ,g_nfact_pheno
     :                            ,G_swdef_pheno
     :                            ,C_pesw_germ
     :                            ,C_fasw_emerg
     :                            ,c_rel_emerg_rate
     :                            ,c_num_fasw_emerg
     :                            ,G_dlayer
     :                            ,max_layer
     :                            ,G_sowing_depth
     :                            ,G_sw_dep
     :                            ,g_dul_dep
     :                            ,P_ll_dep
     :                            ,g_dlt_tt
     :                            ,G_phase_tt
     :                            ,g_phase_devel
     :                            ,g_dlt_stage
     :                            ,g_tt_tot
     :                            ,g_days_tot
     :                            )
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_phenology_init (Option)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'sugar.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*     Use temperature, photoperiod and genetic characteristics
*     to determine when the crop begins a new growth phase.
*     The initial daily thermal time and height are also set.

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_phenology_init')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
            ! initialise phenology phase targets
 
         call sugar_phen_init
     :               (
     :                C_shoot_lag
     :              , C_shoot_rate
     :              , G_current_stage
     :              , G_days_tot
     :              , G_sowing_depth
     :              , G_Ratoon_no
     :              , P_tt_begcane_to_flowering
     :              , P_tt_emerg_to_begcane
     :              , P_tt_flowering_to_crop_end
     :              , g_phase_tt
     :               )
 
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_canopy_height
     :               (
     :                C_height_max
     :              , C_height_stem_slope
     :              , G_canopy_height
     :              , G_current_stage
     :              , G_dm_green
     :              , G_plants
     :              , dlt_canopy_height
     :               )
*     ===========================================================
      implicit none
      include   'sugconst.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      REAL       C_height_max          ! (INPUT)  maximum canopy height (mm)
      REAL       C_height_stem_slope   ! (INPUT)  rate of height growth (mm/g/st
      REAL       G_canopy_height       ! (INPUT)  canopy height (mm)
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
      real       dlt_canopy_height     ! (INPUT) canopy height change (mm)

*+  Purpose
*       This routine calculates the daily change in canopy height.

*+  Changes
*       060495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_canopy_height')

*+  Local Variables
      real       dm_stem_plant         ! dry matter of stem (g/plant)
      real       pot_height            ! potential height (mm)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (stage_is_between (emerg, flowering
     :                    , g_current_stage)) then
 
         dm_stem_plant = divide (g_dm_green(sstem), g_plants, 0.0)
         pot_height = c_height_stem_slope * dm_stem_plant
         pot_height = bound (pot_height, 0.0, c_height_max)
         dlt_canopy_height = pot_height - g_canopy_height
 
      else
         dlt_canopy_height = 0.0
 
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_root_depth (Option)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'sugar.inc'
      include 'crp_root.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Plant root depth calculations

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_root_depth')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
         call cproc_root_depth1 (
     :                              g_dlayer
     :                             ,C_num_sw_ratio
     :                             ,C_x_sw_ratio
     :                             ,C_y_sw_fac_root
     :                             ,G_dul_dep
     :                             ,G_sw_dep
     :                             ,P_ll_dep
     :                             ,C_root_depth_rate
     :                             ,G_current_stage
     :                             ,p_xf
     :                             ,g_dlt_root_depth
     :                             ,g_root_depth
     :                             )
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      real function sugar_rue_reduction
     :               (
     :                G_nfact_photo
     :              , G_temp_stress_photo
     :              , G_oxdef_photo
     :               )
*     ===========================================================
      implicit none
      include   'sugconst.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      REAL       G_nfact_photo         ! (INPUT)
      REAL       G_temp_stress_photo   ! (INPUT)
      REAL       G_oxdef_photo         ! (INPUT)

*+  Purpose
*       The overall fractional effect of non-optimal N, Temperature,
*       and water logging conditions on radiation use efficiency.

*+  Changes
*       060495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_rue_reduction')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
 
      sugar_rue_reduction = min (g_temp_stress_photo
     :                      , g_nfact_photo
     :                      ,g_oxdef_photo)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_radn_int
     :               (
     :                C_extinction_coef
     :              , G_fr_intc_radn
     :              , G_lai
     :              , G_radn
     :              , radn_int
     :               )
*     ===========================================================
      implicit none
      include   'sugconst.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      REAL       C_extinction_coef     ! (INPUT)  radiation extinction coefficie
      REAL       G_fr_intc_radn        ! (INPUT)  fraction of radiation intercep
      REAL       G_lai                 ! (INPUT)  live plant green lai
      REAL       G_radn                ! (INPUT)  solar radiation (Mj/m^2/day)
      real       radn_int              ! (OUTPUT) radiation intercepted
                                       ! by leaves (mj/m^2)

*+  Purpose
*       This routine returns the radiation intercepted by leaves (mj/m^2)

*+  Changes
*     060495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_radn_int')

*+  Local Variables
      real       cover                 ! fraction of radn that is intercepted
                                       ! by leaves (0-1) (m^2/m^2)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (reals_are_equal (g_fr_intc_radn, 0.0)) then
            ! we need to calculate our own interception
 
            ! this equation implies that leaf interception of
            ! solar radiation obeys Beer's law
 
         cover = 1.0 - exp (-c_extinction_coef*g_lai)
         radn_int = cover * g_radn
 
      else
            ! interception has already been calculated for us
         radn_int = g_fr_intc_radn * g_radn
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_leaf_area_potential (Option)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'sugar.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Simulate potential crop leaf area development - may be limited by
*       DM production in subsequent routine

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_leaf_area_potential')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
            ! Plant leaf development
      if (Option .eq. 1) then
 
         call sugar_leaf_area_devel
     :               (
     :                C_leaf_no_correction
     :              , G_dlt_leaf_no
     :              , G_leaf_no
     :              , G_plants
     :              , C_leaf_size
     :              , C_leaf_size_no
     :              , C_num_leaf_size
     :              , C_num_tillerf_leaf_size
     :              , C_tillerf_leaf_size
     :              , C_tillerf_leaf_size_no
     :              , g_dlt_lai_pot
     :               )
 
c         g_dlt_lai_stressed = g_dlt_lai_pot
c     :                    * g_nfact_expansion
c     :                    * g_swdef_expansion
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_leaf_area_init (Option)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'sugar.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*     Set the initial plant leaf area

*+  Changes
*      240498 nih specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_leaf_area_init')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
            ! Plant leaf development
      if (Option .eq. 1) then
 
         call sugar_init_leaf_area
     :               (
     :                C_initial_tpla
     :              , G_current_stage
     :              , G_days_tot
     :              , G_plants
     :              , g_lai
     :              , g_leaf_area
     :               )
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_leaf_area_devel
     :               (
     :                C_leaf_no_correction
     :              , G_dlt_leaf_no
     :              , G_leaf_no
     :              , G_plants
     :              , C_leaf_size
     :              , C_leaf_size_no
     :              , C_num_leaf_size
     :              , C_num_tillerf_leaf_size
     :              , C_tillerf_leaf_size
     :              , C_tillerf_leaf_size_no
     :              , dlt_lai_pot
     :               )
*     ===========================================================
      implicit none
      include   'convert.inc'
      include   'sugconst.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      REAL       C_leaf_no_correction  ! (INPUT)  corrects for other growing lea
      REAL       G_dlt_leaf_no         ! (INPUT)  fraction of oldest leaf expand
      REAL       G_leaf_no(*)          ! (INPUT)  number of fully expanded leave
      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
      REAL       C_leaf_size(*)        ! (INPUT)
      REAL       C_leaf_size_no(*)     ! (INPUT)
      INTEGER    C_num_leaf_size       ! (INPUT)
      INTEGER    C_num_tillerf_leaf_size ! (INPUT)
      REAL       C_tillerf_leaf_size(*) ! (INPUT)
      REAL       C_tillerf_leaf_size_no(*) ! (INPUT)
      real       dlt_lai_pot           ! (OUTPUT) change in leaf area

*+  Purpose
*       Return the potential increase in leaf area development (mm^2)
*       calculated on an individual leaf basis.

*+  Changes
*     070495 nih taken from template
*     120196 nih made this really a potential dlt_lai by removing
*                stress factors.  g_dlt_lai_pot can now be used
*                in different places and stress applied only when
*                required.

*+  Calls
      real       sugar_leaf_size       ! function
cbak

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_leaf_area_devel')

*+  Local Variables
      real       area                  ! potential maximum area of oldest
                                       ! expanding leaf (mm^2) in today's
                                       ! conditions
      real       leaf_no_effective     ! effective leaf no - includes
                                       ! younger leaves that have emerged
                                       ! after the current one

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
           ! once leaf no is calculated leaf area of largest expanding leaf
           ! is determined
 
      leaf_no_effective = sum_between (emerg, now, g_leaf_no)
     :                  + c_leaf_no_correction
      area = sugar_leaf_size
     :               (
     :                C_leaf_size
     :              , C_leaf_size_no
     :              , C_num_leaf_size
     :              , C_num_tillerf_leaf_size
     :              , C_tillerf_leaf_size
     :              , C_tillerf_leaf_size_no
     :              , leaf_no_effective
     :               )
 
cbak
 
      dlt_lai_pot = g_dlt_leaf_no * area * smm2sm * g_plants
cnh     :            * min (g_swdef_expansion, g_nfact_expansion)
 
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      real function sugar_leaf_size
     :               (
     :                C_leaf_size
     :              , C_leaf_size_no
     :              , C_num_leaf_size
     :              , C_num_tillerf_leaf_size
     :              , C_tillerf_leaf_size
     :              , C_tillerf_leaf_size_no
     :              , leaf_no
     :               )
*     ===========================================================
      implicit none
      include   'sugconst.inc'
      include 'science.pub'                       
      include 'error.pub'                         

*+  Sub-Program Arguments
      REAL       C_leaf_size(*)        ! (INPUT)
      REAL       C_leaf_size_no(*)     ! (INPUT)
      INTEGER    C_num_leaf_size       ! (INPUT)
      INTEGER    C_num_tillerf_leaf_size ! (INPUT)
      REAL       C_tillerf_leaf_size(*) ! (INPUT)
      REAL       C_tillerf_leaf_size_no(*) ! (INPUT)
      real       leaf_no               ! (INPUT) nominated leaf number

*+  Purpose
*       Return the leaf area (mm^2) of a specified leaf no.

*+  Changes
*       070495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_leaf_size')

*+  Local Variables
      real leaf_size
      real tiller_factor

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      leaf_size = linear_interp_real
     :                     (real(leaf_no)
     :                     ,c_leaf_size_no
     :                     ,c_leaf_size
     :                     ,c_num_leaf_size
     :                     )
 
      tiller_factor = linear_interp_real
     :                     (real(leaf_no)
     :                     ,c_tillerf_leaf_size_no
     :                     ,c_tillerf_leaf_size
     :                     ,c_num_tillerf_leaf_size
     :                     )
 
      sugar_leaf_size = leaf_size * tiller_Factor
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_leaf_area
     :               (
     :                G_dlt_dm_green
     :              , G_dlt_lai
     :              , G_dlt_lai_stressed
     :              , G_dlt_leaf_no
     :              , G_leaf_no
     :              , C_num_sla_lfno
     :              , C_sla_lfno
     :              , C_sla_max
     :               )
*     ===========================================================
      implicit none
      include   'convert.inc'
      include   'sugconst.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      REAL       G_dlt_dm_green(*)     ! (INPUT)  plant biomass growth (g/m^2)
      REAL       G_dlt_lai             ! (INPUT)  actual change in live plant la
      REAL       G_dlt_lai_stressed    ! (INPUT)  potential change in live plant
      REAL       G_dlt_leaf_no         ! (INPUT)  fraction of oldest leaf expand
      REAL       G_leaf_no(*)          ! (INPUT)  number of fully expanded leave
      INTEGER    C_num_sla_lfno        ! (INPUT)
      REAL       C_sla_lfno(*)         ! (INPUT)
      REAL       C_sla_max(*)          ! (INPUT)  maximum specific leaf area for

*+  Purpose
*       Simulate actual crop leaf area development - checks that leaf area
*       development matches DM production.

*+  Changes
*      070495 nih taken from template

*+  Calls
      real       sugar_sla_max         ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_leaf_area')

*+  Local Variables
      real dlt_lai_carbon     ! maximum daily increase in leaf area
                              ! index from carbon supply
      real leaf_no_today      ! total number of leaves today
      real sla_max            ! maximum allowable specific leaf
                              ! area (cm2/g)

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
         ! limit the delta leaf area by carbon supply
         ! and stress factors
 
      leaf_no_today = sum_between (emerg, now, g_leaf_no)
     :              + g_dlt_leaf_no
 
      sla_max = sugar_sla_max
     :               (
     :                C_num_sla_lfno
     :              , C_sla_lfno
     :              , C_sla_max
     :              , leaf_no_today
     :               )
      dlt_lai_carbon = g_dlt_dm_green(leaf) * sla_max * smm2sm
 
      g_dlt_lai = min (g_dlt_lai_stressed, dlt_lai_carbon)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_leaf_death (Option)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'sugar.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Return the fractional death of oldest green leaf.

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_leaf_death')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (Option .eq. 1) then
         call sugar_leaf_death_grass
     :               (
     :                C_green_leaf_no
     :              , G_current_stage
     :              , G_days_tot
     :              , G_dlt_leaf_no
     :              , G_leaf_no
     :              , G_node_no_dead
     :              , g_dlt_node_no_dead
     :               )
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_dm_senescence
     :               (
     :                C_dm_root_sen_frac
     :              , C_leaf_cabbage_ratio
     :              , C_cabbage_sheath_fr
     :              , G_dlt_dm_green
     :              , G_dlt_lai
     :              , G_dlt_slai
     :              , G_dm_green
     :              , G_dm_senesced
     :              , G_lai
     :              , G_leaf_dm
     :              , G_plants
     :              , G_slai
     :              , G_leaf_area
     :              , dlt_dm_senesced
     :               )
*     ===========================================================
      implicit none
      include   'convert.inc'
      include   'sugconst.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      REAL       C_dm_root_sen_frac    ! (INPUT)  fraction of root dry matter senescing each day (0-1)
      REAL       C_leaf_cabbage_ratio  ! (INPUT)  ratio of leaf wt to cabbage wt ()
      REAL       C_cabbage_sheath_fr   ! (INPUT)  fraction of cabbage that is leaf sheath (0-1)
      REAL       G_dlt_dm_green(*)     ! (INPUT)  plant biomass growth (g/m^2)
      REAL       G_dlt_lai             ! (INPUT)  actual change in live plant lai
      REAL       G_dlt_slai            ! (INPUT)  area of leaf that senesces from plant
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass) (g/m^2)
      REAL       G_dm_senesced(*)      ! (INPUT)  senesced plant dry wt (g/m^2)
      REAL       G_lai                 ! (INPUT)  live plant green lai
      REAL       G_leaf_dm(*)          ! (INPUT)  dry matter of each leaf (g)
      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
      REAL       G_slai                ! (INPUT)  area of leaf that senesces from plant
      REAL       G_leaf_area(*)        ! (INPUT)  leaf area of each leaf (mm^2)
*
      real       dlt_dm_senesced(*)    ! (OUTPUT) actual biomass senesced
                                       ! from plant parts (g/m^2)

*+  Purpose
*       Derives seneseced plant dry matter (g/m^2)

*+  Changes
*       070495 nih taken from template

*+  Calls
      real       sugar_leaf_no_from_lai ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_dm_senescence')

*+  Local Variables
      real       dm_senesced_leaf       ! today's dm of senesced leaves
                                        ! (g/m^2)
      real       dm_senesced_leaf_plant ! today's dm of senesced leaves
                                        ! (g/plant)
      real       lai_today             ! today's green lai
      real       slai_today            ! today's senesced lai
      real       leaf_no_senesced      ! number of senesced leaves today
      integer    leaf_no_senescing     ! leaf number senescing today

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
         ! first we zero all plant component deltas
 
      call fill_real_array (dlt_dm_senesced, 0.0, max_part)
 
      lai_today = g_lai + g_dlt_lai
 
      if (g_dlt_slai .lt. lai_today) then
         slai_today = g_slai + g_dlt_slai
         leaf_no_senesced = sugar_leaf_no_from_lai
     :               (
     :                G_leaf_area
     :              , G_plants
     :              , slai_today
     :               )
         leaf_no_senescing = int (leaf_no_senesced + 1.0)
         dm_senesced_leaf_plant =
     :        sum_real_array (g_leaf_dm, int (leaf_no_senesced))
     :      + mod (leaf_no_senesced, 1.0) * g_leaf_dm(leaf_no_senescing)
 
         dm_senesced_leaf = dm_senesced_leaf_plant * g_plants
         dm_senesced_leaf = l_bound (dm_senesced_leaf
     :                                   , g_dm_senesced(leaf))
 
         dlt_dm_senesced(leaf) = dm_senesced_leaf
     :                         - g_dm_senesced(leaf)
 
         ! Take related cabbage with the dying leaf
 
         dlt_dm_senesced(cabbage) = divide (
     :                                      dlt_dm_senesced (leaf)
     :                                     ,c_leaf_cabbage_ratio
     :                                     ,0.0)
     :                            * c_cabbage_sheath_fr
 
c         dlt_dm_senesced(cabbage) =
c     :         u_bound(dlt_dm_senesced(cabbage),
c     :         g_dm_green(cabbage)+g_dlt_dm_green(cabbage))
 
      else
         dlt_dm_senesced(leaf) = g_dm_green(leaf)
     :                         + g_dlt_dm_green(leaf)
 
         dlt_dm_senesced(cabbage) = g_dm_green(cabbage)
     :                         + g_dlt_dm_green(cabbage)
      endif
 
      dlt_dm_senesced(root) = g_dm_green(root) * c_dm_root_sen_frac
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      real function sugar_leaf_no_from_lai
     :               (
     :                G_leaf_area
     :              , G_plants
     :              , lai
     :               )
*     ===========================================================
      implicit none
      include   'convert.inc'
      include   'sugconst.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      REAL       G_leaf_area(*)        ! (INPUT)  leaf area of each leaf (mm^2)
      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
*
      real       lai                   ! (INPUT) lai of leaves

*+  Purpose
*       Derives leaf no from lai and leaf area

*+  Changes
*       070495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_leaf_no_from_lai')

*+  Local Variables
      real       leaf_area             ! plant leaf area from lai (mm^2)
      integer    leaf_no               ! number of leaves containing leaf
                                       ! leaf area (0-max_leaf)
      real       leaf_area_whole       ! number of complete leaves ()
      real       leaf_area_part        ! area from last leaf (mm^2)
      real       leaf_fract            ! fraction of last leaf (0-1)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      leaf_area = divide (lai, g_plants, 0.0) * sm2smm
      leaf_no = get_cumulative_index_real (leaf_area, g_leaf_area
     :                                   , max_leaf)
 
      leaf_area_whole = sum_real_array (g_leaf_area, leaf_no - 1)
      leaf_area_part = leaf_area - leaf_area_whole
      leaf_fract = divide (leaf_area_part, g_leaf_area(leaf_no), 0.0)
      sugar_leaf_no_from_lai = real (leaf_no - 1) + leaf_fract
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_N_senescence
     :               (
     :                C_n_cabbage_sen_conc
     :              , C_n_leaf_sen_conc
     :              , C_n_root_sen_conc
     :              , G_dlt_dm_senesced
     :              , dlt_N_senesced
     :               )
*     ===========================================================
      implicit none
      include   'sugconst.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      REAL       C_n_cabbage_sen_conc  ! (INPUT)  N concentration of senesced ca
      REAL       C_n_leaf_sen_conc     ! (INPUT)  N concentration of senesced le
      REAL       C_n_root_sen_conc     ! (INPUT)  N concentration of senesced ro
      REAL       G_dlt_dm_senesced(*)  ! (INPUT)  plant biomass senescence (g/m^
*
      real       dlt_N_senesced(*)     ! (OUTPUT) actual nitrogen senesced
                                       ! from plant parts (g/m^2)

*+  Purpose
*       Derives seneseced plant nitrogen (g N/m^2)

*+  Changes
*       070495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_N_senescence')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
         ! first we zero all plant component deltas
 
      call fill_real_array (dlt_N_senesced, 0.0, max_part)
 
      dlt_N_senesced(leaf) = g_dlt_dm_senesced(leaf)
     :                     * c_N_leaf_sen_conc
      dlt_N_senesced(cabbage) = g_dlt_dm_senesced(cabbage)
     :                     * c_N_cabbage_sen_conc
      dlt_N_senesced(root) = g_dlt_dm_senesced(root)
     :                     * c_N_root_sen_conc
cnh what checks are there that there is enough N in plant to provide this
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_detachment (Option)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'sugar.inc'
      include 'crp_cnpy.pub'                      
      include 'crp_nitn.pub'                      
      include 'crp_biom.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Simulate plant detachment.

*+  Changes
*      091294 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_detachment')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 1) then
         if (c_sen_detach_frac(leaf).ne.c_sen_detach_frac(cabbage)) then
            call Fatal_error (ERR_internal
     :               , 'Invalid detachment for leaf and cabbage ratio.')
         else
         endif
         call cproc_dm_detachment1  ( max_part
     :                              , c_sen_detach_frac
     :                              , g_dm_senesced
     :                              , g_dlt_dm_detached
     :                              , c_dead_detach_frac
     :                              , g_dm_dead
     :                              , g_dlt_dm_dead_detached)
 
         call cproc_n_detachment1( max_part
     :                           , c_sen_detach_frac
     :                           , g_n_senesced
     :                           , g_dlt_n_detached
     :                           , c_dead_detach_frac
     :                           , g_n_dead
     :                           , g_dlt_n_dead_detached)
 
         call cproc_lai_detachment1 (leaf
     :                             , c_sen_detach_frac
     :                             , g_slai
     :                             , g_dlt_slai_detached
     :                             , c_dead_detach_frac
     :                             , g_tlai_dead
     :                             , g_dlt_tlai_dead_detached)
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_dm_init
     :               (
     :                C_dm_cabbage_init
     :              , C_dm_leaf_init
     :              , C_dm_sstem_init
     :              , C_dm_sucrose_init
     :              , C_specific_root_length
     :              , G_current_stage
     :              , G_days_tot
     :              , G_dlayer
     :              , G_plants
     :              , G_root_length
     :              , dm_green, dm_plant_min
     :              , leaf_dm
     :               )
*     ===========================================================
      implicit none
      include   'convert.inc'
      include   'sugconst.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      REAL       C_dm_cabbage_init     ! (INPUT)  cabbage "    "        "        "
      REAL       C_dm_leaf_init        ! (INPUT)  leaf growth before emergence (g/plant)
      REAL       C_dm_sstem_init       ! (INPUT)  stem growth before emergence (g/plant)
      REAL       C_dm_sucrose_init     ! (INPUT)  sucrose "    "        "        "
      REAL       C_specific_root_length ! (INPUT)  length of root per unit wt (mm/g)
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
      REAL       G_root_length(*)      ! (INPUT)
      real       dm_green(*)           ! (INPUT/OUTPUT) plant part weights
                                       ! (g/m^2)
      real       dm_plant_min(*)       ! (OUTPUT) minimum weight of each
                                       ! plant part (g/plant)
      real       leaf_dm(*)            ! (OUTOUT) leaf wts

*+  Purpose
*       Initialise plant weights and plant weight minimums
*       at required instances.

*+  Changes
*     060495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_dm_init')

*+  Local Variables
      integer layer
      integer num_layers
      real    root_wt_layer
      real    root_length_layer

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
         ! initialise plant weight
         ! initialisations - set up dry matter for leaf, stem,..etc,
         ! and root
 
      if (on_day_of (emerg, g_current_stage, g_days_tot)) then
             ! seedling has just emerged.
 
             ! we initialise root_wt no by adding all root together
             ! as specified by the rlv given by user at sowing.
         num_layers = count_of_real_vals (g_dlayer, max_layer)
         dm_green(root) = 0.0
         do 100 layer = 1, num_layers
            root_length_layer = g_root_length(layer) * sm2smm
            root_wt_layer  = divide (root_length_layer
     :                              ,c_specific_root_length
     :                              ,0.0)
            dm_green(root) = dm_green(root) +   root_wt_layer
  100    continue
 
         dm_green(sstem) = c_dm_sstem_init * g_plants
         dm_green(leaf) = c_dm_leaf_init * g_plants
         leaf_dm(1) = c_dm_leaf_init
         dm_green(cabbage) = c_dm_cabbage_init * g_plants
         dm_green(sucrose) = c_dm_sucrose_init * g_plants
 
cnh     NO MINIMUMS SET AS YET
 
      else   ! no changes
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_dm_partition
     :               (
     :                C_cane_fraction
     :              , C_leaf_cabbage_ratio
     :              , G_min_sstem_sucrose
     :              , C_ratio_root_shoot
     :              , C_sucrose_delay
     :              , G_current_stage
     :              , G_dm_green
     :              , G_sla_min
     :              , G_sucrose_fraction
     :              , G_tt_tot
     :              , dlt_dm
     :                              , dlt_lai_pot
     :                              , dlt_dm_green
     :                              , partition_xs
     :               )
*     ===========================================================
      implicit none
      include   'convert.inc'
      include   'sugconst.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      REAL       C_cane_fraction       ! (INPUT)
      REAL       C_leaf_cabbage_ratio  ! (INPUT)  ratio of leaf wt to cabbage wt
      REAL       G_min_sstem_sucrose   ! (INPUT)
      REAL       C_ratio_root_shoot(*) ! (INPUT)  root:shoot ratio of new dm ()
      REAL       C_sucrose_delay       ! (INPUT)
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
      REAL       G_sla_min             ! (INPUT)  minimum specific leaf area (mm
      REAL       G_sucrose_fraction    ! (INPUT)  fraction of cane C going to su
      REAL       G_tt_tot(*)           ! (INPUT)  the sum of growing degree days
      real       dlt_dm                ! (INPUT) dry matter to partition
      real       dlt_lai_pot           ! (INPUT) increase in lai if
                                       ! unconstrained by carbon supply.
      real       dlt_dm_green (*)      ! (OUTPUT) actual biomass partitioned
                                       ! to plant parts (g/m^2)
      real       partition_xs          ! xs dry matter to that required
                                       ! to supply all demands. (g/m^2)

*+  Purpose
*       Partitions assimilate between individual plant pools.  The rules
*       for partitioning change with stage of crop growth.

*+  Changes
*       060495 nih taken from template
*       110196 nih added dlt_dm to argument list to make this routine
*                  more like a utility routine for partioning dry matter

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_dm_partition')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      call sugar_dm_partition_rules
     :               (
     :                C_cane_fraction
     :              , C_leaf_cabbage_ratio
     :              , G_min_sstem_sucrose
     :              , C_ratio_root_shoot
     :              , C_sucrose_delay
     :              , G_current_stage
     :              , G_dm_green
     :              , G_sla_min
     :              , G_sucrose_fraction
     :              , G_tt_tot
     :              , dlt_dm
     :                        ,dlt_lai_pot
     :                        ,dlt_dm_green
     :                        ,partition_xs
     :               )
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_dm_retranslocate
     :               (
     :                dm_retranslocate
     :               )
*     ===========================================================
      implicit none
      include   'sugconst.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       dm_retranslocate(*)   ! (INPUT) actual change in plant part
                                       ! weights due to translocation (g/m^2)

*+  Purpose
*     Calculate plant dry matter delta's due to retranslocation (g/m^2)

*+  Changes
*       060495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_dm_retranslocate')

*+  Local Variables
      real       mass_balance          ! sum of translocated carbo (g/m^2)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
         ! now translocate carbohydrate between plant components
 
      call fill_real_array (dm_retranslocate, 0.0, max_part)
 
         ! now check that we have mass balance
 
      mass_balance = sum_real_array (dm_retranslocate, max_part)
      call bound_check_real_var (mass_balance, 0.0, 0.0
     :                         , 'dm_retranslocate mass balance')
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_N_retranslocate
     :               (
     :                G_dm_green
     :              , G_n_conc_min
     :              , G_n_green
     :              , dlt_N_retrans
     :               )
*     ===========================================================
      implicit none
      include   'sugconst.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
      REAL       G_n_conc_min(*)       ! (INPUT)  minimum N concentration (g N/g
      REAL       G_n_green(*)          ! (INPUT)  plant nitrogen content (g N/m^
      real       dlt_N_retrans (*)     ! (OUTPUT) plant N taken out from
                                       ! plant parts (g N/m^2)

*+  Purpose
*     <insert here>

*+  Changes
*       060495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_N_retranslocate')

*+  Local Variables
      real       N_avail(max_part)     ! N available for transfer to grain
                                       ! (g/m^2)
      integer    part                  ! plant part number

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      call sugar_N_retrans_avail
     :               (
     :                G_dm_green
     :              , G_n_conc_min
     :              , G_n_green
     :              , N_avail
     :               )  ! grain N potential (supply)
 
          ! limit retranslocation to total available N
 
      call fill_real_array (dlt_N_retrans, 0.0, max_part)
 
             ! just check that we got the maths right.
 
      do 1000 part = 1, max_part
         call bound_check_real_var (abs (dlt_N_retrans(part))
     :                            , 0.0, N_avail(part)
     :                            , 'dlt_N_retrans(part)')
1000  continue
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_N_retrans_avail
     :               (
     :                G_dm_green
     :              , G_n_conc_min
     :              , G_n_green
     :              , N_avail
     :               )
*     ===========================================================
      implicit none
      include   'sugconst.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
      REAL       G_n_conc_min(*)       ! (INPUT)  minimum N concentration (g N/g
      REAL       G_n_green(*)          ! (INPUT)  plant nitrogen content (g N/m^
      real       N_avail (*)           ! (OUTPUT) total N available for
                                       ! transfer to grain (g/m^2)

*+  Purpose
*     Calculate N available for transfer (g/m^2)
*     from each plant part.

*+  Notes
*     NB. No translocation from roots.

*+  Changes
*       060495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_N_retrans_avail')

*+  Local Variables
      real       N_min                 ! nitrogen minimum level (g/m^2)
      integer    part                  ! plant part number

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
         ! now find the available N of each part.
 
      do 1000 part = 1, max_part
         N_min = g_N_conc_min(part) * g_dm_green(part)
         N_avail(part) = l_bound (g_N_green(part) - N_min, 0.0)
1000  continue
 
      N_avail(sucrose) = 0.0
      N_avail(root) = 0.0
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_N_conc_limits
     :               (
     :                C_n_conc_crit_root
     :              , C_n_conc_min_root
     :              , C_x_stage_code
     :              , C_y_n_conc_crit_cabbage
     :              , C_y_n_conc_crit_cane
     :              , C_y_n_conc_crit_leaf
     :              , C_y_n_conc_min_cabbage
     :              , C_y_n_conc_min_cane
     :              , C_y_n_conc_min_leaf
     :              , G_current_stage
     :              , C_stage_code_list
     :              , G_phase_tt
     :              , G_tt_tot
     :              , N_conc_crit, N_conc_min
     :               )
*     ===========================================================
      implicit none
      include   'sugconst.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      REAL       C_n_conc_crit_root    ! (INPUT)  critical N concentration of ro
      REAL       C_n_conc_min_root     ! (INPUT)  minimum N concentration of roo
      REAL       C_x_stage_code(*)     ! (INPUT)  stage table for N concentratio
      REAL       C_y_n_conc_crit_cabbage(*) ! (INPUT)  critical N concentration
      REAL       C_y_n_conc_crit_cane(*) ! (INPUT)  critical N concentration of
      REAL       C_y_n_conc_crit_leaf(*) ! (INPUT)  critical N concentration of
      REAL       C_y_n_conc_min_cabbage(*) ! (INPUT)  minimum N concentration of
      REAL       C_y_n_conc_min_cane(*) ! (INPUT)  minimum N concentration of fl
      REAL       C_y_n_conc_min_leaf(*) ! (INPUT)  minimum N concentration of le
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       C_stage_code_list(*)  ! (INPUT)  list of stage numbers
      REAL       G_phase_tt(*)         ! (INPUT)  Cumulative growing degree days
      REAL       G_tt_tot(*)           ! (INPUT)  the sum of growing degree days
      real       N_conc_crit(*)        ! (OUTPUT) critical N concentration
                                       ! (g N/g part)
      real       N_conc_min(*)         ! (OUTPUT) minimum N concentration
                                       ! (g N/g part)

*+  Purpose
*       Calculate the critical N concentration below which plant growth
*       is affected.  Also minimum N concentration below which it is not
*       allowed to fall.  These are analogous to the water concentrations
*       of dul and ll.

*+  Changes
*       070495 nih taken from template

*+  Calls
      real       sugar_stage_code      ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_N_conc_limits')

*+  Local Variables
      integer    numvals               ! number of values in stage code table
      real       stage_code            ! interpolated current stage code

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      call fill_real_array (N_conc_crit, 0.0, max_part)
      call fill_real_array (N_conc_min, 0.0, max_part)
 
      if (stage_is_between (emerg, crop_end, g_current_stage)) then
 
         N_conc_crit(root) = c_N_conc_crit_root
         N_conc_min(root) = c_N_conc_min_root
 
             ! the tops critical N percentage concentration is the stover
             ! (non-grain shoot) concentration below which N concentration
             ! begins to affect plant growth.
 
         numvals = count_of_real_vals (c_x_stage_code, max_stage)
         stage_code = sugar_stage_code
     :               (
     :                C_stage_code_list
     :              , G_phase_tt
     :              , G_tt_tot
     :              , g_current_stage, c_x_stage_code
     :                                , numvals
     :               )
         ! nih - I put cane critical conc in the sstem element of the
         ! array because there is no 'cane' (sstem+sucrose) pool
         N_conc_crit(sstem) = linear_interp_real (stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_crit_cane
     :                             , numvals)
         N_conc_crit(leaf) = linear_interp_real (stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_crit_leaf
     :                             , numvals)
         N_conc_crit(cabbage) = linear_interp_real (stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_crit_cabbage
     :                             , numvals)
 
             ! the  minimum N concentration is the N concentration
             ! below which N does not fall.
 
         ! nih - I put cane minimum conc in the sstem element of the
         ! array because there is no 'cane' (sstem+sucrose) pool
         N_conc_min(sstem) = linear_interp_real (stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_min_cane
     :                             , numvals)
 
         N_conc_min(leaf) = linear_interp_real (stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_min_leaf
     :                             , numvals)
 
         N_conc_min(cabbage) = linear_interp_real (stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_min_cabbage
     :                             , numvals)
 
      else
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_update
     :               (
     :                G_canopy_height
     :              , G_cnd_photo
     :              , G_cswd_expansion
     :              , G_cswd_pheno
     :              , G_cswd_photo
     :              , G_dlt_canopy_height
     :              , G_dlt_dm
     :              , G_dlt_dm_dead_detached
     :              , G_dlt_dm_detached
     :              , G_dlt_dm_green
     :              , G_dlt_dm_green_retrans
     :              , G_dlt_dm_senesced
     :              , G_dlt_dm_realloc
     :              , G_dlt_lai
     :              , G_dlt_leaf_no
     :              , G_dlt_node_no
     :              , G_dlt_node_no_dead
     :              , G_dlt_n_dead_detached
     :              , G_dlt_n_detached
     :              , G_dlt_n_green
     :              , G_dlt_n_retrans
     :              , G_dlt_n_senesced
     :              , G_dlt_n_realloc
     :              , G_dlt_plants
     :              , G_dlt_plant_wc
     :              , G_dlt_root_length
     :              , G_dlt_root_length_senesced
     :              , G_dlt_root_depth
     :              , G_dlt_slai
     :              , G_dlt_slai_detached
     :              , G_dlt_stage
     :              , G_dlt_tlai_dead_detached
     :              , G_dm_dead
     :              , G_dm_green
     :              , G_dm_plant_top_tot
     :              , G_dm_senesced
     :              , G_lai
     :              , G_leaf_area
     :              , G_leaf_dm
     :              , G_leaf_no
     :              , G_node_no
     :              , G_node_no_dead
     :              , G_nfact_photo
     :              , G_n_conc_crit
     :              , G_n_conc_min
     :              , G_n_dead
     :              , G_n_green
     :              , G_n_senesced
     :              , G_plants
     :              , G_plant_wc
     :              , G_previous_stage
     :              , G_root_length
     :              , G_root_depth
     :              , G_slai
     :              , G_swdef_expansion
     :              , G_swdef_pheno
     :              , G_swdef_photo
     :              , G_tlai_dead
     :              , C_n_conc_crit_root
     :              , C_n_conc_min_root
     :              , C_x_stage_code
     :              , C_y_n_conc_crit_cabbage
     :              , C_y_n_conc_crit_cane
     :              , C_y_n_conc_crit_leaf
     :              , C_y_n_conc_min_cabbage
     :              , C_y_n_conc_min_cane
     :              , C_y_n_conc_min_leaf
     :              , G_current_stage
     :              , C_stage_code_list
     :              , G_phase_tt
     :              , G_tt_tot
     :              , G_node_no_detached
     :              , C_leaf_no_at_emerg
     :               )
*     ===========================================================
      implicit none
      include   'convert.inc'
      include   'sugconst.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      REAL       G_canopy_height       ! (INPUT)  canopy height (mm)
      REAL       G_cnd_photo(*)        ! (INPUT)  cumulative nitrogen stress typ
      REAL       G_cswd_expansion(*)   ! (INPUT)  cumulative water stress type 2
      REAL       G_cswd_pheno(*)       ! (INPUT)  cumulative water stress type 3
      REAL       G_cswd_photo(*)       ! (INPUT)  cumulative water stress type 1
      REAL       G_dlt_canopy_height   ! (INPUT)  change in canopy height (mm)
      REAL       G_dlt_dm              ! (INPUT)  the daily biomass production (
      REAL       G_dlt_dm_dead_detached(*) ! (INPUT)  plant biomass detached fro
      REAL       G_dlt_dm_detached(*)  ! (INPUT)  plant biomass detached (g/m^2)
      REAL       G_dlt_dm_green(*)     ! (INPUT)  plant biomass growth (g/m^2)
      REAL       G_dlt_dm_green_retrans(*) ! (INPUT)  plant biomass retranslocat
      REAL       G_dlt_dm_senesced(*)  ! (INPUT)  plant biomass senescence (g/m^
      REAL       G_dlt_dm_realloc(*)   ! (INPUT)
      REAL       G_dlt_lai             ! (INPUT)  actual change in live plant la
      REAL       G_dlt_leaf_no         ! (INPUT)  fraction of oldest leaf expand
      REAL       G_dlt_node_no         ! (INPUT)
      REAL       G_dlt_node_no_dead    ! (INPUT)  fraction of oldest green leaf
      REAL       G_dlt_n_dead_detached(*) ! (INPUT)  actual N loss with detached
      REAL       G_dlt_n_detached(*)   ! (INPUT)  actual N loss with detached pl
      REAL       G_dlt_n_green(*)      ! (INPUT)  actual N uptake into plant (g/
      REAL       G_dlt_n_retrans(*)    ! (INPUT)  nitrogen retranslocated out fr
      REAL       G_dlt_n_senesced(*)   ! (INPUT)  actual N loss with senesced pl
      REAL       G_dlt_n_realloc(*)   ! (INPUT)
      REAL       G_dlt_plants          ! (INPUT)  change in Plant density (plant
      REAL       G_dlt_plant_wc(*)     ! (INPUT)
      REAL       G_dlt_root_length(*)  ! (INPUT)
      REAL       G_dlt_root_length_senesced(*) ! (INPUT)
      REAL       G_dlt_root_depth      ! (INPUT)  increase in root depth (mm)
      REAL       G_dlt_slai            ! (INPUT)  area of leaf that senesces fro
      REAL       G_dlt_slai_detached   ! (INPUT)  plant senesced lai detached
      REAL       G_dlt_stage           ! (INPUT)  change in stage number
      REAL       G_dlt_tlai_dead_detached ! (INPUT)  plant lai detached from dea
      REAL       G_dm_dead(*)          ! (INPUT)  dry wt of dead plants (g/m^2)
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
      REAL       G_dm_plant_top_tot(*) ! (INPUT)  total carbohydrate production
      REAL       G_dm_senesced(*)      ! (INPUT)  senesced plant dry wt (g/m^2)
      REAL       G_lai                 ! (INPUT)  live plant green lai
      REAL       G_leaf_area(*)        ! (INPUT)  leaf area of each leaf (mm^2)
      REAL       G_leaf_dm(*)          ! (INPUT)  dry matter of each leaf (g)
      REAL       G_leaf_no(*)          ! (INPUT)  number of fully expanded leave
      REAL       G_node_no(*)
      REAL       G_node_no_dead(*)     ! (INPUT)  no of dead leaves ()
      REAL       G_nfact_photo         ! (INPUT)
      REAL       G_n_conc_crit(*)      ! (INPUT)  critical N concentration (g N/
      REAL       G_n_conc_min(*)       ! (INPUT)  minimum N concentration (g N/g
      REAL       G_n_dead(*)           ! (INPUT)  plant N content of dead plants
      REAL       G_n_green(*)          ! (INPUT)  plant nitrogen content (g N/m^
      REAL       G_n_senesced(*)       ! (INPUT)  plant N content of senesced pl
      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
      REAL       G_plant_wc(*)         ! (INPUT)
      REAL       G_previous_stage      ! (INPUT)  previous phenological stage
      REAL       G_root_length(*)      ! (INPUT)
      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
      REAL       G_slai                ! (INPUT)  area of leaf that senesces fro
      REAL       G_swdef_expansion     ! (INPUT)
      REAL       G_swdef_pheno         ! (INPUT)
      REAL       G_swdef_photo         ! (INPUT)
      REAL       G_tlai_dead           ! (INPUT)  total lai of dead plants
      REAL       C_n_conc_crit_root    ! (INPUT)  critical N concentration of ro
      REAL       C_n_conc_min_root     ! (INPUT)  minimum N concentration of roo
      REAL       C_x_stage_code(*)     ! (INPUT)  stage table for N concentratio
      REAL       C_y_n_conc_crit_cabbage(*) ! (INPUT)  critical N concentration
      REAL       C_y_n_conc_crit_cane(*) ! (INPUT)  critical N concentration of
      REAL       C_y_n_conc_crit_leaf(*) ! (INPUT)  critical N concentration of
      REAL       C_y_n_conc_min_cabbage(*) ! (INPUT)  minimum N concentration of
      REAL       C_y_n_conc_min_cane(*) ! (INPUT)  minimum N concentration of fl
      REAL       C_y_n_conc_min_leaf(*) ! (INPUT)  minimum N concentration of le
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       C_stage_code_list(*)  ! (INPUT)  list of stage numbers
      REAL       G_phase_tt(*)         ! (INPUT)  Cumulative growing degree days
      REAL       G_tt_tot(*)           ! (INPUT)  the sum of growing degree days
      REAL       G_node_no_detached    ! (INPUT)  number of detached leaves
      REAL       C_leaf_no_at_emerg    ! (INPUT)  number of leaves at emergence

*+  Purpose
*       Update states

*+  Changes
*      070495 nih taken from template
*      030996 nih added detachment accounting
*      030498 igh added bound checking to leaf_no

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_update')

*+  Local Variables
      real       dlt_dm_plant          ! dry matter increase (g/plant)
      real       dlt_leaf_area         ! leaf area increase (mm^2/plant)
      real       dlt_leaf_dm           !
      real       dlt_dm_green_dead     ! dry matter of green plant part dying
                                       ! (g/m^2)
      real       dlt_dm_senesced_dead  ! dry matter of senesced plant part
                                       ! dying (g/m^2)
      real       dlt_dm_plant_leaf     ! increase in plant leaf dm (g/plant)
      real       dlt_N_green_dead      ! N content of green plant part dying
                                       ! (g/m^2)
      real       dlt_N_senesced_dead   ! N content of senesced plant part
                                       ! dying (g/m^2)
      real       dlt_lai_dead          ! lai of green leaf of plants dying ()
      real       dlt_slai_dead         ! lai of senesced leaf of plant dying ()
      real       dying_fract           ! fraction op population dying (0-1)
      real       node_no
      integer    part                  ! plant part index
      integer    num_leaves            ! number of leaves on plant
      integer    empty_leaves          ! number of empty leaf records
      integer    leaf_rec              ! leaf record number
      integer    leaf_rec_new          ! new leaf record number

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
 
cnh
      ! take out water with detached stems
      g_plant_wc(sstem) = g_plant_wc(sstem)
     :                  * (1.0 - divide (g_dlt_dm_dead_detached(sstem)
     :                                  ,g_dm_dead(sstem)
     :                                   +g_dm_green(sstem)
     :                                  ,0.0)
     :                    )
      call add_real_array (g_dlt_plant_wc, g_plant_wc, max_part)
 
         ! transfer N
 
      dying_fract = divide (-g_dlt_plants, g_plants, 0.0)
 
      do 1000 part = 1, max_part
         dlt_N_green_dead = g_N_green(part) * dying_fract
         g_N_green(part) = g_N_green(part) - dlt_N_green_dead
         g_N_dead(part) = g_N_dead(part) + dlt_N_green_dead
 
         dlt_N_senesced_dead = g_N_senesced(part) * dying_fract
         g_N_senesced(part) = g_N_senesced(part) - dlt_N_senesced_dead
         g_N_dead(part) = g_N_dead(part) + dlt_N_senesced_dead
1000  continue
 
      call subtract_real_array (g_dlt_N_dead_detached, g_N_dead
     :                        , max_part)
 
      call add_real_array (g_dlt_N_green, g_N_green, max_part)
      call add_real_array (g_dlt_N_retrans, g_N_green, max_part)
      call add_real_array (g_dlt_N_realloc, g_N_green, max_part)
      call subtract_real_array (g_dlt_N_senesced, g_N_green
     :                        , max_part)
 
      call add_real_array (g_dlt_N_senesced, g_N_senesced
     :                   , max_part)
      call subtract_real_array (g_dlt_N_detached, g_N_senesced
     :                        , max_part)
 
 
         ! Transfer plant dry matter
 
      dlt_dm_plant = divide (g_dlt_dm, g_plants, 0.0)
 
      call accumulate (dlt_dm_plant, g_dm_plant_top_tot
     :               , g_previous_stage, g_dlt_stage)
 
      do 2000 part = 1, max_part
         dlt_dm_green_dead = g_dm_green(part) * dying_fract
         g_dm_green(part) = g_dm_green(part) - dlt_dm_green_dead
         g_dm_dead(part) = g_dm_dead(part) + dlt_dm_green_dead
 
         dlt_dm_senesced_dead = g_dm_senesced(part) * dying_fract
         g_dm_senesced(part) = g_dm_senesced(part)
     :                       - dlt_dm_senesced_dead
         g_dm_dead(part) = g_dm_dead(part) + dlt_dm_senesced_dead
2000  continue
 
      call subtract_real_array (g_dlt_dm_dead_detached, g_dm_dead
     :                        , max_part)
 
      call add_real_array (g_dlt_dm_green, g_dm_green, max_part)
      call add_real_array (g_dlt_dm_green_retrans, g_dm_green, max_part)
      call add_real_array (g_dlt_dm_realloc, g_dm_green, max_part)
      call subtract_real_array (g_dlt_dm_senesced, g_dm_green
     :                        , max_part)
 
      call add_real_array (g_dlt_dm_senesced, g_dm_senesced
     :                   , max_part)
      call subtract_real_array (g_dlt_dm_detached, g_dm_senesced
     :                        , max_part)
 
 
c      dm_residue = (sum_real_array (g_dlt_dm_detached, max_part)
c     :           - g_dlt_dm_detached(root))
c      N_residue = (sum_real_array (g_dlt_N_detached, max_part)
c     :          - g_dlt_N_detached(root))
c
c      call sugar_top_residue (dm_residue, N_residue)
 
c             ! put roots into root residue
 
c      call sugar_root_incorp (g_dlt_dm_detached(root)
c     :                    , g_dlt_N_detached(root))
 
 
      call sugar_update_other_variables ()
 
         ! transfer plant leaf area
      dlt_lai_dead  = g_lai  * dying_fract
      dlt_slai_dead = g_slai * dying_fract
 
      g_lai = g_lai + g_dlt_lai - dlt_lai_dead - g_dlt_slai
      g_slai = g_slai + g_dlt_slai - dlt_slai_dead - g_dlt_slai_detached
      g_tlai_dead = g_tlai_dead + dlt_lai_dead + dlt_slai_dead
     :            - g_dlt_tlai_dead_detached
 
 
         ! plant leaf development
         ! need to account for truncation of partially developed leaf (add 1)
c      leaf_no = 1.0 + sum_between (emerg, now, g_leaf_no)
         ! need to add leaf at emergence because we now remove records of detach
         ! and so whereever detached leaves are used we need to account for the
         ! are set at emergence as these offset the records.
         ! THIS NEEDS CHANGING!!!!
      node_no = 1.0 + sum_between (emerg, now, g_node_no)
     :              - g_node_no_detached
     :              - c_leaf_no_at_emerg
 
      node_no = l_bound(node_no, 1.0)
 
      dlt_leaf_area = divide (g_dlt_lai, g_plants, 0.0) * sm2smm
      call accumulate (dlt_leaf_area, g_leaf_area
     :               , node_no, g_dlt_node_no)
 
      dlt_dm_plant_leaf = divide (g_dlt_dm_green(leaf), g_plants, 0.0)
      call accumulate (dlt_dm_plant_leaf, g_leaf_dm
     :               , node_no, g_dlt_node_no)
 
      call accumulate (g_dlt_leaf_no, g_leaf_no
     :               , g_previous_stage, g_dlt_stage)
      call accumulate (g_dlt_node_no, g_node_no
     :               , g_previous_stage, g_dlt_stage)
 
      call accumulate (g_dlt_node_no_dead, g_node_no_dead
     :               , g_previous_stage, g_dlt_stage)
 
 
      ! detached leaf area needs to be accounted for
 
      dlt_leaf_area = divide (g_dlt_slai_detached, g_plants, 0.0)
     :              * sm2smm
      num_leaves = count_of_Real_vals(g_leaf_area,max_leaf)
      num_leaves = max_leaf
      dlt_leaf_dm = g_dlt_dm_detached(leaf)/g_plants
 
      empty_leaves = -1
      do 111 leaf_rec = 1,num_leaves
        if (g_leaf_area(leaf_rec).le.dlt_leaf_area) then
           dlt_leaf_area = dlt_leaf_area - g_leaf_area(leaf_rec)
           g_leaf_area(leaf_rec) = 0.0
        else
           g_leaf_area(leaf_rec) = g_leaf_area(leaf_rec) - dlt_leaf_area
           dlt_leaf_area = 0.0
        endif
        if (g_leaf_dm(leaf_rec).le.dlt_leaf_dm) then
           dlt_leaf_dm = dlt_leaf_dm - g_leaf_dm(leaf_rec)
           g_leaf_dm(leaf_rec) = 0.0
        else
           g_leaf_dm(leaf_rec) = g_leaf_dm(leaf_rec) - dlt_leaf_dm
           dlt_leaf_dm = 0.0
        endif
        if ((g_leaf_dm(leaf_rec).gt.0.0).and.(empty_leaves.eq.-1)) then
           empty_leaves = leaf_rec - 1
        else
        endif
  111 continue
 
      if (empty_leaves.gt.0) then
         g_node_no_detached = g_node_no_detached + empty_leaves
         !kludgy solution for now
         do 112 leaf_rec=empty_leaves+1, num_leaves
            leaf_rec_new = leaf_rec - empty_leaves
            g_leaf_dm(leaf_rec_new)=g_leaf_dm(leaf_rec)
            g_leaf_area(leaf_rec_new)=g_leaf_area(leaf_rec)
            g_leaf_dm(leaf_rec) = 0.0
            g_leaf_area(leaf_rec) = 0.0
  112    continue
      else
      endif
 
 
         ! plant stress
 
      call accumulate (1.0 - g_swdef_photo, g_cswd_photo
     :               , g_previous_stage, g_dlt_stage)
      call accumulate (1.0 - g_swdef_expansion, g_cswd_expansion
     :               , g_previous_stage, g_dlt_stage)
      call accumulate (1.0 - g_swdef_pheno, g_cswd_pheno
     :               , g_previous_stage, g_dlt_stage)
 
      call accumulate (1.0 - g_nfact_photo, g_cnd_photo
     :               , g_previous_stage, g_dlt_stage)
 
         ! other plant states
 
      g_canopy_height = g_canopy_height + g_dlt_canopy_height
      g_plants = g_plants + g_dlt_plants
      g_root_depth = g_root_depth + g_dlt_root_depth
      call add_real_array      (g_dlt_root_length
     :                         , g_root_length, max_layer)
      call subtract_real_array (g_dlt_root_length_senesced
     :                         , g_root_length, max_layer)
 
      call sugar_N_conc_limits
     :               (
     :                C_n_conc_crit_root
     :              , C_n_conc_min_root
     :              , C_x_stage_code
     :              , C_y_n_conc_crit_cabbage
     :              , C_y_n_conc_crit_cane
     :              , C_y_n_conc_crit_leaf
     :              , C_y_n_conc_min_cabbage
     :              , C_y_n_conc_min_cane
     :              , C_y_n_conc_min_leaf
     :              , G_current_stage
     :              , C_stage_code_list
     :              , G_phase_tt
     :              , G_tt_tot
     :              , g_N_conc_crit, g_N_conc_min
     :               )  ! plant N concentr
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_plant_death (Option)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'sugar.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*      Determine plant death in crop

*+  Changes
*       290994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_plant_death')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
         call sugar_failure_germination
     :               (
     :                C_days_germ_limit
     :              , G_current_stage
     :              , G_days_tot
     :              , G_plants
     :              , g_dlt_plants_failure_germ
     :               )
 
         call sugar_failure_emergence
     :               (
     :                C_tt_emerg_limit
     :              , G_current_stage
     :              , G_plants
     :              , G_tt_tot
     :              , g_dlt_plants_failure_emergence
     :               )
 
         call sugar_failure_leaf_sen
     :               (
     :                G_current_stage
     :              , G_lai
     :              , G_plants
     :              , g_dlt_plants_failure_leaf_sen
     :               )
 
         call sugar_death_drought
     :               (
     :                C_leaf_no_crit
     :              , C_swdf_photo_limit
     :              , C_swdf_photo_rate
     :              , G_cswd_photo
     :              , G_leaf_no
     :              , G_plants
     :              , G_swdef_photo
     :              , g_dlt_plants_death_drought
     :               )
 
         call sugar_death_lodging
     :               (
     :                g_lodge_flag
     :              , G_swdef_photo
     :              , G_oxdef_photo
     :              , c_stress_lodge
     :              , c_death_fr_lodge
     :              , c_num_stress_lodge
     :              , G_plants
     :              , g_dlt_plants_death_lodging
     :               )
 
c         call sugar_death_external_action (g_dlt_plants_death_external)
         call sugar_death_actual
     :               (
     :                G_dlt_plants_death_drought
     :              , G_dlt_plants_failure_emergence
     :              , G_dlt_plants_failure_germ
     :              , G_dlt_plants_failure_leaf_sen
     :              , G_dlt_plants_death_lodging
     :              , g_dlt_plants
     :               )
         if (reals_are_equal (g_dlt_plants + g_plants, 0.0)) then
            call sugar_kill_crop
     :               (
     :                G_crop_status
     :              , G_day_of_year
     :              , G_dm_dead
     :              , G_dm_green
     :              , G_dm_senesced
     :              , G_year
     :               )
         else
         endif
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_event
     :               (
     :                C_stage_code_list
     :              , C_stage_names
     :              , G_current_stage
     :              , G_days_tot
     :              , G_day_of_year
     :              , G_dlayer
     :              , G_dm_dead
     :              , G_dm_green
     :              , G_dm_senesced
     :              , G_lai
     :              , G_n_green
     :              , G_root_depth
     :              , G_sw_dep
     :              , G_year
     :              , P_ll_dep
     :               )
*     ===========================================================
      implicit none
      include   'const.inc'            ! new_line, lu_scr_sum, blank,
      include   'convert.inc'
      include   'sugconst.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'write.pub'                         
      include 'error.pub'                         

*+  Sub-Program Arguments
      REAL       C_stage_code_list(*)  ! (INPUT)  list of stage numbers
      CHARACTER  C_stage_names(*)*(*)  ! (INPUT)  full names of stages for repor
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
      INTEGER    G_day_of_year         ! (INPUT)  day of year
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL       G_dm_dead(*)          ! (INPUT)  dry wt of dead plants (g/m^2)
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
      REAL       G_dm_senesced(*)      ! (INPUT)  senesced plant dry wt (g/m^2)
      REAL       G_lai                 ! (INPUT)  live plant green lai
      REAL       G_n_green(*)          ! (INPUT)  plant nitrogen content (g N/m^
      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
      REAL       G_sw_dep(*)           ! (INPUT)  soil water content of layer L
      INTEGER    G_year                ! (INPUT)  year
      REAL       P_ll_dep(*)           ! (INPUT)  lower limit of plant-extractab

*+  Purpose
*       Report occurence of event and the current status of specific
*       variables.

*+  Changes
*     070495 nih taken from template

*+  Calls
                                       ! lu_scr_sum

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_event')

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
 
      stage_no = int (g_current_stage)
      if (on_day_of (stage_no, g_current_stage, g_days_tot)) then
             ! new phase has begun.
         write (string, '(a, f6.1, 1x, a)')
     :                   ' stage '
     :                  , c_stage_code_list(stage_no)
     :                  , c_stage_names(stage_no)
cnh         call report_event (string)
         call report_date_and_event (g_day_of_year,g_year,string)
 
         biomass = sum_real_array (g_dm_green, max_part)
     :           - g_dm_green(root)
 
     :           + sum_real_array (g_dm_senesced, max_part)
     :           - g_dm_senesced(root)
 
     :           + sum_real_array (g_dm_dead, max_part)
     :           - g_dm_dead(root)
 
         dm_green = sum_real_array (g_dm_green, max_part)
     :            - g_dm_green(root)
         N_green = sum_real_array (g_N_green, max_part)
     :           - g_N_green(root)
 
         N_green_conc_percent = divide (N_green, dm_green, 0.0)
     :                        * fract2pcnt
 
         deepest_layer = find_layer_no (g_root_depth, g_dlayer
     :                                , max_layer)
         do 1000 layer = 1, deepest_layer
            pesw(layer) = g_sw_dep(layer) - p_ll_dep(layer)
            pesw(layer) = l_bound (pesw(layer), 0.0)
1000     continue
         pesw_tot = sum_real_array (pesw, deepest_layer)
 
         if (stage_is_between (emerg, crop_end, g_current_stage)) then
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
            call write_string (lu_scr_sum, string)
         else
         endif
 
      else
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_N_init
     :               (
     :                C_n_cabbage_init_conc
     :              , C_n_leaf_init_conc
     :              , C_n_root_init_conc
     :              , C_n_sstem_init_conc
     :              , G_current_stage
     :              , G_days_tot
     :              , G_dm_green
     :              , N_green
     :               )
*     ===========================================================
      implicit none
      include   'sugconst.inc'
      include 'science.pub'                       
      include 'error.pub'                         

*+  Sub-Program Arguments
      REAL       C_n_cabbage_init_conc ! (INPUT)     "   cabbage    "
      REAL       C_n_leaf_init_conc    ! (INPUT)  initial leaf N concentration (
      REAL       C_n_root_init_conc    ! (INPUT)  initial root N concentration (
      REAL       C_n_sstem_init_conc   ! (INPUT)  initial stem N concentration (
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
      real       N_green(*)            ! plant nitrogen (g/m^2)

*+  Purpose
*       Set plant nitrogen

*+  Changes
*     060495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_N_init')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (on_day_of (emerg, g_current_stage, g_days_tot)) then
         if (N_green(root).eq.0.0) then
            ! There is no root system currently operating from
            ! a previous crop
            N_green(root) = c_N_root_init_conc*g_dm_green(root)
         else
            ! There IS a root system currently operating from
            ! a previous crop
         endif
         N_green(sstem) = c_N_sstem_init_conc*g_dm_green(sstem)
         N_green(leaf) = c_N_leaf_init_conc*g_dm_green(leaf)
         N_green(cabbage) = c_N_cabbage_init_conc*g_dm_green(cabbage)
         N_green(sucrose) = 0.0
 
      else
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_sla_min
     :               (
     :                C_num_sla_lfno
     :              , C_sla_lfno
     :              , C_sla_min
     :              , leaf_no, sla_min
     :               )
*     ===========================================================
      implicit none
      include   'sugconst.inc'
      include 'science.pub'                       
      include 'error.pub'                         

*+  Sub-Program Arguments
      INTEGER    C_num_sla_lfno        ! (INPUT)
      REAL       C_sla_lfno(*)         ! (INPUT)
      REAL       C_sla_min(*)          ! (INPUT)  minimum specific leaf area for new leaf area (mm^2/g)
      real       leaf_no               ! (INPUT) nominated leaf number
      real       sla_min               ! (OUTPUT)

*+  Purpose
*       Return the minimum specific leaf area (mm^2/g)
*       of a specified leaf no.

*+  Changes
*       05/05/95 nih specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_sla_min')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      sla_min = linear_interp_real
     :                     (real(leaf_no)
     :                     ,c_sla_lfno
     :                     ,c_sla_min
     :                     ,c_num_sla_lfno
     :                     )
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      real function sugar_sla_max
     :               (
     :                C_num_sla_lfno
     :              , C_sla_lfno
     :              , C_sla_max
     :              , leaf_no
     :               )
*     ===========================================================
      implicit none
      include   'sugconst.inc'
      include 'science.pub'                       
      include 'error.pub'                         

*+  Sub-Program Arguments
      INTEGER    C_num_sla_lfno        ! (INPUT)
      REAL       C_sla_lfno(*)         ! (INPUT)
      REAL       C_sla_max(*)          ! (INPUT)  maximum specific leaf area for new leaf area (mm^2/g)
      real       leaf_no               ! (INPUT) nominated leaf number

*+  Purpose
*       Return the maximum specific leaf area (mm^2/g)
*       of a specified leaf no.

*+  Changes
*       05/05/95 nih specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_sla_max')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      sugar_sla_max = linear_interp_real
     :                     (real(leaf_no)
     :                     ,c_sla_lfno
     :                     ,c_sla_max
     :                     ,c_num_sla_lfno
     :                     )
 
      call pop_routine (my_name)
      return
      end



* ====================================================================
       real function sugar_profile_fasw ()
* ====================================================================
      implicit none
      include 'sugar.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Purpose
*     <insert here>

*+  Changes
*   neilh - 30-06-1995 - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'sugar_profile_fasw')

*+  Local Variables
      real    asw
      real    asw_pot
      integer deepest_layer
      integer layer

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      deepest_layer = find_layer_no (g_root_depth, g_dlayer, max_layer)
      asw_pot = 0.0
      asw     = 0.0
      do 100 layer = 1, deepest_layer
         asw_pot = asw_pot + g_sw_avail_pot (layer)
         asw = asw + u_bound (g_sw_avail(layer), g_sw_avail_pot(layer))
  100 continue
 
      sugar_profile_fasw = divide (asw, asw_pot, 0.0)
 
      call pop_routine (myname)
      return
      end



* ====================================================================
       subroutine sugar_water_content
     :               (
     :                C_cane_dmf_tt
     :              , C_cane_dmf_min
     :              , C_cane_dmf_max
     :              , C_num_cane_dmf
     :              , C_cane_dmf_rate
     :              , g_swdef_stalk
     :              , g_nfact_stalk
     :              , g_temp_stress_stalk
     :              , G_dlt_dm_green
     :              , g_dm_green
     :              , G_dlt_plant_wc
     :              , G_plant_wc
     :              , G_tt_tot
     :               )
* ====================================================================
      implicit none
       include 'sugconst.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      REAL       C_cane_dmf_tt(*)      ! (INPUT)
      REAL       C_cane_dmf_min(*)     ! (INPUT)
      REAL       C_cane_dmf_max(*)     ! (INPUT)
      INTEGER    C_num_cane_dmf        ! (INPUT)
      REAL       C_cane_dmf_rate       ! (INPUT)
      REAL       G_swdef_stalk         ! (INPUT)
      REAL       G_nfact_stalk         ! (INPUT)
      REAL       G_temp_stress_stalk   ! (INPUT)
      REAL       G_dlt_dm_green(*)     ! (INPUT)  plant biomass growth (g/m^2)
      REAL       G_dm_green(*)         ! (INPUT)
      REAL       G_dlt_plant_wc(*)     ! (INPUT)
      REAL       G_plant_wc(*)         ! (OUTPUT)
      REAL       G_tt_tot              ! (INPUT)

*+  Purpose
*     <insert here>

*+  Notes
*   NIH - Eventually this routine will need to be broken down into
*         subroutines.

*+  Changes
*   neilh - 11-10-1995 - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'sugar_water_content')

*+  Local Variables
       real tt                  ! thermal time (deg. day)
       real cane_dmf_max        ! max dm fraction in
                                ! cane(sstem+sucrose)
       real cane_dmf_min        ! min dm fraction in
                                ! cane(sstem+sucrose)
       real cane_dmf
       real stress_factor_min
       real sucrose_fraction

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      call fill_real_array (g_dlt_plant_wc, 0.0, max_part)
 
      tt = sum_between (begcane,now,g_tt_tot)
 
      cane_dmf_max = linear_interp_real (tt
     :                                  ,c_cane_dmf_tt
     :                                  ,c_cane_dmf_max
     :                                  ,c_num_cane_dmf)
 
      cane_dmf_min = linear_interp_real (tt
     :                                  ,c_cane_dmf_tt
     :                                  ,c_cane_dmf_min
     :                                  ,c_num_cane_dmf)
 
      stress_factor_min = min (g_swdef_stalk
     :                        ,g_nfact_stalk
     :                        ,g_temp_stress_stalk)
 
      cane_dmf = cane_dmf_max
     :         - stress_factor_min * (cane_dmf_max-cane_dmf_min)
 
      sucrose_fraction =
     :        divide (g_dlt_dm_green(sucrose)
     :               ,g_dlt_dm_green(sstem) + g_dlt_dm_green(sucrose)
     :               ,0.0)
 
      g_dlt_plant_wc(sstem) = divide (g_dlt_dm_green(sstem)
     :                               ,cane_dmf
     :                               ,0.0)
     :                      * (1.0 - sucrose_fraction)
 
      call pop_routine (myname)
      return
      end



*     ===========================================================
      subroutine sugar_sucrose_fraction
     :               (
     :                C_num_stress_factor_stalk
     :              , C_stress_Factor_stalk
     :              , C_sucrose_fraction_Stalk
     :              , G_swdef_stalk
     :              , G_nfact_stalk
     :              , g_temp_stress_stalk
     :              , sucrose_fraction
     :               )
*     ===========================================================
      implicit none
      include   'sugconst.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      INTEGER    c_num_stress_factor_Stalk ! (INPUT)
      REAL       C_stress_factor_stalk(*) ! (INPUT)
      REAL       C_Sucrose_fraction_stalk(*) ! (INPUT)
      REAL       G_swdef_stalk     ! (INPUT)
      REAL       G_nfact_stalk     ! (INPUT)
      REAL       G_temp_stress_stalk     ! (INPUT)
      real       sucrose_fraction      ! (OUTPUT) fraction of cane C
                                       ! partitioned to sucrose (0-1)

*+  Purpose
*     Returns the fraction of Cane C partioned to sucrose based
*     upon severity of water stress(cell expansion)

*+  Changes
*       240796 nih/mjr programmed and specified

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_sucrose_fraction')

*+  Local Variables
      real       stress_Factor_min     ! minimum of all 0-1 stress
                                       ! factors on stalk growth

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      stress_factor_min = min (g_swdef_stalk
     :                        ,g_nfact_stalk
     :                        ,g_temp_stress_stalk)
 
      ! this should give same results as old version for now
 
      sucrose_fraction = linear_interp_real (stress_Factor_min
     :                                      ,c_stress_factor_stalk
     :                                      ,c_sucrose_fraction_stalk
     :                                      ,c_num_stress_factor_Stalk
     :                                      )
 
      call bound_check_real_var (sucrose_fraction
     :                        , 0.0
     :                        , 1.0
     :                        , 'fraction of Cane C to sucrose')
 
      sucrose_fraction = bound (sucrose_fraction, 0.0, 1.0)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_graze ()
*     ===========================================================
      implicit none
      include   'const.inc'            ! lu_scr_sum, blank
      include   'convert.inc'          ! smm2sm
      include   'sugar.inc'
      include 'write.pub'                         
      include 'crp_comm.pub'                      
      include 'intrface.pub'                      
      include 'error.pub'                         

*+  Purpose
*       remove part of the green material as if grazed

*+  Changes
*     050996 nih specified and programmed

*+  Calls
      real       sugar_leaf_no_from_lai! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_graze')

*+  Local Variables
      real       c_eff                 ! fraction of C returned to soil
      real       dm_residue            ! dry matter going to residue
      real       fraction              ! fraction of green material grazed
      integer    leaf_no               ! index for leaves
      real       node_no_dead          ! number of dead or dying leaves
      real       n_eff                 ! fraction of N returned to soil
      real       n_residue             ! N going to residue
      integer    numvals               ! number of values found in array
      character  report*10             ! report flag
      character  string*150            ! output string
      real       grn_fr               ! fraction of bottom leaf that is dead
      integer    start_leaf            ! leaf to start grazing from
      real       fraction_removed      ! fraction of each leaf grazed

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      ! request and receive variables from owner-modules
c      call sugar_get_other_variables ()
 
      call collect_real_var ('fraction', '()'
     :                      , fraction, numvals, 0.0, 1.0)
 
      call collect_real_var ('n_eff', '()'
     :                      , n_eff, numvals, 0.0, 1.0)
 
      call collect_real_var ('c_eff', '()'
     :                      , c_eff, numvals, 0.0, 1.0)
 
      call collect_char_var_optional ('report', '()'
     :                               , report, numvals)
      if (numvals.eq.0) then
         report = 'no'
      else
      endif
 
      ! Note - I could use loops here but I want to be specific.
      dm_residue = 0.0
      n_Residue = 0.0
 
      g_dm_graze = g_dm_graze + g_dm_green(leaf)*fraction
      g_n_graze = g_n_graze + g_n_green(leaf)*fraction
      dm_residue = dm_residue + g_dm_green(leaf)*fraction*c_eff
      n_residue = n_residue + g_n_green(leaf)*fraction*n_eff
      g_dm_green(leaf) = g_dm_green(leaf) * (1. - fraction)
      g_n_green(leaf) = g_n_green(leaf) * (1. - fraction)
      g_plant_wc(leaf) = g_plant_wc(leaf) * (1. - fraction)
 
      g_dm_graze = g_dm_graze + g_dm_green(cabbage)*fraction
      g_n_graze = g_n_graze + g_n_green(cabbage)*fraction
      dm_residue = dm_residue + g_dm_green(cabbage)*fraction*c_eff
      n_residue = n_residue + g_n_green(cabbage)*fraction*n_eff
      g_dm_green(cabbage) = g_dm_green(cabbage) * (1. - fraction)
      g_n_green(cabbage) = g_n_green(cabbage) * (1. - fraction)
      g_plant_wc(cabbage) = g_plant_wc(cabbage) * (1. - fraction)
 
      g_dm_graze = g_dm_graze + g_dm_green(sstem)*fraction
      g_n_graze = g_n_graze + g_n_green(sstem)*fraction
      dm_residue = dm_residue + g_dm_green(sstem)*fraction*c_eff
      n_residue = n_residue + g_n_green(sstem)*fraction*n_eff
      g_dm_green(sstem)= g_dm_green(sstem) * (1. - fraction)
      g_n_green(sstem)= g_n_green(sstem) * (1. - fraction)
      g_plant_wc(sstem) = g_plant_wc(sstem) * (1. - fraction)
 
      g_dm_graze = g_dm_graze + g_dm_green(sucrose)*fraction
      g_n_graze = g_n_graze + g_n_green(sucrose)*fraction
      dm_residue = dm_residue + g_dm_green(sucrose)*fraction*c_eff
      n_residue = n_residue + g_n_green(sucrose)*fraction*n_eff
      g_dm_green(sucrose)= g_dm_green(sucrose) * (1. - fraction)
      g_n_green(sucrose)= g_n_green(sucrose) * (1. - fraction)
      g_plant_wc(sucrose) = g_plant_wc(sucrose) * (1. - fraction)
 
      call crop_top_residue (c_crop_type, dm_residue, N_residue)
 
      ! Now we need to update the leaf tracking info
 
      g_lai = g_lai * (1. - fraction)
 
         ! get highest senescing leaf
 
 
      node_no_dead = sugar_leaf_no_from_lai
     :               (
     :                G_leaf_area
     :              , G_plants
     :              , g_slai
     :               )
      start_leaf = int(node_no_dead + 1.)
      do 100 leaf_no = start_leaf, max_leaf
         if (leaf_no .eq. start_leaf) then
            grn_fr = 1.0 - mod(node_no_dead,1.)
         else
            grn_fr = 1.0
         endif
         fraction_removed = fraction * grn_fr
         g_leaf_area(leaf_no) = g_leaf_area(leaf_no)
     :                        *(1.-fraction_removed)
         g_leaf_dm (leaf_no) = g_leaf_dm (leaf_no)
     :                        *(1.-fraction_removed)
  100 continue
 
             ! report
 
      if (report.eq.'yes') then
         write(string,'(1x,A,f4.1,A,f4.2,A,f4.2,A)')
     :              'Grazing '
     :             ,fraction*100
     :             ,'% of green material (N_eff = '
     :             ,N_eff
     :             ,', C_eff = '
     :             ,C_eff
     :             ,')'
         call report_event(string)
      else
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_root_depth_init (Option)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'sugar.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Plant root depth calculations

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_root_depth_init')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
         call sugar_init_root_depth
     :               (
     :                G_dlayer
     :              , G_root_length
     :              , G_root_depth
     :              , g_dlt_root_depth
     :               )
                               !NOTE THIS IS STILL THE DELTA
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_root_dist (Option)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'sugar.inc'
      include 'crp_root.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Plant root distribution calculations

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_root_dist')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
          call cproc_root_length_growth1
     :               (
     :                C_specific_root_length
     :              , G_dlayer
     :              , G_dlt_dm_green(root)
     :              , G_dlt_root_length
     :              , G_dlt_root_depth
     :              , G_root_depth
     :              , G_root_length
     :              , g_plants
     :              , P_xf
     :              , C_num_sw_ratio
     :              , C_x_sw_ratio
     :              , C_y_sw_fac_root
     :              , c_x_plant_rld
     :              , c_y_rel_root_rate
     :              , c_num_plant_rld
     :              , G_dul_dep
     :              , G_sw_dep
     :              , P_ll_dep
     :              , max_layer
     :               )
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_water_supply (Option)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'sugar.inc'
      include 'crp_watr.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Plant water supply

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_water_supply')

*+  Local Variables
      integer layer

*- Implementation Section ----------------------------------
c+!!!!!!!!! check order dependency of deltas
      call push_routine (my_name)
 
 
      if (Option .eq. 1) then
 
       call cproc_sw_supply1 (
     :                        C_sw_dep_lb
     :                       ,G_dlayer
     :                       ,P_ll_dep
     :                       ,G_dul_dep
     :                       ,G_sw_dep
     :                       ,max_layer
     :                       ,g_root_depth
     :                       ,p_kl
     :                       ,g_sw_avail
     :                       ,g_sw_avail_pot
     :                       ,g_sw_supply
     :                       )
 
         if (g_uptake_source.eq.'apsim') then
 
            ! Use the water uptake values given by some other
            ! module in the APSIM system. (eg APSWIM)
            If (g_num_uptake_water.gt.0) then
               do 100 layer = 1, g_num_uptake_water
                  g_sw_supply (layer) =        g_uptake_water(layer)
  100          continue
            else
               call Fatal_Error (Err_Internal,
     :             'No soil water uptake information has been provided')
            endif
         endif
 
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_water_uptake (Option)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'sugar.inc'
      include 'crp_watr.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Plant water uptake

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_water_uptake')

*+  Local Variables
      integer layer

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
 
      if (Option .eq. 1) then
 
         if (g_uptake_source.eq.'calc') then
            call cproc_sw_uptake1 (g_num_layers
     :                            ,g_dlayer
     :                            ,g_root_depth
     :                            ,g_sw_demand
     :                            ,g_sw_supply
     :                            ,g_dlt_sw_dep)
 
         else
            ! Use the water uptake values given by some other
            ! module in the APSIM system. (eg APSWIM)
            If (g_num_uptake_water.gt.0) then
               do 100 layer = 1, g_num_uptake_water
                  g_dlt_sw_dep(layer) = -1.0 * g_uptake_water(layer)
  100          continue
            else
               call Fatal_Error (Err_Internal,
     :             'No soil water uptake information has been provided')
            endif
         endif
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_water_demand (Option)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'sugar.inc'
      include 'crp_watr.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Plant water demand

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_water_demand')

*- Implementation Section ----------------------------------
c+!!!!!!!!! check order dependency of deltas
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
         call cproc_sw_demand1 (
     :          g_dlt_dm_pot_rue,
     :          g_transp_eff,
     :          g_sw_demand)
 
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_light_supply (Option)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'sugar.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       light supply

*+  Changes
*      5/9/96 dph

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_light_supply')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
         call sugar_radn_int
     :               (
     :                C_extinction_coef
     :              , G_fr_intc_radn
     :              , G_lai
     :              , G_radn
     :              , g_radn_int
     :               )
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_bio_RUE (Option)
*     ===========================================================
      implicit none
      include     'const.inc'
      include     'sugar.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       biomass light

*+  Changes
*      5/9/96 dph

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_bio_RUE')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
         call sugar_dm_pot_rue
     :               (
     :                C_rue
     :              , G_current_stage
     :              , G_radn_int
     :              , G_nfact_photo
     :              , G_temp_stress_photo
     :              , G_oxdef_photo
     :              , g_dlt_dm_pot_rue
     :               )
 
         call sugar_dm_pot_rue_pot
     :               (
     :                C_rue
     :              , G_current_stage
     :              , G_radn_int
     :              , g_dlt_dm_pot_rue_pot
     :               )
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_dm_pot_rue
     :               (
     :                C_rue
     :              , G_current_stage
     :              , G_radn_int
     :              , G_nfact_photo
     :              , G_temp_stress_photo
     :              , G_oxdef_photo
     :              , dlt_dm_pot
     :               )
*     ===========================================================
      implicit none
      include   'sugconst.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      REAL       C_rue(*)              ! (INPUT)  radiation use efficiency (g dm
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_radn_int            ! (INPUT)
      REAL       G_nfact_photo         ! (INPUT)
      REAL       G_temp_stress_photo   ! (INPUT)
      REAL       G_oxdef_photo         ! (INPUT)
      real       dlt_dm_pot            ! (OUTPUT) potential dry matter
                                       ! (carbohydrate) production (g/m^2)

*+  Purpose
*       This routine calculates the potential biomass (carbohydrate)
*       production for conditions where soil supply is non-limiting.
*

*+  Changes
*       060495 nih taken from template

*+  Calls
      real       sugar_rue_reduction   ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_dm_pot_rue')

*+  Local Variables
      integer    current_phase         ! current phase number
      real       rue                   ! radiation use efficiency under
                                       ! no stress (g biomass/mj)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      current_phase = int (g_current_stage)
      rue = c_rue(current_phase)
     :    * sugar_rue_reduction
     :               (
     :                G_nfact_photo
     :              , G_temp_stress_photo
     :              , G_oxdef_photo
     :               )
 
         ! potential dry matter production with temperature
         ! and N content stresses is calculated.
         ! This is g of dry biomass produced per MJ of intercepted
         ! radiation under stressed conditions.
 
cnh      call sugar_radn_int (radn_int)
      dlt_dm_pot = rue * g_radn_int
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_dm_pot_rue_pot
     :               (
     :                C_rue
     :              , G_current_stage
     :              , G_radn_int
     :              , dlt_dm_pot
     :               )
*     ===========================================================
      implicit none
      include   'sugconst.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      REAL       C_rue(*)              ! (INPUT)  radiation use efficiency (g dm/mj)
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_radn_int            ! (INPUT)
      real       dlt_dm_pot            ! (OUTPUT) potential dry matter
                                       ! (carbohydrate) production (g/m^2)

*+  Purpose
*       This routine calculates the potential biomass (carbohydrate)
*       production for conditions where soil supply is non-limiting.
*

*+  Changes
*       060495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_dm_pot_rue_pot')

*+  Local Variables
      integer    current_phase         ! current phase number
      real       rue                   ! radiation use efficiency under
                                       ! no stress (g biomass/mj)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      current_phase = int (g_current_stage)
      rue = c_rue(current_phase)
 
         ! potential dry matter production with temperature
         ! and N content stresses is calculated.
         ! This is g of dry biomass produced per MJ of intercepted
         ! radiation under stressed conditions.
 
cnh      call sugar_radn_int (radn_int)
      dlt_dm_pot = rue * g_radn_int
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_transpiration_eff (Option)
*     ===========================================================
      implicit none
      include     'const.inc'
      include     'sugar.inc'
      include 'crp_watr.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Calculate today's transpiration efficiency from min and max
*       temperatures and converting mm water to g dry matter
*       (g dm/m^2/mm water)

*+  Changes
*      5/9/96 dph

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_transpiration_efficiency')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
         call cproc_transp_eff1(c_svp_fract, c_transp_eff_cf,
     :            g_current_stage,g_maxt, g_mint, g_transp_eff)
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



* ====================================================================
       subroutine sugar_temperature_stress
     :               (
     :                C_num_ave_temp
     :              , C_x_ave_temp
     :              , C_y_stress_photo
     :              , G_maxt
     :              , G_mint
     :              , tfac
     :               )
* ====================================================================
      implicit none
       include 'sugconst.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      INTEGER    C_num_ave_temp        ! (INPUT)  size_of of critical temperatur
      REAL       C_x_ave_temp(*)       ! (INPUT)  critical temperatures for phot
      REAL       C_y_stress_photo(*)   ! (INPUT)  Factors for critical temperatu
      REAL       G_maxt                ! (INPUT)  maximum air temperature (oC)
      REAL       G_mint                ! (INPUT)  minimum air temperature (oC)
       real tfac

*+  Purpose
*      Temperature stress factor for photosynthesis.

*+  Changes
*     14-01-1997 - neilh - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'sugar_temperature_stress')

*+  Local Variables
      real       ave_temp              ! mean temperature for the day (oC)

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
 
         ! now get the temperature stress factor that reduces
         ! photosynthesis (0-1)
 
      ave_temp = (g_maxt + g_mint) /2.0
 
      tfac = linear_interp_real (ave_temp
     :                          , c_x_ave_temp, c_y_stress_photo
     :                          , c_num_ave_temp)
      tfac = bound (tfac, 0.0, 1.0)
 
      call pop_routine (myname)
      return
      end



*     ===========================================================
      subroutine sugar_water_stress_pheno (Option)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'sugar.inc'
      include 'crp_watr.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*         Get current water stress factors (0-1)

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_water_stress_pheno')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (Option .eq. 1) then
         call crop_swdef_pheno(c_num_sw_avail_ratio,
     :           c_x_sw_avail_ratio, c_y_swdef_pheno, g_num_layers,
     :           g_dlayer, g_root_depth, g_sw_avail, g_sw_avail_pot,
     :           g_swdef_pheno)
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_water_stress_photo (Option)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'sugar.inc'
      include 'crp_watr.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*         Get current water stress factors (0-1)

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_water_stress_photo')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (Option .eq. 1) then
         call crop_swdef_photo(g_num_layers, g_dlayer, g_root_depth,
     :                   g_sw_demand,g_sw_supply, g_swdef_photo)
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_water_stress_expansion (Option)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'sugar.inc'
      include 'crp_watr.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*         Get current water stress factors (0-1)

*+  Changes
*     010994 jngh specified and programmed

*+  Calls
c      real       mungb_swdef           ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_water_stress_expansion')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (Option .eq. 1) then
         call crop_swdef_expansion(c_num_sw_demand_ratio,
     :           c_x_sw_demand_ratio, c_y_swdef_leaf,
     :           g_num_layers, g_dlayer,g_root_depth, g_sw_demand,
     :           g_sw_supply, g_swdef_expansion)
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_nit_stress_photo (Option)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'sugar.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*         Get current Nitrogen stress factors (0-1)

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_nit_stress_photo')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
         call sugar_nfact
     :               (
     :                G_dm_green
     :              , G_n_conc_crit
     :              , G_n_conc_min
     :              , G_n_green
     :              , c_k_nfact_photo
     :              , g_nfact_photo
     :               )
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_nit_stress_expansion (Option)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'sugar.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*         Get current Nitrogen stress factors (0-1)

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_nit_stress_expansion')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
         call sugar_nfact
     :               (
     :                G_dm_green
     :              , G_n_conc_crit
     :              , G_n_conc_min
     :              , G_n_green
     :              , c_k_nfact_expansion
     :              , g_nfact_expansion
     :               )
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_height (Option)
*     ===========================================================
      implicit none
      include   'sugar.inc'
      include   'const.inc'
      include 'crp_cnpy.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*     Canopy height.

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_height')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
         call cproc_canopy_height
     :               (
     :                G_canopy_height
     :              , c_x_stem_wt
     :              , c_y_height
     :              , c_num_stem_wt
     :              , G_dm_green
     :              , G_plants
     :              , sstem
     :              , g_dlt_canopy_height
     :               )
 
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_bio_actual (Option)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'sugar.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Simulate crop biomass processes.

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_bio_actual')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
         call sugar_dm_init
     :               (
     :                C_dm_cabbage_init
     :              , C_dm_leaf_init
     :              , C_dm_sstem_init
     :              , C_dm_sucrose_init
     :              , C_specific_root_length
     :              , G_current_stage
     :              , G_days_tot
     :              , G_dlayer
     :              , G_plants
     :              , G_root_length
     :              , g_dm_green, g_dm_plant_min
     :              , g_leaf_dm
     :               )
 
            ! use whichever is limiting
         g_dlt_dm = min (g_dlt_dm_pot_rue, g_dlt_dm_pot_te)
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_bio_partition (Option)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'sugar.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Partition biomass.

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_bio_partition')

*+  Local Variables
      real       leaf_no_today

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
         leaf_no_today = sum_between (emerg, now, g_leaf_no)
     :              + g_dlt_leaf_no
 
         call sugar_sla_min
     :               (
     :                C_num_sla_lfno
     :              , C_sla_lfno
     :              , C_sla_min
     :              , leaf_no_today, g_sla_min
     :               )
         call sugar_sucrose_fraction
     :               (
     :                c_num_stress_Factor_stalk
     :              , c_stress_factor_Stalk
     :              , c_sucrose_fraction_stalk
     :              , G_swdef_stalk
     :              , g_nfact_stalk
     :              , g_temp_stress_stalk
     :              , g_sucrose_fraction
     :               )
 
         call sugar_dm_partition
     :               (
     :                C_cane_fraction
     :              , C_leaf_cabbage_ratio
     :              , G_min_sstem_sucrose
     :              , C_ratio_root_shoot
     :              , C_sucrose_delay
     :              , G_current_stage
     :              , G_dm_green
     :              , G_sla_min
     :              , G_sucrose_fraction
     :              , G_tt_tot
     :              , g_dlt_dm
     :                          , g_dlt_lai_stressed
     :                          , g_dlt_dm_green
     :                          , g_partition_xs
     :               )
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_dm_partition_rules
     :               (
     :                C_cane_fraction
     :              , C_leaf_cabbage_ratio
     :              , G_min_sstem_sucrose
     :              , C_ratio_root_shoot
     :              , C_sucrose_delay
     :              , G_current_stage
     :              , G_dm_green
     :              , G_sla_min
     :              , G_sucrose_fraction
     :              , G_tt_tot
     :              , dlt_dm
     :                              , dlt_lai_pot
     :                              , dlt_dm_green
     :                              , partition_xs
     :               )
*     ===========================================================
      implicit none
      include   'convert.inc'
      include   'sugconst.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      REAL       C_cane_fraction       ! (INPUT)
      REAL       C_leaf_cabbage_ratio  ! (INPUT)  ratio of leaf wt to cabbage wt
      REAL       G_min_sstem_sucrose   ! (INPUT)
      REAL       C_ratio_root_shoot(*) ! (INPUT)  root:shoot ratio of new dm ()
      REAL       C_sucrose_delay       ! (INPUT)
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
      REAL       G_sla_min             ! (INPUT)  minimum specific leaf area (mm
      REAL       G_sucrose_fraction    ! (INPUT)  fraction of cane C going to su
      REAL       G_tt_tot(*)           ! (INPUT)  the sum of growing degree days
      real       dlt_dm                ! (INPUT) dry matter to partition
      real       dlt_lai_pot           ! (INPUT) increase in lai if
                                       ! unconstrained by carbon supply.
      real       dlt_dm_green (*)      ! (OUTPUT) actual biomass partitioned
                                       ! to plant parts (g/m^2)
      real       partition_xs          ! xs dry matter to that required
                                       ! to supply all demands. (g/m^2)

*+  Purpose
*       Partitions assimilate between individual plant pools.  The rules
*       for partitioning change with stage of crop growth.

*+  Changes
*       060495 nih taken from template
*       110196 nih added dlt_dm to argument list to make this routine
*                  more like a utility routine for partioning dry matter

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_dm_partition_rules')

*+  Local Variables
      integer    current_phase         ! current phase no.
      real       dlt_dm_green_tot      ! total of partitioned dm (g/m^2)
      real       dlt_leaf_max          ! max increase in leaf wt (g/m2)
      real       dlt_cane              ! increase in cane wt (g/m2)
      real       dlt_cane_min          ! min increase in cane wt (g/m2)
      real       tt_since_begcane      ! thermal time since the beginning
                                       ! of cane growth (deg days)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
         ! Root must be satisfied. The roots don't take any of the
         ! carbohydrate produced - that is for tops only.  Here we assume
         ! that enough extra was produced to meet demand. Thus the root
         ! growth is not removed from the carbo produced by the model.
 
         ! first we zero all plant component deltas
 
      call fill_real_array (dlt_dm_green, 0.0, max_part)
      partition_xs = 0.0
 
         ! now we get the root delta for all stages - partition scheme
         ! specified in coeff file
 
      current_phase = int (g_current_stage)
      dlt_dm_green(root) = c_ratio_root_shoot(current_phase)*dlt_dm
 
 
      dlt_leaf_max = divide (dlt_lai_pot
     :                      , g_sla_min*smm2sm
     :                      , 0.0)
 
      if (stage_is_between (emerg, begcane, g_current_stage)) then
            ! we have leaf and cabbage development only
 
         dlt_dm_green(leaf) = dlt_dm
     :                      * (1.0 - 1.0/(c_leaf_cabbage_ratio+1.0))
 
         dlt_dm_green(leaf) = u_bound (dlt_dm_green(leaf),dlt_leaf_max)
 
         dlt_dm_green(cabbage) = dlt_dm_green(leaf)/c_leaf_cabbage_ratio
 
         partition_xs       = dlt_dm
     :                      - dlt_dm_green(leaf)
     :                      - dlt_dm_green(cabbage)
 
         ! Put the excess dry matter in sstem
         dlt_dm_green (sstem) = partition_xs
 
      elseif (stage_is_between (begcane, crop_end
     :                        , g_current_stage)) then
 
         ! if leaf component makes leaves too thick extra goes to sstem
 
         dlt_cane_min = c_cane_fraction * dlt_dm
 
         dlt_dm_green(leaf) = (dlt_dm - dlt_cane_min)
     :                      * (1.0 - 1.0/(c_leaf_cabbage_ratio+1.0))
 
         dlt_dm_green(leaf) = u_bound (dlt_dm_green(leaf),dlt_leaf_max)
         dlt_dm_green(cabbage) = dlt_dm_green(leaf)/c_leaf_cabbage_ratio
         dlt_cane = dlt_dm - dlt_dm_green(leaf)
     :                       - dlt_dm_green(cabbage)
 
         tt_since_begcane = sum_between (begcane,now,g_tt_tot)
 
         if ((tt_since_begcane .gt. c_sucrose_delay)
     :                        .and.
     :       (g_dm_green(SStem).gt. g_min_sstem_sucrose))
     :   then
            ! the SStem pool gets (1 - c_sucrose_fraction) of the DEMAND
            ! for C. Extra C above the demand for cane goes only into
            ! the sucrose pool.
 
            dlt_dm_green(SStem) = dlt_cane_min
     :                          * (1.- g_sucrose_fraction)
            dlt_dm_green(Sucrose) = dlt_cane_min * g_sucrose_fraction
 
            partition_xs = dlt_cane - dlt_cane_min
            dlt_dm_green(Sucrose) = dlt_dm_green(Sucrose) + partition_xs
 
         else
            ! nih - should excess C go into sucrose here too even though
            ! we have not started into the sugar accumulation phase????
            dlt_dm_green(SStem) = dlt_cane
            partition_xs = dlt_cane - dlt_cane_min
 
         endif
 
      else
            ! no partitioning
      endif
 
cnh Due to small rounding errors I will say that small errors are ok
 
         ! do mass balance check - roots are not included
      dlt_dm_green_tot = sum_real_array (dlt_dm_green, max_part)
     :                 - dlt_dm_green(root)
 
      call bound_check_real_var (dlt_dm_green_tot
     :                        , dlt_dm - 1.e-6
     :                        , dlt_dm + 1.e-6
     :                        , 'dlt_dm_green_tot mass balance')
 
         ! check that deltas are in legal range
 
      call bound_check_real_array (dlt_dm_green
     :                        , -1.e-6
     :                        , dlt_dm + 1.e-6
     :                        , 'dlt_dm_green'
     :                        , max_part)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_dm_partition_pot
     :               (
     :                C_cane_fraction
     :              , C_leaf_cabbage_ratio
     :              , G_min_sstem_sucrose
     :              , C_ratio_root_shoot
     :              , C_sucrose_delay
     :              , G_current_stage
     :              , G_dm_green
     :              , G_sla_min
     :              , G_sucrose_fraction
     :              , G_tt_tot
     :              , dlt_dm
     :                              , dlt_lai_pot
     :                              , dlt_dm_green
     :                              , partition_xs
     :               )
*     ===========================================================
      implicit none
      include   'convert.inc'
      include   'sugconst.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      REAL       C_cane_fraction       ! (INPUT)
      REAL       C_leaf_cabbage_ratio  ! (INPUT)  ratio of leaf wt to cabbage wt
      REAL       G_min_sstem_sucrose   ! (INPUT)
      REAL       C_ratio_root_shoot(*) ! (INPUT)  root:shoot ratio of new dm ()
      REAL       C_sucrose_delay       ! (INPUT)
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
      REAL       G_sla_min             ! (INPUT)  minimum specific leaf area (mm
      REAL       G_sucrose_fraction    ! (INPUT)  fraction of cane C going to su
      REAL       G_tt_tot(*)           ! (INPUT)  the sum of growing degree days
      real       dlt_dm                ! (INPUT) dry matter to partition
      real       dlt_lai_pot           ! (INPUT) increase in lai if
                                       ! unconstrained by carbon supply.
      real       dlt_dm_green (*)      ! (OUTPUT) actual biomass partitioned
                                       ! to plant parts (g/m^2)
      real       partition_xs          ! xs dry matter to that required
                                       ! to supply all demands. (g/m^2)

*+  Purpose
*       Partitions assimilate between individual plant pools.  The rules
*       for partitioning change with stage of crop growth.

*+  Changes
*       060495 nih taken from template
*       110196 nih added dlt_dm to argument list to make this routine
*                  more like a utility routine for partioning dry matter

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_dm_partition_pot')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      call sugar_dm_partition_rules
     :               (
     :                C_cane_fraction
     :              , C_leaf_cabbage_ratio
     :              , G_min_sstem_sucrose
     :              , C_ratio_root_shoot
     :              , C_sucrose_delay
     :              , G_current_stage
     :              , G_dm_green
     :              , G_sla_min
     :              , G_sucrose_fraction
     :              , G_tt_tot
     :              , dlt_dm
     :                        ,dlt_lai_pot
     :                        ,dlt_dm_green
     :                        ,partition_xs
     :               )
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_leaf_area_stressed (Option)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'sugar.inc'
      include 'crp_cnpy.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Simulate potential stressed crop leaf area development - may
*       be limited by DM production in subsequent routine

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_leaf_area_potential')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
            ! Plant leaf development
      if (Option .eq. 1) then
 
         call cproc_leaf_area_stressed1 (
     :                                   g_dlt_lai_pot
     :                                  ,g_swdef_expansion
     :                                  ,g_nfact_expansion
     :                                  ,g_dlt_lai_stressed
     :                                  )
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_leaf_actual (Option)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'sugar.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Simulate actual crop leaf area development - checks that leaf area
*       development matches DM production.

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_leaf_actual')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
            ! limit the delta leaf area by carbon supply
         call sugar_leaf_area
     :               (
     :                G_dlt_dm_green
     :              , G_dlt_lai
     :              , G_dlt_lai_stressed
     :              , G_dlt_leaf_no
     :              , G_leaf_no
     :              , C_num_sla_lfno
     :              , C_sla_lfno
     :              , C_sla_max
     :               )
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_bio_retrans (Option)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'sugar.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Retranslocate biomass.

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_bio_retrans')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
         call sugar_dm_retranslocate
     :               (
     :                g_dlt_dm_green_retrans
     :               )
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_leaf_death_grass
     :               (
     :                C_green_leaf_no
     :              , G_current_stage
     :              , G_days_tot
     :              , G_dlt_leaf_no
     :              , G_leaf_no
     :              , G_node_no_dead
     :              , dlt_node_no_dead
     :               )
*     ===========================================================
      implicit none
      include   'sugconst.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      REAL       C_green_leaf_no       ! (INPUT)
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
      REAL       G_dlt_leaf_no         ! (INPUT)  fraction of oldest leaf expand
      REAL       G_leaf_no(*)          ! (INPUT)  number of fully expanded leave
      REAL       G_node_no_dead(*)     ! (INPUT)  no of dead leaves ()
      real       dlt_node_no_dead      ! (OUTPUT) new fraction of oldest
                                       ! green leaf

*+  Purpose
*       Return the fractional death of oldest green leaf.

*+  Changes
*     070495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_leaf_death')

*+  Local Variables
      real       leaf_no_today
      real       node_no_dead_today    ! total number of dead leaves today
      real       node_no_dead_yesterday ! total number of dead leaves
                                        ! yesterday
      real       total_leaf_no         ! total number of leaves today

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      node_no_dead_yesterday = sum_between (emerg, now, g_node_no_dead)
 
      if (stage_is_between (emerg, crop_end, g_current_stage)) then
 
         ! this approach won't work if the growing point gets killed
         ! we will require an approach that integrates the app rate
         ! function to create a dlfno vs tt curve.
         ! this is quick and dirty to allow testing of green leaf
         ! approach
 
         leaf_no_today = sum_between (emerg, now, g_leaf_no)
     :                 + g_dlt_leaf_no
         node_no_dead_today = leaf_no_today - c_green_leaf_no
         node_no_dead_today = l_bound(node_no_dead_today,0.0)
 
 
      elseif (on_day_of (crop_end
     :                 , g_current_stage, g_days_tot)) then
 
         total_leaf_no = sum_between (emerg, now, g_leaf_no)
         node_no_dead_today = total_leaf_no
 
      else
         node_no_dead_today = 0.0
      endif
 
      node_no_dead_today = bound (node_no_dead_today
     :                           , node_no_dead_yesterday
     :                           , real(max_leaf))
      dlt_node_no_dead = node_no_dead_today - node_no_dead_yesterday
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_leaf_area_sen_age0
     :               (
     :                G_dlt_node_no_dead
     :              , G_lai
     :              , G_leaf_area
     :              , G_node_no_dead
     :              , G_plants
     :              , G_slai
     :              , G_node_no_detached
     :              , C_leaf_no_at_emerg
     :              , dlt_slai_age
     :               )
*     ===========================================================
      implicit none
      include   'convert.inc'
      include   'sugconst.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      REAL       G_dlt_node_no_dead    ! (INPUT)  fraction of oldest green leaf
      REAL       G_lai                 ! (INPUT)  live plant green lai
      REAL       G_leaf_area(*)        ! (INPUT)  leaf area of each leaf (mm^2)
      REAL       G_node_no_dead(*)     ! (INPUT)  no of dead leaves ()
      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
      REAL       G_slai                ! (INPUT)  area of leaf that senesces fro
      REAL       G_node_no_detached    ! (INPUT)  number of detached leaves
      REAL       C_leaf_no_at_emerg    ! (INPUT)  number of leaves at emergence
      real       dlt_slai_age          ! (OUTPUT) new senesced lai from
                                       ! phasic devel.

*+  Purpose
*       Return the lai that would senesce on the
*       current day due to ageing

*+  Changes
*     070495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_leaf_area_sen_age0')

*+  Local Variables
      real       dlt_leaf_area         ! potential senesced leaf area from
                                       ! highest leaf no. senescing (mm^2)
      integer    node_no_dead          ! current leaf number dying ()
      real       slai_age              ! lai senesced by natural ageing
      real       dead_fr_highest_dleaf

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
         ! now calculate the leaf senescence
         ! due to normal phenological (phasic) development
 
         ! get highest leaf no. senescing today
 
c      leaf_no_dead = int (1.0
c     :                   + sum_between (emerg, now, g_leaf_no_dead))
 
      ! note that the first leaf record really contains
      ! 1+c_leaf_no_at_emerg leaves in it - not 1.
      node_no_dead = int (1.0
     :                   + sum_between (emerg, now, g_node_no_dead))
     :                   - g_node_no_detached
     :                   - c_leaf_no_at_emerg
      node_no_dead = max(node_no_dead,1)
 
      dead_fr_highest_dleaf = mod(
     :                   1.0 + sum_between (emerg, now, g_node_no_dead)
     :                   - g_node_no_detached
     :                   - c_leaf_no_at_emerg
     :                   , 1.0)
 
         ! get area senesced from highest leaf no.
 
      dlt_leaf_area = mod (g_dlt_node_no_dead, 1.0)
     :                 * g_leaf_area(node_no_dead)
 
      slai_age = (sum_real_array (g_leaf_area, node_no_dead - 1)
     :         + dead_fr_highest_dleaf * g_leaf_area (node_no_dead)
     :         + dlt_leaf_area)
     :         * smm2sm * g_plants
 
      dlt_slai_age = bound (slai_age - g_slai, 0.0, g_lai)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_leaf_area_sen_actual (Option)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'sugar.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Return the lai that senesces on the current day

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_leaf_area_sen_actual')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
         ! now take largest of deltas
         g_dlt_slai = max (g_dlt_slai_age
     :                   , g_dlt_slai_light
     :                   , g_dlt_slai_water
     :                   , g_dlt_slai_frost)
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_sen_nit (Option)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'sugar.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Simulate plant nitrogen senescence.

*+  Changes
*      091294 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_sen_nit')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
         call sugar_N_senescence
     :               (
     :                C_n_cabbage_sen_conc
     :              , C_n_leaf_sen_conc
     :              , C_n_root_sen_conc
     :              , G_dlt_dm_senesced
     :              , g_dlt_N_senesced
     :               )
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_sen_bio (Option)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'sugar.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Simulate plant senescence.

*+  Changes
*      091294 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_sen_bio')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
         call sugar_dm_senescence
     :               (
     :                C_dm_root_sen_frac
     :              , C_leaf_cabbage_ratio
     :              , C_cabbage_sheath_fr
     :              , G_dlt_dm_green
     :              , G_dlt_lai
     :              , G_dlt_slai
     :              , G_dm_green
     :              , G_dm_senesced
     :              , G_lai
     :              , G_leaf_dm
     :              , G_plants
     :              , G_slai
     :              , G_leaf_area
     :              , g_dlt_dm_senesced
     :               )
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_sen_root_length (Option)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'sugar.inc'
      include 'crp_root.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Simulate plant nitrogen senescence.

*+  Changes
*      091294 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_sen_root_length')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
          call cproc_root_length_senescence1
     :               (
     :                C_specific_root_length
     :              , G_dlayer
     :              , G_dlt_dm_senesced(root)
     :              , G_root_length
     :              , G_root_depth
     :              , G_dlt_root_length_senesced
     :              , max_layer
     :               )
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_nit_init (Option)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'sugar.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Initialise plant nitrogen.

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_nit_init')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
         call sugar_N_init
     :               (
     :                C_n_cabbage_init_conc
     :              , C_n_leaf_init_conc
     :              , C_n_root_init_conc
     :              , C_n_sstem_init_conc
     :              , G_current_stage
     :              , G_days_tot
     :              , G_dm_green
     :              , g_N_green
     :               )
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_nit_supply (Option)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'sugar.inc'
      include 'data.pub'                          
      include 'crp_nitn.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Find nitrogen supply.

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_nit_supply')

*+  Local Variables
      real fixation_determinant

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
            ! find potential N uptake (supply, available N)
      if (Option .eq. 1) then
         fixation_determinant = sum_real_array(g_dm_green, max_part)
     :                        - g_dm_green(root)
 
         call cproc_n_supply2 (
     :            g_dlayer
     :          , max_layer
     :          , g_dlt_sw_dep
     :          , g_NO3gsm
     :          , g_NO3gsm_min
     :          , g_root_depth
     :          , g_sw_dep
     :          , g_NO3gsm_mflow_avail
     :          , g_sw_avail
     :          , g_sw_avail_pot
     :          , g_NO3gsm_diffn_pot
     :          , G_current_stage
     :          , C_n_fix_rate
     :          , fixation_determinant
     :          , G_swdef_fixation
     :          , g_N_fix_pot
     :          )
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_nit_retrans (Option)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'sugar.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Do nitrogen retranslocation.

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_nit_retrans')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
         call sugar_N_retranslocate
     :               (
     :                G_dm_green
     :              , G_n_conc_min
     :              , G_n_green
     :              , g_dlt_N_retrans
     :               )
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_nit_demand (Option)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'sugar.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Find nitrogen demand.

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_nit_demand')
*
      integer num_demand_parts
      parameter (num_demand_parts = 4)

*+  Initial Data Values
      integer demand_parts(num_demand_parts)
      data demand_parts /root,leaf,cabbage,sstem/
      save /demand_parts/

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
         ! Use estimate from prepare stage
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_nit_uptake (Option)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'convert.inc'
      include   'sugar.inc'
      include 'crp_nitn.pub'                      
      include 'crp_comm.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Find nitrogen uptake.

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_nit_uptake')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (g_uptake_source .eq. 'apsim') then
        ! NIH - note that I use a -ve conversion
         ! factor FOR NOW to make it a delta.
         call crop_get_ext_uptakes(
     :                 g_uptake_source   ! uptake flag
     :                ,c_crop_type       ! crop type
     :                ,'no3'             ! uptake name
     :                ,-kg2gm/ha2sm      ! unit conversion factor
     :                ,0.0               ! uptake lbound
     :                ,100.0             ! uptake ubound
     :                ,g_dlt_no3gsm      ! uptake array
     :                ,max_layer         ! array dim
     :                )
 
      elseif (Option .eq. 1) then
 
          call cproc_N_uptake1
     :               (
     :                C_no3_diffn_const
     :              , G_dlayer
     :              , max_layer
     :              , G_no3gsm_diffn_pot
     :              , G_no3gsm_mflow_avail
     :              , G_N_fix_pot
     :              , c_n_supply_preference
     :              , G_n_demand
     :              , g_n_demand !sugar does not have n_max
     :              , max_part
     :              , G_root_depth
     :              , g_dlt_NO3gsm
     :               )
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_nit_partition (Option)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'sugar.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Find nitrogen partitioning.

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_nit_partition')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
         call sugar_N_partition
     :               (
     :                G_dlayer
     :              , G_dlt_no3gsm
     :              , G_n_demand
     :              , G_root_depth
     :              , g_dlt_N_green
     :               )
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_cleanup ()
*     ===========================================================
      implicit none
       include 'sugar.inc'
      include 'error.pub'                         

*+  Purpose
*       cleanup after crop processes

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_cleanup')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      call sugar_update
     :               (
     :                G_canopy_height
     :              , G_cnd_photo
     :              , G_cswd_expansion
     :              , G_cswd_pheno
     :              , G_cswd_photo
     :              , G_dlt_canopy_height
     :              , G_dlt_dm
     :              , G_dlt_dm_dead_detached
     :              , G_dlt_dm_detached
     :              , G_dlt_dm_green
     :              , G_dlt_dm_green_retrans
     :              , G_dlt_dm_senesced
     :              , G_dlt_dm_realloc
     :              , G_dlt_lai
     :              , G_dlt_leaf_no
     :              , g_dlt_node_no
     :              , G_dlt_node_no_dead
     :              , G_dlt_n_dead_detached
     :              , G_dlt_n_detached
     :              , G_dlt_n_green
     :              , G_dlt_n_retrans
     :              , G_dlt_n_senesced
     :              , G_dlt_n_realloc
     :              , G_dlt_plants
     :              , G_dlt_plant_wc
     :              , G_dlt_root_length
     :              , G_dlt_root_length_senesced
     :              , G_dlt_root_depth
     :              , G_dlt_slai
     :              , G_dlt_slai_detached
     :              , G_dlt_stage
     :              , G_dlt_tlai_dead_detached
     :              , G_dm_dead
     :              , G_dm_green
     :              , G_dm_plant_top_tot
     :              , G_dm_senesced
     :              , G_lai
     :              , G_leaf_area
     :              , G_leaf_dm
     :              , G_leaf_no
     :              , g_node_no
     :              , G_node_no_dead
     :              , G_nfact_photo
     :              , G_n_conc_crit
     :              , G_n_conc_min
     :              , G_n_dead
     :              , G_n_green
     :              , G_n_senesced
     :              , G_plants
     :              , G_plant_wc
     :              , G_previous_stage
     :              , G_root_length
     :              , G_root_depth
     :              , G_slai
     :              , G_swdef_expansion
     :              , G_swdef_pheno
     :              , G_swdef_photo
     :              , G_tlai_dead
     :              , C_n_conc_crit_root
     :              , C_n_conc_min_root
     :              , C_x_stage_code
     :              , C_y_n_conc_crit_cabbage
     :              , C_y_n_conc_crit_cane
     :              , C_y_n_conc_crit_leaf
     :              , C_y_n_conc_min_cabbage
     :              , C_y_n_conc_min_cane
     :              , C_y_n_conc_min_leaf
     :              , G_current_stage
     :              , C_stage_code_list
     :              , G_phase_tt
     :              , G_tt_tot
     :              , G_node_no_detached
     :              , C_leaf_no_at_emerg
     :               )
      call sugar_totals
     :               (
     :                G_current_stage
     :              , G_days_tot
     :              , G_day_of_year
     :              , G_dlayer
     :              , G_dlt_sw_dep
     :              , G_dm_green
     :              , G_isdate
     :              , G_lai
     :              , G_lai_max
     :              , G_n_conc_act_stover_tot
     :              , G_n_demand
     :              , G_n_demand_tot
     :              , G_n_green
     :              , G_root_depth
     :              , G_transpiration_tot
     :               )
      call sugar_event
     :               (
     :                C_stage_code_list
     :              , C_stage_names
     :              , G_current_stage
     :              , G_days_tot
     :              , G_day_of_year
     :              , G_dlayer
     :              , G_dm_dead
     :              , G_dm_green
     :              , G_dm_senesced
     :              , G_lai
     :              , G_n_green
     :              , G_root_depth
     :              , G_sw_dep
     :              , G_year
     :              , P_ll_dep
     :               )
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_totals
     :               (
     :                G_current_stage
     :              , G_days_tot
     :              , G_day_of_year
     :              , G_dlayer
     :              , G_dlt_sw_dep
     :              , G_dm_green
     :              , G_isdate
     :              , G_lai
     :              , G_lai_max
     :              , G_n_conc_act_stover_tot
     :              , G_n_demand
     :              , G_n_demand_tot
     :              , G_n_green
     :              , G_root_depth
     :              , G_transpiration_tot
     :               )
*     ===========================================================
      implicit none
      include   'sugconst.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
      INTEGER    G_day_of_year         ! (INPUT)  day of year
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL       G_dlt_sw_dep(*)       ! (INPUT)  water uptake in each layer (mm water)
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass) (g/m^2)
      INTEGER    G_isdate              ! (INPUT)  flowering day number
      REAL       G_lai                 ! (INPUT)  live plant green lai
      REAL       G_lai_max             ! (INPUT)  maximum lai - occurs at flowering
      REAL       G_n_conc_act_stover_tot ! (INPUT)  sum of tops actual N concentration (g N/g biomass)
      REAL       G_n_demand(*)         ! (INPUT)  plant nitrogen demand (g/m^2)
      REAL       G_n_demand_tot        ! (INPUT)  sum of N demand since last output (g/m^2)
      REAL       G_n_green(*)          ! (INPUT)  plant nitrogen content (g N/m^2)
      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
      REAL       G_transpiration_tot   ! (INPUT)  cumulative transpiration (mm)

*+  Purpose
*         Collect totals of crop variables for output

*+  Changes
*     070495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_crop_totals')

*+  Local Variables
      real       N_conc_stover         ! tops actual N concentration
                                       ! (g N/g part)
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      real       N_green_demand        ! plant N demand (g/m^2)

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
 
cnh I have removed most of the variables because they were either calculated
cnh wrongly or irrelevant.
 
             ! get totals
      N_conc_stover = divide ((g_N_green(leaf)
     :                       + g_N_green(sstem)
     :                       + g_N_green(cabbage)
     :                       + g_N_green(sucrose))
     :                      , (g_dm_green(leaf)
     :                       + g_dm_green(sstem)
     :                       + g_dm_green(cabbage)
     :                       + g_dm_green(sucrose))
     :                       , 0.0)
 
          ! note - g_N_conc_crit should be done before the stages change
cnh wrong!!!
c      N_conc_stover_crit = (g_N_conc_crit(leaf) + g_N_conc_crit(stem))
c     :                   * 0.5
      N_green_demand = sum_real_array (g_N_demand, max_part)
 
      deepest_layer = find_layer_no (g_root_depth, g_dlayer, max_layer)
 
      if (on_day_of (sowing, g_current_stage, g_days_tot)) then
         g_transpiration_tot =
     :           - sum_real_array (g_dlt_sw_dep, deepest_layer)
         g_N_conc_act_stover_tot = N_conc_stover
         g_N_demand_tot = N_green_demand
 
      else
         g_transpiration_tot = g_transpiration_tot
     :                       + (-sum_real_array (g_dlt_sw_dep
     :                                         , deepest_layer))
         g_N_conc_act_stover_tot = N_conc_stover
         g_N_demand_tot = g_N_demand_tot + N_green_demand
      endif
 
      g_lai_max = max (g_lai_max, g_lai)
      if (on_day_of (flowering, g_current_stage, g_days_tot)) then
         g_isdate = g_day_of_year
      else
      endif
 
 
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_water_content_cane (Option)
*     ===========================================================
      implicit none
      include     'const.inc'
      include     'sugar.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       bio transpiration efficiency

*+  Changes
*      5/9/96 dph

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_water_content_cane')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
         call sugar_water_content
     :               (
     :                C_cane_dmf_tt
     :              , C_cane_dmf_min
     :              , C_cane_dmf_max
     :              , C_num_cane_dmf
     :              , C_cane_dmf_rate
     :              , g_swdef_stalk
     :              , g_nfact_stalk
     :              , g_temp_stress_stalk
     :              , G_dlt_dm_green
     :              , g_dm_green
     :              , G_dlt_plant_wc
     :              , G_plant_wc
     :              , G_tt_tot)
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_failure_germination
     :               (
     :                C_days_germ_limit
     :              , G_current_stage
     :              , G_days_tot
     :              , G_plants
     :              , dlt_plants
     :               )
*     ===========================================================
      implicit none
      include   'const.inc'            ! new_line
      include   'sugconst.inc'
      include 'data.pub'                          
      include 'science.pub'                       
      include 'write.pub'                         
      include 'error.pub'                         

*+  Sub-Program Arguments
      REAL       C_days_germ_limit     ! (INPUT)  maximum days allowed after sowing for germination to take place (days)
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
      real       dlt_plants            ! (OUTPUT) change in plant number

*+  Purpose
*      Crop failure from lack of germination.

*+  Changes
*       290994 jngh specified and programmed
*       110695 psc  added plant death from high soil temp
*       100795 jngh moved sugar_kill crop to end of routine

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_failure_germination')

*+  Local Variables
      character  string*200            ! output string

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (stage_is_between (sowing, sprouting, g_current_stage)
     :   .and. sum_between (sowing, now, g_days_tot)
     :         .ge.c_days_germ_limit) then
 
         dlt_plants = - g_plants
 
         write (string, '(3a, i4, a)')
     :                 ' crop failure because of lack of'
     :                  ,new_line
     :                  ,'         germination within'
     :                  , c_days_germ_limit
     :                  , ' days of sowing'
         call write_string (lu_scr_sum, string)
c         call sugar_kill_crop ()
 
      else
         dlt_plants = 0.0
 
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_failure_emergence
     :               (
     :                C_tt_emerg_limit
     :              , G_current_stage
     :              , G_plants
     :              , G_tt_tot
     :              , dlt_plants
     :               )
*     ===========================================================
      implicit none
      include   'const.inc'            ! new_line
      include   'sugconst.inc'
      include 'data.pub'                          
      include 'science.pub'                       
      include 'write.pub'                         
      include 'error.pub'                         

*+  Sub-Program Arguments
      REAL       C_tt_emerg_limit      ! (INPUT)  maximum degree days allowed for emergence to take place (deg day)
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
      REAL       G_tt_tot(*)           ! (INPUT)  the sum of growing degree days for a phenological stage (oC d)
      real       dlt_plants            ! (OUTPUT) change in plant number

*+  Purpose
*      Crop failure from lack of emergence.

*+  Changes
*       290994 jngh specified and programmed
*       110695 psc  added plant death from high soil temp
*       100795 jngh moved sugar_kill crop to end of routine

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_failure_emergence')

*+  Local Variables
      character  string*200            ! output string

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (stage_is_between (sprouting, emerg, g_current_stage)
     :       .and. sum_between (sprouting, now, g_tt_tot)
     :       .gt.c_tt_emerg_limit) then
 
         dlt_plants = - g_plants
 
         write (string, '(a)')
     :                 ' failed emergence due to deep planting'
         call write_string (lu_scr_sum, string)
c         call sugar_kill_crop ()
 
      else
         dlt_plants = 0.0
 
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_failure_leaf_sen
     :               (
     :                G_current_stage
     :              , G_lai
     :              , G_plants
     :              , dlt_plants
     :               )
*     ===========================================================
      implicit none
      include   'const.inc'            ! new_line
      include   'sugconst.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'write.pub'                         
      include 'error.pub'                         

*+  Sub-Program Arguments
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_lai                 ! (INPUT)  live plant green lai
      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
      real       dlt_plants            ! (OUTPUT) change in plant number

*+  Purpose
*      Determine plant death from all leaf area senescing.

*+  Changes
*       290994 jngh specified and programmed
*       110695 psc  added plant death from high soil temp
*       100795 jngh moved sugar_kill crop to end of routine

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_failure_leaf_sen')

*+  Local Variables
      character  string*200            ! output string

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (reals_are_equal (g_lai, 0.0)
     :       .and. stage_is_between (emerg, crop_end
     :                             , g_current_stage)) then
 
         dlt_plants = - g_plants
 
         write (string, '(3a)')
     :                ' crop failure because of total leaf senescence.'
         call write_string (lu_scr_sum, string)
c         call sugar_kill_crop ()
 
      else
         dlt_plants = 0.0
 
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_death_actual
     :               (
     :                G_dlt_plants_death_drought
     :              , G_dlt_plants_failure_emergence
     :              , G_dlt_plants_failure_germ
     :              , G_dlt_plants_failure_leaf_sen
     :              , G_dlt_plants_death_lodging
     :              , dlt_plants
     :               )
*     ===========================================================
      implicit none
      include   'const.inc'            ! new_line
      include   'sugconst.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      REAL       G_dlt_plants_death_drought ! (INPUT)
      REAL       G_dlt_plants_failure_emergence ! (INPUT)
      REAL       G_dlt_plants_failure_germ ! (INPUT)
      REAL       G_dlt_plants_failure_leaf_sen ! (INPUT)
      REAL       G_dlt_plants_death_lodging ! (INPUT)
      real       dlt_plants            ! (OUTPUT) change in plant number

*+  Purpose
*      Determine actual plant death.

*+  Changes
*       290994 jngh specified and programmed
*       110695 psc  added plant death from high soil temp
*       100795 jngh moved mungb_kill crop to end of routine

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_death_actual')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
 
      dlt_plants = min (g_dlt_plants_failure_germ
     :                , g_dlt_plants_failure_emergence
     :                , g_dlt_plants_failure_leaf_sen
     :                , g_dlt_plants_death_drought
     :                , G_dlt_plants_death_lodging)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_death_drought
     :               (
     :                C_leaf_no_crit
     :              , C_swdf_photo_limit
     :              , C_swdf_photo_rate
     :              , G_cswd_photo
     :              , G_leaf_no
     :              , G_plants
     :              , G_swdef_photo
     :              , dlt_plants
     :               )
*     ===========================================================
      implicit none
      include   'const.inc'            ! new_line
      include   'sugconst.inc'
      include 'data.pub'                          
      include 'write.pub'                         
      include 'error.pub'                         

*+  Sub-Program Arguments
      REAL       C_leaf_no_crit        ! (INPUT)  critical number of leaves below which portion of the crop may die due to water stress
      REAL       C_swdf_photo_limit    ! (INPUT)  critical cumulative photosynthesis water stress above which the crop partly fails (unitless)
      REAL       C_swdf_photo_rate     ! (INPUT)  rate of plant reduction with photosynthesis water stress
      REAL       G_cswd_photo(*)       ! (INPUT)  cumulative water stress type 1
      REAL       G_leaf_no(*)          ! (INPUT)  number of fully expanded leaves ()
      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
      REAL       G_swdef_photo         ! (INPUT)
      real       dlt_plants            ! (OUTPUT) change in plant number

*+  Purpose
*      Determine plant death from drought.

*+  Changes
*       290994 jngh specified and programmed
*       110695 psc  added plant death from high soil temp
*       100795 jngh moved sugar_kill crop to end of routine

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_death_drought')

*+  Local Variables
      real       cswd_photo            ! cumulative water stress for photoperiod
      real       leaf_no               ! number of leaves
      real       killfr                ! fraction of crop population to kill
      character  string*200            ! output string

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      cswd_photo = sum_between (emerg, crop_end, g_cswd_photo)
      leaf_no = sum_between (emerg, now, g_leaf_no)
 
      if (leaf_no.lt.c_leaf_no_crit
     :       .and. cswd_photo.gt.c_swdf_photo_limit
     :       .and. g_swdef_photo.lt.1.0) then
 
         killfr = c_swdf_photo_rate* (cswd_photo - c_swdf_photo_limit)
         killfr = bound (killfr, 0.0, 1.0)
         dlt_plants = - g_plants*killfr
 
         write (string, '(a, i4, a)')
     :          'plant_kill.',
     :          nint (killfr*100.0)
     :         , '% failure because of water stress.'
 
         call report_event (string)
 
      else
         dlt_plants = 0.0
 
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_N_partition
     :               (
     :                G_dlayer
     :              , G_dlt_no3gsm
     :              , G_n_demand
     :              , G_root_depth
     :              , dlt_N_green
     :               )
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'convert.inc'          ! gm2kg, sm2ha
      include   'sugconst.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL       G_dlt_no3gsm(*)       ! (INPUT)  actual NO3 uptake from soil (g
      REAL       G_n_demand(*)         ! (INPUT)  plant nitrogen demand (g/m^2)
      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
      real       dlt_N_green(max_part) ! (OUTPUT) actual plant N uptake
                                       ! into each plant part (g/m^2)

*+  Purpose
*       Return actual plant nitrogen uptake to each plant part and from
*       each soil layer.

*+  Changes
*       060495 nih taken from template
*       130396 nih added fix to stop N above critical conc evaporating

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_N_partition')

*+  Local Variables
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      real       plant_part_fract      ! fraction of nitrogen to use (0-1)
                                       ! for plant part
      integer    part                  ! plant part number
      real       N_demand              ! total nitrogen demand (g/m^2)
      real       N_uptake_sum          ! total plant N uptake (g/m^2)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      call fill_real_array (dlt_N_green, 0.0, max_part)
      deepest_layer = find_layer_no(g_root_depth, g_dlayer, max_layer)
 
               ! find proportion of uptake to be
               ! distributed to each plant part and distribute it.
      N_uptake_sum = - sum_real_array (g_dlt_NO3gsm, deepest_layer)
      N_demand = sum_real_array (g_N_demand, max_part)
 
      ! Partition N, according to relative demand, to each plant
      ! part but do not allow supply to exceed demand.  Any excess
      ! supply is to go into cane. - NIH 13/3/96
 
      do 1300 part = 1, max_part
         plant_part_fract = divide (g_N_demand(part), N_demand, 0.0)
         dlt_N_green(part) = min(N_uptake_sum, N_demand)
     :                     * plant_part_fract
1300  continue
 
      if (N_uptake_sum.gt.N_demand) then
         dlt_N_green(sstem) = dlt_N_green(sstem)
     :                      + (N_uptake_sum - N_demand)
      else
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_water_log (Option)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'sugar.inc'
      include 'crp_watr.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*         Get current water stress factors (0-1)

*+  Changes
*     010994 jngh specified and programmed

*+  Calls
c      real       mungb_swdef           ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_water_log')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (Option .eq. 1) then
          call crop_oxdef_photo1
     :               (
     :                C_num_oxdef_photo
     :              , C_oxdef_photo
     :              , C_oxdef_photo_rtfr
     :              , G_ll15_dep
     :              , G_sat_dep
     :              , G_sw_dep
     :              , G_dlayer
     :              , G_root_length
     :              , G_root_depth
     :              , max_layer
     :              , g_oxdef_photo
     :               )
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_water_stress_stalk (Option)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'sugar.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*         Get current water stress factors (0-1)

*+  Changes
*     010994 jngh specified and programmed

*+  Calls
c      real       mungb_swdef           ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_water_stress_stalk')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (Option .eq. 1) then
         call sugar_swdef_demand_ratio
     :               (
     :                C_num_demand_ratio_stalk
     :              , C_x_demand_ratio_stalk
     :              , C_y_swdef_stalk
     :              , G_dlayer
     :              , G_root_depth
     :              , G_sw_demand
     :              , G_sw_supply
     :              , g_swdef_stalk
     :               )
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_temp_stress_photo (Option)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'sugar.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*         Get current temperature stress factors (0-1)

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_temp_stress_photo')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
cnh I made it a subroutine like all the rest
          call sugar_temperature_stress
     :               (
     :                C_num_ave_temp
     :              , C_x_ave_temp
     :              , C_y_stress_photo
     :              , G_maxt
     :              , G_mint
     :              , g_temp_stress_photo
     :               )
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_temp_stress_stalk (Option)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'sugar.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*         Get current temperature stress factors (0-1)

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_temp_stress_stalk')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
cnh I made it a subroutine like all the rest
          call sugar_temperature_stress
     :               (
     :                C_num_ave_temp_stalk
     :              , C_x_ave_temp_stalk
     :              , C_y_stress_stalk
     :              , G_maxt
     :              , G_mint
     :              , g_temp_stress_stalk
     :               )
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_nit_stress_stalk (Option)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'sugar.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*         Get current Nitrogen stress factors (0-1)

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_nit_stress_stalk')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
         call sugar_nfact
     :               (
     :                G_dm_green
     :              , G_n_conc_crit
     :              , G_n_conc_min
     :              , G_n_green
     :              , c_k_nfact_stalk
     :              , g_nfact_stalk
     :               )
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_nfact
     :               (
     :                G_dm_green
     :              , G_n_conc_crit
     :              , G_n_conc_min
     :              , G_n_green
     :              , k_nfact
     :              , nfact
     :               )
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'sugconst.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
      REAL       G_n_conc_crit(*)      ! (INPUT)  critical N concentration (g N/
      REAL       G_n_conc_min(*)       ! (INPUT)  minimum N concentration (g N/g
      REAL       G_n_green(*)          ! (INPUT)  plant nitrogen content (g N/m^
      REAL       k_nfact               ! (INPUT)  k value for stress factor
      real      nfact                 ! (OUTPUT) N stress factor

*+  Purpose
*     The concentration of Nitrogen in leaves is used to derive a
*     series of Nitrogen stress indices.  The stress indices for
*     photosynthesis and cell expansion are calculated from today's
*     relative nutritional status between a critical and minimum
*     leaf Nitrogen concentration.

*+  Changes
*     060495 nih taken from template
*     090895 bak reestablished N deficiency routines based on leaf N

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_nfact')

*+  Local Variables
cbak      real       N_conc_stover         ! tops (stover) actual N concentratio
                                       ! (0-1)
      real       N_conc_leaf           ! leaf actual N concentration
                                       ! (0-1)
cbak      real       dm_stover             ! tops (stover) plant weight (g/m^2)
      real       N_def                 ! N factor (0-1)
cbak      real       N_cabbage_crit
cbak      real       N_cabbage_min
cbak      real       N_conc_stover_crit    ! tops (stover) critical N concentrat
                                       ! (0-1)
cbak      real       N_conc_stover_min     ! tops (stover) minimum N concentrati
                                       ! (0-1)
      real       N_conc_leaf_crit    ! tops (stover) critical N concentration
                                       ! (0-1)
      real       N_conc_leaf_min     ! tops (stover) minimum N concentration
                                       ! (0-1)
*
      real       N_leaf_crit           ! critical leaf nitrogen (g/m^2)
      real       N_leaf_min            ! minimum leaf nitrogen (g/m^2)
cbak      real       N_stem_crit           ! critical stem nitrogen (g/m^2)
cbak      real       N_stem_min            ! minimum stem nitrogen (g/m^2)
cbak      real       N_stover              ! tops (stover) plant nitrogen (g/m^2
cbak      real       N_stover_crit         ! critical top nitrogen (g/m^2)
cbak      real       N_stover_min          ! minimum top nitrogen (g/m^2)
      real       N_conc_ratio          ! available N as fraction of N capacity
                                       ! (0-1)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
         ! calculate actual N concentrations
 
c      dm_stover = g_dm_green(leaf) + g_dm_green(sstem)
c     :          + g_dm_green(cabbage)
c      N_stover = g_N_green(leaf) + g_N_green(sstem)
c     :          + g_N_green(cabbage)
c
c      N_conc_stover = divide (N_stover, dm_stover, 0.0)
 
       N_conc_leaf = divide (g_N_green(leaf), g_dm_green(leaf), 0.0)
 
         ! calculate critical N concentrations
cbak   Base N deficiency on leaf N concentrations
 
       N_leaf_crit = g_N_conc_crit(leaf) * g_dm_green(leaf)
 
c      N_stem_crit = g_N_conc_crit(sstem) * g_dm_green(sstem)
c      N_cabbage_crit = g_N_conc_crit(cabbage) * g_dm_green(cabbage)
c      N_stover_crit = N_leaf_crit + N_stem_crit + N_cabbage_crit
cbak
       N_conc_leaf_crit = divide (N_leaf_crit, g_dm_green(leaf), 0.0)
 
 
         ! calculate minimum N concentrations
 
       N_leaf_min = g_N_conc_min(leaf) * g_dm_green(leaf)
 
c      N_stem_min = g_N_conc_min(sstem) * g_dm_green(sstem)
c      N_cabbage_min = g_N_conc_min(cabbage) * g_dm_green(cabbage)
c      N_stover_min = N_leaf_min + N_stem_min + N_cabbage_min
 
       N_conc_leaf_min = divide (N_leaf_min, g_dm_green(leaf), 0.0)
 
         ! calculate shortfall in N concentrations
 
      N_conc_ratio = divide ((N_conc_leaf - N_conc_leaf_min)
     :              , (N_conc_leaf_crit - N_conc_leaf_min), 0.0)
 
         ! calculate 0-1 N deficiency factors
 
 
          N_def = k_nfact * N_conc_ratio
          nfact = bound (N_def, 0.0, 1.0)
 
      call pop_routine (my_name)
      return
      end



* ====================================================================
       subroutine sugar_death_lodging
     :               (
     :                g_lodge_flag
     :              , G_swdef_photo
     :              , g_oxdef_photo
     :              , c_stress_lodge
     :              , c_death_fr_lodge
     :              , c_num_stress_lodge
     :              , G_plants
     :              , g_dlt_plants_death_lodging
     :               )
 
* ====================================================================
      implicit none
      include 'science.pub'                       
      include 'error.pub'                         

*+  Sub-Program Arguments
      logical g_lodge_flag
      real    g_swdef_photo
      real    g_oxdef_photo
      real    c_stress_lodge(*)
      real    c_death_fr_lodge(*)
      integer c_num_stress_lodge
      real    g_plants
      real    g_dlt_plants_death_lodging

*+  Purpose
*     <insert here>

*+  Changes
*     25-08-1997 - unknown - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'sugar_death_lodging')

*+  Local Variables
      real min_stress_factor
      real death_fraction

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      if (g_lodge_flag) then
 
         min_stress_factor = min(g_swdef_photo, g_oxdef_photo)
 
         death_fraction = linear_interp_real (min_stress_factor
     :                                       ,c_stress_lodge
     :                                       ,c_death_fr_lodge
     :                                       ,c_num_stress_lodge)
 
         g_dlt_plants_death_lodging = - g_plants * death_fraction
      else
         g_dlt_plants_death_lodging = 0.0
      endif
 
      call pop_routine (myname)
      return
      end



*     ===========================================================
      subroutine sugar_min_sstem_sucrose (Option)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'sugar.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Set limit on SStem for start of sucrose partitioning

*+  Changes
*      260897 nih specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_min_sstem_sucrose')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 1) then
         ! These ideally should go in new routines but it seems overkill for
         ! a simple.  This is a patch job
 
         if (on_day_of (begcane, g_current_stage, g_days_tot)) then
            g_min_sstem_sucrose = c_min_sstem_sucrose
         else
         endif
         if (stage_is_between (begcane, crop_end
     :                        , g_current_stage)) then
 
            g_dlt_min_sstem_sucrose = c_min_sstem_sucrose_redn
     :                     * (1.0 - min(g_nfact_stalk, g_swdef_stalk))
            g_dlt_min_sstem_sucrose = u_bound(g_dlt_min_sstem_sucrose
     :                                       ,g_min_sstem_sucrose)
            g_min_sstem_sucrose = g_min_sstem_sucrose
     :                          - g_dlt_min_sstem_sucrose
         else
         endif
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_realloc (Option)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'sugar.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Reallocate cabbage to cane as plant develops to maintain
*       a fixed leaf:cabbage ratio

*+  Notes
*       NIH - Not a generic realloc routine but will do for now

*+  Changes
*     <insert here>

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_realloc')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
         call sugar_realloc_cabbage
     :               (
     :                leaf
     :              , cabbage
     :              , sstem
     :              , max_part
     :              , C_cabbage_sheath_fr
     :              , G_dm_green
     :              , g_dlt_dm_senesced
     :              , G_n_green
     :              , g_dlt_dm_realloc
     :              , g_dlt_n_realloc
     :               )
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_realloc_cabbage
     :               (
     :                leaf
     :              , cabbage
     :              , sstem
     :              , max_part
     :              , C_cabbage_sheath_fr
     :              , G_dm_green
     :              , g_dlt_dm_senesced
     :              , G_n_green
     :              , g_dlt_dm_realloc
     :              , g_dlt_n_realloc
     :               )
 
*     ===========================================================
      implicit none
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer leaf
      integer cabbage
      integer sstem
      integer max_part
      real C_cabbage_sheath_fr
      real G_dm_green(*)
      real g_dlt_dm_senesced(*)
      real G_n_green(*)
      real g_dlt_dm_realloc(*)
      real g_dlt_n_realloc(*)

*+  Purpose
*       Reallocate cabbage to cane as plant develops to maintain
*       a fixed leaf:cabbage ratio

*+  Notes
*       NIH - Not a generic realloc routine but will do for now

*+  Changes
*     <insert here>

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_realloc_cabbage')

*+  Local Variables
      real realloc_wt
      real realloc_n

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      call fill_real_array (g_dlt_dm_realloc, 0.0, max_part)
      call fill_real_array (g_dlt_n_realloc, 0.0, max_part)
 
      realloc_wt = g_dlt_dm_senesced(cabbage)
     :           * (divide (1.0,c_cabbage_sheath_fr,0.0) - 1.0)
      g_dlt_dm_realloc(cabbage) = - realloc_wt
      g_dlt_dm_realloc(sstem) = realloc_wt
 
      ! this is not 100% accurate but swings and round-abouts will look after
      ! it - I hope (NIH)
      realloc_n = divide (g_n_green (cabbage), g_dm_green(cabbage),0.0)
     :          * realloc_wt
      g_dlt_n_realloc(cabbage) = - realloc_n
      g_dlt_n_realloc(sstem) = realloc_n
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_init_root_depth
     :               (
     :                G_dlayer
     :              , G_root_length
     :              , G_root_depth
     :              , dlt_root_depth
     :               )
*     ===========================================================
      implicit none
      include   'sugconst.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL       G_root_length(*)              ! (INPUT)
      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
      real       dlt_root_depth        ! (OUTPUT) increase in root depth (mm)

*+  Purpose
*       This routine returns the increase in root depth.  The
*       approach used here utilises a potential root front velocity
*       affected by relative moisture content at the rooting front.

*+  Notes
*         there is a discrepency when the root crosses into another
*         layer. - cr380

*+  Changes
*      060495 nih taken from template
*      041095 nih change init of root depth to sprouting (was emergence)
*      200396 nih changed max root depth to deepest xf>0
*      300996 nih changed test for init of root depth due to limitation
*                 in on_day_of routine

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_init_root_depth')

*+  Local Variables
      integer    num_root_layers       !

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
cnh      if (on_day_of (sprouting, g_current_stage, g_days_tot)) then
      if (g_root_depth .eq. 0.0) then
 
             ! initialise root depth
             ! this version does not take account of sowing depth.
cnh it used to do this on first day of sprouting
cnh         dlt_root_depth = c_initial_root_depth
 
cnh now I say roots are at bottom of deepest layer that user said had a value
cnh for rlv at initialisation.
            num_root_layers = count_of_real_vals (g_root_length
     :                                           ,max_layer)
            dlt_root_depth =
     :                 sum_real_array (g_dlayer, num_root_layers)
     :                 - g_root_depth
 
      else  ! we have no root growth
 
         ! do nothing
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_bio_water (Option)
*     ===========================================================
      implicit none
      include     'const.inc'
      include     'sugar.inc'
      include 'crp_watr.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       bio transpiration efficiency

*+  Changes
*      5/9/96 dph

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_bio_water')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
         call cproc_bio_water1 (max_layer, g_dlayer, g_root_depth,
     :               g_sw_supply, g_transp_eff, g_dlt_dm_pot_te)
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



* ====================================================================
       subroutine sugar_leaf_area_sen (Option)
* ====================================================================
      implicit none
      include 'const.inc'
      include 'sugar.inc'
      include 'crp_cnpy.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option

*+  Purpose
*      Calculate Leaf Area Senescence

*+  Changes
*     24-04-1998 - NeilH - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'sugar_leaf_area_sen')

*- Implementation Section ----------------------------------
      call push_routine (myname)
      if (Option .eq. 1) then
 
         call sugar_leaf_area_sen_age0
     :               (
     :                G_dlt_node_no_dead
     :              , G_lai
     :              , G_leaf_area
     :              , G_node_no_dead
     :              , G_plants
     :              , G_slai
     :              , G_node_no_detached
     :              , C_leaf_no_at_emerg
     :              , g_dlt_slai_age
     :               )
 
         call crop_leaf_area_sen_water1(c_sen_rate_water,
     :           g_lai, g_swdef_photo, g_plants, 0.0, g_dlt_slai_water)
 
 
         call crop_leaf_area_sen_light1 (
     .          c_lai_sen_light,
     .          c_sen_light_slope,
     .          g_lai,
     .          g_plants,
     .          0.0,
     .          g_dlt_slai_light)
 
         call crop_leaf_area_sen_frost1(c_frost_temp,
     :                c_frost_fraction, c_num_frost_temp, g_lai,
     :                g_mint, g_plants, 0.0, g_dlt_slai_frost)
 
 
         ! now take largest of deltas
         g_dlt_slai = max (g_dlt_slai_age
     :                   , g_dlt_slai_light
     :                   , g_dlt_slai_water
     :                   , g_dlt_slai_frost)
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
      call pop_routine (myname)
      return
      end



*     ===========================================================
      subroutine sugar_init_leaf_area
     :               (
     :                C_initial_tpla
     :              , G_current_stage
     :              , G_days_tot
     :              , G_plants
     :              , lai
     :              , leaf_area
     :               )
*     ===========================================================
      implicit none
      include   'convert.inc'
      include   'sugconst.inc'
      include 'science.pub'                       
      include 'error.pub'                         

*+  Sub-Program Arguments
      REAL       C_initial_tpla        ! (INPUT)  initial plant leaf area (mm^2)
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
      real       lai                   ! (OUTPUT) total plant leaf area
      real       leaf_area(*)          ! (OUTPUT) plant leaf areas

*+  Purpose
*       Initialise leaf area.

*+  Changes
*     070495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_init_leaf_area')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (on_day_of (emerg, g_current_stage, g_days_tot)) then
         lai = c_initial_tpla * smm2sm * g_plants
         leaf_area(1) = c_initial_tpla
      else
      endif
 
      call pop_routine (my_name)
      return
      end



* ====================================================================
       subroutine sugar_nit_demand_est (Option)
* ====================================================================
      implicit none
      include 'const.inc'
      include 'sugar.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer Option

*+  Purpose
*      Calculate an approximate nitrogen demand for today's growth.
*      The estimate basically = n to fill the plant up to maximum
*      nitrogen concentration.

*+  Changes
*     14-05-1997 - huth - Programmed and Specified

*+  Constant Values
      integer num_demand_parts
      parameter (num_demand_parts = 4)
*
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_nit_demand_est')

*+  Local Variables
      real    dlt_dm_green_pot (max_part) ! potential (est) dlt dm green
      real    dm_green_tot            ! total dm green
      integer part                    ! simple plant part counter
      real    dlt_N_retrans(max_part)

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (Option .eq. 1) then
            ! Option 1 is to assume that the distribution of plant
            ! C will be similar after today and so N demand is that
            ! required to raise all plant parts to max N conc.
 
         ! calculate potential new shoot and root growth
      dm_green_tot = sum_real_array (g_dm_green, max_part)
 
      do 100 part = 1, max_part
         dlt_dm_green_pot(part) = g_dlt_dm_pot_rue_pot
     :                          * divide (g_dm_green(part)
     :                                   ,dm_green_tot
     :                                   ,0.0)
         dlt_N_retrans(part) = 0.0
  100 continue
 
         call sugar_N_demand
     :               (
     :                dlt_dm_green_pot
     :              , G_dlt_dm_pot_rue_pot
     :              , G_dm_green
     :              , G_n_conc_crit
     :              , G_n_green
     :              , g_N_demand
     :               )
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_N_demand
     :               (
     :                G_dlt_dm_green_pot
     :              , G_dlt_dm_pot_rue_pot
     :              , G_dm_green
     :              , G_n_conc_crit
     :              , G_n_green
     :              , N_demand
     :               )
*     ===========================================================
      implicit none
      include   'sugconst.inc'
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      REAL       G_dlt_dm_green_pot(*) ! (INPUT)  plant biomass growth (g/m^2)
      REAL       G_dlt_dm_pot_rue_pot  ! (INPUT)
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
      REAL       G_n_conc_crit(*)      ! (INPUT)  critical N concentration (g N/
      REAL       G_n_green(*)          ! (INPUT)  plant nitrogen content (g N/m^
      real       N_demand (*)          ! (OUTPUT) plant nitrogen demand
                                       ! (g/m^2)

*+  Purpose
*       Return plant nitrogen demand for each plant component.  The
*       demand for Nitrogen for each plant pool occurs as the plant
*       tries to maintain a critical nitrogen concentration in each
*       plant pool.

*+  Notes
*           N demand consists of two components:
*           Firstly, the demand for nitrogen by the potential new growth.
*           Secondly, the demand due to the difference between
*           the actual N concentration and the critical N concentration
*           of the tops (stover), which can be positive or negative

*+  Changes
*     060495 nih taken from template

*+  Calls
cnh      real       bound                 ! function
cnh      real       divide                ! function

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_N_demand')

*+  Local Variables
c      integer    current_phase         ! current phase number
      real       N_crit                ! critical N amount (g/m^2)
      real       N_demand_new          ! demand for N by new growth
                                       ! (g/m^2)
      real       N_demand_old          ! demand for N by old biomass
                                       ! (g/m^2)
      integer    part                  ! plant part

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
         ! calculate potential new shoot and root growth
 
c      current_phase = int (g_current_stage)
 
            ! need to calculate dm using potential rue not affected by
            ! N and temperature
 
cnh      do 500 part = 1, max_part
cnh         part_fract = divide (g_dlt_dm_green(part), g_dlt_dm, 0.0)
cnh         dlt_dm_pot(part) = dlt_dm_pot_radn * part_fract
cnh         dlt_dm_pot(part) = bound (dlt_dm_pot(part)
cnh     :                           , 0.0, dlt_dm_pot_radn)
cnh500   continue
 
            ! recalculate roots because today's drymatter production
            ! does not include roots
 
C      dlt_dm_pot(root) = g_dlt_dm_pot_rue_pot
C     :                 * c_ratio_root_shoot(current_phase)
 
 
         ! g_dlt_dm_pot is above ground biomass only so leave roots
         ! out of comparison
 
      call bound_check_real_var (
     :             sum_real_array (G_dlt_dm_green_pot, max_part)
     :           - g_dlt_dm_green_pot(root)
     :           , 0.0, g_dlt_dm_pot_rue_pot
     :           , 'dlt_dm_pot - dlt_dm_pot(root)')
 
 
      ! NIH - note stem stuff is redone down later.
 
      do 1000 part = 1, max_part
         if (g_dm_green(part).gt.0.0) then
 
               ! get N demands due to difference between actual N concentrations
               ! and critical N concentrations of tops (stover) and roots.
 
            N_crit = g_dm_green(part) * g_N_conc_crit(part)
            N_demand_old = N_crit - g_N_green(part)
 
 
               ! get potential N demand (critical N) of potential growth
 
            N_demand_new = g_dlt_dm_green_pot(part)
     :                   * g_N_conc_crit(part)
 
            N_demand(part) = N_demand_old + N_demand_new
            N_demand(part) = l_bound (N_demand(part), 0.0)
 
         else
            N_demand(part) = 0.0
 
         endif
 
1000  continue
 
cnh I am not 100% happy with this but as this is a first attempt at fully
cnh utilizing a sucrose pool I shall put in this quick fix for now and
cnh re-evaluate later.  Note that g_N_conc_crit(Sstem) is really the crit.
cnh conc for CANE.
 
      ! SStem demand for N is based on N conc in cane (i.e SStem+sucrose)
 
      N_crit = (g_dm_green(sstem)+g_dm_green(sucrose))
     :                    * g_N_conc_crit(sstem)
      N_demand_old = N_crit - g_N_green(sstem)
      N_demand_new = (g_dlt_dm_green_pot(sstem)
     :                + g_dlt_dm_green_pot(sucrose))
     :             * g_N_conc_crit(sstem)
      N_demand(sstem) = N_demand_old + N_demand_new
      N_demand(sstem) = l_bound (N_demand(sstem), 0.0)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_nit_stress_pheno (Option)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'sugar.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*         Get current Nitrogen stress factors (0-1)

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_nit_stress_pheno')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (Option .eq. 1) then
 
         call sugar_nfact
     :               (
     :                G_dm_green
     :              , G_n_conc_crit
     :              , G_n_conc_min
     :              , G_n_green
     :              , c_k_nfact_pheno
     :              , g_nfact_pheno
     :               )
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_phen_init
     :               (
     :                C_shoot_lag
     :              , C_shoot_rate
     :              , G_current_stage
     :              , G_days_tot
     :              , G_sowing_depth
     :              , G_Ratoon_no
     :              , P_tt_begcane_to_flowering
     :              , P_tt_emerg_to_begcane
     :              , P_tt_flowering_to_crop_end
     :              , phase_tt
     :               )
*     ===========================================================
      implicit none
      include   'sugconst.inc'
      include 'science.pub'                       
      include 'error.pub'                         

*+  Sub-Program Arguments
      REAL       C_shoot_lag           ! (INPUT)  minimum growing degree days fo
      REAL       C_shoot_rate          ! (INPUT)  growing deg day increase with
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
      REAL       G_sowing_depth        ! (INPUT)  sowing depth (mm)
      INTEGER    G_Ratoon_no           ! (INPUT)  ratoon no (mm)
      REAL       P_tt_begcane_to_flowering ! (INPUT)
      REAL       P_tt_emerg_to_begcane ! (INPUT)
      REAL       P_tt_flowering_to_crop_end ! (INPUT)
      real       phase_tt (*)          ! (INPUT/OUTPUT) cumulative growing
                                       ! degree days required for
                                       ! each stage (deg days)

*+  Purpose
*       Returns cumulative thermal time targets required for the
*       individual growth stages.

*+  Changes
*     060495 nih taken from template
*     030498 igh changed g_ratoon_no to integer

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_phen_init')

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      if (on_day_of (sprouting, g_current_stage, g_days_tot)) then
         if (G_ratoon_no .eq. 0) then
            phase_tt(sprouting_to_emerg) = c_shoot_lag
     :                                   + g_sowing_depth*c_shoot_rate
         else
            ! Assume the mean depth of shooting is half way between the
            ! set depth and the soil surface.
            phase_tt(sprouting_to_emerg) = c_shoot_lag
     :                                   + g_sowing_depth/2.0
     :                                   * c_shoot_rate
         endif
      elseif (on_day_of (emerg, g_current_stage, g_days_tot)) then
         phase_tt(emerg_to_begcane) = p_tt_emerg_to_begcane
 
      elseif (on_day_of (begcane, g_current_stage, g_days_tot)) then
         phase_tt(begcane_to_flowering) = p_tt_begcane_to_flowering
 
      elseif (on_day_of (flowering, g_current_stage, g_days_tot)) then
         phase_tt(flowering_to_crop_end) = p_tt_flowering_to_crop_end
 
      else
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_swdef_demand_ratio
     :               (
     :                C_num_sw_demand_ratio
     :              , C_x_sw_demand_ratio
     :              , C_y_swdef_leaf
     :              , G_dlayer
     :              , G_root_depth
     :              , G_sw_demand
     :              , G_sw_supply
     :              , swdef
     :               )
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'sugconst.inc'
      include 'science.pub'                       
      include 'data.pub'                          
      include 'error.pub'                         

*+  Sub-Program Arguments
      INTEGER    C_num_sw_demand_ratio ! (INPUT)
      REAL       C_x_sw_demand_ratio(*) ! (INPUT)
      REAL       C_y_swdef_leaf(*)     ! (INPUT)
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
      REAL       G_sw_demand           ! (INPUT)  total crop demand for water (m
      REAL       G_sw_supply(*)        ! (INPUT)  potential water to take up (su
      real      swdef                 ! (OUTPUT) sw stress factor (0-1)

*+  Purpose
*       Get the soil water availability factor (0-1), commonly
*       called soil water deficit factor. 1 is no stress, 0 is full stress.

*+  Changes
*       010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_swdef_demand_ratio')

*+  Local Variables
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      real       sw_demand_ratio       ! water supply:demand ratio
      real       sw_supply_sum         ! total supply over profile (mm)

*- Implementation Section ----------------------------------
 
      call push_routine (my_name)
      deepest_layer = find_layer_no (g_root_depth, g_dlayer, max_layer)
 
            ! get potential water that can be taken up when profile is full
 
         sw_supply_sum = sum_real_array (g_sw_supply, deepest_layer)
         sw_demand_ratio = divide (sw_supply_sum, g_sw_demand, 10.0)
 
         swdef = linear_interp_real (sw_demand_ratio
     :                       , c_x_sw_demand_ratio, c_y_swdef_leaf
     :                       , c_num_sw_demand_ratio)
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_leaf_no_init (Option)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'sugar.inc'
      include 'crp_cnpy.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Leaf number development

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_leaf_no_init')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
            ! Plant leaf development
      if (Option .eq. 1) then
 
            ! initialise total leaf number
         call cproc_leaf_no_init1
     :               (
     :                C_leaf_no_at_emerg
     :              , G_current_stage
     :              , emerg
     :              , G_days_tot
     :              , g_leaf_no
     :              , g_node_no
     :               )
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine sugar_leaf_no_pot (Option)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'sugar.inc'
      include 'crp_cnpy.pub'                      
      include 'error.pub'                         

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Leaf number development

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_leaf_no_pot')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
            ! Plant leaf development
      if (Option .eq. 1) then
 
         call cproc_leaf_no_pot1
     :               (
     :                C_x_node_no_app
     :              , C_y_node_app_rate
     :              , c_num_node_no_app
     :              , c_x_node_no_leaf
     :              , C_y_leaves_per_node
     :              , c_num_node_no_leaf
     :              , G_current_stage
     :              , emerg ! start node emerg
     :              , flowering ! end node emerg
     :              , emerg
     :              , G_days_tot
     :              , G_dlt_tt
     :              , G_node_no
     :              , g_dlt_leaf_no !_pot
     :              , g_dlt_node_no !_pot
     :               )
 
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
 
      call pop_routine (my_name)
      return
      end



