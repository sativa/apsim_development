      module SoilpHModule
      use Registrations

!      real       NO3_valency              ! NO3 valency
!      parameter (NO3_valency = -1.0)

      real       Al_valency               ! Al valency
      parameter (Al_valency = +3.0)

      real       CO3_valency              ! CO3 valency
      parameter (CO3_valency = -2.0)

      real       HCO3_valency             ! HCO3 valency
      parameter (HCO3_valency = -1.0)

      real       H_valency                ! H valency
      parameter (H_valency = +1.0)

      real       OH_valency               ! OH valency
      parameter (OH_valency = -1.0)

      real       Ca_valency               ! Ca valency
      parameter (Ca_valency = +2.0)

      real       Mg_valency               ! Mg valency
      parameter (Mg_valency = +2.0)

      real       K_valency                ! K valency
      parameter (K_valency = +1.0)

      real       Na_valency               ! Na valency
      parameter (Na_valency = +1.0)

      real       H2PO4_valency            ! H2PO4 valency
      parameter (H2PO4_valency = -1.0)

      real       SO4_valency              ! SO4 valency
      parameter (SO4_valency = -2.0)

      real       Cl_valency               ! Cl valency
      parameter (Cl_valency = -1.0)

      real       NH4_valency               ! NH4 valency
      parameter (NH4_valency = +1.0)

      real       NO3_valency               ! NO3 valency
      parameter (NO3_valency = -1.0)

      real       C_wt_atomic
      parameter (C_wt_atomic = 12.0107)

      real       Ca_wt_atomic
      parameter (Ca_wt_atomic = 40.078)

      real       N_wt_atomic
      parameter (N_wt_atomic = 14.00674)

      real       O_wt_atomic
      parameter (O_wt_atomic = 15.9994)

      real       H_wt_atomic
      parameter (H_wt_atomic = 1.00794)

      real       Mg_wt_atomic
      parameter (Mg_wt_atomic = 24.305)

      real       K_wt_atomic
      parameter (K_wt_atomic = 39.0983)

      real       Na_wt_atomic
      parameter (Na_wt_atomic = 22.98977)

      real       P_wt_atomic
      parameter (P_wt_atomic = 30.973762)

      real       S_wt_atomic
      parameter (S_wt_atomic = 32.066)

      real       Cl_wt_atomic
      parameter (Cl_wt_atomic = 35.4527)

      real       CaCO3_Kg2Mol   ! Wt of CaCO3 in Kg to Moles.
      parameter (CaCO3_Kg2Mol = 2.0
     :                              * (1000.0
     :                            /(Ca_wt_atomic
     :                              + C_wt_atomic
     :                              + O_wt_atomic*3.0)))
      real       CaCO3_Mol2Kg
      parameter (CaCO3_Mol2Kg = 1.0/CaCO3_Kg2Mol)

      real       CaCO3_t2KMol
      parameter (CaCO3_t2KMol = CaCO3_Kg2Mol)
      real       CaCO3_KMol2t
      parameter (CaCO3_KMol2t = 1.0/CaCO3_Kg2Mol)

      real       NH4_Kg2Mol   ! Wt of N part of NH4 in Kg to Moles.
      parameter (NH4_Kg2Mol = 1000.0/N_wt_atomic)
      real       NH4_Mol2Kg
      parameter (NH4_Mol2Kg = 1.0/NH4_Kg2Mol)

      real       NO3_Kg2Mol   ! Wt of N part of NO3 in Kg to Moles.
      parameter (NO3_Kg2Mol = 1000.0/N_wt_atomic)
      real       NO3_Mol2Kg
      parameter (NO3_Mol2Kg = 1.0/NO3_Kg2Mol)

      real       Ca_Kg2Mol   ! Wt of Ca(2+) in Kg to Moles.
      parameter (Ca_Kg2Mol = 1000.0/Ca_wt_atomic)
      real       Ca_Mol2Kg
      parameter (Ca_Mol2Kg = 1.0/Ca_Kg2Mol)

      real       Mg_Kg2Mol   ! Wt of Mg(2+) in Kg to Moles.
      parameter (Mg_Kg2Mol = 1000.0/Mg_wt_atomic)
      real       Mg_Mol2Kg
      parameter (Mg_Mol2Kg = 1.0/Mg_Kg2Mol)

      real       K_Kg2Mol   ! Wt of K(+) in Kg to Moles.
      parameter (K_Kg2Mol = 1000.0/K_wt_atomic)
      real       K_Mol2Kg
      parameter (K_Mol2Kg = 1.0/K_Kg2Mol)

      real       Na_Kg2Mol   ! Wt of Na(+) in Kg to Moles.
      parameter (Na_Kg2Mol = 1000.0/Na_wt_atomic)
      real       Na_Mol2Kg
      parameter (Na_Mol2Kg = 1.0/Na_Kg2Mol)

      real       P_Kg2Mol   ! Wt of P(-) in Kg to Moles.
      parameter (P_Kg2Mol = 1000.0/P_wt_atomic)
      real       P_Mol2Kg
      parameter (P_Mol2Kg = 1.0/P_Kg2Mol)

      real       S_Kg2Mol   ! Wt of S(2-) in Kg to Moles.
      parameter (S_Kg2Mol = 1000.0/S_wt_atomic)
      real       S_Mol2Kg
      parameter (S_Mol2Kg = 1.0/S_Kg2Mol)

      real       Cl_Kg2Mol   ! Wt of Cl(-) in Kg to Moles.
      parameter (Cl_Kg2Mol = 1000.0/Cl_wt_atomic)
      real       Cl_Mol2Kg
      parameter (Cl_Mol2Kg = 1.0/Cl_Kg2Mol)

      real       cmol2mol
      parameter (cmol2mol = 1.0/100.0)
      real       mol2cmol
      parameter (mol2cmol = 1.0/cmol2mol)

      real       kmol2mol
      parameter (kmol2mol = 1000.0)
      real       mol2kmol
      parameter (mol2kmol = 1.0/kmol2mol)


!     ===========================================================
!     Basic constants
!     ===========================================================

!+  Changes

!- Declaration Section ----------------------------------

      real       pKc_water                ! Equilibrium constant of water
      parameter (pKc_water = 14.0)

!      real       pCO2_rain                ! pCO2 of the atmospheric air
!      parameter (pCO2_rain = 3.5229)

      real       pKa_CO2                  ! Equilibrium constant of CO2
      parameter (pKa_CO2 = 7.82)

      real       pKa_HCO3                 ! Equilibrium constant of Carbonic Acid
      parameter (pKa_HCO3 = 10.33)

      integer    max_layer                ! Maximum no of soil layers.
      parameter (max_layer = 20)

      integer    lime_sol_tbl_size_max    ! Max size of lime solubility table.
      parameter (lime_sol_tbl_size_max = 20)

      integer    max_MF_equiv_type        ! maximum number of H+ equivalent types in mass flow.
      parameter (max_MF_equiv_type = 5)

      integer    H_index                  ! H index used in mass flow.
      parameter (H_index = 1)

      integer    OH_index                 ! OH index used in mass flow.
      parameter (OH_index = 2)

      integer    HCO3_index               ! HCO3 index used in mass flow.
      parameter (HCO3_index = 3)

      integer    CO3_index                ! CO3 index used in mass flow.
      parameter (CO3_index = 4)

      integer    Al_index                 ! Al index used in mass flow.
      parameter (Al_index = 5)

      integer    max_dm_type              ! maximum number of dry matter types
      parameter (max_dm_type = 10)

      integer    dm_type_size             ! max length of the crop type.
      parameter (dm_type_size = 32)

      integer    max_crops                ! maximum number of crops in at once
      parameter (max_crops = 10)

      integer    max_actions              ! maximum number of actions in at once
      parameter (max_actions = 10)

      integer    module_name_size         ! maximum length of module name
      parameter (module_name_size = 20)

      integer    crop_type_size           ! max length of the crop type.
      parameter (crop_type_size = 50)

      integer    action_type_size         ! max length of the action type.
      parameter (action_type_size = 50)

      character  pHBC_method_unknown*(*)
      parameter (pHBC_method_unknown = 'unknown')

      character  pHBC_method_parameters*(*)
      parameter (pHBC_method_parameters = 'parameters')

      character  pHBC_method_Hochman*(*)
      parameter (pHBC_method_Hochman = 'hochman')

      integer    pHCa2pH_tbl_size_max             ! max no elements in pH
      parameter (pHCa2pH_tbl_size_max = 20)     ! conversion table.

      character  keyword_ash_alkalinity*(*)
      parameter (keyword_ash_alkalinity = 'ash_alkalinity')

      character  keyword_name_dm_type*(*)
      parameter (keyword_name_dm_type = 'name_dm_type')

      character  section_ash_alkalinity_loss*(*)
      parameter (section_ash_alkalinity_loss = 'ash_alkalinity_loss')

!     ================================================================
      type soilpHGlobals
         sequence
         real   infiltration_mol
         real   H_equiv_infiltration(max_MF_equiv_type)! H+ ion equivalents into the top layer by component (Mol/ha)
                                          ! (rain).
         real   residue_ash_alk_wt        ! Residue pool ash alkalinity wt (Mol/ha),

         real   pHBC(max_layer)           !  pH buffer cap of the soil(Kmol/ha/100mm/ph_unit).
         real   pHCa(max_layer)           !  pHCa for the soil layer.
         real   pHca_old(max_layer)       !  old pHCa for the soil layer.
         real   dlt_pHCa(max_layer)       !  Change in pHCa.
         real   dlt_pHCa_tot(max_layer)   !  total change in pHCa.
         real   lime_pool(max_layer)      ! lime pool equivalent for each layer (Mol/Ha).
         real   dlt_lime_pool(max_layer)  ! dlt lime pool for each layer (Mol/Ha).
         real   H_equiv_mass_flow(max_layer, max_MF_equiv_type) ! H+ flow out of each layer by component (Mol/ha).
         real   H_equiv_mass_flow_tot(max_layer, max_MF_equiv_type) ! Total H+ flow out of each layer by component (Mol/ha).
         real   H_equiv_flow_net(max_layer, max_MF_equiv_type)  ! Net H+ flow inflow by component - H_equiv_mass_flow (Mol/ha).
         real   H_equiv_flow_net_tot(max_layer, max_MF_equiv_type)  ! Total Net H+ flow inflow by component - H_equiv_mass_flow (Mol/ha).
         real   dlt_lime_dissl(max_layer) ! lime dissolved this timestep (Mol/Ha).
         real   acid_excretion_root(max_layer) ! (R)oo(t) (Ex)cretion of acid.  (Mol/ha)
         real   tec_init(max_layer)       ! Initial Total Exchange Capacity. (cMol/Kg).
         real   tec(max_layer)            ! Total Exchange Capacity. (cMol/Kg).
         real   Al_exchangable(max_layer) ! Concentration of exchangable Al (cMol/Kg)
         real   sAls_calc(max_layer)      ! Calculated Slope of Al versus H+.
         real   sAls(max_layer)           ! Slope of Al versus H+ actually used.
         real   dlt_acid_N_cycle(max_layer) ! Acid added nitrogen cycle (mol H+/Ha).
         real   dlt_acid_org_C_cycle(max_layer) ! Change in acid due to change in
                                                ! humic C. (mol/ha) H+ equiv.

         real   pH(max_layer)             ! pH of soil in a 1:1 soil-water slurry

      end type soilpHGlobals
!     ================================================================
      type soilpHParameters
         sequence
         real    pH_rain                      ! The pHCa of the rainfall.
         real    ionic_strength_rain          ! ionic strength of rain

         character    pHBC_method*32          ! Number of layers used for this simulation.
         character    report_additions*5
         logical  sAls_supplied_use_flag(max_layer) !  Use the one supplied by the user.
         real     Ca_avail(max_layer)         ! Fraction Ca available in this layer.
         real     Mg_avail(max_layer)         ! Fraction Mg available in this layer.
         real     K_avail(max_layer)          ! Fraction K available in this layer.
         real     Na_avail(max_layer)         ! Fraction Na available in this layer.
         real     P_avail(max_layer)          ! Fraction P available in this layer.
         real     S_avail(max_layer)          ! Fraction S available in this layer.
         real     Cl_avail(max_layer)         ! Fraction Cl available in this layer.
         real     Ca_dm_percent               ! Plant uptake_equiv of Ca   %DM
         real     Mg_dm_percent               ! Plant uptake_equiv of Mg   %DM
         real     K_dm_percent                ! Plant uptake_equiv of K    %DM
         real     Na_dm_percent               ! Plant uptake_equiv of Na   %DM
         real     P_dm_percent                ! Plant uptake_equiv of P    %DM
         real     S_dm_percent                ! Plant uptake_equiv of S    %DM
         real     Cl_dm_percent               ! Plant uptake_equiv of Cl   %DM
         real     pHCa_initial(max_layer)     ! Initial pHCa of layer.
         real     ionic_strength_initial(max_layer) ! initial ionic strength of soil in the layer ()
         real     CO2_pressure_soil(max_layer)! Soil air CO2 partial pressure in the layer (atm).
         real     pHBC(max_layer)             ! pH buffer capacity. (Kmol/ha/100mm/ph_unit).
         real     lime_pool_init(max_layer)   ! Initial lime pool (Mol/Ha).
         real     Al_conc_init(max_layer)     ! Initial concentration of Al. (cMol/Kg)
         real     sAls_supplied(max_layer)    ! Slope of Al versus H+. (CMol/Kg)
         real     ecec_init(max_layer)        ! Initial Effective Cation Exchange Capacity. (CMol/Kg)
         real     hum_acid_slope(max_layer)   ! Slope coefficient for humic acids (cMol/Kg).
         real     hum_acid_pHCa_offset(max_layer) ! Slope offset for humic acids (no units)

            ! Following pair are the slope and intercept of pAl versus pHCa, where
            ! where PAl is the negative log of the molar concentration of
            ! labile aluminium in the soil layer.
         real     pAl_pHca_slope(max_layer)     ! Slope of log ionic conc of labile Al vs pHCa.
         real     pAl_pHCa_intercept(max_layer) ! Intercept of log ionic conc of labile Al vs pHCa.


      end type soilpHParameters
!     ================================================================
      type soilpHConstants
         sequence
         integer       num_crops
         integer       num_actions
         integer       num_dm_type(max_crops)
         character     name_dm_type(max_crops,max_dm_type)
     :                 *(dm_type_size)

         integer       lime_sol_tbl_size                    ! Size of lime solubility table.
         real          hum_acid_slope                       ! Slope coeff for humic acids (cMol/Kg)
         real          hum_acid_pHCa_offset                 ! Slope offset for humic acids (no units)
         character     crop_type(max_crops)*(crop_type_size)
         character     action_type(max_actions)*(action_type_size)

               ! Lime solubility table - lime solubility (g/l) versus pHCa.
         real          lime_sol_tbl_pHCa(lime_sol_tbl_size_max)
         real          lime_sol_tbl_lime(lime_sol_tbl_size_max)

         real          ash_alk_tbl_crop(max_crops, max_dm_type)
         real          ash_alk_tbl_action(max_actions)
         real          pHCa2pH_tbl_phca(pHCa2pH_tbl_size_max)
         real          pHCa2pH_tbl_ph(pHCa2pH_tbl_size_max)
         integer       pHCa2pH_tbl_size
         real          wr_coef                      ! depth at which root weighting is 1.8% (mm)
         real          CO2_pressure_atm             ! Atmospheric CO2 partial pressure

      end type soilpHConstants
!     ================================================================
      type soilpHExternals
         sequence
         integer   num_layers                ! Number of layers used for this simulation.
 !cjh         integer   day
 !cjh         integer   year
         real   infiltration_mm              !  Rain infiltrating into top layer (mm).
         real   crop_ash_alk_wt
         real   dlayer(max_layer)            !  Thickness of the soil layer (mm).
         real   flow_water(max_layer)        !  Water moving down out of layer (mm).
         real   org_C_fract(max_layer)       !  Fraction of organic C in the layer.
         real   dlt_lime_added(max_layer)    !  equivalent lime added (Mol/Ha).
         real   ash_alk_wt_incorp(max_layer) ! Ash alkalinity * dry matter
                                             ! weight of residue decomposed or
                                             ! incorporated by tillage.
         real   dlt_OM(max_layer)            !  Increase in soil organic matter (Kg/Ha).
         real   NO3_transform_net_mol(max_layer) ! net NO3 transformation today(Mol/Ha)
         real   NH4_transform_net_mol(max_layer) ! net NH4 transformation today(Mol/Ha)

            ! Crop uptakes for each layer for the day in Moles H+ equiv per Hectare.
         real   NO3_uptake_equiv(max_layer)   ! NO3 taken by crops today (Mol/Ha).
         real   NH4_uptake_equiv(max_layer)   ! NH4 taken by crops today (Mol/Ha).
         real   Ca_uptake_equiv(max_layer)    ! uptake_equiv of Ca,   (Moles H+ equiv / Ha).
         real   Mg_uptake_equiv(max_layer)    ! uptake_equiv of Mg,   (Moles H+ equiv / Ha).
         real   K_uptake_equiv(max_layer)     ! uptake_equiv of K,    (Moles H+ equiv / Ha).
         real   Na_uptake_equiv(max_layer)    ! uptake_equiv of Na,   (Moles H+ equiv / Ha).
         real   P_uptake_equiv(max_layer)     ! uptake_equiv of P,    (Moles H+ equiv / Ha).
         real   S_uptake_equiv(max_layer)     ! uptake_equiv of S,    (Moles H+ equiv / Ha).
         real   Cl_uptake_equiv(max_layer)    ! uptake_equiv of Cl,   (Moles H+ equiv / Ha).
         real   Cation_uptake_equiv(max_layer)! uptake_equiv of Cations,   (Moles H+ equiv / Ha).
         real   Anion_uptake_equiv(max_layer) ! uptake_equiv of Anions,   (Moles H+ equiv / Ha).

         real   ash_alk_wt_incorp_last(max_layer)  ! Ash alkalinity * dry matter
                                                   ! weight of residue decomposed or
                                                   ! incorporated by tillage.
         real   dlt_OM_last(max_layer)             !  Increase in soil organic matter (Kg/Ha).
         real   NO3_transform_net_mol_last(max_layer)  ! net NO3 transformation today(Mol/Ha)
         real   NH4_transform_net_mol_last(max_layer)  ! net NH4 transformation today(Mol/Ha)

            ! Crop uptakes for each layer for the day in Moles H+ equiv per Hectare.
         real   Ca_uptake_equiv_last(max_layer)     ! uptake_equiv of Ca,   (Moles H+ equiv / Ha).
         real   Mg_uptake_equiv_last(max_layer)     ! uptake_equiv of Mg,   (Moles H+ equiv / Ha).
         real   K_uptake_equiv_last(max_layer)      ! uptake_equiv of K,    (Moles H+ equiv / Ha).
         real   Na_uptake_equiv_last(max_layer)     ! uptake_equiv of Na,   (Moles H+ equiv / Ha).
         real   P_uptake_equiv_last(max_layer)      ! uptake_equiv of P,    (Moles H+ equiv / Ha).
         real   S_uptake_equiv_last(max_layer)      ! uptake_equiv of S,    (Moles H+ equiv / Ha).
         real   Cl_uptake_equiv_last(max_layer)     ! uptake_equiv of Cl,   (Moles H+ equiv / Ha).
         real   Cation_uptake_equiv_last(max_layer) ! uptake_equiv of Cations,   (Moles H+ equiv / Ha).
         real   Anion_uptake_equiv_last(max_layer)  ! uptake_equiv of Anions,   (Moles H+ equiv / Ha).

      end type soilpHExternals
!     ================================================================

      ! instance variables.
      common /InstancePointers/ ID,g,p,c,e
      save InstancePointers
      type (soilpHGlobals),pointer :: g
      type (soilpHParameters),pointer :: p
      type (soilpHConstants),pointer :: c
      type (soilpHExternals),pointer :: e
      type (IDsType), pointer :: id


      contains







*     ===========================================================
      subroutine SoilpH_endrun ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*      Free up any resources etc.

*+  Mission Statement
*      Free resources

*+  Changes
*     170699 sb   created

*+  Constant Values
       character  my_name*(*)
       parameter (my_name='SoilpH_EndRun')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine SoilpH_init ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*      Initialise soilpH module

*+  Mission Statement
*      Initialise soilpH module

*+  Changes
*     170699 sb   created

*+  Constant Values
       character  my_name*(*)
       parameter (my_name='SoilpH_Init')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

         ! Get all coefficients from parameter file
      call SoilpH_read_constants ()

         ! Get all parameters from parameter file
      call SoilpH_read_param ()

         ! Perform initial calculations from inputs
      call SoilpH_init_calc ()

      call soilpH_init_residue_ash_alk_wt ()

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine SoilpH_read_constants ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*      Read in all constants from initialization file.

*+  Mission Statement
*      Read in all constants from initialization file

*+  Changes
*     170699 sb   created

*+  Constant Values
      character  section*(*)
      parameter (section = 'constants')
*
      character  my_name*(*)
      parameter (my_name = 'SoilpH_read_constants')

*+  Local Variables
      integer    numvals  ! number of values read from file
      integer    row      ! Loop counter for checking solubility table.

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call write_string (new_line//'   - Reading Constants')

         ! Coefficient for root weighting.
      call read_real_var (section
     :                     , 'wr_coef'
     :                     , '(mm)'
     :                     , c%wr_coef
     :                     , numvals
     :                     , 0.0, 20000.0)

         ! Atmospheric CO2 partial pressure.
      call read_real_var (section
     :                     , 'co2_pressure_atm'
     :                     , '(atm)'
     :                     , c%CO2_pressure_atm
     :                     , numvals
     :                     , 0.0001, 0.01)

         ! Lime solubility table.
      call read_real_array (section
     :                     , 'lime_sol_tbl_phca'
     :                     , lime_sol_tbl_size_max
     :                     , '()'
     :                     , c%lime_sol_tbl_pHCa
     :                     , c%lime_sol_tbl_size
     :                     , 2.0, 10.0)
      call SoilpH_assert (c%lime_sol_tbl_size .gt. 1
     :             , 'Must be more than 1 entry in solubility table')
      do 1000 row = 2, c%lime_sol_tbl_size
         call SoilpH_assert (c%lime_sol_tbl_pHCa(row-1)
     :                      .lt. c%lime_sol_tbl_pHCa(row)
     :             , 'Strictly ascending ordered X in solubility table')
1000  continue
      call read_real_array (section
     :                     , 'lime_sol_tbl_lime'
     :                     , lime_sol_tbl_size_max
     :                     , '(g/l)'
     :                     , c%lime_sol_tbl_lime
     :                     , numvals
     :                     , 0.0, 2.0)
      call SoilpH_assert (numvals .eq. c%lime_sol_tbl_size
     :            , 'Same no of X vals as Y vals in solubility table')

         ! Coefficients for humic acids.
      call read_real_var (section
     :                     , 'hum_acid_slope'
     :                     , '(cMol/Kg)'
     :                     , c%hum_acid_slope
     :                     , numvals
     :                     , 5.0, 100.0)
      call read_real_var (section
     :                     , 'hum_acid_phca_offset'
     :                     , '()'
     :                     , c%hum_acid_pHCa_offset
     :                     , numvals
     :                     , 0.5, 2.5)

         ! Table to convert Calcium chloride pH to water pH.
      call read_real_array(section, 'phca2ph_tbl_phca'
     :                     , pHCa2pH_tbl_size_max
     :                     , '()'
     :                     , c%pHCa2pH_tbl_pHca
     :                     , c%pHCa2pH_tbl_size
     :                     , 3.0, 10.0)
      call soilpH_assert(c%pHCa2pH_tbl_size .gt. 1
     :      , 'Must be more than 1 entry in pH conversion table')
      do 1100 row=2, c%pHCa2pH_tbl_size
         call soilpH_assert(c%pHCa2pH_tbl_pHca(row-1)
     :                      .lt.  c%pHCa2pH_tbl_pHca(row)
     :   , 'Strictly ascending ordered pHCa in pH conversion table')
1100  continue

      call read_real_array(section
     :                     , 'phca2ph_tbl_ph'
     :                     , pHCa2pH_tbl_size_max
     :                     , '()'
     :                     , c%pHCa2pH_tbl_pH
     :                     , numvals
     :                     , 3.0, 10.0)
      call soilpH_assert(numvals .eq. c%pHCa2pH_tbl_size
     :   , 'Same no of pHCa vals as pH vals in pH conversion table')
      do 1200 row=2, c%pHCa2pH_tbl_size
         call soilpH_assert(c%pHCa2pH_tbl_pH(row-1)
     :                      .lt.  c%pHCa2pH_tbl_pH(row)
     :   , 'Strictly ascending ordered pHss in pH conversion table')
1200  continue

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine SoilpH_read_param ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*      Read in all parameters from parameter file.

*+  Mission Statement
*      Read in all parameters from parameter file.

*+  Changes
*     170699 sb   created

*+  Constant Values
      character  section*(*)
      parameter (section = 'parameters')

      character  my_name*(*)
      parameter (my_name='SoilpH_read_param')

*+  Local Variables
      integer numvals

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call write_string (new_line//'   - Reading Parameters')

         ! scalar variables. ----------------
         ! pHCa of rain water.
      call read_real_var (section
     :                     , 'ph_rain'
     :                     , '()'
     :                     ,  p%pH_rain
     :                     , numvals
     :                     , 2.0, 10.0)

         ! ionic strength of rain water.
      call read_real_var (section
     :                     , 'ionic_strength_rain'
     :                     , '()'
     :                     ,  p%ionic_strength_rain
     :                     , numvals
     :                     , 0.001, 0.1)

         ! Uptakes of each element in percent dry matter of crop growth.
      call read_real_var (section
     :                     , 'ca_dm_percent'
     :                     , '(%)'
     :                     ,  p%Ca_dm_percent
     :                     , numvals
     :                     , 0.0, 100.0)
      call read_real_var (section
     :                     , 'mg_dm_percent'
     :                     , '(%)'
     :                     ,  p%Mg_dm_percent
     :                     , numvals
     :                     , 0.0, 100.0)
      call read_real_var (section
     :                     , 'k_dm_percent'
     :                     , '(%)'
     :                     ,  p%K_dm_percent
     :                     , numvals, 0.0, 100.0)
      call read_real_var (section
     :                     , 'na_dm_percent'
     :                     , '(%)'
     :                     ,  p%Na_dm_percent
     :                     , numvals
     :                     , 0.0, 100.0)
      call read_real_var (section
     :                     , 'p_dm_percent'
     :                     , '(%)'
     :                     ,  p%P_dm_percent
     :                     , numvals
     :                     , 0.0, 100.0)
      call read_real_var (section
     :                     , 's_dm_percent'
     :                     , '(%)'
     :                     ,  p%S_dm_percent
     :                     , numvals
     :                     , 0.0, 100.0)
      call read_real_var (section
     :                     , 'cl_dm_percent'
     :                     , '(%)'
     :                     ,  p%Cl_dm_percent
     :                     , numvals
     :                     , 0.0, 100.0)


         ! soil layer arrays. ----------------

         ! Initial lime pool (Kg/Ha) equivalent.  (Wt of CaCO3.)
      call read_real_array (section
     :                     , 'lime_pool_init'
     :                     , max_layer
     :                     , '(Kg/Ha)'
     :                     , p%lime_pool_init
     :                     , numvals
     :                     , 0.0, 100.0)
      call SoilpH_assert (numvals .ge. e%num_layers
     :                    , 'lime_pool_init: numvals .ge. e%num_layers')
      p%lime_pool_init(:) = p%lime_pool_init(:) * CaCO3_kg2mol
!      call SoilpH_vec_scalar_mul (p%lime_pool_init, e%num_layers
!     :                           , CaCO3_kg2mol)

         ! Initial ionic strength of layer.
      call read_real_array (section
     :                     , 'ionic_strength_initial'
     :                     , max_layer
     :                     , '()'
     :                     , p%ionic_strength_initial
     :                     , numvals
     :                     , 0.001, 0.1)
      call SoilpH_assert (numvals .ge. e%num_layers
     :            , 'ionic_strength_initial: numvals .ge. e%num_layers')

         ! Initial pHCa of layer.
      call read_real_array (section
     :                     , 'phca_initial'
     :                     , max_layer
     :                     , '()'
     :                     , p%pHCa_initial
     :                     , numvals
     :                     , 3.8, 8.0)
         ! We get numerical o'flow in calc of pHBC for pHCa of much less than 3.8.
         ! Soils with pHCa below this are very rare.
      call SoilpH_assert (numvals .ge. e%num_layers
     :                 , 'pHCa_initial: numvals .ge. e%num_layers')

         ! pHBC method.
      p%pHBC_method = pHBC_method_unknown
      call read_char_var (section
     :                     , 'phbc_method'
     :                     ,'()'
     :                     , p%pHBC_method
     :                     , numvals)
      If (p%pHBC_method .eq. pHBC_method_parameters) then

            ! pHBC of each layer.
         call read_real_array_optional (section
     :                     , 'phbc'
     :                     , max_layer
     :                     , '(Kmol/ha/100mm/ph_unit)'
     :                     , p%pHBC
     :                     , numvals
     :                     , 0.0, 100.0)
      else
         ! pHBC method is a calculation
      endif

         ! Soil air CO2 partial pressure in the layer.
      call read_real_array (section
     :                     , 'co2_pressure_soil'
     :                     , max_layer
     :                     , '()'
     :                     , p%CO2_pressure_soil
     :                     , numvals
     :                     , 0.0001, 0.01)
      call SoilpH_assert (numvals .ge. e%num_layers
     :                 , 'CO2_pressure_soil: numvals .ge. e%num_layers')

         ! Initial aluminium concentration. (cMol/Kg).
      call read_real_array (section
     :                     , 'al_conc_init'
     :                     , max_layer
     :                     , '(cMol/Kg)'
     :                     , p%Al_conc_init
     :                     , numvals
     :                     , 0.0, 1000.0)
      call SoilpH_assert (numvals .ge. e%num_layers
     :                 , 'Al_conc_init: numvals .ge. e%num_layers')

         !  User supplied slope of -log(labile Al) vs pHCa.
      call read_real_array (section
     :                     , 'pal_phca_slope'
     :                     , max_layer
     :                     , '()'
     :                     , p%pAl_pHca_slope
     :                     , numvals
     :                     , 0.0, 3.0)
      call SoilpH_assert (numvals .ge. e%num_layers
     :                 , 'pAl_phca_slope: numvals .ge. e%num_layers')

         !  User supplied intercept of -log(labile Al) vs pHCa.
      call read_real_array (section
     :                     , 'pal_phca_intercept'
     :                     , max_layer
     :                     , '()'
     :                     , p%pal_pHCa_intercept
     :                     , numvals
     :                     , -5.0, 0.0)
      call SoilpH_assert (numvals .ge. e%num_layers
     :                , 'pal_pHCa_intercept: numvals .ge. e%num_layers')

         !  User supplied Slope of extractable Al versus H. ().
      call read_logical_array_optional (section
     :                                 , 'sals_supplied_use_flag'
     :                                 , max_layer
     :                                 , '()'
     :                                 , p%sAls_supplied_use_flag
     :                                 , numvals)
      call SoilpH_assert (numvals.ge.e%num_layers .or. numvals.eq.0
     :                   , 'sals_supplied_use_flag: '
     :                   // 'numvals.ge.e%num_layers .or. numvals.eq.0')
      call read_real_array_optional (section
     :                     , 'sals_supplied'
     :                     , max_layer
     :                     , '()'
     :                     , p%sAls_supplied
     :                     , numvals
     :                     , 4.0, 200.0)
      call SoilpH_assert (numvals.ge.e%num_layers .or. numvals.eq.0
     :                  ,  'sals_supplied: '
     :                  //  'numvals.ge.e%num_layers .or. numvals.eq.0')
      if (numvals .eq. 0) then
            ! To indicate that no SALS was supplied by the user.
         p%sAls_supplied(:) = -1.0
      else
      endif

         !  Initial Effective Cation Exchange Capacity. (cMol/Kg)
      call read_real_array (section
     :                     , 'ecec_init'
     :                     , max_layer
     :                     , '()'
     :                     , p%ecec_init
     :                     , numvals
     :                     , 0.0, 1000.0)
      call SoilpH_assert (numvals .ge. e%num_layers
     :                 , 'ecec_init: numvals .ge. e%num_layers')

         ! Coefficients for humic acids.
         !  Slope coefficient for humic acids.
      call read_real_array_optional (section
     :                     , 'hum_acid_slope'
     :                     , max_layer
     :                     , '(cMol/Kg)'
     :                     , p%hum_acid_slope
     :                     , numvals
     :                     , 5.0, 100.0)
      if (numvals .eq. 0)  then
         p%hum_acid_slope(:) = c%hum_acid_slope
      else
         call SoilpH_assert (numvals .ge. e%num_layers
     :                 , 'p%hum_acid_slope: numvals.ge.e%num_layers')
      endif

         !  pH coefficient for humic acids.
      call read_real_array_optional (section
     :                     , 'hum_acid_phca_offset'
     :                     , max_layer
     :                     , '(cMol/Kg)'
     :                     , p%hum_acid_pHCa_offset
     :                     , numvals
     :                     , 0.5, 2.5)
      if (numvals .eq. 0)  then
         p%hum_acid_pHCa_offset(:) = c%hum_acid_pHCa_offset
      else
         call SoilpH_assert (numvals .ge. e%num_layers
     :              , 'p%hum_acid_pHCa_offset: numvals.ge.e%num_layers')
      endif

         ! Availability of elements in each layer.

         !  Availability of Ca in each layer.
      call read_real_array (section
     :                     , 'ca_avail'
     :                     , max_layer
     :                     , '()'
     :                     , p%Ca_avail
     :                     , numvals
     :                     , 0.0, 1.0)
      call SoilpH_assert (numvals .ge. e%num_layers
     :                 , 'Ca_avail: numvals .ge. e%num_layers')

         !  Availability of Mg in each layer.
      call read_real_array (section
     :                     , 'mg_avail'
     :                     , max_layer
     :                     , '()'
     :                     , p%Mg_avail
     :                     , numvals
     :                     , 0.0, 1.0)
      call SoilpH_assert (numvals .ge. e%num_layers
     :                 , 'Mg_avail: numvals .ge. e%num_layers')

         !  Availability of K in each layer.
      call read_real_array (section
     :                     , 'k_avail'
     :                     , max_layer
     :                     , '()'
     :                     , p%K_avail
     :                     , numvals
     :                     , 0.0, 1.0)
      call SoilpH_assert (numvals .ge. e%num_layers
     :                 , 'K_avail: numvals .ge. e%num_layers')

         !  Availability of Na in each layer.
      call read_real_array (section
     :                     , 'na_avail'
     :                     , max_layer
     :                     , '()'
     :                     , p%Na_avail
     :                     , numvals
     :                     , 0.0, 1.0)
      call SoilpH_assert (numvals .ge. e%num_layers
     :                 , 'Na_avail: numvals .ge. e%num_layers')

         !  Availability of P in each layer.
      call read_real_array (section
     :                     , 'p_avail'
     :                     , max_layer
     :                     , '()'
     :                     , p%P_avail
     :                     , numvals
     :                     , 0.0, 1.0)
      call SoilpH_assert (numvals .ge. e%num_layers
     :                 , 'P_avail: numvals .ge. e%num_layers')

         !  Availability of S in each layer.
      call read_real_array (section
     :                     , 's_avail'
     :                     , max_layer
     :                     , '()'
     :                     , p%S_avail
     :                     , numvals
     :                     , 0.0, 1.0)
      call SoilpH_assert (numvals .ge. e%num_layers
     :                 , 'S_avail: numvals .ge. e%num_layers')

         !  Availability of Cl in each layer.
      call read_real_array (section
     :                     , 'cl_avail'
     :                     , max_layer
     :                     , '()'
     :                     , p%Cl_avail
     :                     , numvals
     :                     , 0.0, 1.0)
      call SoilpH_assert (numvals .ge. e%num_layers
     :                 , 'Cl_avail: numvals .ge. e%num_layers')

         ! Read in flag for reporting of residue additions
         ! -----------------------------------------------
      call read_char_var_optional (
     :           section              ! Section header
     :         , 'report_additions'   ! Keyword
     :         , '()'                 ! Units
     :         , p%report_additions   ! Variable
     :         , numvals)             ! Number of values returned
      if (numvals.eq.0) then
         p%report_additions = 'no'
      else
      endif


      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine SoilpH_sum_report ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*     Report SoilpH Module Status

*+  Mission Statement
*     Report SoilpH Module Status

*+  Changes
*       230500 Jngh - Programmed and Specified

*+  Constant Values
      character*(*) my_name            ! name of current procedure
      parameter (my_name = 'SoilpH_sum_report')

*+  Local Variables
      integer    layer                 ! layer number
      integer    num_layers            ! number of soil profile layers
      real       depth_layer_top       ! depth to top of layer (mm)
      real       depth_layer_bottom    ! depth to bottom of layer (mm)
      character  line*80            ! output line

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call write_string (new_line//new_line)

      write (line, '(2a)') 'Report residue additions = '
     :                     , trim(p%report_additions)
      call write_string (line)

      write (line, '(a, f4.1)') 'Rain  - pH               = '
     :                     , p%pH_rain
      call write_string (line)

      write (line, '(a, f6.3)') '      - Ionic strength   = '
     :                     , p%ionic_strength_rain
      call write_string (line)

      call write_string (       'Using pHBC Method:         '
     :                         // trim (p%pHBC_method))

      call write_string (new_line//new_line)

      line = '                Soil Profile Inital values'
      call write_string (line)

      line = '     ------------------------------------------------'
      call write_string (line)

      line = 'Layer    Depth      Lime  Ionic   pHCa   pHBC   '
     :             //'ECEC  CO2 pressure'
      call write_string (line)

      line = '         (mm)    (mol/ha) Strength ()  (KMol/Ha)'
     :             //' ()    (atm)'
      call write_string (line)

      line = '-------------------------------------------------'
     :             //'------------'
      call write_string (line)

      num_layers = count_of_real_vals(e%dlayer, max_layer)
      depth_layer_top = 0.0

      do 100 layer = 1,num_layers
         depth_layer_bottom = depth_layer_top + e%dlayer(layer)
         write (line
     :     , '(i4, f6.0, a, f6.0, f7.1, f7.3, f7.1, f7.2, f7.1, f7.4)')
     :           layer, depth_layer_top, '-', depth_layer_bottom
     :           , p%lime_pool_init(layer)
     :           , p%ionic_strength_initial(layer)
     :           , p%pHCa_initial(layer)
     :           , p%pHBC(layer)
     :           , p%ecec_init(layer)
     :           , p%CO2_pressure_soil(layer)

         call write_string (line)
         depth_layer_top = depth_layer_bottom
  100 continue

      line = '-------------------------------------------------'
     :             //'------------'
      call write_string (line)

      call write_string (new_line//new_line)

      line = '               Soil Profile Al Parameters'
      call write_string (line)

      line = '     ------------------------------------------------'
      call write_string (line)

      line = 'Layer    Depth   Al Conc   pAl-pHCa         sAls     '
      call write_string (line)

      line = '         (mm)   (cMol/Kg) Slope Intercept   ()    Use'
      call write_string (line)

      line = '-----------------------------------------------------'
      call write_string (line)

      depth_layer_top = 0.0

      do 200 layer = 1,num_layers
         depth_layer_bottom = depth_layer_top + e%dlayer(layer)
         write (line
     :     , '(i4, f6.0, a, f6.0, f7.2, f7.3, f8.4, f7.0, L7)')
     :           layer, depth_layer_top, '-', depth_layer_bottom
     :           , p%Al_conc_init(layer)
     :           , p%pAl_pHca_slope(layer)
     :           , p%pal_pHCa_intercept(layer)
     :           , p%sAls_supplied(layer)
     :           , p%sAls_supplied_use_flag(layer)
         call write_string (line)
         depth_layer_top = depth_layer_bottom
  200 continue

      line = '-----------------------------------------------------'
      call write_string (line)
      call write_string (new_line//new_line)

      line = '           Soil Profile Humic Acid Coefficients'
      call write_string (line)

      line = '     ------------------------------------------------'
      call write_string (line)

      line = 'Layer    Depth     Slope Offset'
      call write_string (line)

      line = '         (mm)        ()     () '
      call write_string (line)

      line = '-------------------------------'
      call write_string (line)

      depth_layer_top = 0.0

      do 300 layer = 1,num_layers
         depth_layer_bottom = depth_layer_top + e%dlayer(layer)
         write (line
     :     , '(i4, f6.0, a, f6.0, f7.1, f7.1)')
     :           layer, depth_layer_top, '-', depth_layer_bottom
     :           , p%hum_acid_slope(layer)
     :           , p%hum_acid_pHCa_offset(layer)
         call write_string (line)
         depth_layer_top = depth_layer_bottom
  300 continue

      line = '-------------------------------'
      call write_string (line)
      call write_string (new_line//new_line)

      line = '          Soil Profile Availability of elements'
      call write_string (line)

      line = '     ------------------------------------------------'
      call write_string (line)

      line = 'Layer    Depth     Ca   Mg    K   Na    P    S   Cl'
      call write_string (line)

      line = '         (mm)'
      call write_string (line)

      line = '----------------------------------------------------'
      call write_string (line)

      depth_layer_top = 0.0

      do 400 layer = 1,num_layers
         depth_layer_bottom = depth_layer_top + e%dlayer(layer)
         write (line
     :     , '(i4, f6.0, a, f6.0, 7f5.2)')
     :           layer, depth_layer_top, '-', depth_layer_bottom
     :           , p%Ca_avail(layer)
     :           , p%Mg_avail(layer)
     :           , p%K_avail(layer)
     :           , p%Na_avail(layer)
     :           , p%P_avail(layer)
     :           , p%S_avail(layer)
     :           , p%Cl_avail(layer)
         call write_string (line)
         depth_layer_top = depth_layer_bottom
  400 continue

      line = '----------------------------------------------------'
      call write_string (line)
      call write_string (new_line//new_line)

      line = '          Uptakes of each element (% dry matter)'
      call write_string (line)

      line = '     ------------------------------------------------'
      call write_string (line)

      line = '    Ca     Mg      K     Na      P      S     Cl'
      call write_string (line)

      line = '    (%)    (%)    (%)    (%)    (%)    (%)    (%)'
      call write_string (line)

      line = '----------------------------------------------------'
      call write_string (line)

         write (line
     :     , '(7f7.3)')
     :             p%ca_dm_percent
     :           , p%Mg_dm_percent
     :           , p%K_dm_percent
     :           , p%Na_dm_percent
     :           , p%P_dm_percent
     :           , p%S_dm_percent
     :           , p%Cl_dm_percent
         call write_string (line)

      line = '----------------------------------------------------'
      call write_string (line)

      call write_string (new_line//new_line)

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine soilpH_zero_all_globals ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*       Zero all global variables & arrays

*+  Changes
*     060495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'soilpH_zero_all_globals')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

         ! Globals

      g%H_equiv_infiltration(:)  = 0.0
      g%residue_ash_alk_wt       = 0.0
      g%pHBC(:)                  = 0.0
      g%pHCa(:)                  = 0.0
      g%pHca_old(:)              = 0.0
      g%dlt_pHCa(:)              = 0.0
      g%dlt_pHCa_tot(:)          = 0.0
      g%lime_pool(:)             = 0.0
      g%dlt_lime_pool(:)         = 0.0
      g%H_equiv_mass_flow(:,:)     = 0.0
      g%H_equiv_mass_flow_tot(:,:) = 0.0
      g%H_equiv_flow_net(:,:)      = 0.0
      g%H_equiv_flow_net_tot(:,:)  = 0.0
      g%dlt_lime_dissl(:)        = 0.0
      g%acid_excretion_root(:)   = 0.0
      g%tec_init(:)              = 0.0
      g%tec(:)                   = 0.0
      g%Al_exchangable(:)        = 0.0
      g%sAls_calc(:)             = 0.0
      g%sAls(:)                  = 0.0
      g%dlt_acid_N_cycle(:)      = 0.0
      g%dlt_acid_org_C_cycle(:)  = 0.0
      g%pH(:)                    = 0.0

         ! Parameters

      p%pH_rain                 = 0.0
      p%ionic_strength_rain   = 0.0
      p%pHBC_method          = blank
      p%report_additions     = blank
      p%sAls_supplied_use_flag(:) = .false.
      p%Ca_avail(:)            = 0.0
      p%Mg_avail(:)            = 0.0
      p%K_avail(:)             = 0.0
      p%Na_avail(:)            = 0.0
      p%P_avail(:)             = 0.0
      p%S_avail(:)             = 0.0
      p%Cl_avail(:)            = 0.0
      p%Ca_dm_percent          = 0.0
      p%Mg_dm_percent          = 0.0
      p%K_dm_percent           = 0.0
      p%Na_dm_percent          = 0.0
      p%P_dm_percent           = 0.0
      p%S_dm_percent           = 0.0
      p%Cl_dm_percent          = 0.0
      p%ionic_strength_initial(:) = 0.0
      p%pHCa_initial(:)        = 0.0
      p%CO2_pressure_soil(:)   = 0.0
      c%CO2_pressure_atm       = 0.0
      p%pHBC(:)                = 0.0
      p%lime_pool_init(:)      = 0.0
      p%Al_conc_init(:)        = 0.0
      p%sAls_supplied(:)       = 0.0
      p%ecec_init(:)           = 0.0
      p%hum_acid_slope(:)      = 0.0
      p%hum_acid_pHCa_offset(:)= 0.0
      p%pAl_pHca_slope(:)      = 0.0
      p%pAl_pHCa_intercept(:)  = 0.0

         ! Constants

      c%num_crops          = 0
      c%num_actions        = 0
      c%num_dm_type(:)     = 0
      c%name_dm_type(:,:)  = blank
      c%lime_sol_tbl_size  = 0
      c%hum_acid_slope            = 0.0
      c%hum_acid_pHCa_offset      = 0.0
      c%crop_type(:)       = blank
      c%action_type(:)     = blank
      c%lime_sol_tbl_pHCa(:)      = 0.0
      c%lime_sol_tbl_lime(:)      = 0.0
      c%ash_alk_tbl_crop(:, :)    = 0.0
      c%ash_alk_tbl_action(:)     = 0.0
      c%pHCa2pH_tbl_pHca(:)       = 0.0
      c%pHCa2pH_tbl_pH(:)         = 0.0
      c%pHCa2pH_tbl_size          = 0
      c%wr_coef                   = 0.0

         ! Externals

      e%num_layers                   = 0
      e%infiltration_mm                 = 0.0
      e%crop_ash_alk_wt                 = 0.0
      e%dlayer(:)                       = 0.0
      e%flow_water(:)                   = 0.0
      e%org_C_fract(:)                  = 0.0
      e%dlt_lime_added(:)               = 0.0
      e%ash_alk_wt_incorp(:)            = 0.0
      e%dlt_OM(:)                       = 0.0
      e%NO3_transform_net_mol(:)        = 0.0
      e%NH4_transform_net_mol(:)        = 0.0
      e%NO3_uptake_equiv(:)             = 0.0
      e%NH4_uptake_equiv(:)             = 0.0
      e%Ca_uptake_equiv(:)              = 0.0
      e%Mg_uptake_equiv(:)              = 0.0
      e%K_uptake_equiv(:)               = 0.0
      e%Na_uptake_equiv(:)              = 0.0
      e%P_uptake_equiv(:)               = 0.0
      e%S_uptake_equiv(:)               = 0.0
      e%Cl_uptake_equiv(:)              = 0.0
      e%Cation_uptake_equiv(:)          = 0.0
      e%Anion_uptake_equiv(:)           = 0.0
      e%ash_alk_wt_incorp_last(:)       = 0.0
      e%dlt_OM_last(:)                  = 0.0
      e%NO3_transform_net_mol_last(:)   = 0.0
      e%NH4_transform_net_mol_last(:)   = 0.0
      e%Ca_uptake_equiv_last(:)         = 0.0
      e%Mg_uptake_equiv_last(:)         = 0.0
      e%K_uptake_equiv_last(:)          = 0.0
      e%Na_uptake_equiv_last(:)         = 0.0
      e%P_uptake_equiv_last(:)          = 0.0
      e%S_uptake_equiv_last(:)          = 0.0
      e%Cl_uptake_equiv_last(:)         = 0.0
      e%Cation_uptake_equiv_last(:)     = 0.0
      e%Anion_uptake_equiv_last(:)      = 0.0

      call pop_routine (my_name)
      return
      end subroutine
*     ===========================================================
      subroutine SoilpH_zero_variables ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*       Zero soilpH variables

*+  Mission Statement
*       Zero soilpH variables

*+  Changes
*     170699 sb   created

*+  Constant Values
      character  my_name*(*)           ! subroutine name
      parameter (my_name = 'SoilpH_zero_variables')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      e%num_layers      = 0
      c%num_crops       = 0
      c%num_actions     = 0
      c%num_dm_type(:)  = 0
      c%name_dm_type(:,:)= blank
      c%crop_type(:)    = blank
      c%action_type(:)  = blank
      c%ash_alk_tbl_crop(:,:) = 0.0
      c%ash_alk_tbl_action(:) = 0.0

         !  Constants
      c%lime_sol_tbl_size     = 0
      c%hum_acid_slope        = 0.0
      c%hum_acid_pHCa_offset  = 0.0
      c%CO2_pressure_atm      = 0.0

      c%lime_sol_tbl_pHCa(:) = 0.0
      c%lime_sol_tbl_lime(:) = 0.0

         !  Paramaters
      p%report_additions   = blank
      p%pHBC_method        = blank
      p%pH_rain            = 0.0
      p%ionic_strength_rain = 0.0
      p%Ca_dm_percent      = 0.0
      p%Mg_dm_percent      = 0.0
      p%K_dm_percent       = 0.0
      p%Na_dm_percent      = 0.0
      p%P_dm_percent       = 0.0
      p%S_dm_percent       = 0.0
      p%Cl_dm_percent      = 0.0
      p%sAls_supplied_use_flag(:) = .false.
      p%Ca_avail(:)       = 0.0
      p%Mg_avail(:)       = 0.0
      p%K_avail(:)        = 0.0
      p%Na_avail(:)       = 0.0
      p%P_avail(:)        = 0.0
      p%S_avail(:)        = 0.0
      p%Cl_avail(:)       = 0.0
      p%ionic_strength_initial(:) = 0.0
      p%pHCa_initial(:)   = 0.0
      p%pHBC(:)           = 0.0
      p%CO2_pressure_soil(:)= 0.0
      p%lime_pool_init(:) = 0.0
      p%Al_conc_init(:)   = 0.0
      p%sAls_supplied(:)  = 0.0
      p%ecec_init(:)      = 0.0
      p%hum_acid_slope(:) = 0.0
      p%hum_acid_pHCa_offset(:) = 0.0
      p%pAl_pHca_slope(:) = 0.0
      p%pAl_pHCa_intercept(:)= 0.0

         !  External variables.
 !cjh      e%day = 0
 !cjh      e%year = 0
      e%infiltration_mm    = 0.0
      e%crop_ash_alk_wt    = 0.0

      e%dlayer(:)            = 0.0
      e%flow_water(:)        = 0.0
      e%org_C_fract(:)       = 0.0
      e%dlt_lime_added(:)    = 0.0
      e%ash_alk_wt_incorp(:) = 0.0
      e%dlt_OM(:)            = 0.0
      e%NH4_transform_net_mol(:)= 0.0
      e%NO3_transform_net_mol(:)= 0.0
      e%NO3_uptake_equiv(:)   = 0.0
      e%NH4_uptake_equiv(:)   = 0.0
      e%Ca_uptake_equiv(:)    = 0.0
      e%Mg_uptake_equiv(:)    = 0.0
      e%K_uptake_equiv(:)     = 0.0
      e%Na_uptake_equiv(:)    = 0.0
      e%P_uptake_equiv(:)     = 0.0
      e%S_uptake_equiv(:)     = 0.0
      e%Cl_uptake_equiv(:)    = 0.0
      e%Cation_uptake_equiv(:)= 0.0
      e%Anion_uptake_equiv(:) = 0.0
      e%ash_alk_wt_incorp_last(:)    = 0.0
      e%NH4_transform_net_mol_last(:)= 0.0
      e%NO3_transform_net_mol_last(:)= 0.0
      e%dlt_OM_last(:)          = 0.0
      e%Ca_uptake_equiv_last(:)  = 0.0
      e%Mg_uptake_equiv_last(:)  = 0.0
      e%K_uptake_equiv_last(:)   = 0.0
      e%Na_uptake_equiv_last(:)  = 0.0
      e%P_uptake_equiv_last(:)   = 0.0
      e%S_uptake_equiv_last(:)   = 0.0
      e%Cl_uptake_equiv_last(:)  = 0.0
      e%Cation_uptake_equiv_last(:)     = 0.0
      e%Anion_uptake_equiv_last(:)      = 0.0



         !  Calculated variables.
      g%H_equiv_infiltration(:) = 0.0
      g%residue_ash_alk_wt   = 0.0
      g%pHBC(:)              = 0.0
      g%pHCa(:)              = 0.0
      g%pHca_old(:)          = 0.0
      g%dlt_pHCa(:)          = 0.0
      g%dlt_pHCa_tot(:)      = 0.0
      g%lime_pool(:)         = 0.0
      g%dlt_lime_pool(:)     = 0.0
      g%H_equiv_mass_flow(:,:)     = 0.0
      g%H_equiv_mass_flow_tot(:,:) = 0.0
      g%H_equiv_flow_net(:,:)      = 0.0
      g%H_equiv_flow_net_tot(:,:)  = 0.0
      g%dlt_lime_dissl(:)    = 0.0
      g%acid_excretion_root(:)= 0.0
      g%tec_init(:)          = 0.0
      g%tec(:)               = 0.0
      g%Al_exchangable(:)    = 0.0
      g%sAls_calc(:)         = 0.0
      g%sAls(:)              = 0.0
      g%dlt_acid_N_cycle(:)  = 0.0
      g%dlt_acid_org_C_cycle(:)= 0.0
      g%pH(:)                = 0.0
      c%pHCa2pH_tbl_size = 0
      c%pHCa2pH_tbl_pHca(:) = 0.0
      c%pHCa2pH_tbl_pH(:) = 0.0

         !   The following really do need to be zeroed for soilpH to work.
      e%dlt_lime_added(:)    = 0.0

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine SoilpH_zero_event_variables ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*       Zero soilpH variables

*+  Mission Statement
*       Zero soilpH variables

*+  Changes
*     170699 sb   created

*+  Constant Values
      character  my_name*(*)           ! subroutine name
      parameter (my_name = 'SoilpH_zero_event_variables')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      e%ash_alk_wt_incorp_last(:)      = e%ash_alk_wt_incorp(:)
      e%NH4_transform_net_mol_last(:)  = e%NH4_transform_net_mol(:)
      e%NO3_transform_net_mol_last(:)  = e%NO3_transform_net_mol(:)
      e%dlt_OM_last(:)                 = e%dlt_OM(:)
      e%Ca_uptake_equiv_last(:)         = e%Ca_uptake_equiv(:)
      e%Mg_uptake_equiv_last(:)         = e%Mg_uptake_equiv(:)
      e%K_uptake_equiv_last(:)          = e%K_uptake_equiv(:)
      e%Na_uptake_equiv_last(:)         = e%Na_uptake_equiv(:)
      e%P_uptake_equiv_last(:)          = e%P_uptake_equiv(:)
      e%S_uptake_equiv_last(:)          = e%S_uptake_equiv(:)
      e%Cl_uptake_equiv_last(:)         = e%Cl_uptake_equiv(:)
      e%Cation_uptake_equiv_last(:)     = e%Cation_uptake_equiv(:)
      e%Anion_uptake_equiv_last(:)      = e%Anion_uptake_equiv(:)

      e%ash_alk_wt_incorp(:)    = 0.0
      e%NH4_transform_net_mol(:)= 0.0
      e%NO3_transform_net_mol(:)= 0.0
      e%dlt_OM(:)               = 0.0
      e%Ca_uptake_equiv(:)       = 0.0
      e%Mg_uptake_equiv(:)       = 0.0
      e%K_uptake_equiv(:)        = 0.0
      e%Na_uptake_equiv(:)       = 0.0
      e%P_uptake_equiv(:)        = 0.0
      e%S_uptake_equiv(:)        = 0.0
      e%Cl_uptake_equiv(:)       = 0.0
      e%Cation_uptake_equiv(:)   = 0.0
      e%Anion_uptake_equiv(:)    = 0.0

      e%crop_ash_alk_wt    = 0.0

      call pop_routine (my_name)
      return
      end subroutine




*     ===========================================================
      subroutine SoilpH_get_other_variables ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*      Get the values of variables from other modules

*+  Mission Statement
*      Get the values of variables from other modules

*+  Changes
*     170699 sb   created

*+  Constant Values
      character  my_name*(*)
      parameter (my_name='SoilpH_get_other_variables')

*+  Local Variables
      integer    numvals               ! number of values returned.

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call SoilpH_get_soil_layers ()

         ! Amount of rain for today.
      call get_real_var (unknown_module
     :                     , 'infiltration'
     :                     , '(mm)'
     :                     , e%infiltration_mm
     :                     , numvals
     :                     , 0.0, 1000.0)
      call SoilpH_assert (numvals.eq.1
     :                 , 'infiltration: numvals.eq.1')

         ! Amount of water moving down out of layer (mm).
      call get_real_array (unknown_module
     :                     , 'flow_water'
     :                     , e%num_layers
     :                     , '(mm)'
     :                     , e%flow_water
     :                     , numvals
     :                     , -1000.0, 1000.0)

         ! Get other things.
      call SoilpH_get_crop_uptakes ()  ! Get uptakes from each crop.
      call soilpH_get_org_C_fract ()   ! Get fraction of organic carbon in the layer.

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine SoilpH_get_soil_layers ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*      Get the values of variables from other modules

*+  Mission Statement
*      Get the values of variables from other modules

*+  Changes
*     170699 sb   created

*+  Constant Values
      character  my_name*(*)
      parameter (my_name='SoilpH_get_soil_layers')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

         ! Thickness of the soil layer (mm) and number of layers.
      call get_real_array (unknown_module
     :                     , 'dlayer'
     :                     , max_layer
     :                     , '(mm)'
     :                     , e%dlayer
     :                     , e%num_layers
     :                     , 1.0, 1000.0)


      call pop_routine (my_name)
      return
      end subroutine




*     ===========================================================
      subroutine soilpH_get_org_C_fract ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*     Get fraction of organic carbon in each layer.

*+  Mission Statement
*     Get fraction of organic carbon in each layer

*+  Changes
*     170699 sb   created

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'soilpH_get_org_C_fract')

*+  Local Variables
      integer    numvals               ! number of values returned.
      real       org_C(max_layer)      ! Amount of organic C in each layer (Kg/Ha).

*- Implementation Section ----------------------------------
      call push_routine  (my_name)

         ! Fraction of organic carbon in the layer.
      org_C(:) = 0.0

         ! Get OC%.
      call get_real_array (unknown_module
     :                     , 'oc%'
     :                     , e%num_layers
     :                     , '(%)'
     :                     , org_C
     :                     , numvals
     :                     , 0.0, 100.0)

      e%org_C_fract(:) = org_C(:) * pcnt2fract

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine SoilpH_get_crop_uptakes ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*     Obtain uptakes of elements by each crop.

*+  Mission Statement
*     Obtain uptakes of elements by each crop

*+  Changes
*     170699 sb   created

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'SoilpH_get_crop_uptakes')

*+  Local Variables
      integer    crop_module(max_crops)      ! list of module IDs
      integer    num_crops                   ! How many crops do we know about.
      integer    index_crop                  ! Loop counter to keep track of which crop.
      integer    numvals                     ! Was there a variable.
      character  crop_type*(crop_type_size)  ! Somewhere to put the crop type.
      character  err_msg*200                 ! String to hold error messages.
      real       this_uptake(max_layer)      ! Temp var holds uptake_equiv of thing from 1 crop.

*- Implementation Section ----------------------------------
      call push_routine (my_name)

         ! Find out what crops are active for this run.
      num_crops = 0
110   continue
         call get_char_vars (num_crops+1
     :                     , 'crop_type'
     :                     , '()'
     :                     , crop_type
     :                     ,  numvals)
         if (numvals.eq.1 .and. num_crops.lt.max_crops)  then
            num_crops = num_crops + 1
            crop_module(num_crops) = get_posting_module ()
            goto 110
         else
         endif

      if (numvals .eq. 1) then
         write (err_msg, '(A,I3,A)') 'More than ', max_crops, ' crops'
         call fatal_error (err_user, err_msg)
      else
      endif

         ! Get the NO3 uptake_equiv for each crop.
      e%NO3_uptake_equiv(:) = 0.0

      do 1200 index_crop=1, num_crops
         this_uptake(:) = 0.0
         call get_real_array (crop_module(index_crop)
     :                     , 'no3_uptake'
     :                     , max_layer
     :                     , '(Kg/Ha)'
     :                     , this_uptake
     :                     , numvals
     :                     , -1000.0, 1000.0 )
         call SoilpH_assert (numvals.eq.e%num_layers
     :                 , 'NO3_uptake: numvals.eq.e%num_layers')
         e%NO3_uptake_equiv(:) = e%NO3_uptake_equiv(:)
     :                   + this_uptake(:) * NH4_kg2mol * NO3_valency
!         call add_real_array (this_uptake, e%NO3_uptake_equiv, e%num_layers)
1200  continue

         ! Get the NH4 uptake_equiv for each crop.
      e%NH4_uptake_equiv(:) = 0.0
      do 1300 index_crop=1, num_crops
         this_uptake(:) = 0.0
         call get_real_array (crop_module(index_crop)
     :                     , 'nh4_uptake'
     :                     , max_layer
     :                     , '(Kg/Ha)'
     :                     , this_uptake
     :                     , numvals
     :                     , -1000.0, 1000.0 )
         call SoilpH_assert (numvals.eq.e%num_layers
     :                 , 'NH4_uptake: numvals.eq.e%num_layers')
         e%NH4_uptake_equiv(:) = e%NH4_uptake_equiv(:)
     :                   + this_uptake(:) * NH4_kg2mol * NH4_valency
!         call add_real_array (this_uptake, e%NH4_uptake_equiv, e%num_layers)
1300  continue


      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine SoilpH_uptake_equiv (crop_module
     :                              , crop_ash_alk_wt
     :                              , Ca_uptake_equiv
     :                              , Mg_uptake_equiv
     :                              , K_uptake_equiv
     :                              , Na_uptake_equiv
     :                              , P_uptake_equiv
     :                              , S_uptake_equiv
     :                              , Cl_uptake_equiv
     :                              , Cation_uptake_equiv
     :                              , Anion_uptake_equiv
     :                              )
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    crop_module                    ! (INPUT) name of crop module
      real       crop_ash_alk_wt                ! (INPUT) ash alkalinity weight (Mol/ha)
      real       Ca_uptake_equiv(max_layer)     ! (OUTPUT) H+ uptake equivalents of Ca (mol/ha)
      real       Mg_uptake_equiv(max_layer)     ! (OUTPUT) H+ uptake equivalents of Mg (mol/ha)
      real       K_uptake_equiv(max_layer)      ! (OUTPUT) H+ uptake equivalents of K (mol/ha)
      real       Na_uptake_equiv(max_layer)     ! (OUTPUT) H+ uptake equivalents of Na (mol/ha)
      real       P_uptake_equiv(max_layer)      ! (OUTPUT) H+ uptake equivalents of P (mol/ha)
      real       S_uptake_equiv(max_layer)      ! (OUTPUT) H+ uptake equivalents of S (mol/ha)
      real       Cl_uptake_equiv(max_layer)     ! (OUTPUT) H+ uptake equivalents of Cl (mol/ha)
      real       Cation_uptake_equiv(max_layer) ! (OUTPUT) H+ uptake equivalents of Cations (mol/ha)
      real       Anion_uptake_equiv(max_layer)  ! (OUTPUT) H+ uptake equivalents of Anions (mol/ha)


*+  Purpose
*     Obtain uptakes of elements by each crop.

*+  Mission Statement
*     Obtain uptakes of elements by each crop

*+  Changes
*     170699 sb   created

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'SoilpH_uptake_equiv')

*+  Local Variables
      integer    layer                       ! Loop counter to keep track of which layer.
      integer    numvals                     ! Number of values returnd.
      real       dlt_dm                      ! (Dry matter crop uptake_equiv for day).
      real       rlv(max_layer)              ! Root length volume (mm/mm^2)
      real       rldf(max_layer)             ! Root length density factor ()
      real       root_depth                  ! root depth (mm)
      real       root_length_area(max_layer) ! Root length (rlv(i) * dlayer(i)). (units not considered
                                             ! important). Uptakes are distributed in same proportions.
      real       uptake_equiv_sum            ! sum of H+ uptake equivalents (mm root/mm^2 soil)
      real       dlt_Ca_uptake_equiv(max_layer)
      real       dlt_Mg_uptake_equiv(max_layer)
      real       dlt_K_uptake_equiv(max_layer)
      real       dlt_Na_uptake_equiv(max_layer)
      real       dlt_P_uptake_equiv(max_layer)
      real       dlt_S_uptake_equiv(max_layer)
      real       dlt_Cl_uptake_equiv(max_layer)
      character  module_name*(module_name_size)
      logical    ok

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (crop_ash_alk_wt .gt. 0.0) then
            ! Estimate uptakes of other elements for each crop.

            !  Zero out all uptakes.
         dlt_Ca_uptake_equiv(:)  = 0.0
         dlt_Mg_uptake_equiv(:)  = 0.0
         dlt_K_uptake_equiv(:)   = 0.0
         dlt_Na_uptake_equiv(:)  = 0.0
         dlt_P_uptake_equiv(:)   = 0.0
         dlt_S_uptake_equiv(:)   = 0.0
         dlt_Cl_uptake_equiv(:)  = 0.0


               !  Get Root Length volume.  Uptakes are distributed in these proportions.
            call get_real_array_optional (crop_module
     :                        , 'rlv'
     :                        , max_layer
     :                        , '(mm/mm^3)'
     :                        , rlv
     :                        , numvals
     :                        , 0.0, 1000000.0 )

            if (numvals .gt.0) then
                  ! crop has rlv
               do 1400 layer=1, e%num_layers
                  root_length_area(layer) = rlv(layer)
     :                                    * e%dlayer(layer)
1400           continue
            else
                  ! crop has no rlv - get root depth

               call get_real_var_optional (crop_module
     :                        , 'root_depth'
     :                        , '(mm)'
     :                        , root_depth
     :                        , numvals
     :                        , 0.0, 20000.0 )
               if (numvals .gt. 0) then
                  call soilpH_rldf (rldf
     :                         , root_depth
     :                         , e%dlayer
     :                         , e%num_layers
     :                         , c%wr_coef)
               do 1500 layer=1, e%num_layers
                  root_length_area(layer) = rldf(layer)
     :                                    * e%dlayer(layer)
1500           continue

               else
                  ok = component_id_to_name(crop_module, module_name)
                  call fatal_error (err_internal
     :                  , ' Cannot get RLV or ROOT_DEPTH from '
     :                  //trim(module_name))
               endif
            endif

            dlt_dm = 1.0
               !  Add in estimate of Ca uptake_equiv for this crop.
            call soilpH_estimate_uptake_equiv (dlt_Ca_uptake_equiv
     :                                , dlt_dm
     :                                , root_length_area
     :                                , e%num_layers
     :                                , p%Ca_dm_percent
     :                                , p%Ca_avail
     :                                , Ca_valency
     :                                , Ca_Kg2Mol)

               !  Add in estimate of Mg uptake_equiv for this crop.
            call soilpH_estimate_uptake_equiv (dlt_Mg_uptake_equiv
     :                                      , dlt_dm
     :                                      , root_length_area
     :                                      , e%num_layers
     :                                      , p%Mg_dm_percent
     :                                      , p%Mg_avail
     :                                      , Mg_valency
     :                                      , Mg_Kg2Mol)

               !  Add in estimate of K uptake_equiv for this crop.
            call soilpH_estimate_uptake_equiv (dlt_K_uptake_equiv
     :                                      , dlt_dm
     :                                      , root_length_area
     :                                      , e%num_layers
     :                                      , p%K_dm_percent
     :                                      , p%K_avail
     :                                      , K_valency
     :                                      , K_Kg2Mol)

               !  Add in estimate of Na uptake_equiv for this crop.
            call soilpH_estimate_uptake_equiv (dlt_Na_uptake_equiv
     :                                      , dlt_dm
     :                                      , root_length_area
     :                                      , e%num_layers
     :                                      , p%Na_dm_percent
     :                                      , p%Na_avail
     :                                      , Na_valency
     :                                      , Na_Kg2Mol)

               !  Add in estimate of P uptake_equiv for this crop.
            call soilpH_estimate_uptake_equiv (dlt_P_uptake_equiv
     :                                      , dlt_dm
     :                                      , root_length_area
     :                                      , e%num_layers
     :                                      , p%P_dm_percent
     :                                      , p%P_avail
     :                                      , H2PO4_valency
     :                                      , P_Kg2Mol)

               !  Add in estimate of S uptake_equiv for this crop.
            call soilpH_estimate_uptake_equiv (dlt_S_uptake_equiv
     :                                      , dlt_dm
     :                                      , root_length_area
     :                                      , e%num_layers
     :                                      , p%S_dm_percent
     :                                      , p%S_avail
     :                                      , SO4_valency
     :                                      , S_Kg2Mol)

               !  Add in estimate of Cl uptake_equiv for this crop.
            call soilpH_estimate_uptake_equiv (dlt_Cl_uptake_equiv
     :                                      , dlt_dm
     :                                      , root_length_area
     :                                      , e%num_layers
     :                                      , p%Cl_dm_percent
     :                                      , p%Cl_avail
     :                                      , Cl_valency
     :                                      , Cl_Kg2Mol)

         uptake_equiv_sum = 0.0
         do 1600 layer =1, e%num_layers
            uptake_equiv_sum = uptake_equiv_sum
     :                       + dlt_Ca_uptake_equiv(layer)
     :                       + dlt_Mg_uptake_equiv(layer)
     :                       + dlt_K_uptake_equiv(layer)
     :                       + dlt_Na_uptake_equiv(layer)
     :                       + dlt_P_uptake_equiv(layer)
     :                       + dlt_S_uptake_equiv(layer)
     :                       + dlt_Cl_uptake_equiv(layer)
1600     continue

         if (uptake_equiv_sum .gt. 0.0) then
            Ca_uptake_equiv(:) = Ca_uptake_equiv(:)
     :                         + dlt_Ca_uptake_equiv(:)
     :                         / uptake_equiv_sum
     :                         * crop_ash_alk_wt

            Mg_uptake_equiv(:) = Mg_uptake_equiv(:)
     :                         + dlt_Mg_uptake_equiv(:)
     :                         / uptake_equiv_sum
     :                         * crop_ash_alk_wt

            K_uptake_equiv(:) =  K_uptake_equiv(:)
     :                        + dlt_K_uptake_equiv(:)
     :                        / uptake_equiv_sum
     :                        * crop_ash_alk_wt

            Na_uptake_equiv(:) = Na_uptake_equiv(:)
     :                         + dlt_Na_uptake_equiv(:)
     :                         / uptake_equiv_sum
     :                         * crop_ash_alk_wt

            P_uptake_equiv(:) =  P_uptake_equiv(:)
     :                        + dlt_P_uptake_equiv(:)
     :                        / uptake_equiv_sum
     :                        * crop_ash_alk_wt

            S_uptake_equiv(:) =  S_uptake_equiv(:)
     :                        + dlt_S_uptake_equiv(:)
     :                        / uptake_equiv_sum
     :                        * crop_ash_alk_wt

            Cl_uptake_equiv(:) = Cl_uptake_equiv(:)
     :                         + dlt_Cl_uptake_equiv(:)
     :                         / uptake_equiv_sum
     :                         * crop_ash_alk_wt
         else
            ! no uptake
         endif
      else
         ! no uptake
      endif

      Cation_uptake_equiv(:) = Ca_uptake_equiv(:)
     :                       + Mg_uptake_equiv(:)
     :                       + K_uptake_equiv(:)
     :                       + Na_uptake_equiv(:)

      Anion_uptake_equiv(:) = P_uptake_equiv(:)
     :                      + S_uptake_equiv(:)
     :                      + Cl_uptake_equiv(:)

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine soilpH_estimate_uptake_equiv (uptake_equiv
     :                                       , dlt_dm
     :                                       , root_length_area
     :                                       , num_layers
     :                                       , dm_percent
     :                                       , fract_avail
     :                                       , valency
     :                                       , Kg2Mol)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    num_layers                     ! (IN)  No of layers.
      real       uptake_equiv(num_layers)       ! (OUT) uptake_equiv of element in each layer.
      real       dlt_dm                         ! (IN) Dry matter uptake_equiv of crop (Kg/Ha).
      real       root_length_area(num_layers)   ! (IN) Root Length Absolute (units not known).
      real       dm_percent                     ! (IN) Moles equiv uptake_equiv per Kg dry matter (mol/kg).
      real       fract_avail(num_layers)        ! (IN) fraction available in layer.
      real       valency                        ! (IN) Valency of component taken up.
      real       Kg2Mol                         ! (IN) Conversion of Kg dm to Mol/Kg dm.

*+  Purpose
*     Estimates root uptake_equiv of a given element in each layer, 'uptake_equiv',
*     for a given crop, given root distribution density 'root_length_area', and
*     availabilty of the element in each layer, 'fract_avail', the amount of
*     dry matter uptake_equiv 'dlt_dm' and the uptake_equiv factor 'dm_percent' for
*     a given element of a given crop.

*+  Mission Statement
*     Estimate root uptake_equiv of a given element from each layer

*+  Changes
*     170699 sb   created

*+  Constant Values
      character  my_name*(*)        ! name of procedure
      parameter (my_name = 'soilpH_estimate_uptake_equiv')

*+  Local Variables
      real       root_length_sum    ! Sum of root 'root_length_area' in layers where 'fract_avail'.
      real       uptake_sum         ! Amount of uptake_equiv of stuff over all layers.
      integer    layer              ! Loop counter to keep track of which layer.

*- Implementation Section ----------------------------------
      call push_routine (my_name)

         !  Sum the root lengths where stuff is available.
      root_length_sum = 0
      do 1100 layer=1, num_layers
            root_length_sum = root_length_sum
     :                      + root_length_area(layer)*fract_avail(layer)
1100  continue

         !  Distribute the stuff where it is available in the proportions of the roots.
      if (root_length_sum .eq. 0)  then
         ! There are no roots where stuff is available.
         uptake_equiv(:) = 0.0
      else
         ! Distribute proportionally according to root length.
         uptake_sum = dlt_dm * dm_percent * pcnt2fract * Kg2Mol*valency
         do 1200 layer=1, num_layers
               uptake_equiv(layer) = uptake_sum
     :                             * fract_avail(layer)
     :                             * divide (root_length_area(layer)
     :                                     , root_length_sum, 0.0)
1200     continue
      endif

      call pop_routine (my_name)
      return
      end subroutine




*     ===========================================================
      subroutine soilpH_rldf (rldf
     :                      , root_depth
     :                      , dlayer
     :                      , num_layers
     :                      , wr_coef)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer num_layers                  ! (IN)  No of layers.
      real rldf(num_layers)               ! (OUT) root length density factor in each layer ().
      real root_depth                     ! (IN) Root depth (mm).
      real dlayer(num_layers)             ! (IN) soil profile layer thicknesses (mm)
      real wr_coef                        ! (IN) Root weighting coefficient.

*+  Purpose
*     Estimates root length density factor for root distribution.

*+  Mission Statement
*     Estimates root length density factor for root distribution.

*+  Changes
*     131099 jngh created

*+  Constant Values
      character  my_name*(*)        ! name of procedure
      parameter (my_name = 'soilpH_rldf')

*+  Local Variables
      real       cumdep             ! Cumulative layer depth (mm)
      real       mid_layer_depth    ! Depth to middle of layer (mm).
      integer    layer              ! Loop counter to keep track of which layer.
      integer    deepest_layer      ! Deepest layer with roots ().
      real       deepest_layer_fract ! fraction of deepest layer with roots (0-1)

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      rldf(:) = 0.0
         !  Sum the root lengths where stuff is available.
      deepest_layer = find_layer_no (root_depth, dlayer
     :                                 , num_layers)
      cumdep = 0
      if (wr_coef .gt. 0.0) then
         do 1100 layer = 1, deepest_layer
            cumdep = cumdep + dlayer(layer)
            mid_layer_depth = cumdep - 0.5 * dlayer(layer)
            rldf(layer) = exp (-4.0 * mid_layer_depth / wr_coef)
1100     continue
      else
      endif
      if (dlayer(deepest_layer) .le. 0.0) then
         deepest_layer_fract = 1.0
     :                    - (cumdep - root_depth)/dlayer(deepest_layer)
         rldf(deepest_layer) = rldf(deepest_layer) * deepest_layer_fract
      else
         rldf(deepest_layer) = 0.0
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine SoilpH_set_other_variables ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*     Update variables owned by other modules.

*+  Mission Statement
*     Update variables owned by other modules

*+  Changes
*     170699 sb   created

*+  Constant Values
      character  my_name*(*)
      parameter (my_name='SoilpH_set_other_variables')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine SoilpH_send_my_variable (variable_name)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      character  variable_name*(*)     ! (IN) Variable name to
                                       ! search for

*+  Purpose
*      Return the value of one of our variables to caller

*+  Mission Statement
*      Return the value of %1 to the calling module

*+  Changes
*     170699 sb   created

*+  Constant Values
      character  my_name*(*)
      parameter (my_name='SoilpH_Send_my_variable')

*+  Local Variables
      real       temp_array(max_layer) ! Holds intermediate values for very short time.

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (variable_name .eq. 'h_equiv_infiltration') then
         call respond2get_real_var (variable_name
     :                              , '(mol/ha)'
     :                              ,  sum(g%H_equiv_infiltration))

      elseif (variable_name .eq. 'infiltration_equiv') then
         call respond2get_real_var (variable_name
     :                              , '(mol/ha)'
     :                              ,  sum(g%H_equiv_infiltration))

      elseif (variable_name .eq. 'h_infiltration_equiv') then
         call respond2get_real_var (variable_name
     :                              , '(mol/ha)'
     :                              , g%H_equiv_infiltration(H_index))

      elseif (variable_name .eq. 'oh_infiltration_equiv') then
         call respond2get_real_var (variable_name
     :                              , '(mol/ha)'
     :                              , g%H_equiv_infiltration(OH_index))

      elseif (variable_name .eq. 'hco3_infiltration_equiv') then
         call respond2get_real_var (variable_name
     :                             , '(mol/ha)'
     :                             , g%H_equiv_infiltration(HCO3_index))

      elseif (variable_name .eq. 'co3_infiltration_equiv') then
         call respond2get_real_var (variable_name
     :                              , '(mol/ha)'
     :                              , g%H_equiv_infiltration(CO3_index))

      elseif (variable_name .eq. 'al_infiltration_equiv') then
         call respond2get_real_var (variable_name
     :                              , '(mol/ha)'
     :                              , g%H_equiv_infiltration(Al_index))

      elseif (variable_name .eq. 'phbc') then
         call respond2get_real_array (variable_name
     :                              , '(Kmol/ha/100mm/ph_unit)'
     :                              , g%pHBC
     :                              , e%num_layers)

      elseif (variable_name .eq. 'phca_old') then
         call respond2get_real_array (variable_name
     :                              , '()'
     :                              , g%pHca_old
     :                              , e%num_layers)

      elseif (variable_name .eq. 'phca') then
         call respond2get_real_array (variable_name
     :                              , '()'
     :                              , g%pHCa
     :                              , e%num_layers)

      elseif (variable_name .eq. 'ph') then
            ! convert pHCa to pH for SoilN processes
         call soilpH_pHCa2pH (g%pH, g%pHCa, e%num_layers)
         call respond2get_real_array (variable_name
     :                              , '()'
     :                              , g%pH
     :                              , e%num_layers)

      elseif (variable_name .eq. 'dlt_phca') then
         call respond2get_real_array (variable_name
     :                              , '()'
     :                              , g%dlt_pHCa_tot
     :                              , e%num_layers)

      elseif (variable_name .eq. 'lime_pool') then
         call respond2get_real_array (variable_name
     :                              , '(mol/ha)'
     :                              , g%lime_pool
     :                              , e%num_layers)

      elseif (variable_name .eq. 'caco3') then
               ! Convert g%lime_pool to (Kg/Ha).
         temp_array(:) = g%lime_pool(:) * CaCO3_mol2kg
         call respond2get_real_array (variable_name
     :                              , '(Kg/Ha)'
     :                              , temp_array
     :                              , e%num_layers)

      elseif (variable_name .eq. 'dlt_lime_pool') then
         call respond2get_real_array (variable_name
     :                              , '(mol/ha)'
     :                              , g%dlt_lime_pool
     :                              , e%num_layers)

      elseif (variable_name .eq. 'dlt_caco3') then
               ! Convert g%dlt_lime_pool to (Kg/Ha).
         temp_array(:) = g%dlt_lime_pool(:) * CaCO3_mol2kg
         call respond2get_real_array (variable_name
     :                              , '(Kg/ha)'
     :                              , temp_array
     :                              , e%num_layers)

      elseif (variable_name .eq. 'h_equiv_mass_flow') then
         call respond2get_real_array (variable_name
     :                             , '(mol/ha)'
     :                             , sum(g%H_equiv_mass_flow_tot, dim=2)
     :                             , e%num_layers)

      elseif (variable_name .eq. 'mass_flow_equiv') then
         call respond2get_real_array (variable_name
     :                             , '(mol/ha)'
     :                             , sum(g%H_equiv_mass_flow_tot, dim=2)
     :                             , e%num_layers)

      elseif (variable_name .eq. 'h_mass_flow_equiv') then
         call respond2get_real_array (variable_name
     :                             , '(mol/ha)'
     :                             , g%H_equiv_mass_flow_tot(:,H_index)
     :                             , e%num_layers)

      elseif (variable_name .eq. 'oh_mass_flow_equiv') then
         call respond2get_real_array (variable_name
     :                             , '(mol/ha)'
     :                             , g%H_equiv_mass_flow_tot(:,OH_index)
     :                             , e%num_layers)

      elseif (variable_name .eq. 'hco3_mass_flow_equiv') then
         call respond2get_real_array (variable_name
     :                           , '(mol/ha)'
     :                           , g%H_equiv_mass_flow_tot(:,HCO3_index)
     :                           , e%num_layers)

      elseif (variable_name .eq. 'co3_mass_flow_equiv') then
         call respond2get_real_array (variable_name
     :                            , '(mol/ha)'
     :                            , g%H_equiv_mass_flow_tot(:,CO3_index)
     :                            , e%num_layers)

      elseif (variable_name .eq. 'al_mass_flow_equiv') then
         call respond2get_real_array (variable_name
     :                             , '(mol/ha)'
     :                             , g%H_equiv_mass_flow_tot(:,Al_index)
     :                             , e%num_layers)

      elseif (variable_name .eq. 'h_equiv_flow_net') then
         call respond2get_real_array (variable_name
     :                              , '(mol/ha)'
     :                              , sum(g%H_equiv_flow_net_tot, dim=2)
     :                              , e%num_layers)

      elseif (variable_name .eq. 'flow_net_equiv') then
         call respond2get_real_array (variable_name
     :                              , '(mol/ha)'
     :                              , sum(g%H_equiv_flow_net_tot, dim=2)
     :                              , e%num_layers)

      elseif (variable_name .eq. 'h_flow_net_equiv') then
         call respond2get_real_array (variable_name
     :                              , '(mol/ha)'
     :                              , g%H_equiv_flow_net_tot(:,H_index)
     :                              , e%num_layers)

      elseif (variable_name .eq. 'oh_flow_net_equiv') then
         call respond2get_real_array (variable_name
     :                              , '(mol/ha)'
     :                              , g%H_equiv_flow_net_tot(:,OH_index)
     :                              , e%num_layers)

      elseif (variable_name .eq. 'hco3_flow_net_equiv') then
         call respond2get_real_array (variable_name
     :                            , '(mol/ha)'
     :                            , g%H_equiv_flow_net_tot(:,HCO3_index)
     :                            , e%num_layers)

      elseif (variable_name .eq. 'co3_flow_net_equiv') then
         call respond2get_real_array (variable_name
     :                             , '(mol/ha)'
     :                             , g%H_equiv_flow_net_tot(:,CO3_index)
     :                             , e%num_layers)

      elseif (variable_name .eq. 'al_flow_net_equiv') then
         call respond2get_real_array (variable_name
     :                              , '(mol/ha)'
     :                              , g%H_equiv_flow_net_tot(:,Al_index)
     :                              , e%num_layers)

      elseif (variable_name .eq. 'dlt_acid_n_cycle') then
         call respond2get_real_array (variable_name
     :                              , '(mol/ha)'
     :                              , g%dlt_acid_N_cycle
     :                              , e%num_layers)

      elseif (variable_name .eq. 'no3_transform_net_mol') then
         call respond2get_real_array (variable_name
     :                              , '(mol/ha)'
     :                              , e%NO3_transform_net_mol_last
     :                              , e%num_layers)

      elseif (variable_name .eq. 'nh4_transform_net_mol') then
         call respond2get_real_array (variable_name
     :                              , '(mol/ha)'
     :                              , e%NH4_transform_net_mol_last
     :                              , e%num_layers)

      elseif (variable_name .eq. 'dlt_lime_dissl') then
         call respond2get_real_array (variable_name
     :                              , '(mol/ha)'
     :                              , g%dlt_lime_dissl
     :                              , e%num_layers)

      elseif (variable_name .eq. 'ash_alk_wt_incorp') then
         call respond2get_real_array (variable_name
     :                              , '(mol/ha)'
     :                              , e%ash_alk_wt_incorp_last
     :                              , e%num_layers)

      elseif (variable_name .eq. 'dlt_acid_org_c_cycle') then
         call respond2get_real_array (variable_name
     :                              , '(mol/ha)'
     :                              , g%dlt_acid_org_C_cycle
     :                              , e%num_layers)

      elseif (variable_name .eq. 'acid_excretion_root') then
         call respond2get_real_array (variable_name
     :                              , '(mol/ha)'
     :                              , g%acid_excretion_root
     :                              , e%num_layers)

      elseif (variable_name .eq. 'tec') then
         call respond2get_real_array (variable_name
     :                              , '(cMol/Kg)'
     :                              , g%tec
     :                              , e%num_layers)

      elseif (variable_name .eq. 'al_exchangable') then
         call respond2get_real_array (variable_name
     :                              , '(cMol/Kg)'
     :                              , g%Al_exchangable
     :                              , e%num_layers)

      elseif (variable_name .eq. 'nh4_uptake_equiv') then
         call respond2get_real_array (variable_name
     :                              , '(mol/ha)'
     :                              , e%NH4_uptake_equiv
     :                              , e%num_layers)

      elseif (variable_name .eq. 'no3_uptake_equiv') then
         call respond2get_real_array (variable_name
     :                              , '(mol/ha)'
     :                              , e%NO3_uptake_equiv
     :                              , e%num_layers)

      elseif (variable_name .eq. 'ca_uptake_equiv') then
         call respond2get_real_array (variable_name
     :                              , '(mol/ha)'
     :                              , e%Ca_uptake_equiv_last
     :                              , e%num_layers)

      elseif (variable_name .eq. 'mg_uptake_equiv') then
         call respond2get_real_array (variable_name
     :                              , '(mol/ha)'
     :                              , e%Mg_uptake_equiv_last
     :                              , e%num_layers)

      elseif (variable_name .eq. 'k_uptake_equiv') then
         call respond2get_real_array (variable_name
     :                              , '(mol/ha)'
     :                              , e%K_uptake_equiv_last
     :                              , e%num_layers)

      elseif (variable_name .eq. 'na_uptake_equiv') then
         call respond2get_real_array (variable_name
     :                              , '(mol/ha)'
     :                              , e%Na_uptake_equiv_last
     :                              , e%num_layers)

      elseif (variable_name .eq. 'p_uptake_equiv') then
         call respond2get_real_array (variable_name
     :                              , '(mol/ha)'
     :                              , e%P_uptake_equiv_last
     :                              , e%num_layers)

      elseif (variable_name .eq. 's_uptake_equiv') then
         call respond2get_real_array (variable_name
     :                              , '(mol/ha)'
     :                              , e%S_uptake_equiv_last
     :                              , e%num_layers)

      elseif (variable_name .eq. 'cl_uptake_equiv') then
         call respond2get_real_array (variable_name
     :                              , '(mol/ha)'
     :                              , e%Cl_uptake_equiv_last
     :                              , e%num_layers)

      elseif (variable_name .eq. 'cation_uptake_equiv') then
         call respond2get_real_array (variable_name
     :                              , '(mol/ha)'
     :                              , e%Cation_uptake_equiv_last
     :                              , e%num_layers)

      elseif (variable_name .eq. 'anion_uptake_equiv') then
         call respond2get_real_array (variable_name
     :                              , '(mol/ha)'
     :                              , e%Anion_uptake_equiv_last
     :                              , e%num_layers)

      elseif (variable_name .eq. 'cation_uptake_equiv_excess') then
         temp_array(:) = e%Cation_uptake_equiv_last(:)
     :                 + e%Anion_uptake_equiv_last(:)
         call respond2get_real_array (variable_name
     :                              , '(mol/ha)'
     :                              , temp_array
     :                              , e%num_layers)

      else
         call message_unused ()
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine SoilpH_set_my_variable (variable_name)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      character  variable_name*(*)     ! (IN) Variable name to
                                       ! search for

*+  Purpose
*     Set one of our variables altered by some other module

*+  Mission Statement
*     Set the variable %1 as requested by some other module

*+  Changes
*     170699 sb   created

*+  Constant Values
      character  my_name*(*)
      parameter (my_name='SoilpH_set_my_variable')

*+  Local Variables
      integer    numvals
      real       temp_array(max_layer)
      character  message_str*500

*- Implementation Section ----------------------------------
      call push_routine(my_name)

       if (variable_name .eq. 'dlt_caco3')  then
         call collect_real_array (variable_name
     :                     , max_layer
     :                     , '(Kg/Ha)'
     :                     , temp_array
     :                     , numvals
     :                     , 0.0, 10000.0)
         call SoilpH_assert (numvals.eq.e%num_layers
     :                 , 'dlt_CaCO3: numvals.eq.e%num_layers')
         message_str =
     :     '     Received dlt_CaCO3 (Lime Pool) (Kg/Ha).  Values ='
     :     // new_line
         call soilpH_real_array2str (message_str
     :                             , temp_array
     :                             , e%num_layers)
         call write_string (message_str)
         temp_array(:) = temp_array(:) * CaCO3_kg2mol
         call add_real_array (temp_array
     :                      , e%dlt_lime_added
     :                      , e%num_layers)

      else if (variable_name .eq. 'caco3')  then
         call collect_real_array (variable_name
     :                     , max_layer
     :                     , '(Kg/Ha)'
     :                     , g%lime_pool
     :                     , numvals
     :                     , 0.0, 10000.0)
         call SoilpH_assert (numvals.eq.e%num_layers
     :                 , 'CaCO3: numvals.eq.e%num_layers')
         message_str =
     :     '     RESET CaCO3 (Lime Pool) (Kg/Ha).  Values =' // new_line
         call soilpH_real_array2str (message_str, g%lime_pool
     :                           , e%num_layers)
         call write_string  (message_str)
         g%lime_pool(:) = g%lime_pool(:) * CaCO3_kg2mol

      else
            ! don't know this variable name
         call message_unused ()
      endif

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine soilpH_ON_Nbalance ()
*     ===========================================================
      Use infrastructure
      implicit none
*+  Purpose
*     Get information of N transformations

*+  Mission Statement
*     Get information of N transformations

*+  Changes
*        070999 jngh

*+  Local Variables
      integer     numvals
      real        NH4_transform_net(max_layer)  ! Net NH4 transformed (kg/ha)
      real        NO3_transform_net(max_layer)  ! Net NO3 transformed (kg/ha)

*+  Constant Values
      character  myname*(*)                     ! name of current procedure
      parameter (myname = 'soilpH_ON_Nbalance')

*- Implementation Section ----------------------------------
      call push_routine (myname)
       NO3_transform_net(:) = 0.0
       NH4_transform_net(:) = 0.0

      call collect_real_array (DATA_NH4_transform_net
     :                         , max_layer
     :                         ,'(kg/ha)'
     :                         ,NH4_transform_net
     :                         ,numvals
     :                         ,-100.0
     :                         ,100.0)


      e%NH4_transform_net_mol(:) = e%NH4_transform_net_mol(:)
     :                           + NH4_transform_net(:) * NH4_kg2mol

      call collect_real_array (DATA_NO3_transform_net
     :                         , max_layer
     :                         ,'(kg/ha)'
     :                         ,NO3_transform_net
     :                         ,numvals
     :                         ,-100.0
     :                         ,100.0)

      e%NO3_transform_net_mol(:) = e%NO3_transform_net_mol(:)
     :                           + NO3_transform_net(:) * NO3_kg2mol

      call pop_routine (myname)
      return
      end subroutine

*     ===========================================================
      subroutine soilpH_ON_Cbalance ()
*     ===========================================================
      Use infrastructure
      implicit none
*+  Purpose
*     Get information of C transformations

*+  Mission Statement
*     Get information of C transformations

*+  Changes
*        070999 jngh
*        160500 jngh corrected Purpose and Mission statement.

*+  Local Variables
      integer numvals
      real       dlt_OM(max_layer)        ! Change in Organic Matter (kg/ha)

*+  Constant Values
      character  myname*(*)               ! name of current procedure
      parameter (myname = 'soilpH_ON_Cbalance')

*- Implementation Section ----------------------------------
      call push_routine (myname)
      dlt_OM(:) = 0.0

      call collect_real_array (DATA_dlt_OM
     :                         , max_layer
     :                         ,'(kg/ha)'
     :                         ,dlt_OM
     :                         ,numvals
     :                         ,-1000.0
     :                         ,1000.0)

 !      write(1000,*) 'SoilpH: '//EVENT_C_balance
 !      write(1000,*) 'SoilpH: '//DATA_dlt_OM
 !     :            , dlt_OM

      e%dlt_OM(:) = e%dlt_OM(:) + dlt_OM(:)

      call pop_routine (myname)
      return
      end subroutine

*     ===========================================================
      subroutine soilpH_ON_Residue_added ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*     Get information of Residue added

*+  Mission Statement
*     Get information of Residue added

*+  Changes
*        230999 jngh

*+  Local Variables
      integer numvals
      character residue_type*(crop_type_size)  ! crop type of residue
      character residue_dm_type*(dm_type_size) ! crop part type of residue
      real      ash_alk_rate                   ! ash alkalinity of this stuff (mol/kg)
      real      dlt_residue_wt                 ! weight of residue change (kg/ha)
      real      dlt_residue_ash_alk_wt         ! weight of ash alkalinity change of residue (mol/ha)
      character string*(400)

*+  Constant Values
      character myname*(*)               ! name of current procedure
      parameter (myname = 'soilpH_ON_Residue_added')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call collect_char_var (DATA_residue_type
     :                     , '()'
     :                     , residue_type
     :                     , numvals)

         ! ----------------------------------------------------------
         !    Find the amount of residue added
         ! ----------------------------------------------------------

      call collect_real_var (DATA_dlt_residue_wt
     :                     , '(kg/ha)'
     :                     , dlt_residue_wt
     :                     , numvals
     :                     , 0.0, 100000.0)

         ! ----------------------------------------------------------
         !    Find the type of residue added
         ! ----------------------------------------------------------

      call collect_char_var (DATA_dm_type
     :                     , '()'
     :                     , residue_dm_type
     :                     , numvals)

         ! ----------------------------------------------------------
         !     Get Residue Ash Alkalinity from LookUp Table
         ! ----------------------------------------------------------
      call soilpH_ash_alk_rate (ash_alk_rate
     :                         , residue_type
     :                         , residue_dm_type)

         ! ----------------------------------------------------------
         !    Calculate addition of residue ash alkalinity and update pools
         ! ----------------------------------------------------------
      dlt_residue_ash_alk_wt = ash_alk_rate
     :                       * dlt_residue_wt
      g%residue_ash_alk_wt = g%residue_ash_alk_wt
     :                     + dlt_residue_ash_alk_wt

      if (p%report_additions.eq.'yes') then

         write (string, '(1x, 2a, 2(40x, 3a), 2(40x, a, f10.2, a))')
     :      ' Residue Added', new_line
     :     ,'Residue Type          = ', trim(residue_type), new_line
     :     ,'Dry matter Type       = ', trim(residue_dm_type), new_line
     :     ,'Amount Added (kg/ha)  = ', dlt_residue_wt, new_line
     :     ,'Ash Alkalinity wt (Mole/ha)= ', dlt_residue_ash_alk_wt
     :                                    , new_line

         call Write_string (string)

      else
         ! The user has asked for no reports for additions of residue
         ! in the summary file.
      endif
 !      write(1000,*) 'SoilpH: '//EVENT_Residue_added
 !      write(1000,*) 'SoilpH: '//DATA_residue_type
 !     :            , ' '//trim(residue_type)
 !      write(1000,*) 'SoilpH: '//DATA_dlt_residue_wt
 !     :            , dlt_residue_wt


      call pop_routine (myname)
      return
      end subroutine

*     ===========================================================
      subroutine soilpH_ON_Residue_removed ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*     Get information of Residue removed

*+  Mission Statement
*     Get information of Residue removed

*+  Changes
*        230999 jngh

*+  Local Variables
      integer numvals
      character residue_removed_action*(action_type_size)    ! action of residue removal
      real      dlt_residue_fraction           ! fraction of residue removed (0-1)
      character string*(300)
      real      dlt_residue_ash_alk_wt         ! changge in ash alkalinity weight (mol/ha)
      real      dlt_residue_ash_alk_wt_incorp  ! ash alkalinity weight incorporated (mol/ha)
      real      residue_incorp_fraction(max_layer) ! proportions of residue distributed to each layer (0-1)
      integer   layer                          ! layer number of profile
      real      ash_alk_loss_fract             ! fraction of ash alkalinity lost (0-1)

*+  Constant Values
      character  myname*(*)               ! name of current procedure
      parameter (myname = 'soilpH_ON_Residue_removed')

*- Implementation Section ----------------------------------
      call push_routine (myname)
      residue_removed_action = ' '
      dlt_residue_fraction = 0.0
      residue_incorp_fraction(:) = 0.0

      call collect_char_var (DATA_residue_removed_action
     :                     , '()'
     :                     , residue_removed_action
     :                     , numvals)

         ! ----------------------------------------------------------
         !    Find the fraction of residue removed
         ! ----------------------------------------------------------

      call collect_real_var (DATA_dlt_residue_fraction
     :                     , '()'
     :                     , dlt_residue_fraction
     :                     , numvals
     :                     , 0.0, 1.0)

      call collect_real_array (DATA_residue_incorp_fraction
     :                     , max_layer
     :                     , '()'
     :                     , residue_incorp_fraction
     :                     , numvals
     :                     , 0.0, 1.0)
!      write(*,*) 'SoilpH: '//EVENT_Residue_removed
!      write(*,*) 'SoilpH: '//DATA_residue_removed_action
!     :            , ' '//trim(residue_removed_action)
!      write(*,*) 'SoilpH: '//DATA_dlt_residue_fraction
!     :            , dlt_residue_fraction
!      write(*,*) 'SoilpH: '//DATA_residue_incorp_fraction
!     :            , residue_incorp_fraction



         ! ----------------------------------------------------------
         !    Calculate removal of ash alkalinity and update pools
         ! ----------------------------------------------------------
      call soilpH_ash_alk_loss_fract (ash_alk_loss_fract
     :                              , residue_removed_action)

      dlt_residue_ash_alk_wt = g%residue_ash_alk_wt
     :                       * dlt_residue_fraction

      g%residue_ash_alk_wt = g%residue_ash_alk_wt
     :                     - dlt_residue_ash_alk_wt
      if (sum(residue_incorp_fraction) .gt. 0.0) then
         do 1000 layer=1, numvals
            dlt_residue_ash_alk_wt_incorp =  dlt_residue_ash_alk_wt
     :                                 * (1.0 - ash_alk_loss_fract)
     :                                 * residue_incorp_fraction(layer)
            e%ash_alk_wt_incorp(layer) = e%ash_alk_wt_incorp(layer)
     :                                 + dlt_residue_ash_alk_wt_incorp
!         print*, 'SpH:ash_alk_incorp(',layer,')='
!     :                              , dlt_residue_ash_alk_wt_incorp
1000     continue
      else
         dlt_residue_ash_alk_wt_incorp = dlt_residue_ash_alk_wt
     :                                 * (1.0 - ash_alk_loss_fract)
         e%ash_alk_wt_incorp(1) = e%ash_alk_wt_incorp(1)
     :                          + dlt_residue_ash_alk_wt_incorp
!         print*, 'SpH:ash_alk_incorp(1)='
!     :                              , dlt_residue_ash_alk_wt_incorp
      endif

      if (p%report_additions.eq.'yes') then

         write (string
     :     , '(1x, 2a, 40x, 3a, 40x, a, f10.5, a, 40x, a, f10.2, a)' )
     :      ' Removed Residue', new_line
     :     ,'Removed action  = ', trim(residue_removed_action), new_line
     :     ,'Fraction Removed  = ', dlt_residue_fraction, new_line
     :     ,'Ash Alkalinity wt (Mole/ha) = ', dlt_residue_ash_alk_wt
     :                                      , new_line

         call Write_string (string)

      else
         ! The user has asked for no reports for additions of residue
         ! in the summary file.
      endif

      call pop_routine (myname)
      return
      end subroutine

*     ===========================================================
      subroutine soilpH_ON_Crop_chopped ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*     Get information of Residue removed

*+  Mission Statement
*     Get information of Residue removed

*+  Changes
*        230999 jngh

*+  Local Variables
      integer numvals                          ! number of values returned
      character crop_type*(crop_type_size)     ! type of crop
      character dm_type(max_dm_type)*(dm_type_size) ! type of crop part
      real      dlt_crop_dm(max_dm_type)       ! change in crop dry matter (kg/ha)
      character string*(400)
      real      ash_alk_rate                   ! ash alkalinity of drymatter type (mol/kg)
      real      dlt_crop_ash_alk_wt            ! ash alkalinity weight of drymatter (mol/ha)
      integer   crop_module                    ! posting module - crop name
      integer   type
      integer   num_types

*+  Constant Values
      character myname*(*)               ! name of current procedure
      parameter (myname = 'soilpH_ON_Crop_chopped')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call collect_char_var (DATA_crop_type
     :                     , '()'
     :                     , crop_type
     :                     , numvals)

      call collect_char_array (DATA_dm_type
     :                     , max_dm_type
     :                     , '()'
     :                     , dm_type
     :                     , num_types)

         ! ----------------------------------------------------------
         !    Find the fraction of residue removed
         ! ----------------------------------------------------------

      call collect_real_array (DATA_dlt_crop_dm
     :                     , max_dm_type
     :                     , '(kg/ha)'
     :                     , dlt_crop_dm
     :                     , numvals
     :                     , 0.0, 100000.0)

         ! ----------------------------------------------------------
         !    Calculate ash alkalinity
         ! ----------------------------------------------------------

      call collect_integer_var ('sender_id'
     :                     , '()'
     :                     , crop_module
     :                     , numvals
     :                     , 0
     :                     , 1000000)

      do 1000 type = 1, num_types
         if (dlt_crop_dm(type) .gt. 0.0) then
            call soilpH_ash_alk_rate (ash_alk_rate, crop_type
     :                              , dm_type(type))
            dlt_crop_ash_alk_wt = ash_alk_rate * dlt_crop_dm(type)

            e%crop_ash_alk_wt = e%crop_ash_alk_wt
     :                        + dlt_crop_ash_alk_wt


            if (p%report_additions.eq.'yes') then
               write (string
     :         , '(1x, 2a, 2(40x, 3a), 2(40x, a, f10.2, a))' )
     :           ' Crop Chopped', new_line
     :         ,'Crop Type           = ', trim(crop_type), new_line
     :         ,'Dry Matter Type     = ', trim(dm_type(type)), new_line
     :         ,'Dry matter Removed (kg/ha)  = ', dlt_crop_dm(type)
     :                                          , new_line
 !     :        ,'Dry matter Destination  = ', destination_dm, new_line
     :         ,'Ash Alkalinity wt (Mole/ha) = ', dlt_crop_ash_alk_wt
     :                                      , new_line

               call Write_string (string)
            else
               ! The user has asked for no reports for additions of residue
               ! in the summary file.
            endif
 !      write(1000,*) 'SoilpH: '//EVENT_Crop_chopped
 !      write(1000,*) 'SoilpH: '//DATA_crop_type
 !     :            , ' '//trim(crop_type)
 !      write(1000,*) 'SoilpH: '//DATA_dm_type(type)
 !     :            , ' '//trim(dm_type)
 !      write(1000,*) 'SoilpH: '//DATA_dlt_crop_dm(type)
 !     :            , dlt_crop_dm
 !      write(1000,*) 'SoilpH: '//DATA_destination_dm
 !     :            , destination_dm
 !      write(1000,*) 'SoilpH: '//'dlt_crop_ash_alk_wt'
 !     :            , dlt_crop_ash_alk_wt

         call SoilpH_uptake_equiv (crop_module
     :                         , dlt_crop_ash_alk_wt
     :                         , e%Ca_uptake_equiv
     :                         , e%Mg_uptake_equiv
     :                         , e%K_uptake_equiv
     :                         , e%Na_uptake_equiv
     :                         , e%P_uptake_equiv
     :                         , e%S_uptake_equiv
     :                         , e%Cl_uptake_equiv
     :                         , e%Cation_uptake_equiv
     :                         , e%Anion_uptake_equiv
     :                         )

         else
            ! nothing to add
         endif
1000  continue

      call pop_routine (myname)
      return
      end subroutine

*     ===========================================================
      subroutine soilpH_ash_alk_rate (ash_alk_rate, crop_type, dm_type)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real      ash_alk_rate                 ! (OUTPUT) ash alkalinity of drymatter (mol/kg)
      character crop_type*(crop_type_size)   ! (INPUT) type of crop
      character dm_type*(dm_type_size)       ! (INPUT) type of crop part

*+  Purpose
*     Get information of Residue removed

*+  Mission Statement
*     Get information of Residue removed

*+  Changes
*        230999 jngh

*+  Local Variables
      integer    numvals                        ! number of values returned
      character  string*(400)
      real       dm_ash_alk_rate(max_dm_type)   ! ask alkalinity of drymatter (mol/kg)
      character  name_dm_type(max_dm_type)*(dm_type_size)  ! dry matter types of crop
      integer    index_dm_type                  ! index of plant part type
      integer    index_crop_type                ! index of crop type
      integer    num_dm_type                    ! number of dry matter types

*+  Constant Values
      character myname*(*)                      ! name of current procedure
      parameter (myname = 'soilpH_ash_alk_rate')

*- Implementation Section ----------------------------------
      call push_routine (myname)

         ! ----------------------------------------------------------
         !    Have we already read this crop?
         ! ----------------------------------------------------------

      index_crop_type = Find_string_in_array(crop_type, c%crop_type
     :                                    , c%num_crops)

      if (index_crop_type .lt.1 ) then
            ! We don't have this crop yet
         if (p%report_additions.eq.'yes') then
            call write_string (
     :                new_line//'   - Reading crop ash alkalinity')
         else
            ! The user has asked for no reports for crop chops
            ! in the summary file.
         endif

         name_dm_type(:) = blank
         call read_char_array (
     :           crop_type
     :         , keyword_name_dm_type ! Keyword
     :         , max_dm_type          ! size
     :         , '()'                 ! Units
     :         , name_dm_type         ! Variable
     :         , num_dm_type)             ! Number of values returned

         if (num_dm_type .lt. 1) then
            string = 'Cannot find dry matter type names '
     :          // 'for crop:- '  // trim(crop_type)
            call fatal_error (err_user, string)
            index_crop_type = 0
         else
            c%num_crops = c%num_crops + 1
            c%crop_type(c%num_crops) = crop_type
            c%num_dm_type(c%num_crops) = num_dm_type
            c%name_dm_type(c%num_crops,:) = name_dm_type(:)
            index_crop_type = c%num_crops
            index_dm_type = num_dm_type

            call read_real_array (
     :           crop_type
     :         , keyword_ash_alkalinity ! Keyword
     :         , max_dm_type          ! size
     :         , '(cMol/Kg)'          ! Units
     :         , dm_ash_alk_rate      ! Variable
     :         , numvals              ! Number of values returned
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 100.0)               ! Upper Limit for bound checking

            if (numvals .lt.1) then
               string = 'Cannot find ash alkalinity for crop:- '
     :               //crop_type
               call fatal_error (err_user, string)

               index_crop_type = 0
            elseif (numvals .ne. num_dm_type) then
               string = 'Number of dry matter type names does not match'
     :               //' number of ash alkalinity values for crop:- '
     :               //crop_type
               call fatal_error (err_user, string)
               index_crop_type = 0

            else
               c%ash_alk_tbl_crop(c%num_crops,:) = dm_ash_alk_rate(:)

            endif
         endif
      else
         ! we already have read this crop's ash alkalinity data
      endif

      if (index_crop_type.gt.0) then
         ! ----------------------------------------------------------
         !     Get Crop Ash Alkalinity from LookUp Table
         ! ----------------------------------------------------------

         index_dm_type = Find_string_in_array(dm_type
     :                             , c%name_dm_type(index_crop_type,:)
     :                             , c%num_dm_type(index_crop_type))

         if (index_dm_type .lt.1
     :      .or. index_dm_type .gt. max_dm_type) then
            string = 'Cannot find ash alkalinity for crop:- '
     :               //trim(crop_type)
     :               //', dry-matter type:- '//trim(dm_type)
            call fatal_error (err_user, string)
            ash_alk_rate = 0.0
         else
         ! ----------------------------------------------------------
         !    Calculate ash alkalinity
         ! ----------------------------------------------------------

            ash_alk_rate
     :              = c%ash_alk_tbl_crop(index_crop_type, index_dm_type)
     :              * cmol2mol
         endif
      else
         ash_alk_rate = 0.0
      endif


      call pop_routine (myname)
      return
      end subroutine

*     ===========================================================
      subroutine soilpH_ash_alk_loss_fract (ash_alk_loss_fract
     :                                    , action_type)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real      ash_alk_loss_fract             ! (OUTPUT) fraction of ash alkalinity lost (0-1)
      character action_type*(action_type_size) ! (INPUT) type of removal action

*+  Purpose
*     Get information of Residue removed action

*+  Mission Statement
*     Get information of Residue removed action

*+  Changes
*        230999 jngh

*+  Local Variables
      integer    numvals                        ! number of values returned
      character  string*(400)
      integer    index_action_type              ! index of action type

*+  Constant Values
      character myname*(*)                      ! name of current procedure
      parameter (myname = 'soilpH_ash_alk_loss_fract')

*- Implementation Section ----------------------------------
      call push_routine (myname)

         ! ----------------------------------------------------------
         !    Have we already read this action?
         ! ----------------------------------------------------------

      index_action_type = Find_string_in_array(action_type
     :                                       , c%action_type
     :                                       , c%num_actions)
      if (index_action_type .lt.1 ) then
            ! We don't have this action yet
         if (p%report_additions.eq.'yes') then
            call write_string (
     :             new_line//'    - Reading action ash alkalinity loss')
         else
            ! The user has asked for no reports for action losses
            ! in the summary file.
         endif

         call read_real_var_optional (
     :           section_ash_alkalinity_loss
     :         , action_type           ! Keyword
     :         , '()'                  ! Units
     :         , ash_alk_loss_fract    ! Variable
     :         , numvals              ! Number of values returned
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 1.0)                 ! Upper Limit for bound checking

         if (numvals .lt.1) then
            string = 'Cannot find ash alkalinity loss for action:- '
     :               //action_type
            call fatal_error (err_user, string)
         else
            c%num_actions = c%num_actions + 1
            c%action_type(c%num_actions) = action_type
            c%ash_alk_tbl_action(c%num_actions) = ash_alk_loss_fract
            index_action_type = c%num_actions
         endif

      else
            ! we already have read this action's ash alkalinity data
         if (index_action_type.gt.0) then
            ! ----------------------------------------------------------
            !     Get action Ash Alkalinity loss from LookUp Table
            ! ----------------------------------------------------------

            ash_alk_loss_fract = c%ash_alk_tbl_action(index_action_type)
         else
            ash_alk_loss_fract = 0.0
         endif

      endif


      call pop_routine (myname)
      return
      end subroutine

*     ===========================================================
      subroutine SoilpH_init_calc ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*       Initialize soil pH variables.

*+  Mission Statement
*       Initialize soil pH variables

*+  Changes
*     170699 sb   created

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'SoilpH_init_calc')

*+  Local Variables
      integer layer                    ! layer number of profile

*- Implementation Section ----------------------------------
      call push_routine (my_name)

         ! Initialise pools.
      do 110 layer=1, e%num_layers
         g%lime_pool(layer) = p%lime_pool_init(layer)
         g%pHCa(layer) = p%pHCa_initial(layer)
110   continue

         ! Determine SALS.
      do 120 layer=1, e%num_layers
         call soilpH_sAls_calc (g%sAls_calc(layer)
     :                        , p%Al_conc_init(layer)
     :                        , p%pHCa_initial(layer))
         call soilpH_sAls (g%sAls(layer)
     :                   , g%sAls_calc(layer)
     :                   , p%sAls_supplied(layer)
     :                   , p%sAls_supplied_use_flag(layer)
     :                   , layer)
120   continue

         ! Calculate initial TEC.
      do 130 layer=1, e%num_layers
         g%tec_init(layer) = p%ecec_init(layer)
     :                     - p%Al_conc_init(layer)
130   continue

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine soilpH_sAls (sAls
     :                   , sAls_calc
     :                   , sAls_supplied
     :                   , sAls_supplied_use_flag
     :                   , layer)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       sAls                     ! (IN) Slope of Al versus H+.
      real       sAls_calc                ! (IN) Calculated slope of Al versus H+.
      real       sAls_supplied            ! (IN) User supplied slope of Al versus H+.
      logical    sAls_supplied_use_flag   ! (IN) Use user supplied sAls.
      integer    layer                    ! (IN) Which layer (for error messages)

*+  Purpose
*     Chooses whether to assign calc'd sAls or user supplied sAls to 'sAls'
*     and then does the assignment.

*+  Mission Statement
*     Set %1 to either calc'd sAls or user supplied sAls

*+  Changes
*     170699 sb   created

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'soilpH_sAls')

*+  Local Variables
      character  err_msg*250
      character  num_str*20
      real       relative_error

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (sAls_supplied_use_flag)  then
            ! use supplied sAls
         if (sAls_supplied .lt. 0) then
               ! sAls not supplied
            call integer_var_to_string (layer, num_str)
            err_msg = 'sAls_supplied_use_flag set and no sAls supplied'
     :             // ' for layer ' // trim (num_str) // ' !'
            call fatal_error (err_user, err_msg)
         else
               ! use supplied sAls
            sAls = sAls_supplied
         endif
      else if (sAls_calc .lt. 0) then
            ! sAls not calculated
         if (sAls_supplied .lt. 0) then
               ! sAls not supplied
            call integer_var_to_string (layer, num_str)
            err_msg = 'Could not calculate SALS and not supplied'
     :             // ' for layer ' // trim(num_str) // ' !'
     :             // new_line // '(cant calc as pHCa is too high)'
            call fatal_error (err_user, err_msg)
         else
               ! sAls is supplied
            sAls = sAls_supplied
         endif
      else
            ! sAls is calculated
         if (sAls_supplied .ge. 0) then
               ! sAls is also supplied
            relative_error = divide (abs (sAls_supplied - sAls_calc)
     :                       ,sAls_calc, 0.0)
            if (relative_error .gt. 0.1) then
                  ! supplied sAls is outrageous
               write (err_msg, '(A,G13.5,A,G13.5,A,I3,A,)')
     :                'Calcd SALS = ', sAls_calc
     :              , ' and supplied SALS = '
     :              , sAls_supplied
     :              , ' for layer '
     :              , layer
     :              , ' !'
               call warning_error (err_user, err_msg)
            else
               ! supplied sAls are not outrageous
            endif
         else
            ! sAls not supplied
         endif
            ! use calculated sAls
         sAls = sAls_calc
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine soilpH_sAls_calc (sAls_calc, Al_conc, pHCa)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       sAls_calc       ! (OUT) Slope of Al versus H (cMol/Kg/Mol/1000.0*L).
      real       Al_conc         ! (IN) Aluminium concentration (cMol/Kg).
      real       pHCa            ! (IN) Measure of acidity.

*+  Purpose
*     Calculates Slope of Al versus H+.

*+  Mission Statement
*     Calculate Slope of Al versus H+

*+  Changes
*     170699 sb   created

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'soilpH_sAls_calc')

*+  Local Variables
      real       H_conc    ! Molar concentration of H+ (Moles H+ / Litre)

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (pHCa .le. 4.8) then
         H_conc = 10.0 ** (-pHCa)
         sAls_calc = divide (Al_conc, (H_conc * 1000.0), 0.0)
      else
         sAls_calc = -1
      end if

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine SoilpH_assert (isOK, err_msg)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      character  err_msg*(*)     ! (IN) Error message.
      logical    isOK            ! (IN) Did the test pass ?

*+  Purpose
*     Gives error message on failure of test 'isOK'.

*+  Mission Statement
*     Give error message and abort if not %1

*+  Changes
*     170699 sb   created

*+  Constant Values
      character  my_name*(*)
      parameter (my_name='SoilpH_assert')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (.not. isOK) then
         call fatal_error (err_user, 'Test Failed: '//err_msg)
!cjh         print *, 'ASSERT FAIL: '//err_msg
      end if

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine soilpH_real_array2str (str, array, num_vars)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      character str*(*)       ! (INOUT)
      integer num_vars        ! (IN)
      real array(num_vars)    ! (IN)

*+  Purpose
*     Prints real array to string.

*+  Mission Statement
*     Print real array %2 to string %1
*
*   Definition
*     Prints out each element of the array "arr" in order onto the end subroutine
*     of "str".  Every group of 5 elements is terminated by a newline.
*     If the last group of numbers contains less than five elements, a
*     newline will follow anyway.

*+  Changes
*     170699 sb   created

*+  Constant Values
      character   my_name*(*)           ! name of procedure
      parameter  (my_name = 'soilpH_real_array2str')

      integer     nums_per_line
      parameter  (nums_per_line=5)

      integer num_width
      parameter (num_width=14)

*+  Local Variables
      character num_str*(num_width)
      integer i
      integer last_nb

*- Implementation Section ----------------------------------
      call push_routine (my_name)

         !  Write out each element of 'array'.
      do 10 i=1, num_vars
         write (num_str,'(1x,g13.5)') array(i)
         last_nb = len_trim (num_str)

         if (last_nb .lt. num_width) then
            num_str = num_str(last_nb+1:) // num_str(1:last_nb)
         end if
         call append_string (str, num_str)

         if (mod (i,nums_per_line) .eq. 0)  then
            call append_string (str,new_line)
         end if
10    continue

         !  Assign a newline if it hasnt just been assigned.
      if (mod (num_vars,nums_per_line) .ne. 0)  then
         call append_string (str,new_line)
      end if

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine SoilpH_process ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*       Do things for each time step.

*+  Mission Statement
*       Simulate a single day's soil acidity

*+  Changes
*     170699 sb   created

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'SoilpH_process')

*+  Local Variables
      integer layer

*- Implementation Section ----------------------------------
      call push_routine (my_name)

         !  Calculate pH buffer capacity, TEC, and Al conc for each layer.
      do 1000 layer=1, e%num_layers
         call SoilpH_tec (g%tec(layer)
     :               , g%tec_init(layer)
     :               , p%pHCa_initial(layer)
     :               , g%sAls(layer)
     :               , e%org_C_fract(layer)
     :               , g%pHCa(layer) )
         call soilpH_Al_exchangable (g%Al_exchangable(layer)
     :                           , g%sAls(layer)
     :                           , g%pHCa(layer) )
         if (p%pHBC_method .eq. pHBC_method_parameters) then
            call soilpH_pHBC_Parameters (g%pHBC(layer)
     :                   , p%pHBC(layer))

         else if (p%pHBC_method .eq. pHBC_method_Hochman) then
            call soilpH_pHBC_Hochman (g%pHBC(layer)
     :                   , g%tec(layer)
     :                   , g%Al_exchangable(layer)
     :                   , g%sAls(layer)
     :                   , e%org_C_fract(layer)
     :                   , g%pHCa(layer) )
         else
            ! pHBC method is unknown.
            call fatal_error (err_user
     :     , 'Unknown pHBC method specified in parameters: '
     :     // trim(p%pHBC_method))

         endif
1000  continue

         !  Adjust lime pool in each layer.
         ! Use water flowing in instead of water flowing out
         call soilpH_dlt_lime(
     :                g%dlt_lime_pool(1)
     :              , g%dlt_lime_dissl(1)
     :              , g%lime_pool(1)
     :              , e%dlt_lime_added(1)
     :              , e%infiltration_mm
     :              , g%pHCa(1)
     :              , c%lime_sol_tbl_pHCa
     :              , c%lime_sol_tbl_lime
     :              , c%lime_sol_tbl_size
     :                  )
      do 1100 layer=2, e%num_layers
         call soilpH_dlt_lime(
     :                g%dlt_lime_pool(layer)
     :              , g%dlt_lime_dissl(layer)
     :              , g%lime_pool(layer)
     :              , e%dlt_lime_added(layer)
     :              , e%flow_water(layer-1)
     :              , g%pHCa(layer)
     :              , c%lime_sol_tbl_pHCa
     :              , c%lime_sol_tbl_lime
     :              , c%lime_sol_tbl_size
     :                  )
1100  continue

         !  'e%dlt_lime_added' is a delta.  It has just been used, so set it to zero.
      e%dlt_lime_added(:) = 0.0

         !  Acid added due to nitrogen cycle.
      do 1150 layer=1, e%num_layers
 !cjh!        print *, 'layer,day,year=', layer,e%day,e%year
         call SoilpH_acid_N_cycle (g%dlt_acid_N_cycle(layer)
     :              , e%NO3_transform_net_mol(layer)
     :              , e%NH4_transform_net_mol(layer))

!         print*, 'layer, g%dlt_acid_N_cycle(layer) '
!     :             //' , e%NO3_transform_net_mol(layer) '
!     :             //' , e%NH4_transform_net_mol(layer)) '
!
!         print*, layer, g%dlt_acid_N_cycle(layer)
!     :              , e%NO3_transform_net_mol(layer)
!     :              , e%NH4_transform_net_mol(layer)

1150  continue
         !  Root excretion of acid.
      do 1170 layer=1, e%num_layers
         call soilpH_acid_excretion_root (
     :                            g%acid_excretion_root(layer)
     :                           , e%NO3_uptake_equiv(layer)
     :                           , e%NH4_uptake_equiv(layer)
     :                           , e%Ca_uptake_equiv(layer)
     :                           , e%Mg_uptake_equiv(layer)
     :                           , e%K_uptake_equiv(layer)
     :                           , e%Na_uptake_equiv(layer)
     :                           , e%P_uptake_equiv(layer)
     :                           , e%S_uptake_equiv(layer)
     :                           , e%Cl_uptake_equiv(layer)
     :                          )
1170  continue

         !  Organic acid accumulated due to increase in humic carbon.
      do 1180 layer=1, e%num_layers
         call SoilpH_acid_org_C_cycle (g%dlt_acid_org_C_cycle(layer)
     :                                 , e%dlt_OM(layer)
     :                                 , p%hum_acid_slope(layer)
     :                                 , p%hum_acid_pHCa_offset(layer)
     :                                 , g%pHCa(layer) )
1180  continue

      g%pHca_old(:) = g%pHCa(:)

         !  Calculate ion balance infiltrating into the soil surface.
      call soilpH_H_equiv_mass_flow (g%H_equiv_infiltration(:)
     :                             , p%pH_rain
     :                             , 0.0
     :                             , e%infiltration_mm
     :                             , c%CO2_pressure_atm
     :                             , 0.0
     :                             , 0.0
     :                             , p%ionic_strength_rain)

      g%H_equiv_mass_flow(:,:)      = 0.0
      g%H_equiv_mass_flow_tot(:,:)  = 0.0
      g%H_equiv_flow_net(:,:)       = 0.0
      g%H_equiv_flow_net_tot(:,:)   = 0.0

      g%dlt_pHCa(:) = 0.0
      g%dlt_pHCa_tot(:) = 0.0

         !  Upward Flow of hydrogen ions out of each layer.
      do 1200 layer=e%num_layers, 2, -1
         if (e%flow_water(layer-1) .lt .0.0) then
            call soilpH_pHCa2pH (g%pH(layer), g%pHCa(layer), 1)
            call soilpH_H_equiv_mass_flow(g%H_equiv_mass_flow(layer-1,:)
     :                                  , g%pH(layer)
     :                                  , g%pHCa(layer)
     :                                  , e%flow_water(layer-1)
     :                                  , p%CO2_pressure_soil(layer)
     :                                  , p%pAl_pHca_slope(layer)
     :                                  , p%pAl_pHCa_intercept(layer)
     :                                , p%ionic_strength_initial(layer))
         else
            g%H_equiv_mass_flow(layer-1,:) = 0.0
         endif
            !  Net flow of hydrogen ions into each layer.
         g%H_equiv_flow_net(layer,:) = g%H_equiv_mass_flow(layer-1,:)
     :                               - g%H_equiv_mass_flow(layer,:)

               !  Difference in pHCa for up flow.
         call soilpH_dlt_pH (
     :                g%dlt_pHCa(layer)
     :               , g%pHBC(layer)
     :               , 0.0
     :               , 0.0
     :               , sum(g%H_equiv_flow_net(layer,:))
     :               , 0.0
     :               , 0.0
     :               , 0.0
     :               , e%dlayer(layer)
     :               )
         g%pHCa(layer) = g%pHCa(layer) + g%dlt_pHCa(layer)
1200  continue
      call soilpH_dlt_pH (
     :                g%dlt_pHCa(1)
     :               , g%pHBC(1)
     :               , 0.0
     :               , 0.0
     :               , - sum(g%H_equiv_mass_flow(1,:))
     :               , 0.0
     :               , 0.0
     :               , 0.0
     :               , e%dlayer(layer)
     :               )
      g%pHCa(layer) = g%pHCa(1) + g%dlt_pHCa(1)
      g%dlt_pHCa_tot(:) = g%dlt_pHCa_tot(:)
     :                  + g%dlt_pHCa(:)

      g%H_equiv_mass_flow_tot(:,:) = g%H_equiv_mass_flow_tot(:,:)
     :                             + g%H_equiv_mass_flow(:,:)
      g%H_equiv_flow_net_tot(:,:) = g%H_equiv_flow_net_tot(:,:)
     :                            + g%H_equiv_flow_net(:,:)

      g%H_equiv_mass_flow(:,:) = 0.0

            !  Downward Flow of hydrogen ions out of each layer.
      do 1300 layer=1, e%num_layers
         if (e%flow_water(layer) .ge .0.0) then
            call soilpH_pHCa2pH (g%pH(layer), g%pHCa(layer), 1)
            call soilpH_H_equiv_mass_flow (g%H_equiv_mass_flow(layer,:)
     :                                    , g%pH(layer)
     :                                    , g%pHCa(layer)
     :                                    , e%flow_water(layer)
     :                                    , p%CO2_pressure_soil(layer)
     :                                    , p%pAl_pHca_slope(layer)
     :                                    , p%pAl_pHCa_intercept(layer)
     :                               , p%ionic_strength_initial(layer))
         else
            g%H_equiv_mass_flow(layer,:) = 0.0
         endif

            !  Net flow of hydrogen ions into each layer.
         if (layer .eq.1) then
            g%H_equiv_flow_net(1,:) = g%H_equiv_infiltration(:)
     :                              - g%H_equiv_mass_flow(1,:)
         else
            g%H_equiv_flow_net(layer,:) = g%H_equiv_mass_flow(layer-1,:)
     :                                  - g%H_equiv_mass_flow(layer,:)
         endif

            !  Difference in pHCa for down flow.
         call soilpH_dlt_pH (
     :                g%dlt_pHCa(layer)
     :               , g%pHBC(layer)
     :               , 0.0
     :               , 0.0
     :               , sum(g%H_equiv_flow_net(layer,:))
     :               , 0.0
     :               , 0.0
     :               , 0.0
     :               , e%dlayer(layer)
     :               )
        g%pHCa(layer) = g%pHCa(layer) + g%dlt_pHCa(layer)
1300  continue

      g%H_equiv_mass_flow_tot(:,:) = g%H_equiv_mass_flow_tot(:,:)
     :                             + g%H_equiv_mass_flow(:,:)
      g%H_equiv_flow_net_tot(:,:) = g%H_equiv_flow_net_tot(:,:)
     :                            + g%H_equiv_flow_net(:,:)

      g%dlt_pHCa_tot(:) = g%dlt_pHCa_tot(:)
     :                  + g%dlt_pHCa(:)

      do 1400 layer=1, e%num_layers

            !  Difference in pHCa for other processes.
         call soilpH_dlt_pH (
     :                g%dlt_pHCa(layer)
     :               , g%pHBC(layer)
     :               , g%dlt_acid_N_cycle(layer)
     :               , g%acid_excretion_root(layer)
     :               , 0.0
     :               , g%dlt_lime_dissl(layer)
     :               , e%ash_alk_wt_incorp(layer)
     :               , g%dlt_acid_org_C_cycle(layer)
     :               , e%dlayer(layer)
     :                        )
         g%pHCa(layer) = g%pHCa(layer) + g%dlt_pHCa(layer)
1400  continue

      g%dlt_pHCa_tot(:) = g%dlt_pHCa_tot(:)
     :                  + g%dlt_pHCa(:)

      g%lime_pool(:) = g%lime_pool(:) + g%dlt_lime_pool(:)


 !      print*, 'soilpH:g%residue_ash_alk_wt=', g%residue_ash_alk_wt


      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine SoilpH_acid_org_C_cycle (dlt_acid_org_C_cycle
     :                                   , dlt_OM
     :                                   , hum_acid_slope
     :                                   , hum_acid_pHCa_offset
     :                                   , pHCa )
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real dlt_acid_org_C_cycle           ! (OUT) Change in acid due to change in humic C. (mol/ha) H+ equiv
      real dlt_OM                         ! (IN) Increase in soil organic matter (Kg/Ha)
      real hum_acid_slope                 ! (IN) Slope coefficient for humic acids (cMol/Kg)
      real hum_acid_pHCa_offset           ! (IN) Slope offset for humic acids (no units)
      real pHCa                           ! (IN) pH unit.

*+  Purpose
*     Calculate Organic acid accumulated due to increase in humic carbon.

*+  Mission Statement
*     Calculate Organic acid accumulated due to increase in humic carbon.

*+  Changes
*     220500 jngh created

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'SoilpH_acid_org_C_cycle')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      dlt_acid_org_C_cycle = dlt_OM
     :                     * hum_acid_slope * cmol2mol
     :                     * (pHCa - hum_acid_pHCa_offset)


      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine SoilpH_tec (tec
     :                   , tec_init
     :                   , pHCa_initial
     :                   , sAls
     :                   , org_C_fract
     :                   , pHCa )
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real tec          ! (OUT) Total Cation Exchange Capacity (cMol/Kg).
      real tec_init     ! (IN) Initial TEC (see above).
      real pHCa_initial ! (IN) Initial pHCa at start of run (pH unit).
      real sAls         ! (IN) Slope of Al/H+ relation (cMol/Kg/Mol/1000.0*L).
      real org_C_fract  ! (IN) Fraction of organic carbon in the layer (gm/gm).
      real pHCa         ! (IN) pH unit.

*+  Purpose
*     Calculate TEC.

*+  Mission Statement
*     Calculate TEC

*+  Changes
*     170699 sb   created

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'SoilpH_tec')

*+  Local Variables
      real sls                ! Slope of TEC - pH relation (cMol/Kg/pH unit).
      real sls_tec_pHCa         ! Term added to get sls.
      real sls_C_fract        ! Term added to get sls.
      real sls_sAls           ! Term added to get sls.

*- Implementation Section ----------------------------------
      call push_routine (my_name)

 !        print *
 !        print *, 'tec_init,pHCa_initial,sAls,org_C_fract,pHCa ='
 !       :           , tec_init, pHCa_initial, sAls, org_C_fract, pHCa

         !  Calculate Slope of TEC versus pHCa.
      sls_tec_pHCa = 0.76 * divide(tec_init, pHCa_initial, 0.0)
      sls_C_fract   = 67.1 * org_C_fract
      sls_sAls   = 0.137 * sAls
      sls        = -1.367 + sls_tec_pHCa + sls_C_fract + sls_sAls

 !        print *, 'sls,sls_tec_pHCa,sls_C_fract,sls_sAls='
 !       :              , sls, sls_tec_pHCa, sls_C_fract, sls_sAls

         !  Calculate Total Cation Exchange Capacity.
      tec = tec_init + ((pHCa - pHCa_initial) * sls)


      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine soilpH_Al_exchangable (Al_exchangable
     :                   , sAls
     :                   , pHCa )
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real Al_exchangable  ! (OUT) Aluminium concentration (cMol/Kg).
      real sAls            ! (IN) Slope of Al/H+ relation (cMol/Kg/Mol/1000.0*L).
      real pHCa            ! (IN) pH unit.

*+  Purpose
*     Calculates Al conc.

*+  Mission Statement
*     Calculate Al conc

*+  Changes
*     170699 sb   created

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'soilpH_Al_exchangable')

*+  Local Variables
      real H_conc ! Molar concentration of H+ (Moles H+ / Litre)
 !cjh  kirsten's version
 !cjh  set lri_fdge to 0 or remove to change to correct version
 !      real lri_fudge ! Stops pHBC going to 0 as pH gets below 4.0.

*- Implementation Section ----------------------------------
      call push_routine (my_name)


         !  Calculate Hydrogen ion concentration (Moles H+ / Litre).
      H_conc = 10.0 ** (-pHCa)

         !  Calculate Aluminium Concentration (cMol/Kg).
      Al_exchangable = H_conc * sAls * 1000.0

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine soilpH_pHBC_Parameters (pHBC, pHBC_param)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real pHBC            ! (OUT) pH buffer capacity. (Kmol/ha/100mm/ph_unit).
      real pHBC_param      ! (IN) pH buffer capacity from parameter section. (Kmol/ha/100mm/ph_unit).

*+  Purpose
*     Calculates pH buffer capacity.

*+  Mission Statement
*     Calculate pH buffer capacity

*+  Changes
*     220500 jngh created

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'soilpH_pHBC_Parameters')


*- Implementation Section ----------------------------------
      call push_routine (my_name)

      pHBC = pHBC_param

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine soilpH_pHBC_Hochman (pHBC
     :                   , tec
     :                   , Al_exchangable
     :                   , sAls
     :                   , org_C_fract
     :                   , pHCa )
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real pHBC            ! (OUT) pH buffer capacity. (Kmol/ha/100mm/ph_unit).
      real tec             ! (IN) Total Cation Exchange Capacity (cMol/Kg).
      real Al_exchangable  ! (IN) Aluminium concentration (cMol/Kg).
      real sAls            ! (IN) Slope of Al/H+ relation (cMol/Kg/Mol/1000.0*L).
      real org_C_fract     ! (IN) Fraction of organic carbon in the layer (gm/gm).
      real pHCa            ! (IN) pH unit.

*+  Purpose
*     Calculates pH buffer capacity.

*+  Mission Statement
*     Calculate pH buffer capacity

*+  Changes
*     170699 sb   created

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'soilpH_pHBC_Hochman')

      real       lime_dissolved_fract         ! fraction of lime dissolved, 12 months after
      parameter (lime_dissolved_fract = 0.83) ! application - Mark Conyers (0-1)

*+  Local Variables
      real H_conc ! Molar concentration of H+.
      real lri    ! Lime requirement index (pH unit/tonne lime/(Ha*100mm)).
      real lri_pHCa
      real lri_tec
      real lri_Al
      real lri_C_fract ! Terms add/sub'ed to get lri.
      real pAl_ex
 !cjh  kirsten's version
 !cjh  set lri_fdge to 0 or remove to change to correct version
 !      real lri_fudge ! Stops pHBC going to 0 as pHCa gets below 4.0.

*- Implementation Section ----------------------------------
      call push_routine (my_name)


         !  Calculate Hydrogen ion concentration (Moles H+ / Litre).
      H_conc = 10.0 ** (-pHCa)

         !  Calculate Lime Requirement Index.
      lri_pHCa = 4.2 * 1000.0 * H_conc
      lri_tec = 0.016 * tec
      lri_Al = 0.097 * Al_exchangable
      lri_C_fract = 1.6 * org_C_fract
 !cjh  kirsten's version
 !cjh  set lri_fdge to 0 or remove to change to correct version
 !      lri_fudge = 1.5e10 * H_conc * H_conc * H_conc
 !      lri = 0.764 + lri_pHCa - lri_tec - lri_Al - lri_C_fract - lri_fudge
      lri = 0.764 + lri_pHCa - lri_tec - lri_Al - lri_C_fract

 !        print *, 'lri, lri_pHCa, lri_tec, lri_Al, lri_C_fract='
 !       :                , lri, lri_pHCa, lri_tec, lri_Al, lri_C_fract

        !  Calculate pH buffer capacity.
      if (pHCa .le. 4.00) then
           ! we have Al buffering dominating
         pAl_ex = 10**(-Al_exchangable)
         pHBC = divide (pHCa, pAl_ex, 9999.0)

      else if (pHCa .ge. 8.2) then
         pHBC = 9999

      else
         pHBC = divide (CaCO3_t2KMol, (lri / lime_dissolved_fract)
     :                 , 9999.0)
      end if

 !        print *, 'pHBC,tec,al=', pHBC, tec, al
      ! pHBC = bound (pHBC, 10.0, 40.0)

      call pop_routine (my_name)
      return
      end subroutine




*     ===========================================================
      subroutine soilpH_dlt_pH (dlt_pHCa
     :                   , pHBC
     :                   , dlt_acid_N_cycle
     :                   , acid_excretion_root
     :                   , H_equiv_flow_net
     :                   , dlt_lime_dissl
     :                   , ash_alk_wt
     :                   , dlt_acid_org_C_cycle
     :                   , dlayer)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real dlt_pHCa              ! (OUT)
      real pHBC                  ! (IN) pH buffer capacity of the soil (Kmol/ha/100mm/ph_unit).
      real dlt_acid_N_cycle      ! (IN) Acid added by nitrogen cycle (mol H+/Ha).
      real acid_excretion_root   ! (IN) (R)oo(t) (Ex)cretion of acid (Mol/Ha).
      real H_equiv_flow_net      ! (IN) Equivalent flow H+ out of layer (Mol/Ha).
      real dlt_lime_dissl        ! (IN) lime_pool dissolved this timestep (Mol/Ha).
      real ash_alk_wt            ! (IN) ash_alk * wt of residue incorp'd (Mol/Ha).
      real dlt_acid_org_C_cycle  ! (IN) acid due to change in humic C. (Mol/ha)
      real dlayer                ! (IN) Layer thickness (mm).

*+  Purpose
*     Calculate dlt pHCa given additions and subtractions of ions.

*+  Mission Statement
*     Calculate dlt pHCa

*+  Changes
*     170699 sb   created

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'soilpH_dlt_pH')

*+  Local Variables
      real dlt_H         ! H+ equiv added (Mol/Ha).
      real pHBC_layer    ! pH buffering capacity of the layer (Kmol/ha/ph_unit).

*- Implementation Section ----------------------------------
      call push_routine (my_name)

  ! Account for thickness of layer.  (convert mm to decimetre)
      pHBC_layer = pHBC * (dlayer/100.0)


  ! Tally H+ ions.
      dlt_H = H_equiv_flow_net + acid_excretion_root
     :      + dlt_acid_N_cycle + dlt_acid_org_C_cycle
     :      - (dlt_lime_dissl + ash_alk_wt)

!       print*, 'H_diff, dlt_lime_dissl,+ ash_alk_wt '
!     :       //',- H_equiv_flow_net,- acid_excretion_root '
!     :       //',- dlt_acid_N_cycle,- dlt_acid_org_C_cycle '
!       print*, H_diff, dlt_lime_dissl,+ ash_alk_wt
!     :       ,- H_equiv_flow_net,- acid_excretion_root
!     :       ,- dlt_acid_N_cycle,- dlt_acid_org_C_cycle

 ! pHCa change for a soil layer.
      dlt_pHCa = divide (-dlt_H, (kmol2mol*pHBC_layer), 0.0)
!      print*,'dlt_pHCa=',dlt_pHCa
      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine soilpH_acid_excretion_root(  acid_excretion_root
     :                              , NO3_uptake_equiv
     :                              , NH4_uptake_equiv
     :                              , Ca_uptake_equiv
     :                              , Mg_uptake_equiv
     :                              , K_uptake_equiv
     :                              , Na_uptake_equiv
     :                              , P_uptake_equiv
     :                              , S_uptake_equiv
     :                              , Cl_uptake_equiv
     :                              )
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real acid_excretion_root ! (OUT) Adjusted estimate of root excretion of acid.
      real NO3_uptake_equiv    ! (IN) NO3 taken by crops today (Mol/Ha).
      real NH4_uptake_equiv    ! (IN) NH4 taken by crops today (Mol/Ha).
      real Ca_uptake_equiv     ! (IN) uptake_equiv of Ca,   (Moles H+ equiv / Ha).
      real Mg_uptake_equiv     ! (IN) uptake_equiv of Mg,   (Moles H+ equiv / Ha).
      real K_uptake_equiv      ! (IN) uptake_equiv of K,   (Moles H+ equiv / Ha).
      real Na_uptake_equiv     ! (IN) uptake_equiv of Na,   (Moles H+ equiv / Ha).
      real P_uptake_equiv      ! (IN) uptake_equiv of P,   (Moles H+ equiv / Ha).
      real S_uptake_equiv      ! (IN) uptake_equiv of S,   (Moles H+ equiv / Ha).
      real Cl_uptake_equiv     ! (IN) uptake_equiv of Cl,   (Moles H+ equiv / Ha).

*+  Purpose
*     Calculates root exretion of acid given crop uptakes of things.

*+  Mission Statement
*     Calculate root exretion of acid

*+  Changes
*     170699 sb   created

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilpH_acid_excretion_root')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      acid_excretion_root =
     :              NH4_uptake_equiv ! NH4 taken by crops today (Mol/Ha).
     :            + NO3_uptake_equiv ! NO3 taken by crops today (Mol/Ha).
     :            + Ca_uptake_equiv ! uptake_equiv of Ca, (Moles H+ equiv / Ha).
     :            + Mg_uptake_equiv ! uptake_equiv of Mg, (Moles H+ equiv / Ha).
     :            + K_uptake_equiv  ! uptake_equiv of K,   (Moles H+ equiv / Ha).
     :            + Na_uptake_equiv ! uptake_equiv of Na, (Moles H+ equiv / Ha).
     :            + P_uptake_equiv  ! uptake_equiv of P,   (Moles H+ equiv / Ha).
     :            + S_uptake_equiv  ! uptake_equiv of S,   (Moles H+ equiv / Ha).
     :            + Cl_uptake_equiv ! uptake_equiv of Cl,   (Moles H+ equiv / Ha).
      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine SoilpH_acid_N_cycle (dlt_acid_N_cycle
     :                              , NO3_transform_net_mol
     :                              , NH4_transform_net_mol)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real dlt_acid_N_cycle       ! (OUT) Acid added due to N cycle (Mol/Ha).
      real NO3_transform_net_mol  ! (IN) Net NO3 transformed (Mol/Ha)
      real NH4_transform_net_mol  ! (IN) Net NH4 transformed (Mol/Ha)

*+  Purpose
*     Calculates Acid added due to nit cycle.

*+  Mission Statement
*     Calculate Acid added due to nit cycle

*+  Changes
*     170699 sb   created
*     970924 sb   Changed calculation of NO3_acc and NH4_acc.

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'SoilpH_acid_N_cycle')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

         !  Work out nitrogen cyle acidification.
      dlt_acid_N_cycle =   NO3_transform_net_mol - NH4_transform_net_mol
 !      print*, 'NO3_transform_net_mol', NO3_transform_net_mol
 !      print*, 'NH4_transform_net_mol', NH4_transform_net_mol

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine soilpH_dlt_lime (dlt_lime_pool
     :                   , dlt_lime_dissl
     :                   , lime_pool
     :                   , dlt_lime_added
     :                   , flow_water
     :                   , pHCa
     :                   , lime_sol_tbl_pHCa
     :                   , lime_sol_tbl_lime
     :                   , lime_sol_tbl_size)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real dlt_lime_pool                        ! (OUT) Difference in lime pool (Mol/Ha).
      real dlt_lime_dissl                       ! (OUT) Lime dissolved (Mol/Ha).
      real lime_pool                            ! (IN) Lime pool equiv for each layer (Mol/Ha).
      real dlt_lime_added                       ! (IN) Lime equivalent added to this layer (Mol/Ha).
      real flow_water                           ! (IN) Water flow moving down out of this layer (mm).
      real pHCa                                 ! (IN) Ionic activity of hydrogen ions in the layer.
      integer lime_sol_tbl_size                 ! (IN) No of elements in lime solubility table.
      real lime_sol_tbl_pHCa(lime_sol_tbl_size) ! (IN) pHCa (X) values of lime solubility table.
      real lime_sol_tbl_lime(lime_sol_tbl_size) ! (IN) lime solubility (Y) values of lime
                                                ! solubility table (g/l).

*+  Purpose
*      Calculate the delta for the lime_pool pool and the amount of
*      lime dissolved for a layer.

*+  Mission Statement
*      Calculate lime dissolved and adjust the lime_pool pool

*+  Changes
*     170699 sb   created

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'soilpH_dlt_lime')

*+  Local Variables
      real dissl_rate  ! Max lime dissolved in grams per litre water flow.
      real max_dissl   ! Max lime can dissolve given pHCa and water flow (Mol/Ha).

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (reals_are_equal (dlt_lime_added,0.0)
     :    .and. reals_are_equal (lime_pool,0.0)) then
         dlt_lime_dissl = 0.0
         dlt_lime_pool = 0.0

      else

            !  The amount of lime equivalent dissolved in the time step.
         dissl_rate = linear_interp_real (pHCa
     :                                 , lime_sol_tbl_pHCa
     :                                 , lime_sol_tbl_lime
     :                                 , lime_sol_tbl_size)  !(gm/l)
         max_dissl = dissl_rate * (gm2kg * CaCO3_kg2mol)
     :             * abs (flow_water)* (mm2lpsm / sm2ha)
     :
         dlt_lime_dissl = max(0.0, min(dlt_lime_added+lime_pool
     :                                 , max_dissl))

         !  The resulting difference in lime equivalent.
         dlt_lime_pool = dlt_lime_added - dlt_lime_dissl

      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine soilpH_H_equiv_mass_flow (H_equiv_mass_flow
     :                                    , pH
     :                                    , pHCa
     :                                    , H20_mass_flow_mm
     :                                    , CO2_pressure
     :                                    , pAl_pHca_slope
     :                                    , pAl_pHCa_intercept
     :                                    , ionic_strength)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real H_equiv_mass_flow(max_MF_equiv_type)  ! (OUT) Equivalent H+ flow out of the layer by component (Mol/ha).
      real pH                 ! (IN) pH
      real pHCa               ! (IN) pHCa
      real H20_mass_flow_mm   ! (IN) How much water flowing out of the layer (mm).
      real CO2_pressure       ! (IN) Air CO2 partial pressure (atm)
      real pAl_pHca_slope     ! (IN) User supplied slope of -log(labile Al) vs pHCa.
      real pAl_pHCa_intercept ! (IN) supplied intercept of -log(labile Al) vs pHCa.
      real ionic_strength     ! (IN) ionic strength of soil layer ()

*+  Purpose
*       Calculate equivalent H+ flow out of the layer.

*+  Mission Statement
*       Calculate equivalent H+ flow out of the layer

*+  Changes
*     170699 sb   created

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'soilpH_H_equiv_mass_flow')

*+  Local Variables
      real H2O_mass_flow      ! Mass flow of water to next layer (L/ha/day)
      real pAl                ! -log(Al_mass_flow) pAl as a function of soil pHCa
                              ! (-log10 of Al ionic activity in solution) (mol/L)
      real Al_conc            !  Al conc (Mol/L)
      real Al_mass_flow       ! labile aluminium (mol/Ha)  Mass flow of Al .
      real Al_mass_flow_equiv ! labile aluminium (mol/Ha)  H+ equiv Mass flow of Al .
      real HCO3_conc          !  (Mol/L)
      real pHCO3              ! -log10 of HCO3 ionic activity
      real HCO3_mass_flow     ! Mass flow HCO3_conc (Mol/ha)
      real HCO3_mass_flow_equiv ! H+ equiv Mass flow HCO3_conc (Mol/ha)
      real CO3_mass_flow      ! Actual mass flow of CO3_conc (Mol/ha)
      real CO3_mass_flow_equiv ! Actual H+ equiv mass flow of CO3_conc (Mol/ha)
      real pOH                ! -log10 of OH ionic activity
      real OH_conc            ! OH conc (Mol/L)
      real OH_mass_flow       ! Mass flow of OH-  (Mol/ha)
      real OH_mass_flow_equiv ! H+ equiv Mass flow of OH-  (Mol/ha)
      real H_conc             ! H+ conc (Mol/L)
      real H_mass_flow        ! Mass flow of H+  (Mol/ha)
      real H_mass_flow_equiv  ! H+ equiv Mass flow of H+  (Mol/ha)
      real pCO3               ! -log10 of CO3 ionic activity
      real CO3_conc           ! HCO3_conc (Mol/L)
      real pCO2               ! -log10 of CO2 partial pressure


*- Implementation Section ----------------------------------
      call push_routine (my_name)

         ! Mass flow of water to next layer (L/ha/day) .
      H2O_mass_flow    =  H20_mass_flow_mm * (mm2lpsm / sm2ha)

         ! HCO3_conc (moles/L).
      pCO2      = - log10 (CO2_pressure)
      pHCO3     = pCO2 - pH + pKa_CO2
      HCO3_conc = soilpH_ionic_conc (pHCO3
     :                              , HCO3_valency
     :                              , ionic_strength)

         ! Mass flow HCO3_conc- (mol/ha).
      HCO3_mass_flow = HCO3_conc * H2O_mass_flow
      HCO3_mass_flow_equiv = HCO3_mass_flow * HCO3_valency

         ! Actual mass flow of CO3_conc-  (mol/ha).
      pCO3          = pHCO3 - pH + pKa_HCO3
      CO3_conc      = soilpH_ionic_conc (pCO3
     :                                  , CO3_valency
     :                                  , ionic_strength)

      CO3_mass_flow = CO3_conc * H2O_mass_flow
      CO3_mass_flow_equiv = CO3_mass_flow * CO3_valency

         ! Mass flow of OH- (hydroxide ions) (mol/ha).
      pOH          = pKc_water - pH
      OH_conc      = soilpH_ionic_conc (pOH
     :                                 , OH_valency
     :                                 , ionic_strength)

      OH_mass_flow = OH_conc * H2O_mass_flow
      OH_mass_flow_equiv = OH_mass_flow * OH_valency

         ! Mass flow of H+ (hydrogen ions) (mol/ha).
      H_conc       = soilpH_ionic_conc (pH
     :                                 , H_valency
     :                                 , ionic_strength)

      H_mass_flow  = H_conc * H2O_mass_flow
      H_mass_flow_equiv = H_mass_flow * H_valency

         ! pAl as a function of soil pHCa (-ve log of [al] in soln) (moles/L).
      pAl  = pAl_pHca_slope * pHCa  +  pAl_pHCa_intercept
 !        write (*,*) 'pAl  = pAl_pHca_slope * pHCa  +  pAl_pHCa_intercept'
 !        write (*,'(g13.5,g15.5,g13.5,g13.5)') pAl, pAl_pHca_slope, pHCa
 !       :                                               ,  pAl_pHCa_intercept
 !        write (*,*) 'Old_method gives:='
 !       :           , 2.0 + 1.0 / (0.297/(pHCa-3.3) + 0.152)
 !        write (*,*)

         ! Mass flow of Al (mol/ha).
      if (pAl .gt. 0.0) then
         Al_conc       = soilpH_ionic_conc (pAl
     :                                     , Al_valency
     :                                     , ionic_strength)

         Al_mass_flow  = Al_conc * H2O_mass_flow
      else
         Al_mass_flow = 0.0
      endif
      Al_mass_flow_equiv = Al_mass_flow * Al_valency

         ! Mass flow of H+ equivalents through the soil layer.
!      H_equiv_mass_flow = H_mass_flow_equiv
!     :                  + OH_mass_flow_equiv
!     :                  + HCO3_mass_flow_equiv
!     :                  + CO3_mass_flow_equiv
!     :                  + Al_mass_flow_equiv
      H_equiv_mass_flow(H_index)    = H_mass_flow_equiv
      H_equiv_mass_flow(OH_index)   = OH_mass_flow_equiv
      H_equiv_mass_flow(HCO3_index) = HCO3_mass_flow_equiv
      H_equiv_mass_flow(CO3_index)  = CO3_mass_flow_equiv
      H_equiv_mass_flow(Al_index)   = Al_mass_flow_equiv

!      print*, 'pHCa, H_equiv_mass_flow, H_mass_flow'
!     :                  //', -OH_mass_flow '
!     :                  //', -HCO3_mass_flow '
!     :                  //', - CO3_mass_flow '
!     :                  //', + Al_equiv_mass_flow '

!      print*, pHCa, H_equiv_mass_flow, H_mass_flow
!     :                  , -OH_mass_flow
!     :                  , -HCO3_mass_flow
!     :                  , - CO3_mass_flow
!     :                  , + Al_equiv_mass_flow

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      real function soilpH_ionic_conc ( pIon
     :                                , valency
     :                                , ionic_strength)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real    pIon         ! (IN) -ve log of [ion] in solution (mol/L)
      real    valency      ! (IN) valency of ion
      real    ionic_strength ! (IN) ionic strength of soil

*+  Purpose
*       Calculate ionic concentration.

*+  Mission Statement
*       Calculate ionic concentration.

*+  Changes
*     210499 jngh   created

*+  Notes
*     Extended Debye-Hückel equation simplified. Based on experimental data
*     suggested by Davies (1962)

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'soilpH_ionic_conc')

      real       A         ! corfficient for water at 25ºC
      parameter (A = 0.509)

*+  Local Variables
      real conc               !  conc (Mol/L)
      real ionic_activity     !  ionic activity ()
      real ionic_activity_coef ! ionic activity coefficient ()

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      ionic_activity = 10.0 ** (-pIon)
      ionic_activity_coef = 10**(-A * valency**2
     :                    * (ionic_strength**0.5
     :                      / (1 + ionic_strength**0.5)
     :                      - 0.3*ionic_strength))
      conc = ionic_activity / ionic_activity_coef
      soilpH_ionic_conc = conc

      call pop_routine (my_name)
      return
      end function

*     ===========================================================
      subroutine soilpH_pHCa2pH (pH, pHCa, num_layers)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer  num_layers                     ! (IN) number of layers in profile ().
      real     pH(num_layers)                 ! (OUT) pH water ().
      real     pHCa(num_layers)               ! (IN) pH Calcium Chloride  ()

*+  Purpose
*     Converts pH water to pH Calcium Chloride

*+  Mission Statement
*     Converts pH water to pH Calcium Chloride

*+  Changes
*     140999 JNGH created

*+  Local Variables
      integer  layer

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'soilpH_pHCa2pH')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      do 110 layer=1, num_layers
            pH(layer) = linear_interp_real(
     :                                       pHCa(layer)
     :                                     , c%pHCa2pH_tbl_pHca
     :                                     , c%pHCa2pH_tbl_pH
     :                                     , c%pHCa2pH_tbl_size)
110   continue

      call pop_routine (my_name)
      return
      end subroutine

*     ================================================================
      subroutine soilpH_init_residue_ash_alk_wt()
*     ================================================================
      Use infrastructure
      implicit none

*+  Purpose
*     <insert here>

*+  Changes
*     230999 jngh

*+  Constant Values
      character my_name*(*)             ! name of current procedure
      parameter (my_name = 'soilpH_init_residue_ash_alk_wt')

*+  Local Variables
      integer    numvals
      real       init_residue_wt
      character  init_residue_type*(crop_type_size)
      character  init_residue_dm_type*(dm_type_size)
      real       ash_alk_rate

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call write_string (
     :             new_line//'   - Initialising residue ash alkalinity')

      call get_char_var (unknown_module
     :             , 'init_residue_type'
     :             , '()'
     :             , init_residue_type
     :             , numvals)

      call get_char_var (unknown_module
     :             , 'init_residue_dm_type'
     :             , '()'
     :             , init_residue_dm_type
     :             , numvals)

      call get_real_var (unknown_module
     :             , 'residue_wt'
     :             , '(kg/ha)'
     :             , init_residue_wt
     :             , numvals
     :             , 0.0
     :             , 100000.0)

      call soilpH_ash_alk_rate (ash_alk_rate
     :                         , init_residue_type
     :                         , init_residue_dm_type)

         ! ----------------------------------------------------------
         !    Calculate residue ash alkalinity
         ! ----------------------------------------------------------
      g%residue_ash_alk_wt = ash_alk_rate
     :                     * init_residue_wt

      call pop_routine (my_name)
      return
      end subroutine


*     ================================================================
      subroutine SoilpH_incorp (Tillage_depth)
*     ================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       Tillage_depth

*+  Purpose
*   Calculate ash alkalinity incorporation as a result of tillage and update
*   ash alkalinity pool.

*+  Notes
*   I do not like updating the pools here but we need to be able to handle
*   the case of multiple tillage events per day.

*+  Mission Statement
*     Incorporate ash alkalinity to %2 depth

*+  Changes
*    131099 jngh  created

*+  Constant Values
      character*(*) my_name             ! name of current procedure
      parameter (my_name = 'SoilpH_incorp')

*+  Local Variables
      real       cum_depth             !
      integer    Deepest_Layer         !
      real       Depth_to_go           !
      real       F_incorp_layer        !
      integer    layer                 !
      real       layer_incorp_depth    !
      real       Ash_Alk_wt          !
      real       proportion
      real       remainder
      real       depth_to_layer_bottom ! depth to bottom of layer (mm)
      real       depth_to_layer_top    ! depth to top of layer (mm)
      real       depth_to_till         ! depth to till in layer (mm)
      real       depth_of_till_in_layer ! depth of till within layer (mm)

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      Deepest_Layer = get_cumulative_index_real (Tillage_depth
     :                                         , e%dlayer, max_layer)
!      print*, 'deepest_layer', deepest_layer
      if (Deepest_Layer .gt. 1) then
         Ash_Alk_wt = sum_real_array(e%ash_alk_wt_incorp, Deepest_Layer)
!      print*, '1 e%ash_alk_wt_incorp', e%ash_alk_wt_incorp

            ! correct bottom layer for actual tillage penetration
         depth_to_layer_bottom = sum_real_array(e%dlayer, Deepest_Layer)
         depth_to_layer_top = depth_to_layer_bottom
     :                   - e%dlayer(Deepest_Layer)
         depth_to_till  = min (depth_to_layer_bottom, Tillage_depth)

         depth_of_till_in_layer = dim(depth_to_till, depth_to_layer_top)
         proportion = divide (depth_of_till_in_layer
     :                   , e%dlayer(deepest_layer), 0.0)
         remainder =  e%ash_alk_wt_incorp(deepest_layer)
     :          * (1.0 - proportion)
         Ash_Alk_wt = Ash_Alk_wt - remainder

         cum_depth = 0.0
         do 2000 layer = 1, Deepest_Layer

            depth_to_go = tillage_depth - cum_depth
            layer_incorp_depth = min (depth_to_go, e%dlayer(layer))
            F_incorp_layer = divide (layer_incorp_depth
     :                          , tillage_depth, 0.0)

            e%ash_alk_wt_incorp(layer) = Ash_Alk_wt *  F_incorp_layer

            cum_depth = cum_depth + e%dlayer(layer)
 2000    continue


         e%ash_alk_wt_incorp(deepest_layer)
     :                  = e%ash_alk_wt_incorp(deepest_layer) + remainder
!      print*, '2 ash_alk_wt_incorp ',e%ash_alk_wt_incorp
      else
      endif

      call pop_routine (my_name)
      return
      end subroutine

*     ================================================================
      subroutine SoilpH_Tillage ()
*     ================================================================
      Use infrastructure
      implicit none

*+  Purpose
*   Calculates ash alkalinity incorporation as a result of tillage operations.

*+  Mission Statement
*     Calculate ash alkalinity incorporation as a result of tillage operations

*+  Changes
*     230600 jngh created

*+  Constant Values
      character*(*) my_name             ! name of current procedure
      parameter (my_name = 'SoilpH_tillage')

*+  Local Variables
      character String*300             ! message string
      integer   Numvals_T              ! Number of values found in data string
      real      Tillage_depth          ! depth of residue incorp (mm)

*- Implementation Section ----------------------------------
      call push_routine (my_name)

         ! ----------------------------------------------------------
         !       Get User defined tillage effects on ash alkalinity
         ! ----------------------------------------------------------

      call collect_real_var_optional ('tillage_depth'
     :                              , '()', tillage_depth, numvals_t
     :                              , 0.0, 1000.0)

      If (Numvals_t .eq.0) then
            tillage_depth = 0.0

            string = 'Cannot find tillage depth:- '
            call FATAL_ERROR (ERR_user, string)

      Else
            ! ----------------------------------------------------------
            !              Now incorporate the ash alkalinity
            ! ----------------------------------------------------------
         Call SoilpH_incorp (Tillage_Depth)


         Write (string, '(10x,a, f8.2)' )
     :     'Ash alkalinity incorporated Depth    = ', Tillage_Depth

         call Write_string (string)
      Endif


      call pop_routine (my_name)
      return
      end subroutine


      end module SoilpHModule


!     ===========================================================
      subroutine alloc_dealloc_instance(doAllocate)
!     ===========================================================
      use SoilpHModule
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
         allocate(e)
         allocate(c)
         allocate(Id)
      else
         deallocate(g)
         deallocate(p)
         deallocate(e)
         deallocate(c)
         deallocate(id)
      end if
      return
      end subroutine



*     ===========================================================
      subroutine main (action, data_string)
*     ===========================================================
      Use infrastructure
      Use SoilpHModule
      implicit none
      ml_external Main

*+  Sub-Program Arguments
       character  action*(*)         ! (IN) Message action to perform
       character  data_string*(*)    ! (IN) Message data

*+  Purpose
*      This routine is the interface between the main system and the
*      SoilPh module.

*+  Mission Statement
*      Simulate soil acidity

*+  Changes
*     170699 sb   created
*     220500 jngh changed the main processing from post to process phase

*+  Constant Values
      character  my_name*(*)
      parameter (my_name='SoilPh_Main')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

 !        call write_string (
 !       :            'message: - ' // action // ' || ' // data_string)

      if (action.eq.ACTION_get_variable) then
         call SoilpH_send_my_variable (data_string)

      else if (action .eq. ACTION_set_variable) then
         call SoilpH_set_my_variable (data_string)

      else if (action.eq.ACTION_process) then
!      else if (action.eq.ACTION_post) then
         call SoilpH_get_other_variables ()
         call SoilpH_process ()
         call SoilpH_zero_event_variables ()
         call SoilpH_set_other_variables ()

      elseif (action .eq. EVENT_N_Balance) then
         call soilpH_ON_Nbalance ()

      elseif (action .eq. EVENT_C_Balance) then
         call soilpH_ON_Cbalance ()

      elseif (action .eq. EVENT_Residue_removed) then
         call soilpH_ON_Residue_removed ()

      elseif (action .eq. EVENT_Residue_added) then
         call soilpH_ON_Residue_added ()

      elseif (action .eq. EVENT_Crop_Chopped) then
         call soilpH_ON_Crop_Chopped ()

      else if (Action.eq.ACTION_Till) then
         call soilpH_tillage ()

      else if ((action.eq.ACTION_reset)
     :          .or.(action.eq.ACTION_user_init)) then
         call SoilpH_zero_variables ()
         call SoilpH_get_soil_layers ()
         call SoilpH_init ()

      else if (action.eq.ACTION_sum_report) then
         call SoilpH_sum_report ()

      else if (action.eq.ACTION_init) then
         call SoilpH_zero_variables ()
         call SoilpH_get_soil_layers ()
         call SoilpH_init ()
         call SoilpH_sum_report ()

      elseif (Action.eq.ACTION_Create) then
         call doRegistrations(id)
         call soilpH_zero_all_globals ()

      elseif (action.eq.ACTION_end_run) then
         call SoilpH_endrun ()

      else
            ! Don't use message
         call message_unused ()

      endif

      call pop_routine (my_name)
      return
      end subroutine

! ====================================================================
! This routine is the event handler for all events
! ====================================================================
      subroutine respondToEvent(fromID, eventID, variant)
      Use infrastructure
      implicit none
      ml_external respondToEvent
      
      integer, intent(in) :: fromID
      integer, intent(in) :: eventID
      integer, intent(in) :: variant
      
      return
      end subroutine respondToEvent
