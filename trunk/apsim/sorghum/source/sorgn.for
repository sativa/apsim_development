*     ===========================================================
      subroutine sorg_nfact_photo(leaf,lai,
     :                  n_green, nfact)
*     ===========================================================
      Use Infrastructure
      implicit none

*+  Sub-Program Arguments
      integer leaf
      real    lai
      REAL    n_green(*)          ! (INPUT)  plant nitrogen content (g N/m^2)
      real    nfact               ! (OUTPUT) N stress factor

*+  Purpose
*     The concentration of Nitrogen in leaves is used to derive a
*     series of Nitrogen stress indices.  The stress indices for
*     photosynthesis and cell expansion are calculated from today's
*     relative nutritional status between a critical and minimum
*     leaf Nitrogen concentration.

*+  Mission Statement
*   Calculate Nitrogen stress factor for photosynthesis

*+  Changes

*+  Calls

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sorg_nfact_photo')

*+  Local Variables
      real      SLN

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      SLN = divide(n_green(leaf), lai, 0.0)

      nfact = (1.0/0.7) * SLN - (3.0/7.0)

      nfact = bound (nfact, 0.0, 1.0)

      call pop_routine (my_name)
      return
      end subroutine


!     ===========================================================
      subroutine sorg_N_senescence1 (num_part ,SLN
     :                              , c_n_sen_conc
     :                              , g_dlt_dm_senesced
     :                              , g_n_green
     :                              , g_dm_green
     :                              , g_nfact_expansion
     :                              , dlt_N_senesced)
*     ===========================================================
      Use Infrastructure
      implicit none

*+  Sub-Program Arguments
      integer num_part            ! (INPUT) number of plant part
      real    SLN                 ! (INPUT) Specific Leaf Nitrogen (g N /m^2)
      REAL    c_n_sen_conc(*)     ! (INPUT)  N concentration of senesced material
                                  !         (g/m^2)
      REAL    g_dlt_dm_senesced(*)! (INPUT)  plant biomass senescence (g/m^2)
      REAL    g_n_green(*)        ! (INPUT) nitrogen in plant material (g/m^2)
      REAL    g_dm_green(*)       ! (INPUT) plant material (g/m^2)
      real    g_nfact_expansion
      real    dlt_N_senesced(*)   ! (OUTPUT) actual nitrogen senesced
                                  !          from plant parts (g/m^2)

*+  Purpose
*       Derives seneseced plant nitrogen (g N/m^2)

*+  Mission Statement
*   Calculate change in senesced plant Nitrogen

*+  Changes
*       121297 nih specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sorg_N_senescence1')

*+  Local Variables
      integer part          ! plant part counter variable
      real    green_n_conc  ! N conc of green material (g/g)
      real    sen_n_conc    ! N conc of senescing material (g/g)

!  changes GMC
!       [N] of senesced leaf cannot be > 0.001 using old approach
!       under hign N low water, senesced leaf must be able to have high SLN
!
!       when there is no n stress (SLN > 1) (g_nfact_expansion = 1) allow sen_n_conc
!       to climb to g_n_green(part)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      ! first we zero all plant component deltas

      do 100 part = 1, num_part

         green_n_conc = divide (g_n_green(part)
     :                         ,g_dm_green(part)
     :                         ,0.0)

         sen_n_conc = green_n_conc * min(SLN,1.0) * 0.75  ! should be externals?
         dlt_N_senesced(part) = g_dlt_dm_senesced(part)
     :                        * sen_n_conc

         dlt_N_senesced(part) = u_bound (dlt_N_senesced(part)
     :                                  ,g_n_green(part))

  100 continue

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine sorg_N_init1
     :               (
     :                C_n_init_conc
     :              , max_part
     :              , G_dm_green
     :              , g_lai
     :              , g_plants
     :              , N_green
     :               )
*     ===========================================================
      Use Infrastructure
      implicit none

*+  Sub-Program Arguments
      REAL       C_n_init_conc(*)      ! (INPUT) initial N concentration (
      INTEGER    max_part              ! (INPUT) size of plant part array
      REAL       G_dm_green(*)         ! (INPUT) live plant dry weight (biomass
      real       N_green(*)            ! (INPUT) plant nitrogen (g/m^2)
      real       g_lai                 ! (INPUT) leaf area index (m2/m2)
      real       g_plants              ! (INPUT) plants / m^2

*+  Purpose
*   Initialise plant Nitrogen pools

*+  Mission Statement
*   Initialise plant Nitrogen pools

*+  Changes
*     210498 nih specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sorg_N_init1')

*+  Local Variables
      real    leafarea
      integer part

*- Implementation Section ----------------------------------

      call push_routine (my_name)


      leafarea = divide(g_lai * 10000, g_plants, 0.0)
         ! Need to check if initialisation has already taken place.
      if (leafarea .lt. 4.0
     :    .and. sum_real_array(N_green, max_part) .lt. 1.0e-6) then   ! ???? what's magic about 4?
         do 100 part = 1, max_part
            N_green(part) = c_N_init_conc(part)*g_dm_green(part)
  100    continue
         N_green(leaf) = c_N_init_conc(leaf) * g_lai
      endif

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine sorg_N_demand3
     :               (
     :                max_parts,
     :                G_dlt_dm_green,
     :                G_dm_green,
     :                G_n_green,
     :                g_lai,
     :                g_dlt_lai,
     :                g_dlt_slai,
     :                G_current_stage,
     :                g_grain_no,
     :                g_plants,
     :                g_tt_tot_fm,
     :                g_dlt_tt_fm,
     :                g_dlt_tt,
     :                g_phase_tt,
     .                c_x_stage_code,
     .                c_y_N_conc_crit_stem,
     .                c_n_target_conc,
     :                N_demand
     :               )
*     ===========================================================
      Use Infrastructure
      implicit none


*+  Sub-Program Arguments
      integer    max_parts
      REAL       G_dlt_dm_green(*)     ! (INPUT)  plant biomass growth (g/m^2)
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
      REAL       G_n_green(*)          ! (INPUT)  plant nitrogen content (g N/m^
      real       g_lai,g_dlt_lai,g_dlt_slai,g_grain_no
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      real       g_plants
      real       N_demand (*)          ! (OUTPUT) critical plant nitrogen demand
      real       g_tt_tot_fm (*)
      real       g_dlt_tt_fm
      real       g_dlt_tt
      real       g_phase_tt (*)
      real       c_x_stage_code(*)
      real       c_y_n_conc_crit_stem(*)
      real       c_n_target_conc(*)


*+  Purpose
*       Return plant nitrogen demand for each plant component

*+  Mission Statement
*       Calculate nitrogen demand and maximum uptake for each plant pool

*+  Notes
*+  Changes

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sorg_N_demand3')

*+  Local Variables


      real    N_required
      real    lai                        ! todays lai
      real    gf_tt_now
      real    grain_no
      real    NTargetStem
      real    SLN, NFillFact, gf_tt
      integer i,   numvals               ! number of values in stage code table

      save SLN

*- Implementation Section ----------------------------------
      call push_routine (my_name)


      call fill_real_array (n_demand, 0.0, max_part)

      if (G_current_stage .lt. emerg)then
         SLN = 0.0
      endif

!     ROOT demand to keep root [N] at 0.2%   (1.2%) c_n_target_conc
!     get root + delta root
!     and calculate rootN  needed to keep [N] above 0.2%
!     root Ndemand = rootNrequired - rootN

      N_required = (G_dlt_dm_green(root) + G_dm_green(root))
     .         * c_n_target_conc(root)
      N_demand(root) = max(0.0, N_required - G_n_green(root))

!     STEM demand to keep stem [N] at levels from  y_N_conc_crit_stem
      if (G_current_stage .lt. start_grain_fill )then
         numvals = count_of_real_vals (c_x_stage_code, max_stage)

         NTargetStem = linear_interp_real (G_current_stage
     :                             , c_x_stage_code
     :                             , c_y_N_conc_crit_stem
     :                             , numvals)

         N_required = (G_dlt_dm_green(stem) + G_dm_green(stem)) *
     .                  NTargetStem
         N_demand(stem) = max(0.0, N_required - G_n_green(stem))

!        FLOWER demand to keep flower [N] at c_n_target_conc
         N_required = (G_dlt_dm_green(flower) + G_dm_green(flower))
     .         * c_n_target_conc(flower)
         N_demand(flower) = max(0.0, N_required - G_n_green(flower))

      endif

      lai = max(0.0, g_lai + g_dlt_lai - g_dlt_slai)
      SLN = divide(G_n_green(leaf), lai, 0.0)

!     LEAF demand to keep SLN = c_n_target_conc
      N_required = 0
      if (G_current_stage .lt. flag_leaf )then
         N_required = lai * c_n_target_conc(leaf)
      else
         N_required = lai * min(SLN,c_n_target_conc(leaf))
      endif

      N_demand(leaf) = max(0.0, N_required - G_n_green(leaf))

!     GRAIN demand to keep grain N filling rate at 0.001mg/grain/dd
!       where dd is degree days from start_grain_fill
!
!       g_grain_no is the final grain number.
!       Ramp grain number from 0 at StartGrainFill to g_grain_no at SGF + 200dd
      if(G_current_stage .ge. start_grain_fill )then
         gf_tt_now = sum_between(start_grain_fill,now,g_tt_tot_fm)
         grain_no = min((gf_tt_now/200.0 *  g_grain_no),g_grain_no)   !200dd is???

         N_required = grain_no * g_dlt_tt *
     .                     c_n_target_conc(grain) / 1000.0
         N_demand(grain) = max(0.0, N_required )

      endif

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================

*     ===========================================================

      subroutine sorg_N_uptake2
     :               (
     :                C_no3_diffn_const,
     :                G_dlayer,
     :                G_no3gsm_diffn_pot,
     :                G_no3gsm_mflow_avail,
     :                G_N_fix_pot,
     :                c_n_supply_preference,
     :                G_n_demand,
     :                G_root_depth,
     :                NFract,
     :                G_current_stage,
     :                dlt_NO3gsm
     :               )
*     ===========================================================
      Use Infrastructure
      implicit none

*+  Sub-Program Arguments
      REAL       C_no3_diffn_const     ! (INPUT)  time constant for uptake by di
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL       G_no3gsm_diffn_pot(*) ! (INPUT)  potential NO3 (supply) from so
      REAL       G_no3gsm_mflow_avail(*) ! (INPUT)  potential NO3 (supply) from
      REAL       G_N_Fix_Pot           ! (INPUT) potential N fixation (g/m2)
      CHARACTER  c_n_supply_preference*(*) !(INPUT)
      REAL       G_n_demand(*)         ! (INPUT)  critical plant nitrogen demand
      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
      real       dlt_NO3gsm(*)         ! (OUTPUT) actual plant N uptake
      REAL       G_current_stage       ! (INPUT)  current phenological stage
                                       ! from NO3 in each layer (g/m^2)
      real       NFract

*+  Purpose
*       Return actual plant nitrogen uptake from
*       each soil layer.

*+  Mission Statement
*   Calculate crop Nitrogen Uptake

*+  Changes
*       160498 nih specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sorg_N_uptake2')

*+  Local Variables
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      real       NO3gsm_diffn          ! actual N available (supply) for
                                       ! plant (g/m^2) by diffusion
      real       NO3gsm_mflow          ! actual N available (supply) for
                                       ! plant (g/m^2) by mass flow
      real       NO3gsm_diffn_avail(crop_max_layer) ! potential NO3 (supply)
                                       ! from soil (g/m^2), by diffusion
      real       NO3gsm_diffn_supply   ! total potential N uptake (supply)
                                       ! for plant (g/m^2) by diffusion
      real       NO3gsm_mflow_supply   ! total potential N uptake (supply)
                                       ! for plant (g/m^2) by mass flow
      real       diffn_fract           ! fraction of nitrogen to use (0-1)
                                       ! for diffusion
      real       mflow_fract           ! fraction of nitrogen to use (0-1)
                                       ! for mass flow
      integer    layer                 ! soil layer number of profile
      real       N_demand              ! total nitrogen demand (g/m^2)
      real       NO3gsm_uptake         ! plant NO3 uptake from layer (g/m^2)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

            ! get potential N uptake (supply) from the root profile.
            ! get totals for diffusion and mass flow.

      deepest_layer = find_layer_no (g_root_depth
     :                              ,g_dlayer
     :                              ,max_layer)
      do 1000 layer = 1, deepest_layer
         NO3gsm_diffn_avail(layer) = g_NO3gsm_diffn_pot(layer)
     :                             - g_NO3gsm_mflow_avail(layer)
         NO3gsm_diffn_avail(layer) = l_bound (NO3gsm_diffn_avail(layer)
     :                                       , 0.0)
1000  continue
      NO3gsm_mflow_supply = sum_real_array (g_NO3gsm_mflow_avail
     :                                     , deepest_layer)
      NO3gsm_diffn_supply = sum_real_array (NO3gsm_diffn_avail
     :                                     , deepest_layer)

            ! get actual total nitrogen uptake for diffusion and mass flow.
            ! If demand is not satisfied by mass flow, then use diffusion.
            ! N uptake above N critical can only happen via mass flow.

      N_demand = sum_real_array (g_N_demand, max_part)
     :    - g_N_demand(grain)


      if (NO3gsm_mflow_supply.ge.N_demand) then
!jh         NO3gsm_mflow = NO3gsm_mflow_supply
!           Sorghum does not take up luxury N here
         NO3gsm_mflow = N_demand
         NO3gsm_diffn = 0.0
!+++++++++++++==                        need to do something with excess N
      else

         NO3gsm_mflow = NO3gsm_mflow_supply

         if (c_n_supply_preference.eq.'active') then
            NO3gsm_diffn = bound (N_demand - NO3gsm_mflow, 0.0
     :                        , NO3gsm_diffn_supply)

         elseif (c_n_supply_preference.eq.'fixation') then
            NO3gsm_diffn = bound (N_demand - NO3gsm_mflow - g_N_fix_pot
     :                        , 0.0
     :                        , NO3gsm_diffn_supply)

         else
            call fatal_error (ERR_USER, 'bad n supply preference')
         endif
         NO3gsm_diffn = divide (NO3gsm_diffn, c_NO3_diffn_const, 0.0)
      endif

            ! get actual change in N contents

      call fill_real_array (dlt_NO3gsm, 0.0, max_layer)

      do 1100 layer = 1,deepest_layer

               ! allocate nitrate
               ! Find proportion of nitrate uptake to be taken from layer
               ! by diffusion and mass flow

         mflow_fract = divide (g_NO3gsm_mflow_avail(layer)
     :                       , NO3gsm_mflow_supply, 0.0)

         diffn_fract = divide (NO3gsm_diffn_avail(layer)
     :                       , NO3gsm_diffn_supply, 0.0)

               ! now find how much nitrate the plant removes from
               ! the layer by both processes

         NO3gsm_uptake = NO3gsm_mflow * mflow_fract
     :                 + NO3gsm_diffn * diffn_fract
         dlt_NO3gsm(layer) = - NO3gsm_uptake
1100  continue

      if(N_demand .gt. 0.1e-5)then
         NFract = min((NO3gsm_mflow + NO3gsm_diffn) / N_demand,1.0)
      else
         NFract = 0.0
      endif



      call pop_routine (my_name)
      return
      end subroutine



* ====================================================================

      subroutine sorg_N_partition1(
     .          g_N_demand,
     .          NFract,
     .          dlt_N_green)

*     ===========================================================
      Use Infrastructure
      implicit none

*+  Sub-Program Arguments
      ! DEMAND
      real    g_N_demand(*)            ! (INPUT) N demand by part
      real    NFract                   ! Demand/Supply ratio of available N
      real    dlt_N_green(max_part)    ! (OUTPUT) actual plant N uptake
                                       ! into each plant part (g/m^2)
      integer i
*+  Purpose
*     Return actual plant nitrogen uptake to each plant part and from
*     each soil layer.

*+  Changes
*      080994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sorg_N_partition1')

*+  Local Variables
      integer    part                  ! plant part number


*- Implementation Section ----------------------------------
      call push_routine (my_name)
		! Grain does not take up N from soil - only retranslocates
      do 1300 part = root, flower
        dlt_N_green(part) = g_N_demand(part) * NFract
1300  continue

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine sorg_N_retranslocate1 (
     .          g_N_demand,g_dlt_N_senesced,
     .          NFract,
     .          g_lai, g_dlt_lai, g_dlt_slai,
     .          G_n_green, dlt_N_green,
     .          G_phase_tt,g_tt_tot_fm,g_dlt_tt_fm,
     .          g_nfact_expansion,
     :          G_dlt_dm_green,
     :          G_dm_green,
     .          g_current_stage,
     .          g_dlt_N_retrans)

*     ===========================================================
      Use Infrastructure
      implicit none

*+  Sub-Program Arguments
      REAL       G_dlt_dm_green(*)     ! (INPUT)  plant biomass growth (g/m^2)
      real       g_dlt_N_senesced(*)
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
      real       g_N_demand(*)
      real       NFract                ! Demand/Supply ratio of available N
      real       g_lai,g_dlt_lai,g_dlt_slai
      REAL       G_n_green(*)          ! (INPUT)  plant nitrogen content (g N/m^
      REAL       dlt_n_green(*)
      real       G_phase_tt (*)        ! (INPUT/OUTPUT) cumulative growing
      real       g_dlt_tt_fm
      real       g_nfact_expansion
      real       g_tt_tot_fm(*)
      real       g_current_stage

      real       g_dlt_N_retrans (*)   ! (OUTPUT) plant N taken out from
                                       ! plant parts (g N/m^2)

*+  Purpose
*     Calculate the nitrogen retranslocation from the various plant parts
*     to the grain.
*
*       Called by srop_nit_retrans(1) in croptree
*       Calls srop_N_dlt_grain_conc,  srop_N_retrans_avail   in crop

*+  Changes
*     080994 jngh specified and programmed

*+  Calls

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sorg_N_retranslocate1')

*+  Local Variables
      integer    part                  ! plant part number
      real SLN,lai
      real dd,NAvail,NRequired

      real StemNRequired,LeafNRequired,StemNAvail,FlowerNAvail
      real ddGF,ddGFNow
      real LeafN,LeafN100,LeafN50,reduct,NReq
      real LowNStem,LowNHead,StemHeadNAvail,LeafNAvail
      real nConcStem,nStemRate,nLeafRate ,leafSenesced
      real SLNdead,dltSLN
      real N_avail(max_part)
      real N_begin
      real N_end
      character*200 string

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call fill_real_array (g_dlt_N_retrans, 0.0, max_part)
      N_begin = sum_real_array(g_dlt_N_retrans, max_part)

!-------------------------------------------------------------------
!
!   Note: New leaf growth SLN is 1.0
!   during vegative stage (when there is leaf N demand)
!       no retrans after flag
!   if N(leaf) demand > N(leaf) supply
!      a)   reduce SLN to 1 (this happens by itself as LeafN reduces)
!      b)   reduce SLN to 0.5 while reducing dlt_lai from 100% to 50%
!            until demand is reduced to supply

      lai = max(0.0, g_lai + g_dlt_lai - g_dlt_slai)
      LeafN = G_n_green(leaf) + dlt_N_green(leaf)

      SLN = divide(LeafN, lai, 0.0)


!     vegetative stage
      g_nfact_expansion = 1.0
      if(g_N_demand(leaf).gt. 0.0)then
         if(SLN .lt. 1.0)then
!             b)   reduce SLN to 0.5 while reducing dlt_lai from 100% to 50%
!             Note: New leaf growth SLN is 1.0
            LeafN50 = 0.5 *g_dlt_lai *1.0 +(g_lai - g_dlt_slai)* 0.5  !?????
            if(LeafN .ge. LeafN50)then
               LeafN100 = lai * 1.0
               reduct = (LeafN-LeafN50)/(LeafN100-LeafN50)*0.5 + 0.5
            else
               NReq = LeafN50 - LeafN
!              get this from killing leaf (SLN = 0.2)
               g_dlt_slai = g_dlt_slai + NReq / 0.3
!              Stop LAI going negative
               g_dlt_slai = min(g_dlt_slai,g_lai + g_dlt_lai)
               reduct = 0.5

            endif
            g_nfact_expansion = reduct
         endif
      endif
!-------------------------------------------------------------------
!
!   in reproductive stage, if n_demand(grain) > supply
!     take from stem then leaf
!     rate of movement of N to grain from stem and leaf is a function
!     of the [N] of those parts
!     Stem rate   dlt[N]%/17dd   = 0.025            [N]% >= 0.7
!                                = 0.05[N]% -0.01   0.2 < [N]% < 0.7
!                                = 0                [N]% <= 0.2
!     Leaf Rate  dltSLN/17dd     = 0.04SLN - 0.016  SLN > 0.4
!       when SLN = 0.4, need to senesce to get more N

      if (g_n_demand(grain) .gt. 0.0 .and. NFract .lt. 1) then
         NRequired = g_n_demand(grain) * (1 - NFract)
!        reduce n required by the amount that will be supplied by
!        leaf senescence. Can get N from SenLeaf until SLNdead = 0.3
c        at the leaf rate
         if (g_dlt_slai .gt. 0) then
             SLNdead = (g_dlt_N_senesced(leaf)/g_dlt_slai)
         else
             SLNdead = 0
         endif
         if (SLNdead > 0.3) then
            dltSLN = max(0.04 * SLNdead - 0.016,0)
         else
            dltSLN = 0
         endif
         LeafNAvail = dltSLN * g_dlt_slai
         N_avail(leaf) = LeafNAvail

c        remove from the senesced pool
         if (LeafNAvail .ge. nRequired) then
            g_dlt_N_Senesced(leaf) = g_dlt_N_Senesced(leaf) - nRequired
				! N must be removed from green leaf if senesced N is reduced
            g_dlt_N_retrans(leaf) = - nRequired
            g_dlt_N_retrans(grain) = nRequired
            nRequired = 0
         else
            g_dlt_N_Senesced(leaf) = g_dlt_N_Senesced(leaf) - LeafNAvail
				! N must be removed from green leaf if senesced N is reduced
            g_dlt_N_retrans(leaf) = - LeafNAvail
            nRequired = nRequired - LeafNAvail
            g_dlt_N_retrans(grain) = LeafNAvail
         endif



!        STEM  and Flower
         if (nRequired .gt. 0) then
            nConcStem = (G_n_green(stem)+dlt_N_green(stem)+
     .                         G_n_green(flower)+dlt_N_green(flower)) /
     .             (G_dlt_dm_green(stem) + G_dm_green(stem) +
     .               G_dlt_dm_green(flower) + G_dm_green(flower)) * 100
            if (nConcStem .ge. 0.7) then
               nStemRate = 0.7
            elseif (nConcStem .gt. 0.2) then
               nStemRate = 0.05 * nConcStem - 0.01
            else
               nStemRate = 0
            endif
            nStemRate = g_dlt_tt_fm / 17.0 * nStemRate
!           Take from Stem
            StemNAvail = min(nStemRate/100 *
     .           (G_dlt_dm_green(stem) + G_dm_green(stem)),NRequired)
            g_dlt_N_retrans(stem) = - StemNAvail
            g_dlt_N_retrans(grain) = g_dlt_N_retrans(grain) + StemNAvail
            NRequired = NRequired - StemNAvail
            N_avail(stem) = StemNAvail
!           Take from flower
            FlowerNAvail = min(nStemRate/100 *
     .          (G_dlt_dm_green(flower) + G_dm_green(flower)),NRequired)
            g_dlt_N_retrans(flower) = - FlowerNAvail
            g_dlt_N_retrans(grain) = g_dlt_N_retrans(grain)+FlowerNAvail
            NRequired = NRequired - FlowerNAvail
            N_avail(flower) = FlowerNAvail
         endif

!        LEAF
         if (NRequired .gt. 0) then
            if (SLN .gt. 0.4) then
               nLeafRate = 0.04 * SLN - 0.016 * (g_dlt_tt_fm / 17.0)
               LeafNAvail = min(nLeafRate * lai, ((SLN - 0.4) *  lai))
               LeafNRequired = min(LeafNAvail,NRequired)
              NRequired = NRequired - LeafNRequired
					! Some Leaf N possibly already retranslocated
               g_dlt_N_retrans(leaf) =
     :                           g_dlt_N_retrans(leaf) - LeafNRequired
               g_dlt_N_retrans(grain) =
     .                           g_dlt_N_retrans(grain) + LeafNRequired
           else
!              need to senesce leaf to get the required N  (SLN = 0.2)
               leafSenesced =  min(lai,NRequired / 0.2)
               LeafNAvail = leafSenesced * 0.2
               g_dlt_N_retrans(leaf) = g_dlt_N_retrans(leaf)- LeafNAvail
               NRequired = NRequired - LeafNAvail
					! should retranslocate to grain same amount retranslocated from leaf
               g_dlt_N_retrans(grain) = g_dlt_N_retrans(grain) +
     .                                 LeafNAvail
            endif
            N_avail(leaf) = LeafNAvail + N_avail(leaf)
         endif
      endif

! just check that we got the maths right.

      do 1000 part = root, flower
         call bound_check_real_var (abs (g_dlt_N_retrans(part))
     :                            , 0.0, N_avail(part)
     :                            , 'g_dlt_N_retrans(part)')
1000  continue

      N_end = sum_real_array(g_dlt_N_retrans, max_part)
      if (abs(N_end-N_begin) .gt. 0.1e-5) then
         call write_string ('N balance error in retranslocation')
      write (string, '(4x,a, f15.5)')
     :                'N_begin                 = ', N_begin
      call write_string (string)
      write (string, '(4x,a, f15.5)')
     :                'N_end                 = ', N_end
      call write_string (string)
      endif

      call pop_routine (my_name)
      return
      end subroutine

