      Module crp_commModule

      contains

!     Last change:  P     1 Nov 2000   12:11 pm
!     ===========================================================
      subroutine crop_root_incorp (          &
                             dlt_dm_root          &
                            ,dlt_N_root          &
                            ,g_dlayer          &
                            ,g_root_length          &
                            ,g_root_depth          &
                            ,c_crop_type          &
                            ,max_layer          &
                            ,event_interface          &
                            )
!     ===========================================================

!      dll_export crop_root_incorp
      use ConstantsModule            ! all_active_modules
      use convertmodule
      use scienceModule
      use interfaceModule
      use ComponentInterfaceModule
      use dataModule
      use errorModule
      use crp_rootModule
      use postboxModule
      implicit none

!+  Sub-Program Arguments
      real       dlt_dm_root           ! (INPUT) new root residue dm (g/m^2)
      real       dlt_N_root            ! (INPUT) new root residue N (g/m^2)
      real       g_dlayer(*)           ! (INPUT) layer thicknesses (mm)
      real       g_root_length(*)      ! (INPUT) layered root length (mm)
      real       g_root_depth          ! (INPUT) root depth (mm)
      character  c_crop_type*(*)       ! (INPUT) crop type
      integer    max_layer             ! (INPUT) maximum no of soil layers
      INTEGER    event_interface       ! (INPUT) event interface object that
                                       !         allows comms. to other comps.

!+  Purpose
!       Calculate and provide root matter incorporation information
!       to the APSIM messaging system.

!+  Mission Statement
!   Pass root material to the soil modules (based on root length distribution)

!+  Changes
!     <insert here>
!     280800 jngh changed literal incorp_fom to ACTION_incorp_fom
!     011100 dph  added event_interface as a parameter.

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'crop_root_incorp')

!+  Local Variables
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      real       dlt_dm_incorp(crop_max_layer) ! root residue (kg/ha)
      real       dlt_N_incorp(crop_max_layer)  ! root residue N (kg/ha)

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (max_layer .gt. crop_max_layer) then
         call fatal_error (Err_Internal          &
                    ,'Too many layers for crop routines')

      else

         if (dlt_dm_root.gt.0.0) then

               ! send out root residue

            call crop_root_dist          &
               (          &
                G_dlayer          &
              , G_root_length          &
              , G_root_depth          &
              , dlt_dm_incorp          &
              , dlt_dm_root * gm2kg /sm2ha          &
              , max_layer          &
               )

            call bound_check_real_array          &
                (          &
                 dlt_dm_incorp          &
                ,0.0          &
                ,dlt_dm_root * gm2kg/sm2ha          &
                ,'dlt_dm_incorp'          &
                ,max_layer          &
                )

            call crop_root_dist          &
               (          &
                G_dlayer          &
              , G_root_length          &
              , G_root_depth          &
              , dlt_N_incorp          &
              , dlt_N_root * gm2kg /sm2ha          &
              , max_layer          &
               )
            call bound_check_real_array          &
               (          &
                dlt_n_incorp          &
               ,0.0          &
               ,dlt_n_root * gm2kg/sm2ha          &
               ,'dlt_n_incorp'          &
               ,max_layer          &
               )

            deepest_layer = find_layer_no          &
                         (g_root_depth          &
                         ,g_dlayer          &
                         ,max_layer)

            call New_postbox ()

            call post_char_var('dlt_fom_type=','()',c_crop_type)

            call post_real_array ('dlt_fom_wt'          &
                           ,'(kg/ha)'          &
                           ,dlt_dm_incorp          &
                           ,deepest_layer)

            call post_real_array ('dlt_fom_n'          &
                           ,'(kg/ha)'          &
                           ,dlt_n_incorp          &
                           ,deepest_layer)

         call EI_BroadcastAction          &
                           (Event_interface          &
                            ,ACTION_incorp_fom          &
                            ,Blank          &
                            )

            call Delete_postbox ()

         else
            ! no roots to incorporate
         endif
      endif

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine crop_top_residue (          &
                                   c_crop_type          &
                                  ,dlt_residue_weight          &
                                  ,dlt_residue_N          &
                                  ,event_interface          &
                                  )
!     ===========================================================

!      dll_export crop_top_residue
      use ConstantsModule            ! all_active_modules
      use convertmodule
      use interfaceModule
      use errorModule
      use postboxModule
      implicit none

!+  Sub-Program Arguments
      character c_crop_type*(*)
      real       dlt_residue_weight    ! (INPUT) new surface residue (g/m^2)
      real       dlt_residue_N         ! (INPUT) new surface residue N (g/m^2)
      INTEGER    event_interface       ! (INPUT) event interface object that
                                       !         allows comms. to other comps.

!+  Purpose
!       Add residue to residue pool

!+  Mission Statement
!   Pass surface residues to the residue module

!+  Changes
!     <insert here>
!     280800 jngh changed ok = Loader_SendActionToFirstComp
!                    to call   Loader_SendActionToAllComps (D372)
!     280800 jngh changed literal add_residue to ACTION_add_residue
!     011100 dph  added event_interface as a parameter.

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'crop_top_residue')

!+  Local Variables
      logical ok
!- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (dlt_residue_weight.gt.0.0) then
            ! send out surface residue

         call New_postbox ()

         call post_char_var('dlt_residue_type','()',c_crop_type)

         call post_real_var ('dlt_residue_wt'          &
                        ,'(kg/ha)'          &
                        ,dlt_residue_weight * gm2kg /sm2ha)

         call post_real_var ('dlt_residue_n'          &
                        ,'(kg/ha)'          &
                        ,dlt_residue_N * gm2kg /sm2ha)

         call EI_BroadcastAction          &
                           (Event_interface          &
                            ,ACTION_add_residue          &
                            ,Blank)

         call Delete_postbox ()

      else
         ! no surface residue
      endif

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      logical function crop_my_type (c_crop_type)
!     ===========================================================

!      dll_export crop_my_type
      use interfacemodule
      use errorModule
      implicit none

!+  Sub-Program Arguments
      character c_crop_type*(*)

!+  Purpose
!       Returns true if 'type' is equal to the crop type or is absent.

!+  Mission Statement
!   the crop type = %1

!+  Assumptions
!       If type is not specified, it is assumed the message was addressed
!        directly to the module.

!+  Changes
!     <insert here>

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'crop_my_type')

!+  Local Variables
      character  crop_type*50          ! crop type in data string
      integer    numvals               ! number of values returned

!- Implementation Section ----------------------------------
      call push_routine (my_name)

      call collect_char_var_optional ('type', '()'          &
                              , crop_type, numvals)

      if (crop_type.eq.c_crop_type .or. numvals.eq.0) then
         crop_my_type = .true.
      else
         crop_my_type = .false.
      endif

      call pop_routine (my_name)
      return
      end function



! ====================================================================
      subroutine crop_get_ext_uptakes (uptake_source          &
                                ,crop_type          &
                                ,uptake_type          &
                                ,unit_conversion_factor          &
                                ,uptake_lbound          &
                                ,uptake_ubound          &
                                ,uptake_array          &
                                ,max_layer          &
                                )
! ====================================================================

!      dll_export crop_get_ext_uptakes
      use ConstantsModule
      use stringModule
      use interfacemodule
      use errorModule
      implicit none

!+  Sub-Program Arguments
      character uptake_source*(*)   !(INPUT) uptake flag
      character crop_type*(*)       !(INPUT) crop type name
      character uptake_type*(*)     !(INPUT) uptake name
      real      unit_conversion_factor!(INPUT) unit conversion factor
      real      uptake_lbound       !(INPUT) uptake lower limit
      real      uptake_ubound       !(INPUT) uptake upper limit
      real      uptake_array(*)     !(OUTPUT) crop uptake array
      integer   max_layer           !(INPUT) max layer number

!+  Purpose
!      Ask swim for uptakes of water or solute

!+  Mission Statement
!   Get the soil uptake for %3 from another module

!+  Notes
!      Bounds should probably be passed in when crops decide what
!      these should be (ie when ini files have limits for uptake
!      in them)

!+  Changes
!     08-05-1997 - huth - Programmed and Specified

!+  Calls


!+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'crop_get_ext_uptakes')

!+  Local Variables
      integer   layer                        ! layer counter
      integer   num_uptakes                  ! num uptake vals
      character uptake_name*(MES_DATA_SIZE)  ! Uptake variable name

!- Implementation Section ----------------------------------
      call push_routine (myname)

      if ((uptake_source.eq.'apsim')          &
          .and.          &
    (crop_type.ne.' '))          &
then
         ! NB - if crop type is blank then swim will know nothing
         ! about this crop (eg if not initialised yet)

         uptake_name = string_concat('uptake_',uptake_type)
         uptake_name = string_concat(uptake_name,'_')
         uptake_name = string_concat(uptake_name,crop_type)

         call get_real_array (unknown_module          &
                       ,uptake_name          &
                       ,max_layer          &
                       ,'()'          &
                       ,uptake_array          &
                       ,num_uptakes          &
                       ,uptake_lbound          &
                       ,uptake_ubound)

         do 100 layer = 1, num_uptakes
            uptake_array(layer) = uptake_array(layer)          &
                          * unit_conversion_factor
  100    continue
      else
      endif

      call pop_routine (myname)
      return
      end subroutine




      end module crp_commModule
