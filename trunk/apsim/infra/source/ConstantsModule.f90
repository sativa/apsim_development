! ====================================================================
!      Constant definitions
! ====================================================================

!   Short description:
!      Globally used constant definitions

!   Notes:

!   Changes:
!      DPH 2/07/92
!      DPH 15/02/93 Added MES_Get_variable and MES_Ret_variable
!      JNGH 11/02/94 increased Mes_data_size to from 500 to 600.
!      JNGH 4/8/94 included file_not_found flag
!                    changed function string length to = mes data size
!      JNGH 29/9/94 added harvest, sow and no_section
!      DPH 7/11/94 Removed var_delimiter.  Modified Mes_delimiter to ','
!                  Changed names of Global ... module specifiers.
!                  Added MES_End_crop and MES_Kill_crop messages
!      DPH 20/10/95 Changed MES_Data_size to 50 characters.
!                   Removed MES_Ret_variable.
!      JNGH 26/2/96 added MES_INITIATE_CROP
!      DPH 20/5/96  MES_intertime_step, Max_variable_name_size, MES_report
!      JNGH 22/06/96 reduced function string length to 600 from 1000
!      dph 24/2/97  added lu_log_file, MES_start, MES_pause, MES_continue,
!                   MES_idle
!      nih 19/8/97  added MES_reset and MES_Sum_Report
!      igh 18/08/98 Changed MES_Till from 'till' to 'tillage'
!      sb 1/9/98 Added max_year and min_year.
!      jngh 21/10/98 added MNP messages
!      nih 09/09/99 changed MES_init contents
!      dph 22/9/99  changed new_line to char(10)
!      dph 23/9/99  moved MES_ constants to action.inc
!      dph 27/9/99  removed all lu_ constants
!      jngh 15/12/00 added close_enough_to_zero
!      dph 22/6/01 renamed file to componentdefinitions.f90 and converted
!                  to a F90 module.

! ----------------------- Declaration section ------------------------

module ConstantsModule
   implicit none

   ! blank character.
   character (len=*), parameter :: Blank = ' '

   ! Size of a message's address char. string
   integer, parameter :: Mes_address_size = 8


   ! Size of action part of message
   integer, parameter :: Mes_Action_size = 20

   ! Size of data part of message
   integer, parameter :: Mes_Data_size = 50

   ! Message delimiter
   character (len=*), parameter :: Mes_delimiter = ','

   ! Len. of string returned by char. fun.
   integer, parameter :: Function_string_len = 600

   ! minimum year for APSIM simulations.
   integer, parameter :: min_year = 1800

   ! maximum year for APSIM simulations.
   integer, parameter :: max_year = 2200

   character (len=*), parameter :: All_active_modules ='act_mods'
   character (len=*), parameter :: Unknown_module = 'unk_mod'
   character (len=*), parameter :: First_active_module = 'unk_mod'
                                                      
   ! Smallest number considered to be zero
   real, parameter :: close_enough_to_zero = 1.0e-15

   ! maximum character size
   integer, parameter :: max_line_size = 2000

   ! maximum allowed size for filenames
   integer, parameter :: Max_file_name_size = 100

   ! maximum allowed size for module names
   integer, parameter :: Max_module_name_size = 30

   ! maximum allowed size for module names
   integer, parameter :: Max_inst_name_size = max_module_name_size+3

   ! Maximum allowed size for section names
   integer, parameter :: Max_section_name_size = 50

   ! maximum size of a variable name.
   integer, parameter :: Max_variable_name_size = 35

   ! maximum size for title
   integer, parameter :: Max_title_size = 100

   ! maximum soil layers
   integer, parameter :: crop_max_layer = 100


   ! New line delimiter
   character (len=*), parameter :: New_line = char(10)

   ! identifier for no section returned
   character (len=*), parameter :: no_section = ' '

   character (len=*), parameter :: ACTION_Create        = 'create'
   character (len=*), parameter :: ACTION_Init          = 'init'
   character (len=*), parameter :: ACTION_Get_variable = 'get'
   character (len=*), parameter :: ACTION_Set_variable = 'set'
   character (len=*), parameter :: ACTION_Prepare      = 'prepare'
   character (len=*), parameter :: ACTION_Process      = 'process'
   character (len=*), parameter :: ACTION_Post         = 'post'

end module ConstantsModule

