*     ===========================================================
      character*(*) function Manager_version ()
*     ===========================================================

*   Short description:
*       return version number of Manager module

*   Assumptions:
*       none

*   Notes:
*       none

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:       
*       DPH - 8/10/92

*   Calls:
*       none

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
*       none

*   Internal variables
*       none

*   Constant values

      character  version_number*(*)    ! version number of module
      parameter (version_number = 'V1.20  04/09/96')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------

      Manager_version = version_number

      return
      end
* ====================================================================
       subroutine APSIM_Manager (Action, Data_string)
* ====================================================================

*   Short description:
*      This module acts as the APSIM manager.

*   Assumptions:

*   Notes:

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*      DPH - 7/10/92
*      DPH - 9/02/95 Substantially modified to incorporate a better
*                    parsing method allowing nesting of brackets in
*                    rules, nesting of ANDS and ORs and allowing
*                    local variables to be defined.
*     jngh 24/2/95 changed data to data_string
*     DPH 19/7/95  Added call to manager_process
*     DPH 27/10/95 Added call to message_unused
*     jngh - 08/06/96 removed a_ from front of version function

*   Calls:
*     Manager_init
*     Manager_prepare
*     Manager_event
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
       character Action*(*)            ! Message action to perform
       character Data_string*(*)       ! Message data

*   Global variables
       include 'const.inc'             ! Global constant definitions
       include 'manager.inc'           ! Manager common block

       character Manager_version*15    ! function

*   Internal variables
*      None

*   Constant values
      character  my_name*(*)           ! name of this procedure
      parameter (my_name='manager')


*   Initial data values
*      None

* --------------------- Executable code section ----------------------
      call push_routine (my_name)
      
      if (Action.eq.MES_Presence) then
         call Write_string (LU_Scr_sum,
     .       'Module = manager ' // Manager_version())

      else if (Action.eq.MES_Init) then
         call Manager_Init ()

      else if (Action.eq.MES_Prepare) then
         call Manager_Prepare ()

      else if (Action.eq.MES_Process) then
         call Manager_Process ()

      else if (Action.eq.MES_Post) then
         call Manager_Post ()

      else if (Action .eq. MES_Event) then
         call Manager_Event (Data_string)

      else if (action .eq. mes_get_variable) then
         call manager_send_my_variable (Data_string)

      else
         ! Don't use message

         call Message_unused ()
      endif

      call pop_routine (my_name)
      return
      end
* ====================================================================
       subroutine Manager_Init ()
* ====================================================================

*   Short description:
*      Initialise Manager model.

*   Assumptions:
*      None

*   Notes:

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*      DPH - 8/10/92
*      DPH - 21/10/94 Modified to bring up to APSIM 1.0 standard.
*      DPH - 6/7/95   Added check for case when no manager lines were found.
*                     Added code to output a 'manager rules' line to summary file
*      DPH - 19/7/95  Added call to manager_init_rules to allow the parsing routine
*                     to parse any user initialisation rules.

*   Calls:
*      Manager_version
*      Get_file_string
*      Manager_read_rules
*      Open_file
*      Return_logical_unit
*     report_event
*     pop_routine
*     push_routine


* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
*      none

*   Global variables
       include 'const.inc'             ! constant definitions
       include 'manager.inc'           ! Manager common block
       character Manager_version*15    ! function

*   Internal variables
       character  msg*200              ! err message

*   Constant values
       character ID_Init*(*)           ! Message indicating initialisation
       parameter (ID_Init='Initialising')

      character This_routine*(*)       ! Name of this routine
      parameter (This_routine='Manager_Init')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      call push_routine(This_routine)

      g_num_local_variables = 0
      g_lines_been_read = .false.

      call Report_event
     .    (ID_Init // ', Version : ' // Manager_version())

      msg = New_line // 'Manager rules' // New_line // '------- -----'
      call Write_string(LU_Summary_file, msg)

      call Manager_read_rules ()
      
      msg = New_line // 
     .    '-----------------------------------------------------------'
      call Write_string(LU_Summary_file, msg)


      ! check for case when no manager lines were found anywhere.  Issue warning
      
      if (g_lines_been_read) then
         ! we're ok - lines were found
         
      else
         msg = 'No manager lines were found in any parameter file.'
         call Warning_error(ERR_user, msg)
      endif

      call manager_init_rules ()

      call pop_routine(This_routine)

      return
      end
* ====================================================================
       subroutine Manager_read_rules ()
* ====================================================================

*   Short description:
*     Read in all criterias one word at a time and pass it to a processing
*     routine.

*   Assumptions:
*      None

*   Notes:

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*     DPH 5/12/94
*     DPH 19/7/95 Added code to look for init, prepare, process and post sections
*     DPH 10/7/96 Re-ordered code so that manager will look for init section
*                 first, then start_of_day, prepare in chronological order.

*   Calls:
*     Lower_case
*     Manager_process_word
*     No_leading_spaces
*     Read_line
*     WRite_string
*     Fatal_error
*     Split_line

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
*     none

*   Global variables
      include 'const.inc'              ! constant definitions
      include 'manager.inc'            ! Manager common block
      include 'parse.inc'              ! Parse common block

*   Internal variables
*     none

*   Constant values
      character Routine_name*(*)       ! Name of this routine
      parameter (Routine_name='Manager_read_rules')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      call push_routine (Routine_name)

      ! Set the read flag so that the next call to manager_read_line
      ! will restart the reading routine.

      g_read_flag = 1

      ! Go tokenize the parameter file.

      g_start_token = 1
      g_start_day_index1 = 1
      g_current_section = Blank
      call Tokenize (g_token_array, g_token_array2, max_tokens)

      g_start_token = g_last_token + 2
      g_init_index = g_start_token
      g_current_section = 'init'
      call Tokenize (g_token_array, g_token_array2, max_tokens)

      g_start_token = g_last_token + 2
      g_start_day_index2 = g_start_token
      g_current_section = 'start_of_day'
      call Tokenize (g_token_array, g_token_array2, max_tokens)
      
      g_start_token = g_last_token + 2
      g_prepare_index = g_start_token
      g_current_section = 'prepare'
      call Tokenize (g_token_array, g_token_array2, max_tokens)

      g_start_token = g_last_token + 2
      g_process_index = g_start_token
      g_current_section = 'process'
      call Tokenize (g_token_array, g_token_array2, max_tokens)

      g_start_token = g_last_token + 2
      g_end_day_index = g_start_token
      g_current_section = 'end_of_day'
      call Tokenize (g_token_array, g_token_array2, max_tokens)

      g_start_token = g_last_token + 2
      g_post_index = g_start_token
      g_current_section = 'post'
      call Tokenize (g_token_array, g_token_array2, max_tokens)

      call pop_routine(Routine_name)
      return
      end

* ====================================================================
       subroutine Manager_init_rules ()
* ====================================================================

*   Short description:
*     Check to see if any criteria for initialisation are met.  If
*     so then issue message to relevent module.

*   Assumptions:
*      None

*   Notes:

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*     DPH 19/7/95

*   Calls:
*     parse
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
*      none

*   Global variables
       include 'const.inc'             ! Global constant definitions
      include 'manager.inc'          ! Manager common block
      include 'parse.inc'            ! Parse common block

*   Internal variables
*     none

*   Constant values
      character  my_name*(*)           ! name of this procedure
      parameter (my_name='manager_init_rules')


*   Initial data values
*      none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      ! Go call the parsing routine.

      g_start_token = g_init_index
      call Parse (g_token_array, g_token_array2)

      call pop_routine (my_name)
      return
      end

* ====================================================================
       subroutine Manager_Prepare ()
* ====================================================================

*   Short description:
*     Check to see if any criteria for prepare is met.  If
*     so then issue message to relevent module.

*   Assumptions:
*      None

*   Notes:

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*     DPH 5/12/94
*     DPH 19/7/95  Added code to parse the prepare index part of token array

*   Calls:
*     parse
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
*      none

*   Global variables
       include 'const.inc'             ! Global constant definitions
      include 'manager.inc'          ! Manager common block
      include 'parse.inc'            ! Parse common block

*   Internal variables
*     none

*   Constant values
      character  my_name*(*)           ! name of this procedure
      parameter (my_name='manager_prepare')


*   Initial data values
*      none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      ! Go call the parsing routine.

      g_start_token = g_start_day_index1
      call Parse (g_token_array, g_token_array2)

      g_start_token = g_start_day_index2
      call Parse (g_token_array, g_token_array2)

      g_start_token = g_prepare_index
      call Parse (g_token_array, g_token_array2)

      call pop_routine (my_name)
      return
      end

* ====================================================================
       subroutine Manager_Process ()
* ====================================================================

*   Short description:
*     Check to see if any criteria for process is met.  If
*     so then issue message to relevent module.

*   Assumptions:
*      None

*   Notes:

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*     DPH 19/7/95

*   Calls:
*     parse
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
*      none

*   Global variables
       include 'const.inc'             ! Global constant definitions
      include 'manager.inc'          ! Manager common block
      include 'parse.inc'            ! Parse common block

*   Internal variables
*     none

*   Constant values
      character  my_name*(*)           ! name of this procedure
      parameter (my_name='manager_process')


*   Initial data values
*      none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      ! Go call the parsing routine.

      g_start_token = g_process_index
      call Parse (g_token_array, g_token_array2)

      call pop_routine (my_name)
      return
      end

* ====================================================================
       subroutine Manager_Post ()
* ====================================================================

*   Short description:
*     Check to see if any criteria for post is met.  If
*     so then issue message to relevent module.

*   Assumptions:
*      None

*   Notes:

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*     DPH 5/12/94
*     DPH 19/7/95  Added code to check in the post index part of token array.

*   Calls:
*     parse
*     pop_routine
*     push_routine

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
*      none

*   Global variables
       include 'const.inc'             ! Global constant definitions
      include 'manager.inc'          ! Manager common block
      include 'parse.inc'            ! Parse common block

*   Internal variables
*     none

*   Constant values
      character  my_name*(*)           ! name of this procedure
      parameter (my_name='manager_post')


*   Initial data values
*      none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      ! Go call the parsing routine.

      g_start_token = g_end_day_index
      call Parse (g_token_array, g_token_array2)

      g_start_token = g_post_index
      call Parse (g_token_array, g_token_array2)

      call pop_routine (my_name)
      return
      end
      
* ====================================================================
       subroutine Manager_event (Event_data)
* ====================================================================

*   Short description:
*     An event has occurred today.  Capture and store it in the
*     events string.

*   Assumptions:
*      None

*   Notes:
*     None

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*     DPH 12/1/94
*     DPH 11/7/94 Added call to no_leading_spaces.

*   Calls:
*     Lower_case
*     manager_check_rule

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      character Event_data*(*)         ! (INPUT) Event data string

*   Global variables
*     none

*   Internal variables
*     none

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      ! Convert module's event string to lowercase and remove
      ! the module name the event came from.

      return
      end
* ====================================================================
      subroutine manager_send_my_variable (variable_name)
* ====================================================================

*   Short Description:
*      return the value of a variable in return_string.  used to return
*      values of variables requested by other modules.

*   Assumptions:
*      none

*   Notes:

*   Procedure Attributes:
*      version:         any hardware/fortran77
*      extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                       implicit none

*   Changes:
*     DPH 9/02/95
*     DPH 27/10/95 Added call to message_unused
*     DPH 10/4/96  Changed the call from respond2get_real_var to
*                  respond2get_char_var so that character variables can
*                  be sent to other modules.

*   Calls:
*      count_of_real_vals
*      divide
*      l_bound
*      pop_routine
*      push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      character variable_name*(*)      ! (input) variable name to search for

*   Global variables
       include 'const.inc'             ! Global constant definitions
      include   'manager.inc'          ! manager common block
      integer Find_string_in_array     ! function

*   Internal variables
      integer Variable_index           ! index into local variable list

*   Constant values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'manager_send_my_variable')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      call push_routine (my_name)

      ! Try to find variable in local variable list.

      Variable_index = Find_string_in_array
     .   (Variable_name, g_local_variable_names, g_num_local_variables)

      if (Variable_index .gt. 0) then
         call respond2get_char_var (Variable_name, '()', 
     .                     g_local_variable_values(Variable_index))
      else
         ! not our variable
         
         call Message_unused ()
      endif

      call pop_routine (my_name)
      return
      end
* ====================================================================
       subroutine Parse_read_line(Line, EOF_flag)
* ====================================================================

*   Short description:
*     Read next line from file.  Return EOF_flag = 0 when end of
*     file encountered.

*   Assumptions:
*      None

*   Notes:
*     None

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*     DPH 5/12/94
*     DPH 6/7/95   Added code to set g_lines_been_read to .true.
*                  Added code to write all lines to summary file

*   Calls:
*     pop_routine
*     push_routine
* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      character Line*(*)               ! (OUTPUT) Line read from file
      integer EOF_flag                 ! (OUTPUT) = 0 when eof reached

*   Global variables
      include 'const.inc'              ! constant definition
      include 'manager.inc'           ! manager common block

*   Internal variables
*     none

*   Constant values
      character  my_name*(*)           ! name of this procedure
      parameter (my_name='Parse_read_line')


*   Initial data values
*      none

* --------------------- Executable code section ----------------------
      call push_routine (my_name)

      call Read_next_param_section 
     .   (Module_name, line, g_read_flag, g_current_section)

      ! reset flag so that next call to read_next_param_line will be
      ! a normal call.

      g_read_flag = 0

      if (Line .eq. Blank) then
         EOF_flag = 1

      else
         EOF_flag = 0
         g_lines_been_read = .true.
      endif

      ! Echo all lines to summary file
      
      call Write_string(LU_Summary_file, Line)

      call pop_routine (my_name)
      return
      end
      
* ====================================================================
       subroutine Manager_new_local_variable(Variable_name,
     .                                       Variable_value)
* ====================================================================

*   Short description:
*     Add a new local variable to list.

*   Assumptions:
*      None

*   Notes:
*     None

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*     DPH 9/02/95
*     jngh 24/2/95 put in calls to assign string

*   Calls:
*      assign_string

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      character Variable_name*(*)      ! (INPUT) Variable name to store
      character Variable_value*(*)     ! (INPUT) Variable value to store

*   Global variables
      include 'const.inc'              ! constant definitions
      include 'manager.inc'            ! manager common block

*   Internal variables
      character Str*300                ! Dummy value returned by APSIM

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      g_num_local_variables = g_num_local_variables + 1

      if (g_num_local_variables .gt. Max_local_variables) then
         write (str, '(50a)' )
     .      'Too many local variables have been specified in ',
     .      new_line,
     .      'manager file.'
         call Fatal_error(ERR_user, str)

      else
         call assign_string (
     :        g_local_variable_names(g_num_local_variables)
     :      , Variable_name)
         call assign_string (
     :        g_local_variable_values(g_num_local_variables)
     :      , Variable_value)
      endif

      return
      end
      
* ====================================================================
       subroutine manager_get_params (Function_call, Params)
* ====================================================================

*   Short description:
*     This routine returns the parameters from the specified function
*     call.  Return blank string on error.

*   Assumptions:
*      assumes no more than 2 parameters

*   Notes:
*     if function_call = 'date_between (1/8, 1/9)
*     then params(1) = 1/8
*          params(2) = 1/9

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*     DPH 4/9/96

*   Calls:
*      assign_string

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      character     Function_call*(*)  ! (INPUT) function call
      character     Params(2)*(*)      ! (OUTPUT) params from function call

*   Global variables
      include 'const.inc'              ! constant definitions
      include 'manager.inc'            ! manager common block

*   Internal variables
      integer pos_open_bracket
      integer pos_close_bracket
      integer pos_comma

*   Constant values
*      none

*   Initial data values

* --------------------- Executable code section ----------------------

      ! locate open and close bracket.
      
      pos_open_bracket = index (Function_call, '(')
      pos_close_bracket = index (Function_call, ')')
      
      ! did we find both an open and a close bracket?
      
      if (pos_open_bracket .gt. 0 .and.
     .    pos_close_bracket .gt. pos_open_bracket) then
     
      
         ! yes - locate position of comma.
      
         pos_comma = index (Function_call, ',')

         ! did we find a comma between the brackets?
      
         if (pos_comma .gt. pos_open_bracket .and. 
     .       pos_comma .lt. pos_close_bracket) then
            ! yes - 2 params
         
            Params(1) = Function_call (pos_open_bracket + 1: 
     .                                 pos_comma - 1)
            Params(2) = Function_call(pos_comma + 1: 
     .                                 pos_close_bracket - 1)
         
         else
            ! no - 1 param
      
            Params(1) = Function_call(pos_open_bracket + 1:
     .                                pos_close_bracket - 1)
            Params(2) = Blank
         endif      
         
      else
         ! no - error
         
         Params(1) = Blank
         Params(2) = Blank
      endif
      
      return
      end
      
* ====================================================================
       subroutine Parse_get_variable (Variable_Name, Variable_Value)
* ====================================================================

*   Short description:
*     The parse routine has requested a variable.  Return value to it.

*   Assumptions:
*      None

*   Notes:
*     None

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*     DPH 5/12/94
*      jngh 24/2/95 put in calls to assign string
*     dph 25/7/96  added code to put a message in summary file when creating
*                  a new local variable
*     dph 4/9/96   added function 'date_within'

*   Calls:
*      assign_string

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      character     Variable_Name*(*)  ! (INPUT) name of variable
      character     Variable_Value*(*) ! (OUTPUT) return value of variable

*   Global variables
      include 'const.inc'              ! constant definitions
      include 'manager.inc'           ! manager common block
      integer Find_string_in_array     ! function
      double precision Date            ! function
      integer lastNb                   ! function
      logical Date_between             ! function

*   Internal variables
      logical Is_apsim_variable        ! Is the requested variable APSIM's?
      integer Numvals                  ! Number of values found.
      integer Variable_index           ! Index into local variable array
      character Mod_name*100           ! name of module owning variable
      character Var_name*100           ! name of variable
      character Str*300                ! Dummy value returned by APSIM
      character Params(2)*(50)         ! params from function call

*   Constant values
*      none

*   Initial data values

* --------------------- Executable code section ----------------------

      ! Look for function first.
      
      if (Variable_name(1:5) .eq. 'date(') then
         call Manager_get_params (Variable_name, Params)
         call Double_var_to_string (Date(Params(1)), Variable_value)
     
      else if (Variable_name(1:12) .eq. 'date_within(') then
         ! get parameters from string.
         
         call Manager_get_params (Variable_name, Params)
         
         if (Date_between(Params(1), Params(2))) then
            Variable_value = '1'
         else
            Variable_value = '0'
         endif

      else
         Is_apsim_variable = (index(Variable_name, '.') .gt. 0)
   
   
         if (Is_apsim_variable) then
            call Split_line(Variable_name, Mod_name, Var_name, '.')
            call Get_char_var
     .           (Mod_name, Var_name, '()',
     .            Variable_value, Numvals)
   
         else
   
            ! Try to find variable in local variable list.
   
            Variable_index = Find_string_in_array
     .         (Variable_name, g_local_variable_names,
     .          g_num_local_variables)
   
            ! If not in local variable list then ask APSIM for it.
   
            if (Variable_index .le. 0) then
               call Get_char_var_optional
     .              (Unknown_module, Variable_name, '()',
     .               Variable_value, Numvals)
   
               ! If not found anywhere in APSIM then it must be a local
               ! variable not already defined.  Add variable to list.

               if (Numvals .eq. 0) then
                  call manager_new_local_variable(Variable_name, '0')
                  Variable_value = '0'
                  write (str, '(4a)' )
     .              'Manager creating a new local variable : ',
     .               Variable_name(1:Lastnb(Variable_name)),
     .               ' = 0'
                  call Report_event (str)
   
               else
                  ! Found variable elsewhere in APSIM
               endif
   
            else
               call assign_string (Variable_value
     .                      , g_local_variable_values(Variable_index))
   
            endif
         endif
  
      endif
         
      return
      end
* ====================================================================
       subroutine Parse_set_variable (Variable_Name, Variable_Value)
* ====================================================================

*   Short description:
*     The parsing routine has requested a set variable

*   Assumptions:
*      None

*   Notes:
*     None

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*     DPH 15/12/94
*      jngh 24/2/95 put in calls to assign string
*      jngh 07/06/96 changed set_ to post_
*     dph 12/7/96  added code to display line in summary file when setting apsim variable
*     dph 25/7/96  added message to summary file when creating a local variable

*   Calls:
*      assign_string

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      character     Variable_Name*(*)  ! (INPUT) name of variable
      character     Variable_Value*(*) ! (INPUT) value of variable to set

*   Global variables
      include 'const.inc'              ! constant definitions
      include 'manager.inc'           ! manager common block
      character Lower_case*(Function_string_len)
                                       ! function
      integer Find_string_in_array     ! function
      integer Lastnb                   ! function

*   Internal variables
      logical Is_apsim_variable        ! Is the requested variable APSIM's?
      integer numvals                  ! number of values returned.
      character Str*300                ! Dummy value returned by APSIM
      integer Variable_index           ! Index into local variable array
      character Mod_name*100           ! name of module owning variable
      character Var_name*100           ! name of variable

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      ! Convert variable name to lowercase

      Variable_name = Lower_case(Variable_name)

      Is_apsim_variable = (index(Variable_name, '.') .gt. 0)

      if (Is_apsim_variable) then
         call Split_line(Variable_name, Mod_name, Var_name, '.')
         call post_char_var
     .        (Mod_name, Var_name,
     .         Variable_value)

      else
         ! Try to find variable in local variable list.

         Variable_index = Find_string_in_array
     .      (Variable_name, g_local_variable_names,
     :       g_num_local_variables)

         ! If not in local variable list then ask APSIM for it.  If
         ! APSIM doesn't know about it then add to local variable list.

         if (Variable_index .le. 0) then
            call Get_char_var_optional
     .          (Unknown_module, Variable_name, '()',
     .           Str, Numvals)
            if (Numvals .eq. 0) then
               ! Add variable to local variable list.

               call manager_new_local_variable(Variable_name, 
     .              Variable_value)

               write (str, '(4a)' )
     .           'Manager creating a new local variable : ',
     .            Variable_name(1:Lastnb(Variable_name)),
     .            ' = ',
     .            Variable_value(1:lastnb(Variable_value))
               call Report_event (str)

            else
               call post_char_var(Unknown_module, Variable_name,
     .            Variable_value)
               Is_apsim_variable = .true.
            endif
         else
            call assign_string (
     :           g_local_variable_values(Variable_index)
     :         , Variable_value)

         endif
      endif
      
      if (Is_apsim_variable) then
         write (str, '(4a)' )
     .      'Manager setting apsim variable : ',
     .      Variable_name(1:Lastnb(Variable_name)),
     .      ' = ',
     .      Variable_value(1:lastnb(Variable_value))
      
         call Report_event (str)
      endif

      return
      end
* ====================================================================
       subroutine Parse_action (Action_string)
* ====================================================================

*   Short description:
*     The parsing routine has requested some action of APSIM.

*   Assumptions:
*      None

*   Notes:
*     None

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*     DPH 15/12/94
*     jngh 24/2/95 changed data to data_string
*     DPH 5/7/95   put in check for a set command without an equals sign
*                  on it.  Produce fatal_error if this happens.
*     DPH 19/7/95  Added code to check for a queue keyword
*     DPH 27/5/96  Added code to check for a set action and to pass the
*                  variable name as data string.
*     dph 12/7/96  Added call to no_leading_spaces (Action) - fixes bug in manager
*     dph 25/7/96  Changed decl of no_leading_spaces*(mes_action_size) to
*                  no_leading_spaces*(function_string_len)

*   Calls:
*     None

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      character Action_string*(*)      ! (INPUT) ACtion to perform.

*   Global variables
      include 'const.inc'              ! constant definitions
      logical Store_in_postbox         ! function
      character No_leading_spaces*(Function_string_len)
                                       ! function

*   Internal variables
      integer Day                      ! Day number of year
      character Module_name*(30)       ! Module name to send action to
      character Action*(MES_Action_size)
                                       ! Action to send to APSIM
      character Data_string*(Function_string_len) 
                                       ! Data string to send to APSIM
      character Variable_name*(Max_variable_name_size)
                                       ! variable name in set actions.
      integer Numvals                  ! Number of values returned
      integer Year                     ! Year number
      character Err*200                ! Error message
      logical Data_was_stored          ! Was data stored in postbox?

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      if (index(Action_string, 'do_output') .eq. 0 .and.
     .    index(Action_string, 'do_end_day_output') .eq. 0) then
         call Get_integer_var
     .       (Unknown_module, 'day', '()', Day, numvals, 1, 366)
         call Get_integer_var
     .      (Unknown_module, 'year', '()', year, numvals, 1700, 2100)

         write (Data_string, '(a,i3,a,i4,2a)' )
     .      ' Day= ', Day, ' Year =  ', Year, 
     .      '     Manager sending message :- ', Action_string
      
         call Write_string(LU_Summary_file, Data_string)
      endif
      
      call split_line (Action_string, Module_name, Data_string, Blank)
      Data_string = No_leading_spaces(Data_string)
      call split_line (Data_string, Action, Data_string, Blank)
      Action = No_leading_spaces(Action)
      
      ! Test for case where user has forgotten to put in equals sign in set command.
      
      if (Action .eq. 'set') then
         if (index(Data_string, '=') .eq. 0) then
            write (Err, '(50a)' )
     .         'Your manager file has a set command that does not have',
     .         new_line,
     .         'have a equals sign in it.  Please correct problem.'
            call Fatal_error(ERR_user, Err)
         endif
      endif
      
      ! Add code to check for a keyword of QUEUE.

      if (Module_name .eq. 'queue') then
         Module_name = Action
         call split_line (Data_string, Action, Data_string, Blank)
         call Message_send (Module_name, Action, Data_string)
         
      else
         call New_postbox ()
         Data_was_stored = Store_in_postbox (Data_string)
         if (Action .eq. 'set') then
            call Get_next_variable (Data_string,
     .                              Variable_name,
     .                              Data_string)

            Data_string = Variable_name
            
         else if (Data_was_stored) then
            Data_string = Blank
                        
         endif
         
         call message_send_immediate (Module_name, Action, Data_string)
         call Delete_postbox ()
      endif

      return
      end
* ====================================================================
       subroutine Parse_error (Error_message, Routine_message)
* ====================================================================

*   Short description:
*     The parsing routine has encountered an error.

*   Assumptions:
*      None

*   Notes:
*     None

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*     DPH 15/12/94
*     DPH 11/4/96  Added code to display a line number and file name
*                  when an error occured - commented it out - doesn't work
*                  because the parsing routine tokenises all manager rules
*                  from all files before validating the rules.

*   Calls:
*      Fatal_error
*      pop_routine
*      push_routine

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      character Error_message*(*)      ! (INPUT) Error message to display
      character Routine_message*(*)    ! (INPUT) Routine name to display

*   Global variables
      include 'const.inc'              ! constant definitions
      include 'parse.inc'              ! parsing common block
!      include 'utility.inc'            ! needed for current line number and file
                                       ! unit number for error messages.

*   Internal variables
!      character File_name*200          ! name of manager file.
!      character Our_error*(Function_string_len)
                                       ! our error message

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      call Push_routine(Routine_message)

!      inquire (unit=Current_unit_num, name=File_name)
!      write (Our_error, '(6a, i3)')      
!     .   Error_message,
!     .   New_line,
!     .   'Manager_file = ', File_name,
!     .   New_line,
!     .   'Line number  = ', Current_record_num
      call Fatal_error(ERR_user, Error_message)

      g_All_OK = NO

      call Pop_routine(Routine_message)

      return
      end
* =====================================================================
       subroutine Parse (Token_array, Token_array2)
* =====================================================================

*   Short description:
*     Parse a given array and perform the specified actions.

*   Assumptions:
*      None

*   Notes:

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*      TM - 21/11/94

*   Calls:
*      Get_next_token
*      Assignment_statement
*      Process_if_statement
*      Process_else_statement

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
       character     Token_array(*)*(*)
       integer       Token_array2(*)

*   Global variables
       include 'parse.inc'                  ! Manager common block

*   Internal variables
       integer       Nested_ifs           ! Number of nested statements

*   Constant values
*      none

*   Initial data values
*      none
* --------------------- Executable code section ----------------------

       Nested_ifs = 0
       g_end_of_file = NO
       g_next_token = g_start_token - 1
       if (g_next_token .lt. 0) then
          g_next_token = 0
       endif

10     continue

       if (g_end_of_file .eq. NO) then
          call   Get_next_token(Token_array, Token_array2)

          if     (g_token .eq. C_WORD) then
                 call Assignment_Statement(Token_array,Token_array2)

          elseif (g_token .eq. C_ACTION) then
                 call   Process_Action(Token_array, Token_array2)

          elseif (g_token .eq. C_IF) then
                 Nested_ifs = Nested_ifs + 1
                 call   Process_if_statement(Nested_ifs,Token_array,
     .                                                Token_array2)

          elseif (g_token .eq. C_ENDIF) then
                 Nested_ifs = Nested_ifs - 1

          elseif (g_token .eq. C_ELSE) then
                 call   Process_else_statement(Nested_ifs,
     .                                 Token_array, Token_array2)

          endif

          goto 10
       endif


       if (Nested_ifs .gt. 0) then
          call   Parse_error('Missing endif       ',
     .                              'Parse               ')
       endif


       return
       end
* =====================================================================
       subroutine Process_if_statement (Nested_ifs, Token_array,
     .                                                   Token_array2)
* =====================================================================

*   Short description:
*     Process a single if statement.

*   Assumptions:
*      None

*   Notes:

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*      TM - 21/11/94

*   Calls:
*      Get_next_token
*      Parse_error

* ----------------------- Declaration section ------------------------

       implicit none

*   Global variables
       include 'parse.inc'                ! Manager common block
       integer       If_statement         ! function

*   Subroutine arguments
       integer       Nested_ifs           ! Number of nested statements
       character     Token_array(*)*(*)
       integer       Token_array2(*)

*   Internal variables
       integer       This_Nested          ! Number of this nested if

*   Constant values
*      none

*   Initial data values
       integer       Last_Token           ! g_last g_token read


* --------------------- Executable code section ----------------------

       call   Get_next_token(Token_array, Token_array2)

       if (If_statement(Token_array, Token_array2) .eq. 0) then
          if (g_all_ok .eq. YES) then
             This_Nested = Nested_ifs
             if (g_token .ne. C_THEN) then
                call   Parse_error('Missing then        ',
     .                             'Process_if_statement')
             endif
          endif

10        continue
          if (g_all_ok .eq. YES) then

             Last_Token = g_token
             call   Get_next_token(Token_array, Token_array2)

             if (g_token .eq. C_IF .and.
     .           Last_Token .eq. C_EOL) then
     
                Nested_ifs = Nested_ifs + 1

                goto 10
                
             elseif (g_token .eq. C_ELSE .and.
     .              Last_Token .eq. C_EOL) then
                 if (Nested_ifs .ne. This_Nested) then
                    goto 10
                 endif

             elseif (g_token .eq. C_ENDIF .and.
     .            Last_Token .eq. C_EOL) then
                 if (Nested_ifs .gt. This_Nested) then
                    Nested_ifs = Nested_ifs - 1
                    goto 10
                 endif
                 Nested_ifs = Nested_ifs - 1

             elseif (g_token .eq. C_EOF) then
                 if (Nested_ifs .gt. This_Nested) then
                 call   Parse_error('Missing endif       ',
     .                              'Process_if_statement')

                 endif

             else
                goto 10
             endif
          endif
       else
          if (g_all_ok .eq. YES) then
             if (g_token .ne. C_THEN) then
                 call   Parse_error('Missing then        ',
     .                              'Process_if_statement')
             endif
          endif
       endif


       return
       end
* =====================================================================
       subroutine Process_else_statement (Nested_ifs, Token_array,
     .                                                   Token_array2)
* =====================================================================

*   Short description:
*     Process the else part of an if-statement.
*   Assumptions:
*      None

*   Notes:

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*      TM - 21/11/94

*   Calls:
*      Get_next_token

* ----------------------- Declaration section ------------------------

       implicit none

*   Global variables
       include 'parse.inc'                  ! Manager common block

*   Subroutine arguments
       integer       Nested_ifs           ! Number of nested statements
       character     Token_array(*)*(*)
       integer       Token_array2(*)

*   Internal variables
       integer       This_Nested          ! Number of this nested if

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

       This_Nested = Nested_ifs
10     continue

       call   Get_next_token(Token_array, Token_array2)

       if     (g_token .eq. C_IF) then
              Nested_ifs = Nested_ifs + 1
              goto 10

       elseif (g_token .eq. C_ENDIF) then
              if (Nested_ifs .gt. This_Nested) then
                     Nested_ifs = Nested_ifs - 1
                     goto 10
              endif
       else

              goto 10
       endif

       Nested_ifs = Nested_ifs - 1

       return
       end
* =====================================================================
       subroutine Assignment_Statement (Token_array, Token_array2)
* =====================================================================

*   Short description:
*     Perform a given assignment.

*   Assumptions:
*      None

*   Notes:

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*      TM - 21/11/94
*      TM - 29/09/95  Took out call to Action (processed in Parse)

*   Calls:
*      Get_next_token
*      Process_Assignment
*      Parse_error

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
       character     Token_array(*)*(*)
       integer       Token_array2(*)

*   Global variables
       include 'parse.inc'                  ! Manager common block

*   Internal variables
       character     Variable_name*(Buffer_size)
                                          ! Variable to assign a value

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

       Variable_name = g_buffer

       call   Get_next_token(Token_array, Token_array2)

       if     (g_token .eq. C_EQUAL) then
              call   Process_Assignment(Variable_name, Token_array,
     .                                                  Token_array2)

       else
              call   Parse_error('Syntax error        ',
     .                           'Assignment_Statement')

       endif


       return
       end
* =====================================================================
       subroutine Process_Assignment (Variable_name, Token_array,
     .                                                    Token_array2)
* =====================================================================

*   Short description:
*     Assign a value to a variable.

*   Assumptions:
*      None

*   Notes:

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*      TM - 21/11/94
*      jngh 24/2/95 put in calls to assign string

*   Calls:
*      assign_string
*      Get_next_token
*      Process_expression
*      Parse_set_variable

* ----------------------- Declaration section ------------------------

       implicit none

*   Global variables
       include 'parse.inc'                  ! Manager common block

*   Subroutine arguments
       character     Variable_name*(Buffer_size)
                                          ! Variable to assign a value
       character     Token_array(*)*(*)
       integer       Token_array2(*)

*   Internal variables
       character     Variable_value*(Buffer_size)
                                          ! value to assign the variable

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

       call   Get_next_token(Token_array, Token_array2)
       g_number_expressions = 1
       call assign_string (g_expression_array(g_number_expressions)
     :                   , g_buffer)
       g_expression_array2(g_number_expressions) = g_token
       call   Get_next_token (Token_array, Token_array2)

10     continue
       if     (g_token .ne. C_EOL) then
              g_number_expressions = g_number_expressions + 1
              call assign_string 
     :            (g_expression_array(g_number_expressions), g_buffer)
              g_expression_array2(g_number_expressions) = g_token
              call   Get_next_token(Token_array, Token_array2)
              goto 10
       endif
       g_expression_array2(g_number_expressions+1) = C_END

       call   Process_expression

       if (g_all_ok .eq. YES) then
              call assign_string (Variable_value, g_expression_result)              
              call   Parse_set_variable(Variable_Name, Variable_Value)
       endif

       return
       end
* =====================================================================
       subroutine Process_Action (Token_array, Token_array2)
* =====================================================================

*   Short description:
*     Perform a given action.

*   Assumptions:
*      None

*   Notes:

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*      TM - 21/11/94
*      TM - 29/09/95 - changed action to be handled as one token

*   Calls:
*      Get_next_token
*      Parse_action

* ----------------------- Declaration section ------------------------

       implicit none

*   Global variables
       include 'const.inc'
       include 'parse.inc'

*   Subroutine arguments
       character     Token_array(*)*(*)
       integer       Token_array2(*)

*   Internal variables

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

       call   Parse_action (g_buffer)

       call   Get_next_token (Token_array, Token_array2)


       return
       end
* =====================================================================
       integer function If_statement(Token_array, Token_array2)
* =====================================================================

*   Short description:
*     Calculate the expression in an if statement.

*   Assumptions:
*      None

*   Notes:
*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*      TM - 21/11/94

*   Calls:
*      Get_expression_array
*      Process_next_expression
*      Process_And_Or_expression
* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
       character     Token_array(*)*(*)
       integer       Token_array2(*)

*   Global variables
       include 'parse.inc'                  ! Manager common block

*   Internal variables
       integer       If_result
       integer       NumVals

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

       g_number_and_or        = 0
       g_number_expressions   = 0
       g_word_or_number       = NO


10     continue
       call   Get_expression_array(Token_array, Token_array2)


20     continue

       if (g_all_ok .eq. YES) then
              call   Process_expression
       endif

       if (g_all_ok .eq. YES) then

              if     (g_save_token .ne. C_THEN) then
                     call   Process_next_expression(Token_array,
     .                                                Token_array2)
                     goto 10
              endif

              if     (g_number_and_or .gt. 0) then
                     call   Process_And_Or_expression
                     goto 20
              endif

              g_token = g_save_token

              call string_to_integer_var (g_expression_result,
     :                                   If_result, NumVals)

              If_statement = If_result
       endif


       return
       end

* =====================================================================
       subroutine Process_next_expression (Token_array, Token_array2)
* =====================================================================

*   Short description:
*     Process the next part of an expression.

*   Assumptions:
*      None

*   Notes:

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*      TM - 21/11/94

*   Calls:
*      Get_next_token

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
       character     Token_array(*)*(*)
       integer       Token_array2(*)

*   Global variables
       include 'parse.inc'                  ! Manager common block

*   Internal variables
       integer       ind                  ! loop index

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

       g_number_expressions = g_number_expressions + 1

       g_expression_array2(g_number_expressions) = g_save_token

       if (g_save_token .eq. C_AND .or. 
     :     g_save_token .eq. C_OR) then

          do 10  ind = g_number_and_or + 1
     .               , g_number_and_or + g_number_expressions
             call assign_string (g_and_or_array(ind)
     .                   , g_expression_array(ind - g_number_and_or))
             g_and_or_array2(ind) =
     .                     g_expression_array2(ind - g_number_and_or)
10        continue

          g_number_and_or = g_number_and_or + g_number_expressions
          g_number_expressions = 0
       endif

       g_save_token = 0
       call   Get_next_token(Token_array, Token_array2)


       return
       end
* =====================================================================
       subroutine Process_And_Or_expression ()
* =====================================================================

*   Short description:
*     Process the AND/C_OR part of an expression.

*   Assumptions:
*      None

*   Notes:

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*      TM - 21/11/94

*   Calls:
*

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
*      none

*   Global variables
       include 'parse.inc'                  ! Manager common block

*   Internal variables
       integer       ind                  ! loop index
*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      do 10  ind = g_number_and_or + 1
     :           , g_number_and_or + g_number_expressions
         call assign_string (g_and_or_array(ind)
     :                     , g_expression_array(ind-g_number_and_or))
         g_and_or_array2(ind)=g_expression_array2(ind-g_number_and_or)
10    continue

       g_number_and_or = g_number_and_or + g_number_expressions

       do 20  ind = 1, g_number_and_or
          call assign_string (g_expression_array(ind)
     :                      , g_and_or_array(ind))
          g_expression_array2(ind) = g_and_or_array2(ind)
20     continue

       g_expression_array2(g_number_and_or+2) = C_END
       g_number_expressions = g_number_and_or
       g_number_and_or = 0


       return
       end
* =====================================================================
       subroutine Process_expression ()
* =====================================================================

*   Short description:
*     Process the calculations in the given expression.

*   Assumptions:
*      None

*   Notes:

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*      TM - 21/11/94
*      jngh 24/2/95 put in calls to assign string

*   Calls:
*      assign_string
*      Process_sub_expression

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
*      none

*   Global variables
       include 'parse.inc'                  ! Manager common block
       character     pop_stack*(Buffer_size)
                                          ! function

*   Internal variables
       integer       ind                  ! loop index
       integer       ind2                 ! loop index
       integer       left                 ! position of the left parent
       integer       right                ! position of the right parent

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

20     continue
       left  = 0
       right = 0

       do 30  ind = 1, g_number_expressions
          if (right .eq. 0) then
             if (g_expression_array2(ind) .eq. C_LEFT_PAREN) then
                left = ind
             elseif (g_expression_array2(ind) .eq. C_RIGHT_PAREN) then
                right = ind
             endif
          endif
30     continue


       if (left .gt. 0 .and. right .gt. 0) then
          g_number_of_tokens = right - left - 1
          do 40  ind = 1, g_number_of_tokens
             call assign_string (g_expression_sub_array(ind)
     :                         , g_expression_array(ind+left))
             g_expression_sub_array2(ind) =
     :                                 g_expression_array2(ind+left)
40        continue
          g_expression_sub_array2(g_number_of_tokens+1) = C_END


          call assign_string (g_buffer, g_expression_sub_array(1))
          g_token = g_expression_sub_array2(1)
          g_current_token = 1

          call Process_sub_expression()

          if (g_all_ok .eq. YES) then
             if (left .eq. 0) then
                left = 1
             endif

             g_expression_result = pop_stack()

          end if

          if (g_all_ok .eq. YES) then
              call assign_string (g_expression_array(left)
     :                         ,  g_expression_result)
              g_expression_array2(left) = C_NUMBER

              ind2 = 0
              do 50  ind = right+1, g_number_expressions
                 ind2 = ind2 + 1
                 call assign_string (g_expression_array(left+ind2)
     .                             , g_expression_array(ind))
                 g_expression_array2(left+ind2) =
     .                            g_expression_array2(ind)
50           continue
             g_number_expressions =
     .              g_number_expressions - (right - left)
             goto 20
          endif
       else

          g_number_of_tokens = g_number_expressions - left

          do 60  ind = 1, g_number_of_tokens
             call assign_string (g_expression_sub_array(ind)
     :                         , g_expression_array(ind+left))
             g_expression_sub_array2(ind) =
     :                             g_expression_array2(ind+left)
60        continue
          g_expression_sub_array2(g_number_of_tokens+1) = C_END

          call assign_string (g_buffer, g_expression_sub_array(1))
          g_token = g_expression_sub_array2(1)
          g_current_token = 1

          call Process_sub_expression()

          if (g_all_ok .eq. YES) then
             call assign_string (g_expression_result, pop_stack())
          endif

          if (g_all_ok .eq. YES) then
             do 70  ind = 1, left
                g_expression_array2(ind) = C_LEFT_PAREN
70           continue

             call assign_string (g_expression_array(left+1)
     :                         , g_expression_result)
             g_expression_array2(left+1) = C_NUMBER


             if (right .gt. 0) then
                do 80  ind = 1, g_number_expressions+1-right
                      g_expression_array2(ind+1) = C_RIGHT_PAREN
80              continue

                g_number_expressions = g_number_expressions + 2-right
             else
                g_number_expressions = left + 1
             endif

             g_expression_array2(g_number_expressions+1) = C_END
          endif
       endif

       return
       end

* =====================================================================
       subroutine Str_to_double_var(String, Double_value, io_result)
* =====================================================================

*   Short description:
*     Convert a string value to a double number.  

*   Assumptions:
*      None

*   Notes:
*     We created this routine because we don't want an error message when
*     the string cannot be converted to a real number.

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*     DPH 1/8/95
*     dph 24/6/96 Changed routine from a real routine to a double routine

*   Calls:

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      character String*(*)             ! (INPUT) String to convert
      double precision Double_value    ! (OUTPUT) Value of string
      integer IO_result                ! (OUTPUT) io_result of internal read.

*   Global variables

*   Internal variables

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      read (String, '(g25.0)',iostat = io_result) Double_value

      return
      end
      
* =====================================================================
       subroutine Process_sub_expression ()
* =====================================================================

*   Short description:
*     Process the comparing part of an expression.

*   Assumptions:
*      None

*   Notes:

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*      TM - 21/11/94
*      jngh 24/2/95 put in calls to assign string and reals_are_equal
*      dph 24/6/96  changed from using reals to double precision for temps

*   Calls:
*      assign_string
*      Process_Simple_Expression
*      Get_sub_token
*      push_stack
*      reals_are_equal

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
*      none

*   Global variables
       include 'parse.inc'                  ! Manager common block
       character     pop_stack*(Buffer_size)
                                          ! function
       logical       doubles_are_equal    ! function

*   Internal variables
       integer       operator             ! save the operator
       character     operand_1*(Buffer_size)
       character     operand_2*(Buffer_size)
       double precision Temp_1, Temp_2
       integer       io_result1, io_result2
                                          ! check for reals

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

       call   Process_Simple_Expression

       if (g_token .eq. C_EQUAL         .or. 
     :     g_token .eq. C_LESS_THAN     .or.
     .     g_token .eq. C_LESS_EQUAL    .or. 
     :     g_token .eq. C_GREATER_THAN  .or.
     .     g_token .eq. C_GREATER_EQUAL .or. 
     :     g_token .eq. C_NOT_EQUAL)    then

          operator = g_token

          call   Get_sub_token
          call   Process_Simple_Expression

          call assign_string (operand_2, pop_stack())
          call assign_string (operand_1, pop_stack())

          call Str_to_double_var(Operand_1, Temp_1, io_result1)
          call Str_to_double_var(Operand_2, Temp_2, io_result2)

          if (io_result1 .eq. 0 .and. io_result2 .eq. 0) then
            if (g_all_ok .eq. YES) then
                if (operator .eq. C_EQUAL) then
                    if (doubles_are_equal(temp_1, temp_2)) then
                        call   push_stack('1.0')
                     else
                        call   push_stack('0.0')
                    endif

                elseif (operator .eq. C_LESS_THAN) then
                    if (temp_1 .lt. temp_2) then
                        call   push_stack('1.0')
                    else
                        call   push_stack('0.0')
                    endif

                elseif (operator .eq. C_LESS_EQUAL) then
                    if (temp_1 .le. temp_2) then
                        call   push_stack('1.0')
                    else
                        call   push_stack('0.0')
                    endif

                elseif (operator .eq. C_GREATER_THAN) then
                    if (temp_1 .gt. temp_2) then
                        call   push_stack('1.0')
                    else
                        call   push_stack('0.0')
                    endif

                elseif (operator .eq. C_GREATER_EQUAL) then
                    if (temp_1 .ge. temp_2) then
                        call   push_stack('1.0')
                    else
                        call   push_stack('0.0')
                    endif

                elseif (operator .eq. C_NOT_EQUAL) then
                    if (doubles_are_equal (temp_1, temp_2)) then
                        call   push_stack('0.0')
                    else
                        call   push_stack('1.0')
                    endif
                endif
            endif
          else
            if (g_all_ok .eq. YES) then
                if (operator .eq. C_EQUAL) then
                    if (Operand_1 .eq. Operand_2) then
                        call   push_stack('1.0')
                     else
                        call   push_stack('0.0')
                    endif

                elseif (operator .eq. C_LESS_THAN) then
                    if (operand_1 .lt. operand_2) then
                        call   push_stack('1.0')
                    else
                        call   push_stack('0.0')
                    endif

                elseif (operator .eq. C_LESS_EQUAL) then
                    if (operand_1 .le. operand_2) then
                        call   push_stack('1.0')
                    else
                        call   push_stack('0.0')
                    endif

                elseif (operator .eq. C_GREATER_THAN) then
                    if (operand_1 .gt. operand_2) then
                        call   push_stack('1.0')
                    else
                        call   push_stack('0.0')
                    endif

                elseif (operator .eq. C_GREATER_EQUAL) then
                    if (operand_1 .ge. operand_2) then
                        call   push_stack('1.0')
                    else
                        call   push_stack('0.0')
                    endif

                elseif (operator .eq. C_NOT_EQUAL) then
                    if (operand_1 .ne. operand_2) then
                        call   push_stack('1.0')
                    else
                        call   push_stack('0.0')
                    endif
                endif
            endif
         endif
       endif


       return
       end
* =====================================================================
       subroutine Process_Simple_Expression ()
* =====================================================================

*   Short description:
*     Process the add/minus/and part of an expression.

*   Assumptions:
*      None

*   Notes:

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*      TM - 21/11/94
*      jngh 24/2/95 put in calls to assign string and reals_are_equal
*      dph  24/6/96 changed data type to doubles for all calculations.

*   Calls:
*      assign_string
*      Get_sub_token
*      Process_Term
*      push_stack
*      reals_are_equal

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
*      none

*   Global variables
       include 'parse.inc'                  ! Manager common block
       character     pop_stack*(Buffer_size)
                                          ! function
       logical       doubles_are_equal    ! function

*   Internal variables
       integer       operator             ! save the operator
       character     Temp_operand*(Buffer_size)
       double precision  Temp_1, Temp_2
       character     operand_1*(Buffer_size)
       character     operand_2*(Buffer_size)
       integer       numvals


*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

       call   Process_Term

10     continue
       if (g_token .eq. C_PLUS  .or. 
     :     g_token .eq. C_MINUS .or.
     .     g_token .eq. C_AND)  then

          operator = g_token

          call  Get_sub_token
          call  Process_Term

          call assign_string (operand_2, pop_stack())
          call assign_string (operand_1, pop_stack())

          if (g_all_ok .eq. YES) then
             if (operator .eq. C_PLUS) then
                call String_to_double_var(Operand_1, Temp_1, numvals)
                call String_to_double_var(Operand_2, Temp_2, numvals)
                call Double_var_to_string(Temp_1 + Temp_2, Temp_operand)
                call   push_stack(Temp_operand)

             elseif (operator .eq. C_MINUS) then
                call String_to_double_var(Operand_1, Temp_1, numvals)
                call String_to_double_var(Operand_2, Temp_2, numvals)
                call Double_var_to_string(Temp_1 - Temp_2, Temp_operand)
                call push_stack(Temp_operand)

             elseif (operator .eq. C_AND) then
                call String_to_double_var(Operand_1, Temp_1, numvals)
                call String_to_double_var(Operand_2, Temp_2, numvals)

                if (doubles_are_equal (Temp_1, 1.0d0) .and.
     .              doubles_are_equal (Temp_2, 1.0d0)) then
                   call push_stack('1.0')
                else
                   call push_stack('0.0')
                endif
             endif

             if (g_all_ok .eq. YES) then
                goto 10
             endif
          endif
       endif

       return
       end
* =====================================================================
       subroutine Process_Term ()
* =====================================================================

*   Short description:
*     Process the mult/div/power/or part of an expression.

*   Assumptions:
*      None

*   Notes:

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*      TM - 21/11/94
*      jngh 24/2/95 put in calls to assign string and reals_are_equal
*      dph  24/6/96 changed data type to doubles for all calculations.

*   Calls:
*      assign_string
*      Get_sub_token
*      Process_Factor
*      push_stack
*      reals_are_equal

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
*      none

*   Global variables       
       include 'parse.inc'                  ! Manager common block
       character     pop_stack*(Buffer_size)
                                          ! function
       logical       doubles_are_equal    ! function

*   Internal variables
       integer       operator             ! save the operator
       character     Temp_operand*(Buffer_size)
       double precision Temp_1, Temp_2
       character     operand_1*(Buffer_size)
       character     operand_2*(Buffer_size)
       integer       numvals

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

       call   Process_Factor


20     continue
       if (g_token .eq. C_MULTIPLY .or. 
     :     g_token .eq. C_DIVIDE   .or.
     .     g_token .eq. C_POWER    .or. 
     :     g_token .eq. C_OR)      then
           operator = g_token

          call  Get_sub_token
          call  Process_Factor

          call assign_string (operand_2, pop_stack())
          call assign_string (operand_1, pop_stack())


          if (g_all_ok .eq. YES) then
             if (operator .eq. C_MULTIPLY) then
                call String_to_double_var(Operand_1, Temp_1, numvals)
                call String_to_double_var(Operand_2, Temp_2, numvals)
                call Double_var_to_string(Temp_1 * Temp_2, Temp_operand)
                call   push_stack(Temp_operand)

             elseif (operator .eq. C_DIVIDE) then
                call String_to_double_var(Operand_1, Temp_1, numvals)
                call String_to_double_var(Operand_2, Temp_2, numvals)

                if (doubles_are_equal(Temp_2, 0.0d0)) then
                   call   Parse_error
     .                         ('Divide by zero      ',
     .                          'Process_term        ')
                else
                   call Double_var_to_string(Temp_1 / Temp_2, 
     .                                       Temp_operand)
                   call   push_stack(Temp_operand)
                endif

             elseif (operator .eq. C_POWER) then
                call String_to_double_var(Operand_1, Temp_1, numvals)
                call String_to_double_var(Operand_2, Temp_2, numvals)
                call Double_var_to_string(Temp_1 ** Temp_2, 
     .                                    Temp_operand)
                call push_stack(Temp_operand)

             elseif (operator .eq. C_OR) then
                call String_to_double_var(Operand_1, Temp_1, numvals)
                call String_to_double_var(Operand_2, Temp_2, numvals)
                if (Doubles_are_equal (Temp_1, 1.0d0) .or.
     .              Doubles_are_equal (Temp_2, 1.0d0)) then
                   call push_stack('1.0')
                else
                   call push_stack('0.0')
                endif
             endif

             if (g_all_ok .eq. YES) then
                goto 20
             endif
          endif
       endif


       return
       end
* =====================================================================
       subroutine Process_Factor ()
* =====================================================================

*   Short description:
*     Get the value to push on the stack.

*   Assumptions:
*      None

*   Notes:

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*      TM - 21/11/94

*   Calls:
*      Get_sub_token
*      Parse_get_variable
*      push_stack

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
*      none

*   Global variables
       include 'parse.inc'                  ! Manager common block
       character     Real_or_not*(Buffer_size)

*   Internal variables
       character     Variable_value*(Buffer_size)
                                          ! Value to push on g_stack
       character     Temp*(Buffer_size)

*   Constant values
*      none
*   Initial data values
*      none

* --------------------- Executable code section ----------------------

       if (g_token .eq. C_WORD) then
          call   Parse_get_variable(g_buffer, Variable_Value)

          call assign_string (Temp, Real_or_not(Variable_Value))

          call   push_stack(Temp)

          call   Get_sub_token

       elseif (g_token .eq. C_NUMBER) then

          call assign_string (Temp, Real_or_not(g_buffer))          
          call   push_stack(Temp)

          call   Get_sub_token

       elseif (g_token .eq. C_LITERAL) then
          call   push_stack(g_buffer)

          call   Get_sub_token

       endif


       return
       end
* =====================================================================
       subroutine push_stack (Variable_Value)
* =====================================================================

*   Short description:*     Add a value to the top of the stack.

*   Assumptions:
*      None

*   Notes:

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*      TM - 21/11/94
*      jngh 24/2/95 put in calls to assign string

*   Calls:
*      assign_string
*      Parse_error

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
       character     Variable_Value*(*) ! (INPUT) Value to push on g_stack

*   Global variables
       include 'parse.inc'             ! Manager common block

*   Internal variables
*      none

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

       g_number_of_variables = g_number_of_variables + 1
       if (g_number_of_variables .gt. Variable_maximum) then
          call   Parse_error('Too many variables  ',
     .                       'push_stack          ')
       else
          call assign_string (g_stack(g_number_of_variables)
     :                      , Variable_Value)
       endif


       return
       end
* =====================================================================
       character*(*) function pop_stack ()
* =====================================================================

*   Short description:
*     Get the string off the top of the stack.

*   Assumptions:
*      None

*   Notes:

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*      TM - 21/11/94
*      jngh 24/2/95 put in calls to assign string

*   Calls:
*      assign_string
*      Parse_error

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
*      none

*   Global variables
       include 'parse.inc'                  ! Manager common block

*   Internal variables
*      none

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

       g_number_of_variables = g_number_of_variables - 1
       if (g_number_of_variables .lt. 0) then
          call   Parse_error('Too few variables   ',
     .                       'pop_stack           ')
       else
          call assign_string (pop_stack
     :                      , g_stack(g_number_of_variables + 1))
       endif


       return
       end
* =====================================================================
       subroutine Get_sub_token ()
* =====================================================================

*   Short description:
*     Get the next token off the sub array.

*   Assumptions:
*      None

*   Notes:

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*      TM - 21/11/94
*      jngh 24/2/95 put in calls to assign string

*   Calls:
*      assign_string
*      Parse_error

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
*      none

*   Global variables
       include 'parse.inc'                  ! Manager common block

*   Internal variables
*      none

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

       g_current_token = g_current_token + 1
       if     (g_current_token .gt. g_number_of_tokens+1) then
              call   Parse_error('Too many tokens     ',
     .                           'Get_sub_token       ')
       endif

       call assign_string (g_buffer
     :                   , g_expression_sub_array(g_current_token))
       g_token = g_expression_sub_array2(g_current_token)


       return
       end
* =====================================================================
       subroutine Get_next_token (Token_array, Token_array2)
* =====================================================================

*   Short description:
*     Get the next token of the g_token array.

*   Assumptions:
*      None

*   Notes:

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*      TM - 21/11/94
*      jngh 24/2/95 put in calls to assign string

*   Calls:
*      assign_string


* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
       character     Token_array(*)*(*)
       integer       Token_array2(*)

*   Global variables
       include 'parse.inc'                  ! Manager common block

*   Internal variables
*      none

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

       g_next_token = g_next_token + 1

       call assign_string (g_buffer, Token_array(g_next_token))
       g_token = Token_array2(g_next_token)

       if     (g_token .eq. C_EOF) then
              g_end_of_file = YES
       endif


       return
       end
* =====================================================================
       subroutine Get_expression_array (Token_array, Token_array2)
* =====================================================================

*   Short description:
*     Put all tokens in expression into expression array.

*   Assumptions:
*      None

*   Notes:

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*      TM - 21/11/94
*      jngh 24/2/95 put in calls to assign string

*   Calls:
*      assign_string
*      Get_next_token
*      Check_previous_word

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
       character     Token_array(*)*(*)
       integer       Token_array2(*)

*   Global variables
       include 'parse.inc'                  ! Manager common block

*   Internal variables
*      none

*   Constant values*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

       if (g_token .ne. C_EOL) then
          call   Check_previous_word

          g_number_expressions = g_number_expressions + 1
          call assign_string (g_expression_array(g_number_expressions)
     :                      , g_buffer)
          g_expression_array2(g_number_expressions) = g_token
          call   Get_next_token(Token_array, Token_array2)
       endif

10     continue
       if (g_all_ok .eq. YES) then
          if (g_token .eq. C_WORD        .or. 
     :        g_token .eq. C_NUMBER      .or.
     .        g_token .eq. C_PLUS        .or. 
     :        g_token .eq. C_MINUS       .or.
     .        g_token .eq. C_MULTIPLY    .or. 
     :        g_token .eq. C_DIVIDE      .or.
     .        g_token .eq. C_POWER       .or. 
     :        g_token .eq. C_LEFT_PAREN  .or.
     .        g_token .eq. C_RIGHT_PAREN .or. 
     :        g_token .eq. C_LITERAL)    then

              call   Check_previous_word
              g_number_expressions = g_number_expressions + 1
              call assign_string (
     :             g_expression_array(g_number_expressions), g_buffer)
              g_expression_array2(g_number_expressions) = g_token

              call   Get_next_token(Token_array, Token_array2)
              goto   10
          endif

          if (g_token .eq. C_EOL) then
             call   Get_next_token(Token_array, Token_array2)
             goto   10
          endif

          if (g_token .eq. C_EQUAL          .or.
     :        g_token .eq. C_NOT_EQUAL      .or.
     :        g_token .eq. C_LESS_THAN      .or.
     .        g_token .eq. C_LESS_EQUAL     .or. 
     :        g_token .eq. C_GREATER_THAN   .or.
     .        g_token .eq. C_GREATER_EQUAL  .or. 
     :        g_token .eq. C_AND            .or.
     .        g_token .eq. C_OR             .or. 
     :        g_token .eq. C_THEN)          then
     
             call   Check_previous_word
             g_save_token = g_token
          endif
       endif


       return
       end
* =====================================================================
       subroutine Check_previous_word ()
* =====================================================================

*   Short description:
*     Make sure you don't have two operators
*     or two variables next to each other.

*   Assumptions:
*      None

*   Notes:

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*      TM - 21/11/94

*   Calls:
*      Parse_error

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
*      none

*   Global variables
       include 'parse.inc'                  ! Manager common block

*   Internal variables
*      none

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

       if (g_token .eq. C_WORD    .or. 
     :     g_token .eq. C_NUMBER  .or.
     .     g_token .eq. C_LITERAL) then
          if (g_word_or_number .eq. YES) then
             call Parse_error('Missing operator    ',
     .                        'Get_expression_array')
          else
             g_word_or_number = YES
          endif
       else
          if (g_token .ne. C_LEFT_PAREN  .and. 
     :        g_token .ne. C_RIGHT_PAREN) then
             if (g_word_or_number .eq. NO) then
                call Parse_error('Missing identifier  ',
     .                           'Get_expression_array')
             else
                g_word_or_number = NO
             endif
          endif
       endif


       return
       end
* =====================================================================
       character*(*) function Real_or_not (Variable_Value)
* =====================================================================

*   Short description:
*     check to see if the value is a real
*     then return the resulting real or not.

*   Assumptions:
*      None

*   Notes:

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*      TM - 21/11/94

*   Calls:
*      none

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
       character*(*) Variable_Value

*   Global variables

*   Internal variables
       double precision Temp
       integer          Double_flag

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

       call Str_to_double_var(Variable_value, Temp, Double_flag)
       
       if     (Double_flag .eq. 0) then
              call Real_var_to_string(real(Temp), Variable_value)
       endif


       Real_or_not = Variable_Value

       return
       end
* =====================================================================
       subroutine Tokenize (Token_array, Token_array2, max_tokens)
* =====================================================================

*   Short description:
*      Read a file token by token
*      and put them into an array.

*   Assumptions:
*      None

*   Notes:

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*      TM - 21/11/94
*      jngh 24/2/95 put in calls to assign string
*      TM - 23/07/95 put in work around for elseifs
*     TM - 29/09/95 put in fix to check for negative numbers
*                    and get Action Strings as one Token

*   Calls:
*      assign_string*      Get_char
*      Get_token_from_file
*      Fatal_error
*     Get_Action

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
       character     Token_array(*)*(*)
       integer       Token_array2(*)
       integer       max_tokens

*   Global variables
       include 'parse.inc'                  ! Manager common block
       include 'const.inc'
*   Internal variables
       integer       ind                  ! loop index
       integer       count                ! loop index
       integer       elseif_count         !
       integer       if_count             !
       character     string_concat*(500)  ! function
       
*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

       g_first = 0
       g_last  = 0
       g_end_of_file = 0
       elseif_count = 0
       if_count = 0

       ind = g_start_token - 1
       if (ind .lt. 0) then
          ind = 0
       endif

       call   Get_Char()

10     continue

       if (ind.eq.max_tokens) then
          call fatal_error (err_internal, 'Token array limit exceeded')

       else
          call   Get_Token_from_file()

          if   (g_token .eq. C_IF .and. elseif_count .gt. 0) then
               if_count = if_count + 1
          endif

          if   (g_token .eq. C_ENDIF .and. elseif_count .gt. 0) then
               if  (if_count .gt. 0) then
                   if_count = if_count - 1
               else
                   do 20  count = 1, elseif_count
                      g_token = C_ENDIF
                      g_buffer = 'endif'
                      ind = ind + 1
                      call assign_string(Token_array(ind),g_buffer)
                      Token_array2(ind) = g_token
20                 continue
                   elseif_count = 0
               endif
          endif


          if   (g_token .eq. C_ELSEIF) then
               elseif_count  = elseif_count + 1
               g_token = C_ELSE
               g_buffer = 'else'
               ind = ind + 1
               call assign_string (Token_array(ind), g_buffer)
               Token_array2(ind) = g_token
               g_token = C_IF
               g_buffer = 'if'
          endif

         
          if   (g_token .eq. C_NUMBER .and. ind .ge. 2 .and.
     :           Token_array2(ind) .eq. C_MINUS .and.
     :           Token_array2(ind-1) .ne. C_NUMBER .and.
     :           Token_array2(ind-1) .ne. C_WORD) then
     
                 call assign_string (g_buffer, '-'//g_buffer)
                 ind = ind -1
        endif


          if   (ind .ge. 1 .and. g_token .eq. C_WORD .and.
     :          Token_array2(ind) .eq. C_WORD) then

                g_buffer = string_concat (Token_Array(ind),
     :                                          ' '//g_buffer)
               call Get_Action()
               g_token = C_ACTION
               ind = ind - 1
          endif


          ind = ind + 1
          call assign_string (Token_array(ind), g_buffer)
          Token_array2(ind) = g_token

          if     (g_end_of_file .eq. 0) then
              goto 10
          endif
       endif

       Token_array2(ind+1) = C_EOF

       g_last_token = ind
       return
       end
* =====================================================================
       subroutine Get_Token_from_file ()
* =====================================================================

*   Short description:
*     Get the next token from the manager file.

*   Assumptions:
*      None
*   Notes:

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*      TM - 21/11/94
*     DPH - 12/4/96   - Added check for quote character
*     DPH - 3/6/96    - Removed check for quote character - not needed.

*   Calls:
*      Get_Word
*      Get_Number
*      Get_Special

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments

*   Global variables
       include 'parse.inc'                  ! Manager common block

*   Internal variables
*      none

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

10     continue

       if     (g_ch .ge. 'a' .and. g_ch .le. 'z') then
              call   Get_Word()

       elseif (g_ch .ge. '0' .and. g_ch .le. '9') then
              call   Get_Number()

       elseif (g_ch .eq. '''') then
              call Get_Literal()

       else
              call   Get_Special()
              if     (g_token .eq. C_SPACE) then
                     goto 10
              end if

       endif

       return
       end
* =====================================================================
       subroutine Get_Char ()
* =====================================================================

*   Short description:
*     Get the next character from the manager file.

*   Assumptions:
*      None

*   Notes:

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*      TM - 21/11/94

*   Calls:
*      Read_line
*     assign_string
*      no_leading_spaces

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments

*   Global variables
       include 'parse.inc'                  ! Manager common block
       integer  LastNB                      ! function
       character no_leading_spaces*(Buffer_size) ! function

*   Internal variables
*      none

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

       g_first = g_first + 1

       if     (g_first .gt. g_last) then
           call    assign_string (g_last_line, g_line)
              call   Parse_read_line (g_line, g_end_of_file)
              g_line = no_leading_spaces (g_line)
              g_first = 0
              g_last = LastNB(g_line)
              g_ch = ';'
       else
              g_ch = g_line(g_first:g_first)
       end if

       return
       end
* =====================================================================
       subroutine Get_Word ()
* =====================================================================

*   Short description:
*     Get the next word token from the manager file.

*   Assumptions:
*      None

*   Notes:

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*      TM - 21/11/94*      TM - 05/10/95 - added check for left bracket
*     DPH - 12/4/96  - added check for strings in quotes.

*   Calls:
*      Get_char
*

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments

*   Global variables
       include 'parse.inc'                  ! Manager common block
       character     String_concat*(Buffer_size)
                                          ! function
       logical       Reserved             ! function
*   Internal variables
       integer       left                 ! left brackets 
       logical       Inside_quotes        ! Are we currently inside quotes?

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

       if (g_ch .eq. '''') then
          Inside_quotes = .true.
       else
          g_buffer = g_ch
          Inside_quotes = .false.
       endif

       left = 0
       
10     continue

       call   Get_Char()

      if (g_ch .eq. '''') then
         if (Inside_quotes) then
            Inside_quotes = .false.
         else
            Inside_quotes = .true.
         endif
         goto 10

      else if (Inside_quotes .or.
     .     (g_ch .ge. 'a' .and. g_ch .le. 'z') .or.
     .      g_ch .eq. '.' .or.
     .     (g_ch .ge. '0' .and. g_ch .le. '9') .or. 
     :     (g_ch .eq. '_'     .or.
     .      g_ch .eq. '['     .or. 
     :      g_ch .eq. ']'     .or. 
     :      g_ch .eq. '(')     .or.
     .      (g_ch .eq. ')' .and. left .gt. 0))   then
              
          g_buffer = String_concat(g_buffer, g_ch)
          
          if  (g_ch .eq. '(') then
               left = left + 1
          endif

          if  (g_ch .eq. ')') then
               left = left - 1
          endif
          
          goto 10
       endif



       if     (Reserved()) then
              ! reserved word
       else
              g_token = C_WORD
       endif

       return
       end
* =====================================================================
       subroutine Get_Literal ()
* =====================================================================

*   Short description:
*     Get the next word in quotes from the manager file.

*   Assumptions:
*      None

*   Notes:

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*      TM - 06/12/94

*   Calls:
*      Get_char
*
* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments

*   Global variables
       include 'parse.inc'                  ! Manager common block
       character     String_concat*(Buffer_size)
                                          ! function

*   Internal variables
*      none

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

       g_buffer = ' '

10     continue

       call   Get_Char()

       if (g_ch .ne. '''') then
              g_buffer = String_concat (g_buffer, g_ch)
              goto 10
       endif

       call   Get_Char()

       g_token = C_LITERAL


       return
       end
* =====================================================================
       subroutine Get_Number ()
* =====================================================================

*   Short description:
*     Get the next number token from the manager file.

*   Assumptions:
*      None

*   Notes:

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*      TM - 21/11/94

*   Calls:
*      Get_char
*

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments

*   Global variables
       include 'parse.inc'                  ! Manager common block
       character     String_concat*(Buffer_size)
                                          ! function
*   Internal variables
*      none

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------


       g_buffer = g_ch
10     continue

       call Get_Char()

       if ((g_ch .ge. '0' .and. g_ch .le. '9')  .or. 
     :     (g_ch .eq. '.'))                     then
              g_buffer = String_concat (g_buffer, g_ch)
              goto 10
       end if


       g_token = C_NUMBER

       return
       end
* =====================================================================
       subroutine Get_Special ()
* =====================================================================

*   Short description:
*     Get the next special token from the manager file.

*   Assumptions:
*      None

*   Notes:

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*      TM - 21/11/94

*   Calls:
*      Get_char
*

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments

*   Global variables
       include 'parse.inc'                  ! Manager common block

*   Internal variables
*      none

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

       g_buffer = g_ch


       if (g_ch .eq. '-') then
              g_token = C_MINUS
              call Get_Char()

       elseif (g_ch .eq. '+') then
              g_token = C_PLUS
              call Get_Char()

       elseif (g_ch .eq. '^') then
              g_token = C_POWER
              call Get_Char()

       elseif (g_ch .eq. '*') then
              g_token = C_MULTIPLY
              call Get_Char()
              if (g_ch .eq. '*') then
                     g_buffer = '**'
                     g_token = C_POWER
                     call Get_Char()
              endif

       elseif (g_ch .eq. '/') then
              g_token = C_DIVIDE
              call Get_Char()

       elseif (g_ch .eq. '=') then
              g_token = C_EQUAL
              call Get_Char()

       elseif (g_ch .eq. '<') then
              g_token = C_LESS_THAN
              call Get_Char()
              if (g_ch .eq. '>') then
                     g_buffer = '<>'
                     g_token = C_NOT_EQUAL
                     call Get_Char()
              elseif (g_ch .eq. '=') then
                     g_buffer = '<='
                     g_token = C_LESS_EQUAL
                     call Get_Char()
              endif

       elseif (g_ch .eq. '>') then
              g_token = C_GREATER_THAN
              call Get_Char()
              if (g_ch .eq. '=') then
                     g_buffer = '>='
                     g_token = C_GREATER_EQUAL
                     call Get_Char()
              endif

       elseif (g_ch .eq. '(') then
              g_token = C_LEFT_PAREN
              call Get_Char()

       elseif (g_ch .eq. ')') then
              g_token = C_RIGHT_PAREN
              call Get_Char()

       elseif (g_ch .eq. ';') then
              g_token = C_EOL
              call Get_Char()
       
       elseif (g_ch .eq. ' ') then
              g_token = C_SPACE
              call Get_Char()
       
       else
              g_token = C_SPECIAL
              call Get_Char()

       endif


       return
       end
* =====================================================================
       subroutine Get_Action ()
* =====================================================================

*   Short description:
*     Get the entire line from the manager file.

*   Assumptions:
*      None

*   Notes:

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*      TM - 29/09/95

*   Calls:
*      Get_char
*      Assign_string

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments

*   Global variables
       include 'parse.inc'                  ! Manager common block

*   Internal variables
*      none

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

       if (g_ch .eq. ';') then
          call assign_string (g_buffer, g_last_line)
      else
          call assign_string (g_buffer, g_line)
       endif
       

10     continue
       if (g_ch .ne. ';') then
          call   Get_Char()
          goto 10
       endif

       return
       end
* =====================================================================
       logical function Reserved()
* =====================================================================

*   Short description:
*     Check to see if word is a reserved word.

*   Assumptions:
*      None

*   Notes:

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*      TM - 21/11/94
*      TM - 23/07/95   Add C_ELSEIF to list

*   Calls:
*

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
*      none

*   Global variables
       include 'parse.inc'                  ! Manager common block

*   Internal variables
       logical       Found                ! Word found flag
       integer       ind                  ! loop index

*   Constant values
       integer       Num_reserved_words
       parameter     (Num_reserved_words = 7)

       character*12  Reserved_word_array(Num_reserved_words)
       integer       Reserved_word_array2(Num_reserved_words)

       data          Reserved_word_array /'if    ','then  ',
     .               'else  ','endif ','or    ','and   ','elseif'/

       data          Reserved_word_array2 /C_IF,C_THEN,C_ELSE,C_ENDIF,
     .                                     C_OR,C_AND,C_ELSEIF/


*   Initial data values
*      none

* --------------------- Executable code section ----------------------

       Found = .false.

       do 10  ind = 1, Num_reserved_words
              if (g_buffer .eq. Reserved_word_array(ind)) then
                     Found = .true.
                     g_token = Reserved_word_array2(ind)
              endif
10     continue

       Reserved = Found

       return
       end
