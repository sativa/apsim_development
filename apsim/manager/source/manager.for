! --------------------------------------------------------------------
!   Short description:
!      Variables used in Manager

!   Assumptions:
!      None

!   Notes:

!   Changes:
!      DPH - 23/11/92
!      TM - 20/11/94
!      JNGH 22/2/95 restructured file
!      JNGH 22/2/95 combined two man_general blocks and changed name
!                   to avoid confilct with block of same name in manager.inc
!                   added save statements to all common blocks
!                   removed nonstandard *2 on integers
!      TM - 13/03/95 added g_start_token so we can use the same array
!                    more than once
!      DPH 19/7/95    Added init, prepare, process and post indexes into token array
!      TM - 29/09/95  added g_last_line
!      TM - 04/10/95 - put a C_ in front of all constants so as not
!                      to have any conflicts with key words
!      DPH 27/5/96   - Increased buffer_size to 500 (was 200)
!      JNGH - 23/4/98 increased max-local-variables from 50 to 100
!                     increased max_tokens from 1000 to 2000
!      DPH 30/8/99    added g_line_number, g_num_lines and g_lines
!                     Merged manager.inc into parse.inc and made into module.


! ----------------------- Declaration section ------------------------
      module ManagerModule

!  Constant variables
      integer Max_local_variables      ! Maximum number of local vars.
      parameter (Max_local_variables=100)

      integer Max_manager_var_name_size
      parameter (Max_manager_var_name_size=35)
      
      integer Max_variable_value_size   ! Maximum size of a local variable name
      parameter (Max_variable_value_size=30)

      integer Max_tokens               ! Maximum number of tokens
      parameter (Max_tokens=4000)

      integer Max_token_size           ! Maximum size of a token
      parameter (Max_token_size=200)

      integer        Buffer_size                 ! size of each buffer
      parameter      (Buffer_size = 500)

      integer        file_maximum                ! maximum tokens in file
      parameter      (file_maximum = 2000)

      integer        C_WORD
      parameter      (c_WORD = 1)

      integer        C_NUMBER
      parameter      (C_NUMBER = 2)

      integer        C_LITERAL
      parameter      (C_LITERAL = 3)

      integer        C_SPECIAL
      parameter      (C_SPECIAL = 4)

      integer        C_PLUS
      parameter      (C_PLUS = 5)

      integer        C_MINUS
      parameter      (C_MINUS = 6)

      integer        C_MULTIPLY
      parameter      (C_MULTIPLY = 7)

      integer        C_DIVIDE
      parameter      (C_DIVIDE = 8)

      integer        C_POWER
      parameter      (C_POWER = 9)

      integer        C_EQUAL
      parameter      (C_EQUAL = 10)

      integer        C_LESS_THAN
      parameter      (C_LESS_THAN = 11)

      integer        C_GREATER_THAN
      parameter      (C_GREATER_THAN = 12)

      integer        C_NOT_EQUAL
      parameter      (C_NOT_EQUAL = 13)

      integer        C_LESS_EQUAL
      parameter      (C_LESS_EQUAL = 14)

      integer        C_GREATER_EQUAL
      parameter      (C_GREATER_EQUAL = 15)

      integer        C_AND
      parameter      (C_AND = 16)

      integer        C_OR
      parameter      (C_OR = 17)

      integer        C_EOL
      parameter      (C_EOL = 18)

      integer        C_EOF
      parameter      (C_EOF = 19)

      integer        C_LEFT_PAREN
      parameter      (C_LEFT_PAREN = 20)

      integer        C_RIGHT_PAREN
      parameter      (C_RIGHT_PAREN = 21)

      integer        C_IF
      parameter      (C_IF = 22)

      integer        C_THEN
      parameter      (C_THEN = 23)

      integer        C_ENDIF
      parameter      (C_ENDIF = 24)

      integer        C_ELSE
      parameter      (C_ELSE = 25)

      integer        C_END
      parameter      (C_END = 26)

      integer        C_SPACE
      parameter      (C_SPACE = 27)

      integer        C_ELSEIF
      parameter      (C_ELSEIF = 28)

      integer        C_ACTION
      parameter      (C_ACTION = 29)

      integer        Variable_maximum            ! maximum number of array
      parameter      (Variable_maximum = 80)

      integer        stack_maximum               ! maximum number of tokens
      parameter      (stack_maximum = 200)       ! that will fit on stack

      integer        YES                         ! YES flag
      parameter      (YES = 0)

      integer        NO                          ! NO flag
      parameter      (NO = -1)

      integer MAX_INSTANCE_NAME_SIZE
      parameter (MAX_INSTANCE_NAME_SIZE=50)

      type ManagerData

!   Global variables
         character Instance_name*(MAX_INSTANCE_NAME_SIZE)
                                          ! instance name 

         character local_variable_names(Max_local_variables)*
     .       (Max_manager_var_name_size)  !  Array to hold local variables names
         character local_variable_values(Max_local_variables)*
     .       (Max_variable_value_size)    ! Array to hold local variables

         character token_array(Max_tokens)*(Max_token_size)
                                          ! Array to hold tokens.
         character current_section*50   ! Current section to look in for reading of rules

         integer num_local_variables    ! Number of local variables.
         integer token_array2(Max_tokens)
                                       ! Second array for tokens.
         integer start_day_index1       ! index into token array for start of day 1 (BLANK keyword)
         integer start_day_index2       ! index into token array for start of day 2 ('start_of_day')
         integer end_day_index          ! index into token array for end of day

         integer init_index             ! index into token array for 'init' keyword
         integer prepare_index          ! index into token array for 'prepare' keyword
         integer process_index          ! index into token array for 'process' keyword
         integer post_index             ! index into token array for 'post' keyword
         integer line_number            ! line number in section to read from.
         integer num_lines              ! number of lines in section
         integer lines                  ! C++ MEMO object containing all lines in section
      
         logical lines_been_read        ! have any lines been read so far?

         ! PARSING variables.
         
         integer       token                     ! type of word
         integer       end_of_file               ! End of file flag
         integer       start_token               ! Where to start filling token array
         integer       last_token                ! Position of last token stored.
         character     buffer*(Buffer_size)      ! extract word
         character     line*(Buffer_size)        ! line read from file
         character     last_line*(Buffer_size)   ! last line read from file
         character     ch                        ! next character in g_line
         integer       save_token                ! saved g_token for sub arrays
         integer       first                     ! first position in line
         integer       last                      ! last position in line
         integer       all_ok                    ! error flag
         integer       number_of_variables       ! number of array variables
         integer       number_of_tokens          ! number of array tokens
         integer       number_and_or             ! number of AND/OR expressions
         integer       number_expressions        ! number of expressions in array
         integer       current_token             ! number of current token in
                                                   ! sub array
         integer       next_token                ! number of current token in
                                                   ! entire array
         integer       word_or_number            ! flag to tell if token is
                                                   ! word/number
         integer       expression_array2(Variable_maximum)
                                                   ! arrays to hold the entire
                                                   ! expression
         integer       expression_sub_array2(Variable_maximum)
                                                   ! arrays to hold a sub part
                                                   ! of each expression
         integer       and_or_array2(Variable_maximum)
                                                   ! arrays to hold an AND/OR
                                                   ! part of each expression
         character     expression_result*(Buffer_size)
                                                   ! returned expression result
         character     expression_array(Variable_maximum)*(Buffer_size)
         character     stack(stack_maximum)*(Buffer_size)
                                                   ! stack used in expression
         character     expression_sub_array(Variable_maximum)*
     .                                       (Buffer_size)
         character     and_or_array(Variable_maximum)*(Buffer_size)

      end type ManagerData

      ! instance variables.
      type (ManagerData), pointer :: g
      integer MAX_NUM_INSTANCES
      parameter (MAX_NUM_INSTANCES=10)  
      type ManagerDataPtr
         type (ManagerData), pointer :: ptr
      end type ManagerDataPtr
      type (ManagerDataPtr), dimension(MAX_NUM_INSTANCES) :: Instances


      end module ManagerModule


!     ===========================================================
      subroutine AllocInstance (InstanceName, InstanceNo)
!     ===========================================================
      use ManagerModule
      implicit none
 
!+  Sub-Program Arguments
      character InstanceName*(*)       ! (INPUT) name of instance
      integer   InstanceNo             ! (INPUT) instance number to allocate
 
!+  Purpose
!      Module instantiation routine.
 
!- Implementation Section ----------------------------------
               
      allocate (Instances(InstanceNo)%ptr)
      Instances(InstanceNo)%ptr%Instance_name = InstanceName
 
      return
      end

!     ===========================================================
      subroutine FreeInstance (anInstanceNo)
!     ===========================================================
      use ManagerModule
      implicit none
 
!+  Sub-Program Arguments
      integer anInstanceNo             ! (INPUT) instance number to allocate
 
!+  Purpose
!      Module de-instantiation routine.
 
!- Implementation Section ----------------------------------
               
      deallocate (Instances(anInstanceNo)%ptr)
 
      return
      end
     
!     ===========================================================
      subroutine SwapInstance (anInstanceNo)
!     ===========================================================
      use ManagerModule
      implicit none
 
!+  Sub-Program Arguments
      integer anInstanceNo             ! (INPUT) instance number to allocate
 
!+  Purpose
!      Swap an instance into the global 'g' pointer
 
!- Implementation Section ----------------------------------
               
      g => Instances(anInstanceNo)%ptr
 
      return
      end


! ====================================================================
       subroutine Main (Action, Data_string)
! ====================================================================
      use ManagerModule
      implicit none
      include 'const.inc'             ! Global constant definitions
      include 'error.pub'                         

!+  Sub-Program Arguments
       character Action*(*)            ! Message action to perform
       character Data_string*(*)       ! Message data

!+  Purpose
!      This module acts as the APSIM manager.

!+  Changes
!      DPH - 7/10/92
!      DPH - 9/02/95 Substantially modified to incorporate a better
!                    parsing method allowing nesting of brackets in
!                    rules, nesting of ANDS and ORs and allowing
!                    local variables to be defined.
!     jngh 24/2/95 changed data to data_string
!     DPH 19/7/95  Added call to manager_process
!     DPH 27/10/95 Added call to message_unused
!     jngh - 08/06/96 removed a_ from front of version function
!     jngh - 23/04/98 added call to zero variables at initialisation
!     dph - 7/5/99 removed version and presence report c186

!+  Calls

!+  Constant Values
      character  my_name*(*)           ! name of this procedure
      parameter (my_name='manager')

!- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (action .eq. mes_get_variable) then
         call manager_send_my_variable (Data_string)
 
      else if (Action.eq.MES_Init) then
         call Manager_zero_variables ()
         call Manager_Init ()
 
      else if (Action.eq.MES_Prepare) then
         call Manager_Prepare ()
 
      else if (Action.eq.MES_Process) then
         call Manager_Process ()
 
      else if (Action.eq.MES_Post) then
         call Manager_Post ()
 
      else if (Action .eq. MES_Event) then
         call Manager_Event (Data_string)
 
      else
         ! Don't use message
 
         call Message_unused ()
      endif
 
      call pop_routine (my_name)
      return
      end



! ====================================================================
       subroutine Manager_Init ()
! ====================================================================
      use ManagerModule
      implicit none
       include 'const.inc'             ! constant definitions
      include 'error.pub'                         

!+  Purpose
!      Initialise Manager model.

!+  Changes
!      DPH - 8/10/92
!      DPH - 21/10/94 Modified to bring up to APSIM 1.0 standard.
!      DPH - 6/7/95   Added check for case when no manager lines were found.
!                     Added code to output a 'manager rules' line to summary file
!      DPH - 19/7/95  Added call to manager_init_rules to allow the parsing routine
!                     to parse any user initialisation rules.
!      dph - 7/5/99   Removed version report c186

!+  Calls

!+  Constant Values
      character This_routine*(*)       ! Name of this routine
      parameter (This_routine='Manager_Init')

!+  Local Variables
       character  msg*200              ! err message

!- Implementation Section ----------------------------------
 
      call push_routine(This_routine)
 
      g%num_local_variables = 0
      g%lines_been_read = .false.
 
      msg = 'Manager rules:'
      call Write_string(LU_Summary_file, msg)
 
      call Manager_read_rules ()
 
      ! check for case when no manager lines were found anywhere.  Issue warning
 
      if (g%lines_been_read) then
         ! we're ok - lines were found
 
      else
         msg = 'No manager lines were found in any parameter file.'
         call Warning_error(ERR_user, msg)
      endif
 
      call manager_init_rules ()
 
      call pop_routine(This_routine)
 
      return
      end



! ====================================================================
       subroutine Manager_zero_variables ()
! ====================================================================
      use ManagerModule
      implicit none
      include 'const.inc'              ! constant definitions
      include 'data.pub'                          
      include 'error.pub'                         

!+  Purpose
!     Zero all common block arrays
!     routine.

!+  Changes
!     230498 jngh

!+  Constant Values
      character Routine_name*(*)       ! Name of this routine
      parameter (Routine_name='Manager_zero_variables')

!- Implementation Section ----------------------------------
 
      call push_routine (Routine_name)
 
      g%buffer = blank
      g%expression_result = blank
 
      call fill_char_array (g%expression_array, blank, Variable_maximum)
      call fill_char_array (g%stack, blank, stack_maximum)
 
      call fill_char_array (g%expression_sub_array
     :                     , blank, Variable_maximum)
      call fill_char_array (g%and_or_array, blank, Variable_maximum)
      g%line = blank
      g%last_line = blank
      g%ch = blank
      call fill_char_array (g%local_variable_names
     :                     , blank, Max_local_variables)
      call fill_char_array (g%local_variable_values
     :                     , blank, Max_local_variables)
 
      call fill_char_array (g%token_array, blank, Max_tokens)
      g%current_section = blank
      g%token         = 0
      g%end_of_file   = 0
      g%start_token   = 0
      g%last_token    = 0
      g%save_token    = 0
      g%first         = 0
      g%last          = 0
      g%all_ok        = 0
      g%number_of_variables = 0
      g%number_of_tokens    = 0
      g%number_and_or       = 0
      g%number_expressions  = 0
      g%current_token       = 0
 
      g%next_token          = 0
 
      g%word_or_number      = 0
 
      call fill_integer_array (g%expression_array2, 0, Variable_maximum)
 
      call fill_integer_array (g%expression_sub_array2
     :                        , 0, Variable_maximum)
 
      call fill_integer_array (g%and_or_array2, 0, Variable_maximum)
 
      g%num_local_variables      = 0
 
      call fill_integer_array (g%token_array2, 0, Max_tokens)
 
      g%start_day_index1         = 0
      g%start_day_index2         = 0
      g%end_day_index            = 0
 
      g%init_index               = 0
      g%prepare_index            = 0
      g%process_index            = 0
      g%post_index               = 0
 
      g%lines_been_read          = .false.
 
      call pop_routine(Routine_name)
      return
      end



! ====================================================================
       subroutine Manager_read_rules ()
! ====================================================================
      use ManagerModule
      implicit none
      include 'const.inc'              ! constant definitions
      include 'error.pub'                         
      include 'apsimengine.pub'

!+  Purpose
!     Read in all criterias one word at a time and pass it to a processing
!     routine.

!+  Changes
!     DPH 5/12/94
!     DPH 19/7/95 Added code to look for init, prepare, process and post sections
!     DPH 10/7/96 Re-ordered code so that manager will look for init section
!                 first, then start_of_day, prepare in chronological order.
!     DPH 30/8/99 Changed code to use C++ MEMO objects

!+  Constant Values
      character Routine_name*(*)       ! Name of this routine
      parameter (Routine_name='Manager_read_rules')

!+  Local variables
      logical ok

!- Implementation Section ----------------------------------
 
      call push_routine (Routine_name)
 
      ! Set the read flag so that the next call to manager_read_line
      ! will restart the reading routine.
 
      call Memo_Create(g%lines)
 
      ! Go tokenize the parameter file.
 
!      g%start_token = 1
!      g%start_day_index1 = 1
!      g%current_section = Blank
!      call Tokenize (g%token_array, g%token_array2, max_tokens)
 
      g%start_token = g%last_token + 2
      g%init_index = g%start_token
      g%current_section = 'init'
      ok = ApsimSystem_Data_Get(
     .    Trim(g%Instance_name) // '.' // g%current_section, g%lines)
      g%num_lines = Memo_GetLineCount(g%lines)
      g%line_number = 0
      call Tokenize (g%token_array, g%token_array2, max_tokens)
 
!      g%start_token = g%last_token + 2
!      g%start_day_index2 = g%start_token
!      g%current_section = 'start_of_day'
!      call Tokenize (g%token_array, g%token_array2, max_tokens)
 
      g%start_token = g%last_token + 2
      g%prepare_index = g%start_token
      g%current_section = 'prepare'
      ok = ApsimSystem_Data_Get(
     .    Trim(g%Instance_name) // '.' // g%current_section, g%lines)
      g%num_lines = Memo_GetLineCount(g%lines)
      g%line_number = 0
      call Tokenize (g%token_array, g%token_array2, max_tokens)
 
      g%start_token = g%last_token + 2
      g%process_index = g%start_token
      g%current_section = 'process'
      ok = ApsimSystem_Data_Get(
     .    Trim(g%Instance_name) // '.' // g%current_section, g%lines)
      g%num_lines = Memo_GetLineCount(g%lines)
      g%line_number = 0
      call Tokenize (g%token_array, g%token_array2, max_tokens)
 
!      g%start_token = g%last_token + 2
!      g%end_day_index = g%start_token
!      g%current_section = 'end_of_day'
!      call Tokenize (g%token_array, g%token_array2, max_tokens)
 
      g%start_token = g%last_token + 2
      g%post_index = g%start_token
      g%current_section = 'post'
      ok = ApsimSystem_Data_Get(
     .    Trim(g%Instance_name) // '.' // g%current_section, g%lines)
      g%num_lines = Memo_GetLineCount(g%lines)
      g%line_number = 0
      call Tokenize (g%token_array, g%token_array2, max_tokens)

      call Memo_Free (g%lines)
      
      ok = ok    ! stops compiler warning
      
      call pop_routine(Routine_name)
      return
      end



! ====================================================================
       subroutine Manager_init_rules ()
! ====================================================================
      use ManagerModule
      implicit none
       include 'const.inc'             ! Global constant definitions
      include 'error.pub'                         

!+  Purpose
!     Check to see if any criteria for initialisation are met.  If
!     so then issue message to relevent module.

!+  Changes
!     DPH 19/7/95

!+  Constant Values
      character  my_name*(*)           ! name of this procedure
      parameter (my_name='manager_init_rules')

!- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      ! Go call the parsing routine.
 
      g%start_token = g%init_index
      call Parse (g%token_array, g%token_array2)
 
      call pop_routine (my_name)
      return
      end



! ====================================================================
       subroutine Manager_Prepare ()
! ====================================================================
      use ManagerModule
      implicit none
       include 'const.inc'             ! Global constant definitions
      include 'error.pub'                         

!+  Purpose
!     Check to see if any criteria for prepare is met.  If
!     so then issue message to relevent module.

!+  Changes
!     DPH 5/12/94
!     DPH 19/7/95  Added code to parse the prepare index part of token array

!+  Constant Values
      character  my_name*(*)           ! name of this procedure
      parameter (my_name='manager_prepare')

!- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      ! Go call the parsing routine.
 
      g%start_token = g%start_day_index1
      call Parse (g%token_array, g%token_array2)
 
      g%start_token = g%start_day_index2
      call Parse (g%token_array, g%token_array2)
 
      g%start_token = g%prepare_index
      call Parse (g%token_array, g%token_array2)
 
      call pop_routine (my_name)
      return
      end



! ====================================================================
       subroutine Manager_Process ()
! ====================================================================
      use ManagerModule
      implicit none
       include 'const.inc'             ! Global constant definitions
      include 'error.pub'                         

!+  Purpose
!     Check to see if any criteria for process is met.  If
!     so then issue message to relevent module.

!+  Changes
!     DPH 19/7/95

!+  Constant Values
      character  my_name*(*)           ! name of this procedure
      parameter (my_name='manager_process')

!- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      ! Go call the parsing routine.
 
      g%start_token = g%process_index
      call Parse (g%token_array, g%token_array2)
 
      call pop_routine (my_name)
      return
      end



! ====================================================================
       subroutine Manager_Post ()
! ====================================================================
      use ManagerModule
      implicit none
       include 'const.inc'             ! Global constant definitions
      include 'error.pub'                         

!+  Purpose
!     Check to see if any criteria for post is met.  If
!     so then issue message to relevent module.

!+  Changes
!     DPH 5/12/94
!     DPH 19/7/95  Added code to check in the post index part of token array.

!+  Constant Values
      character  my_name*(*)           ! name of this procedure
      parameter (my_name='manager_post')

!- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      ! Go call the parsing routine.
      g%start_token = g%end_day_index
      call Parse (g%token_array, g%token_array2)
 
      g%start_token = g%post_index
      call Parse (g%token_array, g%token_array2)
 
      call pop_routine (my_name)
      return
      end



! ====================================================================
       subroutine Manager_event (Event_data)
! ====================================================================
      implicit none

!+  Sub-Program Arguments
      character Event_data*(*)         ! (INPUT) Event data string

!+  Purpose
!     An event has occurred today.  Capture and store it in the
!     events string.

!+  Changes
!     DPH 12/1/94
!     DPH 11/7/94 Added call to no_leading%spaces.

!- Implementation Section ----------------------------------
 
      ! Convert module's event string to lowercase and remove
      ! the module name the event came from.
 
      return
      end



! ====================================================================
      subroutine manager_send_my_variable (variable_name)
! ====================================================================
      use ManagerModule
      implicit none
      include 'const.inc'             ! Global constant definitions
      include 'datastr.pub'                       
      include 'intrface.pub'                      
      include 'error.pub'                         

!+  Sub-Program Arguments
      character variable_name*(*)      ! (input) variable name to search for

!+  Purpose
!      return the value of a variable in return_string.  used to return
!      values of variables requested by other modules.

!+  Changes
!     DPH 9/02/95
!     DPH 27/10/95 Added call to message_unused
!     DPH 10/4/96  Changed the call from respond2get_real_var to
!                  respond2get_char_var so that character variables can
!                  be sent to other modules.

!+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'manager_send_my_variable')

!+  Local Variables
      integer Variable_index           ! index into local variable list

!- Implementation Section ----------------------------------
 
      call push_routine (my_name)
 
      ! Try to find variable in local variable list.
 
      Variable_index = find_string_in_array
     .   (Variable_name, g%local_variable_names, g%num_local_variables)
 
      if (Variable_index .gt. 0) then
         call respond2get_char_var (Variable_name, '()',
     .                     g%local_variable_values(Variable_index))
      else
         ! not our variable
 
         call Message_unused ()
      endif
 
      call pop_routine (my_name)
      return
      end



! ====================================================================
       subroutine Parse_read_line(Line, EOF_flag)
! ====================================================================
      use ManagerModule
      implicit none
      include 'const.inc'              ! constant definition
      include 'read.pub'                          
      include 'error.pub'                         
      include 'apsimengine.pub'

!+  Sub-Program Arguments
      character Line*(*)               ! (OUTPUT) Line read from file
      integer EOF_flag                 ! (OUTPUT) = 0 when eof reached

!+  Purpose
!     Read next line from file.  Return EOF_flag = 0 when end of
!     file encountered.

!+  Changes
!     DPH 5/12/94
!     DPH 6/7/95   Added code to set g%lines_been_read to .true.
!                  Added code to write all lines to summary file
!     DPH 30/8/99  Changed to call MEMO C++ object instead of Read_next_param_section

!+  Constant Values
      character  my_name*(*)           ! name of this procedure
      parameter (my_name='Parse_read_line')

!+  Local variables
      integer Pos_comment

!- Implementation Section ----------------------------------
      call push_routine (my_name)
 
10    continue         
      if (g%line_number .ge. g%Num_lines) then
         EOF_flag = 1
         
      else 
         call Memo_GetLine(g%lines, g%line_number, Line)

         ! advance line number
         g%line_number = g%line_number + 1

         ! remove any comments.
         Pos_comment = index(Line, "!")
         if (Pos_comment .gt. 0) then
            Line(Pos_comment:) = Blank
         endif

         if (Line .eq. Blank) goto 10
         
         g%lines_been_read = .true.
 
         ! Echo all lines to summary file
         call Write_string(LU_Summary_file, Line)
         
      endif
 
      call pop_routine (my_name)
      return
      end



! ====================================================================
       subroutine Manager_new_local_variable(Variable_name,
     .                                       Variable_value)
! ====================================================================
      use ManagerModule
      implicit none
      include 'const.inc'              ! constant definitions
      include 'string.pub'                        
      include 'error.pub'                         

!+  Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name to store
      character Variable_value*(*)     ! (INPUT) Variable value to store

!+  Purpose
!     Add a new local variable to list.

!+  Changes
!     DPH 9/02/95
!     jngh 24/2/95 put in calls to assign string

!+  Local Variables
      character Str*300                ! Dummy value returned by APSIM

!- Implementation Section ----------------------------------
 
      g%num_local_variables = g%num_local_variables + 1
 
      if (g%num_local_variables .gt. Max_local_variables) then
         write (str, '(50a)' )
     .      'Too many local variables have been specified in ',
     .      new_line,
     .      'manager file.'
         call Fatal_error(ERR_user, str)
 
      else
         call assign_string (
     :        g%local_variable_names(g%num_local_variables)
     :      , Variable_name)
         call assign_string (
     :        g%local_variable_values(g%num_local_variables)
     :      , Variable_value)
      endif
 
      return
      end



! ====================================================================
       subroutine manager_get_params (Function_call, Params)
! ====================================================================
      use ManagerModule
      implicit none
      include 'const.inc'              ! constant definitions
      include 'intrface.pub'                      
      include 'string.pub'                        
      include 'error.pub'                         
      include 'datastr.pub'                       

!+  Sub-Program Arguments
      character     Function_call*(*)  ! (INPUT) function call
      character     Params(2)*(*)      ! (OUTPUT) params from function call

!+  Purpose
!     This routine returns the parameters from the specified function
!     call.  Return blank string on error.

!+  Assumptions
!      assumes no more than 2 parameters

!+  Notes
!     if function_call = 'date_between (1/8, 1/9)
!     then params(1) = 1/8
!          params(2) = 1/9

!+  Changes
!     DPH 4/9/96

!+  Local Variables
      integer pos_open_bracket
      integer pos_close_bracket
      integer pos_comma

!- Implementation Section ----------------------------------
 
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



! ====================================================================
      recursive subroutine Parse_get_variable
     .                 (Variable_Name, Variable_Value)
! ====================================================================
      use ManagerModule
      implicit none
      include 'const.inc'              ! constant definitions
      include 'datastr.pub'
      include 'date.pub'
      include 'error.pub'
      include 'string.pub'
      include 'intrface.pub'

!+  Sub-Program Arguments
      character     Variable_Name*(*)  ! (INPUT) name of variable
      character     Variable_Value*(*) ! (OUTPUT) return value of variable

!+  Purpose
!     The parse routine has requested a variable.  Return value to it.

!+  Changes
!     DPH 5/12/94
!      jngh 24/2/95 put in calls to assign string
!     dph 25/7/96  added code to put a message in summary file when creating
!                  a new local variable
!     dph 4/9/96   added function 'date_within'
!     dph 2/10/96  changed call to date_between to date_within
!     sb  19/3/97  added manager function nearest_int().
!     dph 10/2/98  called write_event instead of report_event - d097

!+  Local Variables
      logical Is_apsim_variable        ! Is the requested variable APSIM's?
      integer Numvals                  ! Number of values found.
      integer Variable_index           ! Index into local variable array
      character Mod_name*100           ! name of module owning variable
      character Var_name*100           ! name of variable
      character Str*300                ! Dummy value returned by APSIM
      character Params(2)*(50)         ! params from function call
      double precision d_var_val       ! double precision of variable_value
      double precision today           ! todays date.

!- Implementation Section ----------------------------------
 
      ! Look for function first.
 
      if (variable_name(1:5) .eq. 'date(') then
         call Manager_get_params (variable_name, Params)
         call Get_double_var (Unknown_module, 'today', '', Today, 
     .                        numvals, 0, 10000000d0)
         call Double_var_to_string (Date(Params(1), Today), 
     .                              Variable_value)
 
      else if (variable_name(1:12) .eq. 'date_within(') then
         ! get parameters from string.
 
         call Manager_get_params (variable_name, Params)

         call Get_double_var (Unknown_module, 'today', '', Today, 
     .                        numvals, 0, 10000000d0)
 
         if (Date_within(Params(1), Params(2), Today)) then
            Variable_value = '1'
         else
            Variable_value = '0'
         endif
 
      else if (variable_name(1:12) .eq. 'nearest_int(') then
         call Manager_get_params (variable_name, Params)
         call parse_get_variable(params(1), variable_value)
         call string_to_double_var(variable_value, d_var_val, numvals)
         if (numvals .ne. 1) then
            call fatal_error(ERR_user,
     .              'Bad argument type for function nearest_int()')
         else
            d_var_val = dnint(d_var_val)
            call double_var_to_string (d_var_val, variable_value)
         end if
 
      else
         Is_apsim_variable = (index(variable_name, '.') .gt. 0)
 
         if (Is_apsim_variable) then
            call Split_line(variable_name, Mod_name, Var_name, '.')
            call Get_char_var
     .           (Mod_name, Var_name, '()',
     .            Variable_value, Numvals)
 
         else
 
            ! Try to find variable in local variable list.
 
            Variable_index = find_string_in_array
     .         (variable_name, g%local_variable_names,
     .          g%num_local_variables)
 
            ! If not in local variable list then ask APSIM for it.
 
            if (Variable_index .le. 0) then
               call Get_char_var_optional
     .              (Unknown_module, variable_name, '()',
     .               Variable_value, Numvals)
 
               ! If not found anywhere in APSIM then it must be a local
               ! variable not already defined.  Add variable to list.
 
               if (Numvals .eq. 0) then
                  call manager_new_local_variable(variable_name, '0')
                  Variable_value = '0'
                  write (str, '(4a)' )
     .              'Manager creating a new local variable : ',
     .               trim(variable_name),
     .               ' = 0'
                  call Write_string (0, str)
 
               else
                  ! Found variable elsewhere in APSIM
               endif
 
            else
               call assign_string (Variable_value
     .                      , g%local_variable_values(Variable_index))
 
            endif
         endif
 
      endif
 
      return
      end



! ====================================================================
       subroutine Parse_set_variable (Variable_Name, Variable_Value)
! ====================================================================
      use ManagerModule
      implicit none
      include 'const.inc'              ! constant definitions
      include 'datastr.pub'                       
      include 'intrface.pub'                      
      include 'string.pub'                        

!+  Sub-Program Arguments
      character     Variable_Name*(*)  ! (INPUT) name of variable
      character     Variable_Value*(*) ! (INPUT) value of variable to set

!+  Purpose
!     The parsing routine has requested a set variable

!+  Changes
!     DPH 15/12/94
!      jngh 24/2/95 put in calls to assign string
!      jngh 07/06/96 changed set_ to post_
!     dph 12/7/96  added code to display line in summary file when setting apsim variable
!     dph 25/7/96  added message to summary file when creating a local variable
!     dph 2/10/96  replaced all calls to post_char_var to set_char_var.
!     sb 3/7/97  Trimmed args in both calls of set_char_var().
!     dph 10/2/98  called write_event instead of report_event - d097

!+  Calls
      character Lower_case*(Function_string_len)
                                       ! function

!+  Local Variables
      logical Is_apsim_variable        ! Is the requested variable APSIM's?
      integer numvals                  ! number of values returned.
      character Str*300                ! Dummy value returned by APSIM
      integer Variable_index           ! Index into local variable array
      character Mod_name*100           ! name of module owning variable
      character Var_name*100           ! name of variable

!- Implementation Section ----------------------------------
      variable_name = lower_case(variable_name)
 
      Is_apsim_variable = (index(variable_name, '.') .gt. 0)
      if (Is_apsim_variable) then
         call Split_line(variable_name, Mod_name, Var_name, '.')
         call set_char_var(Mod_name,
     .         trim(var_name), ' ',
     .         trim(Variable_value) )
 
      else
         ! Try to find variable in local variable list.
 
         Variable_index = find_string_in_array
     .      (variable_name, g%local_variable_names,
     :       g%num_local_variables)
 
         ! If not in local variable list then ask APSIM for it.  If
         ! APSIM doesn't know about it then add to local variable list.
 
         if (Variable_index .le. 0) then
            call Get_char_var_optional
     .          (Unknown_module, variable_name, '()',
     .           Str, Numvals)

            if (Numvals .eq. 0) then
               ! Add variable to local variable list.
 
               call manager_new_local_variable(variable_name,
     .              Variable_value)
 
               write (str, '(4a)' )
     .           'Manager creating a new local variable : ',
     .            trim(variable_name),
     .            ' = ',
     .            trim(Variable_value)
               call Write_event (str)
 
            else
               call set_char_var(Unknown_module,
     .            trim(variable_name), ' ',
     .            trim(Variable_value))
               Is_apsim_variable = .true.
            endif
         else
            call assign_string (
     :           g%local_variable_values(Variable_index)
     :         , Variable_value)
 
         endif
      endif
 
      if (Is_apsim_variable) then
         write (str, '(4a)' )
     .      'Manager setting apsim variable : ',
     .      trim(variable_name),
     .      ' = ',
     .      trim(Variable_value)
 
         call Write_string (0, str)
      endif
 
      return
      end



! ====================================================================
       subroutine Parse_action (Action_string)
! ====================================================================
      use ManagerModule
      implicit none
      include 'const.inc'              ! constant definitions
      include 'read.pub'                          
      include 'error.pub'                         
      include 'string.pub'                        
      include 'intrface.pub'                      
      include 'postbox.pub'

!+  Sub-Program Arguments
      character Action_string*(*)      ! (INPUT) ACtion to perform.

!+  Purpose
!     The parsing routine has requested some action of APSIM.

!+  Changes
!     DPH 15/12/94
!     jngh 24/2/95 changed data to data_string
!     DPH 5/7/95   put in check for a set command without an equals sign
!                  on it.  Produce fatal_error if this happens.
!     DPH 19/7/95  Added code to check for a queue keyword
!     DPH 27/5/96  Added code to check for a set action and to pass the
!                  variable name as data string.
!     dph 12/7/96  Added call to no_leading%spaces (Action) - fixes bug in manager
!     dph 25/7/96  Changed decl of no_leading%spaces*(mes_action_size) to
!                  no_leading%spaces*(function_string_len)
!     dph 30/8/99  removed 'queue' keyword

!+  Calls

!+  Local Variables
      integer Day                      ! Day number of year
      character Module_name*(30)       ! Module name to send action to
      character Action*(MES_Action_size)
                                       ! Action to send to APSIM
      character Data_string*(Function_string_len)
                                       ! Data string to send to APSIM
      character Variable_name*(Max_manager_var_name_size)
                                       ! variable name in set actions.
      integer Numvals                  ! Number of values returned
      integer Year                     ! Year number
      character Err*200                ! Error message
      logical Data_was_stored          ! Was data stored in postbox?

!- Implementation Section ----------------------------------
 
      if (index(Action_string, 'do_output') .eq. 0 .and.
     .    index(Action_string, 'do_end_day_output') .eq. 0) then
         call Get_integer_var
     .       (Unknown_module, 'day', '()', Day, numvals, 1, 366)
         call Get_integer_var
     .      (Unknown_module, 'year', '()', year, numvals,
     .               min_year, max_year)
 
         write (Data_string, '(a,i3,a,i4,2a)' )
     .      ' Day= ', Day, ' Year =  ', Year,
     .      '     Manager sending message :- ', Action_string
 
         call Write_string(LU_Summary_file, Data_string)
      endif
 
      call split_line (Action_string, Module_name, Data_string, Blank)
      Data_string = adjustl(Data_string)
      call split_line (Data_string, Action, Data_string, Blank)
      Action = adjustl(Action)
 
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
 
      call New_postbox ()
      Data_was_stored = Store_in_postbox (Data_string)
      if (Action .eq. 'set') then
         call Get_next_variable (Data_string,
     .                           Variable_name,
     .                           Data_string)
 
         Data_string = Variable_name
 
      else if (Data_was_stored) then
         Data_string = Blank
 
      endif
 
      call Action_send (Module_name, Action, Data_string)
      call Delete_postbox ()
 
      return
      end



! ====================================================================
       subroutine Parse_error (Error_message, Routine_message)
! ====================================================================
      use ManagerModule
      implicit none
      include 'const.inc'              ! constant definitions
      include 'error.pub'                         

!+  Sub-Program Arguments
      character Error_message*(*)      ! (INPUT) Error message to display
      character Routine_message*(*)    ! (INPUT) Routine name to display

!+  Purpose
!     The parsing routine has encountered an error.

!+  Changes
!     DPH 15/12/94
!     DPH 11/4/96  Added code to display a line number and file name
!                  when an error occured - commented it out - doesn't work
!                  because the parsing routine tokenises all manager rules
!                  from all files before validating the rules.

!+  Calls
!      include 'utility.inc'            ! needed for current line number and file
                                       ! unit number for error messages.

!+  Local Variables
!      character File_name*200          ! name of manager file.
!      character Our_error*(Function_string_len)
                                       ! our error message

!- Implementation Section ----------------------------------
 
      call Push_routine(Routine_message)
 
!      inquire (unit=Current_unit_num, name=File_name)
!      write (Our_error, '(6a, i3)')
!     .   Error_message,
!     .   New_line,
!     .   'Manager_file = ', File_name,
!     .   New_line,
!     .   'Line number  = ', Current_record_num
      call Fatal_error(ERR_user, Error_message)
 
      g%all_ok = NO
 
      call Pop_routine(Routine_message)
 
      return
      end



! =====================================================================
       subroutine Parse (Token_array, Token_array2)
! =====================================================================
      use ManagerModule
      implicit none

!+  Sub-Program Arguments
       character     Token_array(*)*(*)
       integer       Token_array2(*)

!+  Purpose
!     Parse a given array and perform the specified actions.

!+  Changes
!      TM - 21/11/94

!+  Local Variables
       integer       Nested_ifs           ! Number of nested statements

!- Implementation Section ----------------------------------
 
       Nested_ifs = 0
       g%end_of_file = NO
       g%next_token = g%start_token - 1
       if (g%next_token .lt. 0) then
          g%next_token = 0
       endif
 
10     continue
 
       if (g%end_of_file .eq. NO) then
          call   Get_next_token(Token_array, Token_array2)
 
          if     (g%token .eq. C_WORD) then
                 call Assignment_Statement(Token_array,Token_array2)
 
          elseif (g%token .eq. C_ACTION) then
                 call   Process_Action(Token_array, Token_array2)
 
          elseif (g%token .eq. C_IF) then
                 Nested_ifs = Nested_ifs + 1
                 call   Process_if_statement(Nested_ifs,Token_array,
     .                                                Token_array2)
 
          elseif (g%token .eq. C_ENDIF) then
                 Nested_ifs = Nested_ifs - 1
 
          elseif (g%token .eq. C_ELSE) then
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



! =====================================================================
       subroutine Process_if_statement (Nested_ifs, Token_array,
     .                                                   Token_array2)
! =====================================================================
      use ManagerModule
      implicit none

!+  Sub-Program Arguments
       integer       Nested_ifs           ! Number of nested statements
       character     Token_array(*)*(*)
       integer       Token_array2(*)

!+  Purpose
!     Process a single if statement.

!+  Changes
!      TM - 21/11/94

!+  Calls
       integer       If_statement         ! function

!+  Local Variables
       integer       This_Nested          ! Number of this nested if

!+  Initial Data Values
       integer       Last_Token           ! g%last g%token read

!- Implementation Section ----------------------------------
 
       call   Get_next_token(Token_array, Token_array2)
 
       if (If_statement(Token_array, Token_array2) .eq. 0) then
          if (g%all_ok .eq. YES) then
             This_Nested = Nested_ifs
             if (g%token .ne. C_THEN) then
                call   Parse_error('Missing then        ',
     .                             'Process_if_statement')
             endif
          endif
 
10        continue
          if (g%all_ok .eq. YES) then
 
             Last_Token = g%token
             call   Get_next_token(Token_array, Token_array2)
 
             if (g%token .eq. C_IF .and.
     .           Last_Token .eq. C_EOL) then
 
                Nested_ifs = Nested_ifs + 1
 
                goto 10
 
             elseif (g%token .eq. C_ELSE .and.
     .              Last_Token .eq. C_EOL) then
                 if (Nested_ifs .ne. This_Nested) then
                    goto 10
                 endif
 
             elseif (g%token .eq. C_ENDIF .and.
     .            Last_Token .eq. C_EOL) then
                 if (Nested_ifs .gt. This_Nested) then
                    Nested_ifs = Nested_ifs - 1
                    goto 10
                 endif
                 Nested_ifs = Nested_ifs - 1
 
             elseif (g%token .eq. C_EOF) then
                 if (Nested_ifs .gt. This_Nested) then
                 call   Parse_error('Missing endif       ',
     .                              'Process_if_statement')
 
                 endif
 
             else
                goto 10
             endif
          endif
       else
          if (g%all_ok .eq. YES) then
             if (g%token .ne. C_THEN) then
                 call   Parse_error('Missing then        ',
     .                              'Process_if_statement')
             endif
          endif
       endif
 
       return
       end



! =====================================================================
       subroutine Process_else_statement (Nested_ifs, Token_array,
     .                                                   Token_array2)
! =====================================================================
      use ManagerModule
      implicit none

!+  Sub-Program Arguments
       integer       Nested_ifs           ! Number of nested statements
       character     Token_array(*)*(*)
       integer       Token_array2(*)

!+  Purpose
!     Process the else part of an if-statement.

!+  Changes
!      TM - 21/11/94

!+  Local Variables
       integer       This_Nested          ! Number of this nested if

!- Implementation Section ----------------------------------
 
       This_Nested = Nested_ifs
10     continue
 
       call   Get_next_token(Token_array, Token_array2)
 
       if     (g%token .eq. C_IF) then
              Nested_ifs = Nested_ifs + 1
              goto 10
 
       elseif (g%token .eq. C_ENDIF) then
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



! =====================================================================
       subroutine Assignment_Statement (Token_array, Token_array2)
! =====================================================================
      use ManagerModule
      implicit none

!+  Sub-Program Arguments
       character     Token_array(*)*(*)
       integer       Token_array2(*)

!+  Purpose
!     Perform a given assignment.

!+  Changes
!      TM - 21/11/94
!      TM - 29/09/95  Took out call to Action (processed in Parse)

!+  Local Variables
       character     Variable_name*(Buffer_size)
                                          ! Variable to assign a value

!- Implementation Section ----------------------------------
 
       Variable_name = g%buffer
 
       call   Get_next_token(Token_array, Token_array2)
 
       if     (g%token .eq. C_EQUAL) then
              call   Process_Assignment(Variable_name, Token_array,
     .                                                  Token_array2)
 
       else
              call   Parse_error('Syntax error        ',
     .                           'Assignment_Statement')
 
       endif
 
       return
       end



! =====================================================================
       subroutine Process_Assignment (Variable_name, Token_array,
     .                                                    Token_array2)
! =====================================================================
      use ManagerModule
      implicit none
      include 'string.pub'                        

!+  Sub-Program Arguments
       character     Variable_name*(Buffer_size)
                                          ! Variable to assign a value
       character     Token_array(*)*(*)
       integer       Token_array2(*)

!+  Purpose
!     Assign a value to a variable.

!+  Changes
!      TM - 21/11/94
!      jngh 24/2/95 put in calls to assign string

!+  Local Variables
       character     Variable_value*(Buffer_size)
                                          ! value to assign the variable

!- Implementation Section ----------------------------------
 
       call   Get_next_token(Token_array, Token_array2)
       g%number_expressions = 1
       call assign_string (g%expression_array(g%number_expressions)
     :                   , g%buffer)
       g%expression_array2(g%number_expressions) = g%token
       call   Get_next_token (Token_array, Token_array2)
 
10     continue
       if     (g%token .ne. C_EOL) then
              g%number_expressions = g%number_expressions + 1
              call assign_string
     :            (g%expression_array(g%number_expressions), g%buffer)
              g%expression_array2(g%number_expressions) = g%token
              call   Get_next_token(Token_array, Token_array2)
              goto 10
       endif
       g%expression_array2(g%number_expressions+1) = C_END
 
       call   Process_expression
 
       if (g%all_ok .eq. YES) then
              call assign_string (Variable_value, g%expression_result)
              call   Parse_set_variable(Variable_Name, Variable_Value)
       endif
 
       return
       end



! =====================================================================
       subroutine Process_Action (Token_array, Token_array2)
! =====================================================================
      use ManagerModule
      implicit none
       include 'const.inc'

!+  Sub-Program Arguments
       character     Token_array(*)*(*)
       integer       Token_array2(*)

!+  Purpose
!     Perform a given action.

!+  Changes
!      TM - 21/11/94
!      TM - 29/09/95 - changed action to be handled as one token

!- Implementation Section ----------------------------------
 
       call   Parse_action (g%buffer)
 
       call   Get_next_token (Token_array, Token_array2)
 
       return
       end



! =====================================================================
       integer function If_statement(Token_array, Token_array2)
! =====================================================================
      use ManagerModule
      implicit none
      include 'datastr.pub'                       

!+  Sub-Program Arguments
       character     Token_array(*)*(*)
       integer       Token_array2(*)

!+  Purpose
!     Calculate the expression in an if statement.

!+  Changes
!      TM - 21/11/94

!+  Local Variables
       integer       If_result
       integer       NumVals

!- Implementation Section ----------------------------------
 
       g%number_and_or        = 0
       g%number_expressions   = 0
       g%word_or_number       = NO
 
10     continue
       call   Get_expression_array(Token_array, Token_array2)
 
20     continue
 
       if (g%all_ok .eq. YES) then
              call   Process_expression
       endif
 
       if (g%all_ok .eq. YES) then
 
              if     (g%save_token .ne. C_THEN) then
                     call   Process_next_expression(Token_array,
     .                                                Token_array2)
                     goto 10
              endif
 
              if     (g%number_and_or .gt. 0) then
                     call   Process_And_Or_expression
                     goto 20
              endif
 
              g%token = g%save_token
 
              call string_to_integer_var (g%expression_result,
     :                                   If_result, NumVals)
 
              If_statement = If_result
       endif
 
       return
       end



! =====================================================================
       subroutine Process_next_expression (Token_array, Token_array2)
! =====================================================================
      use ManagerModule
      implicit none
      include 'string.pub'                        

!+  Sub-Program Arguments
       character     Token_array(*)*(*)
       integer       Token_array2(*)

!+  Purpose
!     Process the next part of an expression.

!+  Changes
!      TM - 21/11/94

!+  Local Variables
       integer       ind                  ! loop index

!- Implementation Section ----------------------------------
 
       g%number_expressions = g%number_expressions + 1
 
       g%expression_array2(g%number_expressions) = g%save_token
 
       if (g%save_token .eq. C_AND .or.
     :     g%save_token .eq. C_OR) then
 
          do 10  ind = g%number_and_or + 1
     .               , g%number_and_or + g%number_expressions
             call assign_string (g%and_or_array(ind)
     .                   , g%expression_array(ind - g%number_and_or))
             g%and_or_array2(ind) =
     .                     g%expression_array2(ind - g%number_and_or)
10        continue
 
          g%number_and_or = g%number_and_or + g%number_expressions
          g%number_expressions = 0
       endif
 
       g%save_token = 0
       call   Get_next_token(Token_array, Token_array2)
 
       return
       end



! =====================================================================
       subroutine Process_And_Or_expression ()
! =====================================================================
      use ManagerModule
      implicit none
      include 'string.pub'                        

!+  Purpose
!     Process the AND/C_OR part of an expression.

!+  Changes
!      TM - 21/11/94

!+  Local Variables
       integer       ind                  ! loop index

!- Implementation Section ----------------------------------
 
      do 10  ind = g%number_and_or + 1
     :           , g%number_and_or + g%number_expressions
         call assign_string (g%and_or_array(ind)
     :                     , g%expression_array(ind-g%number_and_or))
         g%and_or_array2(ind)=g%expression_array2(ind-g%number_and_or)
10    continue
 
       g%number_and_or = g%number_and_or + g%number_expressions
 
       do 20  ind = 1, g%number_and_or
          call assign_string (g%expression_array(ind)
     :                      , g%and_or_array(ind))
          g%expression_array2(ind) = g%and_or_array2(ind)
20     continue
 
       g%expression_array2(g%number_and_or+2) = C_END
       g%number_expressions = g%number_and_or
       g%number_and_or = 0
 
       return
       end



! =====================================================================
       subroutine Process_expression ()
! =====================================================================
      use ManagerModule
      implicit none
      include 'string.pub'                        

!+  Purpose
!     Process the calculations in the given expression.

!+  Changes
!      TM - 21/11/94
!      jngh 24/2/95 put in calls to assign string

!+  Calls
       character     pop_stack*(Buffer_size)
                                          ! function

!+  Local Variables
       integer       ind                  ! loop index
       integer       ind2                 ! loop index
       integer       left                 ! position of the left parent
       integer       right                ! position of the right parent

!- Implementation Section ----------------------------------
 
20     continue
       left  = 0
       right = 0
 
       do 30  ind = 1, g%number_expressions
          if (right .eq. 0) then
             if (g%expression_array2(ind) .eq. C_LEFT_PAREN) then
                left = ind
             elseif (g%expression_array2(ind) .eq. C_RIGHT_PAREN) then
                right = ind
             endif
          endif
30     continue
 
       if (left .gt. 0 .and. right .gt. 0) then
          g%number_of_tokens = right - left - 1
          do 40  ind = 1, g%number_of_tokens
             call assign_string (g%expression_sub_array(ind)
     :                         , g%expression_array(ind+left))
             g%expression_sub_array2(ind) =
     :                                 g%expression_array2(ind+left)
40        continue
          g%expression_sub_array2(g%number_of_tokens+1) = C_END
 
          call assign_string (g%buffer, g%expression_sub_array(1))
          g%token = g%expression_sub_array2(1)
          g%current_token = 1
 
          call Process_sub_expression()
 
          if (g%all_ok .eq. YES) then
             if (left .eq. 0) then
                left = 1
             endif
 
             g%expression_result = pop_stack()
 
          end if
 
          if (g%all_ok .eq. YES) then
              call assign_string (g%expression_array(left)
     :                         ,  g%expression_result)
              g%expression_array2(left) = C_NUMBER
 
              ind2 = 0
              do 50  ind = right+1, g%number_expressions
                 ind2 = ind2 + 1
                 call assign_string (g%expression_array(left+ind2)
     .                             , g%expression_array(ind))
                 g%expression_array2(left+ind2) =
     .                            g%expression_array2(ind)
50           continue
             g%number_expressions =
     .              g%number_expressions - (right - left)
             goto 20
          endif
       else
 
          g%number_of_tokens = g%number_expressions - left
 
          do 60  ind = 1, g%number_of_tokens
             call assign_string (g%expression_sub_array(ind)
     :                         , g%expression_array(ind+left))
             g%expression_sub_array2(ind) =
     :                             g%expression_array2(ind+left)
60        continue
          g%expression_sub_array2(g%number_of_tokens+1) = C_END
 
          call assign_string (g%buffer, g%expression_sub_array(1))
          g%token = g%expression_sub_array2(1)
          g%current_token = 1
 
          call Process_sub_expression()
 
          if (g%all_ok .eq. YES) then
             call assign_string (g%expression_result, pop_stack())
          endif
 
          if (g%all_ok .eq. YES) then
             do 70  ind = 1, left
                g%expression_array2(ind) = C_LEFT_PAREN
70           continue
 
             call assign_string (g%expression_array(left+1)
     :                         , g%expression_result)
             g%expression_array2(left+1) = C_NUMBER
 
             if (right .gt. 0) then
                do 80  ind = 1, g%number_expressions+1-right
                      g%expression_array2(ind+1) = C_RIGHT_PAREN
80              continue
 
                g%number_expressions = g%number_expressions + 2-right
             else
                g%number_expressions = left + 1
             endif
 
             g%expression_array2(g%number_expressions+1) = C_END
          endif
       endif
 
       return
       end



! =====================================================================
       subroutine Str_to_double_var(String, Double_value, io_result)
! =====================================================================
      implicit none

!+  Sub-Program Arguments
      character String*(*)             ! (INPUT) String to convert
      double precision Double_value    ! (OUTPUT) Value of string
      integer IO_result                ! (OUTPUT) io_result of internal read.

!+  Purpose
!     Convert a string value to a double number.

!+  Notes
!     We created this routine because we don't want an error message when
!     the string cannot be converted to a real number.

!+  Changes
!     DPH 1/8/95
!     dph 24/6/96 Changed routine from a real routine to a double routine

!- Implementation Section ----------------------------------
 
      read (String, '(g25.0)',iostat = io_result) Double_value
 
      return
      end



! =====================================================================
       subroutine Process_sub_expression ()
! =====================================================================
      use ManagerModule
      implicit none
      include 'data.pub'                          
      include 'string.pub'                        

!+  Purpose
!     Process the comparing part of an expression.

!+  Changes
!      TM - 21/11/94
!      jngh 24/2/95 put in calls to assign string and reals_are_equal
!      dph 24/6/96  changed from using reals to double precision for temps

!+  Calls
       character     pop_stack*(Buffer_size)
                                          ! function

!+  Local Variables
       integer       operator             ! save the operator
       character     operand_1*(Buffer_size)
       character     operand_2*(Buffer_size)
       double precision Temp_1, Temp_2
       integer       io_result1, io_result2
                                          ! check for reals

!- Implementation Section ----------------------------------
 
       call   Process_Simple_Expression
 
       if (g%token .eq. C_EQUAL         .or.
     :     g%token .eq. C_LESS_THAN     .or.
     .     g%token .eq. C_LESS_EQUAL    .or.
     :     g%token .eq. C_GREATER_THAN  .or.
     .     g%token .eq. C_GREATER_EQUAL .or.
     :     g%token .eq. C_NOT_EQUAL)    then
 
          operator = g%token
 
          call   Get_sub_token
          call   Process_Simple_Expression
 
          call assign_string (operand_2, pop_stack())
          call assign_string (operand_1, pop_stack())
 
          call Str_to_double_var(Operand_1, Temp_1, io_result1)
          call Str_to_double_var(Operand_2, Temp_2, io_result2)
 
          if (io_result1 .eq. 0 .and. io_result2 .eq. 0) then
            if (g%all_ok .eq. YES) then
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
            if (g%all_ok .eq. YES) then
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



! =====================================================================
       subroutine Process_Simple_Expression ()
! =====================================================================
      use ManagerModule
      implicit none
      include 'data.pub'                          
      include 'datastr.pub'                       
      include 'string.pub'                        

!+  Purpose
!     Process the add/minus/and part of an expression.

!+  Changes
!      TM - 21/11/94
!      jngh 24/2/95 put in calls to assign string and reals_are_equal
!      dph  24/6/96 changed data type to doubles for all calculations.

!+  Calls
       character     pop_stack*(Buffer_size)
                                          ! function

!+  Local Variables
       integer       operator             ! save the operator
       character     Temp_operand*(Buffer_size)
       double precision  Temp_1, Temp_2
       character     operand_1*(Buffer_size)
       character     operand_2*(Buffer_size)
       integer       numvals

!- Implementation Section ----------------------------------
 
       call   Process_Term
 
10     continue
       if (g%token .eq. C_PLUS  .or.
     :     g%token .eq. C_MINUS .or.
     .     g%token .eq. C_AND)  then
 
          operator = g%token
 
          call  Get_sub_token
          call  Process_Term
 
          call assign_string (operand_2, pop_stack())
          call assign_string (operand_1, pop_stack())
 
          if (g%all_ok .eq. YES) then
             if (operator .eq. C_PLUS) then
                call string_to_double_var(Operand_1, Temp_1, numvals)
                call string_to_double_var(Operand_2, Temp_2, numvals)
                call Double_var_to_string(Temp_1 + Temp_2, Temp_operand)
                call   push_stack(Temp_operand)
 
             elseif (operator .eq. C_MINUS) then
                call string_to_double_var(Operand_1, Temp_1, numvals)
                call string_to_double_var(Operand_2, Temp_2, numvals)
                call Double_var_to_string(Temp_1 - Temp_2, Temp_operand)
                call push_stack(Temp_operand)
 
             elseif (operator .eq. C_AND) then
                call string_to_double_var(Operand_1, Temp_1, numvals)
                call string_to_double_var(Operand_2, Temp_2, numvals)
 
                if (doubles_are_equal (Temp_1, 1.0d0) .and.
     .              doubles_are_equal (Temp_2, 1.0d0)) then
                   call push_stack('1.0')
                else
                   call push_stack('0.0')
                endif
             endif
 
             if (g%all_ok .eq. YES) then
                goto 10
             endif
          endif
       endif
 
       return
       end



! =====================================================================
       subroutine Process_Term ()
! =====================================================================
      use ManagerModule
      implicit none
      include 'data.pub'                          
      include 'datastr.pub'                       
      include 'string.pub'                        

!+  Purpose
!     Process the mult/div/power/or part of an expression.

!+  Changes
!      TM - 21/11/94
!      jngh 24/2/95 put in calls to assign string and reals_are_equal
!      dph  24/6/96 changed data type to doubles for all calculations.

!+  Calls
       character     pop_stack*(Buffer_size)
                                          ! function

!+  Local Variables
       integer       operator             ! save the operator
       character     Temp_operand*(Buffer_size)
       double precision Temp_1, Temp_2
       character     operand_1*(Buffer_size)
       character     operand_2*(Buffer_size)
       integer       numvals

!- Implementation Section ----------------------------------
 
       call   Process_Factor
 
 
20     continue
       if (g%token .eq. C_MULTIPLY .or.
     :     g%token .eq. C_DIVIDE   .or.
     .     g%token .eq. C_POWER    .or.
     :     g%token .eq. C_OR)      then
           operator = g%token
 
          call  Get_sub_token
          call  Process_Factor
 
          call assign_string (operand_2, pop_stack())
          call assign_string (operand_1, pop_stack())
 
 
          if (g%all_ok .eq. YES) then
             if (operator .eq. C_MULTIPLY) then
                call string_to_double_var(Operand_1, Temp_1, numvals)
                call string_to_double_var(Operand_2, Temp_2, numvals)
                call Double_var_to_string(Temp_1 * Temp_2, Temp_operand)
                call   push_stack(Temp_operand)
 
             elseif (operator .eq. C_DIVIDE) then
                call string_to_double_var(Operand_1, Temp_1, numvals)
                call string_to_double_var(Operand_2, Temp_2, numvals)
 
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
                call string_to_double_var(Operand_1, Temp_1, numvals)
                call string_to_double_var(Operand_2, Temp_2, numvals)
                call Double_var_to_string(Temp_1 ** Temp_2,
     .                                    Temp_operand)
                call push_stack(Temp_operand)
 
             elseif (operator .eq. C_OR) then
                call string_to_double_var(Operand_1, Temp_1, numvals)
                call string_to_double_var(Operand_2, Temp_2, numvals)
                if (Doubles_are_equal (Temp_1, 1.0d0) .or.
     .              Doubles_are_equal (Temp_2, 1.0d0)) then
                   call push_stack('1.0')
                else
                   call push_stack('0.0')
                endif
             endif
 
             if (g%all_ok .eq. YES) then
                goto 20
             endif
          endif
       endif
 
       return
       end



! =====================================================================
       subroutine Process_Factor ()
! =====================================================================
      use ManagerModule
      implicit none
      include 'string.pub'                        

!+  Purpose
!     Get the value to push on the stack.

!+  Changes
!      TM - 21/11/94

!+  Calls
       character     Real_or_not*(Buffer_size)

!+  Local Variables
       character     Variable_value*(Buffer_size)
                                          ! Value to push on g%stack
       character     Temp*(Buffer_size)

!- Implementation Section ----------------------------------
 
       if (g%token .eq. C_WORD) then
          call   Parse_get_variable(g%buffer, Variable_Value)
 
          call assign_string (Temp, Real_or_not(Variable_Value))
 
          call   push_stack(Temp)
 
          call   Get_sub_token
 
       elseif (g%token .eq. C_NUMBER) then
 
          call assign_string (Temp, Real_or_not(g%buffer))
          call   push_stack(Temp)
 
          call   Get_sub_token
 
       elseif (g%token .eq. C_LITERAL) then
          call   push_stack(g%buffer)
 
          call   Get_sub_token
 
       endif
 
 
       return
       end



! =====================================================================
       subroutine push_stack (Variable_Value)
! =====================================================================
      use ManagerModule
      implicit none
      include 'string.pub'                        

!+  Sub-Program Arguments
       character     Variable_Value*(*) ! (INPUT) Value to push on g%stack

!+  Purpose
!      Add a value to the top of the stack.

!+  Changes
!      TM - 21/11/94
!      jngh 24/2/95 put in calls to assign string

!- Implementation Section ----------------------------------
 
       g%number_of_variables = g%number_of_variables + 1
       if (g%number_of_variables .gt. Variable_maximum) then
          call   Parse_error('Too many variables  ',
     .                       'push_stack          ')
       else if (g%number_of_variables .gt. 0) then
          call assign_string (g%stack(g%number_of_variables)
     :                      , Variable_Value)
       else
         ! we have a problem elsewhere
       endif
 
 
       return
       end



! =====================================================================
       character*(*) function pop_stack ()
! =====================================================================
      use ManagerModule
      implicit none
      include 'string.pub'                        

!+  Purpose
!     Get the string off the top of the stack.

!+  Changes
!      TM - 21/11/94
!      jngh 24/2/95 put in calls to assign string

!- Implementation Section ----------------------------------
 
       g%number_of_variables = g%number_of_variables - 1
 
       if (g%number_of_variables .lt. 0) then
          call   Parse_error('Too few variables   ',
     .                       'pop_stack           ')
       else if (g%number_of_variables .le. Variable_maximum) then
          call assign_string (pop_stack
     :                      , g%stack(g%number_of_variables + 1))
       else
         ! we have a problem elsewhere
       endif
 
 
       return
       end



! =====================================================================
       subroutine Get_sub_token ()
! =====================================================================
      use ManagerModule
      implicit none
      include 'string.pub'                        

!+  Purpose
!     Get the next token off the sub array.

!+  Changes
!      TM - 21/11/94
!      jngh 24/2/95 put in calls to assign string

!- Implementation Section ----------------------------------
 
       g%current_token = g%current_token + 1
       if     (g%current_token .gt. g%number_of_tokens+1) then
              call   Parse_error('Too many tokens     ',
     .                           'Get_sub_token       ')
       endif
 
       call assign_string (g%buffer
     :                   , g%expression_sub_array(g%current_token))
       g%token = g%expression_sub_array2(g%current_token)
 
 
       return
       end



! =====================================================================
       subroutine Get_next_token (Token_array, Token_array2)
! =====================================================================
      use ManagerModule
      implicit none
      include 'string.pub'                        

!+  Sub-Program Arguments
       character     Token_array(*)*(*)
       integer       Token_array2(*)

!+  Purpose
!     Get the next token of the g%token array.

!+  Changes
!      TM - 21/11/94
!      jngh 24/2/95 put in calls to assign string

!- Implementation Section ----------------------------------
 
       g%next_token = g%next_token + 1
 
       call assign_string (g%buffer, Token_array(g%next_token))
       g%token = Token_array2(g%next_token)
 
       if     (g%token .eq. C_EOF) then
              g%end_of_file = YES
       endif
 
 
       return
       end



! =====================================================================
       subroutine Get_expression_array (Token_array, Token_array2)
! =====================================================================
      use ManagerModule
      implicit none
      include 'string.pub'                        

!+  Sub-Program Arguments
       character     Token_array(*)*(*)
       integer       Token_array2(*)

!+  Purpose
!     Put all tokens in expression into expression array.

!+  Changes
!      TM - 21/11/94
!      jngh 24/2/95 put in calls to assign string

!+  Local Variables
!   Constant values*      none

!- Implementation Section ----------------------------------
 
       if (g%token .ne. C_EOL) then
          call   Check_previous_word
 
          g%number_expressions = g%number_expressions + 1
          call assign_string (g%expression_array(g%number_expressions)
     :                      , g%buffer)
          g%expression_array2(g%number_expressions) = g%token
          call   Get_next_token(Token_array, Token_array2)
       endif
 
10     continue
       if (g%all_ok .eq. YES) then
          if (g%token .eq. C_WORD        .or.
     :        g%token .eq. C_NUMBER      .or.
     .        g%token .eq. C_PLUS        .or.
     :        g%token .eq. C_MINUS       .or.
     .        g%token .eq. C_MULTIPLY    .or.
     :        g%token .eq. C_DIVIDE      .or.
     .        g%token .eq. C_POWER       .or.
     :        g%token .eq. C_LEFT_PAREN  .or.
     .        g%token .eq. C_RIGHT_PAREN .or.
     :        g%token .eq. C_LITERAL)    then
 
              call   Check_previous_word
              g%number_expressions = g%number_expressions + 1
              call assign_string (
     :             g%expression_array(g%number_expressions), g%buffer)
              g%expression_array2(g%number_expressions) = g%token
 
              call   Get_next_token(Token_array, Token_array2)
              goto   10
          endif
 
          if (g%token .eq. C_EOL) then
             call   Get_next_token(Token_array, Token_array2)
             goto   10
          endif
 
          if (g%token .eq. C_EQUAL          .or.
     :        g%token .eq. C_NOT_EQUAL      .or.
     :        g%token .eq. C_LESS_THAN      .or.
     .        g%token .eq. C_LESS_EQUAL     .or.
     :        g%token .eq. C_GREATER_THAN   .or.
     .        g%token .eq. C_GREATER_EQUAL  .or.
     :        g%token .eq. C_AND            .or.
     .        g%token .eq. C_OR             .or.
     :        g%token .eq. C_THEN)          then
 
             call   Check_previous_word
             g%save_token = g%token
          endif
       endif
 
 
       return
       end



! =====================================================================
       subroutine Check_previous_word ()
! =====================================================================
      use ManagerModule
      implicit none

!+  Purpose
!     Make sure you don't have two operators
!     or two variables next to each other.

!+  Changes
!      TM - 21/11/94

!- Implementation Section ----------------------------------
 
       if (g%token .eq. C_WORD    .or.
     :     g%token .eq. C_NUMBER  .or.
     .     g%token .eq. C_LITERAL) then
          if (g%word_or_number .eq. YES) then
             call Parse_error('Missing operator    ',
     .                        'Get_expression_array')
          else
             g%word_or_number = YES
          endif
       else
          if (g%token .ne. C_LEFT_PAREN  .and.
     :        g%token .ne. C_RIGHT_PAREN) then
             if (g%word_or_number .eq. NO) then
                call Parse_error('Missing identifier  ',
     .                           'Get_expression_array')
             else
                g%word_or_number = NO
             endif
          endif
       endif
 
 
       return
       end



! =====================================================================
       character*(*) function Real_or_not (Variable_Value)
! =====================================================================
      implicit none
      include 'datastr.pub'                       

!+  Sub-Program Arguments
       character*(*) Variable_Value

!+  Purpose
!     check to see if the value is a real
!     then return the resulting real or not.

!+  Changes
!      TM - 21/11/94

!+  Local Variables
       double precision Temp
       integer          Double_flag

!- Implementation Section ----------------------------------
 
       call Str_to_double_var(Variable_value, Temp, Double_flag)
 
       if     (Double_flag .eq. 0) then
              call Real_var_to_string(real(Temp), Variable_value)
       endif
 
 
       Real_or_not = Variable_Value
 
       return
       end



! =====================================================================
       subroutine Tokenize (Token_array, Token_array2, maxtokens)
! =====================================================================
      use ManagerModule
      implicit none
       include 'const.inc'
      include 'string.pub'                        
      include 'error.pub'                         

!+  Sub-Program Arguments
       character     Token_array(*)*(*)
       integer       Token_array2(*)
       integer       maxtokens

!+  Purpose
!      Read a file token by token
!      and put them into an array.

!+  Changes
!      TM - 21/11/94
!      jngh 24/2/95 put in calls to assign string
!      TM - 23/07/95 put in work around for elseifs
!     TM - 29/09/95 put in fix to check for negative numbers
!                    and get Action Strings as one Token
!     sb - 19/11/97 put in fix to ensure there is a c_eol before c_eof

!+  Calls
       character     string_concat*(500) ! function

!+  Local Variables
       integer       ind                  ! loop index
       integer       count                ! loop index
       integer       elseif_count         !
       integer       if_count             !

!- Implementation Section ----------------------------------
 
       g%first = 0
       g%last  = 0
       g%end_of_file = 0
       elseif_count = 0
       if_count = 0
 
       ind = g%start_token - 1
       if (ind .lt. 0) then
          ind = 0
       endif
       call   Get_Char()
 
10     continue
 
       if (ind .ge. maxtokens-1) then
          call fatal_error (err_internal, 'Token array limit exceeded')
 
       else
          call   Get_Token_from_file()
 
          if   (g%token .eq. C_IF .and. elseif_count .gt. 0) then
               if_count = if_count + 1
          endif
 
          if   (g%token .eq. C_ENDIF .and. elseif_count .gt. 0) then
               if  (if_count .gt. 0) then
                   if_count = if_count - 1
               else
                   do 20  count = 1, elseif_count
                      g%token = C_ENDIF
                      g%buffer = 'endif'
                      ind = ind + 1
                      call assign_string(Token_array(ind),g%buffer)
                      Token_array2(ind) = g%token
20                 continue
                   elseif_count = 0
               endif
          endif
 
          if   (g%token .eq. C_ELSEIF) then
               elseif_count  = elseif_count + 1
               g%token = C_ELSE
               g%buffer = 'else'
               ind = ind + 1
               call assign_string (Token_array(ind), g%buffer)
               Token_array2(ind) = g%token
               g%token = C_IF
               g%buffer = 'if'
          endif
 
          if   (g%token .eq. C_NUMBER .and. ind .ge. 2 .and.
     :           Token_array2(ind) .eq. C_MINUS .and.
     :           Token_array2(ind-1) .ne. C_NUMBER .and.
     :           Token_array2(ind-1) .ne. C_WORD) then
 
                 call assign_string (g%buffer, '-'//g%buffer)
                 ind = ind -1
        endif
 
          if   (ind .ge. 1 .and. g%token .eq. C_WORD .and.
     :          Token_array2(ind) .eq. C_WORD) then
 
                g%buffer = string_concat (Token_Array(ind),
     :                                          ' '//g%buffer)
               call Get_Action()
               g%token = C_ACTION
               ind = ind - 1
          endif
 
          ind = ind + 1
          call assign_string (Token_array(ind), g%buffer)
          Token_array2(ind) = g%token
 
          if     (g%end_of_file .eq. 0) then
              goto 10
          endif
       endif
 
       if (token_array2(ind) .ne. c_eol) then
          ind = ind+1
          token_array2(ind) = c_eol
       end if
       Token_array2(ind+1) = C_EOF
 
       g%last_token = ind
       return
       end



! =====================================================================
       subroutine Get_Token_from_file ()
! =====================================================================
      use ManagerModule
      implicit none

!+  Purpose
!     Get the next token from the manager file.

!+  Changes
!      TM - 21/11/94
!     DPH - 12/4/96   - Added check for quote character
!     DPH - 3/6/96    - Removed check for quote character - not needed.

!- Implementation Section ----------------------------------
 
10     continue
 
       if     (g%ch .ge. 'a' .and. g%ch .le. 'z') then
              call   Get_Word()
 
       elseif (g%ch .ge. '0' .and. g%ch .le. '9') then
              call   Get_Number()
 
       elseif (g%ch .eq. '''') then
              call Get_Literal()
 
       else
              call   Get_Special()
              if     (g%token .eq. C_SPACE) then
                     goto 10
              end if
 
       endif
 
       return
       end



! =====================================================================
       subroutine Get_Char ()
! =====================================================================
      use ManagerModule
      implicit none
      include 'string.pub'                        

!+  Purpose
!     Get the next character from the manager file.

!+  Changes
!      TM - 21/11/94

!+  Calls

!- Implementation Section ----------------------------------
 
       g%first = g%first + 1
       if     (g%first .gt. g%last) then
           call    assign_string (g%last_line, g%line)
              call   Parse_read_line (g%line, g%end_of_file)
              g%line = adjustl(g%line)
              g%first = 0
              g%last = len_trim(g%line)
              g%ch = ';'
       else
              g%ch = g%line(g%first:g%first)
       end if
 
       return
       end



! =====================================================================
       subroutine Get_Word ()
! =====================================================================
      use ManagerModule
      implicit none
      include 'string.pub'                        

!+  Purpose
!     Get the next word token from the manager file.

!+  Changes
!      TM - 21/11/94*      TM - 05/10/95 - added check for left bracket
!     DPH - 12/4/96  - added check for strings in quotes.
!     JNGH - 23/4/98 - added % character to variable name list

!+  Calls
       character     string_concat*(Buffer_size)
                                          ! function
       logical       Reserved             ! function

!+  Local Variables
       integer       left                 ! left brackets
       logical       Inside_quotes        ! Are we currently inside quotes?

!- Implementation Section ----------------------------------
       if (g%ch .eq. '''') then
          Inside_quotes = .true.
       else
          g%buffer = g%ch
          Inside_quotes = .false.
       endif
 
       left = 0
 
10     continue
 
       call   Get_Char()
 
      if (g%ch .eq. '''') then
         if (Inside_quotes) then
            Inside_quotes = .false.
         else
            Inside_quotes = .true.
         endif
         goto 10
 
      else if (Inside_quotes .or.
     .     (g%ch .ge. 'a' .and. g%ch .le. 'z') .or.
     .      g%ch .eq. '.' .or.
     .     (g%ch .ge. '0' .and. g%ch .le. '9') .or.
     :     (g%ch .eq. '_'     .or.
     :      g%ch .eq. '%'     .or.
     .      g%ch .eq. '['     .or.
     :      g%ch .eq. ']'     .or.
     :      g%ch .eq. '(')     .or.
     .      (g%ch .eq. ')' .and. left .gt. 0))   then
 
          g%buffer = string_concat(g%buffer, g%ch)
 
          if  (g%ch .eq. '(') then
               left = left + 1
          endif
 
          if  (g%ch .eq. ')') then
               left = left - 1
          endif
 
          goto 10
       endif
 
 
 
       if     (Reserved()) then
              ! reserved word
       else
              g%token = C_WORD
       endif
 
       return
       end



! =====================================================================
       subroutine Get_Literal ()
! =====================================================================
      use ManagerModule
      implicit none
      include 'string.pub'                        

!+  Purpose
!     Get the next word in quotes from the manager file.

!+  Changes
!      TM - 06/12/94

!+  Calls
       character     string_concat*(Buffer_size)
                                          ! function

!- Implementation Section ----------------------------------
 
       g%buffer = ' '
 
10     continue
 
       call   Get_Char()
 
       if (g%ch .ne. '''') then
              g%buffer = string_concat (g%buffer, g%ch)
              goto 10
       endif
 
       call   Get_Char()
 
       g%token = C_LITERAL
 
 
       return
       end



! =====================================================================
       subroutine Get_Number ()
! =====================================================================
      use ManagerModule
      implicit none
      include 'string.pub'                        

!+  Purpose
!     Get the next number token from the manager file.

!+  Changes
!      TM - 21/11/94

!+  Calls
       character     string_concat*(Buffer_size)
                                          ! function

!- Implementation Section ----------------------------------
 
 
       g%buffer = g%ch
10     continue
 
       call Get_Char()
 
       if ((g%ch .ge. '0' .and. g%ch .le. '9')  .or.
     :     (g%ch .eq. '.'))                     then
              g%buffer = string_concat (g%buffer, g%ch)
              goto 10
       end if
 
 
       g%token = C_NUMBER
 
       return
       end



! =====================================================================
       subroutine Get_Special ()
! =====================================================================
      use ManagerModule
      implicit none
      include 'const.inc'
      include 'string.pub'                        
      include 'error.pub'                         

!+  Purpose
!     Get the next special token from the manager file.

!+  Changes
!      TM - 21/11/94
!      JNGH - 23/4/98 added warning error when special character found

!+  Local Variables
      character str*200
      integer   i

!- Implementation Section ----------------------------------
 
       g%buffer = g%ch
 
 
       if (g%ch .eq. '-') then
              g%token = C_MINUS
              call Get_Char()
 
       elseif (g%ch .eq. '+') then
              g%token = C_PLUS
              call Get_Char()
 
       elseif (g%ch .eq. '^') then
              g%token = C_POWER
              call Get_Char()
 
       elseif (g%ch .eq. '*') then
              g%token = C_MULTIPLY
              call Get_Char()
              if (g%ch .eq. '*') then
                     g%buffer = '**'
                     g%token = C_POWER
                     call Get_Char()
              endif
 
       elseif (g%ch .eq. '/') then
              g%token = C_DIVIDE
              call Get_Char()
 
       elseif (g%ch .eq. '=') then
              g%token = C_EQUAL
              call Get_Char()
 
       elseif (g%ch .eq. '<') then
              g%token = C_LESS_THAN
              call Get_Char()
              if (g%ch .eq. '>') then
                     g%buffer = '<>'
                     g%token = C_NOT_EQUAL
                     call Get_Char()
              elseif (g%ch .eq. '=') then
                     g%buffer = '<='
                     g%token = C_LESS_EQUAL
                     call Get_Char()
              endif
 
       elseif (g%ch .eq. '>') then
              g%token = C_GREATER_THAN
              call Get_Char()
              if (g%ch .eq. '=') then
                     g%buffer = '>='
                     g%token = C_GREATER_EQUAL
                     call Get_Char()
              endif
 
       elseif (g%ch .eq. '(') then
              g%token = C_LEFT_PAREN
              call Get_Char()
 
       elseif (g%ch .eq. ')') then
              g%token = C_RIGHT_PAREN
              call Get_Char()
 
       elseif (g%ch .eq. ';') then
              g%token = C_EOL
              call Get_Char()
 
       elseif (g%ch .eq. ' ') then
              g%token = C_SPACE
              call Get_Char()
 
       else
              g%token = C_SPECIAL
 
             write (str, '(200a)' )
     .      'Cannot use character "',
     :      g%ch,
     :      '" where it is indicated in line',
     :      new_line,
     :      trim(g%line),
     :      new_line,
     :      (blank, i=1,g%first-1), '^'
 
            call Warning_error(ERR_user, str)
 
 
              call Get_Char()
 
       endif
 
 
       return
       end



! =====================================================================
       subroutine Get_Action ()
! =====================================================================
      use ManagerModule
      implicit none
      include 'string.pub'                        

!+  Purpose
!     Get the entire line from the manager file.

!+  Changes
!      TM - 29/09/95

!- Implementation Section ----------------------------------
 
       if (g%ch .eq. ';') then
          call assign_string (g%buffer, g%last_line)
      else
          call assign_string (g%buffer, g%line)
       endif
 
 
10     continue
       if (g%ch .ne. ';') then
          call   Get_Char()
          goto 10
       endif
 
       return
       end



! =====================================================================
       logical function Reserved()
! =====================================================================
      use ManagerModule
      implicit none

!+  Purpose
!     Check to see if word is a reserved word.

!+  Changes
!      TM - 21/11/94
!      TM - 23/07/95   Add C_ELSEIF to list

!+  Constant Values
       integer       Num_reserved_words
       parameter     (Num_reserved_words = 7)
!
       character*12  Reserved_word_array(Num_reserved_words)
       integer       Reserved_word_array2(Num_reserved_words)
!
       data          Reserved_word_array /'if    ','then  ',
     .               'else  ','endif ','or    ','and   ','elseif'/
!
       data          Reserved_word_array2 /C_IF,C_THEN,C_ELSE,C_ENDIF,
     .                                     C_OR,C_AND,C_ELSEIF/

!+  Local Variables
       logical       Found                ! Word found flag
       integer       ind                  ! loop index

!- Implementation Section ----------------------------------
 
       Found = .false.
 
       do 10  ind = 1, Num_reserved_words
              if (g%buffer .eq. Reserved_word_array(ind)) then
                     Found = .true.
                     g%token = Reserved_word_array2(ind)
              endif
10     continue
 
       Reserved = Found
 
       return
       end



