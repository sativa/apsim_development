      module OperatnsModule
      use Registrations
!     ================================================================
!     operatns module
!     ================================================================

!   Short description:
!

!   Notes:
!      none


!   Changes:
!      081294 jngh

! ----------------------- Declaration section ------------------------

!   Constant values
                             !
      integer    numfiles
      parameter (numfiles = 10)

      integer    record_length
      parameter (record_length = 500)

      integer    max_ops
      parameter (max_ops = 1000)

      integer    prepare_phase
      parameter (prepare_phase = 1)

      integer    process_phase
      parameter (process_phase = 2)

      integer    post_phase
      parameter (post_phase = 3)

      type OperatnsGlobals
         sequence
         integer    today
         integer    thisyear
         integer    oplun
         integer    last_record
         integer    op_days(max_ops)
         integer    op_years(max_ops)
         integer    op_phase(max_ops)
         integer    op_order(max_ops)
         integer    op_pointer
         character phase_name(3)*10
      end type OperatnsGlobals

      ! instance variables.
      common /InstancePointers/ ID,g,p,c
      save InstancePointers
      type (OperatnsGlobals),pointer :: g
      type (IDsType), pointer :: id

      contains

! ====================================================================
       subroutine Set_variable_in_other_module (modnameID
     :                                         ,var_name
     :                                         ,variable_value)
! ====================================================================
      Use Infrastructure
      implicit none

!+  Subprogram Arguments
      integer modNameID                ! ID for module.
      character Var_name*(*)
      Character Variable_value*(*)

!+  Purpose
!      Set the value of a variable in another module

!+  Changes


!+  Calls

!+  Constant Values
      character This_routine*(*)       ! Name of this routine
      parameter (This_routine='Set_variable_in_other_module')

      integer max_size    ! max size of char array
      parameter (max_size = 100)

      integer max_len    ! max length of a string
      parameter (max_len = 100)

!+  Local Variables
      integer numvals
      character values(max_size)*(max_len)

!- Implementation Section ----------------------------------

      call push_routine(This_routine)

      numvals = word_count(Variable_value)

      if (numvals.eq.1) then
         call set_char_var(modNameID,
     .         trim(var_name), ' ',
     .         trim(Variable_value) )
      Else
         call string_to_Char_array(Variable_value
     :                            ,values
     :                            ,max_size
     :                            ,numvals)

         call set_char_array(modNameID
     :                      ,trim(var_name)
     :                      ,' '
     :                      ,values
     :                      ,numvals)

      endif

      call pop_routine(This_routine)

      return
      end subroutine


*     ===========================================================
      subroutine operatns_Init ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*      Initialise operatns module

*+  Changes
*     dph 10/5/99 removed version and presence reports c186
*     dph 15/12/00 added properties back in.
*     dph 18/1/01  changed properties to parameters - mistake

*+  Calls

*+  Constant Values
      character*(*) my_name            ! name of current procedure
      parameter (my_name = 'operatns_init')

*+  Local Variables
      integer    iostatus             ! flag for success of opening file

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      g%oplun = 57
      open (unit=g%oplun, file='operatns.tmp', form='formatted',
     :     access='direct', recl= record_length, iostat=iostatus
     :     , status = 'SCRATCH')

      if (iostatus.eq.0) then
         call operatns_read_section ('start_of_day',prepare_phase)
         call operatns_read_section ('parameters',prepare_phase)
         call operatns_read_section ('process',process_phase)
         call operatns_read_section ('end_of_day',post_phase)
         call operatns_sort_data ()
         call operatns_list ()

      else
         call fatal_error (Err_User, 'Cannot open scratch file.')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine operatns_EndRun ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*      Close operatns module

*+  Changes
*     dph 10/5/99 removed version and presence reports c186
*     dph 15/12/00 added properties back in.
*     dph 18/1/01  changed properties to parameters - mistake

*+  Calls

*+  Constant Values
      character*(*) my_name            ! name of current procedure
      parameter (my_name = 'operatns_EndRun')

*+  Local Variables
      integer    iostatus             ! flag for success of opening file

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      close (unit=g%oplun, status = 'DELETE')

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine operatns_zero_variables ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*     Set all variables in this module to zero.

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) my_name            ! name of current procedure
      parameter (my_name = 'operatns_zero_variables')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      g%last_record = 0
      g%op_pointer = 1

      call fill_integer_array (g%op_days, 0, max_ops)
      call fill_integer_array (g%op_years, 0, max_ops)
      call fill_integer_array (g%op_order, 0, max_ops)
      call fill_integer_array (g%op_phase, 0, max_ops)

      g%phase_name(prepare_phase) = 'Prepare'
      g%phase_name(process_phase) = 'Process'
      g%phase_name(post_phase) = 'Post'

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine operatns_get_other_variables ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*      Get the values of variables from other modules

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) my_name            ! name of current procedure
      parameter (my_name = 'operatns_get_other_variables')

*+  Local Variables
      integer    numvals

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call Get_integer_var (
     :      unknown_module  ! Module that responds (Not Used)
     :    , 'year'          ! Variable Name
     :    , '()'            ! Units                (Not Used)
     :    , g%thisyear      ! Variable
     :    , numvals         ! Number of values returned
     :    , min_year            ! Lower Limit for bound checking
     :    , max_year)           ! Upper Limit for bound checking

      call Get_integer_var (
     :      unknown_module  ! Module that responds (Not Used)
     :    , 'day'           ! Variable Name
     :    , '()'            ! Units                (Not Used)
     :    , g%today         ! Variable
     :    , numvals         ! Number of values returned
     :    , 0               ! Lower Limit for bound checking
     :    , 366)            ! Upper Limit for bound checking

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine operatns_list()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*     <insert here>

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) my_name            ! name of current procedure
      parameter (my_name = 'operatns_list')

*+  Local Variables
      integer    counter
      character  line*(record_Length+80)
      character  record*(record_length)
      integer    recno
      integer    iostatus

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call write_string ('Operations Schedule')
      call write_string ('===================')

      do 100 counter = 1, g%last_record
         recno = g%op_order(counter)
         read (g%oplun, '(A)', rec=recno, iostat=iostatus) Record
         if (iostatus .ne. 0) then
            call Fatal_error(ERR_user, 'Error reading operations data')
            goto 1000
         else
         endif
         write(Line,'(2i5,2x,a,2x,a)')
     :                    g%op_days(recno)
     :                   ,g%op_years(recno)
     :                   ,g%phase_name(g%op_phase(recno))
     :                   ,Record
         call write_string (Line)
  100 continue
 1000 continue
      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine operatns_read_section (section, phase_no)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      character  section*(*)           ! section names
      integer    phase_no

*+  Purpose
*     Read a data section for a given phase.

*+  Changes
*     240395 jngh changed to read from section
*     050895 nih  upgraded to allow operations to be assigned to phase.
*                 Routine used to be called operatns_concat_files.
*     101100 dph  changed to use text object instead of memo object

*+  Constant Values
      character*(*) my_name            ! name of current procedure
      parameter (my_name = 'operatns_read_section')

       INTEGER MAX_RULE_NAME_SIZE
       parameter (MAX_RULE_NAME_SIZE=100)
       INTEGER MAX_RULES
       PARAMETER (MAX_RULES=100)
       INTEGER MAX_CONDITION_SIZE
       parameter (MAX_CONDITION_SIZE=20)

*+  Calls


*+  Local Variables
      character  Line*(record_length) ! line from an operations file
*      integer    recno                ! record number for direct
                                      ! access file
      integer rule_object             ! C++ rule object
      logical ok                      ! created object ok?
      integer Line_number             ! line number
      integer num_lines               ! number of lines in memo
      CHARACTER Rule_names(MAX_RULES)*(MAX_RULE_NAME_SIZE)
                                       ! rule names user has defined
       INTEGER Num_rules               ! number of rules user has defined
       integer Rule_Type               ! index into rules list
       CHARACTER condition*(MAX_CONDITION_SIZE)
                                       ! condition of each rule
       integer rule_index
      integer iostatus 

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      ! get a list of all rule names that user has defined.
      call apsimcomponentdata_getrulenames(get_componentData(),
     .                                     Rule_names,
     .                                     MAX_RULES,
     .                                     Num_rules)

      ! loop through all rules looking for ones that match our section
      do Rule_Index = 1, Num_rules
         call apsimcomponentdata_loadrule(get_componentData(),
     .                                    Rule_names(Rule_index))
         if (index(Rule_names(Rule_index),
     .             section) .ne. 0) then
            call apsimcomponentdata_loadrule(get_componentData(),
     .                                       Rule_names(Rule_index))

            num_lines = apsimcomponentdata_getnumrulelines()

            do 100 Line_number = 0, num_lines-1
               call apsimcomponentdata_getruleline(Line_number,
     .                                             Line)

               ! remove any comments
               if (index(Line, '!') .gt. 0) then
                  Line(index(Line, '!'):) = Blank
               endif

               if (line .ne. blank) then
                  if (g%last_record .lt. max_ops) then
                     g%last_record = g%last_record + 1
                     call operatns_extract_date (line
     :                            , g%op_days(g%last_record)
     :                            , g%op_years(g%last_record))
                     write (g%oplun, '(A)', rec=g%last_record, 
     :                      iostat=iostatus) line
                     if (iostatus .ne. 0) then
                        call Fatal_error(ERR_user, 
     :                             'Error writing operations data')
                        goto 200
                     else
                     endif
                     g%op_order(g%last_record) = g%last_record
                     g%op_phase(g%last_record) = phase_no

                  else
                     call fatal_error (Err_User,
     :                  'Too many operations file to deal with')
                     goto 200
                  endif
               endif
100         continue
         endif
      end do

200   continue
      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine operatns_sort_data ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*   This subroutine uses a simple shell sort algorithm to sort
*   the pointers to the data in the scratch file into
*   chronological order

*+  Changes
*    050895 - NIH - created from operatns_sort_file to include sorting
*                   of data into phases.
*    060201 - DSG - reoved the shell sort and replaced with a bubble
*                   sort (reference d-415)

*+  Constant Values
      character*(*) my_name            ! name of current procedure
      parameter (my_name = 'operatns_sort_data_pointers')

*+  Local Variables
      integer    day1
      integer    day2
      integer    phase1
      integer    phase2
      integer    temp
      integer    year1
      integer    year2
      integer    recno
      integer    i

*- Implementation Section ----------------------------------
      call push_routine (my_name)

        do 250 i = 1, g%last_record
            do 200 recno = 1, (g%last_record-1)

               day1 = g%op_days(g%op_order(recno))
               day2 = g%op_days(g%op_order(recno+1))
               year1 = g%op_years(g%op_order(recno))
               year2 = g%op_years(g%op_order(recno+1))
               phase1 = g%op_phase(g%op_order(recno))
               phase2 = g%op_phase(g%op_order(recno+1))


               if (((day1.gt.day2) .and. (year1.eq.year2))
     :                            .or.
     :                      (year1.gt.year2))
     :                            then

         ! These records need to be swapped to be in chronological order
                  temp = g%op_order(recno+1)
                  g%op_order(recno+1) = g%op_order(recno)
                  g%op_order(recno) = temp


               else if (((day1.eq.day2) .and. (year1.eq.year2))
     :                            .and.
     :                      (phase1.gt.phase2))
     :                            then
         ! These records need to be swapped to be in phase order
                  temp = g%op_order(recno+1)
                  g%op_order(recno+1) = g%op_order(recno)
                  g%op_order(recno) = temp


               else
               endif
  200       continue
  250       continue


 ! finished sorting

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine operatns_extract_date (record, day, year)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      character  record*(*)            ! record from file
      integer    day                   ! day of year from record
      integer    year                  ! year from record

*+  Purpose
*      Reads day and year from a record.  These can be in any order.

*+  Changes
*   5-7-94 NIH specified and programmed
*   5-8-95 NIH change name from operatns_Record_date and change code
*              to extract/remove the date out of the string

*+  Constant Values
      character*(*) my_name            ! name of current procedure
      parameter (my_name = 'operatns_record_date')

*+  Local Variables
      integer    dayflag
      character  day_string*20
      integer    tempinteger           ! temp number
      integer    yearflag
      character  year_string*20

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call get_next_word (record, day_String)
      call string_to_integer_var (day_string, day, dayflag)

      call get_next_word (record, year_string)
      call string_to_integer_var (year_string, year, yearflag)

      if ((dayflag.ne.1) .or. (yearflag.ne.1)) then
         call warning_error (Err_User,
     :               'trouble with date format in file')
         day = 0
         year = 0

      else
         if (day .gt. 366) then
               ! it must be year in first column - swap values
            tempinteger = year
            year = day
            day = tempinteger

         else
            ! assume day in first column
         endif

      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine operatns_schedule (Phase_no)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
       integer Phase_no

*+  Purpose
*      Perform actions for current day.

*+  Changes
*     010495 jngh if 'set' action found, then use call to set array.
*     040595 jngh removed above changes
*     050895 nih  created from operatns_process
*                 now schedules operations for a given phase.
*     011195 jngh changed message_send to Action_send subroutine
*     14/2/96 DPH added calls to new_postbox, delete_postbox

*+  Calls

*+  Constant Values
      character*(*) my_name            ! name of current procedure
      parameter (my_name = 'operatns_schedule')

*+  Local Variables
      character  Action*20
      logical    Data_stored
      character  destination*15
      character  Line*(Record_Length)
      character  Value*(Record_length)
      integer    NextDay
      integer    NextPhase
      integer    NextYear
      integer    recno
      character  Variable_name*32
      integer    modNameID
      character  msg*500
      integer regID
      integer iostatus

*- Implementation Section ----------------------------------
      call push_routine (my_name)

  100 continue

      If (g%op_pointer .le. g%last_record) then

         recno = g%op_order(g%op_pointer)
         nextday = g%op_days(recno)
         nextyear = g%op_years(recno)
         nextphase = g%op_phase(recno)

         if ((nextday .eq. g%today)
     :       .and. (nextyear.eq. g%thisyear)
     :       .and. (nextphase.eq. Phase_no)) then

            read (g%oplun, '(A)', rec=recno, iostat=iostatus) Line
            if (iostatus .ne. 0) then
               write (msg, '(a)' )
     :         'Error reading operations data'
               call Fatal_error(ERR_user, msg)
               goto 1000
            else
            endif
               ! extract components from string
            call get_next_word (Line, Destination)
            call get_next_word (Line, Action)
            Line = adjustl(line)
            Line = Lower_case(Line)
            Action = adjustl(Action)
            Action = Lower_case(Action)
            Destination = adjustl(Destination)
            Destination = lower_case(Destination)

            call Write_string (
     :          ' Sending '
     :       // trim(Action)
     :       // ' message to '
     :       // trim(Destination))

            if (Line <> ' ' .and.
     :         index(Line, '=') .eq. 0) then
               write (msg, '(50a)' )
     :         'Operations message has data in a action line that does',
     :         new_line,
     :         ' not have a equals sign in it.  Please correct problem.'
     :         , new_line
     :         , 'Action line:- ', trim(Line)
               call Fatal_error(ERR_user, msg)
            endif

            if (Action .eq. 'set') then
               call Get_next_variable (Line,
     :                                 Variable_name,
     :                                 value)
               if (component_name_to_id(destination, modNameID)) then
                  call set_variable_in_other_module
     :                     (modNameID
     :                     ,Variable_name
     :                     ,Value)
               else
                  write(msg, '(3a)' )
     :               'Cannot set variable value in module ',
     :               destination,
     :               '.  Module doesnt exist.'
                  call fatal_error(err_user, msg)
               endif
            elseif (Action .eq. 'init') then
                     ! Init probably won't work via this method. Stop dead.
                        call Fatal_error(ERR_user,
     .                 'INIT messages do not work anymore. Use RESET')
            else

               call New_postbox ()
               Data_stored = Store_message_data (Line)
               if (Data_stored) then
                  if (destination .eq. All_active_modules) then
                     regID=Add_Registration (EventReg, Action
     :                                       , ' ', ' ', ' ')
                     call Event_Send (Action)
                  else
                     call Event_Send_directed (destination, Action)
                  endif
               else
!                  write(msg, '(2a)' )
!     :               'Invalid operations line: ',
!     :               Line
!                  call Fatal_error(err_user, msg)
               endif
               call Delete_postbox ()
            endif

            g%op_pointer = g%op_pointer + 1
            goto 100

         else if (((g%today .gt. nextday).and.(nextyear.eq.g%thisyear))
     :                                 .or.
     :                            g%thisyear.gt.nextyear) then

               ! we are actually past this operation date
               ! - try to catch up

            g%op_pointer = g%op_pointer + 1

            goto 100

         else if (((g%today .eq. nextday).and.(nextyear.eq.g%thisyear))
     :                                 .and.
     :                            Phase_no.gt.NextPhase) then
            ! It is the right day but we are past this phase so try
            ! and find a later record that is for this phase.

            g%op_pointer = g%op_pointer + 1

            goto 100

         else
            ! do nothing today - try again tomorrow.

         endif
      else
         ! we are at the end of the operations file
      endif
1000  continue
      call pop_routine (my_name)
      return
      end subroutine

* ====================================================================
       logical function Store_message_data (Data_string)
* ====================================================================
      use ConstantsModule
      use ErrorModule
      use StringModule
      use DataStrModule
      Use infrastructure

      implicit none
!      include 'postbox.inc'

*+ Sub-Program Arguments
      character Data_string*(*)        ! (INPUT & OUTPUT) Data string to store into postbox.

*+ Purpose
*     Store the data string, as a scalar or array if possible into the current postbox.
*     Return TRUE if something was stored.

*+ Notes
*     A data string will only be stored if an equals sign exists in the
*     data_string somewhere.

*+  Mission Statement
*

*+ Changes
*     DPH 19/10/95
*     dph 26/7/99 commented out test for fatal_error_found.  No longer
*                 any such routine

*+ Calls
!      logical Fatal_error_found        ! function

*+ Constant Values
      character This_routine*(*)
      parameter (This_routine='Store_message_data')

*+ Local Variables
      logical Stored                   ! Was message stored in postbox properly?
      character Our_string*1000        ! Copy of string passed in.
      character Variable_name*(MAX_VARIABLE_NAME_SIZE)
                                       ! Our variable name
      character Units*100              ! Units
      character Variable_values*1000   ! Our variable values

      integer MAX_CHAR_ARRAY_SIZE
      parameter (MAX_CHAR_ARRAY_SIZE = 200)

      character array(MAX_CHAR_ARRAY_SIZE)*1000
      integer numvals

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

         ! Make copy of string passed in.

         call assign_string(Our_string, Data_string)

         ! Loop through each variable on data string and store in postbox.


10       continue
         call Get_next_variable (Our_string, Variable_name,
     :                           Variable_values)

         if (Variable_values .ne. Blank) then
            ! Found a variable all right.  Extract units and store variable.

            call Split_off_units(Variable_values, Units)

            !test if array and store in array and post char array
            ! otherwise store in variable and send as char var
            call String_to_char_array(Variable_values
     :                              , array
     :                              , MAX_CHAR_ARRAY_SIZE
     :                              , numvals)
            if (numvals > 1) then
               call Post_char_array (Variable_name
     :                              , units
     :                              , array
     :                              , Numvals)
            else if (numvals == 1) then
               call Post_char_var (Variable_name
     :                           , Units
     :                           , array(1))
            else
               ! no values found - should never occur
            endif

            goto 10

         else
            ! No more variables found - exit.

            Stored = .true.
         endif

      Store_message_data = Stored

      call pop_routine(This_routine)
      return
      end function

      end module OperatnsModule

!     ===========================================================
      subroutine alloc_dealloc_instance(doAllocate)
!     ===========================================================
      use OperatnsModule
      implicit none
      ml_external alloc_dealloc_instance

!+  Sub-Program Arguments
      logical, intent(in) :: doAllocate

!+  Purpose
!      Module instantiation routine.

!- Implementation Section ----------------------------------

      if (doAllocate) then
         allocate(g)
         allocate(id)
      else
         deallocate(g)
         deallocate(id)
      end if
      return
      end subroutine



*     ===========================================================
      subroutine Main (Action, Data_String)
*     ===========================================================
      Use infrastructure
      use OperatnsModule
      implicit none
      ml_external Main

*+  Sub-Program Arguments
      character  Action*(*)            ! Message action to perform
      character  Data_String*(*)       ! Message data

*+  Purpose
*      This routine is the interface between the main system and the
*      operatns module.

*+  Changes
*     dph 10/5/99 removed version and presence reports c186

*+  Calls

*+  Constant Values
      character*(*) my_name            ! name of current procedure
      parameter (my_name = 'operatns')

*+  Local Variables

*- Implementation Section ----------------------------------
      call push_routine (my_name)

!      print*,'Operatns'
!      print*,'action=[',trim(action),']'
!      print*,'data=[',trim(Data_String),']'

      if (Action.eq.ACTION_Init) then
         call operatns_Get_Other_Variables ()
         call operatns_zero_variables ()
         call operatns_Init ()

      else if (Action.eq.ACTION_Create) then
         call doRegistrations(id)

      else if (Action.eq.ACTION_Prepare) then
         call operatns_Get_Other_Variables ()
         call operatns_schedule (Prepare_Phase)

      else if (Action.eq.ACTION_Process) then
         call operatns_schedule (Process_Phase)

      else if (Action.eq.ACTION_Post) then
         call operatns_schedule (Post_Phase)

      else if (Action.eq.ACTION_End_Run) then
         call operatns_endRun ()

      else
            ! Don't use message
         call Message_unused ()

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
