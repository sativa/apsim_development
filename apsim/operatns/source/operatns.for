      include 'Operatns.inc'

!     ===========================================================
      subroutine AllocInstance (InstanceName, InstanceNo)
!     ===========================================================
      use OperatnsModule
      implicit none
 
!+  Sub-Program Arguments
      character InstanceName*(*)       ! (INPUT) name of instance
      integer   InstanceNo             ! (INPUT) instance number to allocate
 
!+  Purpose
!      Module instantiation routine.
 
!- Implementation Section ----------------------------------
               
      allocate (Instances(InstanceNo)%gptr)
      Instances(InstanceNo)%Name = InstanceName
 
      return
      end

!     ===========================================================
      subroutine FreeInstance (anInstanceNo)
!     ===========================================================
      use OperatnsModule
      implicit none
 
!+  Sub-Program Arguments
      integer anInstanceNo             ! (INPUT) instance number to allocate
 
!+  Purpose
!      Module de-instantiation routine.
 
!- Implementation Section ----------------------------------
               
      deallocate (Instances(anInstanceNo)%gptr)
 
      return
      end
     
!     ===========================================================
      subroutine SwapInstance (anInstanceNo)
!     ===========================================================
      use OperatnsModule
      implicit none
 
!+  Sub-Program Arguments
      integer anInstanceNo             ! (INPUT) instance number to allocate
 
!+  Purpose
!      Swap an instance into the global 'g' pointer
 
!- Implementation Section ----------------------------------
               
      g => Instances(anInstanceNo)%gptr
 
      return
      end

*     ===========================================================
      subroutine Main (Action, Data_String)
*     ===========================================================
      use OperatnsModule
      implicit none
      include   'const.inc'            ! Global constant definitions
      include   'string.pub'
      include   'error.pub'
      include   'action.inc'

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
 
      if (Action.eq.ACTION_Init) then
         call operatns_Get_Other_Variables ()
         call operatns_zero_variables ()
         call operatns_Init ()
 
      else if (Action.eq.ACTION_Prepare) then
         call operatns_Get_Other_Variables ()
         call operatns_schedule (Prepare_Phase)
 
      else if (Action.eq.ACTION_Process) then
         call operatns_schedule (Process_Phase)
 
      else if (Action.eq.ACTION_Post) then
         call operatns_schedule (Post_Phase)
 
      else
            ! Don't use message
         call Message_unused ()
 
      endif
 
      call pop_routine (my_name)
      return
      end

*     ===========================================================
      subroutine operatns_Init ()
*     ===========================================================
      use OperatnsModule
      implicit none
      include   'const.inc'
      include   'read.pub'
      include   'error.pub'

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
     :     access='direct', recl= record_length, iostat=iostatus)
 
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
      end



*     ===========================================================
      subroutine operatns_zero_variables ()
*     ===========================================================
      use OperatnsModule
      implicit none
      include   'error.pub'
      include   'data.pub'

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
      end



*     ===========================================================
      subroutine operatns_get_other_variables ()
*     ===========================================================
      use OperatnsModule
      implicit none
      include   'const.inc'            ! Constant definitions
      include   'error.pub'
      include   'intrface.pub'

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
      end



*     ===========================================================
      subroutine operatns_list()
*     ===========================================================
      use OperatnsModule
      implicit none
      include   'const.inc'
      include   'error.pub'

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

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      call write_string ('Operations Schedule')
      call write_string ('===================')
 
      do 100 counter = 1, g%last_record
         recno = g%op_order(counter)
         read (g%oplun, '(A)', rec=recno) Record
         write(Line,'(2i5,2x,a,2x,a)')
     :                    g%op_days(recno)
     :                   ,g%op_years(recno)
     :                   ,g%phase_name(g%op_phase(recno))
     :                   ,Record
         call write_string (Line)
  100 continue
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine operatns_read_section (section, phase_no)
*     ===========================================================
      use OperatnsModule
      implicit none
      include   'const.inc'
      include   'error.pub'
      include   'read.pub'
      include   'string.pub'
      include   'apsimengine.pub'
      include   'componentinterface.inc'

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
      character lower_case*(MAX_CONDITION_SIZE)

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

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      ! get a list of all rule names that user has defined.
      call somcomponent_getpropertynames(componentData,
     .                                   Rule_names,
     .                                   'rule',
     .                                   MAX_RULES,
     .                                   Num_rules)
      ! loop through all rules looking for ones that match our section
      do Rule_Index = 1, Num_rules
         rule_object = component_getrule(ComponentData,
     .                                   Rule_names(Rule_index),
     .                                   ' ')
         if (rule_object .ne. 0) then
            call rule_getcondition(rule_object, condition)
            condition = lower_case(condition)
            if (condition .eq. section) then
               num_lines = rule_getActionLineCount(rule_object)

               do 100 Line_number = 0, num_lines-1
                  call rule_getActionLine(rule_object,
     .                                    Line_number, Line)

                  ! remove any comments
                  if (index(Line, '!') .gt. 0) then
                     Line(index(Line, '!'):) = Blank
                  endif

                  if (line .ne. blank) then
                     if (g%last_record .lt. max_ops) then
                        g%last_record = g%last_record + 1
                        call operatns_extract_date (line
     :                               , g%op_days(g%last_record)
     :                               , g%op_years(g%last_record))
                        write (g%oplun, '(A)', rec=g%last_record) line
                        g%op_order(g%last_record) = g%last_record
                        g%op_phase(g%last_record) = phase_no

                     else
                        call fatal_error (Err_User,
     :                     'Too many operations file to deal with')
                        goto 200
                     endif
                  endif
100            continue
200            continue
            endif
         endif
      end do

      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine operatns_sort_data ()
*     ===========================================================
      use OperatnsModule
      implicit none
      include   'error.pub'

*+  Purpose
*   This subroutine uses a simple shell sort algorithm to sort
*   the pointers to the data in the scratch file into
*   chronological order

*+  Changes
*    050895 - NIH - created from operatns_sort_file to include sorting
*                   of data into phases.

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
      integer    step
      logical    swapped

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      step = g%last_record
  100 continue
         step = step/2
  150    continue
            swapped = .false.
            do 200 recno = 1, g%last_record - step
 
               day1 = g%op_days(g%op_order(recno))
               day2 = g%op_days(g%op_order(recno+step))
               year1 = g%op_years(g%op_order(recno))
               year2 = g%op_years(g%op_order(recno+step))
               phase1 = g%op_phase(g%op_order(recno))
               phase2 = g%op_phase(g%op_order(recno+step))
 
 
               if (((day1.gt.day2) .and. (year1.eq.year2))
     :                            .or.
     :                      (year1.gt.year2))
     :                            then
 
         ! These records need to be swapped to be in chronological order
                  temp = g%op_order(recno+step)
                  g%op_order(recno+step) = g%op_order(recno)
                  g%op_order(recno) = temp
 
                  swapped = .true.
 
               else if (((day1.eq.day2) .and. (year1.eq.year2))
     :                            .and.
     :                      (phase1.gt.phase2))
     :                            then
         ! These records need to be swapped to be in phase order
                  temp = g%op_order(recno+step)
                  g%op_order(recno+step) = g%op_order(recno)
                  g%op_order(recno) = temp
 
                  swapped = .true.
 
               else
               endif
  200       continue
 
            if (.not.swapped) goto 250
         goto 150
 
  250    continue
 
         if (step .le. 1) goto 300
 
      goto 100
 
  300 continue  ! finished sorting
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine operatns_extract_date (record, day, year)
*     ===========================================================
      use OperatnsModule
      implicit none
      include   'const.inc'
      include   'error.pub'
      include   'string.pub'
      include   'datastr.pub'

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
      end



*     ===========================================================
      subroutine operatns_schedule (Phase_no)
*     ===========================================================
      use OperatnsModule
      implicit none
      include   'const.inc'
      include   'string.pub'
      include   'intrface.pub'
      include   'error.pub'
      include   'read.pub'
      include   'postbox.pub'
      include   'datastr.pub'

*+  Sub-Program Arguments
       integer Phase_no

*+  Purpose
*      Perform actions for current day.

*+  Changes
*     010495 jngh if 'set' action found, then use call to set array.
*     040595 jngh removed above changes
*     050895 nih  created from operatns_process
*                 now schedules operations for a given phase.
*     011195 jngh changed message_send to Action_send
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
      integer    NextDay
      integer    NextPhase
      integer    NextYear
      integer    recno
      character  Variable_name*32

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
 
            read (g%oplun, '(A)', rec=recno) Line
 
               ! extract components from string
            call get_next_word (Line, Destination)
            call get_next_word (Line, Action)
            Line = adjustl(line)
 
            call Write_string (
     :          ' Sending '
     :       // trim(Action)
     :       // ' message to '
     :       // trim(Destination))
 
            call New_postbox ()
            Data_stored = Store_in_postbox (Line)
 
            if (Data_stored) then
 
               if (Action .eq. 'set') then
                  call Get_next_variable (Line,
     :                                    Variable_name,
     :                                    Line)
                  call Action_send
     :                      (destination, Action, Variable_name)
               else
                  call Action_send
     :                      (destination, Action, Blank)
               endif
 
            else
               call Action_send (destination, Action, Line)
            endif
 
            call Delete_postbox ()
 
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
 
      call pop_routine (my_name)
      return
      end
