*     ===========================================================
      subroutine APSIM_operatns (Action, Data_String)
*     ===========================================================
      implicit none
      dll_export apsim_operatns
      include   'const.inc'            ! Global constant definitions
      include   'operatns.inc'
      include   'string.pub'
      include   'error.pub'
      include   'engine.pub'

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
      character  module_name*8         ! name of this module

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
         ! initialise error flags
      call set_warning_off ()
 
      if (Action.eq.MES_Init) then
         call operatns_Get_Other_Variables ()
         call operatns_zero_variables ()
         call operatns_Init ()
 
      else if (Action.eq.MES_Prepare) then
         call operatns_Get_Other_Variables ()
         call operatns_schedule (Prepare_Phase)
 
      else if (Action.eq.MES_Process) then
         call operatns_schedule (Process_Phase)
 
      else if (Action.eq.MES_Post) then
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
      implicit none
      include   'const.inc'
      include   'operatns.inc'         ! operatns model common
      include   'read.pub'
      include   'error.pub'
      include   'write.pub'

*+  Purpose
*      Initialise operatns module

*+  Changes
*     dph 10/5/99 removed version and presence reports c186

*+  Calls

*+  Constant Values
      character*(*) my_name            ! name of current procedure
      parameter (my_name = 'operatns_init')

*+  Local Variables
      integer    iostatus             ! flag for success of opening file

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
         ! Notify system that we have initialised
 
      call report_event (' Initialising')
 
      g_oplun = Get_Logical_Unit ()
      open (unit=g_oplun, file='operatns.tmp', form='formatted',
     :     access='direct', recl= record_length, iostat=iostatus)
 
      if (iostatus.eq.0) then
         call operatns_read_section ('prepare',prepare_phase)
         call operatns_read_section ('start_of_day',prepare_phase)
         call operatns_read_section ('parameters',prepare_phase)
         call operatns_read_section ('process',process_phase)
         call operatns_read_section ('post',post_phase)
         call operatns_read_section ('end_of_day',post_phase)
         call operatns_sort_data ()
         call operatns_list ()
 
         rewind (g_oplun)
      else
         call fatal_error (Err_User, 'Cannot open scratch file.')
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine operatns_zero_variables ()
*     ===========================================================
      implicit none
      include   'operatns.inc'         ! operatns common block
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
 
      g_last_record = 0
      g_op_pointer = 1
 
      call fill_integer_array (g_op_days, 0, max_ops)
      call fill_integer_array (g_op_years, 0, max_ops)
      call fill_integer_array (g_op_order, 0, max_ops)
      call fill_integer_array (g_op_phase, 0, max_ops)
 
      g_phase_name(prepare_phase) = 'Prepare'
      g_phase_name(process_phase) = 'Process'
      g_phase_name(post_phase) = 'Post'
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine operatns_get_other_variables ()
*     ===========================================================
      implicit none
      include   'const.inc'            ! Constant definitions
      include   'operatns.inc'         ! operatns common block
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
     :    , g_thisyear      ! Variable
     :    , numvals         ! Number of values returned
     :    , min_year            ! Lower Limit for bound checking
     :    , max_year)           ! Upper Limit for bound checking
 
      call Get_integer_var (
     :      unknown_module  ! Module that responds (Not Used)
     :    , 'day'           ! Variable Name
     :    , '()'            ! Units                (Not Used)
     :    , g_today         ! Variable
     :    , numvals         ! Number of values returned
     :    , 0               ! Lower Limit for bound checking
     :    , 366)            ! Upper Limit for bound checking
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine operatns_list()
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'operatns.inc'
      include   'error.pub'
      include   'write.pub'

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
 
      call write_string (LU_Scr_sum, 'Operations Schedule')
      call write_string (LU_Scr_sum, '===================')
 
      do 100 counter = 1, g_last_record
         recno = g_op_order(counter)
         read (g_oplun, '(A)', rec=recno) Record
         write(Line,'(2i5,2x,a,2x,a)')
     :                    g_op_days(recno)
     :                   ,g_op_years(recno)
     :                   ,g_phase_name(g_op_phase(recno))
     :                   ,Record
         call write_string (LU_Scr_sum, Line)
  100 continue
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine operatns_read_section (section, phase_no)
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'operatns.inc'
      include   'error.pub'
      include   'read.pub'
      include   'engine.pub'

*+  Sub-Program Arguments
      character  section*(*)           ! section names
      integer    phase_no

*+  Purpose
*     Read a data section for a given phase.

*+  Changes
*     240395 jngh changed to read from section
*     050895 nih  upgraded to allow operations to be assigned to phase.
*                 Routine used to be called operatns_concat_files.

*+  Constant Values
      integer    iterate_flag
      parameter (iterate_flag = 0)
*
      integer    start_flag
      parameter (start_flag = 1)
*
      character*(*) my_name            ! name of current procedure
      parameter (my_name = 'operatns_read_section')

*+  Local Variables
      integer    flag                 ! flag for search action type
      character  Line*(record_length) ! line from an operations file
      character  module_name*8        ! Name of this module
*      integer    recno                ! record number for direct
                                      ! access file

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      call get_current_module (module_name)
 
      flag = start_flag
  100 continue
 
      call read_next_param_section (module_name, line, flag, section)
 
      flag = iterate_flag
 
      if (line .ne. blank) then
         if (g_last_record .lt. max_ops) then
            g_last_record = g_last_record + 1
            call operatns_extract_date (line
     :                              , g_op_days(g_last_record)
     :                              , g_op_years(g_last_record))
            write (g_oplun, '(A)', rec=g_last_record) line
            g_op_order(g_last_record) = g_last_record
            g_op_phase(g_last_record) = phase_no
            goto 100
 
         else
            call fatal_error (Err_User,
     :                       'Too many operations file to deal with')
 
         endif
      else
      endif
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine operatns_sort_data ()
*     ===========================================================
      implicit none
      include   'operatns.inc'
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
 
      step = g_last_record
  100 continue
         step = step/2
  150    continue
            swapped = .false.
            do 200 recno = 1, g_last_record - step
 
               day1 = g_op_days(g_op_order(recno))
               day2 = g_op_days(g_op_order(recno+step))
               year1 = g_op_years(g_op_order(recno))
               year2 = g_op_years(g_op_order(recno+step))
               phase1 = g_op_phase(g_op_order(recno))
               phase2 = g_op_phase(g_op_order(recno+step))
 
 
               if (((day1.gt.day2) .and. (year1.eq.year2))
     :                            .or.
     :                      (year1.gt.year2))
     :                            then
 
         ! These records need to be swapped to be in chronological order
                  temp = g_op_order(recno+step)
                  g_op_order(recno+step) = g_op_order(recno)
                  g_op_order(recno) = temp
 
                  swapped = .true.
 
               else if (((day1.eq.day2) .and. (year1.eq.year2))
     :                            .and.
     :                      (phase1.gt.phase2))
     :                            then
         ! These records need to be swapped to be in phase order
                  temp = g_op_order(recno+step)
                  g_op_order(recno+step) = g_op_order(recno)
                  g_op_order(recno) = temp
 
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
      implicit none
      include   'const.inc'
      include   'operatns.inc'         ! operatns common block file
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
      implicit none
      include   'const.inc'
      include   'operatns.inc'         ! operations common block
      include   'string.pub'
      include   'intrface.pub'
      include   'engine.pub'
      include   'error.pub'
      include   'read.pub'
      include   'write.pub'

*+  Sub-Program Arguments
       integer Phase_no

*+  Purpose
*      Perform actions for current day.

*+  Changes
*     010495 jngh if 'set' action found, then use call to set array.
*     040595 jngh removed above changes
*     050895 nih  created from operatns_process
*                 now schedules operations for a given phase.
*     011195 jngh changed message_send to message_send_immediate
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
 
      If (g_op_pointer .le. g_last_record) then
 
         recno = g_op_order(g_op_pointer)
         nextday = g_op_days(recno)
         nextyear = g_op_years(recno)
         nextphase = g_op_phase(recno)
 
         if ((nextday .eq. g_today)
     :       .and. (nextyear.eq. g_thisyear)
     :       .and. (nextphase.eq. Phase_no)) then
 
            read (g_oplun, '(A)', rec=recno) Line
 
               ! extract components from string
            call get_next_word (Line, Destination)
            call get_next_word (Line, Action)
            Line = adjustl(line)
 
            call report_event (
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
                  call Message_Send_immediate
     :                      (destination, Action, Variable_name)
               else
                  call Message_Send_immediate
     :                      (destination, Action, Blank)
               endif
 
            else
               call Message_Send_immediate (destination, Action, Line)
            endif
 
            call Delete_postbox ()
 
            g_op_pointer = g_op_pointer + 1
            goto 100
 
         else if (((g_today .gt. nextday).and.(nextyear.eq.g_thisyear))
     :                                 .or.
     :                            g_thisyear.gt.nextyear) then
 
               ! we are actually past this operation date
               ! - try to catch up
 
            g_op_pointer = g_op_pointer + 1
 
            goto 100
 
         else if (((g_today .eq. nextday).and.(nextyear.eq.g_thisyear))
     :                                 .and.
     :                            Phase_no.gt.NextPhase) then
            ! It is the right day but we are past this phase so try
            ! and find a later record that is for this phase.
 
            g_op_pointer = g_op_pointer + 1
 
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
