*     ===========================================================
      character*(*) function frmrpt_version ()
*     ===========================================================
      implicit none
      include 'data.pub'
      include 'datastr.pub'
      include 'date.pub'
      include 'engine.pub'
      include 'error.pub'
      include 'intrface.pub'
      include 'license.pub'
      include 'read.pub'
      include 'science.pub'
      include 'string.pub'
      include 'write.pub'

*+  Purpose
*       return version number of FrmRpt module

*+  Changes
*     05/06/97  SB  Created

*+  Constant Values
      character  version_number*(*)    ! version number of module
      parameter (version_number = 'V0.00  07/05/97')

*- Implementation Section ----------------------------------
 
      call assign_string(frmrpt_version, version_number)
 
      return
      end



* ====================================================================
       subroutine APSIM_frmrpt(Action, Data)
* ====================================================================
      implicit none
      dll_export apsim_frmrpt
      include 'const.inc'             ! Global common block
      include 'frmrpt.inc'               ! input common
      include 'data.pub'
      include 'datastr.pub'
      include 'date.pub'
      include 'engine.pub'
      include 'error.pub'
      include 'intrface.pub'
      include 'license.pub'
      include 'read.pub'
      include 'science.pub'
      include 'string.pub'
      include 'write.pub'

*+  Sub-Program Arguments
       character Action*(*)            ! Message action to perform
       character Data*(*)              ! Message data

*+  Purpose
*      This module reads in FrmRpt data from input file.

*+  Changes
*     05/06/97  SB  Created

*+  Calls
      external frmrpt_version
      character*(52) frmrpt_version

*- Implementation Section ----------------------------------
 
      if (Action.eq.MES_Presence) then
         call Write_string(LU_Scr_sum,
     .       'Module = frmrpt ' // frmrpt_version())
 
      else if (Action.eq.MES_Init) then
         call frmrpt_read_param()
         call frmrpt_init()
 
      else if (Action.eq.MES_Process) then
         call frmrpt_update_sumvars()
         call frmrpt_report_counts()
 
      else if (Action.eq.'do_output') then
         call frmrpt_do_output(data)
 
      else if (Action.eq.'clear') then
         call frmrpt_clear_vars(data)
 
      else if (Action.eq.MES_End_run) then
         call frmrpt_end_run()
 
      else
         call Message_unused()        ! Don't use message
 
      endif
 
      return
      end



*     ===========================================================
      subroutine frmrpt_end_run()
*     ===========================================================
      implicit none
      include 'frmrpt.inc'
      include 'data.pub'
      include 'datastr.pub'
      include 'date.pub'
      include 'engine.pub'
      include 'error.pub'
      include 'intrface.pub'
      include 'license.pub'
      include 'read.pub'
      include 'science.pub'
      include 'string.pub'
      include 'write.pub'

*+  Purpose
*     Frees resources at end of run.

*+  Changes
*     05/06/97  SB  Created

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'frmrpt_end_run')

*+  Local Variables
      integer i_frm

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      do 110, i_frm=1, p_nforms
         close(g_out_file_unt(i_frm))
110   continue
 
      call pop_routine (my_name)
      return
      end



* ====================================================================
       subroutine frmrpt_Init ()
* ====================================================================
      implicit none
       include 'frmrpt.inc'             ! input common block
      include 'data.pub'
      include 'datastr.pub'
      include 'date.pub'
      include 'engine.pub'
      include 'error.pub'
      include 'intrface.pub'
      include 'license.pub'
      include 'read.pub'
      include 'science.pub'
      include 'string.pub'
      include 'write.pub'

*+  Purpose
*      Initialise FrmRpt module.

*+  Changes
*     05/06/97  SB  Created

*+  Constant Values
      character This_routine*(*)       ! Name of this routine
      parameter (This_routine='frmrpt_init')

*+  Local Variables
      integer i_var  ! Loop counters.
      integer i_frm  ! Loop counters.
      integer err_ret  ! To check status of opening files.
      integer frm_file_unt

*- Implementation Section ----------------------------------
      call push_routine(This_routine)
 
*  Initialise summation storage.
      g_nvars = 0
      do 110, i_var=1, max_vars
         g_val_nelem(i_var) = 0
         g_var_day_cnt(i_var) = 0
         call fill_real_array(g_values(1,i_var), 0.0, max_elems)
110   continue
 
      do 150, i_frm=1, p_nforms
         g_day_count(i_frm) = p_report_start(i_frm)
 
*  Open output files and create summation vars.
      ! Open output file.
         g_out_file_unt(i_frm) = get_logical_unit()
         call frmrpt_assert(g_out_file_unt(i_frm).ge.1,
     :                        'out_file_unt.ge.1')
         open(iostat=err_ret, file=p_out_file_name(i_frm),
     :            status='unknown', unit=g_out_file_unt(i_frm),
     :            carriagecontrol='fortran')
         call frmrpt_assert(err_ret.eq.0, 
     :            'Error opening file ' // p_out_file_name(i_frm))
         write(g_out_file_unt(i_frm), '(1H $)') 
 
      ! Open form file.
         frm_file_unt = get_logical_unit()
         call frmrpt_assert(frm_file_unt.ge.1, 'form_file_unt.ge.1')
         open(iostat=err_ret, file=p_form_file_name(i_frm),
     :            status='old', unit=frm_file_unt)
         call frmrpt_assert(err_ret.eq.0, 
     :            'Error opening file ' // p_form_file_name(i_frm))
 
      ! Look over form, creating any summation variables.
         call frmrpt_prcss_frm(.false.,
     :            frm_file_unt, g_out_file_unt(i_frm),
     :            p_escape_char(i_frm), i_frm)
   
      ! Finished with form file.
         close(frm_file_unt)
 
150   continue
 
      call pop_routine(This_routine)
      return
      end



*     ===========================================================
      subroutine frmrpt_read_param()
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'convert.inc'
      include   'frmrpt.inc'            ! soilph model common block
      include 'data.pub'
      include 'datastr.pub'
      include 'date.pub'
      include 'engine.pub'
      include 'error.pub'
      include 'intrface.pub'
      include 'license.pub'
      include 'read.pub'
      include 'science.pub'
      include 'string.pub'
      include 'write.pub'

*+  Purpose
*      Read in all parameters from parameter file.

*+  Changes
*     05/06/97  SB  Created

*+  Constant Values
      character*(*) section
      parameter (section = 'parameters')
*
      character*(*) my_name
      parameter (my_name='frmrpt_read_param')

*+  Local Variables
      integer numvals

*- Implementation Section ----------------------------------
      call push_routine(my_name)
 
      call write_string(lu_scr_sum
     :                 ,new_line//'   - Reading Parameters')
 
*  Handles.
      call read_char_array(section, 'handle', max_files,
     :                  '()', p_handle, p_nforms)
 
*  File names.
      call read_char_array(section, 'form_file', max_files,
     :                  '()', p_form_file_name, numvals)
      call frmrpt_assert(numvals.eq.p_nforms,
     :   'No of form_file differs from no of handles')
 
      call read_char_array(section, 'out_file', max_files,
     :                    '()', p_out_file_name, numvals)
      call frmrpt_assert(numvals.eq.p_nforms,
     :   'No of out_file differs from no of handles')
 
*  Escape characters.
      call read_char_array_optional(section, 'escape_char', max_files,
     :                  '()', p_escape_char, numvals)
      if (numvals .eq. 0)  then
         call fill_char_array(p_escape_char, '$', p_nforms)
      else
         call frmrpt_assert(numvals.eq.p_nforms,
     :     'No of esacpe_char differs from no of handles')
      end if
 
*  Reporting frequency.
      call read_integer_array_optional(section, 'report_days',
     :                         max_files, '()',
     :                         p_report_days, numvals, 0, 30000)
      if (numvals .eq. 0)  then
         call fill_integer_array(p_report_days, 0, p_nforms)
         call fill_integer_array(p_report_start, 0, p_nforms)
      else
         call frmrpt_assert(numvals.eq.p_nforms,
     :     'No of report_days differs from no of handles')
         call read_integer_array_optional(section, 'report_start',
     :                         max_files, '()',
     :                         p_report_start, numvals, 0, 30000)
         if (numvals .eq. 0) then
            call fill_integer_array(p_report_start, 1, p_nforms)
         else
            call frmrpt_assert(numvals.eq.p_nforms,
     :         'No of report_start differs from no of handles')
         end if
      end if
 
      call pop_routine(my_name)
      return
      end



*     ===========================================================
      subroutine frmrpt_report_counts()
*     ===========================================================
      implicit none
      include 'frmrpt.inc'
      include 'data.pub'
      include 'datastr.pub'
      include 'date.pub'
      include 'engine.pub'
      include 'error.pub'
      include 'intrface.pub'
      include 'license.pub'
      include 'read.pub'
      include 'science.pub'
      include 'string.pub'
      include 'write.pub'

*+  Purpose
*     Look at days past for given forms and report if it is time.

*+  Changes
*     05/06/97  SB  Created

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'frmrpt_report_counts')

*+  Local Variables
      integer err_ret
      integer i_frm    ! Loop counter.
      integer frm_file_unt

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      do 110 i_frm=1, p_nforms
 
      ! Decrement day counts.  If any get to zero, then generate output.
         g_day_count(i_frm) = g_day_count(i_frm) - 1
         if (g_day_count(i_frm) .eq. 0)  then
            g_day_count(i_frm) = p_report_days(i_frm)
 
         ! Open form file.
            frm_file_unt = get_logical_unit()
            call frmrpt_assert(frm_file_unt.ge.1, 'form_file_unt.ge.1')
            open(iostat=err_ret, file=p_form_file_name(i_frm),
     :               status='old', unit=frm_file_unt)
            call frmrpt_assert(err_ret.eq.0, 
     :            'Error opening file ' // p_form_file_name(i_frm))
            
         ! Look over form, generating output.
            call frmrpt_prcss_frm(.true.,
     :            frm_file_unt, g_out_file_unt(i_frm),
     :            p_escape_char(i_frm), i_frm)
            
         ! Finished with form file.
            close(frm_file_unt)
            
         end if
         
110   continue
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine frmrpt_do_output(handle)
*     ===========================================================
      implicit none
      include 'frmrpt.inc'
      include 'data.pub'
      include 'datastr.pub'
      include 'date.pub'
      include 'engine.pub'
      include 'error.pub'
      include 'intrface.pub'
      include 'license.pub'
      include 'read.pub'
      include 'science.pub'
      include 'string.pub'
      include 'write.pub'

*+  Sub-Program Arguments
      character*(*) handle

*+  Purpose
*     Handles a do_output message.

*+  Changes
*     05/06/97  SB  Created

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'frmrpt_do_output')

*+  Local Variables
      integer err_ret
      integer hndl_ndx
      integer frm_file_unt

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      hndl_ndx = find_string_in_array(handle, p_handle, p_nforms)
 
      if (hndl_ndx .gt. 0) then
 
      ! Open form file.
         frm_file_unt = get_logical_unit()
         call frmrpt_assert(frm_file_unt.ge.1, 'form_file_unt.ge.1')
         open(iostat=err_ret, file=p_form_file_name(hndl_ndx),
     :            status='old', unit=frm_file_unt)
         call frmrpt_assert(err_ret.eq.0, 
     :            'Error opening file ' // p_form_file_name(hndl_ndx))
         
      ! Look over form, generating output.
         call frmrpt_prcss_frm(.true.,
     :            frm_file_unt, g_out_file_unt(hndl_ndx),
     :            p_escape_char(hndl_ndx), hndl_ndx)
 
      ! Finished with form file.
         close(frm_file_unt)
 
      else
 
         call frmrpt_assert(.FALSE.,
     :         'No such file handle as "' // handle // '"' )
 
      end if
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine frmrpt_update_sumvars()
*     ===========================================================
      implicit none
      include   'const.inc'
      include   'frmrpt.inc'            ! soilph common block
      include 'data.pub'
      include 'datastr.pub'
      include 'date.pub'
      include 'engine.pub'
      include 'error.pub'
      include 'intrface.pub'
      include 'license.pub'
      include 'read.pub'
      include 'science.pub'
      include 'string.pub'
      include 'write.pub'

*+  Purpose
*     Get the values of variables to be summed from other modules.

*+  Changes
*     05/06/97  SB  Created

*+  Constant Values
      character  my_name*(*)
      parameter (my_name='frmrpt_update_sumvars')

*+  Local Variables
      integer num_vals    ! number of values returned.
      integer i_var
      real vals(max_elems)

*- Implementation Section ----------------------------------
      call push_routine(my_name)
 
      do 110, i_var=1, g_nvars
         call frmrpt_get_var(g_mdlnames(i_var), g_varnames(i_var),
     :                     vals, max_elems, num_vals)
         if (num_vals .gt. g_val_nelem(i_var))  then
            g_val_nelem(i_var) = num_vals
         end if
         if (num_vals .gt. 0)  then
            call add_real_array(vals, g_values(1,i_var), num_vals)
         end if
         g_var_day_cnt(i_var) = g_var_day_cnt(i_var) + 1
110   continue
 
      call pop_routine(my_name)
      return
      end



*     ===============================================================
      subroutine frmrpt_prcss_frm(is_show,
     :                     frm_file_unt, out_file_unt,
     :                     escape_char, frm_ndx)
*     ===============================================================
      implicit none
      include 'frmrpt.inc'
      include 'const.inc'
      include 'data.pub'
      include 'datastr.pub'
      include 'date.pub'
      include 'engine.pub'
      include 'error.pub'
      include 'intrface.pub'
      include 'license.pub'
      include 'read.pub'
      include 'science.pub'
      include 'string.pub'
      include 'write.pub'

*+  Sub-Program Arguments
      integer frm_file_unt  ! The current form file.
      integer out_file_unt  ! The unit for output.
      character*(1) escape_char   ! What delimits for substution.
      integer frm_ndx      ! Internal handle for which file.
      logical is_show  ! If 'is_show' then do output , else we
                       ! are just initialising.

*+  Purpose
*     Process the form file.  If 'is_show' then do the output, else we are
*     initialising and we must create any summation variables.

*+  Calls
      character*(max_line_len) lower_case

*+  Changes
*     05/06/97  SB  Created

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'frmrpt_prcss_frm')

*+  Local Variables
      character*(max_line_len)  in_line
      character*(1) ch
      integer ch_pos, ch_end, end_wd_pos
      integer err_ret

*- Implementation Section ----------------------------------
      call push_routine (my_name)
      
110   continue   ! WHILE a line can be got.
      read(frm_file_unt, '(A)', iostat=err_ret) in_line
      if (err_ret .ne. 0) goto 140
 
         ch_end = lastnb(in_line)
         ch_pos = 1
 
120      continue   ! WHILE there are more chars in the line.
         if (ch_pos .gt. ch_end) goto 130
 
            ch = in_line(ch_pos:ch_pos)
            if (ch .eq. escape_char)  then
               ch_pos = ch_pos+1
 
            ! Guard aganst a lonely escape character. It would cause havoc.
               if (ch_pos .gt. ch_end .or.
     :                   in_line(ch_pos:ch_pos) .eq. ' ')  then
                  call fatal_error(err_user, 
     :                       'lonely esacpe character in form file')
                  goto 140
               end if
 
            ! Process escaped word.
               end_wd_pos = index(in_line(ch_pos:ch_end), ' ')
               if (end_wd_pos .lt. 1) then
                  end_wd_pos = ch_end
               else
                  end_wd_pos = ch_pos + end_wd_pos - 1
               end if
               call frmrpt_prcss_frm_wd(
     :                     lower_case(in_line(ch_pos:end_wd_pos)),
     :                        is_show, out_file_unt, frm_ndx)
               ch_pos = end_wd_pos + 1
 
            else
               if (is_show)  then
                  write(out_file_unt, '(1H+,A1,$)') ch
               end if
               ch_pos = ch_pos + 1
            end if
 
            goto 120
130      continue   ! END WHILE
            
         if (is_show)  then
            write(out_file_unt, '(1H+)') 
            write(out_file_unt, '(1H $)') 
         end if
         goto 110
140   continue  ! END WHILE
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine frmrpt_prcss_frm_wd(word, is_show, out_file_unt,
     :                              frm_ndx)
*     ===========================================================
      implicit none
      include 'frmrpt.inc'
      include 'const.inc'
      include 'data.pub'
      include 'datastr.pub'
      include 'date.pub'
      include 'engine.pub'
      include 'error.pub'
      include 'intrface.pub'
      include 'license.pub'
      include 'read.pub'
      include 'science.pub'
      include 'string.pub'
      include 'write.pub'

*+  Sub-Program Arguments
      integer out_file_unt  ! The unit for output for the current form file.
      character*(*) word
      integer frm_ndx      ! Internal handle for which file.
      logical is_show

*+  Purpose
*     Pocess escaped word 'word'.  If we have an @ function, deal with it.

*+  Changes
*     05/06/97  SB  Created

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'frmrpt_prcss_frm_wd')

*+  Local Variables
      character*(varname_len_max) varname
      character*(varname_len_max) var
      character*(varname_len_max) mdl
      character*(varname_len_max) func
      integer ndx_at

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
   ! Get function name.
      ndx_at = index(word, '@')
      if (ndx_at .gt. 1) then   ! We have an @ function.
         call split_line(word, func, varname, '@')
      else
         func = ' '
         varname = word
      end if
 
   ! Get module name.
      ndx_at = index(word, '.')
      if (ndx_at .gt. 1) then   ! We have a module name.
         call split_line(varname, mdl, var, '.')
      else
         mdl = unknown_module
         var = varname
      end if
 
   ! Do the var.
      if (is_show) then
         call frmrpt_do_var(func, mdl, var, out_file_unt, frm_ndx)
      else if (func .ne. ' ') then
         call frmrpt_create_sumvar(mdl, var, frm_ndx)
      end if
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine frmrpt_do_var(func, mdl, var, out_file_unt,
     :                        frm_ndx)
*     ===========================================================
      implicit none
      include 'frmrpt.inc'
      include 'data.pub'
      include 'datastr.pub'
      include 'date.pub'
      include 'engine.pub'
      include 'error.pub'
      include 'intrface.pub'
      include 'license.pub'
      include 'read.pub'
      include 'science.pub'
      include 'string.pub'
      include 'write.pub'

*+  Sub-Program Arguments
      integer out_file_unt  ! The unit for output for the current form file.
      character*(*) func
      character*(*) mdl
      character*(*) var
      integer frm_ndx      ! Internal handle for which file.

*+  Purpose
*     Outputs the value for substitution.

*+  Changes
*     05/06/97  SB  Created

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'frmrpt_prcss_do_var')

*+  Local Variables
      integer num_vals
      real vals(max_elems)

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (func .ne. ' ') then   ! We have an @ function.
         call frmrpt_get_sumvar(func, mdl, var, frm_ndx,
     :                           vals, max_elems, num_vals)
         call frmrpt_show_var(vals, num_vals, out_file_unt)
      else
         call frmrpt_get_var(mdl, var, vals, max_elems, num_vals)
         call frmrpt_show_var(vals, num_vals, out_file_unt)
      end if
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine frmrpt_get_var(mdl, var, vals, max_vals, num_vals)
*     ===========================================================
      implicit none
      include 'const.inc'
      include 'data.pub'
      include 'datastr.pub'
      include 'date.pub'
      include 'engine.pub'
      include 'error.pub'
      include 'intrface.pub'
      include 'license.pub'
      include 'read.pub'
      include 'science.pub'
      include 'string.pub'
      include 'write.pub'

*+  Sub-Program Arguments
      character*(*) var
      character*(*) mdl
      integer max_vals
      integer num_vals
      real vals(max_vals)

*+  Purpose
*     Get the value for a variriable from APSIM.

*+  Changes
*     05/06/97  SB  Created

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'frmrpt_show_var')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      call get_real_array(mdl, var, max_vals,
     :                     '()', vals, num_vals, -1e9, 1e9)

      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine frmrpt_show_var(vals, num_vals, out_file_unt)
*     ===========================================================
      implicit none
      include 'frmrpt.inc'
      include 'data.pub'
      include 'datastr.pub'
      include 'date.pub'
      include 'engine.pub'
      include 'error.pub'
      include 'intrface.pub'
      include 'license.pub'
      include 'read.pub'
      include 'science.pub'
      include 'string.pub'
      include 'write.pub'

*+  Sub-Program Arguments
      integer out_file_unt  ! The unit for output for the current form file.
      integer num_vals
      real vals(num_vals)

*+  Purpose
*     Outputs values.

*+  Definition
*     Outputs the 'num_vals' elements of 'vals' to the opened output 
*     unit 'out_file_unit'.

*+  Changes
*     05/06/97  SB  Created

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'frmrpt_show_var')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      if (num_vals .gt. 1)  then
         call frmrpt_prtrv(out_file_unt, vals, num_vals)
      else if (num_vals .eq. 1)  then
         write(out_file_unt, '(1H+,G13.5,$)') vals(1)
      end if
         
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine frmrpt_create_sumvar(mdl, var, frm_ndx)
*     ===========================================================
      implicit none
      include 'frmrpt.inc'
      include 'const.inc'
      include 'data.pub'
      include 'datastr.pub'
      include 'date.pub'
      include 'engine.pub'
      include 'error.pub'
      include 'intrface.pub'
      include 'license.pub'
      include 'read.pub'
      include 'science.pub'
      include 'string.pub'
      include 'write.pub'

*+  Sub-Program Arguments
      character*(*) mdl
      character*(*) var
      integer frm_ndx

*+  Purpose
*     Makes a summation variable in the varoables table.

*+  Changes
*     05/06/97  SB  Created

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'frmrpt_create_sumvar')

*+  Local Variables
      integer var_ndx
      integer i_var

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      var_ndx = 0
      do 110, i_var=1, g_nvars
         if (g_varnames(i_var).eq.var .and.
     :                  g_mdlnames(i_var).eq.mdl .and.
     :                  g_frm_ndx(i_var).eq.frm_ndx)  then
            var_ndx = i_var
            goto 120
         end if
110   continue
120   continue
 
      if (var_ndx .eq. 0)  then
         if (g_nvars .eq. max_vars)  then
            call fatal_error(err_user, 'too many summation vars')
         else
            g_nvars = g_nvars+1
            g_varnames(g_nvars) = var
            g_mdlnames(g_nvars) = mdl
            g_frm_ndx(g_nvars) = frm_ndx
            g_val_nelem(g_nvars) = 0
         end if
      end if
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine frmrpt_get_sumvar(func, mdl, var, frm_ndx,
     :                              vals, max_vals, num_vals)
*     ===========================================================
      implicit none
      include 'frmrpt.inc'
      include 'const.inc'
      include 'data.pub'
      include 'datastr.pub'
      include 'date.pub'
      include 'engine.pub'
      include 'error.pub'
      include 'intrface.pub'
      include 'license.pub'
      include 'read.pub'
      include 'science.pub'
      include 'string.pub'
      include 'write.pub'

*+  Sub-Program Arguments
      character*(*) func
      character*(*) mdl
      character*(*) var
      integer frm_ndx
      integer max_vals
      integer num_vals
      real vals(max_vals)

*+  Purpose
*     Get the value of a summation variable form the summation variables
*     table.

*+  Changes
*     05/06/97  SB  Created

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'frmrpt_get_sumvar')

*+  Local Variables
      integer var_ndx
      integer i_var

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      var_ndx = 0
      do 110, i_var=1, g_nvars
         if (g_varnames(i_var).eq.var .and.
     :                  g_mdlnames(i_var).eq.mdl .and.
     :                  g_frm_ndx(i_var).eq.frm_ndx)  then
            var_ndx = i_var
            goto 120
         end if
110   continue
 
120   continue
      if (var_ndx .eq. 0)  then
         num_vals = 0
      else
         num_vals = g_val_nelem(var_ndx)
         call frmrpt_copy_real_arr(vals, g_values(1,var_ndx), num_vals)
         if (func .eq. 'sum') then
            ! Its fine already.
         else if (func .eq. 'avg') then
            call frmrpt_vec_scalar_mul(vals, num_vals,
     :                           1.0/g_var_day_cnt(var_ndx))
         else
            call fatal_error(ERR_USER, 'No such @function as '//func)
            num_vals = 0
         end if
      end if
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine frmrpt_clear_vars(data)
*     ===========================================================
      implicit none
      include 'frmrpt.inc'
      include 'const.inc'
      include 'data.pub'
      include 'datastr.pub'
      include 'date.pub'
      include 'engine.pub'
      include 'error.pub'
      include 'intrface.pub'
      include 'license.pub'
      include 'read.pub'
      include 'science.pub'
      include 'string.pub'
      include 'write.pub'

*+  Sub-Program Arguments
      character*(*) data

*+  Purpose
*     Clears variables.

*+  Definition
*    If "data" is a single identifier, then it will be assumed to be an
*    output file handle and all variables associated with this output file
*    will be cleared.  Otherwise, "data" must be a handle followed by a colon
*    followed by a variable name (which may have a module component) and that
*    variable in that output file will be cleared.  Clearing a variable means
*    that all values of the variable are set to zero and the day count
*    associated with the variable is set to zero.  

*+  Changes
*     05/06/97  SB  Created

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'frmrpt_clear_vars')

*+  Local Variables
      character*(varname_len_max)  handle, module, varname
      integer i_var        ! loop counter.
      integer hndl_ndx

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      call frmrpt_clr_var_prse(handle, module, varname, data)
      hndl_ndx = find_string_in_array(handle, p_handle, p_nforms)
      call frmrpt_assert(hndl_ndx .gt. 0,
     :         'No such file handle as "' // handle // '"' )
 
      if (varname .eq. BLANK) then
 
         do 120, i_var=1, g_nvars
            if (g_frm_ndx(i_var) .eq. hndl_ndx)  then
               call frmrpt_clear_var(i_var)
            end if
120      continue
 
      else
 
         do 130, i_var=1, g_nvars
            if (g_frm_ndx(i_var) .eq. hndl_ndx
     :              .AND.   g_varnames(i_var) .eq. varname
     :              .AND.   g_mdlnames(i_var) .eq. module)  then
               call frmrpt_clear_var(i_var)
            end if
130      continue
 
      end if
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine frmrpt_clear_var(var_ndx)
*     ===========================================================
      implicit none
      include 'frmrpt.inc'
      include 'data.pub'
      include 'datastr.pub'
      include 'date.pub'
      include 'engine.pub'
      include 'error.pub'
      include 'intrface.pub'
      include 'license.pub'
      include 'read.pub'
      include 'science.pub'
      include 'string.pub'
      include 'write.pub'

*+  Sub-Program Arguments
      integer var_ndx

*+  Purpose
*     Clears the variable at index "var_ndx".

*+  Changes
*     05/06/97  SB  Created

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'frmrpt_clear_var')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      call fill_real_array(g_values(1,var_ndx), 0.0, max_elems)
      g_var_day_cnt(var_ndx) = 0
      g_val_nelem(var_ndx) = 0
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine frmrpt_clr_var_prse(handle, module, varname, in_str)
*     ===========================================================
      implicit none
      include 'frmrpt.inc'
      include 'const.inc'
      include 'data.pub'
      include 'datastr.pub'
      include 'date.pub'
      include 'engine.pub'
      include 'error.pub'
      include 'intrface.pub'
      include 'license.pub'
      include 'read.pub'
      include 'science.pub'
      include 'string.pub'
      include 'write.pub'

*+  Sub-Program Arguments
      character*(*) handle    ! (OUT) output file handle.
      character*(*) module    ! (OUT) module name.
      character*(*) varname   ! (OUT) variable name.
      character*(*) in_str    ! (IN) input string.

*+  Purpose
*     Splits string into handle, module and variable name.

*+  Definition
*    "in_str" may be an input file handle OR an input file handle
*    followed by a colon followed by a variable name OR a input file
*    handle followed by a colon followed by a module name followed by a
*    full stop followed by a variable name, where the input file handle,
*    the module name and the variable names are made up of alphanumeric
*    and underscore characters.
* 
*    This subroutine assigns the input file handle to "handle" and
*    assigns the module name to "module" if there is one (else it assigns
*    UNKNOWN_MODULE to "module") and assigns the variable name to "varname" if
*    there is one (else it assigns BLANK to "varname").  

*+  Changes
*     05/06/97  SB  Created

*+  Constant Values
      character*(*) my_name           ! name of procedure
      parameter (my_name = 'frmrpt_clr_var_prse')

*+  Local Variables
      character*(varname_len_max*2+1) rhs

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      call split_line(in_str, handle, rhs, ':')
      if (rhs .eq. BLANK)  then
         module = BLANK
         varname = BLANK
      else
         call split_line(rhs, module, varname, '.')
         if (varname .eq. BLANK)  then
            varname = module
            module = UNKNOWN_MODULE 
         else
            ! We have handle, module and varname assigned correctly.
         end if
      end if
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine frmrpt_prtrv(unt, vec, nvars)
*     ===========================================================
      implicit none
      include 'data.pub'
      include 'datastr.pub'
      include 'date.pub'
      include 'engine.pub'
      include 'error.pub'
      include 'intrface.pub'
      include 'license.pub'
      include 'read.pub'
      include 'science.pub'
      include 'string.pub'
      include 'write.pub'

*+  Sub-Program Arguments
      integer unt, nvars
      real vec(nvars)

*+  Purpose
*    Prints out array 'vec' to logical unit no 'unt'.
* 'vec' has 'nvars' elements.

*+  Changes
*     05/06/97  SB  Created

*+  Constant Values
      integer llen
      parameter (llen=5)
      character*(*) my_name           ! name of procedure
      parameter (my_name = 'frmrpt_prtrv')

*+  Local Variables
      integer i, j      ! Loop counters.
      integer nlines    ! how many filled up lines to print.
      integer lcnt      ! how many variables have we printed so far.

*- Implementation Section ----------------------------------
      call push_routine(my_name)
 
      nlines = nvars/llen
      lcnt = 0
      do 10 i=1, nlines
         write(unt, '(1h+,5(1X,G13.5),$)')
     :                     (vec(j), j=lcnt+1, lcnt+llen)
         lcnt = lcnt + llen
         if (nvars .gt. lcnt) then
            write(unt, '(1H+)') 
            write(unt, '(1H $)') 
         end if
10    continue
      do 20, j=lcnt+1, nvars
         write(unt, '(1h+,1X,G13.5,$)')  vec(j)
20    continue
 
      call pop_routine(my_name)
      return
      end



*     ===========================================================
      subroutine frmrpt_assert(IsOK, WhatChkd)
*     ===========================================================
      implicit none
      include 'const.inc'              ! ERR_internal
      include 'data.pub'
      include 'datastr.pub'
      include 'date.pub'
      include 'engine.pub'
      include 'error.pub'
      include 'intrface.pub'
      include 'license.pub'
      include 'read.pub'
      include 'science.pub'
      include 'string.pub'
      include 'write.pub'

*+  Sub-Program Arguments
      character  WhatChkd*(*)     ! What test did pass or fail.
      logical IsOK         ! Did the test pass ?

*+  Purpose
*     Gives error message on failure of test "IsOK".

*+  Changes
*     05/06/97  SB  Created

*+  Constant Values
      character  my_name*(*)
      parameter (my_name='frmrpt_assert')

*- Implementation Section ----------------------------------
      call push_routine(my_name)
 
      if (.not. IsOK) then
         call fatal_error(ERR_USER, 'ASSERT FAIL: '//WhatChkd)
         print *, 'ASSERT FAIL: '//WhatChkd
      end if
 
      call pop_routine(my_name)
      return
      end



*     ===========================================================
      subroutine frmrpt_copy_real_arr(dest, src, n)
*     ===========================================================
      implicit none
      include 'data.pub'
      include 'datastr.pub'
      include 'date.pub'
      include 'engine.pub'
      include 'error.pub'
      include 'intrface.pub'
      include 'license.pub'
      include 'read.pub'
      include 'science.pub'
      include 'string.pub'
      include 'write.pub'

*+  Sub-Program Arguments
      integer n      ! (IN) Size of 'dest' and 'src'.
      real dest(n)   ! (OUT) Destination array.
      real src(n)   ! (IN) Source array.

*+  Purpose
*     Copies each element of 'src' to its corresponding element in 'dest'.

*+  Changes
*     05/06/97  SB  Created

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'frmrpt_copy_real_arr')

*+  Local Variables
      integer i     ! Loop counter.

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      do 10, i=1, n
         dest(i) = src(i)
10    continue
 
      call pop_routine (my_name)
      return
      end



*     ===========================================================
      subroutine frmrpt_vec_scalar_mul(vec, n, mul)
*     ===========================================================
      implicit none
      include 'data.pub'
      include 'datastr.pub'
      include 'date.pub'
      include 'engine.pub'
      include 'error.pub'
      include 'intrface.pub'
      include 'license.pub'
      include 'read.pub'
      include 'science.pub'
      include 'string.pub'
      include 'write.pub'

*+  Sub-Program Arguments
      integer n      ! (IN) Size of 'vec'.
      real vec(n)    ! (IN/OUT) Vector to be multipied by 'mul'.
      real mul       ! (IN) Value that 'vec' gets mutiplied by.

*+  Purpose
*     Multiplies each element of 'vec' by 'mul'.

*+  Changes
*     05/06/97  SB  Created

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'frmrpt_vec_scalar_mul')

*+  Local Variables
      integer i

*- Implementation Section ----------------------------------
      call push_routine (my_name)
 
      do 10, i=1, n
         vec(i) = vec(i) * mul
10    continue
 
      call pop_routine (my_name)
      return
      end



