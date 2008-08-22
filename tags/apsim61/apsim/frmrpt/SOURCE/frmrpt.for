      module FrmrptModule
      use Registrations
      
!   Constant values
      integer max_vars, varname_len_max
      integer max_elems
      integer units_len_max
      integer file_name_max
      integer max_line_len
      integer max_files
      parameter (max_vars=40, varname_len_max=30)
      parameter (max_elems=20)
      parameter (max_files=5)
      parameter (units_len_max=30)
      parameter (file_name_max=130)
      parameter (max_line_len=200)

      integer LU_FORM_FILE
      parameter (LU_FORM_FILE=69)

      type FrmrptGlobals
         sequence
         real values(max_elems,max_vars)

         character*(varname_len_max) varnames(max_vars)
         character*(varname_len_max) mdlnames(max_vars)
         character*(200) err_msg

         integer nvars
         integer var_day_cnt(max_vars)
         integer val_nelem(max_vars)
         integer frm_ndx(max_vars)
         integer day_count(max_files)
         integer out_file_unt(max_files)
         character out_line*(max_line_len)
         integer out_line_pos

      end type FrmrptGlobals

      type FrmrptParameters
         sequence
         character*(varname_len_max) handle(max_files)
         character*(file_name_max) out_file_name(max_files)
         character*(file_name_max) form_file_name(max_files)
         character*(1) escape_char(max_files)

         integer nforms
         integer report_start(max_files)
         integer report_days(max_files)
      end type FrmrptParameters

      ! instance variables.
      common /InstancePointers/ ID,g,p,c
      save InstancePointers
      type (FrmrptGlobals),pointer :: g
      type (FrmrptParameters),pointer :: p
      type (IDsType), pointer :: id


      contains


*     ===========================================================
      subroutine frmrpt_end_run()
*     ===========================================================
      Use infrastructure
      implicit none

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

      do 110, i_frm=1, p%nforms
         close(g%out_file_unt(i_frm))
110   continue

      call pop_routine (my_name)
      return
      end subroutine



* ====================================================================
       subroutine frmrpt_Init ()
* ====================================================================
      Use infrastructure
      implicit none

*+  Purpose
*      Initialise FrmRpt module.

*+  Changes
*     05/06/97  SB  Created
*     07/5/99 removed version report c186

*+  Constant Values
      character This_routine*(*)       ! Name of this routine
      parameter (This_routine='frmrpt_init')

      integer LU_BASE
      parameter (LU_BASE=70)

*+  Local Variables
      integer i_var  ! Loop counters.
      integer i_frm  ! Loop counters.
      integer err_ret  ! To check status of opening files.
      integer frm_file_unt

*- Implementation Section ----------------------------------
      call push_routine(This_routine)

*  Initialise summation storage.
      g%nvars = 0
      do 110, i_var=1, max_vars
         g%val_nelem(i_var) = 0
         g%var_day_cnt(i_var) = 0
         call fill_real_array(g%values(1,i_var), 0.0, max_elems)
110   continue

      do 150, i_frm=1, p%nforms
         g%day_count(i_frm) = p%report_start(i_frm)

*  Open output files and create summation vars.
      ! Open output file.
         g%out_file_unt(i_frm) = LU_BASE + i_frm
         call frmrpt_assert(g%out_file_unt(i_frm).ge.1,
     :                        'out_file_unt.ge.1')
         open(iostat=err_ret, file=p%out_file_name(i_frm),
     :            status='unknown', unit=g%out_file_unt(i_frm))
         call frmrpt_assert(err_ret.eq.0,
     :            'Error opening file ' // p%out_file_name(i_frm))

      ! Open form file.
         frm_file_unt = LU_FORM_FILE
         call frmrpt_assert(frm_file_unt.ge.1, 'form_file_unt.ge.1')
         open(iostat=err_ret, file=p%form_file_name(i_frm),
     :            status='old', unit=frm_file_unt)
         call frmrpt_assert(err_ret.eq.0,
     :            'Error opening file ' // p%form_file_name(i_frm))

      ! Look over form, creating any summation variables.
         call frmrpt_prcss_frm(.false.,
     :            frm_file_unt, g%out_file_unt(i_frm),
     :            p%escape_char(i_frm), i_frm)

      ! Finished with form file.
         close(frm_file_unt)

150   continue

      call pop_routine(This_routine)
      return
      end subroutine



*     ===========================================================
      subroutine frmrpt_read_param()
*     ===========================================================
      Use infrastructure
      implicit none

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

      call write_string(new_line//'   - Reading Parameters')

*  Handles.
      call read_char_array(section, 'handle', max_files,
     :                  '()', p%handle, p%nforms)

*  File names.
      call read_char_array(section, 'form_file', max_files,
     :                  '()', p%form_file_name, numvals)
      call frmrpt_assert(numvals.eq.p%nforms,
     :   'No of form_file differs from no of handles')

      call read_char_array(section, 'out_file', max_files,
     :                    '()', p%out_file_name, numvals)
      call frmrpt_assert(numvals.eq.p%nforms,
     :   'No of out_file differs from no of handles')

*  Escape characters.
      call read_char_array_optional(section, 'escape_char', max_files,
     :                  '()', p%escape_char, numvals)
      if (numvals .eq. 0)  then
         call fill_char_array(p%escape_char, '$', p%nforms)
      else
         call frmrpt_assert(numvals.eq.p%nforms,
     :     'No of esacpe_char differs from no of handles')
      end if

*  Reporting frequency.
      call read_integer_array_optional(section, 'report_days',
     :                         max_files, '()',
     :                         p%report_days, numvals, 0, 30000)
      if (numvals .eq. 0)  then
         call fill_integer_array(p%report_days, 0, p%nforms)
         call fill_integer_array(p%report_start, 0, p%nforms)
      else
         call frmrpt_assert(numvals.eq.p%nforms,
     :     'No of report_days differs from no of handles')
         call read_integer_array_optional(section, 'report_start',
     :                         max_files, '()',
     :                         p%report_start, numvals, 0, 30000)
         if (numvals .eq. 0) then
            call fill_integer_array(p%report_start, 1, p%nforms)
         else
            call frmrpt_assert(numvals.eq.p%nforms,
     :         'No of report_start differs from no of handles')
         end if
      end if

      call pop_routine(my_name)
      return
      end subroutine



*     ===========================================================
      subroutine frmrpt_report_counts()
*     ===========================================================
      Use infrastructure
      implicit none

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

      do 110 i_frm=1, p%nforms

      ! Decrement day counts.  If any get to zero, then generate output.
         g%day_count(i_frm) = g%day_count(i_frm) - 1
         if (g%day_count(i_frm) .eq. 0)  then
            g%day_count(i_frm) = p%report_days(i_frm)

         ! Open form file.
            frm_file_unt = LU_FORM_FILE
            call frmrpt_assert(frm_file_unt.ge.1, 'form_file_unt.ge.1')
            open(iostat=err_ret, file=p%form_file_name(i_frm),
     :               status='old', unit=frm_file_unt)
            call frmrpt_assert(err_ret.eq.0,
     :            'Error opening file ' // p%form_file_name(i_frm))

         ! Look over form, generating output.
            call frmrpt_prcss_frm(.true.,
     :            frm_file_unt, g%out_file_unt(i_frm),
     :            p%escape_char(i_frm), i_frm)

         ! Finished with form file.
            close(frm_file_unt)

         end if

110   continue

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine frmrpt_do_output()
*     ===========================================================
      Use infrastructure
      implicit none

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
      integer numvals
      character*100 handle

*- Implementation Section ----------------------------------
      call push_routine (my_name)

!      if (keyword == 'name') then
         call collect_char_var ('name', '()'
     :                      , handle, numvals)

      if (numvals.le.0) then
         !
         handle = ' '
      else
      endif


      hndl_ndx = find_string_in_array(handle, p%handle, p%nforms)

      if (hndl_ndx .gt. 0) then

      ! Open form file.
         frm_file_unt = LU_FORM_FILE
         call frmrpt_assert(frm_file_unt.ge.1, 'form_file_unt.ge.1')
         open(iostat=err_ret, file=p%form_file_name(hndl_ndx),
     :            status='old', unit=frm_file_unt)
         call frmrpt_assert(err_ret.eq.0,
     :            'Error opening file ' // p%form_file_name(hndl_ndx))

      ! Look over form, generating output.
         call frmrpt_prcss_frm(.true.,
     :            frm_file_unt, g%out_file_unt(hndl_ndx),
     :            p%escape_char(hndl_ndx), hndl_ndx)

      ! Finished with form file.
         close(frm_file_unt)

      else

         call frmrpt_assert(.FALSE.,
     :         'No such file handle as "' // handle // '"' )

      end if

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine frmrpt_update_sumvars()
*     ===========================================================
      Use infrastructure
      implicit none

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

      do 110, i_var=1, g%nvars
         call frmrpt_get_var(g%mdlnames(i_var), g%varnames(i_var),
     :                     vals, max_elems, num_vals)
         if (num_vals .gt. g%val_nelem(i_var))  then
            g%val_nelem(i_var) = num_vals
         end if
         if (num_vals .gt. 0)  then
            call add_real_array(vals, g%values(1,i_var), num_vals)
         end if
         g%var_day_cnt(i_var) = g%var_day_cnt(i_var) + 1
110   continue

      call pop_routine(my_name)
      return
      end subroutine



*     ===============================================================
      subroutine frmrpt_prcss_frm(is_show,
     :                     frm_file_unt, out_file_unt,
     :                     escape_char, frm_ndx)
*     ===============================================================
      Use infrastructure
      implicit none

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

         g%out_line_pos = 1
         g%out_line = ' '
         ch_end = len_trim(in_line)
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
                  g%out_line(g%out_line_pos:) = ch
                  g%out_line_pos = g%out_line_pos + 1
               end if
               ch_pos = ch_pos + 1
            end if

            goto 120
130      continue   ! END WHILE

         if (is_show)  then
            write(out_file_unt, *) trim(g%out_line)
         end if
         goto 110
140   continue  ! END WHILE

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine frmrpt_prcss_frm_wd(word, is_show, out_file_unt,
     :                              frm_ndx)
*     ===========================================================
      Use infrastructure
      implicit none

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
         mdl = '?'
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
      end subroutine



*     ===========================================================
      subroutine frmrpt_do_var(func, mdl, var, out_file_unt,
     :                        frm_ndx)
*     ===========================================================
      Use infrastructure
      implicit none

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
      end subroutine



*     ===========================================================
      subroutine frmrpt_get_var(mdl, var, vals, max_vals, num_vals)
*     ===========================================================
      Use infrastructure
      implicit none

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

      integer componentID
      logical ok
      integer regID

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (mdl .eq. '?') then
         componentID = Unknown_module
      else
         ok = component_name_to_id(mdl, componentID)
      endif
      call get_real_array(componentID, var, max_vals,
     :                     '()', vals, num_vals, -1e9, 1e9)

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine frmrpt_show_var(vals, num_vals, out_file_unt)
*     ===========================================================
      Use infrastructure
      implicit none

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
         write(g%out_line(g%out_line_pos:), '(G13.5)') vals(1)
         g%out_line_pos = g%out_line_pos + 13
      end if

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine frmrpt_create_sumvar(mdl, var, frm_ndx)
*     ===========================================================
      Use infrastructure
      implicit none

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
      do 110, i_var=1, g%nvars
         if (g%varnames(i_var).eq.var .and.
     :                  g%mdlnames(i_var).eq.mdl .and.
     :                  g%frm_ndx(i_var).eq.frm_ndx)  then
            var_ndx = i_var
            goto 120
         end if
110   continue
120   continue

      if (var_ndx .eq. 0)  then
         if (g%nvars .eq. max_vars)  then
            call fatal_error(err_user, 'too many summation vars')
         else
            g%nvars = g%nvars+1
            g%varnames(g%nvars) = var
            g%mdlnames(g%nvars) = mdl
            g%frm_ndx(g%nvars) = frm_ndx
            g%val_nelem(g%nvars) = 0
         end if
      end if

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine frmrpt_get_sumvar(func, mdl, var, frm_ndx,
     :                              vals, max_vals, num_vals)
*     ===========================================================
      Use infrastructure
      implicit none

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
      do 110, i_var=1, g%nvars
         if (g%varnames(i_var).eq.var .and.
     :                  g%mdlnames(i_var).eq.mdl .and.
     :                  g%frm_ndx(i_var).eq.frm_ndx)  then
            var_ndx = i_var
            goto 120
         end if
110   continue

120   continue
      if (var_ndx .eq. 0)  then
         num_vals = 0
      else
         num_vals = g%val_nelem(var_ndx)
         call frmrpt_copy_real_arr(vals, g%values(1,var_ndx), num_vals)
         if (func .eq. 'sum') then
            ! Its fine already.
         else if (func .eq. 'avg') then
            call frmrpt_vec_scalar_mul(vals, num_vals,
     :                           1.0/g%var_day_cnt(var_ndx))
         else
            call fatal_error(ERR_USER, 'No such @function as '//func)
            num_vals = 0
         end if
      end if

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine frmrpt_clear_vars()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments

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
      integer numvals
      character*100 data

*- Implementation Section ----------------------------------
      call push_routine (my_name)

         call collect_char_var ('name', '()'
     :                      , data, numvals)

      if (numvals.le.0) then
         !
         data = ' '
      else
      endif


      call frmrpt_clr_var_prse(handle, module, varname, data)
      hndl_ndx = find_string_in_array(handle, p%handle, p%nforms)
      call frmrpt_assert(hndl_ndx .gt. 0,
     :         'No such file handle as "' // handle // '"' )

      if (varname .eq. BLANK) then

         do 120, i_var=1, g%nvars
            if (g%frm_ndx(i_var) .eq. hndl_ndx)  then
               call frmrpt_clear_var(i_var)
            end if
120      continue

      else

         do 130, i_var=1, g%nvars
            if (g%frm_ndx(i_var) .eq. hndl_ndx
     :              .AND.   g%varnames(i_var) .eq. varname
     :              .AND.   g%mdlnames(i_var) .eq. module)  then
               call frmrpt_clear_var(i_var)
            end if
130      continue

      end if

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine frmrpt_clear_var(var_ndx)
*     ===========================================================
      Use infrastructure
      implicit none

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

      call fill_real_array(g%values(1,var_ndx), 0.0, max_elems)
      g%var_day_cnt(var_ndx) = 0
      g%val_nelem(var_ndx) = 0

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine frmrpt_clr_var_prse(handle, module, varname, in_str)
*     ===========================================================
      Use infrastructure
      implicit none

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
            module = '?'
         else
            ! We have handle, module and varname assigned correctly.
         end if
      end if

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine frmrpt_prtrv(unt, vec, nvars)
*     ===========================================================
      Use infrastructure
      implicit none

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
         write(g%out_line(g%out_line_pos:), '(5(1X,G13.5))')
     :                     (vec(j), j=lcnt+1, lcnt+llen)
         g%out_line_pos = g%out_line_pos + (llen*14)
         lcnt = lcnt + llen
         if (nvars .gt. lcnt) then
            write(unt, *) trim(g%out_line)
            g%out_line = ' '
            g%out_line_pos = 1
         end if
10    continue
      do 20, j=lcnt+1, nvars
         write(g%out_line(g%out_line_pos:), '(1X,G13.5)')  vec(j)
         g%out_line_pos = g%out_line_pos + 14
20    continue

      call pop_routine(my_name)
      return
      end subroutine



*     ===========================================================
      subroutine frmrpt_assert(IsOK, WhatChkd)
*     ===========================================================
      Use infrastructure
      implicit none

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
      end subroutine



*     ===========================================================
      subroutine frmrpt_copy_real_arr(dest, src, n)
*     ===========================================================
      Use infrastructure
      implicit none

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
      end subroutine



*     ===========================================================
      subroutine frmrpt_vec_scalar_mul(vec, n, mul)
*     ===========================================================
      Use infrastructure
      implicit none

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
      end subroutine



      end module FrmrptModule

!     ===========================================================
      subroutine alloc_dealloc_instance(doAllocate)
!     ===========================================================
      use FrmrptModule
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
         allocate(id)
      else
         deallocate(g)
         deallocate(p)
         deallocate(id)
      end if
      return
      end subroutine



* ====================================================================
       subroutine Main(Action, data)
* ====================================================================
      Use infrastructure
      Use FrmrptModule
      implicit none
      ml_external Main

*+  Sub-Program Arguments
       character Action*(*)            ! Message action to perform
       character data*(*)              ! Message data

*+  Purpose
*      This module reads in FrmRpt data from input file.

*+  Changes
*     05/06/97  SB  Created
*     07/5/99 removed version and presence c186

*+  Calls

*- Implementation Section ----------------------------------
      if (Action.eq.ACTION_Init) then
         call frmrpt_read_param()
         call frmrpt_init()

      else if (Action.eq.ACTION_Create) then
         call doRegistrations(id)

      else if (Action.eq.ACTION_Process) then
         call frmrpt_update_sumvars()
         call frmrpt_report_counts()

      else if (Action.eq.'do_output') then
         call frmrpt_do_output()

      else if (Action.eq.'clear') then
         call frmrpt_clear_vars()

      else if (Action.eq.ACTION_End_run) then
         call frmrpt_end_run()

      else
         call Message_unused()        ! Don't use message

      endif

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

