*===========================================================
      character*(*) function correl_version ()
*===========================================================
      implicit none
      include 'error.pub'                         

*+  Purpose
*             return version number of correl module

*+  Changes
*       210995 jngh programmed

*+  Constant Values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'correl_version')
*
      character  version_number*(*)    ! version number of module
      parameter (version_number = 'V1.0 27/02/98')

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      correl_version = version_number
 
      call pop_routine (myname)
      return
      end



*====================================================================
      subroutine apsim_Correl (Action, Data_string)
*====================================================================
      implicit none
      dll_export apsim_correl
      include   'const.inc'            ! Global constant definitions
      include   'correl.inc'               ! correl common block
      include 'engine.pub'                        
      include 'error.pub'                         

*+  Sub-Program Arguments
      character  Action*(*)            ! Message action to perform
      character  Data_string*(*)       ! Message data

*+  Purpose
*      This routine is the interface between the main system and the
*      correl module.

*+  Changes
*       210995 jngh programmed
*       090696 jngh changed presence report to standard

*+  Calls
      character  correl_version*20         ! function

*+  Constant Values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'apsim_correl')

*+  Local Variables
      character  module_name*8         ! name of this module

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      if (Action.eq.MES_Presence) then
         call get_current_module (Module_name)
         write(*, *) 'Module_name = ', Module_name
     :              // ', Version : ' // correl_version()
 
      elseif (Action.eq.MES_Init) then
         call correl_zero_variables ()
         call correl_init ()
         call correl_get_other_variables_init ()
 
      elseif (Action.eq.MES_Prepare) then
         call correl_prepare ()
 
      elseif (Action.eq.MES_Get_variable) then
         call correl_send_my_variable (Data_string)
 
      elseif (Action.eq.MES_Process) then
         call correl_zero_daily_variables ()
         call correl_get_other_variables ()
         call correl_process ()
 
      elseif (Action.eq.MES_End_Run) then
         call correl_calculate (Data_string)
 
      else
            ! don't use message
         call Message_unused ()
 
      endif
 
      call pop_routine (myname)
      return
      end



*====================================================================
      subroutine correl_zero_variables ()
*====================================================================
      implicit none
      include   'const.inc'            ! Global constant definitions
      include   'correl.inc'               ! correl common block
      include 'error.pub'                         

*+  Purpose
*     Set all variables in this module to zero.

*+  Changes
*       210995 jngh programmed

*+  Constant Values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'correl_zero_variables')

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
         !variables
 
      g_records = 0
 
      call correl_zero_daily_variables ()
 
      call pop_routine (myname)
 
      return
      end



*====================================================================
      subroutine correl_zero_daily_variables ()
*====================================================================
      implicit none
      include    'correl.inc'              ! correl common block
      include 'error.pub'                         

*+  Purpose
*     Set all variables in this module to zero.

*+  Changes
*       210995 jngh programmed

*+  Constant Values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'correl_zero_daily_variables')

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
 
      call pop_routine (myname)
 
      return
      end



*====================================================================
      subroutine correl_init ()
*====================================================================
      implicit none
      include 'write.pub'                         
      include 'error.pub'                         

*+  Purpose
*      Initialise correl module

*+  Changes
*       210995 jngh programmed

*+  Calls
      character  correl_version*15         ! function

*+  Constant Values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'correl_init')

*+  Local Variables
      character  Event_string*40       ! String to output

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
         ! notify system that we have initialised
 
      Event_string = 'Initialising Version : ' // correl_version ()
      call report_event (Event_string)
 
         ! get all parameters from parameter file
 
      call correl_read_param ()
 
         ! get all constants from constants file
 
      call correl_read_constants ()
 
      call pop_routine (myname)
 
      return
      end



*===========================================================
      subroutine correl_read_param ()
*===========================================================
      implicit none
      include   'const.inc'            ! Global constant definitions
      include   'correl.inc'               ! correl common block
      include 'read.pub'                          
      include 'write.pub'                         
      include 'error.pub'                         

*+  Purpose
*       Read all module parameters.

*+  Changes
*       210995 jngh programmed
*       020398 jngh added in reading and reporting for reference height,
*                   disp and Z0 at instrument site.
*                   changed z0soil to mm

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'correl_read_param')
*
      character section_name*(*)
      parameter (section_name = 'parameters')

*+  Local Variables
      integer    numvals               ! number of values read
c      character  line*80               ! output string

*- Implementation Section ----------------------------------
 
      call push_routine (myname)
 
      call write_string (lu_scr_sum
     :          ,new_line//'   - Reading correl Parameters')
 
         ! offset days
      call read_integer_var (
     :           section_name
     :          ,'offset'
     :          ,'(-)'
     :          ,p_offset
     :          ,numvals
     :          ,0
     :          ,offset)
 
         ! met parameter names
      call read_char_array (
     :           section_name
     :          ,'names'
     :          ,numvars
     :          ,'(-)'
     :          ,p_names
     :          ,p_numvars)
 
      call pop_routine  (myname)
      return
      end



*===========================================================
      subroutine correl_read_constants ()
*===========================================================
      implicit none
      include   'const.inc'            ! Global constant definitions
      include   'correl.inc'               ! correl common block
      include 'error.pub'                         

*+  Purpose
*       Read all module constants.

*+  Changes
*       210995 jngh programmed

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'correl_read_constants')
*
      character section_name*(*)
      parameter (section_name = 'constants')

*+  Local Variables
      integer    numvals               ! number of values read

*- Implementation Section ----------------------------------
 
      call push_routine (myname)
 
      call pop_routine  (myname)
      return
      end



*================================================================
      subroutine correl_prepare ()
*================================================================
      implicit none
      include 'error.pub'                         

*+  Purpose
*     perform calculations before the current timestep.

*+  Changes
*       210995 jngh programmed

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'correl_prepare')

*- Implementation Section ----------------------------------
 
      call push_routine (myname)
 
      call pop_routine (myname)
      return
      end



*====================================================================
      subroutine correl_get_other_variables_init ()
*====================================================================
      implicit none
      include   'const.inc'            ! Constant definitions
      include   'convert.inc'          ! conversion constants
      include   'correl.inc'               ! correl common block
      include 'intrface.pub'                      
      include 'error.pub'                         

*+  Purpose
*      Get the values of variables from other modules

*+  Changes
*       210995 jngh programmed

*+  Constant Values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'correl_get_other_variables_init')

*+  Local Variables
      integer    numvals               ! number of values returned
      integer    i
      integer    j
      real       value

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      g_numvars = 0
      do i=1, p_numvars
      if (p_names(i).ne.blank) then
 
      call get_real_var_optional (
     :      unknown_module
     :     ,p_names(i)
     :     ,'()'
     :     ,value
     :     ,numvals
     :     ,-1000.0
     :     ,10000.0)
 
         if (numvals.gt.0) then
            g_numvars = g_numvars + 1
            g_name_found(g_numvars) = p_names(i)
         else
         endif
 
      else
      endif
 
      end do
 
      call pop_routine (myname)
      return
      end



*====================================================================
      subroutine correl_get_other_variables ()
*====================================================================
      implicit none
      include   'const.inc'            ! Constant definitions
      include   'convert.inc'          ! conversion constants
      include   'correl.inc'               ! correl common block
      include 'intrface.pub'                      
      include 'error.pub'                         

*+  Purpose
*      Get the values of variables from other modules

*+  Changes
*       210995 jngh programmed

*+  Constant Values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'correl_get_other_variables')

*+  Local Variables
      integer    numvals               ! number of values returned
      integer    i

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      g_records = g_records + 1
      do i=1, g_numvars
 
         call get_real_var_optional (
     :      unknown_module
     :     ,g_name_found(i)
     :     ,'()'
     :     ,g_values(g_records,i)
     :     ,numvals
     :     ,-1000.0
     :     ,10000.0)
 
      end do
 
      call pop_routine (myname)
      return
      end



*====================================================================
      subroutine correl_send_my_variable (Variable_name)
*====================================================================
      implicit none
      include   'correl.inc'               ! correl common block
      include 'engine.pub'                        
      include 'error.pub'                         

*+  Sub-Program Arguments
      character  Variable_name*(*)     ! (INPUT) Variable name to search for

*+  Purpose
*            return the value of one of our variables to       caller

*+  Changes
*       210995 jngh programmed

*+  Constant Values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'correl_send_my_variable')

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
         call Message_unused ()
 
      call pop_routine (myname)
      return
      end



*================================================================
      subroutine correl_process ()
*================================================================
      implicit none
      include 'error.pub'                         

*+  Purpose
*      perform actions for current day

*+  Changes
*       210995 jngh programmed

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'correl_process')

*- Implementation Section ----------------------------------
 
      call push_routine (myname)
 
      call pop_routine (myname)
      return
      end



*================================================================
      subroutine correl_calculate (variable_name)
*================================================================
      implicit none
      include   'correl.inc'               ! correl common block
      include   'const.inc'
      include 'string.pub'                        
      include 'write.pub'                         
      include 'error.pub'                         

*+  Sub-Program Arguments
      character  Variable_name*(*)     ! (INPUT) Variable name to search for

*+  Purpose
*      perform actions for current day

*+  Changes
*       210995 jngh programmed

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'correl_calculate')

*+  Local Variables
      real     cor(numvars,numvars,offset*2+1)
      integer  i
      integer  j
      integer  k
      integer  n
      integer  nr
      double precision     sum
      double precision     sumsqx(numvars)
      double precision     diffx(numvars)
      double precision     sumsqy(numvars)
      double precision     diffy(numvars)
      double precision     mean(numvars)
      double precision     sumprod(numvars,numvars)
      character string*1000

*- Implementation Section ----------------------------------
 
      call push_routine (myname)
 
 
      do n = 1, p_offset*2+1
         do i = 1, g_numvars
            sum = 0.0
            do nr = p_offset+1, g_records-p_offset
               sum = sum + g_values(nr,i)
            end do
 
            mean(i) = sum/g_records
            sumsqx(i) = 0.0
            sumsqy(i) = 0.0
            do j=1,g_numvars
               sumprod(i,j) = 0.0
            end do
 
 
         end do
 
            ! correlate on next day
 
         do nr = p_offset+1, g_records-p_offset
            do i = 1, g_numvars
               diffx(i) = g_values(nr,i) - mean(i)
               diffy(i) = g_values(nr+n-(p_offset+1),i) - mean(i)
               sumsqx(i) = sumsqx(i) + diffx(i)**2
               sumsqy(i) = sumsqy(i) + diffy(i)**2
            end do
            do i=1,g_numvars
               do j=1,g_numvars
               sumprod(i,j) = sumprod(i,j) + diffx(i)*diffy(j)
               end do
            end do
         end do
 
         do i=1,g_numvars
            if (sumsqx(i).ne.0.0) then
            do j=1,g_numvars
               if (sumsqy(j).ne.0.0) then
               cor(i,j,n) =  sumprod(i,j)
     :                  / sqrt(sumsqx(i)*sumsqy(j) )
               else
                  cor(i,j,n) = 0.0
               endif
 
            end do
            else
               do j=1,g_numvars
               cor(i,j,n) = 0.0
               end do
            endif
         end do
      end do
 
      open (1000, 'correl.out')
 
      string = blank
      write (string, '(10x, 100a7)') (g_name_found(i), i=1,g_numvars)
      call write_string (lu_scr_sum, string)
      write (1000, *) trim(string)
      do i=1,g_numvars
         write (string, '(1x, a7)') g_name_found(i)
         call write_string (lu_scr_sum, string)
         write (1000, *) trim(string)
         do k = 1,p_offset*2+1
            write (string, '(2x, a3, sp, i2, ss, 700f7.2)')
     :            'day',k-(p_offset+1), (cor(i,j,k), j=1,g_numvars)
            call write_string (lu_scr_sum, string)
            write (1000, *) trim(string)
         end do
      end do
 
      close (1000)
 
      call pop_routine (myname)
      return
      end



