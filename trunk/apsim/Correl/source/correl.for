*===========================================================
      character*(*) function correl_version ()
*===========================================================

*   Short description:
*             return version number of correl module

*   Assumptions:
*       none

*   Notes:
*       none

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <=20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       include
*                       implicit none

*   Changes:
*       210995 jngh programmed

*   Calls:
*       pop_routine
*       push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
*       none

*   Internal variables
*       none

*   Constant values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'correl_version')

      character  version_number*(*)    ! version number of module
      parameter (version_number = 'V1.0 27/02/98')

*   Initial data values
*       none

* --------------------- Executable code section ----------------------
      call push_routine (myname)

      correl_version = version_number

      call pop_routine (myname)
      return
      end

*====================================================================
      subroutine apsim_Correl (Action, Data_string)
*====================================================================

*   Short description:
*      This routine is the interface between the main system and the
*      correl module.

*   Assumptions:
*       none

*   Notes:
*       none

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <=20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       include
*                       implicit none

*   Changes:
*       210995 jngh programmed
*       090696 jngh changed presence report to standard

*   Calls:
*       correl_zero_variables
*       correl_zero_daily_variables
*       correl_init
*       correl_get_other_variables
*       correl_prepare
*       correl_process
*       correl_set_other_variables
*       correl_send_my_variable
*       get_current_module
*       message_unused
*       pop_routine
*       push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   subroutine arguments
      character  Action*(*)            ! Message action to perform
      character  Data_string*(*)       ! Message data

*   Global variables
      include   'const.inc'            ! Global constant definitions
      include   'correl.inc'               ! correl common block

      character  correl_version*20         ! function

*   Internal variables
      character  module_name*8         ! name of this module

*   Constant values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'apsim_correl')

*   Initial data values
*      None

* --------------------- Executable code section ----------------------
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

*   Short description:
*     Set all variables in this module to zero.

*   Assumptions:
*      None

*   Notes:
*       none

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <=20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       include
*                       implicit none

*   Changes:
*       210995 jngh programmed

*   Calls:
*       pop_routine
*       push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*      none

*   Global variables
      include   'const.inc'            ! Global constant definitions
      include   'correl.inc'               ! correl common block

*   Internal variables
*      none

*   Constant values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'correl_zero_variables')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------
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

*   Short description:
*     Set all variables in this module to zero.

*   Assumptions:
*      None

*   Notes:
*       none

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <=20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       include
*                       implicit none

*   Changes:
*       210995 jngh programmed

*   Calls:
*       pop_routine
*       push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*      none

*   Global variables
      include    'correl.inc'              ! correl common block

*   Internal variables
*      none

*   Constant values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'correl_zero_daily_variables')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------
      call push_routine (myname)


      call pop_routine (myname)

      return
      end
*====================================================================
      subroutine correl_init ()
*====================================================================

*   Short description:
*      Initialise correl module

*   Assumptions:
*      None

*   Notes:
*       none

*   Procedure attributes:
*      Version:         Any hardwfare/Fortran77
*      Extensions:      Long names <=20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       include
*                       implicit none

*   Changes:
*       210995 jngh programmed

*   calls:
*       correl_version
*       correl_read_param 
*       correl_read_constants 
*       correl_get_other_var_ini
*       report_event
*       pop_routine
*       push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*      none

*   Global variables
      character  correl_version*15         ! function

*   Internal variables
      character  Event_string*40       ! String to output

*   Constant values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'correl_init')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------
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

*   Short description:
*       Read all module parameters.

*   Assumptions:
*       none

*   Notes:
*       none

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <=20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       include
*                       implicit none

*   Changes:
*       210995 jngh programmed
*       020398 jngh added in reading and reporting for reference height,
*                   disp and Z0 at instrument site.
*                   changed z0soil to mm

*   Calls:
*       pop_routine
*       push_routine
*       read_char_var
*       read_real_var

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'const.inc'            ! Global constant definitions
      include   'correl.inc'               ! correl common block

*   Internal variables
      integer    numvals               ! number of values read
c      character  line*80               ! output string

*   Constant values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'correl_read_param')

      character section_name*(*)
      parameter (section_name = 'parameters')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

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

*   Short description:
*       Read all module constants.

*   Assumptions:
*       none

*   Notes:
*       none

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <=20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       include
*                       implicit none

*   Changes:
*       210995 jngh programmed

*   Calls:
*       pop_routine
*       push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*       none

*   Global variables
      include   'const.inc'            ! Global constant definitions
      include   'correl.inc'               ! correl common block

*   Internal variables
      integer    numvals               ! number of values read

*   Constant values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'correl_read_constants')

      character section_name*(*)
      parameter (section_name = 'constants')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      call push_routine (myname)

      call pop_routine  (myname)
      return
      end

*================================================================
      subroutine correl_prepare ()
*================================================================

*   short description:
*     perform calculations before the current timestep.

*   assumptions:
*      none

*   notes:
*     none

*   procedure attributes:
*      version:         any hardware/fortran77
*      extensions:      long names <=20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                             implicit   none

*   changes:
*       210995 jngh programmed

*   Calls:
*       correl_pen_mon
*       pop_routine
*       push_routine

* ----------------------- declaration section ------------------------

      implicit   none

*   Subroutine arguments
*      none

*   global variables
*      none

*   internal variables
*      none

*   constant values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'correl_prepare')

*   initial data values
*      none

* --------------------- executable code section ----------------------

      call push_routine (myname)

      call pop_routine (myname)
      return
      end
*====================================================================
      subroutine correl_get_other_variables_init ()
*====================================================================

*   Short description:
*      Get the values of variables from other modules

*   Assumptions:
*      None

*   Notes:
*       none

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <=20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       include
*                       implicit none

*   Changes:
*       210995 jngh programmed

*   Calls:
*       get_real_var
*       get_real_var_optional
*       get_integer_var
*       pop_routine
*       push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*      none

*   Global variables
      include   'const.inc'            ! Constant definitions
      include   'convert.inc'          ! conversion constants
      include   'correl.inc'               ! correl common block

*   Internal variables
      integer    numvals               ! number of values returned
      integer    i
      integer    j
      real       value                     
      

*   Constant values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'correl_get_other_variables_init')
   

*   Initial data values

* --------------------- Executable code section ----------------------
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

*   Short description:
*      Get the values of variables from other modules

*   Assumptions:
*      None

*   Notes:
*       none

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <=20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       include
*                       implicit none

*   Changes:
*       210995 jngh programmed

*   Calls:
*       get_real_var
*       get_real_var_optional
*       get_integer_var
*       pop_routine
*       push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
*      none

*   Global variables
      include   'const.inc'            ! Constant definitions
      include   'convert.inc'          ! conversion constants
      include   'correl.inc'               ! correl common block

*   Internal variables
      integer    numvals               ! number of values returned
      integer    i                     
      

*   Constant values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'correl_get_other_variables')
   

*   Initial data values

* --------------------- Executable code section ----------------------
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

*   Short description:
*            return the value of one of our variables to       caller

*   Assumptions:
*      None

*   Notes:
*       none

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <=20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       include
*                       implicit none

*   Changes:
*       210995 jngh programmed

*   calls:
*       respond2get_real_var_optional
*       pop_routine
*       push_routine

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      character  Variable_name*(*)     ! (INPUT) Variable name to search for

*   Global variables
      include   'correl.inc'               ! correl common block

*   Internal variables
*      none

*   Constant values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'correl_send_my_variable')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------
      call push_routine (myname)

         call Message_unused ()

      call pop_routine (myname)
      return
      end
*================================================================
      subroutine correl_process ()
*================================================================

*   short description:
*      perform actions for current day

*   assumptions:
*      none

*   notes:
*       none

*   procedure attributes:
*      version:         any hardware/fortran77
*      extensions:      long names <=20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                             implicit   none

*   changes:
*       210995 jngh programmed

*   calls:
*       pop_routine
*       push_routine

* ----------------------- declaration section ------------------------

      implicit   none

*   Subroutine arguments
*      none

*   global variables
*      None

*   internal variables
*      None

*   constant values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'correl_process')

*   initial data values
*      None

* --------------------- executable code section ----------------------

      call push_routine (myname)

      call pop_routine (myname)
      return
      end         

*================================================================
      subroutine correl_calculate (variable_name)
*================================================================

*   short description:
*      perform actions for current day

*   assumptions:
*      none

*   notes:
*       none

*   procedure attributes:
*      version:         any hardware/fortran77
*      extensions:      long names <=20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*                       include
*                             implicit   none

*   changes:
*       210995 jngh programmed

*   calls:
*       pop_routine
*       push_routine

* ----------------------- declaration section ------------------------

      implicit   none

*   Subroutine arguments
      character  Variable_name*(*)     ! (INPUT) Variable name to search for

*   global variables
      include   'correl.inc'               ! correl common block
      include   'const.inc'
      
      integer    lastnb

*   internal variables
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
      
*   constant values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'correl_calculate')

*   initial data values
*      None

* --------------------- executable code section ----------------------

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
      write (1000, *) string(1:lastnb(string))
      do i=1,g_numvars
         write (string, '(1x, a7)') g_name_found(i)
         call write_string (lu_scr_sum, string)
         write (1000, *) string(1:lastnb(string))
         do k = 1,p_offset*2+1
            write (string, '(2x, a3, sp, i2, ss, 700f7.2)') 
     :            'day',k-(p_offset+1), (cor(i,j,k), j=1,g_numvars)
            call write_string (lu_scr_sum, string)
            write (1000, *) string(1:lastnb(string))
         end do
      end do
      
      close (1000)
         
      call pop_routine (myname)
      return
      end


