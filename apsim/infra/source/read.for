C     Last change:  P     8 Nov 2000   12:19 pm
* ====================================================================
       subroutine read_real_var
     .   (section_name, variable_name,
     .    units, variable, numvals,
     .    lower_limit, upper_limit)
* ====================================================================
      implicit none
      dll_export read_real_var
      include 'const.inc'              ! Constant definitions
 
*+ Sub-Program Arguments
      character section_name*(*)       ! (INPUT) section name to search for
      character variable_name*(*)      ! (INPUT) Variable name to search for
      character units*(*)              ! (INPUT) Units required by caller
      real variable                    ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned
      real lower_limit                 ! (INPUT) Lower limit for bounds check
      real upper_limit                 ! (INPUT) Upper limit for bounds check
 
*+ Purpose
*     High level routine to read in a real value from a parameter file.
*     This routine also checks the bounds of the returned variable.
*     Fatal_error will be called if any err is encountered.
 
*+  Mission Statement
*     Read %4 from file (Lower Bound = %6, Upper Bound = %7)
 
*+ Changes
*     DPH 18/10/94
*     271094 jngh added split_off_units to remove units
*     23/9/96 dph added if statement to check for blank
*     13/7/99 dph reworked to use new Read_parameter routine
 
*+ Calls
      dll_import read_parameter
      dll_import string_to_real_var
      dll_import bound_check_real_var
      character read_parameter*(function_string_len)
                                       ! function
 
*+ Constant Values
 
*+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
 
*- Implementation Section ----------------------------------
 
      return_string = read_parameter (variable_name, section_name)
 
      if (return_string .ne. blank) then
         call string_to_real_var
     .      (return_string, variable, numvals)
         call bound_check_real_var
     .      (variable, lower_limit, upper_limit, variable_name)
      endif
 
      return
      end
 
 
 


* ====================================================================
       subroutine read_integer_var
     .    (section_name, variable_name,
     .     units, variable, numvals,
     .     lower_limit, upper_limit)
* ====================================================================
      implicit none
      dll_export read_integer_var
      include 'const.inc'              ! Constant definitions
 
*+ Sub-Program Arguments
      character section_name*(*)       ! (INPUT) section name to search for
      character variable_name*(*)      ! (INPUT) Variable name to search for
      character units*(*)              ! (INPUT) Units required by caller
      integer variable                    ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned
      integer lower_limit                 ! (INPUT) Lower limit for bounds check
      integer upper_limit                 ! (INPUT) Upper limit for bounds check
 
*+ Purpose
*     High level routine to read in a integer value from a parameter file.
*     This routine also checks the bounds of the returned variable.
*     Fatal_error will be called if any err is encountered.
 
*+  Mission Statement
*     Read %4 from file (Lower Bound = %6, Upper Bound = %7)
 
*+ Changes
*     DPH 18/10/94
*     271094 jngh added split_off_units to remove units
*     23/9/96 dph added if statement to check for blank
*     13/7/99 dph reworked to use new Read_parameter routine
 
*+ Calls
      dll_import read_parameter
      dll_import string_to_integer_var
      dll_import bound_check_integer_var
      character read_parameter*(function_string_len)
                                       ! function
 
*+ Constant Values
 
*+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
 
*- Implementation Section ----------------------------------
 
      return_string = read_parameter (variable_name, section_name)
 
      if (return_string .ne. blank) then
         call string_to_integer_var
     .      (return_string, variable, numvals)
         call bound_check_integer_var
     .      (variable, lower_limit, upper_limit, variable_name)
      endif
 
      return
      end
 
 
 


* ====================================================================
       subroutine read_double_var
     .    (section_name, variable_name,
     .     units, variable, numvals,
     .     lower_limit, upper_limit)
* ====================================================================
      implicit none
      dll_export read_double_var
      include 'const.inc'              ! Constant definitions
 
*+ Sub-Program Arguments
      character section_name*(*)       ! (INPUT) section name to search for
      character variable_name*(*)      ! (INPUT) Variable name to search for
      character units*(*)              ! (INPUT) Units required by caller
      double precision variable        ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned
      double precision lower_limit     ! (INPUT) Lower limit for bounds check
      double precision upper_limit     ! (INPUT) Upper limit for bounds check
 
*+ Purpose
*     High level routine to read in a double value from a parameter file.
*     This routine also checks the bounds of the returned variable.
*     Fatal_error will be called if any err is encountered.
 
*+  Mission Statement
*     Read %4 from file (Lower Bound = %6, Upper Bound = %7)
 
*+ Changes
*     DPH 18/10/94
*     271094 jngh added get_units to remove units
*     23/9/96 dph added if statement to check for blank
*     13/7/99 dph reworked to use new Read_parameter routine 
*+ Calls
      dll_import read_parameter
      dll_import string_to_double_var
      dll_import bound_check_double_var
      character read_parameter*(function_string_len)
                                       ! function
 
*+ Constant Values
 
*+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
 
*- Implementation Section ----------------------------------
 
      return_string = read_parameter(variable_name, section_name)
 
      if (return_string .ne. blank) then
         call string_to_double_var
     .      (return_string, variable, numvals)
 
         call bound_check_double_var
     .      (variable, lower_limit, upper_limit, variable_name)
      endif
 
      return
      end
 
 
 


* ====================================================================
       subroutine read_logical_var
     .    (section_name, variable_name,
     .     units, variable, numvals)
* ====================================================================
      implicit none
      dll_export read_logical_var
      include 'const.inc'              ! Constant definitions
 
*+ Sub-Program Arguments
      character section_name*(*)       ! (INPUT) section name to search for
      character variable_name*(*)      ! (INPUT) Variable name to search for
      character units*(*)              ! (INPUT) Units required by caller
      logical variable                    ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned
 
*+ Purpose
*     High level routine to read in a logical value from a parameter file.
*     This routine also checks the bounds of the returned variable.
*     Fatal_error will be called if any err is encountered.
 
*+  Mission Statement
*      Read %4 from file
 
*+ Changes
*     DPH 18/10/94
*     271094 jngh added split_off_units to remove units
*     23/9/96 dph added if statement to check for blank
*     13/7/99 dph reworked to use new Read_parameter routine
 
*+ Calls
      dll_import read_parameter
      dll_import string_to_logical_var
      character read_parameter*(function_string_len)
                                       ! function
 
*+ Constant Values
 
*+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
 
*- Implementation Section ----------------------------------
 
      return_string = read_parameter (variable_name, section_name)
 
      if (return_string .ne. blank) then
         call string_to_logical_var
     .      (return_string, variable, numvals)
      endif
 
      return
      end
 
 
 


* ====================================================================
       subroutine read_char_var
     .    (section_name, variable_name,
     .     units, variable, numvals)
* ====================================================================
      implicit none
      dll_export read_char_var
      include 'const.inc'              ! Constant definitions
 
*+ Sub-Program Arguments
      character section_name*(*)       ! (INPUT) section name to search for
      character variable_name*(*)      ! (INPUT) Variable name to search for
      character units*(*)              ! (INPUT) Units required by caller
      character variable*(*)           ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned
 
*+ Purpose
*     High level routine to read in a character value from a parameter file.
*     This routine also checks the bounds of the returned variable.
*     Fatal_error will be called if any err is encountered.
 
*+  Mission Statement
*      Read %4 from file
 
*+ Changes
*     DPH 18/10/94
*     23/9/96 dph added if statement to check for blank
*     13/7/99 dph reworked to use new Read_parameter routine 
*+ Calls
      dll_import read_parameter
      character read_parameter*(function_string_len)
                                       ! function
 
*+ Constant Values
 
*+ Local Variables
 
*- Implementation Section ----------------------------------
 
      variable = read_parameter(variable_name, section_name)
      if (variable .eq. blank) then
         numvals = 0
 
      else
         numvals = 1
      endif
 
      return
      end
 
 
 


* ====================================================================
       subroutine read_real_var_optional
     .   (section_name, variable_name,
     .    units, variable, numvals,
     .    lower_limit, upper_limit)
* ====================================================================
      implicit none
      dll_export read_real_var_optional
      include 'const.inc'              ! Constant definitions
 
*+ Sub-Program Arguments
      character section_name*(*)       ! (INPUT) section name to search for
      character variable_name*(*)      ! (INPUT) Variable name to search for
      character units*(*)              ! (INPUT) Units required by caller
      real variable                    ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned
      real lower_limit                 ! (INPUT) Lower limit for bounds check
      real upper_limit                 ! (INPUT) Upper limit for bounds check
 
*+ Purpose
*     High level routine to read in a real value from a parameter file.
*     This routine also checks the bounds of the returned variable.
*     Fatal_error will be called if any err is encountered.
 
*+  Mission Statement
*     Read %4 from file (Lower Bound = %6, Upper Bound = %7) |italic{(OPTIONAL)}
 
*+ Changes
*     DPH 18/10/94
*     13/7/99 dph reworked to use new Read_parameter_optional routine 
*+ Calls
      dll_import read_parameter_optional
      dll_import string_to_real_var
      dll_import bound_check_real_var
      logical read_parameter_optional  ! function
 
*+ Constant Values
 
*+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
      character return_units*(100)
                                       ! Units returned from read_file_string
 
*- Implementation Section ----------------------------------

      if (read_parameter_optional (variable_name, 
     .                             section_name,
     .                             return_string,
     .                             return_units)) then
         call string_to_real_var
     .      (return_string, variable, numvals)
         call bound_check_real_var
     .     (variable, lower_limit, upper_limit, variable_name)
      else
         variable = 0.0
         numvals = 0
      endif
 
      return
      end

* ====================================================================
       subroutine read_integer_var_optional
     .    (section_name, variable_name,
     .     units, variable, numvals,
     .     lower_limit, upper_limit)
* ====================================================================
      implicit none
      dll_export read_integer_var_optional
      include 'const.inc'              ! Constant definitions
 
*+ Sub-Program Arguments
      character section_name*(*)       ! (INPUT) section name to search for
      character variable_name*(*)      ! (INPUT) Variable name to search for
      character units*(*)              ! (INPUT) Units required by caller
      integer variable                    ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned
      integer lower_limit                 ! (INPUT) Lower limit for bounds check
      integer upper_limit                 ! (INPUT) Upper limit for bounds check
 
*+ Purpose
*     High level routine to read in a integer value from a parameter file.
*     This routine also checks the bounds of the returned variable.
*     Fatal_error will be called if any err is encountered.
 
*+  Mission Statement
*     Read %4 from file (Lower Bound = %6, Upper Bound = %7) |italic{(OPTIONAL)}
 
*+ Changes
*     DPH 18/10/94
*     271094 jngh added split_off_units to remove units
*     13/7/99 dph reworked to use new Read_parameter_optional routine 
 
*+ Calls
      dll_import read_parameter_optional
      dll_import string_to_integer_var
      dll_import bound_check_integer_var
      logical read_parameter_optional  ! function
 
*+ Constant Values
 
*+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
      character return_units*(100)
                                       ! Units returned from read_file_string
 
*- Implementation Section ----------------------------------
 
      if (read_parameter_optional (variable_name, 
     .                             section_name,
     .                             return_string,
     .                             return_units)) then
         call string_to_integer_var
     .      (return_string, variable, numvals)
         call bound_check_integer_var
     .      (variable, lower_limit, upper_limit, variable_name)
 
      else
         variable = 0
         numvals = 0
      endif
 
      return
      end

* ====================================================================
       subroutine read_double_var_optional
     .    (section_name, variable_name,
     .     units, variable, numvals,
     .     lower_limit, upper_limit)
* ====================================================================
      implicit none
      dll_export read_double_var_optional
      include 'const.inc'              ! Constant definitions
 
*+ Sub-Program Arguments
      character section_name*(*)       ! (INPUT) section name to search for
      character variable_name*(*)      ! (INPUT) Variable name to search for
      character units*(*)              ! (INPUT) Units required by caller
      double precision variable        ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned
      double precision lower_limit     ! (INPUT) Lower limit for bounds check
      double precision upper_limit     ! (INPUT) Upper limit for bounds check
 
*+ Purpose
*     High level routine to read in a double value from a parameter file.
*     This routine also checks the bounds of the returned variable.
*     Fatal_error will be called if any err is encountered.
 
*+  Mission Statement
*     Read %4 from file (Lower Bound = %6, Upper Bound = %7) |italic{(OPTIONAL)}
 
*+ Changes
*     DPH 18/10/94
*     271094 jngh added split_off_units to remove units
*     13/7/99 dph reworked to use new Read_parameter_optional routine 
 
*+ Calls
      dll_import read_parameter_optional
      dll_import split_off_units
      dll_import string_to_double_var
      dll_import bound_check_double_var
      logical read_parameter_optional  ! function
 
*+ Constant Values
 
*+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
      character return_units*(100)
                                       ! Units returned from read_file_string
 
*- Implementation Section ----------------------------------
 
      if (read_parameter_optional (variable_name, 
     .                             section_name,
     .                             return_string,
     .                             return_units)) then
         call string_to_double_var
     .      (return_string, variable, numvals)
 
         call bound_check_double_var
     .      (variable, lower_limit, upper_limit, variable_name)
      else
         variable = 0.0d0
         numvals = 0
      endif
 
      return
      end

* ====================================================================
       subroutine read_logical_var_optional
     .    (section_name, variable_name,
     .     units, variable, numvals)
* ====================================================================
      implicit none
      dll_export read_logical_var_optional
      include 'const.inc'              ! Constant definitions
 
*+ Sub-Program Arguments
      character section_name*(*)       ! (INPUT) section name to search for
      character variable_name*(*)      ! (INPUT) Variable name to search for
      character units*(*)              ! (INPUT) Units required by caller
      logical variable                    ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned
 
*+ Purpose
*     High level routine to read in a logical value from a parameter file.
*     This routine also checks the bounds of the returned variable.
*     Fatal_error will be called if any err is encountered.
 
*+  Mission Statement
*      Read %4 from file |italic{(OPTIONAL)}
 
*+ Changes
*     DPH 18/10/94
*     271094 jngh added split_off_units to remove units
*     13/7/99 dph reworked to use new Read_parameter_optional routine 
 
*+ Calls
      dll_import read_parameter_optional
      dll_import string_to_logical_var
      logical read_parameter_optional  ! function
 
*+ Constant Values
 
*+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
      character return_units*(100)
                                       ! Units returned from read_file_string
 
*- Implementation Section ----------------------------------
 
      if (read_parameter_optional (variable_name, 
     .                             section_name,
     .                             return_string,
     .                             return_units)) then
         call string_to_logical_var
     .      (return_string, variable, numvals)
 
      else
         variable = .false.
         numvals = 0
      endif
 
      return
      end
 
 
 


* ====================================================================
       subroutine read_char_var_optional
     .    (section_name, variable_name,
     .     units, variable, numvals)
* ====================================================================
      implicit none
      dll_export read_char_var_optional
      include 'const.inc'              ! Constant definitions
 
*+ Sub-Program Arguments
      character section_name*(*)       ! (INPUT) section name to search for
      character variable_name*(*)      ! (INPUT) Variable name to search for
      character units*(*)              ! (INPUT) Units required by caller
      character variable*(*)           ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned
 
*+ Purpose
*     High level routine to read in a character value from a parameter file.
*     This routine also checks the bounds of the returned variable.
*     Fatal_error will be called if any err is encountered.
 
*+  Mission Statement
*     Read %4 from file |italic{(OPTIONAL)}
 
*+ Changes
*     DPH 18/10/94
*     13/7/99 dph reworked to use new Read_parameter_optional routine 
 
*+ Calls
      dll_import read_parameter_optional
      logical read_parameter_optional  ! function
 
*+ Constant Values
 
*+ Local Variables
      character return_units*(100)
                                       ! Units returned from read_file_string
 
*- Implementation Section ----------------------------------
 
      if (read_parameter_optional (variable_name, 
     .                             section_name,
     .                             variable,
     .                             return_units)) then
         numvals = 1
 
      else
         numvals = 0
      endif
 
      return
      end

* ====================================================================
       subroutine read_real_array
     .   (section_name, variable_name, size_of,
     .    units, variable, numvals,
     .    lower_limit, upper_limit)
* ====================================================================
      implicit none
      dll_export read_real_array
      include 'const.inc'              ! Constant definitions
 
*+ Sub-Program Arguments
      character section_name*(*)       ! (INPUT) section name to search for
      character variable_name*(*)      ! (INPUT) Variable name to search for
      integer size_of                  ! (INPUT) size_of of array
      character units*(*)              ! (INPUT) Units required by caller
      real variable(*)                 ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned
      real lower_limit                 ! (INPUT) Lower limit for bounds check
      real upper_limit                 ! (INPUT) Upper limit for bounds check
 
*+ Purpose
*     High level routine to read in a real value from a parameter file.
*     This routine also checks the bounds of the returned variable.
*     Fatal_error will be called if any err is encountered.
 
*+  Mission Statement
*     Read %5 from file (Lower Bound = %6, Upper Bound = %7)
 
*+ Changes
*     DPH 18/10/94
*     271094 jngh added split_off_units to remove units
*     23/9/96 dph added if statement to check for blank
*     13/7/99 dph reworked to use new Read_parameter routine 
 
*+ Calls
      dll_import read_parameter
      dll_import string_to_real_array
      dll_import bound_check_real_array
      character read_parameter*(function_string_len)
                                       ! function
 
*+ Constant Values
 
*+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
 
*- Implementation Section ----------------------------------

      return_string = Read_parameter (variable_name, section_name)
      if (return_string .ne. Blank) then
         call string_to_real_array
     .      (return_string, variable, size_of, numvals)
         call bound_check_real_array
     .    (variable, lower_limit, upper_limit, variable_name, numvals)
      endif
 
      return
      end


* ====================================================================
       subroutine read_integer_array
     .    (section_name, variable_name, size_of,
     .     units, variable, numvals,
     .     lower_limit, upper_limit)
* ====================================================================
      implicit none
      dll_export read_integer_array
      include 'const.inc'              ! Constant definitions
 
*+ Sub-Program Arguments
      character section_name*(*)       ! (INPUT) section name to search for
      character variable_name*(*)      ! (INPUT) Variable name to search for
      integer size_of                  ! (INPUT) size_of of array
      character units*(*)              ! (INPUT) Units required by caller
      integer variable(*)                 ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned
      integer lower_limit                 ! (INPUT) Lower limit for bounds check
      integer upper_limit                 ! (INPUT) Upper limit for bounds check
 
*+ Purpose
*     High level routine to read in a integer value from a parameter file.
*     This routine also checks the bounds of the returned variable.
*     Fatal_error will be called if any err is encountered.
 
*+  Mission Statement
*     Read %5 from file (Lower Bound = %6, Upper Bound = %7)
 
*+ Changes
*     DPH 18/10/94
*     271094 jngh added split_off_units to remove units
*     23/9/96 dph added if statement to check for blank
*     13/7/99 dph reworked to use new Read_parameter routine 
 
*+ Calls
      dll_import read_parameter
      dll_import string_to_integer_array
      dll_import bound_check_integer_array
      character read_parameter*(function_string_len)
                                       ! function
 
*+ Constant Values
 
*+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
 
*- Implementation Section ----------------------------------
 
      return_string = Read_parameter (variable_name, section_name)
      if (return_string .ne. Blank) then
         call string_to_integer_array
     .      (return_string, variable, size_of, numvals)
         call bound_check_integer_array
     .    (variable, lower_limit, upper_limit, variable_name, numvals)
      endif
 
      return
      end

* ====================================================================
       subroutine read_double_array
     .    (section_name, variable_name, size_of,
     .     units, variable, numvals,
     .     lower_limit, upper_limit)
* ====================================================================
      implicit none
      dll_export read_double_array
      include 'const.inc'              ! Constant definitions
 
*+ Sub-Program Arguments
      character section_name*(*)       ! (INPUT) section name to search for
      character variable_name*(*)      ! (INPUT) Variable name to search for
      integer size_of                  ! (INPUT) size_of of array
      character units*(*)              ! (INPUT) Units required by caller
      double precision variable(*)     ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned
      double precision lower_limit     ! (INPUT) Lower limit for bounds check
      double precision upper_limit     ! (INPUT) Upper limit for bounds check
 
*+ Purpose
*     High level routine to read in a double value from a parameter file.
*     This routine also checks the bounds of the returned variable.
*     Fatal_error will be called if any err is encountered.
 
*+  Mission Statement
*     Read %5 from file (Lower Bound = %6, Upper Bound = %7)
 
*+ Changes
*     DPH 18/10/94
*     271094 jngh added split_off_units to remove units
*     23/9/96 dph added if statement to check for blank
*     13/7/99 dph reworked to use new Read_parameter routine 
 
*+ Calls
      dll_import read_parameter
      dll_import string_to_double_array
      dll_import bound_check_double_array
      character read_parameter*(function_string_len)
                                       ! function
 
*+ Constant Values
 
*+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
 
*- Implementation Section ----------------------------------
 
      return_string = Read_parameter (variable_name, section_name)
      if (return_string .ne. Blank) then
         call string_to_double_array
     .      (return_string, variable, size_of, numvals)
         call bound_check_double_array
     .     (variable, lower_limit, upper_limit, variable_name, numvals)
      endif
 
      return
      end

* ====================================================================
       subroutine read_logical_array
     .    (section_name, variable_name, size_of,
     .     units, variable, numvals)
* ====================================================================
      implicit none
      dll_export read_logical_array
      include 'const.inc'              ! Constant definitions
 
*+ Sub-Program Arguments
      character section_name*(*)       ! (INPUT) section name to search for
      character variable_name*(*)      ! (INPUT) Variable name to search for
      integer size_of                  ! (INPUT) size_of of array
      character units*(*)              ! (INPUT) Units required by caller
      logical variable(*)                 ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned
 
*+ Purpose
*     High level routine to read in a logical value from a parameter file.
*     This routine also checks the bounds of the returned variable.
*     Fatal_error will be called if any err is encountered.
 
*+  Mission Statement
*     Read %5 from file
 
*+ Changes
*     DPH 18/10/94
*     271094 jngh added split_off_units to remove units
*     23/9/96 dph added if statement to check for blank
*     13/7/99 dph reworked to use new Read_parameter routine 
 
*+ Calls
      dll_import read_parameter
      dll_import string_to_logical_array
      character read_parameter*(function_string_len)
                                       ! function
 
*+ Constant Values
 
*+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
 
*- Implementation Section ----------------------------------
 
      return_string = Read_parameter (variable_name, section_name)
      if (return_string .ne. Blank) then
         call string_to_logical_array
     .      (return_string, variable, size_of, numvals)
      endif
 
      return
      end

* ====================================================================
       subroutine read_char_array
     .    (section_name, variable_name, size_of,
     .     units, variable, numvals)
* ====================================================================
      implicit none
      dll_export read_char_array
      include 'const.inc'              ! Constant definitions
 
*+ Sub-Program Arguments
      character section_name*(*)       ! (INPUT) section name to search for
      character variable_name*(*)      ! (INPUT) Variable name to search for
      integer size_of                  ! (INPUT) size_of of array
      character units*(*)              ! (INPUT) Units required by caller
      character variable(*)*(*)        ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned
 
*+ Purpose
*     High level routine to read in a character value from a parameter file.
*     This routine also checks the bounds of the returned variable.
*     Fatal_error will be called if any err is encountered.
 
*+  Mission Statement
*     Read %5 from file
 
*+ Changes
*     DPH 18/10/94
*     DPH 10/4/96   Added call to split_off_units
*     23/9/96 dph added if statement to check for blank
*     13/7/99 dph reworked to use new Read_parameter routine 
 
*+ Calls
      dll_import read_parameter
      dll_import string_to_char_array
      character read_parameter*(function_string_len)
                                       ! function
 
*+ Constant Values
 
*+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
 
*- Implementation Section ----------------------------------
 
      return_string = Read_parameter (variable_name, section_name)
      if (return_string .ne. Blank) then
         call string_to_char_array
     .      (return_string, variable, size_of, numvals)
      endif
 
      return
      end

* ====================================================================
       subroutine read_real_array_optional
     .   (section_name, variable_name, size_of,
     .    units, variable, numvals,
     .    lower_limit, upper_limit)
* ====================================================================
      implicit none
      dll_export read_real_array_optional
      include 'const.inc'              ! Constant definitions
 
*+ Sub-Program Arguments
      character section_name*(*)       ! (INPUT) section name to search for
      character variable_name*(*)      ! (INPUT) Variable name to search for
      integer size_of                  ! (INPUT) size_of of array
      character units*(*)              ! (INPUT) Units required by caller
      real variable(*)                 ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned
      real lower_limit                 ! (INPUT) Lower limit for bounds check
      real upper_limit                 ! (INPUT) Upper limit for bounds check
 
*+ Purpose
*     High level routine to read in a real value from a parameter file.
*     This routine also checks the bounds of the returned variable.
*     Fatal_error will be called if any err is encountered.
 
*+  Mission Statement
*     Read %5 from file (Lower Bound = %6, Upper Bound = %7) |italic{(OPTIONAL)}
 
*+ Changes
*     DPH 18/10/94
*     271094 jngh added split_off_units to remove units
*     13/7/99 dph reworked to use new Read_parameter routine 
 
*+ Calls
      dll_import read_parameter_optional
      dll_import fill_real_array
      dll_import string_to_real_array
      dll_import bound_check_real_array
      logical read_parameter_optional  ! function
 
*+ Constant Values
 
*+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
      character return_units*(100)
                                       ! Units returned from read_file_string
 
*- Implementation Section ----------------------------------
 
      if (read_parameter_optional (variable_name, 
     .                             section_name, 
     .                             return_string,
     .                             return_units)) then
         call string_to_real_array
     .     (return_string, variable, size_of, numvals)
         call bound_check_real_array
     .      (variable, lower_limit, upper_limit, variable_name, numvals)
 
      else
         call fill_real_array(variable, 0.0, size_of)
         numvals = 0
      endif
 
      return
      end

* ====================================================================
       subroutine read_integer_array_optional
     .    (section_name, variable_name, size_of,
     .     units, variable, numvals,
     .     lower_limit, upper_limit)
* ====================================================================
      implicit none
      dll_export read_integer_array_optional
      include 'const.inc'              ! Constant definitions
 
*+ Sub-Program Arguments
      character section_name*(*)       ! (INPUT) section name to search for
      character variable_name*(*)      ! (INPUT) Variable name to search for
      integer size_of                  ! (INPUT) size_of of array
      character units*(*)              ! (INPUT) Units required by caller
      integer variable(*)                 ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned
      integer lower_limit                 ! (INPUT) Lower limit for bounds check
      integer upper_limit                 ! (INPUT) Upper limit for bounds check
 
*+ Purpose
*     High level routine to read in a integer value from a parameter file.
*     This routine also checks the bounds of the returned variable.
*     Fatal_error will be called if any err is encountered.
 
*+  Mission Statement
*     Read %5 from file (Lower Bound = %6, Upper Bound = %7) |italic{(OPTIONAL)}
 
*+ Changes
*     DPH 18/10/94
*     271094 jngh added split_off_units to remove units
*     13/7/99 dph reworked to use new Read_parameter routine 
 
*+ Calls
      dll_import read_parameter_optional
      dll_import fill_integer_array
      dll_import string_to_integer_array
      dll_import bound_check_integer_array
      logical read_parameter_optional
                                       ! function
 
*+ Constant Values
 
*+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
      character return_units*(100)
                                       ! Units returned from read_file_string
 
*- Implementation Section ----------------------------------
 
      if (read_parameter_optional (variable_name, 
     .                             section_name, 
     .                             return_string,
     .                             return_units)) then
         call string_to_integer_array
     .      (return_string, variable, size_of, numvals)
         call bound_check_integer_array
     .      (variable, lower_limit, upper_limit, variable_name, numvals)
 
      else
         call fill_integer_array(variable, 0, size_of)
         numvals = 0
      endif
 
      return
      end

* ====================================================================
       subroutine read_double_array_optional
     .    (section_name, variable_name, size_of,
     .     units, variable, numvals,
     .     lower_limit, upper_limit)
* ====================================================================
      implicit none
      dll_export read_double_array_optional
      include 'const.inc'              ! Constant definitions
 
*+ Sub-Program Arguments
      character section_name*(*)       ! (INPUT) section name to search for
      character variable_name*(*)      ! (INPUT) Variable name to search for
      integer size_of                  ! (INPUT) size_of of array
      character units*(*)              ! (INPUT) Units required by caller
      double precision variable(*)     ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned
      double precision lower_limit     ! (INPUT) Lower limit for bounds check
      double precision upper_limit     ! (INPUT) Upper limit for bounds check
 
*+ Purpose
*     High level routine to read in a double value from a parameter file.
*     This routine also checks the bounds of the returned variable.
*     Fatal_error will be called if any err is encountered.
 
*+  Mission Statement
*     Read %5 from file (Lower Bound = %6, Upper Bound = %7) |italic{(OPTIONAL)}
 
*+ Changes
*     DPH 18/10/94
*     271094 jngh added split_off_units to remove units
*     13/7/99 dph reworked to use new Read_parameter routine 
 
*+ Calls
      dll_import read_parameter_optional
      dll_import fill_double_array
      dll_import string_to_double_array
      dll_import bound_check_double_array
      logical read_parameter_optional
                                       ! function
 
*+ Constant Values
 
*+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
      character return_units*(100)
                                       ! Units returned from read_file_string
 
*- Implementation Section ----------------------------------
 
      if (read_parameter_optional (variable_name, 
     .                             section_name, 
     .                             return_string,
     .                             return_units)) then
         call string_to_double_array
     .      (return_string, variable, size_of, numvals)
         call bound_check_double_array
     .      (variable, lower_limit, upper_limit, variable_name, numvals)
 
      else
         call fill_double_array(variable, 0.0d0, size_of)
         numvals = 0
      endif
 
      return
      end

* ====================================================================
       subroutine read_logical_array_optional
     .    (section_name, variable_name, size_of,
     .     units, variable, numvals)
* ====================================================================
      implicit none
      dll_export read_logical_array_optional
      include 'const.inc'              ! Constant definitions
 
*+ Sub-Program Arguments
      character section_name*(*)       ! (INPUT) section name to search for
      character variable_name*(*)      ! (INPUT) Variable name to search for
      integer size_of                  ! (INPUT) size_of of array
      character units*(*)              ! (INPUT) Units required by caller
      logical variable(*)                 ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned
 
*+ Purpose
*     High level routine to read in a logical value from a parameter file.
*     This routine also checks the bounds of the returned variable.
*     Fatal_error will be called if any err is encountered.
 
*+  Mission Statement
*     Read %5 from file |italic{(OPTIONAL)}
 
*+ Changes
*     DPH 18/10/94
*     271094 jngh added split_off_units to remove units
*     13/7/99 dph reworked to use new Read_parameter routine 
 
*+ Calls
      dll_import read_parameter_optional
      dll_import fill_logical_array
      dll_import string_to_logical_array
      logical read_parameter_optional
                                       ! function
 
*+ Constant Values
 
*+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
      character return_units*(100)
                                       ! Units returned from read_file_string
 
*- Implementation Section ----------------------------------
 
      if (read_parameter_optional (variable_name, 
     .                             section_name, 
     .                             return_string,
     .                             return_units)) then
         call string_to_logical_array
     .      (return_string, variable, size_of, numvals)
 
      else
         call fill_logical_array(variable, .false., size_of)
         numvals = 0
      endif
 
      return
      end

* ====================================================================
       subroutine read_char_array_optional
     .    (section_name, variable_name, size_of,
     .     units, variable, numvals)
* ====================================================================
      implicit none
      dll_export read_char_array_optional
      include 'const.inc'              ! Constant definitions
 
*+ Sub-Program Arguments
      character section_name*(*)       ! (INPUT) section name to search for
      character variable_name*(*)      ! (INPUT) Variable name to search for
      integer size_of                  ! (INPUT) size_of of array
      character units*(*)              ! (INPUT) Units required by caller
      character variable(*)*(*)        ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned
 
*+ Purpose
*     High level routine to read in a character value from a parameter file.
*     This routine also checks the bounds of the returned variable.
*     Fatal_error will be called if any err is encountered.
 
*+  Mission Statement
*     Read %5 from file |italic{(OPTIONAL)}
 
*+ Changes
*     DPH 18/10/94
*     DPH 10/4/96   Added call to split_off_units
*     13/7/99 dph reworked to use new Read_parameter routine 
 
*+ Calls
      dll_import read_parameter_optional
      dll_import fill_char_array
      dll_import string_to_char_array
      logical read_parameter_optional
                                       ! function
 
*+ Constant Values
 
*+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
      character return_units*(100)
                                       ! Units returned from read_file_string
 
*- Implementation Section ----------------------------------
 
      if (read_parameter_optional (variable_name, 
     .                             section_name, 
     .                             return_string,
     .                             return_units)) then
         call string_to_char_array
     .      (return_string, variable, size_of, numvals)
 
      else
         call fill_char_array(variable, blank, size_of)
         numvals = 0
      endif
 
      return
      end
 
* ====================================================================
       logical function Read_parameter_optional 
     .  (Parameter_name, Section_name, Parameter_value, Parameter_units)
* ====================================================================
      implicit none
      dll_export read_parameter_optional
      include 'const.inc'
      include 'apsimengine.pub'
      include 'componentinterface.inc'
      include 'string.pub'
 
*+ Sub-Program Arguments
      character Parameter_name*(*)     ! (INPUT) name of parameter to retrieve
      character Section_name*(*)       ! (INPUT) name of section to retrieve from
      character Parameter_value*(*)    ! (OUTPUT) value of parameter
      character Parameter_units*(*)    ! (OUTPUT) units of parameter

*+ Purpose
*     Retrieve a parameter from system with the specified name. Returns
*     true and the value of the property if found or false otherwise.
 
*+  Mission Statement
*      
 
*+ Changes
*     dph 13/7/99 Created as part of the new infrastructure re-design.
*     dph 20/10/00 Updated to use new property, constant or generic types.
 
*+ Calls
      dll_import push_routine
      dll_import pop_routine
 
      character*(function_string_len) lower_case
      
*+ Constant Values
      character myname*(*)             ! Name of this routine
      parameter (myname='Read_parameter_optional')
 
*+ Local Variables
      integer*4 property               ! Property handle from infrastructure
 
*- Implementation Section ----------------------------------
 
      call push_routine(myname)

      ! Get property value.
      property = component_getproperty(ComponentData,
     .                                 Parameter_name,
     .                                 section_name)
      if (property .ne. 0) then
         call property_getvalue(property, parameter_value)
      end if

      ! If we got a property, convert value to lower_case.
      ! If we didn't get a property, then return .false.
      if (property .ne. 0) then
         Parameter_value = Lower_case(Parameter_value)
         Parameter_units = ' '
         Read_parameter_optional = .true.

      else
         ! couldn't find property.  Signal that all is not ok.
         Parameter_value = " "
         Read_parameter_optional = .false.
      endif

      call pop_routine(myname)
      
      return
      end

* ====================================================================
       character*(*) function Read_parameter 
     .     (Parameter_name, Section_name)
* ====================================================================
      implicit none
      dll_export read_parameter
      include 'const.inc'

*+ Sub-Program Arguments
      character Parameter_name*(*)     ! (INPUT) name of parameter to retrieve
      character Section_name*(*)       ! (INPUT) name of section to retrieve from
 
*+ Purpose
*     Retrieve a parameter from system with the specified name.  Calls
*     fatal_error if parameter not found.
 
*+  Mission Statement
*      
 
*+ Changes
*     dph 13/7/99 Created as part of the new infrastructure re-design.
 
*+ Calls
      dll_import push_routine
      dll_import fatal_error
      dll_import pop_routine
      dll_import read_parameter_optional
      logical read_parameter_optional
                                       ! function
 
*+ Constant Values
      character myname*(*)             ! Name of this routine
      parameter (myname='Read_parameter')
 
*+ Local Variables
      character msg*300                ! err message to display
      character Parameter_value*(500)  ! value of parameter
      character Parameter_units*(100)  ! units of parameter
 
*- Implementation Section ----------------------------------
 
      call push_routine(myname)
 
      if (Read_parameter_optional(Parameter_name, 
     .                            Section_name, 
     .                            Parameter_value,
     .                            Parameter_units)) then
         Read_parameter = Parameter_value
         
      else
         ! Cannot find parameter.  Issue fatal err message.
 
         msg =
     .      'Cannot find a parameter in any of the files/sections'//
     .      new_line//
     .      'specified in the control file.'//
     .      new_line//
     .      'Parameter name = ' // Parameter_name//
     .      new_line//
     .      'Section name = ' // Section_name
 
         call fatal_error(err_user, msg)
 
      endif
 
      Read_parameter = Parameter_value
      call pop_routine(myname)
      return
      end


* ====================================================================
      subroutine search_read_real_var (section_names
     :                                ,number_of_sections
     :                                ,variable_name
     :                                ,units
     :                                ,variable
     :                                ,numvals
     :                                ,lower
     :                                ,upper)
* ====================================================================
      implicit none
      include 'const.inc'
      include 'error.pub'          

*+  Sub-Program Arguments
      character section_names(*)*(*)   ! list of sections to search
      integer   number_of_sections     ! number of sections to search
      character variable_name*(*)      ! variable to search for
      character units*(*)              ! required units of variable
      real      variable               ! variable value to return
      integer   numvals                ! number of values returned
      real      lower                  ! lower bound of variable value
      real      upper                  ! upper bound of variable value

*+  Purpose
*      Read a real value from input files using a list of possible sections
*      for the search.

*+  Mission Statement
*   Search for %5 in files (Lower Bound = %7, Upper Bound = %8)

*+  Changes
*     25-11-1997 - neilh - Programmed and Specified

*+  Calls
      dll_import read_real_var_optional

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'search_read_real_var')

*+  Local Variables
      integer counter                  ! simple counter variable

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      do 100 counter = 1, number_of_sections
 
        call read_real_var_optional
     :                    (section_names(counter)
     :                    , variable_name
     :                    , units
     :                    , variable
     :                    , numvals
     :                    , lower
     :                    , upper)
 
         if (numvals.gt.0) then
            ! we have found what we want - exit.
            goto 200
         else
            ! try next section name
         endif
 
  100 continue
      ! If we reach this point of execution the search must have failed
      call Fatal_Error (ERR_User,
     :    'Parameter Search Failure. Unable to read variable '
     :    //variable_name)
 
  200 continue
 
      call pop_routine (myname)
      return
      end



* ====================================================================
      subroutine search_read_real_array (section_names
     :                                ,number_of_sections
     :                                ,array_name
     :                                ,array_size
     :                                ,units
     :                                ,array
     :                                ,numvals
     :                                ,lower
     :                                ,upper)
* ====================================================================
      implicit none
      include 'const.inc'
      include 'error.pub'                         

*+  Sub-Program Arguments
      character section_names(*)*(*)   ! list of sections to search
      integer   number_of_sections     ! number of sections to search
      character array_name*(*)         ! variable to search for
      integer   array_size             ! max. size of array
      character units*(*)              ! required units of variable
      real      array(*)               ! variable value to return
      integer   numvals                ! number of values returned
      real      lower                  ! lower bound of variable value
      real      upper                  ! upper bound of variable value

*+  Purpose
*      Read an array from input files using a list of possible sections
*      for the search.

*+  Mission Statement
*   Search for %5 in files (Lower Bound = %7, Upper Bound = %8)

*+  Changes
*     25-11-1997 - neilh - Programmed and Specified

*+  Calls
      dll_import read_real_array_optional

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'search_read_real_array')

*+  Local Variables
      integer counter                  ! simple counter variable

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      do 100 counter = 1, number_of_sections
 
        call read_real_array_optional
     :                    (section_names(counter)
     :                    , array_name
     :                    , array_size
     :                    , units
     :                    , array
     :                    , numvals
     :                    , lower
     :                    , upper)
 
         if (numvals.gt.0) then
            ! we have found what we want - exit.
            goto 200
         else
            ! try next section name
         endif
 
  100 continue
      ! If we reach this point of execution the search must have failed
      call Fatal_Error (ERR_User,
     :    'Parameter Search Failure. Unable to read variable '
     :    //array_name)
 
  200 continue
 
      call pop_routine (myname)
      return
      end



* ====================================================================
      subroutine search_read_char_array (section_names
     :                                ,number_of_sections
     :                                ,array_name
     :                                ,array_size
     :                                ,units
     :                                ,array
     :                                ,numvals)
* ====================================================================
      implicit none
      include 'const.inc'
      include 'error.pub'

*+  Sub-Program Arguments
      character section_names(*)*(*)   ! list of sections to search
      integer   number_of_sections     ! number of sections to search
      character array_name*(*)         ! variable to search for
      integer   array_size             ! max. size of array
      character units*(*)              ! required units of variable
      character array(*)*(*)           ! variable value to return
      integer   numvals                ! number of values returned

*+  Purpose
*      Read an array from input files using a list of possible sections
*      for the search.

*+  Mission Statement
*   Search for %5 in files

*+  Changes
*     25-11-1997 - neilh - Programmed and Specified

*+  Calls
      dll_import read_char_array_optional

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'search_read_char_array')

*+  Local Variables
      integer counter                  ! simple counter variable

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      do 100 counter = 1, number_of_sections
 
        call read_char_array_optional
     :                    (section_names(counter)
     :                    , array_name
     :                    , array_size
     :                    , units
     :                    , array
     :                    , numvals)
 
         if (numvals.gt.0) then
            ! we have found what we want - exit.
            goto 200
         else
            ! try next section name
         endif
 
  100 continue
      ! If we reach this point of execution the search must have failed
      call Fatal_Error (ERR_User,
     :    'Parameter Search Failure. Unable to read variable '
     :    //array_name)
 
  200 continue
 
      call pop_routine (myname)
      return
      end



* ====================================================================
      subroutine search_read_char_var (section_names
     :                                ,number_of_sections
     :                                ,variable_name
     :                                ,units
     :                                ,variable
     :                                ,numvals)
* ====================================================================
      implicit none
      include 'const.inc'
      include 'error.pub'

*+  Sub-Program Arguments
      character section_names(*)*(*)   ! list of sections to search
      integer   number_of_sections     ! number of sections to search
      character variable_name*(*)      ! variable to search for
      character units*(*)              ! required units of variable
      character variable*(*)           ! variable value to return
      integer   numvals                ! number of values returned

*+  Purpose
*      Read a char value from input files using a list of possible sections
*      for the search.

*+  Mission Statement
*   Search for %5 in files (Lower Bound = %7, Upper Bound = %8)

*+  Calls
      dll_import read_char_var_optional

*+  Changes
*     25-11-1997 - neilh - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'search_read_char_var')

*+  Local Variables
      integer counter                  ! simple counter variable

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      do 100 counter = 1, number_of_sections
 
        call read_char_var_optional
     :                    (section_names(counter)
     :                    , variable_name
     :                    , units
     :                    , variable
     :                    , numvals)
 
         if (numvals.gt.0) then
            ! we have found what we want - exit.
            goto 200
         else
            ! try next section name
         endif
 
  100 continue
      ! If we reach this point of execution the search must have failed
      call Fatal_Error (ERR_User,
     :    'Parameter Search Failure. Unable to read variable '
     :    //variable_name)
 
  200 continue
 
      call pop_routine (myname)
      return
      end



* ====================================================================
      subroutine search_read_integer_var (section_names
     :                                ,number_of_sections
     :                                ,variable_name
     :                                ,units
     :                                ,variable
     :                                ,numvals
     :                                ,lower
     :                                ,upper)
* ====================================================================
      implicit none
      include 'const.inc'
      include 'error.pub'

*+  Sub-Program Arguments
      character section_names(*)*(*)   ! list of sections to search
      integer   number_of_sections     ! number of sections to search
      character variable_name*(*)      ! variable to search for
      character units*(*)              ! required units of variable
      integer   variable               ! variable value to return
      integer   numvals                ! number of values returned
      real      lower                  ! lower bound of variable value
      real      upper                  ! upper bound of variable value

*+  Purpose
*      Read a real value from input files using a list of possible sections
*      for the search.

*+  Mission Statement
*   Search for %5 in files (Lower Bound = %7, Upper Bound = %8)

*+  Calls
      dll_import read_integer_var_optional

*+  Changes
*     25-11-1997 - neilh - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'search_read_integer_var')

*+  Local Variables
      integer counter                  ! simple counter variable

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      do 100 counter = 1, number_of_sections
 
        call read_integer_var_optional
     :                    (section_names(counter)
     :                    , variable_name
     :                    , units
     :                    , variable
     :                    , numvals
     :                    , lower
     :                    , upper)
 
         if (numvals.gt.0) then
            ! we have found what we want - exit.
            goto 200
         else
            ! try next section name
         endif
 
  100 continue
      ! If we reach this point of execution the search must have failed
      call Fatal_Error (ERR_User,
     :    'Parameter Search Failure. Unable to read variable '
     :    //variable_name)
 
  200 continue
 
      call pop_routine (myname)
      return
      end



* ====================================================================
      subroutine search_read_integer_array (section_names
     :                                ,number_of_sections
     :                                ,array_name
     :                                ,array_size
     :                                ,units
     :                                ,array
     :                                ,numvals
     :                                ,lower
     :                                ,upper)
* ====================================================================
      implicit none
      include 'const.inc'
      include 'error.pub'

*+  Sub-Program Arguments
      character section_names(*)*(*)   ! list of sections to search
      integer   number_of_sections     ! number of sections to search
      character array_name*(*)         ! variable to search for
      integer   array_size             ! max. size of array
      character units*(*)              ! required units of variable
      integer   array(*)               ! variable value to return
      integer   numvals                ! number of values returned
      real      lower                  ! lower bound of variable value
      real      upper                  ! upper bound of variable value

*+  Purpose
*      Read an array from input files using a list of possible sections
*      for the search.

*+  Mission Statement
*   Search for %5 in files (Lower Bound = %7, Upper Bound = %8)

*+  Changes
*     25-11-1997 - neilh - Programmed and Specified

*+  Calls
      dll_import read_integer_array_optional

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'search_read_integer_array')

*+  Local Variables
      integer counter                  ! simple counter variable

*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      do 100 counter = 1, number_of_sections
 
        call read_integer_array_optional
     :                    (section_names(counter)
     :                    , array_name
     :                    , array_size
     :                    , units
     :                    , array
     :                    , numvals
     :                    , lower
     :                    , upper)
 
         if (numvals.gt.0) then
            ! we have found what we want - exit.
            goto 200
         else
            ! try next section name
         endif
 
  100 continue
      ! If we reach this point of execution the search must have failed
      call Fatal_Error (ERR_User,
     :    'Parameter Search Failure. Unable to read variable '
     :    //array_name)
 
  200 continue
 
      call pop_routine (myname)
      return
      end


