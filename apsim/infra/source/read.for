

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
 
*+ Calls
      dll_import read_param
      dll_import split_off_units
      dll_import string_to_real_var
      dll_import bound_check_real_var
      character read_param*(function_string_len)
                                       ! function
 
*+ Constant Values
      logical no_multiple_keys         ! Don't look for multiple keys
      parameter (no_multiple_keys=.false.)
 
*+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
      character return_units*50        ! units of returned value
 
*- Implementation Section ----------------------------------
 
      return_string = read_param
     .   (section_name, variable_name, no_multiple_keys)
 
      if (return_string .ne. blank) then
         call split_off_units (return_string, return_units)
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
 
*+ Calls
      dll_import read_param
      dll_import split_off_units
      dll_import string_to_integer_var
      dll_import bound_check_integer_var
      character read_param*(function_string_len)
                                       ! function
 
*+ Constant Values
      logical no_multiple_keys         ! Don't look for multiple keys
      parameter (no_multiple_keys=.false.)
 
*+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
      character return_units*50        ! units of returned value
 
*- Implementation Section ----------------------------------
 
      return_string = read_param
     .   (section_name, variable_name, no_multiple_keys)
 
      if (return_string .ne. blank) then
         call split_off_units (return_string, return_units)
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
 
*+ Calls
      dll_import read_param
      dll_import split_off_units
      dll_import string_to_double_var
      dll_import bound_check_double_var
      character read_param*(function_string_len)
                                       ! function
 
*+ Constant Values
      logical no_multiple_keys         ! Don't look for multiple keys
      parameter (no_multiple_keys=.false.)
 
*+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
      character return_units*50        ! units of returned value
 
*- Implementation Section ----------------------------------
 
      return_string = read_param
     .   (section_name, variable_name, no_multiple_keys)
 
      if (return_string .ne. blank) then
         call split_off_units (return_string, return_units)
 
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
 
*+ Calls
      dll_import read_param
      dll_import split_off_units
      dll_import string_to_logical_var
      character read_param*(function_string_len)
                                       ! function
 
*+ Constant Values
      logical no_multiple_keys         ! Don't look for multiple keys
      parameter (no_multiple_keys=.false.)
 
*+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
      character return_units*50        ! units of returned value
 
*- Implementation Section ----------------------------------
 
      return_string = read_param
     .   (section_name, variable_name, no_multiple_keys)
 
      if (return_string .ne. blank) then
         call split_off_units (return_string, return_units)
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
 
*+ Calls
      dll_import read_param
      character read_param*(function_string_len)
                                       ! function
 
*+ Constant Values
      logical no_multiple_keys         ! Don't look for multiple keys
      parameter (no_multiple_keys=.false.)
 
*+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
 
*- Implementation Section ----------------------------------
 
      return_string = read_param
     .   (section_name, variable_name, no_multiple_keys)
      variable = return_string
      if (return_string .eq. blank) then
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
 
*+ Calls
      dll_import read_param_optional
      dll_import split_off_units
      dll_import string_to_real_var
      dll_import bound_check_real_var
      character read_param_optional*(function_string_len)
                                       ! function
 
*+ Constant Values
      logical no_multiple_keys         ! Don't look for multiple keys
      parameter (no_multiple_keys=.false.)
 
*+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
      character return_units*50        ! Units found on string
 
*- Implementation Section ----------------------------------
 
      return_string = read_param_optional
     .   (section_name, variable_name, no_multiple_keys)
 
      call split_off_units (return_string, return_units)
 
      if (return_string .eq. blank) then
         variable = 0.0
         numvals = 0
 
      else
         call string_to_real_var
     .      (return_string, variable, numvals)
         call bound_check_real_var
     .     (variable, lower_limit, upper_limit, variable_name)
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
 
*+ Calls
      dll_import read_param_optional
      dll_import split_off_units
      dll_import string_to_integer_var
      dll_import bound_check_integer_var
      character read_param_optional*(function_string_len)
                                       ! function
 
*+ Constant Values
      logical no_multiple_keys         ! Don't look for multiple keys
      parameter (no_multiple_keys=.false.)
 
*+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
      character return_units*50        ! units of returned value
 
*- Implementation Section ----------------------------------
 
      return_string = read_param_optional
     .   (section_name, variable_name, no_multiple_keys)
 
      call split_off_units (return_string, return_units)
      if (return_string .eq. blank) then
         variable = 0
         numvals = 0
 
      else
         call string_to_integer_var
     .      (return_string, variable, numvals)
         call bound_check_integer_var
     .      (variable, lower_limit, upper_limit, variable_name)
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
 
*+ Calls
      dll_import read_param_optional
      dll_import split_off_units
      dll_import string_to_double_var
      dll_import bound_check_double_var
      character read_param_optional*(function_string_len)
                                       ! function
 
*+ Constant Values
      logical no_multiple_keys         ! Don't look for multiple keys
      parameter (no_multiple_keys=.false.)
 
*+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
      character return_units*50        ! units of returned value
 
*- Implementation Section ----------------------------------
 
      return_string = read_param_optional
     .   (section_name, variable_name, no_multiple_keys)
 
      call split_off_units (return_string, return_units)
      if (return_string .eq. blank) then
         variable = 0.0d0
         numvals = 0
 
      else
         call string_to_double_var
     .      (return_string, variable, numvals)
 
         call bound_check_double_var
     .      (variable, lower_limit, upper_limit, variable_name)
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
 
*+ Calls
      dll_import read_param_optional
      dll_import split_off_units
      dll_import string_to_logical_var
      character read_param_optional*(function_string_len)
                                       ! function
 
*+ Constant Values
      logical no_multiple_keys         ! Don't look for multiple keys
      parameter (no_multiple_keys=.false.)
 
*+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
      character return_units*50        ! units of returned value
 
*- Implementation Section ----------------------------------
 
      return_string = read_param_optional
     .   (section_name, variable_name, no_multiple_keys)
      call split_off_units (return_string, return_units)
      if (return_string .eq. blank) then
         variable = .false.
         numvals = 0
 
      else
         call string_to_logical_var
     .      (return_string, variable, numvals)
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
 
*+ Calls
      dll_import read_param_optional
      dll_import split_off_units
      character read_param_optional*(function_string_len)
                                       ! function
 
*+ Constant Values
      logical no_multiple_keys         ! Don't look for multiple keys
      parameter (no_multiple_keys=.false.)
 
*+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
      character return_units*50        ! Units found on string
 
*- Implementation Section ----------------------------------
 
      return_string = read_param_optional
     .   (section_name, variable_name, no_multiple_keys)
      call split_off_units(return_string, return_units)
 
      variable = return_string
      if (return_string .eq. blank) then
         numvals = 0
 
      else
         numvals = 1
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
 
*+ Calls
      dll_import read_param
      dll_import split_off_units
      dll_import string_to_real_array
      dll_import bound_check_real_array
      character read_param*(function_string_len)
                                       ! function
 
*+ Constant Values
      logical no_multiple_keys         ! Don't look for multiple keys
      parameter (no_multiple_keys=.false.)
 
*+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
      character return_units*50        ! units of returned value
 
*- Implementation Section ----------------------------------
 
      return_string = read_param
     .   (section_name, variable_name, no_multiple_keys)
 
      if (return_string .ne. blank) then
         call split_off_units (return_string, return_units)
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
 
*+ Calls
      dll_import read_param
      dll_import split_off_units
      dll_import string_to_integer_array
      dll_import bound_check_integer_array
      character read_param*(function_string_len)
                                       ! function
 
*+ Constant Values
      logical no_multiple_keys         ! Don't look for multiple keys
      parameter (no_multiple_keys=.false.)
 
*+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
      character return_units*50        ! units of returned value
 
*- Implementation Section ----------------------------------
 
      return_string = read_param
     .   (section_name, variable_name, no_multiple_keys)
 
      if (return_string .ne. blank) then
         call split_off_units (return_string, return_units)
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
 
*+ Calls
      dll_import read_param
      dll_import split_off_units
      dll_import string_to_double_array
      dll_import bound_check_double_array
      character read_param*(function_string_len)
                                       ! function
 
*+ Constant Values
      logical no_multiple_keys         ! Don't look for multiple keys
      parameter (no_multiple_keys=.false.)
 
*+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
      character return_units*50        ! units of returned value
 
*- Implementation Section ----------------------------------
 
      return_string = read_param
     .   (section_name, variable_name, no_multiple_keys)
 
      if (return_string .ne. blank) then
         call split_off_units (return_string, return_units)
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
 
*+ Calls
      dll_import read_param
      dll_import split_off_units
      dll_import string_to_logical_array
      character read_param*(function_string_len)
                                       ! function
 
*+ Constant Values
      logical no_multiple_keys         ! Don't look for multiple keys
      parameter (no_multiple_keys=.false.)
 
*+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
      character return_units*50        ! units of returned value
 
*- Implementation Section ----------------------------------
 
      return_string = read_param
     .   (section_name, variable_name, no_multiple_keys)
 
      if (return_string .ne. blank) then
         call split_off_units (return_string, return_units)
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
 
*+ Calls
      dll_import read_param
      dll_import split_off_units
      dll_import string_to_char_array
      character read_param*(function_string_len)
                                       ! function
 
*+ Constant Values
      logical no_multiple_keys         ! Don't look for multiple keys
      parameter (no_multiple_keys=.false.)
 
*+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
      character return_units*50        ! units of returned value
 
*- Implementation Section ----------------------------------
 
      return_string = read_param
     .   (section_name, variable_name, no_multiple_keys)
 
      if (return_string .ne. blank) then
         call split_off_units (return_string, return_units)
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
 
*+ Calls
      dll_import read_param_optional
      dll_import split_off_units
      dll_import fill_real_array
      dll_import string_to_real_array
      dll_import bound_check_real_array
      character read_param_optional*(function_string_len)
                                       ! function
 
*+ Constant Values
      logical no_multiple_keys         ! Don't look for multiple keys
      parameter (no_multiple_keys=.false.)
 
*+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
      character return_units*50        ! units of returned value
 
*- Implementation Section ----------------------------------
 
      return_string = read_param_optional
     .   (section_name, variable_name, no_multiple_keys)
 
      call split_off_units (return_string, return_units)
      if (return_string .eq. blank) then
         call fill_real_array(variable, 0.0, size_of)
         numvals = 0
 
      else
         call string_to_real_array
     .     (return_string, variable, size_of, numvals)
         call bound_check_real_array
     .      (variable, lower_limit, upper_limit, variable_name, numvals)
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
 
*+ Calls
      dll_import read_param_optional
      dll_import split_off_units
      dll_import fill_integer_array
      dll_import string_to_integer_array
      dll_import bound_check_integer_array
      character read_param_optional*(function_string_len)
                                       ! function
 
*+ Constant Values
      logical no_multiple_keys         ! Don't look for multiple keys
      parameter (no_multiple_keys=.false.)
 
*+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
      character return_units*50        ! units of returned value
 
*- Implementation Section ----------------------------------
 
      return_string = read_param_optional
     .   (section_name, variable_name, no_multiple_keys)
 
      call split_off_units (return_string, return_units)
      if (return_string .eq. blank) then
         call fill_integer_array(variable, 0, size_of)
         numvals = 0
 
      else
         call string_to_integer_array
     .      (return_string, variable, size_of, numvals)
         call bound_check_integer_array
     .      (variable, lower_limit, upper_limit, variable_name, numvals)
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
 
*+ Calls
      dll_import read_param_optional
      dll_import split_off_units
      dll_import fill_double_array
      dll_import string_to_double_array
      dll_import bound_check_double_array
      character read_param_optional*(function_string_len)
                                       ! function
 
*+ Constant Values
      logical no_multiple_keys         ! Don't look for multiple keys
      parameter (no_multiple_keys=.false.)
 
*+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
      character return_units*50        ! units of returned value
 
*- Implementation Section ----------------------------------
 
      return_string = read_param_optional
     .   (section_name, variable_name, no_multiple_keys)
      call split_off_units (return_string, return_units)
      if (return_string .eq. blank) then
         call fill_double_array(variable, 0.0d0, size_of)
         numvals = 0
 
      else
         call string_to_double_array
     .      (return_string, variable, size_of, numvals)
         call bound_check_double_array
     .      (variable, lower_limit, upper_limit, variable_name, numvals)
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
 
*+ Calls
      dll_import read_param_optional
      dll_import split_off_units
      dll_import fill_logical_array
      dll_import string_to_logical_array
      character read_param_optional*(function_string_len)
                                       ! function
 
*+ Constant Values
      logical no_multiple_keys         ! Don't look for multiple keys
      parameter (no_multiple_keys=.false.)
 
*+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
      character return_units*50        ! units of returned value
 
*- Implementation Section ----------------------------------
 
      return_string = read_param_optional
     .   (section_name, variable_name, no_multiple_keys)
 
      call split_off_units (return_string, return_units)
      if (return_string .eq. blank) then
         call fill_logical_array(variable, .false., size_of)
         numvals = 0
 
      else
         call string_to_logical_array
     .      (return_string, variable, size_of, numvals)
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
 
*+ Calls
      dll_import read_param_optional
      dll_import fill_char_array
      dll_import split_off_units
      dll_import string_to_char_array
      character read_param_optional*(function_string_len)
                                       ! function
 
*+ Constant Values
      logical no_multiple_keys         ! Don't look for multiple keys
      parameter (no_multiple_keys=.false.)
 
*+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
      character return_units*50        ! units of returned value
 
*- Implementation Section ----------------------------------
 
      return_string = read_param_optional
     .   (section_name, variable_name, no_multiple_keys)
      if (return_string .eq. blank) then
         call fill_char_array(variable, blank, size_of)
         numvals = 0
 
      else
         call split_off_units (return_string, return_units)
         call string_to_char_array
     .      (return_string, variable, size_of, numvals)
      endif
 
      return
      end
 
 
 


* ====================================================================
       character*(*) function get_data_string (char_string, key_name)
* ====================================================================
      implicit none
      dll_export get_data_string
       include 'const.inc'             ! Constant definitions
 
*+ Sub-Program Arguments
       character char_string*(*)       ! (INPUT) Character string to look in
       character key_name*(*)          ! (INPUT) Key name to search for
 
*+ Purpose
*      Get a parameter string from a character string that
*      matches Key_name.  Returns blank if not found.
 
*+ Assumptions
*      Example of a char_string is
*         sw=30 25 15 15 ! yield=1.6
 
*+ Notes
*     It is possible for this routine to return a blank string even
*     when the key_name was found but had no string to the right
*     of the equals sign.
 
*+  Mission Statement
*      
 
*+ Changes
*      DPH - 16/11/92
*      DPH - 9/06/93 Modified to use the get_next_variable routine
*                    Comments upgraded as suggested by PG.
*      DPH - 12/08/93 Removed leading spaces from return string.
*     JNGH 3/8/94 used assign_substring s/r to  set function.
*                 made character strings same matching lengths
 
*+ Calls
      dll_import lower_case
      dll_import no_spaces
      dll_import assign_string
      dll_import get_next_variable
       character lower_case*30         ! function
       character no_spaces*30          ! function
 
*+ Local Variables
       character char_string_lower
     .    *(function_string_len)       ! Lower case version of char_string
       character key*30                ! Key pulled apart from key_name_lower
       character key_name_lower*30     ! Lower case version of key name
       character parameters
     .    *(function_string_len)       ! Parameters to right of '=' sign
 
*- Implementation Section ----------------------------------
 
      key_name_lower = lower_case (key_name)
      key_name_lower = no_spaces (key_name_lower)
 
cjh      shouldn't need this lower case conversion
cjh      call assign_string (char_string_lower, lower_case (char_string))
      call assign_string (char_string_lower, char_string)
 
10    continue
      call get_next_variable (char_string_lower, key, parameters)
 
      if (key .eq. key_name_lower) then
         call assign_string (get_data_string
     :                      , adjustl(parameters))
 
      else if (key .eq. blank) then
         get_data_string = blank
 
      else
         goto 10
 
      endif
 
      return
      end
 
 
 


* ====================================================================
       subroutine get_next_variable (variables_str,
     .            var_name, values_str)
* ====================================================================
      implicit none
      dll_export get_next_variable
       include 'const.inc'             ! Constant definitions
 
*+ Sub-Program Arguments
       character variables_str*(*)     ! (INPUT & OUTPUT) String to break up
       character var_name*(*)          ! (OUTPUT) Extracted variable name
       character values_str*(*)        ! (OUTPUT) Extracted values string
 
*+ Purpose
*      Returns the next variable from Variables_str and its
*      associated data.
 
*+  Definition
*     variables_str must contain an identifier followed by an
*     '=' followed by an arbitrary sequence of characters.
*     I will refer to this arbitrary sequence of
*     characters as value.  The ',' is used to separate sets of
*     identifiers and their values, so if there is one present
*     before the end of variables_str, then value ends just
*     before the ',', otherwise value goes to the end of
*     variables_str.  This subroutine will assign the
*     identifier with all spaces removed to var_name.  If there
*     is not enough space in var_name for it, a warning error is
*     flagged and truncation occurs.  This subroutine will
*     assign value with leading spaces removed to values_str.  
*     If there is not enough space in values_str for it, a
*     warning error is flagged and truncation occurs.
 
*+ Notes
*      Example of a typical variables_str is :-
*         sw=30 25 15 15, yield=1.6
*      In this example, this routine would return sw as the var_name
*      and "30 25 15 15" as the values_str.
*      Variables_str would be updated to "yield=1.6"
 
*+  Mission Statement
*      
 
*+ Changes
*      DPH - 16/11/92
*      DPH - 9/06/93 Modified to accept situations when Variables_str
*                    doesn't have a MES_delimiter in it.
*      DPH - 10/09/93 Made sure Var_name was returned with no leading spaces
*     JNGH 3/8/94 used assign_string s/r to  set arguments.
*      DPH - 19/10/95 Changed Delimited from MES_Delimiter to a comma
*      DPH - 6/11/95 Added call to No_leading_spaces to strip off leading
*                    spaces from values_str.
 
*+ Calls
      dll_import split_line
      dll_import assign_string
      dll_import no_spaces
       character no_spaces*(function_string_len)
 
*+ Constant Values
      character delimiter*(*)          ! Delimiter to use to separate variables.
      parameter (delimiter=',')
*
       character equals*(*)            ! Equals sign
       parameter (equals='=')
 
*+ Local Variables
       integer pos                     ! Position in variables_str
       character string_right
     .    *(function_string_len)       ! String to right of equals sign
 
*- Implementation Section ----------------------------------
 
      call split_line (variables_str, var_name, string_right, equals)
      call assign_string (variables_str, string_right)
 
      pos = index (variables_str, delimiter)
 
      ! Handle the situation when no MES_delimiter is in Variables_str.
 
      if (pos .eq. 0) then
         call assign_string (values_str, variables_str)
         variables_str = blank
 
      else
         call assign_string (values_str, variables_str (1:pos-1))
         variables_str = variables_str (pos+1:)
      endif
 
      var_name = no_spaces(var_name)
      values_str = adjustl(values_str)
 
      return
      end
 
 
 


* ====================================================================
       subroutine read_line (logical_unit, record_num, line, iostatus)
* ====================================================================
      implicit none
      dll_export read_line
       include 'const.inc'             ! constant definitions
 
*+ Sub-Program Arguments
       character line*(*)              ! (OUTPUT) Line read from file
       integer logical_unit            ! (INPUT) Logical unit to read from
       integer record_num              ! (OUTPUT) Record number in file
       integer iostatus                ! (OUTPUT) Status of read
 
*+ Purpose
*      Read in the next uncommented, non blank line from the control file.
*      Return an I/O status (0=Ok, 1=End_of_runblock, 2=End_of_file).
*      The line returned is converted to lowercase.
 
*+  Definition
*     A comment begins with an exclamation mark and continues up
*     to the end of the line.  A line with no non blank
*     characters other than those within a comment is considered
*     to be a blank line.  This subroutine will try to read the
*     next non blank line from logical unit "logical_unit" into
*     "line".  All uppercase character encountered are folded into
*     their corresponding lowercase characters.  If the end of
*     file is encountered before a non blank line, then "iostatus"
*     is set to 2.  Otherwise if the non blank line finally
*     assigned to line is equal to 'end run', then
*     "iostatus" is set to 1, otherwise "iostatus" is set to 0.  
*     Every time a line is read, regardless of whether it is
*     blank, or end of file, "record_num" is incremented.  
 
*+ Assumptions
*      Assumes that line is long enough to hold an entire line read from
*      control file.
 
*+  Mission Statement
*      
 
*+ Changes
*      DPH - 11/6/92
*      DPH - 23/11/92 Now passing in logical unit number
*      DPH - 9/06/93 Adjusted comments
*      DPH - 8/7/94  Added code to remove all types of comments
*                    include inline comments from line.
*      dph - 7/3/97  added code to check and remove tab characters.
 
*+ Calls
      dll_import lower_case
       character lower_case*(function_string_len)
 
*+ Constant Values
       character comment*(*)           ! Comment specifier
       parameter (comment='!')
*
       integer tab_char                ! TAB character
       parameter (tab_char=9)
*
       integer end_file_status         ! End of file status
       parameter (end_file_status=2)
*
       character end_run*(*)           ! End of run specifier
       parameter (end_run='end run')
*
       integer end_run_status          ! End of run block status
       parameter (end_run_status=1)
*
       integer ok_status               ! Ok status
       parameter (ok_status = 0)
 
*+ Local Variables
       integer comment_pos             ! Position of comment in line
       integer read_status             ! Status of read.
       integer tab_pos                 ! position of tab character
 
*- Implementation Section ----------------------------------
 
10    read (logical_unit, '(a)', iostat=read_status ) line
      record_num = record_num + 1
 
      ! Remove comment from line if necessary
 
      comment_pos = index (line, comment)
      if (comment_pos .eq. 0) then
         ! No comment on line
 
      else
         line(comment_pos:) = blank
      endif
 
      ! remove tab character if necessary.
      tab_pos = index(line, char(tab_char))
      if (tab_pos .gt. 0) then
         line(tab_pos:tab_pos) = ' '
      endif
 
      if (read_status .eq. ok_status) then
 
         line = lower_case(line)
         line = adjustl(line)
 
         if (line.eq.end_run) then
            iostatus = end_run_status
 
         else if (line.eq.blank) then
            goto 10
 
         else
            iostatus = ok_status
 
         endif
 
      else
         iostatus = end_file_status
 
      endif
 
      return
      end
 
 
 


* ====================================================================
       integer function open_param_file (section_name)
* ====================================================================
      implicit none
      dll_export open_param_file
      include 'const.inc'              ! Constant definitions
      include 'read.inc'
 
*+ Sub-Program Arguments
      character section_name*(*)       ! (INPUT) Section name to find
 
*+ Purpose
*     Get the name of the parameter file for a module from the control
*     file and try to open it.  If successful, then return the
*     logical unit number to caller, otherwise return -1.
 
*+  Mission Statement
*      
 
*+ Changes
*     DPH - 5/08/93
*     JNGH 3/8/94 moved lu_param to more logical position
*                 changed get_file_string to same length as file_name
*                    used file_not_found flag instead of -1
*     DPH - 20/10/94 Modified to get module name from engine instead
*           of through argument.
*     jngh 21/2/95 changed write to string to replacement statement.
*     jngh 24/03/95 changed to handle multisection names
 
*+ Calls
      dll_import push_routine
      dll_import get_current_module
      dll_import find_next_matching_section
      dll_import fatal_error
      dll_import pop_routine
 
*+ Constant Values
      character this_routine*(*)       ! Name of this routine
      parameter (this_routine='open_param_file')
 
*+ Local Variables
      character msg*300                ! Message to display
      integer lu_param                 ! Logical unit number of open file
      character module_name*(max_module_name_size)
                                       ! Module name of caller
      integer record_num               ! Record number in file
      character  full_section_name*100
      character  exclude_section_name*10
      integer    exclude_file_num
 
*- Implementation Section ----------------------------------
 
      call push_routine(this_routine)
 
      call get_current_module(module_name)
 
      lu_param = -1
      record_num = 1
      exclude_file_num = 0
      exclude_section_name = '..........'
      full_section_name = blank
      search_section_index = 0
 
      call find_next_matching_section (
     :       module_name, section_name, full_section_name
     :     , lu_param, record_num, exclude_file_num
     :     , exclude_section_name)
 
      if (lu_param.gt.0) then
          ! Everything ok - file and section found
 
      else
         msg = 'Cannot find section : '// section_name
 
         call fatal_error(err_user, msg)
      endif
 
      open_param_file = lu_param
 
      call pop_routine(this_routine)
      return
      end
 
 
 


* ====================================================================
       character*(*) function get_section_name (record)
* ====================================================================
      implicit none
      dll_export get_section_name
      include   'const.inc'            ! constant definition file
 
*+ Sub-Program Arguments
      character  record*(*)            ! record from initialisation file
 
*+ Purpose
*   This function extracts the name of a section in an initialisation file
*   from a section definition line.  If the record does not contain a
*   section definition a blank value is returned
 
*+  Definition
*     A section definition line consists of zero or more blanks
*     followed by an opening square bracket followed by zero or
*     more blanks followed by a section heading followed by a
*     closing square bracket.  If "record" is a section
*     definition line, then this function will return the
*     section heading contained in it.  Otherwise it will return
*     blank.  
 
*+  Mission Statement
*      
 
*+ Changes
*   NIH - 15/06/94 Programmed and specified
*   080994 jngh cleaned up
*   DPH 19/10/94 Removed routine name argument from call to warning_error
*   DPH 8/11/94 Added a quick test for left_delimiter to front of
*               routine for speed reasons.
 
*+ Calls
      dll_import push_routine
      dll_import assign_string
      dll_import lower_case
      dll_import warning_error
      dll_import pop_routine
      character  lower_case*(function_string_len)
 
*+ Local Variables
      logical    section_found         ! legal section has  been found
      integer    left_delimiter_pos    ! position of left delimiter in record
      integer    right_delimiter_pos   ! position of left delimiter in record
      character  section*(max_section_name_size)
                                       ! name of section of ini file
      character  temprecord*(function_string_len)
                                       ! internal copy of record
*
*   Constant valuesz
      character  open_delimiter*(*)    ! section open delimiter
      parameter (open_delimiter = '[')
*
      character  close_delimiter*(*)   ! section close delimiter
      parameter (close_delimiter = ']')
*
      character  myname*(*)            ! name of current procedure
      parameter (myname = 'get_section_name')
 
*- Implementation Section ----------------------------------
 
      call push_routine (myname)
 
      ! Firstly do a quick test for a section name.
 
      if (index(record, open_delimiter) .eq. 0) then
         ! There's no section here - exit.
 
         section = blank
 
      else
         ! OK we could have a section name - find out.
 
         temprecord= adjustl(record)
         left_delimiter_pos = index (temprecord, open_delimiter)
         right_delimiter_pos = index (temprecord, close_delimiter)
 
         section_found = left_delimiter_pos .eq. 1
     :             .and. right_delimiter_pos .ge. 2
 
         if (section_found) then
                  ! We have a section heading
 
            if (right_delimiter_pos .ge. 3) then
               call assign_string (section, temprecord
     :                     (left_delimiter_pos+1:right_delimiter_pos-1))
               section = adjustl(lower_case (section))
 
            else
                  ! This is an empty section heading
               section = blank
            endif
 
            if (section.eq.blank) then
                  ! we have no section name specified
               call warning_error (err_user
     :                          , 'Blank section name in record')
            else
            endif
 
         else
               ! This is not a section heading
            section = blank
         endif
      endif
 
      call assign_string (get_section_name, section)
 
      call pop_routine (myname)
      return
      end
 
 
 


* ====================================================================
       character*(*) function read_param_optional
     .     (section_spec, key_name, multiple_keys)
* ====================================================================
      implicit none
      dll_export read_param_optional
      include 'const.inc'              ! constant definitions
      include 'read.inc'
 
*+ Sub-Program Arguments
      character section_spec*(*)       ! (INPUT) Section specifier to search for
      character key_name*(*)           ! (INPUT) Key name to search in
      logical multiple_keys            ! (INPUT) Should we look for multiple
                                       ! keys?
 
*+ Purpose
*     Loop through all files specified for the current module looking
*     for the specified key name in the specified section.
*     Don't call fatal_error if key_name not found.
*     Multiple keys specifies if the routine should look for more
*     than one key in the section.
 
*+  Mission Statement
*      
 
*+ Changes
*     DPH 8/11/94
*     DPH 10/4/96  Added code so that multiple_keys works across multiple
*                  files/sections instead of within a single section.
*     JNGH 22/06/96 changed function string_concat to call append_string
*     dph 25/7/96  changed line from if (.not. Found_key .or. Multiple_keys) then
*                  to if (.not. Found_key) then
*     jngh 24/10/96 Added test for current_unit_num being 0, so new file
*                   is opened.
*     sb 15/7/98  Added line to set current_unit_num to -1 when closed.
 
*+ Calls
      dll_import push_routine
      dll_import lower_case
      dll_import get_current_module
      dll_import read_key_from_section
      dll_import close_unit
      dll_import find_next_matching_section
      dll_import append_string
      dll_import pop_routine
      character read_key_from_section*(function_string_len)
                                       ! function
      character lower_case*(function_string_len)
                                       ! function
 
*+ Constant Values
      character myname*(*)             ! Name of this routine
      parameter (myname='Read_param_optional')
 
*+ Local Variables
      character current_module_name*(max_module_name_size)
                                       ! Name of current module
      integer exclude_file_num         ! Exclude which file from search for
                                       ! sections
      logical found_key                ! Have we found the key?
      logical found_section            ! Have we found the section?
      character return_string*(function_string_len)
                                       ! String to return to caller.
      character new_string*(function_string_len)
                                       ! String to return to caller.
      logical same_module_calling      ! Is the same module calling
                                       ! as previous call?
      logical same_section_spec        ! Is the section specifier the same as
                                       ! previous call?
 
*- Implementation Section ----------------------------------
 
      call push_routine(myname)
 
      return_string = blank
 
      ! Convert section name to lower case
 
      section_spec = lower_case(section_spec)
 
      ! Determine if this is the same module calling as last module
 
      call get_current_module(current_module_name)
      same_module_calling = (last_module_name .eq. current_module_name)
 
      ! Determine if this section name is the same as the last section.
 
      same_section_spec = (last_section_spec .eq. section_spec)
 
      if (same_module_calling .and. same_section_spec
     :   .and. current_unit_num.ne.0) then
         ! The same module is still calling this routine with the same
         ! section so continue reading where we left off.
 
         return_string = read_key_from_section
     .            (current_unit_num, current_record_num,
     .             last_section_name, key_name, multiple_keys)
 
         found_key = (return_string .ne. blank)
         exclude_file_num = search_section_index
 
      else
         found_key = .false.
         exclude_file_num = 0
      endif
 
      if (.not. found_key) then
 
         last_module_name = current_module_name
         last_section_spec = section_spec
         search_section_index = 0
 
10       continue
         ! We need to go find the section name so close off the
         ! old unit number we were looking in.
 
         if (current_unit_num .gt. lu_control_file2) then
            call close_unit(current_unit_num)
            current_unit_num = -1
 
         else
            ! This is the first time through this routine
         endif
 
         call find_next_matching_section
     .        (current_module_name, section_spec, last_section_name,
     .         current_unit_num, current_record_num,
     .         exclude_file_num, 'weather')
 
         found_section = (current_unit_num .gt. 0)
 
         if (found_section) then
            new_string = read_key_from_section
     .          (current_unit_num, current_record_num,
     .           last_section_name, key_name, multiple_keys)
            found_key = (new_string .ne. blank)
 
            ! Append new_string to return string if multiple_keys was specified
            ! and found_key = true.
 
            if (found_key .and. multiple_keys) then
               new_string = blank // new_string
               call append_string (return_string, new_string)
            endif
 
            if (found_key .and. .not. multiple_keys) then
               ! Need look no further.  Report to summary file
 
               return_string = new_string
               if (current_param_file_name .ne. blank) then
*DPH                  write(msg, '(50a)' )
*DPH     .               'File opened ok -> ',
*DPH     .               Current_param_file_name
*DPH                  call Write_string(LU_Scr_sum, msg)
 
               else
                  ! Dont report the control file usage.
               endif
 
            else
               ! Not found.  Keep looking through other sections until
               ! we find the key name.
 
               goto 10
            endif
 
         else
            ! Cannot find a section name therefore cannot continue
 
            last_module_name = blank
 
         endif
      endif
 
      read_param_optional = return_string
 
      call pop_routine(myname)
 
      return
      end
 
 
 


* ====================================================================
       character*(*) function read_param
     .     (section_spec, key_name, multiple_keys)
* ====================================================================
      implicit none
      dll_export read_param
      include 'const.inc'
 
*+ Sub-Program Arguments
      character section_spec*(*)       ! (INPUT) Section specifier to search in
      character key_name*(*)           ! (INPUT) Key name to search in
      logical multiple_keys            ! (INPUT) Should we look for multiple
                                       ! keys?
 
*+ Purpose
*     Loop through all files specified for the current module looking
*     for the specified key name in the specified section.
*     Call fatal_error if key_name not found.
*     Multiple keys specifies if the routine should look for more
*     than one key in the section.
 
*+  Mission Statement
*      
 
*+ Changes
*     DPH 8/11/94
*     jngh 21/2/95 changed write to string to replacement statement.
 
*+ Calls
      dll_import push_routine
      dll_import read_param_optional
      dll_import fatal_error
      dll_import pop_routine
      character read_param_optional*(function_string_len)
                                       ! function
 
*+ Constant Values
      character myname*(*)             ! Name of this routine
      parameter (myname='Read_param')
 
*+ Local Variables
      character msg*300                ! err message to display
 
*- Implementation Section ----------------------------------
 
      call push_routine(myname)
 
      read_param = read_param_optional
     .    (section_spec, key_name, multiple_keys)
 
      if (read_param .eq. blank) then
         ! Cannot find key name.  Issue fatal err message.
 
         msg =
     .      'Cannot find a parameter in any of the files/sections'//
     .      new_line//
     .      'specified in the control file.  Parameter name = '//
     .      key_name//
     .      new_line//
     .      'Section specifier = '// section_spec
 
         call fatal_error(err_user, msg)
 
      else
         ! Found the key name - exit
 
      endif
 
      call pop_routine(myname)
      return
      end
 
 
 


* ====================================================================
       subroutine read_next_param_section
     .     (module_name, line, flag, section)
* ====================================================================
      implicit none
      dll_export read_next_param_section
      include 'const.inc'
      include 'read.inc'
 
*+ Sub-Program Arguments
      character module_name*(*)    ! name of the module asking for info
      character line*(*)           ! line of file returned to the module
      integer   flag               ! flag for desired response
      character section*(*)        ! section name to append to all sections
 
*+ Purpose
*   This routine is designed for consecutive call usage.  The routine is
*   to be iterated until all parameter section lines have been read.
*   The routine will read each line from each section in files specified
*   after the module name in the current control file APSIM run section.
*   When the entire search is finished a blank line is returned.
*   Flag values are to be 0 for normal iteration, 1 to restart search,
*   and 2 for user to end search prematurely.  Section specifies what suffix
*   to add to all sections for specified module.  Only these sections will then
*   be searched.
 
*+  Mission Statement
*      
 
*+ Changes
*   DPH 13/3/95
 
*+ Calls
      dll_import read_next_param_line
 
*- Implementation Section ----------------------------------
 
      section_suffix = section
      call read_next_param_line(module_name, line, flag)
      section_suffix = blank
 
      return
      end
 
 
 


* ====================================================================
       integer function get_logical_unit ()
* ====================================================================
      implicit none
      dll_export get_logical_unit
       include 'const.inc'             ! Constant definitions
 
*+ Purpose
*      Return the next available logical unit number.  Will produce an err
*      if no more unit numbers are available and return a -1 (file not found
*      flsg).
 
*+  Definition
*     This function will return an available logical unit number
*     if there is one.  Otherwise it will flag a fatal error
*     and return -1.  
 
*+  Mission Statement
*      
 
*+ Changes
*      DPH 12/6/92
*     JNGH 3/8/94 - changed to use inquire  instead of array
*                    used file_not_found flag instead of -1
*       DPH 19/10/94 Removed routine name parameter in call to fatal_error
 
*+ Calls
      dll_import fatal_error
 
*+ Constant Values
       integer    unit_number_start    ! The first available unit number
       parameter (unit_number_start = 20)
*
       integer    unit_number_end      ! The last available unit number
       parameter (unit_number_end = 100)
 
*+ Local Variables
       character  error_string*110     ! err string
       logical    unit_number_unavail  ! Shows if unit number is available
       integer    unit_number          ! Unit number
 
*- Implementation Section ----------------------------------
 
      unit_number = unit_number_start
 
10    continue
         inquire (unit = unit_number, opened = unit_number_unavail)
         if (unit_number_unavail) then
            unit_number = unit_number + 1
 
            if (unit_number.gt.unit_number_end) then
               error_string =
     .         'All logical unit numbers are currently in use.' //
     .         new_line //
     .         'Increase the maximum logical unit number allowed.'
 
               call fatal_error (err_internal, error_string)
               unit_number = file_not_found
 
            else
               goto 10
 
            endif
 
         else
 
      endif
 
      get_logical_unit = unit_number
 
      return
      end
 
 
 


*     ===========================================================
      subroutine close_unit (logical_unit)
*     ===========================================================
      implicit none
      dll_export close_unit
 
*+ Sub-Program Arguments
      integer    logical_unit          ! logical unit to be closed
 
*+ Purpose
*       Closes file attached to specified logical unit.
 
*+  Definition
*     Closes logical unit "logical_unit".
 
*+  Mission Statement
*      
 
*+ Changes
*       190894 jngh specified and programmed
 
*+ Calls
 
*+ Local Variables
      integer    io_status             ! i/o status flag
 
*- Implementation Section ----------------------------------
 
            ! needs to have check for valid logical unit and check err
            ! status of close operation
 
      close (logical_unit, iostat = io_status)
 
      return
      end
 
 
 


* ====================================================================
       integer function open_file_for_read (file_name)
* ====================================================================
      implicit none
      dll_export open_file_for_read
       include 'const.inc'             ! Constant definitions
 
*+ Sub-Program Arguments
       character file_name*(*)         ! File name to open.
 
*+ Purpose
*      Open and rewind a file and return the logical unit number.
*      Return -1 (file_not_found flag) if file cannot be opened and
*      calls Fatal_error
 
*+  Definition
*     Attempts to open the file file_name using the first
*     available logical unit number.  On success, a message is
*     sent to the screen to that effect and the logical unit
*     number is returned.  If there is no logical unit number
*     available, the constant NO_LOGICAL_UNITS will be returned.
*     If the file cannot be opened, a fatal error is flagged and
*     the constant FILE_NOT_FOUND will be returned.  
 
*+  Mission Statement
*     the file unit number for %1
 
*+ Changes
*      DPH 17/6/92
*     JNGH 3/8/94 put in err messages from old open_f routine
*                    used file_not_found flag instead of -1
*                    removed err message re unavailable unit no. This is
*                    already in get_logical_unit
*       DPH 19/10/94 Removed routine name parameter in call to fatal_error
*     jngh 21/2/95 changed write to string to replacement statement.
*        DPH 25/10/95 Changed error_message size to function_string_len
*        dph 22/10/97 commented out the "file opened ok" message c-039
*        jngh 21/5/98 added inquires so file isn't opened more than once
 
*+ Calls
      dll_import push_routine
      dll_import get_logical_unit
      dll_import fatal_error
      dll_import pop_routine
       integer get_logical_unit        ! function
 
*+ Constant Values
       integer no_logical_units
       parameter (no_logical_units=0)
*
       integer ok                      ! Status indicating open was successful.
       parameter (ok = 0)
*
       character this_routine*(*)      ! Name of this routine
       parameter (this_routine='open_file_for_read')
 
*+ Local Variables
       character error_string*(function_string_len) ! err string
       integer iostatus                ! I/O status
       integer unit_number             ! Unit number of open file.
       logical opened                  ! status of file
 
*- Implementation Section ----------------------------------
 
      call push_routine (this_routine)
 
      if (file_name.eq.blank) then           ! is filename ok?
         open_file_for_read = file_not_found
 
         error_string = '  Cannot open file. No file name.'
         call fatal_error (err_user, error_string)
      else
            ! Is the file already open?
         inquire (file = file_name, opened = opened)
         if (opened) then
               ! Lets find out its unit number
            inquire (file = file_name, number = unit_number)
            open_file_for_read = unit_number
         else
               ! get a new unit number and try to open it
            unit_number = get_logical_unit ()
 
            if (unit_number.eq.file_not_found) then
               open_file_for_read = no_logical_units
 
            else
               open (unit = unit_number, file = file_name,
     :              action='READ', status='OLD', iostat=iostatus)
 
               if (iostatus.eq.ok) then
                               ! file has been opened.
 
                  rewind (unit_number)
                  open_file_for_read = unit_number
 
!                  Error_string = new_line
!      :                       // ' File opened ok. -> '
!      :                       // file_name
!                  call write_string (lu_scr_sum, Error_string)
 
               else
                  open_file_for_read = file_not_found
 
                  error_string = ' Cannot open file -> '// file_name
                  call fatal_error (err_user, error_string)
 
               endif
 
            endif
         endif
      endif
 
      call pop_routine (this_routine)
      return
      end
 
 
 


* ====================================================================
       integer function open_file (file_name)
* ====================================================================
      implicit none
      dll_export open_file
       include 'const.inc'             ! Constant definitions
 
*+ Sub-Program Arguments
       character file_name*(*)         ! File name to open.
 
*+ Purpose
*      Open and rewind a file and return the logical unit number.
*      Return -1 (file_not_found flag) if file cannot be opened and
*      calls Fatal_error
 
*+  Definition
*     Attempts to open the file file_name using the first
*     available logical unit number.  On success, a message is
*     sent to the screen to that effect and the logical unit
*     number is returned.  If there is no logical unit number
*     available, the constant NO_LOGICAL_UNITS will be returned.
*     If the file cannot be opened, a fatal error is flagged and
*     the constant FILE_NOT_FOUND will be returned.  
 
*+  Mission Statement
*     the file unit number for %1
 
*+ Changes
*      DPH 17/6/92
*     JNGH 3/8/94 put in err messages from old open_f routine
*                    used file_not_found flag instead of -1
*                    removed err message re unavailable unit no. This is
*                    already in get_logical_unit
*       DPH 19/10/94 Removed routine name parameter in call to fatal_error
*     jngh 21/2/95 changed write to string to replacement statement.
*        DPH 25/10/95 Changed error_message size to function_string_len
*        dph 22/10/97 commented out the "file opened ok" message c-039
*        jngh 21/5/98 added inquires so file isn't opened more than once
 
*+ Calls
      dll_import push_routine
      dll_import get_logical_unit
      dll_import fatal_error
      dll_import pop_routine
       integer get_logical_unit        ! function
 
*+ Constant Values
       integer no_logical_units
       parameter (no_logical_units=0)
*
       integer ok                      ! Status indicating open was successful.
       parameter (ok = 0)
*
       character this_routine*(*)      ! Name of this routine
       parameter (this_routine='Open_file')
 
*+ Local Variables
       character error_string*(function_string_len) ! err string
       integer iostatus                ! I/O status
       integer unit_number             ! Unit number of open file.
       logical opened                  ! status of file
 
*- Implementation Section ----------------------------------
 
      call push_routine (this_routine)
 
      if (file_name.eq.blank) then           ! is filename ok?
         open_file = file_not_found
 
         error_string = '  Cannot open file. No file name.'
         call fatal_error (err_user, error_string)
      else
            ! Is the file already open?
         inquire (file = file_name, opened = opened)
         if (opened) then
               ! Lets find out its unit number
            inquire (file = file_name, number = unit_number)
            open_file = unit_number
         else
               ! get a new unit number and try to open it
            unit_number = get_logical_unit ()
 
            if (unit_number.eq.file_not_found) then
               open_file = no_logical_units
 
            else
               open (unit = unit_number, file = file_name,
     :              status = 'OLD', iostat = iostatus)
 
               if (iostatus.eq.ok) then
                               ! file has been opened.
 
                  rewind (unit_number)
                  open_file = unit_number
 
!                  Error_string = new_line
!      :                       // ' File opened ok. -> '
!      :                       // file_name
!                  call write_string (lu_scr_sum, Error_string)
 
               else
                  open_file = file_not_found
 
                  error_string = ' Cannot open file -> '// file_name
                  call fatal_error (err_user, error_string)
 
               endif
 
            endif
         endif
      endif
 
      call pop_routine (this_routine)
      return
      end
 
 
 


* ====================================================================
       logical function read_init ()
* ====================================================================
      implicit none
      dll_export read_init
      include 'const.inc'
      include 'read.inc'
 
*+ Purpose
*      Initialise the read routines.  Called before every run.
 
*+  Mission Statement
*      
 
*+ Changes
*     sb 24/4/98  Created.
 
*+ Calls
 
*- Implementation Section ----------------------------------
 
      last_module_name = blank
      last_section_name = blank
      current_unit_num = -1
      section_suffix = blank
 
      read_init = .true.
      return
      end
 
 
 


* ====================================================================
       subroutine read_term()
* ====================================================================
      implicit none
      dll_export read_term
 
*+ Purpose
*     The system is about to close down.  Close all open unit numbers
 
*+  Mission Statement
*      
 
*+ Changes
*     DPH 16/02/95
*     SB 24/4/98   Changed name to read_term().
 
*+ Calls
      dll_import push_routine
      dll_import pop_routine
 
*+ Constant Values
       character  this_routine*(*)     ! Name of this routine
       parameter (this_routine='utility_close')
*
       integer    unit_number_start    ! The first available unit number
       parameter (unit_number_start = 20)
*
       integer    unit_number_end      ! The last available unit number
       parameter (unit_number_end = 100)
*
       integer lu_temp_file            ! Temporary file opened by engine
       parameter (lu_temp_file=20)     ! Don't ever close it.
 
*+ Local Variables
       logical    unit_number_open     ! Shows if unit number is open
       integer    unit_number          ! Unit number
 
*- Implementation Section ----------------------------------
 
      call push_routine(this_routine)
 
      do 10 unit_number = unit_number_start, unit_number_end
 
         ! If unit number is open - then close it.
 
         inquire (unit = unit_number, opened = unit_number_open)
 
         if (unit_number_open .and. unit_number .ne. lu_temp_file) then
            close(unit_number)
 
         else
            ! Unit number already closed.
         endif
10    continue
 
      call pop_routine(this_routine)
      return
      end
 
 
 


* ====================================================================
      character*(*) function read_file_string_optional
     :                       (unit_number, section_name, key_name)
* ====================================================================
      implicit none
      dll_export read_file_string_optional
       include   'const.inc'           ! Constant definitions
 
*+ Sub-Program Arguments
       character  key_name*(*)         ! (INPUT) Key name to search for
       character  section_name*(*)     ! (INPUT) name of section of file
                                       ! for search.
       integer    unit_number          ! (INPUT) Unit number of open file
 
*+ Purpose
*      Read a parameter string from a file, opened on Unit_number, within
*      a given section in that file, that matches Key_name.
 
*+ Assumptions
*      Assumes the file is open on Unit_number
 
*+ Notes
*      e.g. If the line read from the file is :- outfreq=daily
*      then outfreq is the key_name and daily is the parameter
*      returned by this function.  This routine will ignore any
*      lines starting in '!'.  These are considered comment lines
*
*      It is possible to search the entire file if section name is blank.
*      This is achieved using 'Entire_File' from const.inc.
 
*+  Mission Statement
*      
 
*+ Changes
*      NIH -16/06/94 Adapted from get_file_string
*   080994 jngh cleaned up
*     DPH - 8/11/94 Re-wrote to use lower level routines.
 
*+ Calls
      dll_import push_routine
      dll_import find_section_name
      dll_import read_key_from_section
      dll_import assign_string
      dll_import pop_routine
       character  read_key_from_section*(function_string_len)
                                       ! function
       logical find_section_name       ! function
 
*+ Constant Values
      character  myname*(*)            ! name of current procedure
      parameter (myname = 'read_file_string_optional')
*
      logical no_multiple_keys         ! Don't read multiple keys
      parameter (no_multiple_keys=.false.)
 
*+ Local Variables
       logical    found                ! Found the key name ?
       integer record_num              ! Record number in file
       character  return_string*(function_string_len)
                                       ! Parameter string read from file
 
*- Implementation Section ----------------------------------
 
      call push_routine (myname)
 
      found = find_section_name (unit_number, record_num, section_name)
 
      if (found) then
          return_string = read_key_from_section
     .        (unit_number, record_num, section_name,
     .         key_name, no_multiple_keys)
 
      else
         ! Cannot find the section name.
      endif
 
      if (found) then
         call assign_string (read_file_string_optional, return_string)
 
      else
         read_file_string_optional = blank
      endif
 
      call pop_routine (myname)
      return
      end
 
 
 


* ====================================================================
       logical function find_section_name
     .         (unit_number, record_num, section_name)
* ====================================================================
      implicit none
      dll_export find_section_name
      include 'const.inc'              ! constant definitions
 
*+ Sub-Program Arguments
      integer unit_number              ! (INPUT) Unit number to read from
      character section_name*(*)       ! (INPUT) Section name to search for
      integer record_num               ! (OUTPUT) Current record number in file
 
*+ Purpose
*     Go find the section name on the specified open logical unit number.
*     Return TRUE if found or FALSE otherwise.  The logical unit number
*     will be positioned to the line AFTER the section name if found.
 
*+  Definition
*     Attempts to find the section name "section" in the file
*     given by the open logical unit number "unit".  If it
*     succeeds, it positions the input to the beginning of the
*     line after the section name, sets "record" to that record
*     number (i.e. the line number of the line after the section
*     name), and returns .TRUE..  Otherwise it returns .FALSE.  
 
*+  Mission Statement
*      
 
*+ Changes
*     DPH 8/11/94
*     010495 jngh prevented search through file after 'weather' subsection
*                 encountered.
 
*+ Calls
      dll_import push_routine
      dll_import read_line
      dll_import get_section_name
      dll_import pop_routine
      character get_section_name*(function_string_len)
                                       ! function
 
*+ Constant Values
      integer ok_status                ! Read was ok
      parameter (ok_status=0)
*
      character routine_name*(*)       ! Name of this routine
      parameter (routine_name='Find_section_name')
 
*+ Local Variables
      character line*(function_string_len)
                                       ! Line read from file
      integer read_status              ! Status of read_line
      logical section_found            ! Have we found the section ?
      character section_name_read*(function_string_len)
 
*- Implementation Section ----------------------------------
 
      call push_routine(routine_name)
 
      rewind (unit_number)
      record_num = 1
 
      if (section_name .eq. no_section) then
         ! we have no section specified - start at beginning of file
 
         section_found = .true.
 
      else
10       continue
         call read_line (unit_number, record_num, line, read_status)
 
         if (read_status .eq. ok_status) then
            section_name_read = get_section_name (line)
            if (section_name_read .ne. section_name) then
               if (index (section_name_read, 'weather').gt.4) then
                     ! don't look past weather section
                  section_found = .false.
               else
                  goto 10
               endif
 
            else
               ! we have found the section we want
 
               section_found = .true.
            endif
         else
            ! we have trouble reading file - probably EOF
 
            section_found = .false.
         endif
      endif
 
      find_section_name = section_found
 
      call pop_routine(routine_name)
      return
      end
 
 
 


* ====================================================================
       character*(*) function read_key_low_level
     .    (unit_number, record_num,
     .     section_name, key_name, multiple_keys, wrap_around)
* ====================================================================
      implicit none
      dll_export read_key_low_level
      include 'const.inc'              ! constant definitions
      include 'read.inc'
 
*+ Sub-Program Arguments
      integer unit_number              ! (INPUT) Open unit number.
      integer record_num               ! (OUTPUT) Current record number in file
      character key_name*(*)           ! (INPUT) Key name to search for
      character section_name*(*)       ! (INPUT) Section name we're in.
      logical multiple_keys            ! (INPUT) Look for multiple keys ?
      logical wrap_around              ! (INPUT) Wrap around to top of section?
 
*+ Purpose
*     Search an entire section for the key_name.  If Multiple_keys
*     equals .true. then don't stop the search when the first key
*     is found.  Keep looking for all keys and append the results
*     to the return string.  If Wrap_around = .true. then wrap
*     around to the top of the section when the bottom of the section
*     is encountered.
 
*+  Mission Statement
*      
 
*+ Changes
*     DPH 8/11/94
*     DPH 21/04/95 Replaced "Line = Blank // Line" with
*                  "e_message = Blank // Line"
*     JNGH 22/06/96 changed function string_concat to call append_string
 
*+ Calls
      dll_import push_routine
      dll_import read_line
      dll_import get_section_name
      dll_import get_data_string
      dll_import append_string
      dll_import assign_string
      dll_import find_section_name
      dll_import pop_routine
      character get_data_string*(function_string_len)
                                       ! function
      character get_section_name*(function_string_len)
                                       ! function
      logical find_section_name        ! function
 
*+ Constant Values
      integer ok_status                ! Status of read if ok
      parameter (ok_status=0)
*
      character routine_name*(*)       ! Name of this routine
      parameter (routine_name='Read_key_low_level')
 
*+ Local Variables
      logical end_section              ! Have we reached end of section?
      logical found                    ! Have we found the key ?
      character line*(function_string_len)
                                       ! Line read from file
      integer read_status              ! Status from read_line
 
*- Implementation Section ----------------------------------
 
      call push_routine(routine_name)
 
      read_key_low_level = blank
 
20    continue
 
      if (searching_top_half .and.
     .    record_num .ge. start_record_num) then
         ! That's it - look no more.
 
         found = .false.
         end_section = .true.
 
      else
 
         call read_line (unit_number, record_num, line, read_status)
 
         if (read_status .eq. ok_status) then
            if (get_section_name (line) .eq. blank) then
               line = get_data_string (line, key_name)
               found = (line .ne. blank)
               end_section = .false.
            else
               ! We have reached the end of section
               found = .false.
               end_section = .true.
 
            endif
         else
            ! We have reached the end of file
            found = .false.
            end_section = .true.
         endif
      endif
 
      if (found) then
         if (multiple_keys) then
 
            e_message = blank // line
            line = e_message
 
            call append_string (read_key_low_level, line)
            goto 20
 
         else
            call assign_string (read_key_low_level, line)
         endif
 
      else if (end_section) then
         if (searching_top_half) then
            ! We've reached our starting point - look no further.
 
         else if (wrap_around) then
            ! We've reached the bottom of the section - rewind unit
            ! to start of section and keep looking.
 
            searching_top_half = .true.
            found = find_section_name
     .              (unit_number, record_num, section_name)
            goto 20
         endif
 
      else
         ! Keep looking
 
         goto 20
      endif
 
      call pop_routine(routine_name)
      return
      end
 
 
 


* ====================================================================
       character*(*) function read_key_from_section
     .    (unit_number, record_num,
     .     section_name, key_name, multiple_keys)
 
* ====================================================================
      implicit none
      dll_export read_key_from_section
      include 'const.inc'
      include 'read.inc'
 
*+ Sub-Program Arguments
      integer unit_number              ! (INPUT) Open unit number.
      integer record_num               ! (OUTPUT) Current record number in file
      character key_name*(*)           ! (INPUT) Key name to search for
      character section_name*(*)       ! (INPUT) Section name we're in.
      logical multiple_keys            ! (INPUT) Look for multiple keys ?
 
*+ Purpose
*     Search an entire section for the key_name.  If Multiple_keys
*     equals .true. then don't stop the search when the first key
*     is found.  Keep looking for all keys and append the results
*     to the return string.
 
*+ Assumptions
*      This routine does NOT search for the section name.  It is
*      assumed that the file is positioned inside the section
*      somewhere.
 
*+  Mission Statement
*      
 
*+ Changes
*     DPH 10/11/94
 
*+ Calls
      dll_import read_key_low_level
      character read_key_low_level*(function_string_len)
                                       ! function
 
*+ Constant Values
      logical do_wrap_around           ! Do wrap around section if necessary
      parameter (do_wrap_around=.true.)
 
*- Implementation Section ----------------------------------
 
      start_record_num = record_num
      searching_top_half = .false.
 
      read_key_from_section = read_key_low_level
     .   (unit_number, record_num, section_name, key_name,
     .    multiple_keys, do_wrap_around)
      return
      end
 
 
 


* ====================================================================
       character*(*) function read_next_key_from_section
     .    (unit_number, record_num,
     .     section_name, key_name, multiple_keys)
* ====================================================================
      implicit none
      dll_export read_next_key_from_section
      include 'const.inc'
 
*+ Sub-Program Arguments
      integer unit_number              ! (INPUT) Open unit number.
      integer record_num               ! (OUTPUT) Current record number in file
      character key_name*(*)           ! (INPUT) Key name to search for
      character section_name*(*)       ! (INPUT) Section name we're in.
      logical multiple_keys            ! (INPUT) Look for multiple keys ?
 
*+ Purpose
*     Search an entire section for the key_name.  If Multiple_keys
*     equals .true. then don't stop the search when the first key
*     is found.  Keep looking for all keys and append the results
*     to the return string.
 
*+ Assumptions
*      This routine does NOT search for the section name.  It is
*      assumed that the file is positioned inside the section
*      somewhere.
 
*+ Notes
*     This routine MUST ALWAYS be called AFTER the
*     read_key_from_section routine.
 
*+  Mission Statement
*      
 
*+ Changes
*     DPH 10/11/94
 
*+ Calls
      dll_import read_key_low_level
      character read_key_low_level*(function_string_len)
                                       ! function
 
*+ Constant Values
      logical dont_wrap_around         ! Dont wrap around section
      parameter (dont_wrap_around=.true.)
 
*- Implementation Section ----------------------------------
 
      read_next_key_from_section = read_key_low_level
     .   (unit_number, record_num, section_name, key_name,
     .    multiple_keys, dont_wrap_around)
 
      return
      end
 
 
 


* ====================================================================
       subroutine find_next_matching_section
     .    (current_module_name, section_spec, section_name,
     .     open_unit_num, record_num, exclude_file_num,
     .     exclude_section_name)
* ====================================================================
      implicit none
      dll_export find_next_matching_section
      include 'const.inc'              ! constant definitions
      include 'read.inc'
 
*+ Sub-Program Arguments
      character current_module_name*(*) ! (INPUT) Name of current module
      character section_spec*(*)       ! (INTPUT) Section specifier to look for.
      character section_name*(*)       ! (OUTPUT) Real section name found in
                                       ! file
      integer open_unit_num            ! (OUTPUT) Unit number of open file
      integer record_num               ! (OUTPUT) Current record number in file
      integer exclude_file_num         ! (INPUT) File number to exclude from
                                       ! search.
      character exclude_section_name*(*) ! (INPUT) Section name to exclude from
                                        ! search.
 
*+ Purpose
*      Find the next matching section from a supplied section specifier
*      and return the logical unit number for the file with the
*      section in it.  The file is
*      advanced to point to the line AFTER the specified section name.
*      A unit_num of -1 is returned if section not found.  Section_name
*      is updated to the matching section name if found.
*      If Exclude_file_num > 0 then that particular file will be
*      excluded from search.
 
*+  Mission Statement
*      
 
*+ Changes
*     DPH 8/11/94
*     JNGH 22/06/96 changed function string_concat to call append_string
*     SB 15/7/98 added line to set open_unit_num to -1 when closed.
 
*+ Calls
      dll_import get_file_section_list
      dll_import append_string
      dll_import get_control_file_section
      dll_import open_file_for_read
      dll_import find_section_name
      integer open_file_for_read                ! function
      logical find_section_name        ! function
      character get_control_file_section*(function_string_len)
                                       ! function
 
*+ Local Variables
      character file_list(max_files)*(max_file_name_size)
                                       ! List of files for current module
      integer num_files                ! Number of files specified for module
      character section_list(max_files)*(max_section_name_size)
                                       ! List of sections for current module
      logical section_found            ! Have we found the section yet?
 
*- Implementation Section ----------------------------------
 
      ! Get all files/sections for current module
 
      call get_file_section_list
     .    (current_module_name, file_list, section_list, num_files)
 
      ! Check to see if we've run out of sections to look in.
 
      if (search_section_index .ge. num_files) then
         ! No more sections left - exit
 
         open_unit_num = -1
 
      else
 
         ! Loop through each of the section names until one is found.
 
10       continue
         search_section_index = search_section_index + 1
 
         if (search_section_index .gt. num_files) then
            ! Cannot find section - exit
 
            open_unit_num = -1
 
         else
            ! If our section spec is blank then we don't need to append
            ! the section spec to our section name.
 
            section_name = section_list(search_section_index)
            if (section_spec .eq. blank) then
               ! Nothing to append to our search_section
 
            else
               ! Append the section name to the search section.
 
               call append_string (section_name, '.')
               call append_string (section_name, current_module_name)
               call append_string (section_name, '.')
               call append_string (section_name, section_spec)
            endif
 
            ! Check for a blank file name in file_list.  If found then
            ! read from either control file1 or control file 2 depending
            ! on whether the section name = current control file section.
 
            if (file_list(search_section_index) .eq. blank) then
               if (section_name .eq. get_control_file_section()) then
                  open_unit_num = lu_control_file
               else
                  open_unit_num = lu_control_file2
               endif
 
            else
               ! Treat the input module as a special case.  If the current
               ! module is input and the section found was 'weather' then
               ! skip to next file/section.
 
               if ((current_module_name .eq. 'input' .or.
     .           current_module_name .eq. 'hardinpt') .and.
     .           index(section_name, exclude_section_name) .gt. 0) then
                  goto 10
 
               else if (exclude_file_num .eq. search_section_index)then
                  goto 10
 
               else
                  ! Not special case - open file
                  open_unit_num = open_file_for_read
     .               (file_list(search_section_index))
               endif
 
            endif
 
            ! If nothing wrong with file then go find section
 
            if (open_unit_num .gt. 0) then
               record_num = 1
               section_found = find_section_name
     .                   (open_unit_num, record_num, section_name)
               if (section_found) then
                  ! Found section - exit routine.
 
                  current_param_file_name = file_list
     .                (search_section_index)
               else
                  ! Cannot find section - continue looking
 
!DPH                  E_message = 'Cannot find section : ' // Section_name
!DPH                  call Warning_error (ERR_user, E_Message)
 
 
                  if (open_unit_num .gt. lu_control_file2) then
                     close(open_unit_num)
                     open_unit_num = -1
 
                  else
                     ! Don't close control file - ever
 
                  endif
 
                  goto 10
               endif
 
            else
               ! Could'nt open file.  Fatal_error already been called.
               ! Simply exit.
 
               open_unit_num = -1
 
            endif
         endif
      endif
 
      return
      end
 
 
 


* ====================================================================
       subroutine read_next_param_line (module_name, line, flag)
* ====================================================================
      implicit none
      dll_export read_next_param_line
      include 'const.inc'
      include 'read.inc'
 
*+ Sub-Program Arguments
      character module_name*(*)    ! name of the module asking for info
      character line*(*)           ! line of file returned to the module
      integer   flag               ! flag for desired response
 
*+ Purpose
*   This routine is designed for consecutive call usage.  The routine is
*   to be iterated until all parameter section lines have been read.
*   The routine will read each line from each section in files specified
*   after the module name in the current control file APSIM run section.
*   When the entire search is finished a blank line is returned.
*   Flag values are to be 0 for normal iteration, 1 to restart search,
*   and 2 for user to end search prematurely.
 
*+  Mission Statement
*      
 
*+ Changes
*   NeilH - 23-11-1994 - Programmed and Specified
*   DPH 11/4/96        - removed LUN variable and used current_unit_num
*                        from common block instead
*   SB 15/7/98  Added line to set current unit num to -1 when closed.
 
*+ Calls
      dll_import push_routine
      dll_import assign_string
      dll_import fill_char_array
      dll_import get_file_section_list
      dll_import close_unit
      dll_import open_file_section
      dll_import read_param_line
      dll_import fatal_error
      dll_import pop_routine
      integer open_file_section   ! function
 
*+ Constant Values
      integer end_flag
      parameter (end_flag = 2)
*
      integer iterate_flag
      parameter (iterate_flag = 0)
*
      character*(*) myname               ! name of current procedure
      parameter (myname = 'Read_next_param_line')
*
      integer start_flag
      parameter (start_flag = 1)
 
*+ Local Variables
      integer   current_fileno    ! index of current param file in
                                  ! file_list
      character file_list(max_files)*(max_file_name_size)
                                   ! List of files for current module
      character last_module*(max_module_name_size)
                                   ! name of the module that last called
                                   ! this routine
      integer num_files            ! No. of files specified for module
      character section_list(max_files)*(max_section_name_size)
                                   ! List of sections for current module
 
*+ Initial Data Values
      save file_list
      save section_list
      save current_fileno
      save num_files
      save last_module
*
      data last_module /blank/
 
*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      ! --------------------------------------------------------------
      ! If we are starting a new series of searches better set it all
      ! up first. Get file/section list and position pointer to start.
      ! --------------------------------------------------------------
 
      if ((module_name .ne. last_module).or.(flag.eq.start_flag)) then
 
         ! we have a different module using this now - get rid of all
         ! of our working from last time
 
         call assign_string (last_module, module_name)
         call fill_char_array (file_list, blank, max_files)
         call fill_char_array (section_list, blank, max_files)
 
         call get_file_section_list (module_name, file_list,
     :               section_list, num_files)
 
         current_fileno = 1
 
         ! close off current unit number if necessary.
 
         if (current_unit_num .gt. lu_control_file2) then
            call close_unit(current_unit_num)
            current_unit_num = -1
         endif
 
10       continue
         if (num_files.gt.0 .and. current_fileno .le. num_files) then
 
            current_unit_num = open_file_section (
     :                               file_list(current_fileno),
     :                               section_list(current_fileno))
            if (current_unit_num .gt. 0) then
                  ! we have started the search ok - go to it!!
 
               call read_param_line (current_unit_num, file_list,
     :                               section_list,
     :                               num_files, current_fileno, line)
 
*DPH               write(msg, '(50a)' )
*DPH     .            'File opened ok -> ',
*DPH     .            File_list(Current_FileNo)
*DPH               call Write_string(LU_Scr_sum, msg)
 
            else
               current_fileno = current_fileno + 1
               goto 10
            endif
 
         else
            line = blank
         endif
 
      else if (flag.eq.iterate_flag) then
 
         call read_param_line (current_unit_num, file_list,
     :                         section_list,
     :                         num_files, current_fileno, line)
 
      else if (flag.eq.end_flag) then
         line = blank
 
      else
         call fatal_error(err_internal, 'Bad flag value')
 
      endif
 
      if (line.eq.blank) then
         ! We have reached the end of the search - reset the last module flag
         ! so that we know to start from scratch next time
 
         last_module = blank
 
      else
      endif
 
      call pop_routine (myname)
      return
      end
 
 
 


* ====================================================================
       subroutine read_param_line (lun, file_list, section_list,
     :                  num_files, current_fileno, line)
* ====================================================================
      implicit none
      dll_export read_param_line
       include 'const.inc'
       include 'read.inc'
 
*+ Sub-Program Arguments
       integer current_fileno
       character file_list(*)*(*)
       character line*(*)
       integer lun
       integer num_files
       character section_list(*)*(*)
 
*+ Purpose
*     <insert here>
 
*+ Assumptions
*      Assume that LUN is the opened File_List(current_fileno).
 
*+  Mission Statement
*      
 
*+ Changes
*   NeilH - 23-11-1994 - Programmed and Specified
*   270295 jngh initialised dummy.
*   15-7-98  SB set LUN to -1 after closing.
 
*+ Calls
      dll_import push_routine
      dll_import read_line
      dll_import get_section_name
      dll_import close_unit
      dll_import open_file_section
      dll_import pop_routine
       integer open_file_section
       character get_section_name*(max_section_name_size)
 
*+ Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'Read_Param_Line')
 
*+ Local Variables
       integer dummy
       integer iostatus
       character section_name*(max_section_name_size)
 
*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      dummy = 0
  100 continue
      call read_line (lun, dummy, line, iostatus)
      section_name = get_section_name (line)
 
      if ((iostatus.ne.0).or.(section_name.ne.blank)) then
 
         if (lun .ne. lu_control_file2) then
            call close_unit (lun)
            lun = -1
         endif
 
10       continue
         if (current_fileno.lt.num_files) then
 
           current_fileno = current_fileno + 1
           lun = open_file_section (
     :                    file_list(current_fileno),
     :                    section_list(current_fileno))
           if (lun.gt.0) then
              goto 100
           else
              goto 10
           endif
 
         else
            ! search is finished - send blank string
            line = blank
 
         endif
 
      else
 
      endif
 
      call pop_routine (myname)
      return
      end
 
 
 


* ====================================================================
       integer function open_file_section (file,section)
* ====================================================================
      implicit none
      dll_export open_file_section
      include 'const.inc'
      include 'read.inc'
 
*+ Sub-Program Arguments
      character file*(*)
      character section*(*)
 
*+ Purpose
*     <insert here>
 
*+  Mission Statement
*      
 
*+ Changes
*   NeilH - 23-11-1994 - Programmed and Specified
*   DPH - 13/03/95 - Added code to append a suffix to section if one has
*                    been specified.
*   jngh 24/3/95 added module name to section name
*   DPH 5/7/95  added code to make sure we don't close lu_control_file2 and
*               to open lu_control_file2 when a blank file is specified.
*     JNGH 22/06/96 changed function string_concat to call append_string
 
*+ Calls
      dll_import push_routine
      dll_import get_current_module
      dll_import open_file_for_read
      dll_import assign_string
      dll_import append_string
      dll_import find_section_name
      dll_import close_unit
      dll_import fatal_error
      dll_import pop_routine
      integer open_file_for_read
      logical find_section_name
 
*+ Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'Open_File_Section')
 
*+ Local Variables
      integer dummy
      integer lun
      logical section_found
      character section_name*100       ! Section to search for
      character current_module_name*(max_module_name_size)
                                       ! Name of current module
 
*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      call get_current_module(current_module_name)
 
      if (file .eq. ' ') then
         lun = lu_control_file2
 
      else
         lun = open_file_for_read (file)
      endif
 
      if (lun .gt. 0) then
 
          if (section_suffix .eq. blank) then
             call assign_string (section_name, section)
          else
             call assign_string (section_name, section)
             call append_string (section_name, '.')
             call append_string
     .             (section_name, current_module_name)
             call append_string (section_name, '.')
             call append_string (section_name, section_suffix)
          endif
 
          section_found = find_section_name (lun, dummy,section_name)
 
          if (section_found) then
             ! Got it - exit routine
 
          else if (section_suffix .eq. blank) then
*DPH             e_message =
*DPH     :            'Cannot find section '//trim(Section)
*DPH     :          //' in file:- '//trim(File)
*DPH             call Fatal_error (Err_Internal, e_message)
             if (lun .ne. lu_control_file2) then
                call close_unit (lun)
             endif
             lun = 0
 
          else
             if (lun .ne. lu_control_file2) then
                call close_unit(lun)
             endif
             lun = 0
          endif
 
       else
          e_message = 'Cannot open file:- '//trim(file)
          call fatal_error (err_internal, e_message)
       endif
 
      open_file_section = lun
 
      call pop_routine (myname)
      return
      end
 
 
 
