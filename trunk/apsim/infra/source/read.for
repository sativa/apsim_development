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
      character Section_name*(*)       ! (INPUT) section name to search for
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      character Units*(*)              ! (INPUT) Units required by caller
      real Variable                    ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned
      real Lower_limit                 ! (INPUT) Lower limit for bounds check
      real Upper_limit                 ! (INPUT) Upper limit for bounds check
 
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
      character Read_param*(Function_string_len)
                                       ! function
 
*+ Constant Values
      logical No_multiple_keys         ! Don't look for multiple keys
      parameter (No_multiple_keys=.false.)
 
*+ Local Variables
      character Return_string*(Function_string_len)
                                       ! String returned from read_file_string
      character return_units*50        ! units of returned value
 
*- Implementation Section ----------------------------------
 
      Return_string = Read_param
     .   (Section_name, Variable_name, No_multiple_keys)
 
      if (Return_string .ne. Blank) then
         call split_off_units (return_string, return_units)
         call String_to_real_var
     .      (Return_string, Variable, numvals)
         call Bound_check_real_var
     .      (Variable, Lower_limit, Upper_limit, Variable_name)
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
      character Section_name*(*)       ! (INPUT) section name to search for
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      character Units*(*)              ! (INPUT) Units required by caller
      integer Variable                    ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned
      integer Lower_limit                 ! (INPUT) Lower limit for bounds check
      integer Upper_limit                 ! (INPUT) Upper limit for bounds check
 
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
      character Read_param*(Function_string_len)
                                       ! function
 
*+ Constant Values
      logical No_multiple_keys         ! Don't look for multiple keys
      parameter (No_multiple_keys=.false.)
 
*+ Local Variables
      character Return_string*(Function_string_len)
                                       ! String returned from read_file_string
      character return_units*50        ! units of returned value
 
*- Implementation Section ----------------------------------
 
      Return_string = Read_param
     .   (Section_name, Variable_name, No_multiple_keys)
 
      if (Return_string .ne. Blank) then
         call split_off_units (return_string, return_units)
         call String_to_integer_var
     .      (Return_string, Variable, numvals)
         call Bound_check_integer_var
     .      (Variable, Lower_limit, Upper_limit, Variable_name)
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
      character Section_name*(*)       ! (INPUT) section name to search for
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      character Units*(*)              ! (INPUT) Units required by caller
      double precision Variable        ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned
      double precision Lower_limit     ! (INPUT) Lower limit for bounds check
      double precision Upper_limit     ! (INPUT) Upper limit for bounds check
 
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
      character Read_param*(Function_string_len)
                                       ! function
 
*+ Constant Values
      logical No_multiple_keys         ! Don't look for multiple keys
      parameter (No_multiple_keys=.false.)
 
*+ Local Variables
      character Return_string*(Function_string_len)
                                       ! String returned from read_file_string
      character return_units*50        ! units of returned value
 
*- Implementation Section ----------------------------------
 
      Return_string = Read_param
     .   (Section_name, Variable_name, No_multiple_keys)
 
      if (Return_string .ne. Blank) then
         call split_off_units (return_string, return_units)
 
         call String_to_double_var
     .      (Return_string, Variable, numvals)
 
         call Bound_check_double_var
     .      (Variable, Lower_limit, Upper_limit, Variable_name)
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
      character Section_name*(*)       ! (INPUT) section name to search for
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      character Units*(*)              ! (INPUT) Units required by caller
      logical Variable                    ! (OUTPUT) Variable returned to caller
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
      character Read_param*(Function_string_len)
                                       ! function
 
*+ Constant Values
      logical No_multiple_keys         ! Don't look for multiple keys
      parameter (No_multiple_keys=.false.)
 
*+ Local Variables
      character Return_string*(Function_string_len)
                                       ! String returned from read_file_string
      character return_units*50        ! units of returned value
 
*- Implementation Section ----------------------------------
 
      Return_string = Read_param
     .   (Section_name, Variable_name, No_multiple_keys)
 
      if (Return_string .ne. Blank) then
         call split_off_units (return_string, return_units)
         call String_to_logical_var
     .      (Return_string, Variable, numvals)
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
      character Section_name*(*)       ! (INPUT) section name to search for
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      character Units*(*)              ! (INPUT) Units required by caller
      character Variable*(*)           ! (OUTPUT) Variable returned to caller
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
      character Read_param*(Function_string_len)
                                       ! function
 
*+ Constant Values
      logical No_multiple_keys         ! Don't look for multiple keys
      parameter (No_multiple_keys=.false.)
 
*+ Local Variables
      character Return_string*(Function_string_len)
                                       ! String returned from read_file_string
 
*- Implementation Section ----------------------------------
 
      Return_string = Read_param
     .   (Section_name, Variable_name, No_multiple_keys)
      Variable = Return_string
      if (Return_string .eq. Blank) then
         Numvals = 0
 
      else
         Numvals = 1
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
      character Section_name*(*)       ! (INPUT) section name to search for
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      character Units*(*)              ! (INPUT) Units required by caller
      real Variable                    ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned
      real Lower_limit                 ! (INPUT) Lower limit for bounds check
      real Upper_limit                 ! (INPUT) Upper limit for bounds check
 
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
      character Read_param_optional*(Function_string_len)
                                       ! function
 
*+ Constant Values
      logical No_multiple_keys         ! Don't look for multiple keys
      parameter (No_multiple_keys=.false.)
 
*+ Local Variables
      character Return_string*(Function_string_len)
                                       ! String returned from read_file_string
      character Return_units*50        ! Units found on string
 
*- Implementation Section ----------------------------------
 
      Return_string = Read_param_optional
     .   (Section_name, Variable_name, No_multiple_keys)
 
      call split_off_units (return_string, return_units)
 
      if (Return_string .eq. Blank) then
         variable = 0.0
         Numvals = 0
 
      else
         call String_to_real_var
     .      (Return_string, Variable, numvals)
         call Bound_check_real_var
     .     (Variable, Lower_limit, Upper_limit, Variable_name)
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
      character Section_name*(*)       ! (INPUT) section name to search for
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      character Units*(*)              ! (INPUT) Units required by caller
      integer Variable                    ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned
      integer Lower_limit                 ! (INPUT) Lower limit for bounds check
      integer Upper_limit                 ! (INPUT) Upper limit for bounds check
 
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
      character Read_param_optional*(Function_string_len)
                                       ! function
 
*+ Constant Values
      logical No_multiple_keys         ! Don't look for multiple keys
      parameter (No_multiple_keys=.false.)
 
*+ Local Variables
      character Return_string*(Function_string_len)
                                       ! String returned from read_file_string
      character return_units*50        ! units of returned value
 
*- Implementation Section ----------------------------------
 
      Return_string = Read_param_optional
     .   (Section_name, Variable_name, No_multiple_keys)
 
      call split_off_units (return_string, return_units)
      if (Return_string .eq. Blank) then
         variable = 0
         Numvals = 0
 
      else
         call String_to_integer_var
     .      (Return_string, Variable, numvals)
         call Bound_check_integer_var
     .      (Variable, Lower_limit, Upper_limit, Variable_name)
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
      character Section_name*(*)       ! (INPUT) section name to search for
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      character Units*(*)              ! (INPUT) Units required by caller
      double precision Variable        ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned
      double precision Lower_limit     ! (INPUT) Lower limit for bounds check
      double precision Upper_limit     ! (INPUT) Upper limit for bounds check
 
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
      character Read_param_optional*(Function_string_len)
                                       ! function
 
*+ Constant Values
      logical No_multiple_keys         ! Don't look for multiple keys
      parameter (No_multiple_keys=.false.)
 
*+ Local Variables
      character Return_string*(Function_string_len)
                                       ! String returned from read_file_string
      character return_units*50        ! units of returned value
 
*- Implementation Section ----------------------------------
 
      Return_string = Read_param_optional
     .   (Section_name, Variable_name, No_multiple_keys)
 
      call split_off_units (return_string, return_units)
      if (Return_string .eq. Blank) then
         variable = 0.0d0
         Numvals = 0
 
      else
         call String_to_double_var
     .      (Return_string, Variable, numvals)
 
         call Bound_check_double_var
     .      (Variable, Lower_limit, Upper_limit, Variable_name)
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
      character Section_name*(*)       ! (INPUT) section name to search for
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      character Units*(*)              ! (INPUT) Units required by caller
      logical Variable                    ! (OUTPUT) Variable returned to caller
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
      character Read_param_optional*(Function_string_len)
                                       ! function
 
*+ Constant Values
      logical No_multiple_keys         ! Don't look for multiple keys
      parameter (No_multiple_keys=.false.)
 
*+ Local Variables
      character Return_string*(Function_string_len)
                                       ! String returned from read_file_string
      character return_units*50        ! units of returned value
 
*- Implementation Section ----------------------------------
 
      Return_string = Read_param_optional
     .   (Section_name, Variable_name, No_multiple_keys)
      call split_off_units (return_string, return_units)
      if (Return_string .eq. Blank) then
         variable = .false.
         Numvals = 0
 
      else
         call String_to_logical_var
     .      (Return_string, Variable, numvals)
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
      character Section_name*(*)       ! (INPUT) section name to search for
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      character Units*(*)              ! (INPUT) Units required by caller
      character Variable*(*)           ! (OUTPUT) Variable returned to caller
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
      character Read_param_optional*(Function_string_len)
                                       ! function
 
*+ Constant Values
      logical No_multiple_keys         ! Don't look for multiple keys
      parameter (No_multiple_keys=.false.)
 
*+ Local Variables
      character Return_string*(Function_string_len)
                                       ! String returned from read_file_string
      character Return_units*50        ! Units found on string
 
*- Implementation Section ----------------------------------
 
      Return_string = Read_param_optional
     .   (Section_name, Variable_name, No_multiple_keys)
      call Split_off_units(Return_string, Return_units)
 
      Variable = Return_string
      if (Return_string .eq. Blank) then
         Numvals = 0
 
      else
         Numvals = 1
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
      character Section_name*(*)       ! (INPUT) section name to search for
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      integer size_of                  ! (INPUT) size_of of array
      character Units*(*)              ! (INPUT) Units required by caller
      real Variable(*)                 ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned
      real Lower_limit                 ! (INPUT) Lower limit for bounds check
      real Upper_limit                 ! (INPUT) Upper limit for bounds check
 
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
      character Read_param*(Function_string_len)
                                       ! function
 
*+ Constant Values
      logical No_multiple_keys         ! Don't look for multiple keys
      parameter (No_multiple_keys=.false.)
 
*+ Local Variables
      character Return_string*(Function_string_len)
                                       ! String returned from read_file_string
      character return_units*50        ! units of returned value
 
*- Implementation Section ----------------------------------
 
      Return_string = Read_param
     .   (Section_name, Variable_name, No_multiple_keys)
 
      if (Return_string .ne. Blank) then
         call split_off_units (return_string, return_units)
         call String_to_real_array
     .      (Return_string, Variable, size_of, numvals)
         call Bound_check_real_array
     .    (Variable, Lower_limit, Upper_limit, Variable_name, Numvals)
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
      character Section_name*(*)       ! (INPUT) section name to search for
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      integer size_of                  ! (INPUT) size_of of array
      character Units*(*)              ! (INPUT) Units required by caller
      integer Variable(*)                 ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned
      integer Lower_limit                 ! (INPUT) Lower limit for bounds check
      integer Upper_limit                 ! (INPUT) Upper limit for bounds check
 
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
      character Read_param*(Function_string_len)
                                       ! function
 
*+ Constant Values
      logical No_multiple_keys         ! Don't look for multiple keys
      parameter (No_multiple_keys=.false.)
 
*+ Local Variables
      character Return_string*(Function_string_len)
                                       ! String returned from read_file_string
      character return_units*50        ! units of returned value
 
*- Implementation Section ----------------------------------
 
      Return_string = Read_param
     .   (Section_name, Variable_name, No_multiple_keys)
 
      if (Return_string .ne. Blank) then
         call split_off_units (return_string, return_units)
         call String_to_integer_array
     .      (Return_string, Variable, size_of, numvals)
         call Bound_check_integer_array
     .    (Variable, Lower_limit, Upper_limit, Variable_name, Numvals)
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
      character Section_name*(*)       ! (INPUT) section name to search for
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      integer size_of                  ! (INPUT) size_of of array
      character Units*(*)              ! (INPUT) Units required by caller
      double precision Variable(*)     ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned
      double precision Lower_limit     ! (INPUT) Lower limit for bounds check
      double precision Upper_limit     ! (INPUT) Upper limit for bounds check
 
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
      character Read_param*(Function_string_len)
                                       ! function
 
*+ Constant Values
      logical No_multiple_keys         ! Don't look for multiple keys
      parameter (No_multiple_keys=.false.)
 
*+ Local Variables
      character Return_string*(Function_string_len)
                                       ! String returned from read_file_string
      character return_units*50        ! units of returned value
 
*- Implementation Section ----------------------------------
 
      Return_string = Read_param
     .   (Section_name, Variable_name, No_multiple_keys)
 
      if (Return_string .ne. Blank) then
         call split_off_units (return_string, return_units)
         call String_to_double_array
     .      (Return_string, Variable, size_of, numvals)
         call Bound_check_double_array
     .     (Variable, Lower_limit, Upper_limit, Variable_name, Numvals)
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
      character Section_name*(*)       ! (INPUT) section name to search for
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      integer size_of                  ! (INPUT) size_of of array
      character Units*(*)              ! (INPUT) Units required by caller
      logical Variable(*)                 ! (OUTPUT) Variable returned to caller
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
      character Read_param*(Function_string_len)
                                       ! function
 
*+ Constant Values
      logical No_multiple_keys         ! Don't look for multiple keys
      parameter (No_multiple_keys=.false.)
 
*+ Local Variables
      character Return_string*(Function_string_len)
                                       ! String returned from read_file_string
      character return_units*50        ! units of returned value
 
*- Implementation Section ----------------------------------
 
      Return_string = Read_param
     .   (Section_name, Variable_name, No_multiple_keys)
 
      if (Return_string .ne. Blank) then
         call split_off_units (return_string, return_units)
         call String_to_logical_array
     .      (Return_string, Variable, size_of, numvals)
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
      character Section_name*(*)       ! (INPUT) section name to search for
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      integer size_of                  ! (INPUT) size_of of array
      character Units*(*)              ! (INPUT) Units required by caller
      character Variable(*)*(*)        ! (OUTPUT) Variable returned to caller
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
      character Read_param*(Function_string_len)
                                       ! function
 
*+ Constant Values
      logical No_multiple_keys         ! Don't look for multiple keys
      parameter (No_multiple_keys=.false.)
 
*+ Local Variables
      character Return_string*(Function_string_len)
                                       ! String returned from read_file_string
      character return_units*50        ! units of returned value
 
*- Implementation Section ----------------------------------
 
      Return_string = Read_param
     .   (Section_name, Variable_name, No_multiple_keys)
 
      if (Return_string .ne. Blank) then
         call Split_off_units (Return_string, Return_units)
         call String_to_char_array
     .      (Return_string, Variable, size_of, numvals)
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
      character Section_name*(*)       ! (INPUT) section name to search for
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      integer size_of                  ! (INPUT) size_of of array
      character Units*(*)              ! (INPUT) Units required by caller
      real Variable(*)                 ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned
      real Lower_limit                 ! (INPUT) Lower limit for bounds check
      real Upper_limit                 ! (INPUT) Upper limit for bounds check
 
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
      character Read_param_optional*(Function_string_len)
                                       ! function
 
*+ Constant Values
      logical No_multiple_keys         ! Don't look for multiple keys
      parameter (No_multiple_keys=.false.)
 
*+ Local Variables
      character Return_string*(Function_string_len)
                                       ! String returned from read_file_string
      character return_units*50        ! units of returned value
 
*- Implementation Section ----------------------------------
 
      Return_string = Read_param_optional
     .   (Section_name, Variable_name, No_multiple_keys)
 
      call split_off_units (return_string, return_units)
      if (Return_string .eq. Blank) then
         call Fill_real_array(variable, 0.0, size_of)
         Numvals = 0
 
      else
         call String_to_real_array
     .     (Return_string, Variable, size_of, numvals)
         call Bound_check_real_array
     .      (Variable, Lower_limit, Upper_limit, Variable_name, Numvals)
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
      character Section_name*(*)       ! (INPUT) section name to search for
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      integer size_of                  ! (INPUT) size_of of array
      character Units*(*)              ! (INPUT) Units required by caller
      integer Variable(*)                 ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned
      integer Lower_limit                 ! (INPUT) Lower limit for bounds check
      integer Upper_limit                 ! (INPUT) Upper limit for bounds check
 
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
      character Read_param_optional*(Function_string_len)
                                       ! function
 
*+ Constant Values
      logical No_multiple_keys         ! Don't look for multiple keys
      parameter (No_multiple_keys=.false.)
 
*+ Local Variables
      character Return_string*(Function_string_len)
                                       ! String returned from read_file_string
      character return_units*50        ! units of returned value
 
*- Implementation Section ----------------------------------
 
      Return_string = Read_param_optional
     .   (Section_name, Variable_name, No_multiple_keys)
 
      call split_off_units (return_string, return_units)
      if (Return_string .eq. Blank) then
         call Fill_integer_array(variable, 0, size_of)
         Numvals = 0
 
      else
         call String_to_integer_array
     .      (Return_string, Variable, size_of, numvals)
         call Bound_check_integer_array
     .      (Variable, Lower_limit, Upper_limit, Variable_name, Numvals)
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
      character Section_name*(*)       ! (INPUT) section name to search for
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      integer size_of                  ! (INPUT) size_of of array
      character Units*(*)              ! (INPUT) Units required by caller
      double precision Variable(*)     ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned
      double precision Lower_limit     ! (INPUT) Lower limit for bounds check
      double precision Upper_limit     ! (INPUT) Upper limit for bounds check
 
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
      character Read_param_optional*(Function_string_len)
                                       ! function
 
*+ Constant Values
      logical No_multiple_keys         ! Don't look for multiple keys
      parameter (No_multiple_keys=.false.)
 
*+ Local Variables
      character Return_string*(Function_string_len)
                                       ! String returned from read_file_string
      character return_units*50        ! units of returned value
 
*- Implementation Section ----------------------------------
 
      Return_string = Read_param_optional
     .   (Section_name, Variable_name, No_multiple_keys)
      call split_off_units (return_string, return_units)
      if (Return_string .eq. Blank) then
         call Fill_double_array(variable, 0.0d0, size_of)
         Numvals = 0
 
      else
         call String_to_double_array
     .      (Return_string, Variable, size_of, numvals)
         call Bound_check_double_array
     .      (Variable, Lower_limit, Upper_limit, Variable_name, Numvals)
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
      character Section_name*(*)       ! (INPUT) section name to search for
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      integer size_of                  ! (INPUT) size_of of array
      character Units*(*)              ! (INPUT) Units required by caller
      logical Variable(*)                 ! (OUTPUT) Variable returned to caller
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
      character Read_param_optional*(Function_string_len)
                                       ! function
 
*+ Constant Values
      logical No_multiple_keys         ! Don't look for multiple keys
      parameter (No_multiple_keys=.false.)
 
*+ Local Variables
      character Return_string*(Function_string_len)
                                       ! String returned from read_file_string
      character return_units*50        ! units of returned value
 
*- Implementation Section ----------------------------------
 
      Return_string = Read_param_optional
     .   (Section_name, Variable_name, No_multiple_keys)
 
      call split_off_units (return_string, return_units)
      if (Return_string .eq. Blank) then
         call Fill_logical_array(variable, .false., size_of)
         Numvals = 0
 
      else
         call String_to_logical_array
     .      (Return_string, Variable, size_of, numvals)
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
      character Section_name*(*)       ! (INPUT) section name to search for
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      integer size_of                  ! (INPUT) size_of of array
      character Units*(*)              ! (INPUT) Units required by caller
      character Variable(*)*(*)        ! (OUTPUT) Variable returned to caller
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
      character Read_param_optional*(Function_string_len)
                                       ! function
 
*+ Constant Values
      logical No_multiple_keys         ! Don't look for multiple keys
      parameter (No_multiple_keys=.false.)
 
*+ Local Variables
      character Return_string*(Function_string_len)
                                       ! String returned from read_file_string
      character return_units*50        ! units of returned value
 
*- Implementation Section ----------------------------------
 
      Return_string = Read_param_optional
     .   (Section_name, Variable_name, No_multiple_keys)
      if (Return_string .eq. Blank) then
         call Fill_char_array(variable, Blank, size_of)
         Numvals = 0
 
      else
         call Split_off_units (Return_string, Return_units)
         call String_to_char_array
     .      (Return_string, Variable, size_of, numvals)
      endif
 
      return
      end
 
 
 
* ====================================================================
       character*(*) function Get_data_string (Char_string, Key_name)
* ====================================================================
      implicit none
      dll_export get_data_string
       include 'const.inc'             ! Constant definitions
 
*+ Sub-Program Arguments
       character Char_string*(*)       ! (INPUT) Character string to look in
       character Key_name*(*)          ! (INPUT) Key name to search for
 
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
      dll_import no_leading_spaces
       character Lower_case*30         ! function
       character No_spaces*30          ! function
       character No_leading_spaces*(Function_string_len)
                                       ! function
 
*+ Local Variables
       character Char_string_lower
     .    *(Function_string_len)       ! Lower case version of char_string
       character Key*30                ! Key pulled apart from key_name_lower
       character Key_name_lower*30     ! Lower case version of key name
       character Parameters
     .    *(Function_string_len)       ! Parameters to right of '=' sign
 
*- Implementation Section ----------------------------------
 
      Key_name_lower = Lower_case (Key_name)
      Key_name_lower = No_spaces (Key_name_lower)
 
cjh      shouldn't need this lower case conversion
cjh      call assign_string (char_string_lower, lower_case (char_string))
      call assign_string (Char_string_lower, Char_string)
 
10    continue
      call Get_next_variable (Char_string_lower, Key, Parameters)
 
      if (Key .eq. Key_name_lower) then
         call assign_string (Get_data_string
     :                      , No_leading_spaces (Parameters))
 
      else if (Key .eq. Blank) then
         Get_data_string = Blank
 
      else
         goto 10
 
      endif
 
      return
      end
 
 
 
* ====================================================================
       subroutine Get_next_variable (Variables_str,
     .            Var_name, Values_str)
* ====================================================================
      implicit none
      dll_export get_next_variable
       include 'const.inc'             ! Constant definitions
 
*+ Sub-Program Arguments
       character Variables_str*(*)     ! (INPUT & OUTPUT) String to break up
       character Var_name*(*)          ! (OUTPUT) Extracted variable name
       character Values_str*(*)        ! (OUTPUT) Extracted values string
 
*+ Purpose
*      Returns the next variable from Variables_str and its
*      associated data.
 
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
      dll_import no_leading_spaces
       character No_spaces*(Function_string_len)
                                       ! function
       character No_leading_spaces*(Function_string_len)
                                       ! function
 
*+ Constant Values
      character Delimiter*(*)          ! Delimiter to use to separate variables.
      parameter (Delimiter=',')
*
       character Equals*(*)            ! Equals sign
       parameter (Equals='=')
 
*+ Local Variables
       integer Pos                     ! Position in variables_str
       character String_right
     .    *(Function_string_len)       ! String to right of equals sign
 
*- Implementation Section ----------------------------------
 
      call Split_line (Variables_str, Var_name, String_right, Equals)
      call assign_string (Variables_str, String_right)
 
      Pos = index (Variables_str, Delimiter)
 
      ! Handle the situation when no MES_delimiter is in Variables_str.
 
      if (Pos .eq. 0) then
         call assign_string (Values_str, Variables_str)
         Variables_str = Blank
 
      else
         call assign_string (Values_str, Variables_str (1:Pos-1))
         Variables_str = Variables_str (Pos+1:)
      endif
 
      Var_name = No_spaces(Var_name)
      Values_str = No_leading_spaces(Values_str)
 
      return
      end
 
 
 
* ====================================================================
       subroutine Read_line (Logical_unit, Record_num, Line, IOStatus)
* ====================================================================
      implicit none
      dll_export read_line
       include 'const.inc'             ! constant definitions
 
*+ Sub-Program Arguments
       character Line*(*)              ! (OUTPUT) Line read from file
       integer Logical_unit            ! (INPUT) Logical unit to read from
       integer Record_num              ! (OUTPUT) Record number in file
       integer IOStatus                ! (OUTPUT) Status of read
 
*+ Purpose
*      Read in the next uncommented, non blank line from the control file.
*      Return an I/O status (0=Ok, 1=End_of_runblock, 2=End_of_file).
*      The line returned is converted to lowercase.
 
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
      dll_import no_leading_spaces
       character Lower_case
     .    *(Function_string_len)       ! function
       character No_leading_spaces     ! function
     .    *(Function_string_len)       ! function
 
*+ Constant Values
       character Comment*(*)           ! Comment specifier
       parameter (Comment='!')
*
       integer TAB_CHAR                ! TAB character
       parameter (TAB_CHAR=9)
*
       integer End_file_status         ! End of file status
       parameter (End_file_status=2)
*
       character End_run*(*)           ! End of run specifier
       parameter (End_run='end run')
*
       integer End_run_status          ! End of run block status
       parameter (End_run_status=1)
*
       integer Ok_status               ! Ok status
       parameter (Ok_status = 0)
 
*+ Local Variables
       integer Comment_pos             ! Position of comment in line
       integer Read_status             ! Status of read.
       integer Tab_pos                 ! position of tab character
 
*- Implementation Section ----------------------------------
 
10    read (logical_unit, '(a)', iostat=read_status ) line
      Record_num = Record_num + 1
 
      ! Remove comment from line if necessary
 
      Comment_pos = index (Line, comment)
      if (Comment_pos .eq. 0) then
         ! No comment on line
 
      else
         Line(Comment_pos:) = Blank
      endif
 
      ! remove tab character if necessary.
      Tab_pos = index(Line, char(TAB_CHAR))
      if (Tab_pos .gt. 0) then
         Line(Tab_pos:Tab_pos) = ' '
      endif
 
      if (Read_status .eq. Ok_status) then
 
         Line = Lower_case(Line)
         Line = No_leading_spaces(Line)
 
         if (Line.eq.End_run) then
            IOStatus = End_run_status
 
         else if (Line.eq.Blank) then
            goto 10
 
         else
            IOStatus = Ok_status
 
         endif
 
      else
         IOStatus = End_file_status
 
      endif
 
      return
      end
 
 
 
* ====================================================================
       integer function Open_param_file (Section_name)
* ====================================================================
      implicit none
      dll_export open_param_file
      include 'const.inc'              ! Constant definitions
      include 'read.inc'
 
*+ Sub-Program Arguments
      character Section_name*(*)       ! (INPUT) Section name to find
 
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
      character This_routine*(*)       ! Name of this routine
      parameter (This_routine='open_param_file')
 
*+ Local Variables
      character Msg*300                ! Message to display
      integer LU_param                 ! Logical unit number of open file
      character Module_name*(Max_module_name_size)
                                       ! Module name of caller
      integer Record_num               ! Record number in file
      character  full_section_name*100
      character  exclude_section_name*10
      integer    exclude_file_num
 
*- Implementation Section ----------------------------------
 
      call push_routine(This_routine)
 
      call Get_current_module(Module_name)
 
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
 
         call Fatal_error(ERR_user, msg)
      endif
 
      Open_param_file = LU_param
 
      call pop_routine(This_routine)
      return
      end
 
 
 
* ====================================================================
       character*(*) function Get_Section_Name (record)
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
      dll_import no_leading_spaces
      dll_import assign_string
      dll_import lower_case
      dll_import warning_error
      dll_import pop_routine
      character  No_Leading_spaces*(Function_string_len)
                                       ! function
      character  Lower_Case*(Function_string_len)
                                       ! function
 
*+ Local Variables
      logical    Section_found         ! legal section has  been found
      integer    Left_delimiter_pos    ! position of left delimiter in record
      integer    Right_delimiter_pos   ! position of left delimiter in record
      character  Section*(Max_section_name_size)
                                       ! name of section of ini file
      character  TempRecord*(Function_string_len)
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
 
      if (index(Record, Open_delimiter) .eq. 0) then
         ! There's no section here - exit.
 
         Section = blank
 
      else
         ! OK we could have a section name - find out.
 
         TempRecord= No_Leading_Spaces (Record)
         Left_delimiter_Pos = index (TempRecord, open_delimiter)
         Right_delimiter_Pos = index (TempRecord, close_delimiter)
 
         Section_found = Left_delimiter_Pos .eq. 1
     :             .and. Right_delimiter_Pos .ge. 2
 
         If (Section_found) then
                  ! We have a section heading
 
            If (Right_delimiter_Pos .ge. 3) then
               call assign_string (Section, TempRecord
     :                     (left_delimiter_pos+1:Right_delimiter_Pos-1))
               Section = No_Leading_Spaces (Lower_case (Section))
 
            Else
                  ! This is an empty section heading
               section = blank
            Endif
 
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
 
      call assign_string (Get_Section_Name, Section)
 
      call pop_routine (myname)
      return
      end
 
 
 
* ====================================================================
       character*(*) function Read_param_optional
     .     (Section_spec, Key_name, Multiple_keys)
* ====================================================================
      implicit none
      dll_export read_param_optional
      include 'const.inc'              ! constant definitions
      include 'read.inc'
 
*+ Sub-Program Arguments
      character Section_spec*(*)       ! (INPUT) Section specifier to search for
      character Key_name*(*)           ! (INPUT) Key name to search in
      logical Multiple_keys            ! (INPUT) Should we look for multiple
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
 
*+ Calls
      dll_import push_routine
      dll_import lower_case
      dll_import get_current_module
      dll_import read_key_from_section
      dll_import close_unit
      dll_import find_next_matching_section
      dll_import append_string
      dll_import pop_routine
      character Read_key_from_section*(Function_string_len)
                                       ! function
      character Lower_case*(Function_string_len)
                                       ! function
 
*+ Constant Values
      character myname*(*)             ! Name of this routine
      parameter (myname='Read_param_optional')
 
*+ Local Variables
      character Current_module_name*(Max_module_name_size)
                                       ! Name of current module
      integer Exclude_file_num         ! Exclude which file from search for
                                       ! sections
      logical Found_key                ! Have we found the key?
      logical Found_section            ! Have we found the section?
      character Return_string*(Function_string_len)
                                       ! String to return to caller.
      character New_string*(Function_string_len)
                                       ! String to return to caller.
      logical Same_module_calling      ! Is the same module calling
                                       ! as previous call?
      logical Same_section_spec        ! Is the section specifier the same as
                                       ! previous call?
 
*- Implementation Section ----------------------------------
 
      call push_routine(myname)
 
      Return_string = Blank
 
      ! Convert section name to lower case
 
      Section_spec = Lower_case(Section_spec)
 
      ! Determine if this is the same module calling as last module
 
      call Get_current_module(Current_module_name)
      Same_module_calling = (Last_module_name .eq. Current_module_name)
 
      ! Determine if this section name is the same as the last section.
 
      Same_section_spec = (Last_section_spec .eq. Section_spec)
 
      if (Same_module_calling .and. Same_section_spec
     :   .and. Current_unit_num.ne.0) then
         ! The same module is still calling this routine with the same
         ! section so continue reading where we left off.
 
         Return_string = Read_key_from_section
     .            (Current_unit_num, Current_record_num,
     .             Last_section_name, Key_name, Multiple_keys)
 
         Found_key = (Return_string .ne. Blank)
         Exclude_file_num = Search_section_index
 
      else
         Found_key = .false.
         Exclude_file_num = 0
      endif
 
      if (.not. Found_key) then
 
         Last_module_name = Current_module_name
         Last_section_spec = Section_spec
         Search_section_index = 0
 
10       continue
         ! We need to go find the section name so close off the
         ! old unit number we were looking in.
 
         if (Current_unit_num .gt. LU_Control_file2) then
            call Close_unit(Current_unit_num)
 
         else
            ! This is the first time through this routine
         endif
 
         call Find_next_matching_section
     .        (Current_module_name, Section_spec, Last_section_name,
     .         Current_unit_num, Current_record_num,
     .         Exclude_file_num, 'weather')
 
         Found_section = (Current_unit_num .gt. 0)
 
         if (Found_section) then
            New_string = Read_key_from_section
     .          (Current_unit_num, Current_record_num,
     .           Last_section_name, Key_name, Multiple_keys)
            Found_key = (New_string .ne. Blank)
 
            ! Append new_string to return string if multiple_keys was specified
            ! and found_key = true.
 
            if (Found_key .and. Multiple_keys) then
               New_string = Blank // New_string
               call append_string (Return_string, New_string)
            endif
 
            if (Found_key .and. .not. Multiple_keys) then
               ! Need look no further.  Report to summary file
 
               Return_string = New_string
               if (Current_param_file_name .ne. Blank) then
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
 
            Last_module_name = Blank
 
         endif
      endif
 
      Read_param_optional = Return_string
 
      call pop_routine(myname)
 
      return
      end
 
 
 
* ====================================================================
       character*(*) function Read_param
     .     (Section_spec, Key_name, Multiple_keys)
* ====================================================================
      implicit none
      dll_export read_param
      include 'const.inc'
 
*+ Sub-Program Arguments
      character Section_spec*(*)       ! (INPUT) Section specifier to search in
      character Key_name*(*)           ! (INPUT) Key name to search in
      logical Multiple_keys            ! (INPUT) Should we look for multiple
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
      character Read_param_optional*(Function_string_len)
                                       ! function
 
*+ Constant Values
      character myname*(*)             ! Name of this routine
      parameter (myname='Read_param')
 
*+ Local Variables
      character msg*300                ! err message to display
 
*- Implementation Section ----------------------------------
 
      call push_routine(myname)
 
      Read_param = Read_param_optional
     .    (Section_spec, Key_name, Multiple_keys)
 
      if (Read_param .eq. Blank) then
         ! Cannot find key name.  Issue fatal err message.
 
         msg =
     .      'Cannot find a parameter in any of the files/sections'//
     .      new_line//
     .      'specified in the control file.  Parameter name = '//
     .      Key_name//
     .      new_line//
     .      'Section specifier = '// Section_spec
 
         call Fatal_error(ERR_user, msg)
 
      else
         ! Found the key name - exit
 
      endif
 
      call pop_routine(myname)
      return
      end
 
 
 
* ====================================================================
       subroutine Read_next_param_section
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
 
      Section_suffix = Section
      call Read_next_param_line(Module_name, line, flag)
      Section_suffix = Blank
 
      return
      end
 
 
 
* ====================================================================
       integer function Get_logical_unit ()
* ====================================================================
      implicit none
      dll_export get_logical_unit
       include 'const.inc'             ! Constant definitions
 
*+ Purpose
*      Return the next available logical unit number.  Will produce an err
*      if no more unit numbers are available and return a -1 (file not found
*      flsg).
 
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
       integer    Unit_number_start    ! The first available unit number
       parameter (Unit_number_start = 20)
*
       integer    Unit_number_end      ! The last available unit number
       parameter (Unit_number_end = 100)
 
*+ Local Variables
       character  Error_string*110     ! err string
       logical    Unit_number_unavail  ! Shows if unit number is available
       integer    Unit_number          ! Unit number
 
*- Implementation Section ----------------------------------
 
      Unit_number = Unit_number_start
 
10    continue
         inquire (unit = Unit_number, opened = unit_number_unavail)
         if (unit_number_unavail) then
            Unit_number = Unit_number + 1
 
            if (Unit_number.gt.Unit_number_end) then
               Error_string =
     .         'All logical unit numbers are currently in use.' //
     .         New_line //
     .         'Increase the maximum logical unit number allowed.'
 
               call Fatal_error (ERR_internal, Error_string)
               Unit_number = file_not_found
 
            else
               goto 10
 
            endif
 
         else
 
      endif
 
      Get_logical_unit = Unit_number
 
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
       integer function Open_file (File_name)
* ====================================================================
      implicit none
      dll_export open_file
       include 'const.inc'             ! Constant definitions
 
*+ Sub-Program Arguments
       character File_name*(*)         ! File name to open.
 
*+ Purpose
*      Open and rewind a file and return the logical unit number.
*      Return -1 (file_not_found flag) if file cannot be opened and
*      calls Fatal_error
 
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
       integer Get_logical_unit        ! function
 
*+ Constant Values
       integer No_logical_units
       parameter (No_logical_units=0)
*
       integer Ok                      ! Status indicating open was successful.
       parameter (Ok = 0)
*
       character This_routine*(*)      ! Name of this routine
       parameter (This_routine='Open_file')
 
*+ Local Variables
       character Error_string*(Function_string_len) ! err string
       integer IOStatus                ! I/O status
       integer Unit_number             ! Unit number of open file.
       logical opened                  ! status of file
 
*- Implementation Section ----------------------------------
 
      call push_routine (this_routine)
 
      if (file_name.eq.blank) then           ! is filename ok?
         Open_file = file_not_found
 
         Error_string = '  Cannot open file. No file name.'
         call fatal_error (err_user, Error_string)
      else
            ! Is the file already open?
         inquire (file = file_name, opened = opened)
         if (opened) then
               ! Lets find out its unit number
            inquire (file = File_name, number = Unit_number)
            Open_file = Unit_number
         else
               ! get a new unit number and try to open it
            Unit_number = Get_logical_unit ()
 
            if (Unit_number.eq.file_not_found) then
               Open_file = No_logical_units
 
            else
 
               open (unit = Unit_number, file = File_name,
     :              status = 'OLD', iostat = IOStatus)
 
               if (IOStatus.eq.Ok) then
                               ! file has been opened.
 
                  rewind (Unit_number)
                  Open_file = Unit_number
 
!                  Error_string = new_line
!      :                       // ' File opened ok. -> '
!      :                       // file_name
!                  call write_string (lu_scr_sum, Error_string)
 
               else
                  Open_file = file_not_found
 
                  Error_string = ' Cannot open file -> '// file_name
                  call fatal_error (err_user, Error_string)
 
               endif
 
            endif
         endif
      endif
 
      call pop_routine (this_routine)
      return
      end
 
 
 
* ====================================================================
       logical function read_Init ()
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
 
      Last_module_name = Blank
      Last_section_name = Blank
      Current_unit_num = -1
      Section_suffix = Blank
 
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
       character  This_routine*(*)     ! Name of this routine
       parameter (This_routine='utility_close')
*
       integer    Unit_number_start    ! The first available unit number
       parameter (Unit_number_start = 20)
*
       integer    Unit_number_end      ! The last available unit number
       parameter (Unit_number_end = 100)
*
       integer LU_temp_file            ! Temporary file opened by engine
       parameter (LU_temp_file=20)     ! Don't ever close it.
 
*+ Local Variables
       logical    Unit_number_open     ! Shows if unit number is open
       integer    Unit_number          ! Unit number
 
*- Implementation Section ----------------------------------
 
      call Push_routine(This_routine)
 
      do 10 Unit_number = Unit_number_start, Unit_number_end
 
         ! If unit number is open - then close it.
 
         inquire (unit = Unit_number, opened = unit_number_open)
 
         if (unit_number_open .and. unit_number .ne. LU_temp_file) then
            close(Unit_number)
 
         else
            ! Unit number already closed.
         endif
10    continue
 
      call Pop_routine(This_routine)
      return
      end
 
 
 
* ====================================================================
      character*(*) function Read_file_string_optional
     :                       (Unit_number, Section_name, Key_name)
* ====================================================================
      implicit none
      dll_export read_file_string_optional
       include   'const.inc'           ! Constant definitions
 
*+ Sub-Program Arguments
       character  Key_name*(*)         ! (INPUT) Key name to search for
       character  Section_name*(*)     ! (INPUT) name of section of file
                                       ! for search.
       integer    Unit_number          ! (INPUT) Unit number of open file
 
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
       character  Read_key_from_section*(Function_string_len)
                                       ! function
       logical Find_section_name       ! function
 
*+ Constant Values
      character  myname*(*)            ! name of current procedure
      parameter (myname = 'read_file_string_optional')
*
      logical no_multiple_keys         ! Don't read multiple keys
      parameter (No_multiple_keys=.false.)
 
*+ Local Variables
       logical    Found                ! Found the key name ?
       integer Record_num              ! Record number in file
       character  Return_string*(Function_string_len)
                                       ! Parameter string read from file
 
*- Implementation Section ----------------------------------
 
      call push_routine (myname)
 
      Found = Find_section_name (Unit_number, Record_num, Section_name)
 
      if (Found) then
          Return_string = Read_key_from_section
     .        (Unit_number, Record_num, Section_name,
     .         Key_name, No_multiple_keys)
 
      else
         ! Cannot find the section name.
      endif
 
      if (Found) then
         call assign_string (Read_file_string_optional, Return_string)
 
      else
         Read_file_string_optional = Blank
      endif
 
      call pop_routine (myname)
      return
      end
 
 
 
* ====================================================================
       logical function Find_section_name
     .         (Unit_number, Record_num, Section_name)
* ====================================================================
      implicit none
      dll_export find_section_name
      include 'const.inc'              ! constant definitions
 
*+ Sub-Program Arguments
      integer Unit_number              ! (INPUT) Unit number to read from
      character Section_name*(*)       ! (INPUT) Section name to search for
      integer Record_num               ! (OUTPUT) Current record number in file
 
*+ Purpose
*     Go find the section name on the specified open logical unit number.
*     Return TRUE if found or FALSE otherwise.  The logical unit number
*     will be positioned to the line AFTER the section name if found.
 
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
      character Get_section_name*(Function_string_len)
                                       ! function
 
*+ Constant Values
      integer Ok_status                ! Read was ok
      parameter (Ok_status=0)
*
      character Routine_name*(*)       ! Name of this routine
      parameter (Routine_name='Find_section_name')
 
*+ Local Variables
      character Line*(Function_string_len)
                                       ! Line read from file
      integer Read_status              ! Status of read_line
      logical Section_found            ! Have we found the section ?
      character section_name_read*(function_string_len)
 
*- Implementation Section ----------------------------------
 
      call push_routine(Routine_name)
 
      rewind (Unit_number)
      Record_num = 1
 
      if (section_name .eq. No_section) then
         ! we have no section specified - start at beginning of file
 
         Section_found = .true.
 
      else
10       continue
         call Read_line (Unit_number, Record_num, Line, Read_status)
 
         if (Read_status .eq. Ok_status) then
            section_name_read = get_section_name (line)
            If (section_name_read .ne. Section_name) then
               if (index (section_name_read, 'weather').gt.4) then
                     ! don't look past weather section
                  section_found = .false.
               else
                  goto 10
               endif
 
            else
               ! we have found the section we want
 
               Section_found = .true.
            endif
         else
            ! we have trouble reading file - probably EOF
 
            Section_found = .false.
         endif
      endif
 
      Find_section_name = Section_found
 
      call pop_routine(Routine_name)
      return
      end
 
 
 
* ====================================================================
       character*(*) function Read_key_low_level
     .    (Unit_number, Record_num,
     .     Section_name, Key_name, Multiple_keys, Wrap_around)
* ====================================================================
      implicit none
      dll_export read_key_low_level
      include 'const.inc'              ! constant definitions
      include 'read.inc'
 
*+ Sub-Program Arguments
      integer Unit_number              ! (INPUT) Open unit number.
      integer Record_num               ! (OUTPUT) Current record number in file
      character Key_name*(*)           ! (INPUT) Key name to search for
      character Section_name*(*)       ! (INPUT) Section name we're in.
      logical Multiple_keys            ! (INPUT) Look for multiple keys ?
      logical Wrap_around              ! (INPUT) Wrap around to top of section?
 
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
      character Get_data_string*(Function_string_len)
                                       ! function
      character Get_section_name*(Function_string_len)
                                       ! function
      logical Find_section_name        ! function
 
*+ Constant Values
      integer Ok_status                ! Status of read if ok
      parameter (Ok_status=0)
*
      character Routine_name*(*)       ! Name of this routine
      parameter (Routine_name='Read_key_low_level')
 
*+ Local Variables
      logical End_section              ! Have we reached end of section?
      logical Found                    ! Have we found the key ?
      character Line*(Function_string_len)
                                       ! Line read from file
      integer Read_status              ! Status from read_line
 
*- Implementation Section ----------------------------------
 
      call push_routine(Routine_name)
 
      Read_key_low_level = Blank
 
20    continue
 
      if (Searching_top_half .and.
     .    Record_num .ge. Start_record_num) then
         ! That's it - look no more.
 
         Found = .false.
         End_section = .true.
 
      else
 
         call Read_line (Unit_number, Record_num, Line, Read_status)
 
         if (Read_status .eq. Ok_status) then
            if (Get_section_name (Line) .eq. blank) then
               Line = Get_data_string (Line, Key_name)
               Found = (Line .ne. Blank)
               End_section = .false.
            else
               ! We have reached the end of section
               Found = .false.
               End_section = .true.
 
            endif
         else
            ! We have reached the end of file
            Found = .false.
            End_section = .true.
         endif
      endif
 
      if (Found) then
         if (Multiple_keys) then
 
            e_message = Blank // Line
            Line = e_message
 
            call append_string (Read_key_low_level, Line)
            goto 20
 
         else
            call assign_string (Read_key_low_level, Line)
         endif
 
      else if (End_section) then
         if (Searching_top_half) then
            ! We've reached our starting point - look no further.
 
         else if (Wrap_around) then
            ! We've reached the bottom of the section - rewind unit
            ! to start of section and keep looking.
 
            Searching_top_half = .true.
            Found = Find_section_name
     .              (Unit_number, Record_num, Section_name)
            goto 20
         endif
 
      else
         ! Keep looking
 
         goto 20
      endif
 
      call pop_routine(Routine_name)
      return
      end
 
 
 
* ====================================================================
       character*(*) function Read_key_from_section
     .    (Unit_number, Record_num,
     .     Section_name, Key_name, Multiple_keys)
 
* ====================================================================
      implicit none
      dll_export read_key_from_section
      include 'const.inc'
      include 'read.inc'
 
*+ Sub-Program Arguments
      integer Unit_number              ! (INPUT) Open unit number.
      integer Record_num               ! (OUTPUT) Current record number in file
      character Key_name*(*)           ! (INPUT) Key name to search for
      character Section_name*(*)       ! (INPUT) Section name we're in.
      logical Multiple_keys            ! (INPUT) Look for multiple keys ?
 
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
      character Read_key_low_level*(Function_string_len)
                                       ! function
 
*+ Constant Values
      logical Do_wrap_around           ! Do wrap around section if necessary
      parameter (Do_wrap_around=.true.)
 
*- Implementation Section ----------------------------------
 
      Start_record_num = Record_num
      Searching_top_half = .false.
 
      Read_key_from_section = Read_key_low_level
     .   (Unit_number, Record_num, Section_name, Key_name,
     .    Multiple_keys, Do_wrap_around)
      return
      end
 
 
 
* ====================================================================
       character*(*) function Read_next_key_from_section
     .    (Unit_number, Record_num,
     .     Section_name, Key_name, Multiple_keys)
* ====================================================================
      implicit none
      dll_export read_next_key_from_section
      include 'const.inc'
 
*+ Sub-Program Arguments
      integer Unit_number              ! (INPUT) Open unit number.
      integer Record_num               ! (OUTPUT) Current record number in file
      character Key_name*(*)           ! (INPUT) Key name to search for
      character Section_name*(*)       ! (INPUT) Section name we're in.
      logical Multiple_keys            ! (INPUT) Look for multiple keys ?
 
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
      character Read_key_low_level*(Function_string_len)
                                       ! function
 
*+ Constant Values
      logical Dont_wrap_around         ! Dont wrap around section
      parameter (Dont_wrap_around=.true.)
 
*- Implementation Section ----------------------------------
 
      Read_next_key_from_section = Read_key_low_level
     .   (Unit_number, Record_num, Section_name, Key_name,
     .    Multiple_keys, Dont_wrap_around)
 
      return
      end
 
 
 
* ====================================================================
       subroutine Find_next_matching_section
     .    (Current_module_name, Section_spec, Section_name,
     .     Open_unit_num, Record_num, Exclude_file_num,
     .     Exclude_section_name)
* ====================================================================
      implicit none
      dll_export find_next_matching_section
      include 'const.inc'              ! constant definitions
      include 'read.inc'
 
*+ Sub-Program Arguments
      character Current_module_name*(*) ! (INPUT) Name of current module
      character Section_spec*(*)       ! (INTPUT) Section specifier to look for.
      character Section_name*(*)       ! (OUTPUT) Real section name found in
                                       ! file
      integer Open_unit_num            ! (OUTPUT) Unit number of open file
      integer Record_num               ! (OUTPUT) Current record number in file
      integer Exclude_file_num         ! (INPUT) File number to exclude from
                                       ! search.
      character Exclude_section_name*(*) ! (INPUT) Section name to exclude from
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
 
*+ Calls
      dll_import get_file_section_list
      dll_import append_string
      dll_import get_control_file_section
      dll_import open_file
      dll_import find_section_name
      integer Open_file                ! function
      logical Find_section_name        ! function
      character Get_control_file_section*(Function_string_len)
                                       ! function
 
*+ Local Variables
      character File_list(Max_files)*(Max_file_name_size)
                                       ! List of files for current module
      integer Num_files                ! Number of files specified for module
      character Section_list(Max_files)*(Max_section_name_size)
                                       ! List of sections for current module
      logical Section_found            ! Have we found the section yet?
 
*- Implementation Section ----------------------------------
 
      ! Get all files/sections for current module
 
      call Get_file_section_list
     .    (Current_module_name, File_list, Section_list, Num_files)
 
      ! Check to see if we've run out of sections to look in.
 
      if (Search_section_index .ge. Num_files) then
         ! No more sections left - exit
 
         Open_unit_num = -1
 
      else
 
         ! Loop through each of the section names until one is found.
 
10       continue
         Search_section_index = Search_section_index + 1
 
         if (Search_section_index .gt. Num_files) then
            ! Cannot find section - exit
 
            Open_unit_num = -1
 
         else
            ! If our section spec is blank then we don't need to append
            ! the section spec to our section name.
 
            Section_name = Section_list(Search_section_index)
            if (Section_spec .eq. Blank) then
               ! Nothing to append to our search_section
 
            else
               ! Append the section name to the search section.
 
               call append_string (Section_name, '.')
               call append_string (Section_name, Current_module_name)
               call append_string (Section_name, '.')
               call append_string (Section_name, Section_spec)
            endif
 
            ! Check for a blank file name in file_list.  If found then
            ! read from either control file1 or control file 2 depending
            ! on whether the section name = current control file section.
 
            if (File_list(Search_section_index) .eq. Blank) then
               if (Section_name .eq. Get_control_file_section()) then
                  Open_unit_num = LU_Control_file
               else
                  Open_unit_num = LU_Control_file2
               endif
 
            else
               ! Treat the input module as a special case.  If the current
               ! module is input and the section found was 'weather' then
               ! skip to next file/section.
 
               if ((Current_module_name .eq. 'input' .or.
     .           Current_module_name .eq. 'hardinpt') .and.
     .           index(Section_name, Exclude_section_name) .gt. 0) then
                  goto 10
 
               else if (Exclude_file_num .eq. Search_section_index)then
                  goto 10
 
               else
                  ! Not special case - open file
                  Open_unit_num = Open_file
     .               (File_list(Search_section_index))
               endif
 
            endif
 
            ! If nothing wrong with file then go find section
 
            if (Open_unit_num .gt. 0) then
               Record_num = 1
               Section_found = Find_section_name
     .                   (Open_unit_num, Record_num, Section_name)
               if (Section_found) then
                  ! Found section - exit routine.
 
                  Current_param_file_name = File_list
     .                (Search_section_index)
               else
                  ! Cannot find section - continue looking
 
!DPH                  E_message = 'Cannot find section : ' // Section_name
!DPH                  call Warning_error (ERR_user, E_Message)
 
 
                  if (Open_unit_num .gt. LU_Control_file2) then
                     close(Open_unit_num)
 
                  else
                     ! Don't close control file - ever
 
                  endif
 
                  goto 10
               endif
 
            else
               ! Could'nt open file.  Fatal_error already been called.
               ! Simply exit.
 
               Open_unit_num = -1
 
            endif
         endif
      endif
 
      return
      end
 
 
 
* ====================================================================
       subroutine Read_next_param_line (module_name, line, flag)
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
      integer Open_file_section   ! function
 
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
      integer   Current_FileNo    ! index of current param file in
                                  ! file_list
      character File_list(Max_files)*(Max_file_name_size)
                                   ! List of files for current module
      character last_module*(max_module_name_size)
                                   ! name of the module that last called
                                   ! this routine
      integer Num_files            ! No. of files specified for module
      character Section_list(Max_files)*(Max_section_name_size)
                                   ! List of sections for current module
 
*+ Initial Data Values
      save file_list
      save section_list
      save Current_FileNo
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
 
         call Get_File_Section_List (module_name, File_List,
     :               Section_List, Num_files)
 
         Current_FileNo = 1
 
         ! close off current unit number if necessary.
 
         if (Current_unit_num .gt. LU_Control_file2) then
            call Close_unit(Current_unit_num)
         endif
 
10       continue
         If (num_files.gt.0 .and. Current_FileNo .le. num_files) then
 
            Current_unit_num = Open_File_section (
     :                               File_List(Current_FileNo),
     :                               Section_List(Current_FileNo))
            if (Current_unit_num .gt. 0) then
                  ! we have started the search ok - go to it!!
 
               call Read_param_line (Current_unit_num, File_List,
     :                               Section_List,
     :                               Num_Files, Current_FileNo, Line)
 
*DPH               write(msg, '(50a)' )
*DPH     .            'File opened ok -> ',
*DPH     .            File_list(Current_FileNo)
*DPH               call Write_string(LU_Scr_sum, msg)
 
            else
               Current_FileNo = Current_FileNo + 1
               goto 10
            endif
 
         else
            line = blank
         endif
 
      else if (flag.eq.iterate_flag) then
 
         call Read_param_line (Current_unit_num, File_List,
     :                         Section_List,
     :                         Num_Files, Current_FileNo, Line)
 
      else if (flag.eq.end_flag) then
         line = blank
 
      else
         call fatal_error(Err_internal, 'Bad flag value')
 
      endif
 
      if (Line.eq.blank) then
         ! We have reached the end of the search - reset the last module flag
         ! so that we know to start from scratch next time
 
         Last_Module = blank
 
      else
      endif
 
      call pop_routine (myname)
      return
      end
 
 
 
* ====================================================================
       subroutine Read_Param_Line (LUN, File_List, Section_List,
     :                  Num_Files, Current_FileNo, Line)
* ====================================================================
      implicit none
      dll_export read_param_line
       include 'const.inc'
       include 'read.inc'
 
*+ Sub-Program Arguments
       integer Current_FileNo
       character File_List(*)*(*)
       character Line*(*)
       integer LUN
       integer Num_Files
       character Section_List(*)*(*)
 
*+ Purpose
*     <insert here>
 
*+ Assumptions
*      Assume that LUN is the opened File_List(current_fileno).
 
*+  Mission Statement
*      
 
*+ Changes
*   NeilH - 23-11-1994 - Programmed and Specified
*   270295 jngh initialised dummy.
 
*+ Calls
      dll_import push_routine
      dll_import read_line
      dll_import get_section_name
      dll_import close_unit
      dll_import open_file_section
      dll_import pop_routine
       integer Open_File_section
       character get_section_name*(max_section_name_size)
 
*+ Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'Read_Param_Line')
 
*+ Local Variables
       integer dummy
       integer IOStatus
       character section_name*(max_section_name_size)
 
*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      dummy = 0
  100 continue
      call Read_Line (LUN, dummy, Line, IOStatus)
      section_name = get_section_name (line)
 
      If ((IOStatus.ne.0).or.(section_name.ne.blank)) then
 
         if (LUN .ne. LU_Control_file2) then
            call close_unit (LUN)
         endif
 
10       continue
         If (Current_FileNo.lt.Num_Files) then
 
           Current_FileNo = Current_FileNo + 1
           LUN = Open_File_section (
     :                    File_List(Current_FileNo),
     :                    Section_List(Current_FileNo))
           If (LUN.gt.0) then
              goto 100
           else
              goto 10
           endif
 
         else
            ! search is finished - send blank string
            Line = blank
 
         endif
 
      else
 
      endif
 
      call pop_routine (myname)
      return
      end
 
 
 
* ====================================================================
       integer function Open_File_Section (file,section)
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
      dll_import open_file
      dll_import assign_string
      dll_import append_string
      dll_import find_section_name
      dll_import close_unit
      dll_import lastnb
      dll_import fatal_error
      dll_import pop_routine
      integer Open_File
      logical Find_section_name
      integer LastNB
 
*+ Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'Open_File_Section')
 
*+ Local Variables
      integer dummy
      integer LUN
      logical section_found
      character Section_name*100       ! Section to search for
      character Current_module_name*(Max_module_name_size)
                                       ! Name of current module
 
*- Implementation Section ----------------------------------
      call push_routine (myname)
 
      call Get_current_module(Current_module_name)
 
      if (File .eq. ' ') then
         LUN = LU_Control_file2
 
      else
         LUN = Open_file (File)
      endif
 
      If (LUN .gt. 0) then
 
          if (Section_suffix .eq. Blank) then
             call assign_string (Section_name, Section)
          else
             call assign_string (Section_name, Section)
             call append_string (Section_name, '.')
             call append_string
     .             (Section_name, Current_module_name)
             call append_string (Section_name, '.')
             call append_string (Section_name, Section_suffix)
          endif
 
          Section_found = Find_section_name (LUN, Dummy,Section_name)
 
          if (Section_found) then
             ! Got it - exit routine
 
          else if (Section_suffix .eq. Blank) then
*DPH             e_message =
*DPH     :            'Cannot find section '//Section(:lastnb(Section))
*DPH     :          //' in file:- '//File(:LastNB(File))
*DPH             call Fatal_error (Err_Internal, e_message)
             if (LUN .ne. LU_Control_file2) then
                call close_unit (LUN)
             endif
             LUN = 0
 
          else
             if (LUN .ne. LU_Control_file2) then
                call close_unit(LUN)
             endif
             LUN = 0
          endif
 
       else
          e_message = 'Cannot open file:- '//File(:LastNB(File))
          call Fatal_error (Err_Internal, e_message)
       endif
 
      Open_file_section = LUN
 
      call pop_routine (myname)
      return
      end
 
 
 
