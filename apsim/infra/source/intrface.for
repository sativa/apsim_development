       module InterfaceModule


       contains

* ====================================================================
       logical function intrface_init ()
* ====================================================================
      Use ErrorModule
      implicit none

*+ Purpose
*     Initialisation for the apsim interface routines.

*+  Mission Statement
*

*+ Changes
*     DPH 18/10/95
*     SB 24/4/98  renamed from postbox_init().

*+ Calls

*- Implementation Section ----------------------------------

!      g_Current_message = 0
!      g_Empty_double_slot = 1
!      g_Empty_char_slot = 1
!      g_Empty_variable_slot = 1

      intrface_init = .true.
      return
      end function



* ====================================================================
       subroutine intrface_term()
* ====================================================================
      Use ErrorModule
      implicit none

*+ Purpose
*     Clean up resources used by interface routines.

*+  Mission Statement
*

*+ Changes
*     SB 24/4/98  Created.

*+ Calls

*- Implementation Section ----------------------------------

      return
      end subroutine



* ====================================================================
       subroutine Collect_char_array (
     :                                array_name, size_of, units
     :                              , array, numvals)
* ====================================================================
      Use ErrorModule
      implicit none

*+ Sub-Program Arguments
      character  array_name*(*)        ! (INPUT) array name to search for
      integer    size_of               ! (INPUT) size_of of array
      character  Units*(*)             ! (INPUT) Units required by caller
      character  array(*)*(*)          ! (OUTPUT) array returned to caller
      integer    numvals               ! (OUTPUT) Number of values returned

*+ Purpose
*   Respond to the 'posting' of the given data type by retrieving data
*   from the postbox for the given variable name.

*+  Mission Statement
*     Get %4 from the message

*+ Changes
*     050696 nih programmed and specified

*+ Calls

*+ Constant Values
      character  my_name*(*)           ! Name of subroutine
      parameter (my_name = 'Collect_char_array')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call Respond2set_char_array
     .    (Array_name, size_of, Units, array, numvals)

      call pop_routine (my_name)
      return
      end subroutine



* ====================================================================
       subroutine Collect_char_array_optional (
     :                                array_name, size_of, units
     :                              , array, numvals)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none

*+ Sub-Program Arguments
      character  array_name*(*)        ! (INPUT) array name to search for
      integer    size_of               ! (INPUT) size_of of array
      character  Units*(*)             ! (INPUT) Units required by caller
      character  array(*)*(*)          ! (OUTPUT) array returned to caller
      integer    numvals               ! (OUTPUT) Number of values returned

*+ Purpose
*   Respond to the 'posting' of the given data type by retrieving data
*   from the postbox for the given variable name.

*+  Mission Statement
*     Get %4 from the message |ITALIC{(OPTIONAL)}

*+ Changes
*     050696 nih programmed and specified

*+ Calls

*+ Constant Values
      logical Allow_zero_numvals       ! Is this an optional routine?
      parameter (Allow_zero_numvals = .true.)
*
      character  my_name*(*)           ! Name of subroutine
      parameter (my_name = 'Collect_char_array_optional')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call Respond2Post_char_array
     .   (array_name, size_of, Units, Array, Numvals,
     .    Allow_zero_numvals, 1)

      call pop_routine (my_name)
      return
      end subroutine



* ====================================================================
       subroutine Collect_char_var (
     :                                variable_name, units
     :                              , variable, numvals)
* ====================================================================
      Use ErrorModule
      implicit none

*+ Sub-Program Arguments
      character  Variable_name*(*)     ! (INPUT) Variable name to search for
      character  Units*(*)             ! (INPUT) Units required by caller
      character  variable*(*)          ! (OUTPUT) Variable returned to caller
      integer    numvals               ! (OUTPUT) Number of values returned

*+ Purpose
*   Respond to the 'posting' of the given data type by retrieving data
*   from the postbox for the given variable name.

*+  Mission Statement
*     Get %3 from the message

*+ Changes
*     050696 nih programmed and specified

*+ Calls

*+ Constant Values
      character  my_name*(*)           ! Name of subroutine
      parameter (my_name = 'Collect_char_var')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call Respond2set_char_var
     .    (Variable_name, Units, Variable, numvals)

      call pop_routine (my_name)
      return
      end subroutine



* ====================================================================
       subroutine Collect_char_var_optional (
     :                                variable_name, units
     :                              , variable, numvals)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none

*+ Sub-Program Arguments
      character  Variable_name*(*)     ! (INPUT) Variable name to search for
      character  Units*(*)             ! (INPUT) Units required by caller
      character  variable*(*)          ! (OUTPUT) Variable returned to caller
      integer    numvals               ! (OUTPUT) Number of values returned

*+ Purpose
*   Respond to the 'posting' of the given data type by retrieving data
*   from the postbox for the given variable name.

*+  Mission Statement
*     Get %3 from the message |ITALIC{(OPTIONAL)}

*+ Changes
*     050696 nih programmed and specified

*+ Calls

*+ Constant Values
      logical Allow_zero_numvals       ! Is this an optional routine?
      parameter (Allow_zero_numvals = .true.)
*
      character  my_name*(*)           ! Name of subroutine
      parameter (my_name = 'Collect_char_var_optional')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call Respond2Post_char_var
     .   (Variable_name, Units, Variable, Numvals,
     .    Allow_zero_numvals, 1)

      call pop_routine (my_name)
      return
      end subroutine



* ====================================================================
       subroutine Collect_double_array (
     :                                array_name, size_of, units
     :                              , array, numvals
     :                              , lower_limit, upper_limit)
* ====================================================================
      Use ErrorModule
      implicit none

*+ Sub-Program Arguments
      character  array_name*(*)        ! (INPUT) array name to search for
      integer    size_of               ! (INPUT) size_of of array
      character  Units*(*)             ! (INPUT) Units required by caller
      double precision array(*)        ! (OUTPUT) array returned to caller
      integer    numvals               ! (OUTPUT) Number of values returned
      double precision Lower_limit     ! (INPUT) Lower limit for bounds check
      double precision Upper_limit     ! (INPUT) Upper limit for bounds check

*+ Purpose
*   Respond to the 'posting' of the given data type by retrieving data
*   from the postbox for the given variable name.

*+  Mission Statement
*     Get %4 from the message (Lower Bound = %6, Upper Bound = %7)

*+ Changes
*     050696 nih programmed and specified

*+ Calls

*+ Constant Values
      character  my_name*(*)           ! Name of subroutine
      parameter (my_name = 'Collect_double_array')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call Respond2set_double_array
     .    (Array_name, size_of, Units, array, numvals,
     .     Lower_limit, Upper_limit)

      call pop_routine (my_name)
      return
      end subroutine



* ====================================================================
       subroutine Collect_double_array_optional (
     :                                array_name, size_of, units
     :                              , array, numvals
     :                              , lower_limit, upper_limit)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none

*+ Sub-Program Arguments
      character  array_name*(*)        ! (INPUT) array name to search for
      integer    size_of               ! (INPUT) size_of of array
      character  Units*(*)             ! (INPUT) Units required by caller
      double precision array(*)        ! (OUTPUT) array returned to caller
      integer    numvals               ! (OUTPUT) Number of values returned
      double precision Lower_limit     ! (INPUT) Lower limit for bounds check
      double precision Upper_limit     ! (INPUT) Upper limit for bounds check

*+ Purpose
*   Respond to the 'posting' of the given data type by retrieving data
*   from the postbox for the given variable name.

*+  Mission Statement
*     Get %4 from the message (Lower Bound = %6, Upper Bound = %7)|ITALIC{(OPTIONAL)}

*+ Changes
*     050696 nih programmed and specified

*+ Calls

*+ Constant Values
      logical Allow_zero_numvals       ! Is this an optional routine?
      parameter (Allow_zero_numvals = .true.)
*
      character  my_name*(*)           ! Name of subroutine
      parameter (my_name = 'Collect_double_array_optional')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call Respond2Post_double_array
     .   (array_name, size_of, Units, Array, Numvals,
     .    Lower_limit, Upper_limit,
     .    Allow_zero_numvals, 1)

      call pop_routine (my_name)
      return
      end subroutine



* ====================================================================
       subroutine Collect_double_var (
     :                                variable_name, units
     :                              , variable, numvals
     :                              , lower_limit, upper_limit)
* ====================================================================
      Use ErrorModule
      implicit none

*+ Sub-Program Arguments
      character  Variable_name*(*)     ! (INPUT) Variable name to search for
      character  Units*(*)             ! (INPUT) Units required by caller
      double precision variable        ! (OUTPUT) Variable returned to caller
      integer    numvals               ! (OUTPUT) Number of values returned
      double precision Lower_limit     ! (INPUT) Lower limit for bounds check
      double precision Upper_limit     ! (INPUT) Upper limit for bounds check

*+ Purpose
*   Respond to the 'posting' of the given data type by retrieving data
*   from the postbox for the given variable name.

*+  Mission Statement
*     Get %3 from the message (Lower Bound = %5, Upper Bound = %6)

*+ Changes
*     050696 nih programmed and specified

*+ Calls

*+ Constant Values
      character  my_name*(*)           ! Name of subroutine
      parameter (my_name = 'Collect_double_var')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call Respond2set_double_var
     .    (Variable_name, Units, Variable, numvals,
     .     Lower_limit, Upper_limit)

      call pop_routine (my_name)
      return
      end subroutine



* ====================================================================
       subroutine Collect_double_var_optional (
     :                                variable_name, units
     :                              , variable, numvals
     :                              , lower_limit, upper_limit)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none

*+ Sub-Program Arguments
      character  Variable_name*(*)     ! (INPUT) Variable name to search for
      character  Units*(*)             ! (INPUT) Units required by caller
      double precision variable        ! (OUTPUT) Variable returned to caller
      integer    numvals               ! (OUTPUT) Number of values returned
      double precision Lower_limit     ! (INPUT) Lower limit for bounds check
      double precision Upper_limit     ! (INPUT) Upper limit for bounds check

*+ Purpose
*   Respond to the 'posting' of the given data type by retrieving data
*   from the postbox for the given variable name.

*+  Mission Statement
*     Get %3 from the message (Lower Bound = %5, Upper Bound = %6)|ITALIC{(OPTIONAL)}

*+ Changes
*     050696 nih programmed and specified

*+ Calls

*+ Constant Values
      logical Allow_zero_numvals       ! Is this an optional routine?
      parameter (Allow_zero_numvals = .true.)
*
      character  my_name*(*)           ! Name of subroutine
      parameter (my_name = 'Collect_double_var_optional')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call Respond2Post_double_var
     .   (Variable_name, Units, Variable, Numvals,
     .    Lower_limit, Upper_limit,
     .    Allow_zero_numvals, 1)

      call pop_routine (my_name)
      return
      end subroutine



* ====================================================================
       subroutine Collect_integer_array (
     :                                array_name, size_of, units
     :                              , array, numvals
     :                              , lower_limit, upper_limit)
* ====================================================================
      Use ErrorModule
      implicit none

*+ Sub-Program Arguments
      character  array_name*(*)        ! (INPUT) array name to search for
      integer    size_of               ! (INPUT) size_of of array
      character  Units*(*)             ! (INPUT) Units required by caller
      integer    array(*)              ! (OUTPUT) array returned to caller
      integer    numvals               ! (OUTPUT) Number of values returned
      integer    Lower_limit           ! (INPUT) Lower limit for bounds check
      integer    Upper_limit           ! (INPUT) Upper limit for bounds check

*+ Purpose
*   Respond to the 'posting' of the given data type by retrieving data
*   from the postbox for the given variable name.

*+  Mission Statement
*     Get %4 from the message (Lower Bound = %6, Upper Bound = %7)

*+ Changes
*     050696 nih programmed and specified

*+ Calls

*+ Constant Values
      character  my_name*(*)           ! Name of subroutine
      parameter (my_name = 'Collect_integer_array')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call Respond2set_integer_array
     .    (Array_name, size_of, Units, array, numvals,
     .     Lower_limit, Upper_limit)

      call pop_routine (my_name)
      return
      end subroutine



* ====================================================================
       subroutine Collect_integer_array_optional (
     :                                array_name, size_of, units
     :                              , array, numvals
     :                              , lower_limit, upper_limit)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none

*+ Sub-Program Arguments
      character  array_name*(*)        ! (INPUT) array name to search for
      integer    size_of               ! (INPUT) size_of of array
      character  Units*(*)             ! (INPUT) Units required by caller
      integer    array(*)              ! (OUTPUT) array returned to caller
      integer    numvals               ! (OUTPUT) Number of values returned
      integer    Lower_limit           ! (INPUT) Lower limit for bounds check
      integer    Upper_limit           ! (INPUT) Upper limit for bounds check

*+ Purpose
*   Respond to the 'posting' of the given data type by retrieving data
*   from the postbox for the given variable name.

*+  Mission Statement
*     Get %4 from the message (Lower Bound = %6, Upper Bound = %7)|ITALIC{(OPTIONAL)}

*+ Changes
*     050696 nih programmed and specified

*+ Calls

*+ Constant Values
      logical Allow_zero_numvals       ! Is this an optional routine?
      parameter (Allow_zero_numvals = .true.)
*
      character  my_name*(*)           ! Name of subroutine
      parameter (my_name = 'Collect_integer_array_optional')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call Respond2Post_integer_array
     .   (array_name, size_of, Units, Array, Numvals,
     .    Lower_limit, Upper_limit,
     .    Allow_zero_numvals, 1)

      call pop_routine (my_name)
      return
      end subroutine



* ====================================================================
       subroutine Collect_integer_var (
     :                                variable_name, units
     :                              , variable, numvals
     :                              , lower_limit, upper_limit)
* ====================================================================
      Use ErrorModule
      implicit none

*+ Sub-Program Arguments
      character  Variable_name*(*)     ! (INPUT) Variable name to search for
      character  Units*(*)             ! (INPUT) Units required by caller
      integer    variable              ! (OUTPUT) Variable returned to caller
      integer    numvals               ! (OUTPUT) Number of values returned
      integer    Lower_limit           ! (INPUT) Lower limit for bounds check
      integer    Upper_limit           ! (INPUT) Upper limit for bounds check

*+ Purpose
*   Respond to the 'posting' of the given data type by retrieving data
*   from the postbox for the given variable name.

*+  Mission Statement
*     Get %3 from the message (Lower Bound = %5, Upper Bound = %6)

*+ Changes
*     050696 nih programmed and specified

*+ Calls

*+ Constant Values
      character  my_name*(*)           ! Name of subroutine
      parameter (my_name = 'Collect_integer_var')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call Respond2set_integer_var
     .    (Variable_name, Units, Variable, numvals,
     .     Lower_limit, Upper_limit)

      call pop_routine (my_name)
      return
      end subroutine



* ====================================================================
       subroutine Collect_integer_var_optional (
     :                                variable_name, units
     :                              , variable, numvals
     :                              , lower_limit, upper_limit)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none

*+ Sub-Program Arguments
      character  Variable_name*(*)     ! (INPUT) Variable name to search for
      character  Units*(*)             ! (INPUT) Units required by caller
      integer    variable              ! (OUTPUT) Variable returned to caller
      integer    numvals               ! (OUTPUT) Number of values returned
      integer    Lower_limit           ! (INPUT) Lower limit for bounds check
      integer    Upper_limit           ! (INPUT) Upper limit for bounds check

*+ Purpose
*   Respond to the 'posting' of the given data type by retrieving data
*   from the postbox for the given variable name.

*+  Mission Statement
*     Get %3 from the message (Lower Bound = %5, Upper Bound = %6)|ITALIC{(OPTIONAL)}

*+ Changes
*     050696 nih programmed and specified

*+ Calls

*+ Constant Values
      logical Allow_zero_numvals       ! Is this an optional routine?
      parameter (Allow_zero_numvals = .true.)
*
      character  my_name*(*)           ! Name of subroutine
      parameter (my_name = 'Collect_integer_var_optional')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call Respond2Post_integer_var
     .   (Variable_name, Units, Variable, Numvals,
     .    Lower_limit, Upper_limit,
     .    Allow_zero_numvals, 1)

      call pop_routine (my_name)
      return
      end subroutine



* ====================================================================
       subroutine Collect_logical_array (
     :                                array_name, size_of, units
     :                              , array, numvals)
* ====================================================================
      Use ErrorModule
      implicit none

*+ Sub-Program Arguments
      character  array_name*(*)        ! (INPUT) array name to search for
      integer    size_of               ! (INPUT) size_of of array
      character  Units*(*)             ! (INPUT) Units required by caller
      logical    array(*)              ! (OUTPUT) array returned to caller
      integer    numvals               ! (OUTPUT) Number of values returned

*+ Purpose
*   Respond to the 'posting' of the given data type by retrieving data
*   from the postbox for the given variable name.

*+  Mission Statement
*     Get %4 from the message

*+ Changes
*     050696 nih programmed and specified

*+ Calls

*+ Constant Values
      character  my_name*(*)           ! Name of subroutine
      parameter (my_name = 'Collect_logical_array')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call Respond2set_logical_array
     .    (Array_name, size_of, Units, array, numvals)

      call pop_routine (my_name)
      return
      end subroutine



* ====================================================================
       subroutine Collect_logical_array_optional (
     :                                array_name, size_of, units
     :                              , array, numvals)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none

*+ Sub-Program Arguments
      character  array_name*(*)        ! (INPUT) array name to search for
      integer    size_of               ! (INPUT) size_of of array
      character  Units*(*)             ! (INPUT) Units required by caller
      logical    array(*)              ! (OUTPUT) array returned to caller
      integer    numvals               ! (OUTPUT) Number of values returned

*+ Purpose
*   Respond to the 'posting' of the given data type by retrieving data
*   from the postbox for the given variable name.

*+  Mission Statement
*     Get %4 from the message |ITALIC{(OPTIONAL)}

*+ Changes
*     050696 nih programmed and specified

*+ Calls

*+ Constant Values
      logical Allow_zero_numvals       ! Is this an optional routine?
      parameter (Allow_zero_numvals = .true.)
*
      character  my_name*(*)           ! Name of subroutine
      parameter (my_name = 'Collect_logical_array_optional')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call Respond2Post_logical_array
     .   (array_name, size_of, Units, Array, Numvals,
     .    Allow_zero_numvals, 1)

      call pop_routine (my_name)
      return
      end subroutine



* ====================================================================
       subroutine Collect_logical_var (
     :                                variable_name, units
     :                              , variable, numvals)
* ====================================================================
      Use ErrorModule
      implicit none

*+ Sub-Program Arguments
      character  Variable_name*(*)     ! (INPUT) Variable name to search for
      character  Units*(*)             ! (INPUT) Units required by caller
      logical    variable              ! (OUTPUT) Variable returned to caller
      integer    numvals               ! (OUTPUT) Number of values returned

*+ Purpose
*   Respond to the 'posting' of the given data type by retrieving data
*   from the postbox for the given variable name.

*+  Mission Statement
*     Get %3 from the message

*+ Changes
*     050696 nih programmed and specified

*+ Calls

*+ Constant Values
      character  my_name*(*)           ! Name of subroutine
      parameter (my_name = 'Collect_logical_var')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call Respond2set_logical_var
     .    (Variable_name, Units, Variable, numvals)

      call pop_routine (my_name)
      return
      end subroutine



* ====================================================================
       subroutine Collect_logical_var_optional (
     :                                variable_name, units
     :                              , variable, numvals)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none

*+ Sub-Program Arguments
      character  Variable_name*(*)     ! (INPUT) Variable name to search for
      character  Units*(*)             ! (INPUT) Units required by caller
      logical    variable              ! (OUTPUT) Variable returned to caller
      integer    numvals               ! (OUTPUT) Number of values returned

*+ Purpose
*   Respond to the 'posting' of the given data type by retrieving data
*   from the postbox for the given variable name.

*+  Mission Statement
*     Get %3 from the message |ITALIC{(OPTIONAL)}

*+ Changes
*     050696 nih programmed and specified

*+ Calls

*+ Constant Values
      logical Allow_zero_numvals       ! Is this an optional routine?
      parameter (Allow_zero_numvals = .true.)
*
      character  my_name*(*)           ! Name of subroutine
      parameter (my_name = 'Collect_logical_var_optional')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call Respond2Post_logical_var
     .   (Variable_name, Units, Variable, Numvals,
     .    Allow_zero_numvals, 1)

      call pop_routine (my_name)
      return
      end subroutine



* ====================================================================
       subroutine Collect_real_array (
     :                                array_name, size_of, units
     :                              , array, numvals
     :                              , lower_limit, upper_limit)
* ====================================================================
      Use ErrorModule
      implicit none

*+ Sub-Program Arguments
      character  array_name*(*)        ! (INPUT) array name to search for
      integer    size_of               ! (INPUT) size_of of array
      character  Units*(*)             ! (INPUT) Units required by caller
      real       array(*)              ! (OUTPUT) array returned to caller
      integer    numvals               ! (OUTPUT) Number of values returned
      real       Lower_limit           ! (INPUT) Lower limit for bounds check
      real       Upper_limit           ! (INPUT) Upper limit for bounds check

*+ Purpose
*   Respond to the 'posting' of the given data type by retrieving data
*   from the postbox for the given variable name.

*+  Mission Statement
*     Get %4 from the message (Lower Bound = %6, Upper Bound = %7)

*+ Changes
*     050696 nih programmed and specified

*+ Calls

*+ Constant Values
      character  my_name*(*)           ! Name of subroutine
      parameter (my_name = 'Collect_real_array')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call Respond2set_real_array
     .    (Array_name, size_of, Units, array, numvals,
     .     Lower_limit, Upper_limit)

      call pop_routine (my_name)
      return
      end subroutine



* ====================================================================
       subroutine Collect_real_array_optional (
     :                                array_name, size_of, units
     :                              , array, numvals
     :                              , lower_limit, upper_limit)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none

*+ Sub-Program Arguments
      character  array_name*(*)        ! (INPUT) array name to search for
      integer    size_of               ! (INPUT) size_of of array
      character  Units*(*)             ! (INPUT) Units required by caller
      real       array(*)              ! (OUTPUT) array returned to caller
      integer    numvals               ! (OUTPUT) Number of values returned
      real       Lower_limit           ! (INPUT) Lower limit for bounds check
      real       Upper_limit           ! (INPUT) Upper limit for bounds check

*+ Purpose
*   Respond to the 'posting' of the given data type by retrieving data
*   from the postbox for the given variable name.

*+  Mission Statement
*     Get %4 from the message (Lower Bound = %6, Upper Bound = %7)|ITALIC{(OPTIONAL)}

*+ Changes
*     050696 nih programmed and specified

*+ Calls

*+ Constant Values
      logical Allow_zero_numvals       ! Is this an optional routine?
      parameter (Allow_zero_numvals = .true.)
*
      character  my_name*(*)           ! Name of subroutine
      parameter (my_name = 'Collect_real_array_optional')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call Respond2Post_real_array
     .   (array_name, size_of, Units, Array, Numvals,
     .    Lower_limit, Upper_limit,
     .    Allow_zero_numvals, 1)

      call pop_routine (my_name)
      return
      end subroutine



* ====================================================================
       subroutine Collect_real_var (
     :                                variable_name, units
     :                              , variable, numvals
     :                              , lower_limit, upper_limit)
* ====================================================================
      Use ErrorModule
      implicit none

*+ Sub-Program Arguments
      character  Variable_name*(*)     ! (INPUT) Variable name to search for
      character  Units*(*)             ! (INPUT) Units required by caller
      real       variable              ! (OUTPUT) Variable returned to caller
      integer    numvals               ! (OUTPUT) Number of values returned
      real       Lower_limit           ! (INPUT) Lower limit for bounds check
      real       Upper_limit           ! (INPUT) Upper limit for bounds check

*+ Purpose
*   Respond to the 'posting' of the given data type by retrieving data
*   from the postbox for the given variable name.

*+  Mission Statement
*     Get %3 from the message (Lower Bound = %5, Upper Bound = %6)

*+ Changes
*     050696 nih programmed and specified

*+ Calls

*+ Constant Values
      character  my_name*(*)           ! Name of subroutine
      parameter (my_name = 'Collect_real_var')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call Respond2set_real_var
     .    (Variable_name, Units, variable, numvals,
     .     Lower_limit, Upper_limit)

      call pop_routine (my_name)
      return
      end subroutine



* ====================================================================
       subroutine Collect_real_var_optional (
     :                                variable_name, units
     :                              , variable, numvals
     :                              , lower_limit, upper_limit)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none

*+ Sub-Program Arguments
      character  Variable_name*(*)     ! (INPUT) Variable name to search for
      character  Units*(*)             ! (INPUT) Units required by caller
      real       variable              ! (OUTPUT) Variable returned to caller
      integer    numvals               ! (OUTPUT) Number of values returned
      real       Lower_limit           ! (INPUT) Lower limit for bounds check
      real       Upper_limit           ! (INPUT) Upper limit for bounds check

*+ Purpose
*   Respond to the 'posting' of the given data type by retrieving data
*   from the postbox for the given variable name.

*+  Mission Statement
*     Get %3 from the message (Lower Bound = %5, Upper Bound = %6)|ITALIC{(OPTIONAL)}

*+ Changes
*     050696 nih programmed and specified

*+ Calls

*+ Constant Values
      logical Allow_zero_numvals       ! Is this an optional routine?
      parameter (Allow_zero_numvals = .true.)
*
      character  my_name*(*)           ! Name of subroutine
      parameter (my_name = 'Collect_real_var_optional')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call Respond2Post_real_var
     .   (Variable_name, Units, Variable, Numvals,
     .    Lower_limit, Upper_limit,
     .    Allow_zero_numvals, 1)

      call pop_routine (my_name)
      return
      end subroutine



* ====================================================================
       subroutine get_char_array
     .   (owner_module_name, variable_name, Array_size,
     .    units, variable, numvals)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none

*+ Sub-Program Arguments
      character Owner_module_name*(*)  ! (INPUT) Owner module name
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      integer Array_size               ! (INPUT) Size of array
      character Units*(*)              ! (INPUT) Units required by caller
      character Variable(*)*(*)        ! (OUTPUT) Variable returned to caller
      integer numvals                  ! (OUTPUT) Number of values returned

*+ Purpose
*     High level routine to get a value from another module.
*     This routine also checks the bounds of the returned variable.
*     Fatal_error will be called if any err is encountered.

*+  Mission Statement
*     Get %5 from the system

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines

*+ Calls

*+ Constant Values
      logical No_zero_numvals          ! Is this an optional routine?
      parameter (No_zero_numvals = .false.)
*
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='get_char_array')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      call New_postbox ()
      call Deliver_get_message (Owner_module_name,
     .                          Variable_name, .false.)

      call Respond2Post_char_array
     .   (Variable_name, Array_size, Units, Variable, Numvals,
     .    No_zero_numvals, 1)
      call Delete_postbox ()

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine get_char_array_optional
     .   (owner_module_name, variable_name, Array_size,
     .    units, variable, numvals)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none

*+ Sub-Program Arguments
      character Owner_module_name*(*)  ! (INPUT) Owner module name
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      integer Array_size               ! (INPUT) Size of array
      character Units*(*)              ! (INPUT) Units required by caller
      character Variable(*)*(*)        ! (OUTPUT) Variable returned to caller
      integer numvals                  ! (OUTPUT) Number of values returned

*+ Purpose
*     High level routine to get a value from another module.
*     This routine also checks the bounds of the returned variable.
*     Fatal_error will be called if any err is encountered.

*+  Mission Statement
*     Get %5 from the system |ITALIC{(OPTIONAL)}

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines
*     dph 24/9/96  Added test to make sure module_name is an active one.
*     dph 26/7/99  removed call to is_active_module.  All modules are now active

*+ Calls

*+ Constant Values
      logical Allow_zero_numvals          ! Is this an optional routine?
      parameter (Allow_zero_numvals = .true.)
*
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='get_char_array_optional')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

         call New_postbox ()
         call Deliver_get_message (Owner_module_name,
     .                             Variable_name, .true.)

         call Respond2Post_char_array
     .      (Variable_name, Array_size, Units, Variable, Numvals,
     .       Allow_zero_numvals, 1)
         call Delete_postbox ()

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine get_char_arrays
     .   (request_no, variable_name, Array_size,
     .    units, variable, numvals)
* ====================================================================
      use ConstantsModule
      Use ErrorModule
      use PostboxModule
      implicit none

*+ Sub-Program Arguments
      integer Request_no               ! (INPUT) Owner module name
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      integer Array_size               ! (INPUT) Size of array
      character Units*(*)              ! (INPUT) Units required by caller
      character Variable(*)*(*)        ! (OUTPUT) Variable returned to caller
      integer numvals                  ! (OUTPUT) Number of values returned

*+ Purpose
*     High level routine to get a value from another module.
*     This routine also checks the bounds of the returned variable.
*     Fatal_error will be called if any err is encountered.

*+  Mission Statement
*     Get next %5 from the system

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines

*+ Calls

*+ Constant Values
      logical Allow_zero_numvals          ! Is this an optional routine?
      parameter (Allow_zero_numvals = .true.)
*
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='get_char_arrays')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      ! Only send a message out if this is first call

      if (Request_no .eq. 1) then
         call New_postbox ()
         call Deliver_get_message (All_active_modules,
     .                             Variable_name, .false.)
      endif

      call Respond2Post_char_array
     .   (Variable_name, Array_size, Units, Variable, Numvals,
     .    Allow_zero_numvals, Request_no)

      if (Numvals .eq. 0) then
         call Delete_postbox ()
      endif

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine get_char_var
     .   (owner_module_name, variable_name,
     .    units, variable, numvals)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none

*+ Sub-Program Arguments
      character Owner_module_name*(*)  ! (INPUT) Owner module name
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      character Units*(*)              ! (INPUT) Units required by caller
      character Variable*(*)           ! (OUTPUT) Variable returned to caller
      integer numvals                  ! (OUTPUT) Number of values returned

*+ Purpose
*     High level routine to get a value from another module.
*     This routine also checks the bounds of the returned variable.
*     Fatal_error will be called if any err is encountered.

*+  Mission Statement
*     Get %4 from the system |ITALIC{(OPTIONAL)}

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines

*+ Calls

*+ Constant Values
      logical No_zero_numvals          ! Is this an optional routine?
      parameter (No_zero_numvals = .false.)
*
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='get_char_var')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      call New_postbox ()
      call Deliver_get_message (Owner_module_name,
     .                          Variable_name, .false.)

      call Respond2Post_char_var
     .   (Variable_name, Units, Variable, Numvals, No_zero_numvals, 1)
      call Delete_postbox ()

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine get_char_var_optional
     .   (owner_module_name, variable_name,
     .    units, variable, numvals)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none

*+ Sub-Program Arguments
      character Owner_module_name*(*)  ! (INPUT) Owner module name
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      character Units*(*)              ! (INPUT) Units required by caller
      character Variable*(*)           ! (OUTPUT) Variable returned to caller
      integer numvals                  ! (OUTPUT) Number of values returned

*+ Purpose
*     High level routine to get a value from another module.
*     This routine also checks the bounds of the returned variable.
*     Fatal_error will be called if any err is encountered.

*+  Mission Statement
*     Get %4 from the system |ITALIC{(OPTIONAL)}

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines
*     dph 24/9/96  Added test to make sure module_name is an active one.
*     dph 26/7/99  removed call to is_active_module.  All modules are now active

*+ Calls

*+ Constant Values
      logical Allow_zero_numvals          ! Is this an optional routine?
      parameter (Allow_zero_numvals = .true.)
*
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='get_char_var_optional')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

         call New_postbox ()
         call Deliver_get_message (Owner_module_name,
     .                             Variable_name, .true.)

         call Respond2Post_char_var
     .      (Variable_name, Units, Variable, Numvals,
     .       Allow_zero_numvals, 1)
         call Delete_postbox ()

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine get_char_vars
     .   (request_no, variable_name,
     .    units, variable, numvals)
* ====================================================================
      use ConstantsModule
      Use ErrorModule
      use PostboxModule
      implicit none

*+ Sub-Program Arguments
      integer Request_no               ! (INPUT) Owner module name
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      character Units*(*)              ! (INPUT) Units required by caller
      character Variable*(*)           ! (OUTPUT) Variable returned to caller
      integer numvals                  ! (OUTPUT) Number of values returned

*+ Purpose
*     High level routine to get a value from another module.
*     This routine also checks the bounds of the returned variable.
*     Fatal_error will be called if any err is encountered.

*+  Mission Statement
*     Get next %4 from the system

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines

*+ Calls

*+ Constant Values
      logical Allow_zero_numvals          ! Is this an optional routine?
      parameter (Allow_zero_numvals = .true.)
*
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='get_char_vars')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      ! Only send a message out if this is first call

      if (Request_no .eq. 1) then
         call New_postbox ()
         call Deliver_get_message (All_active_modules,
     .                             Variable_name, .false.)
      endif

      call Respond2Post_char_var
     .   (Variable_name, Units, Variable, Numvals,
     .    Allow_Zero_numvals, Request_no)

      if (Numvals .eq. 0) then
         call Delete_postbox ()
      endif

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine get_double_array
     .   (owner_module_name, variable_name, Array_size,
     .    units, variable, numvals,
     .    lower_limit, upper_limit)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none

*+ Sub-Program Arguments
      character Owner_module_name*(*)  ! (INPUT) Owner module name
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      integer Array_size               ! (INPUT) Size of array
      character Units*(*)              ! (INPUT) Units required by caller
      double precision Variable(*)     ! (OUTPUT) Variable returned to caller
      integer numvals                  ! (OUTPUT) Number of values returned
      double precision Lower_limit     ! (INPUT) Lower limit for bounds check
      double precision Upper_limit     ! (INPUT) Upper limit for bounds check

*+ Purpose
*     High level routine to get a value from another module.
*     This routine also checks the bounds of the returned variable.
*     Fatal_error will be called if any err is encountered.

*+  Mission Statement
*     Get %5 from the system (Lower Bound = %7, Upper Bound = %8)

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines

*+ Calls

*+ Constant Values
      logical No_zero_numvals          ! Is this an optional routine?
      parameter (No_zero_numvals = .false.)
*
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='get_double_array')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      call New_postbox ()
      call Deliver_get_message (Owner_module_name,
     .                          Variable_name, .false.)

      call Respond2Post_double_array
     .   (Variable_name, Array_size, Units, Variable, Numvals,
     .    Lower_limit, Upper_limit, No_zero_numvals, 1)
      call Delete_postbox ()

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine get_double_array_optional
     .   (owner_module_name, variable_name, Array_size,
     .    units, variable, numvals,
     .    lower_limit, upper_limit)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none

*+ Sub-Program Arguments
      character Owner_module_name*(*)  ! (INPUT) Owner module name
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      integer Array_size               ! (INPUT) Size of array
      character Units*(*)              ! (INPUT) Units required by caller
      double precision Variable(*)     ! (OUTPUT) Variable returned to caller
      integer numvals                  ! (OUTPUT) Number of values returned
      double precision Lower_limit     ! (INPUT) Lower limit for bounds check
      double precision Upper_limit     ! (INPUT) Upper limit for bounds check

*+ Purpose
*     High level routine to get a value from another module.
*     This routine also checks the bounds of the returned variable.
*     Fatal_error will be called if any err is encountered.

*+  Mission Statement
*     Get %5 from the system (Lower Bound = %7, Upper Bound = %8)|ITALIC{(OPTIONAL)}

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines
*     dph 24/9/96  Added test to make sure module_name is an active one.
*     dph 26/7/99  removed call to is_active_module.  All modules are now active

*+ Calls

*+ Constant Values
      logical Allow_zero_numvals          ! Is this an optional routine?
      parameter (Allow_zero_numvals = .true.)
*
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='get_double_array_optional')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

         call New_postbox ()
         call Deliver_get_message (Owner_module_name,
     .                             Variable_name, .true.)

         call Respond2Post_double_array
     .      (Variable_name, Array_size, Units, Variable, Numvals,
     .       Lower_limit, Upper_limit, Allow_zero_numvals, 1)
         call Delete_postbox ()

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine get_double_arrays
     .   (request_no, variable_name, Array_size,
     .    units, variable, numvals,
     .    lower_limit, upper_limit)
* ====================================================================
      use ConstantsModule
      Use ErrorModule
      use PostboxModule
      implicit none

*+ Sub-Program Arguments
      integer Request_no               ! (INPUT) Owner module name
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      integer Array_size               ! (INPUT) Size of array
      character Units*(*)              ! (INPUT) Units required by caller
      double precision Variable(*)     ! (OUTPUT) Variable returned to caller
      integer numvals                  ! (OUTPUT) Number of values returned
      double precision Lower_limit     ! (INPUT) Lower limit for bounds check
      double precision Upper_limit     ! (INPUT) Upper limit for bounds check

*+ Purpose
*     High level routine to get a value from another module.
*     This routine also checks the bounds of the returned variable.
*     Fatal_error will be called if any err is encountered.

*+  Mission Statement
*     Get next %5 from the system (Lower Bound = %7, Upper Bound = %8)

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines

*+ Calls

*+ Constant Values
      logical Allow_zero_numvals          ! Is this an optional routine?
      parameter (Allow_zero_numvals = .true.)
*
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='get_double_arrays')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)
        ! Only send a message out if this is first call

      if (Request_no .eq. 1) then
         call New_postbox ()
         call Deliver_get_message (All_active_modules,
     .                             Variable_name, .false.)
      endif


      call Respond2Post_double_array
     .   (Variable_name, Array_size, Units, Variable, Numvals,
     .    Lower_limit, Upper_limit, Allow_zero_numvals, Request_no)

      if (Numvals .eq. 0) then
         call Delete_postbox ()
      endif

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine get_double_var
     .   (owner_module_name, variable_name,
     .    units, variable, numvals,
     .    lower_limit, upper_limit)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none

*+ Sub-Program Arguments
      character Owner_module_name*(*)  ! (INPUT) Owner module name
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      character Units*(*)              ! (INPUT) Units required by caller
      double precision Variable        ! (OUTPUT) Variable returned to caller
      integer numvals                  ! (OUTPUT) Number of values returned
      double precision Lower_limit     ! (INPUT) Lower limit for bounds check
      double precision Upper_limit     ! (INPUT) Upper limit for bounds check

*+ Purpose
*     High level routine to get a value from another module.
*     This routine also checks the bounds of the returned variable.
*     Fatal_error will be called if any err is encountered.

*+  Mission Statement
*     Get %4 from the system (Lower Bound = %6, Upper Bound = %7)

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines

*+ Calls

*+ Constant Values
      logical No_zero_numvals          ! Is this an optional routine?
      parameter (No_zero_numvals = .false.)
*
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='get_double_var')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      call New_postbox ()
      call Deliver_get_message (Owner_module_name,
     .                          Variable_name, .false.)

      call Respond2Post_double_var
     .   (Variable_name, Units, Variable, Numvals,
     .    Lower_limit, Upper_limit, No_zero_numvals, 1)

      call Delete_postbox ()

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine get_double_var_optional
     .   (owner_module_name, variable_name,
     .    units, variable, numvals,
     .    lower_limit, upper_limit)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none

*+ Sub-Program Arguments
      character Owner_module_name*(*)  ! (INPUT) Owner module name
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      character Units*(*)              ! (INPUT) Units required by caller
      double precision Variable        ! (OUTPUT) Variable returned to caller
      integer numvals                  ! (OUTPUT) Number of values returned
      double precision Lower_limit     ! (INPUT) Lower limit for bounds check
      double precision Upper_limit     ! (INPUT) Upper limit for bounds check

*+ Purpose
*     High level routine to get a value from another module.
*     This routine also checks the bounds of the returned variable.
*     Fatal_error will be called if any err is encountered.

*+  Mission Statement
*     Get %4 from the system (Lower Bound = %6, Upper Bound = %7)|ITALIC{(OPTIONAL)}

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines
*     dph 24/9/96  Added test to make sure module_name is an active one.
*     dph 26/7/99  removed call to is_active_module.  All modules are now active

*+ Calls

*+ Constant Values
      logical Allow_zero_numvals          ! Is this an optional routine?
      parameter (Allow_zero_numvals = .true.)
*
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='get_double_var_optional')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

         call New_postbox ()
         call Deliver_get_message (Owner_module_name,
     .                             Variable_name, .true.)

         call Respond2Post_double_var
     .      (Variable_name, Units, Variable, Numvals,
     .       Lower_limit, Upper_limit, Allow_zero_numvals, 1)
         call Delete_postbox ()

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine get_double_vars
     .   (request_no, variable_name,
     .    units, variable, numvals,
     .    lower_limit, upper_limit)
* ====================================================================
      use ConstantsModule
      Use ErrorModule
      use PostboxModule
      implicit none

*+ Sub-Program Arguments
      integer Request_no               ! (INPUT) Owner module name
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      character Units*(*)              ! (INPUT) Units required by caller
      double precision Variable        ! (OUTPUT) Variable returned to caller
      integer numvals                  ! (OUTPUT) Number of values returned
      double precision Lower_limit     ! (INPUT) Lower limit for bounds check
      double precision Upper_limit     ! (INPUT) Upper limit for bounds check

*+ Purpose
*     High level routine to get a value from another module.
*     This routine also checks the bounds of the returned variable.
*     Fatal_error will be called if any err is encountered.

*+  Mission Statement
*     Get next%4 from the system (Lower Bound = %6, Upper Bound = %7)

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines

*+ Calls

*+ Constant Values
      logical Allow_zero_numvals          ! Is this an optional routine?
      parameter (Allow_zero_numvals = .true.)
*
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='get_double_vars')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      ! Only send a message out if this is first call

      if (Request_no .eq. 1) then
         call New_postbox ()
         call Deliver_get_message (All_active_modules,
     .                             Variable_name, .false.)
      endif

      call Respond2Post_double_var
     .   (Variable_name, Units, Variable, Numvals,
     .    Lower_limit, Upper_limit, Allow_Zero_numvals, Request_no)

      if (Numvals .eq. 0) then
         call Delete_postbox ()
      endif

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine get_integer_array
     .   (owner_module_name, variable_name, Array_size,
     .    units, variable, numvals,
     .    lower_limit, upper_limit)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none

*+ Sub-Program Arguments
      character Owner_module_name*(*)  ! (INPUT) Owner module name
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      integer Array_size               ! (INPUT) Size of array
      character Units*(*)              ! (INPUT) Units required by caller
      integer Variable(*)              ! (OUTPUT) Variable returned to caller
      integer numvals                  ! (OUTPUT) Number of values returned
      integer Lower_limit              ! (INPUT) Lower limit for bounds check
      integer Upper_limit              ! (INPUT) Upper limit for bounds check

*+ Purpose
*     High level routine to get a value from another module.
*     This routine also checks the bounds of the returned variable.
*     Fatal_error will be called if any err is encountered.

*+  Mission Statement
*     Get %5 from the system (Lower Bound = %7, Upper Bound = %8)

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines

*+ Calls

*+ Constant Values
      logical No_zero_numvals          ! Is this an optional routine?
      parameter (No_zero_numvals = .false.)
*
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='get_integer_array')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      call New_postbox ()
      call Deliver_get_message (Owner_module_name,
     .                          Variable_name, .false.)

      call Respond2Post_integer_array
     .   (Variable_name, Array_size, Units, Variable, Numvals,
     .    Lower_limit, Upper_limit, No_zero_numvals, 1)
      call Delete_postbox ()

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine get_integer_array_optional
     .   (owner_module_name, variable_name, Array_size,
     .    units, variable, numvals,
     .    lower_limit, upper_limit)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none

*+ Sub-Program Arguments
      character Owner_module_name*(*)  ! (INPUT) Owner module name
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      integer Array_size               ! (INPUT) Size of array
      character Units*(*)              ! (INPUT) Units required by caller
      integer Variable(*)              ! (OUTPUT) Variable returned to caller
      integer numvals                  ! (OUTPUT) Number of values returned
      integer Lower_limit              ! (INPUT) Lower limit for bounds check
      integer Upper_limit              ! (INPUT) Upper limit for bounds check

*+ Purpose
*     High level routine to get a value from another module.
*     This routine also checks the bounds of the returned variable.
*     Fatal_error will be called if any err is encountered.

*+  Mission Statement
*     Get %5 from the system (Lower Bound = %7, Upper Bound = %8)|ITALIC{(OPTIONAL)}

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines
*     dph 24/9/96  Added test to make sure module_name is an active one.
*     dph 26/7/99  removed call to is_active_module.  All modules are now active

*+ Calls

*+ Constant Values
      logical Allow_zero_numvals          ! Is this an optional routine?
      parameter (Allow_zero_numvals = .true.)
*
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='get_integer_array_optional')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

         call New_postbox ()
         call Deliver_get_message (Owner_module_name,
     .                             Variable_name, .true.)

         call Respond2Post_integer_array
     .      (Variable_name, Array_size, Units, Variable, Numvals,
     .       Lower_limit, Upper_limit, Allow_zero_numvals, 1)
         call Delete_postbox ()

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine get_integer_arrays
     .   (request_no, variable_name, Array_size,
     .    units, variable, numvals,
     .    lower_limit, upper_limit)
* ====================================================================
      use ConstantsModule
      Use ErrorModule
      use PostboxModule
      implicit none

*+ Sub-Program Arguments
      integer Request_no               ! (INPUT) Owner module name
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      integer Array_size               ! (INPUT) Size of array
      character Units*(*)              ! (INPUT) Units required by caller
      integer Variable(*)              ! (OUTPUT) Variable returned to caller
      integer numvals                  ! (OUTPUT) Number of values returned
      integer Lower_limit              ! (INPUT) Lower limit for bounds check
      integer Upper_limit              ! (INPUT) Upper limit for bounds check

*+ Purpose
*     High level routine to get a value from another module.
*     This routine also checks the bounds of the returned variable.
*     Fatal_error will be called if any err is encountered.

*+  Mission Statement
*     Get next %5 from the system (Lower Bound = %7, Upper Bound = %8)

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines

*+ Calls

*+ Constant Values
      logical Allow_zero_numvals          ! Is this an optional routine?
      parameter (Allow_zero_numvals = .true.)
*
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='get_integer_arrays')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      ! Only send a message out if this is first call

      if (Request_no .eq. 1) then
         call New_postbox ()
         call Deliver_get_message (All_active_modules,
     .                             Variable_name, .false.)
      endif

      call Respond2Post_integer_array
     .   (Variable_name, Array_size, Units, Variable, Numvals,
     .    Lower_limit, Upper_limit, Allow_zero_numvals, Request_no)

      if (Numvals .eq. 0) then
         call Delete_postbox ()
      endif

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine get_integer_var
     .   (owner_module_name, variable_name,
     .    units, variable, numvals,
     .    lower_limit, upper_limit)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none

*+ Sub-Program Arguments
      character Owner_module_name*(*)  ! (INPUT) Owner module name
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      character Units*(*)              ! (INPUT) Units required by caller
      integer Variable                 ! (OUTPUT) Variable returned to caller
      integer numvals                  ! (OUTPUT) Number of values returned
      integer Lower_limit              ! (INPUT) Lower limit for bounds check
      integer Upper_limit              ! (INPUT) Upper limit for bounds check

*+ Purpose
*     High level routine to get a value from another module.
*     This routine also checks the bounds of the returned variable.
*     Fatal_error will be called if any err is encountered.

*+  Mission Statement
*     Get %4 from the system (Lower Bound = %6, Upper Bound = %7)

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines

*+ Calls

*+ Constant Values
      logical No_zero_numvals          ! Is this an optional routine?
      parameter (No_zero_numvals = .false.)
*
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='get_integer_var')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      call New_postbox ()
      call Deliver_get_message (Owner_module_name,
     .                          Variable_name, .false.)

      call Respond2Post_integer_var
     .   (Variable_name, Units, Variable, Numvals,
     .    Lower_limit, Upper_limit, No_zero_numvals, 1)


      call Delete_postbox ()

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine get_integer_var_optional
     .   (owner_module_name, variable_name,
     .    units, variable, numvals,
     .    lower_limit, upper_limit)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none

*+ Sub-Program Arguments
      character Owner_module_name*(*)  ! (INPUT) Owner module name
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      character Units*(*)              ! (INPUT) Units required by caller
      integer Variable                 ! (OUTPUT) Variable returned to caller
      integer numvals                  ! (OUTPUT) Number of values returned
      integer Lower_limit              ! (INPUT) Lower limit for bounds check
      integer Upper_limit              ! (INPUT) Upper limit for bounds check

*+ Purpose
*     High level routine to get a value from another module.
*     This routine also checks the bounds of the returned variable.
*     Fatal_error will be called if any err is encountered.

*+  Mission Statement
*     Get %4 from the system (Lower Bound = %6, Upper Bound = %7)|ITALIC{(OPTIONAL)}

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines
*     dph 24/9/96  Added test to make sure module_name is an active one.
*     dph 26/7/99  removed call to is_active_module.  All modules are now active

*+ Calls

*+ Constant Values
      logical Allow_zero_numvals          ! Is this an optional routine?
      parameter (Allow_zero_numvals = .true.)
*
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='get_integer_var_optional')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

         call New_postbox ()
         call Deliver_get_message (Owner_module_name,
     .                             Variable_name, .true.)

         call Respond2Post_integer_var
     .      (Variable_name, Units, Variable, Numvals,
     .       Lower_limit, Upper_limit, Allow_zero_numvals, 1)
         call Delete_postbox ()

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine get_integer_vars
     .   (request_no, variable_name,
     .    units, variable, numvals,
     .    lower_limit, upper_limit)
* ====================================================================
      use ConstantsModule
      Use ErrorModule
      use PostboxModule
      implicit none

*+ Sub-Program Arguments
      integer Request_no               ! (INPUT) Owner module name
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      character Units*(*)              ! (INPUT) Units required by caller
      integer Variable                 ! (OUTPUT) Variable returned to caller
      integer numvals                  ! (OUTPUT) Number of values returned
      integer Lower_limit              ! (INPUT) Lower limit for bounds check
      integer Upper_limit              ! (INPUT) Upper limit for bounds check

*+ Purpose
*     High level routine to get a value from another module.
*     This routine also checks the bounds of the returned variable.
*     Fatal_error will be called if any err is encountered.

*+  Mission Statement
*     Get next %4 from the system (Lower Bound = %6, Upper Bound = %7)

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines

*+ Calls

*+ Constant Values
      logical Allow_zero_numvals          ! Is this an optional routine?
      parameter (Allow_zero_numvals = .true.)
*
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='get_integer_vars')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      ! Only send a message out if this is first call

      if (Request_no .eq. 1) then
         call New_postbox ()
         call Deliver_get_message (All_active_modules,
     .                             Variable_name, .false.)
      endif

      call Respond2Post_integer_var
     .   (Variable_name, Units, Variable, Numvals,
     .    Lower_limit, Upper_limit, Allow_Zero_numvals, Request_no)

      if (Numvals .eq. 0) then
         call Delete_postbox ()
      endif

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine get_logical_array
     .   (owner_module_name, variable_name, Array_size,
     .    units, variable, numvals)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none


*+ Sub-Program Arguments
      character Owner_module_name*(*)  ! (INPUT) Owner module name
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      integer Array_size               ! (INPUT) Size of array
      character Units*(*)              ! (INPUT) Units required by caller
      logical Variable(*)              ! (OUTPUT) Variable returned to caller
      integer numvals                  ! (OUTPUT) Number of values returned

*+ Purpose
*     High level routine to get a value from another module.
*     This routine also checks the bounds of the returned variable.
*     Fatal_error will be called if any err is encountered.

*+  Mission Statement
*

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines

*+ Calls

*+ Constant Values
      logical No_zero_numvals          ! Is this an optional routine?
      parameter (No_zero_numvals = .false.)
*
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='get_logical_array')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      call New_postbox ()
      call Deliver_get_message (Owner_module_name,
     .                          Variable_name, .false.)

      call Respond2Post_logical_array
     .   (Variable_name, Array_size, Units, Variable, Numvals,
     .    No_zero_numvals, 1)
      call Delete_postbox ()

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine get_logical_array_optional
     .   (owner_module_name, variable_name, Array_size,
     .    units, variable, numvals)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none


*+ Sub-Program Arguments
      character Owner_module_name*(*)  ! (INPUT) Owner module name
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      integer Array_size               ! (INPUT) Size of array
      character Units*(*)              ! (INPUT) Units required by caller
      logical Variable(*)              ! (OUTPUT) Variable returned to caller
      integer numvals                  ! (OUTPUT) Number of values returned

*+ Purpose
*     High level routine to get a value from another module.
*     This routine also checks the bounds of the returned variable.
*     Fatal_error will be called if any err is encountered.

*+  Mission Statement
*

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines
*     dph 24/9/96  Added test to make sure module_name is an active one.
*     dph 26/7/99  removed call to is_active_module.  All modules are now active

*+ Calls

*+ Constant Values
      logical Allow_zero_numvals          ! Is this an optional routine?
      parameter (Allow_zero_numvals = .true.)
*
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='get_logical_array_optional')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

         call New_postbox ()
         call Deliver_get_message (Owner_module_name,
     .                             Variable_name, .true.)

         call Respond2Post_logical_array
     .       (Variable_name, Array_size, Units, Variable, Numvals,
     .       Allow_zero_numvals, 1)
         call Delete_postbox ()

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine get_logical_arrays
     .   (request_no, variable_name, Array_size,
     .    units, variable, numvals)
* ====================================================================
      use ConstantsModule
      Use ErrorModule
      use PostboxModule
      implicit none

*+ Sub-Program Arguments
      integer Request_no               ! (INPUT) Owner module name
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      integer Array_size               ! (INPUT) Size of array
      character Units*(*)              ! (INPUT) Units required by caller
      logical Variable(*)              ! (OUTPUT) Variable returned to caller
      integer numvals                  ! (OUTPUT) Number of values returned

*+ Purpose
*     High level routine to get a value from another module.
*     This routine also checks the bounds of the returned variable.
*     Fatal_error will be called if any err is encountered.

*+  Mission Statement
*

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines

*+ Calls

*+ Constant Values
      logical Allow_zero_numvals          ! Is this an optional routine?
      parameter (Allow_zero_numvals = .true.)
*
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='get_logical_arrays')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      ! Only send a message out if this is first call

      if (Request_no .eq. 1) then
         call New_postbox ()
         call Deliver_get_message (All_active_modules,
     .                             Variable_name, .false.)
      endif

      call Respond2Post_logical_array
     .   (Variable_name, Array_size, Units, Variable, Numvals,
     .    Allow_zero_numvals, Request_no)

      if (Numvals .eq. 0) then
         call Delete_postbox ()
      endif

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine get_logical_var
     .   (owner_module_name, variable_name,
     .    units, variable, numvals)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none


*+ Sub-Program Arguments
      character Owner_module_name*(*)  ! (INPUT) Owner module name
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      character Units*(*)              ! (INPUT) Units required by caller
      logical Variable                 ! (OUTPUT) Variable returned to caller
      integer numvals                  ! (OUTPUT) Number of values returned

*+ Purpose
*     High level routine to get a value from another module.
*     This routine also checks the bounds of the returned variable.
*     Fatal_error will be called if any err is encountered.

*+  Mission Statement
*

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines

*+ Calls

*+ Constant Values
      logical No_zero_numvals          ! Is this an optional routine?
      parameter (No_zero_numvals = .false.)
*
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='get_logical_var')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      call New_postbox ()
      call Deliver_get_message (Owner_module_name,
     .                          Variable_name, .false.)

      call Respond2Post_logical_var
     .   (Variable_name, Units, Variable, Numvals, No_zero_numvals, 1)
      call Delete_postbox ()

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine get_logical_var_optional
     .   (owner_module_name, variable_name,
     .    units, variable, numvals)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none


*+ Sub-Program Arguments
      character Owner_module_name*(*)  ! (INPUT) Owner module name
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      character Units*(*)              ! (INPUT) Units required by caller
      logical Variable                 ! (OUTPUT) Variable returned to caller
      integer numvals                  ! (OUTPUT) Number of values returned

*+ Purpose
*     High level routine to get a value from another module.
*     This routine also checks the bounds of the returned variable.
*     Fatal_error will be called if any err is encountered.

*+  Mission Statement
*

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines
*     dph 24/9/96  Added test to make sure module_name is an active one.
*     dph 26/7/99  removed call to is_active_module.  All modules are now active

*+ Calls

*+ Constant Values
      logical Allow_zero_numvals          ! Is this an optional routine?
      parameter (Allow_zero_numvals = .true.)
*
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='get_logical_var_optional')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

         call New_postbox ()
         call Deliver_get_message (Owner_module_name,
     .                             Variable_name, .true.)

         call Respond2Post_logical_var
     .      (Variable_name, Units, Variable, Numvals,
     .       Allow_zero_numvals, 1)
         call Delete_postbox ()

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine get_logical_vars
     .   (request_no, variable_name,
     .    units, variable, numvals)
* ====================================================================
      use ConstantsModule
      Use ErrorModule
      use PostboxModule
      implicit none

*+ Sub-Program Arguments
      integer Request_no               ! (INPUT) Owner module name
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      character Units*(*)              ! (INPUT) Units required by caller
      logical Variable                 ! (OUTPUT) Variable returned to caller
      integer numvals                  ! (OUTPUT) Number of values returned

*+ Purpose
*     High level routine to get a value from another module.
*     This routine also checks the bounds of the returned variable.
*     Fatal_error will be called if any err is encountered.

*+  Mission Statement
*

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines

*+ Calls

*+ Constant Values
      logical Allow_zero_numvals          ! Is this an optional routine?
      parameter (Allow_zero_numvals = .true.)
*
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='get_logical_vars')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      ! Only send a message out if this is first call

      if (Request_no .eq. 1) then
         call New_postbox ()
         call Deliver_get_message (All_active_modules,
     .                             Variable_name, .false.)
      endif

      call Respond2Post_logical_var
     .   (Variable_name, Units, Variable, Numvals,
     .    Allow_Zero_numvals, Request_no)

      if (Numvals .eq. 0) then
         call Delete_postbox ()
      endif

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine get_real_array
     .   (owner_module_name, variable_name, Array_size,
     .    units, variable, numvals,
     .    lower_limit, upper_limit)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none


*+ Sub-Program Arguments
      character Owner_module_name*(*)  ! (INPUT) Owner module name
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      integer Array_size               ! (INPUT) Size of array
      character Units*(*)              ! (INPUT) Units required by caller
      real Variable(*)                 ! (OUTPUT) Variable returned to caller
      integer numvals                  ! (OUTPUT) Number of values returned
      real Lower_limit                 ! (INPUT) Lower limit for bounds check
      real Upper_limit                 ! (INPUT) Upper limit for bounds check

*+ Purpose
*     High level routine to get a value from another module.
*     This routine also checks the bounds of the returned variable.
*     Fatal_error will be called if any err is encountered.

*+  Mission Statement
*     Get %5 from the system (Lower Bound = %7, Upper Bound = %8)

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines

*+ Calls

*+ Constant Values
      logical No_zero_numvals          ! Is this an optional routine?
      parameter (No_zero_numvals = .false.)
*
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='get_real_array')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      call New_postbox ()
      call Deliver_get_message (Owner_module_name,
     .                          Variable_name, .false.)

      call Respond2Post_real_array
     .   (Variable_name, Array_size, Units, Variable, Numvals,
     .    Lower_limit, Upper_limit, No_zero_numvals, 1)
      call Delete_postbox ()

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine get_real_array_optional
     .   (owner_module_name, variable_name, Array_size,
     .    units, variable, numvals,
     .    lower_limit, upper_limit)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none


*+ Sub-Program Arguments
      character Owner_module_name*(*)  ! (INPUT) Owner module name
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      integer Array_size               ! (INPUT) Size of array
      character Units*(*)              ! (INPUT) Units required by caller
      real Variable(*)                 ! (OUTPUT) Variable returned to caller
      integer numvals                  ! (OUTPUT) Number of values returned
      real Lower_limit                 ! (INPUT) Lower limit for bounds check
      real Upper_limit                 ! (INPUT) Upper limit for bounds check

*+ Purpose
*     High level routine to get a value from another module.
*     This routine also checks the bounds of the returned variable.
*     Fatal_error will be called if any err is encountered.

*+  Mission Statement
*     Get %5 from the system (Lower Bound = %7, Upper Bound = %8)|ITALIC{(OPTIONAL)}

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines
*     dph 24/9/96  Added test to make sure module_name is an active one.
*     dph 26/7/99  removed call to is_active_module.  All modules are now active

*+ Calls

*+ Constant Values
      logical Allow_zero_numvals          ! Is this an optional routine?
      parameter (Allow_zero_numvals = .true.)
*
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='get_real_array_optional')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

         call New_postbox ()
         call Deliver_get_message (Owner_module_name,
     .                             Variable_name, .true.)

         call Respond2Post_real_array
     .      (Variable_name, Array_size, Units, Variable, Numvals,
     .       Lower_limit, Upper_limit, Allow_zero_numvals, 1)
         call Delete_postbox ()

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine get_real_arrays
     .   (request_no, variable_name, Array_size,
     .    units, variable, numvals,
     .    lower_limit, upper_limit)
* ====================================================================
      use ConstantsModule
      Use ErrorModule
      use PostboxModule
      implicit none

*+ Sub-Program Arguments
      integer Request_no               ! (INPUT) Owner module name
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      integer Array_size               ! (INPUT) Size of array
      character Units*(*)              ! (INPUT) Units required by caller
      real Variable(*)                 ! (OUTPUT) Variable returned to caller
      integer numvals                  ! (OUTPUT) Number of values returned
      real Lower_limit                 ! (INPUT) Lower limit for bounds check
      real Upper_limit                 ! (INPUT) Upper limit for bounds check

*+ Purpose
*     High level routine to get a value from another module.
*     This routine also checks the bounds of the returned variable.
*     Fatal_error will be called if any err is encountered.

*+  Mission Statement
*     Get next %5 from the system (Lower Bound = %7, Upper Bound = %8)

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines

*+ Calls

*+ Constant Values
      logical Allow_zero_numvals          ! Is this an optional routine?
      parameter (Allow_zero_numvals = .true.)
*
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='get_real_arrays')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      ! Only send a message out if this is first call

      if (Request_no .eq. 1) then
         call New_postbox ()
         call Deliver_get_message (All_active_modules,
     .                             Variable_name, .false.)
      endif

      call Respond2Post_real_array
     .   (Variable_name, Array_size, Units, Variable, Numvals,
     .    Lower_limit, Upper_limit, Allow_zero_numvals, Request_no)

      if (Numvals .eq. 0) then
         call Delete_postbox ()
      endif

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine get_real_var
     .   (owner_module_name, variable_name,
     .    units, variable, numvals,
     .    lower_limit, upper_limit)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none


*+ Sub-Program Arguments
      character Owner_module_name*(*)  ! (INPUT) Owner module name
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      character Units*(*)              ! (INPUT) Units required by caller
      real Variable                    ! (OUTPUT) Variable returned to caller
      integer numvals                  ! (OUTPUT) Number of values returned
      real Lower_limit                 ! (INPUT) Lower limit for bounds check
      real Upper_limit                 ! (INPUT) Upper limit for bounds check

*+ Purpose
*     High level routine to get a value from another module.
*     This routine also checks the bounds of the returned variable.
*     Fatal_error will be called if any err is encountered.

*+  Mission Statement
*     Get %4 from the system (Lower Bound = %6, Upper Bound = %7)

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines
*     DPH 17/5/96  Changed all get routines to use deliver_get_message

*+ Calls

*+ Constant Values
      logical No_zero_numvals          ! Is this an optional routine?
      parameter (No_zero_numvals = .false.)
*
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='get_real_var')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      call New_postbox ()
      call Deliver_get_message (Owner_module_name,
     .                          Variable_name, .false.)

      call Respond2Post_real_var
     .   (Variable_name, Units, Variable, Numvals,
     .    Lower_limit, Upper_limit, No_zero_numvals, 1)

      call Delete_postbox()

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine get_real_var_optional
     .   (owner_module_name, variable_name,
     .    units, variable, numvals,
     .    lower_limit, upper_limit)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none


*+ Sub-Program Arguments
      character Owner_module_name*(*)  ! (INPUT) Owner module name
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      character Units*(*)              ! (INPUT) Units required by caller
      real Variable                    ! (OUTPUT) Variable returned to caller
      integer numvals                  ! (OUTPUT) Number of values returned
      real Lower_limit                 ! (INPUT) Lower limit for bounds check
      real Upper_limit                 ! (INPUT) Upper limit for bounds check

*+ Purpose
*     High level routine to get a value from another module.
*     This routine also checks the bounds of the returned variable.
*     Fatal_error will be called if any err is encountered.

*+  Mission Statement
*     Get %4 from the system (Lower Bound = %6, Upper Bound = %7)|ITALIC{(OPTIONAL)}

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines
*     dph 24/9/96  Added test to make sure module_name is an active one.
*     dph 26/7/99  removed call to is_active_module.  All modules are now active

*+ Calls

*+ Constant Values
      logical Allow_zero_numvals          ! Is this an optional routine?
      parameter (Allow_zero_numvals = .true.)
*
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='get_real_var_optional')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

         call New_postbox ()
         call Deliver_get_message (Owner_module_name,
     .                             Variable_name, .true.)

         call Respond2Post_real_var
     .      (Variable_name, Units, Variable, Numvals,
     .       Lower_limit, Upper_limit, Allow_zero_numvals, 1)
         call Delete_postbox ()

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine get_real_vars
     .   (request_no, variable_name,
     .    units, variable, numvals,
     .    lower_limit, upper_limit)
* ====================================================================
      use ConstantsModule
      Use ErrorModule
      use PostboxModule
      implicit none

*+ Sub-Program Arguments
      integer Request_no               ! (INPUT) Owner module name
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      character Units*(*)              ! (INPUT) Units required by caller
      real Variable                    ! (OUTPUT) Variable returned to caller
      integer numvals                  ! (OUTPUT) Number of values returned
      real Lower_limit                 ! (INPUT) Lower limit for bounds check
      real Upper_limit                 ! (INPUT) Upper limit for bounds check

*+ Purpose
*     High level routine to get a value from another module.
*     This routine also checks the bounds of the returned variable.
*     Fatal_error will be called if any err is encountered.

*+  Mission Statement
*     Get next %4 from the system (Lower Bound = %6, Upper Bound = %7)

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines

*+ Calls

*+ Constant Values
      logical Allow_zero_numvals          ! Is this an optional routine?
      parameter (Allow_zero_numvals = .true.)
*
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='get_real_vars')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      ! Only send a message out if this is first call

      if (Request_no .eq. 1) then
         call New_postbox ()
         call Deliver_get_message (All_active_modules,
     .                             Variable_name, .false.)
      endif

      call Respond2Post_real_var
     .   (Variable_name, Units, Variable, Numvals,
     .    Lower_limit, Upper_limit, Allow_Zero_numvals, Request_no)

      if (Numvals .eq. 0) then
         call Delete_postbox ()
      endif

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine Respond2get_char_array
     .   (variable_name, units, variable, Numvals)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none


*+ Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name
      character Units*(*)              ! (INPUT) Units required by caller
      character Variable(*)*(*)        ! (INPUT) Variable value
      integer Numvals                  ! (INPUT) number of values in array

*+ Purpose
*     High level routine to respond to a get request

*+  Mission Statement
*     Supply %3 to the requesting module

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines

*+ Calls

*+ Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='Respond2get_char_array')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      call Post_char_array (Variable_name, units, variable, Numvals)

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine Respond2get_char_var
     .   (variable_name, units, variable)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none


*+ Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name
      character Units*(*)              ! (INPUT) Units required by caller
      character Variable*(*)           ! (INPUT) Variable value

*+ Purpose
*     High level routine to respond to a get request

*+  Mission Statement
*     Supply %3 to the requesting module

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines

*+ Calls

*+ Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='Respond2get_char_var')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      call Post_char_var (Variable_name, Units, Variable)

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine Respond2get_double_array
     .   (variable_name, units, variable, Numvals)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none


*+ Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name
      character Units*(*)              ! (INPUT) Units required by caller
      double precision Variable(*)     ! (INPUT) Variable value
      integer Numvals                  ! (INPUT) number of values in array

*+ Purpose
*     High level routine to respond to a get request

*+  Mission Statement
*     Supply %3 to the requesting module

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines

*+ Calls

*+ Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='Respond2get_double_array')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      call Post_double_array (Variable_name, units, variable, Numvals)

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine Respond2get_double_var
     .   (variable_name, units, variable)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none


*+ Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name
      character Units*(*)              ! (INPUT) Units required by caller
      double precision Variable        ! (INPUT) Variable value

*+ Purpose
*     High level routine to respond to a get request

*+  Mission Statement
*     Supply %3 to the requesting module

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines

*+ Calls

*+ Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='Respond2get_double_var')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      call Post_double_var (Variable_name, Units, Variable)

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine Respond2get_integer_array
     .   (variable_name, units, variable, Numvals)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none


*+ Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name
      character Units*(*)              ! (INPUT) Units required by caller
      integer Variable(*)              ! (INPUT) Variable value
      integer Numvals                  ! (INPUT) number of values in array

*+ Purpose
*     High level routine to respond to a get request

*+  Mission Statement
*     Supply %3 ro the requesting module

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines

*+ Calls

*+ Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='Respond2get_integer_array')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      call Post_integer_array (Variable_name, units, variable, Numvals)

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine Respond2get_integer_var
     .   (variable_name, units, variable)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none


*+ Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name
      character Units*(*)              ! (INPUT) Units required by caller
      integer Variable                 ! (INPUT) Variable value

*+ Purpose
*     High level routine to respond to a get request

*+  Mission Statement
*     Supply %3 ro the requesting module

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines

*+ Calls

*+ Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='Respond2get_integer_var')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      call Post_integer_var (Variable_name, Units, Variable)

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine Respond2get_logical_array
     .   (variable_name, units, variable, Numvals)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none


*+ Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name
      character Units*(*)              ! (INPUT) Units required by caller
      logical Variable(*)              ! (INPUT) Variable value
      integer Numvals                  ! (INPUT) number of values in array

*+ Purpose
*     High level routine to respond to a get request

*+  Mission Statement
*     Supply %3 ro the requesting module

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines

*+ Calls

*+ Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='Respond2get_logical_array')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      call Post_logical_array (Variable_name, units, variable, Numvals)

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine Respond2get_logical_var
     .   (variable_name, units, variable)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none


*+ Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name
      character Units*(*)              ! (INPUT) Units required by caller
      logical Variable                 ! (INPUT) Variable value

*+ Purpose
*     High level routine to respond to a get request

*+  Mission Statement
*     Supply %3 ro the requesting module

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines

*+ Calls

*+ Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='Respond2get_logical_var')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      call Post_logical_var (Variable_name, Units, Variable)

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine Respond2get_real_array
     .   (variable_name, units, variable, Numvals)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none


*+ Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name
      character Units*(*)              ! (INPUT) Units required by caller
      real Variable(*)                 ! (INPUT) Variable value
      integer Numvals                  ! (INPUT) number of values in array

*+ Purpose
*     High level routine to respond to a get request

*+  Mission Statement
*     Supply %3 to the requesting module

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines

*+ Calls

*+ Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='Respond2get_real_array')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      call Post_real_array (Variable_name, units, variable, Numvals)

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine Respond2get_real_var
     .   (variable_name, units, variable)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none


*+ Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name
      character Units*(*)              ! (INPUT) Units required by caller
      real Variable                    ! (INPUT) Variable value

*+ Purpose
*     High level routine to respond to a get request

*+  Mission Statement
*     Supply %3 to the requesting module

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines

*+ Calls

*+ Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='Respond2get_real_var')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      call Post_real_var (Variable_name, Units, Variable)

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine Respond2set_char_array
     .   (variable_name, Array_size,
     .    units, variable, numvals)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none


*+ Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      integer Array_size               ! (INPUT) Size of array
      character Units*(*)              ! (INPUT) Units required by caller
      character Variable(*)*(*)        ! (OUTPUT) Variable returned to caller
      integer numvals                  ! (OUTPUT) Number of values returned

*+ Purpose
*     High level routine to respond 2 a set request

*+  Mission Statement
*

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines

*+ Calls

*+ Constant Values
      logical No_zero_numvals          ! Is this an optional routine?
      parameter (No_zero_numvals = .false.)
*
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='Respond2set_char_array')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      call Respond2Post_char_array
     .   (variable_name, Array_size, Units, Variable, Numvals,
     .    No_zero_numvals, 1)

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine Respond2set_char_var
     .   (variable_name,
     .    units, variable, numvals)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none


*+ Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      character Units*(*)              ! (INPUT) Units required by caller
      character Variable*(*)           ! (OUTPUT) Variable returned to caller
      integer numvals                  ! (OUTPUT) Number of values returned

*+ Purpose
*     High level routine to respond 2 a set request

*+  Mission Statement
*

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines

*+ Calls

*+ Constant Values
      logical No_zero_numvals          ! Is this an optional routine?
      parameter (No_zero_numvals = .false.)
*
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='Respond2set_char_var')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      call Respond2Post_char_var
     .   (Variable_name, Units, Variable, Numvals, No_zero_numvals, 1)

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine Respond2set_double_array
     .   (variable_name, Array_size,
     .    units, variable, numvals,
     .    lower_limit, upper_limit)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none


*+ Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      integer Array_size               ! (INPUT) Size of array
      character Units*(*)              ! (INPUT) Units required by caller
      double precision Variable(*)     ! (OUTPUT) Variable returned to caller
      integer numvals                  ! (OUTPUT) Number of values returned
      double precision Lower_limit     ! (INPUT) Lower limit for bounds check
      double precision Upper_limit     ! (INPUT) Upper limit for bounds check

*+ Purpose
*     High level routine to respond 2 a set request

*+  Mission Statement
*

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines

*+ Calls

*+ Constant Values
      logical No_zero_numvals          ! Is this an optional routine?
      parameter (No_zero_numvals = .false.)
*
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='Respond2set_double_array')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      call Respond2Post_double_array
     .   (variable_name, Array_size, Units, Variable, Numvals,
     .    Lower_limit, Upper_limit, No_zero_numvals, 1)

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine Respond2set_double_var
     .   (variable_name,
     .    units, variable, numvals,
     .    lower_limit, upper_limit)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none


*+ Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      character Units*(*)              ! (INPUT) Units required by caller
      double precision Variable        ! (OUTPUT) Variable returned to caller
      integer numvals                  ! (OUTPUT) Number of values returned
      double precision Lower_limit     ! (INPUT) Lower limit for bounds check
      double precision Upper_limit     ! (INPUT) Upper limit for bounds check

*+ Purpose
*     High level routine to respond 2 a set request

*+  Mission Statement
*

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines

*+ Calls

*+ Constant Values
      logical No_zero_numvals          ! Is this an optional routine?
      parameter (No_zero_numvals = .false.)
*
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='Respond2set_double_var')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      call Respond2Post_double_var
     .   (Variable_name, Units, Variable, Numvals,
     .    Lower_limit, Upper_limit, No_zero_numvals, 1)

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine Respond2set_integer_array
     .   (variable_name, Array_size,
     .    units, variable, numvals,
     .    lower_limit, upper_limit)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none


*+ Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      integer Array_size               ! (INPUT) Size of array
      character Units*(*)              ! (INPUT) Units required by caller
      integer Variable(*)              ! (OUTPUT) Variable returned to caller
      integer numvals                  ! (OUTPUT) Number of values returned
      integer Lower_limit              ! (INPUT) Lower limit for bounds check
      integer Upper_limit              ! (INPUT) Upper limit for bounds check

*+ Purpose
*     High level routine to respond 2 a set request

*+  Mission Statement
*

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines

*+ Calls

*+ Constant Values
      logical No_zero_numvals          ! Is this an optional routine?
      parameter (No_zero_numvals = .false.)
*
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='Respond2set_integer_array')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      call Respond2Post_integer_array
     .   (variable_name, Array_size, Units, Variable, Numvals,
     .    Lower_limit, Upper_limit, No_zero_numvals, 1)

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine Respond2set_integer_var
     .   (variable_name,
     .    units, variable, numvals,
     .    lower_limit, upper_limit)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none


*+ Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      character Units*(*)              ! (INPUT) Units required by caller
      integer Variable                 ! (OUTPUT) Variable returned to caller
      integer numvals                  ! (OUTPUT) Number of values returned
      integer Lower_limit              ! (INPUT) Lower limit for bounds check
      integer Upper_limit              ! (INPUT) Upper limit for bounds check

*+ Purpose
*     High level routine to respond 2 a set request

*+  Mission Statement
*

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines

*+ Calls

*+ Constant Values
      logical No_zero_numvals          ! Is this an optional routine?
      parameter (No_zero_numvals = .false.)
*
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='Respond2set_integer_var')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      call Respond2Post_integer_var
     .   (Variable_name, Units, Variable, Numvals,
     .    Lower_limit, Upper_limit, No_zero_numvals, 1)

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine Respond2set_logical_array
     .   (variable_name, Array_size,
     .    units, variable, numvals)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none


*+ Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      integer Array_size               ! (INPUT) Size of array
      character Units*(*)              ! (INPUT) Units required by caller
      logical Variable(*)              ! (OUTPUT) Variable returned to caller
      integer numvals                  ! (OUTPUT) Number of values returned

*+ Purpose
*     High level routine to respond 2 a set request

*+  Mission Statement
*

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines

*+ Calls

*+ Constant Values
      logical No_zero_numvals          ! Is this an optional routine?
      parameter (No_zero_numvals = .false.)
*
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='Respond2set_logical_array')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      call Respond2Post_logical_array
     .   (variable_name, Array_size, Units, Variable, Numvals,
     .    No_zero_numvals, 1)

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine Respond2set_logical_var
     .   (variable_name,
     .    units, variable, numvals)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none


*+ Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      character Units*(*)              ! (INPUT) Units required by caller
      logical Variable                 ! (OUTPUT) Variable returned to caller
      integer numvals                  ! (OUTPUT) Number of values returned

*+ Purpose
*     High level routine to respond 2 a set request

*+  Mission Statement
*

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines

*+ Calls

*+ Constant Values
      logical No_zero_numvals          ! Is this an optional routine?
      parameter (No_zero_numvals = .false.)
*
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='Respond2set_logical_var')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      call Respond2Post_logical_var
     .   (Variable_name, Units, Variable, Numvals, No_zero_numvals, 1)

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine Respond2set_real_array
     .   (variable_name, Array_size,
     .    units, variable, numvals,
     .    lower_limit, upper_limit)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none


*+ Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      integer Array_size               ! (INPUT) Size of array
      character Units*(*)              ! (INPUT) Units required by caller
      real Variable(*)                 ! (OUTPUT) Variable returned to caller
      integer numvals                  ! (OUTPUT) Number of values returned
      real Lower_limit                 ! (INPUT) Lower limit for bounds check
      real Upper_limit                 ! (INPUT) Upper limit for bounds check

*+ Purpose
*     High level routine to respond 2 a set request

*+  Mission Statement
*

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines

*+ Calls

*+ Constant Values
      logical No_zero_numvals          ! Is this an optional routine?
      parameter (No_zero_numvals = .false.)
*
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='Respond2set_real_array')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      call Respond2Post_real_array
     .   (variable_name, Array_size, Units, Variable, Numvals,
     .    Lower_limit, Upper_limit, No_zero_numvals, 1)

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine Respond2set_real_var
     .   (variable_name,
     .    units, variable, numvals,
     .    lower_limit, upper_limit)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none


*+ Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name to search for
      character Units*(*)              ! (INPUT) Units required by caller
      real Variable                    ! (OUTPUT) Variable returned to caller
      integer numvals                  ! (OUTPUT) Number of values returned
      real Lower_limit                 ! (INPUT) Lower limit for bounds check
      real Upper_limit                 ! (INPUT) Upper limit for bounds check

*+ Purpose
*     High level routine to respond 2 a set request

*+  Mission Statement
*

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines

*+ Calls

*+ Constant Values
      logical No_zero_numvals          ! Is this an optional routine?
      parameter (No_zero_numvals = .false.)
*
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='Respond2set_real_var')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      call Respond2Post_real_var
     .   (Variable_name, Units, Variable, Numvals,
     .    Lower_limit, Upper_limit, No_zero_numvals, 1)

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine set_char_array
     .   (owner_module_name, variable_name,
     .    units, variable, numvals)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none


*+ Sub-Program Arguments
      character Owner_module_name*(*)  ! (INPUT) Owner module name
      character Variable_name*(*)      ! (INPUT) Variable name being set
      character Units*(*)              ! (INPUT) Units required by caller
      character Variable(*)*(*)        ! (INPUT) Variable value being set
      integer numvals                  ! (OUTPUT) Number of values in array

*+ Purpose
*     High level routine to set a value in another module.

*+  Mission Statement
*

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines

*+ Calls

*+ Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='set_char_array')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      call New_postbox ()
      call Post_char_array (Variable_name, Units, Variable, Numvals)

      call Deliver_set_message(Owner_module_name, Variable_name)
      call Delete_postbox ()

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine set_char_var
     .   (owner_module_name, variable_name,
     .    units, variable)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none


*+ Sub-Program Arguments
      character Owner_module_name*(*)  ! (INPUT) Owner module name
      character Variable_name*(*)      ! (INPUT) Variable name being set
      character Units*(*)              ! (INPUT) Units required by caller
      character Variable*(*)           ! (INPUT) Variable value being set

*+ Purpose
*     High level routine to set a value in another module.

*+  Mission Statement
*

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines

*+ Calls

*+ Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='set_char_var')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      call New_postbox ()
      call Post_char_var (Variable_name, Units, Variable)

      call Deliver_set_message(Owner_module_name, Variable_name)
      call Delete_postbox ()

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine set_double_array
     .   (owner_module_name, variable_name,
     .    units, variable, numvals)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none


*+ Sub-Program Arguments
      character Owner_module_name*(*)  ! (INPUT) Owner module name
      character Variable_name*(*)      ! (INPUT) Variable name being set
      character Units*(*)              ! (INPUT) Units required by caller
      double precision Variable(*)     ! (INPUT) Variable value being set
      integer numvals                  ! (OUTPUT) Number of values in array

*+ Purpose
*     High level routine to set a value in another module.

*+  Mission Statement
*

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines

*+ Calls

*+ Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='set_double_array')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      call New_postbox ()
      call Post_double_array (Variable_name, Units, Variable, Numvals)

      call Deliver_set_message(Owner_module_name, Variable_name)
      call Delete_postbox ()

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine set_double_var
     .   (owner_module_name, variable_name,
     .    units, variable)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none


*+ Sub-Program Arguments
      character Owner_module_name*(*)  ! (INPUT) Owner module name
      character Variable_name*(*)      ! (INPUT) Variable name being set
      character Units*(*)              ! (INPUT) Units required by caller
      double precision Variable        ! (INPUT) Variable value being set

*+ Purpose
*     High level routine to set a value in another module.

*+  Mission Statement
*

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines

*+ Calls

*+ Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='set_real_var')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      call New_postbox ()
      call Post_double_var (Variable_name, Units, Variable)

      call Deliver_set_message(Owner_module_name, Variable_name)
      call Delete_postbox ()

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine set_integer_array
     .   (owner_module_name, variable_name,
     .    units, variable, numvals)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none


*+ Sub-Program Arguments
      character Owner_module_name*(*)  ! (INPUT) Owner module name
      character Variable_name*(*)      ! (INPUT) Variable name being set
      character Units*(*)              ! (INPUT) Units required by caller
      integer Variable(*)              ! (INPUT) Variable value being set
      integer numvals                  ! (OUTPUT) Number of values in array

*+ Purpose
*     High level routine to set a value in another module.

*+  Mission Statement
*

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines

*+ Calls

*+ Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='set_integer_array')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      call New_postbox ()
      call Post_integer_array (Variable_name, Units, Variable, Numvals)

      call Deliver_set_message(Owner_module_name, Variable_name)
      call Delete_postbox ()

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine set_integer_var
     .   (owner_module_name, variable_name,
     .    units, variable)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none


*+ Sub-Program Arguments
      character Owner_module_name*(*)  ! (INPUT) Owner module name
      character Variable_name*(*)      ! (INPUT) Variable name being set
      character Units*(*)              ! (INPUT) Units required by caller
      integer Variable                 ! (INPUT) Variable value being set

*+ Purpose
*     High level routine to set a value in another module.

*+  Mission Statement
*

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines

*+ Calls

*+ Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='set_integer_var')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      call New_postbox ()
      call Post_integer_var (Variable_name, Units, Variable)

      call Deliver_set_message(Owner_module_name, Variable_name)
      call Delete_postbox ()

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine set_logical_array
     .   (owner_module_name, variable_name,
     .    units, variable, numvals)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none


*+ Sub-Program Arguments
      character Owner_module_name*(*)  ! (INPUT) Owner module name
      character Variable_name*(*)      ! (INPUT) Variable name being set
      character Units*(*)              ! (INPUT) Units required by caller
      logical Variable(*)              ! (INPUT) Variable value being set
      integer numvals                  ! (OUTPUT) Number of values in array

*+ Purpose
*     High level routine to set a value in another module.

*+  Mission Statement
*

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines

*+ Calls

*+ Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='set_logical_array')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      call New_postbox ()
      call Post_logical_array (Variable_name, Units, Variable, Numvals)

      call Deliver_set_message(Owner_module_name, Variable_name)
      call Delete_postbox ()

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine set_logical_var
     .   (owner_module_name, variable_name,
     .    units, variable)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none


*+ Sub-Program Arguments
      character Owner_module_name*(*)  ! (INPUT) Owner module name
      character Variable_name*(*)      ! (INPUT) Variable name being set
      character Units*(*)              ! (INPUT) Units required by caller
      logical Variable                 ! (INPUT) Variable value being set

*+ Purpose
*     High level routine to set a value in another module.

*+  Mission Statement
*

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines

*+ Calls

*+ Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='set_logical_var')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      call New_postbox ()
      call Post_logical_var (Variable_name, Units, Variable)

      call Deliver_set_message(Owner_module_name, Variable_name)
      call Delete_postbox ()

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine set_real_array
     .   (owner_module_name, variable_name,
     .    units, variable, numvals)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none


*+ Sub-Program Arguments
      character Owner_module_name*(*)  ! (INPUT) Owner module name
      character Variable_name*(*)      ! (INPUT) Variable name being set
      character Units*(*)              ! (INPUT) Units required by caller
      real Variable(*)                 ! (INPUT) Variable value being set
      integer numvals                  ! (OUTPUT) Number of values in array

*+ Purpose
*     High level routine to set a value in another module.

*+  Mission Statement
*

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines

*+ Calls

*+ Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='set_real_array')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      call New_postbox ()
      call Post_real_array (Variable_name, Units, Variable, Numvals)

      call Deliver_set_message(Owner_module_name, Variable_name)
      call Delete_postbox ()

      call pop_routine(This_routine)
      return
      end subroutine



* ====================================================================
       subroutine set_real_var
     .   (owner_module_name, variable_name,
     .    units, variable)
* ====================================================================
      Use ErrorModule
      use PostboxModule
      implicit none


*+ Sub-Program Arguments
      character Owner_module_name*(*)  ! (INPUT) Owner module name
      character Variable_name*(*)      ! (INPUT) Variable name being set
      character Units*(*)              ! (INPUT) Units required by caller
      real Variable                    ! (INPUT) Variable value being set

*+ Purpose
*     High level routine to set a value in another module.

*+  Mission Statement
*

*+ Changes
*     DPH 18/10/94
*     DPH 25/10/95 Reworked routine to use new postbox routines

*+ Calls

*+ Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='set_real_var')

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      call New_postbox ()
      call Post_real_var (Variable_name, Units, Variable)

      call Deliver_set_message(Owner_module_name, Variable_name)
      call Delete_postbox ()

      call pop_routine(This_routine)
      return
      end subroutine


      end module InterfaceModule
      