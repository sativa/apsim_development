// *******************************************************************
//     file gpoint.cpp
// *******************************************************************

//  Short description:
//    This module provides a generic point class that is used throughout
//    graphlib2.

//  Notes:

//  Changes:
//      DPH 20/7/95

// ----------------------- Declaration section -----------------------

//  Include files
      #include <graph\global.h>
      #pragma hdrstop

//  Constants
//      none

IMPLEMENT_CASTABLE (GPoint);
IMPLEMENT_STREAMABLE (GPoint);

// *******************************************************************
      void GPoint::Streamer::Write(opstream& os) const {
// *******************************************************************

//  Short description:
//    Writes an instance of GPoint to the passed ipstream.

//  Notes:

//  Changes:
//      DPH 23/6/94

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   os << GetObject()->Pixel_x;
   os << GetObject()->Pixel_y;
   os << GetObject()->mm_x;
   os << GetObject()->mm_y;
   os << GetObject()->World_x;
   os << GetObject()->World_y;
   os << GetObject()->Logical_x;
   os << GetObject()->Logical_y;
   os << GetObject()->x_logical_type;
   os << GetObject()->y_logical_type;
   os << GetObject()->Data_in_logical;
   os << GetObject()->Data_in_world;
   }

// *******************************************************************
      void *GPoint::Streamer::Read(ipstream& is, uint32 /*version*/) const {
// *******************************************************************

//  Short description:
//    Reads an instance of GPoint from the passed ipstream.

//  Notes:

//  Changes:
//      DPH 23/6/94

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   is >> GetObject()->Pixel_x;
   is >> GetObject()->Pixel_y;
   is >> GetObject()->mm_x;
   is >> GetObject()->mm_y;
   is >> GetObject()->World_x;
   is >> GetObject()->World_y;
   is >> GetObject()->Logical_x;
   is >> GetObject()->Logical_y;
   is >> (int) GetObject()->x_logical_type;
   is >> (int) GetObject()->y_logical_type;
   is >> GetObject()->Data_in_logical;
   is >> GetObject()->Data_in_world;

   return GetObject();
   }


// *******************************************************************
      GPoint::GPoint() {
// *******************************************************************

//  Short description:
//      constructor

//  Notes:

//  Changes:
//      DPH 20/7/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   Reset();
   }

// *******************************************************************
      GPoint::~GPoint() {
// *******************************************************************

//  Short description:
//      destructor

//  Notes:

//  Changes:
//      DPH 20/7/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   }

// *******************************************************************
      void GPoint::Reset(void) {
// *******************************************************************

//  Short description:
//      reset the object

//  Notes:

//  Changes:
//      DPH 20/7/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   Pixel_x = 0;
   Pixel_y = 0;
   mm_x = 0;
   mm_y = 0;
   World_x = 0;
   World_y = 0;
   Logical_x = 0;
   Logical_y = 0;
   x_logical_type = x1_axis;
   y_logical_type = y1_axis;
   Data_in_logical = FALSE;
   Data_in_world = FALSE;
   }

// *******************************************************************
      void GPoint::Set_pixel(int x, int y) {
// *******************************************************************

//  Short description:
//      set the pixel coordinates of object

//  Notes:

//  Changes:
//      DPH 20/7/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   Reset();
   Pixel_x = x;
   Pixel_y = y;
   }

// *******************************************************************
      void GPoint::Set_mm(int x, int y) {
// *******************************************************************

//  Short description:
//      set the mm coordinates of object

//  Notes:

//  Changes:
//      DPH 20/7/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   Reset();
   mm_x = x;
   mm_y = y;
   }

// *******************************************************************
      void GPoint::Set_world(int x, int y) {
// *******************************************************************

//  Short description:
//      set the world coordinates of object

//  Notes:

//  Changes:
//      DPH 20/7/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   Reset();
   World_x = x;
   World_y = y;
   Data_in_world = TRUE;
   }

// *******************************************************************
      void GPoint::Set_logical(float x, float y,
                               Axis_type_enum X_type, Axis_type_enum Y_type) {
// *******************************************************************

//  Short description:
//      set the logical coordinates of object

//  Notes:

//  Changes:
//      DPH 20/7/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   Reset();
   Logical_x = x;
   Logical_y = y;
   x_logical_type = X_type;
   y_logical_type = Y_type;

   Data_in_logical = TRUE;
   }

// *******************************************************************
      void GPoint::Add_pixel(int x, int y) {
// *******************************************************************

//  Short description:
//      add the pixel coordinates of object

//  Notes:

//  Changes:
//      DPH 20/7/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   Pixel_x += x;
   Pixel_y += y;
   }

// *******************************************************************
      void GPoint::Add_mm(int x, int y) {
// *******************************************************************

//  Short description:
//      add the mm coordinates of object

//  Notes:

//  Changes:
//      DPH 20/7/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   mm_x += x;
   mm_y += y;
   }

// *******************************************************************
      void GPoint::Add_world(int x, int y) {
// *******************************************************************

//  Short description:
//      add the world coordinates of object

//  Notes:

//  Changes:
//      DPH 20/7/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   World_x += x;
   World_y += y;
   Data_in_world = TRUE;
   }

// *******************************************************************
      void GPoint::Add_logical(float x, float y,
                               Axis_type_enum X_type, Axis_type_enum Y_type) {
// *******************************************************************

//  Short description:
//      add the logical coordinates of object

//  Notes:

//  Changes:
//      DPH 20/7/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   Logical_x += x;
   Logical_y += y;
   x_logical_type = X_type;
   y_logical_type = Y_type;
   Data_in_logical = TRUE;
   }

// *******************************************************************
      void GPoint::Get_pixel(GDC& DC, long& X, long& Y) {
// *******************************************************************

//  Short description:
//      return the x and y coordinates in pixels to caller.

//  Notes:

//  Changes:
//      DPH 20/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   X = Pixel_x;
   Y = Pixel_y;

   if (mm_x != 0 || mm_y != 0)
      {
      X += DC.mm_X(mm_x);
      Y += DC.mm_Y(mm_y);
      }

   if (Data_in_world)
      {
      X += DC.World_X(World_x);
      Y += DC.World_Y(World_y);
      }

   if (Data_in_logical)
      {
      if (x_logical_type == x1_axis)
         X += DC.Log_X1(Logical_x);
      else
         X += DC.Log_X2(Logical_x);
      if (y_logical_type == y1_axis)
         Y += DC.Log_Y1(Logical_y);
      else
         Y += DC.Log_Y2(Logical_y);
      }
   }


