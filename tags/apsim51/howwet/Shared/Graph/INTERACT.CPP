#include <graph\global.h>
#pragma hdrstop

#define HALF_HANDLE_SIZE 1         // 2 mm for half a handle

IMPLEMENT_CASTABLE (Interactive_object);
IMPLEMENT_STREAMABLE (Interactive_object);

// *******************************************************************
      void Interactive_object::Streamer::Write(opstream& os) const {
// *******************************************************************

//  Short description:
//    Writes an instance of Interactive_object to the passed ipstream.

//  Notes:

//  Changes:
//      DPH 23/6/94

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   os << GetObject()->Is_selected;
   os << GetObject()->Is_selectable;
   os << GetObject()->Is_movable;
   os << GetObject()->Is_sizable;
   os << GetObject()->Sizing;
   os << GetObject()->Moving;
   }

// *******************************************************************
      void *Interactive_object::Streamer::Read(ipstream& is, uint32 /*version*/) const {
// *******************************************************************

//  Short description:
//    Reads an instance of Interactive_object from the passed ipstream.

//  Notes:

//  Changes:
//      DPH 23/6/94

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   is >> GetObject()->Is_selected;
   is >> GetObject()->Is_selectable;
   is >> GetObject()->Is_movable;
   is >> GetObject()->Is_sizable;
   is >> GetObject()->Sizing;
   is >> GetObject()->Moving;

   return GetObject();
   }

// *******************************************************************
       Interactive_object::Interactive_object(void)  {
// *******************************************************************

//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 10/4/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   Is_selected = FALSE;
   Is_selectable = FALSE;
   Is_movable = FALSE;
   Is_sizable = FALSE;
   Sizing = FALSE;
   Moving = FALSE;
   }

// *******************************************************************
       void Interactive_object::Draw_handle(GDC& DC, GPoint& Point)  {
// *******************************************************************

//  Short description:
//    draw a selection handle at specified point.

//  Notes:

//  Changes:
//    DPH 10/4/95

//  Calls:

//  Internal variables
      TBrush Brush(TColor::White);     // Brush for handle.
      TPen Pen(TColor::White);         // Pen for handle.
      GPoint Point1;                   // First point for handle
      GPoint Point2;                   // second point for handle

// -------------------- Executable code section ----------------------

   Point1 = Point;
   Point2 = Point;
   Point1.Add_mm(-HALF_HANDLE_SIZE, -HALF_HANDLE_SIZE);
   Point1.Add_pixel(-2, -2);
   Point2.Add_mm(HALF_HANDLE_SIZE, HALF_HANDLE_SIZE);
   Point2.Add_pixel(2, 2);

   DC.SelectObject(Pen);
   DC.SelectObject(Brush);
   DC.Rectangle(Point1, Point2);
   }

// *******************************************************************
       void Interactive_object::Draw_selection(GDC& DC)  {
// *******************************************************************

//  Short description:
//    Draw a selection around the current object.

//  Notes:

//  Changes:
//    DPH 10/4/95

//  Calls:

//  Internal variables
      GPoint Point;                    // Point at which to draw handle.
      TRect Current_position;

// -------------------- Executable code section ----------------------

   // Get current position of object.

   Get_bounding_rectangle(DC, Current_position);

   if (Current_position.left != Current_position.right)
      {
      // top left corner

      Point.Set_pixel(Current_position.left, Current_position.top);
      Draw_handle(DC, Point);

      // top right corner

      Point.Set_pixel(Current_position.right, Current_position.top);
      Draw_handle(DC, Point);

      // bottom left corner

      Point.Set_pixel(Current_position.left, Current_position.bottom);
      Draw_handle(DC, Point);

      // bottom right corner

      Point.Set_pixel(Current_position.right, Current_position.bottom);
      Draw_handle(DC, Point);
      }
   }


