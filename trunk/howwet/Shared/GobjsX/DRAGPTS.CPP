#include <owl\owlpch.h>
#pragma hdrstop

#include <consts.h>
#include <gobjsx\dragpts.h>

#define HIT_MARGIN  5                  // Margin around point allowed for a hit.

// *******************************************************************
       Drag_points::Drag_points(void)
          : Points(10, 0, 10)  {
// *******************************************************************

//  Short description:
//    constructor.

//  Notes:

//  Changes:
//    DPH 16/10/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   Dragging_on = TRUE;
   Cursor_is_size = FALSE;
   Point_being_sized = -1;
   Object_being_moved = FALSE;
   DC_ptr = NULL;
   Movable = FALSE;
   Sizable = FALSE;
   }

// *******************************************************************
       void Drag_points::Draw_temporary_object(TDC& DC, Points_array& P) {
// *******************************************************************

//  Short description:
//    Allow derived object to draw temporary object at specified points.  The default
//    is that all points will be connected by a line as a closed polygon.

//  Notes:

//  Changes:
//    DPH 16/10/95

//  Calls:

//  Internal variables
   int Num_points;                     // Number of points we're dealing with.
   TPoint *P_array;                    // Pointer to a tpoint array.

// -------------------- Executable code section ----------------------

   // Get number of points and create working tpoint array.

   Num_points = P.GetItemsInContainer();
   P_array = new TPoint[Num_points];

   // Fill working tpoint array.

   for (int Point = 0;
        Point < Num_points;
        Point++)
      P_array[Point] = P[Point];

   // Draw polyline of points.

   TPen Pen(TColor::White);
   TBrush Brush(TColor::White, BS_NULL);
   DC.SelectObject(Pen);
   DC.SelectObject(Brush);

   // Draw figure and close it.

   DC.Polyline(P_array, Num_points);
   DC.MoveTo(P[Num_points-1].x, P[Num_points-1].y);
   DC.LineTo(P[0].x, P[0].y);

   // Delete working array.

   delete [] P_array;
   }

// *******************************************************************
       void Drag_points::Set_points(Points_array& P)  {
// *******************************************************************

//  Short description:
//    Set the array of points.

//  Notes:

//  Changes:
//    DPH 16/10/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   Points.Flush();

   for (int Point = 0;
        Point < P.GetItemsInContainer();
        Point++)
      {
      Points.Add(P[Point]);
      }
   }

// *******************************************************************
       void Drag_points::Get_points(Points_array& P)  {
// *******************************************************************

//  Short description:
//    return copy of points.

//  Notes:

//  Changes:
//    DPH 16/10/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   P.Flush();

   for (int Point = 0;
        Point < Points.GetItemsInContainer();
        Point++)
      {
      P.Add(Points[Point]);
      }
   }

// *******************************************************************
       bool Drag_points::Mouse_on_object(TPoint& P)  {
// *******************************************************************

//  Short description:
//    Return TRUE if mouse is on object.

//  Notes:

//  Changes:
//    DPH 16/10/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   // assume object is a rectangle.

   RECT r;
   r.left = Points[0].x;
   r.top = Points[0].y;
   r.right = Points[2].x;
   r.bottom = Points[2].y;
   return PtInRect(&r, P);
   }

// *******************************************************************
       void Drag_points::Left_button_down(TWindow& Parent, TPoint& P)  {
// *******************************************************************

//  Short description:
//    Left button was pressed.

//  Notes:

//  Changes:
//    DPH 16/10/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   if (Dragging_on)
      {

      if (Sizable)
         {
         Point_being_sized = Mouse_on_point(P);
         if (Point_being_sized >= 0)
            {
            // Mouse is over point.  User is trying to size object.

            // Get a DC we can use and capture mouse.

            DC_ptr = new TDC(GetDC(Parent));
            DC_ptr->SetROP2(R2_XORPEN);
            SetCapture(Parent);

            // Draw the temporary object.

            Draw_temporary_object(*DC_ptr, Points);
            }
         }

      if (Point_being_sized == -1 && Movable)
         {
         Object_being_moved = Mouse_on_object(P);
         if (Object_being_moved >= 0)
            {
            // Mouse is over object.  User is trying to move object.

            // Get a DC we can use and capture mouse.

            DC_ptr = new TDC(GetDC(Parent));
            DC_ptr->SetROP2(R2_XORPEN);
            SetCapture(Parent);

            // Draw the temporary object.

            Draw_temporary_object(*DC_ptr, Points);
            Old_move_point = P;
            }
         }
      }
   }

// *******************************************************************
       void Drag_points::Left_button_up(TPoint& )  {
// *******************************************************************

//  Short description:
//    Left button was released.

//  Notes:

//  Changes:
//    DPH 16/10/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   if (Point_being_sized >= 0 || Object_being_moved)
      {
      // Release mouse and delete our DC object.

      ReleaseCapture();
      delete DC_ptr;

      Point_being_sized = -1;
      Object_being_moved = FALSE;
      }
   }

// *******************************************************************
       void Drag_points::Mouse_move(TPoint& P)  {
// *******************************************************************

//  Short description:
//    The mouse was moved.

//  Notes:

//  Changes:
//    DPH 16/10/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   if (Point_being_sized >= 0)
      {
      // Draw the temporary object - make it disappear.

      Draw_temporary_object(*DC_ptr, Points);

      // We are resizing a point.  Save current mouse position in points array.

      Points[Point_being_sized] = P;

      // Give derived object a chance to check the points.

      Check_points(Points, Point_being_sized);

      // Draw the temporary object

      Draw_temporary_object(*DC_ptr, Points);
      }

   else if (Object_being_moved)
      {
      // Draw the temporary object - make it disappear.

      Draw_temporary_object(*DC_ptr, Points);

      // We are moving the object.
      // Get the difference of current point from old move point.

      int Diff_x = P.x - Old_move_point.x;
      int Diff_y = P.y - Old_move_point.y;

      // Subtract these differences from each point in object.

      for (int Point = 0;
           Point < Points.GetItemsInContainer();
           Point++)
         {
         Points[Point].x += Diff_x;
         Points[Point].y += Diff_y;
         }
      Old_move_point = P;

      // Give derived object a chance to check the points.

      Check_points(Points, Point_being_sized);

      // Draw the temporary object

      Draw_temporary_object(*DC_ptr, Points);
      }

   else
      {
      int Point_number = Mouse_on_point(P);

      if (Point_number >= 0)
         {
         // We are not sizing the object.  Change cursor shape

         ::SetCursor(LoadCursor(NULL, IDC_SIZE));
         Cursor_is_size = TRUE;
         }

      else if (Cursor_is_size)
         {
         ::SetCursor(LoadCursor(NULL, IDC_ARROW));
         Cursor_is_size = FALSE;
         }
      }
   }

// *******************************************************************
       int Drag_points::Mouse_on_point(TPoint& P)  {
// *******************************************************************

//  Short description:
//    Is the specified mouse point on any point in our array?  Return
//    the point number if so.  Return -1 if not.

//  Notes:

//  Changes:
//    DPH 16/10/95

//  Calls:

//  Internal variables
      bool Found = FALSE;              // Found the point yet?
      TRect r;                         // Our rectangle for current point

// -------------------- Executable code section ----------------------

   int Point;
   for (Point = 0;
        Point < Points.GetItemsInContainer() && !Found;
        Point++)
      {
      // Create a rectangle for this point.

      r.left = Points[Point].x - HIT_MARGIN;
      r.top = Points[Point].y - HIT_MARGIN;
      r.right = Points[Point].x + HIT_MARGIN;
      r.bottom = Points[Point].y + HIT_MARGIN;

      Found = PtInRect(&r, P);
      }

   if (Found)
      return Point - 1;

   else
      return -1;
   }

