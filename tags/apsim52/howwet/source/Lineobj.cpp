#include <owl\owlpch.h>
#pragma hdrstop

#include "lineobj.h"


// *******************************************************************
      Line_object::Line_object(void)  {
// *******************************************************************

//  Short description:
//    constructor.

//  Notes:

//  Changes:
//    DPH 21/4/96

//  Internal variables
//    none

// -------------------- Declaration code section ----------------------

   Old_point1.x = 0;
   Old_point1.y = 0;
   Old_point2.x = 0;
   Old_point2.y = 0;
   }

// *******************************************************************
      void Line_object::Setup(TColor& Above_col,
                              TColor& Below_col,
                              TPoint& Top_left,
                              TPoint& Bottom_left,
                              TPoint& Bottom_right,
                              TPoint& Top_right,
                              float Top_val,
                              float Bottom_val)  {
// *******************************************************************

//  Short description:
//    setup object.

//  Notes:

//  Changes:
//    DPH 21/4/96

//  Internal variables
//    none

// -------------------- Declaration code section ----------------------

   Above_colour = Above_col;
   Below_colour = Below_col;
   Top_left_point = Top_left;
   Bottom_left_point = Bottom_left;
   Bottom_right_point = Bottom_right;
   Top_right_point = Top_right;
   Top_value = Top_val;
   Bottom_value = Bottom_val;
   }

// *******************************************************************
      void Line_object::Paint (TDC& DC,
                                float Point_value)  {
// *******************************************************************

//  Short description:
//    paint the object on the DC.

//  Notes:

//  Changes:
//    DPH 21/4/96

//  Internal variables
//    none

// -------------------- Declaration code section ----------------------

   // remove old line if necessary.

   if (Old_point1.x != 0 || Old_point1.y != 0)
      {
      TPen Rub_out_pen(TColor(255, 255, 255));
      DC.SelectObject(Rub_out_pen);
      DC.MoveTo(Old_point1);
      DC.LineTo(Old_point2);
      }

   // Get first point for our line

   Old_point1 = Interp(Top_left_point,
                       Bottom_left_point,
                       Point_value);
   Old_point2 = Interp(Top_right_point,
                       Bottom_right_point,
                       Point_value);

   // draw in interpolated line in grey.

   TPen Line_pen (TColor(128, 128, 128));
   DC.SelectObject(Line_pen);
   DC.MoveTo(Old_point1);
   DC.LineTo(Old_point2);

   if (Old_point1.y > Top_left_point.y)
      {
      // Fill above our line in the above colour.

      TPoint Fill_point;
      Fill_point.x = (Old_point1.x + Old_point2.x) / 2;
      Fill_point.y = (Old_point1.y + Old_point2.y) / 2 - 2;
      TBrush Above_brush(Above_colour);
      DC.SelectObject(Above_brush);
      DC.FloodFill(Fill_point, TColor(128, 128, 128));
      }

   if (Old_point1.y < Bottom_left_point.y)
      {

      // Fill below our line in the below colour.

      TPoint Fill_point;
      Fill_point.x = (Old_point1.x + Old_point2.x) / 2;
      Fill_point.y = (Old_point1.y + Old_point2.y) / 2 + 2;
      TBrush Below_brush(Below_colour);
      DC.SelectObject(Below_brush);
      DC.FloodFill(Fill_point, TColor(128, 128, 128));
      }
   }

// *******************************************************************
      TPoint Line_object::Interp (TPoint& Top_point,
                                  TPoint& Bottom_point,
                                  float Point_value)  {
// *******************************************************************

//  Short description:
//    return an interpolated point between top and bottom for point value.

//  Notes:

//  Changes:
//    DPH 21/4/96

//  Internal variables
//    none

// -------------------- Declaration code section ----------------------

   int x = Top_point.x + abs(Point_value - Top_value) *
          ( (Bottom_point.x - Top_point.x) * 1.0 / (Bottom_value - Top_value) );
   int y = Top_point.y + abs(Point_value - Top_value) *
          ( (Bottom_point.y - Top_point.y) * 1.0 / (Bottom_value - Top_value) );

   return TPoint(x, y);
   }

