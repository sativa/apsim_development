#include <owl\owlpch.h>
#pragma hdrstop

#include <consts.h>
#include <gobjsx\rectdrag.h>

// *******************************************************************
       void Rectangle_drag::Check_points (Points_array& Points, int Point_modified)  {
// *******************************************************************

//  Short description:
//    Check the array of points passed in for validity.  Modify any
//    points that are not valid.

//  Notes:

//  Changes:
//    DPH 16/10/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   // There will always be 4 points in the point array.

   switch (Point_modified)
      {
      case 0 : Points[1].y = Points[0].y;
               Points[3].x = Points[0].x;
               break;
      case 1 : Points[0].y = Points[1].y;
               Points[2].x = Points[1].x;
               break;
      case 2 : Points[3].y = Points[2].y;
               Points[1].x = Points[2].x;
               break;
      case 3 : Points[2].y = Points[3].y;
               Points[0].x = Points[3].x;
               break;
      }
   }

// *******************************************************************
       void Rectangle_drag::Set_points (TRect& R)  {
// *******************************************************************

//  Short description:
//    Setup the points array from the rectangled passed in.

//  Notes:

//  Changes:
//    DPH 16/10/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   Points.Flush();

   TPoint P0(R.left, R.top);
   Points.Add(P0);

   TPoint P1(R.right, R.top);
   Points.Add(P1);

   TPoint P2(R.right, R.bottom);
   Points.Add(P2);

   TPoint P3(R.left, R.bottom);
   Points.Add(P3);
   }

// *******************************************************************
       void Rectangle_drag::Get_points (TRect& R)  {
// *******************************************************************

//  Short description:
//    Return points to caller.

//  Notes:

//  Changes:
//    DPH 16/10/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   R.left = Points[0].x;
   R.top = Points[0].y;
   R.right = Points[2].x;
   R.bottom = Points[2].y;
   }

