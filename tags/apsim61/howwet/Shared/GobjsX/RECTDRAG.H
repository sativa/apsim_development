#if !defined(__RECTDRAG_H)
#define __RECTDRAG_H

#include <gobjsx\dragpts.h>
// *******************************************************************
       class   Rectangle_drag : public Drag_points  {
// *******************************************************************

//  Short description:
//    Object for encapsulating a draggable rectangle

//  Notes:

//  Changes:
//    DPH 16/10/95

// ---------------------- Declaration section -----------------------

   public :

//   Data
   protected:

//   Member functions
   protected:
      // ****** Derivable routines ******

      void Check_points(Points_array& P, int Point_modified);
                                       // Allow derived object to modify points

   public:
      // ****** Callable routines ******

      void Set_points(TRect& Rect);    // Set the array of points.
      void Get_points(TRect& Rect);    // Return points to caller.
   };

#endif