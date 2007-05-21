#if !defined(__DRAGPTS_H)
#define __DRAGPTS_H

#include <classlib\arrays.h>
typedef TArrayAsVector <TPoint> Points_array;
// *******************************************************************
       class   Drag_points  {
// *******************************************************************

//  Short description:
//    Object for encapsulating a set of user draggable points.

//  Notes:

//  Changes:
//    DPH 16/10/95

// ---------------------- Declaration section -----------------------



   public :

//   Data
   protected:
      bool Dragging_on;                // Is dragging mode currently on.
      bool Movable;                    // Is object movable
      bool Sizable;                    // Is object sizable
      Points_array Points;             // List of current points.
      TDC *DC_ptr;                     // Our DC when we need one.
      int Point_being_sized;           // Point number being resized
      bool Object_being_moved;         // is object being moved?
      bool Cursor_is_size;             // Is the size cursor displayed.
      TPoint Old_move_point;           // previous point where cursor was.
                                       // Used for moving an object.

//   Member functions
   protected:
      int Mouse_on_point(TPoint& P);   // Is the specified mouse point on any
                                       // point in our array?  Return the point
                                       // number if so.  Return -1 if not.

      bool Mouse_on_object(TPoint& P); // Return TRUE if mouse is on object.

      // ****** Derivable routines ******

      virtual void Check_points(Points_array& P, int Point_modified)  {};
                                       // Allow derived object to modify points
      virtual void Draw_temporary_object(TDC& DC, Points_array& P);
                                       // Allow derived object to draw temporary
                                       // object at specified points.  The default
                                       // is that all points will be connected by
                                       // a line as a closed polygon.

   public:
      Drag_points(void);               // constructor

      // ****** Callable routines ******

      void Set_points(Points_array& P_array);
                                       // Set the array of points.
      void Get_points(Points_array& P_array);
                                       // Return the current array of points.
      void Set_dragging_on(bool Mode = TRUE)
         {
         Dragging_on = Mode;
         };                            // set the dragging mode.
      void Set_flags(bool Move, bool Size)
         {
         Movable = Move;
         Sizable = Size;
         };                            // Is object movable and sizable.
      bool Get_dragging_on(void)
         {
         return Dragging_on;
         };                            // return the dragging mode to caller.
      bool Get_is_dragging(void)
         {
         return (Point_being_sized >= 0 || Object_being_moved);
         };                            // return TRUE if a drag process is underway.

      bool Get_mouse_over_point(void)
         {
         return Cursor_is_size;
         };                            // return TRUE if mouse is over a point.

      // ****** EVENT routines ******

      void Left_button_down(TWindow& Parent, TPoint& P);
                                       // Left button was pressed.
      void Left_button_up(TPoint& P);
                                       // Right button was pressed.
      void Mouse_move(TPoint& P);
                                       // Mouse was moved.
   };

#endif
