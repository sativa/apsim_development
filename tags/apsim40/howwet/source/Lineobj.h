#if !defined(__lineobj_h)              // Sentry, use file only if it's not already included.
#define __lineobj_h


// *******************************************************************
      class Line_object  {
// *******************************************************************

//  Short description:
//    Object responsible for drawing a line and shading above and below
//    the line all depending on a value passed to it.  Used in the
//    animated picture window to animate the profile.

//  Notes:

//  Changes:
//    DPH 21/4/96

//  Internal variables
//    none

// -------------------- Declaration code section ----------------------

   protected:
      TColor Above_colour;             // lightest fill colour
      TColor Below_colour;             // darkest fill colour
      TPoint Top_left_point;           // top left corner of object
      TPoint Bottom_left_point;        // bottom left corner of object
      TPoint Bottom_right_point;       // bottom right corner of object
      TPoint Top_right_point;          // top right corner of object.
      float Top_value;                 // value at top of object
      float Bottom_value;              // value at bottom of object

      TPoint Old_point1;               // First point for previous line.
      TPoint Old_point2;               // second point for previous line.

      TPoint Interp (TPoint& Top_point,
                     TPoint& Bottom_point,
                     float Point_value);
                                       // return an interpolated point between
                                       // top and bottom for point value.

   public:
      Line_object(void);               // constructor
      void Setup(TColor& Above_col,
                 TColor& Below_col,
                 TPoint& Top_left,
                 TPoint& Bottom_left,
                 TPoint& Bottom_right,
                 TPoint& Top_right,
                 float Top_val,
                 float Bottom_val);    // setup object.
      void Paint(TDC& DC,
                 float Fill_value);    // paint the DC in the appropriate shade
                                       // based on fill value
   };

#endif
