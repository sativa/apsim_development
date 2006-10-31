#if !defined(__shadeobj_h)              // Sentry, use file only if it's not already included.
#define __shadeobj_h


// *******************************************************************
      class Shade_object  {
// *******************************************************************

//  Short description:
//    Object responsible for shading an area on DC depending
//    on a value passed to it.  Used in the animated picture
//    window.

//  Notes:

//  Changes:
//    DPH 21/4/96

//  Internal variables
//    none

// -------------------- Declaration code section ----------------------

   protected:
      TColor Light_colour;             // lightest fill colour
      TColor Dark_colour;              // darkest fill colour
      float Light_value;               // value at lightest fill colour
      float Dark_value;                // value at darkest fill colour
      TPoint Fill_point;               // point to perform fill.
      uint8 Interp (float Fill_value,
                    float Dark_value,
                    float Light_value,
                    uint8 Dark_colour,
                    uint8 Light_colour);
                                       // interpolate for a new colour based on
                                       // fill value.

   public:
      Shade_object(void);              // constructor
      void Setup(TPoint& Fill_point,
                 TColor& Light_colour,
                 TColor& Dark_colour,
                 float Light_value,
                 float Dark_value);    // setup object.
      void Paint(TDC& DC,
                 float Fill_value);    // paint the DC in the appropriate shade
                                       // based on fill value   
   };

#endif
