#include <owl\owlpch.h>
#pragma hdrstop

#include "shadeobj.h"


// *******************************************************************
      Shade_object::Shade_object(void)  {
// *******************************************************************

//  Short description:
//    constructor.

//  Notes:

//  Changes:
//    DPH 21/4/96

//  Internal variables
//    none

// -------------------- Declaration code section ----------------------

   }

// *******************************************************************
      void Shade_object::Setup(TPoint& Fill_pt,
                               TColor& Light_col,
                               TColor& Dark_col,
                               float Light_val,
                               float Dark_val)  {
// *******************************************************************

//  Short description:
//    setup object.

//  Notes:

//  Changes:
//    DPH 21/4/96

//  Internal variables
//    none

// -------------------- Declaration code section ----------------------

   Fill_point = Fill_pt;
   Light_colour = Light_col;
   Dark_colour = Dark_col;
   Light_value = Light_val;
   Dark_value = Dark_val;
   }

// *******************************************************************
      void Shade_object::Paint (TDC& DC,
                                float Fill_value)  {
// *******************************************************************

//  Short description:
//    paint the object on the DC.

//  Notes:

//  Changes:
//    DPH 21/4/96

//  Internal variables
//    none

// -------------------- Declaration code section ----------------------

   uint8 Red, Green, Blue;
   if (Fill_value >= Light_value)
      {
      Red = Interp (Fill_value, Light_value, Dark_value,
                    Light_colour.Red(), Dark_colour.Red());
      Green = Interp (Fill_value, Light_value, Dark_value,
                    Light_colour.Green(), Dark_colour.Green());
      Blue = Interp (Fill_value, Light_value, Dark_value,
                    Light_colour.Blue(), Dark_colour.Blue());
      }
   else
      {
      Red = 255;
      Green = 255;
      Blue = 255;
      }
   TColor Our_colour (TColor(Red, Green, Blue));
   TBrush Our_brush (Our_colour);
   DC.SelectObject(Our_brush);
   DC.FloodFill(Fill_point, TColor(128, 128, 128));
   }

// *******************************************************************
      uint8 Shade_object::Interp (float Fill_value,
                                  float Light_value,
                                  float Dark_value,
                                  uint8 Light_colour,
                                  uint8 Dark_colour)  {
// *******************************************************************

//  Short description:
//    interpolate for a new colour based on fill value.

//  Notes:

//  Changes:
//    DPH 21/4/96

//  Internal variables
//    none

// -------------------- Declaration code section ----------------------

   return Light_colour + abs(Fill_value - Light_value) *
          ( (Dark_colour - Light_colour) * 1.0 / (Dark_value - Light_value) );
   }

