#include "drawable.h"

/**# implementation Brush:: id(C_0853987306)
*/
// ------------------------------------------------------------------
//  Short description:
//    constructor.

//  Notes:

//  Changes:
//    DPH 3/2/1997

// ------------------------------------------------------------------
Brush::Brush (void)
   {
   Colour.Red = 255;
   Colour.Green = 255;
   Colour.Blue = 255;
   Style = Solid;
   brush = new TChartBrush;
   }

// ------------------------------------------------------------------
//  Short description:
//    constructor.

//  Notes:

//  Changes:
//    DPH 3/2/1997

// ------------------------------------------------------------------
Brush::~Brush (void)
   {
//   delete brush;
   }

// ------------------------------------------------------------------
//  Short description:
//    return a tbrush to caller.

//  Notes:

//  Changes:
//    DPH 3/2/1997

// ------------------------------------------------------------------

TChartBrush* Brush::Get_TChartBrush (void)
   {
   brush->Color = TColor(Colour);
   brush->Style = TBrushStyle(Style);
   return brush;
   }

