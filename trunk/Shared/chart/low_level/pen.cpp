#include "drawable.h"

/**# implementation Pen:: id(C_0853987254)
*/

// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 3/2/1997

// ------------------------------------------------------------------

Pen::Pen (void)
   {
   Colour.Red = 0;
   Colour.Green = 0;
   Colour.Blue = 0;            // black.
   Style = Solid;
   pen = new TChartPen;
   }

// ------------------------------------------------------------------
//  Short description:
//    destructor

//  Notes:

//  Changes:
//    DPH 3/2/1997

// ------------------------------------------------------------------

Pen::~Pen (void)
   {
//   delete pen;
   }

// ------------------------------------------------------------------
//  Short description:
//    return a tchartpen to caller.

//  Notes:

//  Changes:
//    DPH 3/2/1997

// ------------------------------------------------------------------

TChartPen* Pen::Get_TChartpen (void)
   {
   pen->Color = TColor(Colour);
   pen->Style = TPenStyle(Style);
   return pen;
   }

