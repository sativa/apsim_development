#include <graph\global.h>
#pragma hdrstop

IMPLEMENT_CASTABLE1 (Drawable_object, Interactive_object);
IMPLEMENT_STREAMABLE1 (Drawable_object, Interactive_object);

// *******************************************************************
      void Drawable_object::Streamer::Write(opstream& os) const {
// *******************************************************************

//  Short description:
//    Writes an instance of Drawable_object to the passed ipstream.

//  Notes:

//  Changes:
//      DPH 23/6/94

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   WriteBaseObject( (Interactive_object*) GetObject(), os);

   os << GetObject()->Is_visible;
   os << GetObject()->Pen;
   os << GetObject()->Brush;
   }

// *******************************************************************
      void *Drawable_object::Streamer::Read(ipstream& is, uint32 /*version*/) const {
// *******************************************************************

//  Short description:
//    Reads an instance of Drawable_object from the passed ipstream.

//  Notes:

//  Changes:
//      DPH 23/6/94

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   ReadBaseObject ( (Interactive_object*) GetObject(), is);

   is >> GetObject()->Is_visible;
   is >> GetObject()->Pen;
   is >> GetObject()->Brush;

   return GetObject();
   }

// *******************************************************************
       Drawable_object::Drawable_object(void)  {
// *******************************************************************

//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 10/4/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   Is_visible = TRUE;
   }

// *******************************************************************
       void Drawable_object::Paint(GDC& DC)  {
// *******************************************************************

//  Short description:
//    Draw the object on the specified device.

//  Notes:

//  Changes:
//    DPH 10/4/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   if (Is_visible)
      {
      // Change the current pen and brush.

      DC.Select_object(Pen);
      DC.Select_object(Brush);

      // Select the coordinate system to use.

      DC.Set_coord(Coord);

      // Go paint the object.

      Paint_object(DC);
      };
   }


