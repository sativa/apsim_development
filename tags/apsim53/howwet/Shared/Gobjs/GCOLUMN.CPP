#include <consts.h>
#include <gobjs\gcolumn.h>
#include <values.h>

IMPLEMENT_CASTABLE(GColumn);
IMPLEMENT_STREAMABLE(GColumn);

// *******************************************************************
      void GColumn::Streamer::Write(opstream& os) const {
// *******************************************************************

//  Short description:
//    Writes an instance of GColumn to the passed opstream.

//  Notes:

//  Changes:
//      DPH 28/9/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   os << GetObject()->Heading;
   os << GetObject()->Unit;
   os << GetObject()->Num_elements;
   os << GetObject()->Max_elements;
   os << GetObject()->Minimum;
   os << GetObject()->Maximum;

   }

// *******************************************************************
      void *GColumn::Streamer::Read(ipstream& is, uint32 /*version*/) const {
// *******************************************************************

//  Short description:
//    Reads an instance of GColumn from the passed ipstream.

//  Notes:

//  Changes:
//      DPH 28/9/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   is >> GetObject()->Heading;
   is >> GetObject()->Unit;
   is >> GetObject()->Num_elements;
   is >> GetObject()->Max_elements;
   is >> GetObject()->Minimum;
   is >> GetObject()->Maximum;

   return GetObject();
   }

// *******************************************************************
      GColumn::GColumn (void)  {
// *******************************************************************

//  Short description:
//    constructor

//  Notes:

//  Changes:
//      DPH 6/2/96

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   Num_elements = 0;
   Minimum = MAXFLOAT;
   Maximum = -MAXFLOAT;
   }

// *******************************************************************
      GColumn::~GColumn (void)  {
// *******************************************************************

//  Short description:
//    destructor

//  Notes:

//  Changes:
//      DPH 6/2/96

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   }



