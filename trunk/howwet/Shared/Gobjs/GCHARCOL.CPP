#include <consts.h>
#include <gobjs\gcharcol.h>

IMPLEMENT_CASTABLE(GChar_column);
IMPLEMENT_STREAMABLE(GChar_column);

// *******************************************************************
      void GChar_column::Streamer::Write(opstream& os) const {
// *******************************************************************

//  Short description:
//    Writes an instance of GChar_column to the passed opstream.

//  Notes:

//  Changes:
//      DPH 28/9/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   WriteBaseObject( (GColumn*) GetObject(), os);

   os.writeBytes(GetObject()->Char_array,
                 GetObject()->Num_elements * sizeof(Char));
   }

// *******************************************************************
      void *GChar_column::Streamer::Read(ipstream& is, uint32 /*version*/) const {
// *******************************************************************

//  Short description:
//    Reads an instance of GChar_column from the passed ipstream.

//  Notes:

//  Changes:
//      DPH 28/9/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   ReadBaseObject( (GColumn*) GetObject(), is);
   GetObject()->Init(GetObject()->Max_elements);

   is.readBytes(GetObject()->Char_array,
                GetObject()->Num_elements * sizeof(Char));

   return GetObject();
   }

// *******************************************************************
      void GChar_column::Init (int Max_items)  {
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

   Char_array = new Char[Max_items];
   Max_elements = Max_items;
   }

// *******************************************************************
      GChar_column::~GChar_column (void)  {
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

   delete [] Char_array;
   }


