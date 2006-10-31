#include <consts.h>
#include <gobjs\grealcol.h>

IMPLEMENT_CASTABLE(GReal_column);
IMPLEMENT_STREAMABLE(GReal_column);

// *******************************************************************
      void GReal_column::Streamer::Write(opstream& os) const {
// *******************************************************************

//  Short description:
//    Writes an instance of GReal_column to the passed opstream.

//  Notes:

//  Changes:
//      DPH 28/9/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   WriteBaseObject( (GColumn*) GetObject(), os);

   os.writeBytes(GetObject()->Real_array,
                 GetObject()->Num_elements * sizeof(float));
   }

// *******************************************************************
      void *GReal_column::Streamer::Read(ipstream& is, uint32 /*version*/) const {
// *******************************************************************

//  Short description:
//    Reads an instance of GReal_column from the passed ipstream.

//  Notes:

//  Changes:
//      DPH 28/9/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   ReadBaseObject( (GColumn*) GetObject(), is);
   GetObject()->Init(GetObject()->Max_elements);

   is.readBytes(GetObject()->Real_array,
                GetObject()->Num_elements * sizeof(float));

   return GetObject();
   }

// *******************************************************************
      void GReal_column::Init (int Max_items)  {
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

   Real_array = new float[Max_items];
   Max_elements = Max_items;
   }

// *******************************************************************
      GReal_column::~GReal_column (void)  {
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

   delete [] Real_array;
   }


