#include <consts.h>
#include <gobjs\gdatecol.h>
#include <values.h>

IMPLEMENT_CASTABLE(GDate_column);
IMPLEMENT_STREAMABLE(GDate_column);

// *******************************************************************
      void GDate_column::Streamer::Write(opstream& os) const {
// *******************************************************************

//  Short description:
//    Writes an instance of GDate_column to the passed opstream.

//  Notes:

//  Changes:
//      DPH 28/9/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   WriteBaseObject( (GColumn*) GetObject(), os);

   os.writeBytes(GetObject()->Date_array,
                 GetObject()->Num_elements * sizeof(GDate));
   }

// *******************************************************************
      void *GDate_column::Streamer::Read(ipstream& is, uint32 /*version*/) const {
// *******************************************************************

//  Short description:
//    Reads an instance of GDate_column from the passed ipstream.

//  Notes:

//  Changes:
//      DPH 28/9/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   ReadBaseObject( (GColumn*) GetObject(), is);
   GetObject()->Init(GetObject()->Max_elements);

   is.readBytes(GetObject()->Date_array,
                GetObject()->Num_elements * sizeof(GDate));

   return GetObject();
   }

// *******************************************************************
      void GDate_column::Init (int Max_items)  {
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

   Date_array = new GDate[Max_items];
   Max_elements = Max_items;
   Minimum = MAXFLOAT;
   Maximum = -MAXFLOAT;
   }

// *******************************************************************
      GDate_column::~GDate_column (void)  {
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

   delete [] Date_array;
   }


