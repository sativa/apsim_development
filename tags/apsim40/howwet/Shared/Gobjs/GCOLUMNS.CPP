#include <consts.h>
#include <gobjs\gcolumns.h>
#include <values.h>

IMPLEMENT_CASTABLE(GColumns);
IMPLEMENT_STREAMABLE(GColumns);

// *******************************************************************
      void GColumns::Streamer::Write(opstream& os) const {
// *******************************************************************

//  Short description:
//    Writes an instance of GColumns to the passed opstream.

//  Notes:

//  Changes:
//      DPH 28/9/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   os << GetObject()->Max_columns;
   os << GetObject()->Own_all_data;
   os << (GString) GetObject()->Title;
   os << GetObject()->Minimum;
   os << GetObject()->Maximum;
   os << GetObject()->Write_all_data;
   os << GetObject()->Num_columns;

   for (int Column_number = 0;
        Column_number < GetObject()->Num_columns;
        Column_number++)
      {
      if (GetObject()->Write_all_data)
         {
         os << GetObject()->Column_list[Column_number];
         }
      os << GetObject()->Table_number_list[Column_number];
      os << GetObject()->Column_number_list[Column_number];
      }
   }

// *******************************************************************
      void *GColumns::Streamer::Read(ipstream& is, uint32 /*version*/) const {
// *******************************************************************

//  Short description:
//    Reads an instance of GColumns from the passed ipstream.

//  Notes:

//  Changes:
//      DPH 28/9/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   is >> GetObject()->Max_columns;
   GetObject()->Init(GetObject()->Max_columns);

   is >> GetObject()->Own_all_data;

   GString Tit;
   is >> Tit;
   strcpy(GetObject()->Title, Tit.c_str());
   is >> GetObject()->Minimum;
   is >> GetObject()->Maximum;
   is >> GetObject()->Write_all_data;
   is >> GetObject()->Num_columns;

   GColumn *Column_ptr;
   for (int Column_number = 0;
        Column_number < GetObject()->Num_columns;
        Column_number++)
      {
      if (GetObject()->Write_all_data)
         {
         is >> Column_ptr;
         GetObject()->AddAt(Column_ptr, Column_number);
         }
      is >> GetObject()->Table_number_list[Column_number];
      is >> GetObject()->Column_number_list[Column_number];
      }

   return GetObject();
   }

// *******************************************************************
      void GColumns::Init (int Max_items)  {
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

   Column_list = new GColumn*[Max_items];
   Column_number_list = new int [Max_items];
   Table_number_list = new int [Max_items];

   Num_columns = 0;
   Max_columns = Max_items;
   Own_all_data = TRUE;
   Minimum = MAXFLOAT;
   Maximum = -MAXFLOAT;
   Write_all_data = TRUE;
   }

// *******************************************************************
      GColumns::~GColumns (void)  {
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

   if (Own_all_data)
      {
      for (int Column_number = 0;
           Column_number < Num_columns;
           Column_number++)
         delete Column_list[Column_number];
      }

   delete [] Column_list;
   delete [] Column_number_list;
   delete [] Table_number_list;
   }

// *******************************************************************
      void GColumns::Flush (void)  {
// *******************************************************************

//  Short description:
//    flush all items in all lists.

//  Notes:

//  Changes:
//      DPH 6/2/96

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   for (int Column_number = 0;
        Column_number < Num_columns;
        Column_number++)
      {
      Column_list[Column_number]->Flush();
      }
   }

// *******************************************************************
      void GColumns::Delete_all_columns (void)  {
// *******************************************************************

//  Short description:
//    delete all columns in array.

//  Notes:

//  Changes:
//      DPH 6/2/96

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   for (int Column_number = 0;
        Column_number < Num_columns;
        Column_number++)
      delete Column_list[Column_number];

   Num_columns = 0;
   }

// *******************************************************************
      void GColumns::AddAt(GColumn* Col, int loc,
                           int Table_number, int Column_number)  {
// *******************************************************************

//  Short description:
//    add a column to column list.

//  Notes:

//  Changes:
//      DPH 6/2/96

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   assert (loc >= 0 && loc < Max_columns);
   assert (Col != NULL);
   Column_list[loc] = Col;
   Column_number_list[loc] = Column_number;
   Table_number_list[loc] = Table_number;
   Minimum = min(Minimum, Col->Get_minimum());
   Maximum = max(Maximum, Col->Get_maximum());
   }

// *******************************************************************
      void GColumns::Refresh_data(GColumns_array_dummy& Arr_dummy)  {
// *******************************************************************

//  Short description:
//    refresh all columns of data from the table passed in.

//  Notes:

//  Changes:
//      DPH 6/2/96

//  Calls:

//  Internal variables
      GColumns *Table_ptr;             // pointer to table.
      GColumn* Column_ptr;             // pointer to column
      GColumns_array& Arr = *Arr_dummy.Arr_ptr;


// -------------------- Executable code section ----------------------

   for (int Column_number = 0;
        Column_number < Num_columns;
        Column_number++)
      {
      Table_ptr =Arr[Table_number_list[Column_number]];
      Column_ptr = (*Table_ptr)[Column_number_list[Column_number]];
      AddAt(Column_ptr,
            Column_number,
            Table_number_list[Column_number],
            Column_number_list[Column_number]);
      }
   }

