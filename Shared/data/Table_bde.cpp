#include "table base.h"
#include <general\string functions.h>
#include "filter.h"

// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
Table_bde::Table_bde (void)
   {
   Table_ptr = new TTable(NULL);
   }

// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void Table_bde::Open (const char* File_name, const char* Database_name)
   {
   Table_ptr->DatabaseName = Database_name;
   Table_ptr->TableName = Table_name;
   Table_ptr->Active = true;
   }

// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void Table_bde::Close (void)
   {
   delete Table_ptr;
   }

// ------------------------------------------------------------------
//  Short description:
//    return data to caller using an ASCII field name as lookup.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void Table_bde::Get_data_by_name (const char* Field_name, string& Value)
   {
   Value = String(Table_ptr->Field_values[Field_name]).c_str();
   }

// ------------------------------------------------------------------
//  Short description:
//    return data to caller using an index into the field names list as lookup.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void Table_bde::Get_data_by_index (unsigned int Field_index, string& Value)
   {
   Value = String(Table_ptr->Fields[Field_index]).c_str();
   }

// ------------------------------------------------------------------
//  Short description:
//    Goto first record.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void Table_bde::First ()
   {
   Table_base::First();
   Table_ptr->First();
   }

// ------------------------------------------------------------------
//  Short description:
//    Goto last record.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void Table_bde::Last ()
   {
   Table_base::Last();
   Table_ptr->Last();
   }

// ------------------------------------------------------------------
//  Short description:
//    move up or down a specified number of records.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void Table_bde::MoveBy (int Num_records)
   {
   Table_base::MoveBy (Num_records);
   Table_ptr->MoveBy(Num_records);
   }

// ------------------------------------------------------------------
//  Short description:
//    return true if positioned at first record.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
bool Table_bde::At_first ()
   {
   return (Current_record_number == 1);
   }

// ------------------------------------------------------------------
//  Short description:
//    return true if at end of data

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
bool Table_bde::At_end_of_data ()
   {
   return End_of_data;
   }



