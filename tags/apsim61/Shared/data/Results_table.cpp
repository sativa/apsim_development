#include "Results table.h"
#include <stdio.h>
#include <values.h>  // MAXFLOAT
#include <general\string functions.h>

#define Missing_value "*"

// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
Results_table::Results_table (void)
   {
   }

// ------------------------------------------------------------------
//  Short description:
//    open the table.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void Results_table::Open (void)
   {
   Record_set.erase (Record_set.begin(), Record_set.end());

   Table_base::Open();
   }

// ------------------------------------------------------------------
//  Short description:
//    add a record to the table.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void Results_table::Add_result (Record* rec)
   {
   Record_set.push_back (rec);
   }

// ------------------------------------------------------------------
//  Short description:
//    set the field names in the table.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void Results_table::Set_field_names (vector<string>* Fields)
   {
   Field_names = *Fields;
   }

// ------------------------------------------------------------------
//  Short description:
//    goto last record on stream.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void Results_table::Last (void)
   {
   Current_record_number = Record_set.size();
   Read_next_record();
   }

// ------------------------------------------------------------------
//  Short description:
//    return true if current record is at end of dataset.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
bool Results_table::At_last (void)
   {
   return (Current_record_number == Record_set.size());
   }

// ------------------------------------------------------------------
//  Short description:
//    read next line from stream.
//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void Results_table::Read_next_record (void)
   {
   string St;
   Build_string(Record_set[Current_record_number]->Values, " ", St);
   strcpy(Current_record, St.c_str());
   }

// ------------------------------------------------------------------
//  Short description:
//    set the title
//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void Results_table::Set_title (const char* Tit)
   {
   Title = Tit;
   }


