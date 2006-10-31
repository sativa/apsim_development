#include "query.h"
#include <stdio.h>
#include <values.h>  // MAXFLOAT
#include <general\stl functions.h>
#include <general\string functions.h>

#define Missing_value "*"

// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
Field_filter::Field_filter (void)
   {
   }

// ------------------------------------------------------------------
//  Short description:
//    add a field name to filter

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void Field_filter::Add (const char* Field_name)
   {
   Extract_fields.push_back (string(Field_name));
   }

// ------------------------------------------------------------------
//  Short description:
//    clear all fields out of filter.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void Field_filter::Clear (void)
   {
   Extract_fields.erase (Extract_fields.begin(), Extract_fields.end());
   }

// ------------------------------------------------------------------
//  Short description:
//    clear all fields out of filter.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
vector<string>* Field_filter::Get_extract_fields (void)
   {
   return &Extract_fields;
   }

// ------------------------------------------------------------------
//  Short description:
//      Create a record object that satisfies the current filter.
//      Called from Query_table object.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
Record* Field_filter::Create_record (Table_base& Table_obj)
   {
   Record* Record_ptr = new Record;

   // loop through all fields in filter.
   for (vector<string>::iterator Iter = Extract_fields.begin();
                                 Iter != Extract_fields.end();
                                 Iter++)
      {
      string Value;
      Table_obj.Get_data_by_name ((*Iter).c_str(), Value);
      Record_ptr->Add(Value);
      }
   return Record_ptr;
   }

// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
Record_filter::Record_filter (void)
   {
   Is_OR = true;
   }

// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void Record_filter::Add (const char* Field_name, const char* Operator, const char* Value)
   {
   Filter f;
   f.Field_name = Field_name;
   if (strcmpi(Operator, "<") == 0)
      f.Operator = LT;
   else if (strcmpi(Operator, "<=") == 0)
      f.Operator = LE;
   else if (strcmpi(Operator, ">") == 0)
      f.Operator = GT;
   else if (strcmpi(Operator, ">=") == 0)
      f.Operator = GE;
   else if (strcmpi(Operator, "<>") == 0)
      f.Operator = NE;
   else
      f.Operator = EQ;

   f.Value = Value;
   Filter_list.push_back(f);
   }

// ------------------------------------------------------------------
//  Short description:
//    return true if calling Query_table object should keep this record.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
bool Record_filter::Keep_record (Table_base& Table_obj)
   {
   // loop through all the Filters

   bool Result = (!Is_OR);
   for (vector<Filter>::iterator Iter = Filter_list.begin();
                                 Iter != Filter_list.end();
                                 Iter++)
      {
      // get current value for field.
      string Value;
      Table_obj.Get_data_by_name ((*Iter).Field_name.c_str(), Value);

      float Value1 = atof(Value.c_str());
      float Value2 = atof((*Iter).Value.c_str());

      // do comparisons.
      bool Match = false;
      switch ((*Iter).Operator)
         {
         case LT : Match = (Value1 < Value2);
                   break;
         case LE : Match = (Value1 <= Value2);
                   break;
         case EQ : if (Is_numerical(Value.c_str()))
                      Match = (Value1 == Value2);       // numerical comparision
                   else
                      Match = (Value == (*Iter).Value); // string comparison
                   break;
         case GE : Match = (Value1 >= Value2);
                   break;
         case GT : Match = (Value1 > Value2);
                   break;
         case NE : if (Is_numerical(Value.c_str()))
                      Match = (Value1 != Value2);       // numerical comparision
                   else
                      Match = (Value != (*Iter).Value); // string comparison
                   break;
         }

      // combine match with result depending on Is_OR
      if (Is_OR)
         {
         Result = (Result || Match);
         if (Result)
            return true;
         }
      else
         Result = (Result && Match);
      }
   return true;
   }

// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
Query_table::Query_table (void)
   {
   Record_filter_ptr = NULL;
   Field_filter_ptr = NULL;
   }

// ------------------------------------------------------------------
//  Short description:
//    set the field filter.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void Query_table::Set_field_filter (Field_filter* Filter_ptr)
   {
   Field_filter_ptr = Filter_ptr;
   }

// ------------------------------------------------------------------
//  Short description:
//    set the field filter.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void Query_table::Set_record_filter (Record_filter* Filter_ptr)
   {
   Record_filter_ptr = Filter_ptr;
   }

// ------------------------------------------------------------------
//  Short description:
//    execute the query for a single table.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void Query_table::Execute (Table_base& Table_obj)
   {
   // create a backup field filter in case the user hasn't supplied one
   Field_filter Backup_field_filter;

   // open our table.
   Table_obj.Open();

   // set the results table title and field names.
   Set_title (Table_obj.Get_title().c_str());
   if (Field_filter_ptr != NULL)
      Set_field_names (Field_filter_ptr->Get_extract_fields());
   else
      Set_field_names (Backup_field_filter.Get_extract_fields());

   // loop through all records in table.
   while (!Table_obj.At_end_of_data())
      {
      bool Keep_record;

      // do we want to keep this record?
      if (Record_filter_ptr != NULL)
         Keep_record = Record_filter_ptr->Keep_record (Table_obj);
      else
         Keep_record = true;

      if (Keep_record)
         {
         Record* New_record;

         // yes - call field filter to create a record.
         if (Field_filter_ptr != NULL)
            New_record = Field_filter_ptr->Create_record (Table_obj);
         else
            New_record = Backup_field_filter.Create_record (Table_obj);

         // store record in results table.
         Add_result (New_record);
         }
      // goto next record.
      Table_obj.Next();
      }
   Table_obj.Close();
   }

