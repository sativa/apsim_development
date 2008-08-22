#ifndef QUERY_H
#define QUERY_H

#include <data\results table.h>
#include <data\database.h>

// ------------------------------------------------------------------
//  Short description:
//    this class encapsulates a field filter which can be used in
//    a query

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
class Field_filter
   {
   private:
      vector <string> Extract_fields;

   public:
      Field_filter(void);
      void Add (const char* Field_name);
      void Clear (void);
      vector<string>* Get_extract_fields (void);

      // called by query
      Record* Field_filter::Create_record (Table_base& Table_obj);
   };

// ------------------------------------------------------------------
//  Short description:
//    this class encapsulates a record filter which can be used in
//    a query

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
class Record_filter
   {
   private:
      enum Filter_operator {LT, LE, EQ, GE, GT, NE};
      class Filter
         {
         public:
            string Field_name;
            Filter_operator Operator;
            string Value;
         };
      vector <Filter> Filter_list;
      bool Is_OR;
   public:
      Record_filter (void);

      // add a filter
      void Add (const char* Field_name, const char* Operator, const char* Value);

      // called by query_table
      bool Keep_record (Table_base& Table_obj);

      // set the AND_OR operator (OR is the default)
      void Set_AND (bool Is_AND = true)
         {Is_OR = (!Is_AND);}
      void Set_OR (bool Is_OR = true)
         {Is_OR = Is_OR;}
   };

// ------------------------------------------------------------------
//  Short description:
//    this class is encapsulates a query on a database.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
class DATA_EXPORT Query_table : public Results_table
   {
   public:

      Query_table (void);

      // methods to set filters.
      void Set_field_filter (Field_filter* Filter_ptr);
      void Set_record_filter (Record_filter* Filter_ptr);

      // execute query
      void Execute (Table_base& Table_obj);


   private:
      // column filters.
      Field_filter* Field_filter_ptr;
      Record_filter* Record_filter_ptr;

   };

#endif