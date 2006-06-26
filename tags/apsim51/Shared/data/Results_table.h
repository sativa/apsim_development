#ifndef RESULTS_TABLE_H
#define RESULTS_TABLE_H

#include <data\table base.h>

// ------------------------------------------------------------------
//  Short description:
//    this class encapsulates a recordset

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
class Record
    {
    public:
       vector<string> Values;

       void Add (string value)
          {
          Values.push_back(value);
          };
    };

// ------------------------------------------------------------------
//  Short description:
//    this class is used to store the results from a query.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
class DATA_EXPORT Results_table : public Table_base
   {
   public:

      Results_table (void);

      // stream handling functions
      void Open (void);
      bool At_last (void);

      // navigation methods.
      void Last(void);

      // called by Query routines to fill table.
      void Add_result (Record* Record_ptr);
      void Set_field_names (vector<string>* Fields);
      void Set_title (const char* Title);

   protected:
      vector <Record*> Record_set;

      void Read_field_names (void) {};
      void Read_next_record (void);

   };

#endif