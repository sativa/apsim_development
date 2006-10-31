#ifndef TABLE_BDE_H
#define TABLE_BDE_H

#include <fstream.h>
#include <data\data.h>
#include <data\filter.h>
#define MAX_RECORD_SIZE   3000
#define MAX_NUM_FIELDS     500


// ------------------------------------------------------------------
//  Short description:
//    this class is a bass class for all tables.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
class DATA_EXPORT Table_bde
   {
   public:

      Table_bde (void);
      virtual ~Table_bde (void) {};

      // data retrieval methods.
      virtual void Get_data_by_name (const char* Field_name, string& Value);
      virtual void Get_data_by_index (unsigned int Field_index, string& Value);

      // stream handling functions
      virtual void Open (const char* File_name,
                         const char* Database_name);
      virtual void Close (void);

      // navigation methods.
      virtual void First(void);
      virtual void Last(void);
      virtual void Next(void);
      virtual void Prior(void);
      virtual void MoveBy(int Num_records);

      // filter stuff
      Record_filter Filter;

   protected:
      TTable* Table_ptr;
      virtual void Read_field_names (void);

   };

#endif