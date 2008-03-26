//---------------------------------------------------------------------------
#ifndef TAPSRecordH
#define TAPSRecordH
#include <string>
#include <vector>
#include <general\vcl_functions.h>
// ------------------------------------------------------------------
//  Short description:
//      class for encapsulating a record from a table.

//  Notes:

//  Changes:
//    DPH 5/2/98
//    dph 28/3/2001 Made pivot fields separate columns in record C336

// ------------------------------------------------------------------
class PACKAGE TAPSRecord
   {
   public:
      TAPSRecord (void);
      TAPSRecord (const TAPSRecord& from);
      TAPSRecord& operator=  (const TAPSRecord& from);

      // get/set field values.
      std::string getFieldValue(const std::string& FieldName) const;
      void   setFieldValue(const std::string& fieldName, const std::string& fieldvalue);
      void   clear(void);
      std::vector<std::string> getFieldNames(void) const {return fieldNames;}

      // dataset functionality
      void   read (TDataSet* dataset, std::vector<std::string>& fieldNames);
      void   write (TDataSet* dataset) const;

   private:
      std::vector<std::string> fieldNames;
      std::vector<std::string> fieldValues;

      void assign(const TAPSRecord& from);
   };

#endif
