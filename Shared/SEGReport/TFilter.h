//---------------------------------------------------------------------------

#ifndef TFilterH
#define TFilterH
#include "TSEGTable.h"
#include <stdexcept>
#include <vector>
#include <string>
#include <map>
//---------------------------------------------------------------------------
// derived from TSEGTable, this filters an existing dataset.
//---------------------------------------------------------------------------
class TFilter : public TSEGTable
   {
   private:
      AnsiString columnName;
      AnsiString columnValue;
      TStrings* columnNames;
      TStrings* columnValues;

      void __fastcall setColumnName(AnsiString name);
      void __fastcall setColumnValue(AnsiString value);

      virtual void createFields(void) throw(std::runtime_error);
      virtual void storeRecords(void) throw(std::runtime_error);

      // ------------------------------------------------------------------
      // Fill the column names list.
      // ------------------------------------------------------------------
      void getColumnNames();

      // ------------------------------------------------------------------
      // Fill the column values list.
      // ------------------------------------------------------------------
      void getColumnValues();

   public:
      __fastcall TFilter(TComponent* owner);
      __fastcall ~TFilter();

   __published:
      __property AnsiString fieldName = {read=columnName, write=setColumnName};
      __property AnsiString fieldValue = {read=columnValue, write=setColumnValue};
      __property TStrings* fieldNames = {read=columnNames};
      __property TStrings* fieldValues = {read=columnValues};
   };
#endif
