//---------------------------------------------------------------------------

#ifndef TKWTestH
#define TKWTestH
#include "RealSet.h"
#include "TSEGTable.h"
#include <stdexcept>
#include <vector>
#include <string>
#include <map>
//---------------------------------------------------------------------------
// derived from TSEGTable, this creates a dataset that represents a
// probability distribution of the source dataset.
//---------------------------------------------------------------------------
class TKWTest : public TSEGTable
   {
   private:
      TSEGTable* source2;
      AnsiString fieldNameToAnalyse;

      void __fastcall setFieldName(AnsiString fieldName);
      void __fastcall setSource2Dataset(TSEGTable* sourceDataset);
      virtual bool createFields(void) throw(std::runtime_error);
      virtual void storeRecords(void) throw(std::runtime_error);
      void AddDistributionFromTable(TSEGTable* table, std::vector<RealSet>& distributions);

   public:
      __fastcall TKWTest(TComponent* owner);
      __fastcall ~TKWTest();

      virtual void setProperty(const std::string& propertyName,
                               const std::string& propertyValue);

   __published:
      __property TSEGTable* Source2 = {read=source2, write=setSource2Dataset};
      __property AnsiString fieldName = {read=fieldNameToAnalyse, write=setFieldName};
   };
#endif
