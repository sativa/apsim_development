//---------------------------------------------------------------------------
#ifndef TDiffH
#define TDiffH
#include "TSEGTable.h"
#include <stdexcept>
#include <string>
//---------------------------------------------------------------------------
// derived from TSEGTable, this creates a dataset that is calculated
// as the different between 2 datasets.
//---------------------------------------------------------------------------
class TDiff : public TSEGTable
   {
   private:
      AnsiString fieldNameToAnalyse;
      TSEGTable* mySecondDataSet;

      void __fastcall setFieldName(AnsiString fieldName);
      void __fastcall setSecondDataSet(TSEGTable* second);

      virtual bool createFields(void) throw(std::runtime_error);
      virtual void storeRecords(void) throw(std::runtime_error);

   public:
      __fastcall TDiff(TComponent* owner);
      __fastcall ~TDiff();

   __published:
      __property AnsiString fieldName = {read=fieldNameToAnalyse, write=setFieldName};
      __property TSEGTable* secondDataSet = {read=mySecondDataSet, write=setSecondDataSet};
   };
#endif
