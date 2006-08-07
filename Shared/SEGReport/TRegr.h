//---------------------------------------------------------------------------

#ifndef TRegrH
#define TRegrH
#include "TSEGTable.h"
#include <stdexcept>
#include <vector>
#include <string>
#include <map>
//---------------------------------------------------------------------------
// derived from TSEGTable, this creates a dataset that represents a
// regression.
//---------------------------------------------------------------------------
class TRegr : public TSEGTable
   {
   private:
      AnsiString XFieldName;
      AnsiString YFieldName;

      void __fastcall setXFieldName(AnsiString fieldName);
      void __fastcall setYFieldName(AnsiString fieldName);
      virtual bool createFields(void) throw(std::runtime_error);
      virtual void storeRecords(void) throw(std::runtime_error);

   public:
      __fastcall TRegr(TComponent* owner);
      __fastcall ~TRegr();

   __published:
      __property AnsiString xFieldName = {read=XFieldName, write=setXFieldName};
      __property AnsiString yFieldName = {read=YFieldName, write=setYFieldName};

   };
#endif
