//---------------------------------------------------------------------------

#ifndef TProbabilityH
#define TProbabilityH
#include "TSEGTable.h"
#include <stdexcept>
#include <vector>
#include <string>
#include <map>
//---------------------------------------------------------------------------
// derived from TSEGTable, this creates a dataset that represents a
// probability distribution of the source dataset.
//---------------------------------------------------------------------------
class TProbability : public TSEGTable
   {
   private:
      bool isExceedence;
      AnsiString fieldNameToAnalyse;

      void __fastcall setExceedence(bool exceedence);
      void __fastcall setFieldName(AnsiString fieldName);
      virtual void createFields(void) throw(std::runtime_error);
      virtual void storeRecords(void) throw(std::runtime_error);

   public:
      __fastcall TProbability(TComponent* owner);
      __fastcall ~TProbability();

   __published:
      __property bool exceedence = {read=isExceedence, write=setExceedence};
      __property AnsiString fieldName = {read=fieldNameToAnalyse, write=setFieldName};
   };
#endif
