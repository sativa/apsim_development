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

      void __fastcall setFilter(AnsiString filter);
      AnsiString __fastcall getFilter(void);

      virtual void createFields(void) throw(std::runtime_error);
      virtual void storeRecords(void) throw(std::runtime_error);
      void __fastcall onSourceDataChange(TDataSet* dataset);

   public:
      __fastcall TFilter(TComponent* owner);
      __fastcall ~TFilter();

   __published:
      __property AnsiString filter = {read=getFilter, write=setFilter};
   };
#endif
