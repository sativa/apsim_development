//---------------------------------------------------------------------------

#ifndef TFilterH
#define TFilterH
#include "TSEGTable.h"
#include <stdexcept>
#include <vector>
#include <string>
#include <map>
#include "ReportMacros.h"
//---------------------------------------------------------------------------
// derived from TSEGTable, this filters an existing dataset.
//---------------------------------------------------------------------------
class TFilter : public TSEGTable
   {
   private:
      ReportMacros macros;
      AnsiString filterString;
      void __fastcall setFilter(AnsiString filter);

      virtual bool createFields(void) throw(std::runtime_error);
      virtual void storeRecords(void) throw(std::runtime_error);

   public:
      __fastcall TFilter(TComponent* owner);
      __fastcall ~TFilter();

      virtual void setProperty(const std::string& propertyName,
                               const std::string& propertyValue);

   __published:
      __property AnsiString filter = {read=filterString, write=setFilter};
   };
#endif
