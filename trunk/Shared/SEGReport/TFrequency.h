//---------------------------------------------------------------------------

#ifndef TFrequencyH
#define TFrequencyH
#include "TSEGTable.h"
#include <stdexcept>
#include <vector>
#include <string>
#include <map>
#include "ReportMacros.h"
//---------------------------------------------------------------------------
// derived from TSEGTable, this filters an existing dataset.
//---------------------------------------------------------------------------
class TFrequency : public TSEGTable
   {
   private:
      ReportMacros macros;
      TStringList* Labels;
      TStringList* Filters;
      void __fastcall setLabels(TStringList* Labels);
      void __fastcall setFilters(TStringList* Filters);

      virtual bool createFields(void) throw(std::runtime_error);
      virtual void storeRecords(void) throw(std::runtime_error);

   public:
      __fastcall TFrequency(TComponent* owner);
      __fastcall ~TFrequency();

   __published:
      __property TStringList* labels = {read=Labels, write=setLabels};
      __property TStringList* filters = {read=Filters, write=setFilters};
   };
#endif
