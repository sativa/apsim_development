//---------------------------------------------------------------------------

#ifndef TCumulativeH
#define TCumulativeH
#include "TSEGTable.h"
#include <stdexcept>
#include <vector>
#include <string>
#include <map>
//---------------------------------------------------------------------------
// derived from TSEGTable, this adds a cumulative column for each source column
//---------------------------------------------------------------------------
class TCumulative : public TSEGTable
   {
   private:
      virtual bool createFields(void) throw(std::runtime_error);
      virtual void storeRecords(void) throw(std::runtime_error);

   public:
      __fastcall TCumulative(TComponent* owner);
      __fastcall ~TCumulative();
   };
#endif
