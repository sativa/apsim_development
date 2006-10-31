//---------------------------------------------------------------------------

#ifndef TRecFilterH
#define TRecFilterH
#include "TSEGTable.h"
#include <stdexcept>
#include <vector>
#include <string>
//---------------------------------------------------------------------------
// derived from TSEGTable, this filters an existing dataset.
//---------------------------------------------------------------------------
class TRecFilter : public TSEGTable
   {
   private:
      bool FirstRecord;
      bool LastRecord;
      int RecordNumber;
      void __fastcall setFirstRecord(bool first);
      void __fastcall setLastRecord(bool last);
      void __fastcall setRecordNumber(int rec);

      virtual bool createFields(void) throw(std::runtime_error);
      virtual void storeRecords(void) throw(std::runtime_error);

   public:
      __fastcall TRecFilter(TComponent* owner);
      __fastcall ~TRecFilter();

   __published:
      __property bool firstRecord = {read=FirstRecord, write=setFirstRecord};
      __property bool lastRecord = {read=LastRecord, write=setLastRecord};
      __property int recordNumber = {read=RecordNumber, write=setRecordNumber};
   };
#endif
