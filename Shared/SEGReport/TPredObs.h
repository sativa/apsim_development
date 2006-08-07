//---------------------------------------------------------------------------
#ifndef TPredObsH
#define TPredObsH
#include "TSEGTable.h"
#include <stdexcept>
#include <string>
//---------------------------------------------------------------------------
// derived from TSEGTable, this creates a dataset that provides joins
// two
//---------------------------------------------------------------------------
class TPredObs : public TSEGTable
   {
   private:
      TSEGTable* predDataset;
      TSEGTable* obsDataset;
      TStrings* keyFieldNames;

      void __fastcall setPredDataset(TSEGTable* predDataset);
      void __fastcall setObsDataset(TSEGTable* obsDataset);
      void __fastcall setKeyFieldNames(TStrings* keyFieldNames);

      virtual bool createFields(void) throw(std::runtime_error);
      virtual void storeRecords(void) throw(std::runtime_error);

   public:
      __fastcall TPredObs(TComponent* owner);
      __fastcall ~TPredObs();

   __published:
      __property TSEGTable* predData = {read=predDataset, write=setPredDataset};
      __property TSEGTable* obsData = {read=obsDataset, write=setObsDataset};
      __property TStrings* keyFields = {read=keyFieldNames, write=setKeyFieldNames};
   };
#endif
