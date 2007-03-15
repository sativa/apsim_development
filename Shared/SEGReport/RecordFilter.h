//---------------------------------------------------------------------------

#ifndef RecordFilterH
#define RecordFilterH

#include "DataProcessor.h"
//---------------------------------------------------------------------------
// This filters an existing dataset extracting specified records.
//---------------------------------------------------------------------------
class RecordFilter : public DataProcessor
   {
   private:
      virtual void createFields(TDataSet* source, TDataSet* result);
      virtual void process(TDataSet* source, TDataSet* result);

   public:
      RecordFilter(const std::string& type, TComponent* owner)
         : DataProcessor(type, owner) { };

   };
#endif
