//---------------------------------------------------------------------------
#ifndef FrequencyH
#define FrequencyH

#include "DataProcessor.h"
//---------------------------------------------------------------------------
// derived from DataProcessor, this filters an existing dataset.
//---------------------------------------------------------------------------
class Frequency : public DataProcessor
   {
   private:
      virtual void createFields(TDataSet* source, TDataSet* result);
      virtual void process(TDataSet* source, TDataSet* result);

   public:
      Frequency(const std::string& type) : DataProcessor(type) { }
    };
#endif
