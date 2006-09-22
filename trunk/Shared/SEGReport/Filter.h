//---------------------------------------------------------------------------
#ifndef FilterH
#define FilterH

#include "DataProcessor.h"
//---------------------------------------------------------------------------
// derived from DataProcessor, this class filters an existing dataset.
//---------------------------------------------------------------------------
class Filter : public DataProcessor
   {
   private:
      virtual void createFields(TDataSet* source, TDataSet* result);
      virtual void process(TDataSet* source, TDataSet* result);

   public:
      Filter(const std::string& type) : DataProcessor(type) { };
   };
#endif
