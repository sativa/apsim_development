//---------------------------------------------------------------------------

#ifndef CumulativeH
#define CumulativeH
#include "DataProcessor.h"
//---------------------------------------------------------------------------
// derived from DataProcessor, this adds a cumulative column for each source column
//---------------------------------------------------------------------------
class Cumulative : public DataProcessor
   {
   private:
      virtual void createFields(TDataSet* source, TDataSet* result);
      virtual void process(TDataSet* source, TDataSet* result);

   public:
      Cumulative(const std::string& type) : DataProcessor(type) { };
   };
#endif
