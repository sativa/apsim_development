//---------------------------------------------------------------------------
#ifndef StatsH
#define StatsH
#include "DataProcessor.h"

//---------------------------------------------------------------------------
// derived from DataProcessor, this creates a dataset that provides some
// simple stats on a source dataset variable. e.g. mean, median, percentiles...
//---------------------------------------------------------------------------
class Stats : public DataProcessor
   {
   private:
      virtual void createFields(TDataSet* source, TDataSet* result);
      virtual void process(TDataSet* source, TDataSet* result);

   public:
      Stats(const std::string& type) : DataProcessor(type) { }
   };
#endif
