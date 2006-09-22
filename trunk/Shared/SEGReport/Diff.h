//---------------------------------------------------------------------------
#ifndef DiffH
#define DiffH

#include "DataProcessor.h"

//---------------------------------------------------------------------------
// derived from DataProcessor, this creates a dataset that is calculated
// as the different between 2 datasets.
//---------------------------------------------------------------------------
class Diff : public DataProcessor
   {
   private:
      virtual void createFields(TDataSet* source, TDataSet* result);
      virtual void process(TDataSet* source, TDataSet* result);

   public:
      Diff(const std::string& type) : DataProcessor(type) { };
   };
#endif
