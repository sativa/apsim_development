//---------------------------------------------------------------------------
#ifndef RegressionH
#define RegressionH

#include "DataProcessor.h"
//---------------------------------------------------------------------------
// derived from DataProcessor, this creates a dataset that represents a
// regression.
//---------------------------------------------------------------------------
class Regression : public DataProcessor
   {
   private:
      virtual void createFields(TDataSet* source, TDataSet* result);
      virtual void process(TDataSet* source, TDataSet* result);

   public:
      Regression(const std::string& type) : DataProcessor(type) { }

   };
#endif
