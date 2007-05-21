//---------------------------------------------------------------------------
#ifndef ProbabilityH
#define ProbabilityH
#include "DataProcessor.h"
//---------------------------------------------------------------------------
// derived from data processor, this class creates probability distributions
// from source data.
//---------------------------------------------------------------------------
class Probability : public DataProcessor
   {
   public:
      Probability(const std::string& type, TComponent* owner)
         : DataProcessor(type, owner) { }
   private:
      virtual void createFields(TDataSet* source, TDataSet* result);
      virtual void process(TDataSet* source, TDataSet* result);
   };
#endif
