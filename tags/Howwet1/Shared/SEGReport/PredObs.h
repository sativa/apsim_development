//---------------------------------------------------------------------------
#ifndef PredObsH
#define PredObsH

#include "DataProcessor.h"
//---------------------------------------------------------------------------
// derived from DataProcessor, this class does predicted / observed data
// matching.
//---------------------------------------------------------------------------
class PredObs : public DataProcessor
   {
   private:
      virtual void createFields(TDataSet* source, TDataSet* result);
      virtual void process(TDataSet* source, TDataSet* result);

      TDataSet* getObservedData(const std::string& obsFileName);
      bool isKeyField(const std::vector<std::string>& keyFieldNames, const std::string& fieldName);

   public:
      PredObs(const std::string& type) : DataProcessor(type) { };
   };
#endif
