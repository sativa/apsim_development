//---------------------------------------------------------------------------
#ifndef KWTestH
#define KWTestH
#include "RealSet.h"
#include "DataProcessor.h"
//---------------------------------------------------------------------------
// derived from DataProcessor, this creates a dataset that represents a
// KW test of 2 distributions. of the source dataset.
//---------------------------------------------------------------------------
class KWTest : public DataProcessor
   {
   private:
      virtual void createFields(TDataSet* source, TDataSet* result);
      virtual void process(TDataSet* source, TDataSet* result);

      void AddDistributionFromTable(TDataSet* table, std::vector<RealSet>& distributions,
                                    const std::string& fieldName);

   public:
      KWTest(const std::string& type) : DataProcessor(type) { };
   };
#endif
