//---------------------------------------------------------------------------
#ifndef DepthH
#define DepthH

#include "DataProcessor.h"

//---------------------------------------------------------------------------
// derived from TSEGTable, this creates a dataset that represents a
// probability distribution of the source dataset.
//---------------------------------------------------------------------------
class Depth : public DataProcessor
   {
   private:
      std::vector<std::string> variableNames;
      int numLayers;

      virtual void createFields(TDataSet* source, TDataSet* result);
      virtual void process(TDataSet* source, TDataSet* result);

      void discoverVariables(TDataSet* source);

   public:
      Depth(const std::string& type) : DataProcessor(type) { };
   };
#endif
