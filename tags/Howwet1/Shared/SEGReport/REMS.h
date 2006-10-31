//---------------------------------------------------------------------------

#ifndef REMSH
#define REMSH
#include <stdexcept>
#include <ADOdb.hpp>
#include "DataProcessor.h"
//---------------------------------------------------------------------------
// derived from DataProcessor, this class reads in experimental data from
// the REMS database.
//---------------------------------------------------------------------------
class REMS : public DataProcessor
   {
   private:
      std::string fileName;
      std::vector<std::string> allExperimentNames;
      std::vector<std::string> allTreatmentNames;
      std::vector<int> experimentIDs;
      std::vector<int> treatmentIDs;

      virtual void createFields(TDataSet* source, TDataSet* result);
      virtual void process(TDataSet* source, TDataSet* result);

      void lookupExperimentNames();
      void lookupTreatmentNames(const std::string& experimentName);
      TADOQuery* createQuery(const std::string treatmentName,
                             const std::string& dataSourceName);
   public:
      REMS(const std::string& type) : DataProcessor(type) { }

      std::vector<std::string> getExperimentNames() {return allExperimentNames;}
      std::vector<std::string> getTreatmentNames() {return allTreatmentNames;}
   };
#endif
