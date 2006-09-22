//---------------------------------------------------------------------------
#ifndef SOIH
#define SOIH
#include "DataProcessor.h"
//---------------------------------------------------------------------------
// derived from DataProcessor, this adds an SOI column
//---------------------------------------------------------------------------
class SOI : public DataProcessor
   {
   private:
      std::vector<std::string> phaseNames;
      typedef std::map<std::string, unsigned, std::less<std::string> > Phases;
      Phases phases;
      bool allOtherYears;

      virtual void createFields(TDataSet* source, TDataSet* result);
      virtual void process(TDataSet* source, TDataSet* result);

      std::string getSowYearFieldName(TDataSet* data) const throw (std::runtime_error);
      void readSoiData(const string& soiFilename);
      std::string getPhase(unsigned Year, unsigned Month);
      bool keepPhase(const string& phaseName, const std::vector<std::string>& phaseNamesToKeep);

   public:
      SOI(const std::string& type) : DataProcessor(type) { }
   };
#endif
