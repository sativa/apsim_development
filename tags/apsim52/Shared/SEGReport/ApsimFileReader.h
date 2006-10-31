//---------------------------------------------------------------------------
#ifndef ApsimFileReaderH
#define ApsimFileReaderH
#include "DataProcessor.h"
//---------------------------------------------------------------------------
// this class reads all data from 1 or more APSIM output files.
//---------------------------------------------------------------------------
class ApsimFileReader : public DataProcessor
   {
   public:
      ApsimFileReader(const std::string& type) : DataProcessor(type) { }
      virtual void createFields(TDataSet* source, TDataSet* result);
      virtual void process(TDataSet* source, TDataSet* result);

   private:
      void readAndStoreData(const std::string& filename, TDataSet* data);
      void readAndStoreRecords(const std::string& filename,
                               std::vector<std::string>& factorNames,
                               std::vector<std::string>& factorValues);
      void readApsimHeader(std::istream& in,
                           std::vector<std::string>& fieldNames,
                           std::string& title);
      bool readNextRecord(istream& in, vector<string>& fieldValues);
      void splitTitleIntoFactors(const std::string& title,
                                 std::vector<std::string>& factorNames,
                                 std::vector<std::string>& factorValues);
   };
#endif
