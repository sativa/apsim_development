//---------------------------------------------------------------------------
#ifndef ExcelReaderH
#define ExcelReaderH
#include "DataProcessor.h"
//---------------------------------------------------------------------------
// derived from DataProcessor, this class reads in experimental data from
// an EXCEL spreadsheet.
//---------------------------------------------------------------------------
class ExcelReader : public DataProcessor
   {
   private:
      std::vector<std::string> fieldNames, fieldValues;
      virtual void createFields(TDataSet* source, TDataSet* result);
      virtual void process(TDataSet* source, TDataSet* result);

   public:
      ExcelReader(const std::string& type) : DataProcessor(type) { }
   };
#endif
