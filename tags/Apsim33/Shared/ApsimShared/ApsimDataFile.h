//---------------------------------------------------------------------------
#ifndef ApsimDataFileH
#define ApsimDataFileH
#include <stdexcept>
#include <fstream>
#include <map>
//---------------------------------------------------------------------------
// derived from TSEGTable, this class reads all data from 1 or more
// APSIM output files.
//---------------------------------------------------------------------------
class __declspec(dllexport) ApsimDataFile
   {
   public:
      ApsimDataFile(const std::string& filename) throw(std::runtime_error);

      // return a list of field names to caller.
      void getFieldNames(std::vector<std::string>& fieldNames) const;

      // return a list of field names to caller.
      void getFieldUnits(std::vector<std::string>& fieldUnits) const;

      // return the values on the current record.
      void getFieldValues(std::vector<std::string>& fieldValues) const;

      // return the date on the current record.
      TDateTime getDate(void) const throw(std::runtime_error);

      // return a single field value for the specified field name.  Returns
      // a blank string if not found.
      std::string getFieldValue(const std::string& fieldName) const;

      // return a single field value for the specified field index.  Throws
      // on an invalid index.
      std::string getFieldValue(unsigned fieldIndex) const throw(std::runtime_error);

      // position at first record.
      bool first(void);

      // advance to the next record.
      bool next(void);

      // advance to the last record.
      bool last(void);

      // return a list of constant names to caller.
      void getConstantNames(std::vector<std::string>& names) const;

      // return the value of the specified constant.  Returns a blank
      // string if constant doesn't exist.
      std::string getConstant(const std::string& name) const;

   private:
      std::string fileName;
      std::ifstream in;
      std::vector<std::string> fieldNames;
      std::vector<std::string> fieldUnits;
      std::vector<std::string> fieldValues;
      typedef std::map<std::string, std::string, std::less<std::string> > Constants;
      Constants constants;
      unsigned firstRecordPos;
      unsigned yearI;
      unsigned dayOfYearI;
      unsigned dayOfMonthI;
      unsigned monthI;

      void open(void) throw(std::runtime_error);
      std::istream& getline(std::string& line);

      void readAndStoreFields(const std::string& filename) throw(std::runtime_error);
      void readAndStoreRecords(const std::string& filename) throw (std::runtime_error);
      void readApsimHeader(std::istream& in) throw(std::runtime_error);
      bool readNextRecord(std::istream& in) throw(std::runtime_error);
      void lookForDateField(void);

   };
#endif
