//---------------------------------------------------------------------------
#ifndef db_functionsH
#define db_functionsH
#include <vector>
#include <string>
#include <db.hpp>
#include <stdexcept>

//---------------------------------------------------------------------------
// Adds the specified fields to the specified dataset.
// Does NOT removes any existing fields of the dataset
// The values are used to determine the datatype of the field.
// A field is not added if it already exists.
//---------------------------------------------------------------------------
void addDBFields(TDataSet* dataset,
                 std::vector<std::string>& fieldNames,
                 std::vector<std::string>& fieldValues) throw(std::runtime_error);

//---------------------------------------------------------------------------
// Adds the specified field to the specified dataset.
// The fieldValue is used to determine the datatype of the field.
// A field is not added if it already exists.
//---------------------------------------------------------------------------
void addDBField(TDataSet* dataset,
                const std::string& fieldName,
                const std::string& fieldValue);

//---------------------------------------------------------------------------
// Return a list of field names to caller for the specified dataset.
//---------------------------------------------------------------------------
void getDBFieldNames(TDataSet* dataset,
                     std::vector<std::string>& fieldNames);

// ------------------------------------------------------------------
// This routine loops through all records from First to Eof on the
// specified dataset and returns a vector of numbers for the specified field.
// Will through runtime_error if cannot convert the values to a float.
// ------------------------------------------------------------------
void getDBFieldValues(TDataSet* dataset,
                      const std::string& fieldName,
                      std::vector<double>& values) throw (std::runtime_error);
// ------------------------------------------------------------------
// This routine loops through all records from First to Eof on the
// specified dataset and returns a vector of numbers for the specified field.
// ------------------------------------------------------------------
void getDBFieldValues(TDataSet* dataset,
                      const std::string& fieldName,
                      std::vector<std::string>& values);

//---------------------------------------------------------------------------
// Copy a record from the source dataset to the destination dataset.
// Returns true if a record was copied.
// The newly appended record will be the current record after this operation.
//---------------------------------------------------------------------------
bool copyDBRecord(TDataSet* source, TDataSet* destination);

//---------------------------------------------------------------------------
// Append a new record to the specified dataset.
// Then add the specified values to the new record and post the changes.
//---------------------------------------------------------------------------
void appendDBRecord(TDataSet* dataset,
                    std::vector<std::string>& fieldNames,
                    std::vector<std::string>& fieldValues) throw(std::runtime_error);

//---------------------------------------------------------------------------
// Return a DB value to caller - as a string.
//---------------------------------------------------------------------------
string getDBValue(TDataSet* dataset, const std::string& fieldName)
   {
   if (dataset->FieldValues[fieldName.c_str()].IsNull())
      return "";
   else
      return AnsiString(dataset->FieldValues[fieldName.c_str()]).c_str();
   }

#endif
