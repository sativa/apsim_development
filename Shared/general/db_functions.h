//---------------------------------------------------------------------------
#ifndef db_functionsH
#define db_functionsH
#include <vector>
#include <string>
#include <general\general.h>

//---------------------------------------------------------------------------
// Adds the specified fields to the specified dataset.
// Does NOT removes any existing fields of the dataset
// The values are used to determine the datatype of the field.
// A field is not added if it already exists.
//---------------------------------------------------------------------------
void GENERAL_EXPORT addDBFields(TDataSet* dataset,
                                std::vector<std::string>& fieldNames,
                                std::vector<std::string>& fieldValues) throw(std::runtime_error);

//---------------------------------------------------------------------------
// Adds the specified field to the specified dataset.
// The fieldValue is used to determine the datatype of the field.
// A field is not added if it already exists.
//---------------------------------------------------------------------------
void GENERAL_EXPORT addDBField(TDataSet* dataset,
                               const std::string& fieldName,
                               const std::string& fieldValue);

//---------------------------------------------------------------------------
// Return a list of field names to caller for the specified dataset.
//---------------------------------------------------------------------------
void GENERAL_EXPORT getDBFieldNames(TDataSet* dataset,
                                    std::vector<std::string>& fieldNames);

// ------------------------------------------------------------------
// This routine loops through all records from First to Eof on the
// specified dataset and returns a vector of numbers for the specified field.
// Will through runtime_error if cannot convert the values to a float.
// ------------------------------------------------------------------
void GENERAL_EXPORT getDBFieldValues(TDataSet* dataset,  
                                     const std::string& fieldName,
                                     std::vector<double>& values) throw (std::runtime_error);
// ------------------------------------------------------------------
// This routine loops through all records from First to Eof on the
// specified dataset and returns a vector of numbers for the specified field.
// ------------------------------------------------------------------
void GENERAL_EXPORT getDBFieldValues(TDataSet* dataset,
                                     const std::string& fieldName,
                                     std::vector<std::string>& values);

//---------------------------------------------------------------------------
// Copy a record from the source dataset to the destination dataset.
// Returns true if a record was copied.
// The newly appended record will be the current record after this operation.
//---------------------------------------------------------------------------
bool GENERAL_EXPORT copyDBRecord(TDataSet* source, TDataSet* destination);

//---------------------------------------------------------------------------
// Append a new record to the specified dataset.
// Then add the specified values to the new record and post the changes.
//---------------------------------------------------------------------------
void GENERAL_EXPORT appendDBRecord(TDataSet* dataset,
                                   std::vector<std::string>& fieldNames,
                                   std::vector<std::string>& fieldValues) throw(std::runtime_error);

//---------------------------------------------------------------------------
// Append a series of new records to the specified dataset, one record
// for each value in the fieldValues container.
//---------------------------------------------------------------------------
void GENERAL_EXPORT addDBRecords(TDataSet* dataset,
                                 const std::string& fieldName,
                                 std::vector<std::string>& fieldValues);

//---------------------------------------------------------------------------
// Append a series of new records to the specified dataset, one record
// for each value in the fieldValues container.
//---------------------------------------------------------------------------
void GENERAL_EXPORT addDBRecords(TDataSet* dataset,
                                 const std::string& fieldName,
                                 std::vector<double>& fieldValues);
//---------------------------------------------------------------------------
// Stores a series values for a specified field in the record range First()
// to Eof().  This routine DOES NOT create any new records.
// Will throw if there are not enough records to store all fieldValues.
//---------------------------------------------------------------------------
void GENERAL_EXPORT storeDBRecords(TDataSet* dataset,
                                   const std::string& fieldName,
                                   std::vector<std::string>& fieldValues)  throw (std::runtime_error);
//---------------------------------------------------------------------------
// Stores a series values for a specified field in the record range First()
// to Eof().  This routine DOES NOT create any new records.
// Will throw if there are not enough records to store all fieldValues.
//---------------------------------------------------------------------------
void GENERAL_EXPORT storeDBRecords(TDataSet* dataset,
                                   const std::string& fieldName,
                                   std::vector<double>& fieldValues) throw (std::runtime_error);

#endif
