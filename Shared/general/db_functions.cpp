//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "db_functions.h"
#include "string_functions.h"

#pragma package(smart_init)
using namespace std;
//---------------------------------------------------------------------------
// Adds the specified fields to the specified dataset.
// Does NOT removes any existing fields of the dataset
// The values are used to determine the datatype of the field.
//---------------------------------------------------------------------------
void addDBFields(TDataSet* dataset,
                 vector<string>& fieldNames,
                 vector<string>& fieldValues) throw(runtime_error)
   {
   // make sure we have the same number of values as we do field names.
   if (fieldValues.size() != fieldNames.size())
      throw runtime_error("The number of field names does not equal the number "
                          "of field values.");

   dataset->Active = false;
   for (unsigned fieldI = 0; fieldI < fieldNames.size(); fieldI++)
      addDBField(dataset, fieldNames[fieldI], fieldValues[fieldI]);
   }

//---------------------------------------------------------------------------
// Adds the specified field to the specified dataset.
// The fieldValue is used to determine the datatype of the field.
// A field is not added if it already exists.
//---------------------------------------------------------------------------
void addDBField(TDataSet* dataset,
                               const string& fieldName,
                               const string& fieldValue)
   {
   if (dataset->FieldDefs->IndexOf(fieldName.c_str()) == -1)
      {
      TFieldDef *fieldDef = dataset->FieldDefs->AddFieldDef();
      fieldDef->Name = fieldName.c_str();

      if (Is_numerical(fieldValue.c_str()))
         {
         if (fieldValue.find(".") == string::npos)
            fieldDef->DataType = ftInteger;
         else
            fieldDef->DataType = ftFloat;
         }
      else
         {
         try
            {
            // the constructor below will throw if fieldValue is not
            // a date.
            TDateTime(fieldValue.c_str(), TDateTime::Date);
            fieldDef->DataType = ftDate;
            }
         catch (...)
            {
            fieldDef->DataType = ftString;
            }
         }
      }
   }

//---------------------------------------------------------------------------
// Return a list of field names to caller for the specified dataset.
//---------------------------------------------------------------------------
void getDBFieldNames(TDataSet* dataset,
                                    std::vector<std::string>& fieldNames)
   {
   for (int field = 0; field < dataset->FieldCount; field++)
      fieldNames.push_back(dataset->Fields->Fields[field]->FieldName.c_str());
   }
// ------------------------------------------------------------------
// This routine loops through all records from First to Eof on the
// specified dataset and returns a vector of numbers for the specified field.
// Will through runtime_error if cannot convert the values to a float.
// ------------------------------------------------------------------
void getDBFieldValues(TDataSet* dataset,
                                     const string& fieldName,
                                     vector<double>& values) throw (runtime_error)
   {
   try
      {
      dataset->First();
      while (!dataset->Eof)
         {
         values.push_back( StrToFloat(dataset->FieldValues[fieldName.c_str()]) );
         dataset->Next();
         }
      }
   catch (...)
      {
      throw runtime_error("Cannot convert field to numerical value."
                          "\nField: " + fieldName +
                          "\nValue: " + AnsiString(dataset->FieldValues[fieldName.c_str()]).c_str() +
                          "\nDataset: " + dataset->Name.c_str());
      }
   }
// ------------------------------------------------------------------
// This routine loops through all records from First to Eof on the
// specified dataset and returns a vector of numbers for the specified field.
// Will through runtime_error if cannot convert the values to a float.
// ------------------------------------------------------------------
void getDBFieldValues(TDataSet* dataset,
                      const string& fieldName,
                      vector<unsigned>& values)
   {
   try
      {
      dataset->First();
      while (!dataset->Eof)
         {
         values.push_back( StrToInt(dataset->FieldValues[fieldName.c_str()]) );
         dataset->Next();
         }
      }
   catch (...)
      {
      throw runtime_error("Cannot convert field to numerical value."
                          "\nField: " + fieldName +
                          "\nValue: " + AnsiString(dataset->FieldValues[fieldName.c_str()]).c_str() +
                          "\nDataset: " + dataset->Name.c_str());
      }
   }

// ------------------------------------------------------------------
// This routine loops through all records from First to Eof on the
// specified dataset and returns a vector of numbers for the specified field.
// ------------------------------------------------------------------
void getDBFieldValues(TDataSet* dataset,
                                     const std::string& fieldName,
                                     vector<string>& values)
   {
   dataset->First();
   while (!dataset->Eof)
      {
      values.push_back(AnsiString(dataset->FieldValues[fieldName.c_str()]).c_str());
      dataset->Next();
      }
   }

//---------------------------------------------------------------------------
// Copy a record from the source dataset to the destination dataset.
// Returns true if a record was copied.
// The newly appended record will be the current record after this operation.
//---------------------------------------------------------------------------
bool copyDBRecord(TDataSet* source, TDataSet* destination)
   {
   if (!source->Eof)
      {
      destination->Append();
      for (int field = 0; field < source->FieldCount; field++)
         destination->Fields->Fields[field] = source->Fields->Fields[field];
      destination->Post();
      return true;
      }
   else
      return false;
   }
//---------------------------------------------------------------------------
// Append a new record to the specified dataset.
// Then add the specified values to the new record and post the changes.
//---------------------------------------------------------------------------
void appendDBRecord(TDataSet* dataset,
                                   vector<string>& fieldNames,
                                   vector<string>& fieldValues) throw(runtime_error)
   {
   // make sure we have the same number of values as we do field names.
   if (fieldValues.size() != fieldNames.size())
      throw runtime_error("The number of field names does not equal the number "
                          "of field values.");

   unsigned fieldI;

   dataset->Append();
   try
      {
      for (fieldI = 0; fieldI < fieldNames.size(); fieldI++)
         {
         if (fieldValues[fieldI] != "")
            dataset->FieldValues[fieldNames[fieldI].c_str()] = fieldValues[fieldI].c_str();
         }
      }
   catch (...)
      {
      dataset->Post();
      throw runtime_error("Invalid data found in column: " + fieldNames[fieldI] + ".");
      }
   dataset->Post();
   }


