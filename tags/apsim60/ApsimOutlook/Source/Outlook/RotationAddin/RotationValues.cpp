//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "RotationValues.h"
#include <general\stristr.h>
#include <general\path.h>
#include <general\stl_functions.h>
#include <assert.h>
#include <sstream>
#include <iterator>

#pragma package(smart_init)
using namespace std;

string addPerYearToFieldName(string& fieldName);

// ------------------------------------------------------------------
// Return a single value across all files.  FieldValues specifies
// the year and field number while doAverage specifies if the
// value should be averaged within a year.
// ------------------------------------------------------------------
string RotationValues::getValue(const FileValues& fileValues,
                                bool doAverage,
                                unsigned numDataBlocks) const
   {
   float total = 0.0;
   unsigned numValues = 0;
   for (FileValues::const_iterator fileI = fileValues.begin();
                                   fileI != fileValues.end();
                                   fileI++)
      {
      float value;
      if (fileI->second.getValue(false, value))
         {
         total += value;
         numValues += fileI->second.getCount();
         }
      else
         {
         string stringValue;
         fileI->second.getValue(stringValue);
         return stringValue;
         }
      }
   float returnValue = total;
   if (doAverage)
      {
      if (numValues > 0)
         returnValue /= numValues;
      else
         returnValue = 0.0;
      }
   else
      {
      if (numDataBlocks > 0)
         returnValue /= numDataBlocks;
      else
         returnValue = 0.0;
      }

   static char buffer[20];
   sprintf(buffer, "%10.3f",returnValue);
   return &buffer[0];
   }

// ------------------------------------------------------------------
// Return true if the field is to be averaged for a particular year
// and file.
// ------------------------------------------------------------------
bool RotationValues::isAveragedField(const string& fieldName) const
   {
   if (cropFields.isCropField(fieldName))
      return true;

   // Look in fieldsToAverage first.
   vector<string>::const_iterator fieldI = find_if(fieldsToAverage.begin(),
                                                   fieldsToAverage.end(),
                                                   CaseInsensitiveStringComparison(fieldName));
   if (fieldI != fieldsToAverage.end())
      return true;

   else
      {
      // Make sure name is in fieldsDividedByNumFiles.
//      fieldI = find_if(fieldsDividedByNumFiles.begin(),
//                       fieldsDividedByNumFiles.end(),
//                       CaseInsensitiveStringComparison(fieldName));
//      if (fieldI == fieldsDividedByNumFiles.end())
//         throw runtime_error("Cannot find field: " + fieldName +
//                             " in the rotation.ini [fields] section.");
      return false;
      }
   }

// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
RotationValues::RotationValues(const vector<string>& fields)
   : fieldNames(fields), cropFields(NULL)
   {
   string st;
   settings.read("Rotation Fields|fields_averaged", st);
   Split_string(st, ",", fieldsToAverage);

   settings.read("Rotation Fields|fields_divided_by_num_files", st);
   Split_string(st, ",", fieldsDividedByNumFiles);
   }
// ------------------------------------------------------------------
// Add a rotation value for a particular year, field and file.
// ------------------------------------------------------------------
void RotationValues::addValue(unsigned year, unsigned fieldI, unsigned fileI, const string& value)
   {
   yearValues[year][fieldI][fileI].addValue(value);
   }
// ------------------------------------------------------------------
// Write all rotation values to the specified dataset for the specified
// years.
// ------------------------------------------------------------------
void RotationValues::writeToDataset(const string& rotationName, TAPSTable& data,
                                    unsigned firstYear, unsigned lastYear,
                                    unsigned numDataBlocks) const
   {
   // output a single data block containing all years and all averaged
   // numerical field values.  Only consider years that are covered by
   // all datablocks (remember, each data block is offset by a year).
   for (unsigned year = firstYear; year <= lastYear; year++)
      {
      YearValues::const_iterator yearValueI = yearValues.find(year);
      if (yearValueI == yearValues.end())
         {
         TAPSRecord newRecord;
         newRecord.setFieldValue("Simulation" ,rotationName);
         data.storeRecord(newRecord);
         }
      else
         {
         const FieldValues& fieldValues = yearValueI->second;
         TAPSRecord newRecord;
         for (FieldValues::const_iterator fieldValueI = fieldValues.begin();
                                          fieldValueI != fieldValues.end();
                                          fieldValueI++)
            {
            const FileValues& fileValues = fieldValueI->second;
            string name = fieldNames[fieldValueI->first];
            string value;
            if (fieldValueI->first == 0)
               value = rotationName;
            else if (stristr((char*)name.c_str(), "year") != NULL)
               value = IntToStr(year).c_str();
            else
               {
               try
                  {
                  value = getValue(fileValues, isAveragedField(name), numDataBlocks);
                  }
               catch (const runtime_error& error)
                  {
                  warnings.insert(error.what());
                  }
               }
            newRecord.setFieldValue(addPerYearToFieldName(name), value);
            }
         data.storeRecord(newRecord);
         }
      }
   if (warnings.size() > 0)
      {
      ostringstream messageStream;
      ostream_iterator<string, char> out(messageStream, "\n");
      copy(warnings.begin(), warnings.end(), out);
      Application->MessageBox(messageStream.str().c_str(), "Gross margin warnings...",
                              MB_ICONINFORMATION | MB_OK);
      }
   }


