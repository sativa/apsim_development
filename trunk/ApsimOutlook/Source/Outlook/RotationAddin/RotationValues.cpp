//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "RotationValues.h"
#include <general\stristr.h>
#include <general\path.h>
#include <general\ini_file.h>
#include <assert.h>

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
   for (FileValues::const_iterator fileI = fileValues.begin();
                                   fileI != fileValues.end();
                                   fileI++)
      {
      float value;
      if (fileI->second.getValue(doAverage, value))
         total += value;
      else
         {
         string stringValue;
         fileI->second.getValue(stringValue);
         return stringValue;
         }
      }
   float returnValue = total;
   if (numDataBlocks > 0)
      returnValue /= numDataBlocks;
   else
      returnValue = 0.0;

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
   for (vector<string>::const_iterator fieldI = fieldsToAverage.begin();
                                       fieldI != fieldsToAverage.end();
                                       fieldI++)
      {
      string field = *fieldI;
      bool doAverage = false;
      if (field[0] == '*')
         doAverage = (stristr((char*) fieldName.c_str(),
                              field.substr(1, field.length()-1).c_str()) != NULL);
      else
         doAverage = Str_i_Eq(fieldName, field);
      if (doAverage)
         return true;
      }
   return false;
   }

// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
RotationValues::RotationValues(const vector<string>& fields)
   : fieldNames(fields)
   {
   Path iniPath(Application->ExeName.c_str());
   iniPath.Append_path("rotationaddin");
   iniPath.Set_name("rotationaddin.ini");
   Ini_file ini;
   ini.Set_file_name(iniPath.Get_path().c_str());

   string st;
   ini.Read("fields", "averaged_fields", st);
   Split_string(st, ",", fieldsToAverage);
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
   for (int year = firstYear; year <= lastYear; year++)
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
               value = getValue(fileValues, isAveragedField(name), numDataBlocks);
            newRecord.setFieldValue(addPerYearToFieldName(name), value);
            }
         data.storeRecord(newRecord);
         }
      }
   }


