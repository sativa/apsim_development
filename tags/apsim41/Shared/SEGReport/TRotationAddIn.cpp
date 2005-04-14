//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TRotationAddIn.h"
#include <general\string_functions.h>
#include <general\db_functions.h>
#include <general\stristr.h>
#include <values.h>
#include <stdio.h>

using namespace std;
#pragma package(smart_init)
#pragma resource "ComponentRegistration.res"
//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
__fastcall TRotationAddIn::TRotationAddIn(TComponent* owner)
   : TSEGTable(owner)
   {
   recognisedCropNames = new TStringList;
   recognisedCropNames->Text = "ckp\nsor\nlup\ncan\nsun\nwht\nsyb\nsorg\npnt\noz"
                               "\nnw\nnyb\nmgb\nmz\nfab\ncwp\nwd\nwd1\nwd2\nwd3\nluc";
   fieldsToAverage = new TStringList;
   fieldsToAverage->Text = "*year";
   }

//---------------------------------------------------------------------------
// destructor
//---------------------------------------------------------------------------
__fastcall TRotationAddIn::~TRotationAddIn()
   {
   delete recognisedCropNames;
   delete fieldsToAverage;
   }
//---------------------------------------------------------------------------
// set the 'cropNames' property and refresh all data.
//---------------------------------------------------------------------------
void __fastcall TRotationAddIn::setCropNames(TStrings* cropNames)
   {
   recognisedCropNames->Assign(cropNames);
   forceRefresh();
   }
//---------------------------------------------------------------------------
// set the 'averagedFields' property and refresh all data.
//---------------------------------------------------------------------------
void __fastcall TRotationAddIn::setAveragedFields(TStrings* averagedFields)
   {
   fieldsToAverage->Assign(averagedFields);
   forceRefresh();
   }

// ------------------------------------------------------------------
// This class is a helper class to keep track of all the field
// values for a given year for a given simulation and then to
// calculate an average when asked to do so.
// ------------------------------------------------------------------
class YearValues
   {
   private:
      class FieldValue
         {
         private:
            string stringValue;
            double numericalValue;
            unsigned int count;
            enum FieldValueType {str, numerical, unknown};
            FieldValueType type;
         public:
            FieldValue(void) : type(unknown), count(0) { }

            void clear(void)
               {
               count = 0;
               numericalValue = 0;
               }
            bool isString(void) {return type == str;}
            void addValue(const string& value)
               {
               if (type == unknown)
                  {
                  char* endptr;
                  numericalValue = strtod(value.c_str(), &endptr);
                  if (*endptr == 0)
                     type = numerical;
                  else
                     {
                     type = str;
                     stringValue = value;
                     }
                  }
               else if (type == numerical)
                  {
                  char* endptr;
                  numericalValue += strtod(value.c_str(), &endptr);
                  }
               count++;
               }
            double getValue(bool doTotal)
               {
               if (type == numerical)
                  {
                  if (count == 0)
                     return 0.0;
                  else
                     {
                     if (doTotal)
                        return numericalValue;
                     else
                        return numericalValue / count;
                     }
                  }
               else
                  return MAXDOUBLE;
               }
            string getValue(void)
               {
               if (type == str)
                  return stringValue;
               else
                  return "";
               }
         };

      typedef map<unsigned, FieldValue> FieldValues;

      vector<FieldValues> values;
   public:
      YearValues(void) { }

      void addValue(unsigned int simIndex, unsigned int fieldIndex, const string& value)
         {
         while (values.size() <= simIndex)
            values.push_back(FieldValues());
         values[simIndex][fieldIndex].addValue(value);
         }
      string getValue(bool doTotal, unsigned int fieldIndex)
         {
         // loop through all simulations.
         int count = 0;
         double total = 0.0;
         for (unsigned int simIndex = 0; simIndex < values.size(); simIndex++)
            {
            if (values[simIndex][fieldIndex].isString())
               return values[simIndex][fieldIndex].getValue();
            else
               {
               double value = values[simIndex][fieldIndex].getValue(doTotal);
               if (value != MAXDOUBLE)
                  {
                  total += value;
                  count++;
                  }
               }
            }
         char buffer[20];
         if (count == 0)
            sprintf(buffer, "%10.3f", 0.0);
         else
            sprintf(buffer, "%10.3f", total / count);
         return &buffer[0];
         }
   };

//---------------------------------------------------------------------------
// Called by our base class to allow us to add any fielddefs we may want to.
// The table will be closed (Active=false) when this routine is called.
//---------------------------------------------------------------------------
bool TRotationAddIn::createFields(void) throw(runtime_error)
   {
   if (source != NULL)
      {
      FieldDefs->Assign(source->FieldDefs);
      return true;
      }
   return false;
   }
//---------------------------------------------------------------------------
// Called by our base class to allow us to add records to the table.
// The table is open (Active = true) when this routine is called.
//---------------------------------------------------------------------------
void TRotationAddIn::storeRecords(void) throw(runtime_error)
   {
   if (source != NULL)
      {
      // get the name of the year field.
      AnsiString yearFieldName = source->getYearFieldName().c_str();

      // setup some storage for our values indexed by year.
      typedef map<int, YearValues> Values;
      Values values;

      int firstYear = 0;
      int lastYear = 10000;
      unsigned int numSeries = 0;

      // Loop through all series blocks and all records within that series.
      source->firstSeries();
      while (!source->Eof)
         {
         int firstDataBlockYear = source->FieldValues[yearFieldName];
         int lastDataBlockYear = 0;
         while (!source->Eof)
            {
            int year = source->FieldValues[yearFieldName];
            lastDataBlockYear = max(lastDataBlockYear, year);

            for (int fieldI = 0; fieldI < source->FieldCount; fieldI++)
               {
               AnsiString name = source->Fields->Fields[fieldI]->FieldName;
               AnsiString value = source->Fields->Fields[fieldI]->AsString;
               bool addToRecords = false;

               // For crop variables where the crop was sown in the
               // current year, add the value to our Values container.
               // For non-crop variables always add the value to our Values
               // container.
               if (isCropVariable(name))
                  addToRecords = cropWasSown(name);
               else
                  addToRecords = true;

               if (addToRecords)
                  {
                  if (values.find(year) == values.end())
                     values.insert(Values::value_type(year, YearValues()));
                  values[year].addValue(numSeries, fieldI, value.c_str());
                  }
               }
            }

         firstYear = max(firstYear, firstDataBlockYear);
         lastYear = min(lastYear, lastDataBlockYear);

         numSeries++;
         source->nextSeries();
         }

      // output a single data block containing all years and all averaged
      // numerical field values.  Only consider years that are covered by
      // all datablocks (remember, each data block is offset by a year).
      for (Values::iterator valueI = values.begin();
                            valueI != values.end();
                            valueI++)
         {
         if (valueI->first >= firstYear && valueI->first <= lastYear)
            {
            Append();
            for (int fieldI = 0; fieldI < source->FieldCount; fieldI++)
               {
               AnsiString name = source->Fields->Fields[fieldI]->FieldName;
               if (fieldI != 0)
                  Fields->Fields[fieldI]->AsString =
                     valueI->second.getValue(doTotalVariable(name), fieldI).c_str();
               }
            Post();
            }
         }
      }
   }
// ------------------------------------------------------------------
// Return true if the specified field is a recognised crop field name
// ------------------------------------------------------------------
bool TRotationAddIn::isCropVariable(AnsiString fieldName) const
   {
   unsigned posUnderscore = fieldName.Pos("_");
   if (posUnderscore > 0)
      {
      AnsiString potentialCropName = fieldName.SubString(1, posUnderscore);
      return (recognisedCropNames->IndexOf(potentialCropName) >= 0);
      }
   else
      return false;
   }

// ------------------------------------------------------------------
// Return true if the specified crop was actually sown
// for the current record.  A crop is sown if:
//    there is a least 1 non zero value in the fields for this crop OR
//    the crop_fail field (if it exists) has a 'yes' in it.
// ------------------------------------------------------------------
bool TRotationAddIn::cropWasSown(AnsiString fieldName)
   {
   unsigned posUnderscore = fieldName.Pos("_");
   if (posUnderscore > 0)
      {
      AnsiString cropAcronym = fieldName.SubString(1, posUnderscore);
      AnsiString failedFieldName = cropAcronym + "_fail";
      if (!source->FieldValues[failedFieldName].IsNull())
         return (AnsiCompareStr(FieldValues[failedFieldName], "yes") == 0);
      else
         return (cropHasNonZeroValue(cropAcronym));
      }
   else
      return false;
   }

// ------------------------------------------------------------------
// Return true if there is any field for the specified crop, in the
// specified record that has a non zero value.
// ------------------------------------------------------------------
bool TRotationAddIn::cropHasNonZeroValue(AnsiString cropAcronym) const
   {
   for (int fieldI = 0; fieldI < source->FieldCount; fieldI++)
      {
      cropAcronym = cropAcronym + "_";
      AnsiString name = source->Fields->Fields[fieldI]->FieldName;

      if (name.Pos(cropAcronym) != 0)
         {
         AnsiString value = source->Fields->Fields[fieldI]->AsString;
         if (Is_numerical(value.c_str()))
            return (StrToFloat(value.c_str()) != 0.0);
         }
      }
   return false;
   }
// ------------------------------------------------------------------
// Return true if the specified field should be totalled within a year.
// Return false if the specified field should be averaged within a year.
// ------------------------------------------------------------------
bool TRotationAddIn::doTotalVariable(AnsiString fieldName) const
   {
   for (int fieldI = 0; fieldI < fieldsToAverage->Count; fieldI++)
      {
      AnsiString averagedField = fieldsToAverage->Strings[fieldI];
      bool doAverage = false;
      if (averagedField[0] == '*')
         {
         averagedField = averagedField.Delete(1, 1);
         doAverage = (stristr(fieldName.c_str(), averagedField.c_str()) != NULL);
         }
      else
         doAverage = (fieldName.AnsiCompareIC(fieldI) == 0);
      if (doAverage)
         return false;
      }
   return true;
   }

