//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "FieldValues.h"
#include <general\string_functions.h>
using namespace std;
// ------------------------------------------------------------------
//  Short description:
//    Setup all fields from the specified record.

//  Changes:
//    DPH 1/7/2001

// ------------------------------------------------------------------
void FieldValues::clearFields(void)
  {
  fieldInfo.erase(fieldInfo.begin(), fieldInfo.end());
  numericValues.erase(numericValues.begin(), numericValues.end());
  stringValues.erase(stringValues.begin(), stringValues.end());
  }

// ------------------------------------------------------------------
//  Short description:
//    Setup all fields from the specified record.

//  Changes:
//    DPH 1/7/2001

// ------------------------------------------------------------------
void FieldValues::setupFields(const TAPSRecord& record)
   {
   vector<string> fieldNames = record.getFieldNames();
   for (vector<string>::iterator fieldI = fieldNames.begin();
                                 fieldI != fieldNames.end();
                                 fieldI++)
      {
      string nameWithoutUnit = *fieldI;
      unsigned posUnit = nameWithoutUnit.find_first_of("(");
      if (posUnit != string::npos)
         nameWithoutUnit.erase(posUnit);
      Replace_all(nameWithoutUnit, " ", "_");

      string value = record.getFieldValue(*fieldI);
      if (Is_numerical(value.c_str()))
         {
         fieldInfo.push_back(FieldInfo(nameWithoutUnit.c_str(), true, fieldInfo.size()));
         numericValues.push_back(StrToFloat(value.c_str()));
         stringValues.push_back("");
         }
      else
         {
         fieldInfo.push_back(FieldInfo(nameWithoutUnit.c_str(), false, fieldInfo.size()));
         stringValues.push_back(value.c_str());
         numericValues.push_back(0.0);
         }
      }
   }
// ------------------------------------------------------------------
//  Short description:
//    give all fields to filter box.

//  Changes:
//    DPH 1/7/2001

// ------------------------------------------------------------------
void FieldValues::giveFieldsToFilterBox(TPSCFltBox& filterBox)
   {
   filterBox.Fields->Clear();
   for (vector<FieldInfo>::iterator fieldInfoI = fieldInfo.begin();
                                    fieldInfoI != fieldInfo.end();
                                    fieldInfoI++)
      {
      FieldInfo& info = *fieldInfoI;
      TPSCFltBoxField* field = dynamic_cast<TPSCFltBoxField*> (filterBox.Fields->Add());
      field->DataField = info.name;
      if (info.isNumeric)
         field->DataType = ftFloat;
      else
         field->DataType = ftString;
      }
   }
// ------------------------------------------------------------------
//  Short description:
//    send all fields to the parser.

//  Changes:
//    DPH 1/7/2001

// ------------------------------------------------------------------
void FieldValues::giveFieldsToParser(TExpressionParser& parser)
   {
   for (vector<FieldInfo>::iterator fieldInfoI = fieldInfo.begin();
                                    fieldInfoI != fieldInfo.end();
                                    fieldInfoI++)
      {
      // The parser cannot handle square or round braces - removethem.
      string parserFieldName = fieldInfoI->name.c_str();
      Replace_all(parserFieldName, "[", "");
      Replace_all(parserFieldName, "]", "");

      FieldInfo& info = *fieldInfoI;
      if (info.isNumeric)
         parser.DefineVariable(parserFieldName.c_str(), &numericValues[info.vectorIndex]);
      else
         parser.DefineStringVariable(parserFieldName.c_str(), &stringValues[info.vectorIndex]);
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    update all field values to make sure the match the specified record.

//  Changes:
//    DPH 1/7/2001

// ------------------------------------------------------------------
void FieldValues::updateFieldValues(const TAPSRecord& record)
   {
   vector<string> fieldNames = record.getFieldNames();
   for (vector<string>::iterator fieldI = fieldNames.begin();
                                 fieldI != fieldNames.end();
                                 fieldI++)
      {
      string value = record.getFieldValue(*fieldI);

      unsigned int fieldNum = fieldI-fieldNames.begin();
      if (fieldInfo[fieldNum].isNumeric)
         numericValues[fieldNum] = StrToFloat(value.c_str());
      else
         stringValues[fieldNum] = value.c_str();
      }
   }

