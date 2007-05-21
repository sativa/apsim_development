//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TAPSRecord.h"
#include <general\string_functions.h>
#include <general\stl_functions.h>
#pragma package(smart_init)
using namespace std;
// ------------------------------------------------------------------
//  Short description:
//      constructor

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
TAPSRecord::TAPSRecord (void)
   {
   }

// ------------------------------------------------------------------
//  Short description:
//      copy constructor

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
TAPSRecord::TAPSRecord (const TAPSRecord& from)
   {
   assign(from);
   }

// ------------------------------------------------------------------
//  Short description:
//      less than operator for TAPSRecord

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
TAPSRecord& TAPSRecord::operator=  (const TAPSRecord& from)
   {
   assign(from);
   return *this;
   }

// ------------------------------------------------------------------
//  Short description:
//      less than operator for TAPSRecord

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void TAPSRecord::assign(const TAPSRecord& from)
   {
   fieldNames.assign(from.fieldNames.begin(), from.fieldNames.end());
   fieldValues.assign(from.fieldValues.begin(), from.fieldValues.end());
   }

// ------------------------------------------------------------------
//  Short description:
//      Read from specified dataset.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void TAPSRecord::read(TDataSet* dataset, vector<string>& fieldnames)
   {
   // loop through all fields, get values from current record and store in
   // return matrix.
   for (unsigned int field = 0; field < fieldnames.size(); field++)
      {
      string value;
      if (!dataset->FieldValues[fieldnames[field].c_str()].IsNull())
         value = String(dataset->FieldValues[fieldnames[field].c_str()]).c_str();
      else
         value = "0";
      setFieldValue (fieldnames[field], value);
      }
   }

// ------------------------------------------------------------------
//  Short description:
//      set the value of a field.  Adds the field if necessary.

//  Notes:

//  Changes:
//    DPH 5/2/98
//    dph 14/12/99 removed lowercase calls C265

// ------------------------------------------------------------------
void TAPSRecord::setFieldValue(const string& fieldName, const string& fieldValue)
   {
   vector<string>::iterator i = find_if(fieldNames.begin(), fieldNames.end(),
                                        CaseInsensitiveStringComparison(fieldName));
   if (i == fieldNames.end())
      {
      fieldNames.push_back(fieldName);
      fieldValues.push_back(fieldValue);
      }
   else
      fieldValues[i - fieldNames.begin()] = fieldValue;
   }

// ------------------------------------------------------------------
//  Short description:
//      return a specific field value.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
string TAPSRecord::getFieldValue (const string& fieldName) const
   {
   vector<string>::const_iterator i = find_if(fieldNames.begin(), fieldNames.end(),
                                              CaseInsensitiveStringComparison(fieldName));
   if (i != fieldNames.end())
      return fieldValues[i - fieldNames.begin()];
   else
      return "";
   }
// ------------------------------------------------------------------
//  Short description:
//      Write to specified dataset.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void TAPSRecord::write (TDataSet* dataset) const
   {
   dataset->Append();

   for (unsigned int field = 0; field < fieldNames.size(); field++)
      {
      if (fieldValues[field] != "")
         dataset->FieldValues[fieldNames[field].c_str()] = fieldValues[field].c_str();
      }

   dataset->Post();
   }

// ------------------------------------------------------------------
//  Short description:
//      clear this record.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void TAPSRecord::clear(void)
   {
   fieldNames.erase(fieldNames.begin(), fieldNames.end());
   fieldValues.erase(fieldValues.begin(), fieldValues.end());
   }

