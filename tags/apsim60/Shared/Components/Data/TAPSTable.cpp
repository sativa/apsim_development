//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TAPSTable.h"
#include <general\vcl_functions.h>
#include <general\stl_functions.h>
#include <general\string_functions.h>
#include <ApsimShared\ApsimSettings.h>
#include <sstream>
#include <iomanip>
#include <assert.h>
#include "TAPSTable_form.h"
using namespace std;

#pragma package(smart_init)
#pragma link "TMultiStringList"
//---------------------------------------------------------------------------
// ValidCtrCheck is used to assure that the components created do not have
// any pure virtual functions.
//

static inline void ValidCtrCheck(TAPSTable *)
{
   new TAPSTable(NULL);
}
//---------------------------------------------------------------------------
namespace Tapstable
{
   void __fastcall PACKAGE Register()
   {
      TComponentClass classes[1] = {__classid(TAPSTable)};
      RegisterComponents("APSRU", classes, 0);
//      RegisterComponentEditor(__classid(TAPSTable), __classid(TAPSTable_editor));
   }
}
// ------------------------------------------------------------------
//  Short description:
//      constructor

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
__fastcall TAPSTable::TAPSTable(TComponent* Owner)
   : TComponent(Owner)
   {
   FSourceDataset = NULL;
   }
// ------------------------------------------------------------------
//  Short description:
//      destructor

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
__fastcall TAPSTable::~TAPSTable()
   {
   }

// ------------------------------------------------------------------
// Load settings
// ------------------------------------------------------------------
void TAPSTable::load()
   {
   if (FSourceDataset != NULL)
      {
      FSourceDataset->invisibleDataBlockNames.clear();
      ApsimSettings settings;
      settings.read(CHART_SETTINGS_KEY + "|InvisibleDataBlock", FSourceDataset->invisibleDataBlockNames);
      }
   }
// ------------------------------------------------------------------
// Save settings
// ------------------------------------------------------------------
void TAPSTable::save()
   {
   if (FSourceDataset != NULL)
      {
      ApsimSettings settings;
      settings.write(CHART_SETTINGS_KEY + "|InvisibleDataBlock", FSourceDataset->invisibleDataBlockNames);
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     refresh the control only if it is active.

//  Notes:

//  Changes:
//    DPH 5/2/98
//    DAH 10/8/2000  saved and restored cursor

// ------------------------------------------------------------------
void TAPSTable::refresh (void)
   {
   if (FSourceDataset != NULL)
      {
      TCursor savedCursor = Screen->Cursor;
      Screen->Cursor = crHourGlass;
      calcAndStoreRecords();
      Screen->Cursor = savedCursor;
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     return a list of field names to caller.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void TAPSTable::getFieldNames(vector<string>& fieldnames)
   {
   getFieldNamesMinusPivots(fieldnames);
   copy(pivotFieldNames.begin(), pivotFieldNames.end(), back_inserter(fieldnames));
   }


      void getPivotNames(std::vector<std::string>& fieldNames);
// ------------------------------------------------------------------
//  Short description:
//     return a list of pivot names to caller.

//  Notes:

//  Changes: DAH 30/5/01   created

// ------------------------------------------------------------------
void TAPSTable::getPivotNames(vector<string>& pivotNames)
   {
   copy(pivotFieldNames.begin(), pivotFieldNames.end(), back_inserter(pivotNames));
   }

// ------------------------------------------------------------------
//  Short description:
//     return a list of field names to caller.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void TAPSTable::getFieldNamesMinusPivots(vector<string>& fieldnames)
   {
   fieldnames.assign(fieldNames.begin(), fieldNames.end());
   }

// ------------------------------------------------------------------
//  Short description:
//      return the name of the year field name.

//  Notes:

//  Changes:
//    DPH 5/2/98
//    DAH 31/10/00: D380 - moved this from TTime_Series_Analysis to give
//                         more objects access to this common method

// ------------------------------------------------------------------
string TAPSTable::getYearFieldName(void) const
   {
   vector<string>::const_iterator i;

   // look for 'sow_year'
   i = find_if(fieldNames.begin(), fieldNames.end(),
               CaseInsensitiveStringComparison("sow_year"));
   if (i != fieldNames.end())
      return *i;

   // look for a field that has year in it.
   i = find_if(fieldNames.begin(), fieldNames.end(),
               PartialStringComparison("year"));
   if (i != fieldNames.end())
      return *i;
   ShowMessage("Cannot find a year column in dataset.");
   return "";
   }

// ------------------------------------------------------------------
//  Short description:
//    clear all field names.

//  Notes:

//  Changes:
//    DPH 10/4/01

// ------------------------------------------------------------------
void TAPSTable::clearFields(void)
   {
   fieldNames.erase(fieldNames.begin(), fieldNames.end());
   pivotFieldNames.erase(pivotFieldNames.begin(), pivotFieldNames.end());
   }

// ------------------------------------------------------------------
//  Short description:
//    add a field.

//  Notes:

//  Changes:
//    DPH 10/4/01

// ------------------------------------------------------------------
void TAPSTable::addField(const std::string& fieldName)
   {
   if (find_if(fieldNames.begin(), fieldNames.end(),
               CaseInsensitiveStringComparison(fieldName))
       == fieldNames.end())
      fieldNames.push_back(fieldName);
   }

// ------------------------------------------------------------------
//  Short description:
//      position the data pointer to the first dataset.  Return
//      true if data is ready to be read.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
bool TAPSTable::first(void)
   {
   endBlockIterator = allRecords.begin();
   return next();
   }

// ------------------------------------------------------------------
//  Short description:
//      Locates the next dataset that is different from the one that
//      the current all_records_iterator is pointing to.  Return TRUE
//      if a dataset was found.  Also makes sure the found dataset is
//      valid.

//  Notes:

//  Changes:
//    DPH 1/10/98

// ------------------------------------------------------------------
bool TAPSTable::next(void)
   {
   startBlockIterator = endBlockIterator;
   findStartBlockIterator();
   findEndBlockIterator();
   return (startBlockIterator != endBlockIterator);
   }
// ------------------------------------------------------------------
//  Short description:
//    Compare function for comparing records according to the field
//    names pass in.  Returns true if lfs < rhs

//  Notes:

//  Changes:
//    DPH 5/4/01
//    DAH 4/6/01  Changed ordering for pairs that are numerical - they are now
//                ordered numerically, not by a string comparison. A non-numerical
//                pair is ordered as before (including a (numerical,string) pair).
//    DPH 7/8/01  Changed ordering for pairs that are percentages.

// ------------------------------------------------------------------
class RecordsLessThan : public binary_function<TAPSRecord, TAPSRecord, bool>
   {
   private:
      vector<string> compareFields;
   public:
      RecordsLessThan(const vector<string>& comparefields)
         {
         copy(comparefields.begin(), comparefields.end(), back_inserter(compareFields));
         }
      bool operator() (const TAPSRecord& lhs, const TAPSRecord& rhs)
         {
         for (vector<string>::iterator i = compareFields.begin();
                                       i != compareFields.end();
                                       i++)
            {
            string lhsValue = lhs.getFieldValue( (*i).c_str() );
            string rhsValue = rhs.getFieldValue( (*i).c_str() );

            if (lhsValue[lhsValue.length()-1] == '%')
               lhsValue.erase(lhsValue.length()-1);
            if (rhsValue[rhsValue.length()-1] == '%')
               rhsValue.erase(rhsValue.length()-1);

            //int compareResult = strcmpi(lhsValue.c_str(), rhsValue.c_str());
            float compareResult;
            if (Is_numerical(lhsValue.c_str()) && Is_numerical(rhsValue.c_str()))
            {
               float lhs = atof(lhsValue.c_str());
               float rhs = atof(rhsValue.c_str());
               compareResult = lhs - rhs;
            }
            else
               compareResult = strcmpi(lhsValue.c_str(), rhsValue.c_str());

            if (compareResult < 0) return true;
            if (compareResult > 0) return false;
            }
         return false;
         }
   };
// ------------------------------------------------------------------
//  Short description:
//    Compare function for comparing records according to the field
//    names pass in.  Returns true if lfs != rhs

//  Notes:

//  Changes:
//    DPH 5/4/01

// ------------------------------------------------------------------
class RecordsNotEqual
   {
   private:
      vector<string> compareFields;
      const TAPSRecord& rhs;
   public:
      RecordsNotEqual(const vector<string>& comparefields,
                      const TAPSRecord& from)
         : rhs(from)
         {
         copy(comparefields.begin(), comparefields.end(), back_inserter(compareFields));
         }
      bool operator() (const TAPSRecord& lhs)
         {
         for (vector<string>::iterator i = compareFields.begin();
                                       i != compareFields.end();
                                       i++)
            {
            string lhsValue = lhs.getFieldValue( (*i).c_str() );
            string rhsValue = rhs.getFieldValue( (*i).c_str() );
            int compareResult = strcmpi(lhsValue.c_str(), rhsValue.c_str());
            if (compareResult != 0) return true;
            }
         return false;
         }
   };

// ------------------------------------------------------------------
//  Short description:
//     locates the start of the next block of data according to
//     the filter data blocks

//  Notes:

//  Changes:
//    DPH 1/10/98

// ------------------------------------------------------------------
void TAPSTable::findStartBlockIterator(void)
   {
   while (startBlockIterator != allRecords.end() &&
          find(invisibleDataBlockNames.begin(), invisibleDataBlockNames.end(),
               getDataBlockName()) != invisibleDataBlockNames.end())
      {
      findEndBlockIterator();
      startBlockIterator = endBlockIterator;
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     locates the end of the current block of data according to
//     the pivot fields.

//  Notes:

//  Changes:
//    DPH 1/10/98

// ------------------------------------------------------------------
void TAPSTable::findEndBlockIterator(void)
   {
   endBlockIterator = find_if(startBlockIterator, allRecords.end(),
                              RecordsNotEqual(pivotFieldNames, *startBlockIterator));
   }

// ------------------------------------------------------------------
//  Short description:
//      this method converts the current data block into an array of strings
//      for the specified field.

//  Notes:
//

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void TAPSTable::fieldAsStringArray(const string& fieldName,
                                   vector<string>& arrayStrings)
   {
   arrayStrings.erase(arrayStrings.begin(), arrayStrings.end());
   for (vector<TAPSRecord>::const_iterator i = begin();
                                           i != end();
                                           i++)
      arrayStrings.push_back( (*i).getFieldValue(fieldName));
   }

// ------------------------------------------------------------------
//  Short description:
//      this method converts the current data block into an array of doubles
//      for the specified field.

//  Notes:
//

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void TAPSTable::fieldAsNumericArray(const string& fieldName,
                                    vector<double>& arrayNumbers)
   {
   arrayNumbers.erase(arrayNumbers.begin(), arrayNumbers.end());
   for (vector<TAPSRecord>::const_iterator i = begin();
                                           i != end();
                                           i++)
      {
      string stringValue = (*i).getFieldValue(fieldName);
      if (stringValue != "")
         arrayNumbers.push_back( atof(stringValue.c_str()));
      }
   }

// ------------------------------------------------------------------
//  Short description:
//      this method stores the specified array of numbers.

//  Notes:
//

//  Changes:
//    DPH 5/2/98
//    DAH 30/5/01:   checked for fieldName being a pivot field. If it is,
//                   we do not add it as a field.

// ------------------------------------------------------------------
void TAPSTable::storeNumericArray(const string& fieldName,
                                  const vector<double>& arrayNumbers)
   {
   if (find(pivotFieldNames.begin(), pivotFieldNames.end(), fieldName)
       == pivotFieldNames.end())
      addField(fieldName);

   if (startBlockIterator == allRecords.end())
      addRecords(arrayNumbers.size());

   vector<TAPSRecord>::iterator currentPos = startBlockIterator;
   for (unsigned int i = 0; i < arrayNumbers.size(); i++)
      {
      ostringstream out;
      out.setf(ios::fixed, ios::floatfield);
      out << setprecision(3) << arrayNumbers[i] << ends;
      (*currentPos).setFieldValue(fieldName, out.str());
      currentPos++;
      }
   }
// ------------------------------------------------------------------
//  Short description:
//      this method stores the specified array of numbers.

//  Notes:
//

//  Changes:
//    DPH 5/2/98
//    DAH 30/5/01:   checked for fieldName being a pivot field. If it is,
//                   we do not add it as a field.

// ------------------------------------------------------------------
void TAPSTable::storeStringArray(const string& fieldName,
                                 const vector<string>& arrayStrings)
   {
   if (find(pivotFieldNames.begin(), pivotFieldNames.end(), fieldName)
       == pivotFieldNames.end())
      addField(fieldName);

   if (startBlockIterator == allRecords.end())
      addRecords(arrayStrings.size());

   vector<TAPSRecord>::iterator currentPos = startBlockIterator;
   for (unsigned int i = 0; i < arrayStrings.size(); i++)
      {
      (*currentPos).setFieldValue(fieldName, arrayStrings[i]);
      currentPos++;
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    add the specified number of blank records to the allRecords container


//  Notes:
//

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void TAPSTable::addRecords(unsigned int numRecords)
   {
   for (unsigned int i = 0; i < numRecords; i++)
      allRecords.push_back(TAPSRecord());
   startBlockIterator = allRecords.end() - numRecords;
   endBlockIterator = allRecords.end();
   }

// ------------------------------------------------------------------
//  Short description:
//      begin storing data.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void TAPSTable::beginStoringData(void)
   {
   allRecords.erase(allRecords.begin(), allRecords.end());
   }

// ------------------------------------------------------------------
//  Short description:
//      store the values and probability values to the memory table.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void TAPSTable::storeData(const vector<TAPSRecord>& values)
   {
   std::copy (values.begin(), values.end(), std::back_inserter (allRecords));
   startBlockIterator = allRecords.end() - values.size();
   endBlockIterator = allRecords.end();
   }
// ------------------------------------------------------------------
//  Short description:
//      store the specified record.

//  Notes:

//  Changes:
//    DPH 10/4/01

// ------------------------------------------------------------------
void TAPSTable::storeRecord(const TAPSRecord& record)
   {
   unsigned int startBlockPos = startBlockIterator - allRecords.begin();
   allRecords.push_back(record);
   startBlockIterator = allRecords.begin() + startBlockPos;;
   endBlockIterator = allRecords.end();
   }

// ------------------------------------------------------------------
//  Short description:
//      reset the data reading system.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void TAPSTable::endStoringData(void)
   {
   checkInvisibleDataBlocks();
   }

// ------------------------------------------------------------------
//  Short description:
//      fills the specified FieldDefs structure with all fields.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void TAPSTable::getFielddefs(TFieldDefs* fieldDefs)
   {
   vector<string> allFieldNames;
   getFieldNames(allFieldNames);

   // loop through all field names.
   for (unsigned int field = 0; field < allFieldNames.size(); field++)
      {
      bool found = false;
      for (int i = 0; i < fieldDefs->Count && !found; i++)
         found = (fieldDefs->Items[i]->Name == allFieldNames[field].c_str());

      if (!found)
         {
         int numChars = max(allRecords[0].getFieldValue(allFieldNames[field]).length() + 5,
                            allFieldNames[field].length() + 1);
         fieldDefs->Add(allFieldNames[field].c_str(), ftString, numChars, false);
         }
      }
   }

// ------------------------------------------------------------------
//  Short description:
//      clear all pivots

//  Notes:

//  Changes:
//    DPH 10/4/01

// ------------------------------------------------------------------
void TAPSTable::clearPivots(void)
   {
   pivotFieldNames.erase(pivotFieldNames.begin(), pivotFieldNames.end());
   }

// ------------------------------------------------------------------
//  Short description:
//      add a pivot to the pivot list.

//  Notes:

//  Changes:
//    DPH 10/4/01

// ------------------------------------------------------------------
void TAPSTable::markFieldAsAPivot(const string& pivotFieldName)
   {
   if (find(pivotFieldNames.begin(), pivotFieldNames.end(), pivotFieldName)
       == pivotFieldNames.end())
      {
      pivotFieldNames.push_back(pivotFieldName);
      }
   }
// ------------------------------------------------------------------
//  Short description:
//    Copy all pivot fields from the specified data table.

//  Notes:

//  Changes:
//    DPH 10/4/01

// ------------------------------------------------------------------
void TAPSTable::copyPivotsFrom(const TAPSTable& from)
   {
   clearPivots();

   // copy the pivot field names.
   if (fieldNames.size() == 0 || fieldNames[0] != "Simulation")
      fieldNames.insert(fieldNames.begin(), "Simulation");
   copy(from.pivotFieldNames.begin(), from.pivotFieldNames.end(),
        back_inserter(pivotFieldNames));

   // copy the pivot field values.
   for (vector<TAPSRecord>::iterator r = startBlockIterator; r != endBlockIterator; r++)
      {
      (*r).setFieldValue("Simulation",
                         (*from.begin()).getFieldValue("Simulation"));
      for (unsigned int p = 0; p < pivotFieldNames.size(); p++)
         (*r).setFieldValue(pivotFieldNames[p],
                           (*from.begin()).getFieldValue(pivotFieldNames[p]));
      }
   }


// ------------------------------------------------------------------
//  Short description:
//    Copy all pivot fields from the specified data table.
//    Fill them in according to their matching Simulation ID

//  Notes:
//    This method created because copyPivotsFrom has some weird fill side effects

//  Changes:

// ------------------------------------------------------------------
void TAPSTable::copyAndFillPivots(const TAPSTable& from)
   {
   // assume that the only trustable pivot field is the Simulation field
   clearPivots();
   markFieldAsAPivot("Simulation");

   // copy the pivot field names.
   if (fieldNames.size() == 0 || fieldNames[0] != "Simulation")
      fieldNames.insert(fieldNames.begin(), "Simulation");
   copy(from.pivotFieldNames.begin(), from.pivotFieldNames.end(),
        back_inserter(pivotFieldNames));

   // copy the pivot field values and fill in per block.
   bool ok = first() && from.first();
   while (ok)
      {
         for (vector<TAPSRecord>::iterator r = startBlockIterator; r != endBlockIterator; r++)
         {
         (*r).setFieldValue("Simulation",
                            (*from.begin()).getFieldValue("Simulation"));
         for (unsigned int p = 0; p < pivotFieldNames.size(); p++)
            (*r).setFieldValue(pivotFieldNames[p],
                              (*from.begin()).getFieldValue(pivotFieldNames[p]));
         }
      ok = next() && from.next();
      }
   }



// ------------------------------------------------------------------
//  Short description:
//    return a list of unique data block (simulation) names.

//  Notes:

//  Changes:
//    DPH 10/4/01

// ------------------------------------------------------------------
void TAPSTable::getDataBlockNames(std::vector<std::string>& dataBlockNames)
   {
   bool ok = first();
   while (ok)
      {dataBlockNames.push_back(getDataBlockName());
      ok = next();
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    return a list of unique data block (simulation) names.

//  Notes:  created DAH 9/8/01

//  Changes:
// ------------------------------------------------------------------
void TAPSTable::getAllDataBlockNames(std::vector<std::string>& dataBlockNames)
   {
   for (vector<TAPSRecord>::iterator i = allRecords.begin(); i != allRecords.end();
            i++)
      {
      string blockName = i->getFieldValue("Simulation");
      if (find(dataBlockNames.begin(),dataBlockNames.end(),blockName) == dataBlockNames.end())
         dataBlockNames.push_back(blockName);
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    return the current data block name.

//  Notes:

//  Changes:
//    DPH 10/4/01

// ------------------------------------------------------------------
string TAPSTable::getDataBlockName(void) const
   {
   return (*startBlockIterator).getFieldValue("Simulation");
   }

// ------------------------------------------------------------------
//  Short description:
//    set the current data block name

//  Notes:

//  Changes:
//    DPH 10/4/01

// ------------------------------------------------------------------
void TAPSTable::setDataBlockName(const string& dataBlockName)
   {
   for (vector<TAPSRecord>::iterator i = startBlockIterator;
                                     i != endBlockIterator;
                                     i++)
      {
      (*i).setFieldValue("Simulation", dataBlockName);
      }
   }
// ------------------------------------------------------------------
//  Short description:
//    return true if the specified data block name is visible to user.

//  Notes:

//  Changes:
//    DPH 10/4/01

// ------------------------------------------------------------------
bool TAPSTable::isDataBlockVisible(const string& dataBlockName)
   {
   checkInvisibleDataBlocks();
   return (find(invisibleDataBlockNames.begin(),
                invisibleDataBlockNames.end(),
                dataBlockName) == invisibleDataBlockNames.end());
   }

// ------------------------------------------------------------------
//  Short description:
//    mark a data block as visible.

//  Notes:

//  Changes:
//    DPH 2/8/01

// ------------------------------------------------------------------
void TAPSTable::markDataBlockVisible(const std::string& dataBlockName,
                                     bool isVisible)
   {
   vector<string>::iterator invisibleI = find(invisibleDataBlockNames.begin(),
                                              invisibleDataBlockNames.end(),
                                              dataBlockName);
   if (isVisible)
      {
      if (invisibleI != invisibleDataBlockNames.end())
         invisibleDataBlockNames.erase(invisibleI);
      }

   else if (invisibleI == invisibleDataBlockNames.end())
      invisibleDataBlockNames.push_back(dataBlockName);
   }

// ------------------------------------------------------------------
//  Short description:
//    make sure the invisible datablock names actually are data block names.

//  Changes:
//    DPH 10/8/01
//    DAH 9/8/01: changed call to getDataBlockNames to getAllDataBlockNames

// ------------------------------------------------------------------
void TAPSTable::checkInvisibleDataBlocks(void)
   {
   vector<string> dataBlockNames;
   getAllDataBlockNames(dataBlockNames);
   vector<string>::iterator invisibleI = invisibleDataBlockNames.begin();
   while (invisibleI != invisibleDataBlockNames.end())
      {
      if (find(dataBlockNames.begin(), dataBlockNames.end(), *invisibleI)
          == dataBlockNames.end())
         invisibleI = invisibleDataBlockNames.erase(invisibleI);
      else
         invisibleI++;
      }
   }
// ------------------------------------------------------------------
//  Short description:
//    sort the records according to their pivot fields.

//  Notes:

//  Changes:
//    DPH 5/4/01

// ------------------------------------------------------------------
void TAPSTable::sortRecords(const string& fieldName)
   {
   vector<string> sortFields;
   sortFields.push_back(fieldName);
   stable_sort(allRecords.begin(), allRecords.end(),
        RecordsLessThan(sortFields));
   }

// ------------------------------------------------------------------
//  Short description:
//    copy everything from the specified table to 'this' table.

//  Notes:

//  Changes:
//    DPH 5/4/01

// ------------------------------------------------------------------
void TAPSTable::storeData(const TAPSTable& from)
   {
   allRecords = from.allRecords;
   fieldNames = from.fieldNames;
   pivotFieldNames = from.pivotFieldNames;
   invisibleDataBlockNames = from.invisibleDataBlockNames;
   }

// ------------------------------------------------------------------
//  Short description:
//      clear all records.

//  Changes:
//    DPH 30/8/2001
// ------------------------------------------------------------------
void TAPSTable::clearRecords(void)
  {
  allRecords.erase(allRecords.begin(), allRecords.end());
  first();
  }

// ------------------------------------------------------------------
//  Short description:
//      create and return a pointer to an analysis form.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
TAPSTable_form* TAPSTable::createPropertiesForm()
   {
   return new TAPSTable_form(Application);
   }

// ------------------------------------------------------------------
//  Short description:
//      setup properties form.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void TAPSTable::setupPropertiesForm(TAPSTable_form* form)
   {
   }

// ------------------------------------------------------------------
//  Short description:
//      allow user to edit the component.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
bool TAPSTable::edit(void)
   {
   load();
   
   TAPSTable_form* form = createPropertiesForm();
   if (form != NULL)
      {
      setupPropertiesForm(form);
      if (form->ShowModal() == mrOk)
         {
         save();
         return true;
         }
      else
         return false;
      }
   else
      return true;
   }

// ------------------------------------------------------------------
//  Short description:
//      component editor constructor.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
/*__fastcall TAPSTable_editor::TAPSTable_editor(TComponent* AComponent, _di_IFormDesigner* ADesigner)
   : TComponentEditor(AComponent, *ADesigner) { }

// ------------------------------------------------------------------
//  Short description:
//      called when user clicks on "edit chart"

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void __fastcall TAPSTable_editor::ExecuteVerb(int Index)
   {
   if (Index == 0)
      {
      // convert component to a TAPSChart
      TAPSTable* APSTable_ptr = dynamic_cast<TAPSTable*> (Component);
      if (APSTable_ptr != NULL)
         APSTable_ptr->edit();
      }
   }
// ------------------------------------------------------------------
//  Short description:
//      returns the number of verbs to put on context menu.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
int __fastcall TAPSTable_editor::GetVerbCount(void)
   {
   return 1;
   }
// ------------------------------------------------------------------
//  Short description:
//      returns the string to put on context menu.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
AnsiString __fastcall TAPSTable_editor::GetVerb(int Index)
   {
   switch(Index)
      {
      case 0 : return "Edit properties";
      }
   return "";
   }
*/

// ------------------------------------------------------------------
//  Short description:
//    Copy all fields from the specified data table.

//  Notes:

//  Changes:
//    DPH 10/4/01

// ------------------------------------------------------------------
void TAPSTable::copyFieldNamesFrom(const TAPSTable& from)
{
   vector<string> from_names;
   from.getFieldNames(from_names);
   for (vector<string>::iterator i = from_names.begin(); i < from_names.end();
         i++)
   {
      addField(*i);
   }
}

