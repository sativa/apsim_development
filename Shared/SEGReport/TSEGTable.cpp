//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TSEGTable.h"
#include <general\stl_functions.h>
#include <general\db_functions.h>
#include <general\vcl_functions.h>
#include <general\StringTokenizer.h>
using namespace std;

#pragma package(smart_init)
#pragma link "kbmMemTable"

static const char* SERIES_FIELD_NAME = "Series";

// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
__fastcall TSEGTable::TSEGTable(TComponent* Owner)
   : TkbmMemTable(Owner)
   {
   source = NULL;
   BeforeOpen = beforeOpen;
   AfterOpen = afterOpen;
   subscriptionComponents = new TStringList;
   }
// ------------------------------------------------------------------
// destructor
// ------------------------------------------------------------------
__fastcall TSEGTable::~TSEGTable()
   {
   delete subscriptionComponents;
   }
//---------------------------------------------------------------------------
// The component has been loaded from the stream - go refresh ourselves.
//---------------------------------------------------------------------------
void __fastcall TSEGTable::Loaded(void)
   {
   try
      {
      TkbmMemTable::Loaded();
      fixupSubReferences();
      refresh();
      }
   catch (const Exception& error)
      {
      Application->MessageBox(error.Message.c_str(), "Error", MB_ICONSTOP | MB_OK);
      }
   }
// ------------------------------------------------------------------
// Set the subscription component names.
// ------------------------------------------------------------------
void __fastcall TSEGTable::setSubComponentNames(TStringList* compNames)
   {
   subscriptionComponents->Assign(compNames);
   }
// ------------------------------------------------------------------
// User has just done a Active=true - go add fields before the
// table is actually opened.
// ------------------------------------------------------------------
void __fastcall TSEGTable::beforeOpen(TDataSet* dataset)
   {
   if (!ComponentState.Contains(csLoading))
      {
      TCursor savedCursor = Screen->Cursor;
      Screen->Cursor = crHourGlass;

      if (IndexDefs->Count > 0)
         DeleteIndex("mainIndex");

      try
         {
         Active = false;
         FieldDefs->Clear();
         TFieldDef *fieldDef = FieldDefs->AddFieldDef();
         fieldDef->Name = SERIES_FIELD_NAME;
         fieldDef->DataType = ftString;
         fieldDef->Size = 200;
         SortFields = "";
         createFields();
         }
      catch (const exception& error)  // one of our error messages.
         {
         errors().insert(error.what());
         Active = false;
         }
      catch (const Exception& error)  // VCL error
         {
         errors().insert(error.Message.c_str());
         Active = false;
         }
      Screen->Cursor = savedCursor;
      }
   }
// ------------------------------------------------------------------
// User has just done a Active=true - go add records now that the
// table is open
// ------------------------------------------------------------------
void __fastcall TSEGTable::afterOpen(TDataSet* dataset)
   {
   DisableControls();
   TCursor savedCursor = Screen->Cursor;
   Screen->Cursor = crHourGlass;
   try
      {
      // Go load all records.
      storeRecords();

      // Create an index so that the range methods work.
      if (IndexDefs->Count == 0)
         {
         AnsiString indexFields = AnsiString(SERIES_FIELD_NAME);
         AddIndex("mainIndex", indexFields, TIndexOptions());
         IndexFieldNames = indexFields;
         }
      Sort(TkbmMemTableCompareOptions());

      for (unsigned i = 0; i != subscriptionEvents.size(); i++)
         {
         if (subscriptionEvents[i] != NULL)
            subscriptionEvents[i](this);
         }
      EnableControls();

      // force children to update themselves.
      First();
      Edit();
      Post();
      }
   catch (const exception& error)  // one of our error messages.
      {
      errors().insert(error.what());
      Active = false;
      }
   catch (const Exception& error)  // VCL error
      {
      errors().insert(error.Message.c_str());
      Active = false;
      }
   EnableControls();
   Screen->Cursor = savedCursor;
   }
// ------------------------------------------------------------------
// refresh the control only if it is active.
// ------------------------------------------------------------------
void TSEGTable::refresh (void)
   {
   if (!ComponentState.Contains(csLoading) && source == NULL)
      {
      Active = false;
      Active = true;
      }
   }
// ------------------------------------------------------------------
// The source dataset property has changed.  May need to do a
// refresh.
// ------------------------------------------------------------------
void __fastcall TSEGTable::setSourceDataset(TSEGTable* source)
   {
   if (source != sourceDataset)
      {
      if (sourceDataset != NULL)
         sourceDataset->removeDataChangeSubscription(Name.c_str());
      sourceDataset = source;
      sourceDataset->addDataChangeSubscription(Name + ".onSourceDataChanged");
      if (!ComponentState.Contains(csLoading))
         {
         Active = false;
         Active = true;
         }
      }
   }
// ------------------------------------------------------------------
// return the name of the year field name.
// ------------------------------------------------------------------
string TSEGTable::getYearFieldName(void) const throw(runtime_error)
   {
   // Loop through all fields - use the sow_year field if found.
   // Otherwise use anything that has a 'year' in it.
   for (int fieldI = 0; fieldI < FieldDefs->Count; fieldI++)
      {
      AnsiString fieldName = FieldDefs->Items[fieldI]->Name.LowerCase();
      if (fieldName == "sow_year" || fieldName.Pos("year") > 0)
         return FieldDefs->Items[fieldI]->Name.c_str();
      }
   throw runtime_error("Cannot find a year column in dataset.");
   }

// ------------------------------------------------------------------
// return a list of all unique series names.
// ------------------------------------------------------------------
void TSEGTable::getSeriesNames(vector<string>& seriesNames)
   {
   seriesNames.erase(seriesNames.begin(), seriesNames.end());
   First();
   while (!Eof)
      {
      string seriesName = getSeriesName();
      if (find(seriesNames.begin(),
               seriesNames.end(),
               seriesName) == seriesNames.end())
         seriesNames.push_back(seriesName.c_str());
      Next();
      }
   }
// ------------------------------------------------------------------
// Return the current series name.
// ------------------------------------------------------------------
string TSEGTable::getSeriesName(void)
   {
   if (!FieldValues[SERIES_FIELD_NAME].IsNull())
      return AnsiString(FieldValues[SERIES_FIELD_NAME]).c_str();
   else
      return "";
   }
// ------------------------------------------------------------------
// Set the series name for the current record.
// ------------------------------------------------------------------
void TSEGTable::setSeriesName(const std::string& seriesName)
   {
   Edit();
   FieldValues[SERIES_FIELD_NAME] = seriesName.c_str();
   Post();
   }
// ------------------------------------------------------------------
// add a factor to the series name for the current record.
// ------------------------------------------------------------------
void TSEGTable::addFactorToSeriesName(const std::string& factorName)
   {
   string seriesName = getSeriesName();
   if (seriesName != "")
      seriesName += ",";
   seriesName += factorName;
   setSeriesName(seriesName);
   }
// ------------------------------------------------------------------
// This method is called to begin iterating through records for the
// first series only.  Return true if data to be read.
// ------------------------------------------------------------------
bool TSEGTable::firstSeries(void)
   {
   getSeriesNames(seriesNames);
   currentSeriesI = seriesNames.begin();
   return nextSeries();
   }

// ------------------------------------------------------------------
// This method is called to goto the next series of records.  Should
// be called after firstSeries.  Return true if data to be read.
// ------------------------------------------------------------------
bool TSEGTable::nextSeries(void)
   {
   string seriesName = "zzzzzzzzzzzzzzzzzz";
   if (currentSeriesI != seriesNames.end())
      {
      seriesName = *currentSeriesI;
      currentSeriesI++;
      SetRangeStart();
      FieldByName(SERIES_FIELD_NAME)->AsString = seriesName.c_str();
      SetRangeEnd();
      FieldByName(SERIES_FIELD_NAME)->AsString = seriesName.c_str();
      ApplyRange();
      }
   else
      {
      CancelRange();
      Last();
      Next();  // this will force Eof = true
      }
   return !Eof;
   }
// ------------------------------------------------------------------
// Cancel the series ranging.  After this routine, the caller will
// be able to access all records in dataset.
// ------------------------------------------------------------------
void TSEGTable::cancelSeries(void)
   {
   CancelRange();
   }
// ------------------------------------------------------------------
// Add a subscription to the data changed event.
// ------------------------------------------------------------------
void TSEGTable::addDataChangeSubscription(AnsiString eventName)
   {
   int index = subscriptionComponents->IndexOf(eventName);
   if (index == -1)
      index = subscriptionComponents->Add(eventName);
   while (index >= subscriptionEvents.size())
      subscriptionEvents.push_back((TDataSetNotifyEvent)NULL);

   union
      {
      TDataSetNotifyEvent event;
      TMethod     asPointers;
      } u;
   StringTokenizer tokenizer(eventName.c_str(), ".");
   string compName = tokenizer.nextToken();
   string event = tokenizer.nextToken();
   TComponent* subComponent = getComponent<TComponent> (Owner, compName.c_str());
   if (subComponent != NULL)
      {
      u.asPointers.Code = subComponent->MethodAddress(event.c_str());
      u.asPointers.Data = subComponent;
      subscriptionEvents[index] = u.event;
      }
   }
// ------------------------------------------------------------------
// Remove a subscription to the data changed event.
// ------------------------------------------------------------------
void TSEGTable::removeDataChangeSubscription(AnsiString eventName)
   {
   int pos = subscriptionComponents->IndexOf(eventName);
   if (pos >= 0)
      {
      subscriptionComponents->Delete(pos);
      subscriptionEvents.erase(subscriptionEvents.begin() + pos);
      }
   }
// ------------------------------------------------------------------
// Event handler called when the source dataset changes.
// ------------------------------------------------------------------
void __fastcall TSEGTable::onSourceDataChanged(TDataSet* dataset)
   {
   Active = false;
   Active = true;
   }
// ------------------------------------------------------------------
// Fixup all subscription references.
// ------------------------------------------------------------------
void TSEGTable::fixupSubReferences(void)
   {
   for (int i = 0; i != subscriptionComponents->Count; i++)
      {
      subscriptionEvents.push_back((TDataSetNotifyEvent)NULL);
      addDataChangeSubscription(subscriptionComponents->Strings[i]);
      }
   }

