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
AnsiString TSEGTable::errorMessage;

// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
__fastcall TSEGTable::TSEGTable(TComponent* Owner)
   : TkbmMemTable(Owner)
   {
   source = NULL;
   subscriptionComponents = new TStringList;
   addToToolbar = false;
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
   TkbmMemTable::Loaded();
   fixupSubReferences();
   }
// ------------------------------------------------------------------
// Set the subscription component names.
// ------------------------------------------------------------------
void __fastcall TSEGTable::setSubComponentNames(TStringList* compNames)
   {
   subscriptionComponents->Assign(compNames);
   }
// ------------------------------------------------------------------
// Set the sort field names.
// ------------------------------------------------------------------
void __fastcall TSEGTable::setSortFieldNames(AnsiString sortFields)
   {
   if (sortFieldNames != sortFields)
      {
      sortFieldNames = sortFields;
      forceRefresh();
      }
   }
// ------------------------------------------------------------------
// refresh the control only if it is active.
// ------------------------------------------------------------------
void TSEGTable::refresh (void)
   {
   if (!ComponentState.Contains(csLoading) && source == NULL)
      forceRefresh(false);
   }
// ------------------------------------------------------------------
// force refresh regardless of source.
// ------------------------------------------------------------------
void TSEGTable::forceRefresh(bool displayError)
   {
   if (!ComponentState.Contains(csLoading))
      {
      DisableControls();
      TCursor savedCursor = Screen->Cursor;
      Screen->Cursor = crHourGlass;

      try
         {
         Active = false;
         if (IndexDefs->Count > 0)
            DeleteIndex("mainIndex");

         if (createFields())
            {
            if (FieldDefs->IndexOf(SERIES_FIELD_NAME) == -1)
               {
               TFieldDef *fieldDef = FieldDefs->AddFieldDef();
               fieldDef->Name = SERIES_FIELD_NAME;
               fieldDef->DataType = ftString;
               fieldDef->Size = 200;
               fieldDef->FieldNo = 1;
               }

            SortFields = "";

            Active = true;

            // Go load all records.
            storeRecords();
            }
         else
            Active = true;
         }
      catch (const exception& error)  // one of our error messages.
         {
         if (errorMessage == "")
            errorMessage = error.what();
         Active = false;
         }
      catch (const Exception& error)  // VCL error
         {
         if (errorMessage == "")
            errorMessage = error.Message.c_str();
         Active = false;
         }
      catch (...)
         {
         Active = false;
         }
      refreshLinkedComponents();

      EnableControls();

      Screen->Cursor = savedCursor;
      if (displayError && errorMessage != "")
         {
//         ::MessageBox(NULL, errorMessage.c_str(), "Errors were encountered", MB_ICONSTOP | MB_OK);
         errorMessage = "";
         }
      }
   }
// ------------------------------------------------------------------
// Refresh all linked components.
// ------------------------------------------------------------------
void TSEGTable::refreshLinkedComponents()
   {
   if (Active)
      {
      // Create an index so that the range methods work.
      if (IndexDefs->Count == 0)
         {
         AnsiString indexFields = AnsiString(SERIES_FIELD_NAME);
         if (sortFields != "")
            indexFields += ";" + AnsiString(sortFields.c_str());
         AddIndex("mainIndex", indexFields, TIndexOptions());
         IndexFieldNames = indexFields;
         }

      for (unsigned i = 0; i != subscriptionEvents.size(); i++)
         {
         if (subscriptionEvents[i] != NULL)
            subscriptionEvents[i](this);
         }
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
      refresh();
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
   return "";
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
// Return the current series number - zero index based.
// ------------------------------------------------------------------
unsigned TSEGTable::getSeriesNumber(void)
   {
   if (seriesNames.size() == 0)
      getSeriesNames(seriesNames);
   return find(seriesNames.begin(), seriesNames.end(), getSeriesName())
          - seriesNames.begin();
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
   while (index >= (int)subscriptionEvents.size())
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
   if (subComponent == NULL)
      {
//      TComponent* reportComponent = getComponent<TComponent>(Owner->Owner, "report");
      subComponent = getComponent<TComponent> (Owner->Owner, compName.c_str());
      }

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
   forceRefresh();
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

