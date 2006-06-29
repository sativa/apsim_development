//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TSEGTable.h"
#include <general\stl_functions.h>
#include <general\string_functions.h>
#include <general\db_functions.h>
#include <general\vcl_functions.h>
#include <general\StringTokenizer.h>
#include <iterator>
using namespace std;

#pragma package(smart_init)
#pragma link "kbmMemTable"

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
   inForceRefresh = false;
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
//   this->DesignActivation = false;
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
   if (!ComponentState.Contains(csLoading) && !inForceRefresh)
      {
      inForceRefresh = true;
      DisableControls();
      TCursor savedCursor = Screen->Cursor;
      Screen->Cursor = crHourGlass;

      try
         {
         Active = false;
         IndexFieldNames = "";
         IndexDefs->Clear();
         if (createFields())
            {
            Active = true;
            storeRecords();
            }

         // Sort records.
         if (sortFieldNames != "")
            {
            if (sortFieldNames.Pos(";") > 0 || FieldDefs->IndexOf(sortFieldNames) != -1)
               {
               SortFields = sortFieldNames;
               Sort(TkbmMemTableCompareOptions());
               }
            }

         refreshLinkedComponents();
         }
      catch (const exception& error)  // one of our error messages.
         {
         if (errorMessage == "")
            errorMessage = error.what();
//         Active = false;
         }
      catch (const Exception& error)  // VCL error
         {
         if (errorMessage == "")
            errorMessage = error.Message.c_str();
//         Active = false;
         }
      catch (...)
         {
         Active = false;
         }

      EnableControls();

      Screen->Cursor = savedCursor;
      inForceRefresh = false;
      }
   }
// ------------------------------------------------------------------
// Refresh all linked components.
// ------------------------------------------------------------------
void TSEGTable::refreshLinkedComponents()
   {
   if (Active)
      {
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
      forceRefresh(false);
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
// add groupby field defs from source to this dataset.
// ------------------------------------------------------------------
void TSEGTable::addGroupByFieldDefsFromSource(void)
   {
   if (sourceDataset != NULL)
      {
      string sourceGroupByFields = sourceDataset->groupByFields.c_str();
      vector<string> groupByFields;
      splitIntoValues(sourceGroupByFields, ",", groupByFields);
      for (unsigned i = 0; i != groupByFields.size(); i++)
         {
         TFieldDef* groupByFieldDef = sourceDataset->FieldDefs->Find(groupByFields[i].c_str());
         TFieldDef* newFieldDef = FieldDefs->AddFieldDef();
         newFieldDef->Assign(groupByFieldDef);
         }
      }
   }
// ------------------------------------------------------------------
// add groupby values from specified dataset to this dataset.
// ------------------------------------------------------------------
void TSEGTable::addGroupByValuesFromSource()
   {
   if (sourceDataset != NULL)
      {
      vector<string> groupByFields;
      splitIntoValues(sourceDataset->groupByFields.c_str(), ",", groupByFields);
      for (unsigned i = 0; i != groupByFields.size(); i++)
         FieldValues[groupByFields[i].c_str()] = sourceDataset->FieldValues[groupByFields[i].c_str()];
      }
   }
// ------------------------------------------------------------------
// Calculate a groupby filter for current record.
// ------------------------------------------------------------------
string TSEGTable::calcGroupByFilter(void)
   {
   string filter;
   vector<string> groupByFields;
   splitIntoValues(groupByFieldNames.c_str(), ",", groupByFields);
   for (unsigned i = 0; i != groupByFields.size(); i++)
      {
      if (filter != "")
         filter += " and ";
      filter = groupByFields[i] + "=";
      string filterValue = AnsiString(FieldValues[groupByFields[i].c_str()]).c_str();
      if (Is_numerical(filterValue.c_str()))
         filter += filterValue;
      else
         filter += singleQuoted(filterValue);
      }
   return filter;
   }
// ------------------------------------------------------------------
// Calculate all group by filters.
// ------------------------------------------------------------------
void TSEGTable::calcGroupByFilters(void)
   {
   groupByFilters.erase(groupByFilters.begin(), groupByFilters.end());
   if (groupByFieldNames != "")
      {
      First();
      while (!Eof)
         {
         string groupByFilter = calcGroupByFilter();
         if (find(groupByFilters.begin(), groupByFilters.end(), groupByFilter)
             == groupByFilters.end())
             groupByFilters.push_back(groupByFilter);
         Next();
         }
      }
   }
// ------------------------------------------------------------------
// This method is called to begin iterating through records for the
// first series only.  Return true if data to be read.
// ------------------------------------------------------------------
bool TSEGTable::firstSeries(void)
   {
   if (!Active)
      return false;
   calcGroupByFilters();
   groupByFiltersI = groupByFilters.begin();
   if (groupByFiltersI == groupByFilters.end())
      {
      First();
      return !Eof;
      }
   return nextSeries();
   }

// ------------------------------------------------------------------
// This method is called to goto the next series of records.  Should
// be called after firstSeries.  Return true if data to be read.
// ------------------------------------------------------------------
bool TSEGTable::nextSeries(void)
   {
   if (groupByFiltersI != groupByFilters.end())
      {
      Filtered = false;
      Filter = groupByFiltersI->c_str();
      groupByFiltersI++;
      Filtered = true;
      First();
      }
   else
      cancelSeries();
   return Filtered;
   }
// ------------------------------------------------------------------
// Cancel the series ranging.  After this routine, the caller will
// be able to access all records in dataset.
// ------------------------------------------------------------------
void TSEGTable::cancelSeries(void)
   {
   Filtered = false;
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

