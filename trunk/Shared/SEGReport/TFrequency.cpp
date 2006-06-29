//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TFrequency.h"
#include <vector>
#include <string>
#include <fstream>
#include <sstream>
#include <general\string_functions.h>
#include <general\db_functions.h>
#include <general\vcl_functions.h>
#include <general\path.h>
#include <general\inifile.h>

using namespace std;
#pragma package(smart_init)
#pragma resource "ComponentRegistration.res"

//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
__fastcall TFrequency::TFrequency(TComponent* owner)
   : TSEGTable(owner)
   {
   Labels = new TStringList;
   Filters = new TStringList;
   Percent = false;
   }
//---------------------------------------------------------------------------
// destructor
//---------------------------------------------------------------------------
__fastcall TFrequency::~TFrequency()
   {
   delete Labels;
   delete Filters;
   }
//---------------------------------------------------------------------------
// set the 'Percent' property and refresh all data.
//---------------------------------------------------------------------------
void __fastcall TFrequency::setPercent(bool percent)
   {
   if (Percent != percent)
      {
      Percent = percent;
      forceRefresh();
      }
   }
//---------------------------------------------------------------------------
// set the 'Labels' property and refresh all data.
//---------------------------------------------------------------------------
void __fastcall TFrequency::setLabels(TStringList* labels)
   {
   Labels->Assign(labels);
   forceRefresh();
   }
//---------------------------------------------------------------------------
// set the 'Filters' property and refresh all data.
//---------------------------------------------------------------------------
void __fastcall TFrequency::setFilters(TStringList* filters)
   {
   Filters->Assign(filters);
   forceRefresh();
   }
//---------------------------------------------------------------------------
// Called by our base class to allow us to add any fielddefs we may want to.
// The table will be closed (Active=false) when this routine is called.
//---------------------------------------------------------------------------
bool TFrequency::createFields(void) throw(runtime_error)
   {
   try
      {
      if (source != NULL)
         {
         addDBField(this, "Label", "x");
         if (percent)
            addDBField(this, "Percent", "1");
         else
            addDBField(this, "Count", "1");
         addGroupByFieldDefsFromSource();
         return true;
         }
      }
   catch (const Exception& err)
      {
      }
   return false;
   }
//---------------------------------------------------------------------------
// Called by our base class to allow us to add records to the table.
// The table is open (Active = true) when this routine is called.
//---------------------------------------------------------------------------
void TFrequency::storeRecords(void) throw(runtime_error)
   {
   try
      {
      if (source != NULL && Labels->Count == Filters->Count)
         {
         for (int i = 0; i != Labels->Count; i++)
            {
            AnsiString FilterSt = macros.doReplacement(Owner->Owner, Filters->Strings[i].c_str());
            if (FilterSt != "")
               {
               int NumRecords = source->RecordCount;
               source->Filter = FilterSt;
               source->Filtered = true;
               // loop through all records.
               bool ok = source->firstSeries();
               while (ok)
                  {
                  Append();
                  FieldValues["Label"] = Labels->Strings[i];
                  if (Percent)
                     FieldValues["Percent"] = source->RecordCount * 1.0 / NumRecords * 100;
                  else
                     FieldValues["Count"] = source->RecordCount;
                  addGroupByValuesFromSource();
                  Post();
                  ok = source->nextSeries();
                  }
               source->Filtered = false;
               source->Filter = "";
               source->cancelSeries();
               }
            }
         }
      }
   catch (const Exception& err)
      {
      }
   }

