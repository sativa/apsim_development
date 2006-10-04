//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "Filter.h"
#include <general\string_functions.h>
#include <general\db_functions.h>

using namespace std;

//---------------------------------------------------------------------------
// Create the necessary fields in the result dataset.
//---------------------------------------------------------------------------
void Filter::createFields(TDataSet* source, TDataSet* result)
   {
   result->FieldDefs->Assign(source->FieldDefs);
   }

//---------------------------------------------------------------------------
// Go do our processing, putting all results into 'data'
//---------------------------------------------------------------------------
void Filter::process(TDataSet* source, TDataSet* result)
   {
   std::string filter = getProperty("FilterString");
   std::string originalFilter;
   if (source->Filtered)
      originalFilter = source->Filter.c_str();
   if (originalFilter != "")
      filter = originalFilter + " and " + filter;

   source->Filter = filter.c_str();
   source->Filtered = true;

   source->First();
   while (!source->Eof)
      {
      copyDBRecord(source, result);
      source->Next();
      }
   if (originalFilter != "")
      source->Filter = originalFilter.c_str();
   else
      {
      source->Filtered = false;
      source->Filter = "";
      }
   }

