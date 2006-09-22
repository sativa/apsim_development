//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "Frequency.h"
#include <general\string_functions.h>
#include <general\db_functions.h>

using namespace std;

//---------------------------------------------------------------------------
// Create the necessary fields in the result dataset.
//---------------------------------------------------------------------------
void Frequency::createFields(TDataSet* source, TDataSet* result)
   {
   vector<string> labels = getProperties("label");
   vector<string> filters = getProperties("FilterString");
   bool percent = Str_i_Eq(getProperty("percent"), "yes");

   if (labels.size() == filters.size())
      {
      addDBField(result, "Label", "x");
      if (percent)
         addDBField(result, "Percent", "1");
      else
         addDBField(result, "Count", "1");
      }
   }
//---------------------------------------------------------------------------
// Go do our processing, putting all results into 'data'
//---------------------------------------------------------------------------
void Frequency::process(TDataSet* source, TDataSet* result)
   {
   vector<string> labels = getProperties("label");
   vector<string> filters = getProperties("FilterString");
   bool percent = Str_i_Eq(getProperty("percent"), "yes");

   if (labels.size() == filters.size())
      {
      std::string originalFilter = source->Filter.c_str();

      int NumRecords = source->RecordCount;
      for (unsigned i = 0; i != labels.size(); i++)
         {
         string filter = filters[i];
         if (originalFilter != "")
            filter = originalFilter + " and " + filter;

         source->Filter = filter.c_str();
         source->Filtered = true;

         result->Append();
         result->FieldValues["Label"] = labels[i].c_str();
         if (percent)
            result->FieldValues["Percent"] = source->RecordCount * 1.0 / NumRecords * 100;
         else
            result->FieldValues["Count"] = source->RecordCount;
         result->Post();
         }
      if (originalFilter != "")
         source->Filter = originalFilter.c_str();
      else
         {
         source->Filtered = false;
         source->Filter = "";
         }
      }
   }

