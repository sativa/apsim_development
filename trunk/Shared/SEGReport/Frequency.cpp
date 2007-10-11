//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "Frequency.h"
#include "DataContainer.h"
#include <general\string_functions.h>
#include <general\db_functions.h>

using namespace std;

//---------------------------------------------------------------------------
// Create the necessary fields in the result dataset.
//---------------------------------------------------------------------------
void processFrequency(DataContainer& parent,
                      const XMLNode& properties,
                      TDataSet& result)
   {
   vector<string> labels = properties.childValues("label");
   vector<string> filters = properties.childValues("FilterString");
   bool percent = Str_i_Eq(properties.childValue("percent"), "yes");
   TDataSet* source = parent.data(properties.childValue("source"));

   result.Active = false;
   result.FieldDefs->Clear();
   if (labels.size() == filters.size() && source != NULL)
      {
      addDBField(&result, "Label", "x");
      if (percent)
         addDBField(&result, "Percent", "1");
      else
         addDBField(&result, "Count", "1");

      result.Active = true;

      std::string originalFilter = source->Filter.c_str();
      int NumRecords = source->RecordCount;
      for (unsigned i = 0; i != labels.size(); i++)
         {
         string filter = filters[i];
         if (originalFilter != "")
            filter = originalFilter + " and " + filter;

         source->Filter = filter.c_str();
         source->Filtered = true;

         result.Append();
         result.FieldValues["Label"] = labels[i].c_str();
         if (percent)
            result.FieldValues["Percent"] = source->RecordCount * 1.0 / NumRecords * 100;
         else
            result.FieldValues["Count"] = source->RecordCount;
         result.Post();
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

