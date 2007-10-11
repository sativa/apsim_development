//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "Filter.h"
#include "DataContainer.h"
#include <general\string_functions.h>
#include <general\db_functions.h>

using namespace std;

//---------------------------------------------------------------------------
// this function filters an existing dataset.
//---------------------------------------------------------------------------
void processFilter(DataContainer& parent,
                   const XMLNode& properties,
                   TDataSet& result)
   {
   TDataSet* source = parent.data(properties.childValue("source"));
   std::string filter = properties.childValue("FilterString");

   result.Active = false;
   if (source != NULL && filter != "")
      {
      result.FieldDefs->Assign(source->FieldDefs);

      result.Active = true;
      std::string originalFilter;
      if (source->Filtered)
         originalFilter = source->Filter.c_str();
      if (originalFilter != "")
         filter = originalFilter + " and " + filter;

      try
         {
         source->Filter = filter.c_str();
         source->Filtered = true;

         source->First();
         while (!source->Eof)
            {
            copyDBRecord(source, &result);
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
      catch (Exception& err)
         {
         source->Filter = "";
         }
      }
   }

