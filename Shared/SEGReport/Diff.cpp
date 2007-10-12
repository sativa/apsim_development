//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "Diff.h"
#include "DataContainer.h"
#include <general\db_functions.h>
#include <general\math_functions.h>
#include <general\string_functions.h>
#include <general\vcl_functions.h>
#include <numeric>

using namespace std;

//---------------------------------------------------------------------------
// this creates a dataset that is calculated as the different between 2 datasets.
//---------------------------------------------------------------------------
void processDiff(DataContainer& parent,
                 const XMLNode& properties,
                 TDataSet& result)
   {
   vector<string> sourceNames = properties.childValues("source");
   vector<string> diffFieldNames = properties.childValues("diffFieldName");

   result.Active = false;
   result.FieldDefs->Clear();
   if (sourceNames.size() == 2 && diffFieldNames.size() > 0)
      {
      TDataSet* source1 = parent.data(sourceNames[0]);
      TDataSet* source2 = parent.data(sourceNames[1]);
      if (source1 != NULL && source2 != NULL && source1->Active && source2->Active)
         {
         TDataSet* source = parent.data(properties.childValue("source"));
         result.FieldDefs->Assign(source->FieldDefs);

         if (result.FieldDefs->Count > 0)
            {
            result.Active = true;

            source1->First();
            source2->First();
            while (!source1->Eof && !source2->Eof)
               {
               copyDBRecord(source1, &result);

               result.Edit();

               for (unsigned f = 0; f != diffFieldNames.size(); f++)
                  {
                  if (source2->FieldDefs->IndexOf(diffFieldNames[f].c_str()) != -1)
                     result.FieldValues[diffFieldNames[f].c_str()] = source1->FieldValues[diffFieldNames[f].c_str()]
                                                                   - source2->FieldValues[diffFieldNames[f].c_str()];
                  }

               result.Post();
               source1->Next();
               source2->Next();
               }
            }
         }
      }
   }
