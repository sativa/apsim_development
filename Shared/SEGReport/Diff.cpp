//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "Diff.h"
#include <general\db_functions.h>
#include <general\math_functions.h>
#include <general\string_functions.h>
#include <numeric>

using namespace std;

//---------------------------------------------------------------------------
// Create the necessary fields in the result dataset.
//---------------------------------------------------------------------------
void Diff::createFields(TDataSet* source, TDataSet* result)
   {
   string source2Name = getProperty("source2name");
   vector<string> diffFieldNames = getProperties("diffFieldName");
   vector<string> copyFieldNames = getProperties("copyFieldName");

   if (source2Name != "")
      {
      for (unsigned f = 0; f != copyFieldNames.size(); f++)
         {
         int i = source->FieldDefs->IndexOf(copyFieldNames[f].c_str());
         if (i != -1)
            {
            TFieldDef* field = source->FieldDefs->Items[i];
            result->FieldDefs->Add(field->Name, field->DataType, field->Size, false);
            }
         }

      for (unsigned f = 0; f != diffFieldNames.size(); f++)
         {
         int i = source->FieldDefs->IndexOf(diffFieldNames[f].c_str());
         if (i != -1)
            {
            TFieldDef* field = source->FieldDefs->Items[i];
            result->FieldDefs->Add(field->Name, field->DataType, field->Size, false);
            }
         }
      }
   }
//---------------------------------------------------------------------------
// Go do our processing, putting all results into 'data'
//---------------------------------------------------------------------------
void Diff::process(TDataSet* source, TDataSet* result)
   {
   string source2Name = getProperty("source2name");
   vector<string> diffFieldNames = getProperties("diffFieldName");
   vector<string> copyFieldNames = getProperties("copyFieldName");

   if (source2Name != "")
      {
      TDataSet* source2 = getDataSet(source2Name);
      if (source2 != NULL && source2->Active)
         {
         source->First();
         source2->First();
         while (!source->Eof && !source2->Eof)
            {
            result->Append();
            for (unsigned f = 0; f != copyFieldNames.size(); f++)
               result->FieldValues[copyFieldNames[f].c_str()] = source->FieldValues[copyFieldNames[f].c_str()];

            for (unsigned f = 0; f != diffFieldNames.size(); f++)
               {
               if (source2->FieldDefs->IndexOf(diffFieldNames[f].c_str()) != -1)
                  result->FieldValues[diffFieldNames[f].c_str()] = source->FieldValues[diffFieldNames[f].c_str()]
                                                                - source2->FieldValues[diffFieldNames[f].c_str()];
               }

            result->Post();
            source->Next();
            source2->Next();
            }
         }
      }
   }

