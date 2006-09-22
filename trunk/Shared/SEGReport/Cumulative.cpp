//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "Cumulative.h"
#include <vector>
#include <string>
#include <general\string_functions.h>
#include <general\db_functions.h>

using namespace std;

//---------------------------------------------------------------------------
// Create the necessary fields in the result dataset.
//---------------------------------------------------------------------------
void Cumulative::createFields(TDataSet* source, TDataSet* result)
   {
   for (int i = 0; i != source->FieldDefs->Count; i++)
      {
      TFieldDef* fieldDef = source->FieldDefs->Items[i];
      if (fieldDef->DataType == ftFloat)
         {
         AnsiString newField = "CUMULATIVE_" + fieldDef->Name;
         addDBField(result, newField.c_str(), "1.0");
         }
      else
         result->FieldDefs->Add(fieldDef->Name, fieldDef->DataType,
                                fieldDef->Size, false);
      }
   }
//---------------------------------------------------------------------------
// Go do our processing, putting all results into 'data'
//---------------------------------------------------------------------------
void Cumulative::process(TDataSet* source, TDataSet* result)
   {
   // setup some space to store cumulative values for each column.
   int numColumns = source->FieldDefs->Count;
   double* sums = new double[numColumns];
   for (int i = 0; i != numColumns; i++)
      sums[i] = 0.0;

   // loop through all records.
   source->First();
   while (!source->Eof)
      {
      result->Append();
      for (int i = 0; i != source->FieldDefs->Count; i++)
         {
         TFieldDef* fieldDef = source->FieldDefs->Items[i];
         if (fieldDef->DataType == ftFloat)
            {
            sums[i] += source->Fields->Fields[i]->AsFloat;
            AnsiString newField = "CUMULATIVE_" + fieldDef->Name;
            result->FieldValues[newField] = sums[i];
            }
         else
            result->FieldValues[fieldDef->Name] = source->FieldValues[fieldDef->Name];
         }
      result->Post();
      source->Next();
      }

   delete [] sums;
   }

