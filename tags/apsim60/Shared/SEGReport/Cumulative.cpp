//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "Cumulative.h"
#include "DataContainer.h"
#include <vector>
#include <string>
#include <general\string_functions.h>
#include <general\db_functions.h>

using namespace std;

//---------------------------------------------------------------------------
// this function adds a cumulative column for each source column
//---------------------------------------------------------------------------
void processCumulative(DataContainer& parent,
                       const XMLNode& properties,
                       TDataSet& result)
   {
   TDataSet* source = parent.data(parent.read(properties, "source"));
   if (source != NULL && source->Active)
      {
      result.Active = false;
      for (int i = 0; i != source->FieldDefs->Count; i++)
         {
         TFieldDef* fieldDef = source->FieldDefs->Items[i];
         if (fieldDef->DataType == ftFloat)
            {
            AnsiString newField = "CUMULATIVE_" + fieldDef->Name;
            addDBField(&result, newField.c_str(), "1.0");
            }
         else
            result.FieldDefs->Add(fieldDef->Name, fieldDef->DataType,
                                  fieldDef->Size, false);
         }

      result.Active = true;

      // setup some space to store cumulative values for each column.
      int numColumns = source->FieldDefs->Count;
      double* sums = new double[numColumns];
      for (int i = 0; i != numColumns; i++)
         sums[i] = 0.0;

      // loop through all records.
      source->First();
      while (!source->Eof)
         {
         result.Append();
         for (int i = 0; i != source->FieldDefs->Count; i++)
            {
            TFieldDef* fieldDef = source->FieldDefs->Items[i];
            if (fieldDef->DataType == ftFloat)
               {
               sums[i] += source->Fields->Fields[i]->AsFloat;
               AnsiString newField = "CUMULATIVE_" + fieldDef->Name;
               result.FieldValues[newField] = sums[i];
               }
            else
               result.FieldValues[fieldDef->Name] = source->FieldValues[fieldDef->Name];
            }
         result.Post();
         source->Next();
         }

      delete [] sums;
      }
   }
