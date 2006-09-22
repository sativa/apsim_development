//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "Depth.h"
#include <general\db_functions.h>
#include <general\math_functions.h>
#include <general\string_functions.h>

using namespace std;

//---------------------------------------------------------------------------
// Create the necessary fields in the result dataset.
//---------------------------------------------------------------------------
void Depth::createFields(TDataSet* source, TDataSet* result)
   {
   discoverVariables(source);
   if (variableNames.size() > 0)
      addDBField(result, "Depth", "1.0");
   else
      addDBField(result, "Dummy", "?");

   for (unsigned v = 0; v != variableNames.size(); v++)
      addDBField(result, variableNames[v].c_str(), "1.0");
   }

//---------------------------------------------------------------------------
// Go do our processing, putting all results into 'data'
//---------------------------------------------------------------------------
void Depth::process(TDataSet* source, TDataSet* result)
   {
   // Loop through all series blocks and all records within that series.
   source->First();
   while (!source->Eof)
      {
      float DepthSoFar = 0;
      for (int layer = 0; layer != numLayers; layer++)
         {
         result->Append();
         for (unsigned v = 0; v != variableNames.size(); v++)
            {
            string sourceFieldName = variableNames[v] + "(" + itoa(layer+1) + ")";
            result->FieldValues[variableNames[v].c_str()] = source->FieldValues[sourceFieldName.c_str()];
            if (Str_i_Eq(variableNames[v], "dlayer"))
               {
               float previousDepth = DepthSoFar;
               DepthSoFar += (float) source->FieldValues[sourceFieldName.c_str()];
               float midPoint = (DepthSoFar + previousDepth) / 2;
               result->FieldValues["Depth"] = midPoint;
               }
            }
         result->Post();
         }

      source->Next();
      }
   }
// ------------------------------------------------------------------
// set one of our properties.
// ------------------------------------------------------------------
void Depth::discoverVariables(TDataSet* source)
   {
   // firstly look for dlayer to get the number of layers.
   numLayers = 0;
   for (int f = 0; f != source->FieldDefs->Count; f++)
      {
      TFieldDef* Field = source->FieldDefs->Items[f];
      string fieldName = Field->Name.c_str();

      if (Str_i_Eq(fieldName.substr(0, strlen("dlayer")), "dlayer"))
         numLayers = max(numLayers, atoi(splitOffBracketedValue(fieldName, '(', ')').c_str()));
      }

   // now go through all fields looking for profile variables.
   if (numLayers > 0)
      {
      string subStringToFind = "(" + itoa(numLayers) + ")";
      for (int f = 0; f != source->FieldDefs->Count; f++)
         {
         TFieldDef* Field = source->FieldDefs->Items[f];
         string fieldName = Field->Name.c_str();

         if (fieldName.find(subStringToFind) != string::npos)
            {
            splitOffBracketedValue(fieldName, '(', ')');
            variableNames.push_back(fieldName);
            }
         }
      }
   }

