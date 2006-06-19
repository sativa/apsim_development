//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TDepth.h"
#include <general\db_functions.h>
#include <general\math_functions.h>
#include <general\string_functions.h>

using namespace std;
#pragma package(smart_init)
#pragma resource "ComponentRegistration.res"

//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
__fastcall TDepth::TDepth(TComponent* owner)
   : TSEGTable(owner)
   {
   }

//---------------------------------------------------------------------------
// destructor
//---------------------------------------------------------------------------
__fastcall TDepth::~TDepth()
   {
   }
//---------------------------------------------------------------------------
// Called by our base class to allow us to add any fielddefs we may want to.
// The table will be closed (Active=false) when this routine is called.
//---------------------------------------------------------------------------
bool TDepth::createFields(void) throw(runtime_error)
   {
   if (source != NULL)
      {
      FieldDefs->Clear();
      vector<string> variableNames;
      int numLayers;
      discoverVariables(variableNames, numLayers);
      if (variableNames.size() > 0)
         addDBField(this, "Depth", "1.0");
      for (unsigned v = 0; v != variableNames.size(); v++)
         addDBField(this, variableNames[v].c_str(), "1.0");
      return true;
      }
   return false;
   }
//---------------------------------------------------------------------------
// Called by our base class to allow us to add records to the table.
// The table is open (Active = true) when this routine is called.
//---------------------------------------------------------------------------
void TDepth::storeRecords(void) throw(runtime_error)
   {
   if (source != NULL)
      {
      // Loop through all series blocks and all records within that series.
      bool ok = source->firstSeries();
      while (ok)
         {
         vector<string> variableNames;
         int numLayers;
         discoverVariables(variableNames, numLayers);
         float DepthSoFar = 0;
         for (int layer = 0; layer != numLayers; layer++)
            {
            Append();
            for (unsigned v = 0; v != variableNames.size(); v++)
               {
               string sourceFieldName = variableNames[v] + "(" + itoa(layer+1) + ")";
               FieldValues[variableNames[v].c_str()] = source->FieldValues[sourceFieldName.c_str()];
               if (Str_i_Eq(variableNames[v], "dlayer"))
                  {
                  float previousDepth = DepthSoFar;
                  DepthSoFar += (float) source->FieldValues[sourceFieldName.c_str()];
                  float midPoint = (DepthSoFar + previousDepth) / 2;
                  FieldValues["Depth"] = midPoint;
                  }
               }
            Post();
            }

         ok = source->nextSeries();
         }
      source->cancelSeries();
      }
   }
// ------------------------------------------------------------------
// set one of our properties.
// ------------------------------------------------------------------
void TDepth::discoverVariables(std::vector<std::string>& variableNames,
                               int& numLayers)
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

