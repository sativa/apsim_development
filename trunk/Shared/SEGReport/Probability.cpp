//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "Probability.h"
#include <general\db_functions.h>
#include <general\math_functions.h>
#include <general\string_functions.h>

using namespace std;

//---------------------------------------------------------------------------
// Create the necessary fields in the result dataset.
//---------------------------------------------------------------------------
void Probability::createFields(TDataSet* source, TDataSet* result)
   {
   vector<string> fieldNames = getProperties("fieldname");

   for (unsigned f = 0; f != fieldNames.size(); f++)
      addDBField(result, fieldNames[f].c_str(), "1.0");
   addDBField(result, "Probability", "1.0");
   }

//---------------------------------------------------------------------------
// Go do our processing, putting all results into 'data'
//---------------------------------------------------------------------------
void Probability::process(TDataSet* source, TDataSet* result)
   {
   bool exceedence = Str_i_Eq(getProperty("exceedence"), "yes");
   vector<string> fieldNames = getProperties("fieldname");

   int numValues = 0;
   vector<vector<double> > values, probValues;
   for (unsigned f = 0; f != fieldNames.size(); f++)
      {
      values.push_back(vector<double>());
      probValues.push_back(vector<double>());
      getDBFieldValues(source, fieldNames[f], values[f]);
      Calculate_prob_dist(values[f], exceedence, probValues[f]);
      numValues = values[f].size();
      }

   // Now loop through all values and append a record for each.
   for (int recordNum = 0; recordNum < numValues; recordNum++)
      {
      result->Append();
      for (unsigned f = 0; f != fieldNames.size(); f++)
         {
         result->FieldValues[fieldNames[f].c_str()] = values[f][recordNum];
         if (f == 0)
            result->FieldValues["Probability"] = probValues[f][recordNum];
         }
      result->Post();
      }
   }

