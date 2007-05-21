//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "Stats.h"
#include <general\db_functions.h>
#include <general\math_functions.h>
#include <general\string_functions.h>
#include <general\stl_functions.h>
#include <numeric>

using namespace std;

//---------------------------------------------------------------------------
// Return true if the specified array of stats contains the specified stat
//---------------------------------------------------------------------------
bool StatsContains(const vector<string>& stats, const string& name)
   {
   return (find_if(stats.begin(), stats.end(),
                   CaseInsensitiveStringComparison(name)) != stats.end());
   }

//---------------------------------------------------------------------------
// Create the necessary fields in the result dataset.
//---------------------------------------------------------------------------
void Stats::createFields(TDataSet* source, TDataSet* result)
   {
   vector<string> fieldNames = getProperties("fieldname");
   vector<string> stats = getProperties("stat");

   // add fields.
   for (unsigned f = 0; f != fieldNames.size(); f++)
      for (unsigned s = 0; s != stats.size(); s++)
         addDBField(result, fieldNames[f] + "-" + stats[s], "1.0");
   }
//---------------------------------------------------------------------------
// Go do our processing, putting all results into 'data'
//---------------------------------------------------------------------------
void Stats::process(TDataSet* source, TDataSet* result)
   {
   vector<string> fieldNames = getProperties("fieldname");
   vector<string> stats = getProperties("stat");

   result->Append();
   for (unsigned f = 0; f != fieldNames.size(); f++)
      {
      // Loop through all series blocks and all records within that series.
      vector<double> values;
      source->First();
      while (!source->Eof)
         {
         try
            {
            values.push_back( StrToFloat(source->FieldValues[fieldNames[f].c_str()]) );
            }
         catch (const Exception& err)
            { }
         source->Next();
         }

      if (values.size() > 0)
         {
         if (StatsContains(stats, "mean"))
            result->FieldValues[string(fieldNames[f] + "-Mean").c_str()] = Calculate_mean(values);
         if (StatsContains(stats, "Count"))
            result->FieldValues[string(fieldNames[f] + "-Count").c_str()] = values.size();
         if (StatsContains(stats, "Minimum"))
            result->FieldValues[string(fieldNames[f] + "-Minimum").c_str()] = min_element(values.begin(), values.end(),
                                                 less<double>());
         if (StatsContains(stats, "Maximum"))
            result->FieldValues[string(fieldNames[f] + "-Maximum").c_str()] = max_element(values.begin(), values.end(),
                                                 less<double>());
         if (StatsContains(stats, "Sum"))
            result->FieldValues[string(fieldNames[f] + "-Sum").c_str()] = accumulate(values.begin(), values.end(), 0.0);
         if (StatsContains(stats, "10"))
            result->FieldValues[string(fieldNames[f] + "-10").c_str()] = Calculate_percentile(values, false, 10);
         if (StatsContains(stats, "20"))
            result->FieldValues[string(fieldNames[f] + "-20").c_str()] = Calculate_percentile(values, false, 20);
         if (StatsContains(stats, "30"))
            result->FieldValues[string(fieldNames[f] + "-30").c_str()] = Calculate_percentile(values, false, 30);
         if (StatsContains(stats, "40"))
            result->FieldValues[string(fieldNames[f] + "-40").c_str()] = Calculate_percentile(values, false, 40);
         if (StatsContains(stats, "50"))
            result->FieldValues[string(fieldNames[f] + "-50").c_str()] = Calculate_percentile(values, false, 50);
         if (StatsContains(stats, "60"))
            result->FieldValues[string(fieldNames[f] + "-60").c_str()] = Calculate_percentile(values, false, 60);
         if (StatsContains(stats, "70"))
            result->FieldValues[string(fieldNames[f] + "-70").c_str()] = Calculate_percentile(values, false, 70);
         if (StatsContains(stats, "80"))
            result->FieldValues[string(fieldNames[f] + "-80").c_str()] = Calculate_percentile(values, false, 80);
         if (StatsContains(stats, "90"))
            result->FieldValues[string(fieldNames[f] + "-90").c_str()] = Calculate_percentile(values, false, 90);
         }
      }
   result->Post();
   }

