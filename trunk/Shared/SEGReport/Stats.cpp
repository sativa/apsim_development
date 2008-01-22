//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "Stats.h"
#include "DataContainer.h"
#include <general\db_functions.h>
#include <general\math_functions.h>
#include <general\string_functions.h>
#include <general\stl_functions.h>
#include <numeric>

using namespace std;

// ------------------------------------------------------------------
// Calculate a groupby filter for current record.
// ------------------------------------------------------------------
string calcGroupByFilter(TDataSet* data, const vector<string>& groupByFieldNames)
   {
   string filter;
   for (unsigned i = 0; i != groupByFieldNames.size(); i++)
      {
      if (groupByFieldNames[i] != "")
         {
         if (filter != "")
            filter += " and ";
         filter = groupByFieldNames[i] + "=";
         string filterValue = AnsiString(data->FieldValues[groupByFieldNames[i].c_str()]).c_str();
         if (Is_numerical(filterValue.c_str()))
            filter += filterValue;
         else
            filter += singleQuoted(filterValue);
         }
      }
   return filter;
   }
// ------------------------------------------------------------------
// Calculate all group by filters.
// ------------------------------------------------------------------
vector<string> calcGroupByFilters(TDataSet* data, const vector<string>& groupByFieldNames)
   {
   vector<string> groupByFilters;

   if (groupByFieldNames.size() > 0)
      {
      data->First();
      while (!data->Eof)
         {
         string groupByFilter = calcGroupByFilter(data, groupByFieldNames);
         if (find(groupByFilters.begin(), groupByFilters.end(), groupByFilter)
             == groupByFilters.end())
             groupByFilters.push_back(groupByFilter);
         data->Next();
         }
      }
   else
      groupByFilters.push_back("");
   return groupByFilters;
   }



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
void processStats(DataContainer& parent,
                  const XMLNode& properties,
                  TDataSet& result)
   {
   result.Active = false;
   result.FieldDefs->Clear();

   vector<string> fieldNames = parent.reads(properties, "fieldname");
   vector<string> stats = parent.reads(properties, "stat");

   // add fields.
   for (unsigned f = 0; f != fieldNames.size(); f++)
      for (unsigned s = 0; s != stats.size(); s++)
         addDBField(&result, fieldNames[f] + "-" + stats[s], "1.0");

   TDataSet* source = parent.data(parent.read(properties, "source"));

   if (result.FieldDefs->Count > 0 && source != NULL && source->Active)
      {
      vector<string> groupByFieldNames = parent.reads(properties, "GroupByFieldName");
      for (unsigned i = 0; i != groupByFieldNames.size(); i++)
         {
         int f = source->FieldDefs->IndexOf(groupByFieldNames[i].c_str());
         if (f != -1)
            result.FieldDefs->Add(groupByFieldNames[i].c_str(),
                                  source->FieldDefs->Items[f]->DataType,
                                  source->FieldDefs->Items[f]->Size,
                                  false);
         }
      vector<string> groupByFilters = calcGroupByFilters(source, groupByFieldNames);


      result.Active = true;

      for (unsigned i = 0; i != groupByFilters.size(); i++)
         {
         if (groupByFilters[i] != "")
            {
            source->Filter = groupByFilters[i].c_str();
            source->Filtered = true;
            }

         result.Append();
         for (unsigned f = 0; f != fieldNames.size(); f++)
            {
            // Loop through all series blocks and all records within that series.
            vector<double> values;
            source->First();

            for (unsigned i = 0; i != groupByFieldNames.size(); i++)
               result.FieldValues[groupByFieldNames[i].c_str()]
                  = source->FieldValues[groupByFieldNames[i].c_str()];

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
                  result.FieldValues[string(fieldNames[f] + "-Mean").c_str()] = Calculate_mean(values);
               if (StatsContains(stats, "Count"))
                  result.FieldValues[string(fieldNames[f] + "-Count").c_str()] = values.size();
               if (StatsContains(stats, "Minimum"))
                  result.FieldValues[string(fieldNames[f] + "-Minimum").c_str()] = min_element(values.begin(), values.end(),
                                                       less<double>());
               if (StatsContains(stats, "Maximum"))
                  result.FieldValues[string(fieldNames[f] + "-Maximum").c_str()] = max_element(values.begin(), values.end(),
                                                       less<double>());
               if (StatsContains(stats, "Sum"))
                  result.FieldValues[string(fieldNames[f] + "-Sum").c_str()] = accumulate(values.begin(), values.end(), 0.0);
               if (StatsContains(stats, "10"))
                  result.FieldValues[string(fieldNames[f] + "-10").c_str()] = Calculate_percentile(values, false, 10);
               if (StatsContains(stats, "20"))
                  result.FieldValues[string(fieldNames[f] + "-20").c_str()] = Calculate_percentile(values, false, 20);
               if (StatsContains(stats, "30"))
                  result.FieldValues[string(fieldNames[f] + "-30").c_str()] = Calculate_percentile(values, false, 30);
               if (StatsContains(stats, "40"))
                  result.FieldValues[string(fieldNames[f] + "-40").c_str()] = Calculate_percentile(values, false, 40);
               if (StatsContains(stats, "50"))
                  result.FieldValues[string(fieldNames[f] + "-50").c_str()] = Calculate_percentile(values, false, 50);
               if (StatsContains(stats, "60"))
                  result.FieldValues[string(fieldNames[f] + "-60").c_str()] = Calculate_percentile(values, false, 60);
               if (StatsContains(stats, "70"))
                  result.FieldValues[string(fieldNames[f] + "-70").c_str()] = Calculate_percentile(values, false, 70);
               if (StatsContains(stats, "80"))
                  result.FieldValues[string(fieldNames[f] + "-80").c_str()] = Calculate_percentile(values, false, 80);
               if (StatsContains(stats, "90"))
                  result.FieldValues[string(fieldNames[f] + "-90").c_str()] = Calculate_percentile(values, false, 90);
               }
            }

         result.Post();

         source->Filter = "";
         source->Filtered = false;
         }
      }
   }
