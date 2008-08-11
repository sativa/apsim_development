//---------------------------------------------------------------------------
#pragma hdrstop
#include "RealSet.h"
#include "Kruskal_wallis.h"
#include <general\pch.h>
#include <vcl.h>

#include "KWTest.h"
#include "DataContainer.h"
#include "DataProcessor.h"
#include <general\db_functions.h>
#include <general\math_functions.h>
#include <general\string_functions.h>
#include <general\vcl_functions.h>

using namespace std;

static const double KRUSKAL_WALLIS_CRITICAL_VALUE = 0.1;

// ------------------------------------------------------------------
// Loop through all series in specified table and store the values
// as a new distribution in the specified vector of distributions.
// ------------------------------------------------------------------
void AddDistributionFromTable(TDataSet* table, vector<RealSet>& distributions,
                              const string& fieldName)
   {
   table->First();
   while (!table->Eof)
      {
      vector<double> values;
      getDBFieldValues(table, fieldName, values);
      if (values.size() > 0)
         {
         RealSet distribution;
         for (unsigned i = 0; i < values.size(); i++)
            distribution.add(values[i]);
         distributions.push_back(distribution);
         }
      table->Next();
      }
   }


//---------------------------------------------------------------------------
// Create the necessary fields in the result dataset.
//---------------------------------------------------------------------------
void processKWTest(DataContainer& parent,
                   const XMLNode& properties,
                   vector<TDataSet*> sources,
                   TDataSet& result)
   {
   string fieldName = parent.read(properties, "fieldName");

   if (sources.size() == 2 && fieldName != "")
      {
      TDataSet* source1 = sources[0];
      TDataSet* source2 = sources[1];
      if (source1 != NULL && source2 != NULL && source1->Active && source2->Active)
         {
         if (!result.Active)
            {
            result.FieldDefs->Clear();
            copySeriesFieldDefs(source1, result);
            addDBField(&result, "PValue", "1.0");
            addDBField(&result, "Description", "a");
            addDBField(&result, "AreNot", "a");
            result.Active = true;
            }

         vector<RealSet> distributions;
         AddDistributionFromTable(source1, distributions, fieldName);
         AddDistributionFromTable(source2, distributions, fieldName);

         if (distributions.size() == 2)
            {
            source1->First();
            KruskalWallisResult kwResult = KruskalWallis(distributions);
            double pValue = kwResult.p;

            result.Append();
            copySeriesValues(source1, result);
            result.FieldValues["PValue"] = pValue;
            if (pValue < KRUSKAL_WALLIS_CRITICAL_VALUE)
               {
               result.FieldValues["Description"] = "There is a significant difference between the distributions.";
               result.FieldValues["AreNot"] = "ARE";
               }

            else
               {
               result.FieldValues["Description"] = "There is NO significant difference between the distributions.";
               result.FieldValues["AreNot"] = "ARE NOT";
               }

            result.Post();
            }
         }
      }
   }

