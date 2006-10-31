//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "REMS.h"
#include <general\string_functions.h>
#include <general\stl_functions.h>
using namespace std;

//---------------------------------------------------------------------------
// Create the necessary fields in the result dataset.
//---------------------------------------------------------------------------
void REMS::createFields(TDataSet* source, TDataSet* result)
   {
   fileName = getProperty("fileName");
   string experimentName = getProperty("experiment");
   vector<std::string> treatmentNames = getProperties("treatment");
   string dataSourceName = getProperty("dataSource");

   lookupExperimentNames();
   if (experimentName != "")
      {
      lookupTreatmentNames(experimentName);
      if (treatmentNames.size() > 0)
         {
         TADOQuery* query = createQuery(treatmentNames[0], dataSourceName);

         // for some reason FieldDefs in TADOQuery is protected.  The next line
         // casts it back to a TDataSet to get around this.
         TDataSet* tds = query;

         result->FieldDefs->Clear();
         result->FieldDefs->Add("experiment", ftString, 50, true);
         result->FieldDefs->Add("treatment", ftString, 50, true);

         for(int i=0;i < tds->FieldDefs->Count;i++)
            {
            TFieldDef* field = result->FieldDefs->AddFieldDef();
            field->Assign(tds->FieldDefs->Items[i]);
            field->Attributes.Clear();
            }
         }
      }
   }
//---------------------------------------------------------------------------
// Go do our processing, putting all results into 'data'
//---------------------------------------------------------------------------
void REMS::process(TDataSet* source, TDataSet* result)
   {
   string experimentName = getProperty("experiment");
   vector<std::string> treatmentNames = getProperties("treatment");
   string dataSourceName = getProperty("dataSource");

   for (unsigned t = 0; t != treatmentNames.size(); t++)
      {
      TADOQuery* query = createQuery(treatmentNames[t], dataSourceName);
      while(!query->Eof)
         {
         result->Append();
         result->FieldValues["experiment"] = experimentName.c_str();
         result->FieldValues["treatment"] = treatmentNames[t].c_str();
         for(int i=0;i < query->FieldCount;i++)
            result->Fields->Fields[i+2] = query->Fields->Fields[i];
         result->Post();
         query->Next();
         }
      delete query;
      }
   }

//---------------------------------------------------------------------------
// Called to return a list of experment names.
//---------------------------------------------------------------------------
void REMS::lookupExperimentNames()
   {
   string fileName = getProperty("fileName");
   if (fileName != "")
      {
      TADOQuery *query = new TADOQuery(NULL);
      string provider = "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=" +
            fileName + ";Persist Security Info=False";
      query->ConnectionString = provider.c_str();
      String SQL = "SELECT Experiments.ExpID, Experiments.Experiment FROM Experiments;";
      query->SQL->Add(SQL);
      query->Active = true;

      allExperimentNames.erase(allExperimentNames.begin(), allExperimentNames.end());
      experimentIDs.erase(experimentIDs.begin(), experimentIDs.end());

      while(!query->Eof)
         {
         allExperimentNames.push_back(AnsiString(query->FieldValues["Experiment"]).c_str());
         experimentIDs.push_back(query->FieldValues["ExpID"]);
         query->Next();
         }

      delete query;
      }
   }
//---------------------------------------------------------------------------
// Called to return a list of treatment names for the current experiment.
//---------------------------------------------------------------------------
void REMS::lookupTreatmentNames(const string& experimentName)
   {
   string fileName = getProperty("fileName");
   if (fileName != "")
      {
      unsigned experimentIndex = find(allExperimentNames.begin(), allExperimentNames.end(),
                                      experimentName) - allExperimentNames.begin();
      if (experimentIndex >= allExperimentNames.size())
         throw runtime_error("Cannot find experiment name: " + experimentName);

      int experimentID = experimentIDs[experimentIndex];

      TADOQuery *query = new TADOQuery(NULL);
      string provider = "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=" +
            fileName + ";Persist Security Info=False";
      query->ConnectionString = provider.c_str();
      String SQL = "TRANSFORM First(Levels.Level) AS FirstOfLevel \
         SELECT Experiments.ExpID, Designs.TreatmentID \
         FROM (Experiments INNER JOIN Treatments ON Experiments.ExpID = \
         Treatments.ExpID) INNER JOIN ((Factors INNER JOIN Levels ON \
         Factors.FactorID = Levels.FactorID) INNER JOIN Designs ON Levels.LevelID = \
         Designs.LevelID) ON Treatments.TreatmentID = Designs.TreatmentID \
         WHERE (((Experiments.ExpID)=" + String(experimentID) + ")) \
         GROUP BY Experiments.ExpID, Designs.TreatmentID \
         ORDER BY Designs.TreatmentID \
         PIVOT Factors.Factor;";

      query->SQL->Add(SQL);
      query->Active = true;

      allTreatmentNames.erase(allTreatmentNames.begin(), allTreatmentNames.end());
      treatmentIDs.erase(treatmentIDs.begin(), treatmentIDs.end());

      while(!query->Eof)
         {
         String treatment;
         for(int i=2;i < query->FieldCount;i++)
            treatment += query->Fields->Fields[i]->AsString + " ";
         allTreatmentNames.push_back(treatment.c_str());
         treatmentIDs.push_back(query->FieldValues["TreatmentID"]);
         query->Next();
         }

      delete query;
      }
   }

//---------------------------------------------------------------------------
// Create a query for the given experiment and treatment name.
//---------------------------------------------------------------------------
TADOQuery* REMS::createQuery(const std::string treatmentName,
                             const std::string& dataSourceName)
   {
   // Need to convert the treatment name to an ID.
   vector<string>::iterator i = find_if(allTreatmentNames.begin(), allTreatmentNames.end(),
                                        CaseInsensitiveStringComparison(treatmentName));
   if (i == allTreatmentNames.end())
      throw runtime_error("Cannot find treatment name: " + treatmentName);
   int treatmentID = treatmentIDs[i-allTreatmentNames.begin()];

   TADOQuery* query = new TADOQuery(NULL);
   string provider = "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=" +
      fileName + ";Persist Security Info=False";
   query->ConnectionString = provider.c_str();
   String SQL;
   if(dataSourceName == "Statistics")
      {
      SQL = "TRANSFORM Avg(Stats.Mean) AS AvgOfMean \
      SELECT Stats.TreatmentID, Stats.Date \
      FROM Traits INNER JOIN Stats ON Traits.TraitID = Stats.TraitID \
      WHERE (((Stats.TreatmentID)=" + String(treatmentID) + ")) \
      GROUP BY Stats.TreatmentID, Stats.Date PIVOT Traits.Trait;";
      }
   else if (dataSourceName == "Crop")
      {
      SQL = "TRANSFORM Avg(PlotData.Value) AS AvgOfValue \
      SELECT Plots.TreatmentID, PlotData.Date \
      FROM Plots INNER JOIN (Traits INNER JOIN PlotData ON Traits.TraitID = \
      PlotData.TraitID) ON Plots.PlotID = PlotData.PlotID \
      WHERE (((Plots.TreatmentID)=" + String(treatmentID) + ")) \
      GROUP BY Plots.TreatmentID, PlotData.Date \
      ORDER BY PlotData.Date PIVOT Traits.Trait;";
      }
   else if (dataSourceName == "Soil Layered")
      {
      SQL = "TRANSFORM Avg(SoilLayerData.Value) AS AvgOfValue \
      SELECT Treatments.TreatmentID, SoilLayerData.Date, SoilLayerData.DepthFrom, \
      SoilLayerData.DepthTo FROM Treatments INNER JOIN (Traits INNER JOIN (Plots INNER \
      JOIN SoilLayerData ON Plots.PlotID = SoilLayerData.PlotID) ON \
      Traits.TraitID = SoilLayerData.TraitID) ON Treatments.TreatmentID = Plots.TreatmentID \
      WHERE (((Plots.TreatmentID)=" + String(treatmentID) + ")) \
      GROUP BY Treatments.TreatmentID, SoilLayerData.Date, \
      SoilLayerData.DepthFrom, SoilLayerData.DepthTo \
      ORDER BY SoilLayerData.Date, SoilLayerData.DepthFrom \
      PIVOT Traits.Trait;";
      }

   query->SQL->Add(SQL);
   query->Active = true;
   return query;
   }

