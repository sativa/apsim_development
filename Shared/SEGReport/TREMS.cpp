//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TREMS.h"

using namespace std;
#pragma package(smart_init)

#pragma resource "ComponentRegistration.res"
//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
__fastcall TREMS::TREMS(TComponent* owner)
   : TSEGTable(owner)
   {
   experimentNames = new TStringList;
   treatmentNames = new TStringList;
   }

//---------------------------------------------------------------------------
// destructor
//---------------------------------------------------------------------------
__fastcall TREMS::~TREMS()
   {
   delete experimentNames;
   delete treatmentNames;
   }
//---------------------------------------------------------------------------
// set the 'mdbFile' property and refresh all data.
//---------------------------------------------------------------------------
void __fastcall TREMS::setFilename(AnsiString file)
   {
   if (mdbFilename != file)
      {
      mdbFilename = file;
      Active = false;
      Active = true;
      }
   }
//---------------------------------------------------------------------------
// set the 'experimentName' property and refresh all data.
//---------------------------------------------------------------------------
void __fastcall TREMS::setExperimentName(AnsiString ExperimentName)
   {
   if (experimentName != ExperimentName)
      {
      experimentName = ExperimentName;
      getTreatmentNames();
      Active = false;
      Active = true;
      }
   }
//---------------------------------------------------------------------------
// set the 'treatmentName' property and refresh all data.
//---------------------------------------------------------------------------
void __fastcall TREMS::setTreatmentName(AnsiString TreatmentName)
   {
   if (treatmentName != TreatmentName)
      {
      treatmentName = TreatmentName;
      Active = false;
      Active = true;
      }
   }
//---------------------------------------------------------------------------
// set the 'datasource' property and refresh all data.
//---------------------------------------------------------------------------
void __fastcall TREMS::setDatasource(AnsiString dataSource)
   {
   if (datasourceName != dataSource)
      {
      datasourceName = dataSource;
      Active = false;
      Active = true;
      }
   }
//---------------------------------------------------------------------------
// Called by our base class to allow us to add any fielddefs we may want to.
// The table will be closed (Active=false) when this routine is called.
//---------------------------------------------------------------------------
void TREMS::createFields(void) throw(runtime_error)
   {
   if (treatmentName != "")
      {
      // The next 2 lines are necessary when a report is loaded that already
      // has a TREMS component on it.  When this happens, treatmentIDs doesn't
      // have any values because the 'getExperimentNames' and 'getTreatmentNames'
      // routines have never been called.
      getExperimentNames();
      getTreatmentNames();
      int treatmentID = treatmentIDs[treatmentNames->IndexOf(treatmentName)];

      query = new TADOQuery(this);
      String Provider = "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=" +
         mdbFilename + ";Persist Security Info=False";
      query->ConnectionString = Provider;
      String SQL;
      if(datasourceName == "Statistics")
         {
         SQL = "TRANSFORM Avg(Stats.Mean) AS AvgOfMean \
         SELECT Stats.TreatmentID, Stats.Date \
         FROM Traits INNER JOIN Stats ON Traits.TraitID = Stats.TraitID \
         WHERE (((Stats.TreatmentID)=" + String(treatmentID) + ")) \
         GROUP BY Stats.TreatmentID, Stats.Date PIVOT Traits.Trait;";
         }
      else
         {
         SQL = "TRANSFORM Avg(PlotData.Value) AS AvgOfValue \
         SELECT Plots.TreatmentID, PlotData.Date \
         FROM Plots INNER JOIN (Traits INNER JOIN PlotData ON Traits.TraitID = \
         PlotData.TraitID) ON Plots.PlotID = PlotData.PlotID \
         WHERE (((Plots.TreatmentID)=" + String(treatmentID) + ")) \
         GROUP BY Plots.TreatmentID, PlotData.Date \
         ORDER BY PlotData.Date PIVOT Traits.Trait;";
         }

      query->SQL->Add(SQL);
      query->Active = true;

      // for some reason FieldDefs in TADOQuery is protected.  The next line
      // casts it back to a TDataSet to get around this.
      TDataSet* tds = query;

      for(int i=1;i < tds->FieldDefs->Count;i++)
         {
         TFieldDef* field = FieldDefs->AddFieldDef();
         field->Assign(tds->FieldDefs->Items[i]);
//         field->Name = tds->FieldDefs->Items[i]->Name;
//         field->DataType = tds->FieldDefs->Items[i]->DataType;
         }
      }
   }
//---------------------------------------------------------------------------
// Called by our base class to allow us to add records to the table.
// The table is open (Active = true) when this routine is called.
//---------------------------------------------------------------------------
void TREMS::storeRecords(void) throw(runtime_error)
   {
   if (treatmentName != "")
      {
      while(!query->Eof)
         {
         Append();
         FieldValues["Series"] = experimentName + " " + treatmentName;
         for(int i=1;i < query->FieldCount;i++)
            Fields->Fields[i] = query->Fields->Fields[i];
         Post();
         query->Next();
         }

      SortFields = "Date";
      delete query;
      }
   }

//---------------------------------------------------------------------------
// Called to return a list of experment names.
//---------------------------------------------------------------------------
TStrings* __fastcall TREMS::getExperimentNames(void)
   {
   TADOQuery *query = new TADOQuery(NULL);
   String Provider = "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=" +
         mdbFilename + ";Persist Security Info=False";
   query->ConnectionString = Provider;
   String SQL = "SELECT Experiments.ExpID, Experiments.Experiment FROM Experiments;";

   query->SQL->Add(SQL);
   query->Active = true;

   experimentNames->Clear();
   experimentIDs.erase(experimentIDs.begin(), experimentIDs.end());

   while(!query->Eof)
      {
      experimentNames->Add(query->FieldValues["Experiment"]);
      experimentIDs.push_back(query->FieldValues["ExpID"]);
      query->Next();
      }

   delete query;
   return experimentNames;
   }
//---------------------------------------------------------------------------
// Called to return a list of treatment names for the current experiment.
//---------------------------------------------------------------------------
TStrings* __fastcall TREMS::getTreatmentNames(void)
   {
   int experimentID = experimentIDs[experimentNames->IndexOf(experimentName)];

   TADOQuery *query = new TADOQuery(NULL);
   String Provider = "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=" +
         mdbFilename + ";Persist Security Info=False";
   query->ConnectionString = Provider;
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

   treatmentNames->Clear();
   treatmentIDs.erase(treatmentIDs.begin(), treatmentIDs.end());

   while(!query->Eof)
      {
      String Treatment;
      for(int i=2;i < query->FieldCount;i++)
         Treatment += query->Fields->Fields[i]->AsString + " ";
      treatmentNames->Add(Treatment);
      treatmentIDs.push_back(query->FieldValues["TreatmentID"]);
      query->Next();
      }

   delete query;
   return treatmentNames;
   }

