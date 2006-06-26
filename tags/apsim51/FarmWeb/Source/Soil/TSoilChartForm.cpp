//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TSoilChartForm.h"
#include <general\string_functions.h>
#include <general\db_functions.h>
#include <ApsimShared\ApsimSettings.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TSoilChartForm *SoilChartForm;
//---------------------------------------------------------------------------
__fastcall TSoilChartForm::TSoilChartForm(TComponent* Owner)
   : TForm(Owner), report(this)
   {
   }
//---------------------------------------------------------------------------
// populate the data component of the report with the contents of the
// specified grid.
//---------------------------------------------------------------------------
void TSoilChartForm::populateFromGrid(TAdvStringGrid* grid, const string& seriesName)
   {
   if (Visible)
      {
      ApsimSettings settings;
      string reportFileName;
      settings.read("Soil|Soil Chart Report", reportFileName, true);
      report.setZoomToFit(true);

      try
         {
         report.load(reportFileName);
         TDataSet* dataset = dynamic_cast<TDataSet*>(report.getAComponent("data"));
         report.setProperty("Data", "filenames", "");
         dataset->DisableControls();
         dataset->Active = false;
         dataset->FieldDefs->Clear();
         addDBField(dataset, "Depth", "15");
         addDBField(dataset, "water", "26");
         addDBField(dataset, "Series", "xxxx");
         addDBField(dataset, "Title", "xxxx");
         dataset->Active = true;
         addRecords(dataset, grid, 4, "(LL15)", seriesName.c_str());
         addRecords(dataset, grid, 3, "(DUL)", seriesName.c_str());
         int col = 7;
         while (col < grid->ColCount)
            {
            string cropName = grid->Cells[col][0].c_str();
            cropName.erase(cropName.find("\rLL"));
            addRecords(dataset, grid, col, cropName.c_str(), seriesName.c_str());
            col += 4;
            }

         report.refreshLinkedComponents();
         dataset->EnableControls();
         }
      catch (const runtime_error& err)
         {
         ShowMessage(err.what());
         }
      }
   }
//---------------------------------------------------------------------------
// add a series of records for the specified grid and column.
//---------------------------------------------------------------------------
void TSoilChartForm::addRecords(TDataSet* dataset, TStringGrid* grid, int col,
                                AnsiString series, AnsiString title)
   {
   vector<std::string> fieldNames;
   fieldNames.push_back("Depth");
   fieldNames.push_back("water");
   fieldNames.push_back("Series");
   fieldNames.push_back("Title");
   for (int row = 1; row != grid->RowCount; row++)
      {
      if (grid->Cells[0][row] == "")
         break;
      vector<std::string> fieldValues;
      fieldValues.push_back(grid->Cells[0][row].c_str());
      fieldValues.push_back(grid->Cells[col][row].c_str());
      fieldValues.push_back(series.c_str());
      fieldValues.push_back(title.c_str());
      appendDBRecord(dataset, fieldNames, fieldValues);
      }
   }
//---------------------------------------------------------------------------
// User has requested a print.
//---------------------------------------------------------------------------
void __fastcall TSoilChartForm::PrintButtonClick(TObject *Sender)
   {
   report.print(true);
   }
//---------------------------------------------------------------------------
// form has been shown - restore form to former position.
//---------------------------------------------------------------------------
void __fastcall TSoilChartForm::FormShow(TObject *Sender)
   {
   loadFormPosition(this);
   }
//---------------------------------------------------------------------------
// form has been closed - save form position
//---------------------------------------------------------------------------
void __fastcall TSoilChartForm::FormClose(TObject *Sender, TCloseAction &Action)
   {
   saveFormPosition(this);
   }
//---------------------------------------------------------------------------

