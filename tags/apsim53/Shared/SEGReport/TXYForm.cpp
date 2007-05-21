//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TXYForm.h"
#include "TCumulative.h"
#include <general\db_functions.h>
#include <general\vcl_functions.h>
#include <general\stristr.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "AdvCGrid"
#pragma link "AdvGrid"
#pragma link "AdvPanel"
#pragma link "BaseGrid"
#pragma link "TPropertyForm"
#pragma resource "*.dfm"
TXYForm *XYForm;
//---------------------------------------------------------------------------
__fastcall TXYForm::TXYForm(TComponent* Owner)
   : TPropertyForm(Owner)
   {
   }
//---------------------------------------------------------------------------
void TXYForm::setComponent(TComponent* component)
   {
   TPropertyForm::setComponent(component);
   graph = dynamic_cast<TXYGraph*>(component);
   bool sourceIsCumulative = (dynamic_cast<TCumulative*>(graph->source) != NULL);
   if (sourceIsCumulative)
      YVariableList->ColCount = 4;
   else
      YVariableList->ColCount = 3;
   CumX->Visible = sourceIsCumulative;
   CumLabel->Visible = sourceIsCumulative;

   vector<string> variableNames;
   if (graph->source != NULL)
      getDBFieldNames(graph->source, variableNames);

   // populate the X variable list.
   Stl_2_tstrings(variableNames, XVariableList->Items);
   if (graph->Series->Count > 0)
      {
      XVariableList->ItemIndex = XVariableList->Items->IndexOf(graph->Series->Items[0]->XFieldName);
      CumX->Checked = graph->Series->Items[0]->IsXCumulative;
      }

   else
      {
      // go looking for a "year" column.
      for (unsigned i = 0; i < variableNames.size(); i++)
         {
         if (stristr(variableNames[i].c_str(), "year") != NULL)
            {
            XVariableList->ItemIndex = i;
            break;
            }
         }
      }

   // populate the y variable list.
   int numVariables;
   if (sourceIsCumulative)
      numVariables = variableNames.size()/2;
   else
      numVariables = variableNames.size();

   YVariableList->RowCount = max(numVariables + 1, 2);

   for (unsigned i = 0; i < numVariables; i++)
      {
      int row = i+1;
      YVariableList->Cells[0][row] = variableNames[i].c_str();

      int posSeries = graph->Series->IndexOf(variableNames[i].c_str());
      bool checked = (posSeries != -1);
      bool onY2 = false;
      bool cumulative = false;
      int seriesType = 2;
      if (posSeries >= 0)
         {
         onY2 = (graph->Series->Items[posSeries]->YAxisLocation == right_axis);
         cumulative = graph->Series->Items[posSeries]->IsYCumulative;
         seriesType = (int) graph->Series->Items[posSeries]->SeriesType;
         }

      YVariableList->AddCheckBox(0, row, checked, false);
      YVariableList->Ints[1][row] = seriesType;
      YVariableList->AddDataImage(1, row, 0, haLeft, vaCenter);
      YVariableList->AddCheckBox(2, row, onY2, false);
      YVariableList->AddCheckBox(3, row, cumulative, false);
      }
   populateGraph();
   }
//---------------------------------------------------------------------------
void __fastcall TXYForm::YVariableListComboCloseUp(TObject *Sender,
      int ARow, int ACol)
   {
   XVariableList->SetFocus(); // gets rid of the combo box editor.
   YVariableList->SetFocus();
   }
//---------------------------------------------------------------------------
void __fastcall TXYForm::YVariableListEditingDone(TObject *Sender)
   {
   if (YVariableList->Col == 1)
      {
      YVariableList->Cells[1][YVariableList->Row] = IntToStr(YVariableList->Combobox->ItemIndex);
      populateGraph();
      }
   }
//---------------------------------------------------------------------------
void TXYForm::populateGraph(void)
   {
   TXYGraphSeriesList* allSeries = new TXYGraphSeriesList(NULL);

   if (XVariableList->ItemIndex >= 0)
      {
      for (int row = 1; row != YVariableList->RowCount; row++)
         {
         bool checked;
         YVariableList->GetCheckBoxState(0, row, checked);
         if (checked)
            {
            TXYGraphSeries* newSeries = allSeries->Add();
            newSeries->XFieldName = XVariableList->Text;
            newSeries->IsXCumulative = CumX->Checked;
            newSeries->FieldNames->Add(YVariableList->Cells[0][row]);
            newSeries->SeriesTitle = YVariableList->Cells[0][row];

            bool y2Checked;
            YVariableList->GetCheckBoxState(2, row, y2Checked);
            if (y2Checked)
               newSeries->YAxisLocation = right_axis;
            else
               newSeries->YAxisLocation = left_axis;

            newSeries->SeriesType = (SeriesTypes) YVariableList->Ints[1][row];

            bool cumChecked;
            YVariableList->GetCheckBoxState(3, row, cumChecked);
            newSeries->IsYCumulative = cumChecked;

            }
         }
      }
   graph->Series = allSeries;
   delete allSeries;
   }
//---------------------------------------------------------------------------
void __fastcall TXYForm::YVariableListCheckBoxClick(TObject *Sender,
      int ACol, int ARow, bool State)
   {
   populateGraph();
   }
//---------------------------------------------------------------------------
void __fastcall TXYForm::ChartPropertyLabelClick(TObject *Sender)
   {
   graph->userEdit();
   }
//---------------------------------------------------------------------------

void __fastcall TXYForm::XVariableListChange(TObject *Sender)
   {
   populateGraph();
   }
//---------------------------------------------------------------------------
void __fastcall TXYForm::CumXClick(TObject *Sender)
   {
   populateGraph();
   }
//---------------------------------------------------------------------------

