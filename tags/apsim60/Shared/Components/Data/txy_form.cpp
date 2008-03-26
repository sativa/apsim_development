//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TXY_form.h"
#include "TXY_analysis.h"
#include <general\stristr.h>
#include <general\string_functions.h>
using namespace std;
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "TAnalysis_form"
#pragma link "AdvGrid"
#pragma link "ccedlink"
#pragma link "AsgLinks"
#pragma link "BaseGrid"
#pragma resource "*.dfm"
TXY_form *XY_form;
//---------------------------------------------------------------------------
__fastcall TXY_form::TXY_form(TComponent* Owner)
   : TAnalysis_form(Owner)
{
//   allowMultipleVariables();
}
//---------------------------------------------------------------------------
void __fastcall TXY_form::FormShow(TObject *Sender)
   {
   TAnalysis_form::FormShow(Sender);
   TXY_analysis* xyAnalysisPtr = dynamic_cast<TXY_analysis*>(Analysis_ptr);

   for (int i = 0; i < VariableList->Items->Count; i++)
      {
      AnsiString name = VariableList->Items->Item[i]->Caption;
      XVariableList->Items->Add(name);
      if (stristr(name.c_str(), "year") != NULL)
         XVariableList->ItemIndex = XVariableList->Items->IndexOf(name);

      YVariableList->Cells[0][i+1] = name;
      YVariableList->AddCheckBox(0, i+1, xyAnalysisPtr->isYVariableSelected(name), false);
      YVariableList->Cells[1][i+1] = xyAnalysisPtr->getSeriesType(name);
      YVariableList->AddDataImage(1, i+1, 0, haLeft, vaCenter);
      YVariableList->AddCheckBox(2, i+1, xyAnalysisPtr->isYVariableOnY2(name), false);
      YVariableList->AddCheckBox(3, i+1, xyAnalysisPtr->isYVariableCumulative(name), false);
      }
   YVariableList->RowCount = VariableList->Items->Count + 1;
   XVariableList->ItemIndex = XVariableList->Items->IndexOf(
                              xyAnalysisPtr->Field_names_to_analyse->Items->Strings[0]);
   fillScenarioGrid();
   XVariableListChange(this);
   if (allowEnableOkButton())
       OkButton->Enabled = true;
   }
//---------------------------------------------------------------------------
void __fastcall TXY_form::FormClose(TObject *Sender, TCloseAction &Action)
   {
   if (ModalResult == mrOk)
      {
      TXY_analysis* xyAnalysisPtr = dynamic_cast<TXY_analysis*>(Analysis_ptr);
      xyAnalysisPtr->clearSeries();

      for (int i = 0; i != VariableList->Items->Count; i++)
         VariableList->Items->Item[i]->Checked = false;
      VariableList->Items->Item[XVariableList->ItemIndex]->Checked = true;

      // save variables
      for (int row = 1; row < YVariableList->RowCount; row++)
         {
         bool isChecked;
         YVariableList->GetCheckBoxState(0, row, isChecked);
         if (isChecked)
            {
            AnsiString name = YVariableList->Cells[0][row];
            int seriesType = StrToInt(YVariableList->Cells[1][row]);
            bool y2;
            YVariableList->GetCheckBoxState(2, row, y2);
            bool isCumulative;
            YVariableList->GetCheckBoxState(3, row, isCumulative);
            xyAnalysisPtr->addSeries(name, (SeriesTypes)seriesType, y2, isCumulative);
            }
         }

      // save stat lines.
      for (int row = 1; row < ScenarioGrid->RowCount; row++)
         {
         if (ScenarioGrid->Cells[1][row] != "")
            {
            // strip off leading and trailing [ ] characters;
            string contents = ScenarioGrid->Cells[1][row].c_str();
            contents = contents.substr(1, contents.length()-2);

            // convert comma separated text into stat. words.
            vector<string> stats;
            Split_string(contents, ",", stats);

            // store each stat into xyAnalysisPtr
            for (vector<string>::iterator statsI = stats.begin();
                                          statsI != stats.end();
                                          statsI++)
               xyAnalysisPtr->addStatLine(ScenarioGrid->Cells[0][row],
                                          textToStatType(*statsI));
            }
         }
      }
   TAnalysis_form::FormClose(Sender, Action);
   }
//---------------------------------------------------------------------------
void __fastcall TXY_form::YVariableListGetEditorType(TObject *Sender,
      int ACol, int ARow, TEditorType &AEditor)
   {
   if (ACol == 1)
      {
      AEditor = edCustom;
      YVariableList->EditLink = ImagePickerEditLink;
      }
   else
      AEditor = edNormal;
   }
//---------------------------------------------------------------------------
void __fastcall TXY_form::YVariableListGetEditorProp(TObject *Sender,
      int ACol, int ARow, TEditLink *AEditLink)
   {
   if (ACol == 1)
      {
      TImagePicker* imagePicker = dynamic_cast<TImagePicker*> (AEditLink->GetEditControl());
      imagePicker->EditHeight = 18;
      imagePicker->FocusBorder = false;
      imagePicker->BeginUpdate();
      imagePicker->Items->Clear();
      TImagePickerItem* item = imagePicker->Items->Add();
      item->Caption = "Markers";
      item->ImageIndex = 2;
      item = imagePicker->Items->Add();
      item->Caption = "Lines & markers";
      item->ImageIndex = 1;
      item = imagePicker->Items->Add();
      item->Caption = "Lines";
      item->ImageIndex = 0;
      item = imagePicker->Items->Add();
      item->Caption = "Bars";
      item->ImageIndex = 3;
      item = imagePicker->Items->Add();
      item->Caption = "Stacked bars";
      item->ImageIndex = 4;
      item = imagePicker->Items->Add();
      item->Caption = "Stacked bars 100%";
      item->ImageIndex = 5;
      imagePicker->EndUpdate();
      }
   }
//---------------------------------------------------------------------------
bool TXY_form::allowEnableOkButton(void)
   {
   bool enabled = false;
   for (int row = 1; row < YVariableList->RowCount && !enabled; row++)
      YVariableList->GetCheckBoxState(0, row, enabled);
   return enabled;
   }
//---------------------------------------------------------------------------
void __fastcall TXY_form::YVariableListCheckBoxClick(TObject *Sender,
      int ACol, int ARow, bool State)
   {
   if (ACol == 0)
      {
//      VariableList->Items->Item[ARow]->Checked = State;
      checkOkButton();
      }
   }
//---------------------------------------------------------------------------
void TXY_form::fillScenarioGrid(void)
   {
   vector<string> dataBlockNames;
   Analysis_ptr->sourceDataset->getAllDataBlockNames(dataBlockNames);
   unsigned row = 1;
   for (vector<string>::iterator dataBlockNameI = dataBlockNames.begin();
                                 dataBlockNameI != dataBlockNames.end();
                                 dataBlockNameI++)
      {
      ScenarioGrid->Cells[0][row] = dataBlockNameI->c_str();
      row++;
      }
   ScenarioGrid->RowCount = row;
   }
//---------------------------------------------------------------------------
void __fastcall TXY_form::ScenarioGridGetEditorType(TObject *Sender,
      int ACol, int ARow, TEditorType &AEditor)
   {
   if (ACol == 1)
      {
      AEditor = edCustom;
      ScenarioGrid->EditLink = CheckEditLink;
      }
   }
//---------------------------------------------------------------------------
void __fastcall TXY_form::ScenarioGridGetEditorProp(TObject *Sender,
      int ACol, int ARow, TEditLink *AEditLink)
   {
   if (ACol == 1)
      {
      TCheckListEdit* checkListEdit = dynamic_cast<TCheckListEdit*> (AEditLink->GetEditControl());
      checkListEdit->Items->Add("Mean");
      checkListEdit->Items->Add("10%");
      checkListEdit->Items->Add("20%");
      checkListEdit->Items->Add("30%");
      checkListEdit->Items->Add("40%");
      checkListEdit->Items->Add("50%");
      checkListEdit->Items->Add("60%");
      checkListEdit->Items->Add("70%");
      checkListEdit->Items->Add("80%");
      checkListEdit->Items->Add("90%");
      }
   }
//---------------------------------------------------------------------------
TXY_analysis::StatType TXY_form::textToStatType(const string& text)
   {
   if (text == "Mean")
      return TXY_analysis::mean;
   else if (text == "10%")
      return TXY_analysis::P10;
   else if (text == "20%")
      return TXY_analysis::P20;
   else if (text == "30%")
      return TXY_analysis::P30;
   else if (text == "40%")
      return TXY_analysis::P40;
   else if (text == "50%")
      return TXY_analysis::P50;
   else if (text == "60%")
      return TXY_analysis::P60;
   else if (text == "70%")
      return TXY_analysis::P70;
   else if (text == "80%")
      return TXY_analysis::P80;
   else if (text == "90%")
      return TXY_analysis::P90;
   else
      return TXY_analysis::mean;
   }
void __fastcall TXY_form::XVariableListChange(TObject *Sender)
   {
   if (XVariableList->ItemIndex != -1)
      VariableList->Items->Item[XVariableList->ItemIndex]->Checked = true;
   }
//---------------------------------------------------------------------------

