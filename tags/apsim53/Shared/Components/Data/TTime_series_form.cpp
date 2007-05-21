//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TTime_series_form.h"
#include "TTime_series_analysis.h"
#include <general\vcl_functions.h>
#include <vector>
#include <string>
using namespace std;
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "TAnalysis_form"
#pragma link "paramtreeview"
#pragma resource "*.dfm"
TTime_series_form *Time_series_form;
//---------------------------------------------------------------------------
__fastcall TTime_series_form::TTime_series_form(TComponent* Owner)
   : TAnalysis_form(Owner)
{
}
//---------------------------------------------------------------------------
// Changes:
//    DAH:  D-490 fix
//
void __fastcall TTime_series_form::FormShow(TObject *Sender)
   {
   ::TAnalysis_form::FormShow(Sender);
   TTime_series_analysis* Time_analysis_ptr = dynamic_cast<TTime_series_analysis*> (Analysis_ptr);
   if (Time_analysis_ptr != NULL)
      {
      // remove any 'year' variable from variable list - DAMEASY asked for this.
      for (int i = 0; i < VariableList->Items->Count; i++)
         {
         if (stricmp(VariableList->Items->Item[i]->Caption.c_str(), "year") == 0)
            VariableList->Items->Delete(i);
         }

      Time_series_mean_radio->Checked = Time_analysis_ptr->With_mean_line;
      Time_series_percentile_radio->Checked = Time_analysis_ptr->With_percentile_line;
      Diff_from_mean_radio->Checked = Time_analysis_ptr->Diff_from_mean;
      Diff_from_percentile_radio->Checked = Time_analysis_ptr->Diff_from_percentile;
      Percentile_edit->Text = IntToStr(Time_analysis_ptr->Percentile);
      Different_colours_checkbox->Checked = Time_analysis_ptr->Different_above_below_colours;
      }
   Enable_percentile(NULL);
   }
//---------------------------------------------------------------------------
void __fastcall TTime_series_form::FormClose(TObject *Sender,
      TCloseAction &Action)
   {
   ::TAnalysis_form::FormClose(Sender, Action);
   if (ModalResult == mrOk)
      {
      TTime_series_analysis* Time_analysis_ptr = dynamic_cast<TTime_series_analysis*> (Analysis_ptr);
      if (Time_analysis_ptr != NULL)
         {
         Time_analysis_ptr->With_mean_line = Time_series_mean_radio->Checked;
         Time_analysis_ptr->With_percentile_line = Time_series_percentile_radio->Checked;
         Time_analysis_ptr->Diff_from_mean = Diff_from_mean_radio->Checked;
         Time_analysis_ptr->Diff_from_percentile = Diff_from_percentile_radio->Checked;
         Time_analysis_ptr->Percentile = StrToInt(Percentile_edit->Text);
         Time_analysis_ptr->Base_pivot_value = Base_dataset_combo->Text;
         Time_analysis_ptr->Different_above_below_colours = Different_colours_checkbox->Checked;
         }
      }
   }
//---------------------------------------------------------------------------
void __fastcall TTime_series_form::Enable_percentile(TObject *Sender)
   {
   Percentile_edit->Visible = (Diff_from_percentile_radio->Checked || Time_series_percentile_radio->Checked);
   Percentile_label->Visible = Percentile_edit->Visible;

   Base_dataset_combo->Visible = (Diff_from_mean_radio->Checked ||
                                  Diff_from_percentile_radio->Checked ||
                                  Time_series_mean_radio->Checked ||
                                  Time_series_percentile_radio->Checked);
   Base_dataset_label->Visible = Base_dataset_combo->Visible;

   Different_colours_checkbox->Visible = (Time_series_mean_radio->Checked ||
                                          Time_series_percentile_radio->Checked);

   fillBaseDataSetCombo();
   }
//---------------------------------------------------------------------------
bool TTime_series_form::allowEnableOkButton(void)
   {
   fillBaseDataSetCombo();
   return true;
   }
//---------------------------------------------------------------------------
void TTime_series_form::fillBaseDataSetCombo(void)
   {
   TTime_series_analysis* Time_analysis_ptr = dynamic_cast<TTime_series_analysis*> (Analysis_ptr);
   AnsiString savedText = Time_analysis_ptr->Base_pivot_value;

   vector<string> dataBlockNames;
   Analysis_ptr->sourceDataset->getAllDataBlockNames(dataBlockNames);

   Base_dataset_combo->Items->Clear();
   TTreeNode* node = ScenarioTree->Items->GetFirstNode();
   while (node != NULL)
      {
      if (node->Count == 0 && node->ImageIndex == 0)
         {
         AnsiString dataBlockName = dataBlockNames[(int)node->Data].c_str();
         Base_dataset_combo->Items->Add(dataBlockName);
         }
      node = node->GetNext();
      }

   int indx = max(0, Base_dataset_combo->Items->IndexOf(savedText));
   if (indx >= 0)
      Base_dataset_combo->ItemIndex = indx;
   }
//---------------------------------------------------------------------------

