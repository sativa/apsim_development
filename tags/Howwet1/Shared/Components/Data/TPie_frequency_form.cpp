//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TPie_frequency_form.h"
#include "TPie_frequency_analysis.h"
#include <strstream>
#include <iomanip>
using namespace std;
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "TAnalysis_form"
#pragma resource "*.dfm"
TPie_frequency_form *Percentile_frequency_form;
//---------------------------------------------------------------------------
__fastcall TPie_frequency_form::TPie_frequency_form(TComponent* Owner)
   : TAnalysis_form(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TPie_frequency_form::FormShow(TObject *Sender)
   {
   TAnalysis_form::FormShow(Sender);

   TPie_frequency_analysis* Freq_analysis_ptr = dynamic_cast<TPie_frequency_analysis*> (Analysis_ptr);

   vector<string> pivotNames;
   Freq_analysis_ptr->sourceDataset->getDataBlockNames(pivotNames);
   Stl_2_tstrings(pivotNames, BaseDataset->Items);
   Stl_2_tstrings(pivotNames, SecondDataset->Items);
   BaseDataset->Text = Freq_analysis_ptr->BaseDatasetName;
   SecondDataset->Text = Freq_analysis_ptr->SecondDatasetName;

   if (Freq_analysis_ptr != NULL)
      {
      Num_percentiles_edit->Text = IntToStr(Freq_analysis_ptr->Num_equal_percentiles);
      Num_percentiles_editChange(NULL);
      }
   }
//---------------------------------------------------------------------------
void __fastcall TPie_frequency_form::FormClose(TObject *Sender,
      TCloseAction &Action)
   {
   TAnalysis_form::FormClose(Sender, Action);
   if (ModalResult == mrOk)
      {
      TPie_frequency_analysis* Freq_analysis_ptr = dynamic_cast<TPie_frequency_analysis*> (Analysis_ptr);
      if (Freq_analysis_ptr != NULL)
         {
         for (int category = 0; category < Percentile_grid->RowCount; category++)
            {
            if (Percentile_grid->Cells[0][category].Length() > 0)
               Freq_analysis_ptr->Category_percentile[category] = StrToFloat(Percentile_grid->Cells[0][category]);
            else
               Freq_analysis_ptr->Category_percentile[category] = 0;
            }
         }
      Freq_analysis_ptr->BaseDatasetName = BaseDataset->Text;
      Freq_analysis_ptr->SecondDatasetName = SecondDataset->Text;
      }
   }
//---------------------------------------------------------------------------
void __fastcall TPie_frequency_form::Num_percentiles_editChange(
      TObject *Sender)
   {
   int num_percentiles;
   if (Num_percentiles_edit->Text.Length() > 0)
      num_percentiles = StrToInt(Num_percentiles_edit->Text);
   else
      num_percentiles = 0;

   if (num_percentiles <= 6)
      {
      for (int category = 0; category < Percentile_grid->RowCount; category++)
         {
         if (category < num_percentiles-1)
            {
            double value = 100.0 / num_percentiles;
            if (category > 0)
               value += StrToFloat(Percentile_grid->Cells[0][category-1]);
            ostrstream out;
            out.setf(ios::fixed, ios::floatfield);
            out << setprecision(1) << value << ends;
            Percentile_grid->Cells[0][category] = out.str();
            delete out.str();
            }

         else if (category == num_percentiles-1)
            Percentile_grid->Cells[0][category] = "100";
         else
            Percentile_grid->Cells[0][category] = "";
         }
      }
   else
      {
      Application->MessageBox("You are only allowed a maximum of 6 wedges", "Error", MB_ICONSTOP | MB_OK);
      Num_percentiles_edit->Text = "6";
      }
   }
//---------------------------------------------------------------------------

