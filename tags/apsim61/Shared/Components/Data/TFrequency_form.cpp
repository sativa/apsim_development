//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TFrequency_form.h"
#include "TFrequency_analysis.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "TAnalysis_form"
#pragma link "AdvSpin"
#pragma link "paramtreeview"
#pragma resource "*.dfm"
TFrequency_form *Frequency_form;
//---------------------------------------------------------------------------
__fastcall TFrequency_form::TFrequency_form(TComponent* Owner)
   : TAnalysis_form(Owner)
   {
      Num_frequencies_spin->MaxValue = MAX_CATEGORIES;
   }
//---------------------------------------------------------------------------
void __fastcall TFrequency_form::FormShow(TObject *Sender)
   {
   TAnalysis_form::FormShow(Sender);

   TFrequency_analysis* Frequency_analysis_ptr = dynamic_cast<TFrequency_analysis*> (Analysis_ptr);
   if (Frequency_analysis_ptr != NULL)
      {
      //Num_frequencies_edit->Text = StrToInt(Frequency_analysis_ptr->Num_categories);
      Num_frequencies_spin->Value = Frequency_analysis_ptr->Num_categories;

      Frequency_radio->Checked = Frequency_analysis_ptr->Frequency_analysis;
      Probability_radio->Checked = !Frequency_radio->Checked;
      }
   }
//---------------------------------------------------------------------------
void __fastcall TFrequency_form::FormClose(TObject *Sender,
      TCloseAction &Action)
   {
   TAnalysis_form::FormClose(Sender, Action);
   if (ModalResult == mrOk)
      {
      TFrequency_analysis* Frequency_analysis_ptr = dynamic_cast<TFrequency_analysis*> (Analysis_ptr);
      if (Frequency_analysis_ptr != NULL)
         {
         //Frequency_analysis_ptr->Num_categories = StrToInt(Num_frequencies_edit->Text);
         Frequency_analysis_ptr->Num_categories = Num_frequencies_spin->Value;
         Frequency_analysis_ptr->Frequency_analysis = Frequency_radio->Checked;
         }
      }
   }
//---------------------------------------------------------------------------

