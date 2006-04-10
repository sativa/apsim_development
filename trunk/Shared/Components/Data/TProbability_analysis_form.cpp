//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TProbability_analysis_form.h"
#include "TProbability_analysis.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "TAnalysis_form"
#pragma link "paramtreeview"
#pragma resource "*.dfm"
TProbability_analysis_form *Probability_analysis_form;
//---------------------------------------------------------------------------
__fastcall TProbability_analysis_form::TProbability_analysis_form(TComponent* Owner)
   : TAnalysis_form(Owner)
   {
   }
//---------------------------------------------------------------------------
void __fastcall TProbability_analysis_form::FormShow(TObject *Sender)
   {
   TAnalysis_form::FormShow(Sender);
   TProbability_analysis* Prob_analysis_ptr = dynamic_cast<TProbability_analysis*> (Analysis_ptr);
   if (Prob_analysis_ptr != NULL)
      Probability_exceedence_radio->Checked = Prob_analysis_ptr->Prob_exceedence;
   }
//---------------------------------------------------------------------------

void __fastcall TProbability_analysis_form::FormClose(TObject *Sender,
      TCloseAction &Action)
   {
   TAnalysis_form::FormClose(Sender, Action);
   if (ModalResult == mrOk)
      {
      TProbability_analysis* Prob_analysis_ptr = dynamic_cast<TProbability_analysis*> (Analysis_ptr);
      if (Prob_analysis_ptr != NULL)
         Prob_analysis_ptr->Prob_exceedence = Probability_exceedence_radio->Checked;
      }
   }
//---------------------------------------------------------------------------

