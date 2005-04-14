//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TQuestionForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "IWBaseControl"
#pragma link "IWBaseHTMLControl"
#pragma link "IWCompButton"
#pragma link "IWCompLabel"
#pragma link "IWControl"
#pragma link "IWVCLBaseControl"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
__fastcall TQuestionForm::TQuestionForm(TComponent* Owner)
        : TIWAppForm(Owner)
   {
   }
//---------------------------------------------------------------------------
void TQuestionForm::setup(AnsiString prompt, TQuestionEvent callback)
   {
   PromptLabel->Text = prompt;
   OnClickNotify = callback;
   }
//---------------------------------------------------------------------------
void __fastcall TQuestionForm::YesButtonClick(TObject *Sender)
   {
   if (OnClickNotify != NULL)
      OnClickNotify(true);
   }
//---------------------------------------------------------------------------
void __fastcall TQuestionForm::NoButtonClick(TObject *Sender)
   {
   if (OnClickNotify != NULL)
      OnClickNotify(false);
   }
//---------------------------------------------------------------------------
