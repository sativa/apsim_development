//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TInfoForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "IWBaseControl"
#pragma link "IWBaseHTMLControl"
#pragma link "IWCompEdit"
#pragma link "IWCompLabel"
#pragma link "IWCompRectangle"
#pragma link "IWControl"
#pragma link "IWVCLBaseControl"
#pragma link "IWExtCtrls"
#pragma link "IWHTMLControls"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
// constructor.
//---------------------------------------------------------------------------
__fastcall TInfoForm::TInfoForm(TComponent* Owner)
        : TIWAppForm(Owner)
   {

   }
//---------------------------------------------------------------------------
// Setup the form
//---------------------------------------------------------------------------
void TInfoForm::setup(AnsiString prompt1,
                      AnsiString prompt2,
                      AnsiString prompt3,
                      AnsiString prompt4,
                      TInfoEvent callback)
   {
   Prompt1->Text = prompt1;
   Prompt2->Text = prompt2;
   Prompt3->Text = prompt3;
   Prompt4->Text = prompt4;

   Prompt2->Visible = (prompt2 != "");
   Edit2->Visible = (prompt2 != "");
   Prompt3->Visible = (prompt3 != "");
   Edit3->Visible = (prompt3 != "");
   Prompt4->Visible = (prompt4 != "");
   Edit4->Visible = (prompt4 != "");

   if (!Edit1-Visible)
      Edit1->Text = "";
   if (!Edit2-Visible)
      Edit2->Text = "";
   if (!Edit3-Visible)
      Edit3->Text = "";
   if (!Edit4-Visible)
      Edit4->Text = "";

   onClickNotify = callback;
   }
//---------------------------------------------------------------------------
// User has clicked back - call callback
//---------------------------------------------------------------------------
void __fastcall TInfoForm::BackButtonClick(TObject *Sender)
   {
   if (onClickNotify != NULL)
      onClickNotify(false, "", "", "", "");
   }
//---------------------------------------------------------------------------
// User has clicked ok - call callback
//---------------------------------------------------------------------------
void __fastcall TInfoForm::OkButtonClick(TObject *Sender)
   {
   if (onClickNotify != NULL)
      onClickNotify(true, Edit1->Text, Edit2->Text, Edit3->Text, Edit4->Text);
   }
//---------------------------------------------------------------------------

