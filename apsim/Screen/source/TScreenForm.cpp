//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TScreenForm.h"
#include <ApsimShared\ApsimSettings.h>
#include <general\string_functions.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TScreenForm *ScreenForm;
//---------------------------------------------------------------------------
__fastcall TScreenForm::TScreenForm(TComponent* Owner)
   : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TScreenForm::FormShow(TObject *Sender)
   {
   //
   ApsimSettings settings;
   string st;
   settings.read("apsim|pauseOnComplete", st);
   PauseCheckBox->Checked = (st == "" || Str_i_Eq(st, "on"));

   // restore position on screen
   settings.read("apsim|left", st);
   if (st != "")
      {
      Left = StrToInt(st.c_str());
      settings.read("apsim|top", st);
      Top = StrToInt(st.c_str());
      settings.read("apsim|minimize", st);
      if (st == "yes")
         WindowState = wsMinimized;
      }
   }
//---------------------------------------------------------------------------
void __fastcall TScreenForm::PauseCheckBoxClick(TObject *Sender)
   {
   ApsimSettings settings;
   string pause;
   if (PauseCheckBox->Checked)
      settings.write("apsim|pauseOnComplete", "on");
   else
      settings.write("apsim|pauseOnComplete", "off");
   }
//---------------------------------------------------------------------------
void TScreenForm::addLine(const string& line)
   {
   static const int MAX_LINES = 22;
   if (Memo->Lines->Count == MAX_LINES)
      Memo->Lines->Delete(0);
   Memo->Lines->Add(line.c_str());
   }
//---------------------------------------------------------------------------
void TScreenForm::errorsWereEncountered(void)
   {
   ErrorLabel->Visible = true;
   }
//---------------------------------------------------------------------------
void TScreenForm::simulationHasFinished(void)
   {
   FinishedLabel->Visible = true;
   CloseButton->Caption = "Close";
   }
//---------------------------------------------------------------------------
void __fastcall TScreenForm::FormClose(TObject *Sender, TCloseAction &Action)
   {
   CloseButtonClick(NULL);
   }
//---------------------------------------------------------------------------
void __fastcall TScreenForm::CloseButtonClick(TObject *Sender)
   {
   ApsimSettings settings;
   settings.write("apsim|left", Left);
   settings.write("apsim|top", Top);
   if (WindowState == wsMinimized)
      settings.write("apsim|minimize", "yes");
   else
      settings.write("apsim|minimize", "no");
   }
//---------------------------------------------------------------------------

