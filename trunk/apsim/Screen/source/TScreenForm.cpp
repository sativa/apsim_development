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
__fastcall TScreenForm::TScreenForm(HWND handle)
   : TForm(handle)
{
}
//---------------------------------------------------------------------------
void TScreenForm::setup(void)
   {
   // restore position on screen
   ApsimSettings settings;
   string st;
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
   settings.read("apsim|PauseOnComplete", st);
   PauseCheckBox->Checked = !Str_i_Eq(st, "no");
   }
//---------------------------------------------------------------------------
void TScreenForm::addLine(const string& line)
   {
   static const int MAX_LINES = 100;
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
   ApsimSettings settings;
   string moreToGo;
   settings.read("apsim|MoreRunsToGo", moreToGo);
   string quiet;
   settings.read("apsim|Quiet", quiet);
   if (moreToGo == "true" || quiet == "true")
      settings.write("apsim|NextWasClicked", "true");
   else
      {
      CancelButton->Caption = "Close";
      CancelButton->Default = true;
      CancelButton->SetFocus();
      settings.write("apsim|NextWasClicked", "false");
      }

   PauseButton->Enabled = false;
   FinishedLabel->Visible = true;
   }
//---------------------------------------------------------------------------
void __fastcall TScreenForm::FormClose(TObject *Sender, TCloseAction &Action)
   {
   CancelButtonClick(NULL);
   }
//---------------------------------------------------------------------------
void __fastcall TScreenForm::CancelButtonClick(TObject *Sender)
   {
   ApsimSettings settings;
   settings.write("apsim|left", Left);
   settings.write("apsim|top", Top);
   if (WindowState == wsMinimized)
      settings.write("apsim|minimize", "yes");
   else
      settings.write("apsim|minimize", "no");

   settings.write("apsim|MoreRunsToGo", "false");

   // Post a message to parent that we have finished.
   PostMessage(Handle, WM_CLOSE_CLICKED_MSG, 0, 0);
   }
//---------------------------------------------------------------------------
void __fastcall TScreenForm::PauseButtonClick(TObject *Sender)
   {
   static bool pause = false;
   pause = !pause;
   if (pause)
      {
      PauseButton->Caption = "Resume";
      CancelButton->Enabled = false;
      while (ScreenForm->Visible && pause)
         Application->ProcessMessages();
      CancelButton->Enabled = true;
      }
   else
      PauseButton->Caption = "Pause";

   }
//---------------------------------------------------------------------------
void __fastcall TScreenForm::PauseCheckBoxClick(TObject *Sender)
   {
   ApsimSettings settings;
   if (PauseCheckBox->Checked)
      settings.write("Apsim|PauseOnComplete", "yes");
   else
      settings.write("Apsim|PauseOnComplete", "no");
   }
//---------------------------------------------------------------------------

