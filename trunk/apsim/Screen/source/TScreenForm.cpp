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
   string pause;
   settings.read("apsim|pauseOnComplete", pause);
   PauseCheckBox->Checked = (pause == "" || Str_i_Eq(pause, "on"));
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

