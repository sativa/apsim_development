//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TAboutForm.h"
#include "ApsimLicence.h"
#include "ApsimVersion.h"
#include "ApsimDirectories.h"
#include <general\vcl_functions.h>
#include <shellapi.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "HTMLabel"
#pragma link "HTMLText"
#pragma link "paramlabel"
#pragma resource "*.dfm"
TAboutForm *AboutForm;
//---------------------------------------------------------------------------
__fastcall TAboutForm::TAboutForm(TComponent* Owner)
   : TForm(Owner)
   {
   }
//---------------------------------------------------------------------------
void __fastcall TAboutForm::FormShow(TObject *Sender)
   {
   string indent = "<IND x=\"90\">";
   string lf = "<BR>";
   ApsimLicence key = getApsimKey();
   string details = "<B>Version:</B>" + indent + getApsimVersion() + lf;
   details += "<B>Directory:</B>" + indent + getApsimDirectory() + lf;
   details += key.getAllDetails();
   DetailsLabel->HTMLText->Text = details.c_str();
   }
//---------------------------------------------------------------------------
void __fastcall TAboutForm::WEBLabelClick(TObject *Sender)
   {
   ShellExecute(Handle, "open", WEBLabel->Caption.c_str(), NULL, "", SW_SHOW);
   }
//---------------------------------------------------------------------------
void __fastcall TAboutForm::EmailLabelClick(TObject *Sender)
   {
   AnsiString cmd = "mailto:" + EmailLabel->Caption;
   ShellExecute(Handle, "open", cmd.c_str(), NULL, "", SW_SHOW);
   }
//---------------------------------------------------------------------------
void __fastcall TAboutForm::Button1Click(TObject *Sender)
   {
   string command = getApsimDirectory() + "\\diagnost\\APSshowkey.exe ";
   command += getApsimDirectory() + "\\shared\\aps.dll";
   WinExec (command.c_str(), SW_SHOW);
   }
//---------------------------------------------------------------------------

