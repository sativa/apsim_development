//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TSplashForm.h"
#include "ApsimVersion.h"
#include "ApsimDirectories.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "HTMLabel"
#pragma resource "*.dfm"
TSplashForm *SplashForm;
//---------------------------------------------------------------------------
__fastcall TSplashForm::TSplashForm(TComponent* Owner)
   : TForm(Owner)
   {
   }
//---------------------------------------------------------------------------
void __fastcall TSplashForm::Timer1Timer(TObject *Sender)
   {
   Timer1->Enabled = false;
   Close();
   }
//---------------------------------------------------------------------------
void __fastcall TSplashForm::FormShow(TObject *Sender)
   {
   try
      {
      string indent = "<IND x=\"90\">";
      string lf = "<BR>";
      string details = "<B>Version:</B>" + indent + getApsimVersion() + lf;
      details += "<B>Directory:</B>" + indent + getApsimDirectory() + lf;
      DetailsLabel->HTMLText->Text = details.c_str();
      }
   catch (const runtime_error& error)
      {
      Timer1->Enabled = false;
      errorMessage = error.what();
      Close();
      }
   }
//---------------------------------------------------------------------------
void __fastcall TSplashForm::FormClose(TObject *Sender,
      TCloseAction &Action)
   {
   Action = caHide;
   if (errorMessage != "")
      {
      MessageBox(NULL, errorMessage.c_str(), "Error", MB_ICONSTOP | MB_OK);
      exit(1);
      }
   }
//---------------------------------------------------------------------------

