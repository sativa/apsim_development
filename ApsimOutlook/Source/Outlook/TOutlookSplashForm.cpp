//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TOutlookSplashForm.h"
#include <general\path.h>
#include <jpeg.hpp>
#include <ApsimShared\ApsimSettings.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TOutlookSplashForm *OutlookSplashForm;
//---------------------------------------------------------------------------
__fastcall TOutlookSplashForm::TOutlookSplashForm(TComponent* Owner)
   : TForm(Owner)
   {
   // needed to link in jpeg library for some reason.
//	dynamic_cast<TJPEGImage *>(Image1->Picture->Graphic);
   }
//---------------------------------------------------------------------------
void __fastcall TOutlookSplashForm::Timer1Timer(TObject *Sender)
   {
   Timer1->Enabled = false;
   Close();
   }
//---------------------------------------------------------------------------
void __fastcall TOutlookSplashForm::FormShow(TObject *Sender)
   {
   ApsimSettings settings;
   string st;
   settings.read("Skin|SplashScreenOk", st);
   if (Str_i_Eq(st, "yes"))
      {
      Timer1->Enabled = false;
      Width = Image1->Width;
      Height = Image1->Height;
      OkButton->Left = Width / 2 - (OkButton->Width / 2);
      OkButton->Top = Height - OkButton->Height - 10;
      OkButton->Visible = true;
      }
   }
//---------------------------------------------------------------------------
void __fastcall TOutlookSplashForm::OkButtonClick(TObject *Sender)
   {
   Close();
   }
//---------------------------------------------------------------------------

