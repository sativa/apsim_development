//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TOutlookSplashForm.h"
#include <general\path.h>
#include <jpeg.hpp>
#include <ApsimShared\ApsimSettings.h>
#include <ApsimShared\ApsimDirectories.h>

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
   settings.read("Outlook Skin|SplashScreenOk", st);
   showOkPanel = Str_i_Eq(st, "yes");
   if (showOkPanel)
      {
      Timer1->Enabled = false;
      Width = Image1->Width;
      Height = Image1->Height + Panel1->Height;
      vector<string> logos;
      settings.read("Outlook Skin|SplashLogo", logos);
      settings.read("Outlook Skin|LogoGap", logoGap);
      int leftPosn = logoGap;
      int maxHght = Panel1->Height - 2*logoGap;
      for (vector<string>::iterator logo = logos.begin(); logo != logos.end(); logo++)
         {
         TImage* pic = new TImage(Panel1);
         pic->Parent = Panel1;
         pic->AutoSize = true;
         pic->Stretch = true;
         string fileName = getAppHomeDirectory() + "\\" + (*logo);
         pic->Picture->LoadFromFile(fileName.c_str());
         int wdth = pic->Width;
         int hght = pic->Height;
         double quotient = (double)hght/maxHght;
         pic->AutoSize = false;
         pic->Height = maxHght;
         pic->Width = wdth/quotient;
         pic->Top = logoGap;
         pic->Left = leftPosn;
         leftPosn += (logoGap + pic->Width);
         }

//      OkButton->Left = Width / 2 - (OkButton->Width / 2);
//      OkButton->Top = Height - OkButton->Height - 10;
//      OkButton->Visible = true;
      }
   }
//---------------------------------------------------------------------------
void __fastcall TOutlookSplashForm::OkButtonClick(TObject *Sender)
   {
   Close();
   }
//---------------------------------------------------------------------------

void __fastcall TOutlookSplashForm::Button1Click(TObject *Sender)
{
   Application->NormalizeTopMosts();
   Application->MessageBox(AnsiString("ok button is: " + IntToStr(OkButton->Left)).c_str(), "Hello there", MB_OK);
//   Application->RestoreTopMosts();
}
//---------------------------------------------------------------------------

