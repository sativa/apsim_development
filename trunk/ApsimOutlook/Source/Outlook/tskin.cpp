//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TSkin.h"
#include "TOutlookSplashForm.h"
#include "Main.h"
#include "about.h"
#include <general\path.h>
#include <ApsimShared\ApsimDirectories.h>

#pragma package(smart_init)
TSkin* Skin;

// ------------------------------------------------------------------
//  Short description:
//      constructor

//  Notes:

//  Changes:
//    DPH 9/12/99

// ------------------------------------------------------------------
TSkin::TSkin(void)
   {
   Icon = NULL;
   showBackdrop = false;
   }

// ------------------------------------------------------------------
//  Short description:
//      destructor

//  Notes:

//  Changes:
//    DPH 9/12/99

// ------------------------------------------------------------------
TSkin::~TSkin(void)
   {
   delete Icon;
   }

// ------------------------------------------------------------------
//  Short description:
//      Display a splash screen if necessary.

//  Notes:

//  Changes:
//    DPH 9/12/99
//    DAH 31/5/01:   added ability to show splash as an MDI backdrop

// ------------------------------------------------------------------
void TSkin::DisplaySplashScreen(void)
   {
   // read splash screen
   settings.read("skin|splashscreen", BitmapName);
   BitmapName = getAppHomeDirectory() + "\\" + BitmapName;
   if (BitmapName != "" && FileExists(BitmapName.c_str()))
      {
      string backdrop;
      settings.read("skin|Backdrop", backdrop);
      if (backdrop == "on")
         {
         showBackdrop = true;
         }
      else
         {
         OutlookSplashForm = new TOutlookSplashForm(NULL);
         OutlookSplashForm->Image1->Picture->LoadFromFile(BitmapName.c_str());
         Application->ProcessMessages();
         OutlookSplashForm->Show();
         }
      }
   }
// ------------------------------------------------------------------
//  Short description:
//      Initialisation all application properties.

//  Notes:

//  Changes:
//    DPH 9/12/99

// ------------------------------------------------------------------
void TSkin::InitApplication(void)
   {
   string St;

   // show backdrop if required
   if (showBackdrop)
      MainForm->MDIWallpaper1->Picture->LoadFromFile(BitmapName.c_str());

   // read title
   settings.read("skin|title", St);
   if (St != "")
      {
      Application->Title = St.c_str();
      Application->MainForm->Caption = St.c_str();
      }

   // read icon
   settings.read("skin|icon", St);
   string fileName = getAppHomeDirectory() + "\\" + St;
   if (St != "" && FileExists(fileName.c_str()))
      {
      Icon = new TIcon;
      Icon->LoadFromFile(fileName.c_str());
      Application->Icon->Handle = Icon->Handle;
      }

   // read help about
   settings.read("skin|helpabout", St);
   fileName = getAppHomeDirectory() + "\\" + St;
   if (St != "" && FileExists(fileName.c_str()))
      AboutBox->Image1->Picture->LoadFromFile(fileName.c_str());

   // read evaluation
   settings.read("skin|evaluation", evaluation);
   if (evaluation == "")
      {
      delete MainForm->Evaluate1;
      MainForm->Evaluate_button->Visible = false;
      }

   // read help file
   settings.read("skin|helpfile", helpFile);

   // read version
   settings.read("skin|version", St);
   if (St != "")
      AboutBox->VersionLabel->Caption = St.c_str();

   // read SOI
//   Skin.Read("skin", "soi", St);
//   if (St != "")
//      MainForm->SOI_button->Visible = Str_i_Eq(St, "on");
   }
// ------------------------------------------------------------------
// display the evaluation page.
// ------------------------------------------------------------------
void TSkin::displayEvaluation(void)
   {
   ShellExecute (MainForm->Handle, "open",
                 evaluation.c_str(), NULL, "", SW_SHOW);

   }
// ------------------------------------------------------------------
// display the help page.
// ------------------------------------------------------------------
void TSkin::displayHelp(void)
   {
   helpFile = getAppHomeDirectory() + "\\" + helpFile;
   ShellExecute (MainForm->Handle, "open",
                 helpFile.c_str(), NULL, "", SW_SHOW);
   }

