//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "TSkin.h"
#include "TSplashForm.h"
#include "Main.h"
#include "about.h"
#include <general\path.h>
#include <general\ini_file.h>

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
   // open skin .ini file
   Path p(Application->ExeName.c_str());
   p.Set_extension(".ini");
   Ini_file Skin;
   Skin.Set_file_name (p.Get_path().c_str());

   // read splash screen
   Skin.Read("skin", "splashscreen", BitmapName);
   if (BitmapName != "" && FileExists(BitmapName.c_str()))
      {
      string backdrop;
      Skin.Read("skin", "Backdrop", backdrop);
      if (backdrop == "on")
         {
         showBackdrop = true;
         }
      else
         {
         SplashForm = new TSplashForm(NULL);
         SplashForm->Image1->Picture->LoadFromFile(BitmapName.c_str());
         Application->ProcessMessages();
         SplashForm->Show();
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
   // open skin .ini file
   Path p(Application->ExeName.c_str());
   p.Set_extension(".ini");
   Ini_file Skin;
   Skin.Set_file_name (p.Get_path().c_str());
   string St;

   // show backdrop if required
   if (showBackdrop)
      MainForm->MDIWallpaper1->Picture->LoadFromFile(BitmapName.c_str());

   // read title
   Skin.Read("skin", "title", St);
   if (St != "")
      {
      Application->Title = St.c_str();
      Application->MainForm->Caption = St.c_str();
      }

   // read icon
   Skin.Read("skin", "icon", St);
   if (St != "" && FileExists(St.c_str()))
      {
      Icon = new TIcon;
      Icon->LoadFromFile(St.c_str());
      Application->Icon->Handle = Icon->Handle;
      }

   // read help about
   Skin.Read("skin", "helpabout", St);
   if (St != "" && FileExists(St.c_str()))
      AboutBox->Image1->Picture->LoadFromFile(St.c_str());

   // read evaluation
   Skin.Read("skin", "evaluation", St);
   if (St != "")
      MainForm->StrHolder1->Strings->Strings[0] = St.c_str();
   else
      {
      delete MainForm->Evaluate1;
      MainForm->Evaluate_button->Visible = false;
      }

   // read help file
   Skin.Read("skin", "helpfile", St);
   if (St != "")
      MainForm->StrHolder1->Strings->Strings[1] = St.c_str();

   // read version
   Skin.Read("skin", "version", St);
   if (St != "")
      AboutBox->VersionLabel->Caption = St.c_str();

   // read SOI
//   Skin.Read("skin", "soi", St);
//   if (St != "")
//      MainForm->SOI_button->Visible = Str_i_Eq(St, "on");
   }
   
