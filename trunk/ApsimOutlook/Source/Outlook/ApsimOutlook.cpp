//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TOutlookSplashForm.h"
#include "TSkin.h"
USEFORM("about.cpp", AboutBox);
USEFORM("childwin.cpp", MDIChild);
USEFORM("main.cpp", MainForm);
USEFORM("TChartSettingsForm.cpp", ChartSettingsForm);
USEFORM("TDrill_down_form.cpp", Drill_down_form);
USEFORM("TPreferences_form.cpp", Preferences_form);
USEFORM("TTabRenameForm.cpp", TabRenameForm);
USEFORM("TValueSelectionForm.cpp", ValueSelectionForm);
USEFORM("POPUPFORMUNIT.cpp", PopupForm);
USEFORM("TValueSelectPopup.cpp", ValueSelectPopup);
USEFORM("TOutlookSplashForm.cpp", OutlookSplashForm);
//---------------------------------------------------------------------------
AnsiString CommandLine;
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR cmdline, int)
   {
   CommandLine = cmdline;
                                             
   Skin = new TSkin;
   Skin->DisplaySplashScreen();

   try
   {
      Application->Initialize();
      Application->Title = "APSIM Outlook";
      Application->Icon->Handle = LoadIcon(HInstance, "MAINICON");
      Application->CreateHandle();

      Application->CreateForm(__classid(TMainForm), &MainForm);
       Application->CreateForm(__classid(TAboutBox), &AboutBox);
       Application->CreateForm(__classid(TDrill_down_form), &Drill_down_form);
       Application->CreateForm(__classid(TPreferences_form), &Preferences_form);
       Application->CreateForm(__classid(TTabRenameForm), &TabRenameForm);
       Application->CreateForm(__classid(TOutlookSplashForm), &OutlookSplashForm);
       Application->Run();
   }
   catch (Exception &exception)
   {
      Application->ShowException(&exception);
   }

   delete Skin;

   return 0;
}
//---------------------------------------------------------------------------
