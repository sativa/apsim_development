//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
#include "TSplashForm.h"
#include "TSkin.h"
USERES("ApsimOutlook.res");
USEFORM("TPreferences_form.cpp", Preferences_form);
USEFORM("ChildWin.cpp", MDIChild);
USEFORM("Main.cpp", MainForm);
USEFORM("TDirectory_select_form.cpp", Directory_select_form);
USEFORM("TDrill_down_form.cpp", Drill_down_form);
USEFORM("about.cpp", AboutBox);
USELIB("general.lib");
USEFORM("TValueSelectionForm.cpp", ValueSelectionForm);
USEFORM("TChartSettingsForm.cpp", ChartSettingsForm);
USEFORM("TSplashForm.cpp", SplashForm);
USEFORM("TTabRenameForm.cpp", TabRenameForm);
USEUNIT("TSkin.cpp");
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
      Application->CreateForm(__classid(TPreferences_form), &Preferences_form);
      Application->CreateForm(__classid(TDirectory_select_form), &Directory_select_form);
      Application->CreateForm(__classid(TDrill_down_form), &Drill_down_form);
      Application->CreateForm(__classid(TAboutBox), &AboutBox);
      Application->CreateForm(__classid(TValueSelectionForm), &ValueSelectionForm);
      Application->CreateForm(__classid(TTabRenameForm), &TabRenameForm);
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
