//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
#include "TSplashForm.h"
#include "TSkin.h"
USERES("ApsimOutlook.res");
USEFORM("about.cpp", AboutBox);
USEFORM("ChildWin.cpp", MDIChild);
USEFORM("Main.cpp", MainForm);
USEFORM("TChartSettingsForm.cpp", ChartSettingsForm);
USEFORM("TDrill_down_form.cpp", Drill_down_form);
USEFORM("TPreferences_form.cpp", Preferences_form);
USEUNIT("TSkin.cpp");
USEFORM("TSplashForm.cpp", SplashForm);
USEFORM("TTabRenameForm.cpp", TabRenameForm);
USEFORM("TValueSelectionForm.cpp", ValueSelectionForm);
USELIB("aps32.lib");
USEUNIT("Scenarios.cpp");
USEUNIT("factor.cpp");
USEUNIT("Scenario.cpp");
USEUNIT("AddIn.cpp");
USE("ApsimOutlook.todo", ToDo);
USEUNIT("ToolBarAddIn.cpp");
USEUNIT("K:\general\string_functions.cpp");
USEUNIT("K:\general\ini_file.cpp");
USEUNIT("K:\general\path.cpp");
USEUNIT("K:\general\stream_functions.cpp");
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
       Application->CreateForm(__classid(TValueSelectionForm), &ValueSelectionForm);
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
