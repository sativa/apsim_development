//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("ApsimOutlook.res");
USEFORM("TPreferences_form.cpp", Preferences_form);
USEFORM("ChildWin.cpp", MDIChild);
USEFORM("Main.cpp", MainForm);
USEFORM("TDirectory_select_form.cpp", Directory_select_form);
USEFORM("TDrill_down_form.cpp", Drill_down_form);
USEFORM("about.cpp", AboutBox);
USELIB("..\..\..\shared\general\general.lib");
USEFORM("TValueSelectionForm.cpp", ValueSelectionForm);
USEFORM("TChartSettingsForm.cpp", ChartSettingsForm);
//---------------------------------------------------------------------------
AnsiString CommandLine;
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR cmdline, int)
   {
   CommandLine = cmdline;
   try
   {
      Application->Initialize();
      Application->Title = "APSIM Outlook";
      Application->Icon->Handle = LoadIcon(HInstance, "MAINICON");
      Application->CreateForm(__classid(TMainForm), &MainForm);
      Application->CreateForm(__classid(TPreferences_form), &Preferences_form);
      Application->CreateForm(__classid(TDirectory_select_form), &Directory_select_form);
      Application->CreateForm(__classid(TDrill_down_form), &Drill_down_form);
      Application->CreateForm(__classid(TAboutBox), &AboutBox);
      Application->CreateForm(__classid(TValueSelectionForm), &ValueSelectionForm);
      Application->Run();
   }
   catch (Exception &exception)
   {
      Application->ShowException(&exception);
   }
   return 0;
}
//---------------------------------------------------------------------------
