//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USEFORM("TMainForm.cpp", MainForm);
USEFORM("TPageSetupForm.cpp", PageSetupForm);
USEFORM("TLibraryForm.cpp", LibraryForm);
USEFORM("TObjectInspectorForm.cpp", ObjectInspectorForm);
USEFORM("TDataPreviewForm.cpp", DataPreviewForm);
//---------------------------------------------------------------------------
AnsiString commandLine;
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR cmdLine, int)
{
   try
   {
       commandLine = cmdLine;
       Application->Initialize();

       Application->Title = "ApsimReport";
       Application->Icon->Handle = LoadIcon(HInstance, "MAINICON");
       Application->CreateHandle();

       Application->CreateForm(__classid(TMainForm), &MainForm);
       Application->CreateForm(__classid(TPageSetupForm), &PageSetupForm);
       Application->CreateForm(__classid(TLibraryForm), &LibraryForm);
       Application->CreateForm(__classid(TObjectInspectorForm), &ObjectInspectorForm);
       Application->CreateForm(__classid(TDataPreviewForm), &DataPreviewForm);
       Application->Run();
   }
   catch (Exception &exception)
   {
       Application->ShowException(&exception);
   }
   catch (...)
   {
       try
       {
          throw Exception("");
       }
       catch (Exception &exception)
       {
          Application->ShowException(&exception);
       }
   }
   return 0;
}
//---------------------------------------------------------------------------
