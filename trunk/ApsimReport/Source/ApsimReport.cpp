//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USEFORM("TMainForm.cpp", MainForm);
USEFORM("TRenameFileForm.cpp", RenameFileForm);
USEFORM("TPageSetupForm.cpp", PageSetupForm);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
   try
   {
       Application->Initialize();

       Application->Title = "ApsimReport";
       Application->Icon->Handle = LoadIcon(HInstance, "MAINICON");
       Application->CreateHandle();

       Application->CreateForm(__classid(TMainForm), &MainForm);
       Application->CreateForm(__classid(TRenameFileForm), &RenameFileForm);
       Application->CreateForm(__classid(TPageSetupForm), &PageSetupForm);
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
