//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USEFORM("TRunForm.cpp", RunForm);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
   {
   try
      {
      Application->Initialize();
      Application->CreateForm(__classid(TRunForm), &RunForm);
       Application->Run();
      }
   catch (const std::runtime_error& err)
      {
      ::MessageBox(NULL, err.what(), "Error", MB_ICONSTOP | MB_OK);
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

