//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop
#include "GeneratorFromExcel.h"
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
   {
   CoInitialize(NULL);

   try
      {
      if (_argc != 4)
         throw runtime_error("Usage: FileGeneratorFromExcel xlsFileName xlsSheetName scriptFileName");
      GeneratorFromExcel generator;
      generator.go(_argv[1], _argv[2], _argv[3]);
      }
   catch (Exception &exception)
      {
      Application->ShowException(&exception);
      }
   catch (const runtime_error& err)
      {
      MessageBox(NULL, err.what(), "Error", MB_ICONSTOP | MB_OK);
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

   CoUninitialize();
   return 0;
   }
//---------------------------------------------------------------------------
