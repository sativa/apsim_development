//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop
#include "CreateSource.h"
#include <ApsimShared\ApsimSettings.h>
#include <general\string_functions.h>
#include <fstream>
using namespace std;
//---------------------------------------------------------------------------
// Main entry point into exe
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR commandLine, int)
   {
   try
      {
      bool writeXML = Str_i_Eq(commandLine, "/writexml");

      ApsimSettings settings;
      string dataTypesInterfaceFile;
      string dataTypesMacroFile;
      settings.read("APSBuild|DataTypesInterfaceFile", dataTypesInterfaceFile, true);
      settings.read("APSBuild|DataTypesMacroFile", dataTypesMacroFile, true);

      // read contents of datatypes.interface
      ifstream dataTypesInterface(dataTypesInterfaceFile.c_str());
      ostringstream dataTypesContents;
      dataTypesContents << dataTypesInterface.rdbuf();

      // read contents of componentinterface.amf
      ifstream dataTypesMacro(dataTypesMacroFile.c_str());
      ostringstream dataTypesMacroContents;
      dataTypesMacroContents << dataTypesMacro.rdbuf();

      CreateSource createSource;
      createSource.go(dataTypesContents.str(), dataTypesMacroContents.str(), writeXML);
      }
   catch (const runtime_error& err)
      {
      cout << err.what();
      return 1;
      }
   return 0;
   }
//---------------------------------------------------------------------------
