//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop
#include <ApsimShared\ApsimProject.h>
#include <general\path.h>
#include "CreateDataTypesF90.h"

// ------------------------------------------------------------------
// Create a DataTypesModule.f90 file for this module.
// ------------------------------------------------------------------
string getInterfaceFileName(const string& apfFileName)
   {
   static const char* APSBUILD_SECTION = "APSBuild";
   static const char* BINARY_KEY = "binary";

   // need to work out the name of the interface file.
   // It does this by looking at the name of the binary file and
   // assuming the interface file has the same name but with a .interface
   // extension and exists in the parent directory of the apf passed in.
   ApsimProject apf(apfFileName);
   vector<string> binaryFileNames;
   apf.get(BINARY_KEY, binaryFileNames, APSBUILD_SECTION, true);
   if (binaryFileNames.size() > 0)
      {
      Path binaryPath(binaryFileNames.begin()->c_str());
      string moduleName = binaryPath.Get_name_without_ext();

      Path interfaceFilePath(apf.getFileName());
      interfaceFilePath.Back_up_directory();
      interfaceFilePath.Set_name(moduleName.c_str());
      interfaceFilePath.Set_extension(".interface");

      if (interfaceFilePath.Exists())
         return interfaceFilePath.Get_path();
      }
   return "";
   }
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR fileName, int)
   {
   if (_argc == 3)
      {
      CreateDataTypesF90 converter;
      ofstream out(_argv[2], ios::app);
      converter.convert(getInterfaceFileName(_argv[1]), out);
      }
   return 0;
   }
//---------------------------------------------------------------------------
