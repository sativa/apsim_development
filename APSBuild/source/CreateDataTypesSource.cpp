//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop
#include "CreateSource.h"
#include <ApsimShared\ApsimDirectories.h>
#include <fstream>
using namespace std;
//---------------------------------------------------------------------------
// Main entry point into exe
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR fileName, int)
   {
   string apsimDirectory = getApsimDirectory();

   ifstream dataTypesInterface(string(apsimDirectory + "\\apsim\\infra\\datatypes.interface").c_str());
   ostringstream ddml;
   ddml << dataTypesInterface.rdbuf();

   ofstream cpp(string(apsimDirectory + "\\shared\\componentInterface\\dataTypes.cpp").c_str());
   ofstream hpp(string(apsimDirectory + "\\shared\\componentInterface\\dataTypes.h").c_str());
   ofstream forDataTypes(string(apsimDirectory + "\\apsim\\infra\\source\\dataTypes.f90").c_str());
   ofstream forDataTypesInterface(string(apsimDirectory + "\\apsim\\infra\\source\\dataTypesInterface.f90").c_str());

   CreateSource createSource;
   createSource.go(ddml.str(), cpp, hpp, forDataTypes, forDataTypesInterface);
   return 0;
   }
//---------------------------------------------------------------------------
