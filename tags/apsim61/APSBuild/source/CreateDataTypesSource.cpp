//---------------------------------------------------------------------------
#include <stdio.h>
#include <string>
#include <vector>
#include <fstream>
#include <sstream>
#include <stdexcept>
#include <general/string_functions.h>
#include <ApsimShared/ApsimSettings.h>

#include "CreateSource.h"
#include "CreateDataTypesF90.h"
using namespace std;
//---------------------------------------------------------------------------
// Main entry point into exe
//---------------------------------------------------------------------------
int main(int argc, char* argv[])
{
   try
      {
      bool writeXML = false;
      if (argc != 3 && argc != 4)
         throw runtime_error("Usage: \n   CreateDataTypesSource datatypes.interface macro.txt [/writexml]");

      if (argc==4)
        {
        writeXML= Str_i_Eq(argv[3], "/writexml");
        }
      string dataTypesInterfaceFile = argv[1];
      string dataTypesMacroFile = argv[2];

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

//      CreateDataTypesF90 f90;
//      f90.convert(dataTypesContents.str(), cout);
      }
   catch (const runtime_error& err)
      {
      cerr << "Error:" << err.what() << endl;
      return 1;
      }
   return 0;
   }
//---------------------------------------------------------------------------
