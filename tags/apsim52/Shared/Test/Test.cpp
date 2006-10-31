//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop
#include <windows.h>

#include <fstream>
using namespace std;
ofstream out;

//---------------------------------------------------------------------------
// DLL Entry point
//---------------------------------------------------------------------------
#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void* lpReserved)
   {
   return 1;
   }
//---------------------------------------------------------------------------
// internal testing routine.
//---------------------------------------------------------------------------
void _export internalTest(bool condition, const char* fileName, unsigned lineNumber)
   {
   if (!out.is_open())
      {
      AnsiString outPath = ExtractFileDir(Application->ExeName) + "\\test.out";
      out.open(outPath.c_str());
      }
   out << fileName << ", Line: " << lineNumber << ' ';
   if (condition)
      out << "Passed";
   else
      out << "Failed";
   out << endl;
   }
//---------------------------------------------------------------------------
// internal testing routine.
//---------------------------------------------------------------------------
extern "C" void _export __stdcall test
   (unsigned* condition, const char* fortranFileName,
    unsigned* lineNumber, unsigned fileNameSize)
   {
   char fileName[1000];
   strncpy(fileName, fortranFileName, fileNameSize);
   fileName[fileNameSize] = 0;
   internalTest(*condition, fileName, *lineNumber);
   }
