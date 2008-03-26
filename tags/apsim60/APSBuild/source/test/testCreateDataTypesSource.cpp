//---------------------------------------------------------------------------
#include <vcl.h>
#include <windows.h>
#pragma hdrstop
#include "TestCreateSource.h"
#include <test\framework\testsuite.h>
//---------------------------------------------------------------------------
// DLL entry point
//---------------------------------------------------------------------------
#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void* lpReserved)
{
   return 1;
}
// ------------------------------------------------------------------
// Exported function for created an instance of this add-in
// ------------------------------------------------------------------
extern "C" TestSuite* _export __stdcall createAddIn()
   {
   TestSuite* suite = new TestSuite("TestCreateSource");
   suite->addTest(TestCreateSource::suite());
   return suite;
   }
// ------------------------------------------------------------------
// Exported function to delete the specified addin.
// ------------------------------------------------------------------
extern "C" void _export __stdcall deleteAddIn(TestSuite* suite)
   {
   // don't delete the suite.  CPPUnit will delete it for us.
   }
   
