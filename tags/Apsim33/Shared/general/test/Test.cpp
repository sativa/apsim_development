//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include <cppunit\ui\text\TestRunner.h>
#include "TestStringTokenizer.h"
#include "TestIniFile.h"
#include "TestMacro.h"

//#pragma argsused
int main(int argc, char* argv[])
   {
   CppUnit::TextUi::TestRunner runner;
   runner.addTest(TestStringTokenizer::suite());
   runner.addTest(TestIniFile::suite());
   runner.addTest(TestMacro::suite());
   return runner.run();
   }

