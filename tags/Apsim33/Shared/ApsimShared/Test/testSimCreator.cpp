//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TestSimCreator.h"

#include <test\framework\testsuite.h>
#include <test\framework\testcaller.h>
#include <fstream>
#include <general\string_functions.h>
#include <ApsimShared\SimCreator.h>

#pragma package(smart_init)

//---------------------------------------------------------------------------
// Perform all tests.
//---------------------------------------------------------------------------
Test* TestSimCreator::suite(void)
   {
   TestSuite *testSuite = new TestSuite ("SimCreator");
   testSuite->addTest(new TestCaller <TestSimCreator>
      ("testCreateSim", &TestSimCreator::testCreateSim));

   return testSuite;
   }
//---------------------------------------------------------------------------
// Setup the test environment
//---------------------------------------------------------------------------
void TestSimCreator::setUp(void)
   {
   // write a control file.
   static const char* conSt = "Version = 3.0\n"
                              "[Section1]\n"
                              "Title = test\n"
                              "module = clock    accum.par [sample]\n"
                              "module = report(rep1)   accum.par [sample]\n"
                              "module = report(rep2)   accum.par [sample]\n"
                              "module = input(met)  %apsuite\\apsim\\met\\SAMPLE\\DALBY.MET [weather]\n"
                              "module = accum    accum.par [sample]\n"
                              "module = manager  accum.par [sample]\n"
                              "module = summaryfile  accum.par [sample]\n"
                              "[Section2]\n"
                              "Title = test2\n"
                              "module = clock    accum.par [sample]\n"
                              "module = report(rep1)   accum.par [sample]\n"
                              "module = input(met)   %apsuite\\apsim\\met\\SAMPLE\\DALBY.MET [weather]\n"
                              "module = accum    accum.par [sample]\n"
                              "module = manager  accum.par [sample]\n"
                              "module = summaryfile  accum.par [sample]\n";


   ofstream out("accum.con");
   out << conSt;
   out.close();

   // write a par file.
   static const char* parSt = "[sample.rep1.parameters]\n"
                              "outputfile =  out1.out\n"
                              "variable = clock.day\n"
                              "variable = clock.year\n"

                              "[sample.rep2.parameters]\n"
                              "outputfile =  out2.out\n"
                              "variable = accum.rain[3]\n"

                              "[sample.summaryFile.parameters]\n"
                              "summaryfile =  accum.sum\n"

                              "[sample.clock.parameters]\n"
                              "! Start and end date of run (day number of year and year)\n"
                              "start_date =  1/1/1988\n"
                              "end_date   = 31/1/1988\n"

                              "[sample.accum.parameters]\n"
                              "! Accumulate rainfall for 5 days.\n"
                              "! We can then use this variable in manager\n"
                              "accum_variables =  rain[3]\n"

                              "[sample.manager.start_of_day]\n"
                              "! tell report module to output when accumulated rainfall is\n"
                              "! greater than 20 mm.\n"

                              "if (rain[3] >= 20) then\n"
                              "   rep1 do_output\n"
                              "endif\n";
   out.open("accum.par");
   out << parSt;
   out.close();

   static const char* configSt =
              "[system]\n"
              "component = %apsuite\\apsim\\protocolmanager\\lib\\protocolmanager.dll\n"
              "component = %apsuite\\apsim\\summaryfile\\lib\\summaryfile.dll\n"
              "sequencer = %apsuite\\apsim\\clock\\lib\\clock.dll\n"
              "component = %apsuite\\apsim\\report\\lib\\report.dll\n"
              "component = %apsuite\\apsim\\input\\lib\\input.dll\n"
              "component = %apsuite\\apsim\\accum\\lib\\accum.dll\n"
              "component = %apsuite\\apsim\\fertiliz\\lib\\fertiliz.dll\n"
              "component = %apsuite\\apsim\\manager\\lib\\manager.dll\n";
   out.open("test.config");
   out << configSt;
   out.close();
   }
//---------------------------------------------------------------------------
// Tear down the test environment
//---------------------------------------------------------------------------
void TestSimCreator::tearDown(void)
   {
   DeleteFile("accum.con");
   DeleteFile("accum.par");
   DeleteFile("test.config");
   DeleteFile("accum1.sim");
   DeleteFile("accum2.sim");
   }
//---------------------------------------------------------------------------
// test createSim method
//---------------------------------------------------------------------------
void TestSimCreator::testCreateSim(void)
   {
   try
      {
      SimCreator simCreate;
      simCreate.createSims("accum.con");
      test(FileExists("accum1.sim"));
      test(FileExists("accum2.sim"));
      }
   catch (const runtime_error& error)
      {
      test(false);
      }
   }

