//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TestApsimControlFile.h"

#include <test\framework\testsuite.h>
#include <test\framework\testcaller.h>
#include <fstream>
#include <general\string_functions.h>
#include <ApsimShared\ApsimControlFile.h>
#include <ApsimShared\ApsimDirectories.h>

#pragma package(smart_init)

//---------------------------------------------------------------------------
// Perform all tests.
//---------------------------------------------------------------------------
Test* TestApsimControlFile::suite(void)
   {
   TestSuite *testSuite = new TestSuite ("ApsimControlFile");
   testSuite->addTest(new TestCaller <TestApsimControlFile>
      ("testGetAllSectionNames", &TestApsimControlFile::testGetAllSectionNames));
   testSuite->addTest(new TestCaller <TestApsimControlFile>
      ("testGetAllFiles", &TestApsimControlFile::testGetAllFiles));
   testSuite->addTest(new TestCaller <TestApsimControlFile>
      ("testGetOutputFileNames", &TestApsimControlFile::testGetOutputFileNames));
   testSuite->addTest(new TestCaller <TestApsimControlFile>
      ("testGetSummaryFileNames", &TestApsimControlFile::testGetSummaryFileNames));
   testSuite->addTest(new TestCaller <TestApsimControlFile>
      ("testGetParameterValues", &TestApsimControlFile::testGetParameterValues));
   testSuite->addTest(new TestCaller <TestApsimControlFile>
      ("testSetParameterValues", &TestApsimControlFile::testSetParameterValues));
   testSuite->addTest(new TestCaller <TestApsimControlFile>
      ("testChangeModuleName", &TestApsimControlFile::testChangeModuleName));
   testSuite->addTest(new TestCaller <TestApsimControlFile>
      ("testTitle", &TestApsimControlFile::testTitle));
   testSuite->addTest(new TestCaller <TestApsimControlFile>
      ("testGetInstances", &TestApsimControlFile::testGetInstances));
   testSuite->addTest(new TestCaller <TestApsimControlFile>
      ("testRenameParameter", &TestApsimControlFile::testRenameParameter));
   testSuite->addTest(new TestCaller <TestApsimControlFile>
      ("testDeleteParameter", &TestApsimControlFile::testDeleteParameter));
   testSuite->addTest(new TestCaller <TestApsimControlFile>
      ("testMoveParameter", &TestApsimControlFile::testMoveParameter));
   testSuite->addTest(new TestCaller <TestApsimControlFile>
      ("testMoveParametersOutOfCon", &TestApsimControlFile::testMoveParametersOutOfCon));
   testSuite->addTest(new TestCaller <TestApsimControlFile>
      ("testGetAllModuleInstances", &TestApsimControlFile::testGetAllModuleInstances));
   testSuite->addTest(new TestCaller <TestApsimControlFile>
      ("testGetFileForModule", &TestApsimControlFile::testGetFileForModule));

   return testSuite;
   }
//---------------------------------------------------------------------------
// Setup the test environment
//---------------------------------------------------------------------------
void TestApsimControlFile::setUp(void)
   {
   // write a control file.
   static const char* conSt = "Version = 3.0\n"
                              "[Section1]\n"
                              "Title = test\n"
                              "module = clock    accum.par [sample]\n"
                              "module = report(rep1)   accum.par [sample]\n"
                              "module = report(rep2)   accum.par [sample]\n"
                              "module = met      %apsuite\\apsim\\met\\SAMPLE\\DALBY.MET [weather]\n"
                              "module = accum    accum.par [sample]\n"
                              "module = manager  accum.par [sample]\n"
                              "module = fertiliser\n"
                              "module = summaryfile  accum.par [sample]\n"
                              "[Section2]\n"
                              "Title = test2\n"
                              "module = clock    accum.par [sample]\n"
                              "module = report(rep1)   accum.par [sample]\n"
                              "module = met      %apsuite\\apsim\\met\\SAMPLE\\DALBY.MET [weather]\n"
                              "module = accum    accum.par [sample]\n"
                              "module = manager  accum.par [sample]\n"
                              "module = fertiliser\n"
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
   con = new ApsimControlFile("accum.con");
   }
//---------------------------------------------------------------------------
// Tear down the test environment
//---------------------------------------------------------------------------
void TestApsimControlFile::tearDown(void)
   {
   delete con;
   DeleteFile("accum.con");
   DeleteFile("accum.par");
   }
//---------------------------------------------------------------------------
// test GetAllSectionNames method
//---------------------------------------------------------------------------
void TestApsimControlFile::testGetAllSectionNames(void)
   {
   try
      {
      vector<string> sectionNames;
      con->getAllSectionNames(sectionNames);
      test(sectionNames.size() == 2);
      test(sectionNames[0] == "Section1");
      test(sectionNames[1] == "Section2");
      }
   catch (const runtime_error& error)
      {
      test(false);
      }
   }
//---------------------------------------------------------------------------
// test the GetAllFiles method.
//---------------------------------------------------------------------------
void TestApsimControlFile::testGetAllFiles(void)
   {
   try
      {
      vector<string> fileNames;
      con->getAllFiles("Section1", fileNames);
      test(fileNames.size() == 2);
      test(fileNames[0] == "accum.par");
      test(fileNames[1] == getApsimDirectory() + "\\apsim\\met\\SAMPLE\\DALBY.MET");
      }
   catch (const runtime_error& error)
      {
      test(false);
      }
   }
//---------------------------------------------------------------------------
// test the getOutputFileNames method.
//---------------------------------------------------------------------------
void TestApsimControlFile::testGetOutputFileNames(void)
   {
   try
      {
      vector<string> fileNames;
      con->getOutputFileNames("Section1", fileNames);
      test(fileNames.size() == 2);
      test(fileNames[0] == "out1.out");
      test(fileNames[1] == "out2.out");
      }
   catch (const runtime_error& error)
      {
      test(false);
      }
   }
//---------------------------------------------------------------------------
// test the getSummaryFileNames method.
//---------------------------------------------------------------------------
void TestApsimControlFile::testGetSummaryFileNames(void)
   {
   try
      {
      test(con->getSummaryFileName("Section1") == "accum.sum");
      }
   catch (const runtime_error& error)
      {
      test(false);
      }
   }
//---------------------------------------------------------------------------
// test the getParameterValues method.
//---------------------------------------------------------------------------
void TestApsimControlFile::testGetParameterValues(void)
   {
   try
      {
      test(con->getParameterValue("section2", "clock", "start_date") == "1/1/1988");
      vector<string> variables;
      con->getParameterValues("section1", "rep1", "variable", variables);
      test(variables.size() == 2);
      test(variables[0] == "clock.day");
      test(variables[1] == "clock.year");
      }
   catch (const runtime_error& error)
      {
      test(false);
      }
   }

//---------------------------------------------------------------------------
// test the setParameterValues method.
//---------------------------------------------------------------------------
void TestApsimControlFile::testSetParameterValues(void)
   {
   try
      {
      // change a parameter that already exists.
      con->setParameterValue("Section1", "clock", "start_date", "xxxx");
      test(con->getParameterValue("Section1", "clock", "start_date") == "xxxx");

      // change a parameter that doesn't already exist.
      con->setParameterValue("Section1", "clock", "newParam", "xxxx");
      test(con->getParameterValue("Section1", "clock", "newParam") == "xxxx");

      // change a parameter that doesn't exist and there is no par file
      // specified in control file.
      con->setParameterValue("Section1", "fertiliser", "fertParam", "1.0");
      test(con->getParameterValue("Section1", "fertiliser", "fertParam") == "1.0");

      // make sure the format of the control file is ok.
      static const char* conSt = "Version = 3.0\n"
                                 "[Section1]\n"
                                 "Title = test\n"
                                 "module = clock    accum.par [sample]\n"
                                 "module = report(rep1)   accum.par [sample]\n"
                                 "module = report(rep2)   accum.par [sample]\n"
                                 "module = met      %apsuite\\apsim\\met\\SAMPLE\\DALBY.MET [weather]\n"
                                 "module = accum    accum.par [sample]\n"
                                 "module = manager  accum.par [sample]\n"
                                 "module = fertiliser   accum.par [sample]\n"
                                 "module = summaryfile  accum.par [sample]\n"
                                 "[Section2]\n"
                                 "Title = test2\n"
                                 "module = clock    accum.par [sample]\n"
                                 "module = report(rep1)   accum.par [sample]\n"
                                 "module = met      %apsuite\\apsim\\met\\SAMPLE\\DALBY.MET [weather]\n"
                                 "module = accum    accum.par [sample]\n"
                                 "module = manager  accum.par [sample]\n"
                                 "module = fertiliser\n"
                                 "module = summaryfile  accum.par [sample]\n";
      ostringstream conContents;
      ifstream inCon(con->getFileName().c_str());
      conContents << inCon.rdbuf();
      test(conContents.str() == conSt);

      // make sure the format of the par file is ok.
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
                                 "start_date = xxxx\n"
                                 "end_date   = 31/1/1988\n"
                                 "newParam = xxxx\n"

                                 "[sample.accum.parameters]\n"
                                 "! Accumulate rainfall for 5 days.\n"
                                 "! We can then use this variable in manager\n"
                                 "accum_variables =  rain[3]\n"

                                 "[sample.manager.start_of_day]\n"
                                 "! tell report module to output when accumulated rainfall is\n"
                                 "! greater than 20 mm.\n"

                                 "if (rain[3] >= 20) then\n"
                                 "   rep1 do_output\n"
                                 "endif\n"
                                 "\n"
                                 "[sample.fertiliser.parameters]\n"
                                 "fertParam = 1.0\n";
      ostringstream parContents;
      ifstream inPar("accum.par");
      parContents << inPar.rdbuf();
      test(parContents.str() == parSt);

      }
   catch (const runtime_error& error)
      {
      test(false);
      }
   }
//---------------------------------------------------------------------------
// test the changeModuleName method.
//---------------------------------------------------------------------------
void TestApsimControlFile::testChangeModuleName(void)
   {
   try
      {
      // change a parameter that already exists.
      con->changeModuleName("Section2", "clock", "timeServer(clock)");

      // make sure the format of the control file is ok.
      static const char* conSt = "Version = 3.0\n"
                                 "[Section1]\n"
                                 "Title = test\n"
                                 "module = clock    accum.par [sample]\n"
                                 "module = report(rep1)   accum.par [sample]\n"
                                 "module = report(rep2)   accum.par [sample]\n"
                                 "module = met      %apsuite\\apsim\\met\\SAMPLE\\DALBY.MET [weather]\n"
                                 "module = accum    accum.par [sample]\n"
                                 "module = manager  accum.par [sample]\n"
                                 "module = fertiliser\n"
                                 "module = summaryfile  accum.par [sample]\n"
                                 "[Section2]\n"
                                 "Title = test2\n"
                                 "module = timeServer(clock)    accum.par [sample]\n"
                                 "module = report(rep1)   accum.par [sample]\n"
                                 "module = met      %apsuite\\apsim\\met\\SAMPLE\\DALBY.MET [weather]\n"
                                 "module = accum    accum.par [sample]\n"
                                 "module = manager  accum.par [sample]\n"
                                 "module = fertiliser\n"
                                 "module = summaryfile  accum.par [sample]\n";
      ostringstream conContents;
      ifstream inCon(con->getFileName().c_str());
      conContents << inCon.rdbuf();
      test(conContents.str() == conSt);

      }
   catch (const runtime_error& error)
      {
      test(false);
      }
   }
//---------------------------------------------------------------------------
// test the title methods
//---------------------------------------------------------------------------
void TestApsimControlFile::testTitle(void)
   {
   try
      {
      test(con->getTitle("Section2") == "test2");
      }
   catch (const runtime_error& error)
      {
      test(false);
      }
   }
//---------------------------------------------------------------------------
// test the getInstances methods
//---------------------------------------------------------------------------
void TestApsimControlFile::testGetInstances(void)
   {
   try
      {
      vector<string> instances;
      con->getInstances("Section1", "report", instances);
      test(instances.size() == 2);
      test(instances[0] == "rep1");
      test(instances[1] == "rep2");
      }
   catch (const runtime_error& error)
      {
      test(false);
      }
   }
//---------------------------------------------------------------------------
// test the renameParameter method
//---------------------------------------------------------------------------
void TestApsimControlFile::testRenameParameter(void)
   {
   try
      {
      con->renameParameter("Section1", "report", "variable", "newVar");

      // make sure the format of the par file is ok.
      static const char* parSt = "[sample.rep1.parameters]\n"
                                 "outputfile =  out1.out\n"
                                 "newVar = clock.day\n"
                                 "newVar = clock.year\n"

                                 "[sample.rep2.parameters]\n"
                                 "outputfile =  out2.out\n"
                                 "newVar = accum.rain[3]\n"

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
      ostringstream parContents;
      ifstream inPar("accum.par");
      parContents << inPar.rdbuf();
      test(parContents.str() == parSt);

      }
   catch (const runtime_error& error)
      {
      test(false);
      }
   }
//---------------------------------------------------------------------------
// test the deleteParameter method
//---------------------------------------------------------------------------
void TestApsimControlFile::testDeleteParameter(void)
   {
   try
      {
      con->deleteParameter("Section1", "report", "variable");

      // make sure the format of the par file is ok.
      static const char* parSt = "[sample.rep1.parameters]\n"
                                 "outputfile =  out1.out\n"

                                 "[sample.rep2.parameters]\n"
                                 "outputfile =  out2.out\n"

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
      ostringstream parContents;
      ifstream inPar("accum.par");
      parContents << inPar.rdbuf();
      test(parContents.str() == parSt);

      }
   catch (const runtime_error& error)
      {
      test(false);
      }
   }
//---------------------------------------------------------------------------
// test the moveParameter method
//---------------------------------------------------------------------------
void TestApsimControlFile::testMoveParameter(void)
   {
   try
      {
      con->moveParameter("Section1", "report", "variable", "clock");

      // make sure the format of the par file is ok.
      static const char* parSt = "[sample.rep1.parameters]\n"
                                 "outputfile =  out1.out\n"

                                 "[sample.rep2.parameters]\n"
                                 "outputfile =  out2.out\n"

                                 "[sample.summaryFile.parameters]\n"
                                 "summaryfile =  accum.sum\n"

                                 "[sample.clock.parameters]\n"
                                 "! Start and end date of run (day number of year and year)\n"
                                 "start_date =  1/1/1988\n"
                                 "end_date   = 31/1/1988\n"
                                 "variable = clock.day\n"
                                 "variable = clock.year\n"
                                 "variable = accum.rain[3]\n"

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
      ostringstream parContents;
      ifstream inPar("accum.par");
      parContents << inPar.rdbuf();
      test(parContents.str() == parSt);

      }
   catch (const runtime_error& error)
      {
      test(false);
      }
   }
//---------------------------------------------------------------------------
// test the moveParametersOutOfCon method
//---------------------------------------------------------------------------
void TestApsimControlFile::testMoveParametersOutOfCon(void)
   {
   // write a control file.
   static const char* conSt = "Version = 3.0\n"
                              "[Section1]\n"
                              "Title = test\n"
                              "module = clock    accum.par [sample]\n"
                              "module = report(rep1)  [sample] accum.par [sample]\n"
                              "module = report(rep2)  [sample] accum.par [sample]\n"
                              "module = met      %apsuite\\apsim\\met\\SAMPLE\\DALBY.MET [weather]\n"
                              "module = accum    accum.par [sample]\n"
                              "module = manager  accum.par [sample]\n"
                              "module = fertiliser\n"
                              "module = summaryfile  accum.par [sample]\n"
                              "[Section2]\n"
                              "Title = test2\n"
                              "module = clock    accum.par [sample]\n"
                              "module = report(rep1)   accum.par [sample]\n"
                              "module = met      %apsuite\\apsim\\met\\SAMPLE\\DALBY.MET [weather]\n"
                              "module = accum    accum.par [sample]\n"
                              "module = manager  accum.par [sample]\n"
                              "module = fertiliser\n"
                              "module = summaryfile  accum.par [sample]\n"
                              "\n"
                              "[sample.rep1.parameters]\n"
                              "outputfile =  out1.out\n"
                              "\n"
                              "[sample.rep2.parameters]\n"
                              "outputfile =  out1.out\n";


   ofstream out("accum.con");
   out << conSt;
   out.close();

   // write a par file.
   static const char* parSt = "[sample.rep1.parameters]\n"
                              "variable = clock.day\n"
                              "variable = clock.year\n"

                              "[sample.rep2.parameters]\n"
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
   delete con;
   con = new ApsimControlFile("accum.con");

   try
      {
      test(con->moveParametersOutOfCon("Section1", "default.par"));

      // make sure the format of the con file is ok.
      static const char* newConSt = "Version = 3.0\n"
                                 "[Section1]\n"
                                 "Title = test\n"
                                 "module = clock    accum.par [sample]\n"
                                 "module = report(rep1)   default.par [sample] accum.par [sample]\n"
                                 "module = report(rep2)   default.par [sample] accum.par [sample]\n"
                                 "module = met      %apsuite\\apsim\\met\\SAMPLE\\DALBY.MET [weather]\n"
                                 "module = accum    accum.par [sample]\n"
                                 "module = manager  accum.par [sample]\n"
                                 "module = fertiliser\n"
                                 "module = summaryfile  accum.par [sample]\n"
                                 "[Section2]\n"
                                 "Title = test2\n"
                                 "module = clock    accum.par [sample]\n"
                                 "module = report(rep1)   accum.par [sample]\n"
                                 "module = met      %apsuite\\apsim\\met\\SAMPLE\\DALBY.MET [weather]\n"
                                 "module = accum    accum.par [sample]\n"
                                 "module = manager  accum.par [sample]\n"
                                 "module = fertiliser\n"
                                 "module = summaryfile  accum.par [sample]\n\n";
      ostringstream conContents;
      ifstream inCon("accum.con");
      conContents << inCon.rdbuf();
      test(conContents.str() == newConSt);

      // make sure the format of the par file is ok.
      static const char* parSt = "[sample.rep1.parameters]\n"
                                 "outputfile =  out1.out\n"
                                 "\n"
                                 "[sample.rep2.parameters]\n"
                                 "outputfile =  out1.out\n";
      ostringstream parContents;
      ifstream inPar("default.par");
      parContents << inPar.rdbuf();
      test(parContents.str() == parSt);

      }
   catch (const runtime_error& error)
      {
      test(false);
      }
   }
//---------------------------------------------------------------------------
// test the getAllModuleInstances method
//---------------------------------------------------------------------------
void TestApsimControlFile::testGetAllModuleInstances(void)
   {
   try
      {
      ApsimControlFile::ModuleInstances moduleInstances;
      con->getAllModuleInstances("Section2",moduleInstances);
      test(moduleInstances.size() == 7);
      test(moduleInstances[0].moduleName == "clock");
      test(moduleInstances[0].instanceName == "clock");
      test(moduleInstances[1].moduleName == "report");
      test(moduleInstances[1].instanceName == "rep1");
      test(moduleInstances[2].moduleName == "met");
      test(moduleInstances[2].instanceName == "met");
      test(moduleInstances[3].moduleName == "accum");
      test(moduleInstances[3].instanceName == "accum");
      test(moduleInstances[4].moduleName == "manager");
      test(moduleInstances[4].instanceName == "manager");
      test(moduleInstances[5].moduleName == "fertiliser");
      test(moduleInstances[5].instanceName == "fertiliser");
      test(moduleInstances[6].moduleName == "summaryfile");
      test(moduleInstances[6].instanceName == "summaryfile");
      }
   catch (const runtime_error& error)
      {
      test(false);
      }
   }
//---------------------------------------------------------------------------
// test the  getFileForModule method
//---------------------------------------------------------------------------
void TestApsimControlFile::testGetFileForModule(void)
   {
   try
      {
      test(con->getFileForModule("Section1", "met")
         == getApsimDirectory() + "\\apsim\\met\\SAMPLE\\DALBY.MET");

      }
   catch (const runtime_error& error)
      {
      test(false);
      }
   }


