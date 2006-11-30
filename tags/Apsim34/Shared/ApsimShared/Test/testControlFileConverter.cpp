//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "testControlFileConverter.h"
using namespace boost::unit_test_framework;

#include "..\ApsimControlFile.h"
#include "..\ControlFileConverter.h"
#include <general\string_functions.h>
#include <general\inifile.h>

//---------------------------------------------------------------------------
// Setup the test environment
//---------------------------------------------------------------------------
void setUpControlFileConverter1(void)
   {
   // write a control file.
   static const char* con = "[apsim.sample_accum]\n"
                            "Module = clock    accum.par [sample]\n"
                            "Module = report   accum.par [standard] accum.par [extras]\n"
                            "Module = report(report2)   accum.par [sample]\n"
                            "Module = met      %apsuite\\apsim\\met\\SAMPLE\\DALBY.MET [weather]\n"
                            "Module = accum    accum.par [sample]\n"
                            "Module = manager  accum.par [sample]\n"
                            "Module = fertiliser\n";

   ofstream out("accum.con");
   out << con;
   out.close();

   // write a par file.
   static const char* par = "[standard.report.parameters]\n"
                            "title = Accum Sample Simulation\n"
                            "screen_output =  on\n"
                            "outputfile =  accum.out /overwrite\n"
                            "summaryfile =  accum.sum\n"

                            "module_names =   clock clock\n"
                            "variable_names =  day year\n"
                            "variable_alias =  -    -\n"
                            "Units =          -     -\n"

                            "[extras.report.parameters]\n"
                            "Module_names =   soilwat2\n"
                            "Variable_names =  sum@sw()\n"
                            "Variable_alias =   yyyy\n"
                            "Units =            -\n"

                            "[sample.report2.parameters]\n"
                            "outputfile = report2.out /overwrite\n"

                            "module_names =   clock clock soilwat2  soiln2\n"
                            "variable_names =  day year sum@sw      avg@no3\n"
                            "variable_alias =  -    -   xxxx        -\n"
                            "Units =          -     -   -           -\n"

                            "[sample.clock.parameters]\n"
                            "! Start and end date of run (day number of year and year)\n"
                            "simulation_start_day =  1\n"
                            "simulation_start_year =  1988\n"
                            "simulation_end_day =  100\n"
                            "simulation_end_year =  1988\n"


                            "[sample.accum.parameters]\n"
                            "! Accumulate rainfall for 5 days.\n"
                            "! We can then use this variable in manager\n"
                            "accum_variables =  rain[3]\n"


                            "[sample.manager.start_of_day]\n"
                            "! tell report module to output when accumulated rainfall is\n"
                            "! greater than 20 mm.\n"

                            "if (rain[3] >= 20) then\n"
                            "   report do_output\n"
                            "endif\n";
   out.open("accum.par");
   out << par;
   out.close();
   }
//---------------------------------------------------------------------------
// Setup the test environment 2
//---------------------------------------------------------------------------
void setUpControlFileConverter2(void)
   {
   // write a control file.
   static const char* con = "[apsim.sample_accum]\n"
                            "Module = test     accum.par [test]\n"
                            "Module = clock    [sample1] [sample2]\n"
                            "Module = report   accum.par [sample]\n"
                            "Module = met      %apsuite\\apsim\\met\\SAMPLE\\DALBY.MET [weather]\n"
                            "Module = accum    accum.par [sample]\n"
                            "Module = manager  accum.par [sample]\n"
                            "Module = fertiliser\n\n"

                            "[sample1.clock.parameters]\n"
                            "simulation_start_day =  1\n"
                            "simulation_start_year =  1988\n"

                            "[sample2.clock.parameters]\n"
                            "simulation_end_day =  100\n"
                            "simulation_end_year =  1988\n";

   ofstream out("accum.con");
   out << con;
   out.close();

   // write a par file.
   static const char* par = "[sample.report.parameters]\n"
                            "title = Accum Sample Simulation\n"
                            "screen_output =  on\n"
                            "outputfile =  accum.out /overwrite\n"
                            "summaryfile =  accum.sum\n"

                            "module_names =   clock clock\n"
                            "variable_names =  day year\n"
                            "variable_alias =  -    -\n"
                            "Units =          -     -\n"

                            "Module_names =   accum\n"
                            "Variable_names =  rain[3]\n"
                            "Variable_alias =   rainfall_over_3_days\n"
                            "Units =            -\n"

                            "[test.test.parameters]\n"
                            "test = on\n"

                            "[sample.accum.parameters]\n"
                            "! Accumulate rainfall for 5 days.\n"
                            "! We can then use this variable in manager\n"
                            "accum_variables =  rain[3]\n"


                            "[sample.manager.start_of_day]\n"
                            "! tell report module to output when accumulated rainfall is\n"
                            "! greater than 20 mm.\n"

                            "if (rain[3] >= 20) then\n"
                            "   report do_output\n"
                            "endif\n";
   out.open("accum.par");
   out << par;
   out.close();
   }

//---------------------------------------------------------------------------
// Tear down the test environment
//---------------------------------------------------------------------------
void tearDownControlFileConverter(void)
   {
   DeleteFile("accum.con");
   DeleteFile("accum.par");
   DeleteFile("conversion.script");
   }
//---------------------------------------------------------------------------
// test the set parameter value functionality
//---------------------------------------------------------------------------
void testSetParameterValue(void)
   {
   setUpControlFileConverter1();
   ofstream out("conversion.script");
   out << "[TestSetParameterValue]\n";
   out << "command=SetParameterValue(clock.simulation_start_day, xxxx)\n";
   out << "command=SetParameterValue(clock.simulation_start_year, clock.simulation_start_day)\n";
   out << "command=SetParameterValue(clock.simulation_end_day, date(clock.simulation_end_day, clock.simulation_end_year))\n";
   out << "command=SetParameterValue(log.logfile, %controlfilenamebase%.log)\n";
   out.close();

   ControlFileConverter converter;
   converter.convert("accum.con", "conversion.script", (TControlFileConverterEvent) NULL);
   ApsimControlFile con("accum.con");
   BOOST_CHECK(con.getParameterValue("apsim.sample_accum", "clock", "simulation_start_day") == "xxxx");
   BOOST_CHECK(con.getParameterValue("apsim.sample_accum", "clock", "simulation_start_year") == "xxxx");
   BOOST_CHECK(con.getParameterValue("apsim.sample_accum", "clock", "simulation_end_day") == "9/4/1988");
   BOOST_CHECK(con.getParameterValue("apsim.sample_accum", "log", "logfile") == "accum.log");
   tearDownControlFileConverter();
   }

//---------------------------------------------------------------------------
// test the rename parameter functionality
//---------------------------------------------------------------------------
void testRenameParameter(void)
   {
   setUpControlFileConverter1();
   ofstream out("conversion.script");
   out << "[TestRenameParameter]\n";
   out << "command=RenameParameter(clock.simulation_start_day, start_date)\n";
   out.close();

   ControlFileConverter converter;
   converter.convert("accum.con", "conversion.script", (TControlFileConverterEvent) NULL);
   ApsimControlFile con("accum.con");
   BOOST_CHECK(con.getParameterValue("apsim.sample_accum", "clock", "start_date") == "1");
   tearDownControlFileConverter();
   }
//---------------------------------------------------------------------------
// test the delete parameter functionality
//---------------------------------------------------------------------------
void testDeleteParameter(void)
   {
   setUpControlFileConverter1();
   ofstream out("conversion.script");
   out << "[TestDeleteParameter]\n";
   out << "command=DeleteParameter(clock.simulation_start_day)\n";
   out << "command=DeleteParameter(report.module_names)\n";
   out.close();

   ControlFileConverter converter;
   converter.convert("accum.con", "conversion.script", (TControlFileConverterEvent) NULL);
   ApsimControlFile con("accum.con");
   BOOST_CHECK(con.getParameterValue("apsim.sample_accum", "clock", "simulation_start_day") == "");
   BOOST_CHECK(con.getParameterValue("apsim.sample_accum", "report", "module_names") == "");
   tearDownControlFileConverter();
   }
//---------------------------------------------------------------------------
// test the delete parameter functionality
//---------------------------------------------------------------------------
void testChangeInstantiation(void)
   {
   setUpControlFileConverter1();
   ofstream out("conversion.script");
   out << "[TestChangeInstantiation]\n";
   out << "command=ChangeInstantiation(clock, sequencer(clock))\n";
   out.close();

   ControlFileConverter converter;
   converter.convert("accum.con", "conversion.script", (TControlFileConverterEvent) NULL);
   ApsimControlFile con("accum.con");
   BOOST_CHECK(con.getParameterValue("apsim.sample_accum", "clock", "simulation_start_day") == "1");
   tearDownControlFileConverter();
   }
//---------------------------------------------------------------------------
// test the RemoveReportOutputSwitch functionality
//---------------------------------------------------------------------------
void testRemoveReportOutputSwitch(void)
   {
   setUpControlFileConverter1();
   ofstream out("conversion.script");
   out << "[TestRemoveReportOutputSwitch]\n";
   out << "command=RemoveReportOutputSwitch(outputfile)\n";
   out << "command=RemoveReportOutputSwitch(summaryfile)\n";
   out.close();

   ControlFileConverter converter;
   converter.convert("accum.con", "conversion.script", (TControlFileConverterEvent) NULL);
   ApsimControlFile con("accum.con");
   BOOST_CHECK(con.getParameterValue("apsim.sample_accum", "report", "outputfile") == "accum.out");
   BOOST_CHECK(con.getParameterValue("apsim.sample_accum", "report", "summaryfile") == "accum.sum");
   tearDownControlFileConverter();
   }
//---------------------------------------------------------------------------
// test the RemoveReportOutputSwitch functionality
//---------------------------------------------------------------------------
void testMoveParameter(void)
   {
   setUpControlFileConverter1();
   ofstream out("conversion.script");
   out << "[TestMoveParameter]\n";
   out << "command=MoveParameter(report.title,)\n";
   out << "command=MoveParameter(report.summaryfile, SummaryFile)\n";
   out.close();

   ControlFileConverter converter;
   converter.convert("accum.con", "conversion.script", (TControlFileConverterEvent) NULL);
   ApsimControlFile con("accum.con");
   BOOST_CHECK(con.getTitle("apsim.sample_accum") == "Accum Sample Simulation");
   BOOST_CHECK(con.getParameterValue("apsim.sample_accum", "SummaryFile", "summaryfile") == "accum.sum");
   tearDownControlFileConverter();
   }
//---------------------------------------------------------------------------
// test the NewFormatReportVariables functionality
//---------------------------------------------------------------------------
void testNewFormatReportVariables(void)
   {
   setUpControlFileConverter1();
   ofstream out("conversion.script");
   out << "[testNewFormatReportVariables]\n";
   out << "command=NewFormatReportVariables()\n";
   out.close();

   ControlFileConverter converter;
   converter.convert("accum.con", "conversion.script", (TControlFileConverterEvent) NULL);
   ApsimControlFile con("accum.con");
   vector<string> variables;
   con.getParameterValues("apsim.sample_accum", "report", "variable", variables);
   BOOST_CHECK(variables.size() == 3);
   BOOST_CHECK(variables[0] == "clock.day");
   BOOST_CHECK(variables[1] == "clock.year");
   BOOST_CHECK(variables[2] == "soilwat2.sum@sw() as yyyy");

   vector<string> variables2;
   con.getParameterValues("apsim.sample_accum", "report2", "variable", variables2);
   BOOST_CHECK(variables2.size() == 4);
   BOOST_CHECK(variables2[0] == "clock.day");
   BOOST_CHECK(variables2[1] == "clock.year");
   tearDownControlFileConverter();
   }
//---------------------------------------------------------------------------
// test moving parameters from the control file to a par file
//---------------------------------------------------------------------------
void testMoveParamsFromConToPar(void)
   {
   setUpControlFileConverter2();

   ofstream out("conversion.script");
   out << "[testMoveParametersOutOfCon]\n";
   out << "command=MoveParametersOutOfCon(accum.par)\n";
   out.close();

   ControlFileConverter converter;
   converter.convert("accum.con", "conversion.script", (TControlFileConverterEvent) NULL);

   ApsimControlFile con("accum.con");
   BOOST_CHECK(con.getParameterValue("apsim.sample_accum", "clock", "simulation_start_day") == "1");
   BOOST_CHECK(con.getParameterValue("apsim.sample_accum", "clock", "simulation_end_day") == "100");

   // make sure the conversion hasn't removed the %apsuite macros.
   IniFile controlAsIni("accum.con");
   vector<string> lines;
   controlAsIni.read("apsim.sample_accum", "module", lines);
   BOOST_CHECK(lines.size() == 7);
   BOOST_CHECK(lines[3] == "met      %apsuite\\apsim\\met\\SAMPLE\\DALBY.MET [weather]");

   // make sure test = on is still there.
   BOOST_CHECK(con.getParameterValue("apsim.sample_accum", "test", "test") == "on");
   tearDownControlFileConverter();
   }
//---------------------------------------------------------------------------
// test the RemoveSumAvgToTracker functionality
//---------------------------------------------------------------------------
void testRemoveSumAvgToTracker(void)
   {
   setUpControlFileConverter1();
   ofstream out("conversion.script");
   out << "[testNewFormatReportVariables]\n";
   out << "command=NewFormatReportVariables()\n";
   out << "command=RemoveSumAvgToTracker()\n";
   out.close();

   ControlFileConverter converter;
   converter.convert("accum.con", "conversion.script", (TControlFileConverterEvent) NULL);
   ApsimControlFile con("accum.con");
   vector<string> variables;

   vector<string> variables1;
   con.getParameterValues("apsim.sample_accum", "report", "variable", variables1);
   BOOST_CHECK(variables1.size() == 3);
   BOOST_CHECK(variables1[0] == "clock.day");
   BOOST_CHECK(variables1[1] == "clock.year");
   BOOST_CHECK(variables1[2] == "tracker1.sum@soilwat2.sw[] as yyyy");

   vector<string> variables2;
   con.getParameterValues("apsim.sample_accum", "report2", "variable", variables2);
   BOOST_CHECK(variables2.size() == 4);
   BOOST_CHECK(variables2[0] == "clock.day");
   BOOST_CHECK(variables2[1] == "clock.year");
   BOOST_CHECK(variables2[2] == "tracker2.sum@soilwat2.sw as xxxx");
   BOOST_CHECK(variables2[3] == "tracker2.avg@soiln2.no3");

   vector<string> variables3;
   con.getParameterValues("apsim.sample_accum", "tracker1", "variable", variables3);
   BOOST_CHECK(variables3.size() == 1);
   BOOST_CHECK(variables3[0] == "sum of soilwat2.sw() since report.reported as sum@soilwat2.sw[] on post");

   vector<string> variables4;
   con.getParameterValues("apsim.sample_accum", "tracker2", "variable", variables4);
   BOOST_CHECK(variables4.size() == 2);
   BOOST_CHECK(variables4[0] == "sum of soilwat2.sw since report2.reported as sum@soilwat2.sw on post");
   BOOST_CHECK(variables4[1] == "average of soiln2.no3 since report2.reported as avg@soiln2.no3 on post");
   tearDownControlFileConverter();
   }

//---------------------------------------------------------------------------
// Perform all tests.
//---------------------------------------------------------------------------
test_suite* testControlFileConverter(void)
   {
   test_suite* test= BOOST_TEST_SUITE("TestControlFileConverter");
   test->add(BOOST_TEST_CASE(&testSetParameterValue));
   test->add(BOOST_TEST_CASE(&testRenameParameter));
   test->add(BOOST_TEST_CASE(&testDeleteParameter));
   test->add(BOOST_TEST_CASE(&testChangeInstantiation));
   test->add(BOOST_TEST_CASE(&testRemoveReportOutputSwitch));
   test->add(BOOST_TEST_CASE(&testMoveParameter));
   test->add(BOOST_TEST_CASE(&testNewFormatReportVariables));
   test->add(BOOST_TEST_CASE(&testMoveParamsFromConToPar));
   test->add(BOOST_TEST_CASE(&testRemoveSumAvgToTracker));
   return test;
   }
