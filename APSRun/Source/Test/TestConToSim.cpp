//---------------------------------------------------------------------------
#include <stdlib.h>
#include <dir.h>

#include <iostream.h>
#include <fstream.h>
#include <string>
#include "TestConToSim.h"
#include <ApsimShared/SimCreator.h>
#include <ApsimShared/ApsimVersion.h>

#include <boost/test/unit_test.hpp>
using boost::unit_test_framework::test_suite;

void TestAmpersandInTitle()
   // -----------------------------------------------------------------
   // Use case:
   //    The user has put an ampersand in the title of a control file.
   //    Make sure that a sim file can be generated ok.
   // -----------------------------------------------------------------
   {
   ofstream con("test.con");
   con << "version = " << getApsimVersion()  << endl;
   con << "[apsim.wheat_sample]" << endl;
   con << "title=WHEAT & SAMPLE Simulation" << endl;
   con.close();

   SimCreator simCreator(true);
   simCreator.ConToSim("test.con");

   ifstream sim("test.sim");
   ostringstream contentsBuffer;
   contentsBuffer << sim.rdbuf();
   string contents = contentsBuffer.str();
   string expected =  string("<?xml version=\"1.0\"?>\n") + 
      string("<simulation executable=\"%apsuite/apsim/protocolmanager/lib/protocolmanager.dll\" version=\"") + 
      getApsimVersion() + string("\">\n") +
      string("   <title><![CDATA[WHEAT & SAMPLE Simulation]]></title>\n") +
      string("</simulation>\n");

   BOOST_CHECK(contents == expected);


   }


test_suite* TestConToSim(void)
   //---------------------------------------------------------------------------
   // Register all tests.
   //---------------------------------------------------------------------------
   {
   test_suite* test= BOOST_TEST_SUITE("TestConToSim");
   test->add(BOOST_TEST_CASE(&TestAmpersandInTitle));

   return test;
   }

