#include <boost/test/unit_test.hpp>
#include "testXml.h"
#include "testIoFunctions.h"
using namespace boost::unit_test_framework;

test_suite*
init_unit_test_suite( int argc, char* argv[] )
   {
   test_suite* test= BOOST_TEST_SUITE("TestGeneral");
   test->add(testXml());
   test->add(testIoFunctions());

   return test;
   }     
 
