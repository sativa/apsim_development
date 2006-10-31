#include <boost/test/unit_test.hpp>
#include "TestConToSim.h"

using namespace boost::unit_test_framework;

test_suite*
init_unit_test_suite( int argc, char* argv[] )
   {
   test_suite* test= BOOST_TEST_SUITE("TestAPSRunTools");
   test->add(TestConToSim());
   return test;
   }

