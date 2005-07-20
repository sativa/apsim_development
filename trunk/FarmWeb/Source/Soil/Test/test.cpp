//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include <boost/test/unit_test.hpp>
#include "TestSoil.h"
#include "TestSoilSample.h"
using namespace boost::unit_test_framework;

test_suite*
init_unit_test_suite( int argc, char* argv[] )
   {
   test_suite* test= BOOST_TEST_SUITE("TestSoil");
   test->add(testSoil());
   test->add(testSoilSample());

   return test;
   }

