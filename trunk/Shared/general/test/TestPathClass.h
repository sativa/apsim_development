// TestPathClass.h - header for TestPathClass.cpp
//
// J Wang
// Aug 2004

#include <iostream.h>
#include <stdlib.h>
#include "../path.h"

//boost
#include <boost/test/unit_test.hpp>
using boost::unit_test_framework::test_suite;

//---------------------------------------------------------------------------
// 1. Create the testing class
//---------------------------------------------------------------------------
class Path_test{
public:
    Path_test(){};
    Path mpath;

    // Test Functions
    void TestIsEmpty();
};

//---------------------------------------------------------------------------
// 2. Register the testing class into test suite
//---------------------------------------------------------------------------
class Path_test_suite : public test_suite{
public:
    Path_test_suite() : test_suite("Path_test_suite"){
    boost::shared_ptr<Path_test> instance(new Path_test);

    add(BOOST_CLASS_TEST_CASE(&Path_test::TestIsEmpty,instance));
    }
};

//---------------------------------------------------------------------------
// 3. Register the test suite onto Boost - see Test.cpp
//---------------------------------------------------------------------------
