//---------------------------------------------------------------------------
#include <stdio.h>
#include <string>
#include <boost/test/unit_test.hpp>
using boost::unit_test_framework::test_suite;

#include "../StringTokenizer.h"

//---------------------------------------------------------------------------
// Perform all tests.
//---------------------------------------------------------------------------
void TestStringTokenizer(void)
   {
   static const char* st1 = "Dean.Holzworth@tag.csiro.au";
   StringTokenizer tokenizer1(st1, ".@", false);
   BOOST_CHECK(tokenizer1.countTokens() == 5);
   BOOST_CHECK(tokenizer1.nextToken() == "Dean");
   BOOST_CHECK(tokenizer1.nextToken() == "Holzworth");
   BOOST_CHECK(tokenizer1.nextToken() == "tag");
   BOOST_CHECK(tokenizer1.nextToken() == "csiro");
   BOOST_CHECK(tokenizer1.nextToken() == "au");
   BOOST_CHECK(tokenizer1.countTokens() == 0);
   
   static const char* st2 = ".Dean.Holzworth@tag.csiro.au@";
   StringTokenizer tokenizer2(st2, ".@", true);
   BOOST_CHECK(tokenizer2.countTokens() == 11);
   BOOST_CHECK(tokenizer2.nextToken() == ".");
   BOOST_CHECK(tokenizer2.nextToken() == "Dean");
   BOOST_CHECK(tokenizer2.nextToken() == ".");
   BOOST_CHECK(tokenizer2.nextToken(".") == "Holzworth@tag");
   BOOST_CHECK(tokenizer2.nextToken(".@") == ".");
   BOOST_CHECK(tokenizer2.nextToken() == "csiro");
   BOOST_CHECK(tokenizer2.nextToken() == ".");
   BOOST_CHECK(tokenizer2.nextToken() == "au");
   BOOST_CHECK(tokenizer2.nextToken() == "@");
   BOOST_CHECK(tokenizer2.countTokens() == 0);

   static const char* st3 = "one     two     three   ";
   StringTokenizer tokenizer3(st3, " ");
   BOOST_CHECK(tokenizer3.countTokens() == 3);
   BOOST_CHECK(tokenizer3.nextToken() == "one");
   BOOST_CHECK(tokenizer3.nextToken() == "two");
   BOOST_CHECK(tokenizer3.nextToken() == "three");

   static const char* st4 = "  one     two     three";
   StringTokenizer tokenizer4(st4, " ");
   BOOST_CHECK(tokenizer4.countTokens() == 3);
   BOOST_CHECK(tokenizer4.nextToken() == "one");
   BOOST_CHECK(tokenizer4.nextToken() == "two");
   BOOST_CHECK(tokenizer4.nextToken() == "three");
}

