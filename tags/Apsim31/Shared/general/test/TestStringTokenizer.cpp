//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TestStringTokenizer.h"
#include <general\StringTokenizer.h>
#include <test\framework\testsuite.h>
#include <test\framework\testcaller.h>

#pragma package(smart_init)

//---------------------------------------------------------------------------
// Perform all tests.
//---------------------------------------------------------------------------
Test* TestStringTokenizer::suite(void)
   {
   TestSuite *testSuite = new TestSuite ("StringTokenizer");
   testSuite->addTest (new TestCaller <TestStringTokenizer> ("all", &TestStringTokenizer::all));
   return testSuite;
   }

//---------------------------------------------------------------------------
// Perform all tests.
//---------------------------------------------------------------------------
void TestStringTokenizer::all(void)
   {
   try
      {
      static const char* st1 = "Dean.Holzworth@tag.csiro.au";
      static const char* st2 = ".Dean.Holzworth@tag.csiro.au@";
      StringTokenizer tokenizer1(st1, ".@", false);
      assert(tokenizer1.countTokens() == 5);
      assert(tokenizer1.nextToken() == "Dean");
      assert(tokenizer1.nextToken() == "Holzworth");
      assert(tokenizer1.nextToken() == "tag");
      assert(tokenizer1.nextToken() == "csiro");
      assert(tokenizer1.nextToken() == "au");
      assert(tokenizer1.countTokens() == 0);
      StringTokenizer tokenizer2(st2, ".@", true);
      assert(tokenizer2.countTokens() == 11);
      assert(tokenizer2.nextToken() == ".");
      assert(tokenizer2.nextToken() == "Dean");
      assert(tokenizer2.nextToken() == ".");
      assert(tokenizer2.nextToken(".") == "Holzworth@tag");
      assert(tokenizer2.nextToken(".@") == ".");
      assert(tokenizer2.nextToken() == "csiro");
      assert(tokenizer2.nextToken() == ".");
      assert(tokenizer2.nextToken() == "au");
      assert(tokenizer2.nextToken() == "@");
      assert(tokenizer2.countTokens() == 0);

      static const char* st3 = "one     two     three   ";
      StringTokenizer tokenizer3(st3, " ");
      assert(tokenizer3.countTokens() == 3);
      assert(tokenizer3.nextToken() == "one");
      assert(tokenizer3.nextToken() == "two");
      assert(tokenizer3.nextToken() == "three");

      static const char* st4 = "  one     two     three";
      StringTokenizer tokenizer4(st4, " ");
      assert(tokenizer4.countTokens() == 3);
      assert(tokenizer4.nextToken() == "one");
      assert(tokenizer4.nextToken() == "two");
      assert(tokenizer4.nextToken() == "three");
      }
   catch (std::string& msg)
      {
      assert(false);
      }
   };

