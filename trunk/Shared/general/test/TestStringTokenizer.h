//---------------------------------------------------------------------------

#ifndef TestStringTokenizerH
#define TestStringTokenizerH
#include <StringTokenizer.h>
#include <test\test.h>
//---------------------------------------------------------------------------
class TestStringTokenizer : public Test
   {
   public:
      TestStringTokenizer(void) { }

      void run()
         {
            try
               {
             static const char* st1 = "Dean.Holzworth@tag.csiro.au";
             static const char* st2 = ".Dean.Holzworth@tag.csiro.au@";
             StringTokenizer tokenizer1(st1, ".@", false);
             _test(tokenizer1.countTokens() == 5);
             _test(tokenizer1.nextToken() == "Dean");
             _test(tokenizer1.nextToken() == "Holzworth");
             _test(tokenizer1.nextToken() == "tag");
             _test(tokenizer1.nextToken() == "csiro");
             _test(tokenizer1.nextToken() == "au");
             _test(tokenizer1.countTokens() == 0);
             StringTokenizer tokenizer2(st2, ".@", true);
             _test(tokenizer2.countTokens() == 11);
             _test(tokenizer2.nextToken() == ".");
             _test(tokenizer2.nextToken() == "Dean");
             _test(tokenizer2.nextToken() == ".");
             _test(tokenizer2.nextToken(".") == "Holzworth@tag");
             _test(tokenizer2.nextToken(".@") == ".");
             _test(tokenizer2.nextToken() == "csiro");
             _test(tokenizer2.nextToken() == ".");
             _test(tokenizer2.nextToken() == "au");
             _test(tokenizer2.nextToken() == "@");
             _test(tokenizer2.countTokens() == 0);

             static const char* st3 = "one     two     three   ";
             StringTokenizer tokenizer3(st3, " ");
             _test(tokenizer3.countTokens() == 3);
             _test(tokenizer3.nextToken() == "one");
             _test(tokenizer3.nextToken() == "two");
             _test(tokenizer3.nextToken() == "three");

             static const char* st4 = "  one     two     three";
             StringTokenizer tokenizer4(st4, " ");
             _test(tokenizer4.countTokens() == 3);
             _test(tokenizer4.nextToken() == "one");
             _test(tokenizer4.nextToken() == "two");
             _test(tokenizer4.nextToken() == "three");
             }
         catch (string& msg)
            {
            _fail(msg.c_str());
            }
         };
   };
#endif
