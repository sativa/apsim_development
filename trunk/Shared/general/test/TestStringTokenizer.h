//---------------------------------------------------------------------------

#ifndef TestStringTokenizerH
#define TestStringTokenizerH
#include <test\framework\testcase.h>
#include <string>
//---------------------------------------------------------------------------
class TestStringTokenizer : public TestCase
   {
   public:
      TestStringTokenizer(const std::string& name)
         : TestCase(name) { }

      void all(void);
      static Test* suite (void);
   };
#endif
