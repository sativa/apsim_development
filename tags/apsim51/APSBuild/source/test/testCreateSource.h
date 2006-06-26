//---------------------------------------------------------------------------

#ifndef testCreateSourceH
#define testCreateSourceH
#include <test\framework\testcase.h>
#include <string>
#include <iostream>
//---------------------------------------------------------------------------
class TestCreateSource : public TestCase
   {
   public:
      TestCreateSource(const std::string& name)
         : TestCase(name) { }

      virtual void setUp();
      virtual void tearDown();

      //---------------------------------------------------------------------------
      // test that a structure converts ok.
      //---------------------------------------------------------------------------
      void convertStructure(void);
      
      static Test* suite (void);
   private:

   };
#endif
