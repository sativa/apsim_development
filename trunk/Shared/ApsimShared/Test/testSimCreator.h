//---------------------------------------------------------------------------
#ifndef testSimCreatorH
#define testSimCreatorH
#include <test\framework\testcase.h>
#include <string>
class ApsimControlFile;
//---------------------------------------------------------------------------
class TestSimCreator : public TestCase
   {
   public:
      TestSimCreator(const std::string& name)
         : TestCase(name) { }

      virtual void setUp();
      virtual void tearDown();

      void testCreateSim(void);
      static Test* suite (void);
   private:

   };
#endif
