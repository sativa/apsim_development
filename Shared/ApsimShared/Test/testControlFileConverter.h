//---------------------------------------------------------------------------
#ifndef testControlFileConverterH
#define testControlFileConverterH
#include <test\framework\testcase.h>
#include <string>
#include <iostream>
//---------------------------------------------------------------------------
class TestControlFileConverter : public TestCase
   {
   public:
      TestControlFileConverter(const std::string& name)
         : TestCase(name) { }

      virtual void setUp();
      virtual void tearDown();

      void setUp2();
      void testSetParameterValue(void);
      void testRenameParameter(void);
      void testDeleteParameter(void);
      void testChangeInstantiation(void);
      void testRemoveReportOutputSwitch(void);
      void testMoveParameter(void);
      void testNewFormatReportVariables(void);
      void testMoveParamsFromConToPar(void);
      void testRemoveSumAvgToTracker(void);
      static Test* suite (void);
   private:

   };
#endif
