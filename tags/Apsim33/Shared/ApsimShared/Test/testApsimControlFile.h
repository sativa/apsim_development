//---------------------------------------------------------------------------
#ifndef testApsimControlFileH
#define testApsimControlFileH
#include <test\framework\testcase.h>
#include <string>
class ApsimControlFile;
//---------------------------------------------------------------------------
class TestApsimControlFile : public TestCase
   {
   public:
      TestApsimControlFile(const std::string& name)
         : TestCase(name) { }

      virtual void setUp();
      virtual void tearDown();

      void testGetAllSectionNames(void);
      void testGetAllFiles(void);
      void testGetOutputFileNames(void);
      void testGetSummaryFileNames(void);
      void testGetParameterValues(void);
      void testSetParameterValues(void);
      void testChangeModuleName(void);
      void testTitle(void);
      void testGetInstances(void);
      void testRenameParameter(void);
      void testDeleteParameter(void);
      void testMoveParameter(void);
      void testMoveParametersOutOfCon(void);
      void testGetAllModuleInstances(void);
      void testGetFileForModule(void);

      static Test* suite (void);
   private:
      ApsimControlFile* con;
   };
#endif
