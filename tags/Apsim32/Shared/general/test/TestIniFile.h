//---------------------------------------------------------------------------
#ifndef TestIniFileH
#define TestIniFileH
#include <test\framework\testcase.h>
#include <string>
#include <general\inifile.h>
//---------------------------------------------------------------------------
class TestIniFile : public TestCase
   {
   public:
      TestIniFile(const std::string& name)
         : TestCase(name) { }

      static Test* suite (void);
      virtual void setUp();
      virtual void tearDown();

      void testReadSectionNames(void);
      void testReadSection(void);
      void testRead(void);
      void testWriteSection(void);
      void testWrite(void);
      void testDeleteKey(void);
      void testDeleteSection(void);
      void testGetKeysInSection(void);
      void testRenameSection(void);
      void testRenameKey(void);

   private:
      IniFile ini;
      
   };
#endif
