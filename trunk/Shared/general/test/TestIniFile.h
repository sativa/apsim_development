//---------------------------------------------------------------------------
#ifndef TestIniFileH
#define TestIniFileH

class TestIniFile
   {
   public:
      TestIniFile();
      ~TestIniFile();

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
      void setup(void);
   };
#endif
