//---------------------------------------------------------------------------
#ifndef GeneratorFromExcelH
#define GeneratorFromExcelH
#include <string>
//---------------------------------------------------------------------------
// This class will use the MacroSubstFile class to generate a set of
// of files given values from an EXCEL spreadsheet.
//---------------------------------------------------------------------------
class GeneratorFromExcel
   {
   public:
      GeneratorFromExcel(void);

      //---------------------------------------------------------------------------
      // Go generate all files given the specified macro spreadsheet file.
      //---------------------------------------------------------------------------
      void go(const std::string& xlsFileName,
              const std::string& xlsSheetName,
              const std::string& scriptFileName);

   private:
   };
#endif
