//---------------------------------------------------------------------------
#ifndef TExcelH
#define TExcelH
#include "TSEGTable.h"
#include <stdexcept>
//---------------------------------------------------------------------------
// derived from TSEGTable, this class reads in experimental data from
// the REMS database.
//---------------------------------------------------------------------------
class TExcel : public TSEGTable
   {
   private:
      AnsiString xlsFileName;
      int xlsPageIndex;
      TStrings* xlsPageNames;
      vector<string> fieldNames;

      void __fastcall setFilename(AnsiString filename);
      AnsiString __fastcall getPageName(void);
      void __fastcall setPageName(AnsiString pageName);

      virtual bool createFields(void) throw(std::runtime_error);
      virtual void storeRecords(void) throw(std::runtime_error);

   public:
      __fastcall TExcel(TComponent* owner);
      __fastcall ~TExcel(void);

      virtual void setProperty(const std::string& propertyName,
                               const std::string& propertyValue);

   __published:
      __property AnsiString fileName = {read=xlsFileName, write=setFilename};
      __property AnsiString pageName = {read=getPageName, write=setPageName};
      __property TStrings* pageNames = {read=xlsPageNames};
   };
#endif
