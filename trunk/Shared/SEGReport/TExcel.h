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
      AnsiString reportDirectory;

      void __fastcall setFilename(AnsiString filename);
      AnsiString __fastcall getPageName(void);
      void __fastcall setPageName(AnsiString pageName);
      void relativeToAbsoluteFile(void);
      void absoluteToRelativeFile(void);

      virtual void __fastcall Loaded(void);
      virtual bool createFields(void) throw(std::runtime_error);
      virtual void storeRecords(void) throw(std::runtime_error);

   public:
      __fastcall TExcel(TComponent* owner);
      __fastcall ~TExcel(void);

      // Called by SEGReport to give components a chance to know the current
      // report directory.  Used by ApsimFileReader to use relative paths.
      virtual void setReportDirectory(AnsiString reportDir);

      virtual void setProperty(const std::string& propertyName,
                               const std::string& propertyValue);

   __published:
      __property AnsiString fileName = {read=xlsFileName, write=setFilename};
      __property AnsiString pageName = {read=getPageName, write=setPageName};
      __property TStrings* pageNames = {read=xlsPageNames};
   };
#endif
