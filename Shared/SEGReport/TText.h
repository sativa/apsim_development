//---------------------------------------------------------------------------

#ifndef TTextH
#define TTextH
//---------------------------------------------------------------------------
#include <SysUtils.hpp>
#include <Controls.hpp>
#include <Classes.hpp>
#include <Forms.hpp>
#include <Qrctrls.hpp>
#include <QuickRpt.hpp>
#include <vector>
#include <string>
#include "TSEGTable.h"
#include "ReportMacros.h"
//---------------------------------------------------------------------------
// This class adds functionality to the standard TQRRichText by
// looking for macros in the text and replacing them with values.
//---------------------------------------------------------------------------
class PACKAGE TText : public TQRMemo
   {
   private:
      ReportMacros macros;
      AnsiString contentsWithMacros;

      std::vector<std::string> sourceNames;

      void trapSourceDataRefresh(void);
      void refresh(void);

      void __fastcall setText(AnsiString text);
      AnsiString __fastcall getText(void);
      virtual void __fastcall Loaded(void);
   protected:
   public:
      __fastcall TText(TComponent* Owner);
   __published:
      void __fastcall afterDataRefresh(TDataSet* dataset);

      __property AnsiString text = {read=getText, write=setText};
   };
//---------------------------------------------------------------------------
#endif
