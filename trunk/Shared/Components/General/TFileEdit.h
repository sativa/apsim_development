//---------------------------------------------------------------------------
#ifndef TFileEditH
#define TFileEditH
//---------------------------------------------------------------------------
#include <SysUtils.hpp>
#include <Controls.hpp>
#include <Classes.hpp>
#include <Forms.hpp>
#include <ComCtrls.hpp>
#include <StdCtrls.hpp>
// ------------------------------------------------------------------
//  Short description:
//      this class extends TRichEdit to provide file editing capabilities.

//  Notes:

//  Changes:
//    DPH 23/10/98

// ------------------------------------------------------------------
class PACKAGE TFileEdit : public TRichEdit
   {
   private:
      AnsiString FFileName;

      // property setters.
      void __fastcall SetFileName (AnsiString FileName);

      // event we're interested in.
      void __fastcall KillFocus (TMessage& msg);
      void __fastcall OnChange (TObject* Sender);

      BEGIN_MESSAGE_MAP
         MESSAGE_HANDLER(WM_KILLFOCUS, TMessage, KillFocus)
      END_MESSAGE_MAP(TRichEdit)

   protected:

   public:
      __fastcall TFileEdit(TComponent* Owner);

      void Read(void);
      void Write(void);
      void Goto (const char* Search_text);

   __published:
      __property AnsiString FileName = {read=FFileName, write=SetFileName};
   };
//---------------------------------------------------------------------------
#endif
