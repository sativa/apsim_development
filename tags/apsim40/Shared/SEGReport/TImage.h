//---------------------------------------------------------------------------
#ifndef TImageH
#define TImageH
//---------------------------------------------------------------------------
#include <SysUtils.hpp>
#include <Classes.hpp>
#include <Controls.hpp>
#include <gtQrCtrls.hpp>
#include <QuickRpt.hpp>
//---------------------------------------------------------------------------
// A simple wrapper around a TQRImage so that we can give the palette component
// our own name.
//---------------------------------------------------------------------------
class PACKAGE TImage : public TgtQRImage
   {
   private:
   protected:
   public:
      __fastcall TImage(TComponent* Owner);
   __published:
   };
//---------------------------------------------------------------------------
#endif
