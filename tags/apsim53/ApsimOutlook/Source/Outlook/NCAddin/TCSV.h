//---------------------------------------------------------------------------

#ifndef TCSVH
#define TCSVH
//---------------------------------------------------------------------------
#include <Classes.hpp>
//---------------------------------------------------------------------------

class TCSV : public TStringList
   {
   __published:
	   __property System::AnsiString __fastcall CommaText = {read=GetCSV, write=ParseCSV};
   private:	// User declarations
	   void __fastcall ParseCSV(System::AnsiString Value);
      System::AnsiString __fastcall GetCSV(void);
   public:		// User declarations


   };
//---------------------------------------------------------------------------

#endif

