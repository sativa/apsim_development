//---------------------------------------------------------------------------
#ifndef TSplashFormH
#define TSplashFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include "HTMLabel.hpp"
#include <jpeg.hpp>
#include <string>
//---------------------------------------------------------------------------
class TSplashForm : public TForm
{
__published:	// IDE-managed Components
   TTimer *Timer1;
   THTMLabel *DetailsLabel;
   TImage *Image1;
   void __fastcall Timer1Timer(TObject *Sender);
   void __fastcall FormShow(TObject *Sender);
   void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
private:	// User declarations
   std::string errorMessage;
public:		// User declarations
   __fastcall TSplashForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TSplashForm *SplashForm;
//---------------------------------------------------------------------------
#endif
