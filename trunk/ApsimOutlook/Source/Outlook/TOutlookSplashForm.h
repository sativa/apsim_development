//---------------------------------------------------------------------------
#ifndef TOutlookSplashFormH
#define TOutlookSplashFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TOutlookSplashForm : public TForm
{
__published:	// IDE-managed Components
   TImage *Image1;
   TTimer *Timer1;
   TButton *OkButton;
   void __fastcall Timer1Timer(TObject *Sender);
   void __fastcall FormShow(TObject *Sender);
   void __fastcall OkButtonClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
   __fastcall TOutlookSplashForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TOutlookSplashForm *OutlookSplashForm;
//---------------------------------------------------------------------------
#endif
