//---------------------------------------------------------------------------
#ifndef TOutlookSplashFormH
#define TOutlookSplashFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include <ImgList.hpp>
//---------------------------------------------------------------------------
class TOutlookSplashForm : public TForm
{
__published:	// IDE-managed Components
   TImage *Image1;
   TTimer *Timer1;
   TPanel *Panel1;
   TButton *OkButton;
   void __fastcall Timer1Timer(TObject *Sender);
   void __fastcall FormShow(TObject *Sender);
   void __fastcall OkButtonClick(TObject *Sender);
   void __fastcall Button1Click(TObject *Sender);
private:	// User declarations
   int logoGap;
   bool showOkPanel;
public:		// User declarations
   __fastcall TOutlookSplashForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TOutlookSplashForm *OutlookSplashForm;
//---------------------------------------------------------------------------
#endif
