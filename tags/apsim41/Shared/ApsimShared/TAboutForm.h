//---------------------------------------------------------------------------
#ifndef TAboutFormH
#define TAboutFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Buttons.hpp>
#include <ExtCtrls.hpp>
#include "HTMLabel.hpp"
#include "HTMLText.hpp"
#include "paramlabel.hpp"
//---------------------------------------------------------------------------
class TAboutForm : public TForm
{
__published:	// IDE-managed Components
   TLabel *Label3;
   TLabel *Label5;
   TPanel *Panel1;
   TLabel *Label4;
   TLabel *WEBLabel;
   TLabel *Label6;
   TLabel *EmailLabel;
   TBitBtn *BitBtn1;
   TScrollBox *ScrollBox1;
   TParamLabel *DetailsLabel;
   void __fastcall FormShow(TObject *Sender);
   
   void __fastcall WEBLabelClick(TObject *Sender);
   void __fastcall EmailLabelClick(TObject *Sender);
   void __fastcall Button1Click(TObject *Sender);
private:	// User declarations
public:		// User declarations
   __fastcall TAboutForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TAboutForm *AboutForm;
//---------------------------------------------------------------------------
#endif
