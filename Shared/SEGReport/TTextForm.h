//---------------------------------------------------------------------------

#ifndef TTextFormH
#define TTextFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <QRCTRLS.hpp>
#include <Buttons.hpp>
#include <ComCtrls.hpp>
#include <Dialogs.hpp>
#include "TText.h"
//---------------------------------------------------------------------------
class TTextForm : public TForm
{
__published:	// IDE-managed Components
   TPageControl *PageControl1;
   TTabSheet *Properties;
   TLabel *Label1;
   TRichEdit *TextEdit;
   TBitBtn *FontButton;
   TTabSheet *TabSheet1;
   TLabel *Label3;
   TEdit *NameEdit;
   TFontDialog *FontDialog;
   TLabel *Label4;
   TLabel *Label2;
   TLabel *Label5;
   void __fastcall TextEditExit(TObject *Sender);
   void __fastcall FontButtonClick(TObject *Sender);
   void __fastcall NameEditExit(TObject *Sender);
private:	// User declarations
      TText* richText;
public:		// User declarations
   __fastcall TTextForm(TComponent* Owner);
      void setComponent(TText* richText);
};
//---------------------------------------------------------------------------
extern PACKAGE TTextForm *TextForm;
//---------------------------------------------------------------------------
#endif
