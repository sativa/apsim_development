//---------------------------------------------------------------------------
#ifndef TSOI_formH
#define TSOI_formH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Buttons.hpp>
#include "SOIToolBar.h"
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TSOI_form : public TForm
{
__published:	// IDE-managed Components
   TListBox *SOI_listbox;
   TPanel *Panel1;
   TBitBtn *BitBtn1;
   TBitBtn *BitBtn2;
   TPanel *Panel2;
   TLabel *Label1;
   TLabel *Label2;
   TLabel *Label3;
   TPanel *Panel3;
   TCheckBox *SOI_toggle;
   TCheckBox *CheckBox1;
   TCheckBox *CheckBox2;
   TCheckBox *CheckBox3;
   TCheckBox *CheckBox4;
   TCheckBox *CheckBox5;
   TCheckBox *AllYears;
   void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
   void __fastcall FormShow(TObject *Sender);
   void __fastcall SOI_toggleClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
   __fastcall TSOI_form(TComponent* Owner);
   SOIToolBar* SOI_ptr;
};
//---------------------------------------------------------------------------
extern PACKAGE TSOI_form *SOI_form;
//---------------------------------------------------------------------------
#endif
