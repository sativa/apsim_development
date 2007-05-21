//---------------------------------------------------------------------------
#ifndef TDifference_formH
#define TDifference_formH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "TAnalysis_form.h"
#include <Buttons.hpp>
#include <ComCtrls.hpp>
#include "paramtreeview.hpp"
#include <ImgList.hpp>
//---------------------------------------------------------------------------
class TDifference_form : public TAnalysis_form
{
__published:	// IDE-managed Components
   TLabel *Label3;
   TGroupBox *GroupBox1;
   TComboBox *LHS1;
   TComboBox *RHS1;
   TGroupBox *GroupBox2;
   TComboBox *LHS2;
   TComboBox *RHS2;
   void __fastcall FormShow(TObject *Sender);
   void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
   void __fastcall Combo_change(TObject *Sender);
private:	// User declarations
   virtual bool allowEnableOkButton(void);
   void fillCombos(void);

public:		// User declarations
   __fastcall TDifference_form(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TDifference_form *Difference_form;
//---------------------------------------------------------------------------
#endif
  