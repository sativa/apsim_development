//---------------------------------------------------------------------------

#ifndef TTextFormH
#define TTextFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "TPropertyForm.h"
#include <Buttons.hpp>
#include <ComCtrls.hpp>
#include <Dialogs.hpp>
#include "TText.h"
#include "AdvPanel.hpp"
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TTextForm : public TPropertyForm
{
__published:	// IDE-managed Components
   TRichEdit *TextEdit;
   TFontDialog *FontDialog;
   TLabel *Label6;
   TComboBox *AlignmentCombo;
   TLabel *Label7;
   TCheckBox *AutosizeCheckBox;
   TLabel *Label5;
   TLabel *FontLabel;
   void __fastcall TextEditExit(TObject *Sender);
   void __fastcall AlignmentComboChange(TObject *Sender);
   void __fastcall ToolbarCheckBoxClick(TObject *Sender);
   void __fastcall FontLabelClick(TObject *Sender);
private:	// User declarations
      TText* text;
public:		// User declarations
   __fastcall TTextForm(TComponent* Owner);
   virtual void setComponent(TComponent* component);

};
//---------------------------------------------------------------------------
extern PACKAGE TTextForm *TextForm;
//---------------------------------------------------------------------------
#endif
