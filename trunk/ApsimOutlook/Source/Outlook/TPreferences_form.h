//---------------------------------------------------------------------------
#ifndef TPreferences_formH
#define TPreferences_formH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Buttons.hpp>
#include <Mask.hpp>
#include "AdvEdBtn.hpp"
#include "AdvEdit.hpp"
#include "AdvFileNameEdit.hpp"
#include <ApsimShared\ApsimSettings.h>
//---------------------------------------------------------------------------
class TPreferences_form : public TForm
{
__published:	// IDE-managed Components
   TLabel *SOILabel;
   TBitBtn *BitBtn1;
   TBitBtn *BitBtn2;
   TCheckBox *ColourBackgroundCheckbox;
   TAdvFileNameEdit *File_name_edit;
   void __fastcall FormShow(TObject *Sender);
   void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
private:	// User declarations
   ApsimSettings settings;
public:		// User declarations
   __fastcall TPreferences_form(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TPreferences_form *Preferences_form;
//---------------------------------------------------------------------------
#endif
