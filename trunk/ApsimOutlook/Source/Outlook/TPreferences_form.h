//---------------------------------------------------------------------------
#ifndef TPreferences_formH
#define TPreferences_formH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "ToolEdit.hpp"
#include <Buttons.hpp>
#include <Mask.hpp>
//---------------------------------------------------------------------------
class TPreferences_form : public TForm
{
__published:	// IDE-managed Components
   TLabel *Label1;
   TFilenameEdit *File_name_edit;
   TBitBtn *BitBtn1;
   TBitBtn *BitBtn2;
   void __fastcall FormShow(TObject *Sender);
   void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
private:	// User declarations
public:		// User declarations
   __fastcall TPreferences_form(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TPreferences_form *Preferences_form;
//---------------------------------------------------------------------------
#endif
