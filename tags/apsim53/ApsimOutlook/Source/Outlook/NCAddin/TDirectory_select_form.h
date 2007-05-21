//---------------------------------------------------------------------------
#ifndef TDirectory_select_formH
#define TDirectory_select_formH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <FileCtrl.hpp>
#include <Buttons.hpp>
#include <Mask.hpp>
#include <list>
#include <ApsimShared\ApsimSettings.h>
using std::list;
//---------------------------------------------------------------------------
class TDirectory_select_form : public TForm
{
__published:	// IDE-managed Components
   TFileListBox *SubsetList;
   TLabel *Label1;
   TBitBtn *Ok_button;
   TBitBtn *BitBtn2;
   TComboBox *DatasetCombo;
   TLabel *Label2;
   void __fastcall FormShow(TObject *Sender);
   void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
   void __fastcall Directory_listboxChange(TObject *Sender);
   void __fastcall DatasetComboChange(TObject *Sender);
   void __fastcall SubsetListChange(TObject *Sender);
private:	// User declarations
   ApsimSettings settings;
   void Cleanup_dir_list (void);

public:		// User declarations
   __fastcall TDirectory_select_form(TComponent* Owner);
   __fastcall ~TDirectory_select_form(void);

   TStringList* SelectedNCs;
};
//---------------------------------------------------------------------------
extern PACKAGE TDirectory_select_form *Directory_select_form;
//---------------------------------------------------------------------------
#endif
