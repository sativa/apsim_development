//---------------------------------------------------------------------------
#ifndef TBatch_import_formH
#define TBatch_import_formH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Buttons.hpp>
#include <Grids.hpp>
#include <Dialogs.hpp>
#include <Mask.hpp>
#include "AdvGrid.hpp"
#include "BaseGrid.hpp"
#include "AdvDirectoryEdit.hpp"
#include "AdvEdBtn.hpp"
#include "AdvEdit.hpp"
//---------------------------------------------------------------------------
class TBatch_import_form : public TForm
{
__published:	// IDE-managed Components
   TEdit *FilespecEdit;
   TLabel *Label1;
   TOpenDialog *OpenDialog;
   TAdvStringGrid *Grid;
   TButton *Button1;
   TButton *Button2;
   TAdvDirectoryEdit *DirectoryEdit;
   void __fastcall GridGetEditorType(TObject *Sender, int ACol,
          int ARow, TEditorType &AEditor);
   void __fastcall GridButtonClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
   __fastcall TBatch_import_form(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TBatch_import_form *Batch_import_form;
//---------------------------------------------------------------------------
#endif
