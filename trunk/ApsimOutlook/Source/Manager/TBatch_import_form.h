//---------------------------------------------------------------------------
#ifndef TBatch_import_formH
#define TBatch_import_formH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "HgGrid.hpp"
#include "HgHGrid.hpp"
#include <Buttons.hpp>
#include <Grids.hpp>
#include <Dialogs.hpp>
#include "ToolEdit.hpp"
#include <Mask.hpp>
//---------------------------------------------------------------------------
class TBatch_import_form : public TForm
{
__published:	// IDE-managed Components
   THyperGrid *Grid;
   TBitBtn *BitBtn1;
   TBitBtn *BitBtn2;
   TEdit *FilespecEdit;
   TLabel *Label1;
   TOpenDialog *OpenDialog;
   TDirectoryEdit *DirectoryEdit;
   void __fastcall GridButtonClick(TObject *Sender, int ACol, int ARow);
private:	// User declarations
public:		// User declarations
   __fastcall TBatch_import_form(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TBatch_import_form *Batch_import_form;
//---------------------------------------------------------------------------
#endif
