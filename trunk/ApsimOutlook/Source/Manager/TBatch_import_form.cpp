//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "TBatch_import_form.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "ToolEdit"
#pragma resource "*.dfm"
TBatch_import_form *Batch_import_form;
//---------------------------------------------------------------------------
__fastcall TBatch_import_form::TBatch_import_form(TComponent* Owner)
   : TForm(Owner)
   {
   }
//---------------------------------------------------------------------------
void __fastcall TBatch_import_form::GridButtonClick(TObject *Sender,
      int ACol, int ARow)
   {
   if (ACol == 0)
      {
      if (OpenDialog->Execute())
         Grid->Cells[ACol][ARow] = OpenDialog->FileName;
      }
   else if (ACol == 1)
      {
      DirectoryEdit->DoClick();
      Grid->Cells[ACol][ARow] = DirectoryEdit->Text;
      }
   }
//---------------------------------------------------------------------------
void __fastcall TBatch_import_form::FormShow(TObject *Sender)
   {
   Grid->Cells[0][0] = "Database to import files into";
   Grid->Cells[1][0] = "Directory containing simulation outputs";
   }
//---------------------------------------------------------------------------

