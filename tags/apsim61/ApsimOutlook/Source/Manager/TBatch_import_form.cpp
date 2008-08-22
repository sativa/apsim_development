//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TBatch_import_form.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "AdvGrid"
#pragma link "BaseGrid"
#pragma link "AdvDirectoryEdit"
#pragma link "AdvEdBtn"
#pragma link "AdvEdit"
#pragma resource "*.dfm"
TBatch_import_form *Batch_import_form;
//---------------------------------------------------------------------------
__fastcall TBatch_import_form::TBatch_import_form(TComponent* Owner)
   : TForm(Owner)
   {
   }
//---------------------------------------------------------------------------
void __fastcall TBatch_import_form::GridGetEditorType(
      TObject *Sender, int ACol, int ARow, TEditorType &AEditor)
   {
   AEditor = edEditBtn;
   Grid->BtnEdit->OnClickBtn = GridButtonClick;
   }
//---------------------------------------------------------------------------
void __fastcall TBatch_import_form::GridButtonClick(TObject *Sender)
   {
   if (Grid->Col == 0)
      {
      if (OpenDialog->Execute())
         {
         Grid->BtnEdit->Text = OpenDialog->FileName;
         Grid->Cells[Grid->Col][Grid->Row] = OpenDialog->FileName;
         }

      }
   else if (Grid->Col == 1)
      {
      DirectoryEdit->Button->OnClick(NULL);
      Grid->BtnEdit->Text = DirectoryEdit->Text;
      Grid->Cells[Grid->Col][Grid->Row] = DirectoryEdit->Text;
      }
   }
//---------------------------------------------------------------------------

