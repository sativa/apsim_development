//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TTabRenameForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TTabRenameForm *TabRenameForm;
//---------------------------------------------------------------------------
__fastcall TTabRenameForm::TTabRenameForm(TComponent* Owner)
   : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TTabRenameForm::FormShow(TObject *Sender)
{
   EditBoxChange(this);   
}
//---------------------------------------------------------------------------

void __fastcall TTabRenameForm::EditBoxChange(TObject *Sender)
{
   if (EditBox->Text == "")
      BitBtn1->Enabled = false;
   else
      BitBtn1->Enabled = true;
}
//---------------------------------------------------------------------------

