//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TObjectInspectorForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TObjectInspectorForm *ObjectInspectorForm;
//---------------------------------------------------------------------------
__fastcall TObjectInspectorForm::TObjectInspectorForm(TComponent* Owner)
   : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TObjectInspectorForm::FormClose(TObject *Sender,
      TCloseAction &Action)
   {
   if (Parent != NULL)
      Parent->Visible = false;
   }
//---------------------------------------------------------------------------

