//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TSOIForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "AdvGrid"
#pragma link "BaseGrid"
#pragma link "dbadvgrd"
#pragma link "TSEGTableForm"
#pragma resource "*.dfm"
TSOIForm *SOIForm;
//---------------------------------------------------------------------------
__fastcall TSOIForm::TSOIForm(TComponent* Owner)
   : TSEGTableForm(Owner)
{
}
//---------------------------------------------------------------------------
// This is the component we're to modify.
//---------------------------------------------------------------------------
void TSOIForm::setComponent(TSOI* s)
   {
   soi = s;
   TSEGTableForm::setComponent(soi);
   SOIList->ItemIndex = SOIList->Items->IndexOf(soi->month);
   }
//---------------------------------------------------------------------------
void __fastcall TSOIForm::SOIListClick(TObject *Sender)
   {
   soi->month = SOIList->Items->Strings[SOIList->ItemIndex];
   }
//---------------------------------------------------------------------------
