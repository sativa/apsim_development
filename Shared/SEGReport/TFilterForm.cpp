//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TFilterForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "AdvGrid"
#pragma link "BaseGrid"
#pragma link "dbadvgrd"
#pragma link "TSEGTableForm"
#pragma link "DBAdvGrd"
#pragma resource "*.dfm"
TFilterForm *FilterForm;
//---------------------------------------------------------------------------
__fastcall TFilterForm::TFilterForm(TComponent* Owner)
   : TSEGTableForm(Owner)
{
}
//---------------------------------------------------------------------------
// This is the component we're to modify.
//---------------------------------------------------------------------------
void TFilterForm::setComponent(::TFilter* f)
   {
   filter = f;
   TSEGTableForm::setComponent(filter);
   FilterEdit->Text = filter->filter;
   }
//---------------------------------------------------------------------------

void __fastcall TFilterForm::FilterEditExit(TObject *Sender)
   {
   filter->filter = FilterEdit->Text;
   }
//---------------------------------------------------------------------------

