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
#pragma link "TPropertyForm"
#pragma link "DBAdvGrd"
#pragma link "AdvPanel"
#pragma resource "*.dfm"
TFilterForm *FilterForm;
//---------------------------------------------------------------------------
__fastcall TFilterForm::TFilterForm(TComponent* Owner)
   : TPropertyForm(Owner)
{
}
//---------------------------------------------------------------------------
// This is the component we're to modify.
//---------------------------------------------------------------------------
void TFilterForm::setComponent(TComponent* component)
   {
   TPropertyForm::setComponent(component);

   filter = dynamic_cast< ::TFilter*>(component);
   FilterEdit->Text = filter->filter;
   }
//---------------------------------------------------------------------------

void __fastcall TFilterForm::FilterEditExit(TObject *Sender)
   {
   filter->filter = FilterEdit->Text;
   }
//---------------------------------------------------------------------------

