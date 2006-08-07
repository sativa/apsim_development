//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TRegrForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "AdvGrid"
#pragma link "BaseGrid"
#pragma link "dbadvgrd"
#pragma link "TPropertyForm"
#pragma link "AdvPanel"
#pragma link "TPropertyForm"
#pragma resource "*.dfm"
TRegrForm *RegrForm;
//---------------------------------------------------------------------------
__fastcall TRegrForm::TRegrForm(TComponent* Owner)
   : TPropertyForm(Owner)
{
}
//---------------------------------------------------------------------------
// This is the component we're to modify.
//---------------------------------------------------------------------------
void TRegrForm::setComponent(TComponent* component)
   {
   TPropertyForm::setComponent(component);

   regr = dynamic_cast<TRegr*> (component);

   if (regr->source != NULL)
      {
      XCombo->Items->Assign(regr->source->FieldList);
      YCombo->Items->Assign(XCombo->Items);
      XCombo->Text = regr->xFieldName;
      YCombo->Text = regr->yFieldName;
      }
   }
//---------------------------------------------------------------------------
void __fastcall TRegrForm::XComboChange(TObject *Sender)
   {
   regr->xFieldName = XCombo->Text;
   }
//---------------------------------------------------------------------------
void __fastcall TRegrForm::YComboChange(TObject *Sender)
   {
   regr->yFieldName = YCombo->Text;
   }
//---------------------------------------------------------------------------

