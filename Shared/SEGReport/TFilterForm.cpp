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
   FieldNameCombo->ItemIndex = FieldNameCombo->Items->IndexOf(filter->fieldName);
   FieldValueCombo->ItemIndex = FieldValueCombo->Items->IndexOf(filter->fieldValue);
   }
//---------------------------------------------------------------------------
void __fastcall TFilterForm::FieldNameComboChange(TObject *Sender)
   {
   filter->fieldName = FieldNameCombo->Items->Strings[FieldNameCombo->ItemIndex];
   FieldValueCombo->Items->Assign(filter->fieldValues);
   }
//---------------------------------------------------------------------------
void __fastcall TFilterForm::FieldValueComboChange(TObject *Sender)
   {
   filter->fieldValue = FieldValueCombo->Items->Strings[FieldValueCombo->ItemIndex];
   }
//---------------------------------------------------------------------------
void __fastcall TFilterForm::PropertiesSheetShow(TObject *Sender)
   {
   FieldNameCombo->Items->Assign(filter->fieldNames);
   FieldValueCombo->Items->Assign(filter->fieldValues);
   }
//---------------------------------------------------------------------------

