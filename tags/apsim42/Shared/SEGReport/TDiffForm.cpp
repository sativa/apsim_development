#pragma link "DBAdvGrd"
//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TDiffForm.h"
#include <general\vcl_functions.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "AdvGrid"
#pragma link "BaseGrid"
#pragma link "dbadvgrd"
#pragma link "TPropertyForm"
#pragma link "AdvPanel"
#pragma resource "*.dfm"
TDiffForm *DiffForm;
//---------------------------------------------------------------------------
__fastcall TDiffForm::TDiffForm(TComponent* Owner)
   : TPropertyForm(Owner)
{
}
//---------------------------------------------------------------------------
// This is the component we're to modify.
//---------------------------------------------------------------------------
void TDiffForm::setComponent(TComponent* component)
   {
   TPropertyForm::setComponent(component);

   diff = dynamic_cast<TDiff*>(component);

   if (diff->source != NULL)
      {
      FieldNameCombo->Items->Assign(diff->source->FieldList);
      FieldNameCombo->Text = diff->fieldName;
      }
   getComponentNames<TSEGTable>(component->Owner, SecondDataSetCombo->Items);
   if (diff->secondDataSet != NULL)
      SecondDataSetCombo->Text = diff->secondDataSet->Name;

   }
//---------------------------------------------------------------------------
void __fastcall TDiffForm::FieldNameComboChange(TObject *Sender)
   {
   diff->fieldName = FieldNameCombo->Text;
   }

//---------------------------------------------------------------------------
// User has changed the source property - update field list.
//---------------------------------------------------------------------------
void TDiffForm::sourceHasChanged(TSEGTable* segTable)
   {
   if (diff->source != NULL)
      {
      FieldNameCombo->Items->Assign(diff->source->FieldList);
      FieldNameCombo->Text = diff->fieldName;
      }
   }
void __fastcall TDiffForm::SecondDataSetComboChange(TObject *Sender)
   {
   diff->secondDataSet = getComponent<TSEGTable>(diff->Owner, SecondDataSetCombo->Text);
   }
//---------------------------------------------------------------------------

