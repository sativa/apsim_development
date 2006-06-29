//---------------------------------------------------------------------------
#pragma hdrstop
#include "TKWTest.h"
#include <general\pch.h>
#include <vcl.h>

#include "TKWTestForm.h"
#include <general\vcl_functions.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "AdvGrid"
#pragma link "BaseGrid"
#pragma link "dbadvgrd"
#pragma link "TPropertyForm"
#pragma link "AdvPanel"
#pragma resource "*.dfm"
TKWTestForm *KWTestForm;
//---------------------------------------------------------------------------
__fastcall TKWTestForm::TKWTestForm(TComponent* Owner)
   : TPropertyForm(Owner)
{
}
//---------------------------------------------------------------------------
// This is the component we're to modify.
//---------------------------------------------------------------------------
void TKWTestForm::setComponent(TComponent* component)
   {
   TPropertyForm::setComponent(component);

   kwTest = dynamic_cast<TKWTest*> (component);

   if (kwTest->source != NULL)
      {
      FieldNameCombo->Items->Assign(kwTest->source->FieldList);
      FieldNameCombo->Text = kwTest->fieldName;
      }
   Source2Combo->Items->AddStrings(SourceCombo->Items);
   if (kwTest->Source2 != NULL)
      Source2Combo->Text = kwTest->Source2->Name;
   }
//---------------------------------------------------------------------------
void __fastcall TKWTestForm::FieldNameComboChange(TObject *Sender)
   {
   kwTest->fieldName = FieldNameCombo->Text;
   }
//---------------------------------------------------------------------------
void __fastcall TKWTestForm::Source2ComboChange(TObject *Sender)
   {
   kwTest->Source2 = getComponent<TSEGTable>(kwTest->Owner, Source2Combo->Text);
   }
//---------------------------------------------------------------------------

