//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TProbabilityForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "AdvGrid"
#pragma link "BaseGrid"
#pragma link "dbadvgrd"
#pragma link "TPropertyForm"
#pragma link "AdvPanel"
#pragma resource "*.dfm"
TProbabilityForm *ProbabilityForm;
//---------------------------------------------------------------------------
__fastcall TProbabilityForm::TProbabilityForm(TComponent* Owner)
   : TPropertyForm(Owner)
{
}
//---------------------------------------------------------------------------
// This is the component we're to modify.
//---------------------------------------------------------------------------
void TProbabilityForm::setComponent(TComponent* component)
   {
   TPropertyForm::setComponent(component);

   probability = dynamic_cast<TProbability*> (component);

   if (probability->source != NULL)
      {
      FieldNameCombo->Items->Assign(probability->source->FieldList);
      FieldNameCombo->Text = probability->fieldName;
      }
   ExceedenceCheckBox->Checked = probability->exceedence;
   }
//---------------------------------------------------------------------------
void __fastcall TProbabilityForm::ExceedenceCheckBoxClick(TObject *Sender)
   {
   probability->exceedence = ExceedenceCheckBox->Checked;
   }
//---------------------------------------------------------------------------
void __fastcall TProbabilityForm::FieldNameComboChange(TObject *Sender)
   {
   probability->fieldName = FieldNameCombo->Text;
   }
//---------------------------------------------------------------------------

