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
#pragma link "TSEGTableForm"
#pragma resource "*.dfm"
TProbabilityForm *ProbabilityForm;
//---------------------------------------------------------------------------
__fastcall TProbabilityForm::TProbabilityForm(TComponent* Owner)
   : TSEGTableForm(Owner)
{
}
//---------------------------------------------------------------------------
// This is the component we're to modify.
//---------------------------------------------------------------------------
void TProbabilityForm::setComponent(TProbability* prob)
   {
   probability = prob;
   TSEGTableForm::setComponent(probability);
   }
//---------------------------------------------------------------------------
void __fastcall TProbabilityForm::PropertiesSheetShow(TObject *Sender)
   {
   if (probability->source != NULL)
      {
      FieldNameCombo->Items->Assign(probability->source->FieldList);
      FieldNameCombo->Text = probability->fieldName;
      }
   if (probability->exceedence)
      ExceedenceRadio->Checked = true;
   else
      CumulativeRadio->Checked = true;
   }
//---------------------------------------------------------------------------
void __fastcall TProbabilityForm::ExceedenceRadioClick(TObject *Sender)
   {
   probability->exceedence = true;
   }
//---------------------------------------------------------------------------
void __fastcall TProbabilityForm::CumulativeRadioClick(TObject *Sender)
   {
   probability->exceedence = false;
   }
//---------------------------------------------------------------------------
void __fastcall TProbabilityForm::FieldNameComboChange(TObject *Sender)
   {
   probability->fieldName = FieldNameCombo->Text;
   }
//---------------------------------------------------------------------------

